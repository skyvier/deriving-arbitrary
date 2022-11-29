{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module MyLib where

import GHC.Generics
import GHC.TypeLits

import Test.QuickCheck

import Data.Kind
import Data.Coerce

data TestRecord = TestRecord
  { a :: Int
  , b :: Maybe Int
  , c :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving Arbitrary via (CanBeTuple TestRecord)

-- | This converts a record like 'TestRecord' to a nested tuple.
--
-- > :kind! RecordToNestedTuple TestRecord
-- (Int, (Maybe Int, String))
type family RecordToNestedTuple rep where
  RecordToNestedTuple (D1 _metadata rest) = RecordToNestedTuple rest
  RecordToNestedTuple (C1 _metadata rest) = RecordToNestedTuple rest
  RecordToNestedTuple (S1 _metadata field :*: rest) =
    (RecordToNestedTuple field, RecordToNestedTuple rest)
  RecordToNestedTuple (S1 _metadata field) = RecordToNestedTuple field
  RecordToNestedTuple (Rec0 _type) = _type
  RecordToNestedTuple _ = TypeError ('Text "The data type was a sum type")

-- Converts a nested tuple to a flat tuple.
--
-- This is pretty inelegant.
type family FlattenTuple tuple where
  FlattenTuple (a, (b, (c, (d, (e, (f, (g, h))))))) = (a, b, c, d, e, f, g, h)
  FlattenTuple (a, (b, (c, (d, (e, (f, g)))))) = (a, b, c, d, e, f, g)
  FlattenTuple (a, (b, (c, (d, (e, f))))) = (a, b, c, d, e, f)
  FlattenTuple (a, (b, (c, (d, e)))) = (a, b, c, d, e)
  FlattenTuple (a, (b, (c, d))) = (a, b, c, d)
  FlattenTuple (a, (b, c)) = (a, b, c)
  FlattenTuple (a, b) = (a, b)
  FlattenTuple a = TypeError ('Text "The constructor must have more than one field")

newtype CanBeTuple a = CanBeTuple a

type GenericCoercible a b =
  (Generic a, Generic b, Coercible (Rep a ()) (Rep b ()))

genericCoerce :: forall a b. GenericCoercible a b => a -> b
genericCoerce = to . (coerce @(Rep a ()) @(Rep b ())) . from

instance ( GenericCoercible (FlattenTuple (RecordToNestedTuple (Rep a))) a
         , Arbitrary (FlattenTuple (RecordToNestedTuple (Rep a)))
         ) => Arbitrary (CanBeTuple a) where
  arbitrary =
    CanBeTuple . genericCoerce <$> arbitrary @(FlattenTuple (RecordToNestedTuple (Rep a)))
