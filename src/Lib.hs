module Lib
    ( tessitura,
      computeInterval,
      computeNote,
      FullNote,
      Note (..),
      Interval (..)
    ) where

import Data.List (elemIndex)

data Note = C | D | E | F | G | A | B
    deriving (Eq, Show)

type FullNote = (Note, Int)

data Interval = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh | Octave | Ninth | Tenth | Eleventh | Twelfth | Thirteenth | Fourteenth | Fifteenth
    deriving (Enum, Eq, Show)

naturalDiatonicScale :: [Note]
naturalDiatonicScale = [C, D, E, F, G, A, B]

tessitura :: [FullNote]
tessitura = concatMap (\x -> zip naturalDiatonicScale (repeat x)) [0..8]

computeInterval :: FullNote -> FullNote -> Maybe Interval
computeInterval x y = computeInterval' (x `elemIndex` tessitura) (y `elemIndex` tessitura)

computeInterval' :: Maybe Int -> Maybe Int -> Maybe Interval
computeInterval' Nothing _ = Nothing
computeInterval' _ Nothing = Nothing
computeInterval' (Just x) (Just y)
    | x <= y = Just $ toEnum (y - x)
    | otherwise = Just $ toEnum (x - y)

computeNote :: FullNote -> Interval -> Maybe FullNote
computeNote note interval = computeNote' (note `elemIndex` tessitura) (fromEnum interval)

computeNote' :: Maybe Int -> Int -> Maybe FullNote
computeNote' Nothing _ = Nothing
computeNote' (Just x) y
    | x + y <= length tessitura = Just $ tessitura !! (x + y)
    | otherwise = Nothing
