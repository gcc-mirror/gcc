------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . S T R I N G S . B O U N D E D                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  The language-defined package Strings.Bounded provides a generic package
--  each of whose instances yields a private type Bounded_String and a set
--  of operations. An object of a particular Bounded_String type represents
--  a String whose low bound is 1 and whose length can vary conceptually
--  between 0 and a maximum size established at the generic instantiation. The
--  subprograms for fixed-length string handling are either overloaded directly
--  for Bounded_String, or are modified as needed to reflect the variability in
--  length. Additionally, since the Bounded_String type is private, appropriate
--  constructor and selector operations are provided.

with Ada.Strings.Maps; use type Ada.Strings.Maps.Character_Mapping_Function;
with Ada.Strings.Superbounded;
with Ada.Strings.Search;

package Ada.Strings.Bounded with
  SPARK_Mode,
  Always_Terminates
is
   pragma Preelaborate;

   generic
      Max : Positive;
      --  Maximum length of a Bounded_String

   package Generic_Bounded_Length with SPARK_Mode,
     Initial_Condition => Length (Null_Bounded_String) = 0,
     Abstract_State    => null,
     Always_Terminates
   is
      --  Preconditions in this unit are meant for analysis only, not for
      --  run-time checking, so that the expected exceptions are raised. This
      --  is enforced by setting the corresponding assertion policy to Ignore.
      --  Postconditions and contract cases should not be executed at runtime
      --  as well, in order not to slow down the execution of these functions.

      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Contract_Cases => Ignore,
                               Ghost          => Ignore);

      Max_Length : constant Positive := Max;

      type Bounded_String is private;
      pragma Preelaborable_Initialization (Bounded_String);

      Null_Bounded_String : constant Bounded_String;
      --  Null_Bounded_String represents the null string. If an object of type
      --  Bounded_String is not otherwise initialized, it will be initialized
      --  to the same value as Null_Bounded_String.

      subtype Length_Range is Natural range 0 .. Max_Length;

      function Length (Source : Bounded_String) return Length_Range with
        Global => null;
      --  The Length function returns the length of the string represented by
      --  Source.

      --------------------------------------------------------
      -- Conversion, Concatenation, and Selection Functions --
      --------------------------------------------------------

      function To_Bounded_String
        (Source : String;
         Drop   : Truncation := Error) return Bounded_String
      with
        Pre            => (if Source'Length > Max_Length then Drop /= Error),
        Contract_Cases =>
          (Source'Length <= Max_Length
           =>
             To_String (To_Bounded_String'Result) = Source,

           Source'Length > Max_Length and then Drop = Left
           =>
             To_String (To_Bounded_String'Result) =
               Source (Source'Last - Max_Length + 1 .. Source'Last),

           others  --  Drop = Right
           =>
             To_String (To_Bounded_String'Result) =
               Source (Source'First .. Source'First - 1 + Max_Length));
      --  If Source'Length <= Max_Length, then this function returns a
      --  Bounded_String that represents Source. Otherwise, the effect
      --  depends on the value of Drop:
      --
      --  * If Drop=Left, then the result is a Bounded_String that represents
      --    the string comprising the rightmost Max_Length characters of
      --    Source.
      --
      --  * If Drop=Right, then the result is a Bounded_String that represents
      --    the string comprising the leftmost Max_Length characters of Source.
      --
      --  * If Drop=Error, then Strings.Length_Error is propagated.

      function To_String (Source : Bounded_String) return String with
        Global => null;
      --  To_String returns the String value with lower bound 1
      --  represented by Source. If B is a Bounded_String, then
      --  B = To_Bounded_String(To_String(B)).

      procedure Set_Bounded_String
        (Target : out Bounded_String;
         Source : String;
         Drop   : Truncation := Error)
      with
        Pre            => (if Source'Length > Max_Length then Drop /= Error),
        Contract_Cases =>
          (Source'Length <= Max_Length
           =>
             To_String (Target) = Source,

           Source'Length > Max_Length and then Drop = Left
           =>
             To_String (Target) =
               Source (Source'Last - Max_Length + 1 .. Source'Last),

           others  --  Drop = Right
           =>
             To_String (Target) =
               Source (Source'First .. Source'First - 1 + Max_Length));
      pragma Ada_05 (Set_Bounded_String);
      --  Equivalent to Target := To_Bounded_String (Source, Drop);

      --  Each of the Append functions returns a Bounded_String obtained by
      --  concatenating the string or character given or represented by one
      --  of the parameters, with the string or character given or represented
      --  by the other parameter, and applying To_Bounded_String to the
      --  concatenation result string, with Drop as provided to the Append
      --  function.

      function Append
        (Left  : Bounded_String;
         Right : Bounded_String;
         Drop  : Truncation  := Error) return Bounded_String
      with
        Pre            =>
          (if Length (Left) > Max_Length - Length (Right)
           then Drop /= Error),
        Contract_Cases =>
          (Length (Left) <= Max_Length - Length (Right)
           =>
             Length (Append'Result) = Length (Left) + Length (Right)
               and then
                 Slice (Append'Result, 1, Length (Left)) = To_String (Left)
               and then
                 (if Length (Right) > 0 then
                    Slice (Append'Result,
                      Length (Left) + 1, Length (Append'Result)) =
                        To_String (Right)),

           Length (Left) > Max_Length - Length (Right)
             and then Drop = Strings.Left
           =>
             Length (Append'Result) = Max_Length
               and then
                 (if Length (Right) < Max_Length then
                    Slice (Append'Result, 1, Max_Length - Length (Right)) =
                      Slice (Left,
                        Length (Left) - Max_Length + Length (Right) + 1,
                        Length (Left)))
               and then
                 Slice (Append'Result,
                   Max_Length - Length (Right) + 1, Max_Length) =
                     To_String (Right),

           others  --  Drop = Right
           =>
             Length (Append'Result) = Max_Length
               and then
                 Slice (Append'Result, 1, Length (Left)) = To_String (Left)
               and then
                 (if Length (Left) < Max_Length then
                    Slice (Append'Result, Length (Left) + 1, Max_Length) =
                      Slice (Right, 1, Max_Length - Length (Left))));

      function Append
        (Left  : Bounded_String;
         Right : String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre            =>
          (if Right'Length > Max_Length - Length (Left)
           then Drop /= Error),
        Contract_Cases =>
          (Length (Left) <= Max_Length - Right'Length
           =>
             Length (Append'Result) = Length (Left) + Right'Length
               and then
                 Slice (Append'Result, 1, Length (Left)) = To_String (Left)
               and then
                 (if Right'Length > 0 then
                    Slice (Append'Result,
                      Length (Left) + 1, Length (Append'Result)) =
                        Right),

           Length (Left) > Max_Length - Right'Length
             and then Drop = Strings.Left
           =>
             Length (Append'Result) = Max_Length
               and then
                 (if Right'Length < Max_Length then

                    --  The result is the end of Left followed by Right

                    Slice (Append'Result, 1, Max_Length - Right'Length) =
                      Slice (Left,
                        Length (Left) - Max_Length + Right'Length + 1,
                        Length (Left))
                      and then
                        Slice (Append'Result,
                          Max_Length - Right'Length + 1, Max_Length) =
                            Right
                  else
                    --  The result is the last Max_Length characters of Right

                    To_String (Append'Result) =
                      Right (Right'Last - Max_Length + 1 .. Right'Last)),

           others  --  Drop = Right
           =>
             Length (Append'Result) = Max_Length
               and then
                 Slice (Append'Result, 1, Length (Left)) = To_String (Left)
               and then
                 (if Length (Left) < Max_Length then
                    Slice (Append'Result, Length (Left) + 1, Max_Length) =
                      Right (Right'First
                        .. Max_Length - Length (Left) - 1 + Right'First)));
      function Append
        (Left  : String;
         Right : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre            =>
          (if Left'Length > Max_Length - Length (Right)
           then Drop /= Error),
        Contract_Cases =>
          (Left'Length <= Max_Length - Length (Right)
           =>
             Length (Append'Result) = Left'Length + Length (Right)
               and then Slice (Append'Result, 1, Left'Length) = Left
               and then
                 (if Length (Right) > 0 then
                    Slice (Append'Result,
                      Left'Length + 1, Length (Append'Result)) =
                        To_String (Right)),

           Left'Length > Max_Length - Length (Right)
             and then Drop = Strings.Left
           =>
             Length (Append'Result) = Max_Length
               and then
                 (if Length (Right) < Max_Length then
                    Slice (Append'Result, 1, Max_Length - Length (Right)) =
                      Left (Left'Last - Max_Length + Length (Right) + 1
                            .. Left'Last))
               and then
                 Slice (Append'Result,
                   Max_Length - Length (Right) + 1, Max_Length) =
                     To_String (Right),

           others  --  Drop = Right
           =>
             Length (Append'Result) = Max_Length
               and then
                 (if Left'Length < Max_Length then

                    --  The result is Left followed by the beginning of Right

                    Slice (Append'Result, 1, Left'Length) = Left
                      and then
                        Slice (Append'Result, Left'Length + 1, Max_Length) =
                          Slice (Right, 1, Max_Length - Left'Length)
                  else
                    --  The result is the first Max_Length characters of Left

                    To_String (Append'Result) =
                      Left (Left'First .. Max_Length - 1 + Left'First)));

      function Append
        (Left  : Bounded_String;
         Right : Character;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre            => (if Length (Left) = Max_Length then Drop /= Error),
        Contract_Cases =>
          (Length (Left) < Max_Length
           =>
             Length (Append'Result) = Length (Left) + 1
               and then
                 Slice (Append'Result, 1, Length (Left)) = To_String (Left)
               and then Element (Append'Result, Length (Left) + 1) = Right,

           Length (Left) = Max_Length and then Drop = Strings.Right
           =>
             Length (Append'Result) = Max_Length
               and then To_String (Append'Result) = To_String (Left),

           others  --  Drop = Left
           =>
             Length (Append'Result) = Max_Length
               and then
                 Slice (Append'Result, 1, Max_Length - 1) =
                   Slice (Left, 2, Max_Length)
               and then Element (Append'Result, Max_Length) = Right);

      function Append
        (Left  : Character;
         Right : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre            => (if Length (Right) = Max_Length then Drop /= Error),
        Contract_Cases =>
          (Length (Right) < Max_Length
           =>
             Length (Append'Result) = Length (Right) + 1
               and then
                 Slice (Append'Result, 2, Length (Right) + 1) =
                   To_String (Right)
               and then Element (Append'Result, 1) = Left,

           Length (Right) = Max_Length and then Drop = Strings.Left
           =>
             Length (Append'Result) = Max_Length
               and then To_String (Append'Result) = To_String (Right),

           others  --  Drop = Right
           =>
             Length (Append'Result) = Max_Length
               and then
                 Slice (Append'Result, 2, Max_Length) =
                   Slice (Right, 1, Max_Length - 1)
               and then Element (Append'Result, 1) = Left);

      --  Each of the procedures Append(Source, New_Item, Drop) has the same
      --  effect as the corresponding assignment
      --  Source := Append(Source, New_Item, Drop).

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : Bounded_String;
         Drop     : Truncation  := Error)
      with
        Pre            =>
          (if Length (Source) > Max_Length - Length (New_Item)
           then Drop /= Error),
        Contract_Cases =>
          (Length (Source) <= Max_Length - Length (New_Item)
           =>
             Length (Source) = Length (Source'Old) + Length (New_Item)
               and then
                 Slice (Source, 1, Length (Source'Old)) =
                   To_String (Source'Old)
               and then
                 (if Length (New_Item) > 0 then
                    Slice (Source, Length (Source'Old) + 1, Length (Source)) =
                      To_String (New_Item)),

           Length (Source) > Max_Length - Length (New_Item)
             and then Drop = Left
           =>
             Length (Source) = Max_Length
               and then
                 (if Length (New_Item) < Max_Length then
                    Slice (Source, 1, Max_Length - Length (New_Item)) =
                      Slice (Source'Old,
                        Length (Source'Old) - Max_Length + Length (New_Item)
                          + 1,
                        Length (Source'Old)))
               and then
                 Slice (Source, Max_Length - Length (New_Item) + 1, Max_Length)
                   = To_String (New_Item),

           others  --  Drop = Right
           =>
             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Length (Source'Old)) =
                   To_String (Source'Old)
               and then
                 (if Length (Source'Old) < Max_Length then
                    Slice (Source, Length (Source'Old) + 1, Max_Length) =
                      Slice (New_Item, 1, Max_Length - Length (Source'Old))));

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : String;
         Drop     : Truncation  := Error)
      with
        Pre            =>
          (if New_Item'Length > Max_Length - Length (Source)
           then Drop /= Error),
        Contract_Cases =>
          (Length (Source) <= Max_Length - New_Item'Length
           =>
             Length (Source) = Length (Source'Old) + New_Item'Length
               and then
                 Slice (Source, 1, Length (Source'Old)) =
                   To_String (Source'Old)
               and then
                 (if New_Item'Length > 0 then
                    Slice (Source, Length (Source'Old) + 1, Length (Source)) =
                      New_Item),

           Length (Source) > Max_Length - New_Item'Length
             and then Drop = Left
           =>
             Length (Source) = Max_Length
               and then
                 (if New_Item'Length < Max_Length then

                    --  The result is the end of Source followed by New_Item

                    Slice (Source, 1, Max_Length - New_Item'Length) =
                      Slice (Source'Old,
                        Length (Source'Old) - Max_Length + New_Item'Length + 1,
                        Length (Source'Old))
                    and then
                      Slice (Source,
                        Max_Length - New_Item'Length + 1, Max_Length) =
                          New_Item
                  else
                    --  The result is the last Max_Length characters of
                    --  New_Item.

                    To_String (Source) = New_Item
                      (New_Item'Last - Max_Length + 1 .. New_Item'Last)),

           others  --  Drop = Right
           =>
             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Length (Source'Old)) =
                   To_String (Source'Old)
               and then
                 (if Length (Source'Old) < Max_Length then
                    Slice (Source, Length (Source'Old) + 1, Max_Length) =
                      New_Item (New_Item'First
                        .. Max_Length - Length (Source'Old) - 1
                          + New_Item'First)));

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : Character;
         Drop     : Truncation  := Error)
      with
        Pre            => (if Length (Source) = Max_Length then Drop /= Error),
        Contract_Cases =>
          (Length (Source) < Max_Length
           =>
             Length (Source) = Length (Source'Old) + 1
               and then
                 Slice (Source, 1, Length (Source'Old)) =
                   To_String (Source'Old)
               and then Element (Source, Length (Source'Old) + 1) = New_Item,

           Length (Source) = Max_Length and then Drop = Right
           =>
             Length (Source) = Max_Length
               and then To_String (Source) = To_String (Source'Old),

           others  --  Drop = Left
           =>
             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Max_Length - 1) =
                   Slice (Source'Old, 2, Max_Length)
               and then Element (Source, Max_Length) = New_Item);

      --  Each of the "&" functions has the same effect as the corresponding
      --  Append function, with Error as the Drop parameter.

      function "&"
        (Left  : Bounded_String;
         Right : Bounded_String) return Bounded_String
      with
        Pre  => Length (Left) <= Max_Length - Length (Right),
        Post => Length ("&"'Result) = Length (Left) + Length (Right)
          and then Slice ("&"'Result, 1, Length (Left)) = To_String (Left)
          and then
            (if Length (Right) > 0 then
               Slice ("&"'Result, Length (Left) + 1, Length ("&"'Result)) =
                 To_String (Right));

      function "&"
        (Left  : Bounded_String;
         Right : String) return Bounded_String
      with
        Pre  => Right'Length <= Max_Length - Length (Left),
        Post => Length ("&"'Result) = Length (Left) + Right'Length
          and then Slice ("&"'Result, 1, Length (Left)) = To_String (Left)
          and then
            (if Right'Length > 0 then
               Slice ("&"'Result, Length (Left) + 1, Length ("&"'Result)) =
                 Right);

      function "&"
        (Left  : String;
         Right : Bounded_String) return Bounded_String
      with
        Pre  => Left'Length <= Max_Length - Length (Right),
        Post => Length ("&"'Result) = Left'Length + Length (Right)
          and then Slice ("&"'Result, 1, Left'Length) = Left
          and then
            (if Length (Right) > 0 then
               Slice ("&"'Result, Left'Length + 1, Length ("&"'Result)) =
                 To_String (Right));

      function "&"
        (Left  : Bounded_String;
         Right : Character) return Bounded_String
      with
        Pre  => Length (Left) < Max_Length,
        Post => Length ("&"'Result) = Length (Left) + 1
          and then Slice ("&"'Result, 1, Length (Left)) = To_String (Left)
          and then Element ("&"'Result, Length (Left) + 1) = Right;

      function "&"
        (Left  : Character;
         Right : Bounded_String) return Bounded_String
      with
        Pre  => Length (Right) < Max_Length,
        Post => Length ("&"'Result) = 1 + Length (Right)
          and then Element ("&"'Result, 1) = Left
          and then
            Slice ("&"'Result, 2, Length ("&"'Result)) = To_String (Right);

      function Element
        (Source : Bounded_String;
         Index  : Positive) return Character
      with
        Pre    => Index <= Length (Source),
        Global => null;
      --  Returns the character at position Index in the string represented by
      --  Source; propagates Index_Error if Index > Length(Source).

      procedure Replace_Element
        (Source : in out Bounded_String;
         Index  : Positive;
         By     : Character)
      with
        Pre    => Index <= Length (Source),
        Post   => Length (Source) = Length (Source'Old)
          and then (for all K in 1 .. Length (Source) =>
                      Element (Source, K) =
                        (if K = Index then By else Element (Source'Old, K))),
        Global => null;
      --  Updates Source such that the character at position Index in the
      --  string represented by Source is By; propagates Index_Error if
      --  Index > Length(Source).

      function Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural) return String
      with
        Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
        Global => null;
      --  Returns the slice at positions Low through High in the
      --  string represented by Source; propagates Index_Error if
      --  Low > Length(Source)+1 or High > Length(Source).
      --  The bounds of the returned string are Low and High.

      function Bounded_Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural) return Bounded_String
       with
        Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
        Post   => To_String (Bounded_Slice'Result) = Slice (Source, Low, High),
        Global => null;
      pragma Ada_05 (Bounded_Slice);
      --  Returns the slice at positions Low through High in the string
      --  represented by Source as a bounded string; propagates Index_Error
      --  if Low > Length(Source)+1 or High > Length(Source).

      procedure Bounded_Slice
        (Source : Bounded_String;
         Target : out Bounded_String;
         Low    : Positive;
         High   : Natural)
      with
        Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
        Post   => To_String (Target) = Slice (Source, Low, High),
        Global => null;
      pragma Ada_05 (Bounded_Slice);
      --  Equivalent to Target := Bounded_Slice (Source, Low, High);

      --  Each of the functions "=", "<", ">", "<=", and ">=" returns the same
      --  result as the corresponding String operation applied to the String
      --  values given or represented by the two parameters.

      function "="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Post   => "="'Result = (To_String (Left) = To_String (Right)),
        Global => null;

      function "="
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Post   => "="'Result = (To_String (Left) = Right),
        Global => null;

      function "="
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Post   => "="'Result = (Left = To_String (Right)),
        Global => null;

      function "<"
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Post   => "<"'Result = (To_String (Left) < To_String (Right)),
        Global => null;

      function "<"
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Post   => "<"'Result = (To_String (Left) < Right),
        Global => null;

      function "<"
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Post   => "<"'Result = (Left < To_String (Right)),
        Global => null;

      function "<="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Post   => "<="'Result = (To_String (Left) <= To_String (Right)),
        Global => null;

      function "<="
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Post   => "<="'Result = (To_String (Left) <= Right),
        Global => null;

      function "<="
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Post   => "<="'Result = (Left <= To_String (Right)),
        Global => null;

      function ">"
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Post   => ">"'Result = (To_String (Left) > To_String (Right)),
        Global => null;

      function ">"
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Post   => ">"'Result = (To_String (Left) > Right),
        Global => null;

      function ">"
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Post   => ">"'Result = (Left > To_String (Right)),
        Global => null;

      function ">="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Post   => ">="'Result = (To_String (Left) >= To_String (Right)),
        Global => null;

      function ">="
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Post   => ">="'Result = (To_String (Left) >= Right),
        Global => null;

      function ">="
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Post   => ">="'Result = (Left >= To_String (Right)),
        Global => null;

      ----------------------
      -- Search Functions --
      ----------------------

      --  Each of the search subprograms (Index, Index_Non_Blank, Count,
      --  Find_Token) has the same effect as the corresponding subprogram in
      --  Strings.Fixed applied to the string represented by the Bounded_String
      --  parameter.

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
      with
        Pre            => Pattern'Length > 0,
        Post           => Index'Result <= Length (Source),
        Contract_Cases =>

          --  If Source is the empty string, then 0 is returned

          (Length (Source) = 0
           =>
             Index'Result = 0,

           --  If some slice of Source matches Pattern, then a valid index is
           --  returned.

           Length (Source) > 0
             and then
               (for some J in 1 .. Length (Source) - (Pattern'Length - 1) =>
                  Search.Match (To_String (Source), Pattern, Mapping, J))
           =>
             --  The result is in the considered range of Source

             Index'Result in 1 .. Length (Source) - (Pattern'Length - 1)

               --  The slice beginning at the returned index matches Pattern

               and then Search.Match
                 (To_String (Source), Pattern, Mapping, Index'Result)

               --  The result is the smallest or largest index which satisfies
               --  the matching, respectively when Going = Forward and Going =
               --  Backward.

               and then
                 (for all J in 1 .. Length (Source) =>
                    (if (if Going = Forward
                         then J <= Index'Result - 1
                         else J - 1 in Index'Result
                                       .. Length (Source) - Pattern'Length)
                     then not (Search.Match
                       (To_String (Source), Pattern, Mapping, J)))),

           --  Otherwise, 0 is returned

           others
           =>
             Index'Result = 0),
        Global         => null;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping_Function) return Natural
      with
        Pre            => Pattern'Length /= 0 and then Mapping /= null,
        Post           => Index'Result <= Length (Source),
        Contract_Cases =>

          --  If Source is the empty string, then 0 is returned

          (Length (Source) = 0
           =>
             Index'Result = 0,

           --  If some slice of Source matches Pattern, then a valid index is
           --  returned.

           Length (Source) > 0
             and then
               (for some J in 1 .. Length (Source) - (Pattern'Length - 1) =>
                  Search.Match (To_String (Source), Pattern, Mapping, J))
           =>
             --  The result is in the considered range of Source

             Index'Result in 1 .. Length (Source) - (Pattern'Length - 1)

               --  The slice beginning at the returned index matches Pattern

               and then Search.Match
                 (To_String (Source), Pattern, Mapping, Index'Result)

               --  The result is the smallest or largest index which satisfies
               --  the matching, respectively when Going = Forward and Going =
               --  Backward.

               and then
                 (for all J in 1 .. Length (Source) =>
                    (if (if Going = Forward
                         then J <= Index'Result - 1
                         else J - 1 in Index'Result
                                       .. Length (Source) - Pattern'Length)
                     then not (Search.Match
                       (To_String (Source), Pattern, Mapping, J)))),

           --  Otherwise, 0 is returned

           others
           =>
             Index'Result = 0),
        Global         => null;

      function Index
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         Test   : Membership := Inside;
         Going  : Direction  := Forward) return Natural
      with
        Post           => Index'Result <= Length (Source),
        Contract_Cases =>

           --  If no character of Source satisfies the property Test on Set,
           --  then 0 is returned.

          ((for all C of To_String (Source) =>
              (Test = Inside) /= Maps.Is_In (C, Set))
           =>
             Index'Result = 0,

           --  Otherwise, an index in the range of Source is returned

           others
           =>
             --  The result is in the range of Source

             Index'Result in 1 .. Length (Source)

               --  The character at the returned index satisfies the property
               --  Test on Set.

               and then
                 (Test = Inside) =
                   Maps.Is_In (Element (Source, Index'Result), Set)

               --  The result is the smallest or largest index which satisfies
               --  the property, respectively when Going = Forward and Going =
               --  Backward.

               and then
                 (for all J in 1 .. Length (Source) =>
                    (if J /= Index'Result
                          and then (J < Index'Result) = (Going = Forward)
                     then (Test = Inside)
                          /= Maps.Is_In (Element (Source, J), Set)))),
        Global         => null;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
      with
        Pre            =>
          (if Length (Source) /= 0 then From <= Length (Source))
            and then Pattern'Length /= 0,
        Post           => Index'Result <= Length (Source),
        Contract_Cases =>

          --  If Source is the empty string, then 0 is returned

          (Length (Source) = 0
           =>
             Index'Result = 0,

           --  If some slice of Source matches Pattern, then a valid index is
           --  returned.

           Length (Source) > 0
             and then
               (for some J in
                 (if Going = Forward then From else 1)
                  .. (if Going = Forward then Length (Source) else From)
                   - (Pattern'Length - 1) =>
                 Search.Match (To_String (Source), Pattern, Mapping, J))
           =>
             --  The result is in the considered range of Source

             Index'Result in
               (if Going = Forward then From else 1)
               .. (if Going = Forward then Length (Source) else From)
                - (Pattern'Length - 1)

               --  The slice beginning at the returned index matches Pattern

               and then Search.Match
                 (To_String (Source), Pattern, Mapping, Index'Result)

               --  The result is the smallest or largest index which satisfies
               --  the matching, respectively when Going = Forward and Going =
               --  Backward.

               and then
                 (for all J in 1 .. Length (Source) =>
                    (if (if Going = Forward
                         then J in From .. Index'Result - 1
                         else J - 1 in Index'Result
                                       .. From - Pattern'Length)
                     then not (Search.Match
                       (To_String (Source), Pattern, Mapping, J)))),

           --  Otherwise, 0 is returned

           others
           =>
             Index'Result = 0),
        Global         => null;
      pragma Ada_05 (Index);

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping_Function) return Natural
      with
        Pre            =>
          (if Length (Source) /= 0 then From <= Length (Source))
            and then Pattern'Length /= 0
            and then Mapping /= null,
        Post           => Index'Result <= Length (Source),
        Contract_Cases =>

          --  If Source is the empty string, then 0 is returned

          (Length (Source) = 0
           =>
             Index'Result = 0,

           --  If some slice of Source matches Pattern, then a valid index is
           --  returned.

           Length (Source) > 0
             and then
               (for some J in
                 (if Going = Forward then From else 1)
                  .. (if Going = Forward then Length (Source) else From)
                   - (Pattern'Length - 1) =>
                 Search.Match (To_String (Source), Pattern, Mapping, J))
           =>
             --  The result is in the considered range of Source

             Index'Result in
               (if Going = Forward then From else 1)
               .. (if Going = Forward then Length (Source) else From)
                - (Pattern'Length - 1)

               --  The slice beginning at the returned index matches Pattern

               and then Search.Match
                 (To_String (Source), Pattern, Mapping, Index'Result)

               --  The result is the smallest or largest index which satisfies
               --  the matching, respectively when Going = Forward and Going =
               --  Backward.

               and then
                 (for all J in 1 .. Length (Source) =>
                    (if (if Going = Forward
                         then J in From .. Index'Result - 1
                         else J - 1 in Index'Result
                                       .. From - Pattern'Length)
                     then not (Search.Match
                       (To_String (Source), Pattern, Mapping, J)))),

           --  Otherwise, 0 is returned

           others
           =>
             Index'Result = 0),
        Global         => null;
      pragma Ada_05 (Index);

      function Index
        (Source  : Bounded_String;
         Set     : Maps.Character_Set;
         From    : Positive;
         Test    : Membership := Inside;
         Going   : Direction := Forward) return Natural
      with
        Pre            =>
          (if Length (Source) /= 0 then From <= Length (Source)),
        Post           => Index'Result <= Length (Source),
        Contract_Cases =>

           --  If Source is the empty string, or no character of the considered
           --  slice of Source satisfies the property Test on Set, then 0 is
           --  returned.

          (Length (Source) = 0
             or else
               (for all J in 1 .. Length (Source) =>
                  (if J = From or else (J > From) = (Going = Forward) then
                     (Test = Inside) /= Maps.Is_In (Element (Source, J), Set)))
           =>
             Index'Result = 0,

           --  Otherwise, an index in the considered range of Source is
           --  returned.

           others
           =>
             --  The result is in the considered range of Source

             Index'Result in 1 .. Length (Source)
               and then
                 (Index'Result = From
                    or else (Index'Result > From) = (Going = Forward))

               --  The character at the returned index satisfies the property
               --  Test on Set.

               and then
                 (Test = Inside) =
                   Maps.Is_In (Element (Source, Index'Result), Set)

               --  The result is the smallest or largest index which satisfies
               --  the property, respectively when Going = Forward and Going =
               --  Backward.

               and then
                 (for all J in 1 .. Length (Source) =>
                    (if J /= Index'Result
                       and then (J < Index'Result) = (Going = Forward)
                       and then (J = From
                                   or else (J > From) = (Going = Forward))
                     then (Test = Inside)
                          /= Maps.Is_In (Element (Source, J), Set)))),
        Global         => null;
      pragma Ada_05 (Index);

      function Index_Non_Blank
        (Source : Bounded_String;
         Going  : Direction := Forward) return Natural
      with
        Post           => Index_Non_Blank'Result <= Length (Source),
        Contract_Cases =>

           --  If all characters of Source are Space characters, then 0 is
           --  returned.

          ((for all C of To_String (Source) => C = ' ')
           =>
             Index_Non_Blank'Result = 0,

           --  Otherwise, an index in the range of Source is returned

           others
           =>
             --  The result is in the range of Source

             Index_Non_Blank'Result in 1 .. Length (Source)

               --  The character at the returned index is not a Space character

               and then Element (Source, Index_Non_Blank'Result) /= ' '

               --  The result is the smallest or largest index which is not a
               --  Space character, respectively when Going = Forward and Going
               --  = Backward.

               and then
                 (for all J in 1 .. Length (Source) =>
                    (if J /= Index_Non_Blank'Result
                          and then
                            (J < Index_Non_Blank'Result) = (Going = Forward)
                     then Element (Source, J) = ' '))),
        Global         => null;

      function Index_Non_Blank
        (Source : Bounded_String;
         From   : Positive;
         Going  : Direction := Forward) return Natural
      with
        Pre            =>
          (if Length (Source) /= 0 then From <= Length (Source)),
        Post           => Index_Non_Blank'Result <= Length (Source),
        Contract_Cases =>

           --  If Source is the empty string, or all characters of the
           --  considered slice of Source are Space characters, then 0
           --  is returned.

          (Length (Source) = 0
             or else
               (for all J in 1 .. Length (Source) =>
                  (if J = From or else (J > From) = (Going = Forward) then
                     Element (Source, J) = ' '))
           =>
             Index_Non_Blank'Result = 0,

           --  Otherwise, an index in the considered range of Source is
           --  returned.

           others
           =>
             --  The result is in the considered range of Source

             Index_Non_Blank'Result in 1 .. Length (Source)
               and then
                 (Index_Non_Blank'Result = From
                    or else
                      (Index_Non_Blank'Result > From) = (Going = Forward))

               --  The character at the returned index is not a Space character

               and then Element (Source, Index_Non_Blank'Result) /= ' '

               --  The result is the smallest or largest index which isn't a
               --  Space character, respectively when Going = Forward and Going
               --  = Backward.

               and then
                 (for all J in 1 .. Length (Source) =>
                    (if J /= Index_Non_Blank'Result
                       and then
                         (J < Index_Non_Blank'Result) = (Going = Forward)
                       and then (J = From
                                   or else (J > From) = (Going = Forward))
                     then Element (Source, J) = ' '))),
        Global         => null;
      pragma Ada_05 (Index_Non_Blank);

      function Count
        (Source  : Bounded_String;
         Pattern : String;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
      with
        Pre    => Pattern'Length /= 0,
        Global => null;

      function Count
        (Source  : Bounded_String;
         Pattern : String;
         Mapping : Maps.Character_Mapping_Function) return Natural
      with
        Pre    => Pattern'Length /= 0 and then Mapping /= null,
        Global => null;

      function Count
        (Source : Bounded_String;
         Set    : Maps.Character_Set) return Natural
      with
        Global => null;

      procedure Find_Token
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         From   : Positive;
         Test   : Membership;
         First  : out Positive;
         Last   : out Natural)
      with
        Pre            =>
          (if Length (Source) /= 0 then From <= Length (Source)),
        Contract_Cases =>

           --  If Source is the empty string, or if no character of the
           --  considered slice of Source satisfies the property Test on
           --  Set, then First is set to From and Last is set to 0.

          (Length (Source) = 0
             or else
               (for all J in From .. Length (Source) =>
                  (Test = Inside) /= Maps.Is_In (Element (Source, J), Set))
           =>
             First = From and then Last = 0,

           --  Otherwise, First and Last are set to valid indexes

           others
           =>
             --  First and Last are in the considered range of Source

             First in From .. Length (Source)
               and then Last in First .. Length (Source)

               --  No character between From and First satisfies the property
               --  Test on Set.

               and then
                 (for all J in From .. First - 1 =>
                    (Test = Inside) /= Maps.Is_In (Element (Source, J), Set))

               --  All characters between First and Last satisfy the property
               --  Test on Set.

               and then
                 (for all J in First .. Last =>
                    (Test = Inside) = Maps.Is_In (Element (Source, J), Set))

               --  If Last is not Source'Last, then the character at position
               --  Last + 1 does not satify the property Test on Set.

               and then
                 (if Last < Length (Source)
                  then
                    (Test = Inside)
                    /= Maps.Is_In (Element (Source, Last + 1), Set))),
        Global         => null;
      pragma Ada_2012 (Find_Token);

      procedure Find_Token
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         Test   : Membership;
         First  : out Positive;
         Last   : out Natural)
      with
        Contract_Cases =>

           --  If Source is the empty string, or if no character of the
           --  considered slice of Source satisfies the property Test on
           --  Set, then First is set to 1 and Last is set to 0.

          (Length (Source) = 0
             or else
               (for all J in 1 .. Length (Source) =>
                  (Test = Inside) /= Maps.Is_In (Element (Source, J), Set))
           =>
             First = 1 and then Last = 0,

           --  Otherwise, First and Last are set to valid indexes

           others
           =>
             --  First and Last are in the considered range of Source

             First in 1 .. Length (Source)
               and then Last in First .. Length (Source)

               --  No character between 1 and First satisfies the property Test
               --  on Set.

               and then
                 (for all J in 1 .. First - 1 =>
                    (Test = Inside) /= Maps.Is_In (Element (Source, J), Set))

               --  All characters between First and Last satisfy the property
               --  Test on Set.

               and then
                 (for all J in First .. Last =>
                    (Test = Inside) = Maps.Is_In (Element (Source, J), Set))

               --  If Last is not Source'Last, then the character at position
               --  Last + 1 does not satify the property Test on Set.

               and then
                 (if Last < Length (Source)
                  then
                    (Test = Inside)
                    /= Maps.Is_In (Element (Source, Last + 1), Set))),
        Global         => null;

      ------------------------------------
      -- String Translation Subprograms --
      ------------------------------------

      --  Each of the Translate subprograms, when applied to a Bounded_String,
      --  has an analogous effect to the corresponding subprogram in
      --  Strings.Fixed. For the Translate function, the translation is applied
      --  to the string represented by the Bounded_String parameter, and the
      --  result is converted (via To_Bounded_String) to a Bounded_String. For
      --  the Translate procedure, the string represented by the Bounded_String
      --  parameter after the translation is given by the Translate function
      --  for fixed-length strings applied to the string represented by the
      --  original value of the parameter.

      function Translate
        (Source  : Bounded_String;
         Mapping : Maps.Character_Mapping) return Bounded_String
      with
        Post   => Length (Translate'Result) = Length (Source)
          and then
            (for all K in 1 .. Length (Source) =>
               Element (Translate'Result, K) =
                 Ada.Strings.Maps.Value (Mapping, Element (Source, K))),
        Global => null;

      procedure Translate
        (Source   : in out Bounded_String;
         Mapping  : Maps.Character_Mapping)
      with
        Post   => Length (Source) = Length (Source'Old)
          and then
            (for all K in 1 .. Length (Source) =>
               Element (Source, K) =
                 Ada.Strings.Maps.Value (Mapping, Element (Source'Old, K))),
        Global => null;

      function Translate
        (Source  : Bounded_String;
         Mapping : Maps.Character_Mapping_Function) return Bounded_String
      with
        Pre    => Mapping /= null,
        Post   => Length (Translate'Result) = Length (Source)
          and then
            (for all K in 1 .. Length (Source) =>
               Element (Translate'Result, K) = Mapping (Element (Source, K))),
        Global => null;
      pragma Annotate (GNATprove, False_Positive,
                       "call via access-to-subprogram",
                       "function Mapping must always terminate");

      procedure Translate
        (Source  : in out Bounded_String;
         Mapping : Maps.Character_Mapping_Function)
      with
        Pre    => Mapping /= null,
        Post   => Length (Source) = Length (Source'Old)
          and then
            (for all K in 1 .. Length (Source) =>
               Element (Source, K) = Mapping (Element (Source'Old, K))),
        Global => null;
      pragma Annotate (GNATprove, False_Positive,
                       "call via access-to-subprogram",
                       "function Mapping must always terminate");

      ---------------------------------------
      -- String Transformation Subprograms --
      ---------------------------------------

      --  Each of the transformation subprograms (Replace_Slice, Insert,
      --  Overwrite, Delete), selector subprograms (Trim, Head, Tail), and
      --  constructor functions ("*") has an effect based on its corresponding
      --  subprogram in Strings.Fixed, and Replicate is based on Fixed."*".
      --  In the case of a function, the corresponding fixed-length string
      --  subprogram is applied to the string represented by the Bounded_String
      --  parameter. To_Bounded_String is applied the result string, with Drop
      --  (or Error in the case of Generic_Bounded_Length."*") determining
      --  the effect when the string length exceeds Max_Length. In
      --  the case of a procedure, the corresponding function in
      --  Strings.Bounded.Generic_Bounded_Length is applied, with the
      --  result assigned into the Source parameter.

      function Replace_Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural;
         By     : String;
         Drop   : Truncation := Error) return Bounded_String
      with
        Pre            =>
          Low - 1 <= Length (Source)
            and then
              (if Drop = Error
                 then (if High >= Low
                         then Low - 1
                           <= Max_Length - By'Length
                            - Integer'Max (Length (Source) - High, 0)
                         else Length (Source) <= Max_Length - By'Length)),
        Contract_Cases =>
          (Low - 1 <= Max_Length - By'Length
             - Integer'Max (Length (Source) - Integer'Max (High, Low - 1), 0)
           =>
             --  Total length is lower than Max_Length: nothing is dropped

             --  Note that if High < Low, the insertion is done before Low,
             --  so in all cases the starting position of the slice of Source
             --  remaining after the replaced Slice is Integer'Max (High + 1,
             --  Low).

             Length (Replace_Slice'Result) = Low - 1 + By'Length
               + Integer'Max (Length (Source) - Integer'Max (High, Low - 1), 0)
               and then
                 Slice (Replace_Slice'Result, 1, Low - 1) =
                   Slice (Source, 1, Low - 1)
               and then
                 Slice (Replace_Slice'Result, Low, Low - 1 + By'Length) = By
               and then
                 (if Integer'Max (High, Low - 1) < Length (Source) then
                    Slice (Replace_Slice'Result,
                      Low + By'Length, Length (Replace_Slice'Result)) =
                        Slice (Source,
                          Integer'Max (High + 1, Low), Length (Source))),

           Low - 1 > Max_Length - By'Length
             - Integer'Max (Length (Source) - Integer'Max (High, Low - 1), 0)
             and then Drop = Left
           =>
             --  Final_Slice is the length of the slice of Source remaining
             --  after the replaced part.
             (declare
                Final_Slice : constant Natural :=
                  Integer'Max
                    (Length (Source) - Integer'Max (High, Low - 1), 0);
              begin
                --  The result is of maximal length and ends by the last
                --  Final_Slice characters of Source.

                Length (Replace_Slice'Result) = Max_Length
                  and then
                    (if Final_Slice > 0 then
                       Slice (Replace_Slice'Result,
                         Max_Length - Final_Slice + 1, Max_Length) =
                           Slice (Source,
                             Integer'Max (High + 1, Low), Length (Source)))

                  --  Depending on when we reach Max_Length, either the first
                  --  part of Source is fully dropped and By is partly dropped,
                  --  or By is fully added and the first part of Source is
                  --  partly dropped.

                  and then
                   (if Max_Length - Final_Slice - By'Length <= 0 then

                      --  The first (possibly zero) characters of By are
                      --  dropped.

                      (if Final_Slice < Max_Length then
                         Slice (Replace_Slice'Result,
                           1, Max_Length - Final_Slice) =
                             By (By'Last - Max_Length + Final_Slice + 1
                                 .. By'Last))

                    else  --  By is added to the result

                      Slice (Replace_Slice'Result,
                        Max_Length - Final_Slice - By'Length + 1,
                        Max_Length - Final_Slice) =
                          By

                        --  The first characters of Source (1 .. Low - 1) are
                        --  dropped.

                        and then Slice (Replace_Slice'Result, 1,
                          Max_Length - Final_Slice - By'Length) =
                            Slice (Source,
                              Low - Max_Length + Final_Slice + By'Length,
                              Low - 1))),

           others  --  Drop = Right
           =>
             --  The result is of maximal length and starts by the first Low -
             --  1 characters of Source.

             Length (Replace_Slice'Result) = Max_Length
               and then
                 Slice (Replace_Slice'Result, 1, Low - 1) =
                   Slice (Source, 1, Low - 1)

               --  Depending on when we reach Max_Length, either the last part
               --  of Source is fully dropped and By is partly dropped, or By
               --  is fully added and the last part of Source is partly
               --  dropped.

               and then
                 (if Low - 1 >= Max_Length - By'Length then

                    --  The last characters of By are dropped

                    Slice (Replace_Slice'Result, Low, Max_Length) =
                      By (By'First .. Max_Length - Low + By'First)

                  else  --  By is fully added

                    Slice (Replace_Slice'Result, Low, Low + By'Length - 1) = By

                      --  Then Source starting from Integer'Max (High + 1, Low)
                      --  is added but the last characters are dropped.

                      and then Slice (Replace_Slice'Result,
                        Low + By'Length, Max_Length) =
                          Slice (Source, Integer'Max (High + 1, Low),
                            Integer'Max (High + 1, Low) +
                              (Max_Length - Low - By'Length))));

      procedure Replace_Slice
        (Source   : in out Bounded_String;
         Low      : Positive;
         High     : Natural;
         By       : String;
         Drop     : Truncation := Error)
      with
        Pre            =>
          Low - 1 <= Length (Source)
            and then
              (if Drop = Error
                 then (if High >= Low
                         then Low - 1
                           <= Max_Length - By'Length
                            - Natural'Max (Length (Source) - High, 0)
                         else Length (Source) <= Max_Length - By'Length)),
        Contract_Cases =>
          (Low - 1 <= Max_Length - By'Length
             - Integer'Max (Length (Source) - Integer'Max (High, Low - 1), 0)
           =>
             --  Total length is lower than Max_Length: nothing is dropped

             --  Note that if High < Low, the insertion is done before Low,
             --  so in all cases the starting position of the slice of Source
             --  remaining after the replaced Slice is Integer'Max (High + 1,
             --  Low).

             Length (Source) = Low - 1 + By'Length + Integer'Max
               (Length (Source'Old) - Integer'Max (High, Low - 1), 0)
               and then
                 Slice (Source, 1, Low - 1) = Slice (Source'Old, 1, Low - 1)
               and then Slice (Source, Low, Low - 1 + By'Length) = By
               and then
                 (if Integer'Max (High, Low - 1) < Length (Source'Old) then
                    Slice (Source, Low + By'Length, Length (Source)) =
                      Slice (Source'Old,
                        Integer'Max (High + 1, Low), Length (Source'Old))),

           Low - 1 > Max_Length - By'Length
             - Integer'Max (Length (Source) - Integer'Max (High, Low - 1), 0)
             and then Drop = Left
           =>
             --  Final_Slice is the length of the slice of Source remaining
             --  after the replaced part.
             (declare
                Final_Slice : constant Integer :=
                  Integer'Max (0,
                    Length (Source'Old) - Integer'Max (High, Low - 1));
              begin
                --  The result is of maximal length and ends by the last
                --  Final_Slice characters of Source.

                Length (Source) = Max_Length
                  and then
                    (if Final_Slice > 0 then
                       Slice (Source,
                         Max_Length - Final_Slice + 1, Max_Length) =
                           Slice (Source'Old,
                             Integer'Max (High + 1, Low), Length (Source'Old)))

                  --  Depending on when we reach Max_Length, either the first
                  --  part of Source is fully dropped and By is partly dropped,
                  --  or By is fully added and the first part of Source is
                  --  partly dropped.

                  and then
                    (if Max_Length - Final_Slice - By'Length <= 0 then

                       --  The first characters of By are dropped

                       (if Final_Slice < Max_Length then
                          Slice (Source, 1, Max_Length - Final_Slice) =
                            By (By'Last - Max_Length + Final_Slice + 1
                                .. By'Last))

                     else  --  By is added to the result

                       Slice (Source,
                         Max_Length - Final_Slice - By'Length + 1,
                         Max_Length - Final_Slice) = By

                         --  The first characters of Source (1 .. Low - 1) are
                         --  dropped.

                         and then Slice (Source, 1,
                           Max_Length - Final_Slice - By'Length) =
                             Slice (Source'Old,
                               Low - Max_Length + Final_Slice + By'Length,
                               Low - 1))),

           others  --  Drop = Right
           =>
             --  The result is of maximal length and starts by the first Low -
             --  1 characters of Source.

             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Low - 1) = Slice (Source'Old, 1, Low - 1)

               --  Depending on when we reach Max_Length, either the last part
               --  of Source is fully dropped and By is partly dropped, or By
               --  is fully added and the last part of Source is partly
               --  dropped.

               and then
                 (if Low - 1 >= Max_Length - By'Length then

                    --  The last characters of By are dropped

                    Slice (Source, Low, Max_Length) =
                      By (By'First .. Max_Length - Low + By'First)

                  else  --  By is fully added

                    Slice (Source, Low, Low + By'Length - 1) = By

                      --  Then Source starting from Natural'Max (High + 1, Low)
                      --  is added but the last characters are dropped.

                      and then Slice (Source, Low + By'Length, Max_Length) =
                        Slice (Source'Old, Integer'Max (High + 1, Low),
                          Integer'Max (High + 1, Low) +
                            (Max_Length - Low - By'Length))));

      function Insert
        (Source   : Bounded_String;
         Before   : Positive;
         New_Item : String;
         Drop     : Truncation := Error) return Bounded_String
      with
        Pre            =>
          Before - 1 <= Length (Source)
            and then (if New_Item'Length > Max_Length - Length (Source)
                      then Drop /= Error),
        Contract_Cases =>
          (Length (Source) <= Max_Length - New_Item'Length
           =>
             --  Total length is lower than Max_Length: nothing is dropped

             Length (Insert'Result) = Length (Source) + New_Item'Length
               and then
                 Slice (Insert'Result, 1, Before - 1) =
                   Slice (Source, 1, Before - 1)
               and then
                 Slice (Insert'Result, Before, Before - 1 + New_Item'Length) =
                   New_Item
               and then
                 (if Before <= Length (Source) then
                    Slice (Insert'Result,
                      Before + New_Item'Length, Length (Insert'Result)) =
                        Slice (Source, Before, Length (Source))),

           Length (Source) > Max_Length - New_Item'Length and then Drop = Left
           =>
             --  The result is of maximal length and ends by the last
             --  characters of Source.

             Length (Insert'Result) = Max_Length
               and then
                 (if Before <= Length (Source) then
                    Slice (Insert'Result,
                      Max_Length - Length (Source) + Before, Max_Length) =
                        Slice (Source, Before, Length (Source)))

               --  Depending on when we reach Max_Length, either the first part
               --  of Source is fully dropped and New_Item is partly dropped,
               --  or New_Item is fully added and the first part of Source is
               --  partly dropped.

               and then
                 (if Max_Length - Length (Source) - 1 + Before
                   < New_Item'Length
                  then
                    --  The first characters of New_Item are dropped

                    (if Length (Source) - Before + 1 < Max_Length then
                       Slice (Insert'Result,
                         1, Max_Length - Length (Source) - 1 + Before) =
                           New_Item
                             (New_Item'Last - Max_Length + Length (Source)
                               - Before + 2
                              .. New_Item'Last))

                  else  --  New_Item is added to the result

                    Slice (Insert'Result,
                      Max_Length - Length (Source) - New_Item'Length + Before,
                      Max_Length - Length (Source) - 1 + Before) = New_Item

                      --  The first characters of Source (1 .. Before - 1) are
                      --  dropped.

                      and then Slice (Insert'Result,
                        1, Max_Length - Length (Source) - New_Item'Length
                          - 1 + Before) =
                            Slice (Source,
                              Length (Source) - Max_Length + New_Item'Length
                                + 1,
                              Before - 1)),

           others  --  Drop = Right
           =>
             --  The result is of maximal length and starts by the first
             --  characters of Source.

             Length (Insert'Result) = Max_Length
               and then
                 Slice (Insert'Result, 1, Before - 1) =
                   Slice (Source, 1, Before - 1)

               --  Depending on when we reach Max_Length, either the last part
               --  of Source is fully dropped and New_Item is partly dropped,
               --  or New_Item is fully added and the last part of Source is
               --  partly dropped.

               and then
                 (if Before - 1 >= Max_Length - New_Item'Length then

                    --  The last characters of New_Item are dropped

                    Slice (Insert'Result, Before, Max_Length) =
                      New_Item (New_Item'First
                        .. Max_Length - Before + New_Item'First)

                  else  --  New_Item is fully added

                    Slice (Insert'Result,
                      Before, Before + New_Item'Length - 1) =
                        New_Item

                      --  Then Source starting from Before is added but the
                      --  last characters are dropped.

                      and then Slice (Insert'Result,
                        Before + New_Item'Length, Max_Length) =
                          Slice (Source,
                            Before, Max_Length - New_Item'Length)));

      procedure Insert
        (Source   : in out Bounded_String;
         Before   : Positive;
         New_Item : String;
         Drop     : Truncation := Error)
      with
        Pre            =>
          Before - 1 <= Length (Source)
            and then (if New_Item'Length > Max_Length - Length (Source)
                      then Drop /= Error),
        Contract_Cases =>
          (Length (Source) <= Max_Length - New_Item'Length
           =>
             --  Total length is lower than Max_Length: nothing is dropped

             Length (Source) = Length (Source'Old) + New_Item'Length
               and then
                 Slice (Source, 1, Before - 1) =
                   Slice (Source'Old, 1, Before - 1)
               and then
                 Slice (Source, Before, Before - 1 + New_Item'Length) =
                   New_Item
               and then
                 (if Before <= Length (Source'Old) then
                    Slice (Source, Before + New_Item'Length, Length (Source)) =
                      Slice (Source'Old, Before, Length (Source'Old))),

           Length (Source) > Max_Length - New_Item'Length and then Drop = Left
           =>
             --  The result is of maximal length and ends by the last
             --  characters of Source.

             Length (Source) = Max_Length
               and then
                 (if Before <= Length (Source'Old) then
                    Slice (Source,
                      Max_Length - Length (Source'Old) + Before, Max_Length) =
                        Slice (Source'Old, Before, Length (Source'Old)))

               --  Depending on when we reach Max_Length, either the first part
               --  of Source is fully dropped and New_Item is partly dropped,
               --  or New_Item is fully added and the first part of Source is
               --  partly dropped.

               and then
                 (if Max_Length - Length (Source'Old) - 1 + Before
                   < New_Item'Length
                  then
                    --  The first characters of New_Item are dropped

                    (if Length (Source'Old) - Before + 1 < Max_Length then
                       Slice (Source,
                         1, Max_Length - Length (Source'Old) - 1 + Before) =
                           New_Item
                             (New_Item'Last - Max_Length + Length (Source'Old)
                               - Before + 2
                              .. New_Item'Last))

                  else  --  New_Item is added to the result

                    Slice (Source,
                      Max_Length - Length (Source'Old) - New_Item'Length
                        + Before,
                      Max_Length - Length (Source'Old) - 1 + Before) = New_Item

                      --  The first characters of Source (1 .. Before - 1) are
                      --  dropped.

                      and then Slice (Source, 1,
                        Max_Length - Length (Source'Old) - New_Item'Length
                          - 1 + Before) =
                            Slice (Source'Old,
                              Length (Source'Old)
                                - Max_Length + New_Item'Length + 1,
                              Before - 1)),

           others  --  Drop = Right
           =>
             --  The result is of maximal length and starts by the first
             --  characters of Source.

             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Before - 1) =
                   Slice (Source'Old, 1, Before - 1)

               --  Depending on when we reach Max_Length, either the last part
               --  of Source is fully dropped and New_Item is partly dropped,
               --  or New_Item is fully added and the last part of Source is
               --  partly dropped.

               and then
                 (if Before - 1 >= Max_Length - New_Item'Length then

                    --  The last characters of New_Item are dropped

                    Slice (Source, Before, Max_Length) =
                      New_Item (New_Item'First
                        .. Max_Length - Before + New_Item'First)

                  else  --  New_Item is fully added

                    Slice (Source, Before, Before + New_Item'Length - 1) =
                      New_Item

                      --  Then Source starting from Before is added but the
                      --  last characters are dropped.

                      and then
                        Slice (Source, Before + New_Item'Length, Max_Length) =
                          Slice (Source'Old,
                            Before, Max_Length - New_Item'Length)));

      function Overwrite
        (Source   : Bounded_String;
         Position : Positive;
         New_Item : String;
         Drop     : Truncation := Error) return Bounded_String
      with
        Pre            =>
          Position - 1 <= Length (Source)
            and then (if New_Item'Length > Max_Length - (Position - 1)
                      then Drop /= Error),
        Contract_Cases =>
          (Position - 1 <= Max_Length - New_Item'Length
           =>
             --  The length is unchanged, unless New_Item overwrites further
             --  than the end of Source. In this contract case, we suppose
             --  New_Item doesn't overwrite further than Max_Length.

             Length (Overwrite'Result) =
               Integer'Max (Length (Source), Position - 1 + New_Item'Length)
               and then
                 Slice (Overwrite'Result, 1, Position - 1) =
                   Slice (Source, 1, Position - 1)
               and then Slice (Overwrite'Result,
                 Position, Position - 1 + New_Item'Length) =
                   New_Item
               and then
                 (if Position - 1 + New_Item'Length < Length (Source) then

                    --  There are some unchanged characters of Source remaining
                    --  after New_Item.

                    Slice (Overwrite'Result,
                      Position + New_Item'Length, Length (Source)) =
                        Slice (Source,
                          Position + New_Item'Length, Length (Source))),

           Position - 1 > Max_Length - New_Item'Length and then Drop = Left
           =>
             Length (Overwrite'Result) = Max_Length

               --  If a part of the result has to be dropped, it means New_Item
               --  is overwriting further than the end of Source. Thus the
               --  result is necessarily ending by New_Item. However, we don't
               --  know whether New_Item covers all Max_Length characters or
               --  some characters of Source are remaining at the left.

               and then
                 (if New_Item'Length >= Max_Length then

                    --  New_Item covers all Max_Length characters

                    To_String (Overwrite'Result) =
                      New_Item
                        (New_Item'Last - Max_Length + 1 .. New_Item'Last)
                  else
                    --  New_Item fully appears at the end

                    Slice (Overwrite'Result,
                      Max_Length - New_Item'Length + 1, Max_Length) =
                        New_Item

                      --  The left of Source is cut

                      and then
                        Slice (Overwrite'Result,
                          1, Max_Length - New_Item'Length) =
                            Slice (Source,
                              Position - Max_Length + New_Item'Length,
                              Position - 1)),

           others  --  Drop = Right
           =>
             --  The result is of maximal length and starts by the first
             --  characters of Source.

             Length (Overwrite'Result) = Max_Length
               and then
                 Slice (Overwrite'Result, 1, Position - 1) =
                   Slice (Source, 1, Position - 1)

               --  Then New_Item is written until Max_Length

               and then Slice (Overwrite'Result, Position, Max_Length) =
                 New_Item
                   (New_Item'First .. Max_Length - Position + New_Item'First));

      procedure Overwrite
        (Source    : in out Bounded_String;
         Position  : Positive;
         New_Item  : String;
         Drop      : Truncation := Error)
      with
        Pre            =>
          Position - 1 <= Length (Source)
            and then (if New_Item'Length > Max_Length - (Position - 1)
                      then Drop /= Error),
        Contract_Cases =>
          (Position - 1 <= Max_Length - New_Item'Length
           =>
             --  The length of Source is unchanged, unless New_Item overwrites
             --  further than the end of Source. In this contract case, we
             --  suppose New_Item doesn't overwrite further than Max_Length.

             Length (Source) = Integer'Max
               (Length (Source'Old), Position - 1 + New_Item'Length)
               and then
                 Slice (Source, 1, Position - 1) =
                   Slice (Source'Old, 1, Position - 1)
               and then Slice (Source,
                 Position, Position - 1 + New_Item'Length) =
                   New_Item
               and then
                 (if Position - 1 + New_Item'Length < Length (Source'Old) then

                    --  There are some unchanged characters of Source remaining
                    --  after New_Item.

                    Slice (Source,
                      Position + New_Item'Length, Length (Source'Old)) =
                        Slice (Source'Old,
                          Position + New_Item'Length, Length (Source'Old))),

           Position - 1 > Max_Length - New_Item'Length and then Drop = Left
           =>
             Length (Source) = Max_Length

               --  If a part of the result has to be dropped, it means New_Item
               --  is overwriting further than the end of Source. Thus the
               --  result is necessarily ending by New_Item. However, we don't
               --  know whether New_Item covers all Max_Length characters or
               --  some characters of Source are remaining at the left.

               and then
                 (if New_Item'Length >= Max_Length then

                    --  New_Item covers all Max_Length characters

                    To_String (Source) =
                      New_Item
                        (New_Item'Last - Max_Length + 1 .. New_Item'Last)
                  else
                    --  New_Item fully appears at the end

                    Slice (Source,
                      Max_Length - New_Item'Length + 1, Max_Length) =
                        New_Item

                      --  The left of Source is cut

                      and then
                        Slice (Source, 1, Max_Length - New_Item'Length) =
                          Slice (Source'Old,
                            Position - Max_Length + New_Item'Length,
                            Position - 1)),

           others  --  Drop = Right
           =>
             --  The result is of maximal length and starts by the first
             --  characters of Source.

             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Position - 1) =
                   Slice (Source'Old, 1, Position - 1)

               --  New_Item is written until Max_Length

               and then Slice (Source, Position, Max_Length) =
                 New_Item
                   (New_Item'First .. Max_Length - Position + New_Item'First));

      function Delete
        (Source  : Bounded_String;
         From    : Positive;
         Through : Natural) return Bounded_String
      with
        Pre            =>
          (if Through >= From then From - 1 <= Length (Source)),
        Contract_Cases =>
          (Through >= From =>
             Length (Delete'Result) =
               From - 1 + Natural'Max (Length (Source) - Through, 0)
               and then
                 Slice (Delete'Result, 1, From - 1) =
                   Slice (Source, 1, From - 1)
               and then
                 (if Through < Length (Source) then
                    Slice (Delete'Result, From, Length (Delete'Result)) =
                      Slice (Source, Through + 1, Length (Source))),
           others          =>
             Delete'Result = Source),
        Global         => null;

      procedure Delete
        (Source  : in out Bounded_String;
         From    : Positive;
         Through : Natural)
      with
        Pre            =>
          (if Through >= From then From - 1 <= Length (Source)),
        Contract_Cases =>
          (Through >= From =>
             Length (Source) =
               From - 1 + Natural'Max (Length (Source'Old) - Through, 0)
               and then
                 Slice (Source, 1, From - 1) = Slice (Source'Old, 1, From - 1)
               and then
                 (if Through < Length (Source) then
                    Slice (Source, From, Length (Source)) =
                      Slice (Source'Old, Through + 1, Length (Source'Old))),
           others          =>
             Source = Source'Old),
        Global         => null;

      ---------------------------------
      -- String Selector Subprograms --
      ---------------------------------

      function Trim
        (Source : Bounded_String;
         Side   : Trim_End) return Bounded_String
      with
        Contract_Cases =>
          --  If all characters in Source are Space, the returned string is
          --  empty.

          ((for all C of To_String (Source) => C = ' ')
           =>
             Length (Trim'Result) = 0,

           --  Otherwise, the returned string is a slice of Source

           others
           =>
             (declare
                Low  : constant Positive :=
                  (if Side = Right then 1
                   else Index_Non_Blank (Source, Forward));
                High : constant Positive :=
                  (if Side = Left then Length (Source)
                   else Index_Non_Blank (Source, Backward));
              begin
                To_String (Trim'Result) = Slice (Source, Low, High))),
        Global         => null;

      procedure Trim
        (Source : in out Bounded_String;
         Side   : Trim_End)
      with
        Contract_Cases =>
          --  If all characters in Source are Space, the returned string is
          --  empty.

          ((for all C of To_String (Source) => C = ' ')
           =>
             Length (Source) = 0,

           --  Otherwise, the returned string is a slice of Source

           others
           =>
             (declare
                Low  : constant Positive :=
                  (if Side = Right then 1
                   else Index_Non_Blank (Source'Old, Forward));
                High : constant Positive :=
                  (if Side = Left then Length (Source'Old)
                   else Index_Non_Blank (Source'Old, Backward));
              begin
                To_String (Source) = Slice (Source'Old, Low, High))),
        Global         => null;

      function Trim
        (Source : Bounded_String;
         Left   : Maps.Character_Set;
         Right  : Maps.Character_Set) return Bounded_String
      with
        Contract_Cases =>
          --  If all characters in Source are contained in one of the sets Left
          --  or Right, then the returned string is empty.

          ((for all C of To_String (Source) => Maps.Is_In (C, Left))
             or else
               (for all C of To_String (Source) => Maps.Is_In (C, Right))
           =>
             Length (Trim'Result) = 0,

           --  Otherwise, the returned string is a slice of Source

           others
           =>
             (declare
                Low  : constant Positive :=
                  Index (Source, Left, Outside, Forward);
                High : constant Positive :=
                  Index (Source, Right, Outside, Backward);
              begin
                To_String (Trim'Result) = Slice (Source, Low, High))),
        Global         => null;

      procedure Trim
        (Source : in out Bounded_String;
         Left   : Maps.Character_Set;
         Right  : Maps.Character_Set)
      with
        Contract_Cases =>
          --  If all characters in Source are contained in one of the sets Left
          --  or Right, then the returned string is empty.

          ((for all C of To_String (Source) => Maps.Is_In (C, Left))
             or else
               (for all C of To_String (Source) => Maps.Is_In (C, Right))
           =>
             Length (Source) = 0,

           --  Otherwise, the returned string is a slice of Source

           others
           =>
             (declare
                Low  : constant Positive :=
                  Index (Source'Old, Left, Outside, Forward);
                High : constant Positive :=
                  Index (Source'Old, Right, Outside, Backward);
              begin
                To_String (Source) = Slice (Source'Old, Low, High))),
        Global         => null;

      function Head
        (Source : Bounded_String;
         Count  : Natural;
         Pad    : Character := Space;
         Drop   : Truncation := Error) return Bounded_String
      with
        Pre            => (if Count > Max_Length then Drop /= Error),
        Contract_Cases =>
          (Count <= Length (Source)
           =>
             --  Source is cut

             To_String (Head'Result) = Slice (Source, 1, Count),

           Count > Length (Source) and then Count <= Max_Length
           =>
             --  Source is followed by Pad characters

             Length (Head'Result) = Count
               and then
                 Slice (Head'Result, 1, Length (Source)) = To_String (Source)
               and then
                 Slice (Head'Result, Length (Source) + 1, Count) =
                   [1 .. Count - Length (Source) => Pad],

           Count > Max_Length and then Drop = Right
           =>
             --  Source is followed by Pad characters

             Length (Head'Result) = Max_Length
               and then
                 Slice (Head'Result, 1, Length (Source)) = To_String (Source)
               and then
                 Slice (Head'Result, Length (Source) + 1, Max_Length) =
                   [1 .. Max_Length - Length (Source) => Pad],

           Count - Length (Source) > Max_Length and then Drop = Left
           =>
             --  Source is fully dropped at the left

             To_String (Head'Result) = [1 .. Max_Length => Pad],

           others
           =>
             --  Source is partly dropped at the left

             Length (Head'Result) = Max_Length
               and then
                 Slice (Head'Result, 1, Max_Length - Count + Length (Source)) =
                   Slice (Source, Count - Max_Length + 1, Length (Source))
               and then
                 Slice (Head'Result,
                   Max_Length - Count + Length (Source) + 1, Max_Length) =
                     [1 .. Count - Length (Source) => Pad]);

      procedure Head
        (Source : in out Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error)
      with
        Pre            => (if Count > Max_Length then Drop /= Error),
        Contract_Cases =>
          (Count <= Length (Source)
           =>
             --  Source is cut

             To_String (Source) = Slice (Source'Old, 1, Count),

           Count > Length (Source) and then Count <= Max_Length
           =>
             --  Source is followed by Pad characters

             Length (Source) = Count
               and then
                 Slice (Source, 1, Length (Source'Old)) =
                   To_String (Source'Old)
               and then
                 Slice (Source, Length (Source'Old) + 1, Count) =
                   [1 .. Count - Length (Source'Old) => Pad],

           Count > Max_Length and then Drop = Right
           =>
             --  Source is followed by Pad characters

             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Length (Source'Old)) =
                   To_String (Source'Old)
               and then
                 Slice (Source, Length (Source'Old) + 1, Max_Length) =
                   [1 .. Max_Length - Length (Source'Old) => Pad],

           Count - Length (Source) > Max_Length and then Drop = Left
           =>
             --  Source is fully dropped on the left

             To_String (Source) = [1 .. Max_Length => Pad],

           others
           =>
             --  Source is partly dropped on the left

             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Max_Length - Count + Length (Source'Old)) =
                   Slice (Source'Old,
                     Count - Max_Length + 1, Length (Source'Old))
               and then
                 Slice (Source,
                   Max_Length - Count + Length (Source'Old) + 1, Max_Length) =
                     [1 .. Count - Length (Source'Old) => Pad]);

      function Tail
        (Source : Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error) return Bounded_String
      with
        Pre            => (if Count > Max_Length then Drop /= Error),
        Contract_Cases =>
          (Count < Length (Source)
           =>
             --  Source is cut

             (if Count > 0 then
                To_String (Tail'Result) =
                  Slice (Source, Length (Source) - Count + 1, Length (Source))
              else Length (Tail'Result) = 0),

           Count >= Length (Source) and then Count < Max_Length
           =>
             --  Source is preceded by Pad characters

             Length (Tail'Result) = Count
               and then
                 Slice (Tail'Result, 1, Count - Length (Source)) =
                   [1 .. Count - Length (Source) => Pad]
               and then
                 Slice (Tail'Result, Count - Length (Source) + 1, Count) =
                   To_String (Source),

           Count >= Max_Length and then Drop = Left
           =>
             --  Source is preceded by Pad characters

             Length (Tail'Result) = Max_Length
               and then
                 Slice (Tail'Result, 1, Max_Length - Length (Source)) =
                   [1 .. Max_Length - Length (Source) => Pad]
               and then
                 (if Length (Source) > 0 then
                    Slice (Tail'Result,
                      Max_Length - Length (Source) + 1, Max_Length) =
                        To_String (Source)),

           Count - Length (Source) >= Max_Length and then Drop /= Left
           =>
             --  Source is fully dropped on the right

             To_String (Tail'Result) = [1 .. Max_Length => Pad],

           others
           =>
             --  Source is partly dropped on the right

             Length (Tail'Result) = Max_Length
               and then
                 Slice (Tail'Result, 1, Count - Length (Source)) =
                   [1 .. Count - Length (Source) => Pad]
               and then
                 Slice (Tail'Result, Count - Length (Source) + 1, Max_Length) =
                   Slice (Source, 1, Max_Length - Count + Length (Source)));

      procedure Tail
        (Source : in out Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error)
      with
        Pre            => (if Count > Max_Length then Drop /= Error),
        Contract_Cases =>
          (Count < Length (Source)
           =>
             --  Source is cut

             (if Count > 0 then
                To_String (Source) =
                  Slice (Source'Old,
                    Length (Source'Old) - Count + 1, Length (Source'Old))
              else Length (Source) = 0),

           Count >= Length (Source) and then Count < Max_Length
           =>
             --  Source is preceded by Pad characters

             Length (Source) = Count
               and then
                 Slice (Source, 1, Count - Length (Source'Old)) =
                   [1 .. Count - Length (Source'Old) => Pad]
               and then
                 Slice (Source, Count - Length (Source'Old) + 1, Count) =
                   To_String (Source'Old),

           Count >= Max_Length and then Drop = Left
           =>
             --  Source is preceded by Pad characters

             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Max_Length - Length (Source'Old)) =
                   [1 .. Max_Length - Length (Source'Old) => Pad]
               and then
                 (if Length (Source'Old) > 0 then
                    Slice (Source,
                      Max_Length - Length (Source'Old) + 1, Max_Length) =
                        To_String (Source'Old)),

           Count - Length (Source) >= Max_Length and then Drop /= Left
           =>
             --  Source is fully dropped at the right

             To_String (Source) = [1 .. Max_Length => Pad],

           others
           =>
             --  Source is partly dropped at the right

             Length (Source) = Max_Length
               and then
                 Slice (Source, 1, Count - Length (Source'Old)) =
                   [1 .. Count - Length (Source'Old) => Pad]
               and then
                 Slice (Source, Count - Length (Source'Old) + 1, Max_Length) =
                   Slice (Source'Old,
                     1, Max_Length - Count + Length (Source'Old)));

      ------------------------------------
      -- String Constructor Subprograms --
      ------------------------------------

      function "*"
        (Left  : Natural;
         Right : Character) return Bounded_String
      with
        Pre  => Left <= Max_Length,
        Post => To_String ("*"'Result) = [1 .. Left => Right];

      function "*"
        (Left  : Natural;
         Right : String) return Bounded_String
      with
        Pre  => (if Left /= 0 then Right'Length <= Max_Length / Left),
        Post =>
          Length ("*"'Result) = Left * Right'Length
            and then
              (if Right'Length > 0 then
                 (for all K in 1 .. Left * Right'Length =>
                    Element ("*"'Result, K) =
                      Right (Right'First + (K - 1) mod Right'Length)));

      function "*"
        (Left  : Natural;
         Right : Bounded_String) return Bounded_String
      with
        Pre  => (if Left /= 0 then Length (Right) <= Max_Length / Left),
        Post =>
          Length ("*"'Result) = Left * Length (Right)
            and then
              (if Length (Right) > 0 then
                 (for all K in 1 .. Left * Length (Right) =>
                    Element ("*"'Result, K) =
                      Element (Right, 1 + (K - 1) mod Length (Right))));

      function Replicate
        (Count : Natural;
         Item  : Character;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre  => (if Count > Max_Length then Drop /= Error),
        Post =>
          To_String (Replicate'Result) =
            [1 .. Natural'Min (Max_Length, Count) => Item];

      function Replicate
        (Count : Natural;
         Item  : String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre            =>
          (if Count /= 0 and then Item'Length > Max_Length / Count
           then Drop /= Error),
        Contract_Cases =>
          (Count = 0 or else Item'Length <= Max_Length / Count
           =>
             Length (Replicate'Result) = Count * Item'Length
               and then
                 (if Item'Length > 0 then
                    (for all K in 1 .. Count * Item'Length =>
                       Element (Replicate'Result, K) =
                         Item (Item'First + (K - 1) mod Item'Length))),
           Count /= 0
             and then Item'Length > Max_Length / Count
             and then Drop = Right
           =>
             Length (Replicate'Result) = Max_Length
               and then
                 (for all K in 1 .. Max_Length =>
                    Element (Replicate'Result, K) =
                      Item (Item'First + (K - 1) mod Item'Length)),
           others  --  Drop = Left
           =>
             Length (Replicate'Result) = Max_Length
               and then
                 (for all K in 1 .. Max_Length =>
                    Element (Replicate'Result, K) =
                      Item (Item'Last - (Max_Length - K) mod Item'Length)));

      function Replicate
        (Count : Natural;
         Item  : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre            =>
          (if Count /= 0 and then Length (Item) > Max_Length / Count
           then Drop /= Error),
        Contract_Cases =>
          ((if Count /= 0 then Length (Item) <= Max_Length / Count)
           =>
             Length (Replicate'Result) = Count * Length (Item)
               and then
                 (if Length (Item) > 0 then
                    (for all K in 1 .. Count * Length (Item) =>
                       Element (Replicate'Result, K) =
                         Element (Item, 1 + (K - 1) mod Length (Item)))),
           Count /= 0
             and then Length (Item) > Max_Length / Count
             and then Drop = Right
           =>
             Length (Replicate'Result) = Max_Length
               and then
                 (for all K in 1 .. Max_Length =>
                    Element (Replicate'Result, K) =
                      Element (Item, 1 + (K - 1) mod Length (Item))),
           others  --  Drop = Left
           =>
             Length (Replicate'Result) = Max_Length
               and then
                 (for all K in 1 .. Max_Length =>
                    Element (Replicate'Result, K) =
                      Element (Item,
                        Length (Item) - (Max_Length - K) mod Length (Item))));

   private
      --  Most of the implementation is in the separate non generic package
      --  Ada.Strings.Superbounded. Type Bounded_String is derived from type
      --  Superbounded.Super_String with the maximum length constraint. In
      --  almost all cases, the routines in Superbounded can be called with
      --  no requirement to pass the maximum length explicitly, since there
      --  is at least one Bounded_String argument from which the maximum
      --  length can be obtained. For all such routines, the implementation
      --  in this private part is simply a renaming of the corresponding
      --  routine in the superbounded package.

      --  The five exceptions are the * and Replicate routines operating on
      --  character values. For these cases, we have a routine in the body
      --  that calls the superbounded routine passing the maximum length
      --  explicitly as an extra parameter.

      type Bounded_String is new Superbounded.Super_String (Max_Length);
      --  Deriving Bounded_String from Superbounded.Super_String is the
      --  real trick, it ensures that the type Bounded_String declared in
      --  the generic instantiation is compatible with the Super_String
      --  type declared in the Superbounded package.

      function From_String (Source : String) return Bounded_String
      with Pre => Source'Length <= Max_Length;
      --  Private routine used only by Stream_Convert

      pragma Stream_Convert (Bounded_String, From_String, To_String);
      --  Provide stream routines without dragging in Ada.Streams

      Null_Bounded_String : constant Bounded_String :=
                              (Max_Length     => Max_Length,
                               Current_Length => 0,
                               Data           =>
                                 [1 .. Max_Length => ASCII.NUL]);

      pragma Inline (To_Bounded_String);

      procedure Set_Bounded_String
        (Target : out Bounded_String;
         Source : String;
         Drop   : Truncation := Error)
         renames Set_Super_String;

      function Length
        (Source : Bounded_String) return Length_Range
         renames Super_Length;

      function To_String
        (Source : Bounded_String) return String
         renames Super_To_String;

      function Append
        (Left  : Bounded_String;
         Right : Bounded_String;
         Drop  : Truncation  := Error) return Bounded_String
         renames Super_Append;

      function Append
        (Left  : Bounded_String;
         Right : String;
         Drop  : Truncation := Error) return Bounded_String
         renames Super_Append;

      function Append
        (Left  : String;
         Right : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
         renames Super_Append;

      function Append
        (Left  : Bounded_String;
         Right : Character;
         Drop  : Truncation := Error) return Bounded_String
         renames Super_Append;

      function Append
        (Left  : Character;
         Right : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
         renames Super_Append;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : Bounded_String;
         Drop     : Truncation  := Error)
         renames Super_Append;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : String;
         Drop     : Truncation  := Error)
         renames Super_Append;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : Character;
         Drop     : Truncation  := Error)
         renames Super_Append;

      function "&"
        (Left  : Bounded_String;
         Right : Bounded_String) return Bounded_String
         renames Concat;

      function "&"
        (Left  : Bounded_String;
         Right : String) return Bounded_String
         renames Concat;

      function "&"
        (Left  : String;
         Right : Bounded_String) return Bounded_String
         renames Concat;

      function "&"
        (Left  : Bounded_String;
         Right : Character) return Bounded_String
         renames Concat;

      function "&"
        (Left  : Character;
         Right : Bounded_String) return Bounded_String
         renames Concat;

      function Element
        (Source : Bounded_String;
         Index  : Positive) return Character
         renames Super_Element;

      procedure Replace_Element
        (Source : in out Bounded_String;
         Index  : Positive;
         By     : Character)
         renames Super_Replace_Element;

      function Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural) return String
         renames Super_Slice;

      function Bounded_Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural) return Bounded_String
         renames Super_Slice;

      procedure Bounded_Slice
        (Source : Bounded_String;
         Target : out Bounded_String;
         Low    : Positive;
         High   : Natural)
         renames Super_Slice;

      overriding function "="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
         renames Equal;

      function "="
        (Left  : Bounded_String;
         Right : String) return Boolean
         renames Equal;

      function "="
        (Left  : String;
         Right : Bounded_String) return Boolean
         renames Equal;

      function "<"
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
         renames Less;

      function "<"
        (Left  : Bounded_String;
         Right : String) return Boolean
         renames Less;

      function "<"
        (Left  : String;
         Right : Bounded_String) return Boolean
         renames Less;

      function "<="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
         renames Less_Or_Equal;

      function "<="
        (Left  : Bounded_String;
         Right : String) return Boolean
         renames Less_Or_Equal;

      function "<="
        (Left  : String;
         Right : Bounded_String) return Boolean
         renames Less_Or_Equal;

      function ">"
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
         renames Greater;

      function ">"
        (Left  : Bounded_String;
         Right : String) return Boolean
         renames Greater;

      function ">"
        (Left  : String;
         Right : Bounded_String) return Boolean
         renames Greater;

      function ">="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
         renames Greater_Or_Equal;

      function ">="
        (Left  : Bounded_String;
         Right : String) return Boolean
         renames Greater_Or_Equal;

      function ">="
        (Left  : String;
         Right : Bounded_String) return Boolean
         renames Greater_Or_Equal;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
         renames Super_Index;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping_Function) return Natural
         renames Super_Index;

      function Index
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         Test   : Membership := Inside;
         Going  : Direction  := Forward) return Natural
         renames Super_Index;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
         renames Super_Index;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping_Function) return Natural
      renames Super_Index;

      function Index
        (Source  : Bounded_String;
         Set     : Maps.Character_Set;
         From    : Positive;
         Test    : Membership := Inside;
         Going   : Direction := Forward) return Natural
      renames Super_Index;

      function Index_Non_Blank
        (Source : Bounded_String;
         Going  : Direction := Forward) return Natural
         renames Super_Index_Non_Blank;

      function Index_Non_Blank
        (Source : Bounded_String;
         From   : Positive;
         Going  : Direction := Forward) return Natural
         renames Super_Index_Non_Blank;

      function Count
        (Source  : Bounded_String;
         Pattern : String;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
         renames Super_Count;

      function Count
        (Source  : Bounded_String;
         Pattern : String;
         Mapping : Maps.Character_Mapping_Function) return Natural
         renames Super_Count;

      function Count
        (Source : Bounded_String;
         Set    : Maps.Character_Set) return Natural
         renames Super_Count;

      procedure Find_Token
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         From   : Positive;
         Test   : Membership;
         First  : out Positive;
         Last   : out Natural)
         renames Super_Find_Token;

      procedure Find_Token
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         Test   : Membership;
         First  : out Positive;
         Last   : out Natural)
         renames Super_Find_Token;

      function Translate
        (Source  : Bounded_String;
         Mapping : Maps.Character_Mapping) return Bounded_String
         renames Super_Translate;

      procedure Translate
        (Source   : in out Bounded_String;
         Mapping  : Maps.Character_Mapping)
         renames Super_Translate;

      function Translate
        (Source  : Bounded_String;
         Mapping : Maps.Character_Mapping_Function) return Bounded_String
         renames Super_Translate;

      procedure Translate
        (Source  : in out Bounded_String;
         Mapping : Maps.Character_Mapping_Function)
         renames Super_Translate;

      function Replace_Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural;
         By     : String;
         Drop   : Truncation := Error) return Bounded_String
         renames Super_Replace_Slice;

      procedure Replace_Slice
        (Source   : in out Bounded_String;
         Low      : Positive;
         High     : Natural;
         By       : String;
         Drop     : Truncation := Error)
         renames Super_Replace_Slice;

      function Insert
        (Source   : Bounded_String;
         Before   : Positive;
         New_Item : String;
         Drop     : Truncation := Error) return Bounded_String
         renames Super_Insert;

      procedure Insert
        (Source   : in out Bounded_String;
         Before   : Positive;
         New_Item : String;
         Drop     : Truncation := Error)
         renames Super_Insert;

      function Overwrite
        (Source   : Bounded_String;
         Position : Positive;
         New_Item : String;
         Drop     : Truncation := Error) return Bounded_String
         renames Super_Overwrite;

      procedure Overwrite
        (Source    : in out Bounded_String;
         Position  : Positive;
         New_Item  : String;
         Drop      : Truncation := Error)
         renames Super_Overwrite;

      function Delete
        (Source  : Bounded_String;
         From    : Positive;
         Through : Natural) return Bounded_String
         renames Super_Delete;

      procedure Delete
        (Source  : in out Bounded_String;
         From    : Positive;
         Through : Natural)
         renames Super_Delete;

      function Trim
        (Source : Bounded_String;
         Side   : Trim_End) return Bounded_String
         renames Super_Trim;

      procedure Trim
        (Source : in out Bounded_String;
         Side   : Trim_End)
         renames Super_Trim;

      function Trim
        (Source : Bounded_String;
         Left   : Maps.Character_Set;
         Right  : Maps.Character_Set) return Bounded_String
         renames Super_Trim;

      procedure Trim
        (Source : in out Bounded_String;
         Left   : Maps.Character_Set;
         Right  : Maps.Character_Set)
         renames Super_Trim;

      function Head
        (Source : Bounded_String;
         Count  : Natural;
         Pad    : Character := Space;
         Drop   : Truncation := Error) return Bounded_String
         renames Super_Head;

      procedure Head
        (Source : in out Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error)
         renames Super_Head;

      function Tail
        (Source : Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error) return Bounded_String
         renames Super_Tail;

      procedure Tail
        (Source : in out Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error)
         renames Super_Tail;

      function "*"
        (Left  : Natural;
         Right : Bounded_String) return Bounded_String
         renames Times;

      function Replicate
        (Count : Natural;
         Item  : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
         renames Super_Replicate;

   end Generic_Bounded_Length;

end Ada.Strings.Bounded;
