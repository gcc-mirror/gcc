------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . S T R I N G S . U N B O U N D E D                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with Ada.Strings.Maps; use type Ada.Strings.Maps.Character_Mapping_Function;
with Ada.Finalization;
with Ada.Strings.Search;
private with Ada.Strings.Text_Buffers;

--  The language-defined package Strings.Unbounded provides a private type
--  Unbounded_String and a set of operations. An object of type
--  Unbounded_String represents a String whose low bound is 1 and whose length
--  can vary conceptually between 0 and Natural'Last. The subprograms for
--  fixed-length string handling are either overloaded directly for
--  Unbounded_String, or are modified as needed to reflect the flexibility in
--  length. Since the Unbounded_String type is private, relevant constructor
--  and selector operations are provided.

package Ada.Strings.Unbounded with
  SPARK_Mode,
  Initial_Condition => Length (Null_Unbounded_String) = 0,
  Always_Terminates
is
   pragma Preelaborate;

   --  Contracts may call function Length in the prefix of attribute reference
   --  to Old as in Length (Source)'Old. Allow this use.

   pragma Unevaluated_Use_Of_Old (Allow);

   subtype String_1 is String (1 .. <>) with Ghost;  --  Type used in contracts

   type Unbounded_String is private with
     Default_Initial_Condition => Length (Unbounded_String) = 0;
   pragma Preelaborable_Initialization (Unbounded_String);

   Null_Unbounded_String : constant Unbounded_String;
   --  Represents the null String. If an object of type Unbounded_String is not
   --  otherwise initialized, it will be initialized to the same value as
   --  Null_Unbounded_String.

   function Length (Source : Unbounded_String) return Natural with
     Global => null;
   --  Returns the length of the String represented by Source

   type String_Access is access all String;
   --  Provides a (nonprivate) access type for explicit processing of
   --  unbounded-length strings.

   procedure Free (X : in out String_Access) with SPARK_Mode => Off;
   --  Performs an unchecked deallocation of an object of type String_Access

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_String
     (Source : String) return Unbounded_String
   with
     Post   => To_String (To_Unbounded_String'Result) = Source
       and then Length (To_Unbounded_String'Result) = Source'Length,
     Global => null;
   --  Returns an Unbounded_String that represents Source

   function To_Unbounded_String
     (Length : Natural) return Unbounded_String
   with
     SPARK_Mode => Off,
     Global     => null;
   --  Returns an Unbounded_String that represents an uninitialized String
   --  whose length is Length.

   function To_String (Source : Unbounded_String) return String with
     Post   =>
       To_String'Result'First = 1
         and then To_String'Result'Length = Length (Source),
     Global => null;
   --  Returns the String with lower bound 1 represented by Source

   --  To_String and To_Unbounded_String are related as follows:
   --
   --  * If S is a String, then To_String (To_Unbounded_String (S)) = S.
   --
   --  * If U is an Unbounded_String, then
   --    To_Unbounded_String (To_String (U)) = U.

   procedure Set_Unbounded_String
     (Target : out Unbounded_String;
      Source : String)
   with
     Post   => To_String (Target) = Source,
     Global => null;
   pragma Ada_05 (Set_Unbounded_String);
   --  Sets Target to an Unbounded_String that represents Source

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Unbounded_String)
   with
     Pre    => Length (New_Item) <= Natural'Last - Length (Source),
     Post   =>
       To_String (Source) = To_String (Source)'Old & To_String (New_Item),
     Global => null;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : String)
   with
     Pre    => New_Item'Length <= Natural'Last - Length (Source),
     Post   => To_String (Source) = To_String (Source)'Old & New_Item,
     Global => null;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : Character)
   with
     Pre    => Length (Source) < Natural'Last,
     Post   => To_String (Source) = To_String (Source)'Old & New_Item,
     Global => null;

   --  For each of the Append procedures, the resulting string represented by
   --  the Source parameter is given by the concatenation of the original value
   --  of Source and the value of New_Item.

   function "&"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Unbounded_String
   with
     Pre    => Length (Right) <= Natural'Last - Length (Left),
     Post   => To_String ("&"'Result) = To_String (Left) & To_String (Right),
     Global => null;

   function "&"
     (Left  : Unbounded_String;
      Right : String) return Unbounded_String
   with
     Pre    => Right'Length <= Natural'Last - Length (Left),
     Post   => To_String ("&"'Result) = To_String (Left) & Right,
     Global => null;

   function "&"
     (Left  : String;
      Right : Unbounded_String) return Unbounded_String
   with
     Pre    => Left'Length <= Natural'Last - Length (Right),
     Post   => To_String ("&"'Result) = String_1 (Left) & To_String (Right),
     Global => null;

   function "&"
     (Left  : Unbounded_String;
      Right : Character) return Unbounded_String
   with
     Pre    => Length (Left) < Natural'Last,
     Post   => To_String ("&"'Result) = To_String (Left) & Right,
     Global => null;

   function "&"
     (Left  : Character;
      Right : Unbounded_String) return Unbounded_String
   with
     Pre    => Length (Right) < Natural'Last,
     Post   => To_String ("&"'Result) = Left & To_String (Right),
     Global => null;

   --  Each of the "&" functions returns an Unbounded_String obtained by
   --  concatenating the string or character given or represented by one of the
   --  parameters, with the string or character given or represented by the
   --  other parameter, and applying To_Unbounded_String to the concatenation
   --  result string.

   function Element
     (Source : Unbounded_String;
      Index  : Positive) return Character
   with
     Pre    => Index <= Length (Source),
     Post   => Element'Result = To_String (Source) (Index),
     Global => null;
   --  Returns the character at position Index in the string represented by
   --  Source; propagates Index_Error if Index > Length (Source).

   procedure Replace_Element
     (Source : in out Unbounded_String;
      Index  : Positive;
      By     : Character)
   with
     Pre    => Index <= Length (Source),
     Post   =>
       To_String (Source) = (To_String (Source)'Old with delta Index => By),
     Global => null;
   --  Updates Source such that the character at position Index in the string
   --  represented by Source is By; propagates Index_Error if
   --  Index > Length (Source).

   function Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return String
   with
     Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
     Post   => Slice'Result'First = Low
       and then Slice'Result'Last = High
       and then Slice'Result = To_String (Source) (Low .. High),
     Global => null;
   --  Returns the slice at positions Low through High in the string
   --  represented by Source; propagates Index_Error if
   --  Low > Length (Source) + 1 or High > Length (Source). The bounds of the
   --  returned string are Low and High.

   function Unbounded_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural) return Unbounded_String
   with
     Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
     Post   =>
       To_String (Unbounded_Slice'Result) = To_String (Source) (Low .. High),
     Global => null;
   pragma Ada_05 (Unbounded_Slice);
   --  Returns the slice at positions Low through High in the string
   --  represented by Source as an Unbounded_String. This propagates
   --  Index_Error if Low > Length(Source) + 1 or High > Length (Source).

   procedure Unbounded_Slice
     (Source : Unbounded_String;
      Target : out Unbounded_String;
      Low    : Positive;
      High   : Natural)
   with
     Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
     Post   => To_String (Target) = To_String (Source) (Low .. High),
     Global => null;
   pragma Ada_05 (Unbounded_Slice);
   --  Sets Target to the Unbounded_String representing the slice at positions
   --  Low through High in the string represented by Source. This propagates
   --  Index_Error if Low > Length(Source) + 1 or High > Length (Source).

   function "="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Post   => "="'Result = (To_String (Left) = To_String (Right)),
     Global => null;

   function "="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Post   => "="'Result = (To_String (Left) = Right),
     Global => null;

   function "="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Post   => "="'Result = (Left = To_String (Right)),
     Global => null;

   function "<"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Post   => "<"'Result = (To_String (Left) < To_String (Right)),
     Global => null;

   function "<"
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Post   => "<"'Result = (To_String (Left) < Right),
     Global => null;

   function "<"
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Post   => "<"'Result = (Left < To_String (Right)),
     Global => null;

   function "<="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Post   => "<="'Result = (To_String (Left) <= To_String (Right)),
     Global => null;

   function "<="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Post   => "<="'Result = (To_String (Left) <= Right),
     Global => null;

   function "<="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Post   => "<="'Result = (Left <= To_String (Right)),
     Global => null;

   function ">"
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Post   => ">"'Result = (To_String (Left) > To_String (Right)),
     Global => null;

   function ">"
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Post   => ">"'Result = (To_String (Left) > Right),
     Global => null;

   function ">"
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Post   => ">"'Result = (Left > To_String (Right)),
     Global => null;

   function ">="
     (Left  : Unbounded_String;
      Right : Unbounded_String) return Boolean
   with
     Post   => ">="'Result = (To_String (Left) >= To_String (Right)),
     Global => null;

   function ">="
     (Left  : Unbounded_String;
      Right : String) return Boolean
   with
     Post   => ">="'Result = (To_String (Left) >= Right),
     Global => null;

   function ">="
     (Left  : String;
      Right : Unbounded_String) return Boolean
   with
     Post   => ">="'Result = (Left >= To_String (Right)),
     Global => null;

   --  Each of the functions "=", "<", ">", "<=", and ">=" returns the same
   --  result as the corresponding String operation applied to the String
   --  values given or represented by Left and Right.

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : Unbounded_String;
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
                  then not Search.Match
                    (To_String (Source), Pattern, Mapping, J))),

        --  Otherwise, 0 is returned

        others
        =>
          Index'Result = 0),
     Global         => null;

   function Index
     (Source  : Unbounded_String;
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
                  then not Search.Match
                    (To_String (Source), Pattern, Mapping, J))),

        --  Otherwise, 0 is returned

        others
        =>
          Index'Result = 0),
     Global         => null;

   function Index
     (Source : Unbounded_String;
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
     (Source  : Unbounded_String;
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
                  then not Search.Match
                    (To_String (Source), Pattern, Mapping, J))),

        --  Otherwise, 0 is returned

        others
        =>
          Index'Result = 0),
     Global         => null;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_String;
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
                  then not Search.Match
                    (To_String (Source), Pattern, Mapping, J))),

        --  Otherwise, 0 is returned

        others
        =>
          Index'Result = 0),
     Global         => null;
   pragma Ada_05 (Index);

   function Index
     (Source  : Unbounded_String;
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
     (Source : Unbounded_String;
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
     (Source : Unbounded_String;
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
     (Source  : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
   with
     Pre    => Pattern'Length /= 0,
     Global => null;

   function Count
     (Source  : Unbounded_String;
      Pattern : String;
      Mapping : Maps.Character_Mapping_Function) return Natural
   with
     Pre    => Pattern'Length /= 0 and then Mapping /= null,
     Global => null;

   function Count
     (Source : Unbounded_String;
      Set    : Maps.Character_Set) return Natural
   with
     Global => null;

   procedure Find_Token
     (Source : Unbounded_String;
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
     (Source : Unbounded_String;
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

   --  Each of the search subprograms (Index, Index_Non_Blank, Count,
   --  Find_Token) has the same effect as the corresponding subprogram in
   --  Strings.Fixed applied to the string represented by the Unbounded_String
   --  parameter.

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping) return Unbounded_String
   with
     Post   => Length (Translate'Result) = Length (Source)
       and then
         (for all K in 1 .. Length (Source) =>
            Element (Translate'Result, K) =
              Ada.Strings.Maps.Value (Mapping, Element (Source, K))),
     Global => null;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping)
   with
     Post   => Length (Source) = Length (Source)'Old
       and then
         (for all K in 1 .. Length (Source) =>
            Element (Source, K) =
              Ada.Strings.Maps.Value (Mapping, To_String (Source)'Old (K))),
     Global => null;

   function Translate
     (Source  : Unbounded_String;
      Mapping : Maps.Character_Mapping_Function) return Unbounded_String
   with
     Pre    => Mapping /= null,
     Post   => Length (Translate'Result) = Length (Source)
       and then
         (for all K in 1 .. Length (Source) =>
            Element (Translate'Result, K) = Mapping (Element (Source, K))),
     Global => null;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping_Function)
   with
     Pre    => Mapping /= null,
     Post   => Length (Source) = Length (Source)'Old
       and then
         (for all K in 1 .. Length (Source) =>
            Element (Source, K) = Mapping (To_String (Source)'Old (K))),
     Global => null;

   --  The Translate function has an analogous effect to the corresponding
   --  subprogram in Strings.Fixed. The translation is applied to the string
   --  represented by the Unbounded_String parameter, and the result is
   --  converted (via To_Unbounded_String) to an Unbounded_String.

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String) return Unbounded_String
   with
     Pre            =>
       Low - 1 <= Length (Source)
         and then (if High >= Low
                   then Low - 1
                     <= Natural'Last - By'Length
                      - Integer'Max (Length (Source) - High, 0)
                   else Length (Source) <= Natural'Last - By'Length),
     Contract_Cases =>

        --  If High >= Low, when considering the application of To_String to
        --  convert an Unbounded_String into String, then the returned string
        --  comprises Source (Source'First .. Low - 1) & By
        --  & Source(High + 1 .. Source'Last).

       (High >= Low =>

          --  Length of the returned string

          Length (Replace_Slice'Result)
          = Low - 1 + By'Length + Integer'Max (Length (Source) - High, 0)

            --  Elements starting at Low are replaced by elements of By

            and then
              Slice (Replace_Slice'Result, 1, Low - 1) =
                Slice (Source, 1, Low - 1)
            and then
              Slice (Replace_Slice'Result, Low, Low - 1 + By'Length) = By

            --  When there are remaining characters after the replaced slice,
            --  they are appended to the result.

            and then
              (if High < Length (Source)
               then
                 Slice (Replace_Slice'Result,
                   Low + By'Length, Length (Replace_Slice'Result)) =
                     Slice (Source, High + 1, Length (Source))),

        --  If High < Low, then the returned string is
        --  Insert (Source, Before => Low, New_Item => By).

        others      =>

          --  Length of the returned string

          Length (Replace_Slice'Result) = Length (Source) + By'Length

            --  Elements of By are inserted after the element at Low

            and then
              Slice (Replace_Slice'Result, 1, Low - 1) =
                Slice (Source, 1, Low - 1)
            and then
              Slice (Replace_Slice'Result, Low, Low - 1 + By'Length) = By

            --  When there are remaining characters after Low in Source, they
            --  are appended to the result.

            and then
              (if Low < Length (Source)
               then
                Slice (Replace_Slice'Result,
                  Low + By'Length, Length (Replace_Slice'Result)) =
                    Slice (Source, Low, Length (Source)))),
     Global         => null;
   --  If Low > Source'Last + 1, or High < Source'First - 1, then Index_Error
   --  is propagated. Otherwise:
   --
   --  * If High >= Low, then the returned string comprises
   --    Source (Source'First .. Low - 1)
   --    & By & Source(High + 1 .. Source'Last), but with lower bound 1.
   --
   --  * If High < Low, then the returned string is
   --    Insert (Source, Before => Low, New_Item => By).

   procedure Replace_Slice
     (Source : in out Unbounded_String;
      Low    : Positive;
      High   : Natural;
      By     : String)
   with
     Pre            =>
       Low - 1 <= Length (Source)
         and then (if High >= Low
                   then Low - 1
                     <= Natural'Last - By'Length
                      - Natural'Max (Length (Source) - High, 0)
                   else Length (Source) <= Natural'Last - By'Length),
     Contract_Cases =>

        --  If High >= Low, when considering the application of To_String to
        --  convert an Unbounded_String into String, then the returned string
        --  comprises Source (Source'First .. Low - 1) & By
        --  & Source(High + 1 .. Source'Last).

       (High >= Low =>

          --  Length of the returned string

          Length (Source)
          = Low - 1 + By'Length + Integer'Max (Length (Source)'Old - High, 0)

            --  Elements starting at Low are replaced by elements of By

            and then Slice (Source, 1, Low - 1) =
              Slice (Source, 1, Low - 1)'Old
            and then Slice (Source, Low, Low - 1 + By'Length) = By

            --  When there are remaining characters after the replaced slice,
            --  they are appended to the result.

            and then
              (if High < Length (Source)'Old
               then Slice (Source, Low + By'Length, Length (Source)) =
                 Slice (Source,
                   --  Really "High + 1" but expressed with a conditional
                   --  repeating the above test so that the call to Slice
                   --  is valid on entry (under 'Old) even when the test
                   --  evaluates to False.
                   (if High < Length (Source) then High + 1 else 1),
                   Length (Source))'Old),

        --  If High < Low, then the returned string is
        --  Insert (Source, Before => Low, New_Item => By).

        others      =>

          --  Length of the returned string

          Length (Source) = Length (Source)'Old + By'Length

            --  Elements of By are inserted after the element at Low

            and then Slice (Source, 1, Low - 1) =
              Slice (Source, 1, Low - 1)'Old
            and then Slice (Source, Low, Low - 1 + By'Length) = By

            --  When there are remaining characters after Low in Source, they
            --  are appended to the result.

            and then
              (if Low < Length (Source)'Old
               then Slice (Source, Low + By'Length, Length (Source)) =
                 Slice (Source, Low, Length (Source))'Old)),
     Global         => null;

   function Insert
     (Source   : Unbounded_String;
      Before   : Positive;
      New_Item : String) return Unbounded_String
   with
     Pre    => Before - 1 <= Length (Source)
                 and then New_Item'Length <= Natural'Last - Length (Source),
     Post   =>
       --  Length of the returned string

       Length (Insert'Result) = Length (Source) + New_Item'Length

         --  Elements of New_Item are inserted after element at Before

         and then
           Slice (Insert'Result, 1, Before - 1) = Slice (Source, 1, Before - 1)
         and then
           Slice (Insert'Result, Before, Before - 1 + New_Item'Length)
           = New_Item

         --  When there are remaining characters after Before in Source, they
         --  are appended to the returned string.

         and then
           (if Before <= Length (Source) then
              Slice (Insert'Result, Before + New_Item'Length,
                Length (Insert'Result)) =
                  Slice (Source, Before, Length (Source))),
     Global => null;
   --  Propagates Index_Error if Before is not in
   --  Source'First .. Source'Last + 1; otherwise, returns
   --  Source (Source'First .. Before - 1)
   --  & New_Item & Source(Before..Source'Last), but with lower bound 1.

   procedure Insert
     (Source   : in out Unbounded_String;
      Before   : Positive;
      New_Item : String)
   with
     Pre    => Before - 1 <= Length (Source)
                 and then New_Item'Length <= Natural'Last - Length (Source),
     Post   =>
       --  Length of the returned string

       Length (Source) = Length (Source)'Old + New_Item'Length

         --  Elements of New_Item are inserted after element at Before

         and then
           Slice (Source, 1, Before - 1) = Slice (Source, 1, Before - 1)'Old
         and then
           Slice (Source, Before, Before - 1 + New_Item'Length) = New_Item

         --  When there are remaining characters after Before in Source, they
         --  are appended to the returned string.

         and then
           (if Before <= Length (Source)'Old then
              Slice (Source, Before + New_Item'Length, Length (Source)) =
                Slice (Source, Before, Length (Source))'Old),
     Global => null;

   function Overwrite
     (Source   : Unbounded_String;
      Position : Positive;
      New_Item : String) return Unbounded_String
   with
     Pre    => Position - 1 <= Length (Source)
                 and then New_Item'Length <= Natural'Last - (Position - 1),
     Post   =>
       --  Length of the returned string

       Length (Overwrite'Result) =
         Integer'Max (Length (Source), Position - 1 + New_Item'Length)

         --  Elements after Position are replaced by elements of New_Item

         and then
           Slice (Overwrite'Result, 1, Position - 1) =
             Slice (Source, 1, Position - 1)
         and then
           Slice (Overwrite'Result, Position, Position - 1 + New_Item'Length) =
             New_Item
         and then
           (if Position - 1 + New_Item'Length < Length (Source) then

              --  There are some unchanged characters of Source remaining
              --  after New_Item.

              Slice (Overwrite'Result,
                Position + New_Item'Length, Length (Source)) =
                  Slice (Source,
                    Position + New_Item'Length, Length (Source))),
     Global => null;

   procedure Overwrite
     (Source   : in out Unbounded_String;
      Position : Positive;
      New_Item : String)
   with
     Pre    => Position - 1 <= Length (Source)
                 and then New_Item'Length <= Natural'Last - (Position - 1),
     Post   =>
       --  Length of the returned string

       Length (Source) =
         Integer'Max (Length (Source)'Old, Position - 1 + New_Item'Length)

         --  Elements after Position are replaced by elements of New_Item

         and then
           Slice (Source, 1, Position - 1) = Slice (Source, 1, Position - 1)
         and then
           Slice (Source, Position, Position - 1 + New_Item'Length) = New_Item
         and then
           (if Position - 1 + New_Item'Length < Length (Source)'Old then

              --  There are some unchanged characters of Source remaining
              --  after New_Item.

              Slice (Source,
                Position + New_Item'Length, Length (Source)'Old) =
                  Slice (Source,
                    --  Really "Position + New_Item'Length" but expressed with
                    --  a conditional repeating the above test so that the
                    --  call to Slice is valid on entry (under 'Old) even
                    --  when the test evaluates to False.
                    (if Position - 1 + New_Item'Length < Length (Source)
                     then Position + New_Item'Length
                     else 1),
                    Length (Source))'Old),
     Global => null;

   function Delete
     (Source  : Unbounded_String;
      From    : Positive;
      Through : Natural) return Unbounded_String
   with
     Pre            => (if Through >= From then From - 1 <= Length (Source)),
     Contract_Cases =>
       (Through >= From =>
          Length (Delete'Result) =
            From - 1 + Natural'Max (Length (Source) - Through, 0)
            and then
              Slice (Delete'Result, 1, From - 1) = Slice (Source, 1, From - 1)
            and then
              (if Through < Length (Source) then
                 Slice (Delete'Result, From, Length (Delete'Result)) =
                   Slice (Source, Through + 1, Length (Source))),
        others          =>
             Delete'Result = Source),
     Global         => null;

   procedure Delete
     (Source  : in out Unbounded_String;
      From    : Positive;
      Through : Natural)
   with
     Pre            => (if Through >= From then From - 1 <= Length (Source)),
     Contract_Cases =>
       (Through >= From =>
          Length (Source) =
            From - 1 + Natural'Max (Length (Source)'Old - Through, 0)
            and then
              Slice (Source, 1, From - 1) =
                To_String (Source)'Old (1 .. From - 1)
            and then
              (if Through < Length (Source) then
                Slice (Source, From, Length (Source)) =
                  To_String (Source)'Old (Through + 1 .. Length (Source)'Old)),
        others          =>
          To_String (Source) = To_String (Source)'Old),
     Global         => null;

   function Trim
     (Source : Unbounded_String;
      Side   : Trim_End) return Unbounded_String
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
     (Source : in out Unbounded_String;
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
                else Index_Non_Blank (Source, Forward)'Old);
             High : constant Positive :=
               (if Side = Left then Length (Source)'Old
                else Index_Non_Blank (Source, Backward)'Old);
           begin
             To_String (Source) = To_String (Source)'Old (Low .. High))),
     Global         => null;

   function Trim
     (Source : Unbounded_String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return Unbounded_String
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
     (Source : in out Unbounded_String;
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
               Index (Source, Left, Outside, Forward)'Old;
             High : constant Positive :=
               Index (Source, Right, Outside, Backward)'Old;
           begin
             To_String (Source) = To_String (Source)'Old (Low .. High))),
     Global         => null;

   function Head
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String
   with
     Post           => Length (Head'Result) = Count,
     Contract_Cases =>
       (Count <= Length (Source)
        =>
          --  Source is cut

          To_String (Head'Result) = Slice (Source, 1, Count),

        others
        =>
          --  Source is followed by Pad characters

          Slice (Head'Result, 1, Length (Source)) = To_String (Source)
            and then
              Slice (Head'Result, Length (Source) + 1, Count) =
                [1 .. Count - Length (Source) => Pad]),
     Global         => null;

   procedure Head
     (Source : in out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
   with
     Post           => Length (Source) = Count,
     Contract_Cases =>
       (Count <= Length (Source)
        =>
          --  Source is cut

          To_String (Source) = Slice (Source, 1, Count),

        others
        =>
          --  Source is followed by Pad characters

          Slice (Source, 1, Length (Source)'Old) = To_String (Source)'Old
            and then
              Slice (Source, Length (Source)'Old + 1, Count) =
                [1 .. Count - Length (Source)'Old => Pad]),
     Global         => null;

   function Tail
     (Source : Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space) return Unbounded_String
   with
     Post           => Length (Tail'Result) = Count,
     Contract_Cases =>
       (Count = 0
        =>
          True,

        (Count in 1 .. Length (Source))
        =>
          --  Source is cut

          To_String (Tail'Result) =
            Slice (Source, Length (Source) - Count + 1, Length (Source)),

        others
        =>
          --  Source is preceded by Pad characters

          (if Length (Source) = 0 then
            To_String (Tail'Result) = [1 .. Count => Pad]
          else
            Slice (Tail'Result, 1, Count - Length (Source)) =
              [1 .. Count - Length (Source) => Pad]
              and then
                Slice (Tail'Result, Count - Length (Source) + 1, Count) =
                  To_String (Source))),
     Global         => null;

   procedure Tail
     (Source : in out Unbounded_String;
      Count  : Natural;
      Pad    : Character := Space)
   with
     Post           => Length (Source) = Count,
     Contract_Cases =>
       (Count = 0
        =>
          True,

        (Count in  1 .. Length (Source))
        =>
          --  Source is cut

          To_String (Source) =
            Slice (Source,
              --  Really "Length (Source) - Count + 1" but expressed with a
              --  conditional repeating the above guard so that the call to
              --  Slice is valid on entry (under 'Old) even when the test
              --  evaluates to False.
              (if Count <= Length (Source) then Length (Source) - Count + 1
               else 1),
              Length (Source))'Old,

        others
        =>
          --  Source is preceded by Pad characters

          (if Length (Source)'Old = 0 then
            To_String (Source) = [1 .. Count => Pad]
          else
            Slice (Source, 1, Count - Length (Source)'Old) =
              [1 .. Count - Length (Source)'Old => Pad]
              and then
                Slice (Source, Count - Length (Source)'Old + 1, Count) =
                  To_String (Source)'Old)),
     Global         => null;

   function "*"
     (Left  : Natural;
      Right : Character) return Unbounded_String
   with
     Pre    => Left <= Natural'Last,
     Post   => To_String ("*"'Result) = [1 .. Left => Right],
     Global => null;

   function "*"
     (Left  : Natural;
      Right : String) return Unbounded_String
   with
     Pre    => (if Left /= 0 then Right'Length <= Natural'Last / Left),
     Post =>
       Length ("*"'Result) = Left * Right'Length
         and then
           (if Right'Length > 0 then
              (for all K in 1 .. Left * Right'Length =>
                 Element ("*"'Result, K) =
                   Right (Right'First + (K - 1) mod Right'Length))),
     Global => null;

   function "*"
     (Left  : Natural;
      Right : Unbounded_String) return Unbounded_String
   with
     Pre    => (if Left /= 0 then Length (Right) <= Natural'Last / Left),
     Post =>
       Length ("*"'Result) = Left * Length (Right)
         and then
           (if Length (Right) > 0 then
              (for all K in 1 .. Left * Length (Right) =>
                 Element ("*"'Result, K) =
                   Element (Right, 1 + (K - 1) mod Length (Right)))),
     Global => null;

   --  Each of the transformation functions (Replace_Slice, Insert, Overwrite,
   --  Delete), selector functions (Trim, Head, Tail), and constructor
   --  functions ("*") is likewise analogous to its corresponding subprogram in
   --  Strings.Fixed. For each of the subprograms, the corresponding
   --  fixed-length string subprogram is applied to the string represented by
   --  the Unbounded_String parameter, and To_Unbounded_String is applied the
   --  result string.
   --
   --  For each of the procedures Translate, Replace_Slice, Insert, Overwrite,
   --  Delete, Trim, Head, and Tail, the resulting string represented by the
   --  Source parameter is given by the corresponding function for fixed-length
   --  strings applied to the string represented by Source's original value.

private
   pragma SPARK_Mode (Off);  --  Controlled types are not in SPARK

   pragma Inline (Length);

   package AF renames Ada.Finalization;

   Null_String : aliased String := "";

   function To_Unbounded (S : String) return Unbounded_String
     renames To_Unbounded_String;

   type Unbounded_String is new AF.Controlled with record
      Reference : not null String_Access := Null_String'Access;
      Last      : Natural                := 0;
   end record with Put_Image => Put_Image;

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      V : Unbounded_String);

   --  The Unbounded_String is using a buffered implementation to increase
   --  speed of the Append/Delete/Insert procedures. The Reference string
   --  pointer above contains the current string value and extra room at the
   --  end to be used by the next Append routine. Last is the index of the
   --  string ending character. So the current string value is really
   --  Reference (1 .. Last).

   pragma Stream_Convert (Unbounded_String, To_Unbounded, To_String);
   --  Provide stream routines without dragging in Ada.Streams

   pragma Finalize_Storage_Only (Unbounded_String);
   --  Finalization is required only for freeing storage

   procedure Initialize (Object : in out Unbounded_String);
   procedure Adjust     (Object : in out Unbounded_String);
   procedure Finalize   (Object : in out Unbounded_String);

   procedure Realloc_For_Chunk
     (Source     : in out Unbounded_String;
      Chunk_Size : Natural);
   pragma Inline (Realloc_For_Chunk);
   --  Adjust the size allocated for the string. Add at least Chunk_Size so it
   --  is safe to add a string of this size at the end of the current content.
   --  The real size allocated for the string is Chunk_Size + x of the current
   --  string size. This buffered handling makes the Append unbounded string
   --  routines very fast. This spec is in the private part so that it can be
   --  accessed from children (e.g. from Unbounded.Text_IO).

   Null_Unbounded_String : constant Unbounded_String :=
                             (AF.Controlled with
                                Reference => Null_String'Access,
                                Last      => 0);
end Ada.Strings.Unbounded;
