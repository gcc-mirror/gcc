------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . S T R I N G S . B O U N D E D                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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
--  setting the corresponding assertion policy to Ignore.

pragma Assertion_Policy (Pre => Ignore);

with Ada.Strings.Maps;
with Ada.Strings.Superbounded;

package Ada.Strings.Bounded is
   pragma Preelaborate;

   generic
      Max : Positive;
      --  Maximum length of a Bounded_String

   package Generic_Bounded_Length with
     Initial_Condition => Length (Null_Bounded_String) = 0
   is

      Max_Length : constant Positive := Max;

      type Bounded_String is private;
      pragma Preelaborable_Initialization (Bounded_String);

      Null_Bounded_String : constant Bounded_String;

      subtype Length_Range is Natural range 0 .. Max_Length;

      function Length (Source : Bounded_String) return Length_Range with
        Global => null;

      --------------------------------------------------------
      -- Conversion, Concatenation, and Selection Functions --
      --------------------------------------------------------

      function To_Bounded_String
        (Source : String;
         Drop   : Truncation := Error) return Bounded_String
      with
        Pre    => (if Source'Length > Max_Length then Drop /= Error),
        Post   =>
          Length (To_Bounded_String'Result)
        = Natural'Min (Max_Length, Source'Length),
        Global => null;

      function To_String (Source : Bounded_String) return String with
        Post   => To_String'Result'Length = Length (Source),
        Global => null;

      procedure Set_Bounded_String
        (Target : out Bounded_String;
         Source : String;
         Drop   : Truncation := Error)
      with
        Pre    => (if Source'Length > Max_Length then Drop /= Error),
        Post   => Length (Target) = Natural'Min (Max_Length, Source'Length),
        Global => null;
      pragma Ada_05 (Set_Bounded_String);

      function Append
        (Left  : Bounded_String;
         Right : Bounded_String;
         Drop  : Truncation  := Error) return Bounded_String
      with
        Pre    =>
          (if Length (Left) > Max_Length - Length (Right)
           then Drop /= Error),
        Post   =>
          Length (Append'Result)
        = Natural'Min (Max_Length, Length (Left) + Length (Right)),
        Global => null;

      function Append
        (Left  : Bounded_String;
         Right : String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre    =>
          (if Right'Length > Max_Length - Length (Left)
           then Drop /= Error),
        Post   =>
          Length (Append'Result)
        = Natural'Min (Max_Length, Length (Left) + Right'Length),
        Global => null;

      function Append
        (Left  : String;
         Right : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre    =>
          (if Left'Length > Max_Length - Length (Right)
           then Drop /= Error),
        Post   =>
          Length (Append'Result)
        = Natural'Min (Max_Length, Left'Length + Length (Right)),
        Global => null;

      function Append
        (Left  : Bounded_String;
         Right : Character;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre    => (if Length (Left) = Max_Length then Drop /= Error),
        Post   =>
          Length (Append'Result)
        = Natural'Min (Max_Length, Length (Left) + 1),
        Global => null;

      function Append
        (Left  : Character;
         Right : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre    => (if Length (Right) = Max_Length then Drop /= Error),
        Post   =>
          Length (Append'Result)
        = Natural'Min (Max_Length, 1 + Length (Right)),
        Global => null;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : Bounded_String;
         Drop     : Truncation  := Error)
      with
        Pre    =>
          (if Length (Source) > Max_Length - Length (New_Item)
           then Drop /= Error),
        Post   =>
          Length (Source)
        = Natural'Min (Max_Length, Length (Source)'Old + Length (New_Item)),
        Global => null;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : String;
         Drop     : Truncation  := Error)
      with
        Pre    =>
          (if New_Item'Length > Max_Length - Length (Source)
           then Drop /= Error),
        Post   =>
          Length (Source)
        = Natural'Min (Max_Length, Length (Source)'Old + New_Item'Length),
        Global => null;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : Character;
         Drop     : Truncation  := Error)
      with
        Pre    => (if Length (Source) = Max_Length then Drop /= Error),
        Post   =>
          Length (Source)
        = Natural'Min (Max_Length, Length (Source)'Old + 1),
        Global => null;

      function "&"
        (Left  : Bounded_String;
         Right : Bounded_String) return Bounded_String
      with
        Pre    => Length (Left) <= Max_Length - Length (Right),
        Post   => Length ("&"'Result) = Length (Left) + Length (Right),
        Global => null;

      function "&"
        (Left  : Bounded_String;
         Right : String) return Bounded_String
      with
        Pre    => Right'Length <= Max_Length - Length (Left),
        Post   => Length ("&"'Result) = Length (Left) + Right'Length,
        Global => null;

      function "&"
        (Left  : String;
         Right : Bounded_String) return Bounded_String
      with
        Pre    => Left'Length <= Max_Length - Length (Right),
        Post   => Length ("&"'Result) = Left'Length + Length (Right),
        Global => null;

      function "&"
        (Left  : Bounded_String;
         Right : Character) return Bounded_String
      with
        Pre    => Length (Left) < Max_Length,
        Post   => Length ("&"'Result) = Length (Left) + 1,
        Global => null;

      function "&"
        (Left  : Character;
         Right : Bounded_String) return Bounded_String
      with
        Pre    => Length (Right) < Max_Length,
        Post   => Length ("&"'Result) = 1 + Length (Right),
        Global => null;

      function Element
        (Source : Bounded_String;
         Index  : Positive) return Character
      with
        Pre    => Index <= Length (Source),
        Global => null;

      procedure Replace_Element
        (Source : in out Bounded_String;
         Index  : Positive;
         By     : Character)
      with
        Pre    => Index <= Length (Source),
        Post   => Length (Source) = Length (Source)'Old,
        Global => null;

      function Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural) return String
      with
        Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
        Post   => Slice'Result'Length = Natural'Max (0, High - Low + 1),
        Global => null;

      function Bounded_Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural) return Bounded_String
       with
        Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
        Post   =>
          Length (Bounded_Slice'Result) = Natural'Max (0, High - Low + 1),
        Global => null;
      pragma Ada_05 (Bounded_Slice);

      procedure Bounded_Slice
        (Source : Bounded_String;
         Target : out Bounded_String;
         Low    : Positive;
         High   : Natural)
      with
        Pre    => Low - 1 <= Length (Source) and then High <= Length (Source),
        Post   => Length (Target) = Natural'Max (0, High - Low + 1),
        Global => null;
      pragma Ada_05 (Bounded_Slice);

      function "="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function "="
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Global => null;

      function "="
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function "<"
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function "<"
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Global => null;

      function "<"
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function "<="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function "<="
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Global => null;

      function "<="
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function ">"
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function ">"
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Global => null;

      function ">"
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function ">="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      function ">="
        (Left  : Bounded_String;
         Right : String) return Boolean
      with
        Global => null;

      function ">="
        (Left  : String;
         Right : Bounded_String) return Boolean
      with
        Global => null;

      ----------------------
      -- Search Functions --
      ----------------------

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
      with
        Pre    => Pattern'Length /= 0,
        Global => null;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping_Function) return Natural
      with
        Pre    => Pattern'Length /= 0,
        Global => null;

      function Index
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         Test   : Membership := Inside;
         Going  : Direction  := Forward) return Natural
      with
        Global => null;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural
      with
        Pre    =>
          (if Length (Source) /= 0
           then From <= Length (Source))
                  and then Pattern'Length /= 0,
        Global => null;
      pragma Ada_05 (Index);

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping_Function) return Natural
      with
        Pre    =>
          (if Length (Source) /= 0
           then From <= Length (Source))
                  and then Pattern'Length /= 0,
        Global => null;
      pragma Ada_05 (Index);

      function Index
        (Source  : Bounded_String;
         Set     : Maps.Character_Set;
         From    : Positive;
         Test    : Membership := Inside;
         Going   : Direction := Forward) return Natural
      with
        Pre    => (if Length (Source) /= 0 then From <= Length (Source)),
        Global => null;
      pragma Ada_05 (Index);

      function Index_Non_Blank
        (Source : Bounded_String;
         Going  : Direction := Forward) return Natural
      with
        Global => null;

      function Index_Non_Blank
        (Source : Bounded_String;
         From   : Positive;
         Going  : Direction := Forward) return Natural
      with
        Pre    => (if Length (Source) /= 0 then From <= Length (Source)),
        Global => null;
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
        Pre    => Pattern'Length /= 0,
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
        Pre    => (if Length (Source) /= 0 then From <= Length (Source)),
        Global => null;
      pragma Ada_2012 (Find_Token);

      procedure Find_Token
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         Test   : Membership;
         First  : out Positive;
         Last   : out Natural)
      with
        Global => null;

      ------------------------------------
      -- String Translation Subprograms --
      ------------------------------------

      function Translate
        (Source  : Bounded_String;
         Mapping : Maps.Character_Mapping) return Bounded_String
      with
        Post   => Length (Translate'Result) = Length (Source),
        Global => null;

      procedure Translate
        (Source   : in out Bounded_String;
         Mapping  : Maps.Character_Mapping)
      with
        Post   => Length (Source) = Length (Source)'Old,
        Global => null;

      function Translate
        (Source  : Bounded_String;
         Mapping : Maps.Character_Mapping_Function) return Bounded_String
      with
        Post   => Length (Translate'Result) = Length (Source),
        Global => null;

      procedure Translate
        (Source  : in out Bounded_String;
         Mapping : Maps.Character_Mapping_Function)
      with
        Post   => Length (Source) = Length (Source)'Old,
        Global => null;

      ---------------------------------------
      -- String Transformation Subprograms --
      ---------------------------------------

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
                    - Natural'Max (Length (Source) - High, 0)
                 else Length (Source) <= Max_Length - By'Length)),
        Contract_Cases =>
          (High >= Low =>
             Length (Replace_Slice'Result)
           = Natural'Min
             (Max_Length,
              Low - 1 + By'Length + Natural'Max (Length (Source) - High,
                                                  0)),
           others      =>
             Length (Replace_Slice'Result)
           = Natural'Min (Max_Length, Length (Source) + By'Length)),
        Global         => null;

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
          (High >= Low =>
            Length (Source)
          = Natural'Min
              (Max_Length,
               Low - 1 + By'Length + Natural'Max (Length (Source)'Old - High,
                                                  0)),
           others      =>
             Length (Source)
           = Natural'Min (Max_Length, Length (Source)'Old + By'Length)),
        Global         => null;

      function Insert
        (Source   : Bounded_String;
         Before   : Positive;
         New_Item : String;
         Drop     : Truncation := Error) return Bounded_String
      with
        Pre    =>
          Before - 1 <= Length (Source)
            and then (if New_Item'Length > Max_Length - Length (Source)
                      then Drop /= Error),
        Post   =>
          Length (Insert'Result)
        = Natural'Min (Max_Length, Length (Source) + New_Item'Length),
        Global => null;

      procedure Insert
        (Source   : in out Bounded_String;
         Before   : Positive;
         New_Item : String;
         Drop     : Truncation := Error)
      with
        Pre    =>
          Before - 1 <= Length (Source)
            and then (if New_Item'Length > Max_Length - Length (Source)
                      then Drop /= Error),
        Post   =>
          Length (Source)
        = Natural'Min (Max_Length, Length (Source)'Old + New_Item'Length),
        Global => null;

      function Overwrite
        (Source   : Bounded_String;
         Position : Positive;
         New_Item : String;
         Drop     : Truncation := Error) return Bounded_String
      with
        Pre    =>
          Position - 1 <= Length (Source)
            and then (if New_Item'Length > Max_Length - (Position - 1)
                      then Drop /= Error),
        Post   =>
          Length (Overwrite'Result)
        = Natural'Max
            (Length (Source),
             Natural'Min (Max_Length, Position - 1 + New_Item'Length)),
        Global => null;

      procedure Overwrite
        (Source    : in out Bounded_String;
         Position  : Positive;
         New_Item  : String;
         Drop      : Truncation := Error)
      with
        Pre    =>
          Position - 1 <= Length (Source)
            and then (if New_Item'Length > Max_Length - (Position - 1)
                      then Drop /= Error),
        Post   =>
          Length (Source)
        = Natural'Max
            (Length (Source)'Old,
             Natural'Min (Max_Length, Position - 1 + New_Item'Length)),
        Global => null;

      function Delete
        (Source  : Bounded_String;
         From    : Positive;
         Through : Natural) return Bounded_String
      with
        Pre            =>
          (if Through <= From then From - 1 <= Length (Source)),
        Contract_Cases =>
          (Through >= From =>
             Length (Delete'Result) = Length (Source) - (Through - From + 1),
           others          =>
             Length (Delete'Result) = Length (Source)),

        Global         => null;

      procedure Delete
        (Source  : in out Bounded_String;
         From    : Positive;
         Through : Natural)
      with
        Pre            =>
          (if Through <= From then From - 1 <= Length (Source)),
        Contract_Cases =>
          (Through >= From =>
             Length (Source) = Length (Source)'Old - (Through - From + 1),
           others          =>
             Length (Source) = Length (Source)'Old),
        Global         => null;

      ---------------------------------
      -- String Selector Subprograms --
      ---------------------------------

      function Trim
        (Source : Bounded_String;
         Side   : Trim_End) return Bounded_String
      with
        Post   => Length (Trim'Result) <= Length (Source),
        Global => null;

      procedure Trim
        (Source : in out Bounded_String;
         Side   : Trim_End)
      with
        Post   => Length (Source) <= Length (Source)'Old,
        Global => null;

      function Trim
        (Source : Bounded_String;
         Left   : Maps.Character_Set;
         Right  : Maps.Character_Set) return Bounded_String
      with
        Post   => Length (Trim'Result) <= Length (Source),
        Global => null;

      procedure Trim
        (Source : in out Bounded_String;
         Left   : Maps.Character_Set;
         Right  : Maps.Character_Set)
      with
        Post   => Length (Source) <= Length (Source)'Old,
        Global => null;

      function Head
        (Source : Bounded_String;
         Count  : Natural;
         Pad    : Character := Space;
         Drop   : Truncation := Error) return Bounded_String
      with
        Pre    => (if Count > Max_Length then Drop /= Error),
        Post   => Length (Head'Result) = Natural'Min (Max_Length, Count),
        Global => null;

      procedure Head
        (Source : in out Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error)
      with
        Pre    => (if Count > Max_Length then Drop /= Error),
        Post   => Length (Source) = Natural'Min (Max_Length, Count),
        Global => null;

      function Tail
        (Source : Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error) return Bounded_String
      with
        Pre    => (if Count > Max_Length then Drop /= Error),
        Post   => Length (Tail'Result) = Natural'Min (Max_Length, Count),
        Global => null;

      procedure Tail
        (Source : in out Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error)
      with
        Pre    => (if Count > Max_Length then Drop /= Error),
        Post   => Length (Source) = Natural'Min (Max_Length, Count),
        Global => null;

      ------------------------------------
      -- String Constructor Subprograms --
      ------------------------------------

      function "*"
        (Left  : Natural;
         Right : Character) return Bounded_String
      with
        Pre    => Left <= Max_Length,
        Post   => Length ("*"'Result) = Left,
        Global => null;

      function "*"
        (Left  : Natural;
         Right : String) return Bounded_String
      with
        Pre    => (if Left /= 0 then Right'Length <= Max_Length / Left),
        Post   => Length ("*"'Result) = Left * Right'Length,
        Global => null;

      function "*"
        (Left  : Natural;
         Right : Bounded_String) return Bounded_String
      with
        Pre    => (if Left /= 0 then Length (Right) <= Max_Length / Left),
        Post   => Length ("*"'Result) = Left * Length (Right),
        Global => null;

      function Replicate
        (Count : Natural;
         Item  : Character;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre    => (if Count > Max_Length then Drop /= Error),
        Post   =>
          Length (Replicate'Result)
        = Natural'Min (Max_Length, Count),
        Global => null;

      function Replicate
        (Count : Natural;
         Item  : String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre    =>
          (if Item'Length /= 0
             and then Count > Max_Length / Item'Length
           then Drop /= Error),
        Post   =>
          Length (Replicate'Result)
        = Natural'Min (Max_Length, Count * Item'Length),
        Global => null;

      function Replicate
        (Count : Natural;
         Item  : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String
      with
        Pre    =>
          (if Length (Item) /= 0
             and then Count > Max_Length / Length (Item)
           then Drop /= Error),
        Post   =>
          Length (Replicate'Result)
        = Natural'Min (Max_Length, Count * Length (Item)),
        Global => null;

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

      function From_String (Source : String) return Bounded_String;
      --  Private routine used only by Stream_Convert

      pragma Stream_Convert (Bounded_String, From_String, To_String);
      --  Provide stream routines without dragging in Ada.Streams

      Null_Bounded_String : constant Bounded_String :=
                              (Max_Length     => Max_Length,
                               Current_Length => 0,
                               Data           =>
                                 (1 .. Max_Length => ASCII.NUL));

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
