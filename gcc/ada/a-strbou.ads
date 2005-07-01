------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . S T R I N G S . B O U N D E D                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Maps;
with Ada.Strings.Superbounded;

package Ada.Strings.Bounded is
pragma Preelaborate (Bounded);

   generic
      Max : Positive;
      --  Maximum length of a Bounded_String

   package Generic_Bounded_Length is

      Max_Length : constant Positive := Max;

      type Bounded_String is private;

      Null_Bounded_String : constant Bounded_String;

      subtype Length_Range is Natural range 0 .. Max_Length;

      function Length (Source : Bounded_String) return Length_Range;

      --------------------------------------------------------
      -- Conversion, Concatenation, and Selection Functions --
      --------------------------------------------------------

      function To_Bounded_String
        (Source : String;
         Drop   : Truncation := Error) return Bounded_String;

      function To_String (Source : Bounded_String) return String;

      procedure Set_Bounded_String
        (Target : out Bounded_String;
         Source : String;
         Drop   : Truncation := Error);
      pragma Ada_05 (Set_Bounded_String);

      function Append
        (Left  : Bounded_String;
         Right : Bounded_String;
         Drop  : Truncation  := Error) return Bounded_String;

      function Append
        (Left  : Bounded_String;
         Right : String;
         Drop  : Truncation := Error) return Bounded_String;

      function Append
        (Left  : String;
         Right : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String;

      function Append
        (Left  : Bounded_String;
         Right : Character;
         Drop  : Truncation := Error) return Bounded_String;

      function Append
        (Left  : Character;
         Right : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : Bounded_String;
         Drop     : Truncation  := Error);

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : String;
         Drop     : Truncation  := Error);

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : Character;
         Drop     : Truncation  := Error);

      function "&"
        (Left  : Bounded_String;
         Right : Bounded_String) return Bounded_String;

      function "&"
        (Left  : Bounded_String;
         Right : String) return Bounded_String;

      function "&"
        (Left  : String;
         Right : Bounded_String) return Bounded_String;

      function "&"
        (Left  : Bounded_String;
         Right : Character) return Bounded_String;

      function "&"
        (Left  : Character;
         Right : Bounded_String) return Bounded_String;

      function Element
        (Source : Bounded_String;
         Index  : Positive) return Character;

      procedure Replace_Element
        (Source : in out Bounded_String;
         Index  : Positive;
         By     : Character);

      function Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural) return String;

      function Bounded_Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural) return Bounded_String;
      pragma Ada_05 (Bounded_Slice);

      procedure Bounded_Slice
        (Source : Bounded_String;
         Target : out Bounded_String;
         Low    : Positive;
         High   : Natural);
      pragma Ada_05 (Bounded_Slice);

      function "="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean;

      function "="
        (Left  : Bounded_String;
         Right : String) return Boolean;

      function "="
        (Left  : String;
         Right : Bounded_String) return Boolean;

      function "<"
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean;

      function "<"
        (Left  : Bounded_String;
         Right : String) return Boolean;

      function "<"
        (Left  : String;
         Right : Bounded_String) return Boolean;

      function "<="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean;

      function "<="
        (Left  : Bounded_String;
         Right : String) return Boolean;

      function "<="
        (Left  : String;
         Right : Bounded_String) return Boolean;

      function ">"
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean;

      function ">"
        (Left  : Bounded_String;
         Right : String) return Boolean;

      function ">"
        (Left  : String;
         Right : Bounded_String) return Boolean;

      function ">="
        (Left  : Bounded_String;
         Right : Bounded_String) return Boolean;

      function ">="
        (Left  : Bounded_String;
         Right : String) return Boolean;

      function ">="
        (Left  : String;
         Right : Bounded_String) return Boolean;

      ----------------------
      -- Search Functions --
      ----------------------

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping_Function) return Natural;

      function Index
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         Test   : Membership := Inside;
         Going  : Direction  := Forward) return Natural;

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;
      pragma Ada_05 (Index);

      function Index
        (Source  : Bounded_String;
         Pattern : String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Maps.Character_Mapping_Function) return Natural;
      pragma Ada_05 (Index);

      function Index
        (Source  : Bounded_String;
         Set     : Maps.Character_Set;
         From    : Positive;
         Test    : Membership := Inside;
         Going   : Direction := Forward) return Natural;
      pragma Ada_05 (Index);

      function Index_Non_Blank
        (Source : Bounded_String;
         Going  : Direction := Forward) return Natural;

      function Index_Non_Blank
        (Source : Bounded_String;
         From   : Positive;
         Going  : Direction := Forward) return Natural;
      pragma Ada_05 (Index_Non_Blank);

      function Count
        (Source  : Bounded_String;
         Pattern : String;
         Mapping : Maps.Character_Mapping := Maps.Identity) return Natural;

      function Count
        (Source  : Bounded_String;
         Pattern : String;
         Mapping : Maps.Character_Mapping_Function) return Natural;

      function Count
        (Source : Bounded_String;
         Set    : Maps.Character_Set) return Natural;

      procedure Find_Token
        (Source : Bounded_String;
         Set    : Maps.Character_Set;
         Test   : Membership;
         First  : out Positive;
         Last   : out Natural);

      ------------------------------------
      -- String Translation Subprograms --
      ------------------------------------

      function Translate
        (Source  : Bounded_String;
         Mapping : Maps.Character_Mapping) return Bounded_String;

      procedure Translate
        (Source   : in out Bounded_String;
         Mapping  : Maps.Character_Mapping);

      function Translate
        (Source  : Bounded_String;
         Mapping : Maps.Character_Mapping_Function) return Bounded_String;

      procedure Translate
        (Source  : in out Bounded_String;
         Mapping : Maps.Character_Mapping_Function);

      ---------------------------------------
      -- String Transformation Subprograms --
      ---------------------------------------

      function Replace_Slice
        (Source : Bounded_String;
         Low    : Positive;
         High   : Natural;
         By     : String;
         Drop   : Truncation := Error) return Bounded_String;

      procedure Replace_Slice
        (Source   : in out Bounded_String;
         Low      : Positive;
         High     : Natural;
         By       : String;
         Drop     : Truncation := Error);

      function Insert
        (Source   : Bounded_String;
         Before   : Positive;
         New_Item : String;
         Drop     : Truncation := Error) return Bounded_String;

      procedure Insert
        (Source   : in out Bounded_String;
         Before   : Positive;
         New_Item : String;
         Drop     : Truncation := Error);

      function Overwrite
        (Source   : Bounded_String;
         Position : Positive;
         New_Item : String;
         Drop     : Truncation := Error) return Bounded_String;

      procedure Overwrite
        (Source    : in out Bounded_String;
         Position  : Positive;
         New_Item  : String;
         Drop      : Truncation := Error);

      function Delete
        (Source  : Bounded_String;
         From    : Positive;
         Through : Natural) return Bounded_String;

      procedure Delete
        (Source  : in out Bounded_String;
         From    : Positive;
         Through : Natural);

      ---------------------------------
      -- String Selector Subprograms --
      ---------------------------------

      function Trim
        (Source : Bounded_String;
         Side   : Trim_End) return Bounded_String;

      procedure Trim
        (Source : in out Bounded_String;
         Side   : Trim_End);

      function Trim
        (Source : Bounded_String;
          Left  : Maps.Character_Set;
          Right : Maps.Character_Set) return Bounded_String;

      procedure Trim
        (Source : in out Bounded_String;
         Left   : Maps.Character_Set;
         Right  : Maps.Character_Set);

      function Head
        (Source : Bounded_String;
         Count  : Natural;
         Pad    : Character := Space;
         Drop   : Truncation := Error) return Bounded_String;

      procedure Head
        (Source : in out Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error);

      function Tail
        (Source : Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error) return Bounded_String;

      procedure Tail
        (Source : in out Bounded_String;
         Count  : Natural;
         Pad    : Character  := Space;
         Drop   : Truncation := Error);

      ------------------------------------
      -- String Constructor Subprograms --
      ------------------------------------

      function "*"
        (Left  : Natural;
         Right : Character) return Bounded_String;

      function "*"
        (Left  : Natural;
         Right : String) return Bounded_String;

      function "*"
        (Left  : Natural;
         Right : Bounded_String) return Bounded_String;

      function Replicate
        (Count : Natural;
         Item  : Character;
         Drop  : Truncation := Error) return Bounded_String;

      function Replicate
        (Count : Natural;
         Item  : String;
         Drop  : Truncation := Error) return Bounded_String;

      function Replicate
        (Count : Natural;
         Item  : Bounded_String;
         Drop  : Truncation := Error) return Bounded_String;

   private
      --  Most of the implementation is in the separate non generic package
      --  Ada.Strings.Superbounded. Type Bounded_String is derived from type
      --  Superbounded.Super_String with the maximum length constraint. In
      --  almost all cases, the routines in Superbounded can be called with
      --  no requirement to pass the maximum length explicitly, since there
      --  is at least one Bounded_String argument from which the maximum
      --  length can be obtained. For all such routines, the implementation
      --  in this private part is simply a renaming of the corresponding
      --  routine in the super bouded package.

      --  The five exceptions are the * and Replicate routines operating on
      --  character values. For these cases, we have a routine in the body
      --  that calls the superbounded routine passing the maximum length
      --  explicitly as an extra parameter.

      type Bounded_String is new Superbounded.Super_String (Max_Length);
      --  Deriving Bounded_String from Superbounded.Super_String is the
      --  real trick, it ensures that the type Bounded_String declared in
      --  the generic instantiation is compatible with the Super_String
      --  type declared in the Superbounded package.

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

      function "="
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
