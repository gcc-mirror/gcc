------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--        A D A . S T R I N G S . W I D E _ W I D E _ B O U N D E D         --
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
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.Wide_Wide_Superbounded;

package Ada.Strings.Wide_Wide_Bounded is
pragma Preelaborate (Wide_Wide_Bounded);

   generic
      Max : Positive;
      --  Maximum length of a Bounded_Wide_Wide_String

   package Generic_Bounded_Length is

      Max_Length : constant Positive := Max;

      type Bounded_Wide_Wide_String is private;

      Null_Bounded_Wide_Wide_String : constant Bounded_Wide_Wide_String;

      subtype Length_Range is Natural range 0 .. Max_Length;

      function Length (Source : Bounded_Wide_Wide_String) return Length_Range;

      --------------------------------------------------------
      -- Conversion, Concatenation, and Selection Functions --
      --------------------------------------------------------

      function To_Bounded_Wide_Wide_String
        (Source : Wide_Wide_String;
         Drop   : Truncation := Error) return Bounded_Wide_Wide_String;

      function To_Wide_Wide_String
        (Source : Bounded_Wide_Wide_String) return Wide_Wide_String;

      procedure Set_Bounded_Wide_Wide_String
        (Target : out Bounded_Wide_Wide_String;
         Source : Wide_Wide_String;
         Drop   : Truncation := Error);
      pragma Ada_05 (Set_Bounded_Wide_Wide_String);

      function Append
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String;
         Drop  : Truncation  := Error) return Bounded_Wide_Wide_String;

      function Append
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String;

      function Append
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String;

      function Append
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_Character;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String;

      function Append
        (Left  : Wide_Wide_Character;
         Right : Bounded_Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String;

      procedure Append
        (Source   : in out Bounded_Wide_Wide_String;
         New_Item : Bounded_Wide_Wide_String;
         Drop     : Truncation  := Error);

      procedure Append
        (Source   : in out Bounded_Wide_Wide_String;
         New_Item : Wide_Wide_String;
         Drop     : Truncation  := Error);

      procedure Append
        (Source   : in out Bounded_Wide_Wide_String;
         New_Item : Wide_Wide_Character;
         Drop     : Truncation  := Error);

      function "&"
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Bounded_Wide_Wide_String;

      function "&"
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Bounded_Wide_Wide_String;

      function "&"
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Bounded_Wide_Wide_String;

      function "&"
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_Character) return Bounded_Wide_Wide_String;

      function "&"
        (Left  : Wide_Wide_Character;
         Right : Bounded_Wide_Wide_String) return Bounded_Wide_Wide_String;

      function Element
        (Source : Bounded_Wide_Wide_String;
         Index  : Positive) return Wide_Wide_Character;

      procedure Replace_Element
        (Source : in out Bounded_Wide_Wide_String;
         Index  : Positive;
         By     : Wide_Wide_Character);

      function Slice
        (Source : Bounded_Wide_Wide_String;
         Low    : Positive;
         High   : Natural) return Wide_Wide_String;

      function Bounded_Slice
        (Source : Bounded_Wide_Wide_String;
         Low    : Positive;
         High   : Natural) return Bounded_Wide_Wide_String;
      pragma Ada_05 (Bounded_Slice);

      procedure Bounded_Slice
        (Source : Bounded_Wide_Wide_String;
         Target : out Bounded_Wide_Wide_String;
         Low    : Positive;
         High   : Natural);
      pragma Ada_05 (Bounded_Slice);

      function "="
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function "="
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean;

      function "="
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function "<"
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function "<"
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean;

      function "<"
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function "<="
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function "<="
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean;

      function "<="
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function ">"
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function ">"
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean;

      function ">"
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function ">="
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      function ">="
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean;

      function ">="
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean;

      ----------------------
      -- Search Functions --
      ----------------------

      function Index
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Going   : Direction := Forward;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                     Wide_Wide_Maps.Identity)
         return Natural;

      function Index
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Going   : Direction := Forward;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         return Natural;

      function Index
        (Source : Bounded_Wide_Wide_String;
         Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
         Test   : Membership := Inside;
         Going  : Direction  := Forward) return Natural;

      function Index
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                     Wide_Wide_Maps.Identity)
         return Natural;
      pragma Ada_05 (Index);

      function Index
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         return Natural;
      pragma Ada_05 (Index);

      function Index
        (Source : Bounded_Wide_Wide_String;
         Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
         From   : Positive;
         Test   : Membership := Inside;
         Going  : Direction := Forward) return Natural;
      pragma Ada_05 (Index);

      function Index_Non_Blank
        (Source : Bounded_Wide_Wide_String;
         Going  : Direction := Forward) return Natural;

      function Index_Non_Blank
        (Source : Bounded_Wide_Wide_String;
         From   : Positive;
         Going  : Direction := Forward) return Natural;
      pragma Ada_05 (Index_Non_Blank);

      function Count
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                     Wide_Wide_Maps.Identity)
         return Natural;

      function Count
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         return Natural;

      function Count
        (Source : Bounded_Wide_Wide_String;
         Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural;

      procedure Find_Token
        (Source : Bounded_Wide_Wide_String;
         Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
         Test   : Membership;
         First  : out Positive;
         Last   : out Natural);

      ------------------------------------
      -- String Translation Subprograms --
      ------------------------------------

      function Translate
        (Source  : Bounded_Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
         return Bounded_Wide_Wide_String;

      procedure Translate
        (Source  : in out Bounded_Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping);

      function Translate
        (Source  : Bounded_Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         return Bounded_Wide_Wide_String;

      procedure Translate
        (Source  : in out Bounded_Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function);

      ---------------------------------------
      -- String Transformation Subprograms --
      ---------------------------------------

      function Replace_Slice
        (Source : Bounded_Wide_Wide_String;
         Low    : Positive;
         High   : Natural;
         By     : Wide_Wide_String;
         Drop   : Truncation := Error) return Bounded_Wide_Wide_String;

      procedure Replace_Slice
        (Source   : in out Bounded_Wide_Wide_String;
         Low      : Positive;
         High     : Natural;
         By       : Wide_Wide_String;
         Drop     : Truncation := Error);

      function Insert
        (Source   : Bounded_Wide_Wide_String;
         Before   : Positive;
         New_Item : Wide_Wide_String;
         Drop     : Truncation := Error) return Bounded_Wide_Wide_String;

      procedure Insert
        (Source   : in out Bounded_Wide_Wide_String;
         Before   : Positive;
         New_Item : Wide_Wide_String;
         Drop     : Truncation := Error);

      function Overwrite
        (Source   : Bounded_Wide_Wide_String;
         Position : Positive;
         New_Item : Wide_Wide_String;
         Drop     : Truncation := Error) return Bounded_Wide_Wide_String;

      procedure Overwrite
        (Source    : in out Bounded_Wide_Wide_String;
         Position  : Positive;
         New_Item  : Wide_Wide_String;
         Drop      : Truncation := Error);

      function Delete
        (Source  : Bounded_Wide_Wide_String;
         From    : Positive;
         Through : Natural) return Bounded_Wide_Wide_String;

      procedure Delete
        (Source  : in out Bounded_Wide_Wide_String;
         From    : Positive;
         Through : Natural);

      ---------------------------------
      -- String Selector Subprograms --
      ---------------------------------

      function Trim
        (Source : Bounded_Wide_Wide_String;
         Side   : Trim_End) return Bounded_Wide_Wide_String;

      procedure Trim
        (Source : in out Bounded_Wide_Wide_String;
         Side   : Trim_End);

      function Trim
        (Source : Bounded_Wide_Wide_String;
          Left  : Wide_Wide_Maps.Wide_Wide_Character_Set;
          Right : Wide_Wide_Maps.Wide_Wide_Character_Set)
          return Bounded_Wide_Wide_String;

      procedure Trim
        (Source : in out Bounded_Wide_Wide_String;
         Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
         Right  : Wide_Wide_Maps.Wide_Wide_Character_Set);

      function Head
        (Source : Bounded_Wide_Wide_String;
         Count  : Natural;
         Pad    : Wide_Wide_Character := Wide_Wide_Space;
         Drop   : Truncation := Error) return Bounded_Wide_Wide_String;

      procedure Head
        (Source : in out Bounded_Wide_Wide_String;
         Count  : Natural;
         Pad    : Wide_Wide_Character  := Wide_Wide_Space;
         Drop   : Truncation := Error);

      function Tail
        (Source : Bounded_Wide_Wide_String;
         Count  : Natural;
         Pad    : Wide_Wide_Character := Wide_Wide_Space;
         Drop   : Truncation     := Error) return Bounded_Wide_Wide_String;

      procedure Tail
        (Source : in out Bounded_Wide_Wide_String;
         Count  : Natural;
         Pad    : Wide_Wide_Character := Wide_Wide_Space;
         Drop   : Truncation     := Error);

      ------------------------------------
      -- String Constructor Subprograms --
      ------------------------------------

      function "*"
        (Left  : Natural;
         Right : Wide_Wide_Character) return Bounded_Wide_Wide_String;

      function "*"
        (Left  : Natural;
         Right : Wide_Wide_String) return Bounded_Wide_Wide_String;

      function "*"
        (Left  : Natural;
         Right : Bounded_Wide_Wide_String) return Bounded_Wide_Wide_String;

      function Replicate
        (Count : Natural;
         Item  : Wide_Wide_Character;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String;

      function Replicate
        (Count : Natural;
         Item  : Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String;

      function Replicate
        (Count : Natural;
         Item  : Bounded_Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String;

   private
      --  Most of the implementation is in the separate non generic package
      --  Ada.Strings.Wide_Wide_Superbounded. Type Bounded_Wide_Wide_String is
      --  derived from type Wide_Wide_Superbounded.Super_String with the
      --  maximum length constraint. In almost all cases, the routines in
      --  Wide_Wide_Superbounded can be called with no requirement to pass the
      --  maximum length explicitly, since there is at least one
      --  Bounded_Wide_Wide_String argument from which the maximum length can
      --  be obtained. For all such routines, the implementation in this
      --  private part is simply renaming of the corresponding routine in the
      --  super bouded package.

      --  The five exceptions are the * and Replicate routines operating on
      --  character values. For these cases, we have a routine in the body
      --  that calls the superbounded routine passing the maximum length
      --  explicitly as an extra parameter.

      type Bounded_Wide_Wide_String is
        new Wide_Wide_Superbounded.Super_String (Max_Length);
      --  Deriving Bounded_Wide_Wide_String from
      --  Wide_Wide_Superbounded.Super_String is the real trick, it ensures
      --  that the type Bounded_Wide_Wide_String declared in the generic
      --  instantiation is compatible with the Super_String type declared in
      --  the Wide_Wide_Superbounded package.

      Null_Bounded_Wide_Wide_String : constant Bounded_Wide_Wide_String :=
                                   (Max_Length         => Max_Length,
                                    Current_Length     => 0,
                                    Data               =>
                                      (1 .. Max_Length =>
                                        Wide_Wide_Superbounded.Wide_Wide_NUL));

      pragma Inline (To_Bounded_Wide_Wide_String);

      procedure Set_Bounded_Wide_Wide_String
        (Target : out Bounded_Wide_Wide_String;
         Source : Wide_Wide_String;
         Drop   : Truncation := Error)
         renames Set_Super_String;

      function Length
        (Source : Bounded_Wide_Wide_String) return Length_Range
         renames Super_Length;

      function To_Wide_Wide_String
        (Source : Bounded_Wide_Wide_String) return Wide_Wide_String
         renames Super_To_String;

      function Append
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String;
         Drop  : Truncation  := Error) return Bounded_Wide_Wide_String
         renames Super_Append;

      function Append
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String
         renames Super_Append;

      function Append
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String
         renames Super_Append;

      function Append
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_Character;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String
         renames Super_Append;

      function Append
        (Left  : Wide_Wide_Character;
         Right : Bounded_Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String
         renames Super_Append;

      procedure Append
        (Source   : in out Bounded_Wide_Wide_String;
         New_Item : Bounded_Wide_Wide_String;
         Drop     : Truncation  := Error)
         renames Super_Append;

      procedure Append
        (Source   : in out Bounded_Wide_Wide_String;
         New_Item : Wide_Wide_String;
         Drop     : Truncation  := Error)
         renames Super_Append;

      procedure Append
        (Source   : in out Bounded_Wide_Wide_String;
         New_Item : Wide_Wide_Character;
         Drop     : Truncation  := Error)
         renames Super_Append;

      function "&"
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Bounded_Wide_Wide_String
         renames Concat;

      function "&"
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Bounded_Wide_Wide_String
         renames Concat;

      function "&"
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Bounded_Wide_Wide_String
         renames Concat;

      function "&"
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_Character) return Bounded_Wide_Wide_String
         renames Concat;

      function "&"
        (Left  : Wide_Wide_Character;
         Right : Bounded_Wide_Wide_String) return Bounded_Wide_Wide_String
         renames Concat;

      function Element
        (Source : Bounded_Wide_Wide_String;
         Index  : Positive) return Wide_Wide_Character
         renames Super_Element;

      procedure Replace_Element
        (Source : in out Bounded_Wide_Wide_String;
         Index  : Positive;
         By     : Wide_Wide_Character)
         renames Super_Replace_Element;

      function Slice
        (Source : Bounded_Wide_Wide_String;
         Low    : Positive;
         High   : Natural) return Wide_Wide_String
         renames Super_Slice;

      function Bounded_Slice
        (Source : Bounded_Wide_Wide_String;
         Low    : Positive;
         High   : Natural) return Bounded_Wide_Wide_String
         renames Super_Slice;

      procedure Bounded_Slice
        (Source : Bounded_Wide_Wide_String;
         Target : out Bounded_Wide_Wide_String;
         Low    : Positive;
         High   : Natural)
         renames Super_Slice;

      function "="
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Equal;

      function "="
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean
         renames Equal;

      function "="
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Equal;

      function "<"
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Less;

      function "<"
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean
         renames Less;

      function "<"
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Less;

      function "<="
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Less_Or_Equal;

      function "<="
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean
         renames Less_Or_Equal;

      function "<="
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Less_Or_Equal;

      function ">"
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Greater;

      function ">"
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean
         renames Greater;

      function ">"
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Greater;

      function ">="
        (Left  : Bounded_Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Greater_Or_Equal;

      function ">="
        (Left  : Bounded_Wide_Wide_String;
         Right : Wide_Wide_String) return Boolean
         renames Greater_Or_Equal;

      function ">="
        (Left  : Wide_Wide_String;
         Right : Bounded_Wide_Wide_String) return Boolean
         renames Greater_Or_Equal;

      function Index
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Going   : Direction := Forward;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                     Wide_Wide_Maps.Identity)
         return Natural
         renames Super_Index;

      function Index
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Going   : Direction := Forward;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         return Natural
         renames Super_Index;

      function Index
        (Source : Bounded_Wide_Wide_String;
         Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
         Test   : Membership := Inside;
         Going  : Direction  := Forward) return Natural
         renames Super_Index;

      function Index
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                     Wide_Wide_Maps.Identity)
         return Natural
         renames Super_Index;

      function Index
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         From    : Positive;
         Going   : Direction := Forward;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         return Natural
      renames Super_Index;

      function Index
        (Source : Bounded_Wide_Wide_String;
         Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
         From   : Positive;
         Test   : Membership := Inside;
         Going  : Direction := Forward) return Natural
      renames Super_Index;

      function Index_Non_Blank
        (Source : Bounded_Wide_Wide_String;
         Going  : Direction := Forward) return Natural
         renames Super_Index_Non_Blank;

      function Index_Non_Blank
        (Source : Bounded_Wide_Wide_String;
         From   : Positive;
         Going  : Direction := Forward) return Natural
         renames Super_Index_Non_Blank;

      function Count
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
                     Wide_Wide_Maps.Identity)
         return Natural
         renames Super_Count;

      function Count
        (Source  : Bounded_Wide_Wide_String;
         Pattern : Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         return Natural
         renames Super_Count;

      function Count
        (Source : Bounded_Wide_Wide_String;
         Set    : Wide_Wide_Maps.Wide_Wide_Character_Set) return Natural
         renames Super_Count;

      procedure Find_Token
        (Source : Bounded_Wide_Wide_String;
         Set    : Wide_Wide_Maps.Wide_Wide_Character_Set;
         Test   : Membership;
         First  : out Positive;
         Last   : out Natural)
         renames Super_Find_Token;

      function Translate
        (Source  : Bounded_Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
         return Bounded_Wide_Wide_String
         renames Super_Translate;

      procedure Translate
        (Source   : in out Bounded_Wide_Wide_String;
         Mapping  : Wide_Wide_Maps.Wide_Wide_Character_Mapping)
         renames Super_Translate;

      function Translate
        (Source  : Bounded_Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         return Bounded_Wide_Wide_String
         renames Super_Translate;

      procedure Translate
        (Source  : in out Bounded_Wide_Wide_String;
         Mapping : Wide_Wide_Maps.Wide_Wide_Character_Mapping_Function)
         renames Super_Translate;

      function Replace_Slice
        (Source : Bounded_Wide_Wide_String;
         Low    : Positive;
         High   : Natural;
         By     : Wide_Wide_String;
         Drop   : Truncation := Error) return Bounded_Wide_Wide_String
         renames Super_Replace_Slice;

      procedure Replace_Slice
        (Source   : in out Bounded_Wide_Wide_String;
         Low      : Positive;
         High     : Natural;
         By       : Wide_Wide_String;
         Drop     : Truncation := Error)
         renames Super_Replace_Slice;

      function Insert
        (Source   : Bounded_Wide_Wide_String;
         Before   : Positive;
         New_Item : Wide_Wide_String;
         Drop     : Truncation := Error) return Bounded_Wide_Wide_String
         renames Super_Insert;

      procedure Insert
        (Source   : in out Bounded_Wide_Wide_String;
         Before   : Positive;
         New_Item : Wide_Wide_String;
         Drop     : Truncation := Error)
         renames Super_Insert;

      function Overwrite
        (Source   : Bounded_Wide_Wide_String;
         Position : Positive;
         New_Item : Wide_Wide_String;
         Drop     : Truncation := Error) return Bounded_Wide_Wide_String
         renames Super_Overwrite;

      procedure Overwrite
        (Source    : in out Bounded_Wide_Wide_String;
         Position  : Positive;
         New_Item  : Wide_Wide_String;
         Drop      : Truncation := Error)
         renames Super_Overwrite;

      function Delete
        (Source  : Bounded_Wide_Wide_String;
         From    : Positive;
         Through : Natural) return Bounded_Wide_Wide_String
         renames Super_Delete;

      procedure Delete
        (Source  : in out Bounded_Wide_Wide_String;
         From    : Positive;
         Through : Natural)
         renames Super_Delete;

      function Trim
        (Source : Bounded_Wide_Wide_String;
         Side   : Trim_End) return Bounded_Wide_Wide_String
         renames Super_Trim;

      procedure Trim
        (Source : in out Bounded_Wide_Wide_String;
         Side   : Trim_End)
         renames Super_Trim;

      function Trim
        (Source : Bounded_Wide_Wide_String;
         Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
         Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
         return Bounded_Wide_Wide_String
         renames Super_Trim;

      procedure Trim
        (Source : in out Bounded_Wide_Wide_String;
         Left   : Wide_Wide_Maps.Wide_Wide_Character_Set;
         Right  : Wide_Wide_Maps.Wide_Wide_Character_Set)
         renames Super_Trim;

      function Head
        (Source : Bounded_Wide_Wide_String;
         Count  : Natural;
         Pad    : Wide_Wide_Character := Wide_Wide_Space;
         Drop   : Truncation     := Error) return Bounded_Wide_Wide_String
         renames Super_Head;

      procedure Head
        (Source : in out Bounded_Wide_Wide_String;
         Count  : Natural;
         Pad    : Wide_Wide_Character := Wide_Wide_Space;
         Drop   : Truncation     := Error)
         renames Super_Head;

      function Tail
        (Source : Bounded_Wide_Wide_String;
         Count  : Natural;
         Pad    : Wide_Wide_Character := Wide_Wide_Space;
         Drop   : Truncation     := Error) return Bounded_Wide_Wide_String
         renames Super_Tail;

      procedure Tail
        (Source : in out Bounded_Wide_Wide_String;
         Count  : Natural;
         Pad    : Wide_Wide_Character := Wide_Wide_Space;
         Drop   : Truncation := Error)
         renames Super_Tail;

      function "*"
        (Left  : Natural;
         Right : Bounded_Wide_Wide_String) return Bounded_Wide_Wide_String
         renames Times;

      function Replicate
        (Count : Natural;
         Item  : Bounded_Wide_Wide_String;
         Drop  : Truncation := Error) return Bounded_Wide_Wide_String
         renames Super_Replicate;

   end Generic_Bounded_Length;

end Ada.Strings.Wide_Wide_Bounded;
