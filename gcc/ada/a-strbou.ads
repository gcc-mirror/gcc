------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . S T R I N G S . B O U N D E D                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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

      function Length (Source : in Bounded_String) return Length_Range;

      --------------------------------------------------------
      -- Conversion, Concatenation, and Selection Functions --
      --------------------------------------------------------

      function To_Bounded_String
        (Source : in String;
         Drop   : in Truncation := Error)
         return   Bounded_String;

      function To_String (Source : in Bounded_String) return String;

      function Append
        (Left, Right : in Bounded_String;
         Drop        : in Truncation  := Error)
         return        Bounded_String;

      function Append
        (Left  : in Bounded_String;
         Right : in String;
         Drop  : in Truncation := Error)
         return  Bounded_String;

      function Append
        (Left  : in String;
         Right : in Bounded_String;
         Drop  : in Truncation := Error)
         return  Bounded_String;

      function Append
        (Left  : in Bounded_String;
         Right : in Character;
         Drop  : in Truncation := Error)
         return  Bounded_String;

      function Append
        (Left  : in Character;
         Right : in Bounded_String;
         Drop  : in Truncation := Error)
         return  Bounded_String;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : in Bounded_String;
         Drop     : in Truncation  := Error);

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : in String;
         Drop     : in Truncation  := Error);

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : in Character;
         Drop     : in Truncation  := Error);

      function "&"
        (Left, Right : in Bounded_String)
         return        Bounded_String;

      function "&"
        (Left  : in Bounded_String;
         Right : in String)
         return  Bounded_String;

      function "&"
        (Left  : in String;
         Right : in Bounded_String)
         return  Bounded_String;

      function "&"
        (Left  : in Bounded_String;
         Right : in Character)
         return  Bounded_String;

      function "&"
        (Left  : in Character;
         Right : in Bounded_String)
         return  Bounded_String;

      function Element
        (Source : in Bounded_String;
         Index  : in Positive)
         return   Character;

      procedure Replace_Element
        (Source : in out Bounded_String;
         Index  : in Positive;
         By     : in Character);

      function Slice
        (Source : in Bounded_String;
         Low    : in Positive;
         High   : in Natural)
         return   String;

      function "="  (Left, Right : in Bounded_String) return Boolean;

      function "="
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean;

      function "="
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean;

      function "<"  (Left, Right : in Bounded_String) return Boolean;

      function "<"
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean;

      function "<"
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean;

      function "<=" (Left, Right : in Bounded_String) return Boolean;

      function "<="
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean;

      function "<="
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean;

      function ">"  (Left, Right : in Bounded_String) return Boolean;

      function ">"
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean;

      function ">"
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean;

      function ">=" (Left, Right : in Bounded_String) return Boolean;

      function ">="
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean;

      function ">="
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean;

      ----------------------
      -- Search Functions --
      ----------------------

      function Index
        (Source  : in Bounded_String;
         Pattern : in String;
         Going   : in Direction := Forward;
         Mapping : in Maps.Character_Mapping := Maps.Identity)
         return    Natural;

      function Index
        (Source  : in Bounded_String;
         Pattern : in String;
         Going   : in Direction := Forward;
         Mapping : in Maps.Character_Mapping_Function)
         return    Natural;

      function Index
        (Source : in Bounded_String;
         Set    : in Maps.Character_Set;
         Test   : in Membership := Inside;
         Going  : in Direction  := Forward)
         return   Natural;

      function Index_Non_Blank
        (Source : in Bounded_String;
         Going  : in Direction := Forward)
         return   Natural;

      function Count
        (Source  : in Bounded_String;
         Pattern : in String;
         Mapping : in Maps.Character_Mapping := Maps.Identity)
         return    Natural;

      function Count
        (Source  : in Bounded_String;
         Pattern : in String;
         Mapping : in Maps.Character_Mapping_Function)
         return    Natural;

      function Count
        (Source : in Bounded_String;
         Set    : in Maps.Character_Set)
         return   Natural;

      procedure Find_Token
        (Source : in Bounded_String;
         Set    : in Maps.Character_Set;
         Test   : in Membership;
         First  : out Positive;
         Last   : out Natural);

      ------------------------------------
      -- String Translation Subprograms --
      ------------------------------------

      function Translate
        (Source   : in Bounded_String;
         Mapping  : in Maps.Character_Mapping)
         return     Bounded_String;

      procedure Translate
        (Source   : in out Bounded_String;
         Mapping  : in Maps.Character_Mapping);

      function Translate
        (Source  : in Bounded_String;
         Mapping : in Maps.Character_Mapping_Function)
         return    Bounded_String;

      procedure Translate
        (Source  : in out Bounded_String;
         Mapping : in Maps.Character_Mapping_Function);

      ---------------------------------------
      -- String Transformation Subprograms --
      ---------------------------------------

      function Replace_Slice
        (Source   : in Bounded_String;
         Low      : in Positive;
         High     : in Natural;
         By       : in String;
         Drop     : in Truncation := Error)
         return     Bounded_String;

      procedure Replace_Slice
        (Source   : in out Bounded_String;
         Low      : in Positive;
         High     : in Natural;
         By       : in String;
         Drop     : in Truncation := Error);

      function Insert
        (Source   : in Bounded_String;
         Before   : in Positive;
         New_Item : in String;
         Drop     : in Truncation := Error)
         return     Bounded_String;

      procedure Insert
        (Source   : in out Bounded_String;
         Before   : in Positive;
         New_Item : in String;
         Drop     : in Truncation := Error);

      function Overwrite
        (Source    : in Bounded_String;
         Position  : in Positive;
         New_Item  : in String;
         Drop      : in Truncation := Error)
         return      Bounded_String;

      procedure Overwrite
        (Source    : in out Bounded_String;
         Position  : in Positive;
         New_Item  : in String;
         Drop      : in Truncation := Error);

      function Delete
        (Source  : in Bounded_String;
         From    : in Positive;
         Through : in Natural)
         return    Bounded_String;

      procedure Delete
        (Source  : in out Bounded_String;
         From    : in Positive;
         Through : in Natural);

      ---------------------------------
      -- String Selector Subprograms --
      ---------------------------------

      function Trim
        (Source : in Bounded_String;
         Side   : in Trim_End)
         return   Bounded_String;

      procedure Trim
        (Source : in out Bounded_String;
         Side   : in Trim_End);

      function Trim
        (Source  : in Bounded_String;
          Left   : in Maps.Character_Set;
          Right  : in Maps.Character_Set)
          return   Bounded_String;

      procedure Trim
        (Source : in out Bounded_String;
         Left   : in Maps.Character_Set;
         Right  : in Maps.Character_Set);

      function Head
        (Source : in Bounded_String;
         Count  : in Natural;
         Pad    : in Character := Space;
         Drop   : in Truncation := Error)
         return   Bounded_String;

      procedure Head
        (Source : in out Bounded_String;
         Count  : in Natural;
         Pad    : in Character  := Space;
         Drop   : in Truncation := Error);

      function Tail
        (Source : in Bounded_String;
         Count  : in Natural;
         Pad    : in Character  := Space;
         Drop   : in Truncation := Error)
         return Bounded_String;

      procedure Tail
        (Source : in out Bounded_String;
         Count  : in Natural;
         Pad    : in Character  := Space;
         Drop   : in Truncation := Error);

      ------------------------------------
      -- String Constructor Subprograms --
      ------------------------------------

      function "*"
        (Left  : in Natural;
         Right : in Character)
         return  Bounded_String;

      function "*"
        (Left  : in Natural;
         Right : in String)
         return  Bounded_String;

      function "*"
        (Left  : in Natural;
         Right : in Bounded_String)
         return  Bounded_String;

      function Replicate
        (Count : in Natural;
         Item  : in Character;
         Drop  : in Truncation := Error)
         return  Bounded_String;

      function Replicate
        (Count : in Natural;
         Item  : in String;
         Drop  : in Truncation := Error)
         return  Bounded_String;

      function Replicate
        (Count : in Natural;
         Item  : in Bounded_String;
         Drop  : in Truncation := Error)
         return  Bounded_String;

   private

      --  Most of the implementation is in the non generic package
      --  Ada.Strings.Superbounded. Type Bounded_String is derived from type
      --  Superbounded.Super_String with the maximum length constraint.
      --  Except for five, all subprograms are renames of subprograms that
      --  are inherited from Superbounded.Super_String.

      type Bounded_String is new Superbounded.Super_String (Max_Length);

      Null_Bounded_String : constant Bounded_String :=
        (Max_Length     => Max_Length,
         Current_Length => 0,
         Data           => (1 .. Max_Length => ASCII.NUL));

      pragma Inline (To_Bounded_String);

      function Length (Source : in Bounded_String) return Length_Range
        renames Super_Length;

      function To_String (Source : in Bounded_String) return String
        renames Super_To_String;

      function Append
        (Left, Right : in Bounded_String;
         Drop        : in Truncation  := Error)
         return        Bounded_String
        renames Super_Append;

      function Append
        (Left  : in Bounded_String;
         Right : in String;
         Drop  : in Truncation := Error)
         return  Bounded_String
        renames Super_Append;

      function Append
        (Left  : in String;
         Right : in Bounded_String;
         Drop  : in Truncation := Error)
         return  Bounded_String
        renames Super_Append;

      function Append
        (Left  : in Bounded_String;
         Right : in Character;
         Drop  : in Truncation := Error)
         return  Bounded_String
        renames Super_Append;

      function Append
        (Left  : in Character;
         Right : in Bounded_String;
         Drop  : in Truncation := Error)
         return  Bounded_String
        renames Super_Append;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : in Bounded_String;
         Drop     : in Truncation  := Error)
        renames Super_Append;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : in String;
         Drop     : in Truncation  := Error)
        renames Super_Append;

      procedure Append
        (Source   : in out Bounded_String;
         New_Item : in Character;
         Drop     : in Truncation  := Error)
        renames Super_Append;

      function "&"
        (Left, Right : in Bounded_String)
         return        Bounded_String
        renames Concat;

      function "&"
        (Left  : in Bounded_String;
         Right : in String)
         return  Bounded_String
        renames Concat;

      function "&"
        (Left  : in String;
         Right : in Bounded_String)
         return  Bounded_String
        renames Concat;

      function "&"
        (Left  : in Bounded_String;
         Right : in Character)
         return  Bounded_String
        renames Concat;

      function "&"
        (Left  : in Character;
         Right : in Bounded_String)
         return  Bounded_String
        renames Concat;

      function Element
        (Source : in Bounded_String;
         Index  : in Positive)
         return   Character
        renames Super_Element;

      procedure Replace_Element
        (Source : in out Bounded_String;
         Index  : in Positive;
         By     : in Character)
        renames Super_Replace_Element;

      function Slice
        (Source : in Bounded_String;
         Low    : in Positive;
         High   : in Natural)
         return   String
        renames Super_Slice;

      function "="  (Left, Right : in Bounded_String) return Boolean
        renames Equal;

      function "="
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean
        renames Equal;

      function "="
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean
        renames Equal;

      function "<"  (Left, Right : in Bounded_String) return Boolean
        renames Less;

      function "<"
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean
        renames Less;

      function "<"
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean
        renames Less;

      function "<=" (Left, Right : in Bounded_String) return Boolean
        renames Less_Or_Equal;

      function "<="
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean
        renames Less_Or_Equal;

      function "<="
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean
        renames Less_Or_Equal;

      function ">"  (Left, Right : in Bounded_String) return Boolean
        renames Greater;

      function ">"
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean
        renames Greater;

      function ">"
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean
        renames Greater;

      function ">=" (Left, Right : in Bounded_String) return Boolean
        renames Greater_Or_Equal;

      function ">="
        (Left  : in Bounded_String;
         Right : in String)
         return  Boolean
        renames Greater_Or_Equal;

      function ">="
        (Left  : in String;
         Right : in Bounded_String)
         return  Boolean
        renames Greater_Or_Equal;

      function Index
        (Source  : in Bounded_String;
         Pattern : in String;
         Going   : in Direction := Forward;
         Mapping : in Maps.Character_Mapping := Maps.Identity)
         return    Natural
        renames Super_Index;

      function Index
        (Source  : in Bounded_String;
         Pattern : in String;
         Going   : in Direction := Forward;
         Mapping : in Maps.Character_Mapping_Function)
         return    Natural
        renames Super_Index;

      function Index
        (Source : in Bounded_String;
         Set    : in Maps.Character_Set;
         Test   : in Membership := Inside;
         Going  : in Direction  := Forward)
         return   Natural
        renames Super_Index;

      function Index_Non_Blank
        (Source : in Bounded_String;
         Going  : in Direction := Forward)
         return   Natural
        renames Super_Index_Non_Blank;

      function Count
        (Source  : in Bounded_String;
         Pattern : in String;
         Mapping : in Maps.Character_Mapping := Maps.Identity)
         return    Natural
        renames Super_Count;

      function Count
        (Source  : in Bounded_String;
         Pattern : in String;
         Mapping : in Maps.Character_Mapping_Function)
         return    Natural
        renames Super_Count;

      function Count
        (Source : in Bounded_String;
         Set    : in Maps.Character_Set)
         return   Natural
        renames Super_Count;

      procedure Find_Token
        (Source : in Bounded_String;
         Set    : in Maps.Character_Set;
         Test   : in Membership;
         First  : out Positive;
         Last   : out Natural)
        renames Super_Find_Token;

      function Translate
        (Source   : in Bounded_String;
         Mapping  : in Maps.Character_Mapping)
         return     Bounded_String
        renames Super_Translate;

      procedure Translate
        (Source   : in out Bounded_String;
         Mapping  : in Maps.Character_Mapping)
        renames Super_Translate;

      function Translate
        (Source  : in Bounded_String;
         Mapping : in Maps.Character_Mapping_Function)
         return    Bounded_String
        renames Super_Translate;

      procedure Translate
        (Source  : in out Bounded_String;
         Mapping : in Maps.Character_Mapping_Function)
        renames Super_Translate;

      function Replace_Slice
        (Source   : in Bounded_String;
         Low      : in Positive;
         High     : in Natural;
         By       : in String;
         Drop     : in Truncation := Error)
         return     Bounded_String
        renames Super_Replace_Slice;

      procedure Replace_Slice
        (Source   : in out Bounded_String;
         Low      : in Positive;
         High     : in Natural;
         By       : in String;
         Drop     : in Truncation := Error)
        renames Super_Replace_Slice;

      function Insert
        (Source   : in Bounded_String;
         Before   : in Positive;
         New_Item : in String;
         Drop     : in Truncation := Error)
         return     Bounded_String
        renames Super_Insert;

      procedure Insert
        (Source   : in out Bounded_String;
         Before   : in Positive;
         New_Item : in String;
         Drop     : in Truncation := Error)
        renames Super_Insert;

      function Overwrite
        (Source    : in Bounded_String;
         Position  : in Positive;
         New_Item  : in String;
         Drop      : in Truncation := Error)
         return      Bounded_String
        renames Super_Overwrite;

      procedure Overwrite
        (Source    : in out Bounded_String;
         Position  : in Positive;
         New_Item  : in String;
         Drop      : in Truncation := Error)
        renames Super_Overwrite;

      function Delete
        (Source  : in Bounded_String;
         From    : in Positive;
         Through : in Natural)
         return    Bounded_String
        renames Super_Delete;

      procedure Delete
        (Source  : in out Bounded_String;
         From    : in Positive;
         Through : in Natural)
        renames Super_Delete;

      function Trim
        (Source : in Bounded_String;
         Side   : in Trim_End)
         return   Bounded_String
        renames Super_Trim;

      procedure Trim
        (Source : in out Bounded_String;
         Side   : in Trim_End)
        renames Super_Trim;

      function Trim
        (Source  : in Bounded_String;
          Left   : in Maps.Character_Set;
          Right  : in Maps.Character_Set)
          return   Bounded_String
        renames Super_Trim;

      procedure Trim
        (Source : in out Bounded_String;
         Left   : in Maps.Character_Set;
         Right  : in Maps.Character_Set)
        renames Super_Trim;

      function Head
        (Source : in Bounded_String;
         Count  : in Natural;
         Pad    : in Character := Space;
         Drop   : in Truncation := Error)
         return   Bounded_String
        renames Super_Head;

      procedure Head
        (Source : in out Bounded_String;
         Count  : in Natural;
         Pad    : in Character  := Space;
         Drop   : in Truncation := Error)
        renames Super_Head;

      function Tail
        (Source : in Bounded_String;
         Count  : in Natural;
         Pad    : in Character  := Space;
         Drop   : in Truncation := Error)
         return Bounded_String
        renames Super_Tail;

      procedure Tail
        (Source : in out Bounded_String;
         Count  : in Natural;
         Pad    : in Character  := Space;
         Drop   : in Truncation := Error)
        renames Super_Tail;

      function "*"
        (Left  : in Natural;
         Right : in Bounded_String)
         return  Bounded_String
        renames Times;

      function Replicate
        (Count : in Natural;
         Item  : in Bounded_String;
         Drop  : in Truncation := Error)
         return  Bounded_String
         renames Super_Replicate;

   end Generic_Bounded_Length;

end Ada.Strings.Bounded;
