------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--       A D A . S T R I N G S .  W I D E _ S U P E R B O U N D E D         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003-2005, Free Software Foundation, Inc.         --
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

--  This non generic package contains most of the implementation of the
--  generic package Ada.Strings.Wide_Bounded.Generic_Bounded_Length.

--  It defines type Super_String as a discriminated record with the maximum
--  length as the discriminant. Individual instantiations of the package
--  Strings.Wide_Bounded.Generic_Bounded_Length use this type with
--  an appropriate discriminant value set.

with Ada.Strings.Wide_Maps;

package Ada.Strings.Wide_Superbounded is
   pragma Preelaborate;

   Wide_NUL : constant Wide_Character := Wide_Character'Val (0);

   type Super_String (Max_Length : Positive) is record
      Current_Length : Natural := 0;
      Data           : Wide_String (1 .. Max_Length) := (others => Wide_NUL);
   end record;
   --  Ada.Strings.Wide_Bounded.Generic_Bounded_Length.Wide_Bounded_String is
   --  derived from this type, with the constraint of the maximum length.

   --  The subprograms defined for Super_String are similar to those defined
   --  for Bounded_Wide_String, except that they have different names, so that
   --  they can be renamed in Ada.Strings.Wide_Bounded.Generic_Bounded_Length.

   function Super_Length (Source : Super_String) return Natural;

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Super_String
     (Source     : Wide_String;
      Max_Length : Natural;
      Drop       : Truncation := Error) return Super_String;
   --  Note the additional parameter Max_Length, which specifies the maximum
   --  length setting of the resulting Super_String value.

   --  The following procedures have declarations (and semantics) that are
   --  exactly analogous to those declared in Ada.Strings.Wide_Bounded.

   function Super_To_String (Source : Super_String) return Wide_String;

   procedure Set_Super_String
     (Target : out Super_String;
      Source : Wide_String;
      Drop   : Truncation := Error);

   function Super_Append
     (Left  : Super_String;
      Right : Super_String;
      Drop  : Truncation := Error) return Super_String;

   function Super_Append
     (Left  : Super_String;
      Right : Wide_String;
      Drop  : Truncation := Error) return Super_String;

   function Super_Append
     (Left  : Wide_String;
      Right : Super_String;
      Drop  : Truncation := Error) return Super_String;

   function Super_Append
     (Left  : Super_String;
      Right : Wide_Character;
      Drop  : Truncation := Error) return Super_String;

   function Super_Append
     (Left  : Wide_Character;
      Right : Super_String;
      Drop  : Truncation := Error) return Super_String;

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : Super_String;
      Drop     : Truncation := Error);

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : Wide_String;
      Drop     : Truncation := Error);

   procedure Super_Append
     (Source   : in out Super_String;
      New_Item : Wide_Character;
      Drop     : Truncation := Error);

   function Concat
     (Left  : Super_String;
      Right : Super_String) return Super_String;

   function Concat
     (Left  : Super_String;
      Right : Wide_String) return Super_String;

   function Concat
     (Left  : Wide_String;
      Right : Super_String) return Super_String;

   function Concat
     (Left  : Super_String;
      Right : Wide_Character) return Super_String;

   function Concat
     (Left  : Wide_Character;
      Right : Super_String) return Super_String;

   function Super_Element
     (Source : Super_String;
      Index  : Positive) return Wide_Character;

   procedure Super_Replace_Element
     (Source : in out Super_String;
      Index  : Positive;
      By     : Wide_Character);

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return Wide_String;

   function Super_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural) return Super_String;

   procedure Super_Slice
     (Source : Super_String;
      Target : out Super_String;
      Low    : Positive;
      High   : Natural);

   function "="
     (Left  : Super_String;
      Right : Super_String) return Boolean;

   function Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean renames "=";

   function Equal
     (Left  : Super_String;
      Right : Wide_String) return Boolean;

   function Equal
     (Left  : Wide_String;
      Right : Super_String) return Boolean;

   function Less
     (Left  : Super_String;
      Right : Super_String) return Boolean;

   function Less
     (Left  : Super_String;
      Right : Wide_String) return Boolean;

   function Less
     (Left  : Wide_String;
      Right : Super_String) return Boolean;

   function Less_Or_Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean;

   function Less_Or_Equal
     (Left  : Super_String;
      Right : Wide_String) return Boolean;

   function Less_Or_Equal
     (Left  : Wide_String;
      Right : Super_String) return Boolean;

   function Greater
     (Left  : Super_String;
      Right : Super_String) return Boolean;

   function Greater
     (Left  : Super_String;
      Right : Wide_String) return Boolean;

   function Greater
     (Left  : Wide_String;
      Right : Super_String) return Boolean;

   function Greater_Or_Equal
     (Left  : Super_String;
      Right : Super_String) return Boolean;

   function Greater_Or_Equal
     (Left  : Super_String;
      Right : Wide_String) return Boolean;

   function Greater_Or_Equal
     (Left  : Wide_String;
      Right : Super_String) return Boolean;

   ----------------------
   -- Search Functions --
   ----------------------

   function Super_Index
     (Source  : Super_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;

   function Super_Index
     (Source  : Super_String;
      Pattern : Wide_String;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;

   function Super_Index
     (Source : Super_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;

   function Super_Index
     (Source  : Super_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;

   function Super_Index
     (Source  : Super_String;
      Pattern : Wide_String;
      From    : Positive;
      Going   : Direction := Forward;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;

   function Super_Index
     (Source : Super_String;
      Set    : Wide_Maps.Wide_Character_Set;
      From   : Positive;
      Test   : Membership := Inside;
      Going  : Direction := Forward) return Natural;

   function Super_Index_Non_Blank
     (Source : Super_String;
      Going  : Direction := Forward) return Natural;

   function Super_Index_Non_Blank
     (Source : Super_String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;

   function Super_Count
     (Source  : Super_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return Natural;

   function Super_Count
     (Source  : Super_String;
      Pattern : Wide_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Natural;

   function Super_Count
     (Source : Super_String;
      Set    : Wide_Maps.Wide_Character_Set) return Natural;

   procedure Super_Find_Token
     (Source : Super_String;
      Set    : Wide_Maps.Wide_Character_Set;
      Test   : Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Super_Translate
     (Source  : Super_String;
      Mapping : Wide_Maps.Wide_Character_Mapping) return Super_String;

   procedure Super_Translate
     (Source   : in out Super_String;
      Mapping  : Wide_Maps.Wide_Character_Mapping);

   function Super_Translate
     (Source  : Super_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function) return Super_String;

   procedure Super_Translate
     (Source  : in out Super_String;
      Mapping : Wide_Maps.Wide_Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Super_Replace_Slice
     (Source : Super_String;
      Low    : Positive;
      High   : Natural;
      By     : Wide_String;
      Drop   : Truncation := Error) return Super_String;

   procedure Super_Replace_Slice
     (Source  : in out Super_String;
      Low     : Positive;
      High    : Natural;
      By      : Wide_String;
      Drop    : Truncation := Error);

   function Super_Insert
     (Source   : Super_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error) return Super_String;

   procedure Super_Insert
     (Source   : in out Super_String;
      Before   : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error);

   function Super_Overwrite
     (Source   : Super_String;
      Position : Positive;
      New_Item : Wide_String;
      Drop     : Truncation := Error) return Super_String;

   procedure Super_Overwrite
     (Source    : in out Super_String;
      Position  : Positive;
      New_Item  : Wide_String;
      Drop      : Truncation := Error);

   function Super_Delete
     (Source  : Super_String;
      From    : Positive;
      Through : Natural) return Super_String;

   procedure Super_Delete
     (Source  : in out Super_String;
      From    : Positive;
      Through : Natural);

   ---------------------------------
   -- String Selector Subprograms --
   ---------------------------------

   function Super_Trim
     (Source : Super_String;
      Side   : Trim_End) return Super_String;

   procedure Super_Trim
     (Source : in out Super_String;
      Side   : Trim_End);

   function Super_Trim
     (Source : Super_String;
      Left   : Wide_Maps.Wide_Character_Set;
      Right  : Wide_Maps.Wide_Character_Set) return Super_String;

   procedure Super_Trim
     (Source : in out Super_String;
      Left   : Wide_Maps.Wide_Character_Set;
      Right  : Wide_Maps.Wide_Character_Set);

   function Super_Head
     (Source : Super_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error) return Super_String;

   procedure Super_Head
     (Source : in out Super_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error);

   function Super_Tail
     (Source : Super_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error) return Super_String;

   procedure Super_Tail
     (Source : in out Super_String;
      Count  : Natural;
      Pad    : Wide_Character := Wide_Space;
      Drop   : Truncation := Error);

   ------------------------------------
   -- String Constructor Subprograms --
   ------------------------------------

   --  Note: in some of the following routines, there is an extra parameter
   --  Max_Length which specifies the value of the maximum length for the
   --  resulting Super_String value.

   function Times
     (Left       : Natural;
      Right      : Wide_Character;
      Max_Length : Positive) return Super_String;
   --  Note the additional parameter Max_Length

   function Times
     (Left       : Natural;
      Right      : Wide_String;
      Max_Length : Positive) return Super_String;
   --  Note the additional parameter Max_Length

   function Times
     (Left  : Natural;
      Right : Super_String) return Super_String;

   function Super_Replicate
     (Count      : Natural;
      Item       : Wide_Character;
      Drop       : Truncation := Error;
      Max_Length : Positive) return Super_String;
   --  Note the additional parameter Max_Length

   function Super_Replicate
     (Count      : Natural;
      Item       : Wide_String;
      Drop       : Truncation := Error;
      Max_Length : Positive) return Super_String;
   --  Note the additional parameter Max_Length

   function Super_Replicate
     (Count : Natural;
      Item  : Super_String;
      Drop  : Truncation := Error) return Super_String;

private
      --  Pragma Inline declarations

      pragma Inline ("=");
      pragma Inline (Less);
      pragma Inline (Less_Or_Equal);
      pragma Inline (Greater);
      pragma Inline (Greater_Or_Equal);
      pragma Inline (Concat);
      pragma Inline (Super_Count);
      pragma Inline (Super_Element);
      pragma Inline (Super_Find_Token);
      pragma Inline (Super_Index);
      pragma Inline (Super_Index_Non_Blank);
      pragma Inline (Super_Length);
      pragma Inline (Super_Replace_Element);
      pragma Inline (Super_Slice);
      pragma Inline (Super_To_String);

end Ada.Strings.Wide_Superbounded;
