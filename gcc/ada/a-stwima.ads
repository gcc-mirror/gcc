------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                A D A . S T R I N G S . W I D E _ M A P S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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

with Ada.Finalization;

package Ada.Strings.Wide_Maps is
   pragma Preelaborate (Wide_Maps);

   -------------------------------------
   -- Wide Character Set Declarations --
   -------------------------------------

   type Wide_Character_Set is private;
   --  Representation for a set of Wide_Character values:

   Null_Set : constant Wide_Character_Set;

   ------------------------------------------
   -- Constructors for Wide Character Sets --
   ------------------------------------------

   type Wide_Character_Range is record
      Low  : Wide_Character;
      High : Wide_Character;
   end record;
   --  Represents Wide_Character range Low .. High

   type Wide_Character_Ranges is
     array (Positive range <>) of Wide_Character_Range;

   function To_Set
     (Ranges : in Wide_Character_Ranges)
      return   Wide_Character_Set;

   function To_Set
     (Span : in Wide_Character_Range)
      return Wide_Character_Set;

   function To_Ranges
     (Set :  in Wide_Character_Set)
      return Wide_Character_Ranges;

   ---------------------------------------
   -- Operations on Wide Character Sets --
   ---------------------------------------

   function "=" (Left, Right : in Wide_Character_Set) return Boolean;

   function "not"
     (Right  : in Wide_Character_Set)
      return Wide_Character_Set;

   function "and"
     (Left, Right : in Wide_Character_Set)
      return        Wide_Character_Set;

   function "or"
     (Left, Right : in Wide_Character_Set)
      return        Wide_Character_Set;

   function "xor"
     (Left, Right : in Wide_Character_Set)
      return        Wide_Character_Set;

   function "-"
     (Left, Right : in Wide_Character_Set)
      return        Wide_Character_Set;

   function Is_In
     (Element : in Wide_Character;
      Set     : in Wide_Character_Set)
      return    Boolean;

   function Is_Subset
     (Elements : in Wide_Character_Set;
      Set      : in Wide_Character_Set)
      return     Boolean;

   function "<="
     (Left  : in Wide_Character_Set;
      Right : in Wide_Character_Set)
      return  Boolean
   renames Is_Subset;

   subtype Wide_Character_Sequence is Wide_String;
   --  Alternative representation for a set of character values

   function To_Set
     (Sequence  : in Wide_Character_Sequence)
      return      Wide_Character_Set;

   function To_Set
     (Singleton : in Wide_Character)
      return      Wide_Character_Set;

   function To_Sequence
     (Set  : in Wide_Character_Set)
      return Wide_Character_Sequence;

   -----------------------------------------
   -- Wide Character Mapping Declarations --
   -----------------------------------------

   type Wide_Character_Mapping is private;
   --  Representation for a wide character to wide character mapping:

   function Value
     (Map     : in Wide_Character_Mapping;
      Element : in Wide_Character)
      return    Wide_Character;

   Identity : constant Wide_Character_Mapping;

   ---------------------------------
   -- Operations on Wide Mappings --
   ---------------------------------

   function To_Mapping
     (From, To : in Wide_Character_Sequence)
      return     Wide_Character_Mapping;

   function To_Domain
     (Map  : in Wide_Character_Mapping)
      return Wide_Character_Sequence;

   function To_Range
     (Map  : in Wide_Character_Mapping)
      return Wide_Character_Sequence;

   type Wide_Character_Mapping_Function is
      access function (From : in Wide_Character) return Wide_Character;

private
   package AF renames Ada.Finalization;

   ------------------------------------------
   -- Representation of Wide_Character_Set --
   ------------------------------------------

   --  A wide character set is represented as a sequence of wide character
   --  ranges (i.e. an object of type Wide_Character_Ranges) in which the
   --  following hold:

   --    The lower bound is 1
   --    The ranges are in order by increasing Low values
   --    The ranges are non-overlapping and discontigous

   --  A character value is in the set if it is contained in one of the
   --  ranges. The actual Wide_Character_Set value is a controlled pointer
   --  to this Wide_Character_Ranges value. The use of a controlled type
   --  is necessary to prevent storage leaks.

   type Wide_Character_Ranges_Access is access all Wide_Character_Ranges;

   type Wide_Character_Set is new AF.Controlled with record
      Set : Wide_Character_Ranges_Access;
   end record;

   pragma Finalize_Storage_Only (Wide_Character_Set);
   --  This avoids useless finalizations, and, more importantly avoids
   --  incorrect attempts to finalize constants that are statically
   --  declared here and in Ada.Strings.Wide_Maps, which is incorrect.

   procedure Initialize (Object : in out Wide_Character_Set);
   procedure Adjust     (Object : in out Wide_Character_Set);
   procedure Finalize   (Object : in out Wide_Character_Set);

   Null_Range : aliased constant Wide_Character_Ranges :=
                  (1 .. 0 => (Low => ' ', High => ' '));

   Null_Set : constant Wide_Character_Set :=
                (AF.Controlled with
                 Set => Null_Range'Unrestricted_Access);

   ----------------------------------------------
   -- Representation of Wide_Character_Mapping --
   ----------------------------------------------

   --  A wide character mapping is represented as two strings of equal
   --  length, where any character appearing in Domain is mapped to the
   --  corresponding character in Rangev. A character not appearing in
   --  Domain is mapped to itself. The characters in Domain are sorted
   --  in ascending order.

   --  The actual Wide_Character_Mapping value is a controlled record
   --  that contains a pointer to a discriminated record containing the
   --  range and domain values.

   --  Note: this representation is canonical, and the values stored in
   --  Domain and Rangev are exactly the values that are returned by the
   --  functions To_Domain and To_Range. The use of a controlled type is
   --  necessary to prevent storage leaks.

   type Wide_Character_Mapping_Values (Length : Natural) is record
      Domain : Wide_Character_Sequence (1 .. Length);
      Rangev : Wide_Character_Sequence (1 .. Length);
   end record;

   type Wide_Character_Mapping_Values_Access is
     access all Wide_Character_Mapping_Values;

   type Wide_Character_Mapping is new AF.Controlled with record
      Map : Wide_Character_Mapping_Values_Access;
   end record;

   pragma Finalize_Storage_Only (Wide_Character_Mapping);
   --  This avoids useless finalizations, and, more importantly avoids
   --  incorrect attempts to finalize constants that are statically
   --  declared here and in Ada.Strings.Wide_Maps, which is incorrect.

   procedure Initialize (Object : in out Wide_Character_Mapping);
   procedure Adjust     (Object : in out Wide_Character_Mapping);
   procedure Finalize   (Object : in out Wide_Character_Mapping);

   Null_Map : aliased constant Wide_Character_Mapping_Values :=
                 (Length => 0,
                  Domain => "",
                  Rangev => "");

   Identity : constant Wide_Character_Mapping :=
                (AF.Controlled with
                 Map => Null_Map'Unrestricted_Access);

end Ada.Strings.Wide_Maps;
