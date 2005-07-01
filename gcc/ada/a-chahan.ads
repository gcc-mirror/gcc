------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C H A R A C T E R S . H A N D L I N G               --
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

package Ada.Characters.Handling is
pragma Preelaborate (Handling);
pragma Pure_05 (Handling);
--  In accordance with Ada 2005 AI-362

   ----------------------------------------
   -- Character Classification Functions --
   ----------------------------------------

   function Is_Control           (Item : Character) return Boolean;
   function Is_Graphic           (Item : Character) return Boolean;
   function Is_Letter            (Item : Character) return Boolean;
   function Is_Lower             (Item : Character) return Boolean;
   function Is_Upper             (Item : Character) return Boolean;
   function Is_Basic             (Item : Character) return Boolean;
   function Is_Digit             (Item : Character) return Boolean;
   function Is_Decimal_Digit     (Item : Character) return Boolean
     renames Is_Digit;
   function Is_Hexadecimal_Digit (Item : Character) return Boolean;
   function Is_Alphanumeric      (Item : Character) return Boolean;
   function Is_Special           (Item : Character) return Boolean;

   ---------------------------------------------------
   -- Conversion Functions for Character and String --
   ---------------------------------------------------

   function To_Lower (Item : Character) return Character;
   function To_Upper (Item : Character) return Character;
   function To_Basic (Item : Character) return Character;

   function To_Lower (Item : String) return String;
   function To_Upper (Item : String) return String;
   function To_Basic (Item : String) return String;

   ----------------------------------------------------------------------
   -- Classifications of and Conversions Between Character and ISO 646 --
   ----------------------------------------------------------------------

   subtype ISO_646 is
     Character range Character'Val (0) .. Character'Val (127);

   function Is_ISO_646 (Item : Character) return Boolean;
   function Is_ISO_646 (Item : String)    return Boolean;

   function To_ISO_646
     (Item       : Character;
      Substitute : ISO_646 := ' ') return ISO_646;

   function To_ISO_646
     (Item       : String;
      Substitute : ISO_646 := ' ') return String;

   ------------------------------------------------------
   -- Classifications of Wide_Character and Characters --
   ------------------------------------------------------

   --  Ada 2005 AI 395: these functions are moved to Ada.Characters.Conversions
   --  and are considered obsolete in Ada.Characters.Handling. We deal with
   --  this by using the special Ada_05 form of pragma Obsolescent which is
   --  only active in Ada_05 mode.

   function Is_Character (Item : Wide_Character) return Boolean;
   pragma Obsolescent
     ("(Ada 2005) use Ada.Characters.Conversions.Is_Character", Ada_05);

   function Is_String (Item : Wide_String) return Boolean;
   pragma Obsolescent
     ("(Ada 2005) use Ada.Characters.Conversions.Is_String", Ada_05);

   ------------------------------------------------------
   -- Conversions between Wide_Character and Character --
   ------------------------------------------------------

   --  Ada 2005 AI 395: these functions are moved to Ada.Characters.Conversions
   --  and are considered obsolete in Ada.Characters.Handling. We deal with
   --  this by using the special Ada_05 form of pragma Obsolescent which is
   --  only active in Ada_05 mode.

   function To_Character
     (Item       : Wide_Character;
      Substitute : Character := ' ') return Character;
   pragma Obsolescent
     ("(Ada 2005) use Ada.Characters.Conversions.To_Character", Ada_05);

   function To_String
     (Item       : Wide_String;
      Substitute : Character := ' ') return String;
   pragma Obsolescent
     ("(Ada 2005) use Ada.Characters.Conversions.To_String", Ada_05);

   function To_Wide_Character
     (Item : Character) return Wide_Character;
   pragma Obsolescent
     ("(Ada 2005) use Ada.Characters.Conversions.To_Wide_Character", Ada_05);

   function To_Wide_String
     (Item : String)return Wide_String;
   pragma Obsolescent
     ("(Ada 2005) use Ada.Characters.Conversions.To_Wide_String", Ada_05);

private
   pragma Inline (Is_Control);
   pragma Inline (Is_Graphic);
   pragma Inline (Is_Letter);
   pragma Inline (Is_Lower);
   pragma Inline (Is_Upper);
   pragma Inline (Is_Basic);
   pragma Inline (Is_Digit);
   pragma Inline (Is_Hexadecimal_Digit);
   pragma Inline (Is_Alphanumeric);
   pragma Inline (Is_Special);
   pragma Inline (To_Lower);
   pragma Inline (To_Upper);
   pragma Inline (To_Basic);
   pragma Inline (Is_ISO_646);
   pragma Inline (Is_Character);
   pragma Inline (To_Character);
   pragma Inline (To_Wide_Character);

end Ada.Characters.Handling;
