------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C H A R A C T E R S . H A N D L I N G               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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


package Ada.Characters.Handling is
pragma Preelaborate (Handling);

   ----------------------------------------
   -- Character Classification Functions --
   ----------------------------------------

   function Is_Control           (Item : in Character) return Boolean;
   function Is_Graphic           (Item : in Character) return Boolean;
   function Is_Letter            (Item : in Character) return Boolean;
   function Is_Lower             (Item : in Character) return Boolean;
   function Is_Upper             (Item : in Character) return Boolean;
   function Is_Basic             (Item : in Character) return Boolean;
   function Is_Digit             (Item : in Character) return Boolean;
   function Is_Decimal_Digit     (Item : in Character) return Boolean
                                                          renames Is_Digit;
   function Is_Hexadecimal_Digit (Item : in Character) return Boolean;
   function Is_Alphanumeric      (Item : in Character) return Boolean;
   function Is_Special           (Item : in Character) return Boolean;

   ---------------------------------------------------
   -- Conversion Functions for Character and String --
   ---------------------------------------------------

   function To_Lower (Item : in Character) return Character;
   function To_Upper (Item : in Character) return Character;
   function To_Basic (Item : in Character) return Character;

   function To_Lower (Item : in String) return String;
   function To_Upper (Item : in String) return String;
   function To_Basic (Item : in String) return String;

   ----------------------------------------------------------------------
   -- Classifications of and Conversions Between Character and ISO 646 --
   ----------------------------------------------------------------------

   subtype ISO_646 is
     Character range Character'Val (0) .. Character'Val (127);

   function Is_ISO_646 (Item : in Character) return Boolean;
   function Is_ISO_646 (Item : in String)    return Boolean;

   function To_ISO_646
     (Item       : in Character;
      Substitute : in ISO_646 := ' ')
      return       ISO_646;

   function To_ISO_646
     (Item      : in String;
      Substitute : in ISO_646 := ' ')
      return       String;

   ------------------------------------------------------
   -- Classifications of Wide_Character and Characters --
   ------------------------------------------------------

   function Is_Character (Item : in Wide_Character) return Boolean;
   function Is_String    (Item : in Wide_String)    return Boolean;

   ------------------------------------------------------
   -- Conversions between Wide_Character and Character --
   ------------------------------------------------------

   function To_Character
     (Item       : in Wide_Character;
      Substitute : in Character := ' ')
      return       Character;

   function To_String
     (Item       : in Wide_String;
      Substitute : in Character := ' ')
      return       String;

   function To_Wide_Character (Item : in Character) return Wide_Character;
   function To_Wide_String    (Item : in String)    return Wide_String;

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
