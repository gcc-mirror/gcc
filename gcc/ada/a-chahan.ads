------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C H A R A C T E R S . H A N D L I N G               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
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

package Ada.Characters.Handling is
   pragma Pure;
   --  In accordance with Ada 2005 AI-362

   ----------------------------------------
   -- Character Classification Functions --
   ----------------------------------------

   function Is_Control               (Item : Character) return Boolean;
   function Is_Graphic               (Item : Character) return Boolean;
   function Is_Letter                (Item : Character) return Boolean;
   function Is_Lower                 (Item : Character) return Boolean;
   function Is_Upper                 (Item : Character) return Boolean;
   function Is_Basic                 (Item : Character) return Boolean;
   function Is_Digit                 (Item : Character) return Boolean;
   function Is_Decimal_Digit         (Item : Character) return Boolean
     renames Is_Digit;
   function Is_Hexadecimal_Digit     (Item : Character) return Boolean;
   function Is_Alphanumeric          (Item : Character) return Boolean;
   function Is_Special               (Item : Character) return Boolean;
   function Is_Line_Terminator       (Item : Character) return Boolean;
   function Is_Mark                  (Item : Character) return Boolean;
   function Is_Other_Format          (Item : Character) return Boolean;
   function Is_Punctuation_Connector (Item : Character) return Boolean;
   function Is_Space                 (Item : Character) return Boolean;

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
   --  and are considered obsolete in Ada.Characters.Handling. However we do
   --  not complain about this obsolescence, since in practice it is necessary
   --  to use these routines when creating code that is intended to run in
   --  either Ada 95 or Ada 2005 mode.

   --  We do however have to flag these if the pragma No_Obsolescent_Features
   --  restriction is active (see Restrict.Check_Obsolescent_2005_Entity).

   function Is_Character (Item : Wide_Character) return Boolean;
   function Is_String    (Item : Wide_String)    return Boolean;

   ------------------------------------------------------
   -- Conversions between Wide_Character and Character --
   ------------------------------------------------------

   --  Ada 2005 AI 395: these functions are moved to Ada.Characters.Conversions
   --  and are considered obsolete in Ada.Characters.Handling. However we do
   --  not complain about this obsolescence, since in practice it is necessary
   --  to use these routines when creating code that is intended to run in
   --  either Ada 95 or Ada 2005 mode.

   --  We do however have to flag these if the pragma No_Obsolescent_Features
   --  restriction is active (see Restrict.Check_Obsolescent_2005_Entity).

   function To_Character
     (Item       : Wide_Character;
      Substitute : Character := ' ') return Character;

   function To_String
     (Item       : Wide_String;
      Substitute : Character := ' ') return String;

   function To_Wide_Character
     (Item : Character) return Wide_Character;

   function To_Wide_String
     (Item : String) return Wide_String;

private
   pragma Inline (Is_Alphanumeric);
   pragma Inline (Is_Basic);
   pragma Inline (Is_Character);
   pragma Inline (Is_Control);
   pragma Inline (Is_Digit);
   pragma Inline (Is_Graphic);
   pragma Inline (Is_Hexadecimal_Digit);
   pragma Inline (Is_ISO_646);
   pragma Inline (Is_Letter);
   pragma Inline (Is_Line_Terminator);
   pragma Inline (Is_Lower);
   pragma Inline (Is_Mark);
   pragma Inline (Is_Other_Format);
   pragma Inline (Is_Punctuation_Connector);
   pragma Inline (Is_Space);
   pragma Inline (Is_Special);
   pragma Inline (Is_Upper);
   pragma Inline (To_Basic);
   pragma Inline (To_Character);
   pragma Inline (To_Lower);
   pragma Inline (To_Upper);
   pragma Inline (To_Wide_Character);

end Ada.Characters.Handling;
