------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      ADA.STRINGS.TEXT_BUFFERS.UTILS                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2020-2023, Free Software Foundation, Inc.       --
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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package Ada.Strings.Text_Buffers.Utils with Pure is

   --  Ada.Strings.Text_Buffers is a predefined unit (see Ada RM A.4.12).
   --  This is a GNAT-defined child unit of that parent.

   subtype Character_7 is
     Character range Character'Val (0) .. Character'Val (2**7 - 1);

   procedure Put_7bit
     (Buffer : in out Root_Buffer_Type'Class; Item : Character_7);
   procedure Put_Character
     (Buffer : in out Root_Buffer_Type'Class; Item : Character);
   procedure Put_Wide_Character
     (Buffer : in out Root_Buffer_Type'Class; Item : Wide_Character);
   procedure Put_Wide_Wide_Character
     (Buffer : in out Root_Buffer_Type'Class; Item : Wide_Wide_Character);
   --  Single character output procedures.

   function Column (Buffer : Root_Buffer_Type'Class) return Positive with
      Inline;
   --  Current output column. The Column is initially 1, and is incremented for
   --  each 8-bit character output. A call to New_Line sets Column back to 1.
   --  The next character to be output will go in this column.

   procedure Tab_To_Column
     (Buffer : in out Root_Buffer_Type'Class; Column : Positive);
   --  Put spaces until we're at or past Column.

   subtype Sink is Root_Buffer_Type;

   function NL return Character is (ASCII.LF) with Inline;

   function UTF_8_Length (Buffer : Root_Buffer_Type'Class) return Natural;

   subtype UTF_8_Lines is UTF_Encoding.UTF_8_String with
     Predicate =>
       UTF_Encoding.Wide_Wide_Strings.Encode
         (UTF_Encoding.Wide_Wide_Strings.Decode (UTF_8_Lines)) = UTF_8_Lines;

   subtype UTF_8 is UTF_8_Lines with
     Predicate => (for all UTF_8_Char of UTF_8 => UTF_8_Char /= NL);

   procedure Put_UTF_8_Lines
     (Buffer : in out Root_Buffer_Type'Class; Item : UTF_8_Lines);

private
   function UTF_8_Length (Buffer : Root_Buffer_Type'Class) return Natural
     is (Buffer.UTF_8_Length);
end Ada.Strings.Text_Buffers.Utils;
