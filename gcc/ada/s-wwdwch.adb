------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . W W D _ W C H A R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

with Interfaces; use Interfaces;

with System.WWd_Char;

package body System.Wwd_WChar is

   ------------------------------------
   -- Wide_Wide_Width_Wide_Character --
   ------------------------------------

   --  This is the case where we are talking about the Wide_Wide_Image of
   --  a Wide_Character, which is always the same character sequence as the
   --  Wide_Image of the same Wide_Character.

   function Wide_Wide_Width_Wide_Character
     (Lo, Hi : Wide_Character) return Natural
   is
   begin
      return Wide_Width_Wide_Character (Lo, Hi);
   end Wide_Wide_Width_Wide_Character;

   ------------------------------------
   -- Wide_Wide_Width_Wide_Wide_Char --
   ------------------------------------

   function Wide_Wide_Width_Wide_Wide_Char
     (Lo, Hi : Wide_Wide_Character) return Natural
   is
      LV : constant Unsigned_32 := Wide_Wide_Character'Pos (Lo);
      HV : constant Unsigned_32 := Wide_Wide_Character'Pos (Hi);

   begin
      --  Return zero if empty range

      if LV > HV then
         return 0;

      --  Return max value (12) for wide character (Hex_hhhhhhhh)

      elsif HV > 255 then
         return 12;

      --  If any characters in normal character range, then use normal
      --  Wide_Wide_Width attribute on this range to find out a starting point.
      --  Otherwise start with zero.

      else
         return
           System.WWd_Char.Wide_Wide_Width_Character
             (Lo => Character'Val (LV),
              Hi => Character'Val (Unsigned_32'Min (255, HV)));
      end if;
   end Wide_Wide_Width_Wide_Wide_Char;

   -------------------------------
   -- Wide_Width_Wide_Character --
   -------------------------------

   function Wide_Width_Wide_Character
     (Lo, Hi : Wide_Character) return Natural
   is
      LV : constant Unsigned_32 := Wide_Character'Pos (Lo);
      HV : constant Unsigned_32 := Wide_Character'Pos (Hi);

   begin
      --  Return zero if empty range

      if LV > HV then
         return 0;

      --  Return max value (12) for wide character (Hex_hhhhhhhh)

      elsif HV > 255 then
         return 12;

      --  If any characters in normal character range, then use normal
      --  Wide_Wide_Width attribute on this range to find out a starting point.
      --  Otherwise start with zero.

      else
         return
           System.WWd_Char.Wide_Width_Character
             (Lo => Character'Val (LV),
              Hi => Character'Val (Unsigned_32'Min (255, HV)));
      end if;
   end Wide_Width_Wide_Character;

   ------------------------------------
   -- Wide_Width_Wide_Wide_Character --
   ------------------------------------

   function Wide_Width_Wide_Wide_Character
     (Lo, Hi : Wide_Wide_Character) return Natural
   is
   begin
      return Wide_Wide_Width_Wide_Wide_Char (Lo, Hi);
   end Wide_Width_Wide_Wide_Character;

end System.Wwd_WChar;
