------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     S Y S T E M . W W D _ W C H A R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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
      W  : Natural := 0;
      LV : constant Unsigned_32 := Wide_Wide_Character'Pos (Lo);
      HV : constant Unsigned_32 := Wide_Wide_Character'Pos (Hi);

   begin
      --  Return zero if empty range

      if LV > HV then
         return 0;
      end if;

      --  If any characters in normal character range, then use normal
      --  Wide_Wide_Width attribute on this range to find out a starting point.
      --  Otherwise start with zero.

      if LV <= 255 then
         W :=
           System.WWd_Char.Wide_Wide_Width_Character
             (Lo => Character'Val (LV),
              Hi => Character'Val (Unsigned_32'Min (255, HV)));
      else
         W := 0;
      end if;

      --  Increase to at least 4 if FFFE or FFFF present. These correspond
      --  to the special language defined names FFFE/FFFF for these values.

      if 16#FFFF# in LV .. HV or else 16#FFFE# in LV .. HV then
         W := Natural'Max (W, 4);
      end if;

      --  Increase to at least 3 if any wide characters, corresponding to
      --  the normal ' character ' sequence. We know that the character fits.

      if HV > 255 then
         W := Natural'Max (W, 3);
      end if;

      return W;
   end Wide_Wide_Width_Wide_Wide_Char;

   -------------------------------
   -- Wide_Width_Wide_Character --
   -------------------------------

   function Wide_Width_Wide_Character
     (Lo, Hi : Wide_Character) return Natural
   is
      W  : Natural := 0;
      LV : constant Unsigned_32 := Wide_Character'Pos (Lo);
      HV : constant Unsigned_32 := Wide_Character'Pos (Hi);

   begin
      --  Return zero if empty range

      if LV > HV then
         return 0;
      end if;

      --  If any characters in normal character range, then use normal
      --  Wide_Wide_Width attribute on this range to find out a starting point.
      --  Otherwise start with zero.

      if LV <= 255 then
         W :=
           System.WWd_Char.Wide_Width_Character
             (Lo => Character'Val (LV),
              Hi => Character'Val (Unsigned_32'Min (255, HV)));
      else
         W := 0;
      end if;

      --  Increase to at least 4 if FFFE or FFFF present. These correspond
      --  to the special language defined names FFFE/FFFF for these values.

      if 16#FFFF# in LV .. HV or else 16#FFFE# in LV .. HV then
         W := Natural'Max (W, 4);
      end if;

      --  Increase to at least 3 if any wide characters, corresponding to
      --  the normal 'character' sequence. We know that the character fits.

      if HV > 255 then
         W := Natural'Max (W, 3);
      end if;

      return W;
   end Wide_Width_Wide_Character;

   ------------------------------------
   -- Wide_Width_Wide_Wide_Character --
   ------------------------------------

   --  This is a nasty case, because we get into the business of representing
   --  out of range wide wide characters as wide strings. Let's let image do
   --  the work here. Too bad if this takes lots of time. It's silly anyway!

   function Wide_Width_Wide_Wide_Character
     (Lo, Hi : Wide_Wide_Character) return Natural
   is
      W : Natural;

   begin
      W := 0;
      for J in Lo .. Hi loop
         declare
            S : constant Wide_String := Wide_Wide_Character'Wide_Image (J);
         begin
            W := Natural'Max (W, S'Length);
         end;
      end loop;

      return W;
   end Wide_Width_Wide_Wide_Character;

end System.Wwd_WChar;
