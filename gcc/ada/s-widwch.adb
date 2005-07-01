------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . W I D _ W C H A R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

package body System.Wid_WChar is

   --------------------------
   -- Width_Wide_Character --
   --------------------------

   function Width_Wide_Character
     (Lo, Hi : Wide_Character) return Natural
   is
      W : Natural;
      P : Natural;

   begin
      W := 0;
      for C in Lo .. Hi loop
         P := Wide_Character'Pos (C);

         --  Here if we find a character in wide character range
         --  Width is max value (12) for Hex_hhhhhhhh

         if P > 16#FF# then
            return 12;

            --  If we are in character range then use length of character image

         else
            declare
               S : constant String := Character'Image (Character'Val (P));
            begin
               W := Natural'Max (W, S'Length);
            end;
         end if;
      end loop;

      return W;
   end Width_Wide_Character;

   -------------------------------
   -- Width_Wide_Wide_Character --
   -------------------------------

   function Width_Wide_Wide_Character
     (Lo, Hi : Wide_Wide_Character) return Natural
   is
      W : Natural;
      P : Natural;

   begin
      W := 0;
      for C in Lo .. Hi loop
         P := Wide_Wide_Character'Pos (C);

         --  Here if we find a character in wide wide character range.
         --  Width is max value (12) for Hex_hhhhhhhh

         if P > 16#FF# then
            W := 12;

         --  If we are in character range then use length of character image

         else
            declare
               S : constant String := Character'Image (Character'Val (P));
            begin
               W := Natural'Max (W, S'Length);
            end;
         end if;
      end loop;

      return W;
   end Width_Wide_Wide_Character;

end System.Wid_WChar;
