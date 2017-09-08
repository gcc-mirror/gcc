------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . W I D _ W C H A R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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
