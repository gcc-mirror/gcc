------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     S Y S T E M . W W D _ W C H A R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2000, Free Software Foundation, Inc.         --
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

package body System.Wwd_WChar is

   -------------------------------
   -- Wide_Width_Wide_Character --
   -------------------------------

   function Wide_Width_Wide_Character
     (Lo, Hi : Wide_Character)
      return   Natural
   is
      W : Natural;
      P : Natural;

   begin
      W := 0;

      for C in Lo .. Hi loop
         P := Wide_Character'Pos (C);

         --  If we are in wide character range, the length is always 3
         --  and we are done, since all remaining characters are the same.

         if P > 255 then
            return Natural'Max (W, 3);

         --  If we are in character range then use length of character image
         --  Is this right, what about wide char encodings of 80-FF???

         else
            declare
               S : Wide_String := Character'Wide_Image (Character'Val (P));

            begin
               W := Natural'Max (W, S'Length);
            end;
         end if;
      end loop;

      return W;
   end Wide_Width_Wide_Character;

end System.Wwd_WChar;
