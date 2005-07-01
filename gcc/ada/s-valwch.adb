------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . V A L _ W C H A R                      --
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

with Interfaces;      use Interfaces;
with System.Val_Util; use System.Val_Util;

package body System.Val_WChar is

   --------------------------
   -- Value_Wide_Character --
   --------------------------

   function Value_Wide_Character
      (Str : String) return Wide_Character
   is
      WWC : constant Wide_Wide_Character := Value_Wide_Wide_Character (Str);
      WWV : constant Unsigned_32         := Wide_Wide_Character'Pos (WWC);
   begin
      if WWV > 16#FFFF# then
         raise Constraint_Error
           with "out of range character for Value attribute";
      else
         return Wide_Character'Val (WWV);
      end if;
   end Value_Wide_Character;

   -------------------------------
   -- Value_Wide_Wide_Character --
   -------------------------------

   function Value_Wide_Wide_Character
      (Str : String) return Wide_Wide_Character
   is
      F : Natural;
      L : Natural;
      S : String (Str'Range) := Str;

   begin
      Normalize_String (S, F, L);

      --  Character literal case

      if S (F) = ''' and then S (L) = ''' then

         --  If just three characters, simple character case

         if L - F = 2 then
            return Wide_Wide_Character'Val (Character'Pos (S (F + 1)));

            --  Otherwise something is very wrong

         else
            raise Constraint_Error with "invalid string for Value attribute";
         end if;

      --  Deal with Hex_hhhhhhhh cases for wide_[wide_]character cases

      elsif Str'Length = 12 and then Str (1 .. 4) = "Hex_" then
         declare
            W : Unsigned_32 := 0;

         begin
            for J in 5 .. 12 loop
               W := W * 16 + Character'Pos (Str (J));

               if Str (J) in '0' .. '9' then
                  W := W - Character'Pos ('0');
               elsif Str (J) in 'A' .. 'F' then
                  W := W - Character'Pos ('A') + 10;
               elsif Str (J) in 'a' .. 'f' then
                  W := W - Character'Pos ('a') + 10;
               else
                  raise Constraint_Error
                    with "illegal hex character for Value attribute";
               end if;
            end loop;

            if W > 16#7FFF_FFFF# then
               raise Constraint_Error
                 with "out of range value for Value attribute";
            else
               return Wide_Wide_Character'Val (W);
            end if;
         end;

      --  Otherwise must be one of the special names for Character

      else
         return
           Wide_Wide_Character'Val (Character'Pos (Character'Value (Str)));
      end if;
   end Value_Wide_Wide_Character;

end System.Val_WChar;
