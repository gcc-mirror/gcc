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

with Interfaces;      use Interfaces;
with System.Val_Util; use System.Val_Util;
with System.WCh_Con;  use System.WCh_Con;
with System.WCh_StW;  use System.WCh_StW;

package body System.Val_WChar is

   --------------------------
   -- Value_Wide_Character --
   --------------------------

   function Value_Wide_Character
      (Str : String;
       EM  : WC_Encoding_Method) return Wide_Character
   is
      WWC : constant Wide_Wide_Character :=
              Value_Wide_Wide_Character (Str, EM);
      WWV : constant Unsigned_32 := Wide_Wide_Character'Pos (WWC);
   begin
      if WWV > 16#FFFF# then
         raise Constraint_Error;
      else
         return Wide_Character'Val (WWV);
      end if;
   end Value_Wide_Character;

   -------------------------------
   -- Value_Wide_Wide_Character --
   -------------------------------

   function Value_Wide_Wide_Character
      (Str : String;
       EM  : System.WCh_Con.WC_Encoding_Method) return Wide_Wide_Character
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

         --  Otherwise must be a wide character in quotes. The easiest
         --  thing is to convert the string to a wide wide string and then
         --  pick up the single character that it should contain.

         else
            declare
               WS : constant Wide_Wide_String :=
                      String_To_Wide_Wide_String (S (F + 1 .. L - 1), EM);

            begin
               if WS'Length /= 1 then
                  raise Constraint_Error;
               else
                  return WS (WS'First);
               end if;
            end;
         end if;

      --  the last two values of the type have language-defined names:

      elsif S = "FFFE" then
         return Wide_Wide_Character'Val (16#FFFE#);

      elsif S = "FFFF" then
         return Wide_Wide_Character'Val (16#FFFF#);

      --  Otherwise must be a control character

      else
         for C in Character'Val (16#00#) .. Character'Val (16#1F#) loop
            if S (F .. L) = Character'Image (C) then
               return Wide_Wide_Character'Val (Character'Pos (C));
            end if;
         end loop;

         for C in Character'Val (16#7F#) .. Character'Val (16#9F#) loop
            if S (F .. L) = Character'Image (C) then
               return Wide_Wide_Character'Val (Character'Pos (C));
            end if;
         end loop;

         raise Constraint_Error;
      end if;
   end Value_Wide_Wide_Character;

end System.Val_WChar;
