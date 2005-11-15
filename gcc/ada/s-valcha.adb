------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ C H A R                       --
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

with System.Val_Util; use System.Val_Util;

package body System.Val_Char is

   ---------------------
   -- Value_Character --
   ---------------------

   function Value_Character (Str : String) return Character is
      F : Natural;
      L : Natural;
      S : String (Str'Range) := Str;

   begin
      Normalize_String (S, F, L);

      --  Accept any single character enclosed in quotes

      if L - F = 2 and then S (F) = ''' and then S (L) = ''' then
         return Character'Val (Character'Pos (S (F + 1)));

      --  Check control character cases

      else
         for C in Character'Val (16#00#) .. Character'Val (16#1F#) loop
            if S (F .. L) = Character'Image (C) then
               return C;
            end if;
         end loop;

         for C in Character'Val (16#7F#) .. Character'Val (16#9F#) loop
            if S (F .. L) = Character'Image (C) then
               return C;
            end if;
         end loop;

         raise Constraint_Error;
      end if;
   end Value_Character;

end System.Val_Char;
