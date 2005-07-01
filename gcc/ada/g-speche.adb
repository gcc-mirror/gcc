------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . S P E L L I N G _ C H E C K E R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2005 AdaCore                      --
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

package body GNAT.Spelling_Checker is

   ------------------------
   -- Is_Bad_Spelling_Of --
   ------------------------

   function Is_Bad_Spelling_Of
     (Found  : String;
      Expect : String)
      return   Boolean
   is
      FN : constant Natural := Found'Length;
      FF : constant Natural := Found'First;
      FL : constant Natural := Found'Last;

      EN : constant Natural := Expect'Length;
      EF : constant Natural := Expect'First;
      EL : constant Natural := Expect'Last;

   begin
      --  If both strings null, then we consider this a match, but if one
      --  is null and the other is not, then we definitely do not match

      if FN = 0 then
         return (EN = 0);

      elsif EN = 0 then
         return False;

      --  If first character does not match, then definitely not misspelling

      elsif Found (FF) /= Expect (EF) then
         return False;

      --  Not a bad spelling if both strings are 1-2 characters long

      elsif FN < 3 and then EN < 3 then
         return False;

      --  Lengths match. Execute loop to check for a single error, single
      --  transposition or exact match (we only fall through this loop if
      --  one of these three conditions is found).

      elsif FN = EN then
         for J in 1 .. FN - 2 loop
            if Expect (EF + J) /= Found (FF + J) then

               --  If both mismatched characters are digits, then we do
               --  not consider it a misspelling (e.g. B345 is not a
               --  misspelling of B346, it is something quite different)

               if Expect (EF + J) in '0' .. '9'
                 and then Found (FF + J) in '0' .. '9'
               then
                  return False;

               elsif Expect (EF + J + 1) = Found (FF + J + 1)
                 and then Expect (EF + J + 2 .. EL) = Found (FF + J + 2 .. FL)
               then
                  return True;

               elsif Expect (EF + J) = Found (FF + J + 1)
                 and then Expect (EF + J + 1) = Found (FF + J)
                 and then Expect (EF + J + 2 .. EL) = Found (FF + J + 2 .. FL)
               then
                  return True;

               else
                  return False;
               end if;
            end if;
         end loop;

         --  At last character. Test digit case as above, otherwise we
         --  have a match since at most this last character fails to match.

         if Expect (EL) in '0' .. '9'
           and then Found (FL) in '0' .. '9'
           and then Expect (EL) /= Found (FL)
         then
            return False;
         else
            return True;
         end if;

      --  Length is 1 too short. Execute loop to check for single deletion

      elsif FN = EN - 1 then
         for J in 1 .. FN - 1 loop
            if Found (FF + J) /= Expect (EF + J) then
               return Found (FF + J .. FL) = Expect (EF + J + 1 .. EL);
            end if;
         end loop;

         --  If we fall through then the last character was missing, which
         --  we consider to be a match (e.g. found xyz, expected xyza).

         return True;

      --  Length is 1 too long. Execute loop to check for single insertion

      elsif FN = EN + 1 then
         for J in 1 .. EN - 1 loop
            if Found (FF + J) /= Expect (EF + J) then
               return Found (FF + J + 1 .. FL) = Expect (EF + J .. EL);
            end if;
         end loop;

         --  If we fall through then the last character was an additional
         --  character, which is a match (e.g. found xyza, expected xyz).

         return True;

      --  Length is completely wrong

      else
         return False;
      end if;

   end Is_Bad_Spelling_Of;

end GNAT.Spelling_Checker;
