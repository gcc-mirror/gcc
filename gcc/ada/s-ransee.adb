------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . R A N D O M _ S E E D                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2012, Free Software Foundation, Inc.         --
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

--  Version used on all systems except Ravenscar where Calendar is unavailable

with Ada.Calendar; use Ada.Calendar;
with Ada.Unchecked_Conversion;

package body System.Random_Seed is

   Y2K : constant Time :=
           Time_Of (Year => 2000, Month => 1, Day => 1, Seconds => 0.0);
   --  First day of Year 2000, to get a duration

   function To_U64 is
     new Ada.Unchecked_Conversion (Duration, Interfaces.Unsigned_64);

   --------------
   -- Get_Seed --
   --------------

   function Get_Seed return Interfaces.Unsigned_64 is
   begin
      return To_U64 (Clock - Y2K);
   end Get_Seed;

end System.Random_Seed;
