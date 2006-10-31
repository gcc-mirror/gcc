------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . A R I T H M E T I C               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--        Copyright (C) 2005 - 2006, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

package Ada.Calendar.Arithmetic is

   --  Arithmetic on days:

   type Day_Count is range
     -(366 * (1 + Year_Number'Last - Year_Number'First))
        ..
     +(366 * (1 + Year_Number'Last - Year_Number'First));

   subtype Leap_Seconds_Count is Integer range -2047 .. 2047;

   procedure Difference
     (Left, Right  : Time;
      Days         : out Day_Count;
      Seconds      : out Duration;
      Leap_Seconds : out Leap_Seconds_Count);

   function "+" (Left : Time; Right : Day_Count) return Time;
   function "+" (Left : Day_Count; Right : Time) return Time;
   function "-" (Left : Time; Right : Day_Count) return Time;
   function "-" (Left, Right : Time) return Day_Count;

end Ada.Calendar.Arithmetic;
