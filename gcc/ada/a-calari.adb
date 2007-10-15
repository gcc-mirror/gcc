------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . A R I T H M E T I C               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2006-2007, Free Software Foundation, Inc.         --
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

package body Ada.Calendar.Arithmetic is

   --------------------------
   -- Implementation Notes --
   --------------------------

   --  All operations in this package are target and time representation
   --  independent, thus only one source file is needed for multiple targets.

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Day_Count) return Time is
      R : constant Long_Integer := Long_Integer (Right);
   begin
      return Arithmetic_Operations.Add (Left, R);
   end "+";

   function "+" (Left : Day_Count; Right : Time) return Time is
      L : constant Long_Integer := Long_Integer (Left);
   begin
      return Arithmetic_Operations.Add (Right, L);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Day_Count) return Time is
      R : constant Long_Integer := Long_Integer (Right);
   begin
      return Arithmetic_Operations.Subtract (Left, R);
   end "-";

   function "-" (Left, Right : Time) return Day_Count is
      Days         : Long_Integer;
      Seconds      : Duration;
      Leap_Seconds : Integer;
      pragma Warnings (Off, Seconds);        -- temporary ???
      pragma Warnings (Off, Leap_Seconds);   -- temporary ???
      pragma Unreferenced (Seconds, Leap_Seconds);
   begin
      Arithmetic_Operations.Difference
        (Left, Right, Days, Seconds, Leap_Seconds);
      return Day_Count (Days);
   end "-";

   ----------------
   -- Difference --
   ----------------

   procedure Difference
     (Left         : Time;
      Right        : Time;
      Days         : out Day_Count;
      Seconds      : out Duration;
      Leap_Seconds : out Leap_Seconds_Count)
   is
      Op_Days  : Long_Integer;
      Op_Leaps : Integer;
   begin
      Arithmetic_Operations.Difference
        (Left, Right, Op_Days, Seconds, Op_Leaps);
      Days := Day_Count (Op_Days);
      Leap_Seconds := Leap_Seconds_Count (Op_Leaps);
   end Difference;

end Ada.Calendar.Arithmetic;
