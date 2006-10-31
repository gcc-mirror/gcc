------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C A L E N D A R . A R I T H M E T I C               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
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

with Unchecked_Conversion;

package body Ada.Calendar.Arithmetic is

   use Leap_Sec_Ops;

   Day_Duration : constant Duration := 86_400.0;

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Day_Count) return Time is
   begin
      return Left + Integer (Right) * Day_Duration;
   end "+";

   function "+" (Left : Day_Count; Right : Time) return Time is
   begin
      return Integer (Left) * Day_Duration + Right;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Day_Count) return Time is
   begin
      return Left - Integer (Right) * Day_Duration;
   end "-";

   function "-" (Left, Right : Time) return Day_Count is
      Days         : Day_Count;
      Seconds      : Duration;
      Leap_Seconds : Leap_Seconds_Count;

   begin
      Difference (Left, Right, Days, Seconds, Leap_Seconds);
      return Days;
   end "-";

   ----------------
   -- Difference --
   ----------------

   procedure Difference
     (Left, Right  : Time;
      Days         : out Day_Count;
      Seconds      : out Duration;
      Leap_Seconds : out Leap_Seconds_Count)
   is
      Diff        : Duration;
      Earlier     : Time;
      Later       : Time;
      Leaps_Dur   : Duration;
      Negate      : Boolean;
      Next_Leap   : Time;
      Secs_Diff   : Long_Integer;
      Sub_Seconds : Duration;

   begin
      if Left >= Right then
         Later   := Left;
         Earlier := Right;
         Negate  := False;
      else
         Later   := Right;
         Earlier := Left;
         Negate  := True;
      end if;

      Diff := Later - Earlier;

      Cumulative_Leap_Secs (Earlier, Later, Leaps_Dur, Next_Leap);

      if Later >= Next_Leap then
         Leaps_Dur := Leaps_Dur + 1.0;
      end if;

      Diff := Diff - Leaps_Dur;

      declare
         type D_Int is range 0 .. 2 ** (Duration'Size - 1) - 1;
         for D_Int'Size use Duration'Size;

         Small_Div : constant D_Int := D_Int (1.0 / Duration'Small);
         D_As_Int  : D_Int;

         function To_D_As_Int is new Unchecked_Conversion (Duration, D_Int);
         function To_Duration is new Unchecked_Conversion (D_Int, Duration);

      begin
         D_As_Int    := To_D_As_Int (Diff);
         Secs_Diff   := Long_Integer (D_As_Int / Small_Div);
         Sub_Seconds := To_Duration (D_As_Int rem Small_Div);
      end;

      Days    := Day_Count (Secs_Diff / 86_400);
      Seconds := Duration (Secs_Diff mod 86_400) + Sub_Seconds;
      Leap_Seconds := Leap_Seconds_Count (Leaps_Dur);

      if Negate then
         Days         := -Days;
         Seconds      := -Seconds;
         Leap_Seconds := -Leap_Seconds;
      end if;
   end Difference;

end Ada.Calendar.Arithmetic;
