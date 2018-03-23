------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1991-2017, Florida State University            --
--                     Copyright (C) 1995-2018, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

with System.Tasking;
with Unchecked_Conversion;

package body Ada.Real_Time with
  SPARK_Mode => Off
is

   ---------
   -- "*" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (Duration (Left) * Right);
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (Left * Duration (Right));
   end "*";

   ---------
   -- "+" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "+" (Left : Time; Right : Time_Span) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time (Duration (Left) + Duration (Right));
   end "+";

   function "+" (Left : Time_Span; Right : Time) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time (Duration (Left) + Duration (Right));
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (Duration (Left) + Duration (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "-" (Left : Time; Right : Time_Span) return Time is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Left, Right : Time) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
      pragma Unsuppress (Overflow_Check);
   begin
      return Time_Span_Zero - Right;
   end "-";

   ---------
   -- "/" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "/" (Left, Right : Time_Span) return Integer is
      pragma Unsuppress (Overflow_Check);
      pragma Unsuppress (Division_Check);

      --  RM D.8 (27) specifies the effects of operators on Time_Span, and
      --  rounding of the division operator in particular, to be the same as
      --  effects on integer types. To get the correct rounding we first
      --  convert Time_Span to its root type Duration, which is represented as
      --  a 64-bit signed integer, and then use integer division.

      type Duration_Rep is range -(2 ** 63) .. +((2 ** 63 - 1));

      function To_Integer is
        new Unchecked_Conversion (Duration, Duration_Rep);
   begin
      return Integer
               (To_Integer (Duration (Left)) / To_Integer (Duration (Right)));
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
      pragma Unsuppress (Overflow_Check);
      pragma Unsuppress (Division_Check);
   begin
      --  Even though checks are unsuppressed, we need an explicit check for
      --  the case of largest negative integer divided by minus one, since
      --  some library routines we use fail to catch this case. This will be
      --  fixed at the compiler level in the future, at which point this test
      --  can be removed.

      if Left = Time_Span_First and then Right = -1 then
         raise Constraint_Error with "overflow";
      end if;

      return Time_Span (Duration (Left) / Right);
   end "/";

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (System.Task_Primitives.Operations.Monotonic_Clock);
   end Clock;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      return Time_Span_Unit * US * 1_000;
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return Time_Span_Unit * MS * 1_000_000;
   end Milliseconds;

   -------------
   -- Minutes --
   -------------

   function Minutes (M : Integer) return Time_Span is
   begin
      return Milliseconds (M) * Integer'(60_000);
   end Minutes;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return Time_Span_Unit * NS;
   end Nanoseconds;

   -------------
   -- Seconds --
   -------------

   function Seconds (S : Integer) return Time_Span is
   begin
      return Milliseconds (S) * Integer'(1000);
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
      T_Val : Time;

   begin
      --  Special-case for Time_First, whose absolute value is anomalous,
      --  courtesy of two's complement.

      T_Val := (if T = Time_First then abs (Time_Last) else abs (T));

      --  Extract the integer part of T, truncating towards zero

      SC :=
        (if T_Val < 0.5 then 0 else Seconds_Count (Time_Span'(T_Val - 0.5)));

      if T < 0.0 then
         SC := -SC;
      end if;

      --  If original time is negative, need to truncate towards negative
      --  infinity, to make TS non-negative, as per ARM.

      if Time (SC) > T then
         SC := SC - 1;
      end if;

      TS := Time_Span (Duration (T) - Duration (SC));
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
      pragma Suppress (Overflow_Check);
      pragma Suppress (Range_Check);
      --  We do all our own checks for this function

      --  This is not such a simple case, since TS is already 64 bits, and
      --  so we can't just promote everything to a wider type to ensure proper
      --  testing for overflow. The situation is that Seconds_Count is a MUCH
      --  wider type than Time_Span and Time (both of which have the underlying
      --  type Duration).

      --         <------------------- Seconds_Count -------------------->
      --                            <-- Duration -->

      --  Now it is possible for an SC value outside the Duration range to
      --  be "brought back into range" by an appropriate TS value, but there
      --  are also clearly SC values that are completely out of range. Note
      --  that the above diagram is wildly out of scale, the difference in
      --  ranges is much greater than shown.

      --  We can't just go generating out of range Duration values to test for
      --  overflow, since Duration is a full range type, so we follow the steps
      --  shown below.

      SC_Lo : constant Seconds_Count :=
                Seconds_Count (Duration (Time_Span_First) + Duration'(0.5));
      SC_Hi : constant Seconds_Count :=
                Seconds_Count (Duration (Time_Span_Last)  - Duration'(0.5));
      --  These are the maximum values of the seconds (integer) part of the
      --  Duration range. Used to compute and check the seconds in the result.

      TS_SC : Seconds_Count;
      --  Seconds part of input value

      TS_Fraction : Duration;
      --  Fractional part of input value, may be negative

      Result_SC : Seconds_Count;
      --  Seconds value for result

      Fudge : constant Seconds_Count := 10;
      --  Fudge value used to do end point checks far from end point

      FudgeD : constant Duration := Duration (Fudge);
      --  Fudge value as Duration

      Fudged_Result : Duration;
      --  Result fudged up or down by FudgeD

      procedure Out_Of_Range;
      pragma No_Return (Out_Of_Range);
      --  Raise exception for result out of range

      ------------------
      -- Out_Of_Range --
      ------------------

      procedure Out_Of_Range is
      begin
         raise Constraint_Error with
           "result for Ada.Real_Time.Time_Of is out of range";
      end Out_Of_Range;

   --  Start of processing for Time_Of

   begin
      --  If SC is so far out of range that there is no possibility of the
      --  addition of TS getting it back in range, raise an exception right
      --  away. That way we don't have to worry about SC values overflowing.

      if SC < 3 * SC_Lo or else SC > 3 * SC_Hi then
         Out_Of_Range;
      end if;

      --  Decompose input TS value

      TS_SC := Seconds_Count (Duration (TS));
      TS_Fraction := Duration (TS) - Duration (TS_SC);

      --  Compute result seconds. If clearly out of range, raise error now

      Result_SC := SC + TS_SC;

      if Result_SC < (SC_Lo - 1) or else Result_SC > (SC_Hi + 1) then
         Out_Of_Range;
      end if;

      --  Now the result is simply Result_SC + TS_Fraction, but we can't just
      --  go computing that since it might be out of range. So what we do is
      --  to compute a value fudged down or up by 10.0 (arbitrary value, but
      --  that will do fine), and check that fudged value, and if in range
      --  unfudge it and return the result.

      --  Fudge positive result down, and check high bound

      if Result_SC > 0 then
         Fudged_Result := Duration (Result_SC - Fudge) + TS_Fraction;

         if Fudged_Result <= Duration'Last - FudgeD then
            return Time (Fudged_Result + FudgeD);
         else
            Out_Of_Range;
         end if;

      --  Same for negative values of seconds, fudge up and check low bound

      else
         Fudged_Result := Duration (Result_SC + Fudge) + TS_Fraction;

         if Fudged_Result >= Duration'First + FudgeD then
            return Time (Fudged_Result - FudgeD);
         else
            Out_Of_Range;
         end if;
      end if;
   end Time_Of;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
   begin
      return Duration (TS);
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      --  Note regarding AI-00432 requiring range checking on this conversion.
      --  In almost all versions of GNAT (and all to which this version of the
      --  Ada.Real_Time package apply), the range of Time_Span and Duration are
      --  the same, so there is no issue of overflow.

      return Time_Span (D);
   end To_Time_Span;

begin
   --  Ensure that the tasking run time is initialized when using clock and/or
   --  delay operations. The initialization routine has the required machinery
   --  to prevent multiple calls to Initialize.

   System.Tasking.Initialize;
end Ada.Real_Time;
