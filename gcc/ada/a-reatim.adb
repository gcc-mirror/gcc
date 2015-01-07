------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2014, AdaCore                     --
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

package body Ada.Real_Time is

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
   begin
      return Integer (Duration (Left) / Duration (Right));
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
      pragma Unsuppress (Overflow_Check);
      pragma Unsuppress (Division_Check);
   begin
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
   begin
      --  We want to return Time (SC) + TS. To avoid spurious overflows in
      --  the intermediate result Time (SC) we take advantage of the different
      --  signs in SC and TS (when that is the case).

      --  If the signs of SC and TS are different then we avoid converting SC
      --  to Time (as we do in the else part). The reason for that is that SC
      --  converted to Time may overflow the range of Time, while the addition
      --  of SC plus TS does not overflow (because of their different signs).
      --  The approach is to add and remove the greatest value of time
      --  (greatest absolute value) to both SC and TS. SC and TS have different
      --  signs, so we add the positive constant to the negative value, and the
      --  negative constant to the positive value, to prevent overflows.

      if (SC > 0 and then TS < 0.0) or else (SC < 0 and then TS > 0.0) then
         declare
            Closest_Boundary : constant Seconds_Count :=
              (if TS >= 0.0 then
                  Seconds_Count (Time_Span_Last  - Time_Span (0.5))
               else
                  Seconds_Count (Time_Span_First + Time_Span (0.5)));
            --  Value representing the integer part of the Time_Span boundary
            --  closest to TS (its number of seconds). Truncate towards zero
            --  to be sure that transforming this value back into Time cannot
            --  overflow (when SC is equal to 0). The sign of Closest_Boundary
            --  is always different from the sign of SC, hence avoiding
            --  overflow in the expression Time (SC + Closest_Boundary)
            --  which is part of the return statement.

            Dist_To_Boundary : constant Time_Span :=
              TS - Time_Span (Closest_Boundary);
            --  Distance between TS and Closest_Boundary expressed in Time_Span
            --  Both operands in the substraction have the same sign, hence
            --  avoiding overflow.

         begin
            --  Both operands in the inner addition have different signs,
            --  hence avoiding overflow. The Time () conversion and the outer
            --  addition can overflow only if SC + TC is not within Time'Range.

            return Time (SC + Closest_Boundary) + Dist_To_Boundary;
         end;

      --  Both operands have the same sign, so we can convert SC into Time
      --  right away; if this conversion overflows then the result of adding SC
      --  and TS would overflow anyway (so we would just be detecting the
      --  overflow a bit earlier).

      else
         return Time (SC) + TS;
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
