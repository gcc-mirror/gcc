------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2010, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
   begin
      return Integer (Duration (Left) / Duration (Right));
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
      pragma Unsuppress (Overflow_Check);
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
      return Time (SC) + TS;
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
