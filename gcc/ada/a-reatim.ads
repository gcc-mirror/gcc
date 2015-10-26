------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with System.Task_Primitives.Operations;
pragma Elaborate_All (System.Task_Primitives.Operations);

package Ada.Real_Time with
  SPARK_Mode,
  Abstract_State => (Clock_Time with Synchronous,
                                     External => (Async_Readers,
                                                  Async_Writers))
is

   pragma Compile_Time_Error
     (Duration'Size /= 64,
      "this version of Ada.Real_Time requires 64-bit Duration");

   type Time is private;
   Time_First : constant Time;
   Time_Last  : constant Time;
   Time_Unit  : constant := 10#1.0#E-9;

   type Time_Span is private;
   Time_Span_First : constant Time_Span;
   Time_Span_Last  : constant Time_Span;
   Time_Span_Zero  : constant Time_Span;
   Time_Span_Unit  : constant Time_Span;

   Tick : constant Time_Span;
   function Clock return Time with
     Volatile_Function,
     Global => Clock_Time;

   function "+"  (Left : Time;      Right : Time_Span) return Time with
     Global => null;
   function "+"  (Left : Time_Span; Right : Time)      return Time with
     Global => null;
   function "-"  (Left : Time;      Right : Time_Span) return Time with
     Global => null;
   function "-"  (Left : Time;      Right : Time)      return Time_Span with
     Global => null;

   function "<"  (Left, Right : Time) return Boolean with
     Global => null;
   function "<=" (Left, Right : Time) return Boolean with
     Global => null;
   function ">"  (Left, Right : Time) return Boolean with
     Global => null;
   function ">=" (Left, Right : Time) return Boolean with
     Global => null;

   function "+"  (Left, Right : Time_Span)             return Time_Span with
     Global => null;
   function "-"  (Left, Right : Time_Span)             return Time_Span with
     Global => null;
   function "-"  (Right : Time_Span)                   return Time_Span with
     Global => null;
   function "*"  (Left : Time_Span; Right : Integer)   return Time_Span with
     Global => null;
   function "*"  (Left : Integer;   Right : Time_Span) return Time_Span with
     Global => null;
   function "/"  (Left, Right : Time_Span)             return Integer with
     Global => null;
   function "/"  (Left : Time_Span; Right : Integer)   return Time_Span with
     Global => null;

   function "abs" (Right : Time_Span) return Time_Span with
     Global => null;

   function "<"  (Left, Right : Time_Span) return Boolean with
     Global => null;
   function "<=" (Left, Right : Time_Span) return Boolean with
     Global => null;
   function ">"  (Left, Right : Time_Span) return Boolean with
     Global => null;
   function ">=" (Left, Right : Time_Span) return Boolean with
     Global => null;

   function To_Duration  (TS : Time_Span) return Duration with
     Global => null;
   function To_Time_Span (D : Duration)   return Time_Span with
     Global => null;

   function Nanoseconds  (NS : Integer) return Time_Span with
     Global => null;
   function Microseconds (US : Integer) return Time_Span with
     Global => null;
   function Milliseconds (MS : Integer) return Time_Span with
     Global => null;

   function Seconds (S : Integer) return Time_Span with
     Global => null;
   pragma Ada_05 (Seconds);

   function Minutes (M : Integer) return Time_Span with
     Global => null;
   pragma Ada_05 (Minutes);

   type Seconds_Count is new Long_Long_Integer;
   --  Seconds_Count needs 64 bits, since the type Time has the full range of
   --  Duration. The delta of Duration is 10 ** (-9), so the maximum number of
   --  seconds is 2**63/10**9 = 8*10**9 which does not quite fit in 32 bits.
   --  However, rather than make this explicitly 64-bits we derive from
   --  Long_Long_Integer. In normal usage this will have the same effect. But
   --  in the case of CodePeer with a target configuration file with a maximum
   --  integer size of 32, it allows analysis of this unit.

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span)
   with
     Global => null;
   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time
   with
     Global => null;

private
   pragma SPARK_Mode (Off);

   --  Time and Time_Span are represented in 64-bit Duration value in
   --  nanoseconds. For example, 1 second and 1 nanosecond is represented
   --  as the stored integer 1_000_000_001. This is for the 64-bit Duration
   --  case, not clear if this also is used for 32-bit Duration values.

   type Time is new Duration;

   Time_First : constant Time := Time'First;

   Time_Last  : constant Time := Time'Last;

   type Time_Span is new Duration;

   Time_Span_First : constant Time_Span := Time_Span'First;

   Time_Span_Last  : constant Time_Span := Time_Span'Last;

   Time_Span_Zero  : constant Time_Span := 0.0;

   Time_Span_Unit  : constant Time_Span := 10#1.0#E-9;

   Tick : constant Time_Span :=
            Time_Span (System.Task_Primitives.Operations.RT_Resolution);

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "abs");

   pragma Inline (Microseconds);
   pragma Inline (Milliseconds);
   pragma Inline (Nanoseconds);
   pragma Inline (Seconds);
   pragma Inline (Minutes);

end Ada.Real_Time;
