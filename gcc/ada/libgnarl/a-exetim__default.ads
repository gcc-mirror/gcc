------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . E X E C U T I O N _ T I M E                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2007-2021, Free Software Foundation, Inc.         --
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

with Ada.Task_Identification;
with Ada.Real_Time;

package Ada.Execution_Time with
  SPARK_Mode
is

   type CPU_Time is private;

   CPU_Time_First : constant CPU_Time;
   CPU_Time_Last  : constant CPU_Time;
   CPU_Time_Unit  : constant := Ada.Real_Time.Time_Unit;
   CPU_Tick       : constant Ada.Real_Time.Time_Span;

   use type Ada.Task_Identification.Task_Id;

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
        Ada.Task_Identification.Current_Task)
      return CPU_Time
   with
     Volatile_Function,
     Global => Ada.Real_Time.Clock_Time,
     Pre    => T /= Ada.Task_Identification.Null_Task_Id;

   function "+"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   with
     Global => null;

   function "+"
     (Left  : Ada.Real_Time.Time_Span;
      Right : CPU_Time) return CPU_Time
   with
     Global => null;

   function "-"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   with
     Global => null;

   function "-"
     (Left  : CPU_Time;
      Right : CPU_Time) return Ada.Real_Time.Time_Span
   with
     Global => null;

   function "<"  (Left, Right : CPU_Time) return Boolean with
     Global => null;
   function "<=" (Left, Right : CPU_Time) return Boolean with
     Global => null;
   function ">"  (Left, Right : CPU_Time) return Boolean with
     Global => null;
   function ">=" (Left, Right : CPU_Time) return Boolean with
     Global => null;

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span)
   with
     Global => null;

   function Time_Of
     (SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
      return CPU_Time
   with
     Global => null;

   Interrupt_Clocks_Supported          : constant Boolean := False;
   Separate_Interrupt_Clocks_Supported : constant Boolean := False;

   pragma Warnings (Off, "check will fail at run time");
   function Clock_For_Interrupts return CPU_Time with
     Volatile_Function,
     Global => Ada.Real_Time.Clock_Time,
     Pre    => Interrupt_Clocks_Supported;
   pragma Warnings (On, "check will fail at run time");

private
   pragma SPARK_Mode (Off);

   type CPU_Time is new Ada.Real_Time.Time;

   CPU_Time_First : constant CPU_Time  := CPU_Time (Ada.Real_Time.Time_First);
   CPU_Time_Last  : constant CPU_Time  := CPU_Time (Ada.Real_Time.Time_Last);

   CPU_Tick : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Tick;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

end Ada.Execution_Time;
