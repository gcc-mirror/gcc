------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               SYSTEM.TASKING.ASYNC_DELAYS.ENQUEUE_CALENDAR               --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2017, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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

with Ada.Calendar.Delays;

with System.OS_Constants;
with System.OS_Primitives;
with System.Task_Primitives.Operations;
with System.Tasking.Initialization;

function System.Tasking.Async_Delays.Enqueue_Calendar
  (T : Ada.Calendar.Time;
   D : Delay_Block_Access) return Boolean
is
   use type Ada.Calendar.Time;

   package SOSC renames System.OS_Constants;
   package STPO renames System.Task_Primitives.Operations;

   RT_T : Duration := Ada.Calendar.Delays.To_Duration (T);

begin
   if T <= Ada.Calendar.Clock then
      D.Timed_Out := True;
      System.Task_Primitives.Operations.Yield;
      return False;
   end if;

   --  T is expressed as a duration elapsed since the UNIX epoch, whereas
   --  Time_Enqueue expects duration elapsed since the epoch of the Ada real-
   --  time clock: compensate if necessary.

   --  Comparison "SOSC.CLOCK_RT_Ada = SOSC.CLOCK_REALTIME" is compile
   --  time known, so turn warnings off.

   pragma Warnings (Off);

   if SOSC.CLOCK_RT_Ada /= SOSC.CLOCK_REALTIME then
      pragma Warnings (On);

      RT_T := RT_T - OS_Primitives.Clock + STPO.Monotonic_Clock;
   end if;

   System.Tasking.Initialization.Defer_Abort
     (System.Task_Primitives.Operations.Self);
   Time_Enqueue (RT_T, D);
   return True;
end System.Tasking.Async_Delays.Enqueue_Calendar;
