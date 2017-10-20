------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               SYSTEM.TASK_PRIMITIVES.OPERATIONS.MONOTONIC                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1992-2017, Free Software Foundation, Inc.          --
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

--  This is the Monotonic version of this package for Posix and Linux targets.

separate (System.Task_Primitives.Operations)
package body Monotonic is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Compute_Deadline
     (Time       : Duration;
      Mode       : ST.Delay_Modes;
      Check_Time : out Duration;
      Abs_Time   : out Duration;
      Rel_Time   : out Duration);
   --  Helper for Timed_Sleep and Timed_Delay: given a deadline specified by
   --  Time and Mode, compute the current clock reading (Check_Time), and the
   --  target absolute and relative clock readings (Abs_Time, Rel_Time). The
   --  epoch for Time depends on Mode; the epoch for Check_Time and Abs_Time
   --  is always that of CLOCK_RT_Ada.

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      TS     : aliased timespec;
      Result : Interfaces.C.int;
   begin
      Result := clock_gettime
        (clock_id => OSC.CLOCK_RT_Ada, tp => TS'Unchecked_Access);
      pragma Assert (Result = 0);

      return To_Duration (TS);
   end Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
      TS     : aliased timespec;
      Result : Interfaces.C.int;

   begin
      Result := clock_getres (OSC.CLOCK_REALTIME, TS'Unchecked_Access);
      pragma Assert (Result = 0);

      return To_Duration (TS);
   end RT_Resolution;

   ----------------------
   -- Compute_Deadline --
   ----------------------

   procedure Compute_Deadline
     (Time       : Duration;
      Mode       : ST.Delay_Modes;
      Check_Time : out Duration;
      Abs_Time   : out Duration;
      Rel_Time   : out Duration)
   is
   begin
      Check_Time := Monotonic_Clock;

      --  Relative deadline

      if Mode = Relative then
         Abs_Time := Duration'Min (Time, Max_Sensible_Delay) + Check_Time;

         if Relative_Timed_Wait then
            Rel_Time := Duration'Min (Max_Sensible_Delay, Time);
         end if;

         pragma Warnings (Off);
         --  Comparison "OSC.CLOCK_RT_Ada = OSC.CLOCK_REALTIME" is compile
         --  time known.

      --  Absolute deadline specified using the tasking clock (CLOCK_RT_Ada)

      elsif Mode = Absolute_RT
        or else OSC.CLOCK_RT_Ada = OSC.CLOCK_REALTIME
      then
         pragma Warnings (On);
         Abs_Time := Duration'Min (Check_Time + Max_Sensible_Delay, Time);

         if Relative_Timed_Wait then
            Rel_Time := Duration'Min (Max_Sensible_Delay, Time - Check_Time);
         end if;

      --  Absolute deadline specified using the calendar clock, in the
      --  case where it is not the same as the tasking clock: compensate for
      --  difference between clock epochs (Base_Time - Base_Cal_Time).

      else
         declare
            Cal_Check_Time : constant Duration := OS_Primitives.Clock;
            RT_Time        : constant Duration :=
                               Time + Check_Time - Cal_Check_Time;

         begin
            Abs_Time :=
              Duration'Min (Check_Time + Max_Sensible_Delay, RT_Time);

            if Relative_Timed_Wait then
               Rel_Time :=
                 Duration'Min (Max_Sensible_Delay, RT_Time - Check_Time);
            end if;
         end;
      end if;
   end Compute_Deadline;

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.

   procedure Timed_Sleep
     (Self_ID  : ST.Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      pragma Unreferenced (Reason);

      Base_Time  : Duration;
      Check_Time : Duration;
      Abs_Time   : Duration;
      Rel_Time   : Duration;

      Request    : aliased timespec;
      Result     : Interfaces.C.int;

   begin
      Timedout := True;
      Yielded := False;

      Compute_Deadline
        (Time       => Time,
         Mode       => Mode,
         Check_Time => Check_Time,
         Abs_Time   => Abs_Time,
         Rel_Time   => Rel_Time);
      Base_Time := Check_Time;

      if Abs_Time > Check_Time then
         Request :=
           To_Timespec (if Relative_Timed_Wait then Rel_Time else Abs_Time);

         loop
            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            Result :=
              pthread_cond_timedwait
                (cond    => Self_ID.Common.LL.CV'Access,
                 mutex   => (if Single_Lock
                             then Single_RTS_Lock'Access
                             else Self_ID.Common.LL.L'Access),
                 abstime => Request'Access);

            Check_Time := Monotonic_Clock;
            exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

            if Result in 0 | EINTR then

               --  Somebody may have called Wakeup for us

               Timedout := False;
               exit;
            end if;

            pragma Assert (Result = ETIMEDOUT);
         end loop;
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so we assume the
   --  caller is abort-deferred but is holding no locks.

   procedure Timed_Delay
     (Self_ID : ST.Task_Id;
      Time    : Duration;
      Mode    : ST.Delay_Modes)
   is
      Base_Time  : Duration;
      Check_Time : Duration;
      Abs_Time   : Duration;
      Rel_Time   : Duration;
      Request    : aliased timespec;

      Result : Interfaces.C.int;
      pragma Warnings (Off, Result);

   begin
      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Self_ID);

      Compute_Deadline
        (Time       => Time,
         Mode       => Mode,
         Check_Time => Check_Time,
         Abs_Time   => Abs_Time,
         Rel_Time   => Rel_Time);
      Base_Time := Check_Time;

      if Abs_Time > Check_Time then
         Request :=
           To_Timespec (if Relative_Timed_Wait then Rel_Time else Abs_Time);
         Self_ID.Common.State := Delay_Sleep;

         loop
            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            Result :=
              pthread_cond_timedwait
                (cond    => Self_ID.Common.LL.CV'Access,
                 mutex   => (if Single_Lock
                             then Single_RTS_Lock'Access
                             else Self_ID.Common.LL.L'Access),
                 abstime => Request'Access);

            Check_Time := Monotonic_Clock;
            exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

            pragma Assert (Result in 0 | ETIMEDOUT | EINTR);
         end loop;

         Self_ID.Common.State := Runnable;
      end if;

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Result := sched_yield;
   end Timed_Delay;

end Monotonic;
