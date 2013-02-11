------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2013, Free Software Foundation, Inc.         --
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

--  This is the NT version of this package

with System.Task_Lock;
with System.Win32.Ext;

package body System.OS_Primitives is

   use System.Task_Lock;
   use System.Win32;
   use System.Win32.Ext;

   ----------------------------------------
   -- Data for the high resolution clock --
   ----------------------------------------

   Tick_Frequency : aliased LARGE_INTEGER;
   --  Holds frequency of high-performance counter used by Clock
   --  Windows NT uses a 1_193_182 Hz counter on PCs.

   Base_Monotonic_Ticks : LARGE_INTEGER;
   --  Holds the Tick count for the base monotonic time

   Base_Monotonic_Clock : Duration;
   --  Holds the current clock for monotonic clock's base time

   type Clock_Data is record
      Base_Ticks : LARGE_INTEGER;
      --  Holds the Tick count for the base time

      Base_Time : Long_Long_Integer;
      --  Holds the base time used to check for system time change, used with
      --  the standard clock.

      Base_Clock : Duration;
      --  Holds the current clock for the standard clock's base time
   end record;

   type Clock_Data_Access is access all Clock_Data;

   --  Two base clock buffers. This is used to be able to update a buffer
   --  while the other buffer is read. The point is that we do not want to
   --  use a lock inside the Clock routine for performance reasons. We still
   --  use a lock in the Get_Base_Time which is called very rarely. Current
   --  is a pointer, the pragma Atomic is there to ensure that the value can
   --  be set or read atomically. That's it, when Get_Base_Time has updated
   --  a buffer the switch to the new value is done by changing Current
   --  pointer.

   First, Second : aliased Clock_Data;
   Current       : Clock_Data_Access := First'Access;
   pragma Atomic (Current);

   --  The following signature is to detect change on the base clock data
   --  above. The signature is a modular type, it will wrap around without
   --  raising an exception. We would need to have exactly 2**32 updates of
   --  the base data for the changes to get undetected.

   type Signature_Type is mod 2**32;
   Signature     : Signature_Type := 0;
   pragma Atomic (Signature);

   procedure Get_Base_Time (Data : out Clock_Data);
   --  Retrieve the base time and base ticks. These values will be used by
   --  clock to compute the current time by adding to it a fraction of the
   --  performance counter. This is for the implementation of a
   --  high-resolution clock. Note that this routine does not change the base
   --  monotonic values used by the monotonic clock.

   -----------
   -- Clock --
   -----------

   --  This implementation of clock provides high resolution timer values
   --  using QueryPerformanceCounter. This call return a 64 bits values (based
   --  on the 8253 16 bits counter). This counter is updated every 1/1_193_182
   --  times per seconds. The call to QueryPerformanceCounter takes 6
   --  microsecs to complete.

   function Clock return Duration is
      Max_Shift            : constant Duration        := 2.0;
      Hundreds_Nano_In_Sec : constant Long_Long_Float := 1.0E7;
      Data                 : Clock_Data;
      Current_Ticks        : aliased LARGE_INTEGER;
      Elap_Secs_Tick       : Duration;
      Elap_Secs_Sys        : Duration;
      Now                  : aliased Long_Long_Integer;
      Sig1, Sig2           : Signature_Type;

   begin
      --  Try ten times to get a coherent set of base data. For this we just
      --  check that the signature hasn't changed during the copy of the
      --  current data.
      --
      --  This loop will always be done once if there is no interleaved call
      --  to Get_Base_Time.

      for K in 1 .. 10 loop
         Sig1 := Signature;
         Data := Current.all;
         Sig2 := Signature;
         exit when Sig1 = Sig2;
      end loop;

      if QueryPerformanceCounter (Current_Ticks'Access) = Win32.FALSE then
         return 0.0;
      end if;

      GetSystemTimeAsFileTime (Now'Access);

      Elap_Secs_Sys :=
        Duration (Long_Long_Float (abs (Now - Data.Base_Time)) /
                    Hundreds_Nano_In_Sec);

      Elap_Secs_Tick :=
        Duration (Long_Long_Float (Current_Ticks - Data.Base_Ticks) /
                  Long_Long_Float (Tick_Frequency));

      --  If we have a shift of more than Max_Shift seconds we resynchronize
      --  the Clock. This is probably due to a manual Clock adjustment, a DST
      --  adjustment or an NTP synchronisation. And we want to adjust the time
      --  for this system (non-monotonic) clock.

      if abs (Elap_Secs_Sys - Elap_Secs_Tick) > Max_Shift then
         Get_Base_Time (Data);

         Elap_Secs_Tick :=
           Duration (Long_Long_Float (Current_Ticks - Data.Base_Ticks) /
                     Long_Long_Float (Tick_Frequency));
      end if;

      return Data.Base_Clock + Elap_Secs_Tick;
   end Clock;

   -------------------
   -- Get_Base_Time --
   -------------------

   procedure Get_Base_Time (Data : out Clock_Data) is

      --  The resolution for GetSystemTime is 1 millisecond

      --  The time to get both base times should take less than 1 millisecond.
      --  Therefore, the elapsed time reported by GetSystemTime between both
      --  actions should be null.

      epoch_1970     : constant := 16#19D_B1DE_D53E_8000#; -- win32 UTC epoch
      system_time_ns : constant := 100;                    -- 100 ns per tick
      Sec_Unit       : constant := 10#1#E9;
      Max_Elapsed    : constant LARGE_INTEGER :=
                         LARGE_INTEGER (Tick_Frequency / 100_000);
      --  Look for a precision of 0.01 ms
      Sig            : constant Signature_Type := Signature;

      Loc_Ticks, Ctrl_Ticks : aliased LARGE_INTEGER;
      Loc_Time, Ctrl_Time   : aliased Long_Long_Integer;
      Elapsed               : LARGE_INTEGER;
      Current_Max           : LARGE_INTEGER := LARGE_INTEGER'Last;
      New_Data              : Clock_Data_Access;

   begin
      --  Here we must be sure that both of these calls are done in a short
      --  amount of time. Both are base time and should in theory be taken
      --  at the very same time.

      --  The goal of the following loop is to synchronize the system time
      --  with the Win32 performance counter by getting a base offset for both.
      --  Using these offsets it is then possible to compute actual time using
      --  a performance counter which has a better precision than the Win32
      --  time API.

      --  Try at most 10 times to reach the best synchronisation (below 1
      --  millisecond) otherwise the runtime will use the best value reached
      --  during the runs.

      Lock;

      --  First check that the current value has not been updated. This
      --  could happen if another task has called Clock at the same time
      --  and that Max_Shift has been reached too.
      --
      --  But if the current value has been changed just before we entered
      --  into the critical section, we can safely return as the current
      --  base data (time, clock, ticks) have already been updated.

      if Sig /= Signature then
         return;
      end if;

      --  Check for the unused data buffer and set New_Data to point to it

      if Current = First'Access then
         New_Data := Second'Access;
      else
         New_Data := First'Access;
      end if;

      for K in 1 .. 10 loop
         if QueryPerformanceCounter (Loc_Ticks'Access) = Win32.FALSE then
            pragma Assert
              (Standard.False,
               "Could not query high performance counter in Clock");
            null;
         end if;

         GetSystemTimeAsFileTime (Ctrl_Time'Access);

         --  Scan for clock tick, will take up to 16ms/1ms depending on PC.
         --  This cannot be an infinite loop or the system hardware is badly
         --  damaged.

         loop
            GetSystemTimeAsFileTime (Loc_Time'Access);

            if QueryPerformanceCounter (Ctrl_Ticks'Access) = Win32.FALSE then
               pragma Assert
                 (Standard.False,
                  "Could not query high performance counter in Clock");
               null;
            end if;

            exit when Loc_Time /= Ctrl_Time;
            Loc_Ticks := Ctrl_Ticks;
         end loop;

         --  Check elapsed Performance Counter between samples
         --  to choose the best one.

         Elapsed := Ctrl_Ticks - Loc_Ticks;

         if Elapsed < Current_Max then
            New_Data.Base_Time   := Loc_Time;
            New_Data.Base_Ticks  := Loc_Ticks;
            Current_Max := Elapsed;

            --  Exit the loop when we have reached the expected precision

            exit when Elapsed <= Max_Elapsed;
         end if;
      end loop;

      New_Data.Base_Clock := Duration
        (Long_Long_Float ((New_Data.Base_Time - epoch_1970) * system_time_ns) /
           Long_Long_Float (Sec_Unit));

      --  At this point all the base values have been set into the new data
      --  record. We just change the pointer (atomic operation) to this new
      --  values.

      Current := New_Data;
      Data    := New_Data.all;

      --  Set new signature for this data set

      Signature := Signature + 1;

      Unlock;

   exception
      when others =>
         Unlock;
         raise;
   end Get_Base_Time;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      Current_Ticks  : aliased LARGE_INTEGER;
      Elap_Secs_Tick : Duration;

   begin
      if QueryPerformanceCounter (Current_Ticks'Access) = Win32.FALSE then
         return 0.0;

      else
         Elap_Secs_Tick :=
           Duration (Long_Long_Float (Current_Ticks - Base_Monotonic_Ticks) /
                       Long_Long_Float (Tick_Frequency));
         return Base_Monotonic_Clock + Elap_Secs_Tick;
      end if;
   end Monotonic_Clock;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay (Time : Duration; Mode : Integer) is

      function Mode_Clock return Duration;
      pragma Inline (Mode_Clock);
      --  Return the current clock value using either the monotonic clock or
      --  standard clock depending on the Mode value.

      ----------------
      -- Mode_Clock --
      ----------------

      function Mode_Clock return Duration is
      begin
         case Mode is
            when Absolute_RT =>
               return Monotonic_Clock;
            when others =>
               return Clock;
         end case;
      end Mode_Clock;

      --  Local Variables

      Base_Time : constant Duration := Mode_Clock;
      --  Base_Time is used to detect clock set backward, in this case we
      --  cannot ensure the delay accuracy.

      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Check_Time : Duration := Base_Time;

   --  Start of processing for Timed Delay

   begin
      if Mode = Relative then
         Rel_Time := Time;
         Abs_Time := Time + Check_Time;
      else
         Rel_Time := Time - Check_Time;
         Abs_Time := Time;
      end if;

      if Rel_Time > 0.0 then
         loop
            Sleep (DWORD (Rel_Time * 1000.0));
            Check_Time := Mode_Clock;

            exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

            Rel_Time := Abs_Time - Check_Time;
         end loop;
      end if;
   end Timed_Delay;

   ----------------
   -- Initialize --
   ----------------

   Initialized : Boolean := False;

   procedure Initialize is
   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Get starting time as base

      if QueryPerformanceFrequency (Tick_Frequency'Access) = Win32.FALSE then
         raise Program_Error with
           "cannot get high performance counter frequency";
      end if;

      Get_Base_Time (Current.all);

      --  Keep base clock and ticks for the monotonic clock. These values
      --  should never be changed to ensure proper behavior of the monotonic
      --  clock.

      Base_Monotonic_Clock := Current.Base_Clock;
      Base_Monotonic_Ticks := Current.Base_Ticks;
   end Initialize;

end System.OS_Primitives;
