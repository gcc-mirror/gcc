------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--          Copyright (C) 1998-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the NT version of this package

with Ada.Exceptions;
with System.OS_Interface;

package body System.OS_Primitives is

   use System.OS_Interface;

   ---------------------------------------
   -- Data for the high resolution clock --
   ---------------------------------------

   Tick_Frequency       : aliased LARGE_INTEGER;
   --  Holds frequency of high-performance counter used by Clock
   --  Windows NT uses a 1_193_182 Hz counter on PCs.

   Base_Ticks           : aliased LARGE_INTEGER;
   --  Holds the Tick count for the base time.

   Base_Clock           : Duration;
   --  Holds the current clock for the standard clock's base time

   Base_Monotonic_Clock : Duration;
   --  Holds the current clock for monotonic clock's base time

   Base_Time            : aliased Long_Long_Integer;
   --  Holds the base time used to check for system time change, used with
   --  the standard clock.

   procedure Get_Base_Time;
   --  Retrieve the base time. This base time will be used by clock to
   --  compute the current time by adding to it a fraction of the
   --  performance counter. This is for the implementation of a
   --  high-resolution clock.

   -----------
   -- Clock --
   -----------

   --  This implementation of clock provides high resolution timer values
   --  using QueryPerformanceCounter. This call return a 64 bits values (based
   --  on the 8253 16 bits counter). This counter is updated every 1/1_193_182
   --  times per seconds. The call to QueryPerformanceCounter takes 6
   --  microsecs to complete.

   function Clock return Duration is
      Max_Shift            : constant Duration := 2.0;
      Hundreds_Nano_In_Sec : constant := 1E7;
      Current_Ticks        : aliased LARGE_INTEGER;
      Elap_Secs_Tick       : Duration;
      Elap_Secs_Sys        : Duration;
      Now                  : aliased Long_Long_Integer;

   begin
      if not QueryPerformanceCounter (Current_Ticks'Access) then
         return 0.0;
      end if;

      GetSystemTimeAsFileTime (Now'Access);

      Elap_Secs_Sys :=
        Duration (abs (Now - Base_Time) / Hundreds_Nano_In_Sec);

      Elap_Secs_Tick :=
        Duration (Long_Long_Float (Current_Ticks - Base_Ticks) /
                  Long_Long_Float (Tick_Frequency));

      --  If we have a shift of more than Max_Shift seconds we resynchonize the
      --  Clock. This is probably due to a manual Clock adjustment, an DST
      --  adjustment or an NNTP synchronisation. And we want to adjust the
      --  time for this system (non-monotonic) clock.

      if abs (Elap_Secs_Sys - Elap_Secs_Tick) > Max_Shift then
         Get_Base_Time;

         Elap_Secs_Tick :=
           Duration (Long_Long_Float (Current_Ticks - Base_Ticks) /
                     Long_Long_Float (Tick_Frequency));
      end if;

      return Base_Clock + Elap_Secs_Tick;
   end Clock;

   -------------------
   -- Get_Base_Time --
   -------------------

   procedure Get_Base_Time is
      use System.OS_Interface;

      --  The resolution for GetSystemTime is 1 millisecond.

      --  The time to get both base times should take less than 1 millisecond.
      --  Therefore, the elapsed time reported by GetSystemTime between both
      --  actions should be null.

      Max_Elapsed    : constant := 0;

      Test_Now       : aliased Long_Long_Integer;

      epoch_1970     : constant := 16#19D_B1DE_D53E_8000#; -- win32 UTC epoch
      system_time_ns : constant := 100;                    -- 100 ns per tick
      Sec_Unit       : constant := 10#1#E9;

   begin
      --  Here we must be sure that both of these calls are done in a short
      --  amount of time. Both are base time and should in theory be taken
      --  at the very same time.

      loop
         GetSystemTimeAsFileTime (Base_Time'Access);

         if not QueryPerformanceCounter (Base_Ticks'Access) then
            pragma Assert
              (Standard.False,
               "Could not query high performance counter in Clock");
            null;
         end if;

         GetSystemTimeAsFileTime (Test_Now'Access);

         exit when Test_Now - Base_Time = Max_Elapsed;
      end loop;

      Base_Clock := Duration
        (Long_Long_Float ((Base_Time - epoch_1970) * system_time_ns) /
         Long_Long_Float (Sec_Unit));
   end Get_Base_Time;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      Current_Ticks  : aliased LARGE_INTEGER;
      Elap_Secs_Tick : Duration;
   begin
      if not QueryPerformanceCounter (Current_Ticks'Access) then
         return 0.0;
      end if;

      Elap_Secs_Tick :=
        Duration (Long_Long_Float (Current_Ticks - Base_Ticks) /
                  Long_Long_Float (Tick_Frequency));

      return Base_Monotonic_Clock + Elap_Secs_Tick;
   end Monotonic_Clock;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay (Time : Duration; Mode : Integer) is
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Check_Time : Duration := Monotonic_Clock;

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
            Check_Time := Monotonic_Clock;

            exit when Abs_Time <= Check_Time;

            Rel_Time := Abs_Time - Check_Time;
         end loop;
      end if;
   end Timed_Delay;

--  Package elaboration, get starting time as base

begin
   if not QueryPerformanceFrequency (Tick_Frequency'Access) then
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity,
         "cannot get high performance counter frequency");
   end if;

   Get_Base_Time;

   Base_Monotonic_Clock := Base_Clock;
end System.OS_Primitives;
