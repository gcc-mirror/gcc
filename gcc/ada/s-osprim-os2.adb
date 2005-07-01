------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2005 Free Software Foundation, Inc.          --
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

--  This is the OS/2 version of this package

with Interfaces.C;                      use Interfaces.C;
with Interfaces.OS2Lib;                 use Interfaces.OS2Lib;
with Interfaces.OS2Lib.Synchronization; use Interfaces.OS2Lib.Synchronization;

package body System.OS_Primitives is

   ----------------
   -- Local Data --
   ----------------

   Epoch_Offset    : Duration;       --  See Set_Epoch_Offset
   Max_Tick_Count  : QWORD := 0.0;
   --  This is needed to compensate for small glitches in the
   --  hardware clock or the way it is read by the OS

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_Epoch_Offset;
   --  Initializes the Epoch_1970_Offset to the offset of the System_Clock
   --  relative to the Unix epoch (Jan 1, 1970), such that
   --     Clock = System_Clock + Epoch_1970_Offset

   function System_Clock return Duration;
   pragma Inline (System_Clock);
   --  Function returning value of system clock with system-dependent timebase.
   --  For OS/2 the system clock returns the elapsed time since system boot.
   --  The clock resolution is approximately 838 ns.

   ------------------
   -- System_Clock --
   ------------------

   function System_Clock return Duration is

      --  Implement conversion from tick count to Duration
      --  using fixed point arithmetic. The frequency of
      --  the Intel 8254 timer chip is 18.2 * 2**16 Hz.

      Tick_Duration : constant := 1.0 / (18.2 * 2**16);
      Tick_Count    : aliased QWORD;

   begin
      Must_Not_Fail (DosTmrQueryTime (Tick_Count'Access));
      --  Read nr of clock ticks since boot time

      Max_Tick_Count := QWORD'Max (Tick_Count, Max_Tick_Count);

      return Max_Tick_Count * Tick_Duration;
   end System_Clock;

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
   begin
      return System_Clock + Epoch_Offset;
   end Clock;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration renames Clock;

   ----------------------
   -- Set_Epoch_Offset --
   ----------------------

   procedure Set_Epoch_Offset is

      --  Interface to Unix C style gettimeofday

      type timeval is record
         tv_sec  : long;
         tv_usec : long;
      end record;

      procedure gettimeofday
        (time : access timeval;
         zone : System.Address := System.Address'Null_Parameter);
      pragma Import (C, gettimeofday);

      Time_Of_Day       : aliased timeval;
      Micro_To_Nano     : constant := 1.0E3;
      Sec_To_Nano       : constant := 1.0E9;
      Nanos_Since_Epoch : QWORD;

   begin
      gettimeofday (Time_Of_Day'Access);
      Nanos_Since_Epoch := QWORD (Time_Of_Day.tv_sec) * Sec_To_Nano
        + QWORD (Time_Of_Day.tv_usec) * Micro_To_Nano;

      Epoch_Offset :=
         Duration'(Nanos_Since_Epoch / Sec_To_Nano) - System_Clock;

   end Set_Epoch_Offset;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Time : Duration;
      Mode : Integer)
   is
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Check_Time : Duration := Clock;

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
            Must_Not_Fail (DosSleep (ULONG (Rel_Time * 1000.0)));

            Check_Time := Clock;

            exit when Abs_Time <= Check_Time;

            Rel_Time := Abs_Time - Check_Time;
         end loop;
      end if;
   end Timed_Delay;

begin
   Set_Epoch_Offset;
end System.OS_Primitives;
