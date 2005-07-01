------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2005, AdaCore                     --
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

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.OS2Lib.Errors;
with Interfaces.OS2Lib.Synchronization;

package body System.OS_Interface is

   use Interfaces;
   use Interfaces.OS2Lib;
   use Interfaces.OS2Lib.Synchronization;
   use Interfaces.OS2Lib.Errors;

   -----------
   -- Yield --
   -----------

   --  Give up the remainder of the time-slice and yield the processor
   --  to other threads of equal priority. Yield will return immediately
   --  without giving up the current time-slice when the only threads
   --  that are ready have a lower priority.

   --  ???  Just giving up the current time-slice seems not to be enough
   --  to get the thread to the end of the ready queue if OS/2 does use
   --  a queue at all. As a partial work-around, we give up two time-slices.

   --  This is the best we can do now, and at least is sufficient for passing
   --  the ACVC 2.0.1 Annex D tests.

   procedure Yield is
   begin
      Delay_For (0);
      Delay_For (0);
   end Yield;

   ---------------
   -- Delay_For --
   ---------------

   procedure Delay_For (Period : in Duration_In_Millisec) is
      Result : APIRET;

   begin
      pragma Assert (Period >= 0, "GNULLI---Delay_For: negative argument");

      --  ??? DosSleep is not the appropriate function for a delay in real
      --  time. It only gives up some number of scheduled time-slices.
      --  Use a timer instead or block for some semaphore with a time-out.
      Result := DosSleep (ULONG (Period));

      if Result = ERROR_TS_WAKEUP then

         --  Do appropriate processing for interrupted sleep
         --  Can we raise an exception here?

         null;
      end if;

      pragma Assert (Result = NO_ERROR, "GNULLI---Error in Delay_For");
   end Delay_For;

   -----------
   -- Clock --
   -----------

   function Clock return Duration is

      --  Implement conversion from tick count to Duration
      --  using fixed point arithmetic. The frequency of
      --  the Intel 8254 timer chip is 18.2 * 2**16 Hz.

      Tick_Duration : constant := 1.0 / (18.2 * 2**16);
      Tick_Count    : aliased QWORD;

   begin
      --  Read nr of clock ticks since boot time

      Must_Not_Fail (DosTmrQueryTime (Tick_Count'Access));

      return Tick_Count * Tick_Duration;
   end Clock;

end System.OS_Interface;
