------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.3 $
--                                                                          --
--            Copyright (C) 1991-2001 Florida State University              --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is the OS/2 version of this package

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.C.Strings;
with Interfaces.OS2Lib.Errors;
with Interfaces.OS2Lib.Synchronization;

package body System.OS_Interface is

   use Interfaces;
   use Interfaces.OS2Lib;
   use Interfaces.OS2Lib.Synchronization;
   use Interfaces.OS2Lib.Errors;

   ------------------
   -- Timer (spec) --
   ------------------

   --  Although the OS uses a 32-bit integer representing milliseconds
   --  as timer value that doesn't work for us since 32 bits are not
   --  enough for absolute timing. Also it is useful to use better
   --  intermediate precision when adding/subtracting timing intervals.
   --  So we use the standard Ada Duration type which is implemented using
   --  microseconds.

   --  Shouldn't the timer be moved to a separate package ???

   type Timer is record
      Handle : aliased HTIMER := NULLHANDLE;
      Event  : aliased HEV    := NULLHANDLE;
   end record;

   procedure Initialize (T :    out Timer);
   procedure Finalize   (T : in out Timer);
   procedure Wait       (T : in out Timer);
   procedure Reset      (T : in out Timer);

   procedure Set_Timer_For (T : in out Timer; Period : in Duration);
   procedure Set_Timer_At  (T : in out Timer; Time   : in Duration);
   --  Add a hook to locate the Epoch, for use with Calendar????

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

   ----------------------
   -- Initialize Timer --
   ----------------------

   procedure Initialize (T : out Timer) is
   begin
      pragma Assert
        (T.Handle = NULLHANDLE, "GNULLI---Timer already initialized");

      Must_Not_Fail (DosCreateEventSem
        (pszName => Interfaces.C.Strings.Null_Ptr,
         f_phev  => T.Event'Unchecked_Access,
         flAttr  => DC_SEM_SHARED,
         fState  => False32));
   end Initialize;

   -------------------
   -- Set_Timer_For --
   -------------------

   procedure Set_Timer_For
     (T         : in out Timer;
      Period    : in Duration)
   is
      Rel_Time  : Duration_In_Millisec :=
                    Duration_In_Millisec (Period * 1_000.0);

   begin
      pragma Assert
        (T.Event /= NULLHANDLE, "GNULLI---Timer not initialized");
      pragma Assert
        (T.Handle = NULLHANDLE, "GNULLI---Timer already in use");

      Must_Not_Fail (DosAsyncTimer
        (msec      => ULONG (Rel_Time),
         F_hsem    => HSEM (T.Event),
         F_phtimer => T.Handle'Unchecked_Access));
   end Set_Timer_For;

   ------------------
   -- Set_Timer_At --
   ------------------

   --  Note that the timer is started in a critical section to prevent the
   --  race condition when absolute time is converted to time relative to
   --  current time. T.Event will be posted when the Time has passed

   procedure Set_Timer_At
     (T         : in out Timer;
      Time      : in Duration)
   is
      Relative_Time : Duration;

   begin
      Must_Not_Fail (DosEnterCritSec);

      begin
         Relative_Time := Time - Clock;
         if Relative_Time >  0.0 then
            Set_Timer_For (T, Period => Time - Clock);
         else
            Sem_Must_Not_Fail (DosPostEventSem (T.Event));
         end if;
      end;

      Must_Not_Fail (DosExitCritSec);
   end Set_Timer_At;

   ----------
   -- Wait --
   ----------

   procedure Wait (T : in out Timer) is
   begin
      Sem_Must_Not_Fail (DosWaitEventSem (T.Event, SEM_INDEFINITE_WAIT));
      T.Handle := NULLHANDLE;
   end Wait;

   -----------
   -- Reset --
   -----------

   procedure Reset (T : in out Timer) is
      Dummy_Count : aliased ULONG;

   begin
      if T.Handle /= NULLHANDLE then
         Must_Not_Fail (DosStopTimer (T.Handle));
         T.Handle := NULLHANDLE;
      end if;

      Sem_Must_Not_Fail
        (DosResetEventSem (T.Event, Dummy_Count'Unchecked_Access));
   end Reset;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out Timer) is
   begin
      Reset (T);
      Must_Not_Fail (DosCloseEventSem (T.Event));
      T.Event := NULLHANDLE;
   end Finalize;

end System.OS_Interface;
