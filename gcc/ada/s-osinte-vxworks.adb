------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--         Copyright (C) 1997-2011, Free Software Foundation, Inc.          --
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

--  This is the VxWorks version

--  This package encapsulates all direct interfaces to OS services that are
--  needed by children of System.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

package body System.OS_Interface is

   use type Interfaces.C.int;

   Low_Priority : constant := 255;
   --  VxWorks native (default) lowest scheduling priority

   -------------
   -- sigwait --
   -------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal) return int
   is
      Result : int;

      function sigwaitinfo
        (set : access sigset_t; sigvalue : System.Address) return int;
      pragma Import (C, sigwaitinfo, "sigwaitinfo");

   begin
      Result := sigwaitinfo (set, System.Null_Address);

      if Result /= -1 then
         sig.all := Signal (Result);
         return OK;
      else
         sig.all := 0;
         return errno;
      end if;
   end sigwait;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.ts_sec) + Duration (TS.ts_nsec) / 10#1#E9;
   end To_Duration;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;

   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F is negative due to a round-up, adjust for positive F value

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;

      return timespec'(ts_sec  => S,
                       ts_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -------------------------
   -- To_VxWorks_Priority --
   -------------------------

   function To_VxWorks_Priority (Priority : int) return int is
   begin
      return Low_Priority - Priority;
   end To_VxWorks_Priority;

   --------------------
   -- To_Clock_Ticks --
   --------------------

   --  ??? - For now, we'll always get the system clock rate since it is
   --  allowed to be changed during run-time in VxWorks. A better method would
   --  be to provide an operation to set it that so we can always know its
   --  value.

   --  Another thing we should probably allow for is a resultant tick count
   --  greater than int'Last. This should probably be a procedure with two
   --  output parameters, one in the range 0 .. int'Last, and another
   --  representing the overflow count.

   function To_Clock_Ticks (D : Duration) return int is
      Ticks          : Long_Long_Integer;
      Rate_Duration  : Duration;
      Ticks_Duration : Duration;

   begin
      if D < 0.0 then
         return ERROR;
      end if;

      --  Ensure that the duration can be converted to ticks
      --  at the current clock tick rate without overflowing.

      Rate_Duration := Duration (sysClkRateGet);

      if D > (Duration'Last / Rate_Duration) then
         Ticks := Long_Long_Integer (int'Last);
      else
         Ticks_Duration := D * Rate_Duration;
         Ticks := Long_Long_Integer (Ticks_Duration);

         if Ticks_Duration > Duration (Ticks) then
            Ticks := Ticks + 1;
         end if;

         if Ticks > Long_Long_Integer (int'Last) then
            Ticks := Long_Long_Integer (int'Last);
         end if;
      end if;

      return int (Ticks);
   end To_Clock_Ticks;

   -----------------------------
   -- Binary_Semaphore_Create --
   -----------------------------

   function Binary_Semaphore_Create return Binary_Semaphore_Id is
   begin
      return Binary_Semaphore_Id (semBCreate (SEM_Q_FIFO, SEM_EMPTY));
   end Binary_Semaphore_Create;

   -----------------------------
   -- Binary_Semaphore_Delete --
   -----------------------------

   function Binary_Semaphore_Delete (ID : Binary_Semaphore_Id) return int is
   begin
      return semDelete (SEM_ID (ID));
   end Binary_Semaphore_Delete;

   -----------------------------
   -- Binary_Semaphore_Obtain --
   -----------------------------

   function Binary_Semaphore_Obtain (ID : Binary_Semaphore_Id) return int is
   begin
      return semTake (SEM_ID (ID), WAIT_FOREVER);
   end Binary_Semaphore_Obtain;

   ------------------------------
   -- Binary_Semaphore_Release --
   ------------------------------

   function Binary_Semaphore_Release (ID : Binary_Semaphore_Id) return int is
   begin
      return semGive (SEM_ID (ID));
   end Binary_Semaphore_Release;

   ----------------------------
   -- Binary_Semaphore_Flush --
   ----------------------------

   function Binary_Semaphore_Flush (ID : Binary_Semaphore_Id) return int is
   begin
      return semFlush (SEM_ID (ID));
   end Binary_Semaphore_Flush;

   ----------
   -- kill --
   ----------

   function kill (pid : t_id; sig : Signal) return int is
   begin
      return System.VxWorks.Ext.kill (pid, int (sig));
   end kill;

   -----------------------
   -- Interrupt_Connect --
   -----------------------

   function Interrupt_Connect
     (Vector    : Interrupt_Vector;
      Handler   : Interrupt_Handler;
      Parameter : System.Address := System.Null_Address) return int is
   begin
      return
        System.VxWorks.Ext.Interrupt_Connect
        (System.VxWorks.Ext.Interrupt_Vector (Vector),
         System.VxWorks.Ext.Interrupt_Handler (Handler),
         Parameter);
   end Interrupt_Connect;

   -----------------------
   -- Interrupt_Context --
   -----------------------

   function Interrupt_Context return int is
   begin
      return System.VxWorks.Ext.Interrupt_Context;
   end Interrupt_Context;

   --------------------------------
   -- Interrupt_Number_To_Vector --
   --------------------------------

   function Interrupt_Number_To_Vector
     (intNum : int) return Interrupt_Vector
   is
   begin
      return Interrupt_Vector
        (System.VxWorks.Ext.Interrupt_Number_To_Vector (intNum));
   end Interrupt_Number_To_Vector;

   -----------------
   -- Current_CPU --
   -----------------

   function Current_CPU return Multiprocessors.CPU is
   begin
      --  ??? Should use vxworks multiprocessor interface

      return Multiprocessors.CPU'First;
   end Current_CPU;

end System.OS_Interface;
