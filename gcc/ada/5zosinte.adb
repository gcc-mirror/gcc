------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--             Copyright (C) 1997-2001 Free Software Foundation             --
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

--  This is the VxWorks version.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.C; use Interfaces.C;

with System.VxWorks;
--  used for Wind_TCB_Ptr

with Unchecked_Conversion;

package body System.OS_Interface is

   use System.VxWorks;

   --  Option flags for taskSpawn

   VX_UNBREAKABLE    : constant := 16#0002#;
   VX_FP_TASK        : constant := 16#0008#;
   VX_FP_PRIVATE_ENV : constant := 16#0080#;
   VX_NO_STACK_FILL  : constant := 16#0100#;

   function taskSpawn
     (name          : System.Address;  --  Pointer to task name
      priority      : int;
      options       : int;
      stacksize     : size_t;
      start_routine : Thread_Body;
      arg1          : System.Address;
      arg2          : int := 0;
      arg3          : int := 0;
      arg4          : int := 0;
      arg5          : int := 0;
      arg6          : int := 0;
      arg7          : int := 0;
      arg8          : int := 0;
      arg9          : int := 0;
      arg10         : int := 0) return pthread_t;
   pragma Import (C, taskSpawn, "taskSpawn");

   procedure taskDelete (tid : pthread_t);
   pragma Import (C, taskDelete, "taskDelete");

   --  These are the POSIX scheduling priorities. These are enabled
   --  when the global variable posixPriorityNumbering is 1.

   POSIX_SCHED_FIFO_LOW_PRI  : constant := 0;
   POSIX_SCHED_FIFO_HIGH_PRI : constant := 255;
   POSIX_SCHED_RR_LOW_PRI    : constant := 0;
   POSIX_SCHED_RR_HIGH_PRI   : constant := 255;

   --  These are the VxWorks native (default) scheduling priorities.
   --  These are used when the global variable posixPriorityNumbering
   --  is 0.

   SCHED_FIFO_LOW_PRI  : constant := 255;
   SCHED_FIFO_HIGH_PRI : constant := 0;
   SCHED_RR_LOW_PRI    : constant := 255;
   SCHED_RR_HIGH_PRI   : constant := 0;

   --  Global variable to enable POSIX priority numbering.
   --  By default, it is 0 and VxWorks native priority numbering
   --  is used.

   posixPriorityNumbering : int;
   pragma Import (C, posixPriorityNumbering, "posixPriorityNumbering");

   --  VxWorks will let you set round-robin scheduling globally
   --  for all tasks, but not for individual tasks.  Attempting
   --  to set the scheduling policy for a specific task (using
   --  sched_setscheduler) to something other than what the system
   --  is currently using will fail.  If you wish to change the
   --  scheduling policy, then use the following function to set
   --  it globally for all tasks.  When ticks is 0, time slicing
   --  (round-robin scheduling) is disabled.

   function kernelTimeSlice (ticks : int) return int;
   pragma Import (C, kernelTimeSlice, "kernelTimeSlice");

   function taskPriorityGet
     (tid       : pthread_t;
      pPriority : access int)
     return int;
   pragma Import (C, taskPriorityGet, "taskPriorityGet");

   function taskPrioritySet
     (tid         : pthread_t;
      newPriority : int)
     return int;
   pragma Import (C, taskPrioritySet, "taskPrioritySet");

   function To_Wind_TCB_Ptr is
     new Unchecked_Conversion (pthread_t, Wind_TCB_Ptr);


   --  Error codes (errno).  The lower level 16 bits are the
   --  error code, with the upper 16 bits representing the
   --  module number in which the error occurred.  By convention,
   --  the module number is 0 for UNIX errors.  VxWorks reserves
   --  module numbers 1-500, with the remaining module numbers
   --  being available for user applications.

   M_objLib                 : constant := 61 * 2**16;
   --  semTake() failure with ticks = NO_WAIT
   S_objLib_OBJ_UNAVAILABLE : constant := M_objLib + 2;
   --  semTake() timeout with ticks > NO_WAIT
   S_objLib_OBJ_TIMEOUT     : constant := M_objLib + 4;

   --  We use two different kinds of VxWorks semaphores: mutex
   --  and binary semaphores.  A null (0) ID is returned when
   --  a semaphore cannot be created. Binary semaphores and common
   --  operations are declared in the spec of this package,
   --  as they are used to implement hardware interrupt handling

   function semMCreate
     (options : int) return SEM_ID;
   pragma Import (C, semMCreate, "semMCreate");


   function taskLock return int;
   pragma Import (C, taskLock, "taskLock");

   function taskUnlock return int;
   pragma Import (C, taskUnlock, "taskUnlock");

   -------------------------------------------------------
   --  Convenience routines to convert between VxWorks  --
   --  priority and POSIX priority.                     --
   -------------------------------------------------------

   function To_Vxworks_Priority (Priority : in int) return int;
   pragma Inline (To_Vxworks_Priority);

   function To_Posix_Priority (Priority : in int) return int;
   pragma Inline (To_Posix_Priority);

   function To_Vxworks_Priority (Priority : in int) return int is
   begin
      return SCHED_FIFO_LOW_PRI - Priority;
   end To_Vxworks_Priority;

   function To_Posix_Priority (Priority : in int) return int is
   begin
      return SCHED_FIFO_LOW_PRI - Priority;
   end To_Posix_Priority;

   ----------------------------------------
   --  Implementation of POSIX routines  --
   ----------------------------------------

   -----------------------------------------
   --  Nonstandard Thread Initialization  --
   -----------------------------------------

   procedure pthread_init is
   begin
      Keys_Created := 0;
      Time_Slice := -1;
   end pthread_init;

   ---------------------------
   --  POSIX.1c  Section 3  --
   ---------------------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal) return int
   is
      Result  : Interfaces.C.int;

      function sigwaitinfo
        (set : access sigset_t; sigvalue : System.Address) return int;
      pragma Import (C, sigwaitinfo, "sigwaitinfo");

   begin
      Result := sigwaitinfo (set, System.Null_Address);

      if Result /= -1 then
         sig.all := Signal (Result);
         return 0;
      else
         sig.all := 0;
         return errno;
      end if;
   end sigwait;

   ----------------------------
   --  POSIX.1c  Section 11  --
   ----------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int is
   begin
      --  Let's take advantage of VxWorks priority inversion
      --  protection.
      --
      --  ??? - Do we want to also specify SEM_DELETE_SAFE???

      attr.Flags := int (SEM_Q_PRIORITY + SEM_INVERSION_SAFE);

      --  Initialize the ceiling priority to the maximim priority.
      --  We will use POSIX priorities since these routines are
      --  emulating POSIX routines.

      attr.Prio_Ceiling := POSIX_SCHED_FIFO_HIGH_PRI;
      attr.Protocol := PTHREAD_PRIO_INHERIT;
      return 0;
   end pthread_mutexattr_init;

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int is
   begin
      attr.Flags := 0;
      attr.Prio_Ceiling := POSIX_SCHED_FIFO_HIGH_PRI;
      attr.Protocol := PTHREAD_PRIO_INHERIT;
      return 0;
   end pthread_mutexattr_destroy;

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int
   is
      Result : int := 0;

   begin
      --  A mutex should initially be created full and the task
      --  protected from deletion while holding the semaphore.

      mutex.Mutex := semMCreate (attr.Flags);
      mutex.Prio_Ceiling := attr.Prio_Ceiling;
      mutex.Protocol := attr.Protocol;

      if mutex.Mutex = 0 then
         Result := errno;
      end if;

      return Result;
   end pthread_mutex_init;

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t) return int
   is
      Result : STATUS;
   begin
      Result := semDelete (mutex.Mutex);

      if Result /= 0 then
         Result := errno;
      end if;

      mutex.Mutex := 0;  --  Ensure the mutex is properly cleaned.
      mutex.Prio_Ceiling := POSIX_SCHED_FIFO_HIGH_PRI;
      mutex.Protocol := PTHREAD_PRIO_INHERIT;
      return Result;
   end pthread_mutex_destroy;

   function pthread_mutex_lock
     (mutex : access pthread_mutex_t) return int
   is
      Result    : int;
      WTCB_Ptr  : Wind_TCB_Ptr;
   begin
      WTCB_Ptr := To_Wind_TCB_Ptr (taskIdSelf);

      if WTCB_Ptr = null then
         return errno;
      end if;

      --  Check the current inherited priority in the WIND_TCB
      --  against the mutex ceiling priority and return EINVAL
      --  upon a ceiling violation.
      --
      --  We always convert the VxWorks priority to POSIX priority
      --  in case the current priority ordering has changed (see
      --  posixPriorityNumbering).  The mutex ceiling priority is
      --  maintained as POSIX compatible.

      if mutex.Protocol = PTHREAD_PRIO_PROTECT and then
         To_Posix_Priority (WTCB_Ptr.Priority) > mutex.Prio_Ceiling
      then
         return EINVAL;
      end if;

      Result := semTake (mutex.Mutex, WAIT_FOREVER);

      if Result /= 0 then
         Result := errno;
      end if;

      return Result;
   end pthread_mutex_lock;

   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t) return int
   is
      Result : int;
   begin
      Result := semGive (mutex.Mutex);

      if Result /= 0 then
         Result := errno;
      end if;

      return Result;
   end pthread_mutex_unlock;

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int is
   begin
      attr.Flags := SEM_Q_PRIORITY;
      return 0;
   end pthread_condattr_init;

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int is
   begin
      attr.Flags := 0;
      return 0;
   end pthread_condattr_destroy;

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int
   is
      Result  : int := 0;

   begin
      --  Condition variables should be initially created
      --  empty.

      cond.Sem := semBCreate (attr.Flags, SEM_EMPTY);
      cond.Waiting := 0;

      if cond.Sem = 0 then
         Result := errno;
      end if;

      return Result;
   end pthread_cond_init;

   function pthread_cond_destroy (cond : access pthread_cond_t) return int is
      Result : int;

   begin
      Result := semDelete (cond.Sem);

      if Result /= 0 then
         Result := errno;
      end if;

      return Result;
   end pthread_cond_destroy;

   function pthread_cond_signal
     (cond : access pthread_cond_t) return int
   is
      Result : int := 0;
      Status : int;

   begin
      --  Disable task scheduling.

      Status := taskLock;

      --  Iff someone is currently waiting on the condition variable
      --  then release the semaphore; we don't want to leave the
      --  semaphore in the full state because the next guy to do
      --  a condition wait operation would not block.

      if cond.Waiting > 0 then
         Result := semGive (cond.Sem);

         --  One less thread waiting on the CV.

         cond.Waiting := cond.Waiting - 1;

         if Result /= 0 then
            Result := errno;
         end if;
      end if;

      --  Reenable task scheduling.

      Status := taskUnlock;

      return Result;
   end pthread_cond_signal;

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int
   is
      Result : int;
      Status : int;
   begin
      --  Disable task scheduling.

      Status := taskLock;

      --  Release the mutex as required by POSIX.

      Result := semGive (mutex.Mutex);

      --  Indicate that there is another thread waiting on the CV.

      cond.Waiting := cond.Waiting + 1;

      --  Perform a blocking operation to take the CV semaphore.
      --  Note that a blocking operation in VxWorks will reenable
      --  task scheduling.  When we are no longer blocked and control
      --  is returned, task scheduling will again be disabled.

      Result := semTake (cond.Sem, WAIT_FOREVER);

      if Result /= 0 then
         cond.Waiting := cond.Waiting - 1;
         Result := EINVAL;
      end if;

      --  Take the mutex as required by POSIX.

      Status := semTake (mutex.Mutex, WAIT_FOREVER);

      if Status /= 0 then
         Result := EINVAL;
      end if;

      --  Reenable task scheduling.

      Status := taskUnlock;

      return Result;
   end pthread_cond_wait;

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return int
   is
      Result  : int;
      Status  : int;
      Ticks   : int;
      TS      : aliased timespec;
   begin
      Status := clock_gettime (CLOCK_REALTIME, TS'Unchecked_Access);

      --  Calculate the number of clock ticks for the timeout.

      Ticks := To_Clock_Ticks (To_Duration (abstime.all) - To_Duration (TS));

      if Ticks <= 0 then
         --  It is not worth the time to try to perform a semTake,
         --  because we know it will always fail.  A semTake with
         --  ticks = 0 (NO_WAIT) will not block and therefore not
         --  allow another task to give the semaphore.  And if we've
         --  designed pthread_cond_signal correctly, the semaphore
         --  should never be left in a full state.
         --
         --  Make sure we give up the CPU.

         Status := taskDelay (0);
         return ETIMEDOUT;
      end if;

      --  Disable task scheduling.

      Status := taskLock;

      --  Release the mutex as required by POSIX.

      Result := semGive (mutex.Mutex);

      --  Indicate that there is another thread waiting on the CV.

      cond.Waiting := cond.Waiting + 1;

      --  Perform a blocking operation to take the CV semaphore.
      --  Note that a blocking operation in VxWorks will reenable
      --  task scheduling.  When we are no longer blocked and control
      --  is returned, task scheduling will again be disabled.

      Result := semTake (cond.Sem, Ticks);

      if Result /= 0 then
         if errno = S_objLib_OBJ_TIMEOUT then
            Result := ETIMEDOUT;
         else
            Result := EINVAL;
         end if;
         cond.Waiting := cond.Waiting - 1;
      end if;

      --  Take the mutex as required by POSIX.

      Status := semTake (mutex.Mutex, WAIT_FOREVER);

      if Status /= 0 then
         Result := EINVAL;
      end if;

      --  Reenable task scheduling.

      Status := taskUnlock;

      return Result;
   end pthread_cond_timedwait;

   ----------------------------
   --  POSIX.1c  Section 13  --
   ----------------------------

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int is
   begin
      if protocol < PTHREAD_PRIO_NONE
        or protocol > PTHREAD_PRIO_PROTECT
      then
         return EINVAL;
      end if;

      attr.Protocol := protocol;
      return 0;
   end pthread_mutexattr_setprotocol;

   function pthread_mutexattr_setprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : int) return int is
   begin
      --  Our interface to the rest of the world is meant
      --  to be POSIX compliant; keep the priority in POSIX
      --  format.

      attr.Prio_Ceiling := prioceiling;
      return 0;
   end pthread_mutexattr_setprioceiling;

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int
   is
      Result : int;
   begin
      --  Convert the POSIX priority to VxWorks native
      --  priority.

      Result := taskPrioritySet (thread,
        To_Vxworks_Priority (param.sched_priority));
      return 0;
   end pthread_setschedparam;

   function sched_yield return int is
   begin
      return taskDelay (0);
   end sched_yield;

   function pthread_sched_rr_set_interval (usecs : int) return int is
      Result  : int := 0;
      D_Slice : Duration;
   begin
      --  Check to see if round-robin scheduling (time slicing)
      --  is enabled.  If the time slice is the default value (-1)
      --  or any negative number, we will leave the kernel time
      --  slice unchanged.  If the time slice is 0, we disable
      --  kernel time slicing by setting it to 0.  Otherwise, we
      --  set the kernel time slice to the specified value converted
      --  to clock ticks.

      Time_Slice := usecs;

      if Time_Slice > 0 then
         D_Slice := Duration (Time_Slice) / Duration (1_000_000.0);
         Result := kernelTimeSlice (To_Clock_Ticks (D_Slice));

      else
         if Time_Slice = 0 then
            Result := kernelTimeSlice (0);
         end if;
      end if;

      return Result;
   end pthread_sched_rr_set_interval;

   function pthread_attr_init (attr : access pthread_attr_t) return int is
   begin
      attr.Stacksize := 100000;   -- What else can I do?
      attr.Detachstate := PTHREAD_CREATE_DETACHED;
      attr.Priority := POSIX_SCHED_FIFO_LOW_PRI;
      attr.Taskname := System.Null_Address;
      return 0;
   end pthread_attr_init;

   function pthread_attr_destroy (attr : access pthread_attr_t) return int is
   begin
      attr.Stacksize := 0;
      attr.Detachstate := 0;
      attr.Priority := POSIX_SCHED_FIFO_LOW_PRI;
      attr.Taskname := System.Null_Address;
      return 0;
   end pthread_attr_destroy;

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int) return int is
   begin
      attr.Detachstate := detachstate;
      return 0;
   end pthread_attr_setdetachstate;

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int is
   begin
      attr.Stacksize := stacksize;
      return 0;
   end pthread_attr_setstacksize;

   --  In VxWorks tasks, we can set the task name.  This
   --  makes it really convenient for debugging.

   function pthread_attr_setname_np
     (attr : access pthread_attr_t;
      name : System.Address) return int is
   begin
      attr.Taskname := name;
      return 0;
   end pthread_attr_setname_np;

   function pthread_create
     (thread        : access pthread_t;
      attr          : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address) return int is
   begin
      thread.all := taskSpawn (attr.Taskname,
        To_Vxworks_Priority (attr.Priority), VX_FP_TASK, attr.Stacksize,
        start_routine, arg);

      if thread.all = -1 then
         return -1;
      else
         return 0;
      end if;
   end pthread_create;

   function pthread_detach (thread : pthread_t) return int is
   begin
      return 0;
   end pthread_detach;

   procedure pthread_exit (status : System.Address) is
   begin
      taskDelete (0);
   end pthread_exit;

   function pthread_self return pthread_t is
   begin
      return taskIdSelf;
   end pthread_self;

   function pthread_equal (t1 : pthread_t; t2 : pthread_t) return int is
   begin
      if t1 = t2 then
         return 1;
      else
         return 0;
      end if;
   end pthread_equal;

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address) return int
   is
      Result : int;
   begin
      if Integer (key) not in Key_Storage'Range then
         return EINVAL;
      end if;

      Key_Storage (Integer (key)) := value;
      Result := taskVarAdd (taskIdSelf, Key_Storage (Integer (key))'Access);

      --  We should be able to directly set the key with the following:
      --     Key_Storage (key) := value;
      --  but we'll be safe and use taskVarSet.
      --  ??? Come back and revisit this.

      Result := taskVarSet (taskIdSelf,
        Key_Storage (Integer (key))'Access, value);
      return Result;
   end pthread_setspecific;

   function pthread_getspecific (key : pthread_key_t) return System.Address is
   begin
      return Key_Storage (Integer (key));
   end pthread_getspecific;

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int is
   begin
      Keys_Created := Keys_Created + 1;

      if Keys_Created not in Key_Storage'Range then
         return ENOMEM;
      end if;

      key.all := pthread_key_t (Keys_Created);
      return 0;
   end pthread_key_create;

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

      --  If F has negative value due to a round-up, adjust for positive F
      --  value.
      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;
      return timespec' (ts_sec => S,
        ts_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   --------------------
   -- To_Clock_Ticks --
   --------------------

   --  ??? - For now, we'll always get the system clock rate
   --  since it is allowed to be changed during run-time in
   --  VxWorks.  A better method would be to provide an operation
   --  to set it that so we can always know its value.
   --
   --  Another thing we should probably allow for is a resultant
   --  tick count greater than int'Last.  This should probably
   --  be a procedure with two output parameters, one in the
   --  range 0 .. int'Last, and another representing the overflow
   --  count.

   function To_Clock_Ticks (D : Duration) return int is
      Ticks          : Long_Long_Integer;
      Rate_Duration  : Duration;
      Ticks_Duration : Duration;
   begin

      --  Ensure that the duration can be converted to ticks
      --  at the current clock tick rate without overflowing.

      Rate_Duration := Duration (sysClkRateGet);

      if D > (Duration'Last / Rate_Duration) then
         Ticks := Long_Long_Integer (int'Last);

      else
         --  We always want to round up to the nearest clock tick.

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

end System.OS_Interface;
