------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                    S Y S T E M . O S _ I N T E R F A C E                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                             $Revision: 1.2 $
--                                                                          --
--           Copyright (C) 1997-2001 Free Software Foundation, Inc.         --
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

--  This is the VxWorks version of this package.
--
--  VxWorks does not directly support the needed POSIX routines, but it
--  does have other routines that make it possible to code equivalent
--  POSIX compliant routines.  The approach taken is to provide an
--  FSU threads compliant interface.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
with System.VxWorks;
package System.OS_Interface is
   pragma Preelaborate;

   subtype int            is Interfaces.C.int;
   subtype short          is Interfaces.C.short;
   subtype long           is Interfaces.C.long;
   subtype unsigned       is Interfaces.C.unsigned;
   subtype unsigned_short is Interfaces.C.unsigned_short;
   subtype unsigned_long  is Interfaces.C.unsigned_long;
   subtype unsigned_char  is Interfaces.C.unsigned_char;
   subtype plain_char     is Interfaces.C.plain_char;
   subtype size_t         is Interfaces.C.size_t;
   subtype char           is Interfaces.C.char;

   -----------
   -- Errno --
   -----------

   function errno return int;
   pragma Import (C, errno, "errnoGet");

   EINTR     : constant := 4;
   EAGAIN    : constant := 35;
   ENOMEM    : constant := 12;
   EINVAL    : constant := 22;
   ETIMEDOUT : constant := 60;

   FUNC_ERR  : constant := -1;

   ----------------------------
   -- Signals and Interrupts --
   ----------------------------

   --  In order to support both signal and hardware interrupt handling,
   --  the ranges of "interrupt IDs" for the vectored hardware interrupts
   --  and the signals are catenated. In other words, the external IDs
   --  used to designate signals are relocated beyond the range of the
   --  vectored interrupts. The IDs given in Ada.Interrupts.Names should
   --  be used to designate signals; vectored interrupts are designated
   --  by their interrupt number.

   NSIG : constant := 32;
   --  Number of signals on the target OS
   type Signal is new int range 0 .. Interfaces.C."-" (NSIG, 1);

   Max_HW_Interrupt : constant := System.VxWorks.Num_HW_Interrupts - 1;
   type HW_Interrupt is new int range 0 .. Max_HW_Interrupt;

   Max_Interrupt : constant := Max_HW_Interrupt + NSIG;

   SIGILL  : constant :=  4; --  illegal instruction (not reset)
   SIGABRT : constant :=  6; --  used by abort, replace SIGIOT in the future
   SIGFPE  : constant :=  8; --  floating point exception
   SIGBUS  : constant := 10; --  bus error
   SIGSEGV : constant := 11; --  segmentation violation

   -----------------------------------
   -- Signal processing definitions --
   -----------------------------------

   --  The how in sigprocmask().
   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   --  The sa_flags in struct sigaction.
   SA_SIGINFO : constant := 16#0002#;
   SA_ONSTACK : constant := 16#0004#;

   --  ANSI args and returns from signal().
   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;

   type sigset_t is private;

   type struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   function sigaddset (set : access sigset_t; sig : Signal) return int;
   pragma Import (C, sigaddset, "sigaddset");

   function sigdelset (set : access sigset_t; sig : Signal) return int;
   pragma Import (C, sigdelset, "sigdelset");

   function sigfillset (set : access sigset_t) return int;
   pragma Import (C, sigfillset, "sigfillset");

   function sigismember (set : access sigset_t; sig : Signal) return int;
   pragma Import (C, sigismember, "sigismember");

   function sigemptyset (set : access sigset_t) return int;
   pragma Import (C, sigemptyset, "sigemptyset");

   function sigaction
     (sig  : Signal;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   type isr_address is access procedure (sig : int);

   function c_signal (sig : Signal; handler : isr_address) return isr_address;
   pragma Import (C, c_signal, "signal");

   function sigwait (set : access sigset_t; sig : access Signal) return int;
   pragma Inline (sigwait);

   type sigset_t_ptr is access all sigset_t;

   function pthread_sigmask
     (how  : int;
      set  : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   pragma Import (C, pthread_sigmask, "sigprocmask");

   ----------
   -- Time --
   ----------

   type time_t is new unsigned_long;

   type timespec is record
      ts_sec  : time_t;
      ts_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is private;

   CLOCK_REALTIME : constant clockid_t;   --  System wide realtime clock

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   function To_Clock_Ticks (D : Duration) return int;
   --  Convert a duration value (in seconds) into clock ticks.

   function clock_gettime
     (clock_id : clockid_t; tp : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   -------------------------
   -- Priority Scheduling --
   -------------------------

   --  Scheduling policies.
   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;
   SCHED_OTHER : constant := 4;

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;

   type pthread_t           is private;
   subtype Thread_Id        is pthread_t;

   null_pthread : constant pthread_t;

   type pthread_mutex_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   PTHREAD_CREATE_DETACHED : constant := 0;
   PTHREAD_CREATE_JOINABLE : constant := 1;

   function kill (pid : pthread_t; sig : Signal) return int;
   pragma Import (C, kill, "kill");

   --  VxWorks doesn't have getpid; taskIdSelf is the equivalent
   --  routine.
   function getpid return pthread_t;
   pragma Import (C, getpid, "taskIdSelf");

   ---------------------------------
   -- Nonstandard Thread Routines --
   ---------------------------------

   procedure pthread_init;
   pragma Inline (pthread_init);
   --  Vxworks requires this for the moment.

   function taskIdSelf return pthread_t;
   pragma Import (C, taskIdSelf, "taskIdSelf");

   function taskSuspend (tid : pthread_t) return int;
   pragma Import (C, taskSuspend, "taskSuspend");

   function taskResume (tid : pthread_t) return int;
   pragma Import (C, taskResume, "taskResume");

   function taskIsSuspended (tid : pthread_t) return int;
   pragma Import (C, taskIsSuspended, "taskIsSuspended");

   function taskVarAdd
     (tid  : pthread_t;
      pVar : access System.Address) return int;
   pragma Import (C, taskVarAdd, "taskVarAdd");

   function taskVarDelete
     (tid  : pthread_t;
      pVar : access System.Address) return int;
   pragma Import (C, taskVarDelete, "taskVarDelete");

   function taskVarSet
     (tid   : pthread_t;
      pVar  : access System.Address;
      value : System.Address) return int;
   pragma Import (C, taskVarSet, "taskVarSet");

   function taskVarGet
     (tid   : pthread_t;
      pVar  : access System.Address) return int;
   pragma Import (C, taskVarGet, "taskVarGet");

   function taskInfoGet
     (tid       : pthread_t;
      pTaskDesc : access System.VxWorks.TASK_DESC) return int;
   pragma Import (C, taskInfoGet, "taskInfoGet");

   function taskDelay (ticks : int) return int;
   pragma Import (C, taskDelay, "taskDelay");

   function sysClkRateGet return int;
   pragma Import (C, sysClkRateGet, "sysClkRateGet");

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int;
   pragma Inline (pthread_mutexattr_init);

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int;
   pragma Inline (pthread_mutexattr_destroy);

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int;
   pragma Inline (pthread_mutex_init);

   function pthread_mutex_destroy (mutex : access pthread_mutex_t) return int;
   pragma Inline (pthread_mutex_destroy);

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int;
   pragma Inline (pthread_mutex_lock);

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return int;
   pragma Inline (pthread_mutex_unlock);

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int;
   pragma Inline (pthread_condattr_init);

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int;
   pragma Inline (pthread_condattr_destroy);

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int;
   pragma Inline (pthread_cond_init);

   function pthread_cond_destroy (cond : access pthread_cond_t) return int;
   pragma Inline (pthread_cond_destroy);

   function pthread_cond_signal (cond : access pthread_cond_t) return int;
   pragma Inline (pthread_cond_signal);

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int;
   pragma Inline (pthread_cond_wait);

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return int;
   pragma Inline (pthread_cond_timedwait);

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   PTHREAD_PRIO_NONE    : constant := 0;
   PTHREAD_PRIO_PROTECT : constant := 2;
   PTHREAD_PRIO_INHERIT : constant := 1;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Inline (pthread_mutexattr_setprotocol);

   function pthread_mutexattr_setprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Inline (pthread_mutexattr_setprioceiling);

   type struct_sched_param is record
      sched_priority : int;
   end record;

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int;
   pragma Inline (pthread_setschedparam);

   function sched_yield return int;
   pragma Inline (sched_yield);

   function pthread_sched_rr_set_interval (usecs : int) return int;
   pragma Inline (pthread_sched_rr_set_interval);

   ---------------------------
   -- P1003.1c - Section 16 --
   ---------------------------

   function pthread_attr_init (attr : access pthread_attr_t) return int;
   pragma Inline (pthread_attr_init);

   function pthread_attr_destroy (attr : access pthread_attr_t) return int;
   pragma Inline (pthread_attr_destroy);

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int) return int;
   pragma Inline (pthread_attr_setdetachstate);

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int;
   pragma Inline (pthread_attr_setstacksize);

   function pthread_attr_setname_np
     (attr : access pthread_attr_t;
      name : System.Address) return int;
   --  In VxWorks tasks, we have a non-portable routine to set the
   --  task name. This makes it really convenient for debugging.
   pragma Inline (pthread_attr_setname_np);

   function pthread_create
     (thread        : access pthread_t;
      attr          : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address) return int;
   pragma Inline (pthread_create);

   function pthread_detach (thread : pthread_t) return int;
   pragma Inline (pthread_detach);

   procedure pthread_exit (status : System.Address);
   pragma Inline (pthread_exit);

   function pthread_self return pthread_t;
   pragma Inline (pthread_self);

   function pthread_equal (t1 : pthread_t; t2 : pthread_t) return int;
   pragma Inline (pthread_equal);
   --  be careful not to use "=" on thread_t!

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address) return int;
   pragma Inline (pthread_setspecific);

   function pthread_getspecific (key : pthread_key_t) return System.Address;
   pragma Inline (pthread_getspecific);

   type destructor_pointer is access procedure (arg : System.Address);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;
   pragma Inline (pthread_key_create);

   --  VxWorks binary semaphores. These are exported for use by the
   --  implementation of hardware interrupt handling.

   subtype STATUS is int;
   --  Equivalent of the C type STATUS

   OK    : constant STATUS := 0;
   ERROR : constant STATUS := Interfaces.C."-" (1);

   --  Semaphore creation flags.

   SEM_Q_FIFO         : constant := 0;
   SEM_Q_PRIORITY     : constant := 1;
   SEM_DELETE_SAFE    : constant := 4;  -- only valid for binary semaphore
   SEM_INVERSION_SAFE : constant := 8;  -- only valid for binary semaphore

   --  Semaphore initial state flags;

   SEM_EMPTY : constant := 0;
   SEM_FULL  : constant := 1;

   --  Semaphore take (semTake) time constants.

   WAIT_FOREVER : constant := -1;
   NO_WAIT      : constant := 0;

   type SEM_ID is new long;
   --  The VxWorks semaphore ID is an integer which is really just
   --  a pointer to a semaphore structure.

   function semBCreate (Options : int; Initial_State : int) return SEM_ID;
   --  Create a binary semaphore.  Returns ID, or 0 if memory could not
   --  be allocated
   pragma Import (C, semBCreate, "semBCreate");

   function semTake (SemID : SEM_ID; Timeout : int) return STATUS;
   --  Attempt to take binary semaphore.  Error is returned if operation
   --  times out
   pragma Import (C, semTake, "semTake");

   function semGive (SemID : SEM_ID) return STATUS;
   --  Release one thread blocked on the semaphore
   pragma Import (C, semGive, "semGive");

   function semFlush (SemID : SEM_ID) return STATUS;
   --  Release all threads blocked on the semaphore
   pragma Import (C, semFlush, "semFlush");

   function semDelete (SemID : SEM_ID) return STATUS;
   --  Delete a semaphore
   pragma Import (C, semDelete, "semDelete");


private
   --  This interface assumes that "unsigned" and "int" are 32-bit entities.

   type sigset_t is new long;

   type pid_t is new int;

   ERROR_PID : constant pid_t := -1;

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 0;

   --  Priority ceilings are now implemented in the body of
   --  this package.

   type pthread_mutexattr_t is record
      Flags        : int;   --  mutex semaphore creation flags
      Prio_Ceiling : int;   --  priority ceiling
      Protocol     : int;
   end record;

   type pthread_mutex_t is record
      Mutex        : SEM_ID;
      Protocol     : int;
      Prio_Ceiling : int;  --  priority ceiling of lock
   end record;

   type pthread_condattr_t is record
      Flags : int;
   end record;

   type pthread_cond_t is record
      Sem     : SEM_ID;   --  VxWorks semaphore ID
      Waiting : Integer;  --  Number of queued tasks waiting
   end record;

   type pthread_attr_t is record
      Stacksize   : size_t;
      Detachstate : int;
      Priority    : int;
      Taskname    : System.Address;
   end record;

   type pthread_t is new long;

   null_pthread : constant pthread_t := 0;

   type pthread_key_t is new int;

   --  These are to store the pthread_keys that are created with
   --  pthread_key_create.  Currently, we only need one key.

   Key_Storage  : array (1 .. 10) of aliased System.Address;
   Keys_Created : Integer;

   Time_Slice : int;

end System.OS_Interface;
