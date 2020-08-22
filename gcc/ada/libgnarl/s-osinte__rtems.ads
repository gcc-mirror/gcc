------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--          Copyright (C) 1997-2020, Free Software Foundation, Inc.         --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- The GNARL files that were developed for RTEMS are maintained by  On-Line --
-- Applications Research Corporation (http://www.oarcorp.com)  in  coopera- --
-- tion with Ada Core Technologies Inc. and Florida State University.       --
--                                                                          --
------------------------------------------------------------------------------

--  This is the RTEMS version of this package.
--
--  RTEMS target names are of the form CPU-rtems.
--  This implementation is designed to work on ALL RTEMS targets.
--  The RTEMS implementation is primarily based upon the POSIX threads
--  API but there are also bindings to GNAT/RTEMS support routines
--  to insulate this code from C API specific details and, in some
--  cases, obtain target architecture and BSP specific information
--  that is unavailable at the time this package is built.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Preelaborate.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
with System.OS_Constants;

package System.OS_Interface is
   pragma Preelaborate;

   --  This interface assumes that "unsigned" is a 32-bit entity.  This
   --  will correspond to RTEMS object ids.

   subtype rtems_id       is Interfaces.C.unsigned;

   subtype int            is Interfaces.C.int;
   subtype char           is Interfaces.C.char;
   subtype short          is Interfaces.C.short;
   subtype long           is Interfaces.C.long;
   subtype unsigned       is Interfaces.C.unsigned;
   subtype unsigned_short is Interfaces.C.unsigned_short;
   subtype unsigned_long  is Interfaces.C.unsigned_long;
   subtype unsigned_char  is Interfaces.C.unsigned_char;
   subtype plain_char     is Interfaces.C.plain_char;
   subtype size_t         is Interfaces.C.size_t;
   -----------
   -- Errno --
   -----------

   function errno return int;
   pragma Import (C, errno, "__get_errno");

   EAGAIN    : constant := System.OS_Constants.EAGAIN;
   EINTR     : constant := System.OS_Constants.EINTR;
   EINVAL    : constant := System.OS_Constants.EINVAL;
   ENOMEM    : constant := System.OS_Constants.ENOMEM;
   ETIMEDOUT : constant := System.OS_Constants.ETIMEDOUT;

   -------------
   -- Signals --
   -------------

   Num_HW_Interrupts : constant := 256;

   Max_HW_Interrupt : constant := Num_HW_Interrupts - 1;
   type HW_Interrupt is new int range 0 .. Max_HW_Interrupt;

   Max_Interrupt : constant := Max_HW_Interrupt;

   type Signal is new int range 0 .. Max_Interrupt;

   SIGXCPU     : constant := 0; --  XCPU
   SIGHUP      : constant := 1; --  hangup
   SIGINT      : constant := 2; --  interrupt (rubout)
   SIGQUIT     : constant := 3; --  quit (ASCD FS)
   SIGILL      : constant := 4; --  illegal instruction (not reset)
   SIGTRAP     : constant := 5; --  trace trap (not reset)
   SIGIOT      : constant := 6; --  IOT instruction
   SIGABRT     : constant := 6; --  used by abort, replace SIGIOT in the future
   SIGEMT      : constant := 7; --  EMT instruction
   SIGFPE      : constant := 8; --  floating point exception
   SIGKILL     : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS      : constant := 10; --  bus error
   SIGSEGV     : constant := 11; --  segmentation violation
   SIGSYS      : constant := 12; --  bad argument to system call
   SIGPIPE     : constant := 13; --  write on a pipe with no one to read it
   SIGALRM     : constant := 14; --  alarm clock
   SIGTERM     : constant := 15; --  software termination signal from kill
   SIGUSR1     : constant := 16; --  user defined signal 1
   SIGUSR2     : constant := 17; --  user defined signal 2

   SIGADAABORT : constant := SIGABRT;

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked    : constant Signal_Set := (SIGTRAP, SIGALRM, SIGEMT);
   Reserved    : constant Signal_Set := (1 .. 1 => SIGKILL);

   type sigset_t is private;

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

   type struct_sigaction is record
      sa_flags   : int;
      sa_mask    : sigset_t;
      sa_handler : System.Address;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   SA_SIGINFO  : constant := 16#02#;

   SA_ONSTACK : constant := 16#00#;
   --  SA_ONSTACK is not defined on RTEMS, but it is referred to in the POSIX
   --  implementation of System.Interrupt_Management. Therefore we define a
   --  dummy value of zero here so that setting this flag is a nop.

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;

   function sigaction
     (sig  : Signal;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   ----------
   -- Time --
   ----------

   Time_Slice_Supported : constant Boolean := True;
   --  Indicates whether time slicing is supported (i.e SCHED_RR is supported)

   type timespec is private;

   type clockid_t is new int;

   CLOCK_REALTIME  : constant clockid_t;
   CLOCK_MONOTONIC : constant clockid_t;

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   function clock_getres
     (clock_id : clockid_t;
      res      : access timespec) return int;
   pragma Import (C, clock_getres, "clock_getres");

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;
   SCHED_OTHER : constant := 0;

   function To_Target_Priority
     (Prio : System.Any_Priority) return Interfaces.C.int;
   --  Maps System.Any_Priority to a POSIX priority

   -------------
   -- Process --
   -------------

   type pid_t is private;

   function kill (pid : pid_t; sig : Signal) return int;
   pragma Import (C, kill, "kill");

   function getpid return pid_t;
   pragma Import (C, getpid, "getpid");

   ---------
   -- LWP --
   ---------

   function lwp_self return System.Address;
   --  lwp_self does not exist on this thread library, revert to pthread_self
   --  which is the closest approximation (with getpid). This function is
   --  needed to share 7staprop.adb across POSIX-like targets.
   pragma Import (C, lwp_self, "pthread_self");

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;
   pragma Convention (C, Thread_Body);

   type pthread_t           is private;
   subtype Thread_Id        is pthread_t;

   type pthread_mutex_t      is limited private;
   type pthread_rwlock_t     is limited private;
   type pthread_cond_t       is limited private;
   type pthread_attr_t       is limited private;
   type pthread_mutexattr_t  is limited private;
   type pthread_rwlockattr_t is limited private;
   type pthread_condattr_t   is limited private;
   type pthread_key_t        is private;

   No_Key : constant pthread_key_t;

   PTHREAD_CREATE_DETACHED : constant := 0;

   PTHREAD_SCOPE_PROCESS : constant := 0;
   PTHREAD_SCOPE_SYSTEM  : constant := 1;

   -----------
   -- Stack --
   -----------

   type stack_t is record
      ss_sp    : System.Address;
      ss_flags : int;
      ss_size  : size_t;
   end record;
   pragma Convention (C, stack_t);

   function sigaltstack
     (ss  : not null access stack_t;
      oss : access stack_t) return int;

   Alternate_Stack : aliased System.Address;
   --  This is a dummy definition, never used (Alternate_Stack_Size is null)

   Alternate_Stack_Size : constant := 0;
   --  No alternate signal stack is used on this platform

   Stack_Base_Available : constant Boolean := False;
   --  Indicates whether the stack base is available on this target.
   --  This allows us to share s-osinte.adb between all the FSU/RTEMS
   --  run time.
   --  Note that this value can only be true if pthread_t has a complete
   --  definition that corresponds exactly to the C header files.

   function Get_Stack_Base (thread : pthread_t) return Address;
   pragma Inline (Get_Stack_Base);
   --  returns the stack base of the specified thread.
   --  Only call this function when Stack_Base_Available is True.

   --  These two functions are only needed to share s-taprop.adb with
   --  FSU threads.

   function Get_Page_Size return int;
   pragma Import (C, Get_Page_Size, "getpagesize");
   --  Returns the size of a page

   PROT_ON  : constant := 0;
   PROT_OFF : constant := 0;

   function mprotect (addr : Address; len : size_t; prot : int) return int;
   pragma Import (C, mprotect);

   -----------------------------------------
   --  Nonstandard Thread Initialization  --
   -----------------------------------------

   procedure pthread_init;
   --  FSU_THREADS requires pthread_init, which is nonstandard
   --  and this should be invoked during the elaboration of s-taprop.adb
   --
   --  RTEMS does not require this so we provide an empty Ada body.

   -------------------------
   -- POSIX.1c  Section 3 --
   -------------------------

   function sigwait
     (set : access sigset_t;
      sig : access Signal) return int;
   pragma Import (C, sigwait, "sigwait");

   function pthread_kill
     (thread : pthread_t;
      sig    : Signal) return int;
   pragma Import (C, pthread_kill, "pthread_kill");

   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t) return int;
   pragma Import (C, pthread_sigmask, "pthread_sigmask");

   ----------------------------
   --  POSIX.1c  Section 11  --
   ----------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_init, "pthread_mutexattr_init");

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_destroy, "pthread_mutexattr_destroy");

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutex_init, "pthread_mutex_init");

   function pthread_mutex_destroy (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_destroy, "pthread_mutex_destroy");

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_lock, "pthread_mutex_lock");

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_unlock, "pthread_mutex_unlock");

   function pthread_rwlockattr_init
     (attr : access pthread_rwlockattr_t) return int;
   pragma Import (C, pthread_rwlockattr_init, "pthread_rwlockattr_init");

   function pthread_rwlockattr_destroy
     (attr : access pthread_rwlockattr_t) return int;
   pragma Import (C, pthread_rwlockattr_destroy, "pthread_rwlockattr_destroy");

   PTHREAD_RWLOCK_PREFER_READER_NP              : constant := 0;
   PTHREAD_RWLOCK_PREFER_WRITER_NP              : constant := 1;
   PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP : constant := 2;

   function pthread_rwlockattr_setkind_np
     (attr : access pthread_rwlockattr_t;
      pref : int) return int;

   function pthread_rwlock_init
     (mutex : access pthread_rwlock_t;
      attr  : access pthread_rwlockattr_t) return int;
   pragma Import (C, pthread_rwlock_init, "pthread_rwlock_init");

   function pthread_rwlock_destroy
     (mutex : access pthread_rwlock_t) return int;
   pragma Import (C, pthread_rwlock_destroy, "pthread_rwlock_destroy");

   function pthread_rwlock_rdlock (mutex : access pthread_rwlock_t) return int;
   pragma Import (C, pthread_rwlock_rdlock, "pthread_rwlock_rdlock");

   function pthread_rwlock_wrlock (mutex : access pthread_rwlock_t) return int;
   pragma Import (C, pthread_rwlock_wrlock, "pthread_rwlock_wrlock");

   function pthread_rwlock_unlock (mutex : access pthread_rwlock_t) return int;
   pragma Import (C, pthread_rwlock_unlock, "pthread_rwlock_unlock");

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_condattr_init, "pthread_condattr_init");

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_condattr_destroy, "pthread_condattr_destroy");

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_cond_init, "pthread_cond_init");

   function pthread_cond_destroy (cond : access pthread_cond_t) return int;
   pragma Import (C, pthread_cond_destroy, "pthread_cond_destroy");

   function pthread_cond_signal (cond : access pthread_cond_t) return int;
   pragma Import (C, pthread_cond_signal, "pthread_cond_signal");

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_cond_wait, "pthread_cond_wait");

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return int;
   pragma Import (C, pthread_cond_timedwait, "pthread_cond_timedwait");

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   PTHREAD_PRIO_NONE    : constant := 0;
   PTHREAD_PRIO_PROTECT : constant := 2;
   PTHREAD_PRIO_INHERIT : constant := 1;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol);

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import
     (C, pthread_mutexattr_setprioceiling,
      "pthread_mutexattr_setprioceiling");

   type struct_sched_param is record
      sched_priority      : int;
      ss_low_priority     : int;
      ss_replenish_period : timespec;
      ss_initial_budget   : timespec;
      sched_ss_max_repl   : int;
   end record;
   pragma Convention (C, struct_sched_param);

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int;
   pragma Import (C, pthread_setschedparam, "pthread_setschedparam");

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int) return int;
   pragma Import (C, pthread_attr_setscope, "pthread_attr_setscope");

   function pthread_attr_setinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : int) return int;
   pragma Import (C, pthread_attr_setinheritsched);

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int) return int;
   pragma Import (C, pthread_attr_setschedpolicy);

   function pthread_attr_setschedparam
     (attr        : access pthread_attr_t;
      sched_param : int) return int;
   pragma Import (C, pthread_attr_setschedparam);

   function sched_yield return int;
   pragma Import (C, sched_yield, "sched_yield");

   ---------------------------
   -- P1003.1c - Section 16 --
   ---------------------------

   function pthread_attr_init (attributes : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_init, "pthread_attr_init");

   function pthread_attr_destroy
     (attributes : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_destroy, "pthread_attr_destroy");

   function pthread_attr_setdetachstate
     (attr        : access pthread_attr_t;
      detachstate : int) return int;
   pragma Import (C, pthread_attr_setdetachstate);

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int;
   pragma Import (C, pthread_attr_setstacksize, "pthread_attr_setstacksize");

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address) return int;
   pragma Import (C, pthread_create, "pthread_create");

   procedure pthread_exit (status : System.Address);
   pragma Import (C, pthread_exit, "pthread_exit");

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self, "pthread_self");

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address) return int;
   pragma Import (C, pthread_setspecific, "pthread_setspecific");

   function pthread_getspecific (key : pthread_key_t) return System.Address;
   pragma Import (C, pthread_getspecific, "pthread_getspecific");

   type destructor_pointer is access procedure (arg : System.Address);
   pragma Convention (C, destructor_pointer);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;
   pragma Import (C, pthread_key_create, "pthread_key_create");

   ------------------------------------------------------------
   --   Binary Semaphore Wrapper to Support Interrupt Tasks  --
   ------------------------------------------------------------

   type Binary_Semaphore_Id is new rtems_id;

   function Binary_Semaphore_Create return Binary_Semaphore_Id;
   pragma Import (
      C,
      Binary_Semaphore_Create,
      "__gnat_binary_semaphore_create");

   function Binary_Semaphore_Delete (ID : Binary_Semaphore_Id) return int;
   pragma Import (
      C,
      Binary_Semaphore_Delete,
      "__gnat_binary_semaphore_delete");

   function Binary_Semaphore_Obtain (ID : Binary_Semaphore_Id) return int;
   pragma Import (
      C,
      Binary_Semaphore_Obtain,
      "__gnat_binary_semaphore_obtain");

   function Binary_Semaphore_Release (ID : Binary_Semaphore_Id) return int;
   pragma Import (
      C,
      Binary_Semaphore_Release,
      "__gnat_binary_semaphore_release");

   function Binary_Semaphore_Flush (ID : Binary_Semaphore_Id) return int;
   pragma Import (
      C,
      Binary_Semaphore_Flush,
      "__gnat_binary_semaphore_flush");

   ------------------------------------------------------------
   -- Hardware Interrupt Wrappers to Support Interrupt Tasks --
   ------------------------------------------------------------

   type Interrupt_Handler is access procedure (parameter : System.Address);
   pragma Convention (C, Interrupt_Handler);
   type Interrupt_Vector is new System.Address;

   function Interrupt_Connect
     (vector    : Interrupt_Vector;
      handler   : Interrupt_Handler;
      parameter : System.Address := System.Null_Address) return int;
   pragma Import (C, Interrupt_Connect, "__gnat_interrupt_connect");
   --  Use this to set up an user handler. The routine installs a
   --  a user handler which is invoked after RTEMS has saved enough
   --  context for a high-level language routine to be safely invoked.

   function Interrupt_Vector_Get
     (Vector : Interrupt_Vector) return Interrupt_Handler;
   pragma Import (C, Interrupt_Vector_Get, "__gnat_interrupt_get");
   --  Use this to get the existing handler for later restoral.

   procedure Interrupt_Vector_Set
     (Vector  : Interrupt_Vector;
      Handler : Interrupt_Handler);
   pragma Import (C, Interrupt_Vector_Set, "__gnat_interrupt_set");
   --  Use this to restore a handler obtained using Interrupt_Vector_Get.

   function Interrupt_Number_To_Vector (intNum : int) return Interrupt_Vector;
   --  Convert a logical interrupt number to the hardware interrupt vector
   --  number used to connect the interrupt.
   pragma Import (
      C,
      Interrupt_Number_To_Vector,
      "__gnat_interrupt_number_to_vector"
   );

private

   type sigset_t is new int;

   type pid_t is new int;

   type time_t is new Long_Long_Integer;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   CLOCK_REALTIME :  constant clockid_t := System.OS_Constants.CLOCK_REALTIME;
   CLOCK_MONOTONIC : constant clockid_t := System.OS_Constants.CLOCK_MONOTONIC;

   subtype char_array is Interfaces.C.char_array;

   type pthread_attr_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_ATTR_SIZE);
   end record;
   pragma Convention (C, pthread_attr_t);
   for pthread_attr_t'Alignment use Interfaces.C.double'Alignment;

   type pthread_condattr_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_CONDATTR_SIZE);
   end record;
   pragma Convention (C, pthread_condattr_t);
   for pthread_condattr_t'Alignment use Interfaces.C.double'Alignment;

   type pthread_mutexattr_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_MUTEXATTR_SIZE);
   end  record;
   pragma Convention (C, pthread_mutexattr_t);
   for pthread_mutexattr_t'Alignment use Interfaces.C.int'Alignment;

   type pthread_rwlockattr_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_RWLOCKATTR_SIZE);
   end record;
   pragma Convention (C, pthread_rwlockattr_t);
   for pthread_rwlockattr_t'Alignment use Interfaces.C.int'Alignment;

   type pthread_t is new rtems_id;

   type pthread_mutex_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_MUTEX_SIZE);
   end record;
   pragma Convention (C, pthread_mutex_t);
   for pthread_mutex_t'Alignment use Interfaces.C.double'Alignment;

   type pthread_rwlock_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_RWLOCK_SIZE);
   end record;
   pragma Convention (C, pthread_rwlock_t);
   for pthread_rwlock_t'Alignment use Interfaces.C.size_t'Alignment;

   type pthread_cond_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_COND_SIZE);
   end record;
   pragma Convention (C, pthread_cond_t);
   for pthread_cond_t'Alignment use Interfaces.C.size_t'Alignment;

   type pthread_key_t is new rtems_id;

   No_Key : constant pthread_key_t := 0;

end System.OS_Interface;
