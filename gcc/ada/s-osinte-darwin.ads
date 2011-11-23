------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--          Copyright (C) 1995-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
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

--  This is Darwin pthreads version of this package

--  This package includes all direct interfaces to OS services that are needed
--  by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Elaborate_Body. It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
with System.OS_Constants;

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

   -----------
   -- Errno --
   -----------

   function errno return int;
   pragma Import (C, errno, "__get_errno");

   EINTR     : constant := 4;
   ENOMEM    : constant := 12;
   EINVAL    : constant := 22;
   EAGAIN    : constant := 35;
   ETIMEDOUT : constant := 60;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 31;
   type Signal is new int range 0 .. Max_Interrupt;
   for Signal'Size use int'Size;

   SIGHUP     : constant := 1; --  hangup
   SIGINT     : constant := 2; --  interrupt (rubout)
   SIGQUIT    : constant := 3; --  quit (ASCD FS)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGTRAP    : constant := 5; --  trace trap (not reset)
   SIGIOT     : constant := 6; --  IOT instruction
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the  future
   SIGEMT     : constant := 7; --  EMT instruction
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 10; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGSYS     : constant := 12; --  bad argument to system call
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGURG     : constant := 16; --  urgent condition on IO channel
   SIGSTOP    : constant := 17; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 18; --  user stop requested from tty
   SIGCONT    : constant := 19; --  stopped process has been continued
   SIGCHLD    : constant := 20; --  child status change
   SIGTTIN    : constant := 21; --  background tty read attempted
   SIGTTOU    : constant := 22; --  background tty write attempted
   SIGIO      : constant := 23; --  I/O possible (Solaris SIGPOLL alias)
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 27; --  profiling timer expired
   SIGWINCH   : constant := 28; --  window size change
   SIGINFO    : constant := 29; --  information request
   SIGUSR1    : constant := 30; --  user defined signal 1
   SIGUSR2    : constant := 31; --  user defined signal 2

   SIGADAABORT : constant := SIGABRT;
   --  Change this if you want to use another signal for task abort.
   --  SIGTERM might be a good one.

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked : constant Signal_Set :=
                (SIGTTIN, SIGTTOU, SIGSTOP, SIGTSTP);

   Reserved : constant Signal_Set :=
                (SIGKILL, SIGSTOP);

   Exception_Signals : constant Signal_Set :=
                         (SIGFPE, SIGILL, SIGSEGV, SIGBUS);
   --  These signals (when runtime or system) will be caught and converted
   --  into an Ada exception.

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

   type siginfo_t is private;
   type ucontext_t is private;

   type Signal_Handler is access procedure
     (signo   : Signal;
      info    : access siginfo_t;
      context : access ucontext_t);

   type struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;

   SA_SIGINFO : constant := 16#0040#;
   SA_ONSTACK : constant := 16#0001#;

   function sigaction
     (sig  : Signal;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   ----------
   -- Time --
   ----------

   Time_Slice_Supported : constant Boolean := True;
   --  Indicates whether time slicing is supported

   type timespec is private;

   type clockid_t is new int;

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec) return int;

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_OTHER : constant := 1;
   SCHED_RR    : constant := 2;
   SCHED_FIFO  : constant := 4;

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
   --  Return the mach thread bound to the current thread.  The value is not
   --  used by the run-time library but made available to debuggers.

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;
   pragma Convention (C, Thread_Body);

   type pthread_t           is private;
   subtype Thread_Id        is pthread_t;

   type pthread_mutex_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   type pthread_mutex_ptr is access all pthread_mutex_t;
   type pthread_cond_ptr is access all pthread_cond_t;

   PTHREAD_CREATE_DETACHED : constant := 2;

   PTHREAD_SCOPE_PROCESS : constant := 2;
   PTHREAD_SCOPE_SYSTEM  : constant := 1;

   --  Read/Write lock not supported on Darwin. To add support both types
   --  pthread_rwlock_t and pthread_rwlockattr_t must properly be defined
   --  with the associated routines pthread_rwlock_[init/destroy] and
   --  pthread_rwlock_[rdlock/wrlock/unlock].

   subtype pthread_rwlock_t     is pthread_mutex_t;
   subtype pthread_rwlockattr_t is pthread_mutexattr_t;

   -----------
   -- Stack --
   -----------

   type stack_t is record
      ss_sp    : System.Address;
      ss_size  : size_t;
      ss_flags : int;
   end record;
   pragma Convention (C, stack_t);

   function sigaltstack
     (ss  : not null access stack_t;
      oss : access stack_t) return int;
   pragma Import (C, sigaltstack, "sigaltstack");

   Alternate_Stack : aliased System.Address;
   pragma Import (C, Alternate_Stack, "__gnat_alternate_stack");
   --  The alternate signal stack for stack overflows

   Alternate_Stack_Size : constant := 32 * 1024;
   --  This must be in keeping with init.c:__gnat_alternate_stack

   Stack_Base_Available : constant Boolean := False;
   --  Indicates whether the stack base is available on this target. This
   --  allows us to share s-osinte.adb between all the FSU run time. Note that
   --  this value can only be true if pthread_t has a complete definition that
   --  corresponds exactly to the C header files.

   function Get_Stack_Base (thread : pthread_t) return System.Address;
   pragma Inline (Get_Stack_Base);
   --  returns the stack base of the specified thread. Only call this function
   --  when Stack_Base_Available is True.

   function Get_Page_Size return size_t;
   function Get_Page_Size return System.Address;
   pragma Import (C, Get_Page_Size, "getpagesize");
   --  Returns the size of a page

   PROT_NONE  : constant := 0;
   PROT_READ  : constant := 1;
   PROT_WRITE : constant := 2;
   PROT_EXEC  : constant := 4;
   PROT_ALL   : constant := PROT_READ + PROT_WRITE + PROT_EXEC;

   PROT_ON    : constant := PROT_NONE;
   PROT_OFF   : constant := PROT_ALL;

   function mprotect
     (addr : System.Address;
      len  : size_t;
      prot : int) return int;
   pragma Import (C, mprotect);

   ---------------------------------------
   -- Nonstandard Thread Initialization --
   ---------------------------------------

   procedure pthread_init;

   -------------------------
   -- POSIX.1c  Section 3 --
   -------------------------

   function sigwait (set : access sigset_t; sig : access Signal) return int;
   pragma Import (C, sigwait, "sigwait");

   function pthread_kill (thread : pthread_t; sig : Signal) return int;
   pragma Import (C, pthread_kill, "pthread_kill");

   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t) return int;
   pragma Import (C, pthread_sigmask, "pthread_sigmask");

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

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

   Relative_Timed_Wait : constant Boolean := False;
   --  pthread_cond_timedwait requires an absolute delay time

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   PTHREAD_PRIO_NONE    : constant := 0;
   PTHREAD_PRIO_INHERIT : constant := 1;
   PTHREAD_PRIO_PROTECT : constant := 2;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import
     (C, pthread_mutexattr_setprotocol, "pthread_mutexattr_setprotocol");

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import
     (C, pthread_mutexattr_setprioceiling,
      "pthread_mutexattr_setprioceiling");

   type padding is array (int range <>) of Interfaces.C.char;

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
      opaque         : padding (1 .. 4);
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
     (attr            : access pthread_attr_t;
      inheritsched : int) return int;
   pragma Import
     (C, pthread_attr_setinheritsched, "pthread_attr_setinheritsched");

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int) return int;
   pragma Import (C, pthread_attr_setschedpolicy, "pthread_attr_setsched");

   function sched_yield return int;

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
   pragma Import
     (C, pthread_attr_setdetachstate, "pthread_attr_setdetachstate");

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int;
   pragma Import
     (C, pthread_attr_setstacksize, "pthread_attr_setstacksize");

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

private

   type sigset_t is new unsigned;

   type int32_t is new int;

   type pid_t is new int32_t;

   type time_t is new long;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   --
   --  Darwin specific signal implementation
   --
   type Pad_Type is array (1 .. 7) of unsigned_long;
   type siginfo_t is record
      si_signo  : int;               --  signal number
      si_errno  : int;               --  errno association
      si_code   : int;               --  signal code
      si_pid    : int;               --  sending process
      si_uid    : unsigned;          --  sender's ruid
      si_status : int;               --  exit value
      si_addr   : System.Address;    --  faulting instruction
      si_value  : System.Address;    --  signal value
      si_band   : long;              --  band event for SIGPOLL
      pad       : Pad_Type;          --  RFU
   end record;
   pragma Convention (C, siginfo_t);

   type mcontext_t is new System.Address;

   type ucontext_t is record
      uc_onstack  : int;
      uc_sigmask  : sigset_t;         --  Signal Mask Used By This Context
      uc_stack    : stack_t;          --  Stack Used By This Context
      uc_link     : System.Address;   --  Pointer To Resuming Context
      uc_mcsize   : size_t;           --  Size of The Machine Context
      uc_mcontext : mcontext_t;       --  Machine Specific Context
   end record;
   pragma Convention (C, ucontext_t);

   --
   --  Darwin specific pthread implementation
   --
   type pthread_t is new System.Address;

   type pthread_attr_t is record
      sig    : long;
      opaque : padding (1 .. System.OS_Constants.PTHREAD_ATTR_SIZE);
   end record;
   pragma Convention (C, pthread_attr_t);

   type pthread_mutexattr_t is record
      sig    : long;
      opaque : padding (1 .. System.OS_Constants.PTHREAD_MUTEXATTR_SIZE);
   end record;
   pragma Convention (C, pthread_mutexattr_t);

   type pthread_mutex_t is record
      sig    : long;
      opaque : padding (1 .. System.OS_Constants.PTHREAD_MUTEX_SIZE);
   end record;
   pragma Convention (C, pthread_mutex_t);

   type pthread_condattr_t is record
      sig    : long;
      opaque : padding (1 .. System.OS_Constants.PTHREAD_CONDATTR_SIZE);
   end record;
   pragma Convention (C, pthread_condattr_t);

   type pthread_cond_t is record
      sig    : long;
      opaque : padding (1 .. System.OS_Constants.PTHREAD_COND_SIZE);
   end record;
   pragma Convention (C, pthread_cond_t);

   type pthread_once_t is record
      sig    : long;
      opaque : padding (1 .. System.OS_Constants.PTHREAD_ONCE_SIZE);
   end record;
   pragma Convention (C, pthread_once_t);

   type pthread_key_t is new unsigned_long;

end System.OS_Interface;
