------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--          Copyright (C) 1995-2018, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is the DragonFly BSD PTHREADS version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Ada.Unchecked_Conversion;

with Interfaces.C;

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-pthread");

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

   function Errno return int;
   pragma Inline (Errno);

   EAGAIN    : constant := 35;
   EINTR     : constant := 4;
   EINVAL    : constant := 22;
   ENOMEM    : constant := 12;
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
   SIGCLD     : constant := 20; --  alias for SIGCHLD
   SIGCHLD    : constant := 20; --  child status change
   SIGTTIN    : constant := 21; --  background tty read attempted
   SIGTTOU    : constant := 22; --  background tty write attempted
   SIGIO      : constant := 23; --  I/O possible (Solaris SIGPOLL alias)
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 27; --  profiling timer expired
   SIGWINCH   : constant := 28; --  window size change
   SIGINFO    : constant := 29; --  information request (BSD)
   SIGUSR1    : constant := 30; --  user defined signal 1
   SIGUSR2    : constant := 31; --  user defined signal 2

   SIGADAABORT : constant := SIGABRT;
   --  Change this if you want to use another signal for task abort.
   --  SIGTERM might be a good one.

   type Signal_Set is array (Natural range <>) of Signal;

   --  Interrupts that must be unmasked at all times.  DragonFlyBSD
   --  pthreads will not allow an application to mask out any
   --  interrupt needed by the threads library.
   Unmasked : constant Signal_Set :=
     (SIGTRAP, SIGBUS, SIGTTIN, SIGTTOU, SIGTSTP);

   --  DragonFlyBSD will uses SIGPROF for timing.  Do not allow a
   --  handler to attach to this signal.
   Reserved : constant Signal_Set := (0 .. 0 => SIGPROF);

   type sigset_t is private;

   function sigaddset
     (set : access sigset_t;
      sig : Signal) return int;
   pragma Import (C, sigaddset, "sigaddset");

   function sigdelset
     (set : access sigset_t;
      sig : Signal) return int;
   pragma Import (C, sigdelset, "sigdelset");

   function sigfillset (set : access sigset_t) return int;
   pragma Import (C, sigfillset, "sigfillset");

   function sigismember
     (set : access sigset_t;
      sig : Signal) return int;
   pragma Import (C, sigismember, "sigismember");

   function sigemptyset (set : access sigset_t) return int;
   pragma Import (C, sigemptyset, "sigemptyset");

   --  sigcontext is architecture dependent, so define it private
   type struct_sigcontext is private;

   type old_struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, old_struct_sigaction);

   type new_struct_sigaction is record
      sa_handler : System.Address;
      sa_flags   : int;
      sa_mask    : sigset_t;
   end record;
   pragma Convention (C, new_struct_sigaction);

   subtype struct_sigaction is new_struct_sigaction;
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
   --  Indicates whether time slicing is supported (i.e SCHED_RR is supported)

   type timespec is private;

   function nanosleep (rqtp, rmtp : access timespec)  return int;
   pragma Import (C, nanosleep, "nanosleep");

   type clockid_t is new unsigned_long;

   function clock_getres
     (clock_id : clockid_t;
      res      : access timespec) return int;
   pragma Import (C, clock_getres, "clock_getres");

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec)
      return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   type struct_timezone is record
      tz_minuteswest : int;
      tz_dsttime     : int;
   end record;
   pragma Convention (C, struct_timezone);

   procedure usleep (useconds : unsigned_long);
   pragma Import (C, usleep, "usleep");

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 1;
   SCHED_OTHER : constant := 2;
   SCHED_RR    : constant := 3;

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

   function Thread_Body_Access is new
     Ada.Unchecked_Conversion (System.Address, Thread_Body);

   type pthread_t           is private;
   subtype Thread_Id        is pthread_t;

   type pthread_mutex_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   PTHREAD_CREATE_DETACHED : constant := 1;
   PTHREAD_CREATE_JOINABLE : constant := 0;

   PTHREAD_SCOPE_PROCESS : constant := 0;
   PTHREAD_SCOPE_SYSTEM  : constant := 2;

   --  Read/Write lock not supported on DragonFly. To add support both types
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
   --  This is a dummy definition, never used (Alternate_Stack_Size is null)

   Alternate_Stack_Size : constant := 0;
   --  No alternate signal stack is used on this platform

   Stack_Base_Available : constant Boolean := False;
   --  Indicates whether the stack base is available on this target. This
   --  allows us to share s-osinte.adb between all the FSU run time. Note that
   --  this value can only be true if pthread_t has a complete definition that
   --  corresponds exactly to the C header files.

   function Get_Stack_Base (thread : pthread_t) return Address;
   pragma Inline (Get_Stack_Base);
   --  returns the stack base of the specified thread. Only call this function
   --  when Stack_Base_Available is True.

   function Get_Page_Size return int;
   pragma Import (C, Get_Page_Size, "getpagesize");
   --  Returns the size of a page

   PROT_NONE  : constant := 0;
   PROT_READ  : constant := 1;
   PROT_WRITE : constant := 2;
   PROT_EXEC  : constant := 4;
   PROT_ALL   : constant := PROT_READ + PROT_WRITE + PROT_EXEC;
   PROT_ON    : constant := PROT_NONE;
   PROT_OFF   : constant := PROT_ALL;

   function mprotect (addr : Address; len : size_t; prot : int) return int;
   pragma Import (C, mprotect);

   ---------------------------------------
   -- Nonstandard Thread Initialization --
   ---------------------------------------

   --  FSU_THREADS requires pthread_init, which is nonstandard and this should
   --  be invoked during the elaboration of s-taprop.adb.

   --  DragonFlyBSD does not require this so we provide an empty Ada body

   procedure pthread_init;

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
   PTHREAD_PRIO_PROTECT : constant := 2;
   PTHREAD_PRIO_INHERIT : constant := 1;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import
      (C, pthread_mutexattr_setprotocol, "pthread_mutexattr_setprotocol");

   function pthread_mutexattr_getprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : access int) return int;
   pragma Import
     (C, pthread_mutexattr_getprotocol, "pthread_mutexattr_getprotocol");

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import
     (C, pthread_mutexattr_setprioceiling,
      "pthread_mutexattr_setprioceiling");

   function pthread_mutexattr_getprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : access int) return int;
   pragma Import
     (C, pthread_mutexattr_getprioceiling,
      "pthread_mutexattr_getprioceiling");

   type struct_sched_param is record
      sched_priority : int;
   end record;
   pragma Convention (C, struct_sched_param);

   function pthread_getschedparam
     (thread : pthread_t;
      policy : access int;
      param  : access struct_sched_param) return int;
   pragma Import (C, pthread_getschedparam, "pthread_getschedparam");

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int;
   pragma Import (C, pthread_setschedparam, "pthread_setschedparam");

   function pthread_attr_setscope
     (attr            : access pthread_attr_t;
      contentionscope : int) return int;
   pragma Import (C, pthread_attr_setscope, "pthread_attr_setscope");

   function pthread_attr_getscope
     (attr            : access pthread_attr_t;
      contentionscope : access int) return int;
   pragma Import (C, pthread_attr_getscope, "pthread_attr_getscope");

   function pthread_attr_setinheritsched
     (attr            : access pthread_attr_t;
      inheritsched : int) return int;
   pragma Import
     (C, pthread_attr_setinheritsched, "pthread_attr_setinheritsched");

   function pthread_attr_getinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : access int) return int;
   pragma Import
     (C, pthread_attr_getinheritsched, "pthread_attr_getinheritsched");

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int) return int;
   pragma Import (C, pthread_attr_setschedpolicy,
     "pthread_attr_setschedpolicy");

   function pthread_attr_getschedpolicy
     (attr   : access pthread_attr_t;
      policy : access int) return int;
   pragma Import (C, pthread_attr_getschedpolicy,
     "pthread_attr_getschedpolicy");

   function pthread_attr_setschedparam
     (attr        : access pthread_attr_t;
      sched_param : int) return int;
   pragma Import (C, pthread_attr_setschedparam, "pthread_attr_setschedparam");

   function pthread_attr_getschedparam
     (attr        : access pthread_attr_t;
      sched_param : access int) return int;
   pragma Import (C, pthread_attr_getschedparam, "pthread_attr_getschedparam");

   function sched_yield return int;
   pragma Import (C, sched_yield, "pthread_yield");

   --------------------------
   -- P1003.1c  Section 16 --
   --------------------------

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

   function pthread_attr_getdetachstate
     (attr        : access pthread_attr_t;
      detachstate : access int) return int;
   pragma Import
     (C, pthread_attr_getdetachstate, "pthread_attr_getdetachstate");

   function pthread_attr_getstacksize
     (attr      : access pthread_attr_t;
      stacksize : access size_t) return int;
   pragma Import
     (C, pthread_attr_getstacksize, "pthread_attr_getstacksize");

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

   function pthread_detach (thread : pthread_t) return int;
   pragma Import (C, pthread_detach, "pthread_detach");

   procedure pthread_exit (status : System.Address);
   pragma Import (C, pthread_exit, "pthread_exit");

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self, "pthread_self");

   --------------------------
   -- POSIX.1c  Section 17 --
   --------------------------

   function pthread_setspecific
     (key   : pthread_key_t;
      value : System.Address) return  int;
   pragma Import (C, pthread_setspecific, "pthread_setspecific");

   function pthread_getspecific (key : pthread_key_t) return System.Address;
   pragma Import (C, pthread_getspecific, "pthread_getspecific");

   type destructor_pointer is access procedure (arg : System.Address);
   pragma Convention (C, destructor_pointer);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;
   pragma Import (C, pthread_key_create, "pthread_key_create");

   ------------------------------------
   -- Non-portable Pthread Functions --
   ------------------------------------

   function pthread_set_name_np
     (thread : pthread_t;
      name   : System.Address) return int;
   pragma Import (C, pthread_set_name_np, "pthread_set_name_np");

private

   type sigset_t is array (1 .. 4) of unsigned;

   --  In DragonFlyBSD the component sa_handler turns out to
   --  be one a union type, and the selector is a macro:
   --  #define sa_handler __sigaction_u._handler
   --  #define sa_sigaction __sigaction_u._sigaction

   --  Should we add a signal_context type here ???
   --  How could it be done independent of the CPU architecture ???
   --  sigcontext type is opaque, so it is architecturally neutral.
   --  It is always passed as an access type, so define it as an empty record
   --  since the contents are not used anywhere.

   type struct_sigcontext is null record;
   pragma Convention (C, struct_sigcontext);

   type pid_t is new int;

   type time_t is new long;

   type timespec is record
      ts_sec  : time_t;
      ts_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type pthread_t           is new System.Address;
   type pthread_attr_t      is new System.Address;
   type pthread_mutex_t     is new System.Address;
   type pthread_mutexattr_t is new System.Address;
   type pthread_cond_t      is new System.Address;
   type pthread_condattr_t  is new System.Address;
   type pthread_key_t       is new int;

end System.OS_Interface;
