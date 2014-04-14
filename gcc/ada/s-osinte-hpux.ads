------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--               Copyright (C) 1991-1994, Florida State University          --
--            Copyright (C) 1995-2014, Free Software Foundation, Inc.       --
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

--  This is a HPUX 11.0 (Native THREADS) version of this package

--  This package encapsulates all direct interfaces to OS services that are
--  needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Ada.Unchecked_Conversion;

with Interfaces.C;

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lpthread");

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

   EAGAIN    : constant := 11;
   EINTR     : constant := 4;
   EINVAL    : constant := 22;
   ENOMEM    : constant := 12;
   ETIMEDOUT : constant := 238;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 44;
   type Signal is new int range 0 .. Max_Interrupt;
   for Signal'Size use int'Size;

   SIGHUP     : constant := 1; --  hangup
   SIGINT     : constant := 2; --  interrupt (rubout)
   SIGQUIT    : constant := 3; --  quit (ASCD FS)
   SIGILL     : constant := 4; --  illegal instruction (not reset)
   SIGTRAP    : constant := 5; --  trace trap (not reset)
   SIGIOT     : constant := 6; --  IOT instruction
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the future
   SIGEMT     : constant := 7; --  EMT instruction
   SIGFPE     : constant := 8; --  floating point exception
   SIGKILL    : constant := 9; --  kill (cannot be caught or ignored)
   SIGBUS     : constant := 10; --  bus error
   SIGSEGV    : constant := 11; --  segmentation violation
   SIGSYS     : constant := 12; --  bad argument to system call
   SIGPIPE    : constant := 13; --  write on a pipe with no one to read it
   SIGALRM    : constant := 14; --  alarm clock
   SIGTERM    : constant := 15; --  software termination signal from kill
   SIGUSR1    : constant := 16; --  user defined signal 1
   SIGUSR2    : constant := 17; --  user defined signal 2
   SIGCLD     : constant := 18; --  alias for SIGCHLD
   SIGCHLD    : constant := 18; --  child status change
   SIGPWR     : constant := 19; --  power-fail restart
   SIGVTALRM  : constant := 20; --  virtual timer alarm
   SIGPROF    : constant := 21; --  profiling timer alarm
   SIGIO      : constant := 22; --  asynchronous I/O
   SIGPOLL    : constant := 22; --  pollable event occurred
   SIGWINCH   : constant := 23; --  window size change
   SIGSTOP    : constant := 24; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 25; --  user stop requested from tty
   SIGCONT    : constant := 26; --  stopped process has been continued
   SIGTTIN    : constant := 27; --  background tty read attempted
   SIGTTOU    : constant := 28; --  background tty write attempted
   SIGURG     : constant := 29; --  urgent condition on IO channel
   SIGLOST    : constant := 30; --  remote lock lost  (NFS)
   SIGDIL     : constant := 32; --  DIL signal
   SIGXCPU    : constant := 33; --  CPU time limit exceeded (setrlimit)
   SIGXFSZ    : constant := 34; --  file size limit exceeded (setrlimit)
   SIGCANCEL  : constant := 35; --  used for pthread cancellation.
   SIGGFAULT  : constant := 36; --  Graphics framebuffer fault

   SIGADAABORT : constant := SIGABRT;
   --  Note: on other targets, we usually use SIGABRT, but on HPUX, it
   --  appears that SIGABRT can't be used in sigwait(), so we use SIGTERM.
   --  Do we use SIGTERM or SIGABRT???

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked    : constant Signal_Set :=
     (SIGABRT, SIGPIPE, SIGBUS, SIGTRAP, SIGTTIN, SIGTTOU, SIGTSTP, SIGPROF,
      SIGALRM, SIGVTALRM, SIGIO, SIGCHLD);

   Reserved    : constant Signal_Set := (SIGKILL, SIGSTOP);

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
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   SA_SIGINFO : constant := 16#10#;
   SA_ONSTACK : constant := 16#01#;

   SIG_BLOCK   : constant := 0;
   SIG_UNBLOCK : constant := 1;
   SIG_SETMASK : constant := 2;

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
   --  Indicates whether time slicing is supported

   type timespec is private;

   type clockid_t is new int;

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

   type struct_timezone is record
      tz_minuteswest : int;
      tz_dsttime     : int;
   end record;
   pragma Convention (C, struct_timezone);
   type struct_timezone_ptr is access all struct_timezone;

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 0;
   SCHED_RR    : constant := 1;
   SCHED_OTHER : constant := 2;

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
   pragma Import (C, lwp_self, "_lwp_self");

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

   PTHREAD_CREATE_DETACHED : constant := 16#de#;

   PTHREAD_SCOPE_PROCESS : constant := 2;
   PTHREAD_SCOPE_SYSTEM  : constant := 1;

   --  Read/Write lock not supported on HPUX. To add support both types
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
      ss_flags : int;
      ss_size  : size_t;
   end record;
   pragma Convention (C, stack_t);

   function sigaltstack
     (ss  : not null access stack_t;
      oss : access stack_t) return int;
   pragma Import (C, sigaltstack, "sigaltstack");

   Alternate_Stack : aliased System.Address;
   pragma Import (C, Alternate_Stack, "__gnat_alternate_stack");
   --  The alternate signal stack for stack overflows

   Alternate_Stack_Size : constant := 128 * 1024;
   --  This must be in keeping with init.c:__gnat_alternate_stack

   Stack_Base_Available : constant Boolean := False;
   --  Indicates whether the stack base is available on this target

   function Get_Stack_Base (thread : pthread_t) return Address;
   pragma Inline (Get_Stack_Base);
   --  Returns the stack base of the specified thread. Only call this function
   --  when Stack_Base_Available is True.

   function Get_Page_Size return size_t;
   function Get_Page_Size return Address;
   pragma Import (C, Get_Page_Size, "getpagesize");
   --  Returns the size of a page

   PROT_NONE  : constant := 0;
   PROT_READ  : constant := 1;
   PROT_WRITE : constant := 2;
   PROT_EXEC  : constant := 4;
   PROT_ALL   : constant := PROT_READ + PROT_WRITE + PROT_EXEC;
   PROT_ON    : constant := PROT_READ;
   PROT_OFF   : constant := PROT_ALL;

   function mprotect (addr : Address; len : size_t; prot : int) return int;
   pragma Import (C, mprotect);

   ---------------------------------------
   -- Nonstandard Thread Initialization --
   ---------------------------------------

   procedure pthread_init;
   pragma Inline (pthread_init);
   --  This is a dummy procedure to share some GNULLI files

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

   PTHREAD_PRIO_NONE    : constant := 16#100#;
   PTHREAD_PRIO_PROTECT : constant := 16#200#;
   PTHREAD_PRIO_INHERIT : constant := 16#400#;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol);

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import (C, pthread_mutexattr_setprioceiling);

   type Array_7_Int is array (0 .. 6) of int;
   type struct_sched_param is record
      sched_priority : int;
      sched_reserved : Array_7_Int;
   end record;

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param)
     return int;
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

   function sched_yield return int;
   pragma Import (C, sched_yield, "sched_yield");

   --------------------------
   -- P1003.1c  Section 16 --
   --------------------------

   function pthread_attr_init
     (attributes : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_init, "__pthread_attr_init_system");

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
   pragma Import (C, pthread_create, "__pthread_create_system");

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

   type unsigned_int_array_8 is array (0 .. 7) of unsigned;
   type sigset_t is record
      sigset : unsigned_int_array_8;
   end record;
   pragma Convention (C_Pass_By_Copy, sigset_t);

   type pid_t is new int;

   type time_t is new long;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type pthread_attr_t is new int;
   type pthread_condattr_t is new int;
   type pthread_mutexattr_t is new int;
   type pthread_t is new int;

   type short_array is array (Natural range <>) of short;
   type int_array is array (Natural range <>) of int;

   type pthread_mutex_t is record
      m_short : short_array (0 .. 1);
      m_int   : int;
      m_int1  : int_array (0 .. 3);
      m_pad   : int;

      m_ptr : int;
      --  actually m_ptr is a void*, and on 32 bit ABI, m_pad is added so that
      --  this field takes 64 bits. On 64 bit ABI, m_pad is gone, and m_ptr is
      --  a 64 bit void*. Assume int'Size = 32.

      m_int2   : int_array (0 .. 1);
      m_int3   : int_array (0 .. 3);
      m_short2 : short_array (0 .. 1);
      m_int4   : int_array (0 .. 4);
      m_int5   : int_array (0 .. 1);
   end record;
   for pthread_mutex_t'Alignment use System.Address'Alignment;
   pragma Convention (C, pthread_mutex_t);

   type pthread_cond_t is record
      c_short : short_array (0 .. 1);
      c_int   : int;
      c_int1  : int_array (0 .. 3);
      m_pad   : int;
      m_ptr   : int;  --  see comment in pthread_mutex_t
      c_int2  : int_array (0 .. 1);
      c_int3  : int_array (0 .. 1);
      c_int4  : int_array (0 .. 1);
   end record;
   for pthread_cond_t'Alignment use System.Address'Alignment;
   pragma Convention (C, pthread_cond_t);

   type pthread_key_t is new int;

end System.OS_Interface;
