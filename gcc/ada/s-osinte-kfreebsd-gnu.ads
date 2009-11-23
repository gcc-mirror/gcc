------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--             Copyright (C) 1995-2005,2008 Free Software Foundation, Inc.  --
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

--  This is the GNU/kFreeBSD (GNU/LinuxThreads) version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package

with Interfaces.C;
with Unchecked_Conversion;

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lpthread");

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

   EAGAIN   : constant := 35;
   EINTR    : constant := 4;
   EINVAL   : constant := 22;
   ENOMEM   : constant := 12;
   EPERM    : constant := 1;
   ETIMEDOUT    : constant := 60;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 128;
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
   SIGINFO    : constant := 29; --  information request (NetBSD/FreeBSD)
   SIGUSR1    : constant := 30; --  user defined signal 1
   SIGUSR2    : constant := 31; --  user defined signal 2
   SIGLTHRRES : constant := 32; --  GNU/LinuxThreads restart signal
   SIGLTHRCAN : constant := 33; --  GNU/LinuxThreads cancel signal
   SIGLTHRDBG : constant := 34; --  GNU/LinuxThreads debugger signal

   SIGADAABORT : constant := SIGABRT;
   --  Change this if you want to use another signal for task abort.
   --  SIGTERM might be a good one.

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked    : constant Signal_Set := (
      SIGTRAP,
      --  To enable debugging on multithreaded applications, mark SIGTRAP to
      --  be kept unmasked.

      SIGBUS,

      SIGTTIN, SIGTTOU, SIGTSTP,
      --  Keep these three signals unmasked so that background processes
      --  and IO behaves as normal "C" applications

      SIGPROF,
      --  To avoid confusing the profiler

      SIGKILL, SIGSTOP,
      --  These two signals actually cannot be masked;
      --  POSIX simply won't allow it.

      SIGLTHRRES, SIGLTHRCAN, SIGLTHRDBG);
      --  These three signals are used by GNU/LinuxThreads starting from
      --  glibc 2.1 (future 2.2).

   Reserved    : constant Signal_Set :=
   --  I am not sure why the following signal is reserved.
   --  I guess they are not supported by this version of GNU/kFreeBSD.
     (0 .. 0 => SIGVTALRM);

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

   --  sigcontext is architecture dependent, so define it private
   type struct_sigcontext is private;

   type struct_sigaction is record
      sa_handler : System.Address;
      sa_flags   : int;
      sa_mask    : sigset_t;
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

   type timespec is private;

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   function sysconf (name : int) return long;
   pragma Import (C, sysconf);

   SC_CLK_TCK          : constant := 2;
   SC_NPROCESSORS_ONLN : constant := 84;

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 1;
   SCHED_OTHER : constant := 2;
   SCHED_RR    : constant := 3;

   function To_Target_Priority
     (Prio : System.Any_Priority) return Interfaces.C.int;
   --  Maps System.Any_Priority to a POSIX priority.

   -------------
   -- Process --
   -------------

   type pid_t is private;

   function kill (pid : pid_t; sig : Signal) return int;
   pragma Import (C, kill, "kill");

   function getpid return pid_t;
   pragma Import (C, getpid, "getpid");

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;
   pragma Convention (C, Thread_Body);

   function Thread_Body_Access is new
     Unchecked_Conversion (System.Address, Thread_Body);

   type pthread_t is new unsigned_long;
   subtype Thread_Id        is pthread_t;

   function To_pthread_t is new Unchecked_Conversion
     (unsigned_long, pthread_t);

   type pthread_mutex_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   PTHREAD_CREATE_DETACHED : constant := 1;

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

   function Get_Stack_Base (thread : pthread_t) return Address;
   pragma Inline (Get_Stack_Base);
   --  This is a dummy procedure to share some GNULLI files

   ---------------------------------------
   -- Nonstandard Thread Initialization --
   ---------------------------------------

   procedure pthread_init;
   pragma Inline (pthread_init);
   --  This is a dummy procedure to share some GNULLI files

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

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
   end record;
   pragma Convention (C, struct_sched_param);

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int;
   pragma Import (C, pthread_setschedparam, "pthread_setschedparam");

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int) return int;
   pragma Import
     (C, pthread_attr_setschedpolicy, "pthread_attr_setschedpolicy");

   function sched_yield return int;
   pragma Import (C, sched_yield, "sched_yield");

   ---------------------------
   -- P1003.1c - Section 16 --
   ---------------------------

   function pthread_attr_init
     (attributes : access pthread_attr_t) return int;
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

   CPU_SETSIZE : constant := 1_024;

   type bit_field is array (1 .. CPU_SETSIZE) of Boolean;
   for bit_field'Size use CPU_SETSIZE;
   pragma Pack (bit_field);
   pragma Convention (C, bit_field);

   type cpu_set_t is record
      bits : bit_field;
   end record;
   pragma Convention (C, cpu_set_t);

   function pthread_setaffinity_np
     (thread     : pthread_t;
      cpusetsize : size_t;
      cpuset     : access cpu_set_t) return int;
   pragma Import (C, pthread_setaffinity_np, "__gnat_pthread_setaffinity_np");

private

   type sigset_t is array (1 .. 4) of unsigned;

   --  In FreeBSD the component sa_handler turns out to
   --  be one a union type, and the selector is a macro:
   --  #define sa_handler __sigaction_u._handler
   --  #define sa_sigaction __sigaction_u._sigaction

   --  Should we add a signal_context type here ?
   --  How could it be done independent of the CPU architecture ?
   --  sigcontext type is opaque, so it is architecturally neutral.
   --  It is always passed as an access type, so define it as an empty record
   --  since the contents are not used anywhere.
   type struct_sigcontext is null record;
   pragma Convention (C, struct_sigcontext);

   type pid_t is new int;

   type time_t is new long;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type pthread_attr_t is record
      detachstate   : int;
      schedpolicy   : int;
      schedparam    : struct_sched_param;
      inheritsched  : int;
      scope         : int;
      guardsize     : size_t;
      stackaddr_set : int;
      stackaddr     : System.Address;
      stacksize     : size_t;
   end record;
   pragma Convention (C, pthread_attr_t);

   type pthread_condattr_t is record
      dummy : int;
   end record;
   pragma Convention (C, pthread_condattr_t);

   type pthread_mutexattr_t is record
      mutexkind : int;
   end record;
   pragma Convention (C, pthread_mutexattr_t);

   type struct_pthread_fast_lock is record
      status   : long;
      spinlock : int;
   end record;
   pragma Convention (C, struct_pthread_fast_lock);

   type pthread_mutex_t is record
      m_reserved : int;
      m_count    : int;
      m_owner    : System.Address;
      m_kind     : int;
      m_lock     : struct_pthread_fast_lock;
   end record;
   pragma Convention (C, pthread_mutex_t);

   type pthread_cond_t is array (0 .. 47) of unsigned_char;
   pragma Convention (C, pthread_cond_t);

   type pthread_key_t is new unsigned;

end System.OS_Interface;
