------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--          Copyright (C) 1995-2016, Free Software Foundation, Inc.         --
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

--  This is the GNU/Hurd (POSIX Threads) version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package

with Interfaces.C;
with Unchecked_Conversion;

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lpthread");
   pragma Linker_Options ("-lrt");

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
   --  From /usr/include/i386-gnu/bits/errno.h

   function errno return int;
   pragma Import (C, errno, "__get_errno");

   EAGAIN   : constant := 1073741859;
   EINTR    : constant := 1073741828;
   EINVAL   : constant := 1073741846;
   ENOMEM   : constant := 1073741836;
   EPERM    : constant := 1073741825;
   ETIMEDOUT    : constant := 1073741884;

   -------------
   -- Signals --
   -------------
   --  From /usr/include/i386-gnu/bits/signum.h

   Max_Interrupt : constant := 32;
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
   SIGPOLL    : constant := 23; --  I/O possible (same as SIGIO?)
   SIGXCPU    : constant := 24; --  CPU time limit exceeded
   SIGXFSZ    : constant := 25; --  filesize limit exceeded
   SIGVTALRM  : constant := 26; --  virtual timer expired
   SIGPROF    : constant := 27; --  profiling timer expired
   SIGWINCH   : constant := 28; --  window size change
   SIGINFO    : constant := 29; --  information request (NetBSD/FreeBSD)
   SIGUSR1    : constant := 30; --  user defined signal 1
   SIGUSR2    : constant := 31; --  user defined signal 2
   SIGLOST    : constant := 32; --  Resource lost (Sun); server died (GNU)

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

      SIGKILL, SIGSTOP);
      --  These two signals actually cannot be masked;
      --  POSIX simply won't allow it.

   Reserved    : constant Signal_Set :=
   --  I am not sure why the following signal is reserved.
   --  I guess they are not supported by this version of GNU/Hurd.
     (0 .. 0 => SIGVTALRM);

   type sigset_t is private;

   --  From /usr/include/signal.h /usr/include/i386-gnu/bits/sigset.h
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

   --  From /usr/include/i386-gnu/bits/sigaction.h: Note: arg. order differs
   type struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, struct_sigaction);

   type struct_sigaction_ptr is access all struct_sigaction;

   --  From /usr/include/i386-gnu/bits/sigaction.h
   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   --  From /usr/include/i386-gnu/bits/signum.h
   SIG_ERR  : constant := 1;
   SIG_DFL  : constant := 0;
   SIG_IGN  : constant := 1;
   SIG_HOLD : constant := 2;

   --  From /usr/include/i386-gnu/bits/sigaction.h
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

   function nanosleep (rqtp, rmtp : access timespec) return int;
   pragma Import (C, nanosleep, "nanosleep");

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 0;

   --  From: /usr/include/time.h
   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec)
      return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   function clock_getres
     (clock_id : clockid_t;
      res      : access timespec) return int;
   pragma Import (C, clock_getres, "clock_getres");

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   --  From: /usr/include/unistd.h
   function sysconf (name : int) return long;
   pragma Import (C, sysconf);

   --  From /usr/include/i386-gnu/bits/confname.h
   SC_CLK_TCK          : constant := 2;
   SC_NPROCESSORS_ONLN : constant := 84;

   -------------------------
   -- Priority Scheduling --
   -------------------------
   --  From /usr/include/i386-gnu/bits/sched.h

   SCHED_OTHER : constant := 0;
   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;

   function To_Target_Priority
     (Prio : System.Any_Priority) return Interfaces.C.int;
   --  Maps System.Any_Priority to a POSIX priority.

   -------------
   -- Process --
   -------------

   type pid_t is private;

   --  From: /usr/include/signal.h
   function kill (pid : pid_t; sig : Signal) return int;
   pragma Import (C, kill, "kill");

   --  From: /usr/include/unistd.h
   function getpid return pid_t;
   pragma Import (C, getpid, "getpid");

   ---------
   -- LWP --
   ---------

   --  From: /usr/include/pthread/pthread.h
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
     Unchecked_Conversion (System.Address, Thread_Body);

   --  From: /usr/include/bits/pthread.h:typedef int __pthread_t;
   --  /usr/include/pthread/pthreadtypes.h:typedef __pthread_t pthread_t;
   type pthread_t is new unsigned_long;
   subtype Thread_Id        is pthread_t;

   function To_pthread_t is new Unchecked_Conversion
     (unsigned_long, pthread_t);

   type pthread_mutex_t     is limited private;
   type pthread_rwlock_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_rwlockattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   --  From /usr/include/pthread/pthreadtypes.h
   PTHREAD_CREATE_DETACHED : constant := 1;
   PTHREAD_CREATE_JOINABLE : constant := 0;

   PTHREAD_SCOPE_PROCESS : constant := 1;
   PTHREAD_SCOPE_SYSTEM  : constant := 0;

   -----------
   -- Stack --
   -----------

   --  From: /usr/include/i386-gnu/bits/sigstack.h
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
   --  Indicates whether the stack base is available on this target

   function Get_Stack_Base (thread : pthread_t) return Address;
   pragma Inline (Get_Stack_Base);
   --  returns the stack base of the specified thread. Only call this function
   --  when Stack_Base_Available is True.

   --  From: /usr/include/unistd.h __getpagesize or getpagesize??
   function Get_Page_Size return int;
   pragma Import (C, Get_Page_Size, "__getpagesize");
   --  Returns the size of a page

   --  From /usr/include/i386-gnu/bits/mman.h
   PROT_NONE  : constant := 0;
   PROT_READ  : constant := 4;
   PROT_WRITE : constant := 2;
   PROT_EXEC  : constant := 1;
   PROT_ALL   : constant := PROT_READ + PROT_WRITE + PROT_EXEC;
   PROT_ON    : constant := PROT_NONE;
   PROT_OFF   : constant := PROT_ALL;

   --  From /usr/include/i386-gnu/bits/mman.h
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

   --  From: /usr/include/signal.h:
   --  sigwait (__const sigset_t *__restrict __set, int *__restrict __sig)
   function sigwait (set : access sigset_t; sig : access Signal) return int;
   pragma Import (C, sigwait, "sigwait");

   --  From: /usr/include/pthread/pthread.h:
   --  extern int pthread_kill (pthread_t thread, int signo);
   function pthread_kill (thread : pthread_t; sig : Signal) return int;
   pragma Import (C, pthread_kill, "pthread_kill");

   --  From: /usr/include/i386-gnu/bits/sigthread.h
   --  extern int pthread_sigmask (int __how, __const __sigset_t *__newmask,
   --  __sigset_t *__oldmask) __THROW;
   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t) return int;
   pragma Import (C, pthread_sigmask, "pthread_sigmask");

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

   --  From: /usr/include/pthread/pthread.h and
   --  /usr/include/pthread/pthreadtypes.h
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
   pragma Import
     (C, pthread_rwlockattr_setkind_np, "pthread_rwlockattr_setkind_np");

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

   Relative_Timed_Wait : constant Boolean := False;
   --  pthread_cond_timedwait requires an absolute delay time

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------
   --  From /usr/include/pthread/pthreadtypes.h

   PTHREAD_PRIO_NONE    : constant := 0;
   PTHREAD_PRIO_PROTECT : constant := 2;
   PTHREAD_PRIO_INHERIT : constant := 1;

   --  From: /usr/include/pthread/pthread.h
   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol,
     "pthread_mutexattr_setprotocol");

   function pthread_mutexattr_getprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : access int) return int;
   pragma Import (C, pthread_mutexattr_getprotocol,
     "pthread_mutexattr_getprotocol");

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import (C, pthread_mutexattr_setprioceiling,
     "pthread_mutexattr_setprioceiling");

   function pthread_mutexattr_getprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : access int) return int;
   pragma Import (C, pthread_mutexattr_getprioceiling,
     "pthread_mutexattr_getprioceiling");

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
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

   function pthread_attr_getscope
     (attr            : access pthread_attr_t;
      contentionscope : access int) return int;
   pragma Import (C, pthread_attr_getscope, "pthread_attr_getscope");

   function pthread_attr_setinheritsched
     (attr            : access pthread_attr_t;
      inheritsched : int) return int;
   pragma Import (C, pthread_attr_setinheritsched,
     "pthread_attr_setinheritsched");

   function pthread_attr_getinheritsched
     (attr         : access pthread_attr_t;
      inheritsched : access int) return int;
   pragma Import (C, pthread_attr_getinheritsched,
     "pthread_attr_getinheritsched");

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int) return int;
   pragma Import (C, pthread_attr_setschedpolicy, "pthread_setschedpolicy");

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

   --  From: /usr/include/pthread/pthread.h
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

   --  From /usr/include/i386-gnu/bits/sched.h
   CPU_SETSIZE : constant := 1_024;

   type bit_field is array (1 .. CPU_SETSIZE) of Boolean;
   for bit_field'Size use CPU_SETSIZE;
   pragma Pack (bit_field);
   pragma Convention (C, bit_field);

   type cpu_set_t is record
      bits : bit_field;
   end record;
   pragma Convention (C, cpu_set_t);

private

   type sigset_t is array (1 .. 4) of unsigned;

   --  In GNU/Hurd the component sa_handler turns out to
   --  be one a union type, and the selector is a macro:
   --  #define sa_handler __sigaction_handler.sa_handler
   --  #define sa_sigaction __sigaction_handler.sa_sigaction

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

   --  From: /usr/include/pthread/pthreadtypes.h:
   --  typedef struct __pthread_attr pthread_attr_t;
   --  /usr/include/i386-gnu/bits/thread-attr.h: struct __pthread_attr...
   --  /usr/include/pthread/pthreadtypes.h: enum __pthread_contentionscope
   --   enum __pthread_detachstate detachstate;
   --   enum __pthread_inheritsched inheritsched;
   --   enum __pthread_contentionscope contentionscope;
   --   Not used: schedpolicy   : int;
   type pthread_attr_t is record
      schedparam    : struct_sched_param;
      stackaddr     : System.Address;
      stacksize     : size_t;
      guardsize     : size_t;
      detachstate   : int;
      inheritsched  : int;
      contentionscope : int;
      schedpolicy   : int;
   end record;
   pragma Convention (C, pthread_attr_t);

   --  From: /usr/include/pthread/pthreadtypes.h:
   --  typedef struct __pthread_condattr pthread_condattr_t;
   --  From: /usr/include/i386-gnu/bits/condition-attr.h:
   --  struct __pthread_condattr {
   --    enum __pthread_process_shared pshared;
   --    __Clockid_T Clock;}
   --  From: /usr/include/pthread/pthreadtypes.h:
   --  enum __pthread_process_shared
   type pthread_condattr_t is record
      pshared : int;
      clock   : clockid_t;
   end record;
   pragma Convention (C, pthread_condattr_t);

   --  From: /usr/include/pthread/pthreadtypes.h:
   --  typedef struct __pthread_mutexattr pthread_mutexattr_t; and
   --  /usr/include/i386-gnu/bits/mutex-attr.h
   --  struct __pthread_mutexattr {
   --  int prioceiling;
   --  enum __pthread_mutex_protocol protocol;
   --  enum __pthread_process_shared pshared;
   --  enum __pthread_mutex_type mutex_type;};
   type pthread_mutexattr_t is record
      prioceiling : int;
      protocol    : int;
      pshared     : int;
      mutex_type  : int;
   end record;
   pragma Convention (C, pthread_mutexattr_t);

   --  From: /usr/include/pthread/pthreadtypes.h
   --  typedef struct __pthread_mutex pthread_mutex_t; and
   --  /usr/include/i386-gnu/bits/mutex.h:
   --  struct __pthread_mutex {
   --  __pthread_spinlock_t __held;
   --  __pthread_spinlock_t __lock;
   --  /* in cthreads, mutex_init does not initialized the third
   --    pointer, as such, we cannot rely on its value for anything.  */
   --    char *cthreadscompat1;
   --  struct __pthread *__queue;
   --  struct __pthread_mutexattr *attr;
   --  void *data;
   --  /*  up to this point, we are completely compatible with cthreads
   --    and what libc expects.  */
   --    void *owner;
   --  unsigned locks;
   --  /* if null then the default attributes apply.  */
   --    };

   type pthread_mutex_t is record
      held          : int;
      lock          : int;
      cthreadcompat : System.Address;
      queue         : System.Address;
      attr          : System.Address;
      data          : System.Address;
      owner         : System.Address;
      locks         : unsigned;
   end record;
   pragma Convention (C, pthread_mutex_t);
   --  pointer needed?
   --  type pthread_mutex_t_ptr is access pthread_mutex_t;

   --  From: /usr/include/pthread/pthreadtypes.h:
   --  typedef struct __pthread_cond pthread_cond_t;
   --  typedef struct __pthread_condattr pthread_condattr_t;
   --  /usr/include/i386-gnu/bits/condition.h:struct __pthread_cond{}
   --  pthread_condattr_t: see above!
   --  /usr/include/i386-gnu/bits/condition.h:
   --  struct __pthread_condimpl *__impl;

   type pthread_cond_t is record
      lock       : int;
      queue      : System.Address;
      condattr   : System.Address;
      impl       : System.Address;
      data       : System.Address;
   end record;
   pragma Convention (C, pthread_cond_t);

   --  From: /usr/include/pthread/pthreadtypes.h:
   --  typedef __pthread_key pthread_key_t; and
   --  /usr/include/i386-gnu/bits/thread-specific.h:
   --  typedef int __pthread_key;

   type pthread_key_t is new int;

   --  From: /usr/include/i386-gnu/bits/rwlock-attr.h:
   --  struct __pthread_rwlockattr {
   --  enum __pthread_process_shared pshared; };

   type pthread_rwlockattr_t is record
      pshared : int;
   end record;
   pragma Convention (C, pthread_rwlockattr_t);

   --  From: /usr/include/i386-gnu/bits/rwlock.h:
   --  struct __pthread_rwlock {
   --  __pthread_spinlock_t __held;
   --  __pthread_spinlock_t __lock;
   --  int readers;
   --  struct __pthread *readerqueue;
   --  struct __pthread *writerqueue;
   --  struct __pthread_rwlockattr *__attr;
   --  void *__data; };

   type pthread_rwlock_t is record
      held        : int;
      lock        : int;
      readers     : int;
      readerqueue : System.Address;
      writerqueue : System.Address;
      attr        : pthread_rwlockattr_t;
      data        : int;
   end record;
   pragma Convention (C, pthread_rwlock_t);

end System.OS_Interface;
