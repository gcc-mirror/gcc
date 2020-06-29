------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-2017, Florida State University            --
--          Copyright (C) 1995-2020, Free Software Foundation, Inc.         --
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

--  This is a GNU/Linux version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Ada.Unchecked_Conversion;
with Interfaces.C;
with System.Linux;
with System.OS_Constants;

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lrt");
   --  Needed for clock_getres with glibc versions prior to 2.17

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

   EAGAIN    : constant := System.Linux.EAGAIN;
   EINTR     : constant := System.Linux.EINTR;
   EINVAL    : constant := System.Linux.EINVAL;
   ENOMEM    : constant := System.Linux.ENOMEM;
   EPERM     : constant := System.Linux.EPERM;
   ETIMEDOUT : constant := System.Linux.ETIMEDOUT;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 63;
   type Signal is new int range 0 .. Max_Interrupt;
   for Signal'Size use int'Size;

   SIGHUP     : constant := System.Linux.SIGHUP;
   SIGINT     : constant := System.Linux.SIGINT;
   SIGQUIT    : constant := System.Linux.SIGQUIT;
   SIGILL     : constant := System.Linux.SIGILL;
   SIGTRAP    : constant := System.Linux.SIGTRAP;
   SIGIOT     : constant := System.Linux.SIGIOT;
   SIGABRT    : constant := System.Linux.SIGABRT;
   SIGBUS     : constant := System.Linux.SIGBUS;
   SIGFPE     : constant := System.Linux.SIGFPE;
   SIGKILL    : constant := System.Linux.SIGKILL;
   SIGUSR1    : constant := System.Linux.SIGUSR1;
   SIGSEGV    : constant := System.Linux.SIGSEGV;
   SIGUSR2    : constant := System.Linux.SIGUSR2;
   SIGPIPE    : constant := System.Linux.SIGPIPE;
   SIGALRM    : constant := System.Linux.SIGALRM;
   SIGTERM    : constant := System.Linux.SIGTERM;
   SIGSTKFLT  : constant := System.Linux.SIGSTKFLT;
   SIGCLD     : constant := System.Linux.SIGCLD;
   SIGCHLD    : constant := System.Linux.SIGCHLD;
   SIGCONT    : constant := System.Linux.SIGCONT;
   SIGSTOP    : constant := System.Linux.SIGSTOP;
   SIGTSTP    : constant := System.Linux.SIGTSTP;
   SIGTTIN    : constant := System.Linux.SIGTTIN;
   SIGTTOU    : constant := System.Linux.SIGTTOU;
   SIGURG     : constant := System.Linux.SIGURG;
   SIGXCPU    : constant := System.Linux.SIGXCPU;
   SIGXFSZ    : constant := System.Linux.SIGXFSZ;
   SIGVTALRM  : constant := System.Linux.SIGVTALRM;
   SIGPROF    : constant := System.Linux.SIGPROF;
   SIGWINCH   : constant := System.Linux.SIGWINCH;
   SIGPOLL    : constant := System.Linux.SIGPOLL;
   SIGIO      : constant := System.Linux.SIGIO;
   SIGLOST    : constant := System.Linux.SIGLOST;
   SIGPWR     : constant := System.Linux.SIGPWR;
   SIGSYS     : constant := System.Linux.SIGSYS;
   SIGUNUSED  : constant := System.Linux.SIGUNUSED;
   SIG32      : constant := System.Linux.SIG32;
   SIG33      : constant := System.Linux.SIG33;
   SIG34      : constant := System.Linux.SIG34;

   SIGADAABORT : constant := SIGABRT;
   --  Change this to use another signal for task abort. SIGTERM might be a
   --  good one.

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked : constant Signal_Set := (
      SIGTRAP,
      --  To enable debugging on multithreaded applications, mark SIGTRAP to
      --  be kept unmasked.

      SIGBUS,

      SIGTTIN, SIGTTOU, SIGTSTP,
      --  Keep these three signals unmasked so that background processes and IO
      --  behaves as normal "C" applications

      SIGPROF,
      --  To avoid confusing the profiler

      SIGKILL, SIGSTOP
      --  These two signals actually can't be masked (POSIX won't allow it)
      );

   Reserved : constant Signal_Set := (
      SIG32, SIG33, SIG34
      --  glibc POSIX threads implementation uses two (NPTL) or three
      --  (LinuxThreads) real-time signals for its own use (see SIGNAL(7)).
      --  These signals are considered reserved and not unmasked as glibc does
      --  not permit these signals to be used by the public signal.h API.
      --  While LinuxThreads is mostly likely unused now, SIG34 is still
      --  reserved as this behavior is consistent with past GNAT releases.
      );

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

   type union_type_3 is new String (1 .. 116);
   type siginfo_t is record
      si_signo : int;
      si_code  : int;
      si_errno : int;
      X_data   : union_type_3;
   end record;
   pragma Convention (C, siginfo_t);

   type struct_sigaction is record
      sa_handler  : System.Address;
      sa_mask     : sigset_t;
      sa_flags    : int;
      sa_restorer : System.Address;
   end record;
   pragma Convention (C, struct_sigaction);

   type struct_sigaction_ptr is access all struct_sigaction;

   type Machine_State is record
      eip : unsigned_long;
      ebx : unsigned_long;
      esp : unsigned_long;
      ebp : unsigned_long;
      esi : unsigned_long;
      edi : unsigned_long;
   end record;
   type Machine_State_Ptr is access all Machine_State;

   SA_SIGINFO : constant := System.Linux.SA_SIGINFO;
   SA_ONSTACK : constant := System.Linux.SA_ONSTACK;

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

   subtype time_t    is System.Linux.time_t;
   subtype timespec  is System.Linux.timespec;
   subtype timeval   is System.Linux.timeval;
   subtype clockid_t is System.Linux.clockid_t;

   function clock_gettime
     (clock_id : clockid_t; tp : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   function clock_getres
     (clock_id : clockid_t;
      res      : access timespec) return int;
   pragma Import (C, clock_getres, "clock_getres");

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

   SCHED_OTHER : constant := 0;
   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;

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

   PR_SET_NAME : constant := 15;
   PR_GET_NAME : constant := 16;

   function prctl
     (option : int;
      arg    : unsigned_long) return int;
   pragma Import (C_Variadic_1, prctl, "prctl");

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;
   pragma Convention (C, Thread_Body);

   function Thread_Body_Access is new
     Ada.Unchecked_Conversion (System.Address, Thread_Body);

   type pthread_t is new unsigned_long;
   subtype Thread_Id is pthread_t;

   function To_pthread_t is
     new Ada.Unchecked_Conversion (unsigned_long, pthread_t);

   type pthread_mutex_t      is limited private;
   type pthread_rwlock_t     is limited private;
   type pthread_cond_t       is limited private;
   type pthread_attr_t       is limited private;
   type pthread_mutexattr_t  is limited private;
   type pthread_rwlockattr_t is limited private;
   type pthread_condattr_t   is limited private;
   type pthread_key_t        is private;

   PTHREAD_CREATE_DETACHED : constant := 1;

   -----------
   -- Stack --
   -----------

   subtype char_array is Interfaces.C.char_array;

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

   Alternate_Stack_Size : constant := 16 * 1024;
   --  This must be in keeping with init.c:__gnat_alternate_stack

   Alternate_Stack : aliased char_array (1 .. Alternate_Stack_Size);
   pragma Import (C, Alternate_Stack, "__gnat_alternate_stack");
   --  The alternate signal stack for stack overflows

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

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   PTHREAD_PRIO_NONE    : constant := 0;
   PTHREAD_PRIO_INHERIT : constant := 1;
   PTHREAD_PRIO_PROTECT : constant := 2;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol);

   function pthread_mutexattr_setprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import (C, pthread_mutexattr_setprioceiling);

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

   function lwp_self return System.Address;
   pragma Import (C, lwp_self, "__gnat_lwp_self");

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

   ----------------
   -- Extensions --
   ----------------

   CPU_SETSIZE : constant := 1_024;
   --  Size of the cpu_set_t mask on most linux systems (SUSE 11 uses 4_096).
   --  This is kept for backward compatibility (System.Task_Info uses it), but
   --  the run-time library does no longer rely on static masks, using
   --  dynamically allocated masks instead.

   type bit_field is array (1 .. CPU_SETSIZE) of Boolean;
   for bit_field'Size use CPU_SETSIZE;
   pragma Pack (bit_field);
   pragma Convention (C, bit_field);

   type cpu_set_t is record
      bits : bit_field;
   end record;
   pragma Convention (C, cpu_set_t);

   type cpu_set_t_ptr is access all cpu_set_t;
   --  In the run-time library we use this pointer because the size of type
   --  cpu_set_t varies depending on the glibc version. Hence, objects of type
   --  cpu_set_t are allocated dynamically using the number of processors
   --  available in the target machine (value obtained at execution time).

   function CPU_ALLOC (count : size_t) return cpu_set_t_ptr;
   pragma Import (C, CPU_ALLOC, "__gnat_cpu_alloc");
   --  Wrapper around the CPU_ALLOC C macro

   function CPU_ALLOC_SIZE (count : size_t) return size_t;
   pragma Import (C, CPU_ALLOC_SIZE, "__gnat_cpu_alloc_size");
   --  Wrapper around the CPU_ALLOC_SIZE C macro

   procedure CPU_FREE (cpuset : cpu_set_t_ptr);
   pragma Import (C, CPU_FREE, "__gnat_cpu_free");
   --  Wrapper around the CPU_FREE C macro

   procedure CPU_ZERO (count : size_t; cpuset : cpu_set_t_ptr);
   pragma Import (C, CPU_ZERO, "__gnat_cpu_zero");
   --  Wrapper around the CPU_ZERO_S C macro

   procedure CPU_SET (cpu : int; count : size_t; cpuset : cpu_set_t_ptr);
   pragma Import (C, CPU_SET, "__gnat_cpu_set");
   --  Wrapper around the CPU_SET_S C macro

   function pthread_setaffinity_np
     (thread     : pthread_t;
      cpusetsize : size_t;
      cpuset     : cpu_set_t_ptr) return int;
   pragma Import (C, pthread_setaffinity_np, "pthread_setaffinity_np");
   pragma Weak_External (pthread_setaffinity_np);
   --  Use a weak symbol because this function may be available or not,
   --  depending on the version of the system.

   function pthread_attr_setaffinity_np
     (attr       : access pthread_attr_t;
      cpusetsize : size_t;
      cpuset     : cpu_set_t_ptr) return int;
   pragma Import (C, pthread_attr_setaffinity_np,
                    "pthread_attr_setaffinity_np");
   pragma Weak_External (pthread_attr_setaffinity_np);
   --  Use a weak symbol because this function may be available or not,
   --  depending on the version of the system.

private

   type sigset_t is
     array (0 .. OS_Constants.SIZEOF_sigset - 1) of unsigned_char;
   pragma Convention (C, sigset_t);
   for sigset_t'Alignment use Interfaces.C.unsigned_long'Alignment;

   pragma Warnings (Off);
   for struct_sigaction use record
      sa_handler at Linux.sa_handler_pos range 0 .. Standard'Address_Size - 1;
      sa_mask    at Linux.sa_mask_pos    range 0 .. 1023;
      sa_flags   at Linux.sa_flags_pos   range 0 .. int'Size - 1;
   end record;
   --  We intentionally leave sa_restorer unspecified and let the compiler
   --  append it after the last field, so disable corresponding warning.
   pragma Warnings (On);

   type pid_t is new int;

   type pthread_attr_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_ATTR_SIZE);
   end record;
   pragma Convention (C, pthread_attr_t);
   for pthread_attr_t'Alignment use Interfaces.C.unsigned_long'Alignment;

   type pthread_condattr_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_CONDATTR_SIZE);
   end record;
   pragma Convention (C, pthread_condattr_t);
   for pthread_condattr_t'Alignment use Interfaces.C.int'Alignment;

   type pthread_mutexattr_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_MUTEXATTR_SIZE);
   end  record;
   pragma Convention (C, pthread_mutexattr_t);
   for pthread_mutexattr_t'Alignment use Interfaces.C.int'Alignment;

   type pthread_mutex_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_MUTEX_SIZE);
   end record;
   pragma Convention (C, pthread_mutex_t);
   for pthread_mutex_t'Alignment use Interfaces.C.unsigned_long'Alignment;

   type pthread_rwlockattr_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_RWLOCKATTR_SIZE);
   end record;
   pragma Convention (C, pthread_rwlockattr_t);
   for pthread_rwlockattr_t'Alignment use Interfaces.C.unsigned_long'Alignment;

   type pthread_rwlock_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_RWLOCK_SIZE);
   end record;
   pragma Convention (C, pthread_rwlock_t);
   for pthread_rwlock_t'Alignment use Interfaces.C.unsigned_long'Alignment;

   type pthread_cond_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_COND_SIZE);
   end record;
   pragma Convention (C, pthread_cond_t);
   for pthread_cond_t'Alignment use Interfaces.Unsigned_64'Alignment;

   type pthread_key_t is new unsigned;

end System.OS_Interface;
