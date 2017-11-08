------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1995-2017, Free Software Foundation, Inc.         --
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

--  This is a QNX/Neutrino version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Ada.Unchecked_Conversion;
with Interfaces.C;
with System.OS_Constants;

package System.OS_Interface is
   pragma Preelaborate;

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

   EPERM     : constant := 1;
   EINTR     : constant := 4;
   EAGAIN    : constant := 11;
   ENOMEM    : constant := 12;
   EINVAL    : constant := 22;
   ETIMEDOUT : constant := 260;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 64;
   type Signal is new int range 0 .. Max_Interrupt;
   for Signal'Size use int'Size;

   SIGHUP     : constant := 1;
   SIGINT     : constant := 2;
   SIGQUIT    : constant := 3;
   SIGILL     : constant := 4;
   SIGTRAP    : constant := 5;
   SIGIOT     : constant := 6;
   SIGABRT    : constant := 6;
   SIGDEADLK  : constant := 7;
   SIGFPE     : constant := 8;
   SIGKILL    : constant := 9;
   SIGBUS     : constant := 10;
   SIGSEGV    : constant := 11;
   SIGSYS     : constant := 12;
   SIGPIPE    : constant := 13;
   SIGALRM    : constant := 14;
   SIGTERM    : constant := 15;
   SIGUSR1    : constant := 16;
   SIGUSR2    : constant := 17;
   SIGCLD     : constant := 18;
   SIGCHLD    : constant := 18;
   SIGPWR     : constant := 19;
   SIGWINCH   : constant := 20;
   SIGURG     : constant := 21;
   SIGPOLL    : constant := 22;
   SIGIO      : constant := 22;
   SIGSTOP    : constant := 23;
   SIGTSTP    : constant := 24;
   SIGCONT    : constant := 25;
   SIGTTIN    : constant := 26;
   SIGTTOU    : constant := 27;
   SIGVTALRM  : constant := 28;
   SIGPROF    : constant := 29;
   SIGXCPU    : constant := 30;
   SIGXFSZ    : constant := 31;

   SIGRTMIN   : constant := 41;
   SITRTMAX   : constant := 56;

   SIGSELECT  : constant := 57;
   SIGPHOTON  : constant := 58;

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

      SIGKILL, SIGSTOP);
      --  These two signals actually can't be masked (POSIX won't allow it)

   Reserved : constant Signal_Set := (SIGKILL, SIGSTOP, SIGSEGV);

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
      sa_flags    : Interfaces.C.int;
      sa_mask     : sigset_t;
   end record;
   pragma Convention (C, struct_sigaction);

   type struct_sigaction_ptr is access all struct_sigaction;

   SIG_BLOCK   : constant := 0;
   SIG_UNBLOCK : constant := 1;
   SIG_SETMASK : constant := 2;
   SIG_PENDING : constant := 5;

   SA_NOCLDSTOP : constant := 16#0001#;
   SA_SIGINFO   : constant := 16#0002#;
   SA_RESETHAND : constant := 16#0004#;
   SA_ONSTACK   : constant := 16#0008#;
   SA_NODEFER   : constant := 16#0010#;
   SA_NOCLDWAIT : constant := 16#0020#;

   SS_ONSTACK   : constant := 1;
   SS_DISABLE   : constant := 2;

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
     (Prio : System.Any_Priority) return Interfaces.C.int
     with Inline_Always;
   --  Maps System.Any_Priority to a POSIX priority

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
     Ada.Unchecked_Conversion (System.Address, Thread_Body);

   type pthread_t is new unsigned_long;
   subtype Thread_Id is pthread_t;

   function To_pthread_t is
     new Ada.Unchecked_Conversion (unsigned_long, pthread_t);

   type pthread_mutex_t      is limited private;
   type pthread_cond_t       is limited private;
   type pthread_attr_t       is limited private;
   type pthread_mutexattr_t  is limited private;
   type pthread_condattr_t   is limited private;
   type pthread_key_t        is private;

   PTHREAD_CREATE_DETACHED : constant := 1;

   PTHREAD_SCOPE_PROCESS : constant := 1;
   PTHREAD_SCOPE_SYSTEM  : constant := 0;

   --  Read/Write lock not supported on Android.

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
   --  Dummy definition: alternate stack not available due to missing
   --  sigaltstack

   Alternate_Stack_Size : constant := 0;
   --  This must be in keeping with init.c:__gnat_alternate_stack

   Stack_Base_Available : constant Boolean := False;
   --  Indicates whether the stack base is available on this target

   function Get_Stack_Base (thread : pthread_t) return System.Address
     with Inline_Always;
   --  This is a dummy procedure to share some GNULLI files

   function Get_Page_Size return int;
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

   procedure pthread_init with Inline_Always;

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
   pragma Import (C, pthread_sigmask, "sigprocmask");
   --  pthread_sigmask maybe be broken due to mismatch between sigset_t and
   --  kernel_sigset_t, substitute sigprocmask temporarily.  ???
   --  pragma Import (C, pthread_sigmask, "pthread_sigmask");

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

   PTHREAD_PRIO_PROTECT : constant := 0;
   PTHREAD_PRIO_INHERIT : constant := 1;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int is (0);

   function pthread_mutexattr_setprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : int) return int is (0);

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
     (attr  : access pthread_attr_t;
      scope : int) return int;
   pragma Import (C, pthread_attr_setscope, "pthread_attr_setscope");

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
   pragma Import (C, lwp_self, "pthread_self");

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

private

   type sigset_t is new Interfaces.C.unsigned_long;
   pragma Convention (C, sigset_t);
   for sigset_t'Alignment use Interfaces.C.unsigned_long'Alignment;

   type pid_t is new int;

   type time_t is new long;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type unsigned_long_long_t is mod 2 ** 64;
   --  Local type only used to get the alignment of this type below

   subtype char_array is Interfaces.C.char_array;

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

   type pthread_cond_t is record
      Data : char_array (1 .. OS_Constants.PTHREAD_COND_SIZE);
   end record;
   pragma Convention (C, pthread_cond_t);
   for pthread_cond_t'Alignment use unsigned_long_long_t'Alignment;

   type pthread_key_t is new unsigned;

end System.OS_Interface;
