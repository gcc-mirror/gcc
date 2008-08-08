------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--          Copyright (C) 1995-2008, Free Software Foundation, Inc.         --
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

--  This is the SGI Pthreads version of this package

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

   EINTR     : constant := 4;   --  interrupted system call
   EAGAIN    : constant := 11;  --  No more processes
   ENOMEM    : constant := 12;  --  Not enough core
   EINVAL    : constant := 22;  --  Invalid argument
   ETIMEDOUT : constant := 145; --  Connection timed out

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 64;
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
   SIGWINCH   : constant := 20; --  window size change
   SIGURG     : constant := 21; --  urgent condition on IO channel
   SIGPOLL    : constant := 22; --  pollable event occurred
   SIGIO      : constant := 22; --  I/O possible (Solaris SIGPOLL alias)
   SIGSTOP    : constant := 23; --  stop (cannot be caught or ignored)
   SIGTSTP    : constant := 24; --  user stop requested from tty
   SIGCONT    : constant := 25; --  stopped process has been continued
   SIGTTIN    : constant := 26; --  background tty read attempted
   SIGTTOU    : constant := 27; --  background tty write attempted
   SIGVTALRM  : constant := 28; --  virtual timer expired
   SIGPROF    : constant := 29; --  profiling timer expired
   SIGXCPU    : constant := 30; --  CPU time limit exceeded
   SIGXFSZ    : constant := 31; --  filesize limit exceeded
   SIGK32     : constant := 32; --  reserved for kernel (IRIX)
   SIGCKPT    : constant := 33; --  Checkpoint warning
   SIGRESTART : constant := 34; --  Restart warning
   SIGUME     : constant := 35; --  Uncorrectable memory error
   --  Signals defined for Posix 1003.1c
   SIGPTINTR    : constant := 47;
   SIGPTRESCHED : constant := 48;
   --  Posix 1003.1b signals
   SIGRTMIN   : constant := 49; --  Posix 1003.1b signals
   SIGRTMAX   : constant := 64; --  Posix 1003.1b signals

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

   type array_type_2 is array (Integer range 0 .. 1) of int;
   type struct_sigaction is record
      sa_flags     : int;
      sa_handler   : System.Address;
      sa_mask      : sigset_t;
      sa_resv      : array_type_2;
   end record;
   pragma Convention (C, struct_sigaction);

   type struct_sigaction_ptr is access all struct_sigaction;

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;

   function sigaction
     (sig  : Signal;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr := null) return int;
   pragma Import (C, sigaction, "sigaction");

   ----------
   -- Time --
   ----------

   type timespec is private;
   type timespec_ptr is access all timespec;

   type clockid_t is private;

   CLOCK_REALTIME  : constant clockid_t;
   CLOCK_SGI_FAST  : constant clockid_t;
   CLOCK_SGI_CYCLE : constant clockid_t;

   SGI_CYCLECNTR_SIZE : constant := 165;

   function syssgi (request : Interfaces.C.int) return Interfaces.C.ptrdiff_t;
   pragma Import (C, syssgi, "syssgi");

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   function clock_getres
     (clock_id : clockid_t;
      tp       : access timespec) return int;
   pragma Import (C, clock_getres, "clock_getres");

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   type struct_timeval is private;

   function To_Duration (TV : struct_timeval) return Duration;
   pragma Inline (To_Duration);

   function To_Timeval (D : Duration) return struct_timeval;
   pragma Inline (To_Timeval);

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;
   SCHED_TS    : constant := 3;
   SCHED_OTHER : constant := 3;
   SCHED_NP    : constant := 4;

   function sched_get_priority_min (Policy : int) return int;
   pragma Import (C, sched_get_priority_min, "sched_get_priority_min");

   function sched_get_priority_max (Policy : int) return int;
   pragma Import (C, sched_get_priority_max, "sched_get_priority_max");

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

   type pthread_t           is private;
   subtype Thread_Id        is pthread_t;

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

   Alternate_Stack_Size : constant := 0;
   --  No alternate signal stack is used on this platform

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
   pragma Import (C, pthread_mutexattr_setprioceiling);

   type struct_sched_param is record
      sched_priority : int;
   end record;
   pragma Convention (C, struct_sched_param);

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
   pragma Import
     (C, pthread_attr_setinheritsched, "pthread_attr_setinheritsched");

   function pthread_attr_setschedpolicy
     (attr   : access pthread_attr_t;
      policy : int) return int;
   pragma Import (C, pthread_attr_setschedpolicy);

   function pthread_attr_setschedparam
     (attr        : access pthread_attr_t;
      sched_param : access struct_sched_param)
     return int;
   pragma Import (C, pthread_attr_setschedparam, "pthread_attr_setschedparam");

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

   -------------------
   -- SGI Additions --
   -------------------

   --  Non portable SGI 6.5 additions to the pthread interface must be
   --  executed from within the context of a system scope task.

   function pthread_setrunon_np (cpu : int) return int;
   pragma Import (C, pthread_setrunon_np, "pthread_setrunon_np");

private

   type array_type_1 is array (Integer range 0 .. 3) of unsigned;
   type sigset_t is record
      X_X_sigbits : array_type_1;
   end record;
   pragma Convention (C, sigset_t);

   type pid_t is new long;

   type time_t is new long;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is new int;
   CLOCK_REALTIME  : constant clockid_t := 1;
   CLOCK_SGI_CYCLE : constant clockid_t := 2;
   CLOCK_SGI_FAST  : constant clockid_t := 3;

   type struct_timeval is record
      tv_sec  : time_t;
      tv_usec : time_t;
   end record;
   pragma Convention (C, struct_timeval);

   type array_type_9 is array (Integer range 0 .. 4) of long;
   type pthread_attr_t is record
      X_X_D : array_type_9;
   end record;
   pragma Convention (C, pthread_attr_t);

   type array_type_8 is array (Integer range 0 .. 1) of long;
   type pthread_condattr_t is record
      X_X_D : array_type_8;
   end record;
   pragma Convention (C, pthread_condattr_t);

   type array_type_7 is array (Integer range 0 .. 1) of long;
   type pthread_mutexattr_t is record
      X_X_D : array_type_7;
   end record;
   pragma Convention (C, pthread_mutexattr_t);

   type pthread_t is new unsigned;

   type array_type_10 is array (Integer range 0 .. 7) of long;
   type pthread_mutex_t is record
      X_X_D : array_type_10;
   end record;
   pragma Convention (C, pthread_mutex_t);

   type array_type_11 is array (Integer range 0 .. 7) of long;
   type pthread_cond_t is record
      X_X_D : array_type_11;
   end record;
   pragma Convention (C, pthread_cond_t);

   type pthread_key_t is new int;

end System.OS_Interface;
