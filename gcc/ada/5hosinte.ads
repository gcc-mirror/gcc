------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--             Copyright (C) 1995-2003, Ada Core Technologies               --
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
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the HP-UX version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lcma");

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
   ETIME     : constant := 52;
   ETIMEDOUT : constant := 238;

   FUNC_ERR : constant := -1;

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

   SIGADAABORT : constant := SIGABRT;
   --  Note: on other targets, we usually use SIGABRT, but on HP/UX, it
   --  appears that SIGABRT can't be used in sigwait(), so we use SIGTERM.

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked    : constant Signal_Set :=
     (SIGBUS, SIGTRAP, SIGTTIN, SIGTTOU, SIGTSTP);

   Reserved    : constant Signal_Set := (SIGKILL, SIGSTOP);

   type sigset_t is private;

   type isr_address is access procedure (sig : int);

   function intr_attach (sig : int; handler : isr_address) return long;

   Intr_Attach_Reset : constant Boolean := True;
   --  True if intr_attach is reset after an interrupt handler is called

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

   type Signal_Handler is access procedure (signo : Signal);

   type struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   SA_RESTART  : constant  := 16#40#;
   SA_SIGINFO  : constant  := 16#10#;

   SIG_BLOCK   : constant  := 0;
   SIG_UNBLOCK : constant  := 1;
   SIG_SETMASK : constant  := 2;

   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;
   SIG_ERR : constant := -1;

   function sigaction
     (sig  : Signal;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   ----------
   -- Time --
   ----------

   type timespec is private;

   function nanosleep (rqtp, rmtp : access timespec) return int;
   pragma Import (C, nanosleep);

   type clockid_t is private;

   CLOCK_REALTIME : constant clockid_t;

   function Clock_Gettime
     (Clock_Id : clockid_t; Tp : access timespec) return int;
   pragma Import (C, Clock_Gettime);

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

   SCHED_FIFO  : constant := 0;
   SCHED_RR    : constant := 1;
   SCHED_OTHER : constant := 2;

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
   type pthread_t           is private;
   subtype Thread_Id        is pthread_t;

   type pthread_mutex_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   -----------
   -- Stack --
   -----------

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

   function sigwait (set : access sigset_t) return int;
   pragma Import (C, sigwait, "cma_sigwait");

   function sigwait
     (set : access sigset_t;
      sig : access Signal) return int;
   pragma Inline (sigwait);
   --  DCE_THREADS has a nonstandard sigwait

   function pthread_kill
     (thread : pthread_t;
      sig    : Signal) return int;
   pragma Inline (pthread_kill);
   --  DCE_THREADS doesn't have pthread_kill

   type sigset_t_ptr is access all sigset_t;

   function pthread_sigmask
     (how  : int;
      set  : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   --  DCE THREADS does not have pthread_sigmask. Instead, it uses
   --  sigprocmask to do the signal handling when the thread library is
   --  sucked in.
   pragma Import (C, pthread_sigmask, "sigprocmask");

   --------------------------
   -- POSIX.1c  Section 11 --
   --------------------------

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int;
   --  DCE_THREADS has a nonstandard pthread_mutexattr_init.

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int;
   --  DCE_THREADS has a nonstandard pthread_mutexattr_destroy

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int;
   --  DCE_THREADS has a nonstandard pthread_mutex_init

   function pthread_mutex_destroy (mutex : access pthread_mutex_t) return int;
   --  DCE_THREADS has a nonstandard pthread_mutex_destroy

   function pthread_mutex_lock (mutex : access pthread_mutex_t) return int;
   pragma Inline (pthread_mutex_lock);
   --  DCE_THREADS has nonstandard pthread_mutex_lock

   function pthread_mutex_unlock (mutex : access pthread_mutex_t) return int;
   pragma Inline (pthread_mutex_unlock);
   --  DCE_THREADS has nonstandard pthread_mutex_lock

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int;
   --  DCE_THREADS has nonstandard pthread_condattr_init

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int;
   --  DCE_THREADS has nonstandard pthread_condattr_destroy

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int;
   --  DCE_THREADS has nonstandard pthread_cond_init

   function pthread_cond_destroy (cond : access pthread_cond_t) return int;
   --  DCE_THREADS has nonstandard pthread_cond_destroy

   function pthread_cond_signal (cond : access pthread_cond_t) return int;
   pragma Inline (pthread_cond_signal);
   --  DCE_THREADS has nonstandard pthread_cond_signal

   function pthread_cond_wait
     (cond  : access pthread_cond_t;
      mutex : access pthread_mutex_t) return int;
   pragma Inline (pthread_cond_wait);
   --  DCE_THREADS has a nonstandard pthread_cond_wait

   function pthread_cond_timedwait
     (cond    : access pthread_cond_t;
      mutex   : access pthread_mutex_t;
      abstime : access timespec) return int;
   pragma Inline (pthread_cond_timedwait);
   --  DCE_THREADS has a nonstandard pthread_cond_timedwait

   --------------------------
   -- POSIX.1c  Section 13 --
   --------------------------

   type struct_sched_param is record
      sched_priority : int;  --  scheduling priority
   end record;

   function pthread_setschedparam
     (thread : pthread_t;
      policy : int;
      param  : access struct_sched_param) return int;
   pragma Inline (pthread_setschedparam);
   --  DCE_THREADS has a nonstandard pthread_setschedparam

   function sched_yield return int;
   pragma Inline (sched_yield);
   --  DCE_THREADS has a nonstandard sched_yield

   ---------------------------
   -- P1003.1c - Section 16 --
   ---------------------------

   function pthread_attr_init (attributes : access pthread_attr_t) return int;
   pragma Inline (pthread_attr_init);
   --  DCE_THREADS has a nonstandard pthread_attr_init

   function pthread_attr_destroy
     (attributes : access pthread_attr_t) return int;
   pragma Inline (pthread_attr_destroy);
   --  DCE_THREADS has a nonstandard pthread_attr_destroy

   function pthread_attr_setstacksize
     (attr      : access pthread_attr_t;
      stacksize : size_t) return int;
   pragma Inline (pthread_attr_setstacksize);
   --  DCE_THREADS has a nonstandard pthread_attr_setstacksize

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address) return int;
   pragma Inline (pthread_create);
   --  DCE_THREADS has a nonstandard pthread_create

   procedure pthread_detach (thread : access pthread_t);
   pragma Import (C, pthread_detach);

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
   pragma Inline (pthread_setspecific);
   --  DCE_THREADS has a nonstandard pthread_setspecific

   function pthread_getspecific (key : pthread_key_t) return System.Address;
   pragma Inline (pthread_getspecific);
   --  DCE_THREADS has a nonstandard pthread_getspecific

   type destructor_pointer is access procedure (arg : System.Address);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;
   pragma Inline (pthread_key_create);
   --  DCE_THREADS has a nonstandard pthread_key_create

private

   type array_type_1 is array (Integer range 0 .. 7) of unsigned_long;
   type sigset_t is record
      X_X_sigbits : array_type_1;
   end record;
   pragma Convention (C, sigset_t);

   type pid_t is new int;

   type time_t is new long;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 1;

   type struct_timeval is record
      tv_sec  : time_t;
      tv_usec : time_t;
   end record;
   pragma Convention (C, struct_timeval);

   type cma_t_address is new System.Address;

   type cma_t_handle is record
      field1 : cma_t_address;
      field2 : Short_Integer;
      field3 : Short_Integer;
   end record;
   for cma_t_handle'Size use 64;

   type pthread_attr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_attr_t);

   type pthread_condattr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_condattr_t);

   type pthread_mutexattr_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_mutexattr_t);

   type pthread_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_t);

   type pthread_mutex_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_mutex_t);

   type pthread_cond_t is new cma_t_handle;
   pragma Convention (C_Pass_By_Copy, pthread_cond_t);

   type pthread_key_t is new int;

end System.OS_Interface;
