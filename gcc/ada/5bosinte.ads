------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1997-2003 Free Software Foundation, Inc.          --
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

--  This is a AIX (Native THREADS) version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lpthreads");
   pragma Linker_Options ("-lc_r");

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
   ETIMEDOUT : constant := 78;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 63;
   type Signal is new int range 0 .. Max_Interrupt;
   for Signal'Size use int'Size;

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
   SIGUSR1     : constant := 30; --  user defined signal 1
   SIGUSR2     : constant := 31; --  user defined signal 2
   SIGCLD      : constant := 20; --  alias for SIGCHLD
   SIGCHLD     : constant := 20; --  child status change
   SIGPWR      : constant := 29; --  power-fail restart
   SIGWINCH    : constant := 28; --  window size change
   SIGURG      : constant := 16; --  urgent condition on IO channel
   SIGPOLL     : constant := 23; --  pollable event occurred
   SIGIO       : constant := 23; --  I/O possible (Solaris SIGPOLL alias)
   SIGSTOP     : constant := 17; --  stop (cannot be caught or ignored)
   SIGTSTP     : constant := 18; --  user stop requested from tty
   SIGCONT     : constant := 19; --  stopped process has been continued
   SIGTTIN     : constant := 21; --  background tty read attempted
   SIGTTOU     : constant := 22; --  background tty write attempted
   SIGVTALRM   : constant := 34; --  virtual timer expired
   SIGPROF     : constant := 32; --  profiling timer expired
   SIGXCPU     : constant := 24; --  CPU time limit exceeded
   SIGXFSZ     : constant := 25; --  filesize limit exceeded
   SIGWAITING  : constant := 39; --  m:n scheduling

   --  the following signals are AIX specific
   SIGMSG      : constant := 27; -- input data is in the ring buffer
   SIGDANGER   : constant := 33; -- system crash imminent
   SIGMIGRATE  : constant := 35; -- migrate process
   SIGPRE      : constant := 36; -- programming exception
   SIGVIRT     : constant := 37; -- AIX virtual time alarm
   SIGALRM1    : constant := 38; -- m:n condition variables
   SIGKAP      : constant := 60; -- keep alive poll from native keyboard
   SIGGRANT    : constant := SIGKAP; -- monitor mode granted
   SIGRETRACT  : constant := 61; -- monitor mode should be relinguished
   SIGSOUND    : constant := 62; -- sound control has completed
   SIGSAK      : constant := 63; -- secure attention key

   SIGADAABORT : constant := SIGTERM;
   --  Note: on other targets, we usually use SIGABRT, but on AiX, it
   --  appears that SIGABRT can't be used in sigwait(), so we use SIGTERM.

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked    : constant Signal_Set :=
     (SIGTRAP, SIGTTIN, SIGTTOU, SIGTSTP, SIGPROF);
   Reserved    : constant Signal_Set := (SIGABRT, SIGKILL, SIGSTOP);

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

   SA_SIGINFO  : constant := 16#0100#;

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

   Time_Slice_Supported : constant Boolean := False;
   --  Indicates wether time slicing is supported

   type timespec is private;

   type clockid_t is private;

   CLOCK_REALTIME : constant clockid_t;

   function clock_gettime
     (clock_id : clockid_t;
      tp       : access timespec) return int;
   --  AiX threads don't have clock_gettime
   --  We instead use gettimeofday()

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

   type struct_timeval is private;
   --  This is needed on systems that do not have clock_gettime()
   --  but do have gettimeofday().

   function To_Duration (TV : struct_timeval) return Duration;
   pragma Inline (To_Duration);

   function To_Timeval (D : Duration) return struct_timeval;
   pragma Inline (To_Timeval);

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 1;
   SCHED_RR    : constant := 2;
   SCHED_OTHER : constant := 0;

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
   pragma Import (C, lwp_self, "thread_self");

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

   PTHREAD_CREATE_DETACHED : constant := 1;

   -----------
   -- Stack --
   -----------

   Stack_Base_Available : constant Boolean := False;
   --  Indicates wether the stack base is available on this target.

   function Get_Stack_Base (thread : pthread_t) return Address;
   pragma Inline (Get_Stack_Base);
   --  returns the stack base of the specified thread.
   --  Only call this function when Stack_Base_Available is True.

   function Get_Page_Size return size_t;
   function Get_Page_Size return Address;
   pragma Import (C, Get_Page_Size, "getpagesize");
   --  returns the size of a page, or 0 if this is not relevant on this
   --  target

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

   --  Though not documented, pthread_init *must* be called before any other
   --  pthread call

   procedure pthread_init;
   pragma Import (C, pthread_init, "pthread_init");

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

   type sigset_t_ptr is access all sigset_t;

   function pthread_sigmask
     (how  : int;
      set  : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   pragma Import (C, pthread_sigmask, "sigthreadmask");

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

   ----------------------------
   --  POSIX.1c  Section 13  --
   ----------------------------

   PTHREAD_PRIO_NONE    : constant := 0;
   PTHREAD_PRIO_PROTECT : constant := 0;
   PTHREAD_PRIO_INHERIT : constant := 0;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol);

   function pthread_mutexattr_setprioceiling
     (attr        : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import (C, pthread_mutexattr_setprioceiling);

   type Array_5_Int is array (0 .. 5) of int;
   type struct_sched_param is record
      sched_priority : int;
      sched_policy   : int;
      sched_reserved : Array_5_Int;
   end record;

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
   --  AiX have a nonstandard sched_yield.

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
   pragma Import (C, pthread_attr_setstacksize);

   function pthread_create
     (thread        : access pthread_t;
      attributes    : access pthread_attr_t;
      start_routine : Thread_Body;
      arg           : System.Address)
     return int;
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

   type destructor_pointer is access
      procedure (arg : System.Address);

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;
   pragma Import (C, pthread_key_create, "pthread_key_create");

private

   type sigset_t is record
      losigs : unsigned_long;
      hisigs : unsigned_long;
   end record;
   pragma Convention (C_Pass_By_Copy, sigset_t);

   type pid_t is new int;

   type time_t is new long;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 0;

   type struct_timeval is record
      tv_sec  : long;
      tv_usec : long;
   end record;
   pragma Convention (C, struct_timeval);

   type pthread_attr_t is new System.Address;
   pragma Convention (C, pthread_attr_t);
   --  typedef struct __pt_attr        *pthread_attr_t;

   type pthread_condattr_t is new System.Address;
   pragma Convention (C, pthread_condattr_t);
   --  typedef struct __pt_attr        *pthread_condattr_t;

   type pthread_mutexattr_t is new System.Address;
   pragma Convention (C, pthread_mutexattr_t);
   --  typedef struct __pt_attr        *pthread_mutexattr_t;

   type pthread_t is new System.Address;
   pragma Convention (C, pthread_t);
   --  typedef void    *pthread_t;

   type ptq_queue;
   type ptq_queue_ptr is access all ptq_queue;

   type ptq_queue is record
      ptq_next : ptq_queue_ptr;
      ptq_prev : ptq_queue_ptr;
   end record;

   type Array_3_Int is array (0 .. 3) of int;
   type pthread_mutex_t is record
        link        : ptq_queue;
        ptmtx_lock  : int;
        ptmtx_flags : long;
        protocol    : int;
        prioceiling : int;
        ptmtx_owner : pthread_t;
        mtx_id      : int;
        attr        : pthread_attr_t;
        mtx_kind    : int;
        lock_cpt    : int;
        reserved    : Array_3_Int;
   end record;
   pragma Convention (C, pthread_mutex_t);
   type pthread_mutex_t_ptr is access pthread_mutex_t;

   type pthread_cond_t is record
      link         : ptq_queue;
      ptcv_lock    : int;
      ptcv_flags   : long;
      ptcv_waiters : ptq_queue;
      cv_id        : int;
      attr         : pthread_attr_t;
      mutex        : pthread_mutex_t_ptr;
      cptwait      : int;
      reserved     : int;
   end record;
   pragma Convention (C, pthread_cond_t);

   type pthread_key_t is new unsigned;

end System.OS_Interface;
