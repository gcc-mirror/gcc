------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1999-2001 Free Software Foundation, Inc.          --
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

--  This is a UnixWare (Native THREADS) version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lthread");

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

   EAGAIN    : constant := 11;
   EINTR     : constant := 4;
   EINVAL    : constant := 22;
   ENOMEM    : constant := 12;
   ETIMEDOUT : constant := 145;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 34;
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
   SIGUSR1     : constant := 16; --  user defined signal 1
   SIGUSR2     : constant := 17; --  user defined signal 2
   SIGCLD      : constant := 18; --  alias for SIGCHLD
   SIGCHLD     : constant := 18; --  child status change
   SIGPWR      : constant := 19; --  power-fail restart
   SIGWINCH    : constant := 20; --  window size change
   SIGURG      : constant := 21; --  urgent condition on IO channel
   SIGPOLL     : constant := 22; --  pollable event occurred
   SIGIO       : constant := 22; --  I/O possible (Solaris SIGPOLL alias)
   SIGSTOP     : constant := 23; --  stop (cannot be caught or ignored)
   SIGTSTP     : constant := 24; --  user stop requested from tty
   SIGCONT     : constant := 25; --  stopped process has been continued
   SIGTTIN     : constant := 26; --  background tty read attempted
   SIGTTOU     : constant := 27; --  background tty write attempted
   SIGVTALRM   : constant := 28; --  virtual timer expired
   SIGPROF     : constant := 29; --  profiling timer expired
   SIGXCPU     : constant := 30; --  CPU time limit exceeded
   SIGXFSZ     : constant := 31; --  filesize limit exceeded
   SIGWAITING  : constant := 32; --  all LWPs blocked interruptibly notific.
   SIGLWP      : constant := 33; --  signal reserved for thread lib impl.
   SIGAIO      : constant := 34; --  Asynchronous I/O signal

   SIGADAABORT : constant := SIGABRT;
   --  Change this if you want to use another signal for task abort.
   --  SIGTERM might be a good one.

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked    : constant Signal_Set :=
     (SIGTRAP, SIGLWP, SIGWAITING, SIGTTIN, SIGTTOU, SIGTSTP, SIGPROF);
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
      sa_flags   : int;
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_resv1   : int;
      sa_resv2   : int;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;
   --  SIG_ERR : constant := -1;
   --  not used

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
   --  UnixWare threads don't have clock_gettime
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

   SCHED_FIFO  : constant := 2;
   SCHED_RR    : constant := 3;
   SCHED_OTHER : constant := 1;

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
   type pthread_t           is private;
   subtype Thread_Id        is pthread_t;

   type pthread_mutex_t     is limited private;
   type pthread_cond_t      is limited private;
   type pthread_attr_t      is limited private;
   type pthread_mutexattr_t is limited private;
   type pthread_condattr_t  is limited private;
   type pthread_key_t       is private;

   PTHREAD_CREATE_DETACHED : constant := 0;

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
   PROT_USER  : constant := 8;
   PROT_ALL   : constant := PROT_READ + PROT_WRITE + PROT_EXEC + PROT_USER;

   PROT_ON    : constant := PROT_READ;
   PROT_OFF   : constant := PROT_ALL;

   function mprotect (addr : Address; len : size_t; prot : int) return int;
   pragma Import (C, mprotect);

   -------------------------
   -- POSIX.1c  Section 3 --
   -------------------------

   function sigwait (set : access sigset_t; sig : access Signal) return int;
   pragma Inline (sigwait);
   --  UnixWare provides a non standard sigwait

   function pthread_kill (thread : pthread_t; sig : Signal) return int;
   pragma Inline (pthread_kill);
   --  UnixWare provides a non standard pthread_kill

   type sigset_t_ptr is access all sigset_t;

   function pthread_sigmask
     (how  : int;
      set  : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
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

   PTHREAD_PRIO_NONE    : constant := 1;
   PTHREAD_PRIO_INHERIT : constant := 2;
   PTHREAD_PRIO_PROTECT : constant := 3;

   function pthread_mutexattr_setprotocol
     (attr     : access pthread_mutexattr_t;
      protocol : int) return int;
   pragma Import (C, pthread_mutexattr_setprotocol);

   function pthread_mutexattr_setprioceiling
     (attr     : access pthread_mutexattr_t;
      prioceiling : int) return int;
   pragma Import (C, pthread_mutexattr_setprioceiling);

   type sched_union is record
      sched_fifo    : int;
      sched_fcfs    : int;
      sched_other   : int;
      sched_ts      : int;
      policy_params : long;
   end record;

   type struct_sched_param is record
      sched_priority    : int;
      sched_other_stuff : sched_union;
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
   pragma Import (C, pthread_attr_setstacksize);

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

   function pthread_key_create
     (key        : access pthread_key_t;
      destructor : destructor_pointer) return int;
   pragma Import (C, pthread_key_create, "pthread_key_create");

   procedure pthread_init;
   --  This is a dummy procedure to share some GNULLI files

private

   type sigbit_array is array (1 .. 4) of unsigned;
   type sigset_t is record
      sa_sigbits : sigbit_array;
   end record;
   pragma Convention (C_Pass_By_Copy, sigset_t);

   type pid_t is new unsigned;

   type time_t is new long;

   type timespec is record
      tv_sec       : time_t;
      tv_nsec      : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 0;

   type struct_timeval is record
      tv_sec       : long;
      tv_usec      : long;
   end record;
   pragma Convention (C, struct_timeval);

   type pthread_attr_t is record
      pt_attr_status          : int;
      pt_attr_stacksize       : size_t;
      pt_attr_stackaddr       : System.Address;
      pt_attr_detachstate     : int;
      pt_attr_contentionscope : int;
      pt_attr_inheritsched    : int;
      pt_attr_schedpolicy     : int;
      pt_attr_sched_param     : struct_sched_param;
      pt_attr_tlflags         : int;
   end record;
   pragma Convention (C, pthread_attr_t);

   type pthread_condattr_t is record
      pt_condattr_status  : int;
      pt_condattr_pshared : int;
   end record;
   pragma Convention (C, pthread_condattr_t);

   type pthread_mutexattr_t is record
      pt_mutexattr_status  : int;
      pt_mutexattr_pshared : int;
      pt_mutexattr_type    : int;
   end record;
   pragma Convention (C, pthread_mutexattr_t);

   type thread_t is new long;
   type pthread_t is new thread_t;

   type thrq_elt_t;
   type thrq_elt_t_ptr is access all thrq_elt_t;

   type thrq_elt_t is record
      thrq_next : thrq_elt_t_ptr;
      thrq_prev : thrq_elt_t_ptr;
   end record;
   pragma Convention (C, thrq_elt_t);

   type lwp_mutex_t is record
      wanted : char;
      lock   : unsigned_char;
   end record;
   pragma Convention (C, lwp_mutex_t);
   pragma Volatile (lwp_mutex_t);

   type mutex_t is record
      m_lmutex    : lwp_mutex_t;
      m_sync_lock : lwp_mutex_t;
      m_type      : int;
      m_sleepq    : thrq_elt_t;
      filler1     : int;
      filler2     : int;
   end record;
   pragma Convention (C, mutex_t);
   pragma Volatile (mutex_t);

   type pthread_mutex_t is record
      pt_mutex_mutex : mutex_t;
      pt_mutex_pid   : pid_t;
      pt_mutex_owner : thread_t;
      pt_mutex_depth : int;
      pt_mutex_attr  : pthread_mutexattr_t;
   end record;
   pragma Convention (C, pthread_mutex_t);

   type lwp_cond_t is record
      wanted : char;
   end record;
   pragma Convention (C, lwp_cond_t);
   pragma Volatile (lwp_cond_t);

   type cond_t is record
      c_lcond     : lwp_cond_t;
      c_sync_lock : lwp_mutex_t;
      c_type      : int;
      c_syncq     : thrq_elt_t;
   end record;
   pragma Convention (C, cond_t);
   pragma Volatile (cond_t);

   type pthread_cond_t is record
      pt_cond_cond : cond_t;
      pt_cond_attr : pthread_condattr_t;
   end record;
   pragma Convention (C, pthread_cond_t);

   type pthread_key_t is new unsigned;

end System.OS_Interface;
