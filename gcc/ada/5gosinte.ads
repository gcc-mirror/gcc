------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1997-2001 Free Software Foundation, Inc.          --
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

--  This is an Irix (old pthread library) version of this package.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

package System.OS_Interface is

   pragma Preelaborate;

   pragma Linker_Options ("-lathread");

   subtype int            is Interfaces.C.int;
   subtype short          is Interfaces.C.short;
   subtype long           is Interfaces.C.long;
   subtype unsigned       is Interfaces.C.unsigned;
   subtype unsigned_short is Interfaces.C.unsigned_short;
   subtype unsigned_long  is Interfaces.C.unsigned_long;
   subtype unsigned_char  is Interfaces.C.unsigned_char;
   subtype plain_char     is Interfaces.C.plain_char;
   subtype size_t         is Interfaces.C.size_t;
   subtype chars_ptr      is Interfaces.C.Strings.chars_ptr;

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
   SIGABRT    : constant := 6; --  used by abort, replace SIGIOT in the
   --                              future
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
   --  Signals defined for Posix 1003.1c.
   SIGPTINTR    : constant := 47;
   SIGPTRESCHED : constant := 48;
   --  Posix 1003.1b signals
   SIGRTMIN   : constant := 49; --  Posix 1003.1b signals
   SIGRTMAX   : constant := 64; --  Posix 1003.1b signals

   type sigset_t is private;
   type sigset_t_ptr is access all sigset_t;

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

   type siginfo_t is record
      si_signo     : int;
      si_code      : int;
      si_errno     : int;
      bit_field_substitute_1 : String (1 .. 116);
   end record;
   pragma Convention (C, siginfo_t);

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

   type time_t is new int;

   type timespec is record
      tv_sec  : time_t;
      tv_nsec : long;
   end record;
   pragma Convention (C, timespec);
   type timespec_ptr is access all timespec;

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   type timer_t is new Integer;
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
     (clock_id : clockid_t; tp : access timespec) return int;
   pragma Import (C, clock_getres, "clock_getres");

   type struct_timeval is record
      tv_sec  : time_t;
      tv_usec : time_t;
   end record;
   pragma Convention (C, struct_timeval);

   function To_Duration (TV : struct_timeval) return Duration;
   pragma Inline (To_Duration);

   function To_Timeval (D : Duration) return struct_timeval;
   pragma Inline (To_Timeval);

   function gettimeofday
     (tv : access struct_timeval;
      tz : System.Address := System.Null_Address) return int;
   pragma Import (C, gettimeofday, "gettimeofday");

   -------------------------
   -- Priority Scheduling --
   -------------------------

   SCHED_FIFO  : constant := 0;
   SCHED_RR    : constant := 0;
   SCHED_OTHER : constant := 0;

   -------------
   -- Process --
   -------------

   type pid_t is private;

   function kill (pid : pid_t; sig : Signal) return int;
   pragma Import (C, kill, "kill");

   function getpid return pid_t;
   pragma Import (C, getpid, "getpid");

   ---------------------------------------
   -- Nonstandard Thread Initialization --
   ---------------------------------------

   procedure pthread_init;
   pragma Inline (pthread_init);
   --  This is a dummy procedure to share some GNULLI files

   -------------
   -- Threads --
   -------------

   type Thread_Body is access
     function (arg : System.Address) return System.Address;

   type pthread_t           is private; --   thread identifier
   subtype Thread_Id        is pthread_t;

   type pthread_mutex_t     is private; --   mutex identifier
   type pthread_cond_t      is private; --   cond identifier
   type pthread_attr_t      is private; --   pthread attributes
   type pthread_mutexattr_t is private; --   mutex attributes
   type pthread_condattr_t  is private; --   mutex attributes
   type sem_t               is private; --   semaphore identifier
   type pthread_key_t       is private; --   per thread key

   subtype pthread_once_t   is int;     --   dynamic package initialization
   subtype resource_t       is long;    --   sproc. resource info.
   type start_addr is access function (arg : Address) return Address;
   type sproc_start_addr is access function (arg : Address) return int;
   type callout_addr is
     access function (arg : Address; arg1 : Address) return Address;

   --  SGI specific types

   subtype sproc_t      is Address; --   sproc identifier
   subtype sproc_attr_t is Address; --   sproc attributes

   subtype spcb_p is Address;
   subtype ptcb_p is Address;

   --  Pthread Error Types

   FUNC_OK  : constant := 0;
   FUNC_ERR : constant := -1;

   --  pthread run-time initialization data structure

   type pthread_init_struct is record
      conf_initsize       : int; --  shared area size
      max_sproc_count     : int; --  maximum number of sprocs
      sproc_stack_size    : size_t;  --  sproc stack size
      os_default_priority : int; --  default IRIX pri for main process
      os_sched_signal     : int; --  default OS scheduling signal
      guard_pages         : int; --  number of guard pages per stack
      init_sproc_count    : int; --  initial number of sprocs
   end record;

   --
   --  Pthread Attribute Initialize / Destroy
   --

   function pthread_attr_init (attr : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_init, "pthread_attr_init");

   function pthread_attr_destroy (attr : access pthread_attr_t) return int;
   pragma Import (C, pthread_attr_destroy, "pthread_attr_destroy");

   --
   --  Thread Attributes
   --

   function pthread_attr_setstacksize
     (attr : access pthread_attr_t; stacksize : size_t) return int;
   pragma Import (C, pthread_attr_setstacksize, "pthread_attr_setstacksize");

   function pthread_attr_setdetachstate
     (attr : access pthread_attr_t; detachstate : int) return int;
   pragma Import (C, pthread_attr_setdetachstate);

   function pthread_attr_setname
     (attr : access pthread_attr_t; name : chars_ptr) return int;
   pragma Import (C, pthread_attr_setname, "pthread_attr_setname");

   --
   --  Thread Scheduling Attributes
   --

   function pthread_attr_setscope
     (attr : access pthread_attr_t; contentionscope : int) return int;
   pragma Import (C, pthread_attr_setscope, "pthread_attr_setscope");

   function pthread_attr_setinheritsched
     (attr : access pthread_attr_t; inherit : int) return int;
   pragma Import
     (C, pthread_attr_setinheritsched, "pthread_attr_setinheritsched");

   function pthread_attr_setsched
     (attr : access pthread_attr_t; scheduler : int) return int;
   pragma Import (C, pthread_attr_setsched, "pthread_attr_setsched");

   function  pthread_attr_setprio
     (attr : access pthread_attr_t; priority : int) return int;
   pragma Import (C, pthread_attr_setprio, "pthread_attr_setprio");

   --
   --  SGI Extensions to Thread Attributes
   --

   --  Bound to sproc attribute values

   PTHREAD_BOUND     : constant := 1;
   PTHREAD_NOT_BOUND : constant := 0;

   function pthread_attr_setresources
     (attr : access pthread_attr_t; resources : resource_t) return int;
   pragma Import (C, pthread_attr_setresources, "pthread_attr_setresources");

   function pthread_attr_set_boundtosproc
     (attr : access pthread_attr_t; bound_to_sproc : int) return int;
   pragma Import
     (C, pthread_attr_set_boundtosproc, "pthread_attr_set_boundtosproc");

   function pthread_attr_set_bsproc
     (attr : access pthread_attr_t; bsproc : spcb_p) return int;
   pragma Import (C, pthread_attr_set_bsproc, "pthread_attr_set_bsproc");

   function pthread_attr_set_tslice
     (attr        : access pthread_attr_t;
      ts_interval : access struct_timeval) return int;
   pragma Import (C, pthread_attr_set_tslice, "pthread_attr_set_tslice");

   --
   --  Thread Creation & Management
   --

   function pthread_create
     (thread        : access pthread_t;
      attr          : access pthread_attr_t;
      start_routine : start_addr;
      arg           : Address) return int;
   pragma Import (C, pthread_create, "pthread_create");

   procedure pthread_exit (status : Address);
   pragma Import (C, pthread_exit, "pthread_exit");

   procedure pthread_yield (arg : Address := System.Null_Address);
   pragma Import (C, pthread_yield, "pthread_yield");

   function pthread_self return pthread_t;
   pragma Import (C, pthread_self, "pthread_self");

   function pthread_kill (thread : pthread_t; sig : int) return int;
   pragma Import (C, pthread_kill, "pthread_kill");

   --
   --  SGI Extensions to POSIX thread operations
   --

   function pthread_setprio (thread : pthread_t; priority : int) return int;
   pragma Import (C, pthread_setprio, "pthread_setprio");

   function pthread_suspend (thread : pthread_t) return int;
   pragma Import (C, pthread_suspend, "pthread_suspend");

   function pthread_resume (thread : pthread_t) return int;
   pragma Import (C, pthread_resume, "pthread_resume");

   function pthread_get_current_ada_tcb return Address;
   pragma Import (C, pthread_get_current_ada_tcb);

   function pthread_set_ada_tcb
     (thread : pthread_t; data : Address) return int;
   pragma Import (C, pthread_set_ada_tcb, "pthread_set_ada_tcb");

   --  Mutex Initialization / Destruction

   function pthread_mutexattr_init
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_init, "pthread_mutexattr_init");

   function pthread_mutexattr_destroy
     (attr : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutexattr_destroy, "pthread_mutexattr_destroy");

   function pthread_mutexattr_setqueueorder
     (attr : access pthread_mutexattr_t; order : int) return int;
   pragma Import (C, pthread_mutexattr_setqueueorder);

   function pthread_mutexattr_setceilingprio
     (attr : access pthread_mutexattr_t; priority : int) return int;
   pragma Import (C, pthread_mutexattr_setceilingprio);

   --  Mutex Attributes

   --  Threads queueing order

   MUTEX_PRIORITY         : constant := 0; --   wait in priority order
   MUTEX_FIFO             : constant := 1; --   first-in-first-out
   MUTEX_PRIORITY_INHERIT : constant := 2; --   priority inhertance mutex
   MUTEX_PRIORITY_CEILING : constant := 3; --   priority ceiling mutex

   --  Mutex debugging options

   MUTEX_NO_DEBUG  : constant := 0; --   no debugging on mutex
   MUTEX_DEBUG     : constant := 1; --   debugging is on

   --  Mutex spin on lock operations

   MUTEX_NO_SPIN   : constant := 0;  --   no spin, try once only
   MUTEX_SPIN_ONLY : constant := -1; --   spin forever
   --  cnt > 0, limited spin
   --  Mutex sharing attributes

   MUTEX_SHARED    : constant := 0; --   shared between processes
   MUTEX_NOTSHARED : constant := 1; --   not shared between processes

   --  Mutex Operations

   function pthread_mutex_init
     (mutex : access pthread_mutex_t;
      attr  : access pthread_mutexattr_t) return int;
   pragma Import (C, pthread_mutex_init, "pthread_mutex_init");

   function pthread_mutex_destroy
     (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_destroy, "pthread_mutex_destroy");

   function pthread_mutex_lock
     (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_lock, "pthread_mutex_lock");

   function pthread_mutex_unlock
     (mutex : access pthread_mutex_t) return int;
   pragma Import (C, pthread_mutex_unlock, "pthread_mutex_unlock");

   --  Condition Initialization / Destruction

   function pthread_condattr_init
     (attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_condattr_init, "pthread_condattr_init");

   function pthread_condattr_destroy
     (attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_condattr_destroy, "pthread_condattr_destroy");

   --  Condition Attributes

   COND_PRIORITY  : constant := 0; --   wait in priority order
   COND_FIFO      : constant := 1; --   first-in-first-out

   --  Condition debugging options

   COND_NO_DEBUG  : constant := 0; --   no debugging on mutex
   COND_DEBUG     : constant := 1; --   debugging is on

   --  Condition sharing attributes

   COND_SHARED    : constant := 0; --   shared between processes
   COND_NOTSHARED : constant := 1; --   not shared between processes

   --  Condition Operations

   function pthread_cond_init
     (cond : access pthread_cond_t;
      attr : access pthread_condattr_t) return int;
   pragma Import (C, pthread_cond_init, "pthread_cond_init");

   function pthread_cond_destroy
     (cond : access pthread_cond_t) return int;
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
      abstime : access struct_timeval) return int;
   pragma Import (C, pthread_cond_timedwait, "pthread_cond_timedwait");

   --  Thread-Specific Data

   type foo_h_proc_1 is access procedure (value : Address);

   function pthread_key_create
     (key : access pthread_key_t; destructor : foo_h_proc_1) return int;
   pragma Import (C, pthread_key_create, "pthread_key_create");

   function pthread_setspecific
     (key : pthread_key_t; value : Address) return int;
   pragma Import (C, pthread_setspecific, "pthread_setspecific");

   function pthread_getspecific
     (key : pthread_key_t; value : access Address) return int;
   pragma Import (C, pthread_getspecific, "pthread_getspecific");

   type foo_h_proc_2 is access procedure;

   function pthread_exec_begin (init : access pthread_init_struct) return int;
   pragma Import (C, pthread_exec_begin, "pthread_exec_begin");

   function sproc_create
     (sproc_id      : access sproc_t;
      attr          : access sproc_attr_t;
      start_routine : sproc_start_addr;
      arg           : Address) return int;
   pragma Import (C, sproc_create, "sproc_create");

   function sproc_self return sproc_t;
   pragma Import (C, sproc_self, "sproc_self");

   --  if equal fast TRUE is returned - common case
   --  if not equal thread resource must NOT be null in order to compare bits

   --
   --  Sproc attribute initialize / destroy
   --

   function sproc_attr_init (attr : access sproc_attr_t) return int;
   pragma Import (C, sproc_attr_init, "sproc_attr_init");

   function sproc_attr_destroy (attr : access sproc_attr_t) return int;
   pragma Import (C, sproc_attr_destroy, "sproc_attr_destroy");

   function sproc_attr_setresources
     (attr : access sproc_attr_t; resources : resource_t) return int;
   pragma Import (C, sproc_attr_setresources, "sproc_attr_setresources");

   function sproc_attr_getresources
     (attr      : access sproc_attr_t;
      resources : access resource_t) return int;
   pragma Import (C, sproc_attr_getresources, "sproc_attr_getresources");

   function sproc_attr_setcpu
     (attr : access sproc_attr_t; cpu_num : int) return int;
   pragma Import (C, sproc_attr_setcpu, "sproc_attr_setcpu");

   function sproc_attr_getcpu
     (attr : access sproc_attr_t; cpu_num : access int) return int;
   pragma Import (C, sproc_attr_getcpu, "sproc_attr_getcpu");

   function sproc_attr_setresident
     (attr : access sproc_attr_t; resident : int) return int;
   pragma Import (C, sproc_attr_setresident, "sproc_attr_setresident");

   function sproc_attr_getresident
     (attr : access sproc_attr_t; resident : access int) return int;
   pragma Import (C, sproc_attr_getresident, "sproc_attr_getresident");

   function sproc_attr_setname
     (attr : access sproc_attr_t; name : chars_ptr) return int;
   pragma Import (C, sproc_attr_setname, "sproc_attr_setname");

   function sproc_attr_getname
     (attr : access sproc_attr_t; name : chars_ptr) return int;
   pragma Import (C, sproc_attr_getname, "sproc_attr_getname");

   function sproc_attr_setstacksize
     (attr : access sproc_attr_t; stacksize : size_t) return int;
   pragma Import (C, sproc_attr_setstacksize, "sproc_attr_setstacksize");

   function sproc_attr_getstacksize
     (attr : access sproc_attr_t; stacksize : access size_t) return int;
   pragma Import (C, sproc_attr_getstacksize, "sproc_attr_getstacksize");

   function sproc_attr_setprio
     (attr : access sproc_attr_t; priority : int) return int;
   pragma Import (C, sproc_attr_setprio, "sproc_attr_setprio");

   function sproc_attr_getprio
     (attr : access sproc_attr_t; priority : access int) return int;
   pragma Import (C, sproc_attr_getprio, "sproc_attr_getprio");

   function sproc_attr_setbthread
     (attr : access sproc_attr_t; bthread : ptcb_p) return int;
   pragma Import (C, sproc_attr_setbthread, "sproc_attr_setbthread");

   function sproc_attr_getbthread
     (attr : access sproc_attr_t; bthread : access ptcb_p) return int;
   pragma Import (C, sproc_attr_getbthread, "sproc_attr_getbthread");

   SPROC_NO_RESOURCES : constant := 0;
   SPROC_ANY_CPU      : constant := -1;
   SPROC_MY_PRIORITY  : constant := -1;
   SPROC_SWAPPED      : constant := 0;
   SPROC_RESIDENT     : constant := 1;

   type isr_address is access procedure;

   function intr_attach (sig : int; isr : isr_address) return int;
   pragma Import (C, intr_attach, "intr_attach");

   Intr_Attach_Reset : constant Boolean := False;
   --  True if intr_attach is reset after an interrupt handler is called

   function intr_exchange
     (sig  : int;
      isr  : isr_address;
      oisr : access isr_address) return int;
   pragma Import (C, intr_exchange, "intr_exchange");

   function intr_current_isr
     (sig  : int;
      oisr : access isr_address)
     return int;
   pragma Import (C, intr_current_isr, "intr_current_isr");

private

   type clockid_t is new int;

   CLOCK_REALTIME  : constant clockid_t := 1;
   CLOCK_SGI_CYCLE : constant clockid_t := 2;
   CLOCK_SGI_FAST  : constant clockid_t := 3;

   type pthread_t           is new Address; --   thread identifier
   type pthread_mutex_t     is new Address; --   mutex identifier
   type pthread_cond_t      is new Address; --   cond identifier
   type pthread_attr_t      is new Address; --   pthread attributes
   type pthread_mutexattr_t is new Address; --   mutex attributes
   type pthread_condattr_t  is new Address; --   mutex attributes
   type sem_t               is new Address; --   semaphore identifier
   type pthread_key_t       is new Address; --   per thread key

   type sigbits_t is array (Integer range 0 .. 3) of unsigned;
   type sigset_t is record
      sigbits : sigbits_t;
   end record;
   pragma Convention (C, sigset_t);

   type pid_t is new long;

end System.OS_Interface;
