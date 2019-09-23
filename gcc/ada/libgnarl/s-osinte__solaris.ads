------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-2017, Florida State University            --
--          Copyright (C) 1995-2019, Free Software Foundation, Inc.         --
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

--  This is a Solaris (native) version of this package

--  This package includes all direct interfaces to OS services
--  that are needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Interfaces.C;

with Ada.Unchecked_Conversion;

package System.OS_Interface is
   pragma Preelaborate;

   pragma Linker_Options ("-lposix4");
   pragma Linker_Options ("-lthread");

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
   ETIME     : constant := 62;
   ETIMEDOUT : constant := 145;

   -------------
   -- Signals --
   -------------

   Max_Interrupt : constant := 45;
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
   SIGWAITING : constant := 32; --  process's lwps blocked (Solaris)
   SIGLWP     : constant := 33; --  used by thread library (Solaris)
   SIGFREEZE  : constant := 34; --  used by CPR (Solaris)
   SIGTHAW    : constant := 35; --  used by CPR (Solaris)
   SIGCANCEL  : constant := 36; --  thread cancellation signal (libthread)

   type Signal_Set is array (Natural range <>) of Signal;

   Unmasked : constant Signal_Set := (SIGTRAP, SIGLWP, SIGPROF);

   --  Following signals should not be disturbed.
   --  See c-posix-signals.c in FLORIST.

   Reserved : constant Signal_Set :=
     (SIGKILL, SIGSTOP, SIGWAITING, SIGCANCEL, SIGTRAP, SIGSEGV);

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
      si_signo     : int;
      si_code      : int;
      si_errno     : int;
      X_data       : union_type_3;
   end record;
   pragma Convention (C, siginfo_t);

   --  The types mcontext_t and gregset_t are part of the ucontext_t
   --  information, which is specific to Solaris2.4 for SPARC
   --  The ucontext_t info seems to be used by the handler
   --  for SIGSEGV to decide whether it is a Storage_Error (stack overflow) or
   --  a Constraint_Error (bad pointer).  The original code that did this
   --  is suspect, so it is not clear whether we really need this part of
   --  the signal context information, or perhaps something else.
   --  More analysis is needed, after which these declarations may need to
   --  be changed.

   type greg_t is new int;

   type gregset_t is array (0 .. 18) of greg_t;

   type union_type_2 is new String (1 .. 128);
   type record_type_1 is record
      fpu_fr       : union_type_2;
      fpu_q        : System.Address;
      fpu_fsr      : unsigned;
      fpu_qcnt     : unsigned_char;
      fpu_q_entrysize  : unsigned_char;
      fpu_en       : unsigned_char;
   end record;
   pragma Convention (C, record_type_1);

   type array_type_7 is array (Integer range 0 .. 20) of long;
   type mcontext_t is record
      gregs        : gregset_t;
      gwins        : System.Address;
      fpregs       : record_type_1;
      filler       : array_type_7;
   end record;
   pragma Convention (C, mcontext_t);

   type record_type_2 is record
      ss_sp        : System.Address;
      ss_size      : int;
      ss_flags     : int;
   end record;
   pragma Convention (C, record_type_2);

   type array_type_8 is array (Integer range 0 .. 22) of long;
   type ucontext_t is record
      uc_flags     : unsigned_long;
      uc_link      : System.Address;
      uc_sigmask   : sigset_t;
      uc_stack     : record_type_2;
      uc_mcontext  : mcontext_t;
      uc_filler    : array_type_8;
   end record;
   pragma Convention (C, ucontext_t);

   type Signal_Handler is access procedure
     (signo   : Signal;
      info    : access siginfo_t;
      context : access ucontext_t);

   type union_type_1 is new plain_char;
   type array_type_2 is array (Integer range 0 .. 1) of int;
   type struct_sigaction is record
      sa_flags   : int;
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_resv    : array_type_2;
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
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   ----------
   -- Time --
   ----------

   type timespec is private;

   type clockid_t is new int;

   function clock_gettime
     (clock_id : clockid_t; tp : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   function clock_getres
     (clock_id : clockid_t; res : access timespec) return int;
   pragma Import (C, clock_getres, "clock_getres");

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   function sysconf (name : int) return long;
   pragma Import (C, sysconf);

   SC_NPROCESSORS_ONLN : constant := 15;

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

   THR_DETACHED  : constant := 64;
   THR_BOUND     : constant := 1;
   THR_NEW_LWP   : constant := 2;
   USYNC_THREAD  : constant := 0;

   type thread_t is new unsigned;
   subtype Thread_Id is thread_t;
   --  These types should be commented ???

   function To_thread_t is new Ada.Unchecked_Conversion (Integer, thread_t);

   type mutex_t is limited private;

   type cond_t is limited private;

   type thread_key_t is private;

   function thr_create
     (stack_base    : System.Address;
      stack_size    : size_t;
      start_routine : Thread_Body;
      arg           : System.Address;
      flags         : int;
      new_thread    : access thread_t) return int;
   pragma Import (C, thr_create, "thr_create");

   function thr_min_stack return size_t;
   pragma Import (C, thr_min_stack, "thr_min_stack");

   function thr_self return thread_t;
   pragma Import (C, thr_self, "thr_self");

   function mutex_init
     (mutex : access mutex_t;
      mtype : int;
      arg   : System.Address) return int;
   pragma Import (C, mutex_init, "mutex_init");

   function mutex_destroy (mutex : access mutex_t) return int;
   pragma Import (C, mutex_destroy, "mutex_destroy");

   function mutex_lock (mutex : access mutex_t) return int;
   pragma Import (C, mutex_lock, "mutex_lock");

   function mutex_unlock (mutex : access mutex_t) return int;
   pragma Import (C, mutex_unlock, "mutex_unlock");

   function cond_init
     (cond  : access cond_t;
      ctype : int;
      arg   : int) return int;
   pragma Import (C, cond_init, "cond_init");

   function cond_wait
     (cond : access cond_t; mutex : access mutex_t) return int;
   pragma Import (C, cond_wait, "cond_wait");

   function cond_timedwait
     (cond    : access cond_t;
      mutex   : access mutex_t;
      abstime : access timespec) return int;
   pragma Import (C, cond_timedwait, "cond_timedwait");

   function cond_signal (cond : access cond_t) return int;
   pragma Import (C, cond_signal, "cond_signal");

   function cond_destroy (cond : access cond_t) return int;
   pragma Import (C, cond_destroy, "cond_destroy");

   function thr_setspecific
     (key : thread_key_t; value : System.Address) return int;
   pragma Import (C, thr_setspecific, "thr_setspecific");

   function thr_getspecific
     (key   : thread_key_t;
      value : access System.Address) return int;
   pragma Import (C, thr_getspecific, "thr_getspecific");

   function thr_keycreate
     (key : access thread_key_t; destructor : System.Address) return int;
   pragma Import (C, thr_keycreate, "thr_keycreate");

   function thr_setprio (thread : thread_t; priority : int) return int;
   pragma Import (C, thr_setprio, "thr_setprio");

   procedure thr_exit (status : System.Address);
   pragma Import (C, thr_exit, "thr_exit");

   function thr_setconcurrency (new_level : int) return int;
   pragma Import (C, thr_setconcurrency, "thr_setconcurrency");

   function sigwait (set : access sigset_t; sig : access Signal) return int;
   pragma Import (C, sigwait, "__posix_sigwait");

   function thr_kill (thread : thread_t; sig : Signal) return int;
   pragma Import (C, thr_kill, "thr_kill");

   function thr_sigsetmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t) return int;
   pragma Import (C, thr_sigsetmask, "thr_sigsetmask");

   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t) return int;
   pragma Import (C, pthread_sigmask, "thr_sigsetmask");

   function thr_suspend (target_thread : thread_t) return int;
   pragma Import (C, thr_suspend, "thr_suspend");

   function thr_continue (target_thread : thread_t) return int;
   pragma Import (C, thr_continue, "thr_continue");

   procedure thr_yield;
   pragma Import (C, thr_yield, "thr_yield");

   ---------
   -- LWP --
   ---------

   P_PID   : constant := 0;
   P_LWPID : constant := 8;

   PC_GETCID    : constant := 0;
   PC_GETCLINFO : constant := 1;
   PC_SETPARMS  : constant := 2;
   PC_GETPARMS  : constant := 3;
   PC_ADMIN     : constant := 4;

   PC_CLNULL : constant := -1;

   RT_NOCHANGE : constant := -1;
   RT_TQINF    : constant := -2;
   RT_TQDEF    : constant := -3;

   PC_CLNMSZ : constant := 16;

   PC_VERSION : constant := 1;

   type lwpid_t is new int;

   type pri_t is new short;

   type id_t is new long;

   P_MYID : constant := -1;
   --  The specified LWP or process is the current one

   type struct_pcinfo is record
      pc_cid    : id_t;
      pc_clname : String (1 .. PC_CLNMSZ);
      rt_maxpri : short;
   end record;
   pragma Convention (C, struct_pcinfo);

   type struct_pcparms is record
      pc_cid     : id_t;
      rt_pri     : pri_t;
      rt_tqsecs  : long;
      rt_tqnsecs : long;
   end record;
   pragma Convention (C, struct_pcparms);

   function priocntl
     (ver     : int;
      id_type : int;
      id      : lwpid_t;
      cmd     : int;
      arg     : System.Address) return Interfaces.C.long;
   pragma Import (C, priocntl, "__priocntl");

   function lwp_self return lwpid_t;
   pragma Import (C, lwp_self, "_lwp_self");

   type processorid_t is new int;
   type processorid_t_ptr is access all processorid_t;

   --  Constants for function processor_bind

   PBIND_QUERY : constant processorid_t := -2;
   --  The processor bindings are not changed

   PBIND_NONE  : constant processorid_t := -1;
   --  The processor bindings of the specified LWPs are cleared

   --  Flags for function p_online

   PR_OFFLINE : constant int := 1;
   --  Processor is offline, as quiet as possible

   PR_ONLINE  : constant int := 2;
   --  Processor online

   PR_STATUS  : constant int := 3;
   --  Value passed to p_online to request status

   function p_online (processorid : processorid_t; flag : int) return int;
   pragma Import (C, p_online, "p_online");

   function processor_bind
     (id_type : int;
      id      : id_t;
      proc_id : processorid_t;
      obind   : processorid_t_ptr) return int;
   pragma Import (C, processor_bind, "processor_bind");

   type psetid_t is new int;

   function pset_create (pset : access psetid_t) return int;
   pragma Import (C, pset_create, "pset_create");

   function pset_assign
     (pset    : psetid_t;
      proc_id : processorid_t;
      opset   : access psetid_t) return int;
   pragma Import (C, pset_assign, "pset_assign");

   function pset_bind
     (pset    : psetid_t;
      id_type : int;
      id      : id_t;
      opset   : access psetid_t) return int;
   pragma Import (C, pset_bind, "pset_bind");

   procedure pthread_init;
   --  Dummy procedure to share s-intman.adb with other Solaris targets

private

   type array_type_1 is array (0 .. 3) of unsigned_long;
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

   type array_type_9 is array (0 .. 3) of unsigned_char;
   type record_type_3 is record
      flag  : array_type_9;
      Xtype : unsigned_long;
   end record;
   pragma Convention (C, record_type_3);

   type upad64_t is new Interfaces.Unsigned_64;

   type mutex_t is record
      flags : record_type_3;
      lock  : upad64_t;
      data  : upad64_t;
   end record;
   pragma Convention (C, mutex_t);

   type cond_t is record
      flags : record_type_3;
      data  : upad64_t;
   end record;
   pragma Convention (C, cond_t);

   type thread_key_t is new unsigned;

end System.OS_Interface;
