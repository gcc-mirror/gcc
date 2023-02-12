------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . O S _ I N T E R F A C E                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--            Copyright (C) 1991-2017, Florida State University             --
--          Copyright (C) 1995-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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

--  This is the VxWorks version of this package

--  This package encapsulates all direct interfaces to OS services that are
--  needed by the tasking run-time (libgnarl).

--  PLEASE DO NOT add any with-clauses to this package or remove the pragma
--  Preelaborate. This package is designed to be a bottom-level (leaf) package.

with Interfaces.C;
with System.VxWorks;
with System.VxWorks.Ext;
with System.Multiprocessors;
with System.Parameters;

package System.OS_Interface is
   pragma Preelaborate;

   package SVE renames System.VxWorks.Ext;

   subtype int             is Interfaces.C.int;
   subtype unsigned        is Interfaces.C.unsigned;
   subtype short           is Short_Integer;
   type unsigned_int       is mod 2 ** int'Size;
   type long               is new Long_Integer;
   type unsigned_long      is mod 2 ** long'Size;
   type long_long          is new Long_Long_Integer;
   type unsigned_long_long is mod 2 ** long_long'Size;
   type size_t             is mod 2 ** Standard'Address_Size;

   subtype STATUS    is SVE.STATUS;
   subtype BOOL      is SVE.BOOL;
   subtype vx_freq_t is SVE.vx_freq_t;

   -----------
   -- Errno --
   -----------

   function errno return int;
   pragma Import (C, errno, "errnoGet");

   EINTR     : constant := 4;
   EAGAIN    : constant := 35;
   ENOMEM    : constant := 12;
   EINVAL    : constant := 22;
   ETIMEDOUT : constant := 60;

   FUNC_ERR  : constant := -1;

   ----------------------------
   -- Signals and interrupts --
   ----------------------------

   NSIG : constant := 64;
   --  Number of signals on the target OS
   type Signal is new int range 0 .. Interfaces.C."-" (NSIG, 1);

   Max_HW_Interrupt : constant := System.VxWorks.Num_HW_Interrupts - 1;
   type HW_Interrupt is new int range 0 .. Max_HW_Interrupt;

   Max_Interrupt : constant := Max_HW_Interrupt;
   subtype Interrupt_Range is Natural range 0 .. Max_HW_Interrupt;
   --  For s-interr

   --  Signals common to Vxworks 5.x and 6.x

   SIGILL    : constant :=  4; --  illegal instruction (not reset when caught)
   SIGABRT   : constant :=  6; --  used by abort, replace SIGIOT in the future
   SIGFPE    : constant :=  8; --  floating point exception
   SIGBUS    : constant := 10; --  bus error
   SIGSEGV   : constant := 11; --  segmentation violation

   --  Signals specific to VxWorks 6.x

   SIGHUP    : constant :=  1; --  hangup
   SIGINT    : constant :=  2; --  interrupt
   SIGQUIT   : constant :=  3; --  quit
   SIGTRAP   : constant :=  5; --  trace trap (not reset when caught)
   SIGEMT    : constant :=  7; --  EMT instruction
   SIGKILL   : constant :=  9; --  kill
   SIGFMT    : constant := 12; --  STACK FORMAT ERROR (not posix)
   SIGPIPE   : constant := 13; --  write on a pipe with no one to read it
   SIGALRM   : constant := 14; --  alarm clock
   SIGTERM   : constant := 15; --  software termination signal from kill
   SIGCNCL   : constant := 16; --  pthreads cancellation signal
   SIGSTOP   : constant := 17; --  sendable stop signal not from tty
   SIGTSTP   : constant := 18; --  stop signal from tty
   SIGCONT   : constant := 19; --  continue a stopped process
   SIGCHLD   : constant := 20; --  to parent on child stop or exit
   SIGTTIN   : constant := 21; --  to readers pgrp upon background tty read
   SIGTTOU   : constant := 22; --  like TTIN for output

   SIGRES1   : constant := 23; --  reserved signal number (Not POSIX)
   SIGRES2   : constant := 24; --  reserved signal number (Not POSIX)
   SIGRES3   : constant := 25; --  reserved signal number (Not POSIX)
   SIGRES4   : constant := 26; --  reserved signal number (Not POSIX)
   SIGRES5   : constant := 27; --  reserved signal number (Not POSIX)
   SIGRES6   : constant := 28; --  reserved signal number (Not POSIX)
   SIGRES7   : constant := 29; --  reserved signal number (Not POSIX)

   SIGUSR1   : constant := 30; --  user defined signal 1
   SIGUSR2   : constant := 31; --  user defined signal 2

   SIGPOLL   : constant := 32; --  pollable event
   SIGPROF   : constant := 33; --  profiling timer expired
   SIGSYS    : constant := 34; --  bad system call
   SIGURG    : constant := 35; --  high bandwidth data is available at socket
   SIGVTALRM : constant := 36; --  virtual timer expired
   SIGXCPU   : constant := 37; --  CPU time limit exceeded
   SIGXFSZ   : constant := 38; --  file size time limit exceeded

   SIGEVTS   : constant := 39; --  signal event thread send
   SIGEVTD   : constant := 40; --  signal event thread delete

   SIGRTMIN  : constant := 48; --  Realtime signal min
   SIGRTMAX  : constant := 63; --  Realtime signal max

   -----------------------------------
   -- Signal processing definitions --
   -----------------------------------

   --  The how in sigprocmask()

   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   --  The sa_flags in struct sigaction

   SA_SIGINFO : constant := 16#0002#;
   SA_ONSTACK : constant := 16#0004#;

   SIG_DFL : constant := 0;
   SIG_IGN : constant := 1;

   type sigset_t is private;

   type struct_sigaction is record
      sa_handler : System.Address;
      sa_mask    : sigset_t;
      sa_flags   : int;
   end record;
   pragma Convention (C, struct_sigaction);
   type struct_sigaction_ptr is access all struct_sigaction;

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

   function sigaction
     (sig  : Signal;
      act  : struct_sigaction_ptr;
      oact : struct_sigaction_ptr) return int;
   pragma Import (C, sigaction, "sigaction");

   type isr_address is access procedure (sig : int);
   pragma Convention (C, isr_address);

   function c_signal (sig : Signal; handler : isr_address) return isr_address;
   pragma Import (C, c_signal, "signal");

   function pthread_sigmask
     (how  : int;
      set  : access sigset_t;
      oset : access sigset_t) return int;
   pragma Import (C, pthread_sigmask, "sigprocmask");

   subtype t_id is SVE.t_id;
   subtype Thread_Id is t_id;
   --  Thread_Id and t_id are VxWorks identifiers for tasks. This value,
   --  although represented as a Long_Integer, is in fact an address. With
   --  some BSPs, this address can have a value sufficiently high that the
   --  Thread_Id becomes negative: this should not be considered as an error.

   function kill (pid : t_id; sig : Signal) return int;
   pragma Inline (kill);

   function getpid return t_id renames SVE.getpid;

   function Task_Stop (tid : t_id) return STATUS renames SVE.Task_Stop;
   --  If we are in the kernel space, stop the task whose t_id is given in
   --  parameter in such a way that it can be examined by the debugger. This
   --  typically maps to taskSuspend on VxWorks 5 and to taskStop on VxWorks 6.

   function Task_Cont (tid : t_id) return STATUS renames SVE.Task_Cont;
   --  If we are in the kernel space, continue the task whose t_id is given
   --  in parameter if it has been stopped previously to be examined by the
   --  debugger (e.g. by taskStop). It typically maps to taskResume on VxWorks
   --  5 and to taskCont on VxWorks 6.

   function Int_Lock return int renames SVE.Int_Lock;
   --  If we are in the kernel space, lock interrupts. It typically maps to
   --  intLock.

   procedure Int_Unlock (Old : int) renames SVE.Int_Unlock;
   --  If we are in the kernel space, unlock interrupts. It typically maps to
   --  intUnlock. The parameter Old is only used on PowerPC where it contains
   --  the returned value from Int_Lock (the old MPSR).

   ----------
   -- Time --
   ----------

   type time_t is range -2 ** (System.Parameters.time_t_bits - 1)
     .. 2 ** (System.Parameters.time_t_bits - 1) - 1;
   --  Time_t here used to be unsigned to match the VxWorks header declaration.
   --  The header declaration has changed in newer releases and is now signed
   --  for applications.

   type timespec is record
      ts_sec  : time_t;
      ts_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is new int;

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);
   --  Convert a Duration value to a timespec value. Note that in VxWorks,
   --  timespec is always non-negative (since time_t is defined above as
   --  unsigned long). This means that there is a potential problem if a
   --  negative argument is passed for D. However, in actual usage, the
   --  value of the input argument D is always non-negative, so no problem
   --  arises in practice.

   function To_Clock_Ticks (D : Duration) return int;
   --  Convert a duration value (in seconds) into clock ticks

   function clock_gettime
     (clock_id : clockid_t; tp : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   ----------------------
   -- Utility Routines --
   ----------------------

   function To_VxWorks_Priority (Priority : int) return int;
   pragma Inline (To_VxWorks_Priority);
   --  Convenience routine to convert between VxWorks priority and Ada priority

   --------------------------
   -- VxWorks specific API --
   --------------------------

   function taskIdVerify (tid : t_id) return STATUS;
   pragma Import (C, taskIdVerify, "taskIdVerify");

   function taskIdSelf return t_id;
   pragma Import (C, taskIdSelf, "taskIdSelf");

   function taskOptionsGet (tid : t_id; pOptions : access int) return STATUS;
   pragma Import (C, taskOptionsGet, "taskOptionsGet");

   function taskSuspend (tid : t_id) return STATUS;
   pragma Import (C, taskSuspend, "taskSuspend");

   function taskResume (tid : t_id) return STATUS;
   pragma Import (C, taskResume, "taskResume");

   function taskIsSuspended (tid : t_id) return BOOL;
   pragma Import (C, taskIsSuspended, "taskIsSuspended");

   function taskDelay (ticks : int) return STATUS;
   pragma Import (C, taskDelay, "taskDelay");

   function sysClkRateGet return vx_freq_t;
   pragma Import (C, sysClkRateGet, "sysClkRateGet");

   --  VxWorks 5.x specific functions
   --  Must not be called from run-time for versions that do not support
   --  taskVarLib: eg VxWorks 6 RTPs

   function taskVarAdd
     (tid : t_id; pVar : access System.Address) return STATUS;
   pragma Import (C, taskVarAdd, "taskVarAdd");

   function taskVarDelete
     (tid : t_id; pVar : access System.Address) return STATUS;
   pragma Import (C, taskVarDelete, "taskVarDelete");

   function taskVarSet
     (tid   : t_id;
      pVar  : access System.Address;
      value : System.Address) return STATUS;
   pragma Import (C, taskVarSet, "taskVarSet");

   function taskVarGet
     (tid  : t_id;
      pVar : access System.Address) return int;
   pragma Import (C, taskVarGet, "taskVarGet");

   --  VxWorks 6.x specific functions

   --  Can only be called from the VxWorks 6 run-time libary that supports
   --  tlsLib, and not by the VxWorks 6.6 SMP library

   function tlsKeyCreate return int;
   pragma Import (C, tlsKeyCreate, "tlsKeyCreate");

   function tlsValueGet (key : int) return System.Address;
   pragma Import (C, tlsValueGet, "tlsValueGet");

   function tlsValueSet (key : int; value : System.Address) return STATUS;
   pragma Import (C, tlsValueSet, "tlsValueSet");

   --  Option flags for taskSpawn

   VX_UNBREAKABLE    : constant := 16#0002#;
   VX_FP_PRIVATE_ENV : constant := 16#0080#;
   VX_NO_STACK_FILL  : constant := 16#0100#;

   function taskSpawn
     (name          : System.Address;  --  Pointer to task name
      priority      : int;
      options       : int;
      stacksize     : size_t;
      start_routine : System.Address;
      arg1          : System.Address;
      arg2          : int := 0;
      arg3          : int := 0;
      arg4          : int := 0;
      arg5          : int := 0;
      arg6          : int := 0;
      arg7          : int := 0;
      arg8          : int := 0;
      arg9          : int := 0;
      arg10         : int := 0) return t_id;
   pragma Import (C, taskSpawn, "taskSpawn");

   procedure taskDelete (tid : t_id);
   pragma Import (C, taskDelete, "taskDelete");

   function Set_Time_Slice (ticks : int) return STATUS renames
     SVE.Set_Time_Slice;
   --  Calls kernelTimeSlice under VxWorks 5.x, VxWorks 653, or in VxWorks 6
   --  kernel apps. Returns ERROR for RTPs, VxWorks 5 /CERT

   function taskPriorityGet (tid : t_id; pPriority : access int) return STATUS;
   pragma Import (C, taskPriorityGet, "taskPriorityGet");

   function taskPrioritySet (tid : t_id; newPriority : int) return STATUS;
   pragma Import (C, taskPrioritySet, "taskPrioritySet");

   --  Semaphore creation flags

   SEM_Q_FIFO         : constant := 0;
   SEM_Q_PRIORITY     : constant := 1;
   SEM_DELETE_SAFE    : constant := 4;  -- only valid for binary semaphore
   SEM_INVERSION_SAFE : constant := 8;  -- only valid for binary semaphore

   --  Semaphore initial state flags

   SEM_EMPTY : constant := 0;
   SEM_FULL  : constant := 1;

   --  Semaphore take (semTake) time constants

   WAIT_FOREVER : constant := -1;
   NO_WAIT      : constant := 0;

   --  Error codes (errno). The lower level 16 bits are the error code, with
   --  the upper 16 bits representing the module number in which the error
   --  occurred. By convention, the module number is 0 for UNIX errors. VxWorks
   --  reserves module numbers 1-500, with the remaining module numbers being
   --  available for user applications.

   M_objLib                 : constant := 61 * 2**16;
   --  semTake() failure with ticks = NO_WAIT
   S_objLib_OBJ_UNAVAILABLE : constant := M_objLib + 2;
   --  semTake() timeout with ticks > NO_WAIT
   S_objLib_OBJ_TIMEOUT     : constant := M_objLib + 4;

   subtype SEM_ID is SVE.SEM_ID;
   --  typedef struct semaphore *SEM_ID;

   --  We use two different kinds of VxWorks semaphores: mutex and binary
   --  semaphores. A null ID is returned when a semaphore cannot be created.

   function semBCreate (options : int; initial_state : int) return SEM_ID;
   pragma Import (C, semBCreate, "semBCreate");
   --  Create a binary semaphore. Return ID, or 0 if memory could not
   --  be allocated.

   function semMCreate (options : int) return SEM_ID;
   pragma Import (C, semMCreate, "semMCreate");

   function semDelete (Sem : SEM_ID) return STATUS renames SVE.semDelete;
   --  Delete a semaphore

   function semGive (Sem : SEM_ID) return STATUS;
   pragma Import (C, semGive, "semGive");

   function semTake (Sem : SEM_ID; timeout : int) return STATUS;
   pragma Import (C, semTake, "semTake");
   --  Attempt to take binary semaphore.  Error is returned if operation
   --  times out

   function semFlush (SemID : SEM_ID) return STATUS;
   pragma Import (C, semFlush, "semFlush");
   --  Release all threads blocked on the semaphore

   ------------------------------------------------------------
   --   Binary Semaphore Wrapper to Support interrupt Tasks  --
   ------------------------------------------------------------

   type Binary_Semaphore_Id is new Long_Integer;

   function Binary_Semaphore_Create return Binary_Semaphore_Id;
   pragma Inline (Binary_Semaphore_Create);

   function Binary_Semaphore_Delete (ID : Binary_Semaphore_Id) return STATUS;
   pragma Inline (Binary_Semaphore_Delete);

   function Binary_Semaphore_Obtain (ID : Binary_Semaphore_Id) return STATUS;
   pragma Inline (Binary_Semaphore_Obtain);

   function Binary_Semaphore_Release (ID : Binary_Semaphore_Id) return STATUS;
   pragma Inline (Binary_Semaphore_Release);

   function Binary_Semaphore_Flush (ID : Binary_Semaphore_Id) return STATUS;
   pragma Inline (Binary_Semaphore_Flush);

   ------------------------------------------------------------
   -- Hardware Interrupt Wrappers to Support Interrupt Tasks --
   ------------------------------------------------------------

   type Interrupt_Handler is access procedure (parameter : System.Address);
   pragma Convention (C, Interrupt_Handler);

   type Interrupt_Vector is new System.Address;

   function Interrupt_Connect
     (Vector    : Interrupt_Vector;
      Handler   : Interrupt_Handler;
      Parameter : System.Address := System.Null_Address) return STATUS;
   pragma Inline (Interrupt_Connect);
   --  Use this to set up an user handler. The routine installs a user handler
   --  which is invoked after the OS has saved enough context for a high-level
   --  language routine to be safely invoked.

   function Interrupt_Context return BOOL;
   pragma Inline (Interrupt_Context);
   --  Return 1 (TRUE) if executing in an interrupt context;
   --  return 0 (FALSE) if executing in a task context.

   function Interrupt_Number_To_Vector (intNum : int) return Interrupt_Vector;
   pragma Inline (Interrupt_Number_To_Vector);
   --  Convert a logical interrupt number to the hardware interrupt vector
   --  number used to connect the interrupt.

   --------------------------------
   -- Processor Affinity for SMP --
   --------------------------------

   function taskCpuAffinitySet (tid : t_id; CPU : int) return int
    renames SVE.taskCpuAffinitySet;
   --  For SMP run-times the affinity to CPU.
   --  For uniprocessor systems return ERROR status.

   function taskMaskAffinitySet (tid : t_id; CPU_Set : unsigned) return int
     renames SVE.taskMaskAffinitySet;
   --  For SMP run-times the affinity to CPU_Set.
   --  For uniprocessor systems return ERROR status.

   ---------------------
   -- Multiprocessors --
   ---------------------

   function Current_CPU return Multiprocessors.CPU;
   --  Return the id of the current CPU

private
   type pid_t is new int;

   ERROR_PID : constant pid_t := -1;

   type sigset_t is new SVE.sigset_t;
end System.OS_Interface;
