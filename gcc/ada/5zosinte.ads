------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                    S Y S T E M . O S _ I N T E R F A C E                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--           Copyright (C) 1997-2002 Free Software Foundation, Inc.         --
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

--  This is the VxWorks version of this package.
--
--  VxWorks does not directly support the needed POSIX routines, but it
--  does have other routines that make it possible to code equivalent
--  POSIX compliant routines.  The approach taken is to provide an
--  FSU threads compliant interface.

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.C;
with System.VxWorks;

package System.OS_Interface is
   pragma Preelaborate;

   subtype int         is Interfaces.C.int;
   subtype short       is Short_Integer;
   type long           is new Long_Integer;
   type unsigned_long  is mod 2 ** long'Size;
   type size_t         is mod 2 ** Standard'Address_Size;

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
   -- Signals and Interrupts --
   ----------------------------

   NSIG : constant := 32;
   --  Number of signals on the target OS
   type Signal is new int range 0 .. Interfaces.C."-" (NSIG, 1);

   Max_HW_Interrupt : constant := System.VxWorks.Num_HW_Interrupts - 1;
   type HW_Interrupt is new int range 0 .. Max_HW_Interrupt;

   Max_Interrupt : constant := Max_HW_Interrupt;

   SIGILL  : constant :=  4; --  illegal instruction (not reset)
   SIGABRT : constant :=  6; --  used by abort, replace SIGIOT in the future
   SIGFPE  : constant :=  8; --  floating point exception
   SIGBUS  : constant := 10; --  bus error
   SIGSEGV : constant := 11; --  segmentation violation

   -----------------------------------
   -- Signal processing definitions --
   -----------------------------------

   --  The how in sigprocmask().
   SIG_BLOCK   : constant := 1;
   SIG_UNBLOCK : constant := 2;
   SIG_SETMASK : constant := 3;

   --  The sa_flags in struct sigaction.
   SA_SIGINFO   : constant := 16#0002#;
   SA_ONSTACK   : constant := 16#0004#;

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

   function c_signal (sig : Signal; handler : isr_address) return isr_address;
   pragma Import (C, c_signal, "signal");

   function sigwait (set : access sigset_t; sig : access Signal) return int;
   pragma Inline (sigwait);

   type sigset_t_ptr is access all sigset_t;

   function pthread_sigmask
     (how  : int;
      set  : sigset_t_ptr;
      oset : sigset_t_ptr) return int;
   pragma Import (C, pthread_sigmask, "sigprocmask");

   type t_id is new long;
   subtype Thread_Id is t_id;

   function kill (pid : t_id; sig : Signal) return int;
   pragma Import (C, kill, "kill");

   --  VxWorks doesn't have getpid; taskIdSelf is the equivalent
   --  routine.
   function getpid return t_id;
   pragma Import (C, getpid, "taskIdSelf");

   ----------
   -- Time --
   ----------

   type time_t is new unsigned_long;

   type timespec is record
      ts_sec  : time_t;
      ts_nsec : long;
   end record;
   pragma Convention (C, timespec);

   type clockid_t is private;

   CLOCK_REALTIME : constant clockid_t;   --  System wide realtime clock

   function To_Duration (TS : timespec) return Duration;
   pragma Inline (To_Duration);

   function To_Timespec (D : Duration) return timespec;
   pragma Inline (To_Timespec);

   function To_Clock_Ticks (D : Duration) return int;
   --  Convert a duration value (in seconds) into clock ticks.

   function clock_gettime
     (clock_id : clockid_t; tp : access timespec) return int;
   pragma Import (C, clock_gettime, "clock_gettime");

   type ULONG is new unsigned_long;

   procedure tickSet (ticks : ULONG);
   pragma Import (C, tickSet, "tickSet");

   function tickGet return ULONG;
   pragma Import (C, tickGet, "tickGet");

   -----------------------------------------------------
   --  Convenience routine to convert between VxWorks --
   --  priority and Ada priority.                     --
   -----------------------------------------------------

   function To_VxWorks_Priority (Priority : in int) return int;
   pragma Inline (To_VxWorks_Priority);

   --------------------------
   -- VxWorks specific API --
   --------------------------

   subtype STATUS is int;
   --  Equivalent of the C type STATUS

   OK    : constant STATUS := 0;
   ERROR : constant STATUS := Interfaces.C.int (-1);

   function taskIdVerify (tid : t_id)  return STATUS;
   pragma Import (C, taskIdVerify, "taskIdVerify");

   function taskIdSelf return t_id;
   pragma Import (C, taskIdSelf, "taskIdSelf");

   function taskSuspend (tid : t_id) return int;
   pragma Import (C, taskSuspend, "taskSuspend");

   function taskResume (tid : t_id) return int;
   pragma Import (C, taskResume, "taskResume");

   function taskIsSuspended (tid : t_id) return int;
   pragma Import (C, taskIsSuspended, "taskIsSuspended");

   function taskVarAdd
     (tid : t_id; pVar : access System.Address) return int;
   pragma Import (C, taskVarAdd, "taskVarAdd");

   function taskVarDelete
     (tid : t_id; pVar : access System.Address) return int;
   pragma Import (C, taskVarDelete, "taskVarDelete");

   function taskVarSet
     (tid   : t_id;
      pVar  : access System.Address;
      value : System.Address) return int;
   pragma Import (C, taskVarSet, "taskVarSet");

   function taskVarGet
     (tid  : t_id;
      pVar : access System.Address) return int;
   pragma Import (C, taskVarGet, "taskVarGet");

   function taskDelay (ticks : int) return int;
   procedure taskDelay (ticks : int);
   pragma Import (C, taskDelay, "taskDelay");

   function sysClkRateGet return int;
   pragma Import (C, sysClkRateGet, "sysClkRateGet");

   --  Option flags for taskSpawn

   VX_UNBREAKABLE    : constant := 16#0002#;
   VX_FP_TASK        : constant := 16#0008#;
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

   function kernelTimeSlice (ticks : int) return int;
   pragma Import (C, kernelTimeSlice, "kernelTimeSlice");

   function taskPrioritySet
     (tid : t_id; newPriority : int) return int;
   pragma Import (C, taskPrioritySet, "taskPrioritySet");

   --  Semaphore creation flags.

   SEM_Q_FIFO         : constant := 0;
   SEM_Q_PRIORITY     : constant := 1;
   SEM_DELETE_SAFE    : constant := 4;  -- only valid for binary semaphore
   SEM_INVERSION_SAFE : constant := 8;  -- only valid for binary semaphore

   --  Semaphore initial state flags

   SEM_EMPTY : constant := 0;
   SEM_FULL  : constant := 1;

   --  Semaphore take (semTake) time constants.

   WAIT_FOREVER : constant := -1;
   NO_WAIT      : constant := 0;

   --  Error codes (errno).  The lower level 16 bits are the
   --  error code, with the upper 16 bits representing the
   --  module number in which the error occurred.  By convention,
   --  the module number is 0 for UNIX errors.  VxWorks reserves
   --  module numbers 1-500, with the remaining module numbers
   --  being available for user applications.

   M_objLib                 : constant := 61 * 2**16;
   --  semTake() failure with ticks = NO_WAIT
   S_objLib_OBJ_UNAVAILABLE : constant := M_objLib + 2;
   --  semTake() timeout with ticks > NO_WAIT
   S_objLib_OBJ_TIMEOUT     : constant := M_objLib + 4;

   type SEM_ID is new System.Address;
   --  typedef struct semaphore *SEM_ID;

   --  We use two different kinds of VxWorks semaphores: mutex
   --  and binary semaphores.  A null ID is returned when
   --  a semaphore cannot be created.

   function semBCreate (options : int; initial_state : int) return SEM_ID;
   --  Create a binary semaphore. Return ID, or 0 if memory could not
   --  be allocated.
   pragma Import (C, semBCreate, "semBCreate");

   function semMCreate (options : int) return SEM_ID;
   pragma Import (C, semMCreate, "semMCreate");

   function semDelete (Sem : SEM_ID) return int;
   --  Delete a semaphore
   pragma Import (C, semDelete, "semDelete");

   function semGive (Sem : SEM_ID) return int;
   pragma Import (C, semGive, "semGive");

   function semTake (Sem : SEM_ID; timeout : int) return int;
   --  Attempt to take binary semaphore.  Error is returned if operation
   --  times out
   pragma Import (C, semTake, "semTake");

   function semFlush (SemID : SEM_ID) return STATUS;
   --  Release all threads blocked on the semaphore
   pragma Import (C, semFlush, "semFlush");

   function taskLock return int;
   pragma Import (C, taskLock, "taskLock");

   function taskUnlock return int;
   pragma Import (C, taskUnlock, "taskUnlock");

private
   type sigset_t is new long;

   type pid_t is new int;

   ERROR_PID : constant pid_t := -1;

   type clockid_t is new int;
   CLOCK_REALTIME : constant clockid_t := 0;

end System.OS_Interface;
