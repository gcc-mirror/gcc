------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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

--  This is a POSIX-like version of this package

--  This package contains all the GNULL primitives that interface directly with
--  the underlying OS.

--  Note: this file can only be used for POSIX compliant systems that implement
--  SCHED_FIFO and Ceiling Locking correctly.

--  For configurations where SCHED_FIFO and priority ceiling are not a
--  requirement, this file can also be used (e.g AiX threads)

with Ada.Unchecked_Conversion;

with Interfaces.C;

with System.Interrupt_Management;
with System.Multiprocessors;
with System.OS_Constants;
with System.OS_Primitives;
with System.Task_Info;
with System.Tasking.Debug;

with System.Soft_Links;
--  We use System.Soft_Links instead of System.Tasking.Initialization
--  because the later is a higher level package that we shouldn't depend on.
--  For example when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Stages.

package body System.Task_Primitives.Operations is

   package OSC renames System.OS_Constants;
   package SSL renames System.Soft_Links;

   use Interfaces.C;

   use System.OS_Interface;
   use System.OS_Locks;
   use System.OS_Primitives;
   use System.Parameters;
   use System.Tasking;
   use System.Tasking.Debug;

   ----------------
   -- Local Data --
   ----------------

   --  The followings are logically constants, but need to be initialized
   --  at run time.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used to protect All_Tasks_List

   Environment_Task_Id : Task_Id;
   --  A variable to hold Task_Id for the environment task

   Locking_Policy : constant Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");
   --  Value of the pragma Locking_Policy:
   --    'C' for Ceiling_Locking
   --    'I' for Inherit_Locking
   --    ' ' for none.

   Unblocked_Signal_Mask : aliased sigset_t;
   --  The set of signals that should unblocked in all tasks

   --  The followings are internal configuration constants needed

   Next_Serial_Number : Task_Serial_Number := 100;
   --  We start at 100, to reserve some special values for
   --  using in error checking.

   Time_Slice_Val : constant Integer;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Dispatching_Policy : constant Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   Foreign_Task_Elaborated : aliased Boolean := True;
   --  Used to identified fake tasks (i.e., non-Ada Threads)

   Use_Alternate_Stack : constant Boolean := Alternate_Stack_Size /= 0;
   --  Whether to use an alternate signal stack for stack overflows

   Abort_Handler_Installed : Boolean := False;
   --  True if a handler for the abort signal is installed

   function Initialize_Lock
     (L    : not null access RTS_Lock;
      Prio : Any_Priority) return Interfaces.C.int;
   --  Initialize the lock L. If Ceiling_Support is True, then set the ceiling
   --  to Prio. Returns 0 for success, or ENOMEM for out-of-memory.

   function Get_Policy (Prio : System.Any_Priority) return Character;
   pragma Import (C, Get_Policy, "__gnat_get_specific_dispatching");
   --  Get priority specific dispatching policy

   --------------------
   -- Local Packages --
   --------------------

   package Specific is

      procedure Initialize (Environment_Task : Task_Id);
      pragma Inline (Initialize);
      --  Initialize various data needed by this package

      function Is_Valid_Task return Boolean;
      pragma Inline (Is_Valid_Task);
      --  Does executing thread have a TCB?

      procedure Set (Self_Id : Task_Id);
      pragma Inline (Set);
      --  Set the self id for the current task

      function Self return Task_Id;
      pragma Inline (Self);
      --  Return a pointer to the Ada Task Control Block of the calling task

   end Specific;

   package body Specific is separate;
   --  The body of this package is target specific

   package Monotonic is

      function Monotonic_Clock return Duration;
      pragma Inline (Monotonic_Clock);
      --  Returns an absolute time, represented as an offset relative to some
      --  unspecified starting point, typically system boot time.  This clock
      --  is not affected by discontinuous jumps in the system time.

      function RT_Resolution return Duration;
      pragma Inline (RT_Resolution);
      --  Returns resolution of the underlying clock used to implement RT_Clock

      procedure Timed_Sleep
        (Self_ID  : ST.Task_Id;
         Time     : Duration;
         Mode     : ST.Delay_Modes;
         Reason   : System.Tasking.Task_States;
         Timedout : out Boolean;
         Yielded  : out Boolean);
      --  Combination of Sleep (above) and Timed_Delay

      procedure Timed_Delay
        (Self_ID : ST.Task_Id;
         Time    : Duration;
         Mode    : ST.Delay_Modes);
      --  Implement the semantics of the delay statement.
      --  The caller should be abort-deferred and should not hold any locks.

   end Monotonic;

   package body Monotonic is separate;

   ----------------------------------
   -- ATCB allocation/deallocation --
   ----------------------------------

   package body ATCB_Allocation is separate;
   --  The body of this package is shared across several targets

   ---------------------------------
   -- Support for foreign threads --
   ---------------------------------

   function Register_Foreign_Thread
     (Thread         : Thread_Id;
      Sec_Stack_Size : Size_Type := Unspecified_Size) return Task_Id;
   --  Allocate and initialize a new ATCB for the current Thread. The size of
   --  the secondary stack can be optionally specified.

   function Register_Foreign_Thread
     (Thread         : Thread_Id;
      Sec_Stack_Size : Size_Type := Unspecified_Size)
     return Task_Id is separate;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Abort_Handler (Sig : Signal);
   --  Signal handler used to implement asynchronous abort.
   --  See also comment before body, below.

   function To_Address is
     new Ada.Unchecked_Conversion (Task_Id, System.Address);

   function GNAT_pthread_condattr_setup
     (attr : access pthread_condattr_t) return Interfaces.C.int;
   pragma Import (C,
     GNAT_pthread_condattr_setup, "__gnat_pthread_condattr_setup");

   -------------------
   -- Abort_Handler --
   -------------------

   --  Target-dependent binding of inter-thread Abort signal to the raising of
   --  the Abort_Signal exception.

   --  The technical issues and alternatives here are essentially the
   --  same as for raising exceptions in response to other signals
   --  (e.g. Storage_Error). See code and comments in the package body
   --  System.Interrupt_Management.

   --  Some implementations may not allow an exception to be propagated out of
   --  a handler, and others might leave the signal or interrupt that invoked
   --  this handler masked after the exceptional return to the application
   --  code.

   --  GNAT exceptions are originally implemented using setjmp()/longjmp(). On
   --  most UNIX systems, this will allow transfer out of a signal handler,
   --  which is usually the only mechanism available for implementing
   --  asynchronous handlers of this kind. However, some systems do not
   --  restore the signal mask on longjmp(), leaving the abort signal masked.

   procedure Abort_Handler (Sig : Signal) is
      pragma Unreferenced (Sig);

      T       : constant Task_Id := Self;
      Old_Set : aliased sigset_t;

      Result : Interfaces.C.int;
      pragma Warnings (Off, Result);

   begin
      --  It's not safe to raise an exception when using GCC ZCX mechanism.
      --  Note that we still need to install a signal handler, since in some
      --  cases (e.g. shutdown of the Server_Task in System.Interrupts) we
      --  need to send the Abort signal to a task.

      if ZCX_By_Default then
         return;
      end if;

      if T.Deferral_Level = 0
        and then T.Pending_ATC_Level < T.ATC_Nesting_Level and then
        not T.Aborting
      then
         T.Aborting := True;

         --  Make sure signals used for RTS internal purpose are unmasked

         Result := pthread_sigmask (SIG_UNBLOCK,
           Unblocked_Signal_Mask'Access, Old_Set'Access);
         pragma Assert (Result = 0);

         raise Standard'Abort_Signal;
      end if;
   end Abort_Handler;

   -----------------
   -- Stack_Guard --
   -----------------

   procedure Stack_Guard (T : ST.Task_Id; On : Boolean) is
      Stack_Base : constant Address := Get_Stack_Base (T.Common.LL.Thread);
      Page_Size  : Address;
      Res        : Interfaces.C.int;

   begin
      if Stack_Base_Available then

         --  Compute the guard page address

         Page_Size := Address (Get_Page_Size);
         Res :=
           mprotect
             (Stack_Base - (Stack_Base mod Page_Size) + Page_Size,
              Interfaces.C.size_t (Page_Size),
              prot => (if On then PROT_ON else PROT_OFF));
         pragma Assert (Res = 0);
      end if;
   end Stack_Guard;

   --------------------
   -- Get_Thread_Id  --
   --------------------

   function Get_Thread_Id (T : ST.Task_Id) return OSI.Thread_Id is
   begin
      return T.Common.LL.Thread;
   end Get_Thread_Id;

   ----------
   -- Self --
   ----------

   function Self return Task_Id renames Specific.Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   --  Note: mutexes and cond_variables needed per-task basis are initialized
   --  in Initialize_TCB and the Storage_Error is handled. Other mutexes (such
   --  as RTS_Lock, Memory_Lock...) used in RTS is initialized before any
   --  status change of RTS. Therefore raising Storage_Error in the following
   --  routines should be able to be handled safely.

   function Initialize_Lock
     (L    : not null access RTS_Lock;
      Prio : Any_Priority) return Interfaces.C.int
   is
      Attributes : aliased pthread_mutexattr_t;
      Result     : Interfaces.C.int;
      Result_2   : aliased Interfaces.C.int;

   begin
      Result := pthread_mutexattr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         return Result;
      end if;

      if Locking_Policy = 'C' then
         Result := pthread_mutexattr_setprotocol
           (Attributes'Access, PTHREAD_PRIO_PROTECT);
         pragma Assert (Result = 0);

         Result := pthread_mutexattr_getprotocol
           (Attributes'Access, Result_2'Access);
         if Result_2 /= PTHREAD_PRIO_PROTECT then
            raise Program_Error with "setprotocol failed";
         end if;

         Result := pthread_mutexattr_setprioceiling
            (Attributes'Access, To_Target_Priority (Prio));
         pragma Assert (Result = 0);

      elsif Locking_Policy = 'I' then
         Result := pthread_mutexattr_setprotocol
           (Attributes'Access, PTHREAD_PRIO_INHERIT);
         pragma Assert (Result = 0);
      end if;

      Result := pthread_mutex_init (L, Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      Result_2 := pthread_mutexattr_destroy (Attributes'Access);
      pragma Assert (Result_2 = 0);

      return Result;
   end Initialize_Lock;

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : not null access Lock)
   is
   begin
      if Initialize_Lock (L.WO'Access, Prio) = ENOMEM then
         raise Storage_Error with "Failed to allocate a lock";
      end if;
   end Initialize_Lock;

   procedure Initialize_Lock
     (L     : not null access RTS_Lock;
      Level : Lock_Level)
   is
      pragma Unreferenced (Level);

   begin
      if Initialize_Lock (L, Any_Priority'Last) = ENOMEM then
         raise Storage_Error with "Failed to allocate a lock";
      end if;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : not null access Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_destroy (L.WO'Access);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   procedure Finalize_Lock (L : not null access RTS_Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_destroy (L);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock
     (L : not null access Lock; Ceiling_Violation : out Boolean)
   is
      Self    : constant pthread_t := pthread_self;
      Result  : Interfaces.C.int;
      Policy  : aliased Interfaces.C.int;
      Ceiling : aliased Interfaces.C.int;
      Sched   : aliased struct_sched_param;

   begin
      Result := pthread_mutex_lock (L.WO'Access);

      --  The cause of EINVAL is a priority ceiling violation

      Ceiling_Violation := Result = EINVAL;
      pragma Assert (Result = 0 or else Ceiling_Violation);

      --  Workaround bug in QNX on ceiling locks: tasks with priority higher
      --  than the ceiling priority don't receive EINVAL upon trying to lock.
      if Result = 0 and then Locking_Policy = 'C' then
         Result := pthread_getschedparam (Self, Policy'Access, Sched'Access);
         pragma Assert (Result = 0);
         Result := pthread_mutex_getprioceiling (L.WO'Access, Ceiling'Access);
         pragma Assert (Result = 0);

         --  Ceiling < current priority means Ceiling violation
         --  (otherwise the current priority == ceiling)
         if Ceiling < Sched.sched_curpriority then
            Ceiling_Violation := True;
            Result := pthread_mutex_unlock (L.WO'Access);
            pragma Assert (Result = 0);
         end if;
      end if;
   end Write_Lock;

   procedure Write_Lock (L : not null access RTS_Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_lock (L);
      pragma Assert (Result = 0);
   end Write_Lock;

   procedure Write_Lock (T : Task_Id) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_lock (T.Common.LL.L'Access);
      pragma Assert (Result = 0);
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock
     (L : not null access Lock; Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : not null access Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_unlock (L.WO'Access);
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock (L : not null access RTS_Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_unlock (L);
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock (T : Task_Id) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_unlock (T.Common.LL.L'Access);
      pragma Assert (Result = 0);
   end Unlock;

   -----------------
   -- Set_Ceiling --
   -----------------

   procedure Set_Ceiling
     (L    : not null access Lock;
      Prio : System.Any_Priority)
   is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_setprioceiling
        (L.WO'Access, To_Target_Priority (Prio), null);
      pragma Assert (Result = 0);
   end Set_Ceiling;

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (Self_ID : Task_Id;
      Reason  : System.Tasking.Task_States)
   is
      pragma Unreferenced (Reason);

      Result : Interfaces.C.int;

   begin
      Result :=
        pthread_cond_wait
          (cond  => Self_ID.Common.LL.CV'Access,
           mutex => Self_ID.Common.LL.L'Access);

      --  EINTR is not considered a failure

      pragma Assert (Result = 0 or else Result = EINTR);
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.

   procedure Timed_Sleep
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean) renames Monotonic.Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so we assume the
   --  caller is abort-deferred but is holding no locks.

   procedure Timed_Delay
     (Self_ID : Task_Id;
      Time    : Duration;
      Mode    : ST.Delay_Modes) renames Monotonic.Timed_Delay;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration renames Monotonic.Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration renames Monotonic.RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Unreferenced (Reason);
      Result : Interfaces.C.int;
   begin
      Result := pthread_cond_signal (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);
   end Wakeup;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if Do_Yield then
         Result := sched_yield;
      end if;
   end Yield;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T                   : Task_Id;
      Prio                : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      pragma Unreferenced (Loss_Of_Inheritance);
      Result : Interfaces.C.int;
      Old    : constant System.Any_Priority := T.Common.Current_Priority;

   begin
      T.Common.Current_Priority := Prio;
      Result := pthread_setschedprio
        (T.Common.LL.Thread, To_Target_Priority (Prio));
      pragma Assert (Result = 0);

      if T.Common.LL.Thread = pthread_self
        and then Old > Prio
      then
         --  When lowering the priority via a pthread_setschedprio, QNX ensures
         --  that the running thread remains in the head of the FIFO for tne
         --  new priority. Annex D expects the thread to be requeued so let's
         --  yield to the other threads of the same priority.
         Result := sched_yield;
         pragma Assert (Result = 0);
      end if;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_Id) return System.Any_Priority is
   begin
      return T.Common.Current_Priority;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_Id) is
   begin
      Self_ID.Common.LL.LWP := lwp_self;

      Specific.Set (Self_ID);

      if Use_Alternate_Stack then
         declare
            Stack  : aliased stack_t;
            Result : Interfaces.C.int;
         begin
            Stack.ss_sp    := Self_ID.Common.Task_Alternate_Stack;
            Stack.ss_size  := Alternate_Stack_Size;
            Stack.ss_flags := 0;
            Result := sigaltstack (Stack'Access, null);
            pragma Assert (Result = 0);
         end;
      end if;
   end Enter_Task;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean renames Specific.Is_Valid_Task;

   -----------------------------
   -- Register_Foreign_Thread --
   -----------------------------

   function Register_Foreign_Thread return Task_Id is
   begin
      if Is_Valid_Task then
         return Self;
      else
         return Register_Foreign_Thread (pthread_self);
      end if;
   end Register_Foreign_Thread;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
      Result     : Interfaces.C.int;
      Cond_Attr  : aliased pthread_condattr_t;

   begin
      --  Give the task a unique serial number

      Self_ID.Serial_Number := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;
      pragma Assert (Next_Serial_Number /= 0);

      Result :=
        Initialize_Lock (Self_ID.Common.LL.L'Access, Any_Priority'Last);
      pragma Assert (Result = 0);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result := pthread_condattr_init (Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = 0 then
         Result := GNAT_pthread_condattr_setup (Cond_Attr'Access);
         pragma Assert (Result = 0);

         Result :=
           pthread_cond_init
             (Self_ID.Common.LL.CV'Access, Cond_Attr'Access);
         pragma Assert (Result = 0 or else Result = ENOMEM);
      end if;

      if Result = 0 then
         Succeeded := True;
      else
         Result := pthread_mutex_destroy (Self_ID.Common.LL.L'Access);
         pragma Assert (Result = 0);

         Succeeded := False;
      end if;

      Result := pthread_condattr_destroy (Cond_Attr'Access);
      pragma Assert (Result = 0);
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_Id;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      Attributes               : aliased pthread_attr_t;
      Adjusted_Stack_Size      : Interfaces.C.size_t;
      Page_Size                : constant Interfaces.C.size_t :=
                                   Interfaces.C.size_t (Get_Page_Size);
      Sched_Param              : aliased struct_sched_param;
      Result                   : Interfaces.C.int;

      Priority_Specific_Policy : constant Character := Get_Policy (Priority);
      --  Upper case first character of the policy name corresponding to the
      --  task as set by a Priority_Specific_Dispatching pragma.

      function Thread_Body_Access is new
        Ada.Unchecked_Conversion (System.Address, Thread_Body);

   begin
      Adjusted_Stack_Size :=
         Interfaces.C.size_t (Stack_Size + Alternate_Stack_Size);

      if Stack_Base_Available then

         --  If Stack Checking is supported then allocate 2 additional pages:

         --  In the worst case, stack is allocated at something like
         --  N * Get_Page_Size - epsilon, we need to add the size for 2 pages
         --  to be sure the effective stack size is greater than what
         --  has been asked.

         Adjusted_Stack_Size := Adjusted_Stack_Size + 2 * Page_Size;
      end if;

      --  Round stack size as this is required by some OSes (Darwin)

      Adjusted_Stack_Size := Adjusted_Stack_Size + Page_Size - 1;
      Adjusted_Stack_Size :=
        Adjusted_Stack_Size - Adjusted_Stack_Size mod Page_Size;

      Result := pthread_attr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result :=
        pthread_attr_setdetachstate
          (Attributes'Access, PTHREAD_CREATE_DETACHED);
      pragma Assert (Result = 0);

      Result :=
        pthread_attr_setstacksize
          (Attributes'Access, Adjusted_Stack_Size);
      pragma Assert (Result = 0);

      --  Set thread priority
      T.Common.Current_Priority := Priority;
      Sched_Param.sched_priority := To_Target_Priority (Priority);

      Result := pthread_attr_setinheritsched
        (Attributes'Access, PTHREAD_EXPLICIT_SCHED);
      pragma Assert (Result = 0);

      Result := pthread_attr_setschedparam
        (Attributes'Access, Sched_Param'Access);
      pragma Assert (Result = 0);

      if Time_Slice_Supported
        and then (Dispatching_Policy = 'R'
                  or else Priority_Specific_Policy = 'R'
                  or else Time_Slice_Val > 0)
      then
         Result := pthread_attr_setschedpolicy
           (Attributes'Access, SCHED_RR);

      elsif Dispatching_Policy = 'F'
        or else Priority_Specific_Policy = 'F'
        or else Time_Slice_Val = 0
      then
         Result := pthread_attr_setschedpolicy
           (Attributes'Access, SCHED_FIFO);

      else
         Result := pthread_attr_setschedpolicy
           (Attributes'Access, SCHED_OTHER);
      end if;

      pragma Assert (Result = 0);

      --  Since the initial signal mask of a thread is inherited from the
      --  creator, and the Environment task has all its signals masked, we
      --  do not need to manipulate caller's signal mask at this point.
      --  All tasks in RTS will have All_Tasks_Mask initially.

      --  The write to T.Common.LL.Thread is not racy with regard to the
      --  created thread because the created thread will not access it until
      --  we release the RTS lock (or the current task's lock when
      --  Restricted.Stages is used). One can verify that by inspecting the
      --  Task_Wrapper procedures.

      Result := pthread_create
        (T.Common.LL.Thread'Access,
         Attributes'Access,
         Thread_Body_Access (Wrapper),
         To_Address (T));
      pragma Assert (Result = 0 or else Result = EAGAIN);

      Succeeded := Result = 0;

      Result := pthread_attr_destroy (Attributes'Access);
      pragma Assert (Result = 0);
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_Id) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_destroy (T.Common.LL.L'Access);
      pragma Assert (Result = 0);

      Result := pthread_cond_destroy (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;

      ATCB_Allocation.Free_ATCB (T);
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      --  Mark this task as unknown, so that if Self is called, it won't
      --  return a dangling pointer.

      Specific.Set (null);
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
      Result : Interfaces.C.int;
   begin
      if Abort_Handler_Installed then
         Result :=
           pthread_kill
             (T.Common.LL.Thread,
              Signal (System.Interrupt_Management.Abort_Task_Interrupt));
         pragma Assert (Result = 0);
      end if;
   end Abort_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Suspension_Object) is
      Mutex_Attr : aliased pthread_mutexattr_t;
      Cond_Attr  : aliased pthread_condattr_t;
      Result     : Interfaces.C.int;

   begin
      --  Initialize internal state (always to False (RM D.10 (6)))

      S.State := False;
      S.Waiting := False;

      --  Initialize internal mutex

      Result := pthread_mutexattr_init (Mutex_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

      Result := pthread_mutex_init (S.L'Access, Mutex_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         Result := pthread_mutexattr_destroy (Mutex_Attr'Access);
         pragma Assert (Result = 0);

         raise Storage_Error;
      end if;

      Result := pthread_mutexattr_destroy (Mutex_Attr'Access);
      pragma Assert (Result = 0);

      --  Initialize internal condition variable

      Result := pthread_condattr_init (Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Result := pthread_mutex_destroy (S.L'Access);
         pragma Assert (Result = 0);

         --  Storage_Error is propagated as intended if the allocation of the
         --  underlying OS entities fails.

         raise Storage_Error;

      else
         Result := GNAT_pthread_condattr_setup (Cond_Attr'Access);
         pragma Assert (Result = 0);
      end if;

      Result := pthread_cond_init (S.CV'Access, Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Result := pthread_mutex_destroy (S.L'Access);
         pragma Assert (Result = 0);

         Result := pthread_condattr_destroy (Cond_Attr'Access);
         pragma Assert (Result = 0);

         --  Storage_Error is propagated as intended if the allocation of the
         --  underlying OS entities fails.

         raise Storage_Error;
      end if;

      Result := pthread_condattr_destroy (Cond_Attr'Access);
      pragma Assert (Result = 0);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      --  Destroy internal mutex

      Result := pthread_mutex_destroy (S.L'Access);
      pragma Assert (Result = 0);

      --  Destroy internal condition variable

      Result := pthread_cond_destroy (S.CV'Access);
      pragma Assert (Result = 0);
   end Finalize;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      --  We do not want to use lock on this read operation. State is marked
      --  as Atomic so that we ensure that the value retrieved is correct.

      return S.State;
   end Current_State;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      SSL.Abort_Defer.all;

      Result := pthread_mutex_lock (S.L'Access);
      pragma Assert (Result = 0);

      S.State := False;

      Result := pthread_mutex_unlock (S.L'Access);
      pragma Assert (Result = 0);

      SSL.Abort_Undefer.all;
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      SSL.Abort_Defer.all;

      Result := pthread_mutex_lock (S.L'Access);
      pragma Assert (Result = 0);

      --  If there is already a task waiting on this suspension object then
      --  we resume it, leaving the state of the suspension object to False,
      --  as it is specified in (RM D.10(9)). Otherwise, it just leaves
      --  the state to True.

      if S.Waiting then
         S.Waiting := False;
         S.State := False;

         Result := pthread_cond_signal (S.CV'Access);
         pragma Assert (Result = 0);

      else
         S.State := True;
      end if;

      Result := pthread_mutex_unlock (S.L'Access);
      pragma Assert (Result = 0);

      SSL.Abort_Undefer.all;
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      SSL.Abort_Defer.all;

      Result := pthread_mutex_lock (S.L'Access);
      pragma Assert (Result = 0);

      if S.Waiting then

         --  Program_Error must be raised upon calling Suspend_Until_True
         --  if another task is already waiting on that suspension object
         --  (RM D.10(10)).

         Result := pthread_mutex_unlock (S.L'Access);
         pragma Assert (Result = 0);

         SSL.Abort_Undefer.all;

         raise Program_Error;

      else
         --  Suspend the task if the state is False. Otherwise, the task
         --  continues its execution, and the state of the suspension object
         --  is set to False (ARM D.10 par. 9).

         if S.State then
            S.State := False;
         else
            S.Waiting := True;

            loop
               --  Loop in case pthread_cond_wait returns earlier than expected
               --  (e.g. in case of EINTR caused by a signal).

               Result := pthread_cond_wait (S.CV'Access, S.L'Access);
               pragma Assert (Result = 0 or else Result = EINTR);

               exit when not S.Waiting;
            end loop;
         end if;

         Result := pthread_mutex_unlock (S.L'Access);
         pragma Assert (Result = 0);

         SSL.Abort_Undefer.all;
      end if;
   end Suspend_Until_True;

   ----------------
   -- Check_Exit --
   ----------------

   --  Dummy version

   function Check_Exit (Self_ID : ST.Task_Id) return Boolean is
      pragma Unreferenced (Self_ID);
   begin
      return True;
   end Check_Exit;

   --------------------
   -- Check_No_Locks --
   --------------------

   function Check_No_Locks (Self_ID : ST.Task_Id) return Boolean is
      pragma Unreferenced (Self_ID);
   begin
      return True;
   end Check_No_Locks;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_Id is
   begin
      return Environment_Task_Id;
   end Environment_Task;

   --------------
   -- Lock_RTS --
   --------------

   procedure Lock_RTS is
   begin
      Write_Lock (Single_RTS_Lock'Access);
   end Lock_RTS;

   ----------------
   -- Unlock_RTS --
   ----------------

   procedure Unlock_RTS is
   begin
      Unlock (Single_RTS_Lock'Access);
   end Unlock_RTS;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
      pragma Unreferenced (T, Thread_Self);
   begin
      return False;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
      pragma Unreferenced (T, Thread_Self);
   begin
      return False;
   end Resume_Task;

   --------------------
   -- Stop_All_Tasks --
   --------------------

   procedure Stop_All_Tasks is
   begin
      null;
   end Stop_All_Tasks;

   ---------------
   -- Stop_Task --
   ---------------

   function Stop_Task (T : ST.Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Stop_Task;

   -------------------
   -- Continue_Task --
   -------------------

   function Continue_Task (T : ST.Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Continue_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_Id) is
      act     : aliased struct_sigaction;
      old_act : aliased struct_sigaction;
      Tmp_Set : aliased sigset_t;
      Result  : Interfaces.C.int;

      function State
        (Int : System.Interrupt_Management.Interrupt_ID) return Character;
      pragma Import (C, State, "__gnat_get_interrupt_state");
      --  Get interrupt state.  Defined in a-init.c
      --  The input argument is the interrupt number,
      --  and the result is one of the following:

      Default : constant Character := 's';
      --    'n'   this interrupt not set by any Interrupt_State pragma
      --    'u'   Interrupt_State pragma set state to User
      --    'r'   Interrupt_State pragma set state to Runtime
      --    's'   Interrupt_State pragma set state to System (use "default"
      --           system handler)

   begin
      Environment_Task_Id := Environment_Task;
      Environment_Task.Common.LL.Thread := pthread_self;

      Interrupt_Management.Initialize;

      --  Prepare the set of signals that should unblocked in all tasks

      Result := sigemptyset (Unblocked_Signal_Mask'Access);
      pragma Assert (Result = 0);

      for J in Interrupt_Management.Interrupt_ID loop
         if System.Interrupt_Management.Keep_Unmasked (J) then
            Result := sigaddset (Unblocked_Signal_Mask'Access, Signal (J));
            pragma Assert (Result = 0);
         end if;
      end loop;

      --  Initialize the lock used to synchronize chain of all ATCBs

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      Specific.Initialize (Environment_Task);

      if Use_Alternate_Stack then
         Environment_Task.Common.Task_Alternate_Stack :=
           Alternate_Stack'Address;
      end if;

      --  Make environment task known here because it doesn't go through
      --  Activate_Tasks, which does it for all other tasks.

      Known_Tasks (Known_Tasks'First) := Environment_Task;
      Environment_Task.Known_Tasks_Index := Known_Tasks'First;

      Enter_Task (Environment_Task);

      if State
          (System.Interrupt_Management.Abort_Task_Interrupt) /= Default
      then
         act.sa_flags := 0;
         act.sa_handler := Abort_Handler'Address;

         Result := sigemptyset (Tmp_Set'Access);
         pragma Assert (Result = 0);
         act.sa_mask := Tmp_Set;

         Result :=
           sigaction
             (Signal (System.Interrupt_Management.Abort_Task_Interrupt),
              act'Unchecked_Access,
              old_act'Unchecked_Access);
         pragma Assert (Result = 0);
         Abort_Handler_Installed := True;
      end if;
   end Initialize;

   -----------------------
   -- Set_Task_Affinity --
   -----------------------

   procedure Set_Task_Affinity (T : ST.Task_Id) is
      use type Multiprocessors.CPU_Range;

      function Thread_Ctl_Ext
        (Pid     : pid_t;
         Tid     : Thread_Id;
         Command : Interfaces.C.unsigned;
         Runmask : Interfaces.C.size_t) return Interfaces.C.int
      with
        Import, Convention => C, External_Name => "ThreadCtlExt";
      --  Thread_Ctl_Ext is a generic thread control function in QNX.
      --  It is defined locally because in the C API its second
      --  argument is a void pointer that takes different actual
      --  pointer types or values depending on the command. This
      --  particular instance of this function only accepts the
      --  NTO_TCTL_RUNMASK command. The void * pointer in the C
      --  interface is interpreted as bitmask for this command.
      --  In the binding size_t is used as an integer type that
      --  always has the same size as a pointer.

      NTO_TCTL_RUNMASK : constant := 4;
      --  Command for Thread_Ctl. Using this command in Thread_Ctl
      --  allows the caller to pass a bitmask that describes on
      --  which CPU the current thread is allowed to run on.

      Pid     : constant pid_t := getpid;
      Result  : Interfaces.C.int;
      Runmask : Interfaces.C.size_t;
      --  Each set bit in runmask represents a processor that the thread
      --  can run on. If all bits are set to one the thread can run on any CPU.
   begin
      if T.Common.Base_CPU = Multiprocessors.Not_A_Specific_CPU then
         Runmask := Interfaces.C.size_t'Last;
      else
         Runmask :=
           Interfaces.C.size_t
             (2 ** Natural (T.Common.Base_CPU - Multiprocessors.CPU'First));
      end if;
      Result :=
         Thread_Ctl_Ext (Pid, Get_Thread_Id (T), NTO_TCTL_RUNMASK, Runmask);
      pragma Assert (Result = 0);
   end Set_Task_Affinity;

end System.Task_Primitives.Operations;
