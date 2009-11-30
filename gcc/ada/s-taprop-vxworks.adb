------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2009, Free Software Foundation, Inc.          --
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

--  This is the VxWorks version of this package

--  This package contains all the GNULL primitives that interface directly with
--  the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces.C;

with System.Tasking.Debug;
with System.Interrupt_Management;

with System.Soft_Links;
--  We use System.Soft_Links instead of System.Tasking.Initialization
--  because the later is a higher level package that we shouldn't depend
--  on. For example when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Stages.

with System.Task_Info;
with System.VxWorks.Ext;

package body System.Task_Primitives.Operations is

   package SSL renames System.Soft_Links;

   use System.Tasking.Debug;
   use System.Tasking;
   use System.OS_Interface;
   use System.Parameters;
   use type System.VxWorks.Ext.t_id;
   use type Interfaces.C.int;

   subtype int is System.OS_Interface.int;

   Relative : constant := 0;

   ----------------
   -- Local Data --
   ----------------

   --  The followings are logically constants, but need to be initialized at
   --  run time.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at a
   --  time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   Environment_Task_Id : Task_Id;
   --  A variable to hold Task_Id for the environment task

   Unblocked_Signal_Mask : aliased sigset_t;
   --  The set of signals that should unblocked in all tasks

   --  The followings are internal configuration constants needed

   Time_Slice_Val : Integer;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   Dispatching_Policy : Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   function Get_Policy (Prio : System.Any_Priority) return Character;
   pragma Import (C, Get_Policy, "__gnat_get_specific_dispatching");
   --  Get priority specific dispatching policy

   Mutex_Protocol : Priority_Type;

   Foreign_Task_Elaborated : aliased Boolean := True;
   --  Used to identified fake tasks (i.e., non-Ada Threads)

   type Set_Stack_Limit_Proc_Acc is access procedure;
   pragma Convention (C, Set_Stack_Limit_Proc_Acc);

   Set_Stack_Limit_Hook : Set_Stack_Limit_Proc_Acc;
   pragma Import (C, Set_Stack_Limit_Hook, "__gnat_set_stack_limit_hook");
   --  Procedure to be called when a task is created to set stack
   --  limit.

   --------------------
   -- Local Packages --
   --------------------

   package Specific is

      procedure Initialize;
      pragma Inline (Initialize);
      --  Initialize task specific data

      function Is_Valid_Task return Boolean;
      pragma Inline (Is_Valid_Task);
      --  Does executing thread have a TCB?

      procedure Set (Self_Id : Task_Id);
      pragma Inline (Set);
      --  Set the self id for the current task

      procedure Delete;
      pragma Inline (Delete);
      --  Delete the task specific data associated with the current task

      function Self return Task_Id;
      pragma Inline (Self);
      --  Return a pointer to the Ada Task Control Block of the calling task

   end Specific;

   package body Specific is separate;
   --  The body of this package is target specific

   ---------------------------------
   -- Support for foreign threads --
   ---------------------------------

   function Register_Foreign_Thread (Thread : Thread_Id) return Task_Id;
   --  Allocate and Initialize a new ATCB for the current Thread

   function Register_Foreign_Thread
     (Thread : Thread_Id) return Task_Id is separate;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Abort_Handler (signo : Signal);
   --  Handler for the abort (SIGABRT) signal to handle asynchronous abort

   procedure Install_Signal_Handlers;
   --  Install the default signal handlers for the current task

   function To_Address is
     new Ada.Unchecked_Conversion (Task_Id, System.Address);

   -------------------
   -- Abort_Handler --
   -------------------

   procedure Abort_Handler (signo : Signal) is
      pragma Unreferenced (signo);

      Self_ID : constant Task_Id := Self;
      Old_Set : aliased sigset_t;

      Result : int;
      pragma Warnings (Off, Result);

   begin
      --  It is not safe to raise an exception when using ZCX and the GCC
      --  exception handling mechanism.

      if ZCX_By_Default and then GCC_ZCX_Support then
         return;
      end if;

      if Self_ID.Deferral_Level = 0
        and then Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
        and then not Self_ID.Aborting
      then
         Self_ID.Aborting := True;

         --  Make sure signals used for RTS internal purpose are unmasked

         Result :=
           pthread_sigmask
             (SIG_UNBLOCK,
              Unblocked_Signal_Mask'Access,
              Old_Set'Access);
         pragma Assert (Result = 0);

         raise Standard'Abort_Signal;
      end if;
   end Abort_Handler;

   -----------------
   -- Stack_Guard --
   -----------------

   procedure Stack_Guard (T : ST.Task_Id; On : Boolean) is
      pragma Unreferenced (T);
      pragma Unreferenced (On);

   begin
      --  Nothing needed (why not???)

      null;
   end Stack_Guard;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id (T : ST.Task_Id) return OSI.Thread_Id is
   begin
      return T.Common.LL.Thread;
   end Get_Thread_Id;

   ----------
   -- Self --
   ----------

   function Self return Task_Id renames Specific.Self;

   -----------------------------
   -- Install_Signal_Handlers --
   -----------------------------

   procedure Install_Signal_Handlers is
      act     : aliased struct_sigaction;
      old_act : aliased struct_sigaction;
      Tmp_Set : aliased sigset_t;
      Result  : int;

   begin
      act.sa_flags := 0;
      act.sa_handler := Abort_Handler'Address;

      Result := sigemptyset (Tmp_Set'Access);
      pragma Assert (Result = 0);
      act.sa_mask := Tmp_Set;

      Result :=
        sigaction
          (Signal (Interrupt_Management.Abort_Task_Interrupt),
           act'Unchecked_Access,
           old_act'Unchecked_Access);
      pragma Assert (Result = 0);

      Interrupt_Management.Initialize_Interrupts;
   end Install_Signal_Handlers;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : not null access Lock)
   is
   begin
      L.Mutex := semMCreate (SEM_Q_PRIORITY + SEM_INVERSION_SAFE);
      L.Prio_Ceiling := int (Prio);
      L.Protocol := Mutex_Protocol;
      pragma Assert (L.Mutex /= 0);
   end Initialize_Lock;

   procedure Initialize_Lock
     (L     : not null access RTS_Lock;
      Level : Lock_Level)
   is
      pragma Unreferenced (Level);
   begin
      L.Mutex := semMCreate (SEM_Q_PRIORITY + SEM_INVERSION_SAFE);
      L.Prio_Ceiling := int (System.Any_Priority'Last);
      L.Protocol := Mutex_Protocol;
      pragma Assert (L.Mutex /= 0);
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : not null access Lock) is
      Result : int;
   begin
      Result := semDelete (L.Mutex);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   procedure Finalize_Lock (L : not null access RTS_Lock) is
      Result : int;
   begin
      Result := semDelete (L.Mutex);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock
     (L                 : not null access Lock;
      Ceiling_Violation : out Boolean)
   is
      Result : int;

   begin
      if L.Protocol = Prio_Protect
        and then int (Self.Common.Current_Priority) > L.Prio_Ceiling
      then
         Ceiling_Violation := True;
         return;
      else
         Ceiling_Violation := False;
      end if;

      Result := semTake (L.Mutex, WAIT_FOREVER);
      pragma Assert (Result = 0);
   end Write_Lock;

   procedure Write_Lock
     (L           : not null access RTS_Lock;
      Global_Lock : Boolean := False)
   is
      Result : int;
   begin
      if not Single_Lock or else Global_Lock then
         Result := semTake (L.Mutex, WAIT_FOREVER);
         pragma Assert (Result = 0);
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_Id) is
      Result : int;
   begin
      if not Single_Lock then
         Result := semTake (T.Common.LL.L.Mutex, WAIT_FOREVER);
         pragma Assert (Result = 0);
      end if;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock
     (L                 : not null access Lock;
      Ceiling_Violation : out Boolean)
   is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : not null access Lock) is
      Result : int;
   begin
      Result := semGive (L.Mutex);
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock
     (L           : not null access RTS_Lock;
      Global_Lock : Boolean := False)
   is
      Result : int;
   begin
      if not Single_Lock or else Global_Lock then
         Result := semGive (L.Mutex);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   procedure Unlock (T : Task_Id) is
      Result : int;
   begin
      if not Single_Lock then
         Result := semGive (T.Common.LL.L.Mutex);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   -----------------
   -- Set_Ceiling --
   -----------------

   --  Dynamic priority ceilings are not supported by the underlying system

   procedure Set_Ceiling
     (L    : not null access Lock;
      Prio : System.Any_Priority)
   is
      pragma Unreferenced (L, Prio);
   begin
      null;
   end Set_Ceiling;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Self_ID : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Unreferenced (Reason);

      Result : int;

   begin
      pragma Assert (Self_ID = Self);

      --  Release the mutex before sleeping

      Result :=
        semGive (if Single_Lock
                 then Single_RTS_Lock.Mutex
                 else Self_ID.Common.LL.L.Mutex);
      pragma Assert (Result = 0);

      --  Perform a blocking operation to take the CV semaphore. Note that a
      --  blocking operation in VxWorks will reenable task scheduling. When we
      --  are no longer blocked and control is returned, task scheduling will
      --  again be disabled.

      Result := semTake (Self_ID.Common.LL.CV, WAIT_FOREVER);
      pragma Assert (Result = 0);

      --  Take the mutex back

      Result :=
        semTake ((if Single_Lock
                  then Single_RTS_Lock.Mutex
                  else Self_ID.Common.LL.L.Mutex), WAIT_FOREVER);
      pragma Assert (Result = 0);
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is assumed to be
   --  already deferred, and the caller should be holding its own ATCB lock.

   procedure Timed_Sleep
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      pragma Unreferenced (Reason);

      Orig     : constant Duration := Monotonic_Clock;
      Absolute : Duration;
      Ticks    : int;
      Result   : int;
      Wakeup   : Boolean := False;

   begin
      Timedout := False;
      Yielded  := True;

      if Mode = Relative then
         Absolute := Orig + Time;

         --  Systematically add one since the first tick will delay *at most*
         --  1 / Rate_Duration seconds, so we need to add one to be on the
         --  safe side.

         Ticks := To_Clock_Ticks (Time);

         if Ticks > 0 and then Ticks < int'Last then
            Ticks := Ticks + 1;
         end if;

      else
         Absolute := Time;
         Ticks    := To_Clock_Ticks (Time - Monotonic_Clock);
      end if;

      if Ticks > 0 then
         loop
            --  Release the mutex before sleeping

            Result :=
              semGive (if Single_Lock
                       then Single_RTS_Lock.Mutex
                       else Self_ID.Common.LL.L.Mutex);
            pragma Assert (Result = 0);

            --  Perform a blocking operation to take the CV semaphore. Note
            --  that a blocking operation in VxWorks will reenable task
            --  scheduling. When we are no longer blocked and control is
            --  returned, task scheduling will again be disabled.

            Result := semTake (Self_ID.Common.LL.CV, Ticks);

            if Result = 0 then

               --  Somebody may have called Wakeup for us

               Wakeup := True;

            else
               if errno /= S_objLib_OBJ_TIMEOUT then
                  Wakeup := True;

               else
                  --  If Ticks = int'last, it was most probably truncated so
                  --  let's make another round after recomputing Ticks from
                  --  the absolute time.

                  if Ticks /= int'Last then
                     Timedout := True;

                  else
                     Ticks := To_Clock_Ticks (Absolute - Monotonic_Clock);

                     if Ticks < 0 then
                        Timedout := True;
                     end if;
                  end if;
               end if;
            end if;

            --  Take the mutex back

            Result :=
              semTake ((if Single_Lock
                        then Single_RTS_Lock.Mutex
                        else Self_ID.Common.LL.L.Mutex), WAIT_FOREVER);
            pragma Assert (Result = 0);

            exit when Timedout or Wakeup;
         end loop;

      else
         Timedout := True;

         --  Should never hold a lock while yielding

         if Single_Lock then
            Result := semGive (Single_RTS_Lock.Mutex);
            taskDelay (0);
            Result := semTake (Single_RTS_Lock.Mutex, WAIT_FOREVER);

         else
            Result := semGive (Self_ID.Common.LL.L.Mutex);
            taskDelay (0);
            Result := semTake (Self_ID.Common.LL.L.Mutex, WAIT_FOREVER);
         end if;
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so we assume the
   --  caller is holding no locks.

   procedure Timed_Delay
     (Self_ID : Task_Id;
      Time    : Duration;
      Mode    : ST.Delay_Modes)
   is
      Orig     : constant Duration := Monotonic_Clock;
      Absolute : Duration;
      Ticks    : int;
      Timedout : Boolean;
      Aborted  : Boolean := False;

      Result : int;
      pragma Warnings (Off, Result);

   begin
      if Mode = Relative then
         Absolute := Orig + Time;
         Ticks    := To_Clock_Ticks (Time);

         if Ticks > 0 and then Ticks < int'Last then

            --  First tick will delay anytime between 0 and 1 / sysClkRateGet
            --  seconds, so we need to add one to be on the safe side.

            Ticks := Ticks + 1;
         end if;

      else
         Absolute := Time;
         Ticks    := To_Clock_Ticks (Time - Orig);
      end if;

      if Ticks > 0 then

         --  Modifying State, locking the TCB

         Result :=
           semTake ((if Single_Lock
                     then Single_RTS_Lock.Mutex
                     else Self_ID.Common.LL.L.Mutex), WAIT_FOREVER);

         pragma Assert (Result = 0);

         Self_ID.Common.State := Delay_Sleep;
         Timedout := False;

         loop
            Aborted := Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            --  Release the TCB before sleeping

            Result :=
              semGive (if Single_Lock
                       then Single_RTS_Lock.Mutex
                       else Self_ID.Common.LL.L.Mutex);
            pragma Assert (Result = 0);

            exit when Aborted;

            Result := semTake (Self_ID.Common.LL.CV, Ticks);

            if Result /= 0 then

               --  If Ticks = int'last, it was most probably truncated
               --  so let's make another round after recomputing Ticks
               --  from the absolute time.

               if errno = S_objLib_OBJ_TIMEOUT and then Ticks /= int'Last then
                  Timedout := True;
               else
                  Ticks := To_Clock_Ticks (Absolute - Monotonic_Clock);

                  if Ticks < 0 then
                     Timedout := True;
                  end if;
               end if;
            end if;

            --  Take back the lock after having slept, to protect further
            --  access to Self_ID.

            Result :=
              semTake
                ((if Single_Lock
                  then Single_RTS_Lock.Mutex
                  else Self_ID.Common.LL.L.Mutex), WAIT_FOREVER);

            pragma Assert (Result = 0);

            exit when Timedout;
         end loop;

         Self_ID.Common.State := Runnable;

         Result :=
           semGive
             (if Single_Lock
              then Single_RTS_Lock.Mutex
              else Self_ID.Common.LL.L.Mutex);

      else
         taskDelay (0);
      end if;
   end Timed_Delay;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      TS     : aliased timespec;
      Result : int;
   begin
      Result := clock_gettime (CLOCK_REALTIME, TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return To_Duration (TS);
   end Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      return 1.0 / Duration (sysClkRateGet);
   end RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Unreferenced (Reason);
      Result : int;
   begin
      Result := semGive (T.Common.LL.CV);
      pragma Assert (Result = 0);
   end Wakeup;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
      pragma Unreferenced (Do_Yield);
      Result : int;
      pragma Unreferenced (Result);
   begin
      Result := taskDelay (0);
   end Yield;

   ------------------
   -- Set_Priority --
   ------------------

   type Prio_Array_Type is array (System.Any_Priority) of Integer;
   pragma Atomic_Components (Prio_Array_Type);

   Prio_Array : Prio_Array_Type;
   --  Global array containing the id of the currently running task for each
   --  priority. Note that we assume that we are on a single processor with
   --  run-till-blocked scheduling.

   procedure Set_Priority
     (T                   : Task_Id;
      Prio                : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      Array_Item : Integer;
      Result     : int;

   begin
      Result :=
        taskPrioritySet
          (T.Common.LL.Thread, To_VxWorks_Priority (int (Prio)));
      pragma Assert (Result = 0);

      if (Dispatching_Policy = 'F' or else Get_Policy (Prio) = 'F')
        and then Loss_Of_Inheritance
        and then Prio < T.Common.Current_Priority
      then
         --  Annex D requirement (RM D.2.2(9)):

         --    If the task drops its priority due to the loss of inherited
         --    priority, it is added at the head of the ready queue for its
         --    new active priority.

         Array_Item := Prio_Array (T.Common.Base_Priority) + 1;
         Prio_Array (T.Common.Base_Priority) := Array_Item;

         loop
            --  Give some processes a chance to arrive

            taskDelay (0);

            --  Then wait for our turn to proceed

            exit when Array_Item = Prio_Array (T.Common.Base_Priority)
              or else Prio_Array (T.Common.Base_Priority) = 1;
         end loop;

         Prio_Array (T.Common.Base_Priority) :=
           Prio_Array (T.Common.Base_Priority) - 1;
      end if;

      T.Common.Current_Priority := Prio;
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
      procedure Init_Float;
      pragma Import (C, Init_Float, "__gnat_init_float");
      --  Properly initializes the FPU for PPC/MIPS systems

   begin
      --  Store the user-level task id in the Thread field (to be used
      --  internally by the run-time system) and the kernel-level task id in
      --  the LWP field (to be used by the debugger).

      Self_ID.Common.LL.Thread := taskIdSelf;
      Self_ID.Common.LL.LWP := getpid;

      Specific.Set (Self_ID);

      Init_Float;

      --  Install the signal handlers

      --  This is called for each task since there is no signal inheritance
      --  between VxWorks tasks.

      Install_Signal_Handlers;

      --  If stack checking is enabled, set the stack limit for this task

      if Set_Stack_Limit_Hook /= null then
         Set_Stack_Limit_Hook.all;
      end if;
   end Enter_Task;

   --------------
   -- New_ATCB --
   --------------

   function New_ATCB (Entry_Num : Task_Entry_Index) return Task_Id is
   begin
      return new Ada_Task_Control_Block (Entry_Num);
   end New_ATCB;

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
         return Register_Foreign_Thread (taskIdSelf);
      end if;
   end Register_Foreign_Thread;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
   begin
      Self_ID.Common.LL.CV := semBCreate (SEM_Q_PRIORITY, SEM_EMPTY);
      Self_ID.Common.LL.Thread := 0;

      if Self_ID.Common.LL.CV = 0 then
         Succeeded := False;

      else
         Succeeded := True;

         if not Single_Lock then
            Initialize_Lock (Self_ID.Common.LL.L'Access, ATCB_Level);
         end if;
      end if;
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
      Adjusted_Stack_Size : size_t;
      Result : int;

      use System.Task_Info;

   begin
      --  Ask for four extra bytes of stack space so that the ATCB pointer can
      --  be stored below the stack limit, plus extra space for the frame of
      --  Task_Wrapper. This is so the user gets the amount of stack requested
      --  exclusive of the needs.

      --  We also have to allocate n more bytes for the task name storage and
      --  enough space for the Wind Task Control Block which is around 0x778
      --  bytes. VxWorks also seems to carve out additional space, so use 2048
      --  as a nice round number. We might want to increment to the nearest
      --  page size in case we ever support VxVMI.

      --  ??? - we should come back and visit this so we can set the task name
      --        to something appropriate.

      Adjusted_Stack_Size := size_t (Stack_Size) + 2048;

      --  Since the initial signal mask of a thread is inherited from the
      --  creator, and the Environment task has all its signals masked, we do
      --  not need to manipulate caller's signal mask at this point. All tasks
      --  in RTS will have All_Tasks_Mask initially.

      --  We now compute the VxWorks task name and options, then spawn ...

      declare
         Name         : aliased String (1 .. T.Common.Task_Image_Len + 1);
         Name_Address : System.Address;
         --  Task name we are going to hand down to VxWorks

         function Get_Task_Options return int;
         pragma Import (C, Get_Task_Options, "__gnat_get_task_options");
         --  Function that returns the options to be set for the task that we
         --  are creating. We fetch the options assigned to the current task,
         --  so offering some user level control over the options for a task
         --  hierarchy, and force VX_FP_TASK because it is almost always
         --  required.

      begin
         --  If there is no Ada task name handy, let VxWorks choose one.
         --  Otherwise, tell VxWorks what the Ada task name is.

         if T.Common.Task_Image_Len = 0 then
            Name_Address := System.Null_Address;
         else
            Name (1 .. Name'Last - 1) :=
              T.Common.Task_Image (1 .. T.Common.Task_Image_Len);
            Name (Name'Last) := ASCII.NUL;
            Name_Address := Name'Address;
         end if;

         --  Now spawn the VxWorks task for real

         T.Common.LL.Thread :=
           taskSpawn
             (Name_Address,
              To_VxWorks_Priority (int (Priority)),
              Get_Task_Options,
              Adjusted_Stack_Size,
              Wrapper,
              To_Address (T));
      end;

      --  Set processor affinity

      if T.Common.Task_Info /= Unspecified_Task_Info then
         Result :=
           taskCpuAffinitySet (T.Common.LL.Thread, T.Common.Task_Info);

         if Result = -1 then
            taskDelete (T.Common.LL.Thread);
            T.Common.LL.Thread := -1;
         end if;
      end if;

      if T.Common.LL.Thread = -1 then
         Succeeded := False;
      else
         Succeeded := True;
         Task_Creation_Hook (T.Common.LL.Thread);
         Set_Priority (T, Priority);
      end if;
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_Id) is
      Result  : int;
      Tmp     : Task_Id          := T;
      Is_Self : constant Boolean := (T = Self);

      procedure Free is new
        Ada.Unchecked_Deallocation (Ada_Task_Control_Block, Task_Id);

   begin
      if not Single_Lock then
         Result := semDelete (T.Common.LL.L.Mutex);
         pragma Assert (Result = 0);
      end if;

      T.Common.LL.Thread := 0;

      Result := semDelete (T.Common.LL.CV);
      pragma Assert (Result = 0);

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;

      Free (Tmp);

      if Is_Self then
         Specific.Delete;
      end if;
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      Specific.Set (null);
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
      Result : int;
   begin
      Result :=
        kill
          (T.Common.LL.Thread,
           Signal (Interrupt_Management.Abort_Task_Interrupt));
      pragma Assert (Result = 0);
   end Abort_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Suspension_Object) is
   begin
      --  Initialize internal state (always to False (RM D.10(6)))

      S.State := False;
      S.Waiting := False;

      --  Initialize internal mutex

      --  Use simpler binary semaphore instead of VxWorks
      --  mutual exclusion semaphore, because we don't need
      --  the fancier semantics and their overhead.

      S.L := semBCreate (SEM_Q_FIFO, SEM_FULL);

      --  Initialize internal condition variable

      S.CV := semBCreate (SEM_Q_FIFO, SEM_EMPTY);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Suspension_Object) is
      pragma Unmodified (S);
      --  S may be modified on other targets, but not on VxWorks

      Result : STATUS;

   begin
      --  Destroy internal mutex

      Result := semDelete (S.L);
      pragma Assert (Result = OK);

      --  Destroy internal condition variable

      Result := semDelete (S.CV);
      pragma Assert (Result = OK);
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
      Result : STATUS;

   begin
      SSL.Abort_Defer.all;

      Result := semTake (S.L, WAIT_FOREVER);
      pragma Assert (Result = OK);

      S.State := False;

      Result := semGive (S.L);
      pragma Assert (Result = OK);

      SSL.Abort_Undefer.all;
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
      Result : STATUS;

   begin
      SSL.Abort_Defer.all;

      Result := semTake (S.L, WAIT_FOREVER);
      pragma Assert (Result = OK);

      --  If there is already a task waiting on this suspension object then
      --  we resume it, leaving the state of the suspension object to False,
      --  as it is specified in ARM D.10 par. 9. Otherwise, it just leaves
      --  the state to True.

      if S.Waiting then
         S.Waiting := False;
         S.State := False;

         Result := semGive (S.CV);
         pragma Assert (Result = OK);
      else
         S.State := True;
      end if;

      Result := semGive (S.L);
      pragma Assert (Result = OK);

      SSL.Abort_Undefer.all;
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      Result : STATUS;

   begin
      SSL.Abort_Defer.all;

      Result := semTake (S.L, WAIT_FOREVER);

      if S.Waiting then

         --  Program_Error must be raised upon calling Suspend_Until_True
         --  if another task is already waiting on that suspension object
         --  (ARM D.10 par. 10).

         Result := semGive (S.L);
         pragma Assert (Result = OK);

         SSL.Abort_Undefer.all;

         raise Program_Error;

      else
         --  Suspend the task if the state is False. Otherwise, the task
         --  continues its execution, and the state of the suspension object
         --  is set to False (ARM D.10 par. 9).

         if S.State then
            S.State := False;

            Result := semGive (S.L);
            pragma Assert (Result = 0);

            SSL.Abort_Undefer.all;

         else
            S.Waiting := True;

            --  Release the mutex before sleeping

            Result := semGive (S.L);
            pragma Assert (Result = OK);

            SSL.Abort_Undefer.all;

            Result := semTake (S.CV, WAIT_FOREVER);
            pragma Assert (Result = 0);
         end if;
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
      Write_Lock (Single_RTS_Lock'Access, Global_Lock => True);
   end Lock_RTS;

   ----------------
   -- Unlock_RTS --
   ----------------

   procedure Unlock_RTS is
   begin
      Unlock (Single_RTS_Lock'Access, Global_Lock => True);
   end Unlock_RTS;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
   begin
      if T.Common.LL.Thread /= 0
        and then T.Common.LL.Thread /= Thread_Self
      then
         return taskSuspend (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
   begin
      if T.Common.LL.Thread /= 0
        and then T.Common.LL.Thread /= Thread_Self
      then
         return taskResume (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Resume_Task;

   --------------------
   -- Stop_All_Tasks --
   --------------------

   procedure Stop_All_Tasks
   is
      Thread_Self : constant Thread_Id := taskIdSelf;
      C           : Task_Id;

      Dummy : int;
      pragma Unreferenced (Dummy);

   begin
      Dummy := Int_Lock;

      C := All_Tasks_List;
      while C /= null loop
         if C.Common.LL.Thread /= 0
           and then C.Common.LL.Thread /= Thread_Self
         then
            Dummy := Task_Stop (C.Common.LL.Thread);
         end if;

         C := C.Common.All_Tasks_Link;
      end loop;

      Dummy := Int_Unlock;
   end Stop_All_Tasks;

   ---------------
   -- Stop_Task --
   ---------------

   function Stop_Task (T : ST.Task_Id) return Boolean is
   begin
      if T.Common.LL.Thread /= 0 then
         return Task_Stop (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Stop_Task;

   -------------------
   -- Continue_Task --
   -------------------

   function Continue_Task (T : ST.Task_Id) return Boolean
   is
   begin
      if T.Common.LL.Thread /= 0 then
         return Task_Cont (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Continue_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_Id) is
      Result : int;

   begin
      Environment_Task_Id := Environment_Task;

      Interrupt_Management.Initialize;
      Specific.Initialize;

      if Locking_Policy = 'C' then
         Mutex_Protocol := Prio_Protect;
      elsif Locking_Policy = 'I' then
         Mutex_Protocol := Prio_Inherit;
      else
         Mutex_Protocol := Prio_None;
      end if;

      if Time_Slice_Val > 0 then
         Result :=
           Set_Time_Slice
             (To_Clock_Ticks
                (Duration (Time_Slice_Val) / Duration (1_000_000.0)));

      elsif Dispatching_Policy = 'R' then
         Result := Set_Time_Slice (To_Clock_Ticks (0.01));

      end if;

      Result := sigemptyset (Unblocked_Signal_Mask'Access);
      pragma Assert (Result = 0);

      for J in Interrupt_Management.Signal_ID loop
         if System.Interrupt_Management.Keep_Unmasked (J) then
            Result := sigaddset (Unblocked_Signal_Mask'Access, Signal (J));
            pragma Assert (Result = 0);
         end if;
      end loop;

      --  Initialize the lock used to synchronize chain of all ATCBs

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      --  Make environment task known here because it doesn't go through
      --  Activate_Tasks, which does it for all other tasks.

      Known_Tasks (Known_Tasks'First) := Environment_Task;
      Environment_Task.Known_Tasks_Index := Known_Tasks'First;

      Enter_Task (Environment_Task);
   end Initialize;

end System.Task_Primitives.Operations;
