------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--         Copyright (C) 1992-2002, Free Software Foundation, Inc.          --
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

--  This is the VxWorks version of this package

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Tasking.Debug;
--  used for Known_Tasks

with System.Interrupt_Management;
--  used for Keep_Unmasked
--           Abort_Task_Interrupt
--           Interrupt_ID
--           Initialize_Interrupts

with System.Soft_Links;
--  used for Defer/Undefer_Abort

--  Note that we do not use System.Tasking.Initialization directly since
--  this is a higher level package that we shouldn't depend on. For example
--  when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Initialization

with System.OS_Interface;
--  used for various type, constant, and operations

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Ada_Task_Control_Block
--           Task_ID
--           ATCB components and types

with System.Task_Info;
--  used for Task_Image

with Interfaces.C;

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Task_Primitives.Operations is

   use System.Tasking.Debug;
   use System.Tasking;
   use System.Task_Info;
   use System.OS_Interface;
   use System.Parameters;
   use type Interfaces.C.int;

   package SSL renames System.Soft_Links;

   subtype int is System.OS_Interface.int;

   Relative : constant := 0;

   ----------------
   -- Local Data --
   ----------------

   --  The followings are logically constants, but need to be initialized
   --  at run time.

   Current_Task : aliased Task_ID;
   pragma Export (Ada, Current_Task);
   --  Task specific value used to store the Ada Task_ID.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   Environment_Task_ID : Task_ID;
   --  A variable to hold Task_ID for the environment task.

   Unblocked_Signal_Mask : aliased sigset_t;
   --  The set of signals that should unblocked in all tasks

   --  The followings are internal configuration constants needed.

   Time_Slice_Val : Integer;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   Dispatching_Policy : Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   FIFO_Within_Priorities : constant Boolean := Dispatching_Policy = 'F';
   --  Indicates whether FIFO_Within_Priorities is set.

   Mutex_Protocol : Priority_Type;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Abort_Handler (signo : Signal);

   function To_Address is new Unchecked_Conversion (Task_ID, System.Address);

   -------------------
   -- Abort_Handler --
   -------------------

   procedure Abort_Handler (signo : Signal) is
      Self_ID : constant Task_ID := Self;
      Result  : int;
      Old_Set : aliased sigset_t;

   begin
      if Self_ID.Deferral_Level = 0
        and then Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
        and then not Self_ID.Aborting
      then
         Self_ID.Aborting := True;

         --  Make sure signals used for RTS internal purpose are unmasked

         Result := pthread_sigmask (SIG_UNBLOCK,
           Unblocked_Signal_Mask'Unchecked_Access, Old_Set'Unchecked_Access);
         pragma Assert (Result = 0);

         raise Standard'Abort_Signal;
      end if;
   end Abort_Handler;

   -----------------
   -- Stack_Guard --
   -----------------

   procedure Stack_Guard (T : ST.Task_ID; On : Boolean) is
   begin
      --  Nothing needed.
      null;
   end Stack_Guard;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id (T : ST.Task_ID) return OSI.Thread_Id is
   begin
      return T.Common.LL.Thread;
   end Get_Thread_Id;

   ----------
   -- Self --
   ----------

   function Self return Task_ID is
   begin
      pragma Assert (Current_Task /= null);
      return Current_Task;
   end Self;

   -----------------------------
   -- Install_Signal_Handlers --
   -----------------------------

   procedure Install_Signal_Handlers;
   --  Install the default signal handlers for the current task.

   procedure Install_Signal_Handlers is
      act       : aliased struct_sigaction;
      old_act   : aliased struct_sigaction;
      Tmp_Set   : aliased sigset_t;
      Result    : int;

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

   procedure Initialize_Lock (Prio : System.Any_Priority; L : access Lock) is
   begin
      L.Mutex := semMCreate (SEM_Q_PRIORITY + SEM_INVERSION_SAFE);
      L.Prio_Ceiling := int (Prio);
      L.Protocol := Mutex_Protocol;
      pragma Assert (L.Mutex /= 0);
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock; Level : Lock_Level) is
   begin
      L.Mutex := semMCreate (SEM_Q_PRIORITY + SEM_INVERSION_SAFE);
      L.Prio_Ceiling := int (System.Any_Priority'Last);
      L.Protocol := Mutex_Protocol;
      pragma Assert (L.Mutex /= 0);
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
      Result : int;
   begin
      Result := semDelete (L.Mutex);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
      Result : int;
   begin
      Result := semDelete (L.Mutex);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
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
     (L : access RTS_Lock; Global_Lock : Boolean := False)
   is
      Result : int;
   begin
      if not Single_Lock or else Global_Lock then
         Result := semTake (L.Mutex, WAIT_FOREVER);
         pragma Assert (Result = 0);
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
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

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : access Lock) is
      Result  : int;
   begin
      Result := semGive (L.Mutex);
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock (L : access RTS_Lock; Global_Lock : Boolean := False) is
      Result : int;
   begin
      if not Single_Lock or else Global_Lock then
         Result := semGive (L.Mutex);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   procedure Unlock (T : Task_ID) is
      Result : int;
   begin
      if not Single_Lock then
         Result := semGive (T.Common.LL.L.Mutex);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Self_ID : Task_ID; Reason : System.Tasking.Task_States) is
      Result : int;
   begin
      pragma Assert (Self_ID = Self);

      --  Disable task scheduling.

      Result := taskLock;

      --  Release the mutex before sleeping.

      if Single_Lock then
         Result := semGive (Single_RTS_Lock.Mutex);
      else
         Result := semGive (Self_ID.Common.LL.L.Mutex);
      end if;

      pragma Assert (Result = 0);

      --  Indicate that there is another thread waiting on the CV.

      Self_ID.Common.LL.CV.Waiting := Self_ID.Common.LL.CV.Waiting + 1;

      --  Perform a blocking operation to take the CV semaphore.
      --  Note that a blocking operation in VxWorks will reenable
      --  task scheduling. When we are no longer blocked and control
      --  is returned, task scheduling will again be disabled.

      Result := semTake (Self_ID.Common.LL.CV.Sem, WAIT_FOREVER);

      if Result /= 0 then
         Self_ID.Common.LL.CV.Waiting := Self_ID.Common.LL.CV.Waiting - 1;
         pragma Assert (False);
      end if;

      --  Take the mutex back.

      if Single_Lock then
         Result := semTake (Single_RTS_Lock.Mutex, WAIT_FOREVER);
      else
         Result := semTake (Self_ID.Common.LL.L.Mutex, WAIT_FOREVER);
      end if;

      pragma Assert (Result = 0);

      --  Reenable task scheduling.

      Result := taskUnlock;
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.

   procedure Timed_Sleep
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      Ticks  : int;
      Result : int;

   begin
      Timedout := True;
      Yielded := True;

      if Mode = Relative then
         --  Systematically add one since the first tick will delay
         --  *at most* 1 / Rate_Duration seconds, so we need to add one to
         --  be on the safe side.

         Ticks := To_Clock_Ticks (Time) + 1;
      else
         Ticks := To_Clock_Ticks (Time - Monotonic_Clock);
      end if;

      if Ticks > 0 then
         --  Disable task scheduling.

         Result := taskLock;

         --  Release the mutex before sleeping.

         if Single_Lock then
            Result := semGive (Single_RTS_Lock.Mutex);
         else
            Result := semGive (Self_ID.Common.LL.L.Mutex);
         end if;

         pragma Assert (Result = 0);

         --  Indicate that there is another thread waiting on the CV.

         Self_ID.Common.LL.CV.Waiting := Self_ID.Common.LL.CV.Waiting + 1;

         --  Perform a blocking operation to take the CV semaphore.
         --  Note that a blocking operation in VxWorks will reenable
         --  task scheduling. When we are no longer blocked and control
         --  is returned, task scheduling will again be disabled.

         Result := semTake (Self_ID.Common.LL.CV.Sem, Ticks);

         if Result = 0 then
            --  Somebody may have called Wakeup for us

            Timedout := False;

         else
            Self_ID.Common.LL.CV.Waiting := Self_ID.Common.LL.CV.Waiting - 1;

            if errno /= S_objLib_OBJ_TIMEOUT then
               Timedout := False;
            end if;
         end if;

         --  Take the mutex back.

         if Single_Lock then
            Result := semTake (Single_RTS_Lock.Mutex, WAIT_FOREVER);
         else
            Result := semTake (Self_ID.Common.LL.L.Mutex, WAIT_FOREVER);
         end if;

         pragma Assert (Result = 0);

         --  Reenable task scheduling.

         Result := taskUnlock;

      else
         taskDelay (0);
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so
   --  we assume the caller is holding no locks.

   procedure Timed_Delay
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes)
   is
      Orig     : constant Duration := Monotonic_Clock;
      Absolute : Duration;
      Ticks    : int;
      Timedout : Boolean;
      Result   : int;

   begin
      SSL.Abort_Defer.all;

      if Single_Lock then
         Result := semTake (Single_RTS_Lock.Mutex, WAIT_FOREVER);
      else
         Result := semTake (Self_ID.Common.LL.L.Mutex, WAIT_FOREVER);
      end if;

      pragma Assert (Result = 0);

      if Mode = Relative then
         Absolute := Orig + Time;

         Ticks := To_Clock_Ticks (Time);

         if Ticks > 0 then
            --  The first tick will delay anytime between 0 and
            --  1 / sysClkRateGet seconds, so we need to add one to
            --  be on the safe side.

            Ticks := Ticks + 1;
         end if;
      else
         Absolute := Time;
         Ticks    := To_Clock_Ticks (Time - Orig);
      end if;

      if Ticks > 0 then
         Self_ID.Common.State := Delay_Sleep;

         loop
            if Self_ID.Pending_Priority_Change then
               Self_ID.Pending_Priority_Change := False;
               Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
               Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
            end if;

            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            Timedout := False;
            Result := taskLock;

            if Single_Lock then
               Result := semGive (Single_RTS_Lock.Mutex);
            else
               Result := semGive (Self_ID.Common.LL.L.Mutex);
            end if;

            pragma Assert (Result = 0);

            --  Indicate that there is another thread waiting on the CV.

            Self_ID.Common.LL.CV.Waiting := Self_ID.Common.LL.CV.Waiting + 1;

            Result := semTake (Self_ID.Common.LL.CV.Sem, Ticks);

            if Result /= 0 then
               Self_ID.Common.LL.CV.Waiting :=
                 Self_ID.Common.LL.CV.Waiting - 1;

               if errno = S_objLib_OBJ_TIMEOUT then
                  Timedout := True;
               else
                  Ticks := To_Clock_Ticks (Absolute - Monotonic_Clock);
               end if;
            end if;

            if Single_Lock then
               Result := semTake (Single_RTS_Lock.Mutex, WAIT_FOREVER);
            else
               Result := semTake (Self_ID.Common.LL.L.Mutex, WAIT_FOREVER);
            end if;

            pragma Assert (Result = 0);

            --  Reenable task scheduling.

            Result := taskUnlock;

            exit when Timedout;
         end loop;

         Self_ID.Common.State := Runnable;
      else
         taskDelay (0);
      end if;

      if Single_Lock then
         Result := semGive (Single_RTS_Lock.Mutex);
      else
         Result := semGive (Self_ID.Common.LL.L.Mutex);
      end if;

      pragma Assert (Result = 0);
      SSL.Abort_Undefer.all;
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
      return 10#1.0#E-6;
   end RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_ID; Reason : System.Tasking.Task_States) is
      Result : int;
   begin
      --  Disable task scheduling.

      Result := taskLock;

      --  Iff someone is currently waiting on the condition variable
      --  then release the semaphore; we don't want to leave the
      --  semaphore in the full state because the next guy to do
      --  a condition wait operation would not block.

      if T.Common.LL.CV.Waiting > 0 then
         Result := semGive (T.Common.LL.CV.Sem);

         --  One less thread waiting on the CV.

         T.Common.LL.CV.Waiting := T.Common.LL.CV.Waiting - 1;

         pragma Assert (Result = 0);
      end if;

      --  Reenable task scheduling.

      Result := taskUnlock;
   end Wakeup;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
      Result : int;
   begin
      Result := taskDelay (0);
   end Yield;

   ------------------
   -- Set_Priority --
   ------------------

   type Prio_Array_Type is array (System.Any_Priority) of Integer;
   pragma Atomic_Components (Prio_Array_Type);

   Prio_Array : Prio_Array_Type;
   --  Global array containing the id of the currently running task for
   --  each priority.
   --
   --  Note: we assume that we are on a single processor with run-til-blocked
   --  scheduling.

   procedure Set_Priority
     (T : Task_ID;
      Prio : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      Array_Item : Integer;
      Result     : int;

   begin
      Result := taskPrioritySet
        (T.Common.LL.Thread, To_VxWorks_Priority (int (Prio)));
      pragma Assert (Result = 0);

      if FIFO_Within_Priorities then
         --  Annex D requirement [RM D.2.2 par. 9]:
         --    If the task drops its priority due to the loss of inherited
         --    priority, it is added at the head of the ready queue for its
         --    new active priority.

         if Loss_Of_Inheritance
           and then Prio < T.Common.Current_Priority
         then
            Array_Item := Prio_Array (T.Common.Base_Priority) + 1;
            Prio_Array (T.Common.Base_Priority) := Array_Item;

            loop
               --  Let some processes a chance to arrive

               Yield;

               --  Then wait for our turn to proceed

               exit when Array_Item = Prio_Array (T.Common.Base_Priority)
                 or else Prio_Array (T.Common.Base_Priority) = 1;
            end loop;

            Prio_Array (T.Common.Base_Priority) :=
              Prio_Array (T.Common.Base_Priority) - 1;
         end if;
      end if;

      T.Common.Current_Priority := Prio;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_ID) return System.Any_Priority is
   begin
      return T.Common.Current_Priority;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_ID) is
      Result : int;

      procedure Init_Float;
      pragma Import (C, Init_Float, "__gnat_init_float");
      --  Properly initializes the FPU for PPC/MIPS systems.

   begin
      Self_ID.Common.LL.Thread := taskIdSelf;
      Result := taskVarAdd (0, Current_Task'Address);
      Current_Task := Self_ID;
      Init_Float;

      --  Install the signal handlers.
      --  This is called for each task since there is no signal inheritance
      --  between VxWorks tasks.

      Install_Signal_Handlers;

      Lock_RTS;

      for J in Known_Tasks'Range loop
         if Known_Tasks (J) = null then
            Known_Tasks (J) := Self_ID;
            Self_ID.Known_Tasks_Index := J;
            exit;
         end if;
      end loop;

      Unlock_RTS;
   end Enter_Task;

   --------------
   -- New_ATCB --
   --------------

   function New_ATCB (Entry_Num : Task_Entry_Index) return Task_ID is
   begin
      return new Ada_Task_Control_Block (Entry_Num);
   end New_ATCB;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean) is
   begin
      Self_ID.Common.LL.CV.Sem := semBCreate (SEM_Q_PRIORITY, SEM_EMPTY);
      Self_ID.Common.LL.CV.Waiting := 0;
      Self_ID.Common.LL.Thread := 0;

      if Self_ID.Common.LL.CV.Sem = 0 then
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
     (T          : Task_ID;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      use type System.Task_Info.Task_Image_Type;

      Adjusted_Stack_Size : size_t;

   begin
      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size := size_t (Default_Stack_Size);

      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size := size_t (Minimum_Stack_Size);

      else
         Adjusted_Stack_Size := size_t (Stack_Size);
      end if;

      --  Ask for 4 extra bytes of stack space so that the ATCB
      --  pointer can be stored below the stack limit, plus extra
      --  space for the frame of Task_Wrapper.  This is so the user
      --  gets the amount of stack requested exclusive of the needs
      --  of the runtime.
      --
      --  We also have to allocate n more bytes for the task name
      --  storage and enough space for the Wind Task Control Block
      --  which is around 0x778 bytes.  VxWorks also seems to carve out
      --  additional space, so use 2048 as a nice round number.
      --  We might want to increment to the nearest page size in
      --  case we ever support VxVMI.
      --
      --  XXX - we should come back and visit this so we can
      --        set the task name to something appropriate.
      Adjusted_Stack_Size := Adjusted_Stack_Size + 2048;

      --  Since the initial signal mask of a thread is inherited from the
      --  creator, and the Environment task has all its signals masked, we
      --  do not need to manipulate caller's signal mask at this point.
      --  All tasks in RTS will have All_Tasks_Mask initially.

      if T.Common.Task_Image = null then
         T.Common.LL.Thread := taskSpawn
           (System.Null_Address,
            To_VxWorks_Priority (int (Priority)),
            VX_FP_TASK,
            Adjusted_Stack_Size,
            Wrapper,
            To_Address (T));
      else
         declare
            Name : aliased String (1 .. T.Common.Task_Image'Length + 1);
         begin
            Name (1 .. Name'Last - 1) := T.Common.Task_Image.all;
            Name (Name'Last) := ASCII.NUL;

            T.Common.LL.Thread := taskSpawn
              (Name'Address,
               To_VxWorks_Priority (int (Priority)),
               VX_FP_TASK,
               Adjusted_Stack_Size,
               Wrapper,
               To_Address (T));
         end;
      end if;

      if T.Common.LL.Thread = -1 then
         Succeeded := False;
      else
         Succeeded := True;
      end if;

      Task_Creation_Hook (T.Common.LL.Thread);
      Set_Priority (T, Priority);
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
      Result : int;
      Tmp    : Task_ID := T;

      procedure Free is new
        Unchecked_Deallocation (Ada_Task_Control_Block, Task_ID);

   begin
      if Single_Lock then
         Result := semDelete (T.Common.LL.L.Mutex);
         pragma Assert (Result = 0);
      end if;

      T.Common.LL.Thread := 0;

      Result := semDelete (T.Common.LL.CV.Sem);
      pragma Assert (Result = 0);

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;

      Free (Tmp);
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      Task_Termination_Hook;
      taskDelete (0);
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_ID) is
      Result : int;
   begin
      Result := kill (T.Common.LL.Thread,
        Signal (Interrupt_Management.Abort_Task_Interrupt));
      pragma Assert (Result = 0);
   end Abort_Task;

   ----------------
   -- Check_Exit --
   ----------------

   --  Dummy versions. The only currently working version is for solaris
   --  (native).

   function Check_Exit (Self_ID : ST.Task_ID) return Boolean is
   begin
      return True;
   end Check_Exit;

   --------------------
   -- Check_No_Locks --
   --------------------

   function Check_No_Locks (Self_ID : ST.Task_ID) return Boolean is
   begin
      return True;
   end Check_No_Locks;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_ID is
   begin
      return Environment_Task_ID;
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
     (T           : ST.Task_ID;
      Thread_Self : Thread_Id) return Boolean is
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
     (T           : ST.Task_ID;
      Thread_Self : Thread_Id) return Boolean is
   begin
      if T.Common.LL.Thread /= 0
        and then T.Common.LL.Thread /= Thread_Self
      then
         return taskResume (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Resume_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_ID) is
   begin
      Environment_Task_ID := Environment_Task;

      --  Initialize the lock used to synchronize chain of all ATCBs.

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      Enter_Task (Environment_Task);
   end Initialize;

begin
   declare
      Result : int;
   begin
      if Locking_Policy = 'C' then
         Mutex_Protocol := Prio_Protect;
      elsif Locking_Policy = 'I' then
         Mutex_Protocol := Prio_Inherit;
      else
         Mutex_Protocol := Prio_None;
      end if;

      if Time_Slice_Val > 0 then
         Result := kernelTimeSlice
           (To_Clock_Ticks
             (Duration (Time_Slice_Val) / Duration (1_000_000.0)));
      end if;

      Result := sigemptyset (Unblocked_Signal_Mask'Access);
      pragma Assert (Result = 0);
   end;
end System.Task_Primitives.Operations;
