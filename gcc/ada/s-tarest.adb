------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K I N G . R E S T R I C T E D . S T A G E S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1999-2016, Free Software Foundation, Inc.          --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram alpha order check, since we group soft link
--  bodies and also separate off subprograms for restricted GNARLI.

--  This is a simplified version of the System.Tasking.Stages package,
--  intended to be used in a restricted run time.

--  This package represents the high level tasking interface used by the
--  compiler to expand Ada 95 tasking constructs into simpler run time calls.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Ada.Exceptions;

with System.Task_Primitives.Operations;
with System.Soft_Links.Tasking;
with System.Storage_Elements;

with System.Secondary_Stack;
pragma Elaborate_All (System.Secondary_Stack);
--  Make sure the body of Secondary_Stack is elaborated before calling
--  Init_Tasking_Soft_Links. See comments for this routine for explanation.

with System.Soft_Links;
--  Used for the non-tasking routines (*_NT) that refer to global data. They
--  are needed here before the tasking run time has been elaborated. used for
--  Create_TSD This package also provides initialization routines for task
--  specific data. The GNARL must call these to be sure that all non-tasking
--  Ada constructs will work.

package body System.Tasking.Restricted.Stages is

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;
   package SSE  renames System.Storage_Elements;
   package SST  renames System.Secondary_Stack;

   use Ada.Exceptions;

   use Parameters;
   use Task_Primitives.Operations;
   use Task_Info;

   Tasks_Activation_Chain : Task_Id;
   --  Chain of all the tasks to activate

   Global_Task_Lock : aliased System.Task_Primitives.RTS_Lock;
   --  This is a global lock; it is used to execute in mutual exclusion
   --  from all other tasks. It is only used by Task_Lock and Task_Unlock.

   -----------------------------------------------------------------
   -- Tasking versions of services needed by non-tasking programs --
   -----------------------------------------------------------------

   function Get_Current_Excep return SSL.EOA;
   --  Task-safe version of SSL.Get_Current_Excep

   procedure Task_Lock;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.

   procedure Task_Unlock;
   --  Releases lock previously set by call to Task_Lock. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Task_Wrapper (Self_ID : Task_Id);
   --  This is the procedure that is called by the GNULL from the
   --  new context when a task is created. It waits for activation
   --  and then calls the task body procedure. When the task body
   --  procedure completes, it terminates the task.

   procedure Terminate_Task (Self_ID : Task_Id);
   --  Terminate the calling task.
   --  This should only be called by the Task_Wrapper procedure.

   procedure Create_Restricted_Task
     (Priority             : Integer;
      Stack_Address        : System.Address;
      Size                 : System.Parameters.Size_Type;
      Secondary_Stack_Size : System.Parameters.Size_Type;
      Task_Info            : System.Task_Info.Task_Info_Type;
      CPU                  : Integer;
      State                : Task_Procedure_Access;
      Discriminants        : System.Address;
      Elaborated           : Access_Boolean;
      Task_Image           : String;
      Created_Task         : Task_Id);
   --  Code shared between Create_Restricted_Task (the concurrent version) and
   --  Create_Restricted_Task_Sequential. See comment of the former in the
   --  specification of this package.

   procedure Activate_Tasks (Chain : Task_Id);
   --  Activate the list of tasks started by Chain

   procedure Init_RTS;
   --  This procedure performs the initialization of the GNARL.
   --  It consists of initializing the environment task, global locks, and
   --  installing tasking versions of certain operations used by the compiler.
   --  Init_RTS is called during elaboration.

   -----------------------
   -- Get_Current_Excep --
   -----------------------

   function Get_Current_Excep return SSL.EOA is
   begin
      return STPO.Self.Common.Compiler_Data.Current_Excep'Access;
   end Get_Current_Excep;

   ---------------
   -- Task_Lock --
   ---------------

   procedure Task_Lock is
      Self_ID : constant Task_Id := STPO.Self;

   begin
      Self_ID.Common.Global_Task_Lock_Nesting :=
        Self_ID.Common.Global_Task_Lock_Nesting + 1;

      if Self_ID.Common.Global_Task_Lock_Nesting = 1 then
         STPO.Write_Lock (Global_Task_Lock'Access, Global_Lock => True);
      end if;
   end Task_Lock;

   -----------------
   -- Task_Unlock --
   -----------------

   procedure Task_Unlock is
      Self_ID : constant Task_Id := STPO.Self;

   begin
      pragma Assert (Self_ID.Common.Global_Task_Lock_Nesting > 0);
      Self_ID.Common.Global_Task_Lock_Nesting :=
        Self_ID.Common.Global_Task_Lock_Nesting - 1;

      if Self_ID.Common.Global_Task_Lock_Nesting = 0 then
         STPO.Unlock (Global_Task_Lock'Access, Global_Lock => True);
      end if;
   end Task_Unlock;

   ------------------
   -- Task_Wrapper --
   ------------------

   --  The task wrapper is a procedure that is called first for each task
   --  task body, and which in turn calls the compiler-generated task body
   --  procedure. The wrapper's main job is to do initialization for the task.

   --  The variable ID in the task wrapper is used to implement the Self
   --  function on targets where there is a fast way to find the stack base
   --  of the current thread, since it should be at a fixed offset from the
   --  stack base.

   procedure Task_Wrapper (Self_ID : Task_Id) is
      ID : Task_Id := Self_ID;
      pragma Volatile (ID);
      pragma Warnings (Off, ID);
      --  Variable used on some targets to implement a fast self. We turn off
      --  warnings because a stand alone volatile constant has to be imported,
      --  so we don't want warnings about ID not being referenced, and volatile
      --  having no effect.
      --
      --  DO NOT delete ID. As noted, it is needed on some targets.

      function Secondary_Stack_Size return Storage_Elements.Storage_Offset;
      --  Returns the size of the secondary stack for the task. For fixed
      --  secondary stacks, the function will return the ATCB field
      --  Secondary_Stack_Size if it is not set to Unspecified_Size,
      --  otherwise a percentage of the stack is reserved using the
      --  System.Parameters.Sec_Stack_Percentage property.

      --  Dynamic secondary stacks are allocated in System.Soft_Links.
      --  Create_TSD and thus the function returns 0 to suppress the
      --  creation of the fixed secondary stack in the primary stack.

      --------------------------
      -- Secondary_Stack_Size --
      --------------------------

      function Secondary_Stack_Size return Storage_Elements.Storage_Offset is
         use System.Storage_Elements;
         use System.Secondary_Stack;

      begin
         if Parameters.Sec_Stack_Dynamic then
            return 0;

         elsif Self_ID.Common.Secondary_Stack_Size = Unspecified_Size then
            return (Self_ID.Common.Compiler_Data.Pri_Stack_Info.Size
                       * SSE.Storage_Offset (Sec_Stack_Percentage) / 100);
         else
            --  Use the size specified by aspect Secondary_Stack_Size padded
            --  by the amount of space used by the stack data structure.

            return Storage_Offset (Self_ID.Common.Secondary_Stack_Size) +
                     Storage_Offset (Minimum_Secondary_Stack_Size);
         end if;
      end Secondary_Stack_Size;

      Secondary_Stack : aliased Storage_Elements.Storage_Array
                          (1 .. Secondary_Stack_Size);
      for Secondary_Stack'Alignment use Standard'Maximum_Alignment;
      --  This is the secondary stack data. Note that it is critical that this
      --  have maximum alignment, since any kind of data can be allocated here.

      pragma Warnings (Off);
      Secondary_Stack_Address : System.Address := Secondary_Stack'Address;
      pragma Warnings (On);
      --  Address of secondary stack. In the fixed secondary stack case, this
      --  value is not modified, causing a warning, hence the bracketing with
      --  Warnings (Off/On).

      Cause : Cause_Of_Termination := Normal;
      --  Indicates the reason why this task terminates. Normal corresponds to
      --  a task terminating due to completing the last statement of its body.
      --  If the task terminates because of an exception raised by the
      --  execution of its task body, then Cause is set to Unhandled_Exception.
      --  Aborts are not allowed in the restricted profile to which this file
      --  belongs.

      EO : Exception_Occurrence;
      --  If the task terminates because of an exception raised by the
      --  execution of its task body, then EO will contain the associated
      --  exception occurrence. Otherwise, it will contain Null_Occurrence.

   --  Start of processing for Task_Wrapper

   begin
      if not Parameters.Sec_Stack_Dynamic then
         Self_ID.Common.Compiler_Data.Sec_Stack_Addr :=
           Secondary_Stack'Address;
         SST.SS_Init (Secondary_Stack_Address, Integer (Secondary_Stack'Last));
      end if;

      --  Initialize low-level TCB components, that cannot be initialized by
      --  the creator.

      Enter_Task (Self_ID);

      --  Call the task body procedure

      begin
         --  We are separating the following portion of the code in order to
         --  place the exception handlers in a different block. In this way we
         --  do not call Set_Jmpbuf_Address (which needs Self) before we set
         --  Self in Enter_Task.

         --  Note that in the case of Ravenscar HI-E where there are no
         --  exception handlers, the exception handler is suppressed.

         --  Call the task body procedure

         Self_ID.Common.Task_Entry_Point (Self_ID.Common.Task_Arg);

         --  Normal task termination

         Cause := Normal;
         Save_Occurrence (EO, Ada.Exceptions.Null_Occurrence);

      exception
         when E : others =>

            --  Task terminating because of an unhandled exception

            Cause := Unhandled_Exception;
            Save_Occurrence (EO, E);
      end;

      --  Look for a fall-back handler

      --  This package is part of the restricted run time which supports
      --  neither task hierarchies (No_Task_Hierarchy) nor specific task
      --  termination handlers (No_Specific_Termination_Handlers).

      --  As specified in ARM C.7.3 par. 9/2, "the fall-back handler applies
      --  only to the dependent tasks of the task". Hence, if the terminating
      --  tasks (Self_ID) had a fall-back handler, it would not apply to
      --  itself. This code is always executed by a task whose master is the
      --  environment task (the task termination code for the environment task
      --  is executed by SSL.Task_Termination_Handler), so the fall-back
      --  handler to execute for this task can only be defined by its parent
      --  (there is no grandparent).

      declare
         TH : Termination_Handler := null;

      begin
         if Single_Lock then
            Lock_RTS;
         end if;

         Write_Lock (Self_ID.Common.Parent);

         TH := Self_ID.Common.Parent.Common.Fall_Back_Handler;

         Unlock (Self_ID.Common.Parent);

         if Single_Lock then
            Unlock_RTS;
         end if;

         --  Execute the task termination handler if we found it

         if TH /= null then
            TH.all (Cause, Self_ID, EO);
         end if;
      end;

      Terminate_Task (Self_ID);
   end Task_Wrapper;

   -----------------------
   -- Restricted GNARLI --
   -----------------------

   -----------------------------------
   -- Activate_All_Tasks_Sequential --
   -----------------------------------

   procedure Activate_All_Tasks_Sequential is
   begin
      pragma Assert (Partition_Elaboration_Policy = 'S');

      Activate_Tasks (Tasks_Activation_Chain);
      Tasks_Activation_Chain := Null_Task;
   end Activate_All_Tasks_Sequential;

   -------------------------------
   -- Activate_Restricted_Tasks --
   -------------------------------

   procedure Activate_Restricted_Tasks
     (Chain_Access : Activation_Chain_Access) is
   begin
      if Partition_Elaboration_Policy = 'S' then

         --  In sequential elaboration policy, the chain must be empty. This
         --  procedure can be called if the unit has been compiled without
         --  partition elaboration policy, but the partition has a sequential
         --  elaboration policy.

         pragma Assert (Chain_Access.T_ID = Null_Task);
         null;
      else
         Activate_Tasks (Chain_Access.T_ID);
         Chain_Access.T_ID := Null_Task;
      end if;
   end Activate_Restricted_Tasks;

   --------------------
   -- Activate_Tasks --
   --------------------

   --  Note that locks of activator and activated task are both locked here.
   --  This is necessary because C.State and Self.Wait_Count have to be
   --  synchronized. This is safe from deadlock because the activator is always
   --  created before the activated task. That satisfies our
   --  in-order-of-creation ATCB locking policy.

   procedure Activate_Tasks (Chain : Task_Id) is
      Self_ID       : constant Task_Id := STPO.Self;
      C             : Task_Id;
      Activate_Prio : System.Any_Priority;
      Success       : Boolean;

   begin
      pragma Assert (Self_ID = Environment_Task);
      pragma Assert (Self_ID.Common.Wait_Count = 0);

      if Single_Lock then
         Lock_RTS;
      end if;

      --  Lock self, to prevent activated tasks from racing ahead before we
      --  finish activating the chain.

      Write_Lock (Self_ID);

      --  Activate all the tasks in the chain. Creation of the thread of
      --  control was deferred until activation. So create it now.

      C := Chain;
      while C /= null loop
         if C.Common.State /= Terminated then
            pragma Assert (C.Common.State = Unactivated);

            Write_Lock (C);

            Activate_Prio :=
              (if C.Common.Base_Priority < Get_Priority (Self_ID)
               then Get_Priority (Self_ID)
               else C.Common.Base_Priority);

            STPO.Create_Task
              (C, Task_Wrapper'Address,
               Parameters.Size_Type
                 (C.Common.Compiler_Data.Pri_Stack_Info.Size),
               Activate_Prio, Success);

            Self_ID.Common.Wait_Count := Self_ID.Common.Wait_Count + 1;

            if Success then
               C.Common.State := Runnable;
            else
               raise Program_Error;
            end if;

            Unlock (C);
         end if;

         C := C.Common.Activation_Link;
      end loop;

      Self_ID.Common.State := Activator_Sleep;

      --  Wait for the activated tasks to complete activation. It is unsafe to
      --  abort any of these tasks until the count goes to zero.

      loop
         exit when Self_ID.Common.Wait_Count = 0;
         Sleep (Self_ID, Activator_Sleep);
      end loop;

      Self_ID.Common.State := Runnable;
      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;
   end Activate_Tasks;

   ------------------------------------
   -- Complete_Restricted_Activation --
   ------------------------------------

   --  As in several other places, the locks of the activator and activated
   --  task are both locked here. This follows our deadlock prevention lock
   --  ordering policy, since the activated task must be created after the
   --  activator.

   procedure Complete_Restricted_Activation is
      Self_ID   : constant Task_Id := STPO.Self;
      Activator : constant Task_Id := Self_ID.Common.Activator;

   begin
      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Activator);
      Write_Lock (Self_ID);

      --  Remove dangling reference to Activator, since a task may outlive its
      --  activator.

      Self_ID.Common.Activator := null;

      --  Wake up the activator, if it is waiting for a chain of tasks to
      --  activate, and we are the last in the chain to complete activation

      if Activator.Common.State = Activator_Sleep then
         Activator.Common.Wait_Count := Activator.Common.Wait_Count - 1;

         if Activator.Common.Wait_Count = 0 then
            Wakeup (Activator, Activator_Sleep);
         end if;
      end if;

      Unlock (Self_ID);
      Unlock (Activator);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  After the activation, active priority should be the same as base
      --  priority. We must unlock the Activator first, though, since it should
      --  not wait if we have lower priority.

      if Get_Priority (Self_ID) /= Self_ID.Common.Base_Priority then
         Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
      end if;
   end Complete_Restricted_Activation;

   ------------------------------
   -- Complete_Restricted_Task --
   ------------------------------

   procedure Complete_Restricted_Task is
   begin
      STPO.Self.Common.State := Terminated;
   end Complete_Restricted_Task;

   ----------------------------
   -- Create_Restricted_Task --
   ----------------------------

   procedure Create_Restricted_Task
     (Priority             : Integer;
      Stack_Address        : System.Address;
      Size                 : System.Parameters.Size_Type;
      Secondary_Stack_Size : System.Parameters.Size_Type;
      Task_Info            : System.Task_Info.Task_Info_Type;
      CPU                  : Integer;
      State                : Task_Procedure_Access;
      Discriminants        : System.Address;
      Elaborated           : Access_Boolean;
      Task_Image           : String;
      Created_Task         : Task_Id)
   is
      Self_ID       : constant Task_Id := STPO.Self;
      Base_Priority : System.Any_Priority;
      Base_CPU      : System.Multiprocessors.CPU_Range;
      Success       : Boolean;
      Len           : Integer;

   begin
      --  Stack is not preallocated on this target, so that Stack_Address must
      --  be null.

      pragma Assert (Stack_Address = Null_Address);

      Base_Priority :=
        (if Priority = Unspecified_Priority
         then Self_ID.Common.Base_Priority
         else System.Any_Priority (Priority));

      --  Legal values of CPU are the special Unspecified_CPU value which is
      --  inserted by the compiler for tasks without CPU aspect, and those in
      --  the range of CPU_Range but no greater than Number_Of_CPUs. Otherwise
      --  the task is defined to have failed, and it becomes a completed task
      --  (RM D.16(14/3)).

      if CPU /= Unspecified_CPU
        and then (CPU < Integer (System.Multiprocessors.CPU_Range'First)
          or else CPU > Integer (System.Multiprocessors.Number_Of_CPUs))
      then
         raise Tasking_Error with "CPU not in range";

      --  Normal CPU affinity
      else
         --  When the application code says nothing about the task affinity
         --  (task without CPU aspect) then the compiler inserts the
         --  Unspecified_CPU value which indicates to the run-time library that
         --  the task will activate and execute on the same processor as its
         --  activating task if the activating task is assigned a processor
         --  (RM D.16(14/3)).

         Base_CPU :=
           (if CPU = Unspecified_CPU
            then Self_ID.Common.Base_CPU
            else System.Multiprocessors.CPU_Range (CPU));
      end if;

      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Self_ID);

      --  With no task hierarchy, the parent of all non-Environment tasks that
      --  are created must be the Environment task. Dispatching domains are
      --  not allowed in Ravenscar, so the dispatching domain parameter will
      --  always be null.

      Initialize_ATCB
        (Self_ID, State, Discriminants, Self_ID, Elaborated, Base_Priority,
         Base_CPU, null, Task_Info, Size, Secondary_Stack_Size,
         Created_Task, Success);

      --  If we do our job right then there should never be any failures, which
      --  was probably said about the Titanic; so just to be safe, let's retain
      --  this code for now

      if not Success then
         Unlock (Self_ID);

         if Single_Lock then
            Unlock_RTS;
         end if;

         raise Program_Error;
      end if;

      Created_Task.Entry_Calls (1).Self := Created_Task;

      Len :=
        Integer'Min (Created_Task.Common.Task_Image'Length, Task_Image'Length);
      Created_Task.Common.Task_Image_Len := Len;
      Created_Task.Common.Task_Image (1 .. Len) :=
        Task_Image (Task_Image'First .. Task_Image'First + Len - 1);

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Create TSD as early as possible in the creation of a task, since it
      --  may be used by the operation of Ada code within the task.

      SSL.Create_TSD (Created_Task.Common.Compiler_Data);
   end Create_Restricted_Task;

   procedure Create_Restricted_Task
     (Priority             : Integer;
      Stack_Address        : System.Address;
      Size                 : System.Parameters.Size_Type;
      Secondary_Stack_Size : System.Parameters.Size_Type;
      Task_Info            : System.Task_Info.Task_Info_Type;
      CPU                  : Integer;
      State                : Task_Procedure_Access;
      Discriminants        : System.Address;
      Elaborated           : Access_Boolean;
      Chain                : in out Activation_Chain;
      Task_Image           : String;
      Created_Task         : Task_Id)
   is
   begin
      if Partition_Elaboration_Policy = 'S' then

         --  A unit may have been compiled without partition elaboration
         --  policy, and in this case the compiler will emit calls for the
         --  default policy (concurrent). But if the partition policy is
         --  sequential, activation must be deferred.

         Create_Restricted_Task_Sequential
           (Priority, Stack_Address, Size, Secondary_Stack_Size,
            Task_Info, CPU, State, Discriminants, Elaborated,
            Task_Image, Created_Task);

      else
         Create_Restricted_Task
           (Priority, Stack_Address, Size, Secondary_Stack_Size,
            Task_Info, CPU, State, Discriminants, Elaborated,
            Task_Image, Created_Task);

         --  Append this task to the activation chain

         Created_Task.Common.Activation_Link := Chain.T_ID;
         Chain.T_ID := Created_Task;
      end if;
   end Create_Restricted_Task;

   ---------------------------------------
   -- Create_Restricted_Task_Sequential --
   ---------------------------------------

   procedure Create_Restricted_Task_Sequential
     (Priority             : Integer;
      Stack_Address        : System.Address;
      Size                 : System.Parameters.Size_Type;
      Secondary_Stack_Size : System.Parameters.Size_Type;
      Task_Info            : System.Task_Info.Task_Info_Type;
      CPU                  : Integer;
      State                : Task_Procedure_Access;
      Discriminants        : System.Address;
      Elaborated           : Access_Boolean;
      Task_Image           : String;
      Created_Task         : Task_Id) is
   begin
      Create_Restricted_Task (Priority, Stack_Address, Size,
                              Secondary_Stack_Size, Task_Info,
                              CPU, State, Discriminants, Elaborated,
                              Task_Image, Created_Task);

      --  Append this task to the activation chain

      Created_Task.Common.Activation_Link := Tasks_Activation_Chain;
      Tasks_Activation_Chain := Created_Task;
   end Create_Restricted_Task_Sequential;

   ---------------------------
   -- Finalize_Global_Tasks --
   ---------------------------

   --  This is needed to support the compiler interface; it will only be called
   --  by the Environment task. Instead, it will cause the Environment to block
   --  forever, since none of the dependent tasks are expected to terminate

   procedure Finalize_Global_Tasks is
      Self_ID : constant Task_Id := STPO.Self;

   begin
      pragma Assert (Self_ID = STPO.Environment_Task);

      if Single_Lock then
         Lock_RTS;
      end if;

      --  Handle normal task termination by the environment task, but only for
      --  the normal task termination. In the case of Abnormal and
      --  Unhandled_Exception they must have been handled before, and the task
      --  termination soft link must have been changed so the task termination
      --  routine is not executed twice.

      --  Note that in the "normal" implementation in s-tassta.adb the task
      --  termination procedure for the environment task should be executed
      --  after termination of library-level tasks. However, this
      --  implementation is to be used when the Ravenscar restrictions are in
      --  effect, and AI-394 says that if there is a fall-back handler set for
      --  the partition it should be called when the first task (including the
      --  environment task) attempts to terminate.

      SSL.Task_Termination_Handler.all (Ada.Exceptions.Null_Occurrence);

      Write_Lock (Self_ID);
      Sleep (Self_ID, Master_Completion_Sleep);
      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Should never return from Master Completion Sleep

      raise Program_Error;
   end Finalize_Global_Tasks;

   ---------------------------
   -- Restricted_Terminated --
   ---------------------------

   function Restricted_Terminated (T : Task_Id) return Boolean is
   begin
      return T.Common.State = Terminated;
   end Restricted_Terminated;

   --------------------
   -- Terminate_Task --
   --------------------

   procedure Terminate_Task (Self_ID : Task_Id) is
   begin
      Self_ID.Common.State := Terminated;
   end Terminate_Task;

   --------------
   -- Init_RTS --
   --------------

   procedure Init_RTS is
   begin
      Tasking.Initialize;

      --  Initialize lock used to implement mutual exclusion between all tasks

      STPO.Initialize_Lock (Global_Task_Lock'Access, STPO.Global_Task_Level);

      --  Notify that the tasking run time has been elaborated so that
      --  the tasking version of the soft links can be used.

      SSL.Lock_Task         := Task_Lock'Access;
      SSL.Unlock_Task       := Task_Unlock'Access;
      SSL.Adafinal          := Finalize_Global_Tasks'Access;
      SSL.Get_Current_Excep := Get_Current_Excep'Access;

      --  Initialize the tasking soft links (if not done yet) that are common
      --  to the full and the restricted run times.

      SSL.Tasking.Init_Tasking_Soft_Links;
   end Init_RTS;

begin
   Init_RTS;
end System.Tasking.Restricted.Stages;
