------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                 S Y S T E M . T A S K I N G . S T A G E S                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2013, Free Software Foundation, Inc.          --
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

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

pragma Partition_Elaboration_Policy (Concurrent);
--  This package only implements the concurrent elaboration policy. This pragma
--  will enforce it (and detect conflicts with user specified policy).

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with System.Interrupt_Management;
with System.Tasking.Debug;
with System.Address_Image;
with System.Task_Primitives;
with System.Task_Primitives.Operations;
with System.Tasking.Utilities;
with System.Tasking.Queuing;
with System.Tasking.Rendezvous;
with System.OS_Primitives;
with System.Secondary_Stack;
with System.Storage_Elements;
with System.Restrictions;
with System.Standard_Library;
with System.Traces.Tasking;
with System.Stack_Usage;

with System.Soft_Links;
--  These are procedure pointers to non-tasking routines that use task
--  specific data. In the absence of tasking, these routines refer to global
--  data. In the presence of tasking, they must be replaced with pointers to
--  task-specific versions. Also used for Create_TSD, Destroy_TSD, Get_Current
--  _Excep, Finalize_Library_Objects, Task_Termination, Handler.

with System.Tasking.Initialization;
pragma Elaborate_All (System.Tasking.Initialization);
--  This insures that tasking is initialized if any tasks are created

package body System.Tasking.Stages is

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;
   package SSE  renames System.Storage_Elements;
   package SST  renames System.Secondary_Stack;

   use Ada.Exceptions;

   use Parameters;
   use Task_Primitives;
   use Task_Primitives.Operations;
   use Task_Info;

   use System.Traces;
   use System.Traces.Tasking;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Free is new
     Ada.Unchecked_Deallocation (Ada_Task_Control_Block, Task_Id);

   procedure Trace_Unhandled_Exception_In_Task (Self_Id : Task_Id);
   --  This procedure outputs the task specific message for exception
   --  tracing purposes.

   procedure Task_Wrapper (Self_ID : Task_Id);
   pragma Convention (C, Task_Wrapper);
   --  This is the procedure that is called by the GNULL from the new context
   --  when a task is created. It waits for activation and then calls the task
   --  body procedure. When the task body procedure completes, it terminates
   --  the task.
   --
   --  The Task_Wrapper's address will be provided to the underlying threads
   --  library as the task entry point. Convention C is what makes most sense
   --  for that purpose (Export C would make the function globally visible,
   --  and affect the link name on which GDB depends). This will in addition
   --  trigger an automatic stack alignment suitable for GCC's assumptions if
   --  need be.

   --  "Vulnerable_..." in the procedure names below means they must be called
   --  with abort deferred.

   procedure Vulnerable_Complete_Task (Self_ID : Task_Id);
   --  Complete the calling task. This procedure must be called with
   --  abort deferred. It should only be called by Complete_Task and
   --  Finalize_Global_Tasks (for the environment task).

   procedure Vulnerable_Complete_Master (Self_ID : Task_Id);
   --  Complete the current master of the calling task. This procedure
   --  must be called with abort deferred. It should only be called by
   --  Vulnerable_Complete_Task and Complete_Master.

   procedure Vulnerable_Complete_Activation (Self_ID : Task_Id);
   --  Signal to Self_ID's activator that Self_ID has completed activation.
   --  This procedure must be called with abort deferred.

   procedure Abort_Dependents (Self_ID : Task_Id);
   --  Abort all the direct dependents of Self at its current master nesting
   --  level, plus all of their dependents, transitively. RTS_Lock should be
   --  locked by the caller.

   procedure Vulnerable_Free_Task (T : Task_Id);
   --  Recover all runtime system storage associated with the task T. This
   --  should only be called after T has terminated and will no longer be
   --  referenced.
   --
   --  For tasks created by an allocator that fails, due to an exception, it is
   --  called from Expunge_Unactivated_Tasks.
   --
   --  Different code is used at master completion, in Terminate_Dependents,
   --  due to a need for tighter synchronization with the master.

   ----------------------
   -- Abort_Dependents --
   ----------------------

   procedure Abort_Dependents (Self_ID : Task_Id) is
      C : Task_Id;
      P : Task_Id;

   begin
      C := All_Tasks_List;
      while C /= null loop
         P := C.Common.Parent;
         while P /= null loop
            if P = Self_ID then

               --  ??? C is supposed to take care of its own dependents, so
               --  there should be no need to worry about them. Need to double
               --  check this.

               if C.Master_of_Task = Self_ID.Master_Within then
                  Utilities.Abort_One_Task (Self_ID, C);
                  C.Dependents_Aborted := True;
               end if;

               exit;
            end if;

            P := P.Common.Parent;
         end loop;

         C := C.Common.All_Tasks_Link;
      end loop;

      Self_ID.Dependents_Aborted := True;
   end Abort_Dependents;

   -----------------
   -- Abort_Tasks --
   -----------------

   procedure Abort_Tasks (Tasks : Task_List) is
   begin
      Utilities.Abort_Tasks (Tasks);
   end Abort_Tasks;

   --------------------
   -- Activate_Tasks --
   --------------------

   --  Note that locks of activator and activated task are both locked here.
   --  This is necessary because C.Common.State and Self.Common.Wait_Count have
   --  to be synchronized. This is safe from deadlock because the activator is
   --  always created before the activated task. That satisfies our
   --  in-order-of-creation ATCB locking policy.

   --  At one point, we may also lock the parent, if the parent is different
   --  from the activator. That is also consistent with the lock ordering
   --  policy, since the activator cannot be created before the parent.

   --  Since we are holding both the activator's lock, and Task_Wrapper locks
   --  that before it does anything more than initialize the low-level ATCB
   --  components, it should be safe to wait to update the counts until we see
   --  that the thread creation is successful.

   --  If the thread creation fails, we do need to close the entries of the
   --  task. The first phase, of dequeuing calls, only requires locking the
   --  acceptor's ATCB, but the waking up of the callers requires locking the
   --  caller's ATCB. We cannot safely do this while we are holding other
   --  locks. Therefore, the queue-clearing operation is done in a separate
   --  pass over the activation chain.

   procedure Activate_Tasks (Chain_Access : Activation_Chain_Access) is
      Self_ID        : constant Task_Id := STPO.Self;
      P              : Task_Id;
      C              : Task_Id;
      Next_C, Last_C : Task_Id;
      Activate_Prio  : System.Any_Priority;
      Success        : Boolean;
      All_Elaborated : Boolean := True;

   begin
      --  If pragma Detect_Blocking is active, then we must check whether this
      --  potentially blocking operation is called from a protected action.

      if System.Tasking.Detect_Blocking
        and then Self_ID.Common.Protected_Action_Nesting > 0
      then
         raise Program_Error with "potentially blocking operation";
      end if;

      pragma Debug
        (Debug.Trace (Self_ID, "Activate_Tasks", 'C'));

      Initialization.Defer_Abort_Nestable (Self_ID);

      pragma Assert (Self_ID.Common.Wait_Count = 0);

      --  Lock RTS_Lock, to prevent activated tasks from racing ahead before
      --  we finish activating the chain.

      Lock_RTS;

      --  Check that all task bodies have been elaborated

      C := Chain_Access.T_ID;
      Last_C := null;
      while C /= null loop
         if C.Common.Elaborated /= null
           and then not C.Common.Elaborated.all
         then
            All_Elaborated := False;
         end if;

         --  Reverse the activation chain so that tasks are activated in the
         --  same order they're declared.

         Next_C := C.Common.Activation_Link;
         C.Common.Activation_Link := Last_C;
         Last_C := C;
         C := Next_C;
      end loop;

      Chain_Access.T_ID := Last_C;

      if not All_Elaborated then
         Unlock_RTS;
         Initialization.Undefer_Abort_Nestable (Self_ID);
         raise Program_Error with "Some tasks have not been elaborated";
      end if;

      --  Activate all the tasks in the chain. Creation of the thread of
      --  control was deferred until activation. So create it now.

      C := Chain_Access.T_ID;
      while C /= null loop
         if C.Common.State /= Terminated then
            pragma Assert (C.Common.State = Unactivated);

            P := C.Common.Parent;
            Write_Lock (P);
            Write_Lock (C);

            Activate_Prio :=
              (if C.Common.Base_Priority < Get_Priority (Self_ID)
               then Get_Priority (Self_ID)
               else C.Common.Base_Priority);

            System.Task_Primitives.Operations.Create_Task
              (C, Task_Wrapper'Address,
               Parameters.Size_Type
                 (C.Common.Compiler_Data.Pri_Stack_Info.Size),
               Activate_Prio, Success);

            --  There would be a race between the created task and the creator
            --  to do the following initialization, if we did not have a
            --  Lock/Unlock_RTS pair in the task wrapper to prevent it from
            --  racing ahead.

            if Success then
               C.Common.State := Activating;
               C.Awake_Count := 1;
               C.Alive_Count := 1;
               P.Awake_Count := P.Awake_Count + 1;
               P.Alive_Count := P.Alive_Count + 1;

               if P.Common.State = Master_Completion_Sleep and then
                 C.Master_of_Task = P.Master_Within
               then
                  pragma Assert (Self_ID /= P);
                  P.Common.Wait_Count := P.Common.Wait_Count + 1;
               end if;

               for J in System.Tasking.Debug.Known_Tasks'Range loop
                  if System.Tasking.Debug.Known_Tasks (J) = null then
                     System.Tasking.Debug.Known_Tasks (J) := C;
                     C.Known_Tasks_Index := J;
                     exit;
                  end if;
               end loop;

               if Global_Task_Debug_Event_Set then
                  Debug.Signal_Debug_Event
                   (Debug.Debug_Event_Activating, C);
               end if;

               C.Common.State := Runnable;

               Unlock (C);
               Unlock (P);

            else
               --  No need to set Awake_Count, State, etc. here since the loop
               --  below will do that for any Unactivated tasks.

               Unlock (C);
               Unlock (P);
               Self_ID.Common.Activation_Failed := True;
            end if;
         end if;

         C := C.Common.Activation_Link;
      end loop;

      if not Single_Lock then
         Unlock_RTS;
      end if;

      --  Close the entries of any tasks that failed thread creation, and count
      --  those that have not finished activation.

      Write_Lock (Self_ID);
      Self_ID.Common.State := Activator_Sleep;

      C := Chain_Access.T_ID;
      while C /= null loop
         Write_Lock (C);

         if C.Common.State = Unactivated then
            C.Common.Activator := null;
            C.Common.State := Terminated;
            C.Callable := False;
            Utilities.Cancel_Queued_Entry_Calls (C);

         elsif C.Common.Activator /= null then
            Self_ID.Common.Wait_Count := Self_ID.Common.Wait_Count + 1;
         end if;

         Unlock (C);
         P := C.Common.Activation_Link;
         C.Common.Activation_Link := null;
         C := P;
      end loop;

      --  Wait for the activated tasks to complete activation. It is
      --  unsafe to abort any of these tasks until the count goes to zero.

      loop
         exit when Self_ID.Common.Wait_Count = 0;
         Sleep (Self_ID, Activator_Sleep);
      end loop;

      Self_ID.Common.State := Runnable;
      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Remove the tasks from the chain

      Chain_Access.T_ID := null;
      Initialization.Undefer_Abort_Nestable (Self_ID);

      if Self_ID.Common.Activation_Failed then
         Self_ID.Common.Activation_Failed := False;
         raise Tasking_Error with "Failure during activation";
      end if;
   end Activate_Tasks;

   -------------------------
   -- Complete_Activation --
   -------------------------

   procedure Complete_Activation is
      Self_ID : constant Task_Id := STPO.Self;

   begin
      Initialization.Defer_Abort_Nestable (Self_ID);

      if Single_Lock then
         Lock_RTS;
      end if;

      Vulnerable_Complete_Activation (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Initialization.Undefer_Abort_Nestable (Self_ID);

      --  ??? Why do we need to allow for nested deferral here?

      if Runtime_Traces then
         Send_Trace_Info (T_Activate);
      end if;
   end Complete_Activation;

   ---------------------
   -- Complete_Master --
   ---------------------

   procedure Complete_Master is
      Self_ID : constant Task_Id := STPO.Self;
   begin
      pragma Assert
        (Self_ID.Deferral_Level > 0
          or else not System.Restrictions.Abort_Allowed);
      Vulnerable_Complete_Master (Self_ID);
   end Complete_Master;

   -------------------
   -- Complete_Task --
   -------------------

   --  See comments on Vulnerable_Complete_Task for details

   procedure Complete_Task is
      Self_ID  : constant Task_Id := STPO.Self;

   begin
      pragma Assert
        (Self_ID.Deferral_Level > 0
          or else not System.Restrictions.Abort_Allowed);

      Vulnerable_Complete_Task (Self_ID);

      --  All of our dependents have terminated. Never undefer abort again!

   end Complete_Task;

   -----------------
   -- Create_Task --
   -----------------

   --  Compiler interface only. Do not call from within the RTS. This must be
   --  called to create a new task.

   procedure Create_Task
     (Priority          : Integer;
      Size              : System.Parameters.Size_Type;
      Task_Info         : System.Task_Info.Task_Info_Type;
      CPU               : Integer;
      Relative_Deadline : Ada.Real_Time.Time_Span;
      Domain            : Dispatching_Domain_Access;
      Num_Entries       : Task_Entry_Index;
      Master            : Master_Level;
      State             : Task_Procedure_Access;
      Discriminants     : System.Address;
      Elaborated        : Access_Boolean;
      Chain             : in out Activation_Chain;
      Task_Image        : String;
      Created_Task      : out Task_Id)
   is
      T, P          : Task_Id;
      Self_ID       : constant Task_Id := STPO.Self;
      Success       : Boolean;
      Base_Priority : System.Any_Priority;
      Len           : Natural;
      Base_CPU      : System.Multiprocessors.CPU_Range;

      use type System.Multiprocessors.CPU_Range;

      pragma Unreferenced (Relative_Deadline);
      --  EDF scheduling is not supported by any of the target platforms so
      --  this parameter is not passed any further.

   begin
      --  If Master is greater than the current master, it means that Master
      --  has already awaited its dependent tasks. This raises Program_Error,
      --  by 4.8(10.3/2). See AI-280. Ignore this check for foreign threads.

      if Self_ID.Master_of_Task /= Foreign_Task_Level
        and then Master > Self_ID.Master_Within
      then
         raise Program_Error with
           "create task after awaiting termination";
      end if;

      --  If pragma Detect_Blocking is active must be checked whether this
      --  potentially blocking operation is called from a protected action.

      if System.Tasking.Detect_Blocking
        and then Self_ID.Common.Protected_Action_Nesting > 0
      then
         raise Program_Error with "potentially blocking operation";
      end if;

      pragma Debug (Debug.Trace (Self_ID, "Create_Task", 'C'));

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
                    or else
                  CPU > Integer (System.Multiprocessors.CPU_Range'Last)
                    or else
                  CPU > Integer (System.Multiprocessors.Number_Of_CPUs))
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

      --  Find parent P of new Task, via master level number

      P := Self_ID;

      if P /= null then
         while P.Master_of_Task >= Master loop
            P := P.Common.Parent;
            exit when P = null;
         end loop;
      end if;

      Initialization.Defer_Abort_Nestable (Self_ID);

      begin
         T := New_ATCB (Num_Entries);
      exception
         when others =>
            Initialization.Undefer_Abort_Nestable (Self_ID);
            raise Storage_Error with "Cannot allocate task";
      end;

      --  RTS_Lock is used by Abort_Dependents and Abort_Tasks. Up to this
      --  point, it is possible that we may be part of a family of tasks that
      --  is being aborted.

      Lock_RTS;
      Write_Lock (Self_ID);

      --  Now, we must check that we have not been aborted. If so, we should
      --  give up on creating this task, and simply return.

      if not Self_ID.Callable then
         pragma Assert (Self_ID.Pending_ATC_Level = 0);
         pragma Assert (Self_ID.Pending_Action);
         pragma Assert
           (Chain.T_ID = null or else Chain.T_ID.Common.State = Unactivated);

         Unlock (Self_ID);
         Unlock_RTS;
         Initialization.Undefer_Abort_Nestable (Self_ID);

         --  ??? Should never get here

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      Initialize_ATCB (Self_ID, State, Discriminants, P, Elaborated,
        Base_Priority, Base_CPU, Domain, Task_Info, Size, T, Success);

      if not Success then
         Free (T);
         Unlock (Self_ID);
         Unlock_RTS;
         Initialization.Undefer_Abort_Nestable (Self_ID);
         raise Storage_Error with "Failed to initialize task";
      end if;

      if Master = Foreign_Task_Level + 2 then

         --  This should not happen, except when a foreign task creates non
         --  library-level Ada tasks. In this case, we pretend the master is
         --  a regular library level task, otherwise the run-time will get
         --  confused when waiting for these tasks to terminate.

         T.Master_of_Task := Library_Task_Level;

      else
         T.Master_of_Task := Master;
      end if;

      T.Master_Within := T.Master_of_Task + 1;

      for L in T.Entry_Calls'Range loop
         T.Entry_Calls (L).Self := T;
         T.Entry_Calls (L).Level := L;
      end loop;

      if Task_Image'Length = 0 then
         T.Common.Task_Image_Len := 0;
      else
         Len := 1;
         T.Common.Task_Image (1) := Task_Image (Task_Image'First);

         --  Remove unwanted blank space generated by 'Image

         for J in Task_Image'First + 1 .. Task_Image'Last loop
            if Task_Image (J) /= ' '
              or else Task_Image (J - 1) /= '('
            then
               Len := Len + 1;
               T.Common.Task_Image (Len) := Task_Image (J);
               exit when Len = T.Common.Task_Image'Last;
            end if;
         end loop;

         T.Common.Task_Image_Len := Len;
      end if;

      --  The task inherits the dispatching domain of the parent only if no
      --  specific domain has been defined in the spec of the task (using the
      --  dispatching domain pragma or aspect).

      if T.Common.Domain /= null then
         null;
      elsif T.Common.Activator /= null then
         T.Common.Domain := T.Common.Activator.Common.Domain;
      else
         T.Common.Domain := System.Tasking.System_Domain;
      end if;

      Unlock (Self_ID);
      Unlock_RTS;

      --  The CPU associated to the task (if any) must belong to the
      --  dispatching domain.

      if Base_CPU /= System.Multiprocessors.Not_A_Specific_CPU
        and then
          (Base_CPU not in T.Common.Domain'Range
            or else not T.Common.Domain (Base_CPU))
      then
         Initialization.Undefer_Abort_Nestable (Self_ID);
         raise Tasking_Error with "CPU not in dispatching domain";
      end if;

      --  To handle the interaction between pragma CPU and dispatching domains
      --  we need to signal that this task is being allocated to a processor.
      --  This is needed only for tasks belonging to the system domain (the
      --  creation of new dispatching domains can only take processors from the
      --  system domain) and only before the environment task calls the main
      --  procedure (dispatching domains cannot be created after this).

      if Base_CPU /= System.Multiprocessors.Not_A_Specific_CPU
        and then T.Common.Domain = System.Tasking.System_Domain
        and then not System.Tasking.Dispatching_Domains_Frozen
      then
         --  Increase the number of tasks attached to the CPU to which this
         --  task is being moved.

         Dispatching_Domain_Tasks (Base_CPU) :=
           Dispatching_Domain_Tasks (Base_CPU) + 1;
      end if;

      --  Create TSD as early as possible in the creation of a task, since it
      --  may be used by the operation of Ada code within the task.

      SSL.Create_TSD (T.Common.Compiler_Data);
      T.Common.Activation_Link := Chain.T_ID;
      Chain.T_ID := T;
      Initialization.Initialize_Attributes_Link.all (T);
      Created_Task := T;
      Initialization.Undefer_Abort_Nestable (Self_ID);

      if Runtime_Traces then
         Send_Trace_Info (T_Create, T);
      end if;
   end Create_Task;

   --------------------
   -- Current_Master --
   --------------------

   function Current_Master return Master_Level is
   begin
      return STPO.Self.Master_Within;
   end Current_Master;

   ------------------
   -- Enter_Master --
   ------------------

   procedure Enter_Master is
      Self_ID : constant Task_Id := STPO.Self;
   begin
      Self_ID.Master_Within := Self_ID.Master_Within + 1;
   end Enter_Master;

   -------------------------------
   -- Expunge_Unactivated_Tasks --
   -------------------------------

   --  See procedure Close_Entries for the general case

   procedure Expunge_Unactivated_Tasks (Chain : in out Activation_Chain) is
      Self_ID : constant Task_Id := STPO.Self;
      C       : Task_Id;
      Call    : Entry_Call_Link;
      Temp    : Task_Id;

   begin
      pragma Debug
        (Debug.Trace (Self_ID, "Expunge_Unactivated_Tasks", 'C'));

      Initialization.Defer_Abort_Nestable (Self_ID);

      --  ???
      --  Experimentation has shown that abort is sometimes (but not always)
      --  already deferred when this is called.

      --  That may indicate an error. Find out what is going on

      C := Chain.T_ID;
      while C /= null loop
         pragma Assert (C.Common.State = Unactivated);

         Temp := C.Common.Activation_Link;

         if C.Common.State = Unactivated then
            Lock_RTS;
            Write_Lock (C);

            for J in 1 .. C.Entry_Num loop
               Queuing.Dequeue_Head (C.Entry_Queues (J), Call);
               pragma Assert (Call = null);
            end loop;

            Unlock (C);

            Initialization.Remove_From_All_Tasks_List (C);
            Unlock_RTS;

            Vulnerable_Free_Task (C);
            C := Temp;
         end if;
      end loop;

      Chain.T_ID := null;
      Initialization.Undefer_Abort_Nestable (Self_ID);
   end Expunge_Unactivated_Tasks;

   ---------------------------
   -- Finalize_Global_Tasks --
   ---------------------------

   --  ???
   --  We have a potential problem here if finalization of global objects does
   --  anything with signals or the timer server, since by that time those
   --  servers have terminated.

   --  It is hard to see how that would occur

   --  However, a better solution might be to do all this finalization
   --  using the global finalization chain.

   procedure Finalize_Global_Tasks is
      Self_ID : constant Task_Id := STPO.Self;

      Ignore_1 : Boolean;
      Ignore_2 : Boolean;
      pragma Unreferenced (Ignore_1, Ignore_2);

      function State
        (Int : System.Interrupt_Management.Interrupt_ID) return Character;
      pragma Import (C, State, "__gnat_get_interrupt_state");
      --  Get interrupt state for interrupt number Int. Defined in init.c

      Default : constant Character := 's';
      --    's'   Interrupt_State pragma set state to System (use "default"
      --           system handler)

   begin
      if Self_ID.Deferral_Level = 0 then
         --  ???
         --  In principle, we should be able to predict whether abort is
         --  already deferred here (and it should not be deferred yet but in
         --  practice it seems Finalize_Global_Tasks is being called sometimes,
         --  from RTS code for exceptions, with abort already deferred.

         Initialization.Defer_Abort_Nestable (Self_ID);

         --  Never undefer again!!!
      end if;

      --  This code is only executed by the environment task

      pragma Assert (Self_ID = Environment_Task);

      --  Set Environment_Task'Callable to false to notify library-level tasks
      --  that it is waiting for them.

      Self_ID.Callable := False;

      --  Exit level 2 master, for normal tasks in library-level packages

      Complete_Master;

      --  Force termination of "independent" library-level server tasks

      Lock_RTS;

      Abort_Dependents (Self_ID);

      if not Single_Lock then
         Unlock_RTS;
      end if;

      --  We need to explicitly wait for the task to be terminated here
      --  because on true concurrent system, we may end this procedure before
      --  the tasks are really terminated.

      Write_Lock (Self_ID);

      --  If the Abort_Task signal is set to system, it means that we may not
      --  have been able to abort all independent tasks (in particular
      --  Server_Task may be blocked, waiting for a signal), in which case,
      --  do not wait for Independent_Task_Count to go down to 0.

      if State
          (System.Interrupt_Management.Abort_Task_Interrupt) /= Default
      then
         loop
            exit when Utilities.Independent_Task_Count = 0;

            --  We used to yield here, but this did not take into account low
            --  priority tasks that would cause dead lock in some cases (true
            --  FIFO scheduling).

            Timed_Sleep
              (Self_ID, 0.01, System.OS_Primitives.Relative,
               Self_ID.Common.State, Ignore_1, Ignore_2);
         end loop;
      end if;

      --  ??? On multi-processor environments, it seems that the above loop
      --  isn't sufficient, so we need to add an additional delay.

      Timed_Sleep
        (Self_ID, 0.01, System.OS_Primitives.Relative,
         Self_ID.Common.State, Ignore_1, Ignore_2);

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Complete the environment task

      Vulnerable_Complete_Task (Self_ID);

      --  Handle normal task termination by the environment task, but only
      --  for the normal task termination. In the case of Abnormal and
      --  Unhandled_Exception they must have been handled before, and the
      --  task termination soft link must have been changed so the task
      --  termination routine is not executed twice.

      SSL.Task_Termination_Handler.all (Ada.Exceptions.Null_Occurrence);

      --  Finalize all library-level controlled objects

      if not SSL."=" (SSL.Finalize_Library_Objects, null) then
         SSL.Finalize_Library_Objects.all;
      end if;

      --  Reset the soft links to non-tasking

      SSL.Abort_Defer        := SSL.Abort_Defer_NT'Access;
      SSL.Abort_Undefer      := SSL.Abort_Undefer_NT'Access;
      SSL.Lock_Task          := SSL.Task_Lock_NT'Access;
      SSL.Unlock_Task        := SSL.Task_Unlock_NT'Access;
      SSL.Get_Jmpbuf_Address := SSL.Get_Jmpbuf_Address_NT'Access;
      SSL.Set_Jmpbuf_Address := SSL.Set_Jmpbuf_Address_NT'Access;
      SSL.Get_Sec_Stack_Addr := SSL.Get_Sec_Stack_Addr_NT'Access;
      SSL.Set_Sec_Stack_Addr := SSL.Set_Sec_Stack_Addr_NT'Access;
      SSL.Check_Abort_Status := SSL.Check_Abort_Status_NT'Access;
      SSL.Get_Stack_Info     := SSL.Get_Stack_Info_NT'Access;

      --  Don't bother trying to finalize Initialization.Global_Task_Lock
      --  and System.Task_Primitives.RTS_Lock.

   end Finalize_Global_Tasks;

   ---------------
   -- Free_Task --
   ---------------

   procedure Free_Task (T : Task_Id) is
      Self_Id : constant Task_Id := Self;

   begin
      if T.Common.State = Terminated then

         --  It is not safe to call Abort_Defer or Write_Lock at this stage

         Initialization.Task_Lock (Self_Id);

         Lock_RTS;
         Initialization.Finalize_Attributes_Link.all (T);
         Initialization.Remove_From_All_Tasks_List (T);
         Unlock_RTS;

         Initialization.Task_Unlock (Self_Id);

         System.Task_Primitives.Operations.Finalize_TCB (T);

      else
         --  If the task is not terminated, then mark the task as to be freed
         --  upon termination.

         T.Free_On_Termination := True;
      end if;
   end Free_Task;

   ---------------------------
   -- Move_Activation_Chain --
   ---------------------------

   procedure Move_Activation_Chain
     (From, To   : Activation_Chain_Access;
      New_Master : Master_ID)
   is
      Self_ID : constant Task_Id := STPO.Self;
      C       : Task_Id;

   begin
      pragma Debug
        (Debug.Trace (Self_ID, "Move_Activation_Chain", 'C'));

      --  Nothing to do if From is empty, and we can check that without
      --  deferring aborts.

      C := From.all.T_ID;

      if C = null then
         return;
      end if;

      Initialization.Defer_Abort (Self_ID);

      --  Loop through the From chain, changing their Master_of_Task fields,
      --  and to find the end of the chain.

      loop
         C.Master_of_Task := New_Master;
         exit when C.Common.Activation_Link = null;
         C := C.Common.Activation_Link;
      end loop;

      --  Hook From in at the start of To

      C.Common.Activation_Link := To.all.T_ID;
      To.all.T_ID := From.all.T_ID;

      --  Set From to empty

      From.all.T_ID := null;

      Initialization.Undefer_Abort (Self_ID);
   end Move_Activation_Chain;

   ------------------
   -- Task_Wrapper --
   ------------------

   --  The task wrapper is a procedure that is called first for each task body
   --  and which in turn calls the compiler-generated task body procedure.
   --  The wrapper's main job is to do initialization for the task. It also
   --  has some locally declared objects that serve as per-task local data.
   --  Task finalization is done by Complete_Task, which is called from an
   --  at-end handler that the compiler generates.

   procedure Task_Wrapper (Self_ID : Task_Id) is
      use type SSE.Storage_Offset;
      use System.Standard_Library;
      use System.Stack_Usage;

      Bottom_Of_Stack : aliased Integer;

      Task_Alternate_Stack :
        aliased SSE.Storage_Array (1 .. Alternate_Stack_Size);
      --  The alternate signal stack for this task, if any

      Use_Alternate_Stack : constant Boolean := Alternate_Stack_Size /= 0;
      --  Whether to use above alternate signal stack for stack overflows

      Secondary_Stack_Size :
        constant SSE.Storage_Offset :=
          Self_ID.Common.Compiler_Data.Pri_Stack_Info.Size *
            SSE.Storage_Offset (Parameters.Sec_Stack_Percentage) / 100;

      Secondary_Stack : aliased SSE.Storage_Array (1 .. Secondary_Stack_Size);
      --  Actual area allocated for secondary stack

      Secondary_Stack_Address : System.Address := Secondary_Stack'Address;
      --  Address of secondary stack. In the fixed secondary stack case, this
      --  value is not modified, causing a warning, hence the bracketing with
      --  Warnings (Off/On). But why is so much *more* bracketed???

      SEH_Table : aliased SSE.Storage_Array (1 .. 8);
      --  Structured Exception Registration table (2 words)

      procedure Install_SEH_Handler (Addr : System.Address);
      pragma Import (C, Install_SEH_Handler, "__gnat_install_SEH_handler");
      --  Install the SEH (Structured Exception Handling) handler

      Cause : Cause_Of_Termination := Normal;
      --  Indicates the reason why this task terminates. Normal corresponds to
      --  a task terminating due to completing the last statement of its body,
      --  or as a result of waiting on a terminate alternative. If the task
      --  terminates because it is being aborted then Cause will be set
      --  to Abnormal. If the task terminates because of an exception
      --  raised by the execution of its task body, then Cause is set
      --  to Unhandled_Exception.

      EO : Exception_Occurrence;
      --  If the task terminates because of an exception raised by the
      --  execution of its task body, then EO will contain the associated
      --  exception occurrence. Otherwise, it will contain Null_Occurrence.

      TH : Termination_Handler := null;
      --  Pointer to the protected procedure to be executed upon task
      --  termination.

      procedure Search_Fall_Back_Handler (ID : Task_Id);
      --  Procedure that searches recursively a fall-back handler through the
      --  master relationship. If the handler is found, its pointer is stored
      --  in TH. It stops when the handler is found or when the ID is null.

      ------------------------------
      -- Search_Fall_Back_Handler --
      ------------------------------

      procedure Search_Fall_Back_Handler (ID : Task_Id) is
      begin
         --  A null Task_Id indicates that we have reached the root of the
         --  task hierarchy and no handler has been found.

         if ID = null then
            return;

         --  If there is a fall back handler, store its pointer for later
         --  execution.

         elsif ID.Common.Fall_Back_Handler /= null then
            TH := ID.Common.Fall_Back_Handler;

         --  Otherwise look for a fall back handler in the parent

         else
            Search_Fall_Back_Handler (ID.Common.Parent);
         end if;
      end Search_Fall_Back_Handler;

   --  Start of processing for Task_Wrapper

   begin
      pragma Assert (Self_ID.Deferral_Level = 1);

      --  Assume a size of the stack taken at this stage

      if not Parameters.Sec_Stack_Dynamic then
         Self_ID.Common.Compiler_Data.Sec_Stack_Addr :=
           Secondary_Stack'Address;
         SST.SS_Init (Secondary_Stack_Address, Integer (Secondary_Stack'Last));
      end if;

      if Use_Alternate_Stack then
         Self_ID.Common.Task_Alternate_Stack := Task_Alternate_Stack'Address;
      end if;

      --  Set the guard page at the bottom of the stack. The call to unprotect
      --  the page is done in Terminate_Task

      Stack_Guard (Self_ID, True);

      --  Initialize low-level TCB components, that cannot be initialized by
      --  the creator. Enter_Task sets Self_ID.LL.Thread.

      Enter_Task (Self_ID);

      --  Initialize dynamic stack usage

      if System.Stack_Usage.Is_Enabled then
         declare
            Guard_Page_Size : constant := 16 * 1024;
            --  Part of the stack used as a guard page. This is an OS dependent
            --  value, so we need to use the maximum. This value is only used
            --  when the stack address is known, that is currently Windows.

            Small_Overflow_Guard : constant := 12 * 1024;
            --  Note: this used to be 4K, but was changed to 12K, since
            --  smaller values resulted in segmentation faults from dynamic
            --  stack analysis.

            Big_Overflow_Guard : constant := 64 * 1024 + 8 * 1024;
            Small_Stack_Limit  : constant := 64 * 1024;
            --  ??? These three values are experimental, and seem to work on
            --  most platforms. They still need to be analyzed further. They
            --  also need documentation, what are they and why does the logic
            --  differ depending on whether the stack is large or small???

            Pattern_Size : Natural :=
                             Natural (Self_ID.Common.
                                        Compiler_Data.Pri_Stack_Info.Size);
            --  Size of the pattern

            Stack_Base : Address;
            --  Address of the base of the stack

         begin
            Stack_Base := Self_ID.Common.Compiler_Data.Pri_Stack_Info.Base;

            if Stack_Base = Null_Address then

               --  On many platforms, we don't know the real stack base
               --  address. Estimate it using an address in the frame.

               Stack_Base := Bottom_Of_Stack'Address;

               --  Also reduce the size of the stack to take into account the
               --  secondary stack array declared in this frame. This is for
               --  sure very conservative.

               if not Parameters.Sec_Stack_Dynamic then
                  Pattern_Size :=
                    Pattern_Size - Natural (Secondary_Stack_Size);
               end if;

               --  Adjustments for inner frames

               Pattern_Size := Pattern_Size -
                 (if Pattern_Size < Small_Stack_Limit
                    then Small_Overflow_Guard
                    else Big_Overflow_Guard);
            else
               --  Reduce by the size of the final guard page

               Pattern_Size := Pattern_Size - Guard_Page_Size;
            end if;

            STPO.Lock_RTS;
            Initialize_Analyzer
              (Self_ID.Common.Analyzer,
               Self_ID.Common.Task_Image (1 .. Self_ID.Common.Task_Image_Len),
               Natural (Self_ID.Common.Compiler_Data.Pri_Stack_Info.Size),
               SSE.To_Integer (Stack_Base),
               Pattern_Size);
            STPO.Unlock_RTS;
            Fill_Stack (Self_ID.Common.Analyzer);
         end;
      end if;

      --  We setup the SEH (Structured Exception Handling) handler if supported
      --  on the target.

      Install_SEH_Handler (SEH_Table'Address);

      --  Initialize exception occurrence

      Save_Occurrence (EO, Ada.Exceptions.Null_Occurrence);

      --  We lock RTS_Lock to wait for activator to finish activating the rest
      --  of the chain, so that everyone in the chain comes out in priority
      --  order.

      --  This also protects the value of
      --    Self_ID.Common.Activator.Common.Wait_Count.

      Lock_RTS;
      Unlock_RTS;

      if not System.Restrictions.Abort_Allowed then

         --  If Abort is not allowed, reset the deferral level since it will
         --  not get changed by the generated code. Keeping a default value
         --  of one would prevent some operations (e.g. select or delay) to
         --  proceed successfully.

         Self_ID.Deferral_Level := 0;
      end if;

      if Global_Task_Debug_Event_Set then
         Debug.Signal_Debug_Event (Debug.Debug_Event_Run, Self_ID);
      end if;

      begin
         --  We are separating the following portion of the code in order to
         --  place the exception handlers in a different block. In this way,
         --  we do not call Set_Jmpbuf_Address (which needs Self) before we
         --  set Self in Enter_Task

         --  Call the task body procedure

         --  The task body is called with abort still deferred. That
         --  eliminates a dangerous window, for which we had to patch-up in
         --  Terminate_Task.

         --  During the expansion of the task body, we insert an RTS-call
         --  to Abort_Undefer, at the first point where abort should be
         --  allowed.

         Self_ID.Common.Task_Entry_Point (Self_ID.Common.Task_Arg);
         Initialization.Defer_Abort_Nestable (Self_ID);

      exception
         --  We can't call Terminate_Task in the exception handlers below,
         --  since there may be (e.g. in the case of GCC exception handling)
         --  clean ups associated with the exception handler that need to
         --  access task specific data.

         --  Defer abort so that this task can't be aborted while exiting

         when Standard'Abort_Signal =>
            Initialization.Defer_Abort_Nestable (Self_ID);

            --  Update the cause that motivated the task termination so that
            --  the appropriate information is passed to the task termination
            --  procedure. Task termination as a result of waiting on a
            --  terminate alternative is a normal termination, although it is
            --  implemented using the abort mechanisms.

            if Self_ID.Terminate_Alternative then
               Cause := Normal;

               if Global_Task_Debug_Event_Set then
                  Debug.Signal_Debug_Event
                   (Debug.Debug_Event_Terminated, Self_ID);
               end if;
            else
               Cause := Abnormal;

               if Global_Task_Debug_Event_Set then
                  Debug.Signal_Debug_Event
                   (Debug.Debug_Event_Abort_Terminated, Self_ID);
               end if;
            end if;

         when others =>
            --  ??? Using an E : others here causes CD2C11A to fail on Tru64

            Initialization.Defer_Abort_Nestable (Self_ID);

            --  Perform the task specific exception tracing duty.  We handle
            --  these outputs here and not in the common notification routine
            --  because we need access to tasking related data and we don't
            --  want to drag dependencies against tasking related units in the
            --  the common notification units. Additionally, no trace is ever
            --  triggered from the common routine for the Unhandled_Raise case
            --  in tasks, since an exception never appears unhandled in this
            --  context because of this handler.

            if Exception_Trace = Unhandled_Raise then
               Trace_Unhandled_Exception_In_Task (Self_ID);
            end if;

            --  Update the cause that motivated the task termination so that
            --  the appropriate information is passed to the task termination
            --  procedure, as well as the associated Exception_Occurrence.

            Cause := Unhandled_Exception;

            Save_Occurrence (EO, SSL.Get_Current_Excep.all.all);

            if Global_Task_Debug_Event_Set then
               Debug.Signal_Debug_Event
                 (Debug.Debug_Event_Exception_Terminated, Self_ID);
            end if;
      end;

      --  Look for a task termination handler. This code is for all tasks but
      --  the environment task. The task termination code for the environment
      --  task is executed by SSL.Task_Termination_Handler.

      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Self_ID);

      if Self_ID.Common.Specific_Handler /= null then
         TH := Self_ID.Common.Specific_Handler;
      else
         --  Look for a fall-back handler following the master relationship
         --  for the task. As specified in ARM C.7.3 par. 9/2, "the fall-back
         --  handler applies only to the dependent tasks of the task". Hence,
         --  if the terminating tasks (Self_ID) had a fall-back handler, it
         --  would not apply to itself, so we start the search with the parent.

         Search_Fall_Back_Handler (Self_ID.Common.Parent);
      end if;

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Execute the task termination handler if we found it

      if TH /= null then
         begin
            TH.all (Cause, Self_ID, EO);

         exception

            --  RM-C.7.3 requires all exceptions raised here to be ignored

            when others =>
               null;
         end;
      end if;

      if System.Stack_Usage.Is_Enabled then
         Compute_Result (Self_ID.Common.Analyzer);
         Report_Result (Self_ID.Common.Analyzer);
      end if;

      Terminate_Task (Self_ID);
   end Task_Wrapper;

   --------------------
   -- Terminate_Task --
   --------------------

   --  Before we allow the thread to exit, we must clean up. This is a delicate
   --  job. We must wake up the task's master, who may immediately try to
   --  deallocate the ATCB from the current task WHILE IT IS STILL EXECUTING.

   --  To avoid this, the parent task must be blocked up to the latest
   --  statement executed. The trouble is that we have another step that we
   --  also want to postpone to the very end, i.e., calling SSL.Destroy_TSD.
   --  We have to postpone that until the end because compiler-generated code
   --  is likely to try to access that data at just about any point.

   --  We can't call Destroy_TSD while we are holding any other locks, because
   --  it locks Global_Task_Lock, and our deadlock prevention rules require
   --  that to be the outermost lock. Our first "solution" was to just lock
   --  Global_Task_Lock in addition to the other locks, and force the parent to
   --  also lock this lock between its wakeup and its freeing of the ATCB. See
   --  Complete_Task for the parent-side of the code that has the matching
   --  calls to Task_Lock and Task_Unlock. That was not really a solution,
   --  since the operation Task_Unlock continued to access the ATCB after
   --  unlocking, after which the parent was observed to race ahead, deallocate
   --  the ATCB, and then reallocate it to another task. The call to
   --  Undefer_Abort in Task_Unlock by the "terminated" task was overwriting
   --  the data of the new task that reused the ATCB! To solve this problem, we
   --  introduced the new operation Final_Task_Unlock.

   procedure Terminate_Task (Self_ID : Task_Id) is
      Environment_Task : constant Task_Id := STPO.Environment_Task;
      Master_of_Task   : Integer;
      Deallocate       : Boolean;

   begin
      Debug.Task_Termination_Hook;

      if Runtime_Traces then
         Send_Trace_Info (T_Terminate);
      end if;

      --  Since GCC cannot allocate stack chunks efficiently without reordering
      --  some of the allocations, we have to handle this unexpected situation
      --  here. Normally we never have to call Vulnerable_Complete_Task here.

      if Self_ID.Common.Activator /= null then
         Vulnerable_Complete_Task (Self_ID);
      end if;

      Initialization.Task_Lock (Self_ID);

      if Single_Lock then
         Lock_RTS;
      end if;

      Master_of_Task := Self_ID.Master_of_Task;

      --  Check if the current task is an independent task If so, decrement
      --  the Independent_Task_Count value.

      if Master_of_Task = Independent_Task_Level then
         if Single_Lock then
            Utilities.Independent_Task_Count :=
              Utilities.Independent_Task_Count - 1;

         else
            Write_Lock (Environment_Task);
            Utilities.Independent_Task_Count :=
              Utilities.Independent_Task_Count - 1;
            Unlock (Environment_Task);
         end if;
      end if;

      --  Unprotect the guard page if needed

      Stack_Guard (Self_ID, False);

      Utilities.Make_Passive (Self_ID, Task_Completed => True);
      Deallocate := Self_ID.Free_On_Termination;

      if Single_Lock then
         Unlock_RTS;
      end if;

      pragma Assert (Check_Exit (Self_ID));

      SSL.Destroy_TSD (Self_ID.Common.Compiler_Data);
      Initialization.Final_Task_Unlock (Self_ID);

      --  WARNING: past this point, this thread must assume that the ATCB has
      --  been deallocated, and can't access it anymore (which is why we have
      --  saved the Free_On_Termination flag in a temporary variable).

      if Deallocate then
         Free_Task (Self_ID);
      end if;

      if Master_of_Task > 0 then
         STPO.Exit_Task;
      end if;
   end Terminate_Task;

   ----------------
   -- Terminated --
   ----------------

   function Terminated (T : Task_Id) return Boolean is
      Self_ID : constant Task_Id := STPO.Self;
      Result  : Boolean;

   begin
      Initialization.Defer_Abort_Nestable (Self_ID);

      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (T);
      Result := T.Common.State = Terminated;
      Unlock (T);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Initialization.Undefer_Abort_Nestable (Self_ID);
      return Result;
   end Terminated;

   ----------------------------------------
   -- Trace_Unhandled_Exception_In_Task --
   ----------------------------------------

   procedure Trace_Unhandled_Exception_In_Task (Self_Id : Task_Id) is
      procedure To_Stderr (S : String);
      pragma Import (Ada, To_Stderr, "__gnat_to_stderr");

      use System.Soft_Links;
      use System.Standard_Library;

      function To_Address is new
        Ada.Unchecked_Conversion
         (Task_Id, System.Task_Primitives.Task_Address);

      function Tailored_Exception_Information
        (E : Exception_Occurrence) return String;
      pragma Import
        (Ada, Tailored_Exception_Information,
         "__gnat_tailored_exception_information");

      Excep : constant Exception_Occurrence_Access :=
                SSL.Get_Current_Excep.all;

   begin
      --  This procedure is called by the task outermost handler in
      --  Task_Wrapper below, so only once the task stack has been fully
      --  unwound. The common notification routine has been called at the
      --  raise point already.

      --  Lock to prevent unsynchronized output

      Initialization.Task_Lock (Self_Id);
      To_Stderr ("task ");

      if Self_Id.Common.Task_Image_Len /= 0 then
         To_Stderr
           (Self_Id.Common.Task_Image (1 .. Self_Id.Common.Task_Image_Len));
         To_Stderr ("_");
      end if;

      To_Stderr (System.Address_Image (To_Address (Self_Id)));
      To_Stderr (" terminated by unhandled exception");
      To_Stderr ((1 => ASCII.LF));
      To_Stderr (Tailored_Exception_Information (Excep.all));
      Initialization.Task_Unlock (Self_Id);
   end Trace_Unhandled_Exception_In_Task;

   ------------------------------------
   -- Vulnerable_Complete_Activation --
   ------------------------------------

   --  As in several other places, the locks of the activator and activated
   --  task are both locked here. This follows our deadlock prevention lock
   --  ordering policy, since the activated task must be created after the
   --  activator.

   procedure Vulnerable_Complete_Activation (Self_ID : Task_Id) is
      Activator : constant Task_Id := Self_ID.Common.Activator;

   begin
      pragma Debug (Debug.Trace (Self_ID, "V_Complete_Activation", 'C'));

      Write_Lock (Activator);
      Write_Lock (Self_ID);

      pragma Assert (Self_ID.Common.Activator /= null);

      --  Remove dangling reference to Activator, since a task may outlive its
      --  activator.

      Self_ID.Common.Activator := null;

      --  Wake up the activator, if it is waiting for a chain of tasks to
      --  activate, and we are the last in the chain to complete activation.

      if Activator.Common.State = Activator_Sleep then
         Activator.Common.Wait_Count := Activator.Common.Wait_Count - 1;

         if Activator.Common.Wait_Count = 0 then
            Wakeup (Activator, Activator_Sleep);
         end if;
      end if;

      --  The activator raises a Tasking_Error if any task it is activating
      --  is completed before the activation is done. However, if the reason
      --  for the task completion is an abort, we do not raise an exception.
      --  See RM 9.2(5).

      if not Self_ID.Callable and then Self_ID.Pending_ATC_Level /= 0 then
         Activator.Common.Activation_Failed := True;
      end if;

      Unlock (Self_ID);
      Unlock (Activator);

      --  After the activation, active priority should be the same as base
      --  priority. We must unlock the Activator first, though, since it
      --  should not wait if we have lower priority.

      if Get_Priority (Self_ID) /= Self_ID.Common.Base_Priority then
         Write_Lock (Self_ID);
         Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
         Unlock (Self_ID);
      end if;
   end Vulnerable_Complete_Activation;

   --------------------------------
   -- Vulnerable_Complete_Master --
   --------------------------------

   procedure Vulnerable_Complete_Master (Self_ID : Task_Id) is
      C  : Task_Id;
      P  : Task_Id;
      CM : constant Master_Level := Self_ID.Master_Within;
      T  : aliased Task_Id;

      To_Be_Freed : Task_Id;
      --  This is a list of ATCBs to be freed, after we have released all RTS
      --  locks. This is necessary because of the locking order rules, since
      --  the storage manager uses Global_Task_Lock.

      pragma Warnings (Off);
      function Check_Unactivated_Tasks return Boolean;
      pragma Warnings (On);
      --  Temporary error-checking code below. This is part of the checks
      --  added in the new run time. Call it only inside a pragma Assert.

      -----------------------------
      -- Check_Unactivated_Tasks --
      -----------------------------

      function Check_Unactivated_Tasks return Boolean is
      begin
         if not Single_Lock then
            Lock_RTS;
         end if;

         Write_Lock (Self_ID);

         C := All_Tasks_List;
         while C /= null loop
            if C.Common.Activator = Self_ID and then C.Master_of_Task = CM then
               return False;
            end if;

            if C.Common.Parent = Self_ID and then C.Master_of_Task = CM then
               Write_Lock (C);

               if C.Common.State = Unactivated then
                  return False;
               end if;

               Unlock (C);
            end if;

            C := C.Common.All_Tasks_Link;
         end loop;

         Unlock (Self_ID);

         if not Single_Lock then
            Unlock_RTS;
         end if;

         return True;
      end Check_Unactivated_Tasks;

   --  Start of processing for Vulnerable_Complete_Master

   begin
      pragma Debug
        (Debug.Trace (Self_ID, "V_Complete_Master", 'C'));

      pragma Assert (Self_ID.Common.Wait_Count = 0);
      pragma Assert
        (Self_ID.Deferral_Level > 0
          or else not System.Restrictions.Abort_Allowed);

      --  Count how many active dependent tasks this master currently has, and
      --  record this in Wait_Count.

      --  This count should start at zero, since it is initialized to zero for
      --  new tasks, and the task should not exit the sleep-loops that use this
      --  count until the count reaches zero.

      --  While we're counting, if we run across any unactivated tasks that
      --  belong to this master, we summarily terminate them as required by
      --  RM-9.2(6).

      Lock_RTS;
      Write_Lock (Self_ID);

      C := All_Tasks_List;
      while C /= null loop

         --  Terminate unactivated (never-to-be activated) tasks

         if C.Common.Activator = Self_ID and then C.Master_of_Task = CM then

            --  Usually, C.Common.Activator = Self_ID implies C.Master_of_Task
            --  = CM. The only case where C is pending activation by this
            --  task, but the master of C is not CM is in Ada 2005, when C is
            --  part of a return object of a build-in-place function.

            pragma Assert (C.Common.State = Unactivated);

            Write_Lock (C);
            C.Common.Activator := null;
            C.Common.State := Terminated;
            C.Callable := False;
            Utilities.Cancel_Queued_Entry_Calls (C);
            Unlock (C);
         end if;

         --  Count it if dependent on this master

         if C.Common.Parent = Self_ID and then C.Master_of_Task = CM then
            Write_Lock (C);

            if C.Awake_Count /= 0 then
               Self_ID.Common.Wait_Count := Self_ID.Common.Wait_Count + 1;
            end if;

            Unlock (C);
         end if;

         C := C.Common.All_Tasks_Link;
      end loop;

      Self_ID.Common.State := Master_Completion_Sleep;
      Unlock (Self_ID);

      if not Single_Lock then
         Unlock_RTS;
      end if;

      --  Wait until dependent tasks are all terminated or ready to terminate.
      --  While waiting, the task may be awakened if the task's priority needs
      --  changing, or this master is aborted. In the latter case, we abort the
      --  dependents, and resume waiting until Wait_Count goes to zero.

      Write_Lock (Self_ID);

      loop
         exit when Self_ID.Common.Wait_Count = 0;

         --  Here is a difference as compared to Complete_Master

         if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
           and then not Self_ID.Dependents_Aborted
         then
            if Single_Lock then
               Abort_Dependents (Self_ID);
            else
               Unlock (Self_ID);
               Lock_RTS;
               Abort_Dependents (Self_ID);
               Unlock_RTS;
               Write_Lock (Self_ID);
            end if;
         else
            Sleep (Self_ID, Master_Completion_Sleep);
         end if;
      end loop;

      Self_ID.Common.State := Runnable;
      Unlock (Self_ID);

      --  Dependents are all terminated or on terminate alternatives. Now,
      --  force those on terminate alternatives to terminate, by aborting them.

      pragma Assert (Check_Unactivated_Tasks);

      if Self_ID.Alive_Count > 1 then
         --  ???
         --  Consider finding a way to skip the following extra steps if there
         --  are no dependents with terminate alternatives. This could be done
         --  by adding another count to the ATCB, similar to Awake_Count, but
         --  keeping track of tasks that are on terminate alternatives.

         pragma Assert (Self_ID.Common.Wait_Count = 0);

         --  Force any remaining dependents to terminate by aborting them

         if not Single_Lock then
            Lock_RTS;
         end if;

         Abort_Dependents (Self_ID);

         --  Above, when we "abort" the dependents we are simply using this
         --  operation for convenience. We are not required to support the full
         --  abort-statement semantics; in particular, we are not required to
         --  immediately cancel any queued or in-service entry calls. That is
         --  good, because if we tried to cancel a call we would need to lock
         --  the caller, in order to wake the caller up. Our anti-deadlock
         --  rules prevent us from doing that without releasing the locks on C
         --  and Self_ID. Releasing and retaking those locks would be wasteful
         --  at best, and should not be considered further without more
         --  detailed analysis of potential concurrent accesses to the ATCBs
         --  of C and Self_ID.

         --  Count how many "alive" dependent tasks this master currently has,
         --  and record this in Wait_Count. This count should start at zero,
         --  since it is initialized to zero for new tasks, and the task should
         --  not exit the sleep-loops that use this count until the count
         --  reaches zero.

         pragma Assert (Self_ID.Common.Wait_Count = 0);

         Write_Lock (Self_ID);

         C := All_Tasks_List;
         while C /= null loop
            if C.Common.Parent = Self_ID and then C.Master_of_Task = CM then
               Write_Lock (C);

               pragma Assert (C.Awake_Count = 0);

               if C.Alive_Count > 0 then
                  pragma Assert (C.Terminate_Alternative);
                  Self_ID.Common.Wait_Count := Self_ID.Common.Wait_Count + 1;
               end if;

               Unlock (C);
            end if;

            C := C.Common.All_Tasks_Link;
         end loop;

         Self_ID.Common.State := Master_Phase_2_Sleep;
         Unlock (Self_ID);

         if not Single_Lock then
            Unlock_RTS;
         end if;

         --  Wait for all counted tasks to finish terminating themselves

         Write_Lock (Self_ID);

         loop
            exit when Self_ID.Common.Wait_Count = 0;
            Sleep (Self_ID, Master_Phase_2_Sleep);
         end loop;

         Self_ID.Common.State := Runnable;
         Unlock (Self_ID);
      end if;

      --  We don't wake up for abort here. We are already terminating just as
      --  fast as we can, so there is no point.

      --  Remove terminated tasks from the list of Self_ID's dependents, but
      --  don't free their ATCBs yet, because of lock order restrictions, which
      --  don't allow us to call "free" or "malloc" while holding any other
      --  locks. Instead, we put those ATCBs to be freed onto a temporary list,
      --  called To_Be_Freed.

      if not Single_Lock then
         Lock_RTS;
      end if;

      C := All_Tasks_List;
      P := null;
      while C /= null loop

         --  If Free_On_Termination is set, do nothing here, and let the
         --  task free itself if not already done, otherwise we risk a race
         --  condition where Vulnerable_Free_Task is called in the loop below,
         --  while the task calls Free_Task itself, in Terminate_Task.

         if C.Common.Parent = Self_ID
           and then C.Master_of_Task >= CM
           and then not C.Free_On_Termination
         then
            if P /= null then
               P.Common.All_Tasks_Link := C.Common.All_Tasks_Link;
            else
               All_Tasks_List := C.Common.All_Tasks_Link;
            end if;

            T := C.Common.All_Tasks_Link;
            C.Common.All_Tasks_Link := To_Be_Freed;
            To_Be_Freed := C;
            C := T;

         else
            P := C;
            C := C.Common.All_Tasks_Link;
         end if;
      end loop;

      Unlock_RTS;

      --  Free all the ATCBs on the list To_Be_Freed

      --  The ATCBs in the list are no longer in All_Tasks_List, and after
      --  any interrupt entries are detached from them they should no longer
      --  be referenced.

      --  Global_Task_Lock (Task_Lock/Unlock) is locked in the loop below to
      --  avoid a race between a terminating task and its parent. The parent
      --  might try to deallocate the ACTB out from underneath the exiting
      --  task. Note that Free will also lock Global_Task_Lock, but that is
      --  OK, since this is the *one* lock for which we have a mechanism to
      --  support nested locking. See Task_Wrapper and its finalizer for more
      --  explanation.

      --  ???
      --  The check "T.Common.Parent /= null ..." below is to prevent dangling
      --  references to terminated library-level tasks, which could otherwise
      --  occur during finalization of library-level objects. A better solution
      --  might be to hook task objects into the finalization chain and
      --  deallocate the ATCB when the task object is deallocated. However,
      --  this change is not likely to gain anything significant, since all
      --  this storage should be recovered en-masse when the process exits.

      while To_Be_Freed /= null loop
         T := To_Be_Freed;
         To_Be_Freed := T.Common.All_Tasks_Link;

         --  ??? On SGI there is currently no Interrupt_Manager, that's why we
         --  need to check if the Interrupt_Manager_ID is null.

         if T.Interrupt_Entry and then Interrupt_Manager_ID /= null then
            declare
               Detach_Interrupt_Entries_Index : constant Task_Entry_Index := 1;
               --  Corresponds to the entry index of System.Interrupts.
               --  Interrupt_Manager.Detach_Interrupt_Entries. Be sure
               --  to update this value when changing Interrupt_Manager specs.

               type Param_Type is access all Task_Id;

               Param : aliased Param_Type := T'Access;

            begin
               System.Tasking.Rendezvous.Call_Simple
                 (Interrupt_Manager_ID, Detach_Interrupt_Entries_Index,
                  Param'Address);
            end;
         end if;

         if (T.Common.Parent /= null
              and then T.Common.Parent.Common.Parent /= null)
           or else T.Master_of_Task > Library_Task_Level
         then
            Initialization.Task_Lock (Self_ID);

            --  If Sec_Stack_Addr is not null, it means that Destroy_TSD
            --  has not been called yet (case of an unactivated task).

            if T.Common.Compiler_Data.Sec_Stack_Addr /= Null_Address then
               SSL.Destroy_TSD (T.Common.Compiler_Data);
            end if;

            Vulnerable_Free_Task (T);
            Initialization.Task_Unlock (Self_ID);
         end if;
      end loop;

      --  It might seem nice to let the terminated task deallocate its own
      --  ATCB. That would not cover the case of unactivated tasks. It also
      --  would force us to keep the underlying thread around past termination,
      --  since references to the ATCB are possible past termination.

      --  Currently, we get rid of the thread as soon as the task terminates,
      --  and let the parent recover the ATCB later.

      --  Some day, if we want to recover the ATCB earlier, at task
      --  termination, we could consider using "fat task IDs", that include the
      --  serial number with the ATCB pointer, to catch references to tasks
      --  that no longer have ATCBs. It is not clear how much this would gain,
      --  since the user-level task object would still be occupying storage.

      --  Make next master level up active. We don't need to lock the ATCB,
      --  since the value is only updated by each task for itself.

      Self_ID.Master_Within := CM - 1;
   end Vulnerable_Complete_Master;

   ------------------------------
   -- Vulnerable_Complete_Task --
   ------------------------------

   --  Complete the calling task

   --  This procedure must be called with abort deferred. It should only be
   --  called by Complete_Task and Finalize_Global_Tasks (for the environment
   --  task).

   --  The effect is similar to that of Complete_Master. Differences include
   --  the closing of entries here, and computation of the number of active
   --  dependent tasks in Complete_Master.

   --  We don't lock Self_ID before the call to Vulnerable_Complete_Activation,
   --  because that does its own locking, and because we do not need the lock
   --  to test Self_ID.Common.Activator. That value should only be read and
   --  modified by Self.

   procedure Vulnerable_Complete_Task (Self_ID : Task_Id) is
   begin
      pragma Assert
        (Self_ID.Deferral_Level > 0
          or else not System.Restrictions.Abort_Allowed);
      pragma Assert (Self_ID = Self);
      pragma Assert (Self_ID.Master_Within = Self_ID.Master_of_Task + 1
                       or else
                     Self_ID.Master_Within = Self_ID.Master_of_Task + 2);
      pragma Assert (Self_ID.Common.Wait_Count = 0);
      pragma Assert (Self_ID.Open_Accepts = null);
      pragma Assert (Self_ID.ATC_Nesting_Level = 1);

      pragma Debug (Debug.Trace (Self_ID, "V_Complete_Task", 'C'));

      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Self_ID);
      Self_ID.Callable := False;

      --  In theory, Self should have no pending entry calls left on its
      --  call-stack. Each async. select statement should clean its own call,
      --  and blocking entry calls should defer abort until the calls are
      --  cancelled, then clean up.

      Utilities.Cancel_Queued_Entry_Calls (Self_ID);
      Unlock (Self_ID);

      if Self_ID.Common.Activator /= null then
         Vulnerable_Complete_Activation (Self_ID);
      end if;

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  If Self_ID.Master_Within = Self_ID.Master_of_Task + 2 we may have
      --  dependent tasks for which we need to wait. Otherwise we just exit.

      if Self_ID.Master_Within = Self_ID.Master_of_Task + 2 then
         Vulnerable_Complete_Master (Self_ID);
      end if;
   end Vulnerable_Complete_Task;

   --------------------------
   -- Vulnerable_Free_Task --
   --------------------------

   --  Recover all runtime system storage associated with the task T. This
   --  should only be called after T has terminated and will no longer be
   --  referenced.

   --  For tasks created by an allocator that fails, due to an exception, it
   --  is called from Expunge_Unactivated_Tasks.

   --  For tasks created by elaboration of task object declarations it is
   --  called from the finalization code of the Task_Wrapper procedure.

   procedure Vulnerable_Free_Task (T : Task_Id) is
   begin
      pragma Debug (Debug.Trace (Self, "Vulnerable_Free_Task", 'C', T));

      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (T);
      Initialization.Finalize_Attributes_Link.all (T);
      Unlock (T);

      if Single_Lock then
         Unlock_RTS;
      end if;

      System.Task_Primitives.Operations.Finalize_TCB (T);
   end Vulnerable_Free_Task;

--  Package elaboration code

begin
   --  Establish the Adafinal softlink

   --  This is not done inside the central RTS initialization routine
   --  to avoid with'ing this package from System.Tasking.Initialization.

   SSL.Adafinal := Finalize_Global_Tasks'Access;

   --  Establish soft links for subprograms that manipulate master_id's.
   --  This cannot be done when the RTS is initialized, because of various
   --  elaboration constraints.

   SSL.Current_Master  := Stages.Current_Master'Access;
   SSL.Enter_Master    := Stages.Enter_Master'Access;
   SSL.Complete_Master := Stages.Complete_Master'Access;
end System.Tasking.Stages;
