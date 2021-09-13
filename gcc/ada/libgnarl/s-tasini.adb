------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--         S Y S T E M . T A S K I N G . I N I T I A L I Z A T I O N        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2021, Free Software Foundation, Inc.          --
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
--  Turn off subprogram alpha ordering check, since we group soft link bodies
--  and dummy soft link bodies together separately in this unit.

with System.Task_Primitives;
with System.Task_Primitives.Operations;
with System.Soft_Links;
with System.Soft_Links.Tasking;
with System.Tasking.Debug;
with System.Tasking.Task_Attributes;

with System.Secondary_Stack;
pragma Elaborate_All (System.Secondary_Stack);
pragma Unreferenced (System.Secondary_Stack);
--  Make sure the body of Secondary_Stack is elaborated before calling
--  Init_Tasking_Soft_Links. See comments for this routine for explanation.

package body System.Tasking.Initialization is

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;

   use Parameters;
   use Task_Primitives.Operations;

   Global_Task_Lock : aliased System.Task_Primitives.RTS_Lock;
   --  This is a global lock; it is used to execute in mutual exclusion from
   --  all other tasks. It is only used by Task_Lock, Task_Unlock, and
   --  Final_Task_Unlock.

   ----------------------------------------------------------------------
   -- Tasking versions of some services needed by non-tasking programs --
   ----------------------------------------------------------------------

   procedure Abort_Defer;
   --  NON-INLINE versions without Self_ID for soft links

   procedure Abort_Undefer;
   --  NON-INLINE versions without Self_ID for soft links

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

   function Get_Current_Excep return SSL.EOA;
   --  Task-safe version of SSL.Get_Current_Excep

   function Task_Name return String;
   --  Returns current task's name

   ------------------------
   --  Local Subprograms --
   ------------------------

   ----------------------------
   -- Tasking Initialization --
   ----------------------------

   procedure Init_RTS;
   --  This procedure completes the initialization of the GNARL. The first part
   --  of the initialization is done in the body of System.Tasking. It consists
   --  of initializing global locks, and installing tasking versions of certain
   --  operations used by the compiler. Init_RTS is called during elaboration.

   --------------------------
   -- Change_Base_Priority --
   --------------------------

   --  Call only with abort deferred and holding Self_ID locked

   procedure Change_Base_Priority (T : Task_Id) is
   begin
      if T.Common.Base_Priority /= T.New_Base_Priority then
         T.Common.Base_Priority := T.New_Base_Priority;
         Set_Priority (T, T.Common.Base_Priority);
      end if;
   end Change_Base_Priority;

   ------------------------
   -- Check_Abort_Status --
   ------------------------

   function Check_Abort_Status return Integer is
      Self_ID : constant Task_Id := Self;
   begin
      if Self_ID /= null
        and then Self_ID.Deferral_Level = 0
        and then Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
      then
         return 1;
      else
         return 0;
      end if;
   end Check_Abort_Status;

   -----------------
   -- Defer_Abort --
   -----------------

   procedure Defer_Abort (Self_ID : Task_Id) is
   begin
      if No_Abort then
         return;
      end if;

      pragma Assert (Self_ID.Deferral_Level = 0);

      --  pragma Assert
      --    (Self_ID.Pending_ATC_Level >= Self_ID.ATC_Nesting_Level);

      --  The above check has been useful in detecting mismatched defer/undefer
      --  pairs. You may uncomment it when testing on systems that support
      --  preemptive abort.

      --  If the OS supports preemptive abort (e.g. pthread_kill), it should
      --  have happened already. A problem is with systems that do not support
      --  preemptive abort, and so rely on polling. On such systems we may get
      --  false failures of the assertion, since polling for pending abort does
      --  no occur until the abort undefer operation.

      --  Even on systems that only poll for abort, the assertion may be useful
      --  for catching missed abort completion polling points. The operations
      --  that undefer abort poll for pending aborts. This covers most of the
      --  places where the core Ada semantics require abort to be caught,
      --  without any special attention. However, this generally happens on
      --  exit from runtime system call, which means a pending abort will not
      --  be noticed on the way into the runtime system. We considered adding a
      --  check for pending aborts at this point, but chose not to, because of
      --  the overhead. Instead, we searched for RTS calls where abort
      --  completion is required and a task could go farther than Ada allows
      --  before undeferring abort; we then modified the code to ensure the
      --  abort would be detected.

      Self_ID.Deferral_Level := Self_ID.Deferral_Level + 1;
   end Defer_Abort;

   --------------------------
   -- Defer_Abort_Nestable --
   --------------------------

   procedure Defer_Abort_Nestable (Self_ID : Task_Id) is
   begin
      if No_Abort then
         return;
      end if;

      --  The following assertion is by default disabled. See the comment in
      --  Defer_Abort on the situations in which it may be useful to uncomment
      --  this assertion and enable the test.

      --  pragma Assert
      --    (Self_ID.Pending_ATC_Level >= Self_ID.ATC_Nesting_Level or else
      --     Self_ID.Deferral_Level > 0);

      Self_ID.Deferral_Level := Self_ID.Deferral_Level + 1;
   end Defer_Abort_Nestable;

   -----------------
   -- Abort_Defer --
   -----------------

   procedure Abort_Defer is
      Self_ID : Task_Id;
   begin
      if No_Abort then
         return;
      end if;

      Self_ID := STPO.Self;
      Self_ID.Deferral_Level := Self_ID.Deferral_Level + 1;
   end Abort_Defer;

   -----------------------
   -- Get_Current_Excep --
   -----------------------

   function Get_Current_Excep return SSL.EOA is
   begin
      return STPO.Self.Common.Compiler_Data.Current_Excep'Access;
   end Get_Current_Excep;

   -----------------------
   -- Do_Pending_Action --
   -----------------------

   --  Call only when holding no locks

   procedure Do_Pending_Action (Self_ID : Task_Id) is

   begin
      pragma Assert (Self_ID = Self and then Self_ID.Deferral_Level = 0);

      --  Needs loop to recheck for pending action in case a new one occurred
      --  while we had abort deferred below.

      loop
         --  Temporarily defer abort so that we can lock Self_ID

         Self_ID.Deferral_Level := Self_ID.Deferral_Level + 1;

         Write_Lock (Self_ID);
         Self_ID.Pending_Action := False;
         Unlock (Self_ID);

         --  Restore the original Deferral value

         Self_ID.Deferral_Level := Self_ID.Deferral_Level - 1;

         if not Self_ID.Pending_Action then
            if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level then
               if not Self_ID.Aborting then
                  Self_ID.Aborting := True;
                  pragma Debug
                    (Debug.Trace (Self_ID, "raise Abort_Signal", 'B'));
                  raise Standard'Abort_Signal;

                  pragma Assert (not Self_ID.ATC_Hack);

               elsif Self_ID.ATC_Hack then

                  --  The solution really belongs in the Abort_Signal handler
                  --  for async. entry calls.  The present hack is very
                  --  fragile. It relies that the very next point after
                  --  Exit_One_ATC_Level at which the task becomes abortable
                  --  will be the call to Undefer_Abort in the
                  --  Abort_Signal handler.

                  Self_ID.ATC_Hack := False;

                  pragma Debug
                    (Debug.Trace
                     (Self_ID, "raise Abort_Signal (ATC hack)", 'B'));
                  raise Standard'Abort_Signal;
               end if;
            end if;

            return;
         end if;
      end loop;
   end Do_Pending_Action;

   -----------------------
   -- Final_Task_Unlock --
   -----------------------

   --  This version is only for use in Terminate_Task, when the task is
   --  relinquishing further rights to its own ATCB.

   --  There is a very interesting potential race condition there, where the
   --  old task may run concurrently with a new task that is allocated the old
   --  tasks (now reused) ATCB. The critical thing here is to not make any
   --  reference to the ATCB after the lock is released. See also comments on
   --  Terminate_Task and Unlock.

   procedure Final_Task_Unlock (Self_ID : Task_Id) is
   begin
      pragma Assert (Self_ID.Common.Global_Task_Lock_Nesting = 1);
      Unlock (Global_Task_Lock'Access);
   end Final_Task_Unlock;

   --------------
   -- Init_RTS --
   --------------

   procedure Init_RTS is
      Self_Id : Task_Id;
   begin
      Tasking.Initialize;

      --  Terminate run time (regular vs restricted) specific initialization
      --  of the environment task.

      Self_Id := Environment_Task;
      Self_Id.Master_Of_Task := Environment_Task_Level;
      Self_Id.Master_Within := Self_Id.Master_Of_Task + 1;

      for L in Self_Id.Entry_Calls'Range loop
         Self_Id.Entry_Calls (L).Self := Self_Id;
         Self_Id.Entry_Calls (L).Level := L;
      end loop;

      Self_Id.Awake_Count := 1;
      Self_Id.Alive_Count := 1;

      --  Normally, a task starts out with internal master nesting level one
      --  larger than external master nesting level. It is incremented to one
      --  by Enter_Master, which is called in the task body only if the
      --  compiler thinks the task may have dependent tasks. There is no
      --  corresponding call to Enter_Master for the environment task, so we
      --  would need to increment it to 2 here. Instead, we set it to 3. By
      --  doing this we reserve the level 2 for server tasks of the runtime
      --  system. The environment task does not need to wait for these server

      Self_Id.Master_Within := Library_Task_Level;

      --  Initialize lock used to implement mutual exclusion between all tasks

      Initialize_Lock (Global_Task_Lock'Access, STPO.Global_Task_Level);

      --  Notify that the tasking run time has been elaborated so that
      --  the tasking version of the soft links can be used.

      if not No_Abort then
         SSL.Abort_Defer   := Abort_Defer'Access;
         SSL.Abort_Undefer := Abort_Undefer'Access;
      end if;

      SSL.Lock_Task          := Task_Lock'Access;
      SSL.Unlock_Task        := Task_Unlock'Access;
      SSL.Check_Abort_Status := Check_Abort_Status'Access;
      SSL.Task_Name          := Task_Name'Access;
      SSL.Get_Current_Excep  := Get_Current_Excep'Access;

      --  Initialize the tasking soft links (if not done yet) that are common
      --  to the full and the restricted run times.

      SSL.Tasking.Init_Tasking_Soft_Links;

      --  Abort is deferred in a new ATCB, so we need to undefer abort at this
      --  stage to make the environment task abortable.

      Undefer_Abort (Environment_Task);
   end Init_RTS;

   ---------------------------
   -- Locked_Abort_To_Level--
   ---------------------------

   --  Abort a task to the specified ATC nesting level.
   --  Call this only with T locked.

   --  An earlier version of this code contained a call to Wakeup. That should
   --  not be necessary here, if Abort_Task is implemented correctly, since
   --  Abort_Task should include the effect of Wakeup. However, the above call
   --  was in earlier versions of this file, and at least for some targets
   --  Abort_Task has not been doing Wakeup. It should not hurt to uncomment
   --  the above call, until the error is corrected for all targets.

   --  See extended comments in package body System.Tasking.Abort for the
   --  overall design of the implementation of task abort.
   --  ??? there is no such package ???

   --  If the task is sleeping it will be in an abort-deferred region, and will
   --  not have Abort_Signal raised by Abort_Task. Such an "abort deferral" is
   --  just to protect the RTS internals, and not necessarily required to
   --  enforce Ada semantics. Abort_Task should wake the task up and let it
   --  decide if it wants to complete the aborted construct immediately.

   --  Note that the effect of the low-level Abort_Task is not persistent.
   --  If the target task is not blocked, this wakeup will be missed.

   --  We don't bother calling Abort_Task if this task is aborting itself,
   --  since we are inside the RTS and have abort deferred. Similarly, We don't
   --  bother to call Abort_Task if T is terminated, since there is no need to
   --  abort a terminated task, and it could be dangerous to try if the task
   --  has stopped executing.

   --  Note that an earlier version of this code had some false reasoning about
   --  being able to reliably wake up a task that had suspended on a blocking
   --  system call that does not atomically release the task's lock (e.g., UNIX
   --  nanosleep, which we once thought could be used to implement delays).
   --  That still left the possibility of missed wakeups.

   --  We cannot safely call Vulnerable_Complete_Activation here, since that
   --  requires locking Self_ID.Parent. The anti-deadlock lock ordering rules
   --  would then require us to release the lock on Self_ID first, which would
   --  create a timing window for other tasks to lock Self_ID. This is
   --  significant for tasks that may be aborted before their execution can
   --  enter the task body, and so they do not get a chance to call
   --  Complete_Task. The actual work for this case is done in Terminate_Task.

   procedure Locked_Abort_To_Level
     (Self_ID : Task_Id;
      T       : Task_Id;
      L       : ATC_Level_Base)
   is
   begin
      if not T.Aborting and then T /= Self_ID then
         case T.Common.State is
            when Terminated
               | Unactivated
            =>
               pragma Assert (Standard.False);
               null;

            when Activating
               | Runnable
            =>
               if T.ATC_Nesting_Level > Level_No_ATC_Occurring then
                  --  This scenario occurs when an asynchronous protected entry
                  --  call is canceled during a requeue with abort.

                  T.Entry_Calls
                    (T.ATC_Nesting_Level).Cancellation_Attempted := True;
               end if;

            when Interrupt_Server_Blocked_On_Event_Flag =>
               null;

            when AST_Server_Sleep
               | Async_Select_Sleep
               | Delay_Sleep
               | Interrupt_Server_Blocked_Interrupt_Sleep
               | Interrupt_Server_Idle_Sleep
               | Timer_Server_Sleep
            =>
               Wakeup (T, T.Common.State);

            when Acceptor_Delay_Sleep
               | Acceptor_Sleep
            =>
               T.Open_Accepts := null;
               Wakeup (T, T.Common.State);

            when Entry_Caller_Sleep  =>
               pragma Assert (T.ATC_Nesting_Level > Level_No_ATC_Occurring);

               T.Entry_Calls
                 (T.ATC_Nesting_Level).Cancellation_Attempted := True;
               Wakeup (T, T.Common.State);

            when Activator_Sleep
               | Asynchronous_Hold
               | Master_Completion_Sleep
               | Master_Phase_2_Sleep
            =>
               null;
         end case;
      end if;

      if T.Pending_ATC_Level > L then
         T.Pending_ATC_Level := L;
         T.Pending_Action := True;

         if L = Level_Completed_Task then
            T.Callable := False;
         end if;

         --  This prevents aborted task from accepting calls

         if T.Aborting then

            --  The test above is just a heuristic, to reduce wasteful
            --  calls to Abort_Task.  We are holding T locked, and this
            --  value will not be set to False except with T also locked,
            --  inside Exit_One_ATC_Level, so we should not miss wakeups.

            if T.Common.State = Acceptor_Sleep
                 or else
               T.Common.State = Acceptor_Delay_Sleep
            then
               T.Open_Accepts := null;
            end if;

         elsif T /= Self_ID and then
           (T.Common.State = Runnable
             or else T.Common.State = Interrupt_Server_Blocked_On_Event_Flag)

            --  The task is blocked on a system call waiting for the
            --  completion event. In this case Abort_Task may need to take
            --  special action in order to succeed.

         then
            Abort_Task (T);
         end if;
      end if;
   end Locked_Abort_To_Level;

   --------------------------------
   -- Remove_From_All_Tasks_List --
   --------------------------------

   procedure Remove_From_All_Tasks_List (T : Task_Id) is
      C        : Task_Id;
      Previous : Task_Id;

   begin
      pragma Debug
        (Debug.Trace (Self, "Remove_From_All_Tasks_List", 'C'));

      Previous := Null_Task;
      C := All_Tasks_List;
      while C /= Null_Task loop
         if C = T then
            if Previous = Null_Task then
               All_Tasks_List := All_Tasks_List.Common.All_Tasks_Link;
            else
               Previous.Common.All_Tasks_Link := C.Common.All_Tasks_Link;
            end if;

            return;
         end if;

         Previous := C;
         C := C.Common.All_Tasks_Link;
      end loop;

      pragma Assert (Standard.False);
   end Remove_From_All_Tasks_List;

   ---------------
   -- Task_Lock --
   ---------------

   procedure Task_Lock (Self_ID : Task_Id) is
   begin
      Self_ID.Common.Global_Task_Lock_Nesting :=
        Self_ID.Common.Global_Task_Lock_Nesting + 1;

      if Self_ID.Common.Global_Task_Lock_Nesting = 1 then
         Defer_Abort_Nestable (Self_ID);
         Write_Lock (Global_Task_Lock'Access);
      end if;
   end Task_Lock;

   procedure Task_Lock is
   begin
      Task_Lock (STPO.Self);
   end Task_Lock;

   ---------------
   -- Task_Name --
   ---------------

   function Task_Name return String is
      Self_Id : constant Task_Id := STPO.Self;
   begin
      return Self_Id.Common.Task_Image (1 .. Self_Id.Common.Task_Image_Len);
   end Task_Name;

   -----------------
   -- Task_Unlock --
   -----------------

   procedure Task_Unlock (Self_ID : Task_Id) is
   begin
      pragma Assert (Self_ID.Common.Global_Task_Lock_Nesting > 0);
      Self_ID.Common.Global_Task_Lock_Nesting :=
        Self_ID.Common.Global_Task_Lock_Nesting - 1;

      if Self_ID.Common.Global_Task_Lock_Nesting = 0 then
         Unlock (Global_Task_Lock'Access);
         Undefer_Abort_Nestable (Self_ID);
      end if;
   end Task_Unlock;

   procedure Task_Unlock is
   begin
      Task_Unlock (STPO.Self);
   end Task_Unlock;

   -------------------
   -- Undefer_Abort --
   -------------------

   --  Precondition : Self does not hold any locks

   --  Undefer_Abort is called on any abort completion point (aka.
   --  synchronization point). It performs the following actions if they
   --  are pending: (1) change the base priority, (2) abort the task.

   --  The priority change has to occur before abort. Otherwise, it would
   --  take effect no earlier than the next abort completion point.

   procedure Undefer_Abort (Self_ID : Task_Id) is
   begin
      if No_Abort then
         return;
      end if;

      pragma Assert (Self_ID.Deferral_Level = 1);

      Self_ID.Deferral_Level := Self_ID.Deferral_Level - 1;

      if Self_ID.Deferral_Level = 0 then
         pragma Assert (Check_No_Locks (Self_ID));

         if Self_ID.Pending_Action then
            Do_Pending_Action (Self_ID);
         end if;
      end if;
   end Undefer_Abort;

   ----------------------------
   -- Undefer_Abort_Nestable --
   ----------------------------

   --  An earlier version would re-defer abort if an abort is in progress.
   --  Then, we modified the effect of the raise statement so that it defers
   --  abort until control reaches a handler. That was done to prevent
   --  "skipping over" a handler if another asynchronous abort occurs during
   --  the propagation of the abort to the handler.

   --  There has been talk of reversing that decision, based on a newer
   --  implementation of exception propagation. Care must be taken to evaluate
   --  how such a change would interact with the above code and all the places
   --  where abort-deferral is used to bridge over critical transitions, such
   --  as entry to the scope of a region with a finalizer and entry into the
   --  body of an accept-procedure.

   procedure Undefer_Abort_Nestable (Self_ID : Task_Id) is
   begin
      if No_Abort then
         return;
      end if;

      pragma Assert (Self_ID.Deferral_Level > 0);

      Self_ID.Deferral_Level := Self_ID.Deferral_Level - 1;

      if Self_ID.Deferral_Level = 0 then

         pragma Assert (Check_No_Locks (Self_ID));

         if Self_ID.Pending_Action then
            Do_Pending_Action (Self_ID);
         end if;
      end if;
   end Undefer_Abort_Nestable;

   -------------------
   -- Abort_Undefer --
   -------------------

   procedure Abort_Undefer is
      Self_ID : Task_Id;
   begin
      if No_Abort then
         return;
      end if;

      Self_ID := STPO.Self;

      if Self_ID.Deferral_Level = 0 then

         --  In case there are different views on whether Abort is supported
         --  between the expander and the run time, we may end up with
         --  Self_ID.Deferral_Level being equal to zero, when called from
         --  the procedure created by the expander that corresponds to a
         --  task body. In this case, there's nothing to be done.

         --  See related code in System.Tasking.Stages.Create_Task resetting
         --  Deferral_Level when System.Restrictions.Abort_Allowed is False.

         return;
      end if;

      pragma Assert (Self_ID.Deferral_Level > 0);
      Self_ID.Deferral_Level := Self_ID.Deferral_Level - 1;

      if Self_ID.Deferral_Level = 0 then
         pragma Assert (Check_No_Locks (Self_ID));

         if Self_ID.Pending_Action then
            Do_Pending_Action (Self_ID);
         end if;
      end if;
   end Abort_Undefer;

   --------------------------
   -- Wakeup_Entry_Caller --
   --------------------------

   --  This is called at the end of service of an entry call, to abort the
   --  caller if he is in an abortable part, and to wake up the caller if it
   --  is on Entry_Caller_Sleep. It assumes that the call is already off-queue.

   --  (This enforces the rule that a task must be off-queue if its state is
   --  Done or Cancelled.) Call it holding the lock of Entry_Call.Self.

   --  Timed_Call or Simple_Call:
   --    The caller is waiting on Entry_Caller_Sleep, in
   --    Wait_For_Completion, or Wait_For_Completion_With_Timeout.

   --  Conditional_Call:
   --    The caller might be in Wait_For_Completion,
   --    waiting for a rendezvous (possibly requeued without abort)
   --    to complete.

   --  Asynchronous_Call:
   --    The caller may be executing in the abortable part o
   --    an async. select, or on a time delay,
   --    if Entry_Call.State >= Was_Abortable.

   procedure Wakeup_Entry_Caller
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link;
      New_State  : Entry_Call_State)
   is
      Caller : constant Task_Id := Entry_Call.Self;

   begin
      pragma Debug (Debug.Trace
        (Self_ID, "Wakeup_Entry_Caller", 'E', Caller));
      pragma Assert (New_State = Done or else New_State = Cancelled);

      pragma Assert (Caller.Common.State /= Unactivated);

      Entry_Call.State := New_State;

      if Entry_Call.Mode = Asynchronous_Call then

         --  Abort the caller in his abortable part, but do so only if call has
         --  been queued abortably.

         if Entry_Call.State >= Was_Abortable or else New_State = Done then
            Locked_Abort_To_Level (Self_ID, Caller, Entry_Call.Level - 1);
         end if;

      elsif Caller.Common.State = Entry_Caller_Sleep then
         Wakeup (Caller, Entry_Caller_Sleep);
      end if;
   end Wakeup_Entry_Caller;

   -------------------------
   -- Finalize_Attributes --
   -------------------------

   procedure Finalize_Attributes (T : Task_Id) is
      Attr : Atomic_Address;

   begin
      for J in T.Attributes'Range loop
         Attr := T.Attributes (J);

         if Attr /= 0 and then Task_Attributes.Require_Finalization (J) then
            Task_Attributes.To_Attribute (Attr).Free (Attr);
            T.Attributes (J) := 0;
         end if;
      end loop;
   end Finalize_Attributes;

begin
   Init_RTS;
end System.Tasking.Initialization;
