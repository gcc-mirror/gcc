------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--         S Y S T E M . T A S K I N G . I N I T I A L I Z A T I O N        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.63 $
--                                                                          --
--            Copyright (C) 1991-2001, Florida State University             --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (All_Checks);
--  Turn off subprogram alpha ordering check, since we group soft link
--  bodies and dummy soft link bodies together separately in this unit.

pragma Polling (Off);
--  Turn polling off for this package. We don't need polling during any
--  of the routines in this package, and more to the point, if we try
--  to poll it can cause infinite loops.

--  This package provides overall initialization of the tasking portion
--  of the RTS.  This package must be elaborated before any tasking
--  features are used.  It also contains initialization for
--  Ada Task Control Block (ATCB) records.

with Ada.Exceptions;
--  used for Exception_Occurrence_Access.

with System.Tasking;
pragma Elaborate_All (System.Tasking);
--  ensure that the first step initializations have been performed

with System.Task_Primitives;
--  used for Lock

with System.Task_Primitives.Operations;
--  used for Set_Priority
--           Write_Lock
--           Unlock
--           Initialize_Lock

with System.Soft_Links;
--  used for the non-tasking routines (*_NT) that refer to global data.
--  They are needed here before the tasking run time has been elaborated.

with System.Tasking.Debug;
--  used for Trace

with System.Tasking.Task_Attributes;
--  used for All_Attrs_L

with System.Stack_Checking;

package body System.Tasking.Initialization is

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;
   package AE   renames Ada.Exceptions;

   use System.Task_Primitives.Operations;

   Global_Task_Lock : aliased System.Task_Primitives.RTS_Lock;
   --  This is a global lock; it is used to execute in mutual exclusion
   --  from all other tasks.  It is only used by Task_Lock,
   --  Task_Unlock, and Final_Task_Unlock.

   function Current_Target_Exception return AE.Exception_Occurrence;
   pragma Import
     (Ada, Current_Target_Exception, "__gnat_current_target_exception");
   --  Import this subprogram from the private part of Ada.Exceptions.

   -----------------------------------------------------------------
   -- Tasking versions of services needed by non-tasking programs --
   -----------------------------------------------------------------

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

   function  Get_Jmpbuf_Address return  Address;
   procedure Set_Jmpbuf_Address (Addr : Address);
   --  Get/Set Jmpbuf_Address for current task

   function  Get_Sec_Stack_Addr return  Address;
   procedure Set_Sec_Stack_Addr (Addr : Address);
   --  Get/Set location of current task's secondary stack

   function  Get_Exc_Stack_Addr return Address;
   --  Get the exception stack for the current task

   procedure Set_Exc_Stack_Addr (Self_ID : Address; Addr : Address);
   --  Self_ID is the Task_ID of the task that gets the exception stack.
   --  For Self_ID = Null_Address, the current task gets the exception stack.

   function  Get_Machine_State_Addr return Address;
   procedure Set_Machine_State_Addr (Addr : Address);
   --  Get/Set the address for storing the current task's machine state

   function Get_Current_Excep return SSL.EOA;
   --  Comments needed???

   procedure Timed_Delay_T (Time : Duration; Mode : Integer);
   --  Comments needed???

   function Get_Stack_Info return Stack_Checking.Stack_Access;
   --  Get access to the current task's Stack_Info

   procedure Update_Exception
     (X : AE.Exception_Occurrence := Current_Target_Exception);
   --  Handle exception setting and check for pending actions

   ------------------------
   --  Local Subprograms --
   ------------------------

   procedure Do_Pending_Action (Self_ID : Task_ID);
   --  This is introduced to allow more efficient
   --  in-line expansion of Undefer_Abort.

   ----------------------------
   -- Tasking Initialization --
   ----------------------------

   procedure Init_RTS;
   --  This procedure completes the initialization of the GNARL. The first
   --  part of the initialization is done in the body of System.Tasking.
   --  It consists of initializing global locks, and installing tasking
   --  versions of certain operations used by the compiler. Init_RTS is called
   --  during elaboration.

   --------------------------
   -- Change_Base_Priority --
   --------------------------

   --  Call only with abort deferred and holding Self_ID locked.

   procedure Change_Base_Priority (T : Task_ID) is
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
      Self_ID : Task_ID := Self;

   begin
      if Self_ID /= null and then Self_ID.Deferral_Level = 0
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

   procedure Defer_Abort (Self_ID : Task_ID) is
   begin

      pragma Assert (Self_ID.Deferral_Level = 0);

--        pragma Assert
--          (Self_ID.Pending_ATC_Level >= Self_ID.ATC_Nesting_Level);

      --  The above check has been useful in detecting mismatched
      --  defer/undefer pairs. You may uncomment it when testing on
      --  systems that support preemptive abort.

      --  If the OS supports preemptive abort (e.g. pthread_kill),
      --  it should have happened already. A problem is with systems
      --  that do not support preemptive abort, and so rely on polling.
      --  On such systems we may get false failures of the assertion,
      --  since polling for pending abort does no occur until the abort
      --  undefer operation.

      --  Even on systems that only poll for abort, the assertion may
      --  be useful for catching missed abort completion polling points.
      --  The operations that undefer abort poll for pending aborts.
      --  This covers most of the places where the core Ada semantics
      --  require abort to be caught, without any special attention.
      --  However, this generally happens on exit from runtime system
      --  call, which means a pending abort will not be noticed on the
      --  way into the runtime system.  We considered adding a check
      --  for pending aborts at this point, but chose not to, because
      --  of the overhead.  Instead, we searched for RTS calls that
      --  where abort completion is required and a task could go
      --  farther than Ada allows  before undeferring abort; we then
      --  modified the code to ensure the abort would be detected.

      Self_ID.Deferral_Level := Self_ID.Deferral_Level + 1;
   end Defer_Abort;

   --------------------------
   -- Defer_Abort_Nestable --
   --------------------------

   procedure Defer_Abort_Nestable (Self_ID : Task_ID) is
   begin

--        pragma Assert
--          ((Self_ID.Pending_ATC_Level >= Self_ID.ATC_Nesting_Level or else
--            Self_ID.Deferral_Level > 0));

      --  See comment in Defer_Abort on the situations in which it may
      --  be useful to uncomment the above assertion.

      Self_ID.Deferral_Level := Self_ID.Deferral_Level + 1;
   end Defer_Abort_Nestable;

   --------------------
   -- Defer_Abortion --
   --------------------

   --  ??????
   --  Phase out Defer_Abortion without Self_ID
   --  to reduce overhead due to multiple calls to Self

   procedure Defer_Abortion is
      Self_ID : constant Task_ID := STPO.Self;

   begin
      Self_ID.Deferral_Level := Self_ID.Deferral_Level + 1;
   end Defer_Abortion;

   -----------------------
   -- Do_Pending_Action --
   -----------------------

   --  Call only when holding no locks

   procedure Do_Pending_Action (Self_ID : Task_ID) is
      use type Ada.Exceptions.Exception_Id;

   begin
      pragma Assert (Self_ID = Self and then Self_ID.Deferral_Level = 0);

      --  Needs loop to recheck for pending action in case a new one occurred
      --  while we had abort deferred below.

      loop
         --  Temporarily defer abortion so that we can lock Self_ID.

         Self_ID.Deferral_Level := Self_ID.Deferral_Level + 1;

         Write_Lock (Self_ID);
         Self_ID.Pending_Action := False;
         Poll_Base_Priority_Change (Self_ID);
         Unlock (Self_ID);

         --  Restore the original Deferral value.

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

   --  This version is only for use in Terminate_Task, when the task
   --  is relinquishing further rights to its own ATCB.
   --  There is a very interesting potential race condition there, where
   --  the old task may run concurrently with a new task that is allocated
   --  the old tasks (now reused) ATCB.  The critical thing here is to
   --  not make any reference to the ATCB after the lock is released.
   --  See also comments on Terminate_Task and Unlock.

   procedure Final_Task_Unlock (Self_ID : Task_ID) is
   begin
      pragma Assert (Self_ID.Global_Task_Lock_Nesting = 1);
      Unlock (Global_Task_Lock'Access);
   end Final_Task_Unlock;

   --------------
   -- Init_RTS --
   --------------

   procedure Init_RTS is
      Self_Id : Task_ID;
   begin
      --  Terminate run time (regular vs restricted) specific initialization
      --  of the environment task.

      Self_Id := Environment_Task;
      Self_Id.Master_of_Task := Environment_Task_Level;
      Self_Id.Master_Within := Self_Id.Master_of_Task + 1;

      for L in Self_Id.Entry_Calls'Range loop
         Self_Id.Entry_Calls (L).Self := Self_Id;
         Self_Id.Entry_Calls (L).Level := L;
      end loop;

      Self_Id.Awake_Count := 1;
      Self_Id.Alive_Count := 1;

      Self_Id.Master_Within := Library_Task_Level;
      --  Normally, a task starts out with internal master nesting level
      --  one larger than external master nesting level. It is incremented
      --  to one by Enter_Master, which is called in the task body only if
      --  the compiler thinks the task may have dependent tasks. There is no
      --  corresponding call to Enter_Master for the environment task, so we
      --  would need to increment it to 2 here.  Instead, we set it to 3.
      --  By doing this we reserve the level 2 for server tasks of the runtime
      --  system. The environment task does not need to wait for these server

      --  Initialize lock used to implement mutual exclusion between all tasks

      Initialize_Lock (Global_Task_Lock'Access, STPO.Global_Task_Level);

      --  Initialize lock used to implement mutual exclusion in the package
      --  System.Task_Attributes.

      Initialize_Lock (System.Tasking.Task_Attributes.All_Attrs_L'Access,
        All_Attrs_Level);

      --  Notify that the tasking run time has been elaborated so that
      --  the tasking version of the soft links can be used.

      SSL.Abort_Defer            := Defer_Abortion'Access;
      SSL.Abort_Undefer          := Undefer_Abortion'Access;
      SSL.Update_Exception       := Update_Exception'Access;
      SSL.Lock_Task              := Task_Lock'Access;
      SSL.Unlock_Task            := Task_Unlock'Access;
      SSL.Get_Jmpbuf_Address     := Get_Jmpbuf_Address'Access;
      SSL.Set_Jmpbuf_Address     := Set_Jmpbuf_Address'Access;
      SSL.Get_Sec_Stack_Addr     := Get_Sec_Stack_Addr'Access;
      SSL.Set_Sec_Stack_Addr     := Set_Sec_Stack_Addr'Access;
      SSL.Get_Exc_Stack_Addr     := Get_Exc_Stack_Addr'Access;
      SSL.Set_Exc_Stack_Addr     := Set_Exc_Stack_Addr'Access;
      SSL.Get_Machine_State_Addr := Get_Machine_State_Addr'Access;
      SSL.Set_Machine_State_Addr := Set_Machine_State_Addr'Access;
      SSL.Get_Current_Excep      := Get_Current_Excep'Access;
      SSL.Timed_Delay            := Timed_Delay_T'Access;
      SSL.Check_Abort_Status     := Check_Abort_Status'Access;
      SSL.Get_Stack_Info         := Get_Stack_Info'Access;

      --  No need to create a new Secondary Stack, since we will use the
      --  default one created in s-secsta.adb

      SSL.Set_Sec_Stack_Addr     (SSL.Get_Sec_Stack_Addr_NT);
      SSL.Set_Exc_Stack_Addr     (Null_Address, SSL.Get_Exc_Stack_Addr_NT);
      SSL.Set_Jmpbuf_Address     (SSL.Get_Jmpbuf_Address_NT);
      SSL.Set_Machine_State_Addr (SSL.Get_Machine_State_Addr_NT);

      --  Abortion is deferred in a new ATCB, so we need to undefer abortion
      --  at this stage to make the environment task abortable.

      Undefer_Abort (Environment_Task);
   end Init_RTS;

   ---------------------------
   -- Locked_Abort_To_Level--
   ---------------------------

   --  Abort a task to the specified ATC nesting level.
   --  Call this only with T locked.

   --  An earlier version of this code contained a call to Wakeup. That
   --  should not be necessary here, if Abort_Task is implemented correctly,
   --  since Abort_Task should include the effect of Wakeup. However, the
   --  above call was in earlier versions of this file, and at least for
   --  some targets Abort_Task has not beek doing Wakeup. It should not
   --  hurt to uncomment the above call, until the error is corrected for
   --  all targets.

   --  See extended comments in package body System.Tasking.Abortion
   --  for the overall design of the implementation of task abort.

   --  If the task is sleeping it will be in an abort-deferred region,
   --  and will not have Abort_Signal raised by Abort_Task.
   --  Such an "abort deferral" is just to protect the RTS internals,
   --  and not necessarily required to enforce Ada semantics.
   --  Abort_Task should wake the task up and let it decide if it wants
   --  to complete the aborted construct immediately.

   --  Note that the effect of the lowl-level Abort_Task is not persistent.
   --  If the target task is not blocked, this wakeup will be missed.

   --  We don't bother calling Abort_Task if this task is aborting itself,
   --  since we are inside the RTS and have abort deferred. Similarly, We
   --  don't bother to call Abort_Task if T is terminated, since there is
   --  no need to abort a terminated task, and it could be dangerous to try
   --  if the task has stopped executing.

   --  Note that an earlier version of this code had some false reasoning
   --  about being able to reliably wake up a task that had suspended on
   --  a blocking system call that does not atomically relase the task's
   --  lock (e.g., UNIX nanosleep, which we once thought could be used to
   --  implement delays). That still left the possibility of missed
   --  wakeups.

   --  We cannot safely call Vulnerable_Complete_Activation here,
   --  since that requires locking Self_ID.Parent. The anti-deadlock
   --  lock ordering rules would then require us to release the lock
   --  on Self_ID first, which would create a timing window for other
   --  tasks to lock Self_ID. This is significant for tasks that may be
   --  aborted before their execution can enter the task body, and so
   --  they do not get a chance to call Complete_Task. The actual work
   --  for this case is done in Terminate_Task.

   procedure Locked_Abort_To_Level
     (Self_ID : Task_ID;
      T       : Task_ID;
      L       : ATC_Level) is

   begin
      if not T.Aborting and then T /= Self_ID then
         case T.Common.State is
            when Unactivated | Terminated =>
               pragma Assert (False);
               null;

            when Runnable =>
               --  This is needed to cancel an asynchronous protected entry
               --  call during a requeue with abort.

               T.Entry_Calls
                 (T.ATC_Nesting_Level).Cancellation_Attempted := True;

            when Interrupt_Server_Blocked_On_Event_Flag =>
               null;

            when Delay_Sleep                              |
                 Async_Select_Sleep                       |
                 Interrupt_Server_Idle_Sleep              |
                 Interrupt_Server_Blocked_Interrupt_Sleep |
                 Timer_Server_Sleep                       |
                 AST_Server_Sleep                         =>
               Wakeup (T, T.Common.State);

            when Acceptor_Sleep =>
               T.Open_Accepts := null;
               Wakeup (T, T.Common.State);

            when Entry_Caller_Sleep  =>
               T.Entry_Calls
                 (T.ATC_Nesting_Level).Cancellation_Attempted := True;
               Wakeup (T, T.Common.State);

            when Activator_Sleep         |
                 Master_Completion_Sleep |
                 Master_Phase_2_Sleep    |
                 Asynchronous_Hold       =>
               null;
         end case;
      end if;

      if T.Pending_ATC_Level > L then
         T.Pending_ATC_Level := L;
         T.Pending_Action := True;

         if L = 0 then
            T.Callable := False;
         end if;

         --  This prevents aborted task from accepting calls

         if T.Aborting then

            --  The test above is just a heuristic, to reduce wasteful
            --  calls to Abort_Task.  We are holding T locked, and this
            --  value will not be set to False except with T also locked,
            --  inside Exit_One_ATC_Level, so we should not miss wakeups.

            if T.Common.State = Acceptor_Sleep then
               T.Open_Accepts := null;
            end if;

         elsif T /= Self_ID and then
           (T.Common.State = Runnable
            or else T.Common.State = Interrupt_Server_Blocked_On_Event_Flag)
            --  The task is blocked on a system call waiting for the
            --  completion event. In this case Abort_Task may need to take
            --  special action in order to succeed. Example system: VMS.

         then
            Abort_Task (T);
         end if;
      end if;
   end Locked_Abort_To_Level;

   -------------------------------
   -- Poll_Base_Priority_Change --
   -------------------------------

   --  Poll for pending base priority change and for held tasks.
   --  This should always be called with (only) Self_ID locked.
   --  It may temporarily release Self_ID's lock.

   --  The call to Yield is to force enqueuing at the
   --  tail of the dispatching queue.

   --  We must unlock Self_ID for this to take effect,
   --  since we are inheriting high active priority from the lock.

   --  See also Poll_Base_Priority_Change_At_Entry_Call,
   --  in package System.Tasking.Entry_Calls.

   --  In this version, we check if the task is held too because
   --  doing this only in Do_Pending_Action is not enough.

   procedure Poll_Base_Priority_Change (Self_ID : Task_ID) is
   begin
      if Dynamic_Priority_Support
        and then Self_ID.Pending_Priority_Change
      then
         --  Check for ceiling violations ???

         Self_ID.Pending_Priority_Change := False;

         if Self_ID.Common.Base_Priority = Self_ID.New_Base_Priority then
            Unlock (Self_ID);
            Yield;
            Write_Lock (Self_ID);

         elsif Self_ID.Common.Base_Priority < Self_ID.New_Base_Priority then
            Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
            Set_Priority (Self_ID, Self_ID.Common.Base_Priority);

         else
            --  Lowering priority

            Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
            Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
            Unlock (Self_ID);
            Yield;
            Write_Lock (Self_ID);
         end if;
      end if;
   end Poll_Base_Priority_Change;

   --------------------------------
   -- Remove_From_All_Tasks_List --
   --------------------------------

   procedure Remove_From_All_Tasks_List (T : Task_ID) is
      C        : Task_ID;
      Previous : Task_ID;

   begin
      pragma Debug
        (Debug.Trace ("Remove_From_All_Tasks_List", 'C'));

      Lock_All_Tasks_List;

      Previous := Null_Task;
      C := All_Tasks_List;
      while C /= Null_Task loop
         if C = T then
            if Previous = Null_Task then
               All_Tasks_List :=
                 All_Tasks_List.Common.All_Tasks_Link;
            else
               Previous.Common.All_Tasks_Link := C.Common.All_Tasks_Link;
            end if;

            Unlock_All_Tasks_List;
            return;
         end if;

         Previous := C;
         C := C.Common.All_Tasks_Link;
      end loop;

      pragma Assert (False);
   end Remove_From_All_Tasks_List;

   ---------------
   -- Task_Lock --
   ---------------

   procedure Task_Lock is
      T : Task_ID := STPO.Self;

   begin
      T.Global_Task_Lock_Nesting := T.Global_Task_Lock_Nesting + 1;

      if T.Global_Task_Lock_Nesting = 1 then
         Defer_Abort_Nestable (T);
         Write_Lock (Global_Task_Lock'Access);
      end if;
   end Task_Lock;

   procedure Task_Lock (Self_ID : Task_ID) is
   begin
      Self_ID.Global_Task_Lock_Nesting := Self_ID.Global_Task_Lock_Nesting + 1;

      if Self_ID.Global_Task_Lock_Nesting = 1 then
         Defer_Abort_Nestable (Self_ID);
         Write_Lock (Global_Task_Lock'Access);
      end if;
   end Task_Lock;

   -----------------
   -- Task_Unlock --
   -----------------

   procedure Task_Unlock is
      T : Task_ID := STPO.Self;

   begin
      pragma Assert (T.Global_Task_Lock_Nesting > 0);

      T.Global_Task_Lock_Nesting := T.Global_Task_Lock_Nesting - 1;

      if T.Global_Task_Lock_Nesting = 0 then
         Unlock (Global_Task_Lock'Access);
         Undefer_Abort_Nestable (T);
      end if;
   end Task_Unlock;

   procedure Task_Unlock (Self_ID : Task_ID) is
   begin
      Self_ID.Global_Task_Lock_Nesting := Self_ID.Global_Task_Lock_Nesting - 1;

      if Self_ID.Global_Task_Lock_Nesting = 0 then
         Unlock (Global_Task_Lock'Access);
         Undefer_Abort_Nestable (Self_ID);
      end if;
   end Task_Unlock;

   -------------------
   -- Undefer_Abort --
   -------------------

   --  Precondition : Self does not hold any locks!

   --  Undefer_Abort is called on any abortion completion point (aka.
   --  synchronization point). It performs the following actions if they
   --  are pending: (1) change the base priority, (2) abort the task,
   --  (3) raise a pending exception.

   --  The priority change has to occur before abortion. Otherwise, it would
   --  take effect no earlier than the next abortion completion point.

   procedure Undefer_Abort (Self_ID : Task_ID) is
   begin
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

   --  An earlier version  would re-defer abort if an abort is
   --  in progress.  Then, we modified the effect of the raise
   --  statement so that it defers abort until control reaches a
   --  handler.  That was done to prevent "skipping over" a
   --  handler if another asynchronous abort occurs during the
   --  propagation of the abort to the handler.

   --  There has been talk of reversing that decision, based on
   --  a newer implementation of exception propagation.  Care must
   --  be taken to evaluate how such a change would interact with
   --  the above code and all the places where abort-deferral is
   --  used to bridge over critical transitions, such as entry to
   --  the scope of a region with a finalizer and entry into the
   --  body of an accept-procedure.

   procedure Undefer_Abort_Nestable (Self_ID : Task_ID) is
   begin
      pragma Assert (Self_ID.Deferral_Level > 0);

      Self_ID.Deferral_Level := Self_ID.Deferral_Level - 1;

      if Self_ID.Deferral_Level = 0 then

         pragma Assert (Check_No_Locks (Self_ID));

         if Self_ID.Pending_Action then
            Do_Pending_Action (Self_ID);
         end if;
      end if;
   end Undefer_Abort_Nestable;

   ----------------------
   -- Undefer_Abortion --
   ----------------------

   --  Phase out RTS-internal use of Undefer_Abortion
   --  to reduce overhead due to multiple calls to Self.

   procedure Undefer_Abortion is
      Self_ID : constant Task_ID := STPO.Self;

   begin
      pragma Assert (Self_ID.Deferral_Level > 0);

      Self_ID.Deferral_Level := Self_ID.Deferral_Level - 1;

      if Self_ID.Deferral_Level = 0 then
         pragma Assert (Check_No_Locks (Self_ID));

         if Self_ID.Pending_Action then
            Do_Pending_Action (Self_ID);
         end if;
      end if;
   end Undefer_Abortion;

   ----------------------
   -- Update_Exception --
   ----------------------

   --  Call only when holding no locks.

   procedure Update_Exception
     (X : AE.Exception_Occurrence := Current_Target_Exception)
   is
      Self_Id : constant Task_ID := Self;
      use Ada.Exceptions;

   begin
      Save_Occurrence (Self_Id.Common.Compiler_Data.Current_Excep, X);

      if Self_Id.Deferral_Level = 0 then
         if Self_Id.Pending_Action then
            Self_Id.Pending_Action := False;
            Self_Id.Deferral_Level := Self_Id.Deferral_Level + 1;
            Write_Lock (Self_Id);
            Self_Id.Pending_Action := False;
            Poll_Base_Priority_Change (Self_Id);
            Unlock (Self_Id);
            Self_Id.Deferral_Level := Self_Id.Deferral_Level - 1;

            if Self_Id.Pending_ATC_Level < Self_Id.ATC_Nesting_Level then
               if not Self_Id.Aborting then
                  Self_Id.Aborting := True;
                  raise Standard'Abort_Signal;
               end if;
            end if;
         end if;
      end if;
   end Update_Exception;

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
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link;
      New_State  : Entry_Call_State)
   is
      Caller : constant Task_ID := Entry_Call.Self;

   begin
      pragma Debug (Debug.Trace
        (Self_ID, "Wakeup_Entry_Caller", Caller, 'E'));
      pragma Assert (New_State = Done or else New_State = Cancelled);

      pragma Assert
        (Caller.Common.State /= Terminated
          and then Caller.Common.State /= Unactivated);

      Entry_Call.State := New_State;

      if Entry_Call.Mode = Asynchronous_Call then

         --  Abort the caller in his abortable part,
         --  but do so only if call has been queued abortably

         if Entry_Call.State >= Was_Abortable or else New_State = Done then
            Locked_Abort_To_Level (Self_ID, Caller, Entry_Call.Level - 1);
         end if;

      elsif Caller.Common.State = Entry_Caller_Sleep then
         Wakeup (Caller, Entry_Caller_Sleep);
      end if;
   end Wakeup_Entry_Caller;

   ----------------------
   -- Soft-Link Bodies --
   ----------------------

   function Get_Current_Excep return SSL.EOA is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Current_Excep'Access;
   end Get_Current_Excep;

   function Get_Exc_Stack_Addr return Address is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Exc_Stack_Addr;
   end Get_Exc_Stack_Addr;

   function Get_Jmpbuf_Address return  Address is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Jmpbuf_Address;
   end Get_Jmpbuf_Address;

   function Get_Machine_State_Addr return Address is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Machine_State_Addr;
   end Get_Machine_State_Addr;

   function Get_Sec_Stack_Addr return  Address is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Sec_Stack_Addr;
   end Get_Sec_Stack_Addr;

   function Get_Stack_Info return Stack_Checking.Stack_Access is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Pri_Stack_Info'Access;
   end Get_Stack_Info;

   procedure Set_Exc_Stack_Addr (Self_ID : Address; Addr : Address) is
      Me : Task_ID := To_Task_Id (Self_ID);

   begin
      if Me = Null_Task then
         Me := STPO.Self;
      end if;

      Me.Common.Compiler_Data.Exc_Stack_Addr := Addr;
   end Set_Exc_Stack_Addr;

   procedure Set_Jmpbuf_Address (Addr : Address) is
      Me : Task_ID := STPO.Self;

   begin
      Me.Common.Compiler_Data.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address;

   procedure Set_Machine_State_Addr (Addr : Address) is
      Me : Task_ID := STPO.Self;

   begin
      Me.Common.Compiler_Data.Machine_State_Addr := Addr;
   end Set_Machine_State_Addr;

   procedure Set_Sec_Stack_Addr (Addr : Address) is
      Me : Task_ID := STPO.Self;

   begin
      Me.Common.Compiler_Data.Sec_Stack_Addr := Addr;
   end Set_Sec_Stack_Addr;

   procedure Timed_Delay_T (Time : Duration; Mode : Integer) is
      Self_ID : constant Task_ID := Self;

   begin
      STPO.Timed_Delay (Self_ID, Time, Mode);
   end Timed_Delay_T;

   ------------------------
   -- Soft-Link Dummies  --
   ------------------------

   --  These are dummies for subprograms that are only needed by certain
   --  optional run-time system packages.  If they are needed, the soft
   --  links will be redirected to the real subprogram by elaboration of
   --  the subprogram body where the real subprogram is declared.

   procedure Finalize_Attributes (T : Task_ID) is
   begin
      null;
   end Finalize_Attributes;

   procedure Initialize_Attributes (T : Task_ID) is
   begin
      null;
   end Initialize_Attributes;

begin
   Init_RTS;
end System.Tasking.Initialization;
