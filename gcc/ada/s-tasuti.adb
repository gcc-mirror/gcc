------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--              S Y S T E M . T A S K I N G . U T I L I T I E S             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.67 $
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

--  This package provides RTS Internal Declarations.
--  These declarations are not part of the GNARLI

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Tasking.Debug;
--  used for Known_Tasks

with System.Task_Primitives.Operations;
--  used for Write_Lock
--           Set_Priority
--           Wakeup
--           Unlock
--           Sleep
--           Abort_Task
--           Lock/Unlock_All_Tasks_List

with System.Tasking.Initialization;
--  Used for Defer_Abort
--           Undefer_Abort
--           Locked_Abort_To_Level

with System.Tasking.Queuing;
--  used for Dequeue_Call
--           Dequeue_Head

with System.Tasking.Debug;
--  used for Trace

with Unchecked_Conversion;

package body System.Tasking.Utilities is

   package STPO renames System.Task_Primitives.Operations;

   use System.Tasking.Debug;
   use System.Task_Primitives;
   use System.Task_Primitives.Operations;

   procedure Locked_Abort_To_Level
     (Self_Id : Task_ID;
      T       : Task_ID;
      L       : ATC_Level)
   renames
     Initialization.Locked_Abort_To_Level;

   procedure Defer_Abort (Self_Id : Task_ID) renames
     System.Tasking.Initialization.Defer_Abort;

   procedure Defer_Abort_Nestable (Self_Id : Task_ID) renames
     System.Tasking.Initialization.Defer_Abort_Nestable;

   procedure Undefer_Abort (Self_Id : Task_ID) renames
     System.Tasking.Initialization.Undefer_Abort;

   procedure Undefer_Abort_Nestable (Self_Id : Task_ID) renames
     System.Tasking.Initialization.Undefer_Abort_Nestable;

   procedure Wakeup_Entry_Caller
     (Self_Id    : Task_ID;
      Entry_Call : Entry_Call_Link;
      New_State  : Entry_Call_State)
   renames
     Initialization.Wakeup_Entry_Caller;

   ----------------
   -- Abort_Task --
   ----------------

   --  Similar to Locked_Abort_To_Level (Self_ID, T, 0), but:
   --    (1) caller should be holding no locks
   --    (2) may be called for tasks that have not yet been activated
   --    (3) always aborts whole task

   procedure Abort_One_Task
     (Self_ID : Task_ID;
      T       : Task_ID)
   is
   begin
      Write_Lock (T);

      if T.Common.State = Unactivated then
         T.Common.Activator := null;
         T.Common.State := Terminated;
         T.Callable := False;
         Cancel_Queued_Entry_Calls (T);

      elsif T.Common.State /= Terminated then
         Locked_Abort_To_Level (Self_ID, T, 0);
      end if;

      Unlock (T);
   end Abort_One_Task;

   -----------------
   -- Abort_Tasks --
   -----------------

   --  Compiler interface only: Do not call from within the RTS,

   --  except in the implementation of Ada.Task_Identification.
   --  This must be called to implement the abort statement.
   --  Much of the actual work of the abort is done by the abortee,
   --  via the Abort_Handler signal handler, and propagation of the
   --  Abort_Signal special exception.

   procedure Abort_Tasks (Tasks : Task_List) is
      Self_Id : constant Task_ID := STPO.Self;
      C       : Task_ID;
      P       : Task_ID;

   begin
      --  ????
      --  Since this is a "potentially blocking operation", we should
      --  add a separate check here that we are not inside a protected
      --  operation.

      Defer_Abort_Nestable (Self_Id);

      --  ?????
      --  Really should not be nested deferral here.
      --  Patch for code generation error that defers abort before
      --  evaluating parameters of an entry call (at least, timed entry
      --  calls), and so may propagate an exception that causes abort
      --  to remain undeferred indefinitely.  See C97404B.  When all
      --  such bugs are fixed, this patch can be removed.

      for J in Tasks'Range loop
         C := Tasks (J);
         Abort_One_Task (Self_Id, C);
      end loop;

      Lock_All_Tasks_List;
      C := All_Tasks_List;

      while C /= null loop
         if C.Pending_ATC_Level > 0 then
            P := C.Common.Parent;

            while P /= null loop
               if P.Pending_ATC_Level = 0 then
                  Abort_One_Task (Self_Id, C);
                  exit;
               end if;

               P := P.Common.Parent;
            end loop;
         end if;

         C := C.Common.All_Tasks_Link;
      end loop;

      Unlock_All_Tasks_List;
      Undefer_Abort_Nestable (Self_Id);
   end Abort_Tasks;

   -------------------------------
   -- Cancel_Queued_Entry_Calls --
   -------------------------------

   --  Cancel any entry calls queued on target task. Call this only while
   --  holding T locked, and nothing more. This should only be called by T,
   --  unless T is a terminated previously unactivated task.

   procedure Cancel_Queued_Entry_Calls (T : Task_ID) is
      Next_Entry_Call : Entry_Call_Link;
      Entry_Call      : Entry_Call_Link;
      Caller          : Task_ID;
      Level           : Integer;
      Self_Id         : constant Task_ID := STPO.Self;

   begin
      pragma Assert (T = Self or else T.Common.State = Terminated);

      for J in 1 .. T.Entry_Num loop
         Queuing.Dequeue_Head (T.Entry_Queues (J), Entry_Call);

         while Entry_Call /= null loop

            --  Leave Entry_Call.Done = False, since this is cancelled

            Caller := Entry_Call.Self;
            Entry_Call.Exception_To_Raise := Tasking_Error'Identity;
            Queuing.Dequeue_Head (T.Entry_Queues (J), Next_Entry_Call);
            Level := Entry_Call.Level - 1;
            Unlock (T);
            Write_Lock (Entry_Call.Self);
            Wakeup_Entry_Caller (Self_Id, Entry_Call, Cancelled);
            Unlock (Entry_Call.Self);
            Write_Lock (T);
            Entry_Call.State := Done;
            Entry_Call := Next_Entry_Call;
         end loop;
      end loop;
   end Cancel_Queued_Entry_Calls;

   ------------------------
   -- Exit_One_ATC_Level --
   ------------------------

   --  Call only with abort deferred and holding lock of Self_Id.
   --  This is a bit of common code for all entry calls.
   --  The effect is to exit one level of ATC nesting.

   --  If we have reached the desired ATC nesting level, reset the
   --  requested level to effective infinity, to allow further calls.
   --  In any case, reset Self_Id.Aborting, to allow re-raising of
   --  Abort_Signal.

   procedure Exit_One_ATC_Level (Self_ID : Task_ID) is
   begin
      Self_ID.ATC_Nesting_Level := Self_ID.ATC_Nesting_Level - 1;

      pragma Debug
        (Debug.Trace (Self_ID, "EOAL: exited to ATC level: " &
         ATC_Level'Image (Self_ID.ATC_Nesting_Level), 'A'));

      pragma Assert (Self_ID.ATC_Nesting_Level >= 1);

      if Self_ID.Pending_ATC_Level < ATC_Level_Infinity then
         if Self_ID.Pending_ATC_Level = Self_ID.ATC_Nesting_Level then
            Self_ID.Pending_ATC_Level := ATC_Level_Infinity;
            Self_ID.Aborting := False;
         else
            --  Force the next Undefer_Abort to re-raise Abort_Signal

            pragma Assert
             (Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level);

            if Self_ID.Aborting then
               Self_ID.ATC_Hack := True;
               Self_ID.Pending_Action := True;
            end if;
         end if;
      end if;
   end Exit_One_ATC_Level;

   ----------------------
   -- Make_Independent --
   ----------------------

   --  Move the current task to the outermost level (level 2) of the master
   --  hierarchy of the environment task. That is one level further out
   --  than normal tasks defined in library-level packages (level 3). The
   --  environment task will wait for level 3 tasks to terminate normally,
   --  then it will abort all the level 2 tasks. See Finalize_Global_Tasks
   --  procedure for more information.

   --  This is a dangerous operation, and should only be used on nested tasks
   --  or tasks that depend on any objects that might be finalized earlier than
   --  the termination of the environment task. It is for internal use by the
   --  GNARL, to prevent such internal server tasks from preventing a partition
   --  from terminating.

   --  Also note that the run time assumes that the parent of an independent
   --  task is the environment task. If this is not the case, Make_Independent
   --  will change the task's parent. This assumption is particularly
   --  important for master level completion and for the computation of
   --  Independent_Task_Count.

   --  See procedures Init_RTS and Finalize_Global_Tasks for related code.

   procedure Make_Independent is
      Self_Id               : constant Task_ID := STPO.Self;
      Environment_Task      : constant Task_ID := STPO.Environment_Task;
      Parent                : constant Task_ID := Self_Id.Common.Parent;
      Parent_Needs_Updating : Boolean := False;

   begin
      if Self_Id.Known_Tasks_Index /= -1 then
         Known_Tasks (Self_Id.Known_Tasks_Index) := null;
      end if;

      Defer_Abort (Self_Id);
      Write_Lock (Environment_Task);
      Write_Lock (Self_Id);

      pragma Assert (Parent = Environment_Task
        or else Self_Id.Master_of_Task = Library_Task_Level);

      Self_Id.Master_of_Task := Independent_Task_Level;

      --  The run time assumes that the parent of an independent task is the
      --  environment task.

      if Parent /= Environment_Task then

         --  We can not lock three tasks at the same time, so defer the
         --  operations on the parent.

         Parent_Needs_Updating := True;
         Self_Id.Common.Parent := Environment_Task;
      end if;

      --  Update Independent_Task_Count that is needed for the GLADE
      --  termination rule. See also pending update in
      --  System.Tasking.Stages.Check_Independent

      Independent_Task_Count := Independent_Task_Count + 1;

      Unlock (Self_Id);

      --  Changing the parent after creation is not trivial. Do not forget
      --  to update the old parent counts, and the new parent (i.e. the
      --  Environment_Task) counts.

      if Parent_Needs_Updating then
         Write_Lock (Parent);
         Parent.Awake_Count := Parent.Awake_Count - 1;
         Parent.Alive_Count := Parent.Alive_Count - 1;
         Environment_Task.Awake_Count := Environment_Task.Awake_Count + 1;
         Environment_Task.Alive_Count := Environment_Task.Alive_Count + 1;
         Unlock (Parent);
      end if;

      Unlock (Environment_Task);
      Undefer_Abort (Self_Id);
   end Make_Independent;

   ------------------
   -- Make_Passive --
   ------------------

   --  Update counts to indicate current task is either terminated
   --  or accepting on a terminate alternative. Call holding no locks.

   procedure Make_Passive
     (Self_ID        : Task_ID;
      Task_Completed : Boolean)
   is
      C : Task_ID := Self_ID;
      P : Task_ID := C.Common.Parent;

      Master_Completion_Phase : Integer;

   begin
      if P /= null then
         Write_Lock (P);
      end if;

      Write_Lock (C);

      if Task_Completed then
         Self_ID.Common.State := Terminated;

         if Self_ID.Awake_Count = 0 then

            --  We are completing via a terminate alternative.
            --  Our parent should wait in Phase 2 of Complete_Master.

            Master_Completion_Phase := 2;

            pragma Assert (Task_Completed);
            pragma Assert (Self_ID.Terminate_Alternative);
            pragma Assert (Self_ID.Alive_Count = 1);

         else
            --  We are NOT on a terminate alternative.
            --  Our parent should wait in Phase 1 of Complete_Master.

            Master_Completion_Phase := 1;
            pragma Assert (Self_ID.Awake_Count = 1);
         end if;

      --  We are accepting with a terminate alternative.

      else
         if Self_ID.Open_Accepts = null then

            --  Somebody started a rendezvous while we had our lock open.
            --  Skip the terminate alternative.

            Unlock (C);

            if P /= null then
               Unlock (P);
            end if;

            return;
         end if;

         Self_ID.Terminate_Alternative := True;
         Master_Completion_Phase := 0;

         pragma Assert (Self_ID.Terminate_Alternative);
         pragma Assert (Self_ID.Awake_Count >= 1);
      end if;

      if Master_Completion_Phase = 2 then

         --  Since our Awake_Count is zero but our Alive_Count
         --  is nonzero, we have been accepting with a terminate
         --  alternative, and we now have been told to terminate
         --  by a completed master (in some ancestor task) that
         --  is waiting (with zero Awake_Count) in Phase 2 of
         --  Complete_Master.

         pragma Debug
           (Debug.Trace (Self_ID, "Make_Passive: Phase 2", 'M'));

         pragma Assert (P /= null);

         C.Alive_Count := C.Alive_Count - 1;

         if C.Alive_Count > 0 then
            Unlock (C);
            Unlock (P);
            return;
         end if;

         --  C's count just went to zero, indicating that
         --  all of C's dependents are terminated.
         --  C has a parent, P.

         loop
            --  C's count just went to zero, indicating that all of C's
            --  dependents are terminated. C has a parent, P. Notify P that
            --  C and its dependents have all terminated.

            P.Alive_Count := P.Alive_Count - 1;
            exit when P.Alive_Count > 0;
            Unlock (C);
            Unlock (P);
            C := P;
            P := C.Common.Parent;

            --  Environment task cannot have terminated yet

            pragma Assert (P /= null);

            Write_Lock (P);
            Write_Lock (C);
         end loop;

         pragma Assert (P.Awake_Count /= 0);

         if P.Common.State = Master_Phase_2_Sleep
           and then C.Master_of_Task = P.Master_Within

         then
            pragma Assert (P.Common.Wait_Count > 0);
            P.Common.Wait_Count := P.Common.Wait_Count - 1;

            if P.Common.Wait_Count = 0 then
               Wakeup (P, Master_Phase_2_Sleep);
            end if;
         end if;

         Unlock (C);
         Unlock (P);
         return;
      end if;

      --  We are terminating in Phase 1 or Complete_Master,
      --  or are accepting on a terminate alternative.

      C.Awake_Count := C.Awake_Count - 1;

      if Task_Completed then
         pragma Assert (Self_ID.Awake_Count = 0);
         C.Alive_Count := C.Alive_Count - 1;
      end if;

      if C.Awake_Count > 0 or else P = null then
         Unlock (C);

         if P /= null then
            Unlock (P);
         end if;

         return;
      end if;

      --  C's count just went to zero, indicating that all of C's
      --  dependents are terminated or accepting with terminate alt.
      --  C has a parent, P.

      loop
         --  Notify P that C has gone passive.

         P.Awake_Count := P.Awake_Count - 1;

         if Task_Completed and then C.Alive_Count = 0 then
            P.Alive_Count := P.Alive_Count - 1;
         end if;

         exit when P.Awake_Count > 0;
         Unlock (C);
         Unlock (P);
         C := P;
         P := C.Common.Parent;

         if P = null then
            return;
         end if;

         Write_Lock (P);
         Write_Lock (C);
      end loop;

      --  P has non-passive dependents.

      if P.Common.State = Master_Completion_Sleep and then
         C.Master_of_Task = P.Master_Within
      then
         pragma Debug
           (Debug.Trace
            (Self_ID, "Make_Passive: Phase 1, parent waiting", 'M'));

         --  If parent is in Master_Completion_Sleep, it
         --  cannot be on a terminate alternative, hence
         --  it cannot have Awake_Count of zero.

         pragma Assert (P.Common.Wait_Count > 0);
         P.Common.Wait_Count := P.Common.Wait_Count - 1;

         if P.Common.Wait_Count = 0 then
            Wakeup (P, Master_Completion_Sleep);
         end if;

      else
         pragma Debug
           (Debug.Trace
             (Self_ID, "Make_Passive: Phase 1, parent awake", 'M'));
         null;
      end if;

      Unlock (C);
      Unlock (P);
   end Make_Passive;

end System.Tasking.Utilities;
