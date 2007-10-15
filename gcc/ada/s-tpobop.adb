------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S .    --
--                             O P E R A T I O N S                          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1998-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This package contains all the extended primitives related to
--  Protected_Objects with entries.

--  The handling of protected objects with no entries is done in
--  System.Tasking.Protected_Objects, the simple routines for protected
--  objects with entries in System.Tasking.Protected_Objects.Entries.

--  The split between Entries and Operations is needed to break circular
--  dependencies inside the run time.

--  This package contains all primitives related to Protected_Objects.
--  Note: the compiler generates direct calls to this interface, via Rtsfind.

with System.Task_Primitives.Operations;
--  used for Initialize_Lock
--           Write_Lock
--           Unlock
--           Get_Priority
--           Wakeup

with System.Tasking.Entry_Calls;
--  used for Wait_For_Completion
--           Wait_Until_Abortable
--           Wait_For_Completion_With_Timeout

with System.Tasking.Initialization;
--  Used for Defer_Abort,
--           Undefer_Abort,
--           Change_Base_Priority

pragma Elaborate_All (System.Tasking.Initialization);
--  This insures that tasking is initialized if any protected objects are
--  created.

with System.Tasking.Queuing;
--  used for Enqueue
--           Broadcast_Program_Error
--           Select_Protected_Entry_Call
--           Onqueue
--           Count_Waiting

with System.Tasking.Rendezvous;
--  used for Task_Do_Or_Queue

with System.Tasking.Utilities;
--  used for Exit_One_ATC_Level

with System.Tasking.Debug;
--  used for Trace

with System.Parameters;
--  used for Single_Lock
--           Runtime_Traces

with System.Traces.Tasking;
--  used for Send_Trace_Info

with System.Restrictions;
--  used for Run_Time_Restrictions

package body System.Tasking.Protected_Objects.Operations is

   package STPO renames System.Task_Primitives.Operations;

   use Parameters;
   use Task_Primitives;
   use Ada.Exceptions;
   use Entries;

   use System.Restrictions;
   use System.Restrictions.Rident;
   use System.Traces;
   use System.Traces.Tasking;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Update_For_Queue_To_PO
     (Entry_Call : Entry_Call_Link;
      With_Abort : Boolean);
   pragma Inline (Update_For_Queue_To_PO);
   --  Update the state of an existing entry call to reflect the fact that it
   --  is being enqueued, based on whether the current queuing action is with
   --  or without abort. Call this only while holding the PO's lock. It returns
   --  with the PO's lock still held.

   procedure Requeue_Call
     (Self_Id    : Task_Id;
      Object     : Protection_Entries_Access;
      Entry_Call : Entry_Call_Link);
   --  Handle requeue of Entry_Call.
   --  In particular, queue the call if needed, or service it immediately
   --  if possible.

   ---------------------------------
   -- Cancel_Protected_Entry_Call --
   ---------------------------------

   --  Compiler interface only (do not call from within the RTS)

   --  This should have analogous effect to Cancel_Task_Entry_Call, setting
   --  the value of Block.Cancelled instead of returning the parameter value
   --  Cancelled.

   --  The effect should be idempotent, since the call may already have been
   --  dequeued.

   --  Source code:

   --      select r.e;
   --         ...A...
   --      then abort
   --         ...B...
   --      end select;

   --  Expanded code:

   --      declare
   --         X : protected_entry_index := 1;
   --         B80b : communication_block;
   --         communication_blockIP (B80b);

   --      begin
   --         begin
   --            A79b : label
   --            A79b : declare
   --               procedure _clean is
   --               begin
   --                  if enqueued (B80b) then
   --                     cancel_protected_entry_call (B80b);
   --                  end if;
   --                  return;
   --               end _clean;

   --            begin
   --               protected_entry_call (rTV!(r)._object'unchecked_access, X,
   --                 null_address, asynchronous_call, B80b, objectF => 0);
   --               if enqueued (B80b) then
   --                  ...B...
   --               end if;
   --            at end
   --               _clean;
   --            end A79b;

   --         exception
   --            when _abort_signal =>
   --               abort_undefer.all;
   --               null;
   --         end;

   --         if not cancelled (B80b) then
   --            x := ...A...
   --         end if;
   --      end;

   --  If the entry call completes after we get into the abortable part,
   --  Abort_Signal should be raised and ATC will take us to the at-end
   --  handler, which will call _clean.

   --  If the entry call returns with the call already completed, we can skip
   --  this, and use the "if enqueued()" to go past the at-end handler, but we
   --  will still call _clean.

   --  If the abortable part completes before the entry call is Done, it will
   --  call _clean.

   --  If the entry call or the abortable part raises an exception,
   --  we will still call _clean, but the value of Cancelled should not matter.

   --  Whoever calls _clean first gets to decide whether the call
   --  has been "cancelled".

   --  Enqueued should be true if there is any chance that the call is still on
   --  a queue. It seems to be safe to make it True if the call was Onqueue at
   --  some point before return from Protected_Entry_Call.

   --  Cancelled should be true iff the abortable part completed
   --  and succeeded in cancelling the entry call before it completed.

   --  ?????
   --  The need for Enqueued is less obvious. The "if enqueued ()" tests are
   --  not necessary, since Cancel_Protected_Entry_Call/Protected_Entry_Call
   --  must do the same test internally, with locking. The one that makes
   --  cancellation conditional may be a useful heuristic since at least 1/2
   --  the time the call should be off-queue by that point. The other one seems
   --  totally useless, since Protected_Entry_Call must do the same check and
   --  then possibly wait for the call to be abortable, internally.

   --  We can check Call.State here without locking the caller's mutex,
   --  since the call must be over after returning from Wait_For_Completion.
   --  No other task can access the call record at this point.

   procedure Cancel_Protected_Entry_Call
     (Block : in out Communication_Block) is
   begin
      Entry_Calls.Try_To_Cancel_Entry_Call (Block.Cancelled);
   end Cancel_Protected_Entry_Call;

   ---------------
   -- Cancelled --
   ---------------

   function Cancelled (Block : Communication_Block) return Boolean is
   begin
      return Block.Cancelled;
   end Cancelled;

   -------------------------
   -- Complete_Entry_Body --
   -------------------------

   procedure Complete_Entry_Body (Object : Protection_Entries_Access) is
   begin
      Exceptional_Complete_Entry_Body (Object, Ada.Exceptions.Null_Id);
   end Complete_Entry_Body;

   --------------
   -- Enqueued --
   --------------

   function Enqueued (Block : Communication_Block) return Boolean is
   begin
      return Block.Enqueued;
   end Enqueued;

   -------------------------------------
   -- Exceptional_Complete_Entry_Body --
   -------------------------------------

   procedure Exceptional_Complete_Entry_Body
     (Object : Protection_Entries_Access;
      Ex     : Ada.Exceptions.Exception_Id)
   is
      procedure Transfer_Occurrence
        (Target : Ada.Exceptions.Exception_Occurrence_Access;
         Source : Ada.Exceptions.Exception_Occurrence);
      pragma Import (C, Transfer_Occurrence, "__gnat_transfer_occurrence");

      Entry_Call : constant Entry_Call_Link := Object.Call_In_Progress;
      Self_Id    : Task_Id;

   begin
      pragma Debug
       (Debug.Trace (STPO.Self, "Exceptional_Complete_Entry_Body", 'P'));

      --  We must have abort deferred, since we are inside a protected
      --  operation.

      if Entry_Call /= null then

         --  The call was not requeued

         Entry_Call.Exception_To_Raise := Ex;

         if Ex /= Ada.Exceptions.Null_Id then

            --  An exception was raised and abort was deferred, so adjust
            --  before propagating, otherwise the task will stay with deferral
            --  enabled for its remaining life.

            Self_Id := STPO.Self;
            Initialization.Undefer_Abort_Nestable (Self_Id);
            Transfer_Occurrence
              (Entry_Call.Self.Common.Compiler_Data.Current_Excep'Access,
               Self_Id.Common.Compiler_Data.Current_Excep);
         end if;

         --  Wakeup_Entry_Caller will be called from PO_Do_Or_Queue or
         --  PO_Service_Entries on return.

      end if;

      if Runtime_Traces then
         Send_Trace_Info (PO_Done, Entry_Call.Self);
      end if;
   end Exceptional_Complete_Entry_Body;

   --------------------
   -- PO_Do_Or_Queue --
   --------------------

   procedure PO_Do_Or_Queue
     (Self_ID    : Task_Id;
      Object     : Protection_Entries_Access;
      Entry_Call : Entry_Call_Link)
   is
      E             : constant Protected_Entry_Index :=
                        Protected_Entry_Index (Entry_Call.E);
      Barrier_Value : Boolean;

   begin
      --  When the Action procedure for an entry body returns, it is either
      --  completed (having called [Exceptional_]Complete_Entry_Body) or it
      --  is queued, having executed a requeue statement.

      Barrier_Value :=
        Object.Entry_Bodies (
          Object.Find_Body_Index (Object.Compiler_Info, E)).
            Barrier (Object.Compiler_Info, E);

      if Barrier_Value then

         --  Not abortable while service is in progress

         if Entry_Call.State = Now_Abortable then
            Entry_Call.State := Was_Abortable;
         end if;

         Object.Call_In_Progress := Entry_Call;

         pragma Debug
          (Debug.Trace (Self_ID, "PODOQ: start entry body", 'P'));
         Object.Entry_Bodies (
           Object.Find_Body_Index (Object.Compiler_Info, E)).Action (
             Object.Compiler_Info, Entry_Call.Uninterpreted_Data, E);

         if Object.Call_In_Progress /= null then

            --  Body of current entry served call to completion

            Object.Call_In_Progress := null;

            if Single_Lock then
               STPO.Lock_RTS;
            end if;

            STPO.Write_Lock (Entry_Call.Self);
            Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Done);
            STPO.Unlock (Entry_Call.Self);

            if Single_Lock then
               STPO.Unlock_RTS;
            end if;

         else
            Requeue_Call (Self_ID, Object, Entry_Call);
         end if;

      elsif Entry_Call.Mode /= Conditional_Call
        or else not Entry_Call.With_Abort
      then

         if Run_Time_Restrictions.Set (Max_Entry_Queue_Length)
              and then
            Run_Time_Restrictions.Value (Max_Entry_Queue_Length) <=
              Queuing.Count_Waiting (Object.Entry_Queues (E))
         then
            --  This violates the Max_Entry_Queue_Length restriction,
            --  raise Program_Error.

            Entry_Call.Exception_To_Raise := Program_Error'Identity;

            if Single_Lock then
               STPO.Lock_RTS;
            end if;

            STPO.Write_Lock (Entry_Call.Self);
            Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Done);
            STPO.Unlock (Entry_Call.Self);

            if Single_Lock then
               STPO.Unlock_RTS;
            end if;
         else
            Queuing.Enqueue (Object.Entry_Queues (E), Entry_Call);
            Update_For_Queue_To_PO (Entry_Call, Entry_Call.With_Abort);
         end if;
      else
         --  Conditional_Call and With_Abort

         if Single_Lock then
            STPO.Lock_RTS;
         end if;

         STPO.Write_Lock (Entry_Call.Self);
         pragma Assert (Entry_Call.State >= Was_Abortable);
         Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Cancelled);
         STPO.Unlock (Entry_Call.Self);

         if Single_Lock then
            STPO.Unlock_RTS;
         end if;
      end if;

   exception
      when others =>
         Queuing.Broadcast_Program_Error (Self_ID, Object, Entry_Call);
   end PO_Do_Or_Queue;

   ------------------------
   -- PO_Service_Entries --
   ------------------------

   procedure PO_Service_Entries
     (Self_ID       : Task_Id;
      Object        : Entries.Protection_Entries_Access;
      Unlock_Object : Boolean := True)
   is
      E          : Protected_Entry_Index;
      Caller     : Task_Id;
      Entry_Call : Entry_Call_Link;

   begin
      loop
         Queuing.Select_Protected_Entry_Call (Self_ID, Object, Entry_Call);

         exit when Entry_Call = null;

         E := Protected_Entry_Index (Entry_Call.E);

         --  Not abortable while service is in progress

         if Entry_Call.State = Now_Abortable then
            Entry_Call.State := Was_Abortable;
         end if;

         Object.Call_In_Progress := Entry_Call;

         begin
            if Runtime_Traces then
               Send_Trace_Info (PO_Run, Self_ID,
                                Entry_Call.Self, Entry_Index (E));
            end if;

            pragma Debug
              (Debug.Trace (Self_ID, "POSE: start entry body", 'P'));

            Object.Entry_Bodies
              (Object.Find_Body_Index (Object.Compiler_Info, E)).Action
                (Object.Compiler_Info, Entry_Call.Uninterpreted_Data, E);

         exception
            when others =>
               Queuing.Broadcast_Program_Error
                 (Self_ID, Object, Entry_Call);
         end;

         if Object.Call_In_Progress = null then
            Requeue_Call (Self_ID, Object, Entry_Call);
            exit when Entry_Call.State = Cancelled;

         else
            Object.Call_In_Progress := null;
            Caller := Entry_Call.Self;

            if Single_Lock then
               STPO.Lock_RTS;
            end if;

            STPO.Write_Lock (Caller);
            Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Done);
            STPO.Unlock (Caller);

            if Single_Lock then
               STPO.Unlock_RTS;
            end if;
         end if;
      end loop;

      if Unlock_Object then
         Unlock_Entries (Object);
      end if;
   end PO_Service_Entries;

   ---------------------
   -- Protected_Count --
   ---------------------

   function Protected_Count
     (Object : Protection_Entries'Class;
      E      : Protected_Entry_Index) return Natural
   is
   begin
      return Queuing.Count_Waiting (Object.Entry_Queues (E));
   end Protected_Count;

   --------------------------
   -- Protected_Entry_Call --
   --------------------------

   --  Compiler interface only (do not call from within the RTS)

   --  select r.e;
   --     ...A...
   --  else
   --     ...B...
   --  end select;

   --  declare
   --     X : protected_entry_index := 1;
   --     B85b : communication_block;
   --     communication_blockIP (B85b);

   --  begin
   --     protected_entry_call (rTV!(r)._object'unchecked_access, X,
   --       null_address, conditional_call, B85b, objectF => 0);

   --     if cancelled (B85b) then
   --        ...B...
   --     else
   --        ...A...
   --     end if;
   --  end;

   --  See also Cancel_Protected_Entry_Call for code expansion of asynchronous
   --  entry call.

   --  The initial part of this procedure does not need to lock the the calling
   --  task's ATCB, up to the point where the call record first may be queued
   --  (PO_Do_Or_Queue), since before that no other task will have access to
   --  the record.

   --  If this is a call made inside of an abort deferred region, the call
   --  should be never abortable.

   --  If the call was not queued abortably, we need to wait until it is before
   --  proceeding with the abortable part.

   --  There are some heuristics here, just to save time for frequently
   --  occurring cases. For example, we check Initially_Abortable to try to
   --  avoid calling the procedure Wait_Until_Abortable, since the normal case
   --  for async.  entry calls is to be queued abortably.

   --  Another heuristic uses the Block.Enqueued to try to avoid calling
   --  Cancel_Protected_Entry_Call if the call can be served immediately.

   procedure Protected_Entry_Call
     (Object              : Protection_Entries_Access;
      E                   : Protected_Entry_Index;
      Uninterpreted_Data  : System.Address;
      Mode                : Call_Modes;
      Block               : out Communication_Block)
   is
      Self_ID             : constant Task_Id := STPO.Self;
      Entry_Call          : Entry_Call_Link;
      Initially_Abortable : Boolean;
      Ceiling_Violation   : Boolean;

   begin
      pragma Debug
        (Debug.Trace (Self_ID, "Protected_Entry_Call", 'P'));

      if Runtime_Traces then
         Send_Trace_Info (PO_Call, Entry_Index (E));
      end if;

      if Self_ID.ATC_Nesting_Level = ATC_Level'Last then
         Raise_Exception
           (Storage_Error'Identity, "not enough ATC nesting levels");
      end if;

      --  If pragma Detect_Blocking is active then Program_Error must be
      --  raised if this potentially blocking operation is called from a
      --  protected action.

      if Detect_Blocking
        and then Self_ID.Common.Protected_Action_Nesting > 0
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity, "potentially blocking operation");
      end if;

      --  Self_ID.Deferral_Level should be 0, except when called from Finalize,
      --  where abort is already deferred.

      Initialization.Defer_Abort_Nestable (Self_ID);
      Lock_Entries (Object, Ceiling_Violation);

      if Ceiling_Violation then

         --  Failed ceiling check

         Initialization.Undefer_Abort_Nestable (Self_ID);
         raise Program_Error;
      end if;

      Block.Self := Self_ID;
      Self_ID.ATC_Nesting_Level := Self_ID.ATC_Nesting_Level + 1;
      pragma Debug
        (Debug.Trace (Self_ID, "PEC: entered ATC level: " &
         ATC_Level'Image (Self_ID.ATC_Nesting_Level), 'A'));
      Entry_Call :=
         Self_ID.Entry_Calls (Self_ID.ATC_Nesting_Level)'Access;
      Entry_Call.Next := null;
      Entry_Call.Mode := Mode;
      Entry_Call.Cancellation_Attempted := False;

      if Self_ID.Deferral_Level > 1 then
         Entry_Call.State := Never_Abortable;
      else
         Entry_Call.State := Now_Abortable;
      end if;

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := STPO.Get_Priority (Self_ID);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_PO := To_Address (Object);
      Entry_Call.Called_Task := null;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;
      Entry_Call.With_Abort := True;

      PO_Do_Or_Queue (Self_ID, Object, Entry_Call);
      Initially_Abortable := Entry_Call.State = Now_Abortable;
      PO_Service_Entries (Self_ID, Object);

      --  Try to prevent waiting later (in Try_To_Cancel_Protected_Entry_Call)
      --  for completed or cancelled calls.  (This is a heuristic, only.)

      if Entry_Call.State >= Done then

         --  Once State >= Done it will not change any more

         if Single_Lock then
            STPO.Lock_RTS;
         end if;

         STPO.Write_Lock (Self_ID);
         Utilities.Exit_One_ATC_Level (Self_ID);
         STPO.Unlock (Self_ID);

         if Single_Lock then
            STPO.Unlock_RTS;
         end if;

         Block.Enqueued := False;
         Block.Cancelled := Entry_Call.State = Cancelled;
         Initialization.Undefer_Abort_Nestable (Self_ID);
         Entry_Calls.Check_Exception (Self_ID, Entry_Call);
         return;

      else
         --  In this case we cannot conclude anything, since State can change
         --  concurrently.

         null;
      end if;

      --  Now for the general case

      if Mode = Asynchronous_Call then

         --  Try to avoid an expensive call

         if not Initially_Abortable then
            if Single_Lock then
               STPO.Lock_RTS;
               Entry_Calls.Wait_Until_Abortable (Self_ID, Entry_Call);
               STPO.Unlock_RTS;
            else
               Entry_Calls.Wait_Until_Abortable (Self_ID, Entry_Call);
            end if;
         end if;

      elsif Mode < Asynchronous_Call then

         --  Simple_Call or Conditional_Call

         if Single_Lock then
            STPO.Lock_RTS;
            Entry_Calls.Wait_For_Completion (Entry_Call);
            STPO.Unlock_RTS;

         else
            STPO.Write_Lock (Self_ID);
            Entry_Calls.Wait_For_Completion (Entry_Call);
            STPO.Unlock (Self_ID);
         end if;

         Block.Cancelled := Entry_Call.State = Cancelled;

      else
         pragma Assert (False);
         null;
      end if;

      Initialization.Undefer_Abort_Nestable (Self_ID);
      Entry_Calls.Check_Exception (Self_ID, Entry_Call);
   end Protected_Entry_Call;

   ------------------
   -- Requeue_Call --
   ------------------

   procedure Requeue_Call
     (Self_Id    : Task_Id;
      Object     : Protection_Entries_Access;
      Entry_Call : Entry_Call_Link)
   is
      New_Object        : Protection_Entries_Access;
      Ceiling_Violation : Boolean;
      Result            : Boolean;
      E                 : Protected_Entry_Index;

   begin
      New_Object := To_Protection (Entry_Call.Called_PO);

      if New_Object = null then

         --  Call is to be requeued to a task entry

         if Single_Lock then
            STPO.Lock_RTS;
         end if;

         Result := Rendezvous.Task_Do_Or_Queue (Self_Id, Entry_Call);

         if not Result then
            Queuing.Broadcast_Program_Error
              (Self_Id, Object, Entry_Call, RTS_Locked => True);
         end if;

         if Single_Lock then
            STPO.Unlock_RTS;
         end if;

      else
         --  Call should be requeued to a PO

         if Object /= New_Object then

            --  Requeue is to different PO

            Lock_Entries (New_Object, Ceiling_Violation);

            if Ceiling_Violation then
               Object.Call_In_Progress := null;
               Queuing.Broadcast_Program_Error (Self_Id, Object, Entry_Call);

            else
               PO_Do_Or_Queue (Self_Id, New_Object, Entry_Call);
               PO_Service_Entries (Self_Id, New_Object);
            end if;

         else
            --  Requeue is to same protected object

            --  ??? Try to compensate apparent failure of the scheduler on some
            --  OS (e.g VxWorks) to give higher priority tasks a chance to run
            --  (see CXD6002).

            STPO.Yield (False);

            if Entry_Call.With_Abort
              and then Entry_Call.Cancellation_Attempted
            then
               --  If this is a requeue with abort and someone tried to cancel
               --  this call, cancel it at this point.

               Entry_Call.State := Cancelled;
               return;
            end if;

            if not Entry_Call.With_Abort
              or else Entry_Call.Mode /= Conditional_Call
            then
               E := Protected_Entry_Index (Entry_Call.E);

               if Run_Time_Restrictions.Set (Max_Entry_Queue_Length)
                    and then
                  Run_Time_Restrictions.Value (Max_Entry_Queue_Length) <=
                    Queuing.Count_Waiting (Object.Entry_Queues (E))
               then
                  --  This violates the Max_Entry_Queue_Length restriction,
                  --  raise Program_Error.

                  Entry_Call.Exception_To_Raise := Program_Error'Identity;

                  if Single_Lock then
                     STPO.Lock_RTS;
                  end if;

                  STPO.Write_Lock (Entry_Call.Self);
                  Initialization.Wakeup_Entry_Caller
                    (Self_Id, Entry_Call, Done);
                  STPO.Unlock (Entry_Call.Self);

                  if Single_Lock then
                     STPO.Unlock_RTS;
                  end if;

               else
                  Queuing.Enqueue
                    (New_Object.Entry_Queues (E), Entry_Call);
                  Update_For_Queue_To_PO (Entry_Call, Entry_Call.With_Abort);
               end if;

            else
               PO_Do_Or_Queue (Self_Id, New_Object, Entry_Call);
            end if;
         end if;
      end if;
   end Requeue_Call;

   ----------------------------
   -- Protected_Entry_Caller --
   ----------------------------

   function Protected_Entry_Caller
     (Object : Protection_Entries'Class) return Task_Id is
   begin
      return Object.Call_In_Progress.Self;
   end Protected_Entry_Caller;

   -----------------------------
   -- Requeue_Protected_Entry --
   -----------------------------

   --  Compiler interface only (do not call from within the RTS)

   --  entry e when b is
   --  begin
   --     b := false;
   --     ...A...
   --     requeue e2;
   --  end e;

   --  procedure rPT__E10b (O : address; P : address; E :
   --    protected_entry_index) is
   --     type rTVP is access rTV;
   --     freeze rTVP []
   --     _object : rTVP := rTVP!(O);
   --  begin
   --     declare
   --        rR : protection renames _object._object;
   --        vP : integer renames _object.v;
   --        bP : boolean renames _object.b;
   --     begin
   --        b := false;
   --        ...A...
   --        requeue_protected_entry (rR'unchecked_access, rR'
   --          unchecked_access, 2, false, objectF => 0, new_objectF =>
   --          0);
   --        return;
   --     end;
   --     complete_entry_body (_object._object'unchecked_access, objectF =>
   --       0);
   --     return;
   --  exception
   --     when others =>
   --        abort_undefer.all;
   --        exceptional_complete_entry_body (_object._object'
   --          unchecked_access, current_exception, objectF => 0);
   --        return;
   --  end rPT__E10b;

   procedure Requeue_Protected_Entry
     (Object     : Protection_Entries_Access;
      New_Object : Protection_Entries_Access;
      E          : Protected_Entry_Index;
      With_Abort : Boolean)
   is
      Entry_Call : constant Entry_Call_Link := Object.Call_In_Progress;

   begin
      pragma Debug
        (Debug.Trace (STPO.Self, "Requeue_Protected_Entry", 'P'));
      pragma Assert (STPO.Self.Deferral_Level > 0);

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Called_PO := To_Address (New_Object);
      Entry_Call.Called_Task := null;
      Entry_Call.With_Abort := With_Abort;
      Object.Call_In_Progress := null;
   end Requeue_Protected_Entry;

   -------------------------------------
   -- Requeue_Task_To_Protected_Entry --
   -------------------------------------

   --  Compiler interface only (do not call from within the RTS)

   --    accept e1 do
   --      ...A...
   --      requeue r.e2;
   --    end e1;

   --    A79b : address;
   --    L78b : label

   --    begin
   --       accept_call (1, A79b);
   --       ...A...
   --       requeue_task_to_protected_entry (rTV!(r)._object'
   --         unchecked_access, 2, false, new_objectF => 0);
   --       goto L78b;
   --       <<L78b>>
   --       complete_rendezvous;

   --    exception
   --       when all others =>
   --          exceptional_complete_rendezvous (get_gnat_exception);
   --    end;

   procedure Requeue_Task_To_Protected_Entry
     (New_Object : Protection_Entries_Access;
      E          : Protected_Entry_Index;
      With_Abort : Boolean)
   is
      Self_ID    : constant Task_Id := STPO.Self;
      Entry_Call : constant Entry_Call_Link := Self_ID.Common.Call;

   begin
      Initialization.Defer_Abort (Self_ID);

      --  We do not need to lock Self_ID here since the call is not abortable
      --  at this point, and therefore, the caller cannot cancel the call.

      Entry_Call.Needs_Requeue := True;
      Entry_Call.With_Abort := With_Abort;
      Entry_Call.Called_PO := To_Address (New_Object);
      Entry_Call.Called_Task := null;
      Entry_Call.E := Entry_Index (E);
      Initialization.Undefer_Abort (Self_ID);
   end Requeue_Task_To_Protected_Entry;

   ---------------------
   -- Service_Entries --
   ---------------------

   procedure Service_Entries (Object : Protection_Entries_Access) is
      Self_ID : constant Task_Id := STPO.Self;
   begin
      PO_Service_Entries (Self_ID, Object);
   end Service_Entries;

   --------------------------------
   -- Timed_Protected_Entry_Call --
   --------------------------------

   --  Compiler interface only (do not call from within the RTS)

   procedure Timed_Protected_Entry_Call
     (Object                : Protection_Entries_Access;
      E                     : Protected_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Timeout               : Duration;
      Mode                  : Delay_Modes;
      Entry_Call_Successful : out Boolean)
   is
      Self_Id           : constant Task_Id  := STPO.Self;
      Entry_Call        : Entry_Call_Link;
      Ceiling_Violation : Boolean;

      Yielded : Boolean;
      pragma Unreferenced (Yielded);

   begin
      if Self_Id.ATC_Nesting_Level = ATC_Level'Last then
         Raise_Exception (Storage_Error'Identity,
           "not enough ATC nesting levels");
      end if;

      --  If pragma Detect_Blocking is active then Program_Error must be
      --  raised if this potentially blocking operation is called from a
      --  protected action.

      if Detect_Blocking
        and then Self_Id.Common.Protected_Action_Nesting > 0
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity, "potentially blocking operation");
      end if;

      if Runtime_Traces then
         Send_Trace_Info (POT_Call, Entry_Index (E), Timeout);
      end if;

      Initialization.Defer_Abort (Self_Id);
      Lock_Entries (Object, Ceiling_Violation);

      if Ceiling_Violation then
         Initialization.Undefer_Abort (Self_Id);
         raise Program_Error;
      end if;

      Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level + 1;
      pragma Debug
        (Debug.Trace (Self_Id, "TPEC: exited to ATC level: " &
         ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));
      Entry_Call :=
        Self_Id.Entry_Calls (Self_Id.ATC_Nesting_Level)'Access;
      Entry_Call.Next := null;
      Entry_Call.Mode := Timed_Call;
      Entry_Call.Cancellation_Attempted := False;

      if Self_Id.Deferral_Level > 1 then
         Entry_Call.State := Never_Abortable;
      else
         Entry_Call.State := Now_Abortable;
      end if;

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := STPO.Get_Priority (Self_Id);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_PO := To_Address (Object);
      Entry_Call.Called_Task := null;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;
      Entry_Call.With_Abort := True;

      PO_Do_Or_Queue (Self_Id, Object, Entry_Call);
      PO_Service_Entries (Self_Id, Object);

      if Single_Lock then
         STPO.Lock_RTS;
      else
         STPO.Write_Lock (Self_Id);
      end if;

      --  Try to avoid waiting for completed or cancelled calls

      if Entry_Call.State >= Done then
         Utilities.Exit_One_ATC_Level (Self_Id);

         if Single_Lock then
            STPO.Unlock_RTS;
         else
            STPO.Unlock (Self_Id);
         end if;

         Entry_Call_Successful := Entry_Call.State = Done;
         Initialization.Undefer_Abort (Self_Id);
         Entry_Calls.Check_Exception (Self_Id, Entry_Call);
         return;
      end if;

      Entry_Calls.Wait_For_Completion_With_Timeout
        (Entry_Call, Timeout, Mode, Yielded);

      if Single_Lock then
         STPO.Unlock_RTS;
      else
         STPO.Unlock (Self_Id);
      end if;

      --  ??? Do we need to yield in case Yielded is False

      Initialization.Undefer_Abort (Self_Id);
      Entry_Call_Successful := Entry_Call.State = Done;
      Entry_Calls.Check_Exception (Self_Id, Entry_Call);
   end Timed_Protected_Entry_Call;

   ----------------------------
   -- Update_For_Queue_To_PO --
   ----------------------------

   --  Update the state of an existing entry call, based on
   --  whether the current queuing action is with or without abort.
   --  Call this only while holding the server's lock.
   --  It returns with the server's lock released.

   New_State : constant array (Boolean, Entry_Call_State)
     of Entry_Call_State :=
       (True =>
         (Never_Abortable   => Never_Abortable,
          Not_Yet_Abortable => Now_Abortable,
          Was_Abortable     => Now_Abortable,
          Now_Abortable     => Now_Abortable,
          Done              => Done,
          Cancelled         => Cancelled),
        False =>
         (Never_Abortable   => Never_Abortable,
          Not_Yet_Abortable => Not_Yet_Abortable,
          Was_Abortable     => Was_Abortable,
          Now_Abortable     => Now_Abortable,
          Done              => Done,
          Cancelled         => Cancelled)
       );

   procedure Update_For_Queue_To_PO
     (Entry_Call : Entry_Call_Link;
      With_Abort : Boolean)
   is
      Old : constant Entry_Call_State := Entry_Call.State;

   begin
      pragma Assert (Old < Done);

      Entry_Call.State := New_State (With_Abort, Entry_Call.State);

      if Entry_Call.Mode = Asynchronous_Call then
         if Old < Was_Abortable and then
           Entry_Call.State = Now_Abortable
         then
            if Single_Lock then
               STPO.Lock_RTS;
            end if;

            STPO.Write_Lock (Entry_Call.Self);

            if Entry_Call.Self.Common.State = Async_Select_Sleep then
               STPO.Wakeup (Entry_Call.Self, Async_Select_Sleep);
            end if;

            STPO.Unlock (Entry_Call.Self);

            if Single_Lock then
               STPO.Unlock_RTS;
            end if;

         end if;

      elsif Entry_Call.Mode = Conditional_Call then
         pragma Assert (Entry_Call.State < Was_Abortable);
         null;
      end if;
   end Update_For_Queue_To_PO;

end System.Tasking.Protected_Objects.Operations;
