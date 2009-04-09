------------------------------------------------------------------------------
--                                                                          --
--                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  --
--                                                                          --
--            S Y S T E M . T A S K I N G . R E N D E Z V O U S             --
--                                                                          --
--                                 B o d y                                  --
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

with System.Task_Primitives.Operations;
with System.Tasking.Entry_Calls;
with System.Tasking.Initialization;
with System.Tasking.Queuing;
with System.Tasking.Utilities;
with System.Tasking.Protected_Objects.Operations;
with System.Tasking.Debug;
with System.Restrictions;
with System.Parameters;
with System.Traces.Tasking;

package body System.Tasking.Rendezvous is

   package STPO renames System.Task_Primitives.Operations;
   package POO renames Protected_Objects.Operations;
   package POE renames Protected_Objects.Entries;

   use Parameters;
   use Task_Primitives.Operations;
   use System.Traces;
   use System.Traces.Tasking;

   type Select_Treatment is (
     Accept_Alternative_Selected,   --  alternative with non-null body
     Accept_Alternative_Completed,  --  alternative with null body
     Else_Selected,
     Terminate_Selected,
     Accept_Alternative_Open,
     No_Alternative_Open);

   ----------------
   -- Local Data --
   ----------------

   Default_Treatment : constant array (Select_Modes) of Select_Treatment :=
     (Simple_Mode         => No_Alternative_Open,
      Else_Mode           => Else_Selected,
      Terminate_Mode      => Terminate_Selected,
      Delay_Mode          => No_Alternative_Open);

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

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Local_Defer_Abort (Self_Id : Task_Id) renames
     System.Tasking.Initialization.Defer_Abort_Nestable;

   procedure Local_Undefer_Abort (Self_Id : Task_Id) renames
     System.Tasking.Initialization.Undefer_Abort_Nestable;

   --  Florist defers abort around critical sections that
   --  make entry calls to the Interrupt_Manager task, which
   --  violates the general rule about top-level runtime system
   --  calls from abort-deferred regions.  It is not that this is
   --  unsafe, but when it occurs in "normal" programs it usually
   --  means either the user is trying to do a potentially blocking
   --  operation from within a protected object, or there is a
   --  runtime system/compiler error that has failed to undefer
   --  an earlier abort deferral. Thus, for debugging it may be
   --  wise to modify the above renamings to the non-nestable forms.

   procedure Boost_Priority (Call : Entry_Call_Link; Acceptor : Task_Id);
   pragma Inline (Boost_Priority);
   --  Call this only with abort deferred and holding lock of Acceptor

   procedure Call_Synchronous
     (Acceptor              : Task_Id;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean);
   pragma Inline (Call_Synchronous);
   --  This call is used to make a simple or conditional entry call.
   --  Called from Call_Simple and Task_Entry_Call.

   procedure Setup_For_Rendezvous_With_Body
     (Entry_Call : Entry_Call_Link;
      Acceptor   : Task_Id);
   pragma Inline (Setup_For_Rendezvous_With_Body);
   --  Call this only with abort deferred and holding lock of Acceptor.
   --  When a rendezvous selected (ready for rendezvous) we need to save
   --  previous caller and adjust the priority. Also we need to make
   --  this call not Abortable (Cancellable) since the rendezvous has
   --  already been started.

   procedure Wait_For_Call (Self_Id : Task_Id);
   pragma Inline (Wait_For_Call);
   --  Call this only with abort deferred and holding lock of Self_Id.
   --  An accepting task goes into Sleep by calling this routine
   --  waiting for a call from the caller or waiting for an abort.
   --  Make sure Self_Id is locked before calling this routine.

   -----------------
   -- Accept_Call --
   -----------------

   procedure Accept_Call
     (E                  : Task_Entry_Index;
      Uninterpreted_Data : out System.Address)
   is
      Self_Id      : constant Task_Id := STPO.Self;
      Caller       : Task_Id := null;
      Open_Accepts : aliased Accept_List (1 .. 1);
      Entry_Call   : Entry_Call_Link;

   begin
      Initialization.Defer_Abort (Self_Id);

      if Single_Lock then
         Lock_RTS;
      end if;

      STPO.Write_Lock (Self_Id);

      if not Self_Id.Callable then
         pragma Assert (Self_Id.Pending_ATC_Level = 0);

         pragma Assert (Self_Id.Pending_Action);

         STPO.Unlock (Self_Id);

         if Single_Lock then
            Unlock_RTS;
         end if;

         Initialization.Undefer_Abort (Self_Id);

         --  Should never get here ???

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      Queuing.Dequeue_Head (Self_Id.Entry_Queues (E), Entry_Call);

      if Entry_Call /= null then
         Caller := Entry_Call.Self;
         Setup_For_Rendezvous_With_Body (Entry_Call, Self_Id);
         Uninterpreted_Data := Entry_Call.Uninterpreted_Data;

      else
         --  Wait for a caller

         Open_Accepts (1).Null_Body := False;
         Open_Accepts (1).S := E;
         Self_Id.Open_Accepts := Open_Accepts'Unrestricted_Access;

         --  Wait for normal call

         if Parameters.Runtime_Traces then
            Send_Trace_Info (W_Accept, Self_Id, Integer (Open_Accepts'Length));
         end if;

         pragma Debug
           (Debug.Trace (Self_Id, "Accept_Call: wait", 'R'));
         Wait_For_Call (Self_Id);

         pragma Assert (Self_Id.Open_Accepts = null);

         if Self_Id.Common.Call /= null then
            Caller := Self_Id.Common.Call.Self;
            Uninterpreted_Data :=
              Caller.Entry_Calls (Caller.ATC_Nesting_Level).Uninterpreted_Data;
         else
            --  Case of an aborted task

            Uninterpreted_Data := System.Null_Address;
         end if;
      end if;

      --  Self_Id.Common.Call should already be updated by the Caller
      --  On return, we will start the rendezvous.

      STPO.Unlock (Self_Id);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Initialization.Undefer_Abort (Self_Id);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (M_Accept_Complete, Caller, Entry_Index (E));
      end if;
   end Accept_Call;

   --------------------
   -- Accept_Trivial --
   --------------------

   procedure Accept_Trivial (E : Task_Entry_Index) is
      Self_Id      : constant Task_Id := STPO.Self;
      Caller       : Task_Id := null;
      Open_Accepts : aliased Accept_List (1 .. 1);
      Entry_Call   : Entry_Call_Link;

   begin
      Initialization.Defer_Abort_Nestable (Self_Id);

      if Single_Lock then
         Lock_RTS;
      end if;

      STPO.Write_Lock (Self_Id);

      if not Self_Id.Callable then
         pragma Assert (Self_Id.Pending_ATC_Level = 0);

         pragma Assert (Self_Id.Pending_Action);

         STPO.Unlock (Self_Id);

         if Single_Lock then
            Unlock_RTS;
         end if;

         Initialization.Undefer_Abort_Nestable (Self_Id);

         --  Should never get here ???

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      Queuing.Dequeue_Head (Self_Id.Entry_Queues (E), Entry_Call);

      if Entry_Call = null then
         --  Need to wait for entry call

         Open_Accepts (1).Null_Body := True;
         Open_Accepts (1).S := E;
         Self_Id.Open_Accepts := Open_Accepts'Unrestricted_Access;

         if Parameters.Runtime_Traces then
            Send_Trace_Info (W_Accept, Self_Id, Integer (Open_Accepts'Length));
         end if;

         pragma Debug
          (Debug.Trace (Self_Id, "Accept_Trivial: wait", 'R'));

         Wait_For_Call (Self_Id);

         pragma Assert (Self_Id.Open_Accepts = null);

         --  No need to do anything special here for pending abort.
         --  Abort_Signal will be raised by Undefer on exit.

         STPO.Unlock (Self_Id);

      else  --  found caller already waiting
         pragma Assert (Entry_Call.State < Done);

         STPO.Unlock (Self_Id);
         Caller := Entry_Call.Self;

         STPO.Write_Lock (Caller);
         Initialization.Wakeup_Entry_Caller (Self_Id, Entry_Call, Done);
         STPO.Unlock (Caller);
      end if;

      if Parameters.Runtime_Traces then
         Send_Trace_Info (M_Accept_Complete);

         --  Fake one, since there is (???) no way
         --  to know that the rendezvous is over

         Send_Trace_Info (M_RDV_Complete);
      end if;

      if Single_Lock then
         Unlock_RTS;
      end if;

      Initialization.Undefer_Abort_Nestable (Self_Id);
   end Accept_Trivial;

   --------------------
   -- Boost_Priority --
   --------------------

   procedure Boost_Priority (Call : Entry_Call_Link; Acceptor : Task_Id) is
      Caller        : constant Task_Id := Call.Self;
      Caller_Prio   : constant System.Any_Priority := Get_Priority (Caller);
      Acceptor_Prio : constant System.Any_Priority := Get_Priority (Acceptor);

   begin
      if Caller_Prio > Acceptor_Prio then
         Call.Acceptor_Prev_Priority := Acceptor_Prio;
         Set_Priority (Acceptor, Caller_Prio);

      else
         Call.Acceptor_Prev_Priority := Priority_Not_Boosted;
      end if;
   end Boost_Priority;

   -----------------
   -- Call_Simple --
   -----------------

   procedure Call_Simple
     (Acceptor           : Task_Id;
      E                  : Task_Entry_Index;
      Uninterpreted_Data : System.Address)
   is
      Rendezvous_Successful : Boolean;
      pragma Unreferenced (Rendezvous_Successful);

   begin
      --  If pragma Detect_Blocking is active then Program_Error must be
      --  raised if this potentially blocking operation is called from a
      --  protected action.

      if System.Tasking.Detect_Blocking
        and then STPO.Self.Common.Protected_Action_Nesting > 0
      then
         raise Program_Error with "potentially blocking operation";
      end if;

      Call_Synchronous
        (Acceptor, E, Uninterpreted_Data, Simple_Call, Rendezvous_Successful);
   end Call_Simple;

   ----------------------
   -- Call_Synchronous --
   ----------------------

   procedure Call_Synchronous
     (Acceptor              : Task_Id;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean)
   is
      Self_Id    : constant Task_Id := STPO.Self;
      Level      : ATC_Level;
      Entry_Call : Entry_Call_Link;

   begin
      pragma Assert (Mode /= Asynchronous_Call);

      Local_Defer_Abort (Self_Id);
      Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level + 1;
      pragma Debug
        (Debug.Trace (Self_Id, "CS: entered ATC level: " &
         ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));
      Level := Self_Id.ATC_Nesting_Level;
      Entry_Call := Self_Id.Entry_Calls (Level)'Access;
      Entry_Call.Next := null;
      Entry_Call.Mode := Mode;
      Entry_Call.Cancellation_Attempted := False;

      if Parameters.Runtime_Traces then
         Send_Trace_Info (W_Call, Acceptor, Entry_Index (E));
      end if;

      --  If this is a call made inside of an abort deferred region,
      --  the call should be never abortable.

      if Self_Id.Deferral_Level > 1 then
         Entry_Call.State := Never_Abortable;
      else
         Entry_Call.State := Now_Abortable;
      end if;

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := Get_Priority (Self_Id);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;
      Entry_Call.With_Abort := True;

      --  Note: the caller will undefer abort on return (see WARNING above)

      if Single_Lock then
         Lock_RTS;
      end if;

      if not Task_Do_Or_Queue (Self_Id, Entry_Call) then
         STPO.Write_Lock (Self_Id);
         Utilities.Exit_One_ATC_Level (Self_Id);
         STPO.Unlock (Self_Id);

         if Single_Lock then
            Unlock_RTS;
         end if;

         if Parameters.Runtime_Traces then
            Send_Trace_Info (E_Missed, Acceptor);
         end if;

         Local_Undefer_Abort (Self_Id);
         raise Tasking_Error;
      end if;

      STPO.Write_Lock (Self_Id);
      pragma Debug
        (Debug.Trace (Self_Id, "Call_Synchronous: wait", 'R'));
      Entry_Calls.Wait_For_Completion (Entry_Call);
      pragma Debug
        (Debug.Trace (Self_Id, "Call_Synchronous: done waiting", 'R'));
      Rendezvous_Successful := Entry_Call.State = Done;
      STPO.Unlock (Self_Id);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Local_Undefer_Abort (Self_Id);
      Entry_Calls.Check_Exception (Self_Id, Entry_Call);
   end Call_Synchronous;

   --------------
   -- Callable --
   --------------

   function Callable (T : Task_Id) return Boolean is
      Result  : Boolean;
      Self_Id : constant Task_Id := STPO.Self;

   begin
      Initialization.Defer_Abort_Nestable (Self_Id);

      if Single_Lock then
         Lock_RTS;
      end if;

      STPO.Write_Lock (T);
      Result := T.Callable;
      STPO.Unlock (T);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Initialization.Undefer_Abort_Nestable (Self_Id);
      return Result;
   end Callable;

   ----------------------------
   -- Cancel_Task_Entry_Call --
   ----------------------------

   procedure Cancel_Task_Entry_Call (Cancelled : out Boolean) is
   begin
      Entry_Calls.Try_To_Cancel_Entry_Call (Cancelled);
   end Cancel_Task_Entry_Call;

   -------------------------
   -- Complete_Rendezvous --
   -------------------------

   procedure Complete_Rendezvous is
   begin
      Exceptional_Complete_Rendezvous (Ada.Exceptions.Null_Id);
   end Complete_Rendezvous;

   -------------------------------------
   -- Exceptional_Complete_Rendezvous --
   -------------------------------------

   procedure Exceptional_Complete_Rendezvous
     (Ex : Ada.Exceptions.Exception_Id)
   is
      Self_Id                : constant Task_Id := STPO.Self;
      Entry_Call             : Entry_Call_Link := Self_Id.Common.Call;
      Caller                 : Task_Id;
      Called_PO              : STPE.Protection_Entries_Access;
      Acceptor_Prev_Priority : Integer;

      Exception_To_Raise : Ada.Exceptions.Exception_Id := Ex;
      Ceiling_Violation  : Boolean;

      use type Ada.Exceptions.Exception_Id;
      procedure Internal_Reraise;
      pragma Import (C, Internal_Reraise, "__gnat_reraise");

      procedure Transfer_Occurrence
        (Target : Ada.Exceptions.Exception_Occurrence_Access;
         Source : Ada.Exceptions.Exception_Occurrence);
      pragma Import (C, Transfer_Occurrence, "__gnat_transfer_occurrence");

      use type STPE.Protection_Entries_Access;

   begin
      --  Consider phasing out Complete_Rendezvous in favor
      --  of direct call to this with Ada.Exceptions.Null_ID.
      --  See code expansion examples for Accept_Call and Selective_Wait.
      --  Also consider putting an explicit re-raise after this call, in
      --  the generated code. That way we could eliminate the
      --  code here that reraises the exception.

      --  The deferral level is critical here,
      --  since we want to raise an exception or allow abort to take
      --  place, if there is an exception or abort pending.

      pragma Debug
       (Debug.Trace (Self_Id, "Exceptional_Complete_Rendezvous", 'R'));

      if Ex = Ada.Exceptions.Null_Id then
         --  The call came from normal end-of-rendezvous,
         --  so abort is not yet deferred.

         if Parameters.Runtime_Traces then
            Send_Trace_Info (M_RDV_Complete, Entry_Call.Self);
         end if;

         Initialization.Defer_Abort_Nestable (Self_Id);
      end if;

      --  We need to clean up any accepts which Self may have
      --  been serving when it was aborted.

      if Ex = Standard'Abort_Signal'Identity then
         if Single_Lock then
            Lock_RTS;
         end if;

         while Entry_Call /= null loop
            Entry_Call.Exception_To_Raise := Tasking_Error'Identity;

            --  All forms of accept make sure that the acceptor is not
            --  completed, before accepting further calls, so that we
            --  can be sure that no further calls are made after the
            --  current calls are purged.

            Caller := Entry_Call.Self;

            --  Take write lock. This follows the lock precedence rule that
            --  Caller may be locked while holding lock of Acceptor.
            --  Complete the call abnormally, with exception.

            STPO.Write_Lock (Caller);
            Initialization.Wakeup_Entry_Caller (Self_Id, Entry_Call, Done);
            STPO.Unlock (Caller);
            Entry_Call := Entry_Call.Acceptor_Prev_Call;
         end loop;

         if Single_Lock then
            Unlock_RTS;
         end if;

      else
         Caller := Entry_Call.Self;

         if Entry_Call.Needs_Requeue then
            --  We dare not lock Self_Id at the same time as Caller,
            --  for fear of deadlock.

            Entry_Call.Needs_Requeue := False;
            Self_Id.Common.Call := Entry_Call.Acceptor_Prev_Call;

            if Entry_Call.Called_Task /= null then
               --  Requeue to another task entry

               if Single_Lock then
                  Lock_RTS;
               end if;

               if not Task_Do_Or_Queue (Self_Id, Entry_Call) then
                  if Single_Lock then
                     Unlock_RTS;
                  end if;

                  Initialization.Undefer_Abort (Self_Id);
                  raise Tasking_Error;
               end if;

               if Single_Lock then
                  Unlock_RTS;
               end if;

            else
               --  Requeue to a protected entry

               Called_PO := POE.To_Protection (Entry_Call.Called_PO);
               STPE.Lock_Entries (Called_PO, Ceiling_Violation);

               if Ceiling_Violation then
                  pragma Assert (Ex = Ada.Exceptions.Null_Id);

                  Exception_To_Raise := Program_Error'Identity;
                  Entry_Call.Exception_To_Raise := Exception_To_Raise;

                  if Single_Lock then
                     Lock_RTS;
                  end if;

                  STPO.Write_Lock (Caller);
                  Initialization.Wakeup_Entry_Caller
                    (Self_Id, Entry_Call, Done);
                  STPO.Unlock (Caller);

                  if Single_Lock then
                     Unlock_RTS;
                  end if;

               else
                  POO.PO_Do_Or_Queue (Self_Id, Called_PO, Entry_Call);
                  POO.PO_Service_Entries (Self_Id, Called_PO);
               end if;
            end if;

            Entry_Calls.Reset_Priority
              (Self_Id, Entry_Call.Acceptor_Prev_Priority);

         else
            --  The call does not need to be requeued

            Self_Id.Common.Call := Entry_Call.Acceptor_Prev_Call;
            Entry_Call.Exception_To_Raise := Ex;

            if Single_Lock then
               Lock_RTS;
            end if;

            STPO.Write_Lock (Caller);

            --  Done with Caller locked to make sure that Wakeup is not lost

            if Ex /= Ada.Exceptions.Null_Id then
               Transfer_Occurrence
                 (Caller.Common.Compiler_Data.Current_Excep'Access,
                  Self_Id.Common.Compiler_Data.Current_Excep);
            end if;

            Acceptor_Prev_Priority := Entry_Call.Acceptor_Prev_Priority;
            Initialization.Wakeup_Entry_Caller (Self_Id, Entry_Call, Done);

            STPO.Unlock (Caller);

            if Single_Lock then
               Unlock_RTS;
            end if;

            Entry_Calls.Reset_Priority (Self_Id, Acceptor_Prev_Priority);
         end if;
      end if;

      Initialization.Undefer_Abort (Self_Id);

      if Exception_To_Raise /= Ada.Exceptions.Null_Id then
         Internal_Reraise;
      end if;

      --  ??? Do we need to give precedence to Program_Error that might be
      --  raised due to failure of finalization, over Tasking_Error from
      --  failure of requeue?
   end Exceptional_Complete_Rendezvous;

   -------------------------------------
   -- Requeue_Protected_To_Task_Entry --
   -------------------------------------

   procedure Requeue_Protected_To_Task_Entry
     (Object     : STPE.Protection_Entries_Access;
      Acceptor   : Task_Id;
      E          : Task_Entry_Index;
      With_Abort : Boolean)
   is
      Entry_Call : constant Entry_Call_Link := Object.Call_In_Progress;
   begin
      pragma Assert (STPO.Self.Deferral_Level > 0);

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Called_PO := Null_Address;
      Entry_Call.With_Abort := With_Abort;
      Object.Call_In_Progress := null;
   end Requeue_Protected_To_Task_Entry;

   ------------------------
   -- Requeue_Task_Entry --
   ------------------------

   procedure Requeue_Task_Entry
     (Acceptor   : Task_Id;
      E          : Task_Entry_Index;
      With_Abort : Boolean)
   is
      Self_Id    : constant Task_Id := STPO.Self;
      Entry_Call : constant Entry_Call_Link := Self_Id.Common.Call;

   begin
      Initialization.Defer_Abort (Self_Id);
      Entry_Call.Needs_Requeue := True;
      Entry_Call.With_Abort := With_Abort;
      Entry_Call.E := Entry_Index (E);
      Entry_Call.Called_Task := Acceptor;
      Initialization.Undefer_Abort (Self_Id);
   end Requeue_Task_Entry;

   --------------------
   -- Selective_Wait --
   --------------------

   procedure Selective_Wait
     (Open_Accepts       : Accept_List_Access;
      Select_Mode        : Select_Modes;
      Uninterpreted_Data : out System.Address;
      Index              : out Select_Index)
   is
      Self_Id          : constant Task_Id := STPO.Self;
      Entry_Call       : Entry_Call_Link;
      Treatment        : Select_Treatment;
      Caller           : Task_Id;
      Selection        : Select_Index;
      Open_Alternative : Boolean;

   begin
      Initialization.Defer_Abort (Self_Id);

      if Single_Lock then
         Lock_RTS;
      end if;

      STPO.Write_Lock (Self_Id);

      if not Self_Id.Callable then
         pragma Assert (Self_Id.Pending_ATC_Level = 0);

         pragma Assert (Self_Id.Pending_Action);

         STPO.Unlock (Self_Id);

         if Single_Lock then
            Unlock_RTS;
         end if;

         --  ??? In some cases abort is deferred more than once. Need to
         --  figure out why this happens.

         if Self_Id.Deferral_Level > 1 then
            Self_Id.Deferral_Level := 1;
         end if;

         Initialization.Undefer_Abort (Self_Id);

         --  Should never get here ???

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      pragma Assert (Open_Accepts /= null);

      Uninterpreted_Data := Null_Address;

      Queuing.Select_Task_Entry_Call
        (Self_Id, Open_Accepts, Entry_Call, Selection, Open_Alternative);

      --  Determine the kind and disposition of the select

      Treatment := Default_Treatment (Select_Mode);
      Self_Id.Chosen_Index := No_Rendezvous;

      if Open_Alternative then
         if Entry_Call /= null then
            if Open_Accepts (Selection).Null_Body then
               Treatment := Accept_Alternative_Completed;
            else
               Setup_For_Rendezvous_With_Body (Entry_Call, Self_Id);
               Treatment := Accept_Alternative_Selected;
            end if;

            Self_Id.Chosen_Index := Selection;

         elsif Treatment = No_Alternative_Open then
            Treatment := Accept_Alternative_Open;
         end if;
      end if;

      --  Handle the select according to the disposition selected above

      case Treatment is
         when Accept_Alternative_Selected =>
            --  Ready to rendezvous

            Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;

            --  In this case the accept body is not Null_Body. Defer abort
            --  until it gets into the accept body.

            pragma Assert (Self_Id.Deferral_Level = 1);

            Initialization.Defer_Abort_Nestable (Self_Id);
            STPO.Unlock (Self_Id);

         when Accept_Alternative_Completed =>

            --  Accept body is null, so rendezvous is over immediately

            if Parameters.Runtime_Traces then
               Send_Trace_Info (M_RDV_Complete, Entry_Call.Self);
            end if;

            STPO.Unlock (Self_Id);
            Caller := Entry_Call.Self;

            STPO.Write_Lock (Caller);
            Initialization.Wakeup_Entry_Caller (Self_Id, Entry_Call, Done);
            STPO.Unlock (Caller);

         when Accept_Alternative_Open =>

            --  Wait for caller

            Self_Id.Open_Accepts := Open_Accepts;
            pragma Debug
              (Debug.Trace (Self_Id, "Selective_Wait: wait", 'R'));

            if Parameters.Runtime_Traces then
               Send_Trace_Info (W_Select, Self_Id,
                                Integer (Open_Accepts'Length));
            end if;

            Wait_For_Call (Self_Id);

            pragma Assert (Self_Id.Open_Accepts = null);

            --  Self_Id.Common.Call should already be updated by the Caller if
            --  not aborted. It might also be ready to do rendezvous even if
            --  this wakes up due to an abort. Therefore, if the call is not
            --  empty we need to do the rendezvous if the accept body is not
            --  Null_Body.

            --  Aren't the first two conditions below redundant???

            if Self_Id.Chosen_Index /= No_Rendezvous
              and then Self_Id.Common.Call /= null
              and then not Open_Accepts (Self_Id.Chosen_Index).Null_Body
            then
               Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;

               pragma Assert
                 (Self_Id.Deferral_Level = 1
                   or else
                     (Self_Id.Deferral_Level = 0
                       and then not Restrictions.Abort_Allowed));

               Initialization.Defer_Abort_Nestable (Self_Id);

               --  Leave abort deferred until the accept body
            end if;

            STPO.Unlock (Self_Id);

         when Else_Selected =>
            pragma Assert (Self_Id.Open_Accepts = null);

            if Parameters.Runtime_Traces then
               Send_Trace_Info (M_Select_Else);
            end if;

            STPO.Unlock (Self_Id);

         when Terminate_Selected =>
            --  Terminate alternative is open

            Self_Id.Open_Accepts := Open_Accepts;
            Self_Id.Common.State := Acceptor_Sleep;

            --  Notify ancestors that this task is on a terminate alternative

            STPO.Unlock (Self_Id);
            Utilities.Make_Passive (Self_Id, Task_Completed => False);
            STPO.Write_Lock (Self_Id);

            --  Wait for normal entry call or termination

            Wait_For_Call (Self_Id);

            pragma Assert (Self_Id.Open_Accepts = null);

            if Self_Id.Terminate_Alternative then
               --  An entry call should have reset this to False,
               --  so we must be aborted.
               --  We cannot be in an async. select, since that
               --  is not legal, so the abort must be of the entire
               --  task.  Therefore, we do not need to cancel the
               --  terminate alternative.  The cleanup will be done
               --  in Complete_Master.

               pragma Assert (Self_Id.Pending_ATC_Level = 0);
               pragma Assert (Self_Id.Awake_Count = 0);

               STPO.Unlock (Self_Id);

               if Single_Lock then
                  Unlock_RTS;
               end if;

               Index := Self_Id.Chosen_Index;
               Initialization.Undefer_Abort_Nestable (Self_Id);

               if Self_Id.Pending_Action then
                  Initialization.Do_Pending_Action (Self_Id);
               end if;

               return;

            else
               --  Self_Id.Common.Call and Self_Id.Chosen_Index
               --  should already be updated by the Caller.

               if Self_Id.Chosen_Index /= No_Rendezvous
                 and then not Open_Accepts (Self_Id.Chosen_Index).Null_Body
               then
                  Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;

                  pragma Assert (Self_Id.Deferral_Level = 1);

                  --  We need an extra defer here, to keep abort
                  --  deferred until we get into the accept body

                  Initialization.Defer_Abort_Nestable (Self_Id);
               end if;
            end if;

            STPO.Unlock (Self_Id);

         when No_Alternative_Open =>
            --  In this case, Index will be No_Rendezvous on return, which
            --  should cause a Program_Error if it is not a Delay_Mode.

            --  If delay alternative exists (Delay_Mode) we should suspend
            --  until the delay expires.

            Self_Id.Open_Accepts := null;

            if Select_Mode = Delay_Mode then
               Self_Id.Common.State := Delay_Sleep;

               loop
                  exit when
                    Self_Id.Pending_ATC_Level < Self_Id.ATC_Nesting_Level;
                  Sleep (Self_Id, Delay_Sleep);
               end loop;

               Self_Id.Common.State := Runnable;
               STPO.Unlock (Self_Id);

            else
               STPO.Unlock (Self_Id);

               if Single_Lock then
                  Unlock_RTS;
               end if;

               Initialization.Undefer_Abort (Self_Id);
               raise Program_Error with "Entry call not a delay mode";
            end if;
      end case;

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Caller has been chosen.
      --  Self_Id.Common.Call should already be updated by the Caller.
      --  Self_Id.Chosen_Index should either be updated by the Caller
      --  or by Test_Selective_Wait.
      --  On return, we sill start rendezvous unless the accept body is
      --  null. In the latter case, we will have already completed the RV.

      Index := Self_Id.Chosen_Index;
      Initialization.Undefer_Abort_Nestable (Self_Id);
   end Selective_Wait;

   ------------------------------------
   -- Setup_For_Rendezvous_With_Body --
   ------------------------------------

   procedure Setup_For_Rendezvous_With_Body
     (Entry_Call : Entry_Call_Link;
      Acceptor   : Task_Id) is
   begin
      Entry_Call.Acceptor_Prev_Call := Acceptor.Common.Call;
      Acceptor.Common.Call := Entry_Call;

      if Entry_Call.State = Now_Abortable then
         Entry_Call.State := Was_Abortable;
      end if;

      Boost_Priority (Entry_Call, Acceptor);
   end Setup_For_Rendezvous_With_Body;

   ----------------
   -- Task_Count --
   ----------------

   function Task_Count (E : Task_Entry_Index) return Natural is
      Self_Id      : constant Task_Id := STPO.Self;
      Return_Count : Natural;

   begin
      Initialization.Defer_Abort (Self_Id);

      if Single_Lock then
         Lock_RTS;
      end if;

      STPO.Write_Lock (Self_Id);
      Return_Count := Queuing.Count_Waiting (Self_Id.Entry_Queues (E));
      STPO.Unlock (Self_Id);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Initialization.Undefer_Abort (Self_Id);

      --  Call Yield to let other tasks get a chance to run as this is a
      --  potential dispatching point.

      Yield (Do_Yield => False);
      return Return_Count;
   end Task_Count;

   ----------------------
   -- Task_Do_Or_Queue --
   ----------------------

   function Task_Do_Or_Queue
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link) return Boolean
   is
      E             : constant Task_Entry_Index :=
                        Task_Entry_Index (Entry_Call.E);
      Old_State     : constant Entry_Call_State := Entry_Call.State;
      Acceptor      : constant Task_Id := Entry_Call.Called_Task;
      Parent        : constant Task_Id := Acceptor.Common.Parent;
      Parent_Locked : Boolean := False;
      Null_Body     : Boolean;

   begin
      --  Find out whether Entry_Call can be accepted immediately

      --  If the Acceptor is not callable, return False.
      --  If the rendezvous can start, initiate it.
      --  If the accept-body is trivial, also complete the rendezvous.
      --  If the acceptor is not ready, enqueue the call.

      --  This should have a special case for Accept_Call and Accept_Trivial,
      --  so that we don't have the loop setup overhead, below.

      --  The call state Done is used here and elsewhere to include both the
      --  case of normal successful completion, and the case of an exception
      --  being raised. The difference is that if an exception is raised no one
      --  will pay attention to the fact that State = Done. Instead the
      --  exception will be raised in Undefer_Abort, and control will skip past
      --  the place where we normally would resume from an entry call.

      pragma Assert (not Queuing.Onqueue (Entry_Call));

      --  We rely that the call is off-queue for protection, that the caller
      --  will not exit the Entry_Caller_Sleep, and so will not reuse the call
      --  record for another call.
      --  We rely on the Caller's lock for call State mod's.

      --  We can't lock Acceptor.Parent while holding Acceptor,
      --  so lock it in advance if we expect to need to lock it.

      if Acceptor.Terminate_Alternative then
         STPO.Write_Lock (Parent);
         Parent_Locked := True;
      end if;

      STPO.Write_Lock (Acceptor);

      --  If the acceptor is not callable, abort the call and return False

      if not Acceptor.Callable then
         STPO.Unlock (Acceptor);

         if Parent_Locked then
            STPO.Unlock (Parent);
         end if;

         pragma Assert (Entry_Call.State < Done);

         --  In case we are not the caller, set up the caller
         --  to raise Tasking_Error when it wakes up.

         STPO.Write_Lock (Entry_Call.Self);
         Entry_Call.Exception_To_Raise := Tasking_Error'Identity;
         Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Done);
         STPO.Unlock (Entry_Call.Self);

         return False;
      end if;

      --  Try to serve the call immediately

      if Acceptor.Open_Accepts /= null then
         for J in Acceptor.Open_Accepts'Range loop
            if Entry_Call.E = Entry_Index (Acceptor.Open_Accepts (J).S) then

               --  Commit acceptor to rendezvous with us

               Acceptor.Chosen_Index := J;
               Null_Body := Acceptor.Open_Accepts (J).Null_Body;
               Acceptor.Open_Accepts := null;

               --  Prevent abort while call is being served

               if Entry_Call.State = Now_Abortable then
                  Entry_Call.State := Was_Abortable;
               end if;

               if Acceptor.Terminate_Alternative then

                  --  Cancel terminate alternative. See matching code in
                  --  Selective_Wait and Vulnerable_Complete_Master.

                  Acceptor.Terminate_Alternative := False;
                  Acceptor.Awake_Count := Acceptor.Awake_Count + 1;

                  if Acceptor.Awake_Count = 1 then

                     --  Notify parent that acceptor is awake

                     pragma Assert (Parent.Awake_Count > 0);

                     Parent.Awake_Count := Parent.Awake_Count + 1;

                     if Parent.Common.State = Master_Completion_Sleep
                       and then Acceptor.Master_of_Task = Parent.Master_Within
                     then
                        Parent.Common.Wait_Count :=
                          Parent.Common.Wait_Count + 1;
                     end if;
                  end if;
               end if;

               if Null_Body then

                  --  Rendezvous is over immediately

                  STPO.Wakeup (Acceptor, Acceptor_Sleep);
                  STPO.Unlock (Acceptor);

                  if Parent_Locked then
                     STPO.Unlock (Parent);
                  end if;

                  STPO.Write_Lock (Entry_Call.Self);
                  Initialization.Wakeup_Entry_Caller
                    (Self_ID, Entry_Call, Done);
                  STPO.Unlock (Entry_Call.Self);

               else
                  Setup_For_Rendezvous_With_Body (Entry_Call, Acceptor);

                  --  For terminate_alternative, acceptor may not be asleep
                  --  yet, so we skip the wakeup

                  if Acceptor.Common.State /= Runnable then
                     STPO.Wakeup (Acceptor, Acceptor_Sleep);
                  end if;

                  STPO.Unlock (Acceptor);

                  if Parent_Locked then
                     STPO.Unlock (Parent);
                  end if;
               end if;

               return True;
            end if;
         end loop;

         --  The acceptor is accepting, but not this entry
      end if;

      --  If the acceptor was ready to accept this call,
      --  we would not have gotten this far, so now we should
      --  (re)enqueue the call, if the mode permits that.

      if Entry_Call.Mode /= Conditional_Call
        or else not Entry_Call.With_Abort
      then
         --  Timed_Call, Simple_Call, or Asynchronous_Call

         Queuing.Enqueue (Acceptor.Entry_Queues (E), Entry_Call);

         --  Update abortability of call

         pragma Assert (Old_State < Done);

         Entry_Call.State :=
           New_State (Entry_Call.With_Abort, Entry_Call.State);

         STPO.Unlock (Acceptor);

         if Parent_Locked then
            STPO.Unlock (Parent);
         end if;

         if Old_State /= Entry_Call.State
           and then Entry_Call.State = Now_Abortable
           and then Entry_Call.Mode > Simple_Call
           and then Entry_Call.Self /= Self_ID

         --  Asynchronous_Call or Conditional_Call

         then
            --  Because of ATCB lock ordering rule

            STPO.Write_Lock (Entry_Call.Self);

            if Entry_Call.Self.Common.State = Async_Select_Sleep then

               --  Caller may not yet have reached wait-point

               STPO.Wakeup (Entry_Call.Self, Async_Select_Sleep);
            end if;

            STPO.Unlock (Entry_Call.Self);
         end if;

      else
         --  Conditional_Call and With_Abort

         STPO.Unlock (Acceptor);

         if Parent_Locked then
            STPO.Unlock (Parent);
         end if;

         STPO.Write_Lock (Entry_Call.Self);

         pragma Assert (Entry_Call.State >= Was_Abortable);

         Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Cancelled);
         STPO.Unlock (Entry_Call.Self);
      end if;

      return True;
   end Task_Do_Or_Queue;

   ---------------------
   -- Task_Entry_Call --
   ---------------------

   procedure Task_Entry_Call
     (Acceptor              : Task_Id;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean)
   is
      Self_Id    : constant Task_Id := STPO.Self;
      Entry_Call : Entry_Call_Link;

   begin
      --  If pragma Detect_Blocking is active then Program_Error must be
      --  raised if this potentially blocking operation is called from a
      --  protected action.

      if System.Tasking.Detect_Blocking
        and then Self_Id.Common.Protected_Action_Nesting > 0
      then
         raise Program_Error with "potentially blocking operation";
      end if;

      if Parameters.Runtime_Traces then
         Send_Trace_Info (W_Call, Acceptor, Entry_Index (E));
      end if;

      if Mode = Simple_Call or else Mode = Conditional_Call then
         Call_Synchronous
           (Acceptor, E, Uninterpreted_Data, Mode, Rendezvous_Successful);

      else
         --  This is an asynchronous call

         --  Abort must already be deferred by the compiler-generated code.
         --  Without this, an abort that occurs between the time that this
         --  call is made and the time that the abortable part's cleanup
         --  handler is set up might miss the cleanup handler and leave the
         --  call pending.

         Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level + 1;
         pragma Debug
           (Debug.Trace (Self_Id, "TEC: entered ATC level: " &
            ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));
         Entry_Call := Self_Id.Entry_Calls (Self_Id.ATC_Nesting_Level)'Access;
         Entry_Call.Next := null;
         Entry_Call.Mode := Mode;
         Entry_Call.Cancellation_Attempted := False;
         Entry_Call.State := Not_Yet_Abortable;
         Entry_Call.E := Entry_Index (E);
         Entry_Call.Prio := Get_Priority (Self_Id);
         Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
         Entry_Call.Called_Task := Acceptor;
         Entry_Call.Called_PO := Null_Address;
         Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;
         Entry_Call.With_Abort := True;

         if Single_Lock then
            Lock_RTS;
         end if;

         if not Task_Do_Or_Queue (Self_Id, Entry_Call) then
            STPO.Write_Lock (Self_Id);
            Utilities.Exit_One_ATC_Level (Self_Id);
            STPO.Unlock (Self_Id);

            if Single_Lock then
               Unlock_RTS;
            end if;

            Initialization.Undefer_Abort (Self_Id);

            if Parameters.Runtime_Traces then
               Send_Trace_Info (E_Missed, Acceptor);
            end if;

            raise Tasking_Error;
         end if;

         --  The following is special for async. entry calls.
         --  If the call was not queued abortably, we need to wait until
         --  it is before proceeding with the abortable part.

         --  Wait_Until_Abortable can be called unconditionally here,
         --  but it is expensive.

         if Entry_Call.State < Was_Abortable then
            Entry_Calls.Wait_Until_Abortable (Self_Id, Entry_Call);
         end if;

         if Single_Lock then
            Unlock_RTS;
         end if;

         --  Note: following assignment needs to be atomic

         Rendezvous_Successful := Entry_Call.State = Done;
      end if;
   end Task_Entry_Call;

   -----------------------
   -- Task_Entry_Caller --
   -----------------------

   function Task_Entry_Caller (D : Task_Entry_Nesting_Depth) return Task_Id is
      Self_Id    : constant Task_Id := STPO.Self;
      Entry_Call : Entry_Call_Link;

   begin
      Entry_Call := Self_Id.Common.Call;

      for Depth in 1 .. D loop
         Entry_Call := Entry_Call.Acceptor_Prev_Call;
         pragma Assert (Entry_Call /= null);
      end loop;

      return Entry_Call.Self;
   end Task_Entry_Caller;

   --------------------------
   -- Timed_Selective_Wait --
   --------------------------

   procedure Timed_Selective_Wait
     (Open_Accepts       : Accept_List_Access;
      Select_Mode        : Select_Modes;
      Uninterpreted_Data : out System.Address;
      Timeout            : Duration;
      Mode               : Delay_Modes;
      Index              : out Select_Index)
   is
      Self_Id          : constant Task_Id := STPO.Self;
      Treatment        : Select_Treatment;
      Entry_Call       : Entry_Call_Link;
      Caller           : Task_Id;
      Selection        : Select_Index;
      Open_Alternative : Boolean;
      Timedout         : Boolean := False;
      Yielded          : Boolean := True;

   begin
      pragma Assert (Select_Mode = Delay_Mode);

      Initialization.Defer_Abort (Self_Id);

      --  If we are aborted here, the effect will be pending

      if Single_Lock then
         Lock_RTS;
      end if;

      STPO.Write_Lock (Self_Id);

      if not Self_Id.Callable then
         pragma Assert (Self_Id.Pending_ATC_Level = 0);

         pragma Assert (Self_Id.Pending_Action);

         STPO.Unlock (Self_Id);

         if Single_Lock then
            Unlock_RTS;
         end if;

         Initialization.Undefer_Abort (Self_Id);

         --  Should never get here ???

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      Uninterpreted_Data := Null_Address;

      pragma Assert (Open_Accepts /= null);

      Queuing.Select_Task_Entry_Call
        (Self_Id, Open_Accepts, Entry_Call, Selection, Open_Alternative);

      --  Determine the kind and disposition of the select

      Treatment := Default_Treatment (Select_Mode);
      Self_Id.Chosen_Index := No_Rendezvous;

      if Open_Alternative then
         if Entry_Call /= null then
            if Open_Accepts (Selection).Null_Body then
               Treatment := Accept_Alternative_Completed;

            else
               Setup_For_Rendezvous_With_Body (Entry_Call, Self_Id);
               Treatment := Accept_Alternative_Selected;
            end if;

            Self_Id.Chosen_Index := Selection;

         elsif Treatment = No_Alternative_Open then
            Treatment := Accept_Alternative_Open;
         end if;
      end if;

      --  Handle the select according to the disposition selected above

      case Treatment is
         when Accept_Alternative_Selected =>
            --  Ready to rendezvous
            --  In this case the accept body is not Null_Body. Defer abort
            --  until it gets into the accept body.

            Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;
            Initialization.Defer_Abort (Self_Id);
            STPO.Unlock (Self_Id);

         when Accept_Alternative_Completed =>
            --  Rendezvous is over

            if Parameters.Runtime_Traces then
               Send_Trace_Info (M_RDV_Complete, Entry_Call.Self);
            end if;

            STPO.Unlock (Self_Id);
            Caller := Entry_Call.Self;

            STPO.Write_Lock (Caller);
            Initialization.Wakeup_Entry_Caller (Self_Id, Entry_Call, Done);
            STPO.Unlock (Caller);

         when Accept_Alternative_Open =>

            --  Wait for caller

            Self_Id.Open_Accepts := Open_Accepts;

            --  Wait for a normal call and a pending action until the
            --  Wakeup_Time is reached.

            Self_Id.Common.State := Acceptor_Sleep;

            --  Try to remove calls to Sleep in the loop below by letting the
            --  caller a chance of getting ready immediately, using Unlock
            --  Yield. See similar action in Wait_For_Completion/Wait_For_Call.

            if Single_Lock then
               Unlock_RTS;
            else
               Unlock (Self_Id);
            end if;

            if Self_Id.Open_Accepts /= null then
               Yield;
            end if;

            if Single_Lock then
               Lock_RTS;
            else
               Write_Lock (Self_Id);
            end if;

            --  Check if this task has been aborted while the lock was released

            if Self_Id.Pending_ATC_Level < Self_Id.ATC_Nesting_Level then
               Self_Id.Open_Accepts := null;
            end if;

            loop
               exit when Self_Id.Open_Accepts = null;

               if Timedout then
                  Sleep (Self_Id, Acceptor_Sleep);
               else
                  if Parameters.Runtime_Traces then
                     Send_Trace_Info (WT_Select,
                                      Self_Id,
                                      Integer (Open_Accepts'Length),
                                      Timeout);
                  end if;

                  STPO.Timed_Sleep (Self_Id, Timeout, Mode,
                    Acceptor_Sleep, Timedout, Yielded);
               end if;

               if Timedout then
                  Self_Id.Open_Accepts := null;

                  if Parameters.Runtime_Traces then
                     Send_Trace_Info (E_Timeout);
                  end if;
               end if;
            end loop;

            Self_Id.Common.State := Runnable;

            --  Self_Id.Common.Call should already be updated by the Caller if
            --  not aborted. It might also be ready to do rendezvous even if
            --  this wakes up due to an abort. Therefore, if the call is not
            --  empty we need to do the rendezvous if the accept body is not
            --  Null_Body.

            if Self_Id.Chosen_Index /= No_Rendezvous
              and then Self_Id.Common.Call /= null
              and then not Open_Accepts (Self_Id.Chosen_Index).Null_Body
            then
               Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;

               pragma Assert (Self_Id.Deferral_Level = 1);

               Initialization.Defer_Abort_Nestable (Self_Id);

               --  Leave abort deferred until the accept body
            end if;

            STPO.Unlock (Self_Id);

         when No_Alternative_Open =>
            --  In this case, Index will be No_Rendezvous on return. We sleep
            --  for the time we need to.
            --  Wait for a signal or timeout. A wakeup can be made
            --  for several reasons:
            --  1) Delay is expired
            --  2) Pending_Action needs to be checked
            --     (Abort, Priority change)
            --  3) Spurious wakeup

            Self_Id.Open_Accepts := null;
            Self_Id.Common.State := Acceptor_Sleep;

            STPO.Timed_Sleep (Self_Id, Timeout, Mode, Acceptor_Sleep,
              Timedout, Yielded);

            Self_Id.Common.State := Runnable;

            STPO.Unlock (Self_Id);

         when others =>
            --  Should never get here
            pragma Assert (False);
            null;
      end case;

      if Single_Lock then
         Unlock_RTS;
      end if;

      if not Yielded then
         Yield;
      end if;

      --  Caller has been chosen

      --  Self_Id.Common.Call should already be updated by the Caller

      --  Self_Id.Chosen_Index should either be updated by the Caller
      --  or by Test_Selective_Wait

      Index := Self_Id.Chosen_Index;
      Initialization.Undefer_Abort_Nestable (Self_Id);

      --  Start rendezvous, if not already completed
   end Timed_Selective_Wait;

   ---------------------------
   -- Timed_Task_Entry_Call --
   ---------------------------

   procedure Timed_Task_Entry_Call
     (Acceptor              : Task_Id;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Timeout               : Duration;
      Mode                  : Delay_Modes;
      Rendezvous_Successful : out Boolean)
   is
      Self_Id    : constant Task_Id := STPO.Self;
      Level      : ATC_Level;
      Entry_Call : Entry_Call_Link;

      Yielded : Boolean;
      pragma Unreferenced (Yielded);

   begin
      --  If pragma Detect_Blocking is active then Program_Error must be
      --  raised if this potentially blocking operation is called from a
      --  protected action.

      if System.Tasking.Detect_Blocking
        and then Self_Id.Common.Protected_Action_Nesting > 0
      then
         raise Program_Error with "potentially blocking operation";
      end if;

      Initialization.Defer_Abort (Self_Id);
      Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level + 1;

      pragma Debug
        (Debug.Trace (Self_Id, "TTEC: entered ATC level: " &
         ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));

      if Parameters.Runtime_Traces then
         Send_Trace_Info (WT_Call, Acceptor,
                          Entry_Index (E), Timeout);
      end if;

      Level := Self_Id.ATC_Nesting_Level;
      Entry_Call := Self_Id.Entry_Calls (Level)'Access;
      Entry_Call.Next := null;
      Entry_Call.Mode := Timed_Call;
      Entry_Call.Cancellation_Attempted := False;

      --  If this is a call made inside of an abort deferred region,
      --  the call should be never abortable.

      if Self_Id.Deferral_Level > 1 then
         Entry_Call.State := Never_Abortable;
      else
         Entry_Call.State := Now_Abortable;
      end if;

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := Get_Priority (Self_Id);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Called_PO := Null_Address;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;
      Entry_Call.With_Abort := True;

      --  Note: the caller will undefer abort on return (see WARNING above)

      if Single_Lock then
         Lock_RTS;
      end if;

      if not Task_Do_Or_Queue (Self_Id, Entry_Call) then
         STPO.Write_Lock (Self_Id);
         Utilities.Exit_One_ATC_Level (Self_Id);
         STPO.Unlock (Self_Id);

         if Single_Lock then
            Unlock_RTS;
         end if;

         Initialization.Undefer_Abort (Self_Id);

         if Parameters.Runtime_Traces then
            Send_Trace_Info (E_Missed, Acceptor);
         end if;
         raise Tasking_Error;
      end if;

      Write_Lock (Self_Id);
      Entry_Calls.Wait_For_Completion_With_Timeout
        (Entry_Call, Timeout, Mode, Yielded);
      Unlock (Self_Id);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  ??? Do we need to yield in case Yielded is False

      Rendezvous_Successful := Entry_Call.State = Done;
      Initialization.Undefer_Abort (Self_Id);
      Entry_Calls.Check_Exception (Self_Id, Entry_Call);
   end Timed_Task_Entry_Call;

   -------------------
   -- Wait_For_Call --
   -------------------

   procedure Wait_For_Call (Self_Id : Task_Id) is
   begin
      Self_Id.Common.State := Acceptor_Sleep;

      --  Try to remove calls to Sleep in the loop below by letting the caller
      --  a chance of getting ready immediately, using Unlock & Yield.
      --  See similar action in Wait_For_Completion & Timed_Selective_Wait.

      if Single_Lock then
         Unlock_RTS;
      else
         Unlock (Self_Id);
      end if;

      if Self_Id.Open_Accepts /= null then
         Yield;
      end if;

      if Single_Lock then
         Lock_RTS;
      else
         Write_Lock (Self_Id);
      end if;

      --  Check if this task has been aborted while the lock was released

      if Self_Id.Pending_ATC_Level < Self_Id.ATC_Nesting_Level then
         Self_Id.Open_Accepts := null;
      end if;

      loop
         exit when Self_Id.Open_Accepts = null;
         Sleep (Self_Id, Acceptor_Sleep);
      end loop;

      Self_Id.Common.State := Runnable;
   end Wait_For_Call;

end System.Tasking.Rendezvous;
