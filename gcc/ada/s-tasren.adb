------------------------------------------------------------------------------
--                                                                          --
--               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--            S Y S T E M . T A S K I N G . R E N D E Z V O U S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.101 $
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

with Ada.Exceptions;
--  Used for Exception_ID
--           Null_Id
--           Save_Occurrence
--           Raise_Exception

with System.Task_Primitives.Operations;
--  used for Get_Priority
--           Set_Priority
--           Write_Lock
--           Unlock
--           Sleep
--           Wakeup
--           Timed_Sleep

with System.Tasking.Entry_Calls;
--  Used for Wait_For_Completion
--           Wait_For_Completion_With_Timeout
--           Wait_Until_Abortable

with System.Tasking.Initialization;
--  used for Defer_Abort
--           Undefer_Abort
--           Poll_Base_Priority_Change

with System.Tasking.Queuing;
--  used for Enqueue
--           Dequeue_Head
--           Select_Task_Entry_Call
--           Count_Waiting

with System.Tasking.Utilities;
--  used for Check_Exception
--           Make_Passive
--           Wakeup_Entry_Caller

with System.Tasking.Protected_Objects.Operations;
--  used for PO_Do_Or_Queue
--           PO_Service_Entries
--           Lock_Entries
--           Unlock_Entries

with System.Tasking.Debug;
--  used for Trace

package body System.Tasking.Rendezvous is

   package STPO renames System.Task_Primitives.Operations;
   package POO renames System.Tasking.Protected_Objects.Operations;
   package POE renames System.Tasking.Protected_Objects.Entries;

   use System.Task_Primitives;
   use System.Task_Primitives.Operations;

   type Select_Treatment is (
     Accept_Alternative_Selected,   --  alternative with non-null body
     Accept_Alternative_Completed,  --  alternative with null body
     Else_Selected,
     Terminate_Selected,
     Accept_Alternative_Open,
     No_Alternative_Open);

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

   procedure Local_Defer_Abort (Self_Id : Task_ID) renames
     System.Tasking.Initialization.Defer_Abort_Nestable;

   procedure Local_Undefer_Abort (Self_Id : Task_ID) renames
     System.Tasking.Initialization.Undefer_Abort_Nestable;

   --  Florist defers abort around critical sections that
   --  make entry calls to the Interrupt_Manager task, which
   --  violates the general rule about top-level runtime system
   --  calls from abort-deferred regions.  It is not that this is
   --  unsafe, but when it occurs in "normal" programs it usually
   --  means either the user is trying to do a potentially blocking
   --  operation from within a protected object, or there is a
   --  runtime system/compiler error that has failed to undefer
   --  an earlier abort deferral.  Thus, for debugging it may be
   --  wise to modify the above renamings to the non-nestable forms.

   procedure Boost_Priority
     (Call     : Entry_Call_Link;
      Acceptor : Task_ID);
   pragma Inline (Boost_Priority);
   --  Call this only with abort deferred and holding lock of Acceptor.

   procedure Call_Synchronous
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean);
   pragma Inline (Call_Synchronous);
   --  This call is used to make a simple or conditional entry call.

   procedure Setup_For_Rendezvous_With_Body
     (Entry_Call : Entry_Call_Link;
      Acceptor   : Task_ID);
   pragma Inline (Setup_For_Rendezvous_With_Body);
   --  Call this only with abort deferred and holding lock of Acceptor.
   --  When a rendezvous selected (ready for rendezvous) we need to save
   --  privious caller and adjust the priority. Also we need to make
   --  this call not Abortable (Cancellable) since the rendezvous has
   --  already been started.

   function Is_Entry_Open (T : Task_ID; E : Task_Entry_Index) return Boolean;
   pragma Inline (Is_Entry_Open);
   --  Call this only with abort deferred and holding lock of T.

   procedure Wait_For_Call (Self_Id : Task_ID);
   pragma Inline (Wait_For_Call);
   --  Call this only with abort deferred and holding lock of Self_Id.
   --  An accepting task goes into Sleep by calling this routine
   --  waiting for a call from the caller or waiting for an abortion.
   --  Make sure Self_Id is locked before calling this routine.

   -----------------
   -- Accept_Call --
   -----------------

   --  Compiler interface only.  Do not call from within the RTS.

   --  source:
   --              accept E do  ...A... end E;
   --  expansion:
   --              A27b : address;
   --              L26b : label
   --              begin
   --                 accept_call (1, A27b);
   --                 ...A...
   --                 complete_rendezvous;
   --              <<L26b>>
   --              exception
   --              when all others =>
   --                 exceptional_complete_rendezvous (get_gnat_exception);
   --              end;

   --  The handler for Abort_Signal (*all* others) is to handle the case when
   --  the acceptor is aborted between Accept_Call and the corresponding
   --  Complete_Rendezvous call. We need to wake up the caller in this case.

   --   See also Selective_Wait

   procedure Accept_Call
     (E                  : Task_Entry_Index;
      Uninterpreted_Data : out System.Address)
   is
      Self_Id      : constant Task_ID := STPO.Self;
      Caller       : Task_ID := null;
      Open_Accepts : aliased Accept_List (1 .. 1);
      Entry_Call   : Entry_Call_Link;

   begin
      Initialization.Defer_Abort (Self_Id);

      STPO.Write_Lock (Self_Id);

      if not Self_Id.Callable then
         pragma Assert (Self_Id.Pending_ATC_Level = 0);

         pragma Assert (Self_Id.Pending_Action);

         STPO.Unlock (Self_Id);
         Initialization.Undefer_Abort (Self_Id);

         --  Should never get here ???

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      --  If someone completed this task, this task should not try to
      --  access its pending entry calls or queues in this case, as they
      --  are being emptied. Wait for abortion to kill us.
      --  ?????
      --  Recheck the correctness of the above, now that we have made
      --  changes.  The logic above seems to be based on the assumption
      --  that one task can safely clean up another's in-service accepts.
      --  ?????
      --  Why do we need to block here in this case?
      --  Why not just return and let Undefer_Abort do its work?

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

         pragma Debug
           (Debug.Trace (Self_Id, "Accept_Call: wait", 'R'));
         Wait_For_Call (Self_Id);

         pragma Assert (Self_Id.Open_Accepts = null);

         if Self_Id.Pending_ATC_Level >= Self_Id.ATC_Nesting_Level then
            Caller := Self_Id.Common.Call.Self;
            Uninterpreted_Data :=
              Caller.Entry_Calls (Caller.ATC_Nesting_Level).Uninterpreted_Data;
         end if;

         --  If this task has been aborted, skip the Uninterpreted_Data load
         --  (Caller will not be reliable) and fall through to
         --  Undefer_Abort which will allow the task to be killed.
         --  ?????
         --  Perhaps we could do the code anyway, if it has no harm, in order
         --  to get better performance for the normal case.

      end if;

      --  Self_Id.Common.Call should already be updated by the Caller
      --  On return, we will start the rendezvous.

      STPO.Unlock (Self_Id);
      Initialization.Undefer_Abort (Self_Id);
   end Accept_Call;

   --------------------
   -- Accept_Trivial --
   --------------------

   --  Compiler interface only.  Do not call from within the RTS.
   --  This should only be called when there is no accept body,
   --  or the except body is empty.

   --  source:
   --               accept E;
   --  expansion:
   --               accept_trivial (1);

   --  The compiler is also able to recognize the following and
   --  translate it the same way.

   --     accept E do null; end E;

   procedure Accept_Trivial (E : Task_Entry_Index) is
      Self_Id       : constant Task_ID := STPO.Self;
      Caller        : Task_ID := null;
      Open_Accepts  : aliased Accept_List (1 .. 1);
      Entry_Call    : Entry_Call_Link;

   begin
      Initialization.Defer_Abort_Nestable (Self_Id);
      STPO.Write_Lock (Self_Id);

      if not Self_Id.Callable then
         pragma Assert (Self_Id.Pending_ATC_Level = 0);

         pragma Assert (Self_Id.Pending_Action);

         STPO.Unlock (Self_Id);
         Initialization.Undefer_Abort_Nestable (Self_Id);

         --  Should never get here ???

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      --  If someone completed this task, this task should not try to
      --  access its pending entry calls or queues in this case, as they
      --  are being emptied. Wait for abortion to kill us.
      --  ?????
      --  Recheck the correctness of the above, now that we have made
      --  changes.

      Queuing.Dequeue_Head (Self_Id.Entry_Queues (E), Entry_Call);

      if Entry_Call = null then

         --  Need to wait for entry call

         Open_Accepts (1).Null_Body := True;
         Open_Accepts (1).S := E;
         Self_Id.Open_Accepts := Open_Accepts'Unrestricted_Access;

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

      Initialization.Undefer_Abort_Nestable (Self_Id);
   end Accept_Trivial;

   --------------------
   -- Boost_Priority --
   --------------------

   --  Call this only with abort deferred and holding lock of Acceptor.

   procedure Boost_Priority (Call : Entry_Call_Link; Acceptor : Task_ID) is
      Caller        : Task_ID := Call.Self;
      Caller_Prio   : System.Any_Priority := Get_Priority (Caller);
      Acceptor_Prio : System.Any_Priority := Get_Priority (Acceptor);

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

   --  Compiler interface only.  Do not call from within the RTS.

   procedure Call_Simple
     (Acceptor           : Task_ID;
      E                  : Task_Entry_Index;
      Uninterpreted_Data : System.Address)
   is
      Rendezvous_Successful : Boolean;
   begin
      Call_Synchronous
        (Acceptor, E, Uninterpreted_Data, Simple_Call, Rendezvous_Successful);
   end Call_Simple;

   ----------------------
   -- Call_Synchronous --
   ----------------------

   --  Compiler interface.
   --  Also called from inside Call_Simple and Task_Entry_Call.

   procedure Call_Synchronous
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean)
   is
      Self_Id    : constant Task_ID := STPO.Self;
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

      --  Note: the caller will undefer abortion on return (see WARNING above)

      if not Task_Do_Or_Queue
        (Self_Id, Entry_Call, With_Abort => True)
      then
         Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level - 1;
         Initialization.Undefer_Abort (Self_Id);
         pragma Debug
           (Debug.Trace (Self_Id, "CS: exited to ATC level: " &
            ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));
         raise Tasking_Error;
      end if;

      STPO.Write_Lock (Self_Id);
      pragma Debug
        (Debug.Trace (Self_Id, "Call_Synchronous: wait", 'R'));
      Entry_Calls.Wait_For_Completion (Self_Id, Entry_Call);
      pragma Debug
        (Debug.Trace (Self_Id, "Call_Synchronous: done waiting", 'R'));
      Rendezvous_Successful := Entry_Call.State = Done;
      STPO.Unlock (Self_Id);
      Local_Undefer_Abort (Self_Id);
      Entry_Calls.Check_Exception (Self_Id, Entry_Call);
   end Call_Synchronous;

   --------------
   -- Callable --
   --------------

   --  Compiler interface.
   --  Do not call from within the RTS,
   --  except for body of Ada.Task_Identification.

   function Callable (T : Task_ID) return Boolean is
      Result  : Boolean;
      Self_Id : constant Task_ID := STPO.Self;

   begin
      Initialization.Defer_Abort (Self_Id);
      STPO.Write_Lock (T);
      Result := T.Callable;
      STPO.Unlock (T);
      Initialization.Undefer_Abort (Self_Id);
      return Result;
   end Callable;

   ----------------------------
   -- Cancel_Task_Entry_Call --
   ----------------------------

   --  Compiler interface only.  Do not call from within the RTS.
   --  Call only with abort deferred.

   procedure Cancel_Task_Entry_Call (Cancelled : out Boolean) is
   begin
      Entry_Calls.Try_To_Cancel_Entry_Call (Cancelled);
   end Cancel_Task_Entry_Call;

   -------------------------
   -- Complete_Rendezvous --
   -------------------------

   --  See comments for Exceptional_Complete_Rendezvous.

   procedure Complete_Rendezvous is
   begin
      Exceptional_Complete_Rendezvous (Ada.Exceptions.Null_Id);
   end Complete_Rendezvous;

   -------------------------------------
   -- Exceptional_Complete_Rendezvous --
   -------------------------------------

   --  Compiler interface.
   --  Also called from Complete_Rendezvous.
   --  ?????
   --  Consider phasing out Complete_Rendezvous in favor
   --  of direct call to this with Ada.Exceptions.Null_ID.
   --  See code expansion examples for Accept_Call and Selective_Wait.
   --  ?????
   --  If we don't change the interface, consider instead
   --  putting an explicit re-raise after this call, in
   --  the generated code.  That way we could eliminate the
   --  code here that reraises the exception.

   --  The deferral level is critical here,
   --  since we want to raise an exception or allow abort to take
   --  place, if there is an exception or abort pending.

   procedure Exceptional_Complete_Rendezvous
     (Ex : Ada.Exceptions.Exception_Id)
   is
      Self_Id    : constant Task_ID := STPO.Self;
      Entry_Call : Entry_Call_Link := Self_Id.Common.Call;
      Caller     : Task_ID;
      Called_PO  : STPE.Protection_Entries_Access;

      Exception_To_Raise : Ada.Exceptions.Exception_Id := Ex;
      Ceiling_Violation  : Boolean;

      use type Ada.Exceptions.Exception_Id;
      procedure Internal_Reraise;
      pragma Import (C, Internal_Reraise, "__gnat_reraise");

      use type STPE.Protection_Entries_Access;

   begin
      pragma Debug
       (Debug.Trace (Self_Id, "Exceptional_Complete_Rendezvous", 'R'));

      if Ex = Ada.Exceptions.Null_Id then
         --  The call came from normal end-of-rendezvous,
         --  so abort is not yet deferred.
         Initialization.Defer_Abort_Nestable (Self_Id);
      end if;

      --  We need to clean up any accepts which Self may have
      --  been serving when it was aborted.

      if Ex = Standard'Abort_Signal'Identity then
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

      else
         Caller := Entry_Call.Self;

         if Entry_Call.Needs_Requeue then
            --  We dare not lock Self_Id at the same time as Caller,
            --  for fear of deadlock.

            Entry_Call.Needs_Requeue := False;
            Self_Id.Common.Call := Entry_Call.Acceptor_Prev_Call;

            if Entry_Call.Called_Task /= null then
               --  Requeue to another task entry

               if not Task_Do_Or_Queue
                 (Self_Id, Entry_Call, Entry_Call.Requeue_With_Abort)
               then
                  Initialization.Undefer_Abort (Self_Id);
                  raise Tasking_Error;
               end if;

            else
               --  Requeue to a protected entry

               Called_PO := POE.To_Protection (Entry_Call.Called_PO);
               STPE.Lock_Entries (Called_PO, Ceiling_Violation);

               if Ceiling_Violation then
                  pragma Assert (Ex = Ada.Exceptions.Null_Id);

                  Exception_To_Raise := Program_Error'Identity;
                  Entry_Call.Exception_To_Raise := Exception_To_Raise;
                  STPO.Write_Lock (Caller);
                  Initialization.Wakeup_Entry_Caller
                    (Self_Id, Entry_Call, Done);
                  STPO.Unlock (Caller);

               else
                  POO.PO_Do_Or_Queue
                    (Self_Id, Called_PO, Entry_Call,
                     Entry_Call.Requeue_With_Abort);
                  POO.PO_Service_Entries (Self_Id, Called_PO);
                  STPE.Unlock_Entries (Called_PO);
               end if;
            end if;

            Entry_Calls.Reset_Priority (Entry_Call.Acceptor_Prev_Priority,
              Self_Id);

         else
            --  The call does not need to be requeued.

            Self_Id.Common.Call := Entry_Call.Acceptor_Prev_Call;
            Entry_Call.Exception_To_Raise := Ex;
            STPO.Write_Lock (Caller);

            --  Done with Caller locked to make sure that Wakeup is not lost.

            if Ex /= Ada.Exceptions.Null_Id then
               Ada.Exceptions.Save_Occurrence
                 (Caller.Common.Compiler_Data.Current_Excep,
                  Self_Id.Common.Compiler_Data.Current_Excep);
            end if;

            Initialization.Wakeup_Entry_Caller (Self_Id, Entry_Call, Done);
            STPO.Unlock (Caller);
            Entry_Calls.Reset_Priority (Entry_Call.Acceptor_Prev_Priority,
              Self_Id);
         end if;
      end if;

      Initialization.Undefer_Abort (Self_Id);

      if Exception_To_Raise /= Ada.Exceptions.Null_Id then
         Internal_Reraise;
      end if;

      --  ?????
      --  Do we need to
      --  give precedence to Program_Error that might be raised
      --  due to failure of finalization, over Tasking_Error from
      --  failure of requeue?
   end Exceptional_Complete_Rendezvous;

   -------------------
   -- Is_Entry_Open --
   -------------------

   --  Call this only with abort deferred and holding lock of T.

   function Is_Entry_Open (T : Task_ID; E : Task_Entry_Index) return Boolean is
   begin
      pragma Assert (T.Open_Accepts /= null);

      if T.Open_Accepts /= null then
         for J in T.Open_Accepts'Range loop

            pragma Assert (J > 0);

            if E = T.Open_Accepts (J).S then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Is_Entry_Open;

   -------------------------------------
   -- Requeue_Protected_To_Task_Entry --
   -------------------------------------

   --  Compiler interface only.  Do not call from within the RTS.

   --  entry e2 when b is
   --  begin
   --     b := false;
   --     ...A...
   --     requeue t.e2;
   --  end e2;

   --  procedure rPT__E14b (O : address; P : address; E :
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
   --        requeue_protected_to_task_entry (rR'unchecked_access, tTV!(t).
   --          _task_id, 2, false);
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
   --  end rPT__E14b;

   procedure Requeue_Protected_To_Task_Entry
     (Object     : STPE.Protection_Entries_Access;
      Acceptor   : Task_ID;
      E          : Task_Entry_Index;
      With_Abort : Boolean)
   is
      Entry_Call : constant Entry_Call_Link := Object.Call_In_Progress;
   begin
      pragma Assert (STPO.Self.Deferral_Level > 0);

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Called_Task := Acceptor;
      Entry_Call.Called_PO := Null_Address;
      Entry_Call.Requeue_With_Abort := With_Abort;
      Object.Call_In_Progress := null;
   end Requeue_Protected_To_Task_Entry;

   ------------------------
   -- Requeue_Task_Entry --
   ------------------------

   --  Compiler interface only.  Do not call from within the RTS.
   --  The code generation for task entry requeues is different from that
   --  for protected entry requeues.  There is a "goto" that skips around
   --  the call to Complete_Rendezous, so that Requeue_Task_Entry must also
   --  do the work of Complete_Rendezvous.  The difference is that it does
   --  not report that the call's State = Done.

   --     accept e1 do
   --       ...A...
   --       requeue e2;
   --       ...B...
   --     end e1;

   --     A62b : address;
   --     L61b : label
   --     begin
   --        accept_call (1, A62b);
   --        ...A...
   --        requeue_task_entry (tTV!(t)._task_id, 2, false);
   --        goto L61b;
   --        ...B...
   --        complete_rendezvous;
   --        <<L61b>>
   --     exception
   --        when others =>
   --           exceptional_complete_rendezvous (current_exception);
   --     end;

   procedure Requeue_Task_Entry
     (Acceptor   : Task_ID;
      E          : Task_Entry_Index;
      With_Abort : Boolean)
   is
      Self_Id       : constant Task_ID := STPO.Self;
      Entry_Call    : constant Entry_Call_Link := Self_Id.Common.Call;

   begin
      Initialization.Defer_Abort (Self_Id);
      Entry_Call.Needs_Requeue := True;
      Entry_Call.Requeue_With_Abort := With_Abort;
      Entry_Call.E := Entry_Index (E);
      Entry_Call.Called_Task := Acceptor;
      Initialization.Undefer_Abort (Self_Id);
   end Requeue_Task_Entry;

   --------------------
   -- Selective_Wait --
   --------------------

   --  Compiler interface only.  Do not call from within the RTS.
   --  See comments on Accept_Call.

   --  source code:

   --     select accept e1 do
   --           ...A...
   --        end e1;
   --        ...B...
   --     or accept e2;
   --        ...C...
   --     end select;

   --  expansion:

   --     A32b : address;
   --     declare
   --        null;
   --        if accept_alternative'size * 2 >= 16#8000_0000# then
   --           raise storage_error;
   --        end if;
   --        A37b : T36b;
   --        A37b (1) := (null_body => false, s => 1);
   --        A37b (2) := (null_body => true, s => 2);
   --        if accept_alternative'size * 2 >= 16#8000_0000# then
   --           raise storage_error;
   --        end if;
   --        S0 : aliased T36b := accept_list'A37b;
   --        J1 : select_index := 0;
   --        L3 : label
   --        L1 : label
   --        L2 : label
   --        procedure e1A is
   --        begin
   --           abort_undefer.all;
   --           L31b : label
   --           ...A...
   --           <<L31b>>
   --           complete_rendezvous;
   --        exception
   --           when all others =>
   --              exceptional_complete_rendezvous (get_gnat_exception);
   --        end e1A;
   --     begin
   --        selective_wait (S0'unchecked_access, simple_mode, A32b, J1);
   --        case J1 is
   --           when 0 =>
   --              goto L3;
   --           when 1 =>
   --              e1A;
   --              goto L1;
   --           when 2 =>
   --              goto L2;
   --           when others =>
   --              goto L3;
   --        end case;
   --        <<L1>>
   --        ...B...
   --        goto L3;
   --        <<L2>>
   --        ...C...
   --        goto L3;
   --        <<L3>>
   --     end;

   procedure Selective_Wait
     (Open_Accepts       : Accept_List_Access;
      Select_Mode        : Select_Modes;
      Uninterpreted_Data : out System.Address;
      Index              : out Select_Index)
   is
      Self_Id          : constant Task_ID := STPO.Self;
      Entry_Call       : Entry_Call_Link;
      Treatment        : Select_Treatment;
      Caller           : Task_ID;
      Selection        : Select_Index;
      Open_Alternative : Boolean;

   begin
      Initialization.Defer_Abort (Self_Id);
      STPO.Write_Lock (Self_Id);

      if not Self_Id.Callable then
         pragma Assert (Self_Id.Pending_ATC_Level = 0);

         pragma Assert (Self_Id.Pending_Action);

         STPO.Unlock (Self_Id);

         --  ??? In some cases abort is deferred more than once. Need to figure
         --  out why.

         Self_Id.Deferral_Level := 1;

         Initialization.Undefer_Abort (Self_Id);

         --  Should never get here ???

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      --  If someone completed this task, this task should not try to
      --  access its pending entry calls or queues in this case, as they
      --  are being emptied. Wait for abortion to kill us.
      --  ?????
      --  Recheck the correctness of the above, now that we have made
      --  changes.

      pragma Assert (Open_Accepts /= null);

      Queuing.Select_Task_Entry_Call
        (Self_Id, Open_Accepts, Entry_Call, Selection, Open_Alternative);

      --  Determine the kind and disposition of the select.

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

      --  ??????
      --  Recheck the logic above against the ARM.

      --  Handle the select according to the disposition selected above.

      case Treatment is

      when Accept_Alternative_Selected =>

         --  Ready to rendezvous

         Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;

         --  In this case the accept body is not Null_Body. Defer abortion
         --  until it gets into the accept body.

         pragma Assert (Self_Id.Deferral_Level = 1);

         Initialization.Defer_Abort_Nestable (Self_Id);
         STPO.Unlock (Self_Id);

      when Accept_Alternative_Completed =>

         --  Accept body is null, so rendezvous is over immediately.

         STPO.Unlock (Self_Id);
         Caller := Entry_Call.Self;

         STPO.Write_Lock (Caller);
         Initialization.Wakeup_Entry_Caller (Self_Id, Entry_Call, Done);
         STPO.Unlock (Caller);

      when Accept_Alternative_Open =>

         --  Wait for caller.

         Self_Id.Open_Accepts := Open_Accepts;
         pragma Debug
           (Debug.Trace (Self_Id, "Selective_Wait: wait", 'R'));
         Wait_For_Call (Self_Id);

         pragma Assert (Self_Id.Open_Accepts = null);

         --  Self_Id.Common.Call should already be updated by the Caller if
         --  not aborted. It might also be ready to do rendezvous even if
         --  this wakes up due to an abortion.
         --  Therefore, if the call is not empty we need to do the rendezvous
         --  if the accept body is not Null_Body.

         --  ?????
         --  aren't the first two conditions below redundant?

         if Self_Id.Chosen_Index /= No_Rendezvous and then
           Self_Id.Common.Call /= null and then
           not Open_Accepts (Self_Id.Chosen_Index).Null_Body
         then
            Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;

            pragma Assert (Self_Id.Deferral_Level = 1);

            Initialization.Defer_Abort_Nestable (Self_Id);

            --  Leave abort deferred until the accept body
         end if;

         STPO.Unlock (Self_Id);

      when Else_Selected =>
         pragma Assert (Self_Id.Open_Accepts = null);

         STPO.Unlock (Self_Id);

      when Terminate_Selected =>

         --  Terminate alternative is open

         Self_Id.Open_Accepts := Open_Accepts;
         Self_Id.Common.State := Acceptor_Sleep;
         STPO.Unlock (Self_Id);

         --  ?????
         --  We need to check if a signal is pending on an open interrupt
         --  entry. Otherwise this task would become potentially terminatable
         --  and, if none of the siblings are active
         --  any more, the task could not wake up any more, even though a
         --  signal might be pending on an open interrupt entry.
         --  -------------
         --  This comment paragraph does not make sense.  Is it obsolete?
         --  There was no code here to check for pending signals.

         --  Notify ancestors that this task is on a terminate alternative.

         Utilities.Make_Passive (Self_Id, Task_Completed => False);

         --  Wait for normal entry call or termination

         pragma Assert (Self_Id.ATC_Nesting_Level = 1);

         STPO.Write_Lock (Self_Id);

         loop
            Initialization.Poll_Base_Priority_Change (Self_Id);
            exit when Self_Id.Open_Accepts = null;
            Sleep (Self_Id, Acceptor_Sleep);
         end loop;

         Self_Id.Common.State := Runnable;

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

            --  Trust that it is OK to fall through.

            null;

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
               Initialization.Poll_Base_Priority_Change (Self_Id);
               exit when Self_Id.Pending_ATC_Level < Self_Id.ATC_Nesting_Level;
               Sleep (Self_Id, Delay_Sleep);
            end loop;

            Self_Id.Common.State := Runnable;
            STPO.Unlock (Self_Id);

         else
            STPO.Unlock (Self_Id);
            Initialization.Undefer_Abort (Self_Id);
            Ada.Exceptions.Raise_Exception (Program_Error'Identity,
              "Entry call not a delay mode");
         end if;

      end case;

      --  Caller has been chosen.
      --  Self_Id.Common.Call should already be updated by the Caller.
      --  Self_Id.Chosen_Index should either be updated by the Caller
      --  or by Test_Selective_Wait.
      --  On return, we sill start rendezvous unless the accept body is
      --  null.  In the latter case, we will have already completed the RV.

      Index := Self_Id.Chosen_Index;
      Initialization.Undefer_Abort_Nestable (Self_Id);

   end Selective_Wait;

   ------------------------------------
   -- Setup_For_Rendezvous_With_Body --
   ------------------------------------

   --  Call this only with abort deferred and holding lock of Acceptor.

   procedure Setup_For_Rendezvous_With_Body
     (Entry_Call : Entry_Call_Link;
      Acceptor   : Task_ID)
   is
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

   --  Compiler interface only.  Do not call from within the RTS.

   function Task_Count (E : Task_Entry_Index) return Natural is
      Self_Id      : constant Task_ID := STPO.Self;
      Return_Count : Natural;

   begin
      Initialization.Defer_Abort (Self_Id);
      STPO.Write_Lock (Self_Id);
      Return_Count := Queuing.Count_Waiting (Self_Id.Entry_Queues (E));
      STPO.Unlock (Self_Id);
      Initialization.Undefer_Abort (Self_Id);
      return Return_Count;
   end Task_Count;

   ----------------------
   -- Task_Do_Or_Queue --
   ----------------------

   --  Call this only with abort deferred and holding no locks.
   --  May propagate an exception, including Abort_Signal & Tasking_Error.
   --  ?????
   --  See Check_Callable.  Check all call contexts to verify
   --  it is OK to raise an exception.

   --  Find out whether Entry_Call can be accepted immediately.
   --  If the Acceptor is not callable, raise Tasking_Error.
   --  If the rendezvous can start, initiate it.
   --  If the accept-body is trivial, also complete the rendezvous.
   --  If the acceptor is not ready, enqueue the call.

   --  ?????
   --  This should have a special case for Accept_Call and
   --  Accept_Trivial, so that
   --  we don't have the loop setup overhead, below.

   --  ?????
   --  The call state Done is used here and elsewhere to include
   --  both the case of normal successful completion, and the case
   --  of an exception being raised.  The difference is that if an
   --  exception is raised no one will pay attention to the fact
   --  that State = Done.  Instead the exception will be raised in
   --  Undefer_Abort, and control will skip past the place where
   --  we normally would resume from an entry call.

   function Task_Do_Or_Queue
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link;
      With_Abort : Boolean) return Boolean
   is
      E         : constant Task_Entry_Index := Task_Entry_Index (Entry_Call.E);
      Old_State : constant Entry_Call_State := Entry_Call.State;
      Acceptor  : constant Task_ID := Entry_Call.Called_Task;
      Parent    : constant Task_ID := Acceptor.Common.Parent;
      Parent_Locked : Boolean := False;
      Null_Body : Boolean;

   begin
      pragma Assert (not Queuing.Onqueue (Entry_Call));

      --  We rely that the call is off-queue for protection,
      --  that the caller will not exit the Entry_Caller_Sleep,
      --  and so will not reuse the call record for another call.
      --  We rely on the Caller's lock for call State mod's.

      --  We can't lock Acceptor.Parent while holding Acceptor,
      --  so lock it in advance if we expect to need to lock it.
      --  ?????
      --  Is there some better solution?

      if Acceptor.Terminate_Alternative then
         STPO.Write_Lock (Parent);
         Parent_Locked := True;
      end if;

      STPO.Write_Lock (Acceptor);

      --  If the acceptor is not callable, abort the call
      --  and raise Tasking_Error.  The call is not aborted
      --  for an asynchronous call, since Cancel_Task_Entry_Call
      --  will do the cancelation in that case.
      --  ????? .....
      --  Does the above still make sense?

      if not Acceptor.Callable then
         STPO.Unlock (Acceptor);

         if Parent_Locked then
            STPO.Unlock (Acceptor.Common.Parent);
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

      --  Try to serve the call immediately.

      if Acceptor.Open_Accepts /= null then
         for J in Acceptor.Open_Accepts'Range loop
            if Entry_Call.E = Entry_Index (Acceptor.Open_Accepts (J).S) then

               --  Commit acceptor to rendezvous with us.

               Acceptor.Chosen_Index := J;
               Null_Body := Acceptor.Open_Accepts (J).Null_Body;
               Acceptor.Open_Accepts := null;

               --  Prevent abort while call is being served.

               if Entry_Call.State = Now_Abortable then
                  Entry_Call.State := Was_Abortable;
               end if;

               if Acceptor.Terminate_Alternative then

                  --  Cancel terminate alternative.
                  --  See matching code in Selective_Wait and
                  --  Vulnerable_Complete_Master.

                  Acceptor.Terminate_Alternative := False;
                  Acceptor.Awake_Count := Acceptor.Awake_Count + 1;

                  if Acceptor.Awake_Count = 1 then

                     --  Notify parent that acceptor is awake.

                     pragma Assert (Parent.Awake_Count > 0);

                     Parent.Awake_Count := Parent.Awake_Count + 1;

                     if Parent.Common.State = Master_Completion_Sleep and then
                        Acceptor.Master_of_Task = Parent.Master_Within
                     then
                        Parent.Common.Wait_Count :=
                          Parent.Common.Wait_Count + 1;
                     end if;
                  end if;
               end if;

               if Null_Body then

                  --  Rendezvous is over immediately.

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

                  --  For terminate_alternative, acceptor may not be
                  --  asleep yet, so we skip the wakeup

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

         --  The acceptor is accepting, but not this entry.
      end if;

      --  If the acceptor was ready to accept this call,
      --  we would not have gotten this far, so now we should
      --  (re)enqueue the call, if the mode permits that.

      if Entry_Call.Mode /= Conditional_Call
        or else not With_Abort
      then
         --  Timed_Call, Simple_Call, or Asynchronous_Call

         Queuing.Enqueue (Acceptor.Entry_Queues (E), Entry_Call);

         --  Update abortability of call

         pragma Assert (Old_State < Done);

         Entry_Call.State := New_State (With_Abort, Entry_Call.State);

         STPO.Unlock (Acceptor);

         if Parent_Locked then
            STPO.Unlock (Parent);
         end if;

         if Old_State /= Entry_Call.State and then
           Entry_Call.State = Now_Abortable and then
           Entry_Call.Mode > Simple_Call and then

            --  Asynchronous_Call or Conditional_Call

           Entry_Call.Self /= Self_ID

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
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Mode                  : Call_Modes;
      Rendezvous_Successful : out Boolean)
   is
      Self_Id    : constant Task_ID := STPO.Self;
      Entry_Call : Entry_Call_Link;

   begin
      if Mode = Simple_Call or else Mode = Conditional_Call then
         Call_Synchronous
           (Acceptor, E, Uninterpreted_Data, Mode, Rendezvous_Successful);

      else
         --  This is an asynchronous call

         --  Abortion must already be deferred by the compiler-generated
         --  code.  Without this, an abortion that occurs between the time
         --  that this call is made and the time that the abortable part's
         --  cleanup handler is set up might miss the cleanup handler and
         --  leave the call pending.

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

         if not Task_Do_Or_Queue
           (Self_Id, Entry_Call, With_Abort => True)
         then
            Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level - 1;
            pragma Debug
              (Debug.Trace (Self_Id, "TEC: exited to ATC level: " &
               ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));
            Initialization.Undefer_Abort (Self_Id);
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

         --  Note: following assignment needs to be atomic.

         Rendezvous_Successful := Entry_Call.State = Done;
      end if;
   end Task_Entry_Call;

   -----------------------
   -- Task_Entry_Caller --
   -----------------------

   --  Compiler interface only.

   function Task_Entry_Caller (D : Task_Entry_Nesting_Depth) return Task_ID is
      Self_Id    : constant Task_ID := STPO.Self;
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

   --  Compiler interface only.  Do not call from within the RTS.

   procedure Timed_Selective_Wait
     (Open_Accepts       : Accept_List_Access;
      Select_Mode        : Select_Modes;
      Uninterpreted_Data : out System.Address;
      Timeout            : Duration;
      Mode               : Delay_Modes;
      Index              : out Select_Index)
   is
      Self_Id          : constant Task_ID := STPO.Self;
      Treatment        : Select_Treatment;
      Entry_Call       : Entry_Call_Link;
      Caller           : Task_ID;
      Selection        : Select_Index;
      Open_Alternative : Boolean;
      Timedout         : Boolean := False;
      Yielded          : Boolean := False;
   begin
      pragma Assert (Select_Mode = Delay_Mode);

      Initialization.Defer_Abort (Self_Id);

      --  If we are aborted here, the effect will be pending

      STPO.Write_Lock (Self_Id);

      if not Self_Id.Callable then
         pragma Assert (Self_Id.Pending_ATC_Level = 0);

         pragma Assert (Self_Id.Pending_Action);

         STPO.Unlock (Self_Id);
         Initialization.Undefer_Abort (Self_Id);

         --  Should never get here ???

         pragma Assert (False);
         raise Standard'Abort_Signal;
      end if;

      --  If someone completed this task, this task should not try to
      --  access its pending entry calls or queues in this case, as they
      --  are being emptied. Wait for abortion to kill us.
      --  ?????
      --  Recheck the correctness of the above, now that we have made
      --  changes.

      pragma Assert (Open_Accepts /= null);

      Queuing.Select_Task_Entry_Call
        (Self_Id, Open_Accepts, Entry_Call, Selection, Open_Alternative);

      --  Determine the kind and disposition of the select.

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

      --  Handle the select according to the disposition selected above.

      case Treatment is

      when Accept_Alternative_Selected =>

         --  Ready to rendezvous
         --  In this case the accept body is not Null_Body. Defer abortion
         --  until it gets into the accept body.

         Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;
         Initialization.Defer_Abort (Self_Id);
         STPO.Unlock (Self_Id);

      when Accept_Alternative_Completed =>

         --  Rendezvous is over

         STPO.Unlock (Self_Id);
         Caller := Entry_Call.Self;

         STPO.Write_Lock (Caller);
         Initialization.Wakeup_Entry_Caller (Self_Id, Entry_Call, Done);
         STPO.Unlock (Caller);

      when Accept_Alternative_Open =>

         --  Wait for caller.

         Self_Id.Open_Accepts := Open_Accepts;

         --  Wait for a normal call and a pending action until the
         --  Wakeup_Time is reached.

         Self_Id.Common.State := Acceptor_Sleep;

         loop
            Initialization.Poll_Base_Priority_Change (Self_Id);
            exit when Self_Id.Open_Accepts = null;

            if Timedout then
               Sleep (Self_Id, Acceptor_Sleep);
            else
               STPO.Timed_Sleep (Self_Id, Timeout, Mode,
                 Acceptor_Sleep, Timedout, Yielded);
            end if;

            if Timedout then
               Self_Id.Open_Accepts := null;
            end if;
         end loop;

         Self_Id.Common.State := Runnable;

         --  Self_Id.Common.Call should already be updated by the Caller if
         --  not aborted. It might also be ready to do rendezvous even if
         --  this wakes up due to an abortion.
         --  Therefore, if the call is not empty we need to do the rendezvous
         --  if the accept body is not Null_Body.

         if Self_Id.Chosen_Index /= No_Rendezvous and then
           Self_Id.Common.Call /= null and then
           not Open_Accepts (Self_Id.Chosen_Index).Null_Body
         then
            Uninterpreted_Data := Self_Id.Common.Call.Uninterpreted_Data;

            pragma Assert (Self_Id.Deferral_Level = 1);

            Initialization.Defer_Abort_Nestable (Self_Id);

            --  Leave abort deferred until the accept body

         end if;

         STPO.Unlock (Self_Id);
         if not Yielded then
            Yield;
         end if;

      when No_Alternative_Open =>

         --  In this case, Index will be No_Rendezvous on return. We sleep
         --  for the time we need to.
         --  Wait for a signal or timeout. A wakeup can be made
         --  for several reasons:
         --  1) Delay is expired
         --  2) Pending_Action needs to be checked
         --     (Abortion, Priority change)
         --  3) Spurious wakeup

         Self_Id.Open_Accepts := null;
         Self_Id.Common.State := Acceptor_Sleep;

         Initialization.Poll_Base_Priority_Change (Self_Id);

         STPO.Timed_Sleep (Self_Id, Timeout, Mode, Acceptor_Sleep,
           Timedout, Yielded);

         Self_Id.Common.State := Runnable;

         STPO.Unlock (Self_Id);

         if not Yielded then
            Yield;
         end if;

      when others =>
         --  Should never get here ???

         pragma Assert (False);
         null;
      end case;

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

   --  Compiler interface only.  Do not call from within the RTS.

   procedure Timed_Task_Entry_Call
     (Acceptor              : Task_ID;
      E                     : Task_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Timeout               : Duration;
      Mode                  : Delay_Modes;
      Rendezvous_Successful : out Boolean)
   is
      Self_Id    : constant Task_ID := STPO.Self;
      Level      : ATC_Level;
      Entry_Call : Entry_Call_Link;

   begin
      Initialization.Defer_Abort (Self_Id);
      Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level + 1;

      pragma Debug
        (Debug.Trace (Self_Id, "TTEC: entered ATC level: " &
         ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));

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

      --  Note: the caller will undefer abortion on return (see WARNING above)

      if not Task_Do_Or_Queue
       (Self_Id, Entry_Call, With_Abort => True)
      then
         Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level - 1;

         pragma Debug
           (Debug.Trace (Self_Id, "TTEC: exited to ATC level: " &
            ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));

         Initialization.Undefer_Abort (Self_Id);
         raise Tasking_Error;
      end if;

      Entry_Calls.Wait_For_Completion_With_Timeout
        (Self_Id, Entry_Call, Timeout, Mode);
      Rendezvous_Successful := Entry_Call.State = Done;
      Initialization.Undefer_Abort (Self_Id);
      Entry_Calls.Check_Exception (Self_Id, Entry_Call);
   end Timed_Task_Entry_Call;

   -------------------
   -- Wait_For_Call --
   -------------------

   --  Call this only with abort deferred and holding lock of Self_Id.
   --  Wait for normal call and a pending action.

   procedure Wait_For_Call (Self_Id : Task_ID) is
   begin
      Self_Id.Common.State := Acceptor_Sleep;

      loop
         Initialization.Poll_Base_Priority_Change (Self_Id);

         exit when Self_Id.Open_Accepts = null;

         Sleep (Self_Id, Acceptor_Sleep);
      end loop;

      Self_Id.Common.State := Runnable;
   end Wait_For_Call;

end System.Tasking.Rendezvous;
