------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--             S Y S T E M . T A S K I N G . E N T R Y _ C A L L S          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2007, Free Software Foundation, Inc.          --
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

with System.Task_Primitives.Operations;
--  used for STPO.Write_Lock
--           Unlock
--           STPO.Get_Priority
--           Sleep
--           Timed_Sleep

with System.Tasking.Initialization;
--  used for Change_Base_Priority
--           Defer_Abort/Undefer_Abort

with System.Tasking.Protected_Objects.Entries;
--  used for To_Protection

with System.Tasking.Protected_Objects.Operations;
--  used for PO_Service_Entries

with System.Tasking.Queuing;
--  used for Requeue_Call_With_New_Prio
--           Onqueue
--           Dequeue_Call

with System.Tasking.Utilities;
--  used for Exit_One_ATC_Level

with System.Parameters;
--  used for Single_Lock
--           Runtime_Traces

with System.Traces;
--  used for Send_Trace_Info

package body System.Tasking.Entry_Calls is

   package STPO renames System.Task_Primitives.Operations;

   use Parameters;
   use Task_Primitives;
   use Protected_Objects.Entries;
   use Protected_Objects.Operations;
   use System.Traces;

   --  DO NOT use Protected_Objects.Lock or Protected_Objects.Unlock
   --  internally. Those operations will raise Program_Error, which
   --  we are not prepared to handle inside the RTS. Instead, use
   --  System.Task_Primitives lock operations directly on Protection.L.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Lock_Server (Entry_Call : Entry_Call_Link);

   --  This locks the server targeted by Entry_Call
   --
   --  This may be a task or a protected object, depending on the target of the
   --  original call or any subsequent requeues.
   --
   --  This routine is needed because the field specifying the server for this
   --  call must be protected by the server's mutex. If it were protected by
   --  the caller's mutex, accessing the server's queues would require locking
   --  the caller to get the server, locking the server, and then accessing the
   --  queues. This involves holding two ATCB locks at once, something which we
   --  can guarantee that it will always be done in the same order, or locking
   --  a protected object while we hold an ATCB lock, something which is not
   --  permitted. Since the server cannot be obtained reliably, it must be
   --  obtained unreliably and then checked again once it has been locked.
   --
   --  If Single_Lock and server is a PO, release RTS_Lock
   --
   --  This should only be called by the Entry_Call.Self.
   --  It should be holding no other ATCB locks at the time.

   procedure Unlock_Server (Entry_Call : Entry_Call_Link);
   --  STPO.Unlock the server targeted by Entry_Call. The server must
   --  be locked before calling this.
   --
   --  If Single_Lock and server is a PO, take RTS_Lock on exit.

   procedure Unlock_And_Update_Server
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link);
   --  Similar to Unlock_Server, but services entry calls if the
   --  server is a protected object.
   --
   --  If Single_Lock and server is a PO, take RTS_Lock on exit.

   procedure Check_Pending_Actions_For_Entry_Call
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link);
   --  This procedure performs priority change of a queued call and dequeuing
   --  of an entry call when the call is cancelled. If the call is dequeued the
   --  state should be set to Cancelled. Call only with abort deferred and
   --  holding lock of Self_ID. This is a bit of common code for all entry
   --  calls. The effect is to do any deferred base priority change operation,
   --  in case some other task called STPO.Set_Priority while the current task
   --  had abort deferred, and to dequeue the call if the call has been
   --  aborted.

   procedure Poll_Base_Priority_Change_At_Entry_Call
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link);
   pragma Inline (Poll_Base_Priority_Change_At_Entry_Call);
   --  A specialized version of Poll_Base_Priority_Change, that does the
   --  optional entry queue reordering. Has to be called with the Self_ID's
   --  ATCB write-locked. May temporariliy release the lock.

   ---------------------
   -- Check_Exception --
   ---------------------

   procedure Check_Exception
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link)
   is
      pragma Warnings (Off, Self_ID);

      use type Ada.Exceptions.Exception_Id;

      procedure Internal_Raise (X : Ada.Exceptions.Exception_Id);
      pragma Import (C, Internal_Raise, "__gnat_raise_with_msg");

      E : constant Ada.Exceptions.Exception_Id :=
            Entry_Call.Exception_To_Raise;
   begin
      --  pragma Assert (Self_ID.Deferral_Level = 0);

      --  The above may be useful for debugging, but the Florist packages
      --  contain critical sections that defer abort and then do entry calls,
      --  which causes the above Assert to trip.

      if E /= Ada.Exceptions.Null_Id then
         Internal_Raise (E);
      end if;
   end Check_Exception;

   ------------------------------------------
   -- Check_Pending_Actions_For_Entry_Call --
   ------------------------------------------

   procedure Check_Pending_Actions_For_Entry_Call
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link)
   is
   begin
      pragma Assert (Self_ID = Entry_Call.Self);

      Poll_Base_Priority_Change_At_Entry_Call (Self_ID, Entry_Call);

      if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
        and then Entry_Call.State = Now_Abortable
      then
         STPO.Unlock (Self_ID);
         Lock_Server (Entry_Call);

         if Queuing.Onqueue (Entry_Call)
           and then Entry_Call.State = Now_Abortable
         then
            Queuing.Dequeue_Call (Entry_Call);

            if Entry_Call.Cancellation_Attempted then
               Entry_Call.State := Cancelled;
            else
               Entry_Call.State := Done;
            end if;

            Unlock_And_Update_Server (Self_ID, Entry_Call);

         else
            Unlock_Server (Entry_Call);
         end if;

         STPO.Write_Lock (Self_ID);
      end if;
   end Check_Pending_Actions_For_Entry_Call;

   -----------------
   -- Lock_Server --
   -----------------

   procedure Lock_Server (Entry_Call : Entry_Call_Link) is
      Test_Task         : Task_Id;
      Test_PO           : Protection_Entries_Access;
      Ceiling_Violation : Boolean;
      Failures          : Integer := 0;

   begin
      Test_Task := Entry_Call.Called_Task;

      loop
         if Test_Task = null then

            --  Entry_Call was queued on a protected object, or in transition,
            --  when we last fetched Test_Task.

            Test_PO := To_Protection (Entry_Call.Called_PO);

            if Test_PO = null then

               --  We had very bad luck, interleaving with TWO different
               --  requeue operations. Go around the loop and try again.

               if Single_Lock then
                  STPO.Unlock_RTS;
                  STPO.Yield;
                  STPO.Lock_RTS;
               else
                  STPO.Yield;
               end if;

            else
               if Single_Lock then
                  STPO.Unlock_RTS;
               end if;

               Lock_Entries (Test_PO, Ceiling_Violation);

               --  ???

               --  The following code allows Lock_Server to be called when
               --  cancelling a call, to allow for the possibility that the
               --  priority of the caller has been raised beyond that of the
               --  protected entry call by Ada.Dynamic_Priorities.Set_Priority.

               --  If the current task has a higher priority than the ceiling
               --  of the protected object, temporarily lower it. It will
               --  be reset in Unlock.

               if Ceiling_Violation then
                  declare
                     Current_Task      : constant Task_Id := STPO.Self;
                     Old_Base_Priority : System.Any_Priority;

                  begin
                     if Single_Lock then
                        STPO.Lock_RTS;
                     end if;

                     STPO.Write_Lock (Current_Task);
                     Old_Base_Priority := Current_Task.Common.Base_Priority;
                     Current_Task.New_Base_Priority := Test_PO.Ceiling;
                     System.Tasking.Initialization.Change_Base_Priority
                       (Current_Task);
                     STPO.Unlock (Current_Task);

                     if Single_Lock then
                        STPO.Unlock_RTS;
                     end if;

                     --  Following lock should not fail

                     Lock_Entries (Test_PO);

                     Test_PO.Old_Base_Priority := Old_Base_Priority;
                     Test_PO.Pending_Action := True;
                  end;
               end if;

               exit when To_Address (Test_PO) = Entry_Call.Called_PO;
               Unlock_Entries (Test_PO);

               if Single_Lock then
                  STPO.Lock_RTS;
               end if;
            end if;

         else
            STPO.Write_Lock (Test_Task);
            exit when Test_Task = Entry_Call.Called_Task;
            STPO.Unlock (Test_Task);
         end if;

         Test_Task := Entry_Call.Called_Task;
         Failures := Failures + 1;
         pragma Assert (Failures <= 5);
      end loop;
   end Lock_Server;

   ---------------------------------------------
   -- Poll_Base_Priority_Change_At_Entry_Call --
   ---------------------------------------------

   procedure Poll_Base_Priority_Change_At_Entry_Call
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link)
   is
   begin
      if Self_ID.Pending_Priority_Change then

         --  Check for ceiling violations ???

         Self_ID.Pending_Priority_Change := False;

         --  Requeue the entry call at the new priority. We need to requeue
         --  even if the new priority is the same than the previous (see ACATS
         --  test cxd4006).

         STPO.Unlock (Self_ID);
         Lock_Server (Entry_Call);
         Queuing.Requeue_Call_With_New_Prio
           (Entry_Call, STPO.Get_Priority (Self_ID));
         Unlock_And_Update_Server (Self_ID, Entry_Call);
         STPO.Write_Lock (Self_ID);
      end if;
   end Poll_Base_Priority_Change_At_Entry_Call;

   --------------------
   -- Reset_Priority --
   --------------------

   procedure Reset_Priority
     (Acceptor               : Task_Id;
      Acceptor_Prev_Priority : Rendezvous_Priority)
   is
   begin
      pragma Assert (Acceptor = STPO.Self);

      --  Since we limit this kind of "active" priority change to be done
      --  by the task for itself, we don't need to lock Acceptor.

      if Acceptor_Prev_Priority /= Priority_Not_Boosted then
         STPO.Set_Priority (Acceptor, Acceptor_Prev_Priority,
           Loss_Of_Inheritance => True);
      end if;
   end Reset_Priority;

   ------------------------------
   -- Try_To_Cancel_Entry_Call --
   ------------------------------

   procedure Try_To_Cancel_Entry_Call (Succeeded : out Boolean) is
      Entry_Call : Entry_Call_Link;
      Self_ID    : constant Task_Id := STPO.Self;

      use type Ada.Exceptions.Exception_Id;

   begin
      Entry_Call := Self_ID.Entry_Calls (Self_ID.ATC_Nesting_Level)'Access;

      --  Experimentation has shown that abort is sometimes (but not
      --  always) already deferred when Cancel_xxx_Entry_Call is called.
      --  That may indicate an error. Find out what is going on. ???

      pragma Assert (Entry_Call.Mode = Asynchronous_Call);
      Initialization.Defer_Abort_Nestable (Self_ID);

      if Single_Lock then
         STPO.Lock_RTS;
      end if;

      STPO.Write_Lock (Self_ID);
      Entry_Call.Cancellation_Attempted := True;

      if Self_ID.Pending_ATC_Level >= Entry_Call.Level then
         Self_ID.Pending_ATC_Level := Entry_Call.Level - 1;
      end if;

      Entry_Calls.Wait_For_Completion (Entry_Call);
      STPO.Unlock (Self_ID);

      if Single_Lock then
         STPO.Unlock_RTS;
      end if;

      Succeeded := Entry_Call.State = Cancelled;

      Initialization.Undefer_Abort_Nestable (Self_ID);

      --  Ideally, abort should no longer be deferred at this point, so we
      --  should be able to call Check_Exception. The loop below should be
      --  considered temporary, to work around the possibility that abort
      --  may be deferred more than one level deep ???

      if Entry_Call.Exception_To_Raise /= Ada.Exceptions.Null_Id then
         while Self_ID.Deferral_Level > 0 loop
            System.Tasking.Initialization.Undefer_Abort_Nestable (Self_ID);
         end loop;

         Entry_Calls.Check_Exception (Self_ID, Entry_Call);
      end if;
   end Try_To_Cancel_Entry_Call;

   ------------------------------
   -- Unlock_And_Update_Server --
   ------------------------------

   procedure Unlock_And_Update_Server
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link)
   is
      Called_PO : Protection_Entries_Access;
      Caller    : Task_Id;

   begin
      if Entry_Call.Called_Task /= null then
         STPO.Unlock (Entry_Call.Called_Task);
      else
         Called_PO := To_Protection (Entry_Call.Called_PO);
         PO_Service_Entries (Self_ID, Called_PO, False);

         if Called_PO.Pending_Action then
            Called_PO.Pending_Action := False;
            Caller := STPO.Self;

            if Single_Lock then
               STPO.Lock_RTS;
            end if;

            STPO.Write_Lock (Caller);
            Caller.New_Base_Priority := Called_PO.Old_Base_Priority;
            Initialization.Change_Base_Priority (Caller);
            STPO.Unlock (Caller);

            if Single_Lock then
               STPO.Unlock_RTS;
            end if;
         end if;

         Unlock_Entries (Called_PO);

         if Single_Lock then
            STPO.Lock_RTS;
         end if;
      end if;
   end Unlock_And_Update_Server;

   -------------------
   -- Unlock_Server --
   -------------------

   procedure Unlock_Server (Entry_Call : Entry_Call_Link) is
      Caller    : Task_Id;
      Called_PO : Protection_Entries_Access;

   begin
      if Entry_Call.Called_Task /= null then
         STPO.Unlock (Entry_Call.Called_Task);
      else
         Called_PO := To_Protection (Entry_Call.Called_PO);

         if Called_PO.Pending_Action then
            Called_PO.Pending_Action := False;
            Caller := STPO.Self;

            if Single_Lock then
               STPO.Lock_RTS;
            end if;

            STPO.Write_Lock (Caller);
            Caller.New_Base_Priority := Called_PO.Old_Base_Priority;
            Initialization.Change_Base_Priority (Caller);
            STPO.Unlock (Caller);

            if Single_Lock then
               STPO.Unlock_RTS;
            end if;
         end if;

         Unlock_Entries (Called_PO);

         if Single_Lock then
            STPO.Lock_RTS;
         end if;
      end if;
   end Unlock_Server;

   -------------------------
   -- Wait_For_Completion --
   -------------------------

   procedure Wait_For_Completion (Entry_Call : Entry_Call_Link) is
      Self_Id : constant Task_Id := Entry_Call.Self;

   begin
      --  If this is a conditional call, it should be cancelled when it
      --  becomes abortable. This is checked in the loop below.

      if Parameters.Runtime_Traces then
         Send_Trace_Info (W_Completion);
      end if;

      Self_Id.Common.State := Entry_Caller_Sleep;

      --  Try to remove calls to Sleep in the loop below by letting the caller
      --  a chance of getting ready immediately, using Unlock & Yield.
      --  See similar action in Wait_For_Call & Timed_Selective_Wait.

      if Single_Lock then
         STPO.Unlock_RTS;
      else
         STPO.Unlock (Self_Id);
      end if;

      if Entry_Call.State < Done then
         STPO.Yield;
      end if;

      if Single_Lock then
         STPO.Lock_RTS;
      else
         STPO.Write_Lock (Self_Id);
      end if;

      loop
         Check_Pending_Actions_For_Entry_Call (Self_Id, Entry_Call);

         exit when Entry_Call.State >= Done;

         STPO.Sleep (Self_Id, Entry_Caller_Sleep);
      end loop;

      Self_Id.Common.State := Runnable;
      Utilities.Exit_One_ATC_Level (Self_Id);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (M_Call_Complete);
      end if;
   end Wait_For_Completion;

   --------------------------------------
   -- Wait_For_Completion_With_Timeout --
   --------------------------------------

   procedure Wait_For_Completion_With_Timeout
     (Entry_Call  : Entry_Call_Link;
      Wakeup_Time : Duration;
      Mode        : Delay_Modes;
      Yielded     : out Boolean)
   is
      Self_Id  : constant Task_Id := Entry_Call.Self;
      Timedout : Boolean := False;

      use type Ada.Exceptions.Exception_Id;

   begin
      --  This procedure waits for the entry call to be served, with a timeout.
      --  It tries to cancel the call if the timeout expires before the call is
      --  served.

      --  If we wake up from the timed sleep operation here, it may be for
      --  several possible reasons:

      --  1) The entry call is done being served.
      --  2) There is an abort or priority change to be served.
      --  3) The timeout has expired (Timedout = True)
      --  4) There has been a spurious wakeup.

      --  Once the timeout has expired we may need to continue to wait if the
      --  call is already being serviced. In that case, we want to go back to
      --  sleep, but without any timeout. The variable Timedout is used to
      --  control this. If the Timedout flag is set, we do not need to
      --  STPO.Sleep with a timeout. We just sleep until we get a wakeup for
      --  some status change.

      --  The original call may have become abortable after waking up. We want
      --  to check Check_Pending_Actions_For_Entry_Call again in any case.

      pragma Assert (Entry_Call.Mode = Timed_Call);

      Yielded := False;
      Self_Id.Common.State := Entry_Caller_Sleep;

      --  Looping is necessary in case the task wakes up early from the timed
      --  sleep, due to a "spurious wakeup". Spurious wakeups are a weakness of
      --  POSIX condition variables. A thread waiting for a condition variable
      --  is allowed to wake up at any time, not just when the condition is
      --  signaled. See same loop in the ordinary Wait_For_Completion, above.

      if Parameters.Runtime_Traces then
         Send_Trace_Info (WT_Completion, Wakeup_Time);
      end if;

      loop
         Check_Pending_Actions_For_Entry_Call (Self_Id, Entry_Call);
         exit when Entry_Call.State >= Done;

         STPO.Timed_Sleep (Self_Id, Wakeup_Time, Mode,
           Entry_Caller_Sleep, Timedout, Yielded);

         if Timedout then
            if Parameters.Runtime_Traces then
               Send_Trace_Info (E_Timeout);
            end if;

            --  Try to cancel the call (see Try_To_Cancel_Entry_Call for
            --  corresponding code in the ATC case).

            Entry_Call.Cancellation_Attempted := True;

            if Self_Id.Pending_ATC_Level >= Entry_Call.Level then
               Self_Id.Pending_ATC_Level := Entry_Call.Level - 1;
            end if;

            --  The following loop is the same as the loop and exit code
            --  from the ordinary Wait_For_Completion. If we get here, we
            --  have timed out but we need to keep waiting until the call
            --  has actually completed or been cancelled successfully.

            loop
               Check_Pending_Actions_For_Entry_Call (Self_Id, Entry_Call);
               exit when Entry_Call.State >= Done;
               STPO.Sleep (Self_Id, Entry_Caller_Sleep);
            end loop;

            Self_Id.Common.State := Runnable;
            Utilities.Exit_One_ATC_Level (Self_Id);

            return;
         end if;
      end loop;

      --  This last part is the same as ordinary Wait_For_Completion,
      --  and is only executed if the call completed without timing out.

      if Parameters.Runtime_Traces then
         Send_Trace_Info (M_Call_Complete);
      end if;

      Self_Id.Common.State := Runnable;
      Utilities.Exit_One_ATC_Level (Self_Id);
   end Wait_For_Completion_With_Timeout;

   --------------------------
   -- Wait_Until_Abortable --
   --------------------------

   procedure Wait_Until_Abortable
     (Self_ID : Task_Id;
      Call    : Entry_Call_Link)
   is
   begin
      pragma Assert (Self_ID.ATC_Nesting_Level > 0);
      pragma Assert (Call.Mode = Asynchronous_Call);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (W_Completion);
      end if;

      STPO.Write_Lock (Self_ID);
      Self_ID.Common.State := Entry_Caller_Sleep;

      loop
         Check_Pending_Actions_For_Entry_Call (Self_ID, Call);
         exit when Call.State >= Was_Abortable;
         STPO.Sleep (Self_ID, Async_Select_Sleep);
      end loop;

      Self_ID.Common.State := Runnable;
      STPO.Unlock (Self_ID);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (M_Call_Complete);
      end if;
   end Wait_Until_Abortable;

end System.Tasking.Entry_Calls;
