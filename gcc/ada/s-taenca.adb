------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--             S Y S T E M . T A S K I N G . E N T R Y _ C A L L S          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.36 $
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

--  This package provides internal RTS calls implementing operations
--  that apply to general entry calls, that is, calls to either
--  protected or task entries.

--  These declarations are not part of the GNARL interface

with System.Task_Primitives.Operations;
--  used for STPO.Write_Lock
--           Unlock
--           STPO.Get_Priority
--           Sleep
--           Timed_Sleep

with System.Tasking.Initialization;
--  used for Change_Base_Priority
--           Poll_Base_Priority_Change_At_Entry_Call
--           Dynamic_Priority_Support
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

package body System.Tasking.Entry_Calls is

   package STPO renames System.Task_Primitives.Operations;

   use System.Task_Primitives;
   use System.Tasking.Protected_Objects.Entries;
   use System.Tasking.Protected_Objects.Operations;

   --  DO NOT use Protected_Objects.Lock or Protected_Objects.Unlock
   --  internally. Those operations will raise Program_Error, which
   --  we do are not prepared to handle inside the RTS. Instead, use
   --  System.Task_Primitives lock operations directly on Protection.L.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Lock_Server (Entry_Call : Entry_Call_Link);
   --  This locks the server targeted by Entry_Call.
   --
   --  This may be a task or a protected object,
   --  depending on the target of the original call or any subsequent
   --  requeues.
   --
   --  This routine is needed because the field specifying the server
   --  for this call must be protected by the server's mutex. If it were
   --  protected by the caller's mutex, accessing the server's queues would
   --  require locking the caller to get the server, locking the server,
   --  and then accessing the queues. This involves holding two ATCB
   --  locks at once, something which we can guarantee that it will always
   --  be done in the same order, or locking a protected object while we
   --  hold an ATCB lock, something which is not permitted. Since
   --  the server cannot be obtained reliably, it must be obtained unreliably
   --  and then checked again once it has been locked.

   procedure Unlock_Server (Entry_Call : Entry_Call_Link);
   --  STPO.Unlock the server targeted by Entry_Call. The server must
   --  be locked before calling this.

   procedure Unlock_And_Update_Server
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link);
   --  Similar to Unlock_Server, but services entry calls if the
   --  server is a protected object.

   procedure Check_Pending_Actions_For_Entry_Call
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link);
   pragma Inline (Check_Pending_Actions_For_Entry_Call);
   --  This procedure performs priority change of a queued call and
   --  dequeuing of an entry call when an the call is cancelled.
   --  If the call is dequeued the state should be set to Cancelled.

   procedure Poll_Base_Priority_Change_At_Entry_Call
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link);
   pragma Inline (Poll_Base_Priority_Change_At_Entry_Call);
   --  Has to be called with the Self_ID's ATCB write-locked.
   --  May temporariliy release the lock.

   ---------------------
   -- Check_Exception --
   ---------------------

   --  Raise any pending exception from the Entry_Call.

   --  This should be called at the end of every compiler interface
   --  procedure that implements an entry call.

   --  In principle, the caller should not be abort-deferred (unless
   --  the application program violates the Ada language rules by doing
   --  entry calls from within protected operations -- an erroneous practice
   --  apparently followed with success by some adventurous GNAT users).
   --  Absolutely, the caller should not be holding any locks, or there
   --  will be deadlock.

   procedure Check_Exception
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link)
   is
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

   -----------------------------------------
   -- Check_Pending_Actions_For_Entry_Call --
   -----------------------------------------

   --  Call only with abort deferred and holding lock of Self_ID. This
   --  is a bit of common code for all entry calls. The effect is to do
   --  any deferred base priority change operation, in case some other
   --  task called STPO.Set_Priority while the current task had abort deferred,
   --  and to dequeue the call if the call has been aborted.

   procedure Check_Pending_Actions_For_Entry_Call
     (Self_ID    : Task_ID;
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

   --  This should only be called by the Entry_Call.Self.
   --  It should be holding no other ATCB locks at the time.

   procedure Lock_Server (Entry_Call : Entry_Call_Link) is
      Test_Task         : Task_ID;
      Test_PO           : Protection_Entries_Access;
      Ceiling_Violation : Boolean;
      Failures          : Integer := 0;

   begin
      Test_Task := Entry_Call.Called_Task;

      loop
         if Test_Task = null then

            --  Entry_Call was queued on a protected object,
            --  or in transition, when we last fetched Test_Task.

            Test_PO := To_Protection (Entry_Call.Called_PO);

            if Test_PO = null then

               --  We had very bad luck, interleaving with TWO different
               --  requeue operations. Go around the loop and try again.

               STPO.Yield;

            else
               Lock_Entries (Test_PO, Ceiling_Violation);

               --  ????
               --  The following code allows Lock_Server to be called
               --  when cancelling a call, to allow for the possibility
               --  that the priority of the caller has been raised
               --  beyond that of the protected entry call by
               --  Ada.Dynamic_Priorities.STPO.Set_Priority.

               --  If the current task has a higher priority than the ceiling
               --  of the protected object, temporarily lower it. It will
               --  be reset in Unlock.

               if Ceiling_Violation then
                  declare
                     Current_Task      : Task_ID := STPO.Self;
                     Old_Base_Priority : System.Any_Priority;

                  begin
                     STPO.Write_Lock (Current_Task);
                     Old_Base_Priority := Current_Task.Common.Base_Priority;
                     Current_Task.New_Base_Priority := Test_PO.Ceiling;
                     System.Tasking.Initialization.Change_Base_Priority
                       (Current_Task);
                     STPO.Unlock (Current_Task);

                     --  Following lock should not fail

                     Lock_Entries (Test_PO);

                     Test_PO.Old_Base_Priority := Old_Base_Priority;
                     Test_PO.Pending_Action := True;
                  end;
               end if;

               exit when To_Address (Test_PO) = Entry_Call.Called_PO;
               Unlock_Entries (Test_PO);
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

   --  A specialized version of Poll_Base_Priority_Change,
   --  that does the optional entry queue reordering.

   procedure Poll_Base_Priority_Change_At_Entry_Call
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link)
   is
   begin
      if Initialization.Dynamic_Priority_Support
        and then Self_ID.Pending_Priority_Change
      then
         --  Check for ceiling violations ???

         Self_ID.Pending_Priority_Change := False;

         if Self_ID.Common.Base_Priority = Self_ID.New_Base_Priority then
            STPO.Unlock (Self_ID);
            STPO.Yield;
            STPO.Write_Lock (Self_ID);

         else
            if Self_ID.Common.Base_Priority < Self_ID.New_Base_Priority then

               --  Raising priority

               Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
               STPO.Set_Priority (Self_ID, Self_ID.Common.Base_Priority);

            else
               --  Lowering priority

               Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
               STPO.Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
               STPO.Unlock (Self_ID);
               STPO.Yield;
               STPO.Write_Lock (Self_ID);
            end if;
         end if;

         --  Requeue the entry call at the new priority.
         --  We need to requeue even if the new priority is the same than
         --  the previous (see ACVC cxd4006).

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

   --  Reset the priority of a task completing an accept statement to
   --  the value it had before the call.

   procedure Reset_Priority
     (Acceptor_Prev_Priority : Rendezvous_Priority;
      Acceptor               : Task_ID) is
   begin
      if Acceptor_Prev_Priority /= Priority_Not_Boosted then
         STPO.Set_Priority (Acceptor, Acceptor_Prev_Priority,
           Loss_Of_Inheritance => True);
      end if;
   end Reset_Priority;

   --  ???
   --  Check why we don't need any kind of lock to do this.
   --  Do we limit this kind of "active" priority change to be done
   --  by the task for itself only?

   ------------------------------
   -- Try_To_Cancel_Entry_Call --
   ------------------------------

   --  This is used to implement the Cancel_Task_Entry_Call and
   --  Cancel_Protected_Entry_Call.
   --  Try to cancel async. entry call.
   --  Effect includes Abort_To_Level and Wait_For_Completion.
   --  Cancelled = True iff the cancelation was successful, i.e.,
   --  the call was not Done before this call.
   --  On return, the call is off-queue and the ATC level is reduced by one.

   procedure Try_To_Cancel_Entry_Call (Succeeded : out Boolean) is
      Entry_Call : Entry_Call_Link;
      Self_ID    : constant Task_ID := STPO.Self;

      use type Ada.Exceptions.Exception_Id;

   begin
      Entry_Call := Self_ID.Entry_Calls (Self_ID.ATC_Nesting_Level)'Access;

      --  Experimentation has shown that abort is sometimes (but not
      --  always) already deferred when Cancel_X_Entry_Call is called.
      --  That may indicate an error. Find out what is going on. ???

      pragma Assert (Entry_Call.Mode = Asynchronous_Call);
      pragma Assert (Self_ID = Self);

      Initialization.Defer_Abort_Nestable (Self_ID);
      STPO.Write_Lock (Self_ID);
      Entry_Call.Cancellation_Attempted := True;

      if Self_ID.Pending_ATC_Level >= Entry_Call.Level then
         Self_ID.Pending_ATC_Level := Entry_Call.Level - 1;
      end if;

      Entry_Calls.Wait_For_Completion (Self_ID, Entry_Call);
      STPO.Unlock (Self_ID);
      Succeeded := Entry_Call.State = Cancelled;

      if Succeeded then
         Initialization.Undefer_Abort_Nestable (Self_ID);
      else
         --  ????

         Initialization.Undefer_Abort_Nestable (Self_ID);

         --  Ideally, abort should no longer be deferred at this
         --  point, so we should be able to call Check_Exception.
         --  The loop below should be considered temporary,
         --  to work around the possiblility that abort may be deferred
         --  more than one level deep.

         if Entry_Call.Exception_To_Raise /= Ada.Exceptions.Null_Id then
            while Self_ID.Deferral_Level > 0 loop
               System.Tasking.Initialization.Undefer_Abort_Nestable (Self_ID);
            end loop;

            Entry_Calls.Check_Exception (Self_ID, Entry_Call);
         end if;
      end if;
   end Try_To_Cancel_Entry_Call;

   ------------------------------
   -- Unlock_And_Update_Server --
   ------------------------------

   procedure Unlock_And_Update_Server
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link)
   is
      Called_PO : Protection_Entries_Access;
      Caller    : Task_ID;

   begin
      if Entry_Call.Called_Task /= null then
         STPO.Unlock (Entry_Call.Called_Task);
      else
         Called_PO := To_Protection (Entry_Call.Called_PO);
         PO_Service_Entries (Self_ID, Called_PO);

         if Called_PO.Pending_Action then
            Called_PO.Pending_Action := False;
            Caller := STPO.Self;
            STPO.Write_Lock (Caller);
            Caller.New_Base_Priority := Called_PO.Old_Base_Priority;
            Initialization.Change_Base_Priority (Caller);
            STPO.Unlock (Caller);
         end if;

         Unlock_Entries (Called_PO);
      end if;
   end Unlock_And_Update_Server;

   -------------------
   -- Unlock_Server --
   -------------------

   procedure Unlock_Server (Entry_Call : Entry_Call_Link) is
      Caller    : Task_ID;
      Called_PO : Protection_Entries_Access;

   begin
      if Entry_Call.Called_Task /= null then
         STPO.Unlock (Entry_Call.Called_Task);
      else
         Called_PO := To_Protection (Entry_Call.Called_PO);

         if Called_PO.Pending_Action then
            Called_PO.Pending_Action := False;
            Caller := STPO.Self;
            STPO.Write_Lock (Caller);
            Caller.New_Base_Priority := Called_PO.Old_Base_Priority;
            Initialization.Change_Base_Priority (Caller);
            STPO.Unlock (Caller);
         end if;

         Unlock_Entries (Called_PO);
      end if;
   end Unlock_Server;

   -------------------------
   -- Wait_For_Completion--
   -------------------------

   --  Call this only when holding Self_ID locked

   --  If this is a conditional call, it should be cancelled when it
   --  becomes abortable. This is checked in the loop below.

   --  We do the same thing for Asynchronous_Call. Executing the following
   --  loop will clear the Pending_Action field if there is no
   --  Pending_Action. We want the call made from Cancel_Task_Entry_Call
   --  to check the abortion level so that we make sure that the Cancelled
   --  field reflect the status of an Asynchronous_Call properly.
   --  This problem came up when the triggered statement and the abortable
   --  part depend on entries of the same task. When a cancellation is
   --  delivered, Undefer_Abort in the call made from abortable part
   --  sets the Pending_Action bit to false. However, the call is actually
   --  made to cancel the Asynchronous Call so that we need to check its
   --  status here again. Otherwise we may end up waiting for a cancelled
   --  call forever.

   --  ????? .........
   --  Recheck the logic of the above old comment.  It may be stale.

   procedure Wait_For_Completion
     (Self_ID    : Task_ID;
      Entry_Call : Entry_Call_Link)
   is
   begin
      pragma Assert (Self_ID = Entry_Call.Self);
      Self_ID.Common.State := Entry_Caller_Sleep;

      loop
         Check_Pending_Actions_For_Entry_Call (Self_ID, Entry_Call);
         exit when Entry_Call.State >= Done;
         STPO.Sleep (Self_ID, Entry_Caller_Sleep);
      end loop;

      Self_ID.Common.State := Runnable;
      Utilities.Exit_One_ATC_Level (Self_ID);
   end Wait_For_Completion;

   --------------------------------------
   -- Wait_For_Completion_With_Timeout --
   --------------------------------------

   --  This routine will lock Self_ID.

   --  This procedure waits for the entry call to
   --  be served, with a timeout.  It tries to cancel the
   --  call if the timeout expires before the call is served.

   --  If we wake up from the timed sleep operation here,
   --  it may be for several possible reasons:

   --  1) The entry call is done being served.
   --  2) There is an abort or priority change to be served.
   --  3) The timeout has expired (Timedout = True)
   --  4) There has been a spurious wakeup.

   --  Once the timeout has expired we may need to continue to wait if
   --  the call is already being serviced. In that case, we want to go
   --  back to sleep, but without any timeout. The variable Timedout is
   --  used to control this. If the Timedout flag is set, we do not need
   --  to STPO.Sleep with a timeout. We just sleep until we get a wakeup for
   --  some status change.

   --  The original call may have become abortable after waking up.
   --  We want to check Check_Pending_Actions_For_Entry_Call again
   --  in any case.

   procedure Wait_For_Completion_With_Timeout
     (Self_ID     : Task_ID;
      Entry_Call  : Entry_Call_Link;
      Wakeup_Time : Duration;
      Mode        : Delay_Modes)
   is
      Timedout : Boolean := False;
      Yielded  : Boolean := False;

      use type Ada.Exceptions.Exception_Id;

   begin
      Initialization.Defer_Abort_Nestable (Self_ID);
      STPO.Write_Lock (Self_ID);

      pragma Assert (Entry_Call.Self = Self_ID);
      pragma Assert (Entry_Call.Mode = Timed_Call);
      Self_ID.Common.State := Entry_Caller_Sleep;

      --  Looping is necessary in case the task wakes up early from the
      --  timed sleep, due to a "spurious wakeup". Spurious wakeups are
      --  a weakness of POSIX condition variables. A thread waiting for
      --  a condition variable is allowed to wake up at any time, not just
      --  when the condition is signaled. See the same loop in the
      --  ordinary Wait_For_Completion, above.

      loop
         Check_Pending_Actions_For_Entry_Call (Self_ID, Entry_Call);
         exit when Entry_Call.State >= Done;

         STPO.Timed_Sleep (Self_ID, Wakeup_Time, Mode,
           Entry_Caller_Sleep, Timedout, Yielded);

         if Timedout then

            --  Try to cancel the call (see Try_To_Cancel_Entry_Call for
            --  corresponding code in the ATC case).

            Entry_Call.Cancellation_Attempted := True;

            if Self_ID.Pending_ATC_Level >= Entry_Call.Level then
               Self_ID.Pending_ATC_Level := Entry_Call.Level - 1;
            end if;

            --  The following loop is the same as the loop and exit code
            --  from the ordinary Wait_For_Completion. If we get here, we
            --  have timed out but we need to keep waiting until the call
            --  has actually completed or been cancelled successfully.

            loop
               Check_Pending_Actions_For_Entry_Call (Self_ID, Entry_Call);
               exit when Entry_Call.State >= Done;
               STPO.Sleep (Self_ID, Entry_Caller_Sleep);
            end loop;

            Self_ID.Common.State := Runnable;
            Utilities.Exit_One_ATC_Level (Self_ID);

            STPO.Unlock (Self_ID);

            if Entry_Call.State = Cancelled then
               Initialization.Undefer_Abort_Nestable (Self_ID);
            else
               --  ????

               Initialization.Undefer_Abort_Nestable (Self_ID);

               --  Ideally, abort should no longer be deferred at this
               --  point, so we should be able to call Check_Exception.
               --  The loop below should be considered temporary,
               --  to work around the possiblility that abort may be
               --  deferred more than one level deep.

               if Entry_Call.Exception_To_Raise /=
                 Ada.Exceptions.Null_Id then

                  while Self_ID.Deferral_Level > 0 loop
                     Initialization.Undefer_Abort_Nestable (Self_ID);
                  end loop;

                  Entry_Calls.Check_Exception (Self_ID, Entry_Call);
               end if;
            end if;

            return;
         end if;
      end loop;

      --  This last part is the same as ordinary Wait_For_Completion,
      --  and is only executed if the call completed without timing out.

      Self_ID.Common.State := Runnable;
      Utilities.Exit_One_ATC_Level (Self_ID);
      STPO.Unlock (Self_ID);

      Initialization.Undefer_Abort_Nestable (Self_ID);

      if not Yielded then
         STPO.Yield;
      end if;
   end Wait_For_Completion_With_Timeout;

   --------------------------
   -- Wait_Until_Abortable --
   --------------------------

   --  Wait to start the abortable part of an async. select statement
   --  until the trigger entry call becomes abortable.

   procedure Wait_Until_Abortable
     (Self_ID   : Task_ID;
      Call      : Entry_Call_Link)
   is
   begin
      pragma Assert (Self_ID.ATC_Nesting_Level > 0);
      pragma Assert (Call.Mode = Asynchronous_Call);

      STPO.Write_Lock (Self_ID);
      Self_ID.Common.State := Entry_Caller_Sleep;

      loop
         Check_Pending_Actions_For_Entry_Call (Self_ID, Call);
         exit when Call.State >= Was_Abortable;
         STPO.Sleep (Self_ID, Async_Select_Sleep);
      end loop;

      Self_ID.Common.State := Runnable;
      STPO.Unlock (Self_ID);
   end Wait_Until_Abortable;

   --  It might seem that we should be holding the server's lock when
   --  we test Call.State above.

   --  In an earlier version, the code above temporarily unlocked the
   --  caller and locked the server just for checking Call.State.
   --  The unlocking of the caller risked missing a wakeup
   --  (an error) and locking the server had no apparent value.
   --  We should not need the server's lock, since once Call.State
   --  is set to Was_Abortable or beyond, it never goes back below
   --  Was_Abortable until this task starts another entry call.

   --  ????
   --  It seems that other calls to Lock_Server may also risk missing
   --  wakeups.  We need to check that they do not have this problem.

end System.Tasking.Entry_Calls;
