------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . R E A L _ T I M E . T I M I N G _ E V E N T S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2005-2006, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Task_Primitives.Operations;
with System.Tasking.Utilities;
with System.Soft_Links;
--  used for Abort_Defer/Undefer

with Ada.Containers.Doubly_Linked_Lists;
pragma Elaborate_All (Ada.Containers.Doubly_Linked_Lists);

---------------------------------
-- Ada.Real_Time.Timing_Events --
---------------------------------

package body Ada.Real_Time.Timing_Events is

   use System.Task_Primitives.Operations;
   --  for Write_Lock and Unlock

   package SSL renames System.Soft_Links;

   type Any_Timing_Event is access all Timing_Event'Class;
   --  We must also handle user-defined types derived from Timing_Event

   ------------
   -- Events --
   ------------

   package Events is new Ada.Containers.Doubly_Linked_Lists (Any_Timing_Event);
   --  Provides the type for the container holding pointers to events

   All_Events : Events.List;
   --  The queue of pending events, ordered by increasing timeout value, that
   --  have been "set" by the user via Set_Handler.

   Event_Queue_Lock : aliased System.Task_Primitives.RTS_Lock;
   --  Used for mutually exclusive access to All_Events

   procedure Process_Queued_Events;
   --  Examine the queue of pending events for any that have timed-out. For
   --  those that have timed-out, remove them from the queue and invoke their
   --  handler (unless the user has cancelled the event by setting the handler
   --  pointer to null). Mutually exclusive access is held via Event_Queue_Lock
   --  during part of the processing.

   procedure Insert_Into_Queue (This : Any_Timing_Event);
   --  Insert the specified event pointer into the queue of pending events
   --  with mutually exclusive access via Event_Queue_Lock.

   procedure Remove_From_Queue (This : Any_Timing_Event);
   --  Remove the specified event pointer from the queue of pending events
   --  with mutually exclusive access via Event_Queue_Lock.
   --  This procedure is used by the client-side routines (Set_Handler, etc.).

   -----------
   -- Timer --
   -----------

   task Timer is
      pragma Priority (System.Priority'Last);
      entry Start;
   end Timer;

   task body Timer is
      Period : constant Time_Span := Milliseconds (100);
      --  This is a "chiming" clock timer that fires periodically. The period
      --  selected is arbitrary and could be changed to suit the application
      --  requirements. Obviously a shorter period would give better resolution
      --  at the cost of more overhead.
   begin
      System.Tasking.Utilities.Make_Independent;

      --  We await the call to Start to ensure that Event_Queue_Lock has been
      --  initialized by the package executable part prior to accessing it in
      --  the loop. The task is activated before the first statement of the
      --  executable part so it would otherwise be possible for the task to
      --  call EnterCriticalSection in Process_Queued_Events before the
      --  initialization.

      --  We don't simply put the initialization here, prior to the loop,
      --  because other application tasks could call the visible routines that
      --  also call Enter/LeaveCriticalSection prior to this task doing the
      --  initialization.

      accept Start;

      loop
         Process_Queued_Events;
         delay until Clock + Period;
      end loop;
   end Timer;

   ---------------------------
   -- Process_Queued_Events --
   ---------------------------

   procedure Process_Queued_Events is
      Next_Event : Any_Timing_Event;

   begin
      loop
         SSL.Abort_Defer.all;

         Write_Lock (Event_Queue_Lock'Access);

         if All_Events.Is_Empty then
            Unlock (Event_Queue_Lock'Access);
            SSL.Abort_Undefer.all;
            return;
         else
            Next_Event := All_Events.First_Element;
         end if;

         if Next_Event.Timeout > Clock then

            --  We found one that has not yet timed-out. The queue is in
            --  ascending order by Timeout so there is no need to continue
            --  processing (and indeed we must not continue since we always
            --  delete the first element).

            Unlock (Event_Queue_Lock'Access);
            SSL.Abort_Undefer.all;
            return;
         end if;

         --  We have an event that has timed out so we will process it. It
         --  must be the first in the queue so no search is needed.

         All_Events.Delete_First;

         --  A fundamental issue is that the invocation of the event's handler
         --  might call Set_Handler on itself to re-insert itself back into the
         --  queue of future events. Thus we cannot hold the lock on the queue
         --  while invoking the event's handler.

         Unlock (Event_Queue_Lock'Access);

         SSL.Abort_Undefer.all;

         --  There is no race condition with the user changing the handler
         --  pointer while we are processing because we are executing at the
         --  highest possible application task priority and are not doing
         --  anything to block prior to invoking their handler.

         declare
            Handler : constant Timing_Event_Handler := Next_Event.Handler;
         begin
            --  The first act is to clear the event, per D.15 (13/2). Besides,
            --  we cannot clear the handler pointer *after* invoking the
            --  handler because the handler may have re-inserted the event via
            --  Set_Event. Thus we take a copy and then clear the component.

            Next_Event.Handler := null;

            if Handler /= null then
               Handler (Timing_Event (Next_Event.all));
            end if;
         exception
            when others =>
               null;
         end;
      end loop;
   end Process_Queued_Events;

   -----------------------
   -- Insert_Into_Queue --
   -----------------------

   procedure Insert_Into_Queue (This : Any_Timing_Event) is

      function Sooner (Left, Right : Any_Timing_Event) return Boolean;
      --  Compares events in terms of timeout values

      package By_Timeout is new Events.Generic_Sorting (Sooner);
      --  Used to keep the events in ascending order by timeout value

      function Sooner (Left, Right : Any_Timing_Event) return Boolean is
      begin
         return Left.Timeout < Right.Timeout;
      end Sooner;

   begin
      SSL.Abort_Defer.all;

      Write_Lock (Event_Queue_Lock'Access);

      All_Events.Append (This);

      --  A critical property of the implementation of this package is that
      --  all occurrences are in ascending order by Timeout. Thus the first
      --  event in the queue always has the "next" value for the Timer task
      --  to use in its delay statement.

      By_Timeout.Sort (All_Events);

      Unlock (Event_Queue_Lock'Access);

      SSL.Abort_Undefer.all;
   end Insert_Into_Queue;

   -----------------------
   -- Remove_From_Queue --
   -----------------------

   procedure Remove_From_Queue (This : Any_Timing_Event) is
      use Events;
      Location : Cursor;
   begin
      SSL.Abort_Defer.all;

      Write_Lock (Event_Queue_Lock'Access);

      Location := All_Events.Find (This);
      if Location /= No_Element then
         All_Events.Delete (Location);
      end if;

      Unlock (Event_Queue_Lock'Access);

      SSL.Abort_Undefer.all;
   end Remove_From_Queue;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Event   : in out Timing_Event;
      At_Time : Time;
      Handler : Timing_Event_Handler)
   is
   begin
      Remove_From_Queue (Event'Unchecked_Access);
      Event.Handler := null;
      if At_Time <= Clock then
         if Handler /= null then
            Handler (Event);
         end if;
         return;
      end if;
      if Handler /= null then
         Event.Timeout := At_Time;
         Event.Handler := Handler;
         Insert_Into_Queue (Event'Unchecked_Access);
      end if;
   end Set_Handler;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Event   : in out Timing_Event;
      In_Time : Time_Span;
      Handler : Timing_Event_Handler)
   is
   begin
      Remove_From_Queue (Event'Unchecked_Access);
      Event.Handler := null;
      if In_Time <= Time_Span_Zero then
         if Handler /= null then
            Handler (Event);
         end if;
         return;
      end if;
      if Handler /= null then
         Event.Timeout := Clock + In_Time;
         Event.Handler := Handler;
         Insert_Into_Queue (Event'Unchecked_Access);
      end if;
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Event : Timing_Event) return Timing_Event_Handler
   is
   begin
      return Event.Handler;
   end Current_Handler;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (Event     : in out Timing_Event;
      Cancelled : out Boolean)
   is
   begin
      Remove_From_Queue (Event'Unchecked_Access);
      Cancelled := Event.Handler /= null;
      Event.Handler := null;
   end Cancel_Handler;

   -------------------
   -- Time_Of_Event --
   -------------------

   function Time_Of_Event (Event : Timing_Event) return Time is
   begin
      return Event.Timeout;
   end Time_Of_Event;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Timing_Event) is
   begin
      --  D.15 (19/2) says finalization clears the event

      This.Handler := null;
      Remove_From_Queue (This'Unchecked_Access);
   end Finalize;

begin
   Initialize_Lock (Event_Queue_Lock'Access, Level => PO_Level);
   Timer.Start;
end Ada.Real_Time.Timing_Events;
