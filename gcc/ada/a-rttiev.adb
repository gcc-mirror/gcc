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

with System.Tasking.Utilities;
--  for Make_Independent

with Ada.Containers.Doubly_Linked_Lists;
pragma Elaborate_All (Ada.Containers.Doubly_Linked_Lists);

package body Ada.Real_Time.Timing_Events is

   type Any_Timing_Event is access all Timing_Event'Class;
   --  We must also handle user-defined types derived from Timing_Event

   ------------
   -- Events --
   ------------

   package Events is
      new Ada.Containers.Doubly_Linked_Lists (Any_Timing_Event);

   -----------------
   -- Event_Queue --
   -----------------

   protected Event_Queue is
      pragma Priority (System.Priority'Last);

      procedure Insert (This : Any_Timing_Event);
      --  Inserts This into the queue in ascending order by Timeout

      procedure Process_Events;
      --  Iterates over the list of events and calls the handlers for any of
      --  those that have timed out. Deletes those that have timed out.

    private
      All_Events : Events.List;
   end Event_Queue;

   -----------
   -- Timer --
   -----------

   task Timer is
      pragma Priority (System.Priority'Last);
   end Timer;

   task body Timer is
      Period : constant Time_Span := Milliseconds (100);
      --  This is a "chiming" clock timer that fires periodically. The period
      --  selected is arbitrary and could be changed to suit the application
      --  requirements. Obviously a shorter period would give better resolution
      --  at the cost of more overhead.

   begin
      System.Tasking.Utilities.Make_Independent;
      loop
         Event_Queue.Process_Events;
         delay until Clock + Period;
      end loop;
   end Timer;

   ------------
   -- Sooner --
   ------------

   function Sooner (Left, Right : Any_Timing_Event) return Boolean;
   --  Used by the Event_Queue insertion routine to keep the events in
   --  ascending order by timeout value.

   -----------------
   -- Event_Queue --
   -----------------

   protected body Event_Queue is

      procedure Insert (This : Any_Timing_Event) is
         package By_Timeout is new Events.Generic_Sorting (Sooner);
         --  Used to keep the events in ascending order by timeout value

      begin
         All_Events.Append (This);

         --  A critical property of the implementation of this package is that
         --  all occurrences are in ascending order by Timeout. Thus the first
         --  event in the queue always has the "next" value for the Timer task
         --  to use in its delay statement.

         By_Timeout.Sort (All_Events);
      end Insert;

      procedure Process_Events is
         Next_Event : Any_Timing_Event;
      begin
         while not All_Events.Is_Empty loop
            Next_Event := All_Events.First_Element;

            --  Clients can cancel a timeout (setting the handler to null) but
            --  cannot otherwise change the timeout/handler tuple until the
            --  call to Reset below.

            if Next_Event.Control.Current_Timeout > Clock then

               --  We found one that has not yet timed-out. The queue is in
               --  ascending order by Timeout so there is no need to continue
               --  processing (and indeed we must not continue since we always
               --  delete the first element).

               return;
            end if;

            declare
               Response : Timing_Event_Handler;

            begin
               --  We take a local snapshot of the handler to avoid a race
               --  condition because we evaluate the handler value in the
               --  if-statement and again in the call and the client might have
               --  set it to null between those two evaluations.

               Response := Next_Event.Control.Current_Handler;

               if Response /= null then

                  --  D.15 (13/2) says we only invoke the handler if it is
                  --  set when the timeout expires.

                  Response (Timing_Event (Next_Event.all));
               end if;

            exception
               when others =>
                  null;  --  per D.15 (21/2)
            end;

            Next_Event.Control.Reset;

            --  Clients can now change the timeout/handler pair for this event

            --  And now we can delete the event from the queue. Any item we
            --  delete would be the first in the queue because we exit the loop
            --  when we first find one that is not yet timed-out. This fact
            --  allows us to use these "First oriented" list processing
            --  routines instead of the cursor oriented versions because we can
            --  avoid handling the way deletion affects cursors.

            All_Events.Delete_First;
         end loop;
      end Process_Events;

   end Event_Queue;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Event   : in out Timing_Event;
      At_Time : Time;
      Handler : Timing_Event_Handler)
   is
   begin
      Event.Control.Cancel;

      if At_Time <= Clock then
         if Handler /= null then
            Handler (Event);
         end if;
         return;
      end if;

      if Handler /= null then
         Event.Control.Set (At_Time, Handler);
         Event_Queue.Insert (Event'Unchecked_Access);
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
      Event.Control.Cancel;

      if In_Time <= Time_Span_Zero then
         if Handler /= null then
            Handler (Event);
         end if;
         return;
      end if;

      if Handler /= null then
         Event.Control.Set (Clock + In_Time, Handler);
         Event_Queue.Insert (Event'Unchecked_Access);
      end if;
   end Set_Handler;

   -----------------
   -- Event_State --
   -----------------

   protected body Event_State is

      entry Set
        (Timeout : Time;
         Handler : Timing_Event_Handler)
      when
         Available
      is
      begin
         Event_State.Timeout := Set.Timeout;
         Event_State.Handler := Set.Handler;
         Available := False;
      end Set;

      procedure Reset is
      begin
         Cancel;
         Available := True;
      end Reset;

      procedure Cancel is
      begin
         Handler := null;
         Timeout := Time_First;
      end Cancel;

      function Current_Timeout return Time is
      begin
         return Timeout;
      end Current_Timeout;

      function Current_Handler return Timing_Event_Handler is
      begin
         return Handler;
      end Current_Handler;

   end Event_State;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Event : Timing_Event) return Timing_Event_Handler
   is
   begin
      return Event.Control.Current_Handler;
   end Current_Handler;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (Event     : in out Timing_Event;
      Cancelled : out Boolean)
   is
   begin
      Cancelled := Event.Control.Current_Handler /= null;
      Event.Control.Cancel;
   end Cancel_Handler;

   -------------------
   -- Time_Of_Event --
   -------------------

   function Time_Of_Event (Event : Timing_Event) return Time is
   begin
      return Event.Control.Current_Timeout;
   end Time_Of_Event;

   ------------
   -- Sooner --
   ------------

   function Sooner (Left, Right : Any_Timing_Event) return Boolean is
   begin
      return Left.Control.Current_Timeout < Right.Control.Current_Timeout;
   end Sooner;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Timing_Event) is
   begin
      --  D.15 (19/2) says finalization clears the event

      This.Control.Cancel;
   end Finalize;

end Ada.Real_Time.Timing_Events;
