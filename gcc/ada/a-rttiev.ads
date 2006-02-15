------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . R E A L _ T I M E . T I M I N G _ E V E N T S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2005-2006, Free Software Foundation, Inc.        --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Ada.Finalization;

package Ada.Real_Time.Timing_Events is

   type Timing_Event is tagged limited private;

   type Timing_Event_Handler
     is access protected procedure (Event : in out Timing_Event);

   procedure Set_Handler
     (Event   : in out Timing_Event;
      At_Time : Time;
      Handler : Timing_Event_Handler);

   procedure Set_Handler
     (Event   : in out Timing_Event;
      In_Time : Time_Span;
      Handler : Timing_Event_Handler);

   function Current_Handler
     (Event : Timing_Event) return Timing_Event_Handler;

   procedure Cancel_Handler
     (Event     : in out Timing_Event;
      Cancelled : out Boolean);

   function Time_Of_Event (Event : Timing_Event) return Time;

private

   protected type Event_State is

      --  D.15 (22/2) requires atomicity with respect to the operations
      --  provided by the package and the timing events they manipulate. On
      --  real-time operating systems suitable for implementing this package, a
      --  different implementation strategy would be employed to meet that
      --  requirement.

      entry Set (Timeout : Time;  Handler : Timing_Event_Handler);
      --  Changes the timeout and handler values for procedure Set_Handler. Can
      --  only execute when the event is 'available', to prevent a race
      --  condition between the caller of Set_Handler and the internal Timer
      --  task that processes the events. In particular, D.15 (22/2) requires
      --  that there be no possibility of a new handler executing in response
      --  to an old timeout.

      procedure Reset;
      --  First resets the timeout to Time_First and the handler to
      --  null. Indicates that Set (for Set_Handler) can now change the timeout
      --  and/or handler.  Called only by the interal Timer task.

      procedure Cancel;
      --  Resets the timeout to Time_First and the handler to
      --  null. Called by procedure Cancel_Handler and by procedure Reset.

      function Current_Timeout return Time;
      --  Returns the currently set timeout. The value Time_First is returned
      --  if the Timing_Event is in the "cleared" state. Called by function
      --  Time_of_Event.

      function Current_Handler return Timing_Event_Handler;
      --  Returns the currently set handler. The value null is returned if the
      --  Timing_Event is in the "cleared" state. Called by function
      --  Curent_Handler.

   private
      Timeout : Time := Time_First;
      --  The time at which the user's handler should be invoked when the
      --  event is "set" (i.e., when Handler is not null).

      Handler : Timing_Event_Handler;
      --  An access value designating the protected procedure to be invoked
      --  at the Timeout time in the future.  When this value is null the event
      --  is said to be "cleared" and no timeout is processed.

      Available : Boolean := True;
      --  A flag controlling when users can change the Timeout and Handler
      --  tuple. In particular the entry Set, called by procedure Set_Handler,
      --  is controlled by this flag.

   end Event_State;

   type Timing_Event is new Ada.Finalization.Limited_Controlled with record
      Control : Event_State;
   end record;

   overriding procedure Finalize (This : in out Timing_Event);
   --  Finalization procedure is required to satisfy (RM D.15 (19/2)), which
   --  says that the object must be cleared on finalization.

end Ada.Real_Time.Timing_Events;
