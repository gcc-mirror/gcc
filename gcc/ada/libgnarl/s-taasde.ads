------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--           S Y S T E M . T A S K I N G . A S Y N C _ D E L A Y S          --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1998-2024, Free Software Foundation, Inc.         --
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

--  This package contains the procedures to implements timeouts (delays) for
--  asynchronous select statements.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

package System.Tasking.Async_Delays is

   --  Suppose the following source code is given:

   --  select delay When;
   --     ...continuation for timeout case...
   --  then abort
   --     ...abortable part...
   --  end select;

   --  The compiler should expand this to the following:

   --  declare
   --     DB : aliased Delay_Block;
   --  begin
   --     if System.Tasking.Async_Delays.Enqueue_Duration
   --       (When, DB'Unchecked_Access)
   --     then
   --        begin
   --           A101b : declare
   --              procedure _clean is
   --              begin
   --                 System.Tasking.Async_Delays.Cancel_Async_Delay
   --                   (DB'Unchecked_Access);
   --                 return;
   --              end _clean;
   --           begin
   --              abort_undefer.all;
   --              ...abortable part...
   --           exception
   --              when all others =>
   --                 declare
   --                    E105b : exception_occurrence;
   --                 begin
   --                    save_occurrence (E105b, get_current_excep.all.all);
   --                    _clean;
   --                    reraise_occurrence_no_defer (E105b);
   --                 end;
   --           at end
   --              _clean;
   --           end A101b;
   --        exception
   --           when _abort_signal =>
   --              abort_undefer.all;
   --        end;
   --     end if;

   --     if Timed_Out (DB'Unchecked_Access) then
   --        ...continuation for timeout case...
   --     end if;
   --  end;

   -----------------
   -- Delay_Block --
   -----------------

   type Delay_Block is limited private;
   type Delay_Block_Access is access all Delay_Block;

   function Enqueue_Duration
     (T : Duration;
      D : Delay_Block_Access) return Boolean;
   --  Enqueue the specified relative delay. Returns True if the delay has
   --  been enqueued, False if it has already expired. If the delay has been
   --  enqueued, abort is deferred.

   procedure Cancel_Async_Delay (D : Delay_Block_Access);
   --  Cancel the specified asynchronous delay

   function Timed_Out (D : Delay_Block_Access) return Boolean;
   pragma Inline (Timed_Out);
   --  Return True if the delay specified in D has timed out

   --  There are child units for delays on Ada.Calendar.Time/Ada.Real_Time.Time
   --  so that an application need not link in features that it is not using.

private

   type Delay_Block is limited record
      Self_Id : Task_Id;
      --  ID of the calling task

      Level : ATC_Level_Base;
      --  Normally Level is the ATC nesting level of the asynchronous select
      --  statement to which this delay belongs, but after a call has been
      --  dequeued we set it to Level_No_Pending_Abort so that the Cancel
      --  operation can detect repeated calls, and act idempotently.

      Resume_Time : Duration;
      --  The absolute wake up time, represented as Duration

      Timed_Out : Boolean := False;
      --  Set to true if the delay has timed out

      Succ, Pred : Delay_Block_Access;
      --  A double linked list
   end record;

   --  The above "overlaying" of Self_Id and Level to hold other data that has
   --  a non-overlapping lifetime is an unabashed hack to save memory.

   procedure Time_Enqueue
     (T : Duration;
      D : Delay_Block_Access);
   pragma Inline (Time_Enqueue);
   --  Used by the child units to enqueue delays on the timer queue implemented
   --  in the body of this package. T denotes a point in time as the duration
   --  elapsed since the epoch of the Ada real-time clock.

end System.Tasking.Async_Delays;
