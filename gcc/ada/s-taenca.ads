------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--             S Y S T E M . T A S K I N G . E N T R Y _ C A L L S          --
--                                                                          --
--                                  S p e c                                 --
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

--  This package provides internal RTS calls implementing operations
--  that apply to general entry calls, that is, calls to either
--  protected or task entries.

--  These declarations are not part of the GNARL Interface

package System.Tasking.Entry_Calls is

   procedure Wait_For_Completion (Entry_Call : Entry_Call_Link);
   --  This procedure suspends the calling task until the specified entry
   --  call has either been completed or cancelled. It performs other
   --  operations required of suspended tasks, such as performing
   --  dynamic priority changes. On exit, the call will not be queued.
   --  This waits for calls on task or protected entries.
   --  Abortion must be deferred when calling this procedure.
   --  Call this only when holding Self (= Entry_Call.Self) or global RTS lock.

   procedure Wait_For_Completion_With_Timeout
     (Entry_Call  : Entry_Call_Link;
      Wakeup_Time : Duration;
      Mode        : Delay_Modes;
      Yielded     : out Boolean);
   --  Same as Wait_For_Completion but wait for a timeout with the value
   --  specified in Wakeup_Time as well.
   --  On return, Yielded indicates whether the wait has performed a yield.
   --  Check_Exception must be called after calling this procedure.

   procedure Wait_Until_Abortable
     (Self_ID : Task_Id;
      Call    : Entry_Call_Link);
   --  This procedure suspends the calling task until the specified entry
   --  call is queued abortably or completes.
   --  Abortion must be deferred when calling this procedure, and the global
   --  RTS lock taken when Single_Lock.

   procedure Try_To_Cancel_Entry_Call (Succeeded : out Boolean);
   pragma Inline (Try_To_Cancel_Entry_Call);
   --  Try to cancel async. entry call.
   --  Effect includes Abort_To_Level and Wait_For_Completion.
   --  Cancelled = True iff the cancellation was successful, i.e.,
   --  the call was not Done before this call.
   --  On return, the call is off-queue and the ATC level is reduced by one.

   procedure Reset_Priority
     (Acceptor               : Task_Id;
      Acceptor_Prev_Priority : Rendezvous_Priority);
   pragma Inline (Reset_Priority);
   --  Reset the priority of a task completing an accept statement to
   --  the value it had before the call.
   --  Acceptor should always be equal to Self.

   procedure Check_Exception
     (Self_ID    : Task_Id;
      Entry_Call : Entry_Call_Link);
   pragma Inline (Check_Exception);
   --  Raise any pending exception from the Entry_Call.
   --  This should be called at the end of every compiler interface procedure
   --  that implements an entry call.
   --  In principle, the caller should not be abort-deferred (unless the
   --  application program violates the Ada language rules by doing entry calls
   --  from within protected operations -- an erroneous practice apparently
   --  followed with success by some adventurous GNAT users).
   --  Absolutely, the caller should not be holding any locks, or there
   --  will be deadlock.

end System.Tasking.Entry_Calls;
