------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--             S Y S T E M . T A S K I N G . E N T R Y _ C A L L S          --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--            Copyright (C) 1991-1998, Florida State University             --
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

package System.Tasking.Entry_Calls is

   procedure Wait_For_Completion
     (Self_ID : Task_ID;
      Entry_Call : Entry_Call_Link);
   --  This procedure suspends the calling task until the specified entry
   --  call has either been completed or cancelled. It performs other
   --  operations required of suspended tasks, such as performing
   --  dynamic priority changes. On exit, the call will not be queued.
   --  This waits for calls on task or protected entries.
   --  Abortion must be deferred when calling this procedure.
   --  Call this only when holding Self_ID locked.

   procedure Wait_For_Completion_With_Timeout
     (Self_ID     : Task_ID;
      Entry_Call  : Entry_Call_Link;
      Wakeup_Time : Duration;
      Mode        : Delay_Modes);
   --  Same as Wait_For_Completion but it wait for a timeout with the value
   --  specified in Wakeup_Time as well.
   --  Self_ID will be locked by this procedure.

   procedure Wait_Until_Abortable
     (Self_ID : Task_ID;
      Call : Entry_Call_Link);
   --  This procedure suspends the calling task until the specified entry
   --  call is queued abortably or completes.
   --  Abortion must be deferred when calling this procedure.

   procedure Try_To_Cancel_Entry_Call (Succeeded : out Boolean);
   pragma Inline (Try_To_Cancel_Entry_Call);
   --  Try to cancel async. entry call.
   --  Effect includes Abort_To_Level and Wait_For_Completion.
   --  Cancelled = True iff the cancelation was successful, i.e.,
   --  the call was not Done before this call.
   --  On return, the call is off-queue and the ATC level is reduced by one.

   procedure Reset_Priority
     (Acceptor_Prev_Priority : Rendezvous_Priority;
      Acceptor               : Task_ID);
   pragma Inline (Reset_Priority);
   --  Reset the priority of a task completing an accept statement to
   --  the value it had before the call.

   procedure Check_Exception
     (Self_ID : Task_ID;
      Entry_Call : Entry_Call_Link);
   pragma Inline (Check_Exception);
   --  Raise any pending exception from the Entry_Call.
   --  This should be called at the end of every compiler interface
   --  procedure that implements an entry call.
   --  In principle, the caller should not be abort-deferred (unless
   --  the application program violates the Ada language rules by doing
   --  entry calls from within protected operations -- an erroneous practice
   --  apparently followed with success by some adventurous GNAT users).
   --  Absolutely, the caller should not be holding any locks, or there
   --  will be deadlock.

end System.Tasking.Entry_Calls;
