------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K I N G . Q U E U I N G              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--         Copyright (C) 1992-2001, Free Software Foundation, Inc.          --
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
-- now maintained by Ada Core Technologies, Inc. (http://www.gnat.com).     --
--                                                                          --
------------------------------------------------------------------------------

with System.Tasking.Protected_Objects.Entries;

package System.Tasking.Queuing is

   package POE renames System.Tasking.Protected_Objects.Entries;

   procedure Broadcast_Program_Error
     (Self_ID      : Task_ID;
      Object       : POE.Protection_Entries_Access;
      Pending_Call : Entry_Call_Link;
      RTS_Locked   : Boolean := False);
   --  Raise Program_Error in all tasks calling the protected entries of Object
   --  The exception will not be raised immediately for the calling task; it
   --  will be deferred until it calls Check_Exception.
   --  RTS_Locked indicates whether the global RTS lock is taken (only
   --  relevant if Single_Lock is True).

   procedure Enqueue (E : in out Entry_Queue; Call : Entry_Call_Link);
   --  Enqueue Call at the end of entry_queue E

   procedure Dequeue (E : in out Entry_Queue; Call : Entry_Call_Link);
   --  Dequeue Call from entry_queue E

   function Head (E : in Entry_Queue) return Entry_Call_Link;
   --  Return the head of entry_queue E
   pragma Inline (Head);

   procedure Dequeue_Head
     (E    : in out Entry_Queue;
      Call : out Entry_Call_Link);
   --  Remove and return the head of entry_queue E

   function Onqueue (Call : Entry_Call_Link) return Boolean;
   --  Return True if Call is on any entry_queue at all
   pragma Inline (Onqueue);

   function Count_Waiting (E : in Entry_Queue) return Natural;
   --  Return number of calls on the waiting queue of E

   procedure Select_Task_Entry_Call
     (Acceptor         : Task_ID;
      Open_Accepts     : Accept_List_Access;
      Call             : out Entry_Call_Link;
      Selection        : out Select_Index;
      Open_Alternative : out Boolean);
   --  Select an entry for rendezvous.  On exit:
   --    Call will contain a pointer to the entry call record selected;
   --    Selection will contain the index of the alternative selected
   --    Open_Alternative will be True if there were any open alternatives

   procedure Select_Protected_Entry_Call
     (Self_ID   : Task_ID;
      Object    : POE.Protection_Entries_Access;
      Call      : out Entry_Call_Link);
   --  Select an entry of a protected object

   procedure Enqueue_Call (Entry_Call : Entry_Call_Link);
   procedure Dequeue_Call (Entry_Call : Entry_Call_Link);
   --  Enqueue (dequeue) the call to (from) whatever server they are
   --  calling, whether a task or a protected object.

   procedure Requeue_Call_With_New_Prio
     (Entry_Call : Entry_Call_Link; Prio : System.Any_Priority);
   --  Change Priority of the call and re insert to the queue when priority
   --  queueing is in effect. When FIFO is inforced, this routine
   --  should not have any effect.

end System.Tasking.Queuing;
