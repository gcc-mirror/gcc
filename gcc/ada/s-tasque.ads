------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                 S Y S T E M . T A S K I N G . Q U E U I N G              --
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

with System.Tasking.Protected_Objects.Entries;

package System.Tasking.Queuing is

   package POE renames System.Tasking.Protected_Objects.Entries;

   procedure Broadcast_Program_Error
     (Self_ID      : Task_Id;
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

   function Head (E : Entry_Queue) return Entry_Call_Link;
   pragma Inline (Head);
   --  Return the head of entry_queue E

   procedure Dequeue_Head
     (E    : in out Entry_Queue;
      Call : out Entry_Call_Link);
   --  Remove and return the head of entry_queue E

   function Onqueue (Call : Entry_Call_Link) return Boolean;
   pragma Inline (Onqueue);
   --  Return True if Call is on any entry_queue at all

   function Count_Waiting (E : Entry_Queue) return Natural;
   --  Return number of calls on the waiting queue of E

   procedure Select_Task_Entry_Call
     (Acceptor         : Task_Id;
      Open_Accepts     : Accept_List_Access;
      Call             : out Entry_Call_Link;
      Selection        : out Select_Index;
      Open_Alternative : out Boolean);
   --  Select an entry for rendezvous.  On exit:
   --    Call will contain a pointer to the entry call record selected;
   --    Selection will contain the index of the alternative selected
   --    Open_Alternative will be True if there were any open alternatives

   procedure Select_Protected_Entry_Call
     (Self_ID : Task_Id;
      Object  : POE.Protection_Entries_Access;
      Call    : out Entry_Call_Link);
   --  Select an entry of a protected object

   procedure Enqueue_Call (Entry_Call : Entry_Call_Link);
   procedure Dequeue_Call (Entry_Call : Entry_Call_Link);
   --  Enqueue (dequeue) the call to (from) whatever server they are
   --  calling, whether a task or a protected object.

   procedure Requeue_Call_With_New_Prio
     (Entry_Call : Entry_Call_Link; Prio : System.Any_Priority);
   --  Change Priority of the call and re insert to the queue when priority
   --  queueing is in effect. When FIFO is enforced, this routine
   --  should not have any effect.

end System.Tasking.Queuing;
