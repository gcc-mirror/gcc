------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . T A S K I N G . U T I L I T I E S             --
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

--  This package provides RTS Internal Declarations.
--  These declarations are not part of the GNARLI

with Ada.Unchecked_Conversion;
with System.Task_Primitives;

package System.Tasking.Utilities is

   function ATCB_To_Address is new
     Ada.Unchecked_Conversion (Task_Id, System.Task_Primitives.Task_Address);

   ---------------------------------
   -- Task_Stage Related routines --
   ---------------------------------

   procedure Make_Independent;
   --  Move the current task to the outermost level (level 2) of the master
   --  hierarchy of the environment task. That is one level further out
   --  than normal tasks defined in library-level packages (level 3). The
   --  environment task will wait for level 3 tasks to terminate normally,
   --  then it will abort all the level 2 tasks. See Finalize_Global_Tasks
   --  procedure for more information.
   --
   --  This is a dangerous operation, and should never be used on nested tasks
   --  or tasks that depend on any objects that might be finalized earlier than
   --  the termination of the environment task. It is for internal use by the
   --  GNARL, to prevent such internal server tasks from preventing a partition
   --  from terminating.
   --
   --  Also note that the run time assumes that the parent of an independent
   --  task is the environment task. If this is not the case, Make_Independent
   --  will change the task's parent. This assumption is particularly
   --  important for master level completion and for the computation of
   --  Independent_Task_Count.

   Independent_Task_Count : Natural := 0;
   --  Number of independent task. This counter is incremented each time
   --  Make_Independent is called. Note that if a server task terminates,
   --  this counter will not be decremented. Since Make_Independent locks
   --  the environment task (because every independent task depends on it),
   --  this counter is protected by the environment task's lock.

   ---------------------------------
   -- Task Abort Related Routines --
   ---------------------------------

   procedure Cancel_Queued_Entry_Calls (T : Task_Id);
   --  Cancel any entry calls queued on target task.
   --  Call this while holding T's lock (or RTS_Lock in Single_Lock mode).

   procedure Exit_One_ATC_Level (Self_ID : Task_Id);
   pragma Inline (Exit_One_ATC_Level);
   --  Call only with abort deferred and holding lock of Self_ID.
   --  This is a bit of common code for all entry calls.
   --  The effect is to exit one level of ATC nesting.

   procedure Abort_One_Task (Self_ID : Task_Id; T : Task_Id);
   --  Similar to Locked_Abort_To_Level (Self_ID, T, 0), but:
   --    (1) caller should be holding no locks
   --    (2) may be called for tasks that have not yet been activated
   --    (3) always aborts whole task

   procedure Abort_Tasks (Tasks : Task_List);
   --  Abort_Tasks is called to initiate abort, however, the actual
   --  aborting is done by aborted task by means of Abort_Handler

   procedure Make_Passive (Self_ID : Task_Id; Task_Completed : Boolean);
   --  Update counts to indicate current task is either terminated or
   --  accepting on a terminate alternative. Call holding no locks except
   --  Global_Task_Lock when calling from Terminate_Task, and RTS_Lock when
   --  Single_Lock is True.

end System.Tasking.Utilities;
