------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1997-2001, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package encapsulates all direct interfaces to task debugging services
--  that are needed by gdb with gnat mode (1.17 and higher)

with Interfaces.C;
with System.Tasking;
with System.OS_Interface;

package System.Tasking.Debug is

   subtype int is Interfaces.C.int;
   subtype unsigned_long is Interfaces.C.unsigned_long;

   package ST renames System.Tasking;

   Known_Tasks : array (0 .. 999) of Task_ID;
   --  Global array of tasks read by gdb, and updated by
   --  Create_Task and Finalize_TCB

   procedure Task_Creation_Hook (Thread : OS_Interface.Thread_Id);
   --  This procedure is used to notify VxGdb of task's creation.
   --  It must be called by the task's creator.

   procedure Task_Termination_Hook;
   --  This procedure is used to notify VxGdb of task's termination.

   function Self return Task_ID;
   --  return system ID of current task

   procedure List_Tasks;
   --  Print a list of all the known Ada tasks with abbreviated state
   --  information, one-per-line, to the standard output file

   procedure Print_Current_Task;
   procedure Print_Task_Info_Header;
   procedure Print_Task_Info (T : Task_ID);
   --  Write TASK_ID of current task, in hexadecimal, as one line, to
   --  the standard output file
   --
   --  Beware that Print_Current_Task may print garbage during an early
   --  stage of activation. There is a small window where a task is just
   --  initializing itself and has not yet recorded its own task Id.
   --
   --  Beware that Print_Current_Task will either not work at all or print
   --  garbage if it has interrupted a thread of control that does not
   --  correspond to any Ada task. For example, this is could happen if
   --  the debugger interrupts a signal handler that is using an alternate
   --  stack, or interrupts the dispatcher in the underlying thread
   --  implementation.

   procedure Set_User_State (Value : Integer);

   procedure Print_Accept_Info (T : Task_ID);

   procedure Trace
     (Self_ID  : Task_ID;
      Msg      : String;
      Other_ID : Task_ID;
      Flag     : Character);

   procedure Trace
     (Self_ID : Task_ID;
      Msg     : String;
      Flag    : Character);

   procedure Trace
     (Msg  : String;
      Flag : Character);

   procedure Trace
     (Msg      : String;
      Other_ID : Task_ID;
      Flag     : Character);

   procedure Set_Trace
     (Flag  : Character;
      Value : Boolean := True);

   function Image (T : Task_ID) return String;

   procedure Suspend_All_Tasks (Thread_Self : OS_Interface.Thread_Id);
   --  Suspend all the tasks except the one whose associated thread is
   --  Thread_Self by traversing All_Tasks_Lists and calling
   --  System.Task_Primitives.Operations.Suspend_Task
   --  Such functionality is needed by gdb on some targets (e.g VxWorks)
   --  Warning: for efficiency purposes, there is no locking.

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id);
   --  Resume all the tasks except the one whose associated thread is
   --  Thread_Self by traversing All_Tasks_Lists and calling
   --  System.Task_Primitives.Operations.Continue_Task
   --  Such functionality is needed by gdb on some targets (e.g VxWorks)
   --  Warning: for efficiency purposes, there is no locking.

end System.Tasking.Debug;

-----------------------------
-- Use of These Functions  --
-----------------------------

--  Calling complicated functions from the debugger is generally pretty
--  risky, especially in a multithreaded program.

--  The debugger may interrupt something that is not an Ada task,
--  within the thread implementation, and which is not async-safe.

--  For example, under Solaris, it can interrupt code in "_dynamiclwps",
--  which seems to serve as dispatcher when all the user threads are
--  suspended. By experience, we have found that one cannot safely
--  do certain things, apparently including calls to thread primitives
--  from the debugger if the debugger has interrupted at one of these
--  unsafe points. In general, if you interrupt a running program
--  asynchronously (e.g. via control-C), it will not be safe to
--  call the subprograms in this package.

-----------------
-- Future work --
-----------------

--  It would be nice to be able to tell whether execution has been
--  interrupted in an Ada task. A heuristic way of checking this would
--  be if we added to the Ada TCB a component that always contains a
--  constant value that is unlikely to occur accidentally in code or
--  data. We could then check this in the debugger-callable subprograms,
--  and simply return an error code if it looks unsafe to proceed.

--  ???
--  Recently we have added such a marker as a local variable of the
--  task-wrapper routine. This allows Self to generate a fake ATCB for
--  non-Ada threads of control. Given this capability, it is probably
--  time to revisit the issue above.

--  DEADLOCK

--  We follow a simple rule here to avoid deadlock:

--  We do not use any locks in functions called by gdb, and we do not
--  traverse linked lists.
--
--  The use of an array (Known_Tasks) has many advantages:

--   - Easy and fast to examine;
--   - No risk of dangling references (to the next element) when traversing
--     the array.
