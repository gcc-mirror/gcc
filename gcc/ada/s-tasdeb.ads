------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1997-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package encapsulates all direct interfaces to task debugging services
--  that are needed by gdb with gnat mode.

with System.Tasking;
with System.OS_Interface;

package System.Tasking.Debug is
   pragma Preelaborate;

   ------------------------------------------
   -- Application-level debugging routines --
   ------------------------------------------

   procedure List_Tasks;
   --  Print a list of all the known Ada tasks with abbreviated state
   --  information, one-per-line, to the standard error file.

   procedure Print_Current_Task;
   --  Write information about current task, in hexadecimal, as one line, to
   --  the standard error file.

   procedure Print_Task_Info (T : Task_Id);
   --  Similar to Print_Current_Task, for a given task

   procedure Set_User_State (Value : Long_Integer);
   --  Set user state value in the current task. This state will be displayed
   --  when calling List_Tasks or Print_Current_Task. It is useful for setting
   --  task specific state.

   function Get_User_State return Long_Integer;
   --  Return the user state for the current task.

   -------------------------
   -- General GDB support --
   -------------------------

   Known_Tasks : array (0 .. 999) of Task_Id := (others => null);
   --  Global array of tasks read by gdb, and updated by Create_Task and
   --  Finalize_TCB

   ----------------------------------
   -- VxWorks specific GDB support --
   ----------------------------------

   --  Although the following routines are implemented in a target independent
   --  manner, only VxWorks currently uses them.

   procedure Task_Creation_Hook (Thread : OS_Interface.Thread_Id);
   --  This procedure is used to notify GDB of task's creation. It must be
   --  called by the task's creator.

   procedure Task_Termination_Hook;
   --  This procedure is used to notify GDB of task's termination

   procedure Suspend_All_Tasks (Thread_Self : OS_Interface.Thread_Id);
   --  Suspend all the tasks except the one whose associated thread is
   --  Thread_Self by traversing All_Tasks_Lists and calling
   --  System.Task_Primitives.Operations.Suspend_Task.

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id);
   --  Resume all the tasks except the one whose associated thread is
   --  Thread_Self by traversing All_Tasks_Lists and calling
   --  System.Task_Primitives.Operations.Continue_Task.

   procedure Stop_All_Tasks_Handler;
   --  Stop all the tasks by traversing All_Tasks_Lists and calling
   --  System.Task_Primitives.Operations.Stop_All_Task. This function
   --  can be used in an interrupt handler.

   procedure Stop_All_Tasks;
   --  Stop all the tasks by traversing All_Tasks_Lists and calling
   --  System.Task_Primitives.Operations.Stop_Task.

   procedure Continue_All_Tasks;
   --  Continue all the tasks by traversing All_Tasks_Lists and calling
   --  System.Task_Primitives.Operations.Continue_Task.

   -------------------------------
   -- Run-time tracing routines --
   -------------------------------

   procedure Trace
     (Self_Id  : Task_Id;
      Msg      : String;
      Flag     : Character;
      Other_Id : Task_Id := null);
   --  If traces for Flag are enabled, display on Standard_Error a given
   --  message for the current task. Other_Id is an optional second task id
   --  to display.

   procedure Set_Trace
     (Flag  : Character;
      Value : Boolean := True);
   --  Enable or disable tracing for Flag. By default, flags in the range
   --  'A' .. 'Z' are disabled, others are enabled.

end System.Tasking.Debug;
