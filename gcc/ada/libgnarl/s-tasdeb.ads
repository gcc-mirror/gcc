------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1997-2018, Free Software Foundation, Inc.         --
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
   --  Return the user state for the current task

   -------------------------
   -- General GDB support --
   -------------------------

   Known_Tasks : array (0 .. 999) of Task_Id := (others => null);
   --  Global array of tasks read by gdb, and updated by Create_Task and
   --  Finalize_TCB

   Debug_Event_Activating           : constant := 1;
   Debug_Event_Run                  : constant := 2;
   Debug_Event_Suspended            : constant := 3;
   Debug_Event_Preempted            : constant := 4;
   Debug_Event_Terminated           : constant := 5;
   Debug_Event_Abort_Terminated     : constant := 6;
   Debug_Event_Exception_Terminated : constant := 7;
   Debug_Event_Rendezvous_Exception : constant := 8;
   Debug_Event_Handled              : constant := 9;
   Debug_Event_Dependents_Exception : constant := 10;
   Debug_Event_Handled_Others       : constant := 11;

   subtype Event_Kind_Type is Positive range 1 .. 11;
   --  Event kinds currently defined for debugging, used globally
   --  below and on a per task basis.

   procedure Signal_Debug_Event
     (Event_Kind : Event_Kind_Type;
      Task_Value : Task_Id);

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
   --  Thread_Self by traversing All_Tasks_List and calling
   --  System.Task_Primitives.Operations.Suspend_Task.

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id);
   --  Resume all the tasks except the one whose associated thread is
   --  Thread_Self by traversing All_Tasks_List and calling
   --  System.Task_Primitives.Operations.Continue_Task.

   procedure Stop_All_Tasks_Handler;
   --  Stop all the tasks by traversing All_Tasks_List and calling
   --  System.Task_Primitives.Operations.Stop_All_Task. This function
   --  can be used in an interrupt handler.

   procedure Stop_All_Tasks;
   --  Stop all the tasks by traversing All_Tasks_List and calling
   --  System.Task_Primitives.Operations.Stop_Task.

   procedure Continue_All_Tasks;
   --  Continue all the tasks by traversing All_Tasks_List and calling
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

   ---------------------------------
   -- Hooks for Valgrind/Helgrind --
   ---------------------------------

   procedure Master_Hook
     (Dependent    : Task_Id;
      Parent       : Task_Id;
      Master_Level : Integer);
   --  Indicate to Valgrind/Helgrind that the master of Dependent is
   --  Parent + Master_Level.

   procedure Master_Completed_Hook
     (Self_ID      : Task_Id;
      Master_Level : Integer);
   --  Indicate to Valgrind/Helgrind that Self_ID has completed the master
   --  Master_Level.

end System.Tasking.Debug;
