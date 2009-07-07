------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--            S Y S T E M . S T A C K _ U S A G E . T A S K I N G           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
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

--  This package provides exported subprograms to be called at debug time to
--  measure stack usage at run-time.

--  Note: this package must be a child package of System.Stack_Usage to have
--  visibility over its private part; it is however part of GNARL because it
--  needs to access tasking features via System.Tasking.Debug and
--  System.Task_Primitives.Operations;

package System.Stack_Usage.Tasking is

   procedure Report_All_Tasks;
   --  Print the current stack usage of all tasks on stderr. Exported to be
   --  called also in debug mode.

   pragma Export
     (C,
      Report_All_Tasks,
      "__gnat_tasks_stack_usage_report_all_tasks");

   procedure Report_Current_Task;
   --  Print the stack usage of current task on stderr. Exported to be called
   --  also in debug mode.

   pragma Export
     (C,
      Report_Current_Task,
      "__gnat_tasks_stack_usage_report_current_task");

   subtype Stack_Usage_Result is System.Stack_Usage.Task_Result;
   --  This type is a descriptor for task stack usage result

   type Stack_Usage_Result_Array is
     array (Positive range <>) of Stack_Usage_Result;

   function Get_Current_Task_Usage return Stack_Usage_Result;
   --  Return the current stack usage for the invoking task

   function Get_All_Tasks_Usage return Stack_Usage_Result_Array;
   --  Return an array containing the stack usage results for all tasks

   procedure Print (Obj : Stack_Usage_Result);
   --  Print Obj on stderr

end System.Stack_Usage.Tasking;
