------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K I N G . R E S T R I C T E D . S T A G E S      --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

--  This is a simplified version of the System.Tasking.Stages package,
--  intended to be used in a restricted run time.

--  This package represents the high level tasking interface used by the
--  compiler to expand Ada 95 tasking constructs into simpler run time calls
--  (aka GNARLI, GNU Ada Run-time Library Interface)

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes
--  in exp_ch9.adb and possibly exp_ch7.adb

--  The restricted GNARLI is also composed of System.Protected_Objects and
--  System.Protected_Objects.Single_Entry

with System.Task_Info;
--  used for Task_Info_Type

with System.Parameters;
--  used for Size_Type

package System.Tasking.Restricted.Stages is
   pragma Elaborate_Body;

   ---------------------------------
   -- Compiler Interface (GNARLI) --
   ---------------------------------

   --  The compiler will expand in the GNAT tree the following construct:

   --   task type T (Discr : Integer);

   --   task body T is
   --      ...declarations, possibly some controlled...
   --   begin
   --      ...B...;
   --   end T;

   --   T1 : T (1);

   --  as follows:

   --   task type t (discr : integer);
   --   tE : aliased boolean := false;
   --   tZ : size_type := unspecified_size;

   --   type tV (discr : integer) is limited record
   --      _task_id : task_id;
   --      _atcb : aliased system__tasking__ada_task_control_block (0);
   --   end record;

   --   procedure tB (_task : access tV);
   --   freeze tV [
   --      procedure tVIP (_init : in out tV; _master : master_id;
   --        _chain : in out activation_chain; _task_name : in string;
   --        discr : integer) is
   --      begin
   --         _init.discr := discr;
   --         _init._task_id := null;
   --         system__tasking__ada_task_control_blockIP (_init._atcb, 0);
   --         _init._task_id := _init._atcb'unchecked_access;
   --         create_restricted_task (unspecified_priority, tZ,
   --           unspecified_task_info, task_procedure_access!(tB'address),
   --           _init'address, tE'unchecked_access, _chain, _task_name, _init.
   --           _task_id);
   --         return;
   --      end tVIP;

   --   _chain : aliased activation_chain;
   --   activation_chainIP (_chain);

   --   procedure tB (_task : access tV) is
   --      discr : integer renames _task.discr;

   --      procedure _clean is
   --      begin
   --         complete_restricted_task;
   --         finalize_list (F14b);
   --         return;
   --      end _clean;

   --   begin
   --      ...declarations...
   --      complete_restricted_activation;
   --      ...B...;
   --      return;
   --   at end
   --      _clean;
   --   end tB;

   --   tE := true;
   --   t1 : t (1);
   --   t1S : constant String := "t1";
   --   tIP (t1, 3, _chain, t1S, 1);

   --   activate_restricted_tasks (_chain'unchecked_access);

   procedure Create_Restricted_Task
     (Priority      : Integer;
      Stack_Address : System.Address;
      Size          : System.Parameters.Size_Type;
      Task_Info     : System.Task_Info.Task_Info_Type;
      State         : Task_Procedure_Access;
      Discriminants : System.Address;
      Elaborated    : Access_Boolean;
      Chain         : in out Activation_Chain;
      Task_Image    : String;
      Created_Task  : Task_Id);
   --  Compiler interface only. Do not call from within the RTS.
   --  This must be called to create a new task.
   --
   --  Priority is the task's priority (assumed to be in the
   --  System.Any_Priority'Range)
   --
   --  Stack_Address is the start address of the stack associated to the task,
   --  in case it has been preallocated by the compiler; it is equal to
   --  Null_Address when the stack needs to be allocated by the underlying
   --  operating system.
   --
   --  Size is the stack size of the task to create
   --
   --  Task_Info is the task info associated with the created task, or
   --  Unspecified_Task_Info if none.
   --
   --  State is the compiler generated task's procedure body
   --
   --  Discriminants is a pointer to a limited record whose discriminants are
   --  those of the task to create. This parameter should be passed as the
   --  single argument to State.
   --
   --  Elaborated is a pointer to a Boolean that must be set to true on exit
   --  if the task could be sucessfully elaborated.
   --
   --  Chain is a linked list of task that needs to be created. On exit,
   --  Created_Task.Activation_Link will be Chain.T_ID, and Chain.T_ID will be
   --  Created_Task (the created task will be linked at the front of Chain).
   --
   --  Task_Image is a string created by the compiler that the run time can
   --  store to ease the debugging and the Ada.Task_Identification facility.
   --
   --  Created_Task is the resulting task.
   --
   --  This procedure can raise Storage_Error if the task creation fails

   procedure Activate_Restricted_Tasks
     (Chain_Access : Activation_Chain_Access);
   --  Compiler interface only. Do not call from within the RTS.
   --  This must be called by the creator of a chain of one or more new tasks,
   --  to activate them. The chain is a linked list that up to this point is
   --  only known to the task that created them, though the individual tasks
   --  are already in the All_Tasks_List.
   --
   --  The compiler builds the chain in LIFO order (as a stack). Another
   --  version of this procedure had code to reverse the chain, so as to
   --  activate the tasks in the order of declaration. This might be nice, but
   --  it is not needed if priority-based scheduling is supported, since all
   --  the activated tasks synchronize on the activators lock before they start
   --  activating and so they should start activating in priority order.

   procedure Complete_Restricted_Activation;
   --  Compiler interface only. Do not call from within the RTS. This should be
   --  called from the task body at the end of the elaboration code for its
   --  declarative part. Decrement the count of tasks to be activated by the
   --  activator and wake it up so it can check to see if all tasks have been
   --  activated. Except for the environment task, which should never call this
   --  procedure, T.Activator should only be null iff T has completed
   --  activation.

   procedure Complete_Restricted_Task;
   --  Compiler interface only. Do not call from within the RTS. This should be
   --  called from an implicit at-end handler associated with the task body,
   --  when it completes. From this point, the current task will become not
   --  callable. If the current task have not completed activation, this should
   --  be done now in order to wake up the activator (the environment task).

   function Restricted_Terminated (T : Task_Id) return Boolean;
   --  Compiler interface only. Do not call from within the RTS. This is called
   --  by the compiler to implement the 'Terminated attribute.
   --
   --  source code:
   --     T1'Terminated
   --
   --  code expansion:
   --     restricted_terminated (t1._task_id)

   procedure Finalize_Global_Tasks;
   --  This is needed to support the compiler interface; it will only be called
   --  by the Environment task in the binder generated file (by adafinal).
   --  Instead, it will cause the Environment to block forever, since none of
   --  the dependent tasks are expected to terminate

end System.Tasking.Restricted.Stages;
