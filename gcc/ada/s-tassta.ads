------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K I N G . S T A G E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1999, Free Software Foundation, Inc.         --
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

--  This package represents the high level tasking interface used by the
--  compiler to expand Ada 95 tasking constructs into simpler run time calls
--  (aka GNARLI, GNU Ada Run-time Library Interface)

--  Note: Only the compiler is allowed to use this interface, by generating
--  direct calls to it, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes
--  in exp_ch9.adb and possibly exp_ch7.adb

with System.Task_Info;
--  used for Task_Info_Type

with System.Parameters;
--  used for Size_Type

package System.Tasking.Stages is
   pragma Elaborate_Body;

   --   The compiler will expand in the GNAT tree the following construct:
   --
   --   task type T (Discr : Integer);
   --
   --   task body T is
   --      ...declarations, possibly some controlled...
   --   begin
   --      ...B...;
   --   end T;
   --
   --   T1 : T (1);
   --
   --  as follows:
   --
   --   enter_master.all;
   --
   --   _chain : aliased activation_chain;
   --   _init_proc (_chain);
   --
   --   task type t (discr : integer);
   --   tE : aliased boolean := false;
   --   tZ : size_type := unspecified_size;
   --   type tV (discr : integer) is limited record
   --      _task_id : task_id;
   --   end record;
   --   procedure tB (_task : access tV);
   --   freeze tV [
   --      procedure _init_proc (_init : in out tV; _master : master_id;
   --        _chain : in out activation_chain; _task_id : in task_image_type;
   --        discr : integer) is
   --      begin
   --         _init.discr := discr;
   --         _init._task_id := null;
   --         create_task (unspecified_priority, tZ,
   --           unspecified_task_info, 0, _master,
   --           task_procedure_access!(tB'address),
   --           _init'address, tE'unchecked_access, _chain, _task_id, _init.
   --           _task_id);
   --         return;
   --      end _init_proc;
   --   ]
   --
   --   procedure tB (_task : access tV) is
   --      discr : integer renames _task.discr;
   --
   --      procedure _clean is
   --      begin
   --         abort_defer.all;
   --         complete_task;
   --         finalize_list (F14b);
   --         abort_undefer.all;
   --         return;
   --      end _clean;
   --   begin
   --      abort_undefer.all;
   --      ...declarations...
   --      complete_activation;
   --      ...B...;
   --      return;
   --   at end
   --      _clean;
   --   end tB;
   --
   --   tE := true;
   --   t1 : t (1);
   --   master : constant master_id := current_master.all;
   --   t1I : task_image_type := new string'"t1";
   --   _init_proc (t1, _master, _chain, t1I, 1);
   --
   --   activate_tasks (_chain'unchecked_access);

   procedure Abort_Tasks (Tasks : Task_List);
   --  Compiler interface only. Do not call from within the RTS.
   --  Initiate abortion, however, the actual abortion is done by abortee by
   --  means of Abort_Handler and Abort_Undefer
   --
   --  source code:
   --     Abort T1, T2;
   --  code expansion:
   --     abort_tasks (task_list'(t1._task_id, t2._task_id));

   procedure Activate_Tasks (Chain_Access : Activation_Chain_Access);
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
   --  the activated tasks synchronize on the activators lock before they
   --  start activating and so they should start activating in priority order.

   procedure Complete_Activation;
   --  Compiler interface only. Do not call from within the RTS.
   --  This should be called from the task body at the end of
   --  the elaboration code for its declarative part.
   --  Decrement the count of tasks to be activated by the activator and
   --  wake it up so it can check to see if all tasks have been activated.
   --  Except for the environment task, which should never call this procedure,
   --  T.Activator should only be null iff T has completed activation.

   procedure Complete_Master;
   --  Compiler interface only.  Do not call from within the RTS. This must
   --  be called on exit from any master where Enter_Master was called.
   --  Assume abort is deferred at this point.

   procedure Complete_Task;
   --  Compiler interface only. Do not call from within the RTS.
   --  This should be called from an implicit at-end handler
   --  associated with the task body, when it completes.
   --  From this point, the current task will become not callable.
   --  If the current task have not completed activation, this should be done
   --  now in order to wake up the activator (the environment task).

   procedure Create_Task
     (Priority      : Integer;
      Size          : System.Parameters.Size_Type;
      Task_Info     : System.Task_Info.Task_Info_Type;
      Num_Entries   : Task_Entry_Index;
      Master        : Master_Level;
      State         : Task_Procedure_Access;
      Discriminants : System.Address;
      Elaborated    : Access_Boolean;
      Chain         : in out Activation_Chain;
      Task_Image    : System.Task_Info.Task_Image_Type;
      Created_Task  : out Task_ID);
   --  Compiler interface only. Do not call from within the RTS.
   --  This must be called to create a new task.
   --
   --  Priority is the task's priority (assumed to be in the
   --   System.Any_Priority'Range)
   --  Size is the stack size of the task to create
   --  Task_Info is the task info associated with the created task, or
   --   Unspecified_Task_Info if none.
   --  State is the compiler generated task's procedure body
   --  Discriminants is a pointer to a limited record whose discriminants
   --   are those of the task to create. This parameter should be passed as
   --   the single argument to State.
   --  Elaborated is a pointer to a Boolean that must be set to true on exit
   --   if the task could be successfully elaborated.
   --  Chain is a linked list of task that needs to be created. On exit,
   --   Created_Task.Activation_Link will be Chain.T_ID, and Chain.T_ID
   --   will be Created_Task (e.g the created task will be linked at the front
   --   of Chain).
   --  Task_Image is a pointer to a string created by the compiler that the
   --   run time can store to ease the debugging and the
   --   Ada.Task_Identification facility.
   --  Created_Task is the resulting task.
   --
   --  This procedure can raise Storage_Error if the task creation failed.

   function Current_Master return Master_Level;
   --  Compiler interface only.
   --  This is called to obtain the current master nesting level.

   procedure Enter_Master;
   --  Compiler interface only.  Do not call from within the RTS.
   --  This must be called on entry to any "master" where a task,
   --  or access type designating objects containing tasks, may be
   --  declared.

   procedure Expunge_Unactivated_Tasks (Chain : in out Activation_Chain);
   --  Compiler interface only.  Do not call from within the RTS.
   --  This must be called by the compiler-generated code for an allocator if
   --  the allocated object contains tasks, if the allocator exits without
   --  calling Activate_Tasks for a given activation chains, as can happen if
   --  an exception occurs during initialization of the object.
   --
   --  This should be called ONLY for tasks created via an allocator. Recovery
   --  of storage for unactivated local task declarations is done by
   --  Complete_Master and Complete_Task.
   --
   --  We remove each task from Chain and All_Tasks_List before we free the
   --  storage of its ATCB.
   --
   --  In other places where we recover the storage of unactivated tasks, we
   --  need to clean out the entry queues, but here that should not be
   --  necessary, since these tasks should not have been visible to any other
   --  tasks, and so no task should be able to queue a call on their entries.
   --
   --  Just in case somebody misuses this subprogram, there is a check to
   --  verify this condition.

   procedure Finalize_Global_Tasks;
   --  This should be called to complete the execution of the environment task
   --  and shut down the tasking runtime system. It is the equivalent of
   --  Complete_Task, but for the environment task.
   --
   --  The environment task must first call Complete_Master, to wait for user
   --  tasks that depend on library-level packages to terminate. It then calls
   --  Abort_Dependents to abort the "independent" library-level server tasks
   --  that are created implicitly by the RTS packages (signal and timer server
   --  tasks), and then waits for them to terminate. Then, it calls
   --  Vulnerable_Complete_Task.
   --
   --  It currently also executes the global finalization list, and then resets
   --  the "soft links".

   procedure Free_Task (T : Task_ID);
   --  Recover all runtime system storage associated with the task T, but only
   --  if T has terminated. Do nothing in the other case. It is called from
   --  Unchecked_Deallocation, for objects that are or contain tasks.

   function Terminated (T : Task_ID) return Boolean;
   --  This is called by the compiler to implement the 'Terminated attribute.
   --  Though is not required to be so by the ARM, we choose to synchronize
   --  with the task's ATCB, so that this is more useful for polling the state
   --  of a task, and so that it becomes an abort completion point for the
   --  calling task (via Undefer_Abort).
   --
   --  source code:
   --     T1'Terminated
   --
   --  code expansion:
   --     terminated (t1._task_id)

end System.Tasking.Stages;
