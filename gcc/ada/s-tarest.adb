------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K I N G . R E S T R I C T E D . S T A G E S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1999-2005, Free Software Foundation, Inc.          --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram alpha order check, since we group soft link
--  bodies and also separate off subprograms for restricted GNARLI.

--  This is a simplified version of the System.Tasking.Stages package,
--  intended to be used in a restricted run time.

--  This package represents the high level tasking interface used by the
--  compiler to expand Ada 95 tasking constructs into simpler run time calls.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Parameters;
--  used for Size_Type
--           Single_Lock

with System.Task_Info;
--  used for Task_Info_Type

with System.Task_Primitives.Operations;
--  used for Enter_Task
--           Write_Lock
--           Unlock
--           Wakeup
--           Get_Priority

with System.Soft_Links;
--  used for the non-tasking routines (*_NT) that refer to global data.
--  They are needed here before the tasking run time has been elaborated.
--  used for Create_TSD
--  This package also provides initialization routines for task specific data.
--  The GNARL must call these to be sure that all non-tasking
--  Ada constructs will work.

with System.Soft_Links.Tasking;
--  Used for Init_Tasking_Soft_Links

with System.Secondary_Stack;
--  used for SS_Init;

with System.Storage_Elements;
--  used for Storage_Array;

package body System.Tasking.Restricted.Stages is

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;
   package SSE  renames System.Storage_Elements;
   package SST  renames System.Secondary_Stack;

   use Parameters;
   use Task_Primitives.Operations;
   use Task_Info;

   Global_Task_Lock : aliased System.Task_Primitives.RTS_Lock;
   --  This is a global lock; it is used to execute in mutual exclusion
   --  from all other tasks. It is only used by Task_Lock and Task_Unlock.

   -----------------------------------------------------------------
   -- Tasking versions of services needed by non-tasking programs --
   -----------------------------------------------------------------

   procedure Task_Lock;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.

   procedure Task_Unlock;
   --  Releases lock previously set by call to Task_Lock. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Task_Wrapper (Self_ID : Task_Id);
   --  This is the procedure that is called by the GNULL from the
   --  new context when a task is created. It waits for activation
   --  and then calls the task body procedure. When the task body
   --  procedure completes, it terminates the task.

   procedure Terminate_Task (Self_ID : Task_Id);
   --  Terminate the calling task.
   --  This should only be called by the Task_Wrapper procedure.

   procedure Init_RTS;
   --  This procedure performs the initialization of the GNARL.
   --  It consists of initializing the environment task, global locks, and
   --  installing tasking versions of certain operations used by the compiler.
   --  Init_RTS is called during elaboration.

   ---------------
   -- Task_Lock --
   ---------------

   procedure Task_Lock is
   begin
      STPO.Write_Lock (Global_Task_Lock'Access, Global_Lock => True);
   end Task_Lock;

   -----------------
   -- Task_Unlock --
   -----------------

   procedure Task_Unlock is
   begin
      STPO.Unlock (Global_Task_Lock'Access, Global_Lock => True);
   end Task_Unlock;

   ------------------
   -- Task_Wrapper --
   ------------------

   --  The task wrapper is a procedure that is called first for each task
   --  task body, and which in turn calls the compiler-generated task body
   --  procedure. The wrapper's main job is to do initialization for the task.

   --  The variable ID in the task wrapper is used to implement the Self
   --  function on targets where there is a fast way to find the stack base
   --  of the current thread, since it should be at a fixed offset from the
   --  stack base.

   procedure Task_Wrapper (Self_ID : Task_Id) is
      ID : Task_Id := Self_ID;
      pragma Volatile (ID);

      pragma Warnings (Off, ID);
      --  Turn off warnings (stand alone volatile constant has to be
      --  imported, so we cannot just make ID constant).

      --  Do not delete this variable.
      --  In some targets, we need this variable to implement a fast Self.

      use type System.Parameters.Size_Type;
      use type SSE.Storage_Offset;

      Secondary_Stack : aliased SSE.Storage_Array
        (1 .. Self_ID.Common.Compiler_Data.Pri_Stack_Info.Size *
           SSE.Storage_Offset (Parameters.Sec_Stack_Ratio) / 100);
      Secondary_Stack_Address : System.Address := Secondary_Stack'Address;

   begin
      if not Parameters.Sec_Stack_Dynamic then
         Self_ID.Common.Compiler_Data.Sec_Stack_Addr :=
           Secondary_Stack'Address;
         SST.SS_Init (Secondary_Stack_Address, Integer (Secondary_Stack'Last));
      end if;

      --  Initialize low-level TCB components, that
      --  cannot be initialized by the creator.

      Enter_Task (Self_ID);

      --  Call the task body procedure.

      begin
         --  We are separating the following portion of the code in order to
         --  place the exception handlers in a different block.
         --  In this way we do not call Set_Jmpbuf_Address (which needs
         --  Self) before we set Self in Enter_Task.
         --  Note that in the case of Ravenscar HI-E where there are no
         --  exception handlers, the exception handler is suppressed.

         --  Call the task body procedure.

         Self_ID.Common.Task_Entry_Point (Self_ID.Common.Task_Arg);
         Terminate_Task (Self_ID);

      exception
         when others =>
            Terminate_Task (Self_ID);
      end;
   end Task_Wrapper;

   -----------------------
   -- Restricted GNARLI --
   -----------------------

   -------------------------------
   -- Activate_Restricted_Tasks --
   -------------------------------

   --  Note that locks of activator and activated task are both locked
   --  here. This is necessary because C.State and Self.Wait_Count
   --  have to be synchronized. This is safe from deadlock because
   --  the activator is always created before the activated task.
   --  That satisfies our in-order-of-creation ATCB locking policy.

   procedure Activate_Restricted_Tasks
     (Chain_Access : Activation_Chain_Access)
   is
      Self_ID       : constant Task_Id := STPO.Self;
      C             : Task_Id;
      Activate_Prio : System.Any_Priority;
      Success       : Boolean;

   begin
      pragma Assert (Self_ID = Environment_Task);
      pragma Assert (Self_ID.Common.Wait_Count = 0);

      if Single_Lock then
         Lock_RTS;
      end if;

      --  Lock self, to prevent activated tasks
      --  from racing ahead before we finish activating the chain.

      Write_Lock (Self_ID);

      --  Activate all the tasks in the chain.
      --  Creation of the thread of control was deferred until
      --  activation. So create it now.

      C := Chain_Access.T_ID;

      while C /= null loop
         if C.Common.State /= Terminated then
            pragma Assert (C.Common.State = Unactivated);

            Write_Lock (C);

            if C.Common.Base_Priority < Get_Priority (Self_ID) then
               Activate_Prio := Get_Priority (Self_ID);
            else
               Activate_Prio := C.Common.Base_Priority;
            end if;

            STPO.Create_Task
              (C, Task_Wrapper'Address,
               Parameters.Size_Type
                 (C.Common.Compiler_Data.Pri_Stack_Info.Size),
               Activate_Prio, Success);

            Self_ID.Common.Wait_Count := Self_ID.Common.Wait_Count + 1;

            if Success then
               C.Common.State := Runnable;
            else
               raise Program_Error;
            end if;

            Unlock (C);
         end if;

         C := C.Common.Activation_Link;
      end loop;

      Self_ID.Common.State := Activator_Sleep;

      --  Wait for the activated tasks to complete activation.
      --  It is unsafe to abort any of these tasks until the count goes to
      --  zero.

      loop
         exit when Self_ID.Common.Wait_Count = 0;
         Sleep (Self_ID, Activator_Sleep);
      end loop;

      Self_ID.Common.State := Runnable;
      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Remove the tasks from the chain.

      Chain_Access.T_ID := null;
   end Activate_Restricted_Tasks;

   ------------------------------------
   -- Complete_Restricted_Activation --
   ------------------------------------

   --  As in several other places, the locks of the activator and activated
   --  task are both locked here. This follows our deadlock prevention lock
   --  ordering policy, since the activated task must be created after the
   --  activator.

   procedure Complete_Restricted_Activation is
      Self_ID   : constant Task_Id := STPO.Self;
      Activator : constant Task_Id := Self_ID.Common.Activator;

   begin
      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Activator);
      Write_Lock (Self_ID);

      --  Remove dangling reference to Activator,
      --  since a task may outlive its activator.

      Self_ID.Common.Activator := null;

      --  Wake up the activator, if it is waiting for a chain
      --  of tasks to activate, and we are the last in the chain
      --  to complete activation

      if Activator.Common.State = Activator_Sleep then
         Activator.Common.Wait_Count := Activator.Common.Wait_Count - 1;

         if Activator.Common.Wait_Count = 0 then
            Wakeup (Activator, Activator_Sleep);
         end if;
      end if;

      Unlock (Self_ID);
      Unlock (Activator);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  After the activation, active priority should be the same
      --  as base priority. We must unlock the Activator first,
      --  though, since it should not wait if we have lower priority.

      if Get_Priority (Self_ID) /= Self_ID.Common.Base_Priority then
         Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
      end if;
   end Complete_Restricted_Activation;

   ------------------------------
   -- Complete_Restricted_Task --
   ------------------------------

   procedure Complete_Restricted_Task is
   begin
      STPO.Self.Common.State := Terminated;
   end Complete_Restricted_Task;

   ----------------------------
   -- Create_Restricted_Task --
   ----------------------------

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
      Created_Task  : Task_Id)
   is
      Self_ID       : constant Task_Id := STPO.Self;
      Base_Priority : System.Any_Priority;
      Success       : Boolean;

   begin
      --  Stack is not preallocated on this target, so that
      --  Stack_Address must be null.

      pragma Assert (Stack_Address = Null_Address);

      if Priority = Unspecified_Priority then
         Base_Priority := Self_ID.Common.Base_Priority;
      else
         Base_Priority := System.Any_Priority (Priority);
      end if;

      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Self_ID);

      --  With no task hierarchy, the parent of all non-Environment tasks that
      --  are created must be the Environment task

      Initialize_ATCB
        (Self_ID, State, Discriminants, Self_ID, Elaborated, Base_Priority,
         Task_Info, Size, Created_Task, Success);

      --  If we do our job right then there should never be any failures,
      --  which was probably said about the Titanic; so just to be safe,
      --  let's retain this code for now

      if not Success then
         Unlock (Self_ID);

         if Single_Lock then
            Unlock_RTS;
         end if;

         raise Program_Error;
      end if;

      Created_Task.Entry_Calls (1).Self := Created_Task;

      Created_Task.Common.Task_Image_Len :=
        Integer'Min (Created_Task.Common.Task_Image'Length, Task_Image'Length);
      Created_Task.Common.Task_Image
        (1 .. Created_Task.Common.Task_Image_Len) := Task_Image;

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Create TSD as early as possible in the creation of a task, since it
      --  may be used by the operation of Ada code within the task.

      SSL.Create_TSD (Created_Task.Common.Compiler_Data);
      Created_Task.Common.Activation_Link := Chain.T_ID;
      Chain.T_ID := Created_Task;
   end Create_Restricted_Task;

   ---------------------------
   -- Finalize_Global_Tasks --
   ---------------------------

   --  This is needed to support the compiler interface; it will only be called
   --  by the Environment task. Instead, it will cause the Environment to block
   --  forever, since none of the dependent tasks are expected to terminate

   procedure Finalize_Global_Tasks is
      Self_ID : constant Task_Id := STPO.Self;

   begin
      pragma Assert (Self_ID = STPO.Environment_Task);

      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Self_ID);
      Sleep (Self_ID, Master_Completion_Sleep);
      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      --  Should never return from Master Completion Sleep

      raise Program_Error;
   end Finalize_Global_Tasks;

   ---------------------------
   -- Restricted_Terminated --
   ---------------------------

   function Restricted_Terminated (T : Task_Id) return Boolean is
   begin
      return T.Common.State = Terminated;
   end Restricted_Terminated;

   --------------------
   -- Terminate_Task --
   --------------------

   procedure Terminate_Task (Self_ID : Task_Id) is
   begin
      Self_ID.Common.State := Terminated;
   end Terminate_Task;

   --------------
   -- Init_RTS --
   --------------

   procedure Init_RTS is
   begin
      Tasking.Initialize;

      --  Initialize lock used to implement mutual exclusion between all tasks

      STPO.Initialize_Lock (Global_Task_Lock'Access, STPO.Global_Task_Level);

      --  Notify that the tasking run time has been elaborated so that
      --  the tasking version of the soft links can be used.

      SSL.Lock_Task   := Task_Lock'Access;
      SSL.Unlock_Task := Task_Unlock'Access;
      SSL.Adafinal    := Finalize_Global_Tasks'Access;

      --  Initialize the tasking soft links (if not done yet) that are common
      --  to the full and the restricted run times.

      SSL.Tasking.Init_Tasking_Soft_Links;
   end Init_RTS;

begin
   Init_RTS;
end System.Tasking.Restricted.Stages;
