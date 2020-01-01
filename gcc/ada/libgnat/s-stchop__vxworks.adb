------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . S T A C K _ C H E C K I N G . O P E R A T I O N S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1999-2020, Free Software Foundation, Inc.         --
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

--  This is the verson for VxWorks 5, VxWorks 6 Cert and VxWorks MILS

--  This file should be kept synchronized with the general implementation
--  provided by s-stchop.adb.

pragma Restrictions (No_Elaboration_Code);
--  We want to guarantee the absence of elaboration code because the
--  binder does not handle references to this package.

with System.Storage_Elements; use System.Storage_Elements;
with System.Parameters; use System.Parameters;
with Interfaces.C;

package body System.Stack_Checking.Operations is

   --  In order to have stack checking working appropriately on VxWorks we need
   --  to extract the stack size information from the VxWorks kernel itself.

   --  For VxWorks 5 & 6 the library for showing task-related information
   --  needs to be linked into the VxWorks system, when using stack checking.
   --   The taskShow library can be linked into the VxWorks system by either:

   --    * defining INCLUDE_SHOW_ROUTINES in config.h when using
   --      configuration header files, or

   --    * selecting INCLUDE_TASK_SHOW when using the Tornado project
   --      facility.

   --  VxWorks MILS includes the necessary routine in taskLib, so nothing
   --  special needs to be done there.

   Stack_Limit : Address;

   pragma Import (C, Stack_Limit, "__gnat_stack_limit");

   --  Stack_Limit contains the limit of the stack. This variable is later made
   --  a task variable (by calling taskVarAdd) and then correctly set to the
   --  stack limit of the task. Before being so initialized its value must be
   --  valid so that any subprogram with stack checking enabled will run. We
   --  use extreme values according to the direction of the stack.

   type Set_Stack_Limit_Proc_Acc is access procedure;
   pragma Convention (C, Set_Stack_Limit_Proc_Acc);

   Set_Stack_Limit_Hook : Set_Stack_Limit_Proc_Acc;
   pragma Import (C, Set_Stack_Limit_Hook, "__gnat_set_stack_limit_hook");
   --  Procedure to be called when a task is created to set stack
   --  limit.

   procedure Set_Stack_Limit_For_Current_Task;
   pragma Convention (C, Set_Stack_Limit_For_Current_Task);
   --  Register Initial_SP as the initial stack pointer value for the current
   --  task when it starts and Size as the associated stack area size. This
   --  should be called once, after the soft-links have been initialized?

   -----------------------------
   --  Initialize_Stack_Limit --
   -----------------------------

   procedure Initialize_Stack_Limit is
   begin

      Set_Stack_Limit_For_Current_Task;

      --  Will be called by every created task

      Set_Stack_Limit_Hook := Set_Stack_Limit_For_Current_Task'Access;
   end Initialize_Stack_Limit;

   --------------------------------------
   -- Set_Stack_Limit_For_Current_Task --
   --------------------------------------

   procedure Set_Stack_Limit_For_Current_Task is
      type OS_Stack_Info is record
         Size  : Interfaces.C.int;
         Base  : System.Address;
         Limit : System.Address;
      end record;
      pragma Convention (C, OS_Stack_Info);
      --  Type representing the information that we want to extract from the
      --  underlying kernel.

      procedure Get_Stack_Info (Stack : not null access OS_Stack_Info);
      pragma Import (C, Get_Stack_Info, "__gnat_get_stack_info");
      --  Procedure that fills the stack information associated to the
      --  currently executing task.

      Stack_Info : aliased OS_Stack_Info;

      Limit : System.Address;

   begin

      --  Get stack bounds from VxWorks

      Get_Stack_Info (Stack_Info'Access);

      if Stack_Grows_Down then
         Limit :=
           Stack_Info.Base - Storage_Offset (Stack_Info.Size) +
             Storage_Offset'(12_000);
      else
         Limit :=
           Stack_Info.Base + Storage_Offset (Stack_Info.Size) -
             Storage_Offset'(12_000);
      end if;

      Stack_Limit := Limit;

   end Set_Stack_Limit_For_Current_Task;
end System.Stack_Checking.Operations;
