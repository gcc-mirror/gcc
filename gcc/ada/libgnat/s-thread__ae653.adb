------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T H R E A D S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VxWorks 653 version of this package

pragma Restrictions (No_Tasking);
--  The VxWorks 653 version of this package is intended only for programs
--  which do not use Ada tasking. This restriction ensures that this
--  will be checked by the binder.

with System.OS_Versions; use System.OS_Versions;

package body System.Threads is

   use Interfaces.C;

   package SSL renames System.Soft_Links;

   Current_ATSD : aliased System.Address := System.Null_Address;
   pragma Export (C, Current_ATSD, "__gnat_current_atsd");

   Main_ATSD : aliased ATSD;
   --  TSD for environment task

   Stack_Limit : Address;

   pragma Import (C, Stack_Limit, "__gnat_stack_limit");

   type Set_Stack_Limit_Proc_Acc is access procedure;
   pragma Convention (C, Set_Stack_Limit_Proc_Acc);

   Set_Stack_Limit_Hook : Set_Stack_Limit_Proc_Acc;
   pragma Import (C, Set_Stack_Limit_Hook, "__gnat_set_stack_limit_hook");
   --  Procedure to be called when a task is created to set stack limit if
   --  limit checking is used.

   --------------------------
   -- VxWorks specific API --
   --------------------------

   ERROR : constant STATUS := Interfaces.C.int (-1);

   function taskIdVerify (tid : t_id) return STATUS;
   pragma Import (C, taskIdVerify, "taskIdVerify");

   function taskIdSelf return t_id;
   pragma Import (C, taskIdSelf, "taskIdSelf");

   function taskVarAdd
     (tid : t_id; pVar : System.Address) return int;
   pragma Import (C, taskVarAdd, "taskVarAdd");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Init_RTS;
   --  This procedure performs the initialization of the run-time lib.
   --  It installs System.Threads versions of certain operations of the
   --  run-time lib.

   procedure Install_Handler;
   pragma Import (C, Install_Handler, "__gnat_install_handler");

   function  Get_Sec_Stack return SST.SS_Stack_Ptr;

   procedure Set_Sec_Stack (Stack : SST.SS_Stack_Ptr);

   -----------------------
   -- Thread_Body_Enter --
   -----------------------

   procedure Thread_Body_Enter
     (Sec_Stack_Ptr        : SST.SS_Stack_Ptr;
      Process_ATSD_Address : System.Address)
   is
      --  Current_ATSD must already be a taskVar of taskIdSelf.
      --  No assertion because taskVarGet is not available on VxWorks/CERT,
      --  which is used on VxWorks 653 3.x as a guest OS.

      TSD : constant ATSD_Access := From_Address (Process_ATSD_Address);

   begin

      TSD.Sec_Stack_Ptr := Sec_Stack_Ptr;
      SST.SS_Init (TSD.Sec_Stack_Ptr);
      Current_ATSD := Process_ATSD_Address;

      Install_Handler;

      --  Initialize stack limit if needed

      if Current_ATSD /= Main_ATSD'Address
        and then Set_Stack_Limit_Hook /= null
      then
         Set_Stack_Limit_Hook.all;
      end if;
   end Thread_Body_Enter;

   ----------------------------------
   -- Thread_Body_Exceptional_Exit --
   ----------------------------------

   procedure Thread_Body_Exceptional_Exit
     (EO : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (EO);

   begin
      --  No action for this target

      null;
   end Thread_Body_Exceptional_Exit;

   -----------------------
   -- Thread_Body_Leave --
   -----------------------

   procedure Thread_Body_Leave is
   begin
      --  No action for this target

      null;
   end Thread_Body_Leave;

   --------------
   -- Init_RTS --
   --------------

   procedure Init_RTS is
      --  Register environment task
      Result : constant Interfaces.C.int := Register (taskIdSelf);
      pragma Assert (Result /= ERROR);

   begin
      Main_ATSD.Sec_Stack_Ptr := SSL.Get_Sec_Stack_NT;
      Current_ATSD := Main_ATSD'Address;
      Install_Handler;
      SSL.Get_Sec_Stack := Get_Sec_Stack'Access;
      SSL.Set_Sec_Stack := Set_Sec_Stack'Access;
   end Init_RTS;

   -------------------
   -- Get_Sec_Stack --
   -------------------

   function  Get_Sec_Stack return SST.SS_Stack_Ptr is
      CTSD : constant ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (CTSD /= null);
      return CTSD.Sec_Stack_Ptr;
   end Get_Sec_Stack;

   --------------
   -- Register --
   --------------

   function Register (T : Thread_Id) return STATUS is
      Result : STATUS;

   begin
      --  It cannot be assumed that the caller of this routine has a ATSD;
      --  so neither this procedure nor the procedures that it calls should
      --  raise or handle exceptions, or make use of a secondary stack.

      --  This routine is only necessary because taskVarAdd cannot be
      --  executed once an VxWorks 653 partition has entered normal mode
      --  (depending on configRecord.c, allocation could be disabled).
      --  Otherwise, everything could have been done in Thread_Body_Enter.

      if taskIdVerify (T) = ERROR then
         return ERROR;
      end if;

      Result := taskVarAdd (T, Current_ATSD'Address);
      pragma Assert (Result /= ERROR);

      --  The same issue applies to the task variable that contains the stack
      --  limit when that overflow checking mechanism is used instead of
      --  probing. If stack checking is enabled and limit checking is used,
      --  allocate the limit for this task. The environment task has this
      --  initialized by the binder-generated main when
      --  System.Stack_Check_Limits = True.

      pragma Warnings (Off);
      --  OS is a constant
      if Result /= ERROR
        and then OS /= VxWorks_653
        and then Set_Stack_Limit_Hook /= null
      then
         Result := taskVarAdd (T, Stack_Limit'Address);
         pragma Assert (Result /= ERROR);
      end if;
      pragma Warnings (On);

      return Result;
   end Register;

   -------------------
   -- Set_Sec_Stack --
   -------------------

   procedure Set_Sec_Stack (Stack : SST.SS_Stack_Ptr) is
      CTSD : constant ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (CTSD /= null);
      CTSD.Sec_Stack_Ptr := Stack;
   end Set_Sec_Stack;

begin
   --  Initialize run-time library

   Init_RTS;
end System.Threads;
