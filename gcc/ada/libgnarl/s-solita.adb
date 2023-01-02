------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             S Y S T E M . S O F T _ L I N K S . T A S K I N G            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram alpha ordering check, since we group soft link bodies
--  and dummy soft link bodies together separately in this unit.

with Ada.Exceptions;
with Ada.Exceptions.Is_Null_Occurrence;

with System.Task_Primitives.Operations;
with System.Tasking;
with System.Stack_Checking;
with System.Secondary_Stack;

package body System.Soft_Links.Tasking is

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;

   use Ada.Exceptions;

   use type System.Secondary_Stack.SS_Stack_Ptr;

   use type System.Tasking.Task_Id;
   use type System.Tasking.Termination_Handler;

   ----------------
   -- Local Data --
   ----------------

   Initialized : Boolean := False;
   --  Boolean flag that indicates whether the tasking soft links have
   --  already been set.

   -----------------------------------------------------------------
   -- Tasking Versions of Services Needed by Non-Tasking Programs --
   -----------------------------------------------------------------

   function  Get_Jmpbuf_Address return  Address;
   procedure Set_Jmpbuf_Address (Addr : Address);
   --  Get/Set Jmpbuf_Address for current task

   function  Get_Sec_Stack return SST.SS_Stack_Ptr;
   procedure Set_Sec_Stack (Stack : SST.SS_Stack_Ptr);
   --  Get/Set location of current task's secondary stack

   procedure Timed_Delay_T (Time : Duration; Mode : Integer);
   --  Task-safe version of SSL.Timed_Delay

   procedure Task_Termination_Handler_T  (Excep : SSL.EO);
   --  Task-safe version of the task termination procedure

   function Get_Stack_Info return Stack_Checking.Stack_Access;
   --  Get access to the current task's Stack_Info

   --------------------------
   -- Soft-Link Get Bodies --
   --------------------------

   function Get_Jmpbuf_Address return  Address is
   begin
      return STPO.Self.Common.Compiler_Data.Jmpbuf_Address;
   end Get_Jmpbuf_Address;

   function Get_Sec_Stack return SST.SS_Stack_Ptr is
   begin
      return Result : constant SST.SS_Stack_Ptr :=
        STPO.Self.Common.Compiler_Data.Sec_Stack_Ptr
      do
         pragma Assert (Result /= null);
      end return;
   end Get_Sec_Stack;

   function Get_Stack_Info return Stack_Checking.Stack_Access is
   begin
      return STPO.Self.Common.Compiler_Data.Pri_Stack_Info'Access;
   end Get_Stack_Info;

   --------------------------
   -- Soft-Link Set Bodies --
   --------------------------

   procedure Set_Jmpbuf_Address (Addr : Address) is
   begin
      STPO.Self.Common.Compiler_Data.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address;

   procedure Set_Sec_Stack (Stack : SST.SS_Stack_Ptr) is
   begin
      STPO.Self.Common.Compiler_Data.Sec_Stack_Ptr := Stack;
   end Set_Sec_Stack;

   -------------------
   -- Timed_Delay_T --
   -------------------

   procedure Timed_Delay_T (Time : Duration; Mode : Integer) is
      Self_Id : constant System.Tasking.Task_Id := STPO.Self;

   begin
      --  In case pragma Detect_Blocking is active then Program_Error
      --  must be raised if this potentially blocking operation
      --  is called from a protected operation.

      if System.Tasking.Detect_Blocking
        and then Self_Id.Common.Protected_Action_Nesting > 0
      then
         raise Program_Error with "potentially blocking operation";
      else
         Abort_Defer.all;
         STPO.Timed_Delay (Self_Id, Time, Mode);
         Abort_Undefer.all;
      end if;
   end Timed_Delay_T;

   --------------------------------
   -- Task_Termination_Handler_T --
   --------------------------------

   procedure Task_Termination_Handler_T (Excep : SSL.EO) is
      Self_Id : constant System.Tasking.Task_Id := STPO.Self;
      Cause   : System.Tasking.Cause_Of_Termination;
      EO      : Ada.Exceptions.Exception_Occurrence;

   begin
      --  We can only be here because we are terminating the environment task.
      --  Task termination for all other tasks is handled in the Task_Wrapper.

      --  We do not want to enable this check and e.g. call System.OS_Lib.Abort
      --  here because some restricted run-times may not have System.OS_Lib
      --  and calling abort may do more harm than good to the main application.

      pragma Assert (Self_Id = STPO.Environment_Task);

      --  Normal task termination

      if Is_Null_Occurrence (Excep) then
         Cause := System.Tasking.Normal;
         Ada.Exceptions.Save_Occurrence (EO, Ada.Exceptions.Null_Occurrence);

      --  Abnormal task termination

      elsif Exception_Identity (Excep) = Standard'Abort_Signal'Identity then
         Cause := System.Tasking.Abnormal;
         Ada.Exceptions.Save_Occurrence (EO, Ada.Exceptions.Null_Occurrence);

      --  Termination because of an unhandled exception

      else
         Cause := System.Tasking.Unhandled_Exception;
         Ada.Exceptions.Save_Occurrence (EO, Excep);
      end if;

      --  There is no need for explicit protection against race conditions for
      --  this part because it can only be executed by the environment task
      --  after all the other tasks have been finalized. Note that there is no
      --  fall-back handler which could apply to this environment task because
      --  it has no parents, and, as specified in ARM C.7.3 par. 9/2, "the
      --  fall-back handler applies only to the dependent tasks of the task".

      if Self_Id.Common.Specific_Handler /= null then
         begin
            Self_Id.Common.Specific_Handler.all (Cause, Self_Id, EO);
         exception
            --  RM-C.7.3(16) requires all exceptions raised here to be ignored

            when others =>
               null;
         end;
      end if;
   end Task_Termination_Handler_T;

   -----------------------------
   -- Init_Tasking_Soft_Links --
   -----------------------------

   procedure Init_Tasking_Soft_Links is
   begin
      --  Set links only if not set already

      if not Initialized then

         --  Mark tasking soft links as initialized

         Initialized := True;

         --  The application being executed uses tasking so that the tasking
         --  version of the following soft links need to be used.

         SSL.Get_Jmpbuf_Address       := Get_Jmpbuf_Address'Access;
         SSL.Set_Jmpbuf_Address       := Set_Jmpbuf_Address'Access;
         SSL.Get_Sec_Stack            := Get_Sec_Stack'Access;
         SSL.Get_Stack_Info           := Get_Stack_Info'Access;
         SSL.Set_Sec_Stack            := Set_Sec_Stack'Access;
         SSL.Timed_Delay              := Timed_Delay_T'Access;
         SSL.Task_Termination_Handler := Task_Termination_Handler_T'Access;

         --  No need to create a new secondary stack, since we will use the
         --  default one created in s-secsta.adb.

         SSL.Set_Sec_Stack          (SSL.Get_Sec_Stack_NT);
         SSL.Set_Jmpbuf_Address     (SSL.Get_Jmpbuf_Address_NT);
      end if;

      pragma Assert (Get_Sec_Stack /= null);
   end Init_Tasking_Soft_Links;

end System.Soft_Links.Tasking;
