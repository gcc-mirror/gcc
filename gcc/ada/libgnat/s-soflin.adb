------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

pragma Warnings (Off);
--  Disable warnings as System.Soft_Links.Initialize is not Preelaborate. It is
--  safe to with this unit as its elaboration routine will only be initializing
--  NT_TSD, which is part of this package spec.
with System.Soft_Links.Initialize;
pragma Warnings (On);

package body System.Soft_Links is

   Stack_Limit : aliased System.Address := System.Null_Address;
   pragma Export (C, Stack_Limit, "__gnat_stack_limit");
   --  Needed for Vx6Cert (Vx653mc) GOS cert and ravenscar-cert runtimes,
   --  VxMILS cert, ravenscar-cert and full runtimes, Vx 5 default runtime

   --------------------
   -- Abort_Defer_NT --
   --------------------

   procedure Abort_Defer_NT is
   begin
      null;
   end Abort_Defer_NT;

   ----------------------
   -- Abort_Handler_NT --
   ----------------------

   procedure Abort_Handler_NT is
   begin
      null;
   end Abort_Handler_NT;

   ----------------------
   -- Abort_Undefer_NT --
   ----------------------

   procedure Abort_Undefer_NT is
   begin
      null;
   end Abort_Undefer_NT;

   -----------------
   -- Adafinal_NT --
   -----------------

   procedure Adafinal_NT is
   begin
      --  Handle normal task termination by the environment task, but only
      --  for the normal task termination. In the case of Abnormal and
      --  Unhandled_Exception they must have been handled before, and the
      --  task termination soft link must have been changed so the task
      --  termination routine is not executed twice.

      Task_Termination_Handler.all (Ada.Exceptions.Null_Occurrence);

      --  Finalize all library-level controlled objects if needed

      if Finalize_Library_Objects /= null then
         Finalize_Library_Objects.all;
      end if;
   end Adafinal_NT;

   ---------------------------
   -- Check_Abort_Status_NT --
   ---------------------------

   function Check_Abort_Status_NT return Integer is
   begin
      return Boolean'Pos (False);
   end Check_Abort_Status_NT;

   ------------------------
   -- Complete_Master_NT --
   ------------------------

   procedure Complete_Master_NT is
   begin
      null;
   end Complete_Master_NT;

   ----------------
   -- Create_TSD --
   ----------------

   procedure Create_TSD
     (New_TSD        : in out TSD;
      Sec_Stack      : SST.SS_Stack_Ptr;
      Sec_Stack_Size : System.Parameters.Size_Type)
   is
   begin
      New_TSD.Jmpbuf_Address := Null_Address;

      New_TSD.Sec_Stack_Ptr := Sec_Stack;
      SST.SS_Init (New_TSD.Sec_Stack_Ptr, Sec_Stack_Size);
   end Create_TSD;

   -----------------------
   -- Current_Master_NT --
   -----------------------

   function Current_Master_NT return Integer is
   begin
      return 0;
   end Current_Master_NT;

   -----------------
   -- Destroy_TSD --
   -----------------

   procedure Destroy_TSD (Old_TSD : in out TSD) is
   begin
      SST.SS_Free (Old_TSD.Sec_Stack_Ptr);
   end Destroy_TSD;

   ---------------------
   -- Enter_Master_NT --
   ---------------------

   procedure Enter_Master_NT is
   begin
      null;
   end Enter_Master_NT;

   --------------------------
   -- Get_Current_Excep_NT --
   --------------------------

   function Get_Current_Excep_NT return EOA is
   begin
      return NT_TSD.Current_Excep'Access;
   end Get_Current_Excep_NT;

   ------------------------
   -- Get_GNAT_Exception --
   ------------------------

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id is
   begin
      return Ada.Exceptions.Exception_Identity (Get_Current_Excep.all.all);
   end Get_GNAT_Exception;

   ---------------------------
   -- Get_Jmpbuf_Address_NT --
   ---------------------------

   function Get_Jmpbuf_Address_NT return  Address is
   begin
      return NT_TSD.Jmpbuf_Address;
   end Get_Jmpbuf_Address_NT;

   -----------------------------
   -- Get_Jmpbuf_Address_Soft --
   -----------------------------

   function Get_Jmpbuf_Address_Soft return  Address is
   begin
      return Get_Jmpbuf_Address.all;
   end Get_Jmpbuf_Address_Soft;

   ----------------------
   -- Get_Sec_Stack_NT --
   ----------------------

   function Get_Sec_Stack_NT return SST.SS_Stack_Ptr is
   begin
      return NT_TSD.Sec_Stack_Ptr;
   end Get_Sec_Stack_NT;

   -----------------------------
   -- Get_Sec_Stack_Soft --
   -----------------------------

   function Get_Sec_Stack_Soft return SST.SS_Stack_Ptr is
   begin
      return Get_Sec_Stack.all;
   end Get_Sec_Stack_Soft;

   -----------------------
   -- Get_Stack_Info_NT --
   -----------------------

   function Get_Stack_Info_NT return Stack_Checking.Stack_Access is
   begin
      return NT_TSD.Pri_Stack_Info'Access;
   end Get_Stack_Info_NT;

   -----------------------------
   -- Save_Library_Occurrence --
   -----------------------------

   procedure Save_Library_Occurrence (E : EOA) is
      use Ada.Exceptions;
   begin
      if not Library_Exception_Set then
         Library_Exception_Set := True;
         if E /= null then
            Ada.Exceptions.Save_Occurrence (Library_Exception, E.all);
         end if;
      end if;
   end Save_Library_Occurrence;

   ---------------------------
   -- Set_Jmpbuf_Address_NT --
   ---------------------------

   procedure Set_Jmpbuf_Address_NT (Addr : Address) is
   begin
      NT_TSD.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address_NT;

   procedure Set_Jmpbuf_Address_Soft (Addr : Address) is
   begin
      Set_Jmpbuf_Address (Addr);
   end Set_Jmpbuf_Address_Soft;

   ----------------------
   -- Set_Sec_Stack_NT --
   ----------------------

   procedure Set_Sec_Stack_NT (Stack : SST.SS_Stack_Ptr) is
   begin
      NT_TSD.Sec_Stack_Ptr := Stack;
   end Set_Sec_Stack_NT;

   ------------------------
   -- Set_Sec_Stack_Soft --
   ------------------------

   procedure Set_Sec_Stack_Soft (Stack : SST.SS_Stack_Ptr) is
   begin
      Set_Sec_Stack (Stack);
   end Set_Sec_Stack_Soft;

   ------------------
   -- Task_Lock_NT --
   ------------------

   procedure Task_Lock_NT is
   begin
      null;
   end Task_Lock_NT;

   ------------------
   -- Task_Name_NT --
   -------------------

   function Task_Name_NT return String is
   begin
      return "main_task";
   end Task_Name_NT;

   -------------------------
   -- Task_Termination_NT --
   -------------------------

   procedure Task_Termination_NT (Excep : EO) is
      pragma Unreferenced (Excep);
   begin
      null;
   end Task_Termination_NT;

   --------------------
   -- Task_Unlock_NT --
   --------------------

   procedure Task_Unlock_NT is
   begin
      null;
   end Task_Unlock_NT;
end System.Soft_Links;
