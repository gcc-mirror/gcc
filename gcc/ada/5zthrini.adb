------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . T H R E A D S . I N I T I A L I Z A T I O N         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 1992-2003 Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the VxWorks version of this package; to use this implementation,
--  the task hook libraries should be included in the VxWorks kernel.

with System.Secondary_Stack;
with System.Storage_Elements;
with System.Soft_Links;
with Interfaces.C;

package body System.Threads.Initialization is

   use Interfaces.C;

   package SSS renames System.Secondary_Stack;

   package SSL renames System.Soft_Links;

   procedure Initialize_Task_Hooks;
   --  Register the appropriate hooks (Register and Reset_TSD) to the
   --  underlying OS, so that they will be called when a task is created
   --  or reset.

   Current_ATSD : aliased System.Address;
   pragma Import (C, Current_ATSD, "__gnat_current_atsd");

   ---------------------------
   -- Initialize_Task_Hooks --
   ---------------------------

   procedure Initialize_Task_Hooks is separate;
   --  Separate, as these hooks are different for AE653 and VxWorks 5.5.

   --------------
   -- Init_RTS --
   --------------

   procedure Init_RTS is
   begin
      SSL.Get_Jmpbuf_Address := Get_Jmpbuf_Address'Access;
      SSL.Get_Sec_Stack_Addr := Get_Sec_Stack_Addr'Access;
      SSL.Get_Current_Excep  := Get_Current_Excep'Access;
      SSL.Set_Jmpbuf_Address := Set_Jmpbuf_Address'Access;
      SSL.Set_Sec_Stack_Addr := Set_Sec_Stack_Addr'Access;
   end Init_RTS;

   --------------
   -- Register --
   --------------

   function Register (T : OSI.Thread_Id) return OSI.STATUS is
      Result : OSI.STATUS;
   begin
      --  It cannot be assumed that the caller of this routine has a ATSD;
      --  so neither this procedure nor the procedures that it calls should
      --  raise or handle exceptions, or make use of a secondary stack.

      --  This routine is only necessary because taskVarAdd cannot be
      --  executed once an AE653 partition has entered normal mode
      --  (depending on configRecord.c, allocation could be disabled).
      --  Otherwise, everything could have been done in Thread_Body_Enter.

      if OSI.taskIdVerify (T) = OSI.ERROR then
         return OSI.ERROR;
      end if;

      Result := OSI.taskVarAdd (T, Current_ATSD'Access);
      pragma Assert (Result /= OSI.ERROR);

      return Result;
   end Register;

   subtype Default_Sec_Stack is
     System.Storage_Elements.Storage_Array
       (1 .. SSS.Default_Secondary_Stack_Size);

   Main_Sec_Stack : aliased Default_Sec_Stack;

   --  Secondary stack for environment task

   Main_ATSD : aliased ATSD;

   --  TSD for environment task

begin
   Initialize_Task_Hooks;

   --  Register the environment task
   declare
      Result : Interfaces.C.int := Register (OSI.taskIdSelf);
      pragma Assert (Result /= OSI.ERROR);
   begin
      Thread_Body_Enter
        (Main_Sec_Stack'Address,
         Main_Sec_Stack'Size / System.Storage_Unit,
         Main_ATSD'Address);
   end;
end System.Threads.Initialization;
