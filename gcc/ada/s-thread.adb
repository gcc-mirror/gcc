------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T H R E A D S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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

--  This is the VxWorks version of this package

pragma Restrictions (No_Tasking);
--  The VxWorks version of this package is intended only for programs
--  which do not use Ada tasking. This restriction ensures that this
--  will be checked by the binder.

with System.Secondary_Stack;

with Unchecked_Conversion;

with System.Threads.Initialization;

package body System.Threads is

   package SSS renames System.Secondary_Stack;

   Current_ATSD  : aliased System.Address := System.Null_Address;
   pragma Export (C, Current_ATSD, "__gnat_current_atsd");

   function From_Address is
      new Unchecked_Conversion (Address, ATSD_Access);

   procedure Init_Float;
   pragma Import (C, Init_Float, "__gnat_init_float");

   procedure Install_Handler;
   pragma Import (C, Install_Handler, "__gnat_install_handler");

   -----------------------
   -- Get_Current_Excep --
   -----------------------

   function Get_Current_Excep return EOA is
      CTSD : ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      return CTSD.Current_Excep'Access;
   end Get_Current_Excep;

   ------------------------
   -- Get_Jmpbuf_Address --
   ------------------------

   function  Get_Jmpbuf_Address return  Address is
      CTSD : ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      return CTSD.Jmpbuf_Address;
   end Get_Jmpbuf_Address;

   ------------------------
   -- Get_Sec_Stack_Addr --
   ------------------------

   function  Get_Sec_Stack_Addr return  Address is
      CTSD : ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      return CTSD.Sec_Stack_Addr;
   end Get_Sec_Stack_Addr;

   ------------------------
   -- Set_Jmpbuf_Address --
   ------------------------

   procedure Set_Jmpbuf_Address (Addr : Address) is
      CTSD : ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      CTSD.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address;

   ------------------------
   -- Set_Sec_Stack_Addr --
   ------------------------

   procedure Set_Sec_Stack_Addr (Addr : Address) is
      CTSD : ATSD_Access := From_Address (Current_ATSD);
   begin
      pragma Assert (Current_ATSD /= System.Null_Address);
      CTSD.Sec_Stack_Addr := Addr;
   end Set_Sec_Stack_Addr;

   -----------------------
   -- Thread_Body_Enter --
   -----------------------

   procedure Thread_Body_Enter
     (Sec_Stack_Address    : System.Address;
      Sec_Stack_Size       : Natural;
      Process_ATSD_Address : System.Address)
   is
      --  Current_ATSD must already be a taskVar of taskIdSelf.
      --  No assertion because taskVarGet is not available on VxWorks/CERT

      TSD : ATSD_Access := From_Address (Process_ATSD_Address);

   begin
      TSD.Sec_Stack_Addr := Sec_Stack_Address;
      SSS.SS_Init (TSD.Sec_Stack_Addr, Sec_Stack_Size);
      Current_ATSD := Process_ATSD_Address;

      Install_Handler;
      Init_Float;
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

begin
   System.Threads.Initialization.Init_RTS;
end System.Threads;
