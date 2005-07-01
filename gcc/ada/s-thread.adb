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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a dummy version of this package.

with Unchecked_Conversion;

with System.Threads.Initialization;

package body System.Threads is

   -----------------------
   -- Get_Current_Excep --
   -----------------------

   function Get_Current_Excep return EOA is
   begin
      return null;
   end Get_Current_Excep;

   ------------------------
   -- Get_Jmpbuf_Address --
   ------------------------

   function  Get_Jmpbuf_Address return  Address is
   begin
      return Null_Address;
   end Get_Jmpbuf_Address;

   ------------------------
   -- Get_Sec_Stack_Addr --
   ------------------------

   function  Get_Sec_Stack_Addr return  Address is
   begin
      return Null_Address;
   end Get_Sec_Stack_Addr;

   ------------------------
   -- Set_Jmpbuf_Address --
   ------------------------

   procedure Set_Jmpbuf_Address (Addr : Address) is
      pragma Unreferenced (Addr);
   begin
      null;
   end Set_Jmpbuf_Address;

   ------------------------
   -- Set_Sec_Stack_Addr --
   ------------------------

   procedure Set_Sec_Stack_Addr (Addr : Address) is
      pragma Unreferenced (Addr);
   begin
      null;
   end Set_Sec_Stack_Addr;

   -----------------------
   -- Thread_Body_Enter --
   -----------------------

   procedure Thread_Body_Enter
     (Sec_Stack_Address    : System.Address;
      Sec_Stack_Size       : Natural;
      Process_ATSD_Address : System.Address)
   is
      pragma Unreferenced (Sec_Stack_Address);
      pragma Unreferenced (Sec_Stack_Size);
      pragma Unreferenced (Process_ATSD_Address);
   begin
      null;
   end Thread_Body_Enter;

   ----------------------------------
   -- Thread_Body_Exceptional_Exit --
   ----------------------------------

   procedure Thread_Body_Exceptional_Exit
     (EO : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (EO);
   begin
      null;
   end Thread_Body_Exceptional_Exit;

   -----------------------
   -- Thread_Body_Leave --
   -----------------------

   procedure Thread_Body_Leave is
   begin
      null;
   end Thread_Body_Leave;

end System.Threads;
