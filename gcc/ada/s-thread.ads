------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T H R E A D S                        --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides facilities to register a thread to the runtime,
--  and allocate its task specific datas.

with Ada.Exceptions;

package System.Threads is

   subtype EO is Ada.Exceptions.Exception_Occurrence;

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;

   type ATSD is limited private;
   --  Type of the Ada thread specific data. It contains datas needed
   --  by the GNAT runtime.

   type ATSD_Access is access ATSD;

   --  Get/Set for the attributes of the current thread.

   function  Get_Jmpbuf_Address return  Address;
   pragma Inline (Get_Jmpbuf_Address);

   procedure Set_Jmpbuf_Address (Addr : Address);
   pragma Inline (Get_Jmpbuf_Address);

   function  Get_Sec_Stack_Addr return  Address;
   pragma Inline (Get_Sec_Stack_Addr);

   procedure Set_Sec_Stack_Addr (Addr : Address);
   pragma Inline (Set_Sec_Stack_Addr);

   function Get_Current_Excep return EOA;
   pragma Inline (Get_Current_Excep);

private

   ------------------------
   -- Task Specific Data --
   ------------------------

   type ATSD is limited record
      Jmpbuf_Address : Address := Null_Address;
      --  Address of jump buffer used to store the address of the
      --  current longjmp/setjmp buffer for exception management.
      --  These buffers are threaded into a stack, and the address
      --  here is the top of the stack. A null address means that
      --  no exception handler is currently active.

      Sec_Stack_Addr : Address := Null_Address;
      --  Address of currently allocated secondary stack

      Current_Excep : aliased EO;
      --  Exception occurrence that contains the information for the
      --  current exception. Note that any exception in the same task
      --  destroys this information, so the data in this variable must
      --  be copied out before another exception can occur.

   end record;

end System.Threads;
