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

--  pragma Thread_Body is currently supported for:
--  VxWorks AE653 with the restricted / cert runtime

with Ada.Exceptions;
--  used for Exception_Occurrence

with System.Soft_Links;
--  used for TSD

package System.Threads is

   subtype EO is Ada.Exceptions.Exception_Occurrence;

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;

   type ATSD is limited private;
   --  Type of the Ada thread specific data. It contains datas needed
   --  by the GNAT runtime.

   type ATSD_Access is access ATSD;

   --  Get/Set for the attributes of the current thread

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

   --------------------------
   -- Thread Body Handling --
   --------------------------

   --  The subprograms in this section are called by the expansion of a
   --  subprogram body to which a Thread_Body pragma has been applied:

   --  Given a subprogram body

   --     procedure xyz (params ....) is    -- can also be a function
   --       <user declarations>
   --     begin
   --       <user statements>
   --     <user exception handlers>
   --     end xyz;

   --  The expansion resulting from use of the Thread_Body pragma is:

   --     procedure xyz (params ...) is

   --       _Secondary_Stack : aliased
   --          Storage_Elements.Storage_Array
   --            (1 .. Storage_Offset (Sec_Stack_Size));
   --       for _Secondary_Stack'Alignment use Standard'Maximum_Alignment;

   --       _Process_ATSD : aliased System.Threads.ATSD;

   --     begin
   --        System.Threads.Thread_Body_Enter;
   --          (_Secondary_Stack'Address,
   --           _Secondary_Stack'Length,
   --           _Process_ATSD'Address);

   --        declare
   --           <user declarations>
   --        begin
   --           <user statements>
   --        <user exception handlers>
   --        end;

   --       System.Threads.Thread_Body_Leave;

   --     exception
   --        when E : others =>
   --          System.Threads.Thread_Body_Exceptional_Exit (E);
   --     end;

   --  Note the exception handler is omitted if pragma Restriction
   --  No_Exception_Handlers is currently active.

   --  Note: the secondary stack size (Sec_Stack_Size) comes either from
   --  the pragma, if specified, or is the default value taken from
   --  the declaration in System.Secondary_Stack.

   procedure Thread_Body_Enter
     (Sec_Stack_Address    : System.Address;
      Sec_Stack_Size       : Natural;
      Process_ATSD_Address : System.Address);
   --  Enter thread body, see above for details

   procedure Thread_Body_Leave;
   --  Leave thread body (normally), see above for details

   procedure Thread_Body_Exceptional_Exit
     (EO : Ada.Exceptions.Exception_Occurrence);
   --  Leave thread body (abnormally on exception), see above for details

private

   type ATSD is new System.Soft_Links.TSD;

end System.Threads;
