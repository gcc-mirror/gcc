------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . S T A C K _ C H E C K I N G . O P E R A T I O N S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1999-2008, Free Software Foundation, Inc.         --
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

--  This is the RTEMS version of this package.
--  This file should be kept synchronized with the general implementation
--  provided by s-stchop.adb.

pragma Restrictions (No_Elaboration_Code);
--  We want to guarantee the absence of elaboration code because the
--  binder does not handle references to this package.

with Ada.Exceptions;

with Interfaces.C; use Interfaces.C;

package body System.Stack_Checking.Operations is

   ----------------------------
   -- Invalidate_Stack_Cache --
   ----------------------------

   procedure Invalidate_Stack_Cache (Any_Stack : Stack_Access) is
      pragma Warnings (Off, Any_Stack);
   begin
      Cache := Null_Stack;
   end Invalidate_Stack_Cache;

   -----------------------------
   -- Notify_Stack_Attributes --
   -----------------------------

   procedure Notify_Stack_Attributes
     (Initial_SP : System.Address;
      Size       : System.Storage_Elements.Storage_Offset)
   is

      --  RTEMS keeps all the information we need.

      pragma Unreferenced (Size);
      pragma Unreferenced (Initial_SP);

   begin
      null;
   end Notify_Stack_Attributes;

   -----------------
   -- Stack_Check --
   -----------------

   function Stack_Check
     (Stack_Address : System.Address) return Stack_Access
   is
      pragma Unreferenced (Stack_Address);

      --  RTEMS has a routine to check this.  So use it.
      function rtems_stack_checker_is_blown return Interfaces.C.int;
      pragma Import (C,
         rtems_stack_checker_is_blown, "rtems_stack_checker_is_blown");

   begin
      --  RTEMS has a routine to check this.  So use it.

      if rtems_stack_checker_is_blown /= 0 then
         Ada.Exceptions.Raise_Exception
           (E       => Storage_Error'Identity,
            Message => "stack overflow detected");
      end if;

      return null;

   end Stack_Check;

   ------------------------
   -- Update_Stack_Cache --
   ------------------------

   procedure Update_Stack_Cache (Stack : Stack_Access) is
   begin
      if not Multi_Processor then
         Cache := Stack;
      end if;
   end Update_Stack_Cache;

end System.Stack_Checking.Operations;
