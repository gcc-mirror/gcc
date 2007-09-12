------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . E X C E P T I O N S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2007, Free Software Foundation, Inc.         --
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

--  This package contains internal routines used as debugger helpers.
--  It should be compiled without optimization to let debuggers inspect
--  parameter values reliably from breakpoints on the routines.

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

with System.Standard_Library;

package System.Exceptions is

   pragma Warnings (Off);
   pragma Preelaborate_05;
   pragma Warnings (On);
   --  To let Ada.Exceptions "with" us and let us "with" Standard_Library

   package SSL renames System.Standard_Library;
   --  To let some of the hooks below have formal parameters typed in
   --  accordance with what GDB expects.

   procedure Debug_Raise_Exception (E : SSL.Exception_Data_Ptr);
   pragma Export
     (Ada, Debug_Raise_Exception, "__gnat_debug_raise_exception");
   --  Hook called at a "raise" point for an exception E, when it is
   --  just about to be propagated.

   procedure Debug_Unhandled_Exception (E : SSL.Exception_Data_Ptr);
   pragma Export
     (Ada, Debug_Unhandled_Exception, "__gnat_unhandled_exception");
   --  Hook called during the propagation process of an exception E, as soon
   --  as it is known to be unhandled.

   procedure Debug_Raise_Assert_Failure;
   pragma Export
     (Ada, Debug_Raise_Assert_Failure, "__gnat_debug_raise_assert_failure");
   --  Hook called when an assertion failed. This is used by the debugger to
   --  intercept assertion failures, and treat them specially.

   procedure Local_Raise (Excep : System.Address);
   pragma Export (Ada, Local_Raise);
   --  This is a dummy routine, used only by the debugger for the purpose of
   --  logging local raise statements that were transformed into a direct goto
   --  to the handler code. The compiler in this case generates:
   --
   --    Local_Raise (exception_data'address);
   --    goto Handler
   --
   --  The argument is the address of the exception data

end System.Exceptions;
