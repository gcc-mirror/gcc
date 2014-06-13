------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                S Y S T E M . E X C E P T I O N S _ D E B U G             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2014, Free Software Foundation, Inc.         --
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

--  This package contains internal routines used as debugger helpers.
--  It should be compiled without optimization to let debuggers inspect
--  parameter values reliably from breakpoints on the routines.

pragma Compiler_Unit_Warning;

with System.Standard_Library;

package System.Exceptions_Debug is

   pragma Preelaborate;
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
end System.Exceptions_Debug;
