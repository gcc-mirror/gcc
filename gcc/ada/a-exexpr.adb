------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--  A D A . E X C E P T I O N S . E X C E P T I O N _ P R O P A G A T I O N --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

--  This is the default version, using the __builtin_setjmp/longjmp EH
--  mechanism.

with Ada.Unchecked_Conversion;

separate (Ada.Exceptions)
package body Exception_Propagation is

   --  Common binding to __builtin_longjmp for sjlj variants.

   procedure builtin_longjmp (buffer : System.Address; Flag : Integer);
   pragma No_Return (builtin_longjmp);
   pragma Import (Intrinsic, builtin_longjmp, "__builtin_longjmp");

   procedure Propagate_Continue (E : Exception_Id);
   pragma No_Return (Propagate_Continue);
   pragma Export (C, Propagate_Continue, "__gnat_raise_nodefer_with_msg");
   --  A call to this procedure is inserted automatically by GIGI, in order
   --  to continue the propagation when the exception was not handled.
   --  The linkage name is historical.

   -------------------------
   -- Allocate_Occurrence --
   -------------------------

   function Allocate_Occurrence return EOA is
   begin
      return Get_Current_Excep.all;
   end Allocate_Occurrence;

   -------------------------
   -- Propagate_Exception --
   -------------------------

   procedure Propagate_Exception (Excep : EOA) is
      Jumpbuf_Ptr : constant Address := Get_Jmpbuf_Address.all;

   begin
      --  If the jump buffer pointer is non-null, transfer control using
      --  it. Otherwise announce an unhandled exception (note that this
      --  means that we have no finalizations to do other than at the outer
      --  level). Perform the necessary notification tasks in both cases.

      if Jumpbuf_Ptr /= Null_Address then
         if not Excep.Exception_Raised then
            Excep.Exception_Raised := True;
            Exception_Traces.Notify_Handled_Exception (Excep);
         end if;

         builtin_longjmp (Jumpbuf_Ptr, 1);

      else
         Exception_Traces.Notify_Unhandled_Exception (Excep);
         Exception_Traces.Unhandled_Exception_Terminate (Excep);
      end if;
   end Propagate_Exception;

   ------------------------
   -- Propagate_Continue --
   ------------------------

   procedure Propagate_Continue (E : Exception_Id) is
      pragma Unreferenced (E);
   begin
      Propagate_Exception (Get_Current_Excep.all);
   end Propagate_Continue;

end Exception_Propagation;
