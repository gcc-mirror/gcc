------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T H R E A D S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This package provides facilities to register a thread to the runtime,
--  and allocate its task specific datas.

--  This package is currently implemented for:

--    VxWorks AE653 rts-cert
--    VxWorks AE653 rts-full (not rts-kernel)

with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with Interfaces.C;

with System.Secondary_Stack;
with System.Soft_Links;

package System.Threads is

   package SST renames System.Secondary_Stack;

   type ATSD is limited private;
   --  Type of the Ada thread specific data. It contains datas needed
   --  by the GNAT runtime.

   type ATSD_Access is access ATSD;
   function From_Address is
     new Ada.Unchecked_Conversion (Address, ATSD_Access);

   subtype STATUS is Interfaces.C.int;
   --  Equivalent of the C type STATUS

   type t_id is new Interfaces.C.long;
   subtype Thread_Id is t_id;

   function Register (T : Thread_Id) return STATUS;
   --  Create the task specific data necessary for Ada language support

   --------------------------
   -- Thread Body Handling --
   --------------------------

   --  The subprograms in this section are called from the process body
   --  wrapper in the APEX process registration package.

   procedure Thread_Body_Enter
     (Sec_Stack_Ptr        : SST.SS_Stack_Ptr;
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
