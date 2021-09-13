------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          S Y S T E M . S O F T _ L I N K S . I N I T I A L I Z E         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2017-2021, Free Software Foundation, Inc.       --
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

with System.Secondary_Stack;

package body System.Soft_Links.Initialize is

   package SSS renames System.Secondary_Stack;

begin
   --  Initialize the TSD of the main task

   NT_TSD.Jmpbuf_Address := System.Null_Address;

   --  Allocate and initialize the secondary stack for the main task

   NT_TSD.Sec_Stack_Ptr := null;
   SSS.SS_Init (NT_TSD.Sec_Stack_Ptr);
end System.Soft_Links.Initialize;
