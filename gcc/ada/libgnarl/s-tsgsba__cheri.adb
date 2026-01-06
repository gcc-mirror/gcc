------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
-- S Y S T E M . T A S K I N G . S T A G E S . G E T _ S T A C K _ B A S E  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2026, Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------
with Interfaces.CHERI;

--  This is the version for CHERI targets where we can derive the stack base
--  from the upper bound of the capability stack pointer (CSP).

separate (System.Tasking.Stages)
function Get_Stack_Base (Self_ID : Task_Id) return System.Address is
   pragma Unreferenced (Self_ID);

   use type SSE.Integer_Address;

   CSP : constant System.Address := Interfaces.CHERI.Get_CSP;
begin
   return Interfaces.CHERI.Capability_With_Address
            (Cap  => CSP,
             Addr => Interfaces.CHERI.Get_Base (CSP) +
                       SSE.Integer_Address
                         (Interfaces.CHERI.Get_Length (CSP)));
end Get_Stack_Base;
