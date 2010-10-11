------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . M U L T I P R O C E S S O R S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--           Copyright (C) 2010, Free Software Foundation, Inc.             --
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
------------------------------------------------------------------------------

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

package System.Multiprocessors is
   pragma Preelaborate (Multiprocessors);

   type CPU_Range is range 0 .. 2 ** 16 - 1;

   subtype CPU is CPU_Range range 1 .. CPU_Range'Last;

   Not_A_Specific_CPU : constant CPU_Range := 0;

   function Number_Of_CPUs return CPU;
   --  Number of available CPUs

end System.Multiprocessors;
