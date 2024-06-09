------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T A S K _ I N F O                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2024, Free Software Foundation, Inc.          --
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

--  This is the Windows (native) version of this module

with System.OS_Interface;
pragma Unreferenced (System.OS_Interface);
--  System.OS_Interface is not used today, but the protocol between the
--  run-time and the binder is that any tasking application uses
--  System.OS_Interface, so notify the binder with this "with" clause.

package body System.Task_Info is

   N_CPU : Natural := 0;
   pragma Atomic (N_CPU);
   --  Cache CPU number. Use pragma Atomic to avoid a race condition when
   --  setting N_CPU in Number_Of_Processors below.

   --------------------------
   -- Number_Of_Processors --
   --------------------------

   function Number_Of_Processors return Positive is
   begin
      if N_CPU = 0 then
         declare
            SI : aliased Win32.SYSTEM_INFO;
         begin
            Win32.GetSystemInfo (SI'Access);
            N_CPU := Positive (SI.dwNumberOfProcessors);
         end;
      end if;

      return N_CPU;
   end Number_Of_Processors;

end System.Task_Info;
