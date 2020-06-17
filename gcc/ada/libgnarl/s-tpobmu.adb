------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S .    --
--                     M U L T I P R O C E S S O R S                        --
--                               B o d y                                    --
--                                                                          --
--                       Copyright (C) 2010-2020, AdaCore                   --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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

package body System.Tasking.Protected_Objects.Multiprocessors is

   ------------
   -- Served --
   ------------

   procedure Served (Entry_Call : Entry_Call_Link) is
      pragma Unreferenced (Entry_Call);
   begin
      pragma Assert (False, "Invalid operation");
   end Served;

   -------------------------
   -- Wakeup_Served_Entry --
   -------------------------

   procedure Wakeup_Served_Entry is
   begin
      pragma Assert (False, "Invalid operation");
   end Wakeup_Served_Entry;

end System.Tasking.Protected_Objects.Multiprocessors;
