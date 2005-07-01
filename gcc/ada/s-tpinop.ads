------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               S Y S T E M . T A S K _ P R I M I T I V E S .              --
--                  I N T E R R U P T _ O P E R A T I O N S                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--           Copyright (C) 1998-2005 Free Software Foundation, Inc.         --
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

with System.Interrupt_Management;
with System.Tasking;

package System.Task_Primitives.Interrupt_Operations is

   package IM renames System.Interrupt_Management;
   package ST renames System.Tasking;

   procedure Set_Interrupt_ID (Interrupt : IM.Interrupt_ID; T : ST.Task_Id);
   --  Associate an Interrupt_ID with a task.

   function Get_Interrupt_ID (T : ST.Task_Id) return IM.Interrupt_ID;
   --  Return the Interrupt_ID associated with a task.

   function Get_Task_Id (Interrupt : IM.Interrupt_ID) return ST.Task_Id;
   --  Return the Task_Id associated with an Interrupt.

end System.Task_Primitives.Interrupt_Operations;
