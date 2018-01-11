------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--               SYSTEM.TASK_PRIMITIVES.INTERRUPT_OPERATIONS                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1998-2018, Free Software Foundation, Inc.         --
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

with System.Interrupt_Management;
with System.Tasking;

package System.Task_Primitives.Interrupt_Operations is
   pragma Preelaborate;

   package IM renames System.Interrupt_Management;
   package ST renames System.Tasking;

   procedure Set_Interrupt_ID (Interrupt : IM.Interrupt_ID; T : ST.Task_Id);
   --  Associate an Interrupt_ID with a task

   function Get_Interrupt_ID (T : ST.Task_Id) return IM.Interrupt_ID;
   --  Return the Interrupt_ID associated with a task

   function Get_Task_Id (Interrupt : IM.Interrupt_ID) return ST.Task_Id;
   --  Return the Task_Id associated with an Interrupt

end System.Task_Primitives.Interrupt_Operations;
