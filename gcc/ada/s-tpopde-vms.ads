------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  SYSTEM.TASK_PRIMITIVES.OPERATIONS.DEC                   --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2000-2009, Free Software Foundation, Inc.          --
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

--  This package is for OpenVMS/Alpha.
--
with System.Aux_DEC;
package System.Task_Primitives.Operations.DEC is

   procedure Interrupt_AST_Handler (ID : Address);
   pragma Convention (C, Interrupt_AST_Handler);
   --  Handles the AST for Ada95 Interrupts

   procedure RMS_AST_Handler (ID : Address);
   --  Handles the AST for RMS_Asynch_Operations

   function Self return System.Aux_DEC.Unsigned_Longword;
   --  Returns the task identification for the AST

   procedure Starlet_AST_Handler (ID : Address);
   --  Handles the AST for Starlet Tasking_Services

   procedure Task_Synch;
   --  Synchronizes the task after the system service completes

end System.Task_Primitives.Operations.DEC;
