------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--    S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S     --
--                                 . D E C                                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1.16.1 $
--                                                                          --
--              Copyright (C) 2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------
--
--  This package is for OpenVMS/Alpha.
--
with System.Aux_DEC;
package System.Task_Primitives.Operations.DEC is

   procedure Interrupt_AST_Handler (ID : Address);
   --  Handles the AST for Ada95 Interrupts.

   procedure RMS_AST_Handler (ID : Address);
   --  Handles the AST for RMS_Asynch_Operations.

   function Self return System.Aux_DEC.Unsigned_Longword;
   --  Returns the task identification for the AST.

   procedure Starlet_AST_Handler (ID : Address);
   --  Handles the AST for Starlet Tasking_Services.

   procedure Task_Synch;
   --  Synchronizes the task after the system service completes.

end System.Task_Primitives.Operations.DEC;
