------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . A S T _ H A N D L I N G                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2009, Free Software Foundation, Inc.         --
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

--  Runtime support for Handling of AST's (Used on VMS implementations only)

with Ada.Task_Identification;
with System;
with System.Aux_DEC;

package System.AST_Handling is

   function Create_AST_Handler
     (Taskid  : Ada.Task_Identification.Task_Id;
      Entryno : Natural) return System.Aux_DEC.AST_Handler;
   --  This function implements the appropriate semantics for a use of the
   --  AST_Entry pragma. See body for details of implementation approach.
   --  The parameters are the Task_Id for the task containing the entry
   --  and the entry Index for the specified entry.

   procedure Expand_AST_Packet_Pool
     (Requested_Packets : Natural;
      Actual_Number     : out Natural;
      Total_Number      : out Natural);
   --  This function takes a request for zero or more extra AST packets and
   --  returns the number actually added to the pool and the total number
   --  now available or in use.
   --  This function is not yet fully implemented.

end System.AST_Handling;
