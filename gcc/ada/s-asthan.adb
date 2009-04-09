------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNT-TIME COMPONENTS                        --
--                                                                          --
--                  S Y S T E M . A S T _ H A N D L I N G                   --
--                                                                          --
--                                 B o d y                                  --
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

--  This is the dummy version used on non-VMS systems

package body System.AST_Handling is

   ------------------------
   -- Create_AST_Handler --
   ------------------------

   function Create_AST_Handler
     (Taskid  : Ada.Task_Identification.Task_Id;
      Entryno : Natural) return System.Aux_DEC.AST_Handler
   is
   begin
      raise Program_Error with "AST is implemented only on VMS systems";
      return System.Aux_DEC.No_AST_Handler;
   end Create_AST_Handler;

   procedure Expand_AST_Packet_Pool
     (Requested_Packets : Natural;
      Actual_Number     : out Natural;
      Total_Number      : out Natural)
   is
   begin
      raise Program_Error with "AST is implemented only on VMS systems";
   end Expand_AST_Packet_Pool;

end System.AST_Handling;
