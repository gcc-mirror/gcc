------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNT-TIME COMPONENTS                        --
--                                                                          --
--                  S Y S T E M . A S T _ H A N D L I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the dummy version used on non-VMS systems

with Ada.Exceptions;
with Ada.Task_Identification;
with System.Aux_DEC;

package body System.AST_Handling is

   pragma Warnings (Off); -- kill warnings on unreferenced formals

   ------------------------
   -- Create_AST_Handler --
   ------------------------

   function Create_AST_Handler
     (Taskid  : Ada.Task_Identification.Task_Id;
      Entryno : Natural)
      return    System.Aux_DEC.AST_Handler
   is
   begin
      Ada.Exceptions.Raise_Exception
        (E       => Program_Error'Identity,
         Message => "AST is implemented only on VMS systems");

      return System.Aux_DEC.No_AST_Handler;
   end Create_AST_Handler;

   procedure Expand_AST_Packet_Pool
     (Requested_Packets : in Natural;
      Actual_Number     : out Natural;
      Total_Number      : out Natural)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (E       => Program_Error'Identity,
         Message => "AST is implemented only on VMS systems");

      Actual_Number := 0;
      Total_Number := 0;
   end Expand_AST_Packet_Pool;

end System.AST_Handling;
