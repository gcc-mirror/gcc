------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     SYSTEM.MACHINE_STATE_OPERATIONS                      --
--                                                                          --
--                                 B o d y                                  --
--                             (Dummy version)                              --
--                                                                          --
--          Copyright (C) 1999-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This dummy version of System.Machine_State_Operations is used
--  on targets for which zero cost exception handling is not implemented.

package body System.Machine_State_Operations is

   --  Turn off warnings since many unused parameters

   pragma Warnings (Off);

   use System.Exceptions;

   ----------------------------
   -- Allocate_Machine_State --
   ----------------------------

   function Allocate_Machine_State return Machine_State is
   begin
      return Machine_State (Null_Address);
   end Allocate_Machine_State;

   -------------------
   -- Enter_Handler --
   -------------------

   procedure Enter_Handler (M : Machine_State; Handler : Handler_Loc) is
   begin
      null;
   end Enter_Handler;

   ----------------
   -- Fetch_Code --
   ----------------

   function Fetch_Code (Loc : Code_Loc) return Code_Loc is
   begin
      return Loc;
   end Fetch_Code;

   ------------------------
   -- Free_Machine_State --
   ------------------------

   procedure Free_Machine_State (M : in out Machine_State) is
   begin
      M := Machine_State (Null_Address);
   end Free_Machine_State;

   ------------------
   -- Get_Code_Loc --
   ------------------

   function Get_Code_Loc (M : Machine_State) return Code_Loc is
   begin
      return Null_Address;
   end Get_Code_Loc;

   --------------------------
   -- Machine_State_Length --
   --------------------------

   function Machine_State_Length
     return System.Storage_Elements.Storage_Offset is
   begin
      return 0;
   end Machine_State_Length;

   ---------------
   -- Pop_Frame --
   ---------------

   procedure Pop_Frame
     (M    : Machine_State;
      Info : Subprogram_Info_Type) is
   begin
      null;
   end Pop_Frame;

   -----------------------
   -- Set_Machine_State --
   -----------------------

   procedure Set_Machine_State (M : Machine_State) is
   begin
      null;
   end Set_Machine_State;

   ------------------------------
   -- Set_Signal_Machine_State --
   ------------------------------

   procedure Set_Signal_Machine_State
     (M       : Machine_State;
      Context : System.Address)
   is
   begin
      null;
   end Set_Signal_Machine_State;

end System.Machine_State_Operations;
