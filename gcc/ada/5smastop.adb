------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     SYSTEM.MACHINE_STATE_OPERATIONS                      --
--                                                                          --
--                                 B o d y                                  --
--            (Version using the GCC stack unwinding mechanism)             --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--           Copyright (C) 1999-2001 Ada Core Technologies, Inc.            --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This version of System.Machine_State_Operations is for use on
--  systems where the GCC stack unwinding mechanism is supported.
--  It is currently only used on Solaris

package body System.Machine_State_Operations is

   use System.Storage_Elements;
   use System.Exceptions;

   ----------------------------
   -- Allocate_Machine_State --
   ----------------------------

   function Allocate_Machine_State return Machine_State is
      function Machine_State_Length return Storage_Offset;
      pragma Import (C, Machine_State_Length, "__gnat_machine_state_length");

      function Gnat_Malloc (Size : Storage_Offset) return Machine_State;
      pragma Import (C, Gnat_Malloc, "__gnat_malloc");

   begin
      return Gnat_Malloc (Machine_State_Length);
   end Allocate_Machine_State;

   -------------------
   -- Enter_Handler --
   -------------------

   procedure Enter_Handler (M : Machine_State; Handler : Handler_Loc) is
      procedure c_enter_handler (m : Machine_State; handler : Handler_Loc);
      pragma Import (C, c_enter_handler, "__gnat_enter_handler");

   begin
      c_enter_handler (M, Handler);
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
      procedure Gnat_Free (M : in Machine_State);
      pragma Import (C, Gnat_Free, "__gnat_free");

   begin
      Gnat_Free (M);
      M := Machine_State (Null_Address);
   end Free_Machine_State;

   ------------------
   -- Get_Code_Loc --
   ------------------

   function Get_Code_Loc (M : Machine_State) return Code_Loc is
      function c_get_code_loc (m : Machine_State) return Code_Loc;
      pragma Import (C, c_get_code_loc, "__gnat_get_code_loc");

   begin
      return c_get_code_loc (M);
   end Get_Code_Loc;

   --------------------------
   -- Machine_State_Length --
   --------------------------

   function Machine_State_Length return Storage_Offset is

      function c_machine_state_length return Storage_Offset;
      pragma Import (C, c_machine_state_length, "__gnat_machine_state_length");

   begin
      return c_machine_state_length;
   end Machine_State_Length;

   ---------------
   -- Pop_Frame --
   ---------------

   procedure Pop_Frame
     (M    : Machine_State;
      Info : Subprogram_Info_Type)
   is
      procedure c_pop_frame (m : Machine_State);
      pragma Import (C, c_pop_frame, "__gnat_pop_frame");

   begin
      c_pop_frame (M);
   end Pop_Frame;

   -----------------------
   -- Set_Machine_State --
   -----------------------

   procedure Set_Machine_State (M : Machine_State) is
      procedure c_set_machine_state (m : Machine_State);
      pragma Import (C, c_set_machine_state, "__gnat_set_machine_state");

   begin
      c_set_machine_state (M);
      Pop_Frame (M, System.Null_Address);
   end Set_Machine_State;

   ------------------------------
   -- Set_Signal_Machine_State --
   ------------------------------

   procedure Set_Signal_Machine_State
     (M       : Machine_State;
      Context : System.Address) is
   begin
      null;
   end Set_Signal_Machine_State;

end System.Machine_State_Operations;
