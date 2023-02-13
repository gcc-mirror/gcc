------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         S Y S T E M . E L A B O R A T I O N _ A L L O C A T O R S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2014-2023, Free Software Foundation, Inc.       --
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

package body System.Elaboration_Allocators is

   Elaboration_In_Progress : Boolean;
   pragma Atomic (Elaboration_In_Progress);
   --  Flag to show if elaboration is active. We don't attempt to initialize
   --  this because we want to be sure it gets reset if we are in a multiple
   --  elaboration situation of some kind. Make it atomic to prevent race
   --  conditions of any kind (not clearly necessary, but harmless!)

   ------------------------------
   -- Check_Standard_Allocator --
   ------------------------------

   procedure Check_Standard_Allocator is
   begin
      if not Elaboration_In_Progress then
         raise Storage_Error with
           "standard allocator after elaboration is complete is not allowed "
           & "(No_Standard_Allocators_After_Elaboration restriction active)";
      end if;
   end Check_Standard_Allocator;

   -----------------------------
   -- Mark_End_Of_Elaboration --
   -----------------------------

   procedure Mark_End_Of_Elaboration is
   begin
      Elaboration_In_Progress := False;
   end Mark_End_Of_Elaboration;

   -------------------------------
   -- Mark_Start_Of_Elaboration --
   -------------------------------

   procedure Mark_Start_Of_Elaboration is
   begin
      Elaboration_In_Progress := True;
   end Mark_Start_Of_Elaboration;

end System.Elaboration_Allocators;
