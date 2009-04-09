------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     A D A . F I N A L I Z A T I O N . L I S T _ C O N T R O L L E R      --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

with System.Finalization_Implementation;
package body Ada.Finalization.List_Controller is

   package SFI renames System.Finalization_Implementation;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out List_Controller) is
      use type SFR.Finalizable_Ptr;

      Last_Ptr : constant SFR.Finalizable_Ptr := Object.Last'Unchecked_Access;

   begin
      --  First take note of the fact that finalization of this collection has
      --  started.

      Object.F := SFI.Collection_Finalization_Started;

      --  Then finalize all the objects. Note that finalization can call
      --  Unchecked_Deallocation on other objects in the same collection,
      --  which will cause them to be removed from the list if we have not
      --  gotten to them yet. However, allocation in the collection will raise
      --  Program_Error, due to the above Collection_Finalization_Started.

      while Object.First.Next /= Last_Ptr loop
         SFI.Finalize_One (Object.First.Next.all);
      end loop;
   end Finalize;

   procedure Finalize (Object : in out Simple_List_Controller) is
   begin
      SFI.Finalize_List (Object.F);
      Object.F := null;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out List_Controller) is
   begin
      Object.F          := Object.First'Unchecked_Access;
      Object.First.Next := Object.Last 'Unchecked_Access;
      Object.Last.Prev  := Object.First'Unchecked_Access;
   end Initialize;

end Ada.Finalization.List_Controller;
