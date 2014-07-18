------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                SYSTEM.STORAGE_POOLS.SUBPOOLS.FINALIZATION                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2014, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Deallocation;

with System.Finalization_Masters; use System.Finalization_Masters;

package body System.Storage_Pools.Subpools.Finalization is

   -----------------------------
   -- Finalize_And_Deallocate --
   -----------------------------

   procedure Finalize_And_Deallocate (Subpool : in out Subpool_Handle) is
      procedure Free is new Ada.Unchecked_Deallocation (SP_Node, SP_Node_Ptr);

   begin
      --  Do nothing if the subpool was never created or never used. The latter
      --  case may arise with an array of subpool implementations.

      if Subpool = null
        or else Subpool.Owner = null
        or else Subpool.Node = null
      then
         return;
      end if;

      --  Clean up all controlled objects chained on the subpool's master

      Finalize (Subpool.Master);

      --  Remove the subpool from its owner's list of subpools

      Detach (Subpool.Node);

      --  Destroy the associated doubly linked list node which was created in
      --  Set_Pool_Of_Subpools.

      Free (Subpool.Node);

      --  Dispatch to the user-defined implementation of Deallocate_Subpool. It
      --  is important to first set Subpool.Owner to null, because RM-13.11.5
      --  requires that "The subpool no longer belongs to any pool" BEFORE
      --  calling Deallocate_Subpool. The actual dispatching call required is:
      --
      --     Deallocate_Subpool(Pool_of_Subpool(Subpool).all, Subpool);
      --
      --  but that can't be taken literally, because Pool_of_Subpool will
      --  return null.

      declare
         Owner : constant Any_Storage_Pool_With_Subpools_Ptr := Subpool.Owner;
      begin
         Subpool.Owner := null;
         Deallocate_Subpool (Owner.all, Subpool);
      end;

      Subpool := null;
   end Finalize_And_Deallocate;

end System.Storage_Pools.Subpools.Finalization;
