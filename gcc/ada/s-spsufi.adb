------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . S T O R A G E _ P O O L S . S U B P O O L S .       --
--                          F I N A L I Z A T I O N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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

package body System.Storage_Pools.Subpools.Finalization is

   -----------------------------
   -- Finalize_And_Deallocate --
   -----------------------------

   procedure Finalize_And_Deallocate (Subpool : in out Subpool_Handle) is
   begin
      --  Do nothing if the subpool was never created or never used. The latter
      --  case may arise with an array of subpool implementations.

      if Subpool = null
        or else Subpool.Owner = null
        or else Subpool.Node = null
      then
         return;
      end if;

      --  Clean up all controlled objects allocated through the subpool

      Finalize_Subpool (Subpool);

      --  Dispatch to the user-defined implementation of Deallocate_Subpool

      Deallocate_Subpool (Pool_Of_Subpool (Subpool).all, Subpool);

      Subpool := null;
   end Finalize_And_Deallocate;

end System.Storage_Pools.Subpools.Finalization;
