------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ C O U N T E R S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
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

--  This package implements Atomic_Counter operatiobns for platforms where
--  GCC supports __sync_add_and_fetch_4 and __sync_sub_and_fetch_4 builtins.

package body System.Atomic_Counters is

   procedure Sync_Add_And_Fetch
     (Ptr   : access Unsigned_32;
      Value : Unsigned_32);
   pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_4");

   function Sync_Sub_And_Fetch
     (Ptr   : access Unsigned_32;
      Value : Unsigned_32) return Unsigned_32;
   pragma Import (Intrinsic, Sync_Sub_And_Fetch, "__sync_sub_and_fetch_4");

   ---------------
   -- Decrement --
   ---------------

   function Decrement (Item : in out Atomic_Counter) return Boolean is
   begin
      --  Note: the use of Unrestricted_Access here is required because we
      --  are obtaining an access-to-volatile pointer to a non-volatile object.
      --  This is not allowed for [Unchecked_]Access, but is safe in this case
      --  because we know that no aliases are being created.

      return Sync_Sub_And_Fetch (Item.Value'Unrestricted_Access, 1) = 0;
   end Decrement;

   ---------------
   -- Increment --
   ---------------

   procedure Increment (Item : in out Atomic_Counter) is
   begin
      --  Note: the use of Unrestricted_Access here is required because we
      --  are obtaining an access-to-volatile pointer to a non-volatile object.
      --  This is not allowed for [Unchecked_]Access, but is safe in this case
      --  because we know that no aliases are being created.

      Sync_Add_And_Fetch (Item.Value'Unrestricted_Access, 1);
   end Increment;

   ------------
   -- Is_One --
   ------------

   function Is_One (Item : Atomic_Counter) return Boolean is
   begin
      return Item.Value = 1;
   end Is_One;

end System.Atomic_Counters;
