------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . P O O L _ G L O B A L                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  Storage pool corresponding to default global storage pool used for types
--  for which no storage pool is specified.

with System.Storage_Pools;
with System.Storage_Elements;

package System.Pool_Global is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   --  Allocation strategy:

   --    Call to malloc/free for each Allocate/Deallocate
   --    No user specifiable size
   --    No automatic reclaim
   --    Minimal overhead

   --  Pool simulating the allocation/deallocation strategy used by the
   --  compiler for access types globally declared.

   type Unbounded_No_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with null record;

   overriding function Storage_Size
     (Pool : Unbounded_No_Reclaim_Pool)
      return System.Storage_Elements.Storage_Count;

   overriding procedure Allocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   --  Pool object used by the compiler when implicit Storage Pool objects are
   --  explicitly referred to. For instance when writing something like:
   --     for T'Storage_Pool use Q'Storage_Pool;
   --  and Q'Storage_Pool hasn't been defined explicitly.

   Global_Pool_Object : aliased Unbounded_No_Reclaim_Pool;

end System.Pool_Global;
