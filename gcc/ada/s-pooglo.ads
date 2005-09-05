------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . P O O L _ G L O B A L                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1992-2005 Free Software Foundation, Inc.         --
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

with System;
with System.Storage_Pools;
with System.Storage_Elements;

package System.Pool_Global is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   --  Allocation strategy:

   --    Call to malloc/free for each Allocate/Deallocate
   --    no user specifiable size
   --    no automatic reclaim
   --    minimal overhead

   --  Pool simulating the allocation/deallocation strategy used by the
   --  compiler for access types globally declared.

   type Unbounded_No_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with null record;

   function Storage_Size
     (Pool : Unbounded_No_Reclaim_Pool)
      return System.Storage_Elements.Storage_Count;

   procedure Allocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   --  Pool object used by the compiler when implicit Storage Pool objects are
   --  explicitly referred to. For instance when writing something like:
   --     for T'Storage_Pool use Q'Storage_Pool;
   --  and Q'Storage_Pool hasn't been defined explicitly.

   Global_Pool_Object : Unbounded_No_Reclaim_Pool;

end System.Pool_Global;
