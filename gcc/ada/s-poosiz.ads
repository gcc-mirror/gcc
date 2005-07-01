------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . P O O L _ S I Z E                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

with System.Storage_Pools;
with System.Storage_Elements;

package System.Pool_Size is

pragma Elaborate_Body;
--  Needed to ensure that library routines can execute allocators

   ------------------------
   -- Stack_Bounded_Pool --
   ------------------------

   --  Allocation strategy:

   --    Pool is a regular stack array, no use of malloc
   --    user specified size
   --    Space of pool is globally reclaimed by normal stack management

   --  Used in the compiler for access types with 'STORAGE_SIZE rep. clause
   --  Only used for allocating objects of the same type.

   type Stack_Bounded_Pool
     (Pool_Size : System.Storage_Elements.Storage_Count;
      Elmt_Size : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count)
   is
      new System.Storage_Pools.Root_Storage_Pool with record
         First_Free        : System.Storage_Elements.Storage_Count;
         First_Empty       : System.Storage_Elements.Storage_Count;
         Aligned_Elmt_Size : System.Storage_Elements.Storage_Count;
         The_Pool          : System.Storage_Elements.Storage_Array
                                                       (1 .. Pool_Size);
      end record;

   function Storage_Size
     (Pool : Stack_Bounded_Pool) return System.Storage_Elements.Storage_Count;

   procedure Allocate
     (Pool         : in out Stack_Bounded_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool         : in out Stack_Bounded_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   procedure Initialize (Pool : in out Stack_Bounded_Pool);

end System.Pool_Size;
