------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       G N A T . D E B U G _ P O O L S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Checked_Pools;

package GNAT.Debug_Pools is

   --  The debug pool is used to track down memory corruption due to use of
   --  deallocated memory or incorrect unchecked conversions. Allocation
   --  strategy :

   --     - allocation:   . memory is normally allocated with malloc
   --                     . the allocated address is noted in a table

   --     - deallocation: . memory is  filled with "DEAD_BEEF" patterns
   --                     . memory is not freed
   --                     . exceptions are raised if the memory was not
   --                       allocated or was already deallocated

   --     - dereference:  . exceptions are raised if the memory was not
   --                        allocated or was already deallocated

   Accessing_Not_Allocated_Storage : exception;
   Accessing_Deallocated_Storage   : exception;
   Freeing_Not_Allocated_Storage   : exception;
   Freeing_Deallocated_Storage     : exception;

   type Debug_Pool is
     new System.Checked_Pools.Checked_Pool with private;

   procedure Allocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : out Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);

   procedure Deallocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);

   function Storage_Size
     (Pool : Debug_Pool)
      return System.Storage_Elements.Storage_Count;

   procedure Dereference
     (Pool                     : in out Debug_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);

   generic
      with procedure Put_Line (S : String);
   procedure Print_Info (Pool : Debug_Pool);
   --  Print out information about the High Water Mark, the current and
   --  total number of bytes allocated and the total number of bytes
   --  deallocated.

private
   type Debug_Pool is new System.Checked_Pools.Checked_Pool with record
      Allocated   : Storage_Count := 0;
      --  Total number of bytes allocated in this pool

      Deallocated : Storage_Count := 0;
      --  Total number of bytes deallocated in this pool

      High_Water  : Storage_Count := 0;
      --  Maximum of during the time of Allocated - Deallocated
   end record;
end GNAT.Debug_Pools;
