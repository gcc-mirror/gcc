------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . P O O L _ 3 2 _ G L O B A L                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

with System.Storage_Pools; use System.Storage_Pools;
with System.Memory;

package body System.Pool_32_Global is

   package SSE renames System.Storage_Elements;

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool         : in out Unbounded_No_Reclaim_Pool_32;
      Address      : out System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is
      pragma Warnings (Off, Pool);
      pragma Warnings (Off, Alignment);

   begin
      Address := Memory.Alloc32 (Memory.size_t (Storage_Size));

      --  The call to Alloc returns an address whose alignment is compatible
      --  with the worst case alignment requirement for the machine; thus the
      --  Alignment argument can be safely ignored.

      if Address = Null_Address then
         raise Storage_Error;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   overriding procedure Deallocate
     (Pool         : in out Unbounded_No_Reclaim_Pool_32;
      Address      : System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is
      pragma Warnings (Off, Pool);
      pragma Warnings (Off, Storage_Size);
      pragma Warnings (Off, Alignment);

   begin
      Memory.Free (Address);
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   overriding function Storage_Size
     (Pool  : Unbounded_No_Reclaim_Pool_32)
      return  SSE.Storage_Count
   is
      pragma Warnings (Off, Pool);

   begin
      --  The 32 bit heap is limited to 2 GB of memory

      return SSE.Storage_Count (2 ** 31);
   end Storage_Size;

end System.Pool_32_Global;
