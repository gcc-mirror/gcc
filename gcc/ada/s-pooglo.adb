------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . P O O L _ G L O B A L                    --
--                                                                          --
--                                 B o d y                                  --
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

with System.Storage_Pools;    use System.Storage_Pools;
with System.Memory;

package body System.Pool_Global is

   package SSE renames System.Storage_Elements;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
      Address      : out System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is
      pragma Warnings (Off, Pool);
      pragma Warnings (Off, Alignment);

      Allocated : System.Address;

   begin
      Allocated := Memory.Alloc (Memory.size_t (Storage_Size));

      --  The call to Alloc returns an address whose alignment is compatible
      --  with the worst case alignment requirement for the machine; thus the
      --  Alignment argument can be safely ignored.

      if Allocated = Null_Address then
         raise Storage_Error;
      else
         Address := Allocated;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool         : in out Unbounded_No_Reclaim_Pool;
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

   function Storage_Size
     (Pool  : Unbounded_No_Reclaim_Pool)
      return  SSE.Storage_Count
   is
      pragma Warnings (Off, Pool);

   begin
      --  Intuitively, should return System.Memory_Size. But on Sun/Alsys,
      --  System.Memory_Size > System.Max_Int, which means all you can do with
      --  it is raise CONSTRAINT_ERROR...

      return SSE.Storage_Count'Last;
   end Storage_Size;

end System.Pool_Global;
