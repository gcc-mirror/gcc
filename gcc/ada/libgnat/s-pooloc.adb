------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P O O L _ L O C A L                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

with System.Memory;

with Ada.Unchecked_Conversion;

package body System.Pool_Local is

   package SSE renames System.Storage_Elements;
   use type SSE.Storage_Offset;

   Pointer_Size  : constant SSE.Storage_Offset := Address'Size / Storage_Unit;
   Pointers_Size : constant SSE.Storage_Offset := 2 * Pointer_Size;

   type Acc_Address is access all Address;
   function To_Acc_Address is
     new Ada.Unchecked_Conversion (Address, Acc_Address);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Next (A : Address) return Acc_Address;
   pragma Inline (Next);
   --  Given an address of a block, return an access to the next block

   function Prev (A : Address) return Acc_Address;
   pragma Inline (Prev);
   --  Given an address of a block, return an access to the previous block

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool         : in out Unbounded_Reclaim_Pool;
      Address      : out System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is
      pragma Warnings (Off, Alignment);

      Allocated : constant System.Address :=
                    Memory.Alloc
                      (Memory.size_t (Storage_Size + Pointers_Size));

   begin
      --  The call to Alloc returns an address whose alignment is compatible
      --  with the worst case alignment requirement for the machine; thus the
      --  Alignment argument can be safely ignored.

      if Allocated = Null_Address then
         raise Storage_Error;
      else
         Address := Allocated + Pointers_Size;
         Next (Allocated).all := Pool.First;
         Prev (Allocated).all := Null_Address;

         if Pool.First /= Null_Address then
            Prev (Pool.First).all := Allocated;
         end if;

         Pool.First := Allocated;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool         : in out Unbounded_Reclaim_Pool;
      Address      : System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is
      pragma Warnings (Off, Storage_Size);
      pragma Warnings (Off, Alignment);

      Allocated : constant System.Address := Address - Pointers_Size;

   begin
      if Prev (Allocated).all = Null_Address then
         Pool.First := Next (Allocated).all;

         --  Comment needed

         if Pool.First /= Null_Address then
            Prev (Pool.First).all := Null_Address;
         end if;
      else
         Next (Prev (Allocated).all).all := Next (Allocated).all;
      end if;

      if Next (Allocated).all /= Null_Address then
         Prev (Next (Allocated).all).all := Prev (Allocated).all;
      end if;

      Memory.Free (Allocated);
   end Deallocate;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Pool : in out Unbounded_Reclaim_Pool) is
      N         : System.Address := Pool.First;
      Allocated : System.Address;

   begin
      while N /= Null_Address loop
         Allocated := N;
         N := Next (N).all;
         Memory.Free (Allocated);
      end loop;
   end Finalize;

   ----------
   -- Next --
   ----------

   function Next (A : Address) return Acc_Address is
   begin
      return To_Acc_Address (A);
   end Next;

   ----------
   -- Prev --
   ----------

   function Prev (A : Address) return Acc_Address is
   begin
      return To_Acc_Address (A + Pointer_Size);
   end Prev;

end System.Pool_Local;
