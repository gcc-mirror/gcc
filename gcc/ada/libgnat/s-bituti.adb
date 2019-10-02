------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . B I T F I E L D _ U T I L S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2019, Free Software Foundation, Inc.         --
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

package body System.Bitfield_Utils is

   --  ???
   --
   --  This code does not yet work for overlapping bit fields. We need to copy
   --  backwards in some cases (i.e. from higher to lower bit addresses).
   --  Alternatively, we could avoid calling this if Forwards_OK is False.
   --
   --  ???

   package body G is

      Val_Bytes : constant Address := Address (Val'Size / Storage_Unit);

      --  A Val_2 can cross a memory page boundary (e.g. an 8-byte Val_2 that
      --  starts 4 bytes before the end of a page). If the bit field also
      --  crosses that boundary, then the second page is known to exist, and we
      --  can safely load or store the Val_2. On the other hand, if the bit
      --  field is entirely within the first half of the Val_2, then it is
      --  possible (albeit highly unlikely) that the second page does not
      --  exist, so we must load or store only the first half of the Val_2.
      --  Get_Val_2 and Set_Val_2 take care of all this.

      function Get_Val_2
        (Src_Address : Address;
         Src_Offset : Bit_Offset;
         Size : Small_Size)
        return Val_2;
      --  Get the Val_2, taking care to only load the first half when
      --  necessary.

      procedure Set_Val_2
        (Dest_Address : Address;
         Dest_Offset : Bit_Offset;
         V : Val_2;
         Size : Small_Size);
      --  Set the Val_2, taking care to only store the first half when
      --  necessary.

      --  Get_Bitfield and Set_Bitfield are helper functions that get/set small
      --  bit fields -- the value fits in Val, and the bit field is placed
      --  starting at some offset within the first half of a Val_2.
      --  Copy_Bitfield, on the other hand, supports arbitrarily large bit
      --  fields. All operations require bit offsets to point within the first
      --  Val pointed to by the address.

      function Get_Bitfield
        (Src : Val_2; Src_Offset : Bit_Offset; Size : Small_Size)
         return Val;
      --  Returns the bit field in Src starting at Src_Offset, of the given
      --  Size. If Size < Small_Size'Last, then high order bits are zero.

      function Set_Bitfield
        (Src_Value : Val;
         Dest : Val_2;
         Dest_Offset : Bit_Offset;
         Size : Small_Size)
        return Val_2;
      --  The bit field in Dest starting at Dest_Offset, of the given Size, is
      --  set to Src_Value. Src_Value must have high order bits (Size and
      --  above) zero. The result is returned as the function result.

      procedure Set_Bitfield
        (Src_Value : Val;
         Dest_Address : Address;
         Dest_Offset : Bit_Offset;
         Size : Small_Size);
      --  This version takes the bit address and size of the destination.

      procedure Copy_Small_Bitfield
        (Src_Address  : Address;
         Src_Offset   : Bit_Offset;
         Dest_Address : Address;
         Dest_Offset  : Bit_Offset;
         Size         : Small_Size);
      --  Copy_Bitfield in the case where Size <= Val'Size.
      --  The Address values must be aligned as for Val and Val_2.
      --  This works for overlapping bit fields.

      procedure Copy_Large_Bitfield
        (Src_Address  : Address;
         Src_Offset   : Bit_Offset;
         Dest_Address : Address;
         Dest_Offset  : Bit_Offset;
         Size         : Bit_Size);
      --  Copy_Bitfield in the case where Size > Val'Size.
      --  The Address values must be aligned as for Val and Val_2.
      --  This works for overlapping bit fields only if the source
      --  bit address is greater than or equal to the destination
      --  bit address, because it copies forward (from lower to higher
      --  bit addresses).

      function Get_Val_2
        (Src_Address : Address;
         Src_Offset : Bit_Offset;
         Size : Small_Size)
        return Val_2 is
      begin
         pragma Assert (Src_Address mod Val'Alignment = 0);

         --  Bit field fits in first half; fetch just one Val. On little
         --  endian, we want that in the low half, but on big endian, we
         --  want it in the high half.

         if Src_Offset + Size <= Val'Size then
            declare
               Result : aliased constant Val with
                 Import, Address => Src_Address;
            begin
               return (case Endian is
                  when Little => Val_2 (Result),
                  when Big => Shift_Left (Val_2 (Result), Val'Size));
            end;

         --  Bit field crosses into the second half, so it's safe to fetch the
         --  whole Val_2.

         else
            declare
               Result : aliased constant Val_2 with
                 Import, Address => Src_Address;
            begin
               return Result;
            end;
         end if;
      end Get_Val_2;

      procedure Set_Val_2
        (Dest_Address : Address;
         Dest_Offset : Bit_Offset;
         V : Val_2;
         Size : Small_Size) is
      begin
         pragma Assert (Dest_Address mod Val'Alignment = 0);

         --  Comments in Get_Val_2 apply, except we're storing instead of
         --  fetching.

         if Dest_Offset + Size <= Val'Size then
            declare
               Dest : aliased Val with Import, Address => Dest_Address;
            begin
               Dest := (case Endian is
                  when Little => Val'Mod (V),
                  when Big => Val (Shift_Right (V, Val'Size)));
            end;
         else
            declare
               Dest : aliased Val_2 with Import, Address => Dest_Address;
            begin
               Dest := V;
            end;
         end if;
      end Set_Val_2;

      function Get_Bitfield
        (Src : Val_2; Src_Offset : Bit_Offset; Size : Small_Size)
         return Val
      is
         L_Shift_Amount : constant Natural :=
           (case Endian is
              when Little => Val_2'Size - (Src_Offset + Size),
              when Big => Src_Offset);
         Temp1 : constant Val_2 :=
           Shift_Left (Src, L_Shift_Amount);
         Temp2 : constant Val_2 :=
           Shift_Right (Temp1, Val_2'Size - Size);
      begin
         return Val (Temp2);
      end Get_Bitfield;

      function Set_Bitfield
        (Src_Value : Val;
         Dest : Val_2;
         Dest_Offset : Bit_Offset;
         Size : Small_Size)
        return Val_2
      is
         pragma Assert (Size = Val'Size or else Src_Value < 2**Size);
         L_Shift_Amount : constant Natural :=
           (case Endian is
              when Little => Dest_Offset,
              when Big => Val_2'Size - (Dest_Offset + Size));
         Mask : constant Val_2 :=
           Shift_Left (Shift_Left (1, Size) - 1, L_Shift_Amount);
         Temp1 : constant Val_2 := Dest and not Mask;
         Temp2 : constant Val_2 :=
           Shift_Left (Val_2 (Src_Value), L_Shift_Amount);
         Result : constant Val_2 := Temp1 or Temp2;
      begin
         return Result;
      end Set_Bitfield;

      procedure Set_Bitfield
        (Src_Value : Val;
         Dest_Address : Address;
         Dest_Offset : Bit_Offset;
         Size : Small_Size)
      is
         Old_Dest : constant Val_2 :=
           Get_Val_2 (Dest_Address, Dest_Offset, Size);
         New_Dest : constant Val_2 :=
           Set_Bitfield (Src_Value, Old_Dest, Dest_Offset, Size);
      begin
         Set_Val_2 (Dest_Address, Dest_Offset, New_Dest, Size);
      end Set_Bitfield;

      procedure Copy_Small_Bitfield
        (Src_Address  : Address;
         Src_Offset   : Bit_Offset;
         Dest_Address : Address;
         Dest_Offset  : Bit_Offset;
         Size         : Small_Size)
      is
         Src : constant Val_2 := Get_Val_2 (Src_Address, Src_Offset, Size);
         V : constant Val := Get_Bitfield (Src, Src_Offset, Size);
      begin
         Set_Bitfield (V, Dest_Address, Dest_Offset, Size);
      end Copy_Small_Bitfield;

      --  Copy_Large_Bitfield does the main work. Copying aligned Vals is more
      --  efficient than fiddling with shifting and whatnot. But we can't align
      --  both source and destination. We choose to align the destination,
      --  because that's more efficient -- Set_Bitfield needs to read, then
      --  modify, then write, whereas Get_Bitfield does not.
      --
      --  So the method is:
      --
      --      Step 1:
      --      If the destination is not already aligned, copy Initial_Size
      --      bits, and increment the bit addresses. Initial_Size is chosen to
      --      be the smallest size that will cause the destination bit address
      --      to be aligned (i.e. have zero bit offset from the already-aligned
      --      Address). Get_Bitfield and Set_Bitfield are used here.
      --
      --      Step 2:
      --      Loop, copying Vals. Get_Bitfield is used to fetch a Val-sized
      --      bit field, but Set_Bitfield is not needed -- we can set the
      --      aligned Val with an array indexing.
      --
      --      Step 3:
      --      Copy remaining smaller-than-Val bits, if any

      procedure Copy_Large_Bitfield
        (Src_Address  : Address;
         Src_Offset   : Bit_Offset;
         Dest_Address : Address;
         Dest_Offset  : Bit_Offset;
         Size         : Bit_Size)
      is
         Sz : Bit_Size := Size;
         S_Addr : Address := Src_Address;
         S_Off : Bit_Offset := Src_Offset;
         D_Addr : Address := Dest_Address;
         D_Off : Bit_Offset := Dest_Offset;
      begin
         if S_Addr < D_Addr or else (S_Addr = D_Addr and then S_Off < D_Off)
         then
            --  Here, the source bit address is less than the destination bit
            --  address. Assert that there is no overlap.

            declare
               Temp_Off : constant Bit_Offset'Base := S_Off + Size;
               After_S_Addr : constant Address :=
                 S_Addr + Address (Temp_Off / Storage_Unit);
               After_S_Off : constant Bit_Offset_In_Byte :=
                 Temp_Off mod Storage_Unit;
               --  (After_S_Addr, After_S_Off) is the bit address of the bit
               --  just after the source bit field. Assert that it's less than
               --  or equal to the destination bit address.
               Overlap_OK : constant Boolean :=
                 After_S_Addr < D_Addr
                   or else
                 (After_S_Addr = D_Addr and then After_S_Off <= D_Off);
            begin
               pragma Assert (Overlap_OK);
            end;
         end if;

         if D_Off /= 0 then
            --  Step 1:

            declare
               Initial_Size : constant Small_Size := Val'Size - D_Off;
               Initial_Val_2 : constant Val_2 :=
                 Get_Val_2 (S_Addr, S_Off, Initial_Size);
               Initial_Val : constant Val :=
                 Get_Bitfield (Initial_Val_2, S_Off, Initial_Size);
            begin
               Set_Bitfield
                 (Initial_Val, D_Addr, D_Off, Initial_Size);

               Sz := Sz - Initial_Size;
               declare
                  New_S_Off : constant Bit_Offset'Base := S_Off + Initial_Size;
               begin
                  if New_S_Off > Bit_Offset'Last then
                     S_Addr := S_Addr + Val_Bytes;
                     S_Off := New_S_Off - Small_Size'Last;
                  else
                     S_Off := New_S_Off;
                  end if;
               end;
               D_Addr := D_Addr + Val_Bytes;
               pragma Assert (D_Off + Initial_Size = Val'Size);
               D_Off := 0;
            end;
         end if;

         --  Step 2:

         declare
            Dest_Arr : Val_Array (1 .. Sz / Val'Size) with Import,
              Address => D_Addr;
         begin
            for Dest_Comp of Dest_Arr loop
               declare
                  pragma Warnings (Off);
                  pragma Assert (Dest_Comp in Val);
                  pragma Warnings (On);
                  pragma Assert (Dest_Comp'Valid);
                  Src_V_2 : constant Val_2 :=
                    Get_Val_2 (S_Addr, S_Off, Val'Size);
                  Full_V : constant Val :=
                    Get_Bitfield (Src_V_2, S_Off, Val'Size);
               begin
                  Dest_Comp := Full_V;
                  S_Addr := S_Addr + Val_Bytes;
                  --  S_Off remains the same
               end;
            end loop;

            Sz := Sz mod Val'Size;
            if Sz /= 0 then
               --  Step 3:

               declare
                  Final_Val_2 : constant Val_2 :=
                    Get_Val_2 (S_Addr, S_Off, Sz);
                  Final_Val : constant Val :=
                    Get_Bitfield (Final_Val_2, S_Off, Sz);
               begin
                  Set_Bitfield
                    (Final_Val, D_Addr + Dest_Arr'Length * Val_Bytes, 0, Sz);
               end;
            end if;
         end;
      end Copy_Large_Bitfield;

      procedure Copy_Bitfield
        (Src_Address  : Address;
         Src_Offset   : Bit_Offset_In_Byte;
         Dest_Address : Address;
         Dest_Offset  : Bit_Offset_In_Byte;
         Size         : Bit_Size)
      is
         --  Align the Address values as for Val and Val_2, and adjust the
         --  Bit_Offsets accordingly.

         Src_Adjust     : constant Address := Src_Address mod Val_Bytes;
         Al_Src_Address : constant Address := Src_Address - Src_Adjust;
         Al_Src_Offset  : constant Bit_Offset :=
           Src_Offset + Bit_Offset (Src_Adjust * Storage_Unit);

         Dest_Adjust     : constant Address := Dest_Address mod Val_Bytes;
         Al_Dest_Address : constant Address := Dest_Address - Dest_Adjust;
         Al_Dest_Offset  : constant Bit_Offset :=
           Dest_Offset + Bit_Offset (Dest_Adjust * Storage_Unit);

         pragma Assert (Al_Src_Address mod Val'Alignment = 0);
         pragma Assert (Al_Dest_Address mod Val'Alignment = 0);
      begin
         if Size in Small_Size then
            Copy_Small_Bitfield
              (Al_Src_Address, Al_Src_Offset,
               Al_Dest_Address, Al_Dest_Offset,
               Size);
         else
            Copy_Large_Bitfield
              (Al_Src_Address, Al_Src_Offset,
               Al_Dest_Address, Al_Dest_Offset,
               Size);
         end if;
      end Copy_Bitfield;

   end G;

end System.Bitfield_Utils;
