------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . B I T F I E L D _ U T I L S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2019-2021, Free Software Foundation, Inc.    --
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

package System.Bitfield_Utils is

   --  This package provides a procedure for copying arbitrarily large and
   --  arbitrarily bit-aligned bit fields.

   --  Type Val is used to represent small bit fields. Val_2 represents a
   --  contiguous pair of Vals. Val_2'Alignment is half of its size in bytes,
   --  which is likely not the natural alignment. This is done to ensure that
   --  any bit field that fits in a Val can fit in an aligned Val_2, starting
   --  somewhere in the first half, and possibly crossing over into the second
   --  half. This allows us to isolate a Val value by shifting and masking the
   --  Val_2.
   --
   --  Val can be 8, 16, or 32 bits; larger values are more efficient. It can't
   --  be 64 bits, because we need Val_2 to be a double-wide shiftable type,
   --  and 128 bits is not supported. Instantiating with an 8-bit Val is useful
   --  for testing and debugging; 32 bits should be used for production.
   --
   --  We use modular types here, not because we want modular arithmetic, but
   --  so we can do shifting and masking. The actual for Val_2 should have
   --  pragma Provide_Shift_Operators, so that the Shift_Left and Shift_Right
   --  intrinsics can be passed in. It is impossible to put that pragma on a
   --  generic formal, or on a type derived from a generic formal, so they have
   --  to be passed in.
   --
   --  Endian indicates whether we're on a little- or big-endian machine.

   pragma Elaborate_Body;

   Little : constant Bit_Order := Low_Order_First;
   Big : constant Bit_Order := High_Order_First;

   generic
      type Val is mod <>;
      type Val_2 is mod <>;

     with function Shift_Left
       (Value  : Val_2;
        Amount : Natural) return Val_2 is <>;

     with function Shift_Right
       (Value  : Val_2;
        Amount : Natural) return Val_2 is <>;

      Endian : Bit_Order := Default_Bit_Order;

   package G is
      --  Assert that Val has one of the allowed sizes, and that Val_2 is twice
      --  that.

      pragma Assert (Val'Size in 8 | 16 | 32);
      pragma Assert (Val_2'Size = Val'Size * 2);

      --  Assert that both are aligned the same, to the size in bytes of Val
      --  (not Val_2).

      pragma Assert (Val'Alignment = Val'Size / Storage_Unit);
      pragma Assert (Val_2'Alignment = Val'Alignment);

      type Val_Array is array (Positive range <>) of Val;

      --  It might make more sense to have:
      --    subtype Val is Val_2 range 0 .. 2**Val'Size - 1;
      --  But then GNAT gets the component size of Val_Array wrong.

      pragma Assert (Val_Array'Alignment = Val'Alignment);
      pragma Assert (Val_Array'Component_Size = Val'Size);

      subtype Bit_Size is Natural; -- Size in bits of a bit field
      subtype Small_Size is Bit_Size range 1 .. Val'Size;
      --  Size of a small one
      subtype Bit_Offset is Small_Size'Base range 0 .. Val'Size - 1;
      --  Starting offset
      subtype Bit_Offset_In_Byte is Bit_Offset range 0 .. Storage_Unit - 1;

      procedure Copy_Bitfield
        (Src_Address  : Address;
         Src_Offset   : Bit_Offset_In_Byte;
         Dest_Address : Address;
         Dest_Offset  : Bit_Offset_In_Byte;
         Size         : Bit_Size);
      --  An Address and a Bit_Offset together form a "bit address". This
      --  copies the source bit field to the destination. Size is the size in
      --  bits of the bit field. The bit fields can be arbitrarily large, but
      --  the starting offsets must be within the first byte that the Addresses
      --  point to. The Address values need not be aligned.
      --
      --  For example, a slice assignment of a packed bit field:
      --
      --     D (D_First .. D_Last) := S (S_First .. S_Last);
      --
      --  can be implemented using:
      --
      --     Copy_Bitfield
      --       (S (S_First)'Address, S (S_First)'Bit,
      --        D (D_First)'Address, D (D_First)'Bit,
      --        Size);

      function Fast_Copy_Bitfield
        (Src         : Val_2;
         Src_Offset  : Bit_Offset;
         Dest        : Val_2;
         Dest_Offset : Bit_Offset;
         Size        : Small_Size)
        return Val_2 with Inline;
      --  Faster version of Copy_Bitfield, with a different calling convention.
      --  In particular, we pass by copy rather than passing Addresses. The bit
      --  field must fit in Val_Bits. Src and Dest must be properly aligned.
      --  The result is supposed to be assigned back into Dest, as in:
      --
      --     Dest := Fast_Copy_Bitfield (Src, ..., Dest, ..., ...);

   end G;

end System.Bitfield_Utils;
