------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . B I T F I E L D _ U T I L S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2019-2023, Free Software Foundation, Inc.    --
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

with System.Bitfield_Utils;

package System.Bitfields is

   --  Instances of the generic package in System.Bitfield_Utils. So far
   --  we have just one, which defaults to the natural endianness of the
   --  machine. We might someday want to support Scalar_Storage_Order.
   --  Note: we use Long_Long_Integer'Size / 2 instead of 32 to support
   --  specifying a target configuration file where the largest integer is
   --  32 bits instead of 64.

   Val_Bits  : constant := Long_Long_Integer'Size / 2;
   Val_Bytes : constant := Val_Bits / System.Storage_Unit;

   type Val_2 is mod 2**(Val_Bits * 2) with Alignment => Val_Bytes;
   pragma Provide_Shift_Operators (Val_2);
   type Val is mod 2**Val_Bits with Alignment => Val_Bytes;

   --  Enabling checks on the instantiation of System.Bitfield_Utils.G makes a
   --  latent visibility bug appear on strict alignment platforms related to
   --  alignment checks. Work around it by suppressing these checks explicitly.

   pragma Suppress (Alignment_Check);
   package Utils is new System.Bitfield_Utils.G (Val, Val_2);

   procedure Copy_Bitfield
     (Src_Address  : Address;
      Src_Offset   : Utils.Bit_Offset_In_Byte;
      Dest_Address : Address;
      Dest_Offset  : Utils.Bit_Offset_In_Byte;
      Size         : Utils.Bit_Size)
     renames Utils.Copy_Bitfield;

   function Fast_Copy_Bitfield
     (Src         : Val_2;
      Src_Offset  : Utils.Bit_Offset;
      Dest        : Val_2;
      Dest_Offset : Utils.Bit_Offset;
      Size        : Utils.Small_Size)
     return Val_2 renames Utils.Fast_Copy_Bitfield;

end System.Bitfields;
