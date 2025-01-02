------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ A                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2024-2025, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

with System.Storage_Elements; use System.Storage_Elements;

package body System.Image_A is

   -------------------
   -- Address_Image --
   -------------------

   function Address_Image (A : Address) return Address_String is
      Result : Address_String;

      type Byte is mod 2 ** 8;
      for Byte'Size use 8;

      Hexdigs :
        constant array (Byte range 0 .. 15) of Character := "0123456789ABCDEF";

      type Bytes is array (1 .. Uns'Size / Storage_Unit) of Byte;

      function To_Bytes is new Ada.Unchecked_Conversion (Uns, Bytes);

      Byte_Sequence : constant Bytes := To_Bytes (Uns (Integer_Address (A)));

      LE : constant := Standard'Default_Bit_Order;
      BE : constant := 1 - LE;
      --  Set to 1/0 for True/False for Little-Endian/Big-Endian

      Start : constant Natural := BE * (1) + LE * (Bytes'Length);
      Incr  : constant Integer := BE * (1) + LE * (-1);
      --  Start and increment for accessing characters of address string

      Ptr : Natural;
      --  Scan address string

   begin
      Ptr := Start;

      for N in Bytes'Range loop
         Result (2 * N - 1) := Hexdigs (Byte_Sequence (Ptr) / 16);
         Result (2 * N)     := Hexdigs (Byte_Sequence (Ptr) mod 16);
         Ptr := Ptr + Incr;
      end loop;

      return Result;
   end Address_Image;

end System.Image_A;
