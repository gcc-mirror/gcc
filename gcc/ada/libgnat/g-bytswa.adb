------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    G N A T . B Y T E _ S W A P P I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2006-2020, AdaCore                     --
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

--  This is a general implementation that uses GCC intrinsics to take
--  advantage of any machine-specific instructions.

with Ada.Unchecked_Conversion; use Ada;

with System.Byte_Swapping; use System.Byte_Swapping;

package body GNAT.Byte_Swapping is

   --------------
   -- Swapped2 --
   --------------

   function Swapped2 (Input : Item) return Item is
      function As_U16 is new Unchecked_Conversion (Item, U16);
      function As_Item is new Unchecked_Conversion (U16, Item);
      pragma Compile_Time_Warning (Item'Max_Size_In_Storage_Elements /= 2,
        "storage size must be 2 bytes");
   begin
      return As_Item (Bswap_16 (As_U16 (Input)));
   end Swapped2;

   --------------
   -- Swapped4 --
   --------------

   function Swapped4 (Input : Item) return Item is
      function As_U32 is new Unchecked_Conversion (Item, U32);
      function As_Item is new Unchecked_Conversion (U32, Item);
      pragma Compile_Time_Warning (Item'Max_Size_In_Storage_Elements /= 4,
        "storage size must be 4 bytes");
   begin
      return As_Item (Bswap_32 (As_U32 (Input)));
   end Swapped4;

   --------------
   -- Swapped8 --
   --------------

   function Swapped8 (Input : Item) return Item is
      function As_U64 is new Unchecked_Conversion (Item, U64);
      function As_Item is new Unchecked_Conversion (U64, Item);
      pragma Compile_Time_Warning (Item'Max_Size_In_Storage_Elements /= 8,
        "storage size must be 8 bytes");
   begin
      return As_Item (Bswap_64 (As_U64 (Input)));
   end Swapped8;

   -----------
   -- Swap2 --
   -----------

   procedure Swap2 (Location : System.Address) is
      X : U16;
      for X'Address use Location;
   begin
      X := Bswap_16 (X);
   end Swap2;

   -----------
   -- Swap4 --
   -----------

   procedure Swap4 (Location : System.Address) is
      X : U32;
      for X'Address use Location;
   begin
      X := Bswap_32 (X);
   end Swap4;

   -----------
   -- Swap8 --
   -----------

   procedure Swap8 (Location : System.Address) is
      X : U64;
      for X'Address use Location;
   begin
      X := Bswap_64 (X);
   end Swap8;

end GNAT.Byte_Swapping;
