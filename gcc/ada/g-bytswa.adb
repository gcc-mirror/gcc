------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    G N A T . B Y T E _ S W A P P I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2006-2007, AdaCore                     --
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

--  This is a general implementation that does not take advantage of
--  any machine-specific instructions.

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

package body GNAT.Byte_Swapping is

   --------------
   -- Swapped2 --
   --------------

   function Swapped2 (Input : Item) return Item is

      function As_U16 is new Ada.Unchecked_Conversion
         (Source => Item, Target => Unsigned_16);

      function As_Item is new Ada.Unchecked_Conversion
         (Source => Unsigned_16, Target => Item);

      X : constant Unsigned_16 := As_U16 (Input);

   begin
      return As_Item ((Shift_Left (X, 8)  and 16#FF00#) or
                      (Shift_Right (X, 8) and 16#00FF#));
   end Swapped2;

   --------------
   -- Swapped4 --
   --------------

   function Swapped4 (Input : Item) return Item is

      function As_U32 is new Ada.Unchecked_Conversion
         (Source => Item, Target => Unsigned_32);

      function As_Item is new Ada.Unchecked_Conversion
         (Source => Unsigned_32, Target => Item);

      X : constant Unsigned_32 := As_U32 (Input);

   begin
      return As_Item ((Shift_Right (X, 24) and 16#0000_00FF#) or
                      (Shift_Right (X, 8)  and 16#0000_FF00#) or
                      (Shift_Left (X, 8)   and 16#00FF_0000#) or
                      (Shift_Left (X, 24)  and 16#FF00_0000#));
   end Swapped4;

   --------------
   -- Swapped8 --
   --------------

   function Swapped8 (Input : Item) return Item is

      function As_U64 is new Ada.Unchecked_Conversion
         (Source => Item, Target => Unsigned_64);

      function As_Item is new Ada.Unchecked_Conversion
         (Source => Unsigned_64, Target => Item);

      X : constant Unsigned_64 := As_U64 (Input);

      Low, High : aliased Unsigned_32;

   begin
      Low := Unsigned_32 (X and 16#0000_0000_FFFF_FFFF#);
      Swap4 (Low'Address);
      High := Unsigned_32 (Shift_Right (X, 32));
      Swap4 (High'Address);
      return As_Item
         (Shift_Left (Unsigned_64 (Low), 32) or Unsigned_64 (High));
   end Swapped8;

   -----------
   -- Swap2 --
   -----------

   procedure Swap2 (Location : System.Address) is
      X : Unsigned_16;
      for X'Address use Location;
   begin
      X := (Shift_Left (X, 8)  and 16#FF00#) or
           (Shift_Right (X, 8) and 16#00FF#);
   end Swap2;

   -----------
   -- Swap4 --
   -----------

   procedure Swap4 (Location : System.Address) is
      X : Unsigned_32;
      for X'Address use Location;
   begin
      X := (Shift_Right (X, 24) and 16#0000_00FF#) or
           (Shift_Right (X, 8)  and 16#0000_FF00#) or
           (Shift_Left (X, 8)   and 16#00FF_0000#) or
           (Shift_Left (X, 24)  and 16#FF00_0000#);
   end Swap4;

   -----------
   -- Swap8 --
   -----------

   procedure Swap8 (Location : System.Address) is
      X : Unsigned_64;
      for X'Address use Location;

      Low, High : aliased Unsigned_32;

   begin
      Low := Unsigned_32 (X and 16#0000_0000_FFFF_FFFF#);
      Swap4 (Low'Address);
      High := Unsigned_32 (Shift_Right (X, 32));
      Swap4 (High'Address);
      X := Shift_Left (Unsigned_64 (Low), 32) or Unsigned_64 (High);
   end Swap8;

end GNAT.Byte_Swapping;
