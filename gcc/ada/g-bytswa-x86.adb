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

--  This is a machine-specific version of this package.
--  It uses instructions available on Intel 486 processors (or later).

with Interfaces;          use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with Ada.Unchecked_Conversion;

package body GNAT.Byte_Swapping is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Swapped32 (Value : Unsigned_32) return Unsigned_32;
   pragma Inline_Always (Swapped32);

   --------------
   -- Swapped2 --
   --------------

   function Swapped2 (Input : Item) return Item is

      function As_U16 is new Ada.Unchecked_Conversion
         (Source => Item, Target => Unsigned_16);

      function As_Item is new Ada.Unchecked_Conversion
         (Source => Unsigned_16, Target => Item);

      X : Unsigned_16 := As_U16 (Input);

   begin
      Asm ("xchgb %b0,%h0",
           Unsigned_16'Asm_Output ("=q", X),
           Unsigned_16'Asm_Input ("0", X));
      return As_Item (X);
   end Swapped2;

   --------------
   -- Swapped4 --
   --------------

   function Swapped4 (Input : Item) return Item is

      function As_U32 is new Ada.Unchecked_Conversion
         (Source => Item, Target => Unsigned_32);

      function As_Item is new Ada.Unchecked_Conversion
         (Source => Unsigned_32, Target => Item);

      X : Unsigned_32 := As_U32 (Input);

   begin
      Asm ("bswap %0",
           Unsigned_32'Asm_Output ("=r", X),
           Unsigned_32'Asm_Input ("0", X));
      return As_Item (X);
   end Swapped4;

   --------------
   -- Swapped8 --
   --------------

   function Swapped8 (Input : Item) return Item is

      function As_U64 is new Ada.Unchecked_Conversion
         (Source => Item, Target => Unsigned_64);

      X : constant Unsigned_64 := As_U64 (Input);

      type Two_Words is array (0 .. 1) of Unsigned_32;
      for Two_Words'Component_Size use Unsigned_32'Size;

      function As_Item is new Ada.Unchecked_Conversion
        (Source => Two_Words, Target => Item);

      Result : Two_Words;

   begin
      Asm ("xchgl %0,%1",
         Outputs =>
            (Unsigned_32'Asm_Output ("=r", Result (0)),
             Unsigned_32'Asm_Output ("=r", Result (1))),
         Inputs =>
            (Unsigned_32'Asm_Input ("0",
                Swapped32 (Unsigned_32 (X and 16#0000_0000_FFFF_FFFF#))),
             Unsigned_32'Asm_Input ("1",
                Swapped32 (Unsigned_32 (Shift_Right (X, 32))))));
      return As_Item (Result);
   end Swapped8;

   -----------
   -- Swap2 --
   -----------

   procedure Swap2 (Location : System.Address) is

      X : Unsigned_16;
      for X'Address use Location;

   begin
      Asm ("xchgb %b0,%h0",
           Unsigned_16'Asm_Output ("=q", X),
           Unsigned_16'Asm_Input ("0", X));
   end Swap2;

   -----------
   -- Swap4 --
   -----------

   procedure Swap4 (Location : System.Address) is

      X : Unsigned_32;
      for X'Address use Location;

   begin
      Asm ("bswap %0",
           Unsigned_32'Asm_Output ("=r", X),
           Unsigned_32'Asm_Input ("0", X));
   end Swap4;

   ---------------
   -- Swapped32 --
   ---------------

   function Swapped32 (Value : Unsigned_32) return Unsigned_32 is
      X : Unsigned_32 := Value;
   begin
      Asm ("bswap %0",
           Unsigned_32'Asm_Output ("=r", X),
           Unsigned_32'Asm_Input ("0", X));
      return X;
   end Swapped32;

   -----------
   -- Swap8 --
   -----------

   procedure Swap8 (Location : System.Address) is

      X : Unsigned_64;
      for X'Address use Location;

      type Two_Words is array (0 .. 1) of Unsigned_32;
      for Two_Words'Component_Size use Unsigned_32'Size;

      Words : Two_Words;
      for Words'Address use Location;

   begin
      Asm ("xchgl %0,%1",
         Outputs =>
            (Unsigned_32'Asm_Output ("=r", Words (0)),
             Unsigned_32'Asm_Output ("=r", Words (1))),
         Inputs =>
            (Unsigned_32'Asm_Input ("0",
                Swapped32 (Unsigned_32 (X and 16#0000_0000_FFFF_FFFF#))),
             Unsigned_32'Asm_Input ("1",
                Swapped32 (Unsigned_32 (Shift_Right (X, 32))))));
   end Swap8;

end GNAT.Byte_Swapping;
