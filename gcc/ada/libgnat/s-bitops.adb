------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                       S Y S T E M . B I T _ O P S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2025, Free Software Foundation, Inc.          --
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

with System.Unsigned_Types; use System.Unsigned_Types;

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Unchecked_Conversion;

package body System.Bit_Ops is

   subtype Bits_Array is System.Unsigned_Types.Packed_Bytes1 (Positive);
   --  Dummy array type used to interpret the address values. We use the
   --  unaligned version always, since this will handle both the aligned and
   --  unaligned cases, and we always do these operations by bytes anyway.
   --  Note: we use a ones origin array here so that the computations of the
   --  length in bytes work correctly (give a non-negative value) for the
   --  case of zero length bit strings). Note that we never allocate any
   --  objects of this type (we can't because they would be absurdly big).

   type Bits is access Bits_Array;
   --  This is the actual type into which address values are converted

   function To_Bits is new Ada.Unchecked_Conversion (Address, Bits);

   LE : constant := Standard'Default_Bit_Order;
   --  Static constant set to 0 for big-endian, 1 for little-endian

   --  The following is an array of masks used to mask the final byte, either
   --  at the high end (big-endian case) or the low end (little-endian case).

   Masks : constant array (1 .. 7) of Packed_Byte := [
     (1 - LE) * 2#1000_0000# + LE * 2#0000_0001#,
     (1 - LE) * 2#1100_0000# + LE * 2#0000_0011#,
     (1 - LE) * 2#1110_0000# + LE * 2#0000_0111#,
     (1 - LE) * 2#1111_0000# + LE * 2#0000_1111#,
     (1 - LE) * 2#1111_1000# + LE * 2#0001_1111#,
     (1 - LE) * 2#1111_1100# + LE * 2#0011_1111#,
     (1 - LE) * 2#1111_1110# + LE * 2#0111_1111#];

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Raise_Error;
   pragma No_Return (Raise_Error);
   --  Raise Constraint_Error, complaining about unequal lengths

   -------------
   -- Bit_And --
   -------------

   procedure Bit_And
     (Left   : Address;
      Llen   : Natural;
      Right  : Address;
      Rlen   : Natural;
      Result : Address)
   is
      LeftB   : constant Bits := To_Bits (Left);
      RightB  : constant Bits := To_Bits (Right);
      ResultB : constant Bits := To_Bits (Result);

   begin
      if Llen /= Rlen then
         Raise_Error;
      end if;

      for J in 1 .. (Rlen + 7) / 8 loop
         ResultB (J) := LeftB (J) and RightB (J);
      end loop;
   end Bit_And;

   ------------
   -- Bit_Eq --
   ------------

   function Bit_Eq
     (Left  : Address;
      Llen  : Natural;
      Right : Address;
      Rlen  : Natural) return Boolean
   is
      LeftB  : constant Bits := To_Bits (Left);
      RightB : constant Bits := To_Bits (Right);

   begin
      pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                       "early returns for performance");

      if Llen /= Rlen then
         return False;

      else
         declare
            BLen : constant Natural := Llen / 8;
            Bitc : constant Natural := Llen mod 8;

         begin
            if LeftB (1 .. BLen) /= RightB (1 .. BLen) then
               return False;

            elsif Bitc /= 0 then
               return
                 ((LeftB (BLen + 1) xor RightB (BLen + 1))
                   and Masks (Bitc)) = 0;

            else -- Bitc = 0
               return True;
            end if;
         end;
      end if;

      pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
   end Bit_Eq;

   -------------
   -- Bit_Not --
   -------------

   procedure Bit_Not
     (Opnd   : System.Address;
      Len    : Natural;
      Result : System.Address)
   is
      OpndB   : constant Bits := To_Bits (Opnd);
      ResultB : constant Bits := To_Bits (Result);

   begin
      for J in 1 .. (Len + 7) / 8 loop
         ResultB (J) := not OpndB (J);
      end loop;
   end Bit_Not;

   ------------
   -- Bit_Or --
   ------------

   procedure Bit_Or
     (Left   : Address;
      Llen   : Natural;
      Right  : Address;
      Rlen   : Natural;
      Result : Address)
   is
      LeftB   : constant Bits := To_Bits (Left);
      RightB  : constant Bits := To_Bits (Right);
      ResultB : constant Bits := To_Bits (Result);

   begin
      if Llen /= Rlen then
         Raise_Error;
      end if;

      for J in 1 .. (Rlen + 7) / 8 loop
         ResultB (J) := LeftB (J) or RightB (J);
      end loop;
   end Bit_Or;

   -------------
   -- Bit_Xor --
   -------------

   procedure Bit_Xor
     (Left   : Address;
      Llen   : Natural;
      Right  : Address;
      Rlen   : Natural;
      Result : Address)
   is
      LeftB   : constant Bits := To_Bits (Left);
      RightB  : constant Bits := To_Bits (Right);
      ResultB : constant Bits := To_Bits (Result);

   begin
      if Llen /= Rlen then
         Raise_Error;
      end if;

      for J in 1 .. (Rlen + 7) / 8 loop
         ResultB (J) := LeftB (J) xor RightB (J);
      end loop;
   end Bit_Xor;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error is
   begin
      Raise_Exception
        (Constraint_Error'Identity, "operand lengths are unequal");
   end Raise_Error;

end System.Bit_Ops;
