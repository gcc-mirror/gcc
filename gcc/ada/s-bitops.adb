------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       S Y S T E M . B I T _ O P S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with System;                 use System;
with System.Pure_Exceptions; use System.Pure_Exceptions;
with System.Unsigned_Types;  use System.Unsigned_Types;

with Unchecked_Conversion;

package body System.Bit_Ops is

   subtype Bits_Array is System.Unsigned_Types.Packed_Bytes1 (Positive);
   --  Unconstrained array used to interprete the address values. We use the
   --  unaligned version always, since this will handle both the aligned and
   --  unaligned cases, and we always do these operations by bytes anyway.
   --  Note: we use a ones origin array here so that the computations of the
   --  length in bytes work correctly (give a non-negative value) for the
   --  case of zero length bit strings).

   type Bits is access Bits_Array;
   --  This is the actual type into which address values are converted

   function To_Bits is new Unchecked_Conversion (Address, Bits);

   LE : constant := Standard'Default_Bit_Order;
   --  Static constant set to 0 for big-endian, 1 for little-endian

   --  The following is an array of masks used to mask the final byte, either
   --  at the high end (big-endian case) or the low end (little-endian case).

   Masks : constant array (1 .. 7) of Packed_Byte := (
     (1 - LE) * 2#1000_0000# + LE * 2#0000_0001#,
     (1 - LE) * 2#1100_0000# + LE * 2#0000_0011#,
     (1 - LE) * 2#1110_0000# + LE * 2#0000_0111#,
     (1 - LE) * 2#1111_0000# + LE * 2#0000_1111#,
     (1 - LE) * 2#1111_1000# + LE * 2#0001_1111#,
     (1 - LE) * 2#1111_1100# + LE * 2#0011_1111#,
     (1 - LE) * 2#1111_1110# + LE * 2#0111_1111#);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Raise_Error;
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
      Rlen  : Natural)
      return  Boolean
   is
      LeftB  : constant Bits := To_Bits (Left);
      RightB : constant Bits := To_Bits (Right);

   begin
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
      Raise_Exception (CE, "unequal lengths in logical operation");
   end Raise_Error;

end System.Bit_Ops;
