------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              G N A T . S E C U R E _ H A S H E S . S H A 1               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2023, Free Software Foundation, Inc.          --
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

package body GNAT.Secure_Hashes.SHA1 is

   use Interfaces;
   use GNAT.Byte_Swapping;

   --  The following functions are the four elementary components of each
   --  of the four round groups (0 .. 19, 20 .. 39, 40 .. 59, and 60 .. 79)
   --  defined in RFC 3174.

   function F0 (B, C, D : Unsigned_32) return Unsigned_32;
   pragma Inline (F0);

   function F1 (B, C, D : Unsigned_32) return Unsigned_32;
   pragma Inline (F1);

   function F2 (B, C, D : Unsigned_32) return Unsigned_32;
   pragma Inline (F2);

   function F3 (B, C, D : Unsigned_32) return Unsigned_32;
   pragma Inline (F3);

   --------
   -- F0 --
   --------

   function F0
     (B, C, D : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
   begin
      return (B and C) or ((not B) and D);
   end F0;

   --------
   -- F1 --
   --------

   function F1
     (B, C, D : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
   begin
      return B xor C xor D;
   end F1;

   --------
   -- F2 --
   --------

   function F2
     (B, C, D : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
   begin
      return (B and C) or (B and D) or (C and D);
   end F2;

   --------
   -- F3 --
   --------

   function F3
     (B, C, D : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     renames F1;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (H : in out Hash_State.State;
      M : in out Message_State)
   is
      use System;

      type Words is array (Natural range <>) of Interfaces.Unsigned_32;

      X : Words (0 .. 15);
      for X'Address use M.Buffer'Address;
      pragma Import (Ada, X);

      W : Words (0 .. 79);

      A, B, C, D, E, Temp : Interfaces.Unsigned_32;

   begin
      if Default_Bit_Order /= High_Order_First then
         for J in X'Range loop
            Swap4 (X (J)'Address);
         end loop;
      end if;

      --  a. Divide data block into sixteen words

      W (0 .. 15) := X;

      --  b. Prepare working block of 80 words

      for T in 16 .. 79 loop

         --  W(t) = S^1(W(t-3) XOR W(t-8) XOR W(t-14) XOR W(t-16))

         W (T) := Rotate_Left
           (W (T - 3) xor W (T - 8) xor W (T - 14) xor W (T - 16), 1);

      end loop;

      --  c. Set up transformation variables

      A := H (0);
      B := H (1);
      C := H (2);
      D := H (3);
      E := H (4);

      --  d. For each of the 80 rounds, compute:

      --  TEMP = S^5(A) + f(t;B,C,D) + E + W(t) + K(t);
      --  E = D;  D = C;  C = S^30(B);  B = A; A = TEMP;

      for T in 0 .. 19 loop
         Temp := Rotate_Left (A, 5) + F0 (B, C, D) + E + W (T) + 16#5A827999#;
         E := D; D := C; C := Rotate_Left (B, 30); B := A; A := Temp;
      end loop;

      for T in 20 .. 39 loop
         Temp := Rotate_Left (A, 5) + F1 (B, C, D) + E + W (T) + 16#6ED9EBA1#;
         E := D; D := C; C := Rotate_Left (B, 30); B := A; A := Temp;
      end loop;

      for T in 40 .. 59 loop
         Temp := Rotate_Left (A, 5) + F2 (B, C, D) + E + W (T) + 16#8F1BBCDC#;
         E := D; D := C; C := Rotate_Left (B, 30); B := A; A := Temp;
      end loop;

      for T in 60 .. 79 loop
         Temp := Rotate_Left (A, 5) + F3 (B, C, D) + E + W (T) + 16#CA62C1D6#;
         E := D; D := C; C := Rotate_Left (B, 30); B := A; A := Temp;
      end loop;

      --  e. Update context:
      --  H0 = H0 + A, H1 = H1 + B, H2 = H2 + C, H3 = H3 + D, H4 = H4 + E

      H (0) := H (0) + A;
      H (1) := H (1) + B;
      H (2) := H (2) + C;
      H (3) := H (3) + D;
      H (4) := H (4) + E;
   end Transform;

end GNAT.Secure_Hashes.SHA1;
