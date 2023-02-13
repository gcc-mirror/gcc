------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       G N A T . S E C U R E _ H A S H E S . S H A 2 _ C O M M O N        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2009-2023, Free Software Foundation, Inc.        --
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

package body GNAT.Secure_Hashes.SHA2_Common is

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (H_St : in out Hash_State.State;
      M_St : in out Message_State)
   is
      use System;

      subtype Word is Hash_State.Word;
      use type Hash_State.Word;

      function Ch (X, Y, Z : Word) return Word;
      function Maj (X, Y, Z : Word) return Word;
      pragma Inline (Ch, Maj);
      --  Elementary functions from FIPS PUB 180-3

      --------
      -- Ch --
      --------

      function Ch (X, Y, Z : Word) return Word is
      begin
         return (X and Y) xor ((not X) and Z);
      end Ch;

      ---------
      -- Maj --
      ---------

      function Maj (X, Y, Z : Word) return Word is
      begin
         return (X and Y) xor (X and Z) xor (Y and Z);
      end Maj;

      type Words is array (Natural range <>) of Word;

      X : Words (0 .. 15);
      for X'Address use M_St.Buffer'Address;
      pragma Import (Ada, X);

      W : Words (0 .. Rounds - 1);

      A, B, C, D, E, F, G, H, T1, T2 : Word;

   --  Start of processing for Transform

   begin
      if Default_Bit_Order /= High_Order_First then
         for J in X'Range loop
            Hash_State.Swap (X (J)'Address);
         end loop;
      end if;

      --  1. Prepare message schedule

      W (0 .. 15) := X;

      for T in 16 .. Rounds - 1 loop
         W (T) := S1 (W (T - 2)) + W (T - 7) + S0 (W (T - 15)) + W (T - 16);
      end loop;

      --  2. Initialize working variables

      A := H_St (0);
      B := H_St (1);
      C := H_St (2);
      D := H_St (3);
      E := H_St (4);
      F := H_St (5);
      G := H_St (6);
      H := H_St (7);

      --  3. Perform transformation rounds

      for T in 0 .. Rounds - 1 loop
         T1 := H + Sigma1 (E) + Ch (E, F, G)
             + K (Stream_Element_Offset (T)) + W (T);
         T2 := Sigma0 (A) + Maj (A, B, C);
         H := G;
         G := F;
         F := E;
         E := D + T1;
         D := C;
         C := B;
         B := A;
         A := T1 + T2;
      end loop;

      --  4. Update hash state

      H_St (0) := A + H_St (0);
      H_St (1) := B + H_St (1);
      H_St (2) := C + H_St (2);
      H_St (3) := D + H_St (3);
      H_St (4) := E + H_St (4);
      H_St (5) := F + H_St (5);
      H_St (6) := G + H_St (6);
      H_St (7) := H + H_St (7);
   end Transform;

end GNAT.Secure_Hashes.SHA2_Common;
