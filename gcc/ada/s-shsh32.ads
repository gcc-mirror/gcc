------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--         S Y S T E M . S E C U R E _ H A S H E S . S H A 2 _ 3 2          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2009, Free Software Foundation, Inc.             --
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

--  This pacakge provides support for the 32-bit FIPS PUB 180-3 functions
--  SHA-256 and SHA-224.

with Interfaces;
with GNAT.Byte_Swapping;
with System.Secure_Hashes.SHA2_Common;

package System.Secure_Hashes.SHA2_32 is

   subtype Word is Interfaces.Unsigned_32;

   package Hash_State is new Hash_Function_State
     (Word           => Word,
      Swap           => GNAT.Byte_Swapping.Swap4,
      Hash_Bit_Order => System.High_Order_First);
   --  SHA-224 and SHA-256 operate on 32-bit big endian words

   K : constant Hash_State.State (0 .. 63) :=
         (16#428a2f98#, 16#71374491#, 16#b5c0fbcf#, 16#e9b5dba5#,
          16#3956c25b#, 16#59f111f1#, 16#923f82a4#, 16#ab1c5ed5#,
          16#d807aa98#, 16#12835b01#, 16#243185be#, 16#550c7dc3#,
          16#72be5d74#, 16#80deb1fe#, 16#9bdc06a7#, 16#c19bf174#,
          16#e49b69c1#, 16#efbe4786#, 16#0fc19dc6#, 16#240ca1cc#,
          16#2de92c6f#, 16#4a7484aa#, 16#5cb0a9dc#, 16#76f988da#,
          16#983e5152#, 16#a831c66d#, 16#b00327c8#, 16#bf597fc7#,
          16#c6e00bf3#, 16#d5a79147#, 16#06ca6351#, 16#14292967#,
          16#27b70a85#, 16#2e1b2138#, 16#4d2c6dfc#, 16#53380d13#,
          16#650a7354#, 16#766a0abb#, 16#81c2c92e#, 16#92722c85#,
          16#a2bfe8a1#, 16#a81a664b#, 16#c24b8b70#, 16#c76c51a3#,
          16#d192e819#, 16#d6990624#, 16#f40e3585#, 16#106aa070#,
          16#19a4c116#, 16#1e376c08#, 16#2748774c#, 16#34b0bcb5#,
          16#391c0cb3#, 16#4ed8aa4a#, 16#5b9cca4f#, 16#682e6ff3#,
          16#748f82ee#, 16#78a5636f#, 16#84c87814#, 16#8cc70208#,
          16#90befffa#, 16#a4506ceb#, 16#bef9a3f7#, 16#c67178f2#);
   --  Constants from FIPS PUB 180-3

   function Sigma0 (X : Word) return Word;
   function Sigma1 (X : Word) return Word;
   function S0 (X : Word) return Word;
   function S1 (X : Word) return Word;
   pragma Inline (Sigma0, Sigma1, S0, S1);
   --  Elementary functions Sigma^256_0, Sigma^256_1, sigma^256_0, sigma^256_1
   --  from FIPS PUB 180-3.

   procedure Transform is new SHA2_Common.Transform
     (Hash_State => Hash_State,
      K          => K,
      Rounds     => 64,
      Sigma0     => Sigma0,
      Sigma1     => Sigma1,
      S0         => S0,
      S1         => S1);

   SHA224_Init_State : constant Hash_State.State (0 .. 7) :=
                         (0 => 16#c1059ed8#,
                          1 => 16#367cd507#,
                          2 => 16#3070dd17#,
                          3 => 16#f70e5939#,
                          4 => 16#ffc00b31#,
                          5 => 16#68581511#,
                          6 => 16#64f98fa7#,
                          7 => 16#befa4fa4#);
   SHA256_Init_State : constant Hash_State.State (0 .. 7) :=
                         (0 => 16#6a09e667#,
                          1 => 16#bb67ae85#,
                          2 => 16#3c6ef372#,
                          3 => 16#a54ff53a#,
                          4 => 16#510e527f#,
                          5 => 16#9b05688c#,
                          6 => 16#1f83d9ab#,
                          7 => 16#5be0cd19#);
   --  Initialization vectors from FIPS PUB 180-3

end System.Secure_Hashes.SHA2_32;
