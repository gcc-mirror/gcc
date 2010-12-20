------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           G N A T . S E C U R E _ H A S H E S . S H A 2 _ 6 4            --
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

--  This package provides support for the 64-bit FIPS PUB 180-3 functions
--  SHA-384 and SHA-512.

--  This is an internal unit and should not be used directly in applications.
--  Use GNAT.SHA384 and GNAT.SHA512 instead.

with Interfaces;
with GNAT.Byte_Swapping;

with GNAT.Secure_Hashes.SHA2_Common;

package GNAT.Secure_Hashes.SHA2_64 is
   subtype Word is Interfaces.Unsigned_64;

   package Hash_State is new Hash_Function_State
     (Word           => Word,
      Swap           => GNAT.Byte_Swapping.Swap8,
      Hash_Bit_Order => System.High_Order_First);
   --  SHA-384 and SHA-512 operate on 64-bit big endian words

   K : Hash_State.State (0 .. 79) :=
         (16#428a2f98d728ae22#, 16#7137449123ef65cd#,
          16#b5c0fbcfec4d3b2f#, 16#e9b5dba58189dbbc#,
          16#3956c25bf348b538#, 16#59f111f1b605d019#,
          16#923f82a4af194f9b#, 16#ab1c5ed5da6d8118#,
          16#d807aa98a3030242#, 16#12835b0145706fbe#,
          16#243185be4ee4b28c#, 16#550c7dc3d5ffb4e2#,
          16#72be5d74f27b896f#, 16#80deb1fe3b1696b1#,
          16#9bdc06a725c71235#, 16#c19bf174cf692694#,
          16#e49b69c19ef14ad2#, 16#efbe4786384f25e3#,
          16#0fc19dc68b8cd5b5#, 16#240ca1cc77ac9c65#,
          16#2de92c6f592b0275#, 16#4a7484aa6ea6e483#,
          16#5cb0a9dcbd41fbd4#, 16#76f988da831153b5#,
          16#983e5152ee66dfab#, 16#a831c66d2db43210#,
          16#b00327c898fb213f#, 16#bf597fc7beef0ee4#,
          16#c6e00bf33da88fc2#, 16#d5a79147930aa725#,
          16#06ca6351e003826f#, 16#142929670a0e6e70#,
          16#27b70a8546d22ffc#, 16#2e1b21385c26c926#,
          16#4d2c6dfc5ac42aed#, 16#53380d139d95b3df#,
          16#650a73548baf63de#, 16#766a0abb3c77b2a8#,
          16#81c2c92e47edaee6#, 16#92722c851482353b#,
          16#a2bfe8a14cf10364#, 16#a81a664bbc423001#,
          16#c24b8b70d0f89791#, 16#c76c51a30654be30#,
          16#d192e819d6ef5218#, 16#d69906245565a910#,
          16#f40e35855771202a#, 16#106aa07032bbd1b8#,
          16#19a4c116b8d2d0c8#, 16#1e376c085141ab53#,
          16#2748774cdf8eeb99#, 16#34b0bcb5e19b48a8#,
          16#391c0cb3c5c95a63#, 16#4ed8aa4ae3418acb#,
          16#5b9cca4f7763e373#, 16#682e6ff3d6b2b8a3#,
          16#748f82ee5defb2fc#, 16#78a5636f43172f60#,
          16#84c87814a1f0ab72#, 16#8cc702081a6439ec#,
          16#90befffa23631e28#, 16#a4506cebde82bde9#,
          16#bef9a3f7b2c67915#, 16#c67178f2e372532b#,
          16#ca273eceea26619c#, 16#d186b8c721c0c207#,
          16#eada7dd6cde0eb1e#, 16#f57d4f7fee6ed178#,
          16#06f067aa72176fba#, 16#0a637dc5a2c898a6#,
          16#113f9804bef90dae#, 16#1b710b35131c471b#,
          16#28db77f523047d84#, 16#32caab7b40c72493#,
          16#3c9ebe0a15c9bebc#, 16#431d67c49c100d4c#,
          16#4cc5d4becb3e42b6#, 16#597f299cfc657e2a#,
          16#5fcb6fab3ad6faec#, 16#6c44198c4a475817#);
   --  Constants from FIPS PUB 180-3

   function Sigma0 (X : Word) return Word;
   function Sigma1 (X : Word) return Word;
   function S0 (X : Word) return Word;
   function S1 (X : Word) return Word;
   pragma Inline (Sigma0, Sigma1, S0, S1);
   --  Elementary functions Sigma^512_0, Sigma^512_1, sigma^512_0, sigma^512_1
   --  from FIPS PUB 180-3.

   procedure Transform is new SHA2_Common.Transform
     (Hash_State => Hash_State,
      K          => K,
      Rounds     => 80,
      Sigma0     => Sigma0,
      Sigma1     => Sigma1,
      S0         => S0,
      S1         => S1);

   SHA384_Init_State : constant Hash_State.State :=
                         (0 => 16#cbbb9d5dc1059ed8#,
                          1 => 16#629a292a367cd507#,
                          2 => 16#9159015a3070dd17#,
                          3 => 16#152fecd8f70e5939#,
                          4 => 16#67332667ffc00b31#,
                          5 => 16#8eb44a8768581511#,
                          6 => 16#db0c2e0d64f98fa7#,
                          7 => 16#47b5481dbefa4fa4#);
   SHA512_Init_State : constant Hash_State.State :=
                         (0 => 16#6a09e667f3bcc908#,
                          1 => 16#bb67ae8584caa73b#,
                          2 => 16#3c6ef372fe94f82b#,
                          3 => 16#a54ff53a5f1d36f1#,
                          4 => 16#510e527fade682d1#,
                          5 => 16#9b05688c2b3e6c1f#,
                          6 => 16#1f83d9abfb41bd6b#,
                          7 => 16#5be0cd19137e2179#);
   --  Initialization vectors from FIPS PUB 180-3

end GNAT.Secure_Hashes.SHA2_64;
