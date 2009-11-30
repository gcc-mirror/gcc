------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            S Y S T E M . S E C U R E _ H A S H E S . S H A 1             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2009, Free Software Foundation, Inc.          --
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

--  This package provides supporting code for implementation of the SHA-1
--  secure hash function as decsribed in FIPS PUB 180-3. The complete text
--  of FIPS PUB 180-3 can be found at:
--    http://csrc.nist.gov/publications/fips/fips180-3/fips180-3_final.pdf

with GNAT.Byte_Swapping;
with Interfaces;

package System.Secure_Hashes.SHA1 is

   package Hash_State is new Hash_Function_State
     (Word           => Interfaces.Unsigned_32,
      Swap           => GNAT.Byte_Swapping.Swap4,
      Hash_Bit_Order => System.High_Order_First);
   --  SHA-1 operates on 32-bit big endian words

   Block_Words : constant := 16;
   --  Messages are processed in chunks of 16 words

   procedure Transform
     (H : in out Hash_State.State;
      M : in out Message_State);
   --  Transformation function applied for each block

   Initial_State : constant Hash_State.State;
   --  Initialization vector

private

   Initial_State : constant Hash_State.State :=
                     (0 => 16#67452301#,
                      1 => 16#EFCDAB89#,
                      2 => 16#98BADCFE#,
                      3 => 16#10325476#,
                      4 => 16#C3D2E1F0#);
   --  Initialization vector from FIPS PUB 180-3

end System.Secure_Hashes.SHA1;
