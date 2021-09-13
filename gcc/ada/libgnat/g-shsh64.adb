------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           G N A T . S E C U R E _ H A S H E S . S H A 2 _ 6 4            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2009-2021, Free Software Foundation, Inc.        --
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

package body GNAT.Secure_Hashes.SHA2_64 is

   use Interfaces;

   ------------
   -- Sigma0 --
   ------------

   function Sigma0 (X : Word) return Word is
   begin
      return Rotate_Right (X, 28)
         xor Rotate_Right (X, 34)
         xor Rotate_Right (X, 39);
   end Sigma0;

   ------------
   -- Sigma1 --
   ------------

   function Sigma1 (X : Word) return Word is
   begin
      return Rotate_Right (X, 14)
         xor Rotate_Right (X, 18)
         xor Rotate_Right (X, 41);
   end Sigma1;

   --------
   -- S0 --
   --------

   function S0 (X : Word) return Word is
   begin
      return Rotate_Right (X, 1)
         xor Rotate_Right (X, 8)
         xor Shift_Right  (X, 7);
   end S0;

   --------
   -- S1 --
   --------

   function S1 (X : Word) return Word is
   begin
      return Rotate_Right (X, 19)
         xor Rotate_Right (X, 61)
         xor Shift_Right  (X, 6);
   end S1;

end GNAT.Secure_Hashes.SHA2_64;
