------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--      S Y S T E M . B O O L E A N _ A R R A Y _ O P E R A T I O N S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2017, Free Software Foundation, Inc.         --
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

--  This package contains functions for runtime operations on boolean arrays

with System.Generic_Vector_Operations;
with System.Vectors.Boolean_Operations;

package System.Boolean_Array_Operations is
   pragma Pure;

   type Boolean_Array is array (Integer range <>) of Boolean;

   package Boolean_Operations renames System.Vectors.Boolean_Operations;

   package Vector_Operations is
      new Generic_Vector_Operations (Boolean, Integer, Boolean_Array);

   generic procedure Binary_Operation
      renames Vector_Operations.Binary_Operation;

   generic procedure Unary_Operation
      renames Vector_Operations.Unary_Operation;

   procedure Vector_Not is
      new Unary_Operation ("not", Boolean_Operations."not");
   procedure Vector_And is new Binary_Operation ("and", System.Vectors."and");
   procedure Vector_Or is new Binary_Operation ("or", System.Vectors."or");
   procedure Vector_Xor is new Binary_Operation ("xor", System.Vectors."xor");

   procedure Vector_Nand is
      new Binary_Operation (Boolean_Operations.Nand, Boolean_Operations.Nand);
   procedure Vector_Nor is
      new Binary_Operation (Boolean_Operations.Nor, Boolean_Operations.Nor);
   procedure Vector_Nxor is
      new Binary_Operation (Boolean_Operations.Nxor, Boolean_Operations.Nxor);
end System.Boolean_Array_Operations;
