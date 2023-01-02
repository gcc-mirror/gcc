------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             S Y S T E M . A D D R E S S _ O P E R A T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
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

--  This package provides arithmetic and logical operations on type Address.
--  It is intended for use by other packages in the System hierarchy. For
--  applications requiring this capability, see System.Storage_Elements or
--  the operations introduced in System.Aux_DEC;

--  The reason we need this package is that arithmetic operations may not
--  be available in the case where type Address is non-private and the
--  operations have been made abstract in the spec of System (to avoid
--  inappropriate use by applications programs). In addition, the logical
--  operations may not be available if type Address is a signed integer.

package System.Address_Operations is
   pragma Pure;

   --  The semantics of the arithmetic operations are those that apply to
   --  a modular type with the same length as Address, i.e. they provide
   --  twos complement wrap around arithmetic treating the address value
   --  as an unsigned value, with no overflow checking.

   --  Note that we do not use the infix names for these operations to
   --  avoid problems with ambiguities coming from declarations in package
   --  Standard (which may or may not be visible depending on the exact
   --  form of the declaration of type System.Address).

   --  For addition, subtraction, and multiplication, the effect of overflow
   --  is 2's complement wrapping (as though the type Address were unsigned).

   --  For division and modulus operations, the caller is responsible for
   --  ensuring that the Right argument is non-zero, and the effect of the
   --  call is not specified if a zero argument is passed.

   function AddA (Left, Right : Address) return Address;
   function SubA (Left, Right : Address) return Address;
   function MulA (Left, Right : Address) return Address;
   function DivA (Left, Right : Address) return Address;
   function ModA (Left, Right : Address) return Address;

   --  The semantics of the logical operations are those that apply to
   --  a modular type with the same length as Address, i.e. they provide
   --  bit-wise operations on all bits of the value (including the sign
   --  bit if Address is a signed integer type).

   function AndA (Left, Right : Address) return Address;
   function OrA  (Left, Right : Address) return Address;

   pragma Inline_Always (AddA);
   pragma Inline_Always (SubA);
   pragma Inline_Always (MulA);
   pragma Inline_Always (DivA);
   pragma Inline_Always (ModA);
   pragma Inline_Always (AndA);
   pragma Inline_Always (OrA);

end System.Address_Operations;
