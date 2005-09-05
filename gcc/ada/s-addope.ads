------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             S Y S T E M . A D D R E S S _ O P E R A T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2004-2005 Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the implementation dependent sections of this file.      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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
