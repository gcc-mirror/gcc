------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ V F P T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains specialized routines for handling the expansion
--  of arithmetic and conversion operations involving Vax format floating-
--  point formats as used on the Vax and the Alpha and the ia64.

with Types; use Types;
with Uintp; use Uintp;

package Exp_VFpt is

   procedure Expand_Vax_Arith (N : Node_Id);
   --  The node N is an arithmetic node (N_Op_Abs, N_Op_Add, N_Op_Sub,
   --  N_Op_Div, N_Op_Mul, N_Op_Minus where the operands are in Vax float
   --  format. This procedure expands the necessary call.

   procedure Expand_Vax_Comparison (N : Node_Id);
   --  The node N is an arithmetic comparison node where the types to be
   --  compared are in Vax float format. This procedure expands the necessary
   --  call.

   procedure Expand_Vax_Conversion (N : Node_Id);
   --  The node N is a type conversion node where either the source or the
   --  target type, or both, are Vax floating-point type.

   procedure Expand_Vax_Foreign_Return (N : Node_Id);
   --  The node N is a call to a foreign function that returns a Vax float
   --  value in a floating point register. Wraps the call in an asm stub
   --  that moves the return value to an integer location on Alpha/VMS,
   --  noop everywhere else.

   function Get_Vax_Real_Literal_As_Signed (N : Node_Id) return Uint;
   --  Get the Vax binary representation of a real literal whose type is a Vax
   --  floating-point type. This is used by gigi. Previously we expanded real
   --  literal to a call to a LIB$OTS routine that performed the conversion.
   --  This worked correctly from a funcional point of view, but was
   --  inefficient and generated huge functions for aggregate initializations.

   procedure Expand_Vax_Valid (N : Node_Id);
   --  The node N is an attribute reference node for the Valid attribute where
   --  the prefix is of a Vax floating-point type. This procedure expands the
   --  necessary call for the validity test.

end Exp_VFpt;
