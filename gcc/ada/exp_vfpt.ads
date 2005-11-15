------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ V F P T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains specialized routines for handling the expansion
--  of arithmetic and conversion operations involving Vax format floating-
--  point formats as used on the Vax and the Alpha and the ia64.

with Types; use Types;

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

   procedure Expand_Vax_Real_Literal (N : Node_Id);
   --  The node N is a real literal node where the type is a Vax floating-point
   --  type. This procedure rewrites the node to eliminate the occurrence of
   --  such constants.

   procedure Expand_Vax_Valid (N : Node_Id);
   --  The node N is an attribute reference node for the Valid attribute where
   --  the prefix is of a Vax floating-point type. This procedure expands the
   --  necessary call for the validity test.

end Exp_VFpt;
