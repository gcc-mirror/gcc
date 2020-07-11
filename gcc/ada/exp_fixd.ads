------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ F I X D                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  Expand routines for fixed-point convert, divide and multiply operations

with Types; use Types;

package Exp_Fixd is

   --  General note on universal fixed. In the routines below, a fixed-point
   --  type is always a specific fixed-point type or universal real, never
   --  universal fixed. Universal fixed only appears as the result type of a
   --  division or multiplication and in all such cases, the parent node, which
   --  must be either a conversion node or a 'Round attribute reference node,
   --  has the specific type information. In both cases, the parent node is
   --  removed from the tree, and the appropriate routine in this package is
   --  called with a multiply or divide node with all types (and also possibly
   --  the Rounded_Result flag) set.

   ----------------------------
   -- Fixed-Point Conversion --
   ----------------------------

   procedure Expand_Convert_Fixed_To_Fixed (N : Node_Id);
   --  This routine expands the conversion of one fixed-point type to another,
   --  N is the N_Op_Conversion node with the result and expression types (and
   --  possibly the Rounded_Result flag) set.

   procedure Expand_Convert_Fixed_To_Float (N : Node_Id);
   --  This routine expands the conversion from a fixed-point type to a
   --  floating-point type. N is an N_Type_Conversion node with the result
   --  and expression types set.

   procedure Expand_Convert_Fixed_To_Integer (N : Node_Id);
   --  This routine expands the conversion from a fixed-point type to an
   --  integer type. N is an N_Type_Conversion node with the result and
   --  operand types set.

   procedure Expand_Convert_Float_To_Fixed (N : Node_Id);
   --  This routine expands the conversion from a floating-point type to
   --  a fixed-point type. N is an N_Type_Conversion node with the result
   --  and operand types (and possibly the Rounded_Result flag) set.

   procedure Expand_Convert_Integer_To_Fixed (N : Node_Id);
   --  This routine expands the conversion from an integer type to a
   --  fixed-point type. N is an N_Type_Conversion node with the result
   --  and operand types (and possibly the Rounded_Result flag) set.

   --------------------------
   -- Fixed-Point Division --
   --------------------------

   procedure Expand_Decimal_Divide_Call (N : Node_Id);
   --  This routine expands a call to the procedure Decimal.Divide. The
   --  argument N is the N_Function_Call node.

   procedure Expand_Divide_Fixed_By_Fixed_Giving_Fixed (N : Node_Id);
   --  This routine expands the division between fixed-point types, with
   --  a fixed-point type result. N is an N_Op_Divide node with operand
   --  and result types (and possibly the Rounded_Result flag) set. Either
   --  (but not both) of the operands may be universal real.

   procedure Expand_Divide_Fixed_By_Fixed_Giving_Float (N : Node_Id);
   --  This routine expands the division between two fixed-point types with
   --  a floating-point result. N is an N_Op_Divide node with the result
   --  and operand types set. Either (but not both) of the operands may be
   --  universal real.

   procedure Expand_Divide_Fixed_By_Fixed_Giving_Integer (N : Node_Id);
   --  This routine expands the division between two fixed-point types with
   --  an integer type result. N is an N_Op_Divide node with the result and
   --  operand types set. Either (but not both) of the operands may be
   --  universal real.

   procedure Expand_Divide_Fixed_By_Integer_Giving_Fixed (N : Node_Id);
   --  This routine expands the division between a fixed-point type and
   --  standard integer type. The result type is the same fixed-point type
   --  as the operand type. N is an N_Op_Divide node with the result and
   --  left operand types being the fixed-point type, and the right operand
   --  type being standard integer (and possibly Rounded_Result set).

   --------------------------------
   -- Fixed-Point Multiplication --
   --------------------------------

   procedure Expand_Multiply_Fixed_By_Fixed_Giving_Fixed (N : Node_Id);
   --  This routine expands the multiplication between fixed-point types
   --  with a fixed-point type result. N is an N_Op_Multiply node with the
   --  result and operand types set. Either (but not both) of the operands
   --  may be universal real.

   procedure Expand_Multiply_Fixed_By_Fixed_Giving_Float (N : Node_Id);
   --  This routine expands the multiplication between two fixed-point types
   --  with a floating-point result. N is an N_Op_Multiply node with the
   --  result and operand types set. Either (but not both) of the operands
   --  may be universal real.

   procedure Expand_Multiply_Fixed_By_Fixed_Giving_Integer (N : Node_Id);
   --  This routine expands the multiplication between two fixed-point types
   --  with an integer result. N is an N_Op_Multiply node with the result
   --  and operand types set. Either (but not both) of the operands may be
   --  be universal real.

   procedure Expand_Multiply_Fixed_By_Integer_Giving_Fixed (N : Node_Id);
   --  This routine expands the multiplication between a fixed-point type and
   --  a standard integer type. The result type is the same fixed-point type
   --  as the fixed operand type. N is an N_Op_Multiply node whose result type
   --  and left operand types are the fixed-point type, and whose right operand
   --  type is always standard integer.

   procedure Expand_Multiply_Integer_By_Fixed_Giving_Fixed (N : Node_Id);
   --  This routine expands the multiplication between standard integer and a
   --  fixed-point type. The result type is the same fixed-point type as the
   --  fixed operand type. N is an N_Op_Multiply node whose result type
   --  and right operand types are the fixed-point type, and whose left operand
   --  type is always standard integer.

end Exp_Fixd;
