------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 4                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  Expand routines for chapter 4 constructs

with Types; use Types;

package Exp_Ch4 is

   procedure Expand_N_Allocator                   (N : Node_Id);
   procedure Expand_N_And_Then                    (N : Node_Id);
   procedure Expand_N_Case_Expression             (N : Node_Id);
   procedure Expand_N_Explicit_Dereference        (N : Node_Id);
   procedure Expand_N_Expression_With_Actions     (N : Node_Id);
   procedure Expand_N_If_Expression               (N : Node_Id);
   procedure Expand_N_In                          (N : Node_Id);
   procedure Expand_N_Indexed_Component           (N : Node_Id);
   procedure Expand_N_Not_In                      (N : Node_Id);
   procedure Expand_N_Null                        (N : Node_Id);
   procedure Expand_N_Op_Abs                      (N : Node_Id);
   procedure Expand_N_Op_Add                      (N : Node_Id);
   procedure Expand_N_Op_And                      (N : Node_Id);
   procedure Expand_N_Op_Concat                   (N : Node_Id);
   procedure Expand_N_Op_Divide                   (N : Node_Id);
   procedure Expand_N_Op_Expon                    (N : Node_Id);
   procedure Expand_N_Op_Eq                       (N : Node_Id);
   procedure Expand_N_Op_Ge                       (N : Node_Id);
   procedure Expand_N_Op_Gt                       (N : Node_Id);
   procedure Expand_N_Op_Le                       (N : Node_Id);
   procedure Expand_N_Op_Lt                       (N : Node_Id);
   procedure Expand_N_Op_Minus                    (N : Node_Id);
   procedure Expand_N_Op_Mod                      (N : Node_Id);
   procedure Expand_N_Op_Multiply                 (N : Node_Id);
   procedure Expand_N_Op_Ne                       (N : Node_Id);
   procedure Expand_N_Op_Not                      (N : Node_Id);
   procedure Expand_N_Op_Or                       (N : Node_Id);
   procedure Expand_N_Op_Plus                     (N : Node_Id);
   procedure Expand_N_Op_Rem                      (N : Node_Id);
   procedure Expand_N_Op_Rotate_Left              (N : Node_Id);
   procedure Expand_N_Op_Rotate_Right             (N : Node_Id);
   procedure Expand_N_Op_Shift_Left               (N : Node_Id);
   procedure Expand_N_Op_Shift_Right              (N : Node_Id);
   procedure Expand_N_Op_Shift_Right_Arithmetic   (N : Node_Id);
   procedure Expand_N_Op_Subtract                 (N : Node_Id);
   procedure Expand_N_Op_Xor                      (N : Node_Id);
   procedure Expand_N_Or_Else                     (N : Node_Id);
   procedure Expand_N_Qualified_Expression        (N : Node_Id);
   procedure Expand_N_Quantified_Expression       (N : Node_Id);
   procedure Expand_N_Selected_Component          (N : Node_Id);
   procedure Expand_N_Slice                       (N : Node_Id);
   procedure Expand_N_Type_Conversion             (N : Node_Id);
   procedure Expand_N_Unchecked_Expression        (N : Node_Id);
   procedure Expand_N_Unchecked_Type_Conversion   (N : Node_Id);

   function Build_Eq_Call
     (Typ : Entity_Id;
      Loc : Source_Ptr;
      Lhs : Node_Id;
      Rhs : Node_Id) return Node_Id;
   --  AI05-0123: Locate primitive equality for type if it exists, and build
   --  the corresponding call. If operation is abstract, replace call with
   --  an explicit raise. Return Empty if there is no primitive.
   --  Used in the construction of record-equality routines for records here
   --  and for variant records in exp_ch3.adb. These two paths are distinct
   --  for historical but also technical reasons: for variant records the
   --  constructed function includes a case statement with nested returns,
   --  while for records without variants only a simple expression is needed.

   function Expand_Record_Equality
     (Nod    : Node_Id;
      Typ    : Entity_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id) return Node_Id;
   --  Expand a record equality into an expression that compares the fields
   --  individually to yield the required Boolean result. Loc is the
   --  location for the generated nodes. Typ is the type of the record, and
   --  Lhs, Rhs are the record expressions to be compared, these
   --  expressions need not to be analyzed but have to be side-effect free.
   --  Bodies is a list on which to attach bodies of local functions that
   --  are created in the process. This is the responsibility of the caller
   --  to insert those bodies at the right place. Nod provides the Sloc
   --  value for generated code.

   procedure Expand_Set_Membership (N : Node_Id);
   --  For each choice of a set membership, we create a simple equality or
   --  membership test. The whole membership is rewritten connecting these
   --  with OR ELSE.

   function Integer_Promotion_Possible (N : Node_Id) return Boolean;
   --  Returns true if the node is a type conversion whose operand is an
   --  arithmetic operation on signed integers, and the base type of the
   --  signed integer type is smaller than Standard.Integer. In such case we
   --  have special circuitry in Expand_N_Type_Conversion to promote both of
   --  the operands to type Integer.

end Exp_Ch4;
