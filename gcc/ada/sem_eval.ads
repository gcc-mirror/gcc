------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E V A L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains various subprograms involved in compile time
--  evaluation of expressions and checks for staticness of expressions
--  and types. It also contains the circuitry for checking for violations
--  of pure and preelaborated conditions (this naturally goes here, since
--  these rules involve consideration of staticness).

--  Note: the static evaluation for attributes is found in Sem_Attr even
--  though logically it belongs here. We have done this so that it is easier
--  to add new attributes to GNAT.

with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;

package Sem_Eval is

   ------------------------------------
   -- Handling of Static Expressions --
   ------------------------------------

   --  This package contains a set of routine that process individual
   --  subexpression nodes with the objective of folding (precomputing)
   --  the value of static expressions that are known at compile time and
   --  properly computing the setting of two flags that appear in every
   --  subexpression node:

   --    Is_Static_Expression

   --      This flag is set on any expression that is static according
   --      to the rules in (RM 4.9(3-32)).

   --    Raises_Constraint_Error

   --      This flag indicatest that it is known at compile time that the
   --      evaluation of an expression raises constraint error. If the
   --      expression is static, and this flag is off, then it is also known
   --      at compile time that the expression does not raise constraint error
   --      (i.e. the flag is accurate for static expressions, and conservative
   --      for non-static expressions.

   --  If a static expression does not raise constraint error, then the
   --  Raises_Constraint_Error flag is off, and the expression must be
   --  computed at compile time, which means that it has the form of either
   --  a literal, or a constant that is itself (recursively) either a literal
   --  or a constant.

   --  The above rules must be followed exactly in order for legality
   --  checks to be accurate. For subexpressions that are not static
   --  according to the RM definition, they are sometimes folded anyway,
   --  but of course in this case Is_Static_Expression is not set.

   -------------------------------
   -- Compile-Time Known Values --
   -------------------------------

   --  For most legality checking purposes the flag Is_Static_Expression
   --  defined in Sinfo should be used. This package also provides
   --  a routine called Is_OK_Static_Expression which in addition of
   --  checking that an expression is static in the RM 4.9 sense, it
   --  checks that the expression does not raise constraint error. In
   --  fact for certain legality checks not only do we need to ascertain
   --  that the expression is static, but we must also ensure that it
   --  does not raise constraint error.
   --
   --  Neither of Is_Static_Expression and Is_OK_Static_Expression should
   --  be used for compile time evaluation purposes. In fact certain
   --  expression whose value is known at compile time are not static
   --  in the RM 4.9 sense. A typical example is:
   --
   --     C : constant Integer := Record_Type'Size;
   --
   --  The expression 'C' is not static in the technical RM sense, but for
   --  many simple record types, the size is in fact known at compile time.
   --  When we are trying to perform compile time constant folding (for
   --  instance for expressions such as 'C + 1', Is_Static_Expression or
   --  Is_OK_Static_Expression are not the right functions to test to see
   --  if folding is possible. Instead, we use Compile_Time_Know_Value.
   --  All static expressions that do not raise constraint error (i.e.
   --  those for which Is_OK_Static_Expression is true) are known at
   --  compile time, but as shown by the above example, there are cases
   --  of non-static expressions which are known at compile time.

   -----------------
   -- Subprograms --
   -----------------

   procedure Check_Non_Static_Context (N : Node_Id);
   --  Deals with the special check required for a static expression that
   --  appears in a non-static context, i.e. is not part of a larger static
   --  expression (see RM 4.9(35)), i.e. the value of the expression must be
   --  within the base range of the base type of its expected type. A check
   --  is also made for expressions that are inside the base range, but
   --  outside the range of the expected subtype (this is a warning message
   --  rather than an illegality).
   --
   --  Note: most cases of non-static context checks are handled within
   --  Sem_Eval itself, including all cases of expressions at the outer
   --  level (i.e. those that are not a subexpression). Currently the only
   --  outside customer for this procedure is Sem_Attr (because Eval_Attribute
   --  is there). There is also one special case arising from ranges (see body
   --  of Resolve_Range).

   procedure Check_String_Literal_Length (N : Node_Id; Ttype : Entity_Id);
   --  N is either a string literal, or a constraint error node. In the latter
   --  case, the situation is already dealt with, and the call has no effect.
   --  In the former case, if the target type, Ttyp is constrained, then a
   --  check is made to see if the string literal is of appropriate length.

   type Compare_Result is (LT, LE, EQ, GT, GE, NE, Unknown);
   subtype Compare_GE is Compare_Result range EQ .. GE;
   subtype Compare_LE is Compare_Result range LT .. EQ;
   function Compile_Time_Compare (L, R : Node_Id) return Compare_Result;
   --  Given two expression nodes, finds out whether it can be determined
   --  at compile time how the runtime values will compare. An Unknown
   --  result means that the result of a comparison cannot be determined at
   --  compile time, otherwise the returned result indicates the known result
   --  of the comparison, given as tightly as possible (i.e. EQ or LT is a
   --  preferred returned value to LE).

   function Is_OK_Static_Expression (N : Node_Id) return Boolean;
   --  An OK static expression is one that is static in the RM definition
   --  sense and which does not raise constraint error. For most legality
   --  checking purposes you should use Is_Static_Expression. For those
   --  legality checks where the expression N should not raise constaint
   --  error use this routine. This routine is *not* to be used in contexts
   --  where the test is for compile time evaluation purposes. Use routine
   --  Compile_Time_Known_Value instead (see section on "Compile-Time Known
   --  Values" above).

   function Is_Static_Range (N : Node_Id) return Boolean;
   --  Determine if range is static, as defined in RM 4.9(26). The only
   --  allowed argument is an N_Range node (but note that the semantic
   --  analysis of equivalent range attribute references already turned
   --  them into the equivalent range).

   function Is_OK_Static_Range (N : Node_Id) return Boolean;
   --  Like Is_Static_Range, but also makes sure that the bounds of the
   --  range are compile-time evaluable (i.e. do not raise constraint error).
   --  A result of true means that the bounds are compile time evaluable.
   --  A result of false means they are not (either because the range is
   --  not static, or because one or the other bound raises CE).

   function Is_Static_Subtype (Typ : Entity_Id) return Boolean;
   --  Determines whether a subtype fits the definition of an Ada static
   --  subtype as given in (RM 4.9(26)).

   function Is_OK_Static_Subtype (Typ : Entity_Id) return Boolean;
   --  Like Is_Static_Subtype but also makes sure that the bounds of the
   --  subtype are compile-time evaluable (i.e. do not raise constraint
   --  error). A result of true means that the bounds are compile time
   --  evaluable. A result of false means they are not (either because the
   --  range is not static, or because one or the other bound raises CE).

   function Subtypes_Statically_Compatible
     (T1   : Entity_Id;
      T2   : Entity_Id)
      return Boolean;
   --  Returns true if the subtypes are unconstrained or the constraint on
   --  on T1 is statically compatible with T2 (as defined by 4.9.1(4)).
   --  Otherwise returns false.

   function Subtypes_Statically_Match (T1, T2 : Entity_Id) return Boolean;
   --  Determine whether two types T1, T2, which have the same base type,
   --  are statically matching subtypes (RM 4.9.1(1-2)).

   function Compile_Time_Known_Value (Op : Node_Id) return Boolean;
   --  Returns true if Op is an expression not raising constraint error
   --  whose value is known at compile time. This is true if Op is a static
   --  expression, but can also be true for expressions which are
   --  technically non-static but which are in fact known at compile time,
   --  such as the static lower bound of a non-static range or the value
   --  of a constant object whose initial value is static. Note that this
   --  routine is defended against unanalyzed expressions. Such expressions
   --  will not cause a blowup, they may cause pessimistic (i.e. False)
   --  results to be returned.

   function Compile_Time_Known_Value_Or_Aggr (Op : Node_Id) return Boolean;
   --  Similar to Compile_Time_Known_Value, but also returns True if the
   --  value is a compile time known aggregate, i.e. an aggregate all of
   --  whose constituent expressions are either compile time known values
   --  or compile time known aggregates.

   function Expr_Value (N : Node_Id) return Uint;
   --  Returns the folded value of the expression N. This function is called
   --  in instances where it has already been determined that the expression
   --  is static or its value is known at compile time (ie the call to
   --  Compile_Time_Known_Value (N) returns True). This version is used for
   --  integer values, and enumeration or character literals. In the latter
   --  two cases, the value returned is the Pos value in the relevant
   --  enumeration type. It can also be used for fixed-point values, in
   --  which case it returns the corresponding integer value. It cannot be
   --  used for floating-point values.

   function Expr_Value_E (N : Node_Id) return Entity_Id;
   --  Returns the folded value of the expression. This function is called
   --  in instances where it has already been determined that the expression
   --  is static or its value known at compile time. This version is used
   --  for enumeration types and returns the corresponding enumeration
   --  literal.

   function Expr_Value_R (N : Node_Id) return Ureal;
   --  Returns the folded value of the expression. This function is called
   --  in instances where it has already been determined that the expression
   --  is static or its value known at compile time. This version is used
   --  for real values (including both the floating-point and fixed-point
   --  cases). In the case of a fixed-point type, the real value is returned
   --  (cf above version returning Uint).

   function Expr_Value_S (N : Node_Id) return Node_Id;
   --  Returns the folded value of the expression. This function is called
   --  in instances where it has already been determined that the expression
   --  is static or its value is known at compile time. This version is used
   --  for string types and returns the corresponding N_String_Literal node.

   function Expr_Rep_Value (N : Node_Id) return Uint;
   --  This is identical to Expr_Value, except in the case of enumeration
   --  literals of types for which an enumeration representation clause has
   --  been given, in which case it returns the representation value rather
   --  than the pos value. This is the value that is needed for generating
   --  code sequences, while the Expr_Value value is appropriate for compile
   --  time constraint errors or getting the logical value. Note that this
   --  function does NOT concern itself with biased values, if the caller
   --  needs a properly biased value, the subtraction of the bias must be
   --  handled explicitly.

   procedure Eval_Actual                 (N : Node_Id);
   procedure Eval_Allocator              (N : Node_Id);
   procedure Eval_Arithmetic_Op          (N : Node_Id);
   procedure Eval_Character_Literal      (N : Node_Id);
   procedure Eval_Concatenation          (N : Node_Id);
   procedure Eval_Conditional_Expression (N : Node_Id);
   procedure Eval_Entity_Name            (N : Node_Id);
   procedure Eval_Indexed_Component      (N : Node_Id);
   procedure Eval_Integer_Literal        (N : Node_Id);
   procedure Eval_Logical_Op             (N : Node_Id);
   procedure Eval_Membership_Op          (N : Node_Id);
   procedure Eval_Named_Integer          (N : Node_Id);
   procedure Eval_Named_Real             (N : Node_Id);
   procedure Eval_Op_Expon               (N : Node_Id);
   procedure Eval_Op_Not                 (N : Node_Id);
   procedure Eval_Real_Literal           (N : Node_Id);
   procedure Eval_Relational_Op          (N : Node_Id);
   procedure Eval_Shift                  (N : Node_Id);
   procedure Eval_Short_Circuit          (N : Node_Id);
   procedure Eval_Slice                  (N : Node_Id);
   procedure Eval_String_Literal         (N : Node_Id);
   procedure Eval_Qualified_Expression   (N : Node_Id);
   procedure Eval_Type_Conversion        (N : Node_Id);
   procedure Eval_Unary_Op               (N : Node_Id);
   procedure Eval_Unchecked_Conversion   (N : Node_Id);

   procedure Fold_Str (N : Node_Id; Val : String_Id);
   --  Rewrite N with a new N_String_Literal node as the result of the
   --  compile time evaluation of the node N. Val is the resulting string
   --  value from the folding operation. The Is_Static_Expression flag is
   --  set in the result node. The result is fully analyzed and resolved.

   procedure Fold_Uint (N : Node_Id; Val : Uint);
   --  Rewrite N with a (N_Integer_Literal, N_Identifier, N_Character_Literal)
   --  node as the result of the compile time evaluation of the node N. Val
   --  is the result in the integer case and is the position of the literal
   --  in the literals list for the enumeration case. Is_Static_Expression
   --  is set True in the result node. The result is fully analyzed/resolved.

   procedure Fold_Ureal (N : Node_Id; Val : Ureal);
   --  Rewrite N with a new N_Real_Literal node as the result of the compile
   --  time evaluation of the node N. Val is the resulting real value from
   --  the folding operation. The Is_Static_Expression flag is set in the
   --  result node. The result is fully analyzed and result.

   function Is_In_Range
     (N         : Node_Id;
      Typ       : Entity_Id;
      Fixed_Int : Boolean := False;
      Int_Real  : Boolean := False)
      return      Boolean;
   --  Returns True if it can be guaranteed at compile time that expression
   --  N is known to be in range of the subtype Typ. If the values of N or
   --  of either bouds of Type are unknown at compile time, False will
   --  always be returned. A result of False does not mean that the
   --  expression is out of range, merely that it cannot be determined at
   --  compile time that it is in range. If Typ is a floating point type or
   --  Int_Real is set, any integer value is treated as though it was a real
   --  value (i.e. the underlying real value is used).  In this case we use
   --  the corresponding real value, both for the bounds of Typ, and for the
   --  value of the expression N. If Typ is a fixed type or a discrete type
   --  and Int_Real is False but flag Fixed_Int is True then any fixed-point
   --  value is treated as though it was a discrete value (i.e. the
   --  underlying integer value is used).  In this case we use the
   --  corresponding integer value, both for the bounds of Typ, and for the
   --  value of the expression N. If Typ is a discret type and Fixed_Int as
   --  well as Int_Real are false, intere values are used throughout.

   function Is_Out_Of_Range
     (N         : Node_Id;
      Typ       : Entity_Id;
      Fixed_Int : Boolean := False;
      Int_Real  : Boolean := False)
      return      Boolean;
   --  Returns True if it can be guaranteed at compile time that expression
   --  N is known to be out of range of the subtype Typ.  True is returned
   --  if Typ is a scalar type, at least one of whose bounds is known at
   --  compile time, and N is a compile time known expression which can be
   --  determined to be outside a compile_time known bound of Typ. A result
   --  of False does not mean that the expression is in range, merely that
   --  it cannot be determined at compile time that it is out of range. Flags
   --  Int_Real and Fixed_Int are used like in routine Is_In_Range above.

   function In_Subrange_Of
     (T1        : Entity_Id;
      T2        : Entity_Id;
      Fixed_Int : Boolean := False)
      return      Boolean;
   --  Returns True if it can be guaranteed at compile time that the range
   --  of values for scalar type T1 are always in the range of scalar type
   --  T2.  A result of False does not mean that T1 is not in T2's subrange,
   --  only that it cannot be determined at compile time. Flag Fixed_Int is
   --  used is like in routine Is_In_Range_Above.

   function Is_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean;
   --  Returns True if it can guarantee that Lo .. Hi is a null range.
   --  If it cannot (because the value of Lo or Hi is not known at compile
   --  time) then it returns False.

   function Not_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean;
   --  Returns True if it can guarantee that Lo .. Hi is not a null range.
   --  If it cannot (because the value of Lo or Hi is not known at compile
   --  time) then it returns False.

private
   --  The Eval routines are all marked inline, since they are called once

   pragma Inline (Eval_Actual);
   pragma Inline (Eval_Allocator);
   pragma Inline (Eval_Character_Literal);
   pragma Inline (Eval_Conditional_Expression);
   pragma Inline (Eval_Indexed_Component);
   pragma Inline (Eval_Integer_Literal);
   pragma Inline (Eval_Named_Integer);
   pragma Inline (Eval_Named_Real);
   pragma Inline (Eval_Real_Literal);
   pragma Inline (Eval_Shift);
   pragma Inline (Eval_Slice);
   pragma Inline (Eval_String_Literal);
   pragma Inline (Eval_Unchecked_Conversion);

   pragma Inline (Is_OK_Static_Expression);

end Sem_Eval;
