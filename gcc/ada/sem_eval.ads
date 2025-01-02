------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E V A L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This package contains various subprograms involved in compile time
--  evaluation of expressions and checks for staticness of expressions and
--  types. It also contains the circuitry for checking for violations of pure
--  and preelaborated conditions (this naturally goes here, since these rules
--  involve consideration of staticness).

--  Note: the static evaluation for attributes is found in Sem_Attr even though
--  logically it belongs here. We have done this so that it is easier to add
--  new attributes to GNAT.

with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;

package Sem_Eval is

   ------------------------------------
   -- Handling of Static Expressions --
   ------------------------------------

   --  This package contains a set of routines that process individual
   --  subexpression nodes with the objective of folding (precomputing) the
   --  value of static expressions that are known at compile time and properly
   --  computing the setting of two flags that appear in every subexpression
   --  node:

   --    Is_Static_Expression

   --      True for static expressions, as defined in RM-4.9.

   --    Raises_Constraint_Error

   --      This flag indicates that it is known at compile time that the
   --      evaluation of an expression raises constraint error. If the
   --      expression is static, and this flag is off, then it is also known at
   --      compile time that the expression does not raise constraint error
   --      (i.e. the flag is accurate for static expressions, and conservative
   --      for non-static expressions.

   --  See also Is_OK_Static_Expression, which is True for static
   --  expressions that do not raise Constraint_Error. This is used in most
   --  legality checks, because static expressions that raise Constraint_Error
   --  are usually illegal.

   --  See also Compile_Time_Known_Value, which is True for an expression whose
   --  value is known at compile time. In this case, the expression is folded
   --  to a literal or to a constant that is itself (recursively) either a
   --  literal or a constant

   --  Is_[OK_]Static_Expression are used for legality checks, whereas
   --  Compile_Time_Known_Value is used for optimization purposes.

   --  When we are analyzing and evaluating static expressions, we propagate
   --  both flags. Usually if a subexpression raises a Constraint_Error, then
   --  so will its parent expression, and Raise_Constraint_Error will be
   --  propagated to this parent. The exception is conditional cases like
   --  (True or else 1/0 = 0), which results in an expression that has the
   --  Is_Static_Expression flag True, and Raises_Constraint_Error False. Even
   --  though 1/0 would raise an exception, the right operand is never actually
   --  executed, so the expression as a whole does not raise CE.

   --  Finally, the case of static predicates. These are applied only to entire
   --  expressions, not to subexpressions, so we do not have the case of having
   --  to propagate this information. We handle this case simply by resetting
   --  the Is_Static_Expression flag if a static predicate fails. Note that we
   --  can't use this simpler approach for the constraint error case because of
   --  the (True or else 1/0 = 0) example discussed above.

   -------------------------------
   -- Compile-Time Known Values --
   -------------------------------

   --  For most legality checking purposes the flag Is_Static_Expression
   --  defined in Sinfo should be used. This package also provides a routine
   --  called Is_OK_Static_Expression which in addition of checking that an
   --  expression is static in the RM 4.9 sense, it checks that the expression
   --  does not raise constraint error. In fact for certain legality checks not
   --  only do we need to ascertain that the expression is static, but we must
   --  also ensure that it does not raise constraint error.

   --  Neither of Is_Static_Expression and Is_OK_Static_Expression should be
   --  used for compile time evaluation purposes. In fact certain expression
   --  whose value may be known at compile time are not static in the RM 4.9
   --  sense. A typical example is:

   --     C : constant Integer := Record_Type'Size;

   --  The expression 'C' is not static in the technical RM sense, but for many
   --  simple record types, the size is in fact known at compile time. When we
   --  are trying to perform compile time constant folding (for instance for
   --  expressions like C + 1), Is_Static_Expression or Is_OK_Static_Expression
   --  are not the right functions to test if folding is possible. Instead, we
   --  use Compile_Time_Known_Value. All static expressions that do not raise
   --  constraint error (i.e. those for which Is_OK_Static_Expression is true)
   --  are known at compile time, but as shown by the above example, there may
   --  be cases of non-static expressions which are known at compile time.

   -----------------
   -- Subprograms --
   -----------------

   procedure Check_Expression_Against_Static_Predicate
     (Expr                    : Node_Id;
      Typ                     : Entity_Id;
      Static_Failure_Is_Error : Boolean := False);
   --  Determine whether an arbitrary expression satisfies the static predicate
   --  of a type. The routine does nothing if Expr is not known at compile time
   --  or Typ lacks a static predicate; otherwise it may emit a warning if the
   --  expression is prohibited by the predicate, or if Static_Failure_Is_Error
   --  is True then an error will be flagged. If the expression is a static
   --  expression, it fails a predicate that was not explicitly stated to be
   --  a dynamic predicate, and Static_Failure_Is_Error is False, then an
   --  additional warning is given, and the flag Is_Static_Expression is reset
   --  on Expr.

   procedure Check_Non_Static_Context (N : Node_Id);
   --  Deals with the special check required for a static expression that
   --  appears in a non-static context, i.e. is not part of a larger static
   --  expression (see RM 4.9(35)), i.e. the value of the expression must be
   --  within the base range of the base type of its expected type. A check is
   --  also made for expressions that are inside the base range, but outside
   --  the range of the expected subtype (this is a warning message rather than
   --  an illegality).
   --
   --  Note: most cases of non-static context checks are handled within
   --  Sem_Eval itself, including all cases of expressions at the outer level
   --  (i.e. those that are not a subexpression). The outside customers for
   --  this procedure are Sem_Aggr, Sem_Attr (because Eval_Attribute is there)
   --  and Sem_Res (for a special case arising from ranges, see Resolve_Range).
   --
   --  Note: this procedure is also called by GNATprove on real literals
   --  that are not sub-expressions of static expressions, to convert them to
   --  machine numbers, as GNATprove cannot perform this conversion contrary
   --  to gigi.

   procedure Check_String_Literal_Length (N : Node_Id; Ttype : Entity_Id);
   --  N is either a string literal, or a constraint error node. In the latter
   --  case, the situation is already dealt with, and the call has no effect.
   --  In the former case, if the target type, Ttyp is constrained, then a
   --  check is made to see if the string literal is of appropriate length.

   function Checking_Potentially_Static_Expression return Boolean;
   --  Returns True if the checking for potentially static expressions is
   --  enabled; otherwise returns False.

   procedure Set_Checking_Potentially_Static_Expression (Value : Boolean);
   --  Enables checking for potentially static expressions if Value is True,
   --  and disables such checking if Value is False.

   type Compare_Result is (LT, LE, EQ, GT, GE, NE, Unknown);
   subtype Compare_GE is Compare_Result range EQ .. GE;
   subtype Compare_LE is Compare_Result range LT .. EQ;
   --  Result subtypes for Compile_Time_Compare subprograms

   function Compile_Time_Compare
     (L, R         : Node_Id;
      Assume_Valid : Boolean) return Compare_Result;
   pragma Inline (Compile_Time_Compare);
   --  Given two expression nodes, finds out whether it can be determined at
   --  compile time how the runtime values will compare. An Unknown result
   --  means that the result of a comparison cannot be determined at compile
   --  time, otherwise the returned result indicates the known result of the
   --  comparison, given as tightly as possible (i.e. EQ or LT is preferred
   --  returned value to LE). If Assume_Valid is true, the result reflects
   --  the result of assuming that entities involved in the comparison have
   --  valid representations. If Assume_Valid is false, then the base type of
   --  any involved entity is used so that no assumption of validity is made.

   function Compile_Time_Compare
     (L, R         : Node_Id;
      Diff         : access Uint;
      Assume_Valid : Boolean;
      Rec          : Boolean := False) return Compare_Result;
   --  This version of Compile_Time_Compare returns extra information if the
   --  result is GT or LT. In these cases, if the magnitude of the difference
   --  can be determined at compile time, this (positive) magnitude is returned
   --  in Diff.all. If the magnitude of the difference cannot be determined
   --  then Diff.all contains No_Uint on return. Rec is a parameter that is set
   --  True for a recursive call from within Compile_Time_Compare to avoid some
   --  infinite recursion cases. It should never be set by a client.

   function Compile_Time_Known_Bounds (T : Entity_Id) return Boolean;
   --  If T is an array whose index bounds are all known at compile time, then
   --  True is returned. If T is not an array type, or one or more of its index
   --  bounds is not known at compile time, then False is returned.

   function Compile_Time_Known_Value (Op : Node_Id) return Boolean;
   --  Returns true if Op is an expression not raising Constraint_Error whose
   --  value is known at compile time and for which a call to Expr_Value can
   --  be used to determine this value. This is always true if Op is a static
   --  expression, but can also be true for expressions which are technically
   --  non-static but which are in fact known at compile time. Some examples of
   --  such expressions are the static lower bound of a non-static range or the
   --  value of a constant object whose initial value is itself compile time
   --  known in the sense of this routine. Note that this routine is defended
   --  against unanalyzed expressions. Such expressions will not cause a
   --  blowup, they may cause pessimistic (i.e. False) results to be returned.
   --  In general we take a pessimistic view. False does not mean the value
   --  could not be known at compile time, but True means that absolutely
   --  definition it is known at compile time and it is safe to call
   --  Expr_Value[_XX] on the expression Op.
   --
   --  Note that we don't define precisely the set of expressions that return
   --  True. Callers should not make any assumptions regarding the value that
   --  is returned for non-static expressions. Functional behavior should never
   --  be affected by whether a given non-static expression returns True or
   --  False when this function is called. In other words this is purely for
   --  efficiency optimization purposes. The code generated can often be more
   --  efficient with compile time known values, e.g. range analysis for the
   --  purpose of removing checks is more effective if we know precise bounds.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function CRT_Safe_Compile_Time_Known_Value (Op : Node_Id) return Boolean;
   --  In the case of configurable run-times, there may be an issue calling
   --  Compile_Time_Known_Value with non-static expressions where the legality
   --  of the program is not well-defined. Consider this example:
   --
   --    X := B ** C;
   --
   --  Now if C is compile time known, and has the value 4, then inline code
   --  can be generated at compile time, instead of calling a run-time routine.
   --  That's fine in the normal case, but when we have a configurable run-time
   --  the run-time routine may not be available. This means that the program
   --  will be rejected if C is not known at compile time. We don't want the
   --  legality of a program to depend on how clever the implementation of this
   --  function is. If the run-time in use lacks the exponentiation routine,
   --  then what we say is that exponentiation is permitted if the exponent is
   --  officially static and has a value in the range 0 .. 4.
   --
   --  In a case like this, we use CRT_Safe_Compile_Time_Known_Value to avoid
   --  this effect. This routine will return False for a non-static expression
   --  if we are in configurable run-time mode, even if the expression would
   --  normally be considered compile-time known.

   function Expr_Rep_Value (N : Node_Id) return Uint;
   --  This is identical to Expr_Value, except in the case of enumeration
   --  literals of types for which an enumeration representation clause has
   --  been given, in which case it returns the representation value rather
   --  than the pos value. This is the value that is needed for generating code
   --  sequences, while the Expr_Value value is appropriate for compile time
   --  constraint errors or getting the logical value. Note that this function
   --  does NOT concern itself with biased values, if the caller needs a
   --  properly biased value, the subtraction of the bias must be handled
   --  explicitly.

   function Expr_Value (N : Node_Id) return Uint;
   --  Returns the folded value of the expression N. This function is called in
   --  instances where it has already been determined that the expression is
   --  static or its value is compile time known (Compile_Time_Known_Value (N)
   --  returns True). This version is used for integer values, and enumeration
   --  or character literals. In the latter two cases, the value returned is
   --  the Pos value in the relevant enumeration type. It can also be used for
   --  fixed-point values, in which case it returns the corresponding integer
   --  value, but it cannot be used for floating-point values. Finally, it can
   --  also be used for the Null access value, as well as for the result of an
   --  unchecked conversion of the aforementioned handled values.

   function Expr_Value_E (N : Node_Id) return Entity_Id;
   --  Returns the folded value of the expression. This function is called in
   --  instances where it has already been determined that the expression is
   --  static or its value known at compile time. This version is used for
   --  enumeration types and returns the corresponding enumeration literal.

   function Expr_Value_R (N : Node_Id) return Ureal;
   --  Returns the folded value of the expression. This function is called in
   --  instances where it has already been determined that the expression is
   --  static or its value known at compile time. This version is used for real
   --  values (including both the floating-point and fixed-point cases). In the
   --  case of a fixed-point type, the real value is returned (cf above version
   --  returning Uint).

   function Expr_Value_S (N : Node_Id) return Node_Id;
   --  Returns the folded value of the expression. This function is called
   --  in instances where it has already been determined that the expression
   --  is static or its value is known at compile time. This version is used
   --  for string types and returns the corresponding N_String_Literal node.

   procedure Eval_Actual                 (N : Node_Id);
   procedure Eval_Allocator              (N : Node_Id);
   procedure Eval_Arithmetic_Op          (N : Node_Id);
   procedure Eval_Call                   (N : Node_Id);
   procedure Eval_Case_Expression        (N : Node_Id);
   procedure Eval_Character_Literal      (N : Node_Id);
   procedure Eval_Concatenation          (N : Node_Id);
   procedure Eval_Entity_Name            (N : Node_Id);
   procedure Eval_If_Expression          (N : Node_Id);
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
   procedure Eval_Selected_Component     (N : Node_Id);
   procedure Eval_Shift                  (N : Node_Id);
   procedure Eval_Short_Circuit          (N : Node_Id);
   procedure Eval_Slice                  (N : Node_Id);
   procedure Eval_String_Literal         (N : Node_Id);
   procedure Eval_Qualified_Expression   (N : Node_Id);
   procedure Eval_Type_Conversion        (N : Node_Id);
   procedure Eval_Unary_Op               (N : Node_Id);
   procedure Eval_Unchecked_Conversion   (N : Node_Id);

   procedure Flag_Non_Static_Expr (Msg : String; Expr : Node_Id);
   --  This procedure is called after it has been determined that Expr is not
   --  static when it is required to be. Msg is the text of a message that
   --  explains the error. This procedure checks if an error is already posted
   --  on Expr, if so, it does nothing unless All_Errors_Mode is set in which
   --  case this flag is ignored. Otherwise the given message is posted using
   --  Error_Msg_F, and then Why_Not_Static is called on Expr to generate
   --  additional messages. The string given as Msg should end with ! to make
   --  it an unconditional message, to ensure that if it is posted, the entire
   --  set of messages is all posted.

   procedure Fold_Str (N : Node_Id; Val : String_Id; Static : Boolean);
   --  Rewrite N with a new N_String_Literal node as the result of the compile
   --  time evaluation of the node N. Val is the resulting string value from
   --  the folding operation. The Is_Static_Expression flag is set in the
   --  result node. The result is fully analyzed and resolved. Static indicates
   --  whether the result should be considered static or not (True = consider
   --  static). The point here is that normally all string literals are static,
   --  but if this was the result of some sequence of evaluation where values
   --  were known at compile time but not static, then the result is not
   --  static. The call has no effect if Raises_Constraint_Error (N) is True,
   --  since there is no point in folding if we have an error.

   procedure Fold_Uint (N : Node_Id; Val : Uint; Static : Boolean);
   --  Rewrite N with a (N_Integer_Literal, N_Identifier, N_Character_Literal)
   --  node as the result of the compile time evaluation of the node N. Val is
   --  the result in the integer case and is the position of the literal in the
   --  literals list for the enumeration case. Is_Static_Expression is set True
   --  in the result node. The result is fully analyzed/resolved. Static
   --  indicates whether the result should be considered static or not (True =
   --  consider static). The point here is that normally all integer literals
   --  are static, but if this was the result of some sequence of evaluation
   --  where values were known at compile time but not static, then the result
   --  is not static. The call has no effect if Raises_Constraint_Error (N) is
   --  True, since there is no point in folding if we have an error.

   procedure Fold_Ureal (N : Node_Id; Val : Ureal; Static : Boolean);
   --  Rewrite N with a new N_Real_Literal node as the result of the compile
   --  time evaluation of the node N. Val is the resulting real value from the
   --  folding operation. The Is_Static_Expression flag is set in the result
   --  node. The result is fully analyzed and result. Static indicates whether
   --  the result should be considered static or not (True = consider static).
   --  The point here is that normally all string literals are static, but if
   --  this was the result of some sequence of evaluation where values were
   --  known at compile time but not static, then the result is not static.
   --  The call has no effect if Raises_Constraint_Error (N) is True, since
   --  there is no point in folding if we have an error.

   procedure Fold (N : Node_Id);
   --  Rewrite N with the relevant value if Compile_Time_Known_Value (N) is
   --  True, otherwise a no-op.

   function Is_In_Range
     (N            : Node_Id;
      Typ          : Entity_Id;
      Assume_Valid : Boolean := False;
      Fixed_Int    : Boolean := False;
      Int_Real     : Boolean := False) return Boolean;
   --  Returns True if it can be guaranteed at compile time that expression
   --  N is known to be in range of the subtype Typ. A result of False does
   --  not mean that the expression is out of range, merely that it cannot be
   --  determined at compile time that it is in range. If Typ is a floating
   --  point type or Int_Real is set, any integer value is treated as though it
   --  was a real value (i.e. the underlying real value is used). In this case
   --  we use the corresponding real value, both for the bounds of Typ, and for
   --  the value of the expression N. If Typ is a fixed type or a discrete type
   --  and Int_Real is False but flag Fixed_Int is True then any fixed-point
   --  value is treated as though it was discrete value (i.e. the underlying
   --  integer value is used). In this case we use the corresponding integer
   --  value, both for the bounds of Typ, and for the value of the expression
   --  N. If Typ is a discrete type and Fixed_Int as well as Int_Real are
   --  false, integer values are used throughout.
   --
   --  If Assume_Valid is set True, then N is always assumed to contain a valid
   --  value. If Assume_Valid is set False, then N may be invalid (unless there
   --  is some independent way of knowing that it is valid, i.e. either it is
   --  an entity with Is_Known_Valid set, or Assume_No_Invalid_Values is True.

   function Is_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean;
   --  Returns True if it can guarantee that Lo .. Hi is a null range

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   function Is_OK_Static_Expression (N : Node_Id) return Boolean;
   --  An OK static expression is one that is static in the RM definition sense
   --  and which does not raise constraint error. For most legality checking
   --  purposes you should use Is_Static_Expression. For those legality checks
   --  where the expression N should not raise constraint error use this
   --  routine. This routine is *not* to be used in contexts where the test is
   --  for compile time evaluation purposes. Use Compile_Time_Known_Value
   --  instead (see section on "Compile-Time Known Values" above).

   type Staticity is (Static, Not_Static, Invalid);

   function Is_OK_Static_Expression_Of_Type
     (Expr : Node_Id; Typ : Entity_Id := Empty) return Staticity;
   --  Return whether Expr is a static expression of the given type (i.e. it
   --  will be analyzed and resolved using this type, which can be any valid
   --  argument to Resolve, e.g. Any_Integer is OK). Includes checking that the
   --  expression does not raise Constraint_Error.

   function Is_OK_Static_Range (N : Node_Id) return Boolean;
   --  Determines if range is static, as defined in RM 4.9(26), and also checks
   --  that neither bound of the range raises constraint error, thus ensuring
   --  that both bounds of the range are compile-time evaluable (i.e. do not
   --  raise constraint error). A result of true means that the bounds are
   --  compile time evaluable. A result of false means they are not (either
   --  because the range is not static, or because one or the other bound
   --  raises CE).

   function Is_OK_Static_Subtype (Typ : Entity_Id) return Boolean;
   --  Determines whether a subtype fits the definition of an Ada static
   --  subtype as given in (RM 4.9(26)) with the additional check that neither
   --  bound raises constraint error (meaning that Expr_Value[_R|S] can be used
   --  on these bounds).
   --
   --  This differs from Is_Static_Subtype in that it includes the constraint
   --  error checks, which are missing from Is_Static_Subtype.

   function Is_Out_Of_Range
     (N            : Node_Id;
      Typ          : Entity_Id;
      Assume_Valid : Boolean := False;
      Fixed_Int    : Boolean := False;
      Int_Real     : Boolean := False) return Boolean;
   --  Returns True if it can be guaranteed at compile time that expression is
   --  known to be out of range of the subtype Typ. True is returned if Typ is
   --  a scalar type, and the value of N can be determined to be outside the
   --  range of Typ. A result of False does not mean that the expression is in
   --  range, but rather merely that it cannot be determined at compile time
   --  that it is out of range. The parameters Assume_Valid, Fixed_Int, and
   --  Int_Real are as described for Is_In_Range above.

   function Is_Static_Subtype (Typ : Entity_Id) return Boolean;
   --  Determines whether a subtype fits the definition of an Ada static
   --  subtype as given in (RM 4.9(26)).
   --
   --  This differs from Is_OK_Static_Subtype (which is what must be used by
   --  clients) in that it does not care whether the bounds raise a constraint
   --  error exception or not. Used for checking whether expressions are static
   --  in the 4.9 sense (without worrying about exceptions).

   function Is_Statically_Unevaluated (Expr : Node_Id) return Boolean;
   --  This function returns True if the given expression Expr is statically
   --  unevaluated, as defined in (RM 4.9 (32.1-32.6)).

   function In_Subrange_Of
     (T1        : Entity_Id;
      T2        : Entity_Id;
      Fixed_Int : Boolean := False) return Boolean;
   --  Returns True if it can be guaranteed at compile time that the range of
   --  values for scalar type T1 are always in the range of scalar type T2. A
   --  result of False does not mean that T1 is not in T2's subrange, only that
   --  it cannot be determined at compile time. Flag Fixed_Int is used as in
   --  routine Is_In_Range above.

   function Machine_Number
     (Typ : Entity_Id;
      Val : Ureal;
      N   : Node_Id) return Ureal;
   --  Return the machine number of Typ corresponding to the specified Val as
   --  per RM 4.9(38/2). N is a node only used to post warnings.

   function Not_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean;
   --  Returns True if it can guarantee that Lo .. Hi is not a null range

   function Predicates_Compatible (T1, T2 : Entity_Id) return Boolean;
   --  In Ada 2012, subtypes are statically compatible if the predicates are
   --  compatible as well. This function performs the required check that
   --  predicates are compatible. Split from Subtypes_Statically_Compatible
   --  so that it can be used in specializing error messages.

   function Predicates_Match (T1, T2 : Entity_Id) return Boolean;
   --  In Ada 2012, subtypes statically match if their predicates match as
   --  as well. This function performs the required check that predicates
   --  match. Separated out from Subtypes_Statically_Match so that it can
   --  be used in specializing error messages.

   function Subtypes_Statically_Compatible
     (T1                      : Entity_Id;
      T2                      : Entity_Id;
      Formal_Derived_Matching : Boolean := False) return Boolean;
   --  Returns true if the subtypes are unconstrained or the constraint on
   --  on T1 is statically compatible with T2 (as defined by 4.9.1(4)).
   --  Otherwise returns false. Formal_Derived_Matching indicates whether
   --  the type T1 is a generic actual being checked against ancestor T2
   --  in a formal derived type association.

   function Subtypes_Statically_Match
     (T1                      : Entity_Id;
      T2                      : Entity_Id;
      Formal_Derived_Matching : Boolean := False) return Boolean;
   --  Determine whether two types T1, T2, which have the same base type,
   --  are statically matching subtypes (RM 4.9.1(1-2)). Also includes the
   --  extra GNAT rule that object sizes must match (this can be false for
   --  types that match in the RM sense because of use of 'Object_Size),
   --  except when testing a generic actual T1 against an ancestor T2 in a
   --  formal derived type association (indicated by Formal_Derived_Matching).

   procedure Test_Comparison
     (Op           : Node_Id;
      Assume_Valid : Boolean;
      True_Result  : out Boolean;
      False_Result : out Boolean);
   --  Determine the outcome of evaluating comparison operator Op using routine
   --  Compile_Time_Compare. Assume_Valid should be set when the operands are
   --  to be assumed valid. Flags True_Result and False_Result are set when the
   --  comparison evaluates to True or False respectively.

   procedure Why_Not_Static (Expr : Node_Id);
   --  This procedure may be called after generating an error message that
   --  complains that something is non-static. If it finds good reasons, it
   --  generates one or more error messages pointing the appropriate offending
   --  component of the expression. If no good reasons can be figured out, then
   --  no messages are generated. The expectation here is that the caller has
   --  already issued a message complaining that the expression is non-static.
   --  Note that this message should be placed using Error_Msg_F or
   --  Error_Msg_FE, so that it will sort before any messages placed by this
   --  call. Note that it is fine to call Why_Not_Static with something that
   --  is not an expression, and usually this has no effect, but in some cases
   --  (N_Parameter_Association or N_Range), it makes sense for the internal
   --  recursive calls.
   --
   --  Note that these messages are not continuation messages, instead they are
   --  separate unconditional messages, marked with '!'. The reason for this is
   --  that they can be posted at a different location from the main message as
   --  documented above ("appropriate offending component"), and continuation
   --  messages must always point to the same location as the parent message.

   procedure Initialize;
   --  Initializes the internal data structures

private
   --  The Eval routines are all marked inline, since they are called once

   pragma Inline (Eval_Actual);
   pragma Inline (Eval_Allocator);
   pragma Inline (Eval_Character_Literal);
   pragma Inline (Eval_If_Expression);
   pragma Inline (Eval_Indexed_Component);
   pragma Inline (Eval_Named_Integer);
   pragma Inline (Eval_Named_Real);
   pragma Inline (Eval_Real_Literal);
   pragma Inline (Eval_Shift);
   pragma Inline (Eval_Slice);
   pragma Inline (Eval_String_Literal);
   pragma Inline (Eval_Unchecked_Conversion);

   pragma Inline (Is_OK_Static_Expression);
   pragma Inline (Machine_Number);

end Sem_Eval;
