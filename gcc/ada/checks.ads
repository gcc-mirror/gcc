------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C H E C K S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
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

--  Package containing routines used to deal with runtime checks. These
--  routines are used both by the semantics and by the expander. In some
--  cases, checks are enabled simply by setting flags for gigi, and in
--  other cases the code for the check is expanded.

--  The approach used for range and length checks, in regards to suppressed
--  checks, is to attempt to detect at compilation time that a constraint
--  error will occur. If this is detected a warning or error is issued and the
--  offending expression or statement replaced with a constraint error node.
--  This always occurs whether checks are suppressed or not.  Dynamic range
--  checks are, of course, not inserted if checks are suppressed.

with Types; use Types;
with Uintp; use Uintp;

package Checks is

   procedure Initialize;
   --  Called for each new main source program, to initialize internal
   --  variables used in the package body of the Checks unit.

   function Access_Checks_Suppressed        (E : Entity_Id) return Boolean;
   function Accessibility_Checks_Suppressed (E : Entity_Id) return Boolean;
   function Discriminant_Checks_Suppressed  (E : Entity_Id) return Boolean;
   function Division_Checks_Suppressed      (E : Entity_Id) return Boolean;
   function Elaboration_Checks_Suppressed   (E : Entity_Id) return Boolean;
   function Index_Checks_Suppressed         (E : Entity_Id) return Boolean;
   function Length_Checks_Suppressed        (E : Entity_Id) return Boolean;
   function Overflow_Checks_Suppressed      (E : Entity_Id) return Boolean;
   function Range_Checks_Suppressed         (E : Entity_Id) return Boolean;
   function Storage_Checks_Suppressed       (E : Entity_Id) return Boolean;
   function Tag_Checks_Suppressed           (E : Entity_Id) return Boolean;
   --  These functions check to see if the named check is suppressed,
   --  either by an active scope suppress setting, or because the check
   --  has been specifically suppressed for the given entity. If no entity
   --  is relevant for the current check, then Empty is used as an argument.
   --  Note: the reason we insist on specifying Empty is to force the
   --  caller to think about whether there is any relevant entity that
   --  should be checked.

   --  General note on following checks. These checks are always active if
   --  Expander_Active and not Inside_A_Generic. They are inactive and have
   --  no effect Inside_A_Generic. In the case where not Expander_Active
   --  and not Inside_A_Generic, most of them are inactive, but some of them
   --  operate anyway since they may generate useful compile time warnings.

   procedure Apply_Access_Check (N : Node_Id);
   --  Determines whether an expression node should be flagged as needing
   --  a runtime access check. If the node requires such a check, the
   --  Do_Access_Check flag is turned on.

   procedure Apply_Accessibility_Check (N : Node_Id; Typ : Entity_Id);
   --  Given a name N denoting an access parameter, emits a run-time
   --  accessibility check (if necessary), checking that the level of
   --  the object denoted by the access parameter is not deeper than the
   --  level of the type Typ. Program_Error is raised if the check fails.

   procedure Apply_Alignment_Check (E : Entity_Id; N : Node_Id);
   --  E is the entity for an object. If there is an address clause for
   --  this entity, and checks are enabled, then this procedure generates
   --  a check that the specified address has an alignment consistent with
   --  the alignment of the object, raising PE if this is not the case. The
   --  resulting check (if one is generated) is inserted before node N.

   procedure Apply_Array_Size_Check (N : Node_Id; Typ : Entity_Id);
   --  N is the node for an object declaration that declares an object of
   --  array type Typ. This routine generates, if necessary, a check that
   --  the size of the array is not too large, raising Storage_Error if so.

   procedure Apply_Arithmetic_Overflow_Check (N : Node_Id);
   --  Given a binary arithmetic operator (+ - *) expand a software integer
   --  overflow check using range checks on a larger checking type or a call
   --  to an appropriate runtime routine. This is used for all three operators
   --  for the signed integer case, and for +/- in the fixed-point case. The
   --  check is expanded only if Software_Overflow_Checking is enabled and
   --  Do_Overflow_Check is set on node N. Note that divide is handled
   --  separately using Apply_Arithmetic_Divide_Overflow_Check.

   procedure Apply_Constraint_Check
     (N          : Node_Id;
      Typ        : Entity_Id;
      No_Sliding : Boolean := False);
   --  Top-level procedure, calls all the others depending on the class of Typ.
   --  Checks that expression N verifies the constraint of type Typ. No_Sliding
   --  is only relevant for constrained array types, id set to true, it
   --  checks that indexes are in range.

   procedure Apply_Discriminant_Check
     (N   : Node_Id;
      Typ : Entity_Id;
      Lhs : Node_Id := Empty);
   --  Given an expression N of a discriminated type, or of an access type
   --  whose designated type is a discriminanted type, generates a check to
   --  ensure that the expression can be converted to the subtype given as
   --  the second parameter. Lhs is empty except in the case of assignments,
   --  where the target object may be needed to determine the subtype to
   --  check against (such as the cases of unconstrained formal parameters
   --  and unconstrained aliased objects). For the case of unconstrained
   --  formals, the check is peformed only if the corresponding actual is
   --  constrained, i.e., whether Lhs'Constrained is True.

   function Build_Discriminant_Checks
     (N     : Node_Id;
      T_Typ : Entity_Id)
      return  Node_Id;
   --  Subsidiary routine for Apply_Discriminant_Check. Builds the expression
   --  that compares discriminants of the expression with discriminants of the
   --  type. Also used directly for membership tests (see Exp_Ch4.Expand_N_In).

   procedure Apply_Divide_Check (N : Node_Id);
   --  The node kind is N_Op_Divide, N_Op_Mod, or N_Op_Rem. An appropriate
   --  check is generated to ensure that the right operand is non-zero. In
   --  the divide case, we also check that we do not have the annoying case
   --  of the largest negative number divided by minus one.

   procedure Apply_Type_Conversion_Checks (N : Node_Id);
   --  N is an N_Type_Conversion node. A type conversion actually involves
   --  two sorts of checks. The first check is the checks that ensures that
   --  the operand in the type conversion fits onto the base type of the
   --  subtype it is being converted to (see RM 4.6 (28)-(50)). The second
   --  check is there to ensure that once the operand has been converted to
   --  a value of the target type, this converted value meets the
   --  constraints imposed by the target subtype (see RM 4.6 (51)).

   procedure Apply_Universal_Integer_Attribute_Checks (N : Node_Id);
   --  The argument N is an attribute reference node intended for processing
   --  by gigi. The attribute is one that returns a universal integer, but
   --  the attribute reference node is currently typed with the expected
   --  result type. This routine deals with range and overflow checks needed
   --  to make sure that the universal result is in range.

   procedure Determine_Range
     (N  : Node_Id;
      OK : out Boolean;
      Lo : out Uint;
      Hi : out Uint);
   --  N is a node for a subexpression. If N is of a discrete type with
   --  no error indications, and no other peculiarities (e.g. missing
   --  type fields), then OK is True on return, and Lo and Hi are set
   --  to a conservative estimate of the possible range of values of N.
   --  Thus if OK is True on return, the value of the subexpression N is
   --  known to like in the range Lo .. Hi (inclusive). If the expression
   --  is not of a discrete type, or some kind of error condition is
   --  detected, then OK is False on exit, and Lo/Hi are set to No_Uint.
   --  Thus the significance of OK being False on return is that no
   --  useful information is available on the range of the expression.

   -----------------------------
   -- Length and Range Checks --
   -----------------------------

   --  In the following procedures, there are three arguments which have
   --  a common meaning as follows:

   --    Expr        The expression to be checked. If a check is required,
   --                the appropriate flag will be placed on this node. Whether
   --                this node is further examined depends on the setting of
   --                the parameter Source_Typ, as described below.

   --    Target_Typ  The target type on which the check is to be based. For
   --                example, if we have a scalar range check, then the check
   --                is that we are in range of this type.

   --    Source_Typ  Normally Empty, but can be set to a type, in which case
   --                this type is used for the check, see below.

   --  The checks operate in one of two modes:

   --    If Source_Typ is Empty, then the node Expr is examined, at the
   --    very least to get the source subtype. In addition for some of
   --    the checks, the actual form of the node may be examined. For
   --    example, a node of type Integer whose actual form is an Integer
   --    conversion from a type with range 0 .. 3 can be determined to
   --    have a value in the range 0 .. 3.

   --    If Source_Typ is given, then nothing can be assumed about the
   --    Expr, and indeed its contents are not examined. In this case the
   --    check is based on the assumption that Expr can be an arbitrary
   --    value of the given Source_Typ.

   --  Currently, the only case in which a Source_Typ is explicitly supplied
   --  is for the case of Out and In_Out parameters, where, for the conversion
   --  on return (the Out direction), the types must be reversed. This is
   --  handled by the caller.

   procedure Apply_Length_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty);
   --  This procedure builds a sequence of declarations to do a length check
   --  that checks if the lengths of the two arrays Target_Typ and source type
   --  are the same. The resulting actions are inserted at Node using a call
   --  to Insert_Actions.
   --
   --  For access types, the Directly_Designated_Type is retrieved and
   --  processing continues as enumerated above, with a guard against
   --  null values.
   --
   --  Note: calls to Apply_Length_Check currently never supply an explicit
   --  Source_Typ parameter, but Apply_Length_Check takes this parameter and
   --  processes it as described above for consistency with the other routines
   --  in this section.

   procedure Apply_Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty);
   --  For an Node of kind N_Range, constructs a range check action that
   --  tests first that the range is not null and then that the range
   --  is contained in the Target_Typ range.
   --
   --  For scalar types, constructs a range check action that first tests that
   --  the expression is contained in the Target_Typ range. The difference
   --  between this and Apply_Scalar_Range_Check is that the latter generates
   --  the actual checking code in gigi against the Etype of the expression.
   --
   --  For constrained array types, construct series of range check actions
   --  to check that each Expr range is properly contained in the range of
   --  Target_Typ.
   --
   --  For a type conversion to an unconstrained array type, constructs
   --  a range check action to check that the bounds of the source type
   --  are within the constraints imposed by the Target_Typ.
   --
   --  For access types, the Directly_Designated_Type is retrieved and
   --  processing continues as enumerated above, with a guard against
   --  null values.
   --
   --  The source type is used by type conversions to unconstrained array
   --  types to retrieve the corresponding bounds.

   procedure Apply_Static_Length_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty);
   --  Tries to determine statically whether the two array types source type
   --  and Target_Typ have the same length. If it can be determined at compile
   --  time that they do not, then an N_Raise_Constraint_Error node replaces
   --  Expr, and a warning message is issued.

   procedure Apply_Scalar_Range_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Fixed_Int  : Boolean   := False);
   --  For scalar types, determines whether an expression node should be
   --  flagged as needing a runtime range check. If the node requires such
   --  a check, the Do_Range_Check flag is turned on. The Fixed_Int flag
   --  if set causes any fixed-point values to be treated as though they
   --  were discrete values (i.e. the underlying integer value is used).

   type Check_Result is private;
   --  Type used to return result of Range_Check call, for later use in
   --  call to Insert_Range_Checks procedure.

   procedure Append_Range_Checks
     (Checks       : Check_Result;
      Stmts        : List_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr;
      Flag_Node    : Node_Id);
   --  Called to append range checks as returned by a call to Range_Check.
   --  Stmts is a list to which either the dynamic check is appended or
   --  the raise Constraint_Error statement is appended (for static checks).
   --  Static_Sloc is the Sloc at which the raise CE node points,
   --  Flag_Node is used as the node at which to set the Has_Dynamic_Check
   --  flag. Checks_On is a boolean value that says if range and index checking
   --  is on or not.

   procedure Enable_Range_Check (N : Node_Id);
   pragma Inline (Enable_Range_Check);
   --  Set Do_Range_Check flag in node N to True unless Kill_Range_Check flag
   --  is set in N (the purpose of the latter flag is precisely to prevent
   --  Do_Range_Check from being set).

   procedure Insert_Range_Checks
     (Checks       : Check_Result;
      Node         : Node_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr := No_Location;
      Flag_Node    : Node_Id    := Empty;
      Do_Before    : Boolean    := False);
   --  Called to insert range checks as returned by a call to Range_Check.
   --  Node is the node after which either the dynamic check is inserted or
   --  the raise Constraint_Error statement is inserted (for static checks).
   --  Suppress_Typ is the type to check to determine if checks are suppressed.
   --  Static_Sloc, if passed, is the Sloc at which the raise CE node points,
   --  otherwise Sloc (Node) is used. The Has_Dynamic_Check flag is normally
   --  set at Node. If Flag_Node is present, then this is used instead as the
   --  node at which to set the Has_Dynamic_Check flag. Normally the check is
   --  inserted after, if Do_Before is True, the check is inserted before
   --  Node.

   function Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Warn_Node  : Node_Id   := Empty)
      return       Check_Result;
   --  Like Apply_Range_Check, except it does not modify anything. Instead
   --  it returns an encapsulated result of the check operations for later
   --  use in a call to Insert_Range_Checks. If Warn_Node is non-empty, its
   --  Sloc is used, in the static case, for the generated warning or error.
   --  Additionally, it is used rather than Expr (or Low/High_Bound of Expr)
   --  in constructing the check.

   -----------------------
   -- Validity Checking --
   -----------------------

   --  In (RM 13.9.1(9-11)) we have the following rules on invalid values

   --    9   If the representation of a scalar object does not represent a
   --    value of the object's subtype (perhaps because the object was not
   --    initialized), the object is said to have an invalid representation.
   --    It is a bounded error to evaluate the value of such an object.  If
   --    the error is detected, either Constraint_Error or Program_Error is
   --    raised.  Otherwise, execution continues using the invalid
   --    representation.  The rules of the language outside this subclause
   --    assume that all objects have valid representations.  The semantics
   --    of operations on invalid representations are as follows:
   --
   --       10  If the representation of the object represents a value of the
   --           object's type, the value of the type is used.
   --
   --       11  If the representation of the object does not represent a value
   --           of the object's type, the semantics of operations on such
   --           representations is implementation-defined, but does not by
   --           itself lead to erroneous or unpredictable execution, or to
   --           other objects becoming abnormal.

   --  We quote the rules in full here since they are quite delicate. Most
   --  of the time, we can just compute away with wrong values, and get a
   --  possibly wrong result, which is well within the range of allowed
   --  implementation defined behavior. The two tricky cases are subscripted
   --  array assignments, where we don't want to do wild stores, and case
   --  statements where we don't want to do wild jumps.

   --  In GNAT, we control validity checking with a switch -gnatV that
   --  can take three parameters, n/d/f for None/Default/Full. These
   --  modes have the following meanings:

   --    None (no validity checking)

   --      In this mode, there is no specific checking for invalid values
   --      and the code generator assumes that all stored values are always
   --      within the bounds of the object subtype. The consequences are as
   --      follows:

   --        For case statements, an out of range invalid value will cause
   --        Constraint_Error to be raised, or an arbitrary one of the case
   --        alternatives will be executed. Wild jumps cannot result even
   --        in this mode, since we always do a range check

   --        For subscripted array assignments, wild stores will result in
   --        the expected manner when addresses are calculated using values
   --        of subscripts that are out of range.

   --      It could perhaps be argued that this mode is still conformant with
   --      the letter of the RM, since implementation defined is a rather
   --      broad category, but certainly it is not in the spirit of the
   --      RM requirement, since wild stores certainly seem to be a case of
   --      erroneous behavior.

   --    Default (default standard RM-compatible validity checking)

   --      In this mode, which is the default, minimal validity checking is
   --      performed to ensure no erroneous behavior as follows:

   --        For case statements, an out of range invalid value will cause
   --        Constraint_Error to be raised.

   --        For subscripted array assignments, invalid out of range
   --        subscript values will cause Constraint_Error to be raised.

   --    Full (Full validity checking)

   --      In this mode, the protections guaranteed by the standard mode are
   --      in place, and the following additional checks are made:

   --        For every assignment, the right side is checked for validity

   --        For every call, IN and IN OUT parameters are checked for validity

   --        For every subscripted array reference, both for stores and loads,
   --        all subscripts are checked for validity.

   --      These checks are not required by the RM, but will in practice
   --      improve the detection of uninitialized variables, particularly
   --      if used in conjunction with pragma Normalize_Scalars.

   --  In the above description, we talk about performing validity checks,
   --  but we don't actually generate a check in a case where the compiler
   --  can be sure that the value is valid. Note that this assurance must
   --  be achieved without assuming that any uninitialized value lies within
   --  the range of its type. The following are cases in which values are
   --  known to be valid. The flag Is_Known_Valid is used to keep track of
   --  some of these cases.

   --    If all possible stored values are valid, then any uninitialized
   --    value must be valid.

   --    Literals, including enumeration literals, are clearly always valid.

   --    Constants are always assumed valid, with a validity check being
   --    performed on the initializing value where necessary to ensure that
   --    this is the case.

   --    For variables, the status is set to known valid if there is an
   --    initializing expression. Again a check is made on the initializing
   --    value if necessary to ensure that this assumption is valid. The
   --    status can change as a result of local assignments to a variable.
   --    If a known valid value is unconditionally assigned, then we mark
   --    the left side as known valid. If a value is assigned that is not
   --    known to be valid, then we mark the left side as invalid. This
   --    kind of processing does NOT apply to non-local variables since we
   --    are not following the flow graph (more properly the flow of actual
   --    processing only corresponds to the flow graph for local assignments).
   --    For non-local variables, we preserve the current setting, i.e. a
   --    validity check is performed when assigning to a knonwn valid global.

   --  Note: no validity checking is required if range checks are suppressed
   --  regardless of the setting of the validity checking mode.

   --  The following procedures are used in handling validity checking

   procedure Apply_Subscript_Validity_Checks (Expr : Node_Id);
   --  Expr is the node for an indexed component. If validity checking and
   --  range checking are enabled, all subscripts for this indexed component
   --  are checked for validity.

   procedure Check_Valid_Lvalue_Subscripts (Expr : Node_Id);
   --  Expr is a lvalue, i.e. an expression representing the target of
   --  an assignment. This procedure checks for this expression involving
   --  an assignment to an array value. We have to be sure that all the
   --  subscripts in such a case are valid, since according to the rules
   --  in (RM 13.9.1(9-11)) such assignments are not permitted to result
   --  in erroneous behavior in the case of invalid subscript values.

   procedure Ensure_Valid (Expr : Node_Id; Holes_OK : Boolean := False);
   --  Ensure that Expr represents a valid value of its type. If this type
   --  is not a scalar type, then the call has no effect, since validity
   --  is only an issue for scalar types. The effect of this call is to
   --  check if the value is known valid, if so, nothing needs to be done.
   --  If this is not known, then either Expr is set to be range checked,
   --  or specific checking code is inserted so that an exception is raised
   --  if the value is not valid.
   --
   --  The optional argument Holes_OK indicates whether it is necessary to
   --  worry about enumeration types with non-standard representations leading
   --  to "holes" in the range of possible representations. If Holes_OK is
   --  True, then such values are assumed valid (this is used when the caller
   --  will make a separate check for this case anyway). If Holes_OK is False,
   --  then this case is checked, and code is inserted to ensure that Expr is
   --  valid, raising Constraint_Error if the value is not valid.

   function Expr_Known_Valid (Expr : Node_Id) return Boolean;
   --  This function tests it the value of Expr is known to be valid in
   --  the sense of RM 13.9.1(9-11). In the case of GNAT, it is only
   --  discrete types which are a concern, since for non-discrete types
   --  we simply continue computation with invalid values, which does
   --  not lead to erroneous behavior. Thus Expr_Known_Valid always
   --  returns True if the type of Expr is non-discrete. For discrete
   --  types the value returned is True only if it can be determined
   --  that the value is Valid. Otherwise False is returned.

   procedure Insert_Valid_Check (Expr : Node_Id);
   --  Inserts code that will check for the value of Expr being valid, in
   --  the sense of the 'Valid attribute returning True. Constraint_Error
   --  will be raised if the value is not valid.

private

   type Check_Result is array (Positive range 1 .. 2) of Node_Id;
   --  There are two cases for the result returned by Range_Check:
   --
   --    For the static case the result is one or two nodes that should cause
   --    a Constraint_Error. Typically these will include Expr itself or the
   --    direct descendents of Expr, such as Low/High_Bound (Expr)). It is the
   --    responsibility of the caller to rewrite and substitute the nodes with
   --    N_Raise_Constraint_Error nodes.
   --
   --    For the non-static case a single N_Raise_Constraint_Error node
   --    with a non-empty Condition field is returned.
   --
   --  Unused entries in Check_Result, if any, are simply set to Empty
   --  For external clients, the required processing on this result is
   --  achieved using the Insert_Range_Checks routine.

   pragma Inline (Access_Checks_Suppressed);
   pragma Inline (Accessibility_Checks_Suppressed);
   pragma Inline (Discriminant_Checks_Suppressed);
   pragma Inline (Division_Checks_Suppressed);
   pragma Inline (Elaboration_Checks_Suppressed);
   pragma Inline (Index_Checks_Suppressed);
   pragma Inline (Length_Checks_Suppressed);
   pragma Inline (Overflow_Checks_Suppressed);
   pragma Inline (Range_Checks_Suppressed);
   pragma Inline (Storage_Checks_Suppressed);
   pragma Inline (Tag_Checks_Suppressed);

   pragma Inline (Apply_Length_Check);
   pragma Inline (Apply_Range_Check);
   pragma Inline (Apply_Static_Length_Check);
end Checks;
