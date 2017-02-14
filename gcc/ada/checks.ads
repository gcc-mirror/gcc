------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C H E C K S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

--  Package containing routines used to deal with runtime checks. These
--  routines are used both by the semantics and by the expander. In some
--  cases, checks are enabled simply by setting flags for gigi, and in
--  other cases the code for the check is expanded.

--  The approach used for range and length checks, in regards to suppressed
--  checks, is to attempt to detect at compilation time that a constraint
--  error will occur. If this is detected a warning or error is issued and the
--  offending expression or statement replaced with a constraint error node.
--  This always occurs whether checks are suppressed or not. Dynamic range
--  checks are, of course, not inserted if checks are suppressed.

with Errout; use Errout;
with Namet;  use Namet;
with Table;
with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;

package Checks is

   procedure Initialize;
   --  Called for each new main source program, to initialize internal
   --  variables used in the package body of the Checks unit.

   function Access_Checks_Suppressed          (E : Entity_Id) return Boolean;
   function Accessibility_Checks_Suppressed   (E : Entity_Id) return Boolean;
   function Alignment_Checks_Suppressed       (E : Entity_Id) return Boolean;
   function Allocation_Checks_Suppressed      (E : Entity_Id) return Boolean;
   function Atomic_Synchronization_Disabled   (E : Entity_Id) return Boolean;
   function Discriminant_Checks_Suppressed    (E : Entity_Id) return Boolean;
   function Division_Checks_Suppressed        (E : Entity_Id) return Boolean;
   function Duplicated_Tag_Checks_Suppressed  (E : Entity_Id) return Boolean;
   function Elaboration_Checks_Suppressed     (E : Entity_Id) return Boolean;
   function Index_Checks_Suppressed           (E : Entity_Id) return Boolean;
   function Length_Checks_Suppressed          (E : Entity_Id) return Boolean;
   function Overflow_Checks_Suppressed        (E : Entity_Id) return Boolean;
   function Predicate_Checks_Suppressed       (E : Entity_Id) return Boolean;
   function Range_Checks_Suppressed           (E : Entity_Id) return Boolean;
   function Storage_Checks_Suppressed         (E : Entity_Id) return Boolean;
   function Tag_Checks_Suppressed             (E : Entity_Id) return Boolean;
   function Validity_Checks_Suppressed        (E : Entity_Id) return Boolean;
   --  These functions check to see if the named check is suppressed, either
   --  by an active scope suppress setting, or because the check has been
   --  specifically suppressed for the given entity. If no entity is relevant
   --  for the current check, then Empty is used as an argument. Note: the
   --  reason we insist on specifying Empty is to force the caller to think
   --  about whether there is any relevant entity that should be checked.

   function Is_Check_Suppressed (E : Entity_Id; C : Check_Id) return Boolean;
   --  This function is called if Checks_May_Be_Suppressed (E) is True to
   --  determine whether check C is suppressed either on the entity E or
   --  as the result of a scope suppress pragma. If Checks_May_Be_Suppressed
   --  is False, then the status of the check can be determined simply by
   --  examining Scope_Suppress, so this routine is not called in that case.

   function Overflow_Check_Mode return Overflow_Mode_Type;
   --  Returns current overflow checking mode, taking into account whether
   --  we are inside an assertion expression and the assertion policy.

   -----------------------------------------
   -- Control of Alignment Check Warnings --
   -----------------------------------------

   --  When we have address clauses, there is an issue of whether the address
   --  specified is appropriate to the alignment. In the general case where the
   --  address is dynamic, we generate a check and a possible warning (this
   --  warning occurs for example if we have a restricted run time with the
   --  restriction No_Exception_Propagation). We also issue this warning in
   --  the case where the address is static, but we don't know the alignment
   --  at the time we process the address clause. In such a case, we issue the
   --  warning, but we may be able to find out later (after the back end has
   --  annotated the actual alignment chosen) that the warning was not needed.

   --  To deal with deleting these potentially annoying warnings, we save the
   --  warning information in a table, and then delete the waranings in the
   --  post compilation validation stage if we can tell that the check would
   --  never fail (in general the back end will also optimize away the check
   --  in such cases).

   --  Table used to record information

   type Alignment_Warnings_Record is record
      E : Entity_Id;
      --  Entity whose alignment possibly warrants a warning

      A : Uint;
      --  Compile time known value of address clause for which the alignment
      --  is to be checked once we know the alignment.

      W : Error_Msg_Id;
      --  Id of warning message we might delete
   end record;

   package Alignment_Warnings is new Table.Table (
     Table_Component_Type => Alignment_Warnings_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 10,
     Table_Increment      => 200,
     Table_Name           => "Alignment_Warnings");

   procedure Validate_Alignment_Check_Warnings;
   --  This routine is called after back annotation of type data to delete any
   --  alignment warnings that turn out to be false alarms, based on knowing
   --  the actual alignment, and a compile-time known alignment value.

   -------------------------------------------
   -- Procedures to Activate Checking Flags --
   -------------------------------------------

   procedure Activate_Division_Check (N : Node_Id);
   pragma Inline (Activate_Division_Check);
   --  Sets Do_Division_Check flag in node N, and handles possible local raise.
   --  Always call this routine rather than calling Set_Do_Division_Check to
   --  set an explicit value of True, to ensure handling the local raise case.

   procedure Activate_Overflow_Check (N : Node_Id);
   pragma Inline (Activate_Overflow_Check);
   --  Sets Do_Overflow_Check flag in node N, and handles possible local raise.
   --  Always call this routine rather than calling Set_Do_Overflow_Check to
   --  set an explicit value of True, to ensure handling the local raise case.
   --  Note that for discrete types, this call has no effect for MOD, REM, and
   --  unary "+" for which overflow is never possible in any case.
   --
   --  Note: for the discrete-type case, it is legitimate to call this routine
   --  on an unanalyzed node where the Etype field is not set. However, for the
   --  floating-point case, Etype must be set (to a floating-point type).
   --
   --  For floating-point, we set the flag if we have automatic overflow checks
   --  on the target, or if Check_Float_Overflow mode is set. For the floating-
   --  point case, we ignore all the unary operators ("+", "-", and abs) since
   --  none of these can result in overflow. If there are no overflow checks on
   --  the target, and Check_Float_Overflow mode is not set, then the call has
   --  no effect, since in such cases we want to generate NaN's and infinities.

   procedure Activate_Range_Check (N : Node_Id);
   pragma Inline (Activate_Range_Check);
   --  Sets Do_Range_Check flag in node N, and handles possible local raise
   --  Always call this routine rather than calling Set_Do_Range_Check to
   --  set an explicit value of True, to ensure handling the local raise case.

   --------------------------------
   -- Procedures to Apply Checks --
   --------------------------------

   --  General note on following checks. These checks are always active if
   --  Expander_Active and not Inside_A_Generic. They are inactive and have
   --  no effect Inside_A_Generic. In the case where not Expander_Active
   --  and not Inside_A_Generic, most of them are inactive, but some of them
   --  operate anyway since they may generate useful compile time warnings.

   procedure Apply_Access_Check (N : Node_Id);
   --  Determines whether an expression node requires a runtime access
   --  check and if so inserts the appropriate run-time check.

   procedure Apply_Accessibility_Check
     (N           : Node_Id;
      Typ         : Entity_Id;
      Insert_Node : Node_Id);
   --  Given a name N denoting an access parameter, emits a run-time
   --  accessibility check (if necessary), checking that the level of
   --  the object denoted by the access parameter is not deeper than the
   --  level of the type Typ. Program_Error is raised if the check fails.
   --  Insert_Node indicates the node where the check should be inserted.

   procedure Apply_Address_Clause_Check (E : Entity_Id; N : Node_Id);
   --  E is the entity for an object which has an address clause. If checks
   --  are enabled, then this procedure generates a check that the specified
   --  address has an alignment consistent with the alignment of the object,
   --  raising PE if this is not the case. The resulting check (if one is
   --  generated) is prepended to the Actions list of N_Freeze_Entity node N.
   --  Note that the check references E'Alignment, so it cannot be emitted
   --  before N (its freeze node), otherwise this would cause an illegal
   --  access before elaboration error in GIGI. For the case of a clear overlay
   --  situation, we also check that the size of the overlaying object is not
   --  larger than the overlaid object.

   procedure Apply_Arithmetic_Overflow_Check (N : Node_Id);
   --  Handle overflow checking for an arithmetic operator. Also handles the
   --  cases of ELIMINATED and MINIMIZED overflow checking mode. If the mode
   --  is one of the latter two, then this routine can also be called with
   --  an if or case expression node to make sure that we properly handle
   --  overflow checking for dependent expressions. This routine handles
   --  front end vs back end overflow checks (in the front end case it expands
   --  the necessary check). Note that divide is handled separately using
   --  Apply_Divide_Checks. Node N may or may not have Do_Overflow_Check.
   --  In STRICT mode, there is nothing to do if this flag is off, but in
   --  MINIMIZED/ELIMINATED mode we still have to deal with possible use
   --  of doing operations in Long_Long_Integer or Bignum mode.

   procedure Apply_Constraint_Check
     (N          : Node_Id;
      Typ        : Entity_Id;
      No_Sliding : Boolean := False);
   --  Top-level procedure, calls all the others depending on the class of
   --  Typ. Checks that expression N satisfies the constraint of type Typ.
   --  No_Sliding is only relevant for constrained array types, if set to
   --  True, it checks that indexes are in range.

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
   --  formals, the check is performed only if the corresponding actual is
   --  constrained, i.e., whether Lhs'Constrained is True.

   procedure Apply_Divide_Checks (N : Node_Id);
   --  The node kind is N_Op_Divide, N_Op_Mod, or N_Op_Rem if either of the
   --  flags Do_Division_Check or Do_Overflow_Check is set, then this routine
   --  ensures that the appropriate checks are made. Note that overflow can
   --  occur in the signed case for the case of the largest negative number
   --  divided by minus one. This procedure only applies to Integer types.

   procedure Apply_Parameter_Aliasing_Checks
     (Call : Node_Id;
      Subp : Entity_Id);
   --  Given a subprogram call Call, add a check to verify that none of the
   --  actuals overlap. Subp denotes the subprogram being called.

   procedure Apply_Parameter_Validity_Checks (Subp : Entity_Id);
   --  Given a subprogram Subp, add both a pre and post condition pragmas that
   --  verify the proper initialization of scalars in parameters and function
   --  results.

   procedure Apply_Predicate_Check
     (N   : Node_Id;
      Typ : Entity_Id;
      Fun : Entity_Id := Empty);
   --  N is an expression to which a predicate check may need to be applied for
   --  Typ, if Typ has a predicate function. When N is an actual in a call, Fun
   --  is the function being called, which is used to generate a better warning
   --  if the call leads to an infinite recursion.

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

   function Build_Discriminant_Checks
     (N     : Node_Id;
      T_Typ : Entity_Id)
      return  Node_Id;
   --  Subsidiary routine for Apply_Discriminant_Check. Builds the expression
   --  that compares discriminants of the expression with discriminants of the
   --  type. Also used directly for membership tests (see Exp_Ch4.Expand_N_In).

   function Convert_From_Bignum (N : Node_Id) return Node_Id;
   --  Returns result of converting node N from Bignum. The returned value is
   --  not analyzed, the caller takes responsibility for this. Node N must be
   --  a subexpression node of type Bignum. The result is Long_Long_Integer.

   function Convert_To_Bignum (N : Node_Id) return Node_Id;
   --  Returns result of converting node N to Bignum. The returned value is not
   --  analyzed, the caller takes responsibility for this. Node N must be a
   --  subexpression node of a signed integer type or Bignum type (if it is
   --  already a Bignum, the returned value is Relocate_Node (N)).

   procedure Determine_Range
     (N            : Node_Id;
      OK           : out Boolean;
      Lo           : out Uint;
      Hi           : out Uint;
      Assume_Valid : Boolean := False);
   --  N is a node for a subexpression. If N is of a discrete type with no
   --  error indications, and no other peculiarities (e.g. missing Etype),
   --  then OK is True on return, and Lo and Hi are set to a conservative
   --  estimate of the possible range of values of N. Thus if OK is True on
   --  return, the value of the subexpression N is known to lie in the range
   --  Lo .. Hi (inclusive). If the expression is not of a discrete type, or
   --  some kind of error condition is detected, then OK is False on exit, and
   --  Lo/Hi are set to No_Uint. Thus the significance of OK being False on
   --  return is that no useful information is available on the range of the
   --  expression. Assume_Valid determines whether the processing is allowed to
   --  assume that values are in range of their subtypes. If it is set to True,
   --  then this assumption is valid, if False, then processing is done using
   --  base types to allow invalid values.

   procedure Determine_Range_R
     (N            : Node_Id;
      OK           : out Boolean;
      Lo           : out Ureal;
      Hi           : out Ureal;
      Assume_Valid : Boolean := False);
   --  Similar to Determine_Range, but for a node N of floating-point type. OK
   --  is True on return only for IEEE floating-point types and only if we do
   --  not have to worry about extended precision (i.e. on the x86, we must be
   --  using -msse2 -mfpmath=sse). At the current time, this is used only in
   --  GNATprove, though we could consider using it more generally in future.
   --  For that to happen, the possibility of arguments of infinite or NaN
   --  value should be taken into account, which is not the case currently.

   procedure Install_Null_Excluding_Check (N : Node_Id);
   --  Determines whether an access node requires a runtime access check and
   --  if so inserts the appropriate run-time check.

   function Make_Bignum_Block (Loc : Source_Ptr) return Node_Id;
   --  This function is used by top level overflow checking routines to do a
   --  mark/release operation on the secondary stack around bignum operations.
   --  The block created looks like:
   --
   --    declare
   --       M : Mark_Id := SS_Mark;
   --    begin
   --       SS_Release (M);
   --    end;
   --
   --  The idea is that the caller will insert any needed extra declarations
   --  after the declaration of M, and any needed statements (in particular
   --  the bignum operations) before the call to SS_Release, and then do an
   --  Insert_Action of the whole block (it is returned unanalyzed). The Loc
   --  parameter is used to supply Sloc values for the constructed tree.

   procedure Minimize_Eliminate_Overflows
     (N         : Node_Id;
      Lo        : out Uint;
      Hi        : out Uint;
      Top_Level : Boolean);
   --  This is the main routine for handling MINIMIZED and ELIMINATED overflow
   --  processing. On entry N is a node whose result is a signed integer
   --  subtype. The Do_Overflow_Check flag may or may not be set on N. If the
   --  node is an arithmetic operation, then a range analysis is carried out,
   --  and there are three possibilities:
   --
   --    The node is left unchanged (apart from expansion of an exponentiation
   --    operation). This happens if the routine can determine that the result
   --    is definitely in range. The Do_Overflow_Check flag is turned off in
   --    this case.
   --
   --    The node is transformed into an arithmetic operation with a result
   --    type of Long_Long_Integer.
   --
   --    The node is transformed into a function call that calls an appropriate
   --    function in the System.Bignums package to compute a Bignum result.
   --
   --  In the first two cases, Lo and Hi are set to the bounds of the possible
   --  range of results, computed as accurately as possible. In the third case
   --  Lo and Hi are set to No_Uint (there are some cases where we could get an
   --  advantage from keeping result ranges for Bignum values, but it could use
   --  a lot of space and is very unlikely to be valuable).
   --
   --  If the node is not an arithmetic operation, then it is unchanged but
   --  Lo and Hi are still set (to the bounds of the result subtype if nothing
   --  better can be determined).
   --
   --  Note: this function is recursive, if called with an arithmetic operator,
   --  recursive calls are made to process the operands using this procedure.
   --  So we end up doing things top down. Nothing happens to an arithmetic
   --  expression until this procedure is called on the top level node and
   --  then the recursive calls process all the children. We have to do it
   --  this way. If we try to do it bottom up in natural expansion order, then
   --  there are two problems. First, where do we stash the bounds, and more
   --  importantly, semantic processing will be messed up. Consider A+B+C where
   --  A,B,C are all of type integer, if we processed A+B before doing semantic
   --  analysis of the addition of this result to C, that addition could end up
   --  with a Long_Long_Integer left operand and an Integer right operand, and
   --  we would get a semantic error.
   --
   --  The routine is called in three situations if we are operating in either
   --  MINIMIZED or ELIMINATED modes.
   --
   --    Overflow processing applied to the top node of an expression tree when
   --    that node is an arithmetic operator. In this case the result is
   --    converted to the appropriate result type (there is special processing
   --    when the parent is a conversion, see body for details).
   --
   --    Overflow processing applied to the operands of a comparison operation.
   --    In this case, the comparison is done on the result Long_Long_Integer
   --    or Bignum values, without raising any exceptions.
   --
   --    Overflow processing applied to the left operand of a membership test.
   --    In this case no exception is raised if a Long_Long_Integer or Bignum
   --    result is outside the range of the type of that left operand (it is
   --    just that the result of IN is false in that case).
   --
   --  Note that if Bignum values appear, the caller must take care of doing
   --  the appropriate mark/release operations on the secondary stack.
   --
   --  Top_Level is used to avoid inefficient unnecessary transitions into the
   --  Bignum domain. If Top_Level is True, it means that the caller will have
   --  to convert any Bignum value back to Long_Long_Integer, possibly checking
   --  that the value is in range. This is the normal case for a top level
   --  operator in a subexpression. There is no point in going into Bignum mode
   --  to avoid an overflow just so we can check for overflow the next moment.
   --  For calls from comparisons and membership tests, and for all recursive
   --  calls, we do want to transition into the Bignum domain if necessary.
   --  Note that this setting is only relevant in ELIMINATED mode.

   -------------------------------------------------------
   -- Control and Optimization of Range/Overflow Checks --
   -------------------------------------------------------

   --  Range checks are controlled by the Do_Range_Check flag. The front end
   --  is responsible for setting this flag in relevant nodes. Originally
   --  the back end generated all corresponding range checks. But later on
   --  we decided to generate many range checks in the front end. We are now
   --  in the transitional phase where some of these checks are still done
   --  by the back end, but many are done by the front end. It is possible
   --  that in the future we might move all the checks to the front end. The
   --  main remaining back end checks are for subscript checking.

   --  Overflow checks are similarly controlled by the Do_Overflow_Check flag.
   --  The difference here is that if back end overflow checks are inactive
   --  (Backend_Overflow_Checks_On_Target set False), then the actual overflow
   --  checks are generated by the front end, but if back end overflow checks
   --  are active (Backend_Overflow_Checks_On_Target set True), then the back
   --  end does generate the checks.

   --  The following two routines are used to set these flags, they allow
   --  for the possibility of eliminating checks. Checks can be eliminated
   --  if an identical check has already been performed.

   procedure Enable_Overflow_Check (N : Node_Id);
   --  First this routine determines if an overflow check is needed by doing
   --  an appropriate range check. If a check is not needed, then the call
   --  has no effect. If a check is needed then this routine sets the flag
   --  Do_Overflow_Check in node N to True, unless it can be determined that
   --  the check is not needed. The only condition under which this is the
   --  case is if there was an identical check earlier on.

   procedure Enable_Range_Check (N : Node_Id);
   --  Set Do_Range_Check flag in node N True, unless it can be determined
   --  that the check is not needed. The only condition under which this is
   --  the case is if there was an identical check earlier on. This routine
   --  is not responsible for doing range analysis to determine whether or
   --  not such a check is needed -- the caller is expected to do this. The
   --  one other case in which the request to set the flag is ignored is
   --  when Kill_Range_Check is set in an N_Unchecked_Conversion node.

   --  The following routines are used to keep track of processing sequences
   --  of statements (e.g. the THEN statements of an IF statement). A check
   --  that appears within such a sequence can eliminate an identical check
   --  within this sequence of statements. However, after the end of the
   --  sequence of statements, such a check is no longer of interest, since
   --  it may not have been executed.

   procedure Conditional_Statements_Begin;
   --  This call marks the start of processing of a sequence of statements.
   --  Every call to this procedure must be followed by a matching call to
   --  Conditional_Statements_End.

   procedure Conditional_Statements_End;
   --  This call removes from consideration all saved checks since the
   --  corresponding call to Conditional_Statements_Begin. These two
   --  procedures operate in a stack like manner.

   --  The mechanism for optimizing checks works by remembering checks
   --  that have already been made, but certain conditions, for example
   --  an assignment to a variable involved in a check, may mean that the
   --  remembered check is no longer valid, in the sense that if the same
   --  expression appears again, another check is required because the
   --  value may have changed.

   --  The following routines are used to note conditions which may render
   --  some or all of the stored and remembered checks to be invalidated.

   procedure Kill_Checks (V : Entity_Id);
   --  This procedure records an assignment or other condition that causes
   --  the value of the variable to be changed, invalidating any stored
   --  checks that reference the value. Note that all such checks must
   --  be discarded, even if they are not in the current statement range.

   procedure Kill_All_Checks;
   --  This procedure kills all remembered checks

   -----------------------------
   -- Length and Range Checks --
   -----------------------------

   --  In the following procedures, there are three arguments which have
   --  a common meaning as follows:

   --    Expr        The expression to be checked. If a check is required,
   --                the appropriate flag will be placed on this node. Whether
   --                this node is further examined depends on the setting of
   --                the parameter Source_Typ, as described below.

   --    ??? Apply_Length_Check and Apply_Range_Check do not have an Expr
   --        formal

   --    ??? Apply_Length_Check and Apply_Range_Check have a Ck_Node formal
   --        which is undocumented, is it the same as Expr?

   --    Target_Typ  The target type on which the check is to be based. For
   --                example, if we have a scalar range check, then the check
   --                is that we are in range of this type.

   --    Source_Typ  Normally Empty, but can be set to a type, in which case
   --                this type is used for the check, see below.

   --  The checks operate in one of two modes:

   --    If Source_Typ is Empty, then the node Expr is examined, at the very
   --    least to get the source subtype. In addition for some of the checks,
   --    the actual form of the node may be examined. For example, a node of
   --    type Integer whose actual form is an Integer conversion from a type
   --    with range 0 .. 3 can be determined to have a value in range 0 .. 3.

   --    If Source_Typ is given, then nothing can be assumed about the Expr,
   --    and indeed its contents are not examined. In this case the check is
   --    based on the assumption that Expr can be an arbitrary value of the
   --    given Source_Typ.

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
   --  processing continues as enumerated above, with a guard against null
   --  values.
   --
   --  Note: calls to Apply_Length_Check currently never supply an explicit
   --  Source_Typ parameter, but Apply_Length_Check takes this parameter and
   --  processes it as described above for consistency with the other routines
   --  in this section.

   procedure Apply_Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty);
   --  For a Node of kind N_Range, constructs a range check action that tests
   --  first that the range is not null and then that the range is contained in
   --  the Target_Typ range.
   --
   --  For scalar types, constructs a range check action that first tests that
   --  the expression is contained in the Target_Typ range. The difference
   --  between this and Apply_Scalar_Range_Check is that the latter generates
   --  the actual checking code against the Etype of the expression.
   --
   --  For constrained array types, construct series of range check actions
   --  to check that each Expr range is properly contained in the range of
   --  Target_Typ.
   --
   --  For a type conversion to an unconstrained array type, constructs a range
   --  check action to check that the bounds of the source type are within the
   --  constraints imposed by the Target_Typ.
   --
   --  For access types, the Directly_Designated_Type is retrieved and
   --  processing continues as enumerated above, with a guard against null
   --  values.
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
   --  flagged as needing a runtime range check. If the node requires such a
   --  check, the Do_Range_Check flag is turned on. The Fixed_Int flag if set
   --  causes any fixed-point values to be treated as though they were discrete
   --  values (i.e. the underlying integer value is used).

   type Check_Result is private;
   --  Type used to return result of Get_Range_Checks call, for later use in
   --  call to Insert_Range_Checks procedure.

   function Get_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Warn_Node  : Node_Id   := Empty) return Check_Result;
   --  Like Apply_Range_Check, except it does not modify anything. Instead
   --  it returns an encapsulated result of the check operations for later
   --  use in a call to Insert_Range_Checks. If Warn_Node is non-empty, its
   --  Sloc is used, in the static case, for the generated warning or error.
   --  Additionally, it is used rather than Expr (or Low/High_Bound of Expr)
   --  in constructing the check.

   procedure Append_Range_Checks
     (Checks       : Check_Result;
      Stmts        : List_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr;
      Flag_Node    : Node_Id);
   --  Called to append range checks as returned by a call to Get_Range_Checks.
   --  Stmts is a list to which either the dynamic check is appended or the
   --  raise Constraint_Error statement is appended (for static checks).
   --  Static_Sloc is the Sloc at which the raise CE node points, Flag_Node is
   --  used as the node at which to set the Has_Dynamic_Check flag. Checks_On
   --  is a boolean value that says if range and index checking is on or not.

   procedure Insert_Range_Checks
     (Checks       : Check_Result;
      Node         : Node_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr := No_Location;
      Flag_Node    : Node_Id    := Empty;
      Do_Before    : Boolean    := False);
   --  Called to insert range checks as returned by a call to Get_Range_Checks.
   --  Node is the node after which either the dynamic check is inserted or
   --  the raise Constraint_Error statement is inserted (for static checks).
   --  Suppress_Typ is the type to check to determine if checks are suppressed.
   --  Static_Sloc, if passed, is the Sloc at which the raise CE node points,
   --  otherwise Sloc (Node) is used. The Has_Dynamic_Check flag is normally
   --  set at Node. If Flag_Node is present, then this is used instead as the
   --  node at which to set the Has_Dynamic_Check flag. Normally the check is
   --  inserted after, if Do_Before is True, the check is inserted before
   --  Node.

   -----------------------
   -- Expander Routines --
   -----------------------

   --  Some of the earlier processing for checks results in temporarily setting
   --  the Do_Range_Check flag rather than actually generating checks. Now we
   --  are moving the generation of such checks into the front end for reasons
   --  of efficiency and simplicity (there were difficulties in handling this
   --  in the back end when side effects were present in the expressions being
   --  checked).

   --  Probably we could eliminate the Do_Range_Check flag entirely and
   --  generate the checks earlier, but this is a delicate area and it
   --  seemed safer to implement the following routines, which are called
   --  late on in the expansion process. They check the Do_Range_Check flag
   --  and if it is set, generate the actual checks and reset the flag.

   procedure Generate_Range_Check
     (N           : Node_Id;
      Target_Type : Entity_Id;
      Reason      : RT_Exception_Code);
   --  This procedure is called to actually generate and insert a range check.
   --  A check is generated to ensure that the value of N lies within the range
   --  of the target type. Note that the base type of N may be different from
   --  the base type of the target type. This happens in the conversion case.
   --  The Reason parameter is the exception code to be used for the exception
   --  if raised.
   --
   --  Note: if the expander is not active, or if we are in GNATprove mode,
   --  then we do not generate explicit range code. Instead we just turn the
   --  Do_Range_Check flag on, since in these cases that's what we want to see
   --  in the tree (GNATprove in particular depends on this flag being set). If
   --  we generate the actual range check, then we make sure the flag is off,
   --  since the code we generate takes complete care of the check.
   --
   --  Historical note: We used to just pass on the Do_Range_Check flag to the
   --  back end to generate the check, but now in code-generation mode we never
   --  have this flag set, since the front end takes care of the check. The
   --  normal processing flow now is that the analyzer typically turns on the
   --  Do_Range_Check flag, and if it is set, this routine is called, which
   --  turns the flag off in code-generation mode.

   procedure Generate_Index_Checks (N : Node_Id);
   --  This procedure is called to generate index checks on the subscripts for
   --  the indexed component node N. Each subscript expression is examined, and
   --  if the Do_Range_Check flag is set, an appropriate index check is
   --  generated and the flag is reset.

   --  Similarly, we set the flag Do_Discriminant_Check in the semantic
   --  analysis to indicate that a discriminant check is required for selected
   --  component of a discriminated type. The following routine is called from
   --  the expander to actually generate the call.

   procedure Generate_Discriminant_Check (N : Node_Id);
   --  N is a selected component for which a discriminant check is required to
   --  make sure that the discriminants have appropriate values for the
   --  selection. This is done by calling the appropriate discriminant checking
   --  routine for the selector.

   -----------------------
   -- Validity Checking --
   -----------------------

   --  In (RM 13.9.1(9-11)) we have the following rules on invalid values

   --    If the representation of a scalar object does not represent value of
   --    the object's subtype (perhaps because the object was not initialized),
   --    the object is said to have an invalid representation. It is a bounded
   --    error to evaluate the value of such an object. If the error is
   --    detected, either Constraint_Error or Program_Error is raised.
   --    Otherwise, execution continues using the invalid representation. The
   --    rules of the language outside this subclause assume that all objects
   --    have valid representations. The semantics of operations on invalid
   --    representations are as follows:
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

   --  In GNAT, we control validity checking with a switch -gnatV that can take
   --  three parameters, n/d/f for None/Default/Full. These modes have the
   --  following meanings:

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

   --    Literals, including enumeration literals, are clearly always valid

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
   --  Expr is a lvalue, i.e. an expression representing the target of an
   --  assignment. This procedure checks for this expression involving an
   --  assignment to an array value. We have to be sure that all the subscripts
   --  in such a case are valid, since according to the rules in (RM
   --  13.9.1(9-11)) such assignments are not permitted to result in erroneous
   --  behavior in the case of invalid subscript values.

   procedure Ensure_Valid
     (Expr          : Node_Id;
      Holes_OK      : Boolean   := False;
      Related_Id    : Entity_Id := Empty;
      Is_Low_Bound  : Boolean   := False;
      Is_High_Bound : Boolean   := False);
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
   --
   --  Related_Id denotes the entity of the context where Expr appears. Flags
   --  Is_Low_Bound and Is_High_Bound specify whether the expression to check
   --  is the low or the high bound of a range. These three optional arguments
   --  signal Remove_Side_Effects to create an external symbol of the form
   --  Chars (Related_Id)_FIRST/_LAST. For suggested use of these parameters
   --  see the warning in the body of Sem_Ch3.Process_Range_Expr_In_Decl.

   function Expr_Known_Valid (Expr : Node_Id) return Boolean;
   --  This function tests it the value of Expr is known to be valid in the
   --  sense of RM 13.9.1(9-11). In the case of GNAT, it is only discrete types
   --  which are a concern, since for non-discrete types we simply continue
   --  computation with invalid values, which does not lead to erroneous
   --  behavior. Thus Expr_Known_Valid always returns True if the type of Expr
   --  is non-discrete. For discrete types the value returned is True only if
   --  it can be determined that the value is Valid. Otherwise False is
   --  returned.

   procedure Insert_Valid_Check
     (Expr          : Node_Id;
      Related_Id    : Entity_Id := Empty;
      Is_Low_Bound  : Boolean   := False;
      Is_High_Bound : Boolean   := False);
   --  Inserts code that will check for the value of Expr being valid, in the
   --  sense of the 'Valid attribute returning True. Constraint_Error will be
   --  raised if the value is not valid.
   --
   --  Related_Id denotes the entity of the context where Expr appears. Flags
   --  Is_Low_Bound and Is_High_Bound specify whether the expression to check
   --  is the low or the high bound of a range. These three optional arguments
   --  signal Remove_Side_Effects to create an external symbol of the form
   --  Chars (Related_Id)_FIRST/_LAST. For suggested use of these parameters
   --  see the warning in the body of Sem_Ch3.Process_Range_Expr_In_Decl.

   procedure Null_Exclusion_Static_Checks (N : Node_Id);
   --  Ada 2005 (AI-231): Check bad usages of the null-exclusion issue

   procedure Remove_Checks (Expr : Node_Id);
   --  Remove all checks from Expr except those that are only executed
   --  conditionally (on the right side of And Then/Or Else. This call
   --  removes only embedded checks (Do_Range_Check, Do_Overflow_Check).

   procedure Validity_Check_Range
     (N          : Node_Id;
      Related_Id : Entity_Id := Empty);
   --  If N is an N_Range node, then Ensure_Valid is called on its bounds, if
   --  validity checking of operands is enabled. Related_Id denotes the entity
   --  of the context where N appears.

   -----------------------------
   -- Handling of Check Names --
   -----------------------------

   --  The following table contains Name_Id's for recognized checks. The first
   --  entries (corresponding to the values of the subtype Predefined_Check_Id)
   --  contain the Name_Id values for the checks that are predefined, including
   --  All_Checks (see Types). Remaining entries are those that are introduced
   --  by pragma Check_Names.

   package Check_Names is new Table.Table (
     Table_Component_Type => Name_Id,
     Table_Index_Type     => Check_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 30,
     Table_Increment      => 200,
     Table_Name           => "Name_Check_Names");

   function Get_Check_Id (N : Name_Id) return Check_Id;
   --  Function to search above table for matching name. If found returns the
   --  corresponding Check_Id value in the range 1 .. Check_Name.Last. If not
   --  found returns No_Check_Id.

private

   type Check_Result is array (Positive range 1 .. 2) of Node_Id;
   --  There are two cases for the result returned by Range_Check:
   --
   --    For the static case the result is one or two nodes that should cause
   --    a Constraint_Error. Typically these will include Expr itself or the
   --    direct descendants of Expr, such as Low/High_Bound (Expr)). It is the
   --    responsibility of the caller to rewrite and substitute the nodes with
   --    N_Raise_Constraint_Error nodes.
   --
   --    For the non-static case a single N_Raise_Constraint_Error node with a
   --    non-empty Condition field is returned.
   --
   --  Unused entries in Check_Result, if any, are simply set to Empty For
   --  external clients, the required processing on this result is achieved
   --  using the Insert_Range_Checks routine.

   pragma Inline (Apply_Length_Check);
   pragma Inline (Apply_Range_Check);
   pragma Inline (Apply_Static_Length_Check);
end Checks;
