------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E V A L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Checks;         use Checks;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Eval_Fat;       use Eval_Fat;
with Exp_Util;       use Exp_Util;
with Freeze;         use Freeze;
with Lib;            use Lib;
with Namet;          use Namet;
with Nmake;          use Nmake;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Par_SCO;        use Par_SCO;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aggr;       use Sem_Aggr;
with Sem_Aux;        use Sem_Aux;
with Sem_Cat;        use Sem_Cat;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Elab;       use Sem_Elab;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sem_Type;       use Sem_Type;
with Sem_Warn;       use Sem_Warn;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;
with Warnsw;         use Warnsw;

package body Sem_Eval is

   -----------------------------------------
   -- Handling of Compile Time Evaluation --
   -----------------------------------------

   --  The compile time evaluation of expressions is distributed over several
   --  Eval_xxx procedures. These procedures are called immediately after
   --  a subexpression is resolved and is therefore accomplished in a bottom
   --  up fashion. The flags are synthesized using the following approach.

   --    Is_Static_Expression is determined by following the rules in
   --    RM-4.9. This involves testing the Is_Static_Expression flag of
   --    the operands in many cases.

   --    Raises_Constraint_Error is usually set if any of the operands have
   --    the flag set or if an attempt to compute the value of the current
   --    expression results in Constraint_Error.

   --  The general approach is as follows. First compute Is_Static_Expression.
   --  If the node is not static, then the flag is left off in the node and
   --  we are all done. Otherwise for a static node, we test if any of the
   --  operands will raise Constraint_Error, and if so, propagate the flag
   --  Raises_Constraint_Error to the result node and we are done (since the
   --  error was already posted at a lower level).

   --  For the case of a static node whose operands do not raise constraint
   --  error, we attempt to evaluate the node. If this evaluation succeeds,
   --  then the node is replaced by the result of this computation. If the
   --  evaluation raises Constraint_Error, then we rewrite the node with
   --  Apply_Compile_Time_Constraint_Error to raise the exception and also
   --  to post appropriate error messages.

   ----------------
   -- Local Data --
   ----------------

   type Bits is array (Nat range <>) of Boolean;
   --  Used to convert unsigned (modular) values for folding logical ops

   --  The following declarations are used to maintain a cache of nodes that
   --  have compile-time-known values. The cache is maintained only for
   --  discrete types (the most common case), and is populated by calls to
   --  Compile_Time_Known_Value and Expr_Value, but only used by Expr_Value
   --  since it is possible for the status to change (in particular it is
   --  possible for a node to get replaced by a Constraint_Error node).

   CV_Bits : constant := 5;
   --  Number of low order bits of Node_Id value used to reference entries
   --  in the cache table.

   CV_Cache_Size : constant Nat := 2 ** CV_Bits;
   --  Size of cache for compile time values

   subtype CV_Range is Nat range 0 .. CV_Cache_Size;

   type CV_Entry is record
      N : Node_Id'Base;
      --  We use 'Base here, in case we want to add a predicate to Node_Id
      V : Uint;
   end record;

   type Match_Result is (Match, No_Match, Non_Static);
   --  Result returned from functions that test for a matching result. If the
   --  operands are not OK_Static then Non_Static will be returned. Otherwise
   --  Match/No_Match is returned depending on whether the match succeeds.

   type CV_Cache_Array is array (CV_Range) of CV_Entry;

   CV_Cache : CV_Cache_Array;
   --  This is the actual cache, with entries consisting of node/value pairs,
   --  and the impossible value Node_High_Bound used for unset entries.

   type Range_Membership is (In_Range, Out_Of_Range, Unknown);
   --  Range membership may either be statically known to be in range or out
   --  of range, or not statically known. Used for Test_In_Range below.

   Checking_For_Potentially_Static_Expression : Boolean := False;
   --  Global flag that is set True during Analyze_Static_Expression_Function
   --  in order to verify that the result expression of a static expression
   --  function is a potentially static function (see RM2022 6.8(5.3)).

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Non_Static_Context_For_Overflow
     (N      : Node_Id;
      Stat   : Boolean;
      Result : Uint);
   --  For a signed integer type, check non-static overflow in Result when
   --  Stat is False. This applies also inside inlined code, where the static
   --  property may be an effect of the inlining, which should not be allowed
   --  to remove run-time checks (whether during compilation, or even more
   --  crucially in the special inlining-for-proof in GNATprove mode).

   function Choice_Matches
     (Expr   : Node_Id;
      Choice : Node_Id) return Match_Result;
   --  Determines whether given value Expr matches the given Choice. The Expr
   --  can be of discrete, real, or string type and must be a compile time
   --  known value (it is an error to make the call if these conditions are
   --  not met). The choice can be a range, subtype name, subtype indication,
   --  or expression. The returned result is Non_Static if Choice is not
   --  OK_Static, otherwise either Match or No_Match is returned depending
   --  on whether Choice matches Expr. This is used for case expression
   --  alternatives, and also for membership tests. In each case, more
   --  possibilities are tested than the syntax allows (e.g. membership allows
   --  subtype indications and non-discrete types, and case allows an OTHERS
   --  choice), but it does not matter, since we have already done a full
   --  semantic and syntax check of the construct, so the extra possibilities
   --  just will not arise for correct expressions.
   --
   --  Note: if Choice_Matches finds that a choice raises Constraint_Error, e.g
   --  a reference to a type, one of whose bounds raises Constraint_Error, then
   --  it also sets the Raises_Constraint_Error flag on the Choice itself.

   function Choices_Match
     (Expr    : Node_Id;
      Choices : List_Id) return Match_Result;
   --  This function applies Choice_Matches to each element of Choices. If the
   --  result is No_Match, then it continues and checks the next element. If
   --  the result is Match or Non_Static, this result is immediately given
   --  as the result without checking the rest of the list. Expr can be of
   --  discrete, real, or string type and must be a compile-time-known value
   --  (it is an error to make the call if these conditions are not met).

   procedure Eval_Intrinsic_Call (N : Node_Id; E : Entity_Id);
   --  Evaluate a call N to an intrinsic subprogram E.

   function Find_Universal_Operator_Type (N : Node_Id) return Entity_Id;
   --  Check whether an arithmetic operation with universal operands which is a
   --  rewritten function call with an explicit scope indication is ambiguous:
   --  P."+" (1, 2) will be ambiguous if there is more than one visible numeric
   --  type declared in P and the context does not impose a type on the result
   --  (e.g. in the expression of a type conversion). If ambiguous, emit an
   --  error and return Empty, else return the result type of the operator.

   procedure Fold_Dummy (N : Node_Id; Typ : Entity_Id);
   --  Rewrite N as a constant dummy value in the relevant type if possible.

   procedure Fold_Shift
     (N          : Node_Id;
      Left       : Node_Id;
      Right      : Node_Id;
      Op         : Node_Kind;
      Static     : Boolean := False;
      Check_Elab : Boolean := False);
   --  Rewrite N as the result of evaluating Left <shift op> Right if possible.
   --  Op represents the shift operation.
   --  Static indicates whether the resulting node should be marked static.
   --  Check_Elab indicates whether checks for elaboration calls should be
   --  inserted when relevant.

   function From_Bits (B : Bits; T : Entity_Id) return Uint;
   --  Converts a bit string of length B'Length to a Uint value to be used for
   --  a target of type T, which is a modular type. This procedure includes the
   --  necessary reduction by the modulus in the case of a nonbinary modulus
   --  (for a binary modulus, the bit string is the right length any way so all
   --  is well).

   function Get_String_Val (N : Node_Id) return Node_Id;
   --  Given a tree node for a folded string or character value, returns the
   --  corresponding string literal or character literal (one of the two must
   --  be available, or the operand would not have been marked as foldable in
   --  the earlier analysis of the operation).

   function Is_OK_Static_Choice (Choice : Node_Id) return Boolean;
   --  Given a choice (from a case expression or membership test), returns
   --  True if the choice is static and does not raise a Constraint_Error.

   function Is_OK_Static_Choice_List (Choices : List_Id) return Boolean;
   --  Given a choice list (from a case expression or membership test), return
   --  True if all choices are static in the sense of Is_OK_Static_Choice.

   function Is_Static_Choice (Choice : Node_Id) return Boolean;
   --  Given a choice (from a case expression or membership test), returns
   --  True if the choice is static. No test is made for raising of constraint
   --  error, so this function is used only for legality tests.

   function Is_Static_Choice_List (Choices : List_Id) return Boolean;
   --  Given a choice list (from a case expression or membership test), return
   --  True if all choices are static in the sense of Is_Static_Choice.

   function Is_Static_Range (N : Node_Id) return Boolean;
   --  Determine if range is static, as defined in RM 4.9(26). The only allowed
   --  argument is an N_Range node (but note that the semantic analysis of
   --  equivalent range attribute references already turned them into the
   --  equivalent range). This differs from Is_OK_Static_Range (which is what
   --  must be used by clients) in that it does not care whether the bounds
   --  raise Constraint_Error or not. Used for checking whether expressions are
   --  static in the 4.9 sense (without worrying about exceptions).

   function OK_Bits (N : Node_Id; Bits : Uint) return Boolean;
   --  Bits represents the number of bits in an integer value to be computed
   --  (but the value has not been computed yet). If this value in Bits is
   --  reasonable, a result of True is returned, with the implication that the
   --  caller should go ahead and complete the calculation. If the value in
   --  Bits is unreasonably large, then an error is posted on node N, and
   --  False is returned (and the caller skips the proposed calculation).

   procedure Out_Of_Range (N : Node_Id);
   --  This procedure is called if it is determined that node N, which appears
   --  in a non-static context, is a compile-time-known value which is outside
   --  its range, i.e. the range of Etype. This is used in contexts where
   --  this is an illegality if N is static, and should generate a warning
   --  otherwise.

   function Real_Or_String_Static_Predicate_Matches
     (Val : Node_Id;
      Typ : Entity_Id) return Boolean;
   --  This is the function used to evaluate real or string static predicates.
   --  Val is an unanalyzed N_Real_Literal or N_String_Literal node, which
   --  represents the value to be tested against the predicate. Typ is the
   --  type with the predicate, from which the predicate expression can be
   --  extracted. The result returned is True if the given value satisfies
   --  the predicate.

   procedure Rewrite_In_Raise_CE (N : Node_Id; Exp : Node_Id);
   --  N and Exp are nodes representing an expression, Exp is known to raise
   --  CE. N is rewritten in term of Exp in the optimal way.

   function String_Type_Len (Stype : Entity_Id) return Uint;
   --  Given a string type, determines the length of the index type, or, if
   --  this index type is non-static, the length of the base type of this index
   --  type. Note that if the string type is itself static, then the index type
   --  is static, so the second case applies only if the string type passed is
   --  non-static.

   function Test (Cond : Boolean) return Uint;
   pragma Inline (Test);
   --  This function simply returns the appropriate Boolean'Pos value
   --  corresponding to the value of Cond as a universal integer. It is
   --  used for producing the result of the static evaluation of the
   --  logical operators

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean);
   --  Tests to see if expression N whose single operand is Op1 is foldable,
   --  i.e. the operand value is known at compile time. If the operation is
   --  foldable, then Fold is True on return, and Stat indicates whether the
   --  result is static (i.e. the operand was static). Note that it is quite
   --  possible for Fold to be True, and Stat to be False, since there are
   --  cases in which we know the value of an operand even though it is not
   --  technically static (e.g. the static lower bound of a range whose upper
   --  bound is non-static).
   --
   --  If Stat is set False on return, then Test_Expression_Is_Foldable makes
   --  a call to Check_Non_Static_Context on the operand. If Fold is False on
   --  return, then all processing is complete, and the caller should return,
   --  since there is nothing else to do.
   --
   --  If Stat is set True on return, then Is_Static_Expression is also set
   --  true in node N. There are some cases where this is over-enthusiastic,
   --  e.g. in the two operand case below, for string comparison, the result is
   --  not static even though the two operands are static. In such cases, the
   --  caller must reset the Is_Static_Expression flag in N.
   --
   --  If Fold and Stat are both set to False then this routine performs also
   --  the following extra actions:
   --
   --    If either operand is Any_Type then propagate it to result to prevent
   --    cascaded errors.
   --
   --    If some operand raises Constraint_Error, then replace the node N
   --    with the raise Constraint_Error node. This replacement inherits the
   --    Is_Static_Expression flag from the operands.

   procedure Test_Expression_Is_Foldable
     (N        : Node_Id;
      Op1      : Node_Id;
      Op2      : Node_Id;
      Stat     : out Boolean;
      Fold     : out Boolean;
      CRT_Safe : Boolean := False);
   --  Same processing, except applies to an expression N with two operands
   --  Op1 and Op2. The result is static only if both operands are static. If
   --  CRT_Safe is set True, then CRT_Safe_Compile_Time_Known_Value is used
   --  for the tests that the two operands are known at compile time. See
   --  spec of this routine for further details.

   function Test_In_Range
     (N            : Node_Id;
      Typ          : Entity_Id;
      Assume_Valid : Boolean;
      Fixed_Int    : Boolean;
      Int_Real     : Boolean) return Range_Membership;
   --  Common processing for Is_In_Range and Is_Out_Of_Range: Returns In_Range
   --  or Out_Of_Range if it can be guaranteed at compile time that expression
   --  N is known to be in or out of range of the subtype Typ. If not compile
   --  time known, Unknown is returned. See documentation of Is_In_Range for
   --  complete description of parameters.

   procedure To_Bits (U : Uint; B : out Bits);
   --  Converts a Uint value to a bit string of length B'Length

   -----------------------------------------------
   -- Check_Expression_Against_Static_Predicate --
   -----------------------------------------------

   procedure Check_Expression_Against_Static_Predicate
     (Expr                    : Node_Id;
      Typ                     : Entity_Id;
      Static_Failure_Is_Error : Boolean := False)
   is
   begin
      --  Nothing to do if expression is not known at compile time, or the
      --  type has no static predicate set (will be the case for all non-scalar
      --  types, so no need to make a special test for that).

      if not (Has_Static_Predicate (Typ)
               and then Compile_Time_Known_Value (Expr))
      then
         return;
      end if;

      --  Here we have a static predicate (note that it could have arisen from
      --  an explicitly specified Dynamic_Predicate whose expression met the
      --  rules for being predicate-static). If the expression is known at
      --  compile time and obeys the predicate, then it is static and must be
      --  labeled as such, which matters e.g. for case statements. The original
      --  expression may be a type conversion of a variable with a known value,
      --  which might otherwise not be marked static.

      --  Case of real static predicate

      if Is_Real_Type (Typ) then
         if Real_Or_String_Static_Predicate_Matches
              (Val => Make_Real_Literal (Sloc (Expr), Expr_Value_R (Expr)),
               Typ => Typ)
         then
            Set_Is_Static_Expression (Expr);
            return;
         end if;

      --  Case of string static predicate

      elsif Is_String_Type (Typ) then
         if Real_Or_String_Static_Predicate_Matches
              (Val => Expr_Value_S (Expr), Typ => Typ)
         then
            Set_Is_Static_Expression (Expr);
            return;
         end if;

      --  Case of discrete static predicate

      else
         pragma Assert (Is_Discrete_Type (Typ));

         --  If static predicate matches, nothing to do

         if Choices_Match (Expr, Static_Discrete_Predicate (Typ)) = Match then
            Set_Is_Static_Expression (Expr);
            return;
         end if;
      end if;

      --  Here we know that the predicate will fail

      --  Special case of static expression failing a predicate (other than one
      --  that was explicitly specified with a Dynamic_Predicate aspect). If
      --  the expression comes from a qualified_expression or type_conversion
      --  this is an error (Static_Failure_Is_Error); otherwise we only issue
      --  a warning and the expression is no longer considered static.

      if Is_Static_Expression (Expr)
        and then not Has_Dynamic_Predicate_Aspect (Typ)
        and then not Has_Ghost_Predicate_Aspect (Typ)
      then
         if Static_Failure_Is_Error then
            Error_Msg_NE
              ("static expression fails static predicate check on &",
               Expr, Typ);

         else
            Error_Msg_NE
              ("??static expression fails static predicate check on &",
               Expr, Typ);
            Error_Msg_N
              ("\??expression is no longer considered static", Expr);

            Set_Is_Static_Expression (Expr, False);
         end if;

      --  In all other cases, this is just a warning that a test will fail.
      --  It does not matter if the expression is static or not, or if the
      --  predicate comes from a dynamic predicate aspect or not.

      else
         Error_Msg_NE
           ("??expression fails predicate check on &", Expr, Typ);

         --  Force a check here, which is potentially a redundant check, but
         --  this ensures a check will be done in cases where the expression
         --  is folded, and since this is definitely a failure, extra checks
         --  are OK.

         if Predicate_Enabled (Typ) then
            Insert_Action (Expr,
              Make_Predicate_Check
                (Typ, Duplicate_Subexpr (Expr)), Suppress => All_Checks);
         end if;
      end if;
   end Check_Expression_Against_Static_Predicate;

   ------------------------------
   -- Check_Non_Static_Context --
   ------------------------------

   procedure Check_Non_Static_Context (N : Node_Id) is
      T         : constant Entity_Id := Etype (N);
      Checks_On : constant Boolean   :=
                    not Index_Checks_Suppressed (T)
                      and not Range_Checks_Suppressed (T);

   begin
      --  Ignore cases of non-scalar types, error types, or universal real
      --  types that have no usable bounds.

      if T = Any_Type
        or else not Is_Scalar_Type (T)
        or else T = Universal_Fixed
        or else T = Universal_Real
      then
         return;
      end if;

      --  At this stage we have a scalar type. If we have an expression that
      --  raises CE, then we already issued a warning or error msg so there is
      --  nothing more to be done in this routine.

      if Raises_Constraint_Error (N) then
         return;
      end if;

      --  Now we have a scalar type which is not marked as raising a constraint
      --  error exception. The main purpose of this routine is to deal with
      --  static expressions appearing in a non-static context. That means
      --  that if we do not have a static expression then there is not much
      --  to do. The one case that we deal with here is that if we have a
      --  floating-point value that is out of range, then we post a warning
      --  that an infinity will result.

      if not Is_Static_Expression (N) then
         if Is_Floating_Point_Type (T) then
            if Is_Out_Of_Range (N, Base_Type (T), Assume_Valid => True) then
               Error_Msg_N
                 ("??float value out of range, infinity will be generated", N);

            --  The literal may be the result of constant-folding of a non-
            --  static subexpression of a larger expression (e.g. a conversion
            --  of a non-static variable whose value happens to be known). At
            --  this point we must reduce the value of the subexpression to a
            --  machine number (RM 4.9 (38/2)).

            elsif Nkind (N) = N_Real_Literal
              and then Nkind (Parent (N)) in N_Subexpr
            then
               Rewrite (N, New_Copy (N));
               Set_Realval (N, Machine_Number (Base_Type (T), Realval (N), N));
               Set_Is_Machine_Number (N);
            end if;
         end if;

         return;
      end if;

      --  Here we have the case of outer level static expression of scalar
      --  type, where the processing of this procedure is needed.

      --  For real types, this is where we convert the value to a machine
      --  number (see RM 4.9(38)). Also see ACVC test C490001. We should only
      --  need to do this if the parent is a constant declaration, since in
      --  other cases, gigi should do the necessary conversion correctly, but
      --  experimentation shows that this is not the case on all machines, in
      --  particular if we do not convert all literals to machine values in
      --  non-static contexts, then ACVC test C490001 fails on Sparc/Solaris
      --  and SGI/Irix.

      --  This conversion is always done by GNATprove on real literals in
      --  non-static expressions, by calling Check_Non_Static_Context from
      --  gnat2why, as GNATprove cannot do the conversion later contrary
      --  to gigi. The frontend computes the information about which
      --  expressions are static, which is used by gnat2why to call
      --  Check_Non_Static_Context on exactly those real literals that are
      --  not subexpressions of static expressions.

      if Nkind (N) = N_Real_Literal
        and then not Is_Machine_Number (N)
        and then not Is_Generic_Type (Etype (N))
        and then Etype (N) /= Universal_Real
      then
         --  Check that value is in bounds before converting to machine
         --  number, so as not to lose case where value overflows in the
         --  least significant bit or less. See B490001.

         if Is_Out_Of_Range (N, Base_Type (T), Assume_Valid => True) then
            Out_Of_Range (N);
            return;
         end if;

         --  Note: we have to copy the node, to avoid problems with conformance
         --  of very similar numbers (see ACVC tests B4A010C and B63103A).

         Rewrite (N, New_Copy (N));

         if not Is_Floating_Point_Type (T) then
            Set_Realval
              (N, Corresponding_Integer_Value (N) * Small_Value (T));

         elsif not UR_Is_Zero (Realval (N)) then
            Set_Realval (N, Machine_Number (Base_Type (T), Realval (N), N));
            Set_Is_Machine_Number (N);
         end if;

      end if;

      --  Check for out of range universal integer. This is a non-static
      --  context, so the integer value must be in range of the runtime
      --  representation of universal integers.

      --  We do this only within an expression, because that is the only
      --  case in which non-static universal integer values can occur, and
      --  furthermore, Check_Non_Static_Context is currently (incorrectly???)
      --  called in contexts like the expression of a number declaration where
      --  we certainly want to allow out of range values.

      --  We inhibit the warning when expansion is disabled, because the
      --  preanalysis of a range of a 64-bit modular type may appear to
      --  violate the constraint on non-static Universal_Integer. If there
      --  is a true overflow it will be diagnosed during full analysis.

      if Etype (N) = Universal_Integer
        and then Nkind (N) = N_Integer_Literal
        and then Nkind (Parent (N)) in N_Subexpr
        and then Expander_Active
        and then
          (Intval (N) < Expr_Value (Type_Low_Bound (Universal_Integer))
             or else
           Intval (N) > Expr_Value (Type_High_Bound (Universal_Integer)))
      then
         Apply_Compile_Time_Constraint_Error
           (N, "non-static universal integer value out of range<<",
            CE_Range_Check_Failed);

      --  Check out of range of base type

      elsif Is_Out_Of_Range (N, Base_Type (T), Assume_Valid => True) then
         Out_Of_Range (N);

      --  Give a warning or error on the value outside the subtype. A warning
      --  is omitted if the expression appears in a range that could be null
      --  (warnings are handled elsewhere for this case).

      elsif T /= Base_Type (T) and then Nkind (Parent (N)) /= N_Range then
         if Is_In_Range (N, T, Assume_Valid => True) then
            null;

         elsif Is_Out_Of_Range (N, T, Assume_Valid => True) then
            --  Ignore out of range values for System.Priority in CodePeer
            --  mode since the actual target compiler may provide a wider
            --  range.

            if CodePeer_Mode and then Is_RTE (T, RE_Priority) then
               Set_Do_Range_Check (N, False);

            --  Determine if the out-of-range violation constitutes a warning
            --  or an error based on context, according to RM 4.9 (34/3).

            elsif Nkind (Original_Node (N)) in
                    N_Type_Conversion | N_Qualified_Expression
              and then Comes_From_Source (Original_Node (N))
            then
               Apply_Compile_Time_Constraint_Error
                 (N, "value not in range of}", CE_Range_Check_Failed);
            else
               Apply_Compile_Time_Constraint_Error
                 (N, "value not in range of}<<", CE_Range_Check_Failed);
            end if;

         elsif Checks_On then
            Enable_Range_Check (N);

         else
            Set_Do_Range_Check (N, False);
         end if;
      end if;
   end Check_Non_Static_Context;

   -------------------------------------------
   -- Check_Non_Static_Context_For_Overflow --
   -------------------------------------------

   procedure Check_Non_Static_Context_For_Overflow
     (N      : Node_Id;
      Stat   : Boolean;
      Result : Uint)
   is
   begin
      if (not Stat or else In_Inlined_Body)
        and then Is_Signed_Integer_Type (Etype (N))
      then
         declare
            BT : constant Entity_Id := Base_Type (Etype (N));
            Lo : constant Uint := Expr_Value (Type_Low_Bound (BT));
            Hi : constant Uint := Expr_Value (Type_High_Bound (BT));
         begin
            if Result < Lo or else Result > Hi then
               Apply_Compile_Time_Constraint_Error
                 (N, "value not in range of }??",
                  CE_Overflow_Check_Failed,
                  Ent => BT);
            end if;
         end;
      end if;
   end Check_Non_Static_Context_For_Overflow;

   ---------------------------------
   -- Check_String_Literal_Length --
   ---------------------------------

   procedure Check_String_Literal_Length (N : Node_Id; Ttype : Entity_Id) is
   begin
      if not Raises_Constraint_Error (N) and then Is_Constrained (Ttype) then
         if UI_From_Int (String_Length (Strval (N))) /= String_Type_Len (Ttype)
         then
            Apply_Compile_Time_Constraint_Error
              (N, "string length wrong for}??",
               CE_Length_Check_Failed,
               Ent => Ttype,
               Typ => Ttype);
         end if;
      end if;
   end Check_String_Literal_Length;

   --------------------------------------------
   -- Checking_Potentially_Static_Expression --
   --------------------------------------------

   function Checking_Potentially_Static_Expression return Boolean is
   begin
      return Checking_For_Potentially_Static_Expression;
   end Checking_Potentially_Static_Expression;

   --------------------
   -- Choice_Matches --
   --------------------

   function Choice_Matches
     (Expr   : Node_Id;
      Choice : Node_Id) return Match_Result
   is
      Etyp : constant Entity_Id := Etype (Expr);
      Val  : Uint;
      ValR : Ureal;
      ValS : Node_Id;

   begin
      pragma Assert (Compile_Time_Known_Value (Expr));
      pragma Assert (Is_Scalar_Type (Etyp) or else Is_String_Type (Etyp));

      if not Is_OK_Static_Choice (Choice) then
         Set_Raises_Constraint_Error (Choice);
         return Non_Static;

      --  When the choice denotes a subtype with a static predictate, check the
      --  expression against the predicate values. Different procedures apply
      --  to discrete and non-discrete types.

      elsif (Nkind (Choice) = N_Subtype_Indication
              or else (Is_Entity_Name (Choice)
                        and then Is_Type (Entity (Choice))))
        and then Has_Predicates (Etype (Choice))
        and then Has_Static_Predicate (Etype (Choice))
      then
         if Is_Discrete_Type (Etype (Choice)) then
            return
              Choices_Match
                (Expr, Static_Discrete_Predicate (Etype (Choice)));

         elsif Real_Or_String_Static_Predicate_Matches (Expr, Etype (Choice))
         then
            return Match;

         else
            return No_Match;
         end if;

      --  Discrete type case only

      elsif Is_Discrete_Type (Etyp) then
         Val := Expr_Value (Expr);

         if Nkind (Choice) = N_Range then
            if Val >= Expr_Value (Low_Bound (Choice))
                 and then
               Val <= Expr_Value (High_Bound (Choice))
            then
               return Match;
            else
               return No_Match;
            end if;

         elsif Nkind (Choice) = N_Subtype_Indication
           or else (Is_Entity_Name (Choice) and then Is_Type (Entity (Choice)))
         then
            if Val >= Expr_Value (Type_Low_Bound  (Etype (Choice)))
                 and then
               Val <= Expr_Value (Type_High_Bound (Etype (Choice)))
            then
               return Match;
            else
               return No_Match;
            end if;

         elsif Nkind (Choice) = N_Others_Choice then
            return Match;

         else
            if Val = Expr_Value (Choice) then
               return Match;
            else
               return No_Match;
            end if;
         end if;

      --  Real type case

      elsif Is_Real_Type (Etyp) then
         ValR := Expr_Value_R (Expr);

         if Nkind (Choice) = N_Range then
            if ValR >= Expr_Value_R (Low_Bound  (Choice))
                 and then
               ValR <= Expr_Value_R (High_Bound (Choice))
            then
               return Match;
            else
               return No_Match;
            end if;

         elsif Nkind (Choice) = N_Subtype_Indication
           or else (Is_Entity_Name (Choice) and then Is_Type (Entity (Choice)))
         then
            if ValR >= Expr_Value_R (Type_Low_Bound  (Etype (Choice)))
                 and then
               ValR <= Expr_Value_R (Type_High_Bound (Etype (Choice)))
            then
               return Match;
            else
               return No_Match;
            end if;

         else
            if ValR = Expr_Value_R (Choice) then
               return Match;
            else
               return No_Match;
            end if;
         end if;

      --  String type cases

      else
         pragma Assert (Is_String_Type (Etyp));
         ValS := Expr_Value_S (Expr);

         if Nkind (Choice) = N_Subtype_Indication
           or else (Is_Entity_Name (Choice) and then Is_Type (Entity (Choice)))
         then
            if not Is_Constrained (Etype (Choice)) then
               return Match;

            else
               declare
                  Typlen : constant Uint :=
                             String_Type_Len (Etype (Choice));
                  Strlen : constant Uint :=
                             UI_From_Int (String_Length (Strval (ValS)));
               begin
                  if Typlen = Strlen then
                     return Match;
                  else
                     return No_Match;
                  end if;
               end;
            end if;

         else
            if String_Equal (Strval (ValS), Strval (Expr_Value_S (Choice)))
            then
               return Match;
            else
               return No_Match;
            end if;
         end if;
      end if;
   end Choice_Matches;

   -------------------
   -- Choices_Match --
   -------------------

   function Choices_Match
     (Expr    : Node_Id;
      Choices : List_Id) return Match_Result
   is
      Choice : Node_Id;
      Result : Match_Result;

   begin
      Choice := First (Choices);
      while Present (Choice) loop
         Result := Choice_Matches (Expr, Choice);

         if Result /= No_Match then
            return Result;
         end if;

         Next (Choice);
      end loop;

      return No_Match;
   end Choices_Match;

   --------------------------
   -- Compile_Time_Compare --
   --------------------------

   function Compile_Time_Compare
     (L, R         : Node_Id;
      Assume_Valid : Boolean) return Compare_Result
   is
      Discard : aliased Uint;
   begin
      return Compile_Time_Compare (L, R, Discard'Access, Assume_Valid);
   end Compile_Time_Compare;

   function Compile_Time_Compare
     (L, R         : Node_Id;
      Diff         : access Uint;
      Assume_Valid : Boolean;
      Rec          : Boolean := False) return Compare_Result
   is
      Ltyp : Entity_Id := Etype (L);
      Rtyp : Entity_Id := Etype (R);

      Discard : aliased Uint;

      procedure Compare_Decompose
        (N : Node_Id;
         R : out Node_Id;
         V : out Uint);
      --  This procedure decomposes the node N into an expression node and a
      --  signed offset, so that the value of N is equal to the value of R plus
      --  the value V (which may be negative). If no such decomposition is
      --  possible, then on return R is a copy of N, and V is set to zero.

      function Compare_Fixup (N : Node_Id) return Node_Id;
      --  This function deals with replacing 'Last and 'First references with
      --  their corresponding type bounds, which we then can compare. The
      --  argument is the original node, the result is the identity, unless we
      --  have a 'Last/'First reference in which case the value returned is the
      --  appropriate type bound.

      function Is_Known_Valid_Operand (Opnd : Node_Id) return Boolean;
      --  Even if the context does not assume that values are valid, some
      --  simple cases can be recognized.

      function Is_Same_Value (L, R : Node_Id) return Boolean;
      --  Returns True iff L and R represent expressions that definitely have
      --  identical (but not necessarily compile-time-known) values Indeed the
      --  caller is expected to have already dealt with the cases of compile
      --  time known values, so these are not tested here.

      -----------------------
      -- Compare_Decompose --
      -----------------------

      procedure Compare_Decompose
        (N : Node_Id;
         R : out Node_Id;
         V : out Uint)
      is
      begin
         if Nkind (N) = N_Op_Add
           and then Nkind (Right_Opnd (N)) = N_Integer_Literal
         then
            R := Left_Opnd (N);
            V := Intval (Right_Opnd (N));
            return;

         elsif Nkind (N) = N_Op_Subtract
           and then Nkind (Right_Opnd (N)) = N_Integer_Literal
         then
            R := Left_Opnd (N);
            V := UI_Negate (Intval (Right_Opnd (N)));
            return;

         elsif Nkind (N) = N_Attribute_Reference then
            if Attribute_Name (N) = Name_Succ then
               R := First (Expressions (N));
               V := Uint_1;
               return;

            elsif Attribute_Name (N) = Name_Pred then
               R := First (Expressions (N));
               V := Uint_Minus_1;
               return;
            end if;
         end if;

         R := N;
         V := Uint_0;
      end Compare_Decompose;

      -------------------
      -- Compare_Fixup --
      -------------------

      function Compare_Fixup (N : Node_Id) return Node_Id is
         Indx : Node_Id;
         Xtyp : Entity_Id;
         Subs : Nat;

      begin
         --  Fixup only required for First/Last attribute reference

         if Nkind (N) = N_Attribute_Reference
           and then Attribute_Name (N) in Name_First | Name_Last
         then
            Xtyp := Etype (Prefix (N));

            --  If we have no type, then just abandon the attempt to do
            --  a fixup, this is probably the result of some other error.

            if No (Xtyp) then
               return N;
            end if;

            --  Dereference an access type

            if Is_Access_Type (Xtyp) then
               Xtyp := Designated_Type (Xtyp);
            end if;

            --  If we don't have an array type at this stage, something is
            --  peculiar, e.g. another error, and we abandon the attempt at
            --  a fixup.

            if not Is_Array_Type (Xtyp) then
               return N;
            end if;

            --  Ignore unconstrained array, since bounds are not meaningful

            if not Is_Constrained (Xtyp) then
               return N;
            end if;

            if Ekind (Xtyp) = E_String_Literal_Subtype then
               if Attribute_Name (N) = Name_First then
                  return String_Literal_Low_Bound (Xtyp);
               else
                  return
                    Make_Integer_Literal (Sloc (N),
                      Intval => Intval (String_Literal_Low_Bound (Xtyp)) +
                                          String_Literal_Length (Xtyp));
               end if;
            end if;

            --  Find correct index type

            Indx := First_Index (Xtyp);

            if Present (Expressions (N)) then
               Subs := UI_To_Int (Expr_Value (First (Expressions (N))));

               for J in 2 .. Subs loop
                  Next_Index (Indx);
               end loop;
            end if;

            Xtyp := Etype (Indx);

            if Attribute_Name (N) = Name_First then
               return Type_Low_Bound (Xtyp);
            else
               return Type_High_Bound (Xtyp);
            end if;
         end if;

         return N;
      end Compare_Fixup;

      ----------------------------
      -- Is_Known_Valid_Operand --
      ----------------------------

      function Is_Known_Valid_Operand (Opnd : Node_Id) return Boolean is
      begin
         return (Is_Entity_Name (Opnd)
                  and then
                    (Is_Known_Valid (Entity (Opnd))
                      or else Ekind (Entity (Opnd)) = E_In_Parameter
                      or else
                        (Is_Object (Entity (Opnd))
                          and then Present (Current_Value (Entity (Opnd))))))
           or else Is_OK_Static_Expression (Opnd);
      end Is_Known_Valid_Operand;

      -------------------
      -- Is_Same_Value --
      -------------------

      function Is_Same_Value (L, R : Node_Id) return Boolean is
         Lf : constant Node_Id := Compare_Fixup (L);
         Rf : constant Node_Id := Compare_Fixup (R);

         function Is_Rewritten_Loop_Entry (N : Node_Id) return Boolean;
         --  An attribute reference to Loop_Entry may have been rewritten into
         --  its prefix as a way to avoid generating a constant for that
         --  attribute when the corresponding pragma is ignored. These nodes
         --  should be ignored when deciding if they can be equal to one
         --  another.

         function Is_Same_Subscript (L, R : List_Id) return Boolean;
         --  L, R are the Expressions values from two attribute nodes for First
         --  or Last attributes. Either may be set to No_List if no expressions
         --  are present (indicating subscript 1). The result is True if both
         --  expressions represent the same subscript (note one case is where
         --  one subscript is missing and the other is explicitly set to 1).

         -----------------------------
         -- Is_Rewritten_Loop_Entry --
         -----------------------------

         function Is_Rewritten_Loop_Entry (N : Node_Id) return Boolean is
            Orig_N : constant Node_Id := Original_Node (N);
         begin
            return Orig_N /= N
              and then Nkind (Orig_N) = N_Attribute_Reference
              and then Get_Attribute_Id (Attribute_Name (Orig_N)) =
                Attribute_Loop_Entry;
         end Is_Rewritten_Loop_Entry;

         -----------------------
         -- Is_Same_Subscript --
         -----------------------

         function Is_Same_Subscript (L, R : List_Id) return Boolean is
         begin
            if L = No_List then
               if R = No_List then
                  return True;
               else
                  return Expr_Value (First (R)) = Uint_1;
               end if;

            else
               if R = No_List then
                  return Expr_Value (First (L)) = Uint_1;
               else
                  return Expr_Value (First (L)) = Expr_Value (First (R));
               end if;
            end if;
         end Is_Same_Subscript;

      --  Start of processing for Is_Same_Value

      begin
         --  Loop_Entry nodes rewritten into their prefix inside ignored
         --  pragmas should never lead to a decision of equality.

         if Is_Rewritten_Loop_Entry (Lf)
           or else Is_Rewritten_Loop_Entry (Rf)
         then
            return False;

         --  Values are the same if they refer to the same entity and the
         --  entity is nonvolatile.

         elsif Nkind (Lf) in N_Identifier | N_Expanded_Name
           and then Nkind (Rf) in N_Identifier | N_Expanded_Name
           and then Entity (Lf) = Entity (Rf)

           --  If the entity is a discriminant, the two expressions may be
           --  bounds of components of objects of the same discriminated type.
           --  The values of the discriminants are not static, and therefore
           --  the result is unknown.

           and then Ekind (Entity (Lf)) /= E_Discriminant
           and then Present (Entity (Lf))

           --  This does not however apply to Float types, since we may have
           --  two NaN values and they should never compare equal.

           and then not Is_Floating_Point_Type (Etype (L))
           and then not Is_Volatile_Reference (L)
           and then not Is_Volatile_Reference (R)
         then
            return True;

         --  Or if they are compile-time-known and identical

         elsif Compile_Time_Known_Value (Lf)
                 and then
               Compile_Time_Known_Value (Rf)
           and then Expr_Value (Lf) = Expr_Value (Rf)
         then
            return True;

         --  False if Nkind of the two nodes is different for remaining cases

         elsif Nkind (Lf) /= Nkind (Rf) then
            return False;

         --  True if both 'First or 'Last values applying to the same entity
         --  (first and last don't change even if value does). Note that we
         --  need this even with the calls to Compare_Fixup, to handle the
         --  case of unconstrained array attributes where Compare_Fixup
         --  cannot find useful bounds.

         elsif Nkind (Lf) = N_Attribute_Reference
           and then Attribute_Name (Lf) = Attribute_Name (Rf)
           and then Attribute_Name (Lf) in Name_First | Name_Last
           and then Nkind (Prefix (Lf)) in N_Identifier | N_Expanded_Name
           and then Nkind (Prefix (Rf)) in N_Identifier | N_Expanded_Name
           and then Entity (Prefix (Lf)) = Entity (Prefix (Rf))
           and then Is_Same_Subscript (Expressions (Lf), Expressions (Rf))
         then
            return True;

         --  True if the same selected component from the same record

         elsif Nkind (Lf) = N_Selected_Component
           and then Selector_Name (Lf) = Selector_Name (Rf)
           and then Is_Same_Value (Prefix (Lf), Prefix (Rf))
         then
            return True;

         --  True if the same unary operator applied to the same operand

         elsif Nkind (Lf) in N_Unary_Op
           and then Is_Same_Value (Right_Opnd (Lf), Right_Opnd (Rf))
         then
            return True;

         --  True if the same binary operator applied to the same operands

         elsif Nkind (Lf) in N_Binary_Op
           and then Is_Same_Value (Left_Opnd  (Lf), Left_Opnd  (Rf))
           and then Is_Same_Value (Right_Opnd (Lf), Right_Opnd (Rf))
         then
            return True;

         --  All other cases, we can't tell, so return False

         else
            return False;
         end if;
      end Is_Same_Value;

   --  Start of processing for Compile_Time_Compare

   begin
      Diff.all := No_Uint;

      --  In preanalysis mode, always return Unknown unless the expression
      --  is static. It is too early to be thinking we know the result of a
      --  comparison, save that judgment for the full analysis. This is
      --  particularly important in the case of pre and postconditions, which
      --  otherwise can be prematurely collapsed into having True or False
      --  conditions when this is inappropriate.

      if not (Full_Analysis
               or else (Is_OK_Static_Expression (L)
                          and then
                        Is_OK_Static_Expression (R)))
      then
         return Unknown;
      end if;

      --  If either operand could raise Constraint_Error, then we cannot
      --  know the result at compile time (since CE may be raised).

      if not (Cannot_Raise_Constraint_Error (L)
                and then
              Cannot_Raise_Constraint_Error (R))
      then
         return Unknown;
      end if;

      --  Identical operands are most certainly equal

      if L = R then
         return EQ;
      end if;

      --  If expressions have no types, then do not attempt to determine if
      --  they are the same, since something funny is going on. One case in
      --  which this happens is during generic template analysis, when bounds
      --  are not fully analyzed.

      if No (Ltyp) or else No (Rtyp) then
         return Unknown;
      end if;

      --  These get reset to the base type for the case of entities where
      --  Is_Known_Valid is not set. This takes care of handling possible
      --  invalid representations using the value of the base type, in
      --  accordance with RM 13.9.1(10).

      Ltyp := Underlying_Type (Ltyp);
      Rtyp := Underlying_Type (Rtyp);

      --  Same rationale as above, but for Underlying_Type instead of Etype

      if No (Ltyp) or else No (Rtyp) then
         return Unknown;
      end if;

      --  We do not attempt comparisons for packed arrays represented as
      --  modular types, where the semantics of comparison is quite different.

      if Is_Packed_Array_Impl_Type (Ltyp)
        and then Is_Modular_Integer_Type (Ltyp)
      then
         return Unknown;

      --  For access types, the only time we know the result at compile time
      --  (apart from identical operands, which we handled already) is if we
      --  know one operand is null and the other is not, or both operands are
      --  known null.

      elsif Is_Access_Type (Ltyp) then
         if Known_Null (L) then
            if Known_Null (R) then
               return EQ;
            elsif Known_Non_Null (R) then
               return NE;
            else
               return Unknown;
            end if;

         elsif Known_Non_Null (L) and then Known_Null (R) then
            return NE;

         else
            return Unknown;
         end if;

      --  Case where comparison involves two compile-time-known values

      elsif Compile_Time_Known_Value (L)
              and then
            Compile_Time_Known_Value (R)
      then
         --  For the floating-point case, we have to be a little careful, since
         --  at compile time we are dealing with universal exact values, but at
         --  runtime, these will be in non-exact target form. That's why the
         --  returned results are LE and GE below instead of LT and GT.

         if Is_Floating_Point_Type (Ltyp)
              or else
            Is_Floating_Point_Type (Rtyp)
         then
            declare
               Lo : constant Ureal := Expr_Value_R (L);
               Hi : constant Ureal := Expr_Value_R (R);
            begin
               if Lo < Hi then
                  return LE;
               elsif Lo = Hi then
                  return EQ;
               else
                  return GE;
               end if;
            end;

         --  For string types, we have two string literals and we proceed to
         --  compare them using the Ada style dictionary string comparison.

         elsif not Is_Scalar_Type (Ltyp) then
            declare
               Lstring : constant String_Id := Strval (Expr_Value_S (L));
               Rstring : constant String_Id := Strval (Expr_Value_S (R));
               Llen    : constant Nat       := String_Length (Lstring);
               Rlen    : constant Nat       := String_Length (Rstring);

            begin
               for J in 1 .. Nat'Min (Llen, Rlen) loop
                  declare
                     LC : constant Char_Code := Get_String_Char (Lstring, J);
                     RC : constant Char_Code := Get_String_Char (Rstring, J);
                  begin
                     if LC < RC then
                        return LT;
                     elsif LC > RC then
                        return GT;
                     end if;
                  end;
               end loop;

               if Llen < Rlen then
                  return LT;
               elsif Llen > Rlen then
                  return GT;
               else
                  return EQ;
               end if;
            end;

         --  For remaining scalar cases we know exactly (note that this does
         --  include the fixed-point case, where we know the run time integer
         --  values now).

         else
            declare
               Lo : constant Uint := Expr_Value (L);
               Hi : constant Uint := Expr_Value (R);
            begin
               if Lo < Hi then
                  Diff.all := Hi - Lo;
                  return LT;
               elsif Lo = Hi then
                  return EQ;
               else
                  Diff.all := Lo - Hi;
                  return GT;
               end if;
            end;
         end if;

      --  Cases where at least one operand is not known at compile time

      else
         --  Remaining checks apply only for discrete types

         if not Is_Discrete_Type (Ltyp)
              or else
            not Is_Discrete_Type (Rtyp)
         then
            return Unknown;
         end if;

         --  Defend against generic types, or actually any expressions that
         --  contain a reference to a generic type from within a generic
         --  template. We don't want to do any range analysis of such
         --  expressions for two reasons. First, the bounds of a generic type
         --  itself are junk and cannot be used for any kind of analysis.
         --  Second, we may have a case where the range at run time is indeed
         --  known, but we don't want to do compile time analysis in the
         --  template based on that range since in an instance the value may be
         --  static, and able to be elaborated without reference to the bounds
         --  of types involved. As an example, consider:

         --     (F'Pos (F'Last) + 1) > Integer'Last

         --  The expression on the left side of > is Universal_Integer and thus
         --  acquires the type Integer for evaluation at run time, and at run
         --  time it is true that this condition is always False, but within
         --  an instance F may be a type with a static range greater than the
         --  range of Integer, and the expression statically evaluates to True.

         if References_Generic_Formal_Type (L)
              or else
            References_Generic_Formal_Type (R)
         then
            return Unknown;
         end if;

         --  Replace types by base types for the case of values which are not
         --  known to have valid representations. This takes care of properly
         --  dealing with invalid representations.

         if not Assume_Valid then
            if not (Is_Entity_Name (L)
                     and then (Is_Known_Valid (Entity (L))
                                or else Assume_No_Invalid_Values))
            then
               Ltyp := Underlying_Type (Base_Type (Ltyp));
            end if;

            if not (Is_Entity_Name (R)
                     and then (Is_Known_Valid (Entity (R))
                                or else Assume_No_Invalid_Values))
            then
               Rtyp := Underlying_Type (Base_Type (Rtyp));
            end if;
         end if;

         --  First attempt is to decompose the expressions to extract a
         --  constant offset resulting from the use of any of the forms:

         --     expr + literal
         --     expr - literal
         --     typ'Succ (expr)
         --     typ'Pred (expr)

         --  Then we see if the two expressions are the same value, and if so
         --  the result is obtained by comparing the offsets.

         --  Note: the reason we do this test first is that it returns only
         --  decisive results (with diff set), where other tests, like the
         --  range test, may not be as so decisive. Consider for example
         --  J .. J + 1. This code can conclude LT with a difference of 1,
         --  even if the range of J is not known.

         declare
            Lnode : Node_Id;
            Loffs : Uint;
            Rnode : Node_Id;
            Roffs : Uint;

         begin
            Compare_Decompose (L, Lnode, Loffs);
            Compare_Decompose (R, Rnode, Roffs);

            if Is_Same_Value (Lnode, Rnode) then
               if Loffs = Roffs then
                  return EQ;
               end if;

               --  When the offsets are not equal, we can go farther only if
               --  the types are not modular (e.g. X < X + 1 is False if X is
               --  the largest number).

               if not Is_Modular_Integer_Type (Ltyp)
                 and then not Is_Modular_Integer_Type (Rtyp)
               then
                  if Loffs < Roffs then
                     Diff.all := Roffs - Loffs;
                     return LT;
                  else
                     Diff.all := Loffs - Roffs;
                     return GT;
                  end if;
               end if;
            end if;
         end;

         --  Next, try range analysis and see if operand ranges are disjoint

         declare
            LOK, ROK : Boolean;
            LLo, LHi : Uint;
            RLo, RHi : Uint;

            Single : Boolean;
            --  True if each range is a single point

         begin
            Determine_Range (L, LOK, LLo, LHi, Assume_Valid);
            Determine_Range (R, ROK, RLo, RHi, Assume_Valid);

            if LOK and ROK then
               Single := LLo = LHi and then RLo = RHi;

               if LHi < RLo then
                  if Single and Assume_Valid then
                     Diff.all := RLo - LLo;
                  end if;

                  return LT;

               elsif RHi < LLo then
                  if Single and Assume_Valid then
                     Diff.all := LLo - RLo;
                  end if;

                  return GT;

               elsif Single and then LLo = RLo then

                  --  If the range includes a single literal and we can assume
                  --  validity then the result is known even if an operand is
                  --  not static.

                  if Assume_Valid then
                     return EQ;
                  else
                     return Unknown;
                  end if;

               elsif LHi = RLo then
                  return LE;

               elsif RHi = LLo then
                  return GE;

               elsif not Is_Known_Valid_Operand (L)
                 and then not Assume_Valid
               then
                  if Is_Same_Value (L, R) then
                     return EQ;
                  else
                     return Unknown;
                  end if;
               end if;

            --  If the range of either operand cannot be determined, nothing
            --  further can be inferred.

            else
               return Unknown;
            end if;
         end;

         --  Here is where we check for comparisons against maximum bounds of
         --  types, where we know that no value can be outside the bounds of
         --  the subtype. Note that this routine is allowed to assume that all
         --  expressions are within their subtype bounds. Callers wishing to
         --  deal with possibly invalid values must in any case take special
         --  steps (e.g. conversions to larger types) to avoid this kind of
         --  optimization, which is always considered to be valid. We do not
         --  attempt this optimization with generic types, since the type
         --  bounds may not be meaningful in this case.

         --  We are in danger of an infinite recursion here. It does not seem
         --  useful to go more than one level deep, so the parameter Rec is
         --  used to protect ourselves against this infinite recursion.

         if not Rec then

            --  See if we can get a decisive check against one operand and a
            --  bound of the other operand (four possible tests here). Note
            --  that we avoid testing junk bounds of a generic type.

            if not Is_Generic_Type (Rtyp) then
               case Compile_Time_Compare (L, Type_Low_Bound (Rtyp),
                                          Discard'Access,
                                          Assume_Valid, Rec => True)
               is
                  when LT => return LT;
                  when LE => return LE;
                  when EQ => return LE;
                  when others => null;
               end case;

               case Compile_Time_Compare (L, Type_High_Bound (Rtyp),
                                          Discard'Access,
                                          Assume_Valid, Rec => True)
               is
                  when GT => return GT;
                  when GE => return GE;
                  when EQ => return GE;
                  when others => null;
               end case;
            end if;

            if not Is_Generic_Type (Ltyp) then
               case Compile_Time_Compare (Type_Low_Bound (Ltyp), R,
                                          Discard'Access,
                                          Assume_Valid, Rec => True)
               is
                  when GT => return GT;
                  when GE => return GE;
                  when EQ => return GE;
                  when others => null;
               end case;

               case Compile_Time_Compare (Type_High_Bound (Ltyp), R,
                                          Discard'Access,
                                          Assume_Valid, Rec => True)
               is
                  when LT => return LT;
                  when LE => return LE;
                  when EQ => return LE;
                  when others => null;
               end case;
            end if;
         end if;

         --  Next attempt is to see if we have an entity compared with a
         --  compile-time-known value, where there is a current value
         --  conditional for the entity which can tell us the result.

         declare
            Var : Node_Id;
            --  Entity variable (left operand)

            Val : Uint;
            --  Value (right operand)

            Inv : Boolean;
            --  If False, we have reversed the operands

            Op : Node_Kind;
            --  Comparison operator kind from Get_Current_Value_Condition call

            Opn : Node_Id;
            --  Value from Get_Current_Value_Condition call

            Opv : Uint;
            --  Value of Opn

            Result : Compare_Result;
            --  Known result before inversion

         begin
            if Is_Entity_Name (L)
              and then Compile_Time_Known_Value (R)
            then
               Var := L;
               Val := Expr_Value (R);
               Inv := False;

            elsif Is_Entity_Name (R)
              and then Compile_Time_Known_Value (L)
            then
               Var := R;
               Val := Expr_Value (L);
               Inv := True;

               --  That was the last chance at finding a compile time result

            else
               return Unknown;
            end if;

            Get_Current_Value_Condition (Var, Op, Opn);

            --  That was the last chance, so if we got nothing return

            if No (Opn) then
               return Unknown;
            end if;

            Opv := Expr_Value (Opn);

            --  We got a comparison, so we might have something interesting

            --  Convert LE to LT and GE to GT, just so we have fewer cases

            if Op = N_Op_Le then
               Op := N_Op_Lt;
               Opv := Opv + 1;

            elsif Op = N_Op_Ge then
               Op := N_Op_Gt;
               Opv := Opv - 1;
            end if;

            --  Deal with equality case

            if Op = N_Op_Eq then
               if Val = Opv then
                  Result := EQ;
               elsif Opv < Val then
                  Result := LT;
               else
                  Result := GT;
               end if;

            --  Deal with inequality case

            elsif Op = N_Op_Ne then
               if Val = Opv then
                  Result := NE;
               else
                  return Unknown;
               end if;

            --  Deal with greater than case

            elsif Op = N_Op_Gt then
               if Opv >= Val then
                  Result := GT;
               elsif Opv = Val - 1 then
                  Result := GE;
               else
                  return Unknown;
               end if;

            --  Deal with less than case

            else pragma Assert (Op = N_Op_Lt);
               if Opv <= Val then
                  Result := LT;
               elsif Opv = Val + 1 then
                  Result := LE;
               else
                  return Unknown;
               end if;
            end if;

            --  Deal with inverting result

            if Inv then
               case Result is
                  when GT     => return LT;
                  when GE     => return LE;
                  when LT     => return GT;
                  when LE     => return GE;
                  when others => return Result;
               end case;
            end if;

            return Result;
         end;
      end if;
   end Compile_Time_Compare;

   -------------------------------
   -- Compile_Time_Known_Bounds --
   -------------------------------

   function Compile_Time_Known_Bounds (T : Entity_Id) return Boolean is
      Indx : Node_Id;
      Typ  : Entity_Id;

   begin
      if T = Any_Composite or else not Is_Array_Type (T) then
         return False;
      end if;

      Indx := First_Index (T);
      while Present (Indx) loop
         Typ := Underlying_Type (Etype (Indx));

         --  Never look at junk bounds of a generic type

         if Is_Generic_Type (Typ) then
            return False;
         end if;

         --  Otherwise check bounds for compile-time-known

         if not Compile_Time_Known_Value (Type_Low_Bound (Typ)) then
            return False;
         elsif not Compile_Time_Known_Value (Type_High_Bound (Typ)) then
            return False;
         else
            Next_Index (Indx);
         end if;
      end loop;

      return True;
   end Compile_Time_Known_Bounds;

   ------------------------------
   -- Compile_Time_Known_Value --
   ------------------------------

   function Compile_Time_Known_Value (Op : Node_Id) return Boolean is
      K      : constant Node_Kind := Nkind (Op);
      CV_Ent : CV_Entry renames CV_Cache (Nat (Op) mod CV_Cache_Size);

   begin
      --  Never known at compile time if bad type or raises Constraint_Error
      --  or empty (which can occur as a result of a previous error or in the
      --  case of e.g. an imported constant).

      if No (Op) then
         return False;

      elsif Op = Error
        or else Nkind (Op) not in N_Has_Etype
        or else Etype (Op) = Any_Type
        or else Raises_Constraint_Error (Op)
      then
         return False;
      end if;

      --  If we have an entity name, then see if it is the name of a constant
      --  and if so, test the corresponding constant value, or the name of an
      --  enumeration literal, which is always a constant.

      if Present (Etype (Op)) and then Is_Entity_Name (Op) then
         declare
            Ent : constant Entity_Id := Entity (Op);
            Val : Node_Id;

         begin
            --  Never known at compile time if it is a packed array value. We
            --  might want to try to evaluate these at compile time one day,
            --  but we do not make that attempt now.

            if Is_Packed_Array_Impl_Type (Etype (Op)) then
               return False;

            elsif Ekind (Ent) = E_Enumeration_Literal then
               return True;

            elsif Ekind (Ent) = E_Constant then
               Val := Constant_Value (Ent);

               if Present (Val) then

                  --  Guard against an illegal deferred constant whose full
                  --  view is initialized with a reference to itself. Treat
                  --  this case as a value not known at compile time.

                  if Is_Entity_Name (Val) and then Entity (Val) = Ent then
                     return False;
                  else
                     return Compile_Time_Known_Value (Val);
                  end if;

               --  Otherwise, the constant does not have a compile-time-known
               --  value.

               else
                  return False;
               end if;
            end if;
         end;

      --  We have a value, see if it is compile-time-known

      else
         --  Integer literals are worth storing in the cache

         if K = N_Integer_Literal then
            CV_Ent.N := Op;
            CV_Ent.V := Intval (Op);
            return True;

         --  Other literals and NULL are known at compile time

         elsif K in
           N_Character_Literal | N_Real_Literal | N_String_Literal | N_Null
         then
            return True;

         --  Evaluate static discriminants, to eliminate dead paths and
         --  redundant discriminant checks.

         elsif Is_Static_Discriminant_Component (Op) then
            return True;
         end if;
      end if;

      --  If we fall through, not known at compile time

      return False;

   --  If we get an exception while trying to do this test, then some error
   --  has occurred, and we simply say that the value is not known after all

   exception
      when others =>
         --  With debug flag K we will get an exception unless an error has
         --  already occurred (useful for debugging).

         if Debug_Flag_K then
            Check_Error_Detected;
         end if;

         return False;
   end Compile_Time_Known_Value;

   ---------------------------------------
   -- CRT_Safe_Compile_Time_Known_Value --
   ---------------------------------------

   function CRT_Safe_Compile_Time_Known_Value (Op : Node_Id) return Boolean is
   begin
      if (Configurable_Run_Time_Mode or No_Run_Time_Mode)
        and then not Is_OK_Static_Expression (Op)
      then
         return False;
      else
         return Compile_Time_Known_Value (Op);
      end if;
   end CRT_Safe_Compile_Time_Known_Value;

   -----------------
   -- Eval_Actual --
   -----------------

   --  This is only called for actuals of functions that are not predefined
   --  operators (which have already been rewritten as operators at this
   --  stage), so the call can never be folded, and all that needs doing for
   --  the actual is to do the check for a non-static context.

   procedure Eval_Actual (N : Node_Id) is
   begin
      Check_Non_Static_Context (N);
   end Eval_Actual;

   --------------------
   -- Eval_Allocator --
   --------------------

   --  Allocators are never static, so all we have to do is to do the
   --  check for a non-static context if an expression is present.

   procedure Eval_Allocator (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);
   begin
      if Nkind (Expr) = N_Qualified_Expression then
         Check_Non_Static_Context (Expression (Expr));
      end if;
   end Eval_Allocator;

   ------------------------
   -- Eval_Arithmetic_Op --
   ------------------------

   --  Arithmetic operations are static functions, so the result is static
   --  if both operands are static (RM 4.9(7), 4.9(20)).

   procedure Eval_Arithmetic_Op (N : Node_Id) is
      Left  : constant Node_Id   := Left_Opnd (N);
      Right : constant Node_Id   := Right_Opnd (N);
      Ltype : constant Entity_Id := Etype (Left);
      Rtype : constant Entity_Id := Etype (Right);
      Otype : Entity_Id          := Empty;
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Otherwise attempt to fold

      if Is_Universal_Numeric_Type (Etype (Left))
           and then
         Is_Universal_Numeric_Type (Etype (Right))
      then
         Otype := Find_Universal_Operator_Type (N);
      end if;

      --  Fold for cases where both operands are of integer type

      if Is_Integer_Type (Ltype) and then Is_Integer_Type (Rtype) then
         declare
            Left_Int  : constant Uint := Expr_Value (Left);
            Right_Int : constant Uint := Expr_Value (Right);
            Result    : Uint;

         begin
            case Nkind (N) is
               when N_Op_Add =>
                  Result := Left_Int + Right_Int;

               when N_Op_Subtract =>
                  Result := Left_Int - Right_Int;

               when N_Op_Multiply =>
                  if OK_Bits
                       (N, UI_From_Int
                             (Num_Bits (Left_Int) + Num_Bits (Right_Int)))
                  then
                     Result := Left_Int * Right_Int;
                  else
                     Result := Left_Int;
                  end if;

               when N_Op_Divide =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then

                     --  When SPARK_Mode is On, force a warning instead of
                     --  an error in that case, as this likely corresponds
                     --  to deactivated code.

                     Apply_Compile_Time_Constraint_Error
                       (N, "division by zero", CE_Divide_By_Zero,
                        Loc  => Sloc (Right),
                        Warn => not Stat or SPARK_Mode = On);
                     return;

                  --  Otherwise we can do the division

                  else
                     Result := Left_Int / Right_Int;
                  end if;

               when N_Op_Mod =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then

                     --  When SPARK_Mode is On, force a warning instead of
                     --  an error in that case, as this likely corresponds
                     --  to deactivated code.

                     Apply_Compile_Time_Constraint_Error
                       (N, "mod with zero divisor", CE_Divide_By_Zero,
                        Loc  => Sloc (Right),
                        Warn => not Stat or SPARK_Mode = On);
                     return;

                  else
                     Result := Left_Int mod Right_Int;
                  end if;

               when N_Op_Rem =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then

                     --  When SPARK_Mode is On, force a warning instead of
                     --  an error in that case, as this likely corresponds
                     --  to deactivated code.

                     Apply_Compile_Time_Constraint_Error
                       (N, "rem with zero divisor", CE_Divide_By_Zero,
                        Loc  => Sloc (Right),
                        Warn => not Stat or SPARK_Mode = On);
                     return;

                  else
                     Result := Left_Int rem Right_Int;
                  end if;

               when others =>
                  raise Program_Error;
            end case;

            --  Adjust the result by the modulus if the type is a modular type

            if Is_Modular_Integer_Type (Ltype) then
               Result := Result mod Modulus (Ltype);
            end if;

            Check_Non_Static_Context_For_Overflow (N, Stat, Result);

            --  If we get here we can fold the result

            Fold_Uint (N, Result, Stat);
         end;

      --  Cases where at least one operand is a real. We handle the cases of
      --  both reals, or mixed/real integer cases (the latter happen only for
      --  divide and multiply, and the result is always real).

      elsif Is_Real_Type (Ltype) or else Is_Real_Type (Rtype) then
         declare
            Left_Real  : Ureal;
            Right_Real : Ureal;
            Result     : Ureal;

         begin
            if Is_Real_Type (Ltype) then
               Left_Real := Expr_Value_R (Left);
            else
               Left_Real := UR_From_Uint (Expr_Value (Left));
            end if;

            if Is_Real_Type (Rtype) then
               Right_Real := Expr_Value_R (Right);
            else
               Right_Real := UR_From_Uint (Expr_Value (Right));
            end if;

            if Nkind (N) = N_Op_Add then
               Result := Left_Real + Right_Real;

            elsif Nkind (N) = N_Op_Subtract then
               Result := Left_Real - Right_Real;

            elsif Nkind (N) = N_Op_Multiply then
               Result := Left_Real * Right_Real;

            else pragma Assert (Nkind (N) = N_Op_Divide);
               if UR_Is_Zero (Right_Real) then
                  Apply_Compile_Time_Constraint_Error
                    (N, "division by zero", CE_Divide_By_Zero,
                     Loc => Sloc (Right));
                  return;
               end if;

               Result := Left_Real / Right_Real;
            end if;

            Fold_Ureal (N, Result, Stat);
         end;
      end if;

      --  If the operator was resolved to a specific type, make sure that type
      --  is frozen even if the expression is folded into a literal (which has
      --  a universal type).

      if Present (Otype) then
         Freeze_Before (N, Otype);
      end if;
   end Eval_Arithmetic_Op;

   ----------------------------
   -- Eval_Character_Literal --
   ----------------------------

   --  Nothing to be done

   procedure Eval_Character_Literal (N : Node_Id) is
      pragma Warnings (Off, N);
   begin
      null;
   end Eval_Character_Literal;

   ---------------
   -- Eval_Call --
   ---------------

   --  Static function calls are either calls to predefined operators
   --  with static arguments, or calls to functions that rename a literal.
   --  Only the latter case is handled here, predefined operators are
   --  constant-folded elsewhere.

   --  If the function is itself inherited the literal of the parent type must
   --  be explicitly converted to the return type of the function.

   procedure Eval_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Lit : Entity_Id;

   begin
      if Nkind (N) = N_Function_Call
        and then No (Parameter_Associations (N))
        and then Is_Entity_Name (Name (N))
        and then Present (Alias (Entity (Name (N))))
        and then Is_Enumeration_Type (Base_Type (Typ))
      then
         Lit := Ultimate_Alias (Entity (Name (N)));

         if Ekind (Lit) = E_Enumeration_Literal then
            if Base_Type (Etype (Lit)) /= Base_Type (Typ) then
               Rewrite
                 (N, Convert_To (Typ, New_Occurrence_Of (Lit, Loc)));
            else
               Rewrite (N, New_Occurrence_Of (Lit, Loc));
            end if;

            Resolve (N, Typ);
         end if;

      elsif Nkind (N) = N_Function_Call
        and then Is_Entity_Name (Name (N))
        and then Is_Intrinsic_Subprogram (Entity (Name (N)))
      then
         Eval_Intrinsic_Call (N, Entity (Name (N)));

      --  Ada 2022 (AI12-0075): If checking for potentially static expressions
      --  is enabled and we have a call to a static function, substitute a
      --  static value for the call, to allow folding the expression. This
      --  supports checking the requirement of RM 6.8(5.3/5) in
      --  Analyze_Expression_Function.

      elsif Checking_Potentially_Static_Expression
        and then Is_Static_Function_Call (N)
      then
         Fold_Dummy (N, Typ);
      end if;
   end Eval_Call;

   --------------------------
   -- Eval_Case_Expression --
   --------------------------

   --  A conditional expression is static if all its conditions and dependent
   --  expressions are static. Note that we do not care if the dependent
   --  expressions raise CE, except for the one that will be selected.

   procedure Eval_Case_Expression (N : Node_Id) is
      Alt    : Node_Id;
      Choice : Node_Id;

   begin
      Set_Is_Static_Expression (N, False);

      if Error_Posted (Expression (N))
        or else not Is_Static_Expression (Expression (N))
      then
         Check_Non_Static_Context (Expression (N));
         return;
      end if;

      --  First loop, make sure all the alternatives are static expressions
      --  none of which raise Constraint_Error. We make the Constraint_Error
      --  check because part of the legality condition for a correct static
      --  case expression is that the cases are covered, like any other case
      --  expression. And we can't do that if any of the conditions raise an
      --  exception, so we don't even try to evaluate if that is the case.

      Alt := First (Alternatives (N));
      while Present (Alt) loop

         --  The expression must be static, but we don't care at this stage
         --  if it raises Constraint_Error (the alternative might not match,
         --  in which case the expression is statically unevaluated anyway).

         if not Is_Static_Expression (Expression (Alt)) then
            Check_Non_Static_Context (Expression (Alt));
            return;
         end if;

         --  The choices of a case always have to be static, and cannot raise
         --  an exception. If this condition is not met, then the expression
         --  is plain illegal, so just abandon evaluation attempts. No need
         --  to check non-static context when we have something illegal anyway.

         if not Is_OK_Static_Choice_List (Discrete_Choices (Alt)) then
            return;
         end if;

         Next (Alt);
      end loop;

      --  OK, if the above loop gets through it means that all choices are OK
      --  static (don't raise exceptions), so the whole case is static, and we
      --  can find the matching alternative.

      Set_Is_Static_Expression (N);

      --  Now to deal with propagating a possible Constraint_Error

      --  If the selecting expression raises CE, propagate and we are done

      if Raises_Constraint_Error (Expression (N)) then
         Set_Raises_Constraint_Error (N);

      --  Otherwise we need to check the alternatives to find the matching
      --  one. CE's in other than the matching one are not relevant. But we
      --  do need to check the matching one. Unlike the first loop, we do not
      --  have to go all the way through, when we find the matching one, quit.

      else
         Alt := First (Alternatives (N));
         Search : loop

            --  We must find a match among the alternatives. If not, this must
            --  be due to other errors, so just ignore, leaving as non-static.

            if No (Alt) then
               Set_Is_Static_Expression (N, False);
               return;
            end if;

            --  Otherwise loop through choices of this alternative

            Choice := First (Discrete_Choices (Alt));
            while Present (Choice) loop

               --  If we find a matching choice, then the Expression of this
               --  alternative replaces N (Raises_Constraint_Error flag is
               --  included, so we don't have to special case that).

               if Choice_Matches (Expression (N), Choice) = Match then
                  Rewrite (N, Relocate_Node (Expression (Alt)));
                  return;
               end if;

               Next (Choice);
            end loop;

            Next (Alt);
         end loop Search;
      end if;
   end Eval_Case_Expression;

   ------------------------
   -- Eval_Concatenation --
   ------------------------

   --  Concatenation is a static function, so the result is static if both
   --  operands are static (RM 4.9(7), 4.9(21)).

   procedure Eval_Concatenation (N : Node_Id) is
      Left  : constant Node_Id   := Left_Opnd (N);
      Right : constant Node_Id   := Right_Opnd (N);
      C_Typ : constant Entity_Id := Root_Type (Component_Type (Etype (N)));
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  Concatenation is never static in Ada 83, so if Ada 83 check operand
      --  non-static context.

      if Ada_Version = Ada_83
        and then Comes_From_Source (N)
      then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  If not foldable we are done. In principle concatenation that yields
      --  any string type is static (i.e. an array type of character types).
      --  However, character types can include enumeration literals, and
      --  concatenation in that case cannot be described by a literal, so we
      --  only consider the operation static if the result is an array of
      --  (a descendant of) a predefined character type.

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not (Is_Standard_Character_Type (C_Typ) and then Fold) then
         Set_Is_Static_Expression (N, False);
         return;
      end if;

      --  Compile time string concatenation

      --  ??? Note that operands that are aggregates can be marked as static,
      --  so we should attempt at a later stage to fold concatenations with
      --  such aggregates.

      declare
         Left_Str   : constant Node_Id := Get_String_Val (Left);
         Left_Len   : Nat;
         Right_Str  : constant Node_Id := Get_String_Val (Right);
         Folded_Val : String_Id        := No_String;

      begin
         --  Establish new string literal, and store left operand. We make
         --  sure to use the special Start_String that takes an operand if
         --  the left operand is a string literal. Since this is optimized
         --  in the case where that is the most recently created string
         --  literal, we ensure efficient time/space behavior for the
         --  case of a concatenation of a series of string literals.

         if Nkind (Left_Str) = N_String_Literal then
            Left_Len := String_Length (Strval (Left_Str));

            --  If the left operand is the empty string, and the right operand
            --  is a string literal (the case of "" & "..."), the result is the
            --  value of the right operand. This optimization is important when
            --  Is_Folded_In_Parser, to avoid copying an enormous right
            --  operand.

            if Left_Len = 0 and then Nkind (Right_Str) = N_String_Literal then
               Folded_Val := Strval (Right_Str);
            else
               Start_String (Strval (Left_Str));
            end if;

         else
            Start_String;
            Store_String_Char (UI_To_CC (Char_Literal_Value (Left_Str)));
            Left_Len := 1;
         end if;

         --  Now append the characters of the right operand, unless we
         --  optimized the "" & "..." case above.

         if Nkind (Right_Str) = N_String_Literal then
            if Left_Len /= 0 then
               Store_String_Chars (Strval (Right_Str));
               Folded_Val := End_String;
            end if;
         else
            Store_String_Char (UI_To_CC (Char_Literal_Value (Right_Str)));
            Folded_Val := End_String;
         end if;

         Set_Is_Static_Expression (N, Stat);

         --  If left operand is the empty string, the result is the
         --  right operand, including its bounds if anomalous.

         if Left_Len = 0
           and then Is_Array_Type (Etype (Right))
           and then Etype (Right) /= Any_String
         then
            Set_Etype (N, Etype (Right));
         end if;

         Fold_Str (N, Folded_Val, Static => Stat);
      end;
   end Eval_Concatenation;

   ----------------------
   -- Eval_Entity_Name --
   ----------------------

   --  This procedure is used for identifiers and expanded names other than
   --  named numbers (see Eval_Named_Integer, Eval_Named_Real. These are
   --  static if they denote a static constant (RM 4.9(6)) or if the name
   --  denotes an enumeration literal (RM 4.9(22)).

   procedure Eval_Entity_Name (N : Node_Id) is
      Def_Id : constant Entity_Id := Entity (N);
      Val    : Node_Id;

   begin
      --  Enumeration literals are always considered to be constants
      --  and cannot raise Constraint_Error (RM 4.9(22)).

      if Ekind (Def_Id) = E_Enumeration_Literal then
         Set_Is_Static_Expression (N);
         return;

      --  A name is static if it denotes a static constant (RM 4.9(5)), and
      --  we also copy Raise_Constraint_Error. Notice that even if non-static,
      --  it does not violate 10.2.1(8) here, since this is not a variable.

      elsif Ekind (Def_Id) = E_Constant then

         --  Deferred constants must always be treated as nonstatic outside the
         --  scope of their full view.

         if Present (Full_View (Def_Id))
           and then not In_Open_Scopes (Scope (Def_Id))
         then
            Val := Empty;
         else
            Val := Constant_Value (Def_Id);
         end if;

         if Present (Val) then
            Set_Is_Static_Expression
              (N, Is_Static_Expression (Val)
                    and then Is_Static_Subtype (Etype (Def_Id)));
            Set_Raises_Constraint_Error (N, Raises_Constraint_Error (Val));

            if not Is_Static_Expression (N)
              and then not Is_Generic_Type (Etype (N))
            then
               Validate_Static_Object_Name (N);
            end if;

            --  Mark constant condition in SCOs

            if Generate_SCO
              and then Comes_From_Source (N)
              and then Is_Boolean_Type (Etype (Def_Id))
              and then Compile_Time_Known_Value (N)
            then
               Set_SCO_Condition (N, Expr_Value_E (N) = Standard_True);
            end if;

            return;
         end if;

      --  Ada 2022 (AI12-0075): If checking for potentially static expressions
      --  is enabled and we have a reference to a formal parameter of mode in,
      --  substitute a static value for the reference, to allow folding the
      --  expression. This supports checking the requirement of RM 6.8(5.3/5)
      --  in Analyze_Expression_Function.

      elsif Ekind (Def_Id) = E_In_Parameter
        and then Checking_Potentially_Static_Expression
        and then Is_Static_Function (Scope (Def_Id))
      then
         Fold_Dummy (N, Etype (Def_Id));
      end if;

      --  Fall through if the name is not static

      Validate_Static_Object_Name (N);
   end Eval_Entity_Name;

   ------------------------
   -- Eval_If_Expression --
   ------------------------

   --  We can fold to a static expression if the condition and both dependent
   --  expressions are static. Otherwise, the only required processing is to do
   --  the check for non-static context for the then and else expressions.

   procedure Eval_If_Expression (N : Node_Id) is
      Condition  : constant Node_Id := First (Expressions (N));
      Then_Expr  : constant Node_Id := Next (Condition);
      Else_Expr  : constant Node_Id := Next (Then_Expr);
      Result     : Node_Id;
      Non_Result : Node_Id;

      Rstat : constant Boolean :=
                Is_Static_Expression (Condition)
                  and then
                Is_Static_Expression (Then_Expr)
                  and then
                Is_Static_Expression (Else_Expr);
      --  True if result is static

   begin
      --  If result not static, nothing to do, otherwise set static result

      if not Rstat then
         return;
      else
         Set_Is_Static_Expression (N);
      end if;

      --  If any operand is Any_Type, just propagate to result and do not try
      --  to fold, this prevents cascaded errors.

      if Etype (Condition) = Any_Type or else
         Etype (Then_Expr) = Any_Type or else
         Etype (Else_Expr) = Any_Type
      then
         Set_Etype (N, Any_Type);
         Set_Is_Static_Expression (N, False);
         return;
      end if;

      --  If condition raises Constraint_Error then we have already signaled
      --  an error, and we just propagate to the result and do not fold.

      if Raises_Constraint_Error (Condition) then
         Set_Raises_Constraint_Error (N);
         return;
      end if;

      --  Static case where we can fold. Note that we don't try to fold cases
      --  where the condition is known at compile time, but the result is
      --  non-static. This avoids possible cases of infinite recursion where
      --  the expander puts in a redundant test and we remove it. Instead we
      --  deal with these cases in the expander.

      --  Select result operand

      if Is_True (Expr_Value (Condition)) then
         Result     := Then_Expr;
         Non_Result := Else_Expr;
      else
         Result     := Else_Expr;
         Non_Result := Then_Expr;
      end if;

      --  Note that it does not matter if the non-result operand raises a
      --  Constraint_Error, but if the result raises Constraint_Error then we
      --  replace the node with a raise Constraint_Error. This will properly
      --  propagate Raises_Constraint_Error since this flag is set in Result.

      if Raises_Constraint_Error (Result) then
         Rewrite_In_Raise_CE (N, Result);
         Check_Non_Static_Context (Non_Result);

      --  Otherwise the result operand replaces the original node

      else
         Rewrite (N, Relocate_Node (Result));
         Set_Is_Static_Expression (N);
      end if;
   end Eval_If_Expression;

   ----------------------------
   -- Eval_Indexed_Component --
   ----------------------------

   --  Indexed components are never static, so we need to perform the check
   --  for non-static context on the index values. Then, we check if the
   --  value can be obtained at compile time, even though it is non-static.

   procedure Eval_Indexed_Component (N : Node_Id) is
      Expr : Node_Id;

   begin
      --  Check for non-static context on index values

      Expr := First (Expressions (N));
      while Present (Expr) loop
         Check_Non_Static_Context (Expr);
         Next (Expr);
      end loop;

      --  If the indexed component appears in an object renaming declaration
      --  then we do not want to try to evaluate it, since in this case we
      --  need the identity of the array element.

      if Nkind (Parent (N)) = N_Object_Renaming_Declaration then
         return;

      --  Similarly if the indexed component appears as the prefix of an
      --  attribute we don't want to evaluate it, because at least for
      --  some cases of attributes we need the identify (e.g. Access, Size).

      elsif Nkind (Parent (N)) = N_Attribute_Reference then
         return;
      end if;

      --  Note: there are other cases, such as the left side of an assignment,
      --  or an OUT parameter for a call, where the replacement results in the
      --  illegal use of a constant, But these cases are illegal in the first
      --  place, so the replacement, though silly, is harmless.

      --  Now see if this is a constant array reference

      if List_Length (Expressions (N)) = 1
        and then Is_Entity_Name (Prefix (N))
        and then Ekind (Entity (Prefix (N))) = E_Constant
        and then Present (Constant_Value (Entity (Prefix (N))))
      then
         declare
            Loc : constant Source_Ptr := Sloc (N);
            Arr : constant Node_Id    := Constant_Value (Entity (Prefix (N)));
            Sub : constant Node_Id    := First (Expressions (N));

            Atyp : Entity_Id;
            --  Type of array

            Lin : Nat;
            --  Linear one's origin subscript value for array reference

            Lbd : Node_Id;
            --  Lower bound of the first array index

            Elm : Node_Id;
            --  Value from constant array

         begin
            Atyp := Etype (Arr);

            if Is_Access_Type (Atyp) then
               Atyp := Designated_Type (Atyp);
            end if;

            --  If we have an array type (we should have but perhaps there are
            --  error cases where this is not the case), then see if we can do
            --  a constant evaluation of the array reference.

            if Is_Array_Type (Atyp) and then Atyp /= Any_Composite then
               if Ekind (Atyp) = E_String_Literal_Subtype then
                  Lbd := String_Literal_Low_Bound (Atyp);
               else
                  Lbd := Type_Low_Bound (Etype (First_Index (Atyp)));
               end if;

               if Compile_Time_Known_Value (Sub)
                 and then Nkind (Arr) = N_Aggregate
                 and then Compile_Time_Known_Value (Lbd)
                 and then Is_Discrete_Type (Component_Type (Atyp))
               then
                  Lin := UI_To_Int (Expr_Value (Sub) - Expr_Value (Lbd)) + 1;

                  if List_Length (Expressions (Arr)) >= Lin then
                     Elm := Pick (Expressions (Arr), Lin);

                     --  If the resulting expression is compile-time-known,
                     --  then we can rewrite the indexed component with this
                     --  value, being sure to mark the result as non-static.
                     --  We also reset the Sloc, in case this generates an
                     --  error later on (e.g. 136'Access).

                     if Compile_Time_Known_Value (Elm) then
                        Rewrite (N, Duplicate_Subexpr_No_Checks (Elm));
                        Set_Is_Static_Expression (N, False);
                        Set_Sloc (N, Loc);
                     end if;
                  end if;

               --  We can also constant-fold if the prefix is a string literal.
               --  This will be useful in an instantiation or an inlining.

               elsif Compile_Time_Known_Value (Sub)
                 and then Nkind (Arr) = N_String_Literal
                 and then Compile_Time_Known_Value (Lbd)
                 and then Expr_Value (Lbd) = 1
                 and then Expr_Value (Sub) <=
                   String_Literal_Length (Etype (Arr))
               then
                  declare
                     C : constant Char_Code :=
                           Get_String_Char (Strval (Arr),
                             UI_To_Int (Expr_Value (Sub)));
                  begin
                     Set_Character_Literal_Name (C);

                     Elm :=
                       Make_Character_Literal (Loc,
                         Chars              => Name_Find,
                         Char_Literal_Value => UI_From_CC (C));
                     Set_Etype (Elm, Component_Type (Atyp));
                     Rewrite (N, Duplicate_Subexpr_No_Checks (Elm));
                     Set_Is_Static_Expression (N, False);
                  end;
               end if;
            end if;
         end;
      end if;
   end Eval_Indexed_Component;

   --------------------------
   -- Eval_Integer_Literal --
   --------------------------

   --  Numeric literals are static (RM 4.9(1)), and have already been marked
   --  as static by the analyzer. The reason we did it that early is to allow
   --  the possibility of turning off the Is_Static_Expression flag after
   --  analysis, but before resolution, when integer literals are generated in
   --  the expander that do not correspond to static expressions.

   procedure Eval_Integer_Literal (N : Node_Id) is
      function In_Any_Integer_Context (K : Node_Kind) return Boolean;
      --  If the literal is resolved with a specific type in a context where
      --  the expected type is Any_Integer, there are no range checks on the
      --  literal. By the time the literal is evaluated, it carries the type
      --  imposed by the enclosing expression, and we must recover the context
      --  to determine that Any_Integer is meant.

      ----------------------------
      -- In_Any_Integer_Context --
      ----------------------------

      function In_Any_Integer_Context (K : Node_Kind) return Boolean is
      begin
         --  Any_Integer also appears in digits specifications for real types,
         --  but those have bounds smaller that those of any integer base type,
         --  so we can safely ignore these cases.

         return K in N_Attribute_Definition_Clause
                   | N_Modular_Type_Definition
                   | N_Number_Declaration
                   | N_Signed_Integer_Type_Definition;
      end In_Any_Integer_Context;

      --  Local variables

      PK  : constant Node_Kind := Nkind (Parent (N));
      Typ : constant Entity_Id := Etype (N);

   --  Start of processing for Eval_Integer_Literal

   begin
      --  If the literal appears in a non-expression context, then it is
      --  certainly appearing in a non-static context, so check it. This is
      --  actually a redundant check, since Check_Non_Static_Context would
      --  check it, but it seems worthwhile to optimize out the call.

      --  Additionally, when the literal appears within an if or case
      --  expression it must be checked as well. However, due to the literal
      --  appearing within a conditional statement, expansion greatly changes
      --  the nature of its context and performing some of the checks within
      --  Check_Non_Static_Context on an expanded literal may lead to spurious
      --  and misleading warnings.

      if (PK not in N_Case_Expression_Alternative | N_Subexpr
           or else (PK in N_Case_Expression_Alternative | N_If_Expression
                     and then
                    Comes_From_Source (N)))
        and then not In_Any_Integer_Context (PK)
      then
         Check_Non_Static_Context (N);
      end if;

      --  Modular integer literals must be in their base range

      if Is_Modular_Integer_Type (Typ)
        and then Is_Out_Of_Range (N, Base_Type (Typ), Assume_Valid => True)
      then
         Out_Of_Range (N);
      end if;
   end Eval_Integer_Literal;

   -------------------------
   -- Eval_Intrinsic_Call --
   -------------------------

   procedure Eval_Intrinsic_Call (N : Node_Id; E : Entity_Id) is

      procedure Eval_Shift (N : Node_Id; E : Entity_Id; Op : Node_Kind);
      --  Evaluate an intrinsic shift call N on the given subprogram E.
      --  Op is the kind for the shift node.

      ----------------
      -- Eval_Shift --
      ----------------

      procedure Eval_Shift (N : Node_Id; E : Entity_Id; Op : Node_Kind) is
         Left   : constant Node_Id := First_Actual (N);
         Right  : constant Node_Id := Next_Actual (Left);
         Static : constant Boolean := Is_Static_Function (E);

      begin
         if Static then
            if Checking_Potentially_Static_Expression then
               Fold_Dummy (N, Etype (N));
               return;
            end if;
         end if;

         Fold_Shift
           (N, Left, Right, Op, Static => Static, Check_Elab => not Static);
      end Eval_Shift;

      Nam : Name_Id;

   begin
      --  Nothing to do if the intrinsic is handled by the back end.

      if Present (Interface_Name (E)) then
         return;
      end if;

      --  Intrinsic calls as part of a static function is a (core)
      --  language extension.

      if Checking_Potentially_Static_Expression
        and then not Core_Extensions_Allowed
      then
         return;
      end if;

      --  If we have a renaming, expand the call to the original operation,
      --  which must itself be intrinsic, since renaming requires matching
      --  conventions and this has already been checked.

      if Present (Alias (E)) then
         Eval_Intrinsic_Call (N, Alias (E));
         return;
      end if;

      --  If the intrinsic subprogram is generic, gets its original name

      if Present (Parent (E))
        and then Present (Generic_Parent (Parent (E)))
      then
         Nam := Chars (Generic_Parent (Parent (E)));
      else
         Nam := Chars (E);
      end if;

      case Nam is
         when Name_Shift_Left  =>
            Eval_Shift (N, E, N_Op_Shift_Left);
         when Name_Shift_Right =>
            Eval_Shift (N, E, N_Op_Shift_Right);
         when Name_Shift_Right_Arithmetic =>
            Eval_Shift (N, E, N_Op_Shift_Right_Arithmetic);
         when others           =>
            null;
      end case;
   end Eval_Intrinsic_Call;

   ---------------------
   -- Eval_Logical_Op --
   ---------------------

   --  Logical operations are static functions, so the result is potentially
   --  static if both operands are potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Logical_Op (N : Node_Id) is
      Left      : constant Node_Id := Left_Opnd (N);
      Right     : constant Node_Id := Right_Opnd (N);
      Left_Int  : Uint := No_Uint;
      Right_Int : Uint := No_Uint;
      Stat      : Boolean;
      Fold      : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Compile time evaluation of logical operation

      if Is_Modular_Integer_Type (Etype (N)) then
         Left_Int  := Expr_Value (Left);
         Right_Int := Expr_Value (Right);

         declare
            Left_Bits  : Bits (0 .. UI_To_Int (Esize (Etype (N))) - 1);
            Right_Bits : Bits (0 .. UI_To_Int (Esize (Etype (N))) - 1);

         begin
            To_Bits (Left_Int, Left_Bits);
            To_Bits (Right_Int, Right_Bits);

            --  Note: should really be able to use array ops instead of
            --  these loops, but they break the build with a cryptic error
            --  during the bind of gnat1 likely due to a wrong computation
            --  of a date or checksum.

            if Nkind (N) = N_Op_And then
               for J in Left_Bits'Range loop
                  Left_Bits (J) := Left_Bits (J) and Right_Bits (J);
               end loop;

            elsif Nkind (N) = N_Op_Or then
               for J in Left_Bits'Range loop
                  Left_Bits (J) := Left_Bits (J) or Right_Bits (J);
               end loop;

            else
               pragma Assert (Nkind (N) = N_Op_Xor);

               for J in Left_Bits'Range loop
                  Left_Bits (J) := Left_Bits (J) xor Right_Bits (J);
               end loop;
            end if;

            Fold_Uint (N, From_Bits (Left_Bits, Etype (N)), Stat);
         end;

      else
         pragma Assert (Is_Boolean_Type (Etype (N)));

         if Compile_Time_Known_Value (Left)
           and then Compile_Time_Known_Value (Right)
         then
            Right_Int := Expr_Value (Right);
            Left_Int  := Expr_Value (Left);
         end if;

         if Nkind (N) = N_Op_And then

            --  If Left or Right are not compile time known values it means
            --  that the result is always False as per
            --  Test_Expression_Is_Foldable.
            --  Note that in this case, both Right_Int and Left_Int are set
            --  to No_Uint, so need to test for both.

            if No (Right_Int) then
               Fold_Uint (N, Uint_0, Stat);
            else
               Fold_Uint (N,
                 Test (Is_True (Left_Int) and then Is_True (Right_Int)), Stat);
            end if;
         elsif Nkind (N) = N_Op_Or then

            --  If Left or Right are not compile time known values it means
            --  that the result is always True. as per
            --  Test_Expression_Is_Foldable.
            --  Note that in this case, both Right_Int and Left_Int are set
            --  to No_Uint, so need to test for both.

            if No (Right_Int) then
               Fold_Uint (N, Uint_1, Stat);
            else
               Fold_Uint (N,
                 Test (Is_True (Left_Int) or else Is_True (Right_Int)), Stat);
            end if;
         else
            pragma Assert (Nkind (N) = N_Op_Xor);
            Fold_Uint (N,
              Test (Is_True (Left_Int) xor Is_True (Right_Int)), Stat);
         end if;
      end if;
   end Eval_Logical_Op;

   ------------------------
   -- Eval_Membership_Op --
   ------------------------

   --  A membership test is potentially static if the expression is static, and
   --  the range is a potentially static range, or is a subtype mark denoting a
   --  static subtype (RM 4.9(12)).

   procedure Eval_Membership_Op (N : Node_Id) is
      Alts   : constant List_Id := Alternatives (N);
      Choice : constant Node_Id := Right_Opnd (N);
      Expr   : constant Node_Id := Left_Opnd (N);
      Result : Match_Result;

   begin
      --  Ignore if error in either operand, except to make sure that Any_Type
      --  is properly propagated to avoid junk cascaded errors.

      if Etype (Expr) = Any_Type
        or else (Present (Choice) and then Etype (Choice) = Any_Type)
      then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  If left operand non-static, then nothing to do

      if not Is_Static_Expression (Expr) then
         return;
      end if;

      --  If choice is non-static, left operand is in non-static context

      if (Present (Choice) and then not Is_Static_Choice (Choice))
        or else (Present (Alts) and then not Is_Static_Choice_List (Alts))
      then
         Check_Non_Static_Context (Expr);
         return;
      end if;

      --  Otherwise we definitely have a static expression

      Set_Is_Static_Expression (N);

      --  If left operand raises Constraint_Error, propagate and we are done

      if Raises_Constraint_Error (Expr) then
         Set_Raises_Constraint_Error (N, True);

      --  See if we match

      else
         if Present (Choice) then
            Result := Choice_Matches (Expr, Choice);
         else
            Result := Choices_Match (Expr, Alts);
         end if;

         --  If result is Non_Static, it means that we raise Constraint_Error,
         --  since we already tested that the operands were themselves static.

         if Result = Non_Static then
            Set_Raises_Constraint_Error (N);

         --  Otherwise we have our result (flipped if NOT IN case)

         else
            Fold_Uint
              (N, Test (Result = Match xor Nkind (N) = N_Not_In), True);
            Warn_On_Known_Condition (N);
         end if;
      end if;
   end Eval_Membership_Op;

   ------------------------
   -- Eval_Named_Integer --
   ------------------------

   procedure Eval_Named_Integer (N : Node_Id) is
   begin
      Fold_Uint (N,
        Expr_Value (Expression (Declaration_Node (Entity (N)))), True);
   end Eval_Named_Integer;

   ---------------------
   -- Eval_Named_Real --
   ---------------------

   procedure Eval_Named_Real (N : Node_Id) is
   begin
      Fold_Ureal (N,
        Expr_Value_R (Expression (Declaration_Node (Entity (N)))), True);
   end Eval_Named_Real;

   -------------------
   -- Eval_Op_Expon --
   -------------------

   --  Exponentiation is a static functions, so the result is potentially
   --  static if both operands are potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Op_Expon (N : Node_Id) is
      Left  : constant Node_Id := Left_Opnd (N);
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable
        (N, Left, Right, Stat, Fold, CRT_Safe => True);

      --  Return if not foldable

      if not Fold then
         return;
      end if;

      if Configurable_Run_Time_Mode and not Stat then
         return;
      end if;

      --  Fold exponentiation operation

      declare
         Right_Int : constant Uint := Expr_Value (Right);

      begin
         --  Integer case

         if Is_Integer_Type (Etype (Left)) then
            declare
               Left_Int : constant Uint := Expr_Value (Left);
               Result   : Uint;

            begin
               --  Exponentiation of an integer raises Constraint_Error for a
               --  negative exponent (RM 4.5.6).

               if Right_Int < 0 then
                  Apply_Compile_Time_Constraint_Error
                    (N, "integer exponent negative", CE_Range_Check_Failed,
                     Warn => not Stat);
                  return;

               else
                  if OK_Bits (N, Num_Bits (Left_Int) * Right_Int) then
                     Result := Left_Int ** Right_Int;
                  else
                     Result := Left_Int;
                  end if;

                  if Is_Modular_Integer_Type (Etype (N)) then
                     Result := Result mod Modulus (Etype (N));
                  end if;

                  Check_Non_Static_Context_For_Overflow (N, Stat, Result);

                  Fold_Uint (N, Result, Stat);
               end if;
            end;

         --  Real case

         else
            declare
               Left_Real : constant Ureal := Expr_Value_R (Left);

            begin
               --  Cannot have a zero base with a negative exponent

               if UR_Is_Zero (Left_Real) then

                  if Right_Int < 0 then
                     Apply_Compile_Time_Constraint_Error
                       (N, "zero ** negative integer", CE_Range_Check_Failed,
                        Warn => not Stat);
                     return;
                  else
                     Fold_Ureal (N, Ureal_0, Stat);
                  end if;

               else
                  Fold_Ureal (N, Left_Real ** Right_Int, Stat);
               end if;
            end;
         end if;
      end;
   end Eval_Op_Expon;

   -----------------
   -- Eval_Op_Not --
   -----------------

   --  The not operation is a static function, so the result is potentially
   --  static if the operand is potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Op_Not (N : Node_Id) is
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Fold not operation

      declare
         Rint : constant Uint      := Expr_Value (Right);
         Typ  : constant Entity_Id := Etype (N);

      begin
         --  Negation is equivalent to subtracting from the modulus minus one.
         --  For a binary modulus this is equivalent to the ones-complement of
         --  the original value. For a nonbinary modulus this is an arbitrary
         --  but consistent definition.

         if Is_Modular_Integer_Type (Typ) then
            Fold_Uint (N, Modulus (Typ) - 1 - Rint, Stat);
         else pragma Assert (Is_Boolean_Type (Typ));
            Fold_Uint (N, Test (not Is_True (Rint)), Stat);
         end if;

         Set_Is_Static_Expression (N, Stat);
      end;
   end Eval_Op_Not;

   -------------------------------
   -- Eval_Qualified_Expression --
   -------------------------------

   --  A qualified expression is potentially static if its subtype mark denotes
   --  a static subtype and its expression is potentially static (RM 4.9 (10)).

   procedure Eval_Qualified_Expression (N : Node_Id) is
      Operand     : constant Node_Id   := Expression (N);
      Target_Type : constant Entity_Id := Entity (Subtype_Mark (N));

      Stat : Boolean;
      Fold : Boolean;
      Hex  : Boolean;

   begin
      --  Can only fold if target is string or scalar and subtype is static.
      --  Also, do not fold if our parent is an allocator (this is because the
      --  qualified expression is really part of the syntactic structure of an
      --  allocator, and we do not want to end up with something that
      --  corresponds to "new 1" where the 1 is the result of folding a
      --  qualified expression).

      if not Is_Static_Subtype (Target_Type)
        or else Nkind (Parent (N)) = N_Allocator
      then
         Check_Non_Static_Context (Operand);

         --  If operand is known to raise Constraint_Error, set the flag on the
         --  expression so it does not get optimized away.

         if Nkind (Operand) = N_Raise_Constraint_Error then
            Set_Raises_Constraint_Error (N);
         end if;

         return;

      --  Also return if a semantic error has been posted on the node, as we
      --  don't want to fold in that case (for GNATprove, the node might lead
      --  to Constraint_Error but won't have been replaced with a raise node
      --  or marked as raising CE).

      elsif Error_Posted (N) then
         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Operand, Stat, Fold);

      if not Fold then
         return;

      --  Don't try fold if target type has Constraint_Error bounds

      elsif not Is_OK_Static_Subtype (Target_Type) then
         Set_Raises_Constraint_Error (N);
         return;
      end if;

      --  Fold the result of qualification

      if Is_Discrete_Type (Target_Type) then

         --  Save Print_In_Hex indication

         Hex := Nkind (Operand) = N_Integer_Literal
                  and then Print_In_Hex (Operand);

         Fold_Uint (N, Expr_Value (Operand), Stat);

         --  Preserve Print_In_Hex indication

         if Hex and then Nkind (N) = N_Integer_Literal then
            Set_Print_In_Hex (N);
         end if;

      elsif Is_Real_Type (Target_Type) then
         Fold_Ureal (N, Expr_Value_R (Operand), Stat);

      else
         Fold_Str (N, Strval (Get_String_Val (Operand)), Stat);

         if not Stat then
            Set_Is_Static_Expression (N, False);
         else
            Check_String_Literal_Length (N, Target_Type);
         end if;

         return;
      end if;

      --  The expression may be foldable but not static

      Set_Is_Static_Expression (N, Stat);

      if Is_Out_Of_Range (N, Etype (N), Assume_Valid => True) then
         Out_Of_Range (N);
      end if;
   end Eval_Qualified_Expression;

   -----------------------
   -- Eval_Real_Literal --
   -----------------------

   --  Numeric literals are static (RM 4.9(1)), and have already been marked
   --  as static by the analyzer. The reason we did it that early is to allow
   --  the possibility of turning off the Is_Static_Expression flag after
   --  analysis, but before resolution, when integer literals are generated
   --  in the expander that do not correspond to static expressions.

   procedure Eval_Real_Literal (N : Node_Id) is
      PK : constant Node_Kind := Nkind (Parent (N));

   begin
      --  If the literal appears in a non-expression context and not as part of
      --  a number declaration, then it is appearing in a non-static context,
      --  so check it.

      if PK not in N_Subexpr and then PK /= N_Number_Declaration then
         Check_Non_Static_Context (N);
      end if;
   end Eval_Real_Literal;

   ------------------------
   -- Eval_Relational_Op --
   ------------------------

   --  Relational operations are static functions, so the result is static if
   --  both operands are static (RM 4.9(7), 4.9(20)), except that up to Ada
   --  2012, for strings the result is never static, even if the operands are.
   --  The string case was relaxed in Ada 2022, see AI12-0201.

   --  However, for internally generated nodes, we allow string equality and
   --  inequality to be static. This is because we rewrite A in "ABC" as an
   --  equality test A = "ABC", and the former is definitely static.

   procedure Eval_Relational_Op (N : Node_Id) is
      Left  : constant Node_Id := Left_Opnd  (N);
      Right : constant Node_Id := Right_Opnd (N);

      procedure Decompose_Expr
        (Expr : Node_Id;
         Ent  : out Entity_Id;
         Kind : out Character;
         Cons : out Uint;
         Orig : Boolean := True);
      --  Given expression Expr, see if it is of the form X [+/- K]. If so, Ent
      --  is set to the entity in X, Kind is 'F','L','E' for 'First or 'Last or
      --  simple entity, and Cons is the value of K. If the expression is not
      --  of the required form, Ent is set to Empty.
      --
      --  Orig indicates whether Expr is the original expression to consider,
      --  or if we are handling a subexpression (e.g. recursive call to
      --  Decompose_Expr).

      procedure Fold_General_Op (Is_Static : Boolean);
      --  Attempt to fold arbitrary relational operator N. Flag Is_Static must
      --  be set when the operator denotes a static expression.

      procedure Fold_Static_Real_Op;
      --  Attempt to fold static real type relational operator N

      function Static_Length (Expr : Node_Id) return Uint;
      --  If Expr is an expression for a constrained array whose length is
      --  known at compile time, return the non-negative length, otherwise
      --  return -1.

      --------------------
      -- Decompose_Expr --
      --------------------

      procedure Decompose_Expr
        (Expr : Node_Id;
         Ent  : out Entity_Id;
         Kind : out Character;
         Cons : out Uint;
         Orig : Boolean := True)
      is
         Exp : Node_Id;

      begin
         --  Assume that the expression does not meet the expected form

         Cons := No_Uint;
         Ent  := Empty;
         Kind := '?';

         if Nkind (Expr) = N_Op_Add
           and then Compile_Time_Known_Value (Right_Opnd (Expr))
         then
            Exp  := Left_Opnd (Expr);
            Cons := Expr_Value (Right_Opnd (Expr));

         elsif Nkind (Expr) = N_Op_Subtract
           and then Compile_Time_Known_Value (Right_Opnd (Expr))
         then
            Exp  := Left_Opnd (Expr);
            Cons := -Expr_Value (Right_Opnd (Expr));

         --  If the bound is a constant created to remove side effects, recover
         --  the original expression to see if it has one of the recognizable
         --  forms.

         elsif Nkind (Expr) = N_Identifier
           and then not Comes_From_Source (Entity (Expr))
           and then Ekind (Entity (Expr)) = E_Constant
           and then Nkind (Parent (Entity (Expr))) = N_Object_Declaration
         then
            Exp := Expression (Parent (Entity (Expr)));
            Decompose_Expr (Exp, Ent, Kind, Cons, Orig => False);

            --  If original expression includes an entity, create a reference
            --  to it for use below.

            if Present (Ent) then
               Exp := New_Occurrence_Of (Ent, Sloc (Ent));
            else
               return;
            end if;

         else
            --  Only consider the case of X + 0 for a full expression, and
            --  not when recursing, otherwise we may end up with evaluating
            --  expressions not known at compile time to 0.

            if Orig then
               Exp  := Expr;
               Cons := Uint_0;
            else
               return;
            end if;
         end if;

         --  At this stage Exp is set to the potential X

         if Nkind (Exp) = N_Attribute_Reference then
            if Attribute_Name (Exp) = Name_First then
               Kind := 'F';
            elsif Attribute_Name (Exp) = Name_Last then
               Kind := 'L';
            else
               return;
            end if;

            Exp := Prefix (Exp);

         else
            Kind := 'E';
         end if;

         if Is_Entity_Name (Exp) and then Present (Entity (Exp)) then
            Ent := Entity (Exp);
         end if;
      end Decompose_Expr;

      ---------------------
      -- Fold_General_Op --
      ---------------------

      procedure Fold_General_Op (Is_Static : Boolean) is
         CR : constant Compare_Result :=
                Compile_Time_Compare (Left, Right, Assume_Valid => False);

         Result : Boolean;

      begin
         if CR = Unknown then
            return;
         end if;

         case Nkind (N) is
            when N_Op_Eq =>
               if CR = EQ then
                  Result := True;
               elsif CR = NE or else CR = GT or else CR = LT then
                  Result := False;
               else
                  return;
               end if;

            when N_Op_Ge =>
               if CR = GT or else CR = EQ or else CR = GE then
                  Result := True;
               elsif CR = LT then
                  Result := False;
               else
                  return;
               end if;

            when N_Op_Gt =>
               if CR = GT then
                  Result := True;
               elsif CR = EQ or else CR = LT or else CR = LE then
                  Result := False;
               else
                  return;
               end if;

            when N_Op_Le =>
               if CR = LT or else CR = EQ or else CR = LE then
                  Result := True;
               elsif CR = GT then
                  Result := False;
               else
                  return;
               end if;

            when N_Op_Lt =>
               if CR = LT then
                  Result := True;
               elsif CR = EQ or else CR = GT or else CR = GE then
                  Result := False;
               else
                  return;
               end if;

            when N_Op_Ne =>
               if CR = NE or else CR = GT or else CR = LT then
                  Result := True;
               elsif CR = EQ then
                  Result := False;
               else
                  return;
               end if;

            when others =>
               raise Program_Error;
         end case;

         --  Determine the potential outcome of the relation assuming the
         --  operands are valid and emit a warning when the relation yields
         --  True or False only in the presence of invalid values.

         Warn_On_Constant_Valid_Condition (N);

         Fold_Uint (N, Test (Result), Is_Static);
      end Fold_General_Op;

      -------------------------
      -- Fold_Static_Real_Op --
      -------------------------

      procedure Fold_Static_Real_Op is
         Left_Real  : constant Ureal := Expr_Value_R (Left);
         Right_Real : constant Ureal := Expr_Value_R (Right);
         Result     : Boolean;

      begin
         case Nkind (N) is
            when N_Op_Eq => Result := (Left_Real =  Right_Real);
            when N_Op_Ge => Result := (Left_Real >= Right_Real);
            when N_Op_Gt => Result := (Left_Real >  Right_Real);
            when N_Op_Le => Result := (Left_Real <= Right_Real);
            when N_Op_Lt => Result := (Left_Real <  Right_Real);
            when N_Op_Ne => Result := (Left_Real /= Right_Real);
            when others  => raise Program_Error;
         end case;

         Fold_Uint (N, Test (Result), True);
      end Fold_Static_Real_Op;

      -------------------
      -- Static_Length --
      -------------------

      function Static_Length (Expr : Node_Id) return Uint is
         Cons1 : Uint;
         Cons2 : Uint;
         Ent1  : Entity_Id;
         Ent2  : Entity_Id;
         Kind1 : Character;
         Kind2 : Character;
         Typ   : Entity_Id;

      begin
         --  First easy case string literal

         if Nkind (Expr) = N_String_Literal then
            return UI_From_Int (String_Length (Strval (Expr)));

         --  With frontend inlining as performed in GNATprove mode, a variable
         --  may be inserted that has a string literal subtype. Deal with this
         --  specially as for the previous case.

         elsif Ekind (Etype (Expr)) = E_String_Literal_Subtype then
            return String_Literal_Length (Etype (Expr));

         --  Second easy case, not constrained subtype, so no length

         elsif not Is_Constrained (Etype (Expr)) then
            return Uint_Minus_1;
         end if;

         --  General case

         Typ := Etype (First_Index (Etype (Expr)));

         --  The simple case, both bounds are known at compile time

         if Is_Discrete_Type (Typ)
           and then Compile_Time_Known_Value (Type_Low_Bound (Typ))
           and then Compile_Time_Known_Value (Type_High_Bound (Typ))
         then
            return
              UI_Max (Uint_0, Expr_Value (Type_High_Bound (Typ)) -
                              Expr_Value (Type_Low_Bound  (Typ)) + 1);
         end if;

         --  A more complex case, where the bounds are of the form X [+/- K1]
         --  .. X [+/- K2]), where X is an expression that is either A'First or
         --  A'Last (with A an entity name), or X is an entity name, and the
         --  two X's are the same and K1 and K2 are known at compile time, in
         --  this case, the length can also be computed at compile time, even
         --  though the bounds are not known. A common case of this is e.g.
         --  (X'First .. X'First+5).

         Decompose_Expr
           (Original_Node (Type_Low_Bound  (Typ)), Ent1, Kind1, Cons1);
         Decompose_Expr
           (Original_Node (Type_High_Bound (Typ)), Ent2, Kind2, Cons2);

         if Present (Ent1) and then Ent1 = Ent2 and then Kind1 = Kind2 then
            return Cons2 - Cons1 + 1;
         else
            return Uint_Minus_1;
         end if;
      end Static_Length;

      --  Local variables

      Left_Typ  : constant Entity_Id := Etype (Left);
      Right_Typ : constant Entity_Id := Etype (Right);
      Fold      : Boolean;
      Left_Len  : Uint;
      Op_Typ    : Entity_Id := Empty;
      Right_Len : Uint;

      Is_Static_Expression : Boolean;

   --  Start of processing for Eval_Relational_Op

   begin
      --  One special case to deal with first. If we can tell that the result
      --  will be false because the lengths of one or more index subtypes are
      --  compile-time known and different, then we can replace the entire
      --  result by False. We only do this for one-dimensional arrays, because
      --  the case of multidimensional arrays is rare and too much trouble. If
      --  one of the operands is an illegal aggregate, its type might still be
      --  an arbitrary composite type, so nothing to do.

      if Is_Array_Type (Left_Typ)
        and then Left_Typ /= Any_Composite
        and then Number_Dimensions (Left_Typ) = 1
        and then Nkind (N) in N_Op_Eq | N_Op_Ne
      then
         if Raises_Constraint_Error (Left)
              or else
            Raises_Constraint_Error (Right)
         then
            return;
         end if;

         --  OK, we have the case where we may be able to do this fold

         Left_Len  := Static_Length (Left);
         Right_Len := Static_Length (Right);

         if Left_Len /= Uint_Minus_1
           and then Right_Len /= Uint_Minus_1
           and then Left_Len /= Right_Len
         then
            --  AI12-0201: comparison of string is static in Ada 2022

            Fold_Uint
              (N,
               Test (Nkind (N) = N_Op_Ne),
               Static => Ada_Version >= Ada_2022
                           and then Is_String_Type (Left_Typ));
            Warn_On_Known_Condition (N);
            return;
         end if;
      end if;

      --  General case

      --  Initialize the value of Is_Static_Expression. The value of Fold
      --  returned by Test_Expression_Is_Foldable is not needed since, even
      --  when some operand is a variable, we can still perform the static
      --  evaluation of the expression in some cases (for example, for a
      --  variable of a subtype of Integer we statically know that any value
      --  stored in such variable is smaller than Integer'Last).

      Test_Expression_Is_Foldable
        (N, Left, Right, Is_Static_Expression, Fold);

      --  Comparisons of scalars can give static results.
      --  In addition starting with Ada 2022 (AI12-0201), comparison of strings
      --  can also give static results, and as noted above, we also allow for
      --  earlier Ada versions internally generated equality and inequality for
      --  strings.
      --  The Comes_From_Source test below isn't correct and will accept
      --  some cases that are illegal in Ada 2012 and before. Now that Ada
      --  2022 has relaxed the rules, this doesn't really matter.

      if Is_String_Type (Left_Typ) then
         if Ada_Version < Ada_2022
           and then (Comes_From_Source (N)
                      or else Nkind (N) not in N_Op_Eq | N_Op_Ne)
         then
            Is_Static_Expression := False;
            Set_Is_Static_Expression (N, False);
         end if;

      elsif not Is_Scalar_Type (Left_Typ) then
         Is_Static_Expression := False;
         Set_Is_Static_Expression (N, False);
      end if;

      --  For operators on universal numeric types called as functions with an
      --  explicit scope, determine appropriate specific numeric type, and
      --  diagnose possible ambiguity.

      if Is_Universal_Numeric_Type (Left_Typ)
           and then
         Is_Universal_Numeric_Type (Right_Typ)
      then
         Op_Typ := Find_Universal_Operator_Type (N);
      end if;

      --  Attempt to fold the relational operator

      if Is_Static_Expression and then Is_Real_Type (Left_Typ) then
         Fold_Static_Real_Op;
      else
         Fold_General_Op (Is_Static_Expression);
      end if;

      --  For the case of a folded relational operator on a specific numeric
      --  type, freeze the operand type now.

      if Present (Op_Typ) then
         Freeze_Before (N, Op_Typ);
      end if;

      Warn_On_Known_Condition (N);
   end Eval_Relational_Op;

   -----------------------------
   -- Eval_Selected_Component --
   -----------------------------

   procedure Eval_Selected_Component (N : Node_Id) is
      Node : Node_Id;
      Comp : Node_Id;
      C    : Node_Id;
      Nam  : Name_Id;

   begin
      --  If an attribute reference or a LHS, nothing to do.
      --  Also do not fold if N is an [in] out subprogram parameter.
      --  Fold will perform the other relevant tests.

      if Nkind (Parent (N)) /= N_Attribute_Reference
        and then not Known_To_Be_Assigned (N)
        and then not Is_Actual_Out_Or_In_Out_Parameter (N)
      then
         --  Simplify a selected_component on an aggregate by extracting
         --  the field directly.

         Node := Unqualify (Prefix (N));

         if Nkind (Node) = N_Aggregate
           and then Compile_Time_Known_Aggregate (Node)
         then
            Comp := First (Component_Associations (Node));
            Nam  := Chars (Selector_Name (N));

            while Present (Comp) loop
               C := First (Choices (Comp));

               while Present (C) loop
                  if Chars (C) = Nam then
                     Rewrite (N, Relocate_Node (Expression (Comp)));
                     return;
                  end if;

                  Next (C);
               end loop;

               Next (Comp);
            end loop;
         else
            Fold (N);
         end if;
      end if;
   end Eval_Selected_Component;

   ----------------
   -- Eval_Shift --
   ----------------

   procedure Eval_Shift (N : Node_Id) is
   begin
      --  This procedure is only called for compiler generated code (e.g.
      --  packed arrays), so there is nothing to do except attempting to fold
      --  the expression.

      Fold_Shift (N, Left_Opnd (N), Right_Opnd (N), Nkind (N));
   end Eval_Shift;

   ------------------------
   -- Eval_Short_Circuit --
   ------------------------

   --  A short circuit operation is potentially static if both operands are
   --  potentially static (RM 4.9 (13)).

   procedure Eval_Short_Circuit (N : Node_Id) is
      Kind     : constant Node_Kind := Nkind (N);
      Left     : constant Node_Id   := Left_Opnd (N);
      Right    : constant Node_Id   := Right_Opnd (N);
      Left_Int : Uint;

      Rstat : constant Boolean :=
                Is_Static_Expression (Left)
                  and then
                Is_Static_Expression (Right);

   begin
      --  Short circuit operations are never static in Ada 83

      if Ada_Version = Ada_83 and then Comes_From_Source (N) then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  Now look at the operands, we can't quite use the normal call to
      --  Test_Expression_Is_Foldable here because short circuit operations
      --  are a special case, they can still be foldable, even if the right
      --  operand raises Constraint_Error.

      --  If either operand is Any_Type, just propagate to result and do not
      --  try to fold, this prevents cascaded errors.

      if Etype (Left) = Any_Type or else Etype (Right) = Any_Type then
         Set_Etype (N, Any_Type);
         return;

      --  If left operand raises Constraint_Error, then replace node N with
      --  the raise Constraint_Error node, and we are obviously not foldable.
      --  Is_Static_Expression is set from the two operands in the normal way,
      --  and we check the right operand if it is in a non-static context.

      elsif Raises_Constraint_Error (Left) then
         if not Rstat then
            Check_Non_Static_Context (Right);
         end if;

         Rewrite_In_Raise_CE (N, Left);
         Set_Is_Static_Expression (N, Rstat);
         return;

      --  If the result is not static, then we won't in any case fold

      elsif not Rstat then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  Here the result is static, note that, unlike the normal processing
      --  in Test_Expression_Is_Foldable, we did *not* check above to see if
      --  the right operand raises Constraint_Error, that's because it is not
      --  significant if the left operand is decisive.

      Set_Is_Static_Expression (N);

      --  It does not matter if the right operand raises Constraint_Error if
      --  it will not be evaluated. So deal specially with the cases where
      --  the right operand is not evaluated. Note that we will fold these
      --  cases even if the right operand is non-static, which is fine, but
      --  of course in these cases the result is not potentially static.

      Left_Int := Expr_Value (Left);

      if (Kind = N_And_Then and then Is_False (Left_Int))
           or else
         (Kind = N_Or_Else  and then Is_True  (Left_Int))
      then
         Fold_Uint (N, Left_Int, Rstat);
         return;
      end if;

      --  If first operand not decisive, then it does matter if the right
      --  operand raises Constraint_Error, since it will be evaluated, so
      --  we simply replace the node with the right operand. Note that this
      --  properly propagates Is_Static_Expression and Raises_Constraint_Error
      --  (both are set to True in Right).

      if Raises_Constraint_Error (Right) then
         Rewrite_In_Raise_CE (N, Right);
         Check_Non_Static_Context (Left);
         return;
      end if;

      --  Otherwise the result depends on the right operand

      Fold_Uint (N, Expr_Value (Right), Rstat);
      return;
   end Eval_Short_Circuit;

   ----------------
   -- Eval_Slice --
   ----------------

   --  Slices can never be static, so the only processing required is to check
   --  for non-static context if an explicit range is given.

   procedure Eval_Slice (N : Node_Id) is
      Drange : constant Node_Id := Discrete_Range (N);
      Name   : constant Node_Id := Prefix (N);

   begin
      if Nkind (Drange) = N_Range then
         Check_Non_Static_Context (Low_Bound (Drange));
         Check_Non_Static_Context (High_Bound (Drange));
      end if;

      --  A slice of the form A (subtype), when the subtype is the index of
      --  the type of A, is redundant, the slice can be replaced with A, and
      --  this is worth a warning.

      if Is_Entity_Name (Name) then
         declare
            E : constant Entity_Id := Entity (Name);
            T : constant Entity_Id := Etype (E);

         begin
            if Is_Object (E)
              and then Is_Array_Type (T)
              and then Is_Entity_Name (Drange)
            then
               if Is_Entity_Name (Original_Node (First_Index (T)))
                 and then Entity (Original_Node (First_Index (T)))
                    = Entity (Drange)
               then
                  if Warn_On_Redundant_Constructs then
                     Error_Msg_N ("redundant slice denotes whole array?r?", N);
                  end if;

                  --  The following might be a useful optimization???

                  --  Rewrite (N, New_Occurrence_Of (E, Sloc (N)));
               end if;
            end if;
         end;
      end if;
   end Eval_Slice;

   -------------------------
   -- Eval_String_Literal --
   -------------------------

   procedure Eval_String_Literal (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);
      Bas : constant Entity_Id := Base_Type (Typ);
      Xtp : Entity_Id;
      Len : Nat;
      Lo  : Node_Id;

   begin
      --  Nothing to do if error type (handles cases like default expressions
      --  or generics where we have not yet fully resolved the type).

      if Bas = Any_Type or else Bas = Any_String then
         return;
      end if;

      --  String literals are static if the subtype is static (RM 4.9(2)), so
      --  reset the static expression flag (it was set unconditionally in
      --  Analyze_String_Literal) if the subtype is non-static. We tell if
      --  the subtype is static by looking at the lower bound.

      if Ekind (Typ) = E_String_Literal_Subtype then
         if not Is_OK_Static_Expression (String_Literal_Low_Bound (Typ)) then
            Set_Is_Static_Expression (N, False);
            return;
         end if;

      --  Here if Etype of string literal is normal Etype (not yet possible,
      --  but may be possible in future).

      elsif not Is_OK_Static_Expression
                  (Type_Low_Bound (Etype (First_Index (Typ))))
      then
         Set_Is_Static_Expression (N, False);
         return;
      end if;

      --  If original node was a type conversion, then result if non-static
      --  up to Ada 2012. AI12-0201 changes that with Ada 2022.

      if Nkind (Original_Node (N)) = N_Type_Conversion
        and then Ada_Version <= Ada_2012
      then
         Set_Is_Static_Expression (N, False);
         return;
      end if;

      --  Test for illegal Ada 95 cases. A string literal is illegal in Ada 95
      --  if its bounds are outside the index base type and this index type is
      --  static. This can happen in only two ways. Either the string literal
      --  is too long, or it is null, and the lower bound is type'First. Either
      --  way it is the upper bound that is out of range of the index type.

      if Ada_Version >= Ada_95 then
         if Is_Standard_String_Type (Bas) then
            Xtp := Standard_Positive;
         else
            Xtp := Etype (First_Index (Bas));
         end if;

         if Ekind (Typ) = E_String_Literal_Subtype then
            Lo := String_Literal_Low_Bound (Typ);
         else
            Lo := Type_Low_Bound (Etype (First_Index (Typ)));
         end if;

         --  Check for string too long

         Len := String_Length (Strval (N));

         if Len > String_Type_Len (Bas) then

            --  Issue message. Note that this message is a warning if the
            --  string literal is not marked as static (happens in some cases
            --  of folding strings known at compile time, but not static).
            --  Furthermore in such cases, we reword the message, since there
            --  is no string literal in the source program.

            if Is_Static_Expression (N) then
               Apply_Compile_Time_Constraint_Error
                 (N, "string literal too long for}", CE_Length_Check_Failed,
                  Ent => Bas,
                  Typ => First_Subtype (Bas));
            else
               Apply_Compile_Time_Constraint_Error
                 (N, "string value too long for}", CE_Length_Check_Failed,
                  Ent  => Bas,
                  Typ  => First_Subtype (Bas),
                  Warn => True);
            end if;

         --  Test for null string not allowed

         elsif Len = 0
           and then not Is_Generic_Type (Xtp)
           and then
             Expr_Value (Lo) = Expr_Value (Type_Low_Bound (Base_Type (Xtp)))
         then
            --  Same specialization of message

            if Is_Static_Expression (N) then
               Apply_Compile_Time_Constraint_Error
                 (N, "null string literal not allowed for}",
                  CE_Length_Check_Failed,
                  Ent => Bas,
                  Typ => First_Subtype (Bas));
            else
               Apply_Compile_Time_Constraint_Error
                 (N, "null string value not allowed for}",
                  CE_Length_Check_Failed,
                  Ent  => Bas,
                  Typ  => First_Subtype (Bas),
                  Warn => True);
            end if;
         end if;
      end if;
   end Eval_String_Literal;

   --------------------------
   -- Eval_Type_Conversion --
   --------------------------

   --  A type conversion is potentially static if its subtype mark is for a
   --  static scalar subtype, and its operand expression is potentially static
   --  (RM 4.9(10)).
   --  Also add support for static string types.

   procedure Eval_Type_Conversion (N : Node_Id) is
      Operand     : constant Node_Id   := Expression (N);
      Source_Type : constant Entity_Id := Etype (Operand);
      Target_Type : constant Entity_Id := Etype (N);

      function To_Be_Treated_As_Integer (T : Entity_Id) return Boolean;
      --  Returns true if type T is an integer type, or if it is a fixed-point
      --  type to be treated as an integer (i.e. the flag Conversion_OK is set
      --  on the conversion node).

      function To_Be_Treated_As_Real (T : Entity_Id) return Boolean;
      --  Returns true if type T is a floating-point type, or if it is a
      --  fixed-point type that is not to be treated as an integer (i.e. the
      --  flag Conversion_OK is not set on the conversion node).

      ------------------------------
      -- To_Be_Treated_As_Integer --
      ------------------------------

      function To_Be_Treated_As_Integer (T : Entity_Id) return Boolean is
      begin
         return
           Is_Integer_Type (T)
             or else (Is_Fixed_Point_Type (T) and then Conversion_OK (N));
      end To_Be_Treated_As_Integer;

      ---------------------------
      -- To_Be_Treated_As_Real --
      ---------------------------

      function To_Be_Treated_As_Real (T : Entity_Id) return Boolean is
      begin
         return
           Is_Floating_Point_Type (T)
             or else (Is_Fixed_Point_Type (T) and then not Conversion_OK (N));
      end To_Be_Treated_As_Real;

      --  Local variables

      Fold : Boolean;
      Stat : Boolean;

   --  Start of processing for Eval_Type_Conversion

   begin
      --  Cannot fold if target type is non-static or if semantic error

      if not Is_Static_Subtype (Target_Type) then
         Check_Non_Static_Context (Operand);
         return;
      elsif Error_Posted (N) then
         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Operand, Stat, Fold);

      if not Fold then
         return;

      --  Don't try fold if target type has Constraint_Error bounds

      elsif not Is_OK_Static_Subtype (Target_Type) then
         Set_Raises_Constraint_Error (N);
         return;
      end if;

      --  Remaining processing depends on operand types. Note that in the
      --  following type test, fixed-point counts as real unless the flag
      --  Conversion_OK is set, in which case it counts as integer.

      --  Fold conversion, case of string type. The result is static starting
      --  with Ada 2022 (AI12-0201).

      if Is_String_Type (Target_Type) then
         Fold_Str
           (N,
            Strval (Get_String_Val (Operand)),
            Static => Ada_Version >= Ada_2022);
         return;

      --  Fold conversion, case of integer target type

      elsif To_Be_Treated_As_Integer (Target_Type) then
         declare
            Result : Uint;

         begin
            --  Integer to integer conversion

            if To_Be_Treated_As_Integer (Source_Type) then
               Result := Expr_Value (Operand);

            --  Real to integer conversion

            elsif To_Be_Treated_As_Real (Source_Type) then
               Result := UR_To_Uint (Expr_Value_R (Operand));

            --  Enumeration to integer conversion, aka 'Enum_Rep

            else
               Result := Expr_Rep_Value (Operand);
            end if;

            --  If fixed-point type (Conversion_OK must be set), then the
            --  result is logically an integer, but we must replace the
            --  conversion with the corresponding real literal, since the
            --  type from a semantic point of view is still fixed-point.

            if Is_Fixed_Point_Type (Target_Type) then
               Fold_Ureal
                 (N, UR_From_Uint (Result) * Small_Value (Target_Type), Stat);

            --  Otherwise result is integer literal

            else
               Fold_Uint (N, Result, Stat);
            end if;
         end;

      --  Fold conversion, case of real target type

      elsif To_Be_Treated_As_Real (Target_Type) then
         declare
            Result : Ureal;

         begin
            if To_Be_Treated_As_Real (Source_Type) then
               Result := Expr_Value_R (Operand);
            else
               Result := UR_From_Uint (Expr_Value (Operand));
            end if;

            Fold_Ureal (N, Result, Stat);
         end;

      --  Enumeration types

      else
         Fold_Uint (N, Expr_Value (Operand), Stat);
      end if;

      --  If the target is a static floating-point subtype, then its bounds
      --  are machine numbers so we must consider the machine-rounded value.

      if Is_Floating_Point_Type (Target_Type)
        and then Nkind (N) = N_Real_Literal
        and then not Is_Machine_Number (N)
      then
         declare
            Lo   : constant Node_Id := Type_Low_Bound (Target_Type);
            Hi   : constant Node_Id := Type_High_Bound (Target_Type);
            Valr : constant Ureal   :=
                     Machine_Number (Target_Type, Expr_Value_R (N), N);
         begin
            if Valr < Expr_Value_R (Lo) or else Valr > Expr_Value_R (Hi) then
               Out_Of_Range (N);
            end if;
         end;

      elsif Is_Out_Of_Range (N, Etype (N), Assume_Valid => True) then
         Out_Of_Range (N);
      end if;
   end Eval_Type_Conversion;

   -------------------
   -- Eval_Unary_Op --
   -------------------

   --  Predefined unary operators are static functions (RM 4.9(20)) and thus
   --  are potentially static if the operand is potentially static (RM 4.9(7)).

   procedure Eval_Unary_Op (N : Node_Id) is
      Right : constant Node_Id := Right_Opnd (N);
      Otype : Entity_Id := Empty;
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      if Is_Universal_Numeric_Type (Etype (Right)) then
         Otype := Find_Universal_Operator_Type (N);
      end if;

      --  Fold for integer case

      if Is_Integer_Type (Etype (N)) then
         declare
            Rint   : constant Uint := Expr_Value (Right);
            Result : Uint;

         begin
            --  In the case of modular unary plus and abs there is no need
            --  to adjust the result of the operation since if the original
            --  operand was in bounds the result will be in the bounds of the
            --  modular type. However, in the case of modular unary minus the
            --  result may go out of the bounds of the modular type and needs
            --  adjustment.

            if Nkind (N) = N_Op_Plus then
               Result := Rint;

            elsif Nkind (N) = N_Op_Minus then
               if Is_Modular_Integer_Type (Etype (N)) then
                  Result := (-Rint) mod Modulus (Etype (N));
               else
                  Result := (-Rint);
               end if;

            else
               pragma Assert (Nkind (N) = N_Op_Abs);
               Result := abs Rint;
            end if;

            Check_Non_Static_Context_For_Overflow (N, Stat, Result);

            Fold_Uint (N, Result, Stat);
         end;

      --  Fold for real case

      elsif Is_Real_Type (Etype (N)) then
         declare
            Rreal  : constant Ureal := Expr_Value_R (Right);
            Result : Ureal;

         begin
            if Nkind (N) = N_Op_Plus then
               Result := Rreal;
            elsif Nkind (N) = N_Op_Minus then
               Result := UR_Negate (Rreal);
            else
               pragma Assert (Nkind (N) = N_Op_Abs);
               Result := abs Rreal;
            end if;

            Fold_Ureal (N, Result, Stat);
         end;
      end if;

      --  If the operator was resolved to a specific type, make sure that type
      --  is frozen even if the expression is folded into a literal (which has
      --  a universal type).

      if Present (Otype) then
         Freeze_Before (N, Otype);
      end if;
   end Eval_Unary_Op;

   -------------------------------
   -- Eval_Unchecked_Conversion --
   -------------------------------

   --  Unchecked conversions can never be static, so the only required
   --  processing is to check for a non-static context for the operand.

   procedure Eval_Unchecked_Conversion (N : Node_Id) is
      Target_Type  : constant Entity_Id := Etype (N);
      Operand      : constant Node_Id   := Expression (N);
      Operand_Type : constant Entity_Id := Etype (Operand);

   begin
      Check_Non_Static_Context (Operand);

      --  If we have a conversion of a compile time known value to a target
      --  type and the value is in range of the target type, then we can simply
      --  replace the construct by an integer literal of the correct type. We
      --  only apply this to discrete types being converted. Possibly it may
      --  apply in other cases, but it is too much trouble to worry about.

      --  Note that we do not do this transformation if the Kill_Range_Check
      --  flag is set, since then the value may be outside the expected range.
      --  This happens in the Normalize_Scalars case.

      --  We also skip this if either the target or operand type is biased
      --  because in this case, the unchecked conversion is supposed to
      --  preserve the bit pattern, not the integer value.

      if Is_Integer_Type (Target_Type)
        and then not Has_Biased_Representation (Target_Type)
        and then Is_Discrete_Type (Operand_Type)
        and then not Has_Biased_Representation (Operand_Type)
        and then Compile_Time_Known_Value (Operand)
        and then not Kill_Range_Check (N)
      then
         declare
            Val : constant Uint := Expr_Rep_Value (Operand);

         begin
            if Compile_Time_Known_Value (Type_Low_Bound (Target_Type))
                 and then
               Compile_Time_Known_Value (Type_High_Bound (Target_Type))
                 and then
               Val >= Expr_Value (Type_Low_Bound (Target_Type))
                 and then
               Val <= Expr_Value (Type_High_Bound (Target_Type))
            then
               Rewrite (N, Make_Integer_Literal (Sloc (N), Val));

               --  If Address is the target type, just set the type to avoid a
               --  spurious type error on the literal when Address is a visible
               --  integer type.

               if Is_Descendant_Of_Address (Target_Type) then
                  Set_Etype (N, Target_Type);
               else
                  Analyze_And_Resolve (N, Target_Type);
               end if;

               return;
            end if;
         end;
      end if;
   end Eval_Unchecked_Conversion;

   --------------------
   -- Expr_Rep_Value --
   --------------------

   function Expr_Rep_Value (N : Node_Id) return Uint is
      Kind : constant Node_Kind := Nkind (N);
      Ent  : Entity_Id;

   begin
      if Is_Entity_Name (N) then
         Ent := Entity (N);

         --  An enumeration literal that was either in the source or created
         --  as a result of static evaluation.

         if Ekind (Ent) = E_Enumeration_Literal then
            return Enumeration_Rep (Ent);

         --  A user defined static constant

         else
            pragma Assert (Ekind (Ent) = E_Constant);
            return Expr_Rep_Value (Constant_Value (Ent));
         end if;

      --  An integer literal that was either in the source or created as a
      --  result of static evaluation.

      elsif Kind = N_Integer_Literal then
         return Intval (N);

      --  A real literal for a fixed-point type. This must be the fixed-point
      --  case, either the literal is of a fixed-point type, or it is a bound
      --  of a fixed-point type, with type universal real. In either case we
      --  obtain the desired value from Corresponding_Integer_Value.

      elsif Kind = N_Real_Literal then
         pragma Assert (Is_Fixed_Point_Type (Underlying_Type (Etype (N))));
         return Corresponding_Integer_Value (N);

      --  The NULL access value

      elsif Kind = N_Null then
         pragma Assert (Is_Access_Type (Underlying_Type (Etype (N)))
           or else Error_Posted (N));
         return Uint_0;

      --  Character literal

      elsif Kind = N_Character_Literal then
         Ent := Entity (N);

         --  Since Character literals of type Standard.Character don't have any
         --  defining character literals built for them, they do not have their
         --  Entity set, so just use their Char code. Otherwise for user-
         --  defined character literals use their Pos value as usual which is
         --  the same as the Rep value.

         if No (Ent) then
            return Char_Literal_Value (N);
         else
            return Enumeration_Rep (Ent);
         end if;

      --  Unchecked conversion, which can come from System'To_Address (X)
      --  where X is a static integer expression. Recursively evaluate X.

      elsif Kind = N_Unchecked_Type_Conversion then
         return Expr_Rep_Value (Expression (N));

      --  Static discriminant value

      elsif Is_Static_Discriminant_Component (N) then
         return Expr_Rep_Value
                  (Get_Discriminant_Value
                     (Entity (Selector_Name (N)),
                      Etype (Prefix (N)),
                      Discriminant_Constraint (Etype (Prefix (N)))));

      else
         raise Program_Error;
      end if;
   end Expr_Rep_Value;

   ----------------
   -- Expr_Value --
   ----------------

   function Expr_Value (N : Node_Id) return Uint is
      Kind   : constant Node_Kind := Nkind (N);
      CV_Ent : CV_Entry renames CV_Cache (Nat (N) mod CV_Cache_Size);
      Ent    : Entity_Id;
      Val    : Uint;

   begin
      --  If already in cache, then we know it's compile-time-known and we can
      --  return the value that was previously stored in the cache since
      --  compile-time-known values cannot change.

      if CV_Ent.N = N then
         return CV_Ent.V;
      end if;

      --  Otherwise proceed to test value

      if Is_Entity_Name (N) then
         Ent := Entity (N);

         --  An enumeration literal that was either in the source or created as
         --  a result of static evaluation.

         if Ekind (Ent) = E_Enumeration_Literal then
            Val := Enumeration_Pos (Ent);

         --  A user defined static constant

         else
            pragma Assert (Ekind (Ent) = E_Constant);
            Val := Expr_Value (Constant_Value (Ent));
         end if;

      --  An integer literal that was either in the source or created as a
      --  result of static evaluation.

      elsif Kind = N_Integer_Literal then
         Val := Intval (N);

      --  A real literal for a fixed-point type. This must be the fixed-point
      --  case, either the literal is of a fixed-point type, or it is a bound
      --  of a fixed-point type, with type universal real. In either case we
      --  obtain the desired value from Corresponding_Integer_Value.

      elsif Kind = N_Real_Literal then
         pragma Assert (Is_Fixed_Point_Type (Underlying_Type (Etype (N))));
         Val := Corresponding_Integer_Value (N);

      --  The NULL access value

      elsif Kind = N_Null then
         pragma Assert (Is_Access_Type (Underlying_Type (Etype (N)))
           or else Error_Posted (N));
         Val := Uint_0;

      --  Character literal

      elsif Kind = N_Character_Literal then
         Ent := Entity (N);

         --  Since Character literals of type Standard.Character don't
         --  have any defining character literals built for them, they
         --  do not have their Entity set, so just use their Char
         --  code. Otherwise for user-defined character literals use
         --  their Pos value as usual.

         if No (Ent) then
            Val := Char_Literal_Value (N);
         else
            Val := Enumeration_Pos (Ent);
         end if;

      --  Unchecked conversion, which can come from System'To_Address (X)
      --  where X is a static integer expression. Recursively evaluate X.

      elsif Kind = N_Unchecked_Type_Conversion then
         Val := Expr_Value (Expression (N));

      --  Static discriminant value

      elsif Is_Static_Discriminant_Component (N) then
         Val := Expr_Value
                  (Get_Discriminant_Value
                     (Entity (Selector_Name (N)),
                      Etype (Prefix (N)),
                      Discriminant_Constraint (Etype (Prefix (N)))));

      else
         raise Program_Error;
      end if;

      --  Come here with Val set to value to be returned, set cache

      CV_Ent.N := N;
      CV_Ent.V := Val;
      return Val;
   end Expr_Value;

   ------------------
   -- Expr_Value_E --
   ------------------

   function Expr_Value_E (N : Node_Id) return Entity_Id is
      Ent : constant Entity_Id := Entity (N);
   begin
      if Ekind (Ent) = E_Enumeration_Literal then
         return Ent;
      else
         pragma Assert (Ekind (Ent) = E_Constant);

         --  We may be dealing with a enumerated character type constant, so
         --  handle that case here.

         if Nkind (Constant_Value (Ent)) = N_Character_Literal then
            return Ent;
         else
            return Expr_Value_E (Constant_Value (Ent));
         end if;
      end if;
   end Expr_Value_E;

   ------------------
   -- Expr_Value_R --
   ------------------

   function Expr_Value_R (N : Node_Id) return Ureal is
      Kind : constant Node_Kind := Nkind (N);
      Ent  : Entity_Id;

   begin
      if Kind = N_Real_Literal then
         return Realval (N);

      elsif Kind = N_Identifier or else Kind = N_Expanded_Name then
         Ent := Entity (N);
         pragma Assert (Ekind (Ent) = E_Constant);
         return Expr_Value_R (Constant_Value (Ent));

      elsif Kind = N_Integer_Literal then
         return UR_From_Uint (Expr_Value (N));

      --  Here, we have a node that cannot be interpreted as a compile time
      --  constant. That is definitely an error.

      else
         raise Program_Error;
      end if;
   end Expr_Value_R;

   ------------------
   -- Expr_Value_S --
   ------------------

   function Expr_Value_S (N : Node_Id) return Node_Id is
   begin
      if Nkind (N) = N_String_Literal then
         return N;
      else
         pragma Assert (Ekind (Entity (N)) = E_Constant);
         return Expr_Value_S (Constant_Value (Entity (N)));
      end if;
   end Expr_Value_S;

   ----------------------------------
   -- Find_Universal_Operator_Type --
   ----------------------------------

   function Find_Universal_Operator_Type (N : Node_Id) return Entity_Id is
      PN     : constant Node_Id := Parent (N);
      Call   : constant Node_Id := Original_Node (N);
      Is_Int : constant Boolean := Is_Integer_Type (Etype (N));

      Is_Fix : constant Boolean :=
                 Nkind (N) in N_Binary_Op
                   and then Nkind (Right_Opnd (N)) /= Nkind (Left_Opnd (N));
      --  A mixed-mode operation in this context indicates the presence of
      --  fixed-point type in the designated package.

      Is_Relational : constant Boolean := Etype (N) = Standard_Boolean;
      --  Case where N is a relational (or membership) operator (else it is an
      --  arithmetic one).

      In_Membership : constant Boolean :=
                        Nkind (PN) in N_Membership_Test
                          and then
                        Nkind (Right_Opnd (PN)) = N_Range
                          and then
                        Is_Universal_Numeric_Type (Etype (Left_Opnd (PN)))
                          and then
                        Is_Universal_Numeric_Type
                          (Etype (Low_Bound (Right_Opnd (PN))))
                          and then
                        Is_Universal_Numeric_Type
                          (Etype (High_Bound (Right_Opnd (PN))));
      --  Case where N is part of a membership test with a universal range

      E      : Entity_Id;
      Pack   : Entity_Id;
      Typ1   : Entity_Id := Empty;
      Priv_E : Entity_Id;

      function Is_Mixed_Mode_Operand (Op : Node_Id) return Boolean;
      --  Check whether one operand is a mixed-mode operation that requires the
      --  presence of a fixed-point type. Given that all operands are universal
      --  and have been constant-folded, retrieve the original function call.

      ---------------------------
      -- Is_Mixed_Mode_Operand --
      ---------------------------

      function Is_Mixed_Mode_Operand (Op : Node_Id) return Boolean is
         Onod : constant Node_Id := Original_Node (Op);
      begin
         return Nkind (Onod) = N_Function_Call
           and then Present (Next_Actual (First_Actual (Onod)))
           and then Etype (First_Actual (Onod)) /=
                    Etype (Next_Actual (First_Actual (Onod)));
      end Is_Mixed_Mode_Operand;

   --  Start of processing for Find_Universal_Operator_Type

   begin
      if Nkind (Call) /= N_Function_Call
        or else Nkind (Name (Call)) /= N_Expanded_Name
      then
         return Empty;

      --  There are several cases where the context does not imply the type of
      --  the operands:
      --     - the universal expression appears in a type conversion;
      --     - the expression is a relational operator applied to universal
      --       operands;
      --     - the expression is a membership test with a universal operand
      --       and a range with universal bounds.

      elsif Nkind (Parent (N)) = N_Type_Conversion
        or else Is_Relational
        or else In_Membership
      then
         Pack := Entity (Prefix (Name (Call)));

         --  If the prefix is a package declared elsewhere, iterate over its
         --  visible entities, otherwise iterate over all declarations in the
         --  designated scope.

         if Ekind (Pack) = E_Package
           and then not In_Open_Scopes (Pack)
         then
            Priv_E := First_Private_Entity (Pack);
         else
            Priv_E := Empty;
         end if;

         Typ1 := Empty;
         E := First_Entity (Pack);
         while Present (E) and then E /= Priv_E loop
            if Is_Numeric_Type (E)
              and then Nkind (Parent (E)) /= N_Subtype_Declaration
              and then Comes_From_Source (E)
              and then Is_Integer_Type (E) = Is_Int
              and then (Nkind (N) in N_Unary_Op
                         or else Is_Relational
                         or else Is_Fixed_Point_Type (E) = Is_Fix)
            then
               if No (Typ1) then
                  Typ1 := E;

               --  Before emitting an error, check for the presence of a
               --  mixed-mode operation that specifies a fixed point type.

               elsif Is_Relational
                 and then
                   (Is_Mixed_Mode_Operand (Left_Opnd (N))
                     or else Is_Mixed_Mode_Operand (Right_Opnd (N)))
                 and then Is_Fixed_Point_Type (E) /= Is_Fixed_Point_Type (Typ1)

               then
                  if Is_Fixed_Point_Type (E) then
                     Typ1 := E;
                  end if;

               else
                  --  More than one type of the proper class declared in P

                  Error_Msg_N ("ambiguous operation", N);
                  Error_Msg_Sloc := Sloc (Typ1);
                  Error_Msg_N ("\possible interpretation (inherited)#", N);
                  Error_Msg_Sloc := Sloc (E);
                  Error_Msg_N ("\possible interpretation (inherited)#", N);
                  return Empty;
               end if;
            end if;

            Next_Entity (E);
         end loop;
      end if;

      return Typ1;
   end Find_Universal_Operator_Type;

   --------------------------
   -- Flag_Non_Static_Expr --
   --------------------------

   procedure Flag_Non_Static_Expr (Msg : String; Expr : Node_Id) is
   begin
      if Error_Posted (Expr) and then not All_Errors_Mode then
         return;
      else
         Error_Msg_F (Msg, Expr);
         Why_Not_Static (Expr);
      end if;
   end Flag_Non_Static_Expr;

   ----------
   -- Fold --
   ----------

   procedure Fold (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);
   begin
      --  If not known at compile time or if already a literal, nothing to do

      if Nkind (N) in N_Numeric_Or_String_Literal
        or else not Compile_Time_Known_Value (N)
      then
         null;

      elsif Is_Discrete_Type (Typ) then
         Fold_Uint (N, Expr_Value (N), Static => Is_Static_Expression (N));

      elsif Is_Real_Type (Typ) then
         Fold_Ureal (N, Expr_Value_R (N), Static => Is_Static_Expression (N));

      elsif Is_String_Type (Typ) then
         Fold_Str
           (N, Strval (Expr_Value_S (N)), Static => Is_Static_Expression (N));
      end if;
   end Fold;

   ----------------
   -- Fold_Dummy --
   ----------------

   procedure Fold_Dummy (N : Node_Id; Typ : Entity_Id) is
   begin
      if Is_Integer_Type (Typ) then
         Fold_Uint (N, Uint_1, Static => True);

      elsif Is_Real_Type (Typ) then
         Fold_Ureal (N, Ureal_1, Static => True);

      elsif Is_Enumeration_Type (Typ) then
         Fold_Uint
           (N,
            Expr_Value (Type_Low_Bound (Base_Type (Typ))),
            Static => True);

      elsif Is_String_Type (Typ) then
         Fold_Str
           (N,
            Strval (Make_String_Literal (Sloc (N), "")),
            Static => True);
      end if;
   end Fold_Dummy;

   ----------------
   -- Fold_Shift --
   ----------------

   procedure Fold_Shift
     (N          : Node_Id;
      Left       : Node_Id;
      Right      : Node_Id;
      Op         : Node_Kind;
      Static     : Boolean := False;
      Check_Elab : Boolean := False)
   is
      Typ : constant Entity_Id := Base_Type (Etype (Left));

      procedure Check_Elab_Call;
      --  Add checks related to calls in elaboration code

      ---------------------
      -- Check_Elab_Call --
      ---------------------

      procedure Check_Elab_Call is
      begin
         if Check_Elab then
            if Legacy_Elaboration_Checks then
               Check_Elab_Call (N);
            end if;

            Build_Call_Marker (N);
         end if;
      end Check_Elab_Call;

      Modulus, Val : Uint;

   begin
      if Compile_Time_Known_Value (Left)
        and then Compile_Time_Known_Value (Right)
      then
         pragma Assert (not Non_Binary_Modulus (Typ));

         if Op = N_Op_Shift_Left then
            Check_Elab_Call;

            if Is_Modular_Integer_Type (Typ) then
               Modulus := Einfo.Entities.Modulus (Typ);
            else
               Modulus := Uint_2 ** RM_Size (Typ);
            end if;

            --  Fold Shift_Left (X, Y) by computing
            --  (X * 2**Y) rem modulus [- Modulus]

            Val := (Expr_Value (Left) * (Uint_2 ** Expr_Value (Right)))
                     rem Modulus;

            if Is_Modular_Integer_Type (Typ)
              or else Val < Modulus / Uint_2
            then
               Fold_Uint (N, Val, Static => Static);
            else
               Fold_Uint (N, Val - Modulus, Static => Static);
            end if;

         elsif Op = N_Op_Shift_Right then
            Check_Elab_Call;

            --  X >> 0 is a no-op

            if Expr_Value (Right) = Uint_0 then
               Fold_Uint (N, Expr_Value (Left), Static => Static);
            else
               if Is_Modular_Integer_Type (Typ) then
                  Modulus := Einfo.Entities.Modulus (Typ);
               else
                  Modulus := Uint_2 ** RM_Size (Typ);
               end if;

               --  Fold X >> Y by computing (X [+ Modulus]) / 2**Y
               --  Note that after a Shift_Right operation (with Y > 0), the
               --  result is always positive, even if the original operand was
               --  negative.

               declare
                  M : Unat;
               begin
                  if Expr_Value (Left) >= Uint_0 then
                     M := Uint_0;
                  else
                     M := Modulus;
                  end if;

                  Fold_Uint
                    (N,
                     (Expr_Value (Left) + M) / (Uint_2 ** Expr_Value (Right)),
                     Static => Static);
               end;
            end if;
         elsif Op = N_Op_Shift_Right_Arithmetic then
            Check_Elab_Call;

            declare
               Two_Y : constant Uint := Uint_2 ** Expr_Value (Right);
            begin
               if Is_Modular_Integer_Type (Typ) then
                  Modulus := Einfo.Entities.Modulus (Typ);
               else
                  Modulus := Uint_2 ** RM_Size (Typ);
               end if;

               --  X / 2**Y if X if positive or a small enough modular integer

               if (Is_Modular_Integer_Type (Typ)
                    and then Expr_Value (Left) < Modulus / Uint_2)
                 or else
                   (not Is_Modular_Integer_Type (Typ)
                     and then Expr_Value (Left) >= 0)
               then
                  Fold_Uint (N, Expr_Value (Left) / Two_Y, Static => Static);

               --  -1 (aka all 1's) if Y is larger than the number of bits
               --  available or if X = -1.

               elsif Two_Y > Modulus
                 or else Expr_Value (Left) = Uint_Minus_1
               then
                  if Is_Modular_Integer_Type (Typ) then
                     Fold_Uint (N, Modulus - Uint_1, Static => Static);
                  else
                     Fold_Uint (N, Uint_Minus_1, Static => Static);
                  end if;

               --  Large modular integer, compute via multiply/divide the
               --  following: X >> Y + (1 << Y - 1) << (RM_Size - Y)

               elsif Is_Modular_Integer_Type (Typ) then
                  Fold_Uint
                    (N,
                     (Expr_Value (Left)) / Two_Y
                        + (Two_Y - Uint_1)
                          * Uint_2 ** (RM_Size (Typ) - Expr_Value (Right)),
                     Static => Static);

               --  Negative signed integer, compute via multiple/divide the
               --  following:
               --  (Modulus + X) >> Y + (1 << Y - 1) << (RM_Size - Y) - Modulus

               else
                  Fold_Uint
                    (N,
                     (Modulus + Expr_Value (Left)) / Two_Y
                        + (Two_Y - Uint_1)
                          * Uint_2 ** (RM_Size (Typ) - Expr_Value (Right))
                        - Modulus,
                     Static => Static);
               end if;
            end;
         end if;
      end if;
   end Fold_Shift;

   --------------
   -- Fold_Str --
   --------------

   procedure Fold_Str (N : Node_Id; Val : String_Id; Static : Boolean) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      if Raises_Constraint_Error (N) then
         Set_Is_Static_Expression (N, Static);
         return;
      end if;

      Rewrite (N, Make_String_Literal (Loc, Strval => Val));

      --  We now have the literal with the right value, both the actual type
      --  and the expected type of this literal are taken from the expression
      --  that was evaluated. So now we do the Analyze and Resolve.

      --  Note that we have to reset Is_Static_Expression both after the
      --  analyze step (because Resolve will evaluate the literal, which
      --  will cause semantic errors if it is marked as static), and after
      --  the Resolve step (since Resolve in some cases resets this flag).

      Analyze (N);
      Set_Is_Static_Expression (N, Static);
      Set_Etype (N, Typ);
      Resolve (N);
      Set_Is_Static_Expression (N, Static);
   end Fold_Str;

   ---------------
   -- Fold_Uint --
   ---------------

   procedure Fold_Uint (N : Node_Id; Val : Uint; Static : Boolean) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : Entity_Id  := Etype (N);
      Ent : Entity_Id;

   begin
      if Raises_Constraint_Error (N) then
         Set_Is_Static_Expression (N, Static);
         return;
      end if;

      --  If we are folding a named number, retain the entity in the literal
      --  in the original tree.

      if Is_Entity_Name (N) and then Ekind (Entity (N)) = E_Named_Integer then
         Ent := Entity (N);
      else
         Ent := Empty;
      end if;

      if Is_Private_Type (Typ) then
         Typ := Full_View (Typ);
      end if;

      --  For a result of type integer, substitute an N_Integer_Literal node
      --  for the result of the compile time evaluation of the expression.
      --  Set a link to the original named number when not in a generic context
      --  for reference in the original tree.

      if Is_Integer_Type (Typ) then
         Rewrite (N, Make_Integer_Literal (Loc, Val));
         Set_Original_Entity (N, Ent);

      --  Otherwise we have an enumeration type, and we substitute either
      --  an N_Identifier or N_Character_Literal to represent the enumeration
      --  literal corresponding to the given value, which must always be in
      --  range, because appropriate tests have already been made for this.

      else pragma Assert (Is_Enumeration_Type (Typ));
         Rewrite (N, Get_Enum_Lit_From_Pos (Etype (N), Val, Loc));
      end if;

      --  We now have the literal with the right value, both the actual type
      --  and the expected type of this literal are taken from the expression
      --  that was evaluated. So now we do the Analyze and Resolve.

      --  Note that we have to reset Is_Static_Expression both after the
      --  analyze step (because Resolve will evaluate the literal, which
      --  will cause semantic errors if it is marked as static), and after
      --  the Resolve step (since Resolve in some cases sets this flag).

      Analyze (N);
      Set_Is_Static_Expression (N, Static);
      Set_Etype (N, Typ);
      Resolve (N);
      Set_Is_Static_Expression (N, Static);
   end Fold_Uint;

   ----------------
   -- Fold_Ureal --
   ----------------

   procedure Fold_Ureal (N : Node_Id; Val : Ureal; Static : Boolean) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Ent : Entity_Id;

   begin
      if Raises_Constraint_Error (N) then
         Set_Is_Static_Expression (N, Static);
         return;
      end if;

      --  If we are folding a named number, retain the entity in the literal
      --  in the original tree.

      if Is_Entity_Name (N) and then Ekind (Entity (N)) = E_Named_Real then
         Ent := Entity (N);
      else
         Ent := Empty;
      end if;

      Rewrite (N, Make_Real_Literal (Loc, Realval => Val));

      --  Set link to original named number

      Set_Original_Entity (N, Ent);

      --  We now have the literal with the right value, both the actual type
      --  and the expected type of this literal are taken from the expression
      --  that was evaluated. So now we do the Analyze and Resolve.

      --  Note that we have to reset Is_Static_Expression both after the
      --  analyze step (because Resolve will evaluate the literal, which
      --  will cause semantic errors if it is marked as static), and after
      --  the Resolve step (since Resolve in some cases sets this flag).

      --  We mark the node as analyzed so that its type is not erased by
      --  calling Analyze_Real_Literal.

      Analyze (N);
      Set_Is_Static_Expression (N, Static);
      Set_Etype (N, Typ);
      Resolve (N);
      Set_Analyzed (N);
      Set_Is_Static_Expression (N, Static);
   end Fold_Ureal;

   ---------------
   -- From_Bits --
   ---------------

   function From_Bits (B : Bits; T : Entity_Id) return Uint is
      V : Uint := Uint_0;

   begin
      for J in 0 .. B'Last loop
         if B (J) then
            V := V + 2 ** J;
         end if;
      end loop;

      if Non_Binary_Modulus (T) then
         V := V mod Modulus (T);
      end if;

      return V;
   end From_Bits;

   --------------------
   -- Get_String_Val --
   --------------------

   function Get_String_Val (N : Node_Id) return Node_Id is
   begin
      if Nkind (N) in N_String_Literal | N_Character_Literal then
         return N;
      else
         pragma Assert (Is_Entity_Name (N));
         return Get_String_Val (Constant_Value (Entity (N)));
      end if;
   end Get_String_Val;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      CV_Cache := (others => (Node_High_Bound, Uint_0));
   end Initialize;

   --------------------
   -- In_Subrange_Of --
   --------------------

   function In_Subrange_Of
     (T1        : Entity_Id;
      T2        : Entity_Id;
      Fixed_Int : Boolean := False) return Boolean
   is
      L1 : Node_Id;
      H1 : Node_Id;

      L2 : Node_Id;
      H2 : Node_Id;

   begin
      if T1 = T2 or else Is_Subtype_Of (T1, T2) then
         return True;

      --  Never in range if both types are not scalar. Don't know if this can
      --  actually happen, but just in case.

      elsif not Is_Scalar_Type (T1) or else not Is_Scalar_Type (T2) then
         return False;

      --  If T1 has infinities but T2 doesn't have infinities, then T1 is
      --  definitely not compatible with T2.

      elsif Is_Floating_Point_Type (T1)
        and then Has_Infinities (T1)
        and then Is_Floating_Point_Type (T2)
        and then not Has_Infinities (T2)
      then
         return False;

      else
         L1 := Type_Low_Bound  (T1);
         H1 := Type_High_Bound (T1);

         L2 := Type_Low_Bound  (T2);
         H2 := Type_High_Bound (T2);

         --  Check bounds to see if comparison possible at compile time

         if Compile_Time_Compare (L1, L2, Assume_Valid => True) in Compare_GE
              and then
            Compile_Time_Compare (H1, H2, Assume_Valid => True) in Compare_LE
         then
            return True;
         end if;

         --  If bounds not comparable at compile time, then the bounds of T2
         --  must be compile-time-known or we cannot answer the query.

         if not Compile_Time_Known_Value (L2)
           or else not Compile_Time_Known_Value (H2)
         then
            return False;
         end if;

         --  If the bounds of T1 are know at compile time then use these
         --  ones, otherwise use the bounds of the base type (which are of
         --  course always static).

         if not Compile_Time_Known_Value (L1) then
            L1 := Type_Low_Bound (Base_Type (T1));
         end if;

         if not Compile_Time_Known_Value (H1) then
            H1 := Type_High_Bound (Base_Type (T1));
         end if;

         --  Fixed point types should be considered as such only if
         --  flag Fixed_Int is set to False.

         if Is_Floating_Point_Type (T1) or else Is_Floating_Point_Type (T2)
           or else (Is_Fixed_Point_Type (T1) and then not Fixed_Int)
           or else (Is_Fixed_Point_Type (T2) and then not Fixed_Int)
         then
            return
              Expr_Value_R (L2) <= Expr_Value_R (L1)
                and then
              Expr_Value_R (H2) >= Expr_Value_R (H1);

         else
            return
              Expr_Value (L2) <= Expr_Value (L1)
                and then
              Expr_Value (H2) >= Expr_Value (H1);

         end if;
      end if;

   --  If any exception occurs, it means that we have some bug in the compiler
   --  possibly triggered by a previous error, or by some unforeseen peculiar
   --  occurrence. However, this is only an optimization attempt, so there is
   --  really no point in crashing the compiler. Instead we just decide, too
   --  bad, we can't figure out the answer in this case after all.

   exception
      when others =>
         --  With debug flag K we will get an exception unless an error has
         --  already occurred (useful for debugging).

         if Debug_Flag_K then
            Check_Error_Detected;
         end if;

         return False;
   end In_Subrange_Of;

   -----------------
   -- Is_In_Range --
   -----------------

   function Is_In_Range
     (N            : Node_Id;
      Typ          : Entity_Id;
      Assume_Valid : Boolean := False;
      Fixed_Int    : Boolean := False;
      Int_Real     : Boolean := False) return Boolean
   is
   begin
      return
        Test_In_Range (N, Typ, Assume_Valid, Fixed_Int, Int_Real) = In_Range;
   end Is_In_Range;

   -------------------
   -- Is_Null_Range --
   -------------------

   function Is_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean is
   begin
      if Compile_Time_Known_Value (Lo)
        and then Compile_Time_Known_Value (Hi)
      then
         declare
            Typ : Entity_Id := Etype (Lo);
         begin
            --  When called from the frontend, as part of the analysis of
            --  potentially static expressions, Typ will be the full view of a
            --  type with all the info needed to answer this query. When called
            --  from the backend, for example to know whether a range of a loop
            --  is null, Typ might be a private type and we need to explicitly
            --  switch to its corresponding full view to access the same info.

            if Is_Incomplete_Or_Private_Type (Typ)
              and then Present (Full_View (Typ))
            then
               Typ := Full_View (Typ);
            end if;

            if Is_Discrete_Type (Typ) then
               return Expr_Value (Lo) > Expr_Value (Hi);
            else pragma Assert (Is_Real_Type (Typ));
               return Expr_Value_R (Lo) > Expr_Value_R (Hi);
            end if;
         end;

      else
         return Compile_Time_Compare (Lo, Hi, Assume_Valid => False) = GT;
      end if;
   end Is_Null_Range;

   -------------------------
   -- Is_OK_Static_Choice --
   -------------------------

   function Is_OK_Static_Choice (Choice : Node_Id) return Boolean is
   begin
      --  Check various possibilities for choice

      --  Note: for membership tests, we test more cases than are possible
      --  (in particular subtype indication), but it doesn't matter because
      --  it just won't occur (we have already done a syntax check).

      if Nkind (Choice) = N_Others_Choice then
         return True;

      elsif Nkind (Choice) = N_Range then
         return Is_OK_Static_Range (Choice);

      elsif Nkind (Choice) = N_Subtype_Indication
        or else (Is_Entity_Name (Choice) and then Is_Type (Entity (Choice)))
      then
         return Is_OK_Static_Subtype (Etype (Choice));

      else
         return Is_OK_Static_Expression (Choice);
      end if;
   end Is_OK_Static_Choice;

   ------------------------------
   -- Is_OK_Static_Choice_List --
   ------------------------------

   function Is_OK_Static_Choice_List (Choices : List_Id) return Boolean is
      Choice : Node_Id;

   begin
      if not Is_Static_Choice_List (Choices) then
         return False;
      end if;

      Choice := First (Choices);
      while Present (Choice) loop
         if not Is_OK_Static_Choice (Choice) then
            Set_Raises_Constraint_Error (Choice);
            return False;
         end if;

         Next (Choice);
      end loop;

      return True;
   end Is_OK_Static_Choice_List;

   -----------------------------
   -- Is_OK_Static_Expression --
   -----------------------------

   function Is_OK_Static_Expression (N : Node_Id) return Boolean is
   begin
      return Is_Static_Expression (N) and then not Raises_Constraint_Error (N);
   end Is_OK_Static_Expression;

   ------------------------
   -- Is_OK_Static_Range --
   ------------------------

   --  A static range is a range whose bounds are static expressions, or a
   --  Range_Attribute_Reference equivalent to such a range (RM 4.9(26)).
   --  We have already converted range attribute references, so we get the
   --  "or" part of this rule without needing a special test.

   function Is_OK_Static_Range (N : Node_Id) return Boolean is
   begin
      return Is_OK_Static_Expression (Low_Bound (N))
        and then Is_OK_Static_Expression (High_Bound (N));
   end Is_OK_Static_Range;

   --------------------------
   -- Is_OK_Static_Subtype --
   --------------------------

   --  Determines if Typ is a static subtype as defined in (RM 4.9(26)) where
   --  neither bound raises Constraint_Error when evaluated.

   function Is_OK_Static_Subtype (Typ : Entity_Id) return Boolean is
      Base_T   : constant Entity_Id := Base_Type (Typ);
      Anc_Subt : Entity_Id;

   begin
      --  First a quick check on the non static subtype flag. As described
      --  in further detail in Einfo, this flag is not decisive in all cases,
      --  but if it is set, then the subtype is definitely non-static.

      if Is_Non_Static_Subtype (Typ) then
         return False;
      end if;

      --  Then, check if the subtype is strictly static. This takes care of
      --  checking for generics and predicates.

      if not Is_Static_Subtype (Typ) then
         return False;
      end if;

      --  String types

      if Is_String_Type (Typ) then
         return
           Ekind (Typ) = E_String_Literal_Subtype
             or else
               (Is_OK_Static_Subtype (Component_Type (Typ))
                 and then Is_OK_Static_Subtype (Etype (First_Index (Typ))));

      --  Scalar types

      elsif Is_Scalar_Type (Typ) then
         if Base_T = Typ then
            return True;

         else
            Anc_Subt := Ancestor_Subtype (Typ);

            if No (Anc_Subt) then
               Anc_Subt := Base_T;
            end if;

            --  Scalar_Range (Typ) might be an N_Subtype_Indication, so use
            --  Get_Type_{Low,High}_Bound.

            return     Is_OK_Static_Subtype (Anc_Subt)
              and then Is_OK_Static_Expression (Type_Low_Bound (Typ))
              and then Is_OK_Static_Expression (Type_High_Bound (Typ));
         end if;

      --  Types other than string and scalar types are never static

      else
         return False;
      end if;
   end Is_OK_Static_Subtype;

   ---------------------
   -- Is_Out_Of_Range --
   ---------------------

   function Is_Out_Of_Range
     (N            : Node_Id;
      Typ          : Entity_Id;
      Assume_Valid : Boolean := False;
      Fixed_Int    : Boolean := False;
      Int_Real     : Boolean := False) return Boolean
   is
   begin
      return Test_In_Range (N, Typ, Assume_Valid, Fixed_Int, Int_Real) =
                                                               Out_Of_Range;
   end Is_Out_Of_Range;

   ----------------------
   -- Is_Static_Choice --
   ----------------------

   function Is_Static_Choice (Choice : Node_Id) return Boolean is
   begin
      --  Check various possibilities for choice

      --  Note: for membership tests, we test more cases than are possible
      --  (in particular subtype indication), but it doesn't matter because
      --  it just won't occur (we have already done a syntax check).

      if Nkind (Choice) = N_Others_Choice then
         return True;

      elsif Nkind (Choice) = N_Range then
         return Is_Static_Range (Choice);

      elsif Nkind (Choice) = N_Subtype_Indication
        or else (Is_Entity_Name (Choice) and then Is_Type (Entity (Choice)))
      then
         return Is_Static_Subtype (Etype (Choice));

      else
         return Is_Static_Expression (Choice);
      end if;
   end Is_Static_Choice;

   ---------------------------
   -- Is_Static_Choice_List --
   ---------------------------

   function Is_Static_Choice_List (Choices : List_Id) return Boolean is
      Choice : Node_Id;

   begin
      Choice := First (Choices);
      while Present (Choice) loop
         if not Is_Static_Choice (Choice) then
            return False;
         end if;

         Next (Choice);
      end loop;

      return True;
   end Is_Static_Choice_List;

   ---------------------
   -- Is_Static_Range --
   ---------------------

   --  A static range is a range whose bounds are static expressions, or a
   --  Range_Attribute_Reference equivalent to such a range (RM 4.9(26)).
   --  We have already converted range attribute references, so we get the
   --  "or" part of this rule without needing a special test.

   function Is_Static_Range (N : Node_Id) return Boolean is
   begin
      return Is_Static_Expression (Low_Bound  (N))
               and then
             Is_Static_Expression (High_Bound (N));
   end Is_Static_Range;

   -----------------------
   -- Is_Static_Subtype --
   -----------------------

   --  Determines if Typ is a static subtype as defined in (RM 4.9(26))

   function Is_Static_Subtype (Typ : Entity_Id) return Boolean is
      Base_T   : constant Entity_Id := Base_Type (Typ);
      Anc_Subt : Entity_Id;

   begin
      --  First a quick check on the non static subtype flag. As described
      --  in further detail in Einfo, this flag is not decisive in all cases,
      --  but if it is set, then the subtype is definitely non-static.

      if Is_Non_Static_Subtype (Typ) then
         return False;
      end if;

      Anc_Subt := Ancestor_Subtype (Typ);

      if Anc_Subt = Empty then
         Anc_Subt := Base_T;
      end if;

      if Is_Generic_Type (Root_Type (Base_T))
        or else Is_Generic_Actual_Type (Base_T)
      then
         return False;

      --  If there is a non-static predicate for the type (declared or
      --  inherited) the expression is not static.

      elsif Has_Dynamic_Predicate_Aspect (Typ)
        or else (Is_Derived_Type (Typ)
                  and then Has_Aspect (Typ, Aspect_Dynamic_Predicate))
        or else Has_Ghost_Predicate_Aspect (Typ)
        or else (Is_Derived_Type (Typ)
                 and then Has_Aspect (Typ, Aspect_Ghost_Predicate))
        or else (Has_Aspect (Typ, Aspect_Predicate)
                  and then not Has_Static_Predicate (Typ))
      then
         return False;

      --  String types

      elsif Is_String_Type (Typ) then
         return
           Ekind (Typ) = E_String_Literal_Subtype
             or else (Is_Static_Subtype (Component_Type (Typ))
                       and then Is_Static_Subtype (Etype (First_Index (Typ))));

      --  Scalar types

      elsif Is_Scalar_Type (Typ) then
         if Base_T = Typ then
            return True;

         else
            return     Is_Static_Subtype (Anc_Subt)
              and then Is_Static_Expression (Type_Low_Bound (Typ))
              and then Is_Static_Expression (Type_High_Bound (Typ));
         end if;

      --  Types other than string and scalar types are never static

      else
         return False;
      end if;
   end Is_Static_Subtype;

   -------------------------------
   -- Is_Statically_Unevaluated --
   -------------------------------

   function Is_Statically_Unevaluated (Expr : Node_Id) return Boolean is
      function Check_Case_Expr_Alternative
        (CEA : Node_Id) return Match_Result;
      --  We have a message emanating from the Expression of a case expression
      --  alternative. We examine this alternative, as follows:
      --
      --  If the selecting expression of the parent case is non-static, or
      --  if any of the discrete choices of the given case alternative are
      --  non-static or raise Constraint_Error, return Non_Static.
      --
      --  Otherwise check if the selecting expression matches any of the given
      --  discrete choices. If so, the alternative is executed and we return
      --  Match, otherwise, the alternative can never be executed, and so we
      --  return No_Match.

      ---------------------------------
      -- Check_Case_Expr_Alternative --
      ---------------------------------

      function Check_Case_Expr_Alternative
        (CEA : Node_Id) return Match_Result
      is
         Case_Exp : constant Node_Id := Parent (CEA);
         Choice   : Node_Id;
         Prev_CEA : Node_Id;

      begin
         pragma Assert (Nkind (Case_Exp) = N_Case_Expression);

         --  Check that selecting expression is static

         if not Is_OK_Static_Expression (Expression (Case_Exp)) then
            return Non_Static;
         end if;

         if not Is_OK_Static_Choice_List (Discrete_Choices (CEA)) then
            return Non_Static;
         end if;

         --  All choices are now known to be static. Now see if alternative
         --  matches one of the choices.

         Choice := First (Discrete_Choices (CEA));
         while Present (Choice) loop

            --  Check various possibilities for choice, returning Match if we
            --  find the selecting value matches any of the choices. Note that
            --  we know we are the last choice, so we don't have to keep going.

            if Nkind (Choice) = N_Others_Choice then

               --  Others choice is a bit annoying, it matches if none of the
               --  previous alternatives matches (note that we know we are the
               --  last alternative in this case, so we can just go backwards
               --  from us to see if any previous one matches).

               Prev_CEA := Prev (CEA);
               while Present (Prev_CEA) loop
                  if Check_Case_Expr_Alternative (Prev_CEA) = Match then
                     return No_Match;
                  end if;

                  Prev (Prev_CEA);
               end loop;

               return Match;

            --  Else we have a normal static choice

            elsif Choice_Matches (Expression (Case_Exp), Choice) = Match then
               return Match;
            end if;

            --  If we fall through, it means that the discrete choice did not
            --  match the selecting expression, so continue.

            Next (Choice);
         end loop;

         --  If we get through that loop then all choices were static, and none
         --  of them matched the selecting expression. So return No_Match.

         return No_Match;
      end Check_Case_Expr_Alternative;

      --  Local variables

      P      : Node_Id;
      OldP   : Node_Id;
      Choice : Node_Id;

   --  Start of processing for Is_Statically_Unevaluated

   begin
      --  The (32.x) references here are from RM section 4.9

      --  (32.1) An expression is statically unevaluated if it is part of ...

      --  This means we have to climb the tree looking for one of the cases

      P := Expr;
      loop
         OldP := P;
         P := Parent (P);

         --  (32.2) The right operand of a static short-circuit control form
         --  whose value is determined by its left operand.

         --  AND THEN with False as left operand

         if Nkind (P) = N_And_Then
           and then Compile_Time_Known_Value (Left_Opnd (P))
           and then Is_False (Expr_Value (Left_Opnd (P)))
         then
            return True;

         --  OR ELSE with True as left operand

         elsif Nkind (P) = N_Or_Else
           and then Compile_Time_Known_Value (Left_Opnd (P))
           and then Is_True (Expr_Value (Left_Opnd (P)))
         then
            return True;

         --  (32.3) A dependent_expression of an if_expression whose associated
         --  condition is static and equals False.

         elsif Nkind (P) = N_If_Expression then
            declare
               Cond : constant Node_Id := First (Expressions (P));
               Texp : constant Node_Id := Next (Cond);
               Fexp : constant Node_Id := Next (Texp);

            begin
               if Compile_Time_Known_Value (Cond) then

                  --  Condition is True and we are in the right operand

                  if Is_True (Expr_Value (Cond)) and then OldP = Fexp then
                     return True;

                  --  Condition is False and we are in the left operand

                  elsif Is_False (Expr_Value (Cond)) and then OldP = Texp then
                     return True;
                  end if;
               end if;
            end;

         --  (32.4) A condition or dependent_expression of an if_expression
         --  where the condition corresponding to at least one preceding
         --  dependent_expression of the if_expression is static and equals
         --  True.

         --  This refers to cases like

         --    (if True then 1 elsif 1/0=2 then 2 else 3)

         --  But we expand elsif's out anyway, so the above looks like:

         --    (if True then 1 else (if 1/0=2 then 2 else 3))

         --  So for us this is caught by the above check for the 32.3 case.

         --  (32.5) A dependent_expression of a case_expression whose
         --  selecting_expression is static and whose value is not covered
         --  by the corresponding discrete_choice_list.

         elsif Nkind (P) = N_Case_Expression_Alternative then

            --  First, we have to be in the expression to suppress messages.
            --  If we are within one of the choices, we want the message.

            if OldP = Expression (P) then

               --  Statically unevaluated if alternative does not match

               if Check_Case_Expr_Alternative (P) = No_Match then
                  return True;
               end if;
            end if;

         --  (32.6) A choice_expression (or a simple_expression of a range
         --  that occurs as a membership_choice of a membership_choice_list)
         --  of a static membership test that is preceded in the enclosing
         --  membership_choice_list by another item whose individual
         --  membership test (see (RM 4.5.2)) statically yields True.

         elsif Nkind (P) in N_Membership_Test then

            --  Only possibly unevaluated if simple expression is static

            if not Is_OK_Static_Expression (Left_Opnd (P)) then
               null;

            --  All members of the choice list must be static

            elsif (Present (Right_Opnd (P))
                    and then not Is_OK_Static_Choice (Right_Opnd (P)))
              or else (Present (Alternatives (P))
                        and then
                          not Is_OK_Static_Choice_List (Alternatives (P)))
            then
               null;

            --  If expression is the one and only alternative, then it is
            --  definitely not statically unevaluated, so we only have to
            --  test the case where there are alternatives present.

            elsif Present (Alternatives (P)) then

               --  Look for previous matching Choice

               Choice := First (Alternatives (P));
               while Present (Choice) loop

                  --  If we reached us and no previous choices matched, this
                  --  is not the case where we are statically unevaluated.

                  exit when OldP = Choice;

                  --  If a previous choice matches, then that is the case where
                  --  we know our choice is statically unevaluated.

                  if Choice_Matches (Left_Opnd (P), Choice) = Match then
                     return True;
                  end if;

                  Next (Choice);
               end loop;

               --  If we fall through the loop, we were not one of the choices,
               --  we must have been the expression, so that is not covered by
               --  this rule, and we keep going.

               null;
            end if;
         end if;

         --  OK, not statically unevaluated at this level, see if we should
         --  keep climbing to look for a higher level reason.

         --  Special case for component association in aggregates, where
         --  we want to keep climbing up to the parent aggregate.

         if Nkind (P) = N_Component_Association
           and then Nkind (Parent (P)) = N_Aggregate
         then
            null;

         --  All done if not still within subexpression

         else
            exit when Nkind (P) not in N_Subexpr;
         end if;
      end loop;

      --  If we fall through the loop, not one of the cases covered!

      return False;
   end Is_Statically_Unevaluated;

   --------------------
   -- Machine_Number --
   --------------------

   --  Historical note: RM 4.9(38) originally specified biased rounding but
   --  this has been modified by AI-268 to prevent confusing differences in
   --  rounding between static and nonstatic expressions. This AI specifies
   --  that the effect of such rounding is implementation-dependent instead,
   --  and in GNAT we round to nearest even to match the run-time behavior.
   --  Note that this applies to floating-point literals, not fixed-point
   --  ones, even though their representation is also a universal real.

   function Machine_Number
     (Typ : Entity_Id;
      Val : Ureal;
      N   : Node_Id) return Ureal
   is
   begin
      return Machine (Typ, Val, Round_Even, N);
   end Machine_Number;

   --------------------
   -- Not_Null_Range --
   --------------------

   function Not_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean is
   begin
      if Compile_Time_Known_Value (Lo)
        and then Compile_Time_Known_Value (Hi)
      then
         declare
            Typ : Entity_Id := Etype (Lo);
         begin
            --  When called from the frontend, as part of the analysis of
            --  potentially static expressions, Typ will be the full view of a
            --  type with all the info needed to answer this query. When called
            --  from the backend, for example to know whether a range of a loop
            --  is null, Typ might be a private type and we need to explicitly
            --  switch to its corresponding full view to access the same info.

            if Is_Incomplete_Or_Private_Type (Typ)
              and then Present (Full_View (Typ))
            then
               Typ := Full_View (Typ);
            end if;

            if Is_Discrete_Type (Typ) then
               return Expr_Value (Lo) <= Expr_Value (Hi);
            else pragma Assert (Is_Real_Type (Typ));
               return Expr_Value_R (Lo) <= Expr_Value_R (Hi);
            end if;
         end;

      else
         return
           Compile_Time_Compare (Lo, Hi, Assume_Valid => False) in Compare_LE;
      end if;
   end Not_Null_Range;

   -------------
   -- OK_Bits --
   -------------

   function OK_Bits (N : Node_Id; Bits : Uint) return Boolean is
   begin
      --  We allow a maximum of 500,000 bits which seems a reasonable limit

      if Bits < 500_000 then
         return True;

      --  Error if this maximum is exceeded

      else
         Error_Msg_N ("static value too large, capacity exceeded", N);
         return False;
      end if;
   end OK_Bits;

   ------------------
   -- Out_Of_Range --
   ------------------

   procedure Out_Of_Range (N : Node_Id) is

      --  If the FE conjures up an expression that would normally be
      --  an illegal static expression (e.g., an integer literal with
      --  a value outside of its base subtype), we don't want to
      --  flag it as illegal; we only want a warning in such cases.

      function Force_Warning return Boolean is
        (if Comes_From_Source (Original_Node (N)) then False
         elsif Nkind (Original_Node (N)) = N_Type_Conversion then True
         else Is_Null_Array_Aggregate_High_Bound (N));
   begin
      --  If we have the static expression case, then this is an illegality
      --  in Ada 95 mode, except that in an instance, we never generate an
      --  error (if the error is legitimate, it was already diagnosed in the
      --  template).

      if Is_Static_Expression (N)
        and then not In_Instance
        and then not In_Inlined_Body
        and then Ada_Version >= Ada_95
      then
         --  No message if we are statically unevaluated

         if Is_Statically_Unevaluated (N) then
            null;

         --  The expression to compute the length of a packed array is attached
         --  to the array type itself, and deserves a separate message.

         elsif Nkind (Parent (N)) = N_Defining_Identifier
           and then Is_Array_Type (Parent (N))
           and then Present (Packed_Array_Impl_Type (Parent (N)))
           and then Present (First_Rep_Item (Parent (N)))
         then
            Error_Msg_N
             ("length of packed array must not exceed Integer''Last",
              First_Rep_Item (Parent (N)));
            Rewrite (N, Make_Integer_Literal (Sloc (N), Uint_1));

         --  All cases except the special array case.
         --  No message if we are dealing with System.Priority values in
         --  CodePeer mode where the target runtime may have more priorities.

         elsif not CodePeer_Mode
           or else not Is_RTE (Etype (N), RE_Priority)
         then
            --  Determine if the out-of-range violation constitutes a warning
            --  or an error based on context, according to RM 4.9 (34/3).

            if Force_Warning then
               Apply_Compile_Time_Constraint_Error
                 (N, "value not in range of}??", CE_Range_Check_Failed);
            else
               Apply_Compile_Time_Constraint_Error
                 (N, "value not in range of}", CE_Range_Check_Failed);
            end if;
         end if;

      --  Here we generate a warning for the Ada 83 case, or when we are in an
      --  instance, or when we have a non-static expression case.

      else
         Apply_Compile_Time_Constraint_Error
           (N, "value not in range of}??", CE_Range_Check_Failed);
      end if;
   end Out_Of_Range;

   ---------------------------
   -- Predicates_Compatible --
   ---------------------------

   function Predicates_Compatible (T1, T2 : Entity_Id) return Boolean is

      function T2_Rep_Item_Applies_To_T1 (Nam : Name_Id) return Boolean;
      --  Return True if the rep item for Nam is either absent on T2 or also
      --  applies to T1.

      -------------------------------
      -- T2_Rep_Item_Applies_To_T1 --
      -------------------------------

      function T2_Rep_Item_Applies_To_T1 (Nam : Name_Id) return Boolean is
         Rep_Item : constant Node_Id := Get_Rep_Item (T2, Nam);

      begin
         return No (Rep_Item) or else Get_Rep_Item (T1, Nam) = Rep_Item;
      end T2_Rep_Item_Applies_To_T1;

   --  Start of processing for Predicates_Compatible

   begin
      if Ada_Version < Ada_2012 then
         return True;

      --  If T2 has no predicates, there is no compatibility issue

      elsif not Has_Predicates (T2) then
         return True;

      --  T2 has predicates, if T1 has none then we defer to the static check

      elsif not Has_Predicates (T1) then
         null;

      --  Both T2 and T1 have predicates, check that all predicates that apply
      --  to T2 apply also to T1 (RM 4.9.1(9/3)).

      elsif T2_Rep_Item_Applies_To_T1 (Name_Static_Predicate)
        and then T2_Rep_Item_Applies_To_T1 (Name_Dynamic_Predicate)
        and then T2_Rep_Item_Applies_To_T1 (Name_Predicate)
      then
         return True;
      end if;

      --  Implement the static check prescribed by RM 4.9.1(10/3)

      if Is_Static_Subtype (T1) and then Is_Static_Subtype (T2) then
         --  We just need to query Interval_Lists for discrete types

         if Is_Discrete_Type (T1) and then Is_Discrete_Type (T2) then
            declare
               Interval_List1 : constant Interval_Lists.Discrete_Interval_List
                 := Interval_Lists.Type_Intervals (T1);
               Interval_List2 : constant Interval_Lists.Discrete_Interval_List
                 := Interval_Lists.Type_Intervals (T2);
            begin
               return Interval_Lists.Is_Subset (Interval_List1, Interval_List2)
                 and then not (Has_Predicates (T1)
                                and then not Predicate_Checks_Suppressed (T2)
                                and then Predicate_Checks_Suppressed (T1));
            end;

         else
            --  ??? Need to implement Interval_Lists for real types

            return False;
         end if;

      --  If either subtype is not static, the predicates are not compatible

      else
         return False;
      end if;
   end Predicates_Compatible;

   ----------------------
   -- Predicates_Match --
   ----------------------

   function Predicates_Match (T1, T2 : Entity_Id) return Boolean is

      function Have_Same_Rep_Item (Nam : Name_Id) return Boolean;
      --  Return True if T1 and T2 have the same rep item for Nam

      ------------------------
      -- Have_Same_Rep_Item --
      ------------------------

      function Have_Same_Rep_Item (Nam : Name_Id) return Boolean is
      begin
         return Get_Rep_Item (T1, Nam) = Get_Rep_Item (T2, Nam);
      end Have_Same_Rep_Item;

   --  Start of processing for Predicates_Match

   begin
      if Ada_Version < Ada_2012 then
         return True;

      --  If T2 has no predicates, match if and only if T1 has none

      elsif not Has_Predicates (T2) then
         return not Has_Predicates (T1);

      --  T2 has predicates, no match if T1 has none

      elsif not Has_Predicates (T1) then
         return False;

      --  Both T2 and T1 have predicates, check that they all come
      --  from the same declarations.

      else
         return Have_Same_Rep_Item (Name_Static_Predicate)
           and then Have_Same_Rep_Item (Name_Dynamic_Predicate)
           and then Have_Same_Rep_Item (Name_Predicate);
      end if;
   end Predicates_Match;

   ---------------------------------------------
   -- Real_Or_String_Static_Predicate_Matches --
   ---------------------------------------------

   function Real_Or_String_Static_Predicate_Matches
     (Val : Node_Id;
      Typ : Entity_Id) return Boolean
   is
      Expr : constant Node_Id := Static_Real_Or_String_Predicate (Typ);
      --  The predicate expression from the type

      Pfun : constant Entity_Id := Predicate_Function (Typ);
      --  The entity for the predicate function

      Ent_Name : constant Name_Id := Chars (First_Formal (Pfun));
      --  The name of the formal of the predicate function. Occurrences of the
      --  type name in Expr have been rewritten as references to this formal,
      --  and it has a unique name, so we can identify references by this name.

      Copy : Node_Id;
      --  Copy of the predicate function tree

      function Process (N : Node_Id) return Traverse_Result;
      --  Function used to process nodes during the traversal in which we will
      --  find occurrences of the entity name, and replace such occurrences
      --  by a real literal with the value to be tested.

      procedure Traverse is new Traverse_Proc (Process);
      --  The actual traversal procedure

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) = N_Identifier and then Chars (N) = Ent_Name then
            declare
               Nod : constant Node_Id := New_Copy (Val);
            begin
               Set_Sloc (Nod, Sloc (N));
               Rewrite (N, Nod);
               return Skip;
            end;

         --  The predicate function may contain string-comparison operations
         --  that have been converted into calls to run-time array-comparison
         --  routines. To evaluate the predicate statically, we recover the
         --  original comparison operation and replace the occurrence of the
         --  formal by the static string value. The actuals of the generated
         --  call are of the form X'Address.

         elsif Nkind (N) in N_Op_Compare
           and then Nkind (Left_Opnd (N)) = N_Function_Call
         then
            declare
               C : constant Node_Id := Left_Opnd (N);
               F : constant Node_Id := First (Parameter_Associations (C));
               L : constant Node_Id := Prefix (F);
               R : constant Node_Id := Prefix (Next (F));

            begin
               --  If an operand is an entity name, it is the formal of the
               --  predicate function, so replace it with the string value.
               --  It may be either operand in the call. The other operand
               --  is a static string from the original predicate.

               if Is_Entity_Name (L) then
                  Rewrite (Left_Opnd (N),  New_Copy (Val));
                  Rewrite (Right_Opnd (N), New_Copy (R));

               else
                  Rewrite (Left_Opnd (N),  New_Copy (L));
                  Rewrite (Right_Opnd (N), New_Copy (Val));
               end if;

               return Skip;
            end;

         else
            return OK;
         end if;
      end Process;

   --  Start of processing for Real_Or_String_Static_Predicate_Matches

   begin
      --  First deal with special case of inherited predicate, where the
      --  predicate expression looks like:

      --     xxPredicate (typ (Ent)) and then Expr

      --  where Expr is the predicate expression for this level, and the
      --  left operand is the call to evaluate the inherited predicate.

      if Nkind (Expr) = N_And_Then
        and then Nkind (Left_Opnd (Expr)) = N_Function_Call
        and then Is_Predicate_Function (Entity (Name (Left_Opnd (Expr))))
      then
         --  OK we have the inherited case, so make a call to evaluate the
         --  inherited predicate. If that fails, so do we!

         if not
           Real_Or_String_Static_Predicate_Matches
             (Val => Val,
              Typ => Etype (First_Formal (Entity (Name (Left_Opnd (Expr))))))
         then
            return False;
         end if;

         --  Use the right operand for the continued processing

         Copy := Copy_Separate_Tree (Right_Opnd (Expr));

      --  Case where call to predicate function appears on its own (this means
      --  that the predicate at this level is just inherited from the parent).

      elsif Nkind (Expr) = N_Function_Call then
         declare
            Typ : constant Entity_Id :=
                    Etype (First_Formal (Entity (Name (Expr))));

         begin
            --  If the inherited predicate is not static, just ignore it. We
            --  can't go trying to evaluate a dynamic predicate as a static
            --  one!

            if Has_Dynamic_Predicate_Aspect (Typ)
              or else Has_Ghost_Predicate_Aspect (Typ)
            then
               return True;

            --  Otherwise inherited predicate is static, check for match

            else
               return Real_Or_String_Static_Predicate_Matches (Val, Typ);
            end if;
         end;

      --  If not just an inherited predicate, copy whole expression

      else
         Copy := Copy_Separate_Tree (Expr);
      end if;

      --  Now we replace occurrences of the entity by the value

      Traverse (Copy);

      --  And analyze the resulting static expression to see if it is True

      Analyze_And_Resolve (Copy, Standard_Boolean);
      return Is_True (Expr_Value (Copy));
   end Real_Or_String_Static_Predicate_Matches;

   -------------------------
   -- Rewrite_In_Raise_CE --
   -------------------------

   procedure Rewrite_In_Raise_CE (N : Node_Id; Exp : Node_Id) is
      Stat : constant Boolean   := Is_Static_Expression (N);
      Typ  : constant Entity_Id := Etype (N);

   begin
      --  If we want to raise CE in the condition of a N_Raise_CE node, we
      --  can just clear the condition if the reason is appropriate. We do
      --  not do this operation if the parent has a reason other than range
      --  check failed, because otherwise we would change the reason.

      if Present (Parent (N))
        and then Nkind (Parent (N)) = N_Raise_Constraint_Error
        and then Reason (Parent (N)) =
                   UI_From_Int (RT_Exception_Code'Pos (CE_Range_Check_Failed))
      then
         Set_Condition (Parent (N), Empty);

      --  Else build an explicit N_Raise_CE

      else
         if Nkind (Exp) = N_Raise_Constraint_Error then
            Rewrite (N,
              Make_Raise_Constraint_Error (Sloc (Exp),
                Reason => Reason (Exp)));
         else
            Rewrite (N,
              Make_Raise_Constraint_Error (Sloc (Exp),
                Reason => CE_Range_Check_Failed));
         end if;

         Set_Raises_Constraint_Error (N);
         Set_Etype (N, Typ);
      end if;

      --  Set proper flags in result

      Set_Raises_Constraint_Error (N, True);
      Set_Is_Static_Expression (N, Stat);
   end Rewrite_In_Raise_CE;

   ------------------------------------------------
   -- Set_Checking_Potentially_Static_Expression --
   ------------------------------------------------

   procedure Set_Checking_Potentially_Static_Expression (Value : Boolean) is
   begin
      --  Verify that we only start/stop checking for a potentially static
      --  expression and do not start or stop it twice in a row.

      pragma Assert (Checking_For_Potentially_Static_Expression /= Value);

      Checking_For_Potentially_Static_Expression := Value;
   end Set_Checking_Potentially_Static_Expression;

   ---------------------
   -- String_Type_Len --
   ---------------------

   function String_Type_Len (Stype : Entity_Id) return Uint is
      NT : constant Entity_Id := Etype (First_Index (Stype));
      T  : Entity_Id;

   begin
      if Is_OK_Static_Subtype (NT) then
         T := NT;
      else
         T := Base_Type (NT);
      end if;

      return Expr_Value (Type_High_Bound (T)) -
             Expr_Value (Type_Low_Bound (T)) + 1;
   end String_Type_Len;

   ------------------------------------
   -- Subtypes_Statically_Compatible --
   ------------------------------------

   function Subtypes_Statically_Compatible
     (T1                      : Entity_Id;
      T2                      : Entity_Id;
      Formal_Derived_Matching : Boolean := False) return Boolean
   is
   begin
      --  A type is always statically compatible with itself

      if T1 = T2 then
         return True;

      --  Not compatible if predicates are not compatible

      elsif not Predicates_Compatible (T1, T2) then
         return False;

      --  Scalar types

      elsif Is_Scalar_Type (T1) then

         --  Definitely compatible if we match

         if Subtypes_Statically_Match (T1, T2) then
            return True;

         --  A scalar subtype S1 is compatible with S2 if their bounds
         --  are static and compatible, even if S1 has dynamic predicates
         --  and is thus non-static. Predicate compatibility has been
         --  checked above.

         elsif not Is_Static_Range (Scalar_Range (T1))
                 or else not Is_Static_Range (Scalar_Range (T2))
         then
            return False;

         --  Base types must match, but we don't check that (should we???) but
         --  we do at least check that both types are real, or both types are
         --  not real.

         elsif Is_Real_Type (T1) /= Is_Real_Type (T2) then
            return False;

         --  Here we check the bounds

         else
            declare
               LB1 : constant Node_Id := Type_Low_Bound  (T1);
               HB1 : constant Node_Id := Type_High_Bound (T1);
               LB2 : constant Node_Id := Type_Low_Bound  (T2);
               HB2 : constant Node_Id := Type_High_Bound (T2);

            begin
               if Is_Real_Type (T1) then
                  return
                    Expr_Value_R (LB1) > Expr_Value_R (HB1)
                      or else
                        (Expr_Value_R (LB2) <= Expr_Value_R (LB1)
                          and then Expr_Value_R (HB1) <= Expr_Value_R (HB2));

               else
                  return
                    Expr_Value (LB1) > Expr_Value (HB1)
                      or else
                        (Expr_Value (LB2) <= Expr_Value (LB1)
                          and then Expr_Value (HB1) <= Expr_Value (HB2));
               end if;
            end;
         end if;

      --  Access types

      elsif Is_Access_Type (T1) then
         return
           (not Is_Constrained (T2)
             or else Subtypes_Statically_Match
                       (Designated_Type (T1), Designated_Type (T2)))
           and then not (Can_Never_Be_Null (T2)
                          and then not Can_Never_Be_Null (T1));

      --  Private types without discriminants can be handled specially.
      --  Predicate matching has been checked above.

      elsif Is_Private_Type (T1)
        and then not Has_Discriminants (T1)
      then
         return not Has_Discriminants (T2);

      --  All other cases

      else
         return
           (Is_Composite_Type (T1) and then not Is_Constrained (T2))
             or else Subtypes_Statically_Match
                       (T1, T2, Formal_Derived_Matching);
      end if;
   end Subtypes_Statically_Compatible;

   -------------------------------
   -- Subtypes_Statically_Match --
   -------------------------------

   --  Subtypes statically match if they have statically matching constraints
   --  (RM 4.9.1(2)). Constraints statically match if there are none, or if
   --  they are the same identical constraint, or if they are static and the
   --  values match (RM 4.9.1(1)).

   --  In addition, in GNAT, the object size (Esize) values of the types must
   --  match if they are set (unless checking an actual for a formal derived
   --  type). The use of 'Object_Size can cause this to be false even if the
   --  types would otherwise match in the Ada 95 RM sense, but this deviation
   --  is adopted by AI12-059 which introduces Object_Size in Ada 2022.

   function Subtypes_Statically_Match
     (T1                      : Entity_Id;
      T2                      : Entity_Id;
      Formal_Derived_Matching : Boolean := False) return Boolean
   is
   begin
      --  A type always statically matches itself

      if T1 = T2 then
         return True;

      --  No match if sizes different (from use of 'Object_Size). This test
      --  is excluded if Formal_Derived_Matching is True, as the base types
      --  can be different in that case and typically have different sizes.

      elsif not Formal_Derived_Matching
        and then Known_Static_Esize (T1)
        and then Known_Static_Esize (T2)
        and then Esize (T1) /= Esize (T2)
      then
         return False;

      --  No match if predicates do not match

      elsif not Predicates_Match (T1, T2) then
         return False;

      --  Scalar types

      elsif Is_Scalar_Type (T1) then

         --  Base types must be the same

         if Base_Type (T1) /= Base_Type (T2) then
            return False;
         end if;

         --  A constrained numeric subtype never matches an unconstrained
         --  subtype, i.e. both types must be constrained or unconstrained.

         --  To understand the requirement for this test, see RM 4.9.1(1).
         --  As is made clear in RM 3.5.4(11), type Integer, for example is
         --  a constrained subtype with constraint bounds matching the bounds
         --  of its corresponding unconstrained base type. In this situation,
         --  Integer and Integer'Base do not statically match, even though
         --  they have the same bounds.

         --  We only apply this test to types in Standard and types that appear
         --  in user programs. That way, we do not have to be too careful about
         --  setting Is_Constrained right for Itypes.

         if Is_Numeric_Type (T1)
           and then Is_Constrained (T1) /= Is_Constrained (T2)
           and then (Scope (T1) = Standard_Standard
                      or else Comes_From_Source (T1))
           and then (Scope (T2) = Standard_Standard
                      or else Comes_From_Source (T2))
         then
            return False;

         --  A generic scalar type does not statically match its base type
         --  (AI-311). In this case we make sure that the formals, which are
         --  first subtypes of their bases, are constrained.

         elsif Is_Generic_Type (T1)
           and then Is_Generic_Type (T2)
           and then Is_Constrained (T1) /= Is_Constrained (T2)
         then
            return False;
         end if;

         --  If there was an error in either range, then just assume the types
         --  statically match to avoid further junk errors.

         if No (Scalar_Range (T1)) or else No (Scalar_Range (T2))
           or else Error_Posted (Scalar_Range (T1))
           or else Error_Posted (Scalar_Range (T2))
         then
            return True;
         end if;

         --  Otherwise both types have bounds that can be compared

         declare
            LB1 : constant Node_Id := Type_Low_Bound  (T1);
            HB1 : constant Node_Id := Type_High_Bound (T1);
            LB2 : constant Node_Id := Type_Low_Bound  (T2);
            HB2 : constant Node_Id := Type_High_Bound (T2);

         begin
            --  If the bounds are the same tree node, then match (common case)

            if LB1 = LB2 and then HB1 = HB2 then
               return True;

            --  Otherwise bounds must be static and identical value

            else
               if not Is_OK_Static_Subtype (T1)
                    or else
                  not Is_OK_Static_Subtype (T2)
               then
                  return False;

               elsif Is_Real_Type (T1) then
                  return
                    Expr_Value_R (LB1) = Expr_Value_R (LB2)
                      and then
                    Expr_Value_R (HB1) = Expr_Value_R (HB2);

               else
                  return
                    Expr_Value (LB1) = Expr_Value (LB2)
                      and then
                    Expr_Value (HB1) = Expr_Value (HB2);
               end if;
            end if;
         end;

      --  Type with discriminants

      elsif Has_Discriminants (T1) or else Has_Discriminants (T2) then

         --  Handle derivations of private subtypes. For example S1 statically
         --  matches the full view of T1 in the following example:

         --      type T1(<>) is new Root with private;
         --      subtype S1 is new T1;
         --      overriding proc P1 (P : S1);
         --    private
         --      type T1 (D : Disc) is new Root with ...

         if Ekind (T2) = E_Record_Subtype_With_Private
           and then not Has_Discriminants (T2)
           and then Partial_View_Has_Unknown_Discr (T1)
           and then Etype (T2) = T1
         then
            return True;

         elsif Ekind (T1) = E_Record_Subtype_With_Private
           and then not Has_Discriminants (T1)
           and then Partial_View_Has_Unknown_Discr (T2)
           and then Etype (T1) = T2
         then
            return True;

         --  Because of view exchanges in multiple instantiations, conformance
         --  checking might try to match a partial view of a type with no
         --  discriminants with a full view that has defaulted discriminants.
         --  In such a case, use the discriminant constraint of the full view,
         --  which must exist because we know that the two subtypes have the
         --  same base type.

         elsif Has_Discriminants (T1) /= Has_Discriminants (T2) then
            if In_Instance then
               if Is_Private_Type (T2)
                 and then Present (Full_View (T2))
                 and then Has_Discriminants (Full_View (T2))
               then
                  return Subtypes_Statically_Match (T1, Full_View (T2));

               elsif Is_Private_Type (T1)
                 and then Present (Full_View (T1))
                 and then Has_Discriminants (Full_View (T1))
               then
                  return Subtypes_Statically_Match (Full_View (T1), T2);

               else
                  return False;
               end if;
            else
               return False;
            end if;
         end if;

         declare

            function Original_Discriminant_Constraint
              (Typ : Entity_Id) return Elist_Id;
            --  Returns Typ's discriminant constraint, or if the constraint
            --  is inherited from an ancestor type, then climbs the parent
            --  types to locate and return the constraint farthest up the
            --  parent chain that Typ's constraint is ultimately inherited
            --  from (stopping before a parent that doesn't impose a constraint
            --  or a parent that has new discriminants). This ensures a proper
            --  result from the equality comparison of Elist_Ids below (as
            --  otherwise, derived types that inherit constraints may appear
            --  to be unequal, because each level of derivation can have its
            --  own copy of the constraint).

            function Original_Discriminant_Constraint
              (Typ : Entity_Id) return Elist_Id
            is
            begin
               if not Has_Discriminants (Typ) then
                  return No_Elist;

               --  If Typ is not a derived type, then directly return the
               --  its constraint.

               elsif not Is_Derived_Type (Typ) then
                  return Discriminant_Constraint (Typ);

               --  If the parent type doesn't have discriminants, doesn't
               --  have a constraint, or has new discriminants, then stop
               --  and return Typ's constraint.

               elsif not Has_Discriminants (Etype (Typ))

                 --  No constraint on the parent type

                 or else No (Discriminant_Constraint (Etype (Typ)))
                 or else Is_Empty_Elmt_List
                           (Discriminant_Constraint (Etype (Typ)))

                 --  The parent type defines new discriminants

                 or else
                   (Is_Base_Type (Etype (Typ))
                     and then Present (Discriminant_Specifications
                                         (Parent (Etype (Typ)))))
               then
                  return Discriminant_Constraint (Typ);

               --  Otherwise, make a recursive call on the parent type

               else
                  return Original_Discriminant_Constraint (Etype (Typ));
               end if;
            end Original_Discriminant_Constraint;

            --  Local variables

            DL1 : constant Elist_Id := Original_Discriminant_Constraint (T1);
            DL2 : constant Elist_Id := Original_Discriminant_Constraint (T2);

            DA1 : Elmt_Id;
            DA2 : Elmt_Id;

         begin
            if DL1 = DL2 then
               return True;
            elsif Is_Constrained (T1) /= Is_Constrained (T2) then
               return False;
            end if;

            --  Now loop through the discriminant constraints

            --  Note: the guard here seems necessary, since it is possible at
            --  least for DL1 to be No_Elist. Not clear this is reasonable ???

            if Present (DL1) and then Present (DL2) then
               DA1 := First_Elmt (DL1);
               DA2 := First_Elmt (DL2);
               while Present (DA1) loop
                  declare
                     Expr1 : constant Node_Id := Node (DA1);
                     Expr2 : constant Node_Id := Node (DA2);

                  begin
                     if not Is_OK_Static_Expression (Expr1)
                       or else not Is_OK_Static_Expression (Expr2)
                     then
                        return False;

                        --  If either expression raised a Constraint_Error,
                        --  consider the expressions as matching, since this
                        --  helps to prevent cascading errors.

                     elsif Raises_Constraint_Error (Expr1)
                       or else Raises_Constraint_Error (Expr2)
                     then
                        null;

                     elsif Expr_Value (Expr1) /= Expr_Value (Expr2) then
                        return False;
                     end if;
                  end;

                  Next_Elmt (DA1);
                  Next_Elmt (DA2);
               end loop;
            end if;
         end;

         return True;

      --  A definite type does not match an indefinite or classwide type.
      --  However, a generic type with unknown discriminants may be
      --  instantiated with a type with no discriminants, and conformance
      --  checking on an inherited operation may compare the actual with the
      --  subtype that renames it in the instance.

      elsif Has_Unknown_Discriminants (T1) /= Has_Unknown_Discriminants (T2)
      then
         return
           Is_Generic_Actual_Type (T1) or else Is_Generic_Actual_Type (T2);

      --  Array type

      elsif Is_Array_Type (T1) then

         --  If either subtype is unconstrained then both must be, and if both
         --  are unconstrained then no further checking is needed.

         if not Is_Constrained (T1) or else not Is_Constrained (T2) then
            return not (Is_Constrained (T1) or else Is_Constrained (T2));
         end if;

         --  Both subtypes are constrained, so check that the index subtypes
         --  statically match.

         declare
            Index1 : Node_Id := First_Index (T1);
            Index2 : Node_Id := First_Index (T2);

         begin
            while Present (Index1) loop
               if not
                 Subtypes_Statically_Match (Etype (Index1), Etype (Index2))
               then
                  return False;
               end if;

               Next_Index (Index1);
               Next_Index (Index2);
            end loop;

            return True;
         end;

      elsif Is_Access_Type (T1) then
         if Can_Never_Be_Null (T1) /= Can_Never_Be_Null (T2) then
            return False;

         elsif Ekind (T1) in E_Access_Subprogram_Type
                           | E_Anonymous_Access_Subprogram_Type
         then
            return
              Subtype_Conformant
                (Designated_Type (T1),
                 Designated_Type (T2));
         else
            return
              Subtypes_Statically_Match
                (Designated_Type (T1),
                 Designated_Type (T2))
              and then Is_Access_Constant (T1) = Is_Access_Constant (T2);
         end if;

      --  All other types definitely match

      else
         return True;
      end if;
   end Subtypes_Statically_Match;

   ----------
   -- Test --
   ----------

   function Test (Cond : Boolean) return Uint is
   begin
      if Cond then
         return Uint_1;
      else
         return Uint_0;
      end if;
   end Test;

   ---------------------
   -- Test_Comparison --
   ---------------------

   procedure Test_Comparison
     (Op           : Node_Id;
      Assume_Valid : Boolean;
      True_Result  : out Boolean;
      False_Result : out Boolean)
   is
      Left     : constant Node_Id   := Left_Opnd (Op);
      Left_Typ : constant Entity_Id := Etype (Left);
      Orig_Op  : constant Node_Id   := Original_Node (Op);

      procedure Replacement_Warning (Msg : String);
      --  Emit a warning on a comparison that can be replaced by '='

      -------------------------
      -- Replacement_Warning --
      -------------------------

      procedure Replacement_Warning (Msg : String) is
      begin
         if Constant_Condition_Warnings
           and then Comes_From_Source (Orig_Op)
           and then Is_Integer_Type (Left_Typ)
           and then not Error_Posted (Op)
           and then not Has_Warnings_Off (Left_Typ)
           and then not In_Instance
         then
            Error_Msg_N (Msg, Op);
         end if;
      end Replacement_Warning;

      --  Local variables

      Res : constant Compare_Result :=
              Compile_Time_Compare (Left, Right_Opnd (Op), Assume_Valid);

   --  Start of processing for Test_Comparison

   begin
      case N_Op_Compare (Nkind (Op)) is
         when N_Op_Eq =>
            True_Result  := Res = EQ;
            False_Result := Res = LT or else Res = GT or else Res = NE;

         when N_Op_Ge =>
            True_Result  := Res in Compare_GE;
            False_Result := Res = LT;

            if Res = LE and then Nkind (Orig_Op) = N_Op_Ge then
               Replacement_Warning
                 ("can never be greater than, could replace by ""'=""?c?");
            end if;

         when N_Op_Gt =>
            True_Result  := Res = GT;
            False_Result := Res in Compare_LE;

         when N_Op_Le =>
            True_Result  := Res in Compare_LE;
            False_Result := Res = GT;

            if Res = GE and then Nkind (Orig_Op) = N_Op_Le then
               Replacement_Warning
                 ("can never be less than, could replace by ""'=""?c?");
            end if;

         when N_Op_Lt =>
            True_Result  := Res = LT;
            False_Result := Res in Compare_GE;

         when N_Op_Ne =>
            True_Result  := Res = NE or else Res = GT or else Res = LT;
            False_Result := Res = EQ;
      end case;
   end Test_Comparison;

   ---------------------------------
   -- Test_Expression_Is_Foldable --
   ---------------------------------

   --  One operand case

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean)
   is
   begin
      Stat := False;
      Fold := False;

      if Debug_Flag_Dot_F and then In_Extended_Main_Source_Unit (N) then
         return;
      end if;

      --  If operand is Any_Type, just propagate to result and do not
      --  try to fold, this prevents cascaded errors.

      if Etype (Op1) = Any_Type then
         Set_Etype (N, Any_Type);
         return;

      --  If operand raises Constraint_Error, then replace node N with the
      --  raise Constraint_Error node, and we are obviously not foldable.
      --  Note that this replacement inherits the Is_Static_Expression flag
      --  from the operand.

      elsif Raises_Constraint_Error (Op1) then
         Rewrite_In_Raise_CE (N, Op1);
         return;

      --  If the operand is not static, then the result is not static, and
      --  all we have to do is to check the operand since it is now known
      --  to appear in a non-static context.

      elsif not Is_Static_Expression (Op1) then
         Check_Non_Static_Context (Op1);
         Fold := Compile_Time_Known_Value (Op1);
         return;

      --   An expression of a formal modular type is not foldable because
      --   the modulus is unknown.

      elsif Is_Modular_Integer_Type (Etype (Op1))
        and then Is_Generic_Type (Etype (Op1))
      then
         Check_Non_Static_Context (Op1);
         return;

      --  Here we have the case of an operand whose type is OK, which is
      --  static, and which does not raise Constraint_Error, we can fold.

      else
         Set_Is_Static_Expression (N);
         Fold := True;
         Stat := True;
      end if;
   end Test_Expression_Is_Foldable;

   --  Two operand case

   procedure Test_Expression_Is_Foldable
     (N        : Node_Id;
      Op1      : Node_Id;
      Op2      : Node_Id;
      Stat     : out Boolean;
      Fold     : out Boolean;
      CRT_Safe : Boolean := False)
   is
      Rstat : constant Boolean := Is_Static_Expression (Op1)
                                    and then
                                  Is_Static_Expression (Op2);

   begin
      Stat := False;
      Fold := False;

      --  Inhibit folding if -gnatd.f flag set

      if Debug_Flag_Dot_F and then In_Extended_Main_Source_Unit (N) then
         return;
      end if;

      --  If either operand is Any_Type, just propagate to result and
      --  do not try to fold, this prevents cascaded errors.

      if Etype (Op1) = Any_Type or else Etype (Op2) = Any_Type then
         Set_Etype (N, Any_Type);
         return;

      --  If left operand raises Constraint_Error, then replace node N with the
      --  Raise_Constraint_Error node, and we are obviously not foldable.
      --  Is_Static_Expression is set from the two operands in the normal way,
      --  and we check the right operand if it is in a non-static context.

      elsif Raises_Constraint_Error (Op1) then
         if not Rstat then
            Check_Non_Static_Context (Op2);
         end if;

         Rewrite_In_Raise_CE (N, Op1);
         Set_Is_Static_Expression (N, Rstat);
         return;

      --  Similar processing for the case of the right operand. Note that we
      --  don't use this routine for the short-circuit case, so we do not have
      --  to worry about that special case here.

      elsif Raises_Constraint_Error (Op2) then
         if not Rstat then
            Check_Non_Static_Context (Op1);
         end if;

         Rewrite_In_Raise_CE (N, Op2);
         Set_Is_Static_Expression (N, Rstat);
         return;

      --  Exclude expressions of a generic modular type, as above

      elsif Is_Modular_Integer_Type (Etype (Op1))
        and then Is_Generic_Type (Etype (Op1))
      then
         Check_Non_Static_Context (Op1);
         return;

      --  If result is not static, then check non-static contexts on operands
      --  since one of them may be static and the other one may not be static.

      elsif not Rstat then
         Check_Non_Static_Context (Op1);
         Check_Non_Static_Context (Op2);

         if CRT_Safe then
            Fold := CRT_Safe_Compile_Time_Known_Value (Op1)
                      and then CRT_Safe_Compile_Time_Known_Value (Op2);
         else
            Fold := Compile_Time_Known_Value (Op1)
                      and then Compile_Time_Known_Value (Op2);
         end if;

         if not Fold
           and then not Is_Modular_Integer_Type (Etype (N))
         then
            case Nkind (N) is
               when N_Op_And =>

                  --  (False and XXX) = (XXX and False) = False

                  Fold :=
                    (Compile_Time_Known_Value (Op1)
                       and then Is_False (Expr_Value (Op1))
                       and then Side_Effect_Free (Op2))
                      or else (Compile_Time_Known_Value (Op2)
                                and then Is_False (Expr_Value (Op2))
                                and then Side_Effect_Free (Op1));

               when N_Op_Or =>

                  --  (True and XXX) = (XXX and True) = True

                  Fold :=
                    (Compile_Time_Known_Value (Op1)
                       and then Is_True (Expr_Value (Op1))
                       and then Side_Effect_Free (Op2))
                      or else (Compile_Time_Known_Value (Op2)
                                and then Is_True (Expr_Value (Op2))
                                and then Side_Effect_Free (Op1));

               when others => null;
            end case;
         end if;

         return;

      --  Else result is static and foldable. Both operands are static, and
      --  neither raises Constraint_Error, so we can definitely fold.

      else
         Set_Is_Static_Expression (N);
         Fold := True;
         Stat := True;
         return;
      end if;
   end Test_Expression_Is_Foldable;

   -------------------
   -- Test_In_Range --
   -------------------

   function Test_In_Range
     (N            : Node_Id;
      Typ          : Entity_Id;
      Assume_Valid : Boolean;
      Fixed_Int    : Boolean;
      Int_Real     : Boolean) return Range_Membership
   is
      Val  : Uint;
      Valr : Ureal;

      pragma Warnings (Off, Assume_Valid);
      --  For now Assume_Valid is unreferenced since the current implementation
      --  always returns Unknown if N is not a compile-time-known value, but we
      --  keep the parameter to allow for future enhancements in which we try
      --  to get the information in the variable case as well.

   begin
      --  If an error was posted on expression, then return Unknown, we do not
      --  want cascaded errors based on some false analysis of a junk node.

      if Error_Posted (N) then
         return Unknown;

      --  Expression that raises Constraint_Error is an odd case. We certainly
      --  do not want to consider it to be in range. It might make sense to
      --  consider it always out of range, but this causes incorrect error
      --  messages about static expressions out of range. So we just return
      --  Unknown, which is always safe.

      elsif Raises_Constraint_Error (N) then
         return Unknown;

      --  Universal types have no range limits, so always in range

      elsif Is_Universal_Numeric_Type (Typ) then
         return In_Range;

      --  Never known if not scalar type. Don't know if this can actually
      --  happen, but our spec allows it, so we must check.

      elsif not Is_Scalar_Type (Typ) then
         return Unknown;

      --  Never known if this is a generic type, since the bounds of generic
      --  types are junk. Note that if we only checked for static expressions
      --  (instead of compile-time-known values) below, we would not need this
      --  check, because values of a generic type can never be static, but they
      --  can be known at compile time.

      elsif Is_Generic_Type (Typ) then
         return Unknown;

      --  Case of a known compile time value, where we can check if it is in
      --  the bounds of the given type.

      elsif Compile_Time_Known_Value (N) then
         declare
            Lo       : constant Node_Id := Type_Low_Bound (Typ);
            Hi       : constant Node_Id := Type_High_Bound (Typ);
            LB_Known : constant Boolean := Compile_Time_Known_Value (Lo);
            HB_Known : constant Boolean := Compile_Time_Known_Value (Hi);

         begin
            --  Fixed point types should be considered as such only if flag
            --  Fixed_Int is set to False.

            if Is_Floating_Point_Type (Typ)
              or else (Is_Fixed_Point_Type (Typ) and then not Fixed_Int)
              or else Int_Real
            then
               Valr := Expr_Value_R (N);

               if LB_Known and HB_Known then
                  if Valr >= Expr_Value_R (Lo)
                       and then
                     Valr <= Expr_Value_R (Hi)
                  then
                     return In_Range;
                  else
                     return Out_Of_Range;
                  end if;

               elsif (LB_Known and then Valr < Expr_Value_R (Lo))
                       or else
                     (HB_Known and then Valr > Expr_Value_R (Hi))
               then
                  return Out_Of_Range;

               else
                  return Unknown;
               end if;

            else
               Val := Expr_Value (N);

               if LB_Known and HB_Known then
                  if Val >= Expr_Value (Lo) and then Val <= Expr_Value (Hi)
                  then
                     return In_Range;
                  else
                     return Out_Of_Range;
                  end if;

               elsif (LB_Known and then Val < Expr_Value (Lo))
                       or else
                     (HB_Known and then Val > Expr_Value (Hi))
               then
                  return Out_Of_Range;

               else
                  return Unknown;
               end if;
            end if;
         end;

      --  Here for value not known at compile time. Case of expression subtype
      --  is Typ or is a subtype of Typ, and we can assume expression is valid.
      --  In this case we know it is in range without knowing its value.

      elsif Assume_Valid
        and then (Etype (N) = Typ or else Is_Subtype_Of (Etype (N), Typ))
      then
         return In_Range;

      --  Another special case. For signed integer types, if the target type
      --  has Is_Known_Valid set, and the source type does not have a larger
      --  size, then the source value must be in range. We exclude biased
      --  types, because they bizarrely can generate out of range values.

      elsif Is_Signed_Integer_Type (Etype (N))
        and then Is_Known_Valid (Typ)
        and then Esize (Etype (N)) <= Esize (Typ)
        and then not Has_Biased_Representation (Etype (N))
      then
         return In_Range;

      --  For all other cases, result is unknown

      else
         return Unknown;
      end if;
   end Test_In_Range;

   --------------
   -- To_Bits --
   --------------

   procedure To_Bits (U : Uint; B : out Bits) is
   begin
      for J in 0 .. B'Last loop
         B (J) := (U / (2 ** J)) mod 2 /= 0;
      end loop;
   end To_Bits;

   --------------------
   -- Why_Not_Static --
   --------------------

   procedure Why_Not_Static (Expr : Node_Id) is
      N   : constant Node_Id := Original_Node (Expr);
      Typ : Entity_Id        := Empty;
      E   : Entity_Id;
      Alt : Node_Id;
      Exp : Node_Id;

      procedure Why_Not_Static_List (L : List_Id);
      --  A version that can be called on a list of expressions. Finds all
      --  non-static violations in any element of the list.

      -------------------------
      -- Why_Not_Static_List --
      -------------------------

      procedure Why_Not_Static_List (L : List_Id) is
         N : Node_Id;
      begin
         N := First (L);
         while Present (N) loop
            Why_Not_Static (N);
            Next (N);
         end loop;
      end Why_Not_Static_List;

   --  Start of processing for Why_Not_Static

   begin
      --  Ignore call on error or empty node

      if No (Expr) or else Nkind (Expr) = N_Error then
         return;
      end if;

      --  Preprocessing for sub expressions

      if Nkind (Expr) in N_Subexpr then

         --  Nothing to do if expression is static

         if Is_OK_Static_Expression (Expr) then
            return;
         end if;

         --  Test for Constraint_Error raised

         if Raises_Constraint_Error (Expr) then

            --  Special case membership to find out which piece to flag

            if Nkind (N) in N_Membership_Test then
               if Raises_Constraint_Error (Left_Opnd (N)) then
                  Why_Not_Static (Left_Opnd (N));
                  return;

               elsif Present (Right_Opnd (N))
                 and then Raises_Constraint_Error (Right_Opnd (N))
               then
                  Why_Not_Static (Right_Opnd (N));
                  return;

               else
                  pragma Assert (Present (Alternatives (N)));

                  Alt := First (Alternatives (N));
                  while Present (Alt) loop
                     if Raises_Constraint_Error (Alt) then
                        Why_Not_Static (Alt);
                        return;
                     else
                        Next (Alt);
                     end if;
                  end loop;
               end if;

            --  Special case a range to find out which bound to flag

            elsif Nkind (N) = N_Range then
               if Raises_Constraint_Error (Low_Bound (N)) then
                  Why_Not_Static (Low_Bound (N));
                  return;

               elsif Raises_Constraint_Error (High_Bound (N)) then
                  Why_Not_Static (High_Bound (N));
                  return;
               end if;

            --  Special case attribute to see which part to flag

            elsif Nkind (N) = N_Attribute_Reference then
               if Raises_Constraint_Error (Prefix (N)) then
                  Why_Not_Static (Prefix (N));
                  return;
               end if;

               Exp := First (Expressions (N));
               while Present (Exp) loop
                  if Raises_Constraint_Error (Exp) then
                     Why_Not_Static (Exp);
                     return;
                  end if;

                  Next (Exp);
               end loop;

            --  Special case a subtype name

            elsif Is_Entity_Name (Expr) and then Is_Type (Entity (Expr)) then
               Error_Msg_NE
                 ("!& is not a static subtype (RM 4.9(26))", N, Entity (Expr));
               return;
            end if;

            --  End of special cases

            Error_Msg_N
              ("!expression raises exception, cannot be static (RM 4.9(34))",
               N);
            return;
         end if;

         --  If no type, then something is pretty wrong, so ignore

         Typ := Etype (Expr);

         if No (Typ) then
            return;
         end if;

         --  Type must be scalar or string type (but allow Bignum, since this
         --  is really a scalar type from our point of view in this diagnosis).

         if not Is_Scalar_Type (Typ)
           and then not Is_String_Type (Typ)
           and then not Is_RTE (Typ, RE_Bignum)
         then
            Error_Msg_N
              ("!static expression must have scalar or string type " &
               "(RM 4.9(2))", N);
            return;
         end if;
      end if;

      --  If we got through those checks, test particular node kind

      case Nkind (N) is

         --  Entity name

         when N_Expanded_Name
            | N_Identifier
            | N_Operator_Symbol
         =>
            E := Entity (N);

            if Is_Named_Number (E) then
               null;

            elsif Ekind (E) = E_Constant then

               --  One case we can give a better message is when we have a
               --  string literal created by concatenating an aggregate with
               --  an others expression.

               Entity_Case : declare
                  CV : constant Node_Id := Constant_Value (E);
                  CO : constant Node_Id := Original_Node (CV);

                  function Is_Aggregate (N : Node_Id) return Boolean;
                  --  See if node N came from an others aggregate, if so
                  --  return True and set Error_Msg_Sloc to aggregate.

                  ------------------
                  -- Is_Aggregate --
                  ------------------

                  function Is_Aggregate (N : Node_Id) return Boolean is
                  begin
                     if Nkind (Original_Node (N)) = N_Aggregate then
                        Error_Msg_Sloc := Sloc (Original_Node (N));
                        return True;

                     elsif Is_Entity_Name (N)
                       and then Ekind (Entity (N)) = E_Constant
                       and then
                         Nkind (Original_Node (Constant_Value (Entity (N)))) =
                                                                  N_Aggregate
                     then
                        Error_Msg_Sloc :=
                          Sloc (Original_Node (Constant_Value (Entity (N))));
                        return True;

                     else
                        return False;
                     end if;
                  end Is_Aggregate;

               --  Start of processing for Entity_Case

               begin
                  if Is_Aggregate (CV)
                    or else (Nkind (CO) = N_Op_Concat
                              and then (Is_Aggregate (Left_Opnd (CO))
                                          or else
                                        Is_Aggregate (Right_Opnd (CO))))
                  then
                     Error_Msg_N ("!aggregate (#) is never static", N);

                  elsif No (CV) or else not Is_Static_Expression (CV) then
                     Error_Msg_NE
                       ("!& is not a static constant (RM 4.9(5))", N, E);
                  end if;
               end Entity_Case;

            elsif Is_Type (E) then
               Error_Msg_NE
                 ("!& is not a static subtype (RM 4.9(26))", N, E);

            elsif E /= Any_Id then
               Error_Msg_NE
                 ("!& is not static constant or named number "
                  & "(RM 4.9(5))", N, E);
            end if;

         --  Binary operator

         when N_Binary_Op
            | N_Membership_Test
            | N_Short_Circuit
         =>
            if Nkind (N) in N_Op_Shift then
               Error_Msg_N
                 ("!shift functions are never static (RM 4.9(6,18))", N);
            else
               Why_Not_Static (Left_Opnd (N));
               Why_Not_Static (Right_Opnd (N));
            end if;

         --  Unary operator

         when N_Unary_Op =>
            Why_Not_Static (Right_Opnd (N));

         --  Attribute reference

         when N_Attribute_Reference =>
            Why_Not_Static_List (Expressions (N));

            E := Etype (Prefix (N));

            if E = Standard_Void_Type then
               return;
            end if;

            --  Special case non-scalar'Size since this is a common error

            if Attribute_Name (N) = Name_Size then
               Error_Msg_N
                 ("!size attribute is only static for static scalar type "
                  & "(RM 4.9(7,8))", N);

            --  Flag array cases

            elsif Is_Array_Type (E) then
               if Attribute_Name (N)
                    not in Name_First | Name_Last | Name_Length
               then
                  Error_Msg_N
                    ("!static array attribute must be Length, First, or Last "
                     & "(RM 4.9(8))", N);

               --  Since we know the expression is not-static (we already
               --  tested for this, must mean array is not static).

               else
                  Error_Msg_N
                    ("!prefix is non-static array (RM 4.9(8))", Prefix (N));
               end if;

               return;

            --  Special case generic types, since again this is a common source
            --  of confusion.

            elsif Is_Generic_Actual_Type (E) or else Is_Generic_Type (E) then
               Error_Msg_N
                 ("!attribute of generic type is never static "
                  & "(RM 4.9(7,8))", N);

            elsif Is_OK_Static_Subtype (E) then
               null;

            elsif Is_Scalar_Type (E) then
               Error_Msg_N
                 ("!prefix type for attribute is not static scalar subtype "
                  & "(RM 4.9(7))", N);

            else
               Error_Msg_N
                 ("!static attribute must apply to array/scalar type "
                  & "(RM 4.9(7,8))", N);
            end if;

         --  String literal

         when N_String_Literal =>
            Error_Msg_N
              ("!subtype of string literal is non-static (RM 4.9(4))", N);

         --  Explicit dereference

         when N_Explicit_Dereference =>
            Error_Msg_N
              ("!explicit dereference is never static (RM 4.9)", N);

         --  Function call

         when N_Function_Call =>
            Why_Not_Static_List (Parameter_Associations (N));

            --  Complain about non-static function call unless we have Bignum
            --  which means that the underlying expression is really some
            --  scalar arithmetic operation.

            if not Is_RTE (Typ, RE_Bignum) then
               Error_Msg_N ("!non-static function call (RM 4.9(6,18))", N);
            end if;

         --  Parameter assocation (test actual parameter)

         when N_Parameter_Association =>
            Why_Not_Static (Explicit_Actual_Parameter (N));

         --  Indexed component

         when N_Indexed_Component =>
            Error_Msg_N ("!indexed component is never static (RM 4.9)", N);

         --  Procedure call

         when N_Procedure_Call_Statement =>
            Error_Msg_N ("!procedure call is never static (RM 4.9)", N);

         --  Qualified expression (test expression)

         when N_Qualified_Expression =>
            Why_Not_Static (Expression (N));

         --  Aggregate

         when N_Aggregate
            | N_Extension_Aggregate
         =>
            Error_Msg_N ("!an aggregate is never static (RM 4.9)", N);

         --  Range

         when N_Range =>
            Why_Not_Static (Low_Bound (N));
            Why_Not_Static (High_Bound (N));

         --  Range constraint, test range expression

         when N_Range_Constraint =>
            Why_Not_Static (Range_Expression (N));

         --  Subtype indication, test constraint

         when N_Subtype_Indication =>
            Why_Not_Static (Constraint (N));

         --  Selected component

         when N_Selected_Component =>
            Error_Msg_N ("!selected component is never static (RM 4.9)", N);

         --  Slice

         when N_Slice =>
            Error_Msg_N ("!slice is never static (RM 4.9)", N);

         when N_Type_Conversion =>
            Why_Not_Static (Expression (N));

            if not Is_Scalar_Type (Entity (Subtype_Mark (N)))
              or else not Is_OK_Static_Subtype (Entity (Subtype_Mark (N)))
            then
               Error_Msg_N
                 ("!static conversion requires static scalar subtype result "
                  & "(RM 4.9(9))", N);
            end if;

         --  Unchecked type conversion

         when N_Unchecked_Type_Conversion =>
            Error_Msg_N
              ("!unchecked type conversion is never static (RM 4.9)", N);

         --  All other cases, no reason to give

         when others =>
            null;
      end case;
   end Why_Not_Static;

end Sem_Eval;
