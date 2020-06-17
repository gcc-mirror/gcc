/* Diagnostic routines shared by all languages that are variants of C.
   Copyright (C) 1992-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "tree.h"
#include "c-common.h"
#include "memmodel.h"
#include "tm_p.h"
#include "diagnostic.h"
#include "intl.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "gcc-rich-location.h"
#include "gimplify.h"
#include "c-family/c-indentation.h"
#include "c-family/c-spellcheck.h"
#include "calls.h"
#include "stor-layout.h"

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression.
   Note the ANSI C standard says it is erroneous for a
   constant expression to overflow.  */

void
constant_expression_warning (tree value)
{
  if (warn_overflow && pedantic
      && (TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
	  || TREE_CODE (value) == FIXED_CST
	  || TREE_CODE (value) == VECTOR_CST
	  || TREE_CODE (value) == COMPLEX_CST)
      && TREE_OVERFLOW (value))
    pedwarn (input_location, OPT_Woverflow, "overflow in constant expression");
}

/* The same as above but print an unconditional error.  */

void
constant_expression_error (tree value)
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == FIXED_CST
       || TREE_CODE (value) == VECTOR_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_OVERFLOW (value))
    error ("overflow in constant expression");
}

/* Print a warning if an expression result VALUE had an overflow
   in folding and its operands hadn't.  EXPR, which may be null, is
   the operand of the expression.

   Invoke this function on every expression that
   (1) appears in the source code, and
   (2) is a constant expression that overflowed, and
   (3) is not already checked by convert_and_check;
   however, do not invoke this function on operands of explicit casts
   or when the expression is the result of an operator and any operand
   already overflowed.  */

void
overflow_warning (location_t loc, tree value, tree expr)
{
  if (c_inhibit_evaluation_warnings != 0)
    return;

  const char *warnfmt = NULL;

  switch (TREE_CODE (value))
    {
    case INTEGER_CST:
      warnfmt = (expr
		 ? G_("integer overflow in expression %qE of type %qT "
		      "results in %qE")
		 : G_("integer overflow in expression of type %qT "
		      "results in %qE"));
      break;

    case REAL_CST:
      warnfmt = (expr
		 ? G_("floating point overflow in expression %qE "
		      "of type %qT results in %qE")
		 : G_("floating point overflow in expression of type %qT "
		      "results in %qE"));
      break;

    case FIXED_CST:
      warnfmt = (expr
		 ? G_("fixed-point overflow in expression %qE of type %qT "
		      "results in %qE")
		 : G_("fixed-point overflow in expression of type %qT "
		      "results in %qE"));
      break;

    case VECTOR_CST:
      warnfmt = (expr
		 ? G_("vector overflow in expression %qE of type %qT "
		      "results in %qE")
		 : G_("vector overflow in expression of type %qT "
		      "results in %qE"));
      break;

    case COMPLEX_CST:
      if (TREE_CODE (TREE_REALPART (value)) == INTEGER_CST)
	warnfmt = (expr
		   ? G_("complex integer overflow in expression %qE "
			"of type %qT results in %qE")
		   : G_("complex integer overflow in expression of type %qT "
			"results in %qE"));
      else if (TREE_CODE (TREE_REALPART (value)) == REAL_CST)
	warnfmt = (expr
		   ? G_("complex floating point overflow in expression %qE "
			"of type %qT results in %qE")
		   : G_("complex floating point overflow in expression "
			"of type %qT results in %qE"));
      else
	return;
      break;

    default:
      return;
    }

  bool warned;
  if (expr)
    warned = warning_at (loc, OPT_Woverflow, warnfmt, expr, TREE_TYPE (expr),
			 value);
  else
    warned = warning_at (loc, OPT_Woverflow, warnfmt, TREE_TYPE (value),
			 value);

  if (warned)
    TREE_NO_WARNING (value) = 1;
}

/* Helper function for walk_tree.  Unwrap C_MAYBE_CONST_EXPRs in an expression
   pointed to by TP.  */

static tree
unwrap_c_maybe_const (tree *tp, int *walk_subtrees, void *)
{
  if (TREE_CODE (*tp) == C_MAYBE_CONST_EXPR)
    {
      *tp = C_MAYBE_CONST_EXPR_EXPR (*tp);
      /* C_MAYBE_CONST_EXPRs don't nest.  */
      *walk_subtrees = false;
    }
  return NULL_TREE;
}

/* Warn about uses of logical || / && operator in a context where it
   is likely that the bitwise equivalent was intended by the
   programmer.  We have seen an expression in which CODE is a binary
   operator used to combine expressions OP_LEFT and OP_RIGHT, which before folding
   had CODE_LEFT and CODE_RIGHT, into an expression of type TYPE.  */

void
warn_logical_operator (location_t location, enum tree_code code, tree type,
		       enum tree_code code_left, tree op_left,
		       enum tree_code ARG_UNUSED (code_right), tree op_right)
{
  int or_op = (code == TRUTH_ORIF_EXPR || code == TRUTH_OR_EXPR);
  int in0_p, in1_p, in_p;
  tree low0, low1, low, high0, high1, high, lhs, rhs, tem;
  bool strict_overflow_p = false;

  if (!warn_logical_op)
    return;

  if (code != TRUTH_ANDIF_EXPR
      && code != TRUTH_AND_EXPR
      && code != TRUTH_ORIF_EXPR
      && code != TRUTH_OR_EXPR)
    return;

  /* We don't want to warn if either operand comes from a macro
     expansion.  ??? This doesn't work with e.g. NEGATE_EXPR yet;
     see PR61534.  */
  if (from_macro_expansion_at (EXPR_LOCATION (op_left))
      || from_macro_expansion_at (EXPR_LOCATION (op_right)))
    return;

  /* Warn if &&/|| are being used in a context where it is
     likely that the bitwise equivalent was intended by the
     programmer. That is, an expression such as op && MASK
     where op should not be any boolean expression, nor a
     constant, and mask seems to be a non-boolean integer constant.  */
  STRIP_ANY_LOCATION_WRAPPER (op_right);
  if (TREE_CODE (op_right) == CONST_DECL)
    /* An enumerator counts as a constant.  */
    op_right = DECL_INITIAL (op_right);
  tree stripped_op_left = tree_strip_any_location_wrapper (op_left);
  if (!truth_value_p (code_left)
      && INTEGRAL_TYPE_P (TREE_TYPE (op_left))
      && !CONSTANT_CLASS_P (stripped_op_left)
      && TREE_CODE (stripped_op_left) != CONST_DECL
      && !TREE_NO_WARNING (op_left)
      && TREE_CODE (op_right) == INTEGER_CST
      && !integer_zerop (op_right)
      && !integer_onep (op_right))
    {
      bool warned;
      if (or_op)
	warned
	  = warning_at (location, OPT_Wlogical_op,
			"logical %<or%> applied to non-boolean constant");
      else
	warned
	  = warning_at (location, OPT_Wlogical_op,
			"logical %<and%> applied to non-boolean constant");
      if (warned)
	TREE_NO_WARNING (op_left) = true;
      return;
    }

  /* We do not warn for constants because they are typical of macro
     expansions that test for features.  */
  if (CONSTANT_CLASS_P (fold_for_warn (op_left))
      || CONSTANT_CLASS_P (fold_for_warn (op_right)))
    return;

  /* This warning only makes sense with logical operands.  */
  if (!(truth_value_p (TREE_CODE (op_left))
	|| INTEGRAL_TYPE_P (TREE_TYPE (op_left)))
      || !(truth_value_p (TREE_CODE (op_right))
	   || INTEGRAL_TYPE_P (TREE_TYPE (op_right))))
    return;

  /* The range computations only work with scalars.  */
  if (VECTOR_TYPE_P (TREE_TYPE (op_left))
      || VECTOR_TYPE_P (TREE_TYPE (op_right)))
    return;

  /* We first test whether either side separately is trivially true
     (with OR) or trivially false (with AND).  If so, do not warn.
     This is a common idiom for testing ranges of data types in
     portable code.  */
  op_left = unshare_expr (op_left);
  walk_tree_without_duplicates (&op_left, unwrap_c_maybe_const, NULL);
  lhs = make_range (op_left, &in0_p, &low0, &high0, &strict_overflow_p);
  if (!lhs)
    return;

  /* If this is an OR operation, invert both sides; now, the result
     should be always false to get a warning.  */
  if (or_op)
    in0_p = !in0_p;

  tem = build_range_check (UNKNOWN_LOCATION, type, lhs, in0_p, low0, high0);
  if (tem && integer_zerop (tem))
    return;

  op_right = unshare_expr (op_right);
  walk_tree_without_duplicates (&op_right, unwrap_c_maybe_const, NULL);
  rhs = make_range (op_right, &in1_p, &low1, &high1, &strict_overflow_p);
  if (!rhs)
    return;

  /* If this is an OR operation, invert both sides; now, the result
     should be always false to get a warning.  */
  if (or_op)
    in1_p = !in1_p;

  tem = build_range_check (UNKNOWN_LOCATION, type, rhs, in1_p, low1, high1);
  if (tem && integer_zerop (tem))
    return;

  /* If both expressions have the same operand, if we can merge the
     ranges, ...  */
  if (operand_equal_p (lhs, rhs, 0)
      && merge_ranges (&in_p, &low, &high, in0_p, low0, high0,
		       in1_p, low1, high1))
    {
      tem = build_range_check (UNKNOWN_LOCATION, type, lhs, in_p, low, high);
      /* ... and if the range test is always false, then warn.  */
      if (tem && integer_zerop (tem))
	{
	  if (or_op)
	    warning_at (location, OPT_Wlogical_op,
			"logical %<or%> of collectively exhaustive tests is "
			"always true");
	  else
	    warning_at (location, OPT_Wlogical_op,
			"logical %<and%> of mutually exclusive tests is "
			"always false");
	}
      /* Or warn if the operands have exactly the same range, e.g.
	 A > 0 && A > 0.  */
      else if (tree_int_cst_equal (low0, low1)
	       && tree_int_cst_equal (high0, high1))
	{
	  if (or_op)
	    warning_at (location, OPT_Wlogical_op,
			"logical %<or%> of equal expressions");
	  else
	    warning_at (location, OPT_Wlogical_op,
			"logical %<and%> of equal expressions");
	}
    }
}

/* Helper function for warn_tautological_cmp.  Look for ARRAY_REFs
   with constant indices.  */

static tree
find_array_ref_with_const_idx_r (tree *expr_p, int *, void *)
{
  tree expr = *expr_p;

  if ((TREE_CODE (expr) == ARRAY_REF
       || TREE_CODE (expr) == ARRAY_RANGE_REF)
      && (TREE_CODE (fold_for_warn (TREE_OPERAND (expr, 1)))
	  == INTEGER_CST))
    return integer_type_node;

  return NULL_TREE;
}

/* Subroutine of warn_tautological_cmp.  Warn about bitwise comparison
   that always evaluate to true or false.  LOC is the location of the
   ==/!= comparison specified by CODE; LHS and RHS are the usual operands
   of this comparison.  */

static void
warn_tautological_bitwise_comparison (const op_location_t &loc, tree_code code,
				      tree lhs, tree rhs)
{
  if (code != EQ_EXPR && code != NE_EXPR)
    return;

  /* Extract the operands from e.g. (x & 8) == 4.  */
  tree bitop;
  tree cst;
  tree stripped_lhs = tree_strip_any_location_wrapper (lhs);
  tree stripped_rhs = tree_strip_any_location_wrapper (rhs);
  if ((TREE_CODE (lhs) == BIT_AND_EXPR
       || TREE_CODE (lhs) == BIT_IOR_EXPR)
      && TREE_CODE (stripped_rhs) == INTEGER_CST)
    bitop = lhs, cst = stripped_rhs;
  else if ((TREE_CODE (rhs) == BIT_AND_EXPR
	    || TREE_CODE (rhs) == BIT_IOR_EXPR)
	   && TREE_CODE (stripped_lhs) == INTEGER_CST)
    bitop = rhs, cst = stripped_lhs;
  else
    return;

  tree bitopcst;
  tree bitop_op0 = fold_for_warn (TREE_OPERAND (bitop, 0));
  if (TREE_CODE (bitop_op0) == INTEGER_CST)
    bitopcst = bitop_op0;
  else {
    tree bitop_op1 = fold_for_warn (TREE_OPERAND (bitop, 1));
    if (TREE_CODE (bitop_op1) == INTEGER_CST)
      bitopcst = bitop_op1;
    else
      return;
  }

  /* Note that the two operands are from before the usual integer
     conversions, so their types might not be the same.
     Use the larger of the two precisions and ignore bits outside
     of that.  */
  int prec = MAX (TYPE_PRECISION (TREE_TYPE (cst)),
		  TYPE_PRECISION (TREE_TYPE (bitopcst)));

  wide_int bitopcstw = wi::to_wide (bitopcst, prec);
  wide_int cstw = wi::to_wide (cst, prec);

  wide_int res;
  if (TREE_CODE (bitop) == BIT_AND_EXPR)
    res = bitopcstw & cstw;
  else
    res = bitopcstw | cstw;

  /* For BIT_AND only warn if (CST2 & CST1) != CST1, and
     for BIT_OR only if (CST2 | CST1) != CST1.  */
  if (res == cstw)
    return;

  binary_op_rich_location richloc (loc, lhs, rhs, false);
  if (code == EQ_EXPR)
    warning_at (&richloc, OPT_Wtautological_compare,
		"bitwise comparison always evaluates to false");
  else
    warning_at (&richloc, OPT_Wtautological_compare,
		"bitwise comparison always evaluates to true");
}

/* Given LOC from a macro expansion, return the map for the outermost
   macro in the nest of expansions.  */

static const line_map_macro *
get_outermost_macro_expansion (location_t loc)
{
  gcc_assert (from_macro_expansion_at (loc));

  const line_map *map = linemap_lookup (line_table, loc);
  const line_map_macro *macro_map;
  do
    {
      macro_map = linemap_check_macro (map);
      loc = linemap_unwind_toward_expansion (line_table, loc, &map);
    } while (linemap_macro_expansion_map_p (map));

  return macro_map;
}

/* Given LOC_A and LOC_B from macro expansions, return true if
   they are "spelled the same" i.e. if they are both directly from
   expansion of the same non-function-like macro.  */

static bool
spelled_the_same_p (location_t loc_a, location_t loc_b)
{
  gcc_assert (from_macro_expansion_at (loc_a));
  gcc_assert (from_macro_expansion_at (loc_b));

  const line_map_macro *map_a = get_outermost_macro_expansion (loc_a);
  const line_map_macro *map_b = get_outermost_macro_expansion (loc_b);

  if (map_a->macro == map_b->macro)
    if (!cpp_fun_like_macro_p (map_a->macro))
      return true;

  return false;
}

/* Warn if a self-comparison always evaluates to true or false.  LOC
   is the location of the comparison with code CODE, LHS and RHS are
   operands of the comparison.  */

void
warn_tautological_cmp (const op_location_t &loc, enum tree_code code,
		       tree lhs, tree rhs)
{
  if (TREE_CODE_CLASS (code) != tcc_comparison)
    return;

  /* Don't warn for various macro expansions.  */
  if (from_macro_expansion_at (loc))
    return;
  bool lhs_in_macro = from_macro_expansion_at (EXPR_LOCATION (lhs));
  bool rhs_in_macro = from_macro_expansion_at (EXPR_LOCATION (rhs));
  if (lhs_in_macro || rhs_in_macro)
    {
      /* Don't warn if exactly one is from a macro.  */
      if (!(lhs_in_macro && rhs_in_macro))
	return;

      /* If both are in a macro, only warn if they're spelled the same.  */
      if (!spelled_the_same_p (EXPR_LOCATION (lhs), EXPR_LOCATION (rhs)))
	return;
    }

  warn_tautological_bitwise_comparison (loc, code, lhs, rhs);

  /* We do not warn for constants because they are typical of macro
     expansions that test for features, sizeof, and similar.  */
  if (CONSTANT_CLASS_P (fold_for_warn (lhs))
      || CONSTANT_CLASS_P (fold_for_warn (rhs)))
    return;

  /* Don't warn for e.g.
     HOST_WIDE_INT n;
     ...
     if (n == (long) n) ...
   */
  if ((CONVERT_EXPR_P (lhs) || TREE_CODE (lhs) == NON_LVALUE_EXPR)
      || (CONVERT_EXPR_P (rhs) || TREE_CODE (rhs) == NON_LVALUE_EXPR))
    return;

  /* Don't warn if either LHS or RHS has an IEEE floating-point type.
     It could be a NaN, and NaN never compares equal to anything, even
     itself.  */
  if (FLOAT_TYPE_P (TREE_TYPE (lhs)) || FLOAT_TYPE_P (TREE_TYPE (rhs)))
    return;

  if (operand_equal_p (lhs, rhs, 0))
    {
      /* Don't warn about array references with constant indices;
	 these are likely to come from a macro.  */
      if (walk_tree_without_duplicates (&lhs, find_array_ref_with_const_idx_r,
					NULL))
	return;
      const bool always_true = (code == EQ_EXPR || code == LE_EXPR
				|| code == GE_EXPR || code == UNLE_EXPR
				|| code == UNGE_EXPR || code == UNEQ_EXPR);
      binary_op_rich_location richloc (loc, lhs, rhs, false);
      if (always_true)
	warning_at (&richloc, OPT_Wtautological_compare,
		    "self-comparison always evaluates to true");
      else
	warning_at (&richloc, OPT_Wtautological_compare,
		    "self-comparison always evaluates to false");
    }
}

/* Return true iff EXPR only contains boolean operands, or comparisons.  */

static bool
expr_has_boolean_operands_p (tree expr)
{
  STRIP_NOPS (expr);

  if (CONVERT_EXPR_P (expr))
    return bool_promoted_to_int_p (expr);
  else if (UNARY_CLASS_P (expr))
    return expr_has_boolean_operands_p (TREE_OPERAND (expr, 0));
  else if (BINARY_CLASS_P (expr))
    return (expr_has_boolean_operands_p (TREE_OPERAND (expr, 0))
	    && expr_has_boolean_operands_p (TREE_OPERAND (expr, 1)));
  else if (COMPARISON_CLASS_P (expr))
    return true;
  else
    return false;
}

/* Warn about logical not used on the left hand side operand of a comparison.
   This function assumes that the LHS is inside of TRUTH_NOT_EXPR.
   Do not warn if RHS is of a boolean type, a logical operator, or
   a comparison.  */

void
warn_logical_not_parentheses (location_t location, enum tree_code code,
			      tree lhs, tree rhs)
{
  if (TREE_CODE_CLASS (code) != tcc_comparison
      || TREE_TYPE (rhs) == NULL_TREE
      || TREE_CODE (TREE_TYPE (rhs)) == BOOLEAN_TYPE
      || truth_value_p (TREE_CODE (rhs)))
    return;

  /* Don't warn for expression like !x == ~(bool1 | bool2).  */
  if (expr_has_boolean_operands_p (rhs))
    return;

  /* Don't warn for !x == 0 or !y != 0, those are equivalent to
     !(x == 0) or !(y != 0).  */
  if ((code == EQ_EXPR || code == NE_EXPR)
      && integer_zerop (rhs))
    return;

  auto_diagnostic_group d;
  if (warning_at (location, OPT_Wlogical_not_parentheses,
		  "logical not is only applied to the left hand side of "
		  "comparison")
      && EXPR_HAS_LOCATION (lhs))
    {
      location_t lhs_loc = EXPR_LOCATION (lhs);
      rich_location richloc (line_table, lhs_loc);
      richloc.add_fixit_insert_before (lhs_loc, "(");
      richloc.add_fixit_insert_after (lhs_loc, ")");
      inform (&richloc, "add parentheses around left hand side "
	      "expression to silence this warning");
    }
}

/* Warn if EXP contains any computations whose results are not used.
   Return true if a warning is printed; false otherwise.  LOCUS is the
   (potential) location of the expression.  */

bool
warn_if_unused_value (const_tree exp, location_t locus)
{
 restart:
  if (TREE_USED (exp) || TREE_NO_WARNING (exp))
    return false;

  /* Don't warn about void constructs.  This includes casting to void,
     void function calls, and statement expressions with a final cast
     to void.  */
  if (VOID_TYPE_P (TREE_TYPE (exp)))
    return false;

  if (EXPR_HAS_LOCATION (exp))
    locus = EXPR_LOCATION (exp);

  switch (TREE_CODE (exp))
    {
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case MODIFY_EXPR:
    case INIT_EXPR:
    case TARGET_EXPR:
    case CALL_EXPR:
    case TRY_CATCH_EXPR:
    case EXIT_EXPR:
    case VA_ARG_EXPR:
      return false;

    case BIND_EXPR:
      /* For a binding, warn if no side effect within it.  */
      exp = BIND_EXPR_BODY (exp);
      goto restart;

    case SAVE_EXPR:
    case NON_LVALUE_EXPR:
    case NOP_EXPR:
      exp = TREE_OPERAND (exp, 0);
      goto restart;

    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
      /* In && or ||, warn if 2nd operand has no side effect.  */
      exp = TREE_OPERAND (exp, 1);
      goto restart;

    case COMPOUND_EXPR:
      if (warn_if_unused_value (TREE_OPERAND (exp, 0), locus))
	return true;
      /* Let people do `(foo (), 0)' without a warning.  */
      if (TREE_CONSTANT (TREE_OPERAND (exp, 1)))
	return false;
      exp = TREE_OPERAND (exp, 1);
      goto restart;

    case COND_EXPR:
      /* If this is an expression with side effects, don't warn; this
	 case commonly appears in macro expansions.  */
      if (TREE_SIDE_EFFECTS (exp))
	return false;
      goto warn;

    case INDIRECT_REF:
      /* Don't warn about automatic dereferencing of references, since
	 the user cannot control it.  */
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == REFERENCE_TYPE)
	{
	  exp = TREE_OPERAND (exp, 0);
	  goto restart;
	}
      /* Fall through.  */

    default:
      /* Referencing a volatile value is a side effect, so don't warn.  */
      if ((DECL_P (exp) || REFERENCE_CLASS_P (exp))
	  && TREE_THIS_VOLATILE (exp))
	return false;

      /* If this is an expression which has no operands, there is no value
	 to be unused.  There are no such language-independent codes,
	 but front ends may define such.  */
      if (EXPRESSION_CLASS_P (exp) && TREE_OPERAND_LENGTH (exp) == 0)
	return false;

    warn:
      return warning_at (locus, OPT_Wunused_value, "value computed is not used");
    }
}

/* Print a warning about casts that might indicate violation of strict
   aliasing rules if -Wstrict-aliasing is used and strict aliasing
   mode is in effect.  LOC is the location of the expression being
   cast, EXPR might be from inside it.  TYPE is the type we're casting
   to.  */

bool
strict_aliasing_warning (location_t loc, tree type, tree expr)
{
  if (loc == UNKNOWN_LOCATION)
    loc = input_location;

  /* Strip pointer conversion chains and get to the correct original type.  */
  STRIP_NOPS (expr);
  tree otype = TREE_TYPE (expr);

  if (!(flag_strict_aliasing
	&& POINTER_TYPE_P (type)
	&& POINTER_TYPE_P (otype)
	&& !VOID_TYPE_P (TREE_TYPE (type)))
      /* If the type we are casting to is a ref-all pointer
	 dereferencing it is always valid.  */
      || TYPE_REF_CAN_ALIAS_ALL (type))
    return false;

  if ((warn_strict_aliasing > 1) && TREE_CODE (expr) == ADDR_EXPR
      && (DECL_P (TREE_OPERAND (expr, 0))
	  || handled_component_p (TREE_OPERAND (expr, 0))))
    {
      /* Casting the address of an object to non void pointer. Warn
	 if the cast breaks type based aliasing.  */
      if (!COMPLETE_TYPE_P (TREE_TYPE (type)) && warn_strict_aliasing == 2)
	{
	  warning_at (loc, OPT_Wstrict_aliasing,
		      "type-punning to incomplete type "
		      "might break strict-aliasing rules");
	  return true;
	}
      else
	{
	  /* warn_strict_aliasing >= 3.   This includes the default (3).
	     Only warn if the cast is dereferenced immediately.  */
	  alias_set_type set1
	    = get_alias_set (TREE_TYPE (TREE_OPERAND (expr, 0)));
	  alias_set_type set2 = get_alias_set (TREE_TYPE (type));

	  if (set2 != 0
	      && set1 != set2
	      && !alias_set_subset_of (set2, set1)
	      && !alias_sets_conflict_p (set1, set2))
	    {
	      warning_at (loc, OPT_Wstrict_aliasing,
			  "dereferencing type-punned "
			  "pointer will break strict-aliasing rules");
	      return true;
	    }
	  else if (warn_strict_aliasing == 2
		   && !alias_sets_must_conflict_p (set1, set2))
	    {
	      warning_at (loc, OPT_Wstrict_aliasing,
			  "dereferencing type-punned "
			  "pointer might break strict-aliasing rules");
	      return true;
	    }
	}
    }
  else if ((warn_strict_aliasing == 1) && !VOID_TYPE_P (TREE_TYPE (otype)))
    {
      /* At this level, warn for any conversions, even if an address is
	 not taken in the same statement.  This will likely produce many
	 false positives, but could be useful to pinpoint problems that
	 are not revealed at higher levels.  */
      alias_set_type set1 = get_alias_set (TREE_TYPE (otype));
      alias_set_type set2 = get_alias_set (TREE_TYPE (type));
      if (!COMPLETE_TYPE_P (TREE_TYPE (type))
	  || !alias_sets_must_conflict_p (set1, set2))
	{
	  warning_at (loc, OPT_Wstrict_aliasing,
		      "dereferencing type-punned "
		      "pointer might break strict-aliasing rules");
	  return true;
	}
    }

  return false;
}

/* Warn about memset (&a, 0, sizeof (&a)); and similar mistakes with
   sizeof as last operand of certain builtins.  */

void
sizeof_pointer_memaccess_warning (location_t *sizeof_arg_loc, tree callee,
				  vec<tree, va_gc> *params, tree *sizeof_arg,
				  bool (*comp_types) (tree, tree))
{
  tree type, dest = NULL_TREE, src = NULL_TREE, tem;
  bool strop = false, cmp = false;
  unsigned int idx = ~0;
  location_t loc;

  if (TREE_CODE (callee) != FUNCTION_DECL
      || !fndecl_built_in_p (callee, BUILT_IN_NORMAL)
      || vec_safe_length (params) <= 1)
    return;

  enum built_in_function fncode = DECL_FUNCTION_CODE (callee);
  switch (fncode)
    {
    case BUILT_IN_STRNCMP:
    case BUILT_IN_STRNCASECMP:
      cmp = true;
      /* FALLTHRU */
    case BUILT_IN_STRNCPY:
    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STRNCAT:
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_STPNCPY:
    case BUILT_IN_STPNCPY_CHK:
      strop = true;
      /* FALLTHRU */
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMMOVE:
    case BUILT_IN_MEMMOVE_CHK:
      if (params->length () < 3)
	return;
      src = (*params)[1];
      dest = (*params)[0];
      idx = 2;
      break;
    case BUILT_IN_BCOPY:
      if (params->length () < 3)
	return;
      src = (*params)[0];
      dest = (*params)[1];
      idx = 2;
      break;
    case BUILT_IN_MEMCMP:
    case BUILT_IN_BCMP:
      if (params->length () < 3)
	return;
      src = (*params)[1];
      dest = (*params)[0];
      idx = 2;
      cmp = true;
      break;
    case BUILT_IN_MEMSET:
    case BUILT_IN_MEMSET_CHK:
      if (params->length () < 3)
	return;
      dest = (*params)[0];
      idx = 2;
      break;
    case BUILT_IN_BZERO:
      dest = (*params)[0];
      idx = 1;
      break;
    case BUILT_IN_STRNDUP:
      src = (*params)[0];
      strop = true;
      idx = 1;
      break;
    case BUILT_IN_MEMCHR:
      if (params->length () < 3)
	return;
      src = (*params)[0];
      idx = 2;
      break;
    case BUILT_IN_SNPRINTF:
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF:
    case BUILT_IN_VSNPRINTF_CHK:
      dest = (*params)[0];
      idx = 1;
      strop = true;
      break;
    default:
      break;
    }

  if (idx >= 3)
    return;

  /* Use error_operand_p to detect non-error arguments with an error
     type that the C++ front-end constructs.  */
  if (error_operand_p (src)
      || error_operand_p (dest)
      || !sizeof_arg[idx]
      || error_operand_p (sizeof_arg[idx]))
    return;

  type = TYPE_P (sizeof_arg[idx])
	 ? sizeof_arg[idx] : TREE_TYPE (sizeof_arg[idx]);

  if (!POINTER_TYPE_P (type))
    {
      /* The argument type may be an array.  Diagnose bounded string
	 copy functions that specify the bound in terms of the source
	 argument rather than the destination unless they are equal
	 to one another.  Handle constant sizes and also try to handle
	 sizeof expressions involving VLAs.  */
      if (strop && !cmp && fncode != BUILT_IN_STRNDUP && src)
	{
	  tem = tree_strip_nop_conversions (src);
	  if (TREE_CODE (tem) == ADDR_EXPR)
	    tem = TREE_OPERAND (tem, 0);

	  /* Avoid diagnosing sizeof SRC when SRC is declared with
	     attribute nonstring.  */
	  tree dummy;
	  if (get_attr_nonstring_decl (tem, &dummy))
	    return;

	  tree d = tree_strip_nop_conversions (dest);
	  if (TREE_CODE (d) == ADDR_EXPR)
	    d = TREE_OPERAND (d, 0);

	  tree dstsz = TYPE_SIZE_UNIT (TREE_TYPE (d));
	  tree srcsz = TYPE_SIZE_UNIT (TREE_TYPE (tem));

	  if ((!dstsz
	       || !srcsz
	       || !operand_equal_p (dstsz, srcsz, OEP_LEXICOGRAPHIC))
	      && operand_equal_p (tem, sizeof_arg[idx], OEP_ADDRESS_OF))
	    warning_at (sizeof_arg_loc[idx], OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the source; did you mean to use "
			"the size of the destination?",
			callee);
	}

      return;
    }

  if (dest
      && (tem = tree_strip_nop_conversions (dest))
      && POINTER_TYPE_P (TREE_TYPE (tem))
      && comp_types (TREE_TYPE (TREE_TYPE (tem)), type))
    return;

  if (src
      && (tem = tree_strip_nop_conversions (src))
      && POINTER_TYPE_P (TREE_TYPE (tem))
      && comp_types (TREE_TYPE (TREE_TYPE (tem)), type))
    return;

  loc = sizeof_arg_loc[idx];

  if (dest && !cmp)
    {
      if (!TYPE_P (sizeof_arg[idx])
	  && operand_equal_p (dest, sizeof_arg[idx], 0)
	  && comp_types (TREE_TYPE (dest), type))
	{
	  if (TREE_CODE (sizeof_arg[idx]) == ADDR_EXPR && !strop)
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the destination; did you mean to "
			"remove the addressof?", callee);
	  else if ((TYPE_PRECISION (TREE_TYPE (type))
		    == TYPE_PRECISION (char_type_node))
		   || strop)
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the destination; did you mean to "
			"provide an explicit length?", callee);
	  else
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the destination; did you mean to "
			"dereference it?", callee);
	  return;
	}

      if (POINTER_TYPE_P (TREE_TYPE (dest))
	  && !strop
	  && comp_types (TREE_TYPE (dest), type)
	  && !VOID_TYPE_P (TREE_TYPE (type)))
	{
	  warning_at (loc, OPT_Wsizeof_pointer_memaccess,
		      "argument to %<sizeof%> in %qD call is the same "
		      "pointer type %qT as the destination; expected %qT "
		      "or an explicit length", callee, TREE_TYPE (dest),
		      TREE_TYPE (TREE_TYPE (dest)));
	  return;
	}
    }

  if (src && !cmp)
    {
      if (!TYPE_P (sizeof_arg[idx])
	  && operand_equal_p (src, sizeof_arg[idx], 0)
	  && comp_types (TREE_TYPE (src), type))
	{
	  if (TREE_CODE (sizeof_arg[idx]) == ADDR_EXPR && !strop)
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the source; did you mean to "
			"remove the addressof?", callee);
	  else if ((TYPE_PRECISION (TREE_TYPE (type))
		    == TYPE_PRECISION (char_type_node))
		   || strop)
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the source; did you mean to "
			"provide an explicit length?", callee);
	  else
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the source; did you mean to "
			"dereference it?", callee);
	  return;
	}

      if (POINTER_TYPE_P (TREE_TYPE (src))
	  && !strop
	  && comp_types (TREE_TYPE (src), type)
	  && !VOID_TYPE_P (TREE_TYPE (type)))
	{
	  warning_at (loc, OPT_Wsizeof_pointer_memaccess,
		      "argument to %<sizeof%> in %qD call is the same "
		      "pointer type %qT as the source; expected %qT "
		      "or an explicit length", callee, TREE_TYPE (src),
		      TREE_TYPE (TREE_TYPE (src)));
	  return;
	}
    }

  if (dest)
    {
      if (!TYPE_P (sizeof_arg[idx])
	  && operand_equal_p (dest, sizeof_arg[idx], 0)
	  && comp_types (TREE_TYPE (dest), type))
	{
	  if (TREE_CODE (sizeof_arg[idx]) == ADDR_EXPR && !strop)
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the first source; did you mean to "
			"remove the addressof?", callee);
	  else if ((TYPE_PRECISION (TREE_TYPE (type))
		    == TYPE_PRECISION (char_type_node))
		   || strop)
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the first source; did you mean to "
			"provide an explicit length?", callee);
	  else
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the first source; did you mean to "
			"dereference it?", callee);
	  return;
	}

      if (POINTER_TYPE_P (TREE_TYPE (dest))
	  && !strop
	  && comp_types (TREE_TYPE (dest), type)
	  && !VOID_TYPE_P (TREE_TYPE (type)))
	{
	  warning_at (loc, OPT_Wsizeof_pointer_memaccess,
		      "argument to %<sizeof%> in %qD call is the same "
		      "pointer type %qT as the first source; expected %qT "
		      "or an explicit length", callee, TREE_TYPE (dest),
		      TREE_TYPE (TREE_TYPE (dest)));
	  return;
	}
    }

  if (src)
    {
      if (!TYPE_P (sizeof_arg[idx])
	  && operand_equal_p (src, sizeof_arg[idx], 0)
	  && comp_types (TREE_TYPE (src), type))
	{
	  if (TREE_CODE (sizeof_arg[idx]) == ADDR_EXPR && !strop)
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the second source; did you mean to "
			"remove the addressof?", callee);
	  else if ((TYPE_PRECISION (TREE_TYPE (type))
		    == TYPE_PRECISION (char_type_node))
		   || strop)
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the second source; did you mean to "
			"provide an explicit length?", callee);
	  else
	    warning_at (loc, OPT_Wsizeof_pointer_memaccess,
			"argument to %<sizeof%> in %qD call is the same "
			"expression as the second source; did you mean to "
			"dereference it?", callee);
	  return;
	}

      if (POINTER_TYPE_P (TREE_TYPE (src))
	  && !strop
	  && comp_types (TREE_TYPE (src), type)
	  && !VOID_TYPE_P (TREE_TYPE (type)))
	{
	  warning_at (loc, OPT_Wsizeof_pointer_memaccess,
		      "argument to %<sizeof%> in %qD call is the same "
		      "pointer type %qT as the second source; expected %qT "
		      "or an explicit length", callee, TREE_TYPE (src),
		      TREE_TYPE (TREE_TYPE (src)));
	  return;
	}
    }

}

/* Warn for unlikely, improbable, or stupid DECL declarations
   of `main'.  */

void
check_main_parameter_types (tree decl)
{
  function_args_iterator iter;
  tree type;
  int argct = 0;

  FOREACH_FUNCTION_ARGS (TREE_TYPE (decl), type, iter)
    {
      /* XXX void_type_node belies the abstraction.  */
      if (type == void_type_node || type == error_mark_node)
	break;

      tree t = type;
      if (TYPE_ATOMIC (t))
	  pedwarn (input_location, OPT_Wmain,
		   "%<_Atomic%>-qualified parameter type %qT of %q+D",
		   type, decl);
      while (POINTER_TYPE_P (t))
	{
	  t = TREE_TYPE (t);
	  if (TYPE_ATOMIC (t))
	    pedwarn (input_location, OPT_Wmain,
		     "%<_Atomic%>-qualified parameter type %qT of %q+D",
		     type, decl);
	}

      ++argct;
      switch (argct)
	{
	case 1:
	  if (TYPE_MAIN_VARIANT (type) != integer_type_node)
	    pedwarn (input_location, OPT_Wmain,
		     "first argument of %q+D should be %<int%>", decl);
	  break;

	case 2:
	  if (TREE_CODE (type) != POINTER_TYPE
	      || TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
	      || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
		  != char_type_node))
	    pedwarn (input_location, OPT_Wmain,
		     "second argument of %q+D should be %<char **%>", decl);
	  break;

	case 3:
	  if (TREE_CODE (type) != POINTER_TYPE
	      || TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
	      || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
		  != char_type_node))
	    pedwarn (input_location, OPT_Wmain,
		     "third argument of %q+D should probably be "
		     "%<char **%>", decl);
	  break;
	}
    }

  /* It is intentional that this message does not mention the third
    argument because it's only mentioned in an appendix of the
    standard.  */
  if (argct > 0 && (argct < 2 || argct > 3))
    pedwarn (input_location, OPT_Wmain,
	     "%q+D takes only zero or two arguments", decl);

  if (stdarg_p (TREE_TYPE (decl)))
    pedwarn (input_location, OPT_Wmain,
	     "%q+D declared as variadic function", decl);
}

/* Warns and returns true if the conversion of EXPR to TYPE may alter a value.
   This is a helper function for warnings_for_convert_and_check.  */

static bool
conversion_warning (location_t loc, tree type, tree expr, tree result)
{
  tree expr_type = TREE_TYPE (expr);
  enum conversion_safety conversion_kind;
  int arith_ops = 0;

  if (!warn_conversion && !warn_sign_conversion && !warn_float_conversion)
    return false;

  /* This may happen, because for LHS op= RHS we preevaluate
     RHS and create C_MAYBE_CONST_EXPR <SAVE_EXPR <RHS>>, which
     means we could no longer see the code of the EXPR.  */
  if (TREE_CODE (expr) == C_MAYBE_CONST_EXPR)
    expr = C_MAYBE_CONST_EXPR_EXPR (expr);
  if (TREE_CODE (expr) == SAVE_EXPR)
    expr = TREE_OPERAND (expr, 0);

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:
    case NE_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
      /* Conversion from boolean to a signed:1 bit-field (which only
	 can hold the values 0 and -1) doesn't lose information - but
	 it does change the value.  */
      if (TYPE_PRECISION (type) == 1 && !TYPE_UNSIGNED (type))
	warning_at (loc, OPT_Wconversion,
		    "conversion to %qT from boolean expression", type);
      return true;

    case REAL_CST:
    case INTEGER_CST:
    case COMPLEX_CST:
      {
	conversion_kind = unsafe_conversion_p (type, expr, result, true);
	int warnopt;
	if (conversion_kind == UNSAFE_REAL)
	  warnopt = OPT_Wfloat_conversion;
	else if (conversion_kind)
	  warnopt = OPT_Wconversion;
	else
	  break;

	if (conversion_kind == UNSAFE_SIGN)
	  {
	    bool cstresult
	      = (result
		 && TREE_CODE_CLASS (TREE_CODE (result)) == tcc_constant);
	    if (TYPE_UNSIGNED (type))
	      {
		if (cstresult)
		  warning_at (loc, OPT_Wsign_conversion,
			      "unsigned conversion from %qT to %qT "
			      "changes value from %qE to %qE",
			      expr_type, type, expr, result);
		else
		  warning_at (loc, OPT_Wsign_conversion,
			      "unsigned conversion from %qT to %qT "
			      "changes the value of %qE",
			      expr_type, type, expr);
	      }
	    else
	      {
		if (cstresult)
		  warning_at (loc, OPT_Wsign_conversion,
			      "signed conversion from %qT to %qT changes "
			      "value from %qE to %qE",
			      expr_type, type, expr, result);
		else
		  warning_at (loc, OPT_Wsign_conversion,
			      "signed conversion from %qT to %qT changes "
			      "the value of %qE",
			      expr_type, type, expr);
	      }
	  }
	else if (TREE_CODE_CLASS (TREE_CODE (result)) == tcc_constant)
	  warning_at (loc, warnopt,
		      "conversion from %qT to %qT changes value from %qE to %qE",
		      expr_type, type, expr, result);
	else
	  warning_at (loc, warnopt,
		      "conversion from %qT to %qT changes the value of %qE",
		      expr_type, type, expr);
	return true;
      }

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case RDIV_EXPR:
      arith_ops = 2;
      goto default_;

    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case FIX_TRUNC_EXPR:
    case NON_LVALUE_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
      arith_ops = 1;
      goto default_;

    case COND_EXPR:
      {
	/* In case of COND_EXPR, we do not care about the type of
	   COND_EXPR, only about the conversion of each operand.  */
	tree op1 = TREE_OPERAND (expr, 1);
	tree op2 = TREE_OPERAND (expr, 2);

	return (conversion_warning (loc, type, op1, result)
		|| conversion_warning (loc, type, op2, result));
      }

    default_:
    default:
      conversion_kind = unsafe_conversion_p (type, expr, result, true);
      {
	int warnopt;
	if (conversion_kind == UNSAFE_REAL)
	  warnopt = OPT_Wfloat_conversion;
	else if (conversion_kind == UNSAFE_SIGN)
	  warnopt = OPT_Wsign_conversion;
	else if (conversion_kind)
	  warnopt = OPT_Wconversion;
	else
	  break;

	if (arith_ops
	    && global_dc->option_enabled (warnopt,
					  global_dc->lang_mask,
					  global_dc->option_state))
	  {
	    for (int i = 0; i < arith_ops; ++i)
	      {
		tree op = TREE_OPERAND (expr, i);
		/* Avoid -Wsign-conversion for (unsigned)(x + (-1)).  */
		if (TREE_CODE (expr) == PLUS_EXPR && i == 1
		    && INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
		    && TREE_CODE (op) == INTEGER_CST
		    && tree_int_cst_sgn (op) < 0)
		  op = fold_build1 (NEGATE_EXPR, TREE_TYPE (op), op);
		tree opr = convert (type, op);
		if (unsafe_conversion_p (type, op, opr, true))
		  goto op_unsafe;
	      }
	    /* The operands seem safe, we might still want to warn if
	       -Warith-conversion.  */
	    warnopt = OPT_Warith_conversion;
	  op_unsafe:;
	  }

	if (conversion_kind == UNSAFE_SIGN)
	  warning_at (loc, warnopt, "conversion to %qT from %qT "
		      "may change the sign of the result",
		      type, expr_type);
	else if (conversion_kind == UNSAFE_IMAGINARY)
	  warning_at (loc, warnopt,
		      "conversion from %qT to %qT discards imaginary component",
		      expr_type, type);
	else
	  warning_at (loc, warnopt,
		      "conversion from %qT to %qT may change value",
		      expr_type, type);
	return true;
      }
    }
  return false;
}

/* Produce warnings after a conversion. RESULT is the result of
   converting EXPR to TYPE.  This is a helper function for
   convert_and_check and cp_convert_and_check.  */

void
warnings_for_convert_and_check (location_t loc, tree type, tree expr,
				tree result)
{
  loc = expansion_point_location_if_in_system_header (loc);

  while (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 1);
  while (TREE_CODE (result) == COMPOUND_EXPR)
    result = TREE_OPERAND (result, 1);

  bool cst = TREE_CODE_CLASS (TREE_CODE (result)) == tcc_constant;

  tree exprtype = TREE_TYPE (expr);

  if (TREE_CODE (expr) == INTEGER_CST
      && (TREE_CODE (type) == INTEGER_TYPE
	  || TREE_CODE (type) == ENUMERAL_TYPE)
      && !int_fits_type_p (expr, type))
    {
      /* Do not diagnose overflow in a constant expression merely
	 because a conversion overflowed.  */
      if (TREE_OVERFLOW (result))
	TREE_OVERFLOW (result) = TREE_OVERFLOW (expr);

      if (TYPE_UNSIGNED (type))
	{
	  /* This detects cases like converting -129 or 256 to
	     unsigned char.  */
	  if (!int_fits_type_p (expr, c_common_signed_type (type)))
	    {
	      if (cst)
		warning_at (loc, OPT_Woverflow,
			    (TYPE_UNSIGNED (exprtype)
			     ? G_("conversion from %qT to %qT "
				  "changes value from %qE to %qE")
			     : G_("unsigned conversion from %qT to %qT "
				  "changes value from %qE to %qE")),
			    exprtype, type, expr, result);
	      else
		warning_at (loc, OPT_Woverflow,
			    (TYPE_UNSIGNED (exprtype)
			     ? G_("conversion from %qT to %qT "
				  "changes the value of %qE")
			     : G_("unsigned conversion from %qT to %qT "
				  "changes the value of %qE")),
			    exprtype, type, expr);
	    }
	  else
	    conversion_warning (loc, type, expr, result);
	}
      else if (!int_fits_type_p (expr, c_common_unsigned_type (type)))
	{
	  if (cst)
	    warning_at (loc, OPT_Woverflow,
			"overflow in conversion from %qT to %qT "
			"changes value from %qE to %qE",
			exprtype, type, expr, result);
	  else
	    warning_at (loc, OPT_Woverflow,
			"overflow in conversion from %qT to %qT "
			"changes the value of %qE",
			exprtype, type, expr);
	}
      /* No warning for converting 0x80000000 to int.  */
      else if (pedantic
	       && (TREE_CODE (exprtype) != INTEGER_TYPE
		   || TYPE_PRECISION (exprtype)
		   != TYPE_PRECISION (type)))
	{
	  if (cst)
	    warning_at (loc, OPT_Woverflow,
			"overflow in conversion from %qT to %qT "
			"changes value from %qE to %qE",
			exprtype, type, expr, result);
	  else
	    warning_at (loc, OPT_Woverflow,
			"overflow in conversion from %qT to %qT "
			"changes the value of %qE",
			exprtype, type, expr);
	}
      else
	conversion_warning (loc, type, expr, result);
    }
  else if ((TREE_CODE (result) == INTEGER_CST
	    || TREE_CODE (result) == FIXED_CST) && TREE_OVERFLOW (result))
    {
      if (cst)
	warning_at (loc, OPT_Woverflow,
		    "overflow in conversion from %qT to %qT "
		    "changes value from %qE to %qE",
		    exprtype, type, expr, result);
      else
	warning_at (loc, OPT_Woverflow,
		    "overflow in conversion from %qT to %qT "
		    "changes the value of %qE",
		    exprtype, type, expr);
    }
  else
    conversion_warning (loc, type, expr, result);
}

/* Subroutines of c_do_switch_warnings, called via splay_tree_foreach.
   Used to verify that case values match up with enumerator values.  */

static void
match_case_to_enum_1 (tree key, tree type, tree label)
{
  /* Avoid warning about enums that have no enumerators.  */
  if (TYPE_VALUES (type) == NULL_TREE)
    return;

  char buf[WIDE_INT_PRINT_BUFFER_SIZE];

  if (tree_fits_uhwi_p (key))
    print_dec (wi::to_wide (key), buf, UNSIGNED);
  else if (tree_fits_shwi_p (key))
    print_dec (wi::to_wide (key), buf, SIGNED);
  else
    print_hex (wi::to_wide (key), buf);

  if (TYPE_NAME (type) == NULL_TREE)
    warning_at (DECL_SOURCE_LOCATION (CASE_LABEL (label)),
		warn_switch ? OPT_Wswitch : OPT_Wswitch_enum,
		"case value %qs not in enumerated type",
		buf);
  else
    warning_at (DECL_SOURCE_LOCATION (CASE_LABEL (label)),
		warn_switch ? OPT_Wswitch : OPT_Wswitch_enum,
		"case value %qs not in enumerated type %qT",
		buf, type);
}

/* Subroutine of c_do_switch_warnings, called via splay_tree_foreach.
   Used to verify that case values match up with enumerator values.  */

static int
match_case_to_enum (splay_tree_node node, void *data)
{
  tree label = (tree) node->value;
  tree type = (tree) data;

  /* Skip default case.  */
  if (!CASE_LOW (label))
    return 0;

  /* If CASE_LOW_SEEN is not set, that means CASE_LOW did not appear
     when we did our enum->case scan.  Reset our scratch bit after.  */
  if (!CASE_LOW_SEEN (label))
    match_case_to_enum_1 (CASE_LOW (label), type, label);
  else
    CASE_LOW_SEEN (label) = 0;

  /* If CASE_HIGH is non-null, we have a range.  If CASE_HIGH_SEEN is
     not set, that means that CASE_HIGH did not appear when we did our
     enum->case scan.  Reset our scratch bit after.  */
  if (CASE_HIGH (label))
    {
      if (!CASE_HIGH_SEEN (label))
	match_case_to_enum_1 (CASE_HIGH (label), type, label);
      else
	CASE_HIGH_SEEN (label) = 0;
    }

  return 0;
}

/* Handle -Wswitch*.  Called from the front end after parsing the
   switch construct.  */
/* ??? Should probably be somewhere generic, since other languages
   besides C and C++ would want this.  At the moment, however, C/C++
   are the only tree-ssa languages that support enumerations at all,
   so the point is moot.  */

void
c_do_switch_warnings (splay_tree cases, location_t switch_location,
		      tree type, tree cond, bool bool_cond_p)
{
  splay_tree_node default_node;
  splay_tree_node node;
  tree chain;
  bool outside_range_p = false;

  if (type != error_mark_node
      && type != TREE_TYPE (cond)
      && INTEGRAL_TYPE_P (type)
      && INTEGRAL_TYPE_P (TREE_TYPE (cond))
      && (!tree_int_cst_equal (TYPE_MIN_VALUE (type),
			       TYPE_MIN_VALUE (TREE_TYPE (cond)))
	  || !tree_int_cst_equal (TYPE_MAX_VALUE (type),
				  TYPE_MAX_VALUE (TREE_TYPE (cond)))))
    {
      tree min_value = TYPE_MIN_VALUE (type);
      tree max_value = TYPE_MAX_VALUE (type);

      node = splay_tree_predecessor (cases, (splay_tree_key) min_value);
      if (node && node->key)
	{
	  outside_range_p = true;
	  /* There is at least one case smaller than TYPE's minimum value.
	     NODE itself could be still a range overlapping the valid values,
	     but any predecessors thereof except the default case will be
	     completely outside of range.  */
	  if (CASE_HIGH ((tree) node->value)
	      && tree_int_cst_compare (CASE_HIGH ((tree) node->value),
				       min_value) >= 0)
	    {
	      location_t loc = EXPR_LOCATION ((tree) node->value);
	      warning_at (loc, OPT_Wswitch_outside_range,
			  "lower value in case label range less than minimum"
			  " value for type");
	      CASE_LOW ((tree) node->value) = convert (TREE_TYPE (cond),
						       min_value);
	      node->key = (splay_tree_key) CASE_LOW ((tree) node->value);
	    }
	  /* All the following ones are completely outside of range.  */
	  do
	    {
	      node = splay_tree_predecessor (cases,
					     (splay_tree_key) min_value);
	      if (node == NULL || !node->key)
		break;
	      location_t loc = EXPR_LOCATION ((tree) node->value);
	      warning_at (loc, OPT_Wswitch_outside_range, "case label value is"
			  " less than minimum value for type");
	      splay_tree_remove (cases, node->key);
	    }
	  while (1);
	}
      node = splay_tree_lookup (cases, (splay_tree_key) max_value);
      if (node == NULL)
	node = splay_tree_predecessor (cases, (splay_tree_key) max_value);
      /* Handle a single node that might partially overlap the range.  */
      if (node
	  && node->key
	  && CASE_HIGH ((tree) node->value)
	  && tree_int_cst_compare (CASE_HIGH ((tree) node->value),
				   max_value) > 0)
	{
	  location_t loc = EXPR_LOCATION ((tree) node->value);
	  warning_at (loc, OPT_Wswitch_outside_range, "upper value in case"
		      " label range exceeds maximum value for type");
	  CASE_HIGH ((tree) node->value)
	    = convert (TREE_TYPE (cond), max_value);
	  outside_range_p = true;
	}
      /* And any nodes that are completely outside of the range.  */
      while ((node = splay_tree_successor (cases,
					   (splay_tree_key) max_value))
	     != NULL)
	{
	  location_t loc = EXPR_LOCATION ((tree) node->value);
	  warning_at (loc, OPT_Wswitch_outside_range,
		      "case label value exceeds maximum value for type");
	  splay_tree_remove (cases, node->key);
	  outside_range_p = true;
	}
    }

  if (!warn_switch && !warn_switch_enum && !warn_switch_default
      && !warn_switch_bool)
    return;

  default_node = splay_tree_lookup (cases, (splay_tree_key) NULL);
  if (!default_node)
    warning_at (switch_location, OPT_Wswitch_default,
		"switch missing default case");

  /* There are certain cases where -Wswitch-bool warnings aren't
     desirable, such as
     switch (boolean)
       {
       case true: ...
       case false: ...
       }
     so be careful here.  */
  if (warn_switch_bool && bool_cond_p)
    {
      splay_tree_node min_node;
      /* If there's a default node, it's also the value with the minimal
	 key.  So look at the penultimate key (if any).  */
      if (default_node)
	min_node = splay_tree_successor (cases, (splay_tree_key) NULL);
      else
	min_node = splay_tree_min (cases);
      tree min = min_node ? (tree) min_node->key : NULL_TREE;

      splay_tree_node max_node = splay_tree_max (cases);
      /* This might be a case range, so look at the value with the
	 maximal key and then check CASE_HIGH.  */
      tree max = max_node ? (tree) max_node->value : NULL_TREE;
      if (max)
	max = CASE_HIGH (max) ? CASE_HIGH (max) : CASE_LOW (max);

      /* If there's a case value > 1 or < 0, that is outside bool
	 range, warn.  */
      if (outside_range_p
	  || (max && wi::gts_p (wi::to_wide (max), 1))
	  || (min && wi::lts_p (wi::to_wide (min), 0))
	  /* And handle the
	     switch (boolean)
	       {
	       case true: ...
	       case false: ...
	       default: ...
	       }
	     case, where we want to warn.  */
	  || (default_node
	      && max && wi::to_wide (max) == 1
	      && min && wi::to_wide (min) == 0))
	warning_at (switch_location, OPT_Wswitch_bool,
		    "switch condition has boolean value");
    }

  /* From here on, we only care about enumerated types.  */
  if (!type || TREE_CODE (type) != ENUMERAL_TYPE)
    return;

  /* From here on, we only care about -Wswitch and -Wswitch-enum.  */
  if (!warn_switch_enum && !warn_switch)
    return;

  /* Check the cases.  Warn about case values which are not members of
     the enumerated type.  For -Wswitch-enum, or for -Wswitch when
     there is no default case, check that exactly all enumeration
     literals are covered by the cases.  */

  /* Clearing COND if it is not an integer constant simplifies
     the tests inside the loop below.  */
  if (TREE_CODE (cond) != INTEGER_CST)
    cond = NULL_TREE;

  /* The time complexity here is O(N*lg(N)) worst case, but for the
      common case of monotonically increasing enumerators, it is
      O(N), since the nature of the splay tree will keep the next
      element adjacent to the root at all times.  */

  for (chain = TYPE_VALUES (type); chain; chain = TREE_CHAIN (chain))
    {
      tree value = TREE_VALUE (chain);
      if (TREE_CODE (value) == CONST_DECL)
	value = DECL_INITIAL (value);
      node = splay_tree_lookup (cases, (splay_tree_key) value);
      if (node)
	{
	  /* Mark the CASE_LOW part of the case entry as seen.  */
	  tree label = (tree) node->value;
	  CASE_LOW_SEEN (label) = 1;
	  continue;
	}

      /* Even though there wasn't an exact match, there might be a
	 case range which includes the enumerator's value.  */
      node = splay_tree_predecessor (cases, (splay_tree_key) value);
      if (node && CASE_HIGH ((tree) node->value))
	{
	  tree label = (tree) node->value;
	  int cmp = tree_int_cst_compare (CASE_HIGH (label), value);
	  if (cmp >= 0)
	    {
	      /* If we match the upper bound exactly, mark the CASE_HIGH
		 part of the case entry as seen.  */
	      if (cmp == 0)
		CASE_HIGH_SEEN (label) = 1;
	      continue;
	    }
	}

      /* We've now determined that this enumerated literal isn't
	 handled by the case labels of the switch statement.  */

      /* If the switch expression is a constant, we only really care
	 about whether that constant is handled by the switch.  */
      if (cond && tree_int_cst_compare (cond, value))
	continue;

      /* If the enumerator is defined in a system header and uses a reserved
	 name, then we continue to avoid throwing a warning.  */
      location_t loc = DECL_SOURCE_LOCATION
	    (TYPE_STUB_DECL (TYPE_MAIN_VARIANT (type)));
      if (in_system_header_at (loc)
	  && name_reserved_for_implementation_p
	      (IDENTIFIER_POINTER (TREE_PURPOSE (chain))))
	continue;

      /* If there is a default_node, the only relevant option is
	 Wswitch-enum.  Otherwise, if both are enabled then we prefer
	 to warn using -Wswitch because -Wswitch is enabled by -Wall
	 while -Wswitch-enum is explicit.  */
      warning_at (switch_location,
		  (default_node || !warn_switch
		   ? OPT_Wswitch_enum
		   : OPT_Wswitch),
		  "enumeration value %qE not handled in switch",
		  TREE_PURPOSE (chain));
    }

  /* Warn if there are case expressions that don't correspond to
     enumerators.  This can occur since C and C++ don't enforce
     type-checking of assignments to enumeration variables.

     The time complexity here is now always O(N) worst case, since
     we should have marked both the lower bound and upper bound of
     every disjoint case label, with CASE_LOW_SEEN and CASE_HIGH_SEEN
     above.  This scan also resets those fields.  */

  splay_tree_foreach (cases, match_case_to_enum, type);
}

/* Warn for A ?: C expressions (with B omitted) where A is a boolean
   expression, because B will always be true. */

void
warn_for_omitted_condop (location_t location, tree cond)
{
  /* In C++ template declarations it can happen that the type is dependent
     and not yet known, thus TREE_TYPE (cond) == NULL_TREE.  */
  if (truth_value_p (TREE_CODE (cond))
      || (TREE_TYPE (cond) != NULL_TREE
	  && TREE_CODE (TREE_TYPE (cond)) == BOOLEAN_TYPE))
      warning_at (location, OPT_Wparentheses,
		"the omitted middle operand in %<?:%> will always be %<true%>, "
		"suggest explicit middle operand");
}

/* Give an error for storing into ARG, which is 'const'.  USE indicates
   how ARG was being used.  */

void
readonly_error (location_t loc, tree arg, enum lvalue_use use)
{
  gcc_assert (use == lv_assign || use == lv_increment || use == lv_decrement
	      || use == lv_asm);
  STRIP_ANY_LOCATION_WRAPPER (arg);
  /* Using this macro rather than (for example) arrays of messages
     ensures that all the format strings are checked at compile
     time.  */
#define READONLY_MSG(A, I, D, AS) (use == lv_assign ? (A)		\
				   : (use == lv_increment ? (I)		\
				   : (use == lv_decrement ? (D) : (AS))))
  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TYPE_READONLY (TREE_TYPE (TREE_OPERAND (arg, 0))))
	error_at (loc, READONLY_MSG (G_("assignment of member "
					"%qD in read-only object"),
				     G_("increment of member "
					"%qD in read-only object"),
				     G_("decrement of member "
					"%qD in read-only object"),
				     G_("member %qD in read-only object "
					"used as %<asm%> output")),
		  TREE_OPERAND (arg, 1));
      else
	error_at (loc, READONLY_MSG (G_("assignment of read-only member %qD"),
				     G_("increment of read-only member %qD"),
				     G_("decrement of read-only member %qD"),
				     G_("read-only member %qD used as %<asm%> output")),
		  TREE_OPERAND (arg, 1));
    }
  else if (VAR_P (arg))
    error_at (loc, READONLY_MSG (G_("assignment of read-only variable %qD"),
				 G_("increment of read-only variable %qD"),
				 G_("decrement of read-only variable %qD"),
				 G_("read-only variable %qD used as %<asm%> output")),
	      arg);
  else if (TREE_CODE (arg) == PARM_DECL)
    error_at (loc, READONLY_MSG (G_("assignment of read-only parameter %qD"),
				 G_("increment of read-only parameter %qD"),
				 G_("decrement of read-only parameter %qD"),
				 G_("read-only parameter %qD use as %<asm%> output")),
	      arg);
  else if (TREE_CODE (arg) == RESULT_DECL)
    {
      gcc_assert (c_dialect_cxx ());
      error_at (loc, READONLY_MSG (G_("assignment of "
				      "read-only named return value %qD"),
				   G_("increment of "
				      "read-only named return value %qD"),
				   G_("decrement of "
				      "read-only named return value %qD"),
				   G_("read-only named return value %qD "
				      "used as %<asm%>output")),
		arg);
    }
  else if (TREE_CODE (arg) == FUNCTION_DECL)
    error_at (loc, READONLY_MSG (G_("assignment of function %qD"),
				 G_("increment of function %qD"),
				 G_("decrement of function %qD"),
				 G_("function %qD used as %<asm%> output")),
	      arg);
  else
    error_at (loc, READONLY_MSG (G_("assignment of read-only location %qE"),
				 G_("increment of read-only location %qE"),
				 G_("decrement of read-only location %qE"),
				 G_("read-only location %qE used as %<asm%> output")),
	      arg);
}

/* Print an error message for an invalid lvalue.  USE says
   how the lvalue is being used and so selects the error message.  LOC
   is the location for the error.  */

void
lvalue_error (location_t loc, enum lvalue_use use)
{
  switch (use)
    {
    case lv_assign:
      error_at (loc, "lvalue required as left operand of assignment");
      break;
    case lv_increment:
      error_at (loc, "lvalue required as increment operand");
      break;
    case lv_decrement:
      error_at (loc, "lvalue required as decrement operand");
      break;
    case lv_addressof:
      error_at (loc, "lvalue required as unary %<&%> operand");
      break;
    case lv_asm:
      error_at (loc, "lvalue required in %<asm%> statement");
      break;
    default:
      gcc_unreachable ();
    }
}

/* Print an error message for an invalid indirection of type TYPE.
   ERRSTRING is the name of the operator for the indirection.  */

void
invalid_indirection_error (location_t loc, tree type, ref_operator errstring)
{
  switch (errstring)
    {
    case RO_NULL:
      gcc_assert (c_dialect_cxx ());
      error_at (loc, "invalid type argument (have %qT)", type);
      break;
    case RO_ARRAY_INDEXING:
      error_at (loc,
		"invalid type argument of array indexing (have %qT)",
		type);
      break;
    case RO_UNARY_STAR:
      error_at (loc,
		"invalid type argument of unary %<*%> (have %qT)",
		type);
      break;
    case RO_ARROW:
      error_at (loc,
		"invalid type argument of %<->%> (have %qT)",
		type);
      break;
    case RO_ARROW_STAR:
      error_at (loc,
		"invalid type argument of %<->*%> (have %qT)",
		type);
      break;
    case RO_IMPLICIT_CONVERSION:
      error_at (loc,
		"invalid type argument of implicit conversion (have %qT)",
		type);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Subscripting with type char is likely to lose on a machine where
   chars are signed.  So warn on any machine, but optionally.  Don't
   warn for unsigned char since that type is safe.  Don't warn for
   signed char because anyone who uses that must have done so
   deliberately. Furthermore, we reduce the false positive load by
   warning only for non-constant value of type char.
   LOC is the location of the subscripting expression.  */

void
warn_array_subscript_with_type_char (location_t loc, tree index)
{
  if (TYPE_MAIN_VARIANT (TREE_TYPE (index)) == char_type_node)
    {
      /* If INDEX has a location, use it; otherwise use LOC (the location
	 of the subscripting expression as a whole).  */
      loc = EXPR_LOC_OR_LOC (index, loc);
      STRIP_ANY_LOCATION_WRAPPER (index);
      if (TREE_CODE (index) != INTEGER_CST)
	warning_at (loc, OPT_Wchar_subscripts,
		    "array subscript has type %<char%>");
    }
}

/* Implement -Wparentheses for the unexpected C precedence rules, to
   cover cases like x + y << z which readers are likely to
   misinterpret.  We have seen an expression in which CODE is a binary
   operator used to combine expressions ARG_LEFT and ARG_RIGHT, which
   before folding had CODE_LEFT and CODE_RIGHT.  CODE_LEFT and
   CODE_RIGHT may be ERROR_MARK, which means that that side of the
   expression was not formed using a binary or unary operator, or it
   was enclosed in parentheses.  */

void
warn_about_parentheses (location_t loc, enum tree_code code,
			enum tree_code code_left, tree arg_left,
			enum tree_code code_right, tree arg_right)
{
  if (!warn_parentheses)
    return;

  /* This macro tests that the expression ARG with original tree code
     CODE appears to be a boolean expression. or the result of folding a
     boolean expression.  */
#define APPEARS_TO_BE_BOOLEAN_EXPR_P(CODE, ARG)				    \
	(truth_value_p (TREE_CODE (ARG))				    \
	 || TREE_CODE (TREE_TYPE (ARG)) == BOOLEAN_TYPE			    \
	 /* Folding may create 0 or 1 integers from other expressions.  */  \
	 || ((CODE) != INTEGER_CST					    \
	     && (integer_onep (ARG) || integer_zerop (ARG))))

  switch (code)
    {
    case LSHIFT_EXPR:
      if (code_left == PLUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		    "suggest parentheses around %<+%> inside %<<<%>");
      else if (code_right == PLUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		    "suggest parentheses around %<+%> inside %<<<%>");
      else if (code_left == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		    "suggest parentheses around %<-%> inside %<<<%>");
      else if (code_right == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		    "suggest parentheses around %<-%> inside %<<<%>");
      return;

    case RSHIFT_EXPR:
      if (code_left == PLUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		    "suggest parentheses around %<+%> inside %<>>%>");
      else if (code_right == PLUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		    "suggest parentheses around %<+%> inside %<>>%>");
      else if (code_left == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		    "suggest parentheses around %<-%> inside %<>>%>");
      else if (code_right == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		    "suggest parentheses around %<-%> inside %<>>%>");
      return;

    case TRUTH_ORIF_EXPR:
      if (code_left == TRUTH_ANDIF_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		    "suggest parentheses around %<&&%> within %<||%>");
      else if (code_right == TRUTH_ANDIF_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		    "suggest parentheses around %<&&%> within %<||%>");
      return;

    case BIT_IOR_EXPR:
      if (code_left == BIT_AND_EXPR || code_left == BIT_XOR_EXPR
	  || code_left == PLUS_EXPR || code_left == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around arithmetic in operand of %<|%>");
      else if (code_right == BIT_AND_EXPR || code_right == BIT_XOR_EXPR
	       || code_right == PLUS_EXPR || code_right == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around arithmetic in operand of %<|%>");
      /* Check cases like x|y==z */
      else if (TREE_CODE_CLASS (code_left) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<|%>");
      else if (TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<|%>");
      /* Check cases like !x | y */
      else if (code_left == TRUTH_NOT_EXPR
	       && !APPEARS_TO_BE_BOOLEAN_EXPR_P (code_right, arg_right))
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		    "suggest parentheses around operand of "
		    "%<!%> or change %<|%> to %<||%> or %<!%> to %<~%>");
      return;

    case BIT_XOR_EXPR:
      if (code_left == BIT_AND_EXPR
	  || code_left == PLUS_EXPR || code_left == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around arithmetic in operand of %<^%>");
      else if (code_right == BIT_AND_EXPR
	       || code_right == PLUS_EXPR || code_right == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around arithmetic in operand of %<^%>");
      /* Check cases like x^y==z */
      else if (TREE_CODE_CLASS (code_left) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<^%>");
      else if (TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<^%>");
      return;

    case BIT_AND_EXPR:
      if (code_left == PLUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around %<+%> in operand of %<&%>");
      else if (code_right == PLUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around %<+%> in operand of %<&%>");
      else if (code_left == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around %<-%> in operand of %<&%>");
      else if (code_right == MINUS_EXPR)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around %<-%> in operand of %<&%>");
      /* Check cases like x&y==z */
      else if (TREE_CODE_CLASS (code_left) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<&%>");
      else if (TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<&%>");
      /* Check cases like !x & y */
      else if (code_left == TRUTH_NOT_EXPR
	       && !APPEARS_TO_BE_BOOLEAN_EXPR_P (code_right, arg_right))
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		    "suggest parentheses around operand of "
		    "%<!%> or change %<&%> to %<&&%> or %<!%> to %<~%>");
      return;

    case EQ_EXPR:
      if (TREE_CODE_CLASS (code_left) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<==%>");
      else if (TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<==%>");
      return;
    case NE_EXPR:
      if (TREE_CODE_CLASS (code_left) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<!=%>");
      else if (TREE_CODE_CLASS (code_right) == tcc_comparison)
	warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
		 "suggest parentheses around comparison in operand of %<!=%>");
      return;

    default:
      if (TREE_CODE_CLASS (code) == tcc_comparison)
	{
	  if (TREE_CODE_CLASS (code_left) == tcc_comparison
		&& code_left != NE_EXPR && code_left != EQ_EXPR
		&& INTEGRAL_TYPE_P (TREE_TYPE (arg_left)))
	    warning_at (EXPR_LOC_OR_LOC (arg_left, loc), OPT_Wparentheses,
			"comparisons like %<X<=Y<=Z%> do not "
			"have their mathematical meaning");
	  else if (TREE_CODE_CLASS (code_right) == tcc_comparison
		   && code_right != NE_EXPR && code_right != EQ_EXPR
		   && INTEGRAL_TYPE_P (TREE_TYPE (arg_right)))
	    warning_at (EXPR_LOC_OR_LOC (arg_right, loc), OPT_Wparentheses,
			"comparisons like %<X<=Y<=Z%> do not "
			"have their mathematical meaning");
	}
      return;
    }
#undef NOT_A_BOOLEAN_EXPR_P
}

/* If LABEL (a LABEL_DECL) has not been used, issue a warning.  */

void
warn_for_unused_label (tree label)
{
  if (!TREE_USED (label))
    {
      if (DECL_INITIAL (label))
	warning (OPT_Wunused_label, "label %q+D defined but not used", label);
      else
	warning (OPT_Wunused_label, "label %q+D declared but not defined", label);
    }
  else if (asan_sanitize_use_after_scope ())
    {
      if (asan_used_labels == NULL)
	asan_used_labels = new hash_set<tree> (16);

      asan_used_labels->add (label);
    }
}

/* Warn for division by zero according to the value of DIVISOR.  LOC
   is the location of the division operator.  */

void
warn_for_div_by_zero (location_t loc, tree divisor)
{
  /* If DIVISOR is zero, and has integral or fixed-point type, issue a warning
     about division by zero.  Do not issue a warning if DIVISOR has a
     floating-point type, since we consider 0.0/0.0 a valid way of
     generating a NaN.  */
  if (c_inhibit_evaluation_warnings == 0
      && (integer_zerop (divisor) || fixed_zerop (divisor)))
    warning_at (loc, OPT_Wdiv_by_zero, "division by zero");
}

/* Warn for patterns where memset appears to be used incorrectly.  The
   warning location should be LOC.  ARG0, and ARG2 are the first and
   last arguments to the call, while LITERAL_ZERO_MASK has a 1 bit for
   each argument that was a literal zero.  */

void
warn_for_memset (location_t loc, tree arg0, tree arg2,
		 int literal_zero_mask)
{
  arg0 = fold_for_warn (arg0);
  arg2 = fold_for_warn (arg2);

  if (warn_memset_transposed_args
      && integer_zerop (arg2)
      && (literal_zero_mask & (1 << 2)) != 0
      && (literal_zero_mask & (1 << 1)) == 0)
    warning_at (loc, OPT_Wmemset_transposed_args,
		"%<memset%> used with constant zero length "
		"parameter; this could be due to transposed "
		"parameters");

  if (warn_memset_elt_size && TREE_CODE (arg2) == INTEGER_CST)
    {
      STRIP_NOPS (arg0);
      if (TREE_CODE (arg0) == ADDR_EXPR)
	arg0 = TREE_OPERAND (arg0, 0);
      tree type = TREE_TYPE (arg0);
      if (type != NULL_TREE && TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree elt_type = TREE_TYPE (type);
	  tree domain = TYPE_DOMAIN (type);
	  if (COMPLETE_TYPE_P (elt_type)
	      && !integer_onep (TYPE_SIZE_UNIT (elt_type))
	      && domain != NULL_TREE
	      && TYPE_MAX_VALUE (domain)
	      && TYPE_MIN_VALUE (domain)
	      && integer_zerop (TYPE_MIN_VALUE (domain))
	      && integer_onep (fold_build2 (MINUS_EXPR, domain,
					    arg2,
					    TYPE_MAX_VALUE (domain))))
	    warning_at (loc, OPT_Wmemset_elt_size,
			"%<memset%> used with length equal to "
			"number of elements without multiplication "
			"by element size");
	}
    }
}

/* Subroutine of build_binary_op. Give warnings for comparisons
   between signed and unsigned quantities that may fail. Do the
   checking based on the original operand trees ORIG_OP0 and ORIG_OP1,
   so that casts will be considered, but default promotions won't
   be.

   LOCATION is the location of the comparison operator.

   The arguments of this function map directly to local variables
   of build_binary_op.  */

void
warn_for_sign_compare (location_t location,
		       tree orig_op0, tree orig_op1,
		       tree op0, tree op1,
		       tree result_type, enum tree_code resultcode)
{
  if (error_operand_p (orig_op0) || error_operand_p (orig_op1))
    return;

  int op0_signed = !TYPE_UNSIGNED (TREE_TYPE (orig_op0));
  int op1_signed = !TYPE_UNSIGNED (TREE_TYPE (orig_op1));
  int unsignedp0, unsignedp1;

  /* In C++, check for comparison of different enum types.  */
  if (c_dialect_cxx()
      && TREE_CODE (TREE_TYPE (orig_op0)) == ENUMERAL_TYPE
      && TREE_CODE (TREE_TYPE (orig_op1)) == ENUMERAL_TYPE
      && TYPE_MAIN_VARIANT (TREE_TYPE (orig_op0))
	 != TYPE_MAIN_VARIANT (TREE_TYPE (orig_op1)))
    {
      warning_at (location,
		  OPT_Wsign_compare, "comparison between types %qT and %qT",
		  TREE_TYPE (orig_op0), TREE_TYPE (orig_op1));
    }

  /* Do not warn if the comparison is being done in a signed type,
     since the signed type will only be chosen if it can represent
     all the values of the unsigned type.  */
  if (!TYPE_UNSIGNED (result_type))
    /* OK */;
  /* Do not warn if both operands are unsigned.  */
  else if (op0_signed == op1_signed)
    /* OK */;
  else
    {
      tree sop, uop, base_type;
      bool ovf;

      if (op0_signed)
	sop = orig_op0, uop = orig_op1;
      else
	sop = orig_op1, uop = orig_op0;

      sop = fold_for_warn (sop);
      uop = fold_for_warn (uop);

      STRIP_TYPE_NOPS (sop);
      STRIP_TYPE_NOPS (uop);
      base_type = (TREE_CODE (result_type) == COMPLEX_TYPE
		   ? TREE_TYPE (result_type) : result_type);

      /* Do not warn if the signed quantity is an unsuffixed integer
	 literal (or some static constant expression involving such
	 literals or a conditional expression involving such literals)
	 and it is non-negative.  */
      if (tree_expr_nonnegative_warnv_p (sop, &ovf))
	/* OK */;
      /* Do not warn if the comparison is an equality operation, the
	 unsigned quantity is an integral constant, and it would fit
	 in the result if the result were signed.  */
      else if (TREE_CODE (uop) == INTEGER_CST
	       && (resultcode == EQ_EXPR || resultcode == NE_EXPR)
	       && int_fits_type_p (uop, c_common_signed_type (base_type)))
	/* OK */;
      /* In C, do not warn if the unsigned quantity is an enumeration
	 constant and its maximum value would fit in the result if the
	 result were signed.  */
      else if (!c_dialect_cxx() && TREE_CODE (uop) == INTEGER_CST
	       && TREE_CODE (TREE_TYPE (uop)) == ENUMERAL_TYPE
	       && int_fits_type_p (TYPE_MAX_VALUE (TREE_TYPE (uop)),
				   c_common_signed_type (base_type)))
	/* OK */;
      else
	warning_at (location, OPT_Wsign_compare,
		    "comparison of integer expressions of different "
		    "signedness: %qT and %qT", TREE_TYPE (orig_op0),
		    TREE_TYPE (orig_op1));
    }

  /* Warn if two unsigned values are being compared in a size larger
     than their original size, and one (and only one) is the result of
     a `~' operator.  This comparison will always fail.

     Also warn if one operand is a constant, and the constant does not
     have all bits set that are set in the ~ operand when it is
     extended.  */

  op0 = c_common_get_narrower (op0, &unsignedp0);
  op1 = c_common_get_narrower (op1, &unsignedp1);

  if ((TREE_CODE (op0) == BIT_NOT_EXPR)
      ^ (TREE_CODE (op1) == BIT_NOT_EXPR))
    {
      if (TREE_CODE (op0) == BIT_NOT_EXPR)
	op0 = c_common_get_narrower (TREE_OPERAND (op0, 0), &unsignedp0);
      if (TREE_CODE (op1) == BIT_NOT_EXPR)
	op1 = c_common_get_narrower (TREE_OPERAND (op1, 0), &unsignedp1);

      if (tree_fits_shwi_p (op0) || tree_fits_shwi_p (op1))
	{
	  tree primop;
	  HOST_WIDE_INT constant, mask;
	  int unsignedp;
	  unsigned int bits;

	  if (tree_fits_shwi_p (op0))
	    {
	      primop = op1;
	      unsignedp = unsignedp1;
	      constant = tree_to_shwi (op0);
	    }
	  else
	    {
	      primop = op0;
	      unsignedp = unsignedp0;
	      constant = tree_to_shwi (op1);
	    }

	  bits = TYPE_PRECISION (TREE_TYPE (primop));
	  if (bits < TYPE_PRECISION (result_type)
	      && bits < HOST_BITS_PER_LONG && unsignedp)
	    {
	      mask = HOST_WIDE_INT_M1U << bits;
	      if ((mask & constant) != mask)
		{
		  if (constant == 0)
		    warning_at (location, OPT_Wsign_compare,
				"promoted bitwise complement of an unsigned "
				"value is always nonzero");
		  else
		    warning_at (location, OPT_Wsign_compare,
				"comparison of promoted bitwise complement "
				"of an unsigned value with constant");
		}
	    }
	}
      else if (unsignedp0 && unsignedp1
	       && (TYPE_PRECISION (TREE_TYPE (op0))
		   < TYPE_PRECISION (result_type))
	       && (TYPE_PRECISION (TREE_TYPE (op1))
		   < TYPE_PRECISION (result_type)))
	warning_at (location, OPT_Wsign_compare,
		    "comparison of promoted bitwise complement "
		    "of an unsigned value with unsigned");
    }
}

/* RESULT_TYPE is the result of converting TYPE1 and TYPE2 to a common
   type via c_common_type.  If -Wdouble-promotion is in use, and the
   conditions for warning have been met, issue a warning.  GMSGID is
   the warning message.  It must have two %T specifiers for the type
   that was converted (generally "float") and the type to which it was
   converted (generally "double), respectively.  LOC is the location
   to which the warning should refer.  */

void
do_warn_double_promotion (tree result_type, tree type1, tree type2,
			 const char *gmsgid, location_t loc)
{
  tree source_type;

  if (!warn_double_promotion)
    return;
  /* If the conversion will not occur at run-time, there is no need to
     warn about it.  */
  if (c_inhibit_evaluation_warnings)
    return;
  /* If an invalid conversion has occured, don't warn.  */
  if (result_type == error_mark_node)
    return;
  if (TYPE_MAIN_VARIANT (result_type) != double_type_node
      && TYPE_MAIN_VARIANT (result_type) != complex_double_type_node)
    return;
  if (TYPE_MAIN_VARIANT (type1) == float_type_node
      || TYPE_MAIN_VARIANT (type1) == complex_float_type_node)
    source_type = type1;
  else if (TYPE_MAIN_VARIANT (type2) == float_type_node
	   || TYPE_MAIN_VARIANT (type2) == complex_float_type_node)
    source_type = type2;
  else
    return;
  warning_at (loc, OPT_Wdouble_promotion, gmsgid, source_type, result_type);
}

/* Possibly warn about unused parameters.  */

void
do_warn_unused_parameter (tree fn)
{
  tree decl;

  for (decl = DECL_ARGUMENTS (fn);
       decl; decl = DECL_CHAIN (decl))
    if (!TREE_USED (decl) && TREE_CODE (decl) == PARM_DECL
	&& DECL_NAME (decl) && !DECL_ARTIFICIAL (decl)
	&& !TREE_NO_WARNING (decl))
      warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wunused_parameter,
		  "unused parameter %qD", decl);
}

/* If DECL is a typedef that is declared in the current function,
   record it for the purpose of -Wunused-local-typedefs.  */

void
record_locally_defined_typedef (tree decl)
{
  struct c_language_function *l;

  if (!warn_unused_local_typedefs
      || cfun == NULL
      /* if this is not a locally defined typedef then we are not
	 interested.  */
      || !is_typedef_decl (decl)
      || !decl_function_context (decl))
    return;

  l = (struct c_language_function *) cfun->language;
  vec_safe_push (l->local_typedefs, decl);
}

/* If T is a TYPE_DECL declared locally, mark it as used.  */

void
maybe_record_typedef_use (tree t)
{
  if (!is_typedef_decl (t))
    return;

  TREE_USED (t) = true;
}

/* Warn if there are some unused locally defined typedefs in the
   current function. */

void
maybe_warn_unused_local_typedefs (void)
{
  int i;
  tree decl;
  /* The number of times we have emitted -Wunused-local-typedefs
     warnings.  If this is different from errorcount, that means some
     unrelated errors have been issued.  In which case, we'll avoid
     emitting "unused-local-typedefs" warnings.  */
  static int unused_local_typedefs_warn_count;
  struct c_language_function *l;

  if (cfun == NULL)
    return;

  if ((l = (struct c_language_function *) cfun->language) == NULL)
    return;

  if (warn_unused_local_typedefs
      && errorcount == unused_local_typedefs_warn_count)
    {
      FOR_EACH_VEC_SAFE_ELT (l->local_typedefs, i, decl)
	if (!TREE_USED (decl))
	  warning_at (DECL_SOURCE_LOCATION (decl),
		      OPT_Wunused_local_typedefs,
		      "typedef %qD locally defined but not used", decl);
      unused_local_typedefs_warn_count = errorcount;
    }

  vec_free (l->local_typedefs);
}

/* If we're creating an if-else-if condition chain, first see if we
   already have this COND in the CHAIN.  If so, warn and don't add COND
   into the vector, otherwise add the COND there.  LOC is the location
   of COND.  */

void
warn_duplicated_cond_add_or_warn (location_t loc, tree cond, vec<tree> **chain)
{
  /* No chain has been created yet.  Do nothing.  */
  if (*chain == NULL)
    return;

  if (TREE_SIDE_EFFECTS (cond))
    {
      /* Uh-oh!  This condition has a side-effect, thus invalidates
	 the whole chain.  */
      delete *chain;
      *chain = NULL;
      return;
    }

  unsigned int ix;
  tree t;
  bool found = false;
  FOR_EACH_VEC_ELT (**chain, ix, t)
    if (operand_equal_p (cond, t, 0))
      {
	auto_diagnostic_group d;
	if (warning_at (loc, OPT_Wduplicated_cond,
			"duplicated %<if%> condition"))
	  inform (EXPR_LOCATION (t), "previously used here");
	found = true;
	break;
      }

  if (!found
      && !CONSTANT_CLASS_P (cond)
      /* Don't infinitely grow the chain.  */
      && (*chain)->length () < 512)
    (*chain)->safe_push (cond);
}

/* Check and possibly warn if two declarations have contradictory
   attributes, such as always_inline vs. noinline.  */

bool
diagnose_mismatched_attributes (tree olddecl, tree newdecl)
{
  bool warned = false;

  tree a1 = lookup_attribute ("optimize", DECL_ATTRIBUTES (olddecl));
  tree a2 = lookup_attribute ("optimize", DECL_ATTRIBUTES (newdecl));
  /* An optimization attribute applied on a declaration after the
     definition is likely not what the user wanted.  */
  if (a2 != NULL_TREE
      && DECL_SAVED_TREE (olddecl) != NULL_TREE
      && (a1 == NULL_TREE || !attribute_list_equal (a1, a2)))
    warned |= warning (OPT_Wattributes,
		       "optimization attribute on %qD follows "
		       "definition but the attribute doesn%'t match",
		       newdecl);

  /* Diagnose inline __attribute__ ((noinline)) which is silly.  */
  if (DECL_DECLARED_INLINE_P (newdecl)
      && DECL_UNINLINABLE (olddecl)
      && lookup_attribute ("noinline", DECL_ATTRIBUTES (olddecl)))
    warned |= warning (OPT_Wattributes, "inline declaration of %qD follows "
		       "declaration with attribute %<noinline%>", newdecl);
  else if (DECL_DECLARED_INLINE_P (olddecl)
	   && DECL_UNINLINABLE (newdecl)
	   && lookup_attribute ("noinline", DECL_ATTRIBUTES (newdecl)))
    warned |= warning (OPT_Wattributes, "declaration of %q+D with attribute "
		       "%<noinline%> follows inline declaration", newdecl);

  return warned;
}

/* Warn if signed left shift overflows.  We don't warn
   about left-shifting 1 into the sign bit in C++14; cf.
   <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3367.html#1457>
   and don't warn for C++20 at all, as signed left shifts never
   overflow.
   LOC is a location of the shift; OP0 and OP1 are the operands.
   Return true if an overflow is detected, false otherwise.  */

bool
maybe_warn_shift_overflow (location_t loc, tree op0, tree op1)
{
  if (TREE_CODE (op0) != INTEGER_CST
      || TREE_CODE (op1) != INTEGER_CST)
    return false;

  tree type0 = TREE_TYPE (op0);
  unsigned int prec0 = TYPE_PRECISION (type0);

  /* Left-hand operand must be signed.  */
  if (TYPE_UNSIGNED (type0) || cxx_dialect >= cxx20)
    return false;

  unsigned int min_prec = (wi::min_precision (wi::to_wide (op0), SIGNED)
			   + TREE_INT_CST_LOW (op1));
  /* Handle the case of left-shifting 1 into the sign bit.
   * However, shifting 1 _out_ of the sign bit, as in
   * INT_MIN << 1, is considered an overflow.
   */
  if (!tree_int_cst_sign_bit (op0) && min_prec == prec0 + 1)
    {
      /* Never warn for C++14 onwards.  */
      if (cxx_dialect >= cxx14)
	return false;
      /* Otherwise only if -Wshift-overflow=2.  But return
	 true to signal an overflow for the sake of integer
	 constant expressions.  */
      if (warn_shift_overflow < 2)
	return true;
    }

  bool overflowed = min_prec > prec0;
  if (overflowed && c_inhibit_evaluation_warnings == 0)
    warning_at (loc, OPT_Wshift_overflow_,
		"result of %qE requires %u bits to represent, "
		"but %qT only has %u bits",
		build2_loc (loc, LSHIFT_EXPR, type0, op0, op1),
		min_prec, type0, prec0);

  return overflowed;
}

/* Warn about boolean expression compared with an integer value different
   from true/false.  Warns also e.g. about "(i1 == i2) == 2".
   LOC is the location of the comparison, CODE is its code, OP0 and OP1
   are the operands of the comparison.  The caller must ensure that
   either operand is a boolean expression.  */

void
maybe_warn_bool_compare (location_t loc, enum tree_code code, tree op0,
			 tree op1)
{
  if (TREE_CODE_CLASS (code) != tcc_comparison)
    return;

  tree f, cst;
  if (f = fold_for_warn (op0),
      TREE_CODE (f) == INTEGER_CST)
    cst = op0 = f;
  else if (f = fold_for_warn (op1),
	   TREE_CODE (f) == INTEGER_CST)
    cst = op1 = f;
  else
    return;

  if (!integer_zerop (cst) && !integer_onep (cst))
    {
      int sign = (TREE_CODE (op0) == INTEGER_CST
		 ? tree_int_cst_sgn (cst) : -tree_int_cst_sgn (cst));
      if (code == EQ_EXPR
	  || ((code == GT_EXPR || code == GE_EXPR) && sign < 0)
	  || ((code == LT_EXPR || code == LE_EXPR) && sign > 0))
	warning_at (loc, OPT_Wbool_compare, "comparison of constant %qE "
		    "with boolean expression is always false", cst);
      else
	warning_at (loc, OPT_Wbool_compare, "comparison of constant %qE "
		    "with boolean expression is always true", cst);
    }
  else if (integer_zerop (cst) || integer_onep (cst))
    {
      /* If the non-constant operand isn't of a boolean type, we
	 don't want to warn here.  */
      tree noncst = TREE_CODE (op0) == INTEGER_CST ? op1 : op0;
      /* Handle booleans promoted to integers.  */
      if (bool_promoted_to_int_p (noncst))
	/* Warn.  */;
      else if (TREE_CODE (TREE_TYPE (noncst)) != BOOLEAN_TYPE
	       && !truth_value_p (TREE_CODE (noncst)))
	return;
      /* Do some magic to get the right diagnostics.  */
      bool flag = TREE_CODE (op0) == INTEGER_CST;
      flag = integer_zerop (cst) ? flag : !flag;
      if ((code == GE_EXPR && !flag) || (code == LE_EXPR && flag))
	warning_at (loc, OPT_Wbool_compare, "comparison of constant %qE "
		    "with boolean expression is always true", cst);
      else if ((code == LT_EXPR && !flag) || (code == GT_EXPR && flag))
	warning_at (loc, OPT_Wbool_compare, "comparison of constant %qE "
		    "with boolean expression is always false", cst);
    }
}

/* Warn if an argument at position param_pos is passed to a
   restrict-qualified param, and it aliases with another argument.
   Return true if a warning has been issued.  */

bool
warn_for_restrict (unsigned param_pos, tree *argarray, unsigned nargs)
{
  tree arg = argarray[param_pos];
  if (TREE_VISITED (arg) || integer_zerop (arg))
    return false;

  location_t loc = EXPR_LOC_OR_LOC (arg, input_location);
  gcc_rich_location richloc (loc);

  unsigned i;
  auto_vec<int, 16> arg_positions;

  for (i = 0; i < nargs; i++)
    {
      if (i == param_pos)
	continue;

      tree current_arg = argarray[i];
      if (operand_equal_p (arg, current_arg, 0))
	{
	  TREE_VISITED (current_arg) = 1;
	  arg_positions.safe_push (i + 1);
	}
    }

  if (arg_positions.is_empty ())
    return false;

  int pos;
  FOR_EACH_VEC_ELT (arg_positions, i, pos)
    {
      arg = argarray[pos - 1];
      if (EXPR_HAS_LOCATION (arg))
	richloc.add_range (EXPR_LOCATION (arg));
    }

  return warning_n (&richloc, OPT_Wrestrict, arg_positions.length (),
		    "passing argument %i to %qs-qualified parameter"
		    " aliases with argument %Z",
		    "passing argument %i to %qs-qualified parameter"
		    " aliases with arguments %Z",
		    param_pos + 1, "restrict", arg_positions.address (),
		    arg_positions.length ());
}

/* Callback function to determine whether an expression TP or one of its
   subexpressions comes from macro expansion.  Used to suppress bogus
   warnings.  */

static tree
expr_from_macro_expansion_r (tree *tp, int *, void *)
{
  if (CAN_HAVE_LOCATION_P (*tp)
      && from_macro_expansion_at (EXPR_LOCATION (*tp)))
    return integer_zero_node;

  return NULL_TREE;
}

/* Possibly warn when an if-else has identical branches.  */

static void
do_warn_duplicated_branches (tree expr)
{
  tree thenb = COND_EXPR_THEN (expr);
  tree elseb = COND_EXPR_ELSE (expr);

  /* Don't bother if any of the branches is missing.  */
  if (thenb == NULL_TREE || elseb == NULL_TREE)
    return;

  /* And don't warn for empty statements.  */
  if (TREE_CODE (thenb) == NOP_EXPR
      && TREE_TYPE (thenb) == void_type_node
      && TREE_OPERAND (thenb, 0) == size_zero_node)
    return;

  /* ... or empty branches.  */
  if (TREE_CODE (thenb) == STATEMENT_LIST
      && STATEMENT_LIST_HEAD (thenb) == NULL)
    return;

  /* Compute the hash of the then branch.  */
  inchash::hash hstate0 (0);
  inchash::add_expr (thenb, hstate0);
  hashval_t h0 = hstate0.end ();

  /* Compute the hash of the else branch.  */
  inchash::hash hstate1 (0);
  inchash::add_expr (elseb, hstate1);
  hashval_t h1 = hstate1.end ();

  /* Compare the hashes.  */
  if (h0 == h1
      && operand_equal_p (thenb, elseb, OEP_LEXICOGRAPHIC)
      /* Don't warn if any of the branches or their subexpressions comes
	 from a macro.  */
      && !walk_tree_without_duplicates (&thenb, expr_from_macro_expansion_r,
					NULL)
      && !walk_tree_without_duplicates (&elseb, expr_from_macro_expansion_r,
					NULL))
    warning_at (EXPR_LOCATION (expr), OPT_Wduplicated_branches,
		"this condition has identical branches");
}

/* Callback for c_genericize to implement -Wduplicated-branches.  */

tree
do_warn_duplicated_branches_r (tree *tp, int *, void *)
{
  if (TREE_CODE (*tp) == COND_EXPR)
    do_warn_duplicated_branches (*tp);
  return NULL_TREE;
}

/* Implementation of -Wmultistatement-macros.  This warning warns about
   cases when a macro expands to multiple statements not wrapped in
   do {} while (0) or ({ }) and is used as a body of if/else/for/while
   conditionals.  For example,

   #define DOIT x++; y++

   if (c)
     DOIT;

   will increment y unconditionally.

   BODY_LOC is the location of the first token in the body after labels
   have been parsed, NEXT_LOC is the location of the next token after the
   body of the conditional has been parsed, and GUARD_LOC is the location
   of the conditional.  */

void
warn_for_multistatement_macros (location_t body_loc, location_t next_loc,
				location_t guard_loc, enum rid keyword)
{
  if (!warn_multistatement_macros)
    return;

  /* Ain't got time to waste.  We only care about macros here.  */
  if (!from_macro_expansion_at (body_loc)
      || !from_macro_expansion_at (next_loc))
    return;

  /* Let's skip macros defined in system headers.  */
  if (in_system_header_at (body_loc)
      || in_system_header_at (next_loc))
    return;

  /* Find the actual tokens in the macro definition.  BODY_LOC and
     NEXT_LOC have to come from the same spelling location, but they
     will resolve to different locations in the context of the macro
     definition.  */
  location_t body_loc_exp
    = linemap_resolve_location (line_table, body_loc,
				LRK_MACRO_DEFINITION_LOCATION, NULL);
  location_t next_loc_exp
    = linemap_resolve_location (line_table, next_loc,
				LRK_MACRO_DEFINITION_LOCATION, NULL);
  location_t guard_loc_exp
    = linemap_resolve_location (line_table, guard_loc,
				LRK_MACRO_DEFINITION_LOCATION, NULL);

  /* These are some funky cases we don't want to warn about.  */
  if (body_loc_exp == guard_loc_exp
      || next_loc_exp == guard_loc_exp
      || body_loc_exp == next_loc_exp)
    return;

  /* Find the macro maps for the macro expansions.  */
  const line_map *body_map = linemap_lookup (line_table, body_loc);
  const line_map *next_map = linemap_lookup (line_table, next_loc);
  const line_map *guard_map = linemap_lookup (line_table, guard_loc);

  /* Now see if the following token (after the body) is coming from the
     same macro expansion.  If it is, it might be a problem.  */
  if (body_map != next_map)
    return;

  /* The conditional itself must not come from the same expansion, because
     we don't want to warn about
     #define IF if (x) x++; y++
     and similar.  */
  if (guard_map == body_map)
    return;

  /* Handle the case where NEXT and BODY come from the same expansion while
     GUARD doesn't, yet we shouldn't warn.  E.g.

       #define GUARD if (...)
       #define GUARD2 GUARD

     and in the definition of another macro:

       GUARD2
	foo ();
       return 1;
   */
  while (linemap_macro_expansion_map_p (guard_map))
    {
      const line_map_macro *mm = linemap_check_macro (guard_map);
      guard_loc_exp = MACRO_MAP_EXPANSION_POINT_LOCATION (mm);
      guard_map = linemap_lookup (line_table, guard_loc_exp);
      if (guard_map == body_map)
	return;
    }

  auto_diagnostic_group d;
  if (warning_at (body_loc, OPT_Wmultistatement_macros,
		  "macro expands to multiple statements"))
    inform (guard_loc, "some parts of macro expansion are not guarded by "
	    "this %qs clause", guard_tinfo_to_string (keyword));
}

/* Return struct or union type if the alignment of data memeber, FIELD,
   is less than the alignment of TYPE.  Otherwise, return NULL_TREE.
   If RVALUE is true, only arrays evaluate to pointers.  */

static tree
check_alignment_of_packed_member (tree type, tree field, bool rvalue)
{
  /* Check alignment of the data member.  */
  if (TREE_CODE (field) == FIELD_DECL
      && (DECL_PACKED (field) || TYPE_PACKED (TREE_TYPE (field)))
      /* Ignore FIELDs not laid out yet.  */
      && DECL_FIELD_OFFSET (field)
      && (!rvalue || TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE))
    {
      /* Check the expected alignment against the field alignment.  */
      unsigned int type_align = min_align_of_type (type);
      tree context = DECL_CONTEXT (field);
      unsigned int record_align = min_align_of_type (context);
      if (record_align < type_align)
	return context;
      tree field_off = byte_position (field);
      if (!multiple_of_p (TREE_TYPE (field_off), field_off,
			  size_int (type_align)))
	return context;
    }

  return NULL_TREE;
}

/* Return struct or union type if the right hand value, RHS:
   1. Is a pointer value which isn't aligned to a pointer type TYPE.
   2. Is an address which takes the unaligned address of packed member
      of struct or union when assigning to TYPE.
   Otherwise, return NULL_TREE.  */

static tree
check_address_or_pointer_of_packed_member (tree type, tree rhs)
{
  bool rvalue = true;
  bool indirect = false;

  if (INDIRECT_REF_P (rhs))
    {
      rhs = TREE_OPERAND (rhs, 0);
      STRIP_NOPS (rhs);
      indirect = true;
    }

  if (TREE_CODE (rhs) == ADDR_EXPR)
    {
      rhs = TREE_OPERAND (rhs, 0);
      rvalue = indirect;
    }

  if (!POINTER_TYPE_P (type))
    return NULL_TREE;

  type = TREE_TYPE (type);

  if (TREE_CODE (rhs) == PARM_DECL
      || VAR_P (rhs)
      || TREE_CODE (rhs) == CALL_EXPR)
    {
      tree rhstype = TREE_TYPE (rhs);
      if (TREE_CODE (rhs) == CALL_EXPR)
	{
	  rhs = CALL_EXPR_FN (rhs);	/* Pointer expression.  */
	  if (rhs == NULL_TREE)
	    return NULL_TREE;
	  rhs = TREE_TYPE (rhs);	/* Pointer type.  */
	  rhs = TREE_TYPE (rhs);	/* Function type.  */
	  rhstype = TREE_TYPE (rhs);
	  if (!rhstype || !POINTER_TYPE_P (rhstype))
	    return NULL_TREE;
	  rvalue = true;
	}
      if (rvalue && POINTER_TYPE_P (rhstype))
	rhstype = TREE_TYPE (rhstype);
      while (TREE_CODE (rhstype) == ARRAY_TYPE)
	rhstype = TREE_TYPE (rhstype);
      if (TYPE_PACKED (rhstype))
	{
	  unsigned int type_align = min_align_of_type (type);
	  unsigned int rhs_align = min_align_of_type (rhstype);
	  if (rhs_align < type_align)
	    {
	      auto_diagnostic_group d;
	      location_t location = EXPR_LOC_OR_LOC (rhs, input_location);
	      if (warning_at (location, OPT_Waddress_of_packed_member,
			      "converting a packed %qT pointer (alignment %d) "
			      "to a %qT pointer (alignment %d) may result in "
			      "an unaligned pointer value",
			      rhstype, rhs_align, type, type_align))
		{
		  tree decl = TYPE_STUB_DECL (rhstype);
		  if (decl)
		    inform (DECL_SOURCE_LOCATION (decl), "defined here");
		  decl = TYPE_STUB_DECL (type);
		  if (decl)
		    inform (DECL_SOURCE_LOCATION (decl), "defined here");
		}
	    }
	}
      return NULL_TREE;
    }

  tree context = NULL_TREE;

  /* Check alignment of the object.  */
  while (handled_component_p (rhs))
    {
      if (TREE_CODE (rhs) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (rhs, 1);
	  context = check_alignment_of_packed_member (type, field, rvalue);
	  if (context)
	    break;
	}
      if (TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE)
	rvalue = false;
      if (rvalue)
	return NULL_TREE;
      rhs = TREE_OPERAND (rhs, 0);
    }

  return context;
}

/* Check and warn if the right hand value, RHS:
   1. Is a pointer value which isn't aligned to a pointer type TYPE.
   2. Is an address which takes the unaligned address of packed member
      of struct or union when assigning to TYPE.
 */

static void
check_and_warn_address_or_pointer_of_packed_member (tree type, tree rhs)
{
  bool nop_p = false;
  tree orig_rhs;

  do
    {
      while (TREE_CODE (rhs) == COMPOUND_EXPR)
	rhs = TREE_OPERAND (rhs, 1);
      orig_rhs = rhs;
      STRIP_NOPS (rhs);
      nop_p |= orig_rhs != rhs;
    }
  while (orig_rhs != rhs);

  if (TREE_CODE (rhs) == COND_EXPR)
    {
      /* Check the THEN path.  */
      check_and_warn_address_or_pointer_of_packed_member
	(type, TREE_OPERAND (rhs, 1));

      /* Check the ELSE path.  */
      check_and_warn_address_or_pointer_of_packed_member
	(type, TREE_OPERAND (rhs, 2));
    }
  else
    {
      if (nop_p)
	{
	  switch (TREE_CODE (rhs))
	    {
	    case ADDR_EXPR:
	      /* Address is taken.   */
	    case PARM_DECL:
	    case VAR_DECL:
	      /* Pointer conversion.  */
	      break;
	    case CALL_EXPR:
	      /* Function call. */
	      break;
	    default:
	      return;
	    }
	}

      tree context
	= check_address_or_pointer_of_packed_member (type, rhs);
      if (context)
	{
	  location_t loc = EXPR_LOC_OR_LOC (rhs, input_location);
	  warning_at (loc, OPT_Waddress_of_packed_member,
		      "taking address of packed member of %qT may result "
		      "in an unaligned pointer value",
		      context);
	}
    }
}

/* Warn if the right hand value, RHS:
   1. Is a pointer value which isn't aligned to a pointer type TYPE.
   2. Is an address which takes the unaligned address of packed member
      of struct or union when assigning to TYPE.
*/

void
warn_for_address_or_pointer_of_packed_member (tree type, tree rhs)
{
  if (!warn_address_of_packed_member)
    return;

  /* Don't warn if we don't assign RHS to a pointer.  */
  if (!POINTER_TYPE_P (type))
    return;

  check_and_warn_address_or_pointer_of_packed_member (type, rhs);
}
