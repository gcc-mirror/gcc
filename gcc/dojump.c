/* Convert tree expression to rtl instructions, for GNU compiler.
   Copyright (C) 1988-2017 Free Software Foundation, Inc.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "fold-const.h"
#include "stor-layout.h"
/* Include expr.h after insn-config.h so we get HAVE_conditional_move.  */
#include "dojump.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"

static bool prefer_and_bit_test (machine_mode, int);
static void do_jump_by_parts_greater (tree, tree, int,
				      rtx_code_label *, rtx_code_label *, int);
static void do_jump_by_parts_equality (tree, tree, rtx_code_label *,
				       rtx_code_label *, int);
static void do_compare_and_jump	(tree, tree, enum rtx_code, enum rtx_code,
				 rtx_code_label *, rtx_code_label *, int);

/* Invert probability if there is any.  -1 stands for unknown.  */

static inline int
inv (int prob)
{
  return prob == -1 ? -1 : REG_BR_PROB_BASE - prob;
}

/* At the start of a function, record that we have no previously-pushed
   arguments waiting to be popped.  */

void
init_pending_stack_adjust (void)
{
  pending_stack_adjust = 0;
}

/* Discard any pending stack adjustment.  This avoid relying on the
   RTL optimizers to remove useless adjustments when we know the
   stack pointer value is dead.  */
void
discard_pending_stack_adjust (void)
{
  stack_pointer_delta -= pending_stack_adjust;
  pending_stack_adjust = 0;
}

/* When exiting from function, if safe, clear out any pending stack adjust
   so the adjustment won't get done.

   Note, if the current function calls alloca, then it must have a
   frame pointer regardless of the value of flag_omit_frame_pointer.  */

void
clear_pending_stack_adjust (void)
{
  if (optimize > 0
      && (! flag_omit_frame_pointer || cfun->calls_alloca)
      && EXIT_IGNORE_STACK)
    discard_pending_stack_adjust ();
}

/* Pop any previously-pushed arguments that have not been popped yet.  */

void
do_pending_stack_adjust (void)
{
  if (inhibit_defer_pop == 0)
    {
      if (pending_stack_adjust != 0)
        adjust_stack (GEN_INT (pending_stack_adjust));
      pending_stack_adjust = 0;
    }
}

/* Remember pending_stack_adjust/stack_pointer_delta.
   To be used around code that may call do_pending_stack_adjust (),
   but the generated code could be discarded e.g. using delete_insns_since.  */

void
save_pending_stack_adjust (saved_pending_stack_adjust *save)
{
  save->x_pending_stack_adjust = pending_stack_adjust;
  save->x_stack_pointer_delta = stack_pointer_delta;
}

/* Restore the saved pending_stack_adjust/stack_pointer_delta.  */

void
restore_pending_stack_adjust (saved_pending_stack_adjust *save)
{
  if (inhibit_defer_pop == 0)
    {
      pending_stack_adjust = save->x_pending_stack_adjust;
      stack_pointer_delta = save->x_stack_pointer_delta;
    }
}

/* Expand conditional expressions.  */

/* Generate code to evaluate EXP and jump to LABEL if the value is zero.  */

void
jumpifnot (tree exp, rtx_code_label *label, int prob)
{
  do_jump (exp, label, NULL, inv (prob));
}

void
jumpifnot_1 (enum tree_code code, tree op0, tree op1, rtx_code_label *label,
	     int prob)
{
  do_jump_1 (code, op0, op1, label, NULL, inv (prob));
}

/* Generate code to evaluate EXP and jump to LABEL if the value is nonzero.  */

void
jumpif (tree exp, rtx_code_label *label, int prob)
{
  do_jump (exp, NULL, label, prob);
}

void
jumpif_1 (enum tree_code code, tree op0, tree op1,
	  rtx_code_label *label, int prob)
{
  do_jump_1 (code, op0, op1, NULL, label, prob);
}

/* Used internally by prefer_and_bit_test.  */

static GTY(()) rtx and_reg;
static GTY(()) rtx and_test;
static GTY(()) rtx shift_test;

/* Compare the relative costs of "(X & (1 << BITNUM))" and "(X >> BITNUM) & 1",
   where X is an arbitrary register of mode MODE.  Return true if the former
   is preferred.  */

static bool
prefer_and_bit_test (machine_mode mode, int bitnum)
{
  bool speed_p;
  wide_int mask = wi::set_bit_in_zero (bitnum, GET_MODE_PRECISION (mode));

  if (and_test == 0)
    {
      /* Set up rtxes for the two variations.  Use NULL as a placeholder
	 for the BITNUM-based constants.  */
      and_reg = gen_rtx_REG (mode, LAST_VIRTUAL_REGISTER + 1);
      and_test = gen_rtx_AND (mode, and_reg, NULL);
      shift_test = gen_rtx_AND (mode, gen_rtx_ASHIFTRT (mode, and_reg, NULL),
				const1_rtx);
    }
  else
    {
      /* Change the mode of the previously-created rtxes.  */
      PUT_MODE (and_reg, mode);
      PUT_MODE (and_test, mode);
      PUT_MODE (shift_test, mode);
      PUT_MODE (XEXP (shift_test, 0), mode);
    }

  /* Fill in the integers.  */
  XEXP (and_test, 1) = immed_wide_int_const (mask, mode);
  XEXP (XEXP (shift_test, 0), 1) = GEN_INT (bitnum);

  speed_p = optimize_insn_for_speed_p ();
  return (rtx_cost (and_test, mode, IF_THEN_ELSE, 0, speed_p)
	  <= rtx_cost (shift_test, mode, IF_THEN_ELSE, 0, speed_p));
}

/* Subroutine of do_jump, dealing with exploded comparisons of the type
   OP0 CODE OP1 .  IF_FALSE_LABEL and IF_TRUE_LABEL like in do_jump.
   PROB is probability of jump to if_true_label, or -1 if unknown.  */

void
do_jump_1 (enum tree_code code, tree op0, tree op1,
	   rtx_code_label *if_false_label, rtx_code_label *if_true_label,
	   int prob)
{
  machine_mode mode;
  rtx_code_label *drop_through_label = 0;

  switch (code)
    {
    case EQ_EXPR:
      {
        tree inner_type = TREE_TYPE (op0);

        gcc_assert (GET_MODE_CLASS (TYPE_MODE (inner_type))
		    != MODE_COMPLEX_FLOAT);
	gcc_assert (GET_MODE_CLASS (TYPE_MODE (inner_type))
		    != MODE_COMPLEX_INT);

        if (integer_zerop (op1))
	  do_jump (op0, if_true_label, if_false_label, inv (prob));
        else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_INT
                 && !can_compare_p (EQ, TYPE_MODE (inner_type), ccp_jump))
	  do_jump_by_parts_equality (op0, op1, if_false_label, if_true_label,
				     prob);
        else
	  do_compare_and_jump (op0, op1, EQ, EQ, if_false_label, if_true_label,
			       prob);
        break;
      }

    case NE_EXPR:
      {
        tree inner_type = TREE_TYPE (op0);

        gcc_assert (GET_MODE_CLASS (TYPE_MODE (inner_type))
		    != MODE_COMPLEX_FLOAT);
	gcc_assert (GET_MODE_CLASS (TYPE_MODE (inner_type))
		    != MODE_COMPLEX_INT);

        if (integer_zerop (op1))
	  do_jump (op0, if_false_label, if_true_label, prob);
        else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_INT
           && !can_compare_p (NE, TYPE_MODE (inner_type), ccp_jump))
	  do_jump_by_parts_equality (op0, op1, if_true_label, if_false_label,
				     inv (prob));
        else
	  do_compare_and_jump (op0, op1, NE, NE, if_false_label, if_true_label,
			       prob);
        break;
      }

    case LT_EXPR:
      mode = TYPE_MODE (TREE_TYPE (op0));
      if (GET_MODE_CLASS (mode) == MODE_INT
          && ! can_compare_p (LT, mode, ccp_jump))
	do_jump_by_parts_greater (op0, op1, 1, if_false_label, if_true_label,
				  prob);
      else
	do_compare_and_jump (op0, op1, LT, LTU, if_false_label, if_true_label,
			     prob);
      break;

    case LE_EXPR:
      mode = TYPE_MODE (TREE_TYPE (op0));
      if (GET_MODE_CLASS (mode) == MODE_INT
          && ! can_compare_p (LE, mode, ccp_jump))
	do_jump_by_parts_greater (op0, op1, 0, if_true_label, if_false_label,
				  inv (prob));
      else
	do_compare_and_jump (op0, op1, LE, LEU, if_false_label, if_true_label,
			     prob);
      break;

    case GT_EXPR:
      mode = TYPE_MODE (TREE_TYPE (op0));
      if (GET_MODE_CLASS (mode) == MODE_INT
          && ! can_compare_p (GT, mode, ccp_jump))
	do_jump_by_parts_greater (op0, op1, 0, if_false_label, if_true_label,
				  prob);
      else
	do_compare_and_jump (op0, op1, GT, GTU, if_false_label, if_true_label,
			     prob);
      break;

    case GE_EXPR:
      mode = TYPE_MODE (TREE_TYPE (op0));
      if (GET_MODE_CLASS (mode) == MODE_INT
          && ! can_compare_p (GE, mode, ccp_jump))
	do_jump_by_parts_greater (op0, op1, 1, if_true_label, if_false_label,
				  inv (prob));
      else
	do_compare_and_jump (op0, op1, GE, GEU, if_false_label, if_true_label,
			     prob);
      break;

    case ORDERED_EXPR:
      do_compare_and_jump (op0, op1, ORDERED, ORDERED,
			   if_false_label, if_true_label, prob);
      break;

    case UNORDERED_EXPR:
      do_compare_and_jump (op0, op1, UNORDERED, UNORDERED,
			   if_false_label, if_true_label, prob);
      break;

    case UNLT_EXPR:
      do_compare_and_jump (op0, op1, UNLT, UNLT, if_false_label, if_true_label,
			   prob);
      break;

    case UNLE_EXPR:
      do_compare_and_jump (op0, op1, UNLE, UNLE, if_false_label, if_true_label,
			   prob);
      break;

    case UNGT_EXPR:
      do_compare_and_jump (op0, op1, UNGT, UNGT, if_false_label, if_true_label,
			   prob);
      break;

    case UNGE_EXPR:
      do_compare_and_jump (op0, op1, UNGE, UNGE, if_false_label, if_true_label,
			   prob);
      break;

    case UNEQ_EXPR:
      do_compare_and_jump (op0, op1, UNEQ, UNEQ, if_false_label, if_true_label,
			   prob);
      break;

    case LTGT_EXPR:
      do_compare_and_jump (op0, op1, LTGT, LTGT, if_false_label, if_true_label,
			   prob);
      break;

    case TRUTH_ANDIF_EXPR:
      {
        /* Spread the probability that the expression is false evenly between
           the two conditions. So the first condition is false half the total
           probability of being false. The second condition is false the other
           half of the total probability of being false, so its jump has a false
           probability of half the total, relative to the probability we
           reached it (i.e. the first condition was true).  */
        int op0_prob = -1;
        int op1_prob = -1;
        if (prob != -1)
          {
            int false_prob = inv (prob);
            int op0_false_prob = false_prob / 2;
            int op1_false_prob = GCOV_COMPUTE_SCALE ((false_prob / 2),
                                                     inv (op0_false_prob));
            /* Get the probability that each jump below is true.  */
            op0_prob = inv (op0_false_prob);
            op1_prob = inv (op1_false_prob);
          }
	if (if_false_label == NULL)
          {
            drop_through_label = gen_label_rtx ();
	    do_jump (op0, drop_through_label, NULL, op0_prob);
	    do_jump (op1, NULL, if_true_label, op1_prob);
          }
        else
          {
	    do_jump (op0, if_false_label, NULL, op0_prob);
            do_jump (op1, if_false_label, if_true_label, op1_prob);
          }
        break;
      }

    case TRUTH_ORIF_EXPR:
      {
        /* Spread the probability evenly between the two conditions. So
           the first condition has half the total probability of being true.
           The second condition has the other half of the total probability,
           so its jump has a probability of half the total, relative to
           the probability we reached it (i.e. the first condition was false).  */
        int op0_prob = -1;
        int op1_prob = -1;
        if (prob != -1)
          {
            op0_prob = prob / 2;
            op1_prob = GCOV_COMPUTE_SCALE ((prob / 2), inv (op0_prob));
	  }
	if (if_true_label == NULL)
	  {
	    drop_through_label = gen_label_rtx ();
	    do_jump (op0, NULL, drop_through_label, op0_prob);
	    do_jump (op1, if_false_label, NULL, op1_prob);
	  }
	else
	  {
	    do_jump (op0, NULL, if_true_label, op0_prob);
	    do_jump (op1, if_false_label, if_true_label, op1_prob);
	  }
        break;
      }

    default:
      gcc_unreachable ();
    }

  if (drop_through_label)
    {
      do_pending_stack_adjust ();
      emit_label (drop_through_label);
    }
}

/* Generate code to evaluate EXP and jump to IF_FALSE_LABEL if
   the result is zero, or IF_TRUE_LABEL if the result is one.
   Either of IF_FALSE_LABEL and IF_TRUE_LABEL may be zero,
   meaning fall through in that case.

   do_jump always does any pending stack adjust except when it does not
   actually perform a jump.  An example where there is no jump
   is when EXP is `(foo (), 0)' and IF_FALSE_LABEL is null.

   PROB is probability of jump to if_true_label, or -1 if unknown.  */

void
do_jump (tree exp, rtx_code_label *if_false_label,
	 rtx_code_label *if_true_label, int prob)
{
  enum tree_code code = TREE_CODE (exp);
  rtx temp;
  int i;
  tree type;
  machine_mode mode;
  rtx_code_label *drop_through_label = NULL;

  switch (code)
    {
    case ERROR_MARK:
      break;

    case INTEGER_CST:
      {
	rtx_code_label *lab = integer_zerop (exp) ? if_false_label
						  : if_true_label;
	if (lab)
	  emit_jump (lab);
	break;
      }

#if 0
      /* This is not true with #pragma weak  */
    case ADDR_EXPR:
      /* The address of something can never be zero.  */
      if (if_true_label)
        emit_jump (if_true_label);
      break;
#endif

    case NOP_EXPR:
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == COMPONENT_REF
          || TREE_CODE (TREE_OPERAND (exp, 0)) == BIT_FIELD_REF
          || TREE_CODE (TREE_OPERAND (exp, 0)) == ARRAY_REF
          || TREE_CODE (TREE_OPERAND (exp, 0)) == ARRAY_RANGE_REF)
        goto normal;
      /* FALLTHRU */
    case CONVERT_EXPR:
      /* If we are narrowing the operand, we have to do the compare in the
         narrower mode.  */
      if ((TYPE_PRECISION (TREE_TYPE (exp))
           < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp, 0)))))
        goto normal;
      /* FALLTHRU */
    case NON_LVALUE_EXPR:
    case ABS_EXPR:
    case NEGATE_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These cannot change zero->nonzero or vice versa.  */
      do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label, prob);
      break;

    case TRUTH_NOT_EXPR:
      do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label,
	       inv (prob));
      break;

    case COND_EXPR:
      {
	rtx_code_label *label1 = gen_label_rtx ();
	if (!if_true_label || !if_false_label)
	  {
	    drop_through_label = gen_label_rtx ();
	    if (!if_true_label)
	      if_true_label = drop_through_label;
	    if (!if_false_label)
	      if_false_label = drop_through_label;
	  }

        do_pending_stack_adjust ();
	do_jump (TREE_OPERAND (exp, 0), label1, NULL, -1);
	do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label, prob);
        emit_label (label1);
	do_jump (TREE_OPERAND (exp, 2), if_false_label, if_true_label, prob);
	break;
      }

    case COMPOUND_EXPR:
      /* Lowered by gimplify.c.  */
      gcc_unreachable ();

    case MINUS_EXPR:
      /* Nonzero iff operands of minus differ.  */
      code = NE_EXPR;

      /* FALLTHRU */
    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    other_code:
      do_jump_1 (code, TREE_OPERAND (exp, 0), TREE_OPERAND (exp, 1),
		 if_false_label, if_true_label, prob);
      break;

    case BIT_AND_EXPR:
      /* fold_single_bit_test() converts (X & (1 << C)) into (X >> C) & 1.
	 See if the former is preferred for jump tests and restore it
	 if so.  */
      if (integer_onep (TREE_OPERAND (exp, 1)))
	{
	  tree exp0 = TREE_OPERAND (exp, 0);
	  rtx_code_label *set_label, *clr_label;
	  int setclr_prob = prob;

	  /* Strip narrowing integral type conversions.  */
	  while (CONVERT_EXPR_P (exp0)
		 && TREE_OPERAND (exp0, 0) != error_mark_node
		 && TYPE_PRECISION (TREE_TYPE (exp0))
		    <= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp0, 0))))
	    exp0 = TREE_OPERAND (exp0, 0);

	  /* "exp0 ^ 1" inverts the sense of the single bit test.  */
	  if (TREE_CODE (exp0) == BIT_XOR_EXPR
	      && integer_onep (TREE_OPERAND (exp0, 1)))
	    {
	      exp0 = TREE_OPERAND (exp0, 0);
	      clr_label = if_true_label;
	      set_label = if_false_label;
	      setclr_prob = inv (prob);
	    }
	  else
	    {
	      clr_label = if_false_label;
	      set_label = if_true_label;
	    }

	  if (TREE_CODE (exp0) == RSHIFT_EXPR)
	    {
	      tree arg = TREE_OPERAND (exp0, 0);
	      tree shift = TREE_OPERAND (exp0, 1);
	      tree argtype = TREE_TYPE (arg);
	      if (TREE_CODE (shift) == INTEGER_CST
		  && compare_tree_int (shift, 0) >= 0
		  && compare_tree_int (shift, HOST_BITS_PER_WIDE_INT) < 0
		  && prefer_and_bit_test (TYPE_MODE (argtype),
					  TREE_INT_CST_LOW (shift)))
		{
		  unsigned HOST_WIDE_INT mask
		    = HOST_WIDE_INT_1U << TREE_INT_CST_LOW (shift);
		  do_jump (build2 (BIT_AND_EXPR, argtype, arg,
				   build_int_cstu (argtype, mask)),
			   clr_label, set_label, setclr_prob);
		  break;
		}
	    }
	}

      /* If we are AND'ing with a small constant, do this comparison in the
         smallest type that fits.  If the machine doesn't have comparisons
         that small, it will be converted back to the wider comparison.
         This helps if we are testing the sign bit of a narrower object.
         combine can't do this for us because it can't know whether a
         ZERO_EXTRACT or a compare in a smaller mode exists, but we do.  */

      if (! SLOW_BYTE_ACCESS
          && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
          && TYPE_PRECISION (TREE_TYPE (exp)) <= HOST_BITS_PER_WIDE_INT
          && (i = tree_floor_log2 (TREE_OPERAND (exp, 1))) >= 0
          && (mode = mode_for_size (i + 1, MODE_INT, 0)) != BLKmode
          && (type = lang_hooks.types.type_for_mode (mode, 1)) != 0
          && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (exp))
          && have_insn_for (COMPARE, TYPE_MODE (type)))
        {
	  do_jump (fold_convert (type, exp), if_false_label, if_true_label,
		   prob);
          break;
        }

      if (TYPE_PRECISION (TREE_TYPE (exp)) > 1
	  || TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	goto normal;

      /* Boolean comparisons can be compiled as TRUTH_AND_EXPR.  */
      /* FALLTHRU */

    case TRUTH_AND_EXPR:
      /* High branch cost, expand as the bitwise AND of the conditions.
	 Do the same if the RHS has side effects, because we're effectively
	 turning a TRUTH_AND_EXPR into a TRUTH_ANDIF_EXPR.  */
      if (BRANCH_COST (optimize_insn_for_speed_p (),
		       false) >= 4
	  || TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 1)))
	goto normal;
      code = TRUTH_ANDIF_EXPR;
      goto other_code;

    case BIT_IOR_EXPR:
    case TRUTH_OR_EXPR:
      /* High branch cost, expand as the bitwise OR of the conditions.
	 Do the same if the RHS has side effects, because we're effectively
	 turning a TRUTH_OR_EXPR into a TRUTH_ORIF_EXPR.  */
      if (BRANCH_COST (optimize_insn_for_speed_p (), false) >= 4
	  || TREE_SIDE_EFFECTS (TREE_OPERAND (exp, 1)))
	goto normal;
      code = TRUTH_ORIF_EXPR;
      goto other_code;

      /* Fall through and generate the normal code.  */
    default:
    normal:
      temp = expand_normal (exp);
      do_pending_stack_adjust ();
      /* The RTL optimizers prefer comparisons against pseudos.  */
      if (GET_CODE (temp) == SUBREG)
	{
	  /* Compare promoted variables in their promoted mode.  */
	  if (SUBREG_PROMOTED_VAR_P (temp)
	      && REG_P (XEXP (temp, 0)))
	    temp = XEXP (temp, 0);
	  else
	    temp = copy_to_reg (temp);
	}
      do_compare_rtx_and_jump (temp, CONST0_RTX (GET_MODE (temp)),
			       NE, TYPE_UNSIGNED (TREE_TYPE (exp)),
			       GET_MODE (temp), NULL_RTX,
			       if_false_label, if_true_label, prob);
    }

  if (drop_through_label)
    {
      do_pending_stack_adjust ();
      emit_label (drop_through_label);
    }
}

/* Compare OP0 with OP1, word at a time, in mode MODE.
   UNSIGNEDP says to do unsigned comparison.
   Jump to IF_TRUE_LABEL if OP0 is greater, IF_FALSE_LABEL otherwise.  */

static void
do_jump_by_parts_greater_rtx (machine_mode mode, int unsignedp, rtx op0,
			      rtx op1, rtx_code_label *if_false_label,
			      rtx_code_label *if_true_label,
			      int prob)
{
  int nwords = (GET_MODE_SIZE (mode) / UNITS_PER_WORD);
  rtx_code_label *drop_through_label = 0;
  bool drop_through_if_true = false, drop_through_if_false = false;
  enum rtx_code code = GT;
  int i;

  if (! if_true_label || ! if_false_label)
    drop_through_label = gen_label_rtx ();
  if (! if_true_label)
    {
      if_true_label = drop_through_label;
      drop_through_if_true = true;
    }
  if (! if_false_label)
    {
      if_false_label = drop_through_label;
      drop_through_if_false = true;
    }

  /* Deal with the special case 0 > x: only one comparison is necessary and
     we reverse it to avoid jumping to the drop-through label.  */
  if (op0 == const0_rtx && drop_through_if_true && !drop_through_if_false)
    {
      code = LE;
      if_true_label = if_false_label;
      if_false_label = drop_through_label;
      drop_through_if_true = false;
      drop_through_if_false = true;
      prob = inv (prob);
    }

  /* Compare a word at a time, high order first.  */
  for (i = 0; i < nwords; i++)
    {
      rtx op0_word, op1_word;

      if (WORDS_BIG_ENDIAN)
        {
          op0_word = operand_subword_force (op0, i, mode);
          op1_word = operand_subword_force (op1, i, mode);
        }
      else
        {
          op0_word = operand_subword_force (op0, nwords - 1 - i, mode);
          op1_word = operand_subword_force (op1, nwords - 1 - i, mode);
        }

      /* All but high-order word must be compared as unsigned.  */
      do_compare_rtx_and_jump (op0_word, op1_word, code, (unsignedp || i > 0),
			       word_mode, NULL_RTX, NULL, if_true_label,
			       prob);

      /* Emit only one comparison for 0.  Do not emit the last cond jump.  */
      if (op0 == const0_rtx || i == nwords - 1)
	break;

      /* Consider lower words only if these are equal.  */
      do_compare_rtx_and_jump (op0_word, op1_word, NE, unsignedp, word_mode,
			       NULL_RTX, NULL, if_false_label, inv (prob));
    }

  if (!drop_through_if_false)
    emit_jump (if_false_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Given a comparison expression EXP for values too wide to be compared
   with one insn, test the comparison and jump to the appropriate label.
   The code of EXP is ignored; we always test GT if SWAP is 0,
   and LT if SWAP is 1.  */

static void
do_jump_by_parts_greater (tree treeop0, tree treeop1, int swap,
			  rtx_code_label *if_false_label,
			  rtx_code_label *if_true_label, int prob)
{
  rtx op0 = expand_normal (swap ? treeop1 : treeop0);
  rtx op1 = expand_normal (swap ? treeop0 : treeop1);
  machine_mode mode = TYPE_MODE (TREE_TYPE (treeop0));
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (treeop0));

  do_jump_by_parts_greater_rtx (mode, unsignedp, op0, op1, if_false_label,
				if_true_label, prob);
}

/* Jump according to whether OP0 is 0.  We assume that OP0 has an integer
   mode, MODE, that is too wide for the available compare insns.  Either
   Either (but not both) of IF_TRUE_LABEL and IF_FALSE_LABEL may be NULL
   to indicate drop through.  */

static void
do_jump_by_parts_zero_rtx (machine_mode mode, rtx op0,
			   rtx_code_label *if_false_label,
			   rtx_code_label *if_true_label, int prob)
{
  int nwords = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
  rtx part;
  int i;
  rtx_code_label *drop_through_label = NULL;

  /* The fastest way of doing this comparison on almost any machine is to
     "or" all the words and compare the result.  If all have to be loaded
     from memory and this is a very wide item, it's possible this may
     be slower, but that's highly unlikely.  */

  part = gen_reg_rtx (word_mode);
  emit_move_insn (part, operand_subword_force (op0, 0, mode));
  for (i = 1; i < nwords && part != 0; i++)
    part = expand_binop (word_mode, ior_optab, part,
                         operand_subword_force (op0, i, mode),
                         part, 1, OPTAB_WIDEN);

  if (part != 0)
    {
      do_compare_rtx_and_jump (part, const0_rtx, EQ, 1, word_mode,
			       NULL_RTX, if_false_label, if_true_label, prob);
      return;
    }

  /* If we couldn't do the "or" simply, do this with a series of compares.  */
  if (! if_false_label)
    if_false_label = drop_through_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    do_compare_rtx_and_jump (operand_subword_force (op0, i, mode),
                             const0_rtx, EQ, 1, word_mode, NULL_RTX,
			     if_false_label, NULL, prob);

  if (if_true_label)
    emit_jump (if_true_label);

  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Test for the equality of two RTX expressions OP0 and OP1 in mode MODE,
   where MODE is an integer mode too wide to be compared with one insn.
   Either (but not both) of IF_TRUE_LABEL and IF_FALSE_LABEL may be NULL_RTX
   to indicate drop through.  */

static void
do_jump_by_parts_equality_rtx (machine_mode mode, rtx op0, rtx op1,
			       rtx_code_label *if_false_label,
			       rtx_code_label *if_true_label, int prob)
{
  int nwords = (GET_MODE_SIZE (mode) / UNITS_PER_WORD);
  rtx_code_label *drop_through_label = NULL;
  int i;

  if (op1 == const0_rtx)
    {
      do_jump_by_parts_zero_rtx (mode, op0, if_false_label, if_true_label,
				 prob);
      return;
    }
  else if (op0 == const0_rtx)
    {
      do_jump_by_parts_zero_rtx (mode, op1, if_false_label, if_true_label,
				 prob);
      return;
    }

  if (! if_false_label)
    drop_through_label = if_false_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    do_compare_rtx_and_jump (operand_subword_force (op0, i, mode),
                             operand_subword_force (op1, i, mode),
                             EQ, 0, word_mode, NULL_RTX,
			     if_false_label, NULL, prob);

  if (if_true_label)
    emit_jump (if_true_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Given an EQ_EXPR expression EXP for values too wide to be compared
   with one insn, test the comparison and jump to the appropriate label.  */

static void
do_jump_by_parts_equality (tree treeop0, tree treeop1,
			   rtx_code_label *if_false_label,
			   rtx_code_label *if_true_label, int prob)
{
  rtx op0 = expand_normal (treeop0);
  rtx op1 = expand_normal (treeop1);
  machine_mode mode = TYPE_MODE (TREE_TYPE (treeop0));
  do_jump_by_parts_equality_rtx (mode, op0, op1, if_false_label,
				 if_true_label, prob);
}

/* Split a comparison into two others, the second of which has the other
   "orderedness".  The first is always ORDERED or UNORDERED if MODE
   does not honor NaNs (which means that it can be skipped in that case;
   see do_compare_rtx_and_jump).

   The two conditions are written in *CODE1 and *CODE2.  Return true if
   the conditions must be ANDed, false if they must be ORed.  */

bool
split_comparison (enum rtx_code code, machine_mode mode,
		  enum rtx_code *code1, enum rtx_code *code2)
{
  switch (code)
    {
    case LT:
      *code1 = ORDERED;
      *code2 = UNLT;
      return true;
    case LE:
      *code1 = ORDERED;
      *code2 = UNLE;
      return true;
    case GT:
      *code1 = ORDERED;
      *code2 = UNGT;
      return true;
    case GE:
      *code1 = ORDERED;
      *code2 = UNGE;
      return true;
    case EQ:
      *code1 = ORDERED;
      *code2 = UNEQ;
      return true;
    case NE:
      *code1 = UNORDERED;
      *code2 = LTGT;
      return false;
    case UNLT:
      *code1 = UNORDERED;
      *code2 = LT;
      return false;
    case UNLE:
      *code1 = UNORDERED;
      *code2 = LE;
      return false;
    case UNGT:
      *code1 = UNORDERED;
      *code2 = GT;
      return false;
    case UNGE:
      *code1 = UNORDERED;
      *code2 = GE;
      return false;
    case UNEQ:
      *code1 = UNORDERED;
      *code2 = EQ;
      return false;
    case LTGT:
      /* Do not turn a trapping comparison into a non-trapping one.  */
      if (HONOR_SNANS (mode))
	{
          *code1 = LT;
          *code2 = GT;
          return false;
	}
      else
	{
	  *code1 = ORDERED;
	  *code2 = NE;
	  return true;
	}
    default:
      gcc_unreachable ();
    }
}


/* Like do_compare_and_jump but expects the values to compare as two rtx's.
   The decision as to signed or unsigned comparison must be made by the caller.

   If MODE is BLKmode, SIZE is an RTX giving the size of the objects being
   compared.  */

void
do_compare_rtx_and_jump (rtx op0, rtx op1, enum rtx_code code, int unsignedp,
			 machine_mode mode, rtx size,
			 rtx_code_label *if_false_label,
			 rtx_code_label *if_true_label, int prob)
{
  rtx tem;
  rtx_code_label *dummy_label = NULL;

  /* Reverse the comparison if that is safe and we want to jump if it is
     false.  Also convert to the reverse comparison if the target can
     implement it.  */
  if ((! if_true_label
       || ! can_compare_p (code, mode, ccp_jump))
      && (! FLOAT_MODE_P (mode)
	  || code == ORDERED || code == UNORDERED
	  || (! HONOR_NANS (mode) && (code == LTGT || code == UNEQ))
	  || (! HONOR_SNANS (mode) && (code == EQ || code == NE))))
    {
      enum rtx_code rcode;
      if (FLOAT_MODE_P (mode))
        rcode = reverse_condition_maybe_unordered (code);
      else
        rcode = reverse_condition (code);

      /* Canonicalize to UNORDERED for the libcall.  */
      if (can_compare_p (rcode, mode, ccp_jump)
	  || (code == ORDERED && ! can_compare_p (ORDERED, mode, ccp_jump)))
	{
	  std::swap (if_true_label, if_false_label);
	  code = rcode;
	  prob = inv (prob);
	}
    }

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  do_pending_stack_adjust ();

  code = unsignedp ? unsigned_condition (code) : code;
  if (0 != (tem = simplify_relational_operation (code, mode, VOIDmode,
						 op0, op1)))
    {
      if (CONSTANT_P (tem))
	{
	  rtx_code_label *label = (tem == const0_rtx
				   || tem == CONST0_RTX (mode))
					? if_false_label : if_true_label;
	  if (label)
	    emit_jump (label);
	  return;
	}

      code = GET_CODE (tem);
      mode = GET_MODE (tem);
      op0 = XEXP (tem, 0);
      op1 = XEXP (tem, 1);
      unsignedp = (code == GTU || code == LTU || code == GEU || code == LEU);
    }

  if (! if_true_label)
    dummy_label = if_true_label = gen_label_rtx ();

  if (GET_MODE_CLASS (mode) == MODE_INT
      && ! can_compare_p (code, mode, ccp_jump))
    {
      switch (code)
	{
	case LTU:
	  do_jump_by_parts_greater_rtx (mode, 1, op1, op0,
					if_false_label, if_true_label, prob);
	  break;

	case LEU:
	  do_jump_by_parts_greater_rtx (mode, 1, op0, op1,
					if_true_label, if_false_label,
					inv (prob));
	  break;

	case GTU:
	  do_jump_by_parts_greater_rtx (mode, 1, op0, op1,
					if_false_label, if_true_label, prob);
	  break;

	case GEU:
	  do_jump_by_parts_greater_rtx (mode, 1, op1, op0,
					if_true_label, if_false_label,
					inv (prob));
	  break;

	case LT:
	  do_jump_by_parts_greater_rtx (mode, 0, op1, op0,
					if_false_label, if_true_label, prob);
	  break;

	case LE:
	  do_jump_by_parts_greater_rtx (mode, 0, op0, op1,
					if_true_label, if_false_label,
					inv (prob));
	  break;

	case GT:
	  do_jump_by_parts_greater_rtx (mode, 0, op0, op1,
					if_false_label, if_true_label, prob);
	  break;

	case GE:
	  do_jump_by_parts_greater_rtx (mode, 0, op1, op0,
					if_true_label, if_false_label,
					inv (prob));
	  break;

	case EQ:
	  do_jump_by_parts_equality_rtx (mode, op0, op1, if_false_label,
					 if_true_label, prob);
	  break;

	case NE:
	  do_jump_by_parts_equality_rtx (mode, op0, op1, if_true_label,
					 if_false_label, inv (prob));
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      if (SCALAR_FLOAT_MODE_P (mode)
	  && ! can_compare_p (code, mode, ccp_jump)
	  && can_compare_p (swap_condition (code), mode, ccp_jump))
	{
	  code = swap_condition (code);
	  std::swap (op0, op1);
	}
      else if (SCALAR_FLOAT_MODE_P (mode)
	       && ! can_compare_p (code, mode, ccp_jump)
	       /* Never split ORDERED and UNORDERED.
		  These must be implemented.  */
	       && (code != ORDERED && code != UNORDERED)
               /* Split a floating-point comparison if
		  we can jump on other conditions...  */
	       && (have_insn_for (COMPARE, mode)
	           /* ... or if there is no libcall for it.  */
	           || code_to_optab (code) == unknown_optab))
        {
	  enum rtx_code first_code;
	  bool and_them = split_comparison (code, mode, &first_code, &code);

	  /* If there are no NaNs, the first comparison should always fall
	     through.  */
	  if (!HONOR_NANS (mode))
	    gcc_assert (first_code == (and_them ? ORDERED : UNORDERED));

	  else
	    {
	      int first_prob = prob;
	      if (first_code == UNORDERED)
		first_prob = REG_BR_PROB_BASE / 100;
	      else if (first_code == ORDERED)
		first_prob = REG_BR_PROB_BASE - REG_BR_PROB_BASE / 100;
	      if (and_them)
		{
		  rtx_code_label *dest_label;
		  /* If we only jump if true, just bypass the second jump.  */
		  if (! if_false_label)
		    {
		      if (! dummy_label)
		        dummy_label = gen_label_rtx ();
		      dest_label = dummy_label;
		    }
		  else
		    dest_label = if_false_label;
                  do_compare_rtx_and_jump (op0, op1, first_code, unsignedp, mode,
					   size, dest_label, NULL, first_prob);
		}
              else
                do_compare_rtx_and_jump (op0, op1, first_code, unsignedp, mode,
					 size, NULL, if_true_label, first_prob);
	    }
	}

      emit_cmp_and_jump_insns (op0, op1, code, size, mode, unsignedp,
			       if_true_label, prob);
    }

  if (if_false_label)
    emit_jump (if_false_label);
  if (dummy_label)
    emit_label (dummy_label);
}

/* Generate code for a comparison expression EXP (including code to compute
   the values to be compared) and a conditional jump to IF_FALSE_LABEL and/or
   IF_TRUE_LABEL.  One of the labels can be NULL_RTX, in which case the
   generated code will drop through.
   SIGNED_CODE should be the rtx operation for this comparison for
   signed data; UNSIGNED_CODE, likewise for use if data is unsigned.

   We force a stack adjustment unless there are currently
   things pushed on the stack that aren't yet used.  */

static void
do_compare_and_jump (tree treeop0, tree treeop1, enum rtx_code signed_code,
		     enum rtx_code unsigned_code,
		     rtx_code_label *if_false_label,
		     rtx_code_label *if_true_label, int prob)
{
  rtx op0, op1;
  tree type;
  machine_mode mode;
  int unsignedp;
  enum rtx_code code;

  /* Don't crash if the comparison was erroneous.  */
  op0 = expand_normal (treeop0);
  if (TREE_CODE (treeop0) == ERROR_MARK)
    return;

  op1 = expand_normal (treeop1);
  if (TREE_CODE (treeop1) == ERROR_MARK)
    return;

  type = TREE_TYPE (treeop0);
  mode = TYPE_MODE (type);
  if (TREE_CODE (treeop0) == INTEGER_CST
      && (TREE_CODE (treeop1) != INTEGER_CST
          || (GET_MODE_BITSIZE (mode)
              > GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (treeop1))))))
    {
      /* op0 might have been replaced by promoted constant, in which
         case the type of second argument should be used.  */
      type = TREE_TYPE (treeop1);
      mode = TYPE_MODE (type);
    }
  unsignedp = TYPE_UNSIGNED (type);
  code = unsignedp ? unsigned_code : signed_code;

  /* If function pointers need to be "canonicalized" before they can
     be reliably compared, then canonicalize them.
     Only do this if *both* sides of the comparison are function pointers.
     If one side isn't, we want a noncanonicalized comparison.  See PR
     middle-end/17564.  */
  if (targetm.have_canonicalize_funcptr_for_compare ()
      && POINTER_TYPE_P (TREE_TYPE (treeop0))
      && POINTER_TYPE_P (TREE_TYPE (treeop1))
      && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (TREE_TYPE (treeop0)))
      && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (TREE_TYPE (treeop1))))
    {
      rtx new_op0 = gen_reg_rtx (mode);
      rtx new_op1 = gen_reg_rtx (mode);

      emit_insn (targetm.gen_canonicalize_funcptr_for_compare (new_op0, op0));
      op0 = new_op0;

      emit_insn (targetm.gen_canonicalize_funcptr_for_compare (new_op1, op1));
      op1 = new_op1;
    }

  do_compare_rtx_and_jump (op0, op1, code, unsignedp, mode,
                           ((mode == BLKmode)
                            ? expr_size (treeop0) : NULL_RTX),
			   if_false_label, if_true_label, prob);
}

#include "gt-dojump.h"
