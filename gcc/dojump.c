/* Convert tree expression to rtl instructions, for GNU compiler.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "insn-config.h"
#include "insn-attr.h"
/* Include expr.h after insn-config.h so we get HAVE_conditional_move.  */
#include "expr.h"
#include "optabs.h"
#include "langhooks.h"

static void do_jump_by_parts_greater (tree, int, rtx, rtx);
static void do_jump_by_parts_equality (tree, rtx, rtx);
static void do_compare_and_jump	(tree, enum rtx_code, enum rtx_code, rtx,
				 rtx);

/* At the start of a function, record that we have no previously-pushed
   arguments waiting to be popped.  */

void
init_pending_stack_adjust (void)
{
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
      && (! flag_omit_frame_pointer || current_function_calls_alloca)
      && EXIT_IGNORE_STACK
      && ! (DECL_INLINE (current_function_decl) && ! flag_no_inline)
      && ! flag_inline_functions)
    {
      stack_pointer_delta -= pending_stack_adjust,
      pending_stack_adjust = 0;
    }
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

/* Expand conditional expressions.  */

/* Generate code to evaluate EXP and jump to LABEL if the value is zero.
   LABEL is an rtx of code CODE_LABEL, in this function and all the
   functions here.  */

void
jumpifnot (tree exp, rtx label)
{
  do_jump (exp, label, NULL_RTX);
}

/* Generate code to evaluate EXP and jump to LABEL if the value is nonzero.  */

void
jumpif (tree exp, rtx label)
{
  do_jump (exp, NULL_RTX, label);
}

/* Generate code to evaluate EXP and jump to IF_FALSE_LABEL if
   the result is zero, or IF_TRUE_LABEL if the result is one.
   Either of IF_FALSE_LABEL and IF_TRUE_LABEL may be zero,
   meaning fall through in that case.

   do_jump always does any pending stack adjust except when it does not
   actually perform a jump.  An example where there is no jump
   is when EXP is `(foo (), 0)' and IF_FALSE_LABEL is null.

   This function is responsible for optimizing cases such as
   &&, || and comparison operators in EXP.  */

void
do_jump (tree exp, rtx if_false_label, rtx if_true_label)
{
  enum tree_code code = TREE_CODE (exp);
  /* Some cases need to create a label to jump to
     in order to properly fall through.
     These cases set DROP_THROUGH_LABEL nonzero.  */
  rtx drop_through_label = 0;
  rtx temp;
  int i;
  tree type;
  enum machine_mode mode;

  emit_queue ();

  switch (code)
    {
    case ERROR_MARK:
      break;

    case INTEGER_CST:
      temp = integer_zerop (exp) ? if_false_label : if_true_label;
      if (temp)
        emit_jump (temp);
      break;

#if 0
      /* This is not true with #pragma weak  */
    case ADDR_EXPR:
      /* The address of something can never be zero.  */
      if (if_true_label)
        emit_jump (if_true_label);
      break;
#endif

    case UNSAVE_EXPR:
      do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
      TREE_OPERAND (exp, 0)
	= (*lang_hooks.unsave_expr_now) (TREE_OPERAND (exp, 0));
      break;

    case NOP_EXPR:
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == COMPONENT_REF
          || TREE_CODE (TREE_OPERAND (exp, 0)) == BIT_FIELD_REF
          || TREE_CODE (TREE_OPERAND (exp, 0)) == ARRAY_REF
          || TREE_CODE (TREE_OPERAND (exp, 0)) == ARRAY_RANGE_REF)
        goto normal;
    case CONVERT_EXPR:
      /* If we are narrowing the operand, we have to do the compare in the
         narrower mode.  */
      if ((TYPE_PRECISION (TREE_TYPE (exp))
           < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp, 0)))))
        goto normal;
    case NON_LVALUE_EXPR:
    case REFERENCE_EXPR:
    case ABS_EXPR:
    case NEGATE_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These cannot change zero->nonzero or vice versa.  */
      do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
      break;

    case WITH_RECORD_EXPR:
      /* Put the object on the placeholder list, recurse through our first
         operand, and pop the list.  */
      placeholder_list = tree_cons (TREE_OPERAND (exp, 1), NULL_TREE,
                                    placeholder_list);
      do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
      placeholder_list = TREE_CHAIN (placeholder_list);
      break;

#if 0
      /* This is never less insns than evaluating the PLUS_EXPR followed by
         a test and can be longer if the test is eliminated.  */
    case PLUS_EXPR:
      /* Reduce to minus.  */
      exp = build (MINUS_EXPR, TREE_TYPE (exp),
                   TREE_OPERAND (exp, 0),
                   fold (build1 (NEGATE_EXPR, TREE_TYPE (TREE_OPERAND (exp, 1)),
                                 TREE_OPERAND (exp, 1))));
      /* Process as MINUS.  */
#endif

    case MINUS_EXPR:
      /* Nonzero iff operands of minus differ.  */
      do_compare_and_jump (build (NE_EXPR, TREE_TYPE (exp),
                                  TREE_OPERAND (exp, 0),
                                  TREE_OPERAND (exp, 1)),
                           NE, NE, if_false_label, if_true_label);
      break;

    case BIT_AND_EXPR:
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
          && (type = (*lang_hooks.types.type_for_mode) (mode, 1)) != 0
          && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (exp))
          && (cmp_optab->handlers[(int) TYPE_MODE (type)].insn_code
              != CODE_FOR_nothing))
        {
          do_jump (convert (type, exp), if_false_label, if_true_label);
          break;
        }
      goto normal;

    case TRUTH_NOT_EXPR:
      do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);
      break;

    case TRUTH_ANDIF_EXPR:
      if (if_false_label == 0)
        if_false_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), if_false_label, NULL_RTX);
      start_cleanup_deferral ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      end_cleanup_deferral ();
      break;

    case TRUTH_ORIF_EXPR:
      if (if_true_label == 0)
        if_true_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), NULL_RTX, if_true_label);
      start_cleanup_deferral ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      end_cleanup_deferral ();
      break;

    case COMPOUND_EXPR:
      push_temp_slots ();
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      preserve_temp_slots (NULL_RTX);
      free_temp_slots ();
      pop_temp_slots ();
      emit_queue ();
      do_pending_stack_adjust ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      {
        HOST_WIDE_INT bitsize, bitpos;
        int unsignedp;
        enum machine_mode mode;
        tree type;
        tree offset;
        int volatilep = 0;

        /* Get description of this reference.  We don't actually care
           about the underlying object here.  */
        get_inner_reference (exp, &bitsize, &bitpos, &offset, &mode,
                             &unsignedp, &volatilep);

        type = (*lang_hooks.types.type_for_size) (bitsize, unsignedp);
        if (! SLOW_BYTE_ACCESS
            && type != 0 && bitsize >= 0
            && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (exp))
            && (cmp_optab->handlers[(int) TYPE_MODE (type)].insn_code
          != CODE_FOR_nothing))
          {
            do_jump (convert (type, exp), if_false_label, if_true_label);
            break;
          }
        goto normal;
      }

    case COND_EXPR:
      /* Do (a ? 1 : 0) and (a ? 0 : 1) as special cases.  */
      if (integer_onep (TREE_OPERAND (exp, 1))
          && integer_zerop (TREE_OPERAND (exp, 2)))
        do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);

      else if (integer_zerop (TREE_OPERAND (exp, 1))
               && integer_onep (TREE_OPERAND (exp, 2)))
        do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);

      else
      {
        rtx label1 = gen_label_rtx ();
        drop_through_label = gen_label_rtx ();

        do_jump (TREE_OPERAND (exp, 0), label1, NULL_RTX);

        start_cleanup_deferral ();
        /* Now the THEN-expression.  */
        do_jump (TREE_OPERAND (exp, 1),
                 if_false_label ? if_false_label : drop_through_label,
                 if_true_label ? if_true_label : drop_through_label);
        /* In case the do_jump just above never jumps.  */
        do_pending_stack_adjust ();
        emit_label (label1);

        /* Now the ELSE-expression.  */
        do_jump (TREE_OPERAND (exp, 2),
           if_false_label ? if_false_label : drop_through_label,
           if_true_label ? if_true_label : drop_through_label);
        end_cleanup_deferral ();
      }
      break;

    case EQ_EXPR:
      {
        tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

        if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_FLOAT
            || GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_INT)
          {
            tree exp0 = save_expr (TREE_OPERAND (exp, 0));
            tree exp1 = save_expr (TREE_OPERAND (exp, 1));
            do_jump
              (fold
               (build (TRUTH_ANDIF_EXPR, TREE_TYPE (exp),
                 fold (build (EQ_EXPR, TREE_TYPE (exp),
                  fold (build1 (REALPART_EXPR,
                    TREE_TYPE (inner_type),
                    exp0)),
                  fold (build1 (REALPART_EXPR,
                    TREE_TYPE (inner_type),
                    exp1)))),
                 fold (build (EQ_EXPR, TREE_TYPE (exp),
                  fold (build1 (IMAGPART_EXPR,
                    TREE_TYPE (inner_type),
                    exp0)),
                  fold (build1 (IMAGPART_EXPR,
                    TREE_TYPE (inner_type),
                    exp1)))))),
               if_false_label, if_true_label);
          }

        else if (integer_zerop (TREE_OPERAND (exp, 1)))
          do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);

        else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_INT
                 && !can_compare_p (EQ, TYPE_MODE (inner_type), ccp_jump))
          do_jump_by_parts_equality (exp, if_false_label, if_true_label);
        else
          do_compare_and_jump (exp, EQ, EQ, if_false_label, if_true_label);
        break;
      }

    case NE_EXPR:
      {
        tree inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

        if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_FLOAT
            || GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_COMPLEX_INT)
          {
            tree exp0 = save_expr (TREE_OPERAND (exp, 0));
            tree exp1 = save_expr (TREE_OPERAND (exp, 1));
            do_jump
              (fold
               (build (TRUTH_ORIF_EXPR, TREE_TYPE (exp),
                 fold (build (NE_EXPR, TREE_TYPE (exp),
                  fold (build1 (REALPART_EXPR,
                    TREE_TYPE (inner_type),
                    exp0)),
                  fold (build1 (REALPART_EXPR,
                    TREE_TYPE (inner_type),
                    exp1)))),
                 fold (build (NE_EXPR, TREE_TYPE (exp),
                    fold (build1 (IMAGPART_EXPR,
                      TREE_TYPE (inner_type),
                      exp0)),
                    fold (build1 (IMAGPART_EXPR,
                      TREE_TYPE (inner_type),
                      exp1)))))),
               if_false_label, if_true_label);
          }

        else if (integer_zerop (TREE_OPERAND (exp, 1)))
          do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);

        else if (GET_MODE_CLASS (TYPE_MODE (inner_type)) == MODE_INT
           && !can_compare_p (NE, TYPE_MODE (inner_type), ccp_jump))
          do_jump_by_parts_equality (exp, if_true_label, if_false_label);
        else
          do_compare_and_jump (exp, NE, NE, if_false_label, if_true_label);
        break;
      }

    case LT_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
          && ! can_compare_p (LT, mode, ccp_jump))
        do_jump_by_parts_greater (exp, 1, if_false_label, if_true_label);
      else
        do_compare_and_jump (exp, LT, LTU, if_false_label, if_true_label);
      break;

    case LE_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
          && ! can_compare_p (LE, mode, ccp_jump))
        do_jump_by_parts_greater (exp, 0, if_true_label, if_false_label);
      else
        do_compare_and_jump (exp, LE, LEU, if_false_label, if_true_label);
      break;

    case GT_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
          && ! can_compare_p (GT, mode, ccp_jump))
        do_jump_by_parts_greater (exp, 0, if_false_label, if_true_label);
      else
        do_compare_and_jump (exp, GT, GTU, if_false_label, if_true_label);
      break;

    case GE_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
      if (GET_MODE_CLASS (mode) == MODE_INT
          && ! can_compare_p (GE, mode, ccp_jump))
        do_jump_by_parts_greater (exp, 1, if_true_label, if_false_label);
      else
        do_compare_and_jump (exp, GE, GEU, if_false_label, if_true_label);
      break;

    case UNORDERED_EXPR:
    case ORDERED_EXPR:
      {
        enum rtx_code cmp, rcmp;
        int do_rev;

        if (code == UNORDERED_EXPR)
          cmp = UNORDERED, rcmp = ORDERED;
        else
          cmp = ORDERED, rcmp = UNORDERED;
        mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));

        do_rev = 0;
        if (! can_compare_p (cmp, mode, ccp_jump)
            && (can_compare_p (rcmp, mode, ccp_jump)
          /* If the target doesn't provide either UNORDERED or ORDERED
             comparisons, canonicalize on UNORDERED for the library.  */
          || rcmp == UNORDERED))
          do_rev = 1;

        if (! do_rev)
          do_compare_and_jump (exp, cmp, cmp, if_false_label, if_true_label);
        else
          do_compare_and_jump (exp, rcmp, rcmp, if_true_label, if_false_label);
      }
      break;

    {
      enum rtx_code rcode1;
      enum tree_code tcode2;

      case UNLT_EXPR:
        rcode1 = UNLT;
        tcode2 = LT_EXPR;
        goto unordered_bcc;
      case UNLE_EXPR:
        rcode1 = UNLE;
        tcode2 = LE_EXPR;
        goto unordered_bcc;
      case UNGT_EXPR:
        rcode1 = UNGT;
        tcode2 = GT_EXPR;
        goto unordered_bcc;
      case UNGE_EXPR:
        rcode1 = UNGE;
        tcode2 = GE_EXPR;
        goto unordered_bcc;
      case UNEQ_EXPR:
        rcode1 = UNEQ;
        tcode2 = EQ_EXPR;
        goto unordered_bcc;

      unordered_bcc:
        mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
        if (can_compare_p (rcode1, mode, ccp_jump))
          do_compare_and_jump (exp, rcode1, rcode1, if_false_label,
                               if_true_label);
        else
          {
            tree op0 = save_expr (TREE_OPERAND (exp, 0));
            tree op1 = save_expr (TREE_OPERAND (exp, 1));
            tree cmp0, cmp1;

            /* If the target doesn't support combined unordered
               compares, decompose into UNORDERED + comparison.  */
            cmp0 = fold (build (UNORDERED_EXPR, TREE_TYPE (exp), op0, op1));
            cmp1 = fold (build (tcode2, TREE_TYPE (exp), op0, op1));
            exp = build (TRUTH_ORIF_EXPR, TREE_TYPE (exp), cmp0, cmp1);
            do_jump (exp, if_false_label, if_true_label);
          }
      }
      break;

      /* Special case:
          __builtin_expect (<test>, 0)	and
          __builtin_expect (<test>, 1)

         We need to do this here, so that <test> is not converted to a SCC
         operation on machines that use condition code registers and COMPARE
         like the PowerPC, and then the jump is done based on whether the SCC
         operation produced a 1 or 0.  */
    case CALL_EXPR:
      /* Check for a built-in function.  */
      {
	tree fndecl = get_callee_fndecl (exp);
	tree arglist = TREE_OPERAND (exp, 1);

	if (fndecl
	    && DECL_BUILT_IN (fndecl)
	    && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_EXPECT
	    && arglist != NULL_TREE
	    && TREE_CHAIN (arglist) != NULL_TREE)
	  {
	    rtx seq = expand_builtin_expect_jump (exp, if_false_label,
						  if_true_label);

	    if (seq != NULL_RTX)
	      {
		emit_insn (seq);
		return;
	      }
	  }
      }
      /* Fall through and generate the normal code.  */

    default:
    normal:
      temp = expand_expr (exp, NULL_RTX, VOIDmode, 0);
#if 0
      /* This is not needed any more and causes poor code since it causes
         comparisons and tests from non-SI objects to have different code
         sequences.  */
      /* Copy to register to avoid generating bad insns by cse
         from (set (mem ...) (arithop))  (set (cc0) (mem ...)).  */
      if (!cse_not_expected && GET_CODE (temp) == MEM)
        temp = copy_to_reg (temp);
#endif
      do_pending_stack_adjust ();
      /* Do any postincrements in the expression that was tested.  */
      emit_queue ();

      if (GET_CODE (temp) == CONST_INT
          || (GET_CODE (temp) == CONST_DOUBLE && GET_MODE (temp) == VOIDmode)
          || GET_CODE (temp) == LABEL_REF)
        {
          rtx target = temp == const0_rtx ? if_false_label : if_true_label;
          if (target)
            emit_jump (target);
        }
      else if (GET_MODE_CLASS (GET_MODE (temp)) == MODE_INT
               && ! can_compare_p (NE, GET_MODE (temp), ccp_jump))
        /* Note swapping the labels gives us not-equal.  */
        do_jump_by_parts_equality_rtx (temp, if_true_label, if_false_label);
      else if (GET_MODE (temp) != VOIDmode)
	{
	  /* The RTL optimizers prefer comparisons against pseudos.  */
	  if (GET_CODE (temp) == SUBREG)
	    {
	      /* Compare promoted variables in their promoted mode.  */
	      if (SUBREG_PROMOTED_VAR_P (temp)
		  && GET_CODE (XEXP (temp, 0)) == REG)
		temp = XEXP (temp, 0);
	      else
		temp = copy_to_reg (temp);
	    }
	  do_compare_rtx_and_jump (temp, CONST0_RTX (GET_MODE (temp)),
				   NE, TREE_UNSIGNED (TREE_TYPE (exp)),
				   GET_MODE (temp), NULL_RTX,
				   if_false_label, if_true_label);
	}
      else
        abort ();
    }

  if (drop_through_label)
    {
      /* If do_jump produces code that might be jumped around,
         do any stack adjusts from that code, before the place
         where control merges in.  */
      do_pending_stack_adjust ();
      emit_label (drop_through_label);
    }
}

/* Given a comparison expression EXP for values too wide to be compared
   with one insn, test the comparison and jump to the appropriate label.
   The code of EXP is ignored; we always test GT if SWAP is 0,
   and LT if SWAP is 1.  */

static void
do_jump_by_parts_greater (tree exp, int swap, rtx if_false_label,
			  rtx if_true_label)
{
  rtx op0 = expand_expr (TREE_OPERAND (exp, swap), NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (TREE_OPERAND (exp, !swap), NULL_RTX, VOIDmode, 0);
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
  int unsignedp = TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0)));

  do_jump_by_parts_greater_rtx (mode, unsignedp, op0, op1, if_false_label, if_true_label);
}

/* Compare OP0 with OP1, word at a time, in mode MODE.
   UNSIGNEDP says to do unsigned comparison.
   Jump to IF_TRUE_LABEL if OP0 is greater, IF_FALSE_LABEL otherwise.  */

void
do_jump_by_parts_greater_rtx (enum machine_mode mode, int unsignedp, rtx op0,
			      rtx op1, rtx if_false_label, rtx if_true_label)
{
  int nwords = (GET_MODE_SIZE (mode) / UNITS_PER_WORD);
  rtx drop_through_label = 0;
  int i;

  if (! if_true_label || ! if_false_label)
    drop_through_label = gen_label_rtx ();
  if (! if_true_label)
    if_true_label = drop_through_label;
  if (! if_false_label)
    if_false_label = drop_through_label;

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
      do_compare_rtx_and_jump (op0_word, op1_word, GT,
                               (unsignedp || i > 0), word_mode, NULL_RTX,
                               NULL_RTX, if_true_label);

      /* Consider lower words only if these are equal.  */
      do_compare_rtx_and_jump (op0_word, op1_word, NE, unsignedp, word_mode,
                               NULL_RTX, NULL_RTX, if_false_label);
    }

  if (if_false_label)
    emit_jump (if_false_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Given an EQ_EXPR expression EXP for values too wide to be compared
   with one insn, test the comparison and jump to the appropriate label.  */

static void
do_jump_by_parts_equality (tree exp, rtx if_false_label, rtx if_true_label)
{
  rtx op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)));
  int nwords = (GET_MODE_SIZE (mode) / UNITS_PER_WORD);
  int i;
  rtx drop_through_label = 0;

  if (! if_false_label)
    drop_through_label = if_false_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    do_compare_rtx_and_jump (operand_subword_force (op0, i, mode),
                             operand_subword_force (op1, i, mode),
                             EQ, TREE_UNSIGNED (TREE_TYPE (exp)),
                             word_mode, NULL_RTX, if_false_label, NULL_RTX);

  if (if_true_label)
    emit_jump (if_true_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Jump according to whether OP0 is 0.
   We assume that OP0 has an integer mode that is too wide
   for the available compare insns.  */

void
do_jump_by_parts_equality_rtx (rtx op0, rtx if_false_label, rtx if_true_label)
{
  int nwords = GET_MODE_SIZE (GET_MODE (op0)) / UNITS_PER_WORD;
  rtx part;
  int i;
  rtx drop_through_label = 0;

  /* The fastest way of doing this comparison on almost any machine is to
     "or" all the words and compare the result.  If all have to be loaded
     from memory and this is a very wide item, it's possible this may
     be slower, but that's highly unlikely.  */

  part = gen_reg_rtx (word_mode);
  emit_move_insn (part, operand_subword_force (op0, 0, GET_MODE (op0)));
  for (i = 1; i < nwords && part != 0; i++)
    part = expand_binop (word_mode, ior_optab, part,
                         operand_subword_force (op0, i, GET_MODE (op0)),
                         part, 1, OPTAB_WIDEN);

  if (part != 0)
    {
      do_compare_rtx_and_jump (part, const0_rtx, EQ, 1, word_mode,
                               NULL_RTX, if_false_label, if_true_label);

      return;
    }

  /* If we couldn't do the "or" simply, do this with a series of compares.  */
  if (! if_false_label)
    drop_through_label = if_false_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    do_compare_rtx_and_jump (operand_subword_force (op0, i, GET_MODE (op0)),
                             const0_rtx, EQ, 1, word_mode, NULL_RTX,
                             if_false_label, NULL_RTX);

  if (if_true_label)
    emit_jump (if_true_label);

  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Generate code for a comparison of OP0 and OP1 with rtx code CODE.
   (including code to compute the values to be compared)
   and set (CC0) according to the result.
   The decision as to signed or unsigned comparison must be made by the caller.

   We force a stack adjustment unless there are currently
   things pushed on the stack that aren't yet used.

   If MODE is BLKmode, SIZE is an RTX giving the size of the objects being
   compared.  */

rtx
compare_from_rtx (rtx op0, rtx op1, enum rtx_code code, int unsignedp,
		  enum machine_mode mode, rtx size)
{
  enum rtx_code ucode;
  rtx tem;

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      tem = op0;
      op0 = op1;
      op1 = tem;
      code = swap_condition (code);
    }

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  ucode = unsignedp ? unsigned_condition (code) : code;
  if ((tem = simplify_relational_operation (ucode, mode, op0, op1)) != 0)
    return tem;

#if 0
  /* There's no need to do this now that combine.c can eliminate lots of
     sign extensions.  This can be less efficient in certain cases on other
     machines.  */

  /* If this is a signed equality comparison, we can do it as an
     unsigned comparison since zero-extension is cheaper than sign
     extension and comparisons with zero are done as unsigned.  This is
     the case even on machines that can do fast sign extension, since
     zero-extension is easier to combine with other operations than
     sign-extension is.  If we are comparing against a constant, we must
     convert it to what it would look like unsigned.  */
  if ((code == EQ || code == NE) && ! unsignedp
      && GET_MODE_BITSIZE (GET_MODE (op0)) <= HOST_BITS_PER_WIDE_INT)
    {
      if (GET_CODE (op1) == CONST_INT
          && (INTVAL (op1) & GET_MODE_MASK (GET_MODE (op0))) != INTVAL (op1))
        op1 = GEN_INT (INTVAL (op1) & GET_MODE_MASK (GET_MODE (op0)));
      unsignedp = 1;
    }
#endif

  emit_cmp_insn (op0, op1, code, size, mode, unsignedp);

#if HAVE_cc0
  return gen_rtx_fmt_ee (code, VOIDmode, cc0_rtx, const0_rtx);
#else
  return gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
#endif
}

/* Like do_compare_and_jump but expects the values to compare as two rtx's.
   The decision as to signed or unsigned comparison must be made by the caller.

   If MODE is BLKmode, SIZE is an RTX giving the size of the objects being
   compared.  */

void
do_compare_rtx_and_jump (rtx op0, rtx op1, enum rtx_code code, int unsignedp,
			 enum machine_mode mode, rtx size, rtx if_false_label,
			 rtx if_true_label)
{
  enum rtx_code ucode;
  rtx tem;
  int dummy_true_label = 0;

  /* Reverse the comparison if that is safe and we want to jump if it is
     false.  */
  if (! if_true_label && ! FLOAT_MODE_P (mode))
    {
      if_true_label = if_false_label;
      if_false_label = 0;
      code = reverse_condition (code);
    }

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      tem = op0;
      op0 = op1;
      op1 = tem;
      code = swap_condition (code);
    }

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  ucode = unsignedp ? unsigned_condition (code) : code;
  if ((tem = simplify_relational_operation (ucode, mode, op0, op1)) != 0)
    {
      if (tem == const_true_rtx)
        {
          if (if_true_label)
            emit_jump (if_true_label);
        }
      else
        {
          if (if_false_label)
            emit_jump (if_false_label);
        }
      return;
    }

#if 0
  /* There's no need to do this now that combine.c can eliminate lots of
     sign extensions.  This can be less efficient in certain cases on other
     machines.  */

  /* If this is a signed equality comparison, we can do it as an
     unsigned comparison since zero-extension is cheaper than sign
     extension and comparisons with zero are done as unsigned.  This is
     the case even on machines that can do fast sign extension, since
     zero-extension is easier to combine with other operations than
     sign-extension is.  If we are comparing against a constant, we must
     convert it to what it would look like unsigned.  */
  if ((code == EQ || code == NE) && ! unsignedp
      && GET_MODE_BITSIZE (GET_MODE (op0)) <= HOST_BITS_PER_WIDE_INT)
    {
      if (GET_CODE (op1) == CONST_INT
          && (INTVAL (op1) & GET_MODE_MASK (GET_MODE (op0))) != INTVAL (op1))
        op1 = GEN_INT (INTVAL (op1) & GET_MODE_MASK (GET_MODE (op0)));
      unsignedp = 1;
    }
#endif

  if (! if_true_label)
    {
      dummy_true_label = 1;
      if_true_label = gen_label_rtx ();
    }

  emit_cmp_and_jump_insns (op0, op1, code, size, mode, unsignedp,
                           if_true_label);

  if (if_false_label)
    emit_jump (if_false_label);
  if (dummy_true_label)
    emit_label (if_true_label);
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
do_compare_and_jump (tree exp, enum rtx_code signed_code,
		     enum rtx_code unsigned_code, rtx if_false_label,
		     rtx if_true_label)
{
  rtx op0, op1;
  tree type;
  enum machine_mode mode;
  int unsignedp;
  enum rtx_code code;

  /* Don't crash if the comparison was erroneous.  */
  op0 = expand_expr (TREE_OPERAND (exp, 0), NULL_RTX, VOIDmode, 0);
  if (TREE_CODE (TREE_OPERAND (exp, 0)) == ERROR_MARK)
    return;

  op1 = expand_expr (TREE_OPERAND (exp, 1), NULL_RTX, VOIDmode, 0);
  if (TREE_CODE (TREE_OPERAND (exp, 1)) == ERROR_MARK)
    return;

  type = TREE_TYPE (TREE_OPERAND (exp, 0));
  mode = TYPE_MODE (type);
  if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST
      && (TREE_CODE (TREE_OPERAND (exp, 1)) != INTEGER_CST
          || (GET_MODE_BITSIZE (mode)
              > GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp,
                                                                      1)))))))
    {
      /* op0 might have been replaced by promoted constant, in which
         case the type of second argument should be used.  */
      type = TREE_TYPE (TREE_OPERAND (exp, 1));
      mode = TYPE_MODE (type);
    }
  unsignedp = TREE_UNSIGNED (type);
  code = unsignedp ? unsigned_code : signed_code;

#ifdef HAVE_canonicalize_funcptr_for_compare
  /* If function pointers need to be "canonicalized" before they can
     be reliably compared, then canonicalize them.  */
  if (HAVE_canonicalize_funcptr_for_compare
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == POINTER_TYPE
      && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0))))
          == FUNCTION_TYPE))
    {
      rtx new_op0 = gen_reg_rtx (mode);

      emit_insn (gen_canonicalize_funcptr_for_compare (new_op0, op0));
      op0 = new_op0;
    }

  if (HAVE_canonicalize_funcptr_for_compare
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 1))) == POINTER_TYPE
      && (TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 1))))
          == FUNCTION_TYPE))
    {
      rtx new_op1 = gen_reg_rtx (mode);

      emit_insn (gen_canonicalize_funcptr_for_compare (new_op1, op1));
      op1 = new_op1;
    }
#endif

  /* Do any postincrements in the expression that was tested.  */
  emit_queue ();

  do_compare_rtx_and_jump (op0, op1, code, unsignedp, mode,
                           ((mode == BLKmode)
                            ? expr_size (TREE_OPERAND (exp, 0)) : NULL_RTX),
                           if_false_label, if_true_label);
}
