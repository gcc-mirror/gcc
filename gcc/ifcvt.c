/* If-conversion support.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include "config.h"
#include "system.h"

#include "rtl.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "except.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "expr.h"
#include "real.h"
#include "output.h"
#include "toplev.h"
#include "tm_p.h"


#ifndef HAVE_conditional_execution
#define HAVE_conditional_execution 0
#endif
#ifndef HAVE_conditional_move
#define HAVE_conditional_move 0
#endif
#ifndef HAVE_incscc
#define HAVE_incscc 0
#endif
#ifndef HAVE_decscc
#define HAVE_decscc 0
#endif
#ifndef HAVE_trap
#define HAVE_trap 0
#endif
#ifndef HAVE_conditional_trap
#define HAVE_conditional_trap 0
#endif

#ifndef MAX_CONDITIONAL_EXECUTE
#define MAX_CONDITIONAL_EXECUTE   (BRANCH_COST + 1)
#endif

#define NULL_EDGE	((struct edge_def *)NULL)
#define NULL_BLOCK	((struct basic_block_def *)NULL)

/* # of IF-THEN or IF-THEN-ELSE blocks we looked at  */
static int num_possible_if_blocks;

/* # of IF-THEN or IF-THEN-ELSE blocks were converted to conditional
   execution.  */
static int num_updated_if_blocks;

/* # of basic blocks that were removed.  */
static int num_removed_blocks;

/* Whether conditional execution changes were made.  */
static int cond_exec_changed_p;

/* True if life data ok at present.  */
static bool life_data_ok;

/* The post-dominator relation on the original block numbers.  */
static dominance_info post_dominators;

/* Forward references.  */
static int count_bb_insns		PARAMS ((basic_block));
static rtx first_active_insn		PARAMS ((basic_block));
static rtx last_active_insn		PARAMS ((basic_block, int));
static int seq_contains_jump		PARAMS ((rtx));
static basic_block block_fallthru	PARAMS ((basic_block));
static int cond_exec_process_insns	PARAMS ((ce_if_block_t *,
						 rtx, rtx, rtx, rtx, int));
static rtx cond_exec_get_condition	PARAMS ((rtx));
static int cond_exec_process_if_block	PARAMS ((ce_if_block_t *, int));
static rtx noce_get_condition		PARAMS ((rtx, rtx *));
static int noce_operand_ok		PARAMS ((rtx));
static int noce_process_if_block	PARAMS ((ce_if_block_t *));
static int process_if_block		PARAMS ((ce_if_block_t *));
static void merge_if_block		PARAMS ((ce_if_block_t *));
static int find_cond_trap		PARAMS ((basic_block, edge, edge));
static basic_block find_if_header	PARAMS ((basic_block, int));
static int block_jumps_and_fallthru_p	PARAMS ((basic_block, basic_block));
static int find_if_block		PARAMS ((ce_if_block_t *));
static int find_if_case_1		PARAMS ((basic_block, edge, edge));
static int find_if_case_2		PARAMS ((basic_block, edge, edge));
static int find_memory			PARAMS ((rtx *, void *));
static int dead_or_predicable		PARAMS ((basic_block, basic_block,
						 basic_block, basic_block, int));
static void noce_emit_move_insn		PARAMS ((rtx, rtx));
static rtx block_has_only_trap		PARAMS ((basic_block));

/* Count the number of non-jump active insns in BB.  */

static int
count_bb_insns (bb)
     basic_block bb;
{
  int count = 0;
  rtx insn = bb->head;

  while (1)
    {
      if (GET_CODE (insn) == CALL_INSN || GET_CODE (insn) == INSN)
	count++;

      if (insn == bb->end)
	break;
      insn = NEXT_INSN (insn);
    }

  return count;
}

/* Return the first non-jump active insn in the basic block.  */

static rtx
first_active_insn (bb)
     basic_block bb;
{
  rtx insn = bb->head;

  if (GET_CODE (insn) == CODE_LABEL)
    {
      if (insn == bb->end)
	return NULL_RTX;
      insn = NEXT_INSN (insn);
    }

  while (GET_CODE (insn) == NOTE)
    {
      if (insn == bb->end)
	return NULL_RTX;
      insn = NEXT_INSN (insn);
    }

  if (GET_CODE (insn) == JUMP_INSN)
    return NULL_RTX;

  return insn;
}

/* Return the last non-jump active (non-jump) insn in the basic block.  */

static rtx
last_active_insn (bb, skip_use_p)
     basic_block bb;
     int skip_use_p;
{
  rtx insn = bb->end;
  rtx head = bb->head;

  while (GET_CODE (insn) == NOTE
	 || GET_CODE (insn) == JUMP_INSN
	 || (skip_use_p
	     && GET_CODE (insn) == INSN
	     && GET_CODE (PATTERN (insn)) == USE))
    {
      if (insn == head)
	return NULL_RTX;
      insn = PREV_INSN (insn);
    }

  if (GET_CODE (insn) == CODE_LABEL)
    return NULL_RTX;

  return insn;
}

/* It is possible, especially when having dealt with multi-word 
   arithmetic, for the expanders to have emitted jumps.  Search
   through the sequence and return TRUE if a jump exists so that
   we can abort the conversion.  */

static int
seq_contains_jump (insn)
     rtx insn;
{
  while (insn)
    {
      if (GET_CODE (insn) == JUMP_INSN)
	return 1;
      insn = NEXT_INSN (insn);
    }
  return 0;
}

static basic_block
block_fallthru (bb)
     basic_block bb;
{
  edge e;

  for (e = bb->succ;
       e != NULL_EDGE && (e->flags & EDGE_FALLTHRU) == 0;
       e = e->succ_next)
    ;

  return (e) ? e->dest : NULL_BLOCK;
}

/* Go through a bunch of insns, converting them to conditional
   execution format if possible.  Return TRUE if all of the non-note
   insns were processed.  */

static int
cond_exec_process_insns (ce_info, start, end, test, prob_val, mod_ok)
     ce_if_block_t *ce_info ATTRIBUTE_UNUSED;	/* if block information */
     rtx start;			/* first insn to look at */
     rtx end;			/* last insn to look at */
     rtx test;			/* conditional execution test */
     rtx prob_val;		/* probability of branch taken.  */
     int mod_ok;		/* true if modifications ok last insn.  */
{
  int must_be_last = FALSE;
  rtx insn;
  rtx xtest;
  rtx pattern;

  if (!start || !end)
    return FALSE;

  for (insn = start; ; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE)
	goto insn_done;

      if (GET_CODE (insn) != INSN && GET_CODE (insn) != CALL_INSN)
	abort ();

      /* Remove USE insns that get in the way.  */
      if (reload_completed && GET_CODE (PATTERN (insn)) == USE)
	{
	  /* ??? Ug.  Actually unlinking the thing is problematic, 
	     given what we'd have to coordinate with our callers.  */
	  PUT_CODE (insn, NOTE);
	  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (insn) = 0;
	  goto insn_done;
	}

      /* Last insn wasn't last?  */
      if (must_be_last)
	return FALSE;

      if (modified_in_p (test, insn))
	{
	  if (!mod_ok)
	    return FALSE;
	  must_be_last = TRUE;
	}

      /* Now build the conditional form of the instruction.  */
      pattern = PATTERN (insn);
      xtest = copy_rtx (test);

      /* If this is already a COND_EXEC, rewrite the test to be an AND of the
         two conditions.  */
      if (GET_CODE (pattern) == COND_EXEC)
	{
	  if (GET_MODE (xtest) != GET_MODE (COND_EXEC_TEST (pattern)))
	    return FALSE;

	  xtest = gen_rtx_AND (GET_MODE (xtest), xtest,
			       COND_EXEC_TEST (pattern));
	  pattern = COND_EXEC_CODE (pattern);
	}

      pattern = gen_rtx_COND_EXEC (VOIDmode, xtest, pattern);

      /* If the machine needs to modify the insn being conditionally executed,
         say for example to force a constant integer operand into a temp
         register, do so here.  */
#ifdef IFCVT_MODIFY_INSN
      IFCVT_MODIFY_INSN (ce_info, pattern, insn);
      if (! pattern)
	return FALSE;
#endif

      validate_change (insn, &PATTERN (insn), pattern, 1);

      if (GET_CODE (insn) == CALL_INSN && prob_val)
	validate_change (insn, &REG_NOTES (insn),
			 alloc_EXPR_LIST (REG_BR_PROB, prob_val,
					  REG_NOTES (insn)), 1);

    insn_done:
      if (insn == end)
	break;
    }

  return TRUE;
}

/* Return the condition for a jump.  Do not do any special processing.  */

static rtx
cond_exec_get_condition (jump)
     rtx jump;
{
  rtx test_if, cond;

  if (any_condjump_p (jump))
    test_if = SET_SRC (pc_set (jump));
  else
    return NULL_RTX;
  cond = XEXP (test_if, 0);

  /* If this branches to JUMP_LABEL when the condition is false,
     reverse the condition.  */
  if (GET_CODE (XEXP (test_if, 2)) == LABEL_REF
      && XEXP (XEXP (test_if, 2), 0) == JUMP_LABEL (jump))
    {
      enum rtx_code rev = reversed_comparison_code (cond, jump);
      if (rev == UNKNOWN)
	return NULL_RTX;

      cond = gen_rtx_fmt_ee (rev, GET_MODE (cond), XEXP (cond, 0),
			     XEXP (cond, 1));
    }

  return cond;
}

/* Given a simple IF-THEN or IF-THEN-ELSE block, attempt to convert it
   to conditional execution.  Return TRUE if we were successful at
   converting the block.  */

static int
cond_exec_process_if_block (ce_info, do_multiple_p)
     ce_if_block_t * ce_info;	/* if block information */
     int do_multiple_p;		/* != 0 if we should handle && and || blocks */
{
  basic_block test_bb = ce_info->test_bb;	/* last test block */
  basic_block then_bb = ce_info->then_bb;	/* THEN */
  basic_block else_bb = ce_info->else_bb;	/* ELSE or NULL */
  rtx test_expr;		/* expression in IF_THEN_ELSE that is tested */
  rtx then_start;		/* first insn in THEN block */
  rtx then_end;			/* last insn + 1 in THEN block */
  rtx else_start = NULL_RTX;	/* first insn in ELSE block or NULL */
  rtx else_end = NULL_RTX;	/* last insn + 1 in ELSE block */
  int max;			/* max # of insns to convert.  */
  int then_mod_ok;		/* whether conditional mods are ok in THEN */
  rtx true_expr;		/* test for else block insns */
  rtx false_expr;		/* test for then block insns */
  rtx true_prob_val;		/* probability of else block */
  rtx false_prob_val;		/* probability of then block */
  int n_insns;
  enum rtx_code false_code;

  /* If test is comprised of && or || elements, and we've failed at handling
     all of them together, just use the last test if it is the special case of
     && elements without an ELSE block.  */
  if (!do_multiple_p && ce_info->num_multiple_test_blocks)
    {
      if (else_bb || ! ce_info->and_and_p)
	return FALSE;

      ce_info->test_bb = test_bb = ce_info->last_test_bb;
      ce_info->num_multiple_test_blocks = 0;
      ce_info->num_and_and_blocks = 0;
      ce_info->num_or_or_blocks = 0;
    }

  /* Find the conditional jump to the ELSE or JOIN part, and isolate
     the test.  */
  test_expr = cond_exec_get_condition (test_bb->end);
  if (! test_expr)
    return FALSE;

  /* If the conditional jump is more than just a conditional jump,
     then we can not do conditional execution conversion on this block.  */
  if (! onlyjump_p (test_bb->end))
    return FALSE;

  /* Collect the bounds of where we're to search, skipping any labels, jumps
     and notes at the beginning and end of the block.  Then count the total
     number of insns and see if it is small enough to convert.  */
  then_start = first_active_insn (then_bb);
  then_end = last_active_insn (then_bb, TRUE);
  n_insns = ce_info->num_then_insns = count_bb_insns (then_bb);
  max = MAX_CONDITIONAL_EXECUTE;

  if (else_bb)
    {
      max *= 2;
      else_start = first_active_insn (else_bb);
      else_end = last_active_insn (else_bb, TRUE);
      n_insns += ce_info->num_else_insns = count_bb_insns (else_bb);
    }

  if (n_insns > max)
    return FALSE;

  /* Map test_expr/test_jump into the appropriate MD tests to use on
     the conditionally executed code.  */
  
  true_expr = test_expr;

  false_code = reversed_comparison_code (true_expr, test_bb->end);
  if (false_code != UNKNOWN)
    false_expr = gen_rtx_fmt_ee (false_code, GET_MODE (true_expr),
				 XEXP (true_expr, 0), XEXP (true_expr, 1));
  else
    false_expr = NULL_RTX;

#ifdef IFCVT_MODIFY_TESTS
  /* If the machine description needs to modify the tests, such as setting a
     conditional execution register from a comparison, it can do so here.  */
  IFCVT_MODIFY_TESTS (ce_info, true_expr, false_expr);

  /* See if the conversion failed */
  if (!true_expr || !false_expr)
    goto fail;
#endif

  true_prob_val = find_reg_note (test_bb->end, REG_BR_PROB, NULL_RTX);
  if (true_prob_val)
    {
      true_prob_val = XEXP (true_prob_val, 0);
      false_prob_val = GEN_INT (REG_BR_PROB_BASE - INTVAL (true_prob_val));
    }
  else
    false_prob_val = NULL_RTX;

  /* If we have && or || tests, do them here.  These tests are in the adjacent
     blocks after the first block containing the test.  */
  if (ce_info->num_multiple_test_blocks > 0)
    {
      basic_block bb = test_bb;
      basic_block last_test_bb = ce_info->last_test_bb;

      if (! false_expr)
	goto fail;

      do
	{
	  rtx start, end;
	  rtx t, f;

	  bb = block_fallthru (bb);
	  start = first_active_insn (bb);
	  end = last_active_insn (bb, TRUE);
	  if (start
	      && ! cond_exec_process_insns (ce_info, start, end, false_expr,
					    false_prob_val, FALSE))
	    goto fail;

	  /* If the conditional jump is more than just a conditional jump, then
	     we can not do conditional execution conversion on this block.  */
	  if (! onlyjump_p (bb->end))
	    goto fail;

	  /* Find the conditional jump and isolate the test.  */
	  t = cond_exec_get_condition (bb->end);
	  if (! t)
	    goto fail;

	  f = gen_rtx_fmt_ee (reverse_condition (GET_CODE (t)),
			      GET_MODE (t),
			      XEXP (t, 0),
			      XEXP (t, 1));

	  if (ce_info->and_and_p)
	    {
	      t = gen_rtx_AND (GET_MODE (t), true_expr, t);
	      f = gen_rtx_IOR (GET_MODE (t), false_expr, f);
	    }
	  else
	    {
	      t = gen_rtx_IOR (GET_MODE (t), true_expr, t);
	      f = gen_rtx_AND (GET_MODE (t), false_expr, f);
	    }

	  /* If the machine description needs to modify the tests, such as
	     setting a conditional execution register from a comparison, it can
	     do so here.  */
#ifdef IFCVT_MODIFY_MULTIPLE_TESTS
	  IFCVT_MODIFY_MULTIPLE_TESTS (ce_info, bb, t, f);

	  /* See if the conversion failed */
	  if (!t || !f)
	    goto fail;
#endif

	  true_expr = t;
	  false_expr = f;
	}
      while (bb != last_test_bb);
    }

  /* For IF-THEN-ELSE blocks, we don't allow modifications of the test
     on then THEN block.  */
  then_mod_ok = (else_bb == NULL_BLOCK);

  /* Go through the THEN and ELSE blocks converting the insns if possible
     to conditional execution.  */

  if (then_end
      && (! false_expr
	  || ! cond_exec_process_insns (ce_info, then_start, then_end,
					false_expr, false_prob_val,
					then_mod_ok)))
    goto fail;

  if (else_bb && else_end
      && ! cond_exec_process_insns (ce_info, else_start, else_end,
				    true_expr, true_prob_val, TRUE))
    goto fail;

  /* If we cannot apply the changes, fail.  Do not go through the normal fail
     processing, since apply_change_group will call cancel_changes.  */
  if (! apply_change_group ())
    {
#ifdef IFCVT_MODIFY_CANCEL
      /* Cancel any machine dependent changes.  */
      IFCVT_MODIFY_CANCEL (ce_info);
#endif
      return FALSE;
    }

#ifdef IFCVT_MODIFY_FINAL
  /* Do any machine dependent final modifications */
  IFCVT_MODIFY_FINAL (ce_info);
#endif

  /* Conversion succeeded.  */
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "%d insn%s converted to conditional execution.\n",
	     n_insns, (n_insns == 1) ? " was" : "s were");

  /* Merge the blocks!  */
  merge_if_block (ce_info);
  cond_exec_changed_p = TRUE;
  return TRUE;

 fail:
#ifdef IFCVT_MODIFY_CANCEL
  /* Cancel any machine dependent changes.  */
  IFCVT_MODIFY_CANCEL (ce_info);
#endif

  cancel_changes (0);
  return FALSE;
}

/* Used by noce_process_if_block to communicate with its subroutines. 

   The subroutines know that A and B may be evaluated freely.  They
   know that X is a register.  They should insert new instructions 
   before cond_earliest.  */

struct noce_if_info
{
  basic_block test_bb;
  rtx insn_a, insn_b;
  rtx x, a, b;
  rtx jump, cond, cond_earliest;
};

static rtx noce_emit_store_flag		PARAMS ((struct noce_if_info *,
						 rtx, int, int));
static int noce_try_store_flag		PARAMS ((struct noce_if_info *));
static int noce_try_store_flag_inc	PARAMS ((struct noce_if_info *));
static int noce_try_store_flag_constants PARAMS ((struct noce_if_info *));
static int noce_try_store_flag_mask	PARAMS ((struct noce_if_info *));
static rtx noce_emit_cmove		PARAMS ((struct noce_if_info *,
						 rtx, enum rtx_code, rtx,
						 rtx, rtx, rtx));
static int noce_try_cmove		PARAMS ((struct noce_if_info *));
static int noce_try_cmove_arith		PARAMS ((struct noce_if_info *));
static rtx noce_get_alt_condition	PARAMS ((struct noce_if_info *,
						 rtx, rtx *));
static int noce_try_minmax		PARAMS ((struct noce_if_info *));
static int noce_try_abs			PARAMS ((struct noce_if_info *));

/* Helper function for noce_try_store_flag*.  */

static rtx
noce_emit_store_flag (if_info, x, reversep, normalize)
     struct noce_if_info *if_info;
     rtx x;
     int reversep, normalize;
{
  rtx cond = if_info->cond;
  int cond_complex;
  enum rtx_code code;

  cond_complex = (! general_operand (XEXP (cond, 0), VOIDmode)
		  || ! general_operand (XEXP (cond, 1), VOIDmode));

  /* If earliest == jump, or when the condition is complex, try to
     build the store_flag insn directly.  */

  if (cond_complex)
    cond = XEXP (SET_SRC (pc_set (if_info->jump)), 0);

  if (reversep)
    code = reversed_comparison_code (cond, if_info->jump);
  else
    code = GET_CODE (cond);

  if ((if_info->cond_earliest == if_info->jump || cond_complex)
      && (normalize == 0 || STORE_FLAG_VALUE == normalize))
    {
      rtx tmp;

      tmp = gen_rtx_fmt_ee (code, GET_MODE (x), XEXP (cond, 0),
			    XEXP (cond, 1));
      tmp = gen_rtx_SET (VOIDmode, x, tmp);

      start_sequence ();
      tmp = emit_insn (tmp);

      if (recog_memoized (tmp) >= 0)
	{
	  tmp = get_insns ();
	  end_sequence ();
	  emit_insn (tmp);

	  if_info->cond_earliest = if_info->jump;

	  return x;
	}

      end_sequence ();
    }

  /* Don't even try if the comparison operands or the mode of X are weird.  */
  if (cond_complex || !SCALAR_INT_MODE_P (GET_MODE (x)))
    return NULL_RTX;

  return emit_store_flag (x, code, XEXP (cond, 0),
			  XEXP (cond, 1), VOIDmode,
			  (code == LTU || code == LEU
			   || code == GEU || code == GTU), normalize);
}

/* Emit instruction to move an rtx into STRICT_LOW_PART.  */
static void
noce_emit_move_insn (x, y)
     rtx x, y;
{
  enum machine_mode outmode, inmode;
  rtx outer, inner;
  int bitpos;

  if (GET_CODE (x) != STRICT_LOW_PART)
    {
      emit_move_insn (x, y);
      return;
    }

  outer = XEXP (x, 0);
  inner = XEXP (outer, 0);
  outmode = GET_MODE (outer);
  inmode = GET_MODE (inner);
  bitpos = SUBREG_BYTE (outer) * BITS_PER_UNIT;
  store_bit_field (inner, GET_MODE_BITSIZE (outmode), bitpos, outmode, y,
		   GET_MODE_BITSIZE (inmode));
}

/* Convert "if (test) x = 1; else x = 0".

   Only try 0 and STORE_FLAG_VALUE here.  Other combinations will be
   tried in noce_try_store_flag_constants after noce_try_cmove has had
   a go at the conversion.  */

static int
noce_try_store_flag (if_info)
     struct noce_if_info *if_info;
{
  int reversep;
  rtx target, seq;

  if (GET_CODE (if_info->b) == CONST_INT
      && INTVAL (if_info->b) == STORE_FLAG_VALUE
      && if_info->a == const0_rtx)
    reversep = 0;
  else if (if_info->b == const0_rtx
	   && GET_CODE (if_info->a) == CONST_INT
	   && INTVAL (if_info->a) == STORE_FLAG_VALUE
	   && (reversed_comparison_code (if_info->cond, if_info->jump)
	       != UNKNOWN))
    reversep = 1;
  else
    return FALSE;

  start_sequence ();

  target = noce_emit_store_flag (if_info, if_info->x, reversep, 0);
  if (target)
    {
      if (target != if_info->x)
	noce_emit_move_insn (if_info->x, target);

      seq = get_insns ();
      end_sequence ();
      emit_insn_before_scope (seq, if_info->jump, INSN_SCOPE (if_info->insn_a));

      return TRUE;
    }
  else
    {
      end_sequence ();
      return FALSE;
    }
}

/* Convert "if (test) x = a; else x = b", for A and B constant.  */

static int
noce_try_store_flag_constants (if_info)
     struct noce_if_info *if_info;
{
  rtx target, seq;
  int reversep;
  HOST_WIDE_INT itrue, ifalse, diff, tmp;
  int normalize, can_reverse;
  enum machine_mode mode;

  if (! no_new_pseudos
      && GET_CODE (if_info->a) == CONST_INT
      && GET_CODE (if_info->b) == CONST_INT)
    {
      mode = GET_MODE (if_info->x);
      ifalse = INTVAL (if_info->a);
      itrue = INTVAL (if_info->b);

      /* Make sure we can represent the difference between the two values.  */
      if ((itrue - ifalse > 0)
	  != ((ifalse < 0) != (itrue < 0) ? ifalse < 0 : ifalse < itrue))
	return FALSE;

      diff = trunc_int_for_mode (itrue - ifalse, mode);

      can_reverse = (reversed_comparison_code (if_info->cond, if_info->jump)
		     != UNKNOWN);

      reversep = 0;
      if (diff == STORE_FLAG_VALUE || diff == -STORE_FLAG_VALUE)
	normalize = 0;
      else if (ifalse == 0 && exact_log2 (itrue) >= 0
	       && (STORE_FLAG_VALUE == 1
		   || BRANCH_COST >= 2))
	normalize = 1;
      else if (itrue == 0 && exact_log2 (ifalse) >= 0 && can_reverse
	       && (STORE_FLAG_VALUE == 1 || BRANCH_COST >= 2))
	normalize = 1, reversep = 1;
      else if (itrue == -1
	       && (STORE_FLAG_VALUE == -1
		   || BRANCH_COST >= 2))
	normalize = -1;
      else if (ifalse == -1 && can_reverse
	       && (STORE_FLAG_VALUE == -1 || BRANCH_COST >= 2))
	normalize = -1, reversep = 1;
      else if ((BRANCH_COST >= 2 && STORE_FLAG_VALUE == -1)
	       || BRANCH_COST >= 3)
	normalize = -1;
      else
	return FALSE;

      if (reversep)
      	{
	  tmp = itrue; itrue = ifalse; ifalse = tmp;
	  diff = trunc_int_for_mode (-diff, mode);
	}

      start_sequence ();
      target = noce_emit_store_flag (if_info, if_info->x, reversep, normalize);
      if (! target)
	{
	  end_sequence ();
	  return FALSE;
	}

      /* if (test) x = 3; else x = 4;
	 =>   x = 3 + (test == 0);  */
      if (diff == STORE_FLAG_VALUE || diff == -STORE_FLAG_VALUE)
	{
	  target = expand_simple_binop (mode,
					(diff == STORE_FLAG_VALUE
					 ? PLUS : MINUS),
					GEN_INT (ifalse), target, if_info->x, 0,
					OPTAB_WIDEN);
	}

      /* if (test) x = 8; else x = 0;
	 =>   x = (test != 0) << 3;  */
      else if (ifalse == 0 && (tmp = exact_log2 (itrue)) >= 0)
	{
	  target = expand_simple_binop (mode, ASHIFT,
					target, GEN_INT (tmp), if_info->x, 0,
					OPTAB_WIDEN);
	}

      /* if (test) x = -1; else x = b;
	 =>   x = -(test != 0) | b;  */
      else if (itrue == -1)
	{
	  target = expand_simple_binop (mode, IOR,
					target, GEN_INT (ifalse), if_info->x, 0,
					OPTAB_WIDEN);
	}

      /* if (test) x = a; else x = b;
	 =>   x = (-(test != 0) & (b - a)) + a;  */
      else
	{
	  target = expand_simple_binop (mode, AND,
					target, GEN_INT (diff), if_info->x, 0,
					OPTAB_WIDEN);
	  if (target)
	    target = expand_simple_binop (mode, PLUS,
					  target, GEN_INT (ifalse),
					  if_info->x, 0, OPTAB_WIDEN);
	}

      if (! target)
	{
	  end_sequence ();
	  return FALSE;
	}

      if (target != if_info->x)
	noce_emit_move_insn (if_info->x, target);

      seq = get_insns ();
      end_sequence ();

      if (seq_contains_jump (seq))
	return FALSE;

      emit_insn_before_scope (seq, if_info->jump, INSN_SCOPE (if_info->insn_a));

      return TRUE;
    }

  return FALSE;
}

/* Convert "if (test) foo++" into "foo += (test != 0)", and 
   similarly for "foo--".  */

static int
noce_try_store_flag_inc (if_info)
     struct noce_if_info *if_info;
{
  rtx target, seq;
  int subtract, normalize;

  if (! no_new_pseudos
      && (BRANCH_COST >= 2
	  || HAVE_incscc
	  || HAVE_decscc)
      /* Should be no `else' case to worry about.  */
      && if_info->b == if_info->x
      && GET_CODE (if_info->a) == PLUS
      && (XEXP (if_info->a, 1) == const1_rtx
	  || XEXP (if_info->a, 1) == constm1_rtx)
      && rtx_equal_p (XEXP (if_info->a, 0), if_info->x)
      && (reversed_comparison_code (if_info->cond, if_info->jump)
	  != UNKNOWN))
    {
      if (STORE_FLAG_VALUE == INTVAL (XEXP (if_info->a, 1)))
	subtract = 0, normalize = 0;
      else if (-STORE_FLAG_VALUE == INTVAL (XEXP (if_info->a, 1)))
	subtract = 1, normalize = 0;
      else
	subtract = 0, normalize = INTVAL (XEXP (if_info->a, 1));
      
      start_sequence ();

      target = noce_emit_store_flag (if_info,
				     gen_reg_rtx (GET_MODE (if_info->x)),
				     1, normalize);

      if (target)
	target = expand_simple_binop (GET_MODE (if_info->x),
				      subtract ? MINUS : PLUS,
				      if_info->x, target, if_info->x,
				      0, OPTAB_WIDEN);
      if (target)
	{
	  if (target != if_info->x)
	    noce_emit_move_insn (if_info->x, target);

	  seq = get_insns ();
	  end_sequence ();

	  if (seq_contains_jump (seq))
	    return FALSE;

	  emit_insn_before_scope (seq, if_info->jump,
				  INSN_SCOPE (if_info->insn_a));

	  return TRUE;
	}

      end_sequence ();
    }

  return FALSE;
}

/* Convert "if (test) x = 0;" to "x &= -(test == 0);"  */

static int
noce_try_store_flag_mask (if_info)
     struct noce_if_info *if_info;
{
  rtx target, seq;
  int reversep;

  reversep = 0;
  if (! no_new_pseudos
      && (BRANCH_COST >= 2
	  || STORE_FLAG_VALUE == -1)
      && ((if_info->a == const0_rtx
	   && rtx_equal_p (if_info->b, if_info->x))
	  || ((reversep = (reversed_comparison_code (if_info->cond,
						     if_info->jump)
			   != UNKNOWN))
	      && if_info->b == const0_rtx
	      && rtx_equal_p (if_info->a, if_info->x))))
    {
      start_sequence ();
      target = noce_emit_store_flag (if_info,
				     gen_reg_rtx (GET_MODE (if_info->x)),
				     reversep, -1);
      if (target)
        target = expand_simple_binop (GET_MODE (if_info->x), AND,
				      if_info->x, target, if_info->x, 0,
				      OPTAB_WIDEN);

      if (target)
	{
	  if (target != if_info->x)
	    noce_emit_move_insn (if_info->x, target);

	  seq = get_insns ();
	  end_sequence ();

	  if (seq_contains_jump (seq))
	    return FALSE;

	  emit_insn_before_scope (seq, if_info->jump,
				  INSN_SCOPE (if_info->insn_a));

	  return TRUE;
	}

      end_sequence ();
    }

  return FALSE;
}

/* Helper function for noce_try_cmove and noce_try_cmove_arith.  */

static rtx
noce_emit_cmove (if_info, x, code, cmp_a, cmp_b, vfalse, vtrue)
     struct noce_if_info *if_info;
     rtx x, cmp_a, cmp_b, vfalse, vtrue;
     enum rtx_code code;
{
  /* If earliest == jump, try to build the cmove insn directly.
     This is helpful when combine has created some complex condition
     (like for alpha's cmovlbs) that we can't hope to regenerate
     through the normal interface.  */

  if (if_info->cond_earliest == if_info->jump)
    {
      rtx tmp;

      tmp = gen_rtx_fmt_ee (code, GET_MODE (if_info->cond), cmp_a, cmp_b);
      tmp = gen_rtx_IF_THEN_ELSE (GET_MODE (x), tmp, vtrue, vfalse);
      tmp = gen_rtx_SET (VOIDmode, x, tmp);

      start_sequence ();
      tmp = emit_insn (tmp);

      if (recog_memoized (tmp) >= 0)
	{
	  tmp = get_insns ();
	  end_sequence ();
	  emit_insn (tmp);

	  return x;
	}

      end_sequence ();
    }

  /* Don't even try if the comparison operands are weird.  */
  if (! general_operand (cmp_a, GET_MODE (cmp_a))
      || ! general_operand (cmp_b, GET_MODE (cmp_b)))
    return NULL_RTX;

#if HAVE_conditional_move
  return emit_conditional_move (x, code, cmp_a, cmp_b, VOIDmode,
				vtrue, vfalse, GET_MODE (x),
			        (code == LTU || code == GEU
				 || code == LEU || code == GTU));
#else
  /* We'll never get here, as noce_process_if_block doesn't call the
     functions involved.  Ifdef code, however, should be discouraged
     because it leads to typos in the code not selected.  However, 
     emit_conditional_move won't exist either.  */
  return NULL_RTX;
#endif
}

/* Try only simple constants and registers here.  More complex cases
   are handled in noce_try_cmove_arith after noce_try_store_flag_arith
   has had a go at it.  */

static int
noce_try_cmove (if_info)
     struct noce_if_info *if_info;
{
  enum rtx_code code;
  rtx target, seq;

  if ((CONSTANT_P (if_info->a) || register_operand (if_info->a, VOIDmode))
      && (CONSTANT_P (if_info->b) || register_operand (if_info->b, VOIDmode)))
    {
      start_sequence ();

      code = GET_CODE (if_info->cond);
      target = noce_emit_cmove (if_info, if_info->x, code,
				XEXP (if_info->cond, 0),
				XEXP (if_info->cond, 1),
				if_info->a, if_info->b);

      if (target)
	{
	  if (target != if_info->x)
	    noce_emit_move_insn (if_info->x, target);

	  seq = get_insns ();
	  end_sequence ();
	  emit_insn_before_scope (seq, if_info->jump,
				  INSN_SCOPE (if_info->insn_a));
	  return TRUE;
	}
      else
	{
	  end_sequence ();
	  return FALSE;
	}
    }

  return FALSE;
}

/* Try more complex cases involving conditional_move.  */

static int
noce_try_cmove_arith (if_info)
     struct noce_if_info *if_info;
{
  rtx a = if_info->a;
  rtx b = if_info->b;
  rtx x = if_info->x;
  rtx insn_a, insn_b;
  rtx tmp, target;
  int is_mem = 0;
  enum rtx_code code;

  /* A conditional move from two memory sources is equivalent to a
     conditional on their addresses followed by a load.  Don't do this
     early because it'll screw alias analysis.  Note that we've
     already checked for no side effects.  */
  if (! no_new_pseudos && cse_not_expected
      && GET_CODE (a) == MEM && GET_CODE (b) == MEM
      && BRANCH_COST >= 5)
    {
      a = XEXP (a, 0);
      b = XEXP (b, 0);
      x = gen_reg_rtx (Pmode);
      is_mem = 1;
    }

  /* ??? We could handle this if we knew that a load from A or B could
     not fault.  This is also true if we've already loaded
     from the address along the path from ENTRY.  */
  else if (may_trap_p (a) || may_trap_p (b))
    return FALSE;

  /* if (test) x = a + b; else x = c - d;
     => y = a + b;
        x = c - d;
	if (test)
	  x = y;
  */
  
  code = GET_CODE (if_info->cond);
  insn_a = if_info->insn_a;
  insn_b = if_info->insn_b;

  /* Possibly rearrange operands to make things come out more natural.  */
  if (reversed_comparison_code (if_info->cond, if_info->jump) != UNKNOWN)
    {
      int reversep = 0;
      if (rtx_equal_p (b, x))
	reversep = 1;
      else if (general_operand (b, GET_MODE (b)))
	reversep = 1;

      if (reversep)
	{
	  code = reversed_comparison_code (if_info->cond, if_info->jump);
	  tmp = a, a = b, b = tmp;
	  tmp = insn_a, insn_a = insn_b, insn_b = tmp;
	}
    }

  start_sequence ();

  /* If either operand is complex, load it into a register first.
     The best way to do this is to copy the original insn.  In this
     way we preserve any clobbers etc that the insn may have had.  
     This is of course not possible in the IS_MEM case.  */
  if (! general_operand (a, GET_MODE (a)))
    {
      rtx set;

      if (no_new_pseudos)
	goto end_seq_and_fail;

      if (is_mem)
	{
	  tmp = gen_reg_rtx (GET_MODE (a));
	  tmp = emit_insn (gen_rtx_SET (VOIDmode, tmp, a));
	}
      else if (! insn_a)
	goto end_seq_and_fail;
      else
	{
	  a = gen_reg_rtx (GET_MODE (a));
	  tmp = copy_rtx (insn_a);
	  set = single_set (tmp);
	  SET_DEST (set) = a;
	  tmp = emit_insn (PATTERN (tmp));
	}
      if (recog_memoized (tmp) < 0)
	goto end_seq_and_fail;
    }
  if (! general_operand (b, GET_MODE (b)))
    {
      rtx set;

      if (no_new_pseudos)
	goto end_seq_and_fail;

      if (is_mem)
	{
          tmp = gen_reg_rtx (GET_MODE (b));
	  tmp = emit_insn (gen_rtx_SET (VOIDmode, tmp, b));
	}
      else if (! insn_b
#if 0
	       /* In the case we are going to duplicate insn originally
		  present in the front of comparsion, verify that the
		  comparsion didn't clobbered the operands.  */
	       || modified_between_p (SET_SRC (single_set (insn_b)),
				      if_info->cond_earliest,
				      NEXT_INSN (if_info->jump)))
#endif
	      )
	goto end_seq_and_fail;
      else
	{
          b = gen_reg_rtx (GET_MODE (b));
	  tmp = copy_rtx (insn_b);
	  set = single_set (tmp);
	  SET_DEST (set) = b;
	  tmp = emit_insn (PATTERN (tmp));
	}
      if (recog_memoized (tmp) < 0)
	goto end_seq_and_fail;
    }

  target = noce_emit_cmove (if_info, x, code, XEXP (if_info->cond, 0),
			    XEXP (if_info->cond, 1), a, b);

  if (! target)
    goto end_seq_and_fail;

  /* If we're handling a memory for above, emit the load now.  */
  if (is_mem)
    {
      tmp = gen_rtx_MEM (GET_MODE (if_info->x), target);

      /* Copy over flags as appropriate.  */
      if (MEM_VOLATILE_P (if_info->a) || MEM_VOLATILE_P (if_info->b))
	MEM_VOLATILE_P (tmp) = 1;
      if (MEM_IN_STRUCT_P (if_info->a) && MEM_IN_STRUCT_P (if_info->b))
	MEM_IN_STRUCT_P (tmp) = 1;
      if (MEM_SCALAR_P (if_info->a) && MEM_SCALAR_P (if_info->b))
	MEM_SCALAR_P (tmp) = 1;
      if (MEM_ALIAS_SET (if_info->a) == MEM_ALIAS_SET (if_info->b))
	set_mem_alias_set (tmp, MEM_ALIAS_SET (if_info->a));
      set_mem_align (tmp,
		     MIN (MEM_ALIGN (if_info->a), MEM_ALIGN (if_info->b)));

      noce_emit_move_insn (if_info->x, tmp);
    }
  else if (target != x)
    noce_emit_move_insn (x, target);

  tmp = get_insns ();
  end_sequence ();
  emit_insn_before_scope (tmp, if_info->jump, INSN_SCOPE (if_info->insn_a));
  return TRUE;

 end_seq_and_fail:
  end_sequence ();
  return FALSE;
}

/* For most cases, the simplified condition we found is the best
   choice, but this is not the case for the min/max/abs transforms.
   For these we wish to know that it is A or B in the condition.  */

static rtx
noce_get_alt_condition (if_info, target, earliest)
     struct noce_if_info *if_info;
     rtx target;
     rtx *earliest;
{
  rtx cond, set, insn;
  int reverse;

  /* If target is already mentioned in the known condition, return it.  */
  if (reg_mentioned_p (target, if_info->cond))
    {
      *earliest = if_info->cond_earliest;
      return if_info->cond;
    }

  set = pc_set (if_info->jump);
  cond = XEXP (SET_SRC (set), 0);
  reverse
    = GET_CODE (XEXP (SET_SRC (set), 2)) == LABEL_REF
      && XEXP (XEXP (SET_SRC (set), 2), 0) == JUMP_LABEL (if_info->jump);

  /* If we're looking for a constant, try to make the conditional
     have that constant in it.  There are two reasons why it may
     not have the constant we want:

     1. GCC may have needed to put the constant in a register, because
        the target can't compare directly against that constant.  For
        this case, we look for a SET immediately before the comparison
        that puts a constant in that register.

     2. GCC may have canonicalized the conditional, for example
	replacing "if x < 4" with "if x <= 3".  We can undo that (or
	make equivalent types of changes) to get the constants we need
	if they're off by one in the right direction.  */

  if (GET_CODE (target) == CONST_INT)
    {
      enum rtx_code code = GET_CODE (if_info->cond);
      rtx op_a = XEXP (if_info->cond, 0);
      rtx op_b = XEXP (if_info->cond, 1);
      rtx prev_insn;

      /* First, look to see if we put a constant in a register.  */
      prev_insn = PREV_INSN (if_info->cond_earliest);
      if (prev_insn
	  && INSN_P (prev_insn)
	  && GET_CODE (PATTERN (prev_insn)) == SET)
	{
	  rtx src = find_reg_equal_equiv_note (prev_insn);
	  if (!src)
	    src = SET_SRC (PATTERN (prev_insn));
	  if (GET_CODE (src) == CONST_INT)
	    {
	      if (rtx_equal_p (op_a, SET_DEST (PATTERN (prev_insn))))
		op_a = src;
	      else if (rtx_equal_p (op_b, SET_DEST (PATTERN (prev_insn))))
		op_b = src;

	      if (GET_CODE (op_a) == CONST_INT)
		{
		  rtx tmp = op_a;
		  op_a = op_b;
		  op_b = tmp;
		  code = swap_condition (code);
		}
	    }
	}

      /* Now, look to see if we can get the right constant by
	 adjusting the conditional.  */
      if (GET_CODE (op_b) == CONST_INT)
	{
	  HOST_WIDE_INT desired_val = INTVAL (target);
	  HOST_WIDE_INT actual_val = INTVAL (op_b);

	  switch (code)
	    {
	    case LT:
	      if (actual_val == desired_val + 1)
		{
		  code = LE;
		  op_b = GEN_INT (desired_val);
		}
	      break;
	    case LE:
	      if (actual_val == desired_val - 1)
		{
		  code = LT;
		  op_b = GEN_INT (desired_val);
		}
	      break;
	    case GT:
	      if (actual_val == desired_val - 1)
		{
		  code = GE;
		  op_b = GEN_INT (desired_val);
		}
	      break;
	    case GE:
	      if (actual_val == desired_val + 1)
		{
		  code = GT;
		  op_b = GEN_INT (desired_val);
		}
	      break;
	    default:
	      break;
	    }
	}

      /* If we made any changes, generate a new conditional that is
	 equivalent to what we started with, but has the right
	 constants in it.  */
      if (code != GET_CODE (if_info->cond)
	  || op_a != XEXP (if_info->cond, 0)
	  || op_b != XEXP (if_info->cond, 1))
	{
	  cond = gen_rtx_fmt_ee (code, GET_MODE (cond), op_a, op_b);
	  *earliest = if_info->cond_earliest;
	  return cond;
	}
    }

  cond = canonicalize_condition (if_info->jump, cond, reverse,
				 earliest, target);
  if (! cond || ! reg_mentioned_p (target, cond))
    return NULL;

  /* We almost certainly searched back to a different place.
     Need to re-verify correct lifetimes.  */

  /* X may not be mentioned in the range (cond_earliest, jump].  */
  for (insn = if_info->jump; insn != *earliest; insn = PREV_INSN (insn))
    if (INSN_P (insn) && reg_overlap_mentioned_p (if_info->x, PATTERN (insn)))
      return NULL;

  /* A and B may not be modified in the range [cond_earliest, jump).  */
  for (insn = *earliest; insn != if_info->jump; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& (modified_in_p (if_info->a, insn)
	    || modified_in_p (if_info->b, insn)))
      return NULL;

  return cond;
}

/* Convert "if (a < b) x = a; else x = b;" to "x = min(a, b);", etc.  */

static int
noce_try_minmax (if_info)
     struct noce_if_info *if_info;
{ 
  rtx cond, earliest, target, seq;
  enum rtx_code code, op;
  int unsignedp;

  /* ??? Can't guarantee that expand_binop won't create pseudos.  */
  if (no_new_pseudos)
    return FALSE;

  /* ??? Reject modes with NaNs or signed zeros since we don't know how
     they will be resolved with an SMIN/SMAX.  It wouldn't be too hard
     to get the target to tell us...  */
  if (HONOR_SIGNED_ZEROS (GET_MODE (if_info->x))
      || HONOR_NANS (GET_MODE (if_info->x)))
    return FALSE;

  cond = noce_get_alt_condition (if_info, if_info->a, &earliest);
  if (!cond)
    return FALSE;

  /* Verify the condition is of the form we expect, and canonicalize
     the comparison code.  */
  code = GET_CODE (cond);
  if (rtx_equal_p (XEXP (cond, 0), if_info->a))
    {
      if (! rtx_equal_p (XEXP (cond, 1), if_info->b))
	return FALSE;
    }
  else if (rtx_equal_p (XEXP (cond, 1), if_info->a))
    {
      if (! rtx_equal_p (XEXP (cond, 0), if_info->b))
	return FALSE;
      code = swap_condition (code);
    }
  else
    return FALSE;

  /* Determine what sort of operation this is.  Note that the code is for
     a taken branch, so the code->operation mapping appears backwards.  */
  switch (code)
    {
    case LT:
    case LE:
    case UNLT:
    case UNLE:
      op = SMAX;
      unsignedp = 0;
      break;
    case GT:
    case GE:
    case UNGT:
    case UNGE:
      op = SMIN;
      unsignedp = 0;
      break;
    case LTU:
    case LEU:
      op = UMAX;
      unsignedp = 1;
      break;
    case GTU:
    case GEU:
      op = UMIN;
      unsignedp = 1;
      break;
    default:
      return FALSE;
    }

  start_sequence ();

  target = expand_simple_binop (GET_MODE (if_info->x), op,
				if_info->a, if_info->b,
				if_info->x, unsignedp, OPTAB_WIDEN);
  if (! target)
    {
      end_sequence ();
      return FALSE;
    }
  if (target != if_info->x)
    noce_emit_move_insn (if_info->x, target);

  seq = get_insns ();
  end_sequence ();  

  if (seq_contains_jump (seq))
    return FALSE;

  emit_insn_before_scope (seq, if_info->jump, INSN_SCOPE (if_info->insn_a));
  if_info->cond = cond;
  if_info->cond_earliest = earliest;

  return TRUE;
}

/* Convert "if (a < 0) x = -a; else x = a;" to "x = abs(a);", etc.  */

static int
noce_try_abs (if_info)
     struct noce_if_info *if_info;
{ 
  rtx cond, earliest, target, seq, a, b, c;
  int negate;

  /* ??? Can't guarantee that expand_binop won't create pseudos.  */
  if (no_new_pseudos)
    return FALSE;

  /* Recognize A and B as constituting an ABS or NABS.  */
  a = if_info->a;
  b = if_info->b;
  if (GET_CODE (a) == NEG && rtx_equal_p (XEXP (a, 0), b))
    negate = 0;
  else if (GET_CODE (b) == NEG && rtx_equal_p (XEXP (b, 0), a))
    {
      c = a; a = b; b = c;
      negate = 1;
    }
  else
    return FALSE;
   
  cond = noce_get_alt_condition (if_info, b, &earliest);
  if (!cond)
    return FALSE;

  /* Verify the condition is of the form we expect.  */
  if (rtx_equal_p (XEXP (cond, 0), b))
    c = XEXP (cond, 1);
  else if (rtx_equal_p (XEXP (cond, 1), b))
    c = XEXP (cond, 0);
  else
    return FALSE;

  /* Verify that C is zero.  Search backward through the block for
     a REG_EQUAL note if necessary.  */
  if (REG_P (c))
    {
      rtx insn, note = NULL;
      for (insn = earliest;
	   insn != if_info->test_bb->head;
	   insn = PREV_INSN (insn))
	if (INSN_P (insn) 
	    && ((note = find_reg_note (insn, REG_EQUAL, c))
		|| (note = find_reg_note (insn, REG_EQUIV, c))))
	  break;
      if (! note)
	return FALSE;
      c = XEXP (note, 0);
    }
  if (GET_CODE (c) == MEM
      && GET_CODE (XEXP (c, 0)) == SYMBOL_REF
      && CONSTANT_POOL_ADDRESS_P (XEXP (c, 0)))
    c = get_pool_constant (XEXP (c, 0));

  /* Work around funny ideas get_condition has wrt canonicalization.
     Note that these rtx constants are known to be CONST_INT, and 
     therefore imply integer comparisons.  */
  if (c == constm1_rtx && GET_CODE (cond) == GT)
    ;
  else if (c == const1_rtx && GET_CODE (cond) == LT)
    ;
  else if (c != CONST0_RTX (GET_MODE (b)))
    return FALSE;

  /* Determine what sort of operation this is.  */
  switch (GET_CODE (cond))
    {
    case LT:
    case LE:
    case UNLT:
    case UNLE:
      negate = !negate;
      break;
    case GT:
    case GE:
    case UNGT:
    case UNGE:
      break;
    default:
      return FALSE;
    }

  start_sequence ();

  target = expand_simple_unop (GET_MODE (if_info->x), ABS, b, if_info->x, 0);

  /* ??? It's a quandry whether cmove would be better here, especially
     for integers.  Perhaps combine will clean things up.  */
  if (target && negate)
    target = expand_simple_unop (GET_MODE (target), NEG, target, if_info->x, 0);

  if (! target)
    {
      end_sequence ();
      return FALSE;
    }

  if (target != if_info->x)
    noce_emit_move_insn (if_info->x, target);

  seq = get_insns ();
  end_sequence ();  

  if (seq_contains_jump (seq))
    return FALSE;

  emit_insn_before_scope (seq, if_info->jump, INSN_SCOPE (if_info->insn_a));
  if_info->cond = cond;
  if_info->cond_earliest = earliest;

  return TRUE;
}

/* Similar to get_condition, only the resulting condition must be
   valid at JUMP, instead of at EARLIEST.  */

static rtx
noce_get_condition (jump, earliest)
     rtx jump;
     rtx *earliest;
{
  rtx cond, set, tmp, insn;
  bool reverse;

  if (! any_condjump_p (jump))
    return NULL_RTX;

  set = pc_set (jump);

  /* If this branches to JUMP_LABEL when the condition is false,
     reverse the condition.  */
  reverse = (GET_CODE (XEXP (SET_SRC (set), 2)) == LABEL_REF
	     && XEXP (XEXP (SET_SRC (set), 2), 0) == JUMP_LABEL (jump));

  /* If the condition variable is a register and is MODE_INT, accept it.  */

  cond = XEXP (SET_SRC (set), 0);
  tmp = XEXP (cond, 0);
  if (REG_P (tmp) && GET_MODE_CLASS (GET_MODE (tmp)) == MODE_INT)
    {
      *earliest = jump;

      if (reverse)
	cond = gen_rtx_fmt_ee (reverse_condition (GET_CODE (cond)),
			       GET_MODE (cond), tmp, XEXP (cond, 1));
      return cond;
    }

  /* Otherwise, fall back on canonicalize_condition to do the dirty
     work of manipulating MODE_CC values and COMPARE rtx codes.  */

  tmp = canonicalize_condition (jump, cond, reverse, earliest, NULL_RTX);
  if (!tmp)
    return NULL_RTX;

  /* We are going to insert code before JUMP, not before EARLIEST.
     We must therefore be certain that the given condition is valid
     at JUMP by virtue of not having been modified since.  */
  for (insn = *earliest; insn != jump; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && modified_in_p (tmp, insn))
      break;
  if (insn == jump)
    return tmp;

  /* The condition was modified.  See if we can get a partial result
     that doesn't follow all the reversals.  Perhaps combine can fold
     them together later.  */
  tmp = XEXP (tmp, 0);
  if (!REG_P (tmp) || GET_MODE_CLASS (GET_MODE (tmp)) != MODE_INT)
    return NULL_RTX;
  tmp = canonicalize_condition (jump, cond, reverse, earliest, tmp);
  if (!tmp)
    return NULL_RTX;

  /* For sanity's sake, re-validate the new result.  */
  for (insn = *earliest; insn != jump; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && modified_in_p (tmp, insn))
      return NULL_RTX;

  return tmp;
}

/* Return true if OP is ok for if-then-else processing.  */

static int
noce_operand_ok (op)
     rtx op;
{
  /* We special-case memories, so handle any of them with
     no address side effects.  */
  if (GET_CODE (op) == MEM)
    return ! side_effects_p (XEXP (op, 0));

  if (side_effects_p (op))
    return FALSE;

  return ! may_trap_p (op);
}

/* Given a simple IF-THEN or IF-THEN-ELSE block, attempt to convert it
   without using conditional execution.  Return TRUE if we were
   successful at converting the block.  */

static int
noce_process_if_block (ce_info)
     struct ce_if_block * ce_info;
{
  basic_block test_bb = ce_info->test_bb;	/* test block */
  basic_block then_bb = ce_info->then_bb;	/* THEN */
  basic_block else_bb = ce_info->else_bb;	/* ELSE or NULL */
  struct noce_if_info if_info;
  rtx insn_a, insn_b;
  rtx set_a, set_b;
  rtx orig_x, x, a, b;
  rtx jump, cond;

  /* We're looking for patterns of the form

     (1) if (...) x = a; else x = b;
     (2) x = b; if (...) x = a;
     (3) if (...) x = a;   // as if with an initial x = x.

     The later patterns require jumps to be more expensive.

     ??? For future expansion, look for multiple X in such patterns.  */

  /* If test is comprised of && or || elements, don't handle it unless it is
     the special case of && elements without an ELSE block.  */
  if (ce_info->num_multiple_test_blocks)
    {
      if (else_bb || ! ce_info->and_and_p)
	return FALSE;

      ce_info->test_bb = test_bb = ce_info->last_test_bb;
      ce_info->num_multiple_test_blocks = 0;
      ce_info->num_and_and_blocks = 0;
      ce_info->num_or_or_blocks = 0;
    }

  /* If this is not a standard conditional jump, we can't parse it.  */
  jump = test_bb->end;
  cond = noce_get_condition (jump, &if_info.cond_earliest);
  if (! cond)
    return FALSE;

  /* If the conditional jump is more than just a conditional
     jump, then we can not do if-conversion on this block.  */
  if (! onlyjump_p (jump))
    return FALSE;

  /* We must be comparing objects whose modes imply the size.  */
  if (GET_MODE (XEXP (cond, 0)) == BLKmode)
    return FALSE;

  /* Look for one of the potential sets.  */
  insn_a = first_active_insn (then_bb);
  if (! insn_a
      || insn_a != last_active_insn (then_bb, FALSE)
      || (set_a = single_set (insn_a)) == NULL_RTX)
    return FALSE;

  x = SET_DEST (set_a);
  a = SET_SRC (set_a);

  /* Look for the other potential set.  Make sure we've got equivalent
     destinations.  */
  /* ??? This is overconservative.  Storing to two different mems is
     as easy as conditionally computing the address.  Storing to a
     single mem merely requires a scratch memory to use as one of the
     destination addresses; often the memory immediately below the
     stack pointer is available for this.  */
  set_b = NULL_RTX;
  if (else_bb)
    {
      insn_b = first_active_insn (else_bb);
      if (! insn_b
	  || insn_b != last_active_insn (else_bb, FALSE)
	  || (set_b = single_set (insn_b)) == NULL_RTX
	  || ! rtx_equal_p (x, SET_DEST (set_b)))
	return FALSE;
    }
  else
    {
      insn_b = prev_nonnote_insn (if_info.cond_earliest);
      /* We're going to be moving the evaluation of B down from above
	 COND_EARLIEST to JUMP.  Make sure the relevant data is still
	 intact.  */
      if (! insn_b
	  || GET_CODE (insn_b) != INSN
	  || (set_b = single_set (insn_b)) == NULL_RTX
	  || ! rtx_equal_p (x, SET_DEST (set_b))
	  || reg_overlap_mentioned_p (x, SET_SRC (set_b))
	  || modified_between_p (SET_SRC (set_b),
				 PREV_INSN (if_info.cond_earliest), jump)
	  /* Likewise with X.  In particular this can happen when
	     noce_get_condition looks farther back in the instruction
	     stream than one might expect.  */
	  || reg_overlap_mentioned_p (x, cond)
	  || reg_overlap_mentioned_p (x, a)
	  || modified_between_p (x, PREV_INSN (if_info.cond_earliest), jump))
	insn_b = set_b = NULL_RTX;
    }

  /* If x has side effects then only the if-then-else form is safe to
     convert.  But even in that case we would need to restore any notes
     (such as REG_INC) at then end.  That can be tricky if 
     noce_emit_move_insn expands to more than one insn, so disable the
     optimization entirely for now if there are side effects.  */
  if (side_effects_p (x))
    return FALSE;

  b = (set_b ? SET_SRC (set_b) : x);

  /* Only operate on register destinations, and even then avoid extending
     the lifetime of hard registers on small register class machines.  */
  orig_x = x;
  if (GET_CODE (x) != REG
      || (SMALL_REGISTER_CLASSES
	  && REGNO (x) < FIRST_PSEUDO_REGISTER))
    {
      if (no_new_pseudos)
	return FALSE;
      x = gen_reg_rtx (GET_MODE (GET_CODE (x) == STRICT_LOW_PART
				 ? XEXP (x, 0) : x));
    }

  /* Don't operate on sources that may trap or are volatile.  */
  if (! noce_operand_ok (a) || ! noce_operand_ok (b))
    return FALSE;

  /* Set up the info block for our subroutines.  */
  if_info.test_bb = test_bb;
  if_info.cond = cond;
  if_info.jump = jump;
  if_info.insn_a = insn_a;
  if_info.insn_b = insn_b;
  if_info.x = x;
  if_info.a = a;
  if_info.b = b;

  /* Try optimizations in some approximation of a useful order.  */
  /* ??? Should first look to see if X is live incoming at all.  If it
     isn't, we don't need anything but an unconditional set.  */

  /* Look and see if A and B are really the same.  Avoid creating silly
     cmove constructs that no one will fix up later.  */
  if (rtx_equal_p (a, b))
    {
      /* If we have an INSN_B, we don't have to create any new rtl.  Just
	 move the instruction that we already have.  If we don't have an
	 INSN_B, that means that A == X, and we've got a noop move.  In
	 that case don't do anything and let the code below delete INSN_A.  */
      if (insn_b && else_bb)
	{
	  rtx note;

	  if (else_bb && insn_b == else_bb->end)
	    else_bb->end = PREV_INSN (insn_b);
	  reorder_insns (insn_b, insn_b, PREV_INSN (jump));

	  /* If there was a REG_EQUAL note, delete it since it may have been
	     true due to this insn being after a jump.  */
	  if ((note = find_reg_note (insn_b, REG_EQUAL, NULL_RTX)) != 0)
	    remove_note (insn_b, note);

	  insn_b = NULL_RTX;
	}
      /* If we have "x = b; if (...) x = a;", and x has side-effects, then
	 x must be executed twice.  */
      else if (insn_b && side_effects_p (orig_x))
	return FALSE;
	
      x = orig_x;
      goto success;
    }

  if (noce_try_store_flag (&if_info))
    goto success;
  if (noce_try_minmax (&if_info))
    goto success;
  if (noce_try_abs (&if_info))
    goto success;
  if (HAVE_conditional_move
      && noce_try_cmove (&if_info))
    goto success;
  if (! HAVE_conditional_execution)
    {
      if (noce_try_store_flag_constants (&if_info))
	goto success;
      if (noce_try_store_flag_inc (&if_info))
	goto success;
      if (noce_try_store_flag_mask (&if_info))
	goto success;
      if (HAVE_conditional_move
	  && noce_try_cmove_arith (&if_info))
	goto success;
    }

  return FALSE;

 success:
  /* The original sets may now be killed.  */
  delete_insn (insn_a);

  /* Several special cases here: First, we may have reused insn_b above,
     in which case insn_b is now NULL.  Second, we want to delete insn_b
     if it came from the ELSE block, because follows the now correct
     write that appears in the TEST block.  However, if we got insn_b from
     the TEST block, it may in fact be loading data needed for the comparison.
     We'll let life_analysis remove the insn if it's really dead.  */
  if (insn_b && else_bb)
    delete_insn (insn_b);

  /* The new insns will have been inserted immediately before the jump.  We
     should be able to remove the jump with impunity, but the condition itself
     may have been modified by gcse to be shared across basic blocks.  */
  delete_insn (jump);

  /* If we used a temporary, fix it up now.  */
  if (orig_x != x)
    {
      start_sequence ();
      noce_emit_move_insn (copy_rtx (orig_x), x);
      insn_b = get_insns ();
      end_sequence ();

      emit_insn_after_scope (insn_b, test_bb->end, INSN_SCOPE (insn_a));
    }

  /* Merge the blocks!  */
  merge_if_block (ce_info);

  return TRUE;
}

/* Attempt to convert an IF-THEN or IF-THEN-ELSE block into
   straight line code.  Return true if successful.  */

static int
process_if_block (ce_info)
     struct ce_if_block * ce_info;
{
  if (! reload_completed
      && noce_process_if_block (ce_info))
    return TRUE;

  if (HAVE_conditional_execution && reload_completed)
    {
      /* If we have && and || tests, try to first handle combining the && and
         || tests into the conditional code, and if that fails, go back and
         handle it without the && and ||, which at present handles the && case
         if there was no ELSE block.  */
      if (cond_exec_process_if_block (ce_info, TRUE))
	return TRUE;

      if (ce_info->num_multiple_test_blocks)
	{
	  cancel_changes (0);

	  if (cond_exec_process_if_block (ce_info, FALSE))
	    return TRUE;
	}
    }

  return FALSE;
}

/* Merge the blocks and mark for local life update.  */

static void
merge_if_block (ce_info)
     struct ce_if_block * ce_info;
{
  basic_block test_bb = ce_info->test_bb;	/* last test block */
  basic_block then_bb = ce_info->then_bb;	/* THEN */
  basic_block else_bb = ce_info->else_bb;	/* ELSE or NULL */
  basic_block join_bb = ce_info->join_bb;	/* join block */
  basic_block combo_bb;

  /* All block merging is done into the lower block numbers.  */

  combo_bb = test_bb;

  /* Merge any basic blocks to handle && and || subtests.  Each of
     the blocks are on the fallthru path from the predecessor block.  */
  if (ce_info->num_multiple_test_blocks > 0)
    {
      basic_block bb = test_bb;
      basic_block last_test_bb = ce_info->last_test_bb;
      basic_block fallthru = block_fallthru (bb);
      
      do
	{
	  bb = fallthru;
	  fallthru = block_fallthru (bb);
	  if (post_dominators)
	    delete_from_dominance_info (post_dominators, bb);
	  merge_blocks_nomove (combo_bb, bb);
	  num_removed_blocks++;
	}
      while (bb != last_test_bb);
    }

  /* Merge TEST block into THEN block.  Normally the THEN block won't have a
     label, but it might if there were || tests.  That label's count should be
     zero, and it normally should be removed.  */

  if (then_bb)
    {
      if (combo_bb->global_live_at_end)
	COPY_REG_SET (combo_bb->global_live_at_end,
		      then_bb->global_live_at_end);
      if (post_dominators)
	delete_from_dominance_info (post_dominators, then_bb);
      merge_blocks_nomove (combo_bb, then_bb);
      num_removed_blocks++;
    }

  /* The ELSE block, if it existed, had a label.  That label count
     will almost always be zero, but odd things can happen when labels
     get their addresses taken.  */
  if (else_bb)
    {
      if (post_dominators)
	delete_from_dominance_info (post_dominators, else_bb);
      merge_blocks_nomove (combo_bb, else_bb);
      num_removed_blocks++;
    }

  /* If there was no join block reported, that means it was not adjacent
     to the others, and so we cannot merge them.  */

  if (! join_bb)
    {
      rtx last = combo_bb->end;

      /* The outgoing edge for the current COMBO block should already
	 be correct.  Verify this.  */
      if (combo_bb->succ == NULL_EDGE)
	{
	  if (find_reg_note (last, REG_NORETURN, NULL))
	    ;
	  else if (GET_CODE (last) == INSN
		   && GET_CODE (PATTERN (last)) == TRAP_IF
		   && TRAP_CONDITION (PATTERN (last)) == const_true_rtx)
	    ;
	  else
	    abort ();
	}

      /* There should still be something at the end of the THEN or ELSE
         blocks taking us to our final destination.  */
      else if (GET_CODE (last) == JUMP_INSN)
	;
      else if (combo_bb->succ->dest == EXIT_BLOCK_PTR
	       && GET_CODE (last) == CALL_INSN
	       && SIBLING_CALL_P (last))
	;
      else if ((combo_bb->succ->flags & EDGE_EH)
	       && can_throw_internal (last))
	;
      else
	abort ();
    }

  /* The JOIN block may have had quite a number of other predecessors too.
     Since we've already merged the TEST, THEN and ELSE blocks, we should
     have only one remaining edge from our if-then-else diamond.  If there
     is more than one remaining edge, it must come from elsewhere.  There
     may be zero incoming edges if the THEN block didn't actually join 
     back up (as with a call to abort).  */
  else if ((join_bb->pred == NULL
	    || join_bb->pred->pred_next == NULL)
	   && join_bb != EXIT_BLOCK_PTR)
    {
      /* We can merge the JOIN.  */
      if (combo_bb->global_live_at_end)
	COPY_REG_SET (combo_bb->global_live_at_end,
		      join_bb->global_live_at_end);

      if (post_dominators)
	delete_from_dominance_info (post_dominators, join_bb);
      merge_blocks_nomove (combo_bb, join_bb);
      num_removed_blocks++;
    }
  else
    {
      /* We cannot merge the JOIN.  */

      /* The outgoing edge for the current COMBO block should already
	 be correct.  Verify this.  */
      if (combo_bb->succ->succ_next != NULL_EDGE
	  || combo_bb->succ->dest != join_bb)
	abort ();

      /* Remove the jump and cruft from the end of the COMBO block.  */
      if (join_bb != EXIT_BLOCK_PTR)
	tidy_fallthru_edge (combo_bb->succ, combo_bb, join_bb);
    }

  num_updated_if_blocks++;
}

/* Find a block ending in a simple IF condition and try to transform it
   in some way.  When converting a multi-block condition, put the new code
   in the first such block and delete the rest.  Return a pointer to this
   first block if some transformation was done.  Return NULL otherwise.  */

static basic_block
find_if_header (test_bb, pass)
     basic_block test_bb;
     int pass;
{
  ce_if_block_t ce_info;
  edge then_edge;
  edge else_edge;

  /* The kind of block we're looking for has exactly two successors.  */
  if ((then_edge = test_bb->succ) == NULL_EDGE
      || (else_edge = then_edge->succ_next) == NULL_EDGE
      || else_edge->succ_next != NULL_EDGE)
    return NULL;

  /* Neither edge should be abnormal.  */
  if ((then_edge->flags & EDGE_COMPLEX)
      || (else_edge->flags & EDGE_COMPLEX))
    return NULL;

  /* The THEN edge is canonically the one that falls through.  */
  if (then_edge->flags & EDGE_FALLTHRU)
    ;
  else if (else_edge->flags & EDGE_FALLTHRU)
    {
      edge e = else_edge;
      else_edge = then_edge;
      then_edge = e;
    }
  else
    /* Otherwise this must be a multiway branch of some sort.  */
    return NULL;

  memset ((PTR) &ce_info, '\0', sizeof (ce_info));
  ce_info.test_bb = test_bb;
  ce_info.then_bb = then_edge->dest;
  ce_info.else_bb = else_edge->dest;
  ce_info.pass = pass;

#ifdef IFCVT_INIT_EXTRA_FIELDS
  IFCVT_INIT_EXTRA_FIELDS (&ce_info);
#endif

  if (find_if_block (&ce_info))
    goto success;

  if (HAVE_trap && HAVE_conditional_trap
      && find_cond_trap (test_bb, then_edge, else_edge))
    goto success;

  if (post_dominators
      && (! HAVE_conditional_execution || reload_completed))
    {
      if (find_if_case_1 (test_bb, then_edge, else_edge))
	goto success;
      if (find_if_case_2 (test_bb, then_edge, else_edge))
	goto success;
    }

  return NULL;

 success:
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Conversion succeeded on pass %d.\n", pass);
  return ce_info.test_bb;
}

/* Return true if a block has two edges, one of which falls through to the next
   block, and the other jumps to a specific block, so that we can tell if the
   block is part of an && test or an || test.  Returns either -1 or the number
   of non-note, non-jump, non-USE/CLOBBER insns in the block.  */

static int
block_jumps_and_fallthru_p (cur_bb, target_bb)
     basic_block cur_bb;
     basic_block target_bb;
{
  edge cur_edge;
  int fallthru_p = FALSE;
  int jump_p = FALSE;
  rtx insn;
  rtx end;
  int n_insns = 0;

  if (!cur_bb || !target_bb)
    return -1;

  /* If no edges, obviously it doesn't jump or fallthru.  */
  if (cur_bb->succ == NULL_EDGE)
    return FALSE;

  for (cur_edge = cur_bb->succ;
       cur_edge != NULL_EDGE;
       cur_edge = cur_edge->succ_next)
    {
      if (cur_edge->flags & EDGE_COMPLEX)
	/* Anything complex isn't what we want.  */
	return -1;

      else if (cur_edge->flags & EDGE_FALLTHRU)
	fallthru_p = TRUE;

      else if (cur_edge->dest == target_bb)
	jump_p = TRUE;

      else
	return -1;
    }

  if ((jump_p & fallthru_p) == 0)
    return -1;

  /* Don't allow calls in the block, since this is used to group && and ||
     together for conditional execution support.  ??? we should support
     conditional execution support across calls for IA-64 some day, but
     for now it makes the code simpler.  */
  end = cur_bb->end;
  insn = cur_bb->head;

  while (insn != NULL_RTX)
    {
      if (GET_CODE (insn) == CALL_INSN)
	return -1;

      if (INSN_P (insn)
	  && GET_CODE (insn) != JUMP_INSN
	  && GET_CODE (PATTERN (insn)) != USE
	  && GET_CODE (PATTERN (insn)) != CLOBBER)
	n_insns++;

      if (insn == end)
	break;

      insn = NEXT_INSN (insn);
    }

  return n_insns;
}

/* Determine if a given basic block heads a simple IF-THEN or IF-THEN-ELSE
   block.  If so, we'll try to convert the insns to not require the branch.
   Return TRUE if we were successful at converting the block.  */

static int
find_if_block (ce_info)
     struct ce_if_block * ce_info;
{
  basic_block test_bb = ce_info->test_bb;
  basic_block then_bb = ce_info->then_bb;
  basic_block else_bb = ce_info->else_bb;
  basic_block join_bb = NULL_BLOCK;
  edge then_succ = then_bb->succ;
  edge else_succ = else_bb->succ;
  int then_predecessors;
  int else_predecessors;
  edge cur_edge;
  basic_block next;

  ce_info->last_test_bb = test_bb;

  /* Discover if any fall through predecessors of the current test basic block
     were && tests (which jump to the else block) or || tests (which jump to
     the then block).  */
  if (HAVE_conditional_execution && reload_completed
      && test_bb->pred != NULL_EDGE
      && test_bb->pred->pred_next == NULL_EDGE
      && test_bb->pred->flags == EDGE_FALLTHRU)
    {
      basic_block bb = test_bb->pred->src;
      basic_block target_bb;
      int max_insns = MAX_CONDITIONAL_EXECUTE;
      int n_insns;

      /* Determine if the preceeding block is an && or || block.  */
      if ((n_insns = block_jumps_and_fallthru_p (bb, else_bb)) >= 0)
	{
	  ce_info->and_and_p = TRUE;
	  target_bb = else_bb;
	}
      else if ((n_insns = block_jumps_and_fallthru_p (bb, then_bb)) >= 0)
	{
	  ce_info->and_and_p = FALSE;	  
	  target_bb = then_bb;
	}
      else
	target_bb = NULL_BLOCK;

      if (target_bb && n_insns <= max_insns)
	{
	  int total_insns = 0;
	  int blocks = 0;

	  ce_info->last_test_bb = test_bb;

	  /* Found at least one && or || block, look for more.  */
	  do
	    {
	      ce_info->test_bb = test_bb = bb;
	      total_insns += n_insns;
	      blocks++;

	      if (bb->pred == NULL_EDGE || bb->pred->pred_next != NULL_EDGE)
		break;

	      bb = bb->pred->src;
	      n_insns = block_jumps_and_fallthru_p (bb, target_bb);
	    }
	  while (n_insns >= 0 && (total_insns + n_insns) <= max_insns);

	  ce_info->num_multiple_test_blocks = blocks;
	  ce_info->num_multiple_test_insns = total_insns;

	  if (ce_info->and_and_p)
	    ce_info->num_and_and_blocks = blocks;
	  else
	    ce_info->num_or_or_blocks = blocks;
	}
    }

  /* Count the number of edges the THEN and ELSE blocks have.  */
  then_predecessors = 0;
  for (cur_edge = then_bb->pred;
       cur_edge != NULL_EDGE;
       cur_edge = cur_edge->pred_next)
    {
      then_predecessors++;
      if (cur_edge->flags & EDGE_COMPLEX)
	return FALSE;
    }

  else_predecessors = 0;
  for (cur_edge = else_bb->pred;
       cur_edge != NULL_EDGE;
       cur_edge = cur_edge->pred_next)
    {
      else_predecessors++;
      if (cur_edge->flags & EDGE_COMPLEX)
	return FALSE;
    }

  /* The THEN block of an IF-THEN combo must have exactly one predecessor,
     other than any || blocks which jump to the THEN block.  */
  if ((then_predecessors - ce_info->num_or_or_blocks) != 1)
    return FALSE;

  /* The THEN block of an IF-THEN combo must have zero or one successors.  */
  if (then_succ != NULL_EDGE
      && (then_succ->succ_next != NULL_EDGE
          || (then_succ->flags & EDGE_COMPLEX)
	  || (flow2_completed && tablejump_p (then_bb->end))))
    return FALSE;

  /* If the THEN block has no successors, conditional execution can still
     make a conditional call.  Don't do this unless the ELSE block has
     only one incoming edge -- the CFG manipulation is too ugly otherwise.
     Check for the last insn of the THEN block being an indirect jump, which
     is listed as not having any successors, but confuses the rest of the CE
     code processing.  ??? we should fix this in the future.  */
  if (then_succ == NULL)
    {
      if (else_bb->pred->pred_next == NULL_EDGE)
	{
	  rtx last_insn = then_bb->end;

	  while (last_insn
		 && GET_CODE (last_insn) == NOTE
		 && last_insn != then_bb->head)
	    last_insn = PREV_INSN (last_insn);

	  if (last_insn
	      && GET_CODE (last_insn) == JUMP_INSN
	      && ! simplejump_p (last_insn))
	    return FALSE;

	  join_bb = else_bb;
	  else_bb = NULL_BLOCK;
	}
      else
	return FALSE;
    }

  /* If the THEN block's successor is the other edge out of the TEST block,
     then we have an IF-THEN combo without an ELSE.  */
  else if (then_succ->dest == else_bb)
    {
      join_bb = else_bb;
      else_bb = NULL_BLOCK;
    }

  /* If the THEN and ELSE block meet in a subsequent block, and the ELSE
     has exactly one predecessor and one successor, and the outgoing edge
     is not complex, then we have an IF-THEN-ELSE combo.  */
  else if (else_succ != NULL_EDGE
	   && then_succ->dest == else_succ->dest
	   && else_bb->pred->pred_next == NULL_EDGE
	   && else_succ->succ_next == NULL_EDGE
	   && ! (else_succ->flags & EDGE_COMPLEX)
	   && ! (flow2_completed && tablejump_p (else_bb->end)))
    join_bb = else_succ->dest;

  /* Otherwise it is not an IF-THEN or IF-THEN-ELSE combination.  */
  else
    return FALSE;	   

  num_possible_if_blocks++;

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, "\nIF-THEN%s block found, pass %d, start block %d [insn %d], then %d [%d]",
	       (else_bb) ? "-ELSE" : "",
	       ce_info->pass,
	       test_bb->index, (test_bb->head) ? (int)INSN_UID (test_bb->head) : -1,
	       then_bb->index, (then_bb->head) ? (int)INSN_UID (then_bb->head) : -1);

      if (else_bb)
	fprintf (rtl_dump_file, ", else %d [%d]",
		 else_bb->index, (else_bb->head) ? (int)INSN_UID (else_bb->head) : -1);

      fprintf (rtl_dump_file, ", join %d [%d]",
	       join_bb->index, (join_bb->head) ? (int)INSN_UID (join_bb->head) : -1);

      if (ce_info->num_multiple_test_blocks > 0)
	fprintf (rtl_dump_file, ", %d %s block%s last test %d [%d]",
		 ce_info->num_multiple_test_blocks,
		 (ce_info->and_and_p) ? "&&" : "||",
		 (ce_info->num_multiple_test_blocks == 1) ? "" : "s",
		 ce_info->last_test_bb->index,
		 ((ce_info->last_test_bb->head)
		  ? (int)INSN_UID (ce_info->last_test_bb->head)
		  : -1));

      fputc ('\n', rtl_dump_file);
    }

  /* Make sure IF, THEN, and ELSE, blocks are adjacent.  Actually, we get the
     first condition for free, since we've already asserted that there's a
     fallthru edge from IF to THEN.  Likewise for the && and || blocks, since
     we checked the FALLTHRU flag, those are already adjacent to the last IF
     block.  */
  /* ??? As an enhancement, move the ELSE block.  Have to deal with
     BLOCK notes, if by no other means than aborting the merge if they
     exist.  Sticky enough I don't want to think about it now.  */
  next = then_bb;
  if (else_bb && (next = next->next_bb) != else_bb)
    return FALSE;
  if ((next = next->next_bb) != join_bb && join_bb != EXIT_BLOCK_PTR)
    {
      if (else_bb)
	join_bb = NULL;
      else
	return FALSE;
    }

  /* Do the real work.  */
  ce_info->else_bb = else_bb;
  ce_info->join_bb = join_bb;

  return process_if_block (ce_info);
}

/* Convert a branch over a trap, or a branch
   to a trap, into a conditional trap.  */

static int
find_cond_trap (test_bb, then_edge, else_edge)
     basic_block test_bb;
     edge then_edge, else_edge;
{
  basic_block then_bb = then_edge->dest;
  basic_block else_bb = else_edge->dest;
  basic_block other_bb, trap_bb;
  rtx trap, jump, cond, cond_earliest, seq;
  enum rtx_code code;

  /* Locate the block with the trap instruction.  */
  /* ??? While we look for no successors, we really ought to allow
     EH successors.  Need to fix merge_if_block for that to work.  */
  if ((trap = block_has_only_trap (then_bb)) != NULL)
    trap_bb = then_bb, other_bb = else_bb;
  else if ((trap = block_has_only_trap (else_bb)) != NULL)
    trap_bb = else_bb, other_bb = then_bb;
  else
    return FALSE;

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, "\nTRAP-IF block found, start %d, trap %d\n",
	       test_bb->index, trap_bb->index);
    }

  /* If this is not a standard conditional jump, we can't parse it.  */
  jump = test_bb->end;
  cond = noce_get_condition (jump, &cond_earliest);
  if (! cond)
    return FALSE;

  /* If the conditional jump is more than just a conditional jump, then
     we can not do if-conversion on this block.  */
  if (! onlyjump_p (jump))
    return FALSE;

  /* We must be comparing objects whose modes imply the size.  */
  if (GET_MODE (XEXP (cond, 0)) == BLKmode)
    return FALSE;

  /* Reverse the comparison code, if necessary.  */
  code = GET_CODE (cond);
  if (then_bb == trap_bb)
    {
      code = reversed_comparison_code (cond, jump);
      if (code == UNKNOWN)
	return FALSE;
    }

  /* Attempt to generate the conditional trap.  */
  seq = gen_cond_trap (code, XEXP (cond, 0), XEXP (cond, 1),
		       TRAP_CODE (PATTERN (trap)));
  if (seq == NULL)
    return FALSE;

  /* Emit the new insns before cond_earliest.  */
  emit_insn_before_scope (seq, cond_earliest, INSN_SCOPE (trap));

  /* Delete the trap block if possible.  */
  remove_edge (trap_bb == then_bb ? then_edge : else_edge);
  if (trap_bb->pred == NULL)
    {
      if (post_dominators)
	delete_from_dominance_info (post_dominators, trap_bb);
      flow_delete_block (trap_bb);
      num_removed_blocks++;
    }

  /* If the non-trap block and the test are now adjacent, merge them.
     Otherwise we must insert a direct branch.  */
  if (test_bb->next_bb == other_bb)
    {
      struct ce_if_block new_ce_info;
      delete_insn (jump);
      memset ((PTR) &new_ce_info, '\0', sizeof (new_ce_info));
      new_ce_info.test_bb = test_bb;
      new_ce_info.then_bb = NULL;
      new_ce_info.else_bb = NULL;
      new_ce_info.join_bb = other_bb;
      merge_if_block (&new_ce_info);
    }
  else
    {
      rtx lab, newjump;

      lab = JUMP_LABEL (jump);
      newjump = emit_jump_insn_after (gen_jump (lab), jump);
      LABEL_NUSES (lab) += 1;
      JUMP_LABEL (newjump) = lab;
      emit_barrier_after (newjump);

      delete_insn (jump);
    }

  return TRUE;
}

/* Subroutine of find_cond_trap: if BB contains only a trap insn, 
   return it.  */

static rtx
block_has_only_trap (bb)
     basic_block bb;
{
  rtx trap;

  /* We're not the exit block.  */
  if (bb == EXIT_BLOCK_PTR)
    return NULL_RTX;

  /* The block must have no successors.  */
  if (bb->succ)
    return NULL_RTX;

  /* The only instruction in the THEN block must be the trap.  */
  trap = first_active_insn (bb);
  if (! (trap == bb->end
	 && GET_CODE (PATTERN (trap)) == TRAP_IF
         && TRAP_CONDITION (PATTERN (trap)) == const_true_rtx))
    return NULL_RTX;

  return trap;
}

/* Look for IF-THEN-ELSE cases in which one of THEN or ELSE is
   transformable, but not necessarily the other.  There need be no
   JOIN block.

   Return TRUE if we were successful at converting the block.

   Cases we'd like to look at:

   (1)
	if (test) goto over; // x not live
	x = a;
	goto label;
	over:

   becomes

	x = a;
	if (! test) goto label;

   (2)
	if (test) goto E; // x not live
	x = big();
	goto L;
	E:
	x = b;
	goto M;

   becomes

	x = b;
	if (test) goto M;
	x = big();
	goto L;

   (3) // This one's really only interesting for targets that can do
       // multiway branching, e.g. IA-64 BBB bundles.  For other targets
       // it results in multiple branches on a cache line, which often
       // does not sit well with predictors.

	if (test1) goto E; // predicted not taken
	x = a;
	if (test2) goto F;
	...
	E:
	x = b;
	J:

   becomes

	x = a;
	if (test1) goto E;
	if (test2) goto F;

   Notes:

   (A) Don't do (2) if the branch is predicted against the block we're
   eliminating.  Do it anyway if we can eliminate a branch; this requires
   that the sole successor of the eliminated block postdominate the other
   side of the if.

   (B) With CE, on (3) we can steal from both sides of the if, creating

	if (test1) x = a;
	if (!test1) x = b;
	if (test1) goto J;
	if (test2) goto F;
	...
	J:

   Again, this is most useful if J postdominates.

   (C) CE substitutes for helpful life information.

   (D) These heuristics need a lot of work.  */

/* Tests for case 1 above.  */

static int
find_if_case_1 (test_bb, then_edge, else_edge)
      basic_block test_bb;
      edge then_edge, else_edge;
{
  basic_block then_bb = then_edge->dest;
  basic_block else_bb = else_edge->dest, new_bb;
  edge then_succ = then_bb->succ;
  int then_bb_index;

  /* THEN has one successor.  */
  if (!then_succ || then_succ->succ_next != NULL)
    return FALSE;

  /* THEN does not fall through, but is not strange either.  */
  if (then_succ->flags & (EDGE_COMPLEX | EDGE_FALLTHRU))
    return FALSE;

  /* THEN has one predecessor.  */
  if (then_bb->pred->pred_next != NULL)
    return FALSE;

  /* THEN must do something.  */
  if (forwarder_block_p (then_bb))
    return FALSE;

  num_possible_if_blocks++;
  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "\nIF-CASE-1 found, start %d, then %d\n",
	     test_bb->index, then_bb->index);

  /* THEN is small.  */
  if (count_bb_insns (then_bb) > BRANCH_COST)
    return FALSE;

  /* Registers set are dead, or are predicable.  */
  if (! dead_or_predicable (test_bb, then_bb, else_bb, 
			    then_bb->succ->dest, 1))
    return FALSE;

  /* Conversion went ok, including moving the insns and fixing up the
     jump.  Adjust the CFG to match.  */

  bitmap_operation (test_bb->global_live_at_end,
		    else_bb->global_live_at_start,
		    then_bb->global_live_at_end, BITMAP_IOR);
  
  new_bb = redirect_edge_and_branch_force (FALLTHRU_EDGE (test_bb), else_bb);
  then_bb_index = then_bb->index;
  if (post_dominators)
    delete_from_dominance_info (post_dominators, then_bb);
  flow_delete_block (then_bb);

  /* Make rest of code believe that the newly created block is the THEN_BB
     block we removed.  */
  if (new_bb)
    {
      new_bb->index = then_bb_index;
      BASIC_BLOCK (then_bb_index) = new_bb;
      if (post_dominators)
	add_to_dominance_info (post_dominators, new_bb);
    }
  /* We've possibly created jump to next insn, cleanup_cfg will solve that
     later.  */

  num_removed_blocks++;
  num_updated_if_blocks++;

  return TRUE;
}

/* Test for case 2 above.  */

static int
find_if_case_2 (test_bb, then_edge, else_edge)
      basic_block test_bb;
      edge then_edge, else_edge;
{
  basic_block then_bb = then_edge->dest;
  basic_block else_bb = else_edge->dest;
  edge else_succ = else_bb->succ;
  rtx note;

  /* ELSE has one successor.  */
  if (!else_succ || else_succ->succ_next != NULL)
    return FALSE;

  /* ELSE outgoing edge is not complex.  */
  if (else_succ->flags & EDGE_COMPLEX)
    return FALSE;

  /* ELSE has one predecessor.  */
  if (else_bb->pred->pred_next != NULL)
    return FALSE;

  /* THEN is not EXIT.  */
  if (then_bb->index < 0)
    return FALSE;

  /* ELSE is predicted or SUCC(ELSE) postdominates THEN.  */
  note = find_reg_note (test_bb->end, REG_BR_PROB, NULL_RTX);
  if (note && INTVAL (XEXP (note, 0)) >= REG_BR_PROB_BASE / 2)
    ;
  else if (else_succ->dest->index < 0
	   || dominated_by_p (post_dominators, then_bb, 
			      else_succ->dest))
    ;
  else
    return FALSE;

  num_possible_if_blocks++;
  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "\nIF-CASE-2 found, start %d, else %d\n",
	     test_bb->index, else_bb->index);

  /* ELSE is small.  */
  if (count_bb_insns (else_bb) > BRANCH_COST)
    return FALSE;

  /* Registers set are dead, or are predicable.  */
  if (! dead_or_predicable (test_bb, else_bb, then_bb, else_succ->dest, 0))
    return FALSE;

  /* Conversion went ok, including moving the insns and fixing up the
     jump.  Adjust the CFG to match.  */

  bitmap_operation (test_bb->global_live_at_end,
		    then_bb->global_live_at_start,
		    else_bb->global_live_at_end, BITMAP_IOR);
  
  if (post_dominators)
    delete_from_dominance_info (post_dominators, else_bb);
  flow_delete_block (else_bb);

  num_removed_blocks++;
  num_updated_if_blocks++;

  /* ??? We may now fallthru from one of THEN's successors into a join
     block.  Rerun cleanup_cfg?  Examine things manually?  Wait?  */

  return TRUE;
}

/* A subroutine of dead_or_predicable called through for_each_rtx.
   Return 1 if a memory is found.  */

static int
find_memory (px, data)
     rtx *px;
     void *data ATTRIBUTE_UNUSED;
{
  return GET_CODE (*px) == MEM;
}

/* Used by the code above to perform the actual rtl transformations.
   Return TRUE if successful.

   TEST_BB is the block containing the conditional branch.  MERGE_BB
   is the block containing the code to manipulate.  NEW_DEST is the
   label TEST_BB should be branching to after the conversion.
   REVERSEP is true if the sense of the branch should be reversed.  */

static int
dead_or_predicable (test_bb, merge_bb, other_bb, new_dest, reversep)
     basic_block test_bb, merge_bb, other_bb;
     basic_block new_dest;
     int reversep;
{
  rtx head, end, jump, earliest, old_dest, new_label = NULL_RTX;

  jump = test_bb->end;

  /* Find the extent of the real code in the merge block.  */
  head = merge_bb->head;
  end = merge_bb->end;

  if (GET_CODE (head) == CODE_LABEL)
    head = NEXT_INSN (head);
  if (GET_CODE (head) == NOTE)
    {
      if (head == end)
	{
	  head = end = NULL_RTX;
	  goto no_body;
	}
      head = NEXT_INSN (head);
    }

  if (GET_CODE (end) == JUMP_INSN)
    {
      rtx tmp, insn, label;

      if (head == end)
	{
	  head = end = NULL_RTX;
	  goto no_body;
	}

      /* If there is a jump table following merge_bb, fail
	 if there are any insn between head and PREV_INSN (end)
	 references it.  */
      if ((label = JUMP_LABEL (end)) != NULL_RTX
	  && (tmp = NEXT_INSN (label)) != NULL_RTX
	  && GET_CODE (tmp) == JUMP_INSN
	  && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
	      || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
	{
	  for (insn = head; insn != PREV_INSN (end); insn = NEXT_INSN (insn))
	    if (find_reg_note (insn, REG_LABEL, label))
	      return FALSE;
	}

      end = PREV_INSN (end);
    }

  /* Disable handling dead code by conditional execution if the machine needs
     to do anything funny with the tests, etc.  */
#ifndef IFCVT_MODIFY_TESTS
  if (HAVE_conditional_execution)
    {
      /* In the conditional execution case, we have things easy.  We know
	 the condition is reversable.  We don't have to check life info,
	 becase we're going to conditionally execute the code anyway.
	 All that's left is making sure the insns involved can actually
	 be predicated.  */

      rtx cond, prob_val;

      cond = cond_exec_get_condition (jump);
      if (! cond)
	return FALSE;

      prob_val = find_reg_note (jump, REG_BR_PROB, NULL_RTX);
      if (prob_val)
	prob_val = XEXP (prob_val, 0);

      if (reversep)
	{
	  enum rtx_code rev = reversed_comparison_code (cond, jump);
	  if (rev == UNKNOWN)
	    return FALSE;
	  cond = gen_rtx_fmt_ee (rev, GET_MODE (cond), XEXP (cond, 0),
			         XEXP (cond, 1));
	  if (prob_val)
	    prob_val = GEN_INT (REG_BR_PROB_BASE - INTVAL (prob_val));
	}

      if (! cond_exec_process_insns ((ce_if_block_t *)0, head, end, cond,
				     prob_val, 0))
	goto cancel;

      earliest = jump;
    }
  else
#endif
    {
      /* In the non-conditional execution case, we have to verify that there
	 are no trapping operations, no calls, no references to memory, and
	 that any registers modified are dead at the branch site.  */

      rtx insn, cond, prev;
      regset_head merge_set_head, tmp_head, test_live_head, test_set_head;
      regset merge_set, tmp, test_live, test_set;
      struct propagate_block_info *pbi;
      int i, fail = 0;

      /* Check for no calls or trapping operations.  */
      for (insn = head; ; insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == CALL_INSN)
	    return FALSE;
	  if (INSN_P (insn))
	    {
	      if (may_trap_p (PATTERN (insn)))
		return FALSE;

	      /* ??? Even non-trapping memories such as stack frame
		 references must be avoided.  For stores, we collect
		 no lifetime info; for reads, we'd have to assert
		 true_dependence false against every store in the
		 TEST range.  */
	      if (for_each_rtx (&PATTERN (insn), find_memory, NULL))
		return FALSE;
	    }
	  if (insn == end)
	    break;
	}

      if (! any_condjump_p (jump))
	return FALSE;

      /* Find the extent of the conditional.  */
      cond = noce_get_condition (jump, &earliest);
      if (! cond)
	return FALSE;

      /* Collect:
	   MERGE_SET = set of registers set in MERGE_BB
	   TEST_LIVE = set of registers live at EARLIEST
	   TEST_SET  = set of registers set between EARLIEST and the
		       end of the block.  */

      tmp = INITIALIZE_REG_SET (tmp_head);
      merge_set = INITIALIZE_REG_SET (merge_set_head);
      test_live = INITIALIZE_REG_SET (test_live_head);
      test_set = INITIALIZE_REG_SET (test_set_head);

      /* ??? bb->local_set is only valid during calculate_global_regs_live,
	 so we must recompute usage for MERGE_BB.  Not so bad, I suppose, 
         since we've already asserted that MERGE_BB is small.  */
      propagate_block (merge_bb, tmp, merge_set, merge_set, 0);

      /* For small register class machines, don't lengthen lifetimes of
	 hard registers before reload.  */
      if (SMALL_REGISTER_CLASSES && ! reload_completed)
	{
          EXECUTE_IF_SET_IN_BITMAP
	    (merge_set, 0, i,
	     {
	       if (i < FIRST_PSEUDO_REGISTER
		   && ! fixed_regs[i]
		   && ! global_regs[i])
		fail = 1;
	     });
	}

      /* For TEST, we're interested in a range of insns, not a whole block.
	 Moreover, we're interested in the insns live from OTHER_BB.  */

      COPY_REG_SET (test_live, other_bb->global_live_at_start);
      pbi = init_propagate_block_info (test_bb, test_live, test_set, test_set,
				       0);

      for (insn = jump; ; insn = prev)
	{
	  prev = propagate_one_insn (pbi, insn);
	  if (insn == earliest)
	    break;
	}

      free_propagate_block_info (pbi);

      /* We can perform the transformation if
	   MERGE_SET & (TEST_SET | TEST_LIVE)
	 and
	   TEST_SET & merge_bb->global_live_at_start
	 are empty.  */

      bitmap_operation (tmp, test_set, test_live, BITMAP_IOR);
      bitmap_operation (tmp, tmp, merge_set, BITMAP_AND);
      EXECUTE_IF_SET_IN_BITMAP(tmp, 0, i, fail = 1);

      bitmap_operation (tmp, test_set, merge_bb->global_live_at_start,
			BITMAP_AND);
      EXECUTE_IF_SET_IN_BITMAP(tmp, 0, i, fail = 1);

      FREE_REG_SET (tmp);
      FREE_REG_SET (merge_set);
      FREE_REG_SET (test_live);
      FREE_REG_SET (test_set);

      if (fail)
	return FALSE;
    }

 no_body:
  /* We don't want to use normal invert_jump or redirect_jump because
     we don't want to delete_insn called.  Also, we want to do our own
     change group management.  */

  old_dest = JUMP_LABEL (jump);
  if (other_bb != new_dest)
    {
      new_label = block_label (new_dest);
      if (reversep
	  ? ! invert_jump_1 (jump, new_label)
	  : ! redirect_jump_1 (jump, new_label))
	goto cancel;
    }

  if (! apply_change_group ())
    return FALSE;

  if (other_bb != new_dest)
    {
      if (old_dest)
	LABEL_NUSES (old_dest) -= 1;
      if (new_label)
	LABEL_NUSES (new_label) += 1;
      JUMP_LABEL (jump) = new_label;
      if (reversep)
	invert_br_probabilities (jump);

      redirect_edge_succ (BRANCH_EDGE (test_bb), new_dest);
      if (reversep)
	{
	  gcov_type count, probability;
	  count = BRANCH_EDGE (test_bb)->count;
	  BRANCH_EDGE (test_bb)->count = FALLTHRU_EDGE (test_bb)->count;
	  FALLTHRU_EDGE (test_bb)->count = count;
	  probability = BRANCH_EDGE (test_bb)->probability;
	  BRANCH_EDGE (test_bb)->probability
	    = FALLTHRU_EDGE (test_bb)->probability;
	  FALLTHRU_EDGE (test_bb)->probability = probability;
	  update_br_prob_note (test_bb);
	}
    }

  /* Move the insns out of MERGE_BB to before the branch.  */
  if (head != NULL)
    {
      if (end == merge_bb->end)
	merge_bb->end = PREV_INSN (head);

      if (squeeze_notes (&head, &end))
	return TRUE;

      reorder_insns (head, end, PREV_INSN (earliest));
    }

  /* Remove the jump and edge if we can.  */
  if (other_bb == new_dest)
    {
      delete_insn (jump);
      remove_edge (BRANCH_EDGE (test_bb));
      /* ??? Can't merge blocks here, as then_bb is still in use.
	 At minimum, the merge will get done just before bb-reorder.  */
    }

  return TRUE;

 cancel:
  cancel_changes (0);
  return FALSE;
}

/* Main entry point for all if-conversion.  */

void
if_convert (x_life_data_ok)
     int x_life_data_ok;
{
  basic_block bb;
  int pass;

  num_possible_if_blocks = 0;
  num_updated_if_blocks = 0;
  num_removed_blocks = 0;
  life_data_ok = (x_life_data_ok != 0);

  /* Free up basic_block_for_insn so that we don't have to keep it
     up to date, either here or in merge_blocks_nomove.  */
  free_basic_block_vars (1);

  /* Compute postdominators if we think we'll use them.  */
  post_dominators = NULL;
  if (HAVE_conditional_execution || life_data_ok)
    {
      post_dominators = calculate_dominance_info (CDI_POST_DOMINATORS);
    }
  if (life_data_ok)
    clear_bb_flags ();

  /* Go through each of the basic blocks looking for things to convert.  If we
     have conditional execution, we make multiple passes to allow us to handle
     IF-THEN{-ELSE} blocks within other IF-THEN{-ELSE} blocks.  */
  pass = 0;
  do
    {
      cond_exec_changed_p = FALSE;
      pass++;

#ifdef IFCVT_MULTIPLE_DUMPS
      if (rtl_dump_file && pass > 1)
	fprintf (rtl_dump_file, "\n\n========== Pass %d ==========\n", pass);
#endif

      FOR_EACH_BB (bb)
	{
	  basic_block new_bb;
	  while ((new_bb = find_if_header (bb, pass)))
	    bb = new_bb;
	}

#ifdef IFCVT_MULTIPLE_DUMPS
      if (rtl_dump_file && cond_exec_changed_p)
	print_rtl_with_bb (rtl_dump_file, get_insns ());
#endif
    }
  while (cond_exec_changed_p);

#ifdef IFCVT_MULTIPLE_DUMPS
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "\n\n========== no more changes\n");
#endif

  if (post_dominators)
    free_dominance_info (post_dominators);

  if (rtl_dump_file)
    fflush (rtl_dump_file);

  clear_aux_for_blocks ();

  /* Rebuild life info for basic blocks that require it.  */
  if (num_removed_blocks && life_data_ok)
    {
      /* If we allocated new pseudos, we must resize the array for sched1.  */
      if (max_regno < max_reg_num ())
	{
	  max_regno = max_reg_num ();
	  allocate_reg_info (max_regno, FALSE, FALSE);
	}
      update_life_info_in_dirty_blocks (UPDATE_LIFE_GLOBAL_RM_NOTES,
					PROP_DEATH_NOTES | PROP_SCAN_DEAD_CODE
					| PROP_KILL_DEAD_CODE);
    }

  /* Write the final stats.  */
  if (rtl_dump_file && num_possible_if_blocks > 0)
    {
      fprintf (rtl_dump_file,
	       "\n%d possible IF blocks searched.\n",
	       num_possible_if_blocks);
      fprintf (rtl_dump_file,
	       "%d IF blocks converted.\n",
	       num_updated_if_blocks);
      fprintf (rtl_dump_file,
	       "%d basic blocks deleted.\n\n\n",
	       num_removed_blocks);
    }

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}
