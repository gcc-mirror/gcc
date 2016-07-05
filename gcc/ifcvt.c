/* If-conversion support.
   Copyright (C) 2000-2016 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

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
#include "cfghooks.h"
#include "df.h"
#include "tm_p.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"

#include "cfgrtl.h"
#include "cfganal.h"
#include "cfgcleanup.h"
#include "expr.h"
#include "output.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "dbgcnt.h"
#include "shrink-wrap.h"
#include "rtl-iter.h"
#include "ifcvt.h"
#include "params.h"

#ifndef MAX_CONDITIONAL_EXECUTE
#define MAX_CONDITIONAL_EXECUTE \
  (BRANCH_COST (optimize_function_for_speed_p (cfun), false) \
   + 1)
#endif

#define IFCVT_MULTIPLE_DUMPS 1

#define NULL_BLOCK	((basic_block) NULL)

/* True if after combine pass.  */
static bool ifcvt_after_combine;

/* True if the target has the cbranchcc4 optab.  */
static bool have_cbranchcc4;

/* # of IF-THEN or IF-THEN-ELSE blocks we looked at  */
static int num_possible_if_blocks;

/* # of IF-THEN or IF-THEN-ELSE blocks were converted to conditional
   execution.  */
static int num_updated_if_blocks;

/* # of changes made.  */
static int num_true_changes;

/* Whether conditional execution changes were made.  */
static int cond_exec_changed_p;

/* Forward references.  */
static int count_bb_insns (const_basic_block);
static bool cheap_bb_rtx_cost_p (const_basic_block, int, int);
static rtx_insn *first_active_insn (basic_block);
static rtx_insn *last_active_insn (basic_block, int);
static rtx_insn *find_active_insn_before (basic_block, rtx_insn *);
static rtx_insn *find_active_insn_after (basic_block, rtx_insn *);
static basic_block block_fallthru (basic_block);
static int cond_exec_process_insns (ce_if_block *, rtx_insn *, rtx, rtx, int,
				    int);
static rtx cond_exec_get_condition (rtx_insn *);
static rtx noce_get_condition (rtx_insn *, rtx_insn **, bool);
static int noce_operand_ok (const_rtx);
static void merge_if_block (ce_if_block *);
static int find_cond_trap (basic_block, edge, edge);
static basic_block find_if_header (basic_block, int);
static int block_jumps_and_fallthru_p (basic_block, basic_block);
static int noce_find_if_block (basic_block, edge, edge, int);
static int cond_exec_find_if_block (ce_if_block *);
static int find_if_case_1 (basic_block, edge, edge);
static int find_if_case_2 (basic_block, edge, edge);
static int dead_or_predicable (basic_block, basic_block, basic_block,
			       edge, int);
static void noce_emit_move_insn (rtx, rtx);
static rtx_insn *block_has_only_trap (basic_block);

/* Count the number of non-jump active insns in BB.  */

static int
count_bb_insns (const_basic_block bb)
{
  int count = 0;
  rtx_insn *insn = BB_HEAD (bb);

  while (1)
    {
      if (active_insn_p (insn) && !JUMP_P (insn))
	count++;

      if (insn == BB_END (bb))
	break;
      insn = NEXT_INSN (insn);
    }

  return count;
}

/* Determine whether the total insn_rtx_cost on non-jump insns in
   basic block BB is less than MAX_COST.  This function returns
   false if the cost of any instruction could not be estimated. 

   The cost of the non-jump insns in BB is scaled by REG_BR_PROB_BASE
   as those insns are being speculated.  MAX_COST is scaled with SCALE
   plus a small fudge factor.  */

static bool
cheap_bb_rtx_cost_p (const_basic_block bb, int scale, int max_cost)
{
  int count = 0;
  rtx_insn *insn = BB_HEAD (bb);
  bool speed = optimize_bb_for_speed_p (bb);

  /* Set scale to REG_BR_PROB_BASE to void the identical scaling
     applied to insn_rtx_cost when optimizing for size.  Only do
     this after combine because if-conversion might interfere with
     passes before combine.

     Use optimize_function_for_speed_p instead of the pre-defined
     variable speed to make sure it is set to same value for all
     basic blocks in one if-conversion transformation.  */
  if (!optimize_function_for_speed_p (cfun) && ifcvt_after_combine)
    scale = REG_BR_PROB_BASE;
  /* Our branch probability/scaling factors are just estimates and don't
     account for cases where we can get speculation for free and other
     secondary benefits.  So we fudge the scale factor to make speculating
     appear a little more profitable when optimizing for performance.  */
  else
    scale += REG_BR_PROB_BASE / 8;


  max_cost *= scale;

  while (1)
    {
      if (NONJUMP_INSN_P (insn))
	{
	  int cost = insn_rtx_cost (PATTERN (insn), speed) * REG_BR_PROB_BASE;
	  if (cost == 0)
	    return false;

	  /* If this instruction is the load or set of a "stack" register,
	     such as a floating point register on x87, then the cost of
	     speculatively executing this insn may need to include
	     the additional cost of popping its result off of the
	     register stack.  Unfortunately, correctly recognizing and
	     accounting for this additional overhead is tricky, so for
	     now we simply prohibit such speculative execution.  */
#ifdef STACK_REGS
	  {
	    rtx set = single_set (insn);
	    if (set && STACK_REG_P (SET_DEST (set)))
	      return false;
	  }
#endif

	  count += cost;
	  if (count >= max_cost)
	    return false;
	}
      else if (CALL_P (insn))
	return false;

      if (insn == BB_END (bb))
	break;
      insn = NEXT_INSN (insn);
    }

  return true;
}

/* Return the first non-jump active insn in the basic block.  */

static rtx_insn *
first_active_insn (basic_block bb)
{
  rtx_insn *insn = BB_HEAD (bb);

  if (LABEL_P (insn))
    {
      if (insn == BB_END (bb))
	return NULL;
      insn = NEXT_INSN (insn);
    }

  while (NOTE_P (insn) || DEBUG_INSN_P (insn))
    {
      if (insn == BB_END (bb))
	return NULL;
      insn = NEXT_INSN (insn);
    }

  if (JUMP_P (insn))
    return NULL;

  return insn;
}

/* Return the last non-jump active (non-jump) insn in the basic block.  */

static rtx_insn *
last_active_insn (basic_block bb, int skip_use_p)
{
  rtx_insn *insn = BB_END (bb);
  rtx_insn *head = BB_HEAD (bb);

  while (NOTE_P (insn)
	 || JUMP_P (insn)
	 || DEBUG_INSN_P (insn)
	 || (skip_use_p
	     && NONJUMP_INSN_P (insn)
	     && GET_CODE (PATTERN (insn)) == USE))
    {
      if (insn == head)
	return NULL;
      insn = PREV_INSN (insn);
    }

  if (LABEL_P (insn))
    return NULL;

  return insn;
}

/* Return the active insn before INSN inside basic block CURR_BB. */

static rtx_insn *
find_active_insn_before (basic_block curr_bb, rtx_insn *insn)
{
  if (!insn || insn == BB_HEAD (curr_bb))
    return NULL;

  while ((insn = PREV_INSN (insn)) != NULL_RTX)
    {
      if (NONJUMP_INSN_P (insn) || JUMP_P (insn) || CALL_P (insn))
        break;

      /* No other active insn all the way to the start of the basic block. */
      if (insn == BB_HEAD (curr_bb))
        return NULL;
    }

  return insn;
}

/* Return the active insn after INSN inside basic block CURR_BB. */

static rtx_insn *
find_active_insn_after (basic_block curr_bb, rtx_insn *insn)
{
  if (!insn || insn == BB_END (curr_bb))
    return NULL;

  while ((insn = NEXT_INSN (insn)) != NULL_RTX)
    {
      if (NONJUMP_INSN_P (insn) || JUMP_P (insn) || CALL_P (insn))
        break;

      /* No other active insn all the way to the end of the basic block. */
      if (insn == BB_END (curr_bb))
        return NULL;
    }

  return insn;
}

/* Return the basic block reached by falling though the basic block BB.  */

static basic_block
block_fallthru (basic_block bb)
{
  edge e = find_fallthru_edge (bb->succs);

  return (e) ? e->dest : NULL_BLOCK;
}

/* Return true if RTXs A and B can be safely interchanged.  */

static bool
rtx_interchangeable_p (const_rtx a, const_rtx b)
{
  if (!rtx_equal_p (a, b))
    return false;

  if (GET_CODE (a) != MEM)
    return true;

  /* A dead type-unsafe memory reference is legal, but a live type-unsafe memory
     reference is not.  Interchanging a dead type-unsafe memory reference with
     a live type-safe one creates a live type-unsafe memory reference, in other
     words, it makes the program illegal.
     We check here conservatively whether the two memory references have equal
     memory attributes.  */

  return mem_attrs_eq_p (get_mem_attrs (a), get_mem_attrs (b));
}


/* Go through a bunch of insns, converting them to conditional
   execution format if possible.  Return TRUE if all of the non-note
   insns were processed.  */

static int
cond_exec_process_insns (ce_if_block *ce_info ATTRIBUTE_UNUSED,
			 /* if block information */rtx_insn *start,
			 /* first insn to look at */rtx end,
			 /* last insn to look at */rtx test,
			 /* conditional execution test */int prob_val,
			 /* probability of branch taken. */int mod_ok)
{
  int must_be_last = FALSE;
  rtx_insn *insn;
  rtx xtest;
  rtx pattern;

  if (!start || !end)
    return FALSE;

  for (insn = start; ; insn = NEXT_INSN (insn))
    {
      /* dwarf2out can't cope with conditional prologues.  */
      if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_PROLOGUE_END)
	return FALSE;

      if (NOTE_P (insn) || DEBUG_INSN_P (insn))
	goto insn_done;

      gcc_assert (NONJUMP_INSN_P (insn) || CALL_P (insn));

      /* dwarf2out can't cope with conditional unwind info.  */
      if (RTX_FRAME_RELATED_P (insn))
	return FALSE;

      /* Remove USE insns that get in the way.  */
      if (reload_completed && GET_CODE (PATTERN (insn)) == USE)
	{
	  /* ??? Ug.  Actually unlinking the thing is problematic,
	     given what we'd have to coordinate with our callers.  */
	  SET_INSN_DELETED (insn);
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

      if (CALL_P (insn) && prob_val >= 0)
	validate_change (insn, &REG_NOTES (insn),
			 gen_rtx_INT_LIST ((machine_mode) REG_BR_PROB,
					   prob_val, REG_NOTES (insn)), 1);

    insn_done:
      if (insn == end)
	break;
    }

  return TRUE;
}

/* Return the condition for a jump.  Do not do any special processing.  */

static rtx
cond_exec_get_condition (rtx_insn *jump)
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
      && LABEL_REF_LABEL (XEXP (test_if, 2)) == JUMP_LABEL (jump))
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
cond_exec_process_if_block (ce_if_block * ce_info,
			    /* if block information */int do_multiple_p)
{
  basic_block test_bb = ce_info->test_bb;	/* last test block */
  basic_block then_bb = ce_info->then_bb;	/* THEN */
  basic_block else_bb = ce_info->else_bb;	/* ELSE or NULL */
  rtx test_expr;		/* expression in IF_THEN_ELSE that is tested */
  rtx_insn *then_start;		/* first insn in THEN block */
  rtx_insn *then_end;		/* last insn + 1 in THEN block */
  rtx_insn *else_start = NULL;	/* first insn in ELSE block or NULL */
  rtx_insn *else_end = NULL;	/* last insn + 1 in ELSE block */
  int max;			/* max # of insns to convert.  */
  int then_mod_ok;		/* whether conditional mods are ok in THEN */
  rtx true_expr;		/* test for else block insns */
  rtx false_expr;		/* test for then block insns */
  int true_prob_val;		/* probability of else block */
  int false_prob_val;		/* probability of then block */
  rtx_insn *then_last_head = NULL;	/* Last match at the head of THEN */
  rtx_insn *else_last_head = NULL;	/* Last match at the head of ELSE */
  rtx_insn *then_first_tail = NULL;	/* First match at the tail of THEN */
  rtx_insn *else_first_tail = NULL;	/* First match at the tail of ELSE */
  int then_n_insns, else_n_insns, n_insns;
  enum rtx_code false_code;
  rtx note;

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
  test_expr = cond_exec_get_condition (BB_END (test_bb));
  if (! test_expr)
    return FALSE;

  /* If the conditional jump is more than just a conditional jump,
     then we can not do conditional execution conversion on this block.  */
  if (! onlyjump_p (BB_END (test_bb)))
    return FALSE;

  /* Collect the bounds of where we're to search, skipping any labels, jumps
     and notes at the beginning and end of the block.  Then count the total
     number of insns and see if it is small enough to convert.  */
  then_start = first_active_insn (then_bb);
  then_end = last_active_insn (then_bb, TRUE);
  then_n_insns = ce_info->num_then_insns = count_bb_insns (then_bb);
  n_insns = then_n_insns;
  max = MAX_CONDITIONAL_EXECUTE;

  if (else_bb)
    {
      int n_matching;

      max *= 2;
      else_start = first_active_insn (else_bb);
      else_end = last_active_insn (else_bb, TRUE);
      else_n_insns = ce_info->num_else_insns = count_bb_insns (else_bb);
      n_insns += else_n_insns;

      /* Look for matching sequences at the head and tail of the two blocks,
	 and limit the range of insns to be converted if possible.  */
      n_matching = flow_find_cross_jump (then_bb, else_bb,
					 &then_first_tail, &else_first_tail,
					 NULL);
      if (then_first_tail == BB_HEAD (then_bb))
	then_start = then_end = NULL;
      if (else_first_tail == BB_HEAD (else_bb))
	else_start = else_end = NULL;

      if (n_matching > 0)
	{
	  if (then_end)
	    then_end = find_active_insn_before (then_bb, then_first_tail);
	  if (else_end)
	    else_end = find_active_insn_before (else_bb, else_first_tail);
	  n_insns -= 2 * n_matching;
	}

      if (then_start
	  && else_start
	  && then_n_insns > n_matching
	  && else_n_insns > n_matching)
	{
	  int longest_match = MIN (then_n_insns - n_matching,
				   else_n_insns - n_matching);
	  n_matching
	    = flow_find_head_matching_sequence (then_bb, else_bb,
						&then_last_head,
						&else_last_head,
						longest_match);

	  if (n_matching > 0)
	    {
	      rtx_insn *insn;

	      /* We won't pass the insns in the head sequence to
		 cond_exec_process_insns, so we need to test them here
		 to make sure that they don't clobber the condition.  */
	      for (insn = BB_HEAD (then_bb);
		   insn != NEXT_INSN (then_last_head);
		   insn = NEXT_INSN (insn))
		if (!LABEL_P (insn) && !NOTE_P (insn)
		    && !DEBUG_INSN_P (insn)
		    && modified_in_p (test_expr, insn))
		  return FALSE;
	    }

	  if (then_last_head == then_end)
	    then_start = then_end = NULL;
	  if (else_last_head == else_end)
	    else_start = else_end = NULL;

	  if (n_matching > 0)
	    {
	      if (then_start)
		then_start = find_active_insn_after (then_bb, then_last_head);
	      if (else_start)
		else_start = find_active_insn_after (else_bb, else_last_head);
	      n_insns -= 2 * n_matching;
	    }
	}
    }

  if (n_insns > max)
    return FALSE;

  /* Map test_expr/test_jump into the appropriate MD tests to use on
     the conditionally executed code.  */

  true_expr = test_expr;

  false_code = reversed_comparison_code (true_expr, BB_END (test_bb));
  if (false_code != UNKNOWN)
    false_expr = gen_rtx_fmt_ee (false_code, GET_MODE (true_expr),
				 XEXP (true_expr, 0), XEXP (true_expr, 1));
  else
    false_expr = NULL_RTX;

#ifdef IFCVT_MODIFY_TESTS
  /* If the machine description needs to modify the tests, such as setting a
     conditional execution register from a comparison, it can do so here.  */
  IFCVT_MODIFY_TESTS (ce_info, true_expr, false_expr);

  /* See if the conversion failed.  */
  if (!true_expr || !false_expr)
    goto fail;
#endif

  note = find_reg_note (BB_END (test_bb), REG_BR_PROB, NULL_RTX);
  if (note)
    {
      true_prob_val = XINT (note, 0);
      false_prob_val = REG_BR_PROB_BASE - true_prob_val;
    }
  else
    {
      true_prob_val = -1;
      false_prob_val = -1;
    }

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
	  rtx_insn *start, *end;
	  rtx t, f;
	  enum rtx_code f_code;

	  bb = block_fallthru (bb);
	  start = first_active_insn (bb);
	  end = last_active_insn (bb, TRUE);
	  if (start
	      && ! cond_exec_process_insns (ce_info, start, end, false_expr,
					    false_prob_val, FALSE))
	    goto fail;

	  /* If the conditional jump is more than just a conditional jump, then
	     we can not do conditional execution conversion on this block.  */
	  if (! onlyjump_p (BB_END (bb)))
	    goto fail;

	  /* Find the conditional jump and isolate the test.  */
	  t = cond_exec_get_condition (BB_END (bb));
	  if (! t)
	    goto fail;

	  f_code = reversed_comparison_code (t, BB_END (bb));
	  if (f_code == UNKNOWN)
	    goto fail;

	  f = gen_rtx_fmt_ee (f_code, GET_MODE (t), XEXP (t, 0), XEXP (t, 1));
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

	  /* See if the conversion failed.  */
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
  /* Do any machine dependent final modifications.  */
  IFCVT_MODIFY_FINAL (ce_info);
#endif

  /* Conversion succeeded.  */
  if (dump_file)
    fprintf (dump_file, "%d insn%s converted to conditional execution.\n",
	     n_insns, (n_insns == 1) ? " was" : "s were");

  /* Merge the blocks!  If we had matching sequences, make sure to delete one
     copy at the appropriate location first: delete the copy in the THEN branch
     for a tail sequence so that the remaining one is executed last for both
     branches, and delete the copy in the ELSE branch for a head sequence so
     that the remaining one is executed first for both branches.  */
  if (then_first_tail)
    {
      rtx_insn *from = then_first_tail;
      if (!INSN_P (from))
	from = find_active_insn_after (then_bb, from);
      delete_insn_chain (from, get_last_bb_insn (then_bb), false);
    }
  if (else_last_head)
    delete_insn_chain (first_active_insn (else_bb), else_last_head, false);

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
  /* The basic blocks that make up the IF-THEN-{ELSE-,}JOIN block.  */
  basic_block test_bb, then_bb, else_bb, join_bb;

  /* The jump that ends TEST_BB.  */
  rtx_insn *jump;

  /* The jump condition.  */
  rtx cond;

  /* New insns should be inserted before this one.  */
  rtx_insn *cond_earliest;

  /* Insns in the THEN and ELSE block.  There is always just this
     one insns in those blocks.  The insns are single_set insns.
     If there was no ELSE block, INSN_B is the last insn before
     COND_EARLIEST, or NULL_RTX.  In the former case, the insn
     operands are still valid, as if INSN_B was moved down below
     the jump.  */
  rtx_insn *insn_a, *insn_b;

  /* The SET_SRC of INSN_A and INSN_B.  */
  rtx a, b;

  /* The SET_DEST of INSN_A.  */
  rtx x;

  /* The original set destination that the THEN and ELSE basic blocks finally
     write their result to.  */
  rtx orig_x;
  /* True if this if block is not canonical.  In the canonical form of
     if blocks, the THEN_BB is the block reached via the fallthru edge
     from TEST_BB.  For the noce transformations, we allow the symmetric
     form as well.  */
  bool then_else_reversed;

  /* True if the contents of then_bb and else_bb are a
     simple single set instruction.  */
  bool then_simple;
  bool else_simple;

  /* The total rtx cost of the instructions in then_bb and else_bb.  */
  unsigned int then_cost;
  unsigned int else_cost;

  /* Estimated cost of the particular branch instruction.  */
  unsigned int branch_cost;

  /* The name of the noce transform that succeeded in if-converting
     this structure.  Used for debugging.  */
  const char *transform_name;
};

static rtx noce_emit_store_flag (struct noce_if_info *, rtx, int, int);
static int noce_try_move (struct noce_if_info *);
static int noce_try_ifelse_collapse (struct noce_if_info *);
static int noce_try_store_flag (struct noce_if_info *);
static int noce_try_addcc (struct noce_if_info *);
static int noce_try_store_flag_constants (struct noce_if_info *);
static int noce_try_store_flag_mask (struct noce_if_info *);
static rtx noce_emit_cmove (struct noce_if_info *, rtx, enum rtx_code, rtx,
			    rtx, rtx, rtx);
static int noce_try_cmove (struct noce_if_info *);
static int noce_try_cmove_arith (struct noce_if_info *);
static rtx noce_get_alt_condition (struct noce_if_info *, rtx, rtx_insn **);
static int noce_try_minmax (struct noce_if_info *);
static int noce_try_abs (struct noce_if_info *);
static int noce_try_sign_mask (struct noce_if_info *);

/* Helper function for noce_try_store_flag*.  */

static rtx
noce_emit_store_flag (struct noce_if_info *if_info, rtx x, int reversep,
		      int normalize)
{
  rtx cond = if_info->cond;
  int cond_complex;
  enum rtx_code code;

  cond_complex = (! general_operand (XEXP (cond, 0), VOIDmode)
		  || ! general_operand (XEXP (cond, 1), VOIDmode));

  /* If earliest == jump, or when the condition is complex, try to
     build the store_flag insn directly.  */

  if (cond_complex)
    {
      rtx set = pc_set (if_info->jump);
      cond = XEXP (SET_SRC (set), 0);
      if (GET_CODE (XEXP (SET_SRC (set), 2)) == LABEL_REF
	  && LABEL_REF_LABEL (XEXP (SET_SRC (set), 2)) == JUMP_LABEL (if_info->jump))
	reversep = !reversep;
      if (if_info->then_else_reversed)
	reversep = !reversep;
    }

  if (reversep)
    code = reversed_comparison_code (cond, if_info->jump);
  else
    code = GET_CODE (cond);

  if ((if_info->cond_earliest == if_info->jump || cond_complex)
      && (normalize == 0 || STORE_FLAG_VALUE == normalize))
    {
      rtx src = gen_rtx_fmt_ee (code, GET_MODE (x), XEXP (cond, 0),
			    XEXP (cond, 1));
      rtx set = gen_rtx_SET (x, src);

      start_sequence ();
      rtx_insn *insn = emit_insn (set);

      if (recog_memoized (insn) >= 0)
	{
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  emit_insn (seq);

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

/* Emit instruction to move an rtx, possibly into STRICT_LOW_PART.
   X is the destination/target and Y is the value to copy.  */

static void
noce_emit_move_insn (rtx x, rtx y)
{
  machine_mode outmode;
  rtx outer, inner;
  int bitpos;

  if (GET_CODE (x) != STRICT_LOW_PART)
    {
      rtx_insn *seq, *insn;
      rtx target;
      optab ot;

      start_sequence ();
      /* Check that the SET_SRC is reasonable before calling emit_move_insn,
	 otherwise construct a suitable SET pattern ourselves.  */
      insn = (OBJECT_P (y) || CONSTANT_P (y) || GET_CODE (y) == SUBREG)
	     ? emit_move_insn (x, y)
	     : emit_insn (gen_rtx_SET (x, y));
      seq = get_insns ();
      end_sequence ();

      if (recog_memoized (insn) <= 0)
	{
	  if (GET_CODE (x) == ZERO_EXTRACT)
	    {
	      rtx op = XEXP (x, 0);
	      unsigned HOST_WIDE_INT size = INTVAL (XEXP (x, 1));
	      unsigned HOST_WIDE_INT start = INTVAL (XEXP (x, 2));

	      /* store_bit_field expects START to be relative to
		 BYTES_BIG_ENDIAN and adjusts this value for machines with
		 BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN.  In order to be able to
		 invoke store_bit_field again it is necessary to have the START
		 value from the first call.  */
	      if (BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
		{
		  if (MEM_P (op))
		    start = BITS_PER_UNIT - start - size;
		  else
		    {
		      gcc_assert (REG_P (op));
		      start = BITS_PER_WORD - start - size;
		    }
		}

	      gcc_assert (start < (MEM_P (op) ? BITS_PER_UNIT : BITS_PER_WORD));
	      store_bit_field (op, size, start, 0, 0, GET_MODE (x), y, false);
	      return;
	    }

	  switch (GET_RTX_CLASS (GET_CODE (y)))
	    {
	    case RTX_UNARY:
	      ot = code_to_optab (GET_CODE (y));
	      if (ot)
		{
		  start_sequence ();
		  target = expand_unop (GET_MODE (y), ot, XEXP (y, 0), x, 0);
		  if (target != NULL_RTX)
		    {
		      if (target != x)
			emit_move_insn (x, target);
		      seq = get_insns ();
		    }
		  end_sequence ();
		}
	      break;

	    case RTX_BIN_ARITH:
	    case RTX_COMM_ARITH:
	      ot = code_to_optab (GET_CODE (y));
	      if (ot)
		{
		  start_sequence ();
		  target = expand_binop (GET_MODE (y), ot,
					 XEXP (y, 0), XEXP (y, 1),
					 x, 0, OPTAB_DIRECT);
		  if (target != NULL_RTX)
		    {
		      if (target != x)
			  emit_move_insn (x, target);
		      seq = get_insns ();
		    }
		  end_sequence ();
		}
	      break;

	    default:
	      break;
	    }
	}

      emit_insn (seq);
      return;
    }

  outer = XEXP (x, 0);
  inner = XEXP (outer, 0);
  outmode = GET_MODE (outer);
  bitpos = SUBREG_BYTE (outer) * BITS_PER_UNIT;
  store_bit_field (inner, GET_MODE_BITSIZE (outmode), bitpos,
		   0, 0, outmode, y, false);
}

/* Return the CC reg if it is used in COND.  */

static rtx
cc_in_cond (rtx cond)
{
  if (have_cbranchcc4 && cond
      && GET_MODE_CLASS (GET_MODE (XEXP (cond, 0))) == MODE_CC)
    return XEXP (cond, 0);

  return NULL_RTX;
}

/* Return sequence of instructions generated by if conversion.  This
   function calls end_sequence() to end the current stream, ensures
   that the instructions are unshared, recognizable non-jump insns.
   On failure, this function returns a NULL_RTX.  */

static rtx_insn *
end_ifcvt_sequence (struct noce_if_info *if_info)
{
  rtx_insn *insn;
  rtx_insn *seq = get_insns ();
  rtx cc = cc_in_cond (if_info->cond);

  set_used_flags (if_info->x);
  set_used_flags (if_info->cond);
  set_used_flags (if_info->a);
  set_used_flags (if_info->b);

  for (insn = seq; insn; insn = NEXT_INSN (insn))
    set_used_flags (insn);

  unshare_all_rtl_in_chain (seq);
  end_sequence ();

  /* Make sure that all of the instructions emitted are recognizable,
     and that we haven't introduced a new jump instruction.
     As an exercise for the reader, build a general mechanism that
     allows proper placement of required clobbers.  */
  for (insn = seq; insn; insn = NEXT_INSN (insn))
    if (JUMP_P (insn)
	|| recog_memoized (insn) == -1
	   /* Make sure new generated code does not clobber CC.  */
	|| (cc && set_of (cc, insn)))
      return NULL;

  return seq;
}

/* Return true iff the then and else basic block (if it exists)
   consist of a single simple set instruction.  */

static bool
noce_simple_bbs (struct noce_if_info *if_info)
{
  if (!if_info->then_simple)
    return false;

  if (if_info->else_bb)
    return if_info->else_simple;

  return true;
}

/* Convert "if (a != b) x = a; else x = b" into "x = a" and
   "if (a == b) x = a; else x = b" into "x = b".  */

static int
noce_try_move (struct noce_if_info *if_info)
{
  rtx cond = if_info->cond;
  enum rtx_code code = GET_CODE (cond);
  rtx y;
  rtx_insn *seq;

  if (code != NE && code != EQ)
    return FALSE;

  if (!noce_simple_bbs (if_info))
    return FALSE;

  /* This optimization isn't valid if either A or B could be a NaN
     or a signed zero.  */
  if (HONOR_NANS (if_info->x)
      || HONOR_SIGNED_ZEROS (if_info->x))
    return FALSE;

  /* Check whether the operands of the comparison are A and in
     either order.  */
  if ((rtx_equal_p (if_info->a, XEXP (cond, 0))
       && rtx_equal_p (if_info->b, XEXP (cond, 1)))
      || (rtx_equal_p (if_info->a, XEXP (cond, 1))
	  && rtx_equal_p (if_info->b, XEXP (cond, 0))))
    {
      if (!rtx_interchangeable_p (if_info->a, if_info->b))
	return FALSE;

      y = (code == EQ) ? if_info->a : if_info->b;

      /* Avoid generating the move if the source is the destination.  */
      if (! rtx_equal_p (if_info->x, y))
	{
	  start_sequence ();
	  noce_emit_move_insn (if_info->x, y);
	  seq = end_ifcvt_sequence (if_info);
	  if (!seq)
	    return FALSE;

	  emit_insn_before_setloc (seq, if_info->jump,
				   INSN_LOCATION (if_info->insn_a));
	}
      if_info->transform_name = "noce_try_move";
      return TRUE;
    }
  return FALSE;
}

/* Try forming an IF_THEN_ELSE (cond, b, a) and collapsing that
   through simplify_rtx.  Sometimes that can eliminate the IF_THEN_ELSE.
   If that is the case, emit the result into x.  */

static int
noce_try_ifelse_collapse (struct noce_if_info * if_info)
{
  if (!noce_simple_bbs (if_info))
    return FALSE;

  machine_mode mode = GET_MODE (if_info->x);
  rtx if_then_else = simplify_gen_ternary (IF_THEN_ELSE, mode, mode,
					    if_info->cond, if_info->b,
					    if_info->a);

  if (GET_CODE (if_then_else) == IF_THEN_ELSE)
    return FALSE;

  rtx_insn *seq;
  start_sequence ();
  noce_emit_move_insn (if_info->x, if_then_else);
  seq = end_ifcvt_sequence (if_info);
  if (!seq)
    return FALSE;

  emit_insn_before_setloc (seq, if_info->jump,
			  INSN_LOCATION (if_info->insn_a));

  if_info->transform_name = "noce_try_ifelse_collapse";
  return TRUE;
}


/* Convert "if (test) x = 1; else x = 0".

   Only try 0 and STORE_FLAG_VALUE here.  Other combinations will be
   tried in noce_try_store_flag_constants after noce_try_cmove has had
   a go at the conversion.  */

static int
noce_try_store_flag (struct noce_if_info *if_info)
{
  int reversep;
  rtx target;
  rtx_insn *seq;

  if (!noce_simple_bbs (if_info))
    return FALSE;

  if (CONST_INT_P (if_info->b)
      && INTVAL (if_info->b) == STORE_FLAG_VALUE
      && if_info->a == const0_rtx)
    reversep = 0;
  else if (if_info->b == const0_rtx
	   && CONST_INT_P (if_info->a)
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

      seq = end_ifcvt_sequence (if_info);
      if (! seq)
	return FALSE;

      emit_insn_before_setloc (seq, if_info->jump,
			       INSN_LOCATION (if_info->insn_a));
      if_info->transform_name = "noce_try_store_flag";
      return TRUE;
    }
  else
    {
      end_sequence ();
      return FALSE;
    }
}


/* Convert "if (test) x = -A; else x = A" into
   x = A; if (test) x = -x if the machine can do the
   conditional negate form of this cheaply.
   Try this before noce_try_cmove that will just load the
   immediates into two registers and do a conditional select
   between them.  If the target has a conditional negate or
   conditional invert operation we can save a potentially
   expensive constant synthesis.  */

static bool
noce_try_inverse_constants (struct noce_if_info *if_info)
{
  if (!noce_simple_bbs (if_info))
    return false;

  if (!CONST_INT_P (if_info->a)
      || !CONST_INT_P (if_info->b)
      || !REG_P (if_info->x))
    return false;

  machine_mode mode = GET_MODE (if_info->x);

  HOST_WIDE_INT val_a = INTVAL (if_info->a);
  HOST_WIDE_INT val_b = INTVAL (if_info->b);

  rtx cond = if_info->cond;

  rtx x = if_info->x;
  rtx target;

  start_sequence ();

  rtx_code code;
  if (val_b != HOST_WIDE_INT_MIN && val_a == -val_b)
    code = NEG;
  else if (val_a == ~val_b)
    code = NOT;
  else
    {
      end_sequence ();
      return false;
    }

  rtx tmp = gen_reg_rtx (mode);
  noce_emit_move_insn (tmp, if_info->a);

  target = emit_conditional_neg_or_complement (x, code, mode, cond, tmp, tmp);

  if (target)
    {
      rtx_insn *seq = get_insns ();

      if (!seq)
	{
	  end_sequence ();
	  return false;
	}

      if (target != if_info->x)
	noce_emit_move_insn (if_info->x, target);

      seq = end_ifcvt_sequence (if_info);

      if (!seq)
	return false;

      emit_insn_before_setloc (seq, if_info->jump,
			       INSN_LOCATION (if_info->insn_a));
      if_info->transform_name = "noce_try_inverse_constants";
      return true;
    }

  end_sequence ();
  return false;
}


/* Convert "if (test) x = a; else x = b", for A and B constant.
   Also allow A = y + c1, B = y + c2, with a common y between A
   and B.  */

static int
noce_try_store_flag_constants (struct noce_if_info *if_info)
{
  rtx target;
  rtx_insn *seq;
  bool reversep;
  HOST_WIDE_INT itrue, ifalse, diff, tmp;
  int normalize;
  bool can_reverse;
  machine_mode mode = GET_MODE (if_info->x);;
  rtx common = NULL_RTX;

  rtx a = if_info->a;
  rtx b = if_info->b;

  /* Handle cases like x := test ? y + 3 : y + 4.  */
  if (GET_CODE (a) == PLUS
      && GET_CODE (b) == PLUS
      && CONST_INT_P (XEXP (a, 1))
      && CONST_INT_P (XEXP (b, 1))
      && rtx_equal_p (XEXP (a, 0), XEXP (b, 0))
      /* Allow expressions that are not using the result or plain
         registers where we handle overlap below.  */
      && (REG_P (XEXP (a, 0))
	  || (noce_operand_ok (XEXP (a, 0))
	      && ! reg_overlap_mentioned_p (if_info->x, XEXP (a, 0))))
      && if_info->branch_cost >= 2)
    {
      common = XEXP (a, 0);
      a = XEXP (a, 1);
      b = XEXP (b, 1);
    }

  if (!noce_simple_bbs (if_info))
    return FALSE;

  if (CONST_INT_P (a)
      && CONST_INT_P (b))
    {
      ifalse = INTVAL (a);
      itrue = INTVAL (b);
      bool subtract_flag_p = false;

      diff = (unsigned HOST_WIDE_INT) itrue - ifalse;
      /* Make sure we can represent the difference between the two values.  */
      if ((diff > 0)
	  != ((ifalse < 0) != (itrue < 0) ? ifalse < 0 : ifalse < itrue))
	return FALSE;

      diff = trunc_int_for_mode (diff, mode);

      can_reverse = (reversed_comparison_code (if_info->cond, if_info->jump)
		     != UNKNOWN);

      reversep = false;
      if (diff == STORE_FLAG_VALUE || diff == -STORE_FLAG_VALUE)
	{
	  normalize = 0;
	  /* We could collapse these cases but it is easier to follow the
	     diff/STORE_FLAG_VALUE combinations when they are listed
	     explicitly.  */

	  /* test ? 3 : 4
	     => 4 + (test != 0).  */
	  if (diff < 0 && STORE_FLAG_VALUE < 0)
	      reversep = false;
	  /* test ? 4 : 3
	     => can_reverse  | 4 + (test == 0)
		!can_reverse | 3 - (test != 0).  */
	  else if (diff > 0 && STORE_FLAG_VALUE < 0)
	    {
	      reversep = can_reverse;
	      subtract_flag_p = !can_reverse;
	      /* If we need to subtract the flag and we have PLUS-immediate
		 A and B then it is unlikely to be beneficial to play tricks
		 here.  */
	      if (subtract_flag_p && common)
		return FALSE;
	    }
	  /* test ? 3 : 4
	     => can_reverse  | 3 + (test == 0)
		!can_reverse | 4 - (test != 0).  */
	  else if (diff < 0 && STORE_FLAG_VALUE > 0)
	    {
	      reversep = can_reverse;
	      subtract_flag_p = !can_reverse;
	      /* If we need to subtract the flag and we have PLUS-immediate
		 A and B then it is unlikely to be beneficial to play tricks
		 here.  */
	      if (subtract_flag_p && common)
		return FALSE;
	    }
	  /* test ? 4 : 3
	     => 4 + (test != 0).  */
	  else if (diff > 0 && STORE_FLAG_VALUE > 0)
	    reversep = false;
	  else
	    gcc_unreachable ();
	}
      else if (ifalse == 0 && exact_log2 (itrue) >= 0
	       && (STORE_FLAG_VALUE == 1
		   || if_info->branch_cost >= 2))
	normalize = 1;
      else if (itrue == 0 && exact_log2 (ifalse) >= 0 && can_reverse
	       && (STORE_FLAG_VALUE == 1 || if_info->branch_cost >= 2))
	{
	  normalize = 1;
	  reversep = true;
	}
      else if (itrue == -1
	       && (STORE_FLAG_VALUE == -1
		   || if_info->branch_cost >= 2))
	normalize = -1;
      else if (ifalse == -1 && can_reverse
	       && (STORE_FLAG_VALUE == -1 || if_info->branch_cost >= 2))
	{
	  normalize = -1;
	  reversep = true;
	}
      else
	return FALSE;

      if (reversep)
	{
	  std::swap (itrue, ifalse);
	  diff = trunc_int_for_mode (-(unsigned HOST_WIDE_INT) diff, mode);
	}

      start_sequence ();

      /* If we have x := test ? x + 3 : x + 4 then move the original
	 x out of the way while we store flags.  */
      if (common && rtx_equal_p (common, if_info->x))
	{
	  common = gen_reg_rtx (mode);
	  noce_emit_move_insn (common, if_info->x);
	}

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
	  /* Add the common part now.  This may allow combine to merge this
	     with the store flag operation earlier into some sort of conditional
	     increment/decrement if the target allows it.  */
	  if (common)
	    target = expand_simple_binop (mode, PLUS,
					   target, common,
					   target, 0, OPTAB_WIDEN);

	  /* Always use ifalse here.  It should have been swapped with itrue
	     when appropriate when reversep is true.  */
	  target = expand_simple_binop (mode, subtract_flag_p ? MINUS : PLUS,
					gen_int_mode (ifalse, mode), target,
					if_info->x, 0, OPTAB_WIDEN);
	}
      /* Other cases are not beneficial when the original A and B are PLUS
	 expressions.  */
      else if (common)
	{
	  end_sequence ();
	  return FALSE;
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
					target, gen_int_mode (ifalse, mode),
					if_info->x, 0, OPTAB_WIDEN);
	}
      else
	{
	  end_sequence ();
	  return FALSE;
	}

      if (! target)
	{
	  end_sequence ();
	  return FALSE;
	}

      if (target != if_info->x)
	noce_emit_move_insn (if_info->x, target);

      seq = end_ifcvt_sequence (if_info);
      if (!seq)
	return FALSE;

      emit_insn_before_setloc (seq, if_info->jump,
			       INSN_LOCATION (if_info->insn_a));
      if_info->transform_name = "noce_try_store_flag_constants";

      return TRUE;
    }

  return FALSE;
}

/* Convert "if (test) foo++" into "foo += (test != 0)", and
   similarly for "foo--".  */

static int
noce_try_addcc (struct noce_if_info *if_info)
{
  rtx target;
  rtx_insn *seq;
  int subtract, normalize;

  if (!noce_simple_bbs (if_info))
    return FALSE;

  if (GET_CODE (if_info->a) == PLUS
      && rtx_equal_p (XEXP (if_info->a, 0), if_info->b)
      && (reversed_comparison_code (if_info->cond, if_info->jump)
	  != UNKNOWN))
    {
      rtx cond = if_info->cond;
      enum rtx_code code = reversed_comparison_code (cond, if_info->jump);

      /* First try to use addcc pattern.  */
      if (general_operand (XEXP (cond, 0), VOIDmode)
	  && general_operand (XEXP (cond, 1), VOIDmode))
	{
	  start_sequence ();
	  target = emit_conditional_add (if_info->x, code,
					 XEXP (cond, 0),
					 XEXP (cond, 1),
					 VOIDmode,
					 if_info->b,
					 XEXP (if_info->a, 1),
					 GET_MODE (if_info->x),
					 (code == LTU || code == GEU
					  || code == LEU || code == GTU));
	  if (target)
	    {
	      if (target != if_info->x)
		noce_emit_move_insn (if_info->x, target);

	      seq = end_ifcvt_sequence (if_info);
	      if (!seq)
		return FALSE;

	      emit_insn_before_setloc (seq, if_info->jump,
				       INSN_LOCATION (if_info->insn_a));
	      if_info->transform_name = "noce_try_addcc";

	      return TRUE;
	    }
	  end_sequence ();
	}

      /* If that fails, construct conditional increment or decrement using
	 setcc.  */
      if (if_info->branch_cost >= 2
	  && (XEXP (if_info->a, 1) == const1_rtx
	      || XEXP (if_info->a, 1) == constm1_rtx))
        {
	  start_sequence ();
	  if (STORE_FLAG_VALUE == INTVAL (XEXP (if_info->a, 1)))
	    subtract = 0, normalize = 0;
	  else if (-STORE_FLAG_VALUE == INTVAL (XEXP (if_info->a, 1)))
	    subtract = 1, normalize = 0;
	  else
	    subtract = 0, normalize = INTVAL (XEXP (if_info->a, 1));


	  target = noce_emit_store_flag (if_info,
					 gen_reg_rtx (GET_MODE (if_info->x)),
					 1, normalize);

	  if (target)
	    target = expand_simple_binop (GET_MODE (if_info->x),
					  subtract ? MINUS : PLUS,
					  if_info->b, target, if_info->x,
					  0, OPTAB_WIDEN);
	  if (target)
	    {
	      if (target != if_info->x)
		noce_emit_move_insn (if_info->x, target);

	      seq = end_ifcvt_sequence (if_info);
	      if (!seq)
		return FALSE;

	      emit_insn_before_setloc (seq, if_info->jump,
				       INSN_LOCATION (if_info->insn_a));
	      if_info->transform_name = "noce_try_addcc";
	      return TRUE;
	    }
	  end_sequence ();
	}
    }

  return FALSE;
}

/* Convert "if (test) x = 0;" to "x &= -(test == 0);"  */

static int
noce_try_store_flag_mask (struct noce_if_info *if_info)
{
  rtx target;
  rtx_insn *seq;
  int reversep;

  if (!noce_simple_bbs (if_info))
    return FALSE;

  reversep = 0;
  if ((if_info->branch_cost >= 2
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
				      if_info->x,
				      target, if_info->x, 0,
				      OPTAB_WIDEN);

      if (target)
	{
	  int old_cost, new_cost, insn_cost;
	  int speed_p;

	  if (target != if_info->x)
	    noce_emit_move_insn (if_info->x, target);

	  seq = end_ifcvt_sequence (if_info);
	  if (!seq)
	    return FALSE;

	  speed_p = optimize_bb_for_speed_p (BLOCK_FOR_INSN (if_info->insn_a));
	  insn_cost = insn_rtx_cost (PATTERN (if_info->insn_a), speed_p);
	  old_cost = COSTS_N_INSNS (if_info->branch_cost) + insn_cost;
	  new_cost = seq_cost (seq, speed_p);

	  if (new_cost > old_cost)
	    return FALSE;

	  emit_insn_before_setloc (seq, if_info->jump,
				   INSN_LOCATION (if_info->insn_a));
	  if_info->transform_name = "noce_try_store_flag_mask";

	  return TRUE;
	}

      end_sequence ();
    }

  return FALSE;
}

/* Helper function for noce_try_cmove and noce_try_cmove_arith.  */

static rtx
noce_emit_cmove (struct noce_if_info *if_info, rtx x, enum rtx_code code,
		 rtx cmp_a, rtx cmp_b, rtx vfalse, rtx vtrue)
{
  rtx target ATTRIBUTE_UNUSED;
  int unsignedp ATTRIBUTE_UNUSED;

  /* If earliest == jump, try to build the cmove insn directly.
     This is helpful when combine has created some complex condition
     (like for alpha's cmovlbs) that we can't hope to regenerate
     through the normal interface.  */

  if (if_info->cond_earliest == if_info->jump)
    {
      rtx cond = gen_rtx_fmt_ee (code, GET_MODE (if_info->cond), cmp_a, cmp_b);
      rtx if_then_else = gen_rtx_IF_THEN_ELSE (GET_MODE (x),
					       cond, vtrue, vfalse);
      rtx set = gen_rtx_SET (x, if_then_else);

      start_sequence ();
      rtx_insn *insn = emit_insn (set);

      if (recog_memoized (insn) >= 0)
	{
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  emit_insn (seq);

	  return x;
	}

      end_sequence ();
    }

  /* Don't even try if the comparison operands are weird
     except that the target supports cbranchcc4.  */
  if (! general_operand (cmp_a, GET_MODE (cmp_a))
      || ! general_operand (cmp_b, GET_MODE (cmp_b)))
    {
      if (!have_cbranchcc4
	  || GET_MODE_CLASS (GET_MODE (cmp_a)) != MODE_CC
	  || cmp_b != const0_rtx)
	return NULL_RTX;
    }

  unsignedp = (code == LTU || code == GEU
	       || code == LEU || code == GTU);

  target = emit_conditional_move (x, code, cmp_a, cmp_b, VOIDmode,
				  vtrue, vfalse, GET_MODE (x),
				  unsignedp);
  if (target)
    return target;

  /* We might be faced with a situation like:

     x = (reg:M TARGET)
     vtrue = (subreg:M (reg:N VTRUE) BYTE)
     vfalse = (subreg:M (reg:N VFALSE) BYTE)

     We can't do a conditional move in mode M, but it's possible that we
     could do a conditional move in mode N instead and take a subreg of
     the result.

     If we can't create new pseudos, though, don't bother.  */
  if (reload_completed)
    return NULL_RTX;

  if (GET_CODE (vtrue) == SUBREG && GET_CODE (vfalse) == SUBREG)
    {
      rtx reg_vtrue = SUBREG_REG (vtrue);
      rtx reg_vfalse = SUBREG_REG (vfalse);
      unsigned int byte_vtrue = SUBREG_BYTE (vtrue);
      unsigned int byte_vfalse = SUBREG_BYTE (vfalse);
      rtx promoted_target;

      if (GET_MODE (reg_vtrue) != GET_MODE (reg_vfalse)
	  || byte_vtrue != byte_vfalse
	  || (SUBREG_PROMOTED_VAR_P (vtrue)
	      != SUBREG_PROMOTED_VAR_P (vfalse))
	  || (SUBREG_PROMOTED_GET (vtrue)
	      != SUBREG_PROMOTED_GET (vfalse)))
	return NULL_RTX;

      promoted_target = gen_reg_rtx (GET_MODE (reg_vtrue));

      target = emit_conditional_move (promoted_target, code, cmp_a, cmp_b,
				      VOIDmode, reg_vtrue, reg_vfalse,
				      GET_MODE (reg_vtrue), unsignedp);
      /* Nope, couldn't do it in that mode either.  */
      if (!target)
	return NULL_RTX;

      target = gen_rtx_SUBREG (GET_MODE (vtrue), promoted_target, byte_vtrue);
      SUBREG_PROMOTED_VAR_P (target) = SUBREG_PROMOTED_VAR_P (vtrue);
      SUBREG_PROMOTED_SET (target, SUBREG_PROMOTED_GET (vtrue));
      emit_move_insn (x, target);
      return x;
    }
  else
    return NULL_RTX;
}

/* Try only simple constants and registers here.  More complex cases
   are handled in noce_try_cmove_arith after noce_try_store_flag_arith
   has had a go at it.  */

static int
noce_try_cmove (struct noce_if_info *if_info)
{
  enum rtx_code code;
  rtx target;
  rtx_insn *seq;

  if (!noce_simple_bbs (if_info))
    return FALSE;

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

	  seq = end_ifcvt_sequence (if_info);
	  if (!seq)
	    return FALSE;

	  emit_insn_before_setloc (seq, if_info->jump,
				   INSN_LOCATION (if_info->insn_a));
	  if_info->transform_name = "noce_try_cmove";

	  return TRUE;
	}
      /* If both a and b are constants try a last-ditch transformation:
	 if (test) x = a; else x = b;
	 =>   x = (-(test != 0) & (b - a)) + a;
	 Try this only if the target-specific expansion above has failed.
	 The target-specific expander may want to generate sequences that
	 we don't know about, so give them a chance before trying this
	 approach.  */
      else if (!targetm.have_conditional_execution ()
		&& CONST_INT_P (if_info->a) && CONST_INT_P (if_info->b)
		&& ((if_info->branch_cost >= 2 && STORE_FLAG_VALUE == -1)
		    || if_info->branch_cost >= 3))
	{
	  machine_mode mode = GET_MODE (if_info->x);
	  HOST_WIDE_INT ifalse = INTVAL (if_info->a);
	  HOST_WIDE_INT itrue = INTVAL (if_info->b);
	  rtx target = noce_emit_store_flag (if_info, if_info->x, false, -1);
	  if (!target)
	    {
	      end_sequence ();
	      return FALSE;
	    }

	  HOST_WIDE_INT diff = (unsigned HOST_WIDE_INT) itrue - ifalse;
	  /* Make sure we can represent the difference
	     between the two values.  */
	  if ((diff > 0)
	      != ((ifalse < 0) != (itrue < 0) ? ifalse < 0 : ifalse < itrue))
	    {
	      end_sequence ();
	      return FALSE;
	    }

	  diff = trunc_int_for_mode (diff, mode);
	  target = expand_simple_binop (mode, AND,
					target, gen_int_mode (diff, mode),
					if_info->x, 0, OPTAB_WIDEN);
	  if (target)
	    target = expand_simple_binop (mode, PLUS,
					  target, gen_int_mode (ifalse, mode),
					  if_info->x, 0, OPTAB_WIDEN);
	  if (target)
	    {
	      if (target != if_info->x)
		noce_emit_move_insn (if_info->x, target);

	      seq = end_ifcvt_sequence (if_info);
	      if (!seq)
		return FALSE;

	      emit_insn_before_setloc (seq, if_info->jump,
				   INSN_LOCATION (if_info->insn_a));
	      if_info->transform_name = "noce_try_cmove";
	      return TRUE;
	    }
	  else
	    {
	      end_sequence ();
	      return FALSE;
	    }
	}
      else
	end_sequence ();
    }

  return FALSE;
}

/* Return true if X contains a conditional code mode rtx.  */

static bool
contains_ccmode_rtx_p (rtx x)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    if (GET_MODE_CLASS (GET_MODE (*iter)) == MODE_CC)
      return true;

  return false;
}

/* Helper for bb_valid_for_noce_process_p.  Validate that
   the rtx insn INSN is a single set that does not set
   the conditional register CC and is in general valid for
   if-conversion.  */

static bool
insn_valid_noce_process_p (rtx_insn *insn, rtx cc)
{
  if (!insn
      || !NONJUMP_INSN_P (insn)
      || (cc && set_of (cc, insn)))
      return false;

  rtx sset = single_set (insn);

  /* Currently support only simple single sets in test_bb.  */
  if (!sset
      || !noce_operand_ok (SET_DEST (sset))
      || contains_ccmode_rtx_p (SET_DEST (sset))
      || !noce_operand_ok (SET_SRC (sset)))
    return false;

  return true;
}


/* Return true iff the registers that the insns in BB_A set do not get
   used in BB_B.  If TO_RENAME is non-NULL then it is a location that will be
   renamed later by the caller and so conflicts on it should be ignored
   in this function.  */

static bool
bbs_ok_for_cmove_arith (basic_block bb_a, basic_block bb_b, rtx to_rename)
{
  rtx_insn *a_insn;
  bitmap bba_sets = BITMAP_ALLOC (&reg_obstack);

  df_ref def;
  df_ref use;

  FOR_BB_INSNS (bb_a, a_insn)
    {
      if (!active_insn_p (a_insn))
	continue;

      rtx sset_a = single_set (a_insn);

      if (!sset_a)
	{
	  BITMAP_FREE (bba_sets);
	  return false;
	}
      /* Record all registers that BB_A sets.  */
      FOR_EACH_INSN_DEF (def, a_insn)
	if (!(to_rename && DF_REF_REG (def) == to_rename))
	  bitmap_set_bit (bba_sets, DF_REF_REGNO (def));
    }

  rtx_insn *b_insn;

  FOR_BB_INSNS (bb_b, b_insn)
    {
      if (!active_insn_p (b_insn))
	continue;

      rtx sset_b = single_set (b_insn);

      if (!sset_b)
	{
	  BITMAP_FREE (bba_sets);
	  return false;
	}

      /* Make sure this is a REG and not some instance
	 of ZERO_EXTRACT or SUBREG or other dangerous stuff.
	 If we have a memory destination then we have a pair of simple
	 basic blocks performing an operation of the form [addr] = c ? a : b.
	 bb_valid_for_noce_process_p will have ensured that these are
	 the only stores present.  In that case [addr] should be the location
	 to be renamed.  Assert that the callers set this up properly.  */
      if (MEM_P (SET_DEST (sset_b)))
	gcc_assert (rtx_equal_p (SET_DEST (sset_b), to_rename));
      else if (!REG_P (SET_DEST (sset_b)))
	{
	  BITMAP_FREE (bba_sets);
	  return false;
	}

      /* If the insn uses a reg set in BB_A return false.  */
      FOR_EACH_INSN_USE (use, b_insn)
	{
	  if (bitmap_bit_p (bba_sets, DF_REF_REGNO (use)))
	    {
	      BITMAP_FREE (bba_sets);
	      return false;
	    }
	}

    }

  BITMAP_FREE (bba_sets);
  return true;
}

/* Emit copies of all the active instructions in BB except the last.
   This is a helper for noce_try_cmove_arith.  */

static void
noce_emit_all_but_last (basic_block bb)
{
  rtx_insn *last = last_active_insn (bb, FALSE);
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    {
      if (insn != last && active_insn_p (insn))
	{
	  rtx_insn *to_emit = as_a <rtx_insn *> (copy_rtx (insn));

	  emit_insn (PATTERN (to_emit));
	}
    }
}

/* Helper for noce_try_cmove_arith.  Emit the pattern TO_EMIT and return
   the resulting insn or NULL if it's not a valid insn.  */

static rtx_insn *
noce_emit_insn (rtx to_emit)
{
  gcc_assert (to_emit);
  rtx_insn *insn = emit_insn (to_emit);

  if (recog_memoized (insn) < 0)
    return NULL;

  return insn;
}

/* Helper for noce_try_cmove_arith.  Emit a copy of the insns up to
   and including the penultimate one in BB if it is not simple
   (as indicated by SIMPLE).  Then emit LAST_INSN as the last
   insn in the block.  The reason for that is that LAST_INSN may
   have been modified by the preparation in noce_try_cmove_arith.  */

static bool
noce_emit_bb (rtx last_insn, basic_block bb, bool simple)
{
  if (bb && !simple)
    noce_emit_all_but_last (bb);

  if (last_insn && !noce_emit_insn (last_insn))
    return false;

  return true;
}

/* Try more complex cases involving conditional_move.  */

static int
noce_try_cmove_arith (struct noce_if_info *if_info)
{
  rtx a = if_info->a;
  rtx b = if_info->b;
  rtx x = if_info->x;
  rtx orig_a, orig_b;
  rtx_insn *insn_a, *insn_b;
  bool a_simple = if_info->then_simple;
  bool b_simple = if_info->else_simple;
  basic_block then_bb = if_info->then_bb;
  basic_block else_bb = if_info->else_bb;
  rtx target;
  int is_mem = 0;
  enum rtx_code code;
  rtx_insn *ifcvt_seq;

  /* A conditional move from two memory sources is equivalent to a
     conditional on their addresses followed by a load.  Don't do this
     early because it'll screw alias analysis.  Note that we've
     already checked for no side effects.  */
  /* ??? FIXME: Magic number 5.  */
  if (cse_not_expected
      && MEM_P (a) && MEM_P (b)
      && MEM_ADDR_SPACE (a) == MEM_ADDR_SPACE (b)
      && if_info->branch_cost >= 5)
    {
      machine_mode address_mode = get_address_mode (a);

      a = XEXP (a, 0);
      b = XEXP (b, 0);
      x = gen_reg_rtx (address_mode);
      is_mem = 1;
    }

  /* ??? We could handle this if we knew that a load from A or B could
     not trap or fault.  This is also true if we've already loaded
     from the address along the path from ENTRY.  */
  else if (may_trap_or_fault_p (a) || may_trap_or_fault_p (b))
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

  machine_mode x_mode = GET_MODE (x);

  if (!can_conditionally_move_p (x_mode))
    return FALSE;

  unsigned int then_cost;
  unsigned int else_cost;
  if (insn_a)
    then_cost = if_info->then_cost;
  else
    then_cost = 0;

  if (insn_b)
    else_cost = if_info->else_cost;
  else
    else_cost = 0;

  /* We're going to execute one of the basic blocks anyway, so
     bail out if the most expensive of the two blocks is unacceptable.  */
  if (MAX (then_cost, else_cost) > COSTS_N_INSNS (if_info->branch_cost))
    return FALSE;

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
	  std::swap (a, b);
	  std::swap (insn_a, insn_b);
	  std::swap (a_simple, b_simple);
	  std::swap (then_bb, else_bb);
	}
    }

  if (then_bb && else_bb
      && (!bbs_ok_for_cmove_arith (then_bb, else_bb,  if_info->orig_x)
	  || !bbs_ok_for_cmove_arith (else_bb, then_bb,  if_info->orig_x)))
    return FALSE;

  start_sequence ();

  /* If one of the blocks is empty then the corresponding B or A value
     came from the test block.  The non-empty complex block that we will
     emit might clobber the register used by B or A, so move it to a pseudo
     first.  */

  rtx tmp_a = NULL_RTX;
  rtx tmp_b = NULL_RTX;

  if (b_simple || !else_bb)
    tmp_b = gen_reg_rtx (x_mode);

  if (a_simple || !then_bb)
    tmp_a = gen_reg_rtx (x_mode);

  orig_a = a;
  orig_b = b;

  rtx emit_a = NULL_RTX;
  rtx emit_b = NULL_RTX;
  rtx_insn *tmp_insn = NULL;
  bool modified_in_a = false;
  bool  modified_in_b = false;
  /* If either operand is complex, load it into a register first.
     The best way to do this is to copy the original insn.  In this
     way we preserve any clobbers etc that the insn may have had.
     This is of course not possible in the IS_MEM case.  */

  if (! general_operand (a, GET_MODE (a)) || tmp_a)
    {

      if (is_mem)
	{
	  rtx reg = gen_reg_rtx (GET_MODE (a));
	  emit_a = gen_rtx_SET (reg, a);
	}
      else
	{
	  if (insn_a)
	    {
	      a = tmp_a ? tmp_a : gen_reg_rtx (GET_MODE (a));

	      rtx_insn *copy_of_a = as_a <rtx_insn *> (copy_rtx (insn_a));
	      rtx set = single_set (copy_of_a);
	      SET_DEST (set) = a;

	      emit_a = PATTERN (copy_of_a);
	    }
	  else
	    {
	      rtx tmp_reg = tmp_a ? tmp_a : gen_reg_rtx (GET_MODE (a));
	      emit_a = gen_rtx_SET (tmp_reg, a);
	      a = tmp_reg;
	    }
	}
    }

  if (! general_operand (b, GET_MODE (b)) || tmp_b)
    {
      if (is_mem)
	{
          rtx reg = gen_reg_rtx (GET_MODE (b));
	  emit_b = gen_rtx_SET (reg, b);
	}
      else
	{
	  if (insn_b)
	    {
	      b = tmp_b ? tmp_b : gen_reg_rtx (GET_MODE (b));
	      rtx_insn *copy_of_b = as_a <rtx_insn *> (copy_rtx (insn_b));
	      rtx set = single_set (copy_of_b);

	      SET_DEST (set) = b;
	      emit_b = PATTERN (copy_of_b);
	    }
	  else
	    {
	      rtx tmp_reg = tmp_b ? tmp_b : gen_reg_rtx (GET_MODE (b));
	      emit_b = gen_rtx_SET (tmp_reg, b);
	      b = tmp_reg;
	  }
	}
    }

  modified_in_a = emit_a != NULL_RTX && modified_in_p (orig_b, emit_a);
  if (tmp_b && then_bb)
    {
      FOR_BB_INSNS (then_bb, tmp_insn)
	/* Don't check inside insn_a.  We will have changed it to emit_a
	   with a destination that doesn't conflict.  */
	if (!(insn_a && tmp_insn == insn_a)
	    && modified_in_p (orig_b, tmp_insn))
	  {
	    modified_in_a = true;
	    break;
	  }

    }

  modified_in_b = emit_b != NULL_RTX && modified_in_p (orig_a, emit_b);
  if (tmp_a && else_bb)
    {
      FOR_BB_INSNS (else_bb, tmp_insn)
      /* Don't check inside insn_b.  We will have changed it to emit_b
	 with a destination that doesn't conflict.  */
      if (!(insn_b && tmp_insn == insn_b)
	  && modified_in_p (orig_a, tmp_insn))
	{
	  modified_in_b = true;
	  break;
	}
    }

  /* If insn to set up A clobbers any registers B depends on, try to
     swap insn that sets up A with the one that sets up B.  If even
     that doesn't help, punt.  */
  if (modified_in_a && !modified_in_b)
    {
      if (!noce_emit_bb (emit_b, else_bb, b_simple))
	goto end_seq_and_fail;

      if (!noce_emit_bb (emit_a, then_bb, a_simple))
	goto end_seq_and_fail;
    }
  else if (!modified_in_a)
    {
      if (!noce_emit_bb (emit_a, then_bb, a_simple))
	goto end_seq_and_fail;

      if (!noce_emit_bb (emit_b, else_bb, b_simple))
	goto end_seq_and_fail;
    }
  else
    goto end_seq_and_fail;

  target = noce_emit_cmove (if_info, x, code, XEXP (if_info->cond, 0),
			    XEXP (if_info->cond, 1), a, b);

  if (! target)
    goto end_seq_and_fail;

  /* If we're handling a memory for above, emit the load now.  */
  if (is_mem)
    {
      rtx mem = gen_rtx_MEM (GET_MODE (if_info->x), target);

      /* Copy over flags as appropriate.  */
      if (MEM_VOLATILE_P (if_info->a) || MEM_VOLATILE_P (if_info->b))
	MEM_VOLATILE_P (mem) = 1;
      if (MEM_ALIAS_SET (if_info->a) == MEM_ALIAS_SET (if_info->b))
	set_mem_alias_set (mem, MEM_ALIAS_SET (if_info->a));
      set_mem_align (mem,
		     MIN (MEM_ALIGN (if_info->a), MEM_ALIGN (if_info->b)));

      gcc_assert (MEM_ADDR_SPACE (if_info->a) == MEM_ADDR_SPACE (if_info->b));
      set_mem_addr_space (mem, MEM_ADDR_SPACE (if_info->a));

      noce_emit_move_insn (if_info->x, mem);
    }
  else if (target != x)
    noce_emit_move_insn (x, target);

  ifcvt_seq = end_ifcvt_sequence (if_info);
  if (!ifcvt_seq)
    return FALSE;

  emit_insn_before_setloc (ifcvt_seq, if_info->jump,
			   INSN_LOCATION (if_info->insn_a));
  if_info->transform_name = "noce_try_cmove_arith";
  return TRUE;

 end_seq_and_fail:
  end_sequence ();
  return FALSE;
}

/* For most cases, the simplified condition we found is the best
   choice, but this is not the case for the min/max/abs transforms.
   For these we wish to know that it is A or B in the condition.  */

static rtx
noce_get_alt_condition (struct noce_if_info *if_info, rtx target,
			rtx_insn **earliest)
{
  rtx cond, set;
  rtx_insn *insn;
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
      && LABEL_REF_LABEL (XEXP (SET_SRC (set), 2)) == JUMP_LABEL (if_info->jump);
  if (if_info->then_else_reversed)
    reverse = !reverse;

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

  if (CONST_INT_P (target))
    {
      enum rtx_code code = GET_CODE (if_info->cond);
      rtx op_a = XEXP (if_info->cond, 0);
      rtx op_b = XEXP (if_info->cond, 1);
      rtx_insn *prev_insn;

      /* First, look to see if we put a constant in a register.  */
      prev_insn = prev_nonnote_insn (if_info->cond_earliest);
      if (prev_insn
	  && BLOCK_FOR_INSN (prev_insn)
	     == BLOCK_FOR_INSN (if_info->cond_earliest)
	  && INSN_P (prev_insn)
	  && GET_CODE (PATTERN (prev_insn)) == SET)
	{
	  rtx src = find_reg_equal_equiv_note (prev_insn);
	  if (!src)
	    src = SET_SRC (PATTERN (prev_insn));
	  if (CONST_INT_P (src))
	    {
	      if (rtx_equal_p (op_a, SET_DEST (PATTERN (prev_insn))))
		op_a = src;
	      else if (rtx_equal_p (op_b, SET_DEST (PATTERN (prev_insn))))
		op_b = src;

	      if (CONST_INT_P (op_a))
		{
		  std::swap (op_a, op_b);
		  code = swap_condition (code);
		}
	    }
	}

      /* Now, look to see if we can get the right constant by
	 adjusting the conditional.  */
      if (CONST_INT_P (op_b))
	{
	  HOST_WIDE_INT desired_val = INTVAL (target);
	  HOST_WIDE_INT actual_val = INTVAL (op_b);

	  switch (code)
	    {
	    case LT:
	      if (desired_val != HOST_WIDE_INT_MAX
		  && actual_val == desired_val + 1)
		{
		  code = LE;
		  op_b = GEN_INT (desired_val);
		}
	      break;
	    case LE:
	      if (desired_val != HOST_WIDE_INT_MIN
		  && actual_val == desired_val - 1)
		{
		  code = LT;
		  op_b = GEN_INT (desired_val);
		}
	      break;
	    case GT:
	      if (desired_val != HOST_WIDE_INT_MIN
		  && actual_val == desired_val - 1)
		{
		  code = GE;
		  op_b = GEN_INT (desired_val);
		}
	      break;
	    case GE:
	      if (desired_val != HOST_WIDE_INT_MAX
		  && actual_val == desired_val + 1)
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
				 earliest, target, have_cbranchcc4, true);
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
noce_try_minmax (struct noce_if_info *if_info)
{
  rtx cond, target;
  rtx_insn *earliest, *seq;
  enum rtx_code code, op;
  int unsignedp;

  if (!noce_simple_bbs (if_info))
    return FALSE;

  /* ??? Reject modes with NaNs or signed zeros since we don't know how
     they will be resolved with an SMIN/SMAX.  It wouldn't be too hard
     to get the target to tell us...  */
  if (HONOR_SIGNED_ZEROS (if_info->x)
      || HONOR_NANS (if_info->x))
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

  seq = end_ifcvt_sequence (if_info);
  if (!seq)
    return FALSE;

  emit_insn_before_setloc (seq, if_info->jump, INSN_LOCATION (if_info->insn_a));
  if_info->cond = cond;
  if_info->cond_earliest = earliest;
  if_info->transform_name = "noce_try_minmax";

  return TRUE;
}

/* Convert "if (a < 0) x = -a; else x = a;" to "x = abs(a);",
   "if (a < 0) x = ~a; else x = a;" to "x = one_cmpl_abs(a);",
   etc.  */

static int
noce_try_abs (struct noce_if_info *if_info)
{
  rtx cond, target, a, b, c;
  rtx_insn *earliest, *seq;
  int negate;
  bool one_cmpl = false;

  if (!noce_simple_bbs (if_info))
    return FALSE;

  /* Reject modes with signed zeros.  */
  if (HONOR_SIGNED_ZEROS (if_info->x))
    return FALSE;

  /* Recognize A and B as constituting an ABS or NABS.  The canonical
     form is a branch around the negation, taken when the object is the
     first operand of a comparison against 0 that evaluates to true.  */
  a = if_info->a;
  b = if_info->b;
  if (GET_CODE (a) == NEG && rtx_equal_p (XEXP (a, 0), b))
    negate = 0;
  else if (GET_CODE (b) == NEG && rtx_equal_p (XEXP (b, 0), a))
    {
      std::swap (a, b);
      negate = 1;
    }
  else if (GET_CODE (a) == NOT && rtx_equal_p (XEXP (a, 0), b))
    {
      negate = 0;
      one_cmpl = true;
    }
  else if (GET_CODE (b) == NOT && rtx_equal_p (XEXP (b, 0), a))
    {
      std::swap (a, b);
      negate = 1;
      one_cmpl = true;
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
    {
      c = XEXP (cond, 0);
      negate = !negate;
    }
  else
    return FALSE;

  /* Verify that C is zero.  Search one step backward for a
     REG_EQUAL note or a simple source if necessary.  */
  if (REG_P (c))
    {
      rtx set;
      rtx_insn *insn = prev_nonnote_insn (earliest);
      if (insn
	  && BLOCK_FOR_INSN (insn) == BLOCK_FOR_INSN (earliest)
	  && (set = single_set (insn))
	  && rtx_equal_p (SET_DEST (set), c))
	{
	  rtx note = find_reg_equal_equiv_note (insn);
	  if (note)
	    c = XEXP (note, 0);
	  else
	    c = SET_SRC (set);
	}
      else
	return FALSE;
    }
  if (MEM_P (c)
      && GET_CODE (XEXP (c, 0)) == SYMBOL_REF
      && CONSTANT_POOL_ADDRESS_P (XEXP (c, 0)))
    c = get_pool_constant (XEXP (c, 0));

  /* Work around funny ideas get_condition has wrt canonicalization.
     Note that these rtx constants are known to be CONST_INT, and
     therefore imply integer comparisons.
     The one_cmpl case is more complicated, as we want to handle
     only x < 0 ? ~x : x or x >= 0 ? x : ~x to one_cmpl_abs (x)
     and x < 0 ? x : ~x or x >= 0 ? ~x : x to ~one_cmpl_abs (x),
     but not other cases (x > -1 is equivalent of x >= 0).  */
  if (c == constm1_rtx && GET_CODE (cond) == GT)
    ;
  else if (c == const1_rtx && GET_CODE (cond) == LT)
    {
      if (one_cmpl)
	return FALSE;
    }
  else if (c == CONST0_RTX (GET_MODE (b)))
    {
      if (one_cmpl
	  && GET_CODE (cond) != GE
	  && GET_CODE (cond) != LT)
	return FALSE;
    }
  else
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
  if (one_cmpl)
    target = expand_one_cmpl_abs_nojump (GET_MODE (if_info->x), b,
                                         if_info->x);
  else
    target = expand_abs_nojump (GET_MODE (if_info->x), b, if_info->x, 1);

  /* ??? It's a quandary whether cmove would be better here, especially
     for integers.  Perhaps combine will clean things up.  */
  if (target && negate)
    {
      if (one_cmpl)
        target = expand_simple_unop (GET_MODE (target), NOT, target,
                                     if_info->x, 0);
      else
        target = expand_simple_unop (GET_MODE (target), NEG, target,
                                     if_info->x, 0);
    }

  if (! target)
    {
      end_sequence ();
      return FALSE;
    }

  if (target != if_info->x)
    noce_emit_move_insn (if_info->x, target);

  seq = end_ifcvt_sequence (if_info);
  if (!seq)
    return FALSE;

  emit_insn_before_setloc (seq, if_info->jump, INSN_LOCATION (if_info->insn_a));
  if_info->cond = cond;
  if_info->cond_earliest = earliest;
  if_info->transform_name = "noce_try_abs";

  return TRUE;
}

/* Convert "if (m < 0) x = b; else x = 0;" to "x = (m >> C) & b;".  */

static int
noce_try_sign_mask (struct noce_if_info *if_info)
{
  rtx cond, t, m, c;
  rtx_insn *seq;
  machine_mode mode;
  enum rtx_code code;
  bool t_unconditional;

  if (!noce_simple_bbs (if_info))
    return FALSE;

  cond = if_info->cond;
  code = GET_CODE (cond);
  m = XEXP (cond, 0);
  c = XEXP (cond, 1);

  t = NULL_RTX;
  if (if_info->a == const0_rtx)
    {
      if ((code == LT && c == const0_rtx)
	  || (code == LE && c == constm1_rtx))
	t = if_info->b;
    }
  else if (if_info->b == const0_rtx)
    {
      if ((code == GE && c == const0_rtx)
	  || (code == GT && c == constm1_rtx))
	t = if_info->a;
    }

  if (! t || side_effects_p (t))
    return FALSE;

  /* We currently don't handle different modes.  */
  mode = GET_MODE (t);
  if (GET_MODE (m) != mode)
    return FALSE;

  /* This is only profitable if T is unconditionally executed/evaluated in the
     original insn sequence or T is cheap.  The former happens if B is the
     non-zero (T) value and if INSN_B was taken from TEST_BB, or there was no
     INSN_B which can happen for e.g. conditional stores to memory.  For the
     cost computation use the block TEST_BB where the evaluation will end up
     after the transformation.  */
  t_unconditional =
    (t == if_info->b
     && (if_info->insn_b == NULL_RTX
	 || BLOCK_FOR_INSN (if_info->insn_b) == if_info->test_bb));
  if (!(t_unconditional
	|| (set_src_cost (t, mode, optimize_bb_for_speed_p (if_info->test_bb))
	    < COSTS_N_INSNS (2))))
    return FALSE;

  start_sequence ();
  /* Use emit_store_flag to generate "m < 0 ? -1 : 0" instead of expanding
     "(signed) m >> 31" directly.  This benefits targets with specialized
     insns to obtain the signmask, but still uses ashr_optab otherwise.  */
  m = emit_store_flag (gen_reg_rtx (mode), LT, m, const0_rtx, mode, 0, -1);
  t = m ? expand_binop (mode, and_optab, m, t, NULL_RTX, 0, OPTAB_DIRECT)
	: NULL_RTX;

  if (!t)
    {
      end_sequence ();
      return FALSE;
    }

  noce_emit_move_insn (if_info->x, t);

  seq = end_ifcvt_sequence (if_info);
  if (!seq)
    return FALSE;

  emit_insn_before_setloc (seq, if_info->jump, INSN_LOCATION (if_info->insn_a));
  if_info->transform_name = "noce_try_sign_mask";

  return TRUE;
}


/* Optimize away "if (x & C) x |= C" and similar bit manipulation
   transformations.  */

static int
noce_try_bitop (struct noce_if_info *if_info)
{
  rtx cond, x, a, result;
  rtx_insn *seq;
  machine_mode mode;
  enum rtx_code code;
  int bitnum;

  x = if_info->x;
  cond = if_info->cond;
  code = GET_CODE (cond);

  if (!noce_simple_bbs (if_info))
    return FALSE;

  /* Check for no else condition.  */
  if (! rtx_equal_p (x, if_info->b))
    return FALSE;

  /* Check for a suitable condition.  */
  if (code != NE && code != EQ)
    return FALSE;
  if (XEXP (cond, 1) != const0_rtx)
    return FALSE;
  cond = XEXP (cond, 0);

  /* ??? We could also handle AND here.  */
  if (GET_CODE (cond) == ZERO_EXTRACT)
    {
      if (XEXP (cond, 1) != const1_rtx
	  || !CONST_INT_P (XEXP (cond, 2))
	  || ! rtx_equal_p (x, XEXP (cond, 0)))
	return FALSE;
      bitnum = INTVAL (XEXP (cond, 2));
      mode = GET_MODE (x);
      if (BITS_BIG_ENDIAN)
	bitnum = GET_MODE_BITSIZE (mode) - 1 - bitnum;
      if (bitnum < 0 || bitnum >= HOST_BITS_PER_WIDE_INT)
	return FALSE;
    }
  else
    return FALSE;

  a = if_info->a;
  if (GET_CODE (a) == IOR || GET_CODE (a) == XOR)
    {
      /* Check for "if (X & C) x = x op C".  */
      if (! rtx_equal_p (x, XEXP (a, 0))
          || !CONST_INT_P (XEXP (a, 1))
	  || (INTVAL (XEXP (a, 1)) & GET_MODE_MASK (mode))
	     != (unsigned HOST_WIDE_INT) 1 << bitnum)
        return FALSE;

      /* if ((x & C) == 0) x |= C; is transformed to x |= C.   */
      /* if ((x & C) != 0) x |= C; is transformed to nothing.  */
      if (GET_CODE (a) == IOR)
	result = (code == NE) ? a : NULL_RTX;
      else if (code == NE)
	{
	  /* if ((x & C) == 0) x ^= C; is transformed to x |= C.   */
	  result = gen_int_mode ((HOST_WIDE_INT) 1 << bitnum, mode);
	  result = simplify_gen_binary (IOR, mode, x, result);
	}
      else
	{
	  /* if ((x & C) != 0) x ^= C; is transformed to x &= ~C.  */
	  result = gen_int_mode (~((HOST_WIDE_INT) 1 << bitnum), mode);
	  result = simplify_gen_binary (AND, mode, x, result);
	}
    }
  else if (GET_CODE (a) == AND)
    {
      /* Check for "if (X & C) x &= ~C".  */
      if (! rtx_equal_p (x, XEXP (a, 0))
	  || !CONST_INT_P (XEXP (a, 1))
	  || (INTVAL (XEXP (a, 1)) & GET_MODE_MASK (mode))
	     != (~((HOST_WIDE_INT) 1 << bitnum) & GET_MODE_MASK (mode)))
        return FALSE;

      /* if ((x & C) == 0) x &= ~C; is transformed to nothing.  */
      /* if ((x & C) != 0) x &= ~C; is transformed to x &= ~C.  */
      result = (code == EQ) ? a : NULL_RTX;
    }
  else
    return FALSE;

  if (result)
    {
      start_sequence ();
      noce_emit_move_insn (x, result);
      seq = end_ifcvt_sequence (if_info);
      if (!seq)
	return FALSE;

      emit_insn_before_setloc (seq, if_info->jump,
			       INSN_LOCATION (if_info->insn_a));
    }
  if_info->transform_name = "noce_try_bitop";
  return TRUE;
}


/* Similar to get_condition, only the resulting condition must be
   valid at JUMP, instead of at EARLIEST.

   If THEN_ELSE_REVERSED is true, the fallthrough does not go to the
   THEN block of the caller, and we have to reverse the condition.  */

static rtx
noce_get_condition (rtx_insn *jump, rtx_insn **earliest, bool then_else_reversed)
{
  rtx cond, set, tmp;
  bool reverse;

  if (! any_condjump_p (jump))
    return NULL_RTX;

  set = pc_set (jump);

  /* If this branches to JUMP_LABEL when the condition is false,
     reverse the condition.  */
  reverse = (GET_CODE (XEXP (SET_SRC (set), 2)) == LABEL_REF
	     && LABEL_REF_LABEL (XEXP (SET_SRC (set), 2)) == JUMP_LABEL (jump));

  /* We may have to reverse because the caller's if block is not canonical,
     i.e. the THEN block isn't the fallthrough block for the TEST block
     (see find_if_header).  */
  if (then_else_reversed)
    reverse = !reverse;

  /* If the condition variable is a register and is MODE_INT, accept it.  */

  cond = XEXP (SET_SRC (set), 0);
  tmp = XEXP (cond, 0);
  if (REG_P (tmp) && GET_MODE_CLASS (GET_MODE (tmp)) == MODE_INT
      && (GET_MODE (tmp) != BImode
          || !targetm.small_register_classes_for_mode_p (BImode)))
    {
      *earliest = jump;

      if (reverse)
	cond = gen_rtx_fmt_ee (reverse_condition (GET_CODE (cond)),
			       GET_MODE (cond), tmp, XEXP (cond, 1));
      return cond;
    }

  /* Otherwise, fall back on canonicalize_condition to do the dirty
     work of manipulating MODE_CC values and COMPARE rtx codes.  */
  tmp = canonicalize_condition (jump, cond, reverse, earliest,
				NULL_RTX, have_cbranchcc4, true);

  /* We don't handle side-effects in the condition, like handling
     REG_INC notes and making sure no duplicate conditions are emitted.  */
  if (tmp != NULL_RTX && side_effects_p (tmp))
    return NULL_RTX;

  return tmp;
}

/* Return true if OP is ok for if-then-else processing.  */

static int
noce_operand_ok (const_rtx op)
{
  if (side_effects_p (op))
    return FALSE;

  /* We special-case memories, so handle any of them with
     no address side effects.  */
  if (MEM_P (op))
    return ! side_effects_p (XEXP (op, 0));

  return ! may_trap_p (op);
}

/* Return true if X contains a MEM subrtx.  */

static bool
contains_mem_rtx_p (rtx x)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    if (MEM_P (*iter))
      return true;

  return false;
}

/* Return true iff basic block TEST_BB is valid for noce if-conversion.
   The condition used in this if-conversion is in COND.
   In practice, check that TEST_BB ends with a single set
   x := a and all previous computations
   in TEST_BB don't produce any values that are live after TEST_BB.
   In other words, all the insns in TEST_BB are there only
   to compute a value for x.  Put the rtx cost of the insns
   in TEST_BB into COST.  Record whether TEST_BB is a single simple
   set instruction in SIMPLE_P.  */

static bool
bb_valid_for_noce_process_p (basic_block test_bb, rtx cond,
			      unsigned int *cost, bool *simple_p)
{
  if (!test_bb)
    return false;

  rtx_insn *last_insn = last_active_insn (test_bb, FALSE);
  rtx last_set = NULL_RTX;

  rtx cc = cc_in_cond (cond);

  if (!insn_valid_noce_process_p (last_insn, cc))
    return false;
  last_set = single_set (last_insn);

  rtx x = SET_DEST (last_set);
  rtx_insn *first_insn = first_active_insn (test_bb);
  rtx first_set = single_set (first_insn);

  if (!first_set)
    return false;

  /* We have a single simple set, that's okay.  */
  bool speed_p = optimize_bb_for_speed_p (test_bb);

  if (first_insn == last_insn)
    {
      *simple_p = noce_operand_ok (SET_DEST (first_set));
      *cost = insn_rtx_cost (first_set, speed_p);
      return *simple_p;
    }

  rtx_insn *prev_last_insn = PREV_INSN (last_insn);
  gcc_assert (prev_last_insn);

  /* For now, disallow setting x multiple times in test_bb.  */
  if (REG_P (x) && reg_set_between_p (x, first_insn, prev_last_insn))
    return false;

  bitmap test_bb_temps = BITMAP_ALLOC (&reg_obstack);

  /* The regs that are live out of test_bb.  */
  bitmap test_bb_live_out = df_get_live_out (test_bb);

  int potential_cost = insn_rtx_cost (last_set, speed_p);
  rtx_insn *insn;
  FOR_BB_INSNS (test_bb, insn)
    {
      if (insn != last_insn)
	{
	  if (!active_insn_p (insn))
	    continue;

	  if (!insn_valid_noce_process_p (insn, cc))
	    goto free_bitmap_and_fail;

	  rtx sset = single_set (insn);
	  gcc_assert (sset);

	  if (contains_mem_rtx_p (SET_SRC (sset))
	      || !REG_P (SET_DEST (sset))
	      || reg_overlap_mentioned_p (SET_DEST (sset), cond))
	    goto free_bitmap_and_fail;

	  potential_cost += insn_rtx_cost (sset, speed_p);
	  bitmap_set_bit (test_bb_temps, REGNO (SET_DEST (sset)));
	}
    }

  /* If any of the intermediate results in test_bb are live after test_bb
     then fail.  */
  if (bitmap_intersect_p (test_bb_live_out, test_bb_temps))
    goto free_bitmap_and_fail;

  BITMAP_FREE (test_bb_temps);
  *cost = potential_cost;
  *simple_p = false;
  return true;

 free_bitmap_and_fail:
  BITMAP_FREE (test_bb_temps);
  return false;
}

/* We have something like:

     if (x > y)
       { i = a; j = b; k = c; }

   Make it:

     tmp_i = (x > y) ? a : i;
     tmp_j = (x > y) ? b : j;
     tmp_k = (x > y) ? c : k;
     i = tmp_i;
     j = tmp_j;
     k = tmp_k;

   Subsequent passes are expected to clean up the extra moves.

   Look for special cases such as writes to one register which are
   read back in another SET, as might occur in a swap idiom or
   similar.

   These look like:

   if (x > y)
     i = a;
     j = i;

   Which we want to rewrite to:

     tmp_i = (x > y) ? a : i;
     tmp_j = (x > y) ? tmp_i : j;
     i = tmp_i;
     j = tmp_j;

   We can catch these when looking at (SET x y) by keeping a list of the
   registers we would have targeted before if-conversion and looking back
   through it for an overlap with Y.  If we find one, we rewire the
   conditional set to use the temporary we introduced earlier.

   IF_INFO contains the useful information about the block structure and
   jump instructions.  */

static int
noce_convert_multiple_sets (struct noce_if_info *if_info)
{
  basic_block test_bb = if_info->test_bb;
  basic_block then_bb = if_info->then_bb;
  basic_block join_bb = if_info->join_bb;
  rtx_insn *jump = if_info->jump;
  rtx_insn *cond_earliest;
  rtx_insn *insn;

  start_sequence ();

  /* Decompose the condition attached to the jump.  */
  rtx cond = noce_get_condition (jump, &cond_earliest, false);
  rtx x = XEXP (cond, 0);
  rtx y = XEXP (cond, 1);
  rtx_code cond_code = GET_CODE (cond);

  /* The true targets for a conditional move.  */
  auto_vec<rtx> targets;
  /* The temporaries introduced to allow us to not consider register
     overlap.  */
  auto_vec<rtx> temporaries;
  /* The insns we've emitted.  */
  auto_vec<rtx_insn *> unmodified_insns;
  int count = 0;

  FOR_BB_INSNS (then_bb, insn)
    {
      /* Skip over non-insns.  */
      if (!active_insn_p (insn))
	continue;

      rtx set = single_set (insn);
      gcc_checking_assert (set);

      rtx target = SET_DEST (set);
      rtx temp = gen_reg_rtx (GET_MODE (target));
      rtx new_val = SET_SRC (set);
      rtx old_val = target;

      /* If we were supposed to read from an earlier write in this block,
	 we've changed the register allocation.  Rewire the read.  While
	 we are looking, also try to catch a swap idiom.  */
      for (int i = count - 1; i >= 0; --i)
	if (reg_overlap_mentioned_p (new_val, targets[i]))
	  {
	    /* Catch a "swap" style idiom.  */
	    if (find_reg_note (insn, REG_DEAD, new_val) != NULL_RTX)
	      /* The write to targets[i] is only live until the read
		 here.  As the condition codes match, we can propagate
		 the set to here.  */
	      new_val = SET_SRC (single_set (unmodified_insns[i]));
	    else
	      new_val = temporaries[i];
	    break;
	  }

      /* If we had a non-canonical conditional jump (i.e. one where
	 the fallthrough is to the "else" case) we need to reverse
	 the conditional select.  */
      if (if_info->then_else_reversed)
	std::swap (old_val, new_val);


      /* We allow simple lowpart register subreg SET sources in
	 bb_ok_for_noce_convert_multiple_sets.  Be careful when processing
	 sequences like:
	 (set (reg:SI r1) (reg:SI r2))
	 (set (reg:HI r3) (subreg:HI (r1)))
	 For the second insn new_val or old_val (r1 in this example) will be
	 taken from the temporaries and have the wider mode which will not
	 match with the mode of the other source of the conditional move, so
	 we'll end up trying to emit r4:HI = cond ? (r1:SI) : (r3:HI).
	 Wrap the two cmove operands into subregs if appropriate to prevent
	 that.  */
      if (GET_MODE (new_val) != GET_MODE (temp))
	{
	  machine_mode src_mode = GET_MODE (new_val);
	  machine_mode dst_mode = GET_MODE (temp);
	  if (GET_MODE_SIZE (src_mode) <= GET_MODE_SIZE (dst_mode))
	    {
	      end_sequence ();
	      return FALSE;
	    }
	  new_val = lowpart_subreg (dst_mode, new_val, src_mode);
	}
      if (GET_MODE (old_val) != GET_MODE (temp))
	{
	  machine_mode src_mode = GET_MODE (old_val);
	  machine_mode dst_mode = GET_MODE (temp);
	  if (GET_MODE_SIZE (src_mode) <= GET_MODE_SIZE (dst_mode))
	    {
	      end_sequence ();
	      return FALSE;
	    }
	  old_val = lowpart_subreg (dst_mode, old_val, src_mode);
	}

      /* Actually emit the conditional move.  */
      rtx temp_dest = noce_emit_cmove (if_info, temp, cond_code,
				       x, y, new_val, old_val);

      /* If we failed to expand the conditional move, drop out and don't
	 try to continue.  */
      if (temp_dest == NULL_RTX)
	{
	  end_sequence ();
	  return FALSE;
	}

      /* Bookkeeping.  */
      count++;
      targets.safe_push (target);
      temporaries.safe_push (temp_dest);
      unmodified_insns.safe_push (insn);
    }

  /* We must have seen some sort of insn to insert, otherwise we were
     given an empty BB to convert, and we can't handle that.  */
  gcc_assert (!unmodified_insns.is_empty ());

  /* Now fixup the assignments.  */
  for (int i = 0; i < count; i++)
    noce_emit_move_insn (targets[i], temporaries[i]);

  /* Actually emit the sequence.  */
  rtx_insn *seq = get_insns ();

  for (insn = seq; insn; insn = NEXT_INSN (insn))
    set_used_flags (insn);

  /* Mark all our temporaries and targets as used.  */
  for (int i = 0; i < count; i++)
    {
      set_used_flags (temporaries[i]);
      set_used_flags (targets[i]);
    }

  set_used_flags (cond);
  set_used_flags (x);
  set_used_flags (y);

  unshare_all_rtl_in_chain (seq);
  end_sequence ();

  if (!seq)
    return FALSE;

  for (insn = seq; insn; insn = NEXT_INSN (insn))
    if (JUMP_P (insn)
	|| recog_memoized (insn) == -1)
      return FALSE;

  emit_insn_before_setloc (seq, if_info->jump,
			   INSN_LOCATION (unmodified_insns.last ()));

  /* Clean up THEN_BB and the edges in and out of it.  */
  remove_edge (find_edge (test_bb, join_bb));
  remove_edge (find_edge (then_bb, join_bb));
  redirect_edge_and_branch_force (single_succ_edge (test_bb), join_bb);
  delete_basic_block (then_bb);
  num_true_changes++;

  /* Maybe merge blocks now the jump is simple enough.  */
  if (can_merge_blocks_p (test_bb, join_bb))
    {
      merge_blocks (test_bb, join_bb);
      num_true_changes++;
    }

  num_updated_if_blocks++;
  if_info->transform_name = "noce_convert_multiple_sets";
  return TRUE;
}

/* Return true iff basic block TEST_BB is comprised of only
   (SET (REG) (REG)) insns suitable for conversion to a series
   of conditional moves.  FORNOW: Use II to find the expected cost of
   the branch into/over TEST_BB.

   TODO: This creates an implicit "magic number" for branch_cost.
   II->branch_cost now guides the maximum number of set instructions in
   a basic block which is considered profitable to completely
   if-convert.  */

static bool
bb_ok_for_noce_convert_multiple_sets (basic_block test_bb,
				      struct noce_if_info *ii)
{
  rtx_insn *insn;
  unsigned count = 0;
  unsigned param = PARAM_VALUE (PARAM_MAX_RTL_IF_CONVERSION_INSNS);
  unsigned limit = MIN (ii->branch_cost, param);

  FOR_BB_INSNS (test_bb, insn)
    {
      /* Skip over notes etc.  */
      if (!active_insn_p (insn))
	continue;

      /* We only handle SET insns.  */
      rtx set = single_set (insn);
      if (set == NULL_RTX)
	return false;

      rtx dest = SET_DEST (set);
      rtx src = SET_SRC (set);

      /* We can possibly relax this, but for now only handle REG to REG
	 (including subreg) moves.  This avoids any issues that might come
	 from introducing loads/stores that might violate data-race-freedom
	 guarantees.  */
      if (!REG_P (dest))
	return false;

      if (!(REG_P (src)
	   || (GET_CODE (src) == SUBREG && REG_P (SUBREG_REG (src))
	       && subreg_lowpart_p (src))))
	return false;

      /* Destination must be appropriate for a conditional write.  */
      if (!noce_operand_ok (dest))
	return false;

      /* We must be able to conditionally move in this mode.  */
      if (!can_conditionally_move_p (GET_MODE (dest)))
	return false;

      /* FORNOW: Our cost model is a count of the number of instructions we
	 would if-convert.  This is suboptimal, and should be improved as part
	 of a wider rework of branch_cost.  */
      if (++count > limit)
	return false;
    }

  return count > 1;
}

/* Given a simple IF-THEN-JOIN or IF-THEN-ELSE-JOIN block, attempt to convert
   it without using conditional execution.  Return TRUE if we were successful
   at converting the block.  */

static int
noce_process_if_block (struct noce_if_info *if_info)
{
  basic_block test_bb = if_info->test_bb;	/* test block */
  basic_block then_bb = if_info->then_bb;	/* THEN */
  basic_block else_bb = if_info->else_bb;	/* ELSE or NULL */
  basic_block join_bb = if_info->join_bb;	/* JOIN */
  rtx_insn *jump = if_info->jump;
  rtx cond = if_info->cond;
  rtx_insn *insn_a, *insn_b;
  rtx set_a, set_b;
  rtx orig_x, x, a, b;

  /* We're looking for patterns of the form

     (1) if (...) x = a; else x = b;
     (2) x = b; if (...) x = a;
     (3) if (...) x = a;   // as if with an initial x = x.
     (4) if (...) { x = a; y = b; z = c; }  // Like 3, for multiple SETS.
     The later patterns require jumps to be more expensive.
     For the if (...) x = a; else x = b; case we allow multiple insns
     inside the then and else blocks as long as their only effect is
     to calculate a value for x.
     ??? For future expansion, further expand the "multiple X" rules.  */

  /* First look for multiple SETS.  */
  if (!else_bb
      && HAVE_conditional_move
      && !HAVE_cc0
      && bb_ok_for_noce_convert_multiple_sets (then_bb, if_info))
    {
      if (noce_convert_multiple_sets (if_info))
	{
	  if (dump_file && if_info->transform_name)
	    fprintf (dump_file, "if-conversion succeeded through %s\n",
		     if_info->transform_name);
	  return TRUE;
	}
    }

  if (! bb_valid_for_noce_process_p (then_bb, cond, &if_info->then_cost,
				    &if_info->then_simple))
    return false;

  if (else_bb
      && ! bb_valid_for_noce_process_p (else_bb, cond, &if_info->else_cost,
				      &if_info->else_simple))
    return false;

  insn_a = last_active_insn (then_bb, FALSE);
  set_a = single_set (insn_a);
  gcc_assert (set_a);

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
      insn_b = last_active_insn (else_bb, FALSE);
      set_b = single_set (insn_b);
      gcc_assert (set_b);

      if (!rtx_interchangeable_p (x, SET_DEST (set_b)))
	return FALSE;
    }
  else
    {
      insn_b = prev_nonnote_nondebug_insn (if_info->cond_earliest);
      /* We're going to be moving the evaluation of B down from above
	 COND_EARLIEST to JUMP.  Make sure the relevant data is still
	 intact.  */
      if (! insn_b
	  || BLOCK_FOR_INSN (insn_b) != BLOCK_FOR_INSN (if_info->cond_earliest)
	  || !NONJUMP_INSN_P (insn_b)
	  || (set_b = single_set (insn_b)) == NULL_RTX
	  || ! rtx_interchangeable_p (x, SET_DEST (set_b))
	  || ! noce_operand_ok (SET_SRC (set_b))
	  || reg_overlap_mentioned_p (x, SET_SRC (set_b))
	  || modified_between_p (SET_SRC (set_b), insn_b, jump)
	  /* Avoid extending the lifetime of hard registers on small
	     register class machines.  */
	  || (REG_P (SET_SRC (set_b))
	      && HARD_REGISTER_P (SET_SRC (set_b))
	      && targetm.small_register_classes_for_mode_p
		   (GET_MODE (SET_SRC (set_b))))
	  /* Likewise with X.  In particular this can happen when
	     noce_get_condition looks farther back in the instruction
	     stream than one might expect.  */
	  || reg_overlap_mentioned_p (x, cond)
	  || reg_overlap_mentioned_p (x, a)
	  || modified_between_p (x, insn_b, jump))
	{
	  insn_b = NULL;
	  set_b = NULL_RTX;
	}
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
  if_info->orig_x = orig_x;
  if (!REG_P (x)
      || (HARD_REGISTER_P (x)
	  && targetm.small_register_classes_for_mode_p (GET_MODE (x))))
    {
      if (GET_MODE (x) == BLKmode)
	return FALSE;

      if (GET_CODE (x) == ZERO_EXTRACT
	  && (!CONST_INT_P (XEXP (x, 1))
	      || !CONST_INT_P (XEXP (x, 2))))
	return FALSE;

      x = gen_reg_rtx (GET_MODE (GET_CODE (x) == STRICT_LOW_PART
				 ? XEXP (x, 0) : x));
    }

  /* Don't operate on sources that may trap or are volatile.  */
  if (! noce_operand_ok (a) || ! noce_operand_ok (b))
    return FALSE;

 retry:
  /* Set up the info block for our subroutines.  */
  if_info->insn_a = insn_a;
  if_info->insn_b = insn_b;
  if_info->x = x;
  if_info->a = a;
  if_info->b = b;

  /* Try optimizations in some approximation of a useful order.  */
  /* ??? Should first look to see if X is live incoming at all.  If it
     isn't, we don't need anything but an unconditional set.  */

  /* Look and see if A and B are really the same.  Avoid creating silly
     cmove constructs that no one will fix up later.  */
  if (noce_simple_bbs (if_info)
      && rtx_interchangeable_p (a, b))
    {
      /* If we have an INSN_B, we don't have to create any new rtl.  Just
	 move the instruction that we already have.  If we don't have an
	 INSN_B, that means that A == X, and we've got a noop move.  In
	 that case don't do anything and let the code below delete INSN_A.  */
      if (insn_b && else_bb)
	{
	  rtx note;

	  if (else_bb && insn_b == BB_END (else_bb))
	    BB_END (else_bb) = PREV_INSN (insn_b);
	  reorder_insns (insn_b, insn_b, PREV_INSN (jump));

	  /* If there was a REG_EQUAL note, delete it since it may have been
	     true due to this insn being after a jump.  */
	  if ((note = find_reg_note (insn_b, REG_EQUAL, NULL_RTX)) != 0)
	    remove_note (insn_b, note);

	  insn_b = NULL;
	}
      /* If we have "x = b; if (...) x = a;", and x has side-effects, then
	 x must be executed twice.  */
      else if (insn_b && side_effects_p (orig_x))
	return FALSE;

      x = orig_x;
      goto success;
    }

  if (!set_b && MEM_P (orig_x))
    /* We want to avoid store speculation to avoid cases like
	 if (pthread_mutex_trylock(mutex))
	   ++global_variable;
       Rather than go to much effort here, we rely on the SSA optimizers,
       which do a good enough job these days.  */
    return FALSE;

  if (noce_try_move (if_info))
    goto success;
  if (noce_try_ifelse_collapse (if_info))
    goto success;
  if (noce_try_store_flag (if_info))
    goto success;
  if (noce_try_bitop (if_info))
    goto success;
  if (noce_try_minmax (if_info))
    goto success;
  if (noce_try_abs (if_info))
    goto success;
  if (noce_try_inverse_constants (if_info))
    goto success;
  if (!targetm.have_conditional_execution ()
      && noce_try_store_flag_constants (if_info))
    goto success;
  if (HAVE_conditional_move
      && noce_try_cmove (if_info))
    goto success;
  if (! targetm.have_conditional_execution ())
    {
      if (noce_try_addcc (if_info))
	goto success;
      if (noce_try_store_flag_mask (if_info))
	goto success;
      if (HAVE_conditional_move
	  && noce_try_cmove_arith (if_info))
	goto success;
      if (noce_try_sign_mask (if_info))
	goto success;
    }

  if (!else_bb && set_b)
    {
      insn_b = NULL;
      set_b = NULL_RTX;
      b = orig_x;
      goto retry;
    }

  return FALSE;

 success:
  if (dump_file && if_info->transform_name)
    fprintf (dump_file, "if-conversion succeeded through %s\n",
	     if_info->transform_name);

  /* If we used a temporary, fix it up now.  */
  if (orig_x != x)
    {
      rtx_insn *seq;

      start_sequence ();
      noce_emit_move_insn (orig_x, x);
      seq = get_insns ();
      set_used_flags (orig_x);
      unshare_all_rtl_in_chain (seq);
      end_sequence ();

      emit_insn_before_setloc (seq, BB_END (test_bb), INSN_LOCATION (insn_a));
    }

  /* The original THEN and ELSE blocks may now be removed.  The test block
     must now jump to the join block.  If the test block and the join block
     can be merged, do so.  */
  if (else_bb)
    {
      delete_basic_block (else_bb);
      num_true_changes++;
    }
  else
    remove_edge (find_edge (test_bb, join_bb));

  remove_edge (find_edge (then_bb, join_bb));
  redirect_edge_and_branch_force (single_succ_edge (test_bb), join_bb);
  delete_basic_block (then_bb);
  num_true_changes++;

  if (can_merge_blocks_p (test_bb, join_bb))
    {
      merge_blocks (test_bb, join_bb);
      num_true_changes++;
    }

  num_updated_if_blocks++;
  return TRUE;
}

/* Check whether a block is suitable for conditional move conversion.
   Every insn must be a simple set of a register to a constant or a
   register.  For each assignment, store the value in the pointer map
   VALS, keyed indexed by register pointer, then store the register
   pointer in REGS.  COND is the condition we will test.  */

static int
check_cond_move_block (basic_block bb,
		       hash_map<rtx, rtx> *vals,
		       vec<rtx> *regs,
		       rtx cond)
{
  rtx_insn *insn;
  rtx cc = cc_in_cond (cond);

   /* We can only handle simple jumps at the end of the basic block.
      It is almost impossible to update the CFG otherwise.  */
  insn = BB_END (bb);
  if (JUMP_P (insn) && !onlyjump_p (insn))
    return FALSE;

  FOR_BB_INSNS (bb, insn)
    {
      rtx set, dest, src;

      if (!NONDEBUG_INSN_P (insn) || JUMP_P (insn))
	continue;
      set = single_set (insn);
      if (!set)
	return FALSE;

      dest = SET_DEST (set);
      src = SET_SRC (set);
      if (!REG_P (dest)
	  || (HARD_REGISTER_P (dest)
	      && targetm.small_register_classes_for_mode_p (GET_MODE (dest))))
	return FALSE;

      if (!CONSTANT_P (src) && !register_operand (src, VOIDmode))
	return FALSE;

      if (side_effects_p (src) || side_effects_p (dest))
	return FALSE;

      if (may_trap_p (src) || may_trap_p (dest))
	return FALSE;

      /* Don't try to handle this if the source register was
	 modified earlier in the block.  */
      if ((REG_P (src)
	   && vals->get (src))
	  || (GET_CODE (src) == SUBREG && REG_P (SUBREG_REG (src))
	      && vals->get (SUBREG_REG (src))))
	return FALSE;

      /* Don't try to handle this if the destination register was
	 modified earlier in the block.  */
      if (vals->get (dest))
	return FALSE;

      /* Don't try to handle this if the condition uses the
	 destination register.  */
      if (reg_overlap_mentioned_p (dest, cond))
	return FALSE;

      /* Don't try to handle this if the source register is modified
	 later in the block.  */
      if (!CONSTANT_P (src)
	  && modified_between_p (src, insn, NEXT_INSN (BB_END (bb))))
	return FALSE;

      /* Skip it if the instruction to be moved might clobber CC.  */
      if (cc && set_of (cc, insn))
	return FALSE;

      vals->put (dest, src);

      regs->safe_push (dest);
    }

  return TRUE;
}

/* Given a basic block BB suitable for conditional move conversion,
   a condition COND, and pointer maps THEN_VALS and ELSE_VALS containing
   the register values depending on COND, emit the insns in the block as
   conditional moves.  If ELSE_BLOCK is true, THEN_BB was already
   processed.  The caller has started a sequence for the conversion.
   Return true if successful, false if something goes wrong.  */

static bool
cond_move_convert_if_block (struct noce_if_info *if_infop,
			    basic_block bb, rtx cond,
			    hash_map<rtx, rtx> *then_vals,
			    hash_map<rtx, rtx> *else_vals,
			    bool else_block_p)
{
  enum rtx_code code;
  rtx_insn *insn;
  rtx cond_arg0, cond_arg1;

  code = GET_CODE (cond);
  cond_arg0 = XEXP (cond, 0);
  cond_arg1 = XEXP (cond, 1);

  FOR_BB_INSNS (bb, insn)
    {
      rtx set, target, dest, t, e;

      /* ??? Maybe emit conditional debug insn?  */
      if (!NONDEBUG_INSN_P (insn) || JUMP_P (insn))
	continue;
      set = single_set (insn);
      gcc_assert (set && REG_P (SET_DEST (set)));

      dest = SET_DEST (set);

      rtx *then_slot = then_vals->get (dest);
      rtx *else_slot = else_vals->get (dest);
      t = then_slot ? *then_slot : NULL_RTX;
      e = else_slot ? *else_slot : NULL_RTX;

      if (else_block_p)
	{
	  /* If this register was set in the then block, we already
	     handled this case there.  */
	  if (t)
	    continue;
	  t = dest;
	  gcc_assert (e);
	}
      else
	{
	  gcc_assert (t);
	  if (!e)
	    e = dest;
	}

      target = noce_emit_cmove (if_infop, dest, code, cond_arg0, cond_arg1,
				t, e);
      if (!target)
	return false;

      if (target != dest)
	noce_emit_move_insn (dest, target);
    }

  return true;
}

/* Given a simple IF-THEN-JOIN or IF-THEN-ELSE-JOIN block, attempt to convert
   it using only conditional moves.  Return TRUE if we were successful at
   converting the block.  */

static int
cond_move_process_if_block (struct noce_if_info *if_info)
{
  basic_block test_bb = if_info->test_bb;
  basic_block then_bb = if_info->then_bb;
  basic_block else_bb = if_info->else_bb;
  basic_block join_bb = if_info->join_bb;
  rtx_insn *jump = if_info->jump;
  rtx cond = if_info->cond;
  rtx_insn *seq, *loc_insn;
  rtx reg;
  int c;
  vec<rtx> then_regs = vNULL;
  vec<rtx> else_regs = vNULL;
  unsigned int i;
  int success_p = FALSE;
  int limit = PARAM_VALUE (PARAM_MAX_RTL_IF_CONVERSION_INSNS);

  /* Build a mapping for each block to the value used for each
     register.  */
  hash_map<rtx, rtx> then_vals;
  hash_map<rtx, rtx> else_vals;

  /* Make sure the blocks are suitable.  */
  if (!check_cond_move_block (then_bb, &then_vals, &then_regs, cond)
      || (else_bb
	  && !check_cond_move_block (else_bb, &else_vals, &else_regs, cond)))
    goto done;

  /* Make sure the blocks can be used together.  If the same register
     is set in both blocks, and is not set to a constant in both
     cases, then both blocks must set it to the same register.  We
     have already verified that if it is set to a register, that the
     source register does not change after the assignment.  Also count
     the number of registers set in only one of the blocks.  */
  c = 0;
  FOR_EACH_VEC_ELT (then_regs, i, reg)
    {
      rtx *then_slot = then_vals.get (reg);
      rtx *else_slot = else_vals.get (reg);

      gcc_checking_assert (then_slot);
      if (!else_slot)
	++c;
      else
	{
	  rtx then_val = *then_slot;
	  rtx else_val = *else_slot;
	  if (!CONSTANT_P (then_val) && !CONSTANT_P (else_val)
	      && !rtx_equal_p (then_val, else_val))
	    goto done;
	}
    }

  /* Finish off c for MAX_CONDITIONAL_EXECUTE.  */
  FOR_EACH_VEC_ELT (else_regs, i, reg)
    {
      gcc_checking_assert (else_vals.get (reg));
      if (!then_vals.get (reg))
	++c;
    }

  /* Make sure it is reasonable to convert this block.  What matters
     is the number of assignments currently made in only one of the
     branches, since if we convert we are going to always execute
     them.  */
  if (c > MAX_CONDITIONAL_EXECUTE
      || c > limit)
    goto done;

  /* Try to emit the conditional moves.  First do the then block,
     then do anything left in the else blocks.  */
  start_sequence ();
  if (!cond_move_convert_if_block (if_info, then_bb, cond,
				   &then_vals, &else_vals, false)
      || (else_bb
	  && !cond_move_convert_if_block (if_info, else_bb, cond,
					  &then_vals, &else_vals, true)))
    {
      end_sequence ();
      goto done;
    }
  seq = end_ifcvt_sequence (if_info);
  if (!seq)
    goto done;

  loc_insn = first_active_insn (then_bb);
  if (!loc_insn)
    {
      loc_insn = first_active_insn (else_bb);
      gcc_assert (loc_insn);
    }
  emit_insn_before_setloc (seq, jump, INSN_LOCATION (loc_insn));

  if (else_bb)
    {
      delete_basic_block (else_bb);
      num_true_changes++;
    }
  else
    remove_edge (find_edge (test_bb, join_bb));

  remove_edge (find_edge (then_bb, join_bb));
  redirect_edge_and_branch_force (single_succ_edge (test_bb), join_bb);
  delete_basic_block (then_bb);
  num_true_changes++;

  if (can_merge_blocks_p (test_bb, join_bb))
    {
      merge_blocks (test_bb, join_bb);
      num_true_changes++;
    }

  num_updated_if_blocks++;
  success_p = TRUE;

done:
  then_regs.release ();
  else_regs.release ();
  return success_p;
}


/* Determine if a given basic block heads a simple IF-THEN-JOIN or an
   IF-THEN-ELSE-JOIN block.

   If so, we'll try to convert the insns to not require the branch,
   using only transformations that do not require conditional execution.

   Return TRUE if we were successful at converting the block.  */

static int
noce_find_if_block (basic_block test_bb, edge then_edge, edge else_edge,
		    int pass)
{
  basic_block then_bb, else_bb, join_bb;
  bool then_else_reversed = false;
  rtx_insn *jump;
  rtx cond;
  rtx_insn *cond_earliest;
  struct noce_if_info if_info;

  /* We only ever should get here before reload.  */
  gcc_assert (!reload_completed);

  /* Recognize an IF-THEN-ELSE-JOIN block.  */
  if (single_pred_p (then_edge->dest)
      && single_succ_p (then_edge->dest)
      && single_pred_p (else_edge->dest)
      && single_succ_p (else_edge->dest)
      && single_succ (then_edge->dest) == single_succ (else_edge->dest))
    {
      then_bb = then_edge->dest;
      else_bb = else_edge->dest;
      join_bb = single_succ (then_bb);
    }
  /* Recognize an IF-THEN-JOIN block.  */
  else if (single_pred_p (then_edge->dest)
	   && single_succ_p (then_edge->dest)
	   && single_succ (then_edge->dest) == else_edge->dest)
    {
      then_bb = then_edge->dest;
      else_bb = NULL_BLOCK;
      join_bb = else_edge->dest;
    }
  /* Recognize an IF-ELSE-JOIN block.  We can have those because the order
     of basic blocks in cfglayout mode does not matter, so the fallthrough
     edge can go to any basic block (and not just to bb->next_bb, like in
     cfgrtl mode).  */
  else if (single_pred_p (else_edge->dest)
	   && single_succ_p (else_edge->dest)
	   && single_succ (else_edge->dest) == then_edge->dest)
    {
      /* The noce transformations do not apply to IF-ELSE-JOIN blocks.
	 To make this work, we have to invert the THEN and ELSE blocks
	 and reverse the jump condition.  */
      then_bb = else_edge->dest;
      else_bb = NULL_BLOCK;
      join_bb = single_succ (then_bb);
      then_else_reversed = true;
    }
  else
    /* Not a form we can handle.  */
    return FALSE;

  /* The edges of the THEN and ELSE blocks cannot have complex edges.  */
  if (single_succ_edge (then_bb)->flags & EDGE_COMPLEX)
    return FALSE;
  if (else_bb
      && single_succ_edge (else_bb)->flags & EDGE_COMPLEX)
    return FALSE;

  num_possible_if_blocks++;

  if (dump_file)
    {
      fprintf (dump_file,
	       "\nIF-THEN%s-JOIN block found, pass %d, test %d, then %d",
	       (else_bb) ? "-ELSE" : "",
	       pass, test_bb->index, then_bb->index);

      if (else_bb)
	fprintf (dump_file, ", else %d", else_bb->index);

      fprintf (dump_file, ", join %d\n", join_bb->index);
    }

  /* If the conditional jump is more than just a conditional
     jump, then we can not do if-conversion on this block.  */
  jump = BB_END (test_bb);
  if (! onlyjump_p (jump))
    return FALSE;

  /* If this is not a standard conditional jump, we can't parse it.  */
  cond = noce_get_condition (jump, &cond_earliest, then_else_reversed);
  if (!cond)
    return FALSE;

  /* We must be comparing objects whose modes imply the size.  */
  if (GET_MODE (XEXP (cond, 0)) == BLKmode)
    return FALSE;

  /* Initialize an IF_INFO struct to pass around.  */
  memset (&if_info, 0, sizeof if_info);
  if_info.test_bb = test_bb;
  if_info.then_bb = then_bb;
  if_info.else_bb = else_bb;
  if_info.join_bb = join_bb;
  if_info.cond = cond;
  if_info.cond_earliest = cond_earliest;
  if_info.jump = jump;
  if_info.then_else_reversed = then_else_reversed;
  if_info.branch_cost = BRANCH_COST (optimize_bb_for_speed_p (test_bb),
				     predictable_edge_p (then_edge));

  /* Do the real work.  */

  if (noce_process_if_block (&if_info))
    return TRUE;

  if (HAVE_conditional_move
      && cond_move_process_if_block (&if_info))
    return TRUE;

  return FALSE;
}


/* Merge the blocks and mark for local life update.  */

static void
merge_if_block (struct ce_if_block * ce_info)
{
  basic_block test_bb = ce_info->test_bb;	/* last test block */
  basic_block then_bb = ce_info->then_bb;	/* THEN */
  basic_block else_bb = ce_info->else_bb;	/* ELSE or NULL */
  basic_block join_bb = ce_info->join_bb;	/* join block */
  basic_block combo_bb;

  /* All block merging is done into the lower block numbers.  */

  combo_bb = test_bb;
  df_set_bb_dirty (test_bb);

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
	  merge_blocks (combo_bb, bb);
	  num_true_changes++;
	}
      while (bb != last_test_bb);
    }

  /* Merge TEST block into THEN block.  Normally the THEN block won't have a
     label, but it might if there were || tests.  That label's count should be
     zero, and it normally should be removed.  */

  if (then_bb)
    {
      /* If THEN_BB has no successors, then there's a BARRIER after it.
	 If COMBO_BB has more than one successor (THEN_BB), then that BARRIER
	 is no longer needed, and in fact it is incorrect to leave it in
	 the insn stream.  */
      if (EDGE_COUNT (then_bb->succs) == 0
	  && EDGE_COUNT (combo_bb->succs) > 1)
	{
	  rtx_insn *end = NEXT_INSN (BB_END (then_bb));
	  while (end && NOTE_P (end) && !NOTE_INSN_BASIC_BLOCK_P (end))
	    end = NEXT_INSN (end);

	  if (end && BARRIER_P (end))
	    delete_insn (end);
	}
      merge_blocks (combo_bb, then_bb);
      num_true_changes++;
    }

  /* The ELSE block, if it existed, had a label.  That label count
     will almost always be zero, but odd things can happen when labels
     get their addresses taken.  */
  if (else_bb)
    {
      /* If ELSE_BB has no successors, then there's a BARRIER after it.
	 If COMBO_BB has more than one successor (ELSE_BB), then that BARRIER
	 is no longer needed, and in fact it is incorrect to leave it in
	 the insn stream.  */
      if (EDGE_COUNT (else_bb->succs) == 0
	  && EDGE_COUNT (combo_bb->succs) > 1)
	{
	  rtx_insn *end = NEXT_INSN (BB_END (else_bb));
	  while (end && NOTE_P (end) && !NOTE_INSN_BASIC_BLOCK_P (end))
	    end = NEXT_INSN (end);

	  if (end && BARRIER_P (end))
	    delete_insn (end);
	}
      merge_blocks (combo_bb, else_bb);
      num_true_changes++;
    }

  /* If there was no join block reported, that means it was not adjacent
     to the others, and so we cannot merge them.  */

  if (! join_bb)
    {
      rtx_insn *last = BB_END (combo_bb);

      /* The outgoing edge for the current COMBO block should already
	 be correct.  Verify this.  */
      if (EDGE_COUNT (combo_bb->succs) == 0)
	gcc_assert (find_reg_note (last, REG_NORETURN, NULL)
		    || (NONJUMP_INSN_P (last)
			&& GET_CODE (PATTERN (last)) == TRAP_IF
			&& (TRAP_CONDITION (PATTERN (last))
			    == const_true_rtx)));

      else
      /* There should still be something at the end of the THEN or ELSE
         blocks taking us to our final destination.  */
	gcc_assert (JUMP_P (last)
		    || (EDGE_SUCC (combo_bb, 0)->dest
			== EXIT_BLOCK_PTR_FOR_FN (cfun)
			&& CALL_P (last)
			&& SIBLING_CALL_P (last))
		    || ((EDGE_SUCC (combo_bb, 0)->flags & EDGE_EH)
			&& can_throw_internal (last)));
    }

  /* The JOIN block may have had quite a number of other predecessors too.
     Since we've already merged the TEST, THEN and ELSE blocks, we should
     have only one remaining edge from our if-then-else diamond.  If there
     is more than one remaining edge, it must come from elsewhere.  There
     may be zero incoming edges if the THEN block didn't actually join
     back up (as with a call to a non-return function).  */
  else if (EDGE_COUNT (join_bb->preds) < 2
	   && join_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      /* We can merge the JOIN cleanly and update the dataflow try
	 again on this pass.*/
      merge_blocks (combo_bb, join_bb);
      num_true_changes++;
    }
  else
    {
      /* We cannot merge the JOIN.  */

      /* The outgoing edge for the current COMBO block should already
	 be correct.  Verify this.  */
      gcc_assert (single_succ_p (combo_bb)
		  && single_succ (combo_bb) == join_bb);

      /* Remove the jump and cruft from the end of the COMBO block.  */
      if (join_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
	tidy_fallthru_edge (single_succ_edge (combo_bb));
    }

  num_updated_if_blocks++;
}

/* Find a block ending in a simple IF condition and try to transform it
   in some way.  When converting a multi-block condition, put the new code
   in the first such block and delete the rest.  Return a pointer to this
   first block if some transformation was done.  Return NULL otherwise.  */

static basic_block
find_if_header (basic_block test_bb, int pass)
{
  ce_if_block ce_info;
  edge then_edge;
  edge else_edge;

  /* The kind of block we're looking for has exactly two successors.  */
  if (EDGE_COUNT (test_bb->succs) != 2)
    return NULL;

  then_edge = EDGE_SUCC (test_bb, 0);
  else_edge = EDGE_SUCC (test_bb, 1);

  if (df_get_bb_dirty (then_edge->dest))
    return NULL;
  if (df_get_bb_dirty (else_edge->dest))
    return NULL;

  /* Neither edge should be abnormal.  */
  if ((then_edge->flags & EDGE_COMPLEX)
      || (else_edge->flags & EDGE_COMPLEX))
    return NULL;

  /* Nor exit the loop.  */
  if ((then_edge->flags & EDGE_LOOP_EXIT)
      || (else_edge->flags & EDGE_LOOP_EXIT))
    return NULL;

  /* The THEN edge is canonically the one that falls through.  */
  if (then_edge->flags & EDGE_FALLTHRU)
    ;
  else if (else_edge->flags & EDGE_FALLTHRU)
    std::swap (then_edge, else_edge);
  else
    /* Otherwise this must be a multiway branch of some sort.  */
    return NULL;

  memset (&ce_info, 0, sizeof (ce_info));
  ce_info.test_bb = test_bb;
  ce_info.then_bb = then_edge->dest;
  ce_info.else_bb = else_edge->dest;
  ce_info.pass = pass;

#ifdef IFCVT_MACHDEP_INIT
  IFCVT_MACHDEP_INIT (&ce_info);
#endif

  if (!reload_completed
      && noce_find_if_block (test_bb, then_edge, else_edge, pass))
    goto success;

  if (reload_completed
      && targetm.have_conditional_execution ()
      && cond_exec_find_if_block (&ce_info))
    goto success;

  if (targetm.have_trap ()
      && optab_handler (ctrap_optab, word_mode) != CODE_FOR_nothing
      && find_cond_trap (test_bb, then_edge, else_edge))
    goto success;

  if (dom_info_state (CDI_POST_DOMINATORS) >= DOM_NO_FAST_QUERY
      && (reload_completed || !targetm.have_conditional_execution ()))
    {
      if (find_if_case_1 (test_bb, then_edge, else_edge))
	goto success;
      if (find_if_case_2 (test_bb, then_edge, else_edge))
	goto success;
    }

  return NULL;

 success:
  if (dump_file)
    fprintf (dump_file, "Conversion succeeded on pass %d.\n", pass);
  /* Set this so we continue looking.  */
  cond_exec_changed_p = TRUE;
  return ce_info.test_bb;
}

/* Return true if a block has two edges, one of which falls through to the next
   block, and the other jumps to a specific block, so that we can tell if the
   block is part of an && test or an || test.  Returns either -1 or the number
   of non-note, non-jump, non-USE/CLOBBER insns in the block.  */

static int
block_jumps_and_fallthru_p (basic_block cur_bb, basic_block target_bb)
{
  edge cur_edge;
  int fallthru_p = FALSE;
  int jump_p = FALSE;
  rtx_insn *insn;
  rtx_insn *end;
  int n_insns = 0;
  edge_iterator ei;

  if (!cur_bb || !target_bb)
    return -1;

  /* If no edges, obviously it doesn't jump or fallthru.  */
  if (EDGE_COUNT (cur_bb->succs) == 0)
    return FALSE;

  FOR_EACH_EDGE (cur_edge, ei, cur_bb->succs)
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
  end = BB_END (cur_bb);
  insn = BB_HEAD (cur_bb);

  while (insn != NULL_RTX)
    {
      if (CALL_P (insn))
	return -1;

      if (INSN_P (insn)
	  && !JUMP_P (insn)
	  && !DEBUG_INSN_P (insn)
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
cond_exec_find_if_block (struct ce_if_block * ce_info)
{
  basic_block test_bb = ce_info->test_bb;
  basic_block then_bb = ce_info->then_bb;
  basic_block else_bb = ce_info->else_bb;
  basic_block join_bb = NULL_BLOCK;
  edge cur_edge;
  basic_block next;
  edge_iterator ei;

  ce_info->last_test_bb = test_bb;

  /* We only ever should get here after reload,
     and if we have conditional execution.  */
  gcc_assert (reload_completed && targetm.have_conditional_execution ());

  /* Discover if any fall through predecessors of the current test basic block
     were && tests (which jump to the else block) or || tests (which jump to
     the then block).  */
  if (single_pred_p (test_bb)
      && single_pred_edge (test_bb)->flags == EDGE_FALLTHRU)
    {
      basic_block bb = single_pred (test_bb);
      basic_block target_bb;
      int max_insns = MAX_CONDITIONAL_EXECUTE;
      int n_insns;

      /* Determine if the preceding block is an && or || block.  */
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

	      if (!single_pred_p (bb))
		break;

	      bb = single_pred (bb);
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

  /* The THEN block of an IF-THEN combo must have exactly one predecessor,
     other than any || blocks which jump to the THEN block.  */
  if ((EDGE_COUNT (then_bb->preds) - ce_info->num_or_or_blocks) != 1)
    return FALSE;

  /* The edges of the THEN and ELSE blocks cannot have complex edges.  */
  FOR_EACH_EDGE (cur_edge, ei, then_bb->preds)
    {
      if (cur_edge->flags & EDGE_COMPLEX)
	return FALSE;
    }

  FOR_EACH_EDGE (cur_edge, ei, else_bb->preds)
    {
      if (cur_edge->flags & EDGE_COMPLEX)
	return FALSE;
    }

  /* The THEN block of an IF-THEN combo must have zero or one successors.  */
  if (EDGE_COUNT (then_bb->succs) > 0
      && (!single_succ_p (then_bb)
          || (single_succ_edge (then_bb)->flags & EDGE_COMPLEX)
	  || (epilogue_completed
	      && tablejump_p (BB_END (then_bb), NULL, NULL))))
    return FALSE;

  /* If the THEN block has no successors, conditional execution can still
     make a conditional call.  Don't do this unless the ELSE block has
     only one incoming edge -- the CFG manipulation is too ugly otherwise.
     Check for the last insn of the THEN block being an indirect jump, which
     is listed as not having any successors, but confuses the rest of the CE
     code processing.  ??? we should fix this in the future.  */
  if (EDGE_COUNT (then_bb->succs) == 0)
    {
      if (single_pred_p (else_bb) && else_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
	{
	  rtx_insn *last_insn = BB_END (then_bb);

	  while (last_insn
		 && NOTE_P (last_insn)
		 && last_insn != BB_HEAD (then_bb))
	    last_insn = PREV_INSN (last_insn);

	  if (last_insn
	      && JUMP_P (last_insn)
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
  else if (single_succ (then_bb) == else_bb)
    {
      join_bb = else_bb;
      else_bb = NULL_BLOCK;
    }

  /* If the THEN and ELSE block meet in a subsequent block, and the ELSE
     has exactly one predecessor and one successor, and the outgoing edge
     is not complex, then we have an IF-THEN-ELSE combo.  */
  else if (single_succ_p (else_bb)
	   && single_succ (then_bb) == single_succ (else_bb)
	   && single_pred_p (else_bb)
	   && !(single_succ_edge (else_bb)->flags & EDGE_COMPLEX)
	   && !(epilogue_completed
		&& tablejump_p (BB_END (else_bb), NULL, NULL)))
    join_bb = single_succ (else_bb);

  /* Otherwise it is not an IF-THEN or IF-THEN-ELSE combination.  */
  else
    return FALSE;

  num_possible_if_blocks++;

  if (dump_file)
    {
      fprintf (dump_file,
	       "\nIF-THEN%s block found, pass %d, start block %d "
	       "[insn %d], then %d [%d]",
	       (else_bb) ? "-ELSE" : "",
	       ce_info->pass,
	       test_bb->index,
	       BB_HEAD (test_bb) ? (int)INSN_UID (BB_HEAD (test_bb)) : -1,
	       then_bb->index,
	       BB_HEAD (then_bb) ? (int)INSN_UID (BB_HEAD (then_bb)) : -1);

      if (else_bb)
	fprintf (dump_file, ", else %d [%d]",
		 else_bb->index,
		 BB_HEAD (else_bb) ? (int)INSN_UID (BB_HEAD (else_bb)) : -1);

      fprintf (dump_file, ", join %d [%d]",
	       join_bb->index,
	       BB_HEAD (join_bb) ? (int)INSN_UID (BB_HEAD (join_bb)) : -1);

      if (ce_info->num_multiple_test_blocks > 0)
	fprintf (dump_file, ", %d %s block%s last test %d [%d]",
		 ce_info->num_multiple_test_blocks,
		 (ce_info->and_and_p) ? "&&" : "||",
		 (ce_info->num_multiple_test_blocks == 1) ? "" : "s",
		 ce_info->last_test_bb->index,
		 ((BB_HEAD (ce_info->last_test_bb))
		  ? (int)INSN_UID (BB_HEAD (ce_info->last_test_bb))
		  : -1));

      fputc ('\n', dump_file);
    }

  /* Make sure IF, THEN, and ELSE, blocks are adjacent.  Actually, we get the
     first condition for free, since we've already asserted that there's a
     fallthru edge from IF to THEN.  Likewise for the && and || blocks, since
     we checked the FALLTHRU flag, those are already adjacent to the last IF
     block.  */
  /* ??? As an enhancement, move the ELSE block.  Have to deal with
     BLOCK notes, if by no other means than backing out the merge if they
     exist.  Sticky enough I don't want to think about it now.  */
  next = then_bb;
  if (else_bb && (next = next->next_bb) != else_bb)
    return FALSE;
  if ((next = next->next_bb) != join_bb
      && join_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      if (else_bb)
	join_bb = NULL;
      else
	return FALSE;
    }

  /* Do the real work.  */

  ce_info->else_bb = else_bb;
  ce_info->join_bb = join_bb;

  /* If we have && and || tests, try to first handle combining the && and ||
     tests into the conditional code, and if that fails, go back and handle
     it without the && and ||, which at present handles the && case if there
     was no ELSE block.  */
  if (cond_exec_process_if_block (ce_info, TRUE))
    return TRUE;

  if (ce_info->num_multiple_test_blocks)
    {
      cancel_changes (0);

      if (cond_exec_process_if_block (ce_info, FALSE))
	return TRUE;
    }

  return FALSE;
}

/* Convert a branch over a trap, or a branch
   to a trap, into a conditional trap.  */

static int
find_cond_trap (basic_block test_bb, edge then_edge, edge else_edge)
{
  basic_block then_bb = then_edge->dest;
  basic_block else_bb = else_edge->dest;
  basic_block other_bb, trap_bb;
  rtx_insn *trap, *jump;
  rtx cond;
  rtx_insn *cond_earliest;
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

  if (dump_file)
    {
      fprintf (dump_file, "\nTRAP-IF block found, start %d, trap %d\n",
	       test_bb->index, trap_bb->index);
    }

  /* If this is not a standard conditional jump, we can't parse it.  */
  jump = BB_END (test_bb);
  cond = noce_get_condition (jump, &cond_earliest, false);
  if (! cond)
    return FALSE;

  /* If the conditional jump is more than just a conditional jump, then
     we can not do if-conversion on this block.  Give up for returnjump_p,
     changing a conditional return followed by unconditional trap for
     conditional trap followed by unconditional return is likely not
     beneficial and harder to handle.  */
  if (! onlyjump_p (jump) || returnjump_p (jump))
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
  rtx_insn *seq = gen_cond_trap (code, copy_rtx (XEXP (cond, 0)),
				 copy_rtx (XEXP (cond, 1)),
				 TRAP_CODE (PATTERN (trap)));
  if (seq == NULL)
    return FALSE;

  /* Emit the new insns before cond_earliest.  */
  emit_insn_before_setloc (seq, cond_earliest, INSN_LOCATION (trap));

  /* Delete the trap block if possible.  */
  remove_edge (trap_bb == then_bb ? then_edge : else_edge);
  df_set_bb_dirty (test_bb);
  df_set_bb_dirty (then_bb);
  df_set_bb_dirty (else_bb);

  if (EDGE_COUNT (trap_bb->preds) == 0)
    {
      delete_basic_block (trap_bb);
      num_true_changes++;
    }

  /* Wire together the blocks again.  */
  if (current_ir_type () == IR_RTL_CFGLAYOUT)
    single_succ_edge (test_bb)->flags |= EDGE_FALLTHRU;
  else if (trap_bb == then_bb)
    {
      rtx lab = JUMP_LABEL (jump);
      rtx_insn *seq = targetm.gen_jump (lab);
      rtx_jump_insn *newjump = emit_jump_insn_after (seq, jump);
      LABEL_NUSES (lab) += 1;
      JUMP_LABEL (newjump) = lab;
      emit_barrier_after (newjump);
    }
  delete_insn (jump);

  if (can_merge_blocks_p (test_bb, other_bb))
    {
      merge_blocks (test_bb, other_bb);
      num_true_changes++;
    }

  num_updated_if_blocks++;
  return TRUE;
}

/* Subroutine of find_cond_trap: if BB contains only a trap insn,
   return it.  */

static rtx_insn *
block_has_only_trap (basic_block bb)
{
  rtx_insn *trap;

  /* We're not the exit block.  */
  if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return NULL;

  /* The block must have no successors.  */
  if (EDGE_COUNT (bb->succs) > 0)
    return NULL;

  /* The only instruction in the THEN block must be the trap.  */
  trap = first_active_insn (bb);
  if (! (trap == BB_END (bb)
	 && GET_CODE (PATTERN (trap)) == TRAP_IF
         && TRAP_CONDITION (PATTERN (trap)) == const_true_rtx))
    return NULL;

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
find_if_case_1 (basic_block test_bb, edge then_edge, edge else_edge)
{
  basic_block then_bb = then_edge->dest;
  basic_block else_bb = else_edge->dest;
  basic_block new_bb;
  int then_bb_index, then_prob;
  rtx else_target = NULL_RTX;

  /* If we are partitioning hot/cold basic blocks, we don't want to
     mess up unconditional or indirect jumps that cross between hot
     and cold sections.

     Basic block partitioning may result in some jumps that appear to
     be optimizable (or blocks that appear to be mergeable), but which really
     must be left untouched (they are required to make it safely across
     partition boundaries).  See  the comments at the top of
     bb-reorder.c:partition_hot_cold_basic_blocks for complete details.  */

  if ((BB_END (then_bb)
       && JUMP_P (BB_END (then_bb))
       && CROSSING_JUMP_P (BB_END (then_bb)))
      || (BB_END (test_bb)
	  && JUMP_P (BB_END (test_bb))
	  && CROSSING_JUMP_P (BB_END (test_bb)))
      || (BB_END (else_bb)
	  && JUMP_P (BB_END (else_bb))
	  && CROSSING_JUMP_P (BB_END (else_bb))))
    return FALSE;

  /* THEN has one successor.  */
  if (!single_succ_p (then_bb))
    return FALSE;

  /* THEN does not fall through, but is not strange either.  */
  if (single_succ_edge (then_bb)->flags & (EDGE_COMPLEX | EDGE_FALLTHRU))
    return FALSE;

  /* THEN has one predecessor.  */
  if (!single_pred_p (then_bb))
    return FALSE;

  /* THEN must do something.  */
  if (forwarder_block_p (then_bb))
    return FALSE;

  num_possible_if_blocks++;
  if (dump_file)
    fprintf (dump_file,
	     "\nIF-CASE-1 found, start %d, then %d\n",
	     test_bb->index, then_bb->index);

  if (then_edge->probability)
    then_prob = REG_BR_PROB_BASE - then_edge->probability;
  else
    then_prob = REG_BR_PROB_BASE / 2;

  /* We're speculating from the THEN path, we want to make sure the cost
     of speculation is within reason.  */
  if (! cheap_bb_rtx_cost_p (then_bb, then_prob,
	COSTS_N_INSNS (BRANCH_COST (optimize_bb_for_speed_p (then_edge->src),
				    predictable_edge_p (then_edge)))))
    return FALSE;

  if (else_bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      rtx_insn *jump = BB_END (else_edge->src);
      gcc_assert (JUMP_P (jump));
      else_target = JUMP_LABEL (jump);
    }

  /* Registers set are dead, or are predicable.  */
  if (! dead_or_predicable (test_bb, then_bb, else_bb,
			    single_succ_edge (then_bb), 1))
    return FALSE;

  /* Conversion went ok, including moving the insns and fixing up the
     jump.  Adjust the CFG to match.  */

  /* We can avoid creating a new basic block if then_bb is immediately
     followed by else_bb, i.e. deleting then_bb allows test_bb to fall
     through to else_bb.  */

  if (then_bb->next_bb == else_bb
      && then_bb->prev_bb == test_bb
      && else_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      redirect_edge_succ (FALLTHRU_EDGE (test_bb), else_bb);
      new_bb = 0;
    }
  else if (else_bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    new_bb = force_nonfallthru_and_redirect (FALLTHRU_EDGE (test_bb),
					     else_bb, else_target);
  else
    new_bb = redirect_edge_and_branch_force (FALLTHRU_EDGE (test_bb),
					     else_bb);

  df_set_bb_dirty (test_bb);
  df_set_bb_dirty (else_bb);

  then_bb_index = then_bb->index;
  delete_basic_block (then_bb);

  /* Make rest of code believe that the newly created block is the THEN_BB
     block we removed.  */
  if (new_bb)
    {
      df_bb_replace (then_bb_index, new_bb);
      /* This should have been done above via force_nonfallthru_and_redirect
         (possibly called from redirect_edge_and_branch_force).  */
      gcc_checking_assert (BB_PARTITION (new_bb) == BB_PARTITION (test_bb));
    }

  num_true_changes++;
  num_updated_if_blocks++;
  return TRUE;
}

/* Test for case 2 above.  */

static int
find_if_case_2 (basic_block test_bb, edge then_edge, edge else_edge)
{
  basic_block then_bb = then_edge->dest;
  basic_block else_bb = else_edge->dest;
  edge else_succ;
  int then_prob, else_prob;

  /* We do not want to speculate (empty) loop latches.  */
  if (current_loops
      && else_bb->loop_father->latch == else_bb)
    return FALSE;

  /* If we are partitioning hot/cold basic blocks, we don't want to
     mess up unconditional or indirect jumps that cross between hot
     and cold sections.

     Basic block partitioning may result in some jumps that appear to
     be optimizable (or blocks that appear to be mergeable), but which really
     must be left untouched (they are required to make it safely across
     partition boundaries).  See  the comments at the top of
     bb-reorder.c:partition_hot_cold_basic_blocks for complete details.  */

  if ((BB_END (then_bb)
       && JUMP_P (BB_END (then_bb))
       && CROSSING_JUMP_P (BB_END (then_bb)))
      || (BB_END (test_bb)
	  && JUMP_P (BB_END (test_bb))
	  && CROSSING_JUMP_P (BB_END (test_bb)))
      || (BB_END (else_bb)
	  && JUMP_P (BB_END (else_bb))
	  && CROSSING_JUMP_P (BB_END (else_bb))))
    return FALSE;

  /* ELSE has one successor.  */
  if (!single_succ_p (else_bb))
    return FALSE;
  else
    else_succ = single_succ_edge (else_bb);

  /* ELSE outgoing edge is not complex.  */
  if (else_succ->flags & EDGE_COMPLEX)
    return FALSE;

  /* ELSE has one predecessor.  */
  if (!single_pred_p (else_bb))
    return FALSE;

  /* THEN is not EXIT.  */
  if (then_bb->index < NUM_FIXED_BLOCKS)
    return FALSE;

  if (else_edge->probability)
    {
      else_prob = else_edge->probability;
      then_prob = REG_BR_PROB_BASE - else_prob;
    }
  else
    {
      else_prob = REG_BR_PROB_BASE / 2;
      then_prob = REG_BR_PROB_BASE / 2;
    }

  /* ELSE is predicted or SUCC(ELSE) postdominates THEN.  */
  if (else_prob > then_prob)
    ;
  else if (else_succ->dest->index < NUM_FIXED_BLOCKS
	   || dominated_by_p (CDI_POST_DOMINATORS, then_bb,
			      else_succ->dest))
    ;
  else
    return FALSE;

  num_possible_if_blocks++;
  if (dump_file)
    fprintf (dump_file,
	     "\nIF-CASE-2 found, start %d, else %d\n",
	     test_bb->index, else_bb->index);

  /* We're speculating from the ELSE path, we want to make sure the cost
     of speculation is within reason.  */
  if (! cheap_bb_rtx_cost_p (else_bb, else_prob,
	COSTS_N_INSNS (BRANCH_COST (optimize_bb_for_speed_p (else_edge->src),
				    predictable_edge_p (else_edge)))))
    return FALSE;

  /* Registers set are dead, or are predicable.  */
  if (! dead_or_predicable (test_bb, else_bb, then_bb, else_succ, 0))
    return FALSE;

  /* Conversion went ok, including moving the insns and fixing up the
     jump.  Adjust the CFG to match.  */

  df_set_bb_dirty (test_bb);
  df_set_bb_dirty (then_bb);
  delete_basic_block (else_bb);

  num_true_changes++;
  num_updated_if_blocks++;

  /* ??? We may now fallthru from one of THEN's successors into a join
     block.  Rerun cleanup_cfg?  Examine things manually?  Wait?  */

  return TRUE;
}

/* Used by the code above to perform the actual rtl transformations.
   Return TRUE if successful.

   TEST_BB is the block containing the conditional branch.  MERGE_BB
   is the block containing the code to manipulate.  DEST_EDGE is an
   edge representing a jump to the join block; after the conversion,
   TEST_BB should be branching to its destination.
   REVERSEP is true if the sense of the branch should be reversed.  */

static int
dead_or_predicable (basic_block test_bb, basic_block merge_bb,
		    basic_block other_bb, edge dest_edge, int reversep)
{
  basic_block new_dest = dest_edge->dest;
  rtx_insn *head, *end, *jump;
  rtx_insn *earliest = NULL;
  rtx old_dest;
  bitmap merge_set = NULL;
  /* Number of pending changes.  */
  int n_validated_changes = 0;
  rtx new_dest_label = NULL_RTX;

  jump = BB_END (test_bb);

  /* Find the extent of the real code in the merge block.  */
  head = BB_HEAD (merge_bb);
  end = BB_END (merge_bb);

  while (DEBUG_INSN_P (end) && end != head)
    end = PREV_INSN (end);

  /* If merge_bb ends with a tablejump, predicating/moving insn's
     into test_bb and then deleting merge_bb will result in the jumptable
     that follows merge_bb being removed along with merge_bb and then we
     get an unresolved reference to the jumptable.  */
  if (tablejump_p (end, NULL, NULL))
    return FALSE;

  if (LABEL_P (head))
    head = NEXT_INSN (head);
  while (DEBUG_INSN_P (head) && head != end)
    head = NEXT_INSN (head);
  if (NOTE_P (head))
    {
      if (head == end)
	{
	  head = end = NULL;
	  goto no_body;
	}
      head = NEXT_INSN (head);
      while (DEBUG_INSN_P (head) && head != end)
	head = NEXT_INSN (head);
    }

  if (JUMP_P (end))
    {
      if (!onlyjump_p (end))
	return FALSE;
      if (head == end)
	{
	  head = end = NULL;
	  goto no_body;
	}
      end = PREV_INSN (end);
      while (DEBUG_INSN_P (end) && end != head)
	end = PREV_INSN (end);
    }

  /* Don't move frame-related insn across the conditional branch.  This
     can lead to one of the paths of the branch having wrong unwind info.  */
  if (epilogue_completed)
    {
      rtx_insn *insn = head;
      while (1)
	{
	  if (INSN_P (insn) && RTX_FRAME_RELATED_P (insn))
	    return FALSE;
	  if (insn == end)
	    break;
	  insn = NEXT_INSN (insn);
	}
    }

  /* Disable handling dead code by conditional execution if the machine needs
     to do anything funny with the tests, etc.  */
#ifndef IFCVT_MODIFY_TESTS
  if (targetm.have_conditional_execution ())
    {
      /* In the conditional execution case, we have things easy.  We know
	 the condition is reversible.  We don't have to check life info
	 because we're going to conditionally execute the code anyway.
	 All that's left is making sure the insns involved can actually
	 be predicated.  */

      rtx cond;

      cond = cond_exec_get_condition (jump);
      if (! cond)
	return FALSE;

      rtx note = find_reg_note (jump, REG_BR_PROB, NULL_RTX);
      int prob_val = (note ? XINT (note, 0) : -1);

      if (reversep)
	{
	  enum rtx_code rev = reversed_comparison_code (cond, jump);
	  if (rev == UNKNOWN)
	    return FALSE;
	  cond = gen_rtx_fmt_ee (rev, GET_MODE (cond), XEXP (cond, 0),
			         XEXP (cond, 1));
	  if (prob_val >= 0)
	    prob_val = REG_BR_PROB_BASE - prob_val;
	}

      if (cond_exec_process_insns (NULL, head, end, cond, prob_val, 0)
	  && verify_changes (0))
	n_validated_changes = num_validated_changes ();
      else
	cancel_changes (0);

      earliest = jump;
    }
#endif

  /* If we allocated new pseudos (e.g. in the conditional move
     expander called from noce_emit_cmove), we must resize the
     array first.  */
  if (max_regno < max_reg_num ())
    max_regno = max_reg_num ();

  /* Try the NCE path if the CE path did not result in any changes.  */
  if (n_validated_changes == 0)
    {
      rtx cond;
      rtx_insn *insn;
      regset live;
      bool success;

      /* In the non-conditional execution case, we have to verify that there
	 are no trapping operations, no calls, no references to memory, and
	 that any registers modified are dead at the branch site.  */

      if (!any_condjump_p (jump))
	return FALSE;

      /* Find the extent of the conditional.  */
      cond = noce_get_condition (jump, &earliest, false);
      if (!cond)
	return FALSE;

      live = BITMAP_ALLOC (&reg_obstack);
      simulate_backwards_to_point (merge_bb, live, end);
      success = can_move_insns_across (head, end, earliest, jump,
				       merge_bb, live,
				       df_get_live_in (other_bb), NULL);
      BITMAP_FREE (live);
      if (!success)
	return FALSE;

      /* Collect the set of registers set in MERGE_BB.  */
      merge_set = BITMAP_ALLOC (&reg_obstack);

      FOR_BB_INSNS (merge_bb, insn)
	if (NONDEBUG_INSN_P (insn))
	  df_simulate_find_defs (insn, merge_set);

      /* If shrink-wrapping, disable this optimization when test_bb is
	 the first basic block and merge_bb exits.  The idea is to not
	 move code setting up a return register as that may clobber a
	 register used to pass function parameters, which then must be
	 saved in caller-saved regs.  A caller-saved reg requires the
	 prologue, killing a shrink-wrap opportunity.  */
      if ((SHRINK_WRAPPING_ENABLED && !epilogue_completed)
	  && ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb == test_bb
	  && single_succ_p (new_dest)
	  && single_succ (new_dest) == EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && bitmap_intersect_p (df_get_live_in (new_dest), merge_set))
	{
	  regset return_regs;
	  unsigned int i;

	  return_regs = BITMAP_ALLOC (&reg_obstack);

	  /* Start off with the intersection of regs used to pass
	     params and regs used to return values.  */
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (FUNCTION_ARG_REGNO_P (i)
		&& targetm.calls.function_value_regno_p (i))
	      bitmap_set_bit (return_regs, INCOMING_REGNO (i));

	  bitmap_and_into (return_regs,
			   df_get_live_out (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
	  bitmap_and_into (return_regs,
			   df_get_live_in (EXIT_BLOCK_PTR_FOR_FN (cfun)));
	  if (!bitmap_empty_p (return_regs))
	    {
	      FOR_BB_INSNS_REVERSE (new_dest, insn)
		if (NONDEBUG_INSN_P (insn))
		  {
		    df_ref def;

		    /* If this insn sets any reg in return_regs, add all
		       reg uses to the set of regs we're interested in.  */
		    FOR_EACH_INSN_DEF (def, insn)
		      if (bitmap_bit_p (return_regs, DF_REF_REGNO (def)))
			{
			  df_simulate_uses (insn, return_regs);
			  break;
			}
		  }
	      if (bitmap_intersect_p (merge_set, return_regs))
		{
		  BITMAP_FREE (return_regs);
		  BITMAP_FREE (merge_set);
		  return FALSE;
		}
	    }
	  BITMAP_FREE (return_regs);
	}
    }

 no_body:
  /* We don't want to use normal invert_jump or redirect_jump because
     we don't want to delete_insn called.  Also, we want to do our own
     change group management.  */

  old_dest = JUMP_LABEL (jump);
  if (other_bb != new_dest)
    {
      if (!any_condjump_p (jump))
	goto cancel;

      if (JUMP_P (BB_END (dest_edge->src)))
	new_dest_label = JUMP_LABEL (BB_END (dest_edge->src));
      else if (new_dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	new_dest_label = ret_rtx;
      else
	new_dest_label = block_label (new_dest);

      rtx_jump_insn *jump_insn = as_a <rtx_jump_insn *> (jump);
      if (reversep
	  ? ! invert_jump_1 (jump_insn, new_dest_label)
	  : ! redirect_jump_1 (jump_insn, new_dest_label))
	goto cancel;
    }

  if (verify_changes (n_validated_changes))
    confirm_change_group ();
  else
    goto cancel;

  if (other_bb != new_dest)
    {
      redirect_jump_2 (as_a <rtx_jump_insn *> (jump), old_dest, new_dest_label,
		       0, reversep);

      redirect_edge_succ (BRANCH_EDGE (test_bb), new_dest);
      if (reversep)
	{
	  std::swap (BRANCH_EDGE (test_bb)->count,
		     FALLTHRU_EDGE (test_bb)->count);
	  std::swap (BRANCH_EDGE (test_bb)->probability,
		     FALLTHRU_EDGE (test_bb)->probability);
	  update_br_prob_note (test_bb);
	}
    }

  /* Move the insns out of MERGE_BB to before the branch.  */
  if (head != NULL)
    {
      rtx_insn *insn;

      if (end == BB_END (merge_bb))
	BB_END (merge_bb) = PREV_INSN (head);

      /* PR 21767: when moving insns above a conditional branch, the REG_EQUAL
	 notes being moved might become invalid.  */
      insn = head;
      do
	{
	  rtx note;

	  if (! INSN_P (insn))
	    continue;
	  note = find_reg_note (insn, REG_EQUAL, NULL_RTX);
	  if (! note)
	    continue;
	  remove_note (insn, note);
	} while (insn != end && (insn = NEXT_INSN (insn)));

      /* PR46315: when moving insns above a conditional branch, the REG_EQUAL
	 notes referring to the registers being set might become invalid.  */
      if (merge_set)
	{
	  unsigned i;
	  bitmap_iterator bi;

	  EXECUTE_IF_SET_IN_BITMAP (merge_set, 0, i, bi)
	    remove_reg_equal_equiv_notes_for_regno (i);

	  BITMAP_FREE (merge_set);
	}

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

  if (merge_set)
    BITMAP_FREE (merge_set);

  return FALSE;
}

/* Main entry point for all if-conversion.  AFTER_COMBINE is true if
   we are after combine pass.  */

static void
if_convert (bool after_combine)
{
  basic_block bb;
  int pass;

  if (optimize == 1)
    {
      df_live_add_problem ();
      df_live_set_all_dirty ();
    }

  /* Record whether we are after combine pass.  */
  ifcvt_after_combine = after_combine;
  have_cbranchcc4 = (direct_optab_handler (cbranch_optab, CCmode)
		     != CODE_FOR_nothing);
  num_possible_if_blocks = 0;
  num_updated_if_blocks = 0;
  num_true_changes = 0;

  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);
  mark_loop_exit_edges ();
  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);

  /* Compute postdominators.  */
  calculate_dominance_info (CDI_POST_DOMINATORS);

  df_set_flags (DF_LR_RUN_DCE);

  /* Go through each of the basic blocks looking for things to convert.  If we
     have conditional execution, we make multiple passes to allow us to handle
     IF-THEN{-ELSE} blocks within other IF-THEN{-ELSE} blocks.  */
  pass = 0;
  do
    {
      df_analyze ();
      /* Only need to do dce on the first pass.  */
      df_clear_flags (DF_LR_RUN_DCE);
      cond_exec_changed_p = FALSE;
      pass++;

#ifdef IFCVT_MULTIPLE_DUMPS
      if (dump_file && pass > 1)
	fprintf (dump_file, "\n\n========== Pass %d ==========\n", pass);
#endif

      FOR_EACH_BB_FN (bb, cfun)
	{
          basic_block new_bb;
          while (!df_get_bb_dirty (bb)
                 && (new_bb = find_if_header (bb, pass)) != NULL)
            bb = new_bb;
	}

#ifdef IFCVT_MULTIPLE_DUMPS
      if (dump_file && cond_exec_changed_p)
	print_rtl_with_bb (dump_file, get_insns (), dump_flags);
#endif
    }
  while (cond_exec_changed_p);

#ifdef IFCVT_MULTIPLE_DUMPS
  if (dump_file)
    fprintf (dump_file, "\n\n========== no more changes\n");
#endif

  free_dominance_info (CDI_POST_DOMINATORS);

  if (dump_file)
    fflush (dump_file);

  clear_aux_for_blocks ();

  /* If we allocated new pseudos, we must resize the array for sched1.  */
  if (max_regno < max_reg_num ())
    max_regno = max_reg_num ();

  /* Write the final stats.  */
  if (dump_file && num_possible_if_blocks > 0)
    {
      fprintf (dump_file,
	       "\n%d possible IF blocks searched.\n",
	       num_possible_if_blocks);
      fprintf (dump_file,
	       "%d IF blocks converted.\n",
	       num_updated_if_blocks);
      fprintf (dump_file,
	       "%d true changes made.\n\n\n",
	       num_true_changes);
    }

  if (optimize == 1)
    df_remove_problem (df_live);

  checking_verify_flow_info ();
}

/* If-conversion and CFG cleanup.  */
static unsigned int
rest_of_handle_if_conversion (void)
{
  if (flag_if_conversion)
    {
      if (dump_file)
	{
	  dump_reg_info (dump_file);
	  dump_flow_info (dump_file, dump_flags);
	}
      cleanup_cfg (CLEANUP_EXPENSIVE);
      if_convert (false);
    }

  cleanup_cfg (0);
  return 0;
}

namespace {

const pass_data pass_data_rtl_ifcvt =
{
  RTL_PASS, /* type */
  "ce1", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IFCVT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_rtl_ifcvt : public rtl_opt_pass
{
public:
  pass_rtl_ifcvt (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_ifcvt, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (optimize > 0) && dbg_cnt (if_conversion);
    }

  virtual unsigned int execute (function *)
    {
      return rest_of_handle_if_conversion ();
    }

}; // class pass_rtl_ifcvt

} // anon namespace

rtl_opt_pass *
make_pass_rtl_ifcvt (gcc::context *ctxt)
{
  return new pass_rtl_ifcvt (ctxt);
}


/* Rerun if-conversion, as combine may have simplified things enough
   to now meet sequence length restrictions.  */

namespace {

const pass_data pass_data_if_after_combine =
{
  RTL_PASS, /* type */
  "ce2", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IFCVT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_if_after_combine : public rtl_opt_pass
{
public:
  pass_if_after_combine (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_if_after_combine, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return optimize > 0 && flag_if_conversion
	&& dbg_cnt (if_after_combine);
    }

  virtual unsigned int execute (function *)
    {
      if_convert (true);
      return 0;
    }

}; // class pass_if_after_combine

} // anon namespace

rtl_opt_pass *
make_pass_if_after_combine (gcc::context *ctxt)
{
  return new pass_if_after_combine (ctxt);
}


namespace {

const pass_data pass_data_if_after_reload =
{
  RTL_PASS, /* type */
  "ce3", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IFCVT2, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_if_after_reload : public rtl_opt_pass
{
public:
  pass_if_after_reload (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_if_after_reload, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return optimize > 0 && flag_if_conversion2
	&& dbg_cnt (if_after_reload);
    }

  virtual unsigned int execute (function *)
    {
      if_convert (true);
      return 0;
    }

}; // class pass_if_after_reload

} // anon namespace

rtl_opt_pass *
make_pass_if_after_reload (gcc::context *ctxt)
{
  return new pass_if_after_reload (ctxt);
}
