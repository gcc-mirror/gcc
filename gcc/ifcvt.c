/* If-conversion support.
   Copyright (C) 2000 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"

#include "rtl.h"
#include "regs.h"
#include "function.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "basic-block.h"
#include "expr.h"
#include "output.h"
#include "hard-reg-set.h"
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

#ifndef MAX_CONDITIONAL_EXECUTE
#define MAX_CONDITIONAL_EXECUTE   (BRANCH_COST + 1)
#endif

#define EDGE_COMPLEX	(EDGE_ABNORMAL | EDGE_ABNORMAL_CALL | EDGE_EH)

#define NULL_EDGE	((struct edge_def *)NULL)
#define NULL_BLOCK	((struct basic_block_def *)NULL)

/* # of IF-THEN or IF-THEN-ELSE blocks we looked at  */
static int num_possible_if_blocks;

/* # of IF-THEN or IF-THEN-ELSE blocks were converted to conditional
   execution.  */
static int num_updated_if_blocks;

/* # of basic blocks that were removed.  */
static int num_removed_blocks;

/* The post-dominator relation on the original block numbers.  */
static sbitmap *post_dominators;

/* Forward references.  */
static int count_bb_insns		PARAMS ((basic_block));
static rtx first_active_insn		PARAMS ((basic_block));
static int last_active_insn_p		PARAMS ((basic_block, rtx));

static int cond_exec_process_insns	PARAMS ((rtx, rtx, rtx, int));
static rtx cond_exec_get_condition	PARAMS ((rtx));
static int cond_exec_process_if_block	PARAMS ((basic_block, basic_block,
						 basic_block, basic_block));

static rtx noce_get_condition		PARAMS ((rtx, rtx *));
static int noce_process_if_block	PARAMS ((basic_block, basic_block,
						 basic_block, basic_block));

static int process_if_block		PARAMS ((basic_block, basic_block,
						 basic_block, basic_block));
static void merge_if_block		PARAMS ((basic_block, basic_block,
						 basic_block, basic_block));

static int find_if_header		PARAMS ((basic_block));
static int find_if_block		PARAMS ((basic_block, edge, edge));
static int find_if_case_1		PARAMS ((basic_block, edge, edge));
static int find_if_case_2		PARAMS ((basic_block, edge, edge));
static int find_memory			PARAMS ((rtx *, void *));
static int dead_or_predicable		PARAMS ((basic_block, basic_block,
						 basic_block, rtx, int));

/* Abuse the basic_block AUX field to store the original block index,
   as well as a flag indicating that the block should be rescaned for
   life analysis.  */

#define SET_ORIG_INDEX(BB,I)	((BB)->aux = (void *)((size_t)(I) << 1))
#define ORIG_INDEX(BB)		((size_t)(BB)->aux >> 1)
#define SET_UPDATE_LIFE(BB)	((BB)->aux = (void *)((size_t)(BB)->aux | 1))
#define UPDATE_LIFE(BB)		((size_t)(BB)->aux & 1)


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

/* Return true if INSN is the last active non-jump insn in BB.  */

static int
last_active_insn_p (bb, insn)
     basic_block bb;
     rtx insn;
{
  do
    {
      if (insn == bb->end)
	return TRUE;
      insn = NEXT_INSN (insn);
    }
  while (GET_CODE (insn) == NOTE);

  return GET_CODE (insn) == JUMP_INSN;
}

/* Go through a bunch of insns, converting them to conditional
   execution format if possible.  Return TRUE if all of the non-note
   insns were processed.  */

static int
cond_exec_process_insns (start, end, test, mod_ok)
     rtx start;			/* first insn to look at */
     rtx end;			/* last insn to look at */
     rtx test;			/* conditional execution test */
     int mod_ok;		/* true if modifications ok last insn.  */
{
  int must_be_last = FALSE;
  rtx insn;

  for (insn = start; ; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE)
	goto insn_done;

      if (GET_CODE (insn) != INSN && GET_CODE (insn) != CALL_INSN)
	abort ();

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
      validate_change (insn, &PATTERN (insn),
		       gen_rtx_COND_EXEC (VOIDmode, copy_rtx (test),
					  PATTERN (insn)), 1);

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

  if (condjump_p (jump))
    test_if = SET_SRC (PATTERN (jump));
  else if (condjump_in_parallel_p (jump))
    test_if = SET_SRC (XVECEXP (PATTERN (jump), 0, 0));
  else
    return NULL_RTX;
  cond = XEXP (test_if, 0);

  /* If this branches to JUMP_LABEL when the condition is false,
     reverse the condition.  */
  if (GET_CODE (XEXP (test_if, 2)) == LABEL_REF
      && XEXP (XEXP (test_if, 2), 0) == JUMP_LABEL (jump))
    cond = gen_rtx_fmt_ee (reverse_condition (GET_CODE (cond)),
			   GET_MODE (cond), XEXP (cond, 0),
			   XEXP (cond, 1));

  return cond;
}

/* Given a simple IF-THEN or IF-THEN-ELSE block, attempt to convert it
   to conditional execution.  Return TRUE if we were successful at
   converting the the block.  */

static int
cond_exec_process_if_block (test_bb, then_bb, else_bb, join_bb)
     basic_block test_bb;	/* Basic block test is in */
     basic_block then_bb;	/* Basic block for THEN block */
     basic_block else_bb;	/* Basic block for ELSE block */
     basic_block join_bb;	/* Basic block the join label is in */
{
  rtx test_expr;		/* expression in IF_THEN_ELSE that is tested */
  rtx then_start;		/* first insn in THEN block */
  rtx then_end;			/* last insn + 1 in THEN block */
  rtx else_start;		/* first insn in ELSE block or NULL */
  rtx else_end;			/* last insn + 1 in ELSE block */
  int max;			/* max # of insns to convert. */
  int then_mod_ok;		/* whether conditional mods are ok in THEN */
  rtx true_expr;		/* test for else block insns */
  rtx false_expr;		/* test for then block insns */
  int n_insns;

  /* Find the conditional jump to the ELSE or JOIN part, and isolate
     the test.  */
  test_expr = cond_exec_get_condition (test_bb->end);
  if (! test_expr)
    return FALSE;

  /* Collect the bounds of where we're to search.  */

  then_start = then_bb->head;
  then_end = then_bb->end;

  /* Skip a (use (const_int 0)) or branch as the final insn.  */
  if (GET_CODE (then_end) == INSN
      && GET_CODE (PATTERN (then_end)) == USE
      && GET_CODE (XEXP (PATTERN (then_end), 0)) == CONST_INT)
    then_end = PREV_INSN (then_end);
  else if (GET_CODE (then_end) == JUMP_INSN)
    then_end = PREV_INSN (then_end);

  if (else_bb)
    {
      /* Skip the ELSE block's label.  */
      else_start = NEXT_INSN (else_bb->head);
      else_end = else_bb->end;

      /* Skip a (use (const_int 0)) or branch as the final insn.  */
      if (GET_CODE (else_end) == INSN
	  && GET_CODE (PATTERN (else_end)) == USE
	  && GET_CODE (XEXP (PATTERN (else_end), 0)) == CONST_INT)
	else_end = PREV_INSN (else_end);
      else if (GET_CODE (else_end) == JUMP_INSN)
	else_end = PREV_INSN (else_end);
    }

  /* How many instructions should we convert in total?  */
  n_insns = 0;
  if (else_bb)
    {
      max = 2 * MAX_CONDITIONAL_EXECUTE;
      n_insns = count_bb_insns (else_bb);
    }
  else
    max = MAX_CONDITIONAL_EXECUTE;
  n_insns += count_bb_insns (then_bb);
  if (n_insns > max)
    return FALSE;

  /* Map test_expr/test_jump into the appropriate MD tests to use on
     the conditionally executed code.  */
  
  true_expr = test_expr;
  false_expr = gen_rtx_fmt_ee (reverse_condition (GET_CODE (true_expr)),
			       GET_MODE (true_expr), XEXP (true_expr, 0),
			       XEXP (true_expr, 1));

  /* For IF-THEN-ELSE blocks, we don't allow modifications of the test
     on then THEN block.  */
  then_mod_ok = (else_bb == NULL_BLOCK);

  /* Go through the THEN and ELSE blocks converting the insns if possible
     to conditional execution.  */

  if (then_end
      && ! cond_exec_process_insns (then_start, then_end,
				    false_expr, then_mod_ok))
    goto fail;

  if (else_bb
      && ! cond_exec_process_insns (else_start, else_end,
				    true_expr, TRUE))
    goto fail;

  if (! apply_change_group ())
    return FALSE;

  /* Conversion succeeded.  */
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "%d insn%s converted to conditional execution.\n",
	     n_insns, (n_insns == 1) ? " was" : "s were");

  /* Merge the blocks!  */
  merge_if_block (test_bb, then_bb, else_bb, join_bb);
  return TRUE;

 fail:
  cancel_changes (0);
  return FALSE;
}

/* Used by noce_process_if_block to communicate with its subroutines. 

   The subroutines know that A and B may be evaluated freely.  They
   know that X is a register.  They should insert new instructions 
   before cond_earliest.  */

struct noce_if_info
{
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
    cond = XEXP (SET_SRC (PATTERN (if_info->jump)), 0);

  if ((if_info->cond_earliest == if_info->jump || cond_complex)
      && (normalize == 0 || STORE_FLAG_VALUE == normalize))
    {
      rtx tmp;

      code = GET_CODE (cond);
      if (reversep)
	code = reverse_condition (code);

      tmp = gen_rtx_fmt_ee (code, GET_MODE (x), XEXP (cond, 0),
			    XEXP (cond, 1));
      tmp = gen_rtx_SET (VOIDmode, x, tmp);

      start_sequence ();
      tmp = emit_insn (tmp);

      if (recog_memoized (tmp) >= 0)
	{
	  tmp = get_insns ();
	  end_sequence ();
	  emit_insns (tmp);

	  if_info->cond_earliest = if_info->jump;

	  return x;
	}

      end_sequence ();
    }

  /* Don't even try if the comparison operands are weird.  */
  if (cond_complex)
    return NULL_RTX;

  code = GET_CODE (cond);
  if (reversep)
    code = reverse_condition (code);

  return emit_store_flag (x, code, XEXP (cond, 0),
			  XEXP (cond, 1), VOIDmode,
			  (code == LTU || code == LEU
			   || code == GEU || code == GTU), normalize);
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
	   && can_reverse_comparison_p (if_info->cond, if_info->jump))
    reversep = 1;
  else
    return FALSE;

  start_sequence ();

  target = noce_emit_store_flag (if_info, if_info->x, reversep, 0);
  if (target)
    {
      if (target != if_info->x)
	emit_move_insn (if_info->x, target);

      seq = get_insns ();
      end_sequence ();
      emit_insns_before (seq, if_info->cond_earliest);

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

  if (! no_new_pseudos
      && GET_CODE (if_info->a) == CONST_INT
      && GET_CODE (if_info->b) == CONST_INT)
    {
      ifalse = INTVAL (if_info->a);
      itrue = INTVAL (if_info->b);
      diff = itrue - ifalse;

      can_reverse = can_reverse_comparison_p (if_info->cond, if_info->jump);

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
	  diff = -diff;
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
	  target = expand_binop (GET_MODE (if_info->x),
				 (diff == STORE_FLAG_VALUE
				  ? add_optab : sub_optab),
				 GEN_INT (ifalse), target, if_info->x, 0,
				 OPTAB_WIDEN);
	}

      /* if (test) x = 8; else x = 0;
	 =>   x = (test != 0) << 3;  */
      else if (ifalse == 0 && (tmp = exact_log2 (itrue)) >= 0)
	{
	  target = expand_binop (GET_MODE (if_info->x), ashl_optab,
				 target, GEN_INT (tmp), if_info->x, 0,
				 OPTAB_WIDEN);
	}

      /* if (test) x = -1; else x = b;
	 =>   x = -(test != 0) | b;  */
      else if (itrue == -1)
	{
	  target = expand_binop (GET_MODE (if_info->x), ior_optab,
				 target, GEN_INT (ifalse), if_info->x, 0,
				 OPTAB_WIDEN);
	}

      /* if (test) x = a; else x = b;
	 =>   x = (-(test != 0) & (b - a)) + a;  */
      else
	{
	  target = expand_binop (GET_MODE (if_info->x), and_optab,
				 target, GEN_INT (diff), if_info->x, 0,
				 OPTAB_WIDEN);
	  if (target)
	    target = expand_binop (GET_MODE (if_info->x), add_optab,
				   target, GEN_INT (ifalse), if_info->x, 0,
				   OPTAB_WIDEN);
	}

      if (! target)
	{
	  end_sequence ();
	  return FALSE;
	}

      if (target != if_info->x)
	emit_move_insn (if_info->x, target);

      seq = get_insns ();
      end_sequence ();
      emit_insns_before (seq, if_info->cond_earliest);

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
      && can_reverse_comparison_p (if_info->cond, if_info->jump))
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
	target = expand_binop (GET_MODE (if_info->x),
			       subtract ? sub_optab : add_optab,
			       if_info->x, target, if_info->x, 0, OPTAB_WIDEN);
      if (target)
	{
	  if (target != if_info->x)
	    emit_move_insn (if_info->x, target);

	  seq = get_insns ();
	  end_sequence ();
	  emit_insns_before (seq, if_info->cond_earliest);

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
	  || ((reversep = can_reverse_comparison_p (if_info->cond,
						    if_info->jump))
	      && if_info->b == const0_rtx
	      && rtx_equal_p (if_info->a, if_info->x))))
    {
      start_sequence ();
      target = noce_emit_store_flag (if_info,
				     gen_reg_rtx (GET_MODE (if_info->x)),
				     reversep, -1);
      if (target)
        target = expand_binop (GET_MODE (if_info->x), and_optab,
			       if_info->x, target, if_info->x, 0,
			       OPTAB_WIDEN);

      if (target)
	{
	  if (target != if_info->x)
	    emit_move_insn (if_info->x, target);

	  seq = get_insns ();
	  end_sequence ();
	  emit_insns_before (seq, if_info->cond_earliest);

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
	  emit_insns (tmp);

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
	    emit_move_insn (if_info->x, target);

	  seq = get_insns ();
	  end_sequence ();
	  emit_insns_before (seq, if_info->cond_earliest);
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
     not fault.  This is true of stack memories or if we've already loaded
     from the address along the path from ENTRY.  */
  else if (GET_CODE (a) == MEM || GET_CODE (b) == MEM)
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
  if (can_reverse_comparison_p (if_info->cond, if_info->jump))
    {
      int reversep = 0;
      if (rtx_equal_p (b, x))
	reversep = 1;
      else if (general_operand (b, GET_MODE (b)))
	reversep = 1;

      if (reversep)
	{
	  code = reverse_condition (code);
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
      else if (! insn_b)
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
	MEM_ALIAS_SET (tmp) = MEM_ALIAS_SET (if_info->a);

      emit_move_insn (if_info->x, tmp);
    }
  else if (target != x)
    emit_move_insn (x, target);

  tmp = get_insns ();
  end_sequence ();
  emit_insns_before (tmp, if_info->cond_earliest);
  return TRUE;

 end_seq_and_fail:
  end_sequence ();
  return FALSE;
}

/* Look for the condition for the jump first.  We'd prefer to avoid
   get_condition if we can -- it tries to look back for the contents
   of an original compare.  On targets that use normal integers for
   comparisons, e.g. alpha, this is wasteful.  */

static rtx
noce_get_condition (jump, earliest)
     rtx jump;
     rtx *earliest;
{
  rtx cond;

  /* If the condition variable is a register and is MODE_INT, accept it.
     Otherwise, fall back on get_condition.  */

  if (! condjump_p (jump))
    return NULL_RTX;

  cond = XEXP (SET_SRC (PATTERN (jump)), 0);
  if (GET_CODE (XEXP (cond, 0)) == REG
      && GET_MODE_CLASS (GET_MODE (XEXP (cond, 0))) == MODE_INT)
    {
      *earliest = jump;

      /* If this branches to JUMP_LABEL when the condition is false,
	 reverse the condition.  */
      if (GET_CODE (XEXP (SET_SRC (PATTERN (jump)), 2)) == LABEL_REF
	  && XEXP (XEXP (SET_SRC (PATTERN (jump)), 2), 0) == JUMP_LABEL (jump))
	cond = gen_rtx_fmt_ee (reverse_condition (GET_CODE (cond)),
			       GET_MODE (cond), XEXP (cond, 0),
			       XEXP (cond, 1));
    }
  else
    cond = get_condition (jump, earliest);

  return cond;
}

/* Given a simple IF-THEN or IF-THEN-ELSE block, attempt to convert it
   without using conditional execution.  Return TRUE if we were
   successful at converting the the block.  */

static int
noce_process_if_block (test_bb, then_bb, else_bb, join_bb)
     basic_block test_bb;	/* Basic block test is in */
     basic_block then_bb;	/* Basic block for THEN block */
     basic_block else_bb;	/* Basic block for ELSE block */
     basic_block join_bb;	/* Basic block the join label is in */
{
  /* We're looking for patterns of the form

     (1) if (...) x = a; else x = b;
     (2) x = b; if (...) x = a;
     (3) if (...) x = a;   // as if with an initial x = x.

     The later patterns require jumps to be more expensive.

     ??? For future expansion, look for multiple X in such patterns.  */

  struct noce_if_info if_info;
  rtx insn_a, insn_b;
  rtx set_a, set_b;
  rtx orig_x, x, a, b;
  rtx jump, cond, insn;

  /* If this is not a standard conditional jump, we can't parse it.  */
  jump = test_bb->end;
  cond = noce_get_condition (jump, &if_info.cond_earliest);
  if (! cond)
    return FALSE;

  /* We must be comparing objects whose modes imply the size.  */
  if (GET_MODE (XEXP (cond, 0)) == BLKmode)
    return FALSE;

  /* Look for one of the potential sets.  */
  insn_a = first_active_insn (then_bb);
  if (! insn_a
      || ! last_active_insn_p (then_bb, insn_a)
      || (set_a = single_set (insn_a)) == NULL_RTX)
    return FALSE;

  x = SET_DEST (set_a);
  a = SET_SRC (set_a);

  /* X may not be mentioned between cond_earliest and the jump.  */
  for (insn = jump; insn != if_info.cond_earliest; insn = PREV_INSN (insn))
    if (INSN_P (insn) && reg_mentioned_p (x, insn))
      return FALSE;

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
	  || ! last_active_insn_p (else_bb, insn_b)
	  || (set_b = single_set (insn_b)) == NULL_RTX
	  || ! rtx_equal_p (x, SET_DEST (set_b)))
	return FALSE;
    }
  else
    {
      insn_b = prev_nonnote_insn (if_info.cond_earliest);
      if (! insn_b
	  || GET_CODE (insn_b) != INSN
	  || (set_b = single_set (insn_b)) == NULL_RTX
	  || ! rtx_equal_p (x, SET_DEST (set_b))
	  || reg_mentioned_p (x, cond))
	insn_b = set_b = NULL_RTX;
    }
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
      x = gen_reg_rtx (GET_MODE (x));
    }

  /* Don't operate on sources that may trap or are volatile.  */
  if (side_effects_p (a) || side_effects_p (b)
      || (GET_CODE (a) != MEM && may_trap_p (a))
      || (GET_CODE (b) != MEM && may_trap_p (b)))
    return FALSE;

  /* Set up the info block for our subroutines.  */
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
	  if (else_bb && insn_b == else_bb->end)
	    else_bb->end = PREV_INSN (insn_b);
	  reorder_insns (insn_b, insn_b, PREV_INSN (if_info.cond_earliest));
	  insn_b = NULL_RTX;
	  x = orig_x;
	}
      goto success;
    }

  if (noce_try_store_flag (&if_info))
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
  if (insn_a == then_bb->end)
    then_bb->end = PREV_INSN (insn_a);
  flow_delete_insn (insn_a);

  /* Several special cases here: First, we may have reused insn_b above,
     in which case insn_b is now NULL.  Second, we want to delete insn_b
     if it came from the ELSE block, because follows the now correct
     write that appears in the TEST block.  However, if we got insn_b from
     the TEST block, it may in fact be loading data needed for the comparison.
     We'll let life_analysis remove the insn if it's really dead.  */
  if (insn_b && else_bb)
    {
      if (insn_b == else_bb->end)
	else_bb->end = PREV_INSN (insn_b);
      flow_delete_insn (insn_b);
    }

  /* The new insns will have been inserted before cond_earliest.  We should
     be able to remove the jump with impunity, but the condition itself may
     have been modified by gcse to be shared across basic blocks.  */
  test_bb->end = PREV_INSN (jump);
  flow_delete_insn (jump);

  /* If we used a temporary, fix it up now.  */
  if (orig_x != x)
    {
      start_sequence ();
      emit_move_insn (orig_x, x);
      insn_b = gen_sequence ();
      end_sequence ();

      test_bb->end = emit_insn_after (insn_b, test_bb->end);
    }

  /* Merge the blocks!  */
  merge_if_block (test_bb, then_bb, else_bb, join_bb);

  return TRUE;
}

/* Attempt to convert an IF-THEN or IF-THEN-ELSE block into
   straight line code.  Return true if successful.  */

static int
process_if_block (test_bb, then_bb, else_bb, join_bb)
     basic_block test_bb;	/* Basic block test is in */
     basic_block then_bb;	/* Basic block for THEN block */
     basic_block else_bb;	/* Basic block for ELSE block */
     basic_block join_bb;	/* Basic block the join label is in */
{
  if (! reload_completed
      && noce_process_if_block (test_bb, then_bb, else_bb, join_bb))
    return TRUE;

  if (HAVE_conditional_execution
      && reload_completed
      && cond_exec_process_if_block (test_bb, then_bb, else_bb, join_bb))
    return TRUE;

  return FALSE;
}

/* Merge the blocks and mark for local life update.  */

static void
merge_if_block (test_bb, then_bb, else_bb, join_bb)
     basic_block test_bb;	/* Basic block test is in */
     basic_block then_bb;	/* Basic block for THEN block */
     basic_block else_bb;	/* Basic block for ELSE block */
     basic_block join_bb;	/* Basic block the join label is in */
{
  basic_block combo_bb;

  /* All block merging is done into the lower block numbers.  */

  combo_bb = test_bb;

  /* First merge TEST block into THEN block.  This is a no-brainer since
     the THEN block did not have a code label to begin with.  */

  if (combo_bb->global_live_at_end)
    COPY_REG_SET (combo_bb->global_live_at_end, then_bb->global_live_at_end);
  merge_blocks_nomove (combo_bb, then_bb);
  num_removed_blocks++;

  /* The ELSE block, if it existed, had a label.  That label count
     will almost always be zero, but odd things can happen when labels
     get their addresses taken.  */
  if (else_bb)
    {
      if (LABEL_NUSES (else_bb->head) == 0
	  && ! LABEL_PRESERVE_P (else_bb->head)
	  && ! LABEL_NAME (else_bb->head))
	{
	  /* We can merge the ELSE.  */
	  merge_blocks_nomove (combo_bb, else_bb);
	  num_removed_blocks++;
	}
      else
	{
	  /* We cannot merge the ELSE.  */

	  /* Properly rewire the edge out of the now combined
	     TEST-THEN block to point here.  */
	  remove_edge (combo_bb->succ);
	  if (combo_bb->succ || else_bb->pred)
	    abort ();
	  make_edge (NULL, combo_bb, else_bb, EDGE_FALLTHRU);

	  /* Remove the jump and cruft from the end of the TEST-THEN block.  */
	  tidy_fallthru_edge (combo_bb->succ, combo_bb, else_bb);

	  /* Make sure we update life info properly.  */
	  SET_UPDATE_LIFE(combo_bb);
	  if (else_bb->global_live_at_end)
	    COPY_REG_SET (else_bb->global_live_at_start,
			  else_bb->global_live_at_end);

	  /* The ELSE is the new combo block.  */
	  combo_bb = else_bb;
	}
    }

  /* If there was no join block reported, that means it was not adjacent
     to the others, and so we cannot merge them.  */

  if (! join_bb)
    {
      /* The outgoing edge for the current COMBO block should already
	 be correct.  Verify this.  */
      if (combo_bb->succ == NULL_EDGE)
	abort ();

      /* There should sill be a branch at the end of the THEN or ELSE
         blocks taking us to our final destination.  */
      if (! simplejump_p (combo_bb->end)
          && ! returnjump_p (combo_bb->end))
	abort ();
    }

  /* The JOIN block had a label.  It may have had quite a number
     of other predecessors too, but probably not.  See if we can
     merge this with the others.  */
  else if (LABEL_NUSES (join_bb->head) == 0
      && ! LABEL_PRESERVE_P (join_bb->head)
      && ! LABEL_NAME (join_bb->head))
    {
      /* We can merge the JOIN.  */
      if (combo_bb->global_live_at_end)
	COPY_REG_SET (combo_bb->global_live_at_end,
		      join_bb->global_live_at_end);
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
      tidy_fallthru_edge (combo_bb->succ, combo_bb, join_bb);
    }

  /* Make sure we update life info properly.  */
  SET_UPDATE_LIFE (combo_bb);

  num_updated_if_blocks++;
}

/* Find a block ending in a simple IF condition.  Return TRUE if
   we were able to transform it in some way.  */

static int
find_if_header (test_bb)
     basic_block test_bb;
{
  edge then_edge;
  edge else_edge;

  /* The kind of block we're looking for has exactly two successors.  */
  if ((then_edge = test_bb->succ) == NULL_EDGE
      || (else_edge = then_edge->succ_next) == NULL_EDGE
      || else_edge->succ_next != NULL_EDGE)
    return FALSE;

  /* Neither edge should be abnormal.  */
  if ((then_edge->flags & EDGE_COMPLEX)
      || (else_edge->flags & EDGE_COMPLEX))
    return FALSE;

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
    return FALSE;

  if (find_if_block (test_bb, then_edge, else_edge))
    goto success;
  if (post_dominators
      && (! HAVE_conditional_execution || reload_completed))
    {
      if (find_if_case_1 (test_bb, then_edge, else_edge))
	goto success;
      if (find_if_case_2 (test_bb, then_edge, else_edge))
	goto success;
    }

  return FALSE;

 success:
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Conversion succeeded.\n");
  return TRUE;
}

/* Determine if a given basic block heads a simple IF-THEN or IF-THEN-ELSE
   block.  If so, we'll try to convert the insns to not require the branch.
   Return TRUE if we were successful at converting the the block.  */

static int
find_if_block (test_bb, then_edge, else_edge)
      basic_block test_bb;
      edge then_edge, else_edge;
{
  basic_block then_bb = then_edge->dest;
  basic_block else_bb = else_edge->dest;
  basic_block join_bb = NULL_BLOCK;
  edge then_succ = then_bb->succ;
  edge else_succ = else_bb->succ;
  int next_index;

  /* The THEN block of an IF-THEN combo must have exactly one predecessor.  */
  if (then_bb->pred->pred_next != NULL_EDGE)
    return FALSE;

  /* The THEN block of an IF-THEN combo must have exactly one successor.  */
  if (then_succ == NULL_EDGE
      || then_succ->succ_next != NULL_EDGE
      || (then_succ->flags & EDGE_COMPLEX))
    return FALSE;

  /* The THEN block may not start with a label, as might happen with an
     unused user label that has had its address taken.  */
  if (GET_CODE (then_bb->head) == CODE_LABEL)
    return FALSE;

  /* If the THEN block's successor is the other edge out of the TEST block,
     then we have an IF-THEN combo without an ELSE.  */
  if (then_succ->dest == else_bb)
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
	   && ! (else_succ->flags & EDGE_COMPLEX))
    join_bb = else_succ->dest;

  /* Otherwise it is not an IF-THEN or IF-THEN-ELSE combination.  */
  else
    return FALSE;	   

  num_possible_if_blocks++;

  if (rtl_dump_file)
    {
      if (else_bb)
	fprintf (rtl_dump_file,
		 "\nIF-THEN-ELSE block found, start %d, then %d, else %d, join %d\n",
		 test_bb->index, then_bb->index, else_bb->index,
		 join_bb->index);
      else
	fprintf (rtl_dump_file,
		 "\nIF-THEN block found, start %d, then %d, join %d\n",
		 test_bb->index, then_bb->index, join_bb->index);
    }

  /* Make sure IF, THEN, and ELSE, blocks are adjacent.  Actually, we
     get the first condition for free, since we've already asserted that
     there's a fallthru edge from IF to THEN.  */
  /* ??? As an enhancement, move the ELSE block.  Have to deal with EH and
     BLOCK notes, if by no other means than aborting the merge if they
     exist.  Sticky enough I don't want to think about it now.  */
  next_index = then_bb->index;
  if (else_bb && ++next_index != else_bb->index)
    return FALSE;
  if (++next_index != join_bb->index)
    {
      if (else_bb)
	join_bb = NULL;
      else
	return FALSE;
    }

  /* Do the real work.  */
  return process_if_block (test_bb, then_bb, else_bb, join_bb);
}

/* Look for IF-THEN-ELSE cases in which one of THEN or ELSE is
   transformable, but not necessarily the other.  There need be no
   JOIN block.

   Return TRUE if we were successful at converting the the block.

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
  basic_block else_bb = else_edge->dest;
  edge then_succ = then_bb->succ;
  rtx new_lab;

  /* THEN has one successor.  */
  if (!then_succ || then_succ->succ_next != NULL)
    return FALSE;

  /* THEN does not fall through, but is not strange either.  */
  if (then_succ->flags & (EDGE_COMPLEX | EDGE_FALLTHRU))
    return FALSE;

  /* THEN has one predecessor.  */
  if (then_bb->pred->pred_next != NULL)
    return FALSE;

  /* THEN has no label.  */
  if (GET_CODE (then_bb->head) == CODE_LABEL)
    return FALSE;

  /* ELSE follows THEN.  (??? could be moved)  */
  if (else_bb->index != then_bb->index + 1)
    return FALSE;

  num_possible_if_blocks++;
  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "\nIF-CASE-1 found, start %d, then %d\n",
	     test_bb->index, then_bb->index);

  /* THEN is small.  */
  if (count_bb_insns (then_bb) > BRANCH_COST)
    return FALSE;

  /* Find the label for THEN's destination.  */
  if (then_succ->dest == EXIT_BLOCK_PTR)
    new_lab = NULL_RTX;
  else
    {
      new_lab = JUMP_LABEL (then_bb->end);
      if (! new_lab)
	abort ();
    }

  /* Registers set are dead, or are predicable.  */
  if (! dead_or_predicable (test_bb, then_bb, else_bb, new_lab, 1))
    return FALSE;

  /* Conversion went ok, including moving the insns and fixing up the
     jump.  Adjust the CFG to match.  */

  SET_UPDATE_LIFE (test_bb);
  bitmap_operation (test_bb->global_live_at_end,
		    else_bb->global_live_at_start,
		    then_bb->global_live_at_end, BITMAP_IOR);
  
  make_edge (NULL, test_bb, then_succ->dest, 0);
  flow_delete_block (then_bb);
  tidy_fallthru_edge (else_edge, test_bb, else_bb);

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
  rtx new_lab, note;

  /* ELSE has one successor.  */
  if (!else_succ || else_succ->succ_next != NULL)
    return FALSE;

  /* ELSE outgoing edge is not complex.  */
  if (else_succ->flags & EDGE_COMPLEX)
    return FALSE;

  /* ELSE has one predecessor.  */
  if (else_bb->pred->pred_next != NULL)
    return FALSE;

  /* ELSE has a label we can delete.  */
  if (LABEL_NUSES (else_bb->head) > 1
      || LABEL_PRESERVE_P (else_bb->head)
      || LABEL_NAME (else_bb->head))
    return FALSE;

  /* ELSE is predicted or SUCC(ELSE) postdominates THEN.  */
  note = find_reg_note (test_bb->end, REG_BR_PROB, NULL_RTX);
  if (note && INTVAL (XEXP (note, 0)) >= REG_BR_PROB_BASE / 2)
    ;
  else if (else_succ->dest->index < 0
	   || (then_bb->index >= 0
	       && TEST_BIT (post_dominators[ORIG_INDEX (then_bb)], 
			    ORIG_INDEX (else_succ->dest))))
    ;
  else
    return FALSE;

  num_possible_if_blocks++;
  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "\nIF-CASE-2 found, start %d, else %d\n",
	     test_bb->index, else_bb->index);

  /* ELSE is small.  */
  if (count_bb_insns (then_bb) > BRANCH_COST)
    return FALSE;

  /* Find the label for ELSE's destination.  */
  if (else_succ->dest == EXIT_BLOCK_PTR)
    new_lab = NULL_RTX;
  else
    {
      if (else_succ->flags & EDGE_FALLTHRU)
	{
	  new_lab = else_succ->dest->head;
	  if (GET_CODE (new_lab) != CODE_LABEL)
	    abort ();
	}
      else
	{
	  new_lab = JUMP_LABEL (else_bb->end);
	  if (! new_lab)
	    abort ();
	}
    }

  /* Registers set are dead, or are predicable.  */
  if (! dead_or_predicable (test_bb, else_bb, then_bb, new_lab, 0))
    return FALSE;

  /* Conversion went ok, including moving the insns and fixing up the
     jump.  Adjust the CFG to match.  */

  SET_UPDATE_LIFE (test_bb);
  bitmap_operation (test_bb->global_live_at_end,
		    then_bb->global_live_at_start,
		    else_bb->global_live_at_end, BITMAP_IOR);
  
  remove_edge (else_edge);
  make_edge (NULL, test_bb, else_succ->dest, 0);
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
     rtx new_dest;
     int reversep;
{
  rtx head, end, jump, earliest, old_dest;

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
      if (head == end)
	{
	  head = end = NULL_RTX;
	  goto no_body;
	}
      end = PREV_INSN (end);
    }

  if (HAVE_conditional_execution)
    {
      /* In the conditional execution case, we have things easy.  We know
	 the condition is reversable.  We don't have to check life info,
	 becase we're going to conditionally execute the code anyway.
	 All that's left is making sure the insns involved can actually
	 be predicated.  */

      rtx cond;

      cond = cond_exec_get_condition (jump);
      if (reversep)
	cond = gen_rtx_fmt_ee (reverse_condition (GET_CODE (cond)),
			       GET_MODE (cond), XEXP (cond, 0),
			       XEXP (cond, 1));

      if (! cond_exec_process_insns (head, end, cond, 0))
	goto cancel;

      earliest = jump;
    }
  else
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
		 true_dependance false against every store in the
		 TEST range.  */
	      if (for_each_rtx (&PATTERN (insn), find_memory, NULL))
		return FALSE;
	    }
	  if (insn == end)
	    break;
	}

      if (! condjump_p (jump))
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
      propagate_block (merge_bb, tmp, merge_set, 0);

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
      pbi = init_propagate_block_info (test_bb, test_live, test_set, 0);

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
  if (reversep
      ? ! invert_jump_1 (jump, new_dest)
      : ! redirect_jump_1 (jump, new_dest))
    goto cancel;

  if (! apply_change_group ())
    return FALSE;

  if (old_dest)
    LABEL_NUSES (old_dest) -= 1;
  if (new_dest)
    LABEL_NUSES (new_dest) += 1;
  JUMP_LABEL (jump) = new_dest;

  if (reversep)
    {
      rtx note = find_reg_note (jump, REG_BR_PROB, NULL_RTX);
      if (note)
	XEXP (note, 0) = GEN_INT (REG_BR_PROB_BASE - INTVAL (XEXP (note, 0)));
    }

  /* Move the insns out of MERGE_BB to before the branch.  */
  if (end == merge_bb->end)
    merge_bb->end = merge_bb->head;
  if (head != NULL)
    {
      head = squeeze_notes (head, end);
      if (GET_CODE (end) == NOTE
	  && (NOTE_LINE_NUMBER (end) == NOTE_INSN_BLOCK_END
              || NOTE_LINE_NUMBER (end) == NOTE_INSN_BLOCK_BEG
              || NOTE_LINE_NUMBER (end) == NOTE_INSN_LOOP_BEG
              || NOTE_LINE_NUMBER (end) == NOTE_INSN_LOOP_END
              || NOTE_LINE_NUMBER (end) == NOTE_INSN_LOOP_CONT
              || NOTE_LINE_NUMBER (end) == NOTE_INSN_LOOP_VTOP))
	{
	  if (head == end)
	    return TRUE;
	  end = PREV_INSN (end);
	}

      reorder_insns (head, end, PREV_INSN (earliest));
    }
  return TRUE;

 cancel:
  cancel_changes (0);
  return FALSE;
}

/* Main entry point for all if-conversion.  */

void
if_convert (life_data_ok)
     int life_data_ok;
{
  int block_num;

  num_possible_if_blocks = 0;
  num_updated_if_blocks = 0;
  num_removed_blocks = 0;

  /* Free up basic_block_for_insn so that we don't have to keep it 
     up to date, either here or in merge_blocks_nomove.  */
  free_basic_block_vars (1);

  /* Compute postdominators if we think we'll use them.  */
  post_dominators = NULL;
  if (HAVE_conditional_execution || life_data_ok)
    {
      post_dominators = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
      compute_flow_dominators (NULL, post_dominators);
    }

  /* Record initial block numbers.  */
  for (block_num = 0; block_num < n_basic_blocks; block_num++)
    SET_ORIG_INDEX (BASIC_BLOCK (block_num), block_num);

  /* Go through each of the basic blocks looking for things to convert.  */
  for (block_num = 0; block_num < n_basic_blocks; )
    {
      basic_block bb = BASIC_BLOCK (block_num);
      if (find_if_header (bb))
	block_num = bb->index;
      else 
	block_num++;
    }

  sbitmap_vector_free (post_dominators);

  if (rtl_dump_file)
    fflush (rtl_dump_file);

  /* Rebuild basic_block_for_insn for update_life_info and for gcse.  */
  compute_bb_for_insn (get_max_uid ());

  /* Rebuild life info for basic blocks that require it.  */
  if (num_removed_blocks && life_data_ok)
    {
      sbitmap update_life_blocks = sbitmap_alloc (n_basic_blocks);
      sbitmap_zero (update_life_blocks);

      /* If we allocated new pseudos, we must resize the array for sched1.  */
      if (max_regno < max_reg_num ())
	{
	  max_regno = max_reg_num ();
	  allocate_reg_info (max_regno, FALSE, FALSE);
	}

      for (block_num = 0; block_num < n_basic_blocks; block_num++)
	if (UPDATE_LIFE (BASIC_BLOCK (block_num)))
	  SET_BIT (update_life_blocks, block_num);

      count_or_remove_death_notes (update_life_blocks, 1);
      update_life_info (update_life_blocks, UPDATE_LIFE_LOCAL,
			PROP_DEATH_NOTES);

      sbitmap_free (update_life_blocks);
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

  verify_flow_info ();
}
