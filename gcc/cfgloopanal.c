/* Natural loop analysis code for GNU compiler.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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
#include "hard-reg-set.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "expr.h"
#include "output.h"
/* Needed for doloop_condition_get().  */
#include "loop.h"

struct unmark_altered_insn_data;
static void unmark_altered (rtx, rtx, regset);
static void blocks_invariant_registers (basic_block *, int, regset);
static void unmark_altered_insn (rtx, rtx, struct unmark_altered_insn_data *);
static void blocks_single_set_registers (basic_block *, int, rtx *);
static int invariant_rtx_wrto_regs_p_helper (rtx *, regset);
static bool invariant_rtx_wrto_regs_p (rtx, regset);
static rtx test_for_iteration (struct loop_desc *desc, unsigned HOST_WIDE_INT);
static bool constant_iterations (struct loop_desc *, unsigned HOST_WIDE_INT *,
				 bool *);
static bool simple_loop_exit_p (struct loop *, edge, regset,
				rtx *, struct loop_desc *);
static rtx variable_initial_value (rtx, regset, rtx, rtx *, enum machine_mode);
static rtx variable_initial_values (edge, rtx, enum machine_mode);
static bool simple_condition_p (struct loop *, rtx, regset,
				struct loop_desc *);
static basic_block simple_increment (struct loop *, rtx *, struct loop_desc *);
static rtx count_strange_loop_iterations (rtx, rtx, enum rtx_code,
					  int, rtx, enum machine_mode,
					  enum machine_mode);
static unsigned HOST_WIDEST_INT inverse (unsigned HOST_WIDEST_INT, int);
static bool fits_in_mode_p (enum machine_mode mode, rtx expr);

/* Computes inverse to X modulo (1 << MOD).  */
static unsigned HOST_WIDEST_INT
inverse (unsigned HOST_WIDEST_INT x, int mod)
{
  unsigned HOST_WIDEST_INT mask =
	  ((unsigned HOST_WIDEST_INT) 1 << (mod - 1) << 1) - 1;
  unsigned HOST_WIDEST_INT rslt = 1;
  int i;

  for (i = 0; i < mod - 1; i++)
    {
      rslt = (rslt * x) & mask;
      x = (x * x) & mask;
    }

  return rslt;
}

/* Checks whether BB is executed exactly once in each LOOP iteration.  */
bool
just_once_each_iteration_p (struct loop *loop, basic_block bb)
{
  /* It must be executed at least once each iteration.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
    return false;

  /* And just once.  */
  if (bb->loop_father != loop)
    return false;

  /* But this was not enough.  We might have some irreducible loop here.  */
  if (bb->flags & BB_IRREDUCIBLE_LOOP)
    return false;

  return true;
}


/* Unmarks modified registers; helper to blocks_invariant_registers.  */
static void
unmark_altered (rtx what, rtx by ATTRIBUTE_UNUSED, regset regs)
{
  if (GET_CODE (what) == SUBREG)
    what = SUBREG_REG (what);
  if (!REG_P (what))
    return;
  CLEAR_REGNO_REG_SET (regs, REGNO (what));
}

/* Marks registers that are invariant inside blocks BBS.  */
static void
blocks_invariant_registers (basic_block *bbs, int nbbs, regset regs)
{
  rtx insn;
  int i;

  for (i = 0; i < max_reg_num (); i++)
    SET_REGNO_REG_SET (regs, i);
  for (i = 0; i < nbbs; i++)
    for (insn = BB_HEAD (bbs[i]);
	 insn != NEXT_INSN (BB_END (bbs[i]));
	 insn = NEXT_INSN (insn))
      if (INSN_P (insn))
	note_stores (PATTERN (insn),
		     (void (*) (rtx, rtx, void *)) unmark_altered,
		     regs);
}

/* Unmarks modified registers; helper to blocks_single_set_registers.  */
struct unmark_altered_insn_data
{
  rtx *regs;
  rtx insn;
};

static void
unmark_altered_insn (rtx what, rtx by ATTRIBUTE_UNUSED,
		     struct unmark_altered_insn_data *data)
{
  int rn;

  if (GET_CODE (what) == SUBREG)
    what = SUBREG_REG (what);
  if (!REG_P (what))
    return;
  rn = REGNO (what);
  if (data->regs[rn] == data->insn)
    return;
  data->regs[rn] = NULL;
}

/* Marks registers that have just single simple set in BBS; the relevant
   insn is returned in REGS.  */
static void
blocks_single_set_registers (basic_block *bbs, int nbbs, rtx *regs)
{
  rtx insn;
  int i;
  struct unmark_altered_insn_data data;

  for (i = 0; i < max_reg_num (); i++)
    regs[i] = NULL;

  for (i = 0; i < nbbs; i++)
    for (insn = BB_HEAD (bbs[i]);
	 insn != NEXT_INSN (BB_END (bbs[i]));
	 insn = NEXT_INSN (insn))
      {
	rtx set = single_set (insn);

	if (!set && is_bct_cond (insn))
	  set = get_var_set_from_bct(insn);

	if (!set)
	  continue;
	if (!REG_P (SET_DEST (set)))
	  continue;
	regs[REGNO (SET_DEST (set))] = insn;
      }

  data.regs = regs;
  for (i = 0; i < nbbs; i++)
    for (insn = BB_HEAD (bbs[i]);
	 insn != NEXT_INSN (BB_END (bbs[i]));
	 insn = NEXT_INSN (insn))
      {
        if (!INSN_P (insn))
	  continue;
	data.insn = insn;
	note_stores (PATTERN (insn),
	    (void (*) (rtx, rtx, void *)) unmark_altered_insn,
	    &data);
      }
}

/* Helper for invariant_rtx_wrto_regs_p.  */
static int
invariant_rtx_wrto_regs_p_helper (rtx *expr, regset invariant_regs)
{
  switch (GET_CODE (*expr))
    {
    case CC0:
    case PC:
    case UNSPEC_VOLATILE:
      return 1;

    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case ASM_OPERANDS:
      return MEM_VOLATILE_P (*expr);

    case MEM:
      /* If the memory is not constant, assume it is modified.  If it is
	 constant, we still have to check the address.  */
      return !RTX_UNCHANGING_P (*expr);

    case REG:
      return !REGNO_REG_SET_P (invariant_regs, REGNO (*expr));

    default:
      return 0;
    }
}

/* Checks that EXPR is invariant provided that INVARIANT_REGS are invariant.  */
static bool
invariant_rtx_wrto_regs_p (rtx expr, regset invariant_regs)
{
  return !for_each_rtx (&expr, (rtx_function) invariant_rtx_wrto_regs_p_helper,
			invariant_regs);
}

/* Checks whether CONDITION is a simple comparison in that one of operands
   is register and the other one is invariant in the LOOP. Fills var, lim
   and cond fields in DESC.  */
static bool
simple_condition_p (struct loop *loop ATTRIBUTE_UNUSED, rtx condition,
		    regset invariant_regs, struct loop_desc *desc)
{
  rtx op0, op1;

  /* Check condition.  */
  switch (GET_CODE (condition))
    {
      case EQ:
      case NE:
      case LE:
      case LT:
      case GE:
      case GT:
      case GEU:
      case GTU:
      case LEU:
      case LTU:
	break;
      default:
	return false;
    }

  /* Of integers or pointers.  */
  if (GET_MODE_CLASS (GET_MODE (XEXP (condition, 0))) != MODE_INT
      && GET_MODE_CLASS (GET_MODE (XEXP (condition, 0))) != MODE_PARTIAL_INT)
    return false;

  /* One of operands must be a simple register.  */
  op0 = XEXP (condition, 0);
  op1 = XEXP (condition, 1);

  /* One of operands must be invariant.  */
  if (invariant_rtx_wrto_regs_p (op0, invariant_regs))
    {
      /* And the other one must be a register.  */
      if (!REG_P (op1))
	return false;
      desc->var = op1;
      desc->lim = op0;

      desc->cond = swap_condition (GET_CODE (condition));
      if (desc->cond == UNKNOWN)
	return false;
      return true;
    }

  /* Check the other operand.  */
  if (!invariant_rtx_wrto_regs_p (op1, invariant_regs))
    return false;
  if (!REG_P (op0))
    return false;

  desc->var = op0;
  desc->lim = op1;

  desc->cond = GET_CODE (condition);

  return true;
}

/* Checks whether DESC->var is incremented/decremented exactly once each
   iteration.  Fills in DESC->stride and returns block in that DESC->var is
   modified.  */
static basic_block
simple_increment (struct loop *loop, rtx *simple_increment_regs,
		  struct loop_desc *desc)
{
  rtx mod_insn, mod_insn1, set, set_src, set_add;
  basic_block mod_bb, mod_bb1;

  /* Find insn that modifies var.  */
  mod_insn = simple_increment_regs[REGNO (desc->var)];
  if (!mod_insn)
    return NULL;
  mod_bb = BLOCK_FOR_INSN (mod_insn);

  /* Check that it is executed exactly once each iteration.  */
  if (!just_once_each_iteration_p (loop, mod_bb))
    return NULL;

  /* mod_insn must be a simple increment/decrement.  */
  set = single_set (mod_insn);

  if (!set && is_bct_cond (mod_insn))
    set = get_var_set_from_bct(mod_insn);

  if (!set)
    abort ();
  if (!rtx_equal_p (SET_DEST (set), desc->var))
    abort ();

  set_src = find_reg_equal_equiv_note (mod_insn);
  if (!set_src)
    set_src = SET_SRC (set);

  /* Check for variables that iterate in narrower mode.  */
  if (GET_CODE (set_src) == SIGN_EXTEND
      || GET_CODE (set_src) == ZERO_EXTEND)
    {
      /* If we are sign extending variable that is then compared unsigned
	 or vice versa, there is something weird happening.  */
      if (desc->cond != EQ
	  && desc->cond != NE
	  && ((desc->cond == LEU
	       || desc->cond == LTU
	       || desc->cond == GEU
	       || desc->cond == GTU)
	      ^ (GET_CODE (set_src) == ZERO_EXTEND)))
	return NULL;

      if (GET_CODE (XEXP (set_src, 0)) != SUBREG
	  || SUBREG_BYTE (XEXP (set_src, 0)) != 0
	  || GET_MODE (SUBREG_REG (XEXP (set_src, 0))) != GET_MODE (desc->var))
	return NULL;

      desc->inner_mode = GET_MODE (XEXP (set_src, 0));
      desc->extend = GET_CODE (set_src);
      set_src = SUBREG_REG (XEXP (set_src, 0));

      if (GET_CODE (set_src) != REG)
	return NULL;

      /* Find where the reg is set.  */
      mod_insn1 = simple_increment_regs[REGNO (set_src)];
      if (!mod_insn1)
	return NULL;

      mod_bb1 = BLOCK_FOR_INSN (mod_insn1);
      if (!dominated_by_p (CDI_DOMINATORS, mod_bb, mod_bb1))
	return NULL;
      if (mod_bb1 == mod_bb)
	{
	  for (;
	       mod_insn != PREV_INSN (BB_HEAD (mod_bb));
	       mod_insn = PREV_INSN (mod_insn))
	    if (mod_insn == mod_insn1)
	      break;

	  if (mod_insn == PREV_INSN (BB_HEAD (mod_bb)))
	    return NULL;
	}

      /* Replace the source with the possible place of increment.  */
      set = single_set (mod_insn1);
      if (!set)
	abort ();
      if (!rtx_equal_p (SET_DEST (set), set_src))
	abort ();

      set_src = find_reg_equal_equiv_note (mod_insn1);
      if (!set_src)
	set_src = SET_SRC (set);
    }
  else
    {
      desc->inner_mode = GET_MODE (desc->var);
      desc->extend = NIL;
    }

  if (GET_CODE (set_src) != PLUS)
    return NULL;
  if (!rtx_equal_p (XEXP (set_src, 0), desc->var))
    return NULL;

  /* Set desc->stride.  */
  set_add = XEXP (set_src, 1);
  if (CONSTANT_P (set_add))
    desc->stride = set_add;
  else
    return NULL;

  return mod_bb;
}

/* Tries to find initial value of VAR in INSN.  This value must be invariant
   wrto INVARIANT_REGS.  If SET_INSN is not NULL, insn in that var is set is
   placed here.  INNER_MODE is mode in that induction variable VAR iterates.  */
static rtx
variable_initial_value (rtx insn, regset invariant_regs,
			rtx var, rtx *set_insn, enum machine_mode inner_mode)
{
  basic_block bb;
  rtx set;
  rtx ret = NULL;

  /* Go back through cfg.  */
  bb = BLOCK_FOR_INSN (insn);
  while (1)
    {
      for (; insn != BB_HEAD (bb); insn = PREV_INSN (insn))
	{
	  if (INSN_P (insn))
	    note_stores (PATTERN (insn),
		(void (*) (rtx, rtx, void *)) unmark_altered,
		invariant_regs);
	  if (modified_between_p (var, PREV_INSN (insn), NEXT_INSN (insn)))
	    break;
	}

      if (insn != BB_HEAD (bb))
	{
	  /* We found place where var is set.  */
	  rtx set_dest;
	  rtx val;
	  rtx note;

	  set = single_set (insn);
	  if (!set)
	    return NULL;
	  set_dest = SET_DEST (set);
	  if (!rtx_equal_p (set_dest, var))
	    return NULL;

	  note = find_reg_equal_equiv_note (insn);
	  if (note && GET_CODE (XEXP (note, 0)) != EXPR_LIST)
	    val = XEXP (note, 0);
	  else
	    val = SET_SRC (set);

	  /* If we know that the initial value is indeed in range of
	     the inner mode, record the fact even in case the value itself
	     is useless.  */
	  if ((GET_CODE (val) == SIGN_EXTEND
	       || GET_CODE (val) == ZERO_EXTEND)
	      && GET_MODE (XEXP (val, 0)) == inner_mode)
	    ret = gen_rtx_fmt_e (GET_CODE (val),
				 GET_MODE (var),
				 gen_rtx_fmt_ei (SUBREG,
						 inner_mode,
						 var, 0));

	  if (!invariant_rtx_wrto_regs_p (val, invariant_regs))
	    return ret;

	  if (set_insn)
	    *set_insn = insn;
	  return val;
	}


      if (bb->pred->pred_next || bb->pred->src == ENTRY_BLOCK_PTR)
	return NULL;

      bb = bb->pred->src;
      insn = BB_END (bb);
    }

  return NULL;
}

/* Returns list of definitions of initial value of VAR at edge E.  INNER_MODE
   is mode in that induction variable VAR really iterates.  */
static rtx
variable_initial_values (edge e, rtx var, enum machine_mode inner_mode)
{
  rtx set_insn, list;
  regset invariant_regs;
  regset_head invariant_regs_head;
  int i;

  invariant_regs = INITIALIZE_REG_SET (invariant_regs_head);
  for (i = 0; i < max_reg_num (); i++)
    SET_REGNO_REG_SET (invariant_regs, i);

  list = alloc_EXPR_LIST (0, copy_rtx (var), NULL);

  if (e->src == ENTRY_BLOCK_PTR)
    return list;

  set_insn = BB_END (e->src);
  while (REG_P (var)
	 && (var = variable_initial_value (set_insn, invariant_regs, var,
					   &set_insn, inner_mode)))
    list = alloc_EXPR_LIST (0, copy_rtx (var), list);

  FREE_REG_SET (invariant_regs);
  return list;
}

/* Counts constant number of iterations of the loop described by DESC;
   returns false if impossible.  */
static bool
constant_iterations (struct loop_desc *desc, unsigned HOST_WIDE_INT *niter,
		     bool *may_be_zero)
{
  rtx test, expr;
  rtx ainit, alim;

  test = test_for_iteration (desc, 0);
  if (test == const0_rtx)
    {
      *niter = 0;
      *may_be_zero = false;
      return true;
    }

  *may_be_zero = (test != const_true_rtx);

  /* It would make a little sense to check every with every when we
     know that all but the first alternative are simply registers.  */
  for (ainit = desc->var_alts; ainit; ainit = XEXP (ainit, 1))
    {
      alim = XEXP (desc->lim_alts, 0);
      if (!(expr = count_loop_iterations (desc, XEXP (ainit, 0), alim)))
	continue;
      if (GET_CODE (expr) == CONST_INT)
	{
	  *niter = INTVAL (expr);
	  return true;
	}
    }
  for (alim = XEXP (desc->lim_alts, 1); alim; alim = XEXP (alim, 1))
    {
      ainit = XEXP (desc->var_alts, 0);
      if (!(expr = count_loop_iterations (desc, ainit, XEXP (alim, 0))))
	continue;
      if (GET_CODE (expr) == CONST_INT)
	{
	  *niter = INTVAL (expr);
	  return true;
	}
    }

  return false;
}

/* Attempts to determine a number of iterations of a "strange" loop.
   Its induction variable starts with value INIT, is compared by COND
   with LIM.  If POSTINCR, it is incremented after the test.  It is incremented
   by STRIDE each iteration, has mode MODE but iterates in INNER_MODE.

   By "strange" we mean loops where induction variable increases in the wrong
   direction wrto comparison, i.e. for (i = 6; i > 5; i++).  */
static rtx
count_strange_loop_iterations (rtx init, rtx lim, enum rtx_code cond,
			       int postincr, rtx stride, enum machine_mode mode,
			       enum machine_mode inner_mode)
{
  rtx rqmt, n_to_wrap, before_wrap, after_wrap;
  rtx mode_min, mode_max;
  int size;

  /* This could be handled, but it is not important enough to lose time with
     it just now.  */
  if (mode != inner_mode)
    return NULL_RTX;

  if (!postincr)
    init = simplify_gen_binary (PLUS, mode, init, stride);

  /* If we are able to prove that we don't pass the first test, we are
     done.  */
  rqmt = simplify_relational_operation (cond, mode, init, lim);
  if (rqmt == const0_rtx)
    return const0_rtx;

  /* And if we don't know we pass it, the things are too complicated for us.  */
  if (rqmt != const_true_rtx)
    return NULL_RTX;

  switch (cond)
    {
    case GE:
    case GT:
    case LE:
    case LT:
      size = GET_MODE_BITSIZE (mode);
      mode_min = gen_int_mode (-((unsigned HOST_WIDEST_INT) 1 << (size - 1)),
			       mode);
      mode_max = gen_int_mode (((unsigned HOST_WIDEST_INT) 1 << (size - 1)) - 1,
			       mode);
			      
      break;

    case GEU:
    case GTU:
    case LEU:
    case LTU:
    case EQ:
      mode_min = const0_rtx;
      mode_max = simplify_gen_binary (MINUS, mode, const0_rtx, const1_rtx);
      break;

    default:
      abort ();
    }

  switch (cond)
    {
    case EQ:
      /* This iterates once, as init == lim.  */
      return const1_rtx;

      /* The behavior is undefined in signed cases.  Never mind, we still
	 try to behave sanely.  */
    case GE:
    case GT:
    case GEU:
    case GTU:
      if (INTVAL (stride) <= 0)
	abort ();
      n_to_wrap = simplify_gen_binary (MINUS, mode, mode_max, copy_rtx (init));
      n_to_wrap = simplify_gen_binary (UDIV, mode, n_to_wrap, stride);
      before_wrap = simplify_gen_binary (MULT, mode,
					 copy_rtx (n_to_wrap), stride);
      before_wrap = simplify_gen_binary (PLUS, mode,
					 before_wrap, copy_rtx (init));
      after_wrap = simplify_gen_binary (PLUS, mode,
					before_wrap, stride);
      if (GET_CODE (after_wrap) != CONST_INT)
	{
	  after_wrap = simplify_gen_binary (PLUS, mode, mode_min, stride);
	  after_wrap = simplify_gen_binary (MINUS, mode, after_wrap, const1_rtx);
	}
      break;

    case LE:
    case LT:
    case LEU:
    case LTU:
      if (INTVAL (stride) >= 0)
	abort ();
      stride = simplify_gen_unary (NEG, mode, stride, mode);
      n_to_wrap = simplify_gen_binary (MINUS, mode, copy_rtx (init), mode_min);
      n_to_wrap = simplify_gen_binary (UDIV, mode, n_to_wrap, stride);
      before_wrap = simplify_gen_binary (MULT, mode,
					 copy_rtx (n_to_wrap), stride);
      before_wrap = simplify_gen_binary (MINUS, mode,
					 copy_rtx (init), before_wrap);
      after_wrap = simplify_gen_binary (MINUS, mode,
					before_wrap, stride);
      if (GET_CODE (after_wrap) != CONST_INT)
	{
	  after_wrap = simplify_gen_binary (MINUS, mode, mode_max, stride);
	  after_wrap = simplify_gen_binary (PLUS, mode, after_wrap, const1_rtx);
	}
      break;
    default:
      abort ();
    }

  /* If this is const_true_rtx and we did not take a conservative approximation
     of after_wrap above, we might iterate the calculation (but of course we
     would have to take care about infinite cases).  Ignore this for now.  */
  rqmt = simplify_relational_operation (cond, mode, after_wrap, lim);
  if (rqmt != const0_rtx)
    return NULL_RTX;

  return simplify_gen_binary (PLUS, mode, n_to_wrap, const1_rtx);
}

/* Checks whether value of EXPR fits into range of MODE.  */
static bool
fits_in_mode_p (enum machine_mode mode, rtx expr)
{
  unsigned HOST_WIDEST_INT val;
  int n_bits = 0;

  if (GET_CODE (expr) == CONST_INT)
    {
      for (val = INTVAL (expr); val; val >>= 1)
	n_bits++;

      return n_bits <= GET_MODE_BITSIZE (mode);
    }

  if (GET_CODE (expr) == SIGN_EXTEND
      || GET_CODE (expr) == ZERO_EXTEND)
    return GET_MODE (XEXP (expr, 0)) == mode;

  return false;
}

/* Return RTX expression representing number of iterations of loop as bounded
   by test described by DESC (in the case loop really has multiple exit
   edges, fewer iterations may happen in the practice).

   Return NULL if it is unknown.  Additionally the value may be invalid for
   paradoxical loop (lets define paradoxical loops as loops whose test is
   failing at -1th iteration, for instance "for (i=5;i<1;i++);").

   These cases needs to be either cared by copying the loop test in the front
   of loop or keeping the test in first iteration of loop.

   When INIT/LIM are set, they are used instead of var/lim of DESC.  */
rtx
count_loop_iterations (struct loop_desc *desc, rtx init, rtx lim)
{
  enum rtx_code cond = desc->cond;
  rtx stride = desc->stride;
  rtx mod, exp, ainit, bound;
  rtx overflow_check, mx, mxp;
  enum machine_mode mode = GET_MODE (desc->var);
  unsigned HOST_WIDEST_INT s, size, d;

  /* Give up on floating point modes and friends.  It can be possible to do
     the job for constant loop bounds, but it is probably not worthwhile.  */
  if (!INTEGRAL_MODE_P (mode))
    return NULL;

  init = copy_rtx (init ? init : desc->var);
  lim = copy_rtx (lim ? lim : desc->lim);

  /* Ensure that we always handle the condition to stay inside loop.  */
  if (desc->neg)
    cond = reverse_condition (cond);

  if (desc->inner_mode != mode)
    {
      /* We have a case when the variable in fact iterates in the narrower
	 mode.  This has following consequences:
	 
	 For induction variable itself, if !desc->postincr, it does not mean
	 anything too special, since we know the variable is already in range
	 of the inner mode when we compare it (so it is just needed to shorten
	 it into the mode before calculations are done, so that we don't risk
	 wrong results).  More complicated case is when desc->postincr; then
	 the first two iterations are special (the first one because the value
	 may be out of range, the second one because after shortening it to the
	 range it may have absolutely any value), and we do not handle this in
	 unrolling.  So if we aren't able to prove that the initial value is in
	 the range, we fail in this case.
	 
	 Step is just moduled to fit into inner mode.

	 If lim is out of range, then either the loop is infinite (and then
	 we may unroll however we like to), or exits in the first iteration
	 (this is also ok, since we handle it specially for this case anyway).
	 So we may safely assume that it fits into the inner mode.  */

      for (ainit = desc->var_alts; ainit; ainit = XEXP (ainit, 1))
	if (fits_in_mode_p (desc->inner_mode, XEXP (ainit, 0)))
	  break;

      if (!ainit)
	{
	  if (desc->postincr)
	    return NULL_RTX;

	  init = simplify_gen_unary (desc->extend,
				     mode,
				     simplify_gen_subreg (desc->inner_mode,
							  init,
							  mode,
							  0),
				     desc->inner_mode);
	}

      stride = simplify_gen_subreg (desc->inner_mode, stride, mode, 0);
      if (stride == const0_rtx)
	return NULL_RTX;
    }

  /* Prepare condition to verify that we do not risk overflow.  */
  if (stride == const1_rtx
      || stride == constm1_rtx
      || cond == NE
      || cond == EQ)
    {
      /* Overflow at NE conditions does not occur.  EQ condition
	 is weird and is handled in count_strange_loop_iterations.
	 If stride is 1, overflow may occur only for <= and >= conditions,
	 and then they are infinite, so it does not bother us.  */
      overflow_check = const0_rtx;
    }
  else
    {
      if (cond == LT || cond == LTU)
	mx = simplify_gen_binary (MINUS, mode, lim, const1_rtx);
      else if (cond == GT || cond == GTU)
	mx = simplify_gen_binary (PLUS, mode, lim, const1_rtx);
      else
	mx = lim;
      if (mode != desc->inner_mode)
	mxp = simplify_gen_subreg (desc->inner_mode, mx, mode, 0);
      else
	mxp = mx;
      mxp = simplify_gen_binary (PLUS, desc->inner_mode, mxp, stride);
      if (mode != desc->inner_mode)
	mxp = simplify_gen_unary (desc->extend, mode, mxp, desc->inner_mode);
      overflow_check = simplify_gen_relational (cond, SImode, mode, mx, mxp);
    }
    
  /* Compute absolute value of the difference of initial and final value.  */
  if (INTVAL (stride) > 0)
    {
      /* Handle strange tests specially.  */
      if (cond == EQ || cond == GE || cond == GT || cond == GEU
	  || cond == GTU)
	return count_strange_loop_iterations (init, lim, cond, desc->postincr,
					      stride, mode, desc->inner_mode);
      exp = simplify_gen_binary (MINUS, mode, lim, init);
    }
  else
    {
      if (cond == EQ || cond == LE || cond == LT || cond == LEU
	  || cond == LTU)
	return count_strange_loop_iterations (init, lim, cond, desc->postincr,
					      stride, mode, desc->inner_mode);
      exp = simplify_gen_binary (MINUS, mode, init, lim);
      stride = simplify_gen_unary (NEG, mode, stride, mode);
    }

  /* If there is a risk of overflow (i.e. when we increment value satisfying
     a condition, we may again obtain a value satisfying the condition),
     fail.  */
  if (overflow_check != const0_rtx)
    return NULL_RTX;

  /* Normalize difference so the value is always first examined
     and later incremented.  Do not do this for a loop ending with a branch 
     and count register.  */
  if (!is_bct_cond (BB_END (desc->out_edge->src)) && (!desc->postincr))
     exp = simplify_gen_binary (MINUS, mode, exp, stride);

  /* Determine delta caused by exit condition.  */
  switch (cond)
    {
    case NE:
      /* NE tests are easy to handle, because we just perform simple
	 arithmetics modulo power of 2.  Let's use the fact to compute the
	 number of iterations exactly.  We are now in situation when we want to
	 solve an equation stride * i = c (mod size of inner_mode).
	 Let nsd (stride, size of mode) = d.  If d does not divide c, the
	 loop is infinite.  Otherwise, the number of iterations is
	 (inverse(s/d) * (c/d)) mod (size of mode/d).  */
      size = GET_MODE_BITSIZE (desc->inner_mode);
      s = INTVAL (stride);
      d = 1;
      while (s % 2 != 1)
	{
	  s /= 2;
	  d *= 2;
	  size--;
	}
      bound = gen_int_mode (((unsigned HOST_WIDEST_INT) 1 << (size - 1 ) << 1) - 1,
			    mode);
      exp = simplify_gen_binary (UDIV, mode, exp, gen_int_mode (d, mode));
      exp = simplify_gen_binary (MULT, mode,
				 exp, gen_int_mode (inverse (s, size), mode));
      exp = simplify_gen_binary (AND, mode, exp, bound);
      break;

    case LT:
    case GT:
    case LTU:
    case GTU:
      break;
    case LE:
    case GE:
    case LEU:
    case GEU:
      exp = simplify_gen_binary (PLUS, mode, exp, const1_rtx);
      break;
    default:
      abort ();
    }

  if (cond != NE && stride != const1_rtx)
    {
      /* Number of iterations is now (EXP + STRIDE - 1 / STRIDE),
	 but we need to take care for overflows.  */

      mod = simplify_gen_binary (UMOD, mode, exp, stride);

      /* This is dirty trick.  When we can't compute number of iterations
	 to be constant, we simply ignore the possible overflow, as
	 runtime unroller always use power of 2 amounts and does not
	 care about possible lost bits.  */

      if (GET_CODE (mod) != CONST_INT)
	{
	  rtx stridem1 = simplify_gen_binary (PLUS, mode, stride, constm1_rtx);
	  exp = simplify_gen_binary (PLUS, mode, exp, stridem1);
	  exp = simplify_gen_binary (UDIV, mode, exp, stride);
	}
      else
	{
	  exp = simplify_gen_binary (UDIV, mode, exp, stride);
	  if (mod != const0_rtx)
	    exp = simplify_gen_binary (PLUS, mode, exp, const1_rtx);
	}
    }

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, ";  Number of iterations: ");
      print_simple_rtl (rtl_dump_file, exp);
      fprintf (rtl_dump_file, "\n");
    }

  return exp;
}

/* Return simplified RTX expression representing the value of test
   described of DESC at given iteration of loop.  */

static rtx
test_for_iteration (struct loop_desc *desc, unsigned HOST_WIDE_INT iter)
{
  enum rtx_code cond = desc->cond;
  rtx exp = XEXP (desc->var_alts, 0);
  rtx addval;

  /* Give up on floating point modes and friends.  It can be possible to do
     the job for constant loop bounds, but it is probably not worthwhile.  */
  if (!INTEGRAL_MODE_P (GET_MODE (desc->var)))
    return NULL;

  /* Ensure that we always handle the condition to stay inside loop.  */
  if (desc->neg)
    cond = reverse_condition (cond);

  /* Compute the value of induction variable.  */
  addval = simplify_gen_binary (MULT, GET_MODE (desc->var),
				desc->stride,
				gen_int_mode (desc->postincr
					      ? iter : iter + 1,
					      GET_MODE (desc->var)));
  exp = simplify_gen_binary (PLUS, GET_MODE (desc->var), exp, addval);
  /* Test at given condition.  */
  exp = simplify_gen_relational (cond, SImode,
				 GET_MODE (desc->var), exp, desc->lim);

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, ";  Conditional to continue loop at "
	       HOST_WIDE_INT_PRINT_UNSIGNED "th iteration: ", iter);
      print_simple_rtl (rtl_dump_file, exp);
      fprintf (rtl_dump_file, "\n");
    }
  return exp;
}


/* Tests whether exit at EXIT_EDGE from LOOP is simple.  Returns simple loop
   description joined to it in in DESC.  INVARIANT_REGS and SINGLE_SET_REGS
   are results of blocks_{invariant,single_set}_regs over BODY.  */
static bool
simple_loop_exit_p (struct loop *loop, edge exit_edge,
		    regset invariant_regs, rtx *single_set_regs,
		    struct loop_desc *desc)
{
  basic_block mod_bb, exit_bb;
  int fallthru_out;
  rtx condition;
  edge ei, e;

  exit_bb = exit_edge->src;

  fallthru_out = (exit_edge->flags & EDGE_FALLTHRU);

  if (!exit_bb)
    return false;

  /* It must be tested (at least) once during any iteration.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, exit_bb))
    return false;

  /* It must end in a simple conditional jump.  */
  if (!any_condjump_p (BB_END (exit_bb)))
    return false;

  ei = exit_bb->succ;
  if (ei == exit_edge)
    ei = ei->succ_next;

  desc->out_edge = exit_edge;
  desc->in_edge = ei;

  /* Condition must be a simple comparison in that one of operands
     is register and the other one is invariant.  */
  if (!(condition = get_condition (BB_END (exit_bb), NULL, false)))
    return false;

  if (!simple_condition_p (loop, condition, invariant_regs, desc))
    return false;

  /*  Var must be simply incremented or decremented in exactly one insn that
     is executed just once every iteration.  */
  if (!(mod_bb = simple_increment (loop, single_set_regs, desc)))
    return false;

  /* OK, it is simple loop.  Now just fill in remaining info.  */
  desc->postincr = !dominated_by_p (CDI_DOMINATORS, exit_bb, mod_bb);
  desc->neg = !fallthru_out;

  /* Find initial value of var and alternative values for lim.  */
  e = loop_preheader_edge (loop);
  desc->var_alts = variable_initial_values (e, desc->var, desc->inner_mode);
  desc->lim_alts = variable_initial_values (e, desc->lim, desc->inner_mode);

  /* Number of iterations.  */
  desc->const_iter =
    constant_iterations (desc, &desc->niter, &desc->may_be_zero);
  if (!desc->const_iter && !count_loop_iterations (desc, NULL, NULL))
    return false;
  return true;
}

/* Tests whether LOOP is simple for loop.  Returns simple loop description
   in DESC.  */
bool
simple_loop_p (struct loop *loop, struct loop_desc *desc)
{
  unsigned i;
  basic_block *body;
  edge e;
  struct loop_desc act;
  bool any = false;
  regset invariant_regs;
  regset_head invariant_regs_head;
  rtx *single_set_regs;
  int n_branches;

  body = get_loop_body (loop);

  invariant_regs = INITIALIZE_REG_SET (invariant_regs_head);
  single_set_regs = xmalloc (max_reg_num () * sizeof (rtx));

  blocks_invariant_registers (body, loop->num_nodes, invariant_regs);
  blocks_single_set_registers (body, loop->num_nodes, single_set_regs);

  n_branches = 0;
  for (i = 0; i < loop->num_nodes; i++)
    {
      for (e = body[i]->succ; e; e = e->succ_next)
	if (!flow_bb_inside_loop_p (loop, e->dest)
	    && simple_loop_exit_p (loop, e,
		   invariant_regs, single_set_regs, &act))
	  {
	    /* Prefer constant iterations; the less the better.  */
	    if (!any)
	      any = true;
	    else if (!act.const_iter
		     || (desc->const_iter && act.niter >= desc->niter))
	      continue;
	    *desc = act;
	  }

      if (body[i]->succ && body[i]->succ->succ_next)
	n_branches++;
    }
  desc->n_branches = n_branches;

  if (rtl_dump_file && any)
    {
      fprintf (rtl_dump_file, "; Simple loop %i\n", loop->num);
      if (desc->postincr)
	fprintf (rtl_dump_file,
		 ";  does postincrement after loop exit condition\n");

      fprintf (rtl_dump_file, ";  Induction variable:");
      print_simple_rtl (rtl_dump_file, desc->var);
      fputc ('\n', rtl_dump_file);

      fprintf (rtl_dump_file, ";  Initial values:");
      print_simple_rtl (rtl_dump_file, desc->var_alts);
      fputc ('\n', rtl_dump_file);

      fprintf (rtl_dump_file, ";  Stride:");
      print_simple_rtl (rtl_dump_file, desc->stride);
      fputc ('\n', rtl_dump_file);

      fprintf (rtl_dump_file, ";  Compared with:");
      print_simple_rtl (rtl_dump_file, desc->lim);
      fputc ('\n', rtl_dump_file);

      fprintf (rtl_dump_file, ";  Alternative values:");
      print_simple_rtl (rtl_dump_file, desc->lim_alts);
      fputc ('\n', rtl_dump_file);

      fprintf (rtl_dump_file, ";  Exit condition:");
      if (desc->neg)
	fprintf (rtl_dump_file, "(negated)");
      fprintf (rtl_dump_file, "%s\n", GET_RTX_NAME (desc->cond));

      fprintf (rtl_dump_file, ";  Number of branches:");
      fprintf (rtl_dump_file, "%d\n", desc->n_branches);

      fputc ('\n', rtl_dump_file);
    }

  free (body);
  FREE_REG_SET (invariant_regs);
  free (single_set_regs);
  return any;
}

/* Marks blocks and edges that are part of non-recognized loops; i.e. we
   throw away all latch edges and mark blocks inside any remaining cycle.
   Everything is a bit complicated due to fact we do not want to do this
   for parts of cycles that only "pass" through some loop -- i.e. for
   each cycle, we want to mark blocks that belong directly to innermost
   loop containing the whole cycle.  */
void
mark_irreducible_loops (struct loops *loops)
{
  int *dfs_in, *closed, *mr, *mri, *n_edges, *stack;
  unsigned i;
  edge **edges, e;
  edge *estack;
  basic_block act;
  int stack_top, tick, depth;
  struct loop *cloop;

  /* Reset the flags.  */
  FOR_BB_BETWEEN (act, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    {
      act->flags &= ~BB_IRREDUCIBLE_LOOP;
      for (e = act->succ; e; e = e->succ_next)
	e->flags &= ~EDGE_IRREDUCIBLE_LOOP;
    }

  /* The first last_basic_block + 1 entries are for real blocks (including
     entry); then we have loops->num - 1 fake blocks for loops to that we
     assign edges leading from loops (fake loop 0 is not interesting).  */
  dfs_in = xmalloc ((last_basic_block + loops->num) * sizeof (int));
  closed = xmalloc ((last_basic_block + loops->num) * sizeof (int));
  mr = xmalloc ((last_basic_block + loops->num) * sizeof (int));
  mri = xmalloc ((last_basic_block + loops->num) * sizeof (int));
  n_edges = xmalloc ((last_basic_block + loops->num) * sizeof (int));
  edges = xmalloc ((last_basic_block + loops->num) * sizeof (edge *));
  stack = xmalloc ((n_basic_blocks + loops->num) * sizeof (int));
  estack = xmalloc ((n_basic_blocks + loops->num) * sizeof (edge));

  /* Create the edge lists.  */
  for (i = 0; i < last_basic_block + loops->num; i++)
    n_edges[i] = 0;
  FOR_BB_BETWEEN (act, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    for (e = act->succ; e; e = e->succ_next)
      {
        /* Ignore edges to exit.  */
        if (e->dest == EXIT_BLOCK_PTR)
	  continue;
	/* And latch edges.  */
	if (e->dest->loop_father->header == e->dest
	    && e->dest->loop_father->latch == act)
	  continue;
	/* Edges inside a single loop should be left where they are.  Edges
	   to subloop headers should lead to representative of the subloop,
	   but from the same place.  */
	if (act->loop_father == e->dest->loop_father
	    || act->loop_father == e->dest->loop_father->outer)
	  {
	    n_edges[act->index + 1]++;
	    continue;
	  }
	/* Edges exiting loops remain.  They should lead from representative
	   of the son of nearest common ancestor of the loops in that
	   act lays.  */
	depth = find_common_loop (act->loop_father, e->dest->loop_father)->depth + 1;
	if (depth == act->loop_father->depth)
	  cloop = act->loop_father;
	else
	  cloop = act->loop_father->pred[depth];
	n_edges[cloop->num + last_basic_block]++;
      }

  for (i = 0; i < last_basic_block + loops->num; i++)
    {
      edges[i] = xmalloc (n_edges[i] * sizeof (edge));
      n_edges[i] = 0;
    }

  FOR_BB_BETWEEN (act, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    for (e = act->succ; e; e = e->succ_next)
      {
        if (e->dest == EXIT_BLOCK_PTR)
	  continue;
	if (e->dest->loop_father->header == e->dest
	    && e->dest->loop_father->latch == act)
	  continue;
	if (act->loop_father == e->dest->loop_father
	    || act->loop_father == e->dest->loop_father->outer)
	  {
	    edges[act->index + 1][n_edges[act->index + 1]++] = e;
	    continue;
	  }
	depth = find_common_loop (act->loop_father, e->dest->loop_father)->depth + 1;
	if (depth == act->loop_father->depth)
	  cloop = act->loop_father;
	else
	  cloop = act->loop_father->pred[depth];
	i = cloop->num + last_basic_block;
	edges[i][n_edges[i]++] = e;
      }

  /* Compute dfs numbering, starting from loop headers, and mark found
     loops.  */
  tick = 0;
  for (i = 0; i < last_basic_block + loops->num; i++)
    {
      dfs_in[i] = -1;
      closed[i] = 0;
      mr[i] = last_basic_block + loops->num;
      mri[i] = -1;
    }

  stack_top = 0;
  for (i = 0; i < loops->num; i++)
    if (loops->parray[i])
      {
	stack[stack_top] = loops->parray[i]->header->index + 1;
	estack[stack_top] = NULL;
	stack_top++;
      }

  while (stack_top)
    {
      int idx, sidx;

      idx = stack[stack_top - 1];
      if (dfs_in[idx] < 0)
	dfs_in[idx] = tick++;

      while (n_edges[idx])
	{
	  e = edges[idx][--n_edges[idx]];
	  sidx = e->dest->loop_father->header == e->dest
	           ? e->dest->loop_father->num + last_basic_block
	           : e->dest->index + 1;
          if (closed[sidx])
	    {
	      if (mri[sidx] != -1 && !closed[mri[sidx]])
		{
		  if (mr[sidx] < mr[idx])
		    {
		      mr[idx] = mr[sidx];
		      mri[idx] = mri[sidx];
		    }

		  if (mr[sidx] <= dfs_in[idx])
		    e->flags |= EDGE_IRREDUCIBLE_LOOP;
		}
	      continue;
	    }
	  if (dfs_in[sidx] < 0)
	    {
	      stack[stack_top] = sidx;
	      estack[stack_top] = e;
	      stack_top++;
	      goto next;
	    }
	  if (dfs_in[sidx] < mr[idx])
	    {
	      mr[idx] = dfs_in[sidx];
	      mri[idx] = sidx;
	    }
	  e->flags |= EDGE_IRREDUCIBLE_LOOP;
	}

      /* Return back.  */
      closed[idx] = 1;
      e = estack[stack_top - 1];
      stack_top--;
      if (e)
        {
	  /* Propagate information back.  */
	  sidx = stack[stack_top - 1];
	  if (mr[sidx] > mr[idx])
	    {
	      mr[sidx] = mr[idx];
	      mri[sidx] = mri[idx];
	    }
	  if (mr[idx] <= dfs_in[sidx])
	    e->flags |= EDGE_IRREDUCIBLE_LOOP;
	}
      /* Mark the block if relevant.  */
      if (idx && idx <= last_basic_block && mr[idx] <= dfs_in[idx])
        BASIC_BLOCK (idx - 1)->flags |= BB_IRREDUCIBLE_LOOP;
next:;
    }

  free (stack);
  free (estack);
  free (dfs_in);
  free (closed);
  free (mr);
  free (mri);
  for (i = 0; i < last_basic_block + loops->num; i++)
    free (edges[i]);
  free (edges);
  free (n_edges);
  loops->state |= LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS;
}

/* Counts number of insns inside LOOP.  */
int
num_loop_insns (struct loop *loop)
{
  basic_block *bbs, bb;
  unsigned i, ninsns = 0;
  rtx insn;

  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];
      ninsns++;
      for (insn = BB_HEAD (bb); insn != BB_END (bb); insn = NEXT_INSN (insn))
	if (INSN_P (insn))
	  ninsns++;
    }
  free(bbs);

  return ninsns;
}

/* Counts number of insns executed on average per iteration LOOP.  */
int
average_num_loop_insns (struct loop *loop)
{
  basic_block *bbs, bb;
  unsigned i, binsns, ninsns, ratio;
  rtx insn;

  ninsns = 0;
  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];

      binsns = 1;
      for (insn = BB_HEAD (bb); insn != BB_END (bb); insn = NEXT_INSN (insn))
	if (INSN_P (insn))
	  binsns++;

      ratio = loop->header->frequency == 0
	      ? BB_FREQ_MAX
	      : (bb->frequency * BB_FREQ_MAX) / loop->header->frequency;
      ninsns += binsns * ratio;
    }
  free(bbs);

  ninsns /= BB_FREQ_MAX;
  if (!ninsns)
    ninsns = 1; /* To avoid division by zero.  */

  return ninsns;
}

/* Returns expected number of LOOP iterations.
   Compute upper bound on number of iterations in case they do not fit integer
   to help loop peeling heuristics.  Use exact counts if at all possible.  */
unsigned
expected_loop_iterations (const struct loop *loop)
{
  edge e;

  if (loop->header->count)
    {
      gcov_type count_in, count_latch, expected;

      count_in = 0;
      count_latch = 0;

      for (e = loop->header->pred; e; e = e->pred_next)
	if (e->src == loop->latch)
	  count_latch = e->count;
	else
	  count_in += e->count;

      if (count_in == 0)
	return 0;

      expected = (count_latch + count_in - 1) / count_in;

      /* Avoid overflows.  */
      return (expected > REG_BR_PROB_BASE ? REG_BR_PROB_BASE : expected);
    }
  else
    {
      int freq_in, freq_latch;

      freq_in = 0;
      freq_latch = 0;

      for (e = loop->header->pred; e; e = e->pred_next)
	if (e->src == loop->latch)
	  freq_latch = EDGE_FREQUENCY (e);
	else
	  freq_in += EDGE_FREQUENCY (e);

      if (freq_in == 0)
	return 0;

      return (freq_latch + freq_in - 1) / freq_in;
    }
}

/* This function checks if an instruction is a branch and count instruction
   no matter if the flag HAVE_doloop_end is enabled or not.  An alternative 
   would be the modification of doloop_condition_get function itself.  */ 
bool
is_bct_cond (rtx insn) 
{
  if (GET_CODE (insn) != JUMP_INSN)
    return false;

#ifdef HAVE_doloop_end
  if (!doloop_condition_get (PATTERN(insn)))
    return false;
#else
  return false;
#endif

  return true;
}

/* Extract the increment of the count register from the branch and count
   instruction.  */ 
rtx
get_var_set_from_bct (rtx insn)
{
  rtx rhs, lhs, cond;
  rtx pattern;
  rtx set;
  pattern = PATTERN (insn);

  if (!is_bct_cond (insn))
    abort ();

  set = XVECEXP (pattern, 0, 1);

  /* IA64 has the decrement conditional, i.e. done only when the loop does not 
     end.  We match (set (x (if_then_else (ne x 0) (plus x -1) x))) here.  */

  lhs = XEXP (set, 0);
  rhs = XEXP (set, 1);
  if (GET_CODE (set) != IF_THEN_ELSE)
    return set;
 
  cond = XEXP (rhs, 0);
  if (GET_CODE (cond) != NE
      || !rtx_equal_p (XEXP (cond, 0), lhs)
      || !rtx_equal_p (XEXP (cond, 1), const0_rtx))
         return set;

  rhs = XEXP (rhs, 1);
 
  return gen_rtx_SET (GET_MODE (lhs), lhs, rhs);
}

