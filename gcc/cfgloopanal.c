/* Natural loop analysis code for GNU compiler.
   Copyright (C) 2002 Free Software Foundation, Inc.

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

struct unmark_altered_insn_data;
static void unmark_altered	 PARAMS ((rtx, rtx, regset));
static void blocks_invariant_registers PARAMS ((basic_block *, int, regset));
static void unmark_altered_insn	 PARAMS ((rtx, rtx, struct unmark_altered_insn_data *));
static void blocks_single_set_registers PARAMS ((basic_block *, int, rtx *));
static int invariant_rtx_wrto_regs_p_helper PARAMS ((rtx *, regset));
static bool invariant_rtx_wrto_regs_p PARAMS ((rtx, regset));
static rtx test_for_iteration PARAMS ((struct loop_desc *desc,
				       unsigned HOST_WIDE_INT));
static bool constant_iterations PARAMS ((struct loop_desc *,
					 unsigned HOST_WIDE_INT *,
					 bool *));
static bool simple_loop_exit_p PARAMS ((struct loops *, struct loop *,
					edge, regset, rtx *,
					struct loop_desc *));
static rtx variable_initial_value PARAMS ((rtx, regset, rtx, rtx *));
static rtx variable_initial_values PARAMS ((edge, rtx));
static bool simple_condition_p PARAMS ((struct loop *, rtx,
					regset, struct loop_desc *));
static basic_block simple_increment PARAMS ((struct loops *, struct loop *,
					     rtx *, struct loop_desc *));

/* Checks whether BB is executed exactly once in each LOOP iteration.  */
bool
just_once_each_iteration_p (loops, loop, bb)
     struct loops *loops;
     struct loop *loop;
     basic_block bb;
{
  /* It must be executed at least once each iteration.  */
  if (!dominated_by_p (loops->cfg.dom, loop->latch, bb))
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
unmark_altered (what, by, regs)
     rtx what;
     rtx by ATTRIBUTE_UNUSED;
     regset regs;
{
  if (GET_CODE (what) == SUBREG)
    what = SUBREG_REG (what);
  if (!REG_P (what))
    return;
  CLEAR_REGNO_REG_SET (regs, REGNO (what));
}

/* Marks registers that are invariant inside blocks BBS.  */
static void
blocks_invariant_registers (bbs, nbbs, regs)
     basic_block *bbs;
     int nbbs;
     regset regs;
{
  rtx insn;
  int i;

  for (i = 0; i < max_reg_num (); i++)
    SET_REGNO_REG_SET (regs, i);
  for (i = 0; i < nbbs; i++)
    for (insn = bbs[i]->head;
	 insn != NEXT_INSN (bbs[i]->end);
	 insn = NEXT_INSN (insn))
      if (INSN_P (insn))
	note_stores (PATTERN (insn),
		     (void (*) PARAMS ((rtx, rtx, void *))) unmark_altered,
		     regs);
}

/* Unmarks modified registers; helper to blocks_single_set_registers.  */
struct unmark_altered_insn_data
{
  rtx *regs;
  rtx insn;
};

static void
unmark_altered_insn (what, by, data)
     rtx what;
     rtx by ATTRIBUTE_UNUSED;
     struct unmark_altered_insn_data *data;
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
blocks_single_set_registers (bbs, nbbs, regs)
     basic_block *bbs;
     int nbbs;
     rtx *regs;
{
  rtx insn;
  int i;
  struct unmark_altered_insn_data data;

  for (i = 0; i < max_reg_num (); i++)
    regs[i] = NULL;

  for (i = 0; i < nbbs; i++)
    for (insn = bbs[i]->head;
	 insn != NEXT_INSN (bbs[i]->end);
	 insn = NEXT_INSN (insn))
      {
	rtx set = single_set (insn);
	if (!set)
	  continue;
	if (!REG_P (SET_DEST (set)))
	  continue;
	regs[REGNO (SET_DEST (set))] = insn;
      }

  data.regs = regs;
  for (i = 0; i < nbbs; i++)
    for (insn = bbs[i]->head;
	 insn != NEXT_INSN (bbs[i]->end);
	 insn = NEXT_INSN (insn))
      {
        if (!INSN_P (insn))
	  continue;
	data.insn = insn;
	note_stores (PATTERN (insn),
	    (void (*) PARAMS ((rtx, rtx, void *))) unmark_altered_insn,
	    &data);
      }
}

/* Helper for invariant_rtx_wrto_regs_p.  */
static int
invariant_rtx_wrto_regs_p_helper (expr, invariant_regs)
     rtx *expr;
     regset invariant_regs;
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

/* Checks that EXPR is invariant provided that INVARIANT_REGS are invariant. */
static bool
invariant_rtx_wrto_regs_p (expr, invariant_regs)
     rtx expr;
     regset invariant_regs;
{
  return !for_each_rtx (&expr, (rtx_function) invariant_rtx_wrto_regs_p_helper,
			invariant_regs);
}

/* Checks whether CONDITION is a simple comparison in that one of operands
   is register and the other one is invariant in the LOOP. Fills var, lim
   and cond fields in DESC.  */
static bool
simple_condition_p (loop, condition, invariant_regs, desc)
     struct loop *loop ATTRIBUTE_UNUSED;
     rtx condition;
     regset invariant_regs;
     struct loop_desc *desc;
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

  /* Check the other operand. */
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
simple_increment (loops, loop, simple_increment_regs, desc)
     struct loops *loops;
     struct loop *loop;
     rtx *simple_increment_regs;
     struct loop_desc *desc;
{
  rtx mod_insn, set, set_src, set_add;
  basic_block mod_bb;

  /* Find insn that modifies var.  */
  mod_insn = simple_increment_regs[REGNO (desc->var)];
  if (!mod_insn)
    return NULL;
  mod_bb = BLOCK_FOR_INSN (mod_insn);

  /* Check that it is executed exactly once each iteration.  */
  if (!just_once_each_iteration_p (loops, loop, mod_bb))
    return NULL;

  /* mod_insn must be a simple increment/decrement.  */
  set = single_set (mod_insn);
  if (!set)
    abort ();
  if (!rtx_equal_p (SET_DEST (set), desc->var))
    abort ();

  set_src = find_reg_equal_equiv_note (mod_insn);
  if (!set_src)
    set_src = SET_SRC (set);
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
   placed here.  */
static rtx
variable_initial_value (insn, invariant_regs, var, set_insn)
     rtx insn;
     regset invariant_regs;
     rtx var;
     rtx *set_insn;
{
  basic_block bb;
  rtx set;

  /* Go back through cfg.  */
  bb = BLOCK_FOR_INSN (insn);
  while (1)
    {
      for (; insn != bb->head; insn = PREV_INSN (insn))
	{
	  if (modified_between_p (var, PREV_INSN (insn), NEXT_INSN (insn)))
	    break;
	  if (INSN_P (insn))
	    note_stores (PATTERN (insn),
		(void (*) PARAMS ((rtx, rtx, void *))) unmark_altered,
		invariant_regs);
	}

      if (insn != bb->head)
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
	  if (!invariant_rtx_wrto_regs_p (val, invariant_regs))
	    return NULL;

	  if (set_insn)
	    *set_insn = insn;
	  return val;
	}


      if (bb->pred->pred_next || bb->pred->src == ENTRY_BLOCK_PTR)
	return NULL;

      bb = bb->pred->src;
      insn = bb->end;
    }

  return NULL;
}

/* Returns list of definitions of initial value of VAR at Edge.  */
static rtx
variable_initial_values (e, var)
     edge e;
     rtx var;
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

  set_insn = e->src->end;
  while (REG_P (var)
	 && (var = variable_initial_value (set_insn, invariant_regs, var, &set_insn)))
    list = alloc_EXPR_LIST (0, copy_rtx (var), list);

  FREE_REG_SET (invariant_regs);
  return list;
}

/* Counts constant number of iterations of the loop described by DESC;
   returns false if impossible.  */
static bool
constant_iterations (desc, niter, may_be_zero)
     struct loop_desc *desc;
     unsigned HOST_WIDE_INT *niter;
     bool *may_be_zero;
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
	abort ();
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
	abort ();
      if (GET_CODE (expr) == CONST_INT)
	{
	  *niter = INTVAL (expr);
	  return true;
	}
    }

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
   
   When INIT/LIM are set, they are used instead of var/lim of DESC. */
rtx
count_loop_iterations (desc, init, lim)
     struct loop_desc *desc;
     rtx init;
     rtx lim;
{
  enum rtx_code cond = desc->cond;
  rtx stride = desc->stride;
  rtx mod, exp;

  /* Give up on floating point modes and friends.  It can be possible to do
     the job for constant loop bounds, but it is probably not worthwhile.  */
  if (!INTEGRAL_MODE_P (GET_MODE (desc->var)))
    return NULL;

  init = copy_rtx (init ? init : desc->var);
  lim = copy_rtx (lim ? lim : desc->lim);

  /* Ensure that we always handle the condition to stay inside loop.  */
  if (desc->neg)
    cond = reverse_condition (cond);

  /* Compute absolute value of the difference of initial and final value.  */
  if (INTVAL (stride) > 0)
    {
      /* Bypass nonsensical tests.  */
      if (cond == EQ || cond == GE || cond == GT || cond == GEU
	  || cond == GTU)
	return NULL;
      exp = simplify_gen_binary (MINUS, GET_MODE (desc->var),
				 lim, init);
    }
  else
    {
      /* Bypass nonsensical tests.  */
      if (cond == EQ || cond == LE || cond == LT || cond == LEU
	  || cond == LTU)
	return NULL;
      exp = simplify_gen_binary (MINUS, GET_MODE (desc->var),
				 init, lim);
      stride = simplify_gen_unary (NEG, GET_MODE (desc->var),
				   stride, GET_MODE (desc->var));
    }

  /* Normalize difference so the value is always first examined
     and later incremented.  */

  if (!desc->postincr)
    exp = simplify_gen_binary (MINUS, GET_MODE (desc->var),
			       exp, stride);

  /* Determine delta caused by exit condition.  */
  switch (cond)
    {
    case NE:
      /* For NE tests, make sure that the iteration variable won't miss
	 the final value.  If EXP mod STRIDE is not zero, then the
	 iteration variable will overflow before the loop exits, and we
	 can not calculate the number of iterations easily.  */
      if (stride != const1_rtx
	  && (simplify_gen_binary (UMOD, GET_MODE (desc->var), exp, stride)
              != const0_rtx))
	return NULL;
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
      exp = simplify_gen_binary (PLUS, GET_MODE (desc->var),
				 exp, const1_rtx);
      break;
    default:
      abort ();
    }

  if (stride != const1_rtx)
    {
      /* Number of iterations is now (EXP + STRIDE - 1 / STRIDE),
	 but we need to take care for overflows.   */

      mod = simplify_gen_binary (UMOD, GET_MODE (desc->var), exp, stride);

      /* This is dirty trick.  When we can't compute number of iterations
	 to be constant, we simply ignore the possible overflow, as
	 runtime unroller always use power of 2 amounts and does not
	 care about possible lost bits.  */

      if (GET_CODE (mod) != CONST_INT)
	{
	  rtx stridem1 = simplify_gen_binary (PLUS, GET_MODE (desc->var),
					      stride, constm1_rtx);
	  exp = simplify_gen_binary (PLUS, GET_MODE (desc->var),
				     exp, stridem1);
	  exp = simplify_gen_binary (UDIV, GET_MODE (desc->var), exp, stride);
	}
      else
	{
	  exp = simplify_gen_binary (UDIV, GET_MODE (desc->var), exp, stride);
	  if (mod != const0_rtx)
	    exp = simplify_gen_binary (PLUS, GET_MODE (desc->var),
				       exp, const1_rtx);
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
test_for_iteration (desc, iter)
     struct loop_desc *desc;
     unsigned HOST_WIDE_INT iter;
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
      fprintf (rtl_dump_file, ";  Conditional to continue loop at ");
      fprintf (rtl_dump_file, HOST_WIDE_INT_PRINT_UNSIGNED, iter);
      fprintf (rtl_dump_file, "th iteration: ");
      print_simple_rtl (rtl_dump_file, exp);
      fprintf (rtl_dump_file, "\n");
    }
  return exp;
}


/* Tests whether exit at EXIT_EDGE from LOOP is simple.  Returns simple loop
   description joined to it in in DESC.  INVARIANT_REGS and SINGLE_SET_REGS
   are results of blocks_{invariant,single_set}_regs over BODY.  */
static bool
simple_loop_exit_p (loops, loop, exit_edge, invariant_regs, single_set_regs, desc)
     struct loops *loops;
     struct loop *loop;
     edge exit_edge;
     struct loop_desc *desc;
     regset invariant_regs;
     rtx *single_set_regs;
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
  if (!dominated_by_p (loops->cfg.dom, loop->latch, exit_bb))
    return false;

  /* It must end in a simple conditional jump.  */
  if (!any_condjump_p (exit_bb->end))
    return false;

  ei = exit_bb->succ;
  if (ei == exit_edge)
    ei = ei->succ_next;

  desc->out_edge = exit_edge;
  desc->in_edge = ei;

  /* Condition must be a simple comparison in that one of operands
     is register and the other one is invariant.  */
  if (!(condition = get_condition (exit_bb->end, NULL)))
    return false;

  if (!simple_condition_p (loop, condition, invariant_regs, desc))
    return false;

  /*  Var must be simply incremented or decremented in exactly one insn that
     is executed just once every iteration.  */
  if (!(mod_bb = simple_increment (loops, loop, single_set_regs, desc)))
    return false;

  /* OK, it is simple loop.  Now just fill in remaining info.  */
  desc->postincr = !dominated_by_p (loops->cfg.dom, exit_bb, mod_bb);
  desc->neg = !fallthru_out;

  /* Find initial value of var and alternative values for lim.  */
  e = loop_preheader_edge (loop);
  desc->var_alts = variable_initial_values (e, desc->var);
  desc->lim_alts = variable_initial_values (e, desc->lim);

  /* Number of iterations. */
  if (!count_loop_iterations (desc, NULL, NULL))
    return false;
  desc->const_iter =
    constant_iterations (desc, &desc->niter, &desc->may_be_zero);
  return true;
}

/* Tests whether LOOP is simple for loop.  Returns simple loop description
   in DESC.  */
bool
simple_loop_p (loops, loop, desc)
     struct loops *loops;
     struct loop *loop;
     struct loop_desc *desc;
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
	    && simple_loop_exit_p (loops, loop, e,
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

/* Marks blocks that are part of non-recognized loops; i.e. we throw away
   all latch edges and mark blocks inside any remaining cycle.  Everything
   is a bit complicated due to fact we do not want to do this for parts of
   cycles that only "pass" through some loop -- i.e. for each cycle, we want
   to mark blocks that belong directly to innermost loop containing the whole
   cycle.  */
void
mark_irreducible_loops (loops)
     struct loops *loops;
{
  int *dfs_in, *closed, *mr, *mri, *n_edges, *stack;
  unsigned i;
  edge **edges, e;
  basic_block act;
  int stack_top, tick, depth;
  struct loop *cloop;

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
     loops.*/
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
      stack[stack_top++] = loops->parray[i]->header->index + 1;

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
	      if (mr[sidx] < mr[idx] && !closed[mri[sidx]])
		{
		  mr[idx] = mr[sidx];
		  mri[idx] = mri[sidx];
		}
	      continue;
	    }
	  if (dfs_in[sidx] < 0)
	    {
	      stack[stack_top++] = sidx;
	      goto next;
	    }
	  if (dfs_in[sidx] < mr[idx])
	    {
	      mr[idx] = dfs_in[sidx];
	      mri[idx] = sidx;
	    }
	}

      /* Return back.  */
      closed[idx] = 1;
      stack_top--;
      if (stack_top && dfs_in[stack[stack_top - 1]] >= 0)
        {
	  /* Propagate information back.  */
	  sidx = stack[stack_top - 1];
	  if (mr[sidx] > mr[idx])
	    {
	      mr[sidx] = mr[idx];
	      mri[sidx] = mri[idx];
	    }
	}
      /* Mark the block if relevant.  */
      if (idx && idx <= last_basic_block && mr[idx] <= dfs_in[idx])
        BASIC_BLOCK (idx - 1)->flags |= BB_IRREDUCIBLE_LOOP;
next:;
    }

  free (stack);
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
num_loop_insns (loop)
     struct loop *loop;
{
  basic_block *bbs, bb;
  unsigned i, ninsns = 0;
  rtx insn;

  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];
      ninsns++;
      for (insn = bb->head; insn != bb->end; insn = NEXT_INSN (insn))
	ninsns++;
    }
  free(bbs);
  
  return ninsns;
}

/* Counts number of insns executed on average per iteration LOOP.  */
int
average_num_loop_insns (loop)
     struct loop *loop;
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
      for (insn = bb->head; insn != bb->end; insn = NEXT_INSN (insn))
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
expected_loop_iterations (loop)
     const struct loop *loop;
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
