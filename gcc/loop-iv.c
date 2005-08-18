/* Rtl-level induction variable analysis.
   Copyright (C) 2004 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* This is just a very simplistic analysis of induction variables of the loop.
   The major use is for determining the number of iterations of a loop for
   loop unrolling, doloop optimization and branch prediction.  For this we
   are only interested in bivs and a fairly limited set of givs that are
   needed in the exit condition.  We also only compute the iv information on
   demand.

   The interesting registers are determined.  A register is interesting if

   -- it is set only in the blocks that dominate the latch of the current loop
   -- all its sets are simple -- i.e. in the form we understand

   We also number the insns sequentially in each basic block.  For a use of the
   interesting reg, it is now easy to find a reaching definition (there may be
   only one).

   Induction variable is then simply analyzed by walking the use-def
   chains.
   
   Usage:

   iv_analysis_loop_init (loop);
   insn = iv_get_reaching_def (where, reg);
   if (iv_analyze (insn, reg, &iv))
     {
       ...
     }
   iv_analysis_done (); */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "obstack.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "expr.h"
#include "output.h"

/* The insn information.  */

struct insn_info
{
  /* Id of the insn.  */
  unsigned luid;

  /* The previous definition of the register defined by the single
     set in the insn.  */
  rtx prev_def;

  /* The description of the iv.  */
  struct rtx_iv iv;
};

static struct insn_info *insn_info;

/* The last definition of register.  */

static rtx *last_def;

/* The bivs.  */

static struct rtx_iv *bivs;

/* Maximal insn number for that there is place in insn_info array.  */

static unsigned max_insn_no;

/* Maximal register number for that there is place in bivs and last_def
   arrays.  */

static unsigned max_reg_no;

/* Dumps information about IV to FILE.  */

extern void dump_iv_info (FILE *, struct rtx_iv *);
void
dump_iv_info (FILE *file, struct rtx_iv *iv)
{
  if (!iv->base)
    {
      fprintf (file, "not simple");
      return;
    }

  if (iv->step == const0_rtx
      && !iv->first_special)
    fprintf (file, "invariant ");

  print_rtl (file, iv->base);
  if (iv->step != const0_rtx)
    {
      fprintf (file, " + ");
      print_rtl (file, iv->step);
      fprintf (file, " * iteration");
    }
  fprintf (file, " (in %s)", GET_MODE_NAME (iv->mode));

  if (iv->mode != iv->extend_mode)
    fprintf (file, " %s to %s",
	     rtx_name[iv->extend],
	     GET_MODE_NAME (iv->extend_mode));

  if (iv->mult != const1_rtx)
    {
      fprintf (file, " * ");
      print_rtl (file, iv->mult);
    }
  if (iv->delta != const0_rtx)
    {
      fprintf (file, " + ");
      print_rtl (file, iv->delta);
    }
  if (iv->first_special)
    fprintf (file, " (first special)");
}

/* Assigns luids to insns in basic block BB.  */

static void
assign_luids (basic_block bb)
{
  unsigned i = 0, uid;
  rtx insn;

  FOR_BB_INSNS (bb, insn)
    {
      uid = INSN_UID (insn);
      insn_info[uid].luid = i++;
      insn_info[uid].prev_def = NULL_RTX;
      insn_info[uid].iv.analysed = false;
    }
}

/* Generates a subreg to get the least significant part of EXPR (in mode
   INNER_MODE) to OUTER_MODE.  */

rtx
lowpart_subreg (enum machine_mode outer_mode, rtx expr,
		enum machine_mode inner_mode)
{
  return simplify_gen_subreg (outer_mode, expr, inner_mode,
			      subreg_lowpart_offset (outer_mode, inner_mode));
}

/* Checks whether REG is a well-behaved register.  */

static bool
simple_reg_p (rtx reg)
{
  unsigned r;

  if (GET_CODE (reg) == SUBREG)
    {
      if (!subreg_lowpart_p (reg))
	return false;
      reg = SUBREG_REG (reg);
    }

  if (!REG_P (reg))
    return false;

  r = REGNO (reg);
  if (HARD_REGISTER_NUM_P (r))
    return false;

  if (GET_MODE_CLASS (GET_MODE (reg)) != MODE_INT)
    return false;

  if (last_def[r] == const0_rtx)
    return false;

  return true;
}

/* Checks whether assignment LHS = RHS is simple enough for us to process.  */

static bool
simple_set_p (rtx lhs, rtx rhs)
{
  rtx op0, op1;

  if (!REG_P (lhs)
      || !simple_reg_p (lhs))
    return false;

  if (CONSTANT_P (rhs))
    return true;

  switch (GET_CODE (rhs))
    {
    case SUBREG:
    case REG:
      return simple_reg_p (rhs);

    case SIGN_EXTEND:
    case ZERO_EXTEND:
    case NEG:
      return simple_reg_p (XEXP (rhs, 0));

    case PLUS:
    case MINUS:
    case MULT:
    case ASHIFT:
      op0 = XEXP (rhs, 0);
      op1 = XEXP (rhs, 1);

      if (!simple_reg_p (op0)
	  && !CONSTANT_P (op0))
	return false;

      if (!simple_reg_p (op1)
	  && !CONSTANT_P (op1))
	return false;

      if (GET_CODE (rhs) == MULT
	  && !CONSTANT_P (op0)
	  && !CONSTANT_P (op1))
	return false;

      if (GET_CODE (rhs) == ASHIFT
	  && CONSTANT_P (op0))
	return false;

      return true;

    default:
      return false;
    }
}

/* Mark single SET in INSN.  */

static rtx
mark_single_set (rtx insn, rtx set)
{
  rtx def = SET_DEST (set), src;
  unsigned regno, uid;

  src = find_reg_equal_equiv_note (insn);
  if (src)
    src = XEXP (src, 0);
  else
    src = SET_SRC (set);

  if (!simple_set_p (SET_DEST (set), src))
    return NULL_RTX;

  regno = REGNO (def);
  uid = INSN_UID (insn);

  bivs[regno].analysed = false;
  insn_info[uid].prev_def = last_def[regno];
  last_def[regno] = insn;

  return def;
}

/* Invalidate register REG unless it is equal to EXCEPT.  */

static void
kill_sets (rtx reg, rtx by ATTRIBUTE_UNUSED, void *except)
{
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  if (!REG_P (reg))
    return;
  if (reg == except)
    return;

  last_def[REGNO (reg)] = const0_rtx;
}

/* Marks sets in basic block BB.  If DOM is true, BB dominates the loop
   latch.  */

static void
mark_sets (basic_block bb, bool dom)
{
  rtx insn, set, def;

  FOR_BB_INSNS (bb, insn)
    {
      if (!INSN_P (insn))
	continue;

      if (dom
	  && (set = single_set (insn)))
	def = mark_single_set (insn, set);
      else
	def = NULL_RTX;

      note_stores (PATTERN (insn), kill_sets, def);
    }
}

/* Prepare the data for an induction variable analysis of a LOOP.  */

void
iv_analysis_loop_init (struct loop *loop)
{
  basic_block *body = get_loop_body_in_dom_order (loop);
  unsigned b;

  if ((unsigned) get_max_uid () >= max_insn_no)
    {
      /* Add some reserve for insns and registers produced in optimizations.  */
      max_insn_no = get_max_uid () + 100;
      if (insn_info)
	free (insn_info);
      insn_info = xmalloc (max_insn_no * sizeof (struct insn_info));
    }

  if ((unsigned) max_reg_num () >= max_reg_no)
    {
      max_reg_no = max_reg_num () + 100;
      if (last_def)
	free (last_def);
      last_def = xmalloc (max_reg_no * sizeof (rtx));
      if (bivs)
	free (bivs);
      bivs = xmalloc (max_reg_no * sizeof (struct rtx_iv));
    }

  memset (last_def, 0, max_reg_num () * sizeof (rtx));

  for (b = 0; b < loop->num_nodes; b++)
    {
      assign_luids (body[b]);
      mark_sets (body[b], just_once_each_iteration_p (loop, body[b]));
    }

  free (body);
}

/* Gets definition of REG reaching the INSN.  If REG is not simple, const0_rtx
   is returned.  If INSN is before the first def in the loop, NULL_RTX is
   returned.  */

rtx
iv_get_reaching_def (rtx insn, rtx reg)
{
  unsigned regno, luid, auid;
  rtx ainsn;
  basic_block bb, abb;

  if (GET_CODE (reg) == SUBREG)
    {
      if (!subreg_lowpart_p (reg))
	return const0_rtx;
      reg = SUBREG_REG (reg);
    }
  if (!REG_P (reg))
    return NULL_RTX;

  regno = REGNO (reg);
  if (!last_def[regno]
      || last_def[regno] == const0_rtx)
    return last_def[regno];

  bb = BLOCK_FOR_INSN (insn);
  luid = insn_info[INSN_UID (insn)].luid;

  ainsn = last_def[regno];
  while (1)
    {
      abb = BLOCK_FOR_INSN (ainsn);

      if (dominated_by_p (CDI_DOMINATORS, bb, abb))
	break;

      auid = INSN_UID (ainsn);
      ainsn = insn_info[auid].prev_def;

      if (!ainsn)
	return NULL_RTX;
    }

  while (1)
    {
      abb = BLOCK_FOR_INSN (ainsn);
      if (abb != bb)
	return ainsn;

      auid = INSN_UID (ainsn);
      if (luid > insn_info[auid].luid)
	return ainsn;

      ainsn = insn_info[auid].prev_def;
      if (!ainsn)
	return NULL_RTX;
    }
}

/* Sets IV to invariant CST in MODE.  Always returns true (just for
   consistency with other iv manipulation functions that may fail).  */

static bool
iv_constant (struct rtx_iv *iv, rtx cst, enum machine_mode mode)
{
  if (mode == VOIDmode)
    mode = GET_MODE (cst);

  iv->analysed = true;
  iv->mode = mode;
  iv->base = cst;
  iv->step = const0_rtx;
  iv->first_special = false;
  iv->extend = UNKNOWN;
  iv->extend_mode = iv->mode;
  iv->delta = const0_rtx;
  iv->mult = const1_rtx;

  return true;
}

/* Evaluates application of subreg to MODE on IV.  */

static bool
iv_subreg (struct rtx_iv *iv, enum machine_mode mode)
{
  /* If iv is invariant, just calculate the new value.  */
  if (iv->step == const0_rtx
      && !iv->first_special)
    {
      rtx val = get_iv_value (iv, const0_rtx);
      val = lowpart_subreg (mode, val, iv->extend_mode);

      iv->base = val;
      iv->extend = UNKNOWN;
      iv->mode = iv->extend_mode = mode;
      iv->delta = const0_rtx;
      iv->mult = const1_rtx;
      return true;
    }

  if (iv->extend_mode == mode)
    return true;

  if (GET_MODE_BITSIZE (mode) > GET_MODE_BITSIZE (iv->mode))
    return false;

  iv->extend = UNKNOWN;
  iv->mode = mode;

  iv->base = simplify_gen_binary (PLUS, iv->extend_mode, iv->delta,
				  simplify_gen_binary (MULT, iv->extend_mode,
						       iv->base, iv->mult));
  iv->step = simplify_gen_binary (MULT, iv->extend_mode, iv->step, iv->mult);
  iv->mult = const1_rtx;
  iv->delta = const0_rtx;
  iv->first_special = false;

  return true;
}

/* Evaluates application of EXTEND to MODE on IV.  */

static bool
iv_extend (struct rtx_iv *iv, enum rtx_code extend, enum machine_mode mode)
{
  /* If iv is invariant, just calculate the new value.  */
  if (iv->step == const0_rtx
      && !iv->first_special)
    {
      rtx val = get_iv_value (iv, const0_rtx);
      val = simplify_gen_unary (extend, mode, val, iv->extend_mode);

      iv->base = val;
      iv->extend = UNKNOWN;
      iv->mode = iv->extend_mode = mode;
      iv->delta = const0_rtx;
      iv->mult = const1_rtx;
      return true;
    }

  if (mode != iv->extend_mode)
    return false;

  if (iv->extend != UNKNOWN
      && iv->extend != extend)
    return false;

  iv->extend = extend;

  return true;
}

/* Evaluates negation of IV.  */

static bool
iv_neg (struct rtx_iv *iv)
{
  if (iv->extend == UNKNOWN)
    {
      iv->base = simplify_gen_unary (NEG, iv->extend_mode,
				     iv->base, iv->extend_mode);
      iv->step = simplify_gen_unary (NEG, iv->extend_mode,
				     iv->step, iv->extend_mode);
    }
  else
    {
      iv->delta = simplify_gen_unary (NEG, iv->extend_mode,
				      iv->delta, iv->extend_mode);
      iv->mult = simplify_gen_unary (NEG, iv->extend_mode,
				     iv->mult, iv->extend_mode);
    }

  return true;
}

/* Evaluates addition or subtraction (according to OP) of IV1 to IV0.  */

static bool
iv_add (struct rtx_iv *iv0, struct rtx_iv *iv1, enum rtx_code op)
{
  enum machine_mode mode;
  rtx arg;

  /* Extend the constant to extend_mode of the other operand if necessary.  */
  if (iv0->extend == UNKNOWN
      && iv0->mode == iv0->extend_mode
      && iv0->step == const0_rtx
      && GET_MODE_SIZE (iv0->extend_mode) < GET_MODE_SIZE (iv1->extend_mode))
    {
      iv0->extend_mode = iv1->extend_mode;
      iv0->base = simplify_gen_unary (ZERO_EXTEND, iv0->extend_mode,
				      iv0->base, iv0->mode);
    }
  if (iv1->extend == UNKNOWN
      && iv1->mode == iv1->extend_mode
      && iv1->step == const0_rtx
      && GET_MODE_SIZE (iv1->extend_mode) < GET_MODE_SIZE (iv0->extend_mode))
    {
      iv1->extend_mode = iv0->extend_mode;
      iv1->base = simplify_gen_unary (ZERO_EXTEND, iv1->extend_mode,
				      iv1->base, iv1->mode);
    }

  mode = iv0->extend_mode;
  if (mode != iv1->extend_mode)
    return false;

  if (iv0->extend == UNKNOWN && iv1->extend == UNKNOWN)
    {
      if (iv0->mode != iv1->mode)
	return false;

      iv0->base = simplify_gen_binary (op, mode, iv0->base, iv1->base);
      iv0->step = simplify_gen_binary (op, mode, iv0->step, iv1->step);

      return true;
    }

  /* Handle addition of constant.  */
  if (iv1->extend == UNKNOWN
      && iv1->mode == mode
      && iv1->step == const0_rtx)
    {
      iv0->delta = simplify_gen_binary (op, mode, iv0->delta, iv1->base);
      return true;
    }

  if (iv0->extend == UNKNOWN
      && iv0->mode == mode
      && iv0->step == const0_rtx)
    {
      arg = iv0->base;
      *iv0 = *iv1;
      if (op == MINUS
	  && !iv_neg (iv0))
	return false;

      iv0->delta = simplify_gen_binary (PLUS, mode, iv0->delta, arg);
      return true;
    }

  return false;
}

/* Evaluates multiplication of IV by constant CST.  */

static bool
iv_mult (struct rtx_iv *iv, rtx mby)
{
  enum machine_mode mode = iv->extend_mode;

  if (GET_MODE (mby) != VOIDmode
      && GET_MODE (mby) != mode)
    return false;

  if (iv->extend == UNKNOWN)
    {
      iv->base = simplify_gen_binary (MULT, mode, iv->base, mby);
      iv->step = simplify_gen_binary (MULT, mode, iv->step, mby);
    }
  else
    {
      iv->delta = simplify_gen_binary (MULT, mode, iv->delta, mby);
      iv->mult = simplify_gen_binary (MULT, mode, iv->mult, mby);
    }

  return true;
}

/* Evaluates shift of IV by constant CST.  */

static bool
iv_shift (struct rtx_iv *iv, rtx mby)
{
  enum machine_mode mode = iv->extend_mode;

  if (GET_MODE (mby) != VOIDmode
      && GET_MODE (mby) != mode)
    return false;

  if (iv->extend == UNKNOWN)
    {
      iv->base = simplify_gen_binary (ASHIFT, mode, iv->base, mby);
      iv->step = simplify_gen_binary (ASHIFT, mode, iv->step, mby);
    }
  else
    {
      iv->delta = simplify_gen_binary (ASHIFT, mode, iv->delta, mby);
      iv->mult = simplify_gen_binary (ASHIFT, mode, iv->mult, mby);
    }

  return true;
}

/* The recursive part of get_biv_step.  Gets the value of the single value
   defined in INSN wrto initial value of REG inside loop, in shape described
   at get_biv_step.  */

static bool
get_biv_step_1 (rtx insn, rtx reg,
		rtx *inner_step, enum machine_mode *inner_mode,
		enum rtx_code *extend, enum machine_mode outer_mode,
		rtx *outer_step)
{
  rtx set, lhs, rhs, op0 = NULL_RTX, op1 = NULL_RTX;
  rtx next, nextr, def_insn, tmp;
  enum rtx_code code;

  set = single_set (insn);
  rhs = find_reg_equal_equiv_note (insn);
  if (rhs)
    rhs = XEXP (rhs, 0);
  else
    rhs = SET_SRC (set);
  lhs = SET_DEST (set);

  code = GET_CODE (rhs);
  switch (code)
    {
    case SUBREG:
    case REG:
      next = rhs;
      break;

    case PLUS:
    case MINUS:
      op0 = XEXP (rhs, 0);
      op1 = XEXP (rhs, 1);

      if (code == PLUS && CONSTANT_P (op0))
	{
	  tmp = op0; op0 = op1; op1 = tmp;
	}

      if (!simple_reg_p (op0)
	  || !CONSTANT_P (op1))
	return false;

      if (GET_MODE (rhs) != outer_mode)
	{
	  /* ppc64 uses expressions like

	     (set x:SI (plus:SI (subreg:SI y:DI) 1)).

	     this is equivalent to

	     (set x':DI (plus:DI y:DI 1))
	     (set x:SI (subreg:SI (x':DI)).  */
	  if (GET_CODE (op0) != SUBREG)
	    return false;
	  if (GET_MODE (SUBREG_REG (op0)) != outer_mode)
	    return false;
	}

      next = op0;
      break;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (GET_MODE (rhs) != outer_mode)
	return false;

      op0 = XEXP (rhs, 0);
      if (!simple_reg_p (op0))
	return false;

      next = op0;
      break;

    default:
      return false;
    }

  if (GET_CODE (next) == SUBREG)
    {
      if (!subreg_lowpart_p (next))
	return false;

      nextr = SUBREG_REG (next);
      if (GET_MODE (nextr) != outer_mode)
	return false;
    }
  else
    nextr = next;

  def_insn = iv_get_reaching_def (insn, nextr);
  if (def_insn == const0_rtx)
    return false;

  if (!def_insn)
    {
      if (!rtx_equal_p (nextr, reg))
	return false;

      *inner_step = const0_rtx;
      *extend = UNKNOWN;
      *inner_mode = outer_mode;
      *outer_step = const0_rtx;
    }
  else if (!get_biv_step_1 (def_insn, reg,
			    inner_step, inner_mode, extend, outer_mode,
			    outer_step))
    return false;

  if (GET_CODE (next) == SUBREG)
    {
      enum machine_mode amode = GET_MODE (next);

      if (GET_MODE_SIZE (amode) > GET_MODE_SIZE (*inner_mode))
	return false;

      *inner_mode = amode;
      *inner_step = simplify_gen_binary (PLUS, outer_mode,
					 *inner_step, *outer_step);
      *outer_step = const0_rtx;
      *extend = UNKNOWN;
    }

  switch (code)
    {
    case REG:
    case SUBREG:
      break;

    case PLUS:
    case MINUS:
      if (*inner_mode == outer_mode
	  /* See comment in previous switch.  */
	  || GET_MODE (rhs) != outer_mode)
	*inner_step = simplify_gen_binary (code, outer_mode,
					   *inner_step, op1);
      else
	*outer_step = simplify_gen_binary (code, outer_mode,
					   *outer_step, op1);
      break;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (GET_MODE (op0) != *inner_mode
	  || *extend != UNKNOWN
	  || *outer_step != const0_rtx)
	abort ();

      *extend = code;
      break;

    default:
      abort ();
    }

  return true;
}

/* Gets the operation on register REG inside loop, in shape

   OUTER_STEP + EXTEND_{OUTER_MODE} (SUBREG_{INNER_MODE} (REG + INNER_STEP))

   If the operation cannot be described in this shape, return false.  */

static bool
get_biv_step (rtx reg, rtx *inner_step, enum machine_mode *inner_mode,
	      enum rtx_code *extend, enum machine_mode *outer_mode,
	      rtx *outer_step)
{
  *outer_mode = GET_MODE (reg);

  if (!get_biv_step_1 (last_def[REGNO (reg)], reg,
		       inner_step, inner_mode, extend, *outer_mode,
		       outer_step))
    return false;

  if (*inner_mode != *outer_mode
      && *extend == UNKNOWN)
    abort ();

  if (*inner_mode == *outer_mode
      && *extend != UNKNOWN)
    abort ();

  if (*inner_mode == *outer_mode
      && *outer_step != const0_rtx)
    abort ();

  return true;
}

/* Determines whether DEF is a biv and if so, stores its description
   to *IV.  */

static bool
iv_analyze_biv (rtx def, struct rtx_iv *iv)
{
  unsigned regno;
  rtx inner_step, outer_step;
  enum machine_mode inner_mode, outer_mode;
  enum rtx_code extend;

  if (dump_file)
    {
      fprintf (dump_file, "Analysing ");
      print_rtl (dump_file, def);
      fprintf (dump_file, " for bivness.\n");
    }
    
  if (!REG_P (def))
    {
      if (!CONSTANT_P (def))
	return false;

      return iv_constant (iv, def, VOIDmode);
    }

  regno = REGNO (def);
  if (last_def[regno] == const0_rtx)
    {
      if (dump_file)
	fprintf (dump_file, "  not simple.\n");
      return false;
    }

  if (last_def[regno] && bivs[regno].analysed)
    {
      if (dump_file)
	fprintf (dump_file, "  already analysed.\n");

      *iv = bivs[regno];
      return iv->base != NULL_RTX;
    }

  if (!last_def[regno])
    {
      iv_constant (iv, def, VOIDmode);
      goto end;
    }

  iv->analysed = true;
  if (!get_biv_step (def, &inner_step, &inner_mode, &extend,
		     &outer_mode, &outer_step))
    {
      iv->base = NULL_RTX;
      goto end;
    }

  /* Loop transforms base to es (base + inner_step) + outer_step,
     where es means extend of subreg between inner_mode and outer_mode.
     The corresponding induction variable is

     es ((base - outer_step) + i * (inner_step + outer_step)) + outer_step  */

  iv->base = simplify_gen_binary (MINUS, outer_mode, def, outer_step);
  iv->step = simplify_gen_binary (PLUS, outer_mode, inner_step, outer_step);
  iv->mode = inner_mode;
  iv->extend_mode = outer_mode;
  iv->extend = extend;
  iv->mult = const1_rtx;
  iv->delta = outer_step;
  iv->first_special = inner_mode != outer_mode;

 end:
  if (dump_file)
    {
      fprintf (dump_file, "  ");
      dump_iv_info (dump_file, iv);
      fprintf (dump_file, "\n");
    }

  bivs[regno] = *iv;

  return iv->base != NULL_RTX;
}

/* Analyzes operand OP of INSN and stores the result to *IV.  */

static bool
iv_analyze_op (rtx insn, rtx op, struct rtx_iv *iv)
{
  rtx def_insn;
  unsigned regno;
  bool inv = CONSTANT_P (op);

  if (dump_file)
    {
      fprintf (dump_file, "Analysing operand ");
      print_rtl (dump_file, op);
      fprintf (dump_file, " of insn ");
      print_rtl_single (dump_file, insn);
    }

  if (GET_CODE (op) == SUBREG)
    {
      if (!subreg_lowpart_p (op))
	return false;

      if (!iv_analyze_op (insn, SUBREG_REG (op), iv))
	return false;

      return iv_subreg (iv, GET_MODE (op));
    }

  if (!inv)
    {
      regno = REGNO (op);
      if (!last_def[regno])
	inv = true;
      else if (last_def[regno] == const0_rtx)
	{
	  if (dump_file)
	    fprintf (dump_file, "  not simple.\n");
	  return false;
	}
    }

  if (inv)
    {
      iv_constant (iv, op, VOIDmode);

      if (dump_file)
	{
	  fprintf (dump_file, "  ");
	  dump_iv_info (dump_file, iv);
	  fprintf (dump_file, "\n");
	}
      return true;
    }

  def_insn = iv_get_reaching_def (insn, op);
  if (def_insn == const0_rtx)
    {
      if (dump_file)
	fprintf (dump_file, "  not simple.\n");
      return false;
    }

  return iv_analyze (def_insn, op, iv);
}

/* Analyzes iv DEF defined in INSN and stores the result to *IV.  */

bool
iv_analyze (rtx insn, rtx def, struct rtx_iv *iv)
{
  unsigned uid;
  rtx set, rhs, mby = NULL_RTX, tmp;
  rtx op0 = NULL_RTX, op1 = NULL_RTX;
  struct rtx_iv iv0, iv1;
  enum machine_mode amode;
  enum rtx_code code;

  if (insn == const0_rtx)
    return false;

  if (GET_CODE (def) == SUBREG)
    {
      if (!subreg_lowpart_p (def))
	return false;

      if (!iv_analyze (insn, SUBREG_REG (def), iv))
	return false;

      return iv_subreg (iv, GET_MODE (def));
    }

  if (!insn)
    return iv_analyze_biv (def, iv);

  if (dump_file)
    {
      fprintf (dump_file, "Analysing def of ");
      print_rtl (dump_file, def);
      fprintf (dump_file, " in insn ");
      print_rtl_single (dump_file, insn);
    }

  uid = INSN_UID (insn);
  if (insn_info[uid].iv.analysed)
    {
      if (dump_file)
	fprintf (dump_file, "  already analysed.\n");
      *iv = insn_info[uid].iv;
      return iv->base != NULL_RTX;
    }

  iv->mode = VOIDmode;
  iv->base = NULL_RTX;
  iv->step = NULL_RTX;

  set = single_set (insn);
  rhs = find_reg_equal_equiv_note (insn);
  if (rhs)
    rhs = XEXP (rhs, 0);
  else
    rhs = SET_SRC (set);
  code = GET_CODE (rhs);

  if (CONSTANT_P (rhs))
    {
      op0 = rhs;
      amode = GET_MODE (def);
    }
  else
    {
      switch (code)
	{
	case SUBREG:
	  if (!subreg_lowpart_p (rhs))
	    goto end;
	  op0 = rhs;
	  break;
	  
	case REG:
	  op0 = rhs;
	  break;

	case SIGN_EXTEND:
	case ZERO_EXTEND:
	case NEG:
	  op0 = XEXP (rhs, 0);
	  break;

	case PLUS:
	case MINUS:
	  op0 = XEXP (rhs, 0);
	  op1 = XEXP (rhs, 1);
	  break;

	case MULT:
	  op0 = XEXP (rhs, 0);
	  mby = XEXP (rhs, 1);
	  if (!CONSTANT_P (mby))
	    {
	      if (!CONSTANT_P (op0))
		abort ();
	      tmp = op0;
	      op0 = mby;
	      mby = tmp;
	    }
	  break;

	case ASHIFT:
	  if (CONSTANT_P (XEXP (rhs, 0)))
	    abort ();
	  op0 = XEXP (rhs, 0);
	  mby = XEXP (rhs, 1);
	  break;

	default:
	  abort ();
	}

      amode = GET_MODE (rhs);
    }

  if (op0)
    {
      if (!iv_analyze_op (insn, op0, &iv0))
	goto end;
	
      if (iv0.mode == VOIDmode)
	{
	  iv0.mode = amode;
	  iv0.extend_mode = amode;
	}
    }

  if (op1)
    {
      if (!iv_analyze_op (insn, op1, &iv1))
	goto end;

      if (iv1.mode == VOIDmode)
	{
	  iv1.mode = amode;
	  iv1.extend_mode = amode;
	}
    }

  switch (code)
    {
    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (!iv_extend (&iv0, code, amode))
	goto end;
      break;

    case NEG:
      if (!iv_neg (&iv0))
	goto end;
      break;

    case PLUS:
    case MINUS:
      if (!iv_add (&iv0, &iv1, code))
	goto end;
      break;

    case MULT:
      if (!iv_mult (&iv0, mby))
	goto end;
      break;

    case ASHIFT:
      if (!iv_shift (&iv0, mby))
	goto end;
      break;

    default:
      break;
    }

  *iv = iv0;

 end:
  iv->analysed = true;
  insn_info[uid].iv = *iv;

  if (dump_file)
    {
      print_rtl (dump_file, def);
      fprintf (dump_file, " in insn ");
      print_rtl_single (dump_file, insn);
      fprintf (dump_file, "  is ");
      dump_iv_info (dump_file, iv);
      fprintf (dump_file, "\n");
    }

  return iv->base != NULL_RTX;
}

/* Checks whether definition of register REG in INSN a basic induction
   variable.  IV analysis must have been initialized (via a call to
   iv_analysis_loop_init) for this function to produce a result.  */

bool
biv_p (rtx insn, rtx reg)
{
  struct rtx_iv iv;

  if (!REG_P (reg))
    return false;

  if (last_def[REGNO (reg)] != insn)
    return false;

  return iv_analyze_biv (reg, &iv);
}

/* Calculates value of IV at ITERATION-th iteration.  */

rtx
get_iv_value (struct rtx_iv *iv, rtx iteration)
{
  rtx val;

  /* We would need to generate some if_then_else patterns, and so far
     it is not needed anywhere.  */
  if (iv->first_special)
    abort ();

  if (iv->step != const0_rtx && iteration != const0_rtx)
    val = simplify_gen_binary (PLUS, iv->extend_mode, iv->base,
			       simplify_gen_binary (MULT, iv->extend_mode,
						    iv->step, iteration));
  else
    val = iv->base;

  if (iv->extend_mode == iv->mode)
    return val;

  val = lowpart_subreg (iv->mode, val, iv->extend_mode);

  if (iv->extend == UNKNOWN)
    return val;

  val = simplify_gen_unary (iv->extend, iv->extend_mode, val, iv->mode);
  val = simplify_gen_binary (PLUS, iv->extend_mode, iv->delta,
			     simplify_gen_binary (MULT, iv->extend_mode,
						  iv->mult, val));

  return val;
}

/* Free the data for an induction variable analysis.  */

void
iv_analysis_done (void)
{
  max_insn_no = 0;
  max_reg_no = 0;
  if (insn_info)
    {
      free (insn_info);
      insn_info = NULL;
    }
  if (last_def)
    {
      free (last_def);
      last_def = NULL;
    }
  if (bivs)
    {
      free (bivs);
      bivs = NULL;
    }
}

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

/* Tries to estimate the maximum number of iterations.  */

static unsigned HOST_WIDEST_INT
determine_max_iter (struct niter_desc *desc)
{
  rtx niter = desc->niter_expr;
  rtx mmin, mmax, left, right;
  unsigned HOST_WIDEST_INT nmax, inc;

  if (GET_CODE (niter) == AND
      && GET_CODE (XEXP (niter, 0)) == CONST_INT)
    {
      nmax = INTVAL (XEXP (niter, 0));
      if (!(nmax & (nmax + 1)))
	{
	  desc->niter_max = nmax;
	  return nmax;
	}
    }

  get_mode_bounds (desc->mode, desc->signed_p, desc->mode, &mmin, &mmax);
  nmax = INTVAL (mmax) - INTVAL (mmin);

  if (GET_CODE (niter) == UDIV)
    {
      if (GET_CODE (XEXP (niter, 1)) != CONST_INT)
	{
	  desc->niter_max = nmax;
	  return nmax;
	}
      inc = INTVAL (XEXP (niter, 1));
      niter = XEXP (niter, 0);
    }
  else
    inc = 1;

  if (GET_CODE (niter) == PLUS)
    {
      left = XEXP (niter, 0);
      right = XEXP (niter, 0);

      if (GET_CODE (right) == CONST_INT)
	right = GEN_INT (-INTVAL (right));
    }
  else if (GET_CODE (niter) == MINUS)
    {
      left = XEXP (niter, 0);
      right = XEXP (niter, 0);
    }
  else
    {
      left = niter;
      right = mmin;
    }

  if (GET_CODE (left) == CONST_INT)
    mmax = left;
  if (GET_CODE (right) == CONST_INT)
    mmin = right;
  nmax = INTVAL (mmax) - INTVAL (mmin);

  desc->niter_max = nmax / inc;
  return nmax / inc;
}

/* Checks whether register *REG is in set ALT.  Callback for for_each_rtx.  */

static int
altered_reg_used (rtx *reg, void *alt)
{
  if (!REG_P (*reg))
    return 0;

  return REGNO_REG_SET_P (alt, REGNO (*reg));
}

/* Marks registers altered by EXPR in set ALT.  */

static void
mark_altered (rtx expr, rtx by ATTRIBUTE_UNUSED, void *alt)
{
  if (GET_CODE (expr) == SUBREG)
    expr = SUBREG_REG (expr);
  if (!REG_P (expr))
    return;

  SET_REGNO_REG_SET (alt, REGNO (expr));
}

/* Checks whether RHS is simple enough to process.  */

static bool
simple_rhs_p (rtx rhs)
{
  rtx op0, op1;

  if (CONSTANT_P (rhs)
      || REG_P (rhs))
    return true;

  switch (GET_CODE (rhs))
    {
    case PLUS:
    case MINUS:
      op0 = XEXP (rhs, 0);
      op1 = XEXP (rhs, 1);
      /* Allow reg + const sets only.  */
      if (REG_P (op0) && CONSTANT_P (op1))
	return true;
      if (REG_P (op1) && CONSTANT_P (op0))
	return true;

      return false;

    default:
      return false;
    }
}

/* Simplifies *EXPR using assignment in INSN.  ALTERED is the set of registers
   altered so far.  */

static void
simplify_using_assignment (rtx insn, rtx *expr, regset altered)
{
  rtx set = single_set (insn);
  rtx lhs = NULL_RTX, rhs;
  bool ret = false;

  if (set)
    {
      lhs = SET_DEST (set);
      if (!REG_P (lhs)
	  || altered_reg_used (&lhs, altered))
	ret = true;
    }
  else
    ret = true;

  note_stores (PATTERN (insn), mark_altered, altered);
  if (CALL_P (insn))
    {
      int i;

      /* Kill all call clobbered registers.  */
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (regs_invalidated_by_call, i))
	  SET_REGNO_REG_SET (altered, i);
    }

  if (ret)
    return;

  rhs = find_reg_equal_equiv_note (insn);
  if (rhs)
    rhs = XEXP (rhs, 0);
  else
    rhs = SET_SRC (set);

  if (!simple_rhs_p (rhs))
    return;

  if (for_each_rtx (&rhs, altered_reg_used, altered))
    return;

  *expr = simplify_replace_rtx (*expr, lhs, rhs);
}

/* Checks whether A implies B.  */

static bool
implies_p (rtx a, rtx b)
{
  rtx op0, op1, opb0, opb1, r;
  enum machine_mode mode;

  if (GET_CODE (a) == EQ)
    {
      op0 = XEXP (a, 0);
      op1 = XEXP (a, 1);

      if (REG_P (op0))
	{
	  r = simplify_replace_rtx (b, op0, op1);
	  if (r == const_true_rtx)
	    return true;
	}

      if (REG_P (op1))
	{
	  r = simplify_replace_rtx (b, op1, op0);
	  if (r == const_true_rtx)
	    return true;
	}
    }

  /* A < B implies A + 1 <= B.  */
  if ((GET_CODE (a) == GT || GET_CODE (a) == LT)
      && (GET_CODE (b) == GE || GET_CODE (b) == LE))
    {
      op0 = XEXP (a, 0);
      op1 = XEXP (a, 1);
      opb0 = XEXP (b, 0);
      opb1 = XEXP (b, 1);

      if (GET_CODE (a) == GT)
	{
	  r = op0;
	  op0 = op1;
	  op1 = r;
	}

      if (GET_CODE (b) == GE)
	{
	  r = opb0;
	  opb0 = opb1;
	  opb1 = r;
	}

      mode = GET_MODE (op0);
      if (mode != GET_MODE (opb0))
	mode = VOIDmode;
      else if (mode == VOIDmode)
	{
	  mode = GET_MODE (op1);
	  if (mode != GET_MODE (opb1))
	    mode = VOIDmode;
	}

      if (mode != VOIDmode
	  && rtx_equal_p (op1, opb1)
	  && simplify_gen_binary (MINUS, mode, opb0, op0) == const1_rtx)
	return true;
    }

  return false;
}

/* Canonicalizes COND so that

   (1) Ensure that operands are ordered according to
       swap_commutative_operands_p.
   (2) (LE x const) will be replaced with (LT x <const+1>) and similarly
       for GE, GEU, and LEU.  */

rtx
canon_condition (rtx cond)
{
  rtx tem;
  rtx op0, op1;
  enum rtx_code code;
  enum machine_mode mode;

  code = GET_CODE (cond);
  op0 = XEXP (cond, 0);
  op1 = XEXP (cond, 1);

  if (swap_commutative_operands_p (op0, op1))
    {
      code = swap_condition (code);
      tem = op0;
      op0 = op1;
      op1 = tem;
    }

  mode = GET_MODE (op0);
  if (mode == VOIDmode)
    mode = GET_MODE (op1);
  if (mode == VOIDmode)
    abort ();

  if (GET_CODE (op1) == CONST_INT
      && GET_MODE_CLASS (mode) != MODE_CC
      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT const_val = INTVAL (op1);
      unsigned HOST_WIDE_INT uconst_val = const_val;
      unsigned HOST_WIDE_INT max_val
	= (unsigned HOST_WIDE_INT) GET_MODE_MASK (mode);

      switch (code)
	{
	case LE:
	  if ((unsigned HOST_WIDE_INT) const_val != max_val >> 1)
	    code = LT, op1 = gen_int_mode (const_val + 1, GET_MODE (op0));
	  break;

	/* When cross-compiling, const_val might be sign-extended from
	   BITS_PER_WORD to HOST_BITS_PER_WIDE_INT */
	case GE:
	  if ((HOST_WIDE_INT) (const_val & max_val)
	      != (((HOST_WIDE_INT) 1
		   << (GET_MODE_BITSIZE (GET_MODE (op0)) - 1))))
	    code = GT, op1 = gen_int_mode (const_val - 1, mode);
	  break;

	case LEU:
	  if (uconst_val < max_val)
	    code = LTU, op1 = gen_int_mode (uconst_val + 1, mode);
	  break;

	case GEU:
	  if (uconst_val != 0)
	    code = GTU, op1 = gen_int_mode (uconst_val - 1, mode);
	  break;

	default:
	  break;
	}
    }

  if (op0 != XEXP (cond, 0)
      || op1 != XEXP (cond, 1)
      || code != GET_CODE (cond)
      || GET_MODE (cond) != SImode)
    cond = gen_rtx_fmt_ee (code, SImode, op0, op1);

  return cond;
}

/* Tries to use the fact that COND holds to simplify EXPR.  ALTERED is the
   set of altered regs.  */

void
simplify_using_condition (rtx cond, rtx *expr, regset altered)
{
  rtx rev, reve, exp = *expr;

  if (!COMPARISON_P (exp))
    return;

  /* If some register gets altered later, we do not really speak about its
     value at the time of comparison.  */
  if (altered
      && for_each_rtx (&cond, altered_reg_used, altered))
    return;

  rev = reversed_condition (cond);
  reve = reversed_condition (exp);

  cond = canon_condition (cond);
  exp = canon_condition (exp);
  if (rev)
    rev = canon_condition (rev);
  if (reve)
    reve = canon_condition (reve);

  if (rtx_equal_p (exp, cond))
    {
      *expr = const_true_rtx;
      return;
    }


  if (rev && rtx_equal_p (exp, rev))
    {
      *expr = const0_rtx;
      return;
    }

  if (implies_p (cond, exp))
    {
      *expr = const_true_rtx;
      return;
    }
  
  if (reve && implies_p (cond, reve))
    {
      *expr = const0_rtx;
      return;
    }

  /* A proof by contradiction.  If *EXPR implies (not cond), *EXPR must
     be false.  */
  if (rev && implies_p (exp, rev))
    {
      *expr = const0_rtx;
      return;
    }

  /* Similarly, If (not *EXPR) implies (not cond), *EXPR must be true.  */
  if (rev && reve && implies_p (reve, rev))
    {
      *expr = const_true_rtx;
      return;
    }

  /* We would like to have some other tests here.  TODO.  */

  return;
}

/* Use relationship between A and *B to eventually eliminate *B.
   OP is the operation we consider.  */

static void
eliminate_implied_condition (enum rtx_code op, rtx a, rtx *b)
{
  if (op == AND)
    {
      /* If A implies *B, we may replace *B by true.  */
      if (implies_p (a, *b))
	*b = const_true_rtx;
    }
  else if (op == IOR)
    {
      /* If *B implies A, we may replace *B by false.  */
      if (implies_p (*b, a))
	*b = const0_rtx;
    }
  else
    abort ();
}

/* Eliminates the conditions in TAIL that are implied by HEAD.  OP is the
   operation we consider.  */

static void
eliminate_implied_conditions (enum rtx_code op, rtx *head, rtx tail)
{
  rtx elt;

  for (elt = tail; elt; elt = XEXP (elt, 1))
    eliminate_implied_condition (op, *head, &XEXP (elt, 0));
  for (elt = tail; elt; elt = XEXP (elt, 1))
    eliminate_implied_condition (op, XEXP (elt, 0), head);
}

/* Simplifies *EXPR using initial values at the start of the LOOP.  If *EXPR
   is a list, its elements are assumed to be combined using OP.  */

static void
simplify_using_initial_values (struct loop *loop, enum rtx_code op, rtx *expr)
{
  rtx head, tail, insn;
  rtx neutral, aggr;
  regset altered;
  edge e;

  if (!*expr)
    return;

  if (CONSTANT_P (*expr))
    return;

  if (GET_CODE (*expr) == EXPR_LIST)
    {
      head = XEXP (*expr, 0);
      tail = XEXP (*expr, 1);

      eliminate_implied_conditions (op, &head, tail);

      if (op == AND)
	{
	  neutral = const_true_rtx;
	  aggr = const0_rtx;
	}
      else if (op == IOR)
	{
	  neutral = const0_rtx;
	  aggr = const_true_rtx;
	}
      else
	abort ();

      simplify_using_initial_values (loop, UNKNOWN, &head);
      if (head == aggr)
	{
	  XEXP (*expr, 0) = aggr;
	  XEXP (*expr, 1) = NULL_RTX;
	  return;
	}
      else if (head == neutral)
	{
	  *expr = tail;
	  simplify_using_initial_values (loop, op, expr);
	  return;
	}
      simplify_using_initial_values (loop, op, &tail);

      if (tail && XEXP (tail, 0) == aggr)
	{
	  *expr = tail;
	  return;
	}
  
      XEXP (*expr, 0) = head;
      XEXP (*expr, 1) = tail;
      return;
    }

  if (op != UNKNOWN)
    abort ();

  e = loop_preheader_edge (loop);
  if (e->src == ENTRY_BLOCK_PTR)
    return;

  altered = ALLOC_REG_SET (&reg_obstack);

  while (1)
    {
      basic_block tmp_bb;

      insn = BB_END (e->src);
      if (any_condjump_p (insn))
	{
	  rtx cond = get_condition (BB_END (e->src), NULL, false, true);
      
	  if (cond && (e->flags & EDGE_FALLTHRU))
	    cond = reversed_condition (cond);
	  if (cond)
	    {
	      simplify_using_condition (cond, expr, altered);
	      if (CONSTANT_P (*expr))
		{
		  FREE_REG_SET (altered);
		  return;
		}
	    }
	}

      FOR_BB_INSNS_REVERSE (e->src, insn)
	{
	  if (!INSN_P (insn))
	    continue;
	    
	  simplify_using_assignment (insn, expr, altered);
	  if (CONSTANT_P (*expr))
	    {
	      FREE_REG_SET (altered);
	      return;
	    }
	}

      /* This is a bit subtle.  Store away e->src in tmp_bb, since we
	 modify `e' and this can invalidate the subsequent count of
	 e->src's predecessors by looking at the wrong block.  */
      tmp_bb = e->src;
      e = EDGE_PRED (tmp_bb, 0);
      if (EDGE_COUNT (tmp_bb->preds) > 1
	  || e->src == ENTRY_BLOCK_PTR)
	break;
    }

  FREE_REG_SET (altered);
}

/* Transforms invariant IV into MODE.  Adds assumptions based on the fact
   that IV occurs as left operands of comparison COND and its signedness
   is SIGNED_P to DESC.  */

static void
shorten_into_mode (struct rtx_iv *iv, enum machine_mode mode,
		   enum rtx_code cond, bool signed_p, struct niter_desc *desc)
{
  rtx mmin, mmax, cond_over, cond_under;

  get_mode_bounds (mode, signed_p, iv->extend_mode, &mmin, &mmax);
  cond_under = simplify_gen_relational (LT, SImode, iv->extend_mode,
					iv->base, mmin);
  cond_over = simplify_gen_relational (GT, SImode, iv->extend_mode,
				       iv->base, mmax);

  switch (cond)
    {
      case LE:
      case LT:
      case LEU:
      case LTU:
	if (cond_under != const0_rtx)
	  desc->infinite =
		  alloc_EXPR_LIST (0, cond_under, desc->infinite);
	if (cond_over != const0_rtx)
	  desc->noloop_assumptions =
		  alloc_EXPR_LIST (0, cond_over, desc->noloop_assumptions);
	break;

      case GE:
      case GT:
      case GEU:
      case GTU:
	if (cond_over != const0_rtx)
	  desc->infinite =
		  alloc_EXPR_LIST (0, cond_over, desc->infinite);
	if (cond_under != const0_rtx)
	  desc->noloop_assumptions =
		  alloc_EXPR_LIST (0, cond_under, desc->noloop_assumptions);
	break;

      case NE:
	if (cond_over != const0_rtx)
	  desc->infinite =
		  alloc_EXPR_LIST (0, cond_over, desc->infinite);
	if (cond_under != const0_rtx)
	  desc->infinite =
		  alloc_EXPR_LIST (0, cond_under, desc->infinite);
	break;

      default:
	abort ();
    }

  iv->mode = mode;
  iv->extend = signed_p ? SIGN_EXTEND : ZERO_EXTEND;
}

/* Transforms IV0 and IV1 compared by COND so that they are both compared as
   subregs of the same mode if possible (sometimes it is necessary to add
   some assumptions to DESC).  */

static bool
canonicalize_iv_subregs (struct rtx_iv *iv0, struct rtx_iv *iv1,
			 enum rtx_code cond, struct niter_desc *desc)
{
  enum machine_mode comp_mode;
  bool signed_p;

  /* If the ivs behave specially in the first iteration, or are
     added/multiplied after extending, we ignore them.  */
  if (iv0->first_special || iv0->mult != const1_rtx || iv0->delta != const0_rtx)
    return false;
  if (iv1->first_special || iv1->mult != const1_rtx || iv1->delta != const0_rtx)
    return false;

  /* If there is some extend, it must match signedness of the comparison.  */
  switch (cond)
    {
      case LE:
      case LT:
	if (iv0->extend == ZERO_EXTEND
	    || iv1->extend == ZERO_EXTEND)
	  return false;
	signed_p = true;
	break;

      case LEU:
      case LTU:
	if (iv0->extend == SIGN_EXTEND
	    || iv1->extend == SIGN_EXTEND)
	  return false;
	signed_p = false;
	break;

      case NE:
	if (iv0->extend != UNKNOWN
	    && iv1->extend != UNKNOWN
	    && iv0->extend != iv1->extend)
	  return false;

	signed_p = false;
	if (iv0->extend != UNKNOWN)
	  signed_p = iv0->extend == SIGN_EXTEND;
	if (iv1->extend != UNKNOWN)
	  signed_p = iv1->extend == SIGN_EXTEND;
	break;

      default:
	abort ();
    }

  /* Values of both variables should be computed in the same mode.  These
     might indeed be different, if we have comparison like

     (compare (subreg:SI (iv0)) (subreg:SI (iv1)))

     and iv0 and iv1 are both ivs iterating in SI mode, but calculated
     in different modes.  This does not seem impossible to handle, but
     it hardly ever occurs in practice.
     
     The only exception is the case when one of operands is invariant.
     For example pentium 3 generates comparisons like
     (lt (subreg:HI (reg:SI)) 100).  Here we assign HImode to 100, but we
     definitely do not want this prevent the optimization.  */
  comp_mode = iv0->extend_mode;
  if (GET_MODE_BITSIZE (comp_mode) < GET_MODE_BITSIZE (iv1->extend_mode))
    comp_mode = iv1->extend_mode;

  if (iv0->extend_mode != comp_mode)
    {
      if (iv0->mode != iv0->extend_mode
	  || iv0->step != const0_rtx)
	return false;

      iv0->base = simplify_gen_unary (signed_p ? SIGN_EXTEND : ZERO_EXTEND,
				      comp_mode, iv0->base, iv0->mode);
      iv0->extend_mode = comp_mode;
    }

  if (iv1->extend_mode != comp_mode)
    {
      if (iv1->mode != iv1->extend_mode
	  || iv1->step != const0_rtx)
	return false;

      iv1->base = simplify_gen_unary (signed_p ? SIGN_EXTEND : ZERO_EXTEND,
				      comp_mode, iv1->base, iv1->mode);
      iv1->extend_mode = comp_mode;
    }

  /* Check that both ivs belong to a range of a single mode.  If one of the
     operands is an invariant, we may need to shorten it into the common
     mode.  */
  if (iv0->mode == iv0->extend_mode
      && iv0->step == const0_rtx
      && iv0->mode != iv1->mode)
    shorten_into_mode (iv0, iv1->mode, cond, signed_p, desc);

  if (iv1->mode == iv1->extend_mode
      && iv1->step == const0_rtx
      && iv0->mode != iv1->mode)
    shorten_into_mode (iv1, iv0->mode, swap_condition (cond), signed_p, desc);

  if (iv0->mode != iv1->mode)
    return false;

  desc->mode = iv0->mode;
  desc->signed_p = signed_p;

  return true;
}

/* Computes number of iterations of the CONDITION in INSN in LOOP and stores
   the result into DESC.  Very similar to determine_number_of_iterations
   (basically its rtl version), complicated by things like subregs.  */

static void
iv_number_of_iterations (struct loop *loop, rtx insn, rtx condition,
			 struct niter_desc *desc)
{
  rtx op0, op1, delta, step, bound, may_xform, def_insn, tmp, tmp0, tmp1;
  struct rtx_iv iv0, iv1, tmp_iv;
  rtx assumption, may_not_xform;
  enum rtx_code cond;
  enum machine_mode mode, comp_mode;
  rtx mmin, mmax, mode_mmin, mode_mmax;
  unsigned HOST_WIDEST_INT s, size, d, inv;
  HOST_WIDEST_INT up, down, inc, step_val;
  int was_sharp = false;
  rtx old_niter;
  bool step_is_pow2;

  /* The meaning of these assumptions is this:
     if !assumptions
       then the rest of information does not have to be valid
     if noloop_assumptions then the loop does not roll
     if infinite then this exit is never used */

  desc->assumptions = NULL_RTX;
  desc->noloop_assumptions = NULL_RTX;
  desc->infinite = NULL_RTX;
  desc->simple_p = true;

  desc->const_iter = false;
  desc->niter_expr = NULL_RTX;
  desc->niter_max = 0;

  cond = GET_CODE (condition);
  if (!COMPARISON_P (condition))
    abort ();

  mode = GET_MODE (XEXP (condition, 0));
  if (mode == VOIDmode)
    mode = GET_MODE (XEXP (condition, 1));
  /* The constant comparisons should be folded.  */
  if (mode == VOIDmode)
    abort ();

  /* We only handle integers or pointers.  */
  if (GET_MODE_CLASS (mode) != MODE_INT
      && GET_MODE_CLASS (mode) != MODE_PARTIAL_INT)
    goto fail;

  op0 = XEXP (condition, 0);
  def_insn = iv_get_reaching_def (insn, op0);
  if (!iv_analyze (def_insn, op0, &iv0))
    goto fail;
  if (iv0.extend_mode == VOIDmode)
    iv0.mode = iv0.extend_mode = mode;
  
  op1 = XEXP (condition, 1);
  def_insn = iv_get_reaching_def (insn, op1);
  if (!iv_analyze (def_insn, op1, &iv1))
    goto fail;
  if (iv1.extend_mode == VOIDmode)
    iv1.mode = iv1.extend_mode = mode;

  if (GET_MODE_BITSIZE (iv0.extend_mode) > HOST_BITS_PER_WIDE_INT
      || GET_MODE_BITSIZE (iv1.extend_mode) > HOST_BITS_PER_WIDE_INT)
    goto fail;

  /* Check condition and normalize it.  */

  switch (cond)
    {
      case GE:
      case GT:
      case GEU:
      case GTU:
	tmp_iv = iv0; iv0 = iv1; iv1 = tmp_iv;
	cond = swap_condition (cond);
	break;
      case NE:
      case LE:
      case LEU:
      case LT:
      case LTU:
	break;
      default:
	goto fail;
    }

  /* Handle extends.  This is relatively nontrivial, so we only try in some
     easy cases, when we can canonicalize the ivs (possibly by adding some
     assumptions) to shape subreg (base + i * step).  This function also fills
     in desc->mode and desc->signed_p.  */

  if (!canonicalize_iv_subregs (&iv0, &iv1, cond, desc))
    goto fail;

  comp_mode = iv0.extend_mode;
  mode = iv0.mode;
  size = GET_MODE_BITSIZE (mode);
  get_mode_bounds (mode, (cond == LE || cond == LT), comp_mode, &mmin, &mmax);
  mode_mmin = lowpart_subreg (mode, mmin, comp_mode);
  mode_mmax = lowpart_subreg (mode, mmax, comp_mode);

  if (GET_CODE (iv0.step) != CONST_INT || GET_CODE (iv1.step) != CONST_INT)
    goto fail;

  /* We can take care of the case of two induction variables chasing each other
     if the test is NE. I have never seen a loop using it, but still it is
     cool.  */
  if (iv0.step != const0_rtx && iv1.step != const0_rtx)
    {
      if (cond != NE)
	goto fail;

      iv0.step = simplify_gen_binary (MINUS, comp_mode, iv0.step, iv1.step);
      iv1.step = const0_rtx;
    }

  /* This is either infinite loop or the one that ends immediately, depending
     on initial values.  Unswitching should remove this kind of conditions.  */
  if (iv0.step == const0_rtx && iv1.step == const0_rtx)
    goto fail;

  if (cond != NE)
    {
      if (iv0.step == const0_rtx)
	step_val = -INTVAL (iv1.step);
      else
	step_val = INTVAL (iv0.step);

      /* Ignore loops of while (i-- < 10) type.  */
      if (step_val < 0)
	goto fail;

      step_is_pow2 = !(step_val & (step_val - 1));
    }
  else
    {
      /* We do not care about whether the step is power of two in this
	 case.  */
      step_is_pow2 = false;
      step_val = 0;
    }

  /* Some more condition normalization.  We must record some assumptions
     due to overflows.  */
  switch (cond)
    {
      case LT:
      case LTU:
	/* We want to take care only of non-sharp relationals; this is easy,
	   as in cases the overflow would make the transformation unsafe
	   the loop does not roll.  Seemingly it would make more sense to want
	   to take care of sharp relationals instead, as NE is more similar to
	   them, but the problem is that here the transformation would be more
	   difficult due to possibly infinite loops.  */
	if (iv0.step == const0_rtx)
	  {
	    tmp = lowpart_subreg (mode, iv0.base, comp_mode);
	    assumption = simplify_gen_relational (EQ, SImode, mode, tmp,
						  mode_mmax);
	    if (assumption == const_true_rtx)
	      goto zero_iter_simplify;
	    iv0.base = simplify_gen_binary (PLUS, comp_mode,
					    iv0.base, const1_rtx);
	  }
	else
	  {
	    tmp = lowpart_subreg (mode, iv1.base, comp_mode);
	    assumption = simplify_gen_relational (EQ, SImode, mode, tmp,
						  mode_mmin);
	    if (assumption == const_true_rtx)
	      goto zero_iter_simplify;
	    iv1.base = simplify_gen_binary (PLUS, comp_mode,
					    iv1.base, constm1_rtx);
	  }

	if (assumption != const0_rtx)
	  desc->noloop_assumptions =
		  alloc_EXPR_LIST (0, assumption, desc->noloop_assumptions);
	cond = (cond == LT) ? LE : LEU;

	/* It will be useful to be able to tell the difference once more in
	   LE -> NE reduction.  */
	was_sharp = true;
	break;
      default: ;
    }

  /* Take care of trivially infinite loops.  */
  if (cond != NE)
    {
      if (iv0.step == const0_rtx)
	{
	  tmp = lowpart_subreg (mode, iv0.base, comp_mode);
	  if (rtx_equal_p (tmp, mode_mmin))
	    {
	      desc->infinite =
		      alloc_EXPR_LIST (0, const_true_rtx, NULL_RTX);
	      /* Fill in the remaining fields somehow.  */
	      goto zero_iter_simplify;
	    }
	}
      else
	{
	  tmp = lowpart_subreg (mode, iv1.base, comp_mode);
	  if (rtx_equal_p (tmp, mode_mmax))
	    {
	      desc->infinite =
		      alloc_EXPR_LIST (0, const_true_rtx, NULL_RTX);
	      /* Fill in the remaining fields somehow.  */
	      goto zero_iter_simplify;
	    }
	}
    }

  /* If we can we want to take care of NE conditions instead of size
     comparisons, as they are much more friendly (most importantly
     this takes care of special handling of loops with step 1).  We can
     do it if we first check that upper bound is greater or equal to
     lower bound, their difference is constant c modulo step and that
     there is not an overflow.  */
  if (cond != NE)
    {
      if (iv0.step == const0_rtx)
	step = simplify_gen_unary (NEG, comp_mode, iv1.step, comp_mode);
      else
	step = iv0.step;
      delta = simplify_gen_binary (MINUS, comp_mode, iv1.base, iv0.base);
      delta = lowpart_subreg (mode, delta, comp_mode);
      delta = simplify_gen_binary (UMOD, mode, delta, step);
      may_xform = const0_rtx;
      may_not_xform = const_true_rtx;

      if (GET_CODE (delta) == CONST_INT)
	{
	  if (was_sharp && INTVAL (delta) == INTVAL (step) - 1)
	    {
	      /* A special case.  We have transformed condition of type
		 for (i = 0; i < 4; i += 4)
		 into
		 for (i = 0; i <= 3; i += 4)
		 obviously if the test for overflow during that transformation
		 passed, we cannot overflow here.  Most importantly any
		 loop with sharp end condition and step 1 falls into this
		 category, so handling this case specially is definitely
		 worth the troubles.  */
	      may_xform = const_true_rtx;
	    }
	  else if (iv0.step == const0_rtx)
	    {
	      bound = simplify_gen_binary (PLUS, comp_mode, mmin, step);
	      bound = simplify_gen_binary (MINUS, comp_mode, bound, delta);
	      bound = lowpart_subreg (mode, bound, comp_mode);
	      tmp = lowpart_subreg (mode, iv0.base, comp_mode);
	      may_xform = simplify_gen_relational (cond, SImode, mode,
						   bound, tmp);
	      may_not_xform = simplify_gen_relational (reverse_condition (cond),
						       SImode, mode,
						       bound, tmp);
	    }
	  else
	    {
	      bound = simplify_gen_binary (MINUS, comp_mode, mmax, step);
	      bound = simplify_gen_binary (PLUS, comp_mode, bound, delta);
	      bound = lowpart_subreg (mode, bound, comp_mode);
	      tmp = lowpart_subreg (mode, iv1.base, comp_mode);
	      may_xform = simplify_gen_relational (cond, SImode, mode,
						   tmp, bound);
	      may_not_xform = simplify_gen_relational (reverse_condition (cond),
						       SImode, mode,
						       tmp, bound);
	    }
	}

      if (may_xform != const0_rtx)
	{
	  /* We perform the transformation always provided that it is not
	     completely senseless.  This is OK, as we would need this assumption
	     to determine the number of iterations anyway.  */
	  if (may_xform != const_true_rtx)
	    {
	      /* If the step is a power of two and the final value we have
		 computed overflows, the cycle is infinite.  Otherwise it
		 is nontrivial to compute the number of iterations.  */
	      if (step_is_pow2)
		desc->infinite = alloc_EXPR_LIST (0, may_not_xform,
						  desc->infinite);
	      else
		desc->assumptions = alloc_EXPR_LIST (0, may_xform,
						     desc->assumptions);
	    }

	  /* We are going to lose some information about upper bound on
	     number of iterations in this step, so record the information
	     here.  */
	  inc = INTVAL (iv0.step) - INTVAL (iv1.step);
	  if (GET_CODE (iv1.base) == CONST_INT)
	    up = INTVAL (iv1.base);
	  else
	    up = INTVAL (mode_mmax) - inc;
	  down = INTVAL (GET_CODE (iv0.base) == CONST_INT
			 ? iv0.base
			 : mode_mmin);
	  desc->niter_max = (up - down) / inc + 1;

	  if (iv0.step == const0_rtx)
	    {
	      iv0.base = simplify_gen_binary (PLUS, comp_mode, iv0.base, delta);
	      iv0.base = simplify_gen_binary (MINUS, comp_mode, iv0.base, step);
	    }
	  else
	    {
	      iv1.base = simplify_gen_binary (MINUS, comp_mode, iv1.base, delta);
	      iv1.base = simplify_gen_binary (PLUS, comp_mode, iv1.base, step);
	    }

	  tmp0 = lowpart_subreg (mode, iv0.base, comp_mode);
	  tmp1 = lowpart_subreg (mode, iv1.base, comp_mode);
	  assumption = simplify_gen_relational (reverse_condition (cond),
						SImode, mode, tmp0, tmp1);
	  if (assumption == const_true_rtx)
	    goto zero_iter_simplify;
	  else if (assumption != const0_rtx)
	    desc->noloop_assumptions =
		    alloc_EXPR_LIST (0, assumption, desc->noloop_assumptions);
	  cond = NE;
	}
    }

  /* Count the number of iterations.  */
  if (cond == NE)
    {
      /* Everything we do here is just arithmetics modulo size of mode.  This
	 makes us able to do more involved computations of number of iterations
	 than in other cases.  First transform the condition into shape
	 s * i <> c, with s positive.  */
      iv1.base = simplify_gen_binary (MINUS, comp_mode, iv1.base, iv0.base);
      iv0.base = const0_rtx;
      iv0.step = simplify_gen_binary (MINUS, comp_mode, iv0.step, iv1.step);
      iv1.step = const0_rtx;
      if (INTVAL (iv0.step) < 0)
	{
	  iv0.step = simplify_gen_unary (NEG, comp_mode, iv0.step, mode);
	  iv1.base = simplify_gen_unary (NEG, comp_mode, iv1.base, mode);
	}
      iv0.step = lowpart_subreg (mode, iv0.step, comp_mode);

      /* Let nsd (s, size of mode) = d.  If d does not divide c, the loop
	 is infinite.  Otherwise, the number of iterations is
	 (inverse(s/d) * (c/d)) mod (size of mode/d).  */
      s = INTVAL (iv0.step); d = 1;
      while (s % 2 != 1)
	{
	  s /= 2;
	  d *= 2;
	  size--;
	}
      bound = GEN_INT (((unsigned HOST_WIDEST_INT) 1 << (size - 1 ) << 1) - 1);

      tmp1 = lowpart_subreg (mode, iv1.base, comp_mode);
      tmp = simplify_gen_binary (UMOD, mode, tmp1, GEN_INT (d));
      assumption = simplify_gen_relational (NE, SImode, mode, tmp, const0_rtx);
      desc->infinite = alloc_EXPR_LIST (0, assumption, desc->infinite);

      tmp = simplify_gen_binary (UDIV, mode, tmp1, GEN_INT (d));
      inv = inverse (s, size);
      inv = trunc_int_for_mode (inv, mode);
      tmp = simplify_gen_binary (MULT, mode, tmp, GEN_INT (inv));
      desc->niter_expr = simplify_gen_binary (AND, mode, tmp, bound);
    }
  else
    {
      if (iv1.step == const0_rtx)
	/* Condition in shape a + s * i <= b
	   We must know that b + s does not overflow and a <= b + s and then we
	   can compute number of iterations as (b + s - a) / s.  (It might
	   seem that we in fact could be more clever about testing the b + s
	   overflow condition using some information about b - a mod s,
	   but it was already taken into account during LE -> NE transform).  */
	{
	  step = iv0.step;
	  tmp0 = lowpart_subreg (mode, iv0.base, comp_mode);
	  tmp1 = lowpart_subreg (mode, iv1.base, comp_mode);

	  bound = simplify_gen_binary (MINUS, mode, mode_mmax,
				       lowpart_subreg (mode, step,
						       comp_mode));
	  if (step_is_pow2)
	    {
	      rtx t0, t1;

	      /* If s is power of 2, we know that the loop is infinite if
		 a % s <= b % s and b + s overflows.  */
	      assumption = simplify_gen_relational (reverse_condition (cond),
						    SImode, mode,
						    tmp1, bound);

	      t0 = simplify_gen_binary (UMOD, mode, copy_rtx (tmp0), step);
	      t1 = simplify_gen_binary (UMOD, mode, copy_rtx (tmp1), step);
	      tmp = simplify_gen_relational (cond, SImode, mode, t0, t1);
	      assumption = simplify_gen_binary (AND, SImode, assumption, tmp);
	      desc->infinite =
		      alloc_EXPR_LIST (0, assumption, desc->infinite);
	    }
	  else
	    {
	      assumption = simplify_gen_relational (cond, SImode, mode,
						    tmp1, bound);
	      desc->assumptions =
		      alloc_EXPR_LIST (0, assumption, desc->assumptions);
	    }

	  tmp = simplify_gen_binary (PLUS, comp_mode, iv1.base, iv0.step);
	  tmp = lowpart_subreg (mode, tmp, comp_mode);
	  assumption = simplify_gen_relational (reverse_condition (cond),
						SImode, mode, tmp0, tmp);

	  delta = simplify_gen_binary (PLUS, mode, tmp1, step);
	  delta = simplify_gen_binary (MINUS, mode, delta, tmp0);
	}
      else
	{
	  /* Condition in shape a <= b - s * i
	     We must know that a - s does not overflow and a - s <= b and then
	     we can again compute number of iterations as (b - (a - s)) / s.  */
	  step = simplify_gen_unary (NEG, mode, iv1.step, mode);
	  tmp0 = lowpart_subreg (mode, iv0.base, comp_mode);
	  tmp1 = lowpart_subreg (mode, iv1.base, comp_mode);

	  bound = simplify_gen_binary (MINUS, mode, mode_mmin,
				       lowpart_subreg (mode, step, comp_mode));
	  if (step_is_pow2)
	    {
	      rtx t0, t1;

	      /* If s is power of 2, we know that the loop is infinite if
		 a % s <= b % s and a - s overflows.  */
	      assumption = simplify_gen_relational (reverse_condition (cond),
						    SImode, mode,
						    bound, tmp0);

	      t0 = simplify_gen_binary (UMOD, mode, copy_rtx (tmp0), step);
	      t1 = simplify_gen_binary (UMOD, mode, copy_rtx (tmp1), step);
	      tmp = simplify_gen_relational (cond, SImode, mode, t0, t1);
	      assumption = simplify_gen_binary (AND, SImode, assumption, tmp);
	      desc->infinite =
		      alloc_EXPR_LIST (0, assumption, desc->infinite);
	    }
	  else
	    {
	      assumption = simplify_gen_relational (cond, SImode, mode,
						    bound, tmp0);
	      desc->assumptions =
		      alloc_EXPR_LIST (0, assumption, desc->assumptions);
	    }

	  tmp = simplify_gen_binary (PLUS, comp_mode, iv0.base, iv1.step);
	  tmp = lowpart_subreg (mode, tmp, comp_mode);
	  assumption = simplify_gen_relational (reverse_condition (cond),
						SImode, mode,
						tmp, tmp1);
	  delta = simplify_gen_binary (MINUS, mode, tmp0, step);
	  delta = simplify_gen_binary (MINUS, mode, tmp1, delta);
	}
      if (assumption == const_true_rtx)
	goto zero_iter_simplify;
      else if (assumption != const0_rtx)
	desc->noloop_assumptions =
		alloc_EXPR_LIST (0, assumption, desc->noloop_assumptions);
      delta = simplify_gen_binary (UDIV, mode, delta, step);
      desc->niter_expr = delta;
    }

  old_niter = desc->niter_expr;

  simplify_using_initial_values (loop, AND, &desc->assumptions);
  if (desc->assumptions
      && XEXP (desc->assumptions, 0) == const0_rtx)
    goto fail;
  simplify_using_initial_values (loop, IOR, &desc->noloop_assumptions);
  simplify_using_initial_values (loop, IOR, &desc->infinite);
  simplify_using_initial_values (loop, UNKNOWN, &desc->niter_expr);

  /* Rerun the simplification.  Consider code (created by copying loop headers)

     i = 0;

     if (0 < n)
       {
         do
	   {
	     i++;
	   } while (i < n);
       }

    The first pass determines that i = 0, the second pass uses it to eliminate
    noloop assumption.  */

  simplify_using_initial_values (loop, AND, &desc->assumptions);
  if (desc->assumptions
      && XEXP (desc->assumptions, 0) == const0_rtx)
    goto fail;
  simplify_using_initial_values (loop, IOR, &desc->noloop_assumptions);
  simplify_using_initial_values (loop, IOR, &desc->infinite);
  simplify_using_initial_values (loop, UNKNOWN, &desc->niter_expr);

  if (desc->noloop_assumptions
      && XEXP (desc->noloop_assumptions, 0) == const_true_rtx)
    goto zero_iter;

  if (GET_CODE (desc->niter_expr) == CONST_INT)
    {
      unsigned HOST_WIDEST_INT val = INTVAL (desc->niter_expr);

      desc->const_iter = true;
      desc->niter_max = desc->niter = val & GET_MODE_MASK (desc->mode);
    }
  else
    {
      if (!desc->niter_max)
	desc->niter_max = determine_max_iter (desc);

      /* simplify_using_initial_values does a copy propagation on the registers
	 in the expression for the number of iterations.  This prolongs life
	 ranges of registers and increases register pressure, and usually
	 brings no gain (and if it happens to do, the cse pass will take care
	 of it anyway).  So prevent this behavior, unless it enabled us to
	 derive that the number of iterations is a constant.  */
      desc->niter_expr = old_niter;
    }

  return;

zero_iter_simplify:
  /* Simplify the assumptions.  */
  simplify_using_initial_values (loop, AND, &desc->assumptions);
  if (desc->assumptions
      && XEXP (desc->assumptions, 0) == const0_rtx)
    goto fail;
  simplify_using_initial_values (loop, IOR, &desc->infinite);

  /* Fallthru.  */
zero_iter:
  desc->const_iter = true;
  desc->niter = 0;
  desc->niter_max = 0;
  desc->noloop_assumptions = NULL_RTX;
  desc->niter_expr = const0_rtx;
  return;

fail:
  desc->simple_p = false;
  return;
}

/* Checks whether E is a simple exit from LOOP and stores its description
   into DESC.  */

static void
check_simple_exit (struct loop *loop, edge e, struct niter_desc *desc)
{
  basic_block exit_bb;
  rtx condition, at;
  edge ein;

  exit_bb = e->src;
  desc->simple_p = false;

  /* It must belong directly to the loop.  */
  if (exit_bb->loop_father != loop)
    return;

  /* It must be tested (at least) once during any iteration.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, exit_bb))
    return;

  /* It must end in a simple conditional jump.  */
  if (!any_condjump_p (BB_END (exit_bb)))
    return;

  ein = EDGE_SUCC (exit_bb, 0);
  if (ein == e)
    ein = EDGE_SUCC (exit_bb, 1);

  desc->out_edge = e;
  desc->in_edge = ein;

  /* Test whether the condition is suitable.  */
  if (!(condition = get_condition (BB_END (ein->src), &at, false, false)))
    return;

  if (ein->flags & EDGE_FALLTHRU)
    {
      condition = reversed_condition (condition);
      if (!condition)
	return;
    }

  /* Check that we are able to determine number of iterations and fill
     in information about it.  */
  iv_number_of_iterations (loop, at, condition, desc);
}

/* Finds a simple exit of LOOP and stores its description into DESC.  */

void
find_simple_exit (struct loop *loop, struct niter_desc *desc)
{
  unsigned i;
  basic_block *body;
  edge e;
  struct niter_desc act;
  bool any = false;
  edge_iterator ei;

  desc->simple_p = false;
  body = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      FOR_EACH_EDGE (e, ei, body[i]->succs)
	{
	  if (flow_bb_inside_loop_p (loop, e->dest))
	    continue;
	  
	  check_simple_exit (loop, e, &act);
	  if (!act.simple_p)
	    continue;

	  if (!any)
	    any = true;
	  else
	    {
	      /* Prefer constant iterations; the less the better.  */
	      if (!act.const_iter
		  || (desc->const_iter && act.niter >= desc->niter))
		continue;

	      /* Also if the actual exit may be infinite, while the old one
		 not, prefer the old one.  */
	      if (act.infinite && !desc->infinite)
		continue;
	    }
	  
	  *desc = act;
	}
    }

  if (dump_file)
    {
      if (desc->simple_p)
	{
	  fprintf (dump_file, "Loop %d is simple:\n", loop->num);
	  fprintf (dump_file, "  simple exit %d -> %d\n",
		   desc->out_edge->src->index,
		   desc->out_edge->dest->index);
	  if (desc->assumptions)
	    {
	      fprintf (dump_file, "  assumptions: ");
	      print_rtl (dump_file, desc->assumptions);
	      fprintf (dump_file, "\n");
	    }
	  if (desc->noloop_assumptions)
	    {
	      fprintf (dump_file, "  does not roll if: ");
	      print_rtl (dump_file, desc->noloop_assumptions);
	      fprintf (dump_file, "\n");
	    }
	  if (desc->infinite)
	    {
	      fprintf (dump_file, "  infinite if: ");
	      print_rtl (dump_file, desc->infinite);
	      fprintf (dump_file, "\n");
	    }

	  fprintf (dump_file, "  number of iterations: ");
	  print_rtl (dump_file, desc->niter_expr);
      	  fprintf (dump_file, "\n");

	  fprintf (dump_file, "  upper bound: ");
	  fprintf (dump_file, HOST_WIDEST_INT_PRINT_DEC, desc->niter_max);
      	  fprintf (dump_file, "\n");
	}
      else
	fprintf (dump_file, "Loop %d is not simple.\n", loop->num);
    }

  free (body);
}

/* Creates a simple loop description of LOOP if it was not computed
   already.  */

struct niter_desc *
get_simple_loop_desc (struct loop *loop)
{
  struct niter_desc *desc = simple_loop_desc (loop);

  if (desc)
    return desc;

  desc = xmalloc (sizeof (struct niter_desc));
  iv_analysis_loop_init (loop);
  find_simple_exit (loop, desc);
  loop->aux = desc;

  return desc;
}

/* Releases simple loop description for LOOP.  */

void
free_simple_loop_desc (struct loop *loop)
{
  struct niter_desc *desc = simple_loop_desc (loop);

  if (!desc)
    return;

  free (desc);
  loop->aux = NULL;
}
