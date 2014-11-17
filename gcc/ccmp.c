/* Conditional compare related functions
   Copyright (C) 2014-2014 Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "regs.h"
#include "expr.h"
#include "insn-codes.h"
#include "optabs.h"
#include "tree-iterator.h"
#include "predict.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-ssa.h"
#include "tree-ssanames.h"
#include "target.h"
#include "common/common-target.h"
#include "df.h"
#include "tree-ssa-live.h"
#include "tree-outof-ssa.h"
#include "cfgexpand.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "expmed.h"
#include "ccmp.h"

/* The following functions expand conditional compare (CCMP) instructions.
   Here is a short description about the over all algorithm:
     * ccmp_candidate_p is used to identify the CCMP candidate

     * expand_ccmp_expr is the main entry, which calls expand_ccmp_expr_1
       to expand CCMP.

     * expand_ccmp_expr_1 uses a recursive algorithm to expand CCMP.
       It calls two target hooks gen_ccmp_first and gen_ccmp_next to generate
       CCMP instructions.
	 - gen_ccmp_first expands the first compare in CCMP.
	 - gen_ccmp_next expands the following compares.

     * If the final result is not used in a COND_EXPR (checked by function
       used_in_cond_stmt_p), it calls cstorecc4 pattern to store the CC to a
       general register.  */

/* Check whether G is a potential conditional compare candidate.  */
static bool
ccmp_candidate_p (gimple g)
{
  tree rhs = gimple_assign_rhs_to_tree (g);
  tree lhs, op0, op1;
  gimple gs0, gs1;
  enum tree_code tcode, tcode0, tcode1;
  tcode = TREE_CODE (rhs);

  if (tcode != BIT_AND_EXPR && tcode != BIT_IOR_EXPR)
    return false;

  lhs = gimple_assign_lhs (g);
  op0 = TREE_OPERAND (rhs, 0);
  op1 = TREE_OPERAND (rhs, 1);

  if ((TREE_CODE (op0) != SSA_NAME) || (TREE_CODE (op1) != SSA_NAME)
      || !has_single_use (lhs))
    return false;

  gs0 = get_gimple_for_ssa_name (op0);
  gs1 = get_gimple_for_ssa_name (op1);
  if (!gs0 || !gs1 || !is_gimple_assign (gs0) || !is_gimple_assign (gs1)
      /* g, gs0 and gs1 must be in the same basic block, since current stage
	 is out-of-ssa.  We can not guarantee the correctness when forwording
	 the gs0 and gs1 into g whithout DATAFLOW analysis.  */
      || gimple_bb (gs0) != gimple_bb (gs1)
      || gimple_bb (gs0) != gimple_bb (g))
    return false;

  if (!(INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (gs0)))
       || POINTER_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (gs0))))
      || !(INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (gs1)))
	   || POINTER_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (gs1)))))
    return false;

  tcode0 = gimple_assign_rhs_code (gs0);
  tcode1 = gimple_assign_rhs_code (gs1);
  if (TREE_CODE_CLASS (tcode0) == tcc_comparison
      && TREE_CODE_CLASS (tcode1) == tcc_comparison)
    return true;
  if (TREE_CODE_CLASS (tcode0) == tcc_comparison
      && ccmp_candidate_p (gs1))
    return true;
  else if (TREE_CODE_CLASS (tcode1) == tcc_comparison
	   && ccmp_candidate_p (gs0))
    return true;
  /* We skip ccmp_candidate_p (gs1) && ccmp_candidate_p (gs0) since
     there is no way to set the CC flag.  */
  return false;
}

/* Check whether EXP is used in a GIMPLE_COND statement or not.  */
static bool
used_in_cond_stmt_p (tree exp)
{
  bool expand_cond = false;
  imm_use_iterator ui;
  gimple use_stmt;
  FOR_EACH_IMM_USE_STMT (use_stmt, ui, exp)
    if (gimple_code (use_stmt) == GIMPLE_COND)
      {
	tree op1 = gimple_cond_rhs (use_stmt);
	if (integer_zerop (op1))
	  expand_cond = true;
	BREAK_FROM_IMM_USE_STMT (ui);
      }
    else if (gimple_code (use_stmt) == GIMPLE_ASSIGN
	     && gimple_expr_code (use_stmt) == COND_EXPR)
      {
	if (gimple_assign_rhs1 (use_stmt) == exp)
	  expand_cond = true;
      }

  return expand_cond;
}

/* Expand conditional compare gimple G.  A typical CCMP sequence is like:

     CC0 = CMP (a, b);
     CC1 = CCMP (NE (CC0, 0), CMP (e, f));
     ...
     CCn = CCMP (NE (CCn-1, 0), CMP (...));

   hook gen_ccmp_first is used to expand the first compare.
   hook gen_ccmp_next is used to expand the following CCMP.  */
static rtx
expand_ccmp_expr_1 (gimple g)
{
  tree exp = gimple_assign_rhs_to_tree (g);
  enum tree_code code = TREE_CODE (exp);
  gimple gs0 = get_gimple_for_ssa_name (TREE_OPERAND (exp, 0));
  gimple gs1 = get_gimple_for_ssa_name (TREE_OPERAND (exp, 1));
  rtx tmp;
  enum tree_code code0 = gimple_assign_rhs_code (gs0);
  enum tree_code code1 = gimple_assign_rhs_code (gs1);

  gcc_assert (code == BIT_AND_EXPR || code == BIT_IOR_EXPR);
  gcc_assert (gs0 && gs1 && is_gimple_assign (gs0) && is_gimple_assign (gs1));

  if (TREE_CODE_CLASS (code0) == tcc_comparison)
    {
      if (TREE_CODE_CLASS (code1) == tcc_comparison)
	{
	  int unsignedp0, unsignedp1;
	  enum rtx_code rcode0, rcode1;
	  rtx op0, op1, op2, op3, tmp;

	  unsignedp0 = TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (gs0)));
	  rcode0 = get_rtx_code (code0, unsignedp0);
	  unsignedp1 = TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (gs1)));
	  rcode1 = get_rtx_code (code1, unsignedp1);

	  expand_operands (gimple_assign_rhs1 (gs0),
			   gimple_assign_rhs2 (gs0),
			   NULL_RTX, &op0, &op1, EXPAND_NORMAL);

	  /* Since the operands of GS1 might clobber CC reg, we expand the
	     operands of GS1 before GEN_CCMP_FIRST.  */
	  expand_operands (gimple_assign_rhs1 (gs1),
			   gimple_assign_rhs2 (gs1),
			   NULL_RTX, &op2, &op3, EXPAND_NORMAL);
	  tmp = targetm.gen_ccmp_first (rcode0, op0, op1);
	  if (!tmp)
	    return NULL_RTX;

	  return targetm.gen_ccmp_next (tmp, rcode1, op2, op3,
					get_rtx_code (code, 0));
	}
      else
	{
  	  rtx op0, op1;
	  enum rtx_code rcode;
	  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (gs0)));

	  rcode = get_rtx_code (gimple_assign_rhs_code (gs0), unsignedp);

	  /* Hoist the preparation operations above the entire
	     conditional compare sequence.  */
	  expand_operands (gimple_assign_rhs1 (gs0),
			   gimple_assign_rhs2 (gs0),
			   NULL_RTX, &op0, &op1, EXPAND_NORMAL);

	  gcc_assert (code1 == BIT_AND_EXPR || code1 == BIT_IOR_EXPR);

	  /* Note: We swap the order to make the recursive function work.  */
	  tmp = expand_ccmp_expr_1 (gs1);
	  if (tmp)
	    return targetm.gen_ccmp_next (tmp, rcode, op0, op1,
					  get_rtx_code (code, 0));
	}
    }
  else
    {
      gcc_assert (gimple_assign_rhs_code (gs0) == BIT_AND_EXPR
                  || gimple_assign_rhs_code (gs0) == BIT_IOR_EXPR);

      if (TREE_CODE_CLASS (gimple_assign_rhs_code (gs1)) == tcc_comparison)
	{
  	  rtx op0, op1;
	  enum rtx_code rcode;
	  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (gs1)));

	  rcode = get_rtx_code (gimple_assign_rhs_code (gs1), unsignedp);

	  /* Hoist the preparation operations above the entire
	     conditional compare sequence.  */
	  expand_operands (gimple_assign_rhs1 (gs1),
			   gimple_assign_rhs2 (gs1),
			   NULL_RTX, &op0, &op1, EXPAND_NORMAL);
	  tmp = expand_ccmp_expr_1 (gs0);
	  if (tmp)
	    return targetm.gen_ccmp_next (tmp, rcode, op0, op1,
					  get_rtx_code (code, 0));
	}
      else
	{
	  gcc_assert (gimple_assign_rhs_code (gs1) == BIT_AND_EXPR
		      || gimple_assign_rhs_code (gs1) == BIT_IOR_EXPR);
	}
    }

  return NULL_RTX;
}

/* Main entry to expand conditional compare statement G. 
   Return NULL_RTX if G is not a legal candidate or expand fail.
   Otherwise return the target.  */
rtx
expand_ccmp_expr (gimple g)
{
  rtx_insn *last;
  rtx tmp;

  if (!ccmp_candidate_p (g))
    return NULL_RTX;

  last = get_last_insn ();
  tmp = expand_ccmp_expr_1 (g);

  if (tmp)
    {
      enum insn_code icode;
      enum machine_mode cc_mode = CCmode;

      tree lhs = gimple_assign_lhs (g);
      /* TMP should be CC.  If it is used in a GIMPLE_COND, just return it.
	 Note: Target needs to define "cbranchcc4".  */
      if (used_in_cond_stmt_p (lhs))
	return tmp;

#ifdef SELECT_CC_MODE
      cc_mode = SELECT_CC_MODE (NE, tmp, const0_rtx);
#endif
      /* If TMP is not used in a GIMPLE_COND, store it with a csctorecc4_optab.
	 Note: Target needs to define "cstorecc4".  */
      icode = optab_handler (cstore_optab, cc_mode);
      if (icode != CODE_FOR_nothing)
	{
	  tree lhs = gimple_assign_lhs (g);
	  enum machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
	  rtx target = gen_reg_rtx (mode);
	  tmp = emit_cstore (target, icode, NE, cc_mode, cc_mode,
			     0, tmp, const0_rtx, 1, mode);
	  if (tmp)
	    return tmp;
	}
    }
  /* Clean up.  */
  delete_insns_since (last);
  return NULL_RTX;
}

