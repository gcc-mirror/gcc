/* Conditional compare related functions
   Copyright (C) 2014-2016 Free Software Foundation, Inc.

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
#include "gimple.h"
#include "memmodel.h"
#include "tm_p.h"
#include "ssa.h"
#include "expmed.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "stor-layout.h"
#include "tree-ssa-live.h"
#include "tree-outof-ssa.h"
#include "cfgexpand.h"
#include "ccmp.h"
#include "predict.h"

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

       Both hooks return a comparison with the CC register that is equivalent
       to the value of the gimple comparison.  This is used by the next CCMP
       and in the final conditional store.

     * We use cstorecc4 pattern to convert the CCmode intermediate to
       the integer mode result that expand_normal is expecting.

   Since the operands of the later compares might clobber CC reg, we do not
   emit the insns during expand.  We keep the insn sequences in two seq

     * prep_seq, which includes all the insns to prepare the operands.
     * gen_seq, which includes all the compare and conditional compares.

   If all checks OK in expand_ccmp_expr, it emits insns in prep_seq, then
   insns in gen_seq.  */

/* Check whether G is a potential conditional compare candidate.  */
static bool
ccmp_candidate_p (gimple *g)
{
  tree rhs = gimple_assign_rhs_to_tree (g);
  tree lhs, op0, op1;
  gimple *gs0, *gs1;
  tree_code tcode, tcode0, tcode1;
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

/* PREV is a comparison with the CC register which represents the
   result of the previous CMP or CCMP.  The function expands the
   next compare based on G which is ANDed/ORed with the previous
   compare depending on CODE.
   PREP_SEQ returns all insns to prepare opearands for compare.
   GEN_SEQ returns all compare insns.  */
static rtx
expand_ccmp_next (gimple *g, tree_code code, rtx prev,
		  rtx *prep_seq, rtx *gen_seq)
{
  rtx_code rcode;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (g)));

  gcc_assert (code == BIT_AND_EXPR || code == BIT_IOR_EXPR);

  rcode = get_rtx_code (gimple_assign_rhs_code (g), unsignedp);

  return targetm.gen_ccmp_next (prep_seq, gen_seq, prev, rcode,
				gimple_assign_rhs1 (g),
				gimple_assign_rhs2 (g),
				get_rtx_code (code, 0));
}

/* Expand conditional compare gimple G.  A typical CCMP sequence is like:

     CC0 = CMP (a, b);
     CC1 = CCMP (NE (CC0, 0), CMP (e, f));
     ...
     CCn = CCMP (NE (CCn-1, 0), CMP (...));

   hook gen_ccmp_first is used to expand the first compare.
   hook gen_ccmp_next is used to expand the following CCMP.
   PREP_SEQ returns all insns to prepare opearand.
   GEN_SEQ returns all compare insns.  */
static rtx
expand_ccmp_expr_1 (gimple *g, rtx *prep_seq, rtx *gen_seq)
{
  rtx prep_seq_1, gen_seq_1;
  rtx prep_seq_2, gen_seq_2;
  tree exp = gimple_assign_rhs_to_tree (g);
  tree_code code = TREE_CODE (exp);
  gimple *gs0 = get_gimple_for_ssa_name (TREE_OPERAND (exp, 0));
  gimple *gs1 = get_gimple_for_ssa_name (TREE_OPERAND (exp, 1));
  rtx tmp;
  tree_code code0 = gimple_assign_rhs_code (gs0);
  tree_code code1 = gimple_assign_rhs_code (gs1);

  gcc_assert (code == BIT_AND_EXPR || code == BIT_IOR_EXPR);
  gcc_assert (gs0 && gs1 && is_gimple_assign (gs0) && is_gimple_assign (gs1));

  if (TREE_CODE_CLASS (code0) == tcc_comparison)
    {
      if (TREE_CODE_CLASS (code1) == tcc_comparison)
	{
	  int unsignedp0, unsignedp1;
	  rtx_code rcode0, rcode1;
	  int speed_p = optimize_insn_for_speed_p ();
	  rtx tmp2 = NULL_RTX, ret = NULL_RTX, ret2 = NULL_RTX;
	  unsigned cost1 = MAX_COST;
	  unsigned cost2 = MAX_COST;

	  unsignedp0 = TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (gs0)));
	  unsignedp1 = TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (gs1)));
	  rcode0 = get_rtx_code (code0, unsignedp0);
	  rcode1 = get_rtx_code (code1, unsignedp1);

	  tmp = targetm.gen_ccmp_first (&prep_seq_1, &gen_seq_1, rcode0,
					gimple_assign_rhs1 (gs0),
					gimple_assign_rhs2 (gs0));

	  if (tmp != NULL)
	    {
	      ret = expand_ccmp_next (gs1, code, tmp, &prep_seq_1, &gen_seq_1);
	      cost1 = seq_cost (safe_as_a <rtx_insn *> (prep_seq_1), speed_p);
	      cost1 += seq_cost (safe_as_a <rtx_insn *> (gen_seq_1), speed_p);
	    }

	  /* FIXME: Temporary workaround for PR69619.
	     Avoid exponential compile time due to expanding gs0 and gs1 twice.
	     If gs0 and gs1 are complex, the cost will be high, so avoid
	     reevaluation if above an arbitrary threshold.  */
	  if (tmp == NULL || cost1 < COSTS_N_INSNS (25))
	    tmp2 = targetm.gen_ccmp_first (&prep_seq_2, &gen_seq_2, rcode1,
					   gimple_assign_rhs1 (gs1),
					   gimple_assign_rhs2 (gs1));

	  if (!tmp && !tmp2)
	    return NULL_RTX;

	  if (tmp2 != NULL)
	    {
	      ret2 = expand_ccmp_next (gs0, code, tmp2, &prep_seq_2,
				       &gen_seq_2);
	      cost2 = seq_cost (safe_as_a <rtx_insn *> (prep_seq_2), speed_p);
	      cost2 += seq_cost (safe_as_a <rtx_insn *> (gen_seq_2), speed_p);
	    }

	  if (cost2 < cost1)
	    {
	      *prep_seq = prep_seq_2;
	      *gen_seq = gen_seq_2;
	      return ret2;
	    }

	  *prep_seq = prep_seq_1;
	  *gen_seq = gen_seq_1;
	  return ret;
	}
      else
	{
	  tmp = expand_ccmp_expr_1 (gs1, prep_seq, gen_seq);
	  if (!tmp)
	    return NULL_RTX;

	  return expand_ccmp_next (gs0, code, tmp, prep_seq, gen_seq);
	}
    }
  else
    {
      gcc_assert (gimple_assign_rhs_code (gs0) == BIT_AND_EXPR
                  || gimple_assign_rhs_code (gs0) == BIT_IOR_EXPR);

      if (TREE_CODE_CLASS (gimple_assign_rhs_code (gs1)) == tcc_comparison)
	{
	  tmp = expand_ccmp_expr_1 (gs0, prep_seq, gen_seq);
	  if (!tmp)
	    return NULL_RTX;

	  return expand_ccmp_next (gs1, code, tmp, prep_seq, gen_seq);
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
expand_ccmp_expr (gimple *g)
{
  rtx_insn *last;
  rtx tmp;
  rtx prep_seq, gen_seq;

  prep_seq = gen_seq = NULL_RTX;

  if (!ccmp_candidate_p (g))
    return NULL_RTX;

  last = get_last_insn ();
  tmp = expand_ccmp_expr_1 (g, &prep_seq, &gen_seq);

  if (tmp)
    {
      insn_code icode;
      machine_mode cc_mode = CCmode;
      tree lhs = gimple_assign_lhs (g);
      rtx_code cmp_code = GET_CODE (tmp);

#ifdef SELECT_CC_MODE
      cc_mode = SELECT_CC_MODE (cmp_code, XEXP (tmp, 0), const0_rtx);
#endif
      icode = optab_handler (cstore_optab, cc_mode);
      if (icode != CODE_FOR_nothing)
	{
	  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
	  rtx target = gen_reg_rtx (mode);

	  emit_insn (prep_seq);
	  emit_insn (gen_seq);

	  tmp = emit_cstore (target, icode, cmp_code, cc_mode, cc_mode,
			     0, XEXP (tmp, 0), const0_rtx, 1, mode);
	  if (tmp)
	    return tmp;
	}
    }
  /* Clean up.  */
  delete_insns_since (last);
  return NULL_RTX;
}

