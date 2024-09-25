/* Support for avr-passes.def for AVR 8-bit microcontrollers.
   Copyright (C) 2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "intl.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "cfganal.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "explow.h"
#include "cfgrtl.h"
#include "context.h"
#include "tree-pass.h"

namespace
{


//////////////////////////////////////////////////////////////////////////////
// Try to replace 2 cbranch insns with 1 comparison and 2 branches.

static const pass_data avr_pass_data_ifelse =
{
  RTL_PASS,      // type
  "",            // name (will be patched)
  OPTGROUP_NONE, // optinfo_flags
  TV_DF_SCAN,    // tv_id
  0,             // properties_required
  0,             // properties_provided
  0,             // properties_destroyed
  0,             // todo_flags_start
  TODO_df_finish | TODO_df_verify // todo_flags_finish
};

class avr_pass_ifelse : public rtl_opt_pass
{
public:
  avr_pass_ifelse (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_ifelse, ctxt)
  {
    this->name = name;
  }

  bool gate (function *) final override
  {
    return optimize > 0;
  }

  unsigned int execute (function *func) final override;
}; // avr_pass_ifelse


/* Return TRUE iff comparison code CODE is explicitly signed.  */

static bool
avr_strict_signed_p (rtx_code code)
{
  return code == GT || code == GE || code == LT || code == LE;
}


/* Return TRUE iff comparison code CODE is explicitly unsigned.  */

static bool
avr_strict_unsigned_p (rtx_code code)
{
  return code == GTU || code == GEU || code == LTU || code == LEU;
}

#include "config/avr/ranges.h"

/* Suppose the inputs represent a code like

      if (x <CMP1> XVAL1)  goto way1;
      if (x <CMP2> XVAL2)  goto way2;
      way3:;

   with two integer mode comparisons where XVAL1 and XVAL2 are CONST_INT.
   When this can be rewritten in the form

      if (x <cond1> xval)  goto way1;
      if (x <cond2> xval)  goto way2;
      way3:;

  then set CMP1 = cond1, CMP2 = cond2, and return xval.  Else return NULL_RTX.
  When SWAPT is returned true, then way1 and way2 must be swapped.
  When the incomping SWAPT is false, the outgoing one will be false, too.  */

static rtx
avr_2comparisons_rhs (rtx_code &cmp1, rtx xval1,
		      rtx_code &cmp2, rtx xval2,
		      machine_mode mode, bool &swapt)
{
  const bool may_swapt = swapt;
  swapt = false;

  //////////////////////////////////////////////////////////////////
  // Step 0: Decide about signedness, map xval1/2 to the range
  //         of [un]signed machine mode.

  const bool signed1_p = avr_strict_signed_p (cmp1);
  const bool signed2_p = avr_strict_signed_p (cmp2);
  const bool unsigned1_p = avr_strict_unsigned_p (cmp1);
  const bool unsigned2_p = avr_strict_unsigned_p (cmp2);
  const bool signed_p = signed1_p || signed2_p;
  bool unsigned_p = unsigned1_p || unsigned2_p;

  using T = Ranges::scalar_type;
  T val1 = INTVAL (xval1);
  T val2 = INTVAL (xval2);

  if (signed_p + unsigned_p > 1)
    {
      // Don't go down that rabbit hole.  When the RHSs are the
      // same, we can still save one comparison.
      return val1 == val2 ? xval1 : NULL_RTX;
    }

  // Decide about signedness.  When no explicit signedness is present,
  // then cases that are close to the unsigned boundary like  EQ 0, EQ 1
  // can also be optimized.
  if (unsigned_p
      || (! signed_p && IN_RANGE (val1, -2, 2)))
    {
      unsigned_p = true;
      val1 = UINTVAL (xval1) & GET_MODE_MASK (mode);
      val2 = UINTVAL (xval2) & GET_MODE_MASK (mode);
    }

  // No way we can decompose the domain in a usable manner when the
  // RHSes are too far apart.
  if (! IN_RANGE (val1 - val2, -2, 2))
    return NULL_RTX;

  //////////////////////////////////////////////////////////////////
  // Step 1: Represent the input conditions as truth Ranges.  This
  //         establishes a decomposition / coloring of the domain.

  Ranges dom = Ranges::NBitsRanges (GET_MODE_BITSIZE (mode), unsigned_p,
				    Ranges::ALL);
  Ranges r[4] = { dom, dom.truth (cmp1, val1), dom.truth (cmp2, val2), dom };

  // r[1] shadows r[2] shadows r[3].  r[0] is just for nice indices.
  r[3].minus (r[2]);
  r[3].minus (r[1]);
  r[2].minus (r[1]);

  //////////////////////////////////////////////////////////////////
  // Step 2: Filter for cases where the domain decomposes into three
  //         intervals:  One to the left, one to the right, and one
  //         in the middle where the latter holds exactly one value.

  for (int i = 1; i <= 3; ++i)
    {
      // Keep track of which Ranges is which.
      r[i].tag = i;

      gcc_assert (r[i].check ());

      // Filter for proper intervals.  Also return for the empty set,
      // since cases where [m_min, m_max] decomposes into two intervals
      // or less have been sorted out by the generic optimizers already,
      // and hence should not be seen here.  And more than two intervals
      // at a time cannot be optimized of course.
      if (r[i].size () != 1)
	return NULL_RTX;
    }

  // Bubble-sort the three intervals such that:
  // [1] is the left interval, i.e. the one taken by LT[U].
  // [2] is the middle interval, i.e. the one taken by EQ.
  // [3] is the right interval, i.e. the one taken by GT[U].
  Ranges::sort2 (r[1], r[3]);
  Ranges::sort2 (r[2], r[3]);
  Ranges::sort2 (r[1], r[2]);

  if (dump_file)
    fprintf (dump_file,
	     ";; Decomposed: .%d=[%ld, %ld] .%d=[%ld, %ld] .%d=[%ld, %ld]\n",
	     r[1].tag, (long) r[1].ranges[0].lo, (long) r[1].ranges[0].hi,
	     r[2].tag, (long) r[2].ranges[0].lo, (long) r[2].ranges[0].hi,
	     r[3].tag, (long) r[3].ranges[0].lo, (long) r[3].ranges[0].hi);

  // EQ / NE can handle only one value.
  if (r[2].cardinality (0) != 1)
    return NULL_RTX;

  // Success! This is the sought for xval.
  const T val = r[2].ranges[0].lo;

  //////////////////////////////////////////////////////////////////
  // Step 3: Work out which label gets which condition, trying to
  //         avoid the expensive codes GT[U] and LE[U] if possible.
  //         Avoiding expensive codes is always possible when labels
  //         way1 and way2 may be swapped.

  // The xx1 ways have an expensive GT for cmp1 which can be avoided
  // by swapping way1 with way2.
  swapt = may_swapt && r[3].tag == 1;
  if (swapt)
    std::swap (r[3], r[2].tag == 2 ? r[2] : r[1]);

  // 6 = 3! ways to assign LT, EQ, GT to the three labels.
  const int way = 100 * r[1].tag + 10 * r[2].tag + r[3].tag;

  if (dump_file)
    fprintf (dump_file, ";; Success: unsigned=%d, swapt=%d, way=%d, rhs=%ld\n",
	     unsigned_p, swapt, way, (long) val);

#define WAY(w, c1, c2)					\
  case w:						\
    cmp1 = unsigned_p ? unsigned_condition (c1) : c1;	\
    cmp2 = unsigned_p ? unsigned_condition (c2) : c2;	\
    break;

  switch (way)
    {
    default:
      gcc_unreachable();

      // cmp1 gets the LT, avoid difficult branches for cmp2.
      WAY (123, LT, EQ);
      WAY (132, LT, NE);

      // cmp1 gets the EQ, avoid difficult branches for cmp2.
      WAY (213, EQ, LT);
      WAY (312, EQ, GE);

      // cmp1 gets the difficult GT, unavoidable as we may not swap way1/2.
      WAY (231, GT, NE);
      WAY (321, GT, EQ);
    }

#undef WAY

  return gen_int_mode (val, mode);
}


/* A helper for the next method.  Suppose we have two conditional branches
   with REG and CONST_INT operands

      if (reg <cond1> xval1) goto label1;
      if (reg <cond2> xval2) goto label2;

   If the second comparison is redundant and there are codes <cmp1>
   and <cmp2> such that the sequence can be performed as

      REG_CC = compare (reg, xval);
      if (REG_CC <cmp1> 0) goto label1;
      if (REG_CC <cmp2> 0) goto label2;

   then set COND1 to cmp1, COND2 to cmp2, SWAPT to true when the branch
   targets have to be swapped, and return XVAL.  Otherwise, return NULL_RTX.
   This function may clobber COND1 and COND2 even when it returns NULL_RTX.

   REVERSE_COND1 can be set to reverse condition COND1.  This is useful
   when the second comparison does not follow the first one, but is
   located after label1 like in:

      if (reg <cond1> xval1) goto label1;
      ...
      label1:
      if (reg <cond2> xval2) goto label2;

   In such a case we cannot swap the labels, and we may end up with a
   difficult branch -- though one comparison can still be optimized out.
   Getting rid of such difficult branches would require to reorder blocks. */

static rtx
avr_redundant_compare (rtx xreg1, rtx_code &cond1, rtx xval1,
		       rtx xreg2, rtx_code &cond2, rtx xval2,
		       bool reverse_cond1, bool &swapt)
{
  // Make sure we have two REG <cond> CONST_INT comparisons with the same reg.
  if (! rtx_equal_p (xreg1, xreg2)
      || ! CONST_INT_P (xval1)
      || ! CONST_INT_P (xval2))
    return NULL_RTX;

  if (reverse_cond1)
    cond1 = reverse_condition (cond1);

  // Allow swapping label1 <-> label2 only when ! reverse_cond1.
  swapt = ! reverse_cond1;
  rtx_code c1 = cond1;
  rtx_code c2 = cond2;
  rtx xval = avr_2comparisons_rhs (c1, xval1,
				   c2, xval2, GET_MODE (xreg1), swapt);
  if (! xval)
    return NULL_RTX;

  if (dump_file)
    {
      rtx_code a1 = reverse_cond1 ? reverse_condition (cond1) : cond1;
      rtx_code b1 = reverse_cond1 ? reverse_condition (c1) : c1;
      const char *s_rev1 = reverse_cond1 ? " reverse_cond1" : "";
      avr_dump (";; cond1: %C %r%s\n", a1, xval1, s_rev1);
      avr_dump (";; cond2: %C %r\n", cond2, xval2);
      avr_dump (";; => %C %d\n", b1, (int) INTVAL (xval));
      avr_dump (";; => %C %d\n", c2, (int) INTVAL (xval));
    }

  cond1 = c1;
  cond2 = c2;

  return xval;
}


/* Similar to the function above, but assume that

      if (xreg1 <cond1> xval1) goto label1;
      if (xreg2 <cond2> xval2) goto label2;

   are two subsequent REG-REG comparisons.  When this can be represented as

      REG_CC = compare (reg, xval);
      if (REG_CC <cmp1> 0) goto label1;
      if (REG_CC <cmp2> 0) goto label2;

   then set XREG1 to reg, COND1 and COND2 accordingly, and return xval.
   Otherwise, return NULL_RTX.  This optmization can be performed
   when { xreg1, xval1 } and { xreg2, xval2 } are equal as sets.
   It can be done in such a way that no difficult branches occur.  */

static rtx
avr_redundant_compare_regs (rtx &xreg1, rtx_code &cond1, rtx &xval1,
			    rtx &xreg2, rtx_code &cond2, rtx &xval2,
			    bool reverse_cond1)
{
  bool swapped;

  if (! REG_P (xval1))
    return NULL_RTX;
  else if (rtx_equal_p (xreg1, xreg2)
	   && rtx_equal_p (xval1, xval2))
    swapped = false;
  else if (rtx_equal_p (xreg1, xval2)
	   && rtx_equal_p (xreg2, xval1))
    swapped = true;
  else
    return NULL_RTX;

  // Found a redundant REG-REG comparison.  Assume that the incoming
  // representation has been canonicalized by CANONICALIZE_COMPARISON.
  // We can always represent this using only one comparison and in such
  // a way that no difficult branches are required.

  if (dump_file)
    {
      const char *s_rev1 = reverse_cond1 ? " reverse_cond1" : "";
      avr_dump (";; %r %C %r%s\n", xreg1, cond1, xval1, s_rev1);
      avr_dump (";; %r %C %r\n", xreg2, cond2, xval2);
    }

  if (reverse_cond1)
    cond1 = reverse_condition (cond1);

  if (swapped)
    {
      if (cond1 == EQ || cond1 == NE)
	{
	  avr_dump (";; case #21\n");
	  std::swap (xreg1, xval1);
	}
      else
	{
	  std::swap (xreg2, xval2);
	  cond2 = swap_condition (cond2);

	  // The swap may have introduced a difficult comparison.
	  // In order to get of it, only a few cases need extra care.
	  if ((cond1 == LT && cond2 == GT)
	      || (cond1 == LTU && cond2 == GTU))
	    {
	      avr_dump (";; case #22\n");
	      cond2 = NE;
	    }
	  else
	    avr_dump (";; case #23\n");
	}
    }
  else
    avr_dump (";; case #20\n");

  return xval1;
}


/* INSN1 and INSN2 are two cbranch insns for the same integer mode.
   When FOLLOW_LABEL1 is false, then INSN2 is located in the fallthrough
   path of INSN1.  When FOLLOW_LABEL1 is true, then INSN2 is located at
   the true edge of INSN1, INSN2 is preceded by a barrier, and no other
   edge leads to the basic block of INSN2.

   Try to replace INSN1 and INSN2 by a compare insn and two branch insns.
   When such a replacement has been performed, then return the insn where the
   caller should continue scanning the insn stream.  Else, return nullptr.  */

static rtx_insn *
avr_optimize_2ifelse (rtx_jump_insn *insn1,
		      rtx_jump_insn *insn2, bool follow_label1)
{
  avr_dump (";; Investigating jump_insn %d and jump_insn %d.\n",
	    INSN_UID (insn1), INSN_UID (insn2));

  // Extract the operands of the insns:
  // $0 = comparison operator ($1, $2)
  // $1 = reg
  // $2 = reg or const_int
  // $3 = code_label
  // $4 = optional SCRATCH for HI, PSI, SI cases.

  const auto &op = recog_data.operand;

  extract_insn (insn1);
  rtx xop1[5] = { op[0], op[1], op[2], op[3], op[4] };
  int n_operands = recog_data.n_operands;

  extract_insn (insn2);
  rtx xop2[5] = { op[0], op[1], op[2], op[3], op[4] };

  rtx_code code1 = GET_CODE (xop1[0]);
  rtx_code code2 = GET_CODE (xop2[0]);
  bool swap_targets = false;

  // Search redundant REG-REG comparison.
  rtx xval = avr_redundant_compare_regs (xop1[1], code1, xop1[2],
					 xop2[1], code2, xop2[2],
					 follow_label1);

  // Search redundant REG-CONST_INT comparison.
  if (! xval)
    xval = avr_redundant_compare (xop1[1], code1, xop1[2],
				  xop2[1], code2, xop2[2],
				  follow_label1, swap_targets);
  if (! xval)
    {
      avr_dump (";; Nothing found for jump_insn %d and jump_insn %d.\n",
		INSN_UID (insn1), INSN_UID (insn2));
      return nullptr;
    }

  if (follow_label1)
    code1 = reverse_condition (code1);

  //////////////////////////////////////////////////////
  // Found a replacement.

  if (dump_file)
    {
      avr_dump (";; => %C %r\n", code1, xval);
      avr_dump (";; => %C %r\n", code2, xval);

      fprintf (dump_file, "\n;; Found chain of jump_insn %d and"
	       " jump_insn %d, follow_label1=%d:\n",
	       INSN_UID (insn1), INSN_UID (insn2), follow_label1);
      print_rtl_single (dump_file, PATTERN (insn1));
      print_rtl_single (dump_file, PATTERN (insn2));
    }

  rtx_insn *next_insn
    = next_nonnote_nondebug_insn (follow_label1 ? insn1 : insn2);

  // Pop the new branch conditions and the new comparison.
  // Prematurely split into compare + branch so that we can drop
  // the 2nd comparison.  The following pass, split2, splits all
  // insns for REG_CC, and it should still work as usual even when
  // there are already some REG_CC insns around.

  rtx xcond1 = gen_rtx_fmt_ee (code1, VOIDmode, cc_reg_rtx, const0_rtx);
  rtx xcond2 = gen_rtx_fmt_ee (code2, VOIDmode, cc_reg_rtx, const0_rtx);
  rtx xpat1 = gen_branch (xop1[3], xcond1);
  rtx xpat2 = gen_branch (xop2[3], xcond2);
  rtx xcompare = NULL_RTX;
  machine_mode mode = GET_MODE (xop1[1]);

  if (mode == QImode)
    {
      gcc_assert (n_operands == 4);
      xcompare = gen_cmpqi3 (xop1[1], xval);
    }
  else
    {
      gcc_assert (n_operands == 5);
      rtx scratch = GET_CODE (xop1[4]) == SCRATCH ? xop2[4] : xop1[4];
      rtx (*gen_cmp)(rtx,rtx,rtx)
	= mode == HImode  ? gen_gen_comparehi
	: mode == PSImode ? gen_gen_comparepsi
	: gen_gen_comparesi; // SImode
      xcompare = gen_cmp (xop1[1], xval, scratch);
    }

  // Emit that stuff.

  rtx_insn *cmp = emit_insn_before (xcompare, insn1);
  rtx_jump_insn *branch1 = emit_jump_insn_after (xpat1, insn1);
  rtx_jump_insn *branch2 = emit_jump_insn_after (xpat2, insn2);

  JUMP_LABEL (branch1) = xop1[3];
  JUMP_LABEL (branch2) = xop2[3];
  // delete_insn() decrements LABEL_NUSES when deleting a JUMP_INSN,
  // but when we pop a new JUMP_INSN, do it by hand.
  ++LABEL_NUSES (xop1[3]);
  ++LABEL_NUSES (xop2[3]);

  delete_insn (insn1);
  delete_insn (insn2);

  if (swap_targets)
    {
      gcc_assert (! follow_label1);

      basic_block to1 = BLOCK_FOR_INSN (xop1[3]);
      basic_block to2 = BLOCK_FOR_INSN (xop2[3]);
      edge e1 = find_edge (BLOCK_FOR_INSN (branch1), to1);
      edge e2 = find_edge (BLOCK_FOR_INSN (branch2), to2);
      gcc_assert (e1);
      gcc_assert (e2);
      redirect_edge_and_branch (e1, to2);
      redirect_edge_and_branch (e2, to1);
    }

  // As a side effect, also recog the new insns.
  gcc_assert (valid_insn_p (cmp));
  gcc_assert (valid_insn_p (branch1));
  gcc_assert (valid_insn_p (branch2));

  return next_insn;
}


/* Sequences like

      SREG = compare (reg, 1 + val);
	  if (SREG >= 0)  goto label1;
      SREG = compare (reg, val);
	  if (SREG == 0)  goto label2;

   can be optimized to

      SREG = compare (reg, val);
	  if (SREG == 0)  goto label2;
	  if (SREG >= 0)  goto label1;

   Almost all cases where one of the comparisons is redundant can
   be transformed in such a way that only one comparison is required
   and no difficult branches are needed.  */

unsigned int
avr_pass_ifelse::execute (function *)
{
  rtx_insn *next_insn;

  for (rtx_insn *insn = get_insns(); insn; insn = next_insn)
    {
      next_insn = next_nonnote_nondebug_insn (insn);

      if (! next_insn)
	break;

      // Search for two cbranch insns.  The first one is a cbranch.
      // Filter for "cbranch<mode>4_insn" with mode in QI, HI, PSI, SI.

      if (! JUMP_P (insn))
	continue;

      int icode1 = recog_memoized (insn);

      if (icode1 != CODE_FOR_cbranchqi4_insn
	  && icode1 != CODE_FOR_cbranchhi4_insn
	  && icode1 != CODE_FOR_cbranchpsi4_insn
	  && icode1 != CODE_FOR_cbranchsi4_insn)
	continue;

      rtx_jump_insn *insn1 = as_a<rtx_jump_insn *> (insn);

      // jmp[0]: We can optimize cbranches that follow cbranch insn1.
      rtx_insn *jmp[2] = { next_insn, nullptr };

      // jmp[1]: A cbranch following the label of cbranch insn1.
      if (LABEL_NUSES (JUMP_LABEL (insn1)) == 1)
	{
	  rtx_insn *code_label1 = JUMP_LABEL_AS_INSN (insn1);
	  rtx_insn *barrier = prev_nonnote_nondebug_insn (code_label1);

	  // When the target label of insn1 is used exactly once and is
	  // not a fallthrough, i.e. is preceded by a barrier, then
	  // consider the insn following that label.
	  if (barrier && BARRIER_P (barrier))
	    jmp[1] = next_nonnote_nondebug_insn (code_label1);
      }

      // With almost certainty, only one of the two possible jumps can
      // be optimized with insn1, but it's hard to tell which one a priori.
      // Just try both.  In the unlikely case where both could be optimized,
      // prefer jmp[0] because eliminating difficult branches is impeded
      // by following label1.

      for (int j = 0; j < 2; ++j)
	if (jmp[j] && JUMP_P (jmp[j])
	    && recog_memoized (jmp[j]) == icode1)
	  {
	    rtx_insn *next
	      = avr_optimize_2ifelse (insn1, as_a<rtx_jump_insn *> (jmp[j]),
				      j == 1 /* follow_label1 */);
	    if (next)
	      {
		next_insn = next;
		break;
	      }
	  }

    } // loop insns

  return 0;
}



//////////////////////////////////////////////////////////////////////////////
// Optimize results of the casesi expander for modes < SImode.

static const pass_data avr_pass_data_casesi =
{
  RTL_PASS,      // type
  "",            // name (will be patched)
  OPTGROUP_NONE, // optinfo_flags
  TV_DF_SCAN,    // tv_id
  0,             // properties_required
  0,             // properties_provided
  0,             // properties_destroyed
  0,             // todo_flags_start
  0              // todo_flags_finish
};

class avr_pass_casesi : public rtl_opt_pass
{
public:
  avr_pass_casesi (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_casesi, ctxt)
  {
    this->name = name;
  }

  bool gate (function *) final override
  {
    return optimize > 0;
  }

  unsigned int execute (function *) final override;
}; // avr_pass_casesi


/* Make one parallel insn with all the patterns from insns i[0]..i[5].  */

static rtx_insn *
avr_parallel_insn_from_insns (rtx_insn *i[5])
{
  rtvec vec = gen_rtvec (5, PATTERN (i[0]), PATTERN (i[1]), PATTERN (i[2]),
			 PATTERN (i[3]), PATTERN (i[4]));
  start_sequence();
  emit (gen_rtx_PARALLEL (VOIDmode, vec));
  rtx_insn *insn = get_insns();
  end_sequence();

  return insn;
}


/* Return true if we see an insn stream generated by casesi expander together
   with an extension to SImode of the switch value.

   If this is the case, fill in the insns from casesi to INSNS[1..5] and
   the SImode extension to INSNS[0].  Moreover, extract the operands of
   pattern casesi_<mode>_sequence forged from the sequence to recog_data.  */

static bool
avr_is_casesi_sequence (basic_block bb, rtx_insn *insn, rtx_insn *insns[5])
{
  rtx set_4, set_0;

  /* A first and quick test for a casesi sequences.  As a side effect of
     the test, harvest respective insns to INSNS[0..4].  */

  if (!(JUMP_P (insns[4] = insn)
	// casesi is the only insn that comes up with UNSPEC_INDEX_JMP,
	// hence the following test ensures that we are actually dealing
	// with code from casesi.
	&& (set_4 = single_set (insns[4]))
	&& UNSPEC == GET_CODE (SET_SRC (set_4))
	&& UNSPEC_INDEX_JMP == XINT (SET_SRC (set_4), 1)

	&& (insns[3] = prev_real_insn (insns[4]))
	&& (insns[2] = prev_real_insn (insns[3]))
	&& (insns[1] = prev_real_insn (insns[2]))

	// Insn prior to casesi.
	&& (insns[0] = prev_real_insn (insns[1]))
	&& (set_0 = single_set (insns[0]))
	&& extend_operator (SET_SRC (set_0), SImode)))
    {
      return false;
    }

  if (dump_file)
    {
      fprintf (dump_file, ";; Sequence from casesi in "
	       "[bb %d]:\n\n", bb->index);
      for (int i = 0; i < 5; i++)
	print_rtl_single (dump_file, insns[i]);
    }

  /* We have to deal with quite some operands.  Extracting them by hand
     would be tedious, therefore wrap the insn patterns into a parallel,
     run recog against it and then use insn extract to get the operands. */

  rtx_insn *xinsn = avr_parallel_insn_from_insns (insns);

  INSN_CODE (xinsn) = recog (PATTERN (xinsn), xinsn, NULL /* num_clobbers */);

  /* Failing to recognize means that someone changed the casesi expander or
     that some passes prior to this one performed some unexpected changes.
     Gracefully drop such situations instead of aborting.  */

  if (INSN_CODE (xinsn) < 0)
    {
      if (dump_file)
	fprintf (dump_file, ";; Sequence not recognized, giving up.\n\n");

      return false;
    }

  gcc_assert (CODE_FOR_casesi_qi_sequence == INSN_CODE (xinsn)
	      || CODE_FOR_casesi_hi_sequence == INSN_CODE (xinsn));

  extract_insn (xinsn);

  // Assert on the anatomy of xinsn's operands we are going to work with.

  gcc_assert (recog_data.n_operands == 11);
  gcc_assert (recog_data.n_dups == 4);

  if (dump_file)
    {
      fprintf (dump_file, ";; Operands extracted:\n");
      for (int i = 0; i < recog_data.n_operands; i++)
	avr_fdump (dump_file, ";; $%d = %r\n", i, recog_data.operand[i]);
      fprintf (dump_file, "\n");
    }

  return true;
}


/* INSNS[1..4] is a sequence as generated by casesi and INSNS[0] is an
   extension of an 8-bit or 16-bit integer to SImode.  XOP contains the
   operands of INSNS as extracted by insn_extract from pattern
   casesi_<mode>_sequence:

      $0: SImode reg switch value as result of $9.
      $1: Negative of smallest index in switch.
      $2: Number of entries in switch.
      $3: Label to table.
      $4: Label if out-of-bounds.
      $5: $0 + $1.
      $6: 3-byte PC: subreg:HI ($5) + label_ref ($3)
	  2-byte PC: subreg:HI ($5)
      $7: HI reg index into table (Z or pseudo)
      $8: R24 or const0_rtx (to be clobbered)
      $9: Extension to SImode of an 8-bit or 16-bit integer register $10.
      $10: QImode or HImode register input of $9.

   Try to optimize this sequence, i.e. use the original HImode / QImode
   switch value instead of SImode.  */

static void
avr_optimize_casesi (rtx_insn *insns[5], rtx *xop)
{
  // Original mode of the switch value; this is QImode or HImode.
  machine_mode mode = GET_MODE (xop[10]);

  // How the original switch value was extended to SImode; this is
  // SIGN_EXTEND or ZERO_EXTEND.
  rtx_code code = GET_CODE (xop[9]);

  // Lower index, upper index (plus one) and range of case calues.
  HOST_WIDE_INT low_idx = -INTVAL (xop[1]);
  HOST_WIDE_INT num_idx = INTVAL (xop[2]);
  HOST_WIDE_INT hig_idx = low_idx + num_idx;

  // Maximum ranges of (un)signed QImode resp. HImode.
  unsigned umax = QImode == mode ? 0xff : 0xffff;
  int imax = QImode == mode ? 0x7f : 0x7fff;
  int imin = -imax - 1;

  // Testing the case range and whether it fits into the range of the
  // (un)signed mode.  This test should actually always pass because it
  // makes no sense to have case values outside the mode range.  Notice
  // that case labels which are unreachable because they are outside the
  // mode of the switch value (e.g. "case -1" for uint8_t) have already
  // been thrown away by the middle-end.

  if (SIGN_EXTEND == code
      && low_idx >= imin
      && hig_idx <= imax)
    {
      // ok
    }
  else if (ZERO_EXTEND == code
	   && low_idx >= 0
	   && (unsigned) hig_idx <= umax)
    {
      // ok
    }
  else
    {
      if (dump_file)
	fprintf (dump_file, ";; Case ranges too big, giving up.\n\n");
      return;
    }

  // Do normalization of switch value $10 and out-of-bound check in its
  // original mode instead of in SImode.  Use a newly created pseudo.
  // This will replace insns[1..2].

  start_sequence();

  rtx reg = copy_to_mode_reg (mode, xop[10]);

  rtx (*gen_add)(rtx,rtx,rtx) = QImode == mode ? gen_addqi3 : gen_addhi3;
  rtx (*gen_cbranch)(rtx,rtx,rtx,rtx)
    = QImode == mode ? gen_cbranchqi4 : gen_cbranchhi4;

  emit_insn (gen_add (reg, reg, gen_int_mode (-low_idx, mode)));
  rtx op0 = reg; rtx op1 = gen_int_mode (num_idx, mode);
  rtx labelref = copy_rtx (xop[4]);
  rtx xbranch = gen_cbranch (gen_rtx_fmt_ee (GTU, VOIDmode, op0, op1),
			     op0, op1, labelref);
  rtx_insn *cbranch = emit_jump_insn (xbranch);
  JUMP_LABEL (cbranch) = xop[4];
  ++LABEL_NUSES (xop[4]);

  rtx_insn *seq1 = get_insns();
  rtx_insn *last1 = get_last_insn();
  end_sequence();

  emit_insn_after (seq1, insns[2]);

  // After the out-of-bounds test and corresponding branch, use a
  // 16-bit index.  If QImode is used, extend it to HImode first.
  // This will replace insns[4].

  start_sequence();

  if (QImode == mode)
    reg = force_reg (HImode, gen_rtx_fmt_e (code, HImode, reg));

  rtx pat_4 = AVR_3_BYTE_PC
    ? gen_movhi (xop[7], reg)
    : gen_addhi3 (xop[7], reg, gen_rtx_LABEL_REF (VOIDmode, xop[3]));

  emit_insn (pat_4);

  rtx_insn *seq2 = get_insns();
  rtx_insn *last2 = get_last_insn();
  end_sequence();

  emit_insn_after (seq2, insns[3]);

  if (dump_file)
    {
      fprintf (dump_file, ";; New insns: ");

      for (rtx_insn *insn = seq1; ; insn = NEXT_INSN (insn))
	{
	  fprintf (dump_file, "%d, ", INSN_UID (insn));
	  if (insn == last1)
	    break;
	}
      for (rtx_insn *insn = seq2; ; insn = NEXT_INSN (insn))
	{
	  fprintf (dump_file, "%d%s", INSN_UID (insn),
		   insn == last2 ? ".\n\n" : ", ");
	  if (insn == last2)
	    break;
	}

      fprintf (dump_file, ";; Deleting insns: %d, %d, %d.\n\n",
	       INSN_UID (insns[1]), INSN_UID (insns[2]), INSN_UID (insns[3]));
    }

  // Pseudodelete the SImode and subreg of SImode insns.  We don't care
  // about the extension insns[0]: Its result is now unused and other
  // passes will clean it up.

  SET_INSN_DELETED (insns[1]);
  SET_INSN_DELETED (insns[2]);
  SET_INSN_DELETED (insns[3]);
}


unsigned int
avr_pass_casesi::execute (function *func)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, func)
    {
      rtx_insn *insn, *insns[5];

      FOR_BB_INSNS (bb, insn)
	{
	  if (avr_is_casesi_sequence (bb, insn, insns))
	    {
	      avr_optimize_casesi (insns, recog_data.operand);
	    }
	}
    }

  return 0;
}

} // anonymous namespace

/* Perform some extra checks on operands of casesi_<mode>_sequence.
   Not all operand dependencies can be described by means of predicates.
   This function performs left over checks and should always return true.
   Returning false means that someone changed the casesi expander but did
   not adjust casesi_<mode>_sequence.  */

bool
avr_casei_sequence_check_operands (rtx *xop)
{
  rtx sub_5 = NULL_RTX;

  if (AVR_HAVE_EIJMP_EICALL
      // The last clobber op of the tablejump.
      && xop[8] == all_regs_rtx[REG_24])
    {
      // $6 is: (subreg:SI ($5) 0)
      sub_5 = xop[6];
    }

  if (!AVR_HAVE_EIJMP_EICALL
      // $6 is: (plus:HI (subreg:SI ($5) 0)
      //		 (label_ref ($3)))
      && PLUS == GET_CODE (xop[6])
      && LABEL_REF == GET_CODE (XEXP (xop[6], 1))
      && rtx_equal_p (xop[3], XEXP (XEXP (xop[6], 1), 0))
      // The last clobber op of the tablejump.
      && xop[8] == const0_rtx)
    {
      sub_5 = XEXP (xop[6], 0);
    }

  if (sub_5
      && SUBREG_P (sub_5)
      && SUBREG_BYTE (sub_5) == 0
      && rtx_equal_p (xop[5], SUBREG_REG (sub_5)))
    return true;

  if (dump_file)
    fprintf (dump_file, "\n;; Failed condition for casesi_<mode>_sequence\n\n");

  return false;
}

namespace
{


//////////////////////////////////////////////////////////////////////////////
// Find more POST_INC and PRE_DEC cases.

static const pass_data avr_pass_data_fuse_add =
{
  RTL_PASS,	    // type
  "",		    // name (will be patched)
  OPTGROUP_NONE,    // optinfo_flags
  TV_MACH_DEP,	    // tv_id
  0,		    // properties_required
  0,		    // properties_provided
  0,		    // properties_destroyed
  0,		    // todo_flags_start
  TODO_df_finish    // todo_flags_finish
};

class avr_pass_fuse_add : public rtl_opt_pass
{
public:
  avr_pass_fuse_add (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_fuse_add, ctxt)
  {
    this->name = name;
  }

  // Cloning is required because we are running one instance of the pass
  // before peephole2. and a second one after cprop_hardreg.
  opt_pass * clone () final override
  {
    return make_avr_pass_fuse_add (m_ctxt);
  }

  bool gate (function *) final override
  {
    return optimize && avr_fuse_add > 0;
  }

  unsigned int execute (function *) final override;

  struct Some_Insn
  {
    rtx_insn *insn = nullptr;
    rtx dest, src;
    bool valid () const { return insn != nullptr; }
    void set_deleted ()
    {
      gcc_assert (insn);
      SET_INSN_DELETED (insn);
      insn = nullptr;
    }
  };

  // If .insn is not NULL, then this is a  reg:HI += const_int
  // of an address register.
  struct Add_Insn : Some_Insn
  {
    rtx addend;
    int regno;
    Add_Insn () {}
    Add_Insn (rtx_insn *insn);
  };

  // If .insn is not NULL, then this sets an address register
  // to a constant value.
  struct Ldi_Insn : Some_Insn
  {
    int regno;
    Ldi_Insn () {}
    Ldi_Insn (rtx_insn *insn);
  };

  // If .insn is not NULL, then this is a load or store insn where the
  // address is REG or POST_INC with an address register.
  struct Mem_Insn : Some_Insn
  {
    rtx reg_or_0, mem, addr, addr_reg;
    int addr_regno;
    rtx_code addr_code;
    machine_mode mode;
    addr_space_t addr_space;
    bool store_p, volatile_p;
    Mem_Insn () {}
    Mem_Insn (rtx_insn *insn);
  };

  rtx_insn *fuse_ldi_add (Ldi_Insn &prev_ldi, Add_Insn &add);
  rtx_insn *fuse_add_add (Add_Insn &prev_add, Add_Insn &add);
  rtx_insn *fuse_add_mem (Add_Insn &prev_add, Mem_Insn &mem);
  rtx_insn *fuse_mem_add (Mem_Insn &prev_mem, Add_Insn &add);
}; // avr_pass_fuse_add


/* Describe properties of AVR's indirect load and store instructions
   LD, LDD, ST, STD, LPM, ELPM depending on register number, volatility etc.
   Rules for "volatile" accesses are:

	 | Xmega	   |  non-Xmega
   ------+-----------------+----------------
   load  | read LSB first  | read LSB first
   store | write LSB first | write MSB first
*/

struct AVR_LdSt_Props
{
  bool has_postinc, has_predec, has_ldd;
  // The insn printers will use POST_INC or PRE_DEC addressing, no matter
  // what adressing modes we are feeding into them.
  bool want_postinc, want_predec;

  AVR_LdSt_Props (int regno, bool store_p, bool volatile_p, addr_space_t as)
  {
    bool generic_p = ADDR_SPACE_GENERIC_P (as);
    bool flashx_p = ! generic_p && as != ADDR_SPACE_MEMX;
    has_postinc = generic_p || (flashx_p && regno == REG_Z);
    has_predec = generic_p;
    has_ldd = ! AVR_TINY && generic_p && (regno == REG_Y || regno == REG_Z);
    want_predec  = volatile_p && generic_p && ! AVR_XMEGA && store_p;
    want_postinc = volatile_p && generic_p && (AVR_XMEGA || ! store_p);
    want_postinc |= flashx_p && regno == REG_Z;
  }

  AVR_LdSt_Props (const avr_pass_fuse_add::Mem_Insn &m)
    : AVR_LdSt_Props (m.addr_regno, m.store_p, m.volatile_p, m.addr_space)
  {
    gcc_assert (m.valid ());
  }
};


/* Emit a single_set that clobbers REG_CC.  */

static rtx_insn *
emit_move_ccc (rtx dest, rtx src)
{
  return emit_insn (gen_gen_move_clobbercc (dest, src));
}


/* Emit a single_set that clobbers REG_CC after insn AFTER.  */

static rtx_insn *
emit_move_ccc_after (rtx dest, rtx src, rtx_insn *after)
{
  return emit_insn_after (gen_gen_move_clobbercc (dest, src), after);
}

static bool
reg_seen_between_p (const_rtx reg, const rtx_insn *from, const rtx_insn *to)
{
  return (reg_used_between_p (reg, from, to)
	  || reg_set_between_p (reg, from, to));
}


static void
avr_maybe_adjust_cfa (rtx_insn *insn, rtx reg, int addend)
{
  if (addend
      && frame_pointer_needed
      && REGNO (reg) == FRAME_POINTER_REGNUM
      && avr_fuse_add == 3)
    {
      rtx plus = plus_constant (Pmode, reg, addend);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_ADJUST_CFA, gen_rtx_SET (reg, plus));
    }
}


// If successful, this represents a SET of a pointer register to a constant.
avr_pass_fuse_add::Ldi_Insn::Ldi_Insn (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return;

  src = SET_SRC (set);
  dest = SET_DEST (set);

  if (REG_P (dest)
      && GET_MODE (dest) == Pmode
      && IN_RANGE (regno = REGNO (dest), REG_X, REG_Z)
      && CONSTANT_P (src))
    {
      this->insn = insn;
    }
}

// If successful, this represents a PLUS with CONST_INT of a pointer
// register X, Y or Z.  Otherwise, the object is not valid().
avr_pass_fuse_add::Add_Insn::Add_Insn (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return;

  src = SET_SRC (set);
  dest = SET_DEST (set);
  if (REG_P (dest)
      // We are only interested in PLUSes that change address regs.
      && GET_MODE (dest) == Pmode
      && IN_RANGE (regno = REGNO (dest), REG_X, REG_Z)
      && PLUS == GET_CODE (src)
      && rtx_equal_p (XEXP (src, 0), dest)
      && CONST_INT_P (XEXP (src, 1)))
    {
      // This is reg:HI += const_int.
      addend = XEXP (src, 1);
      this->insn = insn;
    }
}

// If successful, this represents a load or store insn where the addressing
// mode uses pointer register X, Y or Z.  Otherwise, the object is not valid().
avr_pass_fuse_add::avr_pass_fuse_add::Mem_Insn::Mem_Insn (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return;

  src = SET_SRC (set);
  dest = SET_DEST (set);
  mode = GET_MODE (dest);

  if (MEM_P (dest)
      && (REG_P (src) || src == CONST0_RTX (mode)))
    {
      reg_or_0 = src;
      mem = dest;
    }
  else if (REG_P (dest) && MEM_P (src))
    {
      reg_or_0 = dest;
      mem = src;
    }
  else
    return;

  if (avr_mem_memx_p (mem)
      || avr_load_libgcc_p (mem))
    return;

  addr = XEXP (mem, 0);
  addr_code = GET_CODE (addr);

  if (addr_code == REG)
    addr_reg = addr;
  else if (addr_code == POST_INC || addr_code == PRE_DEC)
    addr_reg = XEXP (addr, 0);
  else
    return;

  addr_regno = REGNO (addr_reg);

  if (avr_fuse_add == 2
      && frame_pointer_needed
      && addr_regno == FRAME_POINTER_REGNUM)
    MEM_VOLATILE_P (mem) = 0;

  if (reg_overlap_mentioned_p (reg_or_0, addr) // Can handle CONSTANT_P.
      || addr_regno > REG_Z
      || avr_mem_memx_p (mem)
      // The following optimizations only handle REG and POST_INC,
      // so that's all what we allow here.
      || (addr_code != REG && addr_code != POST_INC))
    return;

  addr_space = MEM_ADDR_SPACE (mem);
  volatile_p = MEM_VOLATILE_P (mem);
  store_p = MEM_P (dest);

  // Turn this "valid".
  this->insn = insn;
}

/* Try to combine a Ldi insn with a PLUS CONST_INT addend to one Ldi insn.
   If LDI is valid, then it precedes ADD in the same block.
   When a replacement is found, a new insn is emitted and the old insns
   are pseudo-deleted.  The returned insn is the point where the calling
   scanner should continue.  When no replacement is found, nullptr is
   returned and nothing changed.  */

rtx_insn *
avr_pass_fuse_add::fuse_ldi_add (Ldi_Insn &ldi, Add_Insn &add)
{
  if (! ldi.valid ()
      || reg_seen_between_p (ldi.dest, ldi.insn, add.insn))
    {
      // If something is between the Ldi and the current insn, we can
      // set the Ldi invalid to speed future scans.
      return ldi.insn = nullptr;
    }

  // Found a Ldi with const and a PLUS insns in the same BB,
  // and with no interfering insns between them.

  // Emit new Ldi with the sum of the original offsets after the old Ldi.
  rtx xval = plus_constant (Pmode, ldi.src, INTVAL (add.addend));

  rtx_insn *insn = emit_move_ccc_after (ldi.dest, xval, ldi.insn);
  avr_dump (";; new Ldi[%d] insn %d after %d: R%d = %r\n\n", ldi.regno,
	    INSN_UID (insn), INSN_UID (ldi.insn), ldi.regno, xval);

  rtx_insn *next = NEXT_INSN (add.insn);
  ldi.set_deleted ();
  add.set_deleted ();

  return next;
}

/* Try to combine two PLUS insns with CONST_INT addend to one such insn.
   If PREV_ADD is valid, then it precedes ADD in the same basic block.
   When a replacement is found, a new insn is emitted and the old insns
   are pseudo-deleted.  The returned insn is the point where the calling
   scanner should continue.  When no replacement is found, nullptr is
   returned and nothing changed.  */

rtx_insn *
avr_pass_fuse_add::fuse_add_add (Add_Insn &prev_add, Add_Insn &add)
{
  if (! prev_add.valid ()
      || reg_seen_between_p (add.dest, prev_add.insn, add.insn))
    {
      // If something is between the previous Add and the current insn,
      // we can set the previous Add invalid to speed future scans.
      return prev_add.insn = nullptr;
    }

  // Found two PLUS insns in the same BB, and with no interfering
  // insns between them.
  rtx plus = plus_constant (Pmode, add.src, INTVAL (prev_add.addend));

  rtx_insn *next;
  if (REG_P (plus))
    {
      avr_dump (";; Add[%d] from %d annihilates %d\n\n", add.regno,
		INSN_UID (prev_add.insn), INSN_UID (add.insn));
      next = NEXT_INSN (add.insn);
    }
  else
    {
      // Emit after the current insn, so that it will be picked
      // up as next valid Add insn.
      next = emit_move_ccc_after (add.dest, plus, add.insn);
      avr_dump (";; #1 new Add[%d] insn %d after %d: R%d += %d\n\n",
		add.regno, INSN_UID (next), INSN_UID (add.insn),
		add.regno, (int) INTVAL (XEXP (plus, 1)));
      gcc_assert (GET_CODE (plus) == PLUS);
    }

  add.set_deleted ();
  prev_add.set_deleted ();

  return next;
}

/* Try to combine a PLUS of the address register with a load or store insn.
   If ADD is valid, then it precedes MEM in the same basic block.
   When a replacement is found, a new insn is emitted and the old insns
   are pseudo-deleted.  The returned insn is the point where the calling
   scanner should continue.  When no replacement is found, nullptr is
   returned and nothing changed.  */

rtx_insn *
avr_pass_fuse_add::fuse_add_mem (Add_Insn &add, Mem_Insn &mem)
{
  if (! add.valid ()
      || reg_seen_between_p (add.dest, add.insn, mem.insn))
    {
      // If something is between the Add and the current insn, we can
      // set the Add invalid to speed future scans.
      return add.insn = nullptr;
    }

  AVR_LdSt_Props ap { mem };

  int msize = GET_MODE_SIZE (mem.mode);

  // The mem insn really wants PRE_DEC.
  bool case1 = ((mem.addr_code == REG || mem.addr_code == POST_INC)
		&& msize > 1 && ap.want_predec && ! ap.has_ldd);

  // The offset can be consumed by a PRE_DEC.
  bool case2 = (- INTVAL (add.addend) == msize
		&& (mem.addr_code == REG || mem.addr_code == POST_INC)
		&& ap.has_predec && ! ap.want_postinc);

  if (! case1 && ! case2)
    return nullptr;

  // Change from REG or POST_INC to PRE_DEC.
  rtx xmem = change_address (mem.mem, mem.mode,
			     gen_rtx_PRE_DEC (Pmode, mem.addr_reg));
  rtx dest = mem.store_p ? xmem : mem.reg_or_0;
  rtx src  = mem.store_p ? mem.reg_or_0 : xmem;

  rtx_insn *next = emit_move_ccc_after (dest, src, mem.insn);
  add_reg_note (next, REG_INC, mem.addr_reg);
  avr_dump (";; new Mem[%d] insn %d after %d: %r = %r\n\n", mem.addr_regno,
	    INSN_UID (next), INSN_UID (mem.insn), dest, src);

  // Changing REG or POST_INC -> PRE_DEC means that the addend before
  // the memory access must be increased by the size of the access,
  rtx plus = plus_constant (Pmode, add.src, msize);
  if (! REG_P (plus))
    {
      rtx_insn *insn = emit_move_ccc_after (add.dest, plus, add.insn);
      avr_dump (";; #2 new Add[%d] insn %d after %d: R%d += %d\n\n",
		add.regno, INSN_UID (insn), INSN_UID (add.insn),
		add.regno, (int) INTVAL (XEXP (plus, 1)));
      gcc_assert (GET_CODE (plus) == PLUS);
    }
  else
    avr_dump (";; Add[%d] insn %d consumed into %d\n\n",
	      add.regno, INSN_UID (add.insn), INSN_UID (next));

  // Changing POST_INC -> PRE_DEC means that the addend after the mem has to be
  // the size of the access.  The hope is that this new add insn may be unused.
  if (mem.addr_code == POST_INC)
    {
      plus = plus_constant (Pmode, add.dest, msize);
      rtx_insn *next2 = emit_move_ccc_after (add.dest, plus, next);
      avr_dump (";; #3 new Add[%d] insn %d after %d: R%d += %d\n\n", add.regno,
		INSN_UID (next2), INSN_UID (next), add.regno, msize);
      next = next2;
    }

  add.set_deleted ();
  mem.set_deleted ();

  return next;
}

/* Try to combine a load or store insn with a PLUS of the address register.
   If MEM is valid, then it precedes ADD in the same basic block.
   When a replacement is found, a new insn is emitted and the old insns
   are pseudo-deleted.  The returned insn is the point where the calling
   scanner should continue.  When no replacement is found, nullptr is
   returned and nothing changed.  */

rtx_insn *
avr_pass_fuse_add::fuse_mem_add (Mem_Insn &mem, Add_Insn &add)
{
  if (! mem.valid ()
      || reg_seen_between_p (add.dest, mem.insn, add.insn))
    {
      // If something is between the Mem and the current insn, we can
      // set the Mem invalid to speed future scans.
      return mem.insn = nullptr;
    }

  AVR_LdSt_Props ap { mem };

  int msize = GET_MODE_SIZE (mem.mode);

  // The add insn can be consumed by a POST_INC.
  bool case1 = (mem.addr_code == REG
		&& INTVAL (add.addend) == msize
		&& ap.has_postinc && ! ap.want_predec);

  // There are cases where even a partial consumption of the offset is better.
  // This are the cases where no LD+offset addressing is available, because
  // the address register is obviously used after the mem insn, and a mem insn
  // with REG addressing mode will have to restore the address.
  bool case2 = (mem.addr_code == REG
		&& msize > 1 && ap.want_postinc && ! ap.has_ldd);

  if (! case1 && ! case2)
    return nullptr;

  // Change addressing mode from REG to POST_INC.
  rtx xmem = change_address (mem.mem, mem.mode,
			     gen_rtx_POST_INC (Pmode, mem.addr_reg));
  rtx dest = mem.store_p ? xmem : mem.reg_or_0;
  rtx src  = mem.store_p ? mem.reg_or_0 : xmem;

  rtx_insn *insn = emit_move_ccc_after (dest, src, mem.insn);
  add_reg_note (insn, REG_INC, mem.addr_reg);
  avr_dump (";; new Mem[%d] insn %d after %d: %r = %r\n\n", add.regno,
	    INSN_UID (insn), INSN_UID (mem.insn), dest, src);

  rtx_insn *next = NEXT_INSN (add.insn);

  // Changing REG -> POST_INC means that the post addend must be
  // decreased by the size of the access.
  rtx plus = plus_constant (Pmode, add.src, -msize);
  if (! REG_P (plus))
    {
      next = emit_move_ccc_after (mem.addr_reg, plus, add.insn);
      avr_dump (";; #4 new Add[%d] insn %d after %d: R%d += %d\n\n",
		add.regno, INSN_UID (next), INSN_UID (add.insn),
		add.regno, (int) INTVAL (XEXP (plus, 1)));
      gcc_assert (GET_CODE (plus) == PLUS);
    }
  else
    avr_dump (";; Add[%d] insn %d consumed into %d\n\n",
	      add.regno, INSN_UID (add.insn), INSN_UID (insn));

  add.set_deleted ();
  mem.set_deleted ();

  return next;
}

/* Try to post-reload combine PLUS with CONST_INt of pointer registers with:
   - Sets to a constant address.
   - PLUS insn of that kind.
   - Indirect loads and stores.
   In almost all cases, combine opportunities arise from the preparation
   done by `avr_split_fake_addressing_move', but in some rare cases combinations
   are found for the ordinary cores, too.
      As we consider at most one Mem insn per try, there may still be missed
   optimizations like  POST_INC + PLUS + POST_INC  might be performed
   as  PRE_DEC + PRE_DEC  for two adjacent locations.  */

unsigned int
avr_pass_fuse_add::execute (function *func)
{
  df_note_add_problem ();
  df_analyze ();

  int n_add = 0, n_mem = 0, n_ldi = 0;
  basic_block bb;

  FOR_EACH_BB_FN (bb, func)
    {
      Ldi_Insn prev_ldi_insns[REG_32];
      Add_Insn prev_add_insns[REG_32];
      Mem_Insn prev_mem_insns[REG_32];
      rtx_insn *insn, *curr;

      avr_dump ("\n;; basic block %d\n\n", bb->index);

      FOR_BB_INSNS_SAFE (bb, insn, curr)
	{
	  rtx_insn *next = nullptr;
	  Ldi_Insn ldi_insn { insn };
	  Add_Insn add_insn { insn };
	  Mem_Insn mem_insn { insn };

	  if (add_insn.valid ())
	    {
	      // Found reg:HI += const_int
	      avr_dump (";; insn %d: Add[%d]: R%d += %d\n\n",
			INSN_UID (add_insn.insn), add_insn.regno,
			add_insn.regno, (int) INTVAL (add_insn.addend));
	      Ldi_Insn &prev_ldi_insn = prev_ldi_insns[add_insn.regno];
	      Add_Insn &prev_add_insn = prev_add_insns[add_insn.regno];
	      Mem_Insn &prev_mem_insn = prev_mem_insns[add_insn.regno];
	      if ((next = fuse_ldi_add (prev_ldi_insn, add_insn)))
		curr = next, n_ldi += 1;
	      else if ((next = fuse_add_add (prev_add_insn, add_insn)))
		curr = next, n_add += 1;
	      else if ((next = fuse_mem_add (prev_mem_insn, add_insn)))
		curr = next, n_mem += 1;
	      else
		prev_add_insn = add_insn;
	    }
	  else if (mem_insn.valid ())
	    {
	      int addr_regno = REGNO (mem_insn.addr_reg);
	      avr_dump (";; insn %d: Mem[%d]: %r = %r\n\n",
			INSN_UID (mem_insn.insn), addr_regno,
			mem_insn.dest, mem_insn.src);
	      Add_Insn &prev_add_insn = prev_add_insns[addr_regno];
	      if ((next = fuse_add_mem (prev_add_insn, mem_insn)))
		curr = next, n_mem += 1;
	      else
		prev_mem_insns[addr_regno] = mem_insn;
	    }
	  else if (ldi_insn.valid ())
	    {
	      if (! CONST_INT_P (ldi_insn.src))
		avr_dump (";; insn %d: Ldi[%d]: R%d = %r\n\n",
			  INSN_UID (ldi_insn.insn), ldi_insn.regno,
			  ldi_insn.regno, ldi_insn.src);
	      prev_ldi_insns[ldi_insn.regno] = ldi_insn;
	    }
	} // for insns
    } // for BBs

  avr_dump (";; Function %f: Found %d changes: %d ldi, %d add, %d mem.\n",
	    n_ldi + n_add + n_mem, n_ldi, n_add, n_mem);

  return 0;
}



//////////////////////////////////////////////////////////////////////////////
// Determine whether an ISR may use the __gcc_isr pseudo-instruction.

static const pass_data avr_pass_data_pre_proep =
{
  RTL_PASS,	    // type
  "",		    // name (will be patched)
  OPTGROUP_NONE,    // optinfo_flags
  TV_DF_SCAN,	    // tv_id
  0,		    // properties_required
  0,		    // properties_provided
  0,		    // properties_destroyed
  0,		    // todo_flags_start
  0		    // todo_flags_finish
};

class avr_pass_pre_proep : public rtl_opt_pass
{
public:
  avr_pass_pre_proep (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_pre_proep, ctxt)
  {
    this->name = name;
  }

  void compute_maybe_gasisr (function *);

  unsigned int execute (function *fun) final override
  {
    if (avr_gasisr_prologues
	// Whether this function is an ISR worth scanning at all.
	&& !fun->machine->is_no_gccisr
	&& (fun->machine->is_interrupt
	    || fun->machine->is_signal)
	&& !cfun->machine->is_naked
	// Paranoia: Non-local gotos and labels that might escape.
	&& !cfun->calls_setjmp
	&& !cfun->has_nonlocal_label
	&& !cfun->has_forced_label_in_static)
      {
	compute_maybe_gasisr (fun);
      }

    return 0;
  }

}; // avr_pass_pre_proep


/* Set fun->machine->gasisr.maybe provided we don't find anything that
   prohibits GAS generating parts of ISR prologues / epilogues for us.  */

void
avr_pass_pre_proep::compute_maybe_gasisr (function *fun)
{
  // Don't use BB iterators so that we see JUMP_TABLE_DATA.

  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      // Transparent calls always use [R]CALL and are filtered out by GAS.
      // ISRs don't use -mcall-prologues, hence what remains to be filtered
      // out are open coded (tail) calls.

      if (CALL_P (insn))
	return;

      // __tablejump2__ clobbers something and is targeted by JMP so
      // that GAS won't see its usage.

      if (AVR_HAVE_JMP_CALL
	  && JUMP_TABLE_DATA_P (insn))
	return;

      // Non-local gotos not seen in *FUN.

      if (JUMP_P (insn)
	  && find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
	return;
    }

  fun->machine->gasisr.maybe = 1;
}



//////////////////////////////////////////////////////////////////////////////
// Late recomputation of notes so we can use `reg_unused_after()' and friends.

static const pass_data avr_pass_data_recompute_notes =
{
  RTL_PASS,      // type
  "",            // name (will be patched)
  OPTGROUP_NONE, // optinfo_flags
  TV_DF_SCAN,    // tv_id
  0,             // properties_required
  0,             // properties_provided
  0,             // properties_destroyed
  0,             // todo_flags_start
  TODO_df_finish | TODO_df_verify // todo_flags_finish
};

class avr_pass_recompute_notes : public rtl_opt_pass
{
public:
  avr_pass_recompute_notes (gcc::context *ctxt, const char *name)
    : rtl_opt_pass (avr_pass_data_recompute_notes, ctxt)
  {
    this->name = name;
  }

  unsigned int execute (function *) final override
  {
    df_note_add_problem ();
    df_analyze ();

    return 0;
  }
}; // avr_pass_recompute_notes

} // anonymous namespace



//////////////////////////////////////////////////////////////////////////////
// Function visible and used outside this module.

/* During reload, we allow much more addresses than Reduced Tiny actually
   supports.  Split them after reload in order to get closer to the
   core's capabilities.  This sets the stage for pass .avr-fuse-add.  */

bool
avr_split_fake_addressing_move (rtx_insn * /*insn*/, rtx *xop)
{
  bool store_p = false;
  rtx mem, reg_or_0;

  if (REG_P (xop[0]) && MEM_P (xop[1]))
    {
      reg_or_0 = xop[0];
      mem = xop[1];
    }
  else if (MEM_P (xop[0])
	   && (REG_P (xop[1])
	       || xop[1] == CONST0_RTX (GET_MODE (xop[0]))))
    {
      mem = xop[0];
      reg_or_0 = xop[1];
      store_p = true;
    }
  else
    return false;

  machine_mode mode = GET_MODE (mem);
  rtx base, addr = XEXP (mem, 0);
  rtx_code addr_code = GET_CODE (addr);

  if (REG_P (reg_or_0)
      && reg_overlap_mentioned_p (reg_or_0, addr))
    return false;
  else if (addr_code == PLUS || addr_code == PRE_DEC || addr_code == POST_INC)
    base = XEXP (addr, 0);
  else if (addr_code == REG)
    base = addr;
  else
    return false;

  if (REGNO (base) > REG_Z)
    return false;

  if (! AVR_TINY
      // Only keep base registers that can't do PLUS addressing.
      && ((REGNO (base) != REG_X
	   && ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (mem)))
	  || avr_load_libgcc_p (mem)
	  || avr_mem_memx_p (mem)))
    return false;

  bool volatile_p = MEM_VOLATILE_P (mem);
  bool mem_volatile_p = false;
  if (frame_pointer_needed
      && REGNO (base) == FRAME_POINTER_REGNUM)
    {
      if (avr_fuse_add < 2
	  // Be a projection (we always split PLUS).
	  || (avr_fuse_add == 2 && volatile_p && addr_code != PLUS))
	return false;

      // Changing the frame pointer locally may confuse later passes
      // like .dse2 which don't track changes of FP, not even when
      // respective CFA notes are present.  An example is pr22141-1.c.
      if (avr_fuse_add == 2)
	mem_volatile_p = true;
    }

  rtx_code new_code = UNKNOWN;
  HOST_WIDE_INT add = 0, sub = 0;
  int msize = GET_MODE_SIZE (mode);

  AVR_LdSt_Props ap { REGNO (base), store_p, volatile_p, ADDR_SPACE_GENERIC };

  switch (addr_code)
    {
    default:
      return false;

    case PLUS:
      add = INTVAL (XEXP (addr, 1));
      if (msize == 1)
	{
	  new_code = REG;
	  sub = -add;
	}
      else if (ap.want_predec)
	{
	  // volatile stores prefer PRE_DEC (MSB first)
	  sub = -add;
	  add += msize;
	  new_code = PRE_DEC;
	}
      else
	{
	  new_code = POST_INC;
	  sub = -add - msize;
	}
      break;

    case POST_INC:
      // volatile stores prefer PRE_DEC (MSB first)
      if (msize > 1 && ap.want_predec)
	{
	  add = msize;
	  new_code = PRE_DEC;
	  sub = msize;
	  break;
	}
      return false;

    case PRE_DEC:
      // volatile loads prefer POST_INC (LSB first)
      if (msize > 1 && ap.want_postinc)
	{
	  add = -msize;
	  new_code = POST_INC;
	  sub = -msize;
	  break;
	}
      return false;

    case REG:
      if (msize == 1)
	return false;

      if (ap.want_predec)
	{
	  add = msize;
	  new_code = PRE_DEC;
	  sub = 0;
	}
      else
	{
	  add = 0;
	  new_code = POST_INC;
	  sub = -msize;
	}
      break;
    } // switch addr_code

  rtx_insn *insn;

  if (add)
    {
      insn = emit_move_ccc (base, plus_constant (Pmode, base, add));
      avr_maybe_adjust_cfa (insn, base, add);
    }

  rtx new_addr = new_code == REG
    ? base
    : gen_rtx_fmt_e (new_code, Pmode, base);

  rtx new_mem = change_address (mem, mode, new_addr);
  if (mem_volatile_p)
    MEM_VOLATILE_P (new_mem) = 1;

  insn = emit_move_ccc (store_p ? new_mem : reg_or_0,
			store_p ? reg_or_0 : new_mem);
  if (auto_inc_p (new_addr))
    {
      add_reg_note (insn, REG_INC, base);
      int off = new_code == POST_INC ? msize : -msize;
      avr_maybe_adjust_cfa (insn, base, off);
    }

  if (sub)
    {
      insn = emit_move_ccc (base, plus_constant (Pmode, base, sub));
      avr_maybe_adjust_cfa (insn, base, sub);
    }

  return true;
}



// Functions  make_<pass-name> (gcc::context*)  where <pass-name> is
// according to the pass declaration in avr-passes.def.  GCC's pass
// manager uses these function to create the respective pass object.

// Optimize results of the casesi expander for modes < SImode.

rtl_opt_pass *
make_avr_pass_casesi (gcc::context *ctxt)
{
  return new avr_pass_casesi (ctxt, "avr-casesi");
}

// Try to replace 2 cbranch insns with 1 comparison and 2 branches.

rtl_opt_pass *
make_avr_pass_ifelse (gcc::context *ctxt)
{
  return new avr_pass_ifelse (ctxt, "avr-ifelse");
}

// Determine whether an ISR may use the __gcc_isr pseudo-instruction.

rtl_opt_pass *
make_avr_pass_pre_proep (gcc::context *ctxt)
{
  return new avr_pass_pre_proep (ctxt, "avr-pre-proep");
}

// Find more POST_INC and PRE_DEC cases.

rtl_opt_pass *
make_avr_pass_fuse_add (gcc::context *ctxt)
{
  return new avr_pass_fuse_add (ctxt, "avr-fuse-add");
}

// Late recomputation of notes so we can use `reg_unused_after()' and friends.

rtl_opt_pass *
make_avr_pass_recompute_notes (gcc::context *ctxt)
{
  return new avr_pass_recompute_notes (ctxt, "avr-notes-free-cfg");
}
