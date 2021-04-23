/* Optimize jump instructions, for GNU compiler.
   Copyright (C) 1987-2020 Free Software Foundation, Inc.

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

/* This is the pathetic reminder of old fame of the jump-optimization pass
   of the compiler.  Now it contains basically a set of utility functions to
   operate with jumps.

   Each CODE_LABEL has a count of the times it is used
   stored in the LABEL_NUSES internal field, and each JUMP_INSN
   has one label that it refers to stored in the
   JUMP_LABEL internal field.  With this we can detect labels that
   become unused because of the deletion of all the jumps that
   formerly used them.  The JUMP_LABEL info is sometimes looked
   at by later passes.  For return insns, it contains either a
   RETURN or a SIMPLE_RETURN rtx.

   The subroutines redirect_jump and invert_jump are used
   from other passes as well.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfgrtl.h"
#include "rtl-iter.h"

/* Optimize jump y; x: ... y: jumpif... x?
   Don't know if it is worth bothering with.  */
/* Optimize two cases of conditional jump to conditional jump?
   This can never delete any instruction or make anything dead,
   or even change what is live at any point.
   So perhaps let combiner do it.  */

static void init_label_info (rtx_insn *);
static void mark_all_labels (rtx_insn *);
static void mark_jump_label_1 (rtx, rtx_insn *, bool, bool);
static void mark_jump_label_asm (rtx, rtx_insn *);
static void redirect_exp_1 (rtx *, rtx, rtx, rtx_insn *);
static int invert_exp_1 (rtx, rtx_insn *);

/* Worker for rebuild_jump_labels and rebuild_jump_labels_chain.  */
static void
rebuild_jump_labels_1 (rtx_insn *f, bool count_forced)
{
  timevar_push (TV_REBUILD_JUMP);
  init_label_info (f);
  mark_all_labels (f);

  /* Keep track of labels used from static data; we don't track them
     closely enough to delete them here, so make sure their reference
     count doesn't drop to zero.  */

  if (count_forced)
    {
      rtx_insn *insn;
      unsigned int i;
      FOR_EACH_VEC_SAFE_ELT (forced_labels, i, insn)
	if (LABEL_P (insn))
	  LABEL_NUSES (insn)++;
    }
  timevar_pop (TV_REBUILD_JUMP);
}

/* This function rebuilds the JUMP_LABEL field and REG_LABEL_TARGET
   notes in jumping insns and REG_LABEL_OPERAND notes in non-jumping
   instructions and jumping insns that have labels as operands
   (e.g. cbranchsi4).  */
void
rebuild_jump_labels (rtx_insn *f)
{
  rebuild_jump_labels_1 (f, true);
}

/* This function is like rebuild_jump_labels, but doesn't run over
   forced_labels.  It can be used on insn chains that aren't the 
   main function chain.  */
void
rebuild_jump_labels_chain (rtx_insn *chain)
{
  rebuild_jump_labels_1 (chain, false);
}

/* Some old code expects exactly one BARRIER as the NEXT_INSN of a
   non-fallthru insn.  This is not generally true, as multiple barriers
   may have crept in, or the BARRIER may be separated from the last
   real insn by one or more NOTEs.

   This simple pass moves barriers and removes duplicates so that the
   old code is happy.
 */
static unsigned int
cleanup_barriers (void)
{
  rtx_insn *insn;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (BARRIER_P (insn))
	{
	  rtx_insn *prev = prev_nonnote_nondebug_insn (insn);
	  if (!prev)
	    continue;

	  if (BARRIER_P (prev))
	    delete_insn (insn);
	  else if (prev != PREV_INSN (insn))
	    {
	      basic_block bb = BLOCK_FOR_INSN (prev);
	      rtx_insn *end = PREV_INSN (insn);
	      reorder_insns_nobb (insn, insn, prev);
	      if (bb)
		{
		  /* If the backend called in machine reorg compute_bb_for_insn
		     and didn't free_bb_for_insn again, preserve basic block
		     boundaries.  Move the end of basic block to PREV since
		     it is followed by a barrier now, and clear BLOCK_FOR_INSN
		     on the following notes.
		     ???  Maybe the proper solution for the targets that have
		     cfg around after machine reorg is not to run cleanup_barriers
		     pass at all.  */
		  BB_END (bb) = prev;
		  do
		    {
		      prev = NEXT_INSN (prev);
		      if (prev != insn && BLOCK_FOR_INSN (prev) == bb)
			BLOCK_FOR_INSN (prev) = NULL;
		    }
		  while (prev != end);
		}
	    }
	}
    }
  return 0;
}

namespace {

const pass_data pass_data_cleanup_barriers =
{
  RTL_PASS, /* type */
  "barriers", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_cleanup_barriers : public rtl_opt_pass
{
public:
  pass_cleanup_barriers (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_cleanup_barriers, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return cleanup_barriers (); }

}; // class pass_cleanup_barriers

} // anon namespace

rtl_opt_pass *
make_pass_cleanup_barriers (gcc::context *ctxt)
{
  return new pass_cleanup_barriers (ctxt);
}


/* Initialize LABEL_NUSES and JUMP_LABEL fields, add REG_LABEL_TARGET
   for remaining targets for JUMP_P.  Delete any REG_LABEL_OPERAND
   notes whose labels don't occur in the insn any more.  */

static void
init_label_info (rtx_insn *f)
{
  rtx_insn *insn;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (LABEL_P (insn))
	LABEL_NUSES (insn) = (LABEL_PRESERVE_P (insn) != 0);

      /* REG_LABEL_TARGET notes (including the JUMP_LABEL field) are
	 sticky and not reset here; that way we won't lose association
	 with a label when e.g. the source for a target register
	 disappears out of reach for targets that may use jump-target
	 registers.  Jump transformations are supposed to transform
	 any REG_LABEL_TARGET notes.  The target label reference in a
	 branch may disappear from the branch (and from the
	 instruction before it) for other reasons, like register
	 allocation.  */

      if (INSN_P (insn))
	{
	  rtx note, next;

	  for (note = REG_NOTES (insn); note; note = next)
	    {
	      next = XEXP (note, 1);
	      if (REG_NOTE_KIND (note) == REG_LABEL_OPERAND
		  && ! reg_mentioned_p (XEXP (note, 0), PATTERN (insn)))
		remove_note (insn, note);
	    }
	}
    }
}

/* A subroutine of mark_all_labels.  Trivially propagate a simple label
   load into a jump_insn that uses it.  */

static void
maybe_propagate_label_ref (rtx_insn *jump_insn, rtx_insn *prev_nonjump_insn)
{
  rtx label_note, pc, pc_src;

  pc = pc_set (jump_insn);
  pc_src = pc != NULL ? SET_SRC (pc) : NULL;
  label_note = find_reg_note (prev_nonjump_insn, REG_LABEL_OPERAND, NULL);

  /* If the previous non-jump insn sets something to a label,
     something that this jump insn uses, make that label the primary
     target of this insn if we don't yet have any.  That previous
     insn must be a single_set and not refer to more than one label.
     The jump insn must not refer to other labels as jump targets
     and must be a plain (set (pc) ...), maybe in a parallel, and
     may refer to the item being set only directly or as one of the
     arms in an IF_THEN_ELSE.  */

  if (label_note != NULL && pc_src != NULL)
    {
      rtx label_set = single_set (prev_nonjump_insn);
      rtx label_dest = label_set != NULL ? SET_DEST (label_set) : NULL;

      if (label_set != NULL
	  /* The source must be the direct LABEL_REF, not a
	     PLUS, UNSPEC, IF_THEN_ELSE etc.  */
	  && GET_CODE (SET_SRC (label_set)) == LABEL_REF
	  && (rtx_equal_p (label_dest, pc_src)
	      || (GET_CODE (pc_src) == IF_THEN_ELSE
		  && (rtx_equal_p (label_dest, XEXP (pc_src, 1))
		      || rtx_equal_p (label_dest, XEXP (pc_src, 2))))))
	{
	  /* The CODE_LABEL referred to in the note must be the
	     CODE_LABEL in the LABEL_REF of the "set".  We can
	     conveniently use it for the marker function, which
	     requires a LABEL_REF wrapping.  */
	  gcc_assert (XEXP (label_note, 0) == label_ref_label (SET_SRC (label_set)));

	  mark_jump_label_1 (label_set, jump_insn, false, true);

	  gcc_assert (JUMP_LABEL (jump_insn) == XEXP (label_note, 0));
	}
    }
}

/* Mark the label each jump jumps to.
   Combine consecutive labels, and count uses of labels.  */

static void
mark_all_labels (rtx_insn *f)
{
  rtx_insn *insn;

  if (current_ir_type () == IR_RTL_CFGLAYOUT)
    {
      basic_block bb;
      FOR_EACH_BB_FN (bb, cfun)
	{
	  /* In cfglayout mode, we don't bother with trivial next-insn
	     propagation of LABEL_REFs into JUMP_LABEL.  This will be
	     handled by other optimizers using better algorithms.  */
	  FOR_BB_INSNS (bb, insn)
	    {
	      gcc_assert (! insn->deleted ());
	      if (NONDEBUG_INSN_P (insn))
	        mark_jump_label (PATTERN (insn), insn, 0);
	    }

	  /* In cfglayout mode, there may be non-insns between the
	     basic blocks.  If those non-insns represent tablejump data,
	     they contain label references that we must record.  */
	  for (insn = BB_HEADER (bb); insn; insn = NEXT_INSN (insn))
	    if (JUMP_TABLE_DATA_P (insn))
	      mark_jump_label (PATTERN (insn), insn, 0);
	  for (insn = BB_FOOTER (bb); insn; insn = NEXT_INSN (insn))
	    if (JUMP_TABLE_DATA_P (insn))
	      mark_jump_label (PATTERN (insn), insn, 0);
	}
    }
  else
    {
      rtx_insn *prev_nonjump_insn = NULL;
      for (insn = f; insn; insn = NEXT_INSN (insn))
	{
	  if (insn->deleted ())
	    ;
	  else if (LABEL_P (insn))
	    prev_nonjump_insn = NULL;
	  else if (JUMP_TABLE_DATA_P (insn))
	    mark_jump_label (PATTERN (insn), insn, 0);
	  else if (NONDEBUG_INSN_P (insn))
	    {
	      mark_jump_label (PATTERN (insn), insn, 0);
	      if (JUMP_P (insn))
		{
		  if (JUMP_LABEL (insn) == NULL && prev_nonjump_insn != NULL)
		    maybe_propagate_label_ref (insn, prev_nonjump_insn);
		}
	      else
		prev_nonjump_insn = insn;
	    }
	}
    }
}

/* Given a comparison (CODE ARG0 ARG1), inside an insn, INSN, return a code
   of reversed comparison if it is possible to do so.  Otherwise return UNKNOWN.
   UNKNOWN may be returned in case we are having CC_MODE compare and we don't
   know whether it's source is floating point or integer comparison.  Machine
   description should define REVERSIBLE_CC_MODE and REVERSE_CONDITION macros
   to help this function avoid overhead in these cases.  */
enum rtx_code
reversed_comparison_code_parts (enum rtx_code code, const_rtx arg0,
				const_rtx arg1, const rtx_insn *insn)
{
  machine_mode mode;

  /* If this is not actually a comparison, we can't reverse it.  */
  if (GET_RTX_CLASS (code) != RTX_COMPARE
      && GET_RTX_CLASS (code) != RTX_COMM_COMPARE)
    return UNKNOWN;

  mode = GET_MODE (arg0);
  if (mode == VOIDmode)
    mode = GET_MODE (arg1);

  /* First see if machine description supplies us way to reverse the
     comparison.  Give it priority over everything else to allow
     machine description to do tricks.  */
  if (GET_MODE_CLASS (mode) == MODE_CC
      && REVERSIBLE_CC_MODE (mode))
    return REVERSE_CONDITION (code, mode);

  /* Try a few special cases based on the comparison code.  */
  switch (code)
    {
    case GEU:
    case GTU:
    case LEU:
    case LTU:
    case NE:
    case EQ:
      /* It is always safe to reverse EQ and NE, even for the floating
	 point.  Similarly the unsigned comparisons are never used for
	 floating point so we can reverse them in the default way.  */
      return reverse_condition (code);
    case ORDERED:
    case UNORDERED:
    case LTGT:
    case UNEQ:
      /* In case we already see unordered comparison, we can be sure to
	 be dealing with floating point so we don't need any more tests.  */
      return reverse_condition_maybe_unordered (code);
    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
      /* We don't have safe way to reverse these yet.  */
      return UNKNOWN;
    default:
      break;
    }

  if (GET_MODE_CLASS (mode) == MODE_CC || CC0_P (arg0))
    {
      /* Try to search for the comparison to determine the real mode.
         This code is expensive, but with sane machine description it
         will be never used, since REVERSIBLE_CC_MODE will return true
         in all cases.  */
      if (! insn)
	return UNKNOWN;

      /* These CONST_CAST's are okay because prev_nonnote_insn just
	 returns its argument and we assign it to a const_rtx
	 variable.  */
      for (rtx_insn *prev = prev_nonnote_insn (const_cast<rtx_insn *> (insn));
	   prev != 0 && !LABEL_P (prev);
	   prev = prev_nonnote_insn (prev))
	{
	  const_rtx set = set_of (arg0, prev);
	  if (set && GET_CODE (set) == SET
	      && rtx_equal_p (SET_DEST (set), arg0))
	    {
	      rtx src = SET_SRC (set);

	      if (GET_CODE (src) == COMPARE)
		{
		  rtx comparison = src;
		  arg0 = XEXP (src, 0);
		  mode = GET_MODE (arg0);
		  if (mode == VOIDmode)
		    mode = GET_MODE (XEXP (comparison, 1));
		  break;
		}
	      /* We can get past reg-reg moves.  This may be useful for model
	         of i387 comparisons that first move flag registers around.  */
	      if (REG_P (src))
		{
		  arg0 = src;
		  continue;
		}
	    }
	  /* If register is clobbered in some ununderstandable way,
	     give up.  */
	  if (set)
	    return UNKNOWN;
	}
    }

  /* Test for an integer condition, or a floating-point comparison
     in which NaNs can be ignored.  */
  if (CONST_INT_P (arg0)
      || (GET_MODE (arg0) != VOIDmode
	  && GET_MODE_CLASS (mode) != MODE_CC
	  && !HONOR_NANS (mode)))
    return reverse_condition (code);

  return UNKNOWN;
}

/* A wrapper around the previous function to take COMPARISON as rtx
   expression.  This simplifies many callers.  */
enum rtx_code
reversed_comparison_code (const_rtx comparison, const rtx_insn *insn)
{
  if (!COMPARISON_P (comparison))
    return UNKNOWN;
  return reversed_comparison_code_parts (GET_CODE (comparison),
					 XEXP (comparison, 0),
					 XEXP (comparison, 1), insn);
}

/* Return comparison with reversed code of EXP.
   Return NULL_RTX in case we fail to do the reversal.  */
rtx
reversed_comparison (const_rtx exp, machine_mode mode)
{
  enum rtx_code reversed_code = reversed_comparison_code (exp, NULL);
  if (reversed_code == UNKNOWN)
    return NULL_RTX;
  else
    return simplify_gen_relational (reversed_code, mode, VOIDmode,
                                    XEXP (exp, 0), XEXP (exp, 1));
}


/* Given an rtx-code for a comparison, return the code for the negated
   comparison.  If no such code exists, return UNKNOWN.

   WATCH OUT!  reverse_condition is not safe to use on a jump that might
   be acting on the results of an IEEE floating point comparison, because
   of the special treatment of non-signaling nans in comparisons.
   Use reversed_comparison_code instead.  */

enum rtx_code
reverse_condition (enum rtx_code code)
{
  switch (code)
    {
    case EQ:
      return NE;
    case NE:
      return EQ;
    case GT:
      return LE;
    case GE:
      return LT;
    case LT:
      return GE;
    case LE:
      return GT;
    case GTU:
      return LEU;
    case GEU:
      return LTU;
    case LTU:
      return GEU;
    case LEU:
      return GTU;
    case UNORDERED:
      return ORDERED;
    case ORDERED:
      return UNORDERED;

    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
    case UNEQ:
    case LTGT:
      return UNKNOWN;

    default:
      gcc_unreachable ();
    }
}

/* Similar, but we're allowed to generate unordered comparisons, which
   makes it safe for IEEE floating-point.  Of course, we have to recognize
   that the target will support them too...  */

enum rtx_code
reverse_condition_maybe_unordered (enum rtx_code code)
{
  switch (code)
    {
    case EQ:
      return NE;
    case NE:
      return EQ;
    case GT:
      return UNLE;
    case GE:
      return UNLT;
    case LT:
      return UNGE;
    case LE:
      return UNGT;
    case LTGT:
      return UNEQ;
    case UNORDERED:
      return ORDERED;
    case ORDERED:
      return UNORDERED;
    case UNLT:
      return GE;
    case UNLE:
      return GT;
    case UNGT:
      return LE;
    case UNGE:
      return LT;
    case UNEQ:
      return LTGT;

    default:
      gcc_unreachable ();
    }
}

/* Similar, but return the code when two operands of a comparison are swapped.
   This IS safe for IEEE floating-point.  */

enum rtx_code
swap_condition (enum rtx_code code)
{
  switch (code)
    {
    case EQ:
    case NE:
    case UNORDERED:
    case ORDERED:
    case UNEQ:
    case LTGT:
      return code;

    case GT:
      return LT;
    case GE:
      return LE;
    case LT:
      return GT;
    case LE:
      return GE;
    case GTU:
      return LTU;
    case GEU:
      return LEU;
    case LTU:
      return GTU;
    case LEU:
      return GEU;
    case UNLT:
      return UNGT;
    case UNLE:
      return UNGE;
    case UNGT:
      return UNLT;
    case UNGE:
      return UNLE;

    default:
      gcc_unreachable ();
    }
}

/* Given a comparison CODE, return the corresponding unsigned comparison.
   If CODE is an equality comparison or already an unsigned comparison,
   CODE is returned.  */

enum rtx_code
unsigned_condition (enum rtx_code code)
{
  switch (code)
    {
    case EQ:
    case NE:
    case GTU:
    case GEU:
    case LTU:
    case LEU:
      return code;

    case GT:
      return GTU;
    case GE:
      return GEU;
    case LT:
      return LTU;
    case LE:
      return LEU;

    default:
      gcc_unreachable ();
    }
}

/* Similarly, return the signed version of a comparison.  */

enum rtx_code
signed_condition (enum rtx_code code)
{
  switch (code)
    {
    case EQ:
    case NE:
    case GT:
    case GE:
    case LT:
    case LE:
      return code;

    case GTU:
      return GT;
    case GEU:
      return GE;
    case LTU:
      return LT;
    case LEU:
      return LE;

    default:
      gcc_unreachable ();
    }
}

/* Return nonzero if CODE1 is more strict than CODE2, i.e., if the
   truth of CODE1 implies the truth of CODE2.  */

int
comparison_dominates_p (enum rtx_code code1, enum rtx_code code2)
{
  /* UNKNOWN comparison codes can happen as a result of trying to revert
     comparison codes.
     They can't match anything, so we have to reject them here.  */
  if (code1 == UNKNOWN || code2 == UNKNOWN)
    return 0;

  if (code1 == code2)
    return 1;

  switch (code1)
    {
    case UNEQ:
      if (code2 == UNLE || code2 == UNGE)
	return 1;
      break;

    case EQ:
      if (code2 == LE || code2 == LEU || code2 == GE || code2 == GEU
	  || code2 == ORDERED)
	return 1;
      break;

    case UNLT:
      if (code2 == UNLE || code2 == NE)
	return 1;
      break;

    case LT:
      if (code2 == LE || code2 == NE || code2 == ORDERED || code2 == LTGT)
	return 1;
      break;

    case UNGT:
      if (code2 == UNGE || code2 == NE)
	return 1;
      break;

    case GT:
      if (code2 == GE || code2 == NE || code2 == ORDERED || code2 == LTGT)
	return 1;
      break;

    case GE:
    case LE:
      if (code2 == ORDERED)
	return 1;
      break;

    case LTGT:
      if (code2 == NE || code2 == ORDERED)
	return 1;
      break;

    case LTU:
      if (code2 == LEU || code2 == NE)
	return 1;
      break;

    case GTU:
      if (code2 == GEU || code2 == NE)
	return 1;
      break;

    case UNORDERED:
      if (code2 == NE || code2 == UNEQ || code2 == UNLE || code2 == UNLT
	  || code2 == UNGE || code2 == UNGT)
	return 1;
      break;

    default:
      break;
    }

  return 0;
}

/* Return 1 if INSN is an unconditional jump and nothing else.  */

int
simplejump_p (const rtx_insn *insn)
{
  return (JUMP_P (insn)
	  && GET_CODE (PATTERN (insn)) == SET
	  && GET_CODE (SET_DEST (PATTERN (insn))) == PC
	  && GET_CODE (SET_SRC (PATTERN (insn))) == LABEL_REF);
}

/* Return nonzero if INSN is a (possibly) conditional jump
   and nothing more.

   Use of this function is deprecated, since we need to support combined
   branch and compare insns.  Use any_condjump_p instead whenever possible.  */

int
condjump_p (const rtx_insn *insn)
{
  const_rtx x = PATTERN (insn);

  if (GET_CODE (x) != SET
      || GET_CODE (SET_DEST (x)) != PC)
    return 0;

  x = SET_SRC (x);
  if (GET_CODE (x) == LABEL_REF)
    return 1;
  else
    return (GET_CODE (x) == IF_THEN_ELSE
	    && ((GET_CODE (XEXP (x, 2)) == PC
		 && (GET_CODE (XEXP (x, 1)) == LABEL_REF
		     || ANY_RETURN_P (XEXP (x, 1))))
		|| (GET_CODE (XEXP (x, 1)) == PC
		    && (GET_CODE (XEXP (x, 2)) == LABEL_REF
			|| ANY_RETURN_P (XEXP (x, 2))))));
}

/* Return nonzero if INSN is a (possibly) conditional jump inside a
   PARALLEL.

   Use this function is deprecated, since we need to support combined
   branch and compare insns.  Use any_condjump_p instead whenever possible.  */

int
condjump_in_parallel_p (const rtx_insn *insn)
{
  const_rtx x = PATTERN (insn);

  if (GET_CODE (x) != PARALLEL)
    return 0;
  else
    x = XVECEXP (x, 0, 0);

  if (GET_CODE (x) != SET)
    return 0;
  if (GET_CODE (SET_DEST (x)) != PC)
    return 0;
  if (GET_CODE (SET_SRC (x)) == LABEL_REF)
    return 1;
  if (GET_CODE (SET_SRC (x)) != IF_THEN_ELSE)
    return 0;
  if (XEXP (SET_SRC (x), 2) == pc_rtx
      && (GET_CODE (XEXP (SET_SRC (x), 1)) == LABEL_REF
	  || ANY_RETURN_P (XEXP (SET_SRC (x), 1))))
    return 1;
  if (XEXP (SET_SRC (x), 1) == pc_rtx
      && (GET_CODE (XEXP (SET_SRC (x), 2)) == LABEL_REF
	  || ANY_RETURN_P (XEXP (SET_SRC (x), 2))))
    return 1;
  return 0;
}

/* Return set of PC, otherwise NULL.  */

rtx
pc_set (const rtx_insn *insn)
{
  rtx pat;
  if (!JUMP_P (insn))
    return NULL_RTX;
  pat = PATTERN (insn);

  /* The set is allowed to appear either as the insn pattern or
     the first set in a PARALLEL.  */
  if (GET_CODE (pat) == PARALLEL)
    pat = XVECEXP (pat, 0, 0);
  if (GET_CODE (pat) == SET && GET_CODE (SET_DEST (pat)) == PC)
    return pat;

  return NULL_RTX;
}

/* Return true when insn is an unconditional direct jump,
   possibly bundled inside a PARALLEL.  */

int
any_uncondjump_p (const rtx_insn *insn)
{
  const_rtx x = pc_set (insn);
  if (!x)
    return 0;
  if (GET_CODE (SET_SRC (x)) != LABEL_REF)
    return 0;
  if (find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
    return 0;
  return 1;
}

/* Return true when insn is a conditional jump.  This function works for
   instructions containing PC sets in PARALLELs.  The instruction may have
   various other effects so before removing the jump you must verify
   onlyjump_p.

   Note that unlike condjump_p it returns false for unconditional jumps.  */

int
any_condjump_p (const rtx_insn *insn)
{
  const_rtx x = pc_set (insn);
  enum rtx_code a, b;

  if (!x)
    return 0;
  if (GET_CODE (SET_SRC (x)) != IF_THEN_ELSE)
    return 0;

  a = GET_CODE (XEXP (SET_SRC (x), 1));
  b = GET_CODE (XEXP (SET_SRC (x), 2));

  return ((b == PC && (a == LABEL_REF || a == RETURN || a == SIMPLE_RETURN))
	  || (a == PC
	      && (b == LABEL_REF || b == RETURN || b == SIMPLE_RETURN)));
}

/* Return the label of a conditional jump.  */

rtx
condjump_label (const rtx_insn *insn)
{
  rtx x = pc_set (insn);

  if (!x)
    return NULL_RTX;
  x = SET_SRC (x);
  if (GET_CODE (x) == LABEL_REF)
    return x;
  if (GET_CODE (x) != IF_THEN_ELSE)
    return NULL_RTX;
  if (XEXP (x, 2) == pc_rtx && GET_CODE (XEXP (x, 1)) == LABEL_REF)
    return XEXP (x, 1);
  if (XEXP (x, 1) == pc_rtx && GET_CODE (XEXP (x, 2)) == LABEL_REF)
    return XEXP (x, 2);
  return NULL_RTX;
}

/* Return TRUE if INSN is a return jump.  */

int
returnjump_p (const rtx_insn *insn)
{
  if (JUMP_P (insn))
    {
      subrtx_iterator::array_type array;
      FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
	{
	  const_rtx x = *iter;
	  switch (GET_CODE (x))
	    {
	    case RETURN:
	    case SIMPLE_RETURN:
	    case EH_RETURN:
	      return true;

	    case SET:
	      if (SET_IS_RETURN_P (x))
		return true;
	      break;

	    default:
	      break;
	    }
	}
    }
  return false;
}

/* Return true if INSN is a (possibly conditional) return insn.  */

int
eh_returnjump_p (rtx_insn *insn)
{
  if (JUMP_P (insn))
    {
      subrtx_iterator::array_type array;
      FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
	if (GET_CODE (*iter) == EH_RETURN)
	  return true;
    }
  return false;
}

/* Return true if INSN is a jump that only transfers control and
   nothing more.  */

int
onlyjump_p (const rtx_insn *insn)
{
  rtx set;

  if (!JUMP_P (insn))
    return 0;

  set = single_set (insn);
  if (set == NULL)
    return 0;
  if (GET_CODE (SET_DEST (set)) != PC)
    return 0;
  if (side_effects_p (SET_SRC (set)))
    return 0;

  return 1;
}

/* Return true iff INSN is a jump and its JUMP_LABEL is a label, not
   NULL or a return.  */
bool
jump_to_label_p (const rtx_insn *insn)
{
  return (JUMP_P (insn)
	  && JUMP_LABEL (insn) != NULL && !ANY_RETURN_P (JUMP_LABEL (insn)));
}

/* Return nonzero if X is an RTX that only sets the condition codes
   and has no side effects.  */

int
only_sets_cc0_p (const_rtx x)
{
  if (! x)
    return 0;

  if (INSN_P (x))
    x = PATTERN (x);

  return sets_cc0_p (x) == 1 && ! side_effects_p (x);
}

/* Return 1 if X is an RTX that does nothing but set the condition codes
   and CLOBBER or USE registers.
   Return -1 if X does explicitly set the condition codes,
   but also does other things.  */

int
sets_cc0_p (const_rtx x)
{
  if (! x)
    return 0;

  if (INSN_P (x))
    x = PATTERN (x);

  if (GET_CODE (x) == SET && SET_DEST (x) == cc0_rtx)
    return 1;
  if (GET_CODE (x) == PARALLEL)
    {
      int i;
      int sets_cc0 = 0;
      int other_things = 0;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  if (GET_CODE (XVECEXP (x, 0, i)) == SET
	      && SET_DEST (XVECEXP (x, 0, i)) == cc0_rtx)
	    sets_cc0 = 1;
	  else if (GET_CODE (XVECEXP (x, 0, i)) == SET)
	    other_things = 1;
	}
      return ! sets_cc0 ? 0 : other_things ? -1 : 1;
    }
  return 0;
}

/* Find all CODE_LABELs referred to in X, and increment their use
   counts.  If INSN is a JUMP_INSN and there is at least one
   CODE_LABEL referenced in INSN as a jump target, then store the last
   one in JUMP_LABEL (INSN).  For a tablejump, this must be the label
   for the ADDR_VEC.  Store any other jump targets as REG_LABEL_TARGET
   notes.  If INSN is an INSN or a CALL_INSN or non-target operands of
   a JUMP_INSN, and there is at least one CODE_LABEL referenced in
   INSN, add a REG_LABEL_OPERAND note containing that label to INSN.
   For returnjumps, the JUMP_LABEL will also be set as appropriate.

   Note that two labels separated by a loop-beginning note
   must be kept distinct if we have not yet done loop-optimization,
   because the gap between them is where loop-optimize
   will want to move invariant code to.  CROSS_JUMP tells us
   that loop-optimization is done with.  */

void
mark_jump_label (rtx x, rtx_insn *insn, int in_mem)
{
  rtx asmop = extract_asm_operands (x);
  if (asmop)
    mark_jump_label_asm (asmop, insn);
  else
    mark_jump_label_1 (x, insn, in_mem != 0,
		       (insn != NULL && x == PATTERN (insn) && JUMP_P (insn)));
}

/* Worker function for mark_jump_label.  IN_MEM is TRUE when X occurs
   within a (MEM ...).  IS_TARGET is TRUE when X is to be treated as a
   jump-target; when the JUMP_LABEL field of INSN should be set or a
   REG_LABEL_TARGET note should be added, not a REG_LABEL_OPERAND
   note.  */

static void
mark_jump_label_1 (rtx x, rtx_insn *insn, bool in_mem, bool is_target)
{
  RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  switch (code)
    {
    case PC:
    case CC0:
    case REG:
    case CLOBBER:
    case CALL:
      return;

    case RETURN:
    case SIMPLE_RETURN:
      if (is_target)
	{
	  gcc_assert (JUMP_LABEL (insn) == NULL || JUMP_LABEL (insn) == x);
	  JUMP_LABEL (insn) = x;
	}
      return;

    case MEM:
      in_mem = true;
      break;

    case SEQUENCE:
      {
	rtx_sequence *seq = as_a <rtx_sequence *> (x);
	for (i = 0; i < seq->len (); i++)
	  mark_jump_label (PATTERN (seq->insn (i)),
			   seq->insn (i), 0);
      }
      return;

    case SYMBOL_REF:
      if (!in_mem)
	return;

      /* If this is a constant-pool reference, see if it is a label.  */
      if (CONSTANT_POOL_ADDRESS_P (x))
	mark_jump_label_1 (get_pool_constant (x), insn, in_mem, is_target);
      break;

      /* Handle operands in the condition of an if-then-else as for a
	 non-jump insn.  */
    case IF_THEN_ELSE:
      if (!is_target)
	break;
      mark_jump_label_1 (XEXP (x, 0), insn, in_mem, false);
      mark_jump_label_1 (XEXP (x, 1), insn, in_mem, true);
      mark_jump_label_1 (XEXP (x, 2), insn, in_mem, true);
      return;

    case LABEL_REF:
      {
	rtx_insn *label = label_ref_label (x);

	/* Ignore remaining references to unreachable labels that
	   have been deleted.  */
	if (NOTE_P (label)
	    && NOTE_KIND (label) == NOTE_INSN_DELETED_LABEL)
	  break;

	gcc_assert (LABEL_P (label));

	/* Ignore references to labels of containing functions.  */
	if (LABEL_REF_NONLOCAL_P (x))
	  break;

	set_label_ref_label (x, label);
	if (! insn || ! insn->deleted ())
	  ++LABEL_NUSES (label);

	if (insn)
	  {
	    if (is_target
		/* Do not change a previous setting of JUMP_LABEL.  If the
		   JUMP_LABEL slot is occupied by a different label,
		   create a note for this label.  */
		&& (JUMP_LABEL (insn) == NULL || JUMP_LABEL (insn) == label))
	      JUMP_LABEL (insn) = label;
	    else
	      {
		enum reg_note kind
		  = is_target ? REG_LABEL_TARGET : REG_LABEL_OPERAND;

		/* Add a REG_LABEL_OPERAND or REG_LABEL_TARGET note
		   for LABEL unless there already is one.  All uses of
		   a label, except for the primary target of a jump,
		   must have such a note.  */
		if (! find_reg_note (insn, kind, label))
		  add_reg_note (insn, kind, label);
	      }
	  }
	return;
      }

    /* Do walk the labels in a vector, but not the first operand of an
       ADDR_DIFF_VEC.  Don't set the JUMP_LABEL of a vector.  */
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      if (! insn->deleted ())
	{
	  int eltnum = code == ADDR_DIFF_VEC ? 1 : 0;

	  for (i = 0; i < XVECLEN (x, eltnum); i++)
	    mark_jump_label_1 (XVECEXP (x, eltnum, i), NULL, in_mem,
			       is_target);
	}
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);

  /* The primary target of a tablejump is the label of the ADDR_VEC,
     which is canonically mentioned *last* in the insn.  To get it
     marked as JUMP_LABEL, we iterate over items in reverse order.  */
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	mark_jump_label_1 (XEXP (x, i), insn, in_mem, is_target);
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    mark_jump_label_1 (XVECEXP (x, i, j), insn, in_mem,
			       is_target);
	}
    }
}

/* Worker function for mark_jump_label.  Handle asm insns specially.
   In particular, output operands need not be considered so we can
   avoid re-scanning the replicated asm_operand.  Also, the asm_labels
   need to be considered targets.  */

static void
mark_jump_label_asm (rtx asmop, rtx_insn *insn)
{
  int i;

  for (i = ASM_OPERANDS_INPUT_LENGTH (asmop) - 1; i >= 0; --i)
    mark_jump_label_1 (ASM_OPERANDS_INPUT (asmop, i), insn, false, false);

  for (i = ASM_OPERANDS_LABEL_LENGTH (asmop) - 1; i >= 0; --i)
    mark_jump_label_1 (ASM_OPERANDS_LABEL (asmop, i), insn, false, true);
}

/* Delete insn INSN from the chain of insns and update label ref counts
   and delete insns now unreachable.

   Returns the first insn after INSN that was not deleted.

   Usage of this instruction is deprecated.  Use delete_insn instead and
   subsequent cfg_cleanup pass to delete unreachable code if needed.  */

rtx_insn *
delete_related_insns (rtx uncast_insn)
{
  rtx_insn *insn = as_a <rtx_insn *> (uncast_insn);
  int was_code_label = (LABEL_P (insn));
  rtx note;
  rtx_insn *next = NEXT_INSN (insn), *prev = PREV_INSN (insn);

  while (next && next->deleted ())
    next = NEXT_INSN (next);

  /* This insn is already deleted => return first following nondeleted.  */
  if (insn->deleted ())
    return next;

  delete_insn (insn);

  /* If instruction is followed by a barrier,
     delete the barrier too.  */

  if (next != 0 && BARRIER_P (next))
    delete_insn (next);

  /* If deleting a jump, decrement the count of the label,
     and delete the label if it is now unused.  */

  if (jump_to_label_p (insn))
    {
      rtx lab = JUMP_LABEL (insn);
      rtx_jump_table_data *lab_next;

      if (LABEL_NUSES (lab) == 0)
	/* This can delete NEXT or PREV,
	   either directly if NEXT is JUMP_LABEL (INSN),
	   or indirectly through more levels of jumps.  */
	delete_related_insns (lab);
      else if (tablejump_p (insn, NULL, &lab_next))
	{
	  /* If we're deleting the tablejump, delete the dispatch table.
	     We may not be able to kill the label immediately preceding
	     just yet, as it might be referenced in code leading up to
	     the tablejump.  */
	  delete_related_insns (lab_next);
	}
    }

  /* Likewise if we're deleting a dispatch table.  */

  if (rtx_jump_table_data *table = dyn_cast <rtx_jump_table_data *> (insn))
    {
      rtvec labels = table->get_labels ();
      int i;
      int len = GET_NUM_ELEM (labels);

      for (i = 0; i < len; i++)
	if (LABEL_NUSES (XEXP (RTVEC_ELT (labels, i), 0)) == 0)
	  delete_related_insns (XEXP (RTVEC_ELT (labels, i), 0));
      while (next && next->deleted ())
	next = NEXT_INSN (next);
      return next;
    }

  /* Likewise for any JUMP_P / INSN / CALL_INSN with a
     REG_LABEL_OPERAND or REG_LABEL_TARGET note.  */
  if (INSN_P (insn))
    for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
      if ((REG_NOTE_KIND (note) == REG_LABEL_OPERAND
	   || REG_NOTE_KIND (note) == REG_LABEL_TARGET)
	  /* This could also be a NOTE_INSN_DELETED_LABEL note.  */
	  && LABEL_P (XEXP (note, 0)))
	if (LABEL_NUSES (XEXP (note, 0)) == 0)
	  delete_related_insns (XEXP (note, 0));

  while (prev && (prev->deleted () || NOTE_P (prev)))
    prev = PREV_INSN (prev);

  /* If INSN was a label and a dispatch table follows it,
     delete the dispatch table.  The tablejump must have gone already.
     It isn't useful to fall through into a table.  */

  if (was_code_label
      && NEXT_INSN (insn) != 0
      && JUMP_TABLE_DATA_P (NEXT_INSN (insn)))
    next = delete_related_insns (NEXT_INSN (insn));

  /* If INSN was a label, delete insns following it if now unreachable.  */

  if (was_code_label && prev && BARRIER_P (prev))
    {
      enum rtx_code code;
      while (next)
	{
	  code = GET_CODE (next);
	  if (code == NOTE)
	    next = NEXT_INSN (next);
	  /* Keep going past other deleted labels to delete what follows.  */
	  else if (code == CODE_LABEL && next->deleted ())
	    next = NEXT_INSN (next);
	  /* Keep the (use (insn))s created by dbr_schedule, which needs
	     them in order to track liveness relative to a previous
	     barrier.  */
	  else if (INSN_P (next)
		   && GET_CODE (PATTERN (next)) == USE
		   && INSN_P (XEXP (PATTERN (next), 0)))
	    next = NEXT_INSN (next);
	  else if (code == BARRIER || INSN_P (next))
	    /* Note: if this deletes a jump, it can cause more
	       deletion of unreachable code, after a different label.
	       As long as the value from this recursive call is correct,
	       this invocation functions correctly.  */
	    next = delete_related_insns (next);
	  else
	    break;
	}
    }

  /* I feel a little doubtful about this loop,
     but I see no clean and sure alternative way
     to find the first insn after INSN that is not now deleted.
     I hope this works.  */
  while (next && next->deleted ())
    next = NEXT_INSN (next);
  return next;
}

/* Delete a range of insns from FROM to TO, inclusive.
   This is for the sake of peephole optimization, so assume
   that whatever these insns do will still be done by a new
   peephole insn that will replace them.  */

void
delete_for_peephole (rtx_insn *from, rtx_insn *to)
{
  rtx_insn *insn = from;

  while (1)
    {
      rtx_insn *next = NEXT_INSN (insn);
      rtx_insn *prev = PREV_INSN (insn);

      if (!NOTE_P (insn))
	{
	  insn->set_deleted();

	  /* Patch this insn out of the chain.  */
	  /* We don't do this all at once, because we
	     must preserve all NOTEs.  */
	  if (prev)
	    SET_NEXT_INSN (prev) = next;

	  if (next)
	    SET_PREV_INSN (next) = prev;
	}

      if (insn == to)
	break;
      insn = next;
    }

  /* Note that if TO is an unconditional jump
     we *do not* delete the BARRIER that follows,
     since the peephole that replaces this sequence
     is also an unconditional jump in that case.  */
}

/* A helper function for redirect_exp_1; examines its input X and returns
   either a LABEL_REF around a label, or a RETURN if X was NULL.  */
static rtx
redirect_target (rtx x)
{
  if (x == NULL_RTX)
    return ret_rtx;
  if (!ANY_RETURN_P (x))
    return gen_rtx_LABEL_REF (Pmode, x);
  return x;
}

/* Throughout LOC, redirect OLABEL to NLABEL.  Treat null OLABEL or
   NLABEL as a return.  Accrue modifications into the change group.  */

static void
redirect_exp_1 (rtx *loc, rtx olabel, rtx nlabel, rtx_insn *insn)
{
  rtx x = *loc;
  RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  if ((code == LABEL_REF && label_ref_label (x) == olabel)
      || x == olabel)
    {
      x = redirect_target (nlabel);
      if (GET_CODE (x) == LABEL_REF && loc == &PATTERN (insn))
 	x = gen_rtx_SET (pc_rtx, x);
      validate_change (insn, loc, x, 1);
      return;
    }

  if (code == SET && SET_DEST (x) == pc_rtx
      && ANY_RETURN_P (nlabel)
      && GET_CODE (SET_SRC (x)) == LABEL_REF
      && label_ref_label (SET_SRC (x)) == olabel)
    {
      validate_change (insn, loc, nlabel, 1);
      return;
    }

  if (code == IF_THEN_ELSE)
    {
      /* Skip the condition of an IF_THEN_ELSE.  We only want to
         change jump destinations, not eventual label comparisons.  */
      redirect_exp_1 (&XEXP (x, 1), olabel, nlabel, insn);
      redirect_exp_1 (&XEXP (x, 2), olabel, nlabel, insn);
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	redirect_exp_1 (&XEXP (x, i), olabel, nlabel, insn);
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    redirect_exp_1 (&XVECEXP (x, i, j), olabel, nlabel, insn);
	}
    }
}

/* Make JUMP go to NLABEL instead of where it jumps now.  Accrue
   the modifications into the change group.  Return false if we did
   not see how to do that.  */

int
redirect_jump_1 (rtx_insn *jump, rtx nlabel)
{
  int ochanges = num_validated_changes ();
  rtx *loc, asmop;

  gcc_assert (nlabel != NULL_RTX);
  asmop = extract_asm_operands (PATTERN (jump));
  if (asmop)
    {
      if (nlabel == NULL)
	return 0;
      gcc_assert (ASM_OPERANDS_LABEL_LENGTH (asmop) == 1);
      loc = &ASM_OPERANDS_LABEL (asmop, 0);
    }
  else if (GET_CODE (PATTERN (jump)) == PARALLEL)
    loc = &XVECEXP (PATTERN (jump), 0, 0);
  else
    loc = &PATTERN (jump);

  redirect_exp_1 (loc, JUMP_LABEL (jump), nlabel, jump);
  return num_validated_changes () > ochanges;
}

/* Make JUMP go to NLABEL instead of where it jumps now.  If the old
   jump target label is unused as a result, it and the code following
   it may be deleted.

   Normally, NLABEL will be a label, but it may also be a RETURN rtx;
   in that case we are to turn the jump into a (possibly conditional)
   return insn.

   The return value will be 1 if the change was made, 0 if it wasn't
   (this can only occur when trying to produce return insns).  */

int
redirect_jump (rtx_jump_insn *jump, rtx nlabel, int delete_unused)
{
  rtx olabel = jump->jump_label ();

  if (!nlabel)
    {
      /* If there is no label, we are asked to redirect to the EXIT block.
	 When before the epilogue is emitted, return/simple_return cannot be
	 created so we return 0 immediately.  After the epilogue is emitted,
	 we always expect a label, either a non-null label, or a
	 return/simple_return RTX.  */

      if (!epilogue_completed)
	return 0;
      gcc_unreachable ();
    }

  if (nlabel == olabel)
    return 1;

  if (! redirect_jump_1 (jump, nlabel) || ! apply_change_group ())
    return 0;

  redirect_jump_2 (jump, olabel, nlabel, delete_unused, 0);
  return 1;
}

/* Fix up JUMP_LABEL and label ref counts after OLABEL has been replaced with
   NLABEL in JUMP.
   If DELETE_UNUSED is positive, delete related insn to OLABEL if its ref
   count has dropped to zero.  */
void
redirect_jump_2 (rtx_jump_insn *jump, rtx olabel, rtx nlabel, int delete_unused,
		 int invert)
{
  rtx note;

  gcc_assert (JUMP_LABEL (jump) == olabel);

  /* Negative DELETE_UNUSED used to be used to signalize behavior on
     moving FUNCTION_END note.  Just sanity check that no user still worry
     about this.  */
  gcc_assert (delete_unused >= 0);
  JUMP_LABEL (jump) = nlabel;
  if (!ANY_RETURN_P (nlabel))
    ++LABEL_NUSES (nlabel);

  /* Update labels in any REG_EQUAL note.  */
  if ((note = find_reg_note (jump, REG_EQUAL, NULL_RTX)) != NULL_RTX)
    {
      if (ANY_RETURN_P (nlabel)
	  || (invert && !invert_exp_1 (XEXP (note, 0), jump)))
	remove_note (jump, note);
      else
	{
	  redirect_exp_1 (&XEXP (note, 0), olabel, nlabel, jump);
	  confirm_change_group ();
	}
    }

  /* Handle the case where we had a conditional crossing jump to a return
     label and are now changing it into a direct conditional return.
     The jump is no longer crossing in that case.  */
  if (ANY_RETURN_P (nlabel))
    CROSSING_JUMP_P (jump) = 0;

  if (!ANY_RETURN_P (olabel)
      && --LABEL_NUSES (olabel) == 0 && delete_unused > 0
      /* Undefined labels will remain outside the insn stream.  */
      && INSN_UID (olabel))
    delete_related_insns (olabel);
  if (invert)
    invert_br_probabilities (jump);
}

/* Invert the jump condition X contained in jump insn INSN.  Accrue the
   modifications into the change group.  Return nonzero for success.  */
static int
invert_exp_1 (rtx x, rtx_insn *insn)
{
  RTX_CODE code = GET_CODE (x);

  if (code == IF_THEN_ELSE)
    {
      rtx comp = XEXP (x, 0);
      rtx tem;
      enum rtx_code reversed_code;

      /* We can do this in two ways:  The preferable way, which can only
	 be done if this is not an integer comparison, is to reverse
	 the comparison code.  Otherwise, swap the THEN-part and ELSE-part
	 of the IF_THEN_ELSE.  If we can't do either, fail.  */

      reversed_code = reversed_comparison_code (comp, insn);

      if (reversed_code != UNKNOWN)
	{
	  validate_change (insn, &XEXP (x, 0),
			   gen_rtx_fmt_ee (reversed_code,
					   GET_MODE (comp), XEXP (comp, 0),
					   XEXP (comp, 1)),
			   1);
	  return 1;
	}

      tem = XEXP (x, 1);
      validate_change (insn, &XEXP (x, 1), XEXP (x, 2), 1);
      validate_change (insn, &XEXP (x, 2), tem, 1);
      return 1;
    }
  else
    return 0;
}

/* Invert the condition of the jump JUMP, and make it jump to label
   NLABEL instead of where it jumps now.  Accrue changes into the
   change group.  Return false if we didn't see how to perform the
   inversion and redirection.  */

int
invert_jump_1 (rtx_jump_insn *jump, rtx nlabel)
{
  rtx x = pc_set (jump);
  int ochanges;
  int ok;

  ochanges = num_validated_changes ();
  if (x == NULL)
    return 0;
  ok = invert_exp_1 (SET_SRC (x), jump);
  gcc_assert (ok);

  if (num_validated_changes () == ochanges)
    return 0;

  /* redirect_jump_1 will fail of nlabel == olabel, and the current use is
     in Pmode, so checking this is not merely an optimization.  */
  return nlabel == JUMP_LABEL (jump) || redirect_jump_1 (jump, nlabel);
}

/* Invert the condition of the jump JUMP, and make it jump to label
   NLABEL instead of where it jumps now.  Return true if successful.  */

int
invert_jump (rtx_jump_insn *jump, rtx nlabel, int delete_unused)
{
  rtx olabel = JUMP_LABEL (jump);

  if (invert_jump_1 (jump, nlabel) && apply_change_group ())
    {
      redirect_jump_2 (jump, olabel, nlabel, delete_unused, 1);
      return 1;
    }
  cancel_changes (0);
  return 0;
}


/* Like rtx_equal_p except that it considers two REGs as equal
   if they renumber to the same value and considers two commutative
   operations to be the same if the order of the operands has been
   reversed.  */

int
rtx_renumbered_equal_p (const_rtx x, const_rtx y)
{
  int i;
  const enum rtx_code code = GET_CODE (x);
  const char *fmt;

  if (x == y)
    return 1;

  if ((code == REG || (code == SUBREG && REG_P (SUBREG_REG (x))))
      && (REG_P (y) || (GET_CODE (y) == SUBREG
				  && REG_P (SUBREG_REG (y)))))
    {
      int reg_x = -1, reg_y = -1;
      poly_int64 byte_x = 0, byte_y = 0;
      struct subreg_info info;

      if (GET_MODE (x) != GET_MODE (y))
	return 0;

      /* If we haven't done any renumbering, don't
	 make any assumptions.  */
      if (reg_renumber == 0)
	return rtx_equal_p (x, y);

      if (code == SUBREG)
	{
	  reg_x = REGNO (SUBREG_REG (x));
	  byte_x = SUBREG_BYTE (x);

	  if (reg_renumber[reg_x] >= 0)
	    {
	      subreg_get_info (reg_renumber[reg_x],
			       GET_MODE (SUBREG_REG (x)), byte_x,
			       GET_MODE (x), &info);
	      if (!info.representable_p)
		return 0;
	      reg_x = info.offset;
	      byte_x = 0;
	    }
	}
      else
	{
	  reg_x = REGNO (x);
	  if (reg_renumber[reg_x] >= 0)
	    reg_x = reg_renumber[reg_x];
	}

      if (GET_CODE (y) == SUBREG)
	{
	  reg_y = REGNO (SUBREG_REG (y));
	  byte_y = SUBREG_BYTE (y);

	  if (reg_renumber[reg_y] >= 0)
	    {
	      subreg_get_info (reg_renumber[reg_y],
			       GET_MODE (SUBREG_REG (y)), byte_y,
			       GET_MODE (y), &info);
	      if (!info.representable_p)
		return 0;
	      reg_y = info.offset;
	      byte_y = 0;
	    }
	}
      else
	{
	  reg_y = REGNO (y);
	  if (reg_renumber[reg_y] >= 0)
	    reg_y = reg_renumber[reg_y];
	}

      return reg_x >= 0 && reg_x == reg_y && known_eq (byte_x, byte_y);
    }

  /* Now we have disposed of all the cases
     in which different rtx codes can match.  */
  if (code != GET_CODE (y))
    return 0;

  switch (code)
    {
    case PC:
    case CC0:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    CASE_CONST_UNIQUE:
      return 0;

    case CONST_VECTOR:
      if (!same_vector_encodings_p (x, y))
	return false;
      break;

    case LABEL_REF:
      /* We can't assume nonlocal labels have their following insns yet.  */
      if (LABEL_REF_NONLOCAL_P (x) || LABEL_REF_NONLOCAL_P (y))
	return label_ref_label (x) == label_ref_label (y);

      /* Two label-refs are equivalent if they point at labels
	 in the same position in the instruction stream.  */
      else
	{
	  rtx_insn *xi = next_nonnote_nondebug_insn (label_ref_label (x));
	  rtx_insn *yi = next_nonnote_nondebug_insn (label_ref_label (y));
	  while (xi && LABEL_P (xi))
	    xi = next_nonnote_nondebug_insn (xi);
	  while (yi && LABEL_P (yi))
	    yi = next_nonnote_nondebug_insn (yi);
	  return xi == yi;
	}

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    case CODE_LABEL:
      /* If we didn't match EQ equality above, they aren't the same.  */
      return 0;

    default:
      break;
    }

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* MEMs referring to different address space are not equivalent.  */
  if (code == MEM && MEM_ADDR_SPACE (x) != MEM_ADDR_SPACE (y))
    return 0;

  /* For commutative operations, the RTX match if the operand match in any
     order.  Also handle the simple binary and unary cases without a loop.  */
  if (targetm.commutative_p (x, UNKNOWN))
    return ((rtx_renumbered_equal_p (XEXP (x, 0), XEXP (y, 0))
	     && rtx_renumbered_equal_p (XEXP (x, 1), XEXP (y, 1)))
	    || (rtx_renumbered_equal_p (XEXP (x, 0), XEXP (y, 1))
		&& rtx_renumbered_equal_p (XEXP (x, 1), XEXP (y, 0))));
  else if (NON_COMMUTATIVE_P (x))
    return (rtx_renumbered_equal_p (XEXP (x, 0), XEXP (y, 0))
	    && rtx_renumbered_equal_p (XEXP (x, 1), XEXP (y, 1)));
  else if (UNARY_P (x))
    return rtx_renumbered_equal_p (XEXP (x, 0), XEXP (y, 0));

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      int j;
      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	  break;

	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    {
	      if (((code == ASM_OPERANDS && i == 6)
		   || (code == ASM_INPUT && i == 1)))
		break;
	      return 0;
	    }
	  break;

	case 'p':
	  if (maybe_ne (SUBREG_BYTE (x), SUBREG_BYTE (y)))
	    return 0;
	  break;

	case 't':
	  if (XTREE (x, i) != XTREE (y, i))
	    return 0;
	  break;

	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'e':
	  if (! rtx_renumbered_equal_p (XEXP (x, i), XEXP (y, i)))
	    return 0;
	  break;

	case 'u':
	  if (XEXP (x, i) != XEXP (y, i))
	    return 0;
	  /* Fall through.  */
	case '0':
	  break;

	case 'E':
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (!rtx_renumbered_equal_p (XVECEXP (x, i, j), XVECEXP (y, i, j)))
	      return 0;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  return 1;
}

/* If X is a hard register or equivalent to one or a subregister of one,
   return the hard register number.  If X is a pseudo register that was not
   assigned a hard register, return the pseudo register number.  Otherwise,
   return -1.  Any rtx is valid for X.  */

int
true_regnum (const_rtx x)
{
  if (REG_P (x))
    {
      if (REGNO (x) >= FIRST_PSEUDO_REGISTER
	  && (lra_in_progress || reg_renumber[REGNO (x)] >= 0))
	return reg_renumber[REGNO (x)];
      return REGNO (x);
    }
  if (GET_CODE (x) == SUBREG)
    {
      int base = true_regnum (SUBREG_REG (x));
      if (base >= 0
	  && base < FIRST_PSEUDO_REGISTER)
	{
	  struct subreg_info info;

	  subreg_get_info (lra_in_progress
			   ? (unsigned) base : REGNO (SUBREG_REG (x)),
			   GET_MODE (SUBREG_REG (x)),
			   SUBREG_BYTE (x), GET_MODE (x), &info);

	  if (info.representable_p)
	    return base + info.offset;
	}
    }
  return -1;
}

/* Return regno of the register REG and handle subregs too.  */
unsigned int
reg_or_subregno (const_rtx reg)
{
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  gcc_assert (REG_P (reg));
  return REGNO (reg);
}
