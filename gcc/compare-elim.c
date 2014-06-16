/* Post-reload compare elimination.
   Copyright (C) 2010-2014 Free Software Foundation, Inc.

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

/* There is a set of targets whose general-purpose move or addition
   instructions clobber the flags.  These targets cannot split their
   CBRANCH/CSTORE etc patterns before reload is complete, lest reload
   itself insert these instructions in between the flags setter and user.
   Because these targets cannot split the compare from the use, they
   cannot make use of the comparison elimination offered by the combine pass.

   This is a small pass intended to provide comparison elimination similar to
   what is available via NOTICE_UPDATE_CC for cc0 targets.  This should help
   encourage cc0 targets to convert to an explicit post-reload representation
   of the flags.

   This pass assumes:

   (0) CBRANCH/CSTORE etc have been split in pass_split_after_reload.

   (1) All comparison patterns are represented as

	[(set (reg:CC) (compare:CC (reg) (immediate)))]

   (2) All insn patterns that modify the flags are represented as

	[(set (reg) (operation)
	 (clobber (reg:CC))]

   (3) If an insn of form (2) can usefully set the flags, there is
       another pattern of the form

	[(set (reg) (operation)
	 (set (reg:CCM) (compare:CCM (operation) (immediate)))]

       The mode CCM will be chosen as if by SELECT_CC_MODE.

   Note that unlike NOTICE_UPDATE_CC, we do not handle memory operands.
   This could be handled as a future enhancement.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "flags.h"
#include "basic-block.h"
#include "tree-pass.h"
#include "target.h"
#include "df.h"
#include "domwalk.h"


/* These structures describe a comparison and how it is used.  */

/* The choice of maximum 3 uses comes from wanting to eliminate the two
   duplicate compares from a three-way branch on the sign of a value.
   This is also sufficient to eliminate the duplicate compare against the
   high-part of a double-word comparison.  */
#define MAX_CMP_USE 3

struct comparison_use
{
  /* The instruction in which the result of the compare is used.  */
  rtx insn;
  /* The location of the flags register within the use.  */
  rtx *loc;
  /* The comparison code applied against the flags register.  */
  enum rtx_code code;
};

struct comparison
{
  /* The comparison instruction.  */
  rtx insn;

  /* The insn prior to the comparison insn that clobbers the flags.  */
  rtx prev_clobber;

  /* The two values being compared.  These will be either REGs or
     constants.  */
  rtx in_a, in_b;

  /* Information about how this comparison is used.  */
  struct comparison_use uses[MAX_CMP_USE];

  /* The original CC_MODE for this comparison.  */
  enum machine_mode orig_mode;

  /* The number of uses identified for this comparison.  */
  unsigned short n_uses;

  /* True if not all uses of this comparison have been identified.
     This can happen either for overflowing the array above, or if
     the flags register is used in some unusual context.  */
  bool missing_uses;

  /* True if its inputs are still valid at the end of the block.  */
  bool inputs_valid;
};
  
typedef struct comparison *comparison_struct_p;

static vec<comparison_struct_p> all_compares;

/* Look for a "conforming" comparison, as defined above.  If valid, return
   the rtx for the COMPARE itself.  */

static rtx
conforming_compare (rtx insn)
{
  rtx set, src, dest;

  set = single_set (insn);
  if (set == NULL)
    return NULL;

  src = SET_SRC (set);
  if (GET_CODE (src) != COMPARE)
    return NULL;

  dest = SET_DEST (set);
  if (!REG_P (dest) || REGNO (dest) != targetm.flags_regnum)
    return NULL;

  if (REG_P (XEXP (src, 0))
      && REG_P (XEXP (src, 0))
      && (REG_P (XEXP (src, 1)) || CONSTANT_P (XEXP (src, 1))))
    return src;

  return NULL;
}

/* Look for a pattern of the "correct" form for an insn with a flags clobber
   for which we may be able to eliminate a compare later.  We're not looking
   to validate any inputs at this time, merely see that the basic shape is
   correct.  The term "arithmetic" may be somewhat misleading...  */

static bool
arithmetic_flags_clobber_p (rtx insn)
{
  rtx pat, x;

  if (!NONJUMP_INSN_P (insn))
    return false;
  pat = PATTERN (insn);
  if (extract_asm_operands (pat))
    return false;

  if (GET_CODE (pat) == PARALLEL && XVECLEN (pat, 0) == 2)
    {
      x = XVECEXP (pat, 0, 0);
      if (GET_CODE (x) != SET)
	return false;
      x = SET_DEST (x);
      if (!REG_P (x))
	return false;

      x = XVECEXP (pat, 0, 1);
      if (GET_CODE (x) == CLOBBER)
	{
	  x = XEXP (x, 0);
	  if (REG_P (x) && REGNO (x) == targetm.flags_regnum)
	    return true;
	}
    }

  return false;
}

/* Look for uses of FLAGS in INSN.  If we find one we can analyze, record
   it in CMP; otherwise indicate that we've missed a use.  */

static void
find_flags_uses_in_insn (struct comparison *cmp, rtx insn)
{
  df_ref use;

  /* If we've already lost track of uses, don't bother collecting more.  */
  if (cmp->missing_uses)
    return;

  /* Find a USE of the flags register.  */
  FOR_EACH_INSN_USE (use, insn)
    if (DF_REF_REGNO (use) == targetm.flags_regnum)
      {
	rtx x, *loc;

	/* If this is an unusual use, quit.  */
	if (DF_REF_TYPE (use) != DF_REF_REG_USE)
	  goto fail;

	/* If we've run out of slots to record uses, quit.  */
	if (cmp->n_uses == MAX_CMP_USE)
	  goto fail;

	/* Unfortunately the location of the flags register, while present
	   in the reference structure, doesn't help.  We need to find the
	   comparison code that is outer to the actual flags use.  */
	loc = DF_REF_LOC (use);
	x = PATTERN (insn);
	if (GET_CODE (x) == PARALLEL)
	  x = XVECEXP (x, 0, 0);
	x = SET_SRC (x);
	if (GET_CODE (x) == IF_THEN_ELSE)
	  x = XEXP (x, 0);
	if (COMPARISON_P (x)
	    && loc == &XEXP (x, 0)
	    && XEXP (x, 1) == const0_rtx)
	  {
	    /* We've found a use of the flags that we understand.  */
	    struct comparison_use *cuse = &cmp->uses[cmp->n_uses++];
	    cuse->insn = insn;
	    cuse->loc = loc;
	    cuse->code = GET_CODE (x);
	  }
	else
	  goto fail;
      }
  return;

 fail:
  /* We failed to recognize this use of the flags register.  */
  cmp->missing_uses = true;
}

class find_comparison_dom_walker : public dom_walker
{
public:
  find_comparison_dom_walker (cdi_direction direction)
    : dom_walker (direction) {}

  virtual void before_dom_children (basic_block);
};

/* Identify comparison instructions within BB.  If the flags from the last
   compare in the BB is live at the end of the block, install the compare
   in BB->AUX.  Called via dom_walker.walk ().  */

void
find_comparison_dom_walker::before_dom_children (basic_block bb)
{
  struct comparison *last_cmp;
  rtx insn, next, last_clobber;
  bool last_cmp_valid;
  bitmap killed;

  killed = BITMAP_ALLOC (NULL);

  /* The last comparison that was made.  Will be reset to NULL
     once the flags are clobbered.  */
  last_cmp = NULL;

  /* True iff the last comparison has not been clobbered, nor
     have its inputs.  Used to eliminate duplicate compares.  */
  last_cmp_valid = false;

  /* The last insn that clobbered the flags, if that insn is of
     a form that may be valid for eliminating a following compare.
     To be reset to NULL once the flags are set otherwise.  */
  last_clobber = NULL;

  /* Propagate the last live comparison throughout the extended basic block. */
  if (single_pred_p (bb))
    {
      last_cmp = (struct comparison *) single_pred (bb)->aux;
      if (last_cmp)
	last_cmp_valid = last_cmp->inputs_valid;
    }

  for (insn = BB_HEAD (bb); insn; insn = next)
    {
      rtx src;

      next = (insn == BB_END (bb) ? NULL_RTX : NEXT_INSN (insn));
      if (!NONDEBUG_INSN_P (insn))
	continue;

      /* Compute the set of registers modified by this instruction.  */
      bitmap_clear (killed);
      df_simulate_find_defs (insn, killed);

      src = conforming_compare (insn);
      if (src)
	{
	  enum machine_mode src_mode = GET_MODE (src);

	  /* Eliminate a compare that's redundant with the previous.  */
	  if (last_cmp_valid
	      && rtx_equal_p (last_cmp->in_a, XEXP (src, 0))
	      && rtx_equal_p (last_cmp->in_b, XEXP (src, 1)))
	    {
	      rtx flags, x;
	      enum machine_mode new_mode
		= targetm.cc_modes_compatible (last_cmp->orig_mode, src_mode);

	      /* New mode is incompatible with the previous compare mode.  */
	      if (new_mode == VOIDmode)
		continue;

	      if (new_mode != last_cmp->orig_mode)
		{
		  flags = gen_rtx_REG (src_mode, targetm.flags_regnum);

		  /* Generate new comparison for substitution.  */
		  x = gen_rtx_COMPARE (new_mode, XEXP (src, 0), XEXP (src, 1));
		  x = gen_rtx_SET (VOIDmode, flags, x);

		  if (!validate_change (last_cmp->insn,
					&PATTERN (last_cmp->insn), x, false))
		    continue;

		  last_cmp->orig_mode = new_mode;
		}

	      delete_insn (insn);
	      continue;
	    }

	  last_cmp = XCNEW (struct comparison);
	  last_cmp->insn = insn;
	  last_cmp->prev_clobber = last_clobber;
	  last_cmp->in_a = XEXP (src, 0);
	  last_cmp->in_b = XEXP (src, 1);
	  last_cmp->orig_mode = src_mode;
	  all_compares.safe_push (last_cmp);

	  /* It's unusual, but be prepared for comparison patterns that
	     also clobber an input, or perhaps a scratch.  */
	  last_clobber = NULL;
	  last_cmp_valid = true;
	}

      /* Notice if this instruction kills the flags register.  */
      else if (bitmap_bit_p (killed, targetm.flags_regnum))
	{
	  /* See if this insn could be the "clobber" that eliminates
	     a future comparison.   */
	  last_clobber = (arithmetic_flags_clobber_p (insn) ? insn : NULL);

	  /* In either case, the previous compare is no longer valid.  */
	  last_cmp = NULL;
	  last_cmp_valid = false;
	  continue;
	}

      /* Notice if this instruction uses the flags register.  */
      else if (last_cmp)
	find_flags_uses_in_insn (last_cmp, insn);

      /* Notice if any of the inputs to the comparison have changed.  */
      if (last_cmp_valid
	  && (bitmap_bit_p (killed, REGNO (last_cmp->in_a))
	      || (REG_P (last_cmp->in_b)
		  && bitmap_bit_p (killed, REGNO (last_cmp->in_b)))))
	last_cmp_valid = false;
    }

  BITMAP_FREE (killed);

  /* Remember the live comparison for subsequent members of
     the extended basic block.  */
  if (last_cmp)
    {
      bb->aux = last_cmp;
      last_cmp->inputs_valid = last_cmp_valid;

      /* Look to see if the flags register is live outgoing here, and
	 incoming to any successor not part of the extended basic block.  */
      if (bitmap_bit_p (df_get_live_out (bb), targetm.flags_regnum))
	{
	  edge e;
	  edge_iterator ei;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      basic_block dest = e->dest;
	      if (bitmap_bit_p (df_get_live_in (bb),
				targetm.flags_regnum)
		  && !single_pred_p (dest))
		{
		  last_cmp->missing_uses = true;
		  break;
		}
	    }
	}
    }
}

/* Find all comparisons in the function.  */

static void
find_comparisons (void)
{
  calculate_dominance_info (CDI_DOMINATORS);

  find_comparison_dom_walker (CDI_DOMINATORS)
    .walk (cfun->cfg->x_entry_block_ptr);

  clear_aux_for_blocks ();
  free_dominance_info (CDI_DOMINATORS);
}

/* Select an alternate CC_MODE for a comparison insn comparing A and B.
   Note that inputs are almost certainly different than the IN_A and IN_B
   stored in CMP -- we're called while attempting to eliminate the compare
   after all.  Return the new FLAGS rtx if successful, else return NULL.
   Note that this function may start a change group.  */

static rtx
maybe_select_cc_mode (struct comparison *cmp, rtx a ATTRIBUTE_UNUSED,
		      rtx b ATTRIBUTE_UNUSED)
{
  enum machine_mode sel_mode;
  const int n = cmp->n_uses;
  rtx flags = NULL;

#ifndef SELECT_CC_MODE
  /* Minimize code differences when this target macro is undefined.  */
  return NULL;
#define SELECT_CC_MODE(A,B,C) (gcc_unreachable (), VOIDmode)
#endif

  /* If we don't have access to all of the uses, we can't validate.  */
  if (cmp->missing_uses || n == 0)
    return NULL;

  /* Find a new mode that works for all of the uses.  Special case the
     common case of exactly one use.  */
  if (n == 1)
    {
      sel_mode = SELECT_CC_MODE (cmp->uses[0].code, a, b);
      if (sel_mode != cmp->orig_mode)
	{
	  flags = gen_rtx_REG (sel_mode, targetm.flags_regnum);
	  validate_change (cmp->uses[0].insn, cmp->uses[0].loc, flags, true);
	}
    }
  else
    {
      int i;

      sel_mode = SELECT_CC_MODE (cmp->uses[0].code, a, b);
      for (i = 1; i < n; ++i)
	{
	  enum machine_mode new_mode;
	  new_mode = SELECT_CC_MODE (cmp->uses[i].code, a, b);
	  if (new_mode != sel_mode)
	    {
	      sel_mode = targetm.cc_modes_compatible (sel_mode, new_mode);
	      if (sel_mode == VOIDmode)
		return NULL;
	    }
	}
      
      if (sel_mode != cmp->orig_mode)
	{
	  flags = gen_rtx_REG (sel_mode, targetm.flags_regnum);
	  for (i = 0; i < n; ++i)
	    validate_change (cmp->uses[i].insn, cmp->uses[i].loc, flags, true);
	}
    }

  return flags;
}

/* Attempt to replace a comparison with a prior arithmetic insn that can
   compute the same flags value as the comparison itself.  Return true if
   successful, having made all rtl modifications necessary.  */

static bool
try_eliminate_compare (struct comparison *cmp)
{
  rtx x, insn, bb_head, flags, in_a, cmp_src;

  /* We must have found an interesting "clobber" preceding the compare.  */
  if (cmp->prev_clobber == NULL)
    return false;

  /* ??? For the moment we don't handle comparisons for which IN_B
     is a register.  We accepted these during initial comparison 
     recognition in order to eliminate duplicate compares.
     An improvement here would be to handle x = a - b; if (a cmp b).  */
  if (!CONSTANT_P (cmp->in_b))
    return false;

  /* Verify that IN_A is not clobbered in between CMP and PREV_CLOBBER.
     Given that this target requires this pass, we can assume that most
     insns do clobber the flags, and so the distance between the compare
     and the clobber is likely to be small.  */
  /* ??? This is one point at which one could argue that DF_REF_CHAIN would
     be useful, but it is thought to be too heavy-weight a solution here.  */

  in_a = cmp->in_a;
  insn = cmp->insn;
  bb_head = BB_HEAD (BLOCK_FOR_INSN (insn));
  for (insn = PREV_INSN (insn);
       insn != cmp->prev_clobber;
       insn = PREV_INSN (insn))
    {
      const int abnormal_flags
	= (DF_REF_CONDITIONAL | DF_REF_PARTIAL | DF_REF_MAY_CLOBBER
	   | DF_REF_MUST_CLOBBER | DF_REF_SIGN_EXTRACT
	   | DF_REF_ZERO_EXTRACT | DF_REF_STRICT_LOW_PART
	   | DF_REF_PRE_POST_MODIFY);
      df_ref def;

      /* Note that the BB_HEAD is always either a note or a label, but in
	 any case it means that IN_A is defined outside the block.  */
      if (insn == bb_head)
	return false;
      if (NOTE_P (insn) || DEBUG_INSN_P (insn))
	continue;

      /* Find a possible def of IN_A in INSN.  */
      FOR_EACH_INSN_DEF (def, insn)
	if (DF_REF_REGNO (def) == REGNO (in_a))
	  break;

      /* No definitions of IN_A; continue searching.  */
      if (def == NULL)
	continue;

      /* Bail if this is not a totally normal set of IN_A.  */
      if (DF_REF_IS_ARTIFICIAL (def))
	return false;
      if (DF_REF_FLAGS (def) & abnormal_flags)
	return false;

      /* We've found an insn between the compare and the clobber that sets
	 IN_A.  Given that pass_cprop_hardreg has not yet run, we still find
	 situations in which we can usefully look through a copy insn.  */
      x = single_set (insn);
      if (x == NULL)
	return false;
      in_a = SET_SRC (x);
      if (!REG_P (in_a))
	return false;
    }

  /* We've reached PREV_CLOBBER without finding a modification of IN_A.
     Validate that PREV_CLOBBER itself does in fact refer to IN_A.  Do
     recall that we've already validated the shape of PREV_CLOBBER.  */
  x = XVECEXP (PATTERN (insn), 0, 0);
  if (rtx_equal_p (SET_DEST (x), in_a))
    cmp_src = SET_SRC (x);

  /* Also check operations with implicit extensions, e.g.:
     [(set (reg:DI)
	   (zero_extend:DI (plus:SI (reg:SI)(reg:SI))))
      (set (reg:CCZ flags)
	   (compare:CCZ
	     (plus:SI (reg:SI)(reg:SI))
	     (const_int 0)))]				*/
  else if (REG_P (SET_DEST (x))
	   && REG_P (in_a)
	   && REGNO (SET_DEST (x)) == REGNO (in_a)
	   && (GET_CODE (SET_SRC (x)) == ZERO_EXTEND
	       || GET_CODE (SET_SRC (x)) == SIGN_EXTEND)
	   && GET_MODE (XEXP (SET_SRC (x), 0)) == GET_MODE (in_a))
    cmp_src = XEXP (SET_SRC (x), 0);
  else
    return false;

  /* Determine if we ought to use a different CC_MODE here.  */
  flags = maybe_select_cc_mode (cmp, cmp_src, cmp->in_b);
  if (flags == NULL)
    flags = gen_rtx_REG (cmp->orig_mode, targetm.flags_regnum);

  /* Generate a new comparison for installation in the setter.  */
  x = copy_rtx (cmp_src);
  x = gen_rtx_COMPARE (GET_MODE (flags), x, cmp->in_b);
  x = gen_rtx_SET (VOIDmode, flags, x);

  /* Succeed if the new instruction is valid.  Note that we may have started
     a change group within maybe_select_cc_mode, therefore we must continue. */
  validate_change (insn, &XVECEXP (PATTERN (insn), 0, 1), x, true);
  if (!apply_change_group ())
    return false;
 
  /* Success.  Delete the compare insn...  */
  delete_insn (cmp->insn);

  /* ... and any notes that are now invalid due to multiple sets.  */
  x = find_regno_note (insn, REG_UNUSED, targetm.flags_regnum);
  if (x)
    remove_note (insn, x);
  x = find_reg_note (insn, REG_EQUAL, NULL);
  if (x)
    remove_note (insn, x);
  x = find_reg_note (insn, REG_EQUIV, NULL);
  if (x)
    remove_note (insn, x);

  return true;
}

/* Main entry point to the pass.  */

static unsigned int
execute_compare_elim_after_reload (void)
{
  df_analyze ();

  gcc_checking_assert (!all_compares.exists ());

  /* Locate all comparisons and their uses, and eliminate duplicates.  */
  find_comparisons ();
  if (all_compares.exists ())
    {
      struct comparison *cmp;
      size_t i;

      /* Eliminate comparisons that are redundant with flags computation.  */
      FOR_EACH_VEC_ELT (all_compares, i, cmp)
	{
	  try_eliminate_compare (cmp);
	  XDELETE (cmp);
	}

      all_compares.release ();
    }

  return 0;
}

namespace {

const pass_data pass_data_compare_elim_after_reload =
{
  RTL_PASS, /* type */
  "cmpelim", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_df_finish | TODO_df_verify ), /* todo_flags_finish */
};

class pass_compare_elim_after_reload : public rtl_opt_pass
{
public:
  pass_compare_elim_after_reload (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_compare_elim_after_reload, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      /* Setting this target hook value is how a backend indicates the need.  */
      if (targetm.flags_regnum == INVALID_REGNUM)
	return false;
      return flag_compare_elim_after_reload;
    }

  virtual unsigned int execute (function *)
    {
      return execute_compare_elim_after_reload ();
    }

}; // class pass_compare_elim_after_reload

} // anon namespace

rtl_opt_pass *
make_pass_compare_elim_after_reload (gcc::context *ctxt)
{
  return new pass_compare_elim_after_reload (ctxt);
}
