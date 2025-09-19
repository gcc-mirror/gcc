/* Convert clear lowest bit set idiom into a more efficient
   bclr sequence when possible.

   Copyright (C) 2018-2025 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "backend.h"
#include "df.h"
#include "tree-pass.h"

/* x & (x - 1) clears the lowest set bit in x.  If we have a ctz (x) nearby,
   then we can use a bclr with the bit position defined by the output of
   the ctz.  */

namespace {

const pass_data pass_data_bclr_lowest_set_bit =
{
  RTL_PASS, /* type */
  "bclr_lowest_set_bit", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_bclr_lowest_set_bit : public rtl_opt_pass
{
public:
  pass_bclr_lowest_set_bit (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_bclr_lowest_set_bit, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      /* This uses ctz and bclr, so we need ZBB and ZBS
	 instructions.  */
      return TARGET_ZBB && TARGET_ZBS && optimize > 0;
    }
  virtual unsigned int execute (function *);

private:
}; // class pass_bclr_lowest_set_bit

/* Look at real insns before START to see if any of them compute ctz (src).
   If so, return the insn that is a ctz, otherwise return NULL.  Look no
   further than LIMIT real statements and do not leave this basic block.  */

rtx_insn *
find_prior_ctz (rtx_insn *start, rtx src, int limit)
{
  rtx_insn *prev = start;
  while (prev && limit > 0)
    {
      prev = prev_nonnote_nondebug_insn_bb (prev);
      limit--;

      if (prev)
	{
	  rtx set = single_set (prev);
	  if (!set)
	    continue;

	  /* A ctz of a SI object on rv64 will have an
	     SUBREG argument.  */
	  if (GET_CODE (SET_SRC (set)) == CTZ
	      && (XEXP (SET_SRC (set), 0) == src
		  || (SUBREG_P (XEXP (SET_SRC (set), 0))
		      && SUBREG_REG (XEXP (SET_SRC (set), 0)) == src)))
	    {
	      /* We've found a CTZ, make sure neither the input nor
		 the output change between the CTZ and START.  */
	      if (reg_set_between_p (src, prev, start)
		  || reg_set_between_p (SET_DEST (set), prev, start))
		return NULL;

	      /* Everything looks good.  */
	      return prev;
	    }
	}
    }
  return NULL;
}

/* Look at real insns after START to see if any of them compute ctz (src).
   If so, return the insn that is a ctz, otherwise return NULL.  Look no
   further than LIMIT real statements and do not leave this basic block.  */

rtx_insn *
find_later_ctz (rtx_insn *start, rtx src, int limit)
{
  rtx_insn *next = start;
  while (next && limit > 0)
    {
      next = next_nonnote_nondebug_insn_bb (next);
      limit--;

      if (next)
	{
	  rtx set = single_set (next);
	  if (!set)
	    continue;

	  /* A ctz of a SI object on rv64 will have an
	     SUBREG argument.  */
	  if (GET_CODE (SET_SRC (set)) == CTZ
	      && (XEXP (SET_SRC (set), 0) == src
		  || (SUBREG_P (XEXP (SET_SRC (set), 0))
		      && SUBREG_REG (XEXP (SET_SRC (set), 0)) == src)))
	    {
	      /* We've found a CTZ.  The CTZ is going to be moved, so
		 we need to verify its input doesn't change between
		 START and NEXT.  We also have to verify that its
		 destination is unused between those points.  */
	      if (reg_set_between_p (XEXP (SET_SRC (set), 0), start, next)
		  || reg_used_between_p (SET_DEST (set), start, next))
		return NULL;

	      return next;
	    }
	}
    }
  return NULL;
}

/* So the basic idea here is to find x & (x - 1) idioms which clear the
   lowest set bit.  If there is a nearby ctz (x), then we can profitably
   use a ctz+bclr sequence instead, essentially replacing the addi+and
   with bclr.  This should often be more efficient and save space.

   We don't do this in gimple because the cost model would reject as the
   optimized for appear more expensive from its costing model.

   Combine won't work as there's no data dependency between the
   ctz and the x & (x - 1) idiom.

   Peepholing doesn't work consistently because we're constantly
   inserting unrelated instructions between the two components of
   the x & (x - 1) idiom.  We'd have to match the ctz in various
   positions as well as deal with random insns the scheduler puts
   in the middle of the key instrutions.

   So, this mini pass to optimize this scenario.  */

unsigned int
pass_bclr_lowest_set_bit::execute (function *fn)
{
  basic_block bb;

  /* Scan all the blocks, once.  */
  FOR_ALL_BB_FN (bb, fn)
    {
      rtx_insn *insn;
      /* Scan all the insns once.  */
      FOR_BB_INSNS (bb, insn)
	{
	  /* Ignore as much as we can.  */
	  if (!NONDEBUG_INSN_P (insn)
	      || JUMP_P (insn)
	      || CALL_P (insn))
	    continue;

	  rtx dec_set = single_set (insn);
	  if (!dec_set)
	    continue;

	  rtx dec_src = SET_SRC (dec_set);
	  rtx dec_dest = SET_DEST (dec_set);

	  /* For a 32 bit object on rv64, the decrement will
	     be wrapped by a SIGN_EXTEND.  Strip it.  */
	  if (GET_CODE (dec_src) == SIGN_EXTEND)
	    dec_src = XEXP (dec_src, 0);

	  /* Verify it's res = x - 1, if not proceed to the next insn.  */
	  if (!dec_set
	      || !REG_P (dec_dest)
	      || GET_CODE (dec_src) != PLUS
	      || (XEXP (dec_src, 1) != CONSTM1_RTX (GET_MODE (dec_src))))
	    continue;

	  /* Get the value being decremented.  Note it might be
	     wrapped by a SUBREG which we strip.  */
	  dec_src = XEXP (dec_src, 0);
	  if (SUBREG_P (dec_src))
	    dec_src = SUBREG_REG (dec_src);

	  /* So we've found dest = src - 1;  Now look at the next
	     real insn and see if it's dest2 = (dest & src).  */
	  rtx_insn *next = next_nonnote_nondebug_insn_bb (insn);
	  if (!next)
	    continue;

	  rtx and_set = single_set (next);

	  if (!and_set)
	    continue;

	  rtx and_src = SET_SRC (and_set);
	  rtx and_dest = SET_DEST (and_set);
	  if (!and_set
	      || !REG_P (and_dest)
	      || GET_CODE (and_src) != AND)
	    continue;

	  rtx and_op0 = XEXP (and_src, 0);
	  rtx and_op1 = XEXP (and_src, 1);

	  if (dec_dest != and_op0 && dec_dest != and_op1)
	    continue;

	  if (dec_src != and_op0 && dec_src != and_op1)
	    continue;

	  /* We've found x & (x - 1).  Now look for a suitable ctz nearby.  */

	  rtx_insn *prior_ctz = find_prior_ctz (insn, dec_src, 10);
	  if (prior_ctz)
	    {
	      rtx prior_ctz_output = SET_DEST (single_set (prior_ctz));

	      /* Create a pattern for the variable bit clear idiom.  */
	      rtx pat = gen_rtx_ROTATE (GET_MODE (dec_dest),
				        GEN_INT (-2),
					gen_lowpart (QImode, prior_ctz_output));
	      pat = gen_rtx_AND (GET_MODE (dec_dest), pat, dec_src);

	      /* Slam that pattern in as the SET_SRC of the original AND.  */
	      SET_SRC (and_set) = pat;
	      INSN_CODE (next) = -1;
	      df_insn_rescan (next);

	      /* Start next loop iteration.  */
	      continue;
	    }

	  /* Typically in cases where we can optimize, we'll find a REG->REG
	     copy into DEC_SRC immediately before INSN.  Look for it.  */
	  rtx_insn *prev = prev_nonnote_nondebug_insn_bb (insn);
	  rtx copy = NULL_RTX;
	  if (prev
	      && (copy = single_set (prev)) != NULL_RTX
	      && SET_SRC (copy) == dec_src)
	    dec_src = SET_DEST (copy);

	  /* We didn't find a CTZ before INSN.  So look after NEXT.
	     This case is more complex as we have to move insns around.  */
	  rtx_insn *later_ctz = find_later_ctz (next, dec_src, 10);
	  if (later_ctz)
	    {
	      /* Remove the CTZ from the stream and reemit it immediately
		 after NEXT.  XXX FIXME.  Need to prove this is safe.  */
	      df_insn_delete (later_ctz);
	      remove_insn (later_ctz);
	      SET_PREV_INSN (later_ctz) = NULL;
	      SET_NEXT_INSN (later_ctz) = NULL;
	      df_insn_rescan (emit_insn_after (PATTERN (later_ctz), next));

	      /* Now construct the bclr insn and add it to the stream.  */
	      rtx later_ctz_output = SET_DEST (single_set (later_ctz));
	      rtx pat = gen_rtx_ROTATE (GET_MODE (dec_dest),
				        GEN_INT (-2),
					gen_lowpart (QImode, later_ctz_output));
	      pat = gen_rtx_AND (GET_MODE (dec_dest), pat, dec_src);
	      pat = gen_rtx_SET (and_dest, pat);
	      df_insn_rescan (emit_insn_after (pat, NEXT_INSN (next)));
	    }
	}
    }

  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_bclr_lowest_set_bit (gcc::context *ctxt)
{
  return new pass_bclr_lowest_set_bit (ctxt);
}
