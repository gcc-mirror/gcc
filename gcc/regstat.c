/* Scanning of rtl for dataflow analysis.
   Copyright (C) 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck (zadeck@naturalbridge.com).

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
#include "flags.h"
#include "regs.h"
#include "output.h"
#include "except.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "timevar.h"
#include "df.h"


struct regstat_n_sets_and_refs_t *regstat_n_sets_and_refs;

/*----------------------------------------------------------------------------
   REG_N_SETS and REG_N_REFS.
   ----------------------------------------------------------------------------*/

/* If a pass need to change these values in some magical way or or the
   pass needs to have accurate values for these and is not using
   incremental df scanning, then it should use REG_N_SETS and
   REG_N_USES.  If the pass is doing incremental scanning then it
   should be getting the info from DF_REG_DEF_COUNT and
   DF_REG_USE_COUNT.  */

void
regstat_init_n_sets_and_refs (void)
{
  unsigned int i;
  unsigned int max_regno = max_reg_num ();

  timevar_push (TV_REG_STATS);
  df_grow_reg_info ();
  gcc_assert (!regstat_n_sets_and_refs);

  regstat_n_sets_and_refs = XNEWVEC (struct regstat_n_sets_and_refs_t, max_regno);

  if (MAY_HAVE_DEBUG_INSNS)
    for (i = 0; i < max_regno; i++)
      {
	int use_count;
	df_ref use;

	use_count = DF_REG_USE_COUNT (i);
	for (use = DF_REG_USE_CHAIN (i); use; use = DF_REF_NEXT_REG (use))
	  if (DF_REF_INSN_INFO (use) && DEBUG_INSN_P (DF_REF_INSN (use)))
	    use_count--;


	SET_REG_N_SETS (i, DF_REG_DEF_COUNT (i));
	SET_REG_N_REFS (i, use_count + REG_N_SETS (i));
      }
  else
    for (i = 0; i < max_regno; i++)
      {
	SET_REG_N_SETS (i, DF_REG_DEF_COUNT (i));
	SET_REG_N_REFS (i, DF_REG_USE_COUNT (i) + REG_N_SETS (i));
      }
  timevar_pop (TV_REG_STATS);

}


/* Free the array that holds the REG_N_SETS and REG_N_REFS.  */

void
regstat_free_n_sets_and_refs (void)
{
  gcc_assert (regstat_n_sets_and_refs);
  free (regstat_n_sets_and_refs);
  regstat_n_sets_and_refs = NULL;
}


/*----------------------------------------------------------------------------
   REGISTER INFORMATION

   Process REG_N_DEATHS, REG_LIVE_LENGTH, REG_N_CALLS_CROSSED,
   REG_N_THROWING_CALLS_CROSSED and REG_BASIC_BLOCK.

   ----------------------------------------------------------------------------*/

static bitmap setjmp_crosses;
struct reg_info_t *reg_info_p;

/* The number allocated elements of reg_info_p.  */
size_t reg_info_p_size;

/* Compute register info: lifetime, bb, and number of defs and uses
   for basic block BB.  The three bitvectors are scratch regs used
   here.  */

static void
regstat_bb_compute_ri (unsigned int bb_index,
		       bitmap live, bitmap do_not_gen, bitmap artificial_uses,
		       bitmap local_live, bitmap local_processed)
{
  basic_block bb = BASIC_BLOCK (bb_index);
  rtx insn;
  df_ref *def_rec;
  df_ref *use_rec;
  int luid = 0;
  bitmap_iterator bi;
  unsigned int regno;

  bitmap_copy (live, df_get_live_out (bb));
  bitmap_clear (artificial_uses);

  /* Process the regs live at the end of the block.  Mark them as
     not local to any one basic block.  */
  EXECUTE_IF_SET_IN_BITMAP (live, 0, regno, bi)
    REG_BASIC_BLOCK (regno) = REG_BLOCK_GLOBAL;

  /* Process the artificial defs and uses at the bottom of the block
     to begin processing.  */
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	bitmap_clear_bit (live, DF_REF_REGNO (def));
    }

  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == 0)
	{
	  regno = DF_REF_REGNO (use);
	  bitmap_set_bit (live, regno);
	  bitmap_set_bit (artificial_uses, regno);
	}
    }

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      unsigned int regno;
      bitmap_iterator bi;
      struct df_mw_hardreg **mws_rec;
      rtx link;

      if (!NONDEBUG_INSN_P (insn))
	continue;

      /* Increment the live_length for all of the registers that
	 are are referenced in this block and live at this
	 particular point.  */
      EXECUTE_IF_SET_IN_BITMAP (local_live, 0, regno, bi)
	{
	  REG_LIVE_LENGTH (regno)++;
	}
      luid++;

      bitmap_clear (do_not_gen);

      link = REG_NOTES (insn);
      while (link)
	{
	  if (REG_NOTE_KIND (link) == REG_DEAD)
	    REG_N_DEATHS(REGNO (XEXP (link, 0)))++;
	  link = XEXP (link, 1);
	}

      /* Process the defs.  */
      if (CALL_P (insn))
	{
	  bool can_throw = can_throw_internal (insn);
	  bool set_jump = (find_reg_note (insn, REG_SETJMP, NULL) != NULL);
	  EXECUTE_IF_SET_IN_BITMAP (live, 0, regno, bi)
	    {
	      REG_N_CALLS_CROSSED (regno)++;
	      REG_FREQ_CALLS_CROSSED (regno) += REG_FREQ_FROM_BB (bb);
	      if (can_throw)
		REG_N_THROWING_CALLS_CROSSED (regno)++;

	      /* We have a problem with any pseudoreg that lives
		 across the setjmp.  ANSI says that if a user variable
		 does not change in value between the setjmp and the
		 longjmp, then the longjmp preserves it.  This
		 includes longjmp from a place where the pseudo
		 appears dead.  (In principle, the value still exists
		 if it is in scope.)  If the pseudo goes in a hard
		 reg, some other value may occupy that hard reg where
		 this pseudo is dead, thus clobbering the pseudo.
		 Conclusion: such a pseudo must not go in a hard
		 reg.  */
	      if (set_jump)
		bitmap_set_bit (setjmp_crosses, regno);
	    }
	}

      /* We only care about real sets for calls.  Clobbers only
	 may clobbers cannot be depended on.  */
      for (mws_rec = DF_INSN_UID_MWS (uid); *mws_rec; mws_rec++)
	{
	  struct df_mw_hardreg *mws = *mws_rec;
	  if (DF_MWS_REG_DEF_P (mws))
	    {
	      bool all_dead = true;
	      unsigned int r;

	      for (r=mws->start_regno; r <= mws->end_regno; r++)
		if ((bitmap_bit_p (live, r))
		    || bitmap_bit_p (artificial_uses, r))
		  {
		    all_dead = false;
		    break;
		  }

	      if (all_dead)
		{
		  unsigned int regno = mws->start_regno;
		  bitmap_set_bit (do_not_gen, regno);
		  /* Only do this if the value is totally dead.  */
		  REG_LIVE_LENGTH (regno)++;
		}
	    }
	}

      /* All of the defs except the return value are some sort of
	 clobber.  This code is for the return.  */
      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  if ((!CALL_P (insn))
	      || (!(DF_REF_FLAGS (def) & (DF_REF_MUST_CLOBBER | DF_REF_MAY_CLOBBER))))
	    {
	      unsigned int dregno = DF_REF_REGNO (def);

	      if (bitmap_bit_p (live, dregno))
		{
		  /* If we have seen this regno, then it has already been
		     processed correctly with the per insn increment.  If we
		     have not seen it we need to add the length from here to
		     the end of the block to the live length.  */
		  if (bitmap_bit_p (local_processed, dregno))
		    {
		      if (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL)))
			bitmap_clear_bit (local_live, dregno);
		    }
		  else
		    {
		      bitmap_set_bit (local_processed, dregno);
		      REG_LIVE_LENGTH (dregno) += luid;
		    }
		}
	      else if ((!(DF_REF_FLAGS (def) & DF_REF_MW_HARDREG))
		       && (!bitmap_bit_p (artificial_uses, dregno)))
		{
		  REG_LIVE_LENGTH (dregno)++;
		}

	      if (dregno >= FIRST_PSEUDO_REGISTER)
		{
		  REG_FREQ (dregno) += REG_FREQ_FROM_BB (bb);
		  if (REG_BASIC_BLOCK (dregno) == REG_BLOCK_UNKNOWN)
		    REG_BASIC_BLOCK (dregno) = bb->index;
		  else if (REG_BASIC_BLOCK (dregno) != bb->index)
		    REG_BASIC_BLOCK (dregno) = REG_BLOCK_GLOBAL;
		}

	      if (!(DF_REF_FLAGS (def) & (DF_REF_MUST_CLOBBER + DF_REF_MAY_CLOBBER)))
		bitmap_set_bit (do_not_gen, dregno);

	      /* Kill this register if it is not a subreg store or conditional store.  */
	      if (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL)))
		bitmap_clear_bit (live, dregno);
	    }
	}

      for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  unsigned int uregno = DF_REF_REGNO (use);

	  if (uregno >= FIRST_PSEUDO_REGISTER)
	    {
	      REG_FREQ (uregno) += REG_FREQ_FROM_BB (bb);
	      if (REG_BASIC_BLOCK (uregno) == REG_BLOCK_UNKNOWN)
		REG_BASIC_BLOCK (uregno) = bb->index;
	      else if (REG_BASIC_BLOCK (uregno) != bb->index)
		REG_BASIC_BLOCK (uregno) = REG_BLOCK_GLOBAL;
	    }

	  if (bitmap_set_bit (live, uregno))
	    {
	      /* This register is now live.  */

	      /* If we have seen this regno, then it has already been
		 processed correctly with the per insn increment.  If
		 we have not seen it we set the bit so that begins to
		 get processed locally.  Note that we don't even get
		 here if the variable was live at the end of the block
		 since just a ref inside the block does not effect the
		 calculations.  */
	      REG_LIVE_LENGTH (uregno) ++;
	      bitmap_set_bit (local_live, uregno);
	      bitmap_set_bit (local_processed, uregno);
	    }
	}
    }

  /* Add the length of the block to all of the registers that were not
     referenced, but still live in this block.  */
  bitmap_and_compl_into (live, local_processed);
  EXECUTE_IF_SET_IN_BITMAP (live, 0, regno, bi)
    REG_LIVE_LENGTH (regno) += luid;

  bitmap_clear (local_processed);
  bitmap_clear (local_live);
}


/* Compute register info: lifetime, bb, and number of defs and uses.  */
void
regstat_compute_ri (void)
{
  basic_block bb;
  bitmap live = BITMAP_ALLOC (&df_bitmap_obstack);
  bitmap do_not_gen = BITMAP_ALLOC (&df_bitmap_obstack);
  bitmap artificial_uses = BITMAP_ALLOC (&df_bitmap_obstack);
  bitmap local_live = BITMAP_ALLOC (&df_bitmap_obstack);
  bitmap local_processed = BITMAP_ALLOC (&df_bitmap_obstack);
  unsigned int regno;
  bitmap_iterator bi;

  /* Initialize everything.  */

  gcc_assert (!reg_info_p);

  timevar_push (TV_REG_STATS);
  setjmp_crosses = BITMAP_ALLOC (&df_bitmap_obstack);
  max_regno = max_reg_num ();
  reg_info_p_size = max_regno;
  reg_info_p = XCNEWVEC (struct reg_info_t, max_regno);

  FOR_EACH_BB (bb)
    {
      regstat_bb_compute_ri (bb->index, live, do_not_gen, artificial_uses,
			     local_live, local_processed);
    }

  BITMAP_FREE (live);
  BITMAP_FREE (do_not_gen);
  BITMAP_FREE (artificial_uses);

  /* See the setjmp comment in regstat_ri_bb_compute.  */
  EXECUTE_IF_SET_IN_BITMAP (setjmp_crosses, FIRST_PSEUDO_REGISTER, regno, bi)
    {
      REG_BASIC_BLOCK (regno) = REG_BLOCK_UNKNOWN;
      REG_LIVE_LENGTH (regno) = -1;
    }

  BITMAP_FREE (local_live);
  BITMAP_FREE (local_processed);
  timevar_pop (TV_REG_STATS);
}


/* Free all storage associated with the problem.  */

void
regstat_free_ri (void)
{
  gcc_assert (reg_info_p);
  reg_info_p_size = 0;
  free (reg_info_p);
  reg_info_p = NULL;

  BITMAP_FREE (setjmp_crosses);
}


/* Return a bitmap containing the set of registers that cross a setjmp.
   The client should not change or delete this bitmap.  */

bitmap
regstat_get_setjmp_crosses (void)
{
  return setjmp_crosses;
}

/*----------------------------------------------------------------------------
   Process REG_N_CALLS_CROSSED.

   This is used by sched_deps.  A good implementation of sched-deps
   would really process the blocks directly rather than going through
   lists of insns.  If it did this, it could use the exact regs that
   cross an individual call rather than using this info that merges
   the info for all calls.

   ----------------------------------------------------------------------------*/



/* Compute calls crossed for BB. Live is a scratch bitvector.  */

static void
regstat_bb_compute_calls_crossed (unsigned int bb_index, bitmap live)
{
  basic_block bb = BASIC_BLOCK (bb_index);
  rtx insn;
  df_ref *def_rec;
  df_ref *use_rec;

  bitmap_copy (live, df_get_live_out (bb));

  /* Process the artificial defs and uses at the bottom of the block
     to begin processing.  */
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	bitmap_clear_bit (live, DF_REF_REGNO (def));
    }

  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == 0)
	bitmap_set_bit (live, DF_REF_REGNO (use));
    }

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      unsigned int regno;

      if (!INSN_P (insn))
	continue;

      /* Process the defs.  */
      if (CALL_P (insn))
	{
	  bitmap_iterator bi;
	  EXECUTE_IF_SET_IN_BITMAP (live, 0, regno, bi)
	    {
	      REG_N_CALLS_CROSSED (regno)++;
	      REG_FREQ_CALLS_CROSSED (regno) += REG_FREQ_FROM_BB (bb);
	    }
	}

      /* All of the defs except the return value are some sort of
	 clobber.  This code is for the return.  */
      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  if ((!CALL_P (insn))
	      || (!(DF_REF_FLAGS (def) & (DF_REF_MUST_CLOBBER | DF_REF_MAY_CLOBBER))))
	    {
	      /* Kill this register if it is not a subreg store or conditional store.  */
	      if (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL)))
		bitmap_clear_bit (live, DF_REF_REGNO (def));
	    }
	}

      for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  bitmap_set_bit (live, DF_REF_REGNO (use));
	}
    }
}


/* Compute register info: lifetime, bb, and number of defs and uses.  */
void
regstat_compute_calls_crossed (void)
{
  basic_block bb;
  bitmap live = BITMAP_ALLOC (&df_bitmap_obstack);

  /* Initialize everything.  */
  gcc_assert (!reg_info_p);

  timevar_push (TV_REG_STATS);
  max_regno = max_reg_num ();
  reg_info_p_size = max_regno;
  reg_info_p = XCNEWVEC (struct reg_info_t, max_regno);

  FOR_EACH_BB (bb)
    {
      regstat_bb_compute_calls_crossed (bb->index, live);
    }

  BITMAP_FREE (live);
  timevar_pop (TV_REG_STATS);
}


/* Free all storage associated with the problem.  */

void
regstat_free_calls_crossed (void)
{
  gcc_assert (reg_info_p);
  reg_info_p_size = 0;
  free (reg_info_p);
  reg_info_p = NULL;
}

