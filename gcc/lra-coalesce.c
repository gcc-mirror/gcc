/* Coalesce spilled pseudos.
   Copyright (C) 2010-2014 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@redhat.com>.

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
along with GCC; see the file COPYING3.	If not see
<http://www.gnu.org/licenses/>.	 */


/* This file contains a pass making some simple RTL code
   transformations by coalescing pseudos to remove some move insns.

   Spilling pseudos in LRA can create memory-memory moves.  We should
   remove potential memory-memory moves before the next constraint
   pass because the constraint pass will generate additional insns for
   such moves and all these insns will be hard to remove afterwards.

   Here we coalesce only spilled pseudos.  Coalescing non-spilled
   pseudos (with different hard regs) might result in spilling
   additional pseudos because of possible conflicts with other
   non-spilled pseudos and, as a consequence, in more constraint
   passes and even LRA infinite cycling.  Trivial the same hard
   register moves will be removed by subsequent compiler passes.

   We don't coalesce special reload pseudos.  It complicates LRA code
   a lot without visible generated code improvement.

   The pseudo live-ranges are used to find conflicting pseudos during
   coalescing.

   Most frequently executed moves is tried to be coalesced first.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "basic-block.h"
#include "except.h"
#include "timevar.h"
#include "ira.h"
#include "lra-int.h"
#include "df.h"

/* Arrays whose elements represent the first and the next pseudo
   (regno) in the coalesced pseudos group to which given pseudo (its
   regno is the index) belongs.	 The next of the last pseudo in the
   group refers to the first pseudo in the group, in other words the
   group is represented by a cyclic list.  */
static int *first_coalesced_pseudo, *next_coalesced_pseudo;

/* The function is used to sort moves according to their execution
   frequencies.	 */
static int
move_freq_compare_func (const void *v1p, const void *v2p)
{
  rtx mv1 = *(const rtx *) v1p;
  rtx mv2 = *(const rtx *) v2p;
  int pri1, pri2;

  pri1 = REG_FREQ_FROM_BB (BLOCK_FOR_INSN (mv1));
  pri2 = REG_FREQ_FROM_BB (BLOCK_FOR_INSN (mv2));
  if (pri2 - pri1)
    return pri2 - pri1;

  /* If frequencies are equal, sort by moves, so that the results of
     qsort leave nothing to chance.  */
  return (int) INSN_UID (mv1) - (int) INSN_UID (mv2);
}

/* Pseudos which go away after coalescing.  */
static bitmap_head coalesced_pseudos_bitmap;

/* Merge two sets of coalesced pseudos given correspondingly by
   pseudos REGNO1 and REGNO2 (more accurately merging REGNO2 group
   into REGNO1 group).	Set up COALESCED_PSEUDOS_BITMAP.  */
static void
merge_pseudos (int regno1, int regno2)
{
  int regno, first, first2, last, next;

  first = first_coalesced_pseudo[regno1];
  if ((first2 = first_coalesced_pseudo[regno2]) == first)
    return;
  for (last = regno2, regno = next_coalesced_pseudo[regno2];;
       regno = next_coalesced_pseudo[regno])
    {
      first_coalesced_pseudo[regno] = first;
      bitmap_set_bit (&coalesced_pseudos_bitmap, regno);
      if (regno == regno2)
	break;
      last = regno;
    }
  next = next_coalesced_pseudo[first];
  next_coalesced_pseudo[first] = regno2;
  next_coalesced_pseudo[last] = next;
  lra_reg_info[first].live_ranges
    = (lra_merge_live_ranges
       (lra_reg_info[first].live_ranges,
	lra_copy_live_range_list (lra_reg_info[first2].live_ranges)));
  if (GET_MODE_SIZE (lra_reg_info[first].biggest_mode)
      < GET_MODE_SIZE (lra_reg_info[first2].biggest_mode))
    lra_reg_info[first].biggest_mode = lra_reg_info[first2].biggest_mode;
}

/* Change pseudos in *LOC on their coalescing group
   representatives.  */
static bool
substitute (rtx *loc)
{
  int i, regno;
  const char *fmt;
  enum rtx_code code;
  bool res;

  if (*loc == NULL_RTX)
    return false;
  code = GET_CODE (*loc);
  if (code == REG)
    {
      regno = REGNO (*loc);
      if (regno < FIRST_PSEUDO_REGISTER
	  || first_coalesced_pseudo[regno] == regno)
	return false;
      *loc = regno_reg_rtx[first_coalesced_pseudo[regno]];
      return true;
    }

  res = false;
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (substitute (&XEXP (*loc, i)))
	    res = true;
	}
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (*loc, i) - 1; j >= 0; j--)
	    if (substitute (&XVECEXP (*loc, i, j)))
	      res = true;
	}
    }
  return res;
}

/* The current iteration (1, 2, ...) of the coalescing pass.  */
int lra_coalesce_iter;

/* Return true if the move involving REGNO1 and REGNO2 is a potential
   memory-memory move.	*/
static bool
mem_move_p (int regno1, int regno2)
{
  return reg_renumber[regno1] < 0 && reg_renumber[regno2] < 0;
}

/* Pseudos used instead of the coalesced pseudos.  */
static bitmap_head used_pseudos_bitmap;

/* Set up USED_PSEUDOS_BITMAP, and update LR_BITMAP (a BB live info
   bitmap).  */
static void
update_live_info (bitmap lr_bitmap)
{
  unsigned int j;
  bitmap_iterator bi;

  bitmap_clear (&used_pseudos_bitmap);
  EXECUTE_IF_AND_IN_BITMAP (&coalesced_pseudos_bitmap, lr_bitmap,
			    FIRST_PSEUDO_REGISTER, j, bi)
    bitmap_set_bit (&used_pseudos_bitmap, first_coalesced_pseudo[j]);
  if (! bitmap_empty_p (&used_pseudos_bitmap))
    {
      bitmap_and_compl_into (lr_bitmap, &coalesced_pseudos_bitmap);
      bitmap_ior_into (lr_bitmap, &used_pseudos_bitmap);
    }
}

/* Return true if pseudo REGNO can be potentially coalesced.  */
static bool
coalescable_pseudo_p (int regno)
{
  lra_assert (regno >= FIRST_PSEUDO_REGISTER);
  return (/* We don't want to coalesce regnos with equivalences, at
	     least without updating this info.  */
	  ira_reg_equiv[regno].constant == NULL_RTX
	  && ira_reg_equiv[regno].memory == NULL_RTX
	  && ira_reg_equiv[regno].invariant == NULL_RTX);
}

/* The major function for aggressive pseudo coalescing of moves only
   if the both pseudos were spilled and not special reload pseudos.  */
bool
lra_coalesce (void)
{
  basic_block bb;
  rtx mv, set, insn, next, *sorted_moves;
  int i, mv_num, sregno, dregno;
  unsigned int regno;
  int coalesced_moves;
  int max_regno = max_reg_num ();
  bitmap_head involved_insns_bitmap;
  bitmap_head result_pseudo_vals_bitmap;
  bitmap_iterator bi;

  timevar_push (TV_LRA_COALESCE);

  if (lra_dump_file != NULL)
    fprintf (lra_dump_file,
	     "\n********** Pseudos coalescing #%d: **********\n\n",
	     ++lra_coalesce_iter);
  first_coalesced_pseudo = XNEWVEC (int, max_regno);
  next_coalesced_pseudo = XNEWVEC (int, max_regno);
  for (i = 0; i < max_regno; i++)
    first_coalesced_pseudo[i] = next_coalesced_pseudo[i] = i;
  sorted_moves = XNEWVEC (rtx, get_max_uid ());
  mv_num = 0;
  /* Collect moves.  */
  coalesced_moves = 0;
  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS_SAFE (bb, insn, next)
	if (INSN_P (insn)
	    && (set = single_set (insn)) != NULL_RTX
	    && REG_P (SET_DEST (set)) && REG_P (SET_SRC (set))
	    && (sregno = REGNO (SET_SRC (set))) >= FIRST_PSEUDO_REGISTER
	    && (dregno = REGNO (SET_DEST (set))) >= FIRST_PSEUDO_REGISTER
	    && mem_move_p (sregno, dregno)
	    && coalescable_pseudo_p (sregno) && coalescable_pseudo_p (dregno)
	    && ! side_effects_p (set)
	    && !(lra_intersected_live_ranges_p
		 (lra_reg_info[sregno].live_ranges,
		  lra_reg_info[dregno].live_ranges)))
	  sorted_moves[mv_num++] = insn;
    }
  qsort (sorted_moves, mv_num, sizeof (rtx), move_freq_compare_func);
  /* Coalesced copies, most frequently executed first.	*/
  bitmap_initialize (&coalesced_pseudos_bitmap, &reg_obstack);
  bitmap_initialize (&involved_insns_bitmap, &reg_obstack);
  for (i = 0; i < mv_num; i++)
    {
      mv = sorted_moves[i];
      set = single_set (mv);
      lra_assert (set != NULL && REG_P (SET_SRC (set))
		  && REG_P (SET_DEST (set)));
      sregno = REGNO (SET_SRC (set));
      dregno = REGNO (SET_DEST (set));
      if (first_coalesced_pseudo[sregno] == first_coalesced_pseudo[dregno])
	{
	  coalesced_moves++;
	  if (lra_dump_file != NULL)
	    fprintf
	      (lra_dump_file, "      Coalescing move %i:r%d-r%d (freq=%d)\n",
	       INSN_UID (mv), sregno, dregno,
	       REG_FREQ_FROM_BB (BLOCK_FOR_INSN (mv)));
	  /* We updated involved_insns_bitmap when doing the merge.  */
	}
      else if (!(lra_intersected_live_ranges_p
		 (lra_reg_info[first_coalesced_pseudo[sregno]].live_ranges,
		  lra_reg_info[first_coalesced_pseudo[dregno]].live_ranges)))
	{
	  coalesced_moves++;
	  if (lra_dump_file != NULL)
	    fprintf
	      (lra_dump_file,
	       "  Coalescing move %i:r%d(%d)-r%d(%d) (freq=%d)\n",
	       INSN_UID (mv), sregno, ORIGINAL_REGNO (SET_SRC (set)),
	       dregno, ORIGINAL_REGNO (SET_DEST (set)),
	       REG_FREQ_FROM_BB (BLOCK_FOR_INSN (mv)));
	  bitmap_ior_into (&involved_insns_bitmap,
			   &lra_reg_info[sregno].insn_bitmap);
	  bitmap_ior_into (&involved_insns_bitmap,
			   &lra_reg_info[dregno].insn_bitmap);
	  merge_pseudos (sregno, dregno);
	}
    }
  bitmap_initialize (&used_pseudos_bitmap, &reg_obstack);
  FOR_EACH_BB_FN (bb, cfun)
    {
      update_live_info (df_get_live_in (bb));
      update_live_info (df_get_live_out (bb));
      FOR_BB_INSNS_SAFE (bb, insn, next)
	if (INSN_P (insn)
	    && bitmap_bit_p (&involved_insns_bitmap, INSN_UID (insn)))
	  {
	    if (! substitute (&insn))
	      continue;
	    lra_update_insn_regno_info (insn);
	    if ((set = single_set (insn)) != NULL_RTX && set_noop_p (set))
	      {
		/* Coalesced move.  */
		if (lra_dump_file != NULL)
		  fprintf (lra_dump_file, "	 Removing move %i (freq=%d)\n",
			   INSN_UID (insn),
			   REG_FREQ_FROM_BB (BLOCK_FOR_INSN (insn)));
		lra_set_insn_deleted (insn);
	      }
	  }
    }
  /* If we have situation after inheritance pass:

     r1 <- ...  insn originally setting p1
     i1 <- r1   setting inheritance i1 from reload r1
       ...
     ... <- ... p2 ... dead p2
     ..
     p1 <- i1
     r2 <- i1
     ...<- ... r2 ...

     And we are coalescing p1 and p2 using p1.  In this case i1 and p1
     should have different values, otherwise they can get the same
     hard reg and this is wrong for insn using p2 before coalescing.
     So invalidate such inheritance pseudo values.  */
  bitmap_initialize (&result_pseudo_vals_bitmap, &reg_obstack);
  EXECUTE_IF_SET_IN_BITMAP (&coalesced_pseudos_bitmap, 0, regno, bi)
    bitmap_set_bit (&result_pseudo_vals_bitmap,
		    lra_reg_info[first_coalesced_pseudo[regno]].val);
  EXECUTE_IF_SET_IN_BITMAP (&lra_inheritance_pseudos, 0, regno, bi)
    if (bitmap_bit_p (&result_pseudo_vals_bitmap, lra_reg_info[regno].val))
      {
	lra_set_regno_unique_value (regno);
	if (lra_dump_file != NULL)
	  fprintf (lra_dump_file,
		   "	 Make unique value for inheritance r%d\n", regno);
      }
  bitmap_clear (&result_pseudo_vals_bitmap);
  bitmap_clear (&used_pseudos_bitmap);
  bitmap_clear (&involved_insns_bitmap);
  bitmap_clear (&coalesced_pseudos_bitmap);
  if (lra_dump_file != NULL && coalesced_moves != 0)
    fprintf (lra_dump_file, "Coalesced Moves = %d\n", coalesced_moves);
  free (sorted_moves);
  free (next_coalesced_pseudo);
  free (first_coalesced_pseudo);
  timevar_pop (TV_LRA_COALESCE);
  return coalesced_moves != 0;
}
