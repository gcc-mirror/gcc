/* Build live ranges for pseudos.
   Copyright (C) 2010-2019 Free Software Foundation, Inc.
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


/* This file contains code to build pseudo live-ranges (analogous
   structures used in IRA, so read comments about the live-ranges
   there) and other info necessary for other passes to assign
   hard-registers to pseudos, coalesce the spilled pseudos, and assign
   stack memory slots to spilled pseudos.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "ira.h"
#include "recog.h"
#include "cfganal.h"
#include "sparseset.h"
#include "lra-int.h"
#include "target.h"
#include "function-abi.h"

/* Program points are enumerated by numbers from range
   0..LRA_LIVE_MAX_POINT-1.  There are approximately two times more
   program points than insns.  Program points are places in the
   program where liveness info can be changed.	In most general case
   (there are more complicated cases too) some program points
   correspond to places where input operand dies and other ones
   correspond to places where output operands are born.	 */
int lra_live_max_point;

/* Accumulated execution frequency of all references for each hard
   register.  */
int lra_hard_reg_usage[FIRST_PSEUDO_REGISTER];

/* A global flag whose true value says to build live ranges for all
   pseudos, otherwise the live ranges only for pseudos got memory is
   build.  True value means also building copies and setting up hard
   register preferences.  The complete info is necessary only for the
   assignment pass.  The complete info is not needed for the
   coalescing and spill passes.	 */
static bool complete_info_p;

/* Pseudos live at current point in the RTL scan.  */
static sparseset pseudos_live;

/* Pseudos probably living through calls and setjumps.	As setjump is
   a call too, if a bit in PSEUDOS_LIVE_THROUGH_SETJUMPS is set up
   then the corresponding bit in PSEUDOS_LIVE_THROUGH_CALLS is set up
   too.	 These data are necessary for cases when only one subreg of a
   multi-reg pseudo is set up after a call.  So we decide it is
   probably live when traversing bb backward.  We are sure about
   living when we see its usage or definition of the pseudo.  */
static sparseset pseudos_live_through_calls;
static sparseset pseudos_live_through_setjumps;

/* Set of hard regs (except eliminable ones) currently live.  */
static HARD_REG_SET hard_regs_live;

/* Set of pseudos and hard registers start living/dying in the current
   insn.  These sets are used to update REG_DEAD and REG_UNUSED notes
   in the insn.  */
static sparseset start_living, start_dying;

/* Set of pseudos and hard regs dead and unused in the current
   insn.  */
static sparseset unused_set, dead_set;

/* Bitmap used for holding intermediate bitmap operation results.  */
static bitmap_head temp_bitmap;

/* Pool for pseudo live ranges.	 */
static object_allocator<lra_live_range> lra_live_range_pool ("live ranges");

/* Free live range list LR.  */
static void
free_live_range_list (lra_live_range_t lr)
{
  lra_live_range_t next;

  while (lr != NULL)
    {
      next = lr->next;
      lra_live_range_pool.remove (lr);
      lr = next;
    }
}

/* Create and return pseudo live range with given attributes.  */
static lra_live_range_t
create_live_range (int regno, int start, int finish, lra_live_range_t next)
{
  lra_live_range_t p = lra_live_range_pool.allocate ();
  p->regno = regno;
  p->start = start;
  p->finish = finish;
  p->next = next;
  return p;
}

/* Copy live range R and return the result.  */
static lra_live_range_t
copy_live_range (lra_live_range_t r)
{
  return new (lra_live_range_pool) lra_live_range (*r);
}

/* Copy live range list given by its head R and return the result.  */
lra_live_range_t
lra_copy_live_range_list (lra_live_range_t r)
{
  lra_live_range_t p, first, *chain;

  first = NULL;
  for (chain = &first; r != NULL; r = r->next)
    {
      p = copy_live_range (r);
      *chain = p;
      chain = &p->next;
    }
  return first;
}

/* Merge *non-intersected* ranges R1 and R2 and returns the result.
   The function maintains the order of ranges and tries to minimize
   size of the result range list.  Ranges R1 and R2 may not be used
   after the call.  */
lra_live_range_t
lra_merge_live_ranges (lra_live_range_t r1, lra_live_range_t r2)
{
  lra_live_range_t first, last;

  if (r1 == NULL)
    return r2;
  if (r2 == NULL)
    return r1;
  for (first = last = NULL; r1 != NULL && r2 != NULL;)
    {
      if (r1->start < r2->start)
	std::swap (r1, r2);

      if (r1->start == r2->finish + 1)
	{
	  /* Joint ranges: merge r1 and r2 into r1.  */
	  r1->start = r2->start;
	  lra_live_range_t temp = r2;
	  r2 = r2->next;
	  lra_live_range_pool.remove (temp);
	}
      else
	{
	  gcc_assert (r2->finish + 1 < r1->start);
	  /* Add r1 to the result.  */
	  if (first == NULL)
	    first = last = r1;
	  else
	    {
	      last->next = r1;
	      last = r1;
	    }
	  r1 = r1->next;
	}
    }
  if (r1 != NULL)
    {
      if (first == NULL)
	first = r1;
      else
	last->next = r1;
    }
  else
    {
      lra_assert (r2 != NULL);
      if (first == NULL)
	first = r2;
      else
	last->next = r2;
    }
  return first;
}

/* Return TRUE if live ranges R1 and R2 intersect.  */
bool
lra_intersected_live_ranges_p (lra_live_range_t r1, lra_live_range_t r2)
{
  /* Remember the live ranges are always kept ordered.	*/
  while (r1 != NULL && r2 != NULL)
    {
      if (r1->start > r2->finish)
	r1 = r1->next;
      else if (r2->start > r1->finish)
	r2 = r2->next;
      else
	return true;
    }
  return false;
}

enum point_type {
  DEF_POINT,
  USE_POINT
};

/* Return TRUE if set A contains a pseudo register, otherwise, return FALSE.  */
static bool
sparseset_contains_pseudos_p (sparseset a)
{
  int regno;
  EXECUTE_IF_SET_IN_SPARSESET (a, regno)
    if (!HARD_REGISTER_NUM_P (regno))
      return true;
  return false;
}

/* Mark pseudo REGNO as living or dying at program point POINT, depending on
   whether TYPE is a definition or a use.  If this is the first reference to
   REGNO that we've encountered, then create a new live range for it.  */

static void
update_pseudo_point (int regno, int point, enum point_type type)
{
  lra_live_range_t p;

  /* Don't compute points for hard registers.  */
  if (HARD_REGISTER_NUM_P (regno))
    return;

  if (complete_info_p || lra_get_regno_hard_regno (regno) < 0)
    {
      if (type == DEF_POINT)
	{
	  if (sparseset_bit_p (pseudos_live, regno))
	    {
	      p = lra_reg_info[regno].live_ranges;
	      lra_assert (p != NULL);
	      p->finish = point;
	    }
	}
      else /* USE_POINT */
	{
	  if (!sparseset_bit_p (pseudos_live, regno)
	      && ((p = lra_reg_info[regno].live_ranges) == NULL
		  || (p->finish != point && p->finish + 1 != point)))
	    lra_reg_info[regno].live_ranges
	      = create_live_range (regno, point, -1, p);
	}
    }
}

/* The corresponding bitmaps of BB currently being processed.  */
static bitmap bb_killed_pseudos, bb_gen_pseudos;

/* Record hard register REGNO as now being live.  It updates
   living hard regs and START_LIVING.  */
static void
make_hard_regno_live (int regno)
{
  lra_assert (HARD_REGISTER_NUM_P (regno));
  if (TEST_HARD_REG_BIT (hard_regs_live, regno)
      || TEST_HARD_REG_BIT (eliminable_regset, regno))
    return;
  SET_HARD_REG_BIT (hard_regs_live, regno);
  sparseset_set_bit (start_living, regno);
  if (fixed_regs[regno] || TEST_HARD_REG_BIT (hard_regs_spilled_into, regno))
    bitmap_set_bit (bb_gen_pseudos, regno);
}

/* Process the definition of hard register REGNO.  This updates
   hard_regs_live, START_DYING and conflict hard regs for living
   pseudos.  */
static void
make_hard_regno_dead (int regno)
{
  if (TEST_HARD_REG_BIT (eliminable_regset, regno))
    return;

  lra_assert (HARD_REGISTER_NUM_P (regno));
  unsigned int i;
  EXECUTE_IF_SET_IN_SPARSESET (pseudos_live, i)
    SET_HARD_REG_BIT (lra_reg_info[i].conflict_hard_regs, regno);

  if (! TEST_HARD_REG_BIT (hard_regs_live, regno))
    return;
  CLEAR_HARD_REG_BIT (hard_regs_live, regno);
  sparseset_set_bit (start_dying, regno);
  if (fixed_regs[regno] || TEST_HARD_REG_BIT (hard_regs_spilled_into, regno))
    {
      bitmap_clear_bit (bb_gen_pseudos, regno);
      bitmap_set_bit (bb_killed_pseudos, regno);
    }
}

/* Mark pseudo REGNO as now being live and update START_LIVING.  */
static void
mark_pseudo_live (int regno)
{
  lra_assert (!HARD_REGISTER_NUM_P (regno));
  if (sparseset_bit_p (pseudos_live, regno))
    return;

  sparseset_set_bit (pseudos_live, regno);
  sparseset_set_bit (start_living, regno);
}

/* Mark pseudo REGNO as now being dead and update START_DYING.  */
static void
mark_pseudo_dead (int regno)
{
  lra_assert (!HARD_REGISTER_NUM_P (regno));
  lra_reg_info[regno].conflict_hard_regs |= hard_regs_live;
  if (!sparseset_bit_p (pseudos_live, regno))
    return;

  sparseset_clear_bit (pseudos_live, regno);
  sparseset_set_bit (start_dying, regno);
}

/* Mark register REGNO (pseudo or hard register) in MODE as being live
   and update BB_GEN_PSEUDOS.  */
static void
mark_regno_live (int regno, machine_mode mode)
{
  int last;

  if (HARD_REGISTER_NUM_P (regno))
    {
      for (last = end_hard_regno (mode, regno); regno < last; regno++)
	make_hard_regno_live (regno);
    }
  else
    {
      mark_pseudo_live (regno);
      bitmap_set_bit (bb_gen_pseudos, regno);
    }
}


/* Mark register REGNO (pseudo or hard register) in MODE as being dead
   and update BB_GEN_PSEUDOS and BB_KILLED_PSEUDOS.  */
static void
mark_regno_dead (int regno, machine_mode mode)
{
  int last;

  if (HARD_REGISTER_NUM_P (regno))
    {
      for (last = end_hard_regno (mode, regno); regno < last; regno++)
	make_hard_regno_dead (regno);
    }
  else
    {
      mark_pseudo_dead (regno);
      bitmap_clear_bit (bb_gen_pseudos, regno);
      bitmap_set_bit (bb_killed_pseudos, regno);
    }
}



/* This page contains code for making global live analysis of pseudos.
   The code works only when pseudo live info is changed on a BB
   border.  That might be a consequence of some global transformations
   in LRA, e.g. PIC pseudo reuse or rematerialization.  */

/* Structure describing local BB data used for pseudo
   live-analysis.  */
class bb_data_pseudos
{
public:
  /* Basic block about which the below data are.  */
  basic_block bb;
  bitmap_head killed_pseudos; /* pseudos killed in the BB.  */
  bitmap_head gen_pseudos; /* pseudos generated in the BB.  */
};

/* Array for all BB data.  Indexed by the corresponding BB index.  */
typedef class bb_data_pseudos *bb_data_t;

/* All basic block data are referred through the following array.  */
static bb_data_t bb_data;

/* Two small functions for access to the bb data.  */
static inline bb_data_t
get_bb_data (basic_block bb)
{
  return &bb_data[(bb)->index];
}

static inline bb_data_t
get_bb_data_by_index (int index)
{
  return &bb_data[index];
}

/* Bitmap with all hard regs.  */
static bitmap_head all_hard_regs_bitmap;

/* The transfer function used by the DF equation solver to propagate
   live info through block with BB_INDEX according to the following
   equation:

     bb.livein = (bb.liveout - bb.kill) OR bb.gen
*/
static bool
live_trans_fun (int bb_index)
{
  basic_block bb = get_bb_data_by_index (bb_index)->bb;
  bitmap bb_liveout = df_get_live_out (bb);
  bitmap bb_livein = df_get_live_in (bb);
  bb_data_t bb_info = get_bb_data (bb);

  bitmap_and_compl (&temp_bitmap, bb_liveout, &all_hard_regs_bitmap);
  return bitmap_ior_and_compl (bb_livein, &bb_info->gen_pseudos,
			       &temp_bitmap, &bb_info->killed_pseudos);
}

/* The confluence function used by the DF equation solver to set up
   live info for a block BB without predecessor.  */
static void
live_con_fun_0 (basic_block bb)
{
  bitmap_and_into (df_get_live_out (bb), &all_hard_regs_bitmap);
}

/* The confluence function used by the DF equation solver to propagate
   live info from successor to predecessor on edge E according to the
   following equation:

      bb.liveout = 0 for entry block | OR (livein of successors)
 */
static bool
live_con_fun_n (edge e)
{
  basic_block bb = e->src;
  basic_block dest = e->dest;
  bitmap bb_liveout = df_get_live_out (bb);
  bitmap dest_livein = df_get_live_in (dest);

  return bitmap_ior_and_compl_into (bb_liveout,
				    dest_livein, &all_hard_regs_bitmap);
}

/* Indexes of all function blocks.  */
static bitmap_head all_blocks;

/* Allocate and initialize data needed for global pseudo live
   analysis.  */
static void
initiate_live_solver (void)
{
  bitmap_initialize (&all_hard_regs_bitmap, &reg_obstack);
  bitmap_set_range (&all_hard_regs_bitmap, 0, FIRST_PSEUDO_REGISTER);
  bb_data = XNEWVEC (class bb_data_pseudos, last_basic_block_for_fn (cfun));
  bitmap_initialize (&all_blocks, &reg_obstack);

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    {
      bb_data_t bb_info = get_bb_data (bb);
      bb_info->bb = bb;
      bitmap_initialize (&bb_info->killed_pseudos, &reg_obstack);
      bitmap_initialize (&bb_info->gen_pseudos, &reg_obstack);
      bitmap_set_bit (&all_blocks, bb->index);
    }
}

/* Free all data needed for global pseudo live analysis.  */
static void
finish_live_solver (void)
{
  basic_block bb;

  bitmap_clear (&all_blocks);
  FOR_ALL_BB_FN (bb, cfun)
    {
      bb_data_t bb_info = get_bb_data (bb);
      bitmap_clear (&bb_info->killed_pseudos);
      bitmap_clear (&bb_info->gen_pseudos);
    }
  free (bb_data);
  bitmap_clear (&all_hard_regs_bitmap);
}



/* Insn currently scanned.  */
static rtx_insn *curr_insn;
/* The insn data.  */
static lra_insn_recog_data_t curr_id;
/* The insn static data.  */
static struct lra_static_insn_data *curr_static_id;

/* Vec containing execution frequencies of program points.  */
static vec<int> point_freq_vec;

/* The start of the above vector elements.  */
int *lra_point_freq;

/* Increment the current program point POINT to the next point which has
   execution frequency FREQ.  */
static void
next_program_point (int &point, int freq)
{
  point_freq_vec.safe_push (freq);
  lra_point_freq = point_freq_vec.address ();
  point++;
}

/* Update the preference of HARD_REGNO for pseudo REGNO by PROFIT.  */
void
lra_setup_reload_pseudo_preferenced_hard_reg (int regno,
					      int hard_regno, int profit)
{
  lra_assert (regno >= lra_constraint_new_regno_start);
  if (lra_reg_info[regno].preferred_hard_regno1 == hard_regno)
    lra_reg_info[regno].preferred_hard_regno_profit1 += profit;
  else if (lra_reg_info[regno].preferred_hard_regno2 == hard_regno)
    lra_reg_info[regno].preferred_hard_regno_profit2 += profit;
  else if (lra_reg_info[regno].preferred_hard_regno1 < 0)
    {
      lra_reg_info[regno].preferred_hard_regno1 = hard_regno;
      lra_reg_info[regno].preferred_hard_regno_profit1 = profit;
    }
  else if (lra_reg_info[regno].preferred_hard_regno2 < 0
	   || profit > lra_reg_info[regno].preferred_hard_regno_profit2)
    {
      lra_reg_info[regno].preferred_hard_regno2 = hard_regno;
      lra_reg_info[regno].preferred_hard_regno_profit2 = profit;
    }
  else
    return;
  /* Keep the 1st hard regno as more profitable.  */
  if (lra_reg_info[regno].preferred_hard_regno1 >= 0
      && lra_reg_info[regno].preferred_hard_regno2 >= 0
      && (lra_reg_info[regno].preferred_hard_regno_profit2
	  > lra_reg_info[regno].preferred_hard_regno_profit1))
    {
      std::swap (lra_reg_info[regno].preferred_hard_regno1,
		 lra_reg_info[regno].preferred_hard_regno2);
      std::swap (lra_reg_info[regno].preferred_hard_regno_profit1,
		 lra_reg_info[regno].preferred_hard_regno_profit2);
    }
  if (lra_dump_file != NULL)
    {
      if ((hard_regno = lra_reg_info[regno].preferred_hard_regno1) >= 0)
	fprintf (lra_dump_file,
		 "	Hard reg %d is preferable by r%d with profit %d\n",
		 hard_regno, regno,
		 lra_reg_info[regno].preferred_hard_regno_profit1);
      if ((hard_regno = lra_reg_info[regno].preferred_hard_regno2) >= 0)
	fprintf (lra_dump_file,
		 "	Hard reg %d is preferable by r%d with profit %d\n",
		 hard_regno, regno,
		 lra_reg_info[regno].preferred_hard_regno_profit2);
    }
}

/* Check whether REGNO lives through calls and setjmps and clear
   the corresponding bits in PSEUDOS_LIVE_THROUGH_CALLS and
   PSEUDOS_LIVE_THROUGH_SETJUMPS.  All calls in the region described
   by PSEUDOS_LIVE_THROUGH_CALLS have the given ABI.  */

static inline void
check_pseudos_live_through_calls (int regno, const function_abi &abi)
{
  if (! sparseset_bit_p (pseudos_live_through_calls, regno))
    return;

  machine_mode mode = PSEUDO_REGNO_MODE (regno);

  sparseset_clear_bit (pseudos_live_through_calls, regno);
  lra_reg_info[regno].conflict_hard_regs |= abi.mode_clobbers (mode);
  if (! sparseset_bit_p (pseudos_live_through_setjumps, regno))
    return;
  sparseset_clear_bit (pseudos_live_through_setjumps, regno);
  /* Don't allocate pseudos that cross setjmps or any call, if this
     function receives a nonlocal goto.	 */
  SET_HARD_REG_SET (lra_reg_info[regno].conflict_hard_regs);
}

/* Return true if insn REG is an early clobber operand in alternative
   NALT.  Negative NALT means that we don't know the current insn
   alternative.  So assume the worst.  */
static inline bool
reg_early_clobber_p (const struct lra_insn_reg *reg, int n_alt)
{
  return (n_alt == LRA_UNKNOWN_ALT
	  ? reg->early_clobber_alts != 0
	  : (n_alt != LRA_NON_CLOBBERED_ALT
	     && TEST_BIT (reg->early_clobber_alts, n_alt)));
}

/* Process insns of the basic block BB to update pseudo live ranges,
   pseudo hard register conflicts, and insn notes.  We do it on
   backward scan of BB insns.  CURR_POINT is the program point where
   BB ends.  The function updates this counter and returns in
   CURR_POINT the program point where BB starts.  The function also
   does local live info updates and can delete the dead insns if
   DEAD_INSN_P.  It returns true if pseudo live info was
   changed at the BB start.  */
static bool
process_bb_lives (basic_block bb, int &curr_point, bool dead_insn_p)
{
  int i, regno, freq;
  unsigned int j;
  bitmap_iterator bi;
  bitmap reg_live_out;
  unsigned int px;
  rtx_insn *next;
  rtx link, *link_loc;
  bool need_curr_point_incr;
  /* Only has a meaningful value once we've seen a call.  */
  function_abi last_call_abi = default_function_abi;

  reg_live_out = df_get_live_out (bb);
  sparseset_clear (pseudos_live);
  sparseset_clear (pseudos_live_through_calls);
  sparseset_clear (pseudos_live_through_setjumps);
  REG_SET_TO_HARD_REG_SET (hard_regs_live, reg_live_out);
  hard_regs_live &= ~eliminable_regset;
  EXECUTE_IF_SET_IN_BITMAP (reg_live_out, FIRST_PSEUDO_REGISTER, j, bi)
    {
      update_pseudo_point (j, curr_point, USE_POINT);
      mark_pseudo_live (j);
    }

  bb_gen_pseudos = &get_bb_data (bb)->gen_pseudos;
  bb_killed_pseudos = &get_bb_data (bb)->killed_pseudos;
  bitmap_clear (bb_gen_pseudos);
  bitmap_clear (bb_killed_pseudos);
  freq = REG_FREQ_FROM_BB (bb);

  if (lra_dump_file != NULL)
    fprintf (lra_dump_file, "  BB %d\n", bb->index);

  /* Scan the code of this basic block, noting which pseudos and hard
     regs are born or die.

     Note that this loop treats uninitialized values as live until the
     beginning of the block.  For example, if an instruction uses
     (reg:DI foo), and only (subreg:SI (reg:DI foo) 0) is ever set,
     FOO will remain live until the beginning of the block.  Likewise
     if FOO is not set at all.	This is unnecessarily pessimistic, but
     it probably doesn't matter much in practice.  */
  FOR_BB_INSNS_REVERSE_SAFE (bb, curr_insn, next)
    {
      bool call_p;
      int n_alt, dst_regno, src_regno;
      rtx set;
      struct lra_insn_reg *reg;

      if (!NONDEBUG_INSN_P (curr_insn))
	continue;

      curr_id = lra_get_insn_recog_data (curr_insn);
      curr_static_id = curr_id->insn_static_data;
      n_alt = curr_id->used_insn_alternative;
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "   Insn %u: point = %d, n_alt = %d\n",
		 INSN_UID (curr_insn), curr_point, n_alt);

      set = single_set (curr_insn);

      if (dead_insn_p && set != NULL_RTX
	  && REG_P (SET_DEST (set)) && !HARD_REGISTER_P (SET_DEST (set))
	  && find_reg_note (curr_insn, REG_EH_REGION, NULL_RTX) == NULL_RTX
	  && ! may_trap_p (PATTERN (curr_insn))
	  /* Don't do premature remove of pic offset pseudo as we can
	     start to use it after some reload generation.  */
	  && (pic_offset_table_rtx == NULL_RTX
	      || pic_offset_table_rtx != SET_DEST (set)))
	{
	  bool remove_p = true;

	  for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	    if (reg->type != OP_IN && sparseset_bit_p (pseudos_live, reg->regno))
	      {
		remove_p = false;
		break;
	      }
	  for (reg = curr_static_id->hard_regs; reg != NULL; reg = reg->next)
	    if (reg->type != OP_IN)
	      {
		remove_p = false;
		break;
	      }

	  if (remove_p && ! volatile_refs_p (PATTERN (curr_insn)))
	    {
	      dst_regno = REGNO (SET_DEST (set));
	      if (lra_dump_file != NULL)
		fprintf (lra_dump_file, "   Deleting dead insn %u\n",
			 INSN_UID (curr_insn));
	      lra_set_insn_deleted (curr_insn);
	      if (lra_reg_info[dst_regno].nrefs == 0)
		{
		  /* There might be some debug insns with the pseudo.  */
		  unsigned int uid;
		  rtx_insn *insn;

		  bitmap_copy (&temp_bitmap, &lra_reg_info[dst_regno].insn_bitmap);
		  EXECUTE_IF_SET_IN_BITMAP (&temp_bitmap, 0, uid, bi)
		    {
		      insn = lra_insn_recog_data[uid]->insn;
		      lra_substitute_pseudo_within_insn (insn, dst_regno,
							 SET_SRC (set), true);
		      lra_update_insn_regno_info (insn);
		    }
		}
	      continue;
	    }
	}

      /* Update max ref width and hard reg usage.  */
      for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	{
	  int i, regno = reg->regno;

	  if (partial_subreg_p (lra_reg_info[regno].biggest_mode,
				reg->biggest_mode))
	    lra_reg_info[regno].biggest_mode = reg->biggest_mode;
	  if (HARD_REGISTER_NUM_P (regno))
	    {
	      lra_hard_reg_usage[regno] += freq;
	      /* A hard register explicitly can be used in small mode,
		 but implicitly it can be used in natural mode as a
		 part of multi-register group.  Process this case
		 here.  */
	      for (i = 1; i < hard_regno_nregs (regno, reg->biggest_mode); i++)
		if (partial_subreg_p (lra_reg_info[regno + i].biggest_mode,
				      GET_MODE (regno_reg_rtx[regno + i])))
		  lra_reg_info[regno + i].biggest_mode
		    = GET_MODE (regno_reg_rtx[regno + i]);
	    }
	}

      call_p = CALL_P (curr_insn);

      /* If we have a simple register copy and the source reg is live after
	 this instruction, then remove the source reg from the live set so
	 that it will not conflict with the destination reg.  */
      rtx ignore_reg = non_conflicting_reg_copy_p (curr_insn);
      if (ignore_reg != NULL_RTX)
	{
	  int ignore_regno = REGNO (ignore_reg);
	  if (HARD_REGISTER_NUM_P (ignore_regno)
	      && TEST_HARD_REG_BIT (hard_regs_live, ignore_regno))
	    CLEAR_HARD_REG_BIT (hard_regs_live, ignore_regno);
	  else if (!HARD_REGISTER_NUM_P (ignore_regno)
		   && sparseset_bit_p (pseudos_live, ignore_regno))
	    sparseset_clear_bit (pseudos_live, ignore_regno);
	  else
	    /* We don't need any special handling of the source reg if
	       it is dead after this instruction.  */
	    ignore_reg = NULL_RTX;
	}

      src_regno = (set != NULL_RTX && REG_P (SET_SRC (set))
		   ? REGNO (SET_SRC (set)) : -1);
      dst_regno = (set != NULL_RTX && REG_P (SET_DEST (set))
		   ? REGNO (SET_DEST (set)) : -1);
      if (complete_info_p
	  && src_regno >= 0 && dst_regno >= 0
	  /* Check that source regno does not conflict with
	     destination regno to exclude most impossible
	     preferences.  */
	  && (((!HARD_REGISTER_NUM_P (src_regno)
		&& (! sparseset_bit_p (pseudos_live, src_regno)
		    || (!HARD_REGISTER_NUM_P (dst_regno)
			&& lra_reg_val_equal_p (src_regno,
						lra_reg_info[dst_regno].val,
						lra_reg_info[dst_regno].offset))))
	       || (HARD_REGISTER_NUM_P (src_regno)
		   && ! TEST_HARD_REG_BIT (hard_regs_live, src_regno)))
	      /* It might be 'inheritance pseudo <- reload pseudo'.  */
	      || (src_regno >= lra_constraint_new_regno_start
		  && dst_regno >= lra_constraint_new_regno_start
		  /* Remember to skip special cases where src/dest regnos are
		     the same, e.g. insn SET pattern has matching constraints
		     like =r,0.  */
		  && src_regno != dst_regno)))
	{
	  int hard_regno = -1, regno = -1;

	  if (dst_regno >= lra_constraint_new_regno_start
	      && src_regno >= lra_constraint_new_regno_start)
	    {
	      /* It might be still an original (non-reload) insn with
		 one unused output and a constraint requiring to use
		 the same reg for input/output operands. In this case
		 dst_regno and src_regno have the same value, we don't
		 need a misleading copy for this case.  */
	      if (dst_regno != src_regno)
		lra_create_copy (dst_regno, src_regno, freq);
	    }
	  else if (dst_regno >= lra_constraint_new_regno_start)
	    {
	      if (!HARD_REGISTER_NUM_P (hard_regno = src_regno))
		hard_regno = reg_renumber[src_regno];
	      regno = dst_regno;
	    }
	  else if (src_regno >= lra_constraint_new_regno_start)
	    {
	      if (!HARD_REGISTER_NUM_P (hard_regno = dst_regno))
		hard_regno = reg_renumber[dst_regno];
	      regno = src_regno;
	    }
	  if (regno >= 0 && hard_regno >= 0)
	    lra_setup_reload_pseudo_preferenced_hard_reg
	      (regno, hard_regno, freq);
	}

      sparseset_clear (start_living);

      /* Mark each defined value as live.  We need to do this for
	 unused values because they still conflict with quantities
	 that are live at the time of the definition.  */
      for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_IN)
	  {
	    update_pseudo_point (reg->regno, curr_point, USE_POINT);
	    mark_regno_live (reg->regno, reg->biggest_mode);
	    /* ??? Should be a no-op for unused registers.  */
	    check_pseudos_live_through_calls (reg->regno, last_call_abi);
	  }

      for (reg = curr_static_id->hard_regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_IN)
	  make_hard_regno_live (reg->regno);

      if (curr_id->arg_hard_regs != NULL)
	for (i = 0; (regno = curr_id->arg_hard_regs[i]) >= 0; i++)
	  if (!HARD_REGISTER_NUM_P (regno))
	    /* It is a clobber.  */
	    make_hard_regno_live (regno - FIRST_PSEUDO_REGISTER);

      sparseset_copy (unused_set, start_living);

      sparseset_clear (start_dying);

      /* See which defined values die here.  */
      for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_IN
	    && ! reg_early_clobber_p (reg, n_alt) && ! reg->subreg_p)
	  {
	    if (reg->type == OP_OUT)
	      update_pseudo_point (reg->regno, curr_point, DEF_POINT);
	    mark_regno_dead (reg->regno, reg->biggest_mode);
	  }

      for (reg = curr_static_id->hard_regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_IN
	    && ! reg_early_clobber_p (reg, n_alt) && ! reg->subreg_p)
	  make_hard_regno_dead (reg->regno);

      if (curr_id->arg_hard_regs != NULL)
	for (i = 0; (regno = curr_id->arg_hard_regs[i]) >= 0; i++)
	  if (!HARD_REGISTER_NUM_P (regno))
	    /* It is a clobber.  */
	    make_hard_regno_dead (regno - FIRST_PSEUDO_REGISTER);

      if (call_p)
	{
	  function_abi call_abi = insn_callee_abi (curr_insn);

	  if (last_call_abi != call_abi)
	    EXECUTE_IF_SET_IN_SPARSESET (pseudos_live, j)
	      check_pseudos_live_through_calls (j, last_call_abi);

	  last_call_abi = call_abi;

	  sparseset_ior (pseudos_live_through_calls,
			 pseudos_live_through_calls, pseudos_live);
	  if (cfun->has_nonlocal_label
	      || (!targetm.setjmp_preserves_nonvolatile_regs_p ()
		  && (find_reg_note (curr_insn, REG_SETJMP, NULL_RTX)
		      != NULL_RTX)))
	    sparseset_ior (pseudos_live_through_setjumps,
			   pseudos_live_through_setjumps, pseudos_live);
	}

      /* Increment the current program point if we must.  */
      if (sparseset_contains_pseudos_p (unused_set)
	  || sparseset_contains_pseudos_p (start_dying))
	next_program_point (curr_point, freq);

      /* If we removed the source reg from a simple register copy from the
	 live set above, then add it back now so we don't accidentally add
	 it to the start_living set below.  */
      if (ignore_reg != NULL_RTX)
	{
	  int ignore_regno = REGNO (ignore_reg);
	  if (HARD_REGISTER_NUM_P (ignore_regno))
	    SET_HARD_REG_BIT (hard_regs_live, ignore_regno);
	  else
	    sparseset_set_bit (pseudos_live, ignore_regno);
	}

      sparseset_clear (start_living);

      /* Mark each used value as live.	*/
      for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_OUT)
	  {
	    if (reg->type == OP_IN)
	      update_pseudo_point (reg->regno, curr_point, USE_POINT);
	    mark_regno_live (reg->regno, reg->biggest_mode);
	    check_pseudos_live_through_calls (reg->regno, last_call_abi);
	  }

      for (reg = curr_static_id->hard_regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_OUT)
	  make_hard_regno_live (reg->regno);

      if (curr_id->arg_hard_regs != NULL)
	/* Make argument hard registers live.  */
	for (i = 0; (regno = curr_id->arg_hard_regs[i]) >= 0; i++)
	  if (HARD_REGISTER_NUM_P (regno))
	    make_hard_regno_live (regno);

      sparseset_and_compl (dead_set, start_living, start_dying);

      sparseset_clear (start_dying);

      /* Mark early clobber outputs dead.  */
      for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_IN
	    && reg_early_clobber_p (reg, n_alt) && ! reg->subreg_p)
	  {
	    if (reg->type == OP_OUT)
	      update_pseudo_point (reg->regno, curr_point, DEF_POINT);
	    mark_regno_dead (reg->regno, reg->biggest_mode);

	    /* We're done processing inputs, so make sure early clobber
	       operands that are both inputs and outputs are still live.  */
	    if (reg->type == OP_INOUT)
	      mark_regno_live (reg->regno, reg->biggest_mode);
	  }

      for (reg = curr_static_id->hard_regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_IN
	    && reg_early_clobber_p (reg, n_alt) && ! reg->subreg_p)
	  {
	    struct lra_insn_reg *reg2;

	    /* We can have early clobbered non-operand hard reg and
	       the same hard reg as an insn input.  Don't make hard
	       reg dead before the insns.  */
	    for (reg2 = curr_id->regs; reg2 != NULL; reg2 = reg2->next)
	      if (reg2->type != OP_OUT && reg2->regno == reg->regno)
		break;
	    if (reg2 == NULL)
	      make_hard_regno_dead (reg->regno);
	  }

      /* Increment the current program point if we must.  */
      if (sparseset_contains_pseudos_p (dead_set)
	  || sparseset_contains_pseudos_p (start_dying))
	next_program_point (curr_point, freq);

      /* Update notes.	*/
      for (link_loc = &REG_NOTES (curr_insn); (link = *link_loc) != NULL_RTX;)
	{
	  if (REG_NOTE_KIND (link) != REG_DEAD
	      && REG_NOTE_KIND (link) != REG_UNUSED)
	    ;
	  else if (REG_P (XEXP (link, 0)))
	    {
	      regno = REGNO (XEXP (link, 0));
	      if ((REG_NOTE_KIND (link) == REG_DEAD
		   && ! sparseset_bit_p (dead_set, regno))
		  || (REG_NOTE_KIND (link) == REG_UNUSED
		      && ! sparseset_bit_p (unused_set, regno)))
		{
		  *link_loc = XEXP (link, 1);
		  continue;
		}
	      if (REG_NOTE_KIND (link) == REG_DEAD)
		sparseset_clear_bit (dead_set, regno);
	      else if (REG_NOTE_KIND (link) == REG_UNUSED)
		sparseset_clear_bit (unused_set, regno);
	    }
	  link_loc = &XEXP (link, 1);
	}
      EXECUTE_IF_SET_IN_SPARSESET (dead_set, j)
	add_reg_note (curr_insn, REG_DEAD, regno_reg_rtx[j]);
      EXECUTE_IF_SET_IN_SPARSESET (unused_set, j)
	add_reg_note (curr_insn, REG_UNUSED, regno_reg_rtx[j]);
    }

  if (bb_has_eh_pred (bb))
    for (j = 0; ; ++j)
      {
	unsigned int regno = EH_RETURN_DATA_REGNO (j);

	if (regno == INVALID_REGNUM)
	  break;
	make_hard_regno_live (regno);
      }

  /* Pseudos can't go in stack regs at the start of a basic block that
     is reached by an abnormal edge.  Likewise for registers that are at
     least partly call clobbered, because caller-save, fixup_abnormal_edges
     and possibly the table driven EH machinery are not quite ready to
     handle such pseudos live across such edges.  */
  if (bb_has_abnormal_pred (bb))
    {
#ifdef STACK_REGS
      EXECUTE_IF_SET_IN_SPARSESET (pseudos_live, px)
	lra_reg_info[px].no_stack_p = true;
      for (px = FIRST_STACK_REG; px <= LAST_STACK_REG; px++)
	make_hard_regno_live (px);
#endif
      /* No need to record conflicts for call clobbered regs if we
	 have nonlocal labels around, as we don't ever try to
	 allocate such regs in this case.  */
      if (!cfun->has_nonlocal_label
	  && has_abnormal_call_or_eh_pred_edge_p (bb))
	for (px = 0; HARD_REGISTER_NUM_P (px); px++)
	  if (eh_edge_abi.clobbers_at_least_part_of_reg_p (px)
#ifdef REAL_PIC_OFFSET_TABLE_REGNUM
	      /* We should create a conflict of PIC pseudo with PIC
		 hard reg as PIC hard reg can have a wrong value after
		 jump described by the abnormal edge.  In this case we
		 cannot allocate PIC hard reg to PIC pseudo as PIC
		 pseudo will also have a wrong value.  */
	      || (px == REAL_PIC_OFFSET_TABLE_REGNUM
		  && pic_offset_table_rtx != NULL_RTX
		  && !HARD_REGISTER_P (pic_offset_table_rtx))
#endif
	      )
	    make_hard_regno_live (px);
    }

  bool live_change_p = false;
  /* Check if bb border live info was changed.  */
  unsigned int live_pseudos_num = 0;
  EXECUTE_IF_SET_IN_BITMAP (df_get_live_in (bb),
			    FIRST_PSEUDO_REGISTER, j, bi)
    {
      live_pseudos_num++;
      if (! sparseset_bit_p (pseudos_live, j))
	{
	  live_change_p = true;
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file,
		     "  r%d is removed as live at bb%d start\n", j, bb->index);
	  break;
	}
    }
  if (! live_change_p
      && sparseset_cardinality (pseudos_live) != live_pseudos_num)
    {
      live_change_p = true;
      if (lra_dump_file != NULL)
	EXECUTE_IF_SET_IN_SPARSESET (pseudos_live, j)
	  if (! bitmap_bit_p (df_get_live_in (bb), j))
	    fprintf (lra_dump_file,
		     "  r%d is added to live at bb%d start\n", j, bb->index);
    }
  /* See if we'll need an increment at the end of this basic block.
     An increment is needed if the PSEUDOS_LIVE set is not empty,
     to make sure the finish points are set up correctly.  */
  need_curr_point_incr = (sparseset_cardinality (pseudos_live) > 0);

  EXECUTE_IF_SET_IN_SPARSESET (pseudos_live, i)
    {
      update_pseudo_point (i, curr_point, DEF_POINT);
      mark_pseudo_dead (i);
    }

  EXECUTE_IF_SET_IN_BITMAP (df_get_live_in (bb), FIRST_PSEUDO_REGISTER, j, bi)
    {
      if (sparseset_cardinality (pseudos_live_through_calls) == 0)
	break;
      if (sparseset_bit_p (pseudos_live_through_calls, j))
	check_pseudos_live_through_calls (j, last_call_abi);
    }

  for (i = 0; HARD_REGISTER_NUM_P (i); ++i)
    {
      if (!TEST_HARD_REG_BIT (hard_regs_live, i))
	continue;

      if (!TEST_HARD_REG_BIT (hard_regs_spilled_into, i))
	continue;

      if (bitmap_bit_p (df_get_live_in (bb), i))
	continue;

      live_change_p = true;
      if (lra_dump_file)
	fprintf (lra_dump_file,
		 "  hard reg r%d is added to live at bb%d start\n", i,
		 bb->index);
      bitmap_set_bit (df_get_live_in (bb), i);
    }

  if (need_curr_point_incr)
    next_program_point (curr_point, freq);

  return live_change_p;
}

/* Compress pseudo live ranges by removing program points where
   nothing happens.  Complexity of many algorithms in LRA is linear
   function of program points number.  To speed up the code we try to
   minimize the number of the program points here.  */
static void
remove_some_program_points_and_update_live_ranges (void)
{
  unsigned i;
  int n, max_regno;
  int *map;
  lra_live_range_t r, prev_r, next_r;
  sbitmap_iterator sbi;
  bool born_p, dead_p, prev_born_p, prev_dead_p;

  auto_sbitmap born (lra_live_max_point);
  auto_sbitmap dead (lra_live_max_point);
  bitmap_clear (born);
  bitmap_clear (dead);
  max_regno = max_reg_num ();
  for (i = FIRST_PSEUDO_REGISTER; i < (unsigned) max_regno; i++)
    {
      for (r = lra_reg_info[i].live_ranges; r != NULL; r = r->next)
	{
	  lra_assert (r->start <= r->finish);
	  bitmap_set_bit (born, r->start);
	  bitmap_set_bit (dead, r->finish);
	}
    }
  auto_sbitmap born_or_dead (lra_live_max_point);
  bitmap_ior (born_or_dead, born, dead);
  map = XCNEWVEC (int, lra_live_max_point);
  n = -1;
  prev_born_p = prev_dead_p = false;
  EXECUTE_IF_SET_IN_BITMAP (born_or_dead, 0, i, sbi)
    {
      born_p = bitmap_bit_p (born, i);
      dead_p = bitmap_bit_p (dead, i);
      if ((prev_born_p && ! prev_dead_p && born_p && ! dead_p)
	  || (prev_dead_p && ! prev_born_p && dead_p && ! born_p))
	{
	  map[i] = n;
	  lra_point_freq[n] = MAX (lra_point_freq[n], lra_point_freq[i]);
	}
      else
	{
	  map[i] = ++n;
	  lra_point_freq[n] = lra_point_freq[i];
	}
      prev_born_p = born_p;
      prev_dead_p = dead_p;
    }
  n++;
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file, "Compressing live ranges: from %d to %d - %d%%\n",
	     lra_live_max_point, n,
	     lra_live_max_point ? 100 * n / lra_live_max_point : 100);
  if (n < lra_live_max_point)
    {
      lra_live_max_point = n;
      for (i = FIRST_PSEUDO_REGISTER; i < (unsigned) max_regno; i++)
	{
	  for (prev_r = NULL, r = lra_reg_info[i].live_ranges;
	       r != NULL;
	       r = next_r)
	    {
	      next_r = r->next;
	      r->start = map[r->start];
	      r->finish = map[r->finish];
	      if (prev_r == NULL || prev_r->start > r->finish + 1)
		{
		  prev_r = r;
		  continue;
		}
	      prev_r->start = r->start;
	      prev_r->next = next_r;
	      lra_live_range_pool.remove (r);
	    }
	}
    }
  free (map);
}

/* Print live ranges R to file F.  */
void
lra_print_live_range_list (FILE *f, lra_live_range_t r)
{
  for (; r != NULL; r = r->next)
    fprintf (f, " [%d..%d]", r->start, r->finish);
  fprintf (f, "\n");
}

DEBUG_FUNCTION void
debug (lra_live_range &ref)
{
  lra_print_live_range_list (stderr, &ref);
}

DEBUG_FUNCTION void
debug (lra_live_range *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

/* Print live ranges R to stderr.  */
void
lra_debug_live_range_list (lra_live_range_t r)
{
  lra_print_live_range_list (stderr, r);
}

/* Print live ranges of pseudo REGNO to file F.	 */
static void
print_pseudo_live_ranges (FILE *f, int regno)
{
  if (lra_reg_info[regno].live_ranges == NULL)
    return;
  fprintf (f, " r%d:", regno);
  lra_print_live_range_list (f, lra_reg_info[regno].live_ranges);
}

/* Print live ranges of pseudo REGNO to stderr.	 */
void
lra_debug_pseudo_live_ranges (int regno)
{
  print_pseudo_live_ranges (stderr, regno);
}

/* Print live ranges of all pseudos to file F.	*/
static void
print_live_ranges (FILE *f)
{
  int i, max_regno;

  max_regno = max_reg_num ();
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    print_pseudo_live_ranges (f, i);
}

/* Print live ranges of all pseudos to stderr.	*/
void
lra_debug_live_ranges (void)
{
  print_live_ranges (stderr);
}

/* Compress pseudo live ranges.	 */
static void
compress_live_ranges (void)
{
  remove_some_program_points_and_update_live_ranges ();
  if (lra_dump_file != NULL)
    {
      fprintf (lra_dump_file, "Ranges after the compression:\n");
      print_live_ranges (lra_dump_file);
    }
}



/* The number of the current live range pass.  */
int lra_live_range_iter;

/* The function creates live ranges only for memory pseudos (or for
   all ones if ALL_P), set up CONFLICT_HARD_REGS for the pseudos.  It
   also does dead insn elimination if DEAD_INSN_P and global live
   analysis only for pseudos and only if the pseudo live info was
   changed on a BB border.  Return TRUE if the live info was
   changed.  */
static bool
lra_create_live_ranges_1 (bool all_p, bool dead_insn_p)
{
  basic_block bb;
  int i, hard_regno, max_regno = max_reg_num ();
  int curr_point;
  bool bb_live_change_p, have_referenced_pseudos = false;

  timevar_push (TV_LRA_CREATE_LIVE_RANGES);

  complete_info_p = all_p;
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file,
	     "\n********** Pseudo live ranges #%d: **********\n\n",
	     ++lra_live_range_iter);
  memset (lra_hard_reg_usage, 0, sizeof (lra_hard_reg_usage));
  for (i = 0; i < max_regno; i++)
    {
      lra_reg_info[i].live_ranges = NULL;
      CLEAR_HARD_REG_SET (lra_reg_info[i].conflict_hard_regs);
      lra_reg_info[i].preferred_hard_regno1 = -1;
      lra_reg_info[i].preferred_hard_regno2 = -1;
      lra_reg_info[i].preferred_hard_regno_profit1 = 0;
      lra_reg_info[i].preferred_hard_regno_profit2 = 0;
#ifdef STACK_REGS
      lra_reg_info[i].no_stack_p = false;
#endif
      /* The biggest mode is already set but its value might be to
	 conservative because of recent transformation.  Here in this
	 file we recalculate it again as it costs practically
	 nothing.  */
      if (!HARD_REGISTER_NUM_P (i) && regno_reg_rtx[i] != NULL_RTX)
	lra_reg_info[i].biggest_mode = GET_MODE (regno_reg_rtx[i]);
      else
	lra_reg_info[i].biggest_mode = VOIDmode;
      if (!HARD_REGISTER_NUM_P (i)
	  && lra_reg_info[i].nrefs != 0)
	{
	  if ((hard_regno = reg_renumber[i]) >= 0)
	    lra_hard_reg_usage[hard_regno] += lra_reg_info[i].freq;
	  have_referenced_pseudos = true;
	}
    }
  lra_free_copies ();

  /* Under some circumstances, we can have functions without pseudo
     registers.  For such functions, lra_live_max_point will be 0,
     see e.g. PR55604, and there's nothing more to do for us here.  */
  if (! have_referenced_pseudos)
    {
      timevar_pop (TV_LRA_CREATE_LIVE_RANGES);
      return false;
    }

  pseudos_live = sparseset_alloc (max_regno);
  pseudos_live_through_calls = sparseset_alloc (max_regno);
  pseudos_live_through_setjumps = sparseset_alloc (max_regno);
  start_living = sparseset_alloc (max_regno);
  start_dying = sparseset_alloc (max_regno);
  dead_set = sparseset_alloc (max_regno);
  unused_set = sparseset_alloc (max_regno);
  curr_point = 0;
  unsigned new_length = get_max_uid () * 2;
  point_freq_vec.truncate (0);
  point_freq_vec.reserve_exact (new_length);
  lra_point_freq = point_freq_vec.address ();
  auto_vec<int, 20> post_order_rev_cfg;
  inverted_post_order_compute (&post_order_rev_cfg);
  lra_assert (post_order_rev_cfg.length () == (unsigned) n_basic_blocks_for_fn (cfun));
  bb_live_change_p = false;
  for (i = post_order_rev_cfg.length () - 1; i >= 0; --i)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, post_order_rev_cfg[i]);
      if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun) || bb
	  == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	continue;
      if (process_bb_lives (bb, curr_point, dead_insn_p))
	bb_live_change_p = true;
    }
  if (bb_live_change_p)
    {
      /* We need to clear pseudo live info as some pseudos can
	 disappear, e.g. pseudos with used equivalences.  */
      FOR_EACH_BB_FN (bb, cfun)
	{
	  bitmap_clear_range (df_get_live_in (bb), FIRST_PSEUDO_REGISTER,
			      max_regno - FIRST_PSEUDO_REGISTER);
	  bitmap_clear_range (df_get_live_out (bb), FIRST_PSEUDO_REGISTER,
			      max_regno - FIRST_PSEUDO_REGISTER);
	}
      /* As we did not change CFG since LRA start we can use
	 DF-infrastructure solver to solve live data flow problem.  */
      for (int i = 0; HARD_REGISTER_NUM_P (i); ++i)
	{
	  if (TEST_HARD_REG_BIT (hard_regs_spilled_into, i))
	    bitmap_clear_bit (&all_hard_regs_bitmap, i);
	}
      df_simple_dataflow
	(DF_BACKWARD, NULL, live_con_fun_0, live_con_fun_n,
	 live_trans_fun, &all_blocks,
	 df_get_postorder (DF_BACKWARD), df_get_n_blocks (DF_BACKWARD));
      if (lra_dump_file != NULL)
	{
	  fprintf (lra_dump_file,
		   "Global pseudo live data have been updated:\n");
	  basic_block bb;
	  FOR_EACH_BB_FN (bb, cfun)
	    {
	      bb_data_t bb_info = get_bb_data (bb);
	      bitmap bb_livein = df_get_live_in (bb);
	      bitmap bb_liveout = df_get_live_out (bb);

	      fprintf (lra_dump_file, "\nBB %d:\n", bb->index);
	      lra_dump_bitmap_with_title ("  gen:",
					  &bb_info->gen_pseudos, bb->index);
	      lra_dump_bitmap_with_title ("  killed:",
					  &bb_info->killed_pseudos, bb->index);
	      lra_dump_bitmap_with_title ("  livein:", bb_livein, bb->index);
	      lra_dump_bitmap_with_title ("  liveout:", bb_liveout, bb->index);
	    }
	}
    }
  lra_live_max_point = curr_point;
  if (lra_dump_file != NULL)
    print_live_ranges (lra_dump_file);
  /* Clean up.	*/
  sparseset_free (unused_set);
  sparseset_free (dead_set);
  sparseset_free (start_dying);
  sparseset_free (start_living);
  sparseset_free (pseudos_live_through_calls);
  sparseset_free (pseudos_live_through_setjumps);
  sparseset_free (pseudos_live);
  compress_live_ranges ();
  timevar_pop (TV_LRA_CREATE_LIVE_RANGES);
  return bb_live_change_p;
}

/* The main entry function creates live-ranges and other live info
   necessary for the assignment sub-pass.  It uses
   lra_creates_live_ranges_1 -- so read comments for the
   function.  */
void
lra_create_live_ranges (bool all_p, bool dead_insn_p)
{
  if (! lra_create_live_ranges_1 (all_p, dead_insn_p))
    return;
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file, "Live info was changed -- recalculate it\n");
  /* Live info was changed on a bb border.  It means that some info,
     e.g. about conflict regs, calls crossed, and live ranges may be
     wrong.  We need this info for allocation.  So recalculate it
     again but without removing dead insns which can change live info
     again.  Repetitive live range calculations are expensive therefore
     we stop here as we already have correct info although some
     improvement in rare cases could be possible on this sub-pass if
     we do dead insn elimination again (still the improvement may
     happen later).  */
  lra_clear_live_ranges ();
  bool res = lra_create_live_ranges_1 (all_p, false);
  lra_assert (! res);
}

/* Finish all live ranges.  */
void
lra_clear_live_ranges (void)
{
  int i;

  for (i = 0; i < max_reg_num (); i++)
    free_live_range_list (lra_reg_info[i].live_ranges);
  point_freq_vec.release ();
}

/* Initialize live ranges data once per function.  */
void
lra_live_ranges_init (void)
{
  bitmap_initialize (&temp_bitmap, &reg_obstack);
  initiate_live_solver ();
}

/* Finish live ranges data once per function.  */
void
lra_live_ranges_finish (void)
{
  finish_live_solver ();
  bitmap_clear (&temp_bitmap);
  lra_live_range_pool.release ();
}
