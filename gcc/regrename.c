/* Register renaming for the GNU compiler.
   Copyright (C) 2000-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "addresses.h"
#include "cfganal.h"
#include "tree-pass.h"
#include "function-abi.h"
#include "regrename.h"

/* This file implements the RTL register renaming pass of the compiler.  It is
   a semi-local pass whose goal is to maximize the usage of the register file
   of the processor by substituting registers for others in the solution given
   by the register allocator.  The algorithm is as follows:

     1. Local def/use chains are built: within each basic block, chains are
	opened and closed; if a chain isn't closed at the end of the block,
	it is dropped.  We pre-open chains if we have already examined a
	predecessor block and found chains live at the end which match
	live registers at the start of the new block.

     2. We try to combine the local chains across basic block boundaries by
        comparing chains that were open at the start or end of a block to
	those in successor/predecessor blocks.

     3. For each chain, the set of possible renaming registers is computed.
	This takes into account the renaming of previously processed chains.
	Optionally, a preferred class is computed for the renaming register.

     4. The best renaming register is computed for the chain in the above set,
	using a round-robin allocation.  If a preferred class exists, then the
	round-robin allocation is done within the class first, if possible.
	The round-robin allocation of renaming registers itself is global.

     5. If a renaming register has been found, it is substituted in the chain.

  Targets can parameterize the pass by specifying a preferred class for the
  renaming register for a given (super)class of registers to be renamed.

  DEBUG_INSNs are treated specially, in particular registers occurring inside
  them are treated as requiring ALL_REGS as a class.  */

#if HOST_BITS_PER_WIDE_INT <= MAX_RECOG_OPERANDS
#error "Use a different bitmap implementation for untracked_operands."
#endif

enum scan_actions
{
  terminate_write,
  terminate_dead,
  mark_all_read,
  mark_read,
  mark_write,
  /* mark_access is for marking the destination regs in
     REG_FRAME_RELATED_EXPR notes (as if they were read) so that the
     note is updated properly.  */
  mark_access
};

static const char * const scan_actions_name[] =
{
  "terminate_write",
  "terminate_dead",
  "mark_all_read",
  "mark_read",
  "mark_write",
  "mark_access"
};

/* TICK and THIS_TICK are used to record the last time we saw each
   register.  */
static int tick[FIRST_PSEUDO_REGISTER];
static int this_tick = 0;

static struct obstack rename_obstack;

/* If nonnull, the code calling into the register renamer requested
   information about insn operands, and we store it here.  */
vec<insn_rr_info> insn_rr;

static void scan_rtx (rtx_insn *, rtx *, enum reg_class, enum scan_actions,
		      enum op_type);
static bool build_def_use (basic_block);

/* The id to be given to the next opened chain.  */
static unsigned current_id;

/* A mapping of unique id numbers to chains.  */
static vec<du_head_p> id_to_chain;

/* List of currently open chains.  */
static class du_head *open_chains;

/* Bitmap of open chains.  The bits set always match the list found in
   open_chains.  */
static bitmap_head open_chains_set;

/* Record the registers being tracked in open_chains.  */
static HARD_REG_SET live_in_chains;

/* Record the registers that are live but not tracked.  The intersection
   between this and live_in_chains is empty.  */
static HARD_REG_SET live_hard_regs;

/* Set while scanning RTL if INSN_RR is nonnull, i.e. if the current analysis
   is for a caller that requires operand data.  Used in
   record_operand_use.  */
static operand_rr_info *cur_operand;

/* Set while scanning RTL if a register dies.  Used to tie chains.  */
static class du_head *terminated_this_insn;

/* Return the chain corresponding to id number ID.  Take into account that
   chains may have been merged.  */
du_head_p
regrename_chain_from_id (unsigned int id)
{
  du_head_p first_chain = id_to_chain[id];
  du_head_p chain = first_chain;
  while (chain->id != id)
    {
      id = chain->id;
      chain = id_to_chain[id];
    }
  first_chain->id = id;
  return chain;
}

/* Dump all def/use chains, starting at id FROM.  */

static void
dump_def_use_chain (int from)
{
  du_head_p head;
  int i;
  FOR_EACH_VEC_ELT_FROM (id_to_chain, i, head, from)
    {
      struct du_chain *this_du = head->first;

      fprintf (dump_file, "Register %s (%d):",
	       reg_names[head->regno], head->nregs);
      while (this_du)
	{
	  fprintf (dump_file, " %d [%s]", INSN_UID (this_du->insn),
		   reg_class_names[this_du->cl]);
	  this_du = this_du->next_use;
	}
      fprintf (dump_file, "\n");
      head = head->next_chain;
    }
}

static void
free_chain_data (void)
{
  int i;
  du_head_p ptr;
  for (i = 0; id_to_chain.iterate (i, &ptr); i++)
    bitmap_clear (&ptr->conflicts);

  id_to_chain.release ();
}

/* Walk all chains starting with CHAINS and record that they conflict with
   another chain whose id is ID.  */

static void
mark_conflict (class du_head *chains, unsigned id)
{
  while (chains)
    {
      bitmap_set_bit (&chains->conflicts, id);
      chains = chains->next_chain;
    }
}

/* Examine cur_operand, and if it is nonnull, record information about the
   use THIS_DU which is part of the chain HEAD.  */

static void
record_operand_use (class du_head *head, struct du_chain *this_du)
{
  if (cur_operand == NULL || cur_operand->failed)
    return;
  if (head->cannot_rename)
    {
      cur_operand->failed = true;
      return;
    }
  gcc_assert (cur_operand->n_chains < MAX_REGS_PER_ADDRESS);
  cur_operand->heads[cur_operand->n_chains] = head;
  cur_operand->chains[cur_operand->n_chains++] = this_du;
}

/* Create a new chain for THIS_NREGS registers starting at THIS_REGNO,
   and record its occurrence in *LOC, which is being written to in INSN.
   This access requires a register of class CL.  */

static du_head_p
create_new_chain (unsigned this_regno, unsigned this_nregs, rtx *loc,
		  rtx_insn *insn, enum reg_class cl)
{
  class du_head *head = XOBNEW (&rename_obstack, class du_head);
  struct du_chain *this_du;
  int nregs;

  memset ((void *)head, 0, sizeof *head);
  head->next_chain = open_chains;
  head->regno = this_regno;
  head->nregs = this_nregs;

  id_to_chain.safe_push (head);
  head->id = current_id++;

  bitmap_initialize (&head->conflicts, &bitmap_default_obstack);
  bitmap_copy (&head->conflicts, &open_chains_set);
  mark_conflict (open_chains, head->id);

  /* Since we're tracking this as a chain now, remove it from the
     list of conflicting live hard registers and track it in
     live_in_chains instead.  */
  nregs = head->nregs;
  while (nregs-- > 0)
    {
      SET_HARD_REG_BIT (live_in_chains, head->regno + nregs);
      CLEAR_HARD_REG_BIT (live_hard_regs, head->regno + nregs);
    }

  head->hard_conflicts = live_hard_regs;
  bitmap_set_bit (&open_chains_set, head->id);

  open_chains = head;

  if (dump_file)
    {
      fprintf (dump_file, "Creating chain %s (%d)",
	       reg_names[head->regno], head->id);
      if (insn != NULL_RTX)
	fprintf (dump_file, " at insn %d", INSN_UID (insn));
      fprintf (dump_file, "\n");
    }

  if (insn == NULL_RTX)
    {
      head->first = head->last = NULL;
      return head;
    }

  this_du = XOBNEW (&rename_obstack, struct du_chain);
  head->first = head->last = this_du;

  this_du->next_use = 0;
  this_du->loc = loc;
  this_du->insn = insn;
  this_du->cl = cl;
  record_operand_use (head, this_du);
  return head;
}

/* For a def-use chain HEAD, find which registers overlap its lifetime and
   set the corresponding bits in *PSET.  */

static void
merge_overlapping_regs (HARD_REG_SET *pset, class du_head *head)
{
  bitmap_iterator bi;
  unsigned i;
  *pset |= head->hard_conflicts;
  EXECUTE_IF_SET_IN_BITMAP (&head->conflicts, 0, i, bi)
    {
      du_head_p other = regrename_chain_from_id (i);
      unsigned j = other->nregs;
      gcc_assert (other != head);
      while (j-- > 0)
	SET_HARD_REG_BIT (*pset, other->regno + j);
    }
}

/* Return true if (reg:MODE REGNO) would be clobbered by a call covered
   by THIS_HEAD.  */

static bool
call_clobbered_in_chain_p (du_head *this_head, machine_mode mode,
			   unsigned int regno)
{
  return call_clobbered_in_region_p (this_head->call_abis,
				     this_head->call_clobber_mask,
				     mode, regno);
}

/* Check if NEW_REG can be the candidate register to rename for
   REG in THIS_HEAD chain.  THIS_UNAVAILABLE is a set of unavailable hard
   registers.  */

static bool
check_new_reg_p (int reg ATTRIBUTE_UNUSED, int new_reg,
		 class du_head *this_head, HARD_REG_SET this_unavailable)
{
  machine_mode mode = GET_MODE (*this_head->first->loc);
  int nregs = hard_regno_nregs (new_reg, mode);
  int i;
  struct du_chain *tmp;

  for (i = nregs - 1; i >= 0; --i)
    if (TEST_HARD_REG_BIT (this_unavailable, new_reg + i)
	|| fixed_regs[new_reg + i]
	|| global_regs[new_reg + i]
	/* Can't use regs which aren't saved by the prologue.  */
	|| (! df_regs_ever_live_p (new_reg + i)
	    && ! crtl->abi->clobbers_full_reg_p (new_reg + i))
#ifdef LEAF_REGISTERS
	/* We can't use a non-leaf register if we're in a
	   leaf function.  */
	|| (crtl->is_leaf
	    && !LEAF_REGISTERS[new_reg + i])
#endif
	|| ! HARD_REGNO_RENAME_OK (reg + i, new_reg + i))
      return false;

  /* See whether it accepts all modes that occur in
     definition and uses.  */
  for (tmp = this_head->first; tmp; tmp = tmp->next_use)
    if ((!targetm.hard_regno_mode_ok (new_reg, GET_MODE (*tmp->loc))
	 && ! DEBUG_INSN_P (tmp->insn))
	|| call_clobbered_in_chain_p (this_head, GET_MODE (*tmp->loc),
				      new_reg))
      return false;

  return true;
}

/* For the chain THIS_HEAD, compute and return the best register to
   rename to.  SUPER_CLASS is the superunion of register classes in
   the chain.  UNAVAILABLE is a set of registers that cannot be used.
   OLD_REG is the register currently used for the chain.  BEST_RENAME
   controls whether the register chosen must be better than the
   current one or just respect the given constraint.  */

int
find_rename_reg (du_head_p this_head, enum reg_class super_class,
		 HARD_REG_SET *unavailable, int old_reg, bool best_rename)
{
  bool has_preferred_class;
  enum reg_class preferred_class;
  int pass;
  int best_new_reg = old_reg;

  /* Mark registers that overlap this chain's lifetime as unavailable.  */
  merge_overlapping_regs (unavailable, this_head);

  /* Compute preferred rename class of super union of all the classes
     in the chain.  */
  preferred_class
    = (enum reg_class) targetm.preferred_rename_class (super_class);

  /* Pick and check the register from the tied chain iff the tied chain
     is not renamed.  */
  if (this_head->tied_chain && !this_head->tied_chain->renamed
      && check_new_reg_p (old_reg, this_head->tied_chain->regno,
			  this_head, *unavailable))
    return this_head->tied_chain->regno;

  /* If PREFERRED_CLASS is not NO_REGS, we iterate in the first pass
     over registers that belong to PREFERRED_CLASS and try to find the
     best register within the class.  If that failed, we iterate in
     the second pass over registers that don't belong to the class.
     If PREFERRED_CLASS is NO_REGS, we iterate over all registers in
     ascending order without any preference.  */
  has_preferred_class = (preferred_class != NO_REGS);
  for (pass = (has_preferred_class ? 0 : 1); pass < 2; pass++)
    {
      int new_reg;
      for (new_reg = 0; new_reg < FIRST_PSEUDO_REGISTER; new_reg++)
	{
	  if (has_preferred_class
	      && (pass == 0)
	      != TEST_HARD_REG_BIT (reg_class_contents[preferred_class],
				    new_reg))
	    continue;

	  if (!check_new_reg_p (old_reg, new_reg, this_head, *unavailable))
	    continue;

	  if (!best_rename)
	    return new_reg;

	  /* In the first pass, we force the renaming of registers that
	     don't belong to PREFERRED_CLASS to registers that do, even
	     though the latters were used not very long ago.  */
	  if ((pass == 0
	      && !TEST_HARD_REG_BIT (reg_class_contents[preferred_class],
				     best_new_reg))
	      || tick[best_new_reg] > tick[new_reg])
	    best_new_reg = new_reg;
	}
      if (pass == 0 && best_new_reg != old_reg)
	break;
    }
  return best_new_reg;
}

/* Iterate over elements in the chain HEAD in order to:
   1. Count number of uses, storing it in *PN_USES.
   2. Narrow the set of registers we can use for renaming, adding
      unavailable registers to *PUNAVAILABLE, which must be
      initialized by the caller.
   3. Compute the superunion of register classes in this chain
      and return it.  */
reg_class
regrename_find_superclass (du_head_p head, int *pn_uses,
			   HARD_REG_SET *punavailable)
{
  int n_uses = 0;
  reg_class super_class = NO_REGS;
  for (du_chain *tmp = head->first; tmp; tmp = tmp->next_use)
    {
      if (DEBUG_INSN_P (tmp->insn))
	continue;
      n_uses++;
      *punavailable |= ~reg_class_contents[tmp->cl];
      super_class
	= reg_class_superunion[(int) super_class][(int) tmp->cl];
    }
  *pn_uses = n_uses;
  return super_class;
}

/* Perform register renaming on the current function.  */
static void
rename_chains (void)
{
  HARD_REG_SET unavailable;
  du_head_p this_head;
  int i;

  memset (tick, 0, sizeof tick);

  CLEAR_HARD_REG_SET (unavailable);
  /* Don't clobber traceback for noreturn functions.  */
  if (frame_pointer_needed)
    {
      add_to_hard_reg_set (&unavailable, Pmode, FRAME_POINTER_REGNUM);
      if (!HARD_FRAME_POINTER_IS_FRAME_POINTER)
	add_to_hard_reg_set (&unavailable, Pmode, HARD_FRAME_POINTER_REGNUM);
    }

  FOR_EACH_VEC_ELT (id_to_chain, i, this_head)
    {
      int best_new_reg;
      int n_uses;
      HARD_REG_SET this_unavailable;
      int reg = this_head->regno;

      if (this_head->cannot_rename)
	continue;

      if (fixed_regs[reg] || global_regs[reg]
	  || (!HARD_FRAME_POINTER_IS_FRAME_POINTER && frame_pointer_needed
	      && reg == HARD_FRAME_POINTER_REGNUM)
	  || (HARD_FRAME_POINTER_IS_FRAME_POINTER && frame_pointer_needed
	      && reg == FRAME_POINTER_REGNUM))
	continue;

      this_unavailable = unavailable;

      reg_class super_class = regrename_find_superclass (this_head, &n_uses,
							 &this_unavailable);
      if (n_uses < 2)
	continue;

      best_new_reg = find_rename_reg (this_head, super_class,
				      &this_unavailable, reg, true);

      if (dump_file)
	{
	  fprintf (dump_file, "Register %s in insn %d",
		   reg_names[reg], INSN_UID (this_head->first->insn));
	  if (this_head->call_abis)
	    fprintf (dump_file, " crosses a call");
	}

      if (best_new_reg == reg)
	{
	  tick[reg] = ++this_tick;
	  if (dump_file)
	    fprintf (dump_file, "; no available better choice\n");
	  continue;
	}

      if (regrename_do_replace (this_head, best_new_reg))
	{
	  if (dump_file)
	    fprintf (dump_file, ", renamed as %s\n", reg_names[best_new_reg]);
	  tick[best_new_reg] = ++this_tick;
	  df_set_regs_ever_live (best_new_reg, true);
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, ", renaming as %s failed\n",
		     reg_names[best_new_reg]);
	  tick[reg] = ++this_tick;
	}
    }
}

/* A structure to record information for each hard register at the start of
   a basic block.  */
struct incoming_reg_info {
  /* Holds the number of registers used in the chain that gave us information
     about this register.  Zero means no information known yet, while a
     negative value is used for something that is part of, but not the first
     register in a multi-register value.  */
  int nregs;
  /* Set to true if we have accesses that conflict in the number of registers
     used.  */
  bool unusable;
};

/* A structure recording information about each basic block.  It is saved
   and restored around basic block boundaries.
   A pointer to such a structure is stored in each basic block's aux field
   during regrename_analyze, except for blocks we know can't be optimized
   (such as entry and exit blocks).  */
class bb_rename_info
{
public:
  /* The basic block corresponding to this structure.  */
  basic_block bb;
  /* Copies of the global information.  */
  bitmap_head open_chains_set;
  bitmap_head incoming_open_chains_set;
  struct incoming_reg_info incoming[FIRST_PSEUDO_REGISTER];
};

/* Initialize a rename_info structure P for basic block BB, which starts a new
   scan.  */
static void
init_rename_info (class bb_rename_info *p, basic_block bb)
{
  int i;
  df_ref def;
  HARD_REG_SET start_chains_set;

  p->bb = bb;
  bitmap_initialize (&p->open_chains_set, &bitmap_default_obstack);
  bitmap_initialize (&p->incoming_open_chains_set, &bitmap_default_obstack);

  open_chains = NULL;
  bitmap_clear (&open_chains_set);

  CLEAR_HARD_REG_SET (live_in_chains);
  REG_SET_TO_HARD_REG_SET (live_hard_regs, df_get_live_in (bb));
  FOR_EACH_ARTIFICIAL_DEF (def, bb->index)
    if (DF_REF_FLAGS (def) & DF_REF_AT_TOP)
      SET_HARD_REG_BIT (live_hard_regs, DF_REF_REGNO (def));

  /* Open chains based on information from (at least one) predecessor
     block.  This gives us a chance later on to combine chains across
     basic block boundaries.  Inconsistencies (in access sizes) will
     be caught normally and dealt with conservatively by disabling the
     chain for renaming, and there is no risk of losing optimization
     opportunities by opening chains either: if we did not open the
     chains, we'd have to track the live register as a hard reg, and
     we'd be unable to rename it in any case.  */
  CLEAR_HARD_REG_SET (start_chains_set);
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      struct incoming_reg_info *iri = p->incoming + i;
      if (iri->nregs > 0 && !iri->unusable
	  && range_in_hard_reg_set_p (live_hard_regs, i, iri->nregs))
	{
	  SET_HARD_REG_BIT (start_chains_set, i);
	  remove_range_from_hard_reg_set (&live_hard_regs, i, iri->nregs);
	}
    }
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      struct incoming_reg_info *iri = p->incoming + i;
      if (TEST_HARD_REG_BIT (start_chains_set, i))
	{
	  du_head_p chain;
	  if (dump_file)
	    fprintf (dump_file, "opening incoming chain\n");
	  chain = create_new_chain (i, iri->nregs, NULL, NULL, NO_REGS);
	  bitmap_set_bit (&p->incoming_open_chains_set, chain->id);
	}
    }
}

/* Record in RI that the block corresponding to it has an incoming
   live value, described by CHAIN.  */
static void
set_incoming_from_chain (class bb_rename_info *ri, du_head_p chain)
{
  int i;
  int incoming_nregs = ri->incoming[chain->regno].nregs;
  int nregs;

  /* If we've recorded the same information before, everything is fine.  */
  if (incoming_nregs == chain->nregs)
    {
      if (dump_file)
	fprintf (dump_file, "reg %d/%d already recorded\n",
		 chain->regno, chain->nregs);
      return;
    }

  /* If we have no information for any of the involved registers, update
     the incoming array.  */
  nregs = chain->nregs;
  while (nregs-- > 0)
    if (ri->incoming[chain->regno + nregs].nregs != 0
	|| ri->incoming[chain->regno + nregs].unusable)
      break;
  if (nregs < 0)
    {
      nregs = chain->nregs;
      ri->incoming[chain->regno].nregs = nregs;
      while (nregs-- > 1)
	ri->incoming[chain->regno + nregs].nregs = -nregs;
      if (dump_file)
	fprintf (dump_file, "recorded reg %d/%d\n",
		 chain->regno, chain->nregs);
      return;
    }

  /* There must be some kind of conflict.  Prevent both the old and
     new ranges from being used.  */
  if (incoming_nregs < 0)
    ri->incoming[chain->regno + incoming_nregs].unusable = true;
  for (i = 0; i < chain->nregs; i++)
    ri->incoming[chain->regno + i].unusable = true;
}

/* Merge the two chains C1 and C2 so that all conflict information is
   recorded and C1, and the id of C2 is changed to that of C1.  */
static void
merge_chains (du_head_p c1, du_head_p c2)
{
  if (c1 == c2)
    return;

  if (c2->first != NULL)
    {
      if (c1->first == NULL)
	c1->first = c2->first;
      else
	c1->last->next_use = c2->first;
      c1->last = c2->last;
    }

  c2->first = c2->last = NULL;
  c2->id = c1->id;

  c1->hard_conflicts |= c2->hard_conflicts;
  bitmap_ior_into (&c1->conflicts, &c2->conflicts);

  c1->call_clobber_mask |= c2->call_clobber_mask;
  c1->call_abis |= c2->call_abis;
  c1->cannot_rename |= c2->cannot_rename;
}

/* Analyze the current function and build chains for renaming.  */

void
regrename_analyze (bitmap bb_mask)
{
  class bb_rename_info *rename_info;
  int i;
  basic_block bb;
  int n_bbs;
  int *inverse_postorder;

  inverse_postorder = XNEWVEC (int, last_basic_block_for_fn (cfun));
  n_bbs = pre_and_rev_post_order_compute (NULL, inverse_postorder, false);

  /* Gather some information about the blocks in this function.  */
  rename_info = XCNEWVEC (class bb_rename_info, n_basic_blocks_for_fn (cfun));
  i = 0;
  FOR_EACH_BB_FN (bb, cfun)
    {
      class bb_rename_info *ri = rename_info + i;
      ri->bb = bb;
      if (bb_mask != NULL && !bitmap_bit_p (bb_mask, bb->index))
	bb->aux = NULL;
      else
	bb->aux = ri;
      i++;
    }

  current_id = 0;
  id_to_chain.create (0);
  bitmap_initialize (&open_chains_set, &bitmap_default_obstack);

  /* The order in which we visit blocks ensures that whenever
     possible, we only process a block after at least one of its
     predecessors, which provides a "seeding" effect to make the logic
     in set_incoming_from_chain and init_rename_info useful.  */

  for (i = 0; i < n_bbs; i++)
    {
      basic_block bb1 = BASIC_BLOCK_FOR_FN (cfun, inverse_postorder[i]);
      class bb_rename_info *this_info;
      bool success;
      edge e;
      edge_iterator ei;
      int old_length = id_to_chain.length ();

      this_info = (class bb_rename_info *) bb1->aux;
      if (this_info == NULL)
	continue;

      if (dump_file)
	fprintf (dump_file, "\nprocessing block %d:\n", bb1->index);

      init_rename_info (this_info, bb1);

      success = build_def_use (bb1);
      if (!success)
	{
	  if (dump_file)
	    fprintf (dump_file, "failed\n");
	  bb1->aux = NULL;
	  id_to_chain.truncate (old_length);
	  current_id = old_length;
	  bitmap_clear (&this_info->incoming_open_chains_set);
	  open_chains = NULL;
	  if (insn_rr.exists ())
	    {
	      rtx_insn *insn;
	      FOR_BB_INSNS (bb1, insn)
		{
		  insn_rr_info *p = &insn_rr[INSN_UID (insn)];
		  p->op_info = NULL;
		}
	    }
	  continue;
	}

      if (dump_file)
	dump_def_use_chain (old_length);
      bitmap_copy (&this_info->open_chains_set, &open_chains_set);

      /* Add successor blocks to the worklist if necessary, and record
	 data about our own open chains at the end of this block, which
	 will be used to pre-open chains when processing the successors.  */
      FOR_EACH_EDGE (e, ei, bb1->succs)
	{
	  class bb_rename_info *dest_ri;
	  class du_head *chain;

	  if (dump_file)
	    fprintf (dump_file, "successor block %d\n", e->dest->index);

	  if (e->flags & (EDGE_EH | EDGE_ABNORMAL))
	    continue;
	  dest_ri = (class bb_rename_info *)e->dest->aux;
	  if (dest_ri == NULL)
	    continue;
	  for (chain = open_chains; chain; chain = chain->next_chain)
	    set_incoming_from_chain (dest_ri, chain);
	}
    }

  free (inverse_postorder);

  /* Now, combine the chains data we have gathered across basic block
     boundaries.

     For every basic block, there may be chains open at the start, or at the
     end.  Rather than exclude them from renaming, we look for open chains
     with matching registers at the other side of the CFG edge.

     For a given chain using register R, open at the start of block B, we
     must find an open chain using R on the other side of every edge leading
     to B, if the register is live across this edge.  In the code below,
     N_PREDS_USED counts the number of edges where the register is live, and
     N_PREDS_JOINED counts those where we found an appropriate chain for
     joining.

     We perform the analysis for both incoming and outgoing edges, but we
     only need to merge once (in the second part, after verifying outgoing
     edges).  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      class bb_rename_info *bb_ri = (class bb_rename_info *) bb->aux;
      unsigned j;
      bitmap_iterator bi;

      if (bb_ri == NULL)
	continue;

      if (dump_file)
	fprintf (dump_file, "processing bb %d in edges\n", bb->index);

      EXECUTE_IF_SET_IN_BITMAP (&bb_ri->incoming_open_chains_set, 0, j, bi)
	{
	  edge e;
	  edge_iterator ei;
	  class du_head *chain = regrename_chain_from_id (j);
	  int n_preds_used = 0, n_preds_joined = 0;

	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      class bb_rename_info *src_ri;
	      unsigned k;
	      bitmap_iterator bi2;
	      HARD_REG_SET live;
	      bool success = false;

	      REG_SET_TO_HARD_REG_SET (live, df_get_live_out (e->src));
	      if (!range_overlaps_hard_reg_set_p (live, chain->regno,
						  chain->nregs))
		continue;
	      n_preds_used++;

	      if (e->flags & (EDGE_EH | EDGE_ABNORMAL))
		continue;

	      src_ri = (class bb_rename_info *)e->src->aux;
	      if (src_ri == NULL)
		continue;

	      EXECUTE_IF_SET_IN_BITMAP (&src_ri->open_chains_set,
					0, k, bi2)
		{
		  class du_head *outgoing_chain = regrename_chain_from_id (k);

		  if (outgoing_chain->regno == chain->regno
		      && outgoing_chain->nregs == chain->nregs)
		    {
		      n_preds_joined++;
		      success = true;
		      break;
		    }
		}
	      if (!success && dump_file)
		fprintf (dump_file, "failure to match with pred block %d\n",
			 e->src->index);
	    }
	  if (n_preds_joined < n_preds_used)
	    {
	      if (dump_file)
		fprintf (dump_file, "cannot rename chain %d\n", j);
	      chain->cannot_rename = 1;
	    }
	}
    }
  FOR_EACH_BB_FN (bb, cfun)
    {
      class bb_rename_info *bb_ri = (class bb_rename_info *) bb->aux;
      unsigned j;
      bitmap_iterator bi;

      if (bb_ri == NULL)
	continue;

      if (dump_file)
	fprintf (dump_file, "processing bb %d out edges\n", bb->index);

      EXECUTE_IF_SET_IN_BITMAP (&bb_ri->open_chains_set, 0, j, bi)
	{
	  edge e;
	  edge_iterator ei;
	  class du_head *chain = regrename_chain_from_id (j);
	  int n_succs_used = 0, n_succs_joined = 0;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      bool printed = false;
	      class bb_rename_info *dest_ri;
	      unsigned k;
	      bitmap_iterator bi2;
	      HARD_REG_SET live;

	      REG_SET_TO_HARD_REG_SET (live, df_get_live_in (e->dest));
	      if (!range_overlaps_hard_reg_set_p (live, chain->regno,
						  chain->nregs))
		continue;
	      
	      n_succs_used++;

	      dest_ri = (class bb_rename_info *)e->dest->aux;
	      if (dest_ri == NULL)
		continue;

	      EXECUTE_IF_SET_IN_BITMAP (&dest_ri->incoming_open_chains_set,
					0, k, bi2)
		{
		  class du_head *incoming_chain = regrename_chain_from_id (k);

		  if (incoming_chain->regno == chain->regno
		      && incoming_chain->nregs == chain->nregs)
		    {
		      if (dump_file)
			{
			  if (!printed)
			    fprintf (dump_file,
				     "merging blocks for edge %d -> %d\n",
				     e->src->index, e->dest->index);
			  printed = true;
			  fprintf (dump_file,
				   "  merging chains %d (->%d) and %d (->%d) [%s]\n",
				   k, incoming_chain->id, j, chain->id, 
				   reg_names[incoming_chain->regno]);
			}

		      merge_chains (chain, incoming_chain);
		      n_succs_joined++;
		      break;
		    }
		}
	    }
	  if (n_succs_joined < n_succs_used)
	    {
	      if (dump_file)
		fprintf (dump_file, "cannot rename chain %d\n",
			 j);
	      chain->cannot_rename = 1;
	    }
	}
    }

  free (rename_info);

  FOR_EACH_BB_FN (bb, cfun)
    bb->aux = NULL;
}

/* Attempt to replace all uses of the register in the chain beginning with
   HEAD with REG.  Returns true on success and false if the replacement is
   rejected because the insns would not validate.  The latter can happen
   e.g. if a match_parallel predicate enforces restrictions on register
   numbering in its subpatterns.  */

bool
regrename_do_replace (class du_head *head, int reg)
{
  struct du_chain *chain;
  unsigned int base_regno = head->regno;
  machine_mode mode;
  rtx last_reg = NULL_RTX, last_repl = NULL_RTX;

  for (chain = head->first; chain; chain = chain->next_use)
    {
      unsigned int regno = ORIGINAL_REGNO (*chain->loc);
      class reg_attrs *attr = REG_ATTRS (*chain->loc);
      int reg_ptr = REG_POINTER (*chain->loc);

      if (DEBUG_INSN_P (chain->insn) && REGNO (*chain->loc) != base_regno)
	validate_change (chain->insn, &(INSN_VAR_LOCATION_LOC (chain->insn)),
			 gen_rtx_UNKNOWN_VAR_LOC (), true);
      else
	{
	  if (*chain->loc != last_reg)
	    {
	      last_repl = gen_raw_REG (GET_MODE (*chain->loc), reg);
	      if (regno >= FIRST_PSEUDO_REGISTER)
		ORIGINAL_REGNO (last_repl) = regno;
	      REG_ATTRS (last_repl) = attr;
	      REG_POINTER (last_repl) = reg_ptr;
	      last_reg = *chain->loc;
	    }
	  validate_change (chain->insn, chain->loc, last_repl, true);
	}
    }

  if (!apply_change_group ())
    return false;

  mode = GET_MODE (*head->first->loc);
  head->renamed = 1;
  head->regno = reg;
  head->nregs = hard_regno_nregs (reg, mode);
  return true;
}


/* True if we found a register with a size mismatch, which means that we
   can't track its lifetime accurately.  If so, we abort the current block
   without renaming.  */
static bool fail_current_block;

/* Return true if OP is a reg for which all bits are set in PSET, false
   if all bits are clear.
   In other cases, set fail_current_block and return false.  */

static bool
verify_reg_in_set (rtx op, HARD_REG_SET *pset)
{
  unsigned regno, nregs;
  bool all_live, all_dead;
  if (!REG_P (op))
    return false;

  regno = REGNO (op);
  nregs = REG_NREGS (op);
  all_live = all_dead = true;
  while (nregs-- > 0)
    if (TEST_HARD_REG_BIT (*pset, regno + nregs))
      all_dead = false;
    else
      all_live = false;
  if (!all_dead && !all_live)
    {
      fail_current_block = true;
      return false;
    }
  return all_live;
}

/* Return true if OP is a reg that is being tracked already in some form.
   May set fail_current_block if it sees an unhandled case of overlap.  */

static bool
verify_reg_tracked (rtx op)
{
  return (verify_reg_in_set (op, &live_hard_regs)
	  || verify_reg_in_set (op, &live_in_chains));
}

/* Called through note_stores.  DATA points to a rtx_code, either SET or
   CLOBBER, which tells us which kind of rtx to look at.  If we have a
   match, record the set register in live_hard_regs and in the hard_conflicts
   bitmap of open chains.  */

static void
note_sets_clobbers (rtx x, const_rtx set, void *data)
{
  enum rtx_code code = *(enum rtx_code *)data;
  class du_head *chain;

  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);
  if (!REG_P (x) || GET_CODE (set) != code)
    return;
  /* There must not be pseudos at this point.  */
  gcc_assert (HARD_REGISTER_P (x));
  add_to_hard_reg_set (&live_hard_regs, GET_MODE (x), REGNO (x));
  for (chain = open_chains; chain; chain = chain->next_chain)
    add_to_hard_reg_set (&chain->hard_conflicts, GET_MODE (x), REGNO (x));
}

static void
scan_rtx_reg (rtx_insn *insn, rtx *loc, enum reg_class cl, enum scan_actions action,
	      enum op_type type)
{
  class du_head **p;
  rtx x = *loc;
  unsigned this_regno = REGNO (x);
  int this_nregs = REG_NREGS (x);

  if (action == mark_write)
    {
      if (type == OP_OUT)
	{
	  du_head_p c;
	  rtx pat = PATTERN (insn);

	  c = create_new_chain (this_regno, this_nregs, loc, insn, cl);

	  /* We try to tie chains in a move instruction for
	     a single output.  */
	  if (recog_data.n_operands == 2
	      && GET_CODE (pat) == SET
	      && GET_CODE (SET_DEST (pat)) == REG
	      && GET_CODE (SET_SRC (pat)) == REG
	      && terminated_this_insn
	      && terminated_this_insn->nregs
		 == REG_NREGS (recog_data.operand[1]))
	    {
	      gcc_assert (terminated_this_insn->regno
			  == REGNO (recog_data.operand[1]));

	      c->tied_chain = terminated_this_insn;
	      terminated_this_insn->tied_chain = c;

	      if (dump_file)
		fprintf (dump_file, "Tying chain %s (%d) with %s (%d)\n",
			 reg_names[c->regno], c->id,
			 reg_names[terminated_this_insn->regno],
			 terminated_this_insn->id);
	    }
	}

      return;
    }

  if ((type == OP_OUT) != (action == terminate_write || action == mark_access))
    return;

  for (p = &open_chains; *p;)
    {
      class du_head *head = *p;
      class du_head *next = head->next_chain;
      int exact_match = (head->regno == this_regno
			 && head->nregs == this_nregs);
      int superset = (this_regno <= head->regno
		      && this_regno + this_nregs >= head->regno + head->nregs);
      int subset = (this_regno >= head->regno
		      && this_regno + this_nregs <= head->regno + head->nregs);

      if (!bitmap_bit_p (&open_chains_set, head->id)
	  || head->regno + head->nregs <= this_regno
	  || this_regno + this_nregs <= head->regno)
	{
	  p = &head->next_chain;
	  continue;
	}

      if (action == mark_read || action == mark_access)
	{
	  /* ??? Class NO_REGS can happen if the md file makes use of
	     EXTRA_CONSTRAINTS to match registers.  Which is arguably
	     wrong, but there we are.  */

	  if (cl == NO_REGS || (!exact_match && !DEBUG_INSN_P (insn)))
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "Cannot rename chain %s (%d) at insn %d (%s)\n",
			 reg_names[head->regno], head->id, INSN_UID (insn),
			 scan_actions_name[(int) action]);
	      head->cannot_rename = 1;
	      if (superset)
		{
		  unsigned nregs = this_nregs;
		  head->regno = this_regno;
		  head->nregs = this_nregs;
		  while (nregs-- > 0)
		    SET_HARD_REG_BIT (live_in_chains, head->regno + nregs);
		  if (dump_file)
		    fprintf (dump_file,
			     "Widening register in chain %s (%d) at insn %d\n",
			     reg_names[head->regno], head->id, INSN_UID (insn));
		}
	      else if (!subset)
		{
		  fail_current_block = true;
		  if (dump_file)
		    fprintf (dump_file,
			     "Failing basic block due to unhandled overlap\n");
		}
	    }
	  else
	    {
	      struct du_chain *this_du;
	      this_du = XOBNEW (&rename_obstack, struct du_chain);
	      this_du->next_use = 0;
	      this_du->loc = loc;
	      this_du->insn = insn;
	      this_du->cl = cl;
	      if (head->first == NULL)
		head->first = this_du;
	      else
		head->last->next_use = this_du;
	      record_operand_use (head, this_du);
	      head->last = this_du;
	    }
	  /* Avoid adding the same location in a DEBUG_INSN multiple times,
	     which could happen with non-exact overlap.  */
	  if (DEBUG_INSN_P (insn))
	    return;
	  /* Otherwise, find any other chains that do not match exactly;
	     ensure they all get marked unrenamable.  */
	  p = &head->next_chain;
	  continue;
	}

      /* Whether the terminated chain can be used for renaming
	 depends on the action and this being an exact match.
	 In either case, we remove this element from open_chains.  */

      if ((action == terminate_dead || action == terminate_write)
	  && (superset || subset))
	{
	  unsigned nregs;

	  if (subset && !superset)
	    head->cannot_rename = 1;
	  bitmap_clear_bit (&open_chains_set, head->id);

	  nregs = head->nregs;
	  while (nregs-- > 0)
	    {
	      CLEAR_HARD_REG_BIT (live_in_chains, head->regno + nregs);
	      if (subset && !superset
		  && (head->regno + nregs < this_regno
		      || head->regno + nregs >= this_regno + this_nregs))
		SET_HARD_REG_BIT (live_hard_regs, head->regno + nregs);
	    }

	  if (action == terminate_dead)
	    terminated_this_insn = *p;
	  *p = next;
	  if (dump_file)
	    fprintf (dump_file,
		     "Closing chain %s (%d) at insn %d (%s%s)\n",
		     reg_names[head->regno], head->id, INSN_UID (insn),
		     scan_actions_name[(int) action],
		     superset ? ", superset" : subset ? ", subset" : "");
	}
      else if (action == terminate_dead || action == terminate_write)
	{
	  /* In this case, tracking liveness gets too hard.  Fail the
	     entire basic block.  */
	  if (dump_file)
	    fprintf (dump_file,
		     "Failing basic block due to unhandled overlap\n");
	  fail_current_block = true;
	  return;
	}
      else
	{
	  head->cannot_rename = 1;
	  if (dump_file)
	    fprintf (dump_file,
		     "Cannot rename chain %s (%d) at insn %d (%s)\n",
		     reg_names[head->regno], head->id, INSN_UID (insn),
		     scan_actions_name[(int) action]);
	  p = &head->next_chain;
	}
    }
}

/* A wrapper around base_reg_class which returns ALL_REGS if INSN is a
   DEBUG_INSN.  The arguments MODE, AS, CODE and INDEX_CODE are as for
   base_reg_class.  */

static reg_class
base_reg_class_for_rename (rtx_insn *insn, machine_mode mode, addr_space_t as,
			   rtx_code code, rtx_code index_code)
{
  if (DEBUG_INSN_P (insn))
    return ALL_REGS;
  return base_reg_class (mode, as, code, index_code);
}

/* Adapted from find_reloads_address_1.  CL is INDEX_REG_CLASS or
   BASE_REG_CLASS depending on how the register is being considered.  */

static void
scan_rtx_address (rtx_insn *insn, rtx *loc, enum reg_class cl,
		  enum scan_actions action, machine_mode mode,
		  addr_space_t as)
{
  rtx x = *loc;
  RTX_CODE code = GET_CODE (x);
  const char *fmt;
  int i, j;

  if (action == mark_write || action == mark_access)
    return;

  switch (code)
    {
    case PLUS:
      {
	rtx orig_op0 = XEXP (x, 0);
	rtx orig_op1 = XEXP (x, 1);
	RTX_CODE code0 = GET_CODE (orig_op0);
	RTX_CODE code1 = GET_CODE (orig_op1);
	rtx op0 = orig_op0;
	rtx op1 = orig_op1;
	rtx *locI = NULL;
	rtx *locB = NULL;
	enum rtx_code index_code = SCRATCH;

	if (GET_CODE (op0) == SUBREG)
	  {
	    op0 = SUBREG_REG (op0);
	    code0 = GET_CODE (op0);
	  }

	if (GET_CODE (op1) == SUBREG)
	  {
	    op1 = SUBREG_REG (op1);
	    code1 = GET_CODE (op1);
	  }

	if (code0 == MULT || code0 == SIGN_EXTEND || code0 == TRUNCATE
	    || code0 == ZERO_EXTEND || code1 == MEM)
	  {
	    locI = &XEXP (x, 0);
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (*locI);
	  }
	else if (code1 == MULT || code1 == SIGN_EXTEND || code1 == TRUNCATE
		 || code1 == ZERO_EXTEND || code0 == MEM)
	  {
	    locI = &XEXP (x, 1);
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (*locI);
	  }
	else if (code0 == CONST_INT || code0 == CONST
		 || code0 == SYMBOL_REF || code0 == LABEL_REF)
	  {
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (XEXP (x, 0));
	  }
	else if (code1 == CONST_INT || code1 == CONST
		 || code1 == SYMBOL_REF || code1 == LABEL_REF)
	  {
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (XEXP (x, 1));
	  }
	else if (code0 == REG && code1 == REG)
	  {
	    int index_op;
	    unsigned regno0 = REGNO (op0), regno1 = REGNO (op1);

	    if (REGNO_OK_FOR_INDEX_P (regno1)
		&& regno_ok_for_base_p (regno0, mode, as, PLUS, REG))
	      index_op = 1;
	    else if (REGNO_OK_FOR_INDEX_P (regno0)
		     && regno_ok_for_base_p (regno1, mode, as, PLUS, REG))
	      index_op = 0;
	    else if (regno_ok_for_base_p (regno0, mode, as, PLUS, REG)
		     || REGNO_OK_FOR_INDEX_P (regno1))
	      index_op = 1;
	    else if (regno_ok_for_base_p (regno1, mode, as, PLUS, REG))
	      index_op = 0;
	    else
	      index_op = 1;

	    locI = &XEXP (x, index_op);
	    locB = &XEXP (x, !index_op);
	    index_code = GET_CODE (*locI);
	  }
	else if (code0 == REG)
	  {
	    locI = &XEXP (x, 0);
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (*locI);
	  }
	else if (code1 == REG)
	  {
	    locI = &XEXP (x, 1);
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (*locI);
	  }

	if (locI)
	  {
	    reg_class iclass = DEBUG_INSN_P (insn) ? ALL_REGS : INDEX_REG_CLASS;
	    scan_rtx_address (insn, locI, iclass, action, mode, as);
	  }
	if (locB)
	  {
	    reg_class bclass = base_reg_class_for_rename (insn, mode, as, PLUS,
							  index_code);
	    scan_rtx_address (insn, locB, bclass, action, mode, as);
	  }
	return;
      }

    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
    case PRE_INC:
    case PRE_DEC:
    case PRE_MODIFY:
      /* If the target doesn't claim to handle autoinc, this must be
	 something special, like a stack push.  Kill this chain.  */
      if (!AUTO_INC_DEC)
	action = mark_all_read;

      break;

    case MEM:
      {
	reg_class bclass = base_reg_class_for_rename (insn, GET_MODE (x),
						      MEM_ADDR_SPACE (x),
						      MEM, SCRATCH);
	scan_rtx_address (insn, &XEXP (x, 0), bclass, action, GET_MODE (x),
			  MEM_ADDR_SPACE (x));
      }
      return;

    case REG:
      scan_rtx_reg (insn, loc, cl, action, OP_IN);
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan_rtx_address (insn, &XEXP (x, i), cl, action, mode, as);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scan_rtx_address (insn, &XVECEXP (x, i, j), cl, action, mode, as);
    }
}

static void
scan_rtx (rtx_insn *insn, rtx *loc, enum reg_class cl, enum scan_actions action,
	  enum op_type type)
{
  const char *fmt;
  rtx x = *loc;
  int i, j;

  enum rtx_code code = GET_CODE (x);
  switch (code)
    {
    case CONST:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
    case CC0:
    case PC:
      return;

    case REG:
      scan_rtx_reg (insn, loc, cl, action, type);
      return;

    case MEM:
      {
	reg_class bclass = base_reg_class_for_rename (insn, GET_MODE (x),
						      MEM_ADDR_SPACE (x),
						      MEM, SCRATCH);

	scan_rtx_address (insn, &XEXP (x, 0), bclass, action, GET_MODE (x),
			  MEM_ADDR_SPACE (x));
      }
      return;

    case SET:
      scan_rtx (insn, &SET_SRC (x), cl, action, OP_IN);
      scan_rtx (insn, &SET_DEST (x), cl, action,
		(GET_CODE (PATTERN (insn)) == COND_EXEC
		 && verify_reg_tracked (SET_DEST (x))) ? OP_INOUT : OP_OUT);
      return;

    case STRICT_LOW_PART:
      scan_rtx (insn, &XEXP (x, 0), cl, action,
		verify_reg_tracked (XEXP (x, 0)) ? OP_INOUT : OP_OUT);
      return;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      scan_rtx (insn, &XEXP (x, 0), cl, action,
		(type == OP_IN ? OP_IN :
		 verify_reg_tracked (XEXP (x, 0)) ? OP_INOUT : OP_OUT));
      scan_rtx (insn, &XEXP (x, 1), cl, action, OP_IN);
      scan_rtx (insn, &XEXP (x, 2), cl, action, OP_IN);
      return;

    case POST_INC:
    case PRE_INC:
    case POST_DEC:
    case PRE_DEC:
    case POST_MODIFY:
    case PRE_MODIFY:
      /* Should only happen inside MEM.  */
      gcc_unreachable ();

    case CLOBBER:
      scan_rtx (insn, &SET_DEST (x), cl, action,
		(GET_CODE (PATTERN (insn)) == COND_EXEC
		 && verify_reg_tracked (SET_DEST (x))) ? OP_INOUT : OP_OUT);
      return;

    case EXPR_LIST:
      scan_rtx (insn, &XEXP (x, 0), cl, action, type);
      if (XEXP (x, 1))
	scan_rtx (insn, &XEXP (x, 1), cl, action, type);
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan_rtx (insn, &XEXP (x, i), cl, action, type);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scan_rtx (insn, &XVECEXP (x, i, j), cl, action, type);
    }
}

/* Hide operands of the current insn (of which there are N_OPS) by
   substituting pc for them.
   Previous values are stored in the OLD_OPERANDS and OLD_DUPS.
   For every bit set in DO_NOT_HIDE, we leave the operand alone.
   If INOUT_AND_EC_ONLY is set, we only do this for OP_INOUT type operands
   and earlyclobbers.  */

static void
hide_operands (int n_ops, rtx *old_operands, rtx *old_dups,
	       unsigned HOST_WIDE_INT do_not_hide, bool inout_and_ec_only)
{
  int i;
  const operand_alternative *op_alt = which_op_alt ();
  for (i = 0; i < n_ops; i++)
    {
      old_operands[i] = recog_data.operand[i];
      /* Don't squash match_operator or match_parallel here, since
	 we don't know that all of the contained registers are
	 reachable by proper operands.  */
      if (recog_data.constraints[i][0] == '\0')
	continue;
      if (do_not_hide & (1 << i))
	continue;
      if (!inout_and_ec_only || recog_data.operand_type[i] == OP_INOUT
	  || op_alt[i].earlyclobber)
	*recog_data.operand_loc[i] = pc_rtx;
    }
  for (i = 0; i < recog_data.n_dups; i++)
    {
      int opn = recog_data.dup_num[i];
      old_dups[i] = *recog_data.dup_loc[i];
      if (do_not_hide & (1 << opn))
	continue;
      if (!inout_and_ec_only || recog_data.operand_type[opn] == OP_INOUT
	  || op_alt[opn].earlyclobber)
	*recog_data.dup_loc[i] = pc_rtx;
    }
}

/* Undo the substitution performed by hide_operands.  INSN is the insn we
   are processing; the arguments are the same as in hide_operands.  */

static void
restore_operands (rtx_insn *insn, int n_ops, rtx *old_operands, rtx *old_dups)
{
  int i;
  for (i = 0; i < recog_data.n_dups; i++)
    *recog_data.dup_loc[i] = old_dups[i];
  for (i = 0; i < n_ops; i++)
    *recog_data.operand_loc[i] = old_operands[i];
  if (recog_data.n_dups)
    df_insn_rescan (insn);
}

/* For each output operand of INSN, call scan_rtx to create a new
   open chain.  Do this only for normal or earlyclobber outputs,
   depending on EARLYCLOBBER.  If INSN_INFO is nonnull, use it to
   record information about the operands in the insn.  */

static void
record_out_operands (rtx_insn *insn, bool earlyclobber, insn_rr_info *insn_info)
{
  int n_ops = recog_data.n_operands;
  const operand_alternative *op_alt = which_op_alt ();

  int i;

  for (i = 0; i < n_ops + recog_data.n_dups; i++)
    {
      int opn = i < n_ops ? i : recog_data.dup_num[i - n_ops];
      rtx *loc = (i < n_ops
		  ? recog_data.operand_loc[opn]
		  : recog_data.dup_loc[i - n_ops]);
      rtx op = *loc;
      enum reg_class cl = alternative_class (op_alt, opn);

      class du_head *prev_open;

      if (recog_data.operand_type[opn] != OP_OUT
	  || op_alt[opn].earlyclobber != earlyclobber)
	continue;

      if (insn_info)
	cur_operand = insn_info->op_info + i;

      prev_open = open_chains;
      if (earlyclobber)
	scan_rtx (insn, loc, cl, terminate_write, OP_OUT);
      scan_rtx (insn, loc, cl, mark_write, OP_OUT);

      /* ??? Many targets have output constraints on the SET_DEST
	 of a call insn, which is stupid, since these are certainly
	 ABI defined hard registers.  For these, and for asm operands
	 that originally referenced hard registers, we must record that
	 the chain cannot be renamed.  */
      if (CALL_P (insn)
	  || (asm_noperands (PATTERN (insn)) > 0
	      && REG_P (op)
	      && REGNO (op) == ORIGINAL_REGNO (op)))
	{
	  if (prev_open != open_chains)
	    open_chains->cannot_rename = 1;
	}
    }
  cur_operand = NULL;
}

/* Build def/use chain.  */

static bool
build_def_use (basic_block bb)
{
  rtx_insn *insn;
  unsigned HOST_WIDE_INT untracked_operands;

  fail_current_block = false;

  for (insn = BB_HEAD (bb); ; insn = NEXT_INSN (insn))
    {
      if (NONDEBUG_INSN_P (insn))
	{
	  int n_ops;
	  rtx note;
	  rtx old_operands[MAX_RECOG_OPERANDS];
	  rtx old_dups[MAX_DUP_OPERANDS];
	  int i;
	  int predicated;
	  enum rtx_code set_code = SET;
	  enum rtx_code clobber_code = CLOBBER;
	  insn_rr_info *insn_info = NULL;
	  terminated_this_insn = NULL;

	  /* Process the insn, determining its effect on the def-use
	     chains and live hard registers.  We perform the following
	     steps with the register references in the insn, simulating
	     its effect:
	     (1) Deal with earlyclobber operands and CLOBBERs of non-operands
	         by creating chains and marking hard regs live.
	     (2) Any read outside an operand causes any chain it overlaps
	         with to be marked unrenamable.
	     (3) Any read inside an operand is added if there's already
	         an open chain for it.
	     (4) For any REG_DEAD note we find, close open chains that
	         overlap it.
	     (5) For any non-earlyclobber write we find, close open chains
	         that overlap it.
	     (6) For any non-earlyclobber write we find in an operand, make
	         a new chain or mark the hard register as live.
	     (7) For any REG_UNUSED, close any chains we just opened.
	     (8) For any REG_CFA_RESTORE or REG_CFA_REGISTER, kill any chain
	         containing its dest.

	     We cannot deal with situations where we track a reg in one mode
	     and see a reference in another mode; these will cause the chain
	     to be marked unrenamable or even cause us to abort the entire
	     basic block.  */

	  extract_constrain_insn (insn);
	  preprocess_constraints (insn);
	  const operand_alternative *op_alt = which_op_alt ();
	  n_ops = recog_data.n_operands;
	  untracked_operands = 0;

	  if (insn_rr.exists ())
	    {
	      insn_info = &insn_rr[INSN_UID (insn)];
	      insn_info->op_info = XOBNEWVEC (&rename_obstack, operand_rr_info,
					      recog_data.n_operands);
	      memset (insn_info->op_info, 0,
		      sizeof (operand_rr_info) * recog_data.n_operands);
	    }

	  /* Simplify the code below by promoting OP_OUT to OP_INOUT in
	     predicated instructions, but only for register operands
	     that are already tracked, so that we can create a chain
	     when the first SET makes a register live.  */

	  predicated = GET_CODE (PATTERN (insn)) == COND_EXEC;
	  for (i = 0; i < n_ops; ++i)
	    {
	      rtx op = recog_data.operand[i];
	      int matches = op_alt[i].matches;
	      if (matches >= 0 || op_alt[i].matched >= 0
	          || (predicated && recog_data.operand_type[i] == OP_OUT))
		{
		  recog_data.operand_type[i] = OP_INOUT;
		  /* A special case to deal with instruction patterns that
		     have matching operands with different modes.  If we're
		     not already tracking such a reg, we won't start here,
		     and we must instead make sure to make the operand visible
		     to the machinery that tracks hard registers.  */
		  machine_mode i_mode = recog_data.operand_mode[i];
		  if (matches >= 0)
		    {
		      machine_mode matches_mode
			= recog_data.operand_mode[matches];

		      if (maybe_ne (GET_MODE_SIZE (i_mode),
				    GET_MODE_SIZE (matches_mode))
			  && !verify_reg_in_set (op, &live_in_chains))
			{
			  untracked_operands |= 1 << i;
			  untracked_operands |= 1 << matches;
			}
		    }
		}
#ifdef STACK_REGS
	      if (regstack_completed
		  && REG_P (op)
		  && IN_RANGE (REGNO (op), FIRST_STACK_REG, LAST_STACK_REG))
		untracked_operands |= 1 << i;
#endif
	      /* If there's an in-out operand with a register that is not
		 being tracked at all yet, open a chain.  */
	      if (recog_data.operand_type[i] == OP_INOUT
		  && !(untracked_operands & (1 << i))
		  && REG_P (op)
		  && !verify_reg_tracked (op))
		create_new_chain (REGNO (op), REG_NREGS (op), NULL, NULL,
				  NO_REGS);
	    }

	  if (fail_current_block)
	    break;

	  /* Step 1a: Mark hard registers that are clobbered in this insn,
	     outside an operand, as live.  */
	  hide_operands (n_ops, old_operands, old_dups, untracked_operands,
			 false);
	  note_stores (insn, note_sets_clobbers, &clobber_code);
	  restore_operands (insn, n_ops, old_operands, old_dups);

	  /* Step 1b: Begin new chains for earlyclobbered writes inside
	     operands.  */
	  record_out_operands (insn, true, insn_info);

	  /* Step 2: Mark chains for which we have reads outside operands
	     as unrenamable.
	     We do this by munging all operands into PC, and closing
	     everything remaining.  */

	  hide_operands (n_ops, old_operands, old_dups, untracked_operands,
			 false);
	  scan_rtx (insn, &PATTERN (insn), NO_REGS, mark_all_read, OP_IN);
	  restore_operands (insn, n_ops, old_operands, old_dups);

	  /* Step 2B: Can't rename function call argument registers.  */
	  if (CALL_P (insn) && CALL_INSN_FUNCTION_USAGE (insn))
	    scan_rtx (insn, &CALL_INSN_FUNCTION_USAGE (insn),
		      NO_REGS, mark_all_read, OP_IN);

	  /* Step 2C: Can't rename asm operands that were originally
	     hard registers.  */
	  if (asm_noperands (PATTERN (insn)) > 0)
	    for (i = 0; i < n_ops; i++)
	      {
		rtx *loc = recog_data.operand_loc[i];
		rtx op = *loc;

		if (REG_P (op)
		    && REGNO (op) == ORIGINAL_REGNO (op)
		    && (recog_data.operand_type[i] == OP_IN
			|| recog_data.operand_type[i] == OP_INOUT))
		  scan_rtx (insn, loc, NO_REGS, mark_all_read, OP_IN);
	      }

	  /* Step 3: Append to chains for reads inside operands.  */
	  for (i = 0; i < n_ops + recog_data.n_dups; i++)
	    {
	      int opn = i < n_ops ? i : recog_data.dup_num[i - n_ops];
	      rtx *loc = (i < n_ops
			  ? recog_data.operand_loc[opn]
			  : recog_data.dup_loc[i - n_ops]);
	      enum reg_class cl = alternative_class (op_alt, opn);
	      enum op_type type = recog_data.operand_type[opn];

	      /* Don't scan match_operand here, since we've no reg class
		 information to pass down.  Any operands that we could
		 substitute in will be represented elsewhere.  */
	      if (recog_data.constraints[opn][0] == '\0'
		  || untracked_operands & (1 << opn))
		continue;

	      if (insn_info)
		cur_operand = i == opn ? insn_info->op_info + i : NULL;
	      if (op_alt[opn].is_address)
		scan_rtx_address (insn, loc, cl, mark_read,
				  VOIDmode, ADDR_SPACE_GENERIC);
	      else
		scan_rtx (insn, loc, cl, mark_read, type);
	    }
	  cur_operand = NULL;

	  /* Step 3B: Record updates for regs in REG_INC notes, and
	     source regs in REG_FRAME_RELATED_EXPR notes.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_INC
		|| REG_NOTE_KIND (note) == REG_FRAME_RELATED_EXPR)
	      scan_rtx (insn, &XEXP (note, 0), ALL_REGS, mark_read,
			OP_INOUT);

	  /* Step 4: Close chains for registers that die here, unless
	     the register is mentioned in a REG_UNUSED note.  In that
	     case we keep the chain open until step #7 below to ensure
	     it conflicts with other output operands of this insn.
	     See PR 52573.  Arguably the insn should not have both
	     notes; it has proven difficult to fix that without
	     other undesirable side effects.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD
		&& !find_regno_note (insn, REG_UNUSED, REGNO (XEXP (note, 0))))
	      {
		remove_from_hard_reg_set (&live_hard_regs,
					  GET_MODE (XEXP (note, 0)),
					  REGNO (XEXP (note, 0)));
		scan_rtx (insn, &XEXP (note, 0), NO_REGS, terminate_dead,
			  OP_IN);
	      }

	  /* Step 4B: If this is a call, any chain live at this point
	     requires a caller-saved reg.  */
	  if (CALL_P (insn))
	    {
	      function_abi callee_abi = insn_callee_abi (insn);
	      class du_head *p;
	      for (p = open_chains; p; p = p->next_chain)
		{
		  p->call_abis |= (1 << callee_abi.id ());
		  p->call_clobber_mask
		    |= callee_abi.full_and_partial_reg_clobbers ();
		  p->hard_conflicts |= callee_abi.full_reg_clobbers ();
		}
	    }

	  /* Step 5: Close open chains that overlap writes.  Similar to
	     step 2, we hide in-out operands, since we do not want to
	     close these chains.  We also hide earlyclobber operands,
	     since we've opened chains for them in step 1, and earlier
	     chains they would overlap with must have been closed at
	     the previous insn at the latest, as such operands cannot
	     possibly overlap with any input operands.  */

	  hide_operands (n_ops, old_operands, old_dups, untracked_operands,
			 true);
	  scan_rtx (insn, &PATTERN (insn), NO_REGS, terminate_write, OP_IN);
	  restore_operands (insn, n_ops, old_operands, old_dups);

	  /* Step 6a: Mark hard registers that are set in this insn,
	     outside an operand, as live.  */
	  hide_operands (n_ops, old_operands, old_dups, untracked_operands,
			 false);
	  note_stores (insn, note_sets_clobbers, &set_code);
	  restore_operands (insn, n_ops, old_operands, old_dups);

	  /* Step 6b: Begin new chains for writes inside operands.  */
	  record_out_operands (insn, false, insn_info);

	  /* Step 6c: Record destination regs in REG_FRAME_RELATED_EXPR
	     notes for update.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_FRAME_RELATED_EXPR)
	      scan_rtx (insn, &XEXP (note, 0), ALL_REGS, mark_access,
			OP_INOUT);

	  /* Step 7: Close chains for registers that were never
	     really used here.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_UNUSED)
	      {
		remove_from_hard_reg_set (&live_hard_regs,
					  GET_MODE (XEXP (note, 0)),
					  REGNO (XEXP (note, 0)));
		scan_rtx (insn, &XEXP (note, 0), NO_REGS, terminate_dead,
			  OP_IN);
	      }

	  /* Step 8: Kill the chains involving register restores.  Those
	     should restore _that_ register.  Similar for REG_CFA_REGISTER.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_CFA_RESTORE
		|| REG_NOTE_KIND (note) == REG_CFA_REGISTER)
	      {
		rtx *x = &XEXP (note, 0);
		if (!*x)
		  x = &PATTERN (insn);
		if (GET_CODE (*x) == PARALLEL)
		  x = &XVECEXP (*x, 0, 0);
		if (GET_CODE (*x) == SET)
		  x = &SET_DEST (*x);
		scan_rtx (insn, x, NO_REGS, mark_all_read, OP_IN);
	      }
	}
      else if (DEBUG_BIND_INSN_P (insn)
	       && !VAR_LOC_UNKNOWN_P (INSN_VAR_LOCATION_LOC (insn)))
	{
	  scan_rtx (insn, &INSN_VAR_LOCATION_LOC (insn),
		    ALL_REGS, mark_read, OP_IN);
	}
      if (insn == BB_END (bb))
	break;
    }

  if (fail_current_block)
    return false;

  return true;
}

/* Initialize the register renamer.  If INSN_INFO is true, ensure that
   insn_rr is nonnull.  */
void
regrename_init (bool insn_info)
{
  gcc_obstack_init (&rename_obstack);
  insn_rr.create (0);
  if (insn_info)
    insn_rr.safe_grow_cleared (get_max_uid ());
}

/* Free all global data used by the register renamer.  */
void
regrename_finish (void)
{
  insn_rr.release ();
  free_chain_data ();
  obstack_free (&rename_obstack, NULL);
}

/* Perform register renaming on the current function.  */

static unsigned int
regrename_optimize (void)
{
  df_set_flags (DF_LR_RUN_DCE);
  df_note_add_problem ();
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  regrename_init (false);

  regrename_analyze (NULL);

  rename_chains ();

  regrename_finish ();

  return 0;
}

namespace {

const pass_data pass_data_regrename =
{
  RTL_PASS, /* type */
  "rnreg", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_RENAME_REGISTERS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_regrename : public rtl_opt_pass
{
public:
  pass_regrename (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_regrename, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (optimize > 0 && (flag_rename_registers));
    }

  virtual unsigned int execute (function *) { return regrename_optimize (); }

}; // class pass_regrename

} // anon namespace

rtl_opt_pass *
make_pass_regrename (gcc::context *ctxt)
{
  return new pass_regrename (ctxt);
}
