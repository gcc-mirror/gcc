/* Loop manipulation code for GNU compiler.
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "output.h"

static basic_block create_preheader	PARAMS ((struct loop *, dominance_info,
						int));

/* Creates a pre-header for a LOOP.  Returns newly created block.  Unless
   CP_SIMPLE_PREHEADERS is set in FLAGS, we only force LOOP to have single
   entry; otherwise we also force preheader block to have only one successor.
   */
static basic_block
create_preheader (loop, dom, flags)
     struct loop *loop;
     dominance_info dom;
     int flags;
{
  edge e, fallthru;
  basic_block dummy;
  basic_block jump, src;
  struct loop *cloop, *ploop;
  int nentry = 0;
  rtx insn;

  cloop = loop->outer;

  for (e = loop->header->pred; e; e = e->pred_next)
    {
      if (e->src == loop->latch)
	continue;
      nentry++;
    }
  if (!nentry)
    abort ();
  if (nentry == 1)
    {
      for (e = loop->header->pred; e->src == loop->latch; e = e->pred_next);
      if (!(flags & CP_SIMPLE_PREHEADERS)
	  || !e->src->succ->succ_next)
	return NULL;
    }

  insn = first_insn_after_basic_block_note (loop->header);
  if (insn)
    insn = PREV_INSN (insn);
  else
    insn = get_last_insn ();
  if (insn == loop->header->end)
    {
      /* Split_block would not split block after its end.  */
      emit_note_after (NOTE_INSN_DELETED, insn);
    }
  if (flags & CP_INSIDE_CFGLAYOUT)
    fallthru = cfg_layout_split_block (loop->header, insn);
  else
    fallthru = split_block (loop->header, insn);
  dummy = fallthru->src;
  loop->header = fallthru->dest;

  /* The header could be a latch of some superloop(s); due to design of
     split_block, it would now move to fallthru->dest.  */
  for (ploop = loop; ploop; ploop = ploop->outer)
    if (ploop->latch == dummy)
      ploop->latch = fallthru->dest;

  add_to_dominance_info (dom, fallthru->dest);
  
  /* Redirect edges. */
  for (e = dummy->pred; e; e = e->pred_next)
    {
      src = e->src;
      if (src == loop->latch)
	break;
    }
  if (!e)
    abort ();

  dummy->frequency -= EDGE_FREQUENCY (e);
  dummy->count -= e->count;
  fallthru->count -= e->count;
  if (flags & CP_INSIDE_CFGLAYOUT)
    cfg_layout_redirect_edge (e, loop->header);
  else
    {
      jump = redirect_edge_and_branch_force (e, loop->header);
      if (jump)
	{
	  add_to_dominance_info (dom, jump);
	  set_immediate_dominator (dom, jump, src);
	  add_bb_to_loop (jump, loop);
	  loop->latch = jump;
	}
    }

  /* Update structures.  */
  redirect_immediate_dominators (dom, dummy, loop->header);
  set_immediate_dominator (dom, loop->header, dummy);
  loop->header->loop_father = loop;
  add_bb_to_loop (dummy, cloop);
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Created preheader block for loop %i\n",
	     loop->num);

  return dummy;
}

/* Create preheaders for each loop; for meaning of flags see
   create_preheader.  */
void
create_preheaders (loops, flags)
     struct loops *loops;
     int flags;
{
  unsigned i;
  for (i = 1; i < loops->num; i++)
    create_preheader (loops->parray[i], loops->cfg.dom, flags);
  loops->state |= LOOPS_HAVE_PREHEADERS;
}

/* Forces all loop latches to have only single successor.  */
void
force_single_succ_latches (loops)
     struct loops *loops;
{
  unsigned i;
  struct loop *loop;
  edge e;

  for (i = 1; i < loops->num; i++)
    {
      loop = loops->parray[i];
      if (!loop->latch->succ->succ_next)
	continue;
 
      for (e = loop->header->pred; e->src != loop->latch; e = e->pred_next);
	loop_split_edge_with (e, NULL_RTX, loops);
    }
  loops->state |= LOOPS_HAVE_SIMPLE_LATCHES;
}

/* A quite stupid function to put INSNS on E. They are supposed to form
   just one basic block. Jumps out are not handled, so cfg do not have to
   be ok after this function.  */
basic_block
loop_split_edge_with (e, insns, loops)
     edge e;
     rtx insns;
     struct loops *loops;
{
  basic_block src, dest, new_bb;
  struct loop *loop_c;
  edge new_e;
  
  src = e->src;
  dest = e->dest;

  loop_c = find_common_loop (src->loop_father, dest->loop_father);

  /* Create basic block for it.  */

  new_bb = create_basic_block (NULL_RTX, NULL_RTX, EXIT_BLOCK_PTR->prev_bb);
  add_to_dominance_info (loops->cfg.dom, new_bb);
  add_bb_to_loop (new_bb, loop_c);
  new_bb->flags = insns ? BB_SUPERBLOCK : 0;
  if (src->flags & BB_IRREDUCIBLE_LOOP)
    {
      /* We expect simple preheaders here.  */
      if ((dest->flags & BB_IRREDUCIBLE_LOOP)
          || dest->loop_father->header == dest)
        new_bb->flags |= BB_IRREDUCIBLE_LOOP;
    }

  new_e = make_edge (new_bb, dest, EDGE_FALLTHRU);
  new_e->probability = REG_BR_PROB_BASE;
  new_e->count = e->count;

  new_bb->count = e->count;
  new_bb->frequency = EDGE_FREQUENCY (e);
  cfg_layout_redirect_edge (e, new_bb);

  alloc_aux_for_block (new_bb, sizeof (struct reorder_block_def));
  if (insns)
    {
      start_sequence ();
      emit_insn (insns);
      insns = get_insns ();
      end_sequence ();
      emit_insn_after (insns, new_bb->end);
    }

  set_immediate_dominator (loops->cfg.dom, new_bb, src);
  set_immediate_dominator (loops->cfg.dom, dest,
    recount_dominator (loops->cfg.dom, dest));

  if (dest->loop_father->latch == src)
    dest->loop_father->latch = new_bb;
  
  return new_bb;
}
