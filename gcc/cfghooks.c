/* Hooks for cfg representation specific functions.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <s.pop@laposte.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "basic-block.h"
#include "timevar.h"
#include "toplev.h"

/* A pointer to one of the hooks containers.  */
static struct cfg_hooks *cfg_hooks;

/* Initialization of functions specific to the rtl IR.  */
void
rtl_register_cfg_hooks (void)
{
  cfg_hooks = &rtl_cfg_hooks;
}

/* Initialization of functions specific to the rtl IR.  */
void
cfg_layout_rtl_register_cfg_hooks (void)
{
  cfg_hooks = &cfg_layout_rtl_cfg_hooks;
}

/* Verify the CFG consistency.

   Currently it does following: checks edge and basic block list correctness
   and calls into IL dependent checking then.  */

void
verify_flow_info (void)
{
  size_t *edge_checksum;
  int num_bb_notes, err = 0;
  basic_block bb, last_bb_seen;
  basic_block *last_visited;

  timevar_push (TV_CFG_VERIFY);
  last_visited = xcalloc (last_basic_block + 2, sizeof (basic_block));
  edge_checksum = xcalloc (last_basic_block + 2, sizeof (size_t));

  /* Check bb chain & numbers.  */
  last_bb_seen = ENTRY_BLOCK_PTR;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR->next_bb, NULL, next_bb)
    {
      if (bb != EXIT_BLOCK_PTR
	  && bb != BASIC_BLOCK (bb->index))
	{
	  error ("bb %d on wrong place", bb->index);
	  err = 1;
	}

      if (bb->prev_bb != last_bb_seen)
	{
	  error ("prev_bb of %d should be %d, not %d",
		 bb->index, last_bb_seen->index, bb->prev_bb->index);
	  err = 1;
	}

      last_bb_seen = bb;
    }

  /* Now check the basic blocks (boundaries etc.) */
  FOR_EACH_BB_REVERSE (bb)
    {
      int n_fallthru = 0;
      edge e;

      if (bb->count < 0)
	{
	  error ("verify_flow_info: Wrong count of block %i %i",
	         bb->index, (int)bb->count);
	  err = 1;
	}
      if (bb->frequency < 0)
	{
	  error ("verify_flow_info: Wrong frequency of block %i %i",
	         bb->index, bb->frequency);
	  err = 1;
	}
      for (e = bb->succ; e; e = e->succ_next)
	{
	  if (last_visited [e->dest->index + 2] == bb)
	    {
	      error ("verify_flow_info: Duplicate edge %i->%i",
		     e->src->index, e->dest->index);
	      err = 1;
	    }
	  if (e->probability < 0 || e->probability > REG_BR_PROB_BASE)
	    {
	      error ("verify_flow_info: Wrong probability of edge %i->%i %i",
		     e->src->index, e->dest->index, e->probability);
	      err = 1;
	    }
	  if (e->count < 0)
	    {
	      error ("verify_flow_info: Wrong count of edge %i->%i %i",
		     e->src->index, e->dest->index, (int)e->count);
	      err = 1;
	    }

	  last_visited [e->dest->index + 2] = bb;

	  if (e->flags & EDGE_FALLTHRU)
	    n_fallthru++;

	  if (e->src != bb)
	    {
	      error ("verify_flow_info: Basic block %d succ edge is corrupted",
		     bb->index);
	      fprintf (stderr, "Predecessor: ");
	      dump_edge_info (stderr, e, 0);
	      fprintf (stderr, "\nSuccessor: ");
	      dump_edge_info (stderr, e, 1);
	      fprintf (stderr, "\n");
	      err = 1;
	    }

	  edge_checksum[e->dest->index + 2] += (size_t) e;
	}
      if (n_fallthru > 1)
	{
	  error ("Wrong amount of branch edges after unconditional jump %i", bb->index);
	  err = 1;
	}

      for (e = bb->pred; e; e = e->pred_next)
	{
	  if (e->dest != bb)
	    {
	      error ("basic block %d pred edge is corrupted", bb->index);
	      fputs ("Predecessor: ", stderr);
	      dump_edge_info (stderr, e, 0);
	      fputs ("\nSuccessor: ", stderr);
	      dump_edge_info (stderr, e, 1);
	      fputc ('\n', stderr);
	      err = 1;
	    }
	  edge_checksum[e->dest->index + 2] -= (size_t) e;
	}
    }

  /* Complete edge checksumming for ENTRY and EXIT.  */
  {
    edge e;

    for (e = ENTRY_BLOCK_PTR->succ; e ; e = e->succ_next)
      edge_checksum[e->dest->index + 2] += (size_t) e;

    for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
      edge_checksum[e->dest->index + 2] -= (size_t) e;
  }

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    if (edge_checksum[bb->index + 2])
      {
	error ("basic block %i edge lists are corrupted", bb->index);
	err = 1;
      }

  num_bb_notes = 0;
  last_bb_seen = ENTRY_BLOCK_PTR;

  /* Clean up.  */
  free (last_visited);
  free (edge_checksum);

  if (cfg_hooks->verify_flow_info)
    err |= cfg_hooks->verify_flow_info ();
  if (err)
    internal_error ("verify_flow_info failed");
  timevar_pop (TV_CFG_VERIFY);
}

/* Print out one basic block.  This function takes care of the purely
   graph related information.  The cfg hook for the active representation
   should dump representation-specific information.  */

void
dump_bb (basic_block bb, FILE *outf, int indent)
{
  edge e;
  char *s_indent;
 
  s_indent = (char *) alloca ((size_t) indent + 1);
  memset ((void *) s_indent, ' ', (size_t) indent);
  s_indent[indent] = '\0';

  fprintf (outf, ";;%s basic block %d, loop depth %d, count ",
	   s_indent, bb->index, bb->loop_depth);
  fprintf (outf, HOST_WIDEST_INT_PRINT_DEC, (HOST_WIDEST_INT) bb->count);
  putc ('\n', outf);

  fprintf (outf, ";;%s prev block ", s_indent);
  if (bb->prev_bb)
    fprintf (outf, "%d, ", bb->prev_bb->index);
  else
    fprintf (outf, "(nil), ");
  fprintf (outf, "next block ");
  if (bb->next_bb)
    fprintf (outf, "%d", bb->next_bb->index);
  else
    fprintf (outf, "(nil)");
  putc ('\n', outf);

  fprintf (outf, ";;%s pred:      ", s_indent);
  for (e = bb->pred; e; e = e->pred_next)
    dump_edge_info (outf, e, 0);
  putc ('\n', outf);

  fprintf (outf, ";;%s succ:      ", s_indent);
  for (e = bb->succ; e; e = e->succ_next)
    dump_edge_info (outf, e, 1);
  putc ('\n', outf);

  if (cfg_hooks->dump_bb)
    cfg_hooks->dump_bb (bb, outf, indent);
}

/* Redirect edge E to the given basic block DEST and update underlying program
   representation.  Returns edge representing redirected branch (that may not
   be equivalent to E in the case of duplicate edges being removed) or NULL
   if edge is not easily redirectable for whatever reason.  */

bool
redirect_edge_and_branch (edge e, basic_block dest)
{
  bool ret;

  if (!cfg_hooks->redirect_edge_and_branch)
    internal_error ("%s does not support redirect_edge_and_branch.",
		    cfg_hooks->name);

  ret = cfg_hooks->redirect_edge_and_branch (e, dest);

  return ret;
}

/* Redirect the edge E to basic block DEST even if it requires creating
   of a new basic block; then it returns the newly created basic block.
   Aborts when redirection is impossible.  */

basic_block
redirect_edge_and_branch_force (edge e, basic_block dest)
{
  basic_block ret;

  if (!cfg_hooks->redirect_edge_and_branch_force)
    internal_error ("%s does not support redirect_edge_and_branch_force.",
		    cfg_hooks->name);

  ret = cfg_hooks->redirect_edge_and_branch_force (e, dest);

  return ret;
}

/* Splits basic block BB after the specified instruction I (but at least after
   the labels).  If I is NULL, splits just after labels.  The newly created edge
   is returned.  The new basic block is created just after the old one.  */

edge
split_block (basic_block bb, void *i)
{
  basic_block new_bb;
  edge e;

  if (!cfg_hooks->split_block)
    internal_error ("%s does not support split_block.", cfg_hooks->name);

  new_bb = cfg_hooks->split_block (bb, i);
  if (!new_bb)
    return NULL;

  new_bb->count = bb->count;
  new_bb->frequency = bb->frequency;
  new_bb->loop_depth = bb->loop_depth;

  if (dom_computed[CDI_DOMINATORS] >= DOM_CONS_OK)
    {
      redirect_immediate_dominators (CDI_DOMINATORS, bb, new_bb);
      set_immediate_dominator (CDI_DOMINATORS, new_bb, bb);
    }

  e = make_edge (bb, new_bb, EDGE_FALLTHRU);
  e->probability = REG_BR_PROB_BASE;
  e->count = bb->count;

  return e;
}

/* Splits block BB just after labels.  The newly created edge is returned.  */

edge
split_block_after_labels (basic_block bb)
{
  return split_block (bb, NULL);
}

/* Moves block BB immediatelly after block AFTER.  Returns false if the
   movement was impossible.  */

bool
move_block_after (basic_block bb, basic_block after)
{
  bool ret;

  if (!cfg_hooks->move_block_after)
    internal_error ("%s does not support move_block_after.", cfg_hooks->name);

  ret = cfg_hooks->move_block_after (bb, after);

  return ret;
}

/* Deletes the basic block BB.  */

void
delete_basic_block (basic_block bb)
{
  if (!cfg_hooks->delete_basic_block)
    internal_error ("%s does not support delete_basic_block.", cfg_hooks->name);

  cfg_hooks->delete_basic_block (bb);

  /* Remove the edges into and out of this block.  Note that there may
     indeed be edges in, if we are removing an unreachable loop.  */
  while (bb->pred != NULL)
    remove_edge (bb->pred);
  while (bb->succ != NULL)
    remove_edge (bb->succ);

  bb->pred = NULL;
  bb->succ = NULL;

  if (dom_computed[CDI_DOMINATORS])
    delete_from_dominance_info (CDI_DOMINATORS, bb);
  if (dom_computed[CDI_POST_DOMINATORS])
    delete_from_dominance_info (CDI_POST_DOMINATORS, bb);

  /* Remove the basic block from the array.  */
  expunge_block (bb);
}

/* Splits edge E and returns the newly created basic block.  */

basic_block
split_edge (edge e)
{
  basic_block ret;
  gcov_type count = e->count;
  int freq = EDGE_FREQUENCY (e);

  if (!cfg_hooks->split_edge)
    internal_error ("%s does not support split_edge.", cfg_hooks->name);

  ret = cfg_hooks->split_edge (e);
  ret->count = count;
  ret->frequency = freq;
  ret->succ->probability = REG_BR_PROB_BASE;
  ret->succ->count = count;

  if (dom_computed[CDI_DOMINATORS])
    set_immediate_dominator (CDI_DOMINATORS, ret, ret->pred->src);

  if (dom_computed[CDI_DOMINATORS] >= DOM_NO_FAST_QUERY)
    set_immediate_dominator (CDI_DOMINATORS, ret->succ->dest,
			     recount_dominator (CDI_DOMINATORS,
						ret->succ->dest));

  return ret;
}

/* Creates a new basic block just after the basic block AFTER.
   HEAD and END are the first and the last statement belonging
   to the block.  If both are NULL, an empty block is created.  */

basic_block
create_basic_block (void *head, void *end, basic_block after)
{
  basic_block ret;

  if (!cfg_hooks->create_basic_block)
    internal_error ("%s does not support create_basic_block.", cfg_hooks->name);

  ret = cfg_hooks->create_basic_block (head, end, after);

  if (dom_computed[CDI_DOMINATORS])
    add_to_dominance_info (CDI_DOMINATORS, ret);
  if (dom_computed[CDI_POST_DOMINATORS])
    add_to_dominance_info (CDI_POST_DOMINATORS, ret);

  return ret;
}

/* Creates an empty basic block just after basic block AFTER.  */

basic_block
create_empty_bb (basic_block after)
{
  return create_basic_block (NULL, NULL, after);
}

/* Checks whether we may merge blocks BB1 and BB2.  */

bool
can_merge_blocks_p (basic_block bb1, basic_block bb2)
{
  bool ret;

  if (!cfg_hooks->can_merge_blocks_p)
    internal_error ("%s does not support can_merge_blocks_p.", cfg_hooks->name);

  ret = cfg_hooks->can_merge_blocks_p (bb1, bb2);

  return ret;
}

/* Merges basic block B into basic block A.  */

void
merge_blocks (basic_block a, basic_block b)
{
  edge e;

  if (!cfg_hooks->merge_blocks)
    internal_error ("%s does not support merge_blocks.", cfg_hooks->name);

  cfg_hooks->merge_blocks (a, b);

  /* Normally there should only be one successor of A and that is B, but
     partway though the merge of blocks for conditional_execution we'll
     be merging a TEST block with THEN and ELSE successors.  Free the
     whole lot of them and hope the caller knows what they're doing.  */
  while (a->succ)
    remove_edge (a->succ);

  /* Adjust the edges out of B for the new owner.  */
  for (e = b->succ; e; e = e->succ_next)
    e->src = a;
  a->succ = b->succ;
  a->flags |= b->flags;

  /* B hasn't quite yet ceased to exist.  Attempt to prevent mishap.  */
  b->pred = b->succ = NULL;
  a->global_live_at_end = b->global_live_at_end;

  if (dom_computed[CDI_DOMINATORS])
    redirect_immediate_dominators (CDI_DOMINATORS, b, a);

  if (dom_computed[CDI_DOMINATORS])
    delete_from_dominance_info (CDI_DOMINATORS, b);
  if (dom_computed[CDI_POST_DOMINATORS])
    delete_from_dominance_info (CDI_POST_DOMINATORS, b);

  expunge_block (b);
}

/* Split BB into entry part and the rest (the rest is the newly created block).
   Redirect those edges for that REDIRECT_EDGE_P returns true to the entry
   part.  Returns the edge connecting the entry part to the rest.  */

edge
make_forwarder_block (basic_block bb, bool (*redirect_edge_p) (edge),
		      void (*new_bb_cbk) (basic_block))
{
  edge e, next_e, fallthru;
  basic_block dummy, jump;

  if (!cfg_hooks->make_forwarder_block)
    internal_error ("%s does not support make_forwarder_block.",
		    cfg_hooks->name);

  fallthru = split_block_after_labels (bb);
  dummy = fallthru->src;
  bb = fallthru->dest;

  /* Redirect back edges we want to keep.  */
  for (e = dummy->pred; e; e = next_e)
    {
      next_e = e->pred_next;
      if (redirect_edge_p (e))
	continue;

      dummy->frequency -= EDGE_FREQUENCY (e);
      dummy->count -= e->count;
      if (dummy->frequency < 0)
	dummy->frequency = 0;
      if (dummy->count < 0)
	dummy->count = 0;

      jump = redirect_edge_and_branch_force (e, bb);
      if (jump)
	new_bb_cbk (jump);
    }

  if (dom_computed[CDI_DOMINATORS] >= DOM_CONS_OK)
    {
      basic_block doms_to_fix[2];

      doms_to_fix[0] = dummy;
      doms_to_fix[1] = bb;
      iterate_fix_dominators (CDI_DOMINATORS, doms_to_fix, 2);
    }

  cfg_hooks->make_forwarder_block (fallthru);

  return fallthru;
}

void
tidy_fallthru_edge (edge e)
{
  if (cfg_hooks->tidy_fallthru_edge)
    cfg_hooks->tidy_fallthru_edge (e);
}

/* Fix up edges that now fall through, or rather should now fall through
   but previously required a jump around now deleted blocks.  Simplify
   the search by only examining blocks numerically adjacent, since this
   is how find_basic_blocks created them.  */

void
tidy_fallthru_edges (void)
{
  basic_block b, c;

  if (!cfg_hooks->tidy_fallthru_edge)
    return;

  if (ENTRY_BLOCK_PTR->next_bb == EXIT_BLOCK_PTR)
    return;

  FOR_BB_BETWEEN (b, ENTRY_BLOCK_PTR->next_bb, EXIT_BLOCK_PTR->prev_bb, next_bb)
    {
      edge s;

      c = b->next_bb;

      /* We care about simple conditional or unconditional jumps with
	 a single successor.

	 If we had a conditional branch to the next instruction when
	 find_basic_blocks was called, then there will only be one
	 out edge for the block which ended with the conditional
	 branch (since we do not create duplicate edges).

	 Furthermore, the edge will be marked as a fallthru because we
	 merge the flags for the duplicate edges.  So we do not want to
	 check that the edge is not a FALLTHRU edge.  */

      if ((s = b->succ) != NULL
	  && ! (s->flags & EDGE_COMPLEX)
	  && s->succ_next == NULL
	  && s->dest == c)
	tidy_fallthru_edge (s);
    }
}
