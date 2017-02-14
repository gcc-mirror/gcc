/* Hooks for cfg representation specific functions.
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <s.pop@laposte.net>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "cfghooks.h"
#include "timevar.h"
#include "pretty-print.h"
#include "diagnostic-core.h"
#include "dumpfile.h"
#include "cfganal.h"
#include "tree-ssa.h"
#include "cfgloop.h"

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

/* Initialization of functions specific to the tree IR.  */

void
gimple_register_cfg_hooks (void)
{
  cfg_hooks = &gimple_cfg_hooks;
}

struct cfg_hooks
get_cfg_hooks (void)
{
  return *cfg_hooks;
}

void
set_cfg_hooks (struct cfg_hooks new_cfg_hooks)
{
  *cfg_hooks = new_cfg_hooks;
}

/* Returns current ir type.  */

enum ir_type
current_ir_type (void)
{
  if (cfg_hooks == &gimple_cfg_hooks)
    return IR_GIMPLE;
  else if (cfg_hooks == &rtl_cfg_hooks)
    return IR_RTL_CFGRTL;
  else if (cfg_hooks == &cfg_layout_rtl_cfg_hooks)
    return IR_RTL_CFGLAYOUT;
  else
    gcc_unreachable ();
}

/* Verify the CFG consistency.

   Currently it does following: checks edge and basic block list correctness
   and calls into IL dependent checking then.  */

DEBUG_FUNCTION void
verify_flow_info (void)
{
  size_t *edge_checksum;
  int err = 0;
  basic_block bb, last_bb_seen;
  basic_block *last_visited;

  timevar_push (TV_CFG_VERIFY);
  last_visited = XCNEWVEC (basic_block, last_basic_block_for_fn (cfun));
  edge_checksum = XCNEWVEC (size_t, last_basic_block_for_fn (cfun));

  /* Check bb chain & numbers.  */
  last_bb_seen = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb, NULL, next_bb)
    {
      if (bb != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && bb != BASIC_BLOCK_FOR_FN (cfun, bb->index))
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
  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      int n_fallthru = 0;
      edge e;
      edge_iterator ei;

      if (bb->loop_father != NULL && current_loops == NULL)
	{
	  error ("verify_flow_info: Block %i has loop_father, but there are no loops",
		 bb->index);
	  err = 1;
	}
      if (bb->loop_father == NULL && current_loops != NULL)
	{
	  error ("verify_flow_info: Block %i lacks loop_father", bb->index);
	  err = 1;
	}

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
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (last_visited [e->dest->index] == bb)
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

	  last_visited [e->dest->index] = bb;

	  if (e->flags & EDGE_FALLTHRU)
	    n_fallthru++;

	  if (e->src != bb)
	    {
	      error ("verify_flow_info: Basic block %d succ edge is corrupted",
		     bb->index);
	      fprintf (stderr, "Predecessor: ");
	      dump_edge_info (stderr, e, TDF_DETAILS, 0);
	      fprintf (stderr, "\nSuccessor: ");
	      dump_edge_info (stderr, e, TDF_DETAILS, 1);
	      fprintf (stderr, "\n");
	      err = 1;
	    }

	  edge_checksum[e->dest->index] += (size_t) e;
	}
      if (n_fallthru > 1)
	{
	  error ("wrong amount of branch edges after unconditional jump %i", bb->index);
	  err = 1;
	}

      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->dest != bb)
	    {
	      error ("basic block %d pred edge is corrupted", bb->index);
	      fputs ("Predecessor: ", stderr);
	      dump_edge_info (stderr, e, TDF_DETAILS, 0);
	      fputs ("\nSuccessor: ", stderr);
	      dump_edge_info (stderr, e, TDF_DETAILS, 1);
	      fputc ('\n', stderr);
	      err = 1;
	    }

	  if (ei.index != e->dest_idx)
	    {
	      error ("basic block %d pred edge is corrupted", bb->index);
	      error ("its dest_idx should be %d, not %d",
		     ei.index, e->dest_idx);
	      fputs ("Predecessor: ", stderr);
	      dump_edge_info (stderr, e, TDF_DETAILS, 0);
	      fputs ("\nSuccessor: ", stderr);
	      dump_edge_info (stderr, e, TDF_DETAILS, 1);
	      fputc ('\n', stderr);
	      err = 1;
	    }

	  edge_checksum[e->dest->index] -= (size_t) e;
	}
    }

  /* Complete edge checksumming for ENTRY and EXIT.  */
  {
    edge e;
    edge_iterator ei;

    FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs)
      edge_checksum[e->dest->index] += (size_t) e;

    FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
      edge_checksum[e->dest->index] -= (size_t) e;
  }

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, next_bb)
    if (edge_checksum[bb->index])
      {
	error ("basic block %i edge lists are corrupted", bb->index);
	err = 1;
      }

  last_bb_seen = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  /* Clean up.  */
  free (last_visited);
  free (edge_checksum);

  if (cfg_hooks->verify_flow_info)
    err |= cfg_hooks->verify_flow_info ();
  if (err)
    internal_error ("verify_flow_info failed");
  timevar_pop (TV_CFG_VERIFY);
}

/* Print out one basic block BB to file OUTF.  INDENT is printed at the
   start of each new line.  FLAGS are the TDF_* flags in dumpfile.h.

   This function takes care of the purely graph related information.
   The cfg hook for the active representation should dump
   representation-specific information.  */

void
dump_bb (FILE *outf, basic_block bb, int indent, int flags)
{
  if (flags & TDF_BLOCKS)
    dump_bb_info (outf, bb, indent, flags, true, false);
  if (cfg_hooks->dump_bb)
    cfg_hooks->dump_bb (outf, bb, indent, flags);
  if (flags & TDF_BLOCKS)
    dump_bb_info (outf, bb, indent, flags, false, true);
  fputc ('\n', outf);
}

DEBUG_FUNCTION void
debug (basic_block_def &ref)
{
  dump_bb (stderr, &ref, 0, 0);
}

DEBUG_FUNCTION void
debug (basic_block_def *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Dumps basic block BB to pretty-printer PP, for use as a label of
   a DOT graph record-node.  The implementation of this hook is
   expected to write the label to the stream that is attached to PP.
   Field separators between instructions are pipe characters printed
   verbatim.  Instructions should be written with some characters
   escaped, using pp_write_text_as_dot_label_to_stream().  */

void
dump_bb_for_graph (pretty_printer *pp, basic_block bb)
{
  if (!cfg_hooks->dump_bb_for_graph)
    internal_error ("%s does not support dump_bb_for_graph",
		    cfg_hooks->name);
  if (bb->count)
    pp_printf (pp, "COUNT:" "%" PRId64, bb->count);
  pp_printf (pp, " FREQ:%i |", bb->frequency);
  pp_write_text_to_stream (pp);
  if (!(dump_flags & TDF_SLIM))
    cfg_hooks->dump_bb_for_graph (pp, bb);
}

/* Dump the complete CFG to FILE.  FLAGS are the TDF_* flags in dumpfile.h.  */
void
dump_flow_info (FILE *file, int flags)
{
  basic_block bb;

  fprintf (file, "\n%d basic blocks, %d edges.\n", n_basic_blocks_for_fn (cfun),
	   n_edges_for_fn (cfun));
  FOR_ALL_BB_FN (bb, cfun)
    dump_bb (file, bb, 0, flags);

  putc ('\n', file);
}

/* Like above, but dump to stderr.  To be called from debuggers.  */
void debug_flow_info (void);
DEBUG_FUNCTION void
debug_flow_info (void)
{
  dump_flow_info (stderr, TDF_DETAILS);
}

/* Redirect edge E to the given basic block DEST and update underlying program
   representation.  Returns edge representing redirected branch (that may not
   be equivalent to E in the case of duplicate edges being removed) or NULL
   if edge is not easily redirectable for whatever reason.  */

edge
redirect_edge_and_branch (edge e, basic_block dest)
{
  edge ret;

  if (!cfg_hooks->redirect_edge_and_branch)
    internal_error ("%s does not support redirect_edge_and_branch",
		    cfg_hooks->name);

  ret = cfg_hooks->redirect_edge_and_branch (e, dest);

  /* If RET != E, then either the redirection failed, or the edge E
     was removed since RET already lead to the same destination.  */
  if (current_loops != NULL && ret == e)
    rescan_loop_exit (e, false, false);

  return ret;
}

/* Returns true if it is possible to remove the edge E by redirecting it
   to the destination of the other edge going from its source.  */

bool
can_remove_branch_p (const_edge e)
{
  if (!cfg_hooks->can_remove_branch_p)
    internal_error ("%s does not support can_remove_branch_p",
		    cfg_hooks->name);

  if (EDGE_COUNT (e->src->succs) != 2)
    return false;

  return cfg_hooks->can_remove_branch_p (e);
}

/* Removes E, by redirecting it to the destination of the other edge going
   from its source.  Can_remove_branch_p must be true for E, hence this
   operation cannot fail.  */

void
remove_branch (edge e)
{
  edge other;
  basic_block src = e->src;
  int irr;

  gcc_assert (EDGE_COUNT (e->src->succs) == 2);

  other = EDGE_SUCC (src, EDGE_SUCC (src, 0) == e);
  irr = other->flags & EDGE_IRREDUCIBLE_LOOP;

  e = redirect_edge_and_branch (e, other->dest);
  gcc_assert (e != NULL);

  e->flags &= ~EDGE_IRREDUCIBLE_LOOP;
  e->flags |= irr;
}

/* Removes edge E from cfg.  Unlike remove_branch, it does not update IL.  */

void
remove_edge (edge e)
{
  if (current_loops != NULL)
    {
      rescan_loop_exit (e, false, true);

      /* Removal of an edge inside an irreducible region or which leads
	 to an irreducible region can turn the region into a natural loop.
	 In that case, ask for the loop structure fixups.

	 FIXME: Note that LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS is not always
	 set, so always ask for fixups when removing an edge in that case.  */
      if (!loops_state_satisfies_p (LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS)
	  || (e->flags & EDGE_IRREDUCIBLE_LOOP)
	  || (e->dest->flags & BB_IRREDUCIBLE_LOOP))
	loops_state_set (LOOPS_NEED_FIXUP);
    }

  /* This is probably not needed, but it doesn't hurt.  */
  /* FIXME: This should be called via a remove_edge hook.  */
  if (current_ir_type () == IR_GIMPLE)
    redirect_edge_var_map_clear (e);

  remove_edge_raw (e);
}

/* Like redirect_edge_succ but avoid possible duplicate edge.  */

edge
redirect_edge_succ_nodup (edge e, basic_block new_succ)
{
  edge s;

  s = find_edge (e->src, new_succ);
  if (s && s != e)
    {
      s->flags |= e->flags;
      s->probability += e->probability;
      if (s->probability > REG_BR_PROB_BASE)
	s->probability = REG_BR_PROB_BASE;
      s->count += e->count;
      /* FIXME: This should be called via a hook and only for IR_GIMPLE.  */
      redirect_edge_var_map_dup (s, e);
      remove_edge (e);
      e = s;
    }
  else
    redirect_edge_succ (e, new_succ);

  return e;
}

/* Redirect the edge E to basic block DEST even if it requires creating
   of a new basic block; then it returns the newly created basic block.
   Aborts when redirection is impossible.  */

basic_block
redirect_edge_and_branch_force (edge e, basic_block dest)
{
  basic_block ret, src = e->src;

  if (!cfg_hooks->redirect_edge_and_branch_force)
    internal_error ("%s does not support redirect_edge_and_branch_force",
		    cfg_hooks->name);

  if (current_loops != NULL)
    rescan_loop_exit (e, false, true);

  ret = cfg_hooks->redirect_edge_and_branch_force (e, dest);

  if (ret != NULL && dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, ret, src);

  if (current_loops != NULL)
    {
      if (ret != NULL)
	{
	  struct loop *loop
	    = find_common_loop (single_pred (ret)->loop_father,
				single_succ (ret)->loop_father);
	  add_bb_to_loop (ret, loop);
	}
      else if (find_edge (src, dest) == e)
	rescan_loop_exit (e, true, false);
    }

  return ret;
}

/* Splits basic block BB after the specified instruction I (but at least after
   the labels).  If I is NULL, splits just after labels.  The newly created edge
   is returned.  The new basic block is created just after the old one.  */

static edge
split_block_1 (basic_block bb, void *i)
{
  basic_block new_bb;
  edge res;

  if (!cfg_hooks->split_block)
    internal_error ("%s does not support split_block", cfg_hooks->name);

  new_bb = cfg_hooks->split_block (bb, i);
  if (!new_bb)
    return NULL;

  new_bb->count = bb->count;
  new_bb->frequency = bb->frequency;
  new_bb->discriminator = bb->discriminator;

  if (dom_info_available_p (CDI_DOMINATORS))
    {
      redirect_immediate_dominators (CDI_DOMINATORS, bb, new_bb);
      set_immediate_dominator (CDI_DOMINATORS, new_bb, bb);
    }

  if (current_loops != NULL)
    {
      edge_iterator ei;
      edge e;
      add_bb_to_loop (new_bb, bb->loop_father);
      /* Identify all loops bb may have been the latch of and adjust them.  */
      FOR_EACH_EDGE (e, ei, new_bb->succs)
	if (e->dest->loop_father->latch == bb)
	  e->dest->loop_father->latch = new_bb;
    }

  res = make_single_succ_edge (bb, new_bb, EDGE_FALLTHRU);

  if (bb->flags & BB_IRREDUCIBLE_LOOP)
    {
      new_bb->flags |= BB_IRREDUCIBLE_LOOP;
      res->flags |= EDGE_IRREDUCIBLE_LOOP;
    }

  return res;
}

edge
split_block (basic_block bb, gimple *i)
{
  return split_block_1 (bb, i);
}

edge
split_block (basic_block bb, rtx i)
{
  return split_block_1 (bb, i);
}

/* Splits block BB just after labels.  The newly created edge is returned.  */

edge
split_block_after_labels (basic_block bb)
{
  return split_block_1 (bb, NULL);
}

/* Moves block BB immediately after block AFTER.  Returns false if the
   movement was impossible.  */

bool
move_block_after (basic_block bb, basic_block after)
{
  bool ret;

  if (!cfg_hooks->move_block_after)
    internal_error ("%s does not support move_block_after", cfg_hooks->name);

  ret = cfg_hooks->move_block_after (bb, after);

  return ret;
}

/* Deletes the basic block BB.  */

void
delete_basic_block (basic_block bb)
{
  if (!cfg_hooks->delete_basic_block)
    internal_error ("%s does not support delete_basic_block", cfg_hooks->name);

  cfg_hooks->delete_basic_block (bb);

  if (current_loops != NULL)
    {
      struct loop *loop = bb->loop_father;

      /* If we remove the header or the latch of a loop, mark the loop for
	 removal.  */
      if (loop->latch == bb
	  || loop->header == bb)
	mark_loop_for_removal (loop);

      remove_bb_from_loops (bb);
    }

  /* Remove the edges into and out of this block.  Note that there may
     indeed be edges in, if we are removing an unreachable loop.  */
  while (EDGE_COUNT (bb->preds) != 0)
    remove_edge (EDGE_PRED (bb, 0));
  while (EDGE_COUNT (bb->succs) != 0)
    remove_edge (EDGE_SUCC (bb, 0));

  if (dom_info_available_p (CDI_DOMINATORS))
    delete_from_dominance_info (CDI_DOMINATORS, bb);
  if (dom_info_available_p (CDI_POST_DOMINATORS))
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
  edge f;
  bool irr = (e->flags & EDGE_IRREDUCIBLE_LOOP) != 0;
  struct loop *loop;
  basic_block src = e->src, dest = e->dest;

  if (!cfg_hooks->split_edge)
    internal_error ("%s does not support split_edge", cfg_hooks->name);

  if (current_loops != NULL)
    rescan_loop_exit (e, false, true);

  ret = cfg_hooks->split_edge (e);
  ret->count = count;
  ret->frequency = freq;
  single_succ_edge (ret)->probability = REG_BR_PROB_BASE;
  single_succ_edge (ret)->count = count;

  if (irr)
    {
      ret->flags |= BB_IRREDUCIBLE_LOOP;
      single_pred_edge (ret)->flags |= EDGE_IRREDUCIBLE_LOOP;
      single_succ_edge (ret)->flags |= EDGE_IRREDUCIBLE_LOOP;
    }

  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, ret, single_pred (ret));

  if (dom_info_state (CDI_DOMINATORS) >= DOM_NO_FAST_QUERY)
    {
      /* There are two cases:

	 If the immediate dominator of e->dest is not e->src, it
	 remains unchanged.

	 If immediate dominator of e->dest is e->src, it may become
	 ret, provided that all other predecessors of e->dest are
	 dominated by e->dest.  */

      if (get_immediate_dominator (CDI_DOMINATORS, single_succ (ret))
	  == single_pred (ret))
	{
	  edge_iterator ei;
	  FOR_EACH_EDGE (f, ei, single_succ (ret)->preds)
	    {
	      if (f == single_succ_edge (ret))
		continue;

	      if (!dominated_by_p (CDI_DOMINATORS, f->src,
				   single_succ (ret)))
		break;
	    }

	  if (!f)
	    set_immediate_dominator (CDI_DOMINATORS, single_succ (ret), ret);
	}
    }

  if (current_loops != NULL)
    {
      loop = find_common_loop (src->loop_father, dest->loop_father);
      add_bb_to_loop (ret, loop);

      /* If we split the latch edge of loop adjust the latch block.  */
      if (loop->latch == src
	  && loop->header == dest)
	loop->latch = ret;
    }

  return ret;
}

/* Creates a new basic block just after the basic block AFTER.
   HEAD and END are the first and the last statement belonging
   to the block.  If both are NULL, an empty block is created.  */

static basic_block
create_basic_block_1 (void *head, void *end, basic_block after)
{
  basic_block ret;

  if (!cfg_hooks->create_basic_block)
    internal_error ("%s does not support create_basic_block", cfg_hooks->name);

  ret = cfg_hooks->create_basic_block (head, end, after);

  if (dom_info_available_p (CDI_DOMINATORS))
    add_to_dominance_info (CDI_DOMINATORS, ret);
  if (dom_info_available_p (CDI_POST_DOMINATORS))
    add_to_dominance_info (CDI_POST_DOMINATORS, ret);

  return ret;
}

basic_block
create_basic_block (gimple_seq seq, basic_block after)
{
  return create_basic_block_1 (seq, NULL, after);
}

basic_block
create_basic_block (rtx head, rtx end, basic_block after)
{
  return create_basic_block_1 (head, end, after);
}


/* Creates an empty basic block just after basic block AFTER.  */

basic_block
create_empty_bb (basic_block after)
{
  return create_basic_block_1 (NULL, NULL, after);
}

/* Checks whether we may merge blocks BB1 and BB2.  */

bool
can_merge_blocks_p (basic_block bb1, basic_block bb2)
{
  bool ret;

  if (!cfg_hooks->can_merge_blocks_p)
    internal_error ("%s does not support can_merge_blocks_p", cfg_hooks->name);

  ret = cfg_hooks->can_merge_blocks_p (bb1, bb2);

  return ret;
}

void
predict_edge (edge e, enum br_predictor predictor, int probability)
{
  if (!cfg_hooks->predict_edge)
    internal_error ("%s does not support predict_edge", cfg_hooks->name);

  cfg_hooks->predict_edge (e, predictor, probability);
}

bool
predicted_by_p (const_basic_block bb, enum br_predictor predictor)
{
  if (!cfg_hooks->predict_edge)
    internal_error ("%s does not support predicted_by_p", cfg_hooks->name);

  return cfg_hooks->predicted_by_p (bb, predictor);
}

/* Merges basic block B into basic block A.  */

void
merge_blocks (basic_block a, basic_block b)
{
  edge e;
  edge_iterator ei;

  if (!cfg_hooks->merge_blocks)
    internal_error ("%s does not support merge_blocks", cfg_hooks->name);

  cfg_hooks->merge_blocks (a, b);

  if (current_loops != NULL)
    {
      /* If the block we merge into is a loop header do nothing unless ... */
      if (a->loop_father->header == a)
	{
	  /* ... we merge two loop headers, in which case we kill
	     the inner loop.  */
	  if (b->loop_father->header == b)
	    mark_loop_for_removal (b->loop_father);
	}
      /* If we merge a loop header into its predecessor, update the loop
	 structure.  */
      else if (b->loop_father->header == b)
	{
	  remove_bb_from_loops (a);
	  add_bb_to_loop  (a, b->loop_father);
	  a->loop_father->header = a;
	}
      /* If we merge a loop latch into its predecessor, update the loop
         structure.  */
      if (b->loop_father->latch
	  && b->loop_father->latch == b)
	b->loop_father->latch = a;
      remove_bb_from_loops (b);
    }

  /* Normally there should only be one successor of A and that is B, but
     partway though the merge of blocks for conditional_execution we'll
     be merging a TEST block with THEN and ELSE successors.  Free the
     whole lot of them and hope the caller knows what they're doing.  */

  while (EDGE_COUNT (a->succs) != 0)
    remove_edge (EDGE_SUCC (a, 0));

  /* Adjust the edges out of B for the new owner.  */
  FOR_EACH_EDGE (e, ei, b->succs)
    {
      e->src = a;
      if (current_loops != NULL)
	{
	  /* If b was a latch, a now is.  */
	  if (e->dest->loop_father->latch == b)
	    e->dest->loop_father->latch = a;
	  rescan_loop_exit (e, true, false);
	}
    }
  a->succs = b->succs;
  a->flags |= b->flags;

  /* B hasn't quite yet ceased to exist.  Attempt to prevent mishap.  */
  b->preds = b->succs = NULL;

  if (dom_info_available_p (CDI_DOMINATORS))
    redirect_immediate_dominators (CDI_DOMINATORS, b, a);

  if (dom_info_available_p (CDI_DOMINATORS))
    delete_from_dominance_info (CDI_DOMINATORS, b);
  if (dom_info_available_p (CDI_POST_DOMINATORS))
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
  edge e, fallthru;
  edge_iterator ei;
  basic_block dummy, jump;
  struct loop *loop, *ploop, *cloop;

  if (!cfg_hooks->make_forwarder_block)
    internal_error ("%s does not support make_forwarder_block",
		    cfg_hooks->name);

  fallthru = split_block_after_labels (bb);
  dummy = fallthru->src;
  dummy->count = 0;
  dummy->frequency = 0;
  fallthru->count = 0;
  bb = fallthru->dest;

  /* Redirect back edges we want to keep.  */
  for (ei = ei_start (dummy->preds); (e = ei_safe_edge (ei)); )
    {
      basic_block e_src;

      if (redirect_edge_p (e))
	{
	  dummy->frequency += EDGE_FREQUENCY (e);
	  if (dummy->frequency > BB_FREQ_MAX)
	    dummy->frequency = BB_FREQ_MAX;

	  dummy->count += e->count;
	  fallthru->count += e->count;
	  ei_next (&ei);
	  continue;
	}

      e_src = e->src;
      jump = redirect_edge_and_branch_force (e, bb);
      if (jump != NULL)
        {
          /* If we redirected the loop latch edge, the JUMP block now acts like
             the new latch of the loop.  */
          if (current_loops != NULL
              && dummy->loop_father != NULL
              && dummy->loop_father->header == dummy
              && dummy->loop_father->latch == e_src)
            dummy->loop_father->latch = jump;

          if (new_bb_cbk != NULL)
            new_bb_cbk (jump);
        }
    }

  if (dom_info_available_p (CDI_DOMINATORS))
    {
      vec<basic_block> doms_to_fix;
      doms_to_fix.create (2);
      doms_to_fix.quick_push (dummy);
      doms_to_fix.quick_push (bb);
      iterate_fix_dominators (CDI_DOMINATORS, doms_to_fix, false);
      doms_to_fix.release ();
    }

  if (current_loops != NULL)
    {
      /* If we do not split a loop header, then both blocks belong to the
	 same loop.  In case we split loop header and do not redirect the
	 latch edge to DUMMY, then DUMMY belongs to the outer loop, and
	 BB becomes the new header.  If latch is not recorded for the loop,
	 we leave this updating on the caller (this may only happen during
	 loop analysis).  */
      loop = dummy->loop_father;
      if (loop->header == dummy
	  && loop->latch != NULL
	  && find_edge (loop->latch, dummy) == NULL)
	{
	  remove_bb_from_loops (dummy);
	  loop->header = bb;

	  cloop = loop;
	  FOR_EACH_EDGE (e, ei, dummy->preds)
	    {
	      cloop = find_common_loop (cloop, e->src->loop_father);
	    }
	  add_bb_to_loop (dummy, cloop);
	}

      /* In case we split loop latch, update it.  */
      for (ploop = loop; ploop; ploop = loop_outer (ploop))
	if (ploop->latch == dummy)
	  ploop->latch = bb;
    }

  cfg_hooks->make_forwarder_block (fallthru);

  return fallthru;
}

/* Try to make the edge fallthru.  */

void
tidy_fallthru_edge (edge e)
{
  if (cfg_hooks->tidy_fallthru_edge)
    cfg_hooks->tidy_fallthru_edge (e);
}

/* Fix up edges that now fall through, or rather should now fall through
   but previously required a jump around now deleted blocks.  Simplify
   the search by only examining blocks numerically adjacent, since this
   is how they were created.

   ??? This routine is currently RTL specific.  */

void
tidy_fallthru_edges (void)
{
  basic_block b, c;

  if (!cfg_hooks->tidy_fallthru_edge)
    return;

  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return;

  FOR_BB_BETWEEN (b, ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb,
		  EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb, next_bb)
    {
      edge s;

      c = b->next_bb;

      /* We care about simple conditional or unconditional jumps with
	 a single successor.

	 If we had a conditional branch to the next instruction when
	 CFG was built, then there will only be one out edge for the
	 block which ended with the conditional branch (since we do
	 not create duplicate edges).

	 Furthermore, the edge will be marked as a fallthru because we
	 merge the flags for the duplicate edges.  So we do not want to
	 check that the edge is not a FALLTHRU edge.  */

      if (single_succ_p (b))
	{
	  s = single_succ_edge (b);
	  if (! (s->flags & EDGE_COMPLEX)
	      && s->dest == c
	      && !(JUMP_P (BB_END (b)) && CROSSING_JUMP_P (BB_END (b))))
	    tidy_fallthru_edge (s);
	}
    }
}

/* Edge E is assumed to be fallthru edge.  Emit needed jump instruction
   (and possibly create new basic block) to make edge non-fallthru.
   Return newly created BB or NULL if none.  */

basic_block
force_nonfallthru (edge e)
{
  basic_block ret, src = e->src;

  if (!cfg_hooks->force_nonfallthru)
    internal_error ("%s does not support force_nonfallthru",
		    cfg_hooks->name);

  ret = cfg_hooks->force_nonfallthru (e);
  if (ret != NULL)
    {
      if (dom_info_available_p (CDI_DOMINATORS))
	set_immediate_dominator (CDI_DOMINATORS, ret, src);

      if (current_loops != NULL)
	{
	  basic_block pred = single_pred (ret);
	  basic_block succ = single_succ (ret);
	  struct loop *loop
	    = find_common_loop (pred->loop_father, succ->loop_father);
	  rescan_loop_exit (e, false, true);
	  add_bb_to_loop (ret, loop);

	  /* If we split the latch edge of loop adjust the latch block.  */
	  if (loop->latch == pred
	      && loop->header == succ)
	    loop->latch = ret;
	}
    }

  return ret;
}

/* Returns true if we can duplicate basic block BB.  */

bool
can_duplicate_block_p (const_basic_block bb)
{
  if (!cfg_hooks->can_duplicate_block_p)
    internal_error ("%s does not support can_duplicate_block_p",
		    cfg_hooks->name);

  if (bb == EXIT_BLOCK_PTR_FOR_FN (cfun) || bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    return false;

  return cfg_hooks->can_duplicate_block_p (bb);
}

/* Duplicates basic block BB and redirects edge E to it.  Returns the
   new basic block.  The new basic block is placed after the basic block
   AFTER.  */

basic_block
duplicate_block (basic_block bb, edge e, basic_block after)
{
  edge s, n;
  basic_block new_bb;
  gcov_type new_count = e ? e->count : 0;
  edge_iterator ei;

  if (!cfg_hooks->duplicate_block)
    internal_error ("%s does not support duplicate_block",
		    cfg_hooks->name);

  if (bb->count < new_count)
    new_count = bb->count;

  gcc_checking_assert (can_duplicate_block_p (bb));

  new_bb = cfg_hooks->duplicate_block (bb);
  if (after)
    move_block_after (new_bb, after);

  new_bb->flags = bb->flags;
  FOR_EACH_EDGE (s, ei, bb->succs)
    {
      /* Since we are creating edges from a new block to successors
	 of another block (which therefore are known to be disjoint), there
	 is no need to actually check for duplicated edges.  */
      n = unchecked_make_edge (new_bb, s->dest, s->flags);
      n->probability = s->probability;
      if (e && bb->count)
	{
	  /* Take care for overflows!  */
	  n->count = s->count * (new_count * 10000 / bb->count) / 10000;
	  s->count -= n->count;
	}
      else
	n->count = s->count;
      n->aux = s->aux;
    }

  if (e)
    {
      new_bb->count = new_count;
      bb->count -= new_count;

      new_bb->frequency = EDGE_FREQUENCY (e);
      bb->frequency -= EDGE_FREQUENCY (e);

      redirect_edge_and_branch_force (e, new_bb);

      if (bb->count < 0)
	bb->count = 0;
      if (bb->frequency < 0)
	bb->frequency = 0;
    }
  else
    {
      new_bb->count = bb->count;
      new_bb->frequency = bb->frequency;
    }

  set_bb_original (new_bb, bb);
  set_bb_copy (bb, new_bb);

  /* Add the new block to the copy of the loop of BB, or directly to the loop
     of BB if the loop is not being copied.  */
  if (current_loops != NULL)
    {
      struct loop *cloop = bb->loop_father;
      struct loop *copy = get_loop_copy (cloop);
      /* If we copied the loop header block but not the loop
	 we have created a loop with multiple entries.  Ditch the loop,
	 add the new block to the outer loop and arrange for a fixup.  */
      if (!copy
	  && cloop->header == bb)
	{
	  add_bb_to_loop (new_bb, loop_outer (cloop));
	  mark_loop_for_removal (cloop);
	}
      else
	{
	  add_bb_to_loop (new_bb, copy ? copy : cloop);
	  /* If we copied the loop latch block but not the loop, adjust
	     loop state.  */
	  if (!copy
	      && cloop->latch == bb)
	    {
	      cloop->latch = NULL;
	      loops_state_set (LOOPS_MAY_HAVE_MULTIPLE_LATCHES);
	    }
	}
    }

  return new_bb;
}

/* Return 1 if BB ends with a call, possibly followed by some
   instructions that must stay with the call, 0 otherwise.  */

bool
block_ends_with_call_p (basic_block bb)
{
  if (!cfg_hooks->block_ends_with_call_p)
    internal_error ("%s does not support block_ends_with_call_p", cfg_hooks->name);

  return (cfg_hooks->block_ends_with_call_p) (bb);
}

/* Return 1 if BB ends with a conditional branch, 0 otherwise.  */

bool
block_ends_with_condjump_p (const_basic_block bb)
{
  if (!cfg_hooks->block_ends_with_condjump_p)
    internal_error ("%s does not support block_ends_with_condjump_p",
		    cfg_hooks->name);

  return (cfg_hooks->block_ends_with_condjump_p) (bb);
}

/* Add fake edges to the function exit for any non constant and non noreturn
   calls, volatile inline assembly in the bitmap of blocks specified by
   BLOCKS or to the whole CFG if BLOCKS is zero.  Return the number of blocks
   that were split.

   The goal is to expose cases in which entering a basic block does not imply
   that all subsequent instructions must be executed.  */

int
flow_call_edges_add (sbitmap blocks)
{
  if (!cfg_hooks->flow_call_edges_add)
    internal_error ("%s does not support flow_call_edges_add",
		    cfg_hooks->name);

  return (cfg_hooks->flow_call_edges_add) (blocks);
}

/* This function is called immediately after edge E is added to the
   edge vector E->dest->preds.  */

void
execute_on_growing_pred (edge e)
{
  if (cfg_hooks->execute_on_growing_pred)
    cfg_hooks->execute_on_growing_pred (e);
}

/* This function is called immediately before edge E is removed from
   the edge vector E->dest->preds.  */

void
execute_on_shrinking_pred (edge e)
{
  if (cfg_hooks->execute_on_shrinking_pred)
    cfg_hooks->execute_on_shrinking_pred (e);
}

/* This is used inside loop versioning when we want to insert
   stmts/insns on the edges, which have a different behavior
   in tree's and in RTL, so we made a CFG hook.  */
void
lv_flush_pending_stmts (edge e)
{
  if (cfg_hooks->flush_pending_stmts)
    cfg_hooks->flush_pending_stmts (e);
}

/* Loop versioning uses the duplicate_loop_to_header_edge to create
   a new version of the loop basic-blocks, the parameters here are
   exactly the same as in duplicate_loop_to_header_edge or
   tree_duplicate_loop_to_header_edge; while in tree-ssa there is
   additional work to maintain ssa information that's why there is
   a need to call the tree_duplicate_loop_to_header_edge rather
   than duplicate_loop_to_header_edge when we are in tree mode.  */
bool
cfg_hook_duplicate_loop_to_header_edge (struct loop *loop, edge e,
					unsigned int ndupl,
					sbitmap wont_exit, edge orig,
					vec<edge> *to_remove,
					int flags)
{
  gcc_assert (cfg_hooks->cfg_hook_duplicate_loop_to_header_edge);
  return cfg_hooks->cfg_hook_duplicate_loop_to_header_edge (loop, e,
							    ndupl, wont_exit,
							    orig, to_remove,
							    flags);
}

/* Conditional jumps are represented differently in trees and RTL,
   this hook takes a basic block that is known to have a cond jump
   at its end and extracts the taken and not taken edges out of it
   and store it in E1 and E2 respectively.  */
void
extract_cond_bb_edges (basic_block b, edge *e1, edge *e2)
{
  gcc_assert (cfg_hooks->extract_cond_bb_edges);
  cfg_hooks->extract_cond_bb_edges (b, e1, e2);
}

/* Responsible for updating the ssa info (PHI nodes) on the
   new condition basic block that guards the versioned loop.  */
void
lv_adjust_loop_header_phi (basic_block first, basic_block second,
			   basic_block new_block, edge e)
{
  if (cfg_hooks->lv_adjust_loop_header_phi)
    cfg_hooks->lv_adjust_loop_header_phi (first, second, new_block, e);
}

/* Conditions in trees and RTL are different so we need
   a different handling when we add the condition to the
   versioning code.  */
void
lv_add_condition_to_bb (basic_block first, basic_block second,
			basic_block new_block, void *cond)
{
  gcc_assert (cfg_hooks->lv_add_condition_to_bb);
  cfg_hooks->lv_add_condition_to_bb (first, second, new_block, cond);
}

/* Checks whether all N blocks in BBS array can be copied.  */
bool
can_copy_bbs_p (basic_block *bbs, unsigned n)
{
  unsigned i;
  edge e;
  int ret = true;

  for (i = 0; i < n; i++)
    bbs[i]->flags |= BB_DUPLICATED;

  for (i = 0; i < n; i++)
    {
      /* In case we should redirect abnormal edge during duplication, fail.  */
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bbs[i]->succs)
	if ((e->flags & EDGE_ABNORMAL)
	    && (e->dest->flags & BB_DUPLICATED))
	  {
	    ret = false;
	    goto end;
	  }

      if (!can_duplicate_block_p (bbs[i]))
	{
	  ret = false;
	  break;
	}
    }

end:
  for (i = 0; i < n; i++)
    bbs[i]->flags &= ~BB_DUPLICATED;

  return ret;
}

/* Duplicates N basic blocks stored in array BBS.  Newly created basic blocks
   are placed into array NEW_BBS in the same order.  Edges from basic blocks
   in BBS are also duplicated and copies of those that lead into BBS are
   redirected to appropriate newly created block.  The function assigns bbs
   into loops (copy of basic block bb is assigned to bb->loop_father->copy
   loop, so this must be set up correctly in advance)

   If UPDATE_DOMINANCE is true then this function updates dominators locally
   (LOOPS structure that contains the information about dominators is passed
   to enable this), otherwise it does not update the dominator information
   and it assumed that the caller will do this, perhaps by destroying and
   recreating it instead of trying to do an incremental update like this
   function does when update_dominance is true.

   BASE is the superloop to that basic block belongs; if its header or latch
   is copied, we do not set the new blocks as header or latch.

   Created copies of N_EDGES edges in array EDGES are stored in array NEW_EDGES,
   also in the same order.

   Newly created basic blocks are put after the basic block AFTER in the
   instruction stream, and the order of the blocks in BBS array is preserved.  */

void
copy_bbs (basic_block *bbs, unsigned n, basic_block *new_bbs,
	  edge *edges, unsigned num_edges, edge *new_edges,
	  struct loop *base, basic_block after, bool update_dominance)
{
  unsigned i, j;
  basic_block bb, new_bb, dom_bb;
  edge e;

  /* Duplicate bbs, update dominators, assign bbs to loops.  */
  for (i = 0; i < n; i++)
    {
      /* Duplicate.  */
      bb = bbs[i];
      new_bb = new_bbs[i] = duplicate_block (bb, NULL, after);
      after = new_bb;
      bb->flags |= BB_DUPLICATED;
      if (bb->loop_father)
	{
	  /* Possibly set loop header.  */
	  if (bb->loop_father->header == bb && bb->loop_father != base)
	    new_bb->loop_father->header = new_bb;
	  /* Or latch.  */
	  if (bb->loop_father->latch == bb && bb->loop_father != base)
	    new_bb->loop_father->latch = new_bb;
	}
    }

  /* Set dominators.  */
  if (update_dominance)
    {
      for (i = 0; i < n; i++)
	{
	  bb = bbs[i];
	  new_bb = new_bbs[i];

	  dom_bb = get_immediate_dominator (CDI_DOMINATORS, bb);
	  if (dom_bb->flags & BB_DUPLICATED)
	    {
	      dom_bb = get_bb_copy (dom_bb);
	      set_immediate_dominator (CDI_DOMINATORS, new_bb, dom_bb);
	    }
	}
    }

  /* Redirect edges.  */
  for (j = 0; j < num_edges; j++)
    new_edges[j] = NULL;
  for (i = 0; i < n; i++)
    {
      edge_iterator ei;
      new_bb = new_bbs[i];
      bb = bbs[i];

      FOR_EACH_EDGE (e, ei, new_bb->succs)
	{
	  for (j = 0; j < num_edges; j++)
	    if (edges[j] && edges[j]->src == bb && edges[j]->dest == e->dest)
	      new_edges[j] = e;

	  if (!(e->dest->flags & BB_DUPLICATED))
	    continue;
	  redirect_edge_and_branch_force (e, get_bb_copy (e->dest));
	}
    }

  /* Clear information about duplicates.  */
  for (i = 0; i < n; i++)
    bbs[i]->flags &= ~BB_DUPLICATED;
}

/* Return true if BB contains only labels or non-executable
   instructions */
bool
empty_block_p (basic_block bb)
{
  gcc_assert (cfg_hooks->empty_block_p);
  return cfg_hooks->empty_block_p (bb);
}

/* Split a basic block if it ends with a conditional branch and if
   the other part of the block is not empty.  */
basic_block
split_block_before_cond_jump (basic_block bb)
{
  gcc_assert (cfg_hooks->split_block_before_cond_jump);
  return cfg_hooks->split_block_before_cond_jump (bb);
}

/* Work-horse for passes.c:check_profile_consistency.
   Do book-keeping of the CFG for the profile consistency checker.
   If AFTER_PASS is 0, do pre-pass accounting, or if AFTER_PASS is 1
   then do post-pass accounting.  Store the counting in RECORD.  */

void
account_profile_record (struct profile_record *record, int after_pass)
{
  basic_block bb;
  edge_iterator ei;
  edge e;
  int sum;
  gcov_type lsum;

  FOR_ALL_BB_FN (bb, cfun)
   {
      if (bb != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && profile_status_for_fn (cfun) != PROFILE_ABSENT)
	{
	  sum = 0;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    sum += e->probability;
	  if (EDGE_COUNT (bb->succs) && abs (sum - REG_BR_PROB_BASE) > 100)
	    record->num_mismatched_freq_out[after_pass]++;
	  lsum = 0;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    lsum += e->count;
	  if (EDGE_COUNT (bb->succs)
	      && (lsum - bb->count > 100 || lsum - bb->count < -100))
	    record->num_mismatched_count_out[after_pass]++;
	}
      if (bb != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  && profile_status_for_fn (cfun) != PROFILE_ABSENT)
	{
	  sum = 0;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    sum += EDGE_FREQUENCY (e);
	  if (abs (sum - bb->frequency) > 100
	      || (MAX (sum, bb->frequency) > 10
		  && abs ((sum - bb->frequency) * 100 / (MAX (sum, bb->frequency) + 1)) > 10))
	    record->num_mismatched_freq_in[after_pass]++;
	  lsum = 0;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    lsum += e->count;
	  if (lsum - bb->count > 100 || lsum - bb->count < -100)
	    record->num_mismatched_count_in[after_pass]++;
	}
      if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  || bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
	continue;
      gcc_assert (cfg_hooks->account_profile_record);
      cfg_hooks->account_profile_record (bb, after_pass, record);
   }
}
