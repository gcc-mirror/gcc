/* Loop header copying on trees.
   Copyright (C) 2004 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
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
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-inline.h"
#include "flags.h"
#include "tree-inline.h"

/* Duplicates headers of loops if they are small enough, so that the statements
   in the loop body are always executed when the loop is entered.  This
   increases effectivity of code motion optimizations, and reduces the need
   for loop preconditioning.  */

/* Check whether we should duplicate HEADER of LOOP.  At most *LIMIT
   instructions should be duplicated, limit is decreased by the actual
   amount.  */

static bool
should_duplicate_loop_header_p (basic_block header, struct loop *loop,
				int *limit)
{
  block_stmt_iterator bsi;
  tree last;

  /* Do not copy one block more than once (we do not really want to do
     loop peeling here).  */
  if (header->aux)
    return false;

  if (!header->succ)
    abort ();
  if (!header->succ->succ_next)
    return false;
  if (header->succ->succ_next->succ_next)
    return false;
  if (flow_bb_inside_loop_p (loop, header->succ->dest)
      && flow_bb_inside_loop_p (loop, header->succ->succ_next->dest))
    return false;

  /* If this is not the original loop header, we want it to have just
     one predecessor in order to match the && pattern.  */
  if (header != loop->header
      && header->pred->pred_next)
    return false;

  last = last_stmt (header);
  if (TREE_CODE (last) != COND_EXPR)
    return false;

  /* Approximately copy the conditions that used to be used in jump.c --
     at most 20 insns and no calls.  */
  for (bsi = bsi_start (header); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      last = bsi_stmt (bsi);

      if (TREE_CODE (last) == LABEL_EXPR)
	continue;

      if (get_call_expr_in (last))
	return false;

      *limit -= estimate_num_insns (last);
      if (*limit < 0)
	return false;
    }

  return true;
}

/* Marks variables defined in basic block BB for rewriting.  */

static void
mark_defs_for_rewrite (basic_block bb)
{
  tree stmt, var;
  block_stmt_iterator bsi;
  stmt_ann_t ann;
  def_optype defs;
  v_may_def_optype v_may_defs;
  v_must_def_optype v_must_defs;
  unsigned i;

  for (stmt = phi_nodes (bb); stmt; stmt = TREE_CHAIN (stmt))
    {
      var = PHI_RESULT (stmt);
      bitmap_set_bit (vars_to_rename, SSA_NAME_VERSION (var));
    }

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      stmt = bsi_stmt (bsi);
      get_stmt_operands (stmt);
      ann = stmt_ann (stmt);

      defs = DEF_OPS (ann);
      for (i = 0; i < NUM_DEFS (defs); i++)
	{
	  var = DEF_OP (defs, i);
	  bitmap_set_bit (vars_to_rename, SSA_NAME_VERSION (var));
	}

      v_may_defs = V_MAY_DEF_OPS (ann);
      for (i = 0; i < NUM_V_MAY_DEFS (v_may_defs); i++)
	{
	  var = V_MAY_DEF_RESULT (v_may_defs, i);
	  bitmap_set_bit (vars_to_rename, SSA_NAME_VERSION (var));
	}

      v_must_defs = V_MUST_DEF_OPS (ann);
      for (i = 0; i < NUM_V_MUST_DEFS (v_must_defs); i++)
	{
	  var = V_MUST_DEF_OP (v_must_defs, i);
	  bitmap_set_bit (vars_to_rename, SSA_NAME_VERSION (var));
	}
    }
}

/* Duplicates destinations of edges in BBS_TO_DUPLICATE.  */

static void
duplicate_blocks (varray_type bbs_to_duplicate)
{
  unsigned i;
  edge preheader_edge, e, e1;
  basic_block header, new_header;
  tree phi, new_phi, var;

  /* TODO: It should be quite easy to keep the dominance information
     up-to-date.  */
  free_dominance_info (CDI_DOMINATORS);

  for (i = 0; i < VARRAY_ACTIVE_SIZE (bbs_to_duplicate); i++)
    {
      preheader_edge = VARRAY_GENERIC_PTR_NOGC (bbs_to_duplicate, i);
      header = preheader_edge->dest;

      /* It is sufficient to rewrite the definitions, since the uses of
	 the operands defined outside of the duplicated basic block are
	 still valid (every basic block that dominates the original block
	 also dominates the duplicate).  */
      mark_defs_for_rewrite (header);
    }

  for (i = 0; i < VARRAY_ACTIVE_SIZE (bbs_to_duplicate); i++)
    {
      preheader_edge = VARRAY_GENERIC_PTR_NOGC (bbs_to_duplicate, i);
      header = preheader_edge->dest;

      if (!header->aux)
	abort ();
      header->aux = NULL;

      new_header = duplicate_block (header, preheader_edge);

      /* Create the phi nodes on on entry to new_header.  */
      for (phi = phi_nodes (header), var = PENDING_STMT (preheader_edge);
	   phi;
	   phi = TREE_CHAIN (phi), var = TREE_CHAIN (var))
	{
	  new_phi = create_phi_node (PHI_RESULT (phi), new_header);
	  add_phi_arg (&new_phi, TREE_VALUE (var), preheader_edge);
	}
      PENDING_STMT (preheader_edge) = NULL;

      /* Add the phi arguments to the outgoing edges.  */
      for (e = header->succ; e; e = e->succ_next)
	{
	  for (e1 = new_header->succ; e1->dest != e->dest; e1 = e1->succ_next)
	    continue;

	  for (phi = phi_nodes (e->dest); phi; phi = TREE_CHAIN (phi))
	    {
	      tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);
	      add_phi_arg (&phi, def, e1);
	    }
	}
    }

  calculate_dominance_info (CDI_DOMINATORS);

  rewrite_ssa_into_ssa (vars_to_rename);
  bitmap_clear (vars_to_rename);
}

/* Checks whether LOOP is a do-while style loop.  */

static bool
do_while_loop_p (struct loop *loop)
{
  tree stmt = last_stmt (loop->latch);

  /* If the latch of the loop is not empty, it is not a do-while loop.  */
  if (stmt
      && TREE_CODE (stmt) != LABEL_EXPR)
    return false;

  /* If the header contains just a condition, it is not a do-while loop.  */
  stmt = last_and_only_stmt (loop->header);
  if (stmt
      && TREE_CODE (stmt) == COND_EXPR)
    return false;

  return true;
}

/* For all loops, copy the condition at the end of the loop body in front
   of the loop.  This is beneficial since it increases efficiency of
   code motion optimizations.  It also saves one jump on entry to the loop.  */

static void
copy_loop_headers (void)
{
  struct loops *loops;
  unsigned i;
  struct loop *loop;
  basic_block header;
  edge preheader_edge;
  varray_type bbs_to_duplicate = NULL;

  loops = loop_optimizer_init (dump_file);
  if (!loops)
    return;
  
  /* We do not try to keep the information about irreducible regions
     up-to-date.  */
  loops->state &= ~LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS;

#ifdef ENABLE_CHECKING
  verify_loop_structure (loops);
#endif

  for (i = 1; i < loops->num; i++)
    {
      /* Copy at most 20 insns.  */
      int limit = 20;

      loop = loops->parray[i];
      preheader_edge = loop_preheader_edge (loop);
      header = preheader_edge->dest;

      /* If the loop is already a do-while style one (either because it was
	 written as such, or because jump threading transformed it into one),
	 we might be in fact peeling the first iteration of the loop.  This
	 in general is not a good idea.  */
      if (do_while_loop_p (loop))
	continue;

      /* Iterate the header copying up to limit; this takes care of the cases
	 like while (a && b) {...}, where we want to have both of the conditions
	 copied.  TODO -- handle while (a || b) - like cases, by not requiring
	 the header to have just a single successor and copying up to
	 postdominator. 
	 
	 We do not really copy the blocks immediately, so that we do not have
	 to worry about updating loop structures, and also so that we do not
	 have to rewrite variables out of and into ssa form for each block.
	 Instead we just record the block into worklist and duplicate all of
	 them at once.  */
      while (should_duplicate_loop_header_p (header, loop, &limit))
	{
	  if (!bbs_to_duplicate)
	    VARRAY_GENERIC_PTR_NOGC_INIT (bbs_to_duplicate, 10,
					  "bbs_to_duplicate");
	  VARRAY_PUSH_GENERIC_PTR_NOGC (bbs_to_duplicate, preheader_edge);
	  header->aux = &header->aux;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Scheduled basic block %d for duplication.\n",
		     header->index);

	  /* Find a successor of header that is inside a loop; i.e. the new
	     header after the condition is copied.  */
	  if (flow_bb_inside_loop_p (loop, header->succ->dest))
	    preheader_edge = header->succ;
	  else
	    preheader_edge = header->succ->succ_next;
	  header = preheader_edge->dest;
	}
    }

  loop_optimizer_finalize (loops, NULL);

  if (bbs_to_duplicate)
    {
      duplicate_blocks (bbs_to_duplicate);
      VARRAY_FREE (bbs_to_duplicate);
    }

  /* Run cleanup_tree_cfg here regardless of whether we have done anything, so
     that we cleanup the blocks created in order to get the loops into a
     canonical shape.  */
  cleanup_tree_cfg ();
}

static bool
gate_ch (void)
{
  return flag_tree_ch != 0;
}

struct tree_opt_pass pass_ch = 
{
  "ch",					/* name */
  gate_ch,				/* gate */
  copy_loop_headers,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_CH,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  (TODO_dump_func
   | TODO_verify_ssa)			/* todo_flags_finish */
};
