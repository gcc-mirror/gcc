/* Control flow functions for trees.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "errors.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "ggc.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "toplev.h"
#include "except.h"
#include "cfgloop.h"

/* This file contains functions for building the Control Flow Graph (CFG)
   for a function tree.  */

/* Local declarations.  */

/* Initial capacity for the basic block array.  */
static const int initial_cfg_capacity = 20;

/* Mapping of labels to their associated blocks.  This can greatly speed up
   building of the CFG in code with lots of gotos.  */
static GTY(()) varray_type label_to_block_map;

/* CFG statistics.  */
struct cfg_stats_d
{
  long num_merged_labels;
};

static struct cfg_stats_d cfg_stats;

/* Nonzero if we found a computed goto while building basic blocks.  */
static bool found_computed_goto;

/* Basic blocks and flowgraphs.  */
static basic_block create_bb (void *, void *, basic_block);
static void create_block_annotation (basic_block);
static void free_blocks_annotations (void);
static void clear_blocks_annotations (void);
static void make_blocks (tree);
static void factor_computed_gotos (void);

/* Edges.  */
static void make_edges (void);
static void make_ctrl_stmt_edges (basic_block);
static void make_exit_edges (basic_block);
static void make_cond_expr_edges (basic_block);
static void make_switch_expr_edges (basic_block);
static void make_goto_expr_edges (basic_block);
static edge tree_redirect_edge_and_branch (edge, basic_block);
static edge tree_try_redirect_by_replacing_jump (edge, basic_block);
static void split_critical_edges (void);

/* Various helpers.  */
static inline bool stmt_starts_bb_p (tree, tree);
static int tree_verify_flow_info (void);
static void tree_make_forwarder_block (edge);
static bool thread_jumps (void);
static bool tree_forwarder_block_p (basic_block);
static void bsi_commit_edge_inserts_1 (edge e);
static void tree_cfg2vcg (FILE *);

/* Flowgraph optimization and cleanup.  */
static void tree_merge_blocks (basic_block, basic_block);
static bool tree_can_merge_blocks_p (basic_block, basic_block);
static void remove_bb (basic_block);
static void group_case_labels (void);
static void cleanup_dead_labels (void);
static bool cleanup_control_flow (void);
static bool cleanup_control_expr_graph (basic_block, block_stmt_iterator);
static edge find_taken_edge_cond_expr (basic_block, tree);
static edge find_taken_edge_switch_expr (basic_block, tree);
static tree find_case_label_for_value (tree, tree);
static bool phi_alternatives_equal (basic_block, edge, edge);


/*---------------------------------------------------------------------------
			      Create basic blocks
---------------------------------------------------------------------------*/

/* Entry point to the CFG builder for trees.  TP points to the list of
   statements to be added to the flowgraph.  */

static void
build_tree_cfg (tree *tp)
{
  /* Register specific tree functions.  */
  tree_register_cfg_hooks ();

  /* Initialize rbi_pool.  */
  alloc_rbi_pool ();

  /* Initialize the basic block array.  */
  init_flow ();
  n_basic_blocks = 0;
  last_basic_block = 0;
  VARRAY_BB_INIT (basic_block_info, initial_cfg_capacity, "basic_block_info");
  memset ((void *) &cfg_stats, 0, sizeof (cfg_stats));

  /* Build a mapping of labels to their associated blocks.  */
  VARRAY_BB_INIT (label_to_block_map, initial_cfg_capacity,
		  "label to block map");

  ENTRY_BLOCK_PTR->next_bb = EXIT_BLOCK_PTR;
  EXIT_BLOCK_PTR->prev_bb = ENTRY_BLOCK_PTR;

  found_computed_goto = 0;
  make_blocks (*tp);

  /* Computed gotos are hell to deal with, especially if there are
     lots of them with a large number of destinations.  So we factor
     them to a common computed goto location before we build the
     edge list.  After we convert back to normal form, we will un-factor
     the computed gotos since factoring introduces an unwanted jump.  */
  if (found_computed_goto)
    factor_computed_gotos ();

  /* Make sure there is always at least one block, even if its empty.  */
  if (n_basic_blocks == 0)
    create_empty_bb (ENTRY_BLOCK_PTR);

  create_block_annotation (ENTRY_BLOCK_PTR);
  create_block_annotation (EXIT_BLOCK_PTR);
  
  /* Adjust the size of the array.  */
  VARRAY_GROW (basic_block_info, n_basic_blocks);

  /* To speed up statement iterator walks, we first purge dead labels.  */
  cleanup_dead_labels ();

  /* Group case nodes to reduce the number of edges.
     We do this after cleaning up dead labels because otherwise we miss
     a lot of obvious case merging opportunities.  */
  group_case_labels ();

  /* Create the edges of the flowgraph.  */
  make_edges ();

  /* Debugging dumps.  */

  /* Write the flowgraph to a VCG file.  */
  {
    int local_dump_flags;
    FILE *dump_file = dump_begin (TDI_vcg, &local_dump_flags);
    if (dump_file)
      {
	tree_cfg2vcg (dump_file);
	dump_end (TDI_vcg, dump_file);
      }
  }

  /* Dump a textual representation of the flowgraph.  */
  if (dump_file)
    dump_tree_cfg (dump_file, dump_flags);
}

static void
execute_build_cfg (void)
{
  build_tree_cfg (&DECL_SAVED_TREE (current_function_decl));
}

struct tree_opt_pass pass_build_cfg =
{
  "cfg",				/* name */
  NULL,					/* gate */
  execute_build_cfg,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_CFG,				/* tv_id */
  PROP_gimple_leh,			/* properties_required */
  PROP_cfg,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_verify_stmts			/* todo_flags_finish */
};

/* Search the CFG for any computed gotos.  If found, factor them to a 
   common computed goto site.  Also record the location of that site so
   that we can un-factor the gotos after we have converted back to 
   normal form.  */

static void
factor_computed_gotos (void)
{
  basic_block bb;
  tree factored_label_decl = NULL;
  tree var = NULL;
  tree factored_computed_goto_label = NULL;
  tree factored_computed_goto = NULL;

  /* We know there are one or more computed gotos in this function.
     Examine the last statement in each basic block to see if the block
     ends with a computed goto.  */
	
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi = bsi_last (bb);
      tree last;

      if (bsi_end_p (bsi))
	continue;
      last = bsi_stmt (bsi);

      /* Ignore the computed goto we create when we factor the original
	 computed gotos.  */
      if (last == factored_computed_goto)
	continue;

      /* If the last statement is a computed goto, factor it.  */
      if (computed_goto_p (last))
	{
	  tree assignment;

	  /* The first time we find a computed goto we need to create
	     the factored goto block and the variable each original
	     computed goto will use for their goto destination.  */
	  if (! factored_computed_goto)
	    {
	      basic_block new_bb = create_empty_bb (bb);
	      block_stmt_iterator new_bsi = bsi_start (new_bb);

	      /* Create the destination of the factored goto.  Each original
		 computed goto will put its desired destination into this
		 variable and jump to the label we create immediately
		 below.  */
	      var = create_tmp_var (ptr_type_node, "gotovar");

	      /* Build a label for the new block which will contain the
		 factored computed goto.  */
	      factored_label_decl = create_artificial_label ();
	      factored_computed_goto_label
		= build1 (LABEL_EXPR, void_type_node, factored_label_decl);
	      bsi_insert_after (&new_bsi, factored_computed_goto_label,
				BSI_NEW_STMT);

	      /* Build our new computed goto.  */
	      factored_computed_goto = build1 (GOTO_EXPR, void_type_node, var);
	      bsi_insert_after (&new_bsi, factored_computed_goto,
				BSI_NEW_STMT);
	    }

	  /* Copy the original computed goto's destination into VAR.  */
	  assignment = build (MODIFY_EXPR, ptr_type_node,
			      var, GOTO_DESTINATION (last));
	  bsi_insert_before (&bsi, assignment, BSI_SAME_STMT);

	  /* And re-vector the computed goto to the new destination.  */
	  GOTO_DESTINATION (last) = factored_label_decl;
	}
    }
}


/* Create annotations for a single basic block.  */

static void
create_block_annotation (basic_block bb)
{
  /* Verify that the tree_annotations field is clear.  */
  if (bb->tree_annotations)
    abort ();
  bb->tree_annotations = ggc_alloc_cleared (sizeof (struct bb_ann_d));
}


/* Free the annotations for all the basic blocks.  */

static void free_blocks_annotations (void)
{
  clear_blocks_annotations ();  
}


/* Clear the annotations for all the basic blocks.  */

static void
clear_blocks_annotations (void)
{
  basic_block bb;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, NULL, next_bb)
    bb->tree_annotations = NULL;
}


/* Build a flowgraph for the statement_list STMT_LIST.  */

static void
make_blocks (tree stmt_list)
{
  tree_stmt_iterator i = tsi_start (stmt_list);
  tree stmt = NULL;
  bool start_new_block = true;
  bool first_stmt_of_list = true;
  basic_block bb = ENTRY_BLOCK_PTR;

  while (!tsi_end_p (i))
    {
      tree prev_stmt;

      prev_stmt = stmt;
      stmt = tsi_stmt (i);

      /* If the statement starts a new basic block or if we have determined
	 in a previous pass that we need to create a new block for STMT, do
	 so now.  */
      if (start_new_block || stmt_starts_bb_p (stmt, prev_stmt))
	{
	  if (!first_stmt_of_list)
	    stmt_list = tsi_split_statement_list_before (&i);
	  bb = create_basic_block (stmt_list, NULL, bb);
	  start_new_block = false;
	}

      /* Now add STMT to BB and create the subgraphs for special statement
	 codes.  */
      set_bb_for_stmt (stmt, bb);

      if (computed_goto_p (stmt))
	found_computed_goto = true;

      /* If STMT is a basic block terminator, set START_NEW_BLOCK for the
	 next iteration.  */
      if (stmt_ends_bb_p (stmt))
	start_new_block = true;

      tsi_next (&i);
      first_stmt_of_list = false;
    }
}


/* Create and return a new empty basic block after bb AFTER.  */

static basic_block
create_bb (void *h, void *e, basic_block after)
{
  basic_block bb;

  if (e)
    abort ();

  /* Create and initialize a new basic block.  */
  bb = alloc_block ();
  memset (bb, 0, sizeof (*bb));

  bb->index = last_basic_block;
  bb->flags = BB_NEW;
  bb->stmt_list = h ? h : alloc_stmt_list ();

  /* Add the new block to the linked list of blocks.  */
  link_block (bb, after);

  /* Grow the basic block array if needed.  */
  if ((size_t) last_basic_block == VARRAY_SIZE (basic_block_info))
    {
      size_t new_size = last_basic_block + (last_basic_block + 3) / 4;
      VARRAY_GROW (basic_block_info, new_size);
    }

  /* Add the newly created block to the array.  */
  BASIC_BLOCK (last_basic_block) = bb;

  create_block_annotation (bb);

  n_basic_blocks++;
  last_basic_block++;

  initialize_bb_rbi (bb);
  return bb;
}


/*---------------------------------------------------------------------------
				 Edge creation
---------------------------------------------------------------------------*/

/* Join all the blocks in the flowgraph.  */

static void
make_edges (void)
{
  basic_block bb;

  /* Create an edge from entry to the first block with executable
     statements in it.  */
  make_edge (ENTRY_BLOCK_PTR, BASIC_BLOCK (0), EDGE_FALLTHRU);

  /* Traverse basic block array placing edges.  */
  FOR_EACH_BB (bb)
    {
      tree first = first_stmt (bb);
      tree last = last_stmt (bb);

      if (first)
	{
	  /* Edges for statements that always alter flow control.  */
	  if (is_ctrl_stmt (last))
	    make_ctrl_stmt_edges (bb);

	  /* Edges for statements that sometimes alter flow control.  */
	  if (is_ctrl_altering_stmt (last))
	    make_exit_edges (bb);
	}

      /* Finally, if no edges were created above, this is a regular
	 basic block that only needs a fallthru edge.  */
      if (bb->succ == NULL)
	make_edge (bb, bb->next_bb, EDGE_FALLTHRU);
    }

  /* We do not care about fake edges, so remove any that the CFG
     builder inserted for completeness.  */
  remove_fake_edges ();

  /* Clean up the graph and warn for unreachable code.  */
  cleanup_tree_cfg ();
}


/* Create edges for control statement at basic block BB.  */

static void
make_ctrl_stmt_edges (basic_block bb)
{
  tree last = last_stmt (bb);
  tree first = first_stmt (bb);

#if defined ENABLE_CHECKING
  if (last == NULL_TREE)
    abort();
#endif

  if (TREE_CODE (first) == LABEL_EXPR
      && DECL_NONLOCAL (LABEL_EXPR_LABEL (first)))
    make_edge (ENTRY_BLOCK_PTR, bb, EDGE_ABNORMAL);

  switch (TREE_CODE (last))
    {
    case GOTO_EXPR:
      make_goto_expr_edges (bb);
      break;

    case RETURN_EXPR:
      make_edge (bb, EXIT_BLOCK_PTR, 0);
      break;

    case COND_EXPR:
      make_cond_expr_edges (bb);
      break;

    case SWITCH_EXPR:
      make_switch_expr_edges (bb);
      break;

    case RESX_EXPR:
      make_eh_edges (last);
      /* Yet another NORETURN hack.  */
      if (bb->succ == NULL)
	make_edge (bb, EXIT_BLOCK_PTR, EDGE_FAKE);
      break;

    default:
      abort ();
    }
}


/* Create exit edges for statements in block BB that alter the flow of
   control.  Statements that alter the control flow are 'goto', 'return'
   and calls to non-returning functions.  */

static void
make_exit_edges (basic_block bb)
{
  tree last = last_stmt (bb), op;

  if (last == NULL_TREE)
    abort ();

  switch (TREE_CODE (last))
    {
    case CALL_EXPR:
      /* If this function receives a nonlocal goto, then we need to
	 make edges from this call site to all the nonlocal goto
	 handlers.  */
      if (TREE_SIDE_EFFECTS (last)
	  && current_function_has_nonlocal_label)
	make_goto_expr_edges (bb);

      /* If this statement has reachable exception handlers, then
	 create abnormal edges to them.  */
      make_eh_edges (last);

      /* Some calls are known not to return.  For such calls we create
	 a fake edge.

	 We really need to revamp how we build edges so that it's not
	 such a bloody pain to avoid creating edges for this case since
	 all we do is remove these edges when we're done building the
	 CFG.  */
      if (call_expr_flags (last) & (ECF_NORETURN | ECF_LONGJMP))
	{
	  make_edge (bb, EXIT_BLOCK_PTR, EDGE_FAKE);
	  return;
	}

      /* Don't forget the fall-thru edge.  */
      make_edge (bb, bb->next_bb, EDGE_FALLTHRU);
      break;

    case MODIFY_EXPR:
      /* A MODIFY_EXPR may have a CALL_EXPR on its RHS and the CALL_EXPR
	 may have an abnormal edge.  Search the RHS for this case and
	 create any required edges.  */
      op = get_call_expr_in (last);
      if (op && TREE_SIDE_EFFECTS (op)
	  && current_function_has_nonlocal_label)
	make_goto_expr_edges (bb);

      make_eh_edges (last);
      make_edge (bb, bb->next_bb, EDGE_FALLTHRU);
      break;

    default:
      abort ();
    }
}


/* Create the edges for a COND_EXPR starting at block BB.
   At this point, both clauses must contain only simple gotos.  */

static void
make_cond_expr_edges (basic_block bb)
{
  tree entry = last_stmt (bb);
  basic_block then_bb, else_bb;
  tree then_label, else_label;

#if defined ENABLE_CHECKING
  if (entry == NULL_TREE || TREE_CODE (entry) != COND_EXPR)
    abort ();
#endif

  /* Entry basic blocks for each component.  */
  then_label = GOTO_DESTINATION (COND_EXPR_THEN (entry));
  else_label = GOTO_DESTINATION (COND_EXPR_ELSE (entry));
  then_bb = label_to_block (then_label);
  else_bb = label_to_block (else_label);

  make_edge (bb, then_bb, EDGE_TRUE_VALUE);
  make_edge (bb, else_bb, EDGE_FALSE_VALUE);
}


/* Create the edges for a SWITCH_EXPR starting at block BB.
   At this point, the switch body has been lowered and the
   SWITCH_LABELS filled in, so this is in effect a multi-way branch.  */

static void
make_switch_expr_edges (basic_block bb)
{
  tree entry = last_stmt (bb);
  size_t i, n;
  tree vec;

  vec = SWITCH_LABELS (entry);
  n = TREE_VEC_LENGTH (vec);

  for (i = 0; i < n; ++i)
    {
      tree lab = CASE_LABEL (TREE_VEC_ELT (vec, i));
      basic_block label_bb = label_to_block (lab);
      make_edge (bb, label_bb, 0);
    }
}


/* Return the basic block holding label DEST.  */

basic_block
label_to_block (tree dest)
{
  int uid = LABEL_DECL_UID (dest);

  /* We would die hard when faced by undefined label.  Emit label to
     very first basic block.  This will hopefully make even the dataflow
     and undefined variable warnings quite right.  */
  if ((errorcount || sorrycount) && uid < 0)
    {
      block_stmt_iterator bsi = bsi_start (BASIC_BLOCK (0));
      tree stmt;

      stmt = build1 (LABEL_EXPR, void_type_node, dest);
      bsi_insert_before (&bsi, stmt, BSI_NEW_STMT);
      uid = LABEL_DECL_UID (dest);
    }
  return VARRAY_BB (label_to_block_map, uid);
}


/* Create edges for a goto statement at block BB.  */

static void
make_goto_expr_edges (basic_block bb)
{
  tree goto_t, dest;
  basic_block target_bb;
  int for_call;
  block_stmt_iterator last = bsi_last (bb);

  goto_t = bsi_stmt (last);

  /* If the last statement is not a GOTO (i.e., it is a RETURN_EXPR,
     CALL_EXPR or MODIFY_EXPR), then the edge is an abnormal edge resulting
     from a nonlocal goto.  */
  if (TREE_CODE (goto_t) != GOTO_EXPR)
    {
      dest = error_mark_node;
      for_call = 1;
    }
  else
    {
      dest = GOTO_DESTINATION (goto_t);
      for_call = 0;

      /* A GOTO to a local label creates normal edges.  */
      if (simple_goto_p (goto_t))
	{
	  edge e = make_edge (bb, label_to_block (dest), EDGE_FALLTHRU);
#ifdef USE_MAPPED_LOCATION
	  e->goto_locus = EXPR_LOCATION (goto_t);
#else
	  e->goto_locus = EXPR_LOCUS (goto_t);
#endif
	  bsi_remove (&last);
	  return;
	}

      /* Nothing more to do for nonlocal gotos.  */
      if (TREE_CODE (dest) == LABEL_DECL)
	return;

      /* Computed gotos remain.  */
    }

  /* Look for the block starting with the destination label.  In the
     case of a computed goto, make an edge to any label block we find
     in the CFG.  */
  FOR_EACH_BB (target_bb)
    {
      block_stmt_iterator bsi;

      for (bsi = bsi_start (target_bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree target = bsi_stmt (bsi);

	  if (TREE_CODE (target) != LABEL_EXPR)
	    break;

	  if (
	      /* Computed GOTOs.  Make an edge to every label block that has
		 been marked as a potential target for a computed goto.  */
	      (FORCED_LABEL (LABEL_EXPR_LABEL (target)) && for_call == 0)
	      /* Nonlocal GOTO target.  Make an edge to every label block
		 that has been marked as a potential target for a nonlocal
		 goto.  */
	      || (DECL_NONLOCAL (LABEL_EXPR_LABEL (target)) && for_call == 1))
	    {
	      make_edge (bb, target_bb, EDGE_ABNORMAL);
	      break;
	    }
	}
    }

  /* Degenerate case of computed goto with no labels.  */
  if (!for_call && !bb->succ)
    make_edge (bb, EXIT_BLOCK_PTR, EDGE_FAKE);
}


/*---------------------------------------------------------------------------
			       Flowgraph analysis
---------------------------------------------------------------------------*/

/* Remove unreachable blocks and other miscellaneous clean up work.  */

void
cleanup_tree_cfg (void)
{
  bool something_changed = true;

  timevar_push (TV_TREE_CLEANUP_CFG);

  /* These three transformations can cascade, so we iterate on them until
     nothing changes.  */
  while (something_changed)
    {
      something_changed = cleanup_control_flow ();
      something_changed |= thread_jumps ();
      something_changed |= delete_unreachable_blocks ();
    }

  /* Merging the blocks creates no new opportunities for the other
     optimizations, so do it here.  */
  merge_seq_blocks ();

  compact_blocks ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  timevar_pop (TV_TREE_CLEANUP_CFG);
}


/* Cleanup useless labels in basic blocks.  This is something we wish
   to do early because it allows us to group case labels before creating
   the edges for the CFG, and it speeds up block statement iterators in
   all passes later on.
   We only run this pass once, running it more than once is probably not
   profitable.  */

/* A map from basic block index to the leading label of that block.  */
static tree *label_for_bb;

/* Callback for for_each_eh_region.  Helper for cleanup_dead_labels.  */
static void
update_eh_label (struct eh_region *region)
{
  tree old_label = get_eh_region_tree_label (region);
  if (old_label)
    {
      tree new_label = label_for_bb[label_to_block (old_label)->index];
      set_eh_region_tree_label (region, new_label);
    }
}

/* Given LABEL return the first label in the same basic block.  */
static tree
main_block_label (tree label)
{
  basic_block bb = label_to_block (label);

  /* label_to_block possibly inserted undefined label into the chain.  */
  if (!label_for_bb[bb->index])
    label_for_bb[bb->index] = label;
  return label_for_bb[bb->index];
}

/* Cleanup redundant labels.  This is a three-steo process:
     1) Find the leading label for each block.
     2) Redirect all references to labels to the leading labels.
     3) Cleanup all useless labels.  */

static void
cleanup_dead_labels (void)
{
  basic_block bb;
  label_for_bb = xcalloc (last_basic_block, sizeof (tree));

  /* Find a suitable label for each block.  We use the first user-defined
     label is there is one, or otherwise just the first label we see.  */
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;

      for (i = bsi_start (bb); !bsi_end_p (i); bsi_next (&i))
	{
	  tree label, stmt = bsi_stmt (i);

	  if (TREE_CODE (stmt) != LABEL_EXPR)
	    break;

	  label = LABEL_EXPR_LABEL (stmt);

	  /* If we have not yet seen a label for the current block,
	     remember this one and see if there are more labels.  */
	  if (! label_for_bb[bb->index])
	    {
	      label_for_bb[bb->index] = label;
	      continue;
	    }

	  /* If we did see a label for the current block already, but it
	     is an artificially created label, replace it if the current
	     label is a user defined label.  */
	  if (! DECL_ARTIFICIAL (label)
	      && DECL_ARTIFICIAL (label_for_bb[bb->index]))
	    {
	      label_for_bb[bb->index] = label;
	      break;
	    }
	}
    }

  /* Now redirect all jumps/branches to the selected label.
     First do so for each block ending in a control statement.  */
  FOR_EACH_BB (bb)
    {
      tree stmt = last_stmt (bb);
      if (!stmt)
	continue;

      switch (TREE_CODE (stmt))
	{
	case COND_EXPR:
	  {
	    tree true_branch, false_branch;

	    true_branch = COND_EXPR_THEN (stmt);
	    false_branch = COND_EXPR_ELSE (stmt);

	    GOTO_DESTINATION (true_branch)
	      = main_block_label (GOTO_DESTINATION (true_branch));
	    GOTO_DESTINATION (false_branch)
	      = main_block_label (GOTO_DESTINATION (false_branch));

	    break;
	  }
  
	case SWITCH_EXPR:
	  {
	    size_t i;
	    tree vec = SWITCH_LABELS (stmt);
	    size_t n = TREE_VEC_LENGTH (vec);
  
	    /* Replace all destination labels.  */
	    for (i = 0; i < n; ++i)
	      CASE_LABEL (TREE_VEC_ELT (vec, i))
		= main_block_label (CASE_LABEL (TREE_VEC_ELT (vec, i)));
  
	    break;
	  }

	/* We have to handle GOTO_EXPRs until they're removed, and we don't
	   remove them until after we've created the CFG edges.  */
	case GOTO_EXPR:
          if (! computed_goto_p (stmt))
	    {
	      GOTO_DESTINATION (stmt)
		= main_block_label (GOTO_DESTINATION (stmt));
	      break;
	    }

	default:
	  break;
      }
    }

  for_each_eh_region (update_eh_label);

  /* Finally, purge dead labels.  All user-defined labels and labels that
     can be the target of non-local gotos are preserved.  */
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;
      tree label_for_this_bb = label_for_bb[bb->index];

      if (! label_for_this_bb)
	continue;

      for (i = bsi_start (bb); !bsi_end_p (i); )
	{
	  tree label, stmt = bsi_stmt (i);

	  if (TREE_CODE (stmt) != LABEL_EXPR)
	    break;

	  label = LABEL_EXPR_LABEL (stmt);

	  if (label == label_for_this_bb
	      || ! DECL_ARTIFICIAL (label)
	      || DECL_NONLOCAL (label))
	    bsi_next (&i);
	  else
	    bsi_remove (&i);
	}
    }

  free (label_for_bb);
}

/* Look for blocks ending in a multiway branch (a SWITCH_EXPR in GIMPLE),
   and scan the sorted vector of cases.  Combine the ones jumping to the
   same label.
   Eg. three separate entries 1: 2: 3: become one entry 1..3:  */

static void
group_case_labels (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      tree stmt = last_stmt (bb);
      if (stmt && TREE_CODE (stmt) == SWITCH_EXPR)
	{
	  tree labels = SWITCH_LABELS (stmt);
	  int old_size = TREE_VEC_LENGTH (labels);
	  int i, j, new_size = old_size;
	  tree default_label = TREE_VEC_ELT (labels, old_size - 1);

	  /* Look for possible opportunities to merge cases.
	     Ignore the last element of the label vector because it
	     must be the default case.  */
          i = 0;
	  while (i < old_size - 2)
	    {
	      tree base_case, base_label, base_high, type;
	      base_case = TREE_VEC_ELT (labels, i);

	      if (! base_case)
		abort ();

	      base_label = CASE_LABEL (base_case);

	      /* Discard cases that have the same destination as the
		 default case.  */
	      if (base_label == default_label)
		{
		  TREE_VEC_ELT (labels, i) = NULL_TREE;
		  i++;
		  continue;
		}

	      type = TREE_TYPE (CASE_LOW (base_case));
	      base_high = CASE_HIGH (base_case) ?
		CASE_HIGH (base_case) : CASE_LOW (base_case);

	      /* Try to merge case labels.  Break out when we reach the end
		 of the label vector or when we cannot merge the next case
		 label with the current one.  */
	      while (i < old_size - 2)
		{
		  tree merge_case = TREE_VEC_ELT (labels, ++i);
	          tree merge_label = CASE_LABEL (merge_case);
		  tree t = int_const_binop (PLUS_EXPR, base_high,
					    integer_one_node, 1);

		  /* Merge the cases if they jump to the same place,
		     and their ranges are consecutive.  */
		  if (merge_label == base_label
		      && tree_int_cst_equal (CASE_LOW (merge_case), t))
		    {
		      base_high = CASE_HIGH (merge_case) ?
			CASE_HIGH (merge_case) : CASE_LOW (merge_case);
		      CASE_HIGH (base_case) = base_high;
		      TREE_VEC_ELT (labels, i) = NULL_TREE;
		      new_size--;
		    }
		  else
		    break;
		}
	    }

	  /* Compress the case labels in the label vector, and adjust the
	     length of the vector.  */
	  for (i = 0, j = 0; i < new_size; i++)
	    {
	      while (! TREE_VEC_ELT (labels, j))
		j++;
	      TREE_VEC_ELT (labels, i) = TREE_VEC_ELT (labels, j++);
	    }
	  TREE_VEC_LENGTH (labels) = new_size;
	}
    }
}

/* Checks whether we can merge block B into block A.  */

static bool
tree_can_merge_blocks_p (basic_block a, basic_block b)
{
  tree stmt;
  block_stmt_iterator bsi;

  if (!a->succ
      || a->succ->succ_next)
    return false;

  if (a->succ->flags & EDGE_ABNORMAL)
    return false;

  if (a->succ->dest != b)
    return false;

  if (b == EXIT_BLOCK_PTR)
    return false;
  
  if (b->pred->pred_next)
    return false;

  /* If A ends by a statement causing exceptions or something similar, we
     cannot merge the blocks.  */
  stmt = last_stmt (a);
  if (stmt && stmt_ends_bb_p (stmt))
    return false;

  /* Do not allow a block with only a non-local label to be merged.  */
  if (stmt && TREE_CODE (stmt) == LABEL_EXPR
      && DECL_NONLOCAL (LABEL_EXPR_LABEL (stmt)))
    return false;

  /* There may be no phi nodes at the start of b.  Most of these degenerate
     phi nodes should be cleaned up by kill_redundant_phi_nodes.  */
  if (phi_nodes (b))
    return false;

  /* Do not remove user labels.  */
  for (bsi = bsi_start (b); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      stmt = bsi_stmt (bsi);
      if (TREE_CODE (stmt) != LABEL_EXPR)
	break;
      if (!DECL_ARTIFICIAL (LABEL_EXPR_LABEL (stmt)))
	return false;
    }

  return true;
}


/* Merge block B into block A.  */

static void
tree_merge_blocks (basic_block a, basic_block b)
{
  block_stmt_iterator bsi;
  tree_stmt_iterator last;

  if (dump_file)
    fprintf (dump_file, "Merging blocks %d and %d\n", a->index, b->index);

  /* Ensure that B follows A.  */
  move_block_after (b, a);

  if (!(a->succ->flags & EDGE_FALLTHRU))
    abort ();

  if (last_stmt (a)
      && stmt_ends_bb_p (last_stmt (a)))
    abort ();

  /* Remove labels from B and set bb_for_stmt to A for other statements.  */
  for (bsi = bsi_start (b); !bsi_end_p (bsi);)
    {
      if (TREE_CODE (bsi_stmt (bsi)) == LABEL_EXPR)
	bsi_remove (&bsi);
      else
	{
	  set_bb_for_stmt (bsi_stmt (bsi), a);
	  bsi_next (&bsi);
	}
    }

  /* Merge the chains.  */
  last = tsi_last (a->stmt_list);
  tsi_link_after (&last, b->stmt_list, TSI_NEW_STMT);
  b->stmt_list = NULL;
}


/* Walk the function tree removing unnecessary statements.

     * Empty statement nodes are removed

     * Unnecessary TRY_FINALLY and TRY_CATCH blocks are removed

     * Unnecessary COND_EXPRs are removed

     * Some unnecessary BIND_EXPRs are removed

   Clearly more work could be done.  The trick is doing the analysis
   and removal fast enough to be a net improvement in compile times.

   Note that when we remove a control structure such as a COND_EXPR
   BIND_EXPR, or TRY block, we will need to repeat this optimization pass
   to ensure we eliminate all the useless code.  */

struct rus_data
{
  tree *last_goto;
  bool repeat;
  bool may_throw;
  bool may_branch;
  bool has_label;
};

static void remove_useless_stmts_1 (tree *, struct rus_data *);

static bool
remove_useless_stmts_warn_notreached (tree stmt)
{
  if (EXPR_HAS_LOCATION (stmt))
    {
      location_t loc = EXPR_LOCATION (stmt);
      warning ("%Hwill never be executed", &loc);
      return true;
    }

  switch (TREE_CODE (stmt))
    {
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (stmt); !tsi_end_p (i); tsi_next (&i))
	  if (remove_useless_stmts_warn_notreached (tsi_stmt (i)))
	    return true;
      }
      break;

    case COND_EXPR:
      if (remove_useless_stmts_warn_notreached (COND_EXPR_COND (stmt)))
	return true;
      if (remove_useless_stmts_warn_notreached (COND_EXPR_THEN (stmt)))
	return true;
      if (remove_useless_stmts_warn_notreached (COND_EXPR_ELSE (stmt)))
	return true;
      break;

    case TRY_FINALLY_EXPR:
    case TRY_CATCH_EXPR:
      if (remove_useless_stmts_warn_notreached (TREE_OPERAND (stmt, 0)))
	return true;
      if (remove_useless_stmts_warn_notreached (TREE_OPERAND (stmt, 1)))
	return true;
      break;

    case CATCH_EXPR:
      return remove_useless_stmts_warn_notreached (CATCH_BODY (stmt));
    case EH_FILTER_EXPR:
      return remove_useless_stmts_warn_notreached (EH_FILTER_FAILURE (stmt));
    case BIND_EXPR:
      return remove_useless_stmts_warn_notreached (BIND_EXPR_BLOCK (stmt));

    default:
      /* Not a live container.  */
      break;
    }

  return false;
}

static void
remove_useless_stmts_cond (tree *stmt_p, struct rus_data *data)
{
  tree then_clause, else_clause, cond;
  bool save_has_label, then_has_label, else_has_label;

  save_has_label = data->has_label;
  data->has_label = false;
  data->last_goto = NULL;

  remove_useless_stmts_1 (&COND_EXPR_THEN (*stmt_p), data);

  then_has_label = data->has_label;
  data->has_label = false;
  data->last_goto = NULL;

  remove_useless_stmts_1 (&COND_EXPR_ELSE (*stmt_p), data);

  else_has_label = data->has_label;
  data->has_label = save_has_label | then_has_label | else_has_label;

  fold_stmt (stmt_p);
  then_clause = COND_EXPR_THEN (*stmt_p);
  else_clause = COND_EXPR_ELSE (*stmt_p);
  cond = COND_EXPR_COND (*stmt_p);

  /* If neither arm does anything at all, we can remove the whole IF.  */
  if (!TREE_SIDE_EFFECTS (then_clause) && !TREE_SIDE_EFFECTS (else_clause))
    {
      *stmt_p = build_empty_stmt ();
      data->repeat = true;
    }

  /* If there are no reachable statements in an arm, then we can
     zap the entire conditional.  */
  else if (integer_nonzerop (cond) && !else_has_label)
    {
      if (warn_notreached)
	remove_useless_stmts_warn_notreached (else_clause);
      *stmt_p = then_clause;
      data->repeat = true;
    }
  else if (integer_zerop (cond) && !then_has_label)
    {
      if (warn_notreached)
	remove_useless_stmts_warn_notreached (then_clause);
      *stmt_p = else_clause;
      data->repeat = true;
    }

  /* Check a couple of simple things on then/else with single stmts.  */
  else
    {
      tree then_stmt = expr_only (then_clause);
      tree else_stmt = expr_only (else_clause);

      /* Notice branches to a common destination.  */
      if (then_stmt && else_stmt
	  && TREE_CODE (then_stmt) == GOTO_EXPR
	  && TREE_CODE (else_stmt) == GOTO_EXPR
	  && (GOTO_DESTINATION (then_stmt) == GOTO_DESTINATION (else_stmt)))
	{
	  *stmt_p = then_stmt;
	  data->repeat = true;
	}

      /* If the THEN/ELSE clause merely assigns a value to a variable or
	 parameter which is already known to contain that value, then
	 remove the useless THEN/ELSE clause.  */
      else if (TREE_CODE (cond) == VAR_DECL || TREE_CODE (cond) == PARM_DECL)
	{
	  if (else_stmt
	      && TREE_CODE (else_stmt) == MODIFY_EXPR
	      && TREE_OPERAND (else_stmt, 0) == cond
	      && integer_zerop (TREE_OPERAND (else_stmt, 1)))
	    COND_EXPR_ELSE (*stmt_p) = alloc_stmt_list ();
	}
      else if ((TREE_CODE (cond) == EQ_EXPR || TREE_CODE (cond) == NE_EXPR)
	       && (TREE_CODE (TREE_OPERAND (cond, 0)) == VAR_DECL
		   || TREE_CODE (TREE_OPERAND (cond, 0)) == PARM_DECL)
	       && TREE_CONSTANT (TREE_OPERAND (cond, 1)))
	{
	  tree stmt = (TREE_CODE (cond) == EQ_EXPR
		       ? then_stmt : else_stmt);
	  tree *location = (TREE_CODE (cond) == EQ_EXPR
			    ? &COND_EXPR_THEN (*stmt_p)
			    : &COND_EXPR_ELSE (*stmt_p));

	  if (stmt
	      && TREE_CODE (stmt) == MODIFY_EXPR
	      && TREE_OPERAND (stmt, 0) == TREE_OPERAND (cond, 0)
	      && TREE_OPERAND (stmt, 1) == TREE_OPERAND (cond, 1))
	    *location = alloc_stmt_list ();
	}
    }

  /* Protect GOTOs in the arm of COND_EXPRs from being removed.  They
     would be re-introduced during lowering.  */
  data->last_goto = NULL;
}


static void
remove_useless_stmts_tf (tree *stmt_p, struct rus_data *data)
{
  bool save_may_branch, save_may_throw;
  bool this_may_branch, this_may_throw;

  /* Collect may_branch and may_throw information for the body only.  */
  save_may_branch = data->may_branch;
  save_may_throw = data->may_throw;
  data->may_branch = false;
  data->may_throw = false;
  data->last_goto = NULL;

  remove_useless_stmts_1 (&TREE_OPERAND (*stmt_p, 0), data);

  this_may_branch = data->may_branch;
  this_may_throw = data->may_throw;
  data->may_branch |= save_may_branch;
  data->may_throw |= save_may_throw;
  data->last_goto = NULL;

  remove_useless_stmts_1 (&TREE_OPERAND (*stmt_p, 1), data);

  /* If the body is empty, then we can emit the FINALLY block without
     the enclosing TRY_FINALLY_EXPR.  */
  if (!TREE_SIDE_EFFECTS (TREE_OPERAND (*stmt_p, 0)))
    {
      *stmt_p = TREE_OPERAND (*stmt_p, 1);
      data->repeat = true;
    }

  /* If the handler is empty, then we can emit the TRY block without
     the enclosing TRY_FINALLY_EXPR.  */
  else if (!TREE_SIDE_EFFECTS (TREE_OPERAND (*stmt_p, 1)))
    {
      *stmt_p = TREE_OPERAND (*stmt_p, 0);
      data->repeat = true;
    }

  /* If the body neither throws, nor branches, then we can safely
     string the TRY and FINALLY blocks together.  */
  else if (!this_may_branch && !this_may_throw)
    {
      tree stmt = *stmt_p;
      *stmt_p = TREE_OPERAND (stmt, 0);
      append_to_statement_list (TREE_OPERAND (stmt, 1), stmt_p);
      data->repeat = true;
    }
}


static void
remove_useless_stmts_tc (tree *stmt_p, struct rus_data *data)
{
  bool save_may_throw, this_may_throw;
  tree_stmt_iterator i;
  tree stmt;

  /* Collect may_throw information for the body only.  */
  save_may_throw = data->may_throw;
  data->may_throw = false;
  data->last_goto = NULL;

  remove_useless_stmts_1 (&TREE_OPERAND (*stmt_p, 0), data);

  this_may_throw = data->may_throw;
  data->may_throw = save_may_throw;

  /* If the body cannot throw, then we can drop the entire TRY_CATCH_EXPR.  */
  if (!this_may_throw)
    {
      if (warn_notreached)
	remove_useless_stmts_warn_notreached (TREE_OPERAND (*stmt_p, 1));
      *stmt_p = TREE_OPERAND (*stmt_p, 0);
      data->repeat = true;
      return;
    }

  /* Process the catch clause specially.  We may be able to tell that
     no exceptions propagate past this point.  */

  this_may_throw = true;
  i = tsi_start (TREE_OPERAND (*stmt_p, 1));
  stmt = tsi_stmt (i);
  data->last_goto = NULL;

  switch (TREE_CODE (stmt))
    {
    case CATCH_EXPR:
      for (; !tsi_end_p (i); tsi_next (&i))
	{
	  stmt = tsi_stmt (i);
	  /* If we catch all exceptions, then the body does not
	     propagate exceptions past this point.  */
	  if (CATCH_TYPES (stmt) == NULL)
	    this_may_throw = false;
	  data->last_goto = NULL;
	  remove_useless_stmts_1 (&CATCH_BODY (stmt), data);
	}
      break;

    case EH_FILTER_EXPR:
      if (EH_FILTER_MUST_NOT_THROW (stmt))
	this_may_throw = false;
      else if (EH_FILTER_TYPES (stmt) == NULL)
	this_may_throw = false;
      remove_useless_stmts_1 (&EH_FILTER_FAILURE (stmt), data);
      break;

    default:
      /* Otherwise this is a cleanup.  */
      remove_useless_stmts_1 (&TREE_OPERAND (*stmt_p, 1), data);

      /* If the cleanup is empty, then we can emit the TRY block without
	 the enclosing TRY_CATCH_EXPR.  */
      if (!TREE_SIDE_EFFECTS (TREE_OPERAND (*stmt_p, 1)))
	{
	  *stmt_p = TREE_OPERAND (*stmt_p, 0);
	  data->repeat = true;
	}
      break;
    }
  data->may_throw |= this_may_throw;
}


static void
remove_useless_stmts_bind (tree *stmt_p, struct rus_data *data)
{
  tree block;

  /* First remove anything underneath the BIND_EXPR.  */
  remove_useless_stmts_1 (&BIND_EXPR_BODY (*stmt_p), data);

  /* If the BIND_EXPR has no variables, then we can pull everything
     up one level and remove the BIND_EXPR, unless this is the toplevel
     BIND_EXPR for the current function or an inlined function.

     When this situation occurs we will want to apply this
     optimization again.  */
  block = BIND_EXPR_BLOCK (*stmt_p);
  if (BIND_EXPR_VARS (*stmt_p) == NULL_TREE
      && *stmt_p != DECL_SAVED_TREE (current_function_decl)
      && (! block
	  || ! BLOCK_ABSTRACT_ORIGIN (block)
	  || (TREE_CODE (BLOCK_ABSTRACT_ORIGIN (block))
	      != FUNCTION_DECL)))
    {
      *stmt_p = BIND_EXPR_BODY (*stmt_p);
      data->repeat = true;
    }
}


static void
remove_useless_stmts_goto (tree *stmt_p, struct rus_data *data)
{
  tree dest = GOTO_DESTINATION (*stmt_p);

  data->may_branch = true;
  data->last_goto = NULL;

  /* Record the last goto expr, so that we can delete it if unnecessary.  */
  if (TREE_CODE (dest) == LABEL_DECL)
    data->last_goto = stmt_p;
}


static void
remove_useless_stmts_label (tree *stmt_p, struct rus_data *data)
{
  tree label = LABEL_EXPR_LABEL (*stmt_p);

  data->has_label = true;

  /* We do want to jump across non-local label receiver code.  */
  if (DECL_NONLOCAL (label))
    data->last_goto = NULL;

  else if (data->last_goto && GOTO_DESTINATION (*data->last_goto) == label)
    {
      *data->last_goto = build_empty_stmt ();
      data->repeat = true;
    }

  /* ??? Add something here to delete unused labels.  */
}


/* If the function is "const" or "pure", then clear TREE_SIDE_EFFECTS on its
   decl.  This allows us to eliminate redundant or useless
   calls to "const" functions. 

   Gimplifier already does the same operation, but we may notice functions
   being const and pure once their calls has been gimplified, so we need
   to update the flag.  */

static void
update_call_expr_flags (tree call)
{
  tree decl = get_callee_fndecl (call);
  if (!decl)
    return;
  if (call_expr_flags (call) & (ECF_CONST | ECF_PURE))
    TREE_SIDE_EFFECTS (call) = 0;
  if (TREE_NOTHROW (decl))
    TREE_NOTHROW (call) = 1;
}


/* T is CALL_EXPR.  Set current_function_calls_* flags.  */

void
notice_special_calls (tree t)
{
  int flags = call_expr_flags (t);

  if (flags & ECF_MAY_BE_ALLOCA)
    current_function_calls_alloca = true;
  if (flags & ECF_RETURNS_TWICE)
    current_function_calls_setjmp = true;
}


/* Clear flags set by notice_special_calls.  Used by dead code removal
   to update the flags.  */

void
clear_special_calls (void)
{
  current_function_calls_alloca = false;
  current_function_calls_setjmp = false;
}


static void
remove_useless_stmts_1 (tree *tp, struct rus_data *data)
{
  tree t = *tp, op;

  switch (TREE_CODE (t))
    {
    case COND_EXPR:
      remove_useless_stmts_cond (tp, data);
      break;

    case TRY_FINALLY_EXPR:
      remove_useless_stmts_tf (tp, data);
      break;

    case TRY_CATCH_EXPR:
      remove_useless_stmts_tc (tp, data);
      break;

    case BIND_EXPR:
      remove_useless_stmts_bind (tp, data);
      break;

    case GOTO_EXPR:
      remove_useless_stmts_goto (tp, data);
      break;

    case LABEL_EXPR:
      remove_useless_stmts_label (tp, data);
      break;

    case RETURN_EXPR:
      fold_stmt (tp);
      data->last_goto = NULL;
      data->may_branch = true;
      break;

    case CALL_EXPR:
      fold_stmt (tp);
      data->last_goto = NULL;
      notice_special_calls (t);
      update_call_expr_flags (t);
      if (tree_could_throw_p (t))
	data->may_throw = true;
      break;

    case MODIFY_EXPR:
      data->last_goto = NULL;
      fold_stmt (tp);
      op = get_call_expr_in (t);
      if (op)
	{
	  update_call_expr_flags (op);
	  notice_special_calls (op);
	}
      if (tree_could_throw_p (t))
	data->may_throw = true;
      break;

    case STATEMENT_LIST:
      {
	tree_stmt_iterator i = tsi_start (t);
	while (!tsi_end_p (i))
	  {
	    t = tsi_stmt (i);
	    if (IS_EMPTY_STMT (t))
	      {
		tsi_delink (&i);
		continue;
	      }
	    
	    remove_useless_stmts_1 (tsi_stmt_ptr (i), data);

	    t = tsi_stmt (i);
	    if (TREE_CODE (t) == STATEMENT_LIST)
	      {
		tsi_link_before (&i, t, TSI_SAME_STMT);
		tsi_delink (&i);
	      }
	    else
	      tsi_next (&i);
	  }
      }
      break;
    case SWITCH_EXPR:
      fold_stmt (tp);
      data->last_goto = NULL;
      break;

    default:
      data->last_goto = NULL;
      break;
    }
}

static void
remove_useless_stmts (void)
{
  struct rus_data data;

  clear_special_calls ();

  do
    {
      memset (&data, 0, sizeof (data));
      remove_useless_stmts_1 (&DECL_SAVED_TREE (current_function_decl), &data);
    }
  while (data.repeat);
}


struct tree_opt_pass pass_remove_useless_stmts = 
{
  "useless",				/* name */
  NULL,					/* gate */
  remove_useless_stmts,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_gimple_any,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func			/* todo_flags_finish */
};


/* Remove obviously useless statements in basic block BB.  */

static void
cfg_remove_useless_stmts_bb (basic_block bb)
{
  block_stmt_iterator bsi;
  tree stmt = NULL_TREE;
  tree cond, var = NULL_TREE, val = NULL_TREE;
  struct var_ann_d *ann;

  /* Check whether we come here from a condition, and if so, get the
     condition.  */
  if (!bb->pred
      || bb->pred->pred_next
      || !(bb->pred->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
    return;

  cond = COND_EXPR_COND (last_stmt (bb->pred->src));

  if (TREE_CODE (cond) == VAR_DECL || TREE_CODE (cond) == PARM_DECL)
    {
      var = cond;
      val = (bb->pred->flags & EDGE_FALSE_VALUE
	     ? boolean_false_node : boolean_true_node);
    }
  else if (TREE_CODE (cond) == TRUTH_NOT_EXPR
	   && (TREE_CODE (TREE_OPERAND (cond, 0)) == VAR_DECL
	       || TREE_CODE (TREE_OPERAND (cond, 0)) == PARM_DECL))
    {
      var = TREE_OPERAND (cond, 0);
      val = (bb->pred->flags & EDGE_FALSE_VALUE
	     ? boolean_true_node : boolean_false_node);
    }
  else
    {
      if (bb->pred->flags & EDGE_FALSE_VALUE)
	cond = invert_truthvalue (cond);
      if (TREE_CODE (cond) == EQ_EXPR
	  && (TREE_CODE (TREE_OPERAND (cond, 0)) == VAR_DECL
	      || TREE_CODE (TREE_OPERAND (cond, 0)) == PARM_DECL)
	  && (TREE_CODE (TREE_OPERAND (cond, 1)) == VAR_DECL
	      || TREE_CODE (TREE_OPERAND (cond, 1)) == PARM_DECL
	      || TREE_CONSTANT (TREE_OPERAND (cond, 1))))
	{
	  var = TREE_OPERAND (cond, 0);
	  val = TREE_OPERAND (cond, 1);
	}
      else
	return;
    }

  /* Only work for normal local variables.  */
  ann = var_ann (var);
  if (!ann
      || ann->may_aliases
      || TREE_ADDRESSABLE (var))
    return;

  if (! TREE_CONSTANT (val))
    {
      ann = var_ann (val);
      if (!ann
	  || ann->may_aliases
	  || TREE_ADDRESSABLE (val))
	return;
    }

  /* Ignore floating point variables, since comparison behaves weird for
     them.  */
  if (FLOAT_TYPE_P (TREE_TYPE (var)))
    return;

  for (bsi = bsi_start (bb); !bsi_end_p (bsi);)
    {
      stmt = bsi_stmt (bsi);

      /* If the THEN/ELSE clause merely assigns a value to a variable/parameter
	 which is already known to contain that value, then remove the useless
	 THEN/ELSE clause.  */
      if (TREE_CODE (stmt) == MODIFY_EXPR
	  && TREE_OPERAND (stmt, 0) == var
	  && operand_equal_p (val, TREE_OPERAND (stmt, 1), 0))
	{
	  bsi_remove (&bsi);
	  continue;
	}

      /* Invalidate the var if we encounter something that could modify it.  */
      if (TREE_CODE (stmt) == ASM_EXPR
	  || TREE_CODE (stmt) == VA_ARG_EXPR
	  || (TREE_CODE (stmt) == MODIFY_EXPR
	      && (TREE_OPERAND (stmt, 0) == var
		  || TREE_OPERAND (stmt, 0) == val
		  || TREE_CODE (TREE_OPERAND (stmt, 1)) == VA_ARG_EXPR)))
	return;
  
      bsi_next (&bsi);
    }
}


/* A CFG-aware version of remove_useless_stmts.  */

void
cfg_remove_useless_stmts (void)
{
  basic_block bb;

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  FOR_EACH_BB (bb)
    {
      cfg_remove_useless_stmts_bb (bb);
    }
}


/* Remove PHI nodes associated with basic block BB and all edges out of BB.  */

static void
remove_phi_nodes_and_edges_for_unreachable_block (basic_block bb)
{
  tree phi;

  /* Since this block is no longer reachable, we can just delete all
     of its PHI nodes.  */
  phi = phi_nodes (bb);
  while (phi)
    {
      tree next = PHI_CHAIN (phi);
      remove_phi_node (phi, NULL_TREE, bb);
      phi = next;
    }

  /* Remove edges to BB's successors.  */
  while (bb->succ != NULL)
    ssa_remove_edge (bb->succ);
}


/* Remove statements of basic block BB.  */

static void
remove_bb (basic_block bb)
{
  block_stmt_iterator i;
  source_locus loc = 0;

  if (dump_file)
    {
      fprintf (dump_file, "Removing basic block %d\n", bb->index);
      if (dump_flags & TDF_DETAILS)
	{
	  dump_bb (bb, dump_file, 0);
	  fprintf (dump_file, "\n");
	}
    }

  /* Remove all the instructions in the block.  */
  for (i = bsi_start (bb); !bsi_end_p (i); bsi_remove (&i))
    {
      tree stmt = bsi_stmt (i);

      set_bb_for_stmt (stmt, NULL);

      /* Don't warn for removed gotos.  Gotos are often removed due to
	 jump threading, thus resulting in bogus warnings.  Not great,
	 since this way we lose warnings for gotos in the original
	 program that are indeed unreachable.  */
      if (TREE_CODE (stmt) != GOTO_EXPR && EXPR_HAS_LOCATION (stmt) && !loc)
#ifdef USE_MAPPED_LOCATION
	loc = EXPR_LOCATION (stmt);
#else
	loc = EXPR_LOCUS (stmt);
#endif
    }

  /* If requested, give a warning that the first statement in the
     block is unreachable.  We walk statements backwards in the
     loop above, so the last statement we process is the first statement
     in the block.  */
  if (warn_notreached && loc)
#ifdef USE_MAPPED_LOCATION
    warning ("%Hwill never be executed", &loc);
#else
    warning ("%Hwill never be executed", loc);
#endif

  remove_phi_nodes_and_edges_for_unreachable_block (bb);
}


/* Examine BB to determine if it is a forwarding block (a block which only
   transfers control to a new destination).  If BB is a forwarding block,
   then return the edge leading to the ultimate destination.  */

edge
tree_block_forwards_to (basic_block bb)
{
  block_stmt_iterator bsi;
  bb_ann_t ann = bb_ann (bb);
  tree stmt;

  /* If this block is not forwardable, then avoid useless work.  */
  if (! ann->forwardable)
    return NULL;

  /* Set this block to not be forwardable.  This prevents infinite loops since
     any block currently under examination is considered non-forwardable.  */
  ann->forwardable = 0;

  /* No forwarding is possible if this block is a special block (ENTRY/EXIT),
     this block has more than one successor, this block's single successor is
     reached via an abnormal edge, this block has phi nodes, or this block's
     single successor has phi nodes.  */
  if (bb == EXIT_BLOCK_PTR
      || bb == ENTRY_BLOCK_PTR
      || !bb->succ
      || bb->succ->succ_next
      || bb->succ->dest == EXIT_BLOCK_PTR
      || (bb->succ->flags & EDGE_ABNORMAL) != 0
      || phi_nodes (bb)
      || phi_nodes (bb->succ->dest))
    return NULL;

  /* Walk past any labels at the start of this block.  */
  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      stmt = bsi_stmt (bsi);
      if (TREE_CODE (stmt) != LABEL_EXPR)
	break;
    }

  /* If we reached the end of this block we may be able to optimize this
     case.  */
  if (bsi_end_p (bsi))
    {
      edge dest;

      /* Recursive call to pick up chains of forwarding blocks.  */
      dest = tree_block_forwards_to (bb->succ->dest);

      /* If none found, we forward to bb->succ at minimum.  */
      if (!dest)
	dest = bb->succ;

      ann->forwardable = 1;
      return dest;
    }

  /* No forwarding possible.  */
  return NULL;
}


/* Try to remove superfluous control structures.  */

static bool
cleanup_control_flow (void)
{
  basic_block bb;
  block_stmt_iterator bsi;
  bool retval = false;
  tree stmt;

  FOR_EACH_BB (bb)
    {
      bsi = bsi_last (bb);

      if (bsi_end_p (bsi))
	continue;
      
      stmt = bsi_stmt (bsi);
      if (TREE_CODE (stmt) == COND_EXPR
	  || TREE_CODE (stmt) == SWITCH_EXPR)
	retval |= cleanup_control_expr_graph (bb, bsi);
    }
  return retval;
}


/* Disconnect an unreachable block in the control expression starting
   at block BB.  */

static bool
cleanup_control_expr_graph (basic_block bb, block_stmt_iterator bsi)
{
  edge taken_edge;
  bool retval = false;
  tree expr = bsi_stmt (bsi), val;

  if (bb->succ->succ_next)
    {
      edge e, next;

      switch (TREE_CODE (expr))
	{
	case COND_EXPR:
	  val = COND_EXPR_COND (expr);
	  break;

	case SWITCH_EXPR:
	  val = SWITCH_COND (expr);
	  if (TREE_CODE (val) != INTEGER_CST)
	    return false;
	  break;

	default:
	  abort ();
	}

      taken_edge = find_taken_edge (bb, val);
      if (!taken_edge)
	return false;

      /* Remove all the edges except the one that is always executed.  */
      for (e = bb->succ; e; e = next)
	{
	  next = e->succ_next;
	  if (e != taken_edge)
	    {
	      taken_edge->probability += e->probability;
	      taken_edge->count += e->count;
	      ssa_remove_edge (e);
	      retval = true;
	    }
	}
      if (taken_edge->probability > REG_BR_PROB_BASE)
	taken_edge->probability = REG_BR_PROB_BASE;
    }
  else
    taken_edge = bb->succ;

  bsi_remove (&bsi);
  taken_edge->flags = EDGE_FALLTHRU;

  /* We removed some paths from the cfg.  */
  if (dom_computed[CDI_DOMINATORS] >= DOM_CONS_OK)
    dom_computed[CDI_DOMINATORS] = DOM_CONS_OK;

  return retval;
}


/* Given a control block BB and a predicate VAL, return the edge that
   will be taken out of the block.  If VAL does not match a unique
   edge, NULL is returned. */

edge
find_taken_edge (basic_block bb, tree val)
{
  tree stmt;

  stmt = last_stmt (bb);

#if defined ENABLE_CHECKING
  if (stmt == NULL_TREE || !is_ctrl_stmt (stmt))
    abort ();
#endif

  /* If VAL is a predicate of the form N RELOP N, where N is an
     SSA_NAME, we can always determine its truth value (except when
     doing floating point comparisons that may involve NaNs).  */
  if (val
      && TREE_CODE_CLASS (TREE_CODE (val)) == '<'
      && TREE_OPERAND (val, 0) == TREE_OPERAND (val, 1)
      && TREE_CODE (TREE_OPERAND (val, 0)) == SSA_NAME
      && (TREE_CODE (TREE_TYPE (TREE_OPERAND (val, 0))) != REAL_TYPE
	  || !HONOR_NANS (TYPE_MODE (TREE_TYPE (TREE_OPERAND (val, 0))))))
    {
      enum tree_code code = TREE_CODE (val);

      if (code == EQ_EXPR || code == LE_EXPR || code == GE_EXPR)
	val = boolean_true_node;
      else if (code == LT_EXPR || code == GT_EXPR || code == NE_EXPR)
	val = boolean_false_node;
    }

  /* If VAL is not a constant, we can't determine which edge might
     be taken.  */
  if (val == NULL || !really_constant_p (val))
    return NULL;

  if (TREE_CODE (stmt) == COND_EXPR)
    return find_taken_edge_cond_expr (bb, val);

  if (TREE_CODE (stmt) == SWITCH_EXPR)
    return find_taken_edge_switch_expr (bb, val);

  return bb->succ;
}


/* Given a constant value VAL and the entry block BB to a COND_EXPR
   statement, determine which of the two edges will be taken out of the
   block.  Return NULL if either edge may be taken.  */

static edge
find_taken_edge_cond_expr (basic_block bb, tree val)
{
  edge true_edge, false_edge;

  extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

  /* If both edges of the branch lead to the same basic block, it doesn't
     matter which edge is taken.  */
  if (true_edge->dest == false_edge->dest)
    return true_edge;

  /* Otherwise, try to determine which branch of the if() will be taken.
     If VAL is a constant but it can't be reduced to a 0 or a 1, then
     we don't really know which edge will be taken at runtime.  This
     may happen when comparing addresses (e.g., if (&var1 == 4)).  */
  if (integer_nonzerop (val))
    return true_edge;
  else if (integer_zerop (val))
    return false_edge;
  else
    return NULL;
}


/* Given a constant value VAL and the entry block BB to a SWITCH_EXPR
   statement, determine which edge will be taken out of the block.  Return
   NULL if any edge may be taken.  */

static edge
find_taken_edge_switch_expr (basic_block bb, tree val)
{
  tree switch_expr, taken_case;
  basic_block dest_bb;
  edge e;

  if (TREE_CODE (val) != INTEGER_CST)
    return NULL;

  switch_expr = last_stmt (bb);
  taken_case = find_case_label_for_value (switch_expr, val);
  dest_bb = label_to_block (CASE_LABEL (taken_case));

  e = find_edge (bb, dest_bb);
  if (!e)
    abort ();
  return e;
}


/* Return the CASE_LABEL_EXPR that SWITCH_EXPR will take for VAL.
   We can make optimal use here of the fact that the case labels are
   sorted: We can do a binary search for a case matching VAL.  */

static tree
find_case_label_for_value (tree switch_expr, tree val)
{
  tree vec = SWITCH_LABELS (switch_expr);
  size_t low, high, n = TREE_VEC_LENGTH (vec);
  tree default_case = TREE_VEC_ELT (vec, n - 1);

  for (low = -1, high = n - 1; high - low > 1; )
    {
      size_t i = (high + low) / 2;
      tree t = TREE_VEC_ELT (vec, i);
      int cmp;

      /* Cache the result of comparing CASE_LOW and val.  */
      cmp = tree_int_cst_compare (CASE_LOW (t), val);

      if (cmp > 0)
	high = i;
      else
	low = i;

      if (CASE_HIGH (t) == NULL)
	{
	  /* A singe-valued case label.  */
	  if (cmp == 0)
	    return t;
	}
      else
	{
	  /* A case range.  We can only handle integer ranges.  */
	  if (cmp <= 0 && tree_int_cst_compare (CASE_HIGH (t), val) >= 0)
	    return t;
	}
    }

  return default_case;
}


/* If all the PHI nodes in DEST have alternatives for E1 and E2 and
   those alternatives are equal in each of the PHI nodes, then return
   true, else return false.  */

static bool
phi_alternatives_equal (basic_block dest, edge e1, edge e2)
{
  tree phi, val1, val2;
  int n1, n2;

  for (phi = phi_nodes (dest); phi; phi = PHI_CHAIN (phi))
    {
      n1 = phi_arg_from_edge (phi, e1);
      n2 = phi_arg_from_edge (phi, e2);

#ifdef ENABLE_CHECKING
      if (n1 < 0 || n2 < 0)
	abort ();
#endif

      val1 = PHI_ARG_DEF (phi, n1);
      val2 = PHI_ARG_DEF (phi, n2);

      if (!operand_equal_p (val1, val2, 0))
	return false;
    }

  return true;
}


/* Computing the Dominance Frontier:

   As described in Morgan, section 3.5, this may be done simply by
   walking the dominator tree bottom-up, computing the frontier for
   the children before the parent.  When considering a block B,
   there are two cases:

   (1) A flow graph edge leaving B that does not lead to a child
   of B in the dominator tree must be a block that is either equal
   to B or not dominated by B.  Such blocks belong in the frontier
   of B.

   (2) Consider a block X in the frontier of one of the children C
   of B.  If X is not equal to B and is not dominated by B, it
   is in the frontier of B.  */

static void
compute_dominance_frontiers_1 (bitmap *frontiers, basic_block bb, sbitmap done)
{
  edge e;
  basic_block c;

  SET_BIT (done, bb->index);

  /* Do the frontier of the children first.  Not all children in the
     dominator tree (blocks dominated by this one) are children in the
     CFG, so check all blocks.  */
  for (c = first_dom_son (CDI_DOMINATORS, bb);
       c;
       c = next_dom_son (CDI_DOMINATORS, c))
    {
      if (! TEST_BIT (done, c->index))
    	compute_dominance_frontiers_1 (frontiers, c, done);
    }
      
  /* Find blocks conforming to rule (1) above.  */
  for (e = bb->succ; e; e = e->succ_next)
    {
      if (e->dest == EXIT_BLOCK_PTR)
	continue;
      if (get_immediate_dominator (CDI_DOMINATORS, e->dest) != bb)
	bitmap_set_bit (frontiers[bb->index], e->dest->index);
    }

  /* Find blocks conforming to rule (2).  */
  for (c = first_dom_son (CDI_DOMINATORS, bb);
       c;
       c = next_dom_son (CDI_DOMINATORS, c))
    {
      int x;

      EXECUTE_IF_SET_IN_BITMAP (frontiers[c->index], 0, x,
	{
	  if (get_immediate_dominator (CDI_DOMINATORS, BASIC_BLOCK (x)) != bb)
	    bitmap_set_bit (frontiers[bb->index], x);
	});
    }
}


void
compute_dominance_frontiers (bitmap *frontiers)
{
  sbitmap done = sbitmap_alloc (last_basic_block);

  timevar_push (TV_DOM_FRONTIERS);

  sbitmap_zero (done);

  compute_dominance_frontiers_1 (frontiers, ENTRY_BLOCK_PTR->succ->dest, done);

  sbitmap_free (done);

  timevar_pop (TV_DOM_FRONTIERS);
}



/*---------------------------------------------------------------------------
			      Debugging functions
---------------------------------------------------------------------------*/

/* Dump tree-specific information of block BB to file OUTF.  */

void
tree_dump_bb (basic_block bb, FILE *outf, int indent)
{
  dump_generic_bb (outf, bb, indent, TDF_VOPS);
}


/* Dump a basic block on stderr.  */

void
debug_tree_bb (basic_block bb)
{
  dump_bb (bb, stderr, 0);
}


/* Dump basic block with index N on stderr.  */

basic_block
debug_tree_bb_n (int n)
{
  debug_tree_bb (BASIC_BLOCK (n));
  return BASIC_BLOCK (n);
}	 


/* Dump the CFG on stderr.

   FLAGS are the same used by the tree dumping functions
   (see TDF_* in tree.h).  */

void
debug_tree_cfg (int flags)
{
  dump_tree_cfg (stderr, flags);
}


/* Dump the program showing basic block boundaries on the given FILE.

   FLAGS are the same used by the tree dumping functions (see TDF_* in
   tree.h).  */

void
dump_tree_cfg (FILE *file, int flags)
{
  if (flags & TDF_DETAILS)
    {
      const char *funcname
	= lang_hooks.decl_printable_name (current_function_decl, 2);

      fputc ('\n', file);
      fprintf (file, ";; Function %s\n\n", funcname);
      fprintf (file, ";; \n%d basic blocks, %d edges, last basic block %d.\n\n",
	       n_basic_blocks, n_edges, last_basic_block);

      brief_dump_cfg (file);
      fprintf (file, "\n");
    }

  if (flags & TDF_STATS)
    dump_cfg_stats (file);

  dump_function_to_file (current_function_decl, file, flags | TDF_BLOCKS);
}


/* Dump CFG statistics on FILE.  */

void
dump_cfg_stats (FILE *file)
{
  static long max_num_merged_labels = 0;
  unsigned long size, total = 0;
  int n_edges;
  basic_block bb;
  const char * const fmt_str   = "%-30s%-13s%12s\n";
  const char * const fmt_str_1 = "%-30s%13d%11lu%c\n";
  const char * const fmt_str_3 = "%-43s%11lu%c\n";
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);


  fprintf (file, "\nCFG Statistics for %s\n\n", funcname);

  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, fmt_str, "", "  Number of  ", "Memory");
  fprintf (file, fmt_str, "", "  instances  ", "used ");
  fprintf (file, "---------------------------------------------------------\n");

  size = n_basic_blocks * sizeof (struct basic_block_def);
  total += size;
  fprintf (file, fmt_str_1, "Basic blocks", n_basic_blocks,
	   SCALE (size), LABEL (size));

  n_edges = 0;
  FOR_EACH_BB (bb)
    {
      edge e;
      for (e = bb->succ; e; e = e->succ_next)
	n_edges++;
    }
  size = n_edges * sizeof (struct edge_def);
  total += size;
  fprintf (file, fmt_str_1, "Edges", n_edges, SCALE (size), LABEL (size));

  size = n_basic_blocks * sizeof (struct bb_ann_d);
  total += size;
  fprintf (file, fmt_str_1, "Basic block annotations", n_basic_blocks,
	   SCALE (size), LABEL (size));

  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, fmt_str_3, "Total memory used by CFG data", SCALE (total),
	   LABEL (total));
  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, "\n");

  if (cfg_stats.num_merged_labels > max_num_merged_labels)
    max_num_merged_labels = cfg_stats.num_merged_labels;

  fprintf (file, "Coalesced label blocks: %ld (Max so far: %ld)\n",
	   cfg_stats.num_merged_labels, max_num_merged_labels);

  fprintf (file, "\n");
}


/* Dump CFG statistics on stderr.  Keep extern so that it's always
   linked in the final executable.  */

void
debug_cfg_stats (void)
{
  dump_cfg_stats (stderr);
}


/* Dump the flowgraph to a .vcg FILE.  */

static void
tree_cfg2vcg (FILE *file)
{
  edge e;
  basic_block bb;
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);

  /* Write the file header.  */
  fprintf (file, "graph: { title: \"%s\"\n", funcname);
  fprintf (file, "node: { title: \"ENTRY\" label: \"ENTRY\" }\n");
  fprintf (file, "node: { title: \"EXIT\" label: \"EXIT\" }\n");

  /* Write blocks and edges.  */
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    {
      fprintf (file, "edge: { sourcename: \"ENTRY\" targetname: \"%d\"",
	       e->dest->index);

      if (e->flags & EDGE_FAKE)
	fprintf (file, " linestyle: dotted priority: 10");
      else
	fprintf (file, " linestyle: solid priority: 100");

      fprintf (file, " }\n");
    }
  fputc ('\n', file);

  FOR_EACH_BB (bb)
    {
      enum tree_code head_code, end_code;
      const char *head_name, *end_name;
      int head_line = 0;
      int end_line = 0;
      tree first = first_stmt (bb);
      tree last = last_stmt (bb);

      if (first)
	{
	  head_code = TREE_CODE (first);
	  head_name = tree_code_name[head_code];
	  head_line = get_lineno (first);
	}
      else
	head_name = "no-statement";

      if (last)
	{
	  end_code = TREE_CODE (last);
	  end_name = tree_code_name[end_code];
	  end_line = get_lineno (last);
	}
      else
	end_name = "no-statement";

      fprintf (file, "node: { title: \"%d\" label: \"#%d\\n%s (%d)\\n%s (%d)\"}\n",
	       bb->index, bb->index, head_name, head_line, end_name,
	       end_line);

      for (e = bb->succ; e; e = e->succ_next)
	{
	  if (e->dest == EXIT_BLOCK_PTR)
	    fprintf (file, "edge: { sourcename: \"%d\" targetname: \"EXIT\"", bb->index);
	  else
	    fprintf (file, "edge: { sourcename: \"%d\" targetname: \"%d\"", bb->index, e->dest->index);

	  if (e->flags & EDGE_FAKE)
	    fprintf (file, " priority: 10 linestyle: dotted");
	  else
	    fprintf (file, " priority: 100 linestyle: solid");

	  fprintf (file, " }\n");
	}

      if (bb->next_bb != EXIT_BLOCK_PTR)
	fputc ('\n', file);
    }

  fputs ("}\n\n", file);
}



/*---------------------------------------------------------------------------
			     Miscellaneous helpers
---------------------------------------------------------------------------*/

/* Return true if T represents a stmt that always transfers control.  */

bool
is_ctrl_stmt (tree t)
{
  return (TREE_CODE (t) == COND_EXPR
	  || TREE_CODE (t) == SWITCH_EXPR
	  || TREE_CODE (t) == GOTO_EXPR
	  || TREE_CODE (t) == RETURN_EXPR
	  || TREE_CODE (t) == RESX_EXPR);
}


/* Return true if T is a statement that may alter the flow of control
   (e.g., a call to a non-returning function).  */

bool
is_ctrl_altering_stmt (tree t)
{
  tree call;

#if defined ENABLE_CHECKING
  if (t == NULL)
    abort ();
#endif

  call = get_call_expr_in (t);
  if (call)
    {
      /* A non-pure/const CALL_EXPR alters flow control if the current
	 function has nonlocal labels.  */
      if (TREE_SIDE_EFFECTS (call) && current_function_has_nonlocal_label)
	return true;

      /* A CALL_EXPR also alters control flow if it does not return.  */
      if (call_expr_flags (call) & (ECF_NORETURN | ECF_LONGJMP))
	return true;
    }

  /* If a statement can throw, it alters control flow.  */
  return tree_can_throw_internal (t);
}


/* Return true if T is a computed goto.  */

bool
computed_goto_p (tree t)
{
  return (TREE_CODE (t) == GOTO_EXPR
	  && TREE_CODE (GOTO_DESTINATION (t)) != LABEL_DECL);
}


/* Checks whether EXPR is a simple local goto.  */

bool
simple_goto_p (tree expr)
{
  return  (TREE_CODE (expr) == GOTO_EXPR
	   && TREE_CODE (GOTO_DESTINATION (expr)) == LABEL_DECL
	   && (decl_function_context (GOTO_DESTINATION (expr))
	       == current_function_decl));
}


/* Return true if T should start a new basic block.  PREV_T is the
   statement preceding T.  It is used when T is a label or a case label.
   Labels should only start a new basic block if their previous statement
   wasn't a label.  Otherwise, sequence of labels would generate
   unnecessary basic blocks that only contain a single label.  */

static inline bool
stmt_starts_bb_p (tree t, tree prev_t)
{
  enum tree_code code;

  if (t == NULL_TREE)
    return false;

  /* LABEL_EXPRs start a new basic block only if the preceding
     statement wasn't a label of the same type.  This prevents the
     creation of consecutive blocks that have nothing but a single
     label.  */
  code = TREE_CODE (t);
  if (code == LABEL_EXPR)
    {
      /* Nonlocal and computed GOTO targets always start a new block.  */
      if (code == LABEL_EXPR
	  && (DECL_NONLOCAL (LABEL_EXPR_LABEL (t))
	      || FORCED_LABEL (LABEL_EXPR_LABEL (t))))
	return true;

      if (prev_t && TREE_CODE (prev_t) == code)
	{
	  if (DECL_NONLOCAL (LABEL_EXPR_LABEL (prev_t)))
	    return true;

	  cfg_stats.num_merged_labels++;
	  return false;
	}
      else
	return true;
    }

  return false;
}


/* Return true if T should end a basic block.  */

bool
stmt_ends_bb_p (tree t)
{
  return is_ctrl_stmt (t) || is_ctrl_altering_stmt (t);
}


/* Add gotos that used to be represented implicitly in the CFG.  */

void
disband_implicit_edges (void)
{
  basic_block bb;
  block_stmt_iterator last;
  edge e;
  tree stmt, label;

  FOR_EACH_BB (bb)
    {
      last = bsi_last (bb);
      stmt = last_stmt (bb);

      if (stmt && TREE_CODE (stmt) == COND_EXPR)
	{
	  /* Remove superfluous gotos from COND_EXPR branches.  Moved
	     from cfg_remove_useless_stmts here since it violates the
	     invariants for tree--cfg correspondence and thus fits better
	     here where we do it anyway.  */
	  for (e = bb->succ; e; e = e->succ_next)
	    {
	      if (e->dest != bb->next_bb)
		continue;

	      if (e->flags & EDGE_TRUE_VALUE)
		COND_EXPR_THEN (stmt) = build_empty_stmt ();
	      else if (e->flags & EDGE_FALSE_VALUE)
		COND_EXPR_ELSE (stmt) = build_empty_stmt ();
	      else
		abort ();
	      e->flags |= EDGE_FALLTHRU;
	    }

	  continue;
	}

      if (stmt && TREE_CODE (stmt) == RETURN_EXPR)
	{
	  /* Remove the RETURN_EXPR if we may fall though to the exit
	     instead.  */
	  if (!bb->succ
	      || bb->succ->succ_next
	      || bb->succ->dest != EXIT_BLOCK_PTR)
	    abort ();

	  if (bb->next_bb == EXIT_BLOCK_PTR
	      && !TREE_OPERAND (stmt, 0))
	    {
	      bsi_remove (&last);
	      bb->succ->flags |= EDGE_FALLTHRU;
	    }
	  continue;
	}

      /* There can be no fallthru edge if the last statement is a control
	 one.  */
      if (stmt && is_ctrl_stmt (stmt))
	continue;

      /* Find a fallthru edge and emit the goto if necessary.  */
      for (e = bb->succ; e; e = e->succ_next)
	if (e->flags & EDGE_FALLTHRU)
	  break;

      if (!e || e->dest == bb->next_bb)
	continue;

      if (e->dest == EXIT_BLOCK_PTR)
	abort ();

      label = tree_block_label (e->dest);

      stmt = build1 (GOTO_EXPR, void_type_node, label);
#ifdef USE_MAPPED_LOCATION
      SET_EXPR_LOCATION (stmt, e->goto_locus);
#else
      SET_EXPR_LOCUS (stmt, e->goto_locus);
#endif
      bsi_insert_after (&last, stmt, BSI_NEW_STMT);
      e->flags &= ~EDGE_FALLTHRU;
    }
}

/* Remove block annotations and other datastructures.  */

void
delete_tree_cfg_annotations (void)
{
  basic_block bb;
  if (n_basic_blocks > 0)
    free_blocks_annotations ();

  label_to_block_map = NULL;
  free_rbi_pool ();
  FOR_EACH_BB (bb)
    bb->rbi = NULL;
}


/* Return the first statement in basic block BB.  */

tree
first_stmt (basic_block bb)
{
  block_stmt_iterator i = bsi_start (bb);
  return !bsi_end_p (i) ? bsi_stmt (i) : NULL_TREE;
}


/* Return the last statement in basic block BB.  */

tree
last_stmt (basic_block bb)
{
  block_stmt_iterator b = bsi_last (bb);
  return !bsi_end_p (b) ? bsi_stmt (b) : NULL_TREE;
}


/* Return a pointer to the last statement in block BB.  */

tree *
last_stmt_ptr (basic_block bb)
{
  block_stmt_iterator last = bsi_last (bb);
  return !bsi_end_p (last) ? bsi_stmt_ptr (last) : NULL;
}


/* Return the last statement of an otherwise empty block.  Return NULL
   if the block is totally empty, or if it contains more than one
   statement.  */

tree
last_and_only_stmt (basic_block bb)
{
  block_stmt_iterator i = bsi_last (bb);
  tree last, prev;

  if (bsi_end_p (i))
    return NULL_TREE;

  last = bsi_stmt (i);
  bsi_prev (&i);
  if (bsi_end_p (i))
    return last;

  /* Empty statements should no longer appear in the instruction stream.
     Everything that might have appeared before should be deleted by
     remove_useless_stmts, and the optimizers should just bsi_remove
     instead of smashing with build_empty_stmt.

     Thus the only thing that should appear here in a block containing
     one executable statement is a label.  */
  prev = bsi_stmt (i);
  if (TREE_CODE (prev) == LABEL_EXPR)
    return last;
  else
    return NULL_TREE;
}


/* Mark BB as the basic block holding statement T.  */

void
set_bb_for_stmt (tree t, basic_block bb)
{
  if (TREE_CODE (t) == STATEMENT_LIST)
    {
      tree_stmt_iterator i;
      for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	set_bb_for_stmt (tsi_stmt (i), bb);
    }
  else
    {
      stmt_ann_t ann = get_stmt_ann (t);
      ann->bb = bb;

      /* If the statement is a label, add the label to block-to-labels map
	 so that we can speed up edge creation for GOTO_EXPRs.  */
      if (TREE_CODE (t) == LABEL_EXPR)
	{
	  int uid;

	  t = LABEL_EXPR_LABEL (t);
	  uid = LABEL_DECL_UID (t);
	  if (uid == -1)
	    {
	      LABEL_DECL_UID (t) = uid = cfun->last_label_uid++;
	      if (VARRAY_SIZE (label_to_block_map) <= (unsigned) uid)
		VARRAY_GROW (label_to_block_map, 3 * uid / 2);
	    }
	  else
	    {
#ifdef ENABLE_CHECKING
	      /* We're moving an existing label.  Make sure that we've
		 removed it from the old block.  */
	      if (bb && VARRAY_BB (label_to_block_map, uid))
		abort ();
#endif
	    }
	  VARRAY_BB (label_to_block_map, uid) = bb;
	}
    }
}


/* Insert statement (or statement list) T before the statement
   pointed-to by iterator I.  M specifies how to update iterator I
   after insertion (see enum bsi_iterator_update).  */

void
bsi_insert_before (block_stmt_iterator *i, tree t, enum bsi_iterator_update m)
{
  set_bb_for_stmt (t, i->bb);
  modify_stmt (t);
  tsi_link_before (&i->tsi, t, m);
}


/* Insert statement (or statement list) T after the statement
   pointed-to by iterator I.  M specifies how to update iterator I
   after insertion (see enum bsi_iterator_update).  */

void
bsi_insert_after (block_stmt_iterator *i, tree t, enum bsi_iterator_update m)
{
  set_bb_for_stmt (t, i->bb);
  modify_stmt (t);
  tsi_link_after (&i->tsi, t, m);
}


/* Remove the statement pointed to by iterator I.  The iterator is updated
   to the next statement.  */

void
bsi_remove (block_stmt_iterator *i)
{
  tree t = bsi_stmt (*i);
  set_bb_for_stmt (t, NULL);
  modify_stmt (t);
  tsi_delink (&i->tsi);
}


/* Move the statement at FROM so it comes right after the statement at TO.  */

void 
bsi_move_after (block_stmt_iterator *from, block_stmt_iterator *to)
{
  tree stmt = bsi_stmt (*from);
  bsi_remove (from);
  bsi_insert_after (to, stmt, BSI_SAME_STMT);
} 


/* Move the statement at FROM so it comes right before the statement at TO.  */

void 
bsi_move_before (block_stmt_iterator *from, block_stmt_iterator *to)
{
  tree stmt = bsi_stmt (*from);
  bsi_remove (from);
  bsi_insert_before (to, stmt, BSI_SAME_STMT);
}


/* Move the statement at FROM to the end of basic block BB.  */

void
bsi_move_to_bb_end (block_stmt_iterator *from, basic_block bb)
{
  block_stmt_iterator last = bsi_last (bb);
  
  /* Have to check bsi_end_p because it could be an empty block.  */
  if (!bsi_end_p (last) && is_ctrl_stmt (bsi_stmt (last)))
    bsi_move_before (from, &last);
  else
    bsi_move_after (from, &last);
}


/* Replace the contents of the statement pointed to by iterator BSI
   with STMT.  If PRESERVE_EH_INFO is true, the exception handling
   information of the original statement is preserved.  */

void
bsi_replace (const block_stmt_iterator *bsi, tree stmt, bool preserve_eh_info)
{
  int eh_region;
  tree orig_stmt = bsi_stmt (*bsi);

  SET_EXPR_LOCUS (stmt, EXPR_LOCUS (orig_stmt));
  set_bb_for_stmt (stmt, bsi->bb);

  /* Preserve EH region information from the original statement, if
     requested by the caller.  */
  if (preserve_eh_info)
    {
      eh_region = lookup_stmt_eh_region (orig_stmt);
      if (eh_region >= 0)
	add_stmt_to_eh_region (stmt, eh_region);
    }

  *bsi_stmt_ptr (*bsi) = stmt;
  modify_stmt (stmt);
}


/* Insert the statement pointed-to by BSI into edge E.  Every attempt
   is made to place the statement in an existing basic block, but
   sometimes that isn't possible.  When it isn't possible, the edge is
   split and the statement is added to the new block.

   In all cases, the returned *BSI points to the correct location.  The
   return value is true if insertion should be done after the location,
   or false if it should be done before the location.  */

static bool
tree_find_edge_insert_loc (edge e, block_stmt_iterator *bsi)
{
  basic_block dest, src;
  tree tmp;

  dest = e->dest;
 restart:

  /* If the destination has one predecessor which has no PHI nodes,
     insert there.  Except for the exit block. 

     The requirement for no PHI nodes could be relaxed.  Basically we
     would have to examine the PHIs to prove that none of them used
     the value set by the statement we want to insert on E.   That
     hardly seems worth the effort.  */
  if (dest->pred->pred_next == NULL
      && ! phi_nodes (dest)
      && dest != EXIT_BLOCK_PTR)
    {
      *bsi = bsi_start (dest);
      if (bsi_end_p (*bsi))
	return true;

      /* Make sure we insert after any leading labels.  */
      tmp = bsi_stmt (*bsi);
      while (TREE_CODE (tmp) == LABEL_EXPR)
	{
	  bsi_next (bsi);
	  if (bsi_end_p (*bsi))
	    break;
	  tmp = bsi_stmt (*bsi);
	}

      if (bsi_end_p (*bsi))
	{
	  *bsi = bsi_last (dest);
	  return true;
	}
      else
	return false;
    }

  /* If the source has one successor, the edge is not abnormal and
     the last statement does not end a basic block, insert there.
     Except for the entry block.  */
  src = e->src;
  if ((e->flags & EDGE_ABNORMAL) == 0
      && src->succ->succ_next == NULL
      && src != ENTRY_BLOCK_PTR)
    {
      *bsi = bsi_last (src);
      if (bsi_end_p (*bsi))
	return true;

      tmp = bsi_stmt (*bsi);
      if (!stmt_ends_bb_p (tmp))
	return true;

      /* Insert code just before returning the value.  We may need to decompose
         the return in the case it contains non-trivial operand.  */
      if (TREE_CODE (tmp) == RETURN_EXPR)
        {
	  tree op = TREE_OPERAND (tmp, 0);
	  if (!is_gimple_val (op))
	    {
	      if (TREE_CODE (op) != MODIFY_EXPR)
		abort ();
	      bsi_insert_before (bsi, op, BSI_NEW_STMT);
	      TREE_OPERAND (tmp, 0) = TREE_OPERAND (op, 0);
	    }
	  bsi_prev (bsi);
	  return true;
        }
    }

  /* Otherwise, create a new basic block, and split this edge.  */
  dest = split_edge (e);
  e = dest->pred;
  goto restart;
}


/* This routine will commit all pending edge insertions, creating any new
   basic blocks which are necessary.

   If specified, NEW_BLOCKS returns a count of the number of new basic
   blocks which were created.  */

void
bsi_commit_edge_inserts (int *new_blocks)
{
  basic_block bb;
  edge e;
  int blocks;

  blocks = n_basic_blocks;

  bsi_commit_edge_inserts_1 (ENTRY_BLOCK_PTR->succ);

  FOR_EACH_BB (bb)
    for (e = bb->succ; e; e = e->succ_next)
      bsi_commit_edge_inserts_1 (e);

  if (new_blocks)
    *new_blocks = n_basic_blocks - blocks;
}


/* Commit insertions pending at edge E.  */

static void
bsi_commit_edge_inserts_1 (edge e)
{
  if (PENDING_STMT (e))
    {
      block_stmt_iterator bsi;
      tree stmt = PENDING_STMT (e);

      PENDING_STMT (e) = NULL_TREE;

      if (tree_find_edge_insert_loc (e, &bsi))
	bsi_insert_after (&bsi, stmt, BSI_NEW_STMT);
      else
	bsi_insert_before (&bsi, stmt, BSI_NEW_STMT);
    }
}


/* Add STMT to the pending list of edge E.  No actual insertion is
   made until a call to bsi_commit_edge_inserts () is made.  */

void
bsi_insert_on_edge (edge e, tree stmt)
{
  append_to_statement_list (stmt, &PENDING_STMT (e));
}


/*---------------------------------------------------------------------------
	     Tree specific functions for CFG manipulation
---------------------------------------------------------------------------*/

/* Split a (typically critical) edge EDGE_IN.  Return the new block.
   Abort on abnormal edges.  */

static basic_block
tree_split_edge (edge edge_in)
{
  basic_block new_bb, after_bb, dest, src;
  edge new_edge, e;
  tree phi;
  int i, num_elem;

  /* Abnormal edges cannot be split.  */
  if (edge_in->flags & EDGE_ABNORMAL)
    abort ();

  src = edge_in->src;
  dest = edge_in->dest;

  /* Place the new block in the block list.  Try to keep the new block
     near its "logical" location.  This is of most help to humans looking
     at debugging dumps.  */
  for (e = dest->pred; e; e = e->pred_next)
    if (e->src->next_bb == dest)
      break;
  if (!e)
    after_bb = dest->prev_bb;
  else
    after_bb = edge_in->src;

  new_bb = create_empty_bb (after_bb);
  new_edge = make_edge (new_bb, dest, EDGE_FALLTHRU);

  /* Find all the PHI arguments on the original edge, and change them to
     the new edge.  Do it before redirection, so that the argument does not
     get removed.  */
  for (phi = phi_nodes (dest); phi; phi = PHI_CHAIN (phi))
    {
      num_elem = PHI_NUM_ARGS (phi);
      for (i = 0; i < num_elem; i++)
	if (PHI_ARG_EDGE (phi, i) == edge_in)
	  {
	    PHI_ARG_EDGE (phi, i) = new_edge;
	    break;
	  }
    }

  if (!redirect_edge_and_branch (edge_in, new_bb))
    abort ();

  if (PENDING_STMT (edge_in))
    abort ();

  return new_bb;
}


/* Return true when BB has label LABEL in it.  */

static bool
has_label_p (basic_block bb, tree label)
{
  block_stmt_iterator bsi;

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      tree stmt = bsi_stmt (bsi);

      if (TREE_CODE (stmt) != LABEL_EXPR)
	return false;
      if (LABEL_EXPR_LABEL (stmt) == label)
	return true;
    }
  return false;
}


/* Callback for walk_tree, check that all elements with address taken are
   properly noticed as such.  */

static tree
verify_expr (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp, x;

  if (TYPE_P (t))
    *walk_subtrees = 0;
  
  /* Check operand N for being valid GIMPLE and give error MSG if not. 
     We check for constants explicitly since they are not considered
     gimple invariants if they overflowed.  */
#define CHECK_OP(N, MSG) \
  do { if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (t, N))) != 'c'	\
         && !is_gimple_val (TREE_OPERAND (t, N)))			\
       { error (MSG); return TREE_OPERAND (t, N); }} while (0)

  switch (TREE_CODE (t))
    {
    case SSA_NAME:
      if (SSA_NAME_IN_FREE_LIST (t))
	{
	  error ("SSA name in freelist but still referenced");
	  return *tp;
	}
      break;

    case MODIFY_EXPR:
      x = TREE_OPERAND (t, 0);
      if (TREE_CODE (x) == BIT_FIELD_REF
	  && is_gimple_reg (TREE_OPERAND (x, 0)))
	{
	  error ("GIMPLE register modified with BIT_FIELD_REF");
	  return t;
	}
      break;

    case ADDR_EXPR:
      /* Skip any references (they will be checked when we recurse down the
	 tree) and ensure that any variable used as a prefix is marked
	 addressable.  */
      for (x = TREE_OPERAND (t, 0);
	   (handled_component_p (x)
	    || TREE_CODE (x) == REALPART_EXPR
	    || TREE_CODE (x) == IMAGPART_EXPR);
	   x = TREE_OPERAND (x, 0))
	;

      if (TREE_CODE (x) != VAR_DECL && TREE_CODE (x) != PARM_DECL)
	return NULL;
      if (!TREE_ADDRESSABLE (x))
	{
	  error ("address taken, but ADDRESSABLE bit not set");
	  return x;
	}
      break;

    case COND_EXPR:
      x = TREE_OPERAND (t, 0);
      if (TREE_CODE (TREE_TYPE (x)) != BOOLEAN_TYPE)
	{
	  error ("non-boolean used in condition");
	  return x;
	}
      break;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_CEIL_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case BIT_NOT_EXPR:
    case NON_LVALUE_EXPR:
    case TRUTH_NOT_EXPR:
      CHECK_OP (0, "Invalid operand to unary operator");
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case BIT_FIELD_REF:
    case VIEW_CONVERT_EXPR:
      /* We have a nest of references.  Verify that each of the operands
	 that determine where to reference is either a constant or a variable,
	 verify that the base is valid, and then show we've already checked
	 the subtrees.  */
      while (TREE_CODE (t) == REALPART_EXPR || TREE_CODE (t) == IMAGPART_EXPR
	     || handled_component_p (t))
	{
	  if (TREE_CODE (t) == COMPONENT_REF && TREE_OPERAND (t, 2))
	    CHECK_OP (2, "Invalid COMPONENT_REF offset operator");
	  else if (TREE_CODE (t) == ARRAY_REF
		   || TREE_CODE (t) == ARRAY_RANGE_REF)
	    {
	      CHECK_OP (1, "Invalid array index.");
	      if (TREE_OPERAND (t, 2))
		CHECK_OP (2, "Invalid array lower bound.");
	      if (TREE_OPERAND (t, 3))
		CHECK_OP (3, "Invalid array stride.");
	    }
	  else if (TREE_CODE (t) == BIT_FIELD_REF)
	    {
	      CHECK_OP (1, "Invalid operand to BIT_FIELD_REF");
	      CHECK_OP (2, "Invalid operand to BIT_FIELD_REF");
	    }

	  t = TREE_OPERAND (t, 0);
	}

      if (TREE_CODE_CLASS (TREE_CODE (t)) != 'c'
	  && !is_gimple_lvalue (t))
	{
	  error ("Invalid reference prefix.");
	  return t;
	}
      *walk_subtrees = 0;
      break;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
      CHECK_OP (0, "Invalid operand to binary operator");
      CHECK_OP (1, "Invalid operand to binary operator");
      break;

    default:
      break;
    }
  return NULL;

#undef CHECK_OP
}


/* Verify STMT, return true if STMT is not in GIMPLE form.
   TODO: Implement type checking.  */

static bool
verify_stmt (tree stmt, bool last_in_block)
{
  tree addr;

  if (!is_gimple_stmt (stmt))
    {
      error ("Is not a valid GIMPLE statement.");
      goto fail;
    }

  addr = walk_tree (&stmt, verify_expr, NULL, NULL);
  if (addr)
    {
      debug_generic_stmt (addr);
      return true;
    }

  /* If the statement is marked as part of an EH region, then it is
     expected that the statement could throw.  Verify that when we
     have optimizations that simplify statements such that we prove
     that they cannot throw, that we update other data structures
     to match.  */
  if (lookup_stmt_eh_region (stmt) >= 0)
    {
      if (!tree_could_throw_p (stmt))
	{
	  error ("Statement marked for throw, but doesn't.");
	  goto fail;
	}
      if (!last_in_block && tree_can_throw_internal (stmt))
	{
	  error ("Statement marked for throw in middle of block.");
	  goto fail;
	}
    }

  return false;

 fail:
  debug_generic_stmt (stmt);
  return true;
}


/* Return true when the T can be shared.  */

static bool
tree_node_can_be_shared (tree t)
{
  if (TYPE_P (t) || DECL_P (t)
      /* We check for constants explicitly since they are not considered
	 gimple invariants if they overflowed.  */
      || TREE_CODE_CLASS (TREE_CODE (t)) == 'c'
      || is_gimple_min_invariant (t)
      || TREE_CODE (t) == SSA_NAME)
    return true;

  while (((TREE_CODE (t) == ARRAY_REF || TREE_CODE (t) == ARRAY_RANGE_REF)
	  /* We check for constants explicitly since they are not considered
	     gimple invariants if they overflowed.  */
	  && (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (t, 1))) == 'c'
	      || is_gimple_min_invariant (TREE_OPERAND (t, 1))))
	 || (TREE_CODE (t) == COMPONENT_REF
	     || TREE_CODE (t) == REALPART_EXPR
	     || TREE_CODE (t) == IMAGPART_EXPR))
    t = TREE_OPERAND (t, 0);

  if (DECL_P (t))
    return true;

  return false;
}


/* Called via walk_trees.  Verify tree sharing.  */

static tree
verify_node_sharing (tree * tp, int *walk_subtrees, void *data)
{
  htab_t htab = (htab_t) data;
  void **slot;

  if (tree_node_can_be_shared (*tp))
    {
      *walk_subtrees = false;
      return NULL;
    }

  slot = htab_find_slot (htab, *tp, INSERT);
  if (*slot)
    return *slot;
  *slot = *tp;

  return NULL;
}


/* Verify the GIMPLE statement chain.  */

void
verify_stmts (void)
{
  basic_block bb;
  block_stmt_iterator bsi;
  bool err = false;
  htab_t htab;
  tree addr;

  timevar_push (TV_TREE_STMT_VERIFY);
  htab = htab_create (37, htab_hash_pointer, htab_eq_pointer, NULL);

  FOR_EACH_BB (bb)
    {
      tree phi;
      int i;

      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  int phi_num_args = PHI_NUM_ARGS (phi);

	  for (i = 0; i < phi_num_args; i++)
	    {
	      tree t = PHI_ARG_DEF (phi, i);
	      tree addr;

	      /* Addressable variables do have SSA_NAMEs but they
		 are not considered gimple values.  */
	      if (TREE_CODE (t) != SSA_NAME
		  && TREE_CODE (t) != FUNCTION_DECL
		  && !is_gimple_val (t))
		{
		  error ("PHI def is not a GIMPLE value");
		  debug_generic_stmt (phi);
		  debug_generic_stmt (t);
		  err |= true;
		}

	      addr = walk_tree (&t, verify_expr, NULL, NULL);
	      if (addr)
		{
		  debug_generic_stmt (addr);
		  err |= true;
		}

	      addr = walk_tree (&t, verify_node_sharing, htab, NULL);
	      if (addr)
		{
		  error ("Incorrect sharing of tree nodes");
		  debug_generic_stmt (phi);
		  debug_generic_stmt (addr);
		  err |= true;
		}
	    }
	}

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); )
	{
	  tree stmt = bsi_stmt (bsi);
	  bsi_next (&bsi);
	  err |= verify_stmt (stmt, bsi_end_p (bsi));
	  addr = walk_tree (&stmt, verify_node_sharing, htab, NULL);
	  if (addr)
	    {
	      error ("Incorrect sharing of tree nodes");
	      debug_generic_stmt (stmt);
	      debug_generic_stmt (addr);
	      err |= true;
	    }
	}
    }

  if (err)
    internal_error ("verify_stmts failed.");

  htab_delete (htab);
  timevar_pop (TV_TREE_STMT_VERIFY);
}


/* Verifies that the flow information is OK.  */

static int
tree_verify_flow_info (void)
{
  int err = 0;
  basic_block bb;
  block_stmt_iterator bsi;
  tree stmt;
  edge e;

  if (ENTRY_BLOCK_PTR->stmt_list)
    {
      error ("ENTRY_BLOCK has a statement list associated with it\n");
      err = 1;
    }

  if (EXIT_BLOCK_PTR->stmt_list)
    {
      error ("EXIT_BLOCK has a statement list associated with it\n");
      err = 1;
    }

  for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
    if (e->flags & EDGE_FALLTHRU)
      {
	error ("Fallthru to exit from bb %d\n", e->src->index);
	err = 1;
      }

  FOR_EACH_BB (bb)
    {
      bool found_ctrl_stmt = false;

      /* Skip labels on the start of basic block.  */
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  if (TREE_CODE (bsi_stmt (bsi)) != LABEL_EXPR)
	    break;

	  if (label_to_block (LABEL_EXPR_LABEL (bsi_stmt (bsi))) != bb)
	    {
	      error ("Label %s to block does not match in bb %d\n",
		     IDENTIFIER_POINTER (DECL_NAME (bsi_stmt (bsi))),
		     bb->index);
	      err = 1;
	    }

	  if (decl_function_context (LABEL_EXPR_LABEL (bsi_stmt (bsi)))
	      != current_function_decl)
	    {
	      error ("Label %s has incorrect context in bb %d\n",
		     IDENTIFIER_POINTER (DECL_NAME (bsi_stmt (bsi))),
		     bb->index);
	      err = 1;
	    }
	}

      /* Verify that body of basic block BB is free of control flow.  */
      for (; !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree stmt = bsi_stmt (bsi);

	  if (found_ctrl_stmt)
	    {
	      error ("Control flow in the middle of basic block %d\n",
		     bb->index);
	      err = 1;
	    }

	  if (stmt_ends_bb_p (stmt))
	    found_ctrl_stmt = true;

	  if (TREE_CODE (stmt) == LABEL_EXPR)
	    {
	      error ("Label %s in the middle of basic block %d\n",
		     IDENTIFIER_POINTER (DECL_NAME (stmt)),
		     bb->index);
	      err = 1;
	    }
	}
      bsi = bsi_last (bb);
      if (bsi_end_p (bsi))
	continue;

      stmt = bsi_stmt (bsi);

      if (is_ctrl_stmt (stmt))
	{
	  for (e = bb->succ; e; e = e->succ_next)
	    if (e->flags & EDGE_FALLTHRU)
	      {
		error ("Fallthru edge after a control statement in bb %d \n",
		       bb->index);
		err = 1;
	      }
	}

      switch (TREE_CODE (stmt))
	{
	case COND_EXPR:
	  {
	    edge true_edge;
	    edge false_edge;
	    if (TREE_CODE (COND_EXPR_THEN (stmt)) != GOTO_EXPR
		|| TREE_CODE (COND_EXPR_ELSE (stmt)) != GOTO_EXPR)
	      {
		error ("Structured COND_EXPR at the end of bb %d\n", bb->index);
		err = 1;
	      }

	    extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

	    if (!true_edge || !false_edge
		|| !(true_edge->flags & EDGE_TRUE_VALUE)
		|| !(false_edge->flags & EDGE_FALSE_VALUE)
		|| (true_edge->flags & (EDGE_FALLTHRU | EDGE_ABNORMAL))
		|| (false_edge->flags & (EDGE_FALLTHRU | EDGE_ABNORMAL))
		|| bb->succ->succ_next->succ_next)
	      {
		error ("Wrong outgoing edge flags at end of bb %d\n",
		       bb->index);
		err = 1;
	      }

	    if (!has_label_p (true_edge->dest,
			      GOTO_DESTINATION (COND_EXPR_THEN (stmt))))
	      {
		error ("`then' label does not match edge at end of bb %d\n",
		       bb->index);
		err = 1;
	      }

	    if (!has_label_p (false_edge->dest,
			      GOTO_DESTINATION (COND_EXPR_ELSE (stmt))))
	      {
		error ("`else' label does not match edge at end of bb %d\n",
		       bb->index);
		err = 1;
	      }
	  }
	  break;

	case GOTO_EXPR:
	  if (simple_goto_p (stmt))
	    {
	      error ("Explicit goto at end of bb %d\n", bb->index);
    	      err = 1;
	    }
	  else
	    {
	      /* FIXME.  We should double check that the labels in the 
		 destination blocks have their address taken.  */
	      for (e = bb->succ; e; e = e->succ_next)
		if ((e->flags & (EDGE_FALLTHRU | EDGE_TRUE_VALUE
				 | EDGE_FALSE_VALUE))
		    || !(e->flags & EDGE_ABNORMAL))
		  {
		    error ("Wrong outgoing edge flags at end of bb %d\n",
			   bb->index);
		    err = 1;
		  }
	    }
	  break;

	case RETURN_EXPR:
	  if (!bb->succ || bb->succ->succ_next
	      || (bb->succ->flags & (EDGE_FALLTHRU | EDGE_ABNORMAL
		  		     | EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	    {
	      error ("Wrong outgoing edge flags at end of bb %d\n", bb->index);
	      err = 1;
	    }
	  if (bb->succ->dest != EXIT_BLOCK_PTR)
	    {
	      error ("Return edge does not point to exit in bb %d\n",
		     bb->index);
	      err = 1;
	    }
	  break;

	case SWITCH_EXPR:
	  {
	    tree prev;
	    edge e;
	    size_t i, n;
	    tree vec;

	    vec = SWITCH_LABELS (stmt);
	    n = TREE_VEC_LENGTH (vec);

	    /* Mark all the destination basic blocks.  */
	    for (i = 0; i < n; ++i)
	      {
		tree lab = CASE_LABEL (TREE_VEC_ELT (vec, i));
		basic_block label_bb = label_to_block (lab);

		if (label_bb->aux && label_bb->aux != (void *)1)
		  abort ();
		label_bb->aux = (void *)1;
	      }

	    /* Verify that the case labels are sorted.  */
	    prev = TREE_VEC_ELT (vec, 0);
	    for (i = 1; i < n - 1; ++i)
	      {
		tree c = TREE_VEC_ELT (vec, i);
		if (! CASE_LOW (c))
		  {
		    error ("Found default case not at end of case vector");
		    err = 1;
		    continue;
		  }
		if (! tree_int_cst_lt (CASE_LOW (prev), CASE_LOW (c)))
		  {
		    error ("Case labels not sorted:\n ");
		    print_generic_expr (stderr, prev, 0);
		    fprintf (stderr," is greater than ");
		    print_generic_expr (stderr, c, 0);
		    fprintf (stderr," but comes before it.\n");
		    err = 1;
		  }
		prev = c;
	      }
	    if (CASE_LOW (TREE_VEC_ELT (vec, n - 1)))
	      {
		error ("No default case found at end of case vector");
		err = 1;
	      }

	    for (e = bb->succ; e; e = e->succ_next)
	      {
		if (!e->dest->aux)
		  {
		    error ("Extra outgoing edge %d->%d\n",
			   bb->index, e->dest->index);
		    err = 1;
		  }
		e->dest->aux = (void *)2;
		if ((e->flags & (EDGE_FALLTHRU | EDGE_ABNORMAL
				 | EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
		  {
		    error ("Wrong outgoing edge flags at end of bb %d\n",
			   bb->index);
		    err = 1;
		  }
	      }

	    /* Check that we have all of them.  */
	    for (i = 0; i < n; ++i)
	      {
		tree lab = CASE_LABEL (TREE_VEC_ELT (vec, i));
		basic_block label_bb = label_to_block (lab);

		if (label_bb->aux != (void *)2)
		  {
		    error ("Missing edge %i->%i\n",
			   bb->index, label_bb->index);
		    err = 1;
		  }
	      }

	    for (e = bb->succ; e; e = e->succ_next)
	      e->dest->aux = (void *)0;
	  }

	default: ;
	}
    }

  if (dom_computed[CDI_DOMINATORS] >= DOM_NO_FAST_QUERY)
    verify_dominators (CDI_DOMINATORS);

  return err;
}


/* Updates phi nodes after creating forwarder block joined
   by edge FALLTHRU.  */

static void
tree_make_forwarder_block (edge fallthru)
{
  edge e;
  basic_block dummy, bb;
  tree phi, new_phi, var, prev, next;

  dummy = fallthru->src;
  bb = fallthru->dest;

  if (!bb->pred->pred_next)
    return;

  /* If we redirected a branch we must create new phi nodes at the
     start of BB.  */
  for (phi = phi_nodes (dummy); phi; phi = PHI_CHAIN (phi))
    {
      var = PHI_RESULT (phi);
      new_phi = create_phi_node (var, bb);
      SSA_NAME_DEF_STMT (var) = new_phi;
      SET_PHI_RESULT (phi, make_ssa_name (SSA_NAME_VAR (var), phi));
      add_phi_arg (&new_phi, PHI_RESULT (phi), fallthru);
    }

  /* Ensure that the PHI node chain is in the same order.  */
  prev = NULL;
  for (phi = phi_nodes (bb); phi; phi = next)
    {
      next = PHI_CHAIN (phi);
      PHI_CHAIN (phi) = prev;
      prev = phi;
    }
  set_phi_nodes (bb, prev);

  /* Add the arguments we have stored on edges.  */
  for (e = bb->pred; e; e = e->pred_next)
    {
      if (e == fallthru)
	continue;

      for (phi = phi_nodes (bb), var = PENDING_STMT (e);
	   phi;
	   phi = PHI_CHAIN (phi), var = TREE_CHAIN (var))
	add_phi_arg (&phi, TREE_VALUE (var), e);

      PENDING_STMT (e) = NULL;
    }
}


/* Return true if basic block BB does nothing except pass control
   flow to another block and that we can safely insert a label at
   the start of the successor block.  */

static bool
tree_forwarder_block_p (basic_block bb)
{
  block_stmt_iterator bsi;
  edge e;

  /* If we have already determined that this block is not forwardable,
     then no further checks are necessary.  */
  if (! bb_ann (bb)->forwardable)
    return false;

  /* BB must have a single outgoing normal edge.  Otherwise it can not be
     a forwarder block.  */
  if (!bb->succ
      || bb->succ->succ_next
      || bb->succ->dest == EXIT_BLOCK_PTR
      || (bb->succ->flags & EDGE_ABNORMAL)
      || bb == ENTRY_BLOCK_PTR)
    {
      bb_ann (bb)->forwardable = 0;
      return false; 
    }

  /* Successors of the entry block are not forwarders.  */
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    if (e->dest == bb)
      {
	bb_ann (bb)->forwardable = 0;
	return false;
      }

  /* BB can not have any PHI nodes.  This could potentially be relaxed
     early in compilation if we re-rewrote the variables appearing in
     any PHI nodes in forwarder blocks.  */
  if (phi_nodes (bb))
    {
      bb_ann (bb)->forwardable = 0;
      return false; 
    }

  /* Now walk through the statements.  We can ignore labels, anything else
     means this is not a forwarder block.  */
  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      tree stmt = bsi_stmt (bsi);
 
      switch (TREE_CODE (stmt))
	{
	case LABEL_EXPR:
	  if (DECL_NONLOCAL (LABEL_EXPR_LABEL (stmt)))
	    return false;
	  break;

	default:
	  bb_ann (bb)->forwardable = 0;
	  return false;
	}
    }

  return true;
}


/* Thread jumps over empty statements.

   This code should _not_ thread over obviously equivalent conditions
   as that requires nontrivial updates to the SSA graph.  */
   
static bool
thread_jumps (void)
{
  edge e, next, last, old;
  basic_block bb, dest, tmp;
  tree phi;
  int arg;
  bool retval = false;

  FOR_EACH_BB (bb)
    bb_ann (bb)->forwardable = 1;

  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR, next_bb)
    {
      /* Don't waste time on unreachable blocks.  */
      if (!bb->pred)
	continue;

      /* Nor on forwarders.  */
      if (tree_forwarder_block_p (bb))
	continue;
      
      /* This block is now part of a forwarding path, mark it as not
	 forwardable so that we can detect loops.  This bit will be
	 reset below.  */
      bb_ann (bb)->forwardable = 0;

      /* Examine each of our block's successors to see if it is
	 forwardable.  */
      for (e = bb->succ; e; e = next)
	{
	  next = e->succ_next;

	  /* If the edge is abnormal or its destination is not
	     forwardable, then there's nothing to do.  */
	  if ((e->flags & EDGE_ABNORMAL)
	      || !tree_forwarder_block_p (e->dest))
	    continue;

	  /* Now walk through as many forwarder block as possible to
	     find the ultimate destination we want to thread our jump
	     to.  */
	  last = e->dest->succ;
	  bb_ann (e->dest)->forwardable = 0;
	  for (dest = e->dest->succ->dest;
	       tree_forwarder_block_p (dest);
	       last = dest->succ,
	       dest = dest->succ->dest)
	    {
	      /* An infinite loop detected.  We redirect the edge anyway, so
		 that the loop is shrunk into single basic block.  */
	      if (!bb_ann (dest)->forwardable)
		break;

	      if (dest->succ->dest == EXIT_BLOCK_PTR)
		break;

	      bb_ann (dest)->forwardable = 0;
	    }

	  /* Reset the forwardable marks to 1.  */
	  for (tmp = e->dest;
	       tmp != dest;
	       tmp = tmp->succ->dest)
	    bb_ann (tmp)->forwardable = 1;

	  if (dest == e->dest)
	    continue;
	      
	  old = find_edge (bb, dest);
	  if (old)
	    {
	      /* If there already is an edge, check whether the values
		 in phi nodes differ.  */
	      if (!phi_alternatives_equal (dest, last, old))
		{
		  /* The previous block is forwarder.  Redirect our jump
		     to that target instead since we know it has no PHI
		     nodes that will need updating.  */
		  dest = last->src;
	  
		  /* That might mean that no forwarding at all is possible.  */
		  if (dest == e->dest)
		    continue;

		  old = find_edge (bb, dest);
		}
	    }

	  /* Perform the redirection.  */
	  retval = true;
	  e = redirect_edge_and_branch (e, dest);

	  /* TODO -- updating dominators in this case is simple.  */
	  free_dominance_info (CDI_DOMINATORS);

	  if (!old)
	    {
	      /* Update PHI nodes.   We know that the new argument should
		 have the same value as the argument associated with LAST.
		 Otherwise we would have changed our target block above.  */
	      for (phi = phi_nodes (dest); phi; phi = PHI_CHAIN (phi))
		{
		  arg = phi_arg_from_edge (phi, last);
		  if (arg < 0)
		    abort ();
		  add_phi_arg (&phi, PHI_ARG_DEF (phi, arg), e);
		}
	    }
	}

      /* Reset the forwardable bit on our block since it's no longer in
	 a forwarding chain path.  */
      bb_ann (bb)->forwardable = 1;
    }

  return retval;
}


/* Return a non-special label in the head of basic block BLOCK.
   Create one if it doesn't exist.  */

tree
tree_block_label (basic_block bb)
{
  block_stmt_iterator i, s = bsi_start (bb);
  bool first = true;
  tree label, stmt;

  for (i = s; !bsi_end_p (i); first = false, bsi_next (&i))
    {
      stmt = bsi_stmt (i);
      if (TREE_CODE (stmt) != LABEL_EXPR)
	break;
      label = LABEL_EXPR_LABEL (stmt);
      if (!DECL_NONLOCAL (label))
	{
	  if (!first)
	    bsi_move_before (&i, &s);
	  return label;
	}
    }

  label = create_artificial_label ();
  stmt = build1 (LABEL_EXPR, void_type_node, label);
  bsi_insert_before (&s, stmt, BSI_NEW_STMT);
  return label;
}


/* Attempt to perform edge redirection by replacing a possibly complex
   jump instruction by a goto or by removing the jump completely.
   This can apply only if all edges now point to the same block.  The
   parameters and return values are equivalent to
   redirect_edge_and_branch.  */

static edge
tree_try_redirect_by_replacing_jump (edge e, basic_block target)
{
  basic_block src = e->src;
  edge tmp;
  block_stmt_iterator b;
  tree stmt;

  /* Verify that all targets will be TARGET.  */
  for (tmp = src->succ; tmp; tmp = tmp->succ_next)
    if (tmp->dest != target && tmp != e)
      break;

  if (tmp)
    return NULL;

  b = bsi_last (src);
  if (bsi_end_p (b))
    return NULL;
  stmt = bsi_stmt (b);

  if (TREE_CODE (stmt) == COND_EXPR
      || TREE_CODE (stmt) == SWITCH_EXPR)
    {
      bsi_remove (&b);
      e = ssa_redirect_edge (e, target);
      e->flags = EDGE_FALLTHRU;
      return e;
    }

  return NULL;
}


/* Redirect E to DEST.  Return NULL on failure.  Otherwise, return the
   edge representing the redirected branch.  */

static edge
tree_redirect_edge_and_branch (edge e, basic_block dest)
{
  basic_block bb = e->src;
  block_stmt_iterator bsi;
  edge ret;
  tree label, stmt;

  if (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH))
    return NULL;

  if (e->src != ENTRY_BLOCK_PTR 
      && (ret = tree_try_redirect_by_replacing_jump (e, dest)))
    return ret;

  if (e->dest == dest)
    return NULL;

  label = tree_block_label (dest);

  bsi = bsi_last (bb);
  stmt = bsi_end_p (bsi) ? NULL : bsi_stmt (bsi);

  switch (stmt ? TREE_CODE (stmt) : ERROR_MARK)
    {
    case COND_EXPR:
      stmt = (e->flags & EDGE_TRUE_VALUE
	      ? COND_EXPR_THEN (stmt)
	      : COND_EXPR_ELSE (stmt));
      GOTO_DESTINATION (stmt) = label;
      break;

    case GOTO_EXPR:
      /* No non-abnormal edges should lead from a non-simple goto, and
	 simple ones should be represented implicitly.  */
      abort ();

    case SWITCH_EXPR:
      {
	tree vec = SWITCH_LABELS (stmt);
	size_t i, n = TREE_VEC_LENGTH (vec);

	for (i = 0; i < n; ++i)
	  {
	    tree elt = TREE_VEC_ELT (vec, i);
	    if (label_to_block (CASE_LABEL (elt)) == e->dest)
	      CASE_LABEL (elt) = label;
	  }
      }
      break;

    case RETURN_EXPR:
      bsi_remove (&bsi);
      e->flags |= EDGE_FALLTHRU;
      break;

    default:
      /* Otherwise it must be a fallthru edge, and we don't need to
	 do anything besides redirecting it.  */
      if (!(e->flags & EDGE_FALLTHRU))
	abort ();
      break;
    }

  /* Update/insert PHI nodes as necessary.  */

  /* Now update the edges in the CFG.  */
  e = ssa_redirect_edge (e, dest);

  return e;
}


/* Simple wrapper, as we can always redirect fallthru edges.  */

static basic_block
tree_redirect_edge_and_branch_force (edge e, basic_block dest)
{
  e = tree_redirect_edge_and_branch (e, dest);
  if (!e)
    abort ();

  return NULL;
}


/* Splits basic block BB after statement STMT (but at least after the
   labels).  If STMT is NULL, BB is split just after the labels.  */

static basic_block
tree_split_block (basic_block bb, void *stmt)
{
  block_stmt_iterator bsi, bsi_tgt;
  tree act;
  basic_block new_bb;
  edge e;

  new_bb = create_empty_bb (bb);

  /* Redirect the outgoing edges.  */
  new_bb->succ = bb->succ;
  bb->succ = NULL;
  for (e = new_bb->succ; e; e = e->succ_next)
    e->src = new_bb;

  if (stmt && TREE_CODE ((tree) stmt) == LABEL_EXPR)
    stmt = NULL;

  /* Move everything from BSI to the new basic block.  */
  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      act = bsi_stmt (bsi);
      if (TREE_CODE (act) == LABEL_EXPR)
	continue;

      if (!stmt)
	break;

      if (stmt == act)
	{
	  bsi_next (&bsi);
	  break;
	}
    }

  bsi_tgt = bsi_start (new_bb);
  while (!bsi_end_p (bsi))
    {
      act = bsi_stmt (bsi);
      bsi_remove (&bsi);
      bsi_insert_after (&bsi_tgt, act, BSI_NEW_STMT);
    }

  return new_bb;
}


/* Moves basic block BB after block AFTER.  */

static bool
tree_move_block_after (basic_block bb, basic_block after)
{
  if (bb->prev_bb == after)
    return true;

  unlink_block (bb);
  link_block (bb, after);

  return true;
}


/* Return true if basic_block can be duplicated.  */

static bool
tree_can_duplicate_bb_p (basic_block bb ATTRIBUTE_UNUSED)
{
  return true;
}


/* Create a duplicate of the basic block BB.  NOTE: This does not
   preserve SSA form.  */

static basic_block
tree_duplicate_bb (basic_block bb)
{
  basic_block new_bb;
  block_stmt_iterator bsi, bsi_tgt;

  new_bb = create_empty_bb (EXIT_BLOCK_PTR->prev_bb);
  bsi_tgt = bsi_start (new_bb);
  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      tree stmt = bsi_stmt (bsi);
      tree copy;

      if (TREE_CODE (stmt) == LABEL_EXPR)
	continue;

      copy = unshare_expr (stmt);

      /* Copy also the virtual operands.  */
      get_stmt_ann (copy);
      copy_virtual_operands (copy, stmt);
      
      bsi_insert_after (&bsi_tgt, copy, BSI_NEW_STMT);
    }

  return new_bb;
}


/* Dump FUNCTION_DECL FN to file FILE using FLAGS (see TDF_* in tree.h)  */

void
dump_function_to_file (tree fn, FILE *file, int flags)
{
  tree arg, vars, var;
  bool ignore_topmost_bind = false, any_var = false;
  basic_block bb;
  tree chain;

  fprintf (file, "%s (", lang_hooks.decl_printable_name (fn, 2));

  arg = DECL_ARGUMENTS (fn);
  while (arg)
    {
      print_generic_expr (file, arg, dump_flags);
      if (TREE_CHAIN (arg))
	fprintf (file, ", ");
      arg = TREE_CHAIN (arg);
    }
  fprintf (file, ")\n");

  if (flags & TDF_RAW)
    {
      dump_node (fn, TDF_SLIM | flags, file);
      return;
    }

  /* When GIMPLE is lowered, the variables are no longer available in
     BIND_EXPRs, so display them separately.  */
  if (cfun && cfun->unexpanded_var_list)
    {
      ignore_topmost_bind = true;

      fprintf (file, "{\n");
      for (vars = cfun->unexpanded_var_list; vars; vars = TREE_CHAIN (vars))
	{
	  var = TREE_VALUE (vars);

	  print_generic_decl (file, var, flags);
	  fprintf (file, "\n");

	  any_var = true;
	}
    }

  if (basic_block_info)
    {
      /* Make a CFG based dump.  */
      if (!ignore_topmost_bind)
	fprintf (file, "{\n");

      if (any_var && n_basic_blocks)
	fprintf (file, "\n");

      FOR_EACH_BB (bb)
	dump_generic_bb (file, bb, 2, flags);
	
      fprintf (file, "}\n");
    }
  else
    {
      int indent;

      /* Make a tree based dump.  */
      chain = DECL_SAVED_TREE (fn);

      if (TREE_CODE (chain) == BIND_EXPR)
	{
	  if (ignore_topmost_bind)
	    {
	      chain = BIND_EXPR_BODY (chain);
	      indent = 2;
	    }
	  else
	    indent = 0;
	}
      else
	{
	  if (!ignore_topmost_bind)
	    fprintf (file, "{\n");
	  indent = 2;
	}

      if (any_var)
	fprintf (file, "\n");

      print_generic_stmt_indented (file, chain, flags, indent);
      if (ignore_topmost_bind)
	fprintf (file, "}\n");
    }

  fprintf (file, "\n\n");
}


/* Pretty print of the loops intermediate representation.  */
static void print_loop (FILE *, struct loop *, int);
static void print_pred_bbs (FILE *, edge);
static void print_succ_bbs (FILE *, edge);


/* Print the predecessors indexes of edge E on FILE.  */

static void
print_pred_bbs (FILE *file, edge e)
{
  if (e == NULL)
    return;
  
  else if (e->pred_next == NULL)
    fprintf (file, "bb_%d", e->src->index);
  
  else
    {
      fprintf (file, "bb_%d, ", e->src->index);
      print_pred_bbs (file, e->pred_next);
    }
}


/* Print the successors indexes of edge E on FILE.  */

static void
print_succ_bbs (FILE *file, edge e)
{
  if (e == NULL)
    return;
  else if (e->succ_next == NULL)
    fprintf (file, "bb_%d", e->dest->index);
  else
    {
      fprintf (file, "bb_%d, ", e->dest->index);
      print_succ_bbs (file, e->succ_next);
    }
}


/* Pretty print LOOP on FILE, indented INDENT spaces.  */

static void
print_loop (FILE *file, struct loop *loop, int indent)
{
  char *s_indent;
  basic_block bb;
  
  if (loop == NULL)
    return;

  s_indent = (char *) alloca ((size_t) indent + 1);
  memset ((void *) s_indent, ' ', (size_t) indent);
  s_indent[indent] = '\0';

  /* Print the loop's header.  */
  fprintf (file, "%sloop_%d\n", s_indent, loop->num);
  
  /* Print the loop's body.  */
  fprintf (file, "%s{\n", s_indent);
  FOR_EACH_BB (bb)
    if (bb->loop_father == loop)
      {
	/* Print the basic_block's header.  */
	fprintf (file, "%s  bb_%d (preds = {", s_indent, bb->index);
	print_pred_bbs (file, bb->pred);
	fprintf (file, "}, succs = {");
	print_succ_bbs (file, bb->succ);
	fprintf (file, "})\n");
	
	/* Print the basic_block's body.  */
	fprintf (file, "%s  {\n", s_indent);
	tree_dump_bb (bb, file, indent + 4);
	fprintf (file, "%s  }\n", s_indent);
      }
  
  print_loop (file, loop->inner, indent + 2);
  fprintf (file, "%s}\n", s_indent);
  print_loop (file, loop->next, indent);
}


/* Follow a CFG edge from the entry point of the program, and on entry
   of a loop, pretty print the loop structure on FILE.  */

void 
print_loop_ir (FILE *file)
{
  basic_block bb;
  
  bb = BASIC_BLOCK (0);
  if (bb && bb->loop_father)
    print_loop (file, bb->loop_father, 0);
}


/* Debugging loops structure at tree level.  */

void 
debug_loop_ir (void)
{
  print_loop_ir (stderr);
}


/* Return true if BB ends with a call, possibly followed by some
   instructions that must stay with the call.  Return false,
   otherwise.  */

static bool
tree_block_ends_with_call_p (basic_block bb)
{
  block_stmt_iterator bsi = bsi_last (bb);
  return get_call_expr_in (bsi_stmt (bsi)) != NULL;
}


/* Return true if BB ends with a conditional branch.  Return false,
   otherwise.  */

static bool
tree_block_ends_with_condjump_p (basic_block bb)
{
  tree stmt = tsi_stmt (bsi_last (bb).tsi);
  return (TREE_CODE (stmt) == COND_EXPR);
}


/* Return true if we need to add fake edge to exit at statement T.
   Helper function for tree_flow_call_edges_add.  */

static bool
need_fake_edge_p (tree t)
{
  tree call;

  /* NORETURN and LONGJMP calls already have an edge to exit.
     CONST, PURE and ALWAYS_RETURN calls do not need one.
     We don't currently check for CONST and PURE here, although
     it would be a good idea, because those attributes are
     figured out from the RTL in mark_constant_function, and
     the counter incrementation code from -fprofile-arcs
     leads to different results from -fbranch-probabilities.  */
  call = get_call_expr_in (t);
  if (call
      && !(call_expr_flags (call) & 
	   (ECF_NORETURN | ECF_LONGJMP | ECF_ALWAYS_RETURN)))
    return true;

  if (TREE_CODE (t) == ASM_EXPR
       && (ASM_VOLATILE_P (t) || ASM_INPUT_P (t)))
    return true;

  return false;
}


/* Add fake edges to the function exit for any non constant and non
   noreturn calls, volatile inline assembly in the bitmap of blocks
   specified by BLOCKS or to the whole CFG if BLOCKS is zero.  Return
   the number of blocks that were split.

   The goal is to expose cases in which entering a basic block does
   not imply that all subsequent instructions must be executed.  */

static int
tree_flow_call_edges_add (sbitmap blocks)
{
  int i;
  int blocks_split = 0;
  int last_bb = last_basic_block;
  bool check_last_block = false;

  if (n_basic_blocks == 0)
    return 0;

  if (! blocks)
    check_last_block = true;
  else
    check_last_block = TEST_BIT (blocks, EXIT_BLOCK_PTR->prev_bb->index);

  /* In the last basic block, before epilogue generation, there will be
     a fallthru edge to EXIT.  Special care is required if the last insn
     of the last basic block is a call because make_edge folds duplicate
     edges, which would result in the fallthru edge also being marked
     fake, which would result in the fallthru edge being removed by
     remove_fake_edges, which would result in an invalid CFG.

     Moreover, we can't elide the outgoing fake edge, since the block
     profiler needs to take this into account in order to solve the minimal
     spanning tree in the case that the call doesn't return.

     Handle this by adding a dummy instruction in a new last basic block.  */
  if (check_last_block)
    {
      basic_block bb = EXIT_BLOCK_PTR->prev_bb;
      block_stmt_iterator bsi = bsi_last (bb);
      tree t = NULL_TREE;
      if (!bsi_end_p (bsi))
	t = bsi_stmt (bsi);

      if (need_fake_edge_p (t))
	{
	  edge e;

	  for (e = bb->succ; e; e = e->succ_next)
	    if (e->dest == EXIT_BLOCK_PTR)
	      {
		bsi_insert_on_edge (e, build_empty_stmt ());
		bsi_commit_edge_inserts ((int *)NULL);
		break;
	      }
	}
    }

  /* Now add fake edges to the function exit for any non constant
     calls since there is no way that we can determine if they will
     return or not...  */
  for (i = 0; i < last_bb; i++)
    {
      basic_block bb = BASIC_BLOCK (i);
      block_stmt_iterator bsi;
      tree stmt, last_stmt;

      if (!bb)
	continue;

      if (blocks && !TEST_BIT (blocks, i))
	continue;

      bsi = bsi_last (bb);
      if (!bsi_end_p (bsi))
	{
	  last_stmt = bsi_stmt (bsi);
	  do
	    {
	      stmt = bsi_stmt (bsi);
	      if (need_fake_edge_p (stmt))
		{
		  edge e;
		  /* The handling above of the final block before the
		     epilogue should be enough to verify that there is
		     no edge to the exit block in CFG already.
		     Calling make_edge in such case would cause us to
		     mark that edge as fake and remove it later.  */
#ifdef ENABLE_CHECKING
		  if (stmt == last_stmt)
		    for (e = bb->succ; e; e = e->succ_next)
		      if (e->dest == EXIT_BLOCK_PTR)
			abort ();
#endif

		  /* Note that the following may create a new basic block
		     and renumber the existing basic blocks.  */
		  if (stmt != last_stmt)
		    {
		      e = split_block (bb, stmt);
		      if (e)
			blocks_split++;
		    }
		  make_edge (bb, EXIT_BLOCK_PTR, EDGE_FAKE);
		}
	      bsi_prev (&bsi);
	    }
	  while (!bsi_end_p (bsi));
	}
    }

  if (blocks_split)
    verify_flow_info ();

  return blocks_split;
}

bool
tree_purge_dead_eh_edges (basic_block bb)
{
  bool changed = false;
  edge e, next;
  tree stmt = last_stmt (bb);

  if (stmt && tree_can_throw_internal (stmt))
    return false;

  for (e = bb->succ; e ; e = next)
    {
      next = e->succ_next;
      if (e->flags & EDGE_EH)
	{
	  ssa_remove_edge (e);
	  changed = true;
	}
    }

  return changed;
}

bool
tree_purge_all_dead_eh_edges (bitmap blocks)
{
  bool changed = false;
  size_t i;

  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, i,
    { changed |= tree_purge_dead_eh_edges (BASIC_BLOCK (i)); });

  return changed;
}

struct cfg_hooks tree_cfg_hooks = {
  "tree",
  tree_verify_flow_info,
  tree_dump_bb,			/* dump_bb  */
  create_bb,			/* create_basic_block  */
  tree_redirect_edge_and_branch,/* redirect_edge_and_branch  */
  tree_redirect_edge_and_branch_force,/* redirect_edge_and_branch_force  */
  remove_bb,			/* delete_basic_block  */
  tree_split_block,		/* split_block  */
  tree_move_block_after,	/* move_block_after  */
  tree_can_merge_blocks_p,	/* can_merge_blocks_p  */
  tree_merge_blocks,		/* merge_blocks  */
  tree_predict_edge,		/* predict_edge  */
  tree_predicted_by_p,		/* predicted_by_p  */
  tree_can_duplicate_bb_p,	/* can_duplicate_block_p  */
  tree_duplicate_bb,		/* duplicate_block  */
  tree_split_edge,		/* split_edge  */
  tree_make_forwarder_block,	/* make_forward_block  */
  NULL,				/* tidy_fallthru_edge  */
  tree_block_ends_with_call_p,	/* block_ends_with_call_p */
  tree_block_ends_with_condjump_p, /* block_ends_with_condjump_p */
  tree_flow_call_edges_add      /* flow_call_edges_add */
};


/* Split all critical edges.  */

static void
split_critical_edges (void)
{
  basic_block bb;
  edge e;

  FOR_ALL_BB (bb)
    {
      for (e = bb->succ; e ; e = e->succ_next)
	if (EDGE_CRITICAL_P (e) && !(e->flags & EDGE_ABNORMAL))
	  {
	    split_edge (e);
	  }
    }
}

struct tree_opt_pass pass_split_crit_edges = 
{
  "crited",                          /* name */
  NULL,                          /* gate */
  split_critical_edges,          /* execute */
  NULL,                          /* sub */
  NULL,                          /* next */
  0,                             /* static_pass_number */
  TV_TREE_SPLIT_EDGES,           /* tv_id */
  PROP_cfg,                      /* properties required */
  PROP_no_crit_edges,            /* properties_provided */
  0,                             /* properties_destroyed */
  0,                             /* todo_flags_start */
  TODO_dump_func,                             /* todo_flags_finish */
};

/* Emit return warnings.  */

static void
execute_warn_function_return (void)
{
#ifdef USE_MAPPED_LOCATION
  source_location location;
#else
  location_t *locus;
#endif
  tree last;
  edge e;

  if (warn_missing_noreturn
      && !TREE_THIS_VOLATILE (cfun->decl)
      && EXIT_BLOCK_PTR->pred == NULL
      && !lang_hooks.function.missing_noreturn_ok_p (cfun->decl))
    warning ("%Jfunction might be possible candidate for attribute `noreturn'",
	     cfun->decl);

  /* If we have a path to EXIT, then we do return.  */
  if (TREE_THIS_VOLATILE (cfun->decl)
      && EXIT_BLOCK_PTR->pred != NULL)
    {
#ifdef USE_MAPPED_LOCATION
      location = UNKNOWN_LOCATION;
#else
      locus = NULL;
#endif
      for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
	{
	  last = last_stmt (e->src);
	  if (TREE_CODE (last) == RETURN_EXPR
#ifdef USE_MAPPED_LOCATION
	      && (location = EXPR_LOCATION (last)) != UNKNOWN_LOCATION)
#else
	      && (locus = EXPR_LOCUS (last)) != NULL)
#endif
	    break;
	}
#ifdef USE_MAPPED_LOCATION
      if (location == UNKNOWN_LOCATION)
	location = cfun->function_end_locus;
      warning ("%H`noreturn' function does return", &location);
#else
      if (!locus)
	locus = &cfun->function_end_locus;
      warning ("%H`noreturn' function does return", locus);
#endif
    }

  /* If we see "return;" in some basic block, then we do reach the end
     without returning a value.  */
  else if (warn_return_type
	   && EXIT_BLOCK_PTR->pred != NULL
	   && !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (cfun->decl))))
    {
      for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
	{
	  tree last = last_stmt (e->src);
	  if (TREE_CODE (last) == RETURN_EXPR
	      && TREE_OPERAND (last, 0) == NULL)
	    {
#ifdef USE_MAPPED_LOCATION
	      location = EXPR_LOCATION (last);
	      if (location == UNKNOWN_LOCATION)
		  location = cfun->function_end_locus;
	      warning ("%Hcontrol reaches end of non-void function", &location);
#else
	      locus = EXPR_LOCUS (last);
	      if (!locus)
		locus = &cfun->function_end_locus;
	      warning ("%Hcontrol reaches end of non-void function", locus);
#endif
	      break;
	    }
	}
    }
}


/* Given a basic block B which ends with a conditional and has
   precisely two successors, determine which of the edges is taken if
   the conditional is true and which is taken if the conditional is
   false.  Set TRUE_EDGE and FALSE_EDGE appropriately.  */

void
extract_true_false_edges_from_block (basic_block b,
				     edge *true_edge,
				     edge *false_edge)
{
  edge e = b->succ;

  if (e->flags & EDGE_TRUE_VALUE)
    {
      *true_edge = e;
      *false_edge = e->succ_next;
    }
  else
    {
      *false_edge = e;
      *true_edge = e->succ_next;
    }
}

struct tree_opt_pass pass_warn_function_return =
{
  NULL,					/* name */
  NULL,					/* gate */
  execute_warn_function_return,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
};

#include "gt-tree-cfg.h"
