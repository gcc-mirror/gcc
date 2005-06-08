/* Dead store elimination
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "basic-block.h"
#include "timevar.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "domwalk.h"
#include "flags.h"

/* This file implements dead store elimination.

   A dead store is a store into a memory location which will later be
   overwritten by another store without any intervening loads.  In this
   case the earlier store can be deleted.

   In our SSA + virtual operand world we use immediate uses of virtual
   operands to detect dead stores.  If a store's virtual definition
   is used precisely once by a later store to the same location which
   post dominates the first store, then the first store is dead. 

   The single use of the store's virtual definition ensures that
   there are no intervening aliased loads and the requirement that
   the second load post dominate the first ensures that if the earlier
   store executes, then the later stores will execute before the function
   exits.

   It may help to think of this as first moving the earlier store to
   the point immediately before the later store.  Again, the single
   use of the virtual definition and the post-dominance relationship
   ensure that such movement would be safe.  Clearly if there are 
   back to back stores, then the second is redundant.

   Reviewing section 10.7.2 in Morgan's "Building an Optimizing Compiler"
   may also help in understanding this code since it discusses the
   relationship between dead store and redundant load elimination.  In
   fact, they are the same transformation applied to different views of
   the CFG.  */
   

struct dse_global_data
{
  /* This is the global bitmap for store statements.

     Each statement has a unique ID.  When we encounter a store statement
     that we want to record, set the bit corresponding to the statement's
     unique ID in this bitmap.  */
  bitmap stores;
};

/* We allocate a bitmap-per-block for stores which are encountered
   during the scan of that block.  This allows us to restore the 
   global bitmap of stores when we finish processing a block.  */
struct dse_block_local_data
{
  bitmap stores;
};

static bool gate_dse (void);
static void tree_ssa_dse (void);
static void dse_initialize_block_local_data (struct dom_walk_data *,
					     basic_block,
					     bool);
static void dse_optimize_stmt (struct dom_walk_data *,
			       basic_block,
			       block_stmt_iterator);
static void dse_record_phis (struct dom_walk_data *, basic_block);
static void dse_finalize_block (struct dom_walk_data *, basic_block);
static void fix_phi_uses (tree, tree);
static void fix_stmt_v_may_defs (tree, tree);
static void record_voperand_set (bitmap, bitmap *, unsigned int);

static unsigned max_stmt_uid;	/* Maximal uid of a statement.  Uids to phi
				   nodes are assigned using the versions of
				   ssa names they define.  */

/* Returns uid of statement STMT.  */

static unsigned
get_stmt_uid (tree stmt)
{
  if (TREE_CODE (stmt) == PHI_NODE)
    return SSA_NAME_VERSION (PHI_RESULT (stmt)) + max_stmt_uid;

  return stmt_ann (stmt)->uid;
}

/* Function indicating whether we ought to include information for 'var'
   when calculating immediate uses.  For this pass we only want use
   information for virtual variables.  */

static bool
need_imm_uses_for (tree var)
{
  return !is_gimple_reg (var);
}


/* Replace uses in PHI which match V_MAY_DEF_RESULTs in STMT with the 
   corresponding V_MAY_DEF_OP in STMT.  */

static void
fix_phi_uses (tree phi, tree stmt)
{
  use_operand_p use_p;
  def_operand_p def_p;
  ssa_op_iter iter;
  int i;
  edge e;
  edge_iterator ei;
  
  FOR_EACH_EDGE (e, ei, PHI_BB (phi)->preds) 
  if (e->flags & EDGE_ABNORMAL)
    break;
  
  get_stmt_operands (stmt);

  FOR_EACH_SSA_MAYDEF_OPERAND (def_p, use_p, stmt, iter)
    {
      tree v_may_def = DEF_FROM_PTR (def_p);
      tree v_may_use = USE_FROM_PTR (use_p);

      /* Find any uses in the PHI which match V_MAY_DEF and replace
	 them with the appropriate V_MAY_DEF_OP.  */
      for (i = 0; i < PHI_NUM_ARGS (phi); i++)
	if (v_may_def == PHI_ARG_DEF (phi, i))
	  {
	    SET_PHI_ARG_DEF (phi, i, v_may_use);
	    /* Update if the new phi argument is an abnormal phi.  */
	    if (e != NULL)
	      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (v_may_use) = 1;
	  }
    }
}

/* Replace the V_MAY_DEF_OPs in STMT1 which match V_MAY_DEF_RESULTs 
   in STMT2 with the appropriate V_MAY_DEF_OPs from STMT2.  */

static void
fix_stmt_v_may_defs (tree stmt1, tree stmt2)
{
  bool found = false;
  ssa_op_iter iter1;
  ssa_op_iter iter2;
  use_operand_p use1_p, use2_p;
  def_operand_p def1_p, def2_p;

  get_stmt_operands (stmt1);
  get_stmt_operands (stmt2);

  /* Walk each V_MAY_DEF_OP in stmt1.  */
  FOR_EACH_SSA_MAYDEF_OPERAND (def1_p, use1_p, stmt1, iter1)
    {
      tree use = USE_FROM_PTR (use1_p);

      /* Find the appropriate V_MAY_DEF_RESULT in STMT2.  */
      FOR_EACH_SSA_MAYDEF_OPERAND (def2_p, use2_p, stmt2, iter2)
	{
	  tree def = DEF_FROM_PTR (def2_p);
	  if (use == def)
	    {
	      /* Update.  */
	      SET_USE (use1_p, USE_FROM_PTR (use2_p));
	      found = true;
              break;
            }
	}

      /* If we did not find a corresponding V_MAY_DEF_RESULT,
	 then something has gone terribly wrong.  */
      gcc_assert (found);
    }
}


/* Set bit UID in bitmaps GLOBAL and *LOCAL, creating *LOCAL as needed.  */
static void
record_voperand_set (bitmap global, bitmap *local, unsigned int uid)
{
  /* Lazily allocate the bitmap.  Note that we do not get a notification
     when the block local data structures die, so we allocate the local
     bitmap backed by the GC system.  */
  if (*local == NULL)
    *local = BITMAP_GGC_ALLOC ();

  /* Set the bit in the local and global bitmaps.  */
  bitmap_set_bit (*local, uid);
  bitmap_set_bit (global, uid);
}
/* Initialize block local data structures.  */

static void
dse_initialize_block_local_data (struct dom_walk_data *walk_data,
				 basic_block bb ATTRIBUTE_UNUSED,
				 bool recycled)
{
  struct dse_block_local_data *bd
    = VARRAY_TOP_GENERIC_PTR (walk_data->block_data_stack);

  /* If we are given a recycled block local data structure, ensure any
     bitmap associated with the block is cleared.  */
  if (recycled)
    {
      if (bd->stores)
	bitmap_clear (bd->stores);
    }
}

/* Attempt to eliminate dead stores in the statement referenced by BSI.

   A dead store is a store into a memory location which will later be
   overwritten by another store without any intervening loads.  In this
   case the earlier store can be deleted.

   In our SSA + virtual operand world we use immediate uses of virtual
   operands to detect dead stores.  If a store's virtual definition
   is used precisely once by a later store to the same location which
   post dominates the first store, then the first store is dead.  */

static void
dse_optimize_stmt (struct dom_walk_data *walk_data,
		   basic_block bb ATTRIBUTE_UNUSED,
		   block_stmt_iterator bsi)
{
  struct dse_block_local_data *bd
    = VARRAY_TOP_GENERIC_PTR (walk_data->block_data_stack);
  struct dse_global_data *dse_gd = walk_data->global_data;
  tree stmt = bsi_stmt (bsi);
  stmt_ann_t ann = stmt_ann (stmt);
  v_may_def_optype v_may_defs;

  get_stmt_operands (stmt);
  v_may_defs = V_MAY_DEF_OPS (ann);

  /* If this statement has no virtual uses, then there is nothing
     to do.  */
  if (NUM_V_MAY_DEFS (v_may_defs) == 0)
    return;

  /* We know we have virtual definitions.  If this is a MODIFY_EXPR that's
     not also a function call, then record it into our table.  */
  if (get_call_expr_in (stmt))
    return;

  if (ann->has_volatile_ops)
    return;

  if (TREE_CODE (stmt) == MODIFY_EXPR)
    {
      dataflow_t df = get_immediate_uses (stmt);
      unsigned int num_uses = num_immediate_uses (df);
      tree use;
      tree skipped_phi;

      /* If there are no uses then there is nothing left to do.  */
      if (num_uses == 0)
	{
	  record_voperand_set (dse_gd->stores, &bd->stores, ann->uid);
	  return;
	}

      use = immediate_use (df, 0);
      skipped_phi = NULL;

      /* Skip through any PHI nodes we have already seen if the PHI
	 represents the only use of this store.

	 Note this does not handle the case where the store has
	 multiple V_MAY_DEFs which all reach a set of PHI nodes in the
	 same block.  */
      while (num_uses == 1
	     && TREE_CODE (use) == PHI_NODE
	     && bitmap_bit_p (dse_gd->stores, get_stmt_uid (use)))
	{
	  /* Record the first PHI we skip so that we can fix its
	     uses if we find that STMT is a dead store.  */
	  if (!skipped_phi)
	    skipped_phi = use;

	  /* Skip past this PHI and loop again in case we had a PHI
	     chain.  */
	  df = get_immediate_uses (use);
	  num_uses = num_immediate_uses (df);
	  use = immediate_use (df, 0);
	}

      /* If we have precisely one immediate use at this point, then we may
	 have found redundant store.  */
      if (num_uses == 1
	  && bitmap_bit_p (dse_gd->stores, get_stmt_uid (use))
	  && operand_equal_p (TREE_OPERAND (stmt, 0),
			      TREE_OPERAND (use, 0), 0))
	{
	  /* We need to fix the operands if either the first PHI we
	     skipped, or the store which we are not deleting if we did
	     not skip any PHIs.  */
	  if (skipped_phi)
	    fix_phi_uses (skipped_phi, stmt);
	  else
	    fix_stmt_v_may_defs (use, stmt);

	  if (dump_file && (dump_flags & TDF_DETAILS))
            {
              fprintf (dump_file, "  Deleted dead store '");
              print_generic_expr (dump_file, bsi_stmt (bsi), dump_flags);
              fprintf (dump_file, "'\n");
            }

	  /* Any immediate uses which reference STMT need to instead
	     reference the new consumer, either SKIPPED_PHI or USE.  
	     This allows us to cascade dead stores.  */
	  redirect_immediate_uses (stmt, skipped_phi ? skipped_phi : use);

	  /* Be sure to remove any dataflow information attached to
	     this statement.  */
	  free_df_for_stmt (stmt);

	  /* And release any SSA_NAMEs set in this statement back to the
	     SSA_NAME manager.  */
	  release_defs (stmt);

	  /* Finally remove the dead store.  */
	  bsi_remove (&bsi);
	}

      record_voperand_set (dse_gd->stores, &bd->stores, ann->uid);
    }
}

/* Record that we have seen the PHIs at the start of BB which correspond
   to virtual operands.  */
static void
dse_record_phis (struct dom_walk_data *walk_data, basic_block bb)
{
  struct dse_block_local_data *bd
    = VARRAY_TOP_GENERIC_PTR (walk_data->block_data_stack);
  struct dse_global_data *dse_gd = walk_data->global_data;
  tree phi;

  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    if (need_imm_uses_for (PHI_RESULT (phi)))
      record_voperand_set (dse_gd->stores,
			   &bd->stores,
			   get_stmt_uid (phi));
}

static void
dse_finalize_block (struct dom_walk_data *walk_data,
		    basic_block bb ATTRIBUTE_UNUSED)
{
  struct dse_block_local_data *bd
    = VARRAY_TOP_GENERIC_PTR (walk_data->block_data_stack);
  struct dse_global_data *dse_gd = walk_data->global_data;
  bitmap stores = dse_gd->stores;
  unsigned int i;
  bitmap_iterator bi;

  /* Unwind the stores noted in this basic block.  */
  if (bd->stores)
    EXECUTE_IF_SET_IN_BITMAP (bd->stores, 0, i, bi)
      {
	bitmap_clear_bit (stores, i);
      }
}

static void
tree_ssa_dse (void)
{
  struct dom_walk_data walk_data;
  struct dse_global_data dse_gd;
  basic_block bb;

  /* Create a UID for each statement in the function.  Ordering of the
     UIDs is not important for this pass.  */
  max_stmt_uid = 0;
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi;

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	stmt_ann (bsi_stmt (bsi))->uid = max_stmt_uid++;
    }

  /* We might consider making this a property of each pass so that it
     can be [re]computed on an as-needed basis.  Particularly since
     this pass could be seen as an extension of DCE which needs post
     dominators.  */
  calculate_dominance_info (CDI_POST_DOMINATORS);

  /* We also need immediate use information for virtual operands.  */
  compute_immediate_uses (TDFA_USE_VOPS, need_imm_uses_for);

  /* Dead store elimination is fundamentally a walk of the post-dominator
     tree and a backwards walk of statements within each block.  */
  walk_data.walk_stmts_backward = true;
  walk_data.dom_direction = CDI_POST_DOMINATORS;
  walk_data.initialize_block_local_data = dse_initialize_block_local_data;
  walk_data.before_dom_children_before_stmts = NULL;
  walk_data.before_dom_children_walk_stmts = dse_optimize_stmt;
  walk_data.before_dom_children_after_stmts = dse_record_phis;
  walk_data.after_dom_children_before_stmts = NULL;
  walk_data.after_dom_children_walk_stmts = NULL;
  walk_data.after_dom_children_after_stmts = dse_finalize_block;

  walk_data.block_local_data_size = sizeof (struct dse_block_local_data);

  /* This is the main hash table for the dead store elimination pass.  */
  dse_gd.stores = BITMAP_ALLOC (NULL);
  walk_data.global_data = &dse_gd;

  /* Initialize the dominator walker.  */
  init_walk_dominator_tree (&walk_data);

  /* Recursively walk the dominator tree.  */
  walk_dominator_tree (&walk_data, EXIT_BLOCK_PTR);

  /* Finalize the dominator walker.  */
  fini_walk_dominator_tree (&walk_data);

  /* Release the main bitmap.  */
  BITMAP_FREE (dse_gd.stores);

  /* Free dataflow information.  It's probably out of date now anyway.  */
  free_df ();

  /* For now, just wipe the post-dominator information.  */
  free_dominance_info (CDI_POST_DOMINATORS);
}

static bool
gate_dse (void)
{
  return flag_tree_dse != 0;
}

struct tree_opt_pass pass_dse = {
  "dse",			/* name */
  gate_dse,			/* gate */
  tree_ssa_dse,			/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_TREE_DSE,			/* tv_id */
  PROP_cfg | PROP_ssa
    | PROP_alias,		/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect	/* todo_flags_finish */
  | TODO_verify_ssa,
  0					/* letter */
};
