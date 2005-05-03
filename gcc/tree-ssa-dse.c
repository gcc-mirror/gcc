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

  /* If this statement has no virtual defs, then there is nothing
     to do.  */
  if (ZERO_SSA_OPERANDS (stmt, (SSA_OP_VMAYDEF|SSA_OP_VMUSTDEF)))
    return;

  /* We know we have virtual definitions.  If this is a MODIFY_EXPR that's
     not also a function call, then record it into our table.  */
  if (get_call_expr_in (stmt))
    return;

  if (ann->has_volatile_ops)
    return;

  if (TREE_CODE (stmt) == MODIFY_EXPR)
    {
      use_operand_p first_use_p = NULL_USE_OPERAND_P;
      use_operand_p use_p = NULL;
      tree use, use_stmt, temp;
      tree defvar = NULL_TREE, usevar = NULL_TREE;
      bool fail = false;
      use_operand_p var2;
      def_operand_p var1;
      ssa_op_iter op_iter;

      /* We want to verify that each virtual definition in STMT has
	 precisely one use and that all the virtual definitions are
	 used by the same single statement.  When complete, we
	 want USE_STMT to refer to the one statment which uses
	 all of the virtual definitions from STMT.  */
      use_stmt = NULL;
      FOR_EACH_SSA_MUST_AND_MAY_DEF_OPERAND (var1, var2, stmt, op_iter)
	{
	  defvar = DEF_FROM_PTR (var1);
	  usevar = USE_FROM_PTR (var2);

	  /* If this virtual def does not have precisely one use, then
	     we will not be able to eliminate STMT.  */
	  if (num_imm_uses (defvar) != 1)
	    {
	      fail = true;
	      break;
	    }

	  /* Get the one and only immediate use of DEFVAR.  */
	  single_imm_use (defvar, &use_p, &temp);
	  gcc_assert (use_p != NULL_USE_OPERAND_P);
	  first_use_p = use_p;
	  use = USE_FROM_PTR (use_p);

	  /* If the immediate use of DEF_VAR is not the same as the
	     previously find immediate uses, then we will not be able
	     to eliminate STMT.  */
	  if (use_stmt == NULL)
	    use_stmt = temp;
	  else if (temp != use_stmt)
	    {
	      fail = true;
	      break;
	    }
	}

      if (fail)
	{
	  record_voperand_set (dse_gd->stores, &bd->stores, ann->uid);
	  return;
	}

      /* Skip through any PHI nodes we have already seen if the PHI
	 represents the only use of this store.

	 Note this does not handle the case where the store has
	 multiple V_{MAY,MUST}_DEFs which all reach a set of PHI nodes in the
	 same block.  */
      while (use_p != NULL_USE_OPERAND_P
	     && TREE_CODE (use_stmt) == PHI_NODE
	     && bitmap_bit_p (dse_gd->stores, get_stmt_uid (use_stmt)))
	{
	  /* Skip past this PHI and loop again in case we had a PHI
	     chain.  */
	  if (single_imm_use (PHI_RESULT (use_stmt), &use_p, &use_stmt))
	    use = USE_FROM_PTR (use_p);
	}

      /* If we have precisely one immediate use at this point, then we may
	 have found redundant store.  */
      if (use_p != NULL_USE_OPERAND_P
	  && bitmap_bit_p (dse_gd->stores, get_stmt_uid (use_stmt))
	  && operand_equal_p (TREE_OPERAND (stmt, 0),
			      TREE_OPERAND (use_stmt, 0), 0))
	{
	  tree def;
	  ssa_op_iter iter;

	  /* Make sure we propagate the ABNORMAL bit setting.  */
	  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (USE_FROM_PTR (first_use_p)))
	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (usevar) = 1;
	  /* Then we need to fix the operand of the consuming stmt.  */
	  SET_USE (first_use_p, usevar);

	  if (dump_file && (dump_flags & TDF_DETAILS))
            {
              fprintf (dump_file, "  Deleted dead store '");
              print_generic_expr (dump_file, bsi_stmt (bsi), dump_flags);
              fprintf (dump_file, "'\n");
            }

	  /* Remove the dead store.  */
	  bsi_remove (&bsi);

	  /* The virtual defs for the dead statement will need to be
	     updated.  Since these names are going to disappear,
	     FUD chains for uses downstream need to be updated.  */
	  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_VIRTUAL_DEFS)
	    mark_sym_for_renaming (SSA_NAME_VAR (def));

	  /* And release any SSA_NAMEs set in this statement back to the
	     SSA_NAME manager.  */
	  release_defs (stmt);
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
    if (!is_gimple_reg (PHI_RESULT (phi)))
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
  walk_data.interesting_blocks = NULL;

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
  PROP_cfg
    | PROP_ssa
    | PROP_alias,		/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_func
    | TODO_ggc_collect
    | TODO_update_ssa
    | TODO_verify_ssa,		/* todo_flags_finish */
  0				/* letter */
};
