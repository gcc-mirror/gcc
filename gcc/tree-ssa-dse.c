/* Dead store elimination
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
#include "tm.h"
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
#include "langhooks.h"

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
static unsigned int tree_ssa_dse (void);
static void dse_initialize_block_local_data (struct dom_walk_data *,
					     basic_block,
					     bool);
static void dse_enter_block (struct dom_walk_data *, basic_block);
static void dse_leave_block (struct dom_walk_data *, basic_block);
static void record_voperand_set (bitmap, bitmap *, unsigned int);

/* Returns uid of statement STMT.  */

static unsigned
get_stmt_uid (gimple stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    return SSA_NAME_VERSION (gimple_phi_result (stmt))
           + gimple_stmt_max_uid (cfun);

  return gimple_uid (stmt);
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
    = (struct dse_block_local_data *)
	VEC_last (void_p, walk_data->block_data_stack);

  /* If we are given a recycled block local data structure, ensure any
     bitmap associated with the block is cleared.  */
  if (recycled)
    {
      if (bd->stores)
	bitmap_clear (bd->stores);
    }
}

/* A helper of dse_optimize_stmt.
   Given a GIMPLE_ASSIGN in STMT, find a candidate statement *USE_STMT that
   may prove STMT to be dead.
   Return TRUE if the above conditions are met, otherwise FALSE.  */

static bool
dse_possible_dead_store_p (gimple stmt, gimple *use_stmt)
{
  gimple temp;
  unsigned cnt = 0;

  *use_stmt = NULL;

  /* Find the first dominated statement that clobbers (part of) the
     memory stmt stores to with no intermediate statement that may use
     part of the memory stmt stores.  That is, find a store that may
     prove stmt to be a dead store.  */
  temp = stmt;
  do
    {
      gimple prev, use_stmt;
      imm_use_iterator ui;
      bool fail = false;
      tree defvar;

      /* Limit stmt walking to be linear in the number of possibly
         dead stores.  */
      if (++cnt > 256)
	return false;

      if (gimple_code (temp) == GIMPLE_PHI)
	defvar = PHI_RESULT (temp);
      else
	defvar = gimple_vdef (temp);
      prev = temp;
      temp = NULL;
      FOR_EACH_IMM_USE_STMT (use_stmt, ui, defvar)
	{
	  cnt++;

	  /* In simple cases we can look through PHI nodes, but we
	     have to be careful with loops and with memory references
	     containing operands that are also operands of PHI nodes.
	     See gcc.c-torture/execute/20051110-*.c.  */
	  if (gimple_code (use_stmt) == GIMPLE_PHI)
	    {
	      if (temp
		  /* We can look through PHIs to post-dominated regions
		     without worrying if the use not also dominates prev
		     (in which case it would be a loop PHI with the use
		     in a latch block).  */
		  || gimple_bb (prev) == gimple_bb (use_stmt)
		  || !dominated_by_p (CDI_POST_DOMINATORS,
				      gimple_bb (prev), gimple_bb (use_stmt))
		  || dominated_by_p (CDI_DOMINATORS,
				     gimple_bb (prev), gimple_bb (use_stmt)))
		{
		  fail = true;
		  BREAK_FROM_IMM_USE_STMT (ui);
		}
	      temp = use_stmt;
	    }
	  /* If the statement is a use the store is not dead.  */
	  else if (ref_maybe_used_by_stmt_p (use_stmt,
					     gimple_assign_lhs (stmt)))
	    {
	      fail = true;
	      BREAK_FROM_IMM_USE_STMT (ui);
	    }
	  /* If this is a store, remember it or bail out if we have
	     multiple ones (the will be in different CFG parts then).  */
	  else if (gimple_vdef (use_stmt))
	    {
	      if (temp)
		{
		  fail = true;
		  BREAK_FROM_IMM_USE_STMT (ui);
		}
	      temp = use_stmt;
	    }
	}

      if (fail)
	return false;

      /* If we didn't find any definition this means the store is dead
         if it isn't a store to global reachable memory.  In this case
	 just pretend the stmt makes itself dead.  Otherwise fail.  */
      if (!temp)
	{
	  if (is_hidden_global_store (stmt))
	    return false;

	  temp = stmt;
	  break;
	}
    }
  /* We deliberately stop on clobbering statements and not only on
     killing ones to make walking cheaper.  Otherwise we can just
     continue walking until both stores have equal reference trees.  */
  while (!stmt_may_clobber_ref_p (temp, gimple_assign_lhs (stmt)));

  if (!is_gimple_assign (temp))
    return false;

  *use_stmt = temp;

  return true;
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
dse_optimize_stmt (struct dse_global_data *dse_gd,
		   struct dse_block_local_data *bd,
		   gimple_stmt_iterator gsi)
{
  gimple stmt = gsi_stmt (gsi);

  /* If this statement has no virtual defs, then there is nothing
     to do.  */
  if (!gimple_vdef (stmt))
    return;

  /* We know we have virtual definitions.  If this is a GIMPLE_ASSIGN
     that's not also a function call, then record it into our table.  */
  if (is_gimple_call (stmt) && gimple_call_fndecl (stmt))
    return;

  if (gimple_has_volatile_ops (stmt))
    return;

  if (is_gimple_assign (stmt))
    {
      gimple use_stmt;

      record_voperand_set (dse_gd->stores, &bd->stores, gimple_uid (stmt));

      if (!dse_possible_dead_store_p (stmt, &use_stmt))
	return;

      /* If we have precisely one immediate use at this point and the
	 stores are to the same memory location or there is a chain of
	 virtual uses from stmt and the stmt which stores to that same
	 memory location, then we may have found redundant store.  */
      if (bitmap_bit_p (dse_gd->stores, get_stmt_uid (use_stmt))
	  && operand_equal_p (gimple_assign_lhs (stmt),
			      gimple_assign_lhs (use_stmt), 0))
	{
	  /* If use_stmt is or might be a nop assignment, e.g. for
	     struct { ... } S a, b, *p; ...
	     b = a; b = b;
	     or
	     b = a; b = *p; where p might be &b,
	     or
	     *p = a; *p = b; where p might be &b,
	     or
	     *p = *u; *p = *v; where p might be v, then USE_STMT
	     acts as a use as well as definition, so store in STMT
	     is not dead.  */
	  if (stmt != use_stmt
	      && !is_gimple_reg (gimple_assign_rhs1 (use_stmt))
	      && !is_gimple_min_invariant (gimple_assign_rhs1 (use_stmt))
	      /* ???  Should {} be invariant?  */
	      && gimple_assign_rhs_code (use_stmt) != CONSTRUCTOR
	      && refs_may_alias_p (gimple_assign_lhs (use_stmt),
				   gimple_assign_rhs1 (use_stmt)))
	    return;

	  if (dump_file && (dump_flags & TDF_DETAILS))
            {
              fprintf (dump_file, "  Deleted dead store '");
              print_gimple_stmt (dump_file, gsi_stmt (gsi), dump_flags, 0);
              fprintf (dump_file, "'\n");
            }

	  /* Then we need to fix the operand of the consuming stmt.  */
	  unlink_stmt_vdef (stmt);

	  /* Remove the dead store.  */
	  gsi_remove (&gsi, true);

	  /* And release any SSA_NAMEs set in this statement back to the
	     SSA_NAME manager.  */
	  release_defs (stmt);
	}
    }
}

/* Record that we have seen the PHIs at the start of BB which correspond
   to virtual operands.  */
static void
dse_record_phi (struct dse_global_data *dse_gd,
		struct dse_block_local_data *bd,
		gimple phi)
{
  if (!is_gimple_reg (gimple_phi_result (phi)))
    record_voperand_set (dse_gd->stores, &bd->stores, get_stmt_uid (phi));
}

static void
dse_enter_block (struct dom_walk_data *walk_data, basic_block bb)
{
  struct dse_block_local_data *bd
    = (struct dse_block_local_data *)
	VEC_last (void_p, walk_data->block_data_stack);
  struct dse_global_data *dse_gd
    = (struct dse_global_data *) walk_data->global_data;
  gimple_stmt_iterator gsi;

  for (gsi = gsi_last (bb_seq (bb)); !gsi_end_p (gsi); gsi_prev (&gsi))
    dse_optimize_stmt (dse_gd, bd, gsi);
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    dse_record_phi (dse_gd, bd, gsi_stmt (gsi));
}

static void
dse_leave_block (struct dom_walk_data *walk_data,
		 basic_block bb ATTRIBUTE_UNUSED)
{
  struct dse_block_local_data *bd
    = (struct dse_block_local_data *)
	VEC_last (void_p, walk_data->block_data_stack);
  struct dse_global_data *dse_gd
    = (struct dse_global_data *) walk_data->global_data;
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

/* Main entry point.  */

static unsigned int
tree_ssa_dse (void)
{
  struct dom_walk_data walk_data;
  struct dse_global_data dse_gd;

  renumber_gimple_stmt_uids ();

  /* We might consider making this a property of each pass so that it
     can be [re]computed on an as-needed basis.  Particularly since
     this pass could be seen as an extension of DCE which needs post
     dominators.  */
  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);

  /* Dead store elimination is fundamentally a walk of the post-dominator
     tree and a backwards walk of statements within each block.  */
  walk_data.dom_direction = CDI_POST_DOMINATORS;
  walk_data.initialize_block_local_data = dse_initialize_block_local_data;
  walk_data.before_dom_children = dse_enter_block;
  walk_data.after_dom_children = dse_leave_block;

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
  return 0;
}

static bool
gate_dse (void)
{
  return flag_tree_dse != 0;
}

struct gimple_opt_pass pass_dse = 
{
 {
  GIMPLE_PASS,
  "dse",			/* name */
  gate_dse,			/* gate */
  tree_ssa_dse,			/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_TREE_DSE,			/* tv_id */
  PROP_cfg | PROP_ssa,		/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_func
    | TODO_ggc_collect
    | TODO_verify_ssa		/* todo_flags_finish */
 }
};

