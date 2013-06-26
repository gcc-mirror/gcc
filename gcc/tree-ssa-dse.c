/* Dead store elimination
   Copyright (C) 2004-2013 Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-pass.h"
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


/* Bitmap of blocks that have had EH statements cleaned.  We should
   remove their dead edges eventually.  */
static bitmap need_eh_cleanup;

static bool gate_dse (void);
static unsigned int tree_ssa_dse (void);
static void dse_enter_block (struct dom_walk_data *, basic_block);


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

  /* Self-assignments are zombies.  */
  if (operand_equal_p (gimple_assign_rhs1 (stmt), gimple_assign_lhs (stmt), 0))
    {
      *use_stmt = stmt;
      return true;
    }

  /* Find the first dominated statement that clobbers (part of) the
     memory stmt stores to with no intermediate statement that may use
     part of the memory stmt stores.  That is, find a store that may
     prove stmt to be a dead store.  */
  temp = stmt;
  do
    {
      gimple use_stmt, defvar_def;
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
      defvar_def = temp;
      temp = NULL;
      FOR_EACH_IMM_USE_STMT (use_stmt, ui, defvar)
	{
	  cnt++;

	  /* If we ever reach our DSE candidate stmt again fail.  We
	     cannot handle dead stores in loops.  */
	  if (use_stmt == stmt)
	    {
	      fail = true;
	      BREAK_FROM_IMM_USE_STMT (ui);
	    }
	  /* In simple cases we can look through PHI nodes, but we
	     have to be careful with loops and with memory references
	     containing operands that are also operands of PHI nodes.
	     See gcc.c-torture/execute/20051110-*.c.  */
	  else if (gimple_code (use_stmt) == GIMPLE_PHI)
	    {
	      if (temp
		  /* Make sure we are not in a loop latch block.  */
		  || gimple_bb (stmt) == gimple_bb (use_stmt)
		  || dominated_by_p (CDI_DOMINATORS,
				     gimple_bb (stmt), gimple_bb (use_stmt))
		  /* We can look through PHIs to regions post-dominating
		     the DSE candidate stmt.  */
		  || !dominated_by_p (CDI_POST_DOMINATORS,
				      gimple_bb (stmt), gimple_bb (use_stmt)))
		{
		  fail = true;
		  BREAK_FROM_IMM_USE_STMT (ui);
		}
	      /* Do not consider the PHI as use if it dominates the 
	         stmt defining the virtual operand we are processing,
		 we have processed it already in this case.  */
	      if (gimple_bb (defvar_def) != gimple_bb (use_stmt)
		  && !dominated_by_p (CDI_DOMINATORS,
				      gimple_bb (defvar_def),
				      gimple_bb (use_stmt)))
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
	  if (stmt_may_clobber_global_p (stmt))
	    return false;

	  temp = stmt;
	  break;
	}
    }
  /* We deliberately stop on clobbering statements and not only on
     killing ones to make walking cheaper.  Otherwise we can just
     continue walking until both stores have equal reference trees.  */
  while (!stmt_may_clobber_ref_p (temp, gimple_assign_lhs (stmt)));

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
dse_optimize_stmt (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);

  /* If this statement has no virtual defs, then there is nothing
     to do.  */
  if (!gimple_vdef (stmt))
    return;

  /* We know we have virtual definitions.  If this is a GIMPLE_ASSIGN
     that's not also a function call, then record it into our table.  */
  if (is_gimple_call (stmt) && gimple_call_fndecl (stmt))
    return;

  /* Don't return early on *this_2(D) ={v} {CLOBBER}.  */
  if (gimple_has_volatile_ops (stmt)
      && (!gimple_clobber_p (stmt)
	  || TREE_CODE (gimple_assign_lhs (stmt)) != MEM_REF))
    return;

  if (is_gimple_assign (stmt))
    {
      gimple use_stmt;

      if (!dse_possible_dead_store_p (stmt, &use_stmt))
	return;

      /* But only remove *this_2(D) ={v} {CLOBBER} if killed by
	 another clobber stmt.  */
      if (gimple_clobber_p (stmt)
	  && !gimple_clobber_p (use_stmt))
	return;

      /* If we have precisely one immediate use at this point and the
	 stores are to the same memory location or there is a chain of
	 virtual uses from stmt and the stmt which stores to that same
	 memory location, then we may have found redundant store.  */
      if ((gimple_has_lhs (use_stmt)
	   && (operand_equal_p (gimple_assign_lhs (stmt),
				gimple_get_lhs (use_stmt), 0)))
	  || stmt_kills_ref_p (use_stmt, gimple_assign_lhs (stmt)))
	{
	  basic_block bb;

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
	      && ref_maybe_used_by_stmt_p (use_stmt, gimple_assign_lhs (stmt)))
	    return;

	  if (dump_file && (dump_flags & TDF_DETAILS))
            {
              fprintf (dump_file, "  Deleted dead store '");
              print_gimple_stmt (dump_file, gsi_stmt (*gsi), dump_flags, 0);
              fprintf (dump_file, "'\n");
            }

	  /* Then we need to fix the operand of the consuming stmt.  */
	  unlink_stmt_vdef (stmt);

	  /* Remove the dead store.  */
	  bb = gimple_bb (stmt);
	  if (gsi_remove (gsi, true))
	    bitmap_set_bit (need_eh_cleanup, bb->index);

	  /* And release any SSA_NAMEs set in this statement back to the
	     SSA_NAME manager.  */
	  release_defs (stmt);
	}
    }
}

static void
dse_enter_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
		 basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi);)
    {
      dse_optimize_stmt (&gsi);
      if (gsi_end_p (gsi))
	gsi = gsi_last_bb (bb);
      else
	gsi_prev (&gsi);
    }
}

/* Main entry point.  */

static unsigned int
tree_ssa_dse (void)
{
  struct dom_walk_data walk_data;

  need_eh_cleanup = BITMAP_ALLOC (NULL);

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
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children = dse_enter_block;
  walk_data.after_dom_children = NULL;

  walk_data.block_local_data_size = 0;
  walk_data.global_data = NULL;

  /* Initialize the dominator walker.  */
  init_walk_dominator_tree (&walk_data);

  /* Recursively walk the dominator tree.  */
  walk_dominator_tree (&walk_data, EXIT_BLOCK_PTR);

  /* Finalize the dominator walker.  */
  fini_walk_dominator_tree (&walk_data);

  /* Removal of stores may make some EH edges dead.  Purge such edges from
     the CFG as needed.  */
  if (!bitmap_empty_p (need_eh_cleanup))
    {
      gimple_purge_all_dead_eh_edges (need_eh_cleanup);
      cleanup_tree_cfg ();
    }

  BITMAP_FREE (need_eh_cleanup);
    
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
  OPTGROUP_NONE,                /* optinfo_flags */
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
  TODO_verify_ssa		/* todo_flags_finish */
 }
};
