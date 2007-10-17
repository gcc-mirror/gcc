/* Top-level control of tree optimizations.
   Copyright 2001, 2002, 2003, 2004, 2005, 2007 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "expr.h"
#include "diagnostic.h"
#include "basic-block.h"
#include "flags.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "function.h"
#include "langhooks.h"
#include "toplev.h"
#include "flags.h"
#include "cgraph.h"
#include "tree-inline.h"
#include "tree-mudflap.h"
#include "tree-pass.h"
#include "ggc.h"
#include "cgraph.h"
#include "graph.h"
#include "cfgloop.h"
#include "except.h"


/* Gate: execute, or not, all of the non-trivial optimizations.  */

static bool
gate_all_optimizations (void)
{
  return (optimize >= 1
	  /* Don't bother doing anything if the program has errors. 
	     We have to pass down the queue if we already went into SSA */
	  && (!(errorcount || sorrycount) || gimple_in_ssa_p (cfun)));
}

struct tree_opt_pass pass_all_optimizations =
{
  NULL,					/* name */
  gate_all_optimizations,		/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
  0					/* letter */
};

/* Gate: execute, or not, all of the non-trivial optimizations.  */

static bool
gate_all_early_local_passes (void)
{
	  /* Don't bother doing anything if the program has errors.  */
  return (!errorcount && !sorrycount);
}

struct tree_opt_pass pass_early_local_passes =
{
  "early_local_cleanups",		/* name */
  gate_all_early_local_passes,		/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_remove_functions,		/* todo_flags_finish */
  0					/* letter */
};

static unsigned int
execute_early_local_optimizations (void)
{
  if (flag_unit_at_a_time)
    cgraph_state = CGRAPH_STATE_IPA_SSA;
  return 0;
}

/* Gate: execute, or not, all of the non-trivial optimizations.  */

static bool
gate_all_early_optimizations (void)
{
  return (optimize >= 1
	  /* Don't bother doing anything if the program has errors.  */
	  && !(errorcount || sorrycount));
}

struct tree_opt_pass pass_all_early_optimizations =
{
  "early_optimizations",		/* name */
  gate_all_early_optimizations,		/* gate */
  execute_early_local_optimizations,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
  0					/* letter */
};

/* Pass: cleanup the CFG just before expanding trees to RTL.
   This is just a round of label cleanups and case node grouping
   because after the tree optimizers have run such cleanups may
   be necessary.  */

static unsigned int
execute_cleanup_cfg_pre_ipa (void)
{
  cleanup_tree_cfg ();
  return 0;
}

struct tree_opt_pass pass_cleanup_cfg =
{
  "cleanup_cfg",			/* name */
  NULL,					/* gate */
  execute_cleanup_cfg_pre_ipa,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func,					/* todo_flags_finish */
  0					/* letter */
};


/* Pass: cleanup the CFG just before expanding trees to RTL.
   This is just a round of label cleanups and case node grouping
   because after the tree optimizers have run such cleanups may
   be necessary.  */

static unsigned int
execute_cleanup_cfg_post_optimizing (void)
{
  fold_cond_expr_cond ();
  cleanup_tree_cfg ();
  cleanup_dead_labels ();
  group_case_labels ();
  return 0;
}

struct tree_opt_pass pass_cleanup_cfg_post_optimizing =
{
  "final_cleanup",			/* name */
  NULL,					/* gate */
  execute_cleanup_cfg_post_optimizing,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func,					/* todo_flags_finish */
  0					/* letter */
};

/* Pass: do the actions required to finish with tree-ssa optimization
   passes.  */

static unsigned int
execute_free_datastructures (void)
{
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  /* Remove the ssa structures.  */
  if (cfun->gimple_df)
    delete_tree_ssa ();
  return 0;
}

struct tree_opt_pass pass_free_datastructures =
{
  NULL,					/* name */
  NULL,					/* gate */
  execute_free_datastructures,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
  0					/* letter */
};
/* Pass: free cfg annotations.  */

static unsigned int
execute_free_cfg_annotations (void)
{
  /* And get rid of annotations we no longer need.  */
  delete_tree_cfg_annotations ();

  return 0;
}

struct tree_opt_pass pass_free_cfg_annotations =
{
  NULL,					/* name */
  NULL,					/* gate */
  execute_free_cfg_annotations,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
  0					/* letter */
};

/* Pass: fixup_cfg.  IPA passes, compilation of earlier functions or inlining
   might have changed some properties, such as marked functions nothrow.
   Remove redundant edges and basic blocks, and create new ones if necessary.

   This pass can't be executed as stand alone pass from pass manager, because
   in between inlining and this fixup the verify_flow_info would fail.  */

unsigned int
execute_fixup_cfg (void)
{
  basic_block bb;
  block_stmt_iterator bsi;
  int todo = gimple_in_ssa_p (cfun) ? TODO_verify_ssa : 0;

  cfun->after_inlining = true;

  if (cfun->eh)
    FOR_EACH_BB (bb)
      {
	for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	  {
	    tree stmt = bsi_stmt (bsi);
	    tree call = get_call_expr_in (stmt);
	    tree decl = call ? get_callee_fndecl (call) : NULL;

	    if (decl && call_expr_flags (call) & (ECF_CONST | ECF_PURE)
		&& TREE_SIDE_EFFECTS (call))
	      {
		if (gimple_in_ssa_p (cfun))
		  {
		    todo |= TODO_update_ssa | TODO_cleanup_cfg;
	            update_stmt (stmt);
		  }
	        TREE_SIDE_EFFECTS (call) = 0;
	      }
	    if (decl && TREE_NOTHROW (decl))
	      TREE_NOTHROW (call) = 1;
	    if (!tree_could_throw_p (stmt) && lookup_stmt_eh_region (stmt))
	      remove_stmt_from_eh_region (stmt);
	  }
	if (tree_purge_dead_eh_edges (bb))
          todo |= TODO_cleanup_cfg;
      }

  /* Dump a textual representation of the flowgraph.  */
  if (dump_file)
    dump_tree_cfg (dump_file, dump_flags);

  return todo;
}

/* Do the actions required to initialize internal data structures used
   in tree-ssa optimization passes.  */

static unsigned int
execute_init_datastructures (void)
{
  /* Allocate hash tables, arrays and other structures.  */
  init_tree_ssa ();
  return 0;
}

/* Gate: initialize or not the SSA datastructures.  */

static bool
gate_init_datastructures (void)
{
  return (optimize >= 1);
}

struct tree_opt_pass pass_init_datastructures =
{
  NULL,					/* name */
  gate_init_datastructures,		/* gate */
  execute_init_datastructures,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0,					/* todo_flags_finish */
  0					/* letter */
};

void
tree_lowering_passes (tree fn)
{
  tree saved_current_function_decl = current_function_decl;

  current_function_decl = fn;
  push_cfun (DECL_STRUCT_FUNCTION (fn));
  tree_register_cfg_hooks ();
  bitmap_obstack_initialize (NULL);
  execute_pass_list (all_lowering_passes);
  if (optimize && cgraph_global_info_ready)
    execute_pass_list (pass_early_local_passes.sub);
  free_dominance_info (CDI_POST_DOMINATORS);
  free_dominance_info (CDI_DOMINATORS);
  compact_blocks ();
  current_function_decl = saved_current_function_decl;
  bitmap_obstack_release (NULL);
  pop_cfun ();
}

/* For functions-as-trees languages, this performs all optimization and
   compilation for FNDECL.  */

void
tree_rest_of_compilation (tree fndecl)
{
  location_t saved_loc;
  struct cgraph_node *node;

  timevar_push (TV_EXPAND);

  gcc_assert (!flag_unit_at_a_time || cgraph_global_info_ready);

  node = cgraph_node (fndecl);

  /* Initialize the default bitmap obstack.  */
  bitmap_obstack_initialize (NULL);

  /* Initialize the RTL code for the function.  */
  current_function_decl = fndecl;
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (fndecl);
  init_function_start (fndecl);

  /* Even though we're inside a function body, we still don't want to
     call expand_expr to calculate the size of a variable-sized array.
     We haven't necessarily assigned RTL to all variables yet, so it's
     not safe to try to expand expressions involving them.  */
  cfun->x_dont_save_pending_sizes_p = 1;
  
  tree_register_cfg_hooks ();

  bitmap_obstack_initialize (&reg_obstack); /* FIXME, only at RTL generation*/
  /* Perform all tree transforms and optimizations.  */
  execute_pass_list (all_passes);
  
  bitmap_obstack_release (&reg_obstack);

  /* Release the default bitmap obstack.  */
  bitmap_obstack_release (NULL);
  
  DECL_SAVED_TREE (fndecl) = NULL;
  set_cfun (NULL);

  /* If requested, warn about function definitions where the function will
     return a value (usually of some struct or union type) which itself will
     take up a lot of stack space.  */
  if (warn_larger_than && !DECL_EXTERNAL (fndecl) && TREE_TYPE (fndecl))
    {
      tree ret_type = TREE_TYPE (TREE_TYPE (fndecl));

      if (ret_type && TYPE_SIZE_UNIT (ret_type)
	  && TREE_CODE (TYPE_SIZE_UNIT (ret_type)) == INTEGER_CST
	  && 0 < compare_tree_int (TYPE_SIZE_UNIT (ret_type),
				   larger_than_size))
	{
	  unsigned int size_as_int
	    = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (ret_type));

	  if (compare_tree_int (TYPE_SIZE_UNIT (ret_type), size_as_int) == 0)
	    warning (OPT_Wlarger_than_, "size of return value of %q+D is %u bytes",
                     fndecl, size_as_int);
	  else
	    warning (OPT_Wlarger_than_, "size of return value of %q+D is larger than %wd bytes",
                     fndecl, larger_than_size);
	}
    }

  if (!flag_inline_trees)
    {
      DECL_SAVED_TREE (fndecl) = NULL;
      if (DECL_STRUCT_FUNCTION (fndecl) == 0
	  && !cgraph_node (fndecl)->origin)
	{
	  /* Stop pointing to the local nodes about to be freed.
	     But DECL_INITIAL must remain nonzero so we know this
	     was an actual function definition.
	     For a nested function, this is done in c_pop_function_context.
	     If rest_of_compilation set this to 0, leave it 0.  */
	  if (DECL_INITIAL (fndecl) != 0)
	    DECL_INITIAL (fndecl) = error_mark_node;
	}
    }

  input_location = saved_loc;

  ggc_collect ();
  timevar_pop (TV_EXPAND);
}
