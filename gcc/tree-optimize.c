/* Top-level control of tree optimizations.
   Copyright 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
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
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "flags.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "function.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "flags.h"
#include "cgraph.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "ggc.h"
#include "cgraph.h"
#include "graph.h"
#include "cfgloop.h"
#include "except.h"
#include "plugin.h"
#include "regset.h"	/* FIXME: For reg_obstack.  */

/* Gate: execute, or not, all of the non-trivial optimizations.  */

static bool
gate_all_optimizations (void)
{
  return (optimize >= 1
	  /* Don't bother doing anything if the program has errors.
	     We have to pass down the queue if we already went into SSA */
	  && (!seen_error () || gimple_in_ssa_p (cfun)));
}

struct gimple_opt_pass pass_all_optimizations =
{
 {
  GIMPLE_PASS,
  "*all_optimizations",			/* name */
  gate_all_optimizations,		/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_OPTIMIZE,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};

/* Gate: execute, or not, all of the non-trivial optimizations.  */

static bool
gate_all_early_local_passes (void)
{
	  /* Don't bother doing anything if the program has errors.  */
  return (!seen_error () && !in_lto_p);
}

static unsigned int
execute_all_early_local_passes (void)
{
  /* Once this pass (and its sub-passes) are complete, all functions
     will be in SSA form.  Technically this state change is happening
     a tad early, since the sub-passes have not yet run, but since
     none of the sub-passes are IPA passes and do not create new
     functions, this is ok.  We're setting this value for the benefit
     of IPA passes that follow.  */
  if (cgraph_state < CGRAPH_STATE_IPA_SSA)
    cgraph_state = CGRAPH_STATE_IPA_SSA;
  return 0;
}

struct simple_ipa_opt_pass pass_early_local_passes =
{
 {
  SIMPLE_IPA_PASS,
  "early_local_cleanups",		/* name */
  gate_all_early_local_passes,		/* gate */
  execute_all_early_local_passes,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_EARLY_LOCAL,			/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_remove_functions	 		/* todo_flags_finish */
 }
};

/* Gate: execute, or not, all of the non-trivial optimizations.  */

static bool
gate_all_early_optimizations (void)
{
  return (optimize >= 1
	  /* Don't bother doing anything if the program has errors.  */
	  && !seen_error ());
}

struct gimple_opt_pass pass_all_early_optimizations =
{
 {
  GIMPLE_PASS,
  "early_optimizations",		/* name */
  gate_all_early_optimizations,		/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};


/* Pass: cleanup the CFG just before expanding trees to RTL.
   This is just a round of label cleanups and case node grouping
   because after the tree optimizers have run such cleanups may
   be necessary.  */

static unsigned int
execute_cleanup_cfg_post_optimizing (void)
{
  cleanup_tree_cfg ();
  maybe_remove_unreachable_handlers ();
  cleanup_dead_labels ();
  group_case_labels ();
  if ((flag_compare_debug_opt || flag_compare_debug)
      && flag_dump_final_insns)
    {
      FILE *final_output = fopen (flag_dump_final_insns, "a");

      if (!final_output)
	{
	  error ("could not open final insn dump file %qs: %m",
		 flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
      else
	{
	  int save_unnumbered = flag_dump_unnumbered;
	  int save_noaddr = flag_dump_noaddr;

	  flag_dump_noaddr = flag_dump_unnumbered = 1;
	  fprintf (final_output, "\n");
	  dump_enumerated_decls (final_output, dump_flags | TDF_NOUID);
	  flag_dump_noaddr = save_noaddr;
	  flag_dump_unnumbered = save_unnumbered;
	  if (fclose (final_output))
	    {
	      error ("could not close final insn dump file %qs: %m",
		     flag_dump_final_insns);
	      flag_dump_final_insns = NULL;
	    }
	}
    }
  return 0;
}

struct gimple_opt_pass pass_cleanup_cfg_post_optimizing =
{
 {
  GIMPLE_PASS,
  "optimized",				/* name */
  NULL,					/* gate */
  execute_cleanup_cfg_post_optimizing,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_CLEANUP_CFG,			/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_remove_unused_locals             /* todo_flags_finish */
 }
};

/* Pass: do the actions required to finish with tree-ssa optimization
   passes.  */

unsigned int
execute_free_datastructures (void)
{
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  /* And get rid of annotations we no longer need.  */
  delete_tree_cfg_annotations ();

  return 0;
}

/* IPA passes, compilation of earlier functions or inlining
   might have changed some properties, such as marked functions nothrow,
   pure, const or noreturn.
   Remove redundant edges and basic blocks, and create new ones if necessary.

   This pass can't be executed as stand alone pass from pass manager, because
   in between inlining and this fixup the verify_flow_info would fail.  */

unsigned int
execute_fixup_cfg (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  int todo = gimple_in_ssa_p (cfun) ? TODO_verify_ssa : 0;
  gcov_type count_scale;
  edge e;
  edge_iterator ei;

  if (ENTRY_BLOCK_PTR->count)
    count_scale = ((cgraph_get_node (current_function_decl)->count
		    * REG_BR_PROB_BASE + ENTRY_BLOCK_PTR->count / 2)
		   / ENTRY_BLOCK_PTR->count);
  else
    count_scale = REG_BR_PROB_BASE;

  ENTRY_BLOCK_PTR->count = cgraph_get_node (current_function_decl)->count;
  EXIT_BLOCK_PTR->count = (EXIT_BLOCK_PTR->count * count_scale
  			   + REG_BR_PROB_BASE / 2) / REG_BR_PROB_BASE;

  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
    e->count = (e->count * count_scale
       + REG_BR_PROB_BASE / 2) / REG_BR_PROB_BASE;

  FOR_EACH_BB (bb)
    {
      bb->count = (bb->count * count_scale
		   + REG_BR_PROB_BASE / 2) / REG_BR_PROB_BASE;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree decl = is_gimple_call (stmt)
		      ? gimple_call_fndecl (stmt)
		      : NULL;
	  if (decl)
	    {
	      int flags = gimple_call_flags (stmt);
	      if (flags & (ECF_CONST | ECF_PURE | ECF_LOOPING_CONST_OR_PURE))
		{
		  if (gimple_purge_dead_abnormal_call_edges (bb))
		    todo |= TODO_cleanup_cfg;

		  if (gimple_in_ssa_p (cfun))
		    {
		      todo |= TODO_update_ssa | TODO_cleanup_cfg;
		      update_stmt (stmt);
		    }
		}

	      if (flags & ECF_NORETURN
		  && fixup_noreturn_call (stmt))
		todo |= TODO_cleanup_cfg;
	     }

	  if (maybe_clean_eh_stmt (stmt)
	      && gimple_purge_dead_eh_edges (bb))
	    todo |= TODO_cleanup_cfg;
	}

      FOR_EACH_EDGE (e, ei, bb->succs)
        e->count = (e->count * count_scale
		    + REG_BR_PROB_BASE / 2) / REG_BR_PROB_BASE;
    }
  if (count_scale != REG_BR_PROB_BASE)
    compute_function_frequency ();

  /* We just processed all calls.  */
  if (cfun->gimple_df)
    {
      VEC_free (gimple, gc, MODIFIED_NORETURN_CALLS (cfun));
      MODIFIED_NORETURN_CALLS (cfun) = NULL;
    }

  /* Dump a textual representation of the flowgraph.  */
  if (dump_file)
    gimple_dump_cfg (dump_file, dump_flags);

  return todo;
}

struct gimple_opt_pass pass_fixup_cfg =
{
 {
  GIMPLE_PASS,
  "*free_cfg_annotations",		/* name */
  NULL,					/* gate */
  execute_fixup_cfg,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};

/* Do the actions required to initialize internal data structures used
   in tree-ssa optimization passes.  */

static unsigned int
execute_init_datastructures (void)
{
  /* Allocate hash tables, arrays and other structures.  */
  init_tree_ssa (cfun);
  return 0;
}

struct gimple_opt_pass pass_init_datastructures =
{
 {
  GIMPLE_PASS,
  "*init_datastructures",		/* name */
  NULL,					/* gate */
  execute_init_datastructures,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};

void
tree_lowering_passes (tree fn)
{
  tree saved_current_function_decl = current_function_decl;

  current_function_decl = fn;
  push_cfun (DECL_STRUCT_FUNCTION (fn));
  gimple_register_cfg_hooks ();
  bitmap_obstack_initialize (NULL);
  execute_pass_list (all_lowering_passes);
  if (optimize && cgraph_global_info_ready)
    execute_pass_list (pass_early_local_passes.pass.sub);
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

  timevar_push (TV_REST_OF_COMPILATION);

  gcc_assert (cgraph_global_info_ready);

  /* Initialize the default bitmap obstack.  */
  bitmap_obstack_initialize (NULL);

  /* Initialize the RTL code for the function.  */
  current_function_decl = fndecl;
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (fndecl);
  init_function_start (fndecl);

  gimple_register_cfg_hooks ();

  bitmap_obstack_initialize (&reg_obstack); /* FIXME, only at RTL generation*/

  execute_all_ipa_transforms ();

  /* Perform all tree transforms and optimizations.  */

  /* Signal the start of passes.  */
  invoke_plugin_callbacks (PLUGIN_ALL_PASSES_START, NULL);

  execute_pass_list (all_passes);

  /* Signal the end of passes.  */
  invoke_plugin_callbacks (PLUGIN_ALL_PASSES_END, NULL);

  bitmap_obstack_release (&reg_obstack);

  /* Release the default bitmap obstack.  */
  bitmap_obstack_release (NULL);

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

  gimple_set_body (fndecl, NULL);
  if (DECL_STRUCT_FUNCTION (fndecl) == 0
      && !cgraph_get_node (fndecl)->origin)
    {
      /* Stop pointing to the local nodes about to be freed.
	 But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.
	 For a nested function, this is done in c_pop_function_context.
	 If rest_of_compilation set this to 0, leave it 0.  */
      if (DECL_INITIAL (fndecl) != 0)
	DECL_INITIAL (fndecl) = error_mark_node;
    }

  input_location = saved_loc;

  ggc_collect ();
  timevar_pop (TV_REST_OF_COMPILATION);
}
