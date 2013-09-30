/* Top-level control of tree optimizations.
   Copyright (C) 2001-2013 Free Software Foundation, Inc.
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
#include "flags.h"
#include "tree-ssa.h"
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
#include "cfgloop.h"
#include "except.h"
#include "plugin.h"


/* Pass: cleanup the CFG just before expanding trees to RTL.
   This is just a round of label cleanups and case node grouping
   because after the tree optimizers have run such cleanups may
   be necessary.  */

static unsigned int
execute_cleanup_cfg_post_optimizing (void)
{
  unsigned int todo = 0;
  if (cleanup_tree_cfg ())
    todo |= TODO_update_ssa;
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
  return todo;
}

namespace {

const pass_data pass_data_cleanup_cfg_post_optimizing =
{
  GIMPLE_PASS, /* type */
  "optimized", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_TREE_CLEANUP_CFG, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_remove_unused_locals, /* todo_flags_finish */
};

class pass_cleanup_cfg_post_optimizing : public gimple_opt_pass
{
public:
  pass_cleanup_cfg_post_optimizing (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_cleanup_cfg_post_optimizing, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () {
    return execute_cleanup_cfg_post_optimizing ();
  }

}; // class pass_cleanup_cfg_post_optimizing

} // anon namespace

gimple_opt_pass *
make_pass_cleanup_cfg_post_optimizing (gcc::context *ctxt)
{
  return new pass_cleanup_cfg_post_optimizing (ctxt);
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

  count_scale
      = GCOV_COMPUTE_SCALE (cgraph_get_node (current_function_decl)->count,
                            ENTRY_BLOCK_PTR->count);

  ENTRY_BLOCK_PTR->count = cgraph_get_node (current_function_decl)->count;
  EXIT_BLOCK_PTR->count = apply_scale (EXIT_BLOCK_PTR->count,
                                       count_scale);

  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
    e->count = apply_scale (e->count, count_scale);

  FOR_EACH_BB (bb)
    {
      bb->count = apply_scale (bb->count, count_scale);
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
        e->count = apply_scale (e->count, count_scale);

      /* If we have a basic block with no successors that does not
	 end with a control statement or a noreturn call end it with
	 a call to __builtin_unreachable.  This situation can occur
	 when inlining a noreturn call that does in fact return.  */
      if (EDGE_COUNT (bb->succs) == 0)
	{
	  gimple stmt = last_stmt (bb);
	  if (!stmt
	      || (!is_ctrl_stmt (stmt)
		  && (!is_gimple_call (stmt)
		      || (gimple_call_flags (stmt) & ECF_NORETURN) == 0)))
	    {
	      stmt = gimple_build_call
		  (builtin_decl_implicit (BUILT_IN_UNREACHABLE), 0);
	      gimple_stmt_iterator gsi = gsi_last_bb (bb);
	      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
	    }
	}
    }
  if (count_scale != REG_BR_PROB_BASE)
    compute_function_frequency ();

  /* We just processed all calls.  */
  if (cfun->gimple_df)
    vec_free (MODIFIED_NORETURN_CALLS (cfun));

  /* Dump a textual representation of the flowgraph.  */
  if (dump_file)
    gimple_dump_cfg (dump_file, dump_flags);

  if (current_loops
      && (todo & TODO_cleanup_cfg))
    loops_state_set (LOOPS_NEED_FIXUP);

  return todo;
}

namespace {

const pass_data pass_data_fixup_cfg =
{
  GIMPLE_PASS, /* type */
  "*free_cfg_annotations", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_fixup_cfg : public gimple_opt_pass
{
public:
  pass_fixup_cfg (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_fixup_cfg, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_fixup_cfg (ctxt_); }
  unsigned int execute () { return execute_fixup_cfg (); }

}; // class pass_fixup_cfg

} // anon namespace

gimple_opt_pass *
make_pass_fixup_cfg (gcc::context *ctxt)
{
  return new pass_fixup_cfg (ctxt);
}
