/* Gimple Represented as Polyhedra.
   Copyright (C) 2006-2017 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@inria.fr>.

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

/* This pass converts GIMPLE to GRAPHITE, performs some loop
   transformations and then converts the resulting representation back
   to GIMPLE.

   An early description of this pass can be found in the GCC Summit'06
   paper "GRAPHITE: Polyhedral Analyses and Optimizations for GCC".
   The wiki page http://gcc.gnu.org/wiki/Graphite contains pointers to
   the related work.  */

#define USES_ISL

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "diagnostic-core.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "params.h"
#include "pretty-print.h"

#ifdef HAVE_isl
#include "cfghooks.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "dbgcnt.h"
#include "tree-parloops.h"
#include "tree-cfgcleanup.h"
#include "tree-vectorizer.h"
#include "tree-ssa-loop-manip.h"
#include "graphite.h"

/* Print global statistics to FILE.  */

static void
print_global_statistics (FILE* file)
{
  long n_bbs = 0;
  long n_loops = 0;
  long n_stmts = 0;
  long n_conditions = 0;
  profile_count n_p_bbs = profile_count::zero ();
  profile_count n_p_loops = profile_count::zero ();
  profile_count n_p_stmts = profile_count::zero ();
  profile_count n_p_conditions = profile_count::zero ();

  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator psi;

      n_bbs++;
      if (bb->count.initialized_p ())
        n_p_bbs += bb->count;

      /* Ignore artificial surrounding loop.  */
      if (bb == bb->loop_father->header
	  && bb->index != 0)
	{
	  n_loops++;
	  n_p_loops += bb->count;
	}

      if (EDGE_COUNT (bb->succs) > 1)
	{
	  n_conditions++;
	  if (bb->count.initialized_p ())
	    n_p_conditions += bb->count;
	}

      for (psi = gsi_start_bb (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  n_stmts++;
	  if (bb->count.initialized_p ())
	    n_p_stmts += bb->count;
	}
    }

  fprintf (file, "\nGlobal statistics (");
  fprintf (file, "BBS:%ld, ", n_bbs);
  fprintf (file, "LOOPS:%ld, ", n_loops);
  fprintf (file, "CONDITIONS:%ld, ", n_conditions);
  fprintf (file, "STMTS:%ld)\n", n_stmts);
  fprintf (file, "\nGlobal profiling statistics (");
  fprintf (file, "BBS:");
  n_p_bbs.dump (file);
  fprintf (file, ", LOOPS:");
  n_p_loops.dump (file);
  fprintf (file, ", CONDITIONS:");
  n_p_conditions.dump (file);
  fprintf (file, ", STMTS:");
  n_p_stmts.dump (file);
  fprintf (file, ")\n");
}

/* Print statistics for SCOP to FILE.  */

static void
print_graphite_scop_statistics (FILE* file, scop_p scop)
{
  long n_bbs = 0;
  long n_loops = 0;
  long n_stmts = 0;
  long n_conditions = 0;
  profile_count n_p_bbs = profile_count::zero ();
  profile_count n_p_loops = profile_count::zero ();
  profile_count n_p_stmts = profile_count::zero ();
  profile_count n_p_conditions = profile_count::zero ();

  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator psi;
      loop_p loop = bb->loop_father;

      if (!bb_in_sese_p (bb, scop->scop_info->region))
	continue;

      n_bbs++;
      if (bb->count.initialized_p ())
        n_p_bbs += bb->count;

      if (EDGE_COUNT (bb->succs) > 1)
	{
	  n_conditions++;
	  n_p_conditions += bb->count;
	}

      for (psi = gsi_start_bb (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  n_stmts++;
	  n_p_stmts += bb->count;
	}

      if (loop->header == bb && loop_in_sese_p (loop, scop->scop_info->region))
	{
	  n_loops++;
	  n_p_loops += bb->count;
	}
    }

  fprintf (file, "\nFunction Name: %s\n", current_function_name ());

  edge scop_begin = scop->scop_info->region.entry;
  edge scop_end = scop->scop_info->region.exit;

  fprintf (file, "\nSCoP (entry_edge (bb_%d, bb_%d), ",
	   scop_begin->src->index, scop_begin->dest->index);
  fprintf (file, "exit_edge (bb_%d, bb_%d))",
	   scop_end->src->index, scop_end->dest->index);

  fprintf (file, "\nSCoP statistics (");
  fprintf (file, "BBS:%ld, ", n_bbs);
  fprintf (file, "LOOPS:%ld, ", n_loops);
  fprintf (file, "CONDITIONS:%ld, ", n_conditions);
  fprintf (file, "STMTS:%ld)\n", n_stmts);
  fprintf (file, "\nSCoP profiling statistics (");
  fprintf (file, "BBS:");
  n_p_bbs.dump (file);
  fprintf (file, ", LOOPS:");
  n_p_loops.dump (file);
  fprintf (file, ", CONDITIONS:");
  n_p_conditions.dump (file);
  fprintf (file, ", STMTS:");
  n_p_stmts.dump (file);
  fprintf (file, ")\n");
}

/* Print statistics for SCOPS to FILE.  */

static void
print_graphite_statistics (FILE* file, vec<scop_p> scops)
{
  int i;

  scop_p scop;

  FOR_EACH_VEC_ELT (scops, i, scop)
    print_graphite_scop_statistics (file, scop);

  /* Print the loop structure.  */
  print_loops (file, 2);
  print_loops (file, 3);
}

/* Initialize graphite: when there are no loops returns false.  */

static bool
graphite_initialize (void)
{
  int min_loops = PARAM_VALUE (PARAM_GRAPHITE_MIN_LOOPS_PER_FUNCTION);
  int max_bbs = PARAM_VALUE (PARAM_GRAPHITE_MAX_BBS_PER_FUNCTION);
  int nbbs = n_basic_blocks_for_fn (cfun);
  int nloops = number_of_loops (cfun);

  if (nloops <= min_loops
      /* FIXME: This limit on the number of basic blocks of a function
	 should be removed when the SCOP detection is faster.  */
      || (nbbs > max_bbs))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (nloops <= min_loops)
	    fprintf (dump_file, "\nFunction does not have enough loops: "
		     "PARAM_GRAPHITE_MIN_LOOPS_PER_FUNCTION = %d.\n",
		     min_loops);

	  else if (nbbs > max_bbs)
	    fprintf (dump_file, "\nFunction has too many basic blocks: "
		     "PARAM_GRAPHITE_MAX_BBS_PER_FUNCTION = %d.\n", max_bbs);

	  fprintf (dump_file, "\nnumber of SCoPs: 0\n");
	  print_global_statistics (dump_file);
	}

      return false;
    }

  calculate_dominance_info (CDI_DOMINATORS);
  initialize_original_copy_tables ();

  if (dump_file && dump_flags)
    {
      dump_function_to_file (current_function_decl, dump_file, dump_flags);
      print_loops (dump_file, 3);
    }

  return true;
}

/* Finalize graphite: perform CFG cleanup when NEED_CFG_CLEANUP_P is
   true.  */

static void
graphite_finalize (bool need_cfg_cleanup_p)
{
  if (need_cfg_cleanup_p)
    {
      free_dominance_info (CDI_DOMINATORS);
      scev_reset ();
      cleanup_tree_cfg ();
      profile_status_for_fn (cfun) = PROFILE_ABSENT;
      release_recorded_exits (cfun);
      tree_estimate_probability (false);
    }

  free_original_copy_tables ();

  if (dump_file && dump_flags)
    print_loops (dump_file, 3);
}

/* Deletes all scops in SCOPS.  */

static void
free_scops (vec<scop_p> scops)
{
  int i;
  scop_p scop;

  FOR_EACH_VEC_ELT (scops, i, scop)
    free_scop (scop);

  scops.release ();
}

/* Transforms LOOP to the canonical loop closed SSA form.  */

static void
canonicalize_loop_closed_ssa (loop_p loop)
{
  edge e = single_exit (loop);
  basic_block bb;
  gphi_iterator psi;

  if (!e || (e->flags & EDGE_COMPLEX))
    return;

  bb = e->dest;

  /* Make the loop-close PHI node BB contain only PHIs and have a
     single predecessor.  */
  if (single_pred_p (bb))
    {
      e = split_block_after_labels (bb);
      bb = e->src;
    }
  else
    {
      basic_block close = split_edge (e);
      e = single_succ_edge (close);
      for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  gphi *phi = psi.phi ();
	  use_operand_p use_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  tree arg = USE_FROM_PTR (use_p);

	  /* Only add close phi nodes for SSA_NAMEs defined in LOOP.  */
	  if (TREE_CODE (arg) != SSA_NAME
	      || loop_containing_stmt (SSA_NAME_DEF_STMT (arg)) != loop)
	    continue;

	  tree res = copy_ssa_name (arg);
	  gphi *close_phi = create_phi_node (res, close);
	  add_phi_arg (close_phi, arg, gimple_phi_arg_edge (close_phi, 0),
		       UNKNOWN_LOCATION);
	  SET_USE (use_p, res);
	}
      bb = close;
    }

  /* Eliminate duplicates.  This relies on processing loops from
     innermost to outer.  */
  for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
    {
      gphi_iterator gsi = psi;
      gphi *phi = psi.phi ();

      /* At this point, PHI should be a close phi in normal form.  */
      gcc_assert (gimple_phi_num_args (phi) == 1);

      /* Iterate over the next phis and remove duplicates.  */
      gsi_next (&gsi);
      while (!gsi_end_p (gsi))
	if (gimple_phi_arg_def (phi, 0) == gimple_phi_arg_def (gsi.phi (), 0))
	  {
	    replace_uses_by (gimple_phi_result (gsi.phi ()),
			     gimple_phi_result (phi));
	    remove_phi_node (&gsi, true);
	  }
	else
	  gsi_next (&gsi);
    }
}

/* Converts the current loop closed SSA form to a canonical form
   expected by the Graphite code generation.

   The loop closed SSA form has the following invariant: a variable
   defined in a loop that is used outside the loop appears only in the
   phi nodes in the destination of the loop exit.  These phi nodes are
   called close phi nodes.

   The canonical loop closed SSA form contains the extra invariants:

   - when the loop contains only one exit, the close phi nodes contain
   only one argument.  That implies that the basic block that contains
   the close phi nodes has only one predecessor, that is a basic block
   in the loop.

   - the basic block containing the close phi nodes does not contain
   other statements.

   - there exist only one phi node per definition in the loop.
*/

static void
canonicalize_loop_closed_ssa_form (void)
{
  loop_p loop;
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    canonicalize_loop_closed_ssa (loop);

  checking_verify_loop_closed_ssa (true);
}

isl_ctx *the_isl_ctx;

/* Perform a set of linear transforms on the loops of the current
   function.  */

void
graphite_transform_loops (void)
{
  int i;
  scop_p scop;
  bool need_cfg_cleanup_p = false;
  vec<scop_p> scops = vNULL;
  isl_ctx *ctx;

  /* If a function is parallel it was most probably already run through graphite
     once. No need to run again.  */
  if (parallelized_function_p (cfun->decl))
    return;

  if (!graphite_initialize ())
    return;

  ctx = isl_ctx_alloc ();
  isl_options_set_on_error (ctx, ISL_ON_ERROR_ABORT);
  the_isl_ctx = ctx;

  sort_sibling_loops (cfun);
  canonicalize_loop_closed_ssa_form ();

  calculate_dominance_info (CDI_POST_DOMINATORS);
  build_scops (&scops);
  free_dominance_info (CDI_POST_DOMINATORS);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_graphite_statistics (dump_file, scops);
      print_global_statistics (dump_file);
    }

  FOR_EACH_VEC_ELT (scops, i, scop)
    if (dbg_cnt (graphite_scop))
      {
	scop->isl_context = ctx;
	if (!build_poly_scop (scop))
	  continue;

	if (!apply_poly_transforms (scop))
	  continue;

	location_t loc = find_loop_location
	  (scops[i]->scop_info->region.entry->dest->loop_father);

	need_cfg_cleanup_p = true;
	if (!graphite_regenerate_ast_isl (scop))
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			   "loop nest not optimized, code generation error\n");
	else
	  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
			   "loop nest optimized\n");
      }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      loop_p loop;
      int num_no_dependency = 0;

      FOR_EACH_LOOP (loop, 0)
	if (loop->can_be_parallel)
	  num_no_dependency++;

      fprintf (dump_file, "%d loops carried no dependency.\n",
	       num_no_dependency);
    }

  free_scops (scops);
  graphite_finalize (need_cfg_cleanup_p);
  the_isl_ctx = NULL;
  isl_ctx_free (ctx);
}

#else /* If isl is not available: #ifndef HAVE_isl.  */

static void
graphite_transform_loops (void)
{
  sorry ("Graphite loop optimizations cannot be used (isl is not available).");
}

#endif


static unsigned int
graphite_transforms (struct function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  graphite_transform_loops ();

  return 0;
}

static bool
gate_graphite_transforms (void)
{
  /* Enable -fgraphite pass if any one of the graphite optimization flags
     is turned on.  */
  if (flag_graphite_identity
      || flag_loop_parallelize_all
      || flag_loop_nest_optimize)
    flag_graphite = 1;

  return flag_graphite != 0;
}

namespace {

const pass_data pass_data_graphite =
{
  GIMPLE_PASS, /* type */
  "graphite0", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_GRAPHITE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_graphite : public gimple_opt_pass
{
public:
  pass_graphite (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_graphite, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return gate_graphite_transforms (); }

}; // class pass_graphite

} // anon namespace

gimple_opt_pass *
make_pass_graphite (gcc::context *ctxt)
{
  return new pass_graphite (ctxt);
}

namespace {

const pass_data pass_data_graphite_transforms =
{
  GIMPLE_PASS, /* type */
  "graphite", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_GRAPHITE_TRANSFORMS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_graphite_transforms : public gimple_opt_pass
{
public:
  pass_graphite_transforms (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_graphite_transforms, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return gate_graphite_transforms (); }
  virtual unsigned int execute (function *fun) { return graphite_transforms (fun); }

}; // class pass_graphite_transforms

} // anon namespace

gimple_opt_pass *
make_pass_graphite_transforms (gcc::context *ctxt)
{
  return new pass_graphite_transforms (ctxt);
}


