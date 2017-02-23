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
#include "graphite.h"

/* Print global statistics to FILE.  */

static void
print_global_statistics (FILE* file)
{
  long n_bbs = 0;
  long n_loops = 0;
  long n_stmts = 0;
  long n_conditions = 0;
  long n_p_bbs = 0;
  long n_p_loops = 0;
  long n_p_stmts = 0;
  long n_p_conditions = 0;

  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator psi;

      n_bbs++;
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
	  n_p_conditions += bb->count;
	}

      for (psi = gsi_start_bb (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  n_stmts++;
	  n_p_stmts += bb->count;
	}
    }

  fprintf (file, "\nGlobal statistics (");
  fprintf (file, "BBS:%ld, ", n_bbs);
  fprintf (file, "LOOPS:%ld, ", n_loops);
  fprintf (file, "CONDITIONS:%ld, ", n_conditions);
  fprintf (file, "STMTS:%ld)\n", n_stmts);
  fprintf (file, "\nGlobal profiling statistics (");
  fprintf (file, "BBS:%ld, ", n_p_bbs);
  fprintf (file, "LOOPS:%ld, ", n_p_loops);
  fprintf (file, "CONDITIONS:%ld, ", n_p_conditions);
  fprintf (file, "STMTS:%ld)\n", n_p_stmts);
}

/* Print statistics for SCOP to FILE.  */

static void
print_graphite_scop_statistics (FILE* file, scop_p scop)
{
  long n_bbs = 0;
  long n_loops = 0;
  long n_stmts = 0;
  long n_conditions = 0;
  long n_p_bbs = 0;
  long n_p_loops = 0;
  long n_p_stmts = 0;
  long n_p_conditions = 0;

  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator psi;
      loop_p loop = bb->loop_father;

      if (!bb_in_sese_p (bb, scop->scop_info->region))
	continue;

      n_bbs++;
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
  fprintf (file, "BBS:%ld, ", n_p_bbs);
  fprintf (file, "LOOPS:%ld, ", n_p_loops);
  fprintf (file, "CONDITIONS:%ld, ", n_p_conditions);
  fprintf (file, "STMTS:%ld)\n", n_p_stmts);
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
graphite_initialize (isl_ctx *ctx)
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

      isl_ctx_free (ctx);
      return false;
    }

  scev_reset ();
  recompute_all_dominators ();
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
  free_dominance_info (CDI_POST_DOMINATORS);
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

  ctx = isl_ctx_alloc ();
  isl_options_set_on_error (ctx, ISL_ON_ERROR_ABORT);
  if (!graphite_initialize (ctx))
    return;

  the_isl_ctx = ctx;
  build_scops (&scops);

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

	need_cfg_cleanup_p = true;
	/* When code generation is not successful, do not continue
	   generating code for the next scops: the IR has to be cleaned up
	   and could be in an inconsistent state.  */
	if (!graphite_regenerate_ast_isl (scop))
	  break;

	location_t loc = find_loop_location
			   (scop->scop_info->region.entry->dest->loop_father);
	dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
			 "loop nest optimized\n");
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


