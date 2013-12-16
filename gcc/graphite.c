/* Gimple Represented as Polyhedra.
   Copyright (C) 2006-2013 Free Software Foundation, Inc.
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
   the related work.

   One important document to read is CLooG's internal manual:
   http://repo.or.cz/w/cloog-ppl.git?a=blob_plain;f=doc/cloog.texi;hb=HEAD
   that describes the data structure of loops used in this file, and
   the functions that are used for transforming the code.  */

#include "config.h"

#ifdef HAVE_cloog
#include <isl/set.h>
#include <isl/map.h>
#include <isl/options.h>
#include <isl/union_map.h>
#include <cloog/cloog.h>
#include <cloog/isl/domain.h>
#include <cloog/isl/cloog.h>
#endif

#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "sese.h"
#include "dbgcnt.h"
#include "tree-parloops.h"
#include "tree-pass.h"
#include "tree-cfgcleanup.h"

#ifdef HAVE_cloog

#include "graphite-poly.h"
#include "graphite-scop-detection.h"
#include "graphite-clast-to-gimple.h"
#include "graphite-sese-to-poly.h"
#include "graphite-htab.h"

CloogState *cloog_state;

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

      if (!bb_in_sese_p (bb, SCOP_REGION (scop)))
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

      if (loop->header == bb && loop_in_sese_p (loop, SCOP_REGION (scop)))
	{
	  n_loops++;
	  n_p_loops += bb->count;
	}
    }

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
}

/* Initialize graphite: when there are no loops returns false.  */

static bool
graphite_initialize (isl_ctx *ctx)
{
  if (number_of_loops (cfun) <= 1
      /* FIXME: This limit on the number of basic blocks of a function
	 should be removed when the SCOP detection is faster.  */
      || (n_basic_blocks_for_fn (cfun) >
	  PARAM_VALUE (PARAM_GRAPHITE_MAX_BBS_PER_FUNCTION)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	print_global_statistics (dump_file);

      isl_ctx_free (ctx);
      return false;
    }

  scev_reset ();
  recompute_all_dominators ();
  initialize_original_copy_tables ();

  cloog_state = cloog_isl_state_malloc (ctx);

  if (dump_file && dump_flags)
    dump_function_to_file (current_function_decl, dump_file, dump_flags);

  return true;
}

/* Finalize graphite: perform CFG cleanup when NEED_CFG_CLEANUP_P is
   true.  */

static void
graphite_finalize (bool need_cfg_cleanup_p)
{
  if (need_cfg_cleanup_p)
    {
      scev_reset ();
      cleanup_tree_cfg ();
      profile_status_for_fn (cfun) = PROFILE_ABSENT;
      release_recorded_exits ();
      tree_estimate_probability ();
    }

  cloog_state_free (cloog_state);
  free_original_copy_tables ();

  if (dump_file && dump_flags)
    print_loops (dump_file, 3);
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
  bb_pbb_htab_type bb_pbb_mapping;
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

  bb_pbb_mapping.create (10);

  FOR_EACH_VEC_ELT (scops, i, scop)
    if (dbg_cnt (graphite_scop))
      {
	scop->ctx = ctx;
	build_poly_scop (scop);

	if (POLY_SCOP_P (scop)
	    && apply_poly_transforms (scop)
	    && gloog (scop, bb_pbb_mapping))
	  need_cfg_cleanup_p = true;
      }

  bb_pbb_mapping.dispose ();
  free_scops (scops);
  graphite_finalize (need_cfg_cleanup_p);
  the_isl_ctx = NULL;
  isl_ctx_free (ctx);
}

#else /* If Cloog is not available: #ifndef HAVE_cloog.  */

static void
graphite_transform_loops (void)
{
  sorry ("Graphite loop optimizations cannot be used");
}

#endif


static unsigned int
graphite_transforms (void)
{
  if (!current_loops)
    return 0;

  graphite_transform_loops ();

  return 0;
}

static bool
gate_graphite_transforms (void)
{
  /* Enable -fgraphite pass if any one of the graphite optimization flags
     is turned on.  */
  if (flag_loop_block
      || flag_loop_interchange
      || flag_loop_strip_mine
      || flag_graphite_identity
      || flag_loop_parallelize_all
      || flag_loop_optimize_isl)
    flag_graphite = 1;

  return flag_graphite != 0;
}

namespace {

const pass_data pass_data_graphite =
{
  GIMPLE_PASS, /* type */
  "graphite0", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  false, /* has_execute */
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
  bool gate () { return gate_graphite_transforms (); }

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
  true, /* has_gate */
  true, /* has_execute */
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
  bool gate () { return gate_graphite_transforms (); }
  unsigned int execute () { return graphite_transforms (); }

}; // class pass_graphite_transforms

} // anon namespace

gimple_opt_pass *
make_pass_graphite_transforms (gcc::context *ctxt)
{
  return new pass_graphite_transforms (ctxt);
}


