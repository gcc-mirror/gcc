/* Loop optimizations over tree-ssa.
   Copyright (C) 2003, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

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
#include "tree-flow.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "flags.h"
#include "tree-inline.h"
#include "tree-scalar-evolution.h"
#include "diagnostic-core.h"
#include "tree-vectorizer.h"

/* The loop superpass.  */

static bool
gate_tree_loop (void)
{
  return flag_tree_loop_optimize != 0;
}

struct gimple_opt_pass pass_tree_loop =
{
 {
  GIMPLE_PASS,
  "loop",				/* name */
  gate_tree_loop,			/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_LOOP,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  TODO_ggc_collect,			/* todo_flags_start */
  TODO_verify_ssa | TODO_ggc_collect	/* todo_flags_finish */
 }
};

/* Loop optimizer initialization.  */

static unsigned int
tree_ssa_loop_init (void)
{
  loop_optimizer_init (LOOPS_NORMAL
		       | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);

  if (number_of_loops () <= 1)
    return 0;

  scev_initialize ();
  return 0;
}

struct gimple_opt_pass pass_tree_loop_init =
{
 {
  GIMPLE_PASS,
  "loopinit",				/* name */
  NULL,					/* gate */
  tree_ssa_loop_init,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_LOOP_INIT,			/* tv_id */
  PROP_cfg,				/* properties_required */
  PROP_loops,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0             			/* todo_flags_finish */
 }
};

/* Loop invariant motion pass.  */

static unsigned int
tree_ssa_loop_im (void)
{
  if (number_of_loops () <= 1)
    return 0;

  return tree_ssa_lim ();
}

static bool
gate_tree_ssa_loop_im (void)
{
  return flag_tree_loop_im != 0;
}

struct gimple_opt_pass pass_lim =
{
 {
  GIMPLE_PASS,
  "lim",				/* name */
  gate_tree_ssa_loop_im,		/* gate */
  tree_ssa_loop_im,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_LIM,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0             			/* todo_flags_finish */
 }
};

/* Loop unswitching pass.  */

static unsigned int
tree_ssa_loop_unswitch (void)
{
  if (number_of_loops () <= 1)
    return 0;

  return tree_ssa_unswitch_loops ();
}

static bool
gate_tree_ssa_loop_unswitch (void)
{
  return flag_unswitch_loops != 0;
}

struct gimple_opt_pass pass_tree_unswitch =
{
 {
  GIMPLE_PASS,
  "unswitch",				/* name */
  gate_tree_ssa_loop_unswitch,		/* gate */
  tree_ssa_loop_unswitch,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_LOOP_UNSWITCH,		/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_ggc_collect                  	/* todo_flags_finish */
 }
};

/* Predictive commoning.  */

static unsigned
run_tree_predictive_commoning (void)
{
  if (!current_loops)
    return 0;

  return tree_predictive_commoning ();
}

static bool
gate_tree_predictive_commoning (void)
{
  return flag_predictive_commoning != 0;
}

struct gimple_opt_pass pass_predcom =
{
 {
  GIMPLE_PASS,
  "pcom",				/* name */
  gate_tree_predictive_commoning,	/* gate */
  run_tree_predictive_commoning,	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_PREDCOM,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa_only_virtuals 	/* todo_flags_finish */
 }
};

/* Loop autovectorization.  */

static unsigned int
tree_vectorize (void)
{
  if (number_of_loops () <= 1)
    return 0;

  return vectorize_loops ();
}

static bool
gate_tree_vectorize (void)
{
  return flag_tree_vectorize;
}

struct gimple_opt_pass pass_vectorize =
{
 {
  GIMPLE_PASS,
  "vect",                               /* name */
  gate_tree_vectorize,                  /* gate */
  tree_vectorize,                       /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_TREE_VECTORIZATION,                /* tv_id */
  PROP_cfg | PROP_ssa,                  /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa
    | TODO_ggc_collect			/* todo_flags_finish */
 }
};

/* GRAPHITE optimizations.  */

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

struct gimple_opt_pass pass_graphite =
{
 {
  GIMPLE_PASS,
  "graphite0",				/* name */
  gate_graphite_transforms,		/* gate */
  NULL,					/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_GRAPHITE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_graphite_transforms =
{
 {
  GIMPLE_PASS,
  "graphite",				/* name */
  gate_graphite_transforms,		/* gate */
  graphite_transforms,       		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_GRAPHITE_TRANSFORMS,  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0             			/* todo_flags_finish */
 }
};

/* Check the correctness of the data dependence analyzers.  */

static unsigned int
check_data_deps (void)
{
  if (number_of_loops () <= 1)
    return 0;

  tree_check_data_deps ();
  return 0;
}

static bool
gate_check_data_deps (void)
{
  return flag_check_data_deps != 0;
}

struct gimple_opt_pass pass_check_data_deps =
{
 {
  GIMPLE_PASS,
  "ckdd",				/* name */
  gate_check_data_deps,	        	/* gate */
  check_data_deps,       		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_CHECK_DATA_DEPS,  	        	/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                             	/* todo_flags_finish */
 }
};

/* Canonical induction variable creation pass.  */

static unsigned int
tree_ssa_loop_ivcanon (void)
{
  if (number_of_loops () <= 1)
    return 0;

  return canonicalize_induction_variables ();
}

static bool
gate_tree_ssa_loop_ivcanon (void)
{
  return flag_tree_loop_ivcanon != 0;
}

struct gimple_opt_pass pass_iv_canon =
{
 {
  GIMPLE_PASS,
  "ivcanon",				/* name */
  gate_tree_ssa_loop_ivcanon,		/* gate */
  tree_ssa_loop_ivcanon,	       	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_LOOP_IVCANON,	  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0             			/* todo_flags_finish */
 }
};

/* Propagation of constants using scev.  */

static bool
gate_scev_const_prop (void)
{
  return flag_tree_scev_cprop;
}

struct gimple_opt_pass pass_scev_cprop =
{
 {
  GIMPLE_PASS,
  "sccp",				/* name */
  gate_scev_const_prop,			/* gate */
  scev_const_prop,	       		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_SCEV_CONST,	  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_cleanup_cfg
    | TODO_update_ssa_only_virtuals
					/* todo_flags_finish */
 }
};

/* Record bounds on numbers of iterations of loops.  */

static unsigned int
tree_ssa_loop_bounds (void)
{
  if (number_of_loops () <= 1)
    return 0;

  estimate_numbers_of_iterations ();
  scev_reset ();
  return 0;
}

struct gimple_opt_pass pass_record_bounds =
{
 {
  GIMPLE_PASS,
  "*record_bounds",			/* name */
  NULL,					/* gate */
  tree_ssa_loop_bounds,		       	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_LOOP_BOUNDS,	  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0			              	/* todo_flags_finish */
 }
};

/* Complete unrolling of loops.  */

static unsigned int
tree_complete_unroll (void)
{
  if (number_of_loops () <= 1)
    return 0;

  return tree_unroll_loops_completely (flag_unroll_loops
				       || flag_peel_loops
				       || optimize >= 3, true);
}

static bool
gate_tree_complete_unroll (void)
{
  return true;
}

struct gimple_opt_pass pass_complete_unroll =
{
 {
  GIMPLE_PASS,
  "cunroll",				/* name */
  gate_tree_complete_unroll,		/* gate */
  tree_complete_unroll,		       	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_COMPLETE_UNROLL,	  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_ggc_collect			/* todo_flags_finish */
 }
};

/* Complete unrolling of inner loops.  */

static unsigned int
tree_complete_unroll_inner (void)
{
  unsigned ret = 0;

  loop_optimizer_init (LOOPS_NORMAL
		       | LOOPS_HAVE_RECORDED_EXITS);
  if (number_of_loops () > 1)
    {
      scev_initialize ();
      ret = tree_unroll_loops_completely (optimize >= 3, false);
      free_numbers_of_iterations_estimates ();
      scev_finalize ();
    }
  loop_optimizer_finalize ();

  return ret;
}

static bool
gate_tree_complete_unroll_inner (void)
{
  return optimize >= 2;
}

struct gimple_opt_pass pass_complete_unrolli =
{
 {
  GIMPLE_PASS,
  "cunrolli",				/* name */
  gate_tree_complete_unroll_inner,	/* gate */
  tree_complete_unroll_inner,	       	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_COMPLETE_UNROLL,	  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_verify_flow
    | TODO_ggc_collect 			/* todo_flags_finish */
 }
};

/* Parallelization.  */

static bool
gate_tree_parallelize_loops (void)
{
  return flag_tree_parallelize_loops > 1;
}

static unsigned
tree_parallelize_loops (void)
{
  if (number_of_loops () <= 1)
    return 0;

  if (parallelize_loops ())
    return TODO_cleanup_cfg | TODO_rebuild_alias;
  return 0;
}

struct gimple_opt_pass pass_parallelize_loops =
{
 {
  GIMPLE_PASS,
  "parloops",				/* name */
  gate_tree_parallelize_loops,		/* gate */
  tree_parallelize_loops,      		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PARALLELIZE_LOOPS,  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0             			/* todo_flags_finish */
 }
};

/* Prefetching.  */

static unsigned int
tree_ssa_loop_prefetch (void)
{
  if (number_of_loops () <= 1)
    return 0;

  return tree_ssa_prefetch_arrays ();
}

static bool
gate_tree_ssa_loop_prefetch (void)
{
  return flag_prefetch_loop_arrays > 0;
}

struct gimple_opt_pass pass_loop_prefetch =
{
 {
  GIMPLE_PASS,
  "aprefetch",				/* name */
  gate_tree_ssa_loop_prefetch,		/* gate */
  tree_ssa_loop_prefetch,	       	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PREFETCH,	  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0             			/* todo_flags_finish */
 }
};

/* Induction variable optimizations.  */

static unsigned int
tree_ssa_loop_ivopts (void)
{
  if (number_of_loops () <= 1)
    return 0;

  tree_ssa_iv_optimize ();
  return 0;
}

static bool
gate_tree_ssa_loop_ivopts (void)
{
  return flag_ivopts != 0;
}

struct gimple_opt_pass pass_iv_optimize =
{
 {
  GIMPLE_PASS,
  "ivopts",				/* name */
  gate_tree_ssa_loop_ivopts,		/* gate */
  tree_ssa_loop_ivopts,		       	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_LOOP_IVOPTS,	  		/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa | TODO_ggc_collect	/* todo_flags_finish */
 }
};

/* Loop optimizer finalization.  */

static unsigned int
tree_ssa_loop_done (void)
{
  free_numbers_of_iterations_estimates ();
  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

struct gimple_opt_pass pass_tree_loop_done =
{
 {
  GIMPLE_PASS,
  "loopdone",				/* name */
  NULL,					/* gate */
  tree_ssa_loop_done,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_LOOP_FINI,			/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_cleanup_cfg
    | TODO_verify_flow			/* todo_flags_finish */
 }
};
