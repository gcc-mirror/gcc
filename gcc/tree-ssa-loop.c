/* Loop optimizations over tree-ssa.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.

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
#include "tree-ssa.h"
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

namespace {

const pass_data pass_data_tree_loop =
{
  GIMPLE_PASS, /* type */
  "loop", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  false, /* has_execute */
  TV_TREE_LOOP, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_ssa, /* todo_flags_finish */
};

class pass_tree_loop : public gimple_opt_pass
{
public:
  pass_tree_loop(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_tree_loop, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_loop (); }

}; // class pass_tree_loop

} // anon namespace

gimple_opt_pass *
make_pass_tree_loop (gcc::context *ctxt)
{
  return new pass_tree_loop (ctxt);
}

/* Loop optimizer initialization.  */

static unsigned int
tree_ssa_loop_init (void)
{
  loop_optimizer_init (LOOPS_NORMAL
		       | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);

  /* We might discover new loops, e.g. when turning irreducible
     regions into reducible.  */
  scev_initialize ();

  if (number_of_loops (cfun) <= 1)
    return 0;

  return 0;
}

namespace {

const pass_data pass_data_tree_loop_init =
{
  GIMPLE_PASS, /* type */
  "loopinit", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tree_loop_init : public gimple_opt_pass
{
public:
  pass_tree_loop_init(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_tree_loop_init, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () { return tree_ssa_loop_init (); }

}; // class pass_tree_loop_init

} // anon namespace

gimple_opt_pass *
make_pass_tree_loop_init (gcc::context *ctxt)
{
  return new pass_tree_loop_init (ctxt);
}

/* Loop invariant motion pass.  */

static unsigned int
tree_ssa_loop_im (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  return tree_ssa_lim ();
}

static bool
gate_tree_ssa_loop_im (void)
{
  return flag_tree_loop_im != 0;
}

namespace {

const pass_data pass_data_lim =
{
  GIMPLE_PASS, /* type */
  "lim", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_LIM, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lim : public gimple_opt_pass
{
public:
  pass_lim(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_lim, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_lim (ctxt_); }
  bool gate () { return gate_tree_ssa_loop_im (); }
  unsigned int execute () { return tree_ssa_loop_im (); }

}; // class pass_lim

} // anon namespace

gimple_opt_pass *
make_pass_lim (gcc::context *ctxt)
{
  return new pass_lim (ctxt);
}

/* Loop unswitching pass.  */

static unsigned int
tree_ssa_loop_unswitch (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  return tree_ssa_unswitch_loops ();
}

static bool
gate_tree_ssa_loop_unswitch (void)
{
  return flag_unswitch_loops != 0;
}

namespace {

const pass_data pass_data_tree_unswitch =
{
  GIMPLE_PASS, /* type */
  "unswitch", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_LOOP_UNSWITCH, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tree_unswitch : public gimple_opt_pass
{
public:
  pass_tree_unswitch(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_tree_unswitch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_ssa_loop_unswitch (); }
  unsigned int execute () { return tree_ssa_loop_unswitch (); }

}; // class pass_tree_unswitch

} // anon namespace

gimple_opt_pass *
make_pass_tree_unswitch (gcc::context *ctxt)
{
  return new pass_tree_unswitch (ctxt);
}

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

namespace {

const pass_data pass_data_predcom =
{
  GIMPLE_PASS, /* type */
  "pcom", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_PREDCOM, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa_only_virtuals, /* todo_flags_finish */
};

class pass_predcom : public gimple_opt_pass
{
public:
  pass_predcom(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_predcom, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_predictive_commoning (); }
  unsigned int execute () { return run_tree_predictive_commoning (); }

}; // class pass_predcom

} // anon namespace

gimple_opt_pass *
make_pass_predcom (gcc::context *ctxt)
{
  return new pass_predcom (ctxt);
}

/* Loop autovectorization.  */

static unsigned int
tree_vectorize (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  return vectorize_loops ();
}

static bool
gate_tree_vectorize (void)
{
  return flag_tree_vectorize || cfun->has_force_vect_loops;
}

namespace {

const pass_data pass_data_vectorize =
{
  GIMPLE_PASS, /* type */
  "vect", /* name */
  OPTGROUP_LOOP | OPTGROUP_VEC, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_VECTORIZATION, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_vectorize : public gimple_opt_pass
{
public:
  pass_vectorize(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_vectorize, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_vectorize (); }
  unsigned int execute () { return tree_vectorize (); }

}; // class pass_vectorize

} // anon namespace

gimple_opt_pass *
make_pass_vectorize (gcc::context *ctxt)
{
  return new pass_vectorize (ctxt);
}

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
  pass_graphite(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_graphite, ctxt)
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
  pass_graphite_transforms(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_graphite_transforms, ctxt)
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

/* Check the correctness of the data dependence analyzers.  */

static unsigned int
check_data_deps (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  tree_check_data_deps ();
  return 0;
}

static bool
gate_check_data_deps (void)
{
  return flag_check_data_deps != 0;
}

namespace {

const pass_data pass_data_check_data_deps =
{
  GIMPLE_PASS, /* type */
  "ckdd", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_CHECK_DATA_DEPS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_check_data_deps : public gimple_opt_pass
{
public:
  pass_check_data_deps(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_check_data_deps, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_check_data_deps (); }
  unsigned int execute () { return check_data_deps (); }

}; // class pass_check_data_deps

} // anon namespace

gimple_opt_pass *
make_pass_check_data_deps (gcc::context *ctxt)
{
  return new pass_check_data_deps (ctxt);
}

/* Canonical induction variable creation pass.  */

static unsigned int
tree_ssa_loop_ivcanon (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  return canonicalize_induction_variables ();
}

static bool
gate_tree_ssa_loop_ivcanon (void)
{
  return flag_tree_loop_ivcanon != 0;
}

namespace {

const pass_data pass_data_iv_canon =
{
  GIMPLE_PASS, /* type */
  "ivcanon", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_LOOP_IVCANON, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_iv_canon : public gimple_opt_pass
{
public:
  pass_iv_canon(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_iv_canon, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_ssa_loop_ivcanon (); }
  unsigned int execute () { return tree_ssa_loop_ivcanon (); }

}; // class pass_iv_canon

} // anon namespace

gimple_opt_pass *
make_pass_iv_canon (gcc::context *ctxt)
{
  return new pass_iv_canon (ctxt);
}

/* Propagation of constants using scev.  */

static bool
gate_scev_const_prop (void)
{
  return flag_tree_scev_cprop;
}

namespace {

const pass_data pass_data_scev_cprop =
{
  GIMPLE_PASS, /* type */
  "sccp", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_SCEV_CONST, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg
    | TODO_update_ssa_only_virtuals ), /* todo_flags_finish */
};

class pass_scev_cprop : public gimple_opt_pass
{
public:
  pass_scev_cprop(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_scev_cprop, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_scev_const_prop (); }
  unsigned int execute () { return scev_const_prop (); }

}; // class pass_scev_cprop

} // anon namespace

gimple_opt_pass *
make_pass_scev_cprop (gcc::context *ctxt)
{
  return new pass_scev_cprop (ctxt);
}

/* Record bounds on numbers of iterations of loops.  */

static unsigned int
tree_ssa_loop_bounds (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  estimate_numbers_of_iterations ();
  scev_reset ();
  return 0;
}

namespace {

const pass_data pass_data_record_bounds =
{
  GIMPLE_PASS, /* type */
  "*record_bounds", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_TREE_LOOP_BOUNDS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_record_bounds : public gimple_opt_pass
{
public:
  pass_record_bounds(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_record_bounds, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () { return tree_ssa_loop_bounds (); }

}; // class pass_record_bounds

} // anon namespace

gimple_opt_pass *
make_pass_record_bounds (gcc::context *ctxt)
{
  return new pass_record_bounds (ctxt);
}

/* Complete unrolling of loops.  */

static unsigned int
tree_complete_unroll (void)
{
  if (number_of_loops (cfun) <= 1)
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

namespace {

const pass_data pass_data_complete_unroll =
{
  GIMPLE_PASS, /* type */
  "cunroll", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_COMPLETE_UNROLL, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_complete_unroll : public gimple_opt_pass
{
public:
  pass_complete_unroll(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_complete_unroll, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_complete_unroll (); }
  unsigned int execute () { return tree_complete_unroll (); }

}; // class pass_complete_unroll

} // anon namespace

gimple_opt_pass *
make_pass_complete_unroll (gcc::context *ctxt)
{
  return new pass_complete_unroll (ctxt);
}

/* Complete unrolling of inner loops.  */

static unsigned int
tree_complete_unroll_inner (void)
{
  unsigned ret = 0;

  loop_optimizer_init (LOOPS_NORMAL
		       | LOOPS_HAVE_RECORDED_EXITS);
  if (number_of_loops (cfun) > 1)
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

namespace {

const pass_data pass_data_complete_unrolli =
{
  GIMPLE_PASS, /* type */
  "cunrolli", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_COMPLETE_UNROLL, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_flow, /* todo_flags_finish */
};

class pass_complete_unrolli : public gimple_opt_pass
{
public:
  pass_complete_unrolli(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_complete_unrolli, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_complete_unroll_inner (); }
  unsigned int execute () { return tree_complete_unroll_inner (); }

}; // class pass_complete_unrolli

} // anon namespace

gimple_opt_pass *
make_pass_complete_unrolli (gcc::context *ctxt)
{
  return new pass_complete_unrolli (ctxt);
}

/* Parallelization.  */

static bool
gate_tree_parallelize_loops (void)
{
  return flag_tree_parallelize_loops > 1;
}

static unsigned
tree_parallelize_loops (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  if (parallelize_loops ())
    return TODO_cleanup_cfg | TODO_rebuild_alias;
  return 0;
}

namespace {

const pass_data pass_data_parallelize_loops =
{
  GIMPLE_PASS, /* type */
  "parloops", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_PARALLELIZE_LOOPS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_flow, /* todo_flags_finish */
};

class pass_parallelize_loops : public gimple_opt_pass
{
public:
  pass_parallelize_loops(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_parallelize_loops, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_parallelize_loops (); }
  unsigned int execute () { return tree_parallelize_loops (); }

}; // class pass_parallelize_loops

} // anon namespace

gimple_opt_pass *
make_pass_parallelize_loops (gcc::context *ctxt)
{
  return new pass_parallelize_loops (ctxt);
}

/* Prefetching.  */

static unsigned int
tree_ssa_loop_prefetch (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  return tree_ssa_prefetch_arrays ();
}

static bool
gate_tree_ssa_loop_prefetch (void)
{
  return flag_prefetch_loop_arrays > 0;
}

namespace {

const pass_data pass_data_loop_prefetch =
{
  GIMPLE_PASS, /* type */
  "aprefetch", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_PREFETCH, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_loop_prefetch : public gimple_opt_pass
{
public:
  pass_loop_prefetch(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_loop_prefetch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_ssa_loop_prefetch (); }
  unsigned int execute () { return tree_ssa_loop_prefetch (); }

}; // class pass_loop_prefetch

} // anon namespace

gimple_opt_pass *
make_pass_loop_prefetch (gcc::context *ctxt)
{
  return new pass_loop_prefetch (ctxt);
}

/* Induction variable optimizations.  */

static unsigned int
tree_ssa_loop_ivopts (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  tree_ssa_iv_optimize ();
  return 0;
}

static bool
gate_tree_ssa_loop_ivopts (void)
{
  return flag_ivopts != 0;
}

namespace {

const pass_data pass_data_iv_optimize =
{
  GIMPLE_PASS, /* type */
  "ivopts", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_LOOP_IVOPTS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_iv_optimize : public gimple_opt_pass
{
public:
  pass_iv_optimize(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_iv_optimize, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_ssa_loop_ivopts (); }
  unsigned int execute () { return tree_ssa_loop_ivopts (); }

}; // class pass_iv_optimize

} // anon namespace

gimple_opt_pass *
make_pass_iv_optimize (gcc::context *ctxt)
{
  return new pass_iv_optimize (ctxt);
}

/* Loop optimizer finalization.  */

static unsigned int
tree_ssa_loop_done (void)
{
  free_numbers_of_iterations_estimates ();
  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

namespace {

const pass_data pass_data_tree_loop_done =
{
  GIMPLE_PASS, /* type */
  "loopdone", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_verify_flow ), /* todo_flags_finish */
};

class pass_tree_loop_done : public gimple_opt_pass
{
public:
  pass_tree_loop_done(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_tree_loop_done, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () { return tree_ssa_loop_done (); }

}; // class pass_tree_loop_done

} // anon namespace

gimple_opt_pass *
make_pass_tree_loop_done (gcc::context *ctxt)
{
  return new pass_tree_loop_done (ctxt);
}
