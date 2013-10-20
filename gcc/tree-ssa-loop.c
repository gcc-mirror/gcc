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
  pass_tree_loop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_loop, ctxt)
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
  pass_tree_loop_init (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_loop_init, ctxt)
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

/* Loop autovectorization.  */

static unsigned int
tree_loop_vectorize (void)
{
  if (number_of_loops (cfun) <= 1)
    return 0;

  return vectorize_loops ();
}

static bool
gate_tree_loop_vectorize (void)
{
  return flag_tree_loop_vectorize || cfun->has_force_vect_loops;
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
  pass_vectorize (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_vectorize, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_loop_vectorize (); }
  unsigned int execute () { return tree_loop_vectorize (); }

}; // class pass_vectorize

} // anon namespace

gimple_opt_pass *
make_pass_vectorize (gcc::context *ctxt)
{
  return new pass_vectorize (ctxt);
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
  pass_check_data_deps (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_check_data_deps, ctxt)
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
  pass_scev_cprop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_scev_cprop, ctxt)
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
  pass_record_bounds (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_record_bounds, ctxt)
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
  pass_iv_optimize (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_iv_optimize, ctxt)
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
  pass_tree_loop_done (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_loop_done, ctxt)
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

/* Calls CBCK for each index in memory reference ADDR_P.  There are two
   kinds situations handled; in each of these cases, the memory reference
   and DATA are passed to the callback:

   Access to an array: ARRAY_{RANGE_}REF (base, index).  In this case we also
   pass the pointer to the index to the callback.

   Pointer dereference: INDIRECT_REF (addr).  In this case we also pass the
   pointer to addr to the callback.

   If the callback returns false, the whole search stops and false is returned.
   Otherwise the function returns true after traversing through the whole
   reference *ADDR_P.  */

bool
for_each_index (tree *addr_p, bool (*cbck) (tree, tree *, void *), void *data)
{
  tree *nxt, *idx;

  for (; ; addr_p = nxt)
    {
      switch (TREE_CODE (*addr_p))
	{
	case SSA_NAME:
	  return cbck (*addr_p, addr_p, data);

	case MEM_REF:
	  nxt = &TREE_OPERAND (*addr_p, 0);
	  return cbck (*addr_p, nxt, data);

	case BIT_FIELD_REF:
	case VIEW_CONVERT_EXPR:
	case REALPART_EXPR:
	case IMAGPART_EXPR:
	  nxt = &TREE_OPERAND (*addr_p, 0);
	  break;

	case COMPONENT_REF:
	  /* If the component has varying offset, it behaves like index
	     as well.  */
	  idx = &TREE_OPERAND (*addr_p, 2);
	  if (*idx
	      && !cbck (*addr_p, idx, data))
	    return false;

	  nxt = &TREE_OPERAND (*addr_p, 0);
	  break;

	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  nxt = &TREE_OPERAND (*addr_p, 0);
	  if (!cbck (*addr_p, &TREE_OPERAND (*addr_p, 1), data))
	    return false;
	  break;

	case VAR_DECL:
	case PARM_DECL:
	case CONST_DECL:
	case STRING_CST:
	case RESULT_DECL:
	case VECTOR_CST:
	case COMPLEX_CST:
	case INTEGER_CST:
	case REAL_CST:
	case FIXED_CST:
	case CONSTRUCTOR:
	  return true;

	case ADDR_EXPR:
	  gcc_assert (is_gimple_min_invariant (*addr_p));
	  return true;

	case TARGET_MEM_REF:
	  idx = &TMR_BASE (*addr_p);
	  if (*idx
	      && !cbck (*addr_p, idx, data))
	    return false;
	  idx = &TMR_INDEX (*addr_p);
	  if (*idx
	      && !cbck (*addr_p, idx, data))
	    return false;
	  idx = &TMR_INDEX2 (*addr_p);
	  if (*idx
	      && !cbck (*addr_p, idx, data))
	    return false;
	  return true;

	default:
    	  gcc_unreachable ();
	}
    }
}


/* The name and the length of the currently generated variable
   for lsm.  */
#define MAX_LSM_NAME_LENGTH 40
static char lsm_tmp_name[MAX_LSM_NAME_LENGTH + 1];
static int lsm_tmp_name_length;

/* Adds S to lsm_tmp_name.  */

static void
lsm_tmp_name_add (const char *s)
{
  int l = strlen (s) + lsm_tmp_name_length;
  if (l > MAX_LSM_NAME_LENGTH)
    return;

  strcpy (lsm_tmp_name + lsm_tmp_name_length, s);
  lsm_tmp_name_length = l;
}

/* Stores the name for temporary variable that replaces REF to
   lsm_tmp_name.  */

static void
gen_lsm_tmp_name (tree ref)
{
  const char *name;

  switch (TREE_CODE (ref))
    {
    case MEM_REF:
    case TARGET_MEM_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_");
      break;

    case ADDR_EXPR:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      break;

    case BIT_FIELD_REF:
    case VIEW_CONVERT_EXPR:
    case ARRAY_RANGE_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      break;

    case REALPART_EXPR:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_RE");
      break;

    case IMAGPART_EXPR:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_IM");
      break;

    case COMPONENT_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_");
      name = get_name (TREE_OPERAND (ref, 1));
      if (!name)
	name = "F";
      lsm_tmp_name_add (name);
      break;

    case ARRAY_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_I");
      break;

    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
      name = get_name (ref);
      if (!name)
	name = "D";
      lsm_tmp_name_add (name);
      break;

    case STRING_CST:
      lsm_tmp_name_add ("S");
      break;

    case RESULT_DECL:
      lsm_tmp_name_add ("R");
      break;

    case INTEGER_CST:
      /* Nothing.  */
      break;

    default:
      gcc_unreachable ();
    }
}

/* Determines name for temporary variable that replaces REF.
   The name is accumulated into the lsm_tmp_name variable.
   N is added to the name of the temporary.  */

char *
get_lsm_tmp_name (tree ref, unsigned n, const char *suffix)
{
  char ns[2];

  lsm_tmp_name_length = 0;
  gen_lsm_tmp_name (ref);
  lsm_tmp_name_add ("_lsm");
  if (n < 10)
    {
      ns[0] = '0' + n;
      ns[1] = 0;
      lsm_tmp_name_add (ns);
    }
  return lsm_tmp_name;
  if (suffix != NULL)
    lsm_tmp_name_add (suffix);
}

/* Computes an estimated number of insns in LOOP, weighted by WEIGHTS.  */

unsigned
tree_num_loop_insns (struct loop *loop, eni_weights *weights)
{
  basic_block *body = get_loop_body (loop);
  gimple_stmt_iterator gsi;
  unsigned size = 0, i;

  for (i = 0; i < loop->num_nodes; i++)
    for (gsi = gsi_start_bb (body[i]); !gsi_end_p (gsi); gsi_next (&gsi))
      size += estimate_num_insns (gsi_stmt (gsi), weights);
  free (body);

  return size;
}



