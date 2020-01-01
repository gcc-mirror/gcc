/* Loop optimizations over tree-ssa.
   Copyright (C) 2003-2020 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "memmodel.h"
#include "tm_p.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-inline.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "omp-general.h"
#include "diagnostic-core.h"
#include "stringpool.h"
#include "attribs.h"


/* A pass making sure loops are fixed up.  */

namespace {

const pass_data pass_data_fix_loops =
{
  GIMPLE_PASS, /* type */
  "fix_loops", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_fix_loops : public gimple_opt_pass
{
public:
  pass_fix_loops (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_fix_loops, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tree_loop_optimize; }

  virtual unsigned int execute (function *fn);
}; // class pass_fix_loops

unsigned int
pass_fix_loops::execute (function *)
{
  if (loops_state_satisfies_p (LOOPS_NEED_FIXUP))
    {
      calculate_dominance_info (CDI_DOMINATORS);
      fix_loop_structure (NULL);
    }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_fix_loops (gcc::context *ctxt)
{
  return new pass_fix_loops (ctxt);
}


/* Gate for loop pass group.  The group is controlled by -ftree-loop-optimize
   but we also avoid running it when the IL doesn't contain any loop.  */

static bool
gate_loop (function *fn)
{
  if (!flag_tree_loop_optimize)
    return false;

  /* For -fdump-passes which runs before loop discovery print the
     state of -ftree-loop-optimize.  */
  if (!loops_for_fn (fn))
    return true;

  return number_of_loops (fn) > 1;
}

/* The loop superpass.  */

namespace {

const pass_data pass_data_tree_loop =
{
  GIMPLE_PASS, /* type */
  "loop", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tree_loop : public gimple_opt_pass
{
public:
  pass_tree_loop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_loop, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *fn) { return gate_loop (fn); }

}; // class pass_tree_loop

} // anon namespace

gimple_opt_pass *
make_pass_tree_loop (gcc::context *ctxt)
{
  return new pass_tree_loop (ctxt);
}

/* Gate for oacc kernels pass group.  */

static bool
gate_oacc_kernels (function *fn)
{
  if (!flag_openacc)
    return false;

  if (!lookup_attribute ("oacc kernels", DECL_ATTRIBUTES (fn->decl)))
    return false;

  class loop *loop;
  FOR_EACH_LOOP (loop, 0)
    if (loop->in_oacc_kernels_region)
      return true;

  return false;
}

/* The oacc kernels superpass.  */

namespace {

const pass_data pass_data_oacc_kernels =
{
  GIMPLE_PASS, /* type */
  "oacc_kernels", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_oacc_kernels : public gimple_opt_pass
{
public:
  pass_oacc_kernels (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_oacc_kernels, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *fn) { return gate_oacc_kernels (fn); }

}; // class pass_oacc_kernels

} // anon namespace

gimple_opt_pass *
make_pass_oacc_kernels (gcc::context *ctxt)
{
  return new pass_oacc_kernels (ctxt);
}

/* The ipa oacc superpass.  */

namespace {

const pass_data pass_data_ipa_oacc =
{
  SIMPLE_IPA_PASS, /* type */
  "ipa_oacc", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_oacc : public simple_ipa_opt_pass
{
public:
  pass_ipa_oacc (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_oacc, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return (optimize
	    && flag_openacc
	    /* Don't bother doing anything if the program has errors.  */
	    && !seen_error ());
  }

}; // class pass_ipa_oacc

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_oacc (gcc::context *ctxt)
{
  return new pass_ipa_oacc (ctxt);
}

/* The ipa oacc kernels pass.  */

namespace {

const pass_data pass_data_ipa_oacc_kernels =
{
  SIMPLE_IPA_PASS, /* type */
  "ipa_oacc_kernels", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_TREE_LOOP, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_oacc_kernels : public simple_ipa_opt_pass
{
public:
  pass_ipa_oacc_kernels (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_oacc_kernels, ctxt)
  {}

}; // class pass_ipa_oacc_kernels

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_oacc_kernels (gcc::context *ctxt)
{
  return new pass_ipa_oacc_kernels (ctxt);
}

/* The no-loop superpass.  */

namespace {

const pass_data pass_data_tree_no_loop =
{
  GIMPLE_PASS, /* type */
  "no_loop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_NOLOOP, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tree_no_loop : public gimple_opt_pass
{
public:
  pass_tree_no_loop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_no_loop, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *fn) { return !gate_loop (fn); }

}; // class pass_tree_no_loop

} // anon namespace

gimple_opt_pass *
make_pass_tree_no_loop (gcc::context *ctxt)
{
  return new pass_tree_no_loop (ctxt);
}


/* Loop optimizer initialization.  */

namespace {

const pass_data pass_data_tree_loop_init =
{
  GIMPLE_PASS, /* type */
  "loopinit", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  TODO_update_address_taken, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_tree_loop_init : public gimple_opt_pass
{
public:
  pass_tree_loop_init (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_loop_init, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_tree_loop_init

unsigned int
pass_tree_loop_init::execute (function *fun ATTRIBUTE_UNUSED)
{
  /* When processing a loop in the loop pipeline, we should be able to assert
     that:
       (loops_state_satisfies_p (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS
					      | LOOP_CLOSED_SSA)
	&& scev_initialized_p ())
  */
  loop_optimizer_init (LOOPS_NORMAL
		       | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  scev_initialize ();

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_tree_loop_init (gcc::context *ctxt)
{
  return new pass_tree_loop_init (ctxt);
}

/* Loop autovectorization.  */

namespace {

const pass_data pass_data_vectorize =
{
  GIMPLE_PASS, /* type */
  "vect", /* name */
  OPTGROUP_LOOP | OPTGROUP_VEC, /* optinfo_flags */
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
  virtual bool gate (function *fun)
    {
      return flag_tree_loop_vectorize || fun->has_force_vectorize_loops;
    }

  virtual unsigned int execute (function *);

}; // class pass_vectorize

unsigned int
pass_vectorize::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  return vectorize_loops ();
}

} // anon namespace

gimple_opt_pass *
make_pass_vectorize (gcc::context *ctxt)
{
  return new pass_vectorize (ctxt);
}

/* Propagation of constants using scev.  */

namespace {

const pass_data pass_data_scev_cprop =
{
  GIMPLE_PASS, /* type */
  "sccp", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_SCEV_CONST, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_scev_cprop : public gimple_opt_pass
{
public:
  pass_scev_cprop (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_scev_cprop, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_tree_scev_cprop; }
  virtual unsigned int execute (function *);

}; // class pass_scev_cprop

unsigned
pass_scev_cprop::execute (function *)
{
  class loop *loop;
  bool any = false;

  /* Perform final value replacement in loops, in case the replacement
     expressions are cheap.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    any |= final_value_replacement_loop (loop);

  return any ? TODO_cleanup_cfg | TODO_update_ssa_only_virtuals : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_scev_cprop (gcc::context *ctxt)
{
  return new pass_scev_cprop (ctxt);
}

/* Induction variable optimizations.  */

namespace {

const pass_data pass_data_iv_optimize =
{
  GIMPLE_PASS, /* type */
  "ivopts", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
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
  virtual bool gate (function *) { return flag_ivopts != 0; }
  virtual unsigned int execute (function *);

}; // class pass_iv_optimize

unsigned int
pass_iv_optimize::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  tree_ssa_iv_optimize ();
  return 0;
}

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
  free_numbers_of_iterations_estimates (cfun);
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
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_tree_loop_done : public gimple_opt_pass
{
public:
  pass_tree_loop_done (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_loop_done, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return tree_ssa_loop_done (); }

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
	  if (DECL_P (*addr_p)
	      || CONSTANT_CLASS_P (*addr_p))
	    return true;
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
    case FUNCTION_DECL:
    case LABEL_DECL:
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
    default:
      /* Nothing.  */
      break;
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
  if (suffix != NULL)
    lsm_tmp_name_add (suffix);
  return lsm_tmp_name;
}

/* Computes an estimated number of insns in LOOP, weighted by WEIGHTS.  */

unsigned
tree_num_loop_insns (class loop *loop, eni_weights *weights)
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



