/* Transformation pass for OpenACC kernels regions.  Converts a kernels
   region into a series of smaller parallel regions.  There is a parallel
   region for each parallelizable loop nest, as well as a "gang-single"
   parallel region for each non-parallelizable piece of code.

   Contributed by Gergö Barany <gergo@codesourcery.com> and
		  Thomas Schwinge <thomas@codesourcery.com>

   Copyright (C) 2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gomp-constants.h"

/* This is a preprocessing pass to be run immediately before lower_omp.  It
   will convert OpenACC "kernels" regions into sequences of "parallel"
   regions.
   For now, the translation is as follows:
   - The entire kernels region is turned into a data region with clauses
     taken from the kernels region.  New "create" clauses are added for all
     variables declared at the top level in the kernels region.  */

/* Transform KERNELS_REGION, which is an OpenACC kernels region, into a data
   region containing the original kernels region.  */

static gimple *
transform_kernels_region (gimple *kernels_region)
{
  gcc_checking_assert (gimple_omp_target_kind (kernels_region)
		       == GF_OMP_TARGET_KIND_OACC_KERNELS);

  /* Collect the kernels region's data clauses and create the new data
     region with those clauses.  */
  tree kernels_clauses = gimple_omp_target_clauses (kernels_region);
  tree data_clauses = NULL;
  for (tree c = kernels_clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      /* Certain map clauses are copied to the enclosing data region.  Any
	 non-data clause remains on the kernels region.  */
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP)
	{
	  tree decl = OMP_CLAUSE_DECL (c);
	  HOST_WIDE_INT kind = OMP_CLAUSE_MAP_KIND (c);
	  switch (kind)
	    {
	    default:
	      if (kind == GOMP_MAP_ALLOC &&
		  integer_zerop (OMP_CLAUSE_SIZE (c)))
		/* ??? This is an alloc clause for mapping a pointer whose
		   target is already mapped.  We leave these on the inner
		   parallel regions because moving them to the outer data
		   region causes runtime errors.  */
		break;

	      /* For non-artificial variables, and for non-declaration
		 expressions like A[0:n], copy the clause to the data
		 region.  */
	      if ((DECL_P (decl) && !DECL_ARTIFICIAL (decl))
		  || !DECL_P (decl))
		{
		  tree new_clause = build_omp_clause (OMP_CLAUSE_LOCATION (c),
						      OMP_CLAUSE_MAP);
		  OMP_CLAUSE_SET_MAP_KIND (new_clause, kind);
		  /* This must be unshared here to avoid "incorrect sharing
		     of tree nodes" errors from verify_gimple.  */
		  OMP_CLAUSE_DECL (new_clause) = unshare_expr (decl);
		  OMP_CLAUSE_SIZE (new_clause) = OMP_CLAUSE_SIZE (c);
		  OMP_CLAUSE_CHAIN (new_clause) = data_clauses;
		  data_clauses = new_clause;

		  /* Now that this data is mapped, the inner data clause on
		     the kernels region can become a present clause.  */
		  OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_FORCE_PRESENT);
		}
	      break;

	    case GOMP_MAP_POINTER:
	    case GOMP_MAP_TO_PSET:
	    case GOMP_MAP_FORCE_TOFROM:
	    case GOMP_MAP_FIRSTPRIVATE_POINTER:
	    case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	      /* ??? Copying these map kinds leads to internal compiler
		 errors in later passes.  */
	      break;
	    }
	}
      else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IF)
	{
	  /* If there is an if clause, it must also be present on the
	     enclosing data region.  Temporarily remove the if clause's
	     chain to avoid copying it.  */
	  tree saved_chain = OMP_CLAUSE_CHAIN (c);
	  OMP_CLAUSE_CHAIN (c) = NULL;
	  tree new_if_clause = unshare_expr (c);
	  OMP_CLAUSE_CHAIN (c) = saved_chain;
	  OMP_CLAUSE_CHAIN (new_if_clause) = data_clauses;
	  data_clauses = new_if_clause;
	}
    }
  /* Restore the original order of the clauses.  */
  data_clauses = nreverse (data_clauses);

  gimple *data_region
    = gimple_build_omp_target (NULL, GF_OMP_TARGET_KIND_OACC_DATA_KERNELS,
			       data_clauses);
  gimple_set_location (data_region, gimple_location (kernels_region));

  /* For now, just construct a new parallel region inside the data region.  */
  gimple *inner_region
    = gimple_build_omp_target (NULL, GF_OMP_TARGET_KIND_OACC_PARALLEL,
			       kernels_clauses);
  gimple_set_location (inner_region, gimple_location (kernels_region));
  gimple_omp_set_body (inner_region, gimple_omp_body (kernels_region));

  gbind *bind = gimple_build_bind (NULL, NULL, NULL);
  gimple_bind_add_stmt (bind, inner_region);

  /* Put the transformed pieces together.  The entire body of the region is
     wrapped in a try-finally statement that calls __builtin_GOACC_data_end
     for cleanup.  */
  tree data_end_fn = builtin_decl_explicit (BUILT_IN_GOACC_DATA_END);
  gimple *call = gimple_build_call (data_end_fn, 0);
  gimple_seq cleanup = NULL;
  gimple_seq_add_stmt (&cleanup, call);
  gimple *try_stmt = gimple_build_try (bind, cleanup, GIMPLE_TRY_FINALLY);
  gimple_omp_set_body (data_region, try_stmt);

  return data_region;
}

/* Helper function of convert_oacc_kernels for walking the tree, calling
   transform_kernels_region on each kernels region found.  */

static tree
scan_kernels (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
	      struct walk_stmt_info *)
{
  gimple *stmt = gsi_stmt (*gsi_p);
  *handled_ops_p = false;

  int kind;
  switch (gimple_code (stmt))
    {
    case GIMPLE_OMP_TARGET:
      kind = gimple_omp_target_kind (stmt);
      if (kind == GF_OMP_TARGET_KIND_OACC_KERNELS)
	{
	  gimple *new_region = transform_kernels_region (stmt);
	  gsi_replace (gsi_p, new_region, false);
	  *handled_ops_p = true;
	}
      break;

    default:
      break;
    }

  return NULL;
}

/* Find and transform OpenACC kernels regions in the current function.  */

static unsigned int
convert_oacc_kernels (void)
{
  struct walk_stmt_info wi;
  gimple_seq body = gimple_body (current_function_decl);

  memset (&wi, 0, sizeof (wi));
  walk_gimple_seq_mod (&body, scan_kernels, NULL, &wi);

  gimple_set_body (current_function_decl, body);

  return 0;
}

namespace {

const pass_data pass_data_convert_oacc_kernels =
{
  GIMPLE_PASS, /* type */
  "convert_oacc_kernels", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_convert_oacc_kernels : public gimple_opt_pass
{
public:
  pass_convert_oacc_kernels (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_convert_oacc_kernels, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return (flag_openacc
	    && flag_openacc_kernels == OPENACC_KERNELS_SPLIT);
  }
  virtual unsigned int execute (function *)
  {
    return convert_oacc_kernels ();
  }

}; // class pass_convert_oacc_kernels

} // anon namespace

gimple_opt_pass *
make_pass_convert_oacc_kernels (gcc::context *ctxt)
{
  return new pass_convert_oacc_kernels (ctxt);
}
