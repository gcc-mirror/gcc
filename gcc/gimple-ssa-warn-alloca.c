/* Warn on problematic uses of alloca and variable length arrays.
   Copyright (C) 2016-2021 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

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
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa.h"
#include "tree-cfg.h"
#include "builtins.h"
#include "calls.h"
#include "cfgloop.h"
#include "intl.h"
#include "gimple-range.h"

static unsigned HOST_WIDE_INT adjusted_warn_limit (bool);

const pass_data pass_data_walloca = {
  GIMPLE_PASS,
  "walloca",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg, // properties_required
  0,	    // properties_provided
  0,	    // properties_destroyed
  0,	    // properties_start
  0,	    // properties_finish
};

class pass_walloca : public gimple_opt_pass
{
public:
  pass_walloca (gcc::context *ctxt)
    : gimple_opt_pass(pass_data_walloca, ctxt), first_time_p (false)
  {}
  opt_pass *clone () { return new pass_walloca (m_ctxt); }
  void set_pass_param (unsigned int n, bool param)
    {
      gcc_assert (n == 0);
      first_time_p = param;
    }
  virtual bool gate (function *);
  virtual unsigned int execute (function *);

 private:
  // Set to TRUE the first time we run this pass on a function.
  bool first_time_p;
};

bool
pass_walloca::gate (function *fun ATTRIBUTE_UNUSED)
{
  // The first time this pass is called, it is called before
  // optimizations have been run and range information is unavailable,
  // so we can only perform strict alloca checking.
  if (first_time_p)
    return warn_alloca != 0;

  // Warning is disabled when its size limit is greater than PTRDIFF_MAX
  // for the target maximum, which makes the limit negative since when
  // represented in signed HOST_WIDE_INT.
  unsigned HOST_WIDE_INT max = tree_to_uhwi (TYPE_MAX_VALUE (ptrdiff_type_node));
  return (adjusted_warn_limit (false) <= max
	  || adjusted_warn_limit (true) <= max);
}

// Possible problematic uses of alloca.
enum alloca_type {
  // Alloca argument is within known bounds that are appropriate.
  ALLOCA_OK,

  // Alloca argument is KNOWN to have a value that is too large.
  ALLOCA_BOUND_DEFINITELY_LARGE,

  // Alloca argument may be too large.
  ALLOCA_BOUND_MAYBE_LARGE,

  // Alloca appears in a loop.
  ALLOCA_IN_LOOP,

  // Alloca argument is 0.
  ALLOCA_ARG_IS_ZERO,

  // Alloca call is unbounded.  That is, there is no controlling
  // predicate for its argument.
  ALLOCA_UNBOUNDED
};

// Type of an alloca call with its corresponding limit, if applicable.
class alloca_type_and_limit {
public:
  enum alloca_type type;
  // For ALLOCA_BOUND_MAYBE_LARGE and ALLOCA_BOUND_DEFINITELY_LARGE
  // types, this field indicates the assumed limit if known or
  // integer_zero_node if unknown.  For any other alloca types, this
  // field is undefined.
  wide_int limit;
  alloca_type_and_limit ();
  alloca_type_and_limit (enum alloca_type type,
			 wide_int i) : type(type), limit(i) { }
  alloca_type_and_limit (enum alloca_type type) : type(type)
  {
    limit = wi::to_wide (integer_zero_node);
  }
};

/* Return TRUE if the user specified a limit for either VLAs or ALLOCAs.  */

static bool
warn_limit_specified_p (bool is_vla)
{
  unsigned HOST_WIDE_INT max = is_vla ? warn_vla_limit : warn_alloca_limit;
  return max != HOST_WIDE_INT_MAX;
}

/* Return the value of the argument N to -Walloca-larger-than= or
   -Wvla-larger-than= adjusted for the target data model so that
   when N == HOST_WIDE_INT_MAX, the adjusted value is set to
   PTRDIFF_MAX on the target.  This is done to prevent warnings
   for unknown/unbounded allocations in the "permissive mode"
   while still diagnosing excessive and necessarily invalid
   allocations.  */

static unsigned HOST_WIDE_INT
adjusted_warn_limit (bool idx)
{
  static HOST_WIDE_INT limits[2];
  if (limits[idx])
    return limits[idx];

  limits[idx] = idx ? warn_vla_limit : warn_alloca_limit;
  if (limits[idx] != HOST_WIDE_INT_MAX)
    return limits[idx];

  limits[idx] = tree_to_shwi (TYPE_MAX_VALUE (ptrdiff_type_node));
  return limits[idx];
}

// Analyze the alloca call in STMT and return the alloca type with its
// corresponding limit (if applicable).  IS_VLA is set if the alloca
// call was created by the gimplifier for a VLA.

static class alloca_type_and_limit
alloca_call_type (range_query &query, gimple *stmt, bool is_vla)
{
  gcc_assert (gimple_alloca_call_p (stmt));
  tree len = gimple_call_arg (stmt, 0);

  gcc_assert (!is_vla || warn_vla_limit >= 0);
  gcc_assert (is_vla || warn_alloca_limit >= 0);

  // Adjust warn_alloca_max_size for VLAs, by taking the underlying
  // type into account.
  unsigned HOST_WIDE_INT max_size = adjusted_warn_limit (is_vla);

  // Check for the obviously bounded case.
  if (TREE_CODE (len) == INTEGER_CST)
    {
      if (tree_to_uhwi (len) > max_size)
	return alloca_type_and_limit (ALLOCA_BOUND_DEFINITELY_LARGE,
				      wi::to_wide (len));
      if (integer_zerop (len))
	{
	  const offset_int maxobjsize
	    = wi::to_offset (max_object_size ());
	  alloca_type result = (max_size < maxobjsize
				? ALLOCA_ARG_IS_ZERO : ALLOCA_OK);
	  return alloca_type_and_limit (result);
	}

      return alloca_type_and_limit (ALLOCA_OK);
    }

  struct alloca_type_and_limit ret = alloca_type_and_limit (ALLOCA_OK);
  // If we have a declared maximum size, we can take it into account.
  if (gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX))
    {
      tree arg = gimple_call_arg (stmt, 2);
      if (compare_tree_int (arg, max_size) <= 0)
	ret = alloca_type_and_limit (ALLOCA_OK);
      else
	{
	  const offset_int maxobjsize
	    = wi::to_offset (max_object_size ());
	  alloca_type result = (max_size < maxobjsize
				? ALLOCA_BOUND_MAYBE_LARGE : ALLOCA_OK);
	  ret = alloca_type_and_limit (result, wi::to_wide (arg));
	}
      return ret;
    }

  // If the user specified a limit, use it.
  int_range_max r;
  if (warn_limit_specified_p (is_vla)
      && TREE_CODE (len) == SSA_NAME
      && query.range_of_expr (r, len, stmt)
      && !r.varying_p ())
    {
      // The invalid bits are anything outside of [0, MAX_SIZE].
      static int_range<2> invalid_range (build_int_cst (size_type_node, 0),
					 build_int_cst (size_type_node,
							max_size),
					 VR_ANTI_RANGE);

      r.intersect (invalid_range);
      if (r.undefined_p ())
	return alloca_type_and_limit (ALLOCA_OK);

      return alloca_type_and_limit (ALLOCA_BOUND_MAYBE_LARGE,
				    wi::to_wide (integer_zero_node));
    }

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());
  /* When MAX_SIZE is greater than or equal to PTRDIFF_MAX treat
     allocations that aren't visibly constrained as OK, otherwise
     report them as (potentially) unbounded.  */
  alloca_type unbounded_result = (max_size < maxobjsize.to_uhwi ()
				  ? ALLOCA_UNBOUNDED : ALLOCA_OK);
  return alloca_type_and_limit (unbounded_result);
}

// Return TRUE if STMT is in a loop, otherwise return FALSE.

static bool
in_loop_p (gimple *stmt)
{
  basic_block bb = gimple_bb (stmt);
  return
    bb->loop_father && bb->loop_father->header != ENTRY_BLOCK_PTR_FOR_FN (cfun);
}

unsigned int
pass_walloca::execute (function *fun)
{
  gimple_ranger ranger;
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  if (!gimple_alloca_call_p (stmt))
	    continue;

	  location_t loc = gimple_nonartificial_location (stmt);
	  loc = expansion_point_location_if_in_system_header (loc);

	  const bool is_vla
	    = gimple_call_alloca_for_var_p (as_a <gcall *> (stmt));

	  // Strict mode whining for VLAs is handled by the front-end,
	  // so we can safely ignore this case.  Also, ignore VLAs if
	  // the user doesn't care about them.
	  if (is_vla)
	    {
	      if (warn_vla > 0 || warn_vla_limit < 0)
		continue;
	    }
	  else if (warn_alloca)
	    {
	      warning_at (loc, OPT_Walloca, "%Guse of %<alloca%>", stmt);
	      continue;
	    }
	  else if (warn_alloca_limit < 0)
	    continue;

	  class alloca_type_and_limit t
	    = alloca_call_type (ranger, stmt, is_vla);

	  unsigned HOST_WIDE_INT adjusted_alloca_limit
	    = adjusted_warn_limit (false);
	  // Even if we think the alloca call is OK, make sure it's not in a
	  // loop, except for a VLA, since VLAs are guaranteed to be cleaned
	  // up when they go out of scope, including in a loop.
	  if (t.type == ALLOCA_OK && !is_vla && in_loop_p (stmt))
	    {
	      /* As in other instances, only diagnose this when the limit
		 is less than the maximum valid object size.  */
	      const offset_int maxobjsize
		= wi::to_offset (max_object_size ());
	      if (adjusted_alloca_limit < maxobjsize.to_uhwi ())
		t = alloca_type_and_limit (ALLOCA_IN_LOOP);
	    }

	  enum opt_code wcode
	    = is_vla ? OPT_Wvla_larger_than_ : OPT_Walloca_larger_than_;
	  char buff[WIDE_INT_MAX_PRECISION / 4 + 4];
	  switch (t.type)
	    {
	    case ALLOCA_OK:
	      break;
	    case ALLOCA_BOUND_MAYBE_LARGE:
	      {
		auto_diagnostic_group d;
		if (warning_at (loc, wcode,
				(is_vla
				 ? G_("%Gargument to variable-length "
				      "array may be too large")
				 : G_("%Gargument to %<alloca%> may be too "
				      "large")),
				stmt)
		    && t.limit != 0)
		  {
		    print_decu (t.limit, buff);
		    inform (loc, "limit is %wu bytes, but argument "
				 "may be as large as %s",
			    is_vla ? warn_vla_limit : adjusted_alloca_limit,
			    buff);
		  }
	      }
	      break;
	    case ALLOCA_BOUND_DEFINITELY_LARGE:
	      {
		auto_diagnostic_group d;
		if (warning_at (loc, wcode,
				(is_vla
				 ? G_("%Gargument to variable-length"
				      " array is too large")
				 : G_("%Gargument to %<alloca%> is too large")),
				stmt)
		    && t.limit != 0)
		  {
		    print_decu (t.limit, buff);
		    inform (loc, "limit is %wu bytes, but argument is %s",
			    is_vla ? warn_vla_limit : adjusted_alloca_limit,
			    buff);
		  }
	      }
	      break;
	    case ALLOCA_UNBOUNDED:
	      warning_at (loc, wcode,
			  (is_vla
			   ? G_("%Gunbounded use of variable-length array")
			   : G_("%Gunbounded use of %<alloca%>")),
			  stmt);
	      break;
	    case ALLOCA_IN_LOOP:
	      gcc_assert (!is_vla);
	      warning_at (loc, wcode,
			  "%Guse of %<alloca%> within a loop", stmt);
	      break;
	    case ALLOCA_ARG_IS_ZERO:
	      warning_at (loc, wcode,
			  (is_vla
			   ? G_("%Gargument to variable-length array "
				"is zero")
			   : G_("%Gargument to %<alloca%> is zero")),
			  stmt);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
    }
  return 0;
}

gimple_opt_pass *
make_pass_walloca (gcc::context *ctxt)
{
  return new pass_walloca (ctxt);
}
