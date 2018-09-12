/* Warn on problematic uses of alloca and variable length arrays.
   Copyright (C) 2016-2018 Free Software Foundation, Inc.
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
#include "params.h"
#include "tree-cfg.h"
#include "builtins.h"
#include "calls.h"
#include "cfgloop.h"
#include "intl.h"

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

  // Alloca argument is bounded but of an indeterminate size.
  ALLOCA_BOUND_UNKNOWN,

  // Alloca argument was casted from a signed integer.
  ALLOCA_CAST_FROM_SIGNED,

  // Alloca appears in a loop.
  ALLOCA_IN_LOOP,

  // Alloca argument is 0.
  ALLOCA_ARG_IS_ZERO,

  // Alloca call is unbounded.  That is, there is no controlling
  // predicate for its argument.
  ALLOCA_UNBOUNDED
};

// Type of an alloca call with its corresponding limit, if applicable.
struct alloca_type_and_limit {
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
  { if (type == ALLOCA_BOUND_MAYBE_LARGE
	|| type == ALLOCA_BOUND_DEFINITELY_LARGE)
      limit = wi::to_wide (integer_zero_node);
  }
};

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


// NOTE: When we get better range info, this entire function becomes
// irrelevant, as it should be possible to get range info for an SSA
// name at any point in the program.
//
// We have a few heuristics up our sleeve to determine if a call to
// alloca() is within bounds.  Try them out and return the type of
// alloca call with its assumed limit (if applicable).
//
// Given a known argument (ARG) to alloca() and an EDGE (E)
// calculating said argument, verify that the last statement in the BB
// in E->SRC is a gate comparing ARG to an acceptable bound for
// alloca().  See examples below.
//
// If set, ARG_CASTED is the possible unsigned argument to which ARG
// was casted to.  This is to handle cases where the controlling
// predicate is looking at a casted value, not the argument itself.
//    arg_casted = (size_t) arg;
//    if (arg_casted < N)
//      goto bb3;
//    else
//      goto bb5;
//
// MAX_SIZE is WARN_ALLOCA= adjusted for VLAs.  It is the maximum size
// in bytes we allow for arg.

static struct alloca_type_and_limit
alloca_call_type_by_arg (tree arg, tree arg_casted, edge e,
			 unsigned HOST_WIDE_INT max_size)
{
  basic_block bb = e->src;
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gimple *last = gsi_stmt (gsi);

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  /* When MAX_SIZE is greater than or equal to PTRDIFF_MAX treat
     allocations that aren't visibly constrained as OK, otherwise
     report them as (potentially) unbounded.  */
  alloca_type unbounded_result = (max_size < maxobjsize.to_uhwi ()
				  ? ALLOCA_UNBOUNDED : ALLOCA_OK);

  if (!last || gimple_code (last) != GIMPLE_COND)
    {
      return alloca_type_and_limit (unbounded_result);
    }

  enum tree_code cond_code = gimple_cond_code (last);
  if (e->flags & EDGE_TRUE_VALUE)
    ;
  else if (e->flags & EDGE_FALSE_VALUE)
    cond_code = invert_tree_comparison (cond_code, false);
  else
      return alloca_type_and_limit (unbounded_result);

  // Check for:
  //   if (ARG .COND. N)
  //     goto <bb 3>;
  //   else
  //     goto <bb 4>;
  //   <bb 3>:
  //   alloca(ARG);
  if ((cond_code == LE_EXPR
       || cond_code == LT_EXPR
       || cond_code == GT_EXPR
       || cond_code == GE_EXPR)
      && (gimple_cond_lhs (last) == arg
	  || gimple_cond_lhs (last) == arg_casted))
    {
      if (TREE_CODE (gimple_cond_rhs (last)) == INTEGER_CST)
	{
	  tree rhs = gimple_cond_rhs (last);
	  int tst = wi::cmpu (wi::to_widest (rhs), max_size);
	  if ((cond_code == LT_EXPR && tst == -1)
	      || (cond_code == LE_EXPR && (tst == -1 || tst == 0)))
	    return alloca_type_and_limit (ALLOCA_OK);
	  else
	    {
	      // Let's not get too specific as to how large the limit
	      // may be.  Someone's clearly an idiot when things
	      // degrade into "if (N > Y) alloca(N)".
	      if (cond_code == GT_EXPR || cond_code == GE_EXPR)
		rhs = integer_zero_node;
	      return alloca_type_and_limit (ALLOCA_BOUND_MAYBE_LARGE,
					    wi::to_wide (rhs));
	    }
	}
      else
	{
	  /* Analogous to ALLOCA_UNBOUNDED, when MAX_SIZE is greater
	     than or equal to PTRDIFF_MAX, treat allocations with
	     an unknown bound as OK.  */
	  alloca_type unknown_result
	    = (max_size < maxobjsize.to_uhwi ()
	       ? ALLOCA_BOUND_UNKNOWN : ALLOCA_OK);
	  return alloca_type_and_limit (unknown_result);
	}
    }

  // Similarly, but check for a comparison with an unknown LIMIT.
  //   if (LIMIT .COND. ARG)
  //     alloca(arg);
  //
  //   Where LIMIT has a bound of unknown range.
  //
  // Note: All conditions of the form (ARG .COND. XXXX) where covered
  // by the previous check above, so we only need to look for (LIMIT
  // .COND. ARG) here.
  tree limit = gimple_cond_lhs (last);
  if ((gimple_cond_rhs (last) == arg
       || gimple_cond_rhs (last) == arg_casted)
      && TREE_CODE (limit) == SSA_NAME)
    {
      wide_int min, max;
      value_range_type range_type = get_range_info (limit, &min, &max);

      if (range_type == VR_UNDEFINED || range_type == VR_VARYING)
	return alloca_type_and_limit (ALLOCA_BOUND_UNKNOWN);

      // ?? It looks like the above `if' is unnecessary, as we never
      // get any VR_RANGE or VR_ANTI_RANGE here.  If we had a range
      // for LIMIT, I suppose we would have taken care of it in
      // alloca_call_type(), or handled above where we handle (ARG .COND. N).
      //
      // If this ever triggers, we should probably figure out why and
      // handle it, though it is likely to be just an ALLOCA_UNBOUNDED.
      return alloca_type_and_limit (unbounded_result);
    }

  return alloca_type_and_limit (unbounded_result);
}

// Return TRUE if SSA's definition is a cast from a signed type.
// If so, set *INVALID_CASTED_TYPE to the signed type.

static bool
cast_from_signed_p (tree ssa, tree *invalid_casted_type)
{
  gimple *def = SSA_NAME_DEF_STMT (ssa);
  if (def
      && !gimple_nop_p (def)
      && gimple_assign_cast_p (def)
      && !TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (def))))
    {
      *invalid_casted_type = TREE_TYPE (gimple_assign_rhs1 (def));
      return true;
    }
  return false;
}

// Return TRUE if X has a maximum range of MAX, basically covering the
// entire domain, in which case it's no range at all.

static bool
is_max (tree x, wide_int max)
{
  return wi::max_value (TREE_TYPE (x)) == max;
}

// Analyze the alloca call in STMT and return the alloca type with its
// corresponding limit (if applicable).  IS_VLA is set if the alloca
// call was created by the gimplifier for a VLA.
//
// If the alloca call may be too large because of a cast from a signed
// type to an unsigned type, set *INVALID_CASTED_TYPE to the
// problematic signed type.

static struct alloca_type_and_limit
alloca_call_type (gimple *stmt, bool is_vla, tree *invalid_casted_type)
{
  gcc_assert (gimple_alloca_call_p (stmt));
  bool tentative_cast_from_signed = false;
  tree len = gimple_call_arg (stmt, 0);
  tree len_casted = NULL;
  wide_int min, max;
  edge_iterator ei;
  edge e;

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

  // Check the range info if available.
  if (TREE_CODE (len) == SSA_NAME)
    {
      value_range_type range_type = get_range_info (len, &min, &max);
      if (range_type == VR_RANGE)
	{
	  if (wi::leu_p (max, max_size))
	    return alloca_type_and_limit (ALLOCA_OK);
	  else
	    {
	      // A cast may have created a range we don't care
	      // about.  For instance, a cast from 16-bit to
	      // 32-bit creates a range of 0..65535, even if there
	      // is not really a determinable range in the
	      // underlying code.  In this case, look through the
	      // cast at the original argument, and fall through
	      // to look at other alternatives.
	      //
	      // We only look at through the cast when its from
	      // unsigned to unsigned, otherwise we may risk
	      // looking at SIGNED_INT < N, which is clearly not
	      // what we want.  In this case, we'd be interested
	      // in a VR_RANGE of [0..N].
	      //
	      // Note: None of this is perfect, and should all go
	      // away with better range information.  But it gets
	      // most of the cases.
	      gimple *def = SSA_NAME_DEF_STMT (len);
	      if (gimple_assign_cast_p (def))
		{
		  tree rhs1 = gimple_assign_rhs1 (def);
		  tree rhs1type = TREE_TYPE (rhs1);

		  // Bail if the argument type is not valid.
		  if (!INTEGRAL_TYPE_P (rhs1type))
		    return alloca_type_and_limit (ALLOCA_OK);

		  if (TYPE_UNSIGNED (rhs1type))
		    {
		      len_casted = rhs1;
		      range_type = get_range_info (len_casted, &min, &max);
		    }
		}
	      // An unknown range or a range of the entire domain is
	      // really no range at all.
	      if (range_type == VR_VARYING
		  || (!len_casted && is_max (len, max))
		  || (len_casted && is_max (len_casted, max)))
		{
		  // Fall through.
		}
	      else if (range_type == VR_ANTI_RANGE)
		return alloca_type_and_limit (ALLOCA_UNBOUNDED);

	      if (range_type != VR_VARYING)
		{
		  const offset_int maxobjsize
		    = wi::to_offset (max_object_size ());
		  alloca_type result = (max_size < maxobjsize
					? ALLOCA_BOUND_MAYBE_LARGE : ALLOCA_OK);
		  return alloca_type_and_limit (result, max);
		}
	    }
	}
      else if (range_type == VR_ANTI_RANGE)
	{
	  // There may be some wrapping around going on.  Catch it
	  // with this heuristic.  Hopefully, this VR_ANTI_RANGE
	  // nonsense will go away, and we won't have to catch the
	  // sign conversion problems with this crap.
	  //
	  // This is here to catch things like:
	  // void foo(signed int n) {
	  //   if (n < 100)
	  //     alloca(n);
	  //   ...
	  // }
	  if (cast_from_signed_p (len, invalid_casted_type))
	    {
	      // Unfortunately this also triggers:
	      //
	      // __SIZE_TYPE__ n = (__SIZE_TYPE__)blah;
	      // if (n < 100)
	      //   alloca(n);
	      //
	      // ...which is clearly bounded.  So, double check that
	      // the paths leading up to the size definitely don't
	      // have a bound.
	      tentative_cast_from_signed = true;
	    }
	}
      // No easily determined range and try other things.
    }

  // If we couldn't find anything, try a few heuristics for things we
  // can easily determine.  Check these misc cases but only accept
  // them if all predecessors have a known bound.
  struct alloca_type_and_limit ret = alloca_type_and_limit (ALLOCA_OK);
  FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->preds)
    {
      gcc_assert (!len_casted || TYPE_UNSIGNED (TREE_TYPE (len_casted)));
      ret = alloca_call_type_by_arg (len, len_casted, e, max_size);
      if (ret.type != ALLOCA_OK)
	break;
    }

  if (ret.type != ALLOCA_OK && tentative_cast_from_signed)
    ret = alloca_type_and_limit (ALLOCA_CAST_FROM_SIGNED);

  // If we have a declared maximum size, we can take it into account.
  if (ret.type != ALLOCA_OK
      && gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX))
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
    }

  return ret;
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
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  location_t loc = gimple_location (stmt);

	  if (!gimple_alloca_call_p (stmt))
	    continue;

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
	      warning_at (loc, OPT_Walloca, G_("use of %<alloca%>"));
	      continue;
	    }
	  else if (warn_alloca_limit < 0)
	    continue;

	  tree invalid_casted_type = NULL;
	  struct alloca_type_and_limit t
	    = alloca_call_type (stmt, is_vla, &invalid_casted_type);

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
				is_vla ? G_("argument to variable-length "
					    "array may be too large")
				: G_("argument to %<alloca%> may be too "
				     "large"))
		    && t.limit != 0)
		  {
		    print_decu (t.limit, buff);
		    inform (loc, G_("limit is %wu bytes, but argument "
				    "may be as large as %s"),
			    is_vla ? warn_vla_limit : adjusted_alloca_limit,
			    buff);
		  }
	      }
	      break;
	    case ALLOCA_BOUND_DEFINITELY_LARGE:
	      {
		auto_diagnostic_group d;
		if (warning_at (loc, wcode,
				is_vla ? G_("argument to variable-length"
					    " array is too large")
				: G_("argument to %<alloca%> is too large"))
		    && t.limit != 0)
		  {
		    print_decu (t.limit, buff);
		    inform (loc, G_("limit is %wu bytes, but argument is %s"),
			      is_vla ? warn_vla_limit : adjusted_alloca_limit,
			      buff);
		  }
	      }
	      break;
	    case ALLOCA_BOUND_UNKNOWN:
	      warning_at (loc, wcode,
			  is_vla ? G_("variable-length array bound is unknown")
			  : G_("%<alloca%> bound is unknown"));
	      break;
	    case ALLOCA_UNBOUNDED:
	      warning_at (loc, wcode,
			  is_vla ? G_("unbounded use of variable-length array")
			  : G_("unbounded use of %<alloca%>"));
	      break;
	    case ALLOCA_IN_LOOP:
	      gcc_assert (!is_vla);
	      warning_at (loc, wcode, G_("use of %<alloca%> within a loop"));
	      break;
	    case ALLOCA_CAST_FROM_SIGNED:
	      gcc_assert (invalid_casted_type != NULL_TREE);
	      warning_at (loc, wcode,
			  is_vla ? G_("argument to variable-length array "
				      "may be too large due to "
				      "conversion from %qT to %qT")
			  : G_("argument to %<alloca%> may be too large "
			       "due to conversion from %qT to %qT"),
			  invalid_casted_type, size_type_node);
	      break;
	    case ALLOCA_ARG_IS_ZERO:
	      warning_at (loc, wcode,
			  is_vla ? G_("argument to variable-length array "
				      "is zero")
			  : G_("argument to %<alloca%> is zero"));
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
