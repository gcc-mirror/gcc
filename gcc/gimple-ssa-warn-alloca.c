/* Warn on problematic uses of alloca and variable length arrays.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
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
#include "calls.h"
#include "cfgloop.h"
#include "intl.h"

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

  return ((unsigned HOST_WIDE_INT) warn_alloca_limit > 0
	  || (unsigned HOST_WIDE_INT) warn_vla_limit > 0);
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
  alloca_type_and_limit (enum alloca_type type) : type(type) { }
};

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
alloca_call_type_by_arg (tree arg, tree arg_casted, edge e, unsigned max_size)
{
  basic_block bb = e->src;
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gimple *last = gsi_stmt (gsi);
  if (!last || gimple_code (last) != GIMPLE_COND)
    return alloca_type_and_limit (ALLOCA_UNBOUNDED);

  enum tree_code cond_code = gimple_cond_code (last);
  if (e->flags & EDGE_TRUE_VALUE)
    ;
  else if (e->flags & EDGE_FALSE_VALUE)
    cond_code = invert_tree_comparison (cond_code, false);
  else
    return alloca_type_and_limit (ALLOCA_UNBOUNDED);

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
	      return alloca_type_and_limit (ALLOCA_BOUND_MAYBE_LARGE, rhs);
	    }
	}
      else
	return alloca_type_and_limit (ALLOCA_BOUND_UNKNOWN);
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

      if (!get_range_info (limit, &min, &max))
	return alloca_type_and_limit (ALLOCA_BOUND_UNKNOWN);

      // ?? It looks like the above `if' is unnecessary, as we never
      // get any range information here.  If we had a range
      // for LIMIT, I suppose we would have taken care of it in
      // alloca_call_type(), or handled above where we handle (ARG .COND. N).
      //
      // If this ever triggers, we should probably figure out why and
      // handle it, though it is likely to be just an ALLOCA_UNBOUNDED.
      return alloca_type_and_limit (ALLOCA_UNBOUNDED);
    }

  return alloca_type_and_limit (ALLOCA_UNBOUNDED);
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

// Return TRUE if TYPE has a maximum range of MAX.

static bool
is_max (tree type, wide_int max)
{
  return wi::max_value (type) == max;
}

// Analyze the alloca call in STMT and return the alloca type with its
// corresponding limit (if applicable).  IS_VLA is set if the alloca
// call is really a BUILT_IN_ALLOCA_WITH_ALIGN, signifying a VLA.
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
  struct alloca_type_and_limit ret = alloca_type_and_limit (ALLOCA_UNBOUNDED);

  gcc_assert (!is_vla || (unsigned HOST_WIDE_INT) warn_vla_limit > 0);
  gcc_assert (is_vla || (unsigned HOST_WIDE_INT) warn_alloca_limit > 0);

  // Adjust warn_alloca_max_size for VLAs, by taking the underlying
  // type into account.
  unsigned HOST_WIDE_INT max_user_size;
  if (is_vla)
    max_user_size = (unsigned HOST_WIDE_INT) warn_vla_limit;
  else
    max_user_size = (unsigned HOST_WIDE_INT) warn_alloca_limit;

  // Check for the obviously bounded case.
  if (TREE_CODE (len) == INTEGER_CST)
    {
      if (tree_to_uhwi (len) > max_user_size)
	return alloca_type_and_limit (ALLOCA_BOUND_DEFINITELY_LARGE, len);
      if (integer_zerop (len))
	return alloca_type_and_limit (ALLOCA_ARG_IS_ZERO);
      ret = alloca_type_and_limit (ALLOCA_OK);
    }
  // Check the range info if available.
  else if (TREE_CODE (len) == SSA_NAME && get_range_info (len, &min, &max))
    {
      irange r (len);
      if (wi::leu_p (max, max_user_size))
	ret = alloca_type_and_limit (ALLOCA_OK);
      else if (is_max (TREE_TYPE (len), max)
	       && !r.range_for_type_p ()
	       && cast_from_signed_p (len, invalid_casted_type))
	{
	  // A cast from signed to unsigned may cause us to have very
	  // large numbers that can be caught with the above
	  // heuristic.
	  //
	  // This is here to catch things like:
	  // void foo(signed int n) {
	  //   if (n < 100)
	  //     {
	  //       # RANGE [0,99][0xff80, 0xffff]
	  //       unsigned int _1 = (unsigned int) n;
	  //       alloca (_1);
	  //     }
	  //   ...
	  // }
	  //
	  // Unfortunately it also triggers:
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
	  // We only look through the cast when it's from unsigned to
	  // unsigned, otherwise we risk looking at SIGNED_INT < N,
	  // which is clearly not what we want.  In this case, we'd be
	  // interested in a VR_RANGE of [0..N].
	  //
	  // Note: None of this is perfect, and should all go
	  // away with better range information.  But it gets
	  // most of the cases.
	  gimple *def = SSA_NAME_DEF_STMT (len);
	  bool have_range = true;
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
		  have_range = get_range_info (len_casted, &min, &max);
		}
	    }
	  // An unknown range or a range of the entire domain is
	  // really no range at all.
	  if (!have_range
	      || (!len_casted && is_max (TREE_TYPE (len), max))
	      || (len_casted && is_max (TREE_TYPE (len_casted), max)))
	    {
	      // Fall through.
	    }
	  else
	    return alloca_type_and_limit (ALLOCA_BOUND_MAYBE_LARGE, max);
	}
      // No easily determined range and try other things.
    }

  // If we couldn't find anything, try a few heuristics for things we
  // can easily determine.  Check these misc cases but only accept
  // them if all predecessors have a known bound.
  basic_block bb = gimple_bb (stmt);
  if (ret.type == ALLOCA_UNBOUNDED)
    {
      ret.type = ALLOCA_OK;
      for (unsigned ix = 0; ix < EDGE_COUNT (bb->preds); ix++)
	{
	  gcc_assert (!len_casted || TYPE_UNSIGNED (TREE_TYPE (len_casted)));
	  ret = alloca_call_type_by_arg (len, len_casted,
					 EDGE_PRED (bb, ix), max_user_size);
	  if (ret.type != ALLOCA_OK)
	    break;
	}
    }

  if (tentative_cast_from_signed && ret.type != ALLOCA_OK)
    return alloca_type_and_limit (ALLOCA_CAST_FROM_SIGNED);
  return ret;
}

// Return TRUE if the alloca call in STMT is in a loop, otherwise
// return FALSE. As an exception, ignore alloca calls for VLAs that
// occur in a loop since those will be cleaned up when they go out of
// scope.

static bool
in_loop_p (bool is_vla, gimple *stmt)
{
  basic_block bb = gimple_bb (stmt);
  if (bb->loop_father
      && bb->loop_father->header != ENTRY_BLOCK_PTR_FOR_FN (cfun))
    {
      // Do not warn on VLAs occurring in a loop, since VLAs are
      // guaranteed to be cleaned up when they go out of scope.
      // That is, there is a corresponding __builtin_stack_restore
      // at the end of the scope in which the VLA occurs.
      tree fndecl = gimple_call_fn (stmt);
      while (TREE_CODE (fndecl) == ADDR_EXPR)
	fndecl = TREE_OPERAND (fndecl, 0);
      if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	  && is_vla
	  && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_ALLOCA_WITH_ALIGN)
	return false;

      return true;
    }
  return false;
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
	  gcc_assert (gimple_call_num_args (stmt) >= 1);

	  bool is_vla = gimple_alloca_call_p (stmt)
	    && gimple_call_alloca_for_var_p (as_a <gcall *> (stmt));

	  // Strict mode whining for VLAs is handled by the front-end,
	  // so we can safely ignore this case.  Also, ignore VLAs if
	  // the user doesn't care about them.
	  if (is_vla
	      && (warn_vla > 0 || !warn_vla_limit))
	    continue;

	  if (!is_vla && (warn_alloca || !warn_alloca_limit))
	    {
	      if (warn_alloca)
		warning_at (loc, OPT_Walloca, G_("use of %<alloca%>"));
	      continue;
	    }

	  tree invalid_casted_type = NULL;
	  struct alloca_type_and_limit t
	    = alloca_call_type (stmt, is_vla, &invalid_casted_type);

	  // Even if we think the alloca call is OK, make sure it's
	  // not in a loop.
	  if (t.type == ALLOCA_OK && in_loop_p (is_vla, stmt))
	    t = alloca_type_and_limit (ALLOCA_IN_LOOP);

	  enum opt_code wcode
	    = is_vla ? OPT_Wvla_larger_than_ : OPT_Walloca_larger_than_;
	  char buff[WIDE_INT_MAX_PRECISION / 4 + 4];
	  switch (t.type)
	    {
	    case ALLOCA_OK:
	      break;
	    case ALLOCA_BOUND_MAYBE_LARGE:
	      if (warning_at (loc, wcode,
			      is_vla ? G_("argument to variable-length array "
					  "may be too large")
			      : G_("argument to %<alloca%> may be too large"))
		  && t.limit != integer_zero_node)
		{
		  print_decu (t.limit, buff);
		  inform (loc, G_("limit is %u bytes, but argument "
				  "may be as large as %s"),
			  is_vla ? warn_vla_limit : warn_alloca_limit, buff);
		}
	      break;
	    case ALLOCA_BOUND_DEFINITELY_LARGE:
	      if (warning_at (loc, wcode,
			      is_vla ? G_("argument to variable-length array "
					  "is too large")
			      : G_("argument to %<alloca%> is too large"))
		  && t.limit != integer_zero_node)
		{
		  print_decu (t.limit, buff);
		  inform (loc, G_("limit is %u bytes, but argument is %s"),
			  is_vla ? warn_vla_limit : warn_alloca_limit, buff);
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
