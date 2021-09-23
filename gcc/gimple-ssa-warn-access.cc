/* Pass to detect and issue warnings for invalid accesses, including
   invalid or mismatched allocation/deallocation calls.

   Copyright (C) 2020-2021 Free Software Foundation, Inc.
   Contributed by Martin Sebor <msebor@redhat.com>.

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

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "builtins.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-ssa-warn-access.h"
#include "gimple-ssa-warn-restrict.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "gimple-fold.h"
#include "gimple-iterator.h"
#include "langhooks.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-cfg.h"
#include "tree-object-size.h"
#include "tree-ssa-strlen.h"
#include "calls.h"
#include "cfgloop.h"
#include "intl.h"
#include "gimple-range.h"
#include "stringpool.h"
#include "attribs.h"
#include "demangle.h"
#include "pointer-query.h"

/* Return true if tree node X has an associated location.  */

static inline location_t
has_location (const_tree x)
{
  if (DECL_P (x))
    return DECL_SOURCE_LOCATION (x) != UNKNOWN_LOCATION;

  if (EXPR_P (x))
    return EXPR_HAS_LOCATION (x);

  return false;
}

/* Return the associated location of STMT.  */

static inline location_t
get_location (const gimple *stmt)
{
  return gimple_location (stmt);
}

/* Return the associated location of tree node X.  */

static inline location_t
get_location (tree x)
{
  if (DECL_P (x))
    return DECL_SOURCE_LOCATION (x);

  if (EXPR_P (x))
    return EXPR_LOCATION (x);

  return UNKNOWN_LOCATION;
}

/* Overload of the nascent tree function for GIMPLE STMT.  */

static inline tree
get_callee_fndecl (const gimple *stmt)
{
  return gimple_call_fndecl (stmt);
}

static inline unsigned
call_nargs (const gimple *stmt)
{
  return gimple_call_num_args (stmt);
}

static inline unsigned
call_nargs (const_tree expr)
{
  return call_expr_nargs (expr);
}


static inline tree
call_arg (const gimple *stmt, unsigned argno)
{
  return gimple_call_arg (stmt, argno);
}

static inline tree
call_arg (tree expr, unsigned argno)
{
  return CALL_EXPR_ARG (expr, argno);
}

/* For a call EXPR at LOC to a function FNAME that expects a string
   in the argument ARG, issue a diagnostic due to it being a called
   with an argument that is a character array with no terminating
   NUL.  SIZE is the EXACT size of the array, and BNDRNG the number
   of characters in which the NUL is expected.  Either EXPR or FNAME
   may be null but noth both.  SIZE may be null when BNDRNG is null.  */

template <class GimpleOrTree>
static void
warn_string_no_nul (location_t loc, GimpleOrTree expr, const char *fname,
		    tree arg, tree decl, tree size, bool exact,
		    const wide_int bndrng[2] /* = NULL */)
{
  const opt_code opt = OPT_Wstringop_overread;
  if ((expr && warning_suppressed_p (expr, opt))
      || warning_suppressed_p (arg, opt))
    return;

  loc = expansion_point_location_if_in_system_header (loc);
  bool warned;

  /* Format the bound range as a string to keep the nuber of messages
     from exploding.  */
  char bndstr[80];
  *bndstr = 0;
  if (bndrng)
    {
      if (bndrng[0] == bndrng[1])
	sprintf (bndstr, "%llu", (unsigned long long) bndrng[0].to_uhwi ());
      else
	sprintf (bndstr, "[%llu, %llu]",
		 (unsigned long long) bndrng[0].to_uhwi (),
		 (unsigned long long) bndrng[1].to_uhwi ());
    }

  const tree maxobjsize = max_object_size ();
  const wide_int maxsiz = wi::to_wide (maxobjsize);
  if (expr)
    {
      tree func = get_callee_fndecl (expr);
      if (bndrng)
	{
	  if (wi::ltu_p (maxsiz, bndrng[0]))
	    warned = warning_at (loc, opt,
				 "%qD specified bound %s exceeds "
				 "maximum object size %E",
				 func, bndstr, maxobjsize);
	  else
	    {
	      bool maybe = wi::to_wide (size) == bndrng[0];
	      warned = warning_at (loc, opt,
				   exact
				   ? G_("%qD specified bound %s exceeds "
					"the size %E of unterminated array")
				   : (maybe
				      ? G_("%qD specified bound %s may "
					   "exceed the size of at most %E "
					   "of unterminated array")
				      : G_("%qD specified bound %s exceeds "
					   "the size of at most %E "
					   "of unterminated array")),
				   func, bndstr, size);
	    }
	}
      else
	warned = warning_at (loc, opt,
			     "%qD argument missing terminating nul",
			     func);
    }
  else
    {
      if (bndrng)
	{
	  if (wi::ltu_p (maxsiz, bndrng[0]))
	    warned = warning_at (loc, opt,
				 "%qs specified bound %s exceeds "
				 "maximum object size %E",
				 fname, bndstr, maxobjsize);
	  else
	    {
	      bool maybe = wi::to_wide (size) == bndrng[0];
	      warned = warning_at (loc, opt,
				   exact
				   ? G_("%qs specified bound %s exceeds "
					"the size %E of unterminated array")
				   : (maybe
				      ? G_("%qs specified bound %s may "
					   "exceed the size of at most %E "
					   "of unterminated array")
				      : G_("%qs specified bound %s exceeds "
					   "the size of at most %E "
					   "of unterminated array")),
				   fname, bndstr, size);
	    }
	}
      else
	warned = warning_at (loc, opt,
			     "%qs argument missing terminating nul",
			     fname);
    }

  if (warned)
    {
      inform (get_location (decl),
	      "referenced argument declared here");
      suppress_warning (arg, opt);
      if (expr)
	suppress_warning (expr, opt);
    }
}

void
warn_string_no_nul (location_t loc, gimple *stmt, const char *fname,
		    tree arg, tree decl, tree size /* = NULL_TREE */,
		    bool exact /* = false */,
		    const wide_int bndrng[2] /* = NULL */)
{
  return warn_string_no_nul<gimple *> (loc, stmt, fname,
				       arg, decl, size, exact, bndrng);
}

void
warn_string_no_nul (location_t loc, tree expr, const char *fname,
		    tree arg, tree decl, tree size /* = NULL_TREE */,
		    bool exact /* = false */,
		    const wide_int bndrng[2] /* = NULL */)
{
  return warn_string_no_nul<tree> (loc, expr, fname,
				   arg, decl, size, exact, bndrng);
}

/* If EXP refers to an unterminated constant character array return
   the declaration of the object of which the array is a member or
   element and if SIZE is not null, set *SIZE to the size of
   the unterminated array and set *EXACT if the size is exact or
   clear it otherwise.  Otherwise return null.  */

tree
unterminated_array (tree exp, tree *size /* = NULL */, bool *exact /* = NULL */)
{
  /* C_STRLEN will return NULL and set DECL in the info
     structure if EXP references a unterminated array.  */
  c_strlen_data lendata = { };
  tree len = c_strlen (exp, 1, &lendata);
  if (len || !lendata.minlen || !lendata.decl)
    return NULL_TREE;

  if (!size)
    return lendata.decl;

  len = lendata.minlen;
  if (lendata.off)
    {
      /* Constant offsets are already accounted for in LENDATA.MINLEN,
	 but not in a SSA_NAME + CST expression.  */
      if (TREE_CODE (lendata.off) == INTEGER_CST)
	*exact = true;
      else if (TREE_CODE (lendata.off) == PLUS_EXPR
	       && TREE_CODE (TREE_OPERAND (lendata.off, 1)) == INTEGER_CST)
	{
	  /* Subtract the offset from the size of the array.  */
	  *exact = false;
	  tree temp = TREE_OPERAND (lendata.off, 1);
	  temp = fold_convert (ssizetype, temp);
	  len = fold_build2 (MINUS_EXPR, ssizetype, len, temp);
	}
      else
	*exact = false;
    }
  else
    *exact = true;

  *size = len;
  return lendata.decl;
}

/* For a call EXPR (which may be null) that expects a string argument
   SRC as an argument, returns false if SRC is a character array with
   no terminating NUL.  When nonnull, BOUND is the number of characters
   in which to expect the terminating NUL.  When EXPR is nonnull also
   issues a warning.  */

template <class GimpleOrTree>
static bool
check_nul_terminated_array (GimpleOrTree expr, tree src, tree bound)
{
  /* The constant size of the array SRC points to.  The actual size
     may be less of EXACT is true, but not more.  */
  tree size;
  /* True if SRC involves a non-constant offset into the array.  */
  bool exact;
  /* The unterminated constant array SRC points to.  */
  tree nonstr = unterminated_array (src, &size, &exact);
  if (!nonstr)
    return true;

  /* NONSTR refers to the non-nul terminated constant array and SIZE
     is the constant size of the array in bytes.  EXACT is true when
     SIZE is exact.  */

  wide_int bndrng[2];
  if (bound)
    {
      value_range r;

      get_global_range_query ()->range_of_expr (r, bound);

      if (r.kind () != VR_RANGE)
	return true;

      bndrng[0] = r.lower_bound ();
      bndrng[1] = r.upper_bound ();

      if (exact)
	{
	  if (wi::leu_p (bndrng[0], wi::to_wide (size)))
	    return true;
	}
      else if (wi::lt_p (bndrng[0], wi::to_wide (size), UNSIGNED))
	return true;
    }

  if (expr)
    warn_string_no_nul (get_location (expr), expr, NULL, src, nonstr,
			size, exact, bound ? bndrng : NULL);

  return false;
}

bool
check_nul_terminated_array (gimple *stmt, tree src, tree bound /* = NULL_TREE */)
{
  return check_nul_terminated_array<gimple *>(stmt, src, bound);
}

bool
check_nul_terminated_array (tree expr, tree src, tree bound /* = NULL_TREE */)
{
  return check_nul_terminated_array<tree>(expr, src, bound);
}

/* Warn about passing a non-string array/pointer to a built-in function
   that expects a nul-terminated string argument.  Returns true if
   a warning has been issued.*/

template <class GimpleOrTree>
static bool
maybe_warn_nonstring_arg (tree fndecl, GimpleOrTree exp)
{
  if (!fndecl || !fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    return false;

  if (!warn_stringop_overread
      || warning_suppressed_p (exp, OPT_Wstringop_overread))
    return false;

  /* Avoid clearly invalid calls (more checking done below).  */
  unsigned nargs = call_nargs (exp);
  if (!nargs)
    return false;

  /* The bound argument to a bounded string function like strncpy.  */
  tree bound = NULL_TREE;

  /* The longest known or possible string argument to one of the comparison
     functions.  If the length is less than the bound it is used instead.
     Since the length is only used for warning and not for code generation
     disable strict mode in the calls to get_range_strlen below.  */
  tree maxlen = NULL_TREE;

  /* It's safe to call "bounded" string functions with a non-string
     argument since the functions provide an explicit bound for this
     purpose.  The exception is strncat where the bound may refer to
     either the destination or the source.  */
  int fncode = DECL_FUNCTION_CODE (fndecl);
  switch (fncode)
    {
    case BUILT_IN_STRCMP:
    case BUILT_IN_STRNCMP:
    case BUILT_IN_STRNCASECMP:
      {
	/* For these, if one argument refers to one or more of a set
	   of string constants or arrays of known size, determine
	   the range of their known or possible lengths and use it
	   conservatively as the bound for the unbounded function,
	   and to adjust the range of the bound of the bounded ones.  */
	for (unsigned argno = 0;
	     argno < MIN (nargs, 2)
	       && !(maxlen && TREE_CODE (maxlen) == INTEGER_CST); argno++)
	  {
	    tree arg = call_arg (exp, argno);
	    if (!get_attr_nonstring_decl (arg))
	      {
		c_strlen_data lendata = { };
		/* Set MAXBOUND to an arbitrary non-null non-integer
		   node as a request to have it set to the length of
		   the longest string in a PHI.  */
		lendata.maxbound = arg;
		get_range_strlen (arg, &lendata, /* eltsize = */ 1);
		maxlen = lendata.maxbound;
	      }
	  }
      }
      /* Fall through.  */

    case BUILT_IN_STRNCAT:
    case BUILT_IN_STPNCPY:
    case BUILT_IN_STRNCPY:
      if (nargs > 2)
	bound = call_arg (exp, 2);
      break;

    case BUILT_IN_STRNDUP:
      if (nargs < 2)
	return false;
      bound = call_arg (exp, 1);
      break;

    case BUILT_IN_STRNLEN:
      {
	tree arg = call_arg (exp, 0);
	if (!get_attr_nonstring_decl (arg))
	  {
	    c_strlen_data lendata = { };
	    /* Set MAXBOUND to an arbitrary non-null non-integer
	       node as a request to have it set to the length of
	       the longest string in a PHI.  */
	    lendata.maxbound = arg;
	    get_range_strlen (arg, &lendata, /* eltsize = */ 1);
	    maxlen = lendata.maxbound;
	  }
	if (nargs > 1)
	  bound = call_arg (exp, 1);
	break;
      }

    default:
      break;
    }

  /* Determine the range of the bound argument (if specified).  */
  tree bndrng[2] = { NULL_TREE, NULL_TREE };
  if (bound)
    {
      STRIP_NOPS (bound);
      get_size_range (bound, bndrng);
    }

  location_t loc = get_location (exp);

  if (bndrng[0])
    {
      /* Diagnose excessive bound prior to the adjustment below and
	 regardless of attribute nonstring.  */
      tree maxobjsize = max_object_size ();
      if (tree_int_cst_lt (maxobjsize, bndrng[0]))
	{
	  bool warned = false;
	  if (tree_int_cst_equal (bndrng[0], bndrng[1]))
	    warned = warning_at (loc, OPT_Wstringop_overread,
				 "%qD specified bound %E "
				 "exceeds maximum object size %E",
				 fndecl, bndrng[0], maxobjsize);
	  else
	    warned = warning_at (loc, OPT_Wstringop_overread,
				 "%qD specified bound [%E, %E] "
				 "exceeds maximum object size %E",
				 fndecl, bndrng[0], bndrng[1],
				 maxobjsize);
	  if (warned)
	    suppress_warning (exp, OPT_Wstringop_overread);

	  return warned;
	}
    }

  if (maxlen && !integer_all_onesp (maxlen))
    {
      /* Add one for the nul.  */
      maxlen = const_binop (PLUS_EXPR, TREE_TYPE (maxlen), maxlen,
			    size_one_node);

      if (!bndrng[0])
	{
	  /* Conservatively use the upper bound of the lengths for
	     both the lower and the upper bound of the operation.  */
	  bndrng[0] = maxlen;
	  bndrng[1] = maxlen;
	  bound = void_type_node;
	}
      else if (maxlen)
	{
	  /* Replace the bound on the operation with the upper bound
	     of the length of the string if the latter is smaller.  */
	  if (tree_int_cst_lt (maxlen, bndrng[0]))
	    bndrng[0] = maxlen;
	  else if (tree_int_cst_lt (maxlen, bndrng[1]))
	    bndrng[1] = maxlen;
	}
    }

  bool any_arg_warned = false;
  /* Iterate over the built-in function's formal arguments and check
     each const char* against the actual argument.  If the actual
     argument is declared attribute non-string issue a warning unless
     the argument's maximum length is bounded.  */
  function_args_iterator it;
  function_args_iter_init (&it, TREE_TYPE (fndecl));

  for (unsigned argno = 0; ; ++argno, function_args_iter_next (&it))
    {
      /* Avoid iterating past the declared argument in a call
	 to function declared without a prototype.  */
      if (argno >= nargs)
	break;

      tree argtype = function_args_iter_cond (&it);
      if (!argtype)
	break;

      if (TREE_CODE (argtype) != POINTER_TYPE)
	continue;

      argtype = TREE_TYPE (argtype);

      if (TREE_CODE (argtype) != INTEGER_TYPE
	  || !TYPE_READONLY (argtype))
	continue;

      argtype = TYPE_MAIN_VARIANT (argtype);
      if (argtype != char_type_node)
	continue;

      tree callarg = call_arg (exp, argno);
      if (TREE_CODE (callarg) == ADDR_EXPR)
	callarg = TREE_OPERAND (callarg, 0);

      /* See if the destination is declared with attribute "nonstring".  */
      tree decl = get_attr_nonstring_decl (callarg);
      if (!decl)
	continue;

      /* The maximum number of array elements accessed.  */
      offset_int wibnd = 0;

      if (argno && fncode == BUILT_IN_STRNCAT)
	{
	  /* See if the bound in strncat is derived from the length
	     of the strlen of the destination (as it's expected to be).
	     If so, reset BOUND and FNCODE to trigger a warning.  */
	  tree dstarg = call_arg (exp, 0);
	  if (is_strlen_related_p (dstarg, bound))
	    {
	      /* The bound applies to the destination, not to the source,
		 so reset these to trigger a warning without mentioning
		 the bound.  */
	      bound = NULL;
	      fncode = 0;
	    }
	  else if (bndrng[1])
	    /* Use the upper bound of the range for strncat.  */
	    wibnd = wi::to_offset (bndrng[1]);
	}
      else if (bndrng[0])
	/* Use the lower bound of the range for functions other than
	   strncat.  */
	wibnd = wi::to_offset (bndrng[0]);

      /* Determine the size of the argument array if it is one.  */
      offset_int asize = wibnd;
      bool known_size = false;
      tree type = TREE_TYPE (decl);

      /* Determine the array size.  For arrays of unknown bound and
	 pointers reset BOUND to trigger the appropriate warning.  */
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (tree arrbnd = TYPE_DOMAIN (type))
	    {
	      if ((arrbnd = TYPE_MAX_VALUE (arrbnd)))
		{
		  asize = wi::to_offset (arrbnd) + 1;
		  known_size = true;
		}
	    }
	  else if (bound == void_type_node)
	    bound = NULL_TREE;
	}
      else if (bound == void_type_node)
	bound = NULL_TREE;

      /* In a call to strncat with a bound in a range whose lower but
	 not upper bound is less than the array size, reset ASIZE to
	 be the same as the bound and the other variable to trigger
	 the apprpriate warning below.  */
      if (fncode == BUILT_IN_STRNCAT
	  && bndrng[0] != bndrng[1]
	  && wi::ltu_p (wi::to_offset (bndrng[0]), asize)
	  && (!known_size
	      || wi::ltu_p (asize, wibnd)))
	{
	  asize = wibnd;
	  bound = NULL_TREE;
	  fncode = 0;
	}

      bool warned = false;

      auto_diagnostic_group d;
      if (wi::ltu_p (asize, wibnd))
	{
	  if (bndrng[0] == bndrng[1])
	    warned = warning_at (loc, OPT_Wstringop_overread,
				 "%qD argument %i declared attribute "
				 "%<nonstring%> is smaller than the specified "
				 "bound %wu",
				 fndecl, argno + 1, wibnd.to_uhwi ());
	  else if (wi::ltu_p (asize, wi::to_offset (bndrng[0])))
	    warned = warning_at (loc, OPT_Wstringop_overread,
				 "%qD argument %i declared attribute "
				 "%<nonstring%> is smaller than "
				 "the specified bound [%E, %E]",
				 fndecl, argno + 1, bndrng[0], bndrng[1]);
	  else
	    warned = warning_at (loc, OPT_Wstringop_overread,
				 "%qD argument %i declared attribute "
				 "%<nonstring%> may be smaller than "
				 "the specified bound [%E, %E]",
				 fndecl, argno + 1, bndrng[0], bndrng[1]);
	}
      else if (fncode == BUILT_IN_STRNCAT)
	; /* Avoid warning for calls to strncat() when the bound
	     is equal to the size of the non-string argument.  */
      else if (!bound)
	warned = warning_at (loc, OPT_Wstringop_overread,
			     "%qD argument %i declared attribute %<nonstring%>",
			     fndecl, argno + 1);

      if (warned)
	{
	  inform (DECL_SOURCE_LOCATION (decl),
		  "argument %qD declared here", decl);
	  any_arg_warned = true;
	}
    }

  if (any_arg_warned)
    suppress_warning (exp, OPT_Wstringop_overread);

  return any_arg_warned;
}

bool
maybe_warn_nonstring_arg (tree fndecl, gimple *stmt)
{
  return maybe_warn_nonstring_arg<gimple *>(fndecl, stmt);
}


bool
maybe_warn_nonstring_arg (tree fndecl, tree expr)
{
  return maybe_warn_nonstring_arg<tree>(fndecl, expr);
}

/* Issue a warning OPT for a bounded call EXP with a bound in RANGE
   accessing an object with SIZE.  */

template <class GimpleOrTree>
static bool
maybe_warn_for_bound (opt_code opt, location_t loc, GimpleOrTree exp, tree func,
		      tree bndrng[2], tree size, const access_data *pad)
{
  if (!bndrng[0] || warning_suppressed_p (exp, opt))
    return false;

  tree maxobjsize = max_object_size ();

  bool warned = false;

  if (opt == OPT_Wstringop_overread)
    {
      bool maybe = pad && pad->src.phi ();
      if (maybe)
	{
	  /* Issue a "maybe" warning only if the PHI refers to objects
	     at least one of which has more space remaining than the bound.
	     Otherwise, if the bound is greater, use the definitive form.  */
	  offset_int remmax = pad->src.size_remaining ();
	  if (remmax < wi::to_offset (bndrng[0]))
	    maybe = false;
	}

      if (tree_int_cst_lt (maxobjsize, bndrng[0]))
	{
	  if (bndrng[0] == bndrng[1])
	    warned = (func
		      ? warning_at (loc, opt,
				    (maybe
				     ? G_("%qD specified bound %E may "
					  "exceed maximum object size %E")
				     : G_("%qD specified bound %E "
					  "exceeds maximum object size %E")),
				    func, bndrng[0], maxobjsize)
		      : warning_at (loc, opt,
				    (maybe
				     ? G_("specified bound %E may "
					  "exceed maximum object size %E")
				     : G_("specified bound %E "
					  "exceeds maximum object size %E")),
				    bndrng[0], maxobjsize));
	  else
	    warned = (func
		      ? warning_at (loc, opt,
				    (maybe
				     ? G_("%qD specified bound [%E, %E] may "
					  "exceed maximum object size %E")
				     : G_("%qD specified bound [%E, %E] "
					  "exceeds maximum object size %E")),
				    func,
				    bndrng[0], bndrng[1], maxobjsize)
		      : warning_at (loc, opt,
				    (maybe
				     ? G_("specified bound [%E, %E] may "
					  "exceed maximum object size %E")
				     : G_("specified bound [%E, %E] "
					  "exceeds maximum object size %E")),
				    bndrng[0], bndrng[1], maxobjsize));
	}
      else if (!size || tree_int_cst_le (bndrng[0], size))
	return false;
      else if (tree_int_cst_equal (bndrng[0], bndrng[1]))
	warned = (func
		  ? warning_at (loc, opt,
				(maybe
				 ? G_("%qD specified bound %E may exceed "
				      "source size %E")
				 : G_("%qD specified bound %E exceeds "
				      "source size %E")),
				func, bndrng[0], size)
		  : warning_at (loc, opt,
				(maybe
				 ? G_("specified bound %E may exceed "
				      "source size %E")
				 : G_("specified bound %E exceeds "
				      "source size %E")),
				bndrng[0], size));
      else
	warned = (func
		  ? warning_at (loc, opt,
				(maybe
				 ? G_("%qD specified bound [%E, %E] may "
				      "exceed source size %E")
				 : G_("%qD specified bound [%E, %E] exceeds "
				      "source size %E")),
				func, bndrng[0], bndrng[1], size)
		  : warning_at (loc, opt,
				(maybe
				 ? G_("specified bound [%E, %E] may exceed "
				      "source size %E")
				 : G_("specified bound [%E, %E] exceeds "
				      "source size %E")),
				bndrng[0], bndrng[1], size));
      if (warned)
	{
	  if (pad && pad->src.ref
	      && has_location (pad->src.ref))
	    inform (get_location (pad->src.ref),
		    "source object allocated here");
	  suppress_warning (exp, opt);
	}

      return warned;
    }

  bool maybe = pad && pad->dst.phi ();
  if (maybe)
    {
      /* Issue a "maybe" warning only if the PHI refers to objects
	 at least one of which has more space remaining than the bound.
	 Otherwise, if the bound is greater, use the definitive form.  */
      offset_int remmax = pad->dst.size_remaining ();
      if (remmax < wi::to_offset (bndrng[0]))
	maybe = false;
    }
  if (tree_int_cst_lt (maxobjsize, bndrng[0]))
    {
      if (bndrng[0] == bndrng[1])
	warned = (func
		  ? warning_at (loc, opt,
				(maybe
				 ? G_("%qD specified size %E may "
				      "exceed maximum object size %E")
				 : G_("%qD specified size %E "
				      "exceeds maximum object size %E")),
				func, bndrng[0], maxobjsize)
		  : warning_at (loc, opt,
				(maybe
				 ? G_("specified size %E may exceed "
				      "maximum object size %E")
				 : G_("specified size %E exceeds "
				      "maximum object size %E")),
				bndrng[0], maxobjsize));
      else
	warned = (func
		  ? warning_at (loc, opt,
				(maybe
				 ? G_("%qD specified size between %E and %E "
				      "may exceed maximum object size %E")
				 : G_("%qD specified size between %E and %E "
				      "exceeds maximum object size %E")),
				func, bndrng[0], bndrng[1], maxobjsize)
		  : warning_at (loc, opt,
				(maybe
				 ? G_("specified size between %E and %E "
				      "may exceed maximum object size %E")
				 : G_("specified size between %E and %E "
				      "exceeds maximum object size %E")),
				bndrng[0], bndrng[1], maxobjsize));
    }
  else if (!size || tree_int_cst_le (bndrng[0], size))
    return false;
  else if (tree_int_cst_equal (bndrng[0], bndrng[1]))
    warned = (func
	      ? warning_at (loc, opt,
			    (maybe
			     ? G_("%qD specified bound %E may exceed "
				  "destination size %E")
			     : G_("%qD specified bound %E exceeds "
				  "destination size %E")),
			    func, bndrng[0], size)
	      : warning_at (loc, opt,
			    (maybe
			     ? G_("specified bound %E may exceed "
				  "destination size %E")
			     : G_("specified bound %E exceeds "
				  "destination size %E")),
			    bndrng[0], size));
  else
    warned = (func
	      ? warning_at (loc, opt,
			    (maybe
			     ? G_("%qD specified bound [%E, %E] may exceed "
				  "destination size %E")
			     : G_("%qD specified bound [%E, %E] exceeds "
				  "destination size %E")),
			    func, bndrng[0], bndrng[1], size)
	      : warning_at (loc, opt,
			    (maybe
			     ? G_("specified bound [%E, %E] exceeds "
				  "destination size %E")
			     : G_("specified bound [%E, %E] exceeds "
				  "destination size %E")),
			    bndrng[0], bndrng[1], size));

  if (warned)
    {
      if (pad && pad->dst.ref
	  && has_location (pad->dst.ref))
	inform (get_location (pad->dst.ref),
		"destination object allocated here");
      suppress_warning (exp, opt);
    }

  return warned;
}

bool
maybe_warn_for_bound (opt_code opt, location_t loc, gimple *stmt, tree func,
		      tree bndrng[2], tree size,
		      const access_data *pad /* = NULL */)
{
  return maybe_warn_for_bound<gimple *> (opt, loc, stmt, func, bndrng, size,
					 pad);
}

bool
maybe_warn_for_bound (opt_code opt, location_t loc, tree expr, tree func,
		      tree bndrng[2], tree size,
		      const access_data *pad /* = NULL */)
{
  return maybe_warn_for_bound<tree> (opt, loc, expr, func, bndrng, size, pad);
}

/* For an expression EXP issue an access warning controlled by option OPT
   with access to a region SIZE bytes in size in the RANGE of sizes.
   WRITE is true for a write access, READ for a read access, neither for
   call that may or may not perform an access but for which the range
   is expected to valid.
   Returns true when a warning has been issued.  */

template <class GimpleOrTree>
static bool
warn_for_access (location_t loc, tree func, GimpleOrTree exp, int opt,
		 tree range[2], tree size, bool write, bool read, bool maybe)
{
  bool warned = false;

  if (write && read)
    {
      if (tree_int_cst_equal (range[0], range[1]))
	warned = (func
		  ? warning_n (loc, opt, tree_to_uhwi (range[0]),
			       (maybe
				? G_("%qD may access %E byte in a region "
				     "of size %E")
				: G_("%qD accessing %E byte in a region "
				     "of size %E")),
				(maybe
				 ? G_ ("%qD may access %E bytes in a region "
				       "of size %E")
				 : G_ ("%qD accessing %E bytes in a region "
				       "of size %E")),
			       func, range[0], size)
		  : warning_n (loc, opt, tree_to_uhwi (range[0]),
			       (maybe
				? G_("may access %E byte in a region "
				     "of size %E")
				: G_("accessing %E byte in a region "
				     "of size %E")),
			       (maybe
				? G_("may access %E bytes in a region "
				     "of size %E")
				: G_("accessing %E bytes in a region "
				     "of size %E")),
			       range[0], size));
      else if (tree_int_cst_sign_bit (range[1]))
	{
	  /* Avoid printing the upper bound if it's invalid.  */
	  warned = (func
		    ? warning_at (loc, opt,
				  (maybe
				   ? G_("%qD may access %E or more bytes "
					"in a region of size %E")
				   : G_("%qD accessing %E or more bytes "
					"in a region of size %E")),
				  func, range[0], size)
		    : warning_at (loc, opt,
				  (maybe
				   ? G_("may access %E or more bytes "
					"in a region of size %E")
				   : G_("accessing %E or more bytes "
					"in a region of size %E")),
				  range[0], size));
	}
      else
	warned = (func
		  ? warning_at (loc, opt,
				(maybe
				 ? G_("%qD may access between %E and %E "
				      "bytes in a region of size %E")
				 : G_("%qD accessing between %E and %E "
				      "bytes in a region of size %E")),
				func, range[0], range[1], size)
		  : warning_at (loc, opt,
				(maybe
				 ? G_("may access between %E and %E bytes "
				      "in a region of size %E")
				 : G_("accessing between %E and %E bytes "
				      "in a region of size %E")),
				range[0], range[1], size));
      return warned;
    }

  if (write)
    {
      if (tree_int_cst_equal (range[0], range[1]))
	warned = (func
		  ? warning_n (loc, opt, tree_to_uhwi (range[0]),
			       (maybe
				? G_("%qD may write %E byte into a region "
				     "of size %E")
				: G_("%qD writing %E byte into a region "
				     "of size %E overflows the destination")),
			       (maybe
				? G_("%qD may write %E bytes into a region "
				     "of size %E")
				: G_("%qD writing %E bytes into a region "
				     "of size %E overflows the destination")),
			       func, range[0], size)
		  : warning_n (loc, opt, tree_to_uhwi (range[0]),
			       (maybe
				? G_("may write %E byte into a region "
				     "of size %E")
				: G_("writing %E byte into a region "
				     "of size %E overflows the destination")),
			       (maybe
				? G_("may write %E bytes into a region "
				     "of size %E")
				: G_("writing %E bytes into a region "
				     "of size %E overflows the destination")),
			       range[0], size));
      else if (tree_int_cst_sign_bit (range[1]))
	{
	  /* Avoid printing the upper bound if it's invalid.  */
	  warned = (func
		    ? warning_at (loc, opt,
				  (maybe
				   ? G_("%qD may write %E or more bytes "
					"into a region of size %E")
				   : G_("%qD writing %E or more bytes "
					"into a region of size %E overflows "
					"the destination")),
				  func, range[0], size)
		    : warning_at (loc, opt,
				  (maybe
				   ? G_("may write %E or more bytes into "
					"a region of size %E")
				   : G_("writing %E or more bytes into "
					"a region of size %E overflows "
					"the destination")),
				  range[0], size));
	}
      else
	warned = (func
		  ? warning_at (loc, opt,
				(maybe
				 ? G_("%qD may write between %E and %E bytes "
				      "into a region of size %E")
				 : G_("%qD writing between %E and %E bytes "
				      "into a region of size %E overflows "
				      "the destination")),
				func, range[0], range[1], size)
		  : warning_at (loc, opt,
				(maybe
				 ? G_("may write between %E and %E bytes "
				      "into a region of size %E")
				 : G_("writing between %E and %E bytes "
				      "into a region of size %E overflows "
				      "the destination")),
				range[0], range[1], size));
      return warned;
    }

  if (read)
    {
      if (tree_int_cst_equal (range[0], range[1]))
	warned = (func
		  ? warning_n (loc, OPT_Wstringop_overread,
			       tree_to_uhwi (range[0]),
			       (maybe
				? G_("%qD may read %E byte from a region "
				     "of size %E")
				: G_("%qD reading %E byte from a region "
				     "of size %E")),
			       (maybe
				? G_("%qD may read %E bytes from a region "
				     "of size %E")
				: G_("%qD reading %E bytes from a region "
				     "of size %E")),
			       func, range[0], size)
		  : warning_n (loc, OPT_Wstringop_overread,
			       tree_to_uhwi (range[0]),
			       (maybe
				? G_("may read %E byte from a region "
				     "of size %E")
				: G_("reading %E byte from a region "
				     "of size %E")),
			       (maybe
				? G_("may read %E bytes from a region "
				     "of size %E")
				: G_("reading %E bytes from a region "
				     "of size %E")),
			       range[0], size));
      else if (tree_int_cst_sign_bit (range[1]))
	{
	  /* Avoid printing the upper bound if it's invalid.  */
	  warned = (func
		    ? warning_at (loc, OPT_Wstringop_overread,
				  (maybe
				   ? G_("%qD may read %E or more bytes "
					"from a region of size %E")
				   : G_("%qD reading %E or more bytes "
					"from a region of size %E")),
				  func, range[0], size)
		    : warning_at (loc, OPT_Wstringop_overread,
				  (maybe
				   ? G_("may read %E or more bytes "
					"from a region of size %E")
				   : G_("reading %E or more bytes "
					"from a region of size %E")),
				  range[0], size));
	}
      else
	warned = (func
		  ? warning_at (loc, OPT_Wstringop_overread,
				(maybe
				 ? G_("%qD may read between %E and %E bytes "
				      "from a region of size %E")
				 : G_("%qD reading between %E and %E bytes "
				      "from a region of size %E")),
				func, range[0], range[1], size)
		  : warning_at (loc, opt,
				(maybe
				 ? G_("may read between %E and %E bytes "
				      "from a region of size %E")
				 : G_("reading between %E and %E bytes "
				      "from a region of size %E")),
				range[0], range[1], size));

      if (warned)
	suppress_warning (exp, OPT_Wstringop_overread);

      return warned;
    }

  if (tree_int_cst_equal (range[0], range[1])
      || tree_int_cst_sign_bit (range[1]))
    warned = (func
	      ? warning_n (loc, OPT_Wstringop_overread,
			   tree_to_uhwi (range[0]),
			   "%qD expecting %E byte in a region of size %E",
			   "%qD expecting %E bytes in a region of size %E",
			   func, range[0], size)
	      : warning_n (loc, OPT_Wstringop_overread,
			   tree_to_uhwi (range[0]),
			   "expecting %E byte in a region of size %E",
			   "expecting %E bytes in a region of size %E",
			   range[0], size));
  else if (tree_int_cst_sign_bit (range[1]))
    {
      /* Avoid printing the upper bound if it's invalid.  */
      warned = (func
		? warning_at (loc, OPT_Wstringop_overread,
			      "%qD expecting %E or more bytes in a region "
			      "of size %E",
			      func, range[0], size)
		: warning_at (loc, OPT_Wstringop_overread,
			      "expecting %E or more bytes in a region "
			      "of size %E",
			      range[0], size));
    }
  else
    warned = (func
	      ? warning_at (loc, OPT_Wstringop_overread,
			    "%qD expecting between %E and %E bytes in "
			    "a region of size %E",
			    func, range[0], range[1], size)
	      : warning_at (loc, OPT_Wstringop_overread,
			    "expecting between %E and %E bytes in "
			    "a region of size %E",
			    range[0], range[1], size));

  if (warned)
    suppress_warning (exp, OPT_Wstringop_overread);

  return warned;
}

static bool
warn_for_access (location_t loc, tree func, gimple *stmt, int opt,
		 tree range[2], tree size, bool write, bool read, bool maybe)
{
  return warn_for_access<gimple *>(loc, func, stmt, opt, range, size,
				   write, read, maybe);
}

static bool
warn_for_access (location_t loc, tree func, tree expr, int opt,
		 tree range[2], tree size, bool write, bool read, bool maybe)
{
  return warn_for_access<tree>(loc, func, expr, opt, range, size,
			       write, read, maybe);
}

/* Helper to set RANGE to the range of BOUND if it's nonnull, bounded
   by BNDRNG if nonnull and valid.  */

static void
get_size_range (range_query *query, tree bound, tree range[2],
		const offset_int bndrng[2])
{
  if (bound)
    get_size_range (query, bound, NULL, range);

  if (!bndrng || (bndrng[0] == 0 && bndrng[1] == HOST_WIDE_INT_M1U))
    return;

  if (range[0] && TREE_CODE (range[0]) == INTEGER_CST)
    {
      offset_int r[] =
	{ wi::to_offset (range[0]), wi::to_offset (range[1]) };
      if (r[0] < bndrng[0])
	range[0] = wide_int_to_tree (sizetype, bndrng[0]);
      if (bndrng[1] < r[1])
	range[1] = wide_int_to_tree (sizetype, bndrng[1]);
    }
  else
    {
      range[0] = wide_int_to_tree (sizetype, bndrng[0]);
      range[1] = wide_int_to_tree (sizetype, bndrng[1]);
    }
}

/* Try to verify that the sizes and lengths of the arguments to a string
   manipulation function given by EXP are within valid bounds and that
   the operation does not lead to buffer overflow or read past the end.
   Arguments other than EXP may be null.  When non-null, the arguments
   have the following meaning:
   DST is the destination of a copy call or NULL otherwise.
   SRC is the source of a copy call or NULL otherwise.
   DSTWRITE is the number of bytes written into the destination obtained
   from the user-supplied size argument to the function (such as in
   memcpy(DST, SRCs, DSTWRITE) or strncpy(DST, DRC, DSTWRITE).
   MAXREAD is the user-supplied bound on the length of the source sequence
   (such as in strncat(d, s, N).  It specifies the upper limit on the number
   of bytes to write.  If NULL, it's taken to be the same as DSTWRITE.
   SRCSTR is the source string (such as in strcpy(DST, SRC)) when the
   expression EXP is a string function call (as opposed to a memory call
   like memcpy).  As an exception, SRCSTR can also be an integer denoting
   the precomputed size of the source string or object (for functions like
   memcpy).
   DSTSIZE is the size of the destination object.

   When DSTWRITE is null LEN is checked to verify that it doesn't exceed
   SIZE_MAX.

   WRITE is true for write accesses, READ is true for reads.  Both are
   false for simple size checks in calls to functions that neither read
   from nor write to the region.

   When nonnull, PAD points to a more detailed description of the access.

   If the call is successfully verified as safe return true, otherwise
   return false.  */

template <class GimpleOrTree>
static bool
check_access (GimpleOrTree exp, tree dstwrite,
	      tree maxread, tree srcstr, tree dstsize,
	      access_mode mode, const access_data *pad /* = NULL */)
{
  /* The size of the largest object is half the address space, or
     PTRDIFF_MAX.  (This is way too permissive.)  */
  tree maxobjsize = max_object_size ();

  /* Either an approximate/minimum the length of the source string for
     string functions or the size of the source object for raw memory
     functions.  */
  tree slen = NULL_TREE;

  /* The range of the access in bytes; first set to the write access
     for functions that write and then read for those that also (or
     just) read.  */
  tree range[2] = { NULL_TREE, NULL_TREE };

  /* Set to true when the exact number of bytes written by a string
     function like strcpy is not known and the only thing that is
     known is that it must be at least one (for the terminating nul).  */
  bool at_least_one = false;
  if (srcstr)
    {
      /* SRCSTR is normally a pointer to string but as a special case
	 it can be an integer denoting the length of a string.  */
      if (POINTER_TYPE_P (TREE_TYPE (srcstr)))
	{
	  if (!check_nul_terminated_array (exp, srcstr, maxread))
	    /* Return if the array is not nul-terminated and a warning
	       has been issued.  */
	    return false;

	  /* Try to determine the range of lengths the source string
	     refers to.  If it can be determined and is less than
	     the upper bound given by MAXREAD add one to it for
	     the terminating nul.  Otherwise, set it to one for
	     the same reason, or to MAXREAD as appropriate.  */
	  c_strlen_data lendata = { };
	  get_range_strlen (srcstr, &lendata, /* eltsize = */ 1);
	  range[0] = lendata.minlen;
	  range[1] = lendata.maxbound ? lendata.maxbound : lendata.maxlen;
	  if (range[0]
	      && TREE_CODE (range[0]) == INTEGER_CST
	      && TREE_CODE (range[1]) == INTEGER_CST
	      && (!maxread || TREE_CODE (maxread) == INTEGER_CST))
	    {
	      if (maxread && tree_int_cst_le (maxread, range[0]))
		range[0] = range[1] = maxread;
	      else
		range[0] = fold_build2 (PLUS_EXPR, size_type_node,
					range[0], size_one_node);

	      if (maxread && tree_int_cst_le (maxread, range[1]))
		range[1] = maxread;
	      else if (!integer_all_onesp (range[1]))
		range[1] = fold_build2 (PLUS_EXPR, size_type_node,
					range[1], size_one_node);

	      slen = range[0];
	    }
	  else
	    {
	      at_least_one = true;
	      slen = size_one_node;
	    }
	}
      else
	slen = srcstr;
    }

  if (!dstwrite && !maxread)
    {
      /* When the only available piece of data is the object size
	 there is nothing to do.  */
      if (!slen)
	return true;

      /* Otherwise, when the length of the source sequence is known
	 (as with strlen), set DSTWRITE to it.  */
      if (!range[0])
	dstwrite = slen;
    }

  if (!dstsize)
    dstsize = maxobjsize;

  /* Set RANGE to that of DSTWRITE if non-null, bounded by PAD->DST.BNDRNG
     if valid.  */
  get_size_range (NULL, dstwrite, range, pad ? pad->dst.bndrng : NULL);

  tree func = get_callee_fndecl (exp);
  /* Read vs write access by built-ins can be determined from the const
     qualifiers on the pointer argument.  In the absence of attribute
     access, non-const qualified pointer arguments to user-defined
     functions are assumed to both read and write the objects.  */
  const bool builtin = func ? fndecl_built_in_p (func) : false;

  /* First check the number of bytes to be written against the maximum
     object size.  */
  if (range[0]
      && TREE_CODE (range[0]) == INTEGER_CST
      && tree_int_cst_lt (maxobjsize, range[0]))
    {
      location_t loc = get_location (exp);
      maybe_warn_for_bound (OPT_Wstringop_overflow_, loc, exp, func, range,
			    NULL_TREE, pad);
      return false;
    }

  /* The number of bytes to write is "exact" if DSTWRITE is non-null,
     constant, and in range of unsigned HOST_WIDE_INT.  */
  bool exactwrite = dstwrite && tree_fits_uhwi_p (dstwrite);

  /* Next check the number of bytes to be written against the destination
     object size.  */
  if (range[0] || !exactwrite || integer_all_onesp (dstwrite))
    {
      if (range[0]
	  && TREE_CODE (range[0]) == INTEGER_CST
	  && ((tree_fits_uhwi_p (dstsize)
	       && tree_int_cst_lt (dstsize, range[0]))
	      || (dstwrite
		  && tree_fits_uhwi_p (dstwrite)
		  && tree_int_cst_lt (dstwrite, range[0]))))
	{
	  const opt_code opt = OPT_Wstringop_overflow_;
	  if (warning_suppressed_p (exp, opt)
	      || (pad && pad->dst.ref
		  && warning_suppressed_p (pad->dst.ref, opt)))
	    return false;

	  location_t loc = get_location (exp);
	  bool warned = false;
	  if (dstwrite == slen && at_least_one)
	    {
	      /* This is a call to strcpy with a destination of 0 size
		 and a source of unknown length.  The call will write
		 at least one byte past the end of the destination.  */
	      warned = (func
			? warning_at (loc, opt,
				      "%qD writing %E or more bytes into "
				      "a region of size %E overflows "
				      "the destination",
				      func, range[0], dstsize)
			: warning_at (loc, opt,
				      "writing %E or more bytes into "
				      "a region of size %E overflows "
				      "the destination",
				      range[0], dstsize));
	    }
	  else
	    {
	      const bool read
		= mode == access_read_only || mode == access_read_write;
	      const bool write
		= mode == access_write_only || mode == access_read_write;
	      const bool maybe = pad && pad->dst.parmarray;
	      warned = warn_for_access (loc, func, exp,
					OPT_Wstringop_overflow_,
					range, dstsize,
					write, read && !builtin, maybe);
	    }

	  if (warned)
	    {
	      suppress_warning (exp, OPT_Wstringop_overflow_);
	      if (pad)
		pad->dst.inform_access (pad->mode);
	    }

	  /* Return error when an overflow has been detected.  */
	  return false;
	}
    }

  /* Check the maximum length of the source sequence against the size
     of the destination object if known, or against the maximum size
     of an object.  */
  if (maxread)
    {
      /* Set RANGE to that of MAXREAD, bounded by PAD->SRC.BNDRNG if
	 PAD is nonnull and BNDRNG is valid.  */
      get_size_range (NULL, maxread, range, pad ? pad->src.bndrng : NULL);

      location_t loc = get_location (exp);
      tree size = dstsize;
      if (pad && pad->mode == access_read_only)
	size = wide_int_to_tree (sizetype, pad->src.size_remaining ());

      if (range[0] && maxread && tree_fits_uhwi_p (size))
	{
	  if (tree_int_cst_lt (maxobjsize, range[0]))
	    {
	      maybe_warn_for_bound (OPT_Wstringop_overread, loc, exp, func,
				    range, size, pad);
	      return false;
	    }

	  if (size != maxobjsize && tree_int_cst_lt (size, range[0]))
	    {
	      opt_code opt = (dstwrite || mode != access_read_only
			      ? OPT_Wstringop_overflow_
			      : OPT_Wstringop_overread);
	      maybe_warn_for_bound (opt, loc, exp, func, range, size, pad);
	      return false;
	    }
	}

      maybe_warn_nonstring_arg (func, exp);
    }

  /* Check for reading past the end of SRC.  */
  bool overread = (slen
		   && slen == srcstr
		   && dstwrite
		   && range[0]
		   && TREE_CODE (slen) == INTEGER_CST
		   && tree_int_cst_lt (slen, range[0]));
  /* If none is determined try to get a better answer based on the details
     in PAD.  */
  if (!overread
      && pad
      && pad->src.sizrng[1] >= 0
      && pad->src.offrng[0] >= 0
      && (pad->src.offrng[1] < 0
	  || pad->src.offrng[0] <= pad->src.offrng[1]))
    {
      /* Set RANGE to that of MAXREAD, bounded by PAD->SRC.BNDRNG if
	 PAD is nonnull and BNDRNG is valid.  */
      get_size_range (NULL, maxread, range, pad ? pad->src.bndrng : NULL);
      /* Set OVERREAD for reads starting just past the end of an object.  */
      overread = pad->src.sizrng[1] - pad->src.offrng[0] < pad->src.bndrng[0];
      range[0] = wide_int_to_tree (sizetype, pad->src.bndrng[0]);
      slen = size_zero_node;
    }

  if (overread)
    {
      const opt_code opt = OPT_Wstringop_overread;
      if (warning_suppressed_p (exp, opt)
	  || (srcstr && warning_suppressed_p (srcstr, opt))
	  || (pad && pad->src.ref
	      && warning_suppressed_p (pad->src.ref, opt)))
	return false;

      location_t loc = get_location (exp);
      const bool read
	= mode == access_read_only || mode == access_read_write;
      const bool maybe = pad && pad->dst.parmarray;
      if (warn_for_access (loc, func, exp, opt, range, slen, false, read,
			   maybe))
	{
	  suppress_warning (exp, opt);
	  if (pad)
	    pad->src.inform_access (access_read_only);
	}
      return false;
    }

  return true;
}

bool
check_access (gimple *stmt, tree dstwrite,
	      tree maxread, tree srcstr, tree dstsize,
	      access_mode mode, const access_data *pad /* = NULL */)
{
  return check_access<gimple *>(stmt, dstwrite, maxread, srcstr, dstsize,
				mode, pad);
}

bool
check_access (tree expr, tree dstwrite,
	      tree maxread, tree srcstr, tree dstsize,
	      access_mode mode, const access_data *pad /* = NULL */)
{
  return check_access<tree>(expr, dstwrite, maxread, srcstr, dstsize,
			    mode, pad);
}

/* A convenience wrapper for check_access above to check access
   by a read-only function like puts.  */

template <class GimpleOrTree>
static bool
check_read_access (GimpleOrTree expr, tree src, tree bound, int ost)
{
  if (!warn_stringop_overread)
    return true;

  if (bound && !useless_type_conversion_p (size_type_node, TREE_TYPE (bound)))
    bound = fold_convert (size_type_node, bound);

  tree fndecl = get_callee_fndecl (expr);
  maybe_warn_nonstring_arg (fndecl, expr);

  access_data data (expr, access_read_only, NULL_TREE, false, bound, true);
  compute_objsize (src, ost, &data.src);
  return check_access (expr, /*dstwrite=*/ NULL_TREE, /*maxread=*/ bound,
		       /*srcstr=*/ src, /*dstsize=*/ NULL_TREE, data.mode,
		       &data);
}

bool
check_read_access (gimple *stmt, tree src, tree bound /* = NULL_TREE */,
		   int ost /* = 1 */)
{
  return check_read_access<gimple *>(stmt, src, bound, ost);
}

bool
check_read_access (tree expr, tree src, tree bound /* = NULL_TREE */,
		   int ost /* = 1 */)
{
  return check_read_access<tree>(expr, src, bound, ost);
}

/* Return true if STMT is a call to an allocation function.  Unless
   ALL_ALLOC is set, consider only functions that return dynmamically
   allocated objects.  Otherwise return true even for all forms of
   alloca (including VLA).  */

static bool
fndecl_alloc_p (tree fndecl, bool all_alloc)
{
  if (!fndecl)
    return false;

  /* A call to operator new isn't recognized as one to a built-in.  */
  if (DECL_IS_OPERATOR_NEW_P (fndecl))
    return true;

  if (fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    {
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_ALLOCA:
	case BUILT_IN_ALLOCA_WITH_ALIGN:
	  return all_alloc;
	case BUILT_IN_ALIGNED_ALLOC:
	case BUILT_IN_CALLOC:
	case BUILT_IN_GOMP_ALLOC:
	case BUILT_IN_MALLOC:
	case BUILT_IN_REALLOC:
	case BUILT_IN_STRDUP:
	case BUILT_IN_STRNDUP:
	  return true;
	default:
	  break;
	}
    }

  /* A function is considered an allocation function if it's declared
     with attribute malloc with an argument naming its associated
     deallocation function.  */
  tree attrs = DECL_ATTRIBUTES (fndecl);
  if (!attrs)
    return false;

  for (tree allocs = attrs;
       (allocs = lookup_attribute ("malloc", allocs));
       allocs = TREE_CHAIN (allocs))
    {
      tree args = TREE_VALUE (allocs);
      if (!args)
	continue;

      if (TREE_VALUE (args))
	return true;
    }

  return false;
}

/* Return true if STMT is a call to an allocation function.  A wrapper
   around fndecl_alloc_p.  */

static bool
gimple_call_alloc_p (gimple *stmt, bool all_alloc = false)
{
  return fndecl_alloc_p (gimple_call_fndecl (stmt), all_alloc);
}

/* Return true if DELC doesn't refer to an operator delete that's
   suitable to call with a pointer returned from the operator new
   described by NEWC.  */

static bool
new_delete_mismatch_p (const demangle_component &newc,
		       const demangle_component &delc)
{
  if (newc.type != delc.type)
    return true;

  switch (newc.type)
    {
    case DEMANGLE_COMPONENT_NAME:
      {
	int len = newc.u.s_name.len;
	const char *news = newc.u.s_name.s;
	const char *dels = delc.u.s_name.s;
	if (len != delc.u.s_name.len || memcmp (news, dels, len))
	  return true;

	if (news[len] == 'n')
	  {
	    if (news[len + 1] == 'a')
	      return dels[len] != 'd' || dels[len + 1] != 'a';
	    if (news[len + 1] == 'w')
	      return dels[len] != 'd' || dels[len + 1] != 'l';
	  }
	return false;
      }

    case DEMANGLE_COMPONENT_OPERATOR:
      /* Operator mismatches are handled above.  */
      return false;

    case DEMANGLE_COMPONENT_EXTENDED_OPERATOR:
      if (newc.u.s_extended_operator.args != delc.u.s_extended_operator.args)
	return true;
      return new_delete_mismatch_p (*newc.u.s_extended_operator.name,
				    *delc.u.s_extended_operator.name);

    case DEMANGLE_COMPONENT_FIXED_TYPE:
      if (newc.u.s_fixed.accum != delc.u.s_fixed.accum
	  || newc.u.s_fixed.sat != delc.u.s_fixed.sat)
	return true;
      return new_delete_mismatch_p (*newc.u.s_fixed.length,
				    *delc.u.s_fixed.length);

    case DEMANGLE_COMPONENT_CTOR:
      if (newc.u.s_ctor.kind != delc.u.s_ctor.kind)
	return true;
      return new_delete_mismatch_p (*newc.u.s_ctor.name,
				    *delc.u.s_ctor.name);

    case DEMANGLE_COMPONENT_DTOR:
      if (newc.u.s_dtor.kind != delc.u.s_dtor.kind)
	return true;
      return new_delete_mismatch_p (*newc.u.s_dtor.name,
				    *delc.u.s_dtor.name);

    case DEMANGLE_COMPONENT_BUILTIN_TYPE:
      {
	/* The demangler API provides no better way to compare built-in
	   types except to by comparing their demangled names. */
	size_t nsz, dsz;
	demangle_component *pnc = const_cast<demangle_component *>(&newc);
	demangle_component *pdc = const_cast<demangle_component *>(&delc);
	char *nts = cplus_demangle_print (0, pnc, 16, &nsz);
	char *dts = cplus_demangle_print (0, pdc, 16, &dsz);
	if (!nts != !dts)
	  return true;
	bool mismatch = strcmp (nts, dts);
	free (nts);
	free (dts);
	return mismatch;
      }

    case DEMANGLE_COMPONENT_SUB_STD:
      if (newc.u.s_string.len != delc.u.s_string.len)
	return true;
      return memcmp (newc.u.s_string.string, delc.u.s_string.string,
		     newc.u.s_string.len);

    case DEMANGLE_COMPONENT_FUNCTION_PARAM:
    case DEMANGLE_COMPONENT_TEMPLATE_PARAM:
      return newc.u.s_number.number != delc.u.s_number.number;

    case DEMANGLE_COMPONENT_CHARACTER:
      return newc.u.s_character.character != delc.u.s_character.character;

    case DEMANGLE_COMPONENT_DEFAULT_ARG:
    case DEMANGLE_COMPONENT_LAMBDA:
      if (newc.u.s_unary_num.num != delc.u.s_unary_num.num)
	return true;
      return new_delete_mismatch_p (*newc.u.s_unary_num.sub,
				    *delc.u.s_unary_num.sub);
    default:
      break;
    }

  if (!newc.u.s_binary.left != !delc.u.s_binary.left)
    return true;

  if (!newc.u.s_binary.left)
    return false;

  if (new_delete_mismatch_p (*newc.u.s_binary.left, *delc.u.s_binary.left)
      || !newc.u.s_binary.right != !delc.u.s_binary.right)
    return true;

  if (newc.u.s_binary.right)
    return new_delete_mismatch_p (*newc.u.s_binary.right,
				  *delc.u.s_binary.right);
  return false;
}

/* Return true if DELETE_DECL is an operator delete that's not suitable
   to call with a pointer returned fron NEW_DECL.  */

static bool
new_delete_mismatch_p (tree new_decl, tree delete_decl)
{
  tree new_name = DECL_ASSEMBLER_NAME (new_decl);
  tree delete_name = DECL_ASSEMBLER_NAME (delete_decl);

  /* valid_new_delete_pair_p() returns a conservative result (currently
     it only handles global operators).  A true result is reliable but
     a false result doesn't necessarily mean the operators don't match
     unless CERTAIN is set.  */
  bool certain;
  if (valid_new_delete_pair_p (new_name, delete_name, &certain))
    return false;
  /* CERTAIN is set when the negative result is certain.  */
  if (certain)
    return true;

  /* For anything not handled by valid_new_delete_pair_p() such as member
     operators compare the individual demangled components of the mangled
     name.  */
  const char *new_str = IDENTIFIER_POINTER (new_name);
  const char *del_str = IDENTIFIER_POINTER (delete_name);

  void *np = NULL, *dp = NULL;
  demangle_component *ndc = cplus_demangle_v3_components (new_str, 0, &np);
  demangle_component *ddc = cplus_demangle_v3_components (del_str, 0, &dp);
  bool mismatch = new_delete_mismatch_p (*ndc, *ddc);
  free (np);
  free (dp);
  return mismatch;
}

/* ALLOC_DECL and DEALLOC_DECL are pair of allocation and deallocation
   functions.  Return true if the latter is suitable to deallocate objects
   allocated by calls to the former.  */

static bool
matching_alloc_calls_p (tree alloc_decl, tree dealloc_decl)
{
  /* Set to alloc_kind_t::builtin if ALLOC_DECL is associated with
     a built-in deallocator.  */
  enum class alloc_kind_t { none, builtin, user }
  alloc_dealloc_kind = alloc_kind_t::none;

  if (DECL_IS_OPERATOR_NEW_P (alloc_decl))
    {
      if (DECL_IS_OPERATOR_DELETE_P (dealloc_decl))
	/* Return true iff both functions are of the same array or
	   singleton form and false otherwise.  */
	return !new_delete_mismatch_p (alloc_decl, dealloc_decl);

      /* Return false for deallocation functions that are known not
	 to match.  */
      if (fndecl_built_in_p (dealloc_decl, BUILT_IN_FREE)
	  || fndecl_built_in_p (dealloc_decl, BUILT_IN_REALLOC))
	return false;
      /* Otherwise proceed below to check the deallocation function's
	 "*dealloc" attributes to look for one that mentions this operator
	 new.  */
    }
  else if (fndecl_built_in_p (alloc_decl, BUILT_IN_NORMAL))
    {
      switch (DECL_FUNCTION_CODE (alloc_decl))
	{
	case BUILT_IN_ALLOCA:
	case BUILT_IN_ALLOCA_WITH_ALIGN:
	  return false;

	case BUILT_IN_ALIGNED_ALLOC:
	case BUILT_IN_CALLOC:
	case BUILT_IN_GOMP_ALLOC:
	case BUILT_IN_MALLOC:
	case BUILT_IN_REALLOC:
	case BUILT_IN_STRDUP:
	case BUILT_IN_STRNDUP:
	  if (DECL_IS_OPERATOR_DELETE_P (dealloc_decl))
	    return false;

	  if (fndecl_built_in_p (dealloc_decl, BUILT_IN_FREE)
	      || fndecl_built_in_p (dealloc_decl, BUILT_IN_REALLOC))
	    return true;

	  alloc_dealloc_kind = alloc_kind_t::builtin;
	  break;

	default:
	  break;
	}
    }

  /* Set if DEALLOC_DECL both allocates and deallocates.  */
  alloc_kind_t realloc_kind = alloc_kind_t::none;

  if (fndecl_built_in_p (dealloc_decl, BUILT_IN_NORMAL))
    {
      built_in_function dealloc_code = DECL_FUNCTION_CODE (dealloc_decl);
      if (dealloc_code == BUILT_IN_REALLOC)
	realloc_kind = alloc_kind_t::builtin;

      for (tree amats = DECL_ATTRIBUTES (alloc_decl);
	   (amats = lookup_attribute ("malloc", amats));
	   amats = TREE_CHAIN (amats))
	{
	  tree args = TREE_VALUE (amats);
	  if (!args)
	    continue;

	  tree fndecl = TREE_VALUE (args);
	  if (!fndecl || !DECL_P (fndecl))
	    continue;

	  if (fndecl_built_in_p (fndecl, BUILT_IN_NORMAL)
	      && dealloc_code == DECL_FUNCTION_CODE (fndecl))
	    return true;
	}
    }

  const bool alloc_builtin = fndecl_built_in_p (alloc_decl, BUILT_IN_NORMAL);
  alloc_kind_t realloc_dealloc_kind = alloc_kind_t::none;

  /* If DEALLOC_DECL has an internal "*dealloc" attribute scan the list
     of its associated allocation functions for ALLOC_DECL.
     If the corresponding ALLOC_DECL is found they're a matching pair,
     otherwise they're not.
     With DDATS set to the Deallocator's *Dealloc ATtributes...  */
  for (tree ddats = DECL_ATTRIBUTES (dealloc_decl);
       (ddats = lookup_attribute ("*dealloc", ddats));
       ddats = TREE_CHAIN (ddats))
    {
      tree args = TREE_VALUE (ddats);
      if (!args)
	continue;

      tree alloc = TREE_VALUE (args);
      if (!alloc)
	continue;

      if (alloc == DECL_NAME (dealloc_decl))
	realloc_kind = alloc_kind_t::user;

      if (DECL_P (alloc))
	{
	  gcc_checking_assert (fndecl_built_in_p (alloc, BUILT_IN_NORMAL));

	  switch (DECL_FUNCTION_CODE (alloc))
	    {
	    case BUILT_IN_ALIGNED_ALLOC:
	    case BUILT_IN_CALLOC:
	    case BUILT_IN_GOMP_ALLOC:
	    case BUILT_IN_MALLOC:
	    case BUILT_IN_REALLOC:
	    case BUILT_IN_STRDUP:
	    case BUILT_IN_STRNDUP:
	      realloc_dealloc_kind = alloc_kind_t::builtin;
	      break;
	    default:
	      break;
	    }

	  if (!alloc_builtin)
	    continue;

	  if (DECL_FUNCTION_CODE (alloc) != DECL_FUNCTION_CODE (alloc_decl))
	    continue;

	  return true;
	}

      if (alloc == DECL_NAME (alloc_decl))
	return true;
    }

  if (realloc_kind == alloc_kind_t::none)
    return false;

  hash_set<tree> common_deallocs;
  /* Special handling for deallocators.  Iterate over both the allocator's
     and the reallocator's associated deallocator functions looking for
     the first one in common.  If one is found, the de/reallocator is
     a match for the allocator even though the latter isn't directly
     associated with the former.  This simplifies declarations in system
     headers.
     With AMATS set to the Allocator's Malloc ATtributes,
     and  RMATS set to Reallocator's Malloc ATtributes...  */
  for (tree amats = DECL_ATTRIBUTES (alloc_decl),
	 rmats = DECL_ATTRIBUTES (dealloc_decl);
       (amats = lookup_attribute ("malloc", amats))
	 || (rmats = lookup_attribute ("malloc", rmats));
       amats = amats ? TREE_CHAIN (amats) : NULL_TREE,
	 rmats = rmats ? TREE_CHAIN (rmats) : NULL_TREE)
    {
      if (tree args = amats ? TREE_VALUE (amats) : NULL_TREE)
	if (tree adealloc = TREE_VALUE (args))
	  {
	    if (DECL_P (adealloc)
		&& fndecl_built_in_p (adealloc, BUILT_IN_NORMAL))
	      {
		built_in_function fncode = DECL_FUNCTION_CODE (adealloc);
		if (fncode == BUILT_IN_FREE || fncode == BUILT_IN_REALLOC)
		  {
		    if (realloc_kind == alloc_kind_t::builtin)
		      return true;
		    alloc_dealloc_kind = alloc_kind_t::builtin;
		  }
		continue;
	      }

	    common_deallocs.add (adealloc);
	  }

      if (tree args = rmats ? TREE_VALUE (rmats) : NULL_TREE)
	if (tree ddealloc = TREE_VALUE (args))
	  {
	    if (DECL_P (ddealloc)
		&& fndecl_built_in_p (ddealloc, BUILT_IN_NORMAL))
	      {
		built_in_function fncode = DECL_FUNCTION_CODE (ddealloc);
		if (fncode == BUILT_IN_FREE || fncode == BUILT_IN_REALLOC)
		  {
		    if (alloc_dealloc_kind == alloc_kind_t::builtin)
		      return true;
		    realloc_dealloc_kind = alloc_kind_t::builtin;
		  }
		continue;
	      }

	    if (common_deallocs.add (ddealloc))
	      return true;
	  }
    }

  /* Succeed only if ALLOC_DECL and the reallocator DEALLOC_DECL share
     a built-in deallocator.  */
  return  (alloc_dealloc_kind == alloc_kind_t::builtin
	   && realloc_dealloc_kind == alloc_kind_t::builtin);
}

/* Return true if DEALLOC_DECL is a function suitable to deallocate
   objectes allocated by the ALLOC call.  */

static bool
matching_alloc_calls_p (gimple *alloc, tree dealloc_decl)
{
  tree alloc_decl = gimple_call_fndecl (alloc);
  if (!alloc_decl)
    return true;

  return matching_alloc_calls_p (alloc_decl, dealloc_decl);
}

/* Diagnose a call EXP to deallocate a pointer referenced by AREF if it
   includes a nonzero offset.  Such a pointer cannot refer to the beginning
   of an allocated object.  A negative offset may refer to it only if
   the target pointer is unknown.  */

static bool
warn_dealloc_offset (location_t loc, gimple *call, const access_ref &aref)
{
  if (aref.deref || aref.offrng[0] <= 0 || aref.offrng[1] <= 0)
    return false;

  tree dealloc_decl = gimple_call_fndecl (call);
  if (!dealloc_decl)
    return false;

  if (DECL_IS_OPERATOR_DELETE_P (dealloc_decl)
      && !DECL_IS_REPLACEABLE_OPERATOR (dealloc_decl))
    {
      /* A call to a user-defined operator delete with a pointer plus offset
	 may be valid if it's returned from an unknown function (i.e., one
	 that's not operator new).  */
      if (TREE_CODE (aref.ref) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (aref.ref);
	  if (is_gimple_call (def_stmt))
	    {
	      tree alloc_decl = gimple_call_fndecl (def_stmt);
	      if (!alloc_decl || !DECL_IS_OPERATOR_NEW_P (alloc_decl))
		return false;
	    }
	}
    }

  char offstr[80];
  offstr[0] = '\0';
  if (wi::fits_shwi_p (aref.offrng[0]))
    {
      if (aref.offrng[0] == aref.offrng[1]
	  || !wi::fits_shwi_p (aref.offrng[1]))
	sprintf (offstr, " %lli",
		 (long long)aref.offrng[0].to_shwi ());
      else
	sprintf (offstr, " [%lli, %lli]",
		 (long long)aref.offrng[0].to_shwi (),
		 (long long)aref.offrng[1].to_shwi ());
    }

  if (!warning_at (loc, OPT_Wfree_nonheap_object,
		   "%qD called on pointer %qE with nonzero offset%s",
		   dealloc_decl, aref.ref, offstr))
    return false;

  if (DECL_P (aref.ref))
    inform (get_location (aref.ref), "declared here");
  else if (TREE_CODE (aref.ref) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (aref.ref);
      if (is_gimple_call (def_stmt))
	{
	  location_t def_loc = get_location (def_stmt);
	  tree alloc_decl = gimple_call_fndecl (def_stmt);
	  if (alloc_decl)
	    inform (def_loc,
		    "returned from %qD", alloc_decl);
	  else if (tree alloc_fntype = gimple_call_fntype (def_stmt))
	    inform (def_loc,
		    "returned from %qT", alloc_fntype);
	  else
	    inform (def_loc,  "obtained here");
	}
    }

  return true;
}

namespace {

const pass_data pass_data_waccess = {
  GIMPLE_PASS,
  "waccess",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg, /* properties_required  */
  0,	    /* properties_provided  */
  0,	    /* properties_destroyed  */
  0,	    /* properties_start */
  0,	    /* properties_finish */
};

/* Pass to detect invalid accesses.  */
class pass_waccess : public gimple_opt_pass
{
 public:
  pass_waccess (gcc::context *);

  ~pass_waccess ();

  opt_pass *clone () { return new pass_waccess (m_ctxt); }

  virtual bool gate (function *);
  virtual unsigned int execute (function *);

private:
  /* Not copyable or assignable.  */
  pass_waccess (pass_waccess &) = delete;
  void operator= (pass_waccess &) = delete;

  /* Check a call to a built-in function.  */
  bool check_builtin (gcall *);

  /* Check a call to an ordinary function.  */
  bool check_call (gcall *);

  /* Check statements in a basic block.  */
  void check (basic_block);

  /* Check a call to a function.  */
  void check (gcall *);

  /* Check a call to the named built-in function.  */
  void check_alloca (gcall *);
  void check_alloc_size_call (gcall *);
  void check_strcat (gcall *);
  void check_strncat (gcall *);
  void check_stxcpy (gcall *);
  void check_stxncpy (gcall *);
  void check_strncmp (gcall *);
  void check_memop_access (gimple *, tree, tree, tree);

  void maybe_check_dealloc_call (gcall *);
  void maybe_check_access_sizes (rdwr_map *, tree, tree, gimple *);

  /* A pointer_query object and its cache to store information about
     pointers and their targets in.  */
  pointer_query m_ptr_qry;
  pointer_query::cache_type m_var_cache;
};

/* Construct the pass.  */

pass_waccess::pass_waccess (gcc::context *ctxt)
  : gimple_opt_pass (pass_data_waccess, ctxt),
    m_ptr_qry (NULL, &m_var_cache),
    m_var_cache ()
{
}

/* Release pointer_query cache.  */

pass_waccess::~pass_waccess ()
{
  m_ptr_qry.flush_cache ();
}

/* Return true when any checks performed by the pass are enabled.  */

bool
pass_waccess::gate (function *)
{
  return (warn_free_nonheap_object
	  || warn_mismatched_alloc
	  || warn_mismatched_new_delete);
}

/* Initialize ALLOC_OBJECT_SIZE_LIMIT based on the -Walloc-size-larger-than=
   setting if the option is specified, or to the maximum object size if it
   is not.  Return the initialized value.  */

static tree
alloc_max_size (void)
{
  HOST_WIDE_INT limit = warn_alloc_size_limit;
  if (limit == HOST_WIDE_INT_MAX)
    limit = tree_to_shwi (TYPE_MAX_VALUE (ptrdiff_type_node));

  return build_int_cst (size_type_node, limit);
}

/* Diagnose a call EXP to function FN decorated with attribute alloc_size
   whose argument numbers given by IDX with values given by ARGS exceed
   the maximum object size or cause an unsigned oveflow (wrapping) when
   multiplied.  FN is null when EXP is a call via a function pointer.
   When ARGS[0] is null the function does nothing.  ARGS[1] may be null
   for functions like malloc, and non-null for those like calloc that
   are decorated with a two-argument attribute alloc_size.  */

void
maybe_warn_alloc_args_overflow (gimple *stmt, const tree args[2],
				const int idx[2])
{
  /* The range each of the (up to) two arguments is known to be in.  */
  tree argrange[2][2] = { { NULL_TREE, NULL_TREE }, { NULL_TREE, NULL_TREE } };

  /* Maximum object size set by -Walloc-size-larger-than= or SIZE_MAX / 2.  */
  tree maxobjsize = alloc_max_size ();

  location_t loc = get_location (stmt);

  tree fn = gimple_call_fndecl (stmt);
  tree fntype = fn ? TREE_TYPE (fn) : gimple_call_fntype (stmt);
  bool warned = false;

  /* Validate each argument individually.  */
  for (unsigned i = 0; i != 2 && args[i]; ++i)
    {
      if (TREE_CODE (args[i]) == INTEGER_CST)
	{
	  argrange[i][0] = args[i];
	  argrange[i][1] = args[i];

	  if (tree_int_cst_lt (args[i], integer_zero_node))
	    {
	      warned = warning_at (loc, OPT_Walloc_size_larger_than_,
				   "argument %i value %qE is negative",
				   idx[i] + 1, args[i]);
	    }
	  else if (integer_zerop (args[i]))
	    {
	      /* Avoid issuing -Walloc-zero for allocation functions other
		 than __builtin_alloca that are declared with attribute
		 returns_nonnull because there's no portability risk.  This
		 avoids warning for such calls to libiberty's xmalloc and
		 friends.
		 Also avoid issuing the warning for calls to function named
		 "alloca".  */
	      if (fn && fndecl_built_in_p (fn, BUILT_IN_ALLOCA)
		  ? IDENTIFIER_LENGTH (DECL_NAME (fn)) != 6
		  : !lookup_attribute ("returns_nonnull",
				       TYPE_ATTRIBUTES (fntype)))
		warned = warning_at (loc, OPT_Walloc_zero,
				     "argument %i value is zero",
				     idx[i] + 1);
	    }
	  else if (tree_int_cst_lt (maxobjsize, args[i]))
	    {
	      /* G++ emits calls to ::operator new[](SIZE_MAX) in C++98
		 mode and with -fno-exceptions as a way to indicate array
		 size overflow.  There's no good way to detect C++98 here
		 so avoid diagnosing these calls for all C++ modes.  */
	      if (i == 0
		  && fn
		  && !args[1]
		  && lang_GNU_CXX ()
		  && DECL_IS_OPERATOR_NEW_P (fn)
		  && integer_all_onesp (args[i]))
		continue;

	      warned = warning_at (loc, OPT_Walloc_size_larger_than_,
				   "argument %i value %qE exceeds "
				   "maximum object size %E",
				   idx[i] + 1, args[i], maxobjsize);
	    }
	}
      else if (TREE_CODE (args[i]) == SSA_NAME
	       && get_size_range (args[i], argrange[i]))
	{
	  /* Verify that the argument's range is not negative (including
	     upper bound of zero).  */
	  if (tree_int_cst_lt (argrange[i][0], integer_zero_node)
	      && tree_int_cst_le (argrange[i][1], integer_zero_node))
	    {
	      warned = warning_at (loc, OPT_Walloc_size_larger_than_,
				   "argument %i range [%E, %E] is negative",
				   idx[i] + 1,
				   argrange[i][0], argrange[i][1]);
	    }
	  else if (tree_int_cst_lt (maxobjsize, argrange[i][0]))
	    {
	      warned = warning_at (loc, OPT_Walloc_size_larger_than_,
				   "argument %i range [%E, %E] exceeds "
				   "maximum object size %E",
				   idx[i] + 1,
				   argrange[i][0], argrange[i][1],
				   maxobjsize);
	    }
	}
    }

  if (!argrange[0][0])
    return;

  /* For a two-argument alloc_size, validate the product of the two
     arguments if both of their values or ranges are known.  */
  if (!warned && tree_fits_uhwi_p (argrange[0][0])
      && argrange[1][0] && tree_fits_uhwi_p (argrange[1][0])
      && !integer_onep (argrange[0][0])
      && !integer_onep (argrange[1][0]))
    {
      /* Check for overflow in the product of a function decorated with
	 attribute alloc_size (X, Y).  */
      unsigned szprec = TYPE_PRECISION (size_type_node);
      wide_int x = wi::to_wide (argrange[0][0], szprec);
      wide_int y = wi::to_wide (argrange[1][0], szprec);

      wi::overflow_type vflow;
      wide_int prod = wi::umul (x, y, &vflow);

      if (vflow)
	warned = warning_at (loc, OPT_Walloc_size_larger_than_,
			     "product %<%E * %E%> of arguments %i and %i "
			     "exceeds %<SIZE_MAX%>",
			     argrange[0][0], argrange[1][0],
			     idx[0] + 1, idx[1] + 1);
      else if (wi::ltu_p (wi::to_wide (maxobjsize, szprec), prod))
	warned = warning_at (loc, OPT_Walloc_size_larger_than_,
			     "product %<%E * %E%> of arguments %i and %i "
			     "exceeds maximum object size %E",
			     argrange[0][0], argrange[1][0],
			     idx[0] + 1, idx[1] + 1,
			     maxobjsize);

      if (warned)
	{
	  /* Print the full range of each of the two arguments to make
	     it clear when it is, in fact, in a range and not constant.  */
	  if (argrange[0][0] != argrange [0][1])
	    inform (loc, "argument %i in the range [%E, %E]",
		    idx[0] + 1, argrange[0][0], argrange[0][1]);
	  if (argrange[1][0] != argrange [1][1])
	    inform (loc, "argument %i in the range [%E, %E]",
		    idx[1] + 1, argrange[1][0], argrange[1][1]);
	}
    }

  if (warned && fn)
    {
      location_t fnloc = DECL_SOURCE_LOCATION (fn);

      if (DECL_IS_UNDECLARED_BUILTIN (fn))
	inform (loc,
		"in a call to built-in allocation function %qD", fn);
      else
	inform (fnloc,
		"in a call to allocation function %qD declared here", fn);
    }
}

/* Check a call to an alloca function for an excessive size.  */

void
pass_waccess::check_alloca (gcall *stmt)
{
  if ((warn_vla_limit >= HOST_WIDE_INT_MAX
       && warn_alloc_size_limit < warn_vla_limit)
      || (warn_alloca_limit >= HOST_WIDE_INT_MAX
	  && warn_alloc_size_limit < warn_alloca_limit))
    {
      /* -Walloca-larger-than and -Wvla-larger-than settings of less
	 than  HWI_MAX override the more general -Walloc-size-larger-than
	 so unless either of the former options is smaller than the last
	 one (wchich would imply that the call was already checked), check
	 the alloca arguments for overflow.  */
      const tree alloc_args[] = { call_arg (stmt, 0), NULL_TREE };
      const int idx[] = { 0, -1 };
      maybe_warn_alloc_args_overflow (stmt, alloc_args, idx);
    }
}

/* Check a call to an allocation function for an excessive size.  */

void
pass_waccess::check_alloc_size_call (gcall *stmt)
{
  if (gimple_call_num_args (stmt) < 1)
    /* Avoid invalid calls to functions without a prototype.  */
    return;

  tree fndecl = gimple_call_fndecl (stmt);
  if (fndecl && gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      /* Alloca is handled separately.  */
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_ALLOCA:
	case BUILT_IN_ALLOCA_WITH_ALIGN:
	case BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX:
	  return;
	default:
	  break;
	}
    }

  tree fntype = gimple_call_fntype (stmt);
  tree fntypeattrs = TYPE_ATTRIBUTES (fntype);

  tree alloc_size = lookup_attribute ("alloc_size", fntypeattrs);
  if (!alloc_size)
    return;

  /* Extract attribute alloc_size from the type of the called expression
     (which could be a function or a function pointer) and if set, store
     the indices of the corresponding arguments in ALLOC_IDX, and then
     the actual argument(s) at those indices in ALLOC_ARGS.  */
  int idx[2] = { -1, -1 };
  tree alloc_args[] = { NULL_TREE, NULL_TREE };

  tree args = TREE_VALUE (alloc_size);
  idx[0] = TREE_INT_CST_LOW (TREE_VALUE (args)) - 1;
  alloc_args[0] = call_arg (stmt, idx[0]);
  if (TREE_CHAIN (args))
    {
      idx[1] = TREE_INT_CST_LOW (TREE_VALUE (TREE_CHAIN (args))) - 1;
      alloc_args[1] = call_arg (stmt, idx[1]);
    }

  maybe_warn_alloc_args_overflow (stmt, alloc_args, idx);
}

/* Check a call STMT to strcat() for overflow and warn if it does.  */

void
pass_waccess::check_strcat (gcall *stmt)
{
  if (!warn_stringop_overflow && !warn_stringop_overread)
    return;

  tree dest = call_arg (stmt, 0);
  tree src = call_arg (stmt, 1);

  /* There is no way here to determine the length of the string in
     the destination to which the SRC string is being appended so
     just diagnose cases when the souce string is longer than
     the destination object.  */
  access_data data (stmt, access_read_write, NULL_TREE, true,
		    NULL_TREE, true);
  const int ost = warn_stringop_overflow ? warn_stringop_overflow - 1 : 1;
  compute_objsize (src, ost, &data.src, &m_ptr_qry);
  tree destsize = compute_objsize (dest, ost, &data.dst, &m_ptr_qry);

  check_access (stmt, /*dstwrite=*/NULL_TREE, /*maxread=*/NULL_TREE,
		src, destsize, data.mode, &data);
}

/* Check a call STMT to strcat() for overflow and warn if it does.  */

void
pass_waccess::check_strncat (gcall *stmt)
{
  if (!warn_stringop_overflow && !warn_stringop_overread)
    return;

  tree dest = call_arg (stmt, 0);
  tree src = call_arg (stmt, 1);
  /* The upper bound on the number of bytes to write.  */
  tree maxread = call_arg (stmt, 2);

  /* Detect unterminated source (only).  */
  if (!check_nul_terminated_array (stmt, src, maxread))
    return;

  /* The length of the source sequence.  */
  tree slen = c_strlen (src, 1);

  /* Try to determine the range of lengths that the source expression
     refers to.  Since the lengths are only used for warning and not
     for code generation disable strict mode below.  */
  tree maxlen = slen;
  if (!maxlen)
    {
      c_strlen_data lendata = { };
      get_range_strlen (src, &lendata, /* eltsize = */ 1);
      maxlen = lendata.maxbound;
    }

  access_data data (stmt, access_read_write);
  /* Try to verify that the destination is big enough for the shortest
     string.  First try to determine the size of the destination object
     into which the source is being copied.  */
  const int ost = warn_stringop_overflow - 1;
  tree destsize = compute_objsize (dest, ost, &data.dst, &m_ptr_qry);

  /* Add one for the terminating nul.  */
  tree srclen = (maxlen
		 ? fold_build2 (PLUS_EXPR, size_type_node, maxlen,
				size_one_node)
		 : NULL_TREE);

  /* The strncat function copies at most MAXREAD bytes and always appends
     the terminating nul so the specified upper bound should never be equal
     to (or greater than) the size of the destination.  */
  if (tree_fits_uhwi_p (maxread) && tree_fits_uhwi_p (destsize)
      && tree_int_cst_equal (destsize, maxread))
    {
      location_t loc = get_location (stmt);
      warning_at (loc, OPT_Wstringop_overflow_,
		  "%qD specified bound %E equals destination size",
		  get_callee_fndecl (stmt), maxread);

      return;
    }

  if (!srclen
      || (maxread && tree_fits_uhwi_p (maxread)
	  && tree_fits_uhwi_p (srclen)
	  && tree_int_cst_lt (maxread, srclen)))
    srclen = maxread;

  check_access (stmt, /*dstwrite=*/NULL_TREE, maxread, srclen,
		destsize, data.mode, &data);
}

/* Check a call STMT to stpcpy() or strcpy() for overflow and warn
   if it does.  */

void
pass_waccess::check_stxcpy (gcall *stmt)
{
  tree dst = call_arg (stmt, 0);
  tree src = call_arg (stmt, 1);

  tree size;
  bool exact;
  if (tree nonstr = unterminated_array (src, &size, &exact))
    {
      /* NONSTR refers to the non-nul terminated constant array.  */
      warn_string_no_nul (get_location (stmt), stmt, NULL, src, nonstr,
			  size, exact);
      return;
    }

  if (warn_stringop_overflow)
    {
      access_data data (stmt, access_read_write, NULL_TREE, true,
			NULL_TREE, true);
      const int ost = warn_stringop_overflow ? warn_stringop_overflow - 1 : 1;
      compute_objsize (src, ost, &data.src, &m_ptr_qry);
      tree dstsize = compute_objsize (dst, ost, &data.dst, &m_ptr_qry);
      check_access (stmt, /*dstwrite=*/ NULL_TREE,
		    /*maxread=*/ NULL_TREE, /*srcstr=*/ src,
		    dstsize, data.mode, &data);
    }

  /* Check to see if the argument was declared attribute nonstring
     and if so, issue a warning since at this point it's not known
     to be nul-terminated.  */
  tree fndecl = get_callee_fndecl (stmt);
  maybe_warn_nonstring_arg (fndecl, stmt);
}

/* Check a call STMT to stpncpy() or strncpy() for overflow and warn
   if it does.  */

void
pass_waccess::check_stxncpy (gcall *stmt)
{
  if (!warn_stringop_overflow)
    return;

  tree dst = call_arg (stmt, 0);
  tree src = call_arg (stmt, 1);
  /* The number of bytes to write (not the maximum).  */
  tree len = call_arg (stmt, 2);

  access_data data (stmt, access_read_write, len, true, len, true);
  const int ost = warn_stringop_overflow ? warn_stringop_overflow - 1 : 1;
  compute_objsize (src, ost, &data.src, &m_ptr_qry);
  tree dstsize = compute_objsize (dst, ost, &data.dst, &m_ptr_qry);

  check_access (stmt, /*dstwrite=*/len,
		/*maxread=*/len, src, dstsize, data.mode, &data);
}

/* Check a call STMT to stpncpy() or strncpy() for overflow and warn
   if it does.  */

void
pass_waccess::check_strncmp (gcall *stmt)
{
  if (!warn_stringop_overread)
    return;

  tree arg1 = call_arg (stmt, 0);
  tree arg2 = call_arg (stmt, 1);
  tree bound = call_arg (stmt, 2);

  /* First check each argument separately, considering the bound.  */
  if (!check_nul_terminated_array (stmt, arg1, bound)
      || !check_nul_terminated_array (stmt, arg2, bound))
    return;

  /* A strncmp read from each argument is constrained not just by
     the bound but also by the length of the shorter string.  Specifying
     a bound that's larger than the size of either array makes no sense
     and is likely a bug.  When the length of neither of the two strings
     is known but the sizes of both of the arrays they are stored in is,
     issue a warning if the bound is larger than than the size of
     the larger of the two arrays.  */

  c_strlen_data lendata1{ }, lendata2{ };
  tree len1 = c_strlen (arg1, 1, &lendata1);
  tree len2 = c_strlen (arg2, 1, &lendata2);

  if (len1 && len2)
    /* If the length of both arguments was computed they must both be
       nul-terminated and no further checking is necessary regardless
       of the bound.  */
    return;

  /* Check to see if the argument was declared with attribute nonstring
     and if so, issue a warning since at this point it's not known to be
     nul-terminated.  */
  if (maybe_warn_nonstring_arg (get_callee_fndecl (stmt), stmt))
    return;

  access_data adata1 (stmt, access_read_only, NULL_TREE, false, bound, true);
  access_data adata2 (stmt, access_read_only, NULL_TREE, false, bound, true);

  /* Determine the range of the bound first and bail if it fails; it's
     cheaper than computing the size of the objects.  */
  tree bndrng[2] = { NULL_TREE, NULL_TREE };
  get_size_range (m_ptr_qry.rvals, bound, bndrng, adata1.src.bndrng);
  if (!bndrng[0] || integer_zerop (bndrng[0]))
    return;

  if (len1 && tree_int_cst_lt (len1, bndrng[0]))
    bndrng[0] = len1;
  if (len2 && tree_int_cst_lt (len2, bndrng[0]))
    bndrng[0] = len2;

  /* compute_objsize almost never fails (and ultimately should never
     fail).  Don't bother to handle the rare case when it does.  */
  if (!compute_objsize (arg1, 1, &adata1.src, &m_ptr_qry)
      || !compute_objsize (arg2, 1, &adata2.src, &m_ptr_qry))
    return;

  /* Compute the size of the remaining space in each array after
     subtracting any offset into it.  */
  offset_int rem1 = adata1.src.size_remaining ();
  offset_int rem2 = adata2.src.size_remaining ();

  /* Cap REM1 and REM2 at the other if the other's argument is known
     to be an unterminated array, either because there's no space
     left in it after adding its offset or because it's constant and
     has no nul.  */
  if (rem1 == 0 || (rem1 < rem2 && lendata1.decl))
    rem2 = rem1;
  else if (rem2 == 0 || (rem2 < rem1 && lendata2.decl))
    rem1 = rem2;

  /* Point PAD at the array to reference in the note if a warning
     is issued.  */
  access_data *pad = len1 ? &adata2 : &adata1;
  offset_int maxrem = wi::max (rem1, rem2, UNSIGNED);
  if (lendata1.decl || lendata2.decl
      || maxrem < wi::to_offset (bndrng[0]))
    {
      /* Warn when either argument isn't nul-terminated or the maximum
	 remaining space in the two arrays is less than the bound.  */
      tree func = get_callee_fndecl (stmt);
      location_t loc = gimple_location (stmt);
      maybe_warn_for_bound (OPT_Wstringop_overread, loc, stmt, func,
			    bndrng, wide_int_to_tree (sizetype, maxrem),
			    pad);
    }
}

/* Determine and check the sizes of the source and the destination
   of calls to __builtin_{bzero,memcpy,mempcpy,memset} calls.  STMT is
   the call statement, DEST is the destination argument, SRC is the source
   argument or null, and SIZE is the number of bytes being accessed.  Use
   Object Size type-0 regardless of the OPT_Wstringop_overflow_ setting.
   Return true on success (no overflow or invalid sizes), false otherwise.  */

void
pass_waccess::check_memop_access (gimple *stmt, tree dest, tree src, tree size)
{
  /* For functions like memset and memcpy that operate on raw memory
     try to determine the size of the largest source and destination
     object using type-0 Object Size regardless of the object size
     type specified by the option.  */
  access_data data (stmt, access_read_write);
  tree srcsize
    = src ? compute_objsize (src, 0, &data.src, &m_ptr_qry) : NULL_TREE;
  tree dstsize = compute_objsize (dest, 0, &data.dst, &m_ptr_qry);

  check_access (stmt, size, /*maxread=*/NULL_TREE,
		srcsize, dstsize, data.mode, &data);
}

/* Check call STMT to a built-in function for invalid accesses.  Return
   true if a call has been handled.  */

bool
pass_waccess::check_builtin (gcall *stmt)
{
  tree callee = gimple_call_fndecl (stmt);
  if (!callee)
    return false;

  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_ALLOCA:
    case BUILT_IN_ALLOCA_WITH_ALIGN:
    case BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX:
      check_alloca (stmt);
      return true;

    case BUILT_IN_GETTEXT:
    case BUILT_IN_PUTS:
    case BUILT_IN_PUTS_UNLOCKED:
    case BUILT_IN_STRDUP:
      check_read_access (stmt, call_arg (stmt, 0));
      return true;

    case BUILT_IN_INDEX:
    case BUILT_IN_RINDEX:
    case BUILT_IN_STRCHR:
    case BUILT_IN_STRRCHR:
    case BUILT_IN_STRLEN:
      check_read_access (stmt, call_arg (stmt, 0));
      return true;

    case BUILT_IN_FPUTS:
    case BUILT_IN_FPUTS_UNLOCKED:
      check_read_access (stmt, call_arg (stmt, 0));
      return true;

    case BUILT_IN_STRNDUP:
    case BUILT_IN_STRNLEN:
      check_read_access (stmt, call_arg (stmt, 0), call_arg (stmt, 1));
      return true;

    case BUILT_IN_STRCAT:
      check_strcat (stmt);
      return true;

    case BUILT_IN_STRNCAT:
      check_strncat (stmt);
      return true;

    case BUILT_IN_STPCPY:
    case BUILT_IN_STRCPY:
      check_stxcpy (stmt);
      return true;

    case BUILT_IN_STPNCPY:
    case BUILT_IN_STRNCPY:
      check_stxncpy (stmt);
      return true;

    case BUILT_IN_STRCASECMP:
    case BUILT_IN_STRCMP:
    case BUILT_IN_STRPBRK:
    case BUILT_IN_STRSPN:
    case BUILT_IN_STRCSPN:
    case BUILT_IN_STRSTR:
      check_read_access (stmt, call_arg (stmt, 0));
      check_read_access (stmt, call_arg (stmt, 1));
      return true;

    case BUILT_IN_STRNCASECMP:
    case BUILT_IN_STRNCMP:
      check_strncmp (stmt);
      return true;

    case BUILT_IN_MEMCMP:
      {
	tree a1 = call_arg (stmt, 0);
	tree a2 = call_arg (stmt, 1);
	tree len = call_arg (stmt, 2);
	check_read_access (stmt, a1, len, 0);
	check_read_access (stmt, a2, len, 0);
	return true;
      }

    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMMOVE:
      {
	tree dst = call_arg (stmt, 0);
	tree src = call_arg (stmt, 1);
	tree len = call_arg (stmt, 2);
	check_memop_access (stmt, dst, src, len);
	return true;
      }

    case BUILT_IN_MEMCHR:
      {
	tree src = call_arg (stmt, 0);
	tree len = call_arg (stmt, 2);
	check_read_access (stmt, src, len, 0);
	return true;
      }

    case BUILT_IN_MEMSET:
      {
	tree dst = call_arg (stmt, 0);
	tree len = call_arg (stmt, 2);
	check_memop_access (stmt, dst, NULL_TREE, len);
	return true;
      }
	
    default:
      return false;
    }

  return true;
}

/* Returns the type of the argument ARGNO to function with type FNTYPE
   or null when the typoe cannot be determined or no such argument exists.  */

static tree
fntype_argno_type (tree fntype, unsigned argno)
{
  if (!prototype_p (fntype))
    return NULL_TREE;

  tree argtype;
  function_args_iterator it;
  FOREACH_FUNCTION_ARGS (fntype, argtype, it)
    if (argno-- == 0)
      return argtype;

  return NULL_TREE;
}

/* Helper to append the "human readable" attribute access specification
   described by ACCESS to the array ATTRSTR with size STRSIZE.  Used in
   diagnostics.  */

static inline void
append_attrname (const std::pair<int, attr_access> &access,
		 char *attrstr, size_t strsize)
{
  if (access.second.internal_p)
    return;

  tree str = access.second.to_external_string ();
  gcc_assert (strsize >= (size_t) TREE_STRING_LENGTH (str));
  strcpy (attrstr, TREE_STRING_POINTER (str));
}

/* Iterate over attribute access read-only, read-write, and write-only
   arguments and diagnose past-the-end accesses and related problems
   in the function call EXP.  */

void
pass_waccess::maybe_check_access_sizes (rdwr_map *rwm, tree fndecl, tree fntype,
					gimple *stmt)
{
  auto_diagnostic_group adg;

  /* Set if a warning has been issued for any argument (used to decide
     whether to emit an informational note at the end).  */
  opt_code opt_warned = no_warning;

  /* A string describing the attributes that the warnings issued by this
     function apply to.  Used to print one informational note per function
     call, rather than one per warning.  That reduces clutter.  */
  char attrstr[80];
  attrstr[0] = 0;

  for (rdwr_map::iterator it = rwm->begin (); it != rwm->end (); ++it)
    {
      std::pair<int, attr_access> access = *it;

      /* Get the function call arguments corresponding to the attribute's
	 positional arguments.  When both arguments have been specified
	 there will be two entries in *RWM, one for each.  They are
	 cross-referenced by their respective argument numbers in
	 ACCESS.PTRARG and ACCESS.SIZARG.  */
      const int ptridx = access.second.ptrarg;
      const int sizidx = access.second.sizarg;

      gcc_assert (ptridx != -1);
      gcc_assert (access.first == ptridx || access.first == sizidx);

      /* The pointer is set to null for the entry corresponding to
	 the size argument.  Skip it.  It's handled when the entry
	 corresponding to the pointer argument comes up.  */
      if (!access.second.ptr)
	continue;

      tree ptrtype = fntype_argno_type (fntype, ptridx);
      tree argtype = TREE_TYPE (ptrtype);

      /* The size of the access by the call.  */
      tree access_size;
      if (sizidx == -1)
	{
	  /* If only the pointer attribute operand was specified and
	     not size, set SIZE to the greater of MINSIZE or size of
	     one element of the pointed to type to detect smaller
	     objects (null pointers are diagnosed in this case only
	     if the pointer is also declared with attribute nonnull.  */
	  if (access.second.minsize
	      && access.second.minsize != HOST_WIDE_INT_M1U)
	    access_size = build_int_cstu (sizetype, access.second.minsize);
	  else
	    access_size = size_one_node;
	}
      else
	access_size = rwm->get (sizidx)->size;

      /* Format the value or range to avoid an explosion of messages.  */
      char sizstr[80];
      tree sizrng[2] = { size_zero_node, build_all_ones_cst (sizetype) };
      if (get_size_range (m_ptr_qry.rvals, access_size, NULL, sizrng, 1))
	{
	  char *s0 = print_generic_expr_to_str (sizrng[0]);
	  if (tree_int_cst_equal (sizrng[0], sizrng[1]))
	    {
	      gcc_checking_assert (strlen (s0) < sizeof sizstr);
	      strcpy (sizstr, s0);
	    }
	  else
	    {
	      char *s1 = print_generic_expr_to_str (sizrng[1]);
	      gcc_checking_assert (strlen (s0) + strlen (s1)
				   < sizeof sizstr - 4);
	      sprintf (sizstr, "[%s, %s]", s0, s1);
	      free (s1);
	    }
	  free (s0);
	}
      else
	*sizstr = '\0';

      /* Set if a warning has been issued for the current argument.  */
      opt_code arg_warned = no_warning;
      location_t loc = get_location (stmt);
      tree ptr = access.second.ptr;
      if (*sizstr
	  && tree_int_cst_sgn (sizrng[0]) < 0
	  && tree_int_cst_sgn (sizrng[1]) < 0)
	{
	  /* Warn about negative sizes.  */
	  if (access.second.internal_p)
	    {
	      const std::string argtypestr
		= access.second.array_as_string (ptrtype);

	      if (warning_at (loc, OPT_Wstringop_overflow_,
			      "bound argument %i value %s is "
			      "negative for a variable length array "
			      "argument %i of type %s",
			      sizidx + 1, sizstr,
			      ptridx + 1, argtypestr.c_str ()))
		arg_warned = OPT_Wstringop_overflow_;
	    }
	  else if (warning_at (loc, OPT_Wstringop_overflow_,
			       "argument %i value %s is negative",
			       sizidx + 1, sizstr))
	    arg_warned = OPT_Wstringop_overflow_;

	  if (arg_warned != no_warning)
	    {
	      append_attrname (access, attrstr, sizeof attrstr);
	      /* Remember a warning has been issued and avoid warning
		 again below for the same attribute.  */
	      opt_warned = arg_warned;
	      continue;
	    }
	}

      if (tree_int_cst_sgn (sizrng[0]) >= 0)
	{
	  if (COMPLETE_TYPE_P (argtype))
	    {
	      /* Multiply ACCESS_SIZE by the size of the type the pointer
		 argument points to.  If it's incomplete the size is used
		 as is.  */
	      if (tree argsize = TYPE_SIZE_UNIT (argtype))
		if (TREE_CODE (argsize) == INTEGER_CST)
		  {
		    const int prec = TYPE_PRECISION (sizetype);
		    wide_int minsize = wi::to_wide (sizrng[0], prec);
		    minsize *= wi::to_wide (argsize, prec);
		    access_size = wide_int_to_tree (sizetype, minsize);
		  }
	    }
	}
      else
	access_size = NULL_TREE;

      if (integer_zerop (ptr))
	{
	  if (sizidx >= 0 && tree_int_cst_sgn (sizrng[0]) > 0)
	    {
	      /* Warn about null pointers with positive sizes.  This is
		 different from also declaring the pointer argument with
		 attribute nonnull when the function accepts null pointers
		 only when the corresponding size is zero.  */
	      if (access.second.internal_p)
		{
		  const std::string argtypestr
		    = access.second.array_as_string (ptrtype);

		  if (warning_at (loc, OPT_Wnonnull,
				  "argument %i of variable length "
				  "array %s is null but "
				  "the corresponding bound argument "
				  "%i value is %s",
				  ptridx + 1, argtypestr.c_str (),
				  sizidx + 1, sizstr))
		    arg_warned = OPT_Wnonnull;
		}
	      else if (warning_at (loc, OPT_Wnonnull,
				   "argument %i is null but "
				   "the corresponding size argument "
				   "%i value is %s",
				   ptridx + 1, sizidx + 1, sizstr))
		arg_warned = OPT_Wnonnull;
	    }
	  else if (access_size && access.second.static_p)
	    {
	      /* Warn about null pointers for [static N] array arguments
		 but do not warn for ordinary (i.e., nonstatic) arrays.  */
	      if (warning_at (loc, OPT_Wnonnull,
			      "argument %i to %<%T[static %E]%> "
			      "is null where non-null expected",
			      ptridx + 1, argtype, access_size))
		arg_warned = OPT_Wnonnull;
	    }

	  if (arg_warned != no_warning)
	    {
	      append_attrname (access, attrstr, sizeof attrstr);
	      /* Remember a warning has been issued and avoid warning
		 again below for the same attribute.  */
	      opt_warned = OPT_Wnonnull;
	      continue;
	    }
	}

      access_data data (ptr, access.second.mode, NULL_TREE, false,
			NULL_TREE, false);
      access_ref* const pobj = (access.second.mode == access_write_only
				? &data.dst : &data.src);
      tree objsize = compute_objsize (ptr, 1, pobj, &m_ptr_qry);

      /* The size of the destination or source object.  */
      tree dstsize = NULL_TREE, srcsize = NULL_TREE;
      if (access.second.mode == access_read_only
	  || access.second.mode == access_none)
	{
	  /* For a read-only argument there is no destination.  For
	     no access, set the source as well and differentiate via
	     the access flag below.  */
	  srcsize = objsize;
	  if (access.second.mode == access_read_only
	      || access.second.mode == access_none)
	    {
	      /* For a read-only attribute there is no destination so
		 clear OBJSIZE.  This emits "reading N bytes" kind of
		 diagnostics instead of the "writing N bytes" kind,
		 unless MODE is none.  */
	      objsize = NULL_TREE;
	    }
	}
      else
	dstsize = objsize;

      /* Clear the no-warning bit in case it was set by check_access
	 in a prior iteration so that accesses via different arguments
	 are diagnosed.  */
      suppress_warning (stmt, OPT_Wstringop_overflow_, false);
      access_mode mode = data.mode;
      if (mode == access_deferred)
	mode = TYPE_READONLY (argtype) ? access_read_only : access_read_write;
      check_access (stmt, access_size, /*maxread=*/ NULL_TREE, srcsize,
		    dstsize, mode, &data);

      if (warning_suppressed_p (stmt, OPT_Wstringop_overflow_))
	opt_warned = OPT_Wstringop_overflow_;
      if (opt_warned != no_warning)
	{
	  if (access.second.internal_p)
	    inform (loc, "referencing argument %u of type %qT",
		    ptridx + 1, ptrtype);
	  else
	    /* If check_access issued a warning above, append the relevant
	       attribute to the string.  */
	    append_attrname (access, attrstr, sizeof attrstr);
	}
    }

  if (*attrstr)
    {
      if (fndecl)
	inform (get_location (fndecl),
		"in a call to function %qD declared with attribute %qs",
		fndecl, attrstr);
      else
	inform (get_location (stmt),
		"in a call with type %qT and attribute %qs",
		fntype, attrstr);
    }
  else if (opt_warned != no_warning)
    {
      if (fndecl)
	inform (get_location (fndecl),
		"in a call to function %qD", fndecl);
      else
	inform (get_location (stmt),
		"in a call with type %qT", fntype);
    }

  /* Set the bit in case if was cleared and not set above.  */
  if (opt_warned != no_warning)
    suppress_warning (stmt, opt_warned);
}

/* Check call STMT to an ordinary (non-built-in) function for invalid
   accesses.  Return true if a call has been handled.  */

bool
pass_waccess::check_call (gcall *stmt)
{
  tree fntype = gimple_call_fntype (stmt);
  if (!fntype)
    return false;

  tree fntypeattrs = TYPE_ATTRIBUTES (fntype);
  if (!fntypeattrs)
    return false;

  /* Map of attribute accewss specifications for function arguments.  */
  rdwr_map rdwr_idx;
  init_attr_rdwr_indices (&rdwr_idx, fntypeattrs);

  unsigned nargs = call_nargs (stmt);
  for (unsigned i = 0; i != nargs; ++i)
    {
      tree arg = call_arg (stmt, i);

      /* Save the actual argument that corresponds to the access attribute
	 operand for later processing.  */
      if (attr_access *access = rdwr_idx.get (i))
	{
	  if (POINTER_TYPE_P (TREE_TYPE (arg)))
	    {
	      access->ptr = arg;
	      // A nonnull ACCESS->SIZE contains VLA bounds.  */
	    }
	  else
	    {
	      access->size = arg;
	      gcc_assert (access->ptr == NULL_TREE);
	    }
	}
    }

  /* Check attribute access arguments.  */
  tree fndecl = gimple_call_fndecl (stmt);
  maybe_check_access_sizes (&rdwr_idx, fndecl, fntype, stmt);

  check_alloc_size_call (stmt);
  return true;
}

/* Check arguments in a call STMT for attribute nonstring.  */

static void
check_nonstring_args (gcall *stmt)
{
  tree fndecl = gimple_call_fndecl (stmt);

  /* Detect passing non-string arguments to functions expecting
     nul-terminated strings.  */
  maybe_warn_nonstring_arg (fndecl, stmt);
}

/* Issue a warning if a deallocation function such as free, realloc,
   or C++ operator delete is called with an argument not returned by
   a matching allocation function such as malloc or the corresponding
   form of C++ operatorn new.  */

void
pass_waccess::maybe_check_dealloc_call (gcall *call)
{
  tree fndecl = gimple_call_fndecl (call);
  if (!fndecl)
    return;

  unsigned argno = fndecl_dealloc_argno (fndecl);
  if ((unsigned) call_nargs (call) <= argno)
    return;

  tree ptr = gimple_call_arg (call, argno);
  if (integer_zerop (ptr))
    return;

  access_ref aref;
  if (!compute_objsize (ptr, 0, &aref, &m_ptr_qry))
    return;

  tree ref = aref.ref;
  if (integer_zerop (ref))
    return;

  tree dealloc_decl = fndecl;
  location_t loc = gimple_location (call);

  if (DECL_P (ref) || EXPR_P (ref))
    {
      /* Diagnose freeing a declared object.  */
      if (aref.ref_declared ()
	  && warning_at (loc, OPT_Wfree_nonheap_object,
			 "%qD called on unallocated object %qD",
			 dealloc_decl, ref))
	{
	  inform (get_location (ref), "declared here");
	  return;
	}

      /* Diagnose freeing a pointer that includes a positive offset.
	 Such a pointer cannot refer to the beginning of an allocated
	 object.  A negative offset may refer to it.  */
      if (aref.sizrng[0] != aref.sizrng[1]
	  && warn_dealloc_offset (loc, call, aref))
	return;
    }
  else if (CONSTANT_CLASS_P (ref))
    {
      if (warning_at (loc, OPT_Wfree_nonheap_object,
		      "%qD called on a pointer to an unallocated "
		      "object %qE", dealloc_decl, ref))
	{
	  if (TREE_CODE (ptr) == SSA_NAME)
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (ptr);
	      if (is_gimple_assign (def_stmt))
		{
		  location_t loc = gimple_location (def_stmt);
		  inform (loc, "assigned here");
		}
	    }
	  return;
	}
    }
  else if (TREE_CODE (ref) == SSA_NAME)
    {
      /* Also warn if the pointer argument refers to the result
	 of an allocation call like alloca or VLA.  */
      gimple *def_stmt = SSA_NAME_DEF_STMT (ref);
      if (!def_stmt)
	return;

      if (is_gimple_call (def_stmt))
	{
	  bool warned = false;
	  if (gimple_call_alloc_p (def_stmt))
	    {
	      if (matching_alloc_calls_p (def_stmt, dealloc_decl))
		{
		  if (warn_dealloc_offset (loc, call, aref))
		    return;
		}
	      else
		{
		  tree alloc_decl = gimple_call_fndecl (def_stmt);
		  const opt_code opt =
		    (DECL_IS_OPERATOR_NEW_P (alloc_decl)
		     || DECL_IS_OPERATOR_DELETE_P (dealloc_decl)
		     ? OPT_Wmismatched_new_delete
		     : OPT_Wmismatched_dealloc);
		  warned = warning_at (loc, opt,
				       "%qD called on pointer returned "
				       "from a mismatched allocation "
				       "function", dealloc_decl);
		}
	    }
	  else if (gimple_call_builtin_p (def_stmt, BUILT_IN_ALLOCA)
		   || gimple_call_builtin_p (def_stmt,
					     BUILT_IN_ALLOCA_WITH_ALIGN))
	    warned = warning_at (loc, OPT_Wfree_nonheap_object,
				 "%qD called on pointer to "
				 "an unallocated object",
				 dealloc_decl);
	  else if (warn_dealloc_offset (loc, call, aref))
	    return;

	  if (warned)
	    {
	      tree fndecl = gimple_call_fndecl (def_stmt);
	      inform (gimple_location (def_stmt),
		      "returned from %qD", fndecl);
	      return;
	    }
	}
      else if (gimple_nop_p (def_stmt))
	{
	  ref = SSA_NAME_VAR (ref);
	  /* Diagnose freeing a pointer that includes a positive offset.  */
	  if (TREE_CODE (ref) == PARM_DECL
	      && !aref.deref
	      && aref.sizrng[0] != aref.sizrng[1]
	      && aref.offrng[0] > 0 && aref.offrng[1] > 0
	      && warn_dealloc_offset (loc, call, aref))
	    return;
	}
    }
}

/* Check call STMT for invalid accesses.  */

void
pass_waccess::check (gcall *stmt)
{
  if (gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    check_builtin (stmt);

  if (is_gimple_call (stmt))
    check_call (stmt);

  maybe_check_dealloc_call (stmt);

  check_nonstring_args (stmt);
}

/* Check basic block BB for invalid accesses.  */

void
pass_waccess::check (basic_block bb)
{
  /* Iterate over statements, looking for function calls.  */
  for (auto si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      if (gcall *call = dyn_cast <gcall *> (gsi_stmt (si)))
	check (call);
    }
}

/* Check function FUN for invalid accesses.  */

unsigned
pass_waccess::execute (function *fun)
{
  /* Create a new ranger instance and associate it with FUN.  */
  m_ptr_qry.rvals = enable_ranger (fun);

  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    check (bb);

  if (dump_file)
    m_ptr_qry.dump (dump_file, (dump_flags & TDF_DETAILS) != 0);

  m_ptr_qry.flush_cache ();

  /* Release the ranger instance and replace it with a global ranger.
     Also reset the pointer since calling disable_ranger() deletes it.  */
  disable_ranger (fun);
  m_ptr_qry.rvals = NULL;

  return 0;
}

}   // namespace

/* Return a new instance of the pass.  */

gimple_opt_pass *
make_pass_warn_access (gcc::context *ctxt)
{
  return new pass_waccess (ctxt);
}
