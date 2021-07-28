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
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-cfg.h"
#include "tree-object-size.h"
#include "calls.h"
#include "cfgloop.h"
#include "intl.h"
#include "gimple-range.h"
#include "stringpool.h"
#include "attribs.h"
#include "demangle.h"
#include "pointer-query.h"

/* For a call EXPR at LOC to a function FNAME that expects a string
   in the argument ARG, issue a diagnostic due to it being a called
   with an argument that is a character array with no terminating
   NUL.  SIZE is the EXACT size of the array, and BNDRNG the number
   of characters in which the NUL is expected.  Either EXPR or FNAME
   may be null but noth both.  SIZE may be null when BNDRNG is null.  */

void
warn_string_no_nul (location_t loc, tree expr, const char *fname,
		    tree arg, tree decl, tree size /* = NULL_TREE */,
		    bool exact /* = false */,
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
      inform (DECL_SOURCE_LOCATION (decl),
	      "referenced argument declared here");
      suppress_warning (arg, opt);
      if (expr)
	suppress_warning (expr, opt);
    }
}

/* For a call EXPR (which may be null) that expects a string argument
   SRC as an argument, returns false if SRC is a character array with
   no terminating NUL.  When nonnull, BOUND is the number of characters
   in which to expect the terminating NUL.  RDONLY is true for read-only
   accesses such as strcmp, false for read-write such as strcpy.  When
   EXPR is also issues a warning.  */

bool
check_nul_terminated_array (tree expr, tree src,
			    tree bound /* = NULL_TREE */)
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
    warn_string_no_nul (EXPR_LOCATION (expr), expr, NULL, src, nonstr,
			size, exact, bound ? bndrng : NULL);

  return false;
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
  if (len == NULL_TREE && lendata.minlen && lendata.decl)
     {
       if (size)
	{
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
	}
       return lendata.decl;
     }

  return NULL_TREE;
}

/* Issue a warning OPT for a bounded call EXP with a bound in RANGE
   accessing an object with SIZE.  */

bool
maybe_warn_for_bound (opt_code opt, location_t loc, tree exp, tree func,
		      tree bndrng[2], tree size,
		      const access_data *pad /* = NULL */)
{
  if (!bndrng[0] || warning_suppressed_p (exp, opt))
    return false;

  tree maxobjsize = max_object_size ();

  bool warned = false;

  if (opt == OPT_Wstringop_overread)
    {
      bool maybe = pad && pad->src.phi ();

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
	  if (pad && pad->src.ref)
	    {
	      if (DECL_P (pad->src.ref))
		inform (DECL_SOURCE_LOCATION (pad->src.ref),
			"source object declared here");
	      else if (EXPR_HAS_LOCATION (pad->src.ref))
		inform (EXPR_LOCATION (pad->src.ref),
			"source object allocated here");
	    }
	  suppress_warning (exp, opt);
	}

      return warned;
    }

  bool maybe = pad && pad->dst.phi ();
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
      if (pad && pad->dst.ref)
	{
	  if (DECL_P (pad->dst.ref))
	    inform (DECL_SOURCE_LOCATION (pad->dst.ref),
		    "destination object declared here");
	  else if (EXPR_HAS_LOCATION (pad->dst.ref))
	    inform (EXPR_LOCATION (pad->dst.ref),
		    "destination object allocated here");
	}
      suppress_warning (exp, opt);
    }

  return warned;
}

/* For an expression EXP issue an access warning controlled by option OPT
   with access to a region SIZE bytes in size in the RANGE of sizes.
   WRITE is true for a write access, READ for a read access, neither for
   call that may or may not perform an access but for which the range
   is expected to valid.
   Returns true when a warning has been issued.  */

static bool
warn_for_access (location_t loc, tree func, tree exp, int opt, tree range[2],
		 tree size, bool write, bool read, bool maybe)
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

/* Helper to set RANGE to the range of BOUND if it's nonnull, bounded
   by BNDRNG if nonnull and valid.  */

void
get_size_range (tree bound, tree range[2], const offset_int bndrng[2])
{
  if (bound)
    get_size_range (bound, range);

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

bool
check_access (tree exp, tree dstwrite,
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
  get_size_range (dstwrite, range, pad ? pad->dst.bndrng : NULL);

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
      location_t loc = EXPR_LOCATION (exp);
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

	  location_t loc = EXPR_LOCATION (exp);
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
      get_size_range (maxread, range, pad ? pad->src.bndrng : NULL);

      location_t loc = EXPR_LOCATION (exp);
      tree size = dstsize;
      if (pad && pad->mode == access_read_only)
	size = wide_int_to_tree (sizetype, pad->src.sizrng[1]);

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
      get_size_range (maxread, range, pad ? pad->src.bndrng : NULL);
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

      location_t loc = EXPR_LOCATION (exp);
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
     a false result doesn't necessarily mean the operators don't match.  */
  if (valid_new_delete_pair_p (new_name, delete_name))
    return false;

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
    inform (DECL_SOURCE_LOCATION (aref.ref), "declared here");
  else if (TREE_CODE (aref.ref) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (aref.ref);
      if (is_gimple_call (def_stmt))
	{
	  location_t def_loc = gimple_location (def_stmt);
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

/* Issue a warning if a deallocation function such as free, realloc,
   or C++ operator delete is called with an argument not returned by
   a matching allocation function such as malloc or the corresponding
   form of C++ operatorn new.  */

void
maybe_emit_free_warning (gcall *call)
{
  tree fndecl = gimple_call_fndecl (call);
  if (!fndecl)
    return;

  unsigned argno = fndecl_dealloc_argno (fndecl);
  if ((unsigned) gimple_call_num_args (call) <= argno)
    return;

  tree ptr = gimple_call_arg (call, argno);
  if (integer_zerop (ptr))
    return;

  access_ref aref;
  if (!compute_objsize (ptr, 0, &aref))
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
	  loc = (DECL_P (ref)
		 ? DECL_SOURCE_LOCATION (ref)
		 : EXPR_LOCATION (ref));
	  inform (loc, "declared here");
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
  pass_waccess (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_waccess, ctxt), m_ranger ()
    { }

  opt_pass *clone () { return new pass_waccess (m_ctxt); }

  virtual bool gate (function *);
  virtual unsigned int execute (function *);

  void check (basic_block);
  void check (gcall *);

private:
  gimple_ranger *m_ranger;
};

/* Return true when any checks performed by the pass are enabled.  */

bool
pass_waccess::gate (function *)
{
  return (warn_free_nonheap_object
	  || warn_mismatched_alloc
	  || warn_mismatched_new_delete);
}

/* Check call STMT for invalid accesses.  */

void
pass_waccess::check (gcall *stmt)
{
  maybe_emit_free_warning (stmt);
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
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    check (bb);

  return 0;
}

}   // namespace

/* Return a new instance of the pass.  */

gimple_opt_pass *
make_pass_warn_access (gcc::context *ctxt)
{
  return new pass_waccess (ctxt);
}
