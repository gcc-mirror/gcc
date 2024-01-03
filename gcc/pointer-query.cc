/* Definitions of the pointer_query and related classes.

   Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
#include "stringpool.h"
#include "tree-vrp.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "tree-object-size.h"
#include "tree-ssa-strlen.h"
#include "langhooks.h"
#include "stringpool.h"
#include "attribs.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "gimple-ssa.h"
#include "intl.h"
#include "attr-fnspec.h"
#include "gimple-range.h"
#include "pointer-query.h"
#include "tree-pretty-print.h"
#include "tree-ssanames.h"
#include "target.h"

static bool compute_objsize_r (tree, gimple *, bool, int, access_ref *,
			       ssa_name_limit_t &, pointer_query *);

/* Wrapper around the wide_int overload of get_range that accepts
   offset_int instead.  For middle end expressions returns the same
   result.  For a subset of nonconstamt expressions emitted by the front
   end determines a more precise range than would be possible otherwise.  */

static bool
get_offset_range (tree x, gimple *stmt, offset_int r[2], range_query *rvals)
{
  offset_int add = 0;
  if (TREE_CODE (x) == PLUS_EXPR)
    {
      /* Handle constant offsets in pointer addition expressions seen
	 n the front end IL.  */
      tree op = TREE_OPERAND (x, 1);
      if (TREE_CODE (op) == INTEGER_CST)
	{
	  op = fold_convert (signed_type_for (TREE_TYPE (op)), op);
	  add = wi::to_offset (op);
	  x = TREE_OPERAND (x, 0);
	}
    }

  if (TREE_CODE (x) == NOP_EXPR)
    /* Also handle conversions to sizetype seen in the front end IL.  */
    x = TREE_OPERAND (x, 0);

  tree type = TREE_TYPE (x);
  if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
    return false;

   if (TREE_CODE (x) != INTEGER_CST
      && TREE_CODE (x) != SSA_NAME)
    {
      if (TYPE_UNSIGNED (type)
	  && TYPE_PRECISION (type) == TYPE_PRECISION (sizetype))
	type = signed_type_for (type);

      r[0] = wi::to_offset (TYPE_MIN_VALUE (type)) + add;
      r[1] = wi::to_offset (TYPE_MAX_VALUE (type)) + add;
      return x;
    }

  wide_int wr[2];
  if (!get_range (x, stmt, wr, rvals))
    return false;

  signop sgn = SIGNED;
  /* Only convert signed integers or unsigned sizetype to a signed
     offset and avoid converting large positive values in narrower
     types to negative offsets.  */
  if (TYPE_UNSIGNED (type)
      && wr[0].get_precision () < TYPE_PRECISION (sizetype))
    sgn = UNSIGNED;

  r[0] = offset_int::from (wr[0], sgn);
  r[1] = offset_int::from (wr[1], sgn);
  return true;
}

/* Return the argument that the call STMT to a built-in function returns
   or null if it doesn't.  On success, set OFFRNG[] to the range of offsets
   from the argument reflected in the value returned by the built-in if it
   can be determined, otherwise to 0 and HWI_M1U respectively.  Set
   *PAST_END for functions like mempcpy that might return a past the end
   pointer (most functions return a dereferenceable pointer to an existing
   element of an array).  */

static tree
gimple_call_return_array (gimple *stmt, offset_int offrng[2], bool *past_end,
			  ssa_name_limit_t &snlim, pointer_query *qry)
{
  /* Clear and set below for the rare function(s) that might return
     a past-the-end pointer.  */
  *past_end = false;

  {
    /* Check for attribute fn spec to see if the function returns one
       of its arguments.  */
    attr_fnspec fnspec = gimple_call_fnspec (as_a <gcall *>(stmt));
    unsigned int argno;
    if (fnspec.returns_arg (&argno))
      {
	/* Functions return the first argument (not a range).  */
	offrng[0] = offrng[1] = 0;
	return gimple_call_arg (stmt, argno);
      }
  }

  if (gimple_call_num_args (stmt) < 1)
    return NULL_TREE;

  tree fn = gimple_call_fndecl (stmt);
  if (!gimple_call_builtin_p (stmt, BUILT_IN_NORMAL))
    {
      /* See if this is a call to placement new.  */
      if (!fn
	  || !DECL_IS_OPERATOR_NEW_P (fn)
	  || DECL_IS_REPLACEABLE_OPERATOR_NEW_P (fn))
	return NULL_TREE;

      /* Check the mangling, keeping in mind that operator new takes
	 a size_t which could be unsigned int or unsigned long.  */
      tree fname = DECL_ASSEMBLER_NAME (fn);
      if (!id_equal (fname, "_ZnwjPv")       // ordinary form
	  && !id_equal (fname, "_ZnwmPv")    // ordinary form
	  && !id_equal (fname, "_ZnajPv")    // array form
	  && !id_equal (fname, "_ZnamPv"))   // array form
	return NULL_TREE;

      if (gimple_call_num_args (stmt) != 2)
	return NULL_TREE;

      /* Allocation functions return a pointer to the beginning.  */
      offrng[0] = offrng[1] = 0;
      return gimple_call_arg (stmt, 1);
    }

  switch (DECL_FUNCTION_CODE (fn))
    {
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMMOVE:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET:
    case BUILT_IN_STRCAT:
    case BUILT_IN_STRCAT_CHK:
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STRNCAT:
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_STRNCPY:
    case BUILT_IN_STRNCPY_CHK:
      /* Functions return the first argument (not a range).  */
      offrng[0] = offrng[1] = 0;
      return gimple_call_arg (stmt, 0);

    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMPCPY_CHK:
      {
	/* The returned pointer is in a range constrained by the smaller
	   of the upper bound of the size argument and the source object
	   size.  */
	offrng[0] = 0;
	offrng[1] = HOST_WIDE_INT_M1U;
	tree off = gimple_call_arg (stmt, 2);
	bool off_valid = get_offset_range (off, stmt, offrng, qry->rvals);
	if (!off_valid || offrng[0] != offrng[1])
	  {
	    /* If the offset is either indeterminate or in some range,
	       try to constrain its upper bound to at most the size
	       of the source object.  */
	    access_ref aref;
	    tree src = gimple_call_arg (stmt, 1);
	    if (compute_objsize_r (src, stmt, false, 1, &aref, snlim, qry)
		&& aref.sizrng[1] < offrng[1])
	      offrng[1] = aref.sizrng[1];
	  }

	/* Mempcpy may return a past-the-end pointer.  */
	*past_end = true;
	return gimple_call_arg (stmt, 0);
      }

    case BUILT_IN_MEMCHR:
      {
	tree off = gimple_call_arg (stmt, 2);
	if (get_offset_range (off, stmt, offrng, qry->rvals))
	  offrng[1] -= 1;
	else
	  offrng[1] = HOST_WIDE_INT_M1U;

	offrng[0] = 0;
	return gimple_call_arg (stmt, 0);
      }

    case BUILT_IN_STRCHR:
    case BUILT_IN_STRRCHR:
    case BUILT_IN_STRSTR:
      offrng[0] = 0;
      offrng[1] = HOST_WIDE_INT_M1U;
      return gimple_call_arg (stmt, 0);

    case BUILT_IN_STPCPY:
    case BUILT_IN_STPCPY_CHK:
      {
	access_ref aref;
	tree src = gimple_call_arg (stmt, 1);
	if (compute_objsize_r (src, stmt, false, 1, &aref, snlim, qry))
	  offrng[1] = aref.sizrng[1] - 1;
	else
	  offrng[1] = HOST_WIDE_INT_M1U;
	
	offrng[0] = 0;
	return gimple_call_arg (stmt, 0);
      }

    case BUILT_IN_STPNCPY:
    case BUILT_IN_STPNCPY_CHK:
      {
	/* The returned pointer is in a range between the first argument
	   and it plus the smaller of the upper bound of the size argument
	   and the source object size.  */
	offrng[1] = HOST_WIDE_INT_M1U;
	tree off = gimple_call_arg (stmt, 2);
	if (!get_offset_range (off, stmt, offrng, qry->rvals)
	    || offrng[0] != offrng[1])
	  {
	    /* If the offset is either indeterminate or in some range,
	       try to constrain its upper bound to at most the size
	       of the source object.  */
	    access_ref aref;
	    tree src = gimple_call_arg (stmt, 1);
	    if (compute_objsize_r (src, stmt, false, 1, &aref, snlim, qry)
		&& aref.sizrng[1] < offrng[1])
	      offrng[1] = aref.sizrng[1];
	  }

	/* When the source is the empty string the returned pointer is
	   a copy of the argument.  Otherwise stpcpy can also return
	   a past-the-end pointer.  */
	offrng[0] = 0;
	*past_end = true;
	return gimple_call_arg (stmt, 0);
      }

    default:
      break;
    }

  return NULL_TREE;
}

/* Return true when EXP's range can be determined and set RANGE[] to it
   after adjusting it if necessary to make EXP a represents a valid size
   of object, or a valid size argument to an allocation function declared
   with attribute alloc_size (whose argument may be signed), or to a string
   manipulation function like memset.
   When ALLOW_ZERO is set in FLAGS, allow returning a range of [0, 0] for
   a size in an anti-range [1, N] where N > PTRDIFF_MAX.  A zero range is
   a (nearly) invalid argument to allocation functions like malloc but it
   is a valid argument to functions like memset.
   When USE_LARGEST is set in FLAGS set RANGE to the largest valid subrange
   in a multi-range, otherwise to the smallest valid subrange.  */

bool
get_size_range (range_query *query, tree exp, gimple *stmt, tree range[2],
		int flags /* = 0 */)
{
  if (!exp)
    return false;

  if (tree_fits_uhwi_p (exp))
    {
      /* EXP is a constant.  */
      range[0] = range[1] = exp;
      return true;
    }

  tree exptype = TREE_TYPE (exp);
  bool integral = INTEGRAL_TYPE_P (exptype);

  wide_int min, max;
  enum value_range_kind range_type;

  if (!query)
    query = get_range_query (cfun);

  if (integral)
    {
      value_range vr;
      tree tmin, tmax;

      query->range_of_expr (vr, exp, stmt);

      if (vr.undefined_p ())
	vr.set_varying (TREE_TYPE (exp));
      range_type = get_legacy_range (vr, tmin, tmax);
      min = wi::to_wide (tmin);
      max = wi::to_wide (tmax);
    }
  else
    range_type = VR_VARYING;

  if (range_type == VR_VARYING)
    {
      if (integral)
	{	
	  /* Use the full range of the type of the expression when
	     no value range information is available.  */
	  range[0] = TYPE_MIN_VALUE (exptype);
	  range[1] = TYPE_MAX_VALUE (exptype);
	  return true;
	}

      range[0] = NULL_TREE;
      range[1] = NULL_TREE;
      return false;
    }

  unsigned expprec = TYPE_PRECISION (exptype);

  bool signed_p = !TYPE_UNSIGNED (exptype);

  if (range_type == VR_ANTI_RANGE)
    {
      if (signed_p)
	{
	  if (wi::les_p (max, 0))
	    {
	      /* EXP is not in a strictly negative range.  That means
		 it must be in some (not necessarily strictly) positive
		 range which includes zero.  Since in signed to unsigned
		 conversions negative values end up converted to large
		 positive values, and otherwise they are not valid sizes,
		 the resulting range is in both cases [0, TYPE_MAX].  */
	      min = wi::zero (expprec);
	      max = wi::to_wide (TYPE_MAX_VALUE (exptype));
	    }
	  else if (wi::les_p (min - 1, 0))
	    {
	      /* EXP is not in a negative-positive range.  That means EXP
		 is either negative, or greater than max.  Since negative
		 sizes are invalid make the range [MAX + 1, TYPE_MAX].  */
	      min = max + 1;
	      max = wi::to_wide (TYPE_MAX_VALUE (exptype));
	    }
	  else
	    {
	      max = min - 1;
	      min = wi::zero (expprec);
	    }
	}
      else
	{
	  wide_int maxsize = wi::to_wide (max_object_size ());
	  min = wide_int::from (min, maxsize.get_precision (), UNSIGNED);
	  max = wide_int::from (max, maxsize.get_precision (), UNSIGNED);
	  if (wi::eq_p (0, min - 1))
	    {
	      /* EXP is unsigned and not in the range [1, MAX].  That means
		 it's either zero or greater than MAX.  Even though 0 would
		 normally be detected by -Walloc-zero, unless ALLOW_ZERO
		 is set, set the range to [MAX, TYPE_MAX] so that when MAX
		 is greater than the limit the whole range is diagnosed.  */
	      wide_int maxsize = wi::to_wide (max_object_size ());
	      if (flags & SR_ALLOW_ZERO)
		{
		  if (wi::leu_p (maxsize, max + 1)
		      || !(flags & SR_USE_LARGEST))
		    min = max = wi::zero (expprec);
		  else
		    {
		      min = max + 1;
		      max = wi::to_wide (TYPE_MAX_VALUE (exptype));
		    }
		}
	      else
		{
		  min = max + 1;
		  max = wi::to_wide (TYPE_MAX_VALUE (exptype));
		}
	    }
	  else if ((flags & SR_USE_LARGEST)
		   && wi::ltu_p (max + 1, maxsize))
	    {
	      /* When USE_LARGEST is set and the larger of the two subranges
		 is a valid size, use it...  */
	      min = max + 1;
	      max = maxsize;
	    }
	  else
	    {
	      /* ...otherwise use the smaller subrange.  */
	      max = min - 1;
	      min = wi::zero (expprec);
	    }
	}
    }

  range[0] = wide_int_to_tree (exptype, min);
  range[1] = wide_int_to_tree (exptype, max);

  return true;
}

bool
get_size_range (tree exp, tree range[2], int flags /* = 0 */)
{
  return get_size_range (/*query=*/NULL, exp, /*stmt=*/NULL, range, flags);
}

/* If STMT is a call to an allocation function, returns the constant
   maximum size of the object allocated by the call represented as
   sizetype.  If nonnull, sets RNG1[] to the range of the size.
   When nonnull, uses RVALS for range information, otherwise gets global
   range info.
   Returns null when STMT is not a call to a valid allocation function.  */

tree
gimple_call_alloc_size (gimple *stmt, wide_int rng1[2] /* = NULL */,
			range_query *qry /* = NULL */)
{
  if (!stmt || !is_gimple_call (stmt))
    return NULL_TREE;

  tree allocfntype;
  if (tree fndecl = gimple_call_fndecl (stmt))
    allocfntype = TREE_TYPE (fndecl);
  else
    allocfntype = gimple_call_fntype (stmt);

  if (!allocfntype)
    return NULL_TREE;

  unsigned argidx1 = UINT_MAX, argidx2 = UINT_MAX;
  tree at = lookup_attribute ("alloc_size", TYPE_ATTRIBUTES (allocfntype));
  if (!at)
    {
      if (!gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA_WITH_ALIGN))
	return NULL_TREE;

      argidx1 = 0;
    }

  unsigned nargs = gimple_call_num_args (stmt);

  if (argidx1 == UINT_MAX)
    {
      tree atval = TREE_VALUE (at);
      if (!atval)
	return NULL_TREE;

      argidx1 = TREE_INT_CST_LOW (TREE_VALUE (atval)) - 1;
      if (nargs <= argidx1)
	return NULL_TREE;

      atval = TREE_CHAIN (atval);
      if (atval)
	{
	  argidx2 = TREE_INT_CST_LOW (TREE_VALUE (atval)) - 1;
	  if (nargs <= argidx2)
	    return NULL_TREE;
	}
    }

  tree size = gimple_call_arg (stmt, argidx1);

  wide_int rng1_buf[2];
  /* If RNG1 is not set, use the buffer.  */
  if (!rng1)
    rng1 = rng1_buf;

  /* Use maximum precision to avoid overflow below.  */
  const int prec = ADDR_MAX_PRECISION;

  {
    tree r[2];
    /* Determine the largest valid range size, including zero.  */
    if (!get_size_range (qry, size, stmt, r, SR_ALLOW_ZERO | SR_USE_LARGEST))
      return NULL_TREE;
    rng1[0] = wi::to_wide (r[0], prec);
    rng1[1] = wi::to_wide (r[1], prec);
  }

  if (argidx2 > nargs && TREE_CODE (size) == INTEGER_CST)
    return fold_convert (sizetype, size);

  /* To handle ranges do the math in wide_int and return the product
     of the upper bounds as a constant.  Ignore anti-ranges.  */
  tree n = argidx2 < nargs ? gimple_call_arg (stmt, argidx2) : integer_one_node;
  wide_int rng2[2];
  {
    tree r[2];
      /* As above, use the full non-negative range on failure.  */
    if (!get_size_range (qry, n, stmt, r, SR_ALLOW_ZERO | SR_USE_LARGEST))
      return NULL_TREE;
    rng2[0] = wi::to_wide (r[0], prec);
    rng2[1] = wi::to_wide (r[1], prec);
  }

  /* Compute products of both bounds for the caller but return the lesser
     of SIZE_MAX and the product of the upper bounds as a constant.  */
  rng1[0] = rng1[0] * rng2[0];
  rng1[1] = rng1[1] * rng2[1];

  const tree size_max = TYPE_MAX_VALUE (sizetype);
  if (wi::gtu_p (rng1[1], wi::to_wide (size_max, prec)))
    {
      rng1[1] = wi::to_wide (size_max, prec);
      return size_max;
    }

  return wide_int_to_tree (sizetype, rng1[1]);
}

/* For an access to an object referenced to by the function parameter PTR
   of pointer type, and set RNG[] to the range of sizes of the object
   obtainedfrom the attribute access specification for the current function.
   Set STATIC_ARRAY if the array parameter has been declared [static].
   Return the function parameter on success and null otherwise.  */

static tree
gimple_parm_array_size (tree ptr, wide_int rng[2],
			bool *static_array /* = NULL */)
{
  /* For a function argument try to determine the byte size of the array
     from the current function declaratation (e.g., attribute access or
     related).  */
  tree var = SSA_NAME_VAR (ptr);
  if (TREE_CODE (var) != PARM_DECL || !POINTER_TYPE_P (TREE_TYPE (var)))
    return NULL_TREE;

  const unsigned prec = TYPE_PRECISION (sizetype);

  rdwr_map rdwr_idx;
  attr_access *access = get_parm_access (rdwr_idx, var);
  if (!access)
    return NULL_TREE;

  if (access->sizarg != UINT_MAX)
    {
      /* TODO: Try to extract the range from the argument based on
	 those of subsequent assertions or based on known calls to
	 the current function.  */
      return NULL_TREE;
    }

  if (!access->minsize)
    return NULL_TREE;

  /* Only consider ordinary array bound at level 2 (or above if it's
     ever added).  */
  if (warn_array_parameter < 2 && !access->static_p)
    return NULL_TREE;

  if (static_array)
    *static_array = access->static_p;

  rng[0] = wi::zero (prec);
  rng[1] = wi::uhwi (access->minsize, prec);
  /* Multiply the array bound encoded in the attribute by the size
     of what the pointer argument to which it decays points to.  */
  tree eltype = TREE_TYPE (TREE_TYPE (ptr));
  tree size = TYPE_SIZE_UNIT (eltype);
  if (!size || TREE_CODE (size) != INTEGER_CST)
    return NULL_TREE;

  rng[1] *= wi::to_wide (size, prec);
  return var;
}

/* Initialize the object.  */

access_ref::access_ref ()
  : ref (), eval ([](tree x){ return x; }), deref (), ref_nullptr_p (false),
    trail1special (true), base0 (true), parmarray ()
{
  /* Set to valid.  */
  offrng[0] = offrng[1] = 0;
  offmax[0] = offmax[1] = 0;
  /* Invalidate.   */
  sizrng[0] = sizrng[1] = -1;
}

/* Return the PHI node REF refers to or null if it doesn't.  */

gphi *
access_ref::phi () const
{
  if (!ref || TREE_CODE (ref) != SSA_NAME)
    return NULL;

  gimple *def_stmt = SSA_NAME_DEF_STMT (ref);
  if (!def_stmt || gimple_code (def_stmt) != GIMPLE_PHI)
    return NULL;

  return as_a <gphi *> (def_stmt);
}

/* Determine the size and offset for ARG, append it to ALL_REFS, and
   merge the result with *THIS.  Ignore ARG if SKIP_NULL is set and
   ARG refers to the null pointer.  Return true on success and false
   on failure.  */

void
access_ref::merge_ref (vec<access_ref> *all_refs, tree arg, gimple *stmt,
		       int ostype, bool skip_null,
		       ssa_name_limit_t &snlim, pointer_query &qry)
{
  access_ref aref;
  if (!compute_objsize_r (arg, stmt, false, ostype, &aref, snlim, &qry)
      || aref.sizrng[0] < 0)
    {
      /* This may be a PHI with all null pointer arguments.  Handle it
	 conservatively by setting all properties to the most permissive
	 values. */
      base0 = false;
      offrng[0] = offrng[1] = 0;
      add_max_offset ();
      set_max_size_range ();
      return;
    }

  if (all_refs)
    {
      access_ref dummy_ref;
      aref.get_ref (all_refs, &dummy_ref, ostype, &snlim, &qry);
    }

  if (TREE_CODE (arg) == SSA_NAME)
    qry.put_ref (arg, aref, ostype);

  if (all_refs)
    all_refs->safe_push (aref);

  aref.deref += deref;

  bool merged_parmarray = aref.parmarray;

  const bool nullp = skip_null && integer_zerop (arg);
  const offset_int maxobjsize = wi::to_offset (max_object_size ());
  offset_int minsize = sizrng[0];

  if (sizrng[0] < 0)
    {
      /* If *THIS doesn't contain a meaningful result yet set it to AREF
	 unless the argument is null and it's okay to ignore it.  */
      if (!nullp)
	*this = aref;

      /* Set if the current argument refers to one or more objects of
	 known size (or range of sizes), as opposed to referring to
	 one or more unknown object(s).  */
      const bool arg_known_size = (aref.sizrng[0] != 0
				   || aref.sizrng[1] != maxobjsize);
      if (arg_known_size)
	sizrng[0] = aref.sizrng[0];

      return;
    }

  /* Disregard null pointers in PHIs with two or more arguments.
     TODO: Handle this better!  */
  if (nullp)
    return;

  const bool known_size = (sizrng[0] != 0 || sizrng[1] != maxobjsize);

  if (known_size && aref.sizrng[0] < minsize)
    minsize = aref.sizrng[0];

  /* Extend the size and offset of *THIS to account for AREF.  The result
     can be cached but results in false negatives.  */

  offset_int orng[2];
  if (sizrng[1] < aref.sizrng[1])
    {
      orng[0] = offrng[0];
      orng[1] = offrng[1];
      *this = aref;
    }
  else
    {
      orng[0] = aref.offrng[0];
      orng[1] = aref.offrng[1];
    }

  if (orng[0] < offrng[0])
    offrng[0] = orng[0];
  if (offrng[1] < orng[1])
    offrng[1] = orng[1];

  /* Reset the PHI's BASE0 flag if any of the nonnull arguments
     refers to an object at an unknown offset.  */
  if (!aref.base0)
    base0 = false;

  sizrng[0] = minsize;
  parmarray = merged_parmarray;

  return;
}

/* Determine and return the largest object to which *THIS refers.  If
   *THIS refers to a PHI and PREF is nonnull, fill *PREF with the details
   of the object determined by compute_objsize(ARG, OSTYPE) for each PHI
   argument ARG.  */

tree
access_ref::get_ref (vec<access_ref> *all_refs,
		     access_ref *pref /* = NULL */,
		     int ostype /* = 1 */,
		     ssa_name_limit_t *psnlim /* = NULL */,
		     pointer_query *qry /* = NULL */) const
{
  if (!ref || TREE_CODE (ref) != SSA_NAME)
    return NULL;

  /* FIXME: Calling get_ref() with a null PSNLIM is dangerous and might
     cause unbounded recursion.  */
  ssa_name_limit_t snlim_buf;
  if (!psnlim)
    psnlim = &snlim_buf;

  pointer_query empty_qry;
  if (!qry)
    qry = &empty_qry;

  if (gimple *def_stmt = SSA_NAME_DEF_STMT (ref))
    {
      if (is_gimple_assign (def_stmt))
	{
	  tree_code code = gimple_assign_rhs_code (def_stmt);
	  if (code != MIN_EXPR && code != MAX_EXPR)
	    return NULL_TREE;

	  access_ref aref;
	  tree arg1 = gimple_assign_rhs1 (def_stmt);
	  aref.merge_ref (all_refs, arg1, def_stmt, ostype, false,
			  *psnlim, *qry);

	  tree arg2 = gimple_assign_rhs2 (def_stmt);
	  aref.merge_ref (all_refs, arg2, def_stmt, ostype, false,
			  *psnlim, *qry);

	  if (pref && pref != this)
	    {
	      tree ref = pref->ref;
	      *pref = aref;
	      pref->ref = ref;
	    }

	  return aref.ref;
	}
    }
  else
    return NULL_TREE;

  gphi *phi_stmt = this->phi ();
  if (!phi_stmt)
    return ref;

  if (!psnlim->visit_phi (ref))
    return NULL_TREE;

  /* The conservative result of the PHI reflecting the offset and size
     of the largest PHI argument, regardless of whether or not they all
     refer to the same object.  */
  access_ref phi_ref;
  if (pref)
    {
      /* The identity of the object has not been determined yet but
	 PREF->REF is set by the caller to the PHI for convenience.
	 The size is negative/invalid and the offset is zero (it's
	 updated only after the identity of the object has been
	 established).  */
      gcc_assert (pref->sizrng[0] < 0);
      gcc_assert (pref->offrng[0] == 0 && pref->offrng[1] == 0);

      phi_ref = *pref;
    }

  const offset_int maxobjsize = wi::to_offset (max_object_size ());
  const unsigned nargs = gimple_phi_num_args (phi_stmt);
  for (unsigned i = 0; i < nargs; ++i)
    {
      access_ref phi_arg_ref;
      bool skip_null = i || i + 1 < nargs;
      tree arg = gimple_phi_arg_def (phi_stmt, i);
      phi_ref.merge_ref (all_refs, arg, phi_stmt, ostype, skip_null,
			 *psnlim, *qry);

      if (!phi_ref.base0
	  && phi_ref.sizrng[0] == 0
	  && phi_ref.sizrng[1] >= maxobjsize)
	/* When an argument results in the most permissive result,
	   the remaining arguments cannot constrain it.  Short-circuit
	   the evaluation.  */
	break;
    }

  if (phi_ref.sizrng[0] < 0)
    {
      /* Fail if none of the PHI's arguments resulted in updating PHI_REF
	 (perhaps because they have all been already visited by prior
	 recursive calls).  */
      psnlim->leave_phi (ref);
      return NULL_TREE;
    }

  /* Avoid changing *THIS.  */
  if (pref && pref != this)
    {
      /* Keep the SSA_NAME of the PHI unchanged so that all PHI arguments
	 can be referred to later if necessary.  This is useful even if
	 they all refer to the same object.  */
      tree ref = pref->ref;
      *pref = phi_ref;
      pref->ref = ref;
    }

  psnlim->leave_phi (ref);

  return phi_ref.ref;
}

/* Return the maximum amount of space remaining and if non-null, set
   argument to the minimum.  */

offset_int
access_ref::size_remaining (offset_int *pmin /* = NULL */) const
{
  offset_int minbuf;
  if (!pmin)
    pmin = &minbuf;

  if (sizrng[0] < 0)
    {
      /* If the identity of the object hasn't been determined return
	 the maximum size range.  */
      *pmin = 0;
      return wi::to_offset (max_object_size ());
    }

  /* add_offset() ensures the offset range isn't inverted.  */
  gcc_checking_assert (offrng[0] <= offrng[1]);

  if (base0)
    {
      /* The offset into referenced object is zero-based (i.e., it's
	 not referenced by a pointer into middle of some unknown object).  */
      if (offrng[0] < 0 && offrng[1] < 0)
	{
	  /* If the offset is negative the remaining size is zero.  */
	  *pmin = 0;
	  return 0;
	}

      if (sizrng[1] <= offrng[0])
	{
	  /* If the starting offset is greater than or equal to the upper
	     bound on the size of the object, the space remaining is zero.
	     As a special case, if it's equal, set *PMIN to -1 to let
	     the caller know the offset is valid and just past the end.  */
	  *pmin = sizrng[1] == offrng[0] ? -1 : 0;
	  return 0;
	}

      /* Otherwise return the size minus the lower bound of the offset.  */
      offset_int or0 = offrng[0] < 0 ? 0 : offrng[0];

      *pmin = sizrng[0] - or0;
      return sizrng[1] - or0;
    }

  /* The offset to the referenced object isn't zero-based (i.e., it may
     refer to a byte other than the first.  The size of such an object
     is constrained only by the size of the address space (the result
     of max_object_size()).  */
  if (sizrng[1] <= offrng[0])
    {
      *pmin = 0;
      return 0;
    }

  offset_int or0 = offrng[0] < 0 ? 0 : offrng[0];

  *pmin = sizrng[0] - or0;
  return sizrng[1] - or0;
}

/* Return true if the offset and object size are in range for SIZE.  */

bool
access_ref::offset_in_range (const offset_int &size) const
{
  if (size_remaining () < size)
    return false;

  if (base0)
    return offmax[0] >= 0 && offmax[1] <= sizrng[1];

  offset_int maxoff = wi::to_offset (TYPE_MAX_VALUE (ptrdiff_type_node));
  return offmax[0] > -maxoff && offmax[1] < maxoff;
}

/* Add the range [MIN, MAX] to the offset range.  For known objects (with
   zero-based offsets) at least one of whose offset's bounds is in range,
   constrain the other (or both) to the bounds of the object (i.e., zero
   and the upper bound of its size).  This improves the quality of
   diagnostics.  */

void access_ref::add_offset (const offset_int &min, const offset_int &max)
{
  if (min <= max)
    {
      /* To add an ordinary range just add it to the bounds.  */
      offrng[0] += min;
      offrng[1] += max;
    }
  else if (!base0)
    {
      /* To add an inverted range to an offset to an unknown object
	 expand it to the maximum.  */
      add_max_offset ();
      return;
    }
  else
    {
      /* To add an inverted range to an offset to an known object set
	 the upper bound to the maximum representable offset value
	 (which may be greater than MAX_OBJECT_SIZE).
	 The lower bound is either the sum of the current offset and
	 MIN when abs(MAX) is greater than the former, or zero otherwise.
	 Zero because then the inverted range includes the negative of
	 the lower bound.  */
      offset_int maxoff = wi::to_offset (TYPE_MAX_VALUE (ptrdiff_type_node));
      offrng[1] = maxoff;

      if (max >= 0)
	{
	  offrng[0] = 0;
	  if (offmax[0] > 0)
	    offmax[0] = 0;
	  return;
	}

      offset_int absmax = wi::abs (max);
      if (offrng[0] < absmax)
	{
	  offrng[0] += min;
	  /* Cap the lower bound at the upper (set to MAXOFF above)
	     to avoid inadvertently recreating an inverted range.  */
	  if (offrng[1] < offrng[0])
	    offrng[0] = offrng[1];
	}
      else
	offrng[0] = 0;
    }

  /* Set the minimum and maximmum computed so far. */
  if (offrng[1] < 0 && offrng[1] < offmax[0])
    offmax[0] = offrng[1];
  if (offrng[0] > 0 && offrng[0] > offmax[1])
    offmax[1] = offrng[0];

  if (!base0)
    return;

  /* When referencing a known object check to see if the offset computed
     so far is in bounds... */
  offset_int remrng[2];
  remrng[1] = size_remaining (remrng);
  if (remrng[1] > 0 || remrng[0] < 0)
    {
      /* ...if so, constrain it so that neither bound exceeds the size of
	 the object.  Out of bounds offsets are left unchanged, and, for
	 better or worse, become in bounds later.  They should be detected
	 and diagnosed at the point they first become invalid by
	 -Warray-bounds.  */
      if (offrng[0] < 0)
	offrng[0] = 0;
      if (offrng[1] > sizrng[1])
	offrng[1] = sizrng[1];
    }
}

/* Issue one inform message describing each target of an access REF.
   WRITE is set for a write access and clear for a read access.  */

void
access_ref::inform_access (access_mode mode, int ostype /* = 1 */) const
{
  const access_ref &aref = *this;
  if (!aref.ref)
    return;

  if (phi ())
    {
      /* Set MAXREF to refer to the largest object and fill ALL_REFS
	 with data for all objects referenced by the PHI arguments.  */
      access_ref maxref;
      auto_vec<access_ref> all_refs;
      if (!get_ref (&all_refs, &maxref, ostype))
	return;

      if (all_refs.length ())
	{
	  /* Except for MAXREF, the rest of the arguments' offsets need not
	     reflect one added to the PHI itself.  Determine the latter from
	     MAXREF on which the result is based.  */
	  const offset_int orng[] =
	    {
	     offrng[0] - maxref.offrng[0],
	     wi::smax (offrng[1] - maxref.offrng[1], offrng[0]),
	    };

	  /* Add the final PHI's offset to that of each of the arguments
	     and recurse to issue an inform message for it.  */
	  for (unsigned i = 0; i != all_refs.length (); ++i)
	    {
	      /* Skip any PHIs; those could lead to infinite recursion.  */
	      if (all_refs[i].phi ())
		continue;

	      all_refs[i].add_offset (orng[0], orng[1]);
	      all_refs[i].inform_access (mode, ostype);
	    }
	  return;
	}
    }

  /* Convert offset range and avoid including a zero range since it
     isn't necessarily meaningful.  */
  HOST_WIDE_INT diff_min = tree_to_shwi (TYPE_MIN_VALUE (ptrdiff_type_node));
  HOST_WIDE_INT diff_max = tree_to_shwi (TYPE_MAX_VALUE (ptrdiff_type_node));
  HOST_WIDE_INT minoff;
  HOST_WIDE_INT maxoff = diff_max;
  if (wi::fits_shwi_p (aref.offrng[0]))
    minoff = aref.offrng[0].to_shwi ();
  else
    minoff = aref.offrng[0] < 0 ? diff_min : diff_max;

  if (wi::fits_shwi_p (aref.offrng[1]))
    maxoff = aref.offrng[1].to_shwi ();

  if (maxoff <= diff_min || maxoff >= diff_max)
    /* Avoid mentioning an upper bound that's equal to or in excess
       of the maximum of ptrdiff_t.  */
    maxoff = minoff;

  /* Convert size range and always include it since all sizes are
     meaningful. */
  unsigned long long minsize = 0, maxsize = 0;
  if (wi::fits_shwi_p (aref.sizrng[0])
      && wi::fits_shwi_p (aref.sizrng[1]))
    {
      minsize = aref.sizrng[0].to_shwi ();
      maxsize = aref.sizrng[1].to_shwi ();
    }

  /* SIZRNG doesn't necessarily have the same range as the allocation
     size determined by gimple_call_alloc_size ().  */
  char sizestr[80];
  if (minsize == maxsize)
    sprintf (sizestr, "%llu", minsize);
  else
    sprintf (sizestr, "[%llu, %llu]", minsize, maxsize);

  char offstr[80];
  if (minoff == 0
      && (maxoff == 0 || aref.sizrng[1] <= maxoff))
    offstr[0] = '\0';
  else if (minoff == maxoff)
    sprintf (offstr, "%lli", (long long) minoff);
  else
    sprintf (offstr, "[%lli, %lli]", (long long) minoff, (long long) maxoff);

  location_t loc = UNKNOWN_LOCATION;

  tree ref = this->ref;
  tree allocfn = NULL_TREE;
  if (TREE_CODE (ref) == SSA_NAME)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (ref);
      if (!stmt)
	return;

      if (is_gimple_call (stmt))
	{
	  loc = gimple_location (stmt);
	  if (gimple_call_builtin_p (stmt, BUILT_IN_ALLOCA_WITH_ALIGN))
	    {
	      /* Strip the SSA_NAME suffix from the variable name and
		 recreate an identifier with the VLA's original name.  */
	      ref = gimple_call_lhs (stmt);
	      if (SSA_NAME_IDENTIFIER (ref))
		{
		  ref = SSA_NAME_IDENTIFIER (ref);
		  const char *id = IDENTIFIER_POINTER (ref);
		  size_t len = strcspn (id, ".$");
		  if (!len)
		    len = strlen (id);
		  ref = get_identifier_with_length (id, len);
		}
	    }
	  else
	    {
	      /* Except for VLAs, retrieve the allocation function.  */
	      allocfn = gimple_call_fndecl (stmt);
	      if (!allocfn)
		allocfn = gimple_call_fn (stmt);
	      if (TREE_CODE (allocfn) == SSA_NAME)
		{
		  /* For an ALLOC_CALL via a function pointer make a small
		     effort to determine the destination of the pointer.  */
		  gimple *def = SSA_NAME_DEF_STMT (allocfn);
		  if (gimple_assign_single_p (def))
		    {
		      tree rhs = gimple_assign_rhs1 (def);
		      if (DECL_P (rhs))
			allocfn = rhs;
		      else if (TREE_CODE (rhs) == COMPONENT_REF)
			allocfn = TREE_OPERAND (rhs, 1);
		    }
		}
	    }
	}
      else if (gimple_nop_p (stmt))
	/* Handle DECL_PARM below.  */
	ref = SSA_NAME_VAR (ref);
      else if (is_gimple_assign (stmt)
	       && (gimple_assign_rhs_code (stmt) == MIN_EXPR
		   || gimple_assign_rhs_code (stmt) == MAX_EXPR))
	{
	  /* MIN or MAX_EXPR here implies a reference to a known object
	     and either an unknown or distinct one (the latter being
	     the result of an invalid relational expression).  Determine
	     the identity of the former and point to it in the note.
	     TODO: Consider merging with PHI handling.  */
	  access_ref arg_ref[2];
	  tree arg = gimple_assign_rhs1 (stmt);
	  compute_objsize (arg, /* ostype = */ 1 , &arg_ref[0]);
	  arg = gimple_assign_rhs2 (stmt);
	  compute_objsize (arg, /* ostype = */ 1 , &arg_ref[1]);

	  /* Use the argument that references a known object with more
	     space remaining.  */
	  const bool idx
	    = (!arg_ref[0].ref || !arg_ref[0].base0
	       || (arg_ref[0].base0 && arg_ref[1].base0
		   && (arg_ref[0].size_remaining ()
		       < arg_ref[1].size_remaining ())));

	  arg_ref[idx].offrng[0] = offrng[0];
	  arg_ref[idx].offrng[1] = offrng[1];
	  arg_ref[idx].inform_access (mode);
	  return;
	}
    }

  if (DECL_P (ref))
    loc = DECL_SOURCE_LOCATION (ref);
  else if (EXPR_P (ref) && EXPR_HAS_LOCATION (ref))
    loc = EXPR_LOCATION (ref);
  else if (TREE_CODE (ref) != IDENTIFIER_NODE
	   && TREE_CODE (ref) != SSA_NAME)
    {
      if (TREE_CODE (ref) == INTEGER_CST && ref_nullptr_p)
	{
	  if (mode == access_read_write || mode == access_write_only)
	    inform (loc, "destination object is likely at address zero");
	  else
	    inform (loc, "source object is likely at address zero");
	}
      return;
    }

  if (mode == access_read_write || mode == access_write_only)
    {
      if (allocfn == NULL_TREE)
	{
	  if (*offstr)
	    inform (loc, "at offset %s into destination object %qE of size %s",
		    offstr, ref, sizestr);
	  else
	    inform (loc, "destination object %qE of size %s", ref, sizestr);
	  return;
	}

      if (*offstr)
	inform (loc,
		"at offset %s into destination object of size %s "
		"allocated by %qE", offstr, sizestr, allocfn);
      else
	inform (loc, "destination object of size %s allocated by %qE",
		sizestr, allocfn);
      return;
    }

  if (mode == access_read_only)
    {
      if (allocfn == NULL_TREE)
	{
	  if (*offstr)
	    inform (loc, "at offset %s into source object %qE of size %s",
		    offstr, ref, sizestr);
	  else
	    inform (loc, "source object %qE of size %s", ref, sizestr);

	  return;
	}

      if (*offstr)
	inform (loc,
		"at offset %s into source object of size %s allocated by %qE",
		offstr, sizestr, allocfn);
      else
	inform (loc, "source object of size %s allocated by %qE",
		sizestr, allocfn);
      return;
    }

  if (allocfn == NULL_TREE)
    {
      if (*offstr)
	inform (loc, "at offset %s into object %qE of size %s",
		offstr, ref, sizestr);
      else
	inform (loc, "object %qE of size %s", ref, sizestr);

      return;
    }

  if (*offstr)
    inform (loc,
	    "at offset %s into object of size %s allocated by %qE",
	    offstr, sizestr, allocfn);
  else
    inform (loc, "object of size %s allocated by %qE",
	    sizestr, allocfn);
}

/* Dump *THIS to FILE.  */

void
access_ref::dump (FILE *file) const
{
  for (int i = deref; i < 0; ++i)
    fputc ('&', file);

  for (int i = 0; i < deref; ++i)
    fputc ('*', file);

  if (gphi *phi_stmt = phi ())
    {
      fputs ("PHI <", file);
      unsigned nargs = gimple_phi_num_args (phi_stmt);
      for (unsigned i = 0; i != nargs; ++i)
	{
	  tree arg = gimple_phi_arg_def (phi_stmt, i);
	  print_generic_expr (file, arg);
	  if (i + 1 < nargs)
	    fputs (", ", file);
	}
      fputc ('>', file);
    }
  else
    print_generic_expr (file, ref);

  if (offrng[0] != offrng[1])
    fprintf (file, " + [%lli, %lli]",
	     (long long) offrng[0].to_shwi (),
	     (long long) offrng[1].to_shwi ());
  else if (offrng[0] != 0)
    fprintf (file, " %c %lli",
	     offrng[0] < 0 ? '-' : '+',
	     (long long) offrng[0].to_shwi ());

  if (base0)
    fputs (" (base0)", file);

  fputs ("; size: ", file);
  if (sizrng[0] != sizrng[1])
    {
      offset_int maxsize = wi::to_offset (max_object_size ());
      if (sizrng[0] == 0 && sizrng[1] >= maxsize)
	fputs ("unknown", file);
      else
	fprintf (file, "[%llu, %llu]",
		 (unsigned long long) sizrng[0].to_uhwi (),
		 (unsigned long long) sizrng[1].to_uhwi ());
    }
  else if (sizrng[0] != 0)
    fprintf (file, "%llu",
	     (unsigned long long) sizrng[0].to_uhwi ());

  fputc ('\n', file);
}

/* Set the access to at most MAXWRITE and MAXREAD bytes, and at least 1
   when MINWRITE or MINREAD, respectively, is set.  */
access_data::access_data (range_query *query, gimple *stmt, access_mode mode,
			  tree maxwrite /* = NULL_TREE */,
			  bool minwrite /* = false */,
			  tree maxread /* = NULL_TREE */,
			  bool minread /* = false */)
  : stmt (stmt), call (), dst (), src (), mode (mode), ostype ()
{
  set_bound (dst_bndrng, maxwrite, minwrite, query, stmt);
  set_bound (src_bndrng, maxread, minread, query, stmt);
}

/* Set the access to at most MAXWRITE and MAXREAD bytes, and at least 1
   when MINWRITE or MINREAD, respectively, is set.  */
access_data::access_data (range_query *query, tree expr, access_mode mode,
			  tree maxwrite /* = NULL_TREE */,
			  bool minwrite /* = false */,
			  tree maxread /* = NULL_TREE */,
			  bool minread /* = false */)
  : stmt (), call (expr),  dst (), src (), mode (mode), ostype ()
{
  set_bound (dst_bndrng, maxwrite, minwrite, query, stmt);
  set_bound (src_bndrng, maxread, minread, query, stmt);
}

/* Set BNDRNG to the range of BOUND for the statement STMT.  */

void
access_data::set_bound (offset_int bndrng[2], tree bound, bool minaccess,
			range_query *query, gimple *stmt)
{
  /* Set the default bounds of the access and adjust below.  */
  bndrng[0] = minaccess ? 1 : 0;
  bndrng[1] = HOST_WIDE_INT_M1U;

  /* When BOUND is nonnull and a range can be extracted from it,
     set the bounds of the access to reflect both it and MINACCESS.
     BNDRNG[0] is the size of the minimum access.  */
  tree rng[2];
  if (bound && get_size_range (query, bound, stmt, rng, SR_ALLOW_ZERO))
    {
      bndrng[0] = wi::to_offset (rng[0]);
      bndrng[1] = wi::to_offset (rng[1]);
      bndrng[0] = bndrng[0] > 0 && minaccess ? 1 : 0;
    }
}

/* Set a bit for the PHI in VISITED and return true if it wasn't
   already set.  */

bool
ssa_name_limit_t::visit_phi (tree ssa_name)
{
  if (!visited)
    visited = BITMAP_ALLOC (NULL);

  /* Return false if SSA_NAME has already been visited.  */
  return bitmap_set_bit (visited, SSA_NAME_VERSION (ssa_name));
}

/* Clear a bit for the PHI in VISITED.  */

void
ssa_name_limit_t::leave_phi (tree ssa_name)
{
  /* Return false if SSA_NAME has already been visited.  */
  bitmap_clear_bit (visited, SSA_NAME_VERSION (ssa_name));
}

/* Return false if the SSA_NAME chain length counter has reached
   the limit, otherwise increment the counter and return true.  */

bool
ssa_name_limit_t::next ()
{
  /* Return a negative value to let caller avoid recursing beyond
     the specified limit.  */
  if (ssa_def_max == 0)
    return false;

  --ssa_def_max;
  return true;
}

/* If the SSA_NAME has already been "seen" return a positive value.
   Otherwise add it to VISITED.  If the SSA_NAME limit has been
   reached, return a negative value.  Otherwise return zero.  */

int
ssa_name_limit_t::next_phi (tree ssa_name)
{
  {
    gimple *def_stmt = SSA_NAME_DEF_STMT (ssa_name);
    /* Return a positive value if the PHI has already been visited.  */
    if (gimple_code (def_stmt) == GIMPLE_PHI
	&& !visit_phi (ssa_name))
      return 1;
  }

  /* Return a negative value to let caller avoid recursing beyond
     the specified limit.  */
  if (ssa_def_max == 0)
    return -1;

  --ssa_def_max;

  return 0;
}

ssa_name_limit_t::~ssa_name_limit_t ()
{
  if (visited)
    BITMAP_FREE (visited);
}

/* Default ctor.  Initialize object with pointers to the range_query
   instance to use or null.  */

pointer_query::pointer_query (range_query *qry /* = NULL */)
  : rvals (qry), hits (), misses (), failures (), depth (), max_depth (),
    var_cache ()
{
  /* No op.  */
}

/* Return a pointer to the cached access_ref instance for the SSA_NAME
   PTR if it's there or null otherwise.  */

const access_ref *
pointer_query::get_ref (tree ptr, int ostype /* = 1 */) const
{
  unsigned version = SSA_NAME_VERSION (ptr);
  unsigned idx = version << 1 | (ostype & 1);
  if (var_cache.indices.length () <= idx)
    {
      ++misses;
      return NULL;
    }

  unsigned cache_idx = var_cache.indices[idx];
  if (var_cache.access_refs.length () <= cache_idx)
    {
      ++misses;
      return NULL;
    }

  const access_ref &cache_ref = var_cache.access_refs[cache_idx];
  if (cache_ref.ref)
    {
      ++hits;
      return &cache_ref;
    }

  ++misses;
  return NULL;
}

/* Retrieve the access_ref instance for a variable from the cache if it's
   there or compute it and insert it into the cache if it's nonnonull.  */

bool
pointer_query::get_ref (tree ptr, gimple *stmt, access_ref *pref,
			int ostype /* = 1 */)
{
  const unsigned version
    = TREE_CODE (ptr) == SSA_NAME ? SSA_NAME_VERSION (ptr) : 0;

  if (version)
    {
      unsigned idx = version << 1 | (ostype & 1);
      if (idx < var_cache.indices.length ())
	{
	  unsigned cache_idx = var_cache.indices[idx] - 1;
	  if (cache_idx < var_cache.access_refs.length ()
	      && var_cache.access_refs[cache_idx].ref)
	    {
	      ++hits;
	      *pref = var_cache.access_refs[cache_idx];
	      return true;
	    }
	}

      ++misses;
    }

  if (!compute_objsize (ptr, stmt, ostype, pref, this))
    {
      ++failures;
      return false;
    }

  return true;
}

/* Add a copy of the access_ref REF for the SSA_NAME to the cache if it's
   nonnull.  */

void
pointer_query::put_ref (tree ptr, const access_ref &ref, int ostype /* = 1 */)
{
  /* Only add populated/valid entries.  */
  if (!ref.ref || ref.sizrng[0] < 0)
    return;

  /* Add REF to the two-level cache.  */
  unsigned version = SSA_NAME_VERSION (ptr);
  unsigned idx = version << 1 | (ostype & 1);

  /* Grow INDICES if necessary.  An index is valid if it's nonzero.
     Its value minus one is the index into ACCESS_REFS.  Not all
     entries are valid.  */
  if (var_cache.indices.length () <= idx)
    var_cache.indices.safe_grow_cleared (idx + 1);

  if (!var_cache.indices[idx])
    var_cache.indices[idx] = var_cache.access_refs.length () + 1;

  /* Grow ACCESS_REF cache if necessary.  An entry is valid if its
     REF member is nonnull.  All entries except for the last two
     are valid.  Once nonnull, the REF value must stay unchanged.  */
  unsigned cache_idx = var_cache.indices[idx];
  if (var_cache.access_refs.length () <= cache_idx)
    var_cache.access_refs.safe_grow_cleared (cache_idx + 1);

  access_ref &cache_ref = var_cache.access_refs[cache_idx];
  if (cache_ref.ref)
  {
    gcc_checking_assert (cache_ref.ref == ref.ref);
    return;
  }

  cache_ref = ref;
}

/* Flush the cache if it's nonnull.  */

void
pointer_query::flush_cache ()
{
  var_cache.indices.release ();
  var_cache.access_refs.release ();
}

/* Dump statistics and, optionally, cache contents to DUMP_FILE.  */

void
pointer_query::dump (FILE *dump_file, bool contents /* = false */)
{
  unsigned nused = 0, nrefs = 0;
  unsigned nidxs = var_cache.indices.length ();
  for (unsigned i = 0; i != nidxs; ++i)
    {
      unsigned ari = var_cache.indices[i];
      if (!ari)
	continue;

      ++nused;

      const access_ref &aref = var_cache.access_refs[ari];
      if (!aref.ref)
	continue;

      ++nrefs;
    }

  fprintf (dump_file, "pointer_query counters:\n"
	   "  index cache size:   %u\n"
	   "  index entries:      %u\n"
	   "  access cache size:  %u\n"
	   "  access entries:     %u\n"
	   "  hits:               %u\n"
	   "  misses:             %u\n"
	   "  failures:           %u\n"
	   "  max_depth:          %u\n",
	   nidxs, nused,
	   var_cache.access_refs.length (), nrefs,
	   hits, misses, failures, max_depth);

  if (!contents || !nidxs)
    return;

  fputs ("\npointer_query cache contents:\n", dump_file);

  for (unsigned i = 0; i != nidxs; ++i)
    {
      unsigned ari = var_cache.indices[i];
      if (!ari)
	continue;

      const access_ref &aref = var_cache.access_refs[ari];
      if (!aref.ref)
	continue;

      /* The level-1 cache index corresponds to the SSA_NAME_VERSION
	 shifted left by one and ORed with the Object Size Type in
	 the lowest bit.  Print the two separately.  */
      unsigned ver = i >> 1;
      unsigned ost = i & 1;

      fprintf (dump_file, "  %u.%u[%u]: ", ver, ost, ari);
      if (tree name = ssa_name (ver))
	{
	  print_generic_expr (dump_file, name);
	  fputs (" = ", dump_file);
	}
      else
	fprintf (dump_file, "  _%u = ", ver);

      aref.dump (dump_file);
    }

  fputc ('\n', dump_file);
}

/* A helper of compute_objsize_r() to determine the size from an assignment
   statement STMT with the RHS of either MIN_EXPR or MAX_EXPR.  On success
   set PREF->REF to the operand with more or less space remaining,
   respectively, if both refer to the same (sub)object, or to PTR if they
   might not, and return true.  Otherwise, if the identity of neither
   operand can be determined, return false.  */

static bool
handle_min_max_size (tree ptr, int ostype, access_ref *pref,
		     ssa_name_limit_t &snlim, pointer_query *qry)
{
  gimple *stmt = SSA_NAME_DEF_STMT (ptr);
  const tree_code code = gimple_assign_rhs_code (stmt);

  /* In a valid MAX_/MIN_EXPR both operands must refer to the same array.
     Determine the size/offset of each and use the one with more or less
     space remaining, respectively.  If either fails, use the information
     determined from the other instead, adjusted up or down as appropriate
     for the expression.  */
  access_ref aref[2] = { *pref, *pref };
  tree arg1 = gimple_assign_rhs1 (stmt);
  if (!compute_objsize_r (arg1, stmt, false, ostype, &aref[0], snlim, qry))
    {
      aref[0].base0 = false;
      aref[0].offrng[0] = aref[0].offrng[1] = 0;
      aref[0].add_max_offset ();
      aref[0].set_max_size_range ();
    }

  tree arg2 = gimple_assign_rhs2 (stmt);
  if (!compute_objsize_r (arg2, stmt, false, ostype, &aref[1], snlim, qry))
    {
      aref[1].base0 = false;
      aref[1].offrng[0] = aref[1].offrng[1] = 0;
      aref[1].add_max_offset ();
      aref[1].set_max_size_range ();
    }

  if (!aref[0].ref && !aref[1].ref)
    /* Fail if the identity of neither argument could be determined.  */
    return false;

  bool i0 = false;
  if (aref[0].ref && aref[0].base0)
    {
      if (aref[1].ref && aref[1].base0)
	{
	  /* If the object referenced by both arguments has been determined
	     set *PREF to the one with more or less space remainng, whichever
	     is appopriate for CODE.
	     TODO: Indicate when the objects are distinct so it can be
	     diagnosed.  */
	  i0 = code == MAX_EXPR;
	  const bool i1 = !i0;

	  if (aref[i0].size_remaining () < aref[i1].size_remaining ())
	    *pref = aref[i1];
	  else
	    *pref = aref[i0];

	  if (aref[i0].ref != aref[i1].ref)
	    /* If the operands don't refer to the same (sub)object set
	       PREF->REF to the SSA_NAME from which STMT was obtained
	       so that both can be identified in a diagnostic.  */
	    pref->ref = ptr;

	  return true;
	}

      /* If only the object referenced by one of the arguments could be
	 determined, use it and...  */
      *pref = aref[0];
      i0 = true;
    }
  else
    *pref = aref[1];

  const bool i1 = !i0;
  /* ...see if the offset obtained from the other pointer can be used
     to tighten up the bound on the offset obtained from the first.  */
  if ((code == MAX_EXPR && aref[i1].offrng[1] < aref[i0].offrng[0])
      || (code == MIN_EXPR && aref[i0].offrng[0] < aref[i1].offrng[1]))
    {
      pref->offrng[0] = aref[i0].offrng[0];
      pref->offrng[1] = aref[i0].offrng[1];
    }

  /* Replace PTR->REF with the SSA_NAME to indicate the expression
     might not refer to the same (sub)object.  */
  pref->ref = ptr;
  return true;
}

/* A helper of compute_objsize_r() to determine the size of a DECL.
   Return true on success and (possibly in the future) false on failure.  */

static bool
handle_decl (tree decl, bool addr, access_ref *pref)
{
  tree decl_type = TREE_TYPE (decl);

  pref->ref = decl;

  /* Reset the offset in case it was set by a prior call and not
     cleared by the caller.  The offset is only adjusted after
     the identity of the object has been determined.  */
  pref->offrng[0] = pref->offrng[1] = 0;

  if (!addr && POINTER_TYPE_P (decl_type))
    {
      /* Set the maximum size if the reference is to the pointer
	 itself (as opposed to what it points to), and clear
	 BASE0 since the offset isn't necessarily zero-based.  */
      pref->set_max_size_range ();
      pref->base0 = false;
      return true;
    }

  /* Valid offsets into the object are nonnegative.  */
  pref->base0 = true;

  if (tree size = decl_init_size (decl, false))
    if (TREE_CODE (size) == INTEGER_CST)
      {
	pref->sizrng[0] = wi::to_offset (size);
	pref->sizrng[1] = pref->sizrng[0];
	return true;
      }

  pref->set_max_size_range ();
  return true;
}

/* A helper of compute_objsize_r() to determine the size from ARRAY_REF
   AREF.  ADDR is true if PTR is the operand of ADDR_EXPR.  Return true
   on success and false on failure.  */

static bool
handle_array_ref (tree aref, gimple *stmt, bool addr, int ostype,
		  access_ref *pref, ssa_name_limit_t &snlim,
		  pointer_query *qry)
{
  gcc_assert (TREE_CODE (aref) == ARRAY_REF);

  tree arefop = TREE_OPERAND (aref, 0);
  tree reftype = TREE_TYPE (arefop);
  if (!addr && TREE_CODE (TREE_TYPE (reftype)) == POINTER_TYPE)
    /* Avoid arrays of pointers.  FIXME: Hande pointers to arrays
       of known bound.  */
    return false;

  if (!compute_objsize_r (arefop, stmt, addr, ostype, pref, snlim, qry))
    return false;

  offset_int orng[2];
  tree off = pref->eval (TREE_OPERAND (aref, 1));
  range_query *const rvals = qry ? qry->rvals : NULL;
  if (!get_offset_range (off, stmt, orng, rvals))
    {
      /* Set ORNG to the maximum offset representable in ptrdiff_t.  */
      orng[1] = wi::to_offset (TYPE_MAX_VALUE (ptrdiff_type_node));
      orng[0] = -orng[1] - 1;
    }

  /* Convert the array index range determined above to a byte offset.  */
  tree lowbnd = array_ref_low_bound (aref);
  if (TREE_CODE (lowbnd) == INTEGER_CST && !integer_zerop (lowbnd))
    {
      /* Adjust the index by the low bound of the array domain (0 in C/C++,
	 1 in Fortran and anything in Ada) by applying the same processing
	 as in get_offset_range.  */
      const wide_int wlb = wi::to_wide (lowbnd);
      signop sgn = SIGNED;
      if (TYPE_UNSIGNED (TREE_TYPE (lowbnd))
	  && wlb.get_precision () < TYPE_PRECISION (sizetype))
	sgn = UNSIGNED;
      const offset_int lb = offset_int::from (wlb, sgn);
      orng[0] -= lb;
      orng[1] -= lb;
    }

  tree eltype = TREE_TYPE (aref);
  tree tpsize = TYPE_SIZE_UNIT (eltype);
  if (!tpsize || TREE_CODE (tpsize) != INTEGER_CST)
    {
      pref->add_max_offset ();
      return true;
    }

  offset_int sz = wi::to_offset (tpsize);
  orng[0] *= sz;
  orng[1] *= sz;

  if (ostype && TREE_CODE (eltype) == ARRAY_TYPE)
    {
      /* Except for the permissive raw memory functions which use
	 the size of the whole object determined above, use the size
	 of the referenced array.  Because the overall offset is from
	 the beginning of the complete array object add this overall
	 offset to the size of array.  */
      offset_int sizrng[2] =
	{
	 pref->offrng[0] + orng[0] + sz,
	 pref->offrng[1] + orng[1] + sz
	};
      if (sizrng[1] < sizrng[0])
	std::swap (sizrng[0], sizrng[1]);
      if (sizrng[0] >= 0 && sizrng[0] <= pref->sizrng[0])
	pref->sizrng[0] = sizrng[0];
      if (sizrng[1] >= 0 && sizrng[1] <= pref->sizrng[1])
	pref->sizrng[1] = sizrng[1];
    }

  pref->add_offset (orng[0], orng[1]);
  return true;
}

/* Given a COMPONENT_REF CREF, set *PREF size to the size of the referenced
   member.  */

static void
set_component_ref_size (tree cref, access_ref *pref)
{
  const tree base = TREE_OPERAND (cref, 0);
  const tree base_type = TREE_TYPE (base);

  /* SAM is set for array members that might need special treatment.  */
  special_array_member sam;
  tree size = component_ref_size (cref, &sam);
  if (sam == special_array_member::int_0)
    pref->sizrng[0] = pref->sizrng[1] = 0;
  else if (!pref->trail1special && sam == special_array_member::trail_1)
    pref->sizrng[0] = pref->sizrng[1] = 1;
  else if (size && TREE_CODE (size) == INTEGER_CST)
    pref->sizrng[0] = pref->sizrng[1] = wi::to_offset (size);
  else
    {
      /* When the size of the member is unknown it's either a flexible
	 array member or a trailing special array member (either zero
	 length or one-element).  Set the size to the maximum minus
	 the constant size of the base object's type.  */
      pref->sizrng[0] = 0;
      pref->sizrng[1] = wi::to_offset (TYPE_MAX_VALUE (ptrdiff_type_node));
      if (tree base_size = TYPE_SIZE_UNIT (base_type))
	if (TREE_CODE (base_size) == INTEGER_CST)
	  pref->sizrng[1] -= wi::to_offset (base_size);
    }
}

/* A helper of compute_objsize_r() to determine the size from COMPONENT_REF
   CREF.  Return true on success and false on failure.  */

static bool
handle_component_ref (tree cref, gimple *stmt, bool addr, int ostype,
		      access_ref *pref, ssa_name_limit_t &snlim,
		      pointer_query *qry)
{
  gcc_assert (TREE_CODE (cref) == COMPONENT_REF);

  const tree base = TREE_OPERAND (cref, 0);
  const tree field = TREE_OPERAND (cref, 1);
  access_ref base_ref = *pref;

  /* Unconditionally determine the size of the base object (it could
     be smaller than the referenced member when the object is stored
     in a buffer with an insufficient size).  */
  if (!compute_objsize_r (base, stmt, addr, 0, &base_ref, snlim, qry))
    return false;

  /* Add the offset of the member to the offset into the object computed
     so far.  */
  tree offset = byte_position (field);
  if (TREE_CODE (offset) == INTEGER_CST)
    base_ref.add_offset (wi::to_offset (offset));
  else
    base_ref.add_max_offset ();

  if (!base_ref.ref)
    /* PREF->REF may have been already set to an SSA_NAME earlier
       to provide better context for diagnostics.  In that case,
       leave it unchanged.  */
    base_ref.ref = base;

  const tree base_type = TREE_TYPE (base);
  if (TREE_CODE (base_type) == UNION_TYPE)
    /* In accesses through union types consider the entire unions
       rather than just their members.  */
    ostype = 0;

  if (ostype == 0)
    {
      /* In OSTYPE zero (for raw memory functions like memcpy), use
	 the maximum size instead if the identity of the enclosing
	 object cannot be determined.  */
      *pref = base_ref;
      return true;
    }

  pref->ref = field;

  if (!addr && POINTER_TYPE_P (TREE_TYPE (field)))
    {
      /* Set maximum size if the reference is to the pointer member
	 itself (as opposed to what it points to).  */
      pref->set_max_size_range ();
      return true;
    }

  set_component_ref_size (cref, pref);

  if (base_ref.size_remaining () < pref->size_remaining ())
    /* Use the base object if it's smaller than the member.  */
    *pref = base_ref;

  return true;
}

/* A helper of compute_objsize_r() to determine the size from MEM_REF
   MREF.  Return true on success and false on failure.  */

static bool
handle_mem_ref (tree mref, gimple *stmt, int ostype, access_ref *pref,
		ssa_name_limit_t &snlim, pointer_query *qry)
{
  gcc_assert (TREE_CODE (mref) == MEM_REF);

  tree mreftype = TYPE_MAIN_VARIANT (TREE_TYPE (mref));
  if (VECTOR_TYPE_P (mreftype))
      {
      /* Hack: Handle MEM_REFs of vector types as those to complete
	 objects; those may be synthesized from multiple assignments
	 to consecutive data members (see PR 93200 and 96963).
	 FIXME: Vectorized assignments should only be present after
	 vectorization so this hack is only necessary after it has
	 run and could be avoided in calls from prior passes (e.g.,
	 tree-ssa-strlen.cc).
	 FIXME: Deal with this more generally, e.g., by marking up
	 such MEM_REFs at the time they're created.  */
      ostype = 0;
    }

  tree mrefop = TREE_OPERAND (mref, 0);
  if (!compute_objsize_r (mrefop, stmt, false, ostype, pref, snlim, qry))
    return false;

  ++pref->deref;

  offset_int orng[2];
  tree off = pref->eval (TREE_OPERAND (mref, 1));
  range_query *const rvals = qry ? qry->rvals : NULL;
  if (!get_offset_range (off, stmt, orng, rvals))
    {
      /* Set ORNG to the maximum offset representable in ptrdiff_t.  */
      orng[1] = wi::to_offset (TYPE_MAX_VALUE (ptrdiff_type_node));
      orng[0] = -orng[1] - 1;
    }

  pref->add_offset (orng[0], orng[1]);
  return true;
}

/* A helper of compute_objsize_r() to determine the size from SSA_NAME
   PTR.  Return true on success and false on failure.  */

static bool
handle_ssa_name (tree ptr, bool addr, int ostype,
		 access_ref *pref, ssa_name_limit_t &snlim,
		 pointer_query *qry)
{
  if (!snlim.next ())
    return false;

  /* Only process an SSA_NAME if the recursion limit has not yet
     been reached.  */
  if (qry)
    {
      if (++qry->depth > qry->max_depth)
	qry->max_depth = qry->depth;
      if (const access_ref *cache_ref = qry->get_ref (ptr, ostype))
	{
	  /* Add the number of DEREFerences accummulated so far.  */
	  const int deref = pref->deref;
	  *pref = *cache_ref;
	  pref->deref += deref;
	  return true;
	}
    }

  gimple *stmt = SSA_NAME_DEF_STMT (ptr);
  if (is_gimple_call (stmt))
    {
      /* If STMT is a call to an allocation function get the size
	 from its argument(s).  If successful, also set *PREF->REF
	 to PTR for the caller to include in diagnostics.  */
      wide_int wr[2];
      range_query *const rvals = qry ? qry->rvals : NULL;
      if (gimple_call_alloc_size (stmt, wr, rvals))
	{
	  pref->ref = ptr;
	  pref->sizrng[0] = offset_int::from (wr[0], UNSIGNED);
	  pref->sizrng[1] = offset_int::from (wr[1], UNSIGNED);
	  /* Constrain both bounds to a valid size.  */
	  offset_int maxsize = wi::to_offset (max_object_size ());
	  if (pref->sizrng[0] > maxsize)
	    pref->sizrng[0] = maxsize;
	  if (pref->sizrng[1] > maxsize)
	    pref->sizrng[1] = maxsize;
	}
      else
	{
	  /* For functions known to return one of their pointer arguments
	     try to determine what the returned pointer points to, and on
	     success add OFFRNG which was set to the offset added by
	     the function (e.g., memchr) to the overall offset.  */
	  bool past_end;
	  offset_int offrng[2];
	  if (tree ret = gimple_call_return_array (stmt, offrng, &past_end,
						   snlim, qry))
	    {
	      if (!compute_objsize_r (ret, stmt, addr, ostype, pref, snlim, qry))
		return false;

	      /* Cap OFFRNG[1] to at most the remaining size of
		 the object.  */
	      offset_int remrng[2];
	      remrng[1] = pref->size_remaining (remrng);
	      if (remrng[1] != 0 && !past_end)
		/* Decrement the size for functions that never return
		   a past-the-end pointer.  */
		remrng[1] -= 1;

	      if (remrng[1] < offrng[1])
		offrng[1] = remrng[1];
	      pref->add_offset (offrng[0], offrng[1]);
	    }
	  else
	    {
	      /* For other calls that might return arbitrary pointers
		 including into the middle of objects set the size
		 range to maximum, clear PREF->BASE0, and also set
		 PREF->REF to include in diagnostics.  */
	      pref->set_max_size_range ();
	      pref->base0 = false;
	      pref->ref = ptr;
	    }
	}
      qry->put_ref (ptr, *pref, ostype);
      return true;
    }

  if (gimple_nop_p (stmt))
    {
      /* For a function argument try to determine the byte size
	 of the array from the current function declaratation
	 (e.g., attribute access or related).  */
      wide_int wr[2];
      bool static_array = false;
      if (tree ref = gimple_parm_array_size (ptr, wr, &static_array))
	{
	  pref->parmarray = !static_array;
	  pref->sizrng[0] = offset_int::from (wr[0], UNSIGNED);
	  pref->sizrng[1] = offset_int::from (wr[1], UNSIGNED);
	  pref->ref = ref;
	  qry->put_ref (ptr, *pref, ostype);
	  return true;
	}

      pref->set_max_size_range ();
      pref->base0 = false;
      pref->ref = ptr;
      qry->put_ref (ptr, *pref, ostype);
      return true;
    }

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      /* Pass PTR to get_ref() via PREF.  If all PHI arguments refer
	 to the same object the function will replace it with it.  */
      pref->ref = ptr;
      access_ref phi_ref = *pref;
      if (!pref->get_ref (NULL, &phi_ref, ostype, &snlim, qry))
	return false;
      *pref = phi_ref;
      qry->put_ref (ptr, *pref, ostype);
      return true;
    }

  if (!is_gimple_assign (stmt))
    {
      /* Clear BASE0 since the assigned pointer might point into
	 the middle of the object, set the maximum size range and,
	 if the SSA_NAME refers to a function argumnent, set
	 PREF->REF to it.  */
      pref->base0 = false;
      pref->set_max_size_range ();
      pref->ref = ptr;
      return true;
    }

  tree_code code = gimple_assign_rhs_code (stmt);

  if (code == MAX_EXPR || code == MIN_EXPR)
    {
      if (!handle_min_max_size (ptr, ostype, pref, snlim, qry))
	return false;

      qry->put_ref (ptr, *pref, ostype);
      return true;
    }

  tree rhs = gimple_assign_rhs1 (stmt);

  if (code == POINTER_PLUS_EXPR
      && TREE_CODE (TREE_TYPE (rhs)) == POINTER_TYPE)
    {
      /* Compute the size of the object first. */
      if (!compute_objsize_r (rhs, stmt, addr, ostype, pref, snlim, qry))
	return false;

      offset_int orng[2];
      tree off = gimple_assign_rhs2 (stmt);
      range_query *const rvals = qry ? qry->rvals : NULL;
      if (get_offset_range (off, stmt, orng, rvals))
	pref->add_offset (orng[0], orng[1]);
      else
	pref->add_max_offset ();

      qry->put_ref (ptr, *pref, ostype);
      return true;
    }

  if (code == ADDR_EXPR || code == SSA_NAME)
    {
      if (!compute_objsize_r (rhs, stmt, addr, ostype, pref, snlim, qry))
	return false;
      qry->put_ref (ptr, *pref, ostype);
      return true;
    }

  if (ostype > 1 && POINTER_TYPE_P (TREE_TYPE (rhs)))
    {
      /* When determining the qualifiers follow the pointer but
	 avoid caching the result.  As the pointer is added to
	 and/or dereferenced the computed size and offset need
	 not be meaningful for other queries involving the same
	 pointer.  */
      if (!compute_objsize_r (rhs, stmt, addr, ostype, pref, snlim, qry))
	return false;

      rhs = pref->ref;
    }

  /* (This could also be an assignment from a nonlocal pointer.)  Save
     PTR to mention in diagnostics but otherwise treat it as a pointer
     to an unknown object.  */
  pref->ref = rhs;
  pref->base0 = false;
  pref->set_max_size_range ();
  return true;
}

/* Helper to compute the size of the object referenced by the PTR
   expression which must have pointer type, using Object Size type
   OSTYPE (only the least significant 2 bits are used).
   On success, sets PREF->REF to the DECL of the referenced object
   if it's unique, otherwise to null, PREF->OFFRNG to the range of
   offsets into it, and PREF->SIZRNG to the range of sizes of
   the object(s).
   ADDR is true for an enclosing ADDR_EXPR.
   SNLIM is used to avoid visiting the same PHI operand multiple
   times, and, when nonnull, RVALS to determine range information.
   Returns true on success, false when a meaningful size (or range)
   cannot be determined.

   The function is intended for diagnostics and should not be used
   to influence code generation or optimization.  */

static bool
compute_objsize_r (tree ptr, gimple *stmt, bool addr, int ostype,
		   access_ref *pref, ssa_name_limit_t &snlim,
		   pointer_query *qry)
{
  STRIP_NOPS (ptr);

  if (DECL_P (ptr))
    return handle_decl (ptr, addr, pref);

  switch (TREE_CODE (ptr))
    {
    case ADDR_EXPR:
      {
	tree ref = TREE_OPERAND (ptr, 0);
	if (!compute_objsize_r (ref, stmt, true, ostype, pref, snlim, qry))
	  return false;

	--pref->deref;
	return true;
      }

    case BIT_FIELD_REF:
      {
	tree ref = TREE_OPERAND (ptr, 0);
	if (!compute_objsize_r (ref, stmt, addr, ostype, pref, snlim, qry))
	  return false;

	offset_int off = wi::to_offset (pref->eval (TREE_OPERAND (ptr, 2)));
	pref->add_offset (off / BITS_PER_UNIT);
	return true;
      }

    case ARRAY_REF:
      return handle_array_ref (ptr, stmt, addr, ostype, pref, snlim, qry);

    case COMPONENT_REF:
      return handle_component_ref (ptr, stmt, addr, ostype, pref, snlim, qry);

    case MEM_REF:
      return handle_mem_ref (ptr, stmt, ostype, pref, snlim, qry);

    case TARGET_MEM_REF:
      {
	tree ref = TREE_OPERAND (ptr, 0);
	if (!compute_objsize_r (ref, stmt, addr, ostype, pref, snlim, qry))
	  return false;

	/* TODO: Handle remaining operands.  Until then, add maximum offset.  */
	pref->ref = ptr;
	pref->add_max_offset ();
	return true;
      }

    case INTEGER_CST:
      /* Pointer constants other than null smaller than param_min_pagesize
	 might be the result of erroneous null pointer addition/subtraction.
	 Unless zero is a valid address set size to zero.  For null pointers,
	 set size to the maximum for now since those may be the result of
	 jump threading.  Similarly, for values >= param_min_pagesize in
	 order to support (type *) 0x7cdeab00.  */
      if (integer_zerop (ptr)
	  || wi::to_widest (ptr) >= param_min_pagesize)
	pref->set_max_size_range ();
      else if (POINTER_TYPE_P (TREE_TYPE (ptr)))
	{
	  tree deref_type = TREE_TYPE (TREE_TYPE (ptr));
	  addr_space_t as = TYPE_ADDR_SPACE (deref_type);
	  if (targetm.addr_space.zero_address_valid (as))
	    pref->set_max_size_range ();
	  else
	    {
	      pref->sizrng[0] = pref->sizrng[1] = 0;
	      pref->ref_nullptr_p = true;
	    }
	}
      else
	pref->sizrng[0] = pref->sizrng[1] = 0;

      pref->ref = ptr;
      return true;

    case STRING_CST:
      pref->sizrng[0] = pref->sizrng[1] = TREE_STRING_LENGTH (ptr);
      pref->ref = ptr;
      return true;

    case POINTER_PLUS_EXPR:
    {
      tree ref = TREE_OPERAND (ptr, 0);
      if (!compute_objsize_r (ref, stmt, addr, ostype, pref, snlim, qry))
	return false;

      /* The below only makes sense if the offset is being applied to the
	 address of the object.  */
      if (pref->deref != -1)
	return false;

      offset_int orng[2];
      tree off = pref->eval (TREE_OPERAND (ptr, 1));
      if (get_offset_range (off, stmt, orng, qry->rvals))
	pref->add_offset (orng[0], orng[1]);
      else
	pref->add_max_offset ();
      return true;
    }

    case VIEW_CONVERT_EXPR:
      ptr = TREE_OPERAND (ptr, 0);
      return compute_objsize_r (ptr, stmt, addr, ostype, pref, snlim, qry);

    case SSA_NAME:
      return handle_ssa_name (ptr, addr, ostype, pref, snlim, qry);

    default:
      break;
    }

  /* Assume all other expressions point into an unknown object
     of the maximum valid size.  */
  pref->ref = ptr;
  pref->base0 = false;
  pref->set_max_size_range ();
  if (TREE_CODE (ptr) == SSA_NAME)
    qry->put_ref (ptr, *pref);
  return true;
}

/* A "public" wrapper around the above.  Clients should use this overload
   instead.  */

tree
compute_objsize (tree ptr, gimple *stmt, int ostype, access_ref *pref,
		 pointer_query *ptr_qry)
{
  pointer_query qry;
  if (ptr_qry)
    ptr_qry->depth = 0;
  else
    ptr_qry = &qry;

  /* Clear and invalidate in case *PREF is being reused.  */
  pref->offrng[0] = pref->offrng[1] = 0;
  pref->sizrng[0] = pref->sizrng[1] = -1;

  ssa_name_limit_t snlim;
  if (!compute_objsize_r (ptr, stmt, false, ostype, pref, snlim, ptr_qry))
    return NULL_TREE;

  offset_int maxsize = pref->size_remaining ();
  if (pref->base0 && pref->offrng[0] < 0 && pref->offrng[1] >= 0)
    pref->offrng[0] = 0;
  return wide_int_to_tree (sizetype, maxsize);
}

/* Transitional wrapper.  The function should be removed once callers
   transition to the pointer_query API.  */

tree
compute_objsize (tree ptr, gimple *stmt, int ostype, access_ref *pref,
		 range_query *rvals /* = NULL */)
{
  pointer_query qry;
  qry.rvals = rvals;
  return compute_objsize (ptr, stmt, ostype, pref, &qry);
}

/* Legacy wrapper around the above.  The function should be removed
   once callers transition to one of the two above.  */

tree
compute_objsize (tree ptr, gimple *stmt, int ostype, tree *pdecl /* = NULL */,
		 tree *poff /* = NULL */, range_query *rvals /* = NULL */)
{
  /* Set the initial offsets to zero and size to negative to indicate
     none has been computed yet.  */
  access_ref ref;
  tree size = compute_objsize (ptr, stmt, ostype, &ref, rvals);
  if (!size || !ref.base0)
    return NULL_TREE;

  if (pdecl)
    *pdecl = ref.ref;

  if (poff)
    *poff = wide_int_to_tree (ptrdiff_type_node, ref.offrng[ref.offrng[0] < 0]);

  return size;
}

/* Determine the offset *FLDOFF of the first byte of a struct member
   of TYPE (possibly recursively) into which the byte offset OFF points,
   starting after the field START_AFTER if it's non-null.  On success,
   if nonnull, set *FLDOFF to the offset of the first byte, and return
   the field decl.  If nonnull, set *NEXTOFF to the offset of the next
   field (which reflects any padding between the returned field and
   the next).  Otherwise, if no such member can be found, return null.  */

tree
field_at_offset (tree type, tree start_after, HOST_WIDE_INT off,
		 HOST_WIDE_INT *fldoff /* = nullptr */,
		 HOST_WIDE_INT *nextoff /* = nullptr */)
{
  tree first_fld = TYPE_FIELDS (type);

  HOST_WIDE_INT offbuf = 0, nextbuf = 0;
  if (!fldoff)
    fldoff = &offbuf;
  if (!nextoff)
    nextoff = &nextbuf;

  *nextoff = 0;

  /* The field to return.  */
  tree last_fld = NULL_TREE;
  /* The next field to advance to.  */
  tree next_fld = NULL_TREE;

  /* NEXT_FLD's cached offset.  */
  HOST_WIDE_INT next_pos = -1;

  for (tree fld = first_fld; fld; fld = next_fld)
    {
      next_fld = fld;
      do
	/* Advance to the next relevant data member.  */
	next_fld = TREE_CHAIN (next_fld);
      while (next_fld
	     && (TREE_CODE (next_fld) != FIELD_DECL
		 || DECL_ARTIFICIAL (next_fld)));

      if (TREE_CODE (fld) != FIELD_DECL || DECL_ARTIFICIAL (fld))
	continue;

      if (fld == start_after)
	continue;

      tree fldtype = TREE_TYPE (fld);
      /* The offset of FLD within its immediately enclosing structure.  */
      HOST_WIDE_INT fldpos = next_pos < 0 ? int_byte_position (fld) : next_pos;

      tree typesize = TYPE_SIZE_UNIT (fldtype);
      if (typesize && TREE_CODE (typesize) != INTEGER_CST)
	/* Bail if FLD is a variable length member.  */
	return NULL_TREE;

      /* If the size is not available the field is a flexible array
	 member.  Treat this case as success.  */
      HOST_WIDE_INT fldsize = (tree_fits_uhwi_p (typesize)
			       ? tree_to_uhwi (typesize)
			       : off);

      /* If OFF is beyond the end of the current field continue.  */
      HOST_WIDE_INT fldend = fldpos + fldsize;
      if (fldend < off)
	continue;

      if (next_fld)
	{
	  /* If OFF is equal to the offset of the next field continue
	     to it and skip the array/struct business below.  */
	  tree pos = byte_position (next_fld);
	  if (!tree_fits_shwi_p (pos))
	    /* Bail if NEXT_FLD is a variable length member.  */
	    return NULL_TREE;
	  next_pos = tree_to_shwi (pos);
	  *nextoff = *fldoff + next_pos;
	  if (*nextoff == off && TREE_CODE (type) != UNION_TYPE)
	    continue;
	}
      else
	*nextoff = HOST_WIDE_INT_MAX;

      /* OFF refers somewhere into the current field or just past its end,
	 which could mean it refers to the next field.  */
      if (TREE_CODE (fldtype) == ARRAY_TYPE)
	{
	  /* Will be set to the offset of the first byte of the array
	     element (which may be an array) of FLDTYPE into which
	     OFF - FLDPOS points (which may be past ELTOFF).  */
	  HOST_WIDE_INT eltoff = 0;
	  if (tree ft = array_elt_at_offset (fldtype, off - fldpos, &eltoff))
	    fldtype = ft;
	  else
	    continue;

	  /* Advance the position to include the array element above.
	     If OFF - FLPOS refers to a member of FLDTYPE, the member
	     will be determined below.  */
	  fldpos += eltoff;
	}

      *fldoff += fldpos;

      if (TREE_CODE (fldtype) == RECORD_TYPE)
	/* Drill down into the current field if it's a struct.  */
	fld = field_at_offset (fldtype, start_after, off - fldpos,
			       fldoff, nextoff);

      last_fld = fld;

      /* Unless the offset is just past the end of the field return it.
	 Otherwise save it and return it only if the offset of the next
	 next field is greater (i.e., there is padding between the two)
	 or if there is no next field.  */
      if (off < fldend)
	break;
    }

  if (*nextoff == HOST_WIDE_INT_MAX && next_fld)
    *nextoff = next_pos;

  return last_fld;
}

/* Determine the offset *ELTOFF of the first byte of the array element
   of array ARTYPE into which the byte offset OFF points.  On success
   set *ELTOFF to the offset of the first byte and return type.
   Otherwise, if no such element can be found, return null.  */

tree
array_elt_at_offset (tree artype, HOST_WIDE_INT off,
		     HOST_WIDE_INT *eltoff /* = nullptr */,
		     HOST_WIDE_INT *subar_size /* = nullptr */)
{
  gcc_assert (TREE_CODE (artype) == ARRAY_TYPE);

  HOST_WIDE_INT dummy;
  if (!eltoff)
    eltoff = &dummy;
  if (!subar_size)
    subar_size = &dummy;

  tree eltype = artype;
  while (TREE_CODE (TREE_TYPE (eltype)) == ARRAY_TYPE)
    eltype = TREE_TYPE (eltype);

  tree subartype = eltype;
  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (eltype))
      || TYPE_MODE (TREE_TYPE (eltype)) != TYPE_MODE (char_type_node))
    eltype = TREE_TYPE (eltype);

  *subar_size = int_size_in_bytes (subartype);

  if (eltype == artype)
    {
      *eltoff = 0;
      return artype;
    }

  HOST_WIDE_INT artype_size = int_size_in_bytes (artype);
  HOST_WIDE_INT eltype_size = int_size_in_bytes (eltype);

  if (off < artype_size)// * eltype_size)
    {
      *eltoff = (off / eltype_size) * eltype_size;
      return TREE_CODE (eltype) == ARRAY_TYPE ? TREE_TYPE (eltype) : eltype;
    }

  return NULL_TREE;
}

/* Wrapper around build_array_type_nelts that makes sure the array
   can be created at all and handles zero sized arrays specially.  */

tree
build_printable_array_type (tree eltype, unsigned HOST_WIDE_INT nelts)
{
  if (TYPE_SIZE_UNIT (eltype)
      && TREE_CODE (TYPE_SIZE_UNIT (eltype)) == INTEGER_CST
      && !integer_zerop (TYPE_SIZE_UNIT (eltype))
      && TYPE_ALIGN_UNIT (eltype) > 1
      && wi::zext (wi::to_wide (TYPE_SIZE_UNIT (eltype)),
		   ffs_hwi (TYPE_ALIGN_UNIT (eltype)) - 1) != 0)
    eltype = TYPE_MAIN_VARIANT (eltype);

  /* Consider excessive NELTS an array of unknown bound.  */
  tree idxtype = NULL_TREE;
  if (nelts < HOST_WIDE_INT_MAX)
    {
      if (nelts)
	return build_array_type_nelts (eltype, nelts);
      idxtype = build_range_type (sizetype, size_zero_node, NULL_TREE);
    }

  tree arrtype = build_array_type (eltype, idxtype);
  arrtype = build_distinct_type_copy (TYPE_MAIN_VARIANT (arrtype));
  TYPE_SIZE (arrtype) = bitsize_zero_node;
  TYPE_SIZE_UNIT (arrtype) = size_zero_node;
  return arrtype;
}
