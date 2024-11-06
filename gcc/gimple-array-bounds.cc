/* Array bounds checking.
   Copyright (C) 2005-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "pointer-query.h"
#include "gimple-array-bounds.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-dfa.h"
#include "fold-const.h"
#include "diagnostic-core.h"
#include "intl.h"
#include "tree-vrp.h"
#include "alloc-pool.h"
#include "vr-values.h"
#include "domwalk.h"
#include "tree-cfg.h"
#include "attribs.h"
#include "tree-pass.h"
#include "gimple-range.h"

// Always use the current range query for the bounds checker.
array_bounds_checker::array_bounds_checker (struct function *func)
  : fun (func), m_ptr_qry (get_range_query (func))
{
  /* No-op.  */
}

void
array_bounds_checker::get_value_range (irange &r, const_tree op, gimple *stmt)
{
  if (m_ptr_qry.rvals->range_of_expr (r, const_cast<tree> (op), stmt))
    return;
  r.set_varying (TREE_TYPE (op));
}

/* Try to determine the DECL that REF refers to.  Return the DECL or
   the expression closest to it.  Used in informational notes pointing
   to referenced objects or function parameters.  */

static tree
get_base_decl (tree ref)
{
  tree base = get_base_address (ref);
  if (DECL_P (base))
    return base;

  if (TREE_CODE (base) == MEM_REF)
    base = TREE_OPERAND (base, 0);

  if (TREE_CODE (base) != SSA_NAME)
    return base;

  do
    {
      gimple *def = SSA_NAME_DEF_STMT (base);
      if (gimple_assign_single_p (def))
	{
	  base = gimple_assign_rhs1 (def);
	  return base;
	}

      if (!gimple_nop_p (def))
	return base;

      break;
    } while (true);

  tree var = SSA_NAME_VAR (base);
  if (TREE_CODE (var) != PARM_DECL)
    return base;

  return var;
}

/* Return the constant byte size of the object or type referenced by
   the MEM_REF ARG.  On success, set *PREF to the DECL or expression
   ARG refers to.  Otherwise return null.  */

static tree
get_ref_size (tree arg, tree *pref)
{
  if (TREE_CODE (arg) != MEM_REF)
    return NULL_TREE;

  arg = TREE_OPERAND (arg, 0);
  tree type = TREE_TYPE (arg);
  if (!POINTER_TYPE_P (type))
    return NULL_TREE;

  type = TREE_TYPE (type);
  if (TREE_CODE (type) != ARRAY_TYPE)
    return NULL_TREE;

  tree nbytes = TYPE_SIZE_UNIT (type);
  if (!nbytes || TREE_CODE (nbytes) != INTEGER_CST)
    return NULL_TREE;

  *pref = get_base_decl (arg);
  return nbytes;
}

/* Return true if REF is (likely) an ARRAY_REF to a trailing array member
   of a struct.  It refines array_ref_flexible_size_p by detecting a pointer
   to an array and an array parameter declared using the [N] syntax (as
   opposed to a pointer) and returning false.  Set *PREF to the decl or
   expression REF refers to.  */

static bool
trailing_array (tree arg, tree *pref)
{
  tree ref = arg;
  tree base = get_base_decl (arg);
  while (TREE_CODE (ref) == ARRAY_REF || TREE_CODE (ref) == MEM_REF)
    ref = TREE_OPERAND (ref, 0);

  if (TREE_CODE (ref) == COMPONENT_REF)
    {
      *pref = TREE_OPERAND (ref, 1);
      tree type = TREE_TYPE (*pref);
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  /* A multidimensional trailing array is not considered special
	     no matter what its major bound is.  */
	  type = TREE_TYPE (type);
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    return false;
	}
    }
  else
    *pref = base;

  tree basetype = TREE_TYPE (base);
  if (TREE_CODE (base) == PARM_DECL
      && POINTER_TYPE_P (basetype))
    {
      tree ptype = TREE_TYPE (basetype);
      if (TREE_CODE (ptype) == ARRAY_TYPE)
	return false;
    }

  return array_ref_flexible_size_p (arg);
}

/* Acquire the upper bound and upper bound plus one for the array
   reference REF and record them into UP_BOUND and UP_BOUND_P1.
   Set *DECL to the decl or expresssion REF refers to.  */

static void
get_up_bounds_for_array_ref (tree ref, tree *decl,
			     tree *up_bound, tree *up_bound_p1)
{
  if (!(*up_bound)
      || TREE_CODE (*up_bound) != INTEGER_CST
      || trailing_array (ref, decl))
    {
      /* Accesses to trailing arrays via pointers may access storage
	 beyond the types array bounds.  For such arrays, or for flexible
	 array members, as well as for other arrays of an unknown size,
	 replace the upper bound with a more permissive one that assumes
	 the size of the largest object is PTRDIFF_MAX.  */
      tree eltsize = array_ref_element_size (ref);

      if (TREE_CODE (eltsize) != INTEGER_CST
	  || integer_zerop (eltsize))
	{
	  *up_bound = NULL_TREE;
	  *up_bound_p1 = NULL_TREE;
	}
      else
	{
	  tree ptrdiff_max = TYPE_MAX_VALUE (ptrdiff_type_node);
	  tree maxbound = ptrdiff_max;
	  tree arg = TREE_OPERAND (ref, 0);

	  const bool compref = TREE_CODE (arg) == COMPONENT_REF;
	  if (compref)
	    {
	      /* Try to determine the size of the trailing array from
		 its initializer (if it has one).  */
	      if (tree refsize = component_ref_size (arg))
		if (TREE_CODE (refsize) == INTEGER_CST)
		  maxbound = refsize;
	    }

	  if (maxbound == ptrdiff_max)
	    {
	      /* Try to determine the size of the base object.  Avoid
		 COMPONENT_REF already tried above.  Using its DECL_SIZE
		 size wouldn't necessarily be correct if the reference is
		 to its flexible array member initialized in a different
		 translation unit.  */
	      poly_int64 off;
	      if (tree base = get_addr_base_and_unit_offset (arg, &off))
		{
		  if (TREE_CODE (base) == MEM_REF)
		    {
		      /* Try to determine the size from a pointer to
			 an array if BASE is one.  */
		      if (tree size = get_ref_size (base, decl))
			maxbound = size;
		    }
		  else if (!compref && DECL_P (base))
		    if (tree basesize = DECL_SIZE_UNIT (base))
		      if (TREE_CODE (basesize) == INTEGER_CST)
			{
			  maxbound = basesize;
			  *decl = base;
			}

		  if (known_gt (off, 0))
		    maxbound = wide_int_to_tree (sizetype,
						 wi::sub (wi::to_wide (maxbound),
							  off));
		}
	    }
	  else
	    maxbound = fold_convert (sizetype, maxbound);

	  *up_bound_p1 = int_const_binop (TRUNC_DIV_EXPR, maxbound, eltsize);

	  if (*up_bound_p1 != NULL_TREE)
	    *up_bound = int_const_binop (MINUS_EXPR, *up_bound_p1,
					build_int_cst (ptrdiff_type_node, 1));
	  else
	    *up_bound = NULL_TREE;
	}
    }
  else
    *up_bound_p1 = int_const_binop (PLUS_EXPR, *up_bound,
				   build_int_cst (TREE_TYPE (*up_bound), 1));
  return;
}

/* Given the LOW_SUB_ORG, LOW_SUB and UP_SUB, and the computed UP_BOUND
   and UP_BOUND_P1, check whether the array reference REF is out of bound.
   When out of bounds, set OUT_OF_BOUND to true.
   Issue warnings if FOR_ARRAY_BOUND is true.
   return TRUE if warnings are issued.  */

static bool
check_out_of_bounds_and_warn (location_t location, tree ref,
			      tree low_sub_org, tree low_sub, tree up_sub,
			      tree up_bound, tree up_bound_p1,
			      const irange *vr,
			      bool ignore_off_by_one, bool for_array_bound,
			      bool *out_of_bound)
{
  tree min, max;
  tree low_bound = array_ref_low_bound (ref);
  tree artype = TREE_TYPE (TREE_OPERAND (ref, 0));

  bool warned = false;
  *out_of_bound = false;

  /* Empty array.  */
  if (up_bound && tree_int_cst_equal (low_bound, up_bound_p1))
    {
      *out_of_bound = true;
      if (for_array_bound)
	warned = warning_at (location, OPT_Warray_bounds_,
			     "array subscript %E is outside array"
			     " bounds of %qT", low_sub_org, artype);
    }

  if (warned)
    ; /* Do nothing.  */
  else if (get_legacy_range (*vr, min, max) == VR_ANTI_RANGE)
    {
      if (up_bound
	  && TREE_CODE (up_sub) == INTEGER_CST
	  && (ignore_off_by_one
	      ? tree_int_cst_lt (up_bound, up_sub)
	      : tree_int_cst_le (up_bound, up_sub))
	  && TREE_CODE (low_sub) == INTEGER_CST
	  && tree_int_cst_le (low_sub, low_bound))
	{
	  *out_of_bound = true;
	  if (for_array_bound)
	    warned = warning_at (location, OPT_Warray_bounds_,
				 "array subscript [%E, %E] is outside "
				 "array bounds of %qT",
				 low_sub, up_sub, artype);
	}
    }
  else if (up_bound
	   && TREE_CODE (up_sub) == INTEGER_CST
	   && (ignore_off_by_one
	       ? !tree_int_cst_le (up_sub, up_bound_p1)
	       : !tree_int_cst_le (up_sub, up_bound)))
    {
      *out_of_bound = true;
      if (for_array_bound)
	warned = warning_at (location, OPT_Warray_bounds_,
			     "array subscript %E is above array bounds of %qT",
			     up_sub, artype);
    }
  else if (TREE_CODE (low_sub) == INTEGER_CST
	   && tree_int_cst_lt (low_sub, low_bound))
    {
      *out_of_bound = true;
      if (for_array_bound)
	warned = warning_at (location, OPT_Warray_bounds_,
			     "array subscript %E is below array bounds of %qT",
			     low_sub, artype);
    }
  return warned;
}

/* Checks one ARRAY_REF in REF, located at LOCUS.  Ignores flexible
   arrays and "struct" hacks.  If VRP can determine that the array
   subscript is a constant, check if it is outside valid range.  If
   the array subscript is a RANGE, warn if it is non-overlapping with
   valid range.  IGNORE_OFF_BY_ONE is true if the ARRAY_REF is inside
   a ADDR_EXPR.  Return  true if a warning has been issued or if
   no-warning is set.  */

bool
array_bounds_checker::check_array_ref (location_t location, tree ref,
				       gimple *stmt, bool ignore_off_by_one)
{
  if (warning_suppressed_p (ref, OPT_Warray_bounds_))
    /* Return true to have the caller prevent warnings for enclosing
       refs.  */
    return true;

  /* Upper bound and Upper bound plus one for -Warray-bounds.  */
  tree up_bound = array_ref_up_bound (ref);
  tree up_bound_p1 = NULL_TREE;

  /* Referenced decl if one can be determined.  */
  tree decl = NULL_TREE;

  /* Set to the type of the special array member for a COMPONENT_REF.  */
  special_array_member sam{ };
  tree afield_decl = NULL_TREE;
  tree arg = TREE_OPERAND (ref, 0);

  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      /* Try to determine special array member type for this COMPONENT_REF.  */
      sam = component_ref_sam_type (arg);
      afield_decl = TREE_OPERAND (arg, 1);
    }

  get_up_bounds_for_array_ref (ref, &decl, &up_bound, &up_bound_p1);

  bool warned = false;
  bool out_of_bound = false;

  tree artype = TREE_TYPE (TREE_OPERAND (ref, 0));
  tree low_sub_org = TREE_OPERAND (ref, 1);
  tree up_sub = low_sub_org;
  tree low_sub = low_sub_org;

  int_range_max vr;
  if (TREE_CODE (low_sub_org) == SSA_NAME)
    {
      get_value_range (vr, low_sub_org, stmt);
      if (!vr.undefined_p () && !vr.varying_p ())
	{
	  tree min, max;
	  value_range_kind kind = get_legacy_range (vr, min, max);
	  low_sub = kind == VR_RANGE ? max : min;
	  up_sub = kind == VR_RANGE ? min : max;
	}
    }

  warned = check_out_of_bounds_and_warn (location, ref,
					 low_sub_org, low_sub, up_sub,
					 up_bound, up_bound_p1, &vr,
					 ignore_off_by_one, warn_array_bounds,
					 &out_of_bound);


  if (!warned && sam == special_array_member::int_0)
    warned = warning_at (location, OPT_Wzero_length_bounds,
			 (TREE_CODE (low_sub) == INTEGER_CST
			  ? G_("array subscript %E is outside the bounds "
			       "of an interior zero-length array %qT")
			  : G_("array subscript %qE is outside the bounds "
			       "of an interior zero-length array %qT")),
			 low_sub, artype);

  if (warned && dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Array bound warning for ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, ref);
      fprintf (dump_file, "\n");
    }

   /* Issue warnings for -Wstrict-flex-arrays according to the level of
      flag_strict_flex_arrays.  */
  if (out_of_bound && warn_strict_flex_arrays
      && (sam == special_array_member::trail_0
	  || sam == special_array_member::trail_1
	  || sam == special_array_member::trail_n)
      && DECL_NOT_FLEXARRAY (afield_decl))
    {
      bool warned1
	= warning_at (location, OPT_Wstrict_flex_arrays,
		      "trailing array %qT should not be used as "
		      "a flexible array member",
		      artype);

      if (warned1 && dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Trailing non flexible-like array bound warning for ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, ref);
	  fprintf (dump_file, "\n");
	}
      warned |= warned1;
    }

  if (warned)
    {
      /* Avoid more warnings when checking more significant subscripts
	 of the same expression.  */
      ref = TREE_OPERAND (ref, 0);
      suppress_warning (ref, OPT_Warray_bounds_);
      suppress_warning (ref, OPT_Wstrict_flex_arrays);

      if (decl)
	ref = decl;

      tree rec = NULL_TREE;
      if (TREE_CODE (ref) == COMPONENT_REF)
	{
	  /* For a reference to a member of a struct object also mention
	     the object if it's known.  It may be defined in a different
	     function than the out-of-bounds access.  */
	  rec = TREE_OPERAND (ref, 0);
	  if (!VAR_P (rec))
	    rec = NULL_TREE;
	  ref = TREE_OPERAND (ref, 1);
	}

      if (DECL_P (ref))
	inform (DECL_SOURCE_LOCATION (ref), "while referencing %qD", ref);
      if (rec && DECL_P (rec))
	inform (DECL_SOURCE_LOCATION (rec), "defined here %qD", rec);
    }

  return warned;
}

/* Checks one MEM_REF in REF, located at LOCATION, for out-of-bounds
   references to string constants.  If VRP can determine that the array
   subscript is a constant, check if it is outside valid range.
   If the array subscript is a RANGE, warn if it is non-overlapping
   with valid range.
   IGNORE_OFF_BY_ONE is true if the MEM_REF is inside an ADDR_EXPR
   (used to allow one-past-the-end indices for code that takes
   the address of the just-past-the-end element of an array).
   Returns true if a warning has been issued.  */

bool
array_bounds_checker::check_mem_ref (location_t location, tree ref,
				     bool ignore_off_by_one)
{
  if (warning_suppressed_p (ref, OPT_Warray_bounds_))
    return false;

  /* The statement used to allocate the array or null.  */
  gimple *alloc_stmt = NULL;
  /* For an allocation statement, the low bound of the size range.  */
  offset_int minbound = 0;
  /* The type and size of the access.  */
  tree axstype = TREE_TYPE (ref);
  offset_int axssize = 0;
  if (tree access_size = TYPE_SIZE_UNIT (axstype))
    if (TREE_CODE (access_size) == INTEGER_CST)
      axssize = wi::to_offset (access_size);

  access_ref aref;
  if (!m_ptr_qry.get_ref (ref, m_stmt, &aref, 0))
    return false;

  if (aref.offset_in_range (axssize))
    return false;

  if (TREE_CODE (aref.ref) == SSA_NAME)
    {
      gimple *def = SSA_NAME_DEF_STMT (aref.ref);
      if (is_gimple_call (def))
	{
	  /* Save the allocation call and the low bound on the size.  */
	  alloc_stmt = def;
	  minbound = aref.sizrng[0];
	}
    }

  /* The range of the byte offset into the reference.  Adjusted below.  */
  offset_int offrange[2] = { aref.offrng[0], aref.offrng[1] };

  /* The type of the referenced object.  */
  tree reftype = TREE_TYPE (aref.ref);
  /* The size of the referenced array element.  */
  offset_int eltsize = 1;
  if (POINTER_TYPE_P (reftype))
    reftype = TREE_TYPE (reftype);

  if (TREE_CODE (reftype) == FUNCTION_TYPE)
    /* Restore the original (pointer) type and avoid trying to create
       an array of functions (done below).  */
    reftype = TREE_TYPE (aref.ref);
  else
    {
      /* The byte size of the array has already been determined above
	 based on a pointer ARG.  Set ELTSIZE to the size of the type
	 it points to and REFTYPE to the array with the size, rounded
	 down as necessary.  */
      if (TREE_CODE (reftype) == ARRAY_TYPE)
	reftype = TREE_TYPE (reftype);
      if (tree refsize = TYPE_SIZE_UNIT (reftype))
	if (TREE_CODE (refsize) == INTEGER_CST)
	  eltsize = wi::to_offset (refsize);

      const offset_int nelts = aref.sizrng[1] / eltsize;
      reftype = build_printable_array_type (reftype, nelts.to_uhwi ());
    }

  /* Compute the more permissive upper bound when IGNORE_OFF_BY_ONE
     is set (when taking the address of the one-past-last element
     of an array) but always use the stricter bound in diagnostics. */
  offset_int ubound = aref.sizrng[1];
  if (ignore_off_by_one)
    ubound += eltsize;

  /* Set if the lower bound of the subscript is out of bounds.  */
  const bool lboob = (aref.sizrng[1] == 0
		      || offrange[0] >= ubound
		      || offrange[1] < 0);
  /* Set if only the upper bound of the subscript is out of bounds.
     This can happen when using a bigger type to index into an array
     of a smaller type, as is common with unsigned char.  */
  const bool uboob = !lboob && offrange[0] + axssize > ubound;
  if (lboob || uboob)
    {
      /* Treat a reference to a non-array object as one to an array
	 of a single element.  */
      if (TREE_CODE (reftype) != ARRAY_TYPE)
	reftype = build_printable_array_type (reftype, 1);

      /* Extract the element type out of MEM_REF and use its size
	 to compute the index to print in the diagnostic; arrays
	 in MEM_REF don't mean anything.  A type with no size like
	 void is as good as having a size of 1.  */
      tree type = strip_array_types (TREE_TYPE (ref));
      if (tree size = TYPE_SIZE_UNIT (type))
	{
	  offrange[0] = offrange[0] / wi::to_offset (size);
	  offrange[1] = offrange[1] / wi::to_offset (size);
	}
    }

  bool warned = false;
  if (lboob)
    {
      if (offrange[0] == offrange[1])
	warned = warning_at (location, OPT_Warray_bounds_,
			     "array subscript %wi is outside array bounds "
			     "of %qT",
			     offrange[0].to_shwi (), reftype);
      else
	warned = warning_at (location, OPT_Warray_bounds_,
			     "array subscript [%wi, %wi] is outside "
			     "array bounds of %qT",
			     offrange[0].to_shwi (),
			     offrange[1].to_shwi (), reftype);
    }
  else if (uboob && !ignore_off_by_one)
    {
      tree backtype = reftype;
      if (alloc_stmt)
	/* If the memory was dynamically allocated refer to it as if
	   it were an untyped array of bytes.  */
	backtype = build_array_type_nelts (unsigned_char_type_node,
					   aref.sizrng[1].to_uhwi ());

      warned = warning_at (location, OPT_Warray_bounds_,
			   "array subscript %<%T[%wi]%> is partly "
			   "outside array bounds of %qT",
			   axstype, offrange[0].to_shwi (), backtype);
    }

  if (warned)
    {
      /* TODO: Determine the access from the statement and use it.  */
      aref.inform_access (access_none);
      suppress_warning (ref, OPT_Warray_bounds_);
      return true;
    }

  if (warn_array_bounds < 2)
    return false;

  /* At level 2 check also intermediate offsets.  */
  int i = 0;
  if (aref.offmax[i] < -aref.sizrng[1] || aref.offmax[i = 1] > ubound)
    {
      HOST_WIDE_INT tmpidx = (aref.offmax[i] / eltsize).to_shwi ();

      if (warning_at (location, OPT_Warray_bounds_,
		      "intermediate array offset %wi is outside array bounds "
		      "of %qT", tmpidx, reftype))
	{
	  suppress_warning (ref, OPT_Warray_bounds_);
	  return true;
	}
    }

  return false;
}

/* Searches if the expr T, located at LOCATION computes
   address of an ARRAY_REF, and call check_array_ref on it.  */

void
array_bounds_checker::check_addr_expr (location_t location, tree t,
				       gimple *stmt)
{
  /* For the most significant subscript only, accept taking the address
     of the just-past-the-end element.  */
  bool ignore_off_by_one = true;

  /* Check each ARRAY_REF and MEM_REF in the reference chain. */
  do
    {
      bool warned = false;
      if (TREE_CODE (t) == ARRAY_REF)
	{
	  warned = check_array_ref (location, t, stmt, ignore_off_by_one);
	  ignore_off_by_one = false;
	}
      else if (TREE_CODE (t) == MEM_REF)
	warned = check_mem_ref (location, t, ignore_off_by_one);

      if (warned)
	suppress_warning (t, OPT_Warray_bounds_);

      t = TREE_OPERAND (t, 0);
    }
  while (handled_component_p (t) || TREE_CODE (t) == MEM_REF);

  if (TREE_CODE (t) != MEM_REF
      || TREE_CODE (TREE_OPERAND (t, 0)) != ADDR_EXPR
      || warning_suppressed_p (t, OPT_Warray_bounds_))
    return;

  tree tem = TREE_OPERAND (TREE_OPERAND (t, 0), 0);
  tree low_bound, up_bound, el_sz;
  if (TREE_CODE (TREE_TYPE (tem)) != ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (TREE_TYPE (tem))) == ARRAY_TYPE
      || !TYPE_DOMAIN (TREE_TYPE (tem)))
    return;

  low_bound = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (tem)));
  up_bound = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (tem)));
  el_sz = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (tem)));
  if (!low_bound
      || TREE_CODE (low_bound) != INTEGER_CST
      || !up_bound
      || TREE_CODE (up_bound) != INTEGER_CST
      || !el_sz
      || TREE_CODE (el_sz) != INTEGER_CST)
    return;

  offset_int idx;
  if (!mem_ref_offset (t).is_constant (&idx))
    return;

  bool warned = false;
  idx = wi::sdiv_trunc (idx, wi::to_offset (el_sz));
  if (idx < 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Array bound warning for ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, t);
	  fprintf (dump_file, "\n");
	}
      warned = warning_at (location, OPT_Warray_bounds_,
			   "array subscript %wi is below "
			   "array bounds of %qT",
			   idx.to_shwi (), TREE_TYPE (tem));
    }
  else if (idx > (wi::to_offset (up_bound)
		  - wi::to_offset (low_bound) + 1))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Array bound warning for ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, t);
	  fprintf (dump_file, "\n");
	}
      warned = warning_at (location, OPT_Warray_bounds_,
			   "array subscript %wu is above "
			   "array bounds of %qT",
			   idx.to_uhwi (), TREE_TYPE (tem));
    }

  if (warned)
    {
      if (DECL_P (t))
	inform (DECL_SOURCE_LOCATION (t), "while referencing %qD", t);

      suppress_warning (t, OPT_Warray_bounds_);
    }
}

/* Return true if T is a reference to a member of a base class that's within
   the bounds of the enclosing complete object.  The function "hacks" around
   problems discussed in pr98266 and pr97595.  */

static bool
inbounds_memaccess_p (tree t, gimple *stmt)
{
  if (TREE_CODE (t) != COMPONENT_REF)
    return false;

  tree mref = TREE_OPERAND (t, 0);
  if (TREE_CODE (mref) != MEM_REF)
    return false;

  /* Consider the access if its type is a derived class.  */
  tree mreftype = TREE_TYPE (mref);
  if (!RECORD_OR_UNION_TYPE_P (mreftype)
      || !TYPE_BINFO (mreftype))
    return false;

  /* Compute the size of the referenced object (it could be dynamically
     allocated).  */
  access_ref aref;   // unused
  tree refop = TREE_OPERAND (mref, 0);
  tree refsize = compute_objsize (refop, stmt, 1, &aref);
  if (!refsize || TREE_CODE (refsize) != INTEGER_CST)
    return false;

  /* Compute the byte offset of the member within its enclosing class.  */
  tree fld = TREE_OPERAND (t, 1);
  tree fldpos = byte_position (fld);
  if (TREE_CODE (fldpos) != INTEGER_CST)
    return false;

  /* Compute the byte offset of the member with the outermost complete
     object by adding its offset computed above to the MEM_REF offset.  */
  tree refoff = TREE_OPERAND (mref, 1);
  tree fldoff = int_const_binop (PLUS_EXPR, fldpos, refoff);
  /* Return false if the member offset is greater or equal to the size
     of the complete object.  */
  if (!tree_int_cst_lt (fldoff, refsize))
    return false;

  tree fldsiz = DECL_SIZE_UNIT (fld);
  if (!fldsiz || TREE_CODE (fldsiz) != INTEGER_CST)
    return false;

  /* Return true if the offset just past the end of the member is less
     than or equal to the size of the complete object.  */
  tree fldend = int_const_binop (PLUS_EXPR, fldoff, fldsiz);
  return tree_int_cst_le (fldend, refsize);
}

/* Callback for walk_tree to check a tree for out of bounds array
   accesses.  The array_bounds_checker class is passed in DATA.  */

tree
array_bounds_checker::check_array_bounds (tree *tp, int *walk_subtree,
					  void *data)
{
  tree t = *tp;
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;

  location_t location;

  if (EXPR_HAS_LOCATION (t))
    location = EXPR_LOCATION (t);
  else
    location = gimple_location (wi->stmt);

  *walk_subtree = true;

  bool warned = false;
  array_bounds_checker *checker = (array_bounds_checker *) wi->info;
  gcc_assert (checker->m_stmt == wi->stmt);

  if (TREE_CODE (t) == ARRAY_REF)
    warned = checker->check_array_ref (location, t, wi->stmt,
				       false/*ignore_off_by_one*/);
  else if (TREE_CODE (t) == MEM_REF)
    warned = checker->check_mem_ref (location, t,
				     false /*ignore_off_by_one*/);
  else if (TREE_CODE (t) == ADDR_EXPR)
    {
      checker->check_addr_expr (location, t, wi->stmt);
      *walk_subtree = false;
    }
  else if (inbounds_memaccess_p (t, wi->stmt))
    /* Hack: Skip MEM_REF checks in accesses to a member of a base class
       at an offset that's within the bounds of the enclosing object.
       See pr98266 and pr97595.  */
    *walk_subtree = false;

  /* Propagate the no-warning bit to the outer statement to avoid also
     issuing -Wstringop-overflow/-overread for the out-of-bounds accesses.  */
  if (warned)
    suppress_warning (wi->stmt, OPT_Warray_bounds_);

  return NULL_TREE;
}

/* A dom_walker subclass for use by check_all_array_refs, to walk over
   all statements of all reachable BBs and call check_array_bounds on
   them.  */

class check_array_bounds_dom_walker : public dom_walker
{
public:
  check_array_bounds_dom_walker (array_bounds_checker *checker)
    : dom_walker (CDI_DOMINATORS, REACHABLE_BLOCKS),
    checker (checker) { }
  ~check_array_bounds_dom_walker () {}

  edge before_dom_children (basic_block) final override;

private:
  array_bounds_checker *checker;
};

/* Implementation of dom_walker::before_dom_children.

   Walk over all statements of BB and call check_array_bounds on them,
   and determine if there's a unique successor edge.  */

edge
check_array_bounds_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator si;
  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      if (!gimple_has_location (stmt)
	  || is_gimple_debug (stmt))
	continue;

      struct walk_stmt_info wi{ };
      wi.info = checker;
      checker->m_stmt = stmt;

      walk_gimple_op (stmt, array_bounds_checker::check_array_bounds, &wi);
    }

  /* Determine if there's a unique successor edge, and if so, return
     that back to dom_walker, ensuring that we don't visit blocks that
     became unreachable during the VRP propagation
     (PR tree-optimization/83312).  */
  return find_taken_edge (bb, NULL_TREE);
}

void
array_bounds_checker::check ()
{
  check_array_bounds_dom_walker w (this);
  w.walk (ENTRY_BLOCK_PTR_FOR_FN (fun));
}

const pass_data pass_data_array_bounds =
{
  GIMPLE_PASS, /* type */
  "bounds", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_ARRAY_BOUNDS, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( 0 ),  /* No TODOs */
};

class pass_array_bounds : public gimple_opt_pass
{
public:
  pass_array_bounds (gcc::context *ctxt, const pass_data &data_)
    : gimple_opt_pass (data_, ctxt), data (data_)
    { }

  /* opt_pass methods: */
  opt_pass * clone () final override
    { return new pass_array_bounds (m_ctxt, data); }
  bool gate (function *) final override
    {
      // Gate on the VRP pass to preserve previous behavior.
      return flag_tree_vrp && (warn_array_bounds || warn_strict_flex_arrays);
    }
  unsigned int execute (function *fun) final override
    {
      calculate_dominance_info (CDI_DOMINATORS);
      // Enable ranger as the current range query.
      enable_ranger (fun, false);
      array_bounds_checker array_checker (fun);
      array_checker.check ();
      disable_ranger (fun);
      return 0;
    }

 private:
  const pass_data &data;
}; // class pass_array_bounds

gimple_opt_pass *
make_pass_array_bounds (gcc::context *ctxt)
{
  return new pass_array_bounds (ctxt, pass_data_array_bounds);
}
