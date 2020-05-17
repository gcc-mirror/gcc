/* Array bounds checking.
   Copyright (C) 2005-2020 Free Software Foundation, Inc.

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

// This purposely returns a value_range, not a value_range_equiv, to
// break the dependency on equivalences for this pass.

const value_range *
array_bounds_checker::get_value_range (const_tree op)
{
  return ranges->get_value_range (op);
}

/* Checks one ARRAY_REF in REF, located at LOCUS. Ignores flexible
   arrays and "struct" hacks. If VRP can determine that the array
   subscript is a constant, check if it is outside valid range.  If
   the array subscript is a RANGE, warn if it is non-overlapping with
   valid range.  IGNORE_OFF_BY_ONE is true if the ARRAY_REF is inside
   a ADDR_EXPR.  Returns true if a warning has been issued.  */

bool
array_bounds_checker::check_array_ref (location_t location, tree ref,
				       bool ignore_off_by_one)
{
  if (TREE_NO_WARNING (ref))
    return false;

  tree low_sub = TREE_OPERAND (ref, 1);
  tree up_sub = low_sub;
  tree up_bound = array_ref_up_bound (ref);

  /* Referenced decl if one can be determined.  */
  tree decl = NULL_TREE;

  /* Set for accesses to interior zero-length arrays.  */
  bool interior_zero_len = false;

  tree up_bound_p1;

  if (!up_bound
      || TREE_CODE (up_bound) != INTEGER_CST
      || (warn_array_bounds < 2
	  && array_at_struct_end_p (ref)))
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
	  up_bound = NULL_TREE;
	  up_bound_p1 = NULL_TREE;
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
	      if (tree refsize = component_ref_size (arg, &interior_zero_len))
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
		  if (!compref && DECL_P (base))
		    if (tree basesize = DECL_SIZE_UNIT (base))
		      if (TREE_CODE (basesize) == INTEGER_CST)
			{
			  maxbound = basesize;
			  decl = base;
			}

		  if (known_gt (off, 0))
		    maxbound = wide_int_to_tree (sizetype,
						 wi::sub (wi::to_wide (maxbound),
							  off));
		}
	    }
	  else
	    maxbound = fold_convert (sizetype, maxbound);

	  up_bound_p1 = int_const_binop (TRUNC_DIV_EXPR, maxbound, eltsize);

	  if (up_bound_p1 != NULL_TREE)
	    up_bound = int_const_binop (MINUS_EXPR, up_bound_p1,
					build_int_cst (ptrdiff_type_node, 1));
	  else
	    up_bound = NULL_TREE;
	}
    }
  else
    up_bound_p1 = int_const_binop (PLUS_EXPR, up_bound,
				   build_int_cst (TREE_TYPE (up_bound), 1));

  tree low_bound = array_ref_low_bound (ref);

  tree artype = TREE_TYPE (TREE_OPERAND (ref, 0));

  bool warned = false;

  /* Empty array.  */
  if (up_bound && tree_int_cst_equal (low_bound, up_bound_p1))
    warned = warning_at (location, OPT_Warray_bounds,
			 "array subscript %E is outside array bounds of %qT",
			 low_sub, artype);

  const value_range *vr = NULL;
  if (TREE_CODE (low_sub) == SSA_NAME)
    {
      vr = get_value_range (low_sub);
      if (!vr->undefined_p () && !vr->varying_p ())
	{
	  low_sub = vr->kind () == VR_RANGE ? vr->max () : vr->min ();
	  up_sub = vr->kind () == VR_RANGE ? vr->min () : vr->max ();
	}
    }

  if (warned)
    ; /* Do nothing.  */
  else if (vr && vr->kind () == VR_ANTI_RANGE)
    {
      if (up_bound
	  && TREE_CODE (up_sub) == INTEGER_CST
	  && (ignore_off_by_one
	      ? tree_int_cst_lt (up_bound, up_sub)
	      : tree_int_cst_le (up_bound, up_sub))
	  && TREE_CODE (low_sub) == INTEGER_CST
	  && tree_int_cst_le (low_sub, low_bound))
	warned = warning_at (location, OPT_Warray_bounds,
			     "array subscript [%E, %E] is outside "
			     "array bounds of %qT",
			     low_sub, up_sub, artype);
    }
  else if (up_bound
	   && TREE_CODE (up_sub) == INTEGER_CST
	   && (ignore_off_by_one
	       ? !tree_int_cst_le (up_sub, up_bound_p1)
	       : !tree_int_cst_le (up_sub, up_bound)))
    warned = warning_at (location, OPT_Warray_bounds,
			 "array subscript %E is above array bounds of %qT",
			 up_sub, artype);
  else if (TREE_CODE (low_sub) == INTEGER_CST
	   && tree_int_cst_lt (low_sub, low_bound))
    warned = warning_at (location, OPT_Warray_bounds,
			 "array subscript %E is below array bounds of %qT",
			 low_sub, artype);

  if (!warned && interior_zero_len)
    warned = warning_at (location, OPT_Wzero_length_bounds,
			 (TREE_CODE (low_sub) == INTEGER_CST
			  ? G_("array subscript %E is outside the bounds "
			       "of an interior zero-length array %qT")
			  : G_("array subscript %qE is outside the bounds "
			       "of an interior zero-length array %qT")),
			 low_sub, artype);

  if (warned)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Array bound warning for ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, ref);
	  fprintf (dump_file, "\n");
	}

      ref = decl ? decl : TREE_OPERAND (ref, 0);

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

      TREE_NO_WARNING (ref) = 1;
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
  if (TREE_NO_WARNING (ref))
    return false;

  tree arg = TREE_OPERAND (ref, 0);
  /* The constant and variable offset of the reference.  */
  tree cstoff = TREE_OPERAND (ref, 1);
  tree varoff = NULL_TREE;

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  /* The array or string constant bounds in bytes.  Initially set
     to [-MAXOBJSIZE - 1, MAXOBJSIZE]  until a tighter bound is
     determined.  */
  offset_int arrbounds[2] = { -maxobjsize - 1, maxobjsize };

  /* The minimum and maximum intermediate offset.  For a reference
     to be valid, not only does the final offset/subscript must be
     in bounds but all intermediate offsets should be as well.
     GCC may be able to deal gracefully with such out-of-bounds
     offsets so the checking is only enbaled at -Warray-bounds=2
     where it may help detect bugs in uses of the intermediate
     offsets that could otherwise not be detectable.  */
  offset_int ioff = wi::to_offset (fold_convert (ptrdiff_type_node, cstoff));
  offset_int extrema[2] = { 0, wi::abs (ioff) };

  /* The range of the byte offset into the reference.  */
  offset_int offrange[2] = { 0, 0 };

  const value_range *vr = NULL;

  /* Determine the offsets and increment OFFRANGE for the bounds of each.
     The loop computes the range of the final offset for expressions such
     as (A + i0 + ... + iN)[CSTOFF] where i0 through iN are SSA_NAMEs in
     some range.  */
  const unsigned limit = param_ssa_name_def_chain_limit;
  for (unsigned n = 0; TREE_CODE (arg) == SSA_NAME && n < limit; ++n)
    {
      gimple *def = SSA_NAME_DEF_STMT (arg);
      if (!is_gimple_assign (def))
	break;

      tree_code code = gimple_assign_rhs_code (def);
      if (code == POINTER_PLUS_EXPR)
	{
	  arg = gimple_assign_rhs1 (def);
	  varoff = gimple_assign_rhs2 (def);
	}
      else if (code == ASSERT_EXPR)
	{
	  arg = TREE_OPERAND (gimple_assign_rhs1 (def), 0);
	  continue;
	}
      else
	return false;

      /* VAROFF should always be a SSA_NAME here (and not even
	 INTEGER_CST) but there's no point in taking chances.  */
      if (TREE_CODE (varoff) != SSA_NAME)
	break;

      vr = get_value_range (varoff);
      if (!vr || vr->undefined_p () || vr->varying_p ())
	break;

      if (!vr->constant_p ())
	break;

      if (vr->kind () == VR_RANGE)
	{
	  offset_int min
	    = wi::to_offset (fold_convert (ptrdiff_type_node, vr->min ()));
	  offset_int max
	    = wi::to_offset (fold_convert (ptrdiff_type_node, vr->max ()));
	  if (min < max)
	    {
	      offrange[0] += min;
	      offrange[1] += max;
	    }
	  else
	    {
	      /* When MIN >= MAX, the offset is effectively in a union
		 of two ranges: [-MAXOBJSIZE -1, MAX] and [MIN, MAXOBJSIZE].
		 Since there is no way to represent such a range across
		 additions, conservatively add [-MAXOBJSIZE -1, MAXOBJSIZE]
		 to OFFRANGE.  */
	      offrange[0] += arrbounds[0];
	      offrange[1] += arrbounds[1];
	    }
	}
      else
	{
	  /* For an anti-range, analogously to the above, conservatively
	     add [-MAXOBJSIZE -1, MAXOBJSIZE] to OFFRANGE.  */
	  offrange[0] += arrbounds[0];
	  offrange[1] += arrbounds[1];
	}

      /* Keep track of the minimum and maximum offset.  */
      if (offrange[1] < 0 && offrange[1] < extrema[0])
	extrema[0] = offrange[1];
      if (offrange[0] > 0 && offrange[0] > extrema[1])
	extrema[1] = offrange[0];

      if (offrange[0] < arrbounds[0])
	offrange[0] = arrbounds[0];

      if (offrange[1] > arrbounds[1])
	offrange[1] = arrbounds[1];
    }

  if (TREE_CODE (arg) == ADDR_EXPR)
    {
      arg = TREE_OPERAND (arg, 0);
      if (TREE_CODE (arg) != STRING_CST
	  && TREE_CODE (arg) != PARM_DECL
	  && TREE_CODE (arg) != VAR_DECL)
	return false;
    }
  else
    return false;

  /* The type of the object being referred to.  It can be an array,
     string literal, or a non-array type when the MEM_REF represents
     a reference/subscript via a pointer to an object that is not
     an element of an array.  Incomplete types are excluded as well
     because their size is not known.  */
  tree reftype = TREE_TYPE (arg);
  if (POINTER_TYPE_P (reftype)
      || !COMPLETE_TYPE_P (reftype)
      || TREE_CODE (TYPE_SIZE_UNIT (reftype)) != INTEGER_CST)
    return false;

  /* Except in declared objects, references to trailing array members
     of structs and union objects are excluded because MEM_REF doesn't
     make it possible to identify the member where the reference
     originated.  */
  if (RECORD_OR_UNION_TYPE_P (reftype)
      && (!VAR_P (arg)
	  || (DECL_EXTERNAL (arg) && array_at_struct_end_p (ref))))
    return false;

  arrbounds[0] = 0;

  offset_int eltsize;
  if (TREE_CODE (reftype) == ARRAY_TYPE)
    {
      eltsize = wi::to_offset (TYPE_SIZE_UNIT (TREE_TYPE (reftype)));
      if (tree dom = TYPE_DOMAIN (reftype))
	{
	  tree bnds[] = { TYPE_MIN_VALUE (dom), TYPE_MAX_VALUE (dom) };
	  if (TREE_CODE (arg) == COMPONENT_REF)
	    {
	      offset_int size = maxobjsize;
	      if (tree fldsize = component_ref_size (arg))
		size = wi::to_offset (fldsize);
	      arrbounds[1] = wi::lrshift (size, wi::floor_log2 (eltsize));
	    }
	  else if (array_at_struct_end_p (arg) || !bnds[0] || !bnds[1])
	    arrbounds[1] = wi::lrshift (maxobjsize, wi::floor_log2 (eltsize));
	  else
	    arrbounds[1] = (wi::to_offset (bnds[1]) - wi::to_offset (bnds[0])
			    + 1) * eltsize;
	}
      else
	arrbounds[1] = wi::lrshift (maxobjsize, wi::floor_log2 (eltsize));

      /* Determine a tighter bound of the non-array element type.  */
      tree eltype = TREE_TYPE (reftype);
      while (TREE_CODE (eltype) == ARRAY_TYPE)
	eltype = TREE_TYPE (eltype);
      eltsize = wi::to_offset (TYPE_SIZE_UNIT (eltype));
    }
  else
    {
      eltsize = 1;
      tree size = TYPE_SIZE_UNIT (reftype);
      if (VAR_P (arg))
	if (tree initsize = DECL_SIZE_UNIT (arg))
	  if (tree_int_cst_lt (size, initsize))
	    size = initsize;

      arrbounds[1] = wi::to_offset (size);
    }

  offrange[0] += ioff;
  offrange[1] += ioff;

  /* Compute the more permissive upper bound when IGNORE_OFF_BY_ONE
     is set (when taking the address of the one-past-last element
     of an array) but always use the stricter bound in diagnostics. */
  offset_int ubound = arrbounds[1];
  if (ignore_off_by_one)
    ubound += 1;

  if (arrbounds[0] == arrbounds[1]
      || offrange[0] >= ubound
      || offrange[1] < arrbounds[0])
    {
      /* Treat a reference to a non-array object as one to an array
	 of a single element.  */
      if (TREE_CODE (reftype) != ARRAY_TYPE)
	reftype = build_array_type_nelts (reftype, 1);

      /* Extract the element type out of MEM_REF and use its size
	 to compute the index to print in the diagnostic; arrays
	 in MEM_REF don't mean anything.  A type with no size like
	 void is as good as having a size of 1.  */
      tree type = TREE_TYPE (ref);
      while (TREE_CODE (type) == ARRAY_TYPE)
	type = TREE_TYPE (type);
      if (tree size = TYPE_SIZE_UNIT (type))
	{
	  offrange[0] = offrange[0] / wi::to_offset (size);
	  offrange[1] = offrange[1] / wi::to_offset (size);
	}

      bool warned;
      if (offrange[0] == offrange[1])
	warned = warning_at (location, OPT_Warray_bounds,
			     "array subscript %wi is outside array bounds "
			     "of %qT",
			     offrange[0].to_shwi (), reftype);
      else
	warned = warning_at (location, OPT_Warray_bounds,
			     "array subscript [%wi, %wi] is outside "
			     "array bounds of %qT",
			     offrange[0].to_shwi (),
			     offrange[1].to_shwi (), reftype);
      if (warned && DECL_P (arg))
	inform (DECL_SOURCE_LOCATION (arg), "while referencing %qD", arg);

      if (warned)
	TREE_NO_WARNING (ref) = 1;
      return warned;
    }

  if (warn_array_bounds < 2)
    return false;

  /* At level 2 check also intermediate offsets.  */
  int i = 0;
  if (extrema[i] < -arrbounds[1] || extrema[i = 1] > ubound)
    {
      HOST_WIDE_INT tmpidx = extrema[i].to_shwi () / eltsize.to_shwi ();

      if (warning_at (location, OPT_Warray_bounds,
		      "intermediate array offset %wi is outside array bounds "
		      "of %qT", tmpidx, reftype))
	{
	  TREE_NO_WARNING (ref) = 1;
	  return true;
	}
    }

  return false;
}

/* Searches if the expr T, located at LOCATION computes
   address of an ARRAY_REF, and call check_array_ref on it.  */

void
array_bounds_checker::check_addr_expr (location_t location, tree t)
{
  /* Check each ARRAY_REF and MEM_REF in the reference chain. */
  do
    {
      bool warned = false;
      if (TREE_CODE (t) == ARRAY_REF)
	warned = check_array_ref (location, t, true /*ignore_off_by_one*/);
      else if (TREE_CODE (t) == MEM_REF)
	warned = check_mem_ref (location, t, true /*ignore_off_by_one*/);

      if (warned)
	TREE_NO_WARNING (t) = true;

      t = TREE_OPERAND (t, 0);
    }
  while (handled_component_p (t) || TREE_CODE (t) == MEM_REF);

  if (TREE_CODE (t) != MEM_REF
      || TREE_CODE (TREE_OPERAND (t, 0)) != ADDR_EXPR
      || TREE_NO_WARNING (t))
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
      warned = warning_at (location, OPT_Warray_bounds,
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
      warned = warning_at (location, OPT_Warray_bounds,
			   "array subscript %wu is above "
			   "array bounds of %qT",
			   idx.to_uhwi (), TREE_TYPE (tem));
    }

  if (warned)
    {
      if (DECL_P (t))
	inform (DECL_SOURCE_LOCATION (t), "while referencing %qD", t);

      TREE_NO_WARNING (t) = 1;
    }
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

  *walk_subtree = TRUE;

  bool warned = false;
  array_bounds_checker *checker = (array_bounds_checker *) wi->info;
  if (TREE_CODE (t) == ARRAY_REF)
    warned = checker->check_array_ref (location, t,
				       false/*ignore_off_by_one*/);
  else if (TREE_CODE (t) == MEM_REF)
    warned = checker->check_mem_ref (location, t,
				     false /*ignore_off_by_one*/);
  else if (TREE_CODE (t) == ADDR_EXPR)
    {
      checker->check_addr_expr (location, t);
      *walk_subtree = FALSE;
    }
  /* Propagate the no-warning bit to the outer expression.  */
  if (warned)
    TREE_NO_WARNING (t) = true;

  return NULL_TREE;
}

/* A dom_walker subclass for use by check_all_array_refs, to walk over
   all statements of all reachable BBs and call check_array_bounds on
   them.  */

class check_array_bounds_dom_walker : public dom_walker
{
public:
  check_array_bounds_dom_walker (array_bounds_checker *checker)
    : dom_walker (CDI_DOMINATORS,
		  /* Discover non-executable edges, preserving EDGE_EXECUTABLE
		     flags, so that we can merge in information on
		     non-executable edges from vrp_folder .  */
		  REACHABLE_BLOCKS_PRESERVING_FLAGS),
    checker (checker) { }
  ~check_array_bounds_dom_walker () {}

  edge before_dom_children (basic_block) FINAL OVERRIDE;

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
      struct walk_stmt_info wi;
      if (!gimple_has_location (stmt)
	  || is_gimple_debug (stmt))
	continue;

      memset (&wi, 0, sizeof (wi));

      wi.info = checker;

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
