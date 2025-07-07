/* __builtin_object_size (ptr, object_size_type) computation
   Copyright (C) 2004-2025 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

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
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "tree-object-size.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "stringpool.h"
#include "attribs.h"
#include "builtins.h"
#include "gimplify-me.h"
#include "gimplify.h"
#include "tree-ssa-dce.h"

struct object_size_info
{
  int object_size_type;
  unsigned char pass;
  bool changed;
  bitmap visited, reexamine;
  unsigned int *depths;
  unsigned int *stack, *tos;
};

struct GTY(()) object_size
{
  /* Estimate of bytes till the end of the object.  */
  tree size;
  /* Estimate of the size of the whole object.  */
  tree wholesize;
};

static tree compute_object_offset (tree, const_tree);
static bool addr_object_size (struct object_size_info *,
			      const_tree, int, tree *, tree *t = NULL);
static tree alloc_object_size (const gcall *, int);
static tree access_with_size_object_size (const gcall *, int);
static tree pass_through_call (const gcall *);
static void collect_object_sizes_for (struct object_size_info *, tree);
static void expr_object_size (struct object_size_info *, tree, tree);
static bool merge_object_sizes (struct object_size_info *, tree, tree);
static bool plus_stmt_object_size (struct object_size_info *, tree, gimple *);
static bool cond_expr_object_size (struct object_size_info *, tree, gimple *);
static void init_offset_limit (void);
static void check_for_plus_in_loops (struct object_size_info *, tree);
static void check_for_plus_in_loops_1 (struct object_size_info *, tree,
				       unsigned int);

/* object_sizes[0] is upper bound for the object size and number of bytes till
   the end of the object.
   object_sizes[1] is upper bound for the object size and number of bytes till
   the end of the subobject (innermost array or field with address taken).
   object_sizes[2] is lower bound for the object size and number of bytes till
   the end of the object and object_sizes[3] lower bound for subobject.

   For static object sizes, the object size and the bytes till the end of the
   object are both INTEGER_CST.  In the dynamic case, they are finally either a
   gimple variable or an INTEGER_CST.  */
static vec<object_size> object_sizes[OST_END];

/* Bitmaps what object sizes have been computed already.  */
static bitmap computed[OST_END];

/* Maximum value of offset we consider to be addition.  */
static unsigned HOST_WIDE_INT offset_limit;

/* Tell the generic SSA updater what kind of update is needed after the pass
   executes.  */
static unsigned todo;

/* Return true if VAL represents an initial size for OBJECT_SIZE_TYPE.  */

static inline bool
size_initval_p (tree val, int object_size_type)
{
  return ((object_size_type & OST_MINIMUM)
	  ? integer_all_onesp (val) : integer_zerop (val));
}

/* Return true if VAL represents an unknown size for OBJECT_SIZE_TYPE.  */

static inline bool
size_unknown_p (tree val, int object_size_type)
{
  return ((object_size_type & OST_MINIMUM)
	  ? integer_zerop (val) : integer_all_onesp (val));
}

/* Return true if VAL represents a valid size for OBJECT_SIZE_TYPE.  */

static inline bool
size_valid_p (tree val, int object_size_type)
{
  return ((object_size_type & OST_DYNAMIC) || TREE_CODE (val) == INTEGER_CST);
}

/* Return true if VAL is usable as an object size in the object_sizes
   vectors.  */

static inline bool
size_usable_p (tree val)
{
  return TREE_CODE (val) == SSA_NAME || TREE_CODE (val) == INTEGER_CST;
}

/* Return a tree with initial value for OBJECT_SIZE_TYPE.  */

static inline tree
size_initval (int object_size_type)
{
  return ((object_size_type & OST_MINIMUM)
	  ? TYPE_MAX_VALUE (sizetype) : size_zero_node);
}

/* Return a tree with unknown value for OBJECT_SIZE_TYPE.  */

static inline tree
size_unknown (int object_size_type)
{
  return ((object_size_type & OST_MINIMUM)
	  ? size_zero_node : TYPE_MAX_VALUE (sizetype));
}

/* Grow object_sizes[OBJECT_SIZE_TYPE] to num_ssa_names.  */

static inline void
object_sizes_grow (int object_size_type)
{
  if (num_ssa_names > object_sizes[object_size_type].length ())
    object_sizes[object_size_type].safe_grow (num_ssa_names, true);
}

/* Release object_sizes[OBJECT_SIZE_TYPE].  */

static inline void
object_sizes_release (int object_size_type)
{
  object_sizes[object_size_type].release ();
}

/* Return true if object_sizes[OBJECT_SIZE_TYPE][VARNO] is unknown.  */

static inline bool
object_sizes_unknown_p (int object_size_type, unsigned varno)
{
  return size_unknown_p (object_sizes[object_size_type][varno].size,
			 object_size_type);
}

/* Return the raw size expression for VARNO corresponding to OSI.  This returns
   the TREE_VEC as is and should only be used during gimplification.  */

static inline object_size
object_sizes_get_raw (struct object_size_info *osi, unsigned varno)
{
  gcc_assert (osi->pass != 0);
  return object_sizes[osi->object_size_type][varno];
}

/* Return a size tree for VARNO corresponding to OSI.  If WHOLE is true, return
   the whole object size.  Use this for building size expressions based on size
   of VARNO.  */

static inline tree
object_sizes_get (struct object_size_info *osi, unsigned varno,
		  bool whole = false)
{
  tree ret;
  int object_size_type = osi->object_size_type;

  if (whole)
    ret = object_sizes[object_size_type][varno].wholesize;
  else
    ret = object_sizes[object_size_type][varno].size;

  if (object_size_type & OST_DYNAMIC)
    {
      if (TREE_CODE (ret) == MODIFY_EXPR)
	return TREE_OPERAND (ret, 0);
      else if (TREE_CODE (ret) == TREE_VEC)
	return TREE_VEC_ELT (ret, TREE_VEC_LENGTH (ret) - 1);
      else
	gcc_checking_assert (size_usable_p (ret));
    }

  return ret;
}

/* Set size for VARNO corresponding to OSI to VAL.  */

static inline void
object_sizes_initialize (struct object_size_info *osi, unsigned varno,
			 tree val, tree wholeval)
{
  int object_size_type = osi->object_size_type;

  object_sizes[object_size_type][varno].size = val;
  object_sizes[object_size_type][varno].wholesize = wholeval;
}

/* Return a MODIFY_EXPR for cases where SSA and EXPR have the same type.  The
   TREE_VEC is returned only in case of PHI nodes.  */

static tree
bundle_sizes (tree name, tree expr)
{
  gcc_checking_assert (TREE_TYPE (name) == sizetype);

  if (TREE_CODE (expr) == TREE_VEC)
    {
      TREE_VEC_ELT (expr, TREE_VEC_LENGTH (expr) - 1) = name;
      return expr;
    }

  gcc_checking_assert (types_compatible_p (TREE_TYPE (expr), sizetype));
  return build2 (MODIFY_EXPR, sizetype, name, expr);
}

/* Set size for VARNO corresponding to OSI to VAL if it is the new minimum or
   maximum.  For static sizes, each element of TREE_VEC is always INTEGER_CST
   throughout the computation.  For dynamic sizes, each element may either be a
   gimple variable, a MODIFY_EXPR or a TREE_VEC.  The MODIFY_EXPR is for
   expressions that need to be gimplified.  TREE_VECs are special, they're
   emitted only for GIMPLE_PHI and the PHI result variable is the last element
   of the vector.  */

static bool
object_sizes_set (struct object_size_info *osi, unsigned varno, tree val,
		  tree wholeval)
{
  int object_size_type = osi->object_size_type;
  object_size osize = object_sizes[object_size_type][varno];
  bool changed = true;

  tree oldval = osize.size;
  tree old_wholeval = osize.wholesize;

  if (object_size_type & OST_DYNAMIC)
    {
      if (bitmap_bit_p (osi->reexamine, varno))
	{
	  val = bundle_sizes (oldval, val);
	  wholeval = bundle_sizes (old_wholeval, wholeval);
	}
      else
	{
	  gcc_checking_assert (size_initval_p (oldval, object_size_type));
	  gcc_checking_assert (size_initval_p (old_wholeval,
					       object_size_type));
	  /* For dynamic object sizes, all object sizes that are not gimple
	     variables will need to be gimplified.  */
	  if (wholeval != val && !size_usable_p (wholeval))
	    {
	      bitmap_set_bit (osi->reexamine, varno);
	      wholeval = bundle_sizes (make_ssa_name (sizetype), wholeval);
	    }
	  if (!size_usable_p (val))
	    {
	      bitmap_set_bit (osi->reexamine, varno);
	      tree newval = bundle_sizes (make_ssa_name (sizetype), val);
	      if (val == wholeval)
		wholeval = newval;
	      val = newval;
	    }
	  /* If the new value is a temporary variable, mark it for
	     reexamination.  */
	  else if (TREE_CODE (val) == SSA_NAME && !SSA_NAME_DEF_STMT (val))
	    bitmap_set_bit (osi->reexamine, varno);
	}
    }
  else
    {
      enum tree_code code = (object_size_type & OST_MINIMUM
			     ? MIN_EXPR : MAX_EXPR);

      val = size_binop (code, val, oldval);
      wholeval = size_binop (code, wholeval, old_wholeval);
      changed = (tree_int_cst_compare (val, oldval) != 0
		 || tree_int_cst_compare (old_wholeval, wholeval) != 0);
    }

  object_sizes[object_size_type][varno].size = val;
  object_sizes[object_size_type][varno].wholesize = wholeval;

  return changed;
}

/* Set temporary SSA names for object size and whole size to resolve dependency
   loops in dynamic size computation.  */

static inline void
object_sizes_set_temp (struct object_size_info *osi, unsigned varno)
{
  tree val = object_sizes_get (osi, varno);

  if (size_initval_p (val, osi->object_size_type))
    object_sizes_set (osi, varno,
		      make_ssa_name (sizetype),
		      make_ssa_name (sizetype));
}

/* Initialize OFFSET_LIMIT variable.  */
static void
init_offset_limit (void)
{
  if (tree_fits_uhwi_p (TYPE_MAX_VALUE (sizetype)))
    offset_limit = tree_to_uhwi (TYPE_MAX_VALUE (sizetype));
  else
    offset_limit = -1;
  offset_limit /= 2;
}

/* Bytes at end of the object with SZ from offset OFFSET.  If WHOLESIZE is not
   NULL_TREE, use it to get the net offset of the pointer, which should always
   be positive and hence, be within OFFSET_LIMIT for valid offsets.  */

static tree
size_for_offset (tree sz, tree offset, tree wholesize = NULL_TREE,
		 bool strict = true)
{
  gcc_checking_assert (types_compatible_p (TREE_TYPE (sz), sizetype));

  /* For negative offsets, if we have a distinct WHOLESIZE, use it to get a net
     offset from the whole object.  */
  if (wholesize && wholesize != sz
      && (TREE_CODE (sz) != INTEGER_CST
	  || TREE_CODE (wholesize) != INTEGER_CST
	  || tree_int_cst_compare (sz, wholesize)))
    {
      gcc_checking_assert (types_compatible_p (TREE_TYPE (wholesize),
					       sizetype));

      /* Restructure SZ - OFFSET as
	 WHOLESIZE - (WHOLESIZE + OFFSET - SZ) so that the offset part, i.e.
	 WHOLESIZE + OFFSET - SZ is only allowed to be positive.  */
      tree tmp = size_binop (MAX_EXPR, wholesize, sz);
      offset = fold_build2 (PLUS_EXPR, sizetype, tmp, offset);
      offset = fold_build2 (MINUS_EXPR, sizetype, offset, sz);
      sz = tmp;
    }

  /* Safe to convert now, since a valid net offset should be non-negative.  */
  if (!useless_type_conversion_p (sizetype, TREE_TYPE (offset)))
    offset = fold_convert (sizetype, offset);

  if (TREE_CODE (offset) == INTEGER_CST)
    {
      if (integer_zerop (offset))
	return sz;

      /* Negative or too large offset even after adjustment, cannot be within
	 bounds of an object.  The exception here is when the base object size
	 has been overestimated (e.g. through PHI nodes or a COND_EXPR) and the
	 adjusted offset remains negative.  If the caller wants to be
	 permissive, return the base size.  */
      if (compare_tree_int (offset, offset_limit) > 0)
	{
	  if (strict)
	    return size_zero_node;
	  else
	    return sz;
	}
    }

  return size_binop (MINUS_EXPR, size_binop (MAX_EXPR, sz, offset), offset);
}

/* Compute offset of EXPR within VAR.  Return error_mark_node
   if unknown.  */

static tree
compute_object_offset (tree expr, const_tree var)
{
  enum tree_code code = PLUS_EXPR;
  tree base, off, t;

  if (expr == var)
    return size_zero_node;

  switch (TREE_CODE (expr))
    {
    case COMPONENT_REF:
      base = compute_object_offset (TREE_OPERAND (expr, 0), var);
      if (base == error_mark_node)
	return base;

      t = TREE_OPERAND (expr, 1);
      off = size_binop (PLUS_EXPR,
			component_ref_field_offset (expr),
			size_int (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (t))
				  / BITS_PER_UNIT));
      break;

    case REALPART_EXPR:
    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      return compute_object_offset (TREE_OPERAND (expr, 0), var);

    case IMAGPART_EXPR:
      base = compute_object_offset (TREE_OPERAND (expr, 0), var);
      if (base == error_mark_node)
	return base;

      off = TYPE_SIZE_UNIT (TREE_TYPE (expr));
      break;

    case ARRAY_REF:
      base = compute_object_offset (TREE_OPERAND (expr, 0), var);
      if (base == error_mark_node)
	return base;

      t = TREE_OPERAND (expr, 1);
      tree low_bound, unit_size;
      low_bound = array_ref_low_bound (CONST_CAST_TREE (expr));
      unit_size = array_ref_element_size (CONST_CAST_TREE (expr));
      if (! integer_zerop (low_bound))
	t = fold_build2 (MINUS_EXPR, TREE_TYPE (t), t, low_bound);
      if (TREE_CODE (t) == INTEGER_CST && tree_int_cst_sgn (t) < 0)
	{
	  code = MINUS_EXPR;
	  t = fold_build1 (NEGATE_EXPR, TREE_TYPE (t), t);
	}
      t = fold_convert (sizetype, t);
      off = size_binop (MULT_EXPR, unit_size, t);
      break;

    case MEM_REF:
      gcc_assert (TREE_CODE (TREE_OPERAND (expr, 0)) == ADDR_EXPR);
      return wide_int_to_tree (sizetype, mem_ref_offset (expr));

    default:
      return error_mark_node;
    }

  return size_binop (code, base, off);
}

/* Return true if CONTAINER has a field of type INNER at OFFSET.  */

static bool
inner_at_offset (tree container, tree inner, tree offset)
{
  gcc_assert (RECORD_OR_UNION_TYPE_P (container));

  for (tree t = TYPE_FIELDS (container); t; t = DECL_CHAIN (t))
    {
      if (TREE_CODE (t) != FIELD_DECL)
	continue;

      /* Skip over fields at bit offsets that are not BITS_PER_UNIT aligned
	 to avoid an accidental truncated match with BYTE_POSITION below since
	 the address of such fields cannot be taken.  */
      if (wi::bit_and (wi::to_offset (DECL_FIELD_BIT_OFFSET (t)),
		       BITS_PER_UNIT - 1) != 0)
	continue;

      tree byte_offset = byte_position (t);
      if (TREE_CODE (byte_offset) != INTEGER_CST
	  || tree_int_cst_lt (offset, byte_offset))
	return false;

      /* For an array, check the element type, otherwise the actual type.  This
	 deliberately does not support the case of jumping from a pointer to
	 the middle of an array to its containing struct.  */
      tree t_type = TREE_TYPE (t);
      if (((TREE_CODE (t_type) == ARRAY_TYPE && TREE_TYPE (t_type) == inner)
	   || t_type == inner)
	  && tree_int_cst_equal (byte_offset, offset))
	return true;

      /* Nested structure or union, adjust the expected offset and dive in.  */
      if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (t))
	  && inner_at_offset (TREE_TYPE (t), inner,
			      fold_build2 (MINUS_EXPR, sizetype, offset,
					   byte_offset)))
	return true;
    }

  return false;
}

/* For the input MEMREF of type MEMREF_TYPE, look for the presence of a field
   of BASE_TYPE at OFFSET and return an adjusted WHOLESIZE if found.  */

static tree
get_wholesize_for_memref (tree memref, tree wholesize)
{
  tree base = TREE_OPERAND (memref, 0);
  tree offset = fold_convert (sizetype, TREE_OPERAND (memref, 1));
  tree memref_type = TREE_TYPE (memref);
  tree base_type = TREE_TYPE (base);

  if (POINTER_TYPE_P (base_type))
    base_type = TREE_TYPE ((base_type));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "wholesize_for_memref: ");
      print_generic_expr (dump_file, wholesize, dump_flags);
      fprintf (dump_file, ", offset: ");
      print_generic_expr (dump_file, offset, dump_flags);
      fprintf (dump_file, "\n");
    }

  if (TREE_CODE (offset) != INTEGER_CST
      || compare_tree_int (offset, offset_limit) < 0
      || !RECORD_OR_UNION_TYPE_P (memref_type))
    return wholesize;

  offset = fold_build1 (NEGATE_EXPR, sizetype, offset);

  if (inner_at_offset (memref_type, base_type, offset))
    wholesize = size_binop (PLUS_EXPR, wholesize, offset);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "    new wholesize: ");
      print_generic_expr (dump_file, wholesize, dump_flags);
      fprintf (dump_file, "\n");
    }

  return wholesize;
}

/* Returns the size of the object designated by DECL considering its
   initializer if it either has one or if it would not affect its size,
   otherwise the size of the object without the initializer when MIN
   is true, else null.  An object's initializer affects the object's
   size if it's a struct type with a flexible array member.  */

tree
decl_init_size (tree decl, bool min)
{
  tree size = DECL_SIZE_UNIT (decl);
  tree type = TREE_TYPE (decl);
  if (TREE_CODE (type) != RECORD_TYPE)
    return size;

  tree last = last_field (type);
  if (!last)
    return size;

  tree last_type = TREE_TYPE (last);
  if (TREE_CODE (last_type) != ARRAY_TYPE
      || TYPE_SIZE (last_type))
    return size;

  /* Use TYPE_SIZE_UNIT; DECL_SIZE_UNIT sometimes reflects the size
     of the initializer and sometimes doesn't.  */
  size = TYPE_SIZE_UNIT (type);
  tree ref = build3 (COMPONENT_REF, type, decl, last, NULL_TREE);
  tree compsize = component_ref_size (ref);
  if (!compsize)
    return min ? size : NULL_TREE;

  /* The size includes tail padding and initializer elements.  */
  tree pos = byte_position (last);
  size = fold_build2 (PLUS_EXPR, TREE_TYPE (size), pos, compsize);
  return size;
}

/* Compute __builtin_object_size for PTR, which is a ADDR_EXPR.
   OBJECT_SIZE_TYPE is the second argument from __builtin_object_size.
   If unknown, return size_unknown (object_size_type).  */

static bool
addr_object_size (struct object_size_info *osi, const_tree ptr,
		  int object_size_type, tree *psize, tree *pwholesize)
{
  tree pt_var, pt_var_size = NULL_TREE, pt_var_wholesize = NULL_TREE;
  tree var_size, bytes, wholebytes;

  gcc_assert (TREE_CODE (ptr) == ADDR_EXPR);

  /* Set to unknown and overwrite just before returning if the size
     could be determined.  */
  *psize = size_unknown (object_size_type);
  if (pwholesize)
    *pwholesize = size_unknown (object_size_type);

  pt_var = TREE_OPERAND (ptr, 0);
  while (handled_component_p (pt_var))
    pt_var = TREE_OPERAND (pt_var, 0);

  if (!pt_var)
    return false;

  if (TREE_CODE (pt_var) == MEM_REF)
    {
      tree sz, wholesize;

      if (!osi || (object_size_type & OST_SUBOBJECT) != 0
	  || TREE_CODE (TREE_OPERAND (pt_var, 0)) != SSA_NAME)
	{
	  compute_builtin_object_size (TREE_OPERAND (pt_var, 0),
				       object_size_type & ~OST_SUBOBJECT, &sz);
	  wholesize = get_wholesize_for_memref (pt_var, sz);
	}
      else
	{
	  tree var = TREE_OPERAND (pt_var, 0);
	  if (osi->pass == 0)
	    collect_object_sizes_for (osi, var);
	  if (bitmap_bit_p (computed[object_size_type],
			    SSA_NAME_VERSION (var)))
	    {
	      sz = object_sizes_get (osi, SSA_NAME_VERSION (var));
	      wholesize = object_sizes_get (osi, SSA_NAME_VERSION (var), true);
	      wholesize = get_wholesize_for_memref (pt_var, wholesize);
	    }
	  else
	    sz = wholesize = size_unknown (object_size_type);
	}
      if (!size_unknown_p (sz, object_size_type))
	sz = size_for_offset (sz, TREE_OPERAND (pt_var, 1), wholesize);

      if (!size_unknown_p (sz, object_size_type)
	  && (TREE_CODE (sz) != INTEGER_CST
	      || compare_tree_int (sz, offset_limit) < 0))
	{
	  pt_var_size = sz;
	  pt_var_wholesize = wholesize;
	}
    }
  else if (DECL_P (pt_var))
    {
      pt_var_size = pt_var_wholesize
	= decl_init_size (pt_var, object_size_type & OST_MINIMUM);
      if (!pt_var_size)
	return false;
    }
  else if (TREE_CODE (pt_var) == STRING_CST)
    pt_var_size = pt_var_wholesize = TYPE_SIZE_UNIT (TREE_TYPE (pt_var));
  else
    return false;

  if (pt_var_size)
    {
      /* Validate the size determined above if it is a constant.  */
      if (TREE_CODE (pt_var_size) == INTEGER_CST
	  && compare_tree_int (pt_var_size, offset_limit) >= 0)
	return false;
    }

  if (pt_var != TREE_OPERAND (ptr, 0))
    {
      tree var;

      if (object_size_type & OST_SUBOBJECT)
	{
	  var = TREE_OPERAND (ptr, 0);

	  while (var != pt_var
		 && TREE_CODE (var) != BIT_FIELD_REF
		 && TREE_CODE (var) != COMPONENT_REF
		 && TREE_CODE (var) != ARRAY_REF
		 && TREE_CODE (var) != ARRAY_RANGE_REF
		 && TREE_CODE (var) != REALPART_EXPR
		 && TREE_CODE (var) != IMAGPART_EXPR)
	    var = TREE_OPERAND (var, 0);
	  if (var != pt_var && TREE_CODE (var) == ARRAY_REF)
	    var = TREE_OPERAND (var, 0);
	  if (! TYPE_SIZE_UNIT (TREE_TYPE (var))
	      || ! tree_fits_uhwi_p (TYPE_SIZE_UNIT (TREE_TYPE (var)))
	      || (pt_var_size && TREE_CODE (pt_var_size) == INTEGER_CST
		  && tree_int_cst_lt (pt_var_size,
				      TYPE_SIZE_UNIT (TREE_TYPE (var)))))
	    var = pt_var;
	  else if (var != pt_var && TREE_CODE (pt_var) == MEM_REF)
	    {
	      tree v = var;
	      /* For &X->fld, compute object size if fld isn't a flexible array
		 member.  */
	      bool is_flexible_array_mem_ref = false;
	      while (v && v != pt_var)
		switch (TREE_CODE (v))
		  {
		  case ARRAY_REF:
		    if (TYPE_SIZE_UNIT (TREE_TYPE (TREE_OPERAND (v, 0))))
		      {
			tree domain
			  = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (v, 0)));
			if (domain && TYPE_MAX_VALUE (domain))
			  {
			    v = NULL_TREE;
			    break;
			  }
		      }
		    v = TREE_OPERAND (v, 0);
		    break;
		  case REALPART_EXPR:
		  case IMAGPART_EXPR:
		    v = NULL_TREE;
		    break;
		  case COMPONENT_REF:
		    /* When the ref is not to an aggregate type, i.e, an array,
		       a record or a union, it will not have flexible size,
		       compute the object size directly.  */
		    if (!AGGREGATE_TYPE_P (TREE_TYPE (v)))
		      {
			v = NULL_TREE;
			break;
		      }
		    /* if the ref is to a record or union type, but the type
		       does not include a flexible array recursively, compute
		       the object size directly.  */
		    if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (v)))
		      {
			if (!TYPE_INCLUDES_FLEXARRAY (TREE_TYPE (v)))
			  {
			    v = NULL_TREE;
			    break;
			  }
			else
			  {
			    v = TREE_OPERAND (v, 0);
			    break;
			  }
		      }
		    /* Now the ref is to an array type.  */
		    gcc_assert (TREE_CODE (TREE_TYPE (v)) == ARRAY_TYPE);
		    is_flexible_array_mem_ref = array_ref_flexible_size_p (v);
		    while (v != pt_var && TREE_CODE (v) == COMPONENT_REF)
		      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (v, 0)))
			  != UNION_TYPE
			  && TREE_CODE (TREE_TYPE (TREE_OPERAND (v, 0)))
			  != QUAL_UNION_TYPE)
			break;
		      else
			v = TREE_OPERAND (v, 0);
		    if (TREE_CODE (v) == COMPONENT_REF
			&& TREE_CODE (TREE_TYPE (TREE_OPERAND (v, 0)))
			   == RECORD_TYPE)
		      {
			/* compute object size only if v is not a
			   flexible array member.  */
			if (!is_flexible_array_mem_ref)
			  {
			    v = NULL_TREE;
			    break;
			  }
			v = TREE_OPERAND (v, 0);
		      }
		    while (v != pt_var && TREE_CODE (v) == COMPONENT_REF)
		      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (v, 0)))
			  != UNION_TYPE
			  && TREE_CODE (TREE_TYPE (TREE_OPERAND (v, 0)))
			  != QUAL_UNION_TYPE)
			break;
		      else
			v = TREE_OPERAND (v, 0);
		    if (v != pt_var)
		      v = NULL_TREE;
		    else
		      v = pt_var;
		    break;
		  default:
		    v = pt_var;
		    break;
		  }
	      if (v == pt_var)
		var = pt_var;
	    }
	}
      else
	var = pt_var;

      if (var != pt_var)
	{
	  var_size = TYPE_SIZE_UNIT (TREE_TYPE (var));
	  if (!TREE_CONSTANT (var_size))
	    var_size = get_or_create_ssa_default_def (cfun, var_size);
	  if (!var_size)
	    return false;
	}
      else if (!pt_var_size)
	return false;
      else
	var_size = pt_var_size;
      bytes = compute_object_offset (TREE_OPERAND (ptr, 0), var);
      if (bytes != error_mark_node)
	{
	  bytes = size_for_offset (var_size, bytes);
	  if (var != pt_var && pt_var_size && TREE_CODE (pt_var) == MEM_REF)
	    {
	      tree bytes2 = compute_object_offset (TREE_OPERAND (ptr, 0),
						   pt_var);
	      if (bytes2 != error_mark_node)
		{
		  bytes2 = size_for_offset (pt_var_size, bytes2);
		  bytes = size_binop (MIN_EXPR, bytes, bytes2);
		}
	    }
	}
      else
	bytes = size_unknown (object_size_type);

      wholebytes
	= object_size_type & OST_SUBOBJECT ? var_size : pt_var_wholesize;
    }
  else if (!pt_var_size)
    return false;
  else
    {
      bytes = pt_var_size;
      wholebytes = pt_var_wholesize;
    }

  if (!size_unknown_p (bytes, object_size_type)
      && size_valid_p (bytes, object_size_type)
      && !size_unknown_p (bytes, object_size_type)
      && size_valid_p (wholebytes, object_size_type))
    {
      *psize = bytes;
      if (pwholesize)
	*pwholesize = wholebytes;
      return true;
    }

  return false;
}

/* Compute __builtin_object_size for a CALL to .ACCESS_WITH_SIZE,
   OBJECT_SIZE_TYPE is the second argument from __builtin_object_size.
   The 2nd, 3rd, and the 4th parameters of the call determine the size of
   the CALL:

   2nd argument REF_TO_SIZE: The reference to the size of the object,
   3rd argument CLASS_OF_SIZE: The size referenced by the REF_TO_SIZE represents
     0: the number of bytes;
     1: the number of the elements of the object type;
   4th argument TYPE_OF_SIZE: A constant 0 with its TYPE being the same as the TYPE
    of the object referenced by REF_TO_SIZE
   6th argument: A constant 0 with the pointer TYPE to the original flexible
     array type.

   The size of the element can be retrived from the TYPE of the 6th argument
   of the call, which is the pointer to the array type.  */
static tree
access_with_size_object_size (const gcall *call, int object_size_type)
{
  /* If not for dynamic object size, return.  */
  if ((object_size_type & OST_DYNAMIC) == 0)
    return size_unknown (object_size_type);

  gcc_assert (gimple_call_internal_p (call, IFN_ACCESS_WITH_SIZE));
  /* The type of the 6th argument type is the pointer TYPE to the original
     flexible array type.  */
  tree pointer_to_array_type = TREE_TYPE (gimple_call_arg (call, 5));
  gcc_assert (POINTER_TYPE_P (pointer_to_array_type));
  tree element_type = TREE_TYPE (TREE_TYPE (pointer_to_array_type));
  tree element_size = TYPE_SIZE_UNIT (element_type);
  tree ref_to_size = gimple_call_arg (call, 1);
  unsigned int class_of_size = TREE_INT_CST_LOW (gimple_call_arg (call, 2));
  tree type = TREE_TYPE (gimple_call_arg (call, 3));

  tree size = fold_build2 (MEM_REF, type, ref_to_size,
			   build_int_cst (ptr_type_node, 0));

  /* If size is negative value, treat it as zero.  */
  if (!TYPE_UNSIGNED (type))
  {
    tree cond_expr = fold_build2 (LT_EXPR, boolean_type_node,
				  unshare_expr (size), build_zero_cst (type));
    size = fold_build3 (COND_EXPR, integer_type_node, cond_expr,
			build_zero_cst (type), size);
  }

  if (class_of_size == 1)
    size = size_binop (MULT_EXPR,
		       fold_convert (sizetype, size),
		       fold_convert (sizetype, element_size));
  else
    size = fold_convert (sizetype, size);

  if (!todo)
    todo = TODO_update_ssa_only_virtuals;

  return size;
}

/* Compute __builtin_object_size for CALL, which is a GIMPLE_CALL.
   Handles calls to functions declared with attribute alloc_size.
   OBJECT_SIZE_TYPE is the second argument from __builtin_object_size.
   If unknown, return size_unknown (object_size_type).  */

static tree
alloc_object_size (const gcall *call, int object_size_type)
{
  gcc_assert (is_gimple_call (call));

  tree calltype;
  tree callfn = gimple_call_fndecl (call);
  if (callfn)
    calltype = TREE_TYPE (callfn);
  else
    calltype = gimple_call_fntype (call);

  if (!calltype)
    return size_unknown (object_size_type);

  /* Set to positions of alloc_size arguments.  */
  int arg1 = -1, arg2 = -1;
  tree alloc_size = lookup_attribute ("alloc_size",
				      TYPE_ATTRIBUTES (calltype));
  if (alloc_size && TREE_VALUE (alloc_size))
    {
      tree p = TREE_VALUE (alloc_size);

      arg1 = TREE_INT_CST_LOW (TREE_VALUE (p))-1;
      if (TREE_CHAIN (p))
        arg2 = TREE_INT_CST_LOW (TREE_VALUE (TREE_CHAIN (p)))-1;
    }
  else if (gimple_call_builtin_p (call, BUILT_IN_NORMAL)
	   && callfn
	   && ALLOCA_FUNCTION_CODE_P (DECL_FUNCTION_CODE (callfn)))
    arg1 = 0;

  /* Non-const arguments are OK here, let the caller handle constness.  */
  if (arg1 < 0
      || (unsigned) arg1 >= gimple_call_num_args (call)
      || (arg2 >= 0 && (unsigned) arg2 >= gimple_call_num_args (call)))
    return size_unknown (object_size_type);

  tree targ1 = gimple_call_arg (call, arg1);
  if (!INTEGRAL_TYPE_P (TREE_TYPE (targ1))
      || TYPE_PRECISION (TREE_TYPE (targ1)) > TYPE_PRECISION (sizetype))
    return size_unknown (object_size_type);
  targ1 = fold_convert (sizetype, targ1);
  tree bytes = NULL_TREE;
  if (arg2 >= 0)
    {
      tree targ2 = gimple_call_arg (call, arg2);
      if (!INTEGRAL_TYPE_P (TREE_TYPE (targ2))
	  || TYPE_PRECISION (TREE_TYPE (targ2)) > TYPE_PRECISION (sizetype))
	return size_unknown (object_size_type);
      targ2 = fold_convert (sizetype, targ2);
      bytes = size_binop (MULT_EXPR, targ1, targ2);
    }
  else
    bytes = targ1;

  return bytes ? bytes : size_unknown (object_size_type);
}

/* Compute __builtin_object_size for CALL, which is a call to either
   BUILT_IN_STRDUP or BUILT_IN_STRNDUP; IS_STRNDUP indicates which it is.
   OBJECT_SIZE_TYPE is the second argument from __builtin_object_size.
   If unknown, return size_unknown (object_size_type).  */

static tree
strdup_object_size (const gcall *call, int object_size_type, bool is_strndup)
{
  tree src = gimple_call_arg (call, 0);
  tree sz = size_unknown (object_size_type);
  tree n = NULL_TREE;

  if (is_strndup)
    n = fold_build2 (PLUS_EXPR, sizetype, size_one_node,
		     gimple_call_arg (call, 1));
  /* For strdup, simply emit strlen (SRC) + 1 and let the optimizer fold it the
     way it likes.  */
  else
    {
      tree strlen_fn = builtin_decl_implicit (BUILT_IN_STRLEN);
      if (strlen_fn)
	{
	  sz = fold_build2 (PLUS_EXPR, sizetype, size_one_node,
			    build_call_expr (strlen_fn, 1, src));
	  todo = TODO_update_ssa_only_virtuals;
	}
    }

  /* In all other cases, return the size of SRC since the object size cannot
     exceed that.  We cannot do this for OST_MINIMUM unless SRC points into a
     string constant since otherwise the object size could go all the way down
     to zero.  */
  if (!size_valid_p (sz, object_size_type)
       || size_unknown_p (sz, object_size_type))
    {
      tree wholesrc = NULL_TREE;
      if (TREE_CODE (src) == ADDR_EXPR)
	wholesrc = get_base_address (TREE_OPERAND (src, 0));

      /* If the source points within a string constant, we try to get its
	 length.  */
      if (wholesrc && TREE_CODE (wholesrc) == STRING_CST)
	{
	  tree len = c_strlen (src, 0);
	  if (len)
	    sz = fold_build2 (PLUS_EXPR, sizetype, size_one_node, len);
	}

      /* For maximum estimate, our next best guess is the object size of the
	 source.  */
      if (size_unknown_p (sz, object_size_type)
	  && !(object_size_type & OST_MINIMUM))
	compute_builtin_object_size (src, object_size_type, &sz);
    }

  /* String duplication allocates at least one byte, so we should never fail
     for OST_MINIMUM.  */
  if ((!size_valid_p (sz, object_size_type)
       || size_unknown_p (sz, object_size_type))
      && (object_size_type & OST_MINIMUM))
    sz = size_one_node;

  /* Factor in the N.  */
  return n ? fold_build2 (MIN_EXPR, sizetype, n, sz) : sz;
}

/* If object size is propagated from one of function's arguments directly
   to its return value, return that argument for GIMPLE_CALL statement CALL.
   Otherwise return NULL.  */

static tree
pass_through_call (const gcall *call)
{
  unsigned rf = gimple_call_return_flags (call);
  if (rf & ERF_RETURNS_ARG)
    {
      unsigned argnum = rf & ERF_RETURN_ARG_MASK;
      if (argnum < gimple_call_num_args (call))
	return gimple_call_arg (call, argnum);
    }

  /* __builtin_assume_aligned is intentionally not marked RET1.  */
  if (gimple_call_builtin_p (call, BUILT_IN_ASSUME_ALIGNED))
    return gimple_call_arg (call, 0);

  return NULL_TREE;
}

/* Emit PHI nodes for size expressions fo.  */

static void
emit_phi_nodes (gimple *stmt, tree size, tree wholesize)
{
  tree phires;
  gphi *wholephi = NULL;

  if (wholesize != size)
    {
      phires = TREE_VEC_ELT (wholesize, TREE_VEC_LENGTH (wholesize) - 1);
      wholephi = create_phi_node (phires, gimple_bb (stmt));
    }

  phires = TREE_VEC_ELT (size, TREE_VEC_LENGTH (size) - 1);
  gphi *phi = create_phi_node (phires, gimple_bb (stmt));
  gphi *obj_phi = as_a <gphi *> (stmt);

  gcc_checking_assert (TREE_CODE (wholesize) == TREE_VEC);
  gcc_checking_assert (TREE_CODE (size) == TREE_VEC);

  for (unsigned i = 0; i < gimple_phi_num_args (stmt); i++)
    {
      gimple_seq seq = NULL;
      tree wsz = TREE_VEC_ELT (wholesize, i);
      tree sz = TREE_VEC_ELT (size, i);

      /* If we built an expression, we will need to build statements
	 and insert them on the edge right away.  */
      if (TREE_CODE (wsz) != SSA_NAME)
	wsz = force_gimple_operand (wsz, &seq, true, NULL);
      if (TREE_CODE (sz) != SSA_NAME)
	{
	  gimple_seq s;
	  sz = force_gimple_operand (sz, &s, true, NULL);
	  gimple_seq_add_seq (&seq, s);
	}

      if (seq)
	gsi_insert_seq_on_edge (gimple_phi_arg_edge (obj_phi, i), seq);

      if (wholephi)
	add_phi_arg (wholephi, wsz,
		     gimple_phi_arg_edge (obj_phi, i),
		     gimple_phi_arg_location (obj_phi, i));

      add_phi_arg (phi, sz,
		   gimple_phi_arg_edge (obj_phi, i),
		   gimple_phi_arg_location (obj_phi, i));
    }
}

/* Descend through EXPR and return size_unknown if it uses any SSA variable
   object_size_set or object_size_set_temp generated, which turned out to be
   size_unknown, as noted in UNKNOWNS.  */

static tree
propagate_unknowns (object_size_info *osi, tree expr, bitmap unknowns)
{
  int object_size_type = osi->object_size_type;

  switch (TREE_CODE (expr))
    {
    case SSA_NAME:
      if (bitmap_bit_p (unknowns, SSA_NAME_VERSION (expr)))
	return size_unknown (object_size_type);
      return expr;

    case MIN_EXPR:
    case MAX_EXPR:
	{
	  tree res = propagate_unknowns (osi, TREE_OPERAND (expr, 0),
					 unknowns);
	  if (size_unknown_p (res, object_size_type))
	    return res;

	  res = propagate_unknowns (osi, TREE_OPERAND (expr, 1), unknowns);
	  if (size_unknown_p (res, object_size_type))
	    return res;

	  return expr;
	}
    case MODIFY_EXPR:
	{
	  tree res = propagate_unknowns (osi, TREE_OPERAND (expr, 1),
					 unknowns);
	  if (size_unknown_p (res, object_size_type))
	    return res;
	  return expr;
	}
    case TREE_VEC:
      for (int i = 0; i < TREE_VEC_LENGTH (expr); i++)
	{
	  tree res = propagate_unknowns (osi, TREE_VEC_ELT (expr, i),
					 unknowns);
	  if (size_unknown_p (res, object_size_type))
	    return res;
	}
      return expr;
    case PLUS_EXPR:
    case MINUS_EXPR:
	{
	  tree res = propagate_unknowns (osi, TREE_OPERAND (expr, 0),
					 unknowns);
	  if (size_unknown_p (res, object_size_type))
	    return res;

	  return expr;
	}
    default:
      return expr;
    }
}

/* Walk through size expressions that need reexamination and generate
   statements for them.  */

static void
gimplify_size_expressions (object_size_info *osi)
{
  int object_size_type = osi->object_size_type;
  bitmap_iterator bi;
  unsigned int i;
  bool changed;

  /* Step 1: Propagate unknowns into expressions.  */
  bitmap reexamine = BITMAP_ALLOC (NULL);
  bitmap_copy (reexamine, osi->reexamine);
  bitmap unknowns = BITMAP_ALLOC (NULL);
  do
    {
      changed = false;
      EXECUTE_IF_SET_IN_BITMAP (reexamine, 0, i, bi)
	{
	  object_size cur = object_sizes_get_raw (osi, i);

	  if (size_unknown_p (propagate_unknowns (osi, cur.size, unknowns),
			      object_size_type)
	      || size_unknown_p (propagate_unknowns (osi, cur.wholesize,
						     unknowns),
				 object_size_type))
	    {
	      /* Record the SSAs we're overwriting to propagate the
		 unknwons.  */
	      tree oldval = object_sizes_get (osi, i);
	      tree old_wholeval = object_sizes_get (osi, i, true);

	      bitmap_set_bit (unknowns, SSA_NAME_VERSION (oldval));
	      bitmap_set_bit (unknowns, SSA_NAME_VERSION (old_wholeval));
	      object_sizes_initialize (osi, i,
				       size_unknown (object_size_type),
				       size_unknown (object_size_type));
	      bitmap_clear_bit (osi->reexamine, i);
	      changed = true;
	    }
	}
      bitmap_copy (reexamine, osi->reexamine);
    }
  while (changed);

  /* Release all unknowns.  */
  EXECUTE_IF_SET_IN_BITMAP (unknowns, 0, i, bi)
    release_ssa_name (ssa_name (i));

  BITMAP_FREE (unknowns);
  BITMAP_FREE (reexamine);

  /* Expand all size expressions to put their definitions close to the objects
     for which size is being computed.  */
  EXECUTE_IF_SET_IN_BITMAP (osi->reexamine, 0, i, bi)
    {
      gimple_seq seq = NULL;
      object_size osize = object_sizes_get_raw (osi, i);

      gimple *stmt = SSA_NAME_DEF_STMT (ssa_name (i));
      enum gimple_code code = gimple_code (stmt);

      /* PHI nodes need special attention.  */
      if (code == GIMPLE_PHI)
	emit_phi_nodes (stmt, osize.size, osize.wholesize);
      else
	{
	  tree size_expr = NULL_TREE;

	  /* Bundle wholesize in with the size to gimplify if needed.  */
	  if (osize.wholesize != osize.size
	      && !size_usable_p (osize.wholesize))
	    size_expr = size_binop (COMPOUND_EXPR,
				    osize.wholesize,
				    osize.size);
	  else if (!size_usable_p (osize.size))
	    size_expr = osize.size;

	  if (size_expr)
	    {
	      gimple_stmt_iterator gsi;
	      if (code == GIMPLE_NOP)
		gsi = gsi_start_bb (single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
	      else
		gsi = gsi_for_stmt (stmt);

	      force_gimple_operand (size_expr, &seq, true, NULL);
	      gsi_insert_seq_before (&gsi, seq, GSI_CONTINUE_LINKING);
	    }
	}

      /* We're done, so replace the MODIFY_EXPRs with the SSA names.  */
      object_sizes_initialize (osi, i,
			       object_sizes_get (osi, i),
			       object_sizes_get (osi, i, true));
    }
}

/* Compute __builtin_object_size value for PTR and set *PSIZE to
   the resulting value.  If the declared object is known and PDECL
   is nonnull, sets *PDECL to the object's DECL.  OBJECT_SIZE_TYPE
   is the second argument   to __builtin_object_size.
   Returns true on success and false when the object size could not
   be determined.  */

bool
compute_builtin_object_size (tree ptr, int object_size_type,
			     tree *psize)
{
  gcc_assert (object_size_type >= 0 && object_size_type < OST_END);

  /* Set to unknown and overwrite just before returning if the size
     could be determined.  */
  *psize = size_unknown (object_size_type);

  if (! offset_limit)
    init_offset_limit ();

  if (TREE_CODE (ptr) == ADDR_EXPR)
    return addr_object_size (NULL, ptr, object_size_type, psize);

  if (TREE_CODE (ptr) != SSA_NAME
      || !POINTER_TYPE_P (TREE_TYPE (ptr)))
      return false;

  if (computed[object_size_type] == NULL)
    {
      if (optimize || object_size_type & OST_SUBOBJECT)
	return false;

      /* When not optimizing, rather than failing, make a small effort
	 to determine the object size without the full benefit of
	 the (costly) computation below.  */
      gimple *def = SSA_NAME_DEF_STMT (ptr);
      if (gimple_code (def) == GIMPLE_ASSIGN)
	{
	  tree_code code = gimple_assign_rhs_code (def);
	  if (code == POINTER_PLUS_EXPR)
	    {
	      tree offset = gimple_assign_rhs2 (def);
	      ptr = gimple_assign_rhs1 (def);

	      if (((object_size_type & OST_DYNAMIC)
		   || (tree_fits_shwi_p (offset)
		       && compare_tree_int (offset, offset_limit) <= 0))
		  && compute_builtin_object_size (ptr, object_size_type,
						  psize))
		{
		  *psize = size_for_offset (*psize, offset);
		  return true;
		}
	    }
	}
      return false;
    }

  struct object_size_info osi;
  osi.object_size_type = object_size_type;
  if (!bitmap_bit_p (computed[object_size_type], SSA_NAME_VERSION (ptr)))
    {
      bitmap_iterator bi;
      unsigned int i;

      object_sizes_grow (object_size_type);
      if (dump_file)
	{
	  fprintf (dump_file, "Computing %s %s%sobject size for ",
		   (object_size_type & OST_MINIMUM) ? "minimum" : "maximum",
		   (object_size_type & OST_DYNAMIC) ? "dynamic " : "",
		   (object_size_type & OST_SUBOBJECT) ? "sub" : "");
	  print_generic_expr (dump_file, ptr, dump_flags);
	  fprintf (dump_file, ":\n");
	}

      osi.visited = BITMAP_ALLOC (NULL);
      osi.reexamine = BITMAP_ALLOC (NULL);

      if (!(object_size_type & OST_DYNAMIC))
	{
	  osi.depths = NULL;
	  osi.stack = NULL;
	  osi.tos = NULL;
	}

      /* First pass: walk UD chains, compute object sizes that can be computed.
	 osi.reexamine bitmap at the end will contain versions of SSA_NAMES
	 that need to be reexamined.  For both static and dynamic size
	 computation, reexamination is for propagation across dependency loops.
	 The dynamic case has the additional use case where the computed
	 expression needs to be gimplified.  */
      osi.pass = 0;
      osi.changed = false;
      collect_object_sizes_for (&osi, ptr);

      if (object_size_type & OST_DYNAMIC)
	{
	  osi.pass = 1;
	  gimplify_size_expressions (&osi);
	  bitmap_clear (osi.reexamine);
	}

      /* Second pass: keep recomputing object sizes of variables
	 that need reexamination, until no object sizes are
	 increased or all object sizes are computed.  */
      if (! bitmap_empty_p (osi.reexamine))
	{
	  bitmap reexamine = BITMAP_ALLOC (NULL);

	  /* If looking for minimum instead of maximum object size,
	     detect cases where a pointer is increased in a loop.
	     Although even without this detection pass 2 would eventually
	     terminate, it could take a long time.  If a pointer is
	     increasing this way, we need to assume 0 object size.
	     E.g. p = &buf[0]; while (cond) p = p + 4;  */
	  if (object_size_type & OST_MINIMUM)
	    {
	      osi.depths = XCNEWVEC (unsigned int, num_ssa_names);
	      osi.stack = XNEWVEC (unsigned int, num_ssa_names);
	      osi.tos = osi.stack;
	      osi.pass = 1;
	      /* collect_object_sizes_for is changing
		 osi.reexamine bitmap, so iterate over a copy.  */
	      bitmap_copy (reexamine, osi.reexamine);
	      EXECUTE_IF_SET_IN_BITMAP (reexamine, 0, i, bi)
		if (bitmap_bit_p (osi.reexamine, i))
		  check_for_plus_in_loops (&osi, ssa_name (i));

	      free (osi.depths);
	      osi.depths = NULL;
	      free (osi.stack);
	      osi.stack = NULL;
	      osi.tos = NULL;
	    }

	  do
	    {
	      osi.pass = 2;
	      osi.changed = false;
	      /* collect_object_sizes_for is changing
		 osi.reexamine bitmap, so iterate over a copy.  */
	      bitmap_copy (reexamine, osi.reexamine);
	      EXECUTE_IF_SET_IN_BITMAP (reexamine, 0, i, bi)
		if (bitmap_bit_p (osi.reexamine, i))
		  {
		    collect_object_sizes_for (&osi, ssa_name (i));
		    if (dump_file && (dump_flags & TDF_DETAILS))
		      {
			fprintf (dump_file, "Reexamining ");
			print_generic_expr (dump_file, ssa_name (i),
					    dump_flags);
			fprintf (dump_file, "\n");
		      }
		  }
	    }
	  while (osi.changed);

	  BITMAP_FREE (reexamine);
	}
      EXECUTE_IF_SET_IN_BITMAP (osi.reexamine, 0, i, bi)
	bitmap_set_bit (computed[object_size_type], i);

      /* Debugging dumps.  */
      if (dump_file)
	{
	  EXECUTE_IF_SET_IN_BITMAP (osi.visited, 0, i, bi)
	    if (!object_sizes_unknown_p (object_size_type, i))
	      {
		print_generic_expr (dump_file, ssa_name (i),
				    dump_flags);
		fprintf (dump_file,
			 ": %s %s%sobject size ",
			 ((object_size_type & OST_MINIMUM) ? "minimum"
			  : "maximum"),
			 (object_size_type & OST_DYNAMIC) ? "dynamic " : "",
			 (object_size_type & OST_SUBOBJECT) ? "sub" : "");
		print_generic_expr (dump_file, object_sizes_get (&osi, i),
				    dump_flags);
		fprintf (dump_file, "\n");
	      }
	}

      BITMAP_FREE (osi.reexamine);
      BITMAP_FREE (osi.visited);
    }

  *psize = object_sizes_get (&osi, SSA_NAME_VERSION (ptr));
  return !size_unknown_p (*psize, object_size_type);
}

/* Compute object_sizes for PTR, defined to VALUE, which is not an SSA_NAME.  */

static void
expr_object_size (struct object_size_info *osi, tree ptr, tree value)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (ptr);
  tree bytes, wholesize;

  gcc_assert (!object_sizes_unknown_p (object_size_type, varno));
  gcc_assert (osi->pass == 0);

  if (TREE_CODE (value) == WITH_SIZE_EXPR)
    value = TREE_OPERAND (value, 0);

  /* Pointer variables should have been handled by merge_object_sizes.  */
  gcc_assert (TREE_CODE (value) != SSA_NAME
	      || !POINTER_TYPE_P (TREE_TYPE (value)));

  if (TREE_CODE (value) == ADDR_EXPR)
    addr_object_size (osi, value, object_size_type, &bytes, &wholesize);
  else
    bytes = wholesize = size_unknown (object_size_type);

  object_sizes_set (osi, varno, bytes, wholesize);
}


/* Compute object_sizes for PTR, defined to the result of a call.  */

static void
call_object_size (struct object_size_info *osi, tree ptr, gcall *call)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (ptr);
  tree bytes = NULL_TREE;

  gcc_assert (is_gimple_call (call));

  gcc_assert (!object_sizes_unknown_p (object_size_type, varno));
  gcc_assert (osi->pass == 0);

  bool is_strdup = gimple_call_builtin_p (call, BUILT_IN_STRDUP);
  bool is_strndup = gimple_call_builtin_p (call, BUILT_IN_STRNDUP);
  bool is_access_with_size
	 = gimple_call_internal_p (call, IFN_ACCESS_WITH_SIZE);
  if (is_strdup || is_strndup)
    bytes = strdup_object_size (call, object_size_type, is_strndup);
  else if (is_access_with_size)
    bytes = access_with_size_object_size (call, object_size_type);
  else
    bytes = alloc_object_size (call, object_size_type);

  if (!size_valid_p (bytes, object_size_type))
    bytes = size_unknown (object_size_type);

  object_sizes_set (osi, varno, bytes, bytes);
}


/* Compute object_sizes for PTR, defined to an unknown value.  */

static void
unknown_object_size (struct object_size_info *osi, tree ptr)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (ptr);

  gcc_checking_assert (!object_sizes_unknown_p (object_size_type, varno));
  gcc_checking_assert (osi->pass == 0);
  tree bytes = size_unknown (object_size_type);

  object_sizes_set (osi, varno, bytes, bytes);
}


/* Merge object sizes of ORIG + OFFSET into DEST.  Return true if
   the object size might need reexamination later.  */

static bool
merge_object_sizes (struct object_size_info *osi, tree dest, tree orig)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (dest);
  tree orig_bytes, wholesize;

  if (object_sizes_unknown_p (object_size_type, varno))
    return false;

  if (osi->pass == 0)
    collect_object_sizes_for (osi, orig);

  orig_bytes = object_sizes_get (osi, SSA_NAME_VERSION (orig));
  wholesize = object_sizes_get (osi, SSA_NAME_VERSION (orig), true);

  if (object_sizes_set (osi, varno, orig_bytes, wholesize))
    osi->changed = true;

  return bitmap_bit_p (osi->reexamine, SSA_NAME_VERSION (orig));
}


/* Compute object_sizes for VAR, defined to the result of an assignment
   with operator POINTER_PLUS_EXPR.  Return true if the object size might
   need reexamination  later.  */

static bool
plus_stmt_object_size (struct object_size_info *osi, tree var, gimple *stmt)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (var);
  tree bytes, wholesize;
  tree op0, op1;
  bool reexamine = false;

  if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    {
      op0 = gimple_assign_rhs1 (stmt);
      op1 = gimple_assign_rhs2 (stmt);
    }
  else if (gimple_assign_rhs_code (stmt) == ADDR_EXPR)
    {
      tree rhs = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);
      gcc_assert (TREE_CODE (rhs) == MEM_REF);
      op0 = TREE_OPERAND (rhs, 0);
      op1 = TREE_OPERAND (rhs, 1);
    }
  else
    gcc_unreachable ();

  if (object_sizes_unknown_p (object_size_type, varno))
    return false;

  /* Handle PTR + OFFSET here.  */
  if ((TREE_CODE (op0) == SSA_NAME || TREE_CODE (op0) == ADDR_EXPR))
    {
      if (TREE_CODE (op0) == SSA_NAME)
	{
	  if (osi->pass == 0)
	    collect_object_sizes_for (osi, op0);

	  bytes = object_sizes_get (osi, SSA_NAME_VERSION (op0));
	  wholesize = object_sizes_get (osi, SSA_NAME_VERSION (op0), true);
	  reexamine = bitmap_bit_p (osi->reexamine, SSA_NAME_VERSION (op0));
	}
      else
	{
	  /* op0 will be ADDR_EXPR here.  We should never come here during
	     reexamination.  */
	  gcc_checking_assert (osi->pass == 0);
	  addr_object_size (osi, op0, object_size_type, &bytes, &wholesize);
	}

      bool pos_offset = (size_valid_p (op1, 0)
			 && compare_tree_int (op1, offset_limit) <= 0);

      /* size_for_offset doesn't make sense for -1 size, but it does for size 0
	 since the wholesize could be non-zero and a negative offset could give
	 a non-zero size.  */
      if (size_unknown_p (bytes, 0))
	;
      /* In the static case, We want SIZE_FOR_OFFSET to go a bit easy on us if
	 it sees a negative offset since BYTES could have been
	 overestimated.  */
      else if ((object_size_type & OST_DYNAMIC)
	       || bytes != wholesize
	       || pos_offset)
	bytes = size_for_offset (bytes, op1, wholesize,
				 ((object_size_type & OST_DYNAMIC)
				  || pos_offset));
      /* In the static case, with a negative offset, the best estimate for
	 minimum size is size_unknown but for maximum size, the wholesize is a
	 better estimate than size_unknown.  */
      else if (object_size_type & OST_MINIMUM)
	bytes = size_unknown (object_size_type);
      else
	bytes = wholesize;
    }
  else
    bytes = wholesize = size_unknown (object_size_type);

  if (!size_valid_p (bytes, object_size_type)
      || !size_valid_p (wholesize, object_size_type))
    bytes = wholesize = size_unknown (object_size_type);

  if (object_sizes_set (osi, varno, bytes, wholesize))
    osi->changed = true;
  return reexamine;
}

/* Compute the dynamic object size for VAR.  Return the result in SIZE and
   WHOLESIZE.  */

static void
dynamic_object_size (struct object_size_info *osi, tree var,
		     tree *size, tree *wholesize)
{
  int object_size_type = osi->object_size_type;

  if (TREE_CODE (var) == SSA_NAME)
    {
      unsigned varno = SSA_NAME_VERSION (var);

      collect_object_sizes_for (osi, var);
      *size = object_sizes_get (osi, varno);
      *wholesize = object_sizes_get (osi, varno, true);
    }
  else if (TREE_CODE (var) == ADDR_EXPR)
    addr_object_size (osi, var, object_size_type, size, wholesize);
  else
    *size = *wholesize = size_unknown (object_size_type);
}

/* Compute object_sizes for VAR, defined at STMT, which is
   a COND_EXPR.  Return true if the object size might need reexamination
   later.  */

static bool
cond_expr_object_size (struct object_size_info *osi, tree var, gimple *stmt)
{
  tree then_, else_;
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (var);
  bool reexamine = false;

  gcc_assert (gimple_assign_rhs_code (stmt) == COND_EXPR);

  if (object_sizes_unknown_p (object_size_type, varno))
    return false;

  then_ = gimple_assign_rhs2 (stmt);
  else_ = gimple_assign_rhs3 (stmt);

  if (object_size_type & OST_DYNAMIC)
    {
      tree then_size, then_wholesize, else_size, else_wholesize;

      dynamic_object_size (osi, then_, &then_size, &then_wholesize);
      if (!size_unknown_p (then_size, object_size_type))
	dynamic_object_size (osi, else_, &else_size, &else_wholesize);

      tree cond_size, cond_wholesize;
      if (size_unknown_p (then_size, object_size_type)
	  || size_unknown_p (else_size, object_size_type))
	cond_size = cond_wholesize = size_unknown (object_size_type);
      else
	{
	  cond_size = fold_build3 (COND_EXPR, sizetype,
				   gimple_assign_rhs1 (stmt),
				   then_size, else_size);
	  cond_wholesize = fold_build3 (COND_EXPR, sizetype,
					gimple_assign_rhs1 (stmt),
					then_wholesize, else_wholesize);
	}

      object_sizes_set (osi, varno, cond_size, cond_wholesize);

      return false;
    }

  if (TREE_CODE (then_) == SSA_NAME)
    reexamine |= merge_object_sizes (osi, var, then_);
  else
    expr_object_size (osi, var, then_);

  if (object_sizes_unknown_p (object_size_type, varno))
    return reexamine;

  if (TREE_CODE (else_) == SSA_NAME)
    reexamine |= merge_object_sizes (osi, var, else_);
  else
    expr_object_size (osi, var, else_);

  return reexamine;
}

/* Find size of an object passed as a parameter to the function.  */

static void
parm_object_size (struct object_size_info *osi, tree var)
{
  int object_size_type = osi->object_size_type;
  tree parm = SSA_NAME_VAR (var);

  if (!(object_size_type & OST_DYNAMIC) || !POINTER_TYPE_P (TREE_TYPE (parm)))
    {
      expr_object_size (osi, var, parm);
      return;
    }

  /* Look for access attribute.  */
  rdwr_map rdwr_idx;

  tree fndecl = cfun->decl;
  const attr_access *access = get_parm_access (rdwr_idx, parm, fndecl);
  tree typesize = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (parm)));
  tree sz = NULL_TREE;

  /* If we have an access attribute with a usable size argument... */
  if (access && access->sizarg != UINT_MAX
      /* ... and either PARM is void * or has a type that is complete and has a
	 constant size... */
      && ((typesize && poly_int_tree_p (typesize))
	  || (!typesize && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (parm))))))
    {
      tree fnargs = DECL_ARGUMENTS (fndecl);
      tree arg = NULL_TREE;
      unsigned argpos = 0;

      /* ... then walk through the parameters to pick the size parameter and
	 safely scale it by the type size if needed.

	 TODO: we could also compute the size of VLAs where the size is
	 given by a function parameter.  */
      for (arg = fnargs; arg; arg = TREE_CHAIN (arg), ++argpos)
	if (argpos == access->sizarg)
	  {
	    gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (arg)));
	    sz = get_or_create_ssa_default_def (cfun, arg);
	    if (sz != NULL_TREE)
	      {
		sz = fold_convert (sizetype, sz);
		if (typesize)
		  sz = size_binop (MULT_EXPR, sz, typesize);
	      }
	    break;
	  }
    }
  if (!sz)
    sz = size_unknown (object_size_type);

  object_sizes_set (osi, SSA_NAME_VERSION (var), sz, sz);
}

/* Compute an object size expression for VAR, which is the result of a PHI
   node.  */

static void
phi_dynamic_object_size (struct object_size_info *osi, tree var)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (var);
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  unsigned i, num_args = gimple_phi_num_args (stmt);
  bool wholesize_needed = false;

  /* The extra space is for the PHI result at the end, which object_sizes_set
     sets for us.  */
  tree sizes = make_tree_vec (num_args + 1);
  tree wholesizes = make_tree_vec (num_args + 1);

  /* Bail out if the size of any of the PHI arguments cannot be
     determined.  */
  for (i = 0; i < num_args; i++)
    {
      edge e = gimple_phi_arg_edge (as_a <gphi *> (stmt), i);
      if (e->flags & EDGE_COMPLEX)
	break;

      tree rhs = gimple_phi_arg_def (stmt, i);
      tree size, wholesize;

      dynamic_object_size (osi, rhs, &size, &wholesize);

      if (size_unknown_p (size, object_size_type))
       break;

      if (size != wholesize)
	wholesize_needed = true;

      TREE_VEC_ELT (sizes, i) = size;
      TREE_VEC_ELT (wholesizes, i) = wholesize;
    }

  if (i < num_args)
    {
      ggc_free (sizes);
      ggc_free (wholesizes);
      sizes = wholesizes = size_unknown (object_size_type);
    }

  /* Point to the same TREE_VEC so that we can avoid emitting two PHI
     nodes.  */
  else if (!wholesize_needed)
    {
      ggc_free (wholesizes);
      wholesizes = sizes;
    }

  object_sizes_set (osi, varno, sizes, wholesizes);
}

/* Compute object sizes for VAR.
   For ADDR_EXPR an object size is the number of remaining bytes
   to the end of the object (where what is considered an object depends on
   OSI->object_size_type).
   For allocation GIMPLE_CALL like malloc or calloc object size is the size
   of the allocation.
   For POINTER_PLUS_EXPR where second operand is a constant integer,
   object size is object size of the first operand minus the constant.
   If the constant is bigger than the number of remaining bytes until the
   end of the object, object size is 0, but if it is instead a pointer
   subtraction, object size is size_unknown (object_size_type).
   To differentiate addition from subtraction, ADDR_EXPR returns
   size_unknown (object_size_type) for all objects bigger than half of the
   address space, and constants less than half of the address space are
   considered addition, while bigger constants subtraction.
   For a memcpy like GIMPLE_CALL that always returns one of its arguments, the
   object size is object size of that argument.
   Otherwise, object size is the maximum of object sizes of variables
   that it might be set to.  */

static void
collect_object_sizes_for (struct object_size_info *osi, tree var)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (var);
  gimple *stmt;
  bool reexamine;

  if (bitmap_bit_p (computed[object_size_type], varno))
    return;

  if (osi->pass == 0)
    {
      if (bitmap_set_bit (osi->visited, varno))
	{
	  /* Initialize to 0 for maximum size and M1U for minimum size so that
	     it gets immediately overridden.  */
	  object_sizes_initialize (osi, varno,
				   size_initval (object_size_type),
				   size_initval (object_size_type));
	}
      else
	{
	  /* Found a dependency loop.  Mark the variable for later
	     re-examination.  */
	  if (object_size_type & OST_DYNAMIC)
	    object_sizes_set_temp (osi, varno);

	  bitmap_set_bit (osi->reexamine, varno);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Found a dependency loop at ");
	      print_generic_expr (dump_file, var, dump_flags);
	      fprintf (dump_file, "\n");
	    }
	  return;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Visiting use-def links for ");
      print_generic_expr (dump_file, var, dump_flags);
      fprintf (dump_file, "\n");
    }

  stmt = SSA_NAME_DEF_STMT (var);
  reexamine = false;

  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      {
	tree rhs = gimple_assign_rhs1 (stmt);
        if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
	    || (gimple_assign_rhs_code (stmt) == ADDR_EXPR
		&& TREE_CODE (TREE_OPERAND (rhs, 0)) == MEM_REF))
          reexamine = plus_stmt_object_size (osi, var, stmt);
	else if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	  reexamine = cond_expr_object_size (osi, var, stmt);
        else if (gimple_assign_single_p (stmt)
                 || gimple_assign_unary_nop_p (stmt))
          {
            if (TREE_CODE (rhs) == SSA_NAME
                && POINTER_TYPE_P (TREE_TYPE (rhs)))
	      reexamine = merge_object_sizes (osi, var, rhs);
            else
              expr_object_size (osi, var, rhs);
          }
        else
	  unknown_object_size (osi, var);
        break;
      }

    case GIMPLE_CALL:
      {
	gcall *call_stmt = as_a <gcall *> (stmt);
        tree arg = pass_through_call (call_stmt);
        if (arg)
          {
            if (TREE_CODE (arg) == SSA_NAME
                && POINTER_TYPE_P (TREE_TYPE (arg)))
	      reexamine = merge_object_sizes (osi, var, arg);
            else
              expr_object_size (osi, var, arg);
          }
        else
          call_object_size (osi, var, call_stmt);
	break;
      }

    case GIMPLE_ASM:
      /* Pointers defined by __asm__ statements can point anywhere.  */
      unknown_object_size (osi, var);
      break;

    case GIMPLE_NOP:
      if (SSA_NAME_VAR (var)
	  && TREE_CODE (SSA_NAME_VAR (var)) == PARM_DECL)
	parm_object_size (osi, var);
      else
	/* Uninitialized SSA names point nowhere.  */
	unknown_object_size (osi, var);
      break;

    case GIMPLE_PHI:
      {
	unsigned i;

	if (object_size_type & OST_DYNAMIC)
	  {
	    phi_dynamic_object_size (osi, var);
	    break;
	  }

	for (i = 0; i < gimple_phi_num_args (stmt); i++)
	  {
	    tree rhs = gimple_phi_arg (stmt, i)->def;

	    if (object_sizes_unknown_p (object_size_type, varno))
	      break;

	    if (TREE_CODE (rhs) == SSA_NAME)
	      reexamine |= merge_object_sizes (osi, var, rhs);
	    else if (osi->pass == 0)
	      expr_object_size (osi, var, rhs);
	  }
	break;
      }

    default:
      gcc_unreachable ();
    }

  /* Dynamic sizes use placeholder temps to return an answer, so it is always
     safe to set COMPUTED for them.  */
  if ((object_size_type & OST_DYNAMIC)
      || !reexamine || object_sizes_unknown_p (object_size_type, varno))
    {
      bitmap_set_bit (computed[object_size_type], varno);
      if (!(object_size_type & OST_DYNAMIC))
	bitmap_clear_bit (osi->reexamine, varno);
      else if (reexamine)
	bitmap_set_bit (osi->reexamine, varno);
    }
  else
    {
      bitmap_set_bit (osi->reexamine, varno);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Need to reexamine ");
	  print_generic_expr (dump_file, var, dump_flags);
	  fprintf (dump_file, "\n");
	}
    }
}


/* Helper function for check_for_plus_in_loops.  Called recursively
   to detect loops.  */

static void
check_for_plus_in_loops_1 (struct object_size_info *osi, tree var,
			   unsigned int depth)
{
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  unsigned int varno = SSA_NAME_VERSION (var);

  if (osi->depths[varno])
    {
      if (osi->depths[varno] != depth)
	{
	  unsigned int *sp;

	  /* Found a loop involving pointer addition.  */
	  for (sp = osi->tos; sp > osi->stack; )
	    {
	      --sp;
	      bitmap_clear_bit (osi->reexamine, *sp);
	      bitmap_set_bit (computed[osi->object_size_type], *sp);
	      object_sizes_set (osi, *sp, size_zero_node,
				object_sizes_get (osi, *sp, true));
	      if (*sp == varno)
		break;
	    }
	}
      return;
    }
  else if (! bitmap_bit_p (osi->reexamine, varno))
    return;

  osi->depths[varno] = depth;
  *osi->tos++ = varno;

  switch (gimple_code (stmt))
    {

    case GIMPLE_ASSIGN:
      {
        if ((gimple_assign_single_p (stmt)
             || gimple_assign_unary_nop_p (stmt))
            && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
          {
            tree rhs = gimple_assign_rhs1 (stmt);

            check_for_plus_in_loops_1 (osi, rhs, depth);
          }
        else if (gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
          {
            tree basevar = gimple_assign_rhs1 (stmt);
            tree cst = gimple_assign_rhs2 (stmt);

            gcc_assert (TREE_CODE (cst) == INTEGER_CST);

            check_for_plus_in_loops_1 (osi, basevar,
                                       depth + !integer_zerop (cst));
          }
        else
          gcc_unreachable ();
        break;
      }

    case GIMPLE_CALL:
      {
	gcall *call_stmt = as_a <gcall *> (stmt);
        tree arg = pass_through_call (call_stmt);
        if (arg)
          {
            if (TREE_CODE (arg) == SSA_NAME)
              check_for_plus_in_loops_1 (osi, arg, depth);
            else
              gcc_unreachable ();
          }
        break;
      }

    case GIMPLE_PHI:
      {
	unsigned i;

	for (i = 0; i < gimple_phi_num_args (stmt); i++)
	  {
	    tree rhs = gimple_phi_arg (stmt, i)->def;

	    if (TREE_CODE (rhs) == SSA_NAME)
	      check_for_plus_in_loops_1 (osi, rhs, depth);
	  }
	break;
      }

    default:
      gcc_unreachable ();
    }

  osi->depths[varno] = 0;
  osi->tos--;
}


/* Check if some pointer we are computing object size of is being increased
   within a loop.  If yes, assume all the SSA variables participating in
   that loop have minimum object sizes 0.  */

static void
check_for_plus_in_loops (struct object_size_info *osi, tree var)
{
  gimple *stmt = SSA_NAME_DEF_STMT (var);

  /* NOTE: In the pre-tuples code, we handled a CALL_EXPR here,
     and looked for a POINTER_PLUS_EXPR in the pass-through
     argument, if any.  In GIMPLE, however, such an expression
     is not a valid call operand.  */

  if (is_gimple_assign (stmt)
      && gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR)
    {
      tree basevar = gimple_assign_rhs1 (stmt);
      tree cst = gimple_assign_rhs2 (stmt);

      gcc_assert (TREE_CODE (cst) == INTEGER_CST);

      /* Skip non-positive offsets.  */
      if (integer_zerop (cst) || compare_tree_int (cst, offset_limit) > 0)
        return;

      osi->depths[SSA_NAME_VERSION (basevar)] = 1;
      *osi->tos++ = SSA_NAME_VERSION (basevar);
      check_for_plus_in_loops_1 (osi, var, 2);
      osi->depths[SSA_NAME_VERSION (basevar)] = 0;
      osi->tos--;
    }
}


/* Initialize data structures for the object size computation.  */

void
init_object_sizes (void)
{
  int object_size_type;

  if (computed[0])
    return;

  for (object_size_type = 0; object_size_type < OST_END; object_size_type++)
    {
      object_sizes_grow (object_size_type);
      computed[object_size_type] = BITMAP_ALLOC (NULL);
    }

  init_offset_limit ();
}


/* Destroy data structures after the object size computation.  */

void
fini_object_sizes (void)
{
  int object_size_type;

  for (object_size_type = 0; object_size_type < OST_END; object_size_type++)
    {
      object_sizes_release (object_size_type);
      BITMAP_FREE (computed[object_size_type]);
    }
}

/* Dummy valueize function.  */

static tree
do_valueize (tree t)
{
  return t;
}

/* Process a __builtin_object_size or __builtin_dynamic_object_size call in
   CALL early for subobjects before any object information is lost due to
   optimization.  Insert a MIN or MAX expression of the result and
   __builtin_object_size at I so that it may be processed in the second pass.
   __builtin_dynamic_object_size is treated like __builtin_object_size here
   since we're only looking for constant bounds.  */

static void
early_object_sizes_execute_one (gimple_stmt_iterator *i, gimple *call)
{
  tree ost = gimple_call_arg (call, 1);
  tree lhs = gimple_call_lhs (call);
  gcc_assert (lhs != NULL_TREE);

  if (!tree_fits_uhwi_p (ost))
    return;

  unsigned HOST_WIDE_INT object_size_type = tree_to_uhwi (ost);
  tree ptr = gimple_call_arg (call, 0);

  if (object_size_type != 1 && object_size_type != 3)
    return;

  if (TREE_CODE (ptr) != ADDR_EXPR && TREE_CODE (ptr) != SSA_NAME)
    return;

  tree type = TREE_TYPE (lhs);
  tree bytes;
  if (!compute_builtin_object_size (ptr, object_size_type, &bytes)
      || !int_fits_type_p (bytes, type))
    return;

  tree tem = make_ssa_name (type);
  gimple_call_set_lhs (call, tem);
  enum tree_code code = object_size_type & OST_MINIMUM ? MAX_EXPR : MIN_EXPR;
  tree cst = fold_convert (type, bytes);
  gimple *g = gimple_build_assign (lhs, code, tem, cst);
  gsi_insert_after (i, g, GSI_NEW_STMT);
  update_stmt (call);
}

/* Attempt to fold one __builtin_dynamic_object_size call in CALL into an
   expression and insert it at I.  Return true if it succeeds.  */

static bool
dynamic_object_sizes_execute_one (gimple_stmt_iterator *i, gimple *call)
{
  gcc_assert (gimple_call_num_args (call) == 2);

  tree args[2];
  args[0] = gimple_call_arg (call, 0);
  args[1] = gimple_call_arg (call, 1);

  location_t loc = EXPR_LOC_OR_LOC (args[0], input_location);
  tree result_type = gimple_call_return_type (as_a <gcall *> (call));
  tree result = fold_builtin_call_array (loc, result_type,
					 gimple_call_fn (call), 2, args);

  if (!result)
    return false;

  /* fold_builtin_call_array may wrap the result inside a
     NOP_EXPR.  */
  STRIP_NOPS (result);
  gimplify_and_update_call_from_tree (i, result);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Simplified (dynamic)\n  ");
      print_gimple_stmt (dump_file, call, 0, dump_flags);
      fprintf (dump_file, " to ");
      print_generic_expr (dump_file, result);
      fprintf (dump_file, "\n");
    }
  return true;
}

static unsigned int
object_sizes_execute (function *fun, bool early)
{
  todo = 0;
  auto_bitmap sdce_worklist;

  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator i;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
	{
	  tree result;
	  bool dynamic = false;

	  gimple *call = gsi_stmt (i);
	  if (gimple_call_builtin_p (call, BUILT_IN_DYNAMIC_OBJECT_SIZE))
	    dynamic = true;
	  else if (!gimple_call_builtin_p (call, BUILT_IN_OBJECT_SIZE))
	    continue;

	  tree lhs = gimple_call_lhs (call);
	  if (!lhs)
	    continue;

	  init_object_sizes ();

	  /* If early, only attempt to fold
	     __builtin_object_size (x, 1) and __builtin_object_size (x, 3),
	     and rather than folding the builtin to the constant if any,
	     create a MIN_EXPR or MAX_EXPR of the __builtin_object_size
	     call result and the computed constant.  Do the same for
	     __builtin_dynamic_object_size too.  */
	  if (early)
	    {
	      early_object_sizes_execute_one (&i, call);
	      continue;
	    }

	  if (dynamic)
	    {
	      if (dynamic_object_sizes_execute_one (&i, call))
		continue;
	      else
		{
		  /* If we could not find a suitable size expression, lower to
		     __builtin_object_size so that we may at least get a
		     constant lower or higher estimate.  */
		  tree bosfn = builtin_decl_implicit (BUILT_IN_OBJECT_SIZE);
		  gimple_call_set_fndecl (call, bosfn);
		  update_stmt (call);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      print_generic_expr (dump_file, gimple_call_arg (call, 0),
					  dump_flags);
		      fprintf (dump_file,
			       ": Retrying as __builtin_object_size\n");
		    }
		}
	    }

	  result = gimple_fold_stmt_to_constant (call, do_valueize);
	  if (!result)
	    {
	      tree ost = gimple_call_arg (call, 1);

	      if (tree_fits_uhwi_p (ost))
		{
		  unsigned HOST_WIDE_INT object_size_type = tree_to_uhwi (ost);

		  if (object_size_type & OST_MINIMUM)
		    result = build_zero_cst (size_type_node);
		  else if (object_size_type < OST_END)
		    result = fold_convert (size_type_node,
					   integer_minus_one_node);
		}

	      if (!result)
		continue;
	    }

	  gcc_assert (TREE_CODE (result) == INTEGER_CST);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Simplified\n  ");
	      print_gimple_stmt (dump_file, call, 0, dump_flags);
	      fprintf (dump_file, " to ");
	      print_generic_expr (dump_file, result);
	      fprintf (dump_file, "\n");
	    }

	  /* Propagate into all uses and fold those stmts.  */
	  if (!SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    {
	      replace_uses_by (lhs, result);
	      /* Mark lhs as being possiblely DCEd. */
	      bitmap_set_bit (sdce_worklist, SSA_NAME_VERSION (lhs));
	    }
	  else
	    replace_call_with_value (&i, result);
	}
    }

  fini_object_sizes ();
  simple_dce_from_worklist (sdce_worklist);
  return todo;
}

/* Simple pass to optimize all __builtin_object_size () builtins.  */

namespace {

const pass_data pass_data_object_sizes =
{
  GIMPLE_PASS, /* type */
  "objsz", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  PROP_objsz, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_object_sizes : public gimple_opt_pass
{
public:
  pass_object_sizes (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_object_sizes, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_object_sizes (m_ctxt); }
  unsigned int execute (function *fun) final override
  {
    return object_sizes_execute (fun, false);
  }
}; // class pass_object_sizes

} // anon namespace

gimple_opt_pass *
make_pass_object_sizes (gcc::context *ctxt)
{
  return new pass_object_sizes (ctxt);
}

/* Early version of pass to optimize all __builtin_object_size () builtins.  */

namespace {

const pass_data pass_data_early_object_sizes =
{
  GIMPLE_PASS, /* type */
  "early_objsz", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_early_object_sizes : public gimple_opt_pass
{
public:
  pass_early_object_sizes (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_object_sizes, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute (function *fun) final override
  {
    return object_sizes_execute (fun, true);
  }
}; // class pass_object_sizes

} // anon namespace

gimple_opt_pass *
make_pass_early_object_sizes (gcc::context *ctxt)
{
  return new pass_early_object_sizes (ctxt);
}
