/* __builtin_object_size (ptr, object_size_type) computation
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"

struct object_size_info
{
  int object_size_type;
  bitmap visited, reexamine;
  int pass;
  bool changed;
  unsigned int *depths;
  unsigned int *stack, *tos;
};

static unsigned HOST_WIDE_INT unknown[4] = { -1, -1, 0, 0 };

static tree compute_object_offset (const_tree, const_tree);
static unsigned HOST_WIDE_INT addr_object_size (struct object_size_info *,
						const_tree, int);
static unsigned HOST_WIDE_INT alloc_object_size (const_gimple, int);
static tree pass_through_call (const_gimple);
static void collect_object_sizes_for (struct object_size_info *, tree);
static void expr_object_size (struct object_size_info *, tree, tree);
static bool merge_object_sizes (struct object_size_info *, tree, tree,
				unsigned HOST_WIDE_INT);
static bool plus_stmt_object_size (struct object_size_info *, tree, gimple);
static bool cond_expr_object_size (struct object_size_info *, tree, tree);
static unsigned int compute_object_sizes (void);
static void init_offset_limit (void);
static void check_for_plus_in_loops (struct object_size_info *, tree);
static void check_for_plus_in_loops_1 (struct object_size_info *, tree,
				       unsigned int);

/* object_sizes[0] is upper bound for number of bytes till the end of
   the object.
   object_sizes[1] is upper bound for number of bytes till the end of
   the subobject (innermost array or field with address taken).
   object_sizes[2] is lower bound for number of bytes till the end of
   the object and object_sizes[3] lower bound for subobject.  */
static unsigned HOST_WIDE_INT *object_sizes[4];

/* Bitmaps what object sizes have been computed already.  */
static bitmap computed[4];

/* Maximum value of offset we consider to be addition.  */
static unsigned HOST_WIDE_INT offset_limit;


/* Initialize OFFSET_LIMIT variable.  */
static void
init_offset_limit (void)
{
  if (host_integerp (TYPE_MAX_VALUE (sizetype), 1))
    offset_limit = tree_low_cst (TYPE_MAX_VALUE (sizetype), 1);
  else
    offset_limit = -1;
  offset_limit /= 2;
}


/* Compute offset of EXPR within VAR.  Return error_mark_node
   if unknown.  */

static tree
compute_object_offset (const_tree expr, const_tree var)
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
      off = size_binop (PLUS_EXPR, DECL_FIELD_OFFSET (t),
			size_int (tree_low_cst (DECL_FIELD_BIT_OFFSET (t), 1)
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
      if (TREE_CODE (t) == INTEGER_CST && tree_int_cst_sgn (t) < 0)
	{
	  code = MINUS_EXPR;
	  t = fold_build1 (NEGATE_EXPR, TREE_TYPE (t), t);
	}
      t = fold_convert (sizetype, t);
      off = size_binop (MULT_EXPR, TYPE_SIZE_UNIT (TREE_TYPE (expr)), t);
      break;

    case MEM_REF:
      gcc_assert (TREE_CODE (TREE_OPERAND (expr, 0)) == ADDR_EXPR);
      return double_int_to_tree (sizetype, mem_ref_offset (expr));

    default:
      return error_mark_node;
    }

  return size_binop (code, base, off);
}


/* Compute __builtin_object_size for PTR, which is a ADDR_EXPR.
   OBJECT_SIZE_TYPE is the second argument from __builtin_object_size.
   If unknown, return unknown[object_size_type].  */

static unsigned HOST_WIDE_INT
addr_object_size (struct object_size_info *osi, const_tree ptr,
		  int object_size_type)
{
  tree pt_var, pt_var_size = NULL_TREE, var_size, bytes;

  gcc_assert (TREE_CODE (ptr) == ADDR_EXPR);

  pt_var = TREE_OPERAND (ptr, 0);
  if (REFERENCE_CLASS_P (pt_var))
    pt_var = get_base_address (pt_var);

  if (pt_var
      && TREE_CODE (pt_var) == MEM_REF
      && TREE_CODE (TREE_OPERAND (pt_var, 0)) == SSA_NAME
      && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (pt_var, 0))))
    {
      unsigned HOST_WIDE_INT sz;

      if (!osi || (object_size_type & 1) != 0)
	{
	  sz = compute_builtin_object_size (TREE_OPERAND (pt_var, 0),
					    object_size_type & ~1);
	  if (host_integerp (TREE_OPERAND (pt_var, 1), 0))
	    sz -= TREE_INT_CST_LOW (TREE_OPERAND (pt_var, 1));
	  else
	    sz = offset_limit;
	}
      else
	{
	  tree var = TREE_OPERAND (pt_var, 0);
	  if (osi->pass == 0)
	    collect_object_sizes_for (osi, var);
	  if (bitmap_bit_p (computed[object_size_type],
			    SSA_NAME_VERSION (var)))
	    sz = object_sizes[object_size_type][SSA_NAME_VERSION (var)];
	  else
	    sz = unknown[object_size_type];
	  if (host_integerp (TREE_OPERAND (pt_var, 1), 0))
	    sz -= TREE_INT_CST_LOW (TREE_OPERAND (pt_var, 1));
	  else
	    sz = offset_limit;
	}

      if (sz != unknown[object_size_type] && sz < offset_limit)
	pt_var_size = size_int (sz);
    }
  else if (pt_var
	   && (SSA_VAR_P (pt_var) || TREE_CODE (pt_var) == STRING_CST)
	   && TYPE_SIZE_UNIT (TREE_TYPE (pt_var))
	   && host_integerp (TYPE_SIZE_UNIT (TREE_TYPE (pt_var)), 1)
	   && (unsigned HOST_WIDE_INT)
	      tree_low_cst (TYPE_SIZE_UNIT (TREE_TYPE (pt_var)), 1)
	      < offset_limit)
    pt_var_size = TYPE_SIZE_UNIT (TREE_TYPE (pt_var));
  else
    return unknown[object_size_type];

  if (pt_var != TREE_OPERAND (ptr, 0))
    {
      tree var;

      if (object_size_type & 1)
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
	      || ! host_integerp (TYPE_SIZE_UNIT (TREE_TYPE (var)), 1)
	      || (pt_var_size
		  && tree_int_cst_lt (pt_var_size,
				      TYPE_SIZE_UNIT (TREE_TYPE (var)))))
	    var = pt_var;
	  else if (var != pt_var && TREE_CODE (pt_var) == MEM_REF)
	    {
	      tree v = var;
	      /* For &X->fld, compute object size only if fld isn't the last
		 field, as struct { int i; char c[1]; } is often used instead
		 of flexible array member.  */
	      while (v && v != pt_var)
		switch (TREE_CODE (v))
		  {
		  case ARRAY_REF:
		    if (TYPE_SIZE_UNIT (TREE_TYPE (TREE_OPERAND (v, 0)))
			&& TREE_CODE (TREE_OPERAND (v, 1)) == INTEGER_CST)
		      {
			tree domain
			  = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (v, 0)));
			if (domain
			    && TYPE_MAX_VALUE (domain)
			    && TREE_CODE (TYPE_MAX_VALUE (domain))
			       == INTEGER_CST
			    && tree_int_cst_lt (TREE_OPERAND (v, 1),
						TYPE_MAX_VALUE (domain)))
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
		    if (TREE_CODE (TREE_TYPE (v)) != ARRAY_TYPE)
		      {
			v = NULL_TREE;
			break;
		      }
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
			tree fld_chain = DECL_CHAIN (TREE_OPERAND (v, 1));
			for (; fld_chain; fld_chain = DECL_CHAIN (fld_chain))
			  if (TREE_CODE (fld_chain) == FIELD_DECL)
			    break;

			if (fld_chain)
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
	var_size = TYPE_SIZE_UNIT (TREE_TYPE (var));
      else if (!pt_var_size)
	return unknown[object_size_type];
      else
	var_size = pt_var_size;
      bytes = compute_object_offset (TREE_OPERAND (ptr, 0), var);
      if (bytes != error_mark_node)
	{
	  if (TREE_CODE (bytes) == INTEGER_CST
	      && tree_int_cst_lt (var_size, bytes))
	    bytes = size_zero_node;
	  else
	    bytes = size_binop (MINUS_EXPR, var_size, bytes);
	}
      if (var != pt_var
	  && pt_var_size
	  && TREE_CODE (pt_var) == MEM_REF
	  && bytes != error_mark_node)
	{
	  tree bytes2 = compute_object_offset (TREE_OPERAND (ptr, 0), pt_var);
	  if (bytes2 != error_mark_node)
	    {
	      bytes2 = size_binop (PLUS_EXPR, bytes2,
				   TREE_OPERAND (pt_var, 1));
	      if (TREE_CODE (bytes2) == INTEGER_CST
		  && tree_int_cst_lt (pt_var_size, bytes2))
		bytes2 = size_zero_node;
	      else
		bytes2 = size_binop (MINUS_EXPR, pt_var_size, bytes2);
	      bytes = size_binop (MIN_EXPR, bytes, bytes2);
	    }
	}
    }
  else if (!pt_var_size)
    return unknown[object_size_type];
  else
    bytes = pt_var_size;

  if (host_integerp (bytes, 1))
    return tree_low_cst (bytes, 1);

  return unknown[object_size_type];
}


/* Compute __builtin_object_size for CALL, which is a GIMPLE_CALL.
   Handles various allocation calls.  OBJECT_SIZE_TYPE is the second
   argument from __builtin_object_size.  If unknown, return
   unknown[object_size_type].  */

static unsigned HOST_WIDE_INT
alloc_object_size (const_gimple call, int object_size_type)
{
  tree callee, bytes = NULL_TREE;
  tree alloc_size;
  int arg1 = -1, arg2 = -1;

  gcc_assert (is_gimple_call (call));

  callee = gimple_call_fndecl (call);
  if (!callee)
    return unknown[object_size_type];

  alloc_size = lookup_attribute ("alloc_size", TYPE_ATTRIBUTES (TREE_TYPE(callee)));
  if (alloc_size && TREE_VALUE (alloc_size))
    {
      tree p = TREE_VALUE (alloc_size);

      arg1 = TREE_INT_CST_LOW (TREE_VALUE (p))-1;
      if (TREE_CHAIN (p))
        arg2 = TREE_INT_CST_LOW (TREE_VALUE (TREE_CHAIN (p)))-1;
    }

  if (DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (callee))
      {
      case BUILT_IN_CALLOC:
	arg2 = 1;
	/* fall through */
      case BUILT_IN_MALLOC:
      case BUILT_IN_ALLOCA:
	arg1 = 0;
      default:
	break;
      }

  if (arg1 < 0 || arg1 >= (int)gimple_call_num_args (call)
      || TREE_CODE (gimple_call_arg (call, arg1)) != INTEGER_CST
      || (arg2 >= 0
	  && (arg2 >= (int)gimple_call_num_args (call)
	      || TREE_CODE (gimple_call_arg (call, arg2)) != INTEGER_CST)))
    return unknown[object_size_type];

  if (arg2 >= 0)
    bytes = size_binop (MULT_EXPR,
	fold_convert (sizetype, gimple_call_arg (call, arg1)),
	fold_convert (sizetype, gimple_call_arg (call, arg2)));
  else if (arg1 >= 0)
    bytes = fold_convert (sizetype, gimple_call_arg (call, arg1));

  if (bytes && host_integerp (bytes, 1))
    return tree_low_cst (bytes, 1);

  return unknown[object_size_type];
}


/* If object size is propagated from one of function's arguments directly
   to its return value, return that argument for GIMPLE_CALL statement CALL.
   Otherwise return NULL.  */

static tree
pass_through_call (const_gimple call)
{
  tree callee = gimple_call_fndecl (call);

  if (callee
      && DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (callee))
      {
      case BUILT_IN_MEMCPY:
      case BUILT_IN_MEMMOVE:
      case BUILT_IN_MEMSET:
      case BUILT_IN_STRCPY:
      case BUILT_IN_STRNCPY:
      case BUILT_IN_STRCAT:
      case BUILT_IN_STRNCAT:
      case BUILT_IN_MEMCPY_CHK:
      case BUILT_IN_MEMMOVE_CHK:
      case BUILT_IN_MEMSET_CHK:
      case BUILT_IN_STRCPY_CHK:
      case BUILT_IN_STRNCPY_CHK:
      case BUILT_IN_STRCAT_CHK:
      case BUILT_IN_STRNCAT_CHK:
	if (gimple_call_num_args (call) >= 1)
	  return gimple_call_arg (call, 0);
	break;
      default:
	break;
      }

  return NULL_TREE;
}


/* Compute __builtin_object_size value for PTR.  OBJECT_SIZE_TYPE is the
   second argument from __builtin_object_size.  */

unsigned HOST_WIDE_INT
compute_builtin_object_size (tree ptr, int object_size_type)
{
  gcc_assert (object_size_type >= 0 && object_size_type <= 3);

  if (! offset_limit)
    init_offset_limit ();

  if (TREE_CODE (ptr) == ADDR_EXPR)
    return addr_object_size (NULL, ptr, object_size_type);

  if (TREE_CODE (ptr) == SSA_NAME
      && POINTER_TYPE_P (TREE_TYPE (ptr))
      && object_sizes[object_size_type] != NULL)
    {
      if (!bitmap_bit_p (computed[object_size_type], SSA_NAME_VERSION (ptr)))
	{
	  struct object_size_info osi;
	  bitmap_iterator bi;
	  unsigned int i;

	  if (dump_file)
	    {
	      fprintf (dump_file, "Computing %s %sobject size for ",
		       (object_size_type & 2) ? "minimum" : "maximum",
		       (object_size_type & 1) ? "sub" : "");
	      print_generic_expr (dump_file, ptr, dump_flags);
	      fprintf (dump_file, ":\n");
	    }

	  osi.visited = BITMAP_ALLOC (NULL);
	  osi.reexamine = BITMAP_ALLOC (NULL);
	  osi.object_size_type = object_size_type;
	  osi.depths = NULL;
	  osi.stack = NULL;
	  osi.tos = NULL;

	  /* First pass: walk UD chains, compute object sizes that
	     can be computed.  osi.reexamine bitmap at the end will
	     contain what variables were found in dependency cycles
	     and therefore need to be reexamined.  */
	  osi.pass = 0;
	  osi.changed = false;
	  collect_object_sizes_for (&osi, ptr);

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
	      if (object_size_type & 2)
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
		if (object_sizes[object_size_type][i]
		    != unknown[object_size_type])
		  {
		    print_generic_expr (dump_file, ssa_name (i),
					dump_flags);
		    fprintf (dump_file,
			     ": %s %sobject size "
			     HOST_WIDE_INT_PRINT_UNSIGNED "\n",
			     (object_size_type & 2) ? "minimum" : "maximum",
			     (object_size_type & 1) ? "sub" : "",
			     object_sizes[object_size_type][i]);
		  }
	    }

	  BITMAP_FREE (osi.reexamine);
	  BITMAP_FREE (osi.visited);
	}

      return object_sizes[object_size_type][SSA_NAME_VERSION (ptr)];
    }

  return unknown[object_size_type];
}

/* Compute object_sizes for PTR, defined to VALUE, which is not an SSA_NAME.  */

static void
expr_object_size (struct object_size_info *osi, tree ptr, tree value)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (ptr);
  unsigned HOST_WIDE_INT bytes;

  gcc_assert (object_sizes[object_size_type][varno]
	      != unknown[object_size_type]);
  gcc_assert (osi->pass == 0);

  if (TREE_CODE (value) == WITH_SIZE_EXPR)
    value = TREE_OPERAND (value, 0);

  /* Pointer variables should have been handled by merge_object_sizes.  */
  gcc_assert (TREE_CODE (value) != SSA_NAME
	      || !POINTER_TYPE_P (TREE_TYPE (value)));

  if (TREE_CODE (value) == ADDR_EXPR)
    bytes = addr_object_size (osi, value, object_size_type);
  else
    bytes = unknown[object_size_type];

  if ((object_size_type & 2) == 0)
    {
      if (object_sizes[object_size_type][varno] < bytes)
	object_sizes[object_size_type][varno] = bytes;
    }
  else
    {
      if (object_sizes[object_size_type][varno] > bytes)
	object_sizes[object_size_type][varno] = bytes;
    }
}


/* Compute object_sizes for PTR, defined to the result of a call.  */

static void
call_object_size (struct object_size_info *osi, tree ptr, gimple call)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (ptr);
  unsigned HOST_WIDE_INT bytes;

  gcc_assert (is_gimple_call (call));

  gcc_assert (object_sizes[object_size_type][varno]
	      != unknown[object_size_type]);
  gcc_assert (osi->pass == 0);

  bytes = alloc_object_size (call, object_size_type);

  if ((object_size_type & 2) == 0)
    {
      if (object_sizes[object_size_type][varno] < bytes)
	object_sizes[object_size_type][varno] = bytes;
    }
  else
    {
      if (object_sizes[object_size_type][varno] > bytes)
	object_sizes[object_size_type][varno] = bytes;
    }
}


/* Compute object_sizes for PTR, defined to an unknown value.  */

static void
unknown_object_size (struct object_size_info *osi, tree ptr)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (ptr);
  unsigned HOST_WIDE_INT bytes;

  gcc_assert (object_sizes[object_size_type][varno]
	      != unknown[object_size_type]);
  gcc_assert (osi->pass == 0);

  bytes = unknown[object_size_type];

  if ((object_size_type & 2) == 0)
    {
      if (object_sizes[object_size_type][varno] < bytes)
	object_sizes[object_size_type][varno] = bytes;
    }
  else
    {
      if (object_sizes[object_size_type][varno] > bytes)
	object_sizes[object_size_type][varno] = bytes;
    }
}


/* Merge object sizes of ORIG + OFFSET into DEST.  Return true if
   the object size might need reexamination later.  */

static bool
merge_object_sizes (struct object_size_info *osi, tree dest, tree orig,
		    unsigned HOST_WIDE_INT offset)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (dest);
  unsigned HOST_WIDE_INT orig_bytes;

  if (object_sizes[object_size_type][varno] == unknown[object_size_type])
    return false;
  if (offset >= offset_limit)
    {
      object_sizes[object_size_type][varno] = unknown[object_size_type];
      return false;
    }

  if (osi->pass == 0)
    collect_object_sizes_for (osi, orig);

  orig_bytes = object_sizes[object_size_type][SSA_NAME_VERSION (orig)];
  if (orig_bytes != unknown[object_size_type])
    orig_bytes = (offset > orig_bytes)
		 ? (unsigned HOST_WIDE_INT) 0 : orig_bytes - offset;

  if ((object_size_type & 2) == 0)
    {
      if (object_sizes[object_size_type][varno] < orig_bytes)
	{
	  object_sizes[object_size_type][varno] = orig_bytes;
	  osi->changed = true;
	}
    }
  else
    {
      if (object_sizes[object_size_type][varno] > orig_bytes)
	{
	  object_sizes[object_size_type][varno] = orig_bytes;
	  osi->changed = true;
	}
    }
  return bitmap_bit_p (osi->reexamine, SSA_NAME_VERSION (orig));
}


/* Compute object_sizes for VAR, defined to the result of an assignment
   with operator POINTER_PLUS_EXPR.  Return true if the object size might
   need reexamination  later.  */

static bool
plus_stmt_object_size (struct object_size_info *osi, tree var, gimple stmt)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (var);
  unsigned HOST_WIDE_INT bytes;
  tree op0, op1;

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

  if (object_sizes[object_size_type][varno] == unknown[object_size_type])
    return false;

  /* Handle PTR + OFFSET here.  */
  if (TREE_CODE (op1) == INTEGER_CST
      && (TREE_CODE (op0) == SSA_NAME
	  || TREE_CODE (op0) == ADDR_EXPR))
    {
      if (! host_integerp (op1, 1))
	bytes = unknown[object_size_type];
      else if (TREE_CODE (op0) == SSA_NAME)
	return merge_object_sizes (osi, var, op0, tree_low_cst (op1, 1));
      else
	{
	  unsigned HOST_WIDE_INT off = tree_low_cst (op1, 1);

          /* op0 will be ADDR_EXPR here.  */
	  bytes = addr_object_size (osi, op0, object_size_type);
	  if (bytes == unknown[object_size_type])
	    ;
	  else if (off > offset_limit)
	    bytes = unknown[object_size_type];
	  else if (off > bytes)
	    bytes = 0;
	  else
	    bytes -= off;
	}
    }
  else
    bytes = unknown[object_size_type];

  if ((object_size_type & 2) == 0)
    {
      if (object_sizes[object_size_type][varno] < bytes)
	object_sizes[object_size_type][varno] = bytes;
    }
  else
    {
      if (object_sizes[object_size_type][varno] > bytes)
	object_sizes[object_size_type][varno] = bytes;
    }
  return false;
}


/* Compute object_sizes for VAR, defined to VALUE, which is
   a COND_EXPR.  Return true if the object size might need reexamination
   later.  */

static bool
cond_expr_object_size (struct object_size_info *osi, tree var, tree value)
{
  tree then_, else_;
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (var);
  bool reexamine = false;

  gcc_assert (TREE_CODE (value) == COND_EXPR);

  if (object_sizes[object_size_type][varno] == unknown[object_size_type])
    return false;

  then_ = COND_EXPR_THEN (value);
  else_ = COND_EXPR_ELSE (value);

  if (TREE_CODE (then_) == SSA_NAME)
    reexamine |= merge_object_sizes (osi, var, then_, 0);
  else
    expr_object_size (osi, var, then_);

  if (TREE_CODE (else_) == SSA_NAME)
    reexamine |= merge_object_sizes (osi, var, else_, 0);
  else
    expr_object_size (osi, var, else_);

  return reexamine;
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
   subtraction, object size is unknown[object_size_type].
   To differentiate addition from subtraction, ADDR_EXPR returns
   unknown[object_size_type] for all objects bigger than half of the address
   space, and constants less than half of the address space are considered
   addition, while bigger constants subtraction.
   For a memcpy like GIMPLE_CALL that always returns one of its arguments, the
   object size is object size of that argument.
   Otherwise, object size is the maximum of object sizes of variables
   that it might be set to.  */

static void
collect_object_sizes_for (struct object_size_info *osi, tree var)
{
  int object_size_type = osi->object_size_type;
  unsigned int varno = SSA_NAME_VERSION (var);
  gimple stmt;
  bool reexamine;

  if (bitmap_bit_p (computed[object_size_type], varno))
    return;

  if (osi->pass == 0)
    {
      if (bitmap_set_bit (osi->visited, varno))
	{
	  object_sizes[object_size_type][varno]
	    = (object_size_type & 2) ? -1 : 0;
	}
      else
	{
	  /* Found a dependency loop.  Mark the variable for later
	     re-examination.  */
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
        else if (gimple_assign_single_p (stmt)
                 || gimple_assign_unary_nop_p (stmt))
          {
            if (TREE_CODE (rhs) == SSA_NAME
                && POINTER_TYPE_P (TREE_TYPE (rhs)))
              reexamine = merge_object_sizes (osi, var, rhs, 0);
            else if (TREE_CODE (rhs) == COND_EXPR)
              reexamine = cond_expr_object_size (osi, var, rhs);
            else
              expr_object_size (osi, var, rhs);
          }
        else
          unknown_object_size (osi, var);
        break;
      }

    case GIMPLE_CALL:
      {
        tree arg = pass_through_call (stmt);
        if (arg)
          {
            if (TREE_CODE (arg) == SSA_NAME
                && POINTER_TYPE_P (TREE_TYPE (arg)))
              reexamine = merge_object_sizes (osi, var, arg, 0);
            else if (TREE_CODE (arg) == COND_EXPR)
              reexamine = cond_expr_object_size (osi, var, arg);
            else
              expr_object_size (osi, var, arg);
          }
        else
          call_object_size (osi, var, stmt);
	break;
      }

    case GIMPLE_ASM:
      /* Pointers defined by __asm__ statements can point anywhere.  */
      object_sizes[object_size_type][varno] = unknown[object_size_type];
      break;

    case GIMPLE_NOP:
      {
	tree decl = SSA_NAME_VAR (var);

	if (TREE_CODE (decl) != PARM_DECL && DECL_INITIAL (decl))
	  expr_object_size (osi, var, DECL_INITIAL (decl));
	else
	  expr_object_size (osi, var, decl);
      }
      break;

    case GIMPLE_PHI:
      {
	unsigned i;

	for (i = 0; i < gimple_phi_num_args (stmt); i++)
	  {
	    tree rhs = gimple_phi_arg (stmt, i)->def;

	    if (object_sizes[object_size_type][varno]
		== unknown[object_size_type])
	      break;

	    if (TREE_CODE (rhs) == SSA_NAME)
	      reexamine |= merge_object_sizes (osi, var, rhs, 0);
	    else if (osi->pass == 0)
	      expr_object_size (osi, var, rhs);
	  }
	break;
      }

    default:
      gcc_unreachable ();
    }

  if (! reexamine
      || object_sizes[object_size_type][varno] == unknown[object_size_type])
    {
      bitmap_set_bit (computed[object_size_type], varno);
      bitmap_clear_bit (osi->reexamine, varno);
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
  gimple stmt = SSA_NAME_DEF_STMT (var);
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
	      object_sizes[osi->object_size_type][*sp] = 0;
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
        tree arg = pass_through_call (stmt);
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
  gimple stmt = SSA_NAME_DEF_STMT (var);

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

      if (integer_zerop (cst))
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

  if (object_sizes[0])
    return;

  for (object_size_type = 0; object_size_type <= 3; object_size_type++)
    {
      object_sizes[object_size_type] = XNEWVEC (unsigned HOST_WIDE_INT, num_ssa_names);
      computed[object_size_type] = BITMAP_ALLOC (NULL);
    }

  init_offset_limit ();
}


/* Destroy data structures after the object size computation.  */

void
fini_object_sizes (void)
{
  int object_size_type;

  for (object_size_type = 0; object_size_type <= 3; object_size_type++)
    {
      free (object_sizes[object_size_type]);
      BITMAP_FREE (computed[object_size_type]);
      object_sizes[object_size_type] = NULL;
    }
}


/* Simple pass to optimize all __builtin_object_size () builtins.  */

static unsigned int
compute_object_sizes (void)
{
  basic_block bb;
  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator i;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
	{
	  tree callee, result;
	  gimple call = gsi_stmt (i);

          if (gimple_code (call) != GIMPLE_CALL)
	    continue;

	  callee = gimple_call_fndecl (call);
	  if (!callee
	      || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL
	      || DECL_FUNCTION_CODE (callee) != BUILT_IN_OBJECT_SIZE)
	    continue;

	  init_object_sizes ();
	  result = fold_call_stmt (call, false);
	  if (!result)
	    {
	      if (gimple_call_num_args (call) == 2
		  && POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (call, 0))))
		{
		  tree ost = gimple_call_arg (call, 1);

		  if (host_integerp (ost, 1))
		    {
		      unsigned HOST_WIDE_INT object_size_type
			= tree_low_cst (ost, 1);

		      if (object_size_type < 2)
			result = fold_convert (size_type_node,
					       integer_minus_one_node);
		      else if (object_size_type < 4)
			result = build_zero_cst (size_type_node);
		    }
		}

	      if (!result)
		continue;
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Simplified\n  ");
	      print_gimple_stmt (dump_file, call, 0, dump_flags);
	    }

	  if (!update_call_from_tree (&i, result))
	    gcc_unreachable ();

          /* NOTE: In the pre-tuples code, we called update_stmt here.  This is
             now handled by gsi_replace, called from update_call_from_tree.  */

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "to\n  ");
	      print_gimple_stmt (dump_file, call, 0, dump_flags);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  fini_object_sizes ();
  return 0;
}

struct gimple_opt_pass pass_object_sizes =
{
 {
  GIMPLE_PASS,
  "objsz",				/* name */
  NULL,					/* gate */
  compute_object_sizes,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa	/* todo_flags_finish */
 }
};
