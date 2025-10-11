/* Lower generators.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"


typedef tree (*allocator_t) (tree, tree);

/* Lower to code that fill in BOUNDS and elements pointers in the given buffer
   pointed by BUFFER at offset OFFSET according to the mode MODE, and evals to
   BUFFER.  */

static tree
fill_in_buffer (tree buffer, tree offset, tree_stmt_iterator *bounds, MOID_T *m,
		allocator_t allocator)
{
  tree filler = NULL_TREE;
  tree type = CTYPE (m);
  tree pointer_type = build_pointer_type (type);

  a68_push_stmt_list (M_VOID);

  if (m == M_INT || m == M_BOOL || m == M_CHAR || m == M_REAL || IS_REF (m))
    {
      tree val_address = fold_build2 (POINTER_PLUS_EXPR, pointer_type, buffer, offset);
      tree init_val = a68_get_skip_tree (m);
      tree modify = fold_build2 (MODIFY_EXPR,
				 type,
				 fold_build1 (INDIRECT_REF, type, val_address),
				 init_val);
      a68_add_stmt (modify);
    }
  else if (!HAS_ROWS (m))
    {
      /* This mode has no rows.  We can just fill in with zeroes, which
	 translates into SKIP values for all possibly contained types.  */
      tree call = builtin_decl_explicit (BUILT_IN_MEMSET);
      call = build_call_expr_loc (UNKNOWN_LOCATION, call, 3,
				  buffer,
				  integer_zero_node,
				  fold_convert (sizetype, size_in_bytes (CTYPE (m))));
      a68_add_stmt (call);
    }
  else if (m == M_STRING)
    {
      /* Strings are rows but handled especially as they are created empty and
	 don't feature bounds in the formal declarer.  */

      /* First the descriptor.  */
      tree pointer_byte_size = size_int (POINTER_SIZE / BITS_PER_UNIT);
      tree lb_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
      a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
				 fold_build1 (INDIRECT_REF, ssizetype, lb_address),
				 ssize_int (1)));
      offset = fold_build2 (PLUS_EXPR, sizetype, offset, pointer_byte_size);
      tree ub_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
      a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
				 fold_build1 (INDIRECT_REF, ssizetype, ub_address),
				 ssize_int (0)));
      offset = fold_build2 (PLUS_EXPR, sizetype, offset, pointer_byte_size);
      tree stride_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
      a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
				 fold_build1 (INDIRECT_REF, sizetype, stride_address),
				 size_in_bytes (a68_char_type)));

      /* The data is an empty string, i.e NULL.  */
      offset = fold_build2 (PLUS_EXPR, sizetype, offset, pointer_byte_size);
      tree elems_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
      a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
				 fold_build1 (INDIRECT_REF, build_pointer_type (a68_char_type),
					      elems_address),
				 build_int_cst (build_pointer_type (a68_char_type), 0)));

      /* The size of the elements is zero.  */
      offset = fold_build2 (PLUS_EXPR, sizetype, offset, pointer_byte_size);
      tree elems_size_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
      a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
				 fold_build1 (INDIRECT_REF, build_pointer_type (a68_char_type),
					      elems_size_address),
				 size_zero_node));
    }
  else if (A68_ROW_TYPE_P (type))
    {
      /* If the row mode is flexible we can deflex it now: these also must have
	 bounds specified for them, with the only exception of strings/flexible
	 rows of chars, which are handled above.  Note we cannot use DEFLEXED
	 here because that contains the fully deflexed mode.  For example,
	 DEFLEXED returns [][]INT for FLEX[]FLEX[]INT, and we want []FLEX[]INT
	 instead.  */
      if (IS_FLEX (m))
	m = SUB (m);

      /* Consume two bounds from BOUNDS for each dimension and patch them at
	 their right offsets.  Note that we have to process from upper
	 dimension to lower dimension so we can calculate the stride as we
	 go.  */
      size_t dim = DIM (m);

      /* Collect lower and upper bounds and calculate the number of elements of
	 the multiple.  */
      tree *lower_bounds = (tree *) xmalloc (sizeof (tree) * dim);
      tree *upper_bounds = (tree *) xmalloc (sizeof (tree) * dim);
      tree num_elems = NULL_TREE;
      for (size_t i = 0; i < dim; ++i)
	{
	  /* Note we have to convert the bounds from CTYPE(M_INT) to
	     ssizetype.  */
	  lower_bounds[i] = fold_convert (ssizetype, save_expr (tsi_stmt (*bounds)));
	  tsi_next (bounds);
	  upper_bounds[i] = fold_convert (ssizetype, save_expr (tsi_stmt (*bounds)));
	  tsi_next (bounds);

	  tree dim_num_elems
	    = fold_build2 (PLUS_EXPR, sizetype,
			   fold_convert (sizetype,
					 fold_build2 (MINUS_EXPR, ssizetype,
						      upper_bounds[i], lower_bounds[i])),
			   size_one_node);
	  dim_num_elems = fold_build3 (COND_EXPR,
				       sizetype,
				       fold_build2 (LT_EXPR, ssizetype,
						    upper_bounds[i], lower_bounds[i]),
				       size_zero_node,
				       dim_num_elems);
	  if (num_elems == NULL_TREE)
	    num_elems = dim_num_elems;
	  else
	    num_elems = fold_build2 (MULT_EXPR, sizetype, num_elems, dim_num_elems);
	}

      /* Calculate strides.  */
      tree *strides = (tree *) xmalloc (sizeof (tree) * dim);
      a68_multiple_compute_strides (type, dim, lower_bounds, upper_bounds, strides);

      /* Now emit instructions to patch the bounds and strides.  */
      tree pointer_byte_size = size_int (POINTER_SIZE / BITS_PER_UNIT);
      for (size_t i = 0; i < dim; ++i)
	{
	  /* Lower bound.  */
	  tree lb_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
	  a68_add_stmt (fold_build2 (MODIFY_EXPR,
				     void_type_node,
				     fold_build1 (INDIRECT_REF, ssizetype, lb_address),
				     lower_bounds[i]));
	  /* Upper bound.  */
	  offset = fold_build2 (PLUS_EXPR, sizetype, offset, pointer_byte_size);
	  tree ub_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
	  a68_add_stmt (fold_build2 (MODIFY_EXPR,
				     void_type_node,
				     fold_build1 (INDIRECT_REF, ssizetype, ub_address),
				     upper_bounds[i]));
	  /* Stride.  */
	  offset = fold_build2 (PLUS_EXPR, sizetype, offset, pointer_byte_size);
	  tree stride_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
	  a68_add_stmt (fold_build2 (MODIFY_EXPR,
				     void_type_node,
				     fold_build1 (INDIRECT_REF, sizetype, stride_address),
				     strides[i]));
	  offset = fold_build2 (PLUS_EXPR, sizetype, offset, pointer_byte_size);
	}
      free (lower_bounds);
      free (upper_bounds);
      free (strides);

      /* Now allocate space for the elements.  */
      MOID_T *elem_mode = SUB (m);
      tree elem_size = fold_convert (sizetype, size_in_bytes (CTYPE (elem_mode)));
      tree elems_size = save_expr (fold_build2 (MULT_EXPR, sizetype, elem_size, num_elems));
      tree elemsptr = (*allocator) (CTYPE (elem_mode), elems_size);
      elemsptr = save_expr (elemsptr);

      /* And initialize them.  */
      if (elem_mode == M_INT || elem_mode == M_BOOL || elem_mode == M_CHAR
	  || elem_mode == M_REAL || IS_REF (elem_mode))
	{
	  /* Memsetting the buffer with either zeroes or ones satisfies the
	     SKIP value for these modes.  */
	  tree call = builtin_decl_explicit (BUILT_IN_MEMSET);
	  call = build_call_expr_loc (UNKNOWN_LOCATION, call, 3,
				      elemsptr,
				      integer_zero_node,
				      elems_size);
	  a68_add_stmt (call);
	}
      else
	{
	  /* Recurse in a loop to fill in elements.  */
	  a68_push_range (NULL);
	  tree num_elems_var = a68_lower_tmpvar ("numelems%", size_type_node,
						 num_elems);
	  tree index = a68_lower_tmpvar ("index%", size_type_node, size_zero_node);
	  tree elems_var = a68_lower_tmpvar ("elems%", TREE_TYPE (elemsptr),
					     elemsptr);
	  tree elem_offset = a68_lower_tmpvar ("elem_offset%", size_type_node,
					       size_zero_node);

	  /* Begin of loop body.  */
	  a68_push_range (NULL);
	  a68_add_stmt (fold_build1 (EXIT_EXPR,
				     void_type_node,
				     fold_build2 (EQ_EXPR,
						  size_type_node,
						  index, num_elems_var)));
	  a68_add_stmt (fill_in_buffer (elems_var, elem_offset, bounds, elem_mode,
					allocator));
	  /* Increase elem_offset  */
	  a68_add_stmt (fold_build2 (MODIFY_EXPR, sizetype,
				     elem_offset,
				     fold_build2 (PLUS_EXPR, sizetype,
						  elem_offset, elem_size)));
	  /* index++ */
	  a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR,
				     size_type_node,
				     index, size_one_node));
	  tree loop_body = a68_pop_range ();
	  /* End of loop body.  */
	  a68_add_stmt (fold_build1 (LOOP_EXPR,
				     void_type_node,
				     loop_body));
	  a68_add_stmt (a68_pop_range ());
	}

      /* Patch the elements% field.  */
      tree elems_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
      a68_add_stmt (fold_build2 (MODIFY_EXPR,
				 void_type_node,
				 fold_build1 (INDIRECT_REF,
					      build_pointer_type (CTYPE (elem_mode)), elems_address),
				 elemsptr));
      /* Patch the elements_size% field.  */
      offset = fold_build2 (PLUS_EXPR, sizetype, offset, pointer_byte_size);
      tree elems_size_address = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
      a68_add_stmt (fold_build2 (MODIFY_EXPR,
				 void_type_node,
				 fold_build1 (INDIRECT_REF,
					      sizetype,
					      elems_size_address),
				 elems_size));
    }
  else if (A68_STRUCT_TYPE_P (type))
    {
      /* Initialize the struct's fields in the allocated buffer.  */
      tree base = a68_lower_tmpvar ("base%", TREE_TYPE (buffer),
				    fold_build2 (POINTER_PLUS_EXPR,
						 TREE_TYPE (buffer),
						 buffer, offset));
      PACK_T *field_pack = PACK (m);
      for (tree field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  gcc_assert (COMPLETE_TYPE_P (TREE_TYPE (field)));
	  //	  printf ("BYTE_POSITION\n");
	  //	  debug_tree (byte_position (field));
	  a68_add_stmt (fill_in_buffer (base, byte_position (field),
					bounds, MOID (field_pack), allocator));
	  FORWARD (field_pack);
	}
    }
  else if (A68_UNION_TYPE_P (type))
    {
      /* Union values are initialized with an overhead of (sizetype) -1, which
	 means it is not initialized.  Note that row declarers in united modes
	 are formal declarers, so they never contribute bounds. */
      tree overhead_address
	= fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer), buffer, offset);
      tree uninitialized = fold_convert (sizetype, build_minus_one_cst (ssizetype));
      a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
				 fold_build1 (INDIRECT_REF, sizetype, overhead_address),
				 uninitialized));
#if 0
      /* Set the rest of the union with zeroes.  */
      tree value_address
	= fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (buffer),
		       buffer,
		       fold_build2 (PLUS_EXPR, sizetype, offset, size_in_bytes (sizetype)));

      tree value_field = TREE_CHAIN (TYPE_FIELDS (type));
      tree call = builtin_decl_explicit (BUILT_IN_MEMSET);
      call = build_call_expr_loc (UNKNOWN_LOCATION, call, 3,
				  value_address,
				  integer_zero_node,
				  size_in_bytes (TREE_TYPE (value_field)));
      a68_add_stmt (call);
#endif
    }
  else
    gcc_unreachable ();

  a68_add_stmt (buffer);
  filler = a68_pop_stmt_list ();
  TREE_TYPE (filler) = pointer_type;
  return filler;
}

/* Lower to code that generates storage for a value of mode M, using bounds
   from BOUNDS.  */

static tree
gen_mode (MOID_T *m, tree_stmt_iterator *bounds, allocator_t allocator)
{
  /* Allocate space for the value and fill it.  */
  tree buffer = (*allocator) (CTYPE (m), size_in_bytes (CTYPE (m)));
  buffer = save_expr (buffer);
  return fill_in_buffer (buffer, size_zero_node, bounds, m, allocator);
}

/* Collect row bounds from BOUNDS.
   Lower bounds are optional, and if not found they default to 1.  */

static void
collect_bounds (NODE_T *p, LOW_CTX_T ctx)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, BOUNDS_LIST))
	collect_bounds (SUB (p), ctx);
      else if (IS (p, BOUND))
	collect_bounds (SUB (p), ctx);
      else if (IS (p, UNIT))
	{
	  /* First the lower bound.  */
	  tree lower_bound;
	  if (NEXT (p) != NO_NODE && IS (NEXT (p), COLON_SYMBOL))
	    {
	      lower_bound = a68_lower_tree (p, ctx);
	      p = NEXT_NEXT (p);
	    }
	  else
	    /* Default lower bound.  */
	    lower_bound = integer_one_node;

	  /* Now the upper bound.  */
	  tree upper_bound = a68_lower_tree (p, ctx);

	  /* See the comment for collect_declarer_bounds for an explanation for
	     the usage of save_expr here.  */
	  a68_add_stmt (save_expr (lower_bound));
	  a68_add_stmt (save_expr (upper_bound));
	}
    }
}

/* Append all the bounds found in the given declarer in the current statements
   list.  */

static void
collect_declarer_bounds_1 (NODE_T *p, LOW_CTX_T ctx)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, BOUNDS))
	collect_bounds (SUB (p), ctx);
      else if (IS (p, INDICANT) && IS_LITERALLY (p, "STRING"))
	return;
      else if (IS (p, INDICANT))
	{
	  if (TAX (p) != NO_TAG && HAS_ROWS (MOID (TAX (p))))
	    /* Continue from definition at MODE A = ....  */
	    collect_declarer_bounds_1 (NEXT_NEXT (NODE (TAX (p))), ctx);
	}
      else if (IS (p, DECLARER)
	       && (IS_UNION (MOID (p)) || !HAS_ROWS (MOID (p))))
	  return;
      else
	collect_declarer_bounds_1 (SUB (p), ctx);
    }
}

/* Given a declarer node, return a statements list with all the expressions of
   the bounds within it.

   Note that the language rules mandates that the bounds expression shall be
   evaluated just once even when they are used by several generators, such as
   in

     [n +:= 1]real a, b;

     Therefore the expressions are saved in save_exprs and the statements list
     is cached in the CDECL field of the parse tree node.  */

static tree
collect_declarer_bounds (NODE_T *p, LOW_CTX_T ctx)
{
  if (CDECL (p) == NULL_TREE)
    {
      a68_push_stmt_list (M_VOID);
      collect_declarer_bounds_1 (SUB (p), ctx);
      CDECL (p) = a68_pop_stmt_list ();
    }

  return CDECL (p);
}

/* Low the elaboration of a generator.

   The lowered code evaluates to a pointer.

   DECLARER is the actual declarer passed to the generator.

   MODE is the mode of the value to generate.

   HEAP is true if we are lowering a heap generator, false if we are lowering a
   LOC generator.  */

tree
a68_low_generator (NODE_T *declarer,
		   MOID_T *mode,
		   bool heap, LOW_CTX_T ctx)
{
  /* If the declarer is a mode indicant which has a recursive definition then
     we need to lower to a function which gets immediately called rather than
     an expression, to handle the recursivity.  In that case, though, we need
     to always heap allocated memory for obvious reasons, which sucks, but such
     is life.  */

  if (IS (SUB (declarer), INDICANT) && TAX (SUB (declarer)) != NO_TAG
      && IS_RECURSIVE (TAX (SUB (declarer))))
    {
      if (TAX_TREE_DECL (TAX (SUB (declarer))) != NULL_TREE)
	{
	  /* This is a recursive mode indicant.  Just call the function.  */
	  return save_expr (build_call_expr_loc (a68_get_node_location (SUB (declarer)),
						 TAX_TREE_DECL (TAX (SUB (declarer))),
						 0));
	}

      tree ret_type = build_pointer_type (CTYPE (mode));
      tree func_decl = build_decl (a68_get_node_location (declarer),
				   FUNCTION_DECL,
				   NULL_TREE /* name, set below */,
				   build_function_type (ret_type, void_list_node));
      char *name = xasprintf ("genroutine%d", DECL_UID (func_decl));
      DECL_NAME (func_decl) = a68_get_mangled_identifier (name);
      free (name);
      DECL_EXTERNAL (func_decl) = 0;
      DECL_STATIC_CHAIN (func_decl) = !a68_in_global_range ();
      TREE_ADDRESSABLE (func_decl) = 1;
      TREE_PUBLIC (func_decl) = a68_in_global_range ();
      TREE_STATIC (func_decl) = 1;
      TAX_TREE_DECL (TAX (SUB (declarer))) = func_decl;

      a68_add_decl (func_decl);
      a68_add_decl_expr (fold_build1_loc (a68_get_node_location (declarer),
					  DECL_EXPR,
					  TREE_TYPE (func_decl),
					  func_decl));
      announce_function (func_decl);
      a68_push_function_range (func_decl, ret_type);

      /* Collect bounds from declarer.  */
      tree bounds = collect_declarer_bounds (declarer, ctx);

      /* Allocate and initialize a memory buffer for a value of mode MODE with
	 bounds in BOUNDS.  */
      tree_stmt_iterator bounds_iter = tsi_start (bounds);
      tree gen = gen_mode (mode, &bounds_iter, a68_lower_malloc);
      a68_pop_function_range (gen);
      /* Avoid this generator function, which uses the global lexical
	 environment, to be reused in other contexts.  */
      TAX_TREE_DECL (TAX (SUB (declarer))) = NULL_TREE;
      return save_expr (build_call_expr_loc (a68_get_node_location (declarer),
					     func_decl, 0));
    }
  else
    {
      /* Collect bounds from declarer.  */
      tree bounds = collect_declarer_bounds (declarer, ctx);

      /* Allocate and initialize a memory buffer for a value of mode MODE with
	 bounds in BOUNDS.  */
      tree_stmt_iterator bounds_iter = tsi_start (bounds);
      tree gen = gen_mode (mode, &bounds_iter,
			   heap ? a68_lower_malloc : a68_lower_alloca);
      return gen;
    }
}

/* Allocate storage for a value of mode M.
   NBOUNDS is the number of bounds in BOUNDS.  */

tree
a68_low_gen (MOID_T *m, size_t nbounds, tree *bounds, bool use_heap)
{
  /* First collect bounds from BOUNDS into a statements list, which is what
     gen_mode expects.  */
  tree bounds_list = alloc_stmt_list ();
  for (size_t i = 0; i < nbounds; ++i)
    append_to_statement_list_force (bounds[i], &bounds_list);
  allocator_t allocator = use_heap ? a68_lower_malloc : a68_lower_alloca;

  tree_stmt_iterator q = tsi_start (bounds_list);
  tree ret = gen_mode (m, &q, allocator);
  free_stmt_list (bounds_list);
  return ret;
}
