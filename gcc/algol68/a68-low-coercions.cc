/* Lower Algol 68 coercions to GENERIC.
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

/* Lower a dereferencing coercion.  */
tree
a68_lower_dereferencing (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_low_deref (a68_lower_tree (SUB (p), ctx), SUB (p));
}

/* Lower an uniting coercion.  */

tree
a68_lower_uniting (NODE_T *p, LOW_CTX_T ctx)
{
  tree coercend_tree = a68_lower_tree (SUB (p), ctx);

  if (MOID (p) == M_ROWS)
    {
      /* ROWS is a mode to which any ROW mode can be strongly coerced.  It is
	 used as the mode of the second operand of the ELEMS, LWB and UPB
	 operators.  The coercion is expressed in the parse tree via uniting.
	 This results in replacing the multiple with a "rows" value that
	 contains dimension and bounds information.  */
      if (A68_ROW_TYPE_P (TREE_TYPE (coercend_tree)))
	return a68_rows_value (coercend_tree);
      else if (A68_UNION_TYPE_P (TREE_TYPE (coercend_tree)))
	{
	  /* coercend_tree is expanded more than once below.  */
	  coercend_tree = save_expr (coercend_tree);

	  /* Union of row modes.  We should create a rows value for the currently
	     selected value.  */
	  a68_push_range (M_ROWS);
	  tree done_label = build_decl (UNKNOWN_LOCATION,
					LABEL_DECL,
					get_identifier ("done_label%"),
					void_type_node);
	  DECL_CONTEXT (done_label) = a68_range_context ();
	  a68_add_decl (done_label);
	  a68_add_decl_expr (fold_build1 (DECL_EXPR, TREE_TYPE (done_label), done_label));
	  tree rows = a68_lower_tmpvar ("rows%", CTYPE (M_ROWS),
					a68_get_skip_tree (M_ROWS));
	  tree coercend_overhead = a68_union_overhead (coercend_tree);
	  tree overhead = a68_lower_tmpvar ("overhead%", TREE_TYPE (coercend_overhead),
					    coercend_overhead);
	  int field_index = 0;
	  for (tree field = TYPE_FIELDS (TREE_TYPE (a68_union_cunion (coercend_tree)));
	       field;
	       field = DECL_CHAIN (field))
	    {
	      a68_push_range (M_VOID);
	      {
		/* Set rows% to the rows value computed from coercend_tree.FIELD,
		   which is of some multiple type.  */
		a68_add_stmt (fold_build2 (MODIFY_EXPR, CTYPE (M_ROWS),
					   rows,
					   a68_rows_value (a68_union_alternative (coercend_tree,
										  field_index))));
		a68_add_stmt (fold_build1 (GOTO_EXPR, void_type_node, done_label));
		a68_add_stmt (a68_get_skip_tree (M_VOID));
	      }
	      tree process_entry = a68_pop_range ();

	      /* IF overhead = field_index THEN rows% = rows_from_multiple FI */
	      a68_add_stmt (fold_build3 (COND_EXPR,
					 a68_void_type,
					 fold_build2 (EQ_EXPR,
						      TREE_TYPE (overhead),
						      overhead,
						      build_int_cst (TREE_TYPE (overhead), field_index)),
					 process_entry,
					 a68_get_skip_tree (M_VOID)));
	      field_index += 1;
	    }

	  /* This should not be reached.  Emit run-time error.  */
	  {
	    unsigned int lineno = NUMBER (LINE (INFO (p)));
	    const char *filename_str = FILENAME (LINE (INFO (p)));
	    tree filename = build_string_literal (strlen (filename_str) + 1,
						  filename_str);
	    tree call = a68_build_libcall (A68_LIBCALL_UNREACHABLE,
					   void_type_node, 2,
					   filename,
					   build_int_cst (unsigned_type_node, lineno));
	    a68_add_stmt (call);
	  }

	  a68_add_stmt (build1 (LABEL_EXPR, void_type_node, done_label));
	  a68_add_stmt (rows);
	  return a68_pop_range ();
	}
      else
	{
	  debug_tree (TREE_TYPE (coercend_tree));
	  gcc_assert (A68_ROWS_TYPE_P (TREE_TYPE (coercend_tree)));
	  return coercend_tree;
	}
    }
  else if (IS_UNION (MOID (SUB (p))))
    {
      /* We have to extract the value of the coercend union.  */
      a68_push_range (MOID (p));
      {
	MOID_T *coercend_mode = MOID (SUB (p));
	MOID_T *coercee_mode = MOID (p);

	/* Temporaries for the coercend's components.  */
	tree coercend = a68_lower_tmpvar ("coercend%", TREE_TYPE (coercend_tree), coercend_tree);
	tree cval = a68_union_cunion (coercend);
	tree coverhead = a68_union_overhead (coercend);
	tree coercend_value = a68_lower_tmpvar ("coercend_value%", TREE_TYPE (cval), cval);
	tree coercend_overhead = a68_lower_tmpvar ("coercend_overhead%", sizetype, coverhead);

	/* Create the coercee.  */
	tree coercee = a68_lower_tmpvar ("coercee%",
					 CTYPE (MOID (p)),
					 a68_get_skip_tree (MOID (p)));
	tree coercee_value = a68_union_cunion (coercee);

	/* First translate overhead.  This is crude, but it works.  */
	int idx = 0;
	tree coercee_overhead = size_zero_node;
	while (EQUIVALENT (coercend_mode) != NO_MOID)
	  coercend_mode = EQUIVALENT (coercend_mode);
	for (PACK_T *pack = PACK (coercend_mode); pack != NO_PACK; FORWARD (pack))
	  {
	    coercee_overhead = fold_build3 (COND_EXPR,
					    sizetype,
					    fold_build2 (EQ_EXPR,
							 sizetype,
							 coercend_overhead,
							 size_int (idx)),
					    size_int (a68_united_mode_index (coercee_mode, MOID (pack))),
					    coercee_overhead);
	    idx++;
	  }
	a68_add_stmt (a68_union_set_overhead (coercee, coercee_overhead));

	/* Now copy over the value.  This of course relies on the fact the
	   value of the coercend is smaller or of the same size than the value
	   of the built union.  */
	a68_add_stmt (a68_lower_memcpy (fold_build1 (ADDR_EXPR,
						     build_pointer_type (TREE_TYPE (coercee_value)),
						     coercee_value),
					fold_build1 (ADDR_EXPR,
						     build_pointer_type (TREE_TYPE (coercend_value)),
						     coercend_value),
					size_in_bytes (TREE_TYPE (coercend_value))));
	a68_add_stmt (coercee);
      }
      return a68_pop_range ();
    }
  else
    {
      /* Produce a united mode one of whose component modes is the mode of the
	 coercend.  */
      return a68_union_value (MOID (p), coercend_tree, MOID (SUB (p)));
    }
}

/* Lower a rowing coercion.  */

tree
a68_lower_rowing (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *mode = MOID (p);
  bool did_deref = false;

  /* If the primary is a REF, we need to dereference it to get the referred
     value.  */
  tree primary = NULL_TREE;
  tree orig_primary = NULL_TREE;
  MOID_T *target_mode = NO_MOID;
  if (IS_REF (mode))
    {
      gcc_assert (IS_REF (MOID (SUB (p))));
      did_deref = true;
      target_mode = SUB (mode);

      a68_push_range (mode);
      /* Note that we have to consolidate because we need a pointer to compare
	 to NIL below.  */
      orig_primary = a68_lower_tmpvar ("orig_primary%",
				       CTYPE (MOID (SUB (p))),
				       a68_consolidate_ref (MOID (SUB (p)),
							    a68_lower_tree (SUB (p), ctx)));
      primary = a68_low_deref (orig_primary, SUB (p));
    }
  else
    {
      target_mode = mode;
      primary = a68_lower_tree (SUB (p), ctx);
      /* The primary gets expanded more than once below.  */
      primary = save_expr (primary);
    }

  /* Perform the rowing in the primary. */
  tree ssize_one_node = fold_convert (ssizetype, size_one_node);
  tree rowed_primary = NULL_TREE;
  if (DIM (DEFLEX (target_mode)) >= 2)
    {
      /* []A -> [,]A  */

      /* First determine the number of dimensions of the resulting
	 multiple.  */
      tree primary_dimensions = a68_multiple_dimensions (primary);
      gcc_assert (TREE_CODE (primary_dimensions) == INTEGER_CST);
      int dim = tree_to_shwi (primary_dimensions) + 1;

      /* Compute bounds.  */
      tree *lower_bounds = (tree *) xmalloc (sizeof (tree) * dim);
      tree *upper_bounds = (tree *) xmalloc (sizeof (tree) * dim);

      lower_bounds[0] = ssize_one_node;
      upper_bounds[0] = ssize_one_node;
      for (int d = 1; d < dim; ++d)
	{
	  lower_bounds[d] = a68_multiple_lower_bound (primary, ssize_int (d - 1));
	  upper_bounds[d] = a68_multiple_upper_bound (primary, ssize_int (d - 1));
	}

      rowed_primary = a68_row_value (CTYPE (target_mode), dim,
				     a68_multiple_elements (primary),
				     a68_multiple_elements_size (primary),
				     lower_bounds, upper_bounds);
      free (lower_bounds);
      free (upper_bounds);
    }
  else
    {
      /* A -> []A  */
      tree row_type = CTYPE (target_mode);
      tree lower_bound = ssize_one_node;
      tree upper_bound = ssize_one_node;
      tree elements = (did_deref
		       ? orig_primary
		       : fold_build1 (ADDR_EXPR,
				      build_pointer_type (TREE_TYPE (primary)),
				      build_constructor_va (build_array_type (TREE_TYPE (primary),
									      build_index_type (size_zero_node)),
							    1, size_zero_node,  primary)));
      tree elements_type = a68_row_elements_type (row_type);
      tree elements_size = size_in_bytes (elements_type);
      rowed_primary = a68_row_value (row_type, 1,
				     elements, elements_size,
				     &lower_bound, &upper_bound);
    }

  /* Build a ref if we rowed a ref.  */
  if (did_deref)
    {
      tree pointer_type = build_pointer_type (TREE_TYPE (rowed_primary));
      rowed_primary = fold_build1 (ADDR_EXPR, pointer_type, rowed_primary);
      /* Rowing NIL yields NIL.  */
      rowed_primary = fold_build3_loc (a68_get_node_location (p),
				       COND_EXPR,
				       pointer_type,
				       fold_build2 (EQ_EXPR,
						    pointer_type,
						    fold_convert (pointer_type, orig_primary),
						    build_int_cst (pointer_type, 0)),
				       build_int_cst (pointer_type, 0),
				       rowed_primary);
      a68_add_stmt (rowed_primary);
      rowed_primary = a68_pop_range ();
    }

  return rowed_primary;
}

/* Lower a widening coercion.

   Widening allows the following conversions of mode:

   LONGSETY INT to LONGSETY REAL
   LONGSETY REAL to LONGSETY COMPL
   LONGSETY BITS to []BOOL
   LONGSETY BYTES to []CHAR  */

tree
a68_lower_widening (NODE_T *p, LOW_CTX_T ctx)
{
  if (MOID (p) == M_REAL
      || MOID (p) == M_LONG_REAL
      || MOID (p) == M_LONG_LONG_REAL)
    {
      return convert_to_real (CTYPE (MOID (p)), a68_lower_tree (SUB (p), ctx));
    }
  if (MOID (p) == M_COMPLEX
      || MOID (p) == M_LONG_COMPLEX
      || MOID (p) == M_LONG_LONG_COMPLEX)
    {
      return a68_complex_widen_from_real (MOID (p),
					  a68_lower_tree (SUB (p), ctx));
    }
  else if (MOID (p) == M_ROW_BOOL)
    {
      /* Widen a LONGSETY BITS to a row of BOOLs.  */
      tree coercend = a68_lower_tree (SUB (p), ctx);
      tree coercend_type = TREE_TYPE (coercend);
      HOST_WIDE_INT bits_size = int_size_in_bytes (coercend_type);
      gcc_assert (bits_size != -1);
      bits_size = bits_size * 8;

      tree pointer_to_bool_type = build_pointer_type (a68_bool_type);
      a68_push_range (M_ROW_BOOL);
      /* First allocate space for the elements.  */
      tree elements = a68_lower_tmpvar ("elements%",
					pointer_to_bool_type,
					a68_lower_alloca (M_BOOL,
							  fold_build2 (MULT_EXPR,
								       sizetype,
								       size_int (bits_size),
								       size_in_bytes (a68_bool_type))));

      /* Set the elements, each element is a BOOL which is TRUE if the
	 corresponding bit in the coercend is set, FALSE otherwise.  */
      tree coercend_one_node = build_int_cst (coercend_type, 1);
      coercend = save_expr (coercend);
      for (HOST_WIDE_INT bit = 0; bit < bits_size; ++bit)
	{
	  tree offset = fold_build2 (MULT_EXPR, sizetype,
				     size_int (bit), size_in_bytes (a68_bool_type));
	  tree bit_set = fold_convert (a68_bool_type,
				       fold_build2 (BIT_AND_EXPR, coercend_type,
						    fold_build2 (RSHIFT_EXPR, coercend_type,
								 coercend,
								 build_int_cst (coercend_type,
										bits_size - 1 - bit)),
						    coercend_one_node));

	  a68_add_stmt (fold_build2 (MODIFY_EXPR,
				     a68_bool_type,
				     fold_build2 (MEM_REF,
						  a68_bool_type,
						  fold_build2 (POINTER_PLUS_EXPR,
							       pointer_to_bool_type,
							       elements,
							       offset),
						  fold_convert (pointer_to_bool_type,
								integer_zero_node)),
				     bit_set));
	}

      /* Create multiple.  */
      tree lower_bound = ssize_int (1);
      tree upper_bound = ssize_int (bits_size);
      tree elements_size = fold_build2 (MULT_EXPR, sizetype,
					size_int (bits_size),
					size_in_bytes (a68_bool_type));
      tree multiple = a68_row_value (CTYPE (M_ROW_BOOL), 1 /* dim */,
				     elements, elements_size,
				     &lower_bound, &upper_bound);
      a68_add_stmt (multiple);
      return a68_pop_range ();
    }
  else
    {
      fatal_error (a68_get_node_location (p),
		   "cannot do widening from %s to %s",
		   a68_moid_to_string (MOID (SUB (p)), MOID_ERROR_WIDTH, SUB (p)),
		   a68_moid_to_string (MOID (p), MOID_ERROR_WIDTH, p));
      gcc_unreachable ();
    }
}

/* Lower a voiding coercion.

   The voiding lowers into a compound expression with the voided expression
   (for side-effects) and returns EMPTY.  */

tree
a68_lower_voiding (NODE_T *p, LOW_CTX_T ctx)
{
  return fold_build2_loc (a68_get_node_location (p),
			  COMPOUND_EXPR,
			  a68_void_type,
			  a68_lower_tree (SUB (p), ctx),
			  a68_get_empty ());
}

/* Lower a proceduring coercion.

     proceduring : jump.

   In the Revised language only jump statements can be procedured.  The
   coercion results in a new function whose body is the jump instruction.  */

tree
a68_lower_proceduring (NODE_T *p, LOW_CTX_T ctx)
{
  tree jump = a68_lower_tree (SUB (p), ctx);

  tree procedured_goto = a68_make_anonymous_routine_decl (MOID (p));
  a68_add_decl (procedured_goto);
  a68_add_decl_expr (fold_build1_loc (a68_get_node_location (p),
				      DECL_EXPR,
				      TREE_TYPE (procedured_goto),
				      procedured_goto));
  announce_function (procedured_goto);

  a68_push_function_range (procedured_goto, CTYPE (SUB (MOID (p))));
  a68_pop_function_range (jump);
  return fold_build1 (ADDR_EXPR,
		      build_pointer_type (TREE_TYPE (procedured_goto)),
		      procedured_goto);
}

/* Lower a deproceduring coercion.
   The deproceduring lowers into a call expression.  */

tree
a68_lower_deproceduring (NODE_T *p, LOW_CTX_T ctx)
{
  tree func = a68_lower_tree (SUB (p), ctx);

  if (POINTER_TYPE_P (TREE_TYPE (func)))
    {
      if (TREE_CODE (func) == ADDR_EXPR)
	func = TREE_OPERAND (func, 0);
      else
	func = fold_build1 (INDIRECT_REF,
			    TREE_TYPE (TREE_TYPE (func)),
			    func);
    }

  return build_call_expr_loc (a68_get_node_location (p), func, 0);
}
