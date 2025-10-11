/* Lower units to GENERIC.
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

/* Note that enclosed clauses, which are units, are handled in
   a68-low-clauses.  */

/* Lower an applied identifier.

   This lowers into the declaration of the referred identifier.  The
   declaration of the identifier should now be available in the symbol table
   entry for the identifier.  */

tree
a68_lower_identifier (NODE_T *p, LOW_CTX_T ctx)
{
  if (TAG_TABLE (TAX (p)) == A68_STANDENV)
    {
      /* This identifier is defined in the standard prelude.  Use its lowering
	 handler.  */
      LOWERER_T lowerer = LOWERER (TAX (p));
      return (*lowerer) (p, ctx);
    }
  else
    {
      tree id_decl = TAX_TREE_DECL (TAX (p));

      if (id_decl == NULL_TREE)
	{
	  /* This is an applied identifier used before the corresponding defining
	     identifier gets defined in either an identity declaration or a
	     variable declaration.  Create the declaration and install it in the
	     symbol table.  The declaration itself, declaration expr and
	     initialization assignment for the declaration will be emitted by the
	     corresponding declaration lowering handler.  Note that the defining
	     identifier (and therefore the declaration associated with this applied
	     identifier) may be in an outer lexical block.  */

	  if (IS (MOID (p), PROC_SYMBOL))
	    {
	      bool external = (MOIF (TAX (p)) != NO_MOIF);
	      const char *extern_symbol = EXTERN_SYMBOL (TAX (p));
	      if (VARIABLE (TAX (p)))
		{
		  if (external)
		    id_decl
		      = a68_make_variable_declaration_decl (p, NAME (MOIF (TAX (p))), external,
							    extern_symbol);
		  else
		    id_decl
		      = a68_make_variable_declaration_decl (p, ctx.module_definition_name);
		}
	      else if (IN_PROC (TAX (p)))
		{
		  if (external)
		    id_decl
		      = a68_make_proc_identity_declaration_decl (p, NAME (MOIF (TAX (p))),
								 false /* indicant */,
								 external,
								 extern_symbol);
		  else
		    id_decl
		      = a68_make_proc_identity_declaration_decl (p, ctx.module_definition_name);
		}
	      else
		{
		  if (external)
		    id_decl
		      = a68_make_identity_declaration_decl (p, NAME (MOIF (TAX (p))),
							    false /* indicant */,
							    external, extern_symbol);
		  else
		    id_decl
		      = a68_make_identity_declaration_decl (p, ctx.module_definition_name);
		}
	    }
	  else
	    {
	      bool external = (MOIF (TAX (p)) != NO_MOIF);
	      const char *extern_symbol = EXTERN_SYMBOL (TAX (p));
	      if (VARIABLE (TAX (p)))
		{
		  if (external)
		    id_decl
		      = a68_make_variable_declaration_decl (p, NAME (MOIF (TAX (p))), external,
							    extern_symbol);
		  else
		    id_decl
		      = a68_make_variable_declaration_decl (p, ctx.module_definition_name);
		}
	      else
		{
		  if (external)
		    id_decl
		      = a68_make_identity_declaration_decl (p, NAME (MOIF (TAX (p))),
							    false /* indicant */, external,
							    extern_symbol);
		  else
		    id_decl
		      = a68_make_identity_declaration_decl (p, ctx.module_definition_name);
		}
	    }

	  TAX_TREE_DECL (TAX (p)) = id_decl;
	}

      /* If the identifier refers to a FUNCTION_DECL, this means the declaration
	 was made by a procecure-identity-dclaration.  The applied identifier in
	 that case refers to the address of the corresponding function.  */
      if (TREE_CODE (id_decl) == FUNCTION_DECL)
	return fold_build1 (ADDR_EXPR,
			    build_pointer_type (TREE_TYPE (id_decl)),
			    id_decl);
      else
	return id_decl;
    }
}

/* Lower a string denotation.

   String denotations are of mode []CHAR, and lower into a multiple with a
   single dimension, and with the following characteristics:

   - The lower bound of dimension 0 is 1.
   - The upper bound of dimension 0 is strlen (NSYMBOL (p)).
   - The stride of dimension 0 is 0.
   - The pointed elements are a buffer of CHARs allocated in the stack.  */

tree
a68_lower_string_denotation (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  /* First process string breaks.  */
  char *str = a68_string_process_breaks (p, NSYMBOL (p));

  /* Build a multiple of UCS-4 CHARs from the resulting UTF-8 string.  */
  size_t ucslen;
  uint32_t *ucsbuf = a68_u8_to_u32 ((const uint8_t *) str, strlen (str),
				    NULL, &ucslen);
  free (str);
  tree string_literal = build_string_literal (ucslen * sizeof (uint32_t),
					      (char *) ucsbuf, a68_char_type);
  tree elements = string_literal;
  tree lower_bound = fold_convert (ssizetype, size_one_node);
  tree upper_bound = ssize_int (ucslen);
  tree elements_size = fold_build2 (MULT_EXPR, sizetype,
				    size_int (ucslen),
				    size_in_bytes (a68_char_type));
  tree multiple = a68_row_value (CTYPE (M_ROW_CHAR), 1,
				 elements, elements_size,
				 &lower_bound, &upper_bound);
  TREE_CONSTANT (multiple) = true;
  free (ucsbuf);
  return multiple;
}

/* Lower denotation.

     denotation : int denotation; real denotation; bits denotation;
     		  row char denotation;
		  true symbol; false symbol;
		  empty symbol;
		  longety, int denotation;
                  longety, real denotation;
		  longety, bits denotation;
		  shortety, int denotation;
		  shortety, real denotation;
		  shortety, bits denotation.

   Denotations lower into GENERIC cst expressions.  */

tree
a68_lower_denotation (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *moid = MOID (p);

  if (moid == M_VOID)
    /* EMPTY  */
    return a68_lower_empty (p, ctx);
  else if (moid == M_BOOL)
    /* TRUE or FALSE.  */
    return (NSYMBOL (p)[0] == 'T') ? boolean_true_node : boolean_false_node;
  else if (moid == M_CHAR)
    {
      char *s = a68_string_process_breaks (p, NSYMBOL (p));
      uint32_t ucs;
      int length = a68_u8_mbtouc (&ucs, (const uint8_t *) s, 1);
      gcc_assert (length == 1);
      free (s);
      return build_int_cst (a68_char_type, ucs);
    }
  else if (moid == M_ROW_CHAR)
    return a68_lower_string_denotation (p, ctx);
  else if (moid == M_INT
	   || moid == M_LONG_INT
	   || moid == M_LONG_LONG_INT
	   || moid == M_SHORT_INT
	   || moid == M_SHORT_SHORT_INT)
    {
      /* SIZETY INT */
      tree type;
      char *end;
      NODE_T *s = NO_NODE;
      if (IS (SUB (p), LONGETY) || IS (SUB (p), SHORTETY))
	s = NEXT (SUB (p));
      else
	s = SUB (p);

      type = CTYPE (moid);
      int64_t val = strtol (NSYMBOL (s), &end, 10);
      gcc_assert (end[0] == '\0');
      return build_int_cst (type, val);
    }
  if (moid == M_BITS
      || moid == M_LONG_BITS
      || moid == M_LONG_LONG_BITS
      || moid == M_SHORT_BITS
      || moid == M_SHORT_SHORT_BITS)
    {
      /* SIZETY BITS */

      tree type;
      char *end;
      NODE_T *s = NO_NODE;
      if (IS (SUB (p), LONGETY) || IS (SUB (p), SHORTETY))
	s = NEXT (SUB (p));
      else
	s = SUB (p);

      type = CTYPE (moid);
      int64_t radix = strtol (NSYMBOL (s), &end, 10);
      gcc_assert (end != NSYMBOL (s) && *end == 'r');
      end++;
      int64_t val = strtol (end, &end, radix);
      gcc_assert (end[0] == '\0');
      return build_int_cst (type, val);
    }
  else if (moid == M_REAL
	   || moid == M_LONG_REAL
	   || moid == M_LONG_LONG_REAL)
    {
      /* SIZETY INT */
      tree type;
      NODE_T *s = NO_NODE;
      if (IS (SUB (p), LONGETY) || IS (SUB (p), SHORTETY))
	s = NEXT (SUB (p));
      else
	s = SUB (p);

      if (moid == M_REAL)
	type = float_type_node;
      else if (moid == M_LONG_REAL)
	type = double_type_node;
      else if (moid == M_LONG_LONG_REAL)
	type = long_double_type_node;
      else
	gcc_unreachable ();

      REAL_VALUE_TYPE val;
      real_from_string (&val, NSYMBOL (s));
      return build_real (type, val);
    }

  gcc_unreachable ();
  return NULL_TREE;
}

/* Lower SKIP.

     skip
*/

tree
a68_lower_skip (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_get_skip_tree (MOID (p));
}

/* Lower NIHIL.

     nihil : nil.

   NIL stands for a name referring to no value and which must be
   distinguishable from any other name.  It is of mode REF AMODE.  NIL is never
   subject to coercion and it may only occur where the context is strong,
   i.e. where AMODE is known at compile-time.

   It lowers to a pointer to AMODE with value 0.  */

tree
a68_lower_nihil (NODE_T *p, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  tree type = CTYPE (MOID (p));

  gcc_assert (type == a68_void_type || POINTER_TYPE_P (type));
  if (type == a68_void_type)
    return a68_lower_empty (p, ctx);
  else
    return build_int_cst (type, 0);
}

/* Lower EMPTY.  */

tree
a68_lower_empty (NODE_T *p ATTRIBUTE_UNUSED, LOW_CTX_T ctx ATTRIBUTE_UNUSED)
{
  return a68_get_empty ();
}

/* Lower an identity relation.

     identity relation : tertiary, is symbol, tertiary;
     			 tertiary, isnt symbol, tertiary.

   An identity relation determines whether two name values are the same.  It
   lowers into EQ_EXPR in case of IS and into NE_EXPR in case of ISNT.  */

tree
a68_lower_identity_relation (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *lhs = SUB (p);
  NODE_T *oper = NEXT (lhs);
  NODE_T *rhs = NEXT (oper);

  /* Consolidate arguments to make sure we are comparing pointers in the
     r-value context of the EQ_EXPR or NE_EXPR operation below.  */
  tree op1 = a68_consolidate_ref (MOID (lhs), a68_lower_tree (lhs, ctx));
  tree op2 = a68_consolidate_ref (MOID (rhs), a68_lower_tree (rhs, ctx));

  tree_code code;
  if (IS (oper, IS_SYMBOL))
    code = EQ_EXPR;
  else if (IS (oper, ISNT_SYMBOL))
    code = NE_EXPR;
  else
    gcc_unreachable ();

  return fold_build2_loc (a68_get_node_location (p),
			  code, boolean_type_node, op1, op2);
}

/* Lower AND_FUNCTION and OR_FUNCTION.

     and function : tertiary, andf symbol, tertiary.
     or function : tertiary, orf_symbol, tertiary.

   These are pseudo-operators that are used to implement short-circuits
   evaluation of logical expressions.

   These pseudo-operators lower into TRUTH_ANDIF_EXPR or TRUTH_ORIF_EXPR,
   respectively.  */

tree
a68_lower_logic_function (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *lhs = SUB (p);
  NODE_T *oper = NEXT (lhs);
  NODE_T *rhs = NEXT (oper);

  tree op1 = a68_lower_tree (lhs, ctx);
  tree op2 = a68_lower_tree (rhs, ctx);

  tree_code code;
  if (IS (oper, ANDF_SYMBOL))
    code = TRUTH_ANDIF_EXPR;
  else if (IS (oper, ORF_SYMBOL))
    code = TRUTH_ORIF_EXPR;
  else
    gcc_unreachable ();

  return fold_build2_loc (a68_get_node_location (p),
			  code, boolean_type_node, op1, op2);
}

/* Lower a primary.

     primary : identifier; denotation; cast; enclosed clause; format text.

   The primary lowers into some GENERIC expression.  */

tree
a68_lower_primary (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_lower_tree (SUB (p), ctx);
}

/* Lower a cast.

     cast : declarer, enclosed clause;
            void symbol, enclosed clause.

   A cast establishes a strong context with some required mode.  This context
   allows coercions to be applied, and these coercions have been inserted in
   the parse tree by the parser.  */

tree
a68_lower_cast (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_lower_tree (NEXT (SUB (p)), ctx);
}

/* Lower a slice.

     slice : MULTIPLE INDEXER

   Slicing a multiple may result in either an element of the multiple, if the
   operation is indexing, or another multiple, if the operation is a
   trimming.  */

static void
lower_subscript_for_indexes (NODE_T *p, LOW_CTX_T ctx)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      switch (ATTRIBUTE (p))
	{
	case TRIMMER:
	  /* Because of ANNOTATION (indexer) == SLICE */
	  gcc_unreachable ();
	  break;
	case UNIT:
	  a68_add_stmt (a68_lower_tree (p, ctx));
	  break;
	case GENERIC_ARGUMENT:
	case GENERIC_ARGUMENT_LIST:
	  lower_subscript_for_indexes (SUB (p), ctx);
	  break;
	default:
	  break;
	}
    }
}

static void
lower_subscript_for_trimmers (NODE_T *p, LOW_CTX_T ctx,
			      tree multiple, tree new_multiple,
			      int *dim, int *new_dim,
			      tree elements_pointer_type)
{
  /* new.elements := multiple.elements;
     FOR dim TO num dimensions
     DO CO t[dim] is either a subscript i or a trimmer i : j @ k CO
        new.elements +:= i * multiple.strides[dim];
        IF t[dim] is a trimmer
        THEN INT d := ( k is absent | 1 | multiple.lb[dim] - k );
             new.lb[dim] := multiple.lb[dim] - d;
	     new.ub[dim] := multiple.ub[dim] - d;
	     new.strides[dim] := multiple.strides[dim]
        FI
     OD
  */

  for (; p != NO_NODE; FORWARD (p))
    {
      switch (ATTRIBUTE (p))
	{
	case UNIT:
	  {
	    tree unit = save_expr (fold_convert (ssizetype, a68_lower_tree (p, ctx)));
	    tree new_elements = a68_multiple_elements (new_multiple);
	    tree size_dim = size_int (*dim);
	    tree dim_lower_bound = save_expr (a68_multiple_lower_bound (multiple, size_dim));
	    tree stride = save_expr (a68_multiple_stride (multiple, size_dim));

	    /* Validate bounds.  */
	    if (OPTION_BOUNDS_CHECKING (&A68_JOB))
	      a68_add_stmt (a68_multiple_bounds_check (p, size_dim, multiple, unit));

	    /* new_elements += i * strides[dim] */
	    tree offset = fold_build2 (MULT_EXPR, sizetype,
				       fold_convert (sizetype, fold_build2 (MINUS_EXPR, ssizetype,
									    unit, dim_lower_bound)),
				       stride);

	    offset = save_expr (offset);
	    new_elements = fold_build2 (POINTER_PLUS_EXPR,
					elements_pointer_type,
					new_elements,
					offset);
	    a68_add_stmt (a68_multiple_set_elements (new_multiple, new_elements));

	    /* elements_size -= i * strides[dim] */
	    tree elements_size = a68_multiple_elements_size (new_multiple);
	    elements_size = fold_build2 (MINUS_EXPR, sizetype,
					 elements_size, offset);
	    a68_add_stmt (a68_multiple_set_elements_size (new_multiple, elements_size));

	    *dim += 1;
	    break;
	  }
	case TRIMMER:
	  {
	    /* First collect components from the trimmer.  */
	    tree size_dim = size_int (*dim);
	    tree dim_lower_bound = save_expr (a68_multiple_lower_bound (multiple, size_dim));
	    tree lower_bound = dim_lower_bound;
	    tree upper_bound = save_expr (a68_multiple_upper_bound (multiple, size_dim));
	    tree at = ssize_int (1);

	    NODE_T *q = SUB (p);
	    if (q != NO_NODE)
	      {
		if (IS (q, AT_SYMBOL))
		  {
		    /* Both bounds are implicit.  */
		    at = save_expr (fold_convert (ssizetype, a68_lower_tree (NEXT (q), ctx)));
		  }
		else if (IS (q, COLON_SYMBOL))
		  {
		    /* Lower bound is implicit.  */
		    FORWARD (q);
		    if (IS (q, AT_SYMBOL))
		      {
			/* Upper bound is implicit, AT specified.  */
			gcc_assert (IS (q, AT_SYMBOL));
			at = save_expr (fold_convert (ssizetype, a68_lower_tree (NEXT (q), ctx)));
		      }
		    else
		      {
			upper_bound
			  = save_expr (fold_convert (ssizetype, a68_lower_tree (q, ctx)));
			FORWARD (q);
			if (q != NO_NODE)
			  {
			    gcc_assert (IS (q, AT_SYMBOL));
			    at = save_expr (fold_convert (ssizetype, a68_lower_tree (NEXT (q), ctx)));
			  }
		      }
		  }
		else
		  {
		    /* Lower bound is explicit.  */
		    lower_bound = fold_convert (ssizetype, a68_lower_tree (q, ctx));
		    FORWARD (q);
		    gcc_assert (IS (q, COLON_SYMBOL));
		    FORWARD (q);
		    if (q != NO_NODE)
		      {
			if (IS (q, AT_SYMBOL))
			  at = save_expr (fold_convert (ssizetype, a68_lower_tree (NEXT (q), ctx)));
			else
			  {
			    upper_bound
			      = save_expr (fold_convert (ssizetype, a68_lower_tree (q, ctx)));
			    FORWARD (q);
			    if (q != NO_NODE && IS (q, AT_SYMBOL))
			      at =
				save_expr (fold_convert (ssizetype, a68_lower_tree (NEXT (q), ctx)));
			  }
		      }
		  }
	      }

	    /* Time for some bounds checking.

	       Note that in trimmers, given the current dimension's bounds
	       (L,U), we cannot simply do the check:

	            L <= lower_bound <= U
		    L <= upper_bound <= U

	       This is because the multiple may be flat, and the dimension may
	       have bounds such like U < L.  In that case, the expressions
	       above would always eval to false for any lower_bound and
	       upper_bound.

	       So we check for this instead:

	            L <= lower_bound AND upper_bound <= U

               This allows to trim a "flat dimension" using a trimmer where
	       upper_bound < lower_bound.  The result is, of course, another
	       "flat dimension" in the multiple result of the trimming.  */

	    if (OPTION_BOUNDS_CHECKING (&A68_JOB))
	      {
		a68_add_stmt (a68_multiple_single_bound_check (p, size_dim, multiple,
							       lower_bound,
							       false /* upper_bound */));
		a68_add_stmt (a68_multiple_single_bound_check (p, size_dim, multiple,
							       upper_bound,
							       true /* upper_bound */));
	      }

	    /* new_elements += i * strides[dim] */
	    tree stride = save_expr (a68_multiple_stride (multiple, size_dim));
	    tree new_elements = a68_multiple_elements (new_multiple);
	    tree offset = fold_build2 (MULT_EXPR, sizetype,
				       fold_convert (sizetype, fold_build2 (MINUS_EXPR, ssizetype,
									    lower_bound, dim_lower_bound)),
				       stride);

	    offset = save_expr (offset);
	    new_elements = fold_build2 (POINTER_PLUS_EXPR,
					elements_pointer_type,
					new_elements,
					offset);
	    a68_add_stmt (a68_multiple_set_elements (new_multiple, new_elements));

	    /* elements_size -= i * strides[dim] */
	    tree elements_size = a68_multiple_elements_size (new_multiple);
	    elements_size = fold_build2 (MINUS_EXPR, sizetype,
					 elements_size, offset);
	    a68_add_stmt (a68_multiple_set_elements_size (new_multiple,
							  elements_size));

	    /* Fill the triplet for this dimension in new_multiple.  */
	    tree size_new_dim = size_int (*new_dim);
	    tree d = fold_build2 (MINUS_EXPR, ssizetype, lower_bound, at);

	    a68_add_stmt (a68_multiple_set_lower_bound (new_multiple, size_new_dim,
							fold_build2 (MINUS_EXPR, ssizetype,
								     lower_bound, d)));
	    a68_add_stmt (a68_multiple_set_upper_bound (new_multiple, size_new_dim,
							fold_build2 (MINUS_EXPR, ssizetype,
								     upper_bound, d)));
	    a68_add_stmt (a68_multiple_set_stride (new_multiple, size_new_dim, stride));

	    *new_dim += 1;
	    *dim += 1;
	    break;
	  }
	default:
	  lower_subscript_for_trimmers (SUB (p), ctx,
					multiple, new_multiple,
					dim, new_dim,
					elements_pointer_type);
	}
    }
}

tree
a68_lower_slice (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *indexer = NEXT_SUB (p);
  MOID_T *orig_multiple_mode = MOID (SUB (p));
  MOID_T *multiple_mode = orig_multiple_mode;
  bool slicing_name = false;

  /* First of all, lower the multiple being sliced.  If it is a name to a
     multiple, set a flag and dereference.  */
  tree multiple = a68_lower_tree (SUB (p), ctx);
  MOID_T *orig_sliced_multiple_mode = MOID (p);
  MOID_T *sliced_multiple_mode = MOID (p);
  size_t slice_num_dimensions = 0;
  if (IS_REF (MOID (SUB (p))))
    {
      slicing_name = true;
      multiple = a68_low_deref (multiple, SUB (p));
      multiple_mode = SUB (multiple_mode);
      slice_num_dimensions = DIM (SUB (MOID (p)));
      sliced_multiple_mode = SUB (sliced_multiple_mode);
    }
  else
    slice_num_dimensions = DIM (MOID (p));

  tree slice = NULL_TREE;
  if (ANNOTATION (indexer) == SLICE)
    {
      /* The slice has only indexers and no trimmers.  Collect units and slice
	 an element of the multiple using a68_multiple_slice.  This operation
	 results in an element of the multiple.  */

      /* Collect units  */
      a68_push_range (NULL);
      lower_subscript_for_indexes (SUB (indexer), ctx);
      tree units = a68_pop_range ();

      /* We need to allocate space for as many indexes as dimensions of the
	 multiple.  */
      tree num_dimensions_tree = a68_multiple_dimensions (multiple);
      gcc_assert (TREE_CODE (num_dimensions_tree) == INTEGER_CST);
      int num_dimensions = tree_to_shwi (num_dimensions_tree);

      int num_indexes = 0;
      tree *indexes = (tree *) xmalloc (sizeof (tree) * num_dimensions);
      for (tree_stmt_iterator si = tsi_start (units);
	   !tsi_end_p (si);
	   tsi_next (&si))
	{
	  /* Add the unit to the list of indexes.  */
	  indexes[num_indexes] = tsi_stmt (si);
	  num_indexes++;
	}
      gcc_assert (num_indexes == num_dimensions);

      /* Slice.  */
      slice = a68_multiple_slice (p, multiple, slicing_name,
				  num_indexes, indexes);
      free (indexes);
    }
  else if (ANNOTATION (indexer) == TRIMMER)
    {
      /* The slice has both indexers and trimmers.  Traverse the indexer
	 subtree to obtain the descriptor of the trimmed multiple (which is
	 another multiple) and the pointer to the elements, which points to
	 some position within the elements of the trimmed multiple.  This
	 operation results in a new multiple of the same mode than the trimmed
	 multiple with shared elements.  */

      a68_push_range (sliced_multiple_mode);

      tree sliced_multiple = a68_lower_tmpvar ("multiple%", TREE_TYPE (multiple),
					       multiple);
      tree *lower_bounds = (tree *) xmalloc (sizeof (tree) * slice_num_dimensions);
      tree *upper_bounds = (tree *) xmalloc (sizeof (tree) * slice_num_dimensions);
      tree ssize_one_node = fold_convert (ssizetype, size_one_node);
      tree ssize_zero_node = fold_convert (ssizetype, size_zero_node);
      for (size_t d = 0; d < slice_num_dimensions; ++d)
	{
	  /* Note that these dummy bounds and the implied strides will be
	     overwritten by lower_subscript_for_trimmers below.  */
	  lower_bounds[d] = ssize_one_node;
	  upper_bounds[d] = ssize_zero_node;
	}
      tree new_multiple = a68_row_value (CTYPE (sliced_multiple_mode),
					 slice_num_dimensions,
					 a68_multiple_elements (sliced_multiple),
					 a68_multiple_elements_size (sliced_multiple),
					 lower_bounds, upper_bounds);
      new_multiple = save_expr (new_multiple);
      new_multiple = a68_lower_tmpvar ("new_multiple%", TREE_TYPE (new_multiple),
				       new_multiple);

      int dim = 0;
      int new_dim = 0;
      lower_subscript_for_trimmers (SUB (indexer), ctx,
				    sliced_multiple, new_multiple,
				    &dim, &new_dim,
				    a68_row_elements_pointer_type (TREE_TYPE (multiple)));
      a68_add_stmt (new_multiple);
      slice = a68_pop_range ();

      /* In case we are slicing a ref to a multiple, return the address of the
	 resulting multiple and not the multiple itself.  But in this case we
	 need an address in the heap, because the trimmed multiple may be in
	 the heap and the result shall have the same scope.  */
      if (slicing_name)
	{
	  tree ptrtype = CTYPE (orig_sliced_multiple_mode);
	  tree slice_addr = fold_build1 (ADDR_EXPR, ptrtype, slice);
	  tree alloc = a68_lower_malloc (ptrtype, size_in_bytes (TREE_TYPE (slice)));
	  alloc = save_expr (alloc);
	  tree copy = a68_lower_memcpy (alloc, slice_addr, size_in_bytes (TREE_TYPE (slice)));

	  slice = fold_build2 (COMPOUND_EXPR, ptrtype, copy, alloc);
	}
    }
  else
    gcc_unreachable ();

  return slice;
}

/* Lower a selection.

     selection : selector, secondary.
     selector : field identifier, of symbol.

   The selection lowers into a COMPONENT_REF of the field corresponding to the
   field identifier.  */

tree
a68_lower_selection (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *secondary = NEXT (SUB (p));
  NODE_T *field_identifier = SUB (SUB (p));

  MOID_T *secondary_mode = MOID (secondary);
  tree secondary_expr = a68_lower_tree (secondary, ctx);

  tree res = NULL_TREE;

  /* If the secondary is an address, we need to indirect.  */
  if (IS_REF (secondary_mode))
    {
      secondary_expr = a68_low_deref (secondary_expr, secondary);
      secondary_mode = SUB (secondary_mode);
    }

  if (IS_FLEX (secondary_mode) || IS_ROW (secondary_mode))
    {
      /* This is the selection of a multiple of structs.

	 The result is a multiple with same dimensions, dimension bounds and
	 strides than the indexed multiple.  The elements pointer is made to
	 point to the selected field of the first struct.  */

      MOID_T *result_mode = MOID (p);
      if (IS_REF (result_mode))
	result_mode = SUB (result_mode);
      MOID_T *struct_mode = SUB (secondary_mode);
      tree field_id = a68_get_mangled_identifier (SYMBOL (INFO (field_identifier)));
      tree struct_type = CTYPE (struct_mode);
      a68_push_range (result_mode);
      tree selection = a68_lower_tmpvar ("selection%", CTYPE (result_mode),
					 a68_get_skip_tree (result_mode));
      tree multiple = a68_lower_tmpvar ("multiple%", TREE_TYPE (secondary_expr),
					secondary_expr);

      /* First set the bounds of the selection, which are exactly the same
	 bounds than the selected multiple.  */
      for (int dim = 0; dim < DIM (DEFLEX (secondary_mode)); ++dim)
	{
	  tree size_dim = size_int (dim);
	  tree lower_bound = a68_multiple_lower_bound (multiple, size_dim);
	  tree upper_bound = a68_multiple_upper_bound (multiple, size_dim);
	  tree stride = a68_multiple_stride (multiple, size_dim);
	  a68_add_stmt (a68_multiple_set_lower_bound (selection, size_dim,
						      lower_bound));
	  a68_add_stmt (a68_multiple_set_upper_bound (selection, size_dim,
						      upper_bound));
	  a68_add_stmt (a68_multiple_set_stride (selection, size_dim,
						 stride));
	}

      /* Now set the elements pointer, which is the elements pointer of the
	 selected multiple offset the offset of the selected field in its
	 struct type.  */
      tree elements = a68_multiple_elements (selection);
      tree multiple_elements = a68_multiple_elements (multiple);
      tree multiple_elements_size = a68_multiple_elements_size (multiple);
      tree element_pointer_type = TREE_TYPE (elements);
      tree field_offset = NULL_TREE;
      for (tree f = TYPE_FIELDS (struct_type); f; f = DECL_CHAIN (f))
	{
	  if (field_id == DECL_NAME (f))
	    {
	      field_offset = byte_position (f);
	      break;
	    }
	}
      gcc_assert (field_offset != NULL_TREE);
      a68_add_stmt (a68_multiple_set_elements (selection,
					       fold_build2 (POINTER_PLUS_EXPR,
							    element_pointer_type,
							    multiple_elements,
							    field_offset)));

      /* The size of the buffer pointed by the elements pointer has to be
	 adjusted accordingly.  */
      a68_add_stmt (a68_multiple_set_elements_size (selection,
						    fold_build2 (MINUS_EXPR, sizetype,
								 multiple_elements_size,
								 field_offset)));

      a68_add_stmt (selection);
      res = a68_pop_range ();
    }
  else
    {
      /* This is the selection of a struct field.  */
      gcc_assert (A68_STRUCT_TYPE_P (TREE_TYPE (secondary_expr)));

      /* Search for the selected field in the struct type.  */
      tree struct_type = TREE_TYPE (secondary_expr);
      tree field_id = a68_get_mangled_identifier (SYMBOL (INFO (field_identifier)));
      tree field = NULL_TREE;
      for (tree f = TYPE_FIELDS (struct_type); f; f = DECL_CHAIN (f))
	{
	  if (field_id == DECL_NAME (f))
	    {
	      field = f;
	      break;
	    }
	}
      gcc_assert (field != NULL_TREE);

      /* Emit the COMPONENT_REF.  */
      res = fold_build3_loc (a68_get_node_location (p),
			     COMPONENT_REF,
			     TREE_TYPE (field),
			     secondary_expr,
			     field,
			     NULL_TREE);
    }

  /* The selection of a name yields a name.  */
  if (IS_REF (MOID (secondary)))
    /* XXX This may require copying.  */
    return fold_build1 (ADDR_EXPR, CTYPE (MOID (p)), res);
  else
    return res;
}

/* Lower a secondary.

     secondary : primary; generator; selection.

   The secondary lowers into some GENERIC expression.  */

tree
a68_lower_secondary (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_lower_tree (SUB (p), ctx);
}

/* Lower a formula.

     formula : secondary, operator, secondary;
     	       secondary, operator, monadic formula;
	       secondary, operator, formula;
	       monadic formula;
	       monadic formula, operator, secondary;
	       monadic formula, operator, monadic formula;
	       monadic formula, operator, formula;
	       formula, operator, secondary;
	       formula, operator, monadic formula;
	       formula, operator, formula.

   The formula lowers into some GENERIC expression.  */

tree
a68_lower_formula (NODE_T *p, LOW_CTX_T ctx)
{
  if (IS (SUB (p), MONADIC_FORMULA) && NEXT (SUB (p)) == NO_NODE)
    return a68_lower_tree (SUB (p), ctx);
  else
    {
      /* If the operator is defined in the standard prelude, then use its lowering
	 code.  */
      if (TAG_TABLE (TAX (NEXT (SUB (p)))) == A68_STANDENV)
	{
	  LOWERER_T lowerer = LOWERER (TAX (NEXT (SUB (p))));
	  return (*lowerer) (p, ctx);
	}
      else
	{
	  tree arg1 = a68_lower_tree (SUB (p), ctx);
	  tree op = a68_lower_tree (NEXT (SUB (p)), ctx);
	  tree arg2 = a68_lower_tree (NEXT (NEXT (SUB (p))), ctx);

	  if (POINTER_TYPE_P (TREE_TYPE (op)))
	    op = fold_build1 (INDIRECT_REF,
			      TREE_TYPE (TREE_TYPE (op)),
			      op);
	  return build_call_expr_loc (a68_get_node_location (p), op, 2, arg1, arg2);
	}
    }
}

/* Lower a monadic formula.

     monadic formula : operator, secondary;
                       operator, monadic formula.

   The monadic formula lowers into some GENERIC expression.  */

tree
a68_lower_monadic_formula (NODE_T *p, LOW_CTX_T ctx)
{
  /* If the operator is defined in the standard prelude, then use its lowering
     code.  */
  if (TAG_TABLE (TAX (SUB (p))) == A68_STANDENV)
    {
      LOWERER_T lowerer = LOWERER (TAX (SUB (p)));
      return (*lowerer) (p, ctx);
    }
  else
    {
      tree op = a68_lower_tree (SUB (p), ctx);
      tree secondary = a68_lower_tree (NEXT (SUB (p)), ctx);

      if (POINTER_TYPE_P (TREE_TYPE (op)))
	op = fold_build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (op)), op);
      return build_call_expr_loc (a68_get_node_location (p), op, 1, secondary);
    }
}

/* Lower a tertiary.

     tertiary : nihil; monadic formula; formula; secondary.

  The tertiary lowers to some GENERIC expression.  */

tree
a68_lower_tertiary (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_lower_tree (SUB (p), ctx);
}

/* Lower an assignation.

     assignation : tertiary, assign symbol, tertiary;
                   tertiary, assign symbol, identity relation;
		   tertiary, assign symbol, and function;
		   tertiary, assign symbol, or function;
		   tertiary, assign symbol, routine text;
		   tertiary, assign symbol, jump;
		   tertiary, assign symbol, skip;
		   tertiary, assign symbol, assignation;
		   tertiary, assign symbol, code clause.

   An assignation lowers into appending a MODIFY_EXPR to the statements list,
   and the result of the expression is the left hand side.  A compound
   expression fits perfectly */

tree
a68_lower_assignation (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *lhs_node = SUB (p);
  NODE_T *rhs_node = NEXT (NEXT (SUB (p)));
  tree lhs = a68_lower_tree (lhs_node, ctx);
  tree rhs = a68_lower_tree (rhs_node, ctx);

  return a68_low_assignation (p,
			      lhs, MOID (lhs_node),
			      rhs, MOID (rhs_node));
}

/* Lower a generator.

     generator : loc symbol, declarer;
                 heap symbol, declarer;
		 new symbol, declarer.

   LOC generators lower into calls to BUILT_IN_ALLOCA.
   HEAP generators lower into calls to malloc.  */

tree
a68_lower_generator (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *declarer = NEXT (SUB (p));
  return a68_low_generator (declarer,
			    MOID (declarer),
			    !IS (SUB (p), LOC_SYMBOL),
			    ctx);
}

/* Lower a procedure call.

 */

static void
collect_call_arguments (NODE_T *p, vec<tree, va_gc> *args, LOW_CTX_T ctx)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, UNIT))
	{
	  /* In Algol 68 parameters are passed via an identity declaration, so
	     this must implement same semantics.  */
	  tree arg = a68_lower_tree (p, ctx);
	  if (HAS_ROWS (MOID (p)))
	    arg = a68_low_dup (arg);
	  arg = a68_consolidate_ref (MOID (p), arg);
	  args->quick_push (arg);
	}
      else
	collect_call_arguments (SUB (p), args, ctx);
    }
}

tree
a68_lower_call (NODE_T *p, LOW_CTX_T ctx)
{
  MOID_T *proc_mode = MOID (SUB (p));
  MOID_T *ret_mode = SUB (proc_mode);
  unsigned int nargs = DIM (proc_mode);

  /* Collect arguments.  */
  vec<tree, va_gc> *args;
  vec_alloc (args, nargs);
  collect_call_arguments (NEXT (SUB (p)), args, ctx);

  /* Lower the primary to call.  */
  tree primary = a68_lower_tree (SUB (p), ctx);

  /* We need a pointer to a function type.  */
  if (!POINTER_TYPE_P (TREE_TYPE (primary)))
    primary = fold_build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (primary)),
			   primary);

  /* Build a function call.  */
  tree call = build_call_vec (CTYPE (ret_mode), primary, args);
  SET_EXPR_LOCATION (call, a68_get_node_location (p));
  return call;
}

/* Lower a routine text.

     routine text : parameter pack, (declarer ; void symbol), colon symbol, assignation;
                    parameter pack, (declarer ; void symbol), colon symbol, identity relation;
		    parameter pack, (declarer ; void symbol), colon symbol, and function;
		    parameter pack, (declarer ; void symbol), colon symbol, or runction;
		    parameter pack, (declarer ; void symbol), colon symbol, jump;
		    parameter pack, (declarer ; void symbol), colon symbol, skip;
		    parameter pack, (declarer ; void symbol), colon symbol, tertiary;
		    parameter pack, (declarer ; void symbol), colon symbol, routine text;
		    parameter pack, (declarer ; void symboL), colon symbol, code clause;
                    (declarer ; void symbol), colon symbol, assignation;
                    (declarer ; void symbol), colon symbol, identity relation;
		    (declarer ; void symbol), colon symbol, and function;
		    (declarer ; void symbol), colon symbol, or runction;
		    (declarer ; void symbol), colon symbol, jump;
		    (declarer ; void symbol), colon symbol, skip;
		    (declarer ; void symbol), colon symbol, tertiary;
		    (declarer ; void symbol), colon symbol, routine text;
		    (declarer ; void symbol), colon symbol, code clause.

  Routine texts are used to create routines.  They can stand as the actual
  parameter of an identity declaration, as the actual parameter of a call, or
  as the right-hand side of an assignation.

  This lowering function is called in two different contexts:

  1) As part of a routine-identity-declaration, in which case the routine
     resulting from this routine-text is beign ascribed to an identifier given
     in ctx.proc_decl_identifier.  In that case, we lower to a FUNC_DECL
     initialized with the body of the routine-text.

  2) As a free standing routine-text.  In that case ctx.proc_decl_identifier is
     NO_NODE. We lower to the address of a FUNC_DECL that features some unique
     name.  This pointer will then likely be assigned or ascribed to some
     variable or identifier in non-contracted identity declaration, but we
     cannot assume that so we have to opt for the indirection.  */

tree
a68_lower_routine_text (NODE_T *p, LOW_CTX_T ctx)
{
  NODE_T *s = SUB (p);

  tree func_decl = NULL_TREE;
  NODE_T *defining_identifier = ctx.proc_decl_identifier;
  bool defining_operator = ctx.proc_decl_operator;
  if (defining_identifier != NO_NODE)
    {
      /* The routine-text is part of a routine-identity-declaration.  */
      func_decl = TAX_TREE_DECL (TAX (defining_identifier));
      if (func_decl == NULL_TREE)
	{
	  func_decl
	    = a68_make_proc_identity_declaration_decl (defining_identifier,
						       ctx.module_definition_name,
						       defining_operator /* indicant */);
	  TAX_TREE_DECL (TAX (defining_identifier)) = func_decl;
	}

      /* If the routine-identity-declaration is in a public range then add the
	 declaration to the publicized declarations list.  Otherwise chain the
	 declaration in the proper block and bind it.  */
      if (PUBLIC_RANGE (TABLE (TAX (defining_identifier))))
	vec_safe_push (A68_MODULE_DEFINITION_DECLS, func_decl);
      else
	a68_add_decl (func_decl);
    }
  else
    {
      /* The routine-text is free standing.  */
      func_decl = a68_make_anonymous_routine_decl (MOID (p));
      a68_add_decl (func_decl);
    }

  a68_add_decl_expr (fold_build1_loc (a68_get_node_location (p),
				      DECL_EXPR,
				      TREE_TYPE (func_decl),
				      func_decl));
  announce_function (func_decl);

  /* PARAMETER_PACK.  */
  NODE_T *parameter_pack_node = NO_NODE;
  tree parameter_pack = NULL_TREE; /* This is computed below.  */
  if (IS (s, PARAMETER_PACK))
    {
      parameter_pack_node = s;
      FORWARD (s);
    }

  /* DECLARER or VOID_SYMBOL */
  if (IS (s, DECLARER) || IS (s, VOID_SYMBOL))
    /* This is not used, as this formal declarer is also available in the
       procedure mode.  So just skip it.  */
    FORWARD (s);

  /* Skip the COLON_SYMBOL.  */
  gcc_assert (IS (s, COLON_SYMBOL));
  FORWARD (s);

  /* Lower the function body.

     This should be done in a new range in which the formal parameters of the
     routine-text have been declared.  */
  a68_push_function_range (func_decl, CTYPE (SUB (MOID (p))) /* result_type */);
  if (parameter_pack_node != NO_NODE)
    parameter_pack = a68_lower_tree (parameter_pack_node, ctx);
  DECL_ARGUMENTS (func_decl) = parameter_pack;
  ctx.proc_decl_identifier = NO_NODE;
  tree func_body = a68_lower_tree (s, ctx);
  a68_pop_function_range (func_body);

  if (defining_identifier != NO_NODE)
    /* Routine-text immediately ascribed to some identifier in a
       proc-identity-declaration.  Return the FUNC_DECL.  */
    return func_decl;
  else
    /* Free standing routine-text.  Return its address.  */
    return fold_build1 (ADDR_EXPR,
			build_pointer_type (TREE_TYPE (func_decl)),
			func_decl);
}

/* Lower an unit.

      unit : assignation; identity relation;
             and function; or function; routine text;
             jump; skip; tertiary; assertion; code clause.

   The unit lowers to an expression.  */

tree
a68_lower_unit (NODE_T *p, LOW_CTX_T ctx)
{
  return a68_lower_tree (SUB (p), ctx);
}
