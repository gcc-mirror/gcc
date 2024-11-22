/* Code translation -- generate GCC trees from gfc_code.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
#include "options.h"
#include "tree.h"
#include "gfortran.h"
#include "gimple-expr.h"	/* For create_tmp_var_raw.  */
#include "trans.h"
#include "stringpool.h"
#include "fold-const.h"
#include "tree-iterator.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"
#include "trans-const.h"

/* Naming convention for backend interface code:

   gfc_trans_*	translate gfc_code into STMT trees.

   gfc_conv_*	expression conversion

   gfc_get_*	get a backend tree representation of a decl or type  */

const char gfc_msg_fault[] = N_("Array reference out of bounds");


/* Advance along TREE_CHAIN n times.  */

tree
gfc_advance_chain (tree t, int n)
{
  for (; n > 0; n--)
    {
      gcc_assert (t != NULL_TREE);
      t = DECL_CHAIN (t);
    }
  return t;
}

void
gfc_locus_from_location (locus *where, location_t loc)
{
  where->nextc = (gfc_char_t *) -1;
  where->u.location = loc;
}


static int num_var;

#define MAX_PREFIX_LEN 20

static tree
create_var_debug_raw (tree type, const char *prefix)
{
  /* Space for prefix + "_" + 10-digit-number + \0.  */
  char name_buf[MAX_PREFIX_LEN + 1 + 10 + 1];
  tree t;
  int i;

  if (prefix == NULL)
    prefix = "gfc";
  else
    gcc_assert (strlen (prefix) <= MAX_PREFIX_LEN);

  for (i = 0; prefix[i] != 0; i++)
    name_buf[i] = gfc_wide_toupper (prefix[i]);

  snprintf (name_buf + i, sizeof (name_buf) - i, "_%d", num_var++);

  t = build_decl (input_location, VAR_DECL, get_identifier (name_buf), type);

  /* Not setting this causes some regressions.  */
  DECL_ARTIFICIAL (t) = 1;

  /* We want debug info for it.  */
  DECL_IGNORED_P (t) = 0;
  /* It should not be nameless.  */
  DECL_NAMELESS (t) = 0;

  /* Make the variable writable.  */
  TREE_READONLY (t) = 0;

  DECL_EXTERNAL (t) = 0;
  TREE_STATIC (t) = 0;
  TREE_USED (t) = 1;

  return t;
}

/* Creates a variable declaration with a given TYPE.  */

tree
gfc_create_var_np (tree type, const char *prefix)
{
  tree t;

  if (flag_debug_aux_vars)
    return create_var_debug_raw (type, prefix);

  t = create_tmp_var_raw (type, prefix);

  /* No warnings for anonymous variables.  */
  if (prefix == NULL)
    suppress_warning (t);

  return t;
}


/* Like above, but also adds it to the current scope.  */

tree
gfc_create_var (tree type, const char *prefix)
{
  tree tmp;

  tmp = gfc_create_var_np (type, prefix);

  pushdecl (tmp);

  return tmp;
}


/* If the expression is not constant, evaluate it now.  We assign the
   result of the expression to an artificially created variable VAR, and
   return a pointer to the VAR_DECL node for this variable.  */

tree
gfc_evaluate_now_loc (location_t loc, tree expr, stmtblock_t * pblock)
{
  tree var;

  if (CONSTANT_CLASS_P (expr))
    return expr;

  var = gfc_create_var (TREE_TYPE (expr), NULL);
  gfc_add_modify_loc (loc, pblock, var, expr);

  return var;
}


tree
gfc_evaluate_now (tree expr, stmtblock_t * pblock)
{
  return gfc_evaluate_now_loc (input_location, expr, pblock);
}


/* Returns a fresh pointer variable pointing to the same data as EXPR, adding
   in BLOCK the initialization code that makes it point to EXPR.  */

tree
gfc_evaluate_data_ref_now (tree expr, stmtblock_t *block)
{
  tree t = expr;

  STRIP_NOPS (t);

  /* If EXPR can be used as lhs of an assignment, we have to take the address
     of EXPR.  Otherwise, reassigning the pointer would retarget it to some
     other data without EXPR being retargetted as well.  */
  bool lvalue_p = DECL_P (t) || REFERENCE_CLASS_P (t) || INDIRECT_REF_P (t);

  tree value;
  if (lvalue_p)
    {
      value = gfc_build_addr_expr (NULL_TREE, expr);
      value = gfc_evaluate_now (value, block);
      return build_fold_indirect_ref_loc (input_location, value);
    }
  else
    return gfc_evaluate_now (expr, block);
}


/* Like gfc_evaluate_now, but add the created variable to the
   function scope.  */

tree
gfc_evaluate_now_function_scope (tree expr, stmtblock_t * pblock)
{
  tree var;
  var = gfc_create_var_np (TREE_TYPE (expr), NULL);
  gfc_add_decl_to_function (var);
  gfc_add_modify (pblock, var, expr);

  return var;
}

/* Build a MODIFY_EXPR node and add it to a given statement block PBLOCK.
   A MODIFY_EXPR is an assignment:
   LHS <- RHS.  */

void
gfc_add_modify_loc (location_t loc, stmtblock_t * pblock, tree lhs, tree rhs)
{
  tree tmp;

  tree t1, t2;
  t1 = TREE_TYPE (rhs);
  t2 = TREE_TYPE (lhs);
  /* Make sure that the types of the rhs and the lhs are compatible
     for scalar assignments.  We should probably have something
     similar for aggregates, but right now removing that check just
     breaks everything.  */
  gcc_checking_assert (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2)
		       || AGGREGATE_TYPE_P (TREE_TYPE (lhs)));

  tmp = fold_build2_loc (loc, MODIFY_EXPR, void_type_node, lhs,
			 rhs);
  gfc_add_expr_to_block (pblock, tmp);
}


void
gfc_add_modify (stmtblock_t * pblock, tree lhs, tree rhs)
{
  gfc_add_modify_loc (input_location, pblock, lhs, rhs);
}


/* Create a new scope/binding level and initialize a block.  Care must be
   taken when translating expressions as any temporaries will be placed in
   the innermost scope.  */

void
gfc_start_block (stmtblock_t * block)
{
  /* Start a new binding level.  */
  pushlevel ();
  block->has_scope = 1;

  /* The block is empty.  */
  block->head = NULL_TREE;
}


/* Initialize a block without creating a new scope.  */

void
gfc_init_block (stmtblock_t * block)
{
  block->head = NULL_TREE;
  block->has_scope = 0;
}


/* Sometimes we create a scope but it turns out that we don't actually
   need it.  This function merges the scope of BLOCK with its parent.
   Only variable decls will be merged, you still need to add the code.  */

void
gfc_merge_block_scope (stmtblock_t * block)
{
  tree decl;
  tree next;

  gcc_assert (block->has_scope);
  block->has_scope = 0;

  /* Remember the decls in this scope.  */
  decl = getdecls ();
  poplevel (0, 0);

  /* Add them to the parent scope.  */
  while (decl != NULL_TREE)
    {
      next = DECL_CHAIN (decl);
      DECL_CHAIN (decl) = NULL_TREE;

      pushdecl (decl);
      decl = next;
    }
}


/* Finish a scope containing a block of statements.  */

tree
gfc_finish_block (stmtblock_t * stmtblock)
{
  tree decl;
  tree expr;
  tree block;

  expr = stmtblock->head;
  if (!expr)
    expr = build_empty_stmt (input_location);

  stmtblock->head = NULL_TREE;

  if (stmtblock->has_scope)
    {
      decl = getdecls ();

      if (decl)
	{
	  block = poplevel (1, 0);
	  expr = build3_v (BIND_EXPR, decl, expr, block);
	}
      else
	poplevel (0, 0);
    }

  return expr;
}


/* Build an ADDR_EXPR and cast the result to TYPE.  If TYPE is NULL, the
   natural type is used.  */

tree
gfc_build_addr_expr (tree type, tree t)
{
  tree base_type = TREE_TYPE (t);
  tree natural_type;

  if (type && POINTER_TYPE_P (type)
      && TREE_CODE (base_type) == ARRAY_TYPE
      && TYPE_MAIN_VARIANT (TREE_TYPE (type))
	 == TYPE_MAIN_VARIANT (TREE_TYPE (base_type)))
    {
      tree min_val = size_zero_node;
      tree type_domain = TYPE_DOMAIN (base_type);
      if (type_domain && TYPE_MIN_VALUE (type_domain))
        min_val = TYPE_MIN_VALUE (type_domain);
      t = fold (build4_loc (input_location, ARRAY_REF, TREE_TYPE (type),
			    t, min_val, NULL_TREE, NULL_TREE));
      natural_type = type;
    }
  else
    natural_type = build_pointer_type (base_type);

  if (INDIRECT_REF_P (t))
    {
      if (!type)
	type = natural_type;
      t = TREE_OPERAND (t, 0);
      natural_type = TREE_TYPE (t);
    }
  else
    {
      tree base = get_base_address (t);
      if (base && DECL_P (base))
        TREE_ADDRESSABLE (base) = 1;
      t = fold_build1_loc (input_location, ADDR_EXPR, natural_type, t);
    }

  if (type && natural_type != type)
    t = convert (type, t);

  return t;
}


static tree
get_array_span (tree type, tree decl)
{
  tree span;

  /* Component references are guaranteed to have a reliable value for
     'span'. Likewise indirect references since they emerge from the
     conversion of a CFI descriptor or the hidden dummy descriptor.  */
  if (TREE_CODE (decl) == COMPONENT_REF
      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
    return gfc_conv_descriptor_span_get (decl);
  else if (INDIRECT_REF_P (decl)
	   && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
    return gfc_conv_descriptor_span_get (decl);

  /* Return the span for deferred character length array references.  */
  if (type
      && (TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == INTEGER_TYPE)
      && TYPE_STRING_FLAG (type))
    {
      if (TREE_CODE (decl) == PARM_DECL)
	decl = build_fold_indirect_ref_loc (input_location, decl);
      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
	span = gfc_conv_descriptor_span_get (decl);
      else
	span = gfc_get_character_len_in_bytes (type);
      span = (span && !integer_zerop (span))
	? (fold_convert (gfc_array_index_type, span)) : (NULL_TREE);
    }
  /* Likewise for class array or pointer array references.  */
  else if (TREE_CODE (decl) == FIELD_DECL
	   || VAR_OR_FUNCTION_DECL_P (decl)
	   || TREE_CODE (decl) == PARM_DECL)
    {
      if (GFC_DECL_CLASS (decl))
	{
	  /* When a temporary is in place for the class array, then the
	     original class' declaration is stored in the saved
	     descriptor.  */
	  if (DECL_LANG_SPECIFIC (decl) && GFC_DECL_SAVED_DESCRIPTOR (decl))
	    decl = GFC_DECL_SAVED_DESCRIPTOR (decl);
	  else
	    {
	      /* Allow for dummy arguments and other good things.  */
	      if (POINTER_TYPE_P (TREE_TYPE (decl)))
		decl = build_fold_indirect_ref_loc (input_location, decl);

	      /* Check if '_data' is an array descriptor.  If it is not,
		 the array must be one of the components of the class
		 object, so return a null span.  */
	      if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (
					  gfc_class_data_get (decl))))
		return NULL_TREE;
	    }
	  span = gfc_class_vtab_size_get (decl);
	  /* For unlimited polymorphic entities then _len component needs
	     to be multiplied with the size.  */
	  span = gfc_resize_class_size_with_len (NULL, decl, span);
	}
      else if (GFC_DECL_PTR_ARRAY_P (decl))
	{
	  if (TREE_CODE (decl) == PARM_DECL)
	    decl = build_fold_indirect_ref_loc (input_location, decl);
	  span = gfc_conv_descriptor_span_get (decl);
	}
      else
	span = NULL_TREE;
    }
  else
    span = NULL_TREE;

  return span;
}


tree
gfc_build_spanned_array_ref (tree base, tree offset, tree span)
{
  tree type;
  tree tmp;
  type = TREE_TYPE (TREE_TYPE (base));
  offset = fold_build2_loc (input_location, MULT_EXPR,
			    gfc_array_index_type,
			    offset, span);
  tmp = gfc_build_addr_expr (pvoid_type_node, base);
  tmp = fold_build_pointer_plus_loc (input_location, tmp, offset);
  tmp = fold_convert (build_pointer_type (type), tmp);
  if ((TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != ARRAY_TYPE)
      || !TYPE_STRING_FLAG (type))
    tmp = build_fold_indirect_ref_loc (input_location, tmp);
  return tmp;
}


/* Build an ARRAY_REF with its natural type.
   NON_NEGATIVE_OFFSET indicates if it’s true that OFFSET can’t be negative,
   and thus that an ARRAY_REF can safely be generated.  If it’s false, we
   have to play it safe and use pointer arithmetic.  */

tree
gfc_build_array_ref (tree base, tree offset, tree decl,
		     bool non_negative_offset, tree vptr)
{
  tree type = TREE_TYPE (base);
  tree span = NULL_TREE;

  if (GFC_ARRAY_TYPE_P (type) && GFC_TYPE_ARRAY_RANK (type) == 0)
    {
      gcc_assert (GFC_TYPE_ARRAY_CORANK (type) > 0);

      return fold_convert (TYPE_MAIN_VARIANT (type), base);
    }

  /* Scalar coarray, there is nothing to do.  */
  if (TREE_CODE (type) != ARRAY_TYPE)
    {
      gcc_assert (decl == NULL_TREE);
      gcc_assert (integer_zerop (offset));
      return base;
    }

  type = TREE_TYPE (type);

  if (DECL_P (base))
    TREE_ADDRESSABLE (base) = 1;

  /* Strip NON_LVALUE_EXPR nodes.  */
  STRIP_TYPE_NOPS (offset);

  /* If decl or vptr are non-null, pointer arithmetic for the array reference
     is likely. Generate the 'span' for the array reference.  */
  if (vptr)
    {
      span = gfc_vptr_size_get (vptr);

      /* Check if this is an unlimited polymorphic object carrying a character
	 payload. In this case, the 'len' field is non-zero.  */
      if (decl && GFC_CLASS_TYPE_P (TREE_TYPE (decl)))
	span = gfc_resize_class_size_with_len (NULL, decl, span);
    }
  else if (decl)
    span = get_array_span (type, decl);

  /* If a non-null span has been generated reference the element with
     pointer arithmetic.  */
  if (span != NULL_TREE)
    return gfc_build_spanned_array_ref (base, offset, span);
  /* Else use a straightforward array reference if possible.  */
  else if (non_negative_offset)
    return build4_loc (input_location, ARRAY_REF, type, base, offset,
		       NULL_TREE, NULL_TREE);
  /* Otherwise use pointer arithmetic.  */
  else
    {
      gcc_assert (TREE_CODE (TREE_TYPE (base)) == ARRAY_TYPE);
      tree min = NULL_TREE;
      if (TYPE_DOMAIN (TREE_TYPE (base))
	  && !integer_zerop (TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (base)))))
	min = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (base)));

      tree zero_based_index
	   = min ? fold_build2_loc (input_location, MINUS_EXPR,
				    gfc_array_index_type,
				    fold_convert (gfc_array_index_type, offset),
				    fold_convert (gfc_array_index_type, min))
		 : fold_convert (gfc_array_index_type, offset);

      tree elt_size = fold_convert (gfc_array_index_type,
				    TYPE_SIZE_UNIT (type));

      tree offset_bytes = fold_build2_loc (input_location, MULT_EXPR,
					   gfc_array_index_type,
					   zero_based_index, elt_size);

      tree base_addr = gfc_build_addr_expr (pvoid_type_node, base);

      tree ptr = fold_build_pointer_plus_loc (input_location, base_addr,
					      offset_bytes);
      return build1_loc (input_location, INDIRECT_REF, type,
			 fold_convert (build_pointer_type (type), ptr));
    }
}


/* Generate a call to print a runtime error possibly including multiple
   arguments and a locus.  */

static tree
trans_runtime_error_vararg (tree errorfunc, locus* where, const char* msgid,
			    va_list ap)
{
  stmtblock_t block;
  tree tmp;
  tree arg, arg2;
  tree *argarray;
  tree fntype;
  char *message;
  const char *p;
  int nargs, i;
  location_t loc;

  /* Compute the number of extra arguments from the format string.  */
  for (p = msgid, nargs = 0; *p; p++)
    if (*p == '%')
      {
	p++;
	if (*p != '%')
	  nargs++;
      }

  /* The code to generate the error.  */
  gfc_start_block (&block);

  if (where)
    {
      location_t loc = gfc_get_location (where);
      message = xasprintf ("At line %d of file %s",  LOCATION_LINE (loc),
			   LOCATION_FILE (loc));
    }
  else
    message = xasprintf ("In file '%s', around line %d",
			 gfc_source_file, LOCATION_LINE (input_location));

  arg = gfc_build_addr_expr (pchar_type_node,
			     gfc_build_localized_cstring_const (message));
  free (message);

  message = xasprintf ("%s", _(msgid));
  arg2 = gfc_build_addr_expr (pchar_type_node,
			      gfc_build_localized_cstring_const (message));
  free (message);

  /* Build the argument array.  */
  argarray = XALLOCAVEC (tree, nargs + 2);
  argarray[0] = arg;
  argarray[1] = arg2;
  for (i = 0; i < nargs; i++)
    argarray[2 + i] = va_arg (ap, tree);

  /* Build the function call to runtime_(warning,error)_at; because of the
     variable number of arguments, we can't use build_call_expr_loc dinput_location,
     irectly.  */
  fntype = TREE_TYPE (errorfunc);

  loc = where ? gfc_get_location (where) : input_location;
  tmp = fold_build_call_array_loc (loc, TREE_TYPE (fntype),
				   fold_build1_loc (loc, ADDR_EXPR,
					     build_pointer_type (fntype),
					     errorfunc),
				   nargs + 2, argarray);
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


tree
gfc_trans_runtime_error (bool error, locus* where, const char* msgid, ...)
{
  va_list ap;
  tree result;

  va_start (ap, msgid);
  result = trans_runtime_error_vararg (error
				       ? gfor_fndecl_runtime_error_at
				       : gfor_fndecl_runtime_warning_at,
				       where, msgid, ap);
  va_end (ap);
  return result;
}


/* Generate a runtime error if COND is true.  */

void
gfc_trans_runtime_check (bool error, bool once, tree cond, stmtblock_t * pblock,
			 locus * where, const char * msgid, ...)
{
  va_list ap;
  stmtblock_t block;
  tree body;
  tree tmp;
  tree tmpvar = NULL;

  if (integer_zerop (cond))
    return;

  if (once)
    {
       tmpvar = gfc_create_var (boolean_type_node, "print_warning");
       TREE_STATIC (tmpvar) = 1;
       DECL_INITIAL (tmpvar) = boolean_true_node;
       gfc_add_expr_to_block (pblock, tmpvar);
    }

  gfc_start_block (&block);

  /* For error, runtime_error_at already implies PRED_NORETURN.  */
  if (!error && once)
    gfc_add_expr_to_block (&block, build_predict_expr (PRED_FORTRAN_WARN_ONCE,
						       NOT_TAKEN));

  /* The code to generate the error.  */
  va_start (ap, msgid);
  gfc_add_expr_to_block (&block,
			 trans_runtime_error_vararg
			 (error ? gfor_fndecl_runtime_error_at
			  : gfor_fndecl_runtime_warning_at,
			  where, msgid, ap));
  va_end (ap);

  if (once)
    gfc_add_modify (&block, tmpvar, boolean_false_node);

  body = gfc_finish_block (&block);

  if (integer_onep (cond))
    {
      gfc_add_expr_to_block (pblock, body);
    }
  else
    {
      location_t loc = where ? gfc_get_location (where) : input_location;
      if (once)
	cond = fold_build2_loc (loc, TRUTH_AND_EXPR, boolean_type_node, tmpvar,
				fold_convert (boolean_type_node, cond));

      tmp = fold_build3_loc (loc, COND_EXPR, void_type_node, cond, body,
			     build_empty_stmt (loc));
      gfc_add_expr_to_block (pblock, tmp);
    }
}


static tree
trans_os_error_at (locus* where, const char* msgid, ...)
{
  va_list ap;
  tree result;

  va_start (ap, msgid);
  result = trans_runtime_error_vararg (gfor_fndecl_os_error_at,
				       where, msgid, ap);
  va_end (ap);
  return result;
}



/* Call malloc to allocate size bytes of memory, with special conditions:
      + if size == 0, return a malloced area of size 1,
      + if malloc returns NULL, issue a runtime error.  */
tree
gfc_call_malloc (stmtblock_t * block, tree type, tree size)
{
  tree tmp, malloc_result, null_result, res, malloc_tree;
  stmtblock_t block2;

  /* Create a variable to hold the result.  */
  res = gfc_create_var (prvoid_type_node, NULL);

  /* Call malloc.  */
  gfc_start_block (&block2);

  if (size == NULL_TREE)
    size = build_int_cst (size_type_node, 1);

  size = fold_convert (size_type_node, size);
  size = fold_build2_loc (input_location, MAX_EXPR, size_type_node, size,
			  build_int_cst (size_type_node, 1));

  malloc_tree = builtin_decl_explicit (BUILT_IN_MALLOC);
  gfc_add_modify (&block2, res,
		  fold_convert (prvoid_type_node,
				build_call_expr_loc (input_location,
						     malloc_tree, 1, size)));

  /* Optionally check whether malloc was successful.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_MEM)
    {
      null_result = fold_build2_loc (input_location, EQ_EXPR,
				     logical_type_node, res,
				     build_int_cst (pvoid_type_node, 0));
      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			     null_result,
			     trans_os_error_at (NULL,
						"Error allocating %lu bytes",
						fold_convert
						(long_unsigned_type_node,
						 size)),
			     build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block2, tmp);
    }

  malloc_result = gfc_finish_block (&block2);
  gfc_add_expr_to_block (block, malloc_result);

  if (type != NULL)
    res = fold_convert (type, res);
  return res;
}


/* Allocate memory, using an optional status argument.

   This function follows the following pseudo-code:

    void *
    allocate (size_t size, integer_type stat)
    {
      void *newmem;

      if (stat requested)
	stat = 0;

      // if cond == NULL_NULL:
      newmem = malloc (MAX (size, 1));
      // otherwise:
      newmem = <cond> ? <alt_alloc> : malloc (MAX (size, 1))
      if (newmem == NULL)
      {
        if (stat)
	  *stat = LIBERROR_NO_MEMORY;
        else
	  runtime_error ("Allocation would exceed memory limit");
      }
      return newmem;
    }  */
void
gfc_allocate_using_malloc (stmtblock_t * block, tree pointer,
			   tree size, tree status, tree cond, tree alt_alloc,
			   tree extra_success_expr)
{
  tree tmp, error_cond;
  stmtblock_t on_error;
  tree status_type = status ? TREE_TYPE (status) : NULL_TREE;

  /* If successful and stat= is given, set status to 0.  */
  if (status != NULL_TREE)
      gfc_add_expr_to_block (block,
	     fold_build2_loc (input_location, MODIFY_EXPR, status_type,
			      status, build_int_cst (status_type, 0)));

  /* The allocation itself.  */
  size = fold_convert (size_type_node, size);
  tmp = fold_build2_loc (input_location, MAX_EXPR, size_type_node,
			 size, build_int_cst (size_type_node, 1));

  tmp = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MALLOC), 1, tmp);
  if (cond == boolean_true_node)
    tmp = alt_alloc;
  else if (cond)
    tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp), cond,
		      alt_alloc, tmp);

  gfc_add_modify (block, pointer, fold_convert (TREE_TYPE (pointer), tmp));

  /* What to do in case of error.  */
  gfc_start_block (&on_error);
  if (status != NULL_TREE)
    {
      tmp = fold_build2_loc (input_location, MODIFY_EXPR, status_type, status,
			     build_int_cst (status_type, LIBERROR_NO_MEMORY));
      gfc_add_expr_to_block (&on_error, tmp);
    }
  else
    {
      /* Here, os_error_at already implies PRED_NORETURN.  */
      tree lusize = fold_convert (long_unsigned_type_node, size);
      tmp = trans_os_error_at (NULL, "Error allocating %lu bytes", lusize);
      gfc_add_expr_to_block (&on_error, tmp);
    }

  error_cond = fold_build2_loc (input_location, EQ_EXPR,
				logical_type_node, pointer,
				build_int_cst (prvoid_type_node, 0));
  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			 gfc_unlikely (error_cond, PRED_FORTRAN_FAIL_ALLOC),
			 gfc_finish_block (&on_error),
			 extra_success_expr
			 ? extra_success_expr
			 : build_empty_stmt (input_location));

  gfc_add_expr_to_block (block, tmp);
}


/* Allocate memory, using an optional status argument.

   This function follows the following pseudo-code:

    void *
    allocate (size_t size, void** token, int *stat, char* errmsg, int errlen)
    {
      void *newmem;

      newmem = _caf_register (size, regtype, token, &stat, errmsg, errlen);
      return newmem;
    }  */
void
gfc_allocate_using_caf_lib (stmtblock_t * block, tree pointer, tree size,
			    tree token, tree status, tree errmsg, tree errlen,
			    gfc_coarray_regtype alloc_type)
{
  tree tmp, pstat;

  gcc_assert (token != NULL_TREE);

  /* The allocation itself.  */
  if (status == NULL_TREE)
    pstat  = null_pointer_node;
  else
    pstat  = gfc_build_addr_expr (NULL_TREE, status);

  if (errmsg == NULL_TREE)
    {
      gcc_assert(errlen == NULL_TREE);
      errmsg = null_pointer_node;
      errlen = integer_zero_node;
    }

  size = fold_convert (size_type_node, size);
  tmp = build_call_expr_loc (input_location,
	     gfor_fndecl_caf_register, 7,
	     fold_build2_loc (input_location,
			      MAX_EXPR, size_type_node, size, size_one_node),
	     build_int_cst (integer_type_node, alloc_type),
	     token, gfc_build_addr_expr (pvoid_type_node, pointer),
	     pstat, errmsg, errlen);

  gfc_add_expr_to_block (block, tmp);

  /* It guarantees memory consistency within the same segment */
  tmp = gfc_build_string_const (strlen ("memory")+1, "memory"),
  tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
		    gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
		    tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
  ASM_VOLATILE_P (tmp) = 1;
  gfc_add_expr_to_block (block, tmp);
}


/* Generate code for an ALLOCATE statement when the argument is an
   allocatable variable.  If the variable is currently allocated, it is an
   error to allocate it again.

   This function follows the following pseudo-code:

    void *
    allocate_allocatable (void *mem, size_t size, integer_type stat)
    {
      if (mem == NULL)
	return allocate (size, stat);
      else
      {
	if (stat)
	  stat = LIBERROR_ALLOCATION;
	else
	  runtime_error ("Attempting to allocate already allocated variable");
      }
    }

    expr must be set to the original expression being allocated for its locus
    and variable name in case a runtime error has to be printed.  */
void
gfc_allocate_allocatable (stmtblock_t * block, tree mem, tree size,
			  tree token, tree status, tree errmsg, tree errlen,
			  tree label_finish, gfc_expr* expr, int corank,
			  tree cond, tree alt_alloc, tree extra_success_expr)
{
  stmtblock_t alloc_block;
  tree tmp, null_mem, alloc, error;
  tree type = TREE_TYPE (mem);
  symbol_attribute caf_attr;
  bool need_assign = false, refs_comp = false;
  gfc_coarray_regtype caf_alloc_type = GFC_CAF_COARRAY_ALLOC;

  size = fold_convert (size_type_node, size);
  null_mem = gfc_unlikely (fold_build2_loc (input_location, NE_EXPR,
					    logical_type_node, mem,
					    build_int_cst (type, 0)),
			   PRED_FORTRAN_REALLOC);

  /* If mem is NULL, we call gfc_allocate_using_malloc or
     gfc_allocate_using_lib.  */
  gfc_start_block (&alloc_block);

  if (flag_coarray == GFC_FCOARRAY_LIB)
    caf_attr = gfc_caf_attr (expr, true, &refs_comp);

  if (flag_coarray == GFC_FCOARRAY_LIB
      && (corank > 0 || caf_attr.codimension))
    {
      tree cond2, sub_caf_tree;
      gfc_se se;
      bool compute_special_caf_types_size = false;

      if (expr->ts.type == BT_DERIVED
	  && expr->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
	  && expr->ts.u.derived->intmod_sym_id == ISOFORTRAN_LOCK_TYPE)
	{
	  compute_special_caf_types_size = true;
	  caf_alloc_type = GFC_CAF_LOCK_ALLOC;
	}
      else if (expr->ts.type == BT_DERIVED
	       && expr->ts.u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
	       && expr->ts.u.derived->intmod_sym_id == ISOFORTRAN_EVENT_TYPE)
	{
	  compute_special_caf_types_size = true;
	  caf_alloc_type = GFC_CAF_EVENT_ALLOC;
	}
      else if (!caf_attr.coarray_comp && refs_comp)
	/* Only allocatable components in a derived type coarray can be
	   allocate only.  */
	caf_alloc_type = GFC_CAF_COARRAY_ALLOC_ALLOCATE_ONLY;

      gfc_init_se (&se, NULL);
      sub_caf_tree = gfc_get_ultimate_alloc_ptr_comps_caf_token (&se, expr);
      if (sub_caf_tree == NULL_TREE)
	sub_caf_tree = token;

      /* When mem is an array ref, then strip the .data-ref.  */
      if (TREE_CODE (mem) == COMPONENT_REF
	  && !(GFC_ARRAY_TYPE_P (TREE_TYPE (mem))))
	tmp = TREE_OPERAND (mem, 0);
      else
	tmp = mem;

      if (!(GFC_ARRAY_TYPE_P (TREE_TYPE (tmp))
	    && TYPE_LANG_SPECIFIC (TREE_TYPE (tmp))->corank == 0)
	  && !GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp)))
	{
	  symbol_attribute attr;

	  gfc_clear_attr (&attr);
	  tmp = gfc_conv_scalar_to_descriptor (&se, mem, attr);
	  need_assign = true;
	}
      gfc_add_block_to_block (&alloc_block, &se.pre);

      /* In the front end, we represent the lock variable as pointer. However,
	 the FE only passes the pointer around and leaves the actual
	 representation to the library. Hence, we have to convert back to the
	 number of elements.  */
      if (compute_special_caf_types_size)
	size = fold_build2_loc (input_location, TRUNC_DIV_EXPR, size_type_node,
				size, TYPE_SIZE_UNIT (ptr_type_node));

      gfc_allocate_using_caf_lib (&alloc_block, tmp, size, sub_caf_tree,
				  status, errmsg, errlen, caf_alloc_type);
      if (need_assign)
	gfc_add_modify (&alloc_block, mem, fold_convert (TREE_TYPE (mem),
					   gfc_conv_descriptor_data_get (tmp)));
      if (status != NULL_TREE)
	{
	  TREE_USED (label_finish) = 1;
	  tmp = build1_v (GOTO_EXPR, label_finish);
	  cond2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				   status, build_zero_cst (TREE_TYPE (status)));
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 gfc_unlikely (cond2, PRED_FORTRAN_FAIL_ALLOC),
				 tmp, build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&alloc_block, tmp);
	}
    }
  else
    gfc_allocate_using_malloc (&alloc_block, mem, size, status,
			       cond, alt_alloc, extra_success_expr);

  alloc = gfc_finish_block (&alloc_block);

  /* If mem is not NULL, we issue a runtime error or set the
     status variable.  */
  if (expr)
    {
      tree varname;

      gcc_assert (expr->expr_type == EXPR_VARIABLE && expr->symtree);
      varname = gfc_build_cstring_const (expr->symtree->name);
      varname = gfc_build_addr_expr (pchar_type_node, varname);

      error = gfc_trans_runtime_error (true, &expr->where,
				       "Attempting to allocate already"
				       " allocated variable '%s'",
				       varname);
    }
  else
    error = gfc_trans_runtime_error (true, NULL,
				     "Attempting to allocate already allocated"
				     " variable");

  if (status != NULL_TREE)
    {
      tree status_type = TREE_TYPE (status);

      error = fold_build2_loc (input_location, MODIFY_EXPR, status_type,
	      status, build_int_cst (status_type, LIBERROR_ALLOCATION));
    }

  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, null_mem,
			 error, alloc);
  gfc_add_expr_to_block (block, tmp);
}


/* Free a given variable.  */

tree
gfc_call_free (tree var)
{
  return build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_FREE),
			      1, fold_convert (pvoid_type_node, var));
}


/* Generate the data reference to the finalization procedure pointer associated
   with the expression passed as argument in EXPR.  */

static void
get_final_proc_ref (gfc_se *se, gfc_expr *expr, tree class_container)
{
  gfc_expr *final_wrapper = NULL;

  gcc_assert (expr->ts.type == BT_DERIVED || expr->ts.type == BT_CLASS);

  bool using_class_container = false;
  if (expr->ts.type == BT_DERIVED)
    gfc_is_finalizable (expr->ts.u.derived, &final_wrapper);
  else if (class_container)
    {
      using_class_container = true;
      se->expr = gfc_class_vtab_final_get (class_container);
    }
  else
    {
      final_wrapper = gfc_copy_expr (expr);
      gfc_add_vptr_component (final_wrapper);
      gfc_add_final_component (final_wrapper);
    }

  if (!using_class_container)
    {
      gcc_assert (final_wrapper->expr_type == EXPR_VARIABLE);

      gfc_conv_expr (se, final_wrapper);
    }

  if (POINTER_TYPE_P (TREE_TYPE (se->expr)))
    se->expr = build_fold_indirect_ref_loc (input_location, se->expr);

  if (expr->ts.type != BT_DERIVED && !using_class_container)
    gfc_free_expr (final_wrapper);
}


/* Generate the code to obtain the value of the element size of the expression
   passed as argument in EXPR.  */

static void
get_elem_size (gfc_se *se, gfc_expr *expr, tree class_container)
{
  gcc_assert (expr->ts.type == BT_DERIVED || expr->ts.type == BT_CLASS);

  if (expr->ts.type == BT_DERIVED)
    {
      se->expr = gfc_typenode_for_spec (&expr->ts);
      se->expr = TYPE_SIZE_UNIT (se->expr);
      se->expr = fold_convert (gfc_array_index_type, se->expr);
    }
  else if (class_container)
    se->expr = gfc_class_vtab_size_get (class_container);
  else
    {
      gfc_expr *class_size = gfc_copy_expr (expr);
      gfc_add_vptr_component (class_size);
      gfc_add_size_component (class_size);

      gfc_conv_expr (se, class_size);
      gcc_assert (se->post.head == NULL_TREE);
      gfc_free_expr (class_size);
    }
}


/* Generate the data reference (array) descriptor corresponding to the
   expression passed as argument in VAR.  */

static void
get_var_descr (gfc_se *se, gfc_expr *var, tree class_container)
{
  gfc_se tmp_se;

  gcc_assert (var);

  gfc_init_se (&tmp_se, NULL);

  if (var->ts.type == BT_DERIVED)
    {
      tmp_se.want_pointer = 1;
      if (var->rank)
	{
	  tmp_se.descriptor_only = 1;
	  gfc_conv_expr_descriptor (&tmp_se, var);
	}
      else
	gfc_conv_expr (&tmp_se, var);
    }
  else if (class_container)
    tmp_se.expr = gfc_class_data_get (class_container);
  else
    {
      gfc_expr *array_expr;

      array_expr = gfc_copy_expr (var);

      tmp_se.want_pointer = 1;
      if (array_expr->rank)
	{
	  gfc_add_class_array_ref (array_expr);
	  tmp_se.descriptor_only = 1;
	  gfc_conv_expr_descriptor (&tmp_se, array_expr);
	}
      else
	{
	  gfc_add_data_component (array_expr);
	  gfc_conv_expr (&tmp_se, array_expr);
	  gcc_assert (tmp_se.post.head == NULL_TREE);
	}
      gfc_free_expr (array_expr);
    }

  if (var->rank == 0)
    {
      if (var->ts.type == BT_DERIVED
	  || !gfc_is_coarray (var))
	{
	  /* No copy back needed, hence set attr's allocatable/pointer
	     to zero.  */
	  symbol_attribute attr;
	  gfc_clear_attr (&attr);
	  tmp_se.expr = gfc_conv_scalar_to_descriptor (&tmp_se, tmp_se.expr,
						       attr);
	}
      gcc_assert (tmp_se.post.head == NULL_TREE);
    }

  if (!POINTER_TYPE_P (TREE_TYPE (tmp_se.expr)))
    tmp_se.expr = gfc_build_addr_expr (NULL, tmp_se.expr);

  gfc_add_block_to_block (&se->pre, &tmp_se.pre);
  gfc_add_block_to_block (&se->post, &tmp_se.post);
  se->expr = tmp_se.expr;
}


static void
get_vptr (gfc_se *se, gfc_expr *expr, tree class_container)
{
  if (class_container)
    se->expr = gfc_class_vptr_get (class_container);
  else
    {
      gfc_expr *vptr_expr = gfc_copy_expr (expr);
      gfc_add_vptr_component (vptr_expr);

      gfc_se tmp_se;
      gfc_init_se (&tmp_se, NULL);
      tmp_se.want_pointer = 1;
      gfc_conv_expr (&tmp_se, vptr_expr);
      gfc_free_expr (vptr_expr);

      gfc_add_block_to_block (&se->pre, &tmp_se.pre);
      gfc_add_block_to_block (&se->post, &tmp_se.post);
      se->expr = tmp_se.expr;
    }
}


bool
gfc_add_comp_finalizer_call (stmtblock_t *block, tree decl, gfc_component *comp,
			     bool fini_coarray)
{
  gfc_se se;
  stmtblock_t block2;
  tree final_fndecl, size, array, tmp, cond;
  symbol_attribute attr;
  gfc_expr *final_expr = NULL;

  if (comp->ts.type != BT_DERIVED && comp->ts.type != BT_CLASS)
    return false;

  gfc_init_block (&block2);

  if (comp->ts.type == BT_DERIVED)
    {
      if (comp->attr.pointer)
	return false;

      gfc_is_finalizable (comp->ts.u.derived, &final_expr);
      if (!final_expr)
        return false;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, final_expr);
      final_fndecl = se.expr;
      size = gfc_typenode_for_spec (&comp->ts);
      size = TYPE_SIZE_UNIT (size);
      size = fold_convert (gfc_array_index_type, size);

      array = decl;
    }
  else /* comp->ts.type == BT_CLASS.  */
    {
      if (CLASS_DATA (comp)->attr.class_pointer)
	return false;

      gfc_is_finalizable (CLASS_DATA (comp)->ts.u.derived, &final_expr);
      final_fndecl = gfc_class_vtab_final_get (decl);
      size = gfc_class_vtab_size_get (decl);
      array = gfc_class_data_get (decl);
    }

  if (comp->attr.allocatable
      || (comp->ts.type == BT_CLASS && CLASS_DATA (comp)->attr.allocatable))
    {
      tmp = GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (array))
	    ?  gfc_conv_descriptor_data_get (array) : array;
      cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			    tmp, fold_convert (TREE_TYPE (tmp),
						 null_pointer_node));
    }
  else
    cond = logical_true_node;

  if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (array)))
    {
      gfc_clear_attr (&attr);
      gfc_init_se (&se, NULL);
      array = gfc_conv_scalar_to_descriptor (&se, array, attr);
      gfc_add_block_to_block (&block2, &se.pre);
      gcc_assert (se.post.head == NULL_TREE);
    }

  if (!POINTER_TYPE_P (TREE_TYPE (array)))
    array = gfc_build_addr_expr (NULL, array);

  if (!final_expr)
    {
      tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			     final_fndecl,
			     fold_convert (TREE_TYPE (final_fndecl),
					   null_pointer_node));
      cond = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			      logical_type_node, cond, tmp);
    }

  if (POINTER_TYPE_P (TREE_TYPE (final_fndecl)))
    final_fndecl = build_fold_indirect_ref_loc (input_location, final_fndecl);

  tmp = build_call_expr_loc (input_location,
			     final_fndecl, 3, array,
			     size, fini_coarray ? boolean_true_node
						: boolean_false_node);
  gfc_add_expr_to_block (&block2, tmp);
  tmp = gfc_finish_block (&block2);

  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond, tmp,
			 build_empty_stmt (input_location));
  gfc_add_expr_to_block (block, tmp);

  return true;
}


/* Add a call to the finalizer, using the passed *expr. Returns
   true when a finalizer call has been inserted.  */

bool
gfc_add_finalizer_call (stmtblock_t *block, gfc_expr *expr2,
			tree class_container)
{
  tree tmp;
  gfc_ref *ref;
  gfc_expr *expr;

  if (!expr2 || (expr2->ts.type != BT_DERIVED && expr2->ts.type != BT_CLASS))
    return false;

  /* Finalization of these temporaries is made by explicit calls in
     resolve.cc(generate_component_assignments).  */
  if (expr2->expr_type == EXPR_VARIABLE
      && expr2->symtree->n.sym->name[0] == '_'
      && expr2->ts.type == BT_DERIVED
      && expr2->ts.u.derived->attr.defined_assign_comp)
    return false;

  if (expr2->ts.type == BT_DERIVED
      && !gfc_is_finalizable (expr2->ts.u.derived, NULL))
    return false;

  /* If we have a class array, we need go back to the class
     container.  */
  expr = gfc_copy_expr (expr2);

  if (expr->ref && expr->ref->next && !expr->ref->next->next
      && expr->ref->next->type == REF_ARRAY
      && expr->ref->type == REF_COMPONENT
      && strcmp (expr->ref->u.c.component->name, "_data") == 0)
    {
      gfc_free_ref_list (expr->ref);
      expr->ref = NULL;
    }
  else
    for (ref = expr->ref; ref; ref = ref->next)
      if (ref->next && ref->next->next && !ref->next->next->next
         && ref->next->next->type == REF_ARRAY
         && ref->next->type == REF_COMPONENT
         && strcmp (ref->next->u.c.component->name, "_data") == 0)
       {
         gfc_free_ref_list (ref->next);
         ref->next = NULL;
       }

  if (expr->ts.type == BT_CLASS && (!expr2->rank || !expr2->corank)
      && !expr2->ref && CLASS_DATA (expr2->symtree->n.sym)->as)
    {
      expr->rank = CLASS_DATA (expr2->symtree->n.sym)->as->rank;
      expr->corank = CLASS_DATA (expr2->symtree->n.sym)->as->corank;
    }

  stmtblock_t tmp_block;
  gfc_start_block (&tmp_block);

  gfc_se final_se;
  gfc_init_se (&final_se, NULL);
  get_final_proc_ref (&final_se, expr, class_container);
  gfc_add_block_to_block (block, &final_se.pre);

  gfc_se size_se;
  gfc_init_se (&size_se, NULL);
  get_elem_size (&size_se, expr, class_container);
  gfc_add_block_to_block (&tmp_block, &size_se.pre);

  gfc_se desc_se;
  gfc_init_se (&desc_se, NULL);
  get_var_descr (&desc_se, expr, class_container);
  gfc_add_block_to_block (&tmp_block, &desc_se.pre);

  tmp = build_call_expr_loc (input_location, final_se.expr, 3,
			     desc_se.expr, size_se.expr,
			     boolean_false_node);

  gfc_add_expr_to_block (&tmp_block, tmp);

  gfc_add_block_to_block (&tmp_block, &desc_se.post);
  gfc_add_block_to_block (&tmp_block, &size_se.post);

  tmp = gfc_finish_block (&tmp_block);

  if (expr->ts.type == BT_CLASS
      && !gfc_is_finalizable (expr->ts.u.derived, NULL))
    {
      tree cond;

      tree ptr = gfc_build_addr_expr (NULL_TREE, final_se.expr);

      cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			      ptr, build_int_cst (TREE_TYPE (ptr), 0));

      /* For CLASS(*) not only sym->_vtab->_final can be NULL
	 but already sym->_vtab itself.  */
      if (UNLIMITED_POLY (expr))
	{
	  tree cond2;
	  gfc_se vptr_se;

	  gfc_init_se (&vptr_se, NULL);
	  get_vptr (&vptr_se, expr, class_container);

	  cond2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				   vptr_se.expr,
				   build_int_cst (TREE_TYPE (vptr_se.expr), 0));
	  cond = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
				  logical_type_node, cond2, cond);
	}

      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			     cond, tmp, build_empty_stmt (input_location));
    }

  gfc_add_expr_to_block (block, tmp);
  gfc_add_block_to_block (block, &final_se.post);
  gfc_free_expr (expr);

  return true;
}


  /* F2018 (7.5.6.3): "When an intrinsic assignment statement is executed
     (10.2.1.3), if the variable is not an unallocated allocatable variable,
     it is finalized after evaluation of expr and before the definition of
     the variable. If the variable is an allocated allocatable variable, or
     has an allocated allocatable subobject, that would be deallocated by
     intrinsic assignment, the finalization occurs before the deallocation */

bool
gfc_assignment_finalizer_call (gfc_se *lse, gfc_expr *expr1, bool init_flag)
{
  symbol_attribute lhs_attr;
  tree final_expr;
  tree ptr;
  tree cond;
  gfc_se se;
  gfc_symbol *sym = expr1->symtree->n.sym;
  gfc_ref *ref = expr1->ref;
  stmtblock_t final_block;
  gfc_init_block (&final_block);
  gfc_expr *finalize_expr;
  bool class_array_ref;

  /* We have to exclude vtable procedures (_copy and _final especially), uses
     of gfc_trans_assignment_1 in initialization and allocation before trying
     to build a final call.  */
  if (!expr1->must_finalize
      || sym->attr.artificial
      || sym->ns->proc_name->attr.artificial
      || init_flag)
    return false;

  class_array_ref = ref && ref->type == REF_COMPONENT
		    && !strcmp (ref->u.c.component->name, "_data")
		    && ref->next && ref->next->type == REF_ARRAY
		    && !ref->next->next;

  if (class_array_ref)
    {
      finalize_expr = gfc_lval_expr_from_sym (sym);
      finalize_expr->must_finalize = 1;
      ref = NULL;
    }
  else
    finalize_expr = gfc_copy_expr (expr1);

  /* F2018 7.5.6.2: Only finalizable entities are finalized.  */
  if (!(expr1->ts.type == BT_DERIVED
	&& gfc_is_finalizable (expr1->ts.u.derived, NULL))
      && expr1->ts.type != BT_CLASS)
      return false;

  if (!gfc_may_be_finalized (sym->ts))
    return false;

  gfc_init_block (&final_block);
  bool finalizable = gfc_add_finalizer_call (&final_block, finalize_expr);
  gfc_free_expr (finalize_expr);

  if (!finalizable)
    return false;

  lhs_attr = gfc_expr_attr (expr1);

  /* Check allocatable/pointer is allocated/associated.  */
  if (lhs_attr.allocatable || lhs_attr.pointer)
    {
      if (expr1->ts.type == BT_CLASS)
	{
	  ptr = gfc_get_class_from_gfc_expr (expr1);
	  gcc_assert (ptr != NULL_TREE);
	  ptr = gfc_class_data_get (ptr);
	  if (lhs_attr.dimension)
	    ptr = gfc_conv_descriptor_data_get (ptr);
	}
      else
	{
	  gfc_init_se (&se, NULL);
	  if (expr1->rank)
	    {
	      gfc_conv_expr_descriptor (&se, expr1);
	      ptr = gfc_conv_descriptor_data_get (se.expr);
	    }
	  else
	    {
	      gfc_conv_expr (&se, expr1);
	      ptr = gfc_build_addr_expr (NULL_TREE, se.expr);
	    }
	}

      cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			      ptr, build_zero_cst (TREE_TYPE (ptr)));
      final_expr = build3_loc (input_location, COND_EXPR, void_type_node,
			       cond, gfc_finish_block (&final_block),
			       build_empty_stmt (input_location));
    }
  else
    final_expr = gfc_finish_block (&final_block);

  /* Check optional present.  */
  if (sym->attr.optional)
    {
      cond = gfc_conv_expr_present (sym);
      final_expr = build3_loc (input_location, COND_EXPR, void_type_node,
			       cond, final_expr,
			       build_empty_stmt (input_location));
    }

  gfc_add_expr_to_block (&lse->finalblock, final_expr);

  return true;
}


/* Finalize a TREE expression using the finalizer wrapper. The result is
   fixed in order to prevent repeated calls.  */

void
gfc_finalize_tree_expr (gfc_se *se, gfc_symbol *derived,
			symbol_attribute attr, int rank)
{
  tree vptr, final_fndecl, desc, tmp, size, is_final;
  tree data_ptr, data_null, cond;
  gfc_symbol *vtab;
  gfc_se post_se;
  bool is_class = GFC_CLASS_TYPE_P (TREE_TYPE (se->expr));

  if (attr.pointer)
    return;

  /* Derived type function results with components that have defined
     assignements are handled in resolve.cc(generate_component_assignments)  */
  if (derived && (derived->attr.is_c_interop
		  || derived->attr.is_iso_c
		  || derived->attr.is_bind_c
		  || derived->attr.defined_assign_comp))
    return;

  if (is_class)
    {
      if (!VAR_P (se->expr))
	{
	  desc = gfc_evaluate_now (se->expr, &se->pre);
	  se->expr = desc;
	}
      desc = gfc_class_data_get (se->expr);
      vptr = gfc_class_vptr_get (se->expr);
    }
  else if (derived && gfc_is_finalizable (derived, NULL))
    {
      if (!derived->components && (!rank || attr.elemental))
	{
	  /* Any attempt to assign zero length entities, causes the gimplifier
	     all manner of problems. Instead, a variable is created to act as
	     as the argument for the final call.  */
	  desc = gfc_create_var (TREE_TYPE (se->expr), "zero");
	}
      else if (se->direct_byref)
	{
	  desc = gfc_evaluate_now (se->expr, &se->finalblock);
	  if (derived->attr.alloc_comp)
	    {
	      /* Need to copy allocated components and not finalize.  */
	      tmp = gfc_copy_alloc_comp_no_fini (derived, se->expr, desc, rank, 0);
	      gfc_add_expr_to_block (&se->finalblock, tmp);
	    }
	}
      else
	{
	  desc = gfc_evaluate_now (se->expr, &se->pre);
	  se->expr = gfc_evaluate_now (desc, &se->pre);
	  if (derived->attr.alloc_comp)
	    {
	      /* Need to copy allocated components and not finalize.  */
	      tmp = gfc_copy_alloc_comp_no_fini (derived, se->expr, desc, rank, 0);
	      gfc_add_expr_to_block (&se->pre, tmp);
	    }
	}

      vtab = gfc_find_derived_vtab (derived);
      if (vtab->backend_decl == NULL_TREE)
	vptr = gfc_get_symbol_decl (vtab);
      else
	vptr = vtab->backend_decl;
      vptr = gfc_build_addr_expr (NULL, vptr);
    }
  else
    return;

  size = gfc_vptr_size_get (vptr);
  final_fndecl = gfc_vptr_final_get (vptr);
  is_final = fold_build2_loc (input_location, NE_EXPR,
			      logical_type_node,
			      final_fndecl,
			      fold_convert (TREE_TYPE (final_fndecl),
					    null_pointer_node));

  final_fndecl = build_fold_indirect_ref_loc (input_location,
					      final_fndecl);
  if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
    {
      if (is_class || attr.elemental)
	desc = gfc_conv_scalar_to_descriptor (se, desc, attr);
      else
	{
	  gfc_init_se (&post_se, NULL);
	  desc = gfc_conv_scalar_to_descriptor (&post_se, desc, attr);
	  gfc_add_expr_to_block (&se->pre, gfc_finish_block (&post_se.pre));
	}
    }

  if (derived && !derived->components)
    {
      /* All the conditions below break down for zero length derived types.  */
      tmp = build_call_expr_loc (input_location, final_fndecl, 3,
				 gfc_build_addr_expr (NULL, desc),
				 size, boolean_false_node);
      gfc_add_expr_to_block (&se->finalblock, tmp);
      return;
    }

  if (!VAR_P (desc))
    {
      tmp = gfc_create_var (TREE_TYPE (desc), "res");
      if (se->direct_byref)
	gfc_add_modify (&se->finalblock, tmp, desc);
      else
	gfc_add_modify (&se->pre, tmp, desc);
      desc = tmp;
    }

  data_ptr = gfc_conv_descriptor_data_get (desc);
  data_null = fold_convert (TREE_TYPE (data_ptr), null_pointer_node);
  cond = fold_build2_loc (input_location, NE_EXPR,
			  logical_type_node, data_ptr, data_null);
  is_final = fold_build2_loc (input_location, TRUTH_AND_EXPR,
			      logical_type_node, is_final, cond);
  tmp = build_call_expr_loc (input_location, final_fndecl, 3,
			     gfc_build_addr_expr (NULL, desc),
			     size, boolean_false_node);
  tmp = fold_build3_loc (input_location, COND_EXPR,
			 void_type_node, is_final, tmp,
			 build_empty_stmt (input_location));

  if (is_class && se->ss && se->ss->loop)
    {
      gfc_add_expr_to_block (&se->loop->post, tmp);
      tmp = fold_build3_loc (input_location, COND_EXPR,
			     void_type_node, cond,
			     gfc_call_free (data_ptr),
			     build_empty_stmt (input_location));
      gfc_add_expr_to_block (&se->loop->post, tmp);
      gfc_add_modify (&se->loop->post, data_ptr, data_null);
    }
  else
    {
      gfc_add_expr_to_block (&se->finalblock, tmp);

      /* Let the scalarizer take care of freeing of temporary arrays.  */
      if (attr.allocatable && !(se->loop && se->loop->temp_dim))
	{
	  tmp = fold_build3_loc (input_location, COND_EXPR,
				 void_type_node, cond,
				 gfc_call_free (data_ptr),
				 build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&se->finalblock, tmp);
	  gfc_add_modify (&se->finalblock, data_ptr, data_null);
	}
    }
}


/* User-deallocate; we emit the code directly from the front-end, and the
   logic is the same as the previous library function:

    void
    deallocate (void *pointer, GFC_INTEGER_4 * stat)
    {
      if (!pointer)
	{
	  if (stat)
	    *stat = 1;
	  else
	    runtime_error ("Attempt to DEALLOCATE unallocated memory.");
	}
      else
	{
	  free (pointer);
	  if (stat)
	    *stat = 0;
	}
    }

   In this front-end version, status doesn't have to be GFC_INTEGER_4.
   Moreover, if CAN_FAIL is true, then we will not emit a runtime error,
   even when no status variable is passed to us (this is used for
   unconditional deallocation generated by the front-end at end of
   each procedure).

   If a runtime-message is possible, `expr' must point to the original
   expression being deallocated for its locus and variable name.

   For coarrays, "pointer" must be the array descriptor and not its
   "data" component.

   COARRAY_DEALLOC_MODE gives the mode unregister coarrays.  Available modes are
   the ones of GFC_CAF_DEREGTYPE, -1 when the mode for deregistration is to be
   analyzed and set by this routine, and -2 to indicate that a non-coarray is to
   be deallocated.  */
tree
gfc_deallocate_with_status (tree pointer, tree status, tree errmsg,
			    tree errlen, tree label_finish,
			    bool can_fail, gfc_expr* expr,
			    int coarray_dealloc_mode, tree class_container,
			    tree add_when_allocated, tree caf_token)
{
  stmtblock_t null, non_null;
  tree cond, tmp, error;
  tree status_type = NULL_TREE;
  tree token = NULL_TREE;
  tree descr = NULL_TREE;
  gfc_coarray_deregtype caf_dereg_type = GFC_CAF_COARRAY_DEREGISTER;

  if (coarray_dealloc_mode >= GFC_CAF_COARRAY_ANALYZE)
    {
      if (flag_coarray == GFC_FCOARRAY_LIB)
	{
	  if (caf_token)
	    {
	      token = caf_token;
	      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (pointer)))
		pointer = gfc_conv_descriptor_data_get (pointer);
	    }
	  else
	    {
	      tree caf_type, caf_decl = pointer;
	      pointer = gfc_conv_descriptor_data_get (caf_decl);
	      caf_type = TREE_TYPE (caf_decl);
	      STRIP_NOPS (pointer);
	      if (GFC_DESCRIPTOR_TYPE_P (caf_type))
		token = gfc_conv_descriptor_token (caf_decl);
	      else if (DECL_LANG_SPECIFIC (caf_decl)
		       && GFC_DECL_TOKEN (caf_decl) != NULL_TREE)
		token = GFC_DECL_TOKEN (caf_decl);
	      else
		{
		  gcc_assert (GFC_ARRAY_TYPE_P (caf_type)
			      && GFC_TYPE_ARRAY_CAF_TOKEN (caf_type)
				 != NULL_TREE);
		  token = GFC_TYPE_ARRAY_CAF_TOKEN (caf_type);
		}
	    }

	  if (coarray_dealloc_mode == GFC_CAF_COARRAY_ANALYZE)
	    {
	      bool comp_ref;
	      if (expr && !gfc_caf_attr (expr, false, &comp_ref).coarray_comp
		  && comp_ref)
		caf_dereg_type = GFC_CAF_COARRAY_DEALLOCATE_ONLY;
	      // else do a deregister as set by default.
	    }
	  else
	    caf_dereg_type = (enum gfc_coarray_deregtype) coarray_dealloc_mode;
	}
      else if (flag_coarray == GFC_FCOARRAY_SINGLE
	       && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (pointer)))
	pointer = gfc_conv_descriptor_data_get (pointer);
    }
  else if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (pointer)))
    {
      descr = pointer;
      pointer = gfc_conv_descriptor_data_get (pointer);
    }

  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, pointer,
			  build_int_cst (TREE_TYPE (pointer), 0));

  /* When POINTER is NULL, we set STATUS to 1 if it's present, otherwise
     we emit a runtime error.  */
  gfc_start_block (&null);
  if (!can_fail)
    {
      tree varname;

      gcc_assert (expr && expr->expr_type == EXPR_VARIABLE && expr->symtree);

      varname = gfc_build_cstring_const (expr->symtree->name);
      varname = gfc_build_addr_expr (pchar_type_node, varname);

      error = gfc_trans_runtime_error (true, &expr->where,
				       "Attempt to DEALLOCATE unallocated '%s'",
				       varname);
    }
  else
    error = build_empty_stmt (input_location);

  if (status != NULL_TREE && !integer_zerop (status))
    {
      tree cond2;

      status_type = TREE_TYPE (TREE_TYPE (status));
      cond2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			       status, build_int_cst (TREE_TYPE (status), 0));
      tmp = fold_build2_loc (input_location, MODIFY_EXPR, status_type,
			     fold_build1_loc (input_location, INDIRECT_REF,
					      status_type, status),
			     build_int_cst (status_type, 1));
      error = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			       cond2, tmp, error);
    }

  gfc_add_expr_to_block (&null, error);

  /* When POINTER is not NULL, we free it.  */
  gfc_start_block (&non_null);
  if (add_when_allocated)
    gfc_add_expr_to_block (&non_null, add_when_allocated);
  gfc_add_finalizer_call (&non_null, expr, class_container);
  if (coarray_dealloc_mode == GFC_CAF_COARRAY_NOCOARRAY
      || flag_coarray != GFC_FCOARRAY_LIB)
    {
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_FREE), 1,
				 fold_convert (pvoid_type_node, pointer));
      if (flag_openmp_allocators && coarray_dealloc_mode < GFC_CAF_COARRAY_ANALYZE)
	{
	  tree cond, omp_tmp;
	  if (descr)
	    cond = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				    gfc_conv_descriptor_version (descr),
				    integer_one_node);
	  else
	    cond = gfc_omp_call_is_alloc (pointer);
	  omp_tmp = builtin_decl_explicit (BUILT_IN_GOMP_FREE);
	  omp_tmp = build_call_expr_loc (input_location, omp_tmp, 2, pointer,
					 build_zero_cst (ptr_type_node));
	  tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp), cond,
			    omp_tmp, tmp);
	}
      gfc_add_expr_to_block (&non_null, tmp);
      gfc_add_modify (&non_null, pointer, build_int_cst (TREE_TYPE (pointer),
							 0));
      if (flag_openmp_allocators && descr)
	gfc_add_modify (&non_null, gfc_conv_descriptor_version (descr),
			integer_zero_node);

      if (status != NULL_TREE && !integer_zerop (status))
	{
	  /* We set STATUS to zero if it is present.  */
	  tree status_type = TREE_TYPE (TREE_TYPE (status));
	  tree cond2;

	  cond2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				   status,
				   build_int_cst (TREE_TYPE (status), 0));
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR, status_type,
				 fold_build1_loc (input_location, INDIRECT_REF,
						  status_type, status),
				 build_int_cst (status_type, 0));
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 gfc_unlikely (cond2, PRED_FORTRAN_FAIL_ALLOC),
				 tmp, build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&non_null, tmp);
	}
    }
  else
    {
      tree cond2, pstat = null_pointer_node;

      if (errmsg == NULL_TREE)
	{
	  gcc_assert (errlen == NULL_TREE);
	  errmsg = null_pointer_node;
	  errlen = integer_zero_node;
	}
      else
	{
	  gcc_assert (errlen != NULL_TREE);
	  if (!POINTER_TYPE_P (TREE_TYPE (errmsg)))
	    errmsg = gfc_build_addr_expr (NULL_TREE, errmsg);
	}

      if (status != NULL_TREE && !integer_zerop (status))
	{
	  gcc_assert (status_type == integer_type_node);
	  pstat = status;
	}

      token = gfc_build_addr_expr  (NULL_TREE, token);
      gcc_assert (caf_dereg_type > GFC_CAF_COARRAY_ANALYZE);
      tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_caf_deregister, 5,
				 token, build_int_cst (integer_type_node,
						       caf_dereg_type),
				 pstat, errmsg, errlen);
      gfc_add_expr_to_block (&non_null, tmp);

      /* It guarantees memory consistency within the same segment */
      tmp = gfc_build_string_const (strlen ("memory")+1, "memory"),
      tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
			gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
			tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
      ASM_VOLATILE_P (tmp) = 1;
      gfc_add_expr_to_block (&non_null, tmp);

      if (status != NULL_TREE)
	{
	  tree stat = build_fold_indirect_ref_loc (input_location, status);
	  tree nullify = fold_build2_loc (input_location, MODIFY_EXPR,
					  void_type_node, pointer,
					  build_int_cst (TREE_TYPE (pointer),
							 0));

	  TREE_USED (label_finish) = 1;
	  tmp = build1_v (GOTO_EXPR, label_finish);
	  cond2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				   stat, build_zero_cst (TREE_TYPE (stat)));
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 gfc_unlikely (cond2, PRED_FORTRAN_REALLOC),
				 tmp, nullify);
	  gfc_add_expr_to_block (&non_null, tmp);
	}
      else
	gfc_add_modify (&non_null, pointer, build_int_cst (TREE_TYPE (pointer),
							   0));
    }

  return fold_build3_loc (input_location, COND_EXPR, void_type_node, cond,
			  gfc_finish_block (&null),
			  gfc_finish_block (&non_null));
}


/* Generate code for deallocation of allocatable scalars (variables or
   components). Before the object itself is freed, any allocatable
   subcomponents are being deallocated.  */

tree
gfc_deallocate_scalar_with_status (tree pointer, tree status, tree label_finish,
				   bool can_fail, gfc_expr* expr,
				   gfc_typespec ts, tree class_container,
				   bool coarray)
{
  stmtblock_t null, non_null;
  tree cond, tmp, error;
  bool finalizable, comp_ref;
  gfc_coarray_deregtype caf_dereg_type = GFC_CAF_COARRAY_DEREGISTER;

  if (coarray && expr && !gfc_caf_attr (expr, false, &comp_ref).coarray_comp
      && comp_ref)
    caf_dereg_type = GFC_CAF_COARRAY_DEALLOCATE_ONLY;

  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, pointer,
			  build_int_cst (TREE_TYPE (pointer), 0));

  /* When POINTER is NULL, we set STATUS to 1 if it's present, otherwise
     we emit a runtime error.  */
  gfc_start_block (&null);
  if (!can_fail)
    {
      tree varname;

      gcc_assert (expr && expr->expr_type == EXPR_VARIABLE && expr->symtree);

      varname = gfc_build_cstring_const (expr->symtree->name);
      varname = gfc_build_addr_expr (pchar_type_node, varname);

      error = gfc_trans_runtime_error (true, &expr->where,
				       "Attempt to DEALLOCATE unallocated '%s'",
				       varname);
    }
  else
    error = build_empty_stmt (input_location);

  if (status != NULL_TREE && !integer_zerop (status))
    {
      tree status_type = TREE_TYPE (TREE_TYPE (status));
      tree cond2;

      cond2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			       status, build_int_cst (TREE_TYPE (status), 0));
      tmp = fold_build2_loc (input_location, MODIFY_EXPR, status_type,
			     fold_build1_loc (input_location, INDIRECT_REF,
					      status_type, status),
			     build_int_cst (status_type, 1));
      error = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			       cond2, tmp, error);
    }
  gfc_add_expr_to_block (&null, error);

  /* When POINTER is not NULL, we free it.  */
  gfc_start_block (&non_null);

  /* Free allocatable components.  */
  finalizable = gfc_add_finalizer_call (&non_null, expr, class_container);
  if (!finalizable && ts.type == BT_DERIVED && ts.u.derived->attr.alloc_comp)
    {
      int caf_mode = coarray
	  ? ((caf_dereg_type == GFC_CAF_COARRAY_DEALLOCATE_ONLY
	      ? GFC_STRUCTURE_CAF_MODE_DEALLOC_ONLY : 0)
	     | GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY
	     | GFC_STRUCTURE_CAF_MODE_IN_COARRAY)
	  : 0;
      if (coarray && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (pointer)))
	tmp = gfc_conv_descriptor_data_get (pointer);
      else
	tmp = build_fold_indirect_ref_loc (input_location, pointer);
      tmp = gfc_deallocate_alloc_comp (ts.u.derived, tmp, 0, caf_mode);
      gfc_add_expr_to_block (&non_null, tmp);
    }

  if (!coarray || flag_coarray == GFC_FCOARRAY_SINGLE)
    {
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_FREE), 1,
				 fold_convert (pvoid_type_node, pointer));
      if (flag_openmp_allocators)
	{
	  tree cond, omp_tmp;
	  cond = gfc_omp_call_is_alloc (pointer);
	  omp_tmp = builtin_decl_explicit (BUILT_IN_GOMP_FREE);
	  omp_tmp = build_call_expr_loc (input_location, omp_tmp, 2, pointer,
					 build_zero_cst (ptr_type_node));
	  tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp), cond,
			    omp_tmp, tmp);
	}
      gfc_add_expr_to_block (&non_null, tmp);

      if (status != NULL_TREE && !integer_zerop (status))
	{
	  /* We set STATUS to zero if it is present.  */
	  tree status_type = TREE_TYPE (TREE_TYPE (status));
	  tree cond2;

	  cond2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				   status,
				   build_int_cst (TREE_TYPE (status), 0));
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR, status_type,
				 fold_build1_loc (input_location, INDIRECT_REF,
						  status_type, status),
				 build_int_cst (status_type, 0));
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 cond2, tmp, build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&non_null, tmp);
	}
    }
  else
    {
      tree token;
      tree pstat = null_pointer_node;
      gfc_se se;

      gfc_init_se (&se, NULL);
      token = gfc_get_ultimate_alloc_ptr_comps_caf_token (&se, expr);
      gcc_assert (token != NULL_TREE);

      if (status != NULL_TREE && !integer_zerop (status))
	{
	  gcc_assert (TREE_TYPE (TREE_TYPE (status)) == integer_type_node);
	  pstat = status;
	}

      tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_caf_deregister, 5,
				 token, build_int_cst (integer_type_node,
						       caf_dereg_type),
				 pstat, null_pointer_node, integer_zero_node);
      gfc_add_expr_to_block (&non_null, tmp);

      /* It guarantees memory consistency within the same segment.  */
      tmp = gfc_build_string_const (strlen ("memory")+1, "memory");
      tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
			gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
			tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
      ASM_VOLATILE_P (tmp) = 1;
      gfc_add_expr_to_block (&non_null, tmp);

      if (status != NULL_TREE)
	{
	  tree stat = build_fold_indirect_ref_loc (input_location, status);
	  tree cond2;

	  TREE_USED (label_finish) = 1;
	  tmp = build1_v (GOTO_EXPR, label_finish);
	  cond2 = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				   stat, build_zero_cst (TREE_TYPE (stat)));
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 gfc_unlikely (cond2, PRED_FORTRAN_REALLOC),
				 tmp, build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&non_null, tmp);
	}
    }

  return fold_build3_loc (input_location, COND_EXPR, void_type_node, cond,
			  gfc_finish_block (&null),
			  gfc_finish_block (&non_null));
}

/* Reallocate MEM so it has SIZE bytes of data.  This behaves like the
   following pseudo-code:

void *
internal_realloc (void *mem, size_t size)
{
  res = realloc (mem, size);
  if (!res && size != 0)
    _gfortran_os_error ("Allocation would exceed memory limit");

  return res;
}  */
tree
gfc_call_realloc (stmtblock_t * block, tree mem, tree size)
{
  tree res, nonzero, null_result, tmp;
  tree type = TREE_TYPE (mem);

  /* Only evaluate the size once.  */
  size = save_expr (fold_convert (size_type_node, size));

  /* Create a variable to hold the result.  */
  res = gfc_create_var (type, NULL);

  /* Call realloc and check the result.  */
  tmp = build_call_expr_loc (input_location,
			 builtin_decl_explicit (BUILT_IN_REALLOC), 2,
			 fold_convert (pvoid_type_node, mem), size);
  gfc_add_modify (block, res, fold_convert (type, tmp));
  null_result = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				 res, build_int_cst (pvoid_type_node, 0));
  nonzero = fold_build2_loc (input_location, NE_EXPR, logical_type_node, size,
			     build_int_cst (size_type_node, 0));
  null_result = fold_build2_loc (input_location, TRUTH_AND_EXPR, logical_type_node,
				 null_result, nonzero);
  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			 null_result,
			 trans_os_error_at (NULL,
					    "Error reallocating to %lu bytes",
					    fold_convert
					    (long_unsigned_type_node, size)),
			 build_empty_stmt (input_location));
  gfc_add_expr_to_block (block, tmp);

  return res;
}


/* Add an expression to another one, either at the front or the back.  */

static void
add_expr_to_chain (tree* chain, tree expr, bool front)
{
  if (expr == NULL_TREE || IS_EMPTY_STMT (expr))
    return;

  if (*chain)
    {
      if (TREE_CODE (*chain) != STATEMENT_LIST)
	{
	  tree tmp;

	  tmp = *chain;
	  *chain = NULL_TREE;
	  append_to_statement_list (tmp, chain);
	}

      if (front)
	{
	  tree_stmt_iterator i;

	  i = tsi_start (*chain);
	  tsi_link_before (&i, expr, TSI_CONTINUE_LINKING);
	}
      else
	append_to_statement_list (expr, chain);
    }
  else
    *chain = expr;
}


/* Add a statement at the end of a block.  */

void
gfc_add_expr_to_block (stmtblock_t * block, tree expr)
{
  gcc_assert (block);
  add_expr_to_chain (&block->head, expr, false);
}


/* Add a statement at the beginning of a block.  */

void
gfc_prepend_expr_to_block (stmtblock_t * block, tree expr)
{
  gcc_assert (block);
  add_expr_to_chain (&block->head, expr, true);
}


/* Add a block the end of a block.  */

void
gfc_add_block_to_block (stmtblock_t * block, stmtblock_t * append)
{
  gcc_assert (append);
  gcc_assert (!append->has_scope);

  gfc_add_expr_to_block (block, append->head);
  append->head = NULL_TREE;
}


/* Translate an executable statement. The tree cond is used by gfc_trans_do.
   This static function is wrapped by gfc_trans_code_cond and
   gfc_trans_code.  */

static tree
trans_code (gfc_code * code, tree cond)
{
  stmtblock_t block;
  tree res;

  if (!code)
    return build_empty_stmt (input_location);

  gfc_start_block (&block);

  /* Translate statements one by one into GENERIC trees until we reach
     the end of this gfc_code branch.  */
  for (; code; code = code->next)
    {
      if (code->here != 0)
	{
	  res = gfc_trans_label_here (code);
	  gfc_add_expr_to_block (&block, res);
	}

      input_location = gfc_get_location (&code->loc);

      switch (code->op)
	{
	case EXEC_NOP:
	case EXEC_END_BLOCK:
	case EXEC_END_NESTED_BLOCK:
	case EXEC_END_PROCEDURE:
	  res = NULL_TREE;
	  break;

	case EXEC_ASSIGN:
	  res = gfc_trans_assign (code);
	  break;

        case EXEC_LABEL_ASSIGN:
          res = gfc_trans_label_assign (code);
          break;

	case EXEC_POINTER_ASSIGN:
	  res = gfc_trans_pointer_assign (code);
	  break;

	case EXEC_INIT_ASSIGN:
	  if (code->expr1->ts.type == BT_CLASS)
	    res = gfc_trans_class_init_assign (code);
	  else
	    res = gfc_trans_init_assign (code);
	  break;

	case EXEC_CONTINUE:
	  res = NULL_TREE;
	  break;

	case EXEC_CRITICAL:
	  res = gfc_trans_critical (code);
	  break;

	case EXEC_CYCLE:
	  res = gfc_trans_cycle (code);
	  break;

	case EXEC_EXIT:
	  res = gfc_trans_exit (code);
	  break;

	case EXEC_GOTO:
	  res = gfc_trans_goto (code);
	  break;

	case EXEC_ENTRY:
	  res = gfc_trans_entry (code);
	  break;

	case EXEC_PAUSE:
	  res = gfc_trans_pause (code);
	  break;

	case EXEC_STOP:
	case EXEC_ERROR_STOP:
	  res = gfc_trans_stop (code, code->op == EXEC_ERROR_STOP);
	  break;

	case EXEC_CALL:
	  /* For MVBITS we've got the special exception that we need a
	     dependency check, too.  */
	  {
	    bool is_mvbits = false;

	    if (code->resolved_isym)
	      {
		res = gfc_conv_intrinsic_subroutine (code);
		if (res != NULL_TREE)
		  break;
	      }

	    if (code->resolved_isym
		&& code->resolved_isym->id == GFC_ISYM_MVBITS)
	      is_mvbits = true;

	    res = gfc_trans_call (code, is_mvbits, NULL_TREE,
				  NULL_TREE, false);
	  }
	  break;

	case EXEC_CALL_PPC:
	  res = gfc_trans_call (code, false, NULL_TREE,
				NULL_TREE, false);
	  break;

	case EXEC_ASSIGN_CALL:
	  res = gfc_trans_call (code, true, NULL_TREE,
				NULL_TREE, false);
	  break;

	case EXEC_RETURN:
	  res = gfc_trans_return (code);
	  break;

	case EXEC_IF:
	  res = gfc_trans_if (code);
	  break;

	case EXEC_ARITHMETIC_IF:
	  res = gfc_trans_arithmetic_if (code);
	  break;

	case EXEC_BLOCK:
	  res = gfc_trans_block_construct (code);
	  break;

	case EXEC_DO:
	  res = gfc_trans_do (code, cond);
	  break;

	case EXEC_DO_CONCURRENT:
	  res = gfc_trans_do_concurrent (code);
	  break;

	case EXEC_DO_WHILE:
	  res = gfc_trans_do_while (code);
	  break;

	case EXEC_SELECT:
	  res = gfc_trans_select (code);
	  break;

	case EXEC_SELECT_TYPE:
	  res = gfc_trans_select_type (code);
	  break;

	case EXEC_SELECT_RANK:
	  res = gfc_trans_select_rank (code);
	  break;

	case EXEC_FLUSH:
	  res = gfc_trans_flush (code);
	  break;

	case EXEC_SYNC_ALL:
	case EXEC_SYNC_IMAGES:
	case EXEC_SYNC_MEMORY:
	  res = gfc_trans_sync (code, code->op);
	  break;

	case EXEC_LOCK:
	case EXEC_UNLOCK:
	  res = gfc_trans_lock_unlock (code, code->op);
	  break;

	case EXEC_EVENT_POST:
	case EXEC_EVENT_WAIT:
	  res = gfc_trans_event_post_wait (code, code->op);
	  break;

	case EXEC_FAIL_IMAGE:
	  res = gfc_trans_fail_image (code);
	  break;

	case EXEC_FORALL:
	  res = gfc_trans_forall (code);
	  break;

	case EXEC_FORM_TEAM:
	  res = gfc_trans_form_team (code);
	  break;

	case EXEC_CHANGE_TEAM:
	  res = gfc_trans_change_team (code);
	  break;

	case EXEC_END_TEAM:
	  res = gfc_trans_end_team (code);
	  break;

	case EXEC_SYNC_TEAM:
	  res = gfc_trans_sync_team (code);
	  break;

	case EXEC_WHERE:
	  res = gfc_trans_where (code);
	  break;

	case EXEC_ALLOCATE:
	  res = gfc_trans_allocate (code, NULL);
	  break;

	case EXEC_DEALLOCATE:
	  res = gfc_trans_deallocate (code);
	  break;

	case EXEC_OPEN:
	  res = gfc_trans_open (code);
	  break;

	case EXEC_CLOSE:
	  res = gfc_trans_close (code);
	  break;

	case EXEC_READ:
	  res = gfc_trans_read (code);
	  break;

	case EXEC_WRITE:
	  res = gfc_trans_write (code);
	  break;

	case EXEC_IOLENGTH:
	  res = gfc_trans_iolength (code);
	  break;

	case EXEC_BACKSPACE:
	  res = gfc_trans_backspace (code);
	  break;

	case EXEC_ENDFILE:
	  res = gfc_trans_endfile (code);
	  break;

	case EXEC_INQUIRE:
	  res = gfc_trans_inquire (code);
	  break;

	case EXEC_WAIT:
	  res = gfc_trans_wait (code);
	  break;

	case EXEC_REWIND:
	  res = gfc_trans_rewind (code);
	  break;

	case EXEC_TRANSFER:
	  res = gfc_trans_transfer (code);
	  break;

	case EXEC_DT_END:
	  res = gfc_trans_dt_end (code);
	  break;

	case EXEC_OMP_ALLOCATE:
	case EXEC_OMP_ALLOCATORS:
	case EXEC_OMP_ASSUME:
	case EXEC_OMP_ATOMIC:
	case EXEC_OMP_BARRIER:
	case EXEC_OMP_CANCEL:
	case EXEC_OMP_CANCELLATION_POINT:
	case EXEC_OMP_CRITICAL:
	case EXEC_OMP_DEPOBJ:
	case EXEC_OMP_DISTRIBUTE:
	case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_DISTRIBUTE_SIMD:
	case EXEC_OMP_DO:
	case EXEC_OMP_DO_SIMD:
	case EXEC_OMP_ERROR:
	case EXEC_OMP_FLUSH:
	case EXEC_OMP_INTEROP:
	case EXEC_OMP_LOOP:
	case EXEC_OMP_MASKED:
	case EXEC_OMP_MASKED_TASKLOOP:
	case EXEC_OMP_MASKED_TASKLOOP_SIMD:
	case EXEC_OMP_MASTER:
	case EXEC_OMP_MASTER_TASKLOOP:
	case EXEC_OMP_MASTER_TASKLOOP_SIMD:
	case EXEC_OMP_ORDERED:
	case EXEC_OMP_PARALLEL:
	case EXEC_OMP_PARALLEL_DO:
	case EXEC_OMP_PARALLEL_DO_SIMD:
	case EXEC_OMP_PARALLEL_LOOP:
	case EXEC_OMP_PARALLEL_MASKED:
	case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
	case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
	case EXEC_OMP_PARALLEL_MASTER:
	case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
	case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
	case EXEC_OMP_PARALLEL_SECTIONS:
	case EXEC_OMP_PARALLEL_WORKSHARE:
	case EXEC_OMP_SCOPE:
	case EXEC_OMP_SECTIONS:
	case EXEC_OMP_SIMD:
	case EXEC_OMP_SINGLE:
	case EXEC_OMP_TARGET:
	case EXEC_OMP_TARGET_DATA:
	case EXEC_OMP_TARGET_ENTER_DATA:
	case EXEC_OMP_TARGET_EXIT_DATA:
	case EXEC_OMP_TARGET_PARALLEL:
	case EXEC_OMP_TARGET_PARALLEL_DO:
	case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
	case EXEC_OMP_TARGET_PARALLEL_LOOP:
	case EXEC_OMP_TARGET_SIMD:
	case EXEC_OMP_TARGET_TEAMS:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
	case EXEC_OMP_TARGET_TEAMS_LOOP:
	case EXEC_OMP_TARGET_UPDATE:
	case EXEC_OMP_TASK:
	case EXEC_OMP_TASKGROUP:
	case EXEC_OMP_TASKLOOP:
	case EXEC_OMP_TASKLOOP_SIMD:
	case EXEC_OMP_TASKWAIT:
	case EXEC_OMP_TASKYIELD:
	case EXEC_OMP_TEAMS:
	case EXEC_OMP_TEAMS_DISTRIBUTE:
	case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
	case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
	case EXEC_OMP_TEAMS_LOOP:
	case EXEC_OMP_TILE:
	case EXEC_OMP_UNROLL:
	case EXEC_OMP_WORKSHARE:
	  res = gfc_trans_omp_directive (code);
	  break;

	case EXEC_OACC_CACHE:
	case EXEC_OACC_WAIT:
	case EXEC_OACC_UPDATE:
	case EXEC_OACC_LOOP:
	case EXEC_OACC_HOST_DATA:
	case EXEC_OACC_DATA:
	case EXEC_OACC_KERNELS:
	case EXEC_OACC_KERNELS_LOOP:
	case EXEC_OACC_PARALLEL:
	case EXEC_OACC_PARALLEL_LOOP:
	case EXEC_OACC_SERIAL:
	case EXEC_OACC_SERIAL_LOOP:
	case EXEC_OACC_ENTER_DATA:
	case EXEC_OACC_EXIT_DATA:
	case EXEC_OACC_ATOMIC:
	case EXEC_OACC_DECLARE:
	  res = gfc_trans_oacc_directive (code);
	  break;

	default:
	  gfc_internal_error ("gfc_trans_code(): Bad statement code");
	}

      input_location = gfc_get_location (&code->loc);

      if (res != NULL_TREE && ! IS_EMPTY_STMT (res))
	{
	  if (TREE_CODE (res) != STATEMENT_LIST)
	    SET_EXPR_LOCATION (res, input_location);

	  /* Add the new statement to the block.  */
	  gfc_add_expr_to_block (&block, res);
	}
    }

  /* Return the finished block.  */
  return gfc_finish_block (&block);
}


/* Translate an executable statement with condition, cond.  The condition is
   used by gfc_trans_do to test for IO result conditions inside implied
   DO loops of READ and WRITE statements.  See build_dt in trans-io.cc.  */

tree
gfc_trans_code_cond (gfc_code * code, tree cond)
{
  return trans_code (code, cond);
}

/* Translate an executable statement without condition.  */

tree
gfc_trans_code (gfc_code * code)
{
  return trans_code (code, NULL_TREE);
}


/* This function is called after a complete program unit has been parsed
   and resolved.  */

void
gfc_generate_code (gfc_namespace * ns)
{
  ompws_flags = 0;
  if (ns->is_block_data)
    {
      gfc_generate_block_data (ns);
      return;
    }

  gfc_generate_function_code (ns);
}


/* This function is called after a complete module has been parsed
   and resolved.  */

void
gfc_generate_module_code (gfc_namespace * ns)
{
  gfc_namespace *n;
  struct module_htab_entry *entry;

  gcc_assert (ns->proc_name->backend_decl == NULL);
  ns->proc_name->backend_decl
    = build_decl (gfc_get_location (&ns->proc_name->declared_at),
		  NAMESPACE_DECL, get_identifier (ns->proc_name->name),
		  void_type_node);
  entry = gfc_find_module (ns->proc_name->name);
  if (entry->namespace_decl)
    /* Buggy sourcecode, using a module before defining it?  */
    entry->decls->empty ();
  entry->namespace_decl = ns->proc_name->backend_decl;

  gfc_generate_module_vars (ns);

  /* We need to generate all module function prototypes first, to allow
     sibling calls.  */
  for (n = ns->contained; n; n = n->sibling)
    {
      gfc_entry_list *el;

      if (!n->proc_name)
        continue;

      gfc_create_function_decl (n, false);
      DECL_CONTEXT (n->proc_name->backend_decl) = ns->proc_name->backend_decl;
      gfc_module_add_decl (entry, n->proc_name->backend_decl);
      for (el = ns->entries; el; el = el->next)
	{
	  DECL_CONTEXT (el->sym->backend_decl) = ns->proc_name->backend_decl;
	  gfc_module_add_decl (entry, el->sym->backend_decl);
	}
    }

  for (n = ns->contained; n; n = n->sibling)
    {
      if (!n->proc_name)
        continue;

      gfc_generate_function_code (n);
    }
}


/* Initialize an init/cleanup block with existing code.  */

void
gfc_start_wrapped_block (gfc_wrapped_block* block, tree code)
{
  gcc_assert (block);

  block->init = NULL_TREE;
  block->code = code;
  block->cleanup = NULL_TREE;
}


/* Add a new pair of initializers/clean-up code.  */

void
gfc_add_init_cleanup (gfc_wrapped_block* block, tree init, tree cleanup,
		      bool back)
{
  gcc_assert (block);

  /* The new pair of init/cleanup should be "wrapped around" the existing
     block of code, thus the initialization is added to the front and the
     cleanup to the back.  */
  add_expr_to_chain (&block->init, init, !back);
  add_expr_to_chain (&block->cleanup, cleanup, false);
}


/* Finish up a wrapped block by building a corresponding try-finally expr.  */

tree
gfc_finish_wrapped_block (gfc_wrapped_block* block)
{
  tree result;

  gcc_assert (block);

  /* Build the final expression.  For this, just add init and body together,
     and put clean-up with that into a TRY_FINALLY_EXPR.  */
  result = block->init;
  add_expr_to_chain (&result, block->code, false);
  if (block->cleanup)
    result = build2_loc (input_location, TRY_FINALLY_EXPR, void_type_node,
			 result, block->cleanup);

  /* Clear the block.  */
  block->init = NULL_TREE;
  block->code = NULL_TREE;
  block->cleanup = NULL_TREE;

  return result;
}


/* Helper function for marking a boolean expression tree as unlikely.  */

tree
gfc_unlikely (tree cond, enum br_predictor predictor)
{
  tree tmp;

  if (optimize)
    {
      cond = fold_convert (long_integer_type_node, cond);
      tmp = build_zero_cst (long_integer_type_node);
      cond = build_call_expr_loc (input_location,
				  builtin_decl_explicit (BUILT_IN_EXPECT),
				  3, cond, tmp,
				  build_int_cst (integer_type_node,
						 predictor));
    }
  return cond;
}


/* Helper function for marking a boolean expression tree as likely.  */

tree
gfc_likely (tree cond, enum br_predictor predictor)
{
  tree tmp;

  if (optimize)
    {
      cond = fold_convert (long_integer_type_node, cond);
      tmp = build_one_cst (long_integer_type_node);
      cond = build_call_expr_loc (input_location,
				  builtin_decl_explicit (BUILT_IN_EXPECT),
				  3, cond, tmp,
				  build_int_cst (integer_type_node,
						 predictor));
    }
  return cond;
}


/* Get the string length for a deferred character length component.  */

bool
gfc_deferred_strlen (gfc_component *c, tree *decl)
{
  char name[GFC_MAX_SYMBOL_LEN+9];
  gfc_component *strlen;
  if (!(c->ts.type == BT_CHARACTER
	&& (c->ts.deferred || c->attr.pdt_string)))
    return false;
  sprintf (name, "_%s_length", c->name);
  for (strlen = c; strlen; strlen = strlen->next)
    if (strcmp (strlen->name, name) == 0)
      break;
  *decl = strlen ? strlen->backend_decl : NULL_TREE;
  return strlen != NULL;
}
