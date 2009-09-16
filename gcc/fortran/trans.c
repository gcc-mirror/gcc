/* Code translation -- generate GCC trees from gfc_code.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free
   Software Foundation, Inc.
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
#include "tree.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "ggc.h"
#include "toplev.h"
#include "defaults.h"
#include "real.h"
#include "flags.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"
#include "trans-const.h"

/* Naming convention for backend interface code:

   gfc_trans_*	translate gfc_code into STMT trees.

   gfc_conv_*	expression conversion

   gfc_get_*	get a backend tree representation of a decl or type  */

static gfc_file *gfc_current_backend_file;

const char gfc_msg_bounds[] = N_("Array bound mismatch");
const char gfc_msg_fault[] = N_("Array reference out of bounds");
const char gfc_msg_wrong_return[] = N_("Incorrect function return value");


/* Advance along TREE_CHAIN n times.  */

tree
gfc_advance_chain (tree t, int n)
{
  for (; n > 0; n--)
    {
      gcc_assert (t != NULL_TREE);
      t = TREE_CHAIN (t);
    }
  return t;
}


/* Wrap a node in a TREE_LIST node and add it to the end of a list.  */

tree
gfc_chainon_list (tree list, tree add)
{
  tree l;

  l = tree_cons (NULL_TREE, add, NULL_TREE);

  return chainon (list, l);
}


/* Strip off a legitimate source ending from the input
   string NAME of length LEN.  */

static inline void
remove_suffix (char *name, int len)
{
  int i;

  for (i = 2; i < 8 && len > i; i++)
    {
      if (name[len - i] == '.')
	{
	  name[len - i] = '\0';
	  break;
	}
    }
}


/* Creates a variable declaration with a given TYPE.  */

tree
gfc_create_var_np (tree type, const char *prefix)
{
  tree t;
  
  t = create_tmp_var_raw (type, prefix);

  /* No warnings for anonymous variables.  */
  if (prefix == NULL)
    TREE_NO_WARNING (t) = 1;

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
gfc_evaluate_now (tree expr, stmtblock_t * pblock)
{
  tree var;

  if (CONSTANT_CLASS_P (expr))
    return expr;

  var = gfc_create_var (TREE_TYPE (expr), NULL);
  gfc_add_modify (pblock, var, expr);

  return var;
}


/* Build a MODIFY_EXPR node and add it to a given statement block PBLOCK.  
   A MODIFY_EXPR is an assignment:
   LHS <- RHS.  */

void
gfc_add_modify (stmtblock_t * pblock, tree lhs, tree rhs)
{
  tree tmp;

#ifdef ENABLE_CHECKING
  tree t1, t2;
  t1 = TREE_TYPE (rhs);
  t2 = TREE_TYPE (lhs);
  /* Make sure that the types of the rhs and the lhs are the same
     for scalar assignments.  We should probably have something
     similar for aggregates, but right now removing that check just
     breaks everything.  */
  gcc_assert (t1 == t2
	      || AGGREGATE_TYPE_P (TREE_TYPE (lhs)));
#endif

  tmp = fold_build2 (MODIFY_EXPR, void_type_node, lhs, rhs);
  gfc_add_expr_to_block (pblock, tmp);
}


/* Create a new scope/binding level and initialize a block.  Care must be
   taken when translating expressions as any temporaries will be placed in
   the innermost scope.  */

void
gfc_start_block (stmtblock_t * block)
{
  /* Start a new binding level.  */
  pushlevel (0);
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
  poplevel (0, 0, 0);

  /* Add them to the parent scope.  */
  while (decl != NULL_TREE)
    {
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = NULL_TREE;

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
	  block = poplevel (1, 0, 0);
	  expr = build3_v (BIND_EXPR, decl, expr, block);
	}
      else
	poplevel (0, 0, 0);
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
      t = fold (build4 (ARRAY_REF, TREE_TYPE (type),
			t, min_val, NULL_TREE, NULL_TREE));
      natural_type = type;
    }
  else
    natural_type = build_pointer_type (base_type);

  if (TREE_CODE (t) == INDIRECT_REF)
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
      t = fold_build1 (ADDR_EXPR, natural_type, t);
    }

  if (type && natural_type != type)
    t = convert (type, t);

  return t;
}


/* Build an ARRAY_REF with its natural type.  */

tree
gfc_build_array_ref (tree base, tree offset, tree decl)
{
  tree type = TREE_TYPE (base);
  tree tmp;

  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
  type = TREE_TYPE (type);

  if (DECL_P (base))
    TREE_ADDRESSABLE (base) = 1;

  /* Strip NON_LVALUE_EXPR nodes.  */
  STRIP_TYPE_NOPS (offset);

  /* If the array reference is to a pointer, whose target contains a
     subreference, use the span that is stored with the backend decl
     and reference the element with pointer arithmetic.  */
  if (decl && (TREE_CODE (decl) == FIELD_DECL
		 || TREE_CODE (decl) == VAR_DECL
		 || TREE_CODE (decl) == PARM_DECL)
	&& GFC_DECL_SUBREF_ARRAY_P (decl)
	&& !integer_zerop (GFC_DECL_SPAN(decl)))
    {
      offset = fold_build2 (MULT_EXPR, gfc_array_index_type,
			    offset, GFC_DECL_SPAN(decl));
      tmp = gfc_build_addr_expr (pvoid_type_node, base);
      tmp = fold_build2 (POINTER_PLUS_EXPR, pvoid_type_node,
			 tmp, fold_convert (sizetype, offset));
      tmp = fold_convert (build_pointer_type (type), tmp);
      if (!TYPE_STRING_FLAG (type))
	tmp = build_fold_indirect_ref_loc (input_location, tmp);
      return tmp;
    }
  else
    /* Otherwise use a straightforward array reference.  */
    return build4 (ARRAY_REF, type, base, offset, NULL_TREE, NULL_TREE);
}


/* Generate a call to print a runtime error possibly including multiple
   arguments and a locus.  */

tree
gfc_trans_runtime_error (bool error, locus* where, const char* msgid, ...)
{
  va_list ap;

  va_start (ap, msgid);
  return gfc_trans_runtime_error_vararg (error, where, msgid, ap);
}

tree
gfc_trans_runtime_error_vararg (bool error, locus* where, const char* msgid,
				va_list ap)
{
  stmtblock_t block;
  tree tmp;
  tree arg, arg2;
  tree *argarray;
  tree fntype;
  char *message;
  const char *p;
  int line, nargs, i;

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
      line = LOCATION_LINE (where->lb->location);
      asprintf (&message, "At line %d of file %s",  line,
		where->lb->file->filename);
    }
  else
    asprintf (&message, "In file '%s', around line %d",
	      gfc_source_file, input_line + 1);

  arg = gfc_build_addr_expr (pchar_type_node,
			     gfc_build_localized_cstring_const (message));
  gfc_free(message);
  
  asprintf (&message, "%s", _(msgid));
  arg2 = gfc_build_addr_expr (pchar_type_node,
			      gfc_build_localized_cstring_const (message));
  gfc_free(message);

  /* Build the argument array.  */
  argarray = (tree *) alloca (sizeof (tree) * (nargs + 2));
  argarray[0] = arg;
  argarray[1] = arg2;
  for (i = 0; i < nargs; i++)
    argarray[2 + i] = va_arg (ap, tree);
  va_end (ap);
  
  /* Build the function call to runtime_(warning,error)_at; because of the
     variable number of arguments, we can't use build_call_expr_loc dinput_location,
     irectly.  */
  if (error)
    fntype = TREE_TYPE (gfor_fndecl_runtime_error_at);
  else
    fntype = TREE_TYPE (gfor_fndecl_runtime_warning_at);

  tmp = fold_builtin_call_array (input_location, TREE_TYPE (fntype),
				 fold_build1 (ADDR_EXPR,
					      build_pointer_type (fntype),
					      error
					      ? gfor_fndecl_runtime_error_at
					      : gfor_fndecl_runtime_warning_at),
				 nargs + 2, argarray);
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
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

  /* The code to generate the error.  */
  va_start (ap, msgid);
  gfc_add_expr_to_block (&block,
			 gfc_trans_runtime_error_vararg (error, where,
							 msgid, ap));

  if (once)
    gfc_add_modify (&block, tmpvar, boolean_false_node);

  body = gfc_finish_block (&block);

  if (integer_onep (cond))
    {
      gfc_add_expr_to_block (pblock, body);
    }
  else
    {
      /* Tell the compiler that this isn't likely.  */
      if (once)
	cond = fold_build2 (TRUTH_AND_EXPR, long_integer_type_node, tmpvar,
			    cond);
      else
	cond = fold_convert (long_integer_type_node, cond);

      tmp = build_int_cst (long_integer_type_node, 0);
      cond = build_call_expr_loc (input_location,
			      built_in_decls[BUILT_IN_EXPECT], 2, cond, tmp);
      cond = fold_convert (boolean_type_node, cond);

      tmp = build3_v (COND_EXPR, cond, body, build_empty_stmt (input_location));
      gfc_add_expr_to_block (pblock, tmp);
    }
}


/* Call malloc to allocate size bytes of memory, with special conditions:
      + if size < 0, generate a runtime error,
      + if size == 0, return a malloced area of size 1,
      + if malloc returns NULL, issue a runtime error.  */
tree
gfc_call_malloc (stmtblock_t * block, tree type, tree size)
{
  tree tmp, msg, negative, malloc_result, null_result, res;
  stmtblock_t block2;

  size = gfc_evaluate_now (size, block);

  if (TREE_TYPE (size) != TREE_TYPE (size_type_node))
    size = fold_convert (size_type_node, size);

  /* Create a variable to hold the result.  */
  res = gfc_create_var (prvoid_type_node, NULL);

  /* size < 0 ?  */
  negative = fold_build2 (LT_EXPR, boolean_type_node, size,
			  build_int_cst (size_type_node, 0));
  msg = gfc_build_addr_expr (pchar_type_node, gfc_build_localized_cstring_const
      ("Attempt to allocate a negative amount of memory."));
  tmp = fold_build3 (COND_EXPR, void_type_node, negative,
		     build_call_expr_loc (input_location,
				      gfor_fndecl_runtime_error, 1, msg),
		     build_empty_stmt (input_location));
  gfc_add_expr_to_block (block, tmp);

  /* Call malloc and check the result.  */
  gfc_start_block (&block2);

  size = fold_build2 (MAX_EXPR, size_type_node, size,
		      build_int_cst (size_type_node, 1));

  gfc_add_modify (&block2, res,
		  fold_convert (prvoid_type_node,
				build_call_expr_loc (input_location,
				   built_in_decls[BUILT_IN_MALLOC], 1, size)));
  null_result = fold_build2 (EQ_EXPR, boolean_type_node, res,
			     build_int_cst (pvoid_type_node, 0));
  msg = gfc_build_addr_expr (pchar_type_node, gfc_build_localized_cstring_const
      ("Memory allocation failed"));
  tmp = fold_build3 (COND_EXPR, void_type_node, null_result,
		     build_call_expr_loc (input_location,
				      gfor_fndecl_os_error, 1, msg),
		     build_empty_stmt (input_location));
  gfc_add_expr_to_block (&block2, tmp);
  malloc_result = gfc_finish_block (&block2);

  gfc_add_expr_to_block (block, malloc_result);

  if (type != NULL)
    res = fold_convert (type, res);
  return res;
}

/* Allocate memory, using an optional status argument.
 
   This function follows the following pseudo-code:

    void *
    allocate (size_t size, integer_type* stat)
    {
      void *newmem;
    
      if (stat)
	*stat = 0;

      // The only time this can happen is the size wraps around.
      if (size < 0)
      {
	if (stat)
	{
	  *stat = LIBERROR_ALLOCATION;
	  newmem = NULL;
	}
	else
	  runtime_error ("Attempt to allocate negative amount of memory. "
			 "Possible integer overflow");
      }
      else
      {
	newmem = malloc (MAX (size, 1));
	if (newmem == NULL)
	{
	  if (stat)
	    *stat = LIBERROR_ALLOCATION;
	  else
	    runtime_error ("Out of memory");
	}
      }

      return newmem;
    }  */
tree
gfc_allocate_with_status (stmtblock_t * block, tree size, tree status)
{
  stmtblock_t alloc_block;
  tree res, tmp, error, msg, cond;
  tree status_type = status ? TREE_TYPE (TREE_TYPE (status)) : NULL_TREE;

  /* Evaluate size only once, and make sure it has the right type.  */
  size = gfc_evaluate_now (size, block);
  if (TREE_TYPE (size) != TREE_TYPE (size_type_node))
    size = fold_convert (size_type_node, size);

  /* Create a variable to hold the result.  */
  res = gfc_create_var (prvoid_type_node, NULL);

  /* Set the optional status variable to zero.  */
  if (status != NULL_TREE && !integer_zerop (status))
    {
      tmp = fold_build2 (MODIFY_EXPR, status_type,
			 fold_build1 (INDIRECT_REF, status_type, status),
			 build_int_cst (status_type, 0));
      tmp = fold_build3 (COND_EXPR, void_type_node,
			 fold_build2 (NE_EXPR, boolean_type_node, status,
				      build_int_cst (TREE_TYPE (status), 0)),
			 tmp, build_empty_stmt (input_location));
      gfc_add_expr_to_block (block, tmp);
    }

  /* Generate the block of code handling (size < 0).  */
  msg = gfc_build_addr_expr (pchar_type_node, gfc_build_localized_cstring_const
			("Attempt to allocate negative amount of memory. "
			 "Possible integer overflow"));
  error = build_call_expr_loc (input_location,
			   gfor_fndecl_runtime_error, 1, msg);

  if (status != NULL_TREE && !integer_zerop (status))
    {
      /* Set the status variable if it's present.  */
      stmtblock_t set_status_block;

      gfc_start_block (&set_status_block);
      gfc_add_modify (&set_status_block,
		      fold_build1 (INDIRECT_REF, status_type, status),
			   build_int_cst (status_type, LIBERROR_ALLOCATION));
      gfc_add_modify (&set_status_block, res,
			   build_int_cst (prvoid_type_node, 0));

      tmp = fold_build2 (EQ_EXPR, boolean_type_node, status,
			 build_int_cst (TREE_TYPE (status), 0));
      error = fold_build3 (COND_EXPR, void_type_node, tmp, error,
			   gfc_finish_block (&set_status_block));
    }

  /* The allocation itself.  */
  gfc_start_block (&alloc_block);
  gfc_add_modify (&alloc_block, res,
		  fold_convert (prvoid_type_node,
				build_call_expr_loc (input_location,
				   built_in_decls[BUILT_IN_MALLOC], 1,
					fold_build2 (MAX_EXPR, size_type_node,
						     size,
						     build_int_cst (size_type_node, 1)))));

  msg = gfc_build_addr_expr (pchar_type_node, gfc_build_localized_cstring_const
						("Out of memory"));
  tmp = build_call_expr_loc (input_location,
			 gfor_fndecl_os_error, 1, msg);

  if (status != NULL_TREE && !integer_zerop (status))
    {
      /* Set the status variable if it's present.  */
      tree tmp2;

      cond = fold_build2 (EQ_EXPR, boolean_type_node, status,
			  build_int_cst (TREE_TYPE (status), 0));
      tmp2 = fold_build2 (MODIFY_EXPR, status_type,
			  fold_build1 (INDIRECT_REF, status_type, status),
			  build_int_cst (status_type, LIBERROR_ALLOCATION));
      tmp = fold_build3 (COND_EXPR, void_type_node, cond, tmp,
			 tmp2);
    }

  tmp = fold_build3 (COND_EXPR, void_type_node,
		     fold_build2 (EQ_EXPR, boolean_type_node, res,
				  build_int_cst (prvoid_type_node, 0)),
		     tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&alloc_block, tmp);

  cond = fold_build2 (LT_EXPR, boolean_type_node, size,
		      build_int_cst (TREE_TYPE (size), 0));
  tmp = fold_build3 (COND_EXPR, void_type_node, cond, error,
		     gfc_finish_block (&alloc_block));
  gfc_add_expr_to_block (block, tmp);

  return res;
}


/* Generate code for an ALLOCATE statement when the argument is an
   allocatable array.  If the array is currently allocated, it is an
   error to allocate it again.
 
   This function follows the following pseudo-code:
  
    void *
    allocate_array (void *mem, size_t size, integer_type *stat)
    {
      if (mem == NULL)
	return allocate (size, stat);
      else
      {
	if (stat)
	{
	  free (mem);
	  mem = allocate (size, stat);
	  *stat = LIBERROR_ALLOCATION;
	  return mem;
	}
	else
	  runtime_error ("Attempting to allocate already allocated array");
    }
    
    expr must be set to the original expression being allocated for its locus
    and variable name in case a runtime error has to be printed.  */
tree
gfc_allocate_array_with_status (stmtblock_t * block, tree mem, tree size,
				tree status, gfc_expr* expr)
{
  stmtblock_t alloc_block;
  tree res, tmp, null_mem, alloc, error;
  tree type = TREE_TYPE (mem);

  if (TREE_TYPE (size) != TREE_TYPE (size_type_node))
    size = fold_convert (size_type_node, size);

  /* Create a variable to hold the result.  */
  res = gfc_create_var (type, NULL);
  null_mem = fold_build2 (EQ_EXPR, boolean_type_node, mem,
			  build_int_cst (type, 0));

  /* If mem is NULL, we call gfc_allocate_with_status.  */
  gfc_start_block (&alloc_block);
  tmp = gfc_allocate_with_status (&alloc_block, size, status);
  gfc_add_modify (&alloc_block, res, fold_convert (type, tmp));
  alloc = gfc_finish_block (&alloc_block);

  /* Otherwise, we issue a runtime error or set the status variable.  */
  if (expr)
    {
      tree varname;

      gcc_assert (expr->expr_type == EXPR_VARIABLE && expr->symtree);
      varname = gfc_build_cstring_const (expr->symtree->name);
      varname = gfc_build_addr_expr (pchar_type_node, varname);

      error = gfc_trans_runtime_error (true, &expr->where,
				       "Attempting to allocate already"
				       " allocated array '%s'",
				       varname);
    }
  else
    error = gfc_trans_runtime_error (true, NULL,
				     "Attempting to allocate already allocated"
				     "array");

  if (status != NULL_TREE && !integer_zerop (status))
    {
      tree status_type = TREE_TYPE (TREE_TYPE (status));
      stmtblock_t set_status_block;

      gfc_start_block (&set_status_block);
      tmp = build_call_expr_loc (input_location,
			     built_in_decls[BUILT_IN_FREE], 1,
			     fold_convert (pvoid_type_node, mem));
      gfc_add_expr_to_block (&set_status_block, tmp);

      tmp = gfc_allocate_with_status (&set_status_block, size, status);
      gfc_add_modify (&set_status_block, res, fold_convert (type, tmp));

      gfc_add_modify (&set_status_block,
			   fold_build1 (INDIRECT_REF, status_type, status),
			   build_int_cst (status_type, LIBERROR_ALLOCATION));

      tmp = fold_build2 (EQ_EXPR, boolean_type_node, status,
			 build_int_cst (status_type, 0));
      error = fold_build3 (COND_EXPR, void_type_node, tmp, error,
			   gfc_finish_block (&set_status_block));
    }

  tmp = fold_build3 (COND_EXPR, void_type_node, null_mem, alloc, error);
  gfc_add_expr_to_block (block, tmp);

  return res;
}


/* Free a given variable, if it's not NULL.  */
tree
gfc_call_free (tree var)
{
  stmtblock_t block;
  tree tmp, cond, call;

  if (TREE_TYPE (var) != TREE_TYPE (pvoid_type_node))
    var = fold_convert (pvoid_type_node, var);

  gfc_start_block (&block);
  var = gfc_evaluate_now (var, &block);
  cond = fold_build2 (NE_EXPR, boolean_type_node, var,
		      build_int_cst (pvoid_type_node, 0));
  call = build_call_expr_loc (input_location,
			  built_in_decls[BUILT_IN_FREE], 1, var);
  tmp = fold_build3 (COND_EXPR, void_type_node, cond, call,
		     build_empty_stmt (input_location));
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
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
   expression being deallocated for its locus and variable name.  */
tree
gfc_deallocate_with_status (tree pointer, tree status, bool can_fail,
			    gfc_expr* expr)
{
  stmtblock_t null, non_null;
  tree cond, tmp, error;

  cond = fold_build2 (EQ_EXPR, boolean_type_node, pointer,
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

      cond2 = fold_build2 (NE_EXPR, boolean_type_node, status,
			   build_int_cst (TREE_TYPE (status), 0));
      tmp = fold_build2 (MODIFY_EXPR, status_type,
			 fold_build1 (INDIRECT_REF, status_type, status),
			 build_int_cst (status_type, 1));
      error = fold_build3 (COND_EXPR, void_type_node, cond2, tmp, error);
    }

  gfc_add_expr_to_block (&null, error);

  /* When POINTER is not NULL, we free it.  */
  gfc_start_block (&non_null);
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[BUILT_IN_FREE], 1,
			 fold_convert (pvoid_type_node, pointer));
  gfc_add_expr_to_block (&non_null, tmp);

  if (status != NULL_TREE && !integer_zerop (status))
    {
      /* We set STATUS to zero if it is present.  */
      tree status_type = TREE_TYPE (TREE_TYPE (status));
      tree cond2;

      cond2 = fold_build2 (NE_EXPR, boolean_type_node, status,
			   build_int_cst (TREE_TYPE (status), 0));
      tmp = fold_build2 (MODIFY_EXPR, status_type,
			 fold_build1 (INDIRECT_REF, status_type, status),
			 build_int_cst (status_type, 0));
      tmp = fold_build3 (COND_EXPR, void_type_node, cond2, tmp,
			 build_empty_stmt (input_location));
      gfc_add_expr_to_block (&non_null, tmp);
    }

  return fold_build3 (COND_EXPR, void_type_node, cond,
		      gfc_finish_block (&null), gfc_finish_block (&non_null));
}


/* Reallocate MEM so it has SIZE bytes of data.  This behaves like the
   following pseudo-code:

void *
internal_realloc (void *mem, size_t size)
{
  if (size < 0)
    runtime_error ("Attempt to allocate a negative amount of memory.");
  res = realloc (mem, size);
  if (!res && size != 0)
    _gfortran_os_error ("Out of memory");

  if (size == 0)
    return NULL;

  return res;
}  */
tree
gfc_call_realloc (stmtblock_t * block, tree mem, tree size)
{
  tree msg, res, negative, nonzero, zero, null_result, tmp;
  tree type = TREE_TYPE (mem);

  size = gfc_evaluate_now (size, block);

  if (TREE_TYPE (size) != TREE_TYPE (size_type_node))
    size = fold_convert (size_type_node, size);

  /* Create a variable to hold the result.  */
  res = gfc_create_var (type, NULL);

  /* size < 0 ?  */
  negative = fold_build2 (LT_EXPR, boolean_type_node, size,
			  build_int_cst (size_type_node, 0));
  msg = gfc_build_addr_expr (pchar_type_node, gfc_build_localized_cstring_const
      ("Attempt to allocate a negative amount of memory."));
  tmp = fold_build3 (COND_EXPR, void_type_node, negative,
		     build_call_expr_loc (input_location,
				      gfor_fndecl_runtime_error, 1, msg),
		     build_empty_stmt (input_location));
  gfc_add_expr_to_block (block, tmp);

  /* Call realloc and check the result.  */
  tmp = build_call_expr_loc (input_location,
			 built_in_decls[BUILT_IN_REALLOC], 2,
			 fold_convert (pvoid_type_node, mem), size);
  gfc_add_modify (block, res, fold_convert (type, tmp));
  null_result = fold_build2 (EQ_EXPR, boolean_type_node, res,
			     build_int_cst (pvoid_type_node, 0));
  nonzero = fold_build2 (NE_EXPR, boolean_type_node, size,
			 build_int_cst (size_type_node, 0));
  null_result = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, null_result,
			     nonzero);
  msg = gfc_build_addr_expr (pchar_type_node, gfc_build_localized_cstring_const
						("Out of memory"));
  tmp = fold_build3 (COND_EXPR, void_type_node, null_result,
		     build_call_expr_loc (input_location,
				      gfor_fndecl_os_error, 1, msg),
		     build_empty_stmt (input_location));
  gfc_add_expr_to_block (block, tmp);

  /* if (size == 0) then the result is NULL.  */
  tmp = fold_build2 (MODIFY_EXPR, type, res, build_int_cst (type, 0));
  zero = fold_build1 (TRUTH_NOT_EXPR, boolean_type_node, nonzero);
  tmp = fold_build3 (COND_EXPR, void_type_node, zero, tmp,
		     build_empty_stmt (input_location));
  gfc_add_expr_to_block (block, tmp);

  return res;
}

/* Add a statement to a block.  */

void
gfc_add_expr_to_block (stmtblock_t * block, tree expr)
{
  gcc_assert (block);

  if (expr == NULL_TREE || IS_EMPTY_STMT (expr))
    return;

  if (block->head)
    {
      if (TREE_CODE (block->head) != STATEMENT_LIST)
	{
	  tree tmp;

	  tmp = block->head;
	  block->head = NULL_TREE;
	  append_to_statement_list (tmp, &block->head);
	}
      append_to_statement_list (expr, &block->head);
    }
  else
    /* Don't bother creating a list if we only have a single statement.  */
    block->head = expr;
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


/* Get the current locus.  The structure may not be complete, and should
   only be used with gfc_set_backend_locus.  */

void
gfc_get_backend_locus (locus * loc)
{
  loc->lb = XCNEW (gfc_linebuf);
  loc->lb->location = input_location;
  loc->lb->file = gfc_current_backend_file;
}


/* Set the current locus.  */

void
gfc_set_backend_locus (locus * loc)
{
  gfc_current_backend_file = loc->lb->file;
  input_location = loc->lb->location;
}


/* Translate an executable statement.  */

tree
gfc_trans_code (gfc_code * code)
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

      switch (code->op)
	{
	case EXEC_NOP:
	case EXEC_END_BLOCK:
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
	  res = gfc_trans_init_assign (code);
	  break;

	case EXEC_CONTINUE:
	  res = NULL_TREE;
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
	  res = gfc_trans_stop (code);
	  break;

	case EXEC_CALL:
	  /* For MVBITS we've got the special exception that we need a
	     dependency check, too.  */
	  {
	    bool is_mvbits = false;
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

	case EXEC_DO:
	  res = gfc_trans_do (code);
	  break;

	case EXEC_DO_WHILE:
	  res = gfc_trans_do_while (code);
	  break;

	case EXEC_SELECT:
	  res = gfc_trans_select (code);
	  break;

	case EXEC_FLUSH:
	  res = gfc_trans_flush (code);
	  break;

	case EXEC_FORALL:
	  res = gfc_trans_forall (code);
	  break;

	case EXEC_WHERE:
	  res = gfc_trans_where (code);
	  break;

	case EXEC_ALLOCATE:
	  res = gfc_trans_allocate (code);
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

	case EXEC_OMP_ATOMIC:
	case EXEC_OMP_BARRIER:
	case EXEC_OMP_CRITICAL:
	case EXEC_OMP_DO:
	case EXEC_OMP_FLUSH:
	case EXEC_OMP_MASTER:
	case EXEC_OMP_ORDERED:
	case EXEC_OMP_PARALLEL:
	case EXEC_OMP_PARALLEL_DO:
	case EXEC_OMP_PARALLEL_SECTIONS:
	case EXEC_OMP_PARALLEL_WORKSHARE:
	case EXEC_OMP_SECTIONS:
	case EXEC_OMP_SINGLE:
	case EXEC_OMP_TASK:
	case EXEC_OMP_TASKWAIT:
	case EXEC_OMP_WORKSHARE:
	  res = gfc_trans_omp_directive (code);
	  break;

	default:
	  internal_error ("gfc_trans_code(): Bad statement code");
	}

      gfc_set_backend_locus (&code->loc);

      if (res != NULL_TREE && ! IS_EMPTY_STMT (res))
	{
	  if (TREE_CODE (res) == STATEMENT_LIST)
	    tree_annotate_all_with_location (&res, input_location);
	  else
	    SET_EXPR_LOCATION (res, input_location);
	    
	  /* Add the new statement to the block.  */
	  gfc_add_expr_to_block (&block, res);
	}
    }

  /* Return the finished block.  */
  return gfc_finish_block (&block);
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
    = build_decl (ns->proc_name->declared_at.lb->location,
		  NAMESPACE_DECL, get_identifier (ns->proc_name->name),
		  void_type_node);
  entry = gfc_find_module (ns->proc_name->name);
  if (entry->namespace_decl)
    /* Buggy sourcecode, using a module before defining it?  */
    htab_empty (entry->decls);
  entry->namespace_decl = ns->proc_name->backend_decl;

  gfc_generate_module_vars (ns);

  /* We need to generate all module function prototypes first, to allow
     sibling calls.  */
  for (n = ns->contained; n; n = n->sibling)
    {
      gfc_entry_list *el;

      if (!n->proc_name)
        continue;

      gfc_create_function_decl (n);
      gcc_assert (DECL_CONTEXT (n->proc_name->backend_decl) == NULL_TREE);
      DECL_CONTEXT (n->proc_name->backend_decl) = ns->proc_name->backend_decl;
      gfc_module_add_decl (entry, n->proc_name->backend_decl);
      for (el = ns->entries; el; el = el->next)
	{
	  gcc_assert (DECL_CONTEXT (el->sym->backend_decl) == NULL_TREE);
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

