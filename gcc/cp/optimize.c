/* Perform optimizations on tree structure.

   Copyright (C) 1998, 1999 Free Software Foundation, Inc.
   Written by Mark Michell (mark@codesourcery.com).

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   GNU CC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "rtl.h"
#include "insn-config.h"
#include "integrate.h"
#include "varray.h"

/* To Do:

   o Provide debugging information for inlined function bodies.  

   o In order to make inlining-on-trees work, we pessimized
     function-local static constants.  In particular, they are now
     always output, even when not addressed.  Fix this by treating
     function-local static constants just like global static
     constants; the back-end already knows not to output them if they
     are not needed.
     
   o Provide heuristics to clamp inlining of recursive template
     calls?  */
   
/* Data required for function inlining.  */

typedef struct inline_data
{
  /* A stack of the functions we are inlining.  For example, if we are
     compiling `f', which calls `g', which calls `h', and we are
     inlining the body of `h', the stack will contain, `h', followed
     by `g', followed by `f'.  */
  varray_type fns;
  /* The top of the FNS stack.  */
  size_t fns_top;
  /* The label to jump to when a return statement is encountered.  */
  tree ret_label;
  /* The map from local declarations in the inlined function to
     equivalents in the function into which it is being inlined.  */
  splay_tree decl_map;
} inline_data;

/* Prototypes.  */

static tree initialize_inlined_parameters PROTO((inline_data *, tree));
static tree declare_return_variable PROTO((inline_data *, tree *));
static tree copy_body_r PROTO((tree *, int *, void *));
static tree copy_body PROTO((inline_data *));
static tree expand_call_inline PROTO((tree *, int *, void *));
static void expand_calls_inline PROTO((tree *, inline_data *));
static int inlinable_function_p PROTO((tree, inline_data *));

/* Called from copy_body via walk_tree.  DATA is really an
   `inline_data *'.  */

static tree
copy_body_r (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees;
     void *data;
{
  inline_data* id;
  tree fn;

  /* Set up.  */
  id = (inline_data *) data;
  fn = VARRAY_TREE (id->fns, id->fns_top - 1);

  /* All automatic variables should have a DECL_CONTEXT indicating
     what function they come from.  */
  if ((TREE_CODE (*tp) == VAR_DECL || TREE_CODE (*tp) == LABEL_DECL)
      && DECL_NAMESPACE_SCOPE_P (*tp))
    my_friendly_assert (DECL_EXTERNAL (*tp) || TREE_STATIC (*tp),
			19991113);

  /* If this is a RETURN_STMT, change it into an EXPR_STMT and a
     GOTO_STMT with the RET_LABEL as its target.  */
  if (TREE_CODE (*tp) == RETURN_STMT)
    {
      tree return_stmt = *tp;
      tree goto_stmt;

      /* Build the GOTO_STMT.  */
      goto_stmt = build_min_nt (GOTO_STMT, id->ret_label);
      TREE_CHAIN (goto_stmt) = TREE_CHAIN (return_stmt);

      /* If we're returning something, just turn that into an
	 assignment into the equivalent of the original 
	 RESULT_DECL.  */
      if (RETURN_EXPR (return_stmt))
	{
	  *tp = build_min_nt (EXPR_STMT, 
			      RETURN_EXPR (return_stmt));
	  /* And then jump to the end of the function.  */
	  TREE_CHAIN (*tp) = goto_stmt;
	}
      /* If we're not returning anything just do the jump.  */
      else
	*tp = goto_stmt;
    }
  /* Local variables and labels need to be replaced by equivalent
     variables.  We don't want to copy static variables; there's only
     one of those, no matter how many times we inline the containing
     function.  */
  else if (nonstatic_local_decl_p (*tp) && DECL_CONTEXT (*tp) == fn)
    {
      splay_tree_node n;

      /* Look up the declaration.  */
      n = splay_tree_lookup (id->decl_map, (splay_tree_key) *tp);

      /* If we didn't already have an equivalent for this declaration,
	 create one now.  */
      if (!n)
	{
	  tree t;

	  /* Make a copy of the variable or label.  */
	  t = copy_decl_for_inlining (*tp, fn, 
				      VARRAY_TREE (id->fns, 0));
	  /* Remember it, so that if we encounter this local entity
	     again we can reuse this copy.  */
	  n = splay_tree_insert (id->decl_map, 
				 (splay_tree_key) *tp, 
				 (splay_tree_value) t);
	}

      /* Replace this variable with the copy.  */
      *tp = (tree) n->value;
    }
  else if (TREE_CODE (*tp) == SAVE_EXPR)
    remap_save_expr (tp, id->decl_map, VARRAY_TREE (id->fns, 0));
  else if (TREE_CODE (*tp) == UNSAVE_EXPR)
    my_friendly_abort (19991113);
  /* Otherwise, just copy the node.  Note that copy_tree_r already
     knows not to copy VAR_DECLs, etc., so this is safe.  */
  else
    {
      copy_tree_r (tp, walk_subtrees, NULL);

      /* The copied TARGET_EXPR has never been expanded, even if the
	 original node was expanded already.  */
      if (TREE_CODE (*tp) == TARGET_EXPR && TREE_OPERAND (*tp, 3))
	TREE_OPERAND (*tp, 1) = TREE_OPERAND (*tp, 3);
      /* Similarly, if we're copying a CALL_EXPR, the RTL for the
	 result is no longer valid.  */
      else if (TREE_CODE (*tp) == CALL_EXPR)
	CALL_EXPR_RTL (*tp) = NULL_RTX;
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Make a copy of the body of FN so that it can be inserted inline in
   another function.  */

static tree
copy_body (id)
     inline_data *id;
{
  tree body;

  body = DECL_SAVED_TREE (VARRAY_TREE (id->fns, id->fns_top - 1));
  walk_tree (&body, copy_body_r, id);

  return body;
}

/* Generate code to initialize the parameters of the function at the
   top of the stack in ID from the ARGS (presented as a TREE_LIST).  */

static tree
initialize_inlined_parameters (id, args)
     inline_data *id;
     tree args;
{
  tree fn;
  tree init_stmts;
  tree parms;
  tree a;
  tree p;

  /* Figure out what the parameters are.  */
  fn = VARRAY_TREE (id->fns, id->fns_top - 1);
  parms = DECL_ARGUMENTS (fn);

  /* Start with no initializations whatsoever.  */
  init_stmts = NULL_TREE;

  /* Loop through the parameter declarations, replacing each with an
     equivalent VAR_DECL, appropriately initialized.  */
  for (p = parms, a = args; p; a = TREE_CHAIN (a), p = TREE_CHAIN (p))
    {
      tree init_stmt;
      tree var;

      /* Make an equivalent VAR_DECL.  */
      var = copy_decl_for_inlining (p, fn, VARRAY_TREE (id->fns, 0));
      /* Register the VAR_DECL as the equivalent for the PARM_DECL;
	 that way, when the PARM_DECL is encountered, it will be
	 automatically replaced by the VAR_DECL.  */
      splay_tree_insert (id->decl_map, 
			 (splay_tree_key) p,
			 (splay_tree_value) var);
      /* Initialize this VAR_DECL from the equivalent argument.  If
	 the argument is an object, created via a constructor or copy,
	 this will not result in an extra copy: the TARGET_EXPR
	 representing the argument will be bound to VAR, and the
	 object will be constructed in VAR.  */
      init_stmt = build_min_nt (EXPR_STMT,
				build (INIT_EXPR, TREE_TYPE (p),
				       var, TREE_VALUE (a)));
      /* Declare this new variable.  Note that we do this *after* the
	 initialization because we are going to reverse all the
	 initialization statements below.  */
      TREE_CHAIN (init_stmt) = build_min_nt (DECL_STMT, var);
      /* Add this initialization to the list.  */
      TREE_CHAIN (TREE_CHAIN (init_stmt)) = init_stmts;
      init_stmts = init_stmt;
    }

  /* The initialization statements have been built up in reverse
     order.  Straighten them out now.  */
  return nreverse (init_stmts);
}

/* Declare a return variable to replace the RESULT_DECL for the
   function we are calling.  An appropriate DECL_STMT is returned.
   The USE_STMT is filled in to contain a use of the declaration to
   indicate the return value of the function.  */

static tree
declare_return_variable (id, use_stmt)
     struct inline_data *id;
     tree *use_stmt;
{
  tree fn = VARRAY_TREE (id->fns, id->fns_top - 1);
  tree result = DECL_RESULT (fn);
  tree var;

  /* We don't need to do anything for functions that don't return
     anything.  */
  if (!result || same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (result)), 
			      void_type_node))
    {
      *use_stmt = NULL_TREE;
      return NULL_TREE;
    }

  /* Make an appropriate copy.  */
  var = copy_decl_for_inlining (result, fn, VARRAY_TREE (id->fns, 0));
  /* Register the VAR_DECL as the equivalent for the RESULT_DECL; that
     way, when the RESULT_DECL is encountered, it will be
     automatically replaced by the VAR_DECL.  */
  splay_tree_insert (id->decl_map, 
		     (splay_tree_key) result,
		     (splay_tree_value) var);

  /* Build the USE_STMT.  */
  *use_stmt = build_min_nt (EXPR_STMT, var);

  /* Build the declaration statement.  */
  return build_min_nt (DECL_STMT, var);
}

/* Returns non-zero if FN is a function that can be inlined.  */

static int
inlinable_function_p (fn, id)
     tree fn;
     inline_data *id;
{
  int inlinable;

  /* If we've already decided this function shouldn't be inlined,
     there's no need to check again.  */
  if (DECL_UNINLINABLE (fn))
    return 0;

  /* Assume it is not inlinable.  */
  inlinable = 0;

  /* If the function was not declared `inline', then we don't inline
     it.  */
  if (!DECL_INLINE (fn))
    ;
  /* If we don't have the function body available, we can't inline
     it.  */
  else if (!DECL_SAVED_TREE (fn))
    ;
  /* We can't inline varargs functions.  */
  else if (varargs_function_p (fn))
    ;
  /* All is well.  We can inline this function.  Traditionally, GCC
     has refused to inline functions using setjmp or alloca, or
     functions whose values are returned in a PARALLEL, and a few
     other such obscure conditions.  We are not equally constrained at
     the tree level.  */
  else
    inlinable = 1;

  /* Squirrel away the result so that we don't have to check again.  */
  DECL_UNINLINABLE (fn) = !inlinable;

  /* Don't do recursive inlining, either.  We don't record this in
     DECL_UNLINABLE; we may be able to inline this function later.  */
  if (inlinable)
    {
      size_t i;

      for (i = 0; i < id->fns_top; ++i)
	if (VARRAY_TREE (id->fns, i) == fn)
	  inlinable = 0;
    }

  /* We can inline a template instantiation only if its fully
     instantiated.  */
  if (inlinable
      && DECL_TEMPLATE_INFO (fn) 
      && TI_PENDING_TEMPLATE_FLAG (DECL_TEMPLATE_INFO (fn)))
    {
      fn = instantiate_decl (fn);
      inlinable = !TI_PENDING_TEMPLATE_FLAG (DECL_TEMPLATE_INFO (fn));
    }

  /* Return the result.  */
  return inlinable;
}

/* If *TP is CALL_EXPR, replace it with its inline expansion.  */

static tree
expand_call_inline (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees;
     void *data;
{
  inline_data *id;
  tree t;
  tree expr;
  tree chain;
  tree fn;
  tree use_stmt;
  splay_tree st;

  /* We're only interested in CALL_EXPRs.  */
  t = *tp;
  if (TREE_CODE (t) != CALL_EXPR)
    return NULL_TREE;

  /* First, see if we can figure out what function is being called.
     If we cannot, then there is no hope of inlining the function.  */
  fn = get_callee_fndecl (t);
  if (!fn)
    return NULL_TREE;

  /* Don't try to inline functions that are not well-suited to
     inlining.  */
  id = (inline_data *) data;
  if (!inlinable_function_p (fn, id))
    return NULL_TREE;

  /* Return statements in the function body will be replaced by jumps
     to the RET_LABEL.  */
  id->ret_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
  DECL_CONTEXT (id->ret_label) = VARRAY_TREE (id->fns, 0);

  /* Build a statement-expression containing code to initialize the
     arguments, the actual inline expansion of the body, and a label
     for the return statements within the function to jump to.  The
     type of the statement expression is the return type of the
     function call.  */
  expr = build_min (STMT_EXPR, TREE_TYPE (TREE_TYPE (fn)), NULL_TREE);

  /* Record the function we are about to inline so that we can avoid
     recursing into it.  */
  if (id->fns_top > id->fns->num_elements)
    VARRAY_GROW (id->fns, 2 * id->fns->num_elements);
  VARRAY_TREE (id->fns, id->fns_top++) = fn;

  /* Local declarations will be replaced by their equivalents in this
     map.  */
  st = id->decl_map;
  id->decl_map = splay_tree_new (splay_tree_compare_pointers,
				 NULL, NULL);

  /* Initialize the parameters.  */
  STMT_EXPR_STMT (expr) 
    = initialize_inlined_parameters (id, TREE_OPERAND (t, 1));

  /* Declare the return variable for the function.  */
  STMT_EXPR_STMT (expr)
    = chainon (STMT_EXPR_STMT (expr), 
	       declare_return_variable (id, &use_stmt));
  
  /* After we've initialized the parameters, we insert the body of the
     function itself.  */
  STMT_EXPR_STMT (expr)
    = chainon (STMT_EXPR_STMT (expr), copy_body (id));

  /* Finally, mention the returned value so that the value of the
     statement-expression is the returned value of the function.  */
  STMT_EXPR_STMT (expr) = chainon (STMT_EXPR_STMT (expr), use_stmt);

  /* Clean up.  */
  splay_tree_delete (id->decl_map);
  id->decl_map = st;

  /* After the body of the function comes the RET_LABEL.  */
  STMT_EXPR_STMT (expr)
    = chainon (STMT_EXPR_STMT (expr), 
	       build_min_nt (LABEL_STMT, id->ret_label));

  /* The new expression has side-effects if the old one did.  */
  TREE_SIDE_EFFECTS (expr) = TREE_SIDE_EFFECTS (t);
  /* If the value of the new expression is ignored, that's OK.  We
     don't warn about this for CALL_EXPRs, so we shouldn't warn about
     the equivalent inlined version either.  */
  TREE_USED (expr) = 1;

  /* Replace the call by the inlined body.  */
  chain = TREE_CHAIN (*tp);
  *tp = expr;
  TREE_CHAIN (expr) = chain;

  /* Recurse into the body of the just inlined function.  */
  expand_calls_inline (tp, id);
  --id->fns_top;

  /* Don't walk into subtrees.  We've already handled them above.  */
  *walk_subtrees = 0;

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Walk over the entire tree *TP, replacing CALL_EXPRs with inline
   expansions as appropriate.  */

static void
expand_calls_inline (tp, id)
     tree *tp;
     inline_data *id;
{
  /* Search through *TP, replacing all calls to inline functions by
     appropriate equivalents.  */
  walk_tree (tp, expand_call_inline, id);
}

/* Optimize the body of FN.  */

void
optimize_function (fn)
     tree fn;
{
  /* Expand calls to inline functions.  */
  if (flag_inline_trees)
    {
      inline_data id;

      /* Clear out ID.  */
      bzero (&id, sizeof (id));

      /* Don't allow recursion into FN.  */
      VARRAY_TREE_INIT (id.fns, 32, "fns");
      VARRAY_TREE (id.fns, id.fns_top++) = fn;

      /* Replace all calls to inline functions with the bodies of those
	 functions.  */
      expand_calls_inline (&DECL_SAVED_TREE (fn), &id);

      /* Clean up.  */
      VARRAY_FREE (id.fns);
    }
}
