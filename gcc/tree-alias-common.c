/* Tree based points-to analysis
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree-alias-type.h"
#include "bitmap.h"
#include "tree-alias-common.h"
/* If we have andersen's points-to analysis, include it. */
#ifdef HAVE_BANSHEE
#include "tree-alias-ander.h"
#endif
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "errors.h"
#include "expr.h"
#include "diagnostic.h"
#include "tree.h"
#include "c-common.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "varray.h"
#include "c-tree.h"
#include "tree-simple.h"
#include "hashtab.h"
#include "function.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "timevar.h"

/* Reduce ifdefery later.  */
#ifndef HAVE_BANSHEE
#define HAVE_BANSHEE 0
#endif

/*  This file contains the implementation of the common parts of the
    tree points-to analysis infrastructure.
    
    Overview:
    
    This file contains the points-to analysis driver.  It does two main things:
    1. Keeps track of the PTA data for each variable (IE the data each
    specific PTA implementation wants to keep associated with a
    variable).
    2. Walks the function trees, calling the appropriate functions that
    each PTA implementation has implemented.
    
    In order to speed up PTA queries, the PTA specific data is stored
    in the tree for *_DECL's, in DECL_PTA_ALIASVAR.  This way, we only
    need to use the hash table for non-DECL's.  */
#define FIELD_BASED 0

/*  Array of all created alias_vars.
    Note that this should contain all the alias_vars we wanted
    marked during GC.  */
static GTY((param_is (union alias_var_def))) varray_type alias_vars = NULL;
struct tree_alias_ops *current_alias_ops;

/*  Array of local (to a function) alias_vars.
    Note that this should contain all the alias_vars that are
    local to this function.  We delete these from alias_vars before
    collection.  */
static GTY(()) varray_type local_alias_vars;
static GTY(()) varray_type local_alias_varnums;
tree pta_global_var;
static bitmap addrargs;						 
static alias_var get_alias_var_decl (tree);
static alias_var get_alias_var (tree);
static void find_func_aliases (tree);
static void deal_with_call_aliasing (tree, alias_var);
static alias_var create_fun_alias_var_ptf (tree, tree);
static alias_var create_fun_alias_var (tree, int);
static alias_var create_alias_var (tree);
static void intra_function_call (varray_type);
static void get_values_from_constructor (tree, varray_type *, bitmap, int *);
static bool call_may_clobber (tree);
static bool call_may_return (tree);

/* Return true if a EXPR, which is a CALL_EXPR, may clobber variables.  */

static bool
call_may_clobber (tree expr)
{
  int flags;

  if (TREE_CODE (expr) != CALL_EXPR)
    return false;

  flags = call_expr_flags (expr);
  return (! (flags & (ECF_CONST | ECF_PURE | ECF_NORETURN)));
}

/* Return true if a EXPR, which is a CALL_EXPR, may return.  */

static bool
call_may_return (tree expr)
{
  int flags;
  
  if (TREE_CODE (expr) != CALL_EXPR)
    return false;

  flags = call_expr_flags (expr);
  return ! (flags & ECF_NORETURN);
}

/*  Get the alias_var for DECL.  
    Creates the alias_var if it does not exist already. Also
    handles FUNCTION_DECL properly.  */

static alias_var
get_alias_var_decl (tree decl)
{
  alias_var newvar;
  if (TREE_CODE (decl) == FIELD_DECL)
    abort ();
  if (DECL_P (decl))
    {
      if (DECL_PTA_ALIASVAR (decl))
	return DECL_PTA_ALIASVAR (decl);
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    newvar = create_fun_alias_var  (decl, 0);
  else
    {
      newvar = create_alias_var (decl);
      /* Assign globals to global var for purposes of intraprocedural
	 analysis. */
      if ((DECL_CONTEXT (decl) == NULL 
	   || TREE_PUBLIC (decl)
	   || TREE_STATIC (decl)
	   || decl_function_context (decl) == NULL) 
	  && decl != pta_global_var)
	{
	  current_alias_ops->addr_assign (current_alias_ops, 
					  get_alias_var (pta_global_var), 
					  newvar);
	  /* If the global has some DECL_INITIAL, we need to process
	     it here. */
	  if (DECL_INITIAL (decl))
	    find_func_aliases (decl);
	}
    }

  if (!current_alias_ops->ip)
    {
      if (!current_alias_ops->ip_partial
	  || (TREE_CODE (decl) != FUNCTION_DECL
	      && TREE_CODE (decl) != PARM_DECL))
	{
	  VARRAY_PUSH_INT (local_alias_varnums, ALIAS_VAR_VARNUM (newvar));
	  VARRAY_PUSH_TREE (local_alias_vars, decl);
	}
    }
  return newvar;
}

/* Get the alias_var for an expression EXPR.
   Note that this function expects to only be handed a RHS or LHS, not
   a MODIFY_EXPR.  */

static alias_var
get_alias_var (tree expr)
{
  /* If it's a decl, get the alias var of the decl. We farm this off
     to get_alias_var_decl so it can abort if the alias var doesn't
     exist, and in case something else *knows* it has a decl, and
     wants the alias var. */

  if (DECL_P (expr))
    return get_alias_var_decl (expr);

  /* True constants have no aliases (unless modifiable strings are on,
     in which case i don't think we'll end up with a STRING_CST anyway) */
  if (TREE_CODE_CLASS (TREE_CODE (expr)) == 'c')
    return NULL;


  switch (TREE_CODE (expr))
    {
    case ARRAY_REF:
      {
	/* Find the first non-array ref, and return it's alias
	   variable */
	tree p;
	for (p = expr; TREE_CODE (p) == ARRAY_REF;
	     p = TREE_OPERAND (p, 0));
	return get_alias_var (p);
      }
      break;
    case COMPONENT_REF:
      {
#if FIELD_BASED
	bool safe = true;
	tree p;
	for (p = expr; 
	     TREE_CODE (p) == COMPONENT_REF || TREE_CODE (p) == INDIRECT_REF;
	     p = TREE_OPERAND (p, 0))
	  {
	    if (TREE_CODE (TREE_TYPE (p)) == UNION_TYPE 
		|| TREE_CODE (TREE_TYPE (p)) == QUAL_UNION_TYPE)
	      {
		safe = false;
		break;
	      }
	  }
	if (!safe)
	  {
	    for (p = expr; TREE_CODE (p) == COMPONENT_REF;
		 p = TREE_OPERAND (p, 0));
	    return get_alias_var (p);
	  }
	else
	  {
	    return get_alias_var (TREE_OPERAND (expr, 1));
	  }
#else
        /* Find the first non-component ref, and return its alias variable. */
	tree p;
	for (p = expr; TREE_CODE (p) == COMPONENT_REF;
	     p = TREE_OPERAND (p, 0));
	return get_alias_var (p);
#endif
      }
      break;
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_CEIL_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case BIT_FIELD_REF:
      /* If it's a ref or cast or conversion of something, get the
         alias var of the something. */
      return get_alias_var (TREE_OPERAND (expr, 0));
      break;

    default:
      return NULL;
    }
}

/*  Perform conservative aliasing for an intraprocedural mode function
    call.  ARGS are the arguments that were passed to that function
    call.  */

static void
intra_function_call (varray_type args)
{
  size_t l = VARRAY_ACTIVE_SIZE (args);
  size_t i;
  alias_var av = get_alias_var (pta_global_var);

  /* We assume assignments among the actual parameters. */
  for (i = 0; i < l; i++)
    {
      alias_var argi = VARRAY_GENERIC_PTR (args, i);
      size_t j;
      for (j = 0; j < l; j++)
	{
	  alias_var argj;
	  if (i == j)
	    continue;
	  argj = VARRAY_GENERIC_PTR (args, j);
	  /* Restricted pointers can't be aliased with other
	     restricted pointers. */
	  if (!TYPE_RESTRICT (TREE_TYPE (ALIAS_VAR_DECL (argi)))
	      || !TYPE_RESTRICT (TREE_TYPE (ALIAS_VAR_DECL (argj))))
	    /* Do a bit of TBAA to avoid pointless assignments. */
	    if (alias_sets_conflict_p (get_alias_set (ALIAS_VAR_DECL (argi)),
				       get_alias_set (ALIAS_VAR_DECL (argj))))	      
	      current_alias_ops->simple_assign (current_alias_ops, argi, argj);
	}
    }
  /* We assume that an actual parameter can point to any global. */
  for (i = 0; i < l; i++)
    {
      alias_var argav = VARRAY_GENERIC_PTR (args, i);
      /* Restricted pointers can't be aliased with other
	 restricted pointers. */
      if (!TYPE_RESTRICT (TREE_TYPE (ALIAS_VAR_DECL (argav)))
	  || !TYPE_RESTRICT (TREE_TYPE (ALIAS_VAR_DECL (av))))
	{
	  /* Arguments can alias globals, and whatever they point to
	     can point to a global as well. */
	  current_alias_ops->simple_assign (current_alias_ops, argav, av);
	}
    }
}

/* Put all pointers in a constructor in an array.  */

static void
get_values_from_constructor (tree constructor, varray_type *vals, 
			     bitmap addrargs, int *i)
{
  tree elt_list;
  switch (TREE_CODE (constructor))
    {
    case CONSTRUCTOR:
      {
	for (elt_list = CONSTRUCTOR_ELTS (constructor);
	     elt_list;
	     elt_list = TREE_CHAIN (elt_list))
	  {
	    tree value = TREE_VALUE (elt_list);
	    if (TREE_CODE (value) == TREE_LIST
		|| TREE_CODE (value) == CONSTRUCTOR)
	      {
		get_values_from_constructor (value, vals, addrargs, i);			      }
	    else
	      {
		alias_var aav;
		aav = get_alias_var (value);
		if (aav)
		  VARRAY_PUSH_GENERIC_PTR (*vals, aav);
		if (TREE_CODE (value) == ADDR_EXPR)
		  bitmap_set_bit (addrargs, *i);
		*i = *i + 1;
	      }
	  }
      }
      break;
    case TREE_LIST:
      for (elt_list = constructor;
	   elt_list;
	   elt_list = TREE_CHAIN (elt_list))
	{
	  get_values_from_constructor (TREE_VALUE (elt_list), vals, addrargs, i);
	}
      break;
    default:
      abort();
    }
}

/* Deal with the possible return values of a call that we don't have
   actual PTA info about.  */

static void
deal_with_call_aliasing (tree callargs, alias_var lhsAV)
{
  tree arg, argp;
  
  for (argp = callargs;
       argp;
       argp = TREE_CHAIN (argp))
    {
      arg = TREE_VALUE (argp);
      /* If we take the address of a variable directly in the
	 argument, the return value could be the address of that
	 variable.  */ 
      if (TREE_CODE (arg) == ADDR_EXPR)
	current_alias_ops->addr_assign (current_alias_ops, lhsAV,
					get_alias_var (arg));
      /* If we pass in a pointer, we could return that pointer.  */
      else if (POINTER_TYPE_P (TREE_TYPE (arg)))
	{
	  alias_var argtv = get_alias_var (arg);
	  if (argtv)
	    current_alias_ops->simple_assign (current_alias_ops, lhsAV,
					      argtv);
	}
    }
}

/* Find the operand of the component ref that actually is doing
   something to the DECL  */
static tree
find_op_of_decl (tree cref)
{
  while (!DECL_P (TREE_OPERAND (cref, 0)))
    {
      cref = TREE_OPERAND (cref, 0);
    }
  return cref;
}


/*  Tree walker that is the heart of the aliasing infrastructure.
    TP is a pointer to the current tree.
    WALK_SUBTREES specifies whether to continue traversing subtrees or
    not.
    Returns NULL_TREE when we should stop.
    
    This function is the main part of the aliasing infrastructure. It
    walks the trees, calling the appropriate alias analyzer functions to process
    various statements.  */

static void
find_func_aliases (tree stp)
{
  if (TREE_CODE (stp) == RETURN_EXPR)
    {
      stp = TREE_OPERAND (stp, 0);
      if (!stp)
	return;
    }
  
  if (TREE_CODE (stp) == MODIFY_EXPR
      || (DECL_P (stp) && DECL_INITIAL (stp) != NULL_TREE ))
    {
      tree op0, op1;
      alias_var lhsAV = NULL;
      alias_var rhsAV = NULL;

      if (DECL_P (stp))
	{
	  op0 = stp;
	  op1 = DECL_INITIAL (stp);
	}
      else
	{
	  op0 = TREE_OPERAND (stp, 0);
	  op1 = TREE_OPERAND (stp, 1);
	}
      /* lhsAV should always have an alias variable */
      lhsAV = get_alias_var (op0);
      if (!lhsAV)
	return;
      /* rhsAV might not have one, c.f. c = 5 */
      rhsAV = get_alias_var (op1);
#if !FIELD_BASED
      while (TREE_CODE (op1) == COMPONENT_REF 
	     && TREE_CODE (TREE_OPERAND (op1, 0)) == COMPONENT_REF)
	{
	  op1 = TREE_OPERAND (op1, 0);
	}
      while (TREE_CODE (op1) == BIT_FIELD_REF)
	{
	  op1 = TREE_OPERAND (op1, 0);
	}
      /* Take care of fact that we may have multi-level component
	 refs. */ 
      if (TREE_CODE (op1) == COMPONENT_REF)
	op1 = find_op_of_decl (op1);
#endif
      
      /* You would think we could test rhsAV at the top, rather than
	 50 separate times, but we can't, because it can be NULL for
	 operator assignments, where we'd still collect the individual
	 alias vars for the operator. */

      /* Note that structures are treated as a single alias
	 variable, since we can disambiguate based on TBAA first,
	 and fall back on points-to. */
      /* x = <something> */
      if (is_gimple_variable (op0))
	{
	  /* x = y */
	  if (is_gimple_variable (op1))
	    {
	      if (rhsAV != NULL)
		current_alias_ops->simple_assign (current_alias_ops, lhsAV,
						  rhsAV);
	    }
	  /* x = foo.y */
	  else if (TREE_CODE (op1) == COMPONENT_REF 
		   && DECL_P (TREE_OPERAND (op1, 0)))
	    {
	         if (rhsAV != NULL)
		current_alias_ops->simple_assign (current_alias_ops, lhsAV,
						  rhsAV);
	    }
	  /* x = (cast) [maybe-addr-expr] y */
	  else if (is_gimple_cast (op1))
	    {
	      tree stripped_op1 = op1;
	      STRIP_NOPS (stripped_op1);
	      if (rhsAV != NULL)
		{
		  if (TREE_CODE (stripped_op1) == ADDR_EXPR)
		    current_alias_ops->addr_assign (current_alias_ops, lhsAV, 
						    rhsAV);
		  else
		    current_alias_ops->simple_assign (current_alias_ops, lhsAV,
						      rhsAV);
		}
	    }
	  /* x = *y or x = foo->y */
	  else if (TREE_CODE (op1) == INDIRECT_REF 
		   || TREE_CODE (op1) == ARRAY_REF
		   || (TREE_CODE (op1) == COMPONENT_REF
		       && TREE_CODE (TREE_OPERAND (op1, 0)) == INDIRECT_REF))
	    {
	      if (rhsAV != NULL)
		current_alias_ops->ptr_assign (current_alias_ops, lhsAV,
					       rhsAV);
	    }
	  /* x = &y = x = &foo.y */
	  else if (TREE_CODE (op1) == ADDR_EXPR)
	    {
	      if (rhsAV != NULL)
		current_alias_ops->addr_assign (current_alias_ops, lhsAV,
						rhsAV);
	    }
	  /* x = func(...) */
	  else if (TREE_CODE (op1) == CALL_EXPR)
	    {
	      /* Heap assignment. These are __attribute__ malloc or
		 something, i'll deal with it later. */
	      if (0)
		{}
	      else
		{
		  
		  /* NORETURN functions have no effect on aliasing. */
		  if (call_may_return (op1))
		    {		      
		      varray_type args;
		      tree arg;
		      tree callop0, callop1;
		      int argnum;
		      
		      /* Collect the arguments */
		      VARRAY_GENERIC_PTR_INIT (args, 1, "Arguments");
		      bitmap_clear (addrargs);
		      callop1 = TREE_OPERAND (op1, 1);
		      callop0 = TREE_OPERAND (op1, 0);
		      for (arg = callop1, argnum = 0;
			   arg;
			   arg = TREE_CHAIN (arg), argnum++)
			{
			  alias_var aav = get_alias_var (TREE_VALUE (arg));
			  if (aav)
			    {
			      VARRAY_PUSH_GENERIC_PTR (args, aav);
			      if (TREE_CODE (TREE_VALUE (arg)) == ADDR_EXPR)
				bitmap_set_bit (addrargs, argnum);
			    }
			}
		      /* Simulate the call */
		      if (current_alias_ops->function_call (current_alias_ops, lhsAV,
							    get_alias_var (callop0),
							    args, addrargs))
			{ 
			  if (call_may_clobber (op1)
			      && !current_alias_ops->ip
                              && flag_argument_noalias != 2)
			    {
			      intra_function_call (args);
			    }
			  if (POINTER_TYPE_P (TREE_TYPE (op0)))
			    deal_with_call_aliasing (callop1, lhsAV);
			}
		    }
		}
	    }
	  /* x = op (...) */
	  else
	    {
	      bitmap_clear (addrargs);
	      if (TREE_CODE (op1) == CONSTRUCTOR)
	        {
		  varray_type ops;
		  int i = 0;
		  VARRAY_GENERIC_PTR_INIT (ops, 1, "Operands");
		  get_values_from_constructor (op1, &ops, addrargs, &i);
		  current_alias_ops->op_assign (current_alias_ops, lhsAV,
						ops, op1, addrargs);
		}
	      else
		switch (TREE_CODE_CLASS (TREE_CODE (op1)))
		  {
		  case 'e':  /* an expression */
		  case 's':  /* an expression with side effects */
		  case '<':  /* a comparison expression */
		  case '1':  /* a unary arithmetic expression */
		  case 'r':  /* a reference */
		  case '2':  /* a binary arithmetic expression */
		    {
		      tree op;
		      varray_type ops;
		      int i;
		      VARRAY_GENERIC_PTR_INIT (ops, 1, "Operands");
		      for (i = 0; i < TREE_CODE_LENGTH (TREE_CODE (op1)); i++)
			{
			  alias_var aav;
			  op = TREE_OPERAND (op1, i);
			  aav = get_alias_var (op);
			  if (aav)
			    VARRAY_PUSH_GENERIC_PTR (ops, aav);
			  if (TREE_CODE (op) == ADDR_EXPR)
			    bitmap_set_bit (addrargs, i);
			}
		      current_alias_ops->op_assign (current_alias_ops, lhsAV,
						    ops, op1, addrargs);
		    }
		    break;
		  default:
		    break;
		  }
	    }
	}
      /* *x = <something> */
      else
	{
	  /* x.f = y  or x->f = y */
	  if ((TREE_CODE (op0) == COMPONENT_REF 
	       || TREE_CODE (op0) == BIT_FIELD_REF)
	      && is_gimple_variable (op1))
	    {
	      if (rhsAV != NULL)
		current_alias_ops->simple_assign (current_alias_ops, lhsAV,
						  rhsAV);
	    }
	  /* x.f = &y or x->f = &y */
	  else if (TREE_CODE (op0) == COMPONENT_REF 
		   && TREE_CODE (op1) == ADDR_EXPR)
	    {
	      if (rhsAV != NULL)
		current_alias_ops->addr_assign (current_alias_ops, lhsAV,
						rhsAV);
	    }
	  /* *x.f = y or *x->f = y */
	  else if ((TREE_CODE (op0) == INDIRECT_REF 
		    || TREE_CODE (op0) == ARRAY_REF)
		   && TREE_CODE (TREE_OPERAND (op0, 0)) == COMPONENT_REF
		   && is_gimple_variable (op1))
	    {
	      if (rhsAV != NULL)
		current_alias_ops->assign_ptr (current_alias_ops, lhsAV,
					       rhsAV);
	    }
	  /* *x = &y */
	  else if ((TREE_CODE (op0) == INDIRECT_REF
		    || TREE_CODE (op0) == ARRAY_REF)
		   && TREE_CODE (op1) == ADDR_EXPR)
	    {
	      /* This becomes temp = &y and *x = temp . */
	      alias_var tempvar;
	      tree temp = create_tmp_var_raw (void_type_node, "aliastmp");
	      tempvar = current_alias_ops->add_var (current_alias_ops, temp);
	      current_alias_ops->addr_assign (current_alias_ops, tempvar,
					      rhsAV);
	      current_alias_ops->assign_ptr (current_alias_ops, lhsAV,
					     tempvar);
	    }

	  /* *x = *y */
	  else if ((TREE_CODE (op0) == INDIRECT_REF 
		    || TREE_CODE (op0) == ARRAY_REF)
		   && (TREE_CODE (op1) == INDIRECT_REF
		       || TREE_CODE (op1) == ARRAY_REF))
	    {
	      /* This becomes temp = *y and *x = temp . */
	      alias_var tempvar;
	      tree temp;
	      temp = create_tmp_var_raw (void_type_node, "aliastmp");
	      tempvar = current_alias_ops->add_var (current_alias_ops, temp);
	      current_alias_ops->ptr_assign (current_alias_ops, tempvar,
					     rhsAV);
	      current_alias_ops->assign_ptr (current_alias_ops, lhsAV,
					     tempvar);
	    }

	  /* *x = (cast) y */
	  else if ((TREE_CODE (op0) == INDIRECT_REF 
		    || TREE_CODE (op0) == ARRAY_REF)
		   && is_gimple_cast (op1))
	    {
	      if (rhsAV != NULL)
		{
		  /* This becomes temp = (cast) y and  *x = temp. */
		  alias_var tempvar;
		  tree temp;
		  temp = create_tmp_var_raw (void_type_node, "aliastmp");
		  tempvar = current_alias_ops->add_var (current_alias_ops,
							temp);
		  current_alias_ops->simple_assign (current_alias_ops,
						    tempvar, rhsAV);
		  current_alias_ops->assign_ptr (current_alias_ops, lhsAV,
						 tempvar);
		}
	    }
	  /* *x = <something else> */
	  else
	    {
	      if (rhsAV != NULL)
		current_alias_ops->assign_ptr (current_alias_ops, lhsAV,
					       rhsAV);
	    }
	}
    }
  /* Calls without return values. */
  else if (TREE_CODE (stp) == CALL_EXPR)
    {
      alias_var callvar;
      varray_type args;
      tree arg;
      callvar = get_alias_var (TREE_OPERAND (stp, 0));
      if (callvar != NULL)
	{
	
	  /* NORETURN and CONST functions with no return value
	     have no effect on aliasing (as may be seen above,
	     const functions that return a value might have an
	     effect on aliasing, since the return value can point
	     to one of the arguments.  */
	  if (call_may_clobber (stp))
	    {
	      int argnum;
	      VARRAY_GENERIC_PTR_INIT (args, 1, "Arguments");
	      bitmap_clear (addrargs);
	      for (arg = TREE_OPERAND (stp, 1), argnum=0; 
		   arg; 
		   arg = TREE_CHAIN (arg), argnum++)
		{
		  alias_var aav = get_alias_var (TREE_VALUE (arg));
		  if (aav)
		    {
		      VARRAY_PUSH_GENERIC_PTR (args, aav);
		      if (TREE_CODE (TREE_VALUE (arg)) == ADDR_EXPR)
			bitmap_set_bit (addrargs, argnum);
		    }
		  
		}
	      
	      if (current_alias_ops->function_call (current_alias_ops, NULL,
						    callvar, args, addrargs))
		if (!current_alias_ops->ip && flag_argument_noalias != 2)
		  intra_function_call (args);
	    }
	}
  }
}

/*  Create the alias_var for a function definition DECL, it's
    arguments, and it's return value. If FORCE is true, we force 
    creation of the alias_var, regardless of whether one exists already.
    
    This includes creation of alias_var's for
    - The function itself.
    - The arguments.
    - The return value.  */

static alias_var
create_fun_alias_var (tree decl, int force)
{
  alias_var avar, retvar;
  tree rdecl;
  varray_type params = NULL;

  if (!force)
    {
      if (DECL_PTA_ALIASVAR (decl))
        return DECL_PTA_ALIASVAR (decl);
    }

  VARRAY_GENERIC_PTR_INIT (params, 1, "Arguments");
  if (DECL_ARGUMENTS (decl) != NULL)
    {
      tree arg;
      for (arg = DECL_ARGUMENTS (decl); arg; arg = TREE_CHAIN (arg))
	{
	  alias_var var = get_alias_var (arg);
	  VARRAY_PUSH_GENERIC_PTR (params, var);
	  /* Incoming pointers can point to pta_global_var, unless
	     either we are interprocedural, or we can do ip on all
	     statics + this function has been defined + it's not an
	     external function. */
	  if (POINTER_TYPE_P (TREE_TYPE (arg))
	      && !current_alias_ops->ip
	      /* FIXME: Need to let analyzer decide in partial case. */
	      && (!current_alias_ops->ip_partial
		  || !cgraph_local_info (decl)->local))
	    current_alias_ops->simple_assign (current_alias_ops, var,
					      get_alias_var (pta_global_var));
	}
    }
  else if (TYPE_ARG_TYPES (TREE_TYPE (decl)) != NULL)
    {
      tree arg;
      /* FIXME: Handle varargs */
      for (arg = TYPE_ARG_TYPES (TREE_TYPE (decl));
	   arg && TREE_VALUE (arg) != void_type_node;
	   arg = TREE_CHAIN (arg))
	{
	  tree fakedecl = create_tmp_var_raw (TREE_VALUE (arg), "normarg");
	  alias_var var;
	  DECL_CONTEXT (fakedecl) = current_function_decl; 
	  var = get_alias_var (fakedecl);
	  VARRAY_PUSH_GENERIC_PTR (params, var);

	  /* Incoming pointers can point to pta_global_var, unless
	     either we are interprocedural, or we can do ip on all
	     statics + this function has been defined + it's not an
	     external function. */
	  if (POINTER_TYPE_P (TREE_TYPE (fakedecl))
	      && !current_alias_ops->ip
	      /* FIXME: need to let analyzer decide in partial case. */
	      && (!current_alias_ops->ip_partial
		  || !TREE_STATIC (decl)
		  || TREE_PUBLIC (decl)))
	    current_alias_ops->simple_assign (current_alias_ops, var,
					      get_alias_var (pta_global_var));
	}
    }
  /* Functions declared like void f() are *not* equivalent to void
     f(void). You can pass an argument to them. Thus, we need to
     create some fake argument that would alias any actuals that get
     passed to our function.  */
  else
    {
      tree fakedecl = create_tmp_var_raw (void_type_node, "fakearg");
      alias_var fakevar;
      DECL_CONTEXT (fakedecl) = current_function_decl;
      fakevar = get_alias_var (fakedecl);
      VARRAY_PUSH_GENERIC_PTR (params, fakevar);
    }

  if (!DECL_RESULT (decl))
    {
      rdecl = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (decl)), "_rv_");
      retvar = current_alias_ops->add_var (current_alias_ops, rdecl);
      DECL_PTA_ALIASVAR (rdecl) = retvar;
    }
  else
    {
      retvar = current_alias_ops->add_var (current_alias_ops,
					   DECL_RESULT (decl));
      DECL_PTA_ALIASVAR (DECL_RESULT (decl)) = retvar;
    }
  VARRAY_PUSH_GENERIC_PTR (alias_vars, retvar);
  ALIAS_VAR_VARNUM (retvar) = VARRAY_ACTIVE_SIZE (alias_vars) - 1;
  avar = current_alias_ops->add_var (current_alias_ops, decl);
  VARRAY_PUSH_GENERIC_PTR (alias_vars, avar);
  ALIAS_VAR_VARNUM (avar) = VARRAY_ACTIVE_SIZE (alias_vars) - 1;

  current_alias_ops->function_def (current_alias_ops, avar, params, retvar);
  DECL_PTA_ALIASVAR (decl) = avar;

  /* FIXME: Also, if this is a defining declaration then add the annotation
     to all extern definitions of the function. */
  return avar;
}

/*  Create an alias variable for a pointer-to-member function DECL of 
    type TYPE, it's arguments, and it's return value.
    Returns the alias_var for the PTF.
    
    This includes creating alias_var's for
    - The function itself.
    - The arguments.
    - The return value.  */

static alias_var
create_fun_alias_var_ptf (tree decl, tree type)
{
  alias_var avar, retvar;
  tree rdecl;
  varray_type params = NULL;

  if (DECL_PTA_ALIASVAR (decl))
    return DECL_PTA_ALIASVAR (decl);

  VARRAY_GENERIC_PTR_INIT (params, 1, "Arguments");

  if (TYPE_ARG_TYPES  (type) != NULL)
    {
      tree arg;
      /* FIXME: Handle varargs */
      for (arg = TYPE_ARG_TYPES (type);
	   arg && TREE_VALUE (arg) != void_type_node;
	   arg = TREE_CHAIN (arg))
	{
	  tree fakedecl = create_tmp_var_raw (TREE_VALUE (arg), "ptfarg");
	  alias_var var;
	  DECL_CONTEXT (fakedecl) = DECL_CONTEXT (decl);
	  var = get_alias_var (fakedecl);
	  VARRAY_PUSH_GENERIC_PTR (params, var);
	}
    }
  /* Functions declared like void f() are *not* equivalent to void
     f(void). You can pass an argument to them. Thus, we need to
     create some fake argument that would alias any actuals that get
     passed to our function.  */
  else
    {
      tree fakedecl = create_tmp_var_raw (void_type_node, "fakearg");
      alias_var fakevar;
      DECL_CONTEXT (fakedecl) = DECL_CONTEXT (decl);
      fakevar = get_alias_var (fakedecl);
      VARRAY_PUSH_GENERIC_PTR (params, fakevar);
    }

  rdecl = create_tmp_var_raw (TREE_TYPE (type), "_rv_");
  retvar = current_alias_ops->add_var (current_alias_ops, rdecl);
  VARRAY_PUSH_GENERIC_PTR (alias_vars, retvar);
  ALIAS_VAR_VARNUM (retvar) = VARRAY_ACTIVE_SIZE (alias_vars) - 1;

  avar = current_alias_ops->add_var (current_alias_ops, decl);
  VARRAY_PUSH_GENERIC_PTR (alias_vars, avar);
  ALIAS_VAR_VARNUM (avar) = VARRAY_ACTIVE_SIZE (alias_vars) - 1;

  current_alias_ops->function_def (current_alias_ops, avar, params, retvar);
  DECL_PTA_ALIASVAR (decl) = avar;

  return avar;
}

/*  Create the alias_var for a *_DECL node DECL.
    Returns the alias_var for DECL.
    
    This function also handles creation of alias_var's for PTF 
    variables.  */

static alias_var
create_alias_var (tree decl)
{
  alias_var avar;

  if (!DECL_P (decl))
    abort ();
  
  if (DECL_P (decl))
    {
      if (DECL_PTA_ALIASVAR (decl))
	return DECL_PTA_ALIASVAR (decl);
    }

  if (POINTER_TYPE_P (TREE_TYPE (decl))
      && TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) == FUNCTION_TYPE)
    {
      avar = create_fun_alias_var_ptf (decl, TREE_TYPE (TREE_TYPE (decl)));
    }
  else
    avar = current_alias_ops->add_var (current_alias_ops, decl);

  if (DECL_P (decl))
    {
      DECL_PTA_ALIASVAR (decl) = avar;
    }

  VARRAY_PUSH_GENERIC_PTR (alias_vars, avar);
  ALIAS_VAR_VARNUM (avar) = VARRAY_ACTIVE_SIZE (alias_vars) - 1;
  return avar;
}

/* Create points-to sets for the current function.  */

static void
create_alias_vars (void)
{
  basic_block bb;
#if HAVE_BANSHEE
  if (flag_tree_points_to == PTA_ANDERSEN)
    current_alias_ops = andersen_alias_ops;
  else
#endif
   {
     current_alias_ops = NULL;
     flag_tree_points_to = PTA_NONE;
     return;
   }

  pta_global_var = build_decl (VAR_DECL, get_identifier (".pta_global_var"),
			       size_type_node);
  DECL_ARTIFICIAL (pta_global_var) = 1;
  TREE_READONLY (pta_global_var) = 1;
  DECL_EXTERNAL (pta_global_var) = 0;
  TREE_STATIC (pta_global_var) = 1;
  TREE_USED (pta_global_var) = 1;
  DECL_CONTEXT (pta_global_var) = current_function_decl; 
  TREE_THIS_VOLATILE (pta_global_var) = 1;
  TREE_ADDRESSABLE (pta_global_var) = 0;

  init_alias_vars ();

  DECL_PTA_ALIASVAR (current_function_decl) = NULL;
  get_alias_var (current_function_decl);

  /* First, walk the variables and their DECL_INITIAL's */
  if (cfun->unexpanded_var_list)
    {
      tree vars, var;
      for (vars = cfun->unexpanded_var_list; vars; vars = TREE_CHAIN (vars))
	{
	  var = TREE_VALUE (vars);
	  if (TREE_CODE (var) != LABEL_DECL
	      && decl_function_context (var) == NULL
	      && DECL_INITIAL (var))
	    find_func_aliases (var);
	}
    }

  /* Now walk all statements and derive aliases.  */
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi; 
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	find_func_aliases (bsi_stmt (bsi));
    }

  pta_global_var = NULL_TREE;
}

struct tree_opt_pass pass_build_pta = 
{
  "pta",				/* name */
  NULL,					/* gate */
  create_alias_vars,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PTA,				/* tv_id */
  PROP_cfg,				/* properties_required */
  PROP_pta,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
};
 

/* Delete created points-to sets.  */

static void
delete_alias_vars (void)
{
  size_t i;

  if (flag_tree_points_to != PTA_ANDERSEN)
    return;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (local_alias_vars); i++)
    {
      tree key = VARRAY_TREE (local_alias_vars, i);
      if (DECL_P (key))
	DECL_PTA_ALIASVAR (key) = NULL;
      else
	abort ();
    }

  for (i = 0; i < VARRAY_ACTIVE_SIZE (local_alias_varnums); i ++)
    VARRAY_GENERIC_PTR (alias_vars, VARRAY_INT (local_alias_varnums, i)) = NULL;
  if (!current_alias_ops->ip && !current_alias_ops->ip_partial)
    {
      /*      VARRAY_CLEAR (alias_vars); */
      VARRAY_CLEAR (local_alias_vars);
      VARRAY_CLEAR (local_alias_varnums);
    }
  BITMAP_XFREE (addrargs);
  current_alias_ops->cleanup (current_alias_ops);
}

struct tree_opt_pass pass_del_pta = 
{
  "pta",				/* name */
  NULL,					/* gate */
  delete_alias_vars,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PTA,				/* tv_id */
  PROP_pta,				/* properties_required */
  0,					/* properties_provided */
  PROP_pta,				/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
};
 

/*  Initialize points-to analysis machinery.  */

void
init_alias_vars (void)
{
  current_alias_ops->init (current_alias_ops);
  addrargs = BITMAP_XMALLOC ();
  VARRAY_TREE_INIT (local_alias_vars, 10, "Local alias vars");
  VARRAY_INT_INIT (local_alias_varnums, 10, "Local alias varnums");
  if ((!current_alias_ops->ip && !current_alias_ops->ip_partial)
      || alias_vars == NULL)
    VARRAY_GENERIC_PTR_INIT (alias_vars, 10, "Alias vars");
}

/* Return true if PTR can't point to anything (i.e. it has an empty
   points-to set.  */
bool 
empty_points_to_set (tree ptr)
{
 alias_var ptrtv;
  
#if !FIELD_BASED
#else
  if (TREE_CODE (ptr) == COMPONENT_REF)
    ptr = TREE_OPERAND (ptr, 1);
#endif

  if (DECL_P (ptr))
    {
      ptrtv = DECL_PTA_ALIASVAR (ptr);
      if (!ptrtv)
	return true;
    }
  else
    abort ();

  return current_alias_ops->empty_points_to_set (current_alias_ops, ptrtv);
}

/* Return true if PTR and VAR have the same points-to set.  */

bool
same_points_to_set (tree ptr, tree var)
{
  alias_var ptrtv, vartv;
  
#if !FIELD_BASED
#else
  if (TREE_CODE (ptr) == COMPONENT_REF)
    ptr = TREE_OPERAND (ptr, 1);
  if (TREE_CODE (var) == COMPONENT_REF)
    var = TREE_OPERAND (var, 1);
#endif

  if (ptr == var)
    return true;

  if (DECL_P (ptr))
    {
      ptrtv = DECL_PTA_ALIASVAR (ptr);
      if (!ptrtv)
	return false;
    }
  else
    abort ();

  if (DECL_P (var))
    {
      vartv = DECL_PTA_ALIASVAR (var);
      if (!vartv)
	return false;
    }
  else
    abort ();

  return current_alias_ops->same_points_to_set (current_alias_ops, vartv, ptrtv);
}

/*  Determine whether two variables (PTR and VAR) may-alias.
    Returns TRUE if PTR may-alias VAR.  */

bool
ptr_may_alias_var (tree ptr, tree var)
{
  alias_var ptrtv, vartv;

#if !FIELD_BASED
#else
  if (TREE_CODE (ptr) == COMPONENT_REF)
    ptr = TREE_OPERAND (ptr, 1);
  if (TREE_CODE (var) == COMPONENT_REF)
    var = TREE_OPERAND (var, 1);
#endif

  if (ptr == var)
    return true;

  if (DECL_P (ptr))
    {
      ptrtv = DECL_PTA_ALIASVAR (ptr);
      if (!ptrtv)
	return false;
    }
  else
    abort ();

  if (DECL_P (var))
    {
      vartv = DECL_PTA_ALIASVAR (var);
      if (!vartv)
	return false;
    }
  else
    abort ();

  return current_alias_ops->may_alias (current_alias_ops, ptrtv, vartv);
}

#define MASK_POINTER(P)	((unsigned)((unsigned long)(P) & 0xffff))

const char *
alias_get_name (tree t)
{
  const char *name;

#if FIELD_BASED
  if (TREE_CODE (t) == FIELD_DECL)
    {
      /* First get the name of the field, then the prefix, then smash them
	 together.  */
      const char *fieldname = IDENTIFIER_POINTER (DECL_NAME (t));
      const char *prefix = alias_get_name (DECL_CONTEXT (t));
      char *smashed;
      size_t neededlen = strlen (fieldname) + strlen (prefix) + 2;
      smashed = ggc_alloc (neededlen);
      sprintf (smashed, "%s.%s", prefix, fieldname);
      name = smashed;

    }
  else if (TYPE_P (t))
    {
      if (TYPE_NAME (t) && IDENTIFIER_POINTER (TYPE_NAME (t)))
	name = IDENTIFIER_POINTER (TYPE_NAME (t));
      else
	name = "<unnamed type>";
    }
  else
#endif
    {
      if (TREE_CODE (t) == FUNCTION_DECL)
	name = IDENTIFIER_POINTER (DECL_NAME (t));
      else if (TREE_CODE (t) == RESULT_DECL)
	name = "<return value>";
      else
	name = get_name (t);
    }

  if (!name)
    {
      char *namep;
      /* 2 = UF
	 4 = the masked pointer
	 2 = the <> around it
	 1 = the terminator. */
      namep = ggc_alloc (2 + 4 + 2 + 1);
      sprintf (namep, "<UV%x>", MASK_POINTER (t));
      return namep;
    }

  return name;
}

#include "gt-tree-alias-common.h"
