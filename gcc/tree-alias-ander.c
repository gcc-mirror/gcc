/* Tree based Andersen points-to analysis
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
#include "bitmap.h"
#include "tree-alias-type.h"
#include "tree-alias-ander.h"
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
#include "tree-flow.h"
#include "tree-inline.h"
#include "varray.h"
#include "tree-simple.h"
#include "splay-tree.h"
#include "engine/util.h"
#include "libcompat/regions.h"
#include "andersen_terms.h"
#include "cgraph.h"
#include "tree-pass.h"


/*  Andersen's interprocedural points-to analysis.
    This is a flow-insensitive, context insensitive algorithm.

    This file is an implementation of the alias_ops structure used by
    tree-alias-common.c to drive PTA analysis. 
    
    All these functions do is generate constraints for and through
    libbanshee. When we query for a points-to set, we ask libbanshee
    to solve the constraints and give us the answer.  The terms of the
    constraints are the aterms, which are an opaque data structure
    that stores libbanshee specific data for the constraints.  

    The constraints to be generated come from andersen's paper. By
    constraint, we mean something like "the points-to set of A must be
    a subset or equal to the points-to set of B" or "the points-to set
    of A must include Q".  In order to avoid having to write all the
    constraints directly in the code, we use helper functions such as
    pta_assignment, pta_rvalue, etc, that generate the necessary
    constraint terms  for us, making for much more readable code.

    One could replace libbanshee with some other constraint solving
    engine, and you'd simply have to replace the implementation of the
    pta_* functions, and provide replacements for the aterm specific
    functions (like making a list of aterms, printing the label of an
    aterm).  However, libbanshee is extremely fast, and extremely low
    memory usage, so one would be hard pressed to do better than it
    anyway.    

    Understanding how constraint solving and what each constraint means is
    beyond the scope of this documentation.  See the libbanshee
    documentation, and references therein for more enlightenment.
    
    That said, our constraints inclusion constraints of set
    expressions.  Given the helper functions, the various inference
    functions we implement should *look* relatively straightforward.  

    In order to save time during queries, we cache the resulting
    points-to sets of each variable, rather than recalculate them
    again and again. (libbanshee actually has it's own internal
    caching, but the function call overhead for calling the solver is
    non-trivial, given the number of queries).

    Todo: Don't pass alias ops as first argument, just have a global
    "current_alias_ops".  */
				
static unsigned int id_num = 1;
static region andersen_rgn;
static void andersen_simple_assign (struct tree_alias_ops *,
				    alias_var, alias_var);
static void andersen_addr_assign (struct tree_alias_ops *,
				  alias_var, alias_var);
static void andersen_ptr_assign (struct tree_alias_ops *,
				 alias_var, alias_var);
static void andersen_op_assign (struct tree_alias_ops *,
				alias_var, varray_type, tree, bitmap);
static void andersen_heap_assign (struct tree_alias_ops *, alias_var);
static void andersen_assign_ptr (struct tree_alias_ops *,
				 alias_var, alias_var);
static void andersen_function_def (struct tree_alias_ops *, alias_var,
				   varray_type, alias_var);
static int andersen_function_call (struct tree_alias_ops *, alias_var,
				   alias_var, varray_type, bitmap);
static void andersen_init (struct tree_alias_ops *);
static int print_out_result (splay_tree_node, void *);
static void andersen_cleanup (struct tree_alias_ops *);
static bool andersen_may_alias (struct tree_alias_ops *, alias_var,
				alias_var);
static bool andersen_same_points_to_set (struct tree_alias_ops *, 
					 alias_var, alias_var);
static bool andersen_empty_points_to_set (struct tree_alias_ops *,
					  alias_var);
static alias_var andersen_add_var (struct tree_alias_ops *, tree);
static alias_var andersen_add_var_same (struct tree_alias_ops *,
					    tree, alias_var);
static bool pointer_destroying_op (tree);
static aterm_list get_ptset (alias_var);
static splay_tree ptamap;


static struct tree_alias_ops andersen_ops = {
  andersen_init,
  andersen_cleanup,
  andersen_add_var,
  andersen_add_var_same,
  andersen_simple_assign,
  andersen_addr_assign,
  andersen_ptr_assign,
  andersen_op_assign,
  andersen_heap_assign,
  andersen_assign_ptr,
  andersen_function_def,
  andersen_function_call,
  andersen_may_alias,
  andersen_same_points_to_set,
  andersen_empty_points_to_set,
  0, /* data */
  0, /* Currently non-interprocedural */
  1  /* Can do IP on all statics without help. */
};
struct tree_alias_ops *andersen_alias_ops = &andersen_ops;

static void term_inclusion (aterm, aterm);
static void pta_init (void);
static void pta_reset (void);
static aterm get_ref (aterm);
static argterm fun_rec_aterm (aterm_list);
static aterm pta_make_lam (const char *, aterm, aterm_list);
static aterm pta_make_ref (const char *);
static aterm pta_bottom (void);
static aterm pta_join (aterm, aterm);
static aterm pta_deref (aterm);
static aterm pta_rvalue (aterm);
static aterm pta_address (aterm);
static void pta_assignment (aterm, aterm);
static aterm pta_make_fun (const char *, aterm, aterm_list);
static aterm pta_application (aterm, aterm_list);

typedef aterm contents_type;
static contents_type pta_get_contents (aterm);
static void pr_ptset_aterm_elem (aterm);
static void pta_pr_ptset (contents_type);

/* Hook for debugging.  This function is called instead of
   aterm_inclusion, and lets us print the actual constraints as they
   are generated.  */

static void
term_inclusion (aterm t1, aterm t2)
{
  if (dump_file)
    {
      fprintf (dump_file, "Constraint: ");
      aterm_print (dump_file, t1);
      fprintf (dump_file, " <= ");
      aterm_print (dump_file, t2);
      fprintf (dump_file,  "\n");
    }

  aterm_inclusion (t1, t2);
}

/* Initialize libbanshee's constraint engine.  */

static void
pta_init (void)
{
  andersen_terms_init ();
}

/* Reset libbanshee's constraint engine.  We do this when we are done
   using it, as it releases the memory libbanshee is using.  */

static void
pta_reset (void)
{
  andersen_terms_reset ();
}

static aterm
get_ref (aterm t)
{
  struct ref_decon r_decon;
  r_decon = ref_decon (t);

  assert (r_decon.f1);

  return r_decon.f1;
}

/* Make a function record out of the arguments.  */

static argterm
fun_rec_aterm (aterm_list args)
{
  region scratch;
  int counter = 0;
  argterm rest, result;
  aterm_list_scanner scan;
  aterm temp;
  char field_name[512];
  argterm_map map;

  scratch = newregion ();
  map = new_argterm_map (scratch);
  aterm_list_scan (args, &scan);
  while (aterm_list_next (&scan, &temp))
    {
      snprintf (field_name, 512, "%d", counter++);
      argterm_map_cons (argterm_make_field (field_name, temp), map);
    }

  rest = argterm_wild ();
  /* rest = argterm_fresh(); */

  /*  safe since field_add makes a copy of the string*/
  result = argterm_row (map, rest);

  deleteregion (scratch);

  return result;
}


static aterm
pta_make_lam (const char *id, aterm ret, aterm_list args)
{
  return lam (label_term_constant (id), fun_rec_aterm (args), ret);
}

/* Make a label reference to the given id.  */

static aterm
pta_make_ref (const char *id)
{

  aterm var = aterm_fresh (id);

  label_term tag = label_term_constant (id);

  return ref (tag, var, var);
}

/* Return the empty set.  */

static aterm
pta_bottom (void)
{
  return aterm_zero ();
}

/* Join two terms, such that anything in set t1 will also be in set
   t2, and vice versa.  */

static aterm
pta_join (aterm t1, aterm t2)
{
  aterm result;
  region scratch_rgn = newregion ();
  aterm_list list = new_aterm_list (scratch_rgn);

  aterm_list_cons (t1, list);
  aterm_list_cons (t2, list);


  result = aterm_union (list);
  deleteregion (scratch_rgn);

  return result;
}

/* Generate the constraint for a dereference of term t1.  */

static aterm
pta_deref (aterm t1)
{
  return ref_proj2 (t1);
}

/* Generate the constraint for t1 being an rvalue.  */

static aterm
pta_rvalue (aterm t1)
{
  return pta_deref (t1);
}

/* Generate the constraint for taking the address of t1.  */

static aterm
pta_address (aterm t1)
{
  return ref (label_term_one (), aterm_one (), t1);
}

/* Generate the constraint for assigning t2 to t1.  */

static void
pta_assignment (aterm t1, aterm t2)
{
  term_inclusion (t1, ref_pat1 (t2));
}

/* Make a function from the given name, return value, and arguments.  */

static aterm
pta_make_fun (const char *name, aterm ret, aterm_list args)
{
  aterm temp;
  aterm_list_scanner scan;
  region scratch_rgn = newregion ();
  aterm_list arg_list = new_aterm_list (scratch_rgn);

  aterm_list_scan (args, &scan);

  while (aterm_list_next (&scan, &temp))
    {
      aterm_list_cons (get_ref (temp), arg_list);
    }

  return pta_make_lam (name, get_ref (ret), arg_list);
}

/* Return the constraint for calling function T with arguments
   ACTUALS.  */

static aterm
pta_application (aterm t, aterm_list actuals)
{
  argterm args = fun_rec_aterm (actuals);

  term_inclusion (t, lam_pat1 (args));
  return pta_address (lam_proj2 (t));
}

/* Return the contents of set expression T.  */

static contents_type
pta_get_contents (aterm t)
{
  struct ref_decon t_decon;
  t_decon = ref_decon (t);

  return t_decon.f1;
}

/* Print out a points-to set element.  */

static void
pr_ptset_aterm_elem (aterm t)
{
  struct ref_decon ref;
  struct lam_decon lam;

  ref = ref_decon (t);
  lam = lam_decon (t);

  fprintf (dump_file, ",");
  if (ref.f0)
    label_term_print (dump_file, ref.f0);
  else if (lam.f0)
    label_term_print (dump_file, lam.f0);
}


/* Print out a points-to set.  */

static void
pta_pr_ptset (contents_type t)
{
  int size;
  region scratch_rgn;
  aterm_list ptset;
  scratch_rgn = newregion ();
  ptset = aterm_list_copy (scratch_rgn, aterm_tlb (t));

  size = aterm_list_length (ptset);

  fprintf (dump_file, "{");
  if (!aterm_list_empty (ptset))
    {
      struct ref_decon ref;
      struct lam_decon lam;
      ref = ref_decon (aterm_list_head (ptset));
      lam = lam_decon (aterm_list_head (ptset));
      if (ref.f0)
	label_term_print (dump_file, ref.f0);
      else if (lam.f0)
	label_term_print (dump_file, lam.f0);

      /*      aterm_pr(stdout,aterm_hd(ptset)); */
      ptset = aterm_list_tail (ptset);
    }
  aterm_list_app (ptset, pr_ptset_aterm_elem);
  fprintf (dump_file, "}(%d)\n", size);
  deleteregion (scratch_rgn);
}

/* Initialize Andersen alias analysis. */
static int initted = 0;

static void
andersen_init (struct tree_alias_ops *ops ATTRIBUTE_UNUSED)
{
#if 0
  /* Don't claim we can do ip partial unless we have unit_at_a_time on. */
  if (!flag_unit_at_a_time)   
#endif
    andersen_ops.ip_partial = 0;
  if (!initted || (!andersen_ops.ip_partial && !andersen_ops.ip))
    {
      pta_init ();
      andersen_rgn = newregion ();
      initted = 1;
    }

  ptamap = splay_tree_new (splay_tree_compare_pointers, NULL, NULL);

}

static int
print_out_result (splay_tree_node node, void *data ATTRIBUTE_UNUSED)
{
  fprintf (dump_file, "%s :=",
	   alias_get_name (ALIAS_VAR_DECL (((alias_var) node->value))));
  pta_pr_ptset (pta_get_contents ((aterm) node->key));
  return 0;
}

/* Cleanup after Andersen alias analysis.  */

static void
andersen_cleanup (struct tree_alias_ops *ops ATTRIBUTE_UNUSED)
{
  if (dump_file)
    {
      if (dump_flags & TDF_STATS)
	{
	  fprintf (dump_file, "\nPoints-to stats:\n");
	  andersen_terms_stats (dump_file);
	}

      fprintf (dump_file, "\nPoints-to sets:\n");
      splay_tree_foreach (ptamap, print_out_result, NULL);
    }

  splay_tree_delete (ptamap);

  if (!andersen_ops.ip_partial && !andersen_ops.ip)
    {
      pta_reset ();
      deleteregion (andersen_rgn);
      andersen_rgn = NULL;
    }
}

/* Add decl to the analyzer, and return a var for it.  For
   Andersen, we create a new alias var for the declaration, and
   return that.  */

static alias_var
andersen_add_var (struct tree_alias_ops *ops ATTRIBUTE_UNUSED, tree decl)
{
  alias_var ret;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Adding variable %s\n",
	     alias_get_name (decl));

  if (alias_get_name (decl) != NULL)
    {
      ret = alias_var_new_with_aterm (decl,
				       pta_make_ref (alias_get_name (decl)));
    }
  else
    {
      char *tmp_name;
      ASM_FORMAT_PRIVATE_NAME (tmp_name, "unnamed var", id_num++);
      ret = alias_var_new_with_aterm (decl, pta_make_ref (tmp_name));
    }
  splay_tree_insert (ptamap, (splay_tree_key) ALIAS_VAR_ATERM (ret),
		     (splay_tree_value) ret);
  ALIAS_VAR_PTSET (ret) = NULL;

  return ret;
}

/* Add a variable to the analyzer that is equivalent (as far as
   aliases go) to some existing alias variable.
   For Andersen, we just call a function that does this for us.  */

static alias_var
andersen_add_var_same (struct tree_alias_ops *ops ATTRIBUTE_UNUSED, tree decl,
		       alias_var tv)
{
  alias_var ret;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Adding variable %s same as %s\n",
	     alias_get_name (decl), alias_get_name (ALIAS_VAR_DECL (tv)));

  if (alias_get_name (decl) != NULL)
    ret = alias_var_new_with_aterm (decl,
				     pta_make_ref (alias_get_name (decl)));
  else
    {
      char *tmp_name;
      ASM_FORMAT_PRIVATE_NAME (tmp_name, "unnamed var", id_num++);
      ret = alias_var_new_with_aterm (decl, pta_make_ref (tmp_name));
    }

  pta_join (ALIAS_VAR_ATERM (tv), ALIAS_VAR_ATERM (ret));
  splay_tree_insert (ptamap, (splay_tree_key) ALIAS_VAR_ATERM (ret),
		     (splay_tree_value) ret);
  ALIAS_VAR_PTSET (tv) = NULL;
  ALIAS_VAR_PTSET (ret) = NULL;

  return ret;
}

/* Inference for simple assignment (lhs = rhs) */

static void
andersen_simple_assign (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
			alias_var lhs, alias_var rhs)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Simple assignment %s = %s\n",
	     alias_get_name (ALIAS_VAR_DECL (lhs)),
	     alias_get_name (ALIAS_VAR_DECL (rhs)));
  if (lhs == rhs)
    return;
  
  /* The rvalue is just the term itself, and we generate a constraint
     for assigning it to the lhs.  */
  pta_assignment (ALIAS_VAR_ATERM (lhs),
		  pta_rvalue (ALIAS_VAR_ATERM (rhs)));
}

/* Inference for address assignment (lhs = &addr) */

static void
andersen_addr_assign (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
		      alias_var lhs, alias_var addr)
{
  if (addr == NULL)
    return;
 if (dump_file && (dump_flags & TDF_DETAILS))
   fprintf (dump_file, "Address assignment %s = &%s\n",
	    alias_get_name (ALIAS_VAR_DECL (lhs)),
	    alias_get_name (ALIAS_VAR_DECL (addr)));

 /* The rvalue here is the address of a term, and we generate a
    constraint to assign this address to the lhs.  */
  pta_assignment (ALIAS_VAR_ATERM (lhs),
		  pta_rvalue (pta_address (ALIAS_VAR_ATERM (addr))));
}


/* Inference for pointer assignment (lhs = *ptr) */

static void
andersen_ptr_assign (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
		     alias_var lhs, alias_var ptr)
{

  if (ptr == NULL)
    return;
 if (dump_file && (dump_flags & TDF_DETAILS))
   fprintf (dump_file, "Pointer assignment %s = *%s\n",
	    alias_get_name (ALIAS_VAR_DECL (lhs)),
	    alias_get_name (ALIAS_VAR_DECL (ptr)));

  pta_assignment (ALIAS_VAR_ATERM (lhs),
		  pta_rvalue (pta_deref (ALIAS_VAR_ATERM (ptr))));

}

/* Determine if OP destroys the current assumed to be valid pointer
   (whether it generates a new valid pointer is not relevant).  */

static bool
pointer_destroying_op (tree op)
{
  switch (TREE_CODE (op))
    {
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_NOT_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      return true;
    default:
      return false;
    }
  return false;
}

/* Inference rule for operations (lhs = operation(operands)).  */

static void
andersen_op_assign (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
		    alias_var lhs, varray_type operands, tree operation,
		    bitmap addrargs)
{
  aterm newvar = NULL;
  
  if (VARRAY_ACTIVE_SIZE (operands) == 0)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Op assignment %s = ",
	       alias_get_name (ALIAS_VAR_DECL (lhs)));
      print_generic_stmt (dump_file, operation, dump_flags);
      fprintf (dump_file, "\n");
    }
  
      
  /* Pointer destroying operations do not give us the same valid pointer
     back, and thus, are assignment to pta_bottom. */
  if (pointer_destroying_op (operation))
    {
      pta_assignment (ALIAS_VAR_ATERM (lhs), pta_rvalue (pta_bottom ()));
      return;
    }
  
  /* Operations in general we can't track the exact effect of.  Thus,
     we conservatively assume that it could make the LHS point to
     *anything* the RHS points to.  To signify this, we join the RHS
     variables together and assign it to the LHS.  */
  /* The >2 case occurs when we are dealing with constructors.  */
  if (VARRAY_ACTIVE_SIZE (operands) > 2)
    {
      size_t i;
      alias_var tv1 = VARRAY_GENERIC_PTR (operands, 0);
      newvar = ALIAS_VAR_ATERM (tv1);
      for (i = 1; i < VARRAY_ACTIVE_SIZE (operands); i++)
	{
	  alias_var tempvar = VARRAY_GENERIC_PTR (operands, i);
	  aterm t2 = ALIAS_VAR_ATERM (tempvar);
	  if (bitmap_bit_p (addrargs, i))
	    newvar = pta_join (newvar, pta_address (t2));
	  else
	    newvar = pta_join (newvar, t2);
	}
    }
  else if (VARRAY_ACTIVE_SIZE (operands) == 2)
    {
      alias_var tv1 = VARRAY_GENERIC_PTR (operands, 0);
      alias_var tv2 = VARRAY_GENERIC_PTR (operands, 1);
      aterm t1 = ALIAS_VAR_ATERM (tv1);
      aterm t2 = ALIAS_VAR_ATERM (tv2);
      if (bitmap_bit_p (addrargs, 0) && bitmap_bit_p (addrargs, 1))
	newvar = pta_join (pta_address (t1), pta_address (t2));
      else if (bitmap_bit_p (addrargs, 0))
	newvar = pta_join (pta_address (t1), t2);
      else if (bitmap_bit_p (addrargs, 1))
	newvar = pta_join (t1, pta_address (t2));
      else
	newvar = pta_join (t1, t2);
    }
  else if (VARRAY_ACTIVE_SIZE (operands) == 1)
    {
      alias_var tv1 = VARRAY_GENERIC_PTR (operands, 0);
      aterm t1 = ALIAS_VAR_ATERM (tv1);
      if (bitmap_bit_p (addrargs, 0))
	newvar = pta_address (t1);
      else
	newvar = t1;
    }
  pta_assignment (ALIAS_VAR_ATERM (lhs), pta_rvalue (newvar));
}

/* Inference for heap assignment (lhs = alloc).  */

static void
andersen_heap_assign (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
		      alias_var lhs ATTRIBUTE_UNUSED)
{
#if 0
  alias_type type1;
  ECR tau;
  type1 = ECR_get_type (alias_var_get_ECR (lhs));
  tau = alias_ltype_loc (type1);

  if (ECR_get_type (tau) == alias_bottom)
    ECR_set_type (tau, alias_ltype_new ());
#endif
}

/* Inference for assignment to a pointer (*ptr = rhs).  */

static void
andersen_assign_ptr (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
		     alias_var ptr, alias_var rhs)
{

  if (rhs == NULL)
    return;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Assignment to pointer  *%s = %s\n",
	     alias_get_name (ALIAS_VAR_DECL (ptr)),
	     alias_get_name (ALIAS_VAR_DECL (rhs)));
  /* The RHS is a standard rvalue, and the LHS is a pointer
     dereference.  */
  pta_assignment (pta_deref (ALIAS_VAR_ATERM (ptr)),
		  pta_rvalue (ALIAS_VAR_ATERM (rhs)));
}

/* Inference for a function definition.  */

static void
andersen_function_def (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
		       alias_var func, varray_type params,
		       alias_var retval)
{
  aterm_list args = new_aterm_list (andersen_rgn);
  aterm fun_type;

  size_t l = VARRAY_ACTIVE_SIZE (params);
  size_t i;

  /* Set up the arguments for the new function type. */
  for (i = 0; i < l; i++)
    {
      alias_var tv = VARRAY_GENERIC_PTR (params, i);
      aterm_list_cons (ALIAS_VAR_ATERM (tv), args);
    }
  /* Create the function type.  */
  fun_type = pta_make_fun (alias_get_name (ALIAS_VAR_DECL (func)),
			   ALIAS_VAR_ATERM (retval), args);

  /* Assign the function type itself to the function.  */
  pta_assignment (ALIAS_VAR_ATERM (func), fun_type);
}

/* Inference for a function call assignment.  */

static int
andersen_function_call (struct tree_alias_ops *ops,
			alias_var lhs, alias_var func,
			varray_type args, bitmap addrargs)
{
  aterm_list actuals = new_aterm_list (andersen_rgn);
  aterm ftype = ALIAS_VAR_ATERM (func);
  aterm ret = NULL;
  aterm res;
  tree decl = ALIAS_VAR_DECL (func);

  size_t i;

  if (lhs)
    ret = ALIAS_VAR_ATERM (lhs);
  for (i = 0; i < VARRAY_ACTIVE_SIZE (args); i++)
    {
      alias_var argtv = VARRAY_GENERIC_PTR (args, i);
      aterm arg = ALIAS_VAR_ATERM (argtv);
      if (bitmap_bit_p (addrargs, i))
	aterm_list_cons (pta_rvalue (pta_address (arg)), actuals);
      else
	aterm_list_cons (pta_rvalue (arg), actuals);
    }
  aterm_list_reverse (actuals);
  
  /* Generate the constraint that calls the function with it's
     arguments, and gives us the result.  This in turn applies
     whatever constraints are in that function.  */
  res = pta_application (pta_rvalue (ftype), actuals);
  /* We only need care about the result if we have an LHS.  If we do,
     assign the result of function application back to the LHS.  */
  if (ret)
    pta_assignment (ret, pta_rvalue (res));

  /* We can handle functions we've got trees for. non-statics will
     just have incoming parameters assigned to global_var if
     necessary. */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_PTA_ALIASVAR (decl)
      && ops->ip_partial
      && (cgraph_local_info (decl)->local))
    {
      return 0;
    }
  return 1;
}


/* Simple pointer comparison function for list sorting.  */

static int 
simple_cmp (const aterm a, const aterm b)
{
  return (int *)a - (int *)b;
}


/* Get the points-to set for TV, caching if it we had to compute it. */
   
static aterm_list 
get_ptset (alias_var tv)
{
  aterm_list ptset;
  ptset = ALIAS_VAR_PTSET (tv);
  if (ptset != NULL)
    return ptset;
  ptset = aterm_tlb (pta_get_contents (ALIAS_VAR_ATERM (tv)));
  ALIAS_VAR_PTSET (tv) = ptset;
  return ptset;
}
  
  
/* Determine if two aterm's have the same points-to set. */

static bool
andersen_same_points_to_set (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
			     alias_var ptrtv, alias_var vartv)
{
  aterm_list ptset1, ptset2;
  aterm_list_scanner scan1, scan2;
  aterm data1, data2;
  region scratch_rgn = newregion ();

  ptset1 = get_ptset (ptrtv);
  ptset2 = get_ptset (vartv);
  
  if (aterm_list_length (ptset1) != aterm_list_length (ptset2))
    {
      deleteregion (scratch_rgn);
      return false;
    }

  if (ptset1 == ptset2)
    {
      deleteregion (scratch_rgn);
      return true;
    }

  ptset1 = aterm_list_copy (scratch_rgn, ptset1);
  ptset2 = aterm_list_copy (scratch_rgn, ptset2);
  
  if (aterm_list_length (ptset1) != aterm_list_length (ptset2))
    {
      deleteregion (scratch_rgn);
      return false;
    }

  ptset1 = aterm_list_sort (ptset1, simple_cmp);
  ptset2 = aterm_list_sort (ptset2, simple_cmp);
  
  aterm_list_scan (ptset1, &scan1);
  aterm_list_scan (ptset2, &scan2);
  while (aterm_list_next (&scan1, &data1))
    {
      aterm_list_next (&scan2, &data2);
      if (data1 != data2)
	{
	  deleteregion(scratch_rgn);
	  return false;
	}
    }
  deleteregion(scratch_rgn);
  return true;  
}


/* Determine if two variables may alias.  In our case, this means
   whether the decl represented by PTRTV can point to VARTV.  */

static bool
andersen_may_alias (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
		    alias_var ptrtv, alias_var vartv)
{
  aterm_list ptset;
  ptset = get_ptset (ptrtv);

  if (aterm_list_empty (ptset))
    return false;

  return aterm_list_member (ptset, ALIAS_VAR_ATERM (vartv));
}

/* Determine whether PTRTV has an empty points-to set. IE it may not
   point to anything.  */

static bool 
andersen_empty_points_to_set (struct tree_alias_ops *ops ATTRIBUTE_UNUSED,
			      alias_var ptrtv)
{
  aterm_list ptset;
  ptset = get_ptset (ptrtv);
  return aterm_list_empty (ptset);  
}
