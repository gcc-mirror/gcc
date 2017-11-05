/* Pass manager for Fortran front end.
   Copyright (C) 2010-2017 Free Software Foundation, Inc.
   Contributed by Thomas König.

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
#include "gfortran.h"
#include "dependency.h"
#include "constructor.h"
#include "intrinsic.h"

/* Forward declarations.  */

static void strip_function_call (gfc_expr *);
static void optimize_namespace (gfc_namespace *);
static void optimize_assignment (gfc_code *);
static bool optimize_op (gfc_expr *);
static bool optimize_comparison (gfc_expr *, gfc_intrinsic_op);
static bool optimize_trim (gfc_expr *);
static bool optimize_lexical_comparison (gfc_expr *);
static void optimize_minmaxloc (gfc_expr **);
static bool is_empty_string (gfc_expr *e);
static void doloop_warn (gfc_namespace *);
static int do_intent (gfc_expr **);
static int do_subscript (gfc_expr **);
static void optimize_reduction (gfc_namespace *);
static int callback_reduction (gfc_expr **, int *, void *);
static void realloc_strings (gfc_namespace *);
static gfc_expr *create_var (gfc_expr *, const char *vname=NULL);
static int matmul_to_var_expr (gfc_expr **, int *, void *);
static int matmul_to_var_code (gfc_code **, int *, void *);
static int inline_matmul_assign (gfc_code **, int *, void *);
static gfc_code * create_do_loop (gfc_expr *, gfc_expr *, gfc_expr *,
				  locus *, gfc_namespace *,
				  char *vname=NULL);
static gfc_expr* check_conjg_transpose_variable (gfc_expr *, bool *,
						 bool *);
static bool has_dimen_vector_ref (gfc_expr *);
static int matmul_temp_args (gfc_code **, int *,void *data);
static int index_interchange (gfc_code **, int*, void *);

#ifdef CHECKING_P
static void check_locus (gfc_namespace *);
#endif

/* How deep we are inside an argument list.  */

static int count_arglist;

/* Vector of gfc_expr ** we operate on.  */

static vec<gfc_expr **> expr_array;

/* Pointer to the gfc_code we currently work on - to be able to insert
   a block before the statement.  */

static gfc_code **current_code;

/* Pointer to the block to be inserted, and the statement we are
   changing within the block.  */

static gfc_code *inserted_block, **changed_statement;

/* The namespace we are currently dealing with.  */

static gfc_namespace *current_ns;

/* If we are within any forall loop.  */

static int forall_level;

/* Keep track of whether we are within an OMP workshare.  */

static bool in_omp_workshare;

/* Keep track of whether we are within a WHERE statement.  */

static bool in_where;

/* Keep track of iterators for array constructors.  */

static int iterator_level;

/* Keep track of DO loop levels.  */

typedef struct {
  gfc_code *c;
  int branch_level;
  bool seen_goto;
} do_t;

static vec<do_t> doloop_list;
static int doloop_level;

/* Keep track of if and select case levels.  */

static int if_level;
static int select_level;

/* Vector of gfc_expr * to keep track of DO loops.  */

struct my_struct *evec;

/* Keep track of association lists.  */

static bool in_assoc_list;

/* Counter for temporary variables.  */

static int var_num = 1;

/* What sort of matrix we are dealing with when inlining MATMUL.  */

enum matrix_case { none=0, A2B2, A2B1, A1B2, A2B2T, A2TB2 };

/* Keep track of the number of expressions we have inserted so far
   using create_var.  */

int n_vars;

/* Entry point - run all passes for a namespace.  */

void
gfc_run_passes (gfc_namespace *ns)
{

  /* Warn about dubious DO loops where the index might
     change.  */

  doloop_level = 0;
  if_level = 0;
  select_level = 0;
  doloop_warn (ns);
  doloop_list.release ();
  int w, e;

#ifdef CHECKING_P
  check_locus (ns);
#endif

  if (flag_frontend_optimize || flag_frontend_loop_interchange)
    optimize_namespace (ns);

  if (flag_frontend_optimize)
    {
      optimize_reduction (ns);
      if (flag_dump_fortran_optimized)
	gfc_dump_parse_tree (ns, stdout);

      expr_array.release ();
    }

  gfc_get_errors (&w, &e);
  if (e > 0)
   return;

  if (flag_realloc_lhs)
    realloc_strings (ns);
}

#ifdef CHECKING_P

/* Callback function: Warn if there is no location information in a
   statement.  */

static int
check_locus_code (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
		  void *data ATTRIBUTE_UNUSED)
{
  current_code = c;
  if (c && *c && (((*c)->loc.nextc == NULL) || ((*c)->loc.lb == NULL)))
    gfc_warning_internal (0, "No location in statement");

  return 0;
}


/* Callback function: Warn if there is no location information in an
   expression.  */

static int
check_locus_expr (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
		  void *data ATTRIBUTE_UNUSED)
{

  if (e && *e && (((*e)->where.nextc == NULL || (*e)->where.lb == NULL)))
    gfc_warning_internal (0, "No location in expression near %L",
			  &((*current_code)->loc));
  return 0;
}

/* Run check for missing location information.  */

static void
check_locus (gfc_namespace *ns)
{
  gfc_code_walker (&ns->code, check_locus_code, check_locus_expr, NULL);

  for (ns = ns->contained; ns; ns = ns->sibling)
    {
      if (ns->code == NULL || ns->code->op != EXEC_BLOCK)
	check_locus (ns);
    }
}

#endif

/* Callback for each gfc_code node invoked from check_realloc_strings.
   For an allocatable LHS string which also appears as a variable on
   the RHS, replace

   a = a(x:y)

   with

   tmp = a(x:y)
   a = tmp
 */

static int
realloc_string_callback (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
			 void *data ATTRIBUTE_UNUSED)
{
  gfc_expr *expr1, *expr2;
  gfc_code *co = *c;
  gfc_expr *n;
  gfc_ref *ref;
  bool found_substr;

  if (co->op != EXEC_ASSIGN)
    return 0;

  expr1 = co->expr1;
  if (expr1->ts.type != BT_CHARACTER || expr1->rank != 0
      || !gfc_expr_attr(expr1).allocatable
      || !expr1->ts.deferred)
    return 0;

  expr2 = gfc_discard_nops (co->expr2);

  if (expr2->expr_type == EXPR_VARIABLE)
    {
      found_substr = false;
      for (ref = expr2->ref; ref; ref = ref->next)
	{
	  if (ref->type == REF_SUBSTRING)
	    {
	      found_substr = true;
	      break;
	    }
	}
      if (!found_substr)
	return 0;
    }
  else if (expr2->expr_type != EXPR_OP
	   || expr2->value.op.op != INTRINSIC_CONCAT)
    return 0;
  
  if (!gfc_check_dependency (expr1, expr2, true))
    return 0;

  /* gfc_check_dependency doesn't always pick up identical expressions.
     However, eliminating the above sends the compiler into an infinite
     loop on valid expressions.  Without this check, the gimplifier emits
     an ICE for a = a, where a is deferred character length.  */
  if (!gfc_dep_compare_expr (expr1, expr2))
    return 0;

  current_code = c;
  inserted_block = NULL;
  changed_statement = NULL;
  n = create_var (expr2, "realloc_string");
  co->expr2 = n;
  return 0;
}

/* Callback for each gfc_code node invoked through gfc_code_walker
   from optimize_namespace.  */

static int
optimize_code (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
	       void *data ATTRIBUTE_UNUSED)
{

  gfc_exec_op op;

  op = (*c)->op;

  if (op == EXEC_CALL || op == EXEC_COMPCALL || op == EXEC_ASSIGN_CALL
      || op == EXEC_CALL_PPC)
    count_arglist = 1;
  else
    count_arglist = 0;

  current_code = c;
  inserted_block = NULL;
  changed_statement = NULL;

  if (op == EXEC_ASSIGN)
    optimize_assignment (*c);
  return 0;
}

/* Callback for each gfc_expr node invoked through gfc_code_walker
   from optimize_namespace.  */

static int
optimize_expr (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
	       void *data ATTRIBUTE_UNUSED)
{
  bool function_expr;

  if ((*e)->expr_type == EXPR_FUNCTION)
    {
      count_arglist ++;
      function_expr = true;
    }
  else
    function_expr = false;

  if (optimize_trim (*e))
    gfc_simplify_expr (*e, 0);

  if (optimize_lexical_comparison (*e))
    gfc_simplify_expr (*e, 0);

  if ((*e)->expr_type == EXPR_OP && optimize_op (*e))
    gfc_simplify_expr (*e, 0);

  if ((*e)->expr_type == EXPR_FUNCTION && (*e)->value.function.isym)
    switch ((*e)->value.function.isym->id)
      {
      case GFC_ISYM_MINLOC:
      case GFC_ISYM_MAXLOC:
	optimize_minmaxloc (e);
	break;
      default:
	break;
      }

  if (function_expr)
    count_arglist --;

  return 0;
}

/* Auxiliary function to handle the arguments to reduction intrnisics.  If the
   function is a scalar, just copy it; otherwise returns the new element, the
   old one can be freed.  */

static gfc_expr *
copy_walk_reduction_arg (gfc_constructor *c, gfc_expr *fn)
{
  gfc_expr *fcn, *e = c->expr;

  fcn = gfc_copy_expr (e);
  if (c->iterator)
    {
      gfc_constructor_base newbase;
      gfc_expr *new_expr;
      gfc_constructor *new_c;

      newbase = NULL;
      new_expr = gfc_get_expr ();
      new_expr->expr_type = EXPR_ARRAY;
      new_expr->ts = e->ts;
      new_expr->where = e->where;
      new_expr->rank = 1;
      new_c = gfc_constructor_append_expr (&newbase, fcn, &(e->where));
      new_c->iterator = c->iterator;
      new_expr->value.constructor = newbase;
      c->iterator = NULL;

      fcn = new_expr;
    }

  if (fcn->rank != 0)
    {
      gfc_isym_id id = fn->value.function.isym->id;

      if (id == GFC_ISYM_SUM || id == GFC_ISYM_PRODUCT)
	fcn = gfc_build_intrinsic_call (current_ns, id,
					fn->value.function.isym->name,
					fn->where, 3, fcn, NULL, NULL);
      else if (id == GFC_ISYM_ANY || id == GFC_ISYM_ALL)
	fcn = gfc_build_intrinsic_call (current_ns, id,
					fn->value.function.isym->name,
					fn->where, 2, fcn, NULL);
      else
	gfc_internal_error ("Illegal id in copy_walk_reduction_arg");

      fcn->symtree->n.sym->attr.access = ACCESS_PRIVATE;
    }

  return fcn;
}

/* Callback function for optimzation of reductions to scalars.  Transform ANY
   ([f1,f2,f3, ...]) to f1 .or. f2 .or. f3 .or. ..., with ANY, SUM and PRODUCT
   correspondingly.  Handly only the simple cases without MASK and DIM.  */

static int
callback_reduction (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
		    void *data ATTRIBUTE_UNUSED)
{
  gfc_expr *fn, *arg;
  gfc_intrinsic_op op;
  gfc_isym_id id;
  gfc_actual_arglist *a;
  gfc_actual_arglist *dim;
  gfc_constructor *c;
  gfc_expr *res, *new_expr;
  gfc_actual_arglist *mask;

  fn = *e;

  if (fn->rank != 0 || fn->expr_type != EXPR_FUNCTION
      || fn->value.function.isym == NULL)
    return 0;

  id = fn->value.function.isym->id;

  if (id != GFC_ISYM_SUM && id != GFC_ISYM_PRODUCT
      && id != GFC_ISYM_ANY && id != GFC_ISYM_ALL)
    return 0;

  a = fn->value.function.actual;

  /* Don't handle MASK or DIM.  */

  dim = a->next;

  if (dim->expr != NULL)
    return 0;

  if (id == GFC_ISYM_SUM || id == GFC_ISYM_PRODUCT)
    {
      mask = dim->next;
      if ( mask->expr != NULL)
	return 0;
    }

  arg = a->expr;

  if (arg->expr_type != EXPR_ARRAY)
    return 0;

  switch (id)
    {
    case GFC_ISYM_SUM:
      op = INTRINSIC_PLUS;
      break;

    case GFC_ISYM_PRODUCT:
      op = INTRINSIC_TIMES;
      break;

    case GFC_ISYM_ANY:
      op = INTRINSIC_OR;
      break;

    case GFC_ISYM_ALL:
      op = INTRINSIC_AND;
      break;

    default:
      return 0;
    }

  c = gfc_constructor_first (arg->value.constructor);

  /* Don't do any simplififcation if we have
     - no element in the constructor or
     - only have a single element in the array which contains an
     iterator.  */

  if (c == NULL)
    return 0;

  res = copy_walk_reduction_arg (c, fn);

  c = gfc_constructor_next (c);
  while (c)
    {
      new_expr = gfc_get_expr ();
      new_expr->ts = fn->ts;
      new_expr->expr_type = EXPR_OP;
      new_expr->rank = fn->rank;
      new_expr->where = fn->where;
      new_expr->value.op.op = op;
      new_expr->value.op.op1 = res;
      new_expr->value.op.op2 = copy_walk_reduction_arg (c, fn);
      res = new_expr;
      c = gfc_constructor_next (c);
    }

  gfc_simplify_expr (res, 0);
  *e = res;
  gfc_free_expr (fn);

  return 0;
}

/* Callback function for common function elimination, called from cfe_expr_0.
   Put all eligible function expressions into expr_array.  */

static int
cfe_register_funcs (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
	  void *data ATTRIBUTE_UNUSED)
{

  if ((*e)->expr_type != EXPR_FUNCTION)
    return 0;

  /* We don't do character functions with unknown charlens.  */
  if ((*e)->ts.type == BT_CHARACTER
      && ((*e)->ts.u.cl == NULL || (*e)->ts.u.cl->length == NULL
	  || (*e)->ts.u.cl->length->expr_type != EXPR_CONSTANT))
    return 0;

  /* We don't do function elimination within FORALL statements, it can
     lead to wrong-code in certain circumstances.  */

  if (forall_level > 0)
    return 0;

  /* Function elimination inside an iterator could lead to functions which
     depend on iterator variables being moved outside.  FIXME: We should check
     if the functions do indeed depend on the iterator variable.  */

  if (iterator_level > 0)
    return 0;

  /* If we don't know the shape at compile time, we create an allocatable
     temporary variable to hold the intermediate result, but only if
     allocation on assignment is active.  */

  if ((*e)->rank > 0 && (*e)->shape == NULL && !flag_realloc_lhs)
    return 0;

  /* Skip the test for pure functions if -faggressive-function-elimination
     is specified.  */
  if ((*e)->value.function.esym)
    {
      /* Don't create an array temporary for elemental functions.  */
      if ((*e)->value.function.esym->attr.elemental && (*e)->rank > 0)
	return 0;

      /* Only eliminate potentially impure functions if the
	 user specifically requested it.  */
      if (!flag_aggressive_function_elimination
	  && !(*e)->value.function.esym->attr.pure
	  && !(*e)->value.function.esym->attr.implicit_pure)
	return 0;
    }

  if ((*e)->value.function.isym)
    {
      /* Conversions are handled on the fly by the middle end,
	 transpose during trans-* stages and TRANSFER by the middle end.  */
      if ((*e)->value.function.isym->id == GFC_ISYM_CONVERSION
	  || (*e)->value.function.isym->id == GFC_ISYM_TRANSFER
	  || gfc_inline_intrinsic_function_p (*e))
	return 0;

      /* Don't create an array temporary for elemental functions,
	 as this would be wasteful of memory.
	 FIXME: Create a scalar temporary during scalarization.  */
      if ((*e)->value.function.isym->elemental && (*e)->rank > 0)
	return 0;

      if (!(*e)->value.function.isym->pure)
	return 0;
    }

  expr_array.safe_push (e);
  return 0;
}

/* Auxiliary function to check if an expression is a temporary created by
   create var.  */

static bool
is_fe_temp (gfc_expr *e)
{
  if (e->expr_type != EXPR_VARIABLE)
    return false;

  return e->symtree->n.sym->attr.fe_temp;
}

/* Determine the length of a string, if it can be evaluated as a constant
   expression.  Return a newly allocated gfc_expr or NULL on failure.
   If the user specified a substring which is potentially longer than
   the string itself, the string will be padded with spaces, which
   is harmless.  */

static gfc_expr *
constant_string_length (gfc_expr *e)
{

  gfc_expr *length;
  gfc_ref *ref;
  gfc_expr *res;
  mpz_t value;

  if (e->ts.u.cl)
    {
      length = e->ts.u.cl->length;
      if (length && length->expr_type == EXPR_CONSTANT)
	return gfc_copy_expr(length);
    }

  /* Return length of substring, if constant. */
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_SUBSTRING
	  && gfc_dep_difference (ref->u.ss.end, ref->u.ss.start, &value))
	{
	  res = gfc_get_constant_expr (BT_INTEGER, gfc_charlen_int_kind,
				       &e->where);

	  mpz_add_ui (res->value.integer, value, 1);
	  mpz_clear (value);
	  return res;
	}
    }

  /* Return length of char symbol, if constant.  */

  if (e->symtree && e->symtree->n.sym->ts.u.cl
      && e->symtree->n.sym->ts.u.cl->length
      && e->symtree->n.sym->ts.u.cl->length->expr_type == EXPR_CONSTANT)
    return gfc_copy_expr (e->symtree->n.sym->ts.u.cl->length);

  return NULL;

}

/* Insert a block at the current position unless it has already
   been inserted; in this case use the one already there.  */

static gfc_namespace*
insert_block ()
{
  gfc_namespace *ns;

  /* If the block hasn't already been created, do so.  */
  if (inserted_block == NULL)
    {
      inserted_block = XCNEW (gfc_code);
      inserted_block->op = EXEC_BLOCK;
      inserted_block->loc = (*current_code)->loc;
      ns = gfc_build_block_ns (current_ns);
      inserted_block->ext.block.ns = ns;
      inserted_block->ext.block.assoc = NULL;

      ns->code = *current_code;

      /* If the statement has a label,  make sure it is transferred to
	 the newly created block.  */

      if ((*current_code)->here)
	{
	  inserted_block->here = (*current_code)->here;
	  (*current_code)->here = NULL;
	}

      inserted_block->next = (*current_code)->next;
      changed_statement = &(inserted_block->ext.block.ns->code);
      (*current_code)->next = NULL;
      /* Insert the BLOCK at the right position.  */
      *current_code = inserted_block;
      ns->parent = current_ns;
    }
  else
    ns = inserted_block->ext.block.ns;

  return ns;
}

/* Returns a new expression (a variable) to be used in place of the old one,
   with an optional assignment statement before the current statement to set
   the value of the variable. Creates a new BLOCK for the statement if that
   hasn't already been done and puts the statement, plus the newly created
   variables, in that block.  Special cases: If the expression is constant or
   a temporary which has already been created, just copy it.  */

static gfc_expr*
create_var (gfc_expr * e, const char *vname)
{
  char name[GFC_MAX_SYMBOL_LEN +1];
  gfc_symtree *symtree;
  gfc_symbol *symbol;
  gfc_expr *result;
  gfc_code *n;
  gfc_namespace *ns;
  int i;
  bool deferred;

  if (e->expr_type == EXPR_CONSTANT || is_fe_temp (e))
    return gfc_copy_expr (e);

  ns = insert_block ();

  if (vname)
    snprintf (name, GFC_MAX_SYMBOL_LEN, "__var_%d_%s", var_num++, vname);
  else
    snprintf (name, GFC_MAX_SYMBOL_LEN, "__var_%d", var_num++);

  if (gfc_get_sym_tree (name, ns, &symtree, false) != 0)
    gcc_unreachable ();

  symbol = symtree->n.sym;
  symbol->ts = e->ts;

  if (e->rank > 0)
    {
      symbol->as = gfc_get_array_spec ();
      symbol->as->rank = e->rank;

      if (e->shape == NULL)
	{
	  /* We don't know the shape at compile time, so we use an
	     allocatable.  */
	  symbol->as->type = AS_DEFERRED;
	  symbol->attr.allocatable = 1;
	}
      else
	{
	  symbol->as->type = AS_EXPLICIT;
	  /* Copy the shape.  */
	  for (i=0; i<e->rank; i++)
	    {
	      gfc_expr *p, *q;

	      p = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
					 &(e->where));
	      mpz_set_si (p->value.integer, 1);
	      symbol->as->lower[i] = p;

	      q = gfc_get_constant_expr (BT_INTEGER, gfc_index_integer_kind,
					 &(e->where));
	      mpz_set (q->value.integer, e->shape[i]);
	      symbol->as->upper[i] = q;
	    }
	}
    }

  deferred = 0;
  if (e->ts.type == BT_CHARACTER && e->rank == 0)
    {
      gfc_expr *length;

      symbol->ts.u.cl = gfc_new_charlen (ns, NULL);
      length = constant_string_length (e);
      if (length)
	symbol->ts.u.cl->length = length;
      else
	{
	  symbol->attr.allocatable = 1;
	  deferred = 1;
	}
    }

  symbol->attr.flavor = FL_VARIABLE;
  symbol->attr.referenced = 1;
  symbol->attr.dimension = e->rank > 0;
  symbol->attr.fe_temp = 1;
  gfc_commit_symbol (symbol);

  result = gfc_get_expr ();
  result->expr_type = EXPR_VARIABLE;
  result->ts = e->ts;
  result->ts.deferred = deferred;
  result->rank = e->rank;
  result->shape = gfc_copy_shape (e->shape, e->rank);
  result->symtree = symtree;
  result->where = e->where;
  if (e->rank > 0)
    {
      result->ref = gfc_get_ref ();
      result->ref->type = REF_ARRAY;
      result->ref->u.ar.type = AR_FULL;
      result->ref->u.ar.where = e->where;
      result->ref->u.ar.dimen = e->rank;
      result->ref->u.ar.as = symbol->ts.type == BT_CLASS
			     ? CLASS_DATA (symbol)->as : symbol->as;
      if (warn_array_temporaries)
	gfc_warning (OPT_Warray_temporaries,
		     "Creating array temporary at %L", &(e->where));
    }

  /* Generate the new assignment.  */
  n = XCNEW (gfc_code);
  n->op = EXEC_ASSIGN;
  n->loc = (*current_code)->loc;
  n->next = *changed_statement;
  n->expr1 = gfc_copy_expr (result);
  n->expr2 = e;
  *changed_statement = n;
  n_vars ++;

  return result;
}

/* Warn about function elimination.  */

static void
do_warn_function_elimination (gfc_expr *e)
{
  if (e->expr_type != EXPR_FUNCTION)
    return;
  if (e->value.function.esym)
    gfc_warning (OPT_Wfunction_elimination,
		 "Removing call to function %qs at %L",
		 e->value.function.esym->name, &(e->where));
  else if (e->value.function.isym)
    gfc_warning (OPT_Wfunction_elimination,
		 "Removing call to function %qs at %L",
		 e->value.function.isym->name, &(e->where));
}
/* Callback function for the code walker for doing common function
   elimination.  This builds up the list of functions in the expression
   and goes through them to detect duplicates, which it then replaces
   by variables.  */

static int
cfe_expr_0 (gfc_expr **e, int *walk_subtrees,
	  void *data ATTRIBUTE_UNUSED)
{
  int i,j;
  gfc_expr *newvar;
  gfc_expr **ei, **ej;

  /* Don't do this optimization within OMP workshare or ASSOC lists.  */

  if (in_omp_workshare || in_assoc_list)
    {
      *walk_subtrees = 0;
      return 0;
    }

  expr_array.release ();

  gfc_expr_walker (e, cfe_register_funcs, NULL);

  /* Walk through all the functions.  */

  FOR_EACH_VEC_ELT_FROM (expr_array, i, ei, 1)
    {
      /* Skip if the function has been replaced by a variable already.  */
      if ((*ei)->expr_type == EXPR_VARIABLE)
	continue;

      newvar = NULL;
      for (j=0; j<i; j++)
	{
	  ej = expr_array[j];
	  if (gfc_dep_compare_functions (*ei, *ej, true) == 0)
	    {
	      if (newvar == NULL)
		newvar = create_var (*ei, "fcn");

	      if (warn_function_elimination)
		do_warn_function_elimination (*ej);

	      free (*ej);
	      *ej = gfc_copy_expr (newvar);
	    }
	}
      if (newvar)
	*ei = newvar;
    }

  /* We did all the necessary walking in this function.  */
  *walk_subtrees = 0;
  return 0;
}

/* Callback function for common function elimination, called from
   gfc_code_walker.  This keeps track of the current code, in order
   to insert statements as needed.  */

static int
cfe_code (gfc_code **c, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  current_code = c;
  inserted_block = NULL;
  changed_statement = NULL;

  /* Do not do anything inside a WHERE statement; scalar assignments, BLOCKs
     and allocation on assigment are prohibited inside WHERE, and finally
     masking an expression would lead to wrong-code when replacing

     WHERE (a>0)
       b = sum(foo(a) + foo(a))
     END WHERE

     with

     WHERE (a > 0)
       tmp = foo(a)
       b = sum(tmp + tmp)
     END WHERE
*/

  if ((*c)->op == EXEC_WHERE)
    {
      *walk_subtrees = 0;
      return 0;
    }


  return 0;
}

/* Dummy function for expression call back, for use when we
   really don't want to do any walking.  */

static int
dummy_expr_callback (gfc_expr **e ATTRIBUTE_UNUSED, int *walk_subtrees,
		     void *data ATTRIBUTE_UNUSED)
{
  *walk_subtrees = 0;
  return 0;
}

/* Dummy function for code callback, for use when we really
   don't want to do anything.  */
int
gfc_dummy_code_callback (gfc_code **e ATTRIBUTE_UNUSED,
			 int *walk_subtrees ATTRIBUTE_UNUSED,
			 void *data ATTRIBUTE_UNUSED)
{
  return 0;
}

/* Code callback function for converting
   do while(a)
   end do
   into the equivalent
   do
     if (.not. a) exit
   end do
   This is because common function elimination would otherwise place the
   temporary variables outside the loop.  */

static int
convert_do_while (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
		  void *data ATTRIBUTE_UNUSED)
{
  gfc_code *co = *c;
  gfc_code *c_if1, *c_if2, *c_exit;
  gfc_code *loopblock;
  gfc_expr *e_not, *e_cond;

  if (co->op != EXEC_DO_WHILE)
    return 0;

  if (co->expr1 == NULL || co->expr1->expr_type == EXPR_CONSTANT)
    return 0;

  e_cond = co->expr1;

  /* Generate the condition of the if statement, which is .not. the original
     statement.  */
  e_not = gfc_get_expr ();
  e_not->ts = e_cond->ts;
  e_not->where = e_cond->where;
  e_not->expr_type = EXPR_OP;
  e_not->value.op.op = INTRINSIC_NOT;
  e_not->value.op.op1 = e_cond;

  /* Generate the EXIT statement.  */
  c_exit = XCNEW (gfc_code);
  c_exit->op = EXEC_EXIT;
  c_exit->ext.which_construct = co;
  c_exit->loc = co->loc;

  /* Generate the IF statement.  */
  c_if2 = XCNEW (gfc_code);
  c_if2->op = EXEC_IF;
  c_if2->expr1 = e_not;
  c_if2->next = c_exit;
  c_if2->loc = co->loc;

  /* ... plus the one to chain it to.  */
  c_if1 = XCNEW (gfc_code);
  c_if1->op = EXEC_IF;
  c_if1->block = c_if2;
  c_if1->loc = co->loc;

  /* Make the DO WHILE loop into a DO block by replacing the condition
     with a true constant.  */
  co->expr1 = gfc_get_logical_expr (gfc_default_integer_kind, &co->loc, true);

  /* Hang the generated if statement into the loop body.  */

  loopblock = co->block->next;
  co->block->next = c_if1;
  c_if1->next = loopblock;

  return 0;
}

/* Code callback function for converting
   if (a) then
   ...
   else if (b) then
   end if

   into
   if (a) then
   else
     if (b) then
     end if
   end if

   because otherwise common function elimination would place the BLOCKs
   into the wrong place.  */

static int
convert_elseif (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
		void *data ATTRIBUTE_UNUSED)
{
  gfc_code *co = *c;
  gfc_code *c_if1, *c_if2, *else_stmt;

  if (co->op != EXEC_IF)
    return 0;

  /* This loop starts out with the first ELSE statement.  */
  else_stmt = co->block->block;

  while (else_stmt != NULL)
    {
      gfc_code *next_else;

      /* If there is no condition, we're done.  */
      if (else_stmt->expr1 == NULL)
	break;

      next_else = else_stmt->block;

      /* Generate the new IF statement.  */
      c_if2 = XCNEW (gfc_code);
      c_if2->op = EXEC_IF;
      c_if2->expr1 = else_stmt->expr1;
      c_if2->next = else_stmt->next;
      c_if2->loc = else_stmt->loc;
      c_if2->block = next_else;

      /* ... plus the one to chain it to.  */
      c_if1 = XCNEW (gfc_code);
      c_if1->op = EXEC_IF;
      c_if1->block = c_if2;
      c_if1->loc = else_stmt->loc;

      /* Insert the new IF after the ELSE.  */
      else_stmt->expr1 = NULL;
      else_stmt->next = c_if1;
      else_stmt->block = NULL;

      else_stmt = next_else;
    }
  /*  Don't walk subtrees.  */
  return 0;
}

struct do_stack
{
  struct do_stack *prev;
  gfc_iterator *iter;
  gfc_code *code;
} *stack_top;

/* Recursively traverse the block of a WRITE or READ statement, and maybe
   optimize by replacing do loops with their analog array slices.  For
   example:

     write (*,*) (a(i), i=1,4)

   is replaced with

     write (*,*) a(1:4:1) .  */

static bool
traverse_io_block (gfc_code *code, bool *has_reached, gfc_code *prev)
{
  gfc_code *curr;
  gfc_expr *new_e, *expr, *start;
  gfc_ref *ref;
  struct do_stack ds_push;
  int i, future_rank = 0;
  gfc_iterator *iters[GFC_MAX_DIMENSIONS];
  gfc_expr *e;

  /* Find the first transfer/do statement.  */
  for (curr = code; curr; curr = curr->next)
    {
      if (curr->op == EXEC_DO || curr->op == EXEC_TRANSFER)
	break;
    }

  /* Ensure it is the only transfer/do statement because cases like

     write (*,*) (a(i), b(i), i=1,4)

     cannot be optimized.  */

  if (!curr || curr->next)
    return false;

  if (curr->op == EXEC_DO)
    {
      if (curr->ext.iterator->var->ref)
	return false;
      ds_push.prev = stack_top;
      ds_push.iter = curr->ext.iterator;
      ds_push.code = curr;
      stack_top = &ds_push;
      if (traverse_io_block (curr->block->next, has_reached, prev))
	{
	  if (curr != stack_top->code && !*has_reached)
	    {
	      curr->block->next = NULL;
	      gfc_free_statements (curr);
	    }
	  else
	    *has_reached = true;
	  return true;
	}
      return false;
    }

  gcc_assert (curr->op == EXEC_TRANSFER);

  /* FIXME: Workaround for PR 80945 - array slices with deferred character
     lenghts do not work.  Remove this section when the PR is fixed.  */
  e = curr->expr1;
  if (e->expr_type == EXPR_VARIABLE && e->ts.type == BT_CHARACTER
      && e->ts.deferred)
    return false;
  /* End of section to be removed.  */

  ref = e->ref;
  if (!ref || ref->type != REF_ARRAY || ref->u.ar.codimen != 0 || ref->next)
    return false;

  /* Find the iterators belonging to each variable and check conditions.  */
  for (i = 0; i < ref->u.ar.dimen; i++)
    {
      if (!ref->u.ar.start[i] || ref->u.ar.start[i]->ref
	  || ref->u.ar.dimen_type[i] != DIMEN_ELEMENT)
	return false;

      start = ref->u.ar.start[i];
      gfc_simplify_expr (start, 0);
      switch (start->expr_type)
	{
	case EXPR_VARIABLE:

	  /* write (*,*) (a(i), i=a%b,1) not handled yet.  */
	  if (start->ref)
	    return false;

	  /*  Check for (a(k), i=1,4) or ((a(j, i), i=1,4), j=1,4).  */
	  if (!stack_top || !stack_top->iter
	      || stack_top->iter->var->symtree != start->symtree)
	    {
	      /* Check for (a(i,i), i=1,3).  */
	      int j;
	      
	      for (j=0; j<i; j++)
		if (iters[j] && iters[j]->var->symtree == start->symtree)
		  return false;

	      iters[i] = NULL;
	    }
	  else
	    {
	      iters[i] = stack_top->iter;
	      stack_top = stack_top->prev;
	      future_rank++;
	    }
	  break;
	case EXPR_CONSTANT:
	  iters[i] = NULL;
	  break;
	case EXPR_OP:
	  switch (start->value.op.op)
	    {
	    case INTRINSIC_PLUS:
	    case INTRINSIC_TIMES:
	      if (start->value.op.op1->expr_type != EXPR_VARIABLE)
		std::swap (start->value.op.op1, start->value.op.op2);
	      gcc_fallthrough ();
	    case INTRINSIC_MINUS:
	      if ((start->value.op.op1->expr_type!= EXPR_VARIABLE
		   && start->value.op.op2->expr_type != EXPR_CONSTANT)
		  || start->value.op.op1->ref)
		return false;
	      if (!stack_top || !stack_top->iter
		  || stack_top->iter->var->symtree
		  != start->value.op.op1->symtree)
		return false;
	      iters[i] = stack_top->iter;
	      stack_top = stack_top->prev;
	      break;
	    default:
	      return false;
	    }
	  future_rank++;
	  break;
	default:
	  return false;
	}
    }

  /* Create new expr.  */
  new_e = gfc_copy_expr (curr->expr1);
  new_e->expr_type = EXPR_VARIABLE;
  new_e->rank = future_rank;
  if (curr->expr1->shape)
    new_e->shape = gfc_get_shape (new_e->rank);

  /* Assign new starts, ends and strides if necessary.  */
  for (i = 0; i < ref->u.ar.dimen; i++)
    {
      if (!iters[i])
	continue;
      start = ref->u.ar.start[i];
      switch (start->expr_type)
	{
	case EXPR_CONSTANT:
	  gfc_internal_error ("bad expression");
	  break;
	case EXPR_VARIABLE:
	  new_e->ref->u.ar.dimen_type[i] = DIMEN_RANGE;
	  new_e->ref->u.ar.type = AR_SECTION;
	  gfc_free_expr (new_e->ref->u.ar.start[i]);
	  new_e->ref->u.ar.start[i] = gfc_copy_expr (iters[i]->start);
	  new_e->ref->u.ar.end[i] = gfc_copy_expr (iters[i]->end);
	  new_e->ref->u.ar.stride[i] = gfc_copy_expr (iters[i]->step);
	  break;
	case EXPR_OP:
	  new_e->ref->u.ar.dimen_type[i] = DIMEN_RANGE;
	  new_e->ref->u.ar.type = AR_SECTION;
	  gfc_free_expr (new_e->ref->u.ar.start[i]);
	  expr = gfc_copy_expr (start);
	  expr->value.op.op1 = gfc_copy_expr (iters[i]->start);
	  new_e->ref->u.ar.start[i] = expr;
	  gfc_simplify_expr (new_e->ref->u.ar.start[i], 0);
	  expr = gfc_copy_expr (start);
	  expr->value.op.op1 = gfc_copy_expr (iters[i]->end);
	  new_e->ref->u.ar.end[i] = expr;
	  gfc_simplify_expr (new_e->ref->u.ar.end[i], 0);
	  switch (start->value.op.op)
	    {
	    case INTRINSIC_MINUS:
	    case INTRINSIC_PLUS:
	      new_e->ref->u.ar.stride[i] = gfc_copy_expr (iters[i]->step);
	      break;
	    case INTRINSIC_TIMES:
	      expr = gfc_copy_expr (start);
	      expr->value.op.op1 = gfc_copy_expr (iters[i]->step);
	      new_e->ref->u.ar.stride[i] = expr;
	      gfc_simplify_expr (new_e->ref->u.ar.stride[i], 0);
	      break;
	    default:
	      gfc_internal_error ("bad op");
	    }
	  break;
	default:
	  gfc_internal_error ("bad expression");
	}
    }
  curr->expr1 = new_e;

  /* Insert modified statement. Check whether the statement needs to be
     inserted at the lowest level.  */
  if (!stack_top->iter)
    {
      if (prev)
	{
	  curr->next = prev->next->next;
	  prev->next = curr;
	}
      else
	{
	  curr->next = stack_top->code->block->next->next->next;
	  stack_top->code->block->next = curr;
	}
    }
  else
    stack_top->code->block->next = curr;
  return true;
}

/* Function for the gfc_code_walker.  If code is a READ or WRITE statement, it
   tries to optimize its block.  */

static int
simplify_io_impl_do (gfc_code **code, int *walk_subtrees,
		     void *data ATTRIBUTE_UNUSED)
{
  gfc_code **curr, *prev = NULL;
  struct do_stack write, first;
  bool b = false;
  *walk_subtrees = 1;
  if (!(*code)->block
      || ((*code)->block->op != EXEC_WRITE
	  && (*code)->block->op != EXEC_READ))
    return 0;

  *walk_subtrees = 0;
  write.prev = NULL;
  write.iter = NULL;
  write.code = *code;

  for (curr = &(*code)->block; *curr; curr = &(*curr)->next)
    {
      if ((*curr)->op == EXEC_DO)
	{
	  first.prev = &write;
	  first.iter = (*curr)->ext.iterator;
	  first.code = *curr;
	  stack_top = &first;
	  traverse_io_block ((*curr)->block->next, &b, prev);
	  stack_top = NULL;
	}
      prev = *curr;
    }
  return 0;
}

/* Optimize a namespace, including all contained namespaces.
  flag_frontend_optimize and flag_fronend_loop_interchange are
  handled separately.  */

static void
optimize_namespace (gfc_namespace *ns)
{
  gfc_namespace *saved_ns = gfc_current_ns;
  current_ns = ns;
  gfc_current_ns = ns;
  forall_level = 0;
  iterator_level = 0;
  in_assoc_list = false;
  in_omp_workshare = false;

  if (flag_frontend_optimize)
    {
      gfc_code_walker (&ns->code, simplify_io_impl_do, dummy_expr_callback, NULL);
      gfc_code_walker (&ns->code, convert_do_while, dummy_expr_callback, NULL);
      gfc_code_walker (&ns->code, convert_elseif, dummy_expr_callback, NULL);
      gfc_code_walker (&ns->code, cfe_code, cfe_expr_0, NULL);
      gfc_code_walker (&ns->code, optimize_code, optimize_expr, NULL);
      if (flag_inline_matmul_limit != 0)
	{
	  bool found;
	  do
	    {
	      found = false;
	      gfc_code_walker (&ns->code, matmul_to_var_code, matmul_to_var_expr,
			       (void *) &found);
	    }
	  while (found);

	  gfc_code_walker (&ns->code, matmul_temp_args, dummy_expr_callback,
			   NULL);
	  gfc_code_walker (&ns->code, inline_matmul_assign, dummy_expr_callback,
			   NULL);
	}
    }

  if (flag_frontend_loop_interchange)
    gfc_code_walker (&ns->code, index_interchange, dummy_expr_callback,
		     NULL);

  /* BLOCKs are handled in the expression walker below.  */
  for (ns = ns->contained; ns; ns = ns->sibling)
    {
      if (ns->code == NULL || ns->code->op != EXEC_BLOCK)
	optimize_namespace (ns);
    }
  gfc_current_ns = saved_ns;
}

/* Handle dependencies for allocatable strings which potentially redefine
   themselves in an assignment.  */

static void
realloc_strings (gfc_namespace *ns)
{
  current_ns = ns;
  gfc_code_walker (&ns->code, realloc_string_callback, dummy_expr_callback, NULL);

  for (ns = ns->contained; ns; ns = ns->sibling)
    {
      if (ns->code == NULL || ns->code->op != EXEC_BLOCK)
	realloc_strings (ns);
    }

}

static void
optimize_reduction (gfc_namespace *ns)
{
  current_ns = ns;
  gfc_code_walker (&ns->code, gfc_dummy_code_callback,
		   callback_reduction, NULL);

/* BLOCKs are handled in the expression walker below.  */
  for (ns = ns->contained; ns; ns = ns->sibling)
    {
      if (ns->code == NULL || ns->code->op != EXEC_BLOCK)
	optimize_reduction (ns);
    }
}

/* Replace code like
   a = matmul(b,c) + d
   with
   a = matmul(b,c) ;   a = a + d
   where the array function is not elemental and not allocatable
   and does not depend on the left-hand side.
*/

static bool
optimize_binop_array_assignment (gfc_code *c, gfc_expr **rhs, bool seen_op)
{
  gfc_expr *e;

  if (!*rhs)
    return false;

  e = *rhs;
  if (e->expr_type == EXPR_OP)
    {
      switch (e->value.op.op)
	{
	  /* Unary operators and exponentiation: Only look at a single
	     operand.  */
	case INTRINSIC_NOT:
	case INTRINSIC_UPLUS:
	case INTRINSIC_UMINUS:
	case INTRINSIC_PARENTHESES:
	case INTRINSIC_POWER:
	  if (optimize_binop_array_assignment (c, &e->value.op.op1, seen_op))
	    return true;
	  break;

	case INTRINSIC_CONCAT:
	  /* Do not do string concatenations.  */
	  break;

	default:
	  /* Binary operators.  */
	  if (optimize_binop_array_assignment (c, &e->value.op.op1, true))
	    return true;

	  if (optimize_binop_array_assignment (c, &e->value.op.op2, true))
	    return true;

	  break;
	}
    }
  else if (seen_op && e->expr_type == EXPR_FUNCTION && e->rank > 0
	   && ! (e->value.function.esym
		 && (e->value.function.esym->attr.elemental
		     || e->value.function.esym->attr.allocatable
		     || e->value.function.esym->ts.type != c->expr1->ts.type
		     || e->value.function.esym->ts.kind != c->expr1->ts.kind))
	   && ! (e->value.function.isym
		 && (e->value.function.isym->elemental
		     || e->ts.type != c->expr1->ts.type
		     || e->ts.kind != c->expr1->ts.kind))
	   && ! gfc_inline_intrinsic_function_p (e))
    {

      gfc_code *n;
      gfc_expr *new_expr;

      /* Insert a new assignment statement after the current one.  */
      n = XCNEW (gfc_code);
      n->op = EXEC_ASSIGN;
      n->loc = c->loc;
      n->next = c->next;
      c->next = n;

      n->expr1 = gfc_copy_expr (c->expr1);
      n->expr2 = c->expr2;
      new_expr = gfc_copy_expr (c->expr1);
      c->expr2 = e;
      *rhs = new_expr;

      return true;

    }

  /* Nothing to optimize.  */
  return false;
}

/* Remove unneeded TRIMs at the end of expressions.  */

static bool
remove_trim (gfc_expr *rhs)
{
  bool ret;

  ret = false;
  if (!rhs)
    return ret;

  /* Check for a // b // trim(c).  Looping is probably not
     necessary because the parser usually generates
     (// (// a b ) trim(c) ) , but better safe than sorry.  */

  while (rhs->expr_type == EXPR_OP
	 && rhs->value.op.op == INTRINSIC_CONCAT)
    rhs = rhs->value.op.op2;

  while (rhs->expr_type == EXPR_FUNCTION && rhs->value.function.isym
	 && rhs->value.function.isym->id == GFC_ISYM_TRIM)
    {
      strip_function_call (rhs);
      /* Recursive call to catch silly stuff like trim ( a // trim(b)).  */
      remove_trim (rhs);
      ret = true;
    }

  return ret;
}

/* Optimizations for an assignment.  */

static void
optimize_assignment (gfc_code * c)
{
  gfc_expr *lhs, *rhs;

  lhs = c->expr1;
  rhs = c->expr2;

  if (lhs->ts.type == BT_CHARACTER && !lhs->ts.deferred)
    {
      /* Optimize  a = trim(b)  to  a = b.  */
      remove_trim (rhs);

      /* Replace a = '   ' by a = '' to optimize away a memcpy.  */
      if (is_empty_string (rhs))
	rhs->value.character.length = 0;
    }

  if (lhs->rank > 0 && gfc_check_dependency (lhs, rhs, true) == 0)
    optimize_binop_array_assignment (c, &rhs, false);
}


/* Remove an unneeded function call, modifying the expression.
   This replaces the function call with the value of its
   first argument.  The rest of the argument list is freed.  */

static void
strip_function_call (gfc_expr *e)
{
  gfc_expr *e1;
  gfc_actual_arglist *a;

  a = e->value.function.actual;

  /* We should have at least one argument.  */
  gcc_assert (a->expr != NULL);

  e1 = a->expr;

  /* Free the remaining arglist, if any.  */
  if (a->next)
    gfc_free_actual_arglist (a->next);

  /* Graft the argument expression onto the original function.  */
  *e = *e1;
  free (e1);

}

/* Optimization of lexical comparison functions.  */

static bool
optimize_lexical_comparison (gfc_expr *e)
{
  if (e->expr_type != EXPR_FUNCTION || e->value.function.isym == NULL)
    return false;

  switch (e->value.function.isym->id)
    {
    case GFC_ISYM_LLE:
      return optimize_comparison (e, INTRINSIC_LE);

    case GFC_ISYM_LGE:
      return optimize_comparison (e, INTRINSIC_GE);

    case GFC_ISYM_LGT:
      return optimize_comparison (e, INTRINSIC_GT);

    case GFC_ISYM_LLT:
      return optimize_comparison (e, INTRINSIC_LT);

    default:
      break;
    }
  return false;
}

/* Combine stuff like [a]>b into [a>b], for easier optimization later.  Do not
   do CHARACTER because of possible pessimization involving character
   lengths.  */

static bool
combine_array_constructor (gfc_expr *e)
{

  gfc_expr *op1, *op2;
  gfc_expr *scalar;
  gfc_expr *new_expr;
  gfc_constructor *c, *new_c;
  gfc_constructor_base oldbase, newbase;
  bool scalar_first;
  int n_elem;
  bool all_const;

  /* Array constructors have rank one.  */
  if (e->rank != 1)
    return false;

  /* Don't try to combine association lists, this makes no sense
     and leads to an ICE.  */
  if (in_assoc_list)
    return false;

  /* With FORALL, the BLOCKS created by create_var will cause an ICE.  */
  if (forall_level > 0)
    return false;

  /* Inside an iterator, things can get hairy; we are likely to create
     an invalid temporary variable.  */
  if (iterator_level > 0)
    return false;

  op1 = e->value.op.op1;
  op2 = e->value.op.op2;

  if (!op1 || !op2)
    return false;

  if (op1->expr_type == EXPR_ARRAY && op2->rank == 0)
    scalar_first = false;
  else if (op2->expr_type == EXPR_ARRAY && op1->rank == 0)
    {
      scalar_first = true;
      op1 = e->value.op.op2;
      op2 = e->value.op.op1;
    }
  else
    return false;

  if (op2->ts.type == BT_CHARACTER)
    return false;

  /* This might be an expanded constructor with very many constant values. If
     we perform the operation here, we might end up with a long compile time
     and actually longer execution time, so a length bound is in order here.
     If the constructor constains something which is not a constant, it did
     not come from an expansion, so leave it alone.  */

#define CONSTR_LEN_MAX 4

  oldbase = op1->value.constructor;

  n_elem = 0;
  all_const = true;
  for (c = gfc_constructor_first (oldbase); c; c = gfc_constructor_next(c))
    {
      if (c->expr->expr_type != EXPR_CONSTANT)
	{
	  all_const = false;
	  break;
	}
      n_elem += 1;
    }

  if (all_const && n_elem > CONSTR_LEN_MAX)
    return false;

#undef CONSTR_LEN_MAX

  newbase = NULL;
  e->expr_type = EXPR_ARRAY;

  scalar = create_var (gfc_copy_expr (op2), "constr");

  for (c = gfc_constructor_first (oldbase); c;
       c = gfc_constructor_next (c))
    {
      new_expr = gfc_get_expr ();
      new_expr->ts = e->ts;
      new_expr->expr_type = EXPR_OP;
      new_expr->rank = c->expr->rank;
      new_expr->where = c->expr->where;
      new_expr->value.op.op = e->value.op.op;

      if (scalar_first)
	{
	  new_expr->value.op.op1 = gfc_copy_expr (scalar);
	  new_expr->value.op.op2 = gfc_copy_expr (c->expr);
	}
      else
	{
	  new_expr->value.op.op1 = gfc_copy_expr (c->expr);
	  new_expr->value.op.op2 = gfc_copy_expr (scalar);
	}

      new_c = gfc_constructor_append_expr (&newbase, new_expr, &(e->where));
      new_c->iterator = c->iterator;
      c->iterator = NULL;
    }

  gfc_free_expr (op1);
  gfc_free_expr (op2);
  gfc_free_expr (scalar);

  e->value.constructor = newbase;
  return true;
}

/* Change (-1)**k into 1-ishift(iand(k,1),1) and
 2**k into ishift(1,k) */

static bool
optimize_power (gfc_expr *e)
{
  gfc_expr *op1, *op2;
  gfc_expr *iand, *ishft;

  if (e->ts.type != BT_INTEGER)
    return false;

  op1 = e->value.op.op1;

  if (op1 == NULL || op1->expr_type != EXPR_CONSTANT)
    return false;

  if (mpz_cmp_si (op1->value.integer, -1L) == 0)
    {
      gfc_free_expr (op1);

      op2 = e->value.op.op2;

      if (op2 == NULL)
	return false;

      iand = gfc_build_intrinsic_call (current_ns, GFC_ISYM_IAND,
				       "_internal_iand", e->where, 2, op2,
				       gfc_get_int_expr (e->ts.kind,
							 &e->where, 1));

      ishft = gfc_build_intrinsic_call (current_ns, GFC_ISYM_ISHFT,
					"_internal_ishft", e->where, 2, iand,
					gfc_get_int_expr (e->ts.kind,
							  &e->where, 1));

      e->value.op.op = INTRINSIC_MINUS;
      e->value.op.op1 = gfc_get_int_expr (e->ts.kind, &e->where, 1);
      e->value.op.op2 = ishft;
      return true;
    }
  else if (mpz_cmp_si (op1->value.integer, 2L) == 0)
    {
      gfc_free_expr (op1);

      op2 = e->value.op.op2;
      if (op2 == NULL)
	return false;

      ishft = gfc_build_intrinsic_call (current_ns, GFC_ISYM_ISHFT,
					"_internal_ishft", e->where, 2,
					gfc_get_int_expr (e->ts.kind,
							  &e->where, 1),
					op2);
      *e = *ishft;
      return true;
    }

  else if (mpz_cmp_si (op1->value.integer, 1L) == 0)
    {
      op2 = e->value.op.op2;
      if (op2 == NULL)
	return false;

      gfc_free_expr (op1);
      gfc_free_expr (op2);

      e->expr_type = EXPR_CONSTANT;
      e->value.op.op1 = NULL;
      e->value.op.op2 = NULL;
      mpz_init_set_si (e->value.integer, 1);
      /* Typespec and location are still OK.  */
      return true;
    }

  return false;
}

/* Recursive optimization of operators.  */

static bool
optimize_op (gfc_expr *e)
{
  bool changed;

  gfc_intrinsic_op op = e->value.op.op;

  changed = false;

  /* Only use new-style comparisons.  */
  switch(op)
    {
    case INTRINSIC_EQ_OS:
      op = INTRINSIC_EQ;
      break;

    case INTRINSIC_GE_OS:
      op = INTRINSIC_GE;
      break;

    case INTRINSIC_LE_OS:
      op = INTRINSIC_LE;
      break;

    case INTRINSIC_NE_OS:
      op = INTRINSIC_NE;
      break;

    case INTRINSIC_GT_OS:
      op = INTRINSIC_GT;
      break;

    case INTRINSIC_LT_OS:
      op = INTRINSIC_LT;
      break;

    default:
      break;
    }

  switch (op)
    {
    case INTRINSIC_EQ:
    case INTRINSIC_GE:
    case INTRINSIC_LE:
    case INTRINSIC_NE:
    case INTRINSIC_GT:
    case INTRINSIC_LT:
      changed = optimize_comparison (e, op);

      gcc_fallthrough ();
      /* Look at array constructors.  */
    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
      return combine_array_constructor (e) || changed;

    case INTRINSIC_POWER:
      return optimize_power (e);

    default:
      break;
    }

  return false;
}


/* Return true if a constant string contains only blanks.  */

static bool
is_empty_string (gfc_expr *e)
{
  int i;

  if (e->ts.type != BT_CHARACTER || e->expr_type != EXPR_CONSTANT)
    return false;

  for (i=0; i < e->value.character.length; i++)
    {
      if (e->value.character.string[i] != ' ')
	return false;
    }

  return true;
}


/* Insert a call to the intrinsic len_trim. Use a different name for
   the symbol tree so we don't run into trouble when the user has
   renamed len_trim for some reason.  */

static gfc_expr*
get_len_trim_call (gfc_expr *str, int kind)
{
  gfc_expr *fcn;
  gfc_actual_arglist *actual_arglist, *next;

  fcn = gfc_get_expr ();
  fcn->expr_type = EXPR_FUNCTION;
  fcn->value.function.isym = gfc_intrinsic_function_by_id (GFC_ISYM_LEN_TRIM);
  actual_arglist = gfc_get_actual_arglist ();
  actual_arglist->expr = str;
  next = gfc_get_actual_arglist ();
  next->expr = gfc_get_int_expr (gfc_default_integer_kind, NULL, kind);
  actual_arglist->next = next;

  fcn->value.function.actual = actual_arglist;
  fcn->where = str->where;
  fcn->ts.type = BT_INTEGER;
  fcn->ts.kind = gfc_charlen_int_kind;

  gfc_get_sym_tree ("__internal_len_trim", current_ns, &fcn->symtree, false);
  fcn->symtree->n.sym->ts = fcn->ts;
  fcn->symtree->n.sym->attr.flavor = FL_PROCEDURE;
  fcn->symtree->n.sym->attr.function = 1;
  fcn->symtree->n.sym->attr.elemental = 1;
  fcn->symtree->n.sym->attr.referenced = 1;
  fcn->symtree->n.sym->attr.access = ACCESS_PRIVATE;
  gfc_commit_symbol (fcn->symtree->n.sym);

  return fcn;
}

/* Optimize expressions for equality.  */

static bool
optimize_comparison (gfc_expr *e, gfc_intrinsic_op op)
{
  gfc_expr *op1, *op2;
  bool change;
  int eq;
  bool result;
  gfc_actual_arglist *firstarg, *secondarg;

  if (e->expr_type == EXPR_OP)
    {
      firstarg = NULL;
      secondarg = NULL;
      op1 = e->value.op.op1;
      op2 = e->value.op.op2;
    }
  else if (e->expr_type == EXPR_FUNCTION)
    {
      /* One of the lexical comparison functions.  */
      firstarg = e->value.function.actual;
      secondarg = firstarg->next;
      op1 = firstarg->expr;
      op2 = secondarg->expr;
    }
  else
    gcc_unreachable ();

  /* Strip off unneeded TRIM calls from string comparisons.  */

  change = remove_trim (op1);

  if (remove_trim (op2))
    change = true;

  /* An expression of type EXPR_CONSTANT is only valid for scalars.  */
  /* TODO: A scalar constant may be acceptable in some cases (the scalarizer
     handles them well). However, there are also cases that need a non-scalar
     argument. For example the any intrinsic. See PR 45380.  */
  if (e->rank > 0)
    return change;

  /* Replace a == '' with len_trim(a) == 0 and a /= '' with
     len_trim(a) != 0 */
  if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER
      && (op == INTRINSIC_EQ || op == INTRINSIC_NE))
    {
      bool empty_op1, empty_op2;
      empty_op1 = is_empty_string (op1);
      empty_op2 = is_empty_string (op2);

      if (empty_op1 || empty_op2)
	{
	  gfc_expr *fcn;
	  gfc_expr *zero;
	  gfc_expr *str;

	  /* This can only happen when an error for comparing
	     characters of different kinds has already been issued.  */
	  if (empty_op1 && empty_op2)
	    return false;

	  zero = gfc_get_int_expr (gfc_charlen_int_kind, &e->where, 0);
	  str = empty_op1 ? op2 : op1;

	  fcn = get_len_trim_call (str, gfc_charlen_int_kind);


	  if (empty_op1)
	    gfc_free_expr (op1);
	  else
	    gfc_free_expr (op2);

	  op1 = fcn;
	  op2 = zero;
	  e->value.op.op1 = fcn;
	  e->value.op.op2 = zero;
	}
    }


  /* Don't compare REAL or COMPLEX expressions when honoring NaNs.  */

  if (flag_finite_math_only
      || (op1->ts.type != BT_REAL && op2->ts.type != BT_REAL
	  && op1->ts.type != BT_COMPLEX && op2->ts.type != BT_COMPLEX))
    {
      eq = gfc_dep_compare_expr (op1, op2);
      if (eq <= -2)
	{
	  /* Replace A // B < A // C with B < C, and A // B < C // B
	     with A < C.  */
	  if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER
	      && op1->expr_type == EXPR_OP
	      && op1->value.op.op == INTRINSIC_CONCAT
	      && op2->expr_type == EXPR_OP
	      && op2->value.op.op == INTRINSIC_CONCAT)
	    {
	      gfc_expr *op1_left = op1->value.op.op1;
	      gfc_expr *op2_left = op2->value.op.op1;
	      gfc_expr *op1_right = op1->value.op.op2;
	      gfc_expr *op2_right = op2->value.op.op2;

	      if (gfc_dep_compare_expr (op1_left, op2_left) == 0)
		{
		  /* Watch out for 'A ' // x vs. 'A' // x.  */

		  if (op1_left->expr_type == EXPR_CONSTANT
			&& op2_left->expr_type == EXPR_CONSTANT
			&& op1_left->value.character.length
			   != op2_left->value.character.length)
		    return change;
		  else
		    {
		      free (op1_left);
		      free (op2_left);
		      if (firstarg)
			{
			  firstarg->expr = op1_right;
			  secondarg->expr = op2_right;
			}
		      else
			{
			  e->value.op.op1 = op1_right;
			  e->value.op.op2 = op2_right;
			}
		      optimize_comparison (e, op);
		      return true;
		    }
		}
	      if (gfc_dep_compare_expr (op1_right, op2_right) == 0)
		{
		  free (op1_right);
		  free (op2_right);
		  if (firstarg)
		    {
		      firstarg->expr = op1_left;
		      secondarg->expr = op2_left;
		    }
		  else
		    {
		      e->value.op.op1 = op1_left;
		      e->value.op.op2 = op2_left;
		    }

		  optimize_comparison (e, op);
		  return true;
		}
	    }
	}
      else
	{
	  /* eq can only be -1, 0 or 1 at this point.  */
	  switch (op)
	    {
	    case INTRINSIC_EQ:
	      result = eq == 0;
	      break;

	    case INTRINSIC_GE:
	      result = eq >= 0;
	      break;

	    case INTRINSIC_LE:
	      result = eq <= 0;
	      break;

	    case INTRINSIC_NE:
	      result = eq != 0;
	      break;

	    case INTRINSIC_GT:
	      result = eq > 0;
	      break;

	    case INTRINSIC_LT:
	      result = eq < 0;
	      break;

	    default:
	      gfc_internal_error ("illegal OP in optimize_comparison");
	      break;
	    }

	  /* Replace the expression by a constant expression.  The typespec
	     and where remains the way it is.  */
	  free (op1);
	  free (op2);
	  e->expr_type = EXPR_CONSTANT;
	  e->value.logical = result;
	  return true;
	}
    }

  return change;
}

/* Optimize a trim function by replacing it with an equivalent substring
   involving a call to len_trim.  This only works for expressions where
   variables are trimmed.  Return true if anything was modified.  */

static bool
optimize_trim (gfc_expr *e)
{
  gfc_expr *a;
  gfc_ref *ref;
  gfc_expr *fcn;
  gfc_ref **rr = NULL;

  /* Don't do this optimization within an argument list, because
     otherwise aliasing issues may occur.  */

  if (count_arglist != 1)
    return false;

  if (e->ts.type != BT_CHARACTER || e->expr_type != EXPR_FUNCTION
      || e->value.function.isym == NULL
      || e->value.function.isym->id != GFC_ISYM_TRIM)
    return false;

  a = e->value.function.actual->expr;

  if (a->expr_type != EXPR_VARIABLE)
    return false;

  /* This would pessimize the idiom a = trim(a) for reallocatable strings.  */

  if (a->symtree->n.sym->attr.allocatable)
    return false;

  /* Follow all references to find the correct place to put the newly
     created reference.  FIXME:  Also handle substring references and
     array references.  Array references cause strange regressions at
     the moment.  */

  if (a->ref)
    {
      for (rr = &(a->ref); *rr; rr = &((*rr)->next))
	{
	  if ((*rr)->type == REF_SUBSTRING || (*rr)->type == REF_ARRAY)
	    return false;
	}
    }

  strip_function_call (e);

  if (e->ref == NULL)
    rr = &(e->ref);

  /* Create the reference.  */

  ref = gfc_get_ref ();
  ref->type = REF_SUBSTRING;

  /* Set the start of the reference.  */

  ref->u.ss.start = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);

  /* Build the function call to len_trim(x, gfc_default_integer_kind).  */

  fcn = get_len_trim_call (gfc_copy_expr (e), gfc_default_integer_kind);

  /* Set the end of the reference to the call to len_trim.  */

  ref->u.ss.end = fcn;
  gcc_assert (rr != NULL && *rr == NULL);
  *rr = ref;
  return true;
}

/* Optimize minloc(b), where b is rank 1 array, into
   (/ minloc(b, dim=1) /), and similarly for maxloc,
   as the latter forms are expanded inline.  */

static void
optimize_minmaxloc (gfc_expr **e)
{
  gfc_expr *fn = *e;
  gfc_actual_arglist *a;
  char *name, *p;

  if (fn->rank != 1
      || fn->value.function.actual == NULL
      || fn->value.function.actual->expr == NULL
      || fn->value.function.actual->expr->rank != 1)
    return;

  *e = gfc_get_array_expr (fn->ts.type, fn->ts.kind, &fn->where);
  (*e)->shape = fn->shape;
  fn->rank = 0;
  fn->shape = NULL;
  gfc_constructor_append_expr (&(*e)->value.constructor, fn, &fn->where);

  name = XALLOCAVEC (char, strlen (fn->value.function.name) + 1);
  strcpy (name, fn->value.function.name);
  p = strstr (name, "loc0");
  p[3] = '1';
  fn->value.function.name = gfc_get_string ("%s", name);
  if (fn->value.function.actual->next)
    {
      a = fn->value.function.actual->next;
      gcc_assert (a->expr == NULL);
    }
  else
    {
      a = gfc_get_actual_arglist ();
      fn->value.function.actual->next = a;
    }
  a->expr = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
				   &fn->where);
  mpz_set_ui (a->expr->value.integer, 1);
}

/* Callback function for code checking that we do not pass a DO variable to an
   INTENT(OUT) or INTENT(INOUT) dummy variable.  */

static int
doloop_code (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
	 void *data ATTRIBUTE_UNUSED)
{
  gfc_code *co;
  int i;
  gfc_formal_arglist *f;
  gfc_actual_arglist *a;
  gfc_code *cl;
  do_t loop, *lp;
  bool seen_goto;

  co = *c;

  /* If the doloop_list grew, we have to truncate it here.  */

  if ((unsigned) doloop_level < doloop_list.length())
    doloop_list.truncate (doloop_level);

  seen_goto = false;
  switch (co->op)
    {
    case EXEC_DO:

      if (co->ext.iterator && co->ext.iterator->var)
	loop.c = co;
      else
	loop.c = NULL;

      loop.branch_level = if_level + select_level;
      loop.seen_goto = false;
      doloop_list.safe_push (loop);
      break;

      /* If anything could transfer control away from a suspicious
	 subscript, make sure to set seen_goto in the current DO loop
	 (if any).  */
    case EXEC_GOTO:
    case EXEC_EXIT:
    case EXEC_STOP:
    case EXEC_ERROR_STOP:
    case EXEC_CYCLE:
      seen_goto = true;
      break;

    case EXEC_OPEN:
      if (co->ext.open->err)
	seen_goto = true;
      break;

    case EXEC_CLOSE:
      if (co->ext.close->err)
	seen_goto = true;
      break;

    case EXEC_BACKSPACE:
    case EXEC_ENDFILE:
    case EXEC_REWIND:
    case EXEC_FLUSH:

      if (co->ext.filepos->err)
	seen_goto = true;
      break;

    case EXEC_INQUIRE:
      if (co->ext.filepos->err)
	seen_goto = true;
      break;

    case EXEC_READ:
    case EXEC_WRITE:
      if (co->ext.dt->err || co->ext.dt->end || co->ext.dt->eor)
	seen_goto = true;
      break;

    case EXEC_WAIT:
      if (co->ext.wait->err || co->ext.wait->end || co->ext.wait->eor)
	loop.seen_goto = true;
      break;

    case EXEC_CALL:

      if (co->resolved_sym == NULL)
	break;

      f = gfc_sym_get_dummy_args (co->resolved_sym);

      /* Withot a formal arglist, there is only unknown INTENT,
	 which we don't check for.  */
      if (f == NULL)
	break;

      a = co->ext.actual;

      while (a && f)
	{
	  FOR_EACH_VEC_ELT (doloop_list, i, lp)
	    {
	      gfc_symbol *do_sym;
	      cl = lp->c;

	      if (cl == NULL)
		break;

	      do_sym = cl->ext.iterator->var->symtree->n.sym;

	      if (a->expr && a->expr->symtree
		  && a->expr->symtree->n.sym == do_sym)
		{
		  if (f->sym->attr.intent == INTENT_OUT)
		    gfc_error_now ("Variable %qs at %L set to undefined "
				   "value inside loop  beginning at %L as "
				   "INTENT(OUT) argument to subroutine %qs",
				   do_sym->name, &a->expr->where,
				   &(doloop_list[i].c->loc),
				   co->symtree->n.sym->name);
		  else if (f->sym->attr.intent == INTENT_INOUT)
		    gfc_error_now ("Variable %qs at %L not definable inside "
				   "loop beginning at %L as INTENT(INOUT) "
				   "argument to subroutine %qs",
				   do_sym->name, &a->expr->where,
				   &(doloop_list[i].c->loc),
				   co->symtree->n.sym->name);
		}
	    }
	  a = a->next;
	  f = f->next;
	}
      break;

    default:
      break;
    }
  if (seen_goto && doloop_level > 0)
    doloop_list[doloop_level-1].seen_goto = true;

  return 0;
}

/* Callback function to warn about different things within DO loops.  */

static int
do_function (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
	     void *data ATTRIBUTE_UNUSED)
{
  do_t *last;

  if (doloop_list.length () == 0)
    return 0;

  if ((*e)->expr_type == EXPR_FUNCTION)
    do_intent (e);

  last = &doloop_list.last();
  if (last->seen_goto && !warn_do_subscript)
    return 0;

  if ((*e)->expr_type == EXPR_VARIABLE)
    do_subscript (e);

  return 0;
}

typedef struct
{
  gfc_symbol *sym;
  mpz_t val;
} insert_index_t;

/* Callback function - if the expression is the variable in data->sym,
   replace it with a constant from data->val.  */

static int
callback_insert_index (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
		       void *data)
{
  insert_index_t *d;
  gfc_expr *ex, *n;

  ex = (*e);
  if (ex->expr_type != EXPR_VARIABLE)
    return 0;

  d = (insert_index_t *) data;
  if (ex->symtree->n.sym != d->sym)
    return 0;

  n = gfc_get_constant_expr (BT_INTEGER, ex->ts.kind, &ex->where);
  mpz_set (n->value.integer, d->val);

  gfc_free_expr (ex);
  *e = n;
  return 0;
}

/* In the expression e, replace occurrences of the variable sym with
   val.  If this results in a constant expression, return true and
   return the value in ret.  Return false if the expression already
   is a constant.  Caller has to clear ret in that case.  */

static bool
insert_index (gfc_expr *e, gfc_symbol *sym, mpz_t val, mpz_t ret)
{
  gfc_expr *n;
  insert_index_t data;
  bool rc;

  if (e->expr_type == EXPR_CONSTANT)
    return false;

  n = gfc_copy_expr (e);
  data.sym = sym;
  mpz_init_set (data.val, val);
  gfc_expr_walker (&n, callback_insert_index, (void *) &data);
  gfc_simplify_expr (n, 0);

  if (n->expr_type == EXPR_CONSTANT)
    {
      rc = true;
      mpz_init_set (ret, n->value.integer);
    }
  else
    rc = false;

  mpz_clear (data.val);
  gfc_free_expr (n);
  return rc;

}

/* Check array subscripts for possible out-of-bounds accesses in DO
   loops with constant bounds.  */

static int
do_subscript (gfc_expr **e)
{
  gfc_expr *v;
  gfc_array_ref *ar;
  gfc_ref *ref;
  int i,j;
  gfc_code *dl;
  do_t *lp;

  v = *e;
  /* Constants are already checked.  */
  if (v->expr_type == EXPR_CONSTANT)
    return 0;

  /* Wrong warnings will be generated in an associate list.  */
  if (in_assoc_list)
    return 0;

  for (ref = v->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_ARRAY && ref->u.ar.type == AR_ELEMENT)
	{
	  ar = & ref->u.ar;
	  FOR_EACH_VEC_ELT (doloop_list, j, lp)
	    {
	      gfc_symbol *do_sym;
	      mpz_t do_start, do_step, do_end;
	      bool have_do_start, have_do_end;
	      bool error_not_proven;
	      int warn;

	      dl = lp->c;
	      if (dl == NULL)
		break;

	      /* If we are within a branch, or a goto or equivalent
		 was seen in the DO loop before, then we cannot prove that
		 this expression is actually evaluated.  Don't do anything
		 unless we want to see it all.  */
	      error_not_proven = lp->seen_goto
		|| lp->branch_level < if_level + select_level;

	      if (error_not_proven && !warn_do_subscript)
		break;

	      if (error_not_proven)
		warn = OPT_Wdo_subscript;
	      else
		warn = 0;

	      do_sym = dl->ext.iterator->var->symtree->n.sym;
	      if (do_sym->ts.type != BT_INTEGER)
		continue;

	      /* If we do not know about the stepsize, the loop may be zero trip.
		 Do not warn in this case.  */
	  
	      if (dl->ext.iterator->step->expr_type == EXPR_CONSTANT)
		mpz_init_set (do_step, dl->ext.iterator->step->value.integer);
	      else
		continue;

	      if (dl->ext.iterator->start->expr_type == EXPR_CONSTANT)
		{
		  have_do_start = true;
		  mpz_init_set (do_start, dl->ext.iterator->start->value.integer);
		}
	      else
		have_do_start = false;

	  
	      if (dl->ext.iterator->end->expr_type == EXPR_CONSTANT)
		{
		  have_do_end = true;
		  mpz_init_set (do_end, dl->ext.iterator->end->value.integer);
		}
	      else
		have_do_end = false;

	      if (!have_do_start && !have_do_end)
		return 0;

	      /* May have to correct the end value if the step does not equal
		 one.  */
	      if (have_do_start && have_do_end && mpz_cmp_ui (do_step, 1) != 0)
		{
		  mpz_t diff, rem;

		  mpz_init (diff);
		  mpz_init (rem);
		  mpz_sub (diff, do_end, do_start);
		  mpz_tdiv_r (rem, diff, do_step);
		  mpz_sub (do_end, do_end, rem);
		  mpz_clear (diff);
		  mpz_clear (rem);
		}

	      for (i = 0; i< ar->dimen; i++)
		{
		  mpz_t val;
		  if (ar->dimen_type[i] == DIMEN_ELEMENT && have_do_start
		      && insert_index (ar->start[i], do_sym, do_start, val))
		    {
		      if (ar->as->lower[i]
			  && ar->as->lower[i]->expr_type == EXPR_CONSTANT
			  && mpz_cmp (val, ar->as->lower[i]->value.integer) < 0)
			gfc_warning (warn, "Array reference at %L out of bounds "
				     "(%ld < %ld) in loop beginning at %L",
				     &ar->start[i]->where, mpz_get_si (val),
				     mpz_get_si (ar->as->lower[i]->value.integer),
				     &doloop_list[j].c->loc);

		      if (ar->as->upper[i]
			  && ar->as->upper[i]->expr_type == EXPR_CONSTANT
			  && mpz_cmp (val, ar->as->upper[i]->value.integer) > 0)
			    gfc_warning (warn, "Array reference at %L out of bounds "
					 "(%ld > %ld) in loop beginning at %L",
					 &ar->start[i]->where, mpz_get_si (val),
					 mpz_get_si (ar->as->upper[i]->value.integer),
					 &doloop_list[j].c->loc);

		      mpz_clear (val);
		    }

		  if (ar->dimen_type[i] == DIMEN_ELEMENT && have_do_end
		      && insert_index (ar->start[i], do_sym, do_end, val))
		    {
		      if (ar->as->lower[i]
			  && ar->as->lower[i]->expr_type == EXPR_CONSTANT
			  && mpz_cmp (val, ar->as->lower[i]->value.integer) < 0)
			gfc_warning (warn, "Array reference at %L out of bounds "
				     "(%ld < %ld) in loop beginning at %L",
				     &ar->start[i]->where, mpz_get_si (val),
				     mpz_get_si (ar->as->lower[i]->value.integer),
				     &doloop_list[j].c->loc);

		      if (ar->as->upper[i]
			  && ar->as->upper[i]->expr_type == EXPR_CONSTANT
			  && mpz_cmp (val, ar->as->upper[i]->value.integer) > 0)
			gfc_warning (warn, "Array reference at %L out of bounds "
				     "(%ld > %ld) in loop beginning at %L",
				     &ar->start[i]->where, mpz_get_si (val),
				     mpz_get_si (ar->as->upper[i]->value.integer),
				     &doloop_list[j].c->loc);

		      mpz_clear (val);
		    }
		}
	    }
	}
    }
  return 0;
}
/* Function for functions checking that we do not pass a DO variable
   to an INTENT(OUT) or INTENT(INOUT) dummy variable.  */

static int
do_intent (gfc_expr **e)
{
  gfc_formal_arglist *f;
  gfc_actual_arglist *a;
  gfc_expr *expr;
  gfc_code *dl;
  do_t *lp;
  int i;

  expr = *e;
  if (expr->expr_type != EXPR_FUNCTION)
    return 0;

  /* Intrinsic functions don't modify their arguments.  */

  if (expr->value.function.isym)
    return 0;

  f = gfc_sym_get_dummy_args (expr->symtree->n.sym);

  /* Without a formal arglist, there is only unknown INTENT,
     which we don't check for.  */
  if (f == NULL)
    return 0;

  a = expr->value.function.actual;

  while (a && f)
    {
      FOR_EACH_VEC_ELT (doloop_list, i, lp)
	{
	  gfc_symbol *do_sym;
	  dl = lp->c;
	  if (dl == NULL)
	    break;

	  do_sym = dl->ext.iterator->var->symtree->n.sym;

	  if (a->expr && a->expr->symtree
	      && a->expr->symtree->n.sym == do_sym)
	    {
	      if (f->sym->attr.intent == INTENT_OUT)
		gfc_error_now ("Variable %qs at %L set to undefined value "
			       "inside loop beginning at %L as INTENT(OUT) "
			       "argument to function %qs", do_sym->name,
			       &a->expr->where, &doloop_list[i].c->loc,
			       expr->symtree->n.sym->name);
	      else if (f->sym->attr.intent == INTENT_INOUT)
		gfc_error_now ("Variable %qs at %L not definable inside loop"
			       " beginning at %L as INTENT(INOUT) argument to"
			       " function %qs", do_sym->name,
			       &a->expr->where, &doloop_list[i].c->loc,
			       expr->symtree->n.sym->name);
	    }
	}
      a = a->next;
      f = f->next;
    }

  return 0;
}

static void
doloop_warn (gfc_namespace *ns)
{
  gfc_code_walker (&ns->code, doloop_code, do_function, NULL);
}

/* This selction deals with inlining calls to MATMUL.  */

/* Replace calls to matmul outside of straight assignments with a temporary
   variable so that later inlining will work.  */

static int
matmul_to_var_expr (gfc_expr **ep, int *walk_subtrees ATTRIBUTE_UNUSED,
		    void *data)
{
  gfc_expr *e, *n;
  bool *found = (bool *) data;
  
  e = *ep;

  if (e->expr_type != EXPR_FUNCTION
      || e->value.function.isym == NULL
      || e->value.function.isym->id != GFC_ISYM_MATMUL)
    return 0;

  if (forall_level > 0 || iterator_level > 0 || in_omp_workshare
      || in_where)
    return 0;

  /* Check if this is already in the form c = matmul(a,b).  */
  
  if ((*current_code)->expr2 == e)
    return 0;

  n = create_var (e, "matmul");
  
  /* If create_var is unable to create a variable (for example if
     -fno-realloc-lhs is in force with a variable that does not have bounds
     known at compile-time), just return.  */

  if (n == NULL)
    return 0;
  
  *ep = n;
  *found = true;
  return 0;
}

/* Set current_code and associated variables so that matmul_to_var_expr can
   work.  */

static int
matmul_to_var_code (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
		    void *data ATTRIBUTE_UNUSED)
{
  if (current_code != c)
    {
      current_code = c;
      inserted_block = NULL;
      changed_statement = NULL;
    }
  
  return 0;
}


/* Take a statement of the shape c = matmul(a,b) and create temporaries
   for a and b if there is a dependency between the arguments and the
   result variable or if a or b are the result of calculations that cannot
   be handled by the inliner.  */

static int
matmul_temp_args (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
		  void *data ATTRIBUTE_UNUSED)
{
  gfc_expr *expr1, *expr2;
  gfc_code *co;
  gfc_actual_arglist *a, *b;
  bool a_tmp, b_tmp;
  gfc_expr *matrix_a, *matrix_b;
  bool conjg_a, conjg_b, transpose_a, transpose_b;
  
  co = *c;

  if (co->op != EXEC_ASSIGN)
    return 0;

  if (forall_level > 0 || iterator_level > 0 || in_omp_workshare
      || in_where)
    return 0;

  /* This has some duplication with inline_matmul_assign.  This
     is because the creation of temporary variables could still fail,
     and inline_matmul_assign still needs to be able to handle these
     cases.  */
  expr1 = co->expr1;
  expr2 = co->expr2;

  if (expr2->expr_type != EXPR_FUNCTION
      || expr2->value.function.isym == NULL
      || expr2->value.function.isym->id != GFC_ISYM_MATMUL)
    return 0;

  a_tmp = false;
  a = expr2->value.function.actual;
  matrix_a = check_conjg_transpose_variable (a->expr, &conjg_a, &transpose_a);
  if (matrix_a != NULL)
    {
      if (matrix_a->expr_type == EXPR_VARIABLE
	  && (gfc_check_dependency (matrix_a, expr1, true)
	      || has_dimen_vector_ref (matrix_a)))
	a_tmp = true;
    }
  else
    a_tmp = true;

  b_tmp = false;
  b = a->next;
  matrix_b = check_conjg_transpose_variable (b->expr, &conjg_b, &transpose_b);
  if (matrix_b != NULL)
    {
      if (matrix_b->expr_type == EXPR_VARIABLE
	  && (gfc_check_dependency (matrix_b, expr1, true)
	      || has_dimen_vector_ref (matrix_b)))
	b_tmp = true;
    }
  else
    b_tmp = true;

  if (!a_tmp && !b_tmp)
    return 0;
  
  current_code = c;
  inserted_block = NULL;
  changed_statement = NULL;
  if (a_tmp)
    {
      gfc_expr *at;
      at = create_var (a->expr,"mma");
      if (at)
	a->expr = at;
    }
  if (b_tmp)
    {
      gfc_expr *bt;
      bt = create_var (b->expr,"mmb");
      if (bt)
	b->expr = bt;
    }
  return 0;
}

/* Auxiliary function to build and simplify an array inquiry function.
   dim is zero-based.  */

static gfc_expr *
get_array_inq_function (gfc_isym_id id, gfc_expr *e, int dim)
{
  gfc_expr *fcn;
  gfc_expr *dim_arg, *kind;
  const char *name;
  gfc_expr *ec;

  switch (id)
    {
    case GFC_ISYM_LBOUND:
      name = "_gfortran_lbound";
      break;

    case GFC_ISYM_UBOUND:
      name = "_gfortran_ubound";
      break;

    case GFC_ISYM_SIZE:
      name = "_gfortran_size";
      break;

    default:
      gcc_unreachable ();
    }

  dim_arg =  gfc_get_int_expr (gfc_default_integer_kind, &e->where, dim);
  kind = gfc_get_int_expr (gfc_default_integer_kind, &e->where,
			   gfc_index_integer_kind);

  ec = gfc_copy_expr (e);
  fcn = gfc_build_intrinsic_call (current_ns, id, name, e->where, 3,
				  ec, dim_arg,  kind);
  gfc_simplify_expr (fcn, 0);
  return fcn;
}

/* Builds a logical expression.  */

static gfc_expr*
build_logical_expr (gfc_intrinsic_op op, gfc_expr *e1, gfc_expr *e2)
{
  gfc_typespec ts;
  gfc_expr *res;

  ts.type = BT_LOGICAL;
  ts.kind = gfc_default_logical_kind;
  res = gfc_get_expr ();
  res->where = e1->where;
  res->expr_type = EXPR_OP;
  res->value.op.op = op;
  res->value.op.op1 = e1;
  res->value.op.op2 = e2;
  res->ts = ts;

  return res;
}


/* Return an operation of one two gfc_expr (one if e2 is NULL). This assumes
   compatible typespecs.  */

static gfc_expr *
get_operand (gfc_intrinsic_op op, gfc_expr *e1, gfc_expr *e2)
{
  gfc_expr *res;

  res = gfc_get_expr ();
  res->ts = e1->ts;
  res->where = e1->where;
  res->expr_type = EXPR_OP;
  res->value.op.op = op;
  res->value.op.op1 = e1;
  res->value.op.op2 = e2;
  gfc_simplify_expr (res, 0);
  return res;
}

/* Generate the IF statement for a runtime check if we want to do inlining or
   not - putting in the code for both branches and putting it into the syntax
   tree is the caller's responsibility.  For fixed array sizes, this should be
   removed by DCE. Only called for rank-two matrices A and B.  */

static gfc_code *
inline_limit_check (gfc_expr *a, gfc_expr *b, enum matrix_case m_case)
{
  gfc_expr *inline_limit;
  gfc_code *if_1, *if_2, *else_2;
  gfc_expr *b2, *a2, *a1, *m1, *m2;
  gfc_typespec ts;
  gfc_expr *cond;

  gcc_assert (m_case == A2B2 || m_case == A2B2T || m_case == A2TB2);

  /* Calculation is done in real to avoid integer overflow.  */

  inline_limit = gfc_get_constant_expr (BT_REAL, gfc_default_real_kind,
					&a->where);
  mpfr_set_si (inline_limit->value.real, flag_inline_matmul_limit,
	       GFC_RND_MODE);
  mpfr_pow_ui (inline_limit->value.real, inline_limit->value.real, 3,
	       GFC_RND_MODE);

  a1 = get_array_inq_function (GFC_ISYM_SIZE, a, 1);
  a2 = get_array_inq_function (GFC_ISYM_SIZE, a, 2);
  b2 = get_array_inq_function (GFC_ISYM_SIZE, b, 2);

  gfc_clear_ts (&ts);
  ts.type = BT_REAL;
  ts.kind = gfc_default_real_kind;
  gfc_convert_type_warn (a1, &ts, 2, 0);
  gfc_convert_type_warn (a2, &ts, 2, 0);
  gfc_convert_type_warn (b2, &ts, 2, 0);

  m1 = get_operand (INTRINSIC_TIMES, a1, a2);
  m2 = get_operand (INTRINSIC_TIMES, m1, b2);

  cond = build_logical_expr (INTRINSIC_LE, m2, inline_limit);
  gfc_simplify_expr (cond, 0);

  else_2 = XCNEW (gfc_code);
  else_2->op = EXEC_IF;
  else_2->loc = a->where;

  if_2 = XCNEW (gfc_code);
  if_2->op = EXEC_IF;
  if_2->expr1 = cond;
  if_2->loc = a->where;
  if_2->block = else_2;

  if_1 = XCNEW (gfc_code);
  if_1->op = EXEC_IF;
  if_1->block = if_2;
  if_1->loc = a->where;

  return if_1;
}


/* Insert code to issue a runtime error if the expressions are not equal.  */

static gfc_code *
runtime_error_ne (gfc_expr *e1, gfc_expr *e2, const char *msg)
{
  gfc_expr *cond;
  gfc_code *if_1, *if_2;
  gfc_code *c;
  gfc_actual_arglist *a1, *a2, *a3;

  gcc_assert (e1->where.lb);
  /* Build the call to runtime_error.  */
  c = XCNEW (gfc_code);
  c->op = EXEC_CALL;
  c->loc = e1->where;

  /* Get a null-terminated message string.  */

  a1 = gfc_get_actual_arglist ();
  a1->expr = gfc_get_character_expr (gfc_default_character_kind, &e1->where,
				     msg, strlen(msg)+1);
  c->ext.actual = a1;

  /* Pass the value of the first expression.  */
  a2 = gfc_get_actual_arglist ();
  a2->expr = gfc_copy_expr (e1);
  a1->next = a2;

  /* Pass the value of the second expression.  */
  a3 = gfc_get_actual_arglist ();
  a3->expr = gfc_copy_expr (e2);
  a2->next = a3;

  gfc_check_fe_runtime_error (c->ext.actual);
  gfc_resolve_fe_runtime_error (c);

  if_2 = XCNEW (gfc_code);
  if_2->op = EXEC_IF;
  if_2->loc = e1->where;
  if_2->next = c;

  if_1 = XCNEW (gfc_code);
  if_1->op = EXEC_IF;
  if_1->block = if_2;
  if_1->loc = e1->where;

  cond = build_logical_expr (INTRINSIC_NE, e1, e2);
  gfc_simplify_expr (cond, 0);
  if_2->expr1 = cond;

  return if_1;
}

/* Handle matrix reallocation.  Caller is responsible to insert into
   the code tree.

   For the two-dimensional case, build

  if (allocated(c)) then
     if (size(c,1) /= size(a,1) .or. size(c,2) /= size(b,2)) then
        deallocate(c)
        allocate (c(size(a,1), size(b,2)))
     end if
  else
     allocate (c(size(a,1),size(b,2)))
  end if

  and for the other cases correspondingly.
*/

static gfc_code *
matmul_lhs_realloc (gfc_expr *c, gfc_expr *a, gfc_expr *b,
		    enum matrix_case m_case)
{

  gfc_expr *allocated, *alloc_expr;
  gfc_code *if_alloc_1, *if_alloc_2, *if_size_1, *if_size_2;
  gfc_code *else_alloc;
  gfc_code *deallocate, *allocate1, *allocate_else;
  gfc_array_ref *ar;
  gfc_expr *cond, *ne1, *ne2;

  if (warn_realloc_lhs)
    gfc_warning (OPT_Wrealloc_lhs,
		 "Code for reallocating the allocatable array at %L will "
		 "be added", &c->where);

  alloc_expr = gfc_copy_expr (c);

  ar = gfc_find_array_ref (alloc_expr);
  gcc_assert (ar && ar->type == AR_FULL);

  /* c comes in as a full ref.  Change it into a copy and make it into an
     element ref so it has the right form for for ALLOCATE.  In the same
     switch statement, also generate the size comparison for the secod IF
     statement.  */

  ar->type = AR_ELEMENT;

  switch (m_case)
    {
    case A2B2:
      ar->start[0] = get_array_inq_function (GFC_ISYM_SIZE, a, 1);
      ar->start[1] = get_array_inq_function (GFC_ISYM_SIZE, b, 2);
      ne1 = build_logical_expr (INTRINSIC_NE,
				get_array_inq_function (GFC_ISYM_SIZE, c, 1),
				get_array_inq_function (GFC_ISYM_SIZE, a, 1));
      ne2 = build_logical_expr (INTRINSIC_NE,
				get_array_inq_function (GFC_ISYM_SIZE, c, 2),
				get_array_inq_function (GFC_ISYM_SIZE, b, 2));
      cond = build_logical_expr (INTRINSIC_OR, ne1, ne2);
      break;

    case A2B2T:
      ar->start[0] = get_array_inq_function (GFC_ISYM_SIZE, a, 1);
      ar->start[1] = get_array_inq_function (GFC_ISYM_SIZE, b, 1);

      ne1 = build_logical_expr (INTRINSIC_NE,
				get_array_inq_function (GFC_ISYM_SIZE, c, 1),
				get_array_inq_function (GFC_ISYM_SIZE, a, 1));
      ne2 = build_logical_expr (INTRINSIC_NE,
				get_array_inq_function (GFC_ISYM_SIZE, c, 2),
				get_array_inq_function (GFC_ISYM_SIZE, b, 1));
      cond = build_logical_expr (INTRINSIC_OR, ne1, ne2);
      break;

    case A2TB2:

      ar->start[0] = get_array_inq_function (GFC_ISYM_SIZE, a, 2);
      ar->start[1] = get_array_inq_function (GFC_ISYM_SIZE, b, 2);

      ne1 = build_logical_expr (INTRINSIC_NE,
				get_array_inq_function (GFC_ISYM_SIZE, c, 1),
				get_array_inq_function (GFC_ISYM_SIZE, a, 2));
      ne2 = build_logical_expr (INTRINSIC_NE,
				get_array_inq_function (GFC_ISYM_SIZE, c, 2),
				get_array_inq_function (GFC_ISYM_SIZE, b, 2));
      cond = build_logical_expr (INTRINSIC_OR, ne1, ne2);
      break;

    case A2B1:
      ar->start[0] = get_array_inq_function (GFC_ISYM_SIZE, a, 1);
      cond = build_logical_expr (INTRINSIC_NE,
				 get_array_inq_function (GFC_ISYM_SIZE, c, 1),
				 get_array_inq_function (GFC_ISYM_SIZE, a, 2));
      break;

    case A1B2:
      ar->start[0] = get_array_inq_function (GFC_ISYM_SIZE, b, 2);
      cond = build_logical_expr (INTRINSIC_NE,
				 get_array_inq_function (GFC_ISYM_SIZE, c, 1),
				 get_array_inq_function (GFC_ISYM_SIZE, b, 2));
      break;

    default:
      gcc_unreachable();

    }

  gfc_simplify_expr (cond, 0);

  /* We need two identical allocate statements in two
     branches of the IF statement.  */

  allocate1 = XCNEW (gfc_code);
  allocate1->op = EXEC_ALLOCATE;
  allocate1->ext.alloc.list = gfc_get_alloc ();
  allocate1->loc = c->where;
  allocate1->ext.alloc.list->expr = gfc_copy_expr (alloc_expr);

  allocate_else = XCNEW (gfc_code);
  allocate_else->op = EXEC_ALLOCATE;
  allocate_else->ext.alloc.list = gfc_get_alloc ();
  allocate_else->loc = c->where;
  allocate_else->ext.alloc.list->expr = alloc_expr;

  allocated = gfc_build_intrinsic_call (current_ns, GFC_ISYM_ALLOCATED,
					"_gfortran_allocated", c->where,
					1, gfc_copy_expr (c));

  deallocate = XCNEW (gfc_code);
  deallocate->op = EXEC_DEALLOCATE;
  deallocate->ext.alloc.list = gfc_get_alloc ();
  deallocate->ext.alloc.list->expr = gfc_copy_expr (c);
  deallocate->next = allocate1;
  deallocate->loc = c->where;

  if_size_2 = XCNEW (gfc_code);
  if_size_2->op = EXEC_IF;
  if_size_2->expr1 = cond;
  if_size_2->loc = c->where;
  if_size_2->next = deallocate;

  if_size_1 = XCNEW (gfc_code);
  if_size_1->op = EXEC_IF;
  if_size_1->block = if_size_2;
  if_size_1->loc = c->where;

  else_alloc = XCNEW (gfc_code);
  else_alloc->op = EXEC_IF;
  else_alloc->loc = c->where;
  else_alloc->next = allocate_else;

  if_alloc_2 = XCNEW (gfc_code);
  if_alloc_2->op = EXEC_IF;
  if_alloc_2->expr1 = allocated;
  if_alloc_2->loc = c->where;
  if_alloc_2->next = if_size_1;
  if_alloc_2->block = else_alloc;

  if_alloc_1 = XCNEW (gfc_code);
  if_alloc_1->op = EXEC_IF;
  if_alloc_1->block = if_alloc_2;
  if_alloc_1->loc = c->where;

  return if_alloc_1;
}

/* Callback function for has_function_or_op.  */

static int
is_function_or_op (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
	     void *data ATTRIBUTE_UNUSED)
{
  if ((*e) == 0)
    return 0;
  else
    return (*e)->expr_type == EXPR_FUNCTION
      || (*e)->expr_type == EXPR_OP;
}

/* Returns true if the expression contains a function.  */

static bool
has_function_or_op (gfc_expr **e)
{
  if (e == NULL)
    return false;
  else
    return gfc_expr_walker (e, is_function_or_op, NULL);
}

/* Freeze (assign to a temporary variable) a single expression.  */

static void
freeze_expr (gfc_expr **ep)
{
  gfc_expr *ne;
  if (has_function_or_op (ep))
    {
      ne = create_var (*ep, "freeze");
      *ep = ne;
    }
}

/* Go through an expression's references and assign them to temporary
   variables if they contain functions.  This is usually done prior to
   front-end scalarization to avoid multiple invocations of functions.  */

static void
freeze_references (gfc_expr *e)
{
  gfc_ref *r;
  gfc_array_ref *ar;
  int i;

  for (r=e->ref; r; r=r->next)
    {
      if (r->type == REF_SUBSTRING)
	{
	  if (r->u.ss.start != NULL)
	    freeze_expr (&r->u.ss.start);

	  if (r->u.ss.end != NULL)
	    freeze_expr (&r->u.ss.end);
	}
      else if (r->type == REF_ARRAY)
	{
	  ar = &r->u.ar;
	  switch (ar->type)
	    {
	    case AR_FULL:
	      break;

	    case AR_SECTION:
	      for (i=0; i<ar->dimen; i++)
		{
		  if (ar->dimen_type[i] == DIMEN_RANGE)
		    {
		      freeze_expr (&ar->start[i]);
		      freeze_expr (&ar->end[i]);
		      freeze_expr (&ar->stride[i]);
		    }
		  else if (ar->dimen_type[i] == DIMEN_ELEMENT)
		    {
		      freeze_expr (&ar->start[i]);
		    }
		}
	      break;

	    case AR_ELEMENT:
	      for (i=0; i<ar->dimen; i++)
		freeze_expr (&ar->start[i]);
	      break;

	    default:
	      break;
	    }
	}
    }
}

/* Convert to gfc_index_integer_kind if needed, just do a copy otherwise.  */

static gfc_expr *
convert_to_index_kind (gfc_expr *e)
{
  gfc_expr *res;

  gcc_assert (e != NULL);

  res = gfc_copy_expr (e);

  gcc_assert (e->ts.type == BT_INTEGER);

  if (res->ts.kind != gfc_index_integer_kind)
    {
      gfc_typespec ts;
      gfc_clear_ts (&ts);
      ts.type = BT_INTEGER;
      ts.kind = gfc_index_integer_kind;

      gfc_convert_type_warn (e, &ts, 2, 0);
    }

  return res;
}

/* Function to create a DO loop including creation of the
   iteration variable.  gfc_expr are copied.*/

static gfc_code *
create_do_loop (gfc_expr *start, gfc_expr *end, gfc_expr *step, locus *where,
		gfc_namespace *ns, char *vname)
{

  char name[GFC_MAX_SYMBOL_LEN +1];
  gfc_symtree *symtree;
  gfc_symbol *symbol;
  gfc_expr *i;
  gfc_code *n, *n2;

  /* Create an expression for the iteration variable.  */
  if (vname)
    sprintf (name, "__var_%d_do_%s", var_num++, vname);
  else
    sprintf (name, "__var_%d_do", var_num++);


  if (gfc_get_sym_tree (name, ns, &symtree, false) != 0)
    gcc_unreachable ();

  /* Create the loop variable.  */

  symbol = symtree->n.sym;
  symbol->ts.type = BT_INTEGER;
  symbol->ts.kind = gfc_index_integer_kind;
  symbol->attr.flavor = FL_VARIABLE;
  symbol->attr.referenced = 1;
  symbol->attr.dimension = 0;
  symbol->attr.fe_temp = 1;
  gfc_commit_symbol (symbol);

  i = gfc_get_expr ();
  i->expr_type = EXPR_VARIABLE;
  i->ts = symbol->ts;
  i->rank = 0;
  i->where = *where;
  i->symtree = symtree;

  /* ... and the nested DO statements.  */
  n = XCNEW (gfc_code);
  n->op = EXEC_DO;
  n->loc = *where;
  n->ext.iterator = gfc_get_iterator ();
  n->ext.iterator->var = i;
  n->ext.iterator->start = convert_to_index_kind (start);
  n->ext.iterator->end = convert_to_index_kind (end);
  if (step)
    n->ext.iterator->step = convert_to_index_kind (step);
  else
    n->ext.iterator->step = gfc_get_int_expr (gfc_index_integer_kind,
					      where, 1);

  n2 = XCNEW (gfc_code);
  n2->op = EXEC_DO;
  n2->loc = *where;
  n2->next = NULL;
  n->block = n2;
  return n;
}

/* Get the upper bound of the DO loops for matmul along a dimension.  This
 is one-based.  */

static gfc_expr*
get_size_m1 (gfc_expr *e, int dimen)
{
  mpz_t size;
  gfc_expr *res;

  if (gfc_array_dimen_size (e, dimen - 1, &size))
    {
      res = gfc_get_constant_expr (BT_INTEGER,
				   gfc_index_integer_kind, &e->where);
      mpz_sub_ui (res->value.integer, size, 1);
      mpz_clear (size);
    }
  else
    {
      res = get_operand (INTRINSIC_MINUS,
			 get_array_inq_function (GFC_ISYM_SIZE, e, dimen),
			 gfc_get_int_expr (gfc_index_integer_kind,
					   &e->where, 1));
      gfc_simplify_expr (res, 0);
    }

  return res;
}

/* Function to return a scalarized expression. It is assumed that indices are
 zero based to make generation of DO loops easier.  A zero as index will
 access the first element along a dimension.  Single element references will
 be skipped.  A NULL as an expression will be replaced by a full reference.
 This assumes that the index loops have gfc_index_integer_kind, and that all
 references have been frozen.  */

static gfc_expr*
scalarized_expr (gfc_expr *e_in, gfc_expr **index, int count_index)
{
  gfc_array_ref *ar;
  int i;
  int rank;
  gfc_expr *e;
  int i_index;
  bool was_fullref;

  e = gfc_copy_expr(e_in);

  rank = e->rank;

  ar = gfc_find_array_ref (e);

  /* We scalarize count_index variables, reducing the rank by count_index.  */

  e->rank = rank - count_index;

  was_fullref = ar->type == AR_FULL;

  if (e->rank == 0)
    ar->type = AR_ELEMENT;
  else
    ar->type = AR_SECTION;

  /* Loop over the indices.  For each index, create the expression
     index * stride + lbound(e, dim).  */

  i_index = 0;
  for (i=0; i < ar->dimen; i++)
    {
      if (was_fullref || ar->dimen_type[i] == DIMEN_RANGE)
	{
	  if (index[i_index] != NULL)
	    {
	      gfc_expr *lbound, *nindex;
	      gfc_expr *loopvar;

	      loopvar = gfc_copy_expr (index[i_index]);

	      if (ar->stride[i])
		{
		  gfc_expr *tmp;

		  tmp = gfc_copy_expr(ar->stride[i]);
		  if (tmp->ts.kind != gfc_index_integer_kind)
		    {
		      gfc_typespec ts;
		      gfc_clear_ts (&ts);
		      ts.type = BT_INTEGER;
		      ts.kind = gfc_index_integer_kind;
		      gfc_convert_type (tmp, &ts, 2);
		    }
		  nindex = get_operand (INTRINSIC_TIMES, loopvar, tmp);
		}
	      else
		nindex = loopvar;

	      /* Calculate the lower bound of the expression.  */
	      if (ar->start[i])
		{
		  lbound = gfc_copy_expr (ar->start[i]);
		  if (lbound->ts.kind != gfc_index_integer_kind)
		    {
		      gfc_typespec ts;
		      gfc_clear_ts (&ts);
		      ts.type = BT_INTEGER;
		      ts.kind = gfc_index_integer_kind;
		      gfc_convert_type (lbound, &ts, 2);

		    }
		}
	      else
		{
		  gfc_expr *lbound_e;
		  gfc_ref *ref;

		  lbound_e = gfc_copy_expr (e_in);

		  for (ref = lbound_e->ref; ref; ref = ref->next)
		    if (ref->type == REF_ARRAY
			&& (ref->u.ar.type == AR_FULL
			    || ref->u.ar.type == AR_SECTION))
		      break;

		  if (ref->next)
		    {
		      gfc_free_ref_list (ref->next);
		      ref->next = NULL;
		    }

		  if (!was_fullref)
		    {
		      /* Look at full individual sections, like a(:).  The first index
			 is the lbound of a full ref.  */
		      int j;
		      gfc_array_ref *ar;

		      ar = &ref->u.ar;
		      ar->type = AR_FULL;
		      for (j = 0; j < ar->dimen; j++)
			{
			  gfc_free_expr (ar->start[j]);
			  ar->start[j] = NULL;
			  gfc_free_expr (ar->end[j]);
			  ar->end[j] = NULL;
			  gfc_free_expr (ar->stride[j]);
			  ar->stride[j] = NULL;
			}

		      /* We have to get rid of the shape, if there is one.  Do
			 so by freeing it and calling gfc_resolve to rebuild
			 it, if necessary.  */

		      if (lbound_e->shape)
			gfc_free_shape (&(lbound_e->shape), lbound_e->rank);

		      lbound_e->rank = ar->dimen;
		      gfc_resolve_expr (lbound_e);
		    }
		  lbound = get_array_inq_function (GFC_ISYM_LBOUND, lbound_e,
						   i + 1);
		  gfc_free_expr (lbound_e);
		}

	      ar->dimen_type[i] = DIMEN_ELEMENT;

	      gfc_free_expr (ar->start[i]);
	      ar->start[i] = get_operand (INTRINSIC_PLUS, nindex, lbound);

	      gfc_free_expr (ar->end[i]);
	      ar->end[i] = NULL;
	      gfc_free_expr (ar->stride[i]);
	      ar->stride[i] = NULL;
	      gfc_simplify_expr (ar->start[i], 0);
	    }
	  else if (was_fullref)
	    {
	      gfc_internal_error ("Scalarization using DIMEN_RANGE unimplemented");
	    }
	  i_index ++;
	}
    }

  return e;
}

/* Helper function to check for a dimen vector as subscript.  */

static bool
has_dimen_vector_ref (gfc_expr *e)
{
  gfc_array_ref *ar;
  int i;

  ar = gfc_find_array_ref (e);
  gcc_assert (ar);
  if (ar->type == AR_FULL)
    return false;

  for (i=0; i<ar->dimen; i++)
    if (ar->dimen_type[i] == DIMEN_VECTOR)
      return true;

  return false;
}

/* If handed an expression of the form

   TRANSPOSE(CONJG(A))

   check if A can be handled by matmul and return if there is an uneven number
   of CONJG calls.  Return a pointer to the array when everything is OK, NULL
   otherwise. The caller has to check for the correct rank.  */

static gfc_expr*
check_conjg_transpose_variable (gfc_expr *e, bool *conjg, bool *transpose)
{
  *conjg = false;
  *transpose = false;

  do
    {
      if (e->expr_type == EXPR_VARIABLE)
	{
	  gcc_assert (e->rank == 1 || e->rank == 2);
	  return e;
	}
      else if (e->expr_type == EXPR_FUNCTION)
	{
	  if (e->value.function.isym == NULL)
	    return NULL;

	  if (e->value.function.isym->id == GFC_ISYM_CONJG)
	    *conjg = !*conjg;
	  else if (e->value.function.isym->id == GFC_ISYM_TRANSPOSE)
	    *transpose = !*transpose;
	  else return NULL;
	}
      else
	return NULL;

      e = e->value.function.actual->expr;
    }
  while(1);

  return NULL;
}

/* Inline assignments of the form c = matmul(a,b).
   Handle only the cases currently where b and c are rank-two arrays.

   This basically translates the code to

   BLOCK
     integer i,j,k
     c = 0
     do j=0, size(b,2)-1
       do k=0, size(a, 2)-1
         do i=0, size(a, 1)-1
            c(i * stride(c,1) + lbound(c,1), j * stride(c,2) + lbound(c,2)) =
	    c(i * stride(c,1) + lbound(c,1), j * stride(c,2) + lbound(c,2)) +
            a(i * stride(a,1) + lbound(a,1), k * stride(a,2) + lbound(a,2)) *
            b(k * stride(b,1) + lbound(b,1), j * stride(b,2) + lbound(b,2))
         end do
       end do
     end do
   END BLOCK

*/

static int
inline_matmul_assign (gfc_code **c, int *walk_subtrees,
			  void *data ATTRIBUTE_UNUSED)
{
  gfc_code *co = *c;
  gfc_expr *expr1, *expr2;
  gfc_expr *matrix_a, *matrix_b;
  gfc_actual_arglist *a, *b;
  gfc_code *do_1, *do_2, *do_3, *assign_zero, *assign_matmul;
  gfc_expr *zero_e;
  gfc_expr *u1, *u2, *u3;
  gfc_expr *list[2];
  gfc_expr *ascalar, *bscalar, *cscalar;
  gfc_expr *mult;
  gfc_expr *var_1, *var_2, *var_3;
  gfc_expr *zero;
  gfc_namespace *ns;
  gfc_intrinsic_op op_times, op_plus;
  enum matrix_case m_case;
  int i;
  gfc_code *if_limit = NULL;
  gfc_code **next_code_point;
  bool conjg_a, conjg_b, transpose_a, transpose_b;

  if (co->op != EXEC_ASSIGN)
    return 0;

  if (in_where)
    return 0;

  /* The BLOCKS generated for the temporary variables and FORALL don't
     mix.  */
  if (forall_level > 0)
    return 0;

  /* For now don't do anything in OpenMP workshare, it confuses
     its translation, which expects only the allowed statements in there.
     We should figure out how to parallelize this eventually.  */
  if (in_omp_workshare)
    return 0;

  expr1 = co->expr1;
  expr2 = co->expr2;
  if (expr2->expr_type != EXPR_FUNCTION
      || expr2->value.function.isym == NULL
      || expr2->value.function.isym->id != GFC_ISYM_MATMUL)
    return 0;

  current_code = c;
  inserted_block = NULL;
  changed_statement = NULL;

  a = expr2->value.function.actual;
  matrix_a = check_conjg_transpose_variable (a->expr, &conjg_a, &transpose_a);
  if (matrix_a == NULL)
    return 0;

  b = a->next;
  matrix_b = check_conjg_transpose_variable (b->expr, &conjg_b, &transpose_b);
  if (matrix_b == NULL)
    return 0;

  if (has_dimen_vector_ref (expr1) || has_dimen_vector_ref (matrix_a)
      || has_dimen_vector_ref (matrix_b))
    return 0;

  /* We do not handle data dependencies yet.  */
  if (gfc_check_dependency (expr1, matrix_a, true)
      || gfc_check_dependency (expr1, matrix_b, true))
    return 0;

  m_case = none;
  if (matrix_a->rank == 2)
    {
      if (transpose_a)
	{
	  if (matrix_b->rank == 2 && !transpose_b)
	    m_case = A2TB2;
	}
      else
	{
	  if (matrix_b->rank == 1)
	    m_case = A2B1;
	  else /* matrix_b->rank == 2 */
	    {
	      if (transpose_b)
		m_case = A2B2T;
	      else
		m_case = A2B2;
	    }
	}
    }
  else /* matrix_a->rank == 1 */
    {
      if (matrix_b->rank == 2)
	{
	  if (!transpose_b)
	    m_case = A1B2;
	}
    }
    
  if (m_case == none)
    return 0;

  ns = insert_block ();

  /* Assign the type of the zero expression for initializing the resulting
     array, and the expression (+ and * for real, integer and complex;
     .and. and .or for logical.  */

  switch(expr1->ts.type)
    {
    case BT_INTEGER:
      zero_e = gfc_get_int_expr (expr1->ts.kind, &expr1->where, 0);
      op_times = INTRINSIC_TIMES;
      op_plus = INTRINSIC_PLUS;
      break;

    case BT_LOGICAL:
      op_times = INTRINSIC_AND;
      op_plus = INTRINSIC_OR;
      zero_e = gfc_get_logical_expr (expr1->ts.kind, &expr1->where,
				     0);
      break;
    case BT_REAL:
      zero_e = gfc_get_constant_expr (BT_REAL, expr1->ts.kind,
				      &expr1->where);
      mpfr_set_si (zero_e->value.real, 0, GFC_RND_MODE);
      op_times = INTRINSIC_TIMES;
      op_plus = INTRINSIC_PLUS;
      break;

    case BT_COMPLEX:
      zero_e = gfc_get_constant_expr (BT_COMPLEX, expr1->ts.kind,
				      &expr1->where);
      mpc_set_si_si (zero_e->value.complex, 0, 0, GFC_RND_MODE);
      op_times = INTRINSIC_TIMES;
      op_plus = INTRINSIC_PLUS;

      break;

    default:
      gcc_unreachable();
    }

  current_code = &ns->code;

  /* Freeze the references, keeping track of how many temporary variables were
     created.  */
  n_vars = 0;
  freeze_references (matrix_a);
  freeze_references (matrix_b);
  freeze_references (expr1);

  if (n_vars == 0)
    next_code_point = current_code;
  else
    {
      next_code_point = &ns->code;
      for (i=0; i<n_vars; i++)
	next_code_point = &(*next_code_point)->next;
    }

  /* Take care of the inline flag.  If the limit check evaluates to a
     constant, dead code elimination will eliminate the unneeded branch.  */

  if (m_case == A2B2 && flag_inline_matmul_limit > 0)
    {
      if_limit = inline_limit_check (matrix_a, matrix_b, m_case);

      /* Insert the original statement into the else branch.  */
      if_limit->block->block->next = co;
      co->next = NULL;

      /* ... and the new ones go into the original one.  */
      *next_code_point = if_limit;
      next_code_point = &if_limit->block->next;
    }

  assign_zero = XCNEW (gfc_code);
  assign_zero->op = EXEC_ASSIGN;
  assign_zero->loc = co->loc;
  assign_zero->expr1 = gfc_copy_expr (expr1);
  assign_zero->expr2 = zero_e;

  /* Handle the reallocation, if needed.  */
  if (flag_realloc_lhs && gfc_is_reallocatable_lhs (expr1))
    {
      gfc_code *lhs_alloc;

      /* Only need to check a single dimension for the A2B2 case for
	 bounds checking, the rest will be allocated.  Also check this
	 for A2B1.   */

      if ((gfc_option.rtcheck & GFC_RTCHECK_BOUNDS) && (m_case == A2B2 || m_case == A2B1))
	{
	  gfc_code *test;
	  gfc_expr *a2, *b1;

	  a2 = get_array_inq_function (GFC_ISYM_SIZE, matrix_a, 2);
	  b1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 1);
	  test = runtime_error_ne (b1, a2, "Dimension of array B incorrect "
				   "in MATMUL intrinsic: Is %ld, should be %ld");
	  *next_code_point = test;
	  next_code_point = &test->next;
	}


      lhs_alloc = matmul_lhs_realloc (expr1, matrix_a, matrix_b, m_case);

      *next_code_point = lhs_alloc;
      next_code_point = &lhs_alloc->next;

    }
  else if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
    {
      gfc_code *test;
      gfc_expr *a2, *b1, *c1, *c2, *a1, *b2;

      if (m_case == A2B2 || m_case == A2B1)
	{
	  a2 = get_array_inq_function (GFC_ISYM_SIZE, matrix_a, 2);
	  b1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 1);
	  test = runtime_error_ne (b1, a2, "Dimension of array B incorrect "
				   "in MATMUL intrinsic: Is %ld, should be %ld");
	  *next_code_point = test;
	  next_code_point = &test->next;

	  c1 = get_array_inq_function (GFC_ISYM_SIZE, expr1, 1);
	  a1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_a, 1);

	  if (m_case == A2B2)
	    test = runtime_error_ne (c1, a1, "Incorrect extent in return array in "
				     "MATMUL intrinsic for dimension 1: "
				     "is %ld, should be %ld");
	  else if (m_case == A2B1)
	    test = runtime_error_ne (c1, a1, "Incorrect extent in return array in "
				     "MATMUL intrinsic: "
				     "is %ld, should be %ld");


	  *next_code_point = test;
	  next_code_point = &test->next;
	}
      else if (m_case == A1B2)
	{
	  a1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_a, 1);
	  b1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 1);
	  test = runtime_error_ne (b1, a1, "Dimension of array B incorrect "
				   "in MATMUL intrinsic: Is %ld, should be %ld");
	  *next_code_point = test;
	  next_code_point = &test->next;

	  c1 = get_array_inq_function (GFC_ISYM_SIZE, expr1, 1);
	  b2 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 2);

	  test = runtime_error_ne (c1, b2, "Incorrect extent in return array in "
				   "MATMUL intrinsic: "
				   "is %ld, should be %ld");

	  *next_code_point = test;
	  next_code_point = &test->next;
	}

      if (m_case == A2B2)
	{
	  c2 = get_array_inq_function (GFC_ISYM_SIZE, expr1, 2);
	  b2 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 2);
	  test = runtime_error_ne (c2, b2, "Incorrect extent in return array in "
				   "MATMUL intrinsic for dimension 2: is %ld, should be %ld");

	  *next_code_point = test;
	  next_code_point = &test->next;
	}

      if (m_case == A2B2T)
	{
	  c1 = get_array_inq_function (GFC_ISYM_SIZE, expr1, 1);
	  a1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_a, 1);
	  test = runtime_error_ne (c1, a1, "Incorrect extent in return array in "
				   "MATMUL intrinsic for dimension 1: "
				   "is %ld, should be %ld");

	  *next_code_point = test;
	  next_code_point = &test->next;

	  c2 = get_array_inq_function (GFC_ISYM_SIZE, expr1, 2);
	  b1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 1);
	  test = runtime_error_ne (c2, b1, "Incorrect extent in return array in "
				   "MATMUL intrinsic for dimension 2: "
				   "is %ld, should be %ld");
	  *next_code_point = test;
	  next_code_point = &test->next;

	  a2 = get_array_inq_function (GFC_ISYM_SIZE, matrix_a, 2);
	  b2 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 2);

	  test = runtime_error_ne (b2, a2, "Incorrect extent in argument B in "
				   "MATMUL intrnisic for dimension 2: "
				   "is %ld, should be %ld");
	  *next_code_point = test;
	  next_code_point = &test->next;

	}

      if (m_case == A2TB2)
	{
	  c1 = get_array_inq_function (GFC_ISYM_SIZE, expr1, 1);
	  a2 = get_array_inq_function (GFC_ISYM_SIZE, matrix_a, 2);

	  test = runtime_error_ne (c1, a2, "Incorrect extent in return array in "
				   "MATMUL intrinsic for dimension 1: "
				   "is %ld, should be %ld");

	  *next_code_point = test;
	  next_code_point = &test->next;

	  c2 = get_array_inq_function (GFC_ISYM_SIZE, expr1, 2);
	  b2 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 2);
	  test = runtime_error_ne (c2, b2, "Incorrect extent in return array in "
				   "MATMUL intrinsic for dimension 2: "
				   "is %ld, should be %ld");
	  *next_code_point = test;
	  next_code_point = &test->next;

	  a1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_a, 1);
	  b1 = get_array_inq_function (GFC_ISYM_SIZE, matrix_b, 1);

	  test = runtime_error_ne (b1, a1, "Incorrect extent in argument B in "
				   "MATMUL intrnisic for dimension 2: "
				   "is %ld, should be %ld");
	  *next_code_point = test;
	  next_code_point = &test->next;

	}
    }

  *next_code_point = assign_zero;

  zero = gfc_get_int_expr (gfc_index_integer_kind, &co->loc, 0);

  assign_matmul = XCNEW (gfc_code);
  assign_matmul->op = EXEC_ASSIGN;
  assign_matmul->loc = co->loc;

  /* Get the bounds for the loops, create them and create the scalarized
     expressions.  */

  switch (m_case)
    {
    case A2B2:
      inline_limit_check (matrix_a, matrix_b, m_case);

      u1 = get_size_m1 (matrix_b, 2);
      u2 = get_size_m1 (matrix_a, 2);
      u3 = get_size_m1 (matrix_a, 1);

      do_1 = create_do_loop (gfc_copy_expr (zero), u1, NULL, &co->loc, ns);
      do_2 = create_do_loop (gfc_copy_expr (zero), u2, NULL, &co->loc, ns);
      do_3 = create_do_loop (gfc_copy_expr (zero), u3, NULL, &co->loc, ns);

      do_1->block->next = do_2;
      do_2->block->next = do_3;
      do_3->block->next = assign_matmul;

      var_1 = do_1->ext.iterator->var;
      var_2 = do_2->ext.iterator->var;
      var_3 = do_3->ext.iterator->var;

      list[0] = var_3;
      list[1] = var_1;
      cscalar = scalarized_expr (co->expr1, list, 2);

      list[0] = var_3;
      list[1] = var_2;
      ascalar = scalarized_expr (matrix_a, list, 2);

      list[0] = var_2;
      list[1] = var_1;
      bscalar = scalarized_expr (matrix_b, list, 2);

      break;

    case A2B2T:
      inline_limit_check (matrix_a, matrix_b, m_case);

      u1 = get_size_m1 (matrix_b, 1);
      u2 = get_size_m1 (matrix_a, 2);
      u3 = get_size_m1 (matrix_a, 1);

      do_1 = create_do_loop (gfc_copy_expr (zero), u1, NULL, &co->loc, ns);
      do_2 = create_do_loop (gfc_copy_expr (zero), u2, NULL, &co->loc, ns);
      do_3 = create_do_loop (gfc_copy_expr (zero), u3, NULL, &co->loc, ns);

      do_1->block->next = do_2;
      do_2->block->next = do_3;
      do_3->block->next = assign_matmul;

      var_1 = do_1->ext.iterator->var;
      var_2 = do_2->ext.iterator->var;
      var_3 = do_3->ext.iterator->var;

      list[0] = var_3;
      list[1] = var_1;
      cscalar = scalarized_expr (co->expr1, list, 2);

      list[0] = var_3;
      list[1] = var_2;
      ascalar = scalarized_expr (matrix_a, list, 2);

      list[0] = var_1;
      list[1] = var_2;
      bscalar = scalarized_expr (matrix_b, list, 2);

      break;

    case A2TB2:
      inline_limit_check (matrix_a, matrix_b, m_case);

      u1 = get_size_m1 (matrix_a, 2);
      u2 = get_size_m1 (matrix_b, 2);
      u3 = get_size_m1 (matrix_a, 1);

      do_1 = create_do_loop (gfc_copy_expr (zero), u1, NULL, &co->loc, ns);
      do_2 = create_do_loop (gfc_copy_expr (zero), u2, NULL, &co->loc, ns);
      do_3 = create_do_loop (gfc_copy_expr (zero), u3, NULL, &co->loc, ns);

      do_1->block->next = do_2;
      do_2->block->next = do_3;
      do_3->block->next = assign_matmul;

      var_1 = do_1->ext.iterator->var;
      var_2 = do_2->ext.iterator->var;
      var_3 = do_3->ext.iterator->var;

      list[0] = var_1;
      list[1] = var_2;
      cscalar = scalarized_expr (co->expr1, list, 2);

      list[0] = var_3;
      list[1] = var_1;
      ascalar = scalarized_expr (matrix_a, list, 2);

      list[0] = var_3;
      list[1] = var_2;
      bscalar = scalarized_expr (matrix_b, list, 2);

      break;

    case A2B1:
      u1 = get_size_m1 (matrix_b, 1);
      u2 = get_size_m1 (matrix_a, 1);

      do_1 = create_do_loop (gfc_copy_expr (zero), u1, NULL, &co->loc, ns);
      do_2 = create_do_loop (gfc_copy_expr (zero), u2, NULL, &co->loc, ns);

      do_1->block->next = do_2;
      do_2->block->next = assign_matmul;

      var_1 = do_1->ext.iterator->var;
      var_2 = do_2->ext.iterator->var;

      list[0] = var_2;
      cscalar = scalarized_expr (co->expr1, list, 1);

      list[0] = var_2;
      list[1] = var_1;
      ascalar = scalarized_expr (matrix_a, list, 2);

      list[0] = var_1;
      bscalar = scalarized_expr (matrix_b, list, 1);

      break;

    case A1B2:
      u1 = get_size_m1 (matrix_b, 2);
      u2 = get_size_m1 (matrix_a, 1);

      do_1 = create_do_loop (gfc_copy_expr (zero), u1, NULL, &co->loc, ns);
      do_2 = create_do_loop (gfc_copy_expr (zero), u2, NULL, &co->loc, ns);

      do_1->block->next = do_2;
      do_2->block->next = assign_matmul;

      var_1 = do_1->ext.iterator->var;
      var_2 = do_2->ext.iterator->var;

      list[0] = var_1;
      cscalar = scalarized_expr (co->expr1, list, 1);

      list[0] = var_2;
      ascalar = scalarized_expr (matrix_a, list, 1);

      list[0] = var_2;
      list[1] = var_1;
      bscalar = scalarized_expr (matrix_b, list, 2);

      break;

    default:
      gcc_unreachable();
    }

  /* Build the conjg call around the variables.  Set the typespec manually
     because gfc_build_intrinsic_call sometimes gets this wrong.  */
  if (conjg_a)
    {
      gfc_typespec ts;
      ts = matrix_a->ts;
      ascalar = gfc_build_intrinsic_call (ns, GFC_ISYM_CONJG, "conjg",
					  matrix_a->where, 1, ascalar);
      ascalar->ts = ts;
    }

  if (conjg_b)
    {
      gfc_typespec ts;
      ts = matrix_b->ts;
      bscalar = gfc_build_intrinsic_call (ns, GFC_ISYM_CONJG, "conjg",
					  matrix_b->where, 1, bscalar);
      bscalar->ts = ts;
    }
  /* First loop comes after the zero assignment.  */
  assign_zero->next = do_1;

  /* Build the assignment expression in the loop.  */
  assign_matmul->expr1 = gfc_copy_expr (cscalar);

  mult = get_operand (op_times, ascalar, bscalar);
  assign_matmul->expr2 = get_operand (op_plus, cscalar, mult);

  /* If we don't want to keep the original statement around in
     the else branch, we can free it.  */

  if (if_limit == NULL)
    gfc_free_statements(co);
  else
    co->next = NULL;

  gfc_free_expr (zero);
  *walk_subtrees = 0;
  return 0;
}


/* Code for index interchange for loops which are grouped together in DO
   CONCURRENT or FORALL statements.  This is currently only applied if the
   iterations are grouped together in a single statement.

   For this transformation, it is assumed that memory access in strides is
   expensive, and that loops which access later indices (which access memory
   in bigger strides) should be moved to the first loops.

   For this, a loop over all the statements is executed, counting the times
   that the loop iteration values are accessed in each index.  The loop
   indices are then sorted to minimize access to later indices from inner
   loops.  */

/* Type for holding index information.  */

typedef struct {
  gfc_symbol *sym;
  gfc_forall_iterator *fa;
  int num;
  int n[GFC_MAX_DIMENSIONS];
} ind_type;

/* Callback function to determine if an expression is the 
   corresponding variable.  */

static int
has_var (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED, void *data)
{
  gfc_expr *expr = *e;
  gfc_symbol *sym;

  if (expr->expr_type != EXPR_VARIABLE)
    return 0;

  sym = (gfc_symbol *) data;
  return sym == expr->symtree->n.sym;
}

/* Callback function to calculate the cost of a certain index.  */

static int
index_cost (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
	    void *data)
{
  ind_type *ind;
  gfc_expr *expr;
  gfc_array_ref *ar;
  gfc_ref *ref;
  int i,j;

  expr = *e;
  if (expr->expr_type != EXPR_VARIABLE)
    return 0;

  ar = NULL;
  for (ref = expr->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_ARRAY)
	{
	  ar = &ref->u.ar;
	  break;
	}
    }
  if (ar == NULL || ar->type != AR_ELEMENT)
    return 0;

  ind = (ind_type *) data;
  for (i = 0; i < ar->dimen; i++)
    {
      for (j=0; ind[j].sym != NULL; j++)
	{
	  if (gfc_expr_walker (&ar->start[i], has_var, (void *) (ind[j].sym)))
	      ind[j].n[i]++;
	}
    }
  return 0;
}

/* Callback function for qsort, to sort the loop indices. */

static int
loop_comp (const void *e1, const void *e2)
{
  const ind_type *i1 = (const ind_type *) e1;
  const ind_type *i2 = (const ind_type *) e2;
  int i;

  for (i=GFC_MAX_DIMENSIONS-1; i >= 0; i--)
    {
      if (i1->n[i] != i2->n[i])
	return i1->n[i] - i2->n[i];
    }
  /* All other things being equal, let's not change the ordering.  */
  return i2->num - i1->num;
}

/* Main function to do the index interchange.  */

static int
index_interchange (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
		  void *data ATTRIBUTE_UNUSED)
{
  gfc_code *co;
  co = *c;
  int n_iter;
  gfc_forall_iterator *fa;
  ind_type *ind;
  int i, j;
  
  if (co->op != EXEC_FORALL && co->op != EXEC_DO_CONCURRENT)
    return 0;

  n_iter = 0;
  for (fa = co->ext.forall_iterator; fa; fa = fa->next)
    n_iter ++;

  /* Nothing to reorder. */
  if (n_iter < 2)
    return 0;

  ind = XALLOCAVEC (ind_type, n_iter + 1);

  i = 0;
  for (fa = co->ext.forall_iterator; fa; fa = fa->next)
    {
      ind[i].sym = fa->var->symtree->n.sym;
      ind[i].fa = fa;
      for (j=0; j<GFC_MAX_DIMENSIONS; j++)
	ind[i].n[j] = 0;
      ind[i].num = i;
      i++;
    }
  ind[n_iter].sym = NULL;
  ind[n_iter].fa = NULL;

  gfc_code_walker (c, gfc_dummy_code_callback, index_cost, (void *) ind);
  qsort ((void *) ind, n_iter, sizeof (ind_type), loop_comp);

  /* Do the actual index interchange.  */
  co->ext.forall_iterator = fa = ind[0].fa;
  for (i=1; i<n_iter; i++)
    {
      fa->next = ind[i].fa;
      fa = fa->next;
    }
  fa->next = NULL;

  if (flag_warn_frontend_loop_interchange)
    {
      for (i=1; i<n_iter; i++)
	{
	  if (ind[i-1].num > ind[i].num)
	    {
	      gfc_warning (OPT_Wfrontend_loop_interchange,
			   "Interchanging loops at %L", &co->loc);
	      break;
	    }
	}
    }

  return 0;
}

#define WALK_SUBEXPR(NODE) \
  do							\
    {							\
      result = gfc_expr_walker (&(NODE), exprfn, data);	\
      if (result)					\
	return result;					\
    }							\
  while (0)
#define WALK_SUBEXPR_TAIL(NODE) e = &(NODE); continue

/* Walk expression *E, calling EXPRFN on each expression in it.  */

int
gfc_expr_walker (gfc_expr **e, walk_expr_fn_t exprfn, void *data)
{
  while (*e)
    {
      int walk_subtrees = 1;
      gfc_actual_arglist *a;
      gfc_ref *r;
      gfc_constructor *c;

      int result = exprfn (e, &walk_subtrees, data);
      if (result)
	return result;
      if (walk_subtrees)
	switch ((*e)->expr_type)
	  {
	  case EXPR_OP:
	    WALK_SUBEXPR ((*e)->value.op.op1);
	    WALK_SUBEXPR_TAIL ((*e)->value.op.op2);
	    break;
	  case EXPR_FUNCTION:
	    for (a = (*e)->value.function.actual; a; a = a->next)
	      WALK_SUBEXPR (a->expr);
	    break;
	  case EXPR_COMPCALL:
	  case EXPR_PPC:
	    WALK_SUBEXPR ((*e)->value.compcall.base_object);
	    for (a = (*e)->value.compcall.actual; a; a = a->next)
	      WALK_SUBEXPR (a->expr);
	    break;

	  case EXPR_STRUCTURE:
	  case EXPR_ARRAY:
	    for (c = gfc_constructor_first ((*e)->value.constructor); c;
		 c = gfc_constructor_next (c))
	      {
		if (c->iterator == NULL)
		  WALK_SUBEXPR (c->expr);
		else
		  {
		    iterator_level ++;
		    WALK_SUBEXPR (c->expr);
		    iterator_level --;
		    WALK_SUBEXPR (c->iterator->var);
		    WALK_SUBEXPR (c->iterator->start);
		    WALK_SUBEXPR (c->iterator->end);
		    WALK_SUBEXPR (c->iterator->step);
		  }
	      }

	    if ((*e)->expr_type != EXPR_ARRAY)
	      break;

	    /* Fall through to the variable case in order to walk the
	       reference.  */
	    gcc_fallthrough ();

	  case EXPR_SUBSTRING:
	  case EXPR_VARIABLE:
	    for (r = (*e)->ref; r; r = r->next)
	      {
		gfc_array_ref *ar;
		int i;

		switch (r->type)
		  {
		  case REF_ARRAY:
		    ar = &r->u.ar;
		    if (ar->type == AR_SECTION || ar->type == AR_ELEMENT)
		      {
			for (i=0; i< ar->dimen; i++)
			  {
			    WALK_SUBEXPR (ar->start[i]);
			    WALK_SUBEXPR (ar->end[i]);
			    WALK_SUBEXPR (ar->stride[i]);
			  }
		      }

		    break;

		  case REF_SUBSTRING:
		    WALK_SUBEXPR (r->u.ss.start);
		    WALK_SUBEXPR (r->u.ss.end);
		    break;

		  case REF_COMPONENT:
		    break;
		  }
	      }

	  default:
	    break;
	  }
      return 0;
    }
  return 0;
}

#define WALK_SUBCODE(NODE) \
  do								\
    {								\
      result = gfc_code_walker (&(NODE), codefn, exprfn, data);	\
      if (result)						\
	return result;						\
    }								\
  while (0)

/* Walk code *C, calling CODEFN on each gfc_code node in it and calling EXPRFN
   on each expression in it.  If any of the hooks returns non-zero, that
   value is immediately returned.  If the hook sets *WALK_SUBTREES to 0,
   no subcodes or subexpressions are traversed.  */

int
gfc_code_walker (gfc_code **c, walk_code_fn_t codefn, walk_expr_fn_t exprfn,
		 void *data)
{
  for (; *c; c = &(*c)->next)
    {
      int walk_subtrees = 1;
      int result = codefn (c, &walk_subtrees, data);
      if (result)
	return result;

      if (walk_subtrees)
	{
	  gfc_code *b;
	  gfc_actual_arglist *a;
	  gfc_code *co;
	  gfc_association_list *alist;
	  bool saved_in_omp_workshare;
	  bool saved_in_where;

	  /* There might be statement insertions before the current code,
	     which must not affect the expression walker.  */

	  co = *c;
	  saved_in_omp_workshare = in_omp_workshare;
	  saved_in_where = in_where;

	  switch (co->op)
	    {

	    case EXEC_BLOCK:
	      WALK_SUBCODE (co->ext.block.ns->code);
	      if (co->ext.block.assoc)
		{
		  bool saved_in_assoc_list = in_assoc_list;

		  in_assoc_list = true;
		  for (alist = co->ext.block.assoc; alist; alist = alist->next)
		    WALK_SUBEXPR (alist->target);

		  in_assoc_list = saved_in_assoc_list;
		}

	      break;

	    case EXEC_DO:
	      doloop_level ++;
	      WALK_SUBEXPR (co->ext.iterator->var);
	      WALK_SUBEXPR (co->ext.iterator->start);
	      WALK_SUBEXPR (co->ext.iterator->end);
	      WALK_SUBEXPR (co->ext.iterator->step);
	      break;

	    case EXEC_IF:
	      if_level ++;
	      break;

	    case EXEC_WHERE:
	      in_where = true;
	      break;

	    case EXEC_CALL:
	    case EXEC_ASSIGN_CALL:
	      for (a = co->ext.actual; a; a = a->next)
		WALK_SUBEXPR (a->expr);
	      break;

	    case EXEC_CALL_PPC:
	      WALK_SUBEXPR (co->expr1);
	      for (a = co->ext.actual; a; a = a->next)
		WALK_SUBEXPR (a->expr);
	      break;

	    case EXEC_SELECT:
	      WALK_SUBEXPR (co->expr1);
	      select_level ++;
	      for (b = co->block; b; b = b->block)
		{
		  gfc_case *cp;
		  for (cp = b->ext.block.case_list; cp; cp = cp->next)
		    {
		      WALK_SUBEXPR (cp->low);
		      WALK_SUBEXPR (cp->high);
		    }
		  WALK_SUBCODE (b->next);
		}
	      continue;

	    case EXEC_ALLOCATE:
	    case EXEC_DEALLOCATE:
	      {
		gfc_alloc *a;
		for (a = co->ext.alloc.list; a; a = a->next)
		  WALK_SUBEXPR (a->expr);
		break;
	      }

	    case EXEC_FORALL:
	    case EXEC_DO_CONCURRENT:
	      {
		gfc_forall_iterator *fa;
		for (fa = co->ext.forall_iterator; fa; fa = fa->next)
		  {
		    WALK_SUBEXPR (fa->var);
		    WALK_SUBEXPR (fa->start);
		    WALK_SUBEXPR (fa->end);
		    WALK_SUBEXPR (fa->stride);
		  }
		if (co->op == EXEC_FORALL)
		  forall_level ++;
		break;
	      }

	    case EXEC_OPEN:
	      WALK_SUBEXPR (co->ext.open->unit);
	      WALK_SUBEXPR (co->ext.open->file);
	      WALK_SUBEXPR (co->ext.open->status);
	      WALK_SUBEXPR (co->ext.open->access);
	      WALK_SUBEXPR (co->ext.open->form);
	      WALK_SUBEXPR (co->ext.open->recl);
	      WALK_SUBEXPR (co->ext.open->blank);
	      WALK_SUBEXPR (co->ext.open->position);
	      WALK_SUBEXPR (co->ext.open->action);
	      WALK_SUBEXPR (co->ext.open->delim);
	      WALK_SUBEXPR (co->ext.open->pad);
	      WALK_SUBEXPR (co->ext.open->iostat);
	      WALK_SUBEXPR (co->ext.open->iomsg);
	      WALK_SUBEXPR (co->ext.open->convert);
	      WALK_SUBEXPR (co->ext.open->decimal);
	      WALK_SUBEXPR (co->ext.open->encoding);
	      WALK_SUBEXPR (co->ext.open->round);
	      WALK_SUBEXPR (co->ext.open->sign);
	      WALK_SUBEXPR (co->ext.open->asynchronous);
	      WALK_SUBEXPR (co->ext.open->id);
	      WALK_SUBEXPR (co->ext.open->newunit);
	      WALK_SUBEXPR (co->ext.open->share);
	      WALK_SUBEXPR (co->ext.open->cc);
	      break;

	    case EXEC_CLOSE:
	      WALK_SUBEXPR (co->ext.close->unit);
	      WALK_SUBEXPR (co->ext.close->status);
	      WALK_SUBEXPR (co->ext.close->iostat);
	      WALK_SUBEXPR (co->ext.close->iomsg);
	      break;

	    case EXEC_BACKSPACE:
	    case EXEC_ENDFILE:
	    case EXEC_REWIND:
	    case EXEC_FLUSH:
	      WALK_SUBEXPR (co->ext.filepos->unit);
	      WALK_SUBEXPR (co->ext.filepos->iostat);
	      WALK_SUBEXPR (co->ext.filepos->iomsg);
	      break;

	    case EXEC_INQUIRE:
	      WALK_SUBEXPR (co->ext.inquire->unit);
	      WALK_SUBEXPR (co->ext.inquire->file);
	      WALK_SUBEXPR (co->ext.inquire->iomsg);
	      WALK_SUBEXPR (co->ext.inquire->iostat);
	      WALK_SUBEXPR (co->ext.inquire->exist);
	      WALK_SUBEXPR (co->ext.inquire->opened);
	      WALK_SUBEXPR (co->ext.inquire->number);
	      WALK_SUBEXPR (co->ext.inquire->named);
	      WALK_SUBEXPR (co->ext.inquire->name);
	      WALK_SUBEXPR (co->ext.inquire->access);
	      WALK_SUBEXPR (co->ext.inquire->sequential);
	      WALK_SUBEXPR (co->ext.inquire->direct);
	      WALK_SUBEXPR (co->ext.inquire->form);
	      WALK_SUBEXPR (co->ext.inquire->formatted);
	      WALK_SUBEXPR (co->ext.inquire->unformatted);
	      WALK_SUBEXPR (co->ext.inquire->recl);
	      WALK_SUBEXPR (co->ext.inquire->nextrec);
	      WALK_SUBEXPR (co->ext.inquire->blank);
	      WALK_SUBEXPR (co->ext.inquire->position);
	      WALK_SUBEXPR (co->ext.inquire->action);
	      WALK_SUBEXPR (co->ext.inquire->read);
	      WALK_SUBEXPR (co->ext.inquire->write);
	      WALK_SUBEXPR (co->ext.inquire->readwrite);
	      WALK_SUBEXPR (co->ext.inquire->delim);
	      WALK_SUBEXPR (co->ext.inquire->encoding);
	      WALK_SUBEXPR (co->ext.inquire->pad);
	      WALK_SUBEXPR (co->ext.inquire->iolength);
	      WALK_SUBEXPR (co->ext.inquire->convert);
	      WALK_SUBEXPR (co->ext.inquire->strm_pos);
	      WALK_SUBEXPR (co->ext.inquire->asynchronous);
	      WALK_SUBEXPR (co->ext.inquire->decimal);
	      WALK_SUBEXPR (co->ext.inquire->pending);
	      WALK_SUBEXPR (co->ext.inquire->id);
	      WALK_SUBEXPR (co->ext.inquire->sign);
	      WALK_SUBEXPR (co->ext.inquire->size);
	      WALK_SUBEXPR (co->ext.inquire->round);
	      break;

	    case EXEC_WAIT:
	      WALK_SUBEXPR (co->ext.wait->unit);
	      WALK_SUBEXPR (co->ext.wait->iostat);
	      WALK_SUBEXPR (co->ext.wait->iomsg);
	      WALK_SUBEXPR (co->ext.wait->id);
	      break;

	    case EXEC_READ:
	    case EXEC_WRITE:
	      WALK_SUBEXPR (co->ext.dt->io_unit);
	      WALK_SUBEXPR (co->ext.dt->format_expr);
	      WALK_SUBEXPR (co->ext.dt->rec);
	      WALK_SUBEXPR (co->ext.dt->advance);
	      WALK_SUBEXPR (co->ext.dt->iostat);
	      WALK_SUBEXPR (co->ext.dt->size);
	      WALK_SUBEXPR (co->ext.dt->iomsg);
	      WALK_SUBEXPR (co->ext.dt->id);
	      WALK_SUBEXPR (co->ext.dt->pos);
	      WALK_SUBEXPR (co->ext.dt->asynchronous);
	      WALK_SUBEXPR (co->ext.dt->blank);
	      WALK_SUBEXPR (co->ext.dt->decimal);
	      WALK_SUBEXPR (co->ext.dt->delim);
	      WALK_SUBEXPR (co->ext.dt->pad);
	      WALK_SUBEXPR (co->ext.dt->round);
	      WALK_SUBEXPR (co->ext.dt->sign);
	      WALK_SUBEXPR (co->ext.dt->extra_comma);
	      break;

	    case EXEC_OMP_PARALLEL:
	    case EXEC_OMP_PARALLEL_DO:
	    case EXEC_OMP_PARALLEL_DO_SIMD:
	    case EXEC_OMP_PARALLEL_SECTIONS:

	      in_omp_workshare = false;

	      /* This goto serves as a shortcut to avoid code
		 duplication or a larger if or switch statement.  */
	      goto check_omp_clauses;

	    case EXEC_OMP_WORKSHARE:
	    case EXEC_OMP_PARALLEL_WORKSHARE:

	      in_omp_workshare = true;

	      /* Fall through  */

	    case EXEC_OMP_CRITICAL:
	    case EXEC_OMP_DISTRIBUTE:
	    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
	    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
	    case EXEC_OMP_DISTRIBUTE_SIMD:
	    case EXEC_OMP_DO:
	    case EXEC_OMP_DO_SIMD:
	    case EXEC_OMP_ORDERED:
	    case EXEC_OMP_SECTIONS:
	    case EXEC_OMP_SINGLE:
	    case EXEC_OMP_END_SINGLE:
	    case EXEC_OMP_SIMD:
	    case EXEC_OMP_TASKLOOP:
	    case EXEC_OMP_TASKLOOP_SIMD:
	    case EXEC_OMP_TARGET:
	    case EXEC_OMP_TARGET_DATA:
	    case EXEC_OMP_TARGET_ENTER_DATA:
	    case EXEC_OMP_TARGET_EXIT_DATA:
	    case EXEC_OMP_TARGET_PARALLEL:
	    case EXEC_OMP_TARGET_PARALLEL_DO:
	    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TARGET_SIMD:
	    case EXEC_OMP_TARGET_TEAMS:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
	    case EXEC_OMP_TARGET_UPDATE:
	    case EXEC_OMP_TASK:
	    case EXEC_OMP_TEAMS:
	    case EXEC_OMP_TEAMS_DISTRIBUTE:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:

	      /* Come to this label only from the
		 EXEC_OMP_PARALLEL_* cases above.  */

	    check_omp_clauses:

	      if (co->ext.omp_clauses)
		{
		  gfc_omp_namelist *n;
		  static int list_types[]
		    = { OMP_LIST_ALIGNED, OMP_LIST_LINEAR, OMP_LIST_DEPEND,
			OMP_LIST_MAP, OMP_LIST_TO, OMP_LIST_FROM };
		  size_t idx;
		  WALK_SUBEXPR (co->ext.omp_clauses->if_expr);
		  WALK_SUBEXPR (co->ext.omp_clauses->final_expr);
		  WALK_SUBEXPR (co->ext.omp_clauses->num_threads);
		  WALK_SUBEXPR (co->ext.omp_clauses->chunk_size);
		  WALK_SUBEXPR (co->ext.omp_clauses->safelen_expr);
		  WALK_SUBEXPR (co->ext.omp_clauses->simdlen_expr);
		  WALK_SUBEXPR (co->ext.omp_clauses->num_teams);
		  WALK_SUBEXPR (co->ext.omp_clauses->device);
		  WALK_SUBEXPR (co->ext.omp_clauses->thread_limit);
		  WALK_SUBEXPR (co->ext.omp_clauses->dist_chunk_size);
		  WALK_SUBEXPR (co->ext.omp_clauses->grainsize);
		  WALK_SUBEXPR (co->ext.omp_clauses->hint);
		  WALK_SUBEXPR (co->ext.omp_clauses->num_tasks);
		  WALK_SUBEXPR (co->ext.omp_clauses->priority);
		  for (idx = 0; idx < OMP_IF_LAST; idx++)
		    WALK_SUBEXPR (co->ext.omp_clauses->if_exprs[idx]);
		  for (idx = 0;
		       idx < sizeof (list_types) / sizeof (list_types[0]);
		       idx++)
		    for (n = co->ext.omp_clauses->lists[list_types[idx]];
			 n; n = n->next)
		      WALK_SUBEXPR (n->expr);
		}
	      break;
	    default:
	      break;
	    }

	  WALK_SUBEXPR (co->expr1);
	  WALK_SUBEXPR (co->expr2);
	  WALK_SUBEXPR (co->expr3);
	  WALK_SUBEXPR (co->expr4);
	  for (b = co->block; b; b = b->block)
	    {
	      WALK_SUBEXPR (b->expr1);
	      WALK_SUBEXPR (b->expr2);
	      WALK_SUBCODE (b->next);
	    }

	  if (co->op == EXEC_FORALL)
	    forall_level --;

	  if (co->op == EXEC_DO)
	    doloop_level --;

	  if (co->op == EXEC_IF)
	    if_level --;

	  if (co->op == EXEC_SELECT)
	    select_level --;
  
	  in_omp_workshare = saved_in_omp_workshare;
	  in_where = saved_in_where;
	}
    }
  return 0;
}
