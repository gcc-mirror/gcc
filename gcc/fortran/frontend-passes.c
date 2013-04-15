/* Pass manager for Fortran front end.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
#include "gfortran.h"
#include "arith.h"
#include "flags.h"
#include "dependency.h"
#include "constructor.h"
#include "opts.h"

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
static void optimize_reduction (gfc_namespace *);
static int callback_reduction (gfc_expr **, int *, void *);

/* How deep we are inside an argument list.  */

static int count_arglist;

/* Pointer to an array of gfc_expr ** we operate on, plus its size
   and counter.  */

static gfc_expr ***expr_array;
static int expr_size, expr_count;

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

/* Keep track of iterators for array constructors.  */

static int iterator_level;

/* Keep track of DO loop levels.  */

static gfc_code **doloop_list;
static int doloop_size, doloop_level;

/* Vector of gfc_expr * to keep track of DO loops.  */

struct my_struct *evec;

/* Entry point - run all passes for a namespace. */

void
gfc_run_passes (gfc_namespace *ns)
{

  /* Warn about dubious DO loops where the index might
     change.  */

  doloop_size = 20;
  doloop_level = 0;
  doloop_list = XNEWVEC(gfc_code *, doloop_size);
  doloop_warn (ns);
  XDELETEVEC (doloop_list);

  if (gfc_option.flag_frontend_optimize)
    {
      expr_size = 20;
      expr_array = XNEWVEC(gfc_expr **, expr_size);

      optimize_namespace (ns);
      optimize_reduction (ns);
      if (gfc_option.dump_fortran_optimized)
	gfc_dump_parse_tree (ns, stdout);

      XDELETEVEC (expr_array);
    }
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
copy_walk_reduction_arg (gfc_expr *e, gfc_expr *fn)
{
  gfc_expr *fcn;
  gfc_isym_id id;

  if (e->rank == 0 || e->expr_type == EXPR_FUNCTION)
    fcn = gfc_copy_expr (e);
  else
    {
      id = fn->value.function.isym->id;

      if (id == GFC_ISYM_SUM || id == GFC_ISYM_PRODUCT)
	fcn = gfc_build_intrinsic_call (current_ns,
					fn->value.function.isym->id,
					fn->value.function.isym->name,
					fn->where, 3, gfc_copy_expr (e),
					NULL, NULL);
      else if (id == GFC_ISYM_ANY || id == GFC_ISYM_ALL)
	fcn = gfc_build_intrinsic_call (current_ns,
					fn->value.function.isym->id,
					fn->value.function.isym->name,
					fn->where, 2, gfc_copy_expr (e),
					NULL);
      else
	gfc_internal_error ("Illegal id in copy_walk_reduction_arg");

      fcn->symtree->n.sym->attr.access = ACCESS_PRIVATE;
    }

  (void) gfc_expr_walker (&fcn, callback_reduction, NULL);

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

  if (c == NULL || (c->iterator != NULL && gfc_constructor_next (c) == NULL))
    return 0;

  res = copy_walk_reduction_arg (c->expr, fn);

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
      new_expr->value.op.op2 = copy_walk_reduction_arg (c->expr, fn);
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

  if ((*e)->rank > 0 && (*e)->shape == NULL && !gfc_option.flag_realloc_lhs)
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
      if (!gfc_option.flag_aggressive_function_elimination
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

  if (expr_count >= expr_size)
    {
      expr_size += expr_size;
      expr_array = XRESIZEVEC(gfc_expr **, expr_array, expr_size);
    }
  expr_array[expr_count] = e;
  expr_count ++;
  return 0;
}

/* Returns a new expression (a variable) to be used in place of the old one,
   with an assignment statement before the current statement to set
   the value of the variable. Creates a new BLOCK for the statement if
   that hasn't already been done and puts the statement, plus the
   newly created variables, in that block.  */

static gfc_expr*
create_var (gfc_expr * e)
{
  char name[GFC_MAX_SYMBOL_LEN +1];
  static int num = 1;
  gfc_symtree *symtree;
  gfc_symbol *symbol;
  gfc_expr *result;
  gfc_code *n;
  gfc_namespace *ns;
  int i;

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

  sprintf(name, "__var_%d",num++);
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
	     allocatable. */
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

  symbol->attr.flavor = FL_VARIABLE;
  symbol->attr.referenced = 1;
  symbol->attr.dimension = e->rank > 0;
  gfc_commit_symbol (symbol);

  result = gfc_get_expr ();
  result->expr_type = EXPR_VARIABLE;
  result->ts = e->ts;
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
      result->ref->u.ar.as = symbol->ts.type == BT_CLASS
			     ? CLASS_DATA (symbol)->as : symbol->as;
      if (gfc_option.warn_array_temp)
	gfc_warning ("Creating array temporary at %L", &(e->where));
    }

  /* Generate the new assignment.  */
  n = XCNEW (gfc_code);
  n->op = EXEC_ASSIGN;
  n->loc = (*current_code)->loc;
  n->next = *changed_statement;
  n->expr1 = gfc_copy_expr (result);
  n->expr2 = e;
  *changed_statement = n;

  return result;
}

/* Warn about function elimination.  */

static void
warn_function_elimination (gfc_expr *e)
{
  if (e->expr_type != EXPR_FUNCTION)
    return;
  if (e->value.function.esym)
    gfc_warning ("Removing call to function '%s' at %L",
		 e->value.function.esym->name, &(e->where));
  else if (e->value.function.isym)
    gfc_warning ("Removing call to function '%s' at %L",
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

  /* Don't do this optimization within OMP workshare. */

  if (in_omp_workshare)
    {
      *walk_subtrees = 0;
      return 0;
    }

  expr_count = 0;

  gfc_expr_walker (e, cfe_register_funcs, NULL);

  /* Walk through all the functions.  */

  for (i=1; i<expr_count; i++)
    {
      /* Skip if the function has been replaced by a variable already.  */
      if ((*(expr_array[i]))->expr_type == EXPR_VARIABLE)
	continue;

      newvar = NULL;
      for (j=0; j<i; j++)
	{
	  if (gfc_dep_compare_functions (*(expr_array[i]),
					*(expr_array[j]), true)	== 0)
	    {
	      if (newvar == NULL)
		newvar = create_var (*(expr_array[i]));

	      if (gfc_option.warn_function_elimination)
		warn_function_elimination (*(expr_array[j]));

	      free (*(expr_array[j]));
	      *(expr_array[j]) = gfc_copy_expr (newvar);
	    }
	}
      if (newvar)
	*(expr_array[i]) = newvar;
    }

  /* We did all the necessary walking in this function.  */
  *walk_subtrees = 0;
  return 0;
}

/* Callback function for common function elimination, called from
   gfc_code_walker.  This keeps track of the current code, in order
   to insert statements as needed.  */

static int
cfe_code (gfc_code **c, int *walk_subtrees ATTRIBUTE_UNUSED,
	  void *data ATTRIBUTE_UNUSED)
{
  current_code = c;
  inserted_block = NULL;
  changed_statement = NULL;
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
static int
dummy_code_callback (gfc_code **e ATTRIBUTE_UNUSED,
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
/* Optimize a namespace, including all contained namespaces.  */

static void
optimize_namespace (gfc_namespace *ns)
{

  current_ns = ns;
  forall_level = 0;
  iterator_level = 0;
  in_omp_workshare = false;

  gfc_code_walker (&ns->code, convert_do_while, dummy_expr_callback, NULL);
  gfc_code_walker (&ns->code, convert_elseif, dummy_expr_callback, NULL);
  gfc_code_walker (&ns->code, cfe_code, cfe_expr_0, NULL);
  gfc_code_walker (&ns->code, optimize_code, optimize_expr, NULL);

  /* BLOCKs are handled in the expression walker below.  */
  for (ns = ns->contained; ns; ns = ns->sibling)
    {
      if (ns->code == NULL || ns->code->op != EXEC_BLOCK)
	optimize_namespace (ns);
    }
}

static void
optimize_reduction (gfc_namespace *ns)
{
  current_ns = ns;
  gfc_code_walker (&ns->code, dummy_code_callback, callback_reduction, NULL);

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

  /* Array constructors have rank one.  */
  if (e->rank != 1)
    return false;

  op1 = e->value.op.op1;
  op2 = e->value.op.op2;

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

  if (op2->expr_type == EXPR_CONSTANT)
    scalar = gfc_copy_expr (op2);
  else
    scalar = create_var (gfc_copy_expr (op2));

  oldbase = op1->value.constructor;
  newbase = NULL;
  e->expr_type = EXPR_ARRAY;

  for (c = gfc_constructor_first (oldbase); c;
       c = gfc_constructor_next (c))
    {
      new_expr = gfc_get_expr ();
      new_expr->ts = e->ts;
      new_expr->expr_type = EXPR_OP;
      new_expr->rank = c->expr->rank;
      new_expr->where = c->where;
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

  e->value.constructor = newbase;
  return true;
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

      /* Fall through */
      /* Look at array constructors.  */
    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
      return combine_array_constructor (e) || changed;

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
	      && op1->value.op.op == INTRINSIC_CONCAT
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
  fn->value.function.name = gfc_get_string (name);
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

  co = *c;

  switch (co->op)
    {
    case EXEC_DO:

      /* Grow the temporary storage if necessary.  */
      if (doloop_level >= doloop_size)
	{
	  doloop_size = 2 * doloop_size;
	  doloop_list = XRESIZEVEC (gfc_code *, doloop_list, doloop_size);
	}

      /* Mark the DO loop variable if there is one.  */
      if (co->ext.iterator && co->ext.iterator->var)
	doloop_list[doloop_level] = co;
      else
	doloop_list[doloop_level] = NULL;
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
	  for (i=0; i<doloop_level; i++)
	    {
	      gfc_symbol *do_sym;
	      
	      if (doloop_list[i] == NULL)
		break;

	      do_sym = doloop_list[i]->ext.iterator->var->symtree->n.sym;
	      
	      if (a->expr && a->expr->symtree
		  && a->expr->symtree->n.sym == do_sym)
		{
		  if (f->sym->attr.intent == INTENT_OUT)
		    gfc_error_now("Variable '%s' at %L set to undefined value "
				  "inside loop  beginning at %L as INTENT(OUT) "
				  "argument to subroutine '%s'", do_sym->name,
				  &a->expr->where, &doloop_list[i]->loc,
				  co->symtree->n.sym->name);
		  else if (f->sym->attr.intent == INTENT_INOUT)
		    gfc_error_now("Variable '%s' at %L not definable inside loop "
				  "beginning at %L as INTENT(INOUT) argument to "
				  "subroutine '%s'", do_sym->name,
				  &a->expr->where, &doloop_list[i]->loc,
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
  return 0;
}

/* Callback function for functions checking that we do not pass a DO variable
   to an INTENT(OUT) or INTENT(INOUT) dummy variable.  */

static int
do_function (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
	     void *data ATTRIBUTE_UNUSED)
{
  gfc_formal_arglist *f;
  gfc_actual_arglist *a;
  gfc_expr *expr;
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
      for (i=0; i<doloop_level; i++)
	{
	  gfc_symbol *do_sym;
	 
    
	  if (doloop_list[i] == NULL)
	    break;

	  do_sym = doloop_list[i]->ext.iterator->var->symtree->n.sym;
	  
	  if (a->expr && a->expr->symtree
	      && a->expr->symtree->n.sym == do_sym)
	    {
	      if (f->sym->attr.intent == INTENT_OUT)
		gfc_error_now("Variable '%s' at %L set to undefined value "
			      "inside loop beginning at %L as INTENT(OUT) "
			      "argument to function '%s'", do_sym->name,
			      &a->expr->where, &doloop_list[i]->loc,
			      expr->symtree->n.sym->name);
	      else if (f->sym->attr.intent == INTENT_INOUT)
		gfc_error_now("Variable '%s' at %L not definable inside loop "
			      "beginning at %L as INTENT(INOUT) argument to "
			      "function '%s'", do_sym->name,
			      &a->expr->where, &doloop_list[i]->loc,
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

	  /* There might be statement insertions before the current code,
	     which must not affect the expression walker.  */

	  co = *c;
	  saved_in_omp_workshare = in_omp_workshare;

	  switch (co->op)
	    {

	    case EXEC_BLOCK:
	      WALK_SUBCODE (co->ext.block.ns->code);
	      for (alist = co->ext.block.assoc; alist; alist = alist->next)
		WALK_SUBEXPR (alist->target);
	      break;

	    case EXEC_DO:
	      doloop_level ++;
	      WALK_SUBEXPR (co->ext.iterator->var);
	      WALK_SUBEXPR (co->ext.iterator->start);
	      WALK_SUBEXPR (co->ext.iterator->end);
	      WALK_SUBEXPR (co->ext.iterator->step);
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
	    case EXEC_OMP_PARALLEL_SECTIONS:

	      in_omp_workshare = false;

	      /* This goto serves as a shortcut to avoid code
		 duplication or a larger if or switch statement.  */
	      goto check_omp_clauses;
	      
	    case EXEC_OMP_WORKSHARE:
	    case EXEC_OMP_PARALLEL_WORKSHARE:

	      in_omp_workshare = true;

	      /* Fall through  */
	      
	    case EXEC_OMP_DO:
	    case EXEC_OMP_SECTIONS:
	    case EXEC_OMP_SINGLE:
	    case EXEC_OMP_END_SINGLE:
	    case EXEC_OMP_TASK:

	      /* Come to this label only from the
		 EXEC_OMP_PARALLEL_* cases above.  */

	    check_omp_clauses:

	      if (co->ext.omp_clauses)
		{
		  WALK_SUBEXPR (co->ext.omp_clauses->if_expr);
		  WALK_SUBEXPR (co->ext.omp_clauses->final_expr);
		  WALK_SUBEXPR (co->ext.omp_clauses->num_threads);
		  WALK_SUBEXPR (co->ext.omp_clauses->chunk_size);
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

	  in_omp_workshare = saved_in_omp_workshare;
	}
    }
  return 0;
}
