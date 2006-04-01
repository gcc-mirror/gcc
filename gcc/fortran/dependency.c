/* Dependency analysis
   Copyright (C) 2000, 2001, 2002, 2005 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* dependency.c -- Expression dependency analysis code.  */
/* There's probably quite a bit of duplication in this file.  We currently
   have different dependency checking functions for different types
   if dependencies.  Ideally these would probably be merged.  */
   

#include "config.h"
#include "gfortran.h"
#include "dependency.h"

/* static declarations */
/* Enums  */
enum range {LHS, RHS, MID};

/* Dependency types.  These must be in reverse order of priority.  */
typedef enum
{
  GFC_DEP_ERROR,
  GFC_DEP_EQUAL,	/* Identical Ranges.  */
  GFC_DEP_FORWARD,	/* eg. a(1:3), a(2:4).  */
  GFC_DEP_OVERLAP,	/* May overlap in some other way.  */
  GFC_DEP_NODEP		/* Distinct ranges.  */
}
gfc_dependency;

/* Macros */
#define IS_ARRAY_EXPLICIT(as) ((as->type == AS_EXPLICIT ? 1 : 0))


/* Returns 1 if the expr is an integer constant value 1, 0 if it is not or
   def if the value could not be determined.  */

int
gfc_expr_is_one (gfc_expr * expr, int def)
{
  gcc_assert (expr != NULL);

  if (expr->expr_type != EXPR_CONSTANT)
    return def;

  if (expr->ts.type != BT_INTEGER)
    return def;

  return mpz_cmp_si (expr->value.integer, 1) == 0;
}


/* Compare two values.  Returns 0 if e1 == e2, -1 if e1 < e2, +1 if e1 > e2,
   and -2 if the relationship could not be determined.  */

int
gfc_dep_compare_expr (gfc_expr * e1, gfc_expr * e2)
{
  gfc_actual_arglist *args1;
  gfc_actual_arglist *args2;
  int i;

  if (e1->expr_type == EXPR_OP
      && (e1->value.op.operator == INTRINSIC_UPLUS
          || e1->value.op.operator == INTRINSIC_PARENTHESES))
    return gfc_dep_compare_expr (e1->value.op.op1, e2);
  if (e2->expr_type == EXPR_OP
      && (e2->value.op.operator == INTRINSIC_UPLUS
          || e2->value.op.operator == INTRINSIC_PARENTHESES))
    return gfc_dep_compare_expr (e1, e2->value.op.op1);

  if (e1->expr_type == EXPR_OP
      && e1->value.op.operator == INTRINSIC_PLUS)
    {
      /* Compare X+C vs. X.  */
      if (e1->value.op.op2->expr_type == EXPR_CONSTANT
	  && e1->value.op.op2->ts.type == BT_INTEGER
	  && gfc_dep_compare_expr (e1->value.op.op1, e2) == 0)
	return mpz_sgn (e1->value.op.op2->value.integer);

      /* Compare P+Q vs. R+S.  */
      if (e2->expr_type == EXPR_OP
	  && e2->value.op.operator == INTRINSIC_PLUS)
	{
	  int l, r;

	  l = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1);
	  r = gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op2);
	  if (l == 0 && r == 0)
	    return 0;
	  if (l == 0 && r != -2)
	    return r;
	  if (l != -2 && r == 0)
	    return l;
	  if (l == 1 && r == 1)
	    return 1;
	  if (l == -1 && r == -1)
	    return -1;

	  l = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op2);
	  r = gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op1);
	  if (l == 0 && r == 0)
	    return 0;
	  if (l == 0 && r != -2)
	    return r;
	  if (l != -2 && r == 0)
	    return l;
	  if (l == 1 && r == 1)
	    return 1;
	  if (l == -1 && r == -1)
	    return -1;
	}
    }

  /* Compare X vs. X+C.  */
  if (e2->expr_type == EXPR_OP
      && e2->value.op.operator == INTRINSIC_PLUS)
    {
      if (e2->value.op.op2->expr_type == EXPR_CONSTANT
	  && e2->value.op.op2->ts.type == BT_INTEGER
	  && gfc_dep_compare_expr (e1, e2->value.op.op1) == 0)
	return -mpz_sgn (e2->value.op.op2->value.integer);
    }

  /* Compare X-C vs. X.  */
  if (e1->expr_type == EXPR_OP
      && e1->value.op.operator == INTRINSIC_MINUS)
    {
      if (e1->value.op.op2->expr_type == EXPR_CONSTANT
	  && e1->value.op.op2->ts.type == BT_INTEGER
	  && gfc_dep_compare_expr (e1->value.op.op1, e2) == 0)
	return -mpz_sgn (e1->value.op.op2->value.integer);

      /* Compare P-Q vs. R-S.  */
      if (e2->expr_type == EXPR_OP
	  && e2->value.op.operator == INTRINSIC_MINUS)
	{
	  int l, r;

	  l = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1);
	  r = gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op2);
	  if (l == 0 && r == 0)
	    return 0;
	  if (l != -2 && r == 0)
	    return l;
	  if (l == 0 && r != -2)
	    return -r;
	  if (l == 1 && r == -1)
	    return 1;
	  if (l == -1 && r == 1)
	    return -1;
	}
    }

  /* Compare X vs. X-C.  */
  if (e2->expr_type == EXPR_OP
      && e2->value.op.operator == INTRINSIC_MINUS)
    {
      if (e2->value.op.op2->expr_type == EXPR_CONSTANT
	  && e2->value.op.op2->ts.type == BT_INTEGER
	  && gfc_dep_compare_expr (e1, e2->value.op.op1) == 0)
	return mpz_sgn (e2->value.op.op2->value.integer);
    }

  if (e1->expr_type != e2->expr_type)
    return -2;

  switch (e1->expr_type)
    {
    case EXPR_CONSTANT:
      if (e1->ts.type != BT_INTEGER || e2->ts.type != BT_INTEGER)
	return -2;

      i = mpz_cmp (e1->value.integer, e2->value.integer);
      if (i == 0)
	return 0;
      else if (i < 0)
	return -1;
      return 1;

    case EXPR_VARIABLE:
      if (e1->ref || e2->ref)
	return -2;
      if (e1->symtree->n.sym == e2->symtree->n.sym)
	return 0;
      return -2;

    case EXPR_OP:
      /* Intrinsic operators are the same if their operands are the same.  */
      if (e1->value.op.operator != e2->value.op.operator)
	return -2;
      if (e1->value.op.op2 == 0)
	{
	  i = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1);
	  return i == 0 ? 0 : -2;
	}
      if (gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1) == 0
	  && gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op2) == 0)
	return 0;
      /* TODO Handle commutative binary operators here?  */
      return -2;

    case EXPR_FUNCTION:
      /* We can only compare calls to the same intrinsic function.  */
      if (e1->value.function.isym == 0
	  || e2->value.function.isym == 0
	  || e1->value.function.isym != e2->value.function.isym)
	return -2;

      args1 = e1->value.function.actual;
      args2 = e2->value.function.actual;

      /* We should list the "constant" intrinsic functions.  Those
	 without side-effects that provide equal results given equal
	 argument lists.  */
      switch (e1->value.function.isym->generic_id)
	{
	case GFC_ISYM_CONVERSION:
	  /* Handle integer extensions specially, as __convert_i4_i8
	     is not only "constant" but also "unary" and "increasing".  */
	  if (args1 && !args1->next
	      && args2 && !args2->next
	      && e1->ts.type == BT_INTEGER
	      && args1->expr->ts.type == BT_INTEGER
	      && e1->ts.kind > args1->expr->ts.kind
	      && e2->ts.type == e1->ts.type
	      && e2->ts.kind == e1->ts.kind
	      && args2->expr->ts.type == args1->expr->ts.type
	      && args2->expr->ts.kind == args2->expr->ts.kind)
	    return gfc_dep_compare_expr (args1->expr, args2->expr);
	  break;

	case GFC_ISYM_REAL:
	case GFC_ISYM_LOGICAL:
	case GFC_ISYM_DBLE:
	  break;

	default:
	  return -2;
	}

      /* Compare the argument lists for equality.  */
      while (args1 && args2)
	{
	  if (gfc_dep_compare_expr (args1->expr, args2->expr) != 0)
	    return -2;
	  args1 = args1->next;
	  args2 = args2->next;
	}
      return (args1 || args2) ? -2 : 0;
      
    default:
      return -2;
    }
}


/* Returns 1 if the two ranges are the same, 0 if they are not, and def
   if the results are indeterminate.  N is the dimension to compare.  */

int
gfc_is_same_range (gfc_array_ref * ar1, gfc_array_ref * ar2, int n, int def)
{
  gfc_expr *e1;
  gfc_expr *e2;
  int i;

  /* TODO: More sophisticated range comparison.  */
  gcc_assert (ar1 && ar2);

  gcc_assert (ar1->dimen_type[n] == ar2->dimen_type[n]);

  e1 = ar1->stride[n];
  e2 = ar2->stride[n];
  /* Check for mismatching strides.  A NULL stride means a stride of 1.  */
  if (e1 && !e2)
    {
      i = gfc_expr_is_one (e1, -1);
      if (i == -1)
	return def;
      else if (i == 0)
	return 0;
    }
  else if (e2 && !e1)
    {
      i = gfc_expr_is_one (e2, -1);
      if (i == -1)
	return def;
      else if (i == 0)
	return 0;
    }
  else if (e1 && e2)
    {
      i = gfc_dep_compare_expr (e1, e2);
      if (i == -2)
	return def;
      else if (i != 0)
	return 0;
    }
  /* The strides match.  */

  /* Check the range start.  */
  e1 = ar1->start[n];
  e2 = ar2->start[n];
  if (e1 || e2)
    {
      /* Use the bound of the array if no bound is specified.  */
      if (ar1->as && !e1)
	e1 = ar1->as->lower[n];

      if (ar2->as && !e2)
	e2 = ar2->as->lower[n];

      /* Check we have values for both.  */
      if (!(e1 && e2))
	return def;

      i = gfc_dep_compare_expr (e1, e2);
      if (i == -2)
	return def;
      else if (i != 0)
	return 0;
    }

  /* Check the range end.  */
  e1 = ar1->end[n];
  e2 = ar2->end[n];
  if (e1 || e2)
    {
      /* Use the bound of the array if no bound is specified.  */
      if (ar1->as && !e1)
	e1 = ar1->as->upper[n];

      if (ar2->as && !e2)
	e2 = ar2->as->upper[n];

      /* Check we have values for both.  */
      if (!(e1 && e2))
	return def;

      i = gfc_dep_compare_expr (e1, e2);
      if (i == -2)
	return def;
      else if (i != 0)
	return 0;
    }

  return 1;
}


/* Some array-returning intrinsics can be implemented by reusing the
   data from one of the array arguments.  For example, TRANSPOSE does
   not necessarily need to allocate new data: it can be implemented
   by copying the original array's descriptor and simply swapping the
   two dimension specifications.

   If EXPR is a call to such an intrinsic, return the argument
   whose data can be reused, otherwise return NULL.  */

gfc_expr *
gfc_get_noncopying_intrinsic_argument (gfc_expr * expr)
{
  if (expr->expr_type != EXPR_FUNCTION || !expr->value.function.isym)
    return NULL;

  switch (expr->value.function.isym->generic_id)
    {
    case GFC_ISYM_TRANSPOSE:
      return expr->value.function.actual->expr;

    default:
      return NULL;
    }
}


/* Return true if the result of reference REF can only be constructed
   using a temporary array.  */

bool
gfc_ref_needs_temporary_p (gfc_ref *ref)
{
  int n;
  bool subarray_p;

  subarray_p = false;
  for (; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:
	/* Vector dimensions are generally not monotonic and must be
	   handled using a temporary.  */
	if (ref->u.ar.type == AR_SECTION)
	  for (n = 0; n < ref->u.ar.dimen; n++)
	    if (ref->u.ar.dimen_type[n] == DIMEN_VECTOR)
	      return true;

	subarray_p = true;
	break;

      case REF_SUBSTRING:
	/* Within an array reference, character substrings generally
	   need a temporary.  Character array strides are expressed as
	   multiples of the element size (consistent with other array
	   types), not in characters.  */
	return subarray_p;

      case REF_COMPONENT:
	break;
      }

  return false;
}


/* Return true if array variable VAR could be passed to the same function
   as argument EXPR without interfering with EXPR.  INTENT is the intent
   of VAR.

   This is considerably less conservative than other dependencies
   because many function arguments will already be copied into a
   temporary.  */

static int
gfc_check_argument_var_dependency (gfc_expr * var, sym_intent intent,
				   gfc_expr * expr)
{
  gcc_assert (var->expr_type == EXPR_VARIABLE);
  gcc_assert (var->rank > 0);

  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
      return (gfc_ref_needs_temporary_p (expr->ref)
	      || gfc_check_dependency (var, expr, 1));

    case EXPR_ARRAY:
      return gfc_check_dependency (var, expr, 1);

    case EXPR_FUNCTION:
      if (intent != INTENT_IN && expr->inline_noncopying_intrinsic)
	{
	  expr = gfc_get_noncopying_intrinsic_argument (expr);
	  return gfc_check_argument_var_dependency (var, intent, expr);
	}
      return 0;

    default:
      return 0;
    }
}
  
  
/* Like gfc_check_argument_var_dependency, but extended to any
   array expression OTHER, not just variables.  */

static int
gfc_check_argument_dependency (gfc_expr * other, sym_intent intent,
			       gfc_expr * expr)
{
  switch (other->expr_type)
    {
    case EXPR_VARIABLE:
      return gfc_check_argument_var_dependency (other, intent, expr);

    case EXPR_FUNCTION:
      if (other->inline_noncopying_intrinsic)
	{
	  other = gfc_get_noncopying_intrinsic_argument (other);
	  return gfc_check_argument_dependency (other, INTENT_IN, expr);
	}
      return 0;

    default:
      return 0;
    }
}


/* Like gfc_check_argument_dependency, but check all the arguments in ACTUAL.
   FNSYM is the function being called, or NULL if not known.  */

int
gfc_check_fncall_dependency (gfc_expr * other, sym_intent intent,
			     gfc_symbol * fnsym, gfc_actual_arglist * actual)
{
  gfc_formal_arglist *formal;
  gfc_expr *expr;

  formal = fnsym ? fnsym->formal : NULL;
  for (; actual; actual = actual->next, formal = formal ? formal->next : NULL)
    {
      expr = actual->expr;

      /* Skip args which are not present.  */
      if (!expr)
	continue;

      /* Skip intent(in) arguments if OTHER itself is intent(in).  */
      if (formal
	  && intent == INTENT_IN
	  && formal->sym->attr.intent == INTENT_IN)
	continue;

      if (gfc_check_argument_dependency (other, intent, expr))
	return 1;
    }

  return 0;
}


/* Return 1 if e1 and e2 are equivalenced arrays, either
   directly or indirectly; ie. equivalence (a,b) for a and b
   or equivalence (a,c),(b,c).  This function uses the equiv_
   lists, generated in trans-common(add_equivalences), that are
   guaranteed to pick up indirect equivalences.  We explicitly
   check for overlap using the offset and length of the equivalence.
   This function is symmetric.
   TODO: This function only checks whether the full top-level
   symbols overlap.  An improved implementation could inspect
   e1->ref and e2->ref to determine whether the actually accessed
   portions of these variables/arrays potentially overlap.  */

int
gfc_are_equivalenced_arrays (gfc_expr *e1, gfc_expr *e2)
{
  gfc_equiv_list *l;
  gfc_equiv_info *s, *fl1, *fl2;

  gcc_assert (e1->expr_type == EXPR_VARIABLE
		&& e2->expr_type == EXPR_VARIABLE);

  if (!e1->symtree->n.sym->attr.in_equivalence
	|| !e2->symtree->n.sym->attr.in_equivalence
	|| !e1->rank
	|| !e2->rank)
    return 0;

  /* Go through the equiv_lists and return 1 if the variables
     e1 and e2 are members of the same group and satisfy the
     requirement on their relative offsets.  */
  for (l = gfc_current_ns->equiv_lists; l; l = l->next)
    {
      fl1 = NULL;
      fl2 = NULL;
      for (s = l->equiv; s; s = s->next)
	{
	  if (s->sym == e1->symtree->n.sym)
	    {
	      fl1 = s;
	      if (fl2)
		break;
	    }
	  if (s->sym == e2->symtree->n.sym)
	    {
	      fl2 = s;
	      if (fl1)
		break;
	    }
	}

      if (s)
	{
	  /* Can these lengths be zero?  */
	  if (fl1->length <= 0 || fl2->length <= 0)
	    return 1;
	  /* These can't overlap if [f11,fl1+length] is before 
	     [fl2,fl2+length], or [fl2,fl2+length] is before
	     [fl1,fl1+length], otherwise they do overlap.  */
	  if (fl1->offset + fl1->length > fl2->offset
	      && fl2->offset + fl2->length > fl1->offset)
	    return 1;
	}
    }
  return 0;
}


/* Return true if the statement body redefines the condition.  Returns
   true if expr2 depends on expr1.  expr1 should be a single term
   suitable for the lhs of an assignment.  The IDENTICAL flag indicates
   whether array references to the same symbol with identical range
   references count as a dependency or not.  Used for forall and where
   statements.  Also used with functions returning arrays without a
   temporary.  */

int
gfc_check_dependency (gfc_expr * expr1, gfc_expr * expr2, bool identical)
{
  gfc_ref *ref;
  int n;
  gfc_actual_arglist *actual;

  gcc_assert (expr1->expr_type == EXPR_VARIABLE);

  switch (expr2->expr_type)
    {
    case EXPR_OP:
      n = gfc_check_dependency (expr1, expr2->value.op.op1, identical);
      if (n)
	return n;
      if (expr2->value.op.op2)
	return gfc_check_dependency (expr1, expr2->value.op.op2, identical);
      return 0;

    case EXPR_VARIABLE:
      /* The interesting cases are when the symbols don't match.  */
      if (expr1->symtree->n.sym != expr2->symtree->n.sym)
	{
	  gfc_typespec *ts1 = &expr1->symtree->n.sym->ts;
	  gfc_typespec *ts2 = &expr2->symtree->n.sym->ts;

	  /* Return 1 if expr1 and expr2 are equivalenced arrays.  */
	  if (gfc_are_equivalenced_arrays (expr1, expr2))
	    return 1;

	  /* Symbols can only alias if they have the same type.  */
	  if (ts1->type != BT_UNKNOWN
	      && ts2->type != BT_UNKNOWN
	      && ts1->type != BT_DERIVED
	      && ts2->type != BT_DERIVED)
	    {
	      if (ts1->type != ts2->type
		  || ts1->kind != ts2->kind)
		return 0;
	    }

	  /* If either variable is a pointer, assume the worst.  */
	  /* TODO: -fassume-no-pointer-aliasing */
	  if (expr1->symtree->n.sym->attr.pointer)
	    return 1;
	  for (ref = expr1->ref; ref; ref = ref->next)
	    if (ref->type == REF_COMPONENT && ref->u.c.component->pointer)
	      return 1;

	  if (expr2->symtree->n.sym->attr.pointer)
	    return 1;
	  for (ref = expr2->ref; ref; ref = ref->next)
	    if (ref->type == REF_COMPONENT && ref->u.c.component->pointer)
	      return 1;

	  /* Otherwise distinct symbols have no dependencies.  */
	  return 0;
	}

      if (identical)
	return 1;

      /* Identical and disjoint ranges return 0,
	 overlapping ranges return 1.  */
      /* Return zero if we refer to the same full arrays.  */
      if (expr1->ref->type == REF_ARRAY && expr2->ref->type == REF_ARRAY)
	return gfc_dep_resolver (expr1->ref, expr2->ref);

      return 1;

    case EXPR_FUNCTION:
      if (expr2->inline_noncopying_intrinsic)
	identical = 1;
      /* Remember possible differences between elemental and
	 transformational functions.  All functions inside a FORALL
	 will be pure.  */
      for (actual = expr2->value.function.actual;
	   actual; actual = actual->next)
	{
	  if (!actual->expr)
	    continue;
	  n = gfc_check_dependency (expr1, actual->expr, identical);
	  if (n)
	    return n;
	}
      return 0;

    case EXPR_CONSTANT:
      return 0;

    case EXPR_ARRAY:
      /* Probably ok in the majority of (constant) cases.  */
      return 1;

    default:
      return 1;
    }
}


/* Calculates size of the array reference using lower bound, upper bound
   and stride.  */

static void
get_no_of_elements(mpz_t ele, gfc_expr * u1, gfc_expr * l1, gfc_expr * s1)
{
  /* nNoOfEle = (u1-l1)/s1  */

  mpz_sub (ele, u1->value.integer, l1->value.integer);

  if (s1 != NULL)
    mpz_tdiv_q (ele, ele, s1->value.integer);
}


/* Returns if the ranges ((0..Y), (X1..X2))  overlap.  */

static gfc_dependency
get_deps (mpz_t x1, mpz_t x2, mpz_t y)
{
  int start;
  int end;

  start = mpz_cmp_ui (x1, 0);
  end = mpz_cmp (x2, y);
  
  /* Both ranges the same.  */
  if (start == 0 && end == 0)
    return GFC_DEP_EQUAL;

  /* Distinct ranges.  */
  if ((start < 0 && mpz_cmp_ui (x2, 0) < 0)
      || (mpz_cmp (x1, y) > 0 && end > 0))
    return GFC_DEP_NODEP;

  /* Overlapping, but with corresponding elements of the second range
     greater than the first.  */
  if (start > 0 && end > 0)
    return GFC_DEP_FORWARD;

  /* Overlapping in some other way.  */
  return GFC_DEP_OVERLAP;
}


/* Perform the same linear transformation on sections l and r such that 
   (l_start:l_end:l_stride) -> (0:no_of_elements)
   (r_start:r_end:r_stride) -> (X1:X2)
   Where r_end is implicit as both sections must have the same number of
   elements.
   Returns 0 on success, 1 of the transformation failed.  */
/* TODO: Should this be (0:no_of_elements-1) */

static int
transform_sections (mpz_t X1, mpz_t X2, mpz_t no_of_elements,
		    gfc_expr * l_start, gfc_expr * l_end, gfc_expr * l_stride,
		    gfc_expr * r_start, gfc_expr * r_stride)
{
  if (NULL == l_start || NULL == l_end || NULL == r_start)
    return 1;

  /* TODO : Currently we check the dependency only when start, end and stride
    are constant.  We could also check for equal (variable) values, and
    common subexpressions, eg. x vs. x+1.  */

  if (l_end->expr_type != EXPR_CONSTANT
      || l_start->expr_type != EXPR_CONSTANT
      || r_start->expr_type != EXPR_CONSTANT
      || ((NULL != l_stride) && (l_stride->expr_type != EXPR_CONSTANT))
      || ((NULL != r_stride) && (r_stride->expr_type != EXPR_CONSTANT)))
    {
       return 1;
    }


  get_no_of_elements (no_of_elements, l_end, l_start, l_stride);

  mpz_sub (X1, r_start->value.integer, l_start->value.integer);
  if (l_stride != NULL)
    mpz_cdiv_q (X1, X1, l_stride->value.integer);
  
  if (r_stride == NULL)
    mpz_set (X2, no_of_elements);
  else
    mpz_mul (X2, no_of_elements, r_stride->value.integer);

  if (l_stride != NULL)
    mpz_cdiv_q (X2, X2, l_stride->value.integer);
  mpz_add (X2, X2, X1);

  return 0;
}
  

/* Determines overlapping for two array sections.  */

static gfc_dependency
gfc_check_section_vs_section (gfc_ref * lref, gfc_ref * rref, int n)
{
  gfc_expr *l_start;
  gfc_expr *l_end;
  gfc_expr *l_stride;

  gfc_expr *r_start;
  gfc_expr *r_stride;

  gfc_array_ref l_ar;
  gfc_array_ref r_ar;

  mpz_t no_of_elements;
  mpz_t X1, X2;
  gfc_dependency dep;

  l_ar = lref->u.ar;
  r_ar = rref->u.ar;
  
  /* If they are the same range, return without more ado.  */
  if (gfc_is_same_range (&l_ar, &r_ar, n, 0))
    return GFC_DEP_EQUAL;

  l_start = l_ar.start[n];
  l_end = l_ar.end[n];
  l_stride = l_ar.stride[n];
  r_start = r_ar.start[n];
  r_stride = r_ar.stride[n];

  /* if l_start is NULL take it from array specifier  */
  if (NULL == l_start && IS_ARRAY_EXPLICIT(l_ar.as))
    l_start = l_ar.as->lower[n];

  /* if l_end is NULL take it from array specifier  */
  if (NULL == l_end && IS_ARRAY_EXPLICIT(l_ar.as))
    l_end = l_ar.as->upper[n];

  /* if r_start is NULL take it from array specifier  */
  if (NULL == r_start && IS_ARRAY_EXPLICIT(r_ar.as))
    r_start = r_ar.as->lower[n];

  mpz_init (X1);
  mpz_init (X2);
  mpz_init (no_of_elements);

  if (transform_sections (X1, X2, no_of_elements,
			  l_start, l_end, l_stride,
			  r_start, r_stride))
    dep = GFC_DEP_OVERLAP;
  else
    dep =  get_deps (X1, X2, no_of_elements);

  mpz_clear (no_of_elements);
  mpz_clear (X1);
  mpz_clear (X2);
  return dep;
}


/* Checks if the expr chk is inside the range left-right.
   Returns  GFC_DEP_NODEP if chk is outside the range,
   GFC_DEP_OVERLAP otherwise.
   Assumes left<=right.  */

static gfc_dependency
gfc_is_inside_range (gfc_expr * chk, gfc_expr * left, gfc_expr * right)
{
  int l;
  int r;
  int s;

  s = gfc_dep_compare_expr (left, right);
  if (s == -2)
    return GFC_DEP_OVERLAP;

  l = gfc_dep_compare_expr (chk, left);
  r = gfc_dep_compare_expr (chk, right);

  /* Check for indeterminate relationships.  */
  if (l == -2 || r == -2 || s == -2)
    return GFC_DEP_OVERLAP;

  if (s == 1)
    {
      /* When left>right we want to check for right <= chk <= left.  */
      if (l <= 0 || r >= 0)
	return GFC_DEP_OVERLAP;
    }
  else
    {
      /* Otherwise check for left <= chk <= right.  */
      if (l >= 0 || r <= 0)
	return GFC_DEP_OVERLAP;
    }
  
  return GFC_DEP_NODEP;
}


/* Determines overlapping for a single element and a section.  */

static gfc_dependency
gfc_check_element_vs_section( gfc_ref * lref, gfc_ref * rref, int n)
{
  gfc_array_ref l_ar;
  gfc_array_ref r_ar;
  gfc_expr *l_start;
  gfc_expr *r_start;
  gfc_expr *r_end;

  l_ar = lref->u.ar;
  r_ar = rref->u.ar;
  l_start = l_ar.start[n] ;
  r_start = r_ar.start[n] ;
  r_end = r_ar.end[n] ;
  if (NULL == r_start && IS_ARRAY_EXPLICIT (r_ar.as))
    r_start = r_ar.as->lower[n];
  if (NULL == r_end && IS_ARRAY_EXPLICIT (r_ar.as))
    r_end = r_ar.as->upper[n];
  if (NULL == r_start || NULL == r_end || l_start == NULL)
    return GFC_DEP_OVERLAP;

  return gfc_is_inside_range (l_start, r_end, r_start);
}


/* Traverse expr, checking all EXPR_VARIABLE symbols for their
   forall_index attribute.  Return true if any variable may be
   being used as a FORALL index.  Its safe to pessimistically
   return true, and assume a dependency.  */

static bool
contains_forall_index_p (gfc_expr * expr)
{
  gfc_actual_arglist *arg;
  gfc_constructor *c;
  gfc_ref *ref;
  int i;

  if (!expr)
    return false;

  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
      if (expr->symtree->n.sym->forall_index)
	return true;
      break;

    case EXPR_OP:
      if (contains_forall_index_p (expr->value.op.op1)
	  || contains_forall_index_p (expr->value.op.op2))
	return true;
      break;

    case EXPR_FUNCTION:
      for (arg = expr->value.function.actual; arg; arg = arg->next)
	if (contains_forall_index_p (arg->expr))
	  return true;
      break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
    case EXPR_SUBSTRING:
      break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
      for (c = expr->value.constructor; c; c = c->next)
	if (contains_forall_index_p (c->expr))
	  return true;
      break;

    default:
      gcc_unreachable ();
    }

  for (ref = expr->ref; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:
	for (i = 0; i < ref->u.ar.dimen; i++)
	  if (contains_forall_index_p (ref->u.ar.start[i])
	      || contains_forall_index_p (ref->u.ar.end[i])
	      || contains_forall_index_p (ref->u.ar.stride[i]))
	    return true;
	break;

      case REF_COMPONENT:
	break;

      case REF_SUBSTRING:
	if (contains_forall_index_p (ref->u.ss.start)
	    || contains_forall_index_p (ref->u.ss.end))
	  return true;
	break;

      default:
	gcc_unreachable ();
      }

  return false;
}

/* Determines overlapping for two single element array references.  */

static gfc_dependency
gfc_check_element_vs_element (gfc_ref * lref, gfc_ref * rref, int n)
{
  gfc_array_ref l_ar;
  gfc_array_ref r_ar;
  gfc_expr *l_start;
  gfc_expr *r_start;
  int i;

  l_ar = lref->u.ar;
  r_ar = rref->u.ar;
  l_start = l_ar.start[n] ;
  r_start = r_ar.start[n] ;
  i = gfc_dep_compare_expr (r_start, l_start);
  if (i == 0)
    return GFC_DEP_EQUAL;

  /* Treat two scalar variables as potentially equal.  This allows
     us to prove that a(i,:) and a(j,:) have no dependency.  See
     Gerald Roth, "Evaluation of Array Syntax Dependence Analysis",
     Proceedings of the International Conference on Parallel and
     Distributed Processing Techniques and Applications (PDPTA2001),
     Las Vegas, Nevada, June 2001.  */
  /* However, we need to be careful when either scalar expression
     contains a FORALL index, as these can potentially change value
     during the scalarization/traversal of this array reference.  */
  if (contains_forall_index_p (r_start)
      || contains_forall_index_p (l_start))
    return GFC_DEP_OVERLAP;

  if (i != -2)
    return GFC_DEP_NODEP;
  return GFC_DEP_EQUAL;
}


/* Finds if two array references are overlapping or not.
   Return value
   	1 : array references are overlapping.
   	0 : array references are identical or not overlapping.  */

int
gfc_dep_resolver (gfc_ref * lref, gfc_ref * rref)
{
  int n;
  gfc_dependency fin_dep;
  gfc_dependency this_dep;


  fin_dep = GFC_DEP_ERROR;
  /* Dependencies due to pointers should already have been identified.
     We only need to check for overlapping array references.  */

  while (lref && rref)
    {
      /* We're resolving from the same base symbol, so both refs should be
	 the same type.  We traverse the reference chain intil we find ranges
	 that are not equal.  */
      gcc_assert (lref->type == rref->type);
      switch (lref->type)
	{
	case REF_COMPONENT:
	  /* The two ranges can't overlap if they are from different
	     components.  */
	  if (lref->u.c.component != rref->u.c.component)
	    return 0;
	  break;
	  
	case REF_SUBSTRING:
	  /* Substring overlaps are handled by the string assignment code.  */
	  return 0;
	
	case REF_ARRAY:
	  for (n=0; n < lref->u.ar.dimen; n++)
	    {
	      /* Assume dependency when either of array reference is vector
		 subscript.  */
	      if (lref->u.ar.dimen_type[n] == DIMEN_VECTOR
		  || rref->u.ar.dimen_type[n] == DIMEN_VECTOR)
		return 1;
	      if (lref->u.ar.dimen_type[n] == DIMEN_RANGE
		  && rref->u.ar.dimen_type[n] == DIMEN_RANGE)
		this_dep = gfc_check_section_vs_section (lref, rref, n);
	      else if (lref->u.ar.dimen_type[n] == DIMEN_ELEMENT
		       && rref->u.ar.dimen_type[n] == DIMEN_RANGE)
		this_dep = gfc_check_element_vs_section (lref, rref, n);
	      else if (rref->u.ar.dimen_type[n] == DIMEN_ELEMENT
		       && lref->u.ar.dimen_type[n] == DIMEN_RANGE)
		this_dep = gfc_check_element_vs_section (rref, lref, n);
	      else 
		{
		  gcc_assert (rref->u.ar.dimen_type[n] == DIMEN_ELEMENT
			      && lref->u.ar.dimen_type[n] == DIMEN_ELEMENT);
		  this_dep = gfc_check_element_vs_element (rref, lref, n);
		}

	      /* If any dimension doesn't overlap, we have no dependency.  */
	      if (this_dep == GFC_DEP_NODEP)
		return 0;

	      /* Overlap codes are in order of priority.  We only need to
		 know the worst one.*/
	      if (this_dep > fin_dep)
		fin_dep = this_dep;
	    }
	  /* Exactly matching and forward overlapping ranges don't cause a
	     dependency.  */
	  if (fin_dep < GFC_DEP_OVERLAP)
	    return 0;

	  /* Keep checking.  We only have a dependency if
	     subsequent references also overlap.  */
	  break;

	default:
	  gcc_unreachable ();
	}
      lref = lref->next;
      rref = rref->next;
    }

  /* If we haven't seen any array refs then something went wrong.  */
  gcc_assert (fin_dep != GFC_DEP_ERROR);

  /* Assume the worst if we nest to different depths.  */
  if (lref || rref)
    return 1;

  return fin_dep == GFC_DEP_OVERLAP;
}

