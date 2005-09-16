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
  int i;

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

  if (!(e1 || e2))
    return 1;

  /* Use the bound of the array if no bound is specified.  */
  if (ar1->as && !e1)
    e1 = ar1->as->lower[n];

  if (ar2->as && !e2)
    e2 = ar2->as->upper[n];

  /* Check we have values for both.  */
  if (!(e1 && e2))
    return def;

  i = gfc_dep_compare_expr (e1, e2);

  if (i == -2)
    return def;
  else if (i == 0)
    return 1;
  return 0;
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


/* Dependency checking for direct function return by reference.
   Returns true if the arguments of the function depend on the
   destination.  This is considerably less conservative than other
   dependencies because many function arguments will already be
   copied into a temporary.  */

int
gfc_check_fncall_dependency (gfc_expr * dest, gfc_expr * fncall)
{
  gfc_actual_arglist *actual;
  gfc_expr *expr;

  gcc_assert (dest->expr_type == EXPR_VARIABLE
	  && fncall->expr_type == EXPR_FUNCTION);
  gcc_assert (fncall->rank > 0);

  for (actual = fncall->value.function.actual; actual; actual = actual->next)
    {
      expr = actual->expr;

      /* Skip args which are not present.  */
      if (!expr)
	continue;

      /* Non-variable expressions will be allocated temporaries anyway.  */
      switch (expr->expr_type)
	{
	case EXPR_VARIABLE:
	  if (!gfc_ref_needs_temporary_p (expr->ref)
	      && gfc_check_dependency (dest, expr, NULL, 0))
	    return 1;
	  break;

	case EXPR_ARRAY:
	  if (gfc_check_dependency (dest, expr, NULL, 0))
	    return 1;
	  break;

	default:
	  break;
	}
    }

  return 0;
}


/* Return true if the statement body redefines the condition.  Returns
   true if expr2 depends on expr1.  expr1 should be a single term
   suitable for the lhs of an assignment.  The symbols listed in VARS
   must be considered to have all possible values. All other scalar
   variables may be considered constant.  Used for forall and where
   statements.  Also used with functions returning arrays without a
   temporary.  */

int
gfc_check_dependency (gfc_expr * expr1, gfc_expr * expr2, gfc_expr ** vars,
		      int nvars)
{
  gfc_ref *ref;
  int n;
  gfc_actual_arglist *actual;

  gcc_assert (expr1->expr_type == EXPR_VARIABLE);

  /* TODO: -fassume-no-pointer-aliasing */
  if (expr1->symtree->n.sym->attr.pointer)
    return 1;
  for (ref = expr1->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT && ref->u.c.component->pointer)
	return 1;
    }

  switch (expr2->expr_type)
    {
    case EXPR_OP:
      n = gfc_check_dependency (expr1, expr2->value.op.op1, vars, nvars);
      if (n)
	return n;
      if (expr2->value.op.op2)
	return gfc_check_dependency (expr1, expr2->value.op.op2, vars, nvars);
      return 0;

    case EXPR_VARIABLE:
      if (expr2->symtree->n.sym->attr.pointer)
	return 1;

      for (ref = expr2->ref; ref; ref = ref->next)
	{
	  if (ref->type == REF_COMPONENT && ref->u.c.component->pointer)
	    return 1;
	}

      if (expr1->symtree->n.sym != expr2->symtree->n.sym)
	return 0;

      for (ref = expr2->ref; ref; ref = ref->next)
	{
	  /* Identical ranges return 0, overlapping ranges return 1.  */
	  if (ref->type == REF_ARRAY)
	    return 1;
	}
      return 1;

    case EXPR_FUNCTION:
      /* Remember possible differences between elemental and
         transformational functions.  All functions inside a FORALL
         will be pure.  */
      for (actual = expr2->value.function.actual;
	   actual; actual = actual->next)
	{
	  if (!actual->expr)
	    continue;
	  n = gfc_check_dependency (expr1, actual->expr, vars, nvars);
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

  gfc_array_ref	l_ar;
  gfc_array_ref	r_ar;

  mpz_t no_of_elements;
  mpz_t	X1, X2;
  gfc_dependency dep;

  l_ar = lref->u.ar;
  r_ar = rref->u.ar;

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


/* Determines overlapping for two single element array references.  */

static gfc_dependency
gfc_check_element_vs_element (gfc_ref * lref, gfc_ref * rref, int n)
{
  gfc_array_ref l_ar;
  gfc_array_ref r_ar;
  gfc_expr *l_start;
  gfc_expr *r_start;
  gfc_dependency nIsDep;

  if (lref->type == REF_ARRAY && rref->type == REF_ARRAY)
    {
      l_ar = lref->u.ar;
      r_ar = rref->u.ar;
      l_start = l_ar.start[n] ;
      r_start = r_ar.start[n] ;
      if (gfc_dep_compare_expr (r_start, l_start) == 0)
        nIsDep = GFC_DEP_EQUAL;
      else
	nIsDep = GFC_DEP_NODEP;
  }
  else
    nIsDep = GFC_DEP_NODEP;

  return nIsDep;
}


/* Finds if two array references are overlapping or not.
   Return value
   	1 : array references are overlapping.
   	0 : array references are not overlapping.  */

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

  if (fin_dep < GFC_DEP_OVERLAP)
    return 0;
  else
    return 1;
}

