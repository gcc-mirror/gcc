/* Check functions
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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


/* These functions check to see if an argument list is compatible with
   a particular intrinsic function or subroutine.  Presence of
   required arguments has already been established, the argument list
   has been sorted into the right order and has NULL arguments in the
   correct places for missing optional arguments.  */

#include "config.h"
#include "system.h"
#include "flags.h"
#include "gfortran.h"
#include "intrinsic.h"
#include "constructor.h"
#include "target-memory.h"


/* Make sure an expression is a scalar.  */

static gfc_try
scalar_check (gfc_expr *e, int n)
{
  if (e->rank == 0)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be a scalar",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where);

  return FAILURE;
}


/* Check the type of an expression.  */

static gfc_try
type_check (gfc_expr *e, int n, bt type)
{
  if (e->ts.type == type)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be %s",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where, gfc_basic_typename (type));

  return FAILURE;
}


/* Check that the expression is a numeric type.  */

static gfc_try
numeric_check (gfc_expr *e, int n)
{
  if (gfc_numeric_ts (&e->ts))
    return SUCCESS;

  /* If the expression has not got a type, check if its namespace can
     offer a default type.  */
  if ((e->expr_type == EXPR_VARIABLE || e->expr_type == EXPR_VARIABLE)
	&& e->symtree->n.sym->ts.type == BT_UNKNOWN
	&& gfc_set_default_type (e->symtree->n.sym, 0,
				 e->symtree->n.sym->ns) == SUCCESS
	&& gfc_numeric_ts (&e->symtree->n.sym->ts))
    {
      e->ts = e->symtree->n.sym->ts;
      return SUCCESS;
    }

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be a numeric type",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where);

  return FAILURE;
}


/* Check that an expression is integer or real.  */

static gfc_try
int_or_real_check (gfc_expr *e, int n)
{
  if (e->ts.type != BT_INTEGER && e->ts.type != BT_REAL)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be INTEGER "
		 "or REAL", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Check that an expression is real or complex.  */

static gfc_try
real_or_complex_check (gfc_expr *e, int n)
{
  if (e->ts.type != BT_REAL && e->ts.type != BT_COMPLEX)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be REAL "
		 "or COMPLEX", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Check that an expression is INTEGER or PROCEDURE.  */

static gfc_try
int_or_proc_check (gfc_expr *e, int n)
{
  if (e->ts.type != BT_INTEGER && e->ts.type != BT_PROCEDURE)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be INTEGER "
		 "or PROCEDURE", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Check that the expression is an optional constant integer
   and that it specifies a valid kind for that type.  */

static gfc_try
kind_check (gfc_expr *k, int n, bt type)
{
  int kind;

  if (k == NULL)
    return SUCCESS;

  if (type_check (k, n, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (k, n) == FAILURE)
    return FAILURE;

  if (k->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a constant",
		 gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
		 &k->where);
      return FAILURE;
    }

  if (gfc_extract_int (k, &kind) != NULL
      || gfc_validate_kind (type, kind, true) < 0)
    {
      gfc_error ("Invalid kind for %s at %L", gfc_basic_typename (type),
		 &k->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Make sure the expression is a double precision real.  */

static gfc_try
double_check (gfc_expr *d, int n)
{
  if (type_check (d, n, BT_REAL) == FAILURE)
    return FAILURE;

  if (d->ts.kind != gfc_default_double_kind)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be double "
		 "precision", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &d->where);
      return FAILURE;
    }

  return SUCCESS;
}


static gfc_try
coarray_check (gfc_expr *e, int n)
{
  if (e->ts.type == BT_CLASS && gfc_expr_attr (e).class_ok
	&& CLASS_DATA (e)->attr.codimension
	&& CLASS_DATA (e)->as->corank)
    {
      gfc_add_class_array_ref (e);
      return SUCCESS;
    }

  if (!gfc_is_coarray (e))
    {
      gfc_error ("Expected coarray variable as '%s' argument to the %s "
                 "intrinsic at %L", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return FAILURE;
    }

  return SUCCESS;
} 


/* Make sure the expression is a logical array.  */

static gfc_try
logical_array_check (gfc_expr *array, int n)
{
  if (array->ts.type != BT_LOGICAL || array->rank == 0)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a logical "
		 "array", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &array->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Make sure an expression is an array.  */

static gfc_try
array_check (gfc_expr *e, int n)
{
  if (e->ts.type == BT_CLASS && gfc_expr_attr (e).class_ok
	&& CLASS_DATA (e)->attr.dimension
	&& CLASS_DATA (e)->as->rank)
    {
      gfc_add_class_array_ref (e);
      return SUCCESS;
    }

  if (e->rank != 0 && e->ts.type != BT_PROCEDURE)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be an array",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where);

  return FAILURE;
}


/* If expr is a constant, then check to ensure that it is greater than
   of equal to zero.  */

static gfc_try
nonnegative_check (const char *arg, gfc_expr *expr)
{
  int i;

  if (expr->expr_type == EXPR_CONSTANT)
    {
      gfc_extract_int (expr, &i);
      if (i < 0)
	{
	  gfc_error ("'%s' at %L must be nonnegative", arg, &expr->where);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


/* If expr2 is constant, then check that the value is less than
   (less than or equal to, if 'or_equal' is true) bit_size(expr1).  */

static gfc_try
less_than_bitsize1 (const char *arg1, gfc_expr *expr1, const char *arg2,
		    gfc_expr *expr2, bool or_equal)
{
  int i2, i3;

  if (expr2->expr_type == EXPR_CONSTANT)
    {
      gfc_extract_int (expr2, &i2);
      i3 = gfc_validate_kind (BT_INTEGER, expr1->ts.kind, false);
 
      /* For ISHFT[C], check that |shift| <= bit_size(i).  */
      if (arg2 == NULL)
	{
	  if (i2 < 0)
	    i2 = -i2;

	  if (i2 > gfc_integer_kinds[i3].bit_size)
	    {
	      gfc_error ("The absolute value of SHIFT at %L must be less "
			 "than or equal to BIT_SIZE('%s')",
			 &expr2->where, arg1);
	      return FAILURE;
	    }
	}

      if (or_equal)
	{
	  if (i2 > gfc_integer_kinds[i3].bit_size)
	    {
	      gfc_error ("'%s' at %L must be less than "
			 "or equal to BIT_SIZE('%s')",
			 arg2, &expr2->where, arg1);
	      return FAILURE;
	    }
	}
      else
	{
	  if (i2 >= gfc_integer_kinds[i3].bit_size)
	    {
	      gfc_error ("'%s' at %L must be less than BIT_SIZE('%s')",
			 arg2, &expr2->where, arg1);
	      return FAILURE;
	    }
	}
    }

  return SUCCESS;
}


/* If expr is constant, then check that the value is less than or equal
   to the bit_size of the kind k.  */

static gfc_try
less_than_bitsizekind (const char *arg, gfc_expr *expr, int k)
{
  int i, val;

  if (expr->expr_type != EXPR_CONSTANT)
    return SUCCESS;
 
  i = gfc_validate_kind (BT_INTEGER, k, false);
  gfc_extract_int (expr, &val);

  if (val > gfc_integer_kinds[i].bit_size)
    {
      gfc_error ("'%s' at %L must be less than or equal to the BIT_SIZE of "
		 "INTEGER(KIND=%d)", arg, &expr->where, k);
      return FAILURE;
    }

  return SUCCESS;
}


/* If expr2 and expr3 are constants, then check that the value is less than
   or equal to bit_size(expr1).  */

static gfc_try
less_than_bitsize2 (const char *arg1, gfc_expr *expr1, const char *arg2,
	       gfc_expr *expr2, const char *arg3, gfc_expr *expr3)
{
  int i2, i3;

  if (expr2->expr_type == EXPR_CONSTANT && expr3->expr_type == EXPR_CONSTANT)
    {
      gfc_extract_int (expr2, &i2);
      gfc_extract_int (expr3, &i3);
      i2 += i3;
      i3 = gfc_validate_kind (BT_INTEGER, expr1->ts.kind, false);
      if (i2 > gfc_integer_kinds[i3].bit_size)
	{
	  gfc_error ("'%s + %s' at %L must be less than or equal "
		     "to BIT_SIZE('%s')",
		     arg2, arg3, &expr2->where, arg1);
	  return FAILURE;
	}
    }

  return SUCCESS;
}

/* Make sure two expressions have the same type.  */

static gfc_try
same_type_check (gfc_expr *e, int n, gfc_expr *f, int m)
{
  if (gfc_compare_types (&e->ts, &f->ts))
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be the same type "
	     "and kind as '%s'", gfc_current_intrinsic_arg[m]->name,
	     gfc_current_intrinsic, &f->where,
	     gfc_current_intrinsic_arg[n]->name);

  return FAILURE;
}


/* Make sure that an expression has a certain (nonzero) rank.  */

static gfc_try
rank_check (gfc_expr *e, int n, int rank)
{
  if (e->rank == rank)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be of rank %d",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where, rank);

  return FAILURE;
}


/* Make sure a variable expression is not an optional dummy argument.  */

static gfc_try
nonoptional_check (gfc_expr *e, int n)
{
  if (e->expr_type == EXPR_VARIABLE && e->symtree->n.sym->attr.optional)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must not be OPTIONAL",
		 gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
		 &e->where);
    }

  /* TODO: Recursive check on nonoptional variables?  */

  return SUCCESS;
}


/* Check for ALLOCATABLE attribute.  */

static gfc_try
allocatable_check (gfc_expr *e, int n)
{
  symbol_attribute attr;

  attr = gfc_variable_attr (e, NULL);
  if (!attr.allocatable || attr.associate_var)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be ALLOCATABLE",
		 gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
		 &e->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Check that an expression has a particular kind.  */

static gfc_try
kind_value_check (gfc_expr *e, int n, int k)
{
  if (e->ts.kind == k)
    return SUCCESS;

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be of kind %d",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where, k);

  return FAILURE;
}


/* Make sure an expression is a variable.  */

static gfc_try
variable_check (gfc_expr *e, int n, bool allow_proc)
{
  if (e->expr_type == EXPR_VARIABLE
      && e->symtree->n.sym->attr.intent == INTENT_IN
      && (gfc_current_intrinsic_arg[n]->intent == INTENT_OUT
	  || gfc_current_intrinsic_arg[n]->intent == INTENT_INOUT))
    {
      gfc_ref *ref;
      bool pointer = e->symtree->n.sym->ts.type == BT_CLASS
		     && CLASS_DATA (e->symtree->n.sym)
		     ? CLASS_DATA (e->symtree->n.sym)->attr.class_pointer
		     : e->symtree->n.sym->attr.pointer;

      for (ref = e->ref; ref; ref = ref->next)
	{
	  if (pointer && ref->type == REF_COMPONENT)
	    break;
	  if (ref->type == REF_COMPONENT
	      && ((ref->u.c.component->ts.type == BT_CLASS
		   && CLASS_DATA (ref->u.c.component)->attr.class_pointer)
		  || (ref->u.c.component->ts.type != BT_CLASS
		      && ref->u.c.component->attr.pointer)))
	    break;
	} 

      if (!ref)
	{
	  gfc_error ("'%s' argument of '%s' intrinsic at %L cannot be "
		     "INTENT(IN)", gfc_current_intrinsic_arg[n]->name,
		     gfc_current_intrinsic, &e->where);
	  return FAILURE;
	}
    }

  if (e->expr_type == EXPR_VARIABLE
      && e->symtree->n.sym->attr.flavor != FL_PARAMETER
      && (allow_proc || !e->symtree->n.sym->attr.function))
    return SUCCESS;

  if (e->expr_type == EXPR_VARIABLE && e->symtree->n.sym->attr.function
      && e->symtree->n.sym == e->symtree->n.sym->result)
    {
      gfc_namespace *ns;
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	if (ns->proc_name == e->symtree->n.sym)
	  return SUCCESS;
    }

  gfc_error ("'%s' argument of '%s' intrinsic at %L must be a variable",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic, &e->where);

  return FAILURE;
}


/* Check the common DIM parameter for correctness.  */

static gfc_try
dim_check (gfc_expr *dim, int n, bool optional)
{
  if (dim == NULL)
    return SUCCESS;

  if (type_check (dim, n, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (dim, n) == FAILURE)
    return FAILURE;

  if (!optional && nonoptional_check (dim, n) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* If a coarray DIM parameter is a constant, make sure that it is greater than
   zero and less than or equal to the corank of the given array.  */

static gfc_try
dim_corank_check (gfc_expr *dim, gfc_expr *array)
{
  int corank;

  gcc_assert (array->expr_type == EXPR_VARIABLE);

  if (dim->expr_type != EXPR_CONSTANT)
    return SUCCESS;
  
  if (array->ts.type == BT_CLASS)
    return SUCCESS;

  corank = gfc_get_corank (array);

  if (mpz_cmp_ui (dim->value.integer, 1) < 0
      || mpz_cmp_ui (dim->value.integer, corank) > 0)
    {
      gfc_error ("'dim' argument of '%s' intrinsic at %L is not a valid "
		 "codimension index", gfc_current_intrinsic, &dim->where);

      return FAILURE;
    }

  return SUCCESS;
}


/* If a DIM parameter is a constant, make sure that it is greater than
   zero and less than or equal to the rank of the given array.  If
   allow_assumed is zero then dim must be less than the rank of the array
   for assumed size arrays.  */

static gfc_try
dim_rank_check (gfc_expr *dim, gfc_expr *array, int allow_assumed)
{
  gfc_array_ref *ar;
  int rank;

  if (dim == NULL)
    return SUCCESS;

  if (dim->expr_type != EXPR_CONSTANT)
    return SUCCESS;

  if (array->ts.type == BT_CLASS)
    return SUCCESS;

  if (array->expr_type == EXPR_FUNCTION && array->value.function.isym
      && array->value.function.isym->id == GFC_ISYM_SPREAD)
    rank = array->rank + 1;
  else
    rank = array->rank;

  if (array->expr_type == EXPR_VARIABLE)
    {
      ar = gfc_find_array_ref (array);
      if (ar->as->type == AS_ASSUMED_SIZE
	  && !allow_assumed
	  && ar->type != AR_ELEMENT
	  && ar->type != AR_SECTION)
	rank--;
    }

  if (mpz_cmp_ui (dim->value.integer, 1) < 0
      || mpz_cmp_ui (dim->value.integer, rank) > 0)
    {
      gfc_error ("'dim' argument of '%s' intrinsic at %L is not a valid "
		 "dimension index", gfc_current_intrinsic, &dim->where);

      return FAILURE;
    }

  return SUCCESS;
}


/* Compare the size of a along dimension ai with the size of b along
   dimension bi, returning 0 if they are known not to be identical,
   and 1 if they are identical, or if this cannot be determined.  */

static int
identical_dimen_shape (gfc_expr *a, int ai, gfc_expr *b, int bi)
{
  mpz_t a_size, b_size;
  int ret;

  gcc_assert (a->rank > ai);
  gcc_assert (b->rank > bi);

  ret = 1;

  if (gfc_array_dimen_size (a, ai, &a_size) == SUCCESS)
    {
      if (gfc_array_dimen_size (b, bi, &b_size) == SUCCESS)
	{
	  if (mpz_cmp (a_size, b_size) != 0)
	    ret = 0;
  
	  mpz_clear (b_size);
	}
      mpz_clear (a_size);
    }
  return ret;
}

/*  Calculate the length of a character variable, including substrings.
    Strip away parentheses if necessary.  Return -1 if no length could
    be determined.  */

static long
gfc_var_strlen (const gfc_expr *a)
{
  gfc_ref *ra;

  while (a->expr_type == EXPR_OP && a->value.op.op == INTRINSIC_PARENTHESES)
    a = a->value.op.op1;

  for (ra = a->ref; ra != NULL && ra->type != REF_SUBSTRING; ra = ra->next)
    ;

  if (ra)
    {
      long start_a, end_a;

      if (ra->u.ss.start->expr_type == EXPR_CONSTANT
	  && ra->u.ss.end->expr_type == EXPR_CONSTANT)
	{
	  start_a = mpz_get_si (ra->u.ss.start->value.integer);
	  end_a = mpz_get_si (ra->u.ss.end->value.integer);
	  return end_a - start_a + 1;
	}
      else if (gfc_dep_compare_expr (ra->u.ss.start, ra->u.ss.end) == 0)
	return 1;
      else
	return -1;
    }

  if (a->ts.u.cl && a->ts.u.cl->length
      && a->ts.u.cl->length->expr_type == EXPR_CONSTANT)
    return mpz_get_si (a->ts.u.cl->length->value.integer);
  else if (a->expr_type == EXPR_CONSTANT
	   && (a->ts.u.cl == NULL || a->ts.u.cl->length == NULL))
    return a->value.character.length;
  else
    return -1;

}

/* Check whether two character expressions have the same length;
   returns SUCCESS if they have or if the length cannot be determined,
   otherwise return FAILURE and raise a gfc_error.  */

gfc_try
gfc_check_same_strlen (const gfc_expr *a, const gfc_expr *b, const char *name)
{
   long len_a, len_b;

   len_a = gfc_var_strlen(a);
   len_b = gfc_var_strlen(b);

   if (len_a == -1 || len_b == -1 || len_a == len_b)
     return SUCCESS;
   else
     {
       gfc_error ("Unequal character lengths (%ld/%ld) in %s at %L",
		  len_a, len_b, name, &a->where);
       return FAILURE;
     }
}


/***** Check functions *****/

/* Check subroutine suitable for intrinsics taking a real argument and
   a kind argument for the result.  */

static gfc_try
check_a_kind (gfc_expr *a, gfc_expr *kind, bt type)
{
  if (type_check (a, 0, BT_REAL) == FAILURE)
    return FAILURE;
  if (kind_check (kind, 1, type) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Check subroutine suitable for ceiling, floor and nint.  */

gfc_try
gfc_check_a_ikind (gfc_expr *a, gfc_expr *kind)
{
  return check_a_kind (a, kind, BT_INTEGER);
}


/* Check subroutine suitable for aint, anint.  */

gfc_try
gfc_check_a_xkind (gfc_expr *a, gfc_expr *kind)
{
  return check_a_kind (a, kind, BT_REAL);
}


gfc_try
gfc_check_abs (gfc_expr *a)
{
  if (numeric_check (a, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_achar (gfc_expr *a, gfc_expr *kind)
{
  if (type_check (a, 0, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind_check (kind, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_access_func (gfc_expr *name, gfc_expr *mode)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE
      || scalar_check (name, 0) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (mode, 1, BT_CHARACTER) == FAILURE
      || scalar_check (mode, 1) == FAILURE)
    return FAILURE;
  if (kind_value_check (mode, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_all_any (gfc_expr *mask, gfc_expr *dim)
{
  if (logical_array_check (mask, 0) == FAILURE)
    return FAILURE;

  if (dim_check (dim, 1, false) == FAILURE)
    return FAILURE;

  if (dim_rank_check (dim, mask, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_allocated (gfc_expr *array)
{
  if (variable_check (array, 0, false) == FAILURE)
    return FAILURE;
  if (allocatable_check (array, 0) == FAILURE)
    return FAILURE;
  
  return SUCCESS;
}


/* Common check function where the first argument must be real or
   integer and the second argument must be the same as the first.  */

gfc_try
gfc_check_a_p (gfc_expr *a, gfc_expr *p)
{
  if (int_or_real_check (a, 0) == FAILURE)
    return FAILURE;

  if (a->ts.type != p->ts.type)
    {
      gfc_error ("'%s' and '%s' arguments of '%s' intrinsic at %L must "
		 "have the same type", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &p->where);
      return FAILURE;
    }

  if (a->ts.kind != p->ts.kind)
    {
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Different type kinds at %L",
			  &p->where) == FAILURE)
       return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_x_yd (gfc_expr *x, gfc_expr *y)
{
  if (double_check (x, 0) == FAILURE || double_check (y, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_associated (gfc_expr *pointer, gfc_expr *target)
{
  symbol_attribute attr1, attr2;
  int i;
  gfc_try t;
  locus *where;

  where = &pointer->where;

  if (pointer->expr_type == EXPR_VARIABLE || pointer->expr_type == EXPR_FUNCTION)
    attr1 = gfc_expr_attr (pointer);
  else if (pointer->expr_type == EXPR_NULL)
    goto null_arg;
  else
    gcc_assert (0); /* Pointer must be a variable or a function.  */

  if (!attr1.pointer && !attr1.proc_pointer)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a POINTER",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &pointer->where);
      return FAILURE;
    }

  /* F2008, C1242.  */
  if (attr1.pointer && gfc_is_coindexed (pointer))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L shall not be "
		 "coindexed", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &pointer->where);
      return FAILURE;
    }

  /* Target argument is optional.  */
  if (target == NULL)
    return SUCCESS;

  where = &target->where;
  if (target->expr_type == EXPR_NULL)
    goto null_arg;

  if (target->expr_type == EXPR_VARIABLE || target->expr_type == EXPR_FUNCTION)
    attr2 = gfc_expr_attr (target);
  else
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a pointer "
		 "or target VARIABLE or FUNCTION",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &target->where);
      return FAILURE;
    }

  if (attr1.pointer && !attr2.pointer && !attr2.target)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a POINTER "
		 "or a TARGET", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &target->where);
      return FAILURE;
    }

  /* F2008, C1242.  */
  if (attr1.pointer && gfc_is_coindexed (target))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L shall not be "
		 "coindexed", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &target->where);
      return FAILURE;
    }

  t = SUCCESS;
  if (same_type_check (pointer, 0, target, 1) == FAILURE)
    t = FAILURE;
  if (rank_check (target, 0, pointer->rank) == FAILURE)
    t = FAILURE;
  if (target->rank > 0)
    {
      for (i = 0; i < target->rank; i++)
	if (target->ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	  {
	    gfc_error ("Array section with a vector subscript at %L shall not "
		       "be the target of a pointer",
		       &target->where);
	    t = FAILURE;
	    break;
	  }
    }
  return t;

null_arg:

  gfc_error ("NULL pointer at %L is not permitted as actual argument "
	     "of '%s' intrinsic function", where, gfc_current_intrinsic);
  return FAILURE;

}


gfc_try
gfc_check_atan_2 (gfc_expr *y, gfc_expr *x)
{
  /* gfc_notify_std would be a waste of time as the return value
     is seemingly used only for the generic resolution.  The error
     will be: Too many arguments.  */
  if ((gfc_option.allow_std & GFC_STD_F2008) == 0)
    return FAILURE;

  return gfc_check_atan2 (y, x);
}


gfc_try
gfc_check_atan2 (gfc_expr *y, gfc_expr *x)
{
  if (type_check (y, 0, BT_REAL) == FAILURE)
    return FAILURE;
  if (same_type_check (y, 0, x, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


static gfc_try
gfc_check_atomic (gfc_expr *atom, gfc_expr *value)
{
  if (!(atom->ts.type == BT_INTEGER && atom->ts.kind == gfc_atomic_int_kind)
      && !(atom->ts.type == BT_LOGICAL
	   && atom->ts.kind == gfc_atomic_logical_kind))
    {
      gfc_error ("ATOM argument at %L to intrinsic function %s shall be an "
		 "integer of ATOMIC_INT_KIND or a logical of "
		 "ATOMIC_LOGICAL_KIND", &atom->where, gfc_current_intrinsic);
      return FAILURE;
    }

  if (!gfc_expr_attr (atom).codimension)
    {
      gfc_error ("ATOM argument at %L of the %s intrinsic function shall be a "
		 "coarray or coindexed", &atom->where, gfc_current_intrinsic);
      return FAILURE;
    }

  if (atom->ts.type != value->ts.type)
    {
      gfc_error ("ATOM and VALUE argument of the %s intrinsic function shall "
		 "have the same type at %L", gfc_current_intrinsic,
		 &value->where);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_atomic_def (gfc_expr *atom, gfc_expr *value)
{
  if (scalar_check (atom, 0) == FAILURE || scalar_check (value, 1) == FAILURE)
    return FAILURE;

  if (gfc_check_vardef_context (atom, false, false, NULL) == FAILURE)
    {
      gfc_error ("ATOM argument of the %s intrinsic function at %L shall be "
		 "definable", gfc_current_intrinsic, &atom->where);
      return FAILURE;
    }

  return gfc_check_atomic (atom, value);
}


gfc_try
gfc_check_atomic_ref (gfc_expr *value, gfc_expr *atom)
{
  if (scalar_check (value, 0) == FAILURE || scalar_check (atom, 1) == FAILURE)
    return FAILURE;

  if (gfc_check_vardef_context (value, false, false, NULL) == FAILURE)
    {
      gfc_error ("VALUE argument of the %s intrinsic function at %L shall be "
		 "definable", gfc_current_intrinsic, &value->where);
      return FAILURE;
    }

  return gfc_check_atomic (atom, value);
}


/* BESJN and BESYN functions.  */

gfc_try
gfc_check_besn (gfc_expr *n, gfc_expr *x)
{
  if (type_check (n, 0, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (n->expr_type == EXPR_CONSTANT)
    {
      int i;
      gfc_extract_int (n, &i);
      if (i < 0 && gfc_notify_std (GFC_STD_GNU, "Extension: Negative argument "
				   "N at %L", &n->where) == FAILURE)
	return FAILURE;
    }

  if (type_check (x, 1, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Transformational version of the Bessel JN and YN functions.  */

gfc_try
gfc_check_bessel_n2 (gfc_expr *n1, gfc_expr *n2, gfc_expr *x)
{
  if (type_check (n1, 0, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (scalar_check (n1, 0) == FAILURE)
    return FAILURE;
  if (nonnegative_check("N1", n1) == FAILURE)
    return FAILURE;

  if (type_check (n2, 1, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (scalar_check (n2, 1) == FAILURE)
    return FAILURE;
  if (nonnegative_check("N2", n2) == FAILURE)
    return FAILURE;

  if (type_check (x, 2, BT_REAL) == FAILURE)
    return FAILURE;
  if (scalar_check (x, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_bge_bgt_ble_blt (gfc_expr *i, gfc_expr *j)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_bitfcn (gfc_expr *i, gfc_expr *pos)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (pos, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("pos", pos) == FAILURE)
    return FAILURE;

  if (less_than_bitsize1 ("i", i, "pos", pos, false) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_char (gfc_expr *i, gfc_expr *kind)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind_check (kind, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_chdir (gfc_expr *dir)
{
  if (type_check (dir, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (dir, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_chdir_sub (gfc_expr *dir, gfc_expr *status)
{
  if (type_check (dir, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (dir, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_chmod (gfc_expr *name, gfc_expr *mode)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (mode, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (mode, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_chmod_sub (gfc_expr *name, gfc_expr *mode, gfc_expr *status)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (mode, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (mode, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_cmplx (gfc_expr *x, gfc_expr *y, gfc_expr *kind)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  if (y != NULL)
    {
      if (numeric_check (y, 1) == FAILURE)
	return FAILURE;

      if (x->ts.type == BT_COMPLEX)
	{
	  gfc_error ("'%s' argument of '%s' intrinsic at %L must not be "
		     "present if 'x' is COMPLEX",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &y->where);
	  return FAILURE;
	}

      if (y->ts.type == BT_COMPLEX)
	{
	  gfc_error ("'%s' argument of '%s' intrinsic at %L must have a type "
		     "of either REAL or INTEGER",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &y->where);
	  return FAILURE;
	}

    }

  if (kind_check (kind, 2, BT_COMPLEX) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_complex (gfc_expr *x, gfc_expr *y)
{
  if (int_or_real_check (x, 0) == FAILURE)
    return FAILURE;
  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (int_or_real_check (y, 1) == FAILURE)
    return FAILURE;
  if (scalar_check (y, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_count (gfc_expr *mask, gfc_expr *dim, gfc_expr *kind)
{
  if (logical_array_check (mask, 0) == FAILURE)
    return FAILURE;
  if (dim_check (dim, 1, false) == FAILURE)
    return FAILURE;
  if (dim_rank_check (dim, mask, 0) == FAILURE)
    return FAILURE;
  if (kind_check (kind, 2, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_cshift (gfc_expr *array, gfc_expr *shift, gfc_expr *dim)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (type_check (shift, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (dim_check (dim, 2, true) == FAILURE)
    return FAILURE;

  if (dim_rank_check (dim, array, false) == FAILURE)
    return FAILURE;

  if (array->rank == 1 || shift->rank == 0)
    {
      if (scalar_check (shift, 1) == FAILURE)
	return FAILURE;
    }
  else if (shift->rank == array->rank - 1)
    {
      int d;
      if (!dim)
	d = 1;
      else if (dim->expr_type == EXPR_CONSTANT)
	gfc_extract_int (dim, &d);
      else
	d = -1;

      if (d > 0)
	{
	  int i, j;
	  for (i = 0, j = 0; i < array->rank; i++)
	    if (i != d - 1)
	      {
		if (!identical_dimen_shape (array, i, shift, j))
		  {
		    gfc_error ("'%s' argument of '%s' intrinsic at %L has "
			       "invalid shape in dimension %d (%ld/%ld)",
			       gfc_current_intrinsic_arg[1]->name,
			       gfc_current_intrinsic, &shift->where, i + 1,
			       mpz_get_si (array->shape[i]),
			       mpz_get_si (shift->shape[j]));
		    return FAILURE;
		  }

		j += 1;
	      }
	}
    }
  else
    {
      gfc_error ("'%s' argument of intrinsic '%s' at %L of must have rank "
		 "%d or be a scalar", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &shift->where, array->rank - 1);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_ctime (gfc_expr *time)
{
  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;

  if (type_check (time, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try gfc_check_datan2 (gfc_expr *y, gfc_expr *x)
{
  if (double_check (y, 0) == FAILURE || double_check (x, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

gfc_try
gfc_check_dcmplx (gfc_expr *x, gfc_expr *y)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  if (y != NULL)
    {
      if (numeric_check (y, 1) == FAILURE)
	return FAILURE;

      if (x->ts.type == BT_COMPLEX)
	{
	  gfc_error ("'%s' argument of '%s' intrinsic at %L must not be "
		     "present if 'x' is COMPLEX",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &y->where);
	  return FAILURE;
	}

      if (y->ts.type == BT_COMPLEX)
	{
	  gfc_error ("'%s' argument of '%s' intrinsic at %L must have a type "
		     "of either REAL or INTEGER",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &y->where);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


gfc_try
gfc_check_dble (gfc_expr *x)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_digits (gfc_expr *x)
{
  if (int_or_real_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_dot_product (gfc_expr *vector_a, gfc_expr *vector_b)
{
  switch (vector_a->ts.type)
    {
    case BT_LOGICAL:
      if (type_check (vector_b, 1, BT_LOGICAL) == FAILURE)
	return FAILURE;
      break;

    case BT_INTEGER:
    case BT_REAL:
    case BT_COMPLEX:
      if (numeric_check (vector_b, 1) == FAILURE)
	return FAILURE;
      break;

    default:
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &vector_a->where);
      return FAILURE;
    }

  if (rank_check (vector_a, 0, 1) == FAILURE)
    return FAILURE;

  if (rank_check (vector_b, 1, 1) == FAILURE)
    return FAILURE;

  if (! identical_dimen_shape (vector_a, 0, vector_b, 0))
    {
      gfc_error ("Different shape for arguments '%s' and '%s' at %L for "
		 "intrinsic 'dot_product'", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic_arg[1]->name, &vector_a->where);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_dprod (gfc_expr *x, gfc_expr *y)
{
  if (type_check (x, 0, BT_REAL) == FAILURE
      || type_check (y, 1, BT_REAL) == FAILURE)
    return FAILURE;

  if (x->ts.kind != gfc_default_real_kind)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be default "
		 "real", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &x->where);
      return FAILURE;
    }

  if (y->ts.kind != gfc_default_real_kind)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be default "
		 "real", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &y->where);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_dshift (gfc_expr *i, gfc_expr *j, gfc_expr *shift)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (i->is_boz && j->is_boz)
    {
      gfc_error ("'I' at %L and 'J' at %L cannot both be BOZ literal "
		 "constants", &i->where, &j->where);
      return FAILURE;
    }

  if (!i->is_boz && !j->is_boz && same_type_check (i, 0, j, 1) == FAILURE)
    return FAILURE;

  if (type_check (shift, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("SHIFT", shift) == FAILURE)
    return FAILURE;

  if (i->is_boz)
    {
      if (less_than_bitsize1 ("J", j, "SHIFT", shift, true) == FAILURE)
    	return FAILURE;
      i->ts.kind = j->ts.kind;
    }
  else
    {
      if (less_than_bitsize1 ("I", i, "SHIFT", shift, true) == FAILURE)
    	return FAILURE;
      j->ts.kind = i->ts.kind;
    }

  return SUCCESS;
}


gfc_try
gfc_check_eoshift (gfc_expr *array, gfc_expr *shift, gfc_expr *boundary,
		   gfc_expr *dim)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (type_check (shift, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (dim_check (dim, 3, true) == FAILURE)
    return FAILURE;

  if (dim_rank_check (dim, array, false) == FAILURE)
    return FAILURE;

  if (array->rank == 1 || shift->rank == 0)
    {
      if (scalar_check (shift, 1) == FAILURE)
	return FAILURE;
    }
  else if (shift->rank == array->rank - 1)
    {
      int d;
      if (!dim)
	d = 1;
      else if (dim->expr_type == EXPR_CONSTANT)
	gfc_extract_int (dim, &d);
      else
	d = -1;

      if (d > 0)
	{
	  int i, j;
	  for (i = 0, j = 0; i < array->rank; i++)
	    if (i != d - 1)
	      {
		if (!identical_dimen_shape (array, i, shift, j))
		  {
		    gfc_error ("'%s' argument of '%s' intrinsic at %L has "
			       "invalid shape in dimension %d (%ld/%ld)",
			       gfc_current_intrinsic_arg[1]->name,
			       gfc_current_intrinsic, &shift->where, i + 1,
			       mpz_get_si (array->shape[i]),
			       mpz_get_si (shift->shape[j]));
		    return FAILURE;
		  }

		j += 1;
	      }
	}
    }
  else
    {
      gfc_error ("'%s' argument of intrinsic '%s' at %L of must have rank "
		 "%d or be a scalar", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &shift->where, array->rank - 1);
      return FAILURE;
    }

  if (boundary != NULL)
    {
      if (same_type_check (array, 0, boundary, 2) == FAILURE)
	return FAILURE;

      if (array->rank == 1 || boundary->rank == 0)
	{
	  if (scalar_check (boundary, 2) == FAILURE)
	    return FAILURE;
	}
      else if (boundary->rank == array->rank - 1)
	{
	  if (gfc_check_conformance (shift, boundary,
				     "arguments '%s' and '%s' for "
				     "intrinsic %s",
				     gfc_current_intrinsic_arg[1]->name,
				     gfc_current_intrinsic_arg[2]->name,
				     gfc_current_intrinsic ) == FAILURE)
	    return FAILURE;
	}
      else
	{
	  gfc_error ("'%s' argument of intrinsic '%s' at %L of must have "
		     "rank %d or be a scalar",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &shift->where, array->rank - 1);
	  return FAILURE;
	}
    }

  return SUCCESS;
}

gfc_try
gfc_check_float (gfc_expr *a)
{
  if (type_check (a, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if ((a->ts.kind != gfc_default_integer_kind)
      && gfc_notify_std (GFC_STD_GNU, "GNU extension: non-default INTEGER "
			 "kind argument to %s intrinsic at %L",
			 gfc_current_intrinsic, &a->where) == FAILURE	)
    return FAILURE;

  return SUCCESS;
}

/* A single complex argument.  */

gfc_try
gfc_check_fn_c (gfc_expr *a)
{
  if (type_check (a, 0, BT_COMPLEX) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

/* A single real argument.  */

gfc_try
gfc_check_fn_r (gfc_expr *a)
{
  if (type_check (a, 0, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

/* A single double argument.  */

gfc_try
gfc_check_fn_d (gfc_expr *a)
{
  if (double_check (a, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

/* A single real or complex argument.  */

gfc_try
gfc_check_fn_rc (gfc_expr *a)
{
  if (real_or_complex_check (a, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_fn_rc2008 (gfc_expr *a)
{
  if (real_or_complex_check (a, 0) == FAILURE)
    return FAILURE;

  if (a->ts.type == BT_COMPLEX
      && gfc_notify_std (GFC_STD_F2008, "Fortran 2008: COMPLEX argument '%s' "
			 "argument of '%s' intrinsic at %L",
			 gfc_current_intrinsic_arg[0]->name,
			 gfc_current_intrinsic, &a->where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_fnum (gfc_expr *unit)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_huge (gfc_expr *x)
{
  if (int_or_real_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_hypot (gfc_expr *x, gfc_expr *y)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;
  if (same_type_check (x, 0, y, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Check that the single argument is an integer.  */

gfc_try
gfc_check_i (gfc_expr *i)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_iand (gfc_expr *i, gfc_expr *j)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (i->ts.kind != j->ts.kind)
    {
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Different type kinds at %L",
			  &i->where) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_ibits (gfc_expr *i, gfc_expr *pos, gfc_expr *len)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (pos, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (len, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("pos", pos) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("len", len) == FAILURE)
    return FAILURE;

  if (less_than_bitsize2 ("i", i, "pos", pos, "len", len) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ichar_iachar (gfc_expr *c, gfc_expr *kind)
{
  int i;

  if (type_check (c, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  if (c->expr_type == EXPR_VARIABLE || c->expr_type == EXPR_SUBSTRING)
    {
      gfc_expr *start;
      gfc_expr *end;
      gfc_ref *ref;

      /* Substring references don't have the charlength set.  */
      ref = c->ref;
      while (ref && ref->type != REF_SUBSTRING)
	ref = ref->next;

      gcc_assert (ref == NULL || ref->type == REF_SUBSTRING);

      if (!ref)
	{
	  /* Check that the argument is length one.  Non-constant lengths
	     can't be checked here, so assume they are ok.  */
	  if (c->ts.u.cl && c->ts.u.cl->length)
	    {
	      /* If we already have a length for this expression then use it.  */
	      if (c->ts.u.cl->length->expr_type != EXPR_CONSTANT)
		return SUCCESS;
	      i = mpz_get_si (c->ts.u.cl->length->value.integer);
	    }
	  else 
	    return SUCCESS;
	}
      else
	{
	  start = ref->u.ss.start;
	  end = ref->u.ss.end;

	  gcc_assert (start);
	  if (end == NULL || end->expr_type != EXPR_CONSTANT
	      || start->expr_type != EXPR_CONSTANT)
	    return SUCCESS;

	  i = mpz_get_si (end->value.integer) + 1
	    - mpz_get_si (start->value.integer);
	}
    }
  else
    return SUCCESS;

  if (i != 1)
    {
      gfc_error ("Argument of %s at %L must be of length one", 
		 gfc_current_intrinsic, &c->where);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_idnint (gfc_expr *a)
{
  if (double_check (a, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ieor (gfc_expr *i, gfc_expr *j)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (i->ts.kind != j->ts.kind)
    {
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Different type kinds at %L",
			  &i->where) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_index (gfc_expr *string, gfc_expr *substring, gfc_expr *back,
		 gfc_expr *kind)
{
  if (type_check (string, 0, BT_CHARACTER) == FAILURE
      || type_check (substring, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (back != NULL && type_check (back, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 3, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  if (string->ts.kind != substring->ts.kind)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be the same "
		 "kind as '%s'", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &substring->where,
		 gfc_current_intrinsic_arg[0]->name);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_int (gfc_expr *x, gfc_expr *kind)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_intconv (gfc_expr *x)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ior (gfc_expr *i, gfc_expr *j)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (i->ts.kind != j->ts.kind)
    {
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Different type kinds at %L",
			  &i->where) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_ishft (gfc_expr *i, gfc_expr *shift)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE
      || type_check (shift, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (less_than_bitsize1 ("I", i, NULL, shift, true) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ishftc (gfc_expr *i, gfc_expr *shift, gfc_expr *size)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE
      || type_check (shift, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (size != NULL) 
    {
      int i2, i3;

      if (type_check (size, 2, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (less_than_bitsize1 ("I", i, "SIZE", size, true) == FAILURE)
	return FAILURE;

      if (size->expr_type == EXPR_CONSTANT)
	{
	  gfc_extract_int (size, &i3);
	  if (i3 <= 0)
	    {
	      gfc_error ("SIZE at %L must be positive", &size->where);
	      return FAILURE;
	    }

	  if (shift->expr_type == EXPR_CONSTANT)
	    {
	      gfc_extract_int (shift, &i2);
	      if (i2 < 0)
		i2 = -i2;

	      if (i2 > i3)
		{
		  gfc_error ("The absolute value of SHIFT at %L must be less "
			     "than or equal to SIZE at %L", &shift->where,
			     &size->where);
		  return FAILURE;
		}
	     }
	}
    }
  else if (less_than_bitsize1 ("I", i, NULL, shift, true) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_kill (gfc_expr *pid, gfc_expr *sig)
{
  if (type_check (pid, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (sig, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_kill_sub (gfc_expr *pid, gfc_expr *sig, gfc_expr *status)
{
  if (type_check (pid, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (pid, 0) == FAILURE)
    return FAILURE;

  if (type_check (sig, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (sig, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_kind (gfc_expr *x)
{
  if (x->ts.type == BT_DERIVED)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a "
		 "non-derived type", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &x->where);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_lbound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (dim_check (dim, 1, false) == FAILURE)
    return FAILURE;

  if (dim_rank_check (dim, array, 1) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 2, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_lcobound (gfc_expr *coarray, gfc_expr *dim, gfc_expr *kind)
{
  if (gfc_option.coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use -fcoarray= to enable");
      return FAILURE;
    }

  if (coarray_check (coarray, 0) == FAILURE)
    return FAILURE;

  if (dim != NULL)
    {
      if (dim_check (dim, 1, false) == FAILURE)
        return FAILURE;

      if (dim_corank_check (dim, coarray) == FAILURE)
        return FAILURE;
    }

  if (kind_check (kind, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_len_lentrim (gfc_expr *s, gfc_expr *kind)
{
  if (type_check (s, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 1, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_lge_lgt_lle_llt (gfc_expr *a, gfc_expr *b)
{
  if (type_check (a, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (a, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (b, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (b, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_link (gfc_expr *path1, gfc_expr *path2)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path1, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path2, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_link_sub (gfc_expr *path1, gfc_expr *path2, gfc_expr *status)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path1, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path2, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_loc (gfc_expr *expr)
{
  return variable_check (expr, 0, true);
}


gfc_try
gfc_check_symlnk (gfc_expr *path1, gfc_expr *path2)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path1, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path2, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_symlnk_sub (gfc_expr *path1, gfc_expr *path2, gfc_expr *status)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path1, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path2, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_logical (gfc_expr *a, gfc_expr *kind)
{
  if (type_check (a, 0, BT_LOGICAL) == FAILURE)
    return FAILURE;
  if (kind_check (kind, 1, BT_LOGICAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Min/max family.  */

static gfc_try
min_max_args (gfc_actual_arglist *arg)
{
  if (arg == NULL || arg->next == NULL)
    {
      gfc_error ("Intrinsic '%s' at %L must have at least two arguments",
		 gfc_current_intrinsic, gfc_current_intrinsic_where);
      return FAILURE;
    }

  return SUCCESS;
}


static gfc_try
check_rest (bt type, int kind, gfc_actual_arglist *arglist)
{
  gfc_actual_arglist *arg, *tmp;

  gfc_expr *x;
  int m, n;

  if (min_max_args (arglist) == FAILURE)
    return FAILURE;

  for (arg = arglist, n=1; arg; arg = arg->next, n++)
    {
      x = arg->expr;
      if (x->ts.type != type || x->ts.kind != kind)
	{
	  if (x->ts.type == type)
	    {
	      if (gfc_notify_std (GFC_STD_GNU, "Extension: Different type "
				  "kinds at %L", &x->where) == FAILURE)
		return FAILURE;
	    }
	  else
	    {
	      gfc_error ("'a%d' argument of '%s' intrinsic at %L must be "
			 "%s(%d)", n, gfc_current_intrinsic, &x->where,
			 gfc_basic_typename (type), kind);
	      return FAILURE;
	    }
	}

      for (tmp = arglist, m=1; tmp != arg; tmp = tmp->next, m++)
	if (gfc_check_conformance (tmp->expr, x,
				   "arguments 'a%d' and 'a%d' for "
				   "intrinsic '%s'", m, n,
				   gfc_current_intrinsic) == FAILURE)
	    return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_min_max (gfc_actual_arglist *arg)
{
  gfc_expr *x;

  if (min_max_args (arg) == FAILURE)
    return FAILURE;

  x = arg->expr;

  if (x->ts.type == BT_CHARACTER)
    {
      if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			  "with CHARACTER argument at %L",
			  gfc_current_intrinsic, &x->where) == FAILURE)
	return FAILURE;
    }
  else if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL)
    {
      gfc_error ("'a1' argument of '%s' intrinsic at %L must be INTEGER, "
		 "REAL or CHARACTER", gfc_current_intrinsic, &x->where);
      return FAILURE;
    }

  return check_rest (x->ts.type, x->ts.kind, arg);
}


gfc_try
gfc_check_min_max_integer (gfc_actual_arglist *arg)
{
  return check_rest (BT_INTEGER, gfc_default_integer_kind, arg);
}


gfc_try
gfc_check_min_max_real (gfc_actual_arglist *arg)
{
  return check_rest (BT_REAL, gfc_default_real_kind, arg);
}


gfc_try
gfc_check_min_max_double (gfc_actual_arglist *arg)
{
  return check_rest (BT_REAL, gfc_default_double_kind, arg);
}


/* End of min/max family.  */

gfc_try
gfc_check_malloc (gfc_expr *size)
{
  if (type_check (size, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (size, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_matmul (gfc_expr *matrix_a, gfc_expr *matrix_b)
{
  if ((matrix_a->ts.type != BT_LOGICAL) && !gfc_numeric_ts (&matrix_a->ts))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &matrix_a->where);
      return FAILURE;
    }

  if ((matrix_b->ts.type != BT_LOGICAL) && !gfc_numeric_ts (&matrix_b->ts))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &matrix_b->where);
      return FAILURE;
    }

  if ((matrix_a->ts.type == BT_LOGICAL && gfc_numeric_ts (&matrix_b->ts))
      || (gfc_numeric_ts (&matrix_a->ts) && matrix_b->ts.type == BT_LOGICAL))
    {
      gfc_error ("Argument types of '%s' intrinsic at %L must match (%s/%s)",
		 gfc_current_intrinsic, &matrix_a->where,
		 gfc_typename(&matrix_a->ts), gfc_typename(&matrix_b->ts));
       return FAILURE;
    }

  switch (matrix_a->rank)
    {
    case 1:
      if (rank_check (matrix_b, 1, 2) == FAILURE)
	return FAILURE;
      /* Check for case matrix_a has shape(m), matrix_b has shape (m, k).  */
      if (!identical_dimen_shape (matrix_a, 0, matrix_b, 0))
	{
	  gfc_error ("Different shape on dimension 1 for arguments '%s' "
		     "and '%s' at %L for intrinsic matmul",
		     gfc_current_intrinsic_arg[0]->name,
		     gfc_current_intrinsic_arg[1]->name, &matrix_a->where);
	  return FAILURE;
	}
      break;

    case 2:
      if (matrix_b->rank != 2)
	{
	  if (rank_check (matrix_b, 1, 1) == FAILURE)
	    return FAILURE;
	}
      /* matrix_b has rank 1 or 2 here. Common check for the cases
	 - matrix_a has shape (n,m) and matrix_b has shape (m, k)
	 - matrix_a has shape (n,m) and matrix_b has shape (m).  */
      if (!identical_dimen_shape (matrix_a, 1, matrix_b, 0))
	{
	  gfc_error ("Different shape on dimension 2 for argument '%s' and "
		     "dimension 1 for argument '%s' at %L for intrinsic "
		     "matmul", gfc_current_intrinsic_arg[0]->name,
		     gfc_current_intrinsic_arg[1]->name, &matrix_a->where);
	  return FAILURE;
	}
      break;

    default:
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be of rank "
		 "1 or 2", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &matrix_a->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Whoever came up with this interface was probably on something.
   The possibilities for the occupation of the second and third
   parameters are:

	 Arg #2     Arg #3
	 NULL       NULL
	 DIM	NULL
	 MASK       NULL
	 NULL       MASK	     minloc(array, mask=m)
	 DIM	MASK

   I.e. in the case of minloc(array,mask), mask will be in the second
   position of the argument list and we'll have to fix that up.  */

gfc_try
gfc_check_minloc_maxloc (gfc_actual_arglist *ap)
{
  gfc_expr *a, *m, *d;

  a = ap->expr;
  if (int_or_real_check (a, 0) == FAILURE || array_check (a, 0) == FAILURE)
    return FAILURE;

  d = ap->next->expr;
  m = ap->next->next->expr;

  if (m == NULL && d != NULL && d->ts.type == BT_LOGICAL
      && ap->next->name == NULL)
    {
      m = d;
      d = NULL;
      ap->next->expr = NULL;
      ap->next->next->expr = m;
    }

  if (dim_check (d, 1, false) == FAILURE)
    return FAILURE;

  if (dim_rank_check (d, a, 0) == FAILURE)
    return FAILURE;

  if (m != NULL && type_check (m, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (m != NULL
      && gfc_check_conformance (a, m,
				"arguments '%s' and '%s' for intrinsic %s",
				gfc_current_intrinsic_arg[0]->name,
				gfc_current_intrinsic_arg[2]->name,
				gfc_current_intrinsic ) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Similar to minloc/maxloc, the argument list might need to be
   reordered for the MINVAL, MAXVAL, PRODUCT, and SUM intrinsics.  The
   difference is that MINLOC/MAXLOC take an additional KIND argument.
   The possibilities are:

	 Arg #2     Arg #3
	 NULL       NULL
	 DIM	NULL
	 MASK       NULL
	 NULL       MASK	     minval(array, mask=m)
	 DIM	MASK

   I.e. in the case of minval(array,mask), mask will be in the second
   position of the argument list and we'll have to fix that up.  */

static gfc_try
check_reduction (gfc_actual_arglist *ap)
{
  gfc_expr *a, *m, *d;

  a = ap->expr;
  d = ap->next->expr;
  m = ap->next->next->expr;

  if (m == NULL && d != NULL && d->ts.type == BT_LOGICAL
      && ap->next->name == NULL)
    {
      m = d;
      d = NULL;
      ap->next->expr = NULL;
      ap->next->next->expr = m;
    }

  if (dim_check (d, 1, false) == FAILURE)
    return FAILURE;

  if (dim_rank_check (d, a, 0) == FAILURE)
    return FAILURE;

  if (m != NULL && type_check (m, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (m != NULL
      && gfc_check_conformance (a, m,
				"arguments '%s' and '%s' for intrinsic %s",
				gfc_current_intrinsic_arg[0]->name,
				gfc_current_intrinsic_arg[2]->name,
				gfc_current_intrinsic) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_minval_maxval (gfc_actual_arglist *ap)
{
  if (int_or_real_check (ap->expr, 0) == FAILURE
      || array_check (ap->expr, 0) == FAILURE)
    return FAILURE;

  return check_reduction (ap);
}


gfc_try
gfc_check_product_sum (gfc_actual_arglist *ap)
{
  if (numeric_check (ap->expr, 0) == FAILURE
      || array_check (ap->expr, 0) == FAILURE)
    return FAILURE;

  return check_reduction (ap);
}


/* For IANY, IALL and IPARITY.  */

gfc_try
gfc_check_mask (gfc_expr *i, gfc_expr *kind)
{
  int k;

  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("I", i) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind)
    gfc_extract_int (kind, &k);
  else
    k = gfc_default_integer_kind;

  if (less_than_bitsizekind ("I", i, k) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_transf_bit_intrins (gfc_actual_arglist *ap)
{
  if (ap->expr->ts.type != BT_INTEGER)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be INTEGER",
                 gfc_current_intrinsic_arg[0]->name,
                 gfc_current_intrinsic, &ap->expr->where);
      return FAILURE;
    }

  if (array_check (ap->expr, 0) == FAILURE)
    return FAILURE;

  return check_reduction (ap);
}


gfc_try
gfc_check_merge (gfc_expr *tsource, gfc_expr *fsource, gfc_expr *mask)
{
  if (same_type_check (tsource, 0, fsource, 1) == FAILURE)
    return FAILURE;

  if (type_check (mask, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (tsource->ts.type == BT_CHARACTER)
    return gfc_check_same_strlen (tsource, fsource, "MERGE intrinsic");

  return SUCCESS;
}


gfc_try
gfc_check_merge_bits (gfc_expr *i, gfc_expr *j, gfc_expr *mask)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (j, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (mask, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (same_type_check (i, 0, j, 1) == FAILURE)
    return FAILURE;

  if (same_type_check (i, 0, mask, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_move_alloc (gfc_expr *from, gfc_expr *to)
{
  if (variable_check (from, 0, false) == FAILURE)
    return FAILURE;
  if (allocatable_check (from, 0) == FAILURE)
    return FAILURE;

  if (variable_check (to, 1, false) == FAILURE)
    return FAILURE;
  if (allocatable_check (to, 1) == FAILURE)
    return FAILURE;

  if (from->ts.type == BT_CLASS && to->ts.type == BT_DERIVED)
    {
      gfc_error ("The TO arguments in MOVE_ALLOC at %L must be "
		 "polymorphic if FROM is polymorphic",
		 &from->where);
      return FAILURE;
    }

  if (same_type_check (to, 1, from, 0) == FAILURE)
    return FAILURE;

  if (to->rank != from->rank)
    {
      gfc_error ("the '%s' and '%s' arguments of '%s' intrinsic at %L must "
		 "have the same rank %d/%d", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &to->where,  from->rank, to->rank);
      return FAILURE;
    }

  if (to->ts.kind != from->ts.kind)
    {
      gfc_error ("the '%s' and '%s' arguments of '%s' intrinsic at %L must "
		 "be of the same kind %d/%d",
		 gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &to->where, from->ts.kind, to->ts.kind);
      return FAILURE;
    }

  /* CLASS arguments: Make sure the vtab of from is present.  */
  if (to->ts.type == BT_CLASS)
    gfc_find_derived_vtab (from->ts.u.derived);

  return SUCCESS;
}


gfc_try
gfc_check_nearest (gfc_expr *x, gfc_expr *s)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (type_check (s, 1, BT_REAL) == FAILURE)
    return FAILURE;

  if (s->expr_type == EXPR_CONSTANT)
    {
      if (mpfr_sgn (s->value.real) == 0)
	{
	  gfc_error ("Argument 'S' of NEAREST at %L shall not be zero",
		     &s->where);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


gfc_try
gfc_check_new_line (gfc_expr *a)
{
  if (type_check (a, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_norm2 (gfc_expr *array, gfc_expr *dim)
{
  if (type_check (array, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (dim_rank_check (dim, array, false) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

gfc_try
gfc_check_null (gfc_expr *mold)
{
  symbol_attribute attr;

  if (mold == NULL)
    return SUCCESS;

  if (variable_check (mold, 0, true) == FAILURE)
    return FAILURE;

  attr = gfc_variable_attr (mold, NULL);

  if (!attr.pointer && !attr.proc_pointer && !attr.allocatable)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a POINTER, "
		 "ALLOCATABLE or procedure pointer",
		 gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &mold->where);
      return FAILURE;
    }

  if (attr.allocatable
      && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: NULL intrinsic with "
			 "allocatable MOLD at %L", &mold->where) == FAILURE)
    return FAILURE;

  /* F2008, C1242.  */
  if (gfc_is_coindexed (mold))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L shall not be "
		 "coindexed", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &mold->where);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_pack (gfc_expr *array, gfc_expr *mask, gfc_expr *vector)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (type_check (mask, 1, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (gfc_check_conformance (array, mask,
			     "arguments '%s' and '%s' for intrinsic '%s'",
			     gfc_current_intrinsic_arg[0]->name,
			     gfc_current_intrinsic_arg[1]->name,
			     gfc_current_intrinsic) == FAILURE)
    return FAILURE;

  if (vector != NULL)
    {
      mpz_t array_size, vector_size;
      bool have_array_size, have_vector_size;

      if (same_type_check (array, 0, vector, 2) == FAILURE)
	return FAILURE;

      if (rank_check (vector, 2, 1) == FAILURE)
	return FAILURE;

      /* VECTOR requires at least as many elements as MASK
         has .TRUE. values.  */
      have_array_size = gfc_array_size (array, &array_size) == SUCCESS;
      have_vector_size = gfc_array_size (vector, &vector_size) == SUCCESS;

      if (have_vector_size
	  && (mask->expr_type == EXPR_ARRAY
	      || (mask->expr_type == EXPR_CONSTANT
		  && have_array_size)))
	{
	  int mask_true_values = 0;

	  if (mask->expr_type == EXPR_ARRAY)
	    {
	      gfc_constructor *mask_ctor;
	      mask_ctor = gfc_constructor_first (mask->value.constructor);
	      while (mask_ctor)
		{
		  if (mask_ctor->expr->expr_type != EXPR_CONSTANT)
		    {
		      mask_true_values = 0;
		      break;
		    }

		  if (mask_ctor->expr->value.logical)
		    mask_true_values++;

		  mask_ctor = gfc_constructor_next (mask_ctor);
		}
	    }
	  else if (mask->expr_type == EXPR_CONSTANT && mask->value.logical)
	    mask_true_values = mpz_get_si (array_size);

	  if (mpz_get_si (vector_size) < mask_true_values)
	    {
	      gfc_error ("'%s' argument of '%s' intrinsic at %L must "
			 "provide at least as many elements as there "
			 "are .TRUE. values in '%s' (%ld/%d)",
			 gfc_current_intrinsic_arg[2]->name,
			 gfc_current_intrinsic, &vector->where,
			 gfc_current_intrinsic_arg[1]->name,
			 mpz_get_si (vector_size), mask_true_values);
	      return FAILURE;
	    }
	}

      if (have_array_size)
	mpz_clear (array_size);
      if (have_vector_size)
	mpz_clear (vector_size);
    }

  return SUCCESS;
}


gfc_try
gfc_check_parity (gfc_expr *mask, gfc_expr *dim)
{
  if (type_check (mask, 0, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (array_check (mask, 0) == FAILURE)
    return FAILURE;

  if (dim_rank_check (dim, mask, false) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_precision (gfc_expr *x)
{
  if (real_or_complex_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_present (gfc_expr *a)
{
  gfc_symbol *sym;

  if (variable_check (a, 0, true) == FAILURE)
    return FAILURE;

  sym = a->symtree->n.sym;
  if (!sym->attr.dummy)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be of a "
		 "dummy variable", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &a->where);
      return FAILURE;
    }

  if (!sym->attr.optional)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be of "
		 "an OPTIONAL dummy variable",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &a->where);
      return FAILURE;
    }

  /* 13.14.82  PRESENT(A)
     ......
     Argument.  A shall be the name of an optional dummy argument that is
     accessible in the subprogram in which the PRESENT function reference
     appears...  */

  if (a->ref != NULL
      && !(a->ref->next == NULL && a->ref->type == REF_ARRAY
	   && (a->ref->u.ar.type == AR_FULL
	       || (a->ref->u.ar.type == AR_ELEMENT
		   && a->ref->u.ar.as->rank == 0))))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must not be a "
		 "subobject of '%s'", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &a->where, sym->name);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_radix (gfc_expr *x)
{
  if (int_or_real_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_range (gfc_expr *x)
{
  if (numeric_check (x, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_rank (gfc_expr *a ATTRIBUTE_UNUSED)
{
  /* Any data object is allowed; a "data object" is a "constant (4.1.3),
     variable (6), or subobject of a constant (2.4.3.2.3)" (F2008, 1.3.45).  */

  bool is_variable = true;

  /* Functions returning pointers are regarded as variable, cf. F2008, R602. */
  if (a->expr_type == EXPR_FUNCTION) 
    is_variable = a->value.function.esym
		  ? a->value.function.esym->result->attr.pointer
		  : a->symtree->n.sym->result->attr.pointer;

  if (a->expr_type == EXPR_OP || a->expr_type == EXPR_NULL
      || a->expr_type == EXPR_COMPCALL|| a->expr_type == EXPR_PPC
      || !is_variable)
    {
      gfc_error ("The argument of the RANK intrinsic at %L must be a data "
		 "object", &a->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* real, float, sngl.  */
gfc_try
gfc_check_real (gfc_expr *a, gfc_expr *kind)
{
  if (numeric_check (a, 0) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 1, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_rename (gfc_expr *path1, gfc_expr *path2)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path1, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path2, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_rename_sub (gfc_expr *path1, gfc_expr *path2, gfc_expr *status)
{
  if (type_check (path1, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path1, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (path2, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (path2, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_repeat (gfc_expr *x, gfc_expr *y)
{
  if (type_check (x, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (y, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (y, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_reshape (gfc_expr *source, gfc_expr *shape,
		   gfc_expr *pad, gfc_expr *order)
{
  mpz_t size;
  mpz_t nelems;
  int shape_size;

  if (array_check (source, 0) == FAILURE)
    return FAILURE;

  if (rank_check (shape, 1, 1) == FAILURE)
    return FAILURE;

  if (type_check (shape, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (gfc_array_size (shape, &size) != SUCCESS)
    {
      gfc_error ("'shape' argument of 'reshape' intrinsic at %L must be an "
		 "array of constant size", &shape->where);
      return FAILURE;
    }

  shape_size = mpz_get_ui (size);
  mpz_clear (size);

  if (shape_size <= 0)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L is empty",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &shape->where);
      return FAILURE;
    }
  else if (shape_size > GFC_MAX_DIMENSIONS)
    {
      gfc_error ("'shape' argument of 'reshape' intrinsic at %L has more "
		 "than %d elements", &shape->where, GFC_MAX_DIMENSIONS);
      return FAILURE;
    }
  else if (shape->expr_type == EXPR_ARRAY)
    {
      gfc_expr *e;
      int i, extent;
      for (i = 0; i < shape_size; ++i)
	{
	  e = gfc_constructor_lookup_expr (shape->value.constructor, i);
	  if (e->expr_type != EXPR_CONSTANT)
	    continue;

	  gfc_extract_int (e, &extent);
	  if (extent < 0)
	    {
	      gfc_error ("'%s' argument of '%s' intrinsic at %L has "
			 "negative element (%d)",
			 gfc_current_intrinsic_arg[1]->name,
			 gfc_current_intrinsic, &e->where, extent);
	      return FAILURE;
	    }
	}
    }

  if (pad != NULL)
    {
      if (same_type_check (source, 0, pad, 2) == FAILURE)
	return FAILURE;

      if (array_check (pad, 2) == FAILURE)
	return FAILURE;
    }

  if (order != NULL)
    {
      if (array_check (order, 3) == FAILURE)
	return FAILURE;

      if (type_check (order, 3, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (order->expr_type == EXPR_ARRAY)
	{
	  int i, order_size, dim, perm[GFC_MAX_DIMENSIONS];
	  gfc_expr *e;

	  for (i = 0; i < GFC_MAX_DIMENSIONS; ++i)
	    perm[i] = 0;

	  gfc_array_size (order, &size);
	  order_size = mpz_get_ui (size);
	  mpz_clear (size);

	  if (order_size != shape_size)
	    {
	      gfc_error ("'%s' argument of '%s' intrinsic at %L "
			 "has wrong number of elements (%d/%d)", 
			 gfc_current_intrinsic_arg[3]->name,
			 gfc_current_intrinsic, &order->where,
			 order_size, shape_size);
	      return FAILURE;
	    }

	  for (i = 1; i <= order_size; ++i)
	    {
	      e = gfc_constructor_lookup_expr (order->value.constructor, i-1);
	      if (e->expr_type != EXPR_CONSTANT)
		continue;

	      gfc_extract_int (e, &dim);

	      if (dim < 1 || dim > order_size)
		{
		  gfc_error ("'%s' argument of '%s' intrinsic at %L "
			     "has out-of-range dimension (%d)", 
			     gfc_current_intrinsic_arg[3]->name,
			     gfc_current_intrinsic, &e->where, dim);
		  return FAILURE;
		}

	      if (perm[dim-1] != 0)
		{
		  gfc_error ("'%s' argument of '%s' intrinsic at %L has "
			     "invalid permutation of dimensions (dimension "
			     "'%d' duplicated)",
			     gfc_current_intrinsic_arg[3]->name,
			     gfc_current_intrinsic, &e->where, dim);
		  return FAILURE;
		}

	      perm[dim-1] = 1;
	    }
	}
    }

  if (pad == NULL && shape->expr_type == EXPR_ARRAY
      && gfc_is_constant_expr (shape)
      && !(source->expr_type == EXPR_VARIABLE && source->symtree->n.sym->as
	   && source->symtree->n.sym->as->type == AS_ASSUMED_SIZE))
    {
      /* Check the match in size between source and destination.  */
      if (gfc_array_size (source, &nelems) == SUCCESS)
	{
	  gfc_constructor *c;
	  bool test;

	  
	  mpz_init_set_ui (size, 1);
	  for (c = gfc_constructor_first (shape->value.constructor);
	       c; c = gfc_constructor_next (c))
	    mpz_mul (size, size, c->expr->value.integer);

	  test = mpz_cmp (nelems, size) < 0 && mpz_cmp_ui (size, 0) > 0;
	  mpz_clear (nelems);
	  mpz_clear (size);

	  if (test)
	    {
	      gfc_error ("Without padding, there are not enough elements "
			 "in the intrinsic RESHAPE source at %L to match "
			 "the shape", &source->where);
	      return FAILURE;
	    }
	}
    }

  return SUCCESS;
}


gfc_try
gfc_check_same_type_as (gfc_expr *a, gfc_expr *b)
{

  if (a->ts.type != BT_DERIVED && a->ts.type != BT_CLASS)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L "
		 "must be of a derived type",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &a->where);
      return FAILURE;
    }

  if (!gfc_type_is_extensible (a->ts.u.derived))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L "
		 "must be of an extensible type",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &a->where);
      return FAILURE;
    }

  if (b->ts.type != BT_DERIVED && b->ts.type != BT_CLASS)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L "
		 "must be of a derived type",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &b->where);
      return FAILURE;
    }

  if (!gfc_type_is_extensible (b->ts.u.derived))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L "
		 "must be of an extensible type",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &b->where);
      return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_scale (gfc_expr *x, gfc_expr *i)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (type_check (i, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_scan (gfc_expr *x, gfc_expr *y, gfc_expr *z, gfc_expr *kind)
{
  if (type_check (x, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (type_check (y, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (z != NULL && type_check (z, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 3, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  if (same_type_check (x, 0, y, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_secnds (gfc_expr *r)
{
  if (type_check (r, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check (r, 0, 4) == FAILURE)
    return FAILURE;

  if (scalar_check (r, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_selected_char_kind (gfc_expr *name)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (scalar_check (name, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_selected_int_kind (gfc_expr *r)
{
  if (type_check (r, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (r, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_selected_real_kind (gfc_expr *p, gfc_expr *r, gfc_expr *radix)
{
  if (p == NULL && r == NULL
      && gfc_notify_std (GFC_STD_F2008, "Fortran 2008: SELECTED_REAL_KIND with"
			 " neither 'P' nor 'R' argument at %L",
			 gfc_current_intrinsic_where) == FAILURE)
    return FAILURE;

  if (p)
    {
      if (type_check (p, 0, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (scalar_check (p, 0) == FAILURE)
	return FAILURE;
    }

  if (r)
    {
      if (type_check (r, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (scalar_check (r, 1) == FAILURE)
	return FAILURE;
    }

  if (radix)
    {
      if (type_check (radix, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (scalar_check (radix, 1) == FAILURE)
	return FAILURE;

      if (gfc_notify_std (GFC_STD_F2008, "Fortran 2008: '%s' intrinsic with "
			  "RADIX argument at %L", gfc_current_intrinsic,
			  &radix->where) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_set_exponent (gfc_expr *x, gfc_expr *i)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (type_check (i, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_shape (gfc_expr *source, gfc_expr *kind)
{
  gfc_array_ref *ar;

  if (source->rank == 0 || source->expr_type != EXPR_VARIABLE)
    return SUCCESS;

  ar = gfc_find_array_ref (source);

  if (ar->as && ar->as->type == AS_ASSUMED_SIZE && ar->type == AR_FULL)
    {
      gfc_error ("'source' argument of 'shape' intrinsic at %L must not be "
		 "an assumed size array", &source->where);
      return FAILURE;
    }

  if (kind_check (kind, 1, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_shift (gfc_expr *i, gfc_expr *shift)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (shift, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("SHIFT", shift) == FAILURE)
    return FAILURE;

  if (less_than_bitsize1 ("I", i, "SHIFT", shift, true) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_sign (gfc_expr *a, gfc_expr *b)
{
  if (int_or_real_check (a, 0) == FAILURE)
    return FAILURE;

  if (same_type_check (a, 0, b, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_size (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (dim_check (dim, 1, true) == FAILURE)
    return FAILURE;

  if (dim_rank_check (dim, array, 0) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 2, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;


  return SUCCESS;
}


gfc_try
gfc_check_sizeof (gfc_expr *arg)
{
  if (arg->ts.type == BT_PROCEDURE)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L may not be a procedure",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &arg->where);
      return FAILURE;
    }
  return SUCCESS;
}


gfc_try
gfc_check_c_sizeof (gfc_expr *arg)
{
  if (gfc_verify_c_interop (&arg->ts) != SUCCESS)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be an "
		 "interoperable data entity",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &arg->where);
      return FAILURE;
    }
  return SUCCESS;
}


gfc_try
gfc_check_sleep_sub (gfc_expr *seconds)
{
  if (type_check (seconds, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (seconds, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

gfc_try
gfc_check_sngl (gfc_expr *a)
{
  if (type_check (a, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if ((a->ts.kind != gfc_default_double_kind)
      && gfc_notify_std (GFC_STD_GNU, "GNU extension: non double precision "
			 "REAL argument to %s intrinsic at %L",
			 gfc_current_intrinsic, &a->where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

gfc_try
gfc_check_spread (gfc_expr *source, gfc_expr *dim, gfc_expr *ncopies)
{
  if (source->rank >= GFC_MAX_DIMENSIONS)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be less "
		 "than rank %d", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &source->where, GFC_MAX_DIMENSIONS);

      return FAILURE;
    }

  if (dim == NULL)
    return FAILURE;

  if (dim_check (dim, 1, false) == FAILURE)
    return FAILURE;

  /* dim_rank_check() does not apply here.  */
  if (dim 
      && dim->expr_type == EXPR_CONSTANT
      && (mpz_cmp_ui (dim->value.integer, 1) < 0
	  || mpz_cmp_ui (dim->value.integer, source->rank + 1) > 0))
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L is not a valid "
		 "dimension index", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &dim->where);
      return FAILURE;
    }

  if (type_check (ncopies, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (ncopies, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Functions for checking FGETC, FPUTC, FGET and FPUT (subroutines and
   functions).  */

gfc_try
gfc_check_fgetputc_sub (gfc_expr *unit, gfc_expr *c, gfc_expr *status)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (c, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (c, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE
      || kind_value_check (status, 2, gfc_default_integer_kind) == FAILURE
      || scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_fgetputc (gfc_expr *unit, gfc_expr *c)
{
  return gfc_check_fgetputc_sub (unit, c, NULL);
}


gfc_try
gfc_check_fgetput_sub (gfc_expr *c, gfc_expr *status)
{
  if (type_check (c, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (c, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 1, BT_INTEGER) == FAILURE
      || kind_value_check (status, 1, gfc_default_integer_kind) == FAILURE
      || scalar_check (status, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_fgetput (gfc_expr *c)
{
  return gfc_check_fgetput_sub (c, NULL);
}


gfc_try
gfc_check_fseek_sub (gfc_expr *unit, gfc_expr *offset, gfc_expr *whence, gfc_expr *status)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (offset, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (offset, 1) == FAILURE)
    return FAILURE;

  if (type_check (whence, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (whence, 2) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 3, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check (status, 3, 4) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 3) == FAILURE)
    return FAILURE;

  return SUCCESS;
}



gfc_try
gfc_check_fstat (gfc_expr *unit, gfc_expr *array)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (array, 1, BT_INTEGER) == FAILURE
      || kind_value_check (unit, 0, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (array_check (array, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_fstat_sub (gfc_expr *unit, gfc_expr *array, gfc_expr *status)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (array, 1, BT_INTEGER) == FAILURE
      || kind_value_check (array, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (array_check (array, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE
      || kind_value_check (status, 2, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ftell (gfc_expr *unit)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ftell_sub (gfc_expr *unit, gfc_expr *offset)
{
  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (offset, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (offset, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_stat (gfc_expr *name, gfc_expr *array)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (array, 1, BT_INTEGER) == FAILURE
      || kind_value_check (array, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (array_check (array, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_stat_sub (gfc_expr *name, gfc_expr *array, gfc_expr *status)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (type_check (array, 1, BT_INTEGER) == FAILURE
      || kind_value_check (array, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (array_check (array, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE
      || kind_value_check (array, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_image_index (gfc_expr *coarray, gfc_expr *sub)
{
  mpz_t nelems;

  if (gfc_option.coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use -fcoarray= to enable");
      return FAILURE;
    }

  if (coarray_check (coarray, 0) == FAILURE)
    return FAILURE;

  if (sub->rank != 1)
    {
      gfc_error ("%s argument to IMAGE_INDEX must be a rank one array at %L",
                gfc_current_intrinsic_arg[1]->name, &sub->where);
      return FAILURE;
    }

  if (gfc_array_size (sub, &nelems) == SUCCESS)
    {
      int corank = gfc_get_corank (coarray);

      if (mpz_cmp_ui (nelems, corank) != 0)
	{
	  gfc_error ("The number of array elements of the SUB argument to "
		     "IMAGE_INDEX at %L shall be %d (corank) not %d",
		     &sub->where, corank, (int) mpz_get_si (nelems));
	  mpz_clear (nelems);
	  return FAILURE;
	}
      mpz_clear (nelems);
    }

  return SUCCESS;
}


gfc_try
gfc_check_this_image (gfc_expr *coarray, gfc_expr *dim)
{
  if (gfc_option.coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use -fcoarray= to enable");
      return FAILURE;
    }

  if (dim != NULL &&  coarray == NULL)
    {
      gfc_error ("DIM argument without ARRAY argument not allowed for THIS_IMAGE "
                "intrinsic at %L", &dim->where);
      return FAILURE;
    }

  if (coarray == NULL)
    return SUCCESS;

  if (coarray_check (coarray, 0) == FAILURE)
    return FAILURE;

  if (dim != NULL)
    {
      if (dim_check (dim, 1, false) == FAILURE)
       return FAILURE;

      if (dim_corank_check (dim, coarray) == FAILURE)
       return FAILURE;
    }

  return SUCCESS;
}

/* Calculate the sizes for transfer, used by gfc_check_transfer and also
   by gfc_simplify_transfer.  Return FAILURE if we cannot do so.  */

gfc_try
gfc_calculate_transfer_sizes (gfc_expr *source, gfc_expr *mold, gfc_expr *size,
			      size_t *source_size, size_t *result_size,
			      size_t *result_length_p)
{
  size_t result_elt_size;

  if (source->expr_type == EXPR_FUNCTION)
    return FAILURE;

  if (size && size->expr_type != EXPR_CONSTANT)
    return FAILURE;

  /* Calculate the size of the source.  */
  *source_size = gfc_target_expr_size (source);
  if (*source_size == 0)
    return FAILURE;

  /* Determine the size of the element.  */
  result_elt_size = gfc_element_size (mold);
  if (result_elt_size == 0)
    return FAILURE;

  if (mold->expr_type == EXPR_ARRAY || mold->rank || size)
    {
      int result_length;

      if (size)
	result_length = (size_t)mpz_get_ui (size->value.integer);
      else
	{
	  result_length = *source_size / result_elt_size;
	  if (result_length * result_elt_size < *source_size)
	    result_length += 1;
	}

      *result_size = result_length * result_elt_size;
      if (result_length_p)
	*result_length_p = result_length;
    }
  else
    *result_size = result_elt_size;

  return SUCCESS;
}


gfc_try
gfc_check_transfer (gfc_expr *source, gfc_expr *mold, gfc_expr *size)
{
  size_t source_size;
  size_t result_size;

  if (mold->ts.type == BT_HOLLERITH)
    {
      gfc_error ("'MOLD' argument of 'TRANSFER' intrinsic at %L must not be %s",
		 &mold->where, gfc_basic_typename (BT_HOLLERITH));
      return FAILURE;
    }

  if (size != NULL)
    {
      if (type_check (size, 2, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (scalar_check (size, 2) == FAILURE)
	return FAILURE;

      if (nonoptional_check (size, 2) == FAILURE)
	return FAILURE;
    }

  if (!gfc_option.warn_surprising)
    return SUCCESS;

  /* If we can't calculate the sizes, we cannot check any more.
     Return SUCCESS for that case.  */

  if (gfc_calculate_transfer_sizes (source, mold, size, &source_size,
				    &result_size, NULL) == FAILURE)
    return SUCCESS;

  if (source_size < result_size)
    gfc_warning("Intrinsic TRANSFER at %L has partly undefined result: "
		"source size %ld < result size %ld", &source->where,
		(long) source_size, (long) result_size);

  return SUCCESS;
}


gfc_try
gfc_check_transpose (gfc_expr *matrix)
{
  if (rank_check (matrix, 0, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ubound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  if (array_check (array, 0) == FAILURE)
    return FAILURE;

  if (dim_check (dim, 1, false) == FAILURE)
    return FAILURE;

  if (dim_rank_check (dim, array, 0) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 2, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ucobound (gfc_expr *coarray, gfc_expr *dim, gfc_expr *kind)
{
  if (gfc_option.coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use -fcoarray= to enable");
      return FAILURE;
    }

  if (coarray_check (coarray, 0) == FAILURE)
    return FAILURE;

  if (dim != NULL)
    {
      if (dim_check (dim, 1, false) == FAILURE)
        return FAILURE;

      if (dim_corank_check (dim, coarray) == FAILURE)
        return FAILURE;
    }

  if (kind_check (kind, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_unpack (gfc_expr *vector, gfc_expr *mask, gfc_expr *field)
{
  mpz_t vector_size;

  if (rank_check (vector, 0, 1) == FAILURE)
    return FAILURE;

  if (array_check (mask, 1) == FAILURE)
    return FAILURE;

  if (type_check (mask, 1, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (same_type_check (vector, 0, field, 2) == FAILURE)
    return FAILURE;

  if (mask->expr_type == EXPR_ARRAY
      && gfc_array_size (vector, &vector_size) == SUCCESS)
    {
      int mask_true_count = 0;
      gfc_constructor *mask_ctor;
      mask_ctor = gfc_constructor_first (mask->value.constructor);
      while (mask_ctor)
	{
	  if (mask_ctor->expr->expr_type != EXPR_CONSTANT)
	    {
	      mask_true_count = 0;
	      break;
	    }

	  if (mask_ctor->expr->value.logical)
	    mask_true_count++;

	  mask_ctor = gfc_constructor_next (mask_ctor);
	}

      if (mpz_get_si (vector_size) < mask_true_count)
	{
	  gfc_error ("'%s' argument of '%s' intrinsic at %L must "
		     "provide at least as many elements as there "
		     "are .TRUE. values in '%s' (%ld/%d)",
		     gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		     &vector->where, gfc_current_intrinsic_arg[1]->name,
		     mpz_get_si (vector_size), mask_true_count);
	  return FAILURE;
	}

      mpz_clear (vector_size);
    }

  if (mask->rank != field->rank && field->rank != 0)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must have "
		 "the same rank as '%s' or be a scalar", 
		 gfc_current_intrinsic_arg[2]->name, gfc_current_intrinsic,
		 &field->where, gfc_current_intrinsic_arg[1]->name);
      return FAILURE;
    }

  if (mask->rank == field->rank)
    {
      int i;
      for (i = 0; i < field->rank; i++)
	if (! identical_dimen_shape (mask, i, field, i))
	{
	  gfc_error ("'%s' and '%s' arguments of '%s' intrinsic at %L "
		     "must have identical shape.", 
		     gfc_current_intrinsic_arg[2]->name,
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &field->where);
	}
    }

  return SUCCESS;
}


gfc_try
gfc_check_verify (gfc_expr *x, gfc_expr *y, gfc_expr *z, gfc_expr *kind)
{
  if (type_check (x, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (same_type_check (x, 0, y, 1) == FAILURE)
    return FAILURE;

  if (z != NULL && type_check (z, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (kind_check (kind, 3, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind && gfc_notify_std (GFC_STD_F2003, "Fortran 2003: '%s' intrinsic "
			      "with KIND argument at %L",
			      gfc_current_intrinsic, &kind->where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_trim (gfc_expr *x)
{
  if (type_check (x, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;

  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

   return SUCCESS;
}


gfc_try
gfc_check_ttynam (gfc_expr *unit)
{
  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Common check function for the half a dozen intrinsics that have a
   single real argument.  */

gfc_try
gfc_check_x (gfc_expr *x)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/************* Check functions for intrinsic subroutines *************/

gfc_try
gfc_check_cpu_time (gfc_expr *time)
{
  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;

  if (type_check (time, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (variable_check (time, 0, false) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_date_and_time (gfc_expr *date, gfc_expr *time,
			 gfc_expr *zone, gfc_expr *values)
{
  if (date != NULL)
    {
      if (type_check (date, 0, BT_CHARACTER) == FAILURE)
	return FAILURE;
      if (kind_value_check (date, 0, gfc_default_character_kind) == FAILURE)
	return FAILURE;
      if (scalar_check (date, 0) == FAILURE)
	return FAILURE;
      if (variable_check (date, 0, false) == FAILURE)
	return FAILURE;
    }

  if (time != NULL)
    {
      if (type_check (time, 1, BT_CHARACTER) == FAILURE)
	return FAILURE;
      if (kind_value_check (time, 1, gfc_default_character_kind) == FAILURE)
	return FAILURE;
      if (scalar_check (time, 1) == FAILURE)
	return FAILURE;
      if (variable_check (time, 1, false) == FAILURE)
	return FAILURE;
    }

  if (zone != NULL)
    {
      if (type_check (zone, 2, BT_CHARACTER) == FAILURE)
	return FAILURE;
      if (kind_value_check (zone, 2, gfc_default_character_kind) == FAILURE)
	return FAILURE;
      if (scalar_check (zone, 2) == FAILURE)
	return FAILURE;
      if (variable_check (zone, 2, false) == FAILURE)
	return FAILURE;
    }

  if (values != NULL)
    {
      if (type_check (values, 3, BT_INTEGER) == FAILURE)
	return FAILURE;
      if (array_check (values, 3) == FAILURE)
	return FAILURE;
      if (rank_check (values, 3, 1) == FAILURE)
	return FAILURE;
      if (variable_check (values, 3, false) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_mvbits (gfc_expr *from, gfc_expr *frompos, gfc_expr *len,
		  gfc_expr *to, gfc_expr *topos)
{
  if (type_check (from, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (frompos, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (len, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (same_type_check (from, 0, to, 3) == FAILURE)
    return FAILURE;

  if (variable_check (to, 3, false) == FAILURE)
    return FAILURE;

  if (type_check (topos, 4, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("frompos", frompos) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("topos", topos) == FAILURE)
    return FAILURE;

  if (nonnegative_check ("len", len) == FAILURE)
    return FAILURE;

  if (less_than_bitsize2 ("from", from, "frompos", frompos, "len", len)
      == FAILURE)
    return FAILURE;

  if (less_than_bitsize2 ("to", to, "topos", topos, "len", len) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_random_number (gfc_expr *harvest)
{
  if (type_check (harvest, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (variable_check (harvest, 0, false) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_random_seed (gfc_expr *size, gfc_expr *put, gfc_expr *get)
{
  unsigned int nargs = 0, kiss_size;
  locus *where = NULL;
  mpz_t put_size, get_size;
  bool have_gfc_real_16; /* Try and mimic HAVE_GFC_REAL_16 in libgfortran.  */

  have_gfc_real_16 = gfc_validate_kind (BT_REAL, 16, true) != -1;

  /* Keep the number of bytes in sync with kiss_size in
     libgfortran/intrinsics/random.c.  */
  kiss_size = (have_gfc_real_16 ? 48 : 32) / gfc_default_integer_kind;

  if (size != NULL)
    {
      if (size->expr_type != EXPR_VARIABLE
	  || !size->symtree->n.sym->attr.optional)
	nargs++;

      if (scalar_check (size, 0) == FAILURE)
	return FAILURE;

      if (type_check (size, 0, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (variable_check (size, 0, false) == FAILURE)
	return FAILURE;

      if (kind_value_check (size, 0, gfc_default_integer_kind) == FAILURE)
	return FAILURE;
    }

  if (put != NULL)
    {
      if (put->expr_type != EXPR_VARIABLE
	  || !put->symtree->n.sym->attr.optional)
	{
	  nargs++;
	  where = &put->where;
	}

      if (array_check (put, 1) == FAILURE)
	return FAILURE;

      if (rank_check (put, 1, 1) == FAILURE)
	return FAILURE;

      if (type_check (put, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (kind_value_check (put, 1, gfc_default_integer_kind) == FAILURE)
	return FAILURE;

      if (gfc_array_size (put, &put_size) == SUCCESS
	  && mpz_get_ui (put_size) < kiss_size)
	gfc_error ("Size of '%s' argument of '%s' intrinsic at %L "
		   "too small (%i/%i)",
		   gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		   where, (int) mpz_get_ui (put_size), kiss_size);
    }

  if (get != NULL)
    {
      if (get->expr_type != EXPR_VARIABLE
	  || !get->symtree->n.sym->attr.optional)
	{
	  nargs++;
	  where = &get->where;
	}

      if (array_check (get, 2) == FAILURE)
	return FAILURE;

      if (rank_check (get, 2, 1) == FAILURE)
	return FAILURE;

      if (type_check (get, 2, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (variable_check (get, 2, false) == FAILURE)
	return FAILURE;

      if (kind_value_check (get, 2, gfc_default_integer_kind) == FAILURE)
	return FAILURE;

       if (gfc_array_size (get, &get_size) == SUCCESS
 	  && mpz_get_ui (get_size) < kiss_size)
	gfc_error ("Size of '%s' argument of '%s' intrinsic at %L "
		   "too small (%i/%i)",
		   gfc_current_intrinsic_arg[2]->name, gfc_current_intrinsic,
		   where, (int) mpz_get_ui (get_size), kiss_size);
    }

  /* RANDOM_SEED may not have more than one non-optional argument.  */
  if (nargs > 1)
    gfc_error ("Too many arguments to %s at %L", gfc_current_intrinsic, where);

  return SUCCESS;
}


gfc_try
gfc_check_second_sub (gfc_expr *time)
{
  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;

  if (type_check (time, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check(time, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* The arguments of SYSTEM_CLOCK are scalar, integer variables.  Note,
   count, count_rate, and count_max are all optional arguments */

gfc_try
gfc_check_system_clock (gfc_expr *count, gfc_expr *count_rate,
			gfc_expr *count_max)
{
  if (count != NULL)
    {
      if (scalar_check (count, 0) == FAILURE)
	return FAILURE;

      if (type_check (count, 0, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (variable_check (count, 0, false) == FAILURE)
	return FAILURE;
    }

  if (count_rate != NULL)
    {
      if (scalar_check (count_rate, 1) == FAILURE)
	return FAILURE;

      if (type_check (count_rate, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (variable_check (count_rate, 1, false) == FAILURE)
	return FAILURE;

      if (count != NULL
	  && same_type_check (count, 0, count_rate, 1) == FAILURE)
	return FAILURE;

    }

  if (count_max != NULL)
    {
      if (scalar_check (count_max, 2) == FAILURE)
	return FAILURE;

      if (type_check (count_max, 2, BT_INTEGER) == FAILURE)
	return FAILURE;

      if (variable_check (count_max, 2, false) == FAILURE)
	return FAILURE;

      if (count != NULL
	  && same_type_check (count, 0, count_max, 2) == FAILURE)
	return FAILURE;

      if (count_rate != NULL
	  && same_type_check (count_rate, 1, count_max, 2) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


gfc_try
gfc_check_irand (gfc_expr *x)
{
  if (x == NULL)
    return SUCCESS;

  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(x, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_alarm_sub (gfc_expr *seconds, gfc_expr *handler, gfc_expr *status)
{
  if (scalar_check (seconds, 0) == FAILURE)
    return FAILURE;
  if (type_check (seconds, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (int_or_proc_check (handler, 1) == FAILURE)
    return FAILURE;
  if (handler->ts.type == BT_INTEGER && scalar_check (handler, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;
  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (kind_value_check (status, 2, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_rand (gfc_expr *x)
{
  if (x == NULL)
    return SUCCESS;

  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(x, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_srand (gfc_expr *x)
{
  if (scalar_check (x, 0) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(x, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ctime_sub (gfc_expr *time, gfc_expr *result)
{
  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;
  if (type_check (time, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (result, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (result, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_dtime_etime (gfc_expr *x)
{
  if (array_check (x, 0) == FAILURE)
    return FAILURE;

  if (rank_check (x, 0, 1) == FAILURE)
    return FAILURE;

  if (variable_check (x, 0, false) == FAILURE)
    return FAILURE;

  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check(x, 0, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_dtime_etime_sub (gfc_expr *values, gfc_expr *time)
{
  if (array_check (values, 0) == FAILURE)
    return FAILURE;

  if (rank_check (values, 0, 1) == FAILURE)
    return FAILURE;

  if (variable_check (values, 0, false) == FAILURE)
    return FAILURE;

  if (type_check (values, 0, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check(values, 0, 4) == FAILURE)
    return FAILURE;

  if (scalar_check (time, 1) == FAILURE)
    return FAILURE;

  if (type_check (time, 1, BT_REAL) == FAILURE)
    return FAILURE;

  if (kind_value_check(time, 1, 4) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_fdate_sub (gfc_expr *date)
{
  if (type_check (date, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (date, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_gerror (gfc_expr *msg)
{
  if (type_check (msg, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (msg, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_getcwd_sub (gfc_expr *cwd, gfc_expr *status)
{
  if (type_check (cwd, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (cwd, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_getarg (gfc_expr *pos, gfc_expr *value)
{
  if (type_check (pos, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (pos->ts.kind > gfc_default_integer_kind)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be of a kind "
		 "not wider than the default kind (%d)",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &pos->where, gfc_default_integer_kind);
      return FAILURE;
    }

  if (type_check (value, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (value, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_getlog (gfc_expr *msg)
{
  if (type_check (msg, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (msg, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_exit (gfc_expr *status)
{
  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_flush (gfc_expr *unit)
{
  if (unit == NULL)
    return SUCCESS;

  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_free (gfc_expr *i)
{
  if (type_check (i, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (i, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_hostnm (gfc_expr *name)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_hostnm_sub (gfc_expr *name, gfc_expr *status)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_itime_idate (gfc_expr *values)
{
  if (array_check (values, 0) == FAILURE)
    return FAILURE;

  if (rank_check (values, 0, 1) == FAILURE)
    return FAILURE;

  if (variable_check (values, 0, false) == FAILURE)
    return FAILURE;

  if (type_check (values, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(values, 0, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ltime_gmtime (gfc_expr *time, gfc_expr *values)
{
  if (type_check (time, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(time, 0, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  if (scalar_check (time, 0) == FAILURE)
    return FAILURE;

  if (array_check (values, 1) == FAILURE)
    return FAILURE;

  if (rank_check (values, 1, 1) == FAILURE)
    return FAILURE;

  if (variable_check (values, 1, false) == FAILURE)
    return FAILURE;

  if (type_check (values, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check(values, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_ttynam_sub (gfc_expr *unit, gfc_expr *name)
{
  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (type_check (name, 1, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 1, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_isatty (gfc_expr *unit)
{
  if (unit == NULL)
    return FAILURE;

  if (type_check (unit, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (unit, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_isnan (gfc_expr *x)
{
  if (type_check (x, 0, BT_REAL) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_perror (gfc_expr *string)
{
  if (type_check (string, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (string, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_umask (gfc_expr *mask)
{
  if (type_check (mask, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (mask, 0) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_umask_sub (gfc_expr *mask, gfc_expr *old)
{
  if (type_check (mask, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (mask, 0) == FAILURE)
    return FAILURE;

  if (old == NULL)
    return SUCCESS;

  if (scalar_check (old, 1) == FAILURE)
    return FAILURE;

  if (type_check (old, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_unlink (gfc_expr *name)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_unlink_sub (gfc_expr *name, gfc_expr *status)
{
  if (type_check (name, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (name, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_signal (gfc_expr *number, gfc_expr *handler)
{
  if (scalar_check (number, 0) == FAILURE)
    return FAILURE;
  if (type_check (number, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (int_or_proc_check (handler, 1) == FAILURE)
    return FAILURE;
  if (handler->ts.type == BT_INTEGER && scalar_check (handler, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_signal_sub (gfc_expr *number, gfc_expr *handler, gfc_expr *status)
{
  if (scalar_check (number, 0) == FAILURE)
    return FAILURE;
  if (type_check (number, 0, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (int_or_proc_check (handler, 1) == FAILURE)
    return FAILURE;
  if (handler->ts.type == BT_INTEGER && scalar_check (handler, 1) == FAILURE)
    return FAILURE;

  if (status == NULL)
    return SUCCESS;

  if (type_check (status, 2, BT_INTEGER) == FAILURE)
    return FAILURE;
  if (scalar_check (status, 2) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_system_sub (gfc_expr *cmd, gfc_expr *status)
{
  if (type_check (cmd, 0, BT_CHARACTER) == FAILURE)
    return FAILURE;
  if (kind_value_check (cmd, 0, gfc_default_character_kind) == FAILURE)
    return FAILURE;

  if (scalar_check (status, 1) == FAILURE)
    return FAILURE;

  if (type_check (status, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (kind_value_check (status, 1, gfc_default_integer_kind) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* This is used for the GNU intrinsics AND, OR and XOR.  */
gfc_try
gfc_check_and (gfc_expr *i, gfc_expr *j)
{
  if (i->ts.type != BT_INTEGER && i->ts.type != BT_LOGICAL)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be INTEGER "
		 "or LOGICAL", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &i->where);
      return FAILURE;
    }

  if (j->ts.type != BT_INTEGER && j->ts.type != BT_LOGICAL)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be INTEGER "
		 "or LOGICAL", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &j->where);
      return FAILURE;
    }

  if (i->ts.type != j->ts.type)
    {
      gfc_error ("'%s' and '%s' arguments of '%s' intrinsic at %L must "
		 "have the same type", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &j->where);
      return FAILURE;
    }

  if (scalar_check (i, 0) == FAILURE)
    return FAILURE;

  if (scalar_check (j, 1) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


gfc_try
gfc_check_storage_size (gfc_expr *a ATTRIBUTE_UNUSED, gfc_expr *kind)
{
  if (kind == NULL)
    return SUCCESS;

  if (type_check (kind, 1, BT_INTEGER) == FAILURE)
    return FAILURE;

  if (scalar_check (kind, 1) == FAILURE)
    return FAILURE;

  if (kind->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("'%s' argument of '%s' intrinsic at %L must be a constant",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &kind->where);
      return FAILURE;
    }

  return SUCCESS;
}
