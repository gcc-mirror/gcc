/* Dependency analysis
   Copyright (C) 2000-2018 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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

/* dependency.c -- Expression dependency analysis code.  */
/* There's probably quite a bit of duplication in this file.  We currently
   have different dependency checking functions for different types
   if dependencies.  Ideally these would probably be merged.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gfortran.h"
#include "dependency.h"
#include "constructor.h"
#include "arith.h"

/* static declarations */
/* Enums  */
enum range {LHS, RHS, MID};

/* Dependency types.  These must be in reverse order of priority.  */
enum gfc_dependency
{
  GFC_DEP_ERROR,
  GFC_DEP_EQUAL,	/* Identical Ranges.  */
  GFC_DEP_FORWARD,	/* e.g., a(1:3) = a(2:4).  */
  GFC_DEP_BACKWARD,	/* e.g. a(2:4) = a(1:3).  */
  GFC_DEP_OVERLAP,	/* May overlap in some other way.  */
  GFC_DEP_NODEP		/* Distinct ranges.  */
};

/* Macros */
#define IS_ARRAY_EXPLICIT(as) ((as->type == AS_EXPLICIT ? 1 : 0))

/* Forward declarations */

static gfc_dependency check_section_vs_section (gfc_array_ref *,
						gfc_array_ref *, int);

/* Returns 1 if the expr is an integer constant value 1, 0 if it is not or
   def if the value could not be determined.  */

int
gfc_expr_is_one (gfc_expr *expr, int def)
{
  gcc_assert (expr != NULL);

  if (expr->expr_type != EXPR_CONSTANT)
    return def;

  if (expr->ts.type != BT_INTEGER)
    return def;

  return mpz_cmp_si (expr->value.integer, 1) == 0;
}

/* Check if two array references are known to be identical.  Calls
   gfc_dep_compare_expr if necessary for comparing array indices.  */

static bool
identical_array_ref (gfc_array_ref *a1, gfc_array_ref *a2)
{
  int i;

  if (a1->type == AR_FULL && a2->type == AR_FULL)
    return true;

  if (a1->type == AR_SECTION && a2->type == AR_SECTION)
    {
      gcc_assert (a1->dimen == a2->dimen);

      for ( i = 0; i < a1->dimen; i++)
	{
	  /* TODO: Currently, we punt on an integer array as an index.  */
	  if (a1->dimen_type[i] != DIMEN_RANGE
	      || a2->dimen_type[i] != DIMEN_RANGE)
	    return false;

	  if (check_section_vs_section (a1, a2, i) != GFC_DEP_EQUAL)
	    return false;
	}
      return true;
    }

  if (a1->type == AR_ELEMENT && a2->type == AR_ELEMENT)
    {
      if (a1->dimen != a2->dimen)
	gfc_internal_error ("identical_array_ref(): inconsistent dimensions");

      for (i = 0; i < a1->dimen; i++)
	{
	  if (gfc_dep_compare_expr (a1->start[i], a2->start[i]) != 0)
	    return false;
	}
      return true;
    }
  return false;
}



/* Return true for identical variables, checking for references if
   necessary.  Calls identical_array_ref for checking array sections.  */

static bool
are_identical_variables (gfc_expr *e1, gfc_expr *e2)
{
  gfc_ref *r1, *r2;

  if (e1->symtree->n.sym->attr.dummy && e2->symtree->n.sym->attr.dummy)
    {
      /* Dummy arguments: Only check for equal names.  */
      if (e1->symtree->n.sym->name != e2->symtree->n.sym->name)
	return false;
    }
  else
    {
      /* Check for equal symbols.  */
      if (e1->symtree->n.sym != e2->symtree->n.sym)
	return false;
    }

  /* Volatile variables should never compare equal to themselves.  */

  if (e1->symtree->n.sym->attr.volatile_)
    return false;

  r1 = e1->ref;
  r2 = e2->ref;

  while (r1 != NULL || r2 != NULL)
    {

      /* Assume the variables are not equal if one has a reference and the
	 other doesn't.
	 TODO: Handle full references like comparing a(:) to a.
      */

      if (r1 == NULL || r2 == NULL)
	return false;

      if (r1->type != r2->type)
	return false;

      switch (r1->type)
	{

	case REF_ARRAY:
	  if (!identical_array_ref (&r1->u.ar,  &r2->u.ar))
	    return false;

	  break;

	case REF_COMPONENT:
	  if (r1->u.c.component != r2->u.c.component)
	    return false;
	  break;

	case REF_SUBSTRING:
	  if (gfc_dep_compare_expr (r1->u.ss.start, r2->u.ss.start) != 0)
	    return false;

	  /* If both are NULL, the end length compares equal, because we
	     are looking at the same variable. This can only happen for
	     assumed- or deferred-length character arguments.  */

	  if (r1->u.ss.end == NULL && r2->u.ss.end == NULL)
	    break;

	  if (gfc_dep_compare_expr (r1->u.ss.end, r2->u.ss.end) != 0)
	    return false;

	  break;

	default:
	  gfc_internal_error ("are_identical_variables: Bad type");
	}
      r1 = r1->next;
      r2 = r2->next;
    }
  return true;
}

/* Compare two functions for equality.  Returns 0 if e1==e2, -2 otherwise.  If
   impure_ok is false, only return 0 for pure functions.  */

int
gfc_dep_compare_functions (gfc_expr *e1, gfc_expr *e2, bool impure_ok)
{

  gfc_actual_arglist *args1;
  gfc_actual_arglist *args2;

  if (e1->expr_type != EXPR_FUNCTION || e2->expr_type != EXPR_FUNCTION)
    return -2;

  if ((e1->value.function.esym && e2->value.function.esym
       && e1->value.function.esym == e2->value.function.esym
       && (e1->value.function.esym->result->attr.pure || impure_ok))
       || (e1->value.function.isym && e2->value.function.isym
	   && e1->value.function.isym == e2->value.function.isym
	   && (e1->value.function.isym->pure || impure_ok)))
    {
      args1 = e1->value.function.actual;
      args2 = e2->value.function.actual;

      /* Compare the argument lists for equality.  */
      while (args1 && args2)
	{
	  /*  Bitwise xor, since C has no non-bitwise xor operator.  */
	  if ((args1->expr == NULL) ^ (args2->expr == NULL))
	    return -2;

	  if (args1->expr != NULL && args2->expr != NULL)
	    {
	      gfc_expr *e1, *e2;
	      e1 = args1->expr;
	      e2 = args2->expr;

	      if (gfc_dep_compare_expr (e1, e2) != 0)
		return -2;

	      /* Special case: String arguments which compare equal can have
		 different lengths, which makes them different in calls to
		 procedures.  */
	      
	      if (e1->expr_type == EXPR_CONSTANT
		  && e1->ts.type == BT_CHARACTER
		  && e2->expr_type == EXPR_CONSTANT
		  && e2->ts.type == BT_CHARACTER
		  && e1->value.character.length != e2->value.character.length)
		return -2;
	    }

	  args1 = args1->next;
	  args2 = args2->next;
	}
      return (args1 || args2) ? -2 : 0;
    }
      else
	return -2;
}

/* Helper function to look through parens, unary plus and widening
   integer conversions.  */

gfc_expr *
gfc_discard_nops (gfc_expr *e)
{
  gfc_actual_arglist *arglist;

  if (e == NULL)
    return NULL;

  while (true)
    {
      if (e->expr_type == EXPR_OP
	  && (e->value.op.op == INTRINSIC_UPLUS
	      || e->value.op.op == INTRINSIC_PARENTHESES))
	{
	  e = e->value.op.op1;
	  continue;
	}

      if (e->expr_type == EXPR_FUNCTION && e->value.function.isym
	  && e->value.function.isym->id == GFC_ISYM_CONVERSION
	  && e->ts.type == BT_INTEGER)
	{
	  arglist = e->value.function.actual;
	  if (arglist->expr->ts.type == BT_INTEGER
	      && e->ts.kind > arglist->expr->ts.kind)
	    {
	      e = arglist->expr;
	      continue;
	    }
	}
      break;
    }

  return e;
}


/* Compare two expressions.  Return values:
   * +1 if e1 > e2
   * 0 if e1 == e2
   * -1 if e1 < e2
   * -2 if the relationship could not be determined
   * -3 if e1 /= e2, but we cannot tell which one is larger.
   REAL and COMPLEX constants are only compared for equality
   or inequality; if they are unequal, -2 is returned in all cases.  */

int
gfc_dep_compare_expr (gfc_expr *e1, gfc_expr *e2)
{
  int i;

  if (e1 == NULL && e2 == NULL)
    return 0;

  e1 = gfc_discard_nops (e1);
  e2 = gfc_discard_nops (e2);

  if (e1->expr_type == EXPR_OP && e1->value.op.op == INTRINSIC_PLUS)
    {
      /* Compare X+C vs. X, for INTEGER only.  */
      if (e1->value.op.op2->expr_type == EXPR_CONSTANT
	  && e1->value.op.op2->ts.type == BT_INTEGER
	  && gfc_dep_compare_expr (e1->value.op.op1, e2) == 0)
	return mpz_sgn (e1->value.op.op2->value.integer);

      /* Compare P+Q vs. R+S.  */
      if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_PLUS)
	{
	  int l, r;

	  l = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1);
	  r = gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op2);
	  if (l == 0 && r == 0)
	    return 0;
	  if (l == 0 && r > -2)
	    return r;
	  if (l > -2 && r == 0)
	    return l;
	  if (l == 1 && r == 1)
	    return 1;
	  if (l == -1 && r == -1)
	    return -1;

	  l = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op2);
	  r = gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op1);
	  if (l == 0 && r == 0)
	    return 0;
	  if (l == 0 && r > -2)
	    return r;
	  if (l > -2 && r == 0)
	    return l;
	  if (l == 1 && r == 1)
	    return 1;
	  if (l == -1 && r == -1)
	    return -1;
	}
    }

  /* Compare X vs. X+C, for INTEGER only.  */
  if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_PLUS)
    {
      if (e2->value.op.op2->expr_type == EXPR_CONSTANT
	  && e2->value.op.op2->ts.type == BT_INTEGER
	  && gfc_dep_compare_expr (e1, e2->value.op.op1) == 0)
	return -mpz_sgn (e2->value.op.op2->value.integer);
    }

  /* Compare X-C vs. X, for INTEGER only.  */
  if (e1->expr_type == EXPR_OP && e1->value.op.op == INTRINSIC_MINUS)
    {
      if (e1->value.op.op2->expr_type == EXPR_CONSTANT
	  && e1->value.op.op2->ts.type == BT_INTEGER
	  && gfc_dep_compare_expr (e1->value.op.op1, e2) == 0)
	return -mpz_sgn (e1->value.op.op2->value.integer);

      /* Compare P-Q vs. R-S.  */
      if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_MINUS)
	{
	  int l, r;

	  l = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1);
	  r = gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op2);
	  if (l == 0 && r == 0)
	    return 0;
	  if (l > -2 && r == 0)
	    return l;
	  if (l == 0 && r > -2)
	    return -r;
	  if (l == 1 && r == -1)
	    return 1;
	  if (l == -1 && r == 1)
	    return -1;
	}
    }

  /* Compare A // B vs. C // D.  */

  if (e1->expr_type == EXPR_OP && e1->value.op.op == INTRINSIC_CONCAT
      && e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_CONCAT)
    {
      int l, r;

      l = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1);
      r = gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op2);

      if (l != 0)
	return l;

      /* Left expressions of // compare equal, but
	 watch out for 'A ' // x vs. 'A' // x.  */
      gfc_expr *e1_left = e1->value.op.op1;
      gfc_expr *e2_left = e2->value.op.op1;

      if (e1_left->expr_type == EXPR_CONSTANT
	  && e2_left->expr_type == EXPR_CONSTANT
	  && e1_left->value.character.length
	  != e2_left->value.character.length)
	return -2;
      else
	return r;
    }

  /* Compare X vs. X-C, for INTEGER only.  */
  if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_MINUS)
    {
      if (e2->value.op.op2->expr_type == EXPR_CONSTANT
	  && e2->value.op.op2->ts.type == BT_INTEGER
	  && gfc_dep_compare_expr (e1, e2->value.op.op1) == 0)
	return mpz_sgn (e2->value.op.op2->value.integer);
    }

  if (e1->expr_type != e2->expr_type)
    return -3;

  switch (e1->expr_type)
    {
    case EXPR_CONSTANT:
      /* Compare strings for equality.  */
      if (e1->ts.type == BT_CHARACTER && e2->ts.type == BT_CHARACTER)
	return gfc_compare_string (e1, e2);

      /* Compare REAL and COMPLEX constants.  Because of the
	 traps and pitfalls associated with comparing
	 a + 1.0 with a + 0.5, check for equality only.  */
      if (e2->expr_type == EXPR_CONSTANT)
	{
	  if (e1->ts.type == BT_REAL && e2->ts.type == BT_REAL)
	    {
	      if (mpfr_cmp (e1->value.real, e2->value.real) == 0)
		return 0;
	      else
		return -2;
	    }
	  else if (e1->ts.type == BT_COMPLEX && e2->ts.type == BT_COMPLEX)
	    {
	      if (mpc_cmp (e1->value.complex, e2->value.complex) == 0)
		return 0;
	      else
		return -2;
	    }
	}

      if (e1->ts.type != BT_INTEGER || e2->ts.type != BT_INTEGER)
	return -2;

      /* For INTEGER, all cases where e2 is not constant should have
	 been filtered out above.  */
      gcc_assert (e2->expr_type == EXPR_CONSTANT);

      i = mpz_cmp (e1->value.integer, e2->value.integer);
      if (i == 0)
	return 0;
      else if (i < 0)
	return -1;
      return 1;

    case EXPR_VARIABLE:
      if (are_identical_variables (e1, e2))
	return 0;
      else
	return -3;

    case EXPR_OP:
      /* Intrinsic operators are the same if their operands are the same.  */
      if (e1->value.op.op != e2->value.op.op)
	return -2;
      if (e1->value.op.op2 == 0)
	{
	  i = gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1);
	  return i == 0 ? 0 : -2;
	}
      if (gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op1) == 0
	  && gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op2) == 0)
	return 0;
      else if (e1->value.op.op == INTRINSIC_TIMES
	       && gfc_dep_compare_expr (e1->value.op.op1, e2->value.op.op2) == 0
	       && gfc_dep_compare_expr (e1->value.op.op2, e2->value.op.op1) == 0)
	/* Commutativity of multiplication; addition is handled above.  */
	return 0;

      return -2;

    case EXPR_FUNCTION:
      return gfc_dep_compare_functions (e1, e2, false);

    default:
      return -2;
    }
}


/* Return the difference between two expressions.  Integer expressions of
   the form

   X + constant, X - constant and constant + X

   are handled.  Return true on success, false on failure. result is assumed
   to be uninitialized on entry, and will be initialized on success.
*/

bool
gfc_dep_difference (gfc_expr *e1, gfc_expr *e2, mpz_t *result)
{
  gfc_expr *e1_op1, *e1_op2, *e2_op1, *e2_op2;

  if (e1 == NULL || e2 == NULL)
    return false;

  if (e1->ts.type != BT_INTEGER || e2->ts.type != BT_INTEGER)
    return false;

  e1 = gfc_discard_nops (e1);
  e2 = gfc_discard_nops (e2);

  /* Inizialize tentatively, clear if we don't return anything.  */
  mpz_init (*result);

  /* Case 1: c1 - c2 = c1 - c2, trivially.  */

  if (e1->expr_type == EXPR_CONSTANT && e2->expr_type == EXPR_CONSTANT)
    {
      mpz_sub (*result, e1->value.integer, e2->value.integer);
      return true;
    }

  if (e1->expr_type == EXPR_OP && e1->value.op.op == INTRINSIC_PLUS)
    {
      e1_op1 = gfc_discard_nops (e1->value.op.op1);
      e1_op2 = gfc_discard_nops (e1->value.op.op2);

      /* Case 2: (X + c1) - X = c1.  */
      if (e1_op2->expr_type == EXPR_CONSTANT
	  && gfc_dep_compare_expr (e1_op1, e2) == 0)
	{
	  mpz_set (*result, e1_op2->value.integer);
	  return true;
	}

      /* Case 3: (c1 + X) - X = c1.  */
      if (e1_op1->expr_type == EXPR_CONSTANT
	  && gfc_dep_compare_expr (e1_op2, e2) == 0)
	{
	  mpz_set (*result, e1_op1->value.integer);
	  return true;
	}

      if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_PLUS)
	{
	  e2_op1 = gfc_discard_nops (e2->value.op.op1);
	  e2_op2 = gfc_discard_nops (e2->value.op.op2);

	  if (e1_op2->expr_type == EXPR_CONSTANT)
	    {
	      /* Case 4: X + c1 - (X + c2) = c1 - c2.  */
	      if (e2_op2->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op1, e2_op1) == 0)
		{
		  mpz_sub (*result, e1_op2->value.integer,
			   e2_op2->value.integer);
		  return true;
		}
	      /* Case 5: X + c1 - (c2 + X) = c1 - c2.  */
	      if (e2_op1->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op1, e2_op2) == 0)
		{
		  mpz_sub (*result, e1_op2->value.integer,
			   e2_op1->value.integer);
		  return true;
		}
	    }
	  else if (e1_op1->expr_type == EXPR_CONSTANT)
	    {
	      /* Case 6: c1 + X - (X + c2) = c1 - c2.  */
	      if (e2_op2->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op2, e2_op1) == 0)
		{
		  mpz_sub (*result, e1_op1->value.integer,
			   e2_op2->value.integer);
		  return true;
		}
	      /* Case 7: c1 + X - (c2 + X) = c1 - c2.  */
	      if (e2_op1->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op2, e2_op2) == 0)
		{
		  mpz_sub (*result, e1_op1->value.integer,
			   e2_op1->value.integer);
		  return true;
		}
	    }
	}

      if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_MINUS)
	{
	  e2_op1 = gfc_discard_nops (e2->value.op.op1);
	  e2_op2 = gfc_discard_nops (e2->value.op.op2);

	  if (e1_op2->expr_type == EXPR_CONSTANT)
	    {
	      /* Case 8: X + c1 - (X - c2) = c1 + c2.  */
	      if (e2_op2->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op1, e2_op1) == 0)
		{
		  mpz_add (*result, e1_op2->value.integer,
			   e2_op2->value.integer);
		  return true;
		}
	    }
	  if (e1_op1->expr_type == EXPR_CONSTANT)
	    {
	      /* Case 9: c1 + X - (X - c2) = c1 + c2.  */
	      if (e2_op2->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op2, e2_op1) == 0)
		{
		  mpz_add (*result, e1_op1->value.integer,
			   e2_op2->value.integer);
		  return true;
		}
	    }
	}
    }

  if (e1->expr_type == EXPR_OP && e1->value.op.op == INTRINSIC_MINUS)
    {
      e1_op1 = gfc_discard_nops (e1->value.op.op1);
      e1_op2 = gfc_discard_nops (e1->value.op.op2);

      if (e1_op2->expr_type == EXPR_CONSTANT)
	{
	  /* Case 10: (X - c1) - X = -c1  */

	  if (gfc_dep_compare_expr (e1_op1, e2) == 0)
	    {
	      mpz_neg (*result, e1_op2->value.integer);
	      return true;
	    }

	  if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_PLUS)
	    {
	      e2_op1 = gfc_discard_nops (e2->value.op.op1);
	      e2_op2 = gfc_discard_nops (e2->value.op.op2);

	      /* Case 11: (X - c1) - (X + c2) = -( c1 + c2).  */
	      if (e2_op2->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op1, e2_op1) == 0)
		{
		  mpz_add (*result, e1_op2->value.integer,
			   e2_op2->value.integer);
		  mpz_neg (*result, *result);
		  return true;
		}

	      /* Case 12: X - c1 - (c2 + X) = - (c1 + c2).  */
	      if (e2_op1->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op1, e2_op2) == 0)
		{
		  mpz_add (*result, e1_op2->value.integer,
			   e2_op1->value.integer);
		  mpz_neg (*result, *result);
		  return true;
		}
	    }

	  if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_MINUS)
	    {
	      e2_op1 = gfc_discard_nops (e2->value.op.op1);
	      e2_op2 = gfc_discard_nops (e2->value.op.op2);

	      /* Case 13: (X - c1) - (X - c2) = c2 - c1.  */
	      if (e2_op2->expr_type == EXPR_CONSTANT
		  && gfc_dep_compare_expr (e1_op1, e2_op1) == 0)
		{
		  mpz_sub (*result, e2_op2->value.integer,
			   e1_op2->value.integer);
		  return true;
		}
	    }
	}
      if (e1_op1->expr_type == EXPR_CONSTANT)
	{
	  if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_MINUS)
	    {
	      e2_op1 = gfc_discard_nops (e2->value.op.op1);
	      e2_op2 = gfc_discard_nops (e2->value.op.op2);

	      /* Case 14: (c1 - X) - (c2 - X) == c1 - c2.  */
	      if (gfc_dep_compare_expr (e1_op2, e2_op2) == 0)
		{
		  mpz_sub (*result, e1_op1->value.integer,
			   e2_op1->value.integer);
		    return true;
		}
	    }

	}
    }

  if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_PLUS)
    {
      e2_op1 = gfc_discard_nops (e2->value.op.op1);
      e2_op2 = gfc_discard_nops (e2->value.op.op2);

      /* Case 15: X - (X + c2) = -c2.  */
      if (e2_op2->expr_type == EXPR_CONSTANT
	  && gfc_dep_compare_expr (e1, e2_op1) == 0)
	{
	  mpz_neg (*result, e2_op2->value.integer);
	  return true;
	}
      /* Case 16: X - (c2 + X) = -c2.  */
      if (e2_op1->expr_type == EXPR_CONSTANT
	  && gfc_dep_compare_expr (e1, e2_op2) == 0)
	{
	  mpz_neg (*result, e2_op1->value.integer);
	  return true;
	}
    }

  if (e2->expr_type == EXPR_OP && e2->value.op.op == INTRINSIC_MINUS)
    {
      e2_op1 = gfc_discard_nops (e2->value.op.op1);
      e2_op2 = gfc_discard_nops (e2->value.op.op2);

      /* Case 17: X - (X - c2) = c2.  */
      if (e2_op2->expr_type == EXPR_CONSTANT
	  && gfc_dep_compare_expr (e1, e2_op1) == 0)
	{
	  mpz_set (*result, e2_op2->value.integer);
	  return true;
	}
    }

  if (gfc_dep_compare_expr (e1, e2) == 0)
    {
      /* Case 18: X - X = 0.  */
      mpz_set_si (*result, 0);
      return true;
    }

  mpz_clear (*result);
  return false;
}

/* Returns 1 if the two ranges are the same and 0 if they are not (or if the
   results are indeterminate). 'n' is the dimension to compare.  */

static int
is_same_range (gfc_array_ref *ar1, gfc_array_ref *ar2, int n)
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
      if (i == -1 || i == 0)
	return 0;
    }
  else if (e2 && !e1)
    {
      i = gfc_expr_is_one (e2, -1);
      if (i == -1 || i == 0)
	return 0;
    }
  else if (e1 && e2)
    {
      i = gfc_dep_compare_expr (e1, e2);
      if (i != 0)
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
	return 0;

      i = gfc_dep_compare_expr (e1, e2);
      if (i != 0)
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
	return 0;

      i = gfc_dep_compare_expr (e1, e2);
      if (i != 0)
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
gfc_get_noncopying_intrinsic_argument (gfc_expr *expr)
{
  if (expr->expr_type != EXPR_FUNCTION || !expr->value.function.isym)
    return NULL;

  switch (expr->value.function.isym->id)
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


static int
gfc_is_data_pointer (gfc_expr *e)
{
  gfc_ref *ref;

  if (e->expr_type != EXPR_VARIABLE && e->expr_type != EXPR_FUNCTION)
    return 0;

  /* No subreference if it is a function  */
  gcc_assert (e->expr_type == EXPR_VARIABLE || !e->ref);

  if (e->symtree->n.sym->attr.pointer)
    return 1;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT && ref->u.c.component->attr.pointer)
      return 1;

  return 0;
}


/* Return true if array variable VAR could be passed to the same function
   as argument EXPR without interfering with EXPR.  INTENT is the intent
   of VAR.

   This is considerably less conservative than other dependencies
   because many function arguments will already be copied into a
   temporary.  */

static int
gfc_check_argument_var_dependency (gfc_expr *var, sym_intent intent,
				   gfc_expr *expr, gfc_dep_check elemental)
{
  gfc_expr *arg;

  gcc_assert (var->expr_type == EXPR_VARIABLE);
  gcc_assert (var->rank > 0);

  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
      /* In case of elemental subroutines, there is no dependency
         between two same-range array references.  */
      if (gfc_ref_needs_temporary_p (expr->ref)
	  || gfc_check_dependency (var, expr, elemental == NOT_ELEMENTAL))
	{
	  if (elemental == ELEM_DONT_CHECK_VARIABLE)
	    {
	      /* Too many false positive with pointers.  */
	      if (!gfc_is_data_pointer (var) && !gfc_is_data_pointer (expr))
		{
		  /* Elemental procedures forbid unspecified intents,
		     and we don't check dependencies for INTENT_IN args.  */
		  gcc_assert (intent == INTENT_OUT || intent == INTENT_INOUT);

		  /* We are told not to check dependencies.
		     We do it, however, and issue a warning in case we find one.
		     If a dependency is found in the case
		     elemental == ELEM_CHECK_VARIABLE, we will generate
		     a temporary, so we don't need to bother the user.  */
		  gfc_warning (0, "INTENT(%s) actual argument at %L might "
			       "interfere with actual argument at %L.",
		   	       intent == INTENT_OUT ? "OUT" : "INOUT",
		   	       &var->where, &expr->where);
		}
	      return 0;
	    }
	  else
	    return 1;
	}
      return 0;

    case EXPR_ARRAY:
      /* the scalarizer always generates a temporary for array constructors,
	 so there is no dependency.  */
      return 0;

    case EXPR_FUNCTION:
      if (intent != INTENT_IN)
	{
	  arg = gfc_get_noncopying_intrinsic_argument (expr);
	  if (arg != NULL)
	    return gfc_check_argument_var_dependency (var, intent, arg,
						      NOT_ELEMENTAL);
	}

      if (elemental != NOT_ELEMENTAL)
	{
	  if ((expr->value.function.esym
	       && expr->value.function.esym->attr.elemental)
	      || (expr->value.function.isym
		  && expr->value.function.isym->elemental))
	    return gfc_check_fncall_dependency (var, intent, NULL,
						expr->value.function.actual,
						ELEM_CHECK_VARIABLE);

	  if (gfc_inline_intrinsic_function_p (expr))
	    {
	      /* The TRANSPOSE case should have been caught in the
		 noncopying intrinsic case above.  */
	      gcc_assert (expr->value.function.isym->id != GFC_ISYM_TRANSPOSE);

	      return gfc_check_fncall_dependency (var, intent, NULL,
						  expr->value.function.actual,
						  ELEM_CHECK_VARIABLE);
	    }
	}
      return 0;

    case EXPR_OP:
      /* In case of non-elemental procedures, there is no need to catch
	 dependencies, as we will make a temporary anyway.  */
      if (elemental)
	{
	  /* If the actual arg EXPR is an expression, we need to catch
	     a dependency between variables in EXPR and VAR,
	     an intent((IN)OUT) variable.  */
	  if (expr->value.op.op1
	      && gfc_check_argument_var_dependency (var, intent,
						    expr->value.op.op1,
						    ELEM_CHECK_VARIABLE))
	    return 1;
	  else if (expr->value.op.op2
		   && gfc_check_argument_var_dependency (var, intent,
							 expr->value.op.op2,
							 ELEM_CHECK_VARIABLE))
	    return 1;
	}
      return 0;

    default:
      return 0;
    }
}


/* Like gfc_check_argument_var_dependency, but extended to any
   array expression OTHER, not just variables.  */

static int
gfc_check_argument_dependency (gfc_expr *other, sym_intent intent,
			       gfc_expr *expr, gfc_dep_check elemental)
{
  switch (other->expr_type)
    {
    case EXPR_VARIABLE:
      return gfc_check_argument_var_dependency (other, intent, expr, elemental);

    case EXPR_FUNCTION:
      other = gfc_get_noncopying_intrinsic_argument (other);
      if (other != NULL)
	return gfc_check_argument_dependency (other, INTENT_IN, expr,
					      NOT_ELEMENTAL);

      return 0;

    default:
      return 0;
    }
}


/* Like gfc_check_argument_dependency, but check all the arguments in ACTUAL.
   FNSYM is the function being called, or NULL if not known.  */

int
gfc_check_fncall_dependency (gfc_expr *other, sym_intent intent,
			     gfc_symbol *fnsym, gfc_actual_arglist *actual,
			     gfc_dep_check elemental)
{
  gfc_formal_arglist *formal;
  gfc_expr *expr;

  formal = fnsym ? gfc_sym_get_dummy_args (fnsym) : NULL;
  for (; actual; actual = actual->next, formal = formal ? formal->next : NULL)
    {
      expr = actual->expr;

      /* Skip args which are not present.  */
      if (!expr)
	continue;

      /* Skip other itself.  */
      if (expr == other)
	continue;

      /* Skip intent(in) arguments if OTHER itself is intent(in).  */
      if (formal && intent == INTENT_IN
	  && formal->sym->attr.intent == INTENT_IN)
	continue;

      if (gfc_check_argument_dependency (other, intent, expr, elemental))
	return 1;
    }

  return 0;
}


/* Return 1 if e1 and e2 are equivalenced arrays, either
   directly or indirectly; i.e., equivalence (a,b) for a and b
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
      || !e2->symtree->n.sym->attr.in_equivalence|| !e1->rank || !e2->rank)
    return 0;

  if (e1->symtree->n.sym->ns
	&& e1->symtree->n.sym->ns != gfc_current_ns)
    l = e1->symtree->n.sym->ns->equiv_lists;
  else
    l = gfc_current_ns->equiv_lists;

  /* Go through the equiv_lists and return 1 if the variables
     e1 and e2 are members of the same group and satisfy the
     requirement on their relative offsets.  */
  for (; l; l = l->next)
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


/* Return true if there is no possibility of aliasing because of a type
   mismatch between all the possible pointer references and the
   potential target.  Note that this function is asymmetric in the
   arguments and so must be called twice with the arguments exchanged.  */

static bool
check_data_pointer_types (gfc_expr *expr1, gfc_expr *expr2)
{
  gfc_component *cm1;
  gfc_symbol *sym1;
  gfc_symbol *sym2;
  gfc_ref *ref1;
  bool seen_component_ref;

  if (expr1->expr_type != EXPR_VARIABLE
	|| expr2->expr_type != EXPR_VARIABLE)
    return false;

  sym1 = expr1->symtree->n.sym;
  sym2 = expr2->symtree->n.sym;

  /* Keep it simple for now.  */
  if (sym1->ts.type == BT_DERIVED && sym2->ts.type == BT_DERIVED)
    return false;

  if (sym1->attr.pointer)
    {
      if (gfc_compare_types (&sym1->ts, &sym2->ts))
	return false;
    }

  /* This is a conservative check on the components of the derived type
     if no component references have been seen.  Since we will not dig
     into the components of derived type components, we play it safe by
     returning false.  First we check the reference chain and then, if
     no component references have been seen, the components.  */
  seen_component_ref = false;
  if (sym1->ts.type == BT_DERIVED)
    {
      for (ref1 = expr1->ref; ref1; ref1 = ref1->next)
	{
	  if (ref1->type != REF_COMPONENT)
	    continue;

	  if (ref1->u.c.component->ts.type == BT_DERIVED)
	    return false;

	  if ((sym2->attr.pointer || ref1->u.c.component->attr.pointer)
		&& gfc_compare_types (&ref1->u.c.component->ts, &sym2->ts))
	    return false;

	  seen_component_ref = true;
	}
    }

  if (sym1->ts.type == BT_DERIVED && !seen_component_ref)
    {
      for (cm1 = sym1->ts.u.derived->components; cm1; cm1 = cm1->next)
	{
	  if (cm1->ts.type == BT_DERIVED)
	    return false;

	  if ((sym2->attr.pointer || cm1->attr.pointer)
		&& gfc_compare_types (&cm1->ts, &sym2->ts))
	    return false;
	}
    }

  return true;
}


/* Return true if the statement body redefines the condition.  Returns
   true if expr2 depends on expr1.  expr1 should be a single term
   suitable for the lhs of an assignment.  The IDENTICAL flag indicates
   whether array references to the same symbol with identical range
   references count as a dependency or not.  Used for forall and where
   statements.  Also used with functions returning arrays without a
   temporary.  */

int
gfc_check_dependency (gfc_expr *expr1, gfc_expr *expr2, bool identical)
{
  gfc_actual_arglist *actual;
  gfc_constructor *c;
  int n;

  /* -fcoarray=lib can end up here with expr1->expr_type set to EXPR_FUNCTION
     and a reference to _F.caf_get, so skip the assert.  */
  if (expr1->expr_type == EXPR_FUNCTION
      && strcmp (expr1->value.function.name, "_F.caf_get") == 0)
    return 0;

  if (expr1->expr_type != EXPR_VARIABLE)
    gfc_internal_error ("gfc_check_dependency: expecting an EXPR_VARIABLE");

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
	  symbol_attribute attr1, attr2;
	  gfc_typespec *ts1 = &expr1->symtree->n.sym->ts;
	  gfc_typespec *ts2 = &expr2->symtree->n.sym->ts;

	  /* Return 1 if expr1 and expr2 are equivalenced arrays.  */
	  if (gfc_are_equivalenced_arrays (expr1, expr2))
	    return 1;

	  /* Symbols can only alias if they have the same type.  */
	  if (ts1->type != BT_UNKNOWN && ts2->type != BT_UNKNOWN
	      && ts1->type != BT_DERIVED && ts2->type != BT_DERIVED)
	    {
	      if (ts1->type != ts2->type || ts1->kind != ts2->kind)
		return 0;
	    }

	  /* We have to also include target-target as ptr%comp is not a
	     pointer but it still alias with "dt%comp" for "ptr => dt".  As
	     subcomponents and array access to pointers retains the target
	     attribute, that's sufficient.  */
	  attr1 = gfc_expr_attr (expr1);
	  attr2 = gfc_expr_attr (expr2);
	  if ((attr1.pointer || attr1.target) && (attr2.pointer || attr2.target))
	    {
	      if (check_data_pointer_types (expr1, expr2)
		    && check_data_pointer_types (expr2, expr1))
		return 0;

	      return 1;
	    }
	  else
	    {
	      gfc_symbol *sym1 = expr1->symtree->n.sym;
	      gfc_symbol *sym2 = expr2->symtree->n.sym;
	      if (sym1->attr.target && sym2->attr.target
		  && ((sym1->attr.dummy && !sym1->attr.contiguous
		       && (!sym1->attr.dimension
		           || sym2->as->type == AS_ASSUMED_SHAPE))
		      || (sym2->attr.dummy && !sym2->attr.contiguous
			  && (!sym2->attr.dimension
			      || sym2->as->type == AS_ASSUMED_SHAPE))))
		return 1;
	    }

	  /* Otherwise distinct symbols have no dependencies.  */
	  return 0;
	}

      if (identical)
	return 1;

      /* Identical and disjoint ranges return 0,
	 overlapping ranges return 1.  */
      if (expr1->ref && expr2->ref)
	return gfc_dep_resolver (expr1->ref, expr2->ref, NULL);

      return 1;

    case EXPR_FUNCTION:
      if (gfc_get_noncopying_intrinsic_argument (expr2) != NULL)
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
    case EXPR_NULL:
      return 0;

    case EXPR_ARRAY:
      /* Loop through the array constructor's elements.  */
      for (c = gfc_constructor_first (expr2->value.constructor);
	   c; c = gfc_constructor_next (c))
	{
	  /* If this is an iterator, assume the worst.  */
	  if (c->iterator)
	    return 1;
	  /* Avoid recursion in the common case.  */
	  if (c->expr->expr_type == EXPR_CONSTANT)
	    continue;
	  if (gfc_check_dependency (expr1, c->expr, 1))
	    return 1;
	}
      return 0;

    default:
      return 1;
    }
}


/* Determines overlapping for two array sections.  */

static gfc_dependency
check_section_vs_section (gfc_array_ref *l_ar, gfc_array_ref *r_ar, int n)
{
  gfc_expr *l_start;
  gfc_expr *l_end;
  gfc_expr *l_stride;
  gfc_expr *l_lower;
  gfc_expr *l_upper;
  int l_dir;

  gfc_expr *r_start;
  gfc_expr *r_end;
  gfc_expr *r_stride;
  gfc_expr *r_lower;
  gfc_expr *r_upper;
  gfc_expr *one_expr;
  int r_dir;
  int stride_comparison;
  int start_comparison;
  mpz_t tmp;

  /* If they are the same range, return without more ado.  */
  if (is_same_range (l_ar, r_ar, n))
    return GFC_DEP_EQUAL;

  l_start = l_ar->start[n];
  l_end = l_ar->end[n];
  l_stride = l_ar->stride[n];

  r_start = r_ar->start[n];
  r_end = r_ar->end[n];
  r_stride = r_ar->stride[n];

  /* If l_start is NULL take it from array specifier.  */
  if (l_start == NULL && IS_ARRAY_EXPLICIT (l_ar->as))
    l_start = l_ar->as->lower[n];
  /* If l_end is NULL take it from array specifier.  */
  if (l_end == NULL && IS_ARRAY_EXPLICIT (l_ar->as))
    l_end = l_ar->as->upper[n];

  /* If r_start is NULL take it from array specifier.  */
  if (r_start == NULL && IS_ARRAY_EXPLICIT (r_ar->as))
    r_start = r_ar->as->lower[n];
  /* If r_end is NULL take it from array specifier.  */
  if (r_end == NULL && IS_ARRAY_EXPLICIT (r_ar->as))
    r_end = r_ar->as->upper[n];

  /* Determine whether the l_stride is positive or negative.  */
  if (!l_stride)
    l_dir = 1;
  else if (l_stride->expr_type == EXPR_CONSTANT
	   && l_stride->ts.type == BT_INTEGER)
    l_dir = mpz_sgn (l_stride->value.integer);
  else if (l_start && l_end)
    l_dir = gfc_dep_compare_expr (l_end, l_start);
  else
    l_dir = -2;

  /* Determine whether the r_stride is positive or negative.  */
  if (!r_stride)
    r_dir = 1;
  else if (r_stride->expr_type == EXPR_CONSTANT
	   && r_stride->ts.type == BT_INTEGER)
    r_dir = mpz_sgn (r_stride->value.integer);
  else if (r_start && r_end)
    r_dir = gfc_dep_compare_expr (r_end, r_start);
  else
    r_dir = -2;

  /* The strides should never be zero.  */
  if (l_dir == 0 || r_dir == 0)
    return GFC_DEP_OVERLAP;

  /* Determine the relationship between the strides.  Set stride_comparison to
     -2 if the dependency cannot be determined
     -1 if l_stride < r_stride
      0 if l_stride == r_stride
      1 if l_stride > r_stride
     as determined by gfc_dep_compare_expr.  */

  one_expr = gfc_get_int_expr (gfc_index_integer_kind, NULL, 1);

  stride_comparison = gfc_dep_compare_expr (l_stride ? l_stride : one_expr,
					    r_stride ? r_stride : one_expr);

  if (l_start && r_start)
    start_comparison = gfc_dep_compare_expr (l_start, r_start);
  else
    start_comparison = -2;

  gfc_free_expr (one_expr);

  /* Determine LHS upper and lower bounds.  */
  if (l_dir == 1)
    {
      l_lower = l_start;
      l_upper = l_end;
    }
  else if (l_dir == -1)
    {
      l_lower = l_end;
      l_upper = l_start;
    }
  else
    {
      l_lower = NULL;
      l_upper = NULL;
    }

  /* Determine RHS upper and lower bounds.  */
  if (r_dir == 1)
    {
      r_lower = r_start;
      r_upper = r_end;
    }
  else if (r_dir == -1)
    {
      r_lower = r_end;
      r_upper = r_start;
    }
  else
    {
      r_lower = NULL;
      r_upper = NULL;
    }

  /* Check whether the ranges are disjoint.  */
  if (l_upper && r_lower && gfc_dep_compare_expr (l_upper, r_lower) == -1)
    return GFC_DEP_NODEP;
  if (r_upper && l_lower && gfc_dep_compare_expr (r_upper, l_lower) == -1)
    return GFC_DEP_NODEP;

  /* Handle cases like x:y:1 vs. x:z:-1 as GFC_DEP_EQUAL.  */
  if (l_start && r_start && gfc_dep_compare_expr (l_start, r_start) == 0)
    {
      if (l_dir == 1 && r_dir == -1)
	return GFC_DEP_EQUAL;
      if (l_dir == -1 && r_dir == 1)
	return GFC_DEP_EQUAL;
    }

  /* Handle cases like x:y:1 vs. z:y:-1 as GFC_DEP_EQUAL.  */
  if (l_end && r_end && gfc_dep_compare_expr (l_end, r_end) == 0)
    {
      if (l_dir == 1 && r_dir == -1)
	return GFC_DEP_EQUAL;
      if (l_dir == -1 && r_dir == 1)
	return GFC_DEP_EQUAL;
    }

  /* Handle cases like x:y:2 vs. x+1:z:4 as GFC_DEP_NODEP.
     There is no dependency if the remainder of
     (l_start - r_start) / gcd(l_stride, r_stride) is
     nonzero.
     TODO:
       - Cases like a(1:4:2) = a(2:3) are still not handled.
  */

#define IS_CONSTANT_INTEGER(a) ((a) && ((a)->expr_type == EXPR_CONSTANT) \
			      && (a)->ts.type == BT_INTEGER)

  if (IS_CONSTANT_INTEGER (l_stride) && IS_CONSTANT_INTEGER (r_stride)
      && gfc_dep_difference (l_start, r_start, &tmp))
    {
      mpz_t gcd;
      int result;

      mpz_init (gcd);
      mpz_gcd (gcd, l_stride->value.integer, r_stride->value.integer);

      mpz_fdiv_r (tmp, tmp, gcd);
      result = mpz_cmp_si (tmp, 0L);

      mpz_clear (gcd);
      mpz_clear (tmp);

      if (result != 0)
	return GFC_DEP_NODEP;
    }

#undef IS_CONSTANT_INTEGER

  /* Check for forward dependencies x:y vs. x+1:z and x:y:z vs. x:y:z+1.  */

  if (l_dir == 1 && r_dir == 1 &&
      (start_comparison == 0 || start_comparison == -1)
      && (stride_comparison == 0 || stride_comparison == -1))
	  return GFC_DEP_FORWARD;

  /* Check for forward dependencies x:y:-1 vs. x-1:z:-1 and
     x:y:-1 vs. x:y:-2.  */
  if (l_dir == -1 && r_dir == -1 &&
      (start_comparison == 0 || start_comparison == 1)
      && (stride_comparison == 0 || stride_comparison == 1))
    return GFC_DEP_FORWARD;

  if (stride_comparison == 0 || stride_comparison == -1)
    {
      if (l_start && IS_ARRAY_EXPLICIT (l_ar->as))
	{

	  /* Check for a(low:y:s) vs. a(z:x:s) or
	     a(low:y:s) vs. a(z:x:s+1) where a has a lower bound
	     of low, which is always at least a forward dependence.  */

	  if (r_dir == 1
	      && gfc_dep_compare_expr (l_start, l_ar->as->lower[n]) == 0)
	    return GFC_DEP_FORWARD;
	}
    }

  if (stride_comparison == 0 || stride_comparison == 1)
    {
      if (l_start && IS_ARRAY_EXPLICIT (l_ar->as))
	{

	  /* Check for a(high:y:-s) vs. a(z:x:-s) or
	     a(high:y:-s vs. a(z:x:-s-1) where a has a higher bound
	     of high, which is always at least a forward dependence.  */

	  if (r_dir == -1
	      && gfc_dep_compare_expr (l_start, l_ar->as->upper[n]) == 0)
	    return GFC_DEP_FORWARD;
	}
    }


  if (stride_comparison == 0)
    {
      /* From here, check for backwards dependencies.  */
      /* x+1:y vs. x:z.  */
      if (l_dir == 1 && r_dir == 1  && start_comparison == 1)
	return GFC_DEP_BACKWARD;

      /* x-1:y:-1 vs. x:z:-1.  */
      if (l_dir == -1 && r_dir == -1 && start_comparison == -1)
	return GFC_DEP_BACKWARD;
    }

  return GFC_DEP_OVERLAP;
}


/* Determines overlapping for a single element and a section.  */

static gfc_dependency
gfc_check_element_vs_section( gfc_ref *lref, gfc_ref *rref, int n)
{
  gfc_array_ref *ref;
  gfc_expr *elem;
  gfc_expr *start;
  gfc_expr *end;
  gfc_expr *stride;
  int s;

  elem = lref->u.ar.start[n];
  if (!elem)
    return GFC_DEP_OVERLAP;

  ref = &rref->u.ar;
  start = ref->start[n] ;
  end = ref->end[n] ;
  stride = ref->stride[n];

  if (!start && IS_ARRAY_EXPLICIT (ref->as))
    start = ref->as->lower[n];
  if (!end && IS_ARRAY_EXPLICIT (ref->as))
    end = ref->as->upper[n];

  /* Determine whether the stride is positive or negative.  */
  if (!stride)
    s = 1;
  else if (stride->expr_type == EXPR_CONSTANT
	   && stride->ts.type == BT_INTEGER)
    s = mpz_sgn (stride->value.integer);
  else
    s = -2;

  /* Stride should never be zero.  */
  if (s == 0)
    return GFC_DEP_OVERLAP;

  /* Positive strides.  */
  if (s == 1)
    {
      /* Check for elem < lower.  */
      if (start && gfc_dep_compare_expr (elem, start) == -1)
	return GFC_DEP_NODEP;
      /* Check for elem > upper.  */
      if (end && gfc_dep_compare_expr (elem, end) == 1)
	return GFC_DEP_NODEP;

      if (start && end)
	{
	  s = gfc_dep_compare_expr (start, end);
	  /* Check for an empty range.  */
	  if (s == 1)
	    return GFC_DEP_NODEP;
	  if (s == 0 && gfc_dep_compare_expr (elem, start) == 0)
	    return GFC_DEP_EQUAL;
	}
    }
  /* Negative strides.  */
  else if (s == -1)
    {
      /* Check for elem > upper.  */
      if (end && gfc_dep_compare_expr (elem, start) == 1)
	return GFC_DEP_NODEP;
      /* Check for elem < lower.  */
      if (start && gfc_dep_compare_expr (elem, end) == -1)
	return GFC_DEP_NODEP;

      if (start && end)
	{
	  s = gfc_dep_compare_expr (start, end);
	  /* Check for an empty range.  */
	  if (s == -1)
	    return GFC_DEP_NODEP;
	  if (s == 0 && gfc_dep_compare_expr (elem, start) == 0)
	    return GFC_DEP_EQUAL;
	}
    }
  /* Unknown strides.  */
  else
    {
      if (!start || !end)
	return GFC_DEP_OVERLAP;
      s = gfc_dep_compare_expr (start, end);
      if (s <= -2)
	return GFC_DEP_OVERLAP;
      /* Assume positive stride.  */
      if (s == -1)
	{
	  /* Check for elem < lower.  */
	  if (gfc_dep_compare_expr (elem, start) == -1)
	    return GFC_DEP_NODEP;
	  /* Check for elem > upper.  */
	  if (gfc_dep_compare_expr (elem, end) == 1)
	    return GFC_DEP_NODEP;
	}
      /* Assume negative stride.  */
      else if (s == 1)
	{
	  /* Check for elem > upper.  */
	  if (gfc_dep_compare_expr (elem, start) == 1)
	    return GFC_DEP_NODEP;
	  /* Check for elem < lower.  */
	  if (gfc_dep_compare_expr (elem, end) == -1)
	    return GFC_DEP_NODEP;
	}
      /* Equal bounds.  */
      else if (s == 0)
	{
	  s = gfc_dep_compare_expr (elem, start);
	  if (s == 0)
	    return GFC_DEP_EQUAL;
	  if (s == 1 || s == -1)
	    return GFC_DEP_NODEP;
	}
    }

  return GFC_DEP_OVERLAP;
}


/* Traverse expr, checking all EXPR_VARIABLE symbols for their
   forall_index attribute.  Return true if any variable may be
   being used as a FORALL index.  Its safe to pessimistically
   return true, and assume a dependency.  */

static bool
contains_forall_index_p (gfc_expr *expr)
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
      for (c = gfc_constructor_first (expr->value.constructor);
	   c; gfc_constructor_next (c))
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
gfc_check_element_vs_element (gfc_ref *lref, gfc_ref *rref, int n)
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
  if (contains_forall_index_p (r_start) || contains_forall_index_p (l_start))
    return GFC_DEP_OVERLAP;

  if (i > -2)
    return GFC_DEP_NODEP;
  return GFC_DEP_EQUAL;
}

/* Callback function for checking if an expression depends on a
   dummy variable which is any other than INTENT(IN).  */

static int
callback_dummy_intent_not_in (gfc_expr **ep,
			      int *walk_subtrees ATTRIBUTE_UNUSED,
			      void *data ATTRIBUTE_UNUSED)
{
  gfc_expr *e = *ep;

  if (e->expr_type == EXPR_VARIABLE && e->symtree
      && e->symtree->n.sym->attr.dummy)
    return e->symtree->n.sym->attr.intent != INTENT_IN;
  else
    return 0;
}

/* Auxiliary function to check if subexpressions have dummy variables which
   are not intent(in).
*/

static bool
dummy_intent_not_in (gfc_expr **ep)
{
  return gfc_expr_walker (ep, callback_dummy_intent_not_in, NULL);
}

/* Determine if an array ref, usually an array section specifies the
   entire array.  In addition, if the second, pointer argument is
   provided, the function will return true if the reference is
   contiguous; eg. (:, 1) gives true but (1,:) gives false. 
   If one of the bounds depends on a dummy variable which is
   not INTENT(IN), also return false, because the user may
   have changed the variable.  */

bool
gfc_full_array_ref_p (gfc_ref *ref, bool *contiguous)
{
  int i;
  int n;
  bool lbound_OK = true;
  bool ubound_OK = true;

  if (contiguous)
    *contiguous = false;

  if (ref->type != REF_ARRAY)
    return false;

  if (ref->u.ar.type == AR_FULL)
    {
      if (contiguous)
	*contiguous = true;
      return true;
    }

  if (ref->u.ar.type != AR_SECTION)
    return false;
  if (ref->next)
    return false;

  for (i = 0; i < ref->u.ar.dimen; i++)
    {
      /* If we have a single element in the reference, for the reference
	 to be full, we need to ascertain that the array has a single
	 element in this dimension and that we actually reference the
	 correct element.  */
      if (ref->u.ar.dimen_type[i] == DIMEN_ELEMENT)
	{
	  /* This is unconditionally a contiguous reference if all the
	     remaining dimensions are elements.  */
	  if (contiguous)
	    {
	      *contiguous = true;
	      for (n = i + 1; n < ref->u.ar.dimen; n++)
		if (ref->u.ar.dimen_type[n] != DIMEN_ELEMENT)
		  *contiguous = false;
	    }

	  if (!ref->u.ar.as
	      || !ref->u.ar.as->lower[i]
	      || !ref->u.ar.as->upper[i]
	      || gfc_dep_compare_expr (ref->u.ar.as->lower[i],
				       ref->u.ar.as->upper[i])
	      || !ref->u.ar.start[i]
	      || gfc_dep_compare_expr (ref->u.ar.start[i],
				       ref->u.ar.as->lower[i]))
	    return false;
	  else
	    continue;
	}

      /* Check the lower bound.  */
      if (ref->u.ar.start[i]
	  && (!ref->u.ar.as
	      || !ref->u.ar.as->lower[i]
	      || gfc_dep_compare_expr (ref->u.ar.start[i],
				       ref->u.ar.as->lower[i])
	      || dummy_intent_not_in (&ref->u.ar.start[i])))
	lbound_OK = false;
      /* Check the upper bound.  */
      if (ref->u.ar.end[i]
	  && (!ref->u.ar.as
	      || !ref->u.ar.as->upper[i]
	      || gfc_dep_compare_expr (ref->u.ar.end[i],
				       ref->u.ar.as->upper[i])
	      || dummy_intent_not_in (&ref->u.ar.end[i])))
	ubound_OK = false;
      /* Check the stride.  */
      if (ref->u.ar.stride[i]
	    && !gfc_expr_is_one (ref->u.ar.stride[i], 0))
	return false;

      /* This is unconditionally a contiguous reference as long as all
	 the subsequent dimensions are elements.  */
      if (contiguous)
	{
	  *contiguous = true;
	  for (n = i + 1; n < ref->u.ar.dimen; n++)
	    if (ref->u.ar.dimen_type[n] != DIMEN_ELEMENT)
	      *contiguous = false;
	}

      if (!lbound_OK || !ubound_OK)
	return false;
    }
  return true;
}


/* Determine if a full array is the same as an array section with one
   variable limit.  For this to be so, the strides must both be unity
   and one of either start == lower or end == upper must be true.  */

static bool
ref_same_as_full_array (gfc_ref *full_ref, gfc_ref *ref)
{
  int i;
  bool upper_or_lower;

  if (full_ref->type != REF_ARRAY)
    return false;
  if (full_ref->u.ar.type != AR_FULL)
    return false;
  if (ref->type != REF_ARRAY)
    return false;
  if (ref->u.ar.type != AR_SECTION)
    return false;

  for (i = 0; i < ref->u.ar.dimen; i++)
    {
      /* If we have a single element in the reference, we need to check
	 that the array has a single element and that we actually reference
	 the correct element.  */
      if (ref->u.ar.dimen_type[i] == DIMEN_ELEMENT)
	{
	  if (!full_ref->u.ar.as
	      || !full_ref->u.ar.as->lower[i]
	      || !full_ref->u.ar.as->upper[i]
	      || gfc_dep_compare_expr (full_ref->u.ar.as->lower[i],
				       full_ref->u.ar.as->upper[i])
	      || !ref->u.ar.start[i]
	      || gfc_dep_compare_expr (ref->u.ar.start[i],
				       full_ref->u.ar.as->lower[i]))
	    return false;
	}

      /* Check the strides.  */
      if (full_ref->u.ar.stride[i] && !gfc_expr_is_one (full_ref->u.ar.stride[i], 0))
	return false;
      if (ref->u.ar.stride[i] && !gfc_expr_is_one (ref->u.ar.stride[i], 0))
	return false;

      upper_or_lower = false;
      /* Check the lower bound.  */
      if (ref->u.ar.start[i]
	  && (ref->u.ar.as
	        && full_ref->u.ar.as->lower[i]
	        && gfc_dep_compare_expr (ref->u.ar.start[i],
				         full_ref->u.ar.as->lower[i]) == 0))
	upper_or_lower =  true;
      /* Check the upper bound.  */
      if (ref->u.ar.end[i]
	  && (ref->u.ar.as
	        && full_ref->u.ar.as->upper[i]
	        && gfc_dep_compare_expr (ref->u.ar.end[i],
				         full_ref->u.ar.as->upper[i]) == 0))
	upper_or_lower =  true;
      if (!upper_or_lower)
	return false;
    }
  return true;
}


/* Finds if two array references are overlapping or not.
   Return value
   	2 : array references are overlapping but reversal of one or
	    more dimensions will clear the dependency.
   	1 : array references are overlapping.
   	0 : array references are identical or not overlapping.  */

int
gfc_dep_resolver (gfc_ref *lref, gfc_ref *rref, gfc_reverse *reverse)
{
  int n;
  int m;
  gfc_dependency fin_dep;
  gfc_dependency this_dep;

  this_dep = GFC_DEP_ERROR;
  fin_dep = GFC_DEP_ERROR;
  /* Dependencies due to pointers should already have been identified.
     We only need to check for overlapping array references.  */

  while (lref && rref)
    {
      /* We're resolving from the same base symbol, so both refs should be
	 the same type.  We traverse the reference chain until we find ranges
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
	  /* Substring overlaps are handled by the string assignment code
	     if there is not an underlying dependency.  */
	  return (fin_dep == GFC_DEP_OVERLAP) ? 1 : 0;

	case REF_ARRAY:

	  if (ref_same_as_full_array (lref, rref))
	    return 0;

	  if (ref_same_as_full_array (rref, lref))
	    return 0;

	  if (lref->u.ar.dimen != rref->u.ar.dimen)
	    {
	      if (lref->u.ar.type == AR_FULL)
		fin_dep = gfc_full_array_ref_p (rref, NULL) ? GFC_DEP_EQUAL
							    : GFC_DEP_OVERLAP;
	      else if (rref->u.ar.type == AR_FULL)
		fin_dep = gfc_full_array_ref_p (lref, NULL) ? GFC_DEP_EQUAL
							    : GFC_DEP_OVERLAP;
	      else
		return 1;
	      break;
	    }

	  /* Index for the reverse array.  */
	  m = -1;
	  for (n=0; n < lref->u.ar.dimen; n++)
	    {
	      /* Handle dependency when either of array reference is vector
		 subscript. There is no dependency if the vector indices
		 are equal or if indices are known to be different in a
		 different dimension.  */
	      if (lref->u.ar.dimen_type[n] == DIMEN_VECTOR
		  || rref->u.ar.dimen_type[n] == DIMEN_VECTOR)
		{
		  if (lref->u.ar.dimen_type[n] == DIMEN_VECTOR
		      && rref->u.ar.dimen_type[n] == DIMEN_VECTOR
		      && gfc_dep_compare_expr (lref->u.ar.start[n],
					       rref->u.ar.start[n]) == 0)
		    this_dep = GFC_DEP_EQUAL;
		  else
		    this_dep = GFC_DEP_OVERLAP;

		  goto update_fin_dep;
		}

	      if (lref->u.ar.dimen_type[n] == DIMEN_RANGE
		  && rref->u.ar.dimen_type[n] == DIMEN_RANGE)
		this_dep = check_section_vs_section (&lref->u.ar, &rref->u.ar, n);
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

	      /* Now deal with the loop reversal logic:  This only works on
		 ranges and is activated by setting
				reverse[n] == GFC_ENABLE_REVERSE
		 The ability to reverse or not is set by previous conditions
		 in this dimension.  If reversal is not activated, the
		 value GFC_DEP_BACKWARD is reset to GFC_DEP_OVERLAP.  */

	      /* Get the indexing right for the scalarizing loop. If this
		 is an element, there is no corresponding loop.  */
	      if (lref->u.ar.dimen_type[n] != DIMEN_ELEMENT)
		m++;

	      if (rref->u.ar.dimen_type[n] == DIMEN_RANGE
		    && lref->u.ar.dimen_type[n] == DIMEN_RANGE)
		{
		  /* Set reverse if backward dependence and not inhibited.  */
		  if (reverse && reverse[m] == GFC_ENABLE_REVERSE)
		    reverse[m] = (this_dep == GFC_DEP_BACKWARD) ?
			         GFC_REVERSE_SET : reverse[m];

		  /* Set forward if forward dependence and not inhibited.  */
		  if (reverse && reverse[m] == GFC_ENABLE_REVERSE)
		    reverse[m] = (this_dep == GFC_DEP_FORWARD) ?
			         GFC_FORWARD_SET : reverse[m];

		  /* Flag up overlap if dependence not compatible with
		     the overall state of the expression.  */
		  if (reverse && reverse[m] == GFC_REVERSE_SET
		        && this_dep == GFC_DEP_FORWARD)
		    {
	              reverse[m] = GFC_INHIBIT_REVERSE;
		      this_dep = GFC_DEP_OVERLAP;
		    }
		  else if (reverse && reverse[m] == GFC_FORWARD_SET
		        && this_dep == GFC_DEP_BACKWARD)
		    {
	              reverse[m] = GFC_INHIBIT_REVERSE;
		      this_dep = GFC_DEP_OVERLAP;
		    }

		  /* If no intention of reversing or reversing is explicitly
		     inhibited, convert backward dependence to overlap.  */
		  if ((reverse == NULL && this_dep == GFC_DEP_BACKWARD)
		      || (reverse != NULL && reverse[m] == GFC_INHIBIT_REVERSE))
		    this_dep = GFC_DEP_OVERLAP;
		}

	      /* Overlap codes are in order of priority.  We only need to
		 know the worst one.*/

	    update_fin_dep:
	      if (this_dep > fin_dep)
		fin_dep = this_dep;
	    }

	  /* If this is an equal element, we have to keep going until we find
	     the "real" array reference.  */
	  if (lref->u.ar.type == AR_ELEMENT
		&& rref->u.ar.type == AR_ELEMENT
		&& fin_dep == GFC_DEP_EQUAL)
	    break;

	  /* Exactly matching and forward overlapping ranges don't cause a
	     dependency.  */
	  if (fin_dep < GFC_DEP_BACKWARD)
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
