/* Array things
   Copyright (C) 2000-2016 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
#include "match.h"
#include "constructor.h"

/**************** Array reference matching subroutines *****************/

/* Copy an array reference structure.  */

gfc_array_ref *
gfc_copy_array_ref (gfc_array_ref *src)
{
  gfc_array_ref *dest;
  int i;

  if (src == NULL)
    return NULL;

  dest = gfc_get_array_ref ();

  *dest = *src;

  for (i = 0; i < GFC_MAX_DIMENSIONS; i++)
    {
      dest->start[i] = gfc_copy_expr (src->start[i]);
      dest->end[i] = gfc_copy_expr (src->end[i]);
      dest->stride[i] = gfc_copy_expr (src->stride[i]);
    }

  return dest;
}


/* Match a single dimension of an array reference.  This can be a
   single element or an array section.  Any modifications we've made
   to the ar structure are cleaned up by the caller.  If the init
   is set, we require the subscript to be a valid initialization
   expression.  */

static match
match_subscript (gfc_array_ref *ar, int init, bool match_star)
{
  match m = MATCH_ERROR;
  bool star = false;
  int i;

  i = ar->dimen + ar->codimen;

  gfc_gobble_whitespace ();
  ar->c_where[i] = gfc_current_locus;
  ar->start[i] = ar->end[i] = ar->stride[i] = NULL;

  /* We can't be sure of the difference between DIMEN_ELEMENT and
     DIMEN_VECTOR until we know the type of the element itself at
     resolution time.  */

  ar->dimen_type[i] = DIMEN_UNKNOWN;

  if (gfc_match_char (':') == MATCH_YES)
    goto end_element;

  /* Get start element.  */
  if (match_star && (m = gfc_match_char ('*')) == MATCH_YES)
    star = true;

  if (!star && init)
    m = gfc_match_init_expr (&ar->start[i]);
  else if (!star)
    m = gfc_match_expr (&ar->start[i]);

  if (m == MATCH_NO)
    gfc_error ("Expected array subscript at %C");
  if (m != MATCH_YES)
    return MATCH_ERROR;

  if (gfc_match_char (':') == MATCH_NO)
    goto matched;

  if (star)
    {
      gfc_error ("Unexpected %<*%> in coarray subscript at %C");
      return MATCH_ERROR;
    }

  /* Get an optional end element.  Because we've seen the colon, we
     definitely have a range along this dimension.  */
end_element:
  ar->dimen_type[i] = DIMEN_RANGE;

  if (match_star && (m = gfc_match_char ('*')) == MATCH_YES)
    star = true;
  else if (init)
    m = gfc_match_init_expr (&ar->end[i]);
  else
    m = gfc_match_expr (&ar->end[i]);

  if (m == MATCH_ERROR)
    return MATCH_ERROR;

  /* See if we have an optional stride.  */
  if (gfc_match_char (':') == MATCH_YES)
    {
      if (star)
	{
	  gfc_error ("Strides not allowed in coarray subscript at %C");
	  return MATCH_ERROR;
	}

      m = init ? gfc_match_init_expr (&ar->stride[i])
	       : gfc_match_expr (&ar->stride[i]);

      if (m == MATCH_NO)
	gfc_error ("Expected array subscript stride at %C");
      if (m != MATCH_YES)
	return MATCH_ERROR;
    }

matched:
  if (star)
    ar->dimen_type[i] = DIMEN_STAR;

  return MATCH_YES;
}


/* Match an array reference, whether it is the whole array or particular
   elements or a section.  If init is set, the reference has to consist
   of init expressions.  */

match
gfc_match_array_ref (gfc_array_ref *ar, gfc_array_spec *as, int init,
		     int corank)
{
  match m;
  bool matched_bracket = false;

  memset (ar, '\0', sizeof (*ar));

  ar->where = gfc_current_locus;
  ar->as = as;
  ar->type = AR_UNKNOWN;

  if (gfc_match_char ('[') == MATCH_YES)
    {
       matched_bracket = true;
       goto coarray;
    }

  if (gfc_match_char ('(') != MATCH_YES)
    {
      ar->type = AR_FULL;
      ar->dimen = 0;
      return MATCH_YES;
    }

  for (ar->dimen = 0; ar->dimen < GFC_MAX_DIMENSIONS; ar->dimen++)
    {
      m = match_subscript (ar, init, false);
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      if (gfc_match_char (')') == MATCH_YES)
	{
	  ar->dimen++;
	  goto coarray;
	}

      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Invalid form of array reference at %C");
	  return MATCH_ERROR;
	}
    }

  gfc_error ("Array reference at %C cannot have more than %d dimensions",
	     GFC_MAX_DIMENSIONS);
  return MATCH_ERROR;

coarray:
  if (!matched_bracket && gfc_match_char ('[') != MATCH_YES)
    {
      if (ar->dimen > 0)
	return MATCH_YES;
      else
	return MATCH_ERROR;
    }

  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      return MATCH_ERROR;
    }

  if (corank == 0)
    {
	gfc_error ("Unexpected coarray designator at %C");
	return MATCH_ERROR;
    }

  for (ar->codimen = 0; ar->codimen + ar->dimen < GFC_MAX_DIMENSIONS; ar->codimen++)
    {
      m = match_subscript (ar, init, true);
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      if (gfc_match_char (']') == MATCH_YES)
	{
	  ar->codimen++;
	  if (ar->codimen < corank)
	    {
	      gfc_error ("Too few codimensions at %C, expected %d not %d",
			 corank, ar->codimen);
	      return MATCH_ERROR;
	    }
	  if (ar->codimen > corank)
	    {
	      gfc_error ("Too many codimensions at %C, expected %d not %d",
			 corank, ar->codimen);
	      return MATCH_ERROR;
	    }
	  return MATCH_YES;
	}

      if (gfc_match_char (',') != MATCH_YES)
	{
	  if (gfc_match_char ('*') == MATCH_YES)
	    gfc_error ("Unexpected %<*%> for codimension %d of %d at %C",
		       ar->codimen + 1, corank);
	  else
	    gfc_error ("Invalid form of coarray reference at %C");
	  return MATCH_ERROR;
	}
      else if (ar->dimen_type[ar->codimen + ar->dimen] == DIMEN_STAR)
	{
	  gfc_error ("Unexpected %<*%> for codimension %d of %d at %C",
		     ar->codimen + 1, corank);
	  return MATCH_ERROR;
	}

      if (ar->codimen >= corank)
	{
	  gfc_error ("Invalid codimension %d at %C, only %d codimensions exist",
		     ar->codimen + 1, corank);
	  return MATCH_ERROR;
	}
    }

  gfc_error ("Array reference at %C cannot have more than %d dimensions",
	     GFC_MAX_DIMENSIONS);
  return MATCH_ERROR;

}


/************** Array specification matching subroutines ***************/

/* Free all of the expressions associated with array bounds
   specifications.  */

void
gfc_free_array_spec (gfc_array_spec *as)
{
  int i;

  if (as == NULL)
    return;

  for (i = 0; i < as->rank + as->corank; i++)
    {
      gfc_free_expr (as->lower[i]);
      gfc_free_expr (as->upper[i]);
    }

  free (as);
}


/* Take an array bound, resolves the expression, that make up the
   shape and check associated constraints.  */

static bool
resolve_array_bound (gfc_expr *e, int check_constant)
{
  if (e == NULL)
    return true;

  if (!gfc_resolve_expr (e)
      || !gfc_specification_expr (e))
    return false;

  if (check_constant && !gfc_is_constant_expr (e))
    {
      if (e->expr_type == EXPR_VARIABLE)
	gfc_error ("Variable %qs at %L in this context must be constant",
		   e->symtree->n.sym->name, &e->where);
      else
	gfc_error ("Expression at %L in this context must be constant",
		   &e->where);
      return false;
    }

  return true;
}


/* Takes an array specification, resolves the expressions that make up
   the shape and make sure everything is integral.  */

bool
gfc_resolve_array_spec (gfc_array_spec *as, int check_constant)
{
  gfc_expr *e;
  int i;

  if (as == NULL)
    return true;

  if (as->resolved)
    return true;

  for (i = 0; i < as->rank + as->corank; i++)
    {
      e = as->lower[i];
      if (!resolve_array_bound (e, check_constant))
	return false;

      e = as->upper[i];
      if (!resolve_array_bound (e, check_constant))
	return false;

      if ((as->lower[i] == NULL) || (as->upper[i] == NULL))
	continue;

      /* If the size is negative in this dimension, set it to zero.  */
      if (as->lower[i]->expr_type == EXPR_CONSTANT
	    && as->upper[i]->expr_type == EXPR_CONSTANT
	    && mpz_cmp (as->upper[i]->value.integer,
			as->lower[i]->value.integer) < 0)
	{
	  gfc_free_expr (as->upper[i]);
	  as->upper[i] = gfc_copy_expr (as->lower[i]);
	  mpz_sub_ui (as->upper[i]->value.integer,
		      as->upper[i]->value.integer, 1);
	}
    }

  as->resolved = true;

  return true;
}


/* Match a single array element specification.  The return values as
   well as the upper and lower bounds of the array spec are filled
   in according to what we see on the input.  The caller makes sure
   individual specifications make sense as a whole.


	Parsed       Lower   Upper  Returned
	------------------------------------
	  :           NULL    NULL   AS_DEFERRED (*)
	  x            1       x     AS_EXPLICIT
	  x:           x      NULL   AS_ASSUMED_SHAPE
	  x:y          x       y     AS_EXPLICIT
	  x:*          x      NULL   AS_ASSUMED_SIZE
	  *            1      NULL   AS_ASSUMED_SIZE

  (*) For non-pointer dummy arrays this is AS_ASSUMED_SHAPE.  This
  is fixed during the resolution of formal interfaces.

   Anything else AS_UNKNOWN.  */

static array_type
match_array_element_spec (gfc_array_spec *as)
{
  gfc_expr **upper, **lower;
  match m;
  int rank;

  rank = as->rank == -1 ? 0 : as->rank;
  lower = &as->lower[rank + as->corank - 1];
  upper = &as->upper[rank + as->corank - 1];

  if (gfc_match_char ('*') == MATCH_YES)
    {
      *lower = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);
      return AS_ASSUMED_SIZE;
    }

  if (gfc_match_char (':') == MATCH_YES)
    return AS_DEFERRED;

  m = gfc_match_expr (upper);
  if (m == MATCH_NO)
    gfc_error ("Expected expression in array specification at %C");
  if (m != MATCH_YES)
    return AS_UNKNOWN;
  if (!gfc_expr_check_typed (*upper, gfc_current_ns, false))
    return AS_UNKNOWN;

  if (((*upper)->expr_type == EXPR_CONSTANT
	&& (*upper)->ts.type != BT_INTEGER) ||
      ((*upper)->expr_type == EXPR_FUNCTION
	&& (*upper)->ts.type == BT_UNKNOWN
	&& (*upper)->symtree
	&& strcmp ((*upper)->symtree->name, "null") == 0))
    {
      gfc_error ("Expecting a scalar INTEGER expression at %C, found %s",
		 gfc_basic_typename ((*upper)->ts.type));
      return AS_UNKNOWN;
    }

  if (gfc_match_char (':') == MATCH_NO)
    {
      *lower = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);
      return AS_EXPLICIT;
    }

  *lower = *upper;
  *upper = NULL;

  if (gfc_match_char ('*') == MATCH_YES)
    return AS_ASSUMED_SIZE;

  m = gfc_match_expr (upper);
  if (m == MATCH_ERROR)
    return AS_UNKNOWN;
  if (m == MATCH_NO)
    return AS_ASSUMED_SHAPE;
  if (!gfc_expr_check_typed (*upper, gfc_current_ns, false))
    return AS_UNKNOWN;

  if (((*upper)->expr_type == EXPR_CONSTANT
	&& (*upper)->ts.type != BT_INTEGER) ||
      ((*upper)->expr_type == EXPR_FUNCTION
	&& (*upper)->ts.type == BT_UNKNOWN
	&& (*upper)->symtree
	&& strcmp ((*upper)->symtree->name, "null") == 0))
    {
      gfc_error ("Expecting a scalar INTEGER expression at %C, found %s",
		 gfc_basic_typename ((*upper)->ts.type));
      return AS_UNKNOWN;
    }

  return AS_EXPLICIT;
}


/* Matches an array specification, incidentally figuring out what sort
   it is.  Match either a normal array specification, or a coarray spec
   or both.  Optionally allow [:] for coarrays.  */

match
gfc_match_array_spec (gfc_array_spec **asp, bool match_dim, bool match_codim)
{
  array_type current_type;
  gfc_array_spec *as;
  int i;

  as = gfc_get_array_spec ();

  if (!match_dim)
    goto coarray;

  if (gfc_match_char ('(') != MATCH_YES)
    {
      if (!match_codim)
	goto done;
      goto coarray;
    }

  if (gfc_match (" .. )") == MATCH_YES)
    {
      as->type = AS_ASSUMED_RANK;
      as->rank = -1;

      if (!gfc_notify_std (GFC_STD_F2008_TS, "Assumed-rank array at %C"))
	goto cleanup;

      if (!match_codim)
	goto done;
      goto coarray;
    }

  for (;;)
    {
      as->rank++;
      current_type = match_array_element_spec (as);

      /* Note that current_type == AS_ASSUMED_SIZE for both assumed-size
	 and implied-shape specifications.  If the rank is at least 2, we can
	 distinguish between them.  But for rank 1, we currently return
	 ASSUMED_SIZE; this gets adjusted later when we know for sure
	 whether the symbol parsed is a PARAMETER or not.  */

      if (as->rank == 1)
	{
	  if (current_type == AS_UNKNOWN)
	    goto cleanup;
	  as->type = current_type;
	}
      else
	switch (as->type)
	  {		/* See how current spec meshes with the existing.  */
	  case AS_UNKNOWN:
	    goto cleanup;

	  case AS_IMPLIED_SHAPE:
	    if (current_type != AS_ASSUMED_SHAPE)
	      {
		gfc_error ("Bad array specification for implied-shape"
			   " array at %C");
		goto cleanup;
	      }
	    break;

	  case AS_EXPLICIT:
	    if (current_type == AS_ASSUMED_SIZE)
	      {
		as->type = AS_ASSUMED_SIZE;
		break;
	      }

	    if (current_type == AS_EXPLICIT)
	      break;

	    gfc_error ("Bad array specification for an explicitly shaped "
		       "array at %C");

	    goto cleanup;

	  case AS_ASSUMED_SHAPE:
	    if ((current_type == AS_ASSUMED_SHAPE)
		|| (current_type == AS_DEFERRED))
	      break;

	    gfc_error ("Bad array specification for assumed shape "
		       "array at %C");
	    goto cleanup;

	  case AS_DEFERRED:
	    if (current_type == AS_DEFERRED)
	      break;

	    if (current_type == AS_ASSUMED_SHAPE)
	      {
		as->type = AS_ASSUMED_SHAPE;
		break;
	      }

	    gfc_error ("Bad specification for deferred shape array at %C");
	    goto cleanup;

	  case AS_ASSUMED_SIZE:
	    if (as->rank == 2 && current_type == AS_ASSUMED_SIZE)
	      {
		as->type = AS_IMPLIED_SHAPE;
		break;
	      }

	    gfc_error ("Bad specification for assumed size array at %C");
	    goto cleanup;

	  case AS_ASSUMED_RANK:
	    gcc_unreachable ();
	  }

      if (gfc_match_char (')') == MATCH_YES)
	break;

      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Expected another dimension in array declaration at %C");
	  goto cleanup;
	}

      if (as->rank + as->corank >= GFC_MAX_DIMENSIONS)
	{
	  gfc_error ("Array specification at %C has more than %d dimensions",
		     GFC_MAX_DIMENSIONS);
	  goto cleanup;
	}

      if (as->corank + as->rank >= 7
	  && !gfc_notify_std (GFC_STD_F2008, "Array specification at %C "
			      "with more than 7 dimensions"))
	goto cleanup;
    }

  if (!match_codim)
    goto done;

coarray:
  if (gfc_match_char ('[')  != MATCH_YES)
    goto done;

  if (!gfc_notify_std (GFC_STD_F2008, "Coarray declaration at %C"))
    goto cleanup;

  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      goto cleanup;
    }

  if (as->rank >= GFC_MAX_DIMENSIONS)
    {
      gfc_error ("Array specification at %C has more than %d "
		 "dimensions", GFC_MAX_DIMENSIONS);
      goto cleanup;
    }

  for (;;)
    {
      as->corank++;
      current_type = match_array_element_spec (as);

      if (current_type == AS_UNKNOWN)
	goto cleanup;

      if (as->corank == 1)
	as->cotype = current_type;
      else
	switch (as->cotype)
	  { /* See how current spec meshes with the existing.  */
	    case AS_IMPLIED_SHAPE:
	    case AS_UNKNOWN:
	      goto cleanup;

	    case AS_EXPLICIT:
	      if (current_type == AS_ASSUMED_SIZE)
		{
		  as->cotype = AS_ASSUMED_SIZE;
		  break;
		}

	      if (current_type == AS_EXPLICIT)
		break;

	      gfc_error ("Bad array specification for an explicitly "
			 "shaped array at %C");

	      goto cleanup;

	    case AS_ASSUMED_SHAPE:
	      if ((current_type == AS_ASSUMED_SHAPE)
		  || (current_type == AS_DEFERRED))
		break;

	      gfc_error ("Bad array specification for assumed shape "
			 "array at %C");
	      goto cleanup;

	    case AS_DEFERRED:
	      if (current_type == AS_DEFERRED)
		break;

	      if (current_type == AS_ASSUMED_SHAPE)
		{
		  as->cotype = AS_ASSUMED_SHAPE;
		  break;
		}

	      gfc_error ("Bad specification for deferred shape array at %C");
	      goto cleanup;

	    case AS_ASSUMED_SIZE:
	      gfc_error ("Bad specification for assumed size array at %C");
	      goto cleanup;

	    case AS_ASSUMED_RANK:
	      gcc_unreachable ();
	  }

      if (gfc_match_char (']') == MATCH_YES)
	break;

      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Expected another dimension in array declaration at %C");
	  goto cleanup;
	}

      if (as->rank + as->corank >= GFC_MAX_DIMENSIONS)
	{
	  gfc_error ("Array specification at %C has more than %d "
		     "dimensions", GFC_MAX_DIMENSIONS);
	  goto cleanup;
	}
    }

  if (current_type == AS_EXPLICIT)
    {
      gfc_error ("Upper bound of last coarray dimension must be %<*%> at %C");
      goto cleanup;
    }

  if (as->cotype == AS_ASSUMED_SIZE)
    as->cotype = AS_EXPLICIT;

  if (as->rank == 0)
    as->type = as->cotype;

done:
  if (as->rank == 0 && as->corank == 0)
    {
      *asp = NULL;
      gfc_free_array_spec (as);
      return MATCH_NO;
    }

  /* If a lower bounds of an assumed shape array is blank, put in one.  */
  if (as->type == AS_ASSUMED_SHAPE)
    {
      for (i = 0; i < as->rank + as->corank; i++)
	{
	  if (as->lower[i] == NULL)
	    as->lower[i] = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);
	}
    }

  *asp = as;

  return MATCH_YES;

cleanup:
  /* Something went wrong.  */
  gfc_free_array_spec (as);
  return MATCH_ERROR;
}


/* Given a symbol and an array specification, modify the symbol to
   have that array specification.  The error locus is needed in case
   something goes wrong.  On failure, the caller must free the spec.  */

bool
gfc_set_array_spec (gfc_symbol *sym, gfc_array_spec *as, locus *error_loc)
{
  int i;

  if (as == NULL)
    return true;

  if (as->rank
      && !gfc_add_dimension (&sym->attr, sym->name, error_loc))
    return false;

  if (as->corank
      && !gfc_add_codimension (&sym->attr, sym->name, error_loc))
    return false;

  if (sym->as == NULL)
    {
      sym->as = as;
      return true;
    }

  if ((sym->as->type == AS_ASSUMED_RANK && as->corank)
      || (as->type == AS_ASSUMED_RANK && sym->as->corank))
    {
      gfc_error ("The assumed-rank array %qs at %L shall not have a "
		 "codimension", sym->name, error_loc);
      return false;
    }

  if (as->corank)
    {
      /* The "sym" has no corank (checked via gfc_add_codimension). Thus
	 the codimension is simply added.  */
      gcc_assert (as->rank == 0 && sym->as->corank == 0);

      sym->as->cotype = as->cotype;
      sym->as->corank = as->corank;
      for (i = 0; i < as->corank; i++)
	{
	  sym->as->lower[sym->as->rank + i] = as->lower[i];
	  sym->as->upper[sym->as->rank + i] = as->upper[i];
	}
    }
  else
    {
      /* The "sym" has no rank (checked via gfc_add_dimension). Thus
	 the dimension is added - but first the codimensions (if existing
	 need to be shifted to make space for the dimension.  */
      gcc_assert (as->corank == 0 && sym->as->rank == 0);

      sym->as->rank = as->rank;
      sym->as->type = as->type;
      sym->as->cray_pointee = as->cray_pointee;
      sym->as->cp_was_assumed = as->cp_was_assumed;

      for (i = 0; i < sym->as->corank; i++)
	{
	  sym->as->lower[as->rank + i] = sym->as->lower[i];
	  sym->as->upper[as->rank + i] = sym->as->upper[i];
	}
      for (i = 0; i < as->rank; i++)
	{
	  sym->as->lower[i] = as->lower[i];
	  sym->as->upper[i] = as->upper[i];
	}
    }

  free (as);
  return true;
}


/* Copy an array specification.  */

gfc_array_spec *
gfc_copy_array_spec (gfc_array_spec *src)
{
  gfc_array_spec *dest;
  int i;

  if (src == NULL)
    return NULL;

  dest = gfc_get_array_spec ();

  *dest = *src;

  for (i = 0; i < dest->rank + dest->corank; i++)
    {
      dest->lower[i] = gfc_copy_expr (dest->lower[i]);
      dest->upper[i] = gfc_copy_expr (dest->upper[i]);
    }

  return dest;
}


/* Returns nonzero if the two expressions are equal.  Only handles integer
   constants.  */

static int
compare_bounds (gfc_expr *bound1, gfc_expr *bound2)
{
  if (bound1 == NULL || bound2 == NULL
      || bound1->expr_type != EXPR_CONSTANT
      || bound2->expr_type != EXPR_CONSTANT
      || bound1->ts.type != BT_INTEGER
      || bound2->ts.type != BT_INTEGER)
    gfc_internal_error ("gfc_compare_array_spec(): Array spec clobbered");

  if (mpz_cmp (bound1->value.integer, bound2->value.integer) == 0)
    return 1;
  else
    return 0;
}


/* Compares two array specifications.  They must be constant or deferred
   shape.  */

int
gfc_compare_array_spec (gfc_array_spec *as1, gfc_array_spec *as2)
{
  int i;

  if (as1 == NULL && as2 == NULL)
    return 1;

  if (as1 == NULL || as2 == NULL)
    return 0;

  if (as1->rank != as2->rank)
    return 0;

  if (as1->corank != as2->corank)
    return 0;

  if (as1->rank == 0)
    return 1;

  if (as1->type != as2->type)
    return 0;

  if (as1->type == AS_EXPLICIT)
    for (i = 0; i < as1->rank + as1->corank; i++)
      {
	if (compare_bounds (as1->lower[i], as2->lower[i]) == 0)
	  return 0;

	if (compare_bounds (as1->upper[i], as2->upper[i]) == 0)
	  return 0;
      }

  return 1;
}


/****************** Array constructor functions ******************/


/* Given an expression node that might be an array constructor and a
   symbol, make sure that no iterators in this or child constructors
   use the symbol as an implied-DO iterator.  Returns nonzero if a
   duplicate was found.  */

static int
check_duplicate_iterator (gfc_constructor_base base, gfc_symbol *master)
{
  gfc_constructor *c;
  gfc_expr *e;

  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    {
      e = c->expr;

      if (e->expr_type == EXPR_ARRAY
	  && check_duplicate_iterator (e->value.constructor, master))
	return 1;

      if (c->iterator == NULL)
	continue;

      if (c->iterator->var->symtree->n.sym == master)
	{
	  gfc_error ("DO-iterator %qs at %L is inside iterator of the "
		     "same name", master->name, &c->where);

	  return 1;
	}
    }

  return 0;
}


/* Forward declaration because these functions are mutually recursive.  */
static match match_array_cons_element (gfc_constructor_base *);

/* Match a list of array elements.  */

static match
match_array_list (gfc_constructor_base *result)
{
  gfc_constructor_base head;
  gfc_constructor *p;
  gfc_iterator iter;
  locus old_loc;
  gfc_expr *e;
  match m;
  int n;

  old_loc = gfc_current_locus;

  if (gfc_match_char ('(') == MATCH_NO)
    return MATCH_NO;

  memset (&iter, '\0', sizeof (gfc_iterator));
  head = NULL;

  m = match_array_cons_element (&head);
  if (m != MATCH_YES)
    goto cleanup;

  if (gfc_match_char (',') != MATCH_YES)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  for (n = 1;; n++)
    {
      m = gfc_match_iterator (&iter, 0);
      if (m == MATCH_YES)
	break;
      if (m == MATCH_ERROR)
	goto cleanup;

      m = match_array_cons_element (&head);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	{
	  if (n > 2)
	    goto syntax;
	  m = MATCH_NO;
	  goto cleanup;		/* Could be a complex constant */
	}

      if (gfc_match_char (',') != MATCH_YES)
	{
	  if (n > 2)
	    goto syntax;
	  m = MATCH_NO;
	  goto cleanup;
	}
    }

  if (gfc_match_char (')') != MATCH_YES)
    goto syntax;

  if (check_duplicate_iterator (head, iter.var->symtree->n.sym))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  e = gfc_get_array_expr (BT_UNKNOWN, 0, &old_loc);
  e->value.constructor = head;

  p = gfc_constructor_append_expr (result, e, &gfc_current_locus);
  p->iterator = gfc_get_iterator ();
  *p->iterator = iter;

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in array constructor at %C");
  m = MATCH_ERROR;

cleanup:
  gfc_constructor_free (head);
  gfc_free_iterator (&iter, 0);
  gfc_current_locus = old_loc;
  return m;
}


/* Match a single element of an array constructor, which can be a
   single expression or a list of elements.  */

static match
match_array_cons_element (gfc_constructor_base *result)
{
  gfc_expr *expr;
  match m;

  m = match_array_list (result);
  if (m != MATCH_NO)
    return m;

  m = gfc_match_expr (&expr);
  if (m != MATCH_YES)
    return m;

  gfc_constructor_append_expr (result, expr, &gfc_current_locus);
  return MATCH_YES;
}


/* Match an array constructor.  */

match
gfc_match_array_constructor (gfc_expr **result)
{
  gfc_constructor_base head, new_cons;
  gfc_undo_change_set changed_syms;
  gfc_expr *expr;
  gfc_typespec ts;
  locus where;
  match m;
  const char *end_delim;
  bool seen_ts;

  if (gfc_match (" (/") == MATCH_NO)
    {
      if (gfc_match (" [") == MATCH_NO)
	return MATCH_NO;
      else
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "[...] "
			       "style array constructors at %C"))
	    return MATCH_ERROR;
	  end_delim = " ]";
	}
    }
  else
    end_delim = " /)";

  where = gfc_current_locus;
  head = new_cons = NULL;
  seen_ts = false;

  /* Try to match an optional "type-spec ::"  */
  gfc_clear_ts (&ts);
  gfc_new_undo_checkpoint (changed_syms);
  m = gfc_match_type_spec (&ts);
  if (m == MATCH_YES)
    {
      seen_ts = (gfc_match (" ::") == MATCH_YES);

      if (seen_ts)
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "Array constructor "
			       "including type specification at %C"))
	    {
	      gfc_restore_last_undo_checkpoint ();
	      goto cleanup;
	    }

	  if (ts.deferred)
	    {
	      gfc_error ("Type-spec at %L cannot contain a deferred "
			 "type parameter", &where);
	      gfc_restore_last_undo_checkpoint ();
	      goto cleanup;
	    }
	}
    }
  else if (m == MATCH_ERROR)
    {
      gfc_restore_last_undo_checkpoint ();
      goto cleanup;
    }

  if (seen_ts)
    gfc_drop_last_undo_checkpoint ();
  else
    {
      gfc_restore_last_undo_checkpoint ();
      gfc_current_locus = where;
    }

  if (gfc_match (end_delim) == MATCH_YES)
    {
      if (seen_ts)
	goto done;
      else
	{
	  gfc_error ("Empty array constructor at %C is not allowed");
	  goto cleanup;
	}
    }

  for (;;)
    {
      m = match_array_cons_element (&head);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      if (gfc_match_char (',') == MATCH_NO)
	break;
    }

  if (gfc_match (end_delim) == MATCH_NO)
    goto syntax;

done:
  /* Size must be calculated at resolution time.  */
  if (seen_ts)
    {
      expr = gfc_get_array_expr (ts.type, ts.kind, &where);
      expr->ts = ts;

      /* If the typespec is CHARACTER, check that array elements can
	 be converted.  See PR fortran/67803.  */
      if (ts.type == BT_CHARACTER)
	{
	  gfc_constructor *c;

	  c = gfc_constructor_first (head);
	  for (; c; c = gfc_constructor_next (c))
	    {
	      if (gfc_numeric_ts (&c->expr->ts)
		  || c->expr->ts.type == BT_LOGICAL)
		{
		  gfc_error ("Incompatible typespec for array element at %L",
			     &c->expr->where);
		  return MATCH_ERROR;
		}

	      /* Special case null().  */
	      if (c->expr->expr_type == EXPR_FUNCTION
		  && c->expr->ts.type == BT_UNKNOWN
		  && strcmp (c->expr->symtree->name, "null") == 0)
		{
		  gfc_error ("Incompatible typespec for array element at %L",
			     &c->expr->where);
		  return MATCH_ERROR;
		}
	    }
	}
    }
  else
    expr = gfc_get_array_expr (BT_UNKNOWN, 0, &where);

  expr->value.constructor = head;
  if (expr->ts.u.cl)
    expr->ts.u.cl->length_from_typespec = seen_ts;

  *result = expr;

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in array constructor at %C");

cleanup:
  gfc_constructor_free (head);
  return MATCH_ERROR;
}



/************** Check array constructors for correctness **************/

/* Given an expression, compare it's type with the type of the current
   constructor.  Returns nonzero if an error was issued.  The
   cons_state variable keeps track of whether the type of the
   constructor being read or resolved is known to be good, bad or just
   starting out.  */

static gfc_typespec constructor_ts;
static enum
{ CONS_START, CONS_GOOD, CONS_BAD }
cons_state;

static int
check_element_type (gfc_expr *expr, bool convert)
{
  if (cons_state == CONS_BAD)
    return 0;			/* Suppress further errors */

  if (cons_state == CONS_START)
    {
      if (expr->ts.type == BT_UNKNOWN)
	cons_state = CONS_BAD;
      else
	{
	  cons_state = CONS_GOOD;
	  constructor_ts = expr->ts;
	}

      return 0;
    }

  if (gfc_compare_types (&constructor_ts, &expr->ts))
    return 0;

  if (convert)
    return gfc_convert_type(expr, &constructor_ts, 1) ? 0 : 1;

  gfc_error ("Element in %s array constructor at %L is %s",
	     gfc_typename (&constructor_ts), &expr->where,
	     gfc_typename (&expr->ts));

  cons_state = CONS_BAD;
  return 1;
}


/* Recursive work function for gfc_check_constructor_type().  */

static bool
check_constructor_type (gfc_constructor_base base, bool convert)
{
  gfc_constructor *c;
  gfc_expr *e;

  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    {
      e = c->expr;

      if (e->expr_type == EXPR_ARRAY)
	{
	  if (!check_constructor_type (e->value.constructor, convert))
	    return false;

	  continue;
	}

      if (check_element_type (e, convert))
	return false;
    }

  return true;
}


/* Check that all elements of an array constructor are the same type.
   On false, an error has been generated.  */

bool
gfc_check_constructor_type (gfc_expr *e)
{
  bool t;

  if (e->ts.type != BT_UNKNOWN)
    {
      cons_state = CONS_GOOD;
      constructor_ts = e->ts;
    }
  else
    {
      cons_state = CONS_START;
      gfc_clear_ts (&constructor_ts);
    }

  /* If e->ts.type != BT_UNKNOWN, the array constructor included a
     typespec, and we will now convert the values on the fly.  */
  t = check_constructor_type (e->value.constructor, e->ts.type != BT_UNKNOWN);
  if (t && e->ts.type == BT_UNKNOWN)
    e->ts = constructor_ts;

  return t;
}



typedef struct cons_stack
{
  gfc_iterator *iterator;
  struct cons_stack *previous;
}
cons_stack;

static cons_stack *base;

static bool check_constructor (gfc_constructor_base, bool (*) (gfc_expr *));

/* Check an EXPR_VARIABLE expression in a constructor to make sure
   that that variable is an iteration variables.  */

bool
gfc_check_iter_variable (gfc_expr *expr)
{
  gfc_symbol *sym;
  cons_stack *c;

  sym = expr->symtree->n.sym;

  for (c = base; c && c->iterator; c = c->previous)
    if (sym == c->iterator->var->symtree->n.sym)
      return true;

  return false;
}


/* Recursive work function for gfc_check_constructor().  This amounts
   to calling the check function for each expression in the
   constructor, giving variables with the names of iterators a pass.  */

static bool
check_constructor (gfc_constructor_base ctor, bool (*check_function) (gfc_expr *))
{
  cons_stack element;
  gfc_expr *e;
  bool t;
  gfc_constructor *c;

  for (c = gfc_constructor_first (ctor); c; c = gfc_constructor_next (c))
    {
      e = c->expr;

      if (!e)
	continue;

      if (e->expr_type != EXPR_ARRAY)
	{
	  if (!(*check_function)(e))
	    return false;
	  continue;
	}

      element.previous = base;
      element.iterator = c->iterator;

      base = &element;
      t = check_constructor (e->value.constructor, check_function);
      base = element.previous;

      if (!t)
	return false;
    }

  /* Nothing went wrong, so all OK.  */
  return true;
}


/* Checks a constructor to see if it is a particular kind of
   expression -- specification, restricted, or initialization as
   determined by the check_function.  */

bool
gfc_check_constructor (gfc_expr *expr, bool (*check_function) (gfc_expr *))
{
  cons_stack *base_save;
  bool t;

  base_save = base;
  base = NULL;

  t = check_constructor (expr->value.constructor, check_function);
  base = base_save;

  return t;
}



/**************** Simplification of array constructors ****************/

iterator_stack *iter_stack;

typedef struct
{
  gfc_constructor_base base;
  int extract_count, extract_n;
  gfc_expr *extracted;
  mpz_t *count;

  mpz_t *offset;
  gfc_component *component;
  mpz_t *repeat;

  bool (*expand_work_function) (gfc_expr *);
}
expand_info;

static expand_info current_expand;

static bool expand_constructor (gfc_constructor_base);


/* Work function that counts the number of elements present in a
   constructor.  */

static bool
count_elements (gfc_expr *e)
{
  mpz_t result;

  if (e->rank == 0)
    mpz_add_ui (*current_expand.count, *current_expand.count, 1);
  else
    {
      if (!gfc_array_size (e, &result))
	{
	  gfc_free_expr (e);
	  return false;
	}

      mpz_add (*current_expand.count, *current_expand.count, result);
      mpz_clear (result);
    }

  gfc_free_expr (e);
  return true;
}


/* Work function that extracts a particular element from an array
   constructor, freeing the rest.  */

static bool
extract_element (gfc_expr *e)
{
  if (e->rank != 0)
    {				/* Something unextractable */
      gfc_free_expr (e);
      return false;
    }

  if (current_expand.extract_count == current_expand.extract_n)
    current_expand.extracted = e;
  else
    gfc_free_expr (e);

  current_expand.extract_count++;

  return true;
}


/* Work function that constructs a new constructor out of the old one,
   stringing new elements together.  */

static bool
expand (gfc_expr *e)
{
  gfc_constructor *c = gfc_constructor_append_expr (&current_expand.base,
						    e, &e->where);

  c->n.component = current_expand.component;
  return true;
}


/* Given an initialization expression that is a variable reference,
   substitute the current value of the iteration variable.  */

void
gfc_simplify_iterator_var (gfc_expr *e)
{
  iterator_stack *p;

  for (p = iter_stack; p; p = p->prev)
    if (e->symtree == p->variable)
      break;

  if (p == NULL)
    return;		/* Variable not found */

  gfc_replace_expr (e, gfc_get_int_expr (gfc_default_integer_kind, NULL, 0));

  mpz_set (e->value.integer, p->value);

  return;
}


/* Expand an expression with that is inside of a constructor,
   recursing into other constructors if present.  */

static bool
expand_expr (gfc_expr *e)
{
  if (e->expr_type == EXPR_ARRAY)
    return expand_constructor (e->value.constructor);

  e = gfc_copy_expr (e);

  if (!gfc_simplify_expr (e, 1))
    {
      gfc_free_expr (e);
      return false;
    }

  return current_expand.expand_work_function (e);
}


static bool
expand_iterator (gfc_constructor *c)
{
  gfc_expr *start, *end, *step;
  iterator_stack frame;
  mpz_t trip;
  bool t;

  end = step = NULL;

  t = false;

  mpz_init (trip);
  mpz_init (frame.value);
  frame.prev = NULL;

  start = gfc_copy_expr (c->iterator->start);
  if (!gfc_simplify_expr (start, 1))
    goto cleanup;

  if (start->expr_type != EXPR_CONSTANT || start->ts.type != BT_INTEGER)
    goto cleanup;

  end = gfc_copy_expr (c->iterator->end);
  if (!gfc_simplify_expr (end, 1))
    goto cleanup;

  if (end->expr_type != EXPR_CONSTANT || end->ts.type != BT_INTEGER)
    goto cleanup;

  step = gfc_copy_expr (c->iterator->step);
  if (!gfc_simplify_expr (step, 1))
    goto cleanup;

  if (step->expr_type != EXPR_CONSTANT || step->ts.type != BT_INTEGER)
    goto cleanup;

  if (mpz_sgn (step->value.integer) == 0)
    {
      gfc_error ("Iterator step at %L cannot be zero", &step->where);
      goto cleanup;
    }

  /* Calculate the trip count of the loop.  */
  mpz_sub (trip, end->value.integer, start->value.integer);
  mpz_add (trip, trip, step->value.integer);
  mpz_tdiv_q (trip, trip, step->value.integer);

  mpz_set (frame.value, start->value.integer);

  frame.prev = iter_stack;
  frame.variable = c->iterator->var->symtree;
  iter_stack = &frame;

  while (mpz_sgn (trip) > 0)
    {
      if (!expand_expr (c->expr))
	goto cleanup;

      mpz_add (frame.value, frame.value, step->value.integer);
      mpz_sub_ui (trip, trip, 1);
    }

  t = true;

cleanup:
  gfc_free_expr (start);
  gfc_free_expr (end);
  gfc_free_expr (step);

  mpz_clear (trip);
  mpz_clear (frame.value);

  iter_stack = frame.prev;

  return t;
}


/* Expand a constructor into constant constructors without any
   iterators, calling the work function for each of the expanded
   expressions.  The work function needs to either save or free the
   passed expression.  */

static bool
expand_constructor (gfc_constructor_base base)
{
  gfc_constructor *c;
  gfc_expr *e;

  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next(c))
    {
      if (c->iterator != NULL)
	{
	  if (!expand_iterator (c))
	    return false;
	  continue;
	}

      e = c->expr;

      if (e->expr_type == EXPR_ARRAY)
	{
	  if (!expand_constructor (e->value.constructor))
	    return false;

	  continue;
	}

      e = gfc_copy_expr (e);
      if (!gfc_simplify_expr (e, 1))
	{
	  gfc_free_expr (e);
	  return false;
	}
      current_expand.offset = &c->offset;
      current_expand.repeat = &c->repeat;
      current_expand.component = c->n.component;
      if (!current_expand.expand_work_function(e))
	return false;
    }
  return true;
}


/* Given an array expression and an element number (starting at zero),
   return a pointer to the array element.  NULL is returned if the
   size of the array has been exceeded.  The expression node returned
   remains a part of the array and should not be freed.  Access is not
   efficient at all, but this is another place where things do not
   have to be particularly fast.  */

static gfc_expr *
gfc_get_array_element (gfc_expr *array, int element)
{
  expand_info expand_save;
  gfc_expr *e;
  bool rc;

  expand_save = current_expand;
  current_expand.extract_n = element;
  current_expand.expand_work_function = extract_element;
  current_expand.extracted = NULL;
  current_expand.extract_count = 0;

  iter_stack = NULL;

  rc = expand_constructor (array->value.constructor);
  e = current_expand.extracted;
  current_expand = expand_save;

  if (!rc)
    return NULL;

  return e;
}


/* Top level subroutine for expanding constructors.  We only expand
   constructor if they are small enough.  */

bool
gfc_expand_constructor (gfc_expr *e, bool fatal)
{
  expand_info expand_save;
  gfc_expr *f;
  bool rc;

  /* If we can successfully get an array element at the max array size then
     the array is too big to expand, so we just return.  */
  f = gfc_get_array_element (e, flag_max_array_constructor);
  if (f != NULL)
    {
      gfc_free_expr (f);
      if (fatal)
	{
	  gfc_error ("The number of elements in the array constructor "
		     "at %L requires an increase of the allowed %d "
		     "upper limit.   See %<-fmax-array-constructor%> "
		     "option", &e->where, flag_max_array_constructor);
	  return false;
	}
      return true;
    }

  /* We now know the array is not too big so go ahead and try to expand it.  */
  expand_save = current_expand;
  current_expand.base = NULL;

  iter_stack = NULL;

  current_expand.expand_work_function = expand;

  if (!expand_constructor (e->value.constructor))
    {
      gfc_constructor_free (current_expand.base);
      rc = false;
      goto done;
    }

  gfc_constructor_free (e->value.constructor);
  e->value.constructor = current_expand.base;

  rc = true;

done:
  current_expand = expand_save;

  return rc;
}


/* Work function for checking that an element of a constructor is a
   constant, after removal of any iteration variables.  We return
   false if not so.  */

static bool
is_constant_element (gfc_expr *e)
{
  int rv;

  rv = gfc_is_constant_expr (e);
  gfc_free_expr (e);

  return rv ? true : false;
}


/* Given an array constructor, determine if the constructor is
   constant or not by expanding it and making sure that all elements
   are constants.  This is a bit of a hack since something like (/ (i,
   i=1,100000000) /) will take a while as* opposed to a more clever
   function that traverses the expression tree. FIXME.  */

int
gfc_constant_ac (gfc_expr *e)
{
  expand_info expand_save;
  bool rc;

  iter_stack = NULL;
  expand_save = current_expand;
  current_expand.expand_work_function = is_constant_element;

  rc = expand_constructor (e->value.constructor);

  current_expand = expand_save;
  if (!rc)
    return 0;

  return 1;
}


/* Returns nonzero if an array constructor has been completely
   expanded (no iterators) and zero if iterators are present.  */

int
gfc_expanded_ac (gfc_expr *e)
{
  gfc_constructor *c;

  if (e->expr_type == EXPR_ARRAY)
    for (c = gfc_constructor_first (e->value.constructor);
	 c; c = gfc_constructor_next (c))
      if (c->iterator != NULL || !gfc_expanded_ac (c->expr))
	return 0;

  return 1;
}


/*************** Type resolution of array constructors ***************/


/* The symbol expr_is_sought_symbol_ref will try to find.  */
static const gfc_symbol *sought_symbol = NULL;


/* Tells whether the expression E is a variable reference to the symbol
   in the static variable SOUGHT_SYMBOL, and sets the locus pointer WHERE
   accordingly.
   To be used with gfc_expr_walker: if a reference is found we don't need
   to look further so we return 1 to skip any further walk.  */

static int
expr_is_sought_symbol_ref (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
			   void *where)
{
  gfc_expr *expr = *e;
  locus *sym_loc = (locus *)where;

  if (expr->expr_type == EXPR_VARIABLE
      && expr->symtree->n.sym == sought_symbol)
    {
      *sym_loc = expr->where;
      return 1;
    }

  return 0;
}


/* Tells whether the expression EXPR contains a reference to the symbol
   SYM and in that case sets the position SYM_LOC where the reference is.  */

static bool
find_symbol_in_expr (gfc_symbol *sym, gfc_expr *expr, locus *sym_loc)
{
  int ret;

  sought_symbol = sym;
  ret = gfc_expr_walker (&expr, &expr_is_sought_symbol_ref, sym_loc);
  sought_symbol = NULL;
  return ret;
}


/* Recursive array list resolution function.  All of the elements must
   be of the same type.  */

static bool
resolve_array_list (gfc_constructor_base base)
{
  bool t;
  gfc_constructor *c;
  gfc_iterator *iter;

  t = true;

  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    {
      iter = c->iterator;
      if (iter != NULL)
        {
	  gfc_symbol *iter_var;
	  locus iter_var_loc;

	  if (!gfc_resolve_iterator (iter, false, true))
	    t = false;

	  /* Check for bounds referencing the iterator variable.  */
	  gcc_assert (iter->var->expr_type == EXPR_VARIABLE);
	  iter_var = iter->var->symtree->n.sym;
	  if (find_symbol_in_expr (iter_var, iter->start, &iter_var_loc))
	    {
	      if (!gfc_notify_std (GFC_STD_LEGACY, "AC-IMPLIED-DO initial "
				   "expression references control variable "
				   "at %L", &iter_var_loc))
	       t = false;
	    }
	  if (find_symbol_in_expr (iter_var, iter->end, &iter_var_loc))
	    {
	      if (!gfc_notify_std (GFC_STD_LEGACY, "AC-IMPLIED-DO final "
				   "expression references control variable "
				   "at %L", &iter_var_loc))
	       t = false;
	    }
	  if (find_symbol_in_expr (iter_var, iter->step, &iter_var_loc))
	    {
	      if (!gfc_notify_std (GFC_STD_LEGACY, "AC-IMPLIED-DO step "
				   "expression references control variable "
				   "at %L", &iter_var_loc))
	       t = false;
	    }
	}

      if (!gfc_resolve_expr (c->expr))
	t = false;

      if (UNLIMITED_POLY (c->expr))
	{
	  gfc_error ("Array constructor value at %L shall not be unlimited "
		     "polymorphic [F2008: C4106]", &c->expr->where);
	  t = false;
	}
    }

  return t;
}

/* Resolve character array constructor. If it has a specified constant character
   length, pad/truncate the elements here; if the length is not specified and
   all elements are of compile-time known length, emit an error as this is
   invalid.  */

bool
gfc_resolve_character_array_constructor (gfc_expr *expr)
{
  gfc_constructor *p;
  int found_length;

  gcc_assert (expr->expr_type == EXPR_ARRAY);
  gcc_assert (expr->ts.type == BT_CHARACTER);

  if (expr->ts.u.cl == NULL)
    {
      for (p = gfc_constructor_first (expr->value.constructor);
	   p; p = gfc_constructor_next (p))
	if (p->expr->ts.u.cl != NULL)
	  {
	    /* Ensure that if there is a char_len around that it is
	       used; otherwise the middle-end confuses them!  */
	    expr->ts.u.cl = p->expr->ts.u.cl;
	    goto got_charlen;
	  }

      expr->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
    }

got_charlen:

  found_length = -1;

  if (expr->ts.u.cl->length == NULL)
    {
      /* Check that all constant string elements have the same length until
	 we reach the end or find a variable-length one.  */

      for (p = gfc_constructor_first (expr->value.constructor);
	   p; p = gfc_constructor_next (p))
	{
	  int current_length = -1;
	  gfc_ref *ref;
	  for (ref = p->expr->ref; ref; ref = ref->next)
	    if (ref->type == REF_SUBSTRING
		&& ref->u.ss.start->expr_type == EXPR_CONSTANT
		&& ref->u.ss.end->expr_type == EXPR_CONSTANT)
	      break;

	  if (p->expr->expr_type == EXPR_CONSTANT)
	    current_length = p->expr->value.character.length;
	  else if (ref)
	    {
	      long j;
	      j = mpz_get_ui (ref->u.ss.end->value.integer)
		- mpz_get_ui (ref->u.ss.start->value.integer) + 1;
	      current_length = (int) j;
	    }
	  else if (p->expr->ts.u.cl && p->expr->ts.u.cl->length
		   && p->expr->ts.u.cl->length->expr_type == EXPR_CONSTANT)
	    {
	      long j;
	      j = mpz_get_si (p->expr->ts.u.cl->length->value.integer);
	      current_length = (int) j;
	    }
	  else
	    return true;

	  gcc_assert (current_length != -1);

	  if (found_length == -1)
	    found_length = current_length;
	  else if (found_length != current_length)
	    {
	      gfc_error ("Different CHARACTER lengths (%d/%d) in array"
			 " constructor at %L", found_length, current_length,
			 &p->expr->where);
	      return false;
	    }

	  gcc_assert (found_length == current_length);
	}

      gcc_assert (found_length != -1);

      /* Update the character length of the array constructor.  */
      expr->ts.u.cl->length = gfc_get_int_expr (gfc_default_integer_kind,
						NULL, found_length);
    }
  else
    {
      /* We've got a character length specified.  It should be an integer,
	 otherwise an error is signalled elsewhere.  */
      gcc_assert (expr->ts.u.cl->length);

      /* If we've got a constant character length, pad according to this.
	 gfc_extract_int does check for BT_INTEGER and EXPR_CONSTANT and sets
	 max_length only if they pass.  */
      gfc_extract_int (expr->ts.u.cl->length, &found_length);

      /* Now pad/truncate the elements accordingly to the specified character
	 length.  This is ok inside this conditional, as in the case above
	 (without typespec) all elements are verified to have the same length
	 anyway.  */
      if (found_length != -1)
	for (p = gfc_constructor_first (expr->value.constructor);
	     p; p = gfc_constructor_next (p))
	  if (p->expr->expr_type == EXPR_CONSTANT)
	    {
	      gfc_expr *cl = NULL;
	      int current_length = -1;
	      bool has_ts;

	      if (p->expr->ts.u.cl && p->expr->ts.u.cl->length)
	      {
		cl = p->expr->ts.u.cl->length;
		gfc_extract_int (cl, &current_length);
	      }

	      /* If gfc_extract_int above set current_length, we implicitly
		 know the type is BT_INTEGER and it's EXPR_CONSTANT.  */

	      has_ts = expr->ts.u.cl->length_from_typespec;

	      if (! cl
		  || (current_length != -1 && current_length != found_length))
		gfc_set_constant_character_len (found_length, p->expr,
						has_ts ? -1 : found_length);
	    }
    }

  return true;
}


/* Resolve all of the expressions in an array list.  */

bool
gfc_resolve_array_constructor (gfc_expr *expr)
{
  bool t;

  t = resolve_array_list (expr->value.constructor);
  if (t)
    t = gfc_check_constructor_type (expr);

  /* gfc_resolve_character_array_constructor is called in gfc_resolve_expr after
     the call to this function, so we don't need to call it here; if it was
     called twice, an error message there would be duplicated.  */

  return t;
}


/* Copy an iterator structure.  */

gfc_iterator *
gfc_copy_iterator (gfc_iterator *src)
{
  gfc_iterator *dest;

  if (src == NULL)
    return NULL;

  dest = gfc_get_iterator ();

  dest->var = gfc_copy_expr (src->var);
  dest->start = gfc_copy_expr (src->start);
  dest->end = gfc_copy_expr (src->end);
  dest->step = gfc_copy_expr (src->step);

  return dest;
}


/********* Subroutines for determining the size of an array *********/

/* These are needed just to accommodate RESHAPE().  There are no
   diagnostics here, we just return a negative number if something
   goes wrong.  */


/* Get the size of single dimension of an array specification.  The
   array is guaranteed to be one dimensional.  */

bool
spec_dimen_size (gfc_array_spec *as, int dimen, mpz_t *result)
{
  if (as == NULL)
    return false;

  if (dimen < 0 || dimen > as->rank - 1)
    gfc_internal_error ("spec_dimen_size(): Bad dimension");

  if (as->type != AS_EXPLICIT
      || as->lower[dimen]->expr_type != EXPR_CONSTANT
      || as->upper[dimen]->expr_type != EXPR_CONSTANT
      || as->lower[dimen]->ts.type != BT_INTEGER
      || as->upper[dimen]->ts.type != BT_INTEGER)
    return false;

  mpz_init (*result);

  mpz_sub (*result, as->upper[dimen]->value.integer,
	   as->lower[dimen]->value.integer);

  mpz_add_ui (*result, *result, 1);

  return true;
}


bool
spec_size (gfc_array_spec *as, mpz_t *result)
{
  mpz_t size;
  int d;

  if (!as || as->type == AS_ASSUMED_RANK)
    return false;

  mpz_init_set_ui (*result, 1);

  for (d = 0; d < as->rank; d++)
    {
      if (!spec_dimen_size (as, d, &size))
	{
	  mpz_clear (*result);
	  return false;
	}

      mpz_mul (*result, *result, size);
      mpz_clear (size);
    }

  return true;
}


/* Get the number of elements in an array section. Optionally, also supply
   the end value.  */

bool
gfc_ref_dimen_size (gfc_array_ref *ar, int dimen, mpz_t *result, mpz_t *end)
{
  mpz_t upper, lower, stride;
  mpz_t diff;
  bool t;

  if (dimen < 0 || ar == NULL || dimen > ar->dimen - 1)
    gfc_internal_error ("gfc_ref_dimen_size(): Bad dimension");

  switch (ar->dimen_type[dimen])
    {
    case DIMEN_ELEMENT:
      mpz_init (*result);
      mpz_set_ui (*result, 1);
      t = true;
      break;

    case DIMEN_VECTOR:
      t = gfc_array_size (ar->start[dimen], result);	/* Recurse! */
      break;

    case DIMEN_RANGE:

      mpz_init (stride);

      if (ar->stride[dimen] == NULL)
	mpz_set_ui (stride, 1);
      else
	{
	  if (ar->stride[dimen]->expr_type != EXPR_CONSTANT)
	    {
	      mpz_clear (stride);
	      return false;
	    }
	  mpz_set (stride, ar->stride[dimen]->value.integer);
	}

      /* Calculate the number of elements via gfc_dep_differce, but only if
	 start and end are both supplied in the reference or the array spec.
	 This is to guard against strange but valid code like

	 subroutine foo(a,n)
	 real a(1:n)
	 n = 3
	 print *,size(a(n-1:))

	 where the user changes the value of a variable.  If we have to
	 determine end as well, we cannot do this using gfc_dep_difference.
	 Fall back to the constants-only code then.  */

      if (end == NULL)
	{
	  bool use_dep;

	  use_dep = gfc_dep_difference (ar->end[dimen], ar->start[dimen],
					&diff);
	  if (!use_dep && ar->end[dimen] == NULL && ar->start[dimen] == NULL)
	    use_dep = gfc_dep_difference (ar->as->upper[dimen],
					    ar->as->lower[dimen], &diff);

	  if (use_dep)
	    {
	      mpz_init (*result);
	      mpz_add (*result, diff, stride);
	      mpz_div (*result, *result, stride);
	      if (mpz_cmp_ui (*result, 0) < 0)
		mpz_set_ui (*result, 0);

	      mpz_clear (stride);
	      mpz_clear (diff);
	      return true;
	    }

	}

      /*  Constant-only code here, which covers more cases
	  like a(:4) etc.  */
      mpz_init (upper);
      mpz_init (lower);
      t = false;

      if (ar->start[dimen] == NULL)
	{
	  if (ar->as->lower[dimen] == NULL
	      || ar->as->lower[dimen]->expr_type != EXPR_CONSTANT
	      || ar->as->lower[dimen]->ts.type != BT_INTEGER)
	    goto cleanup;
	  mpz_set (lower, ar->as->lower[dimen]->value.integer);
	}
      else
	{
	  if (ar->start[dimen]->expr_type != EXPR_CONSTANT)
	    goto cleanup;
	  mpz_set (lower, ar->start[dimen]->value.integer);
	}

      if (ar->end[dimen] == NULL)
	{
	  if (ar->as->upper[dimen] == NULL
	      || ar->as->upper[dimen]->expr_type != EXPR_CONSTANT
	      || ar->as->upper[dimen]->ts.type != BT_INTEGER)
	    goto cleanup;
	  mpz_set (upper, ar->as->upper[dimen]->value.integer);
	}
      else
	{
	  if (ar->end[dimen]->expr_type != EXPR_CONSTANT)
	    goto cleanup;
	  mpz_set (upper, ar->end[dimen]->value.integer);
	}

      mpz_init (*result);
      mpz_sub (*result, upper, lower);
      mpz_add (*result, *result, stride);
      mpz_div (*result, *result, stride);

      /* Zero stride caught earlier.  */
      if (mpz_cmp_ui (*result, 0) < 0)
	mpz_set_ui (*result, 0);
      t = true;

      if (end)
	{
	  mpz_init (*end);

	  mpz_sub_ui (*end, *result, 1UL);
	  mpz_mul (*end, *end, stride);
	  mpz_add (*end, *end, lower);
	}

    cleanup:
      mpz_clear (upper);
      mpz_clear (lower);
      mpz_clear (stride);
      return t;

    default:
      gfc_internal_error ("gfc_ref_dimen_size(): Bad dimen_type");
    }

  return t;
}


static bool
ref_size (gfc_array_ref *ar, mpz_t *result)
{
  mpz_t size;
  int d;

  mpz_init_set_ui (*result, 1);

  for (d = 0; d < ar->dimen; d++)
    {
      if (!gfc_ref_dimen_size (ar, d, &size, NULL))
	{
	  mpz_clear (*result);
	  return false;
	}

      mpz_mul (*result, *result, size);
      mpz_clear (size);
    }

  return true;
}


/* Given an array expression and a dimension, figure out how many
   elements it has along that dimension.  Returns true if we were
   able to return a result in the 'result' variable, false
   otherwise.  */

bool
gfc_array_dimen_size (gfc_expr *array, int dimen, mpz_t *result)
{
  gfc_ref *ref;
  int i;

  gcc_assert (array != NULL);

  if (array->ts.type == BT_CLASS)
    return false;

  if (array->rank == -1)
    return false;

  if (dimen < 0 || dimen > array->rank - 1)
    gfc_internal_error ("gfc_array_dimen_size(): Bad dimension");

  switch (array->expr_type)
    {
    case EXPR_VARIABLE:
    case EXPR_FUNCTION:
      for (ref = array->ref; ref; ref = ref->next)
	{
	  if (ref->type != REF_ARRAY)
	    continue;

	  if (ref->u.ar.type == AR_FULL)
	    return spec_dimen_size (ref->u.ar.as, dimen, result);

	  if (ref->u.ar.type == AR_SECTION)
	    {
	      for (i = 0; dimen >= 0; i++)
		if (ref->u.ar.dimen_type[i] != DIMEN_ELEMENT)
		  dimen--;

	      return gfc_ref_dimen_size (&ref->u.ar, i - 1, result, NULL);
	    }
	}

      if (array->shape && array->shape[dimen])
	{
	  mpz_init_set (*result, array->shape[dimen]);
	  return true;
	}

      if (array->symtree->n.sym->attr.generic
	  && array->value.function.esym != NULL)
	{
	  if (!spec_dimen_size (array->value.function.esym->as, dimen, result))
	    return false;
	}
      else if (!spec_dimen_size (array->symtree->n.sym->as, dimen, result))
	return false;

      break;

    case EXPR_ARRAY:
      if (array->shape == NULL) {
	/* Expressions with rank > 1 should have "shape" properly set */
	if ( array->rank != 1 )
	  gfc_internal_error ("gfc_array_dimen_size(): Bad EXPR_ARRAY expr");
	return gfc_array_size(array, result);
      }

      /* Fall through */
    default:
      if (array->shape == NULL)
	return false;

      mpz_init_set (*result, array->shape[dimen]);

      break;
    }

  return true;
}


/* Given an array expression, figure out how many elements are in the
   array.  Returns true if this is possible, and sets the 'result'
   variable.  Otherwise returns false.  */

bool
gfc_array_size (gfc_expr *array, mpz_t *result)
{
  expand_info expand_save;
  gfc_ref *ref;
  int i;
  bool t;

  if (array->ts.type == BT_CLASS)
    return false;

  switch (array->expr_type)
    {
    case EXPR_ARRAY:
      gfc_push_suppress_errors ();

      expand_save = current_expand;

      current_expand.count = result;
      mpz_init_set_ui (*result, 0);

      current_expand.expand_work_function = count_elements;
      iter_stack = NULL;

      t = expand_constructor (array->value.constructor);

      gfc_pop_suppress_errors ();

      if (!t)
	mpz_clear (*result);
      current_expand = expand_save;
      return t;

    case EXPR_VARIABLE:
      for (ref = array->ref; ref; ref = ref->next)
	{
	  if (ref->type != REF_ARRAY)
	    continue;

	  if (ref->u.ar.type == AR_FULL)
	    return spec_size (ref->u.ar.as, result);

	  if (ref->u.ar.type == AR_SECTION)
	    return ref_size (&ref->u.ar, result);
	}

      return spec_size (array->symtree->n.sym->as, result);


    default:
      if (array->rank == 0 || array->shape == NULL)
	return false;

      mpz_init_set_ui (*result, 1);

      for (i = 0; i < array->rank; i++)
	mpz_mul (*result, *result, array->shape[i]);

      break;
    }

  return true;
}


/* Given an array reference, return the shape of the reference in an
   array of mpz_t integers.  */

bool
gfc_array_ref_shape (gfc_array_ref *ar, mpz_t *shape)
{
  int d;
  int i;

  d = 0;

  switch (ar->type)
    {
    case AR_FULL:
      for (; d < ar->as->rank; d++)
	if (!spec_dimen_size (ar->as, d, &shape[d]))
	  goto cleanup;

      return true;

    case AR_SECTION:
      for (i = 0; i < ar->dimen; i++)
	{
	  if (ar->dimen_type[i] != DIMEN_ELEMENT)
	    {
	      if (!gfc_ref_dimen_size (ar, i, &shape[d], NULL))
		goto cleanup;
	      d++;
	    }
	}

      return true;

    default:
      break;
    }

cleanup:
  gfc_clear_shape (shape, d);
  return false;
}


/* Given an array expression, find the array reference structure that
   characterizes the reference.  */

gfc_array_ref *
gfc_find_array_ref (gfc_expr *e)
{
  gfc_ref *ref;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY
	&& (ref->u.ar.type == AR_FULL || ref->u.ar.type == AR_SECTION))
      break;

  if (ref == NULL)
    gfc_internal_error ("gfc_find_array_ref(): No ref found");

  return &ref->u.ar;
}


/* Find out if an array shape is known at compile time.  */

int
gfc_is_compile_time_shape (gfc_array_spec *as)
{
  int i;

  if (as->type != AS_EXPLICIT)
    return 0;

  for (i = 0; i < as->rank; i++)
    if (!gfc_is_constant_expr (as->lower[i])
	|| !gfc_is_constant_expr (as->upper[i]))
      return 0;

  return 1;
}
