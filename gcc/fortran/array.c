/* Array things
   Copyright (C) 2000, 2001, 2002, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "match.h"

/* This parameter is the size of the largest array constructor that we
   will expand to an array constructor without iterators.
   Constructors larger than this will remain in the iterator form.  */

#define GFC_MAX_AC_EXPAND 65535


/**************** Array reference matching subroutines *****************/

/* Copy an array reference structure.  */

gfc_array_ref *
gfc_copy_array_ref (gfc_array_ref * src)
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

  dest->offset = gfc_copy_expr (src->offset);

  return dest;
}


/* Match a single dimension of an array reference.  This can be a
   single element or an array section.  Any modifications we've made
   to the ar structure are cleaned up by the caller.  If the init
   is set, we require the subscript to be a valid initialization
   expression.  */

static match
match_subscript (gfc_array_ref * ar, int init)
{
  match m;
  int i;

  i = ar->dimen;

  ar->c_where[i] = gfc_current_locus;
  ar->start[i] = ar->end[i] = ar->stride[i] = NULL;

  /* We can't be sure of the difference between DIMEN_ELEMENT and
     DIMEN_VECTOR until we know the type of the element itself at
     resolution time.  */

  ar->dimen_type[i] = DIMEN_UNKNOWN;

  if (gfc_match_char (':') == MATCH_YES)
    goto end_element;

  /* Get start element.  */
  if (init)
    m = gfc_match_init_expr (&ar->start[i]);
  else
    m = gfc_match_expr (&ar->start[i]);

  if (m == MATCH_NO)
    gfc_error ("Expected array subscript at %C");
  if (m != MATCH_YES)
    return MATCH_ERROR;

  if (gfc_match_char (':') == MATCH_NO)
    return MATCH_YES;

  /* Get an optional end element.  Because we've seen the colon, we
     definitely have a range along this dimension.  */
end_element:
  ar->dimen_type[i] = DIMEN_RANGE;

  if (init)
    m = gfc_match_init_expr (&ar->end[i]);
  else
    m = gfc_match_expr (&ar->end[i]);

  if (m == MATCH_ERROR)
    return MATCH_ERROR;

  /* See if we have an optional stride.  */
  if (gfc_match_char (':') == MATCH_YES)
    {
      m = init ? gfc_match_init_expr (&ar->stride[i])
	: gfc_match_expr (&ar->stride[i]);

      if (m == MATCH_NO)
	gfc_error ("Expected array subscript stride at %C");
      if (m != MATCH_YES)
	return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* Match an array reference, whether it is the whole array or a
   particular elements or a section. If init is set, the reference has
   to consist of init expressions.  */

match
gfc_match_array_ref (gfc_array_ref * ar, gfc_array_spec * as, int init)
{
  match m;

  memset (ar, '\0', sizeof (ar));

  ar->where = gfc_current_locus;
  ar->as = as;

  if (gfc_match_char ('(') != MATCH_YES)
    {
      ar->type = AR_FULL;
      ar->dimen = 0;
      return MATCH_YES;
    }

  ar->type = AR_UNKNOWN;

  for (ar->dimen = 0; ar->dimen < GFC_MAX_DIMENSIONS; ar->dimen++)
    {
      m = match_subscript (ar, init);
      if (m == MATCH_ERROR)
	goto error;

      if (gfc_match_char (')') == MATCH_YES)
	goto matched;

      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Invalid form of array reference at %C");
	  goto error;
	}
    }

  gfc_error ("Array reference at %C cannot have more than %d dimensions",
	     GFC_MAX_DIMENSIONS);

error:
  return MATCH_ERROR;

matched:
  ar->dimen++;

  return MATCH_YES;
}


/************** Array specification matching subroutines ***************/

/* Free all of the expressions associated with array bounds
   specifications.  */

void
gfc_free_array_spec (gfc_array_spec * as)
{
  int i;

  if (as == NULL)
    return;

  for (i = 0; i < as->rank; i++)
    {
      gfc_free_expr (as->lower[i]);
      gfc_free_expr (as->upper[i]);
    }

  gfc_free (as);
}


/* Take an array bound, resolves the expression, that make up the
   shape and check associated constraints.  */

static try
resolve_array_bound (gfc_expr * e, int check_constant)
{

  if (e == NULL)
    return SUCCESS;

  if (gfc_resolve_expr (e) == FAILURE
      || gfc_specification_expr (e) == FAILURE)
    return FAILURE;

  if (check_constant && gfc_is_constant_expr (e) == 0)
    {
      gfc_error ("Variable '%s' at %L in this context must be constant",
		 e->symtree->n.sym->name, &e->where);
      return FAILURE;
    }

  return SUCCESS;
}


/* Takes an array specification, resolves the expressions that make up
   the shape and make sure everything is integral.  */

try
gfc_resolve_array_spec (gfc_array_spec * as, int check_constant)
{
  gfc_expr *e;
  int i;

  if (as == NULL)
    return SUCCESS;

  for (i = 0; i < as->rank; i++)
    {
      e = as->lower[i];
      if (resolve_array_bound (e, check_constant) == FAILURE)
	return FAILURE;

      e = as->upper[i];
      if (resolve_array_bound (e, check_constant) == FAILURE)
	return FAILURE;
    }

  return SUCCESS;
}


/* Match a single array element specification.  The return values as
   well as the upper and lower bounds of the array spec are filled
   in according to what we see on the input.  The caller makes sure
   individual specifications make sense as a whole.


        Parsed       Lower   Upper  Returned
        ------------------------------------
          :          NULL    NULL   AS_DEFERRED (*)
          x           1       x     AS_EXPLICIT
          x:          x      NULL   AS_ASSUMED_SHAPE
          x:y         x       y     AS_EXPLICIT
          x:*         x      NULL   AS_ASSUMED_SIZE
          *           1      NULL   AS_ASSUMED_SIZE

  (*) For non-pointer dummy arrays this is AS_ASSUMED_SHAPE.  This
  is fixed during the resolution of formal interfaces.

   Anything else AS_UNKNOWN.  */

static array_type
match_array_element_spec (gfc_array_spec * as)
{
  gfc_expr **upper, **lower;
  match m;

  lower = &as->lower[as->rank - 1];
  upper = &as->upper[as->rank - 1];

  if (gfc_match_char ('*') == MATCH_YES)
    {
      *lower = gfc_int_expr (1);
      return AS_ASSUMED_SIZE;
    }

  if (gfc_match_char (':') == MATCH_YES)
    return AS_DEFERRED;

  m = gfc_match_expr (upper);
  if (m == MATCH_NO)
    gfc_error ("Expected expression in array specification at %C");
  if (m != MATCH_YES)
    return AS_UNKNOWN;

  if (gfc_match_char (':') == MATCH_NO)
    {
      *lower = gfc_int_expr (1);
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

  return AS_EXPLICIT;
}


/* Matches an array specification, incidentally figuring out what sort
   it is.  */

match
gfc_match_array_spec (gfc_array_spec ** asp)
{
  array_type current_type;
  gfc_array_spec *as;
  int i;

  if (gfc_match_char ('(') != MATCH_YES)
    {
      *asp = NULL;
      return MATCH_NO;
    }

  as = gfc_get_array_spec ();

  for (i = 0; i < GFC_MAX_DIMENSIONS; i++)
    {
      as->lower[i] = NULL;
      as->upper[i] = NULL;
    }

  as->rank = 1;

  for (;;)
    {
      current_type = match_array_element_spec (as);

      if (as->rank == 1)
	{
	  if (current_type == AS_UNKNOWN)
	    goto cleanup;
	  as->type = current_type;
	}
      else
	switch (as->type)
	  {			/* See how current spec meshes with the existing */
	  case AS_UNKNOWN:
	    goto cleanup;

	  case AS_EXPLICIT:
	    if (current_type == AS_ASSUMED_SIZE)
	      {
		as->type = AS_ASSUMED_SIZE;
		break;
	      }

	    if (current_type == AS_EXPLICIT)
	      break;

	    gfc_error
	      ("Bad array specification for an explicitly shaped array"
	       " at %C");

	    goto cleanup;

	  case AS_ASSUMED_SHAPE:
	    if ((current_type == AS_ASSUMED_SHAPE)
		|| (current_type == AS_DEFERRED))
	      break;

	    gfc_error
	      ("Bad array specification for assumed shape array at %C");
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
	    gfc_error ("Bad specification for assumed size array at %C");
	    goto cleanup;
	  }

      if (gfc_match_char (')') == MATCH_YES)
	break;

      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Expected another dimension in array declaration at %C");
	  goto cleanup;
	}

      if (as->rank >= GFC_MAX_DIMENSIONS)
	{
	  gfc_error ("Array specification at %C has more than %d dimensions",
		     GFC_MAX_DIMENSIONS);
	  goto cleanup;
	}

      as->rank++;
    }

  /* If a lower bounds of an assumed shape array is blank, put in one.  */
  if (as->type == AS_ASSUMED_SHAPE)
    {
      for (i = 0; i < as->rank; i++)
	{
	  if (as->lower[i] == NULL)
	    as->lower[i] = gfc_int_expr (1);
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

try
gfc_set_array_spec (gfc_symbol * sym, gfc_array_spec * as, locus * error_loc)
{

  if (as == NULL)
    return SUCCESS;

  if (gfc_add_dimension (&sym->attr, sym->name, error_loc) == FAILURE)
    return FAILURE;

  sym->as = as;

  return SUCCESS;
}


/* Copy an array specification.  */

gfc_array_spec *
gfc_copy_array_spec (gfc_array_spec * src)
{
  gfc_array_spec *dest;
  int i;

  if (src == NULL)
    return NULL;

  dest = gfc_get_array_spec ();

  *dest = *src;

  for (i = 0; i < dest->rank; i++)
    {
      dest->lower[i] = gfc_copy_expr (dest->lower[i]);
      dest->upper[i] = gfc_copy_expr (dest->upper[i]);
    }

  return dest;
}

/* Returns nonzero if the two expressions are equal.  Only handles integer
   constants.  */

static int
compare_bounds (gfc_expr * bound1, gfc_expr * bound2)
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
gfc_compare_array_spec (gfc_array_spec * as1, gfc_array_spec * as2)
{
  int i;

  if (as1 == NULL && as2 == NULL)
    return 1;

  if (as1 == NULL || as2 == NULL)
    return 0;

  if (as1->rank != as2->rank)
    return 0;

  if (as1->rank == 0)
    return 1;

  if (as1->type != as2->type)
    return 0;

  if (as1->type == AS_EXPLICIT)
    for (i = 0; i < as1->rank; i++)
      {
	if (compare_bounds (as1->lower[i], as2->lower[i]) == 0)
	  return 0;

	if (compare_bounds (as1->upper[i], as2->upper[i]) == 0)
	  return 0;
      }

  return 1;
}


/****************** Array constructor functions ******************/

/* Start an array constructor.  The constructor starts with zero
   elements and should be appended to by gfc_append_constructor().  */

gfc_expr *
gfc_start_constructor (bt type, int kind, locus * where)
{
  gfc_expr *result;

  result = gfc_get_expr ();

  result->expr_type = EXPR_ARRAY;
  result->rank = 1;

  result->ts.type = type;
  result->ts.kind = kind;
  result->where = *where;
  return result;
}


/* Given an array constructor expression, append the new expression
   node onto the constructor.  */

void
gfc_append_constructor (gfc_expr * base, gfc_expr * new)
{
  gfc_constructor *c;

  if (base->value.constructor == NULL)
    base->value.constructor = c = gfc_get_constructor ();
  else
    {
      c = base->value.constructor;
      while (c->next)
	c = c->next;

      c->next = gfc_get_constructor ();
      c = c->next;
    }

  c->expr = new;

  if (new->ts.type != base->ts.type || new->ts.kind != base->ts.kind)
    gfc_internal_error ("gfc_append_constructor(): New node has wrong kind");
}


/* Given an array constructor expression, insert the new expression's
   constructor onto the base's one according to the offset.  */

void
gfc_insert_constructor (gfc_expr * base, gfc_constructor * c1)
{
  gfc_constructor *c, *pre;
  expr_t type;
  int t;

  type = base->expr_type;

  if (base->value.constructor == NULL)
    base->value.constructor = c1;
  else
    {
      c = pre = base->value.constructor;
      while (c)
        {
          if (type == EXPR_ARRAY)
            {
	      t = mpz_cmp (c->n.offset, c1->n.offset);
              if (t < 0)
                {
                  pre = c;
                  c = c->next;
                }
              else if (t == 0)
                {
                  gfc_error ("duplicated initializer");
                  break;
                }
              else
                break;
            }
          else
            {
              pre = c;
              c = c->next;
            }
        }

      if (pre != c)
        {
          pre->next = c1;
          c1->next = c;
        }
      else
        {
          c1->next = c;
          base->value.constructor = c1;
        }
    }
}


/* Get a new constructor.  */

gfc_constructor *
gfc_get_constructor (void)
{
  gfc_constructor *c;

  c = gfc_getmem (sizeof(gfc_constructor));
  c->expr = NULL;
  c->iterator = NULL;
  c->next = NULL;
  mpz_init_set_si (c->n.offset, 0);
  mpz_init_set_si (c->repeat, 0);
  return c;
}


/* Free chains of gfc_constructor structures.  */

void
gfc_free_constructor (gfc_constructor * p)
{
  gfc_constructor *next;

  if (p == NULL)
    return;

  for (; p; p = next)
    {
      next = p->next;

      if (p->expr)
        gfc_free_expr (p->expr);
      if (p->iterator != NULL)
	gfc_free_iterator (p->iterator, 1);
      mpz_clear (p->n.offset);
      mpz_clear (p->repeat);
      gfc_free (p);
    }
}


/* Given an expression node that might be an array constructor and a
   symbol, make sure that no iterators in this or child constructors
   use the symbol as an implied-DO iterator.  Returns nonzero if a
   duplicate was found.  */

static int
check_duplicate_iterator (gfc_constructor * c, gfc_symbol * master)
{
  gfc_expr *e;

  for (; c; c = c->next)
    {
      e = c->expr;

      if (e->expr_type == EXPR_ARRAY
	  && check_duplicate_iterator (e->value.constructor, master))
	return 1;

      if (c->iterator == NULL)
	continue;

      if (c->iterator->var->symtree->n.sym == master)
	{
	  gfc_error
	    ("DO-iterator '%s' at %L is inside iterator of the same name",
	     master->name, &c->where);

	  return 1;
	}
    }

  return 0;
}


/* Forward declaration because these functions are mutually recursive.  */
static match match_array_cons_element (gfc_constructor **);

/* Match a list of array elements.  */

static match
match_array_list (gfc_constructor ** result)
{
  gfc_constructor *p, *head, *tail, *new;
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

  tail = head;

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

      m = match_array_cons_element (&new);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	{
	  if (n > 2)
	    goto syntax;
	  m = MATCH_NO;
	  goto cleanup;		/* Could be a complex constant */
	}

      tail->next = new;
      tail = new;

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

  e = gfc_get_expr ();
  e->expr_type = EXPR_ARRAY;
  e->where = old_loc;
  e->value.constructor = head;

  p = gfc_get_constructor ();
  p->where = gfc_current_locus;
  p->iterator = gfc_get_iterator ();
  *p->iterator = iter;

  p->expr = e;
  *result = p;

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in array constructor at %C");
  m = MATCH_ERROR;

cleanup:
  gfc_free_constructor (head);
  gfc_free_iterator (&iter, 0);
  gfc_current_locus = old_loc;
  return m;
}


/* Match a single element of an array constructor, which can be a
   single expression or a list of elements.  */

static match
match_array_cons_element (gfc_constructor ** result)
{
  gfc_constructor *p;
  gfc_expr *expr;
  match m;

  m = match_array_list (result);
  if (m != MATCH_NO)
    return m;

  m = gfc_match_expr (&expr);
  if (m != MATCH_YES)
    return m;

  p = gfc_get_constructor ();
  p->where = gfc_current_locus;
  p->expr = expr;

  *result = p;
  return MATCH_YES;
}


/* Match an array constructor.  */

match
gfc_match_array_constructor (gfc_expr ** result)
{
  gfc_constructor *head, *tail, *new;
  gfc_expr *expr;
  locus where;
  match m;
  const char *end_delim;

  if (gfc_match (" (/") == MATCH_NO)
    {
      if (gfc_match (" [") == MATCH_NO)
        return MATCH_NO;
      else
        {
          if (gfc_notify_std (GFC_STD_F2003, "New in Fortran 2003: [...] "
                              "style array constructors at %C") == FAILURE)
            return MATCH_ERROR;
          end_delim = " ]";
        }
    }
  else
    end_delim = " /)";

  where = gfc_current_locus;
  head = tail = NULL;

  if (gfc_match (end_delim) == MATCH_YES)
    {
      gfc_error ("Empty array constructor at %C is not allowed");
      goto cleanup;
    }

  for (;;)
    {
      m = match_array_cons_element (&new);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      if (head == NULL)
	head = new;
      else
	tail->next = new;

      tail = new;

      if (gfc_match_char (',') == MATCH_NO)
	break;
    }

  if (gfc_match (end_delim) == MATCH_NO)
    goto syntax;

  expr = gfc_get_expr ();

  expr->expr_type = EXPR_ARRAY;

  expr->value.constructor = head;
  /* Size must be calculated at resolution time.  */

  expr->where = where;
  expr->rank = 1;

  *result = expr;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in array constructor at %C");

cleanup:
  gfc_free_constructor (head);
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
check_element_type (gfc_expr * expr)
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

  gfc_error ("Element in %s array constructor at %L is %s",
	     gfc_typename (&constructor_ts), &expr->where,
	     gfc_typename (&expr->ts));

  cons_state = CONS_BAD;
  return 1;
}


/* Recursive work function for gfc_check_constructor_type().  */

static try
check_constructor_type (gfc_constructor * c)
{
  gfc_expr *e;

  for (; c; c = c->next)
    {
      e = c->expr;

      if (e->expr_type == EXPR_ARRAY)
	{
	  if (check_constructor_type (e->value.constructor) == FAILURE)
	    return FAILURE;

	  continue;
	}

      if (check_element_type (e))
	return FAILURE;
    }

  return SUCCESS;
}


/* Check that all elements of an array constructor are the same type.
   On FAILURE, an error has been generated.  */

try
gfc_check_constructor_type (gfc_expr * e)
{
  try t;

  cons_state = CONS_START;
  gfc_clear_ts (&constructor_ts);

  t = check_constructor_type (e->value.constructor);
  if (t == SUCCESS && e->ts.type == BT_UNKNOWN)
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

static try check_constructor (gfc_constructor *, try (*)(gfc_expr *));

/* Check an EXPR_VARIABLE expression in a constructor to make sure
   that that variable is an iteration variables.  */

try
gfc_check_iter_variable (gfc_expr * expr)
{

  gfc_symbol *sym;
  cons_stack *c;

  sym = expr->symtree->n.sym;

  for (c = base; c; c = c->previous)
    if (sym == c->iterator->var->symtree->n.sym)
      return SUCCESS;

  return FAILURE;
}


/* Recursive work function for gfc_check_constructor().  This amounts
   to calling the check function for each expression in the
   constructor, giving variables with the names of iterators a pass.  */

static try
check_constructor (gfc_constructor * c, try (*check_function) (gfc_expr *))
{
  cons_stack element;
  gfc_expr *e;
  try t;

  for (; c; c = c->next)
    {
      e = c->expr;

      if (e->expr_type != EXPR_ARRAY)
	{
	  if ((*check_function) (e) == FAILURE)
	    return FAILURE;
	  continue;
	}

      element.previous = base;
      element.iterator = c->iterator;

      base = &element;
      t = check_constructor (e->value.constructor, check_function);
      base = element.previous;

      if (t == FAILURE)
	return FAILURE;
    }

  /* Nothing went wrong, so all OK.  */
  return SUCCESS;
}


/* Checks a constructor to see if it is a particular kind of
   expression -- specification, restricted, or initialization as
   determined by the check_function.  */

try
gfc_check_constructor (gfc_expr * expr, try (*check_function) (gfc_expr *))
{
  cons_stack *base_save;
  try t;

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
  gfc_constructor *new_head, *new_tail;
  int extract_count, extract_n;
  gfc_expr *extracted;
  mpz_t *count;

  mpz_t *offset;
  gfc_component *component;
  mpz_t *repeat;

  try (*expand_work_function) (gfc_expr *);
}
expand_info;

static expand_info current_expand;

static try expand_constructor (gfc_constructor *);


/* Work function that counts the number of elements present in a
   constructor.  */

static try
count_elements (gfc_expr * e)
{
  mpz_t result;

  if (e->rank == 0)
    mpz_add_ui (*current_expand.count, *current_expand.count, 1);
  else
    {
      if (gfc_array_size (e, &result) == FAILURE)
	{
	  gfc_free_expr (e);
	  return FAILURE;
	}

      mpz_add (*current_expand.count, *current_expand.count, result);
      mpz_clear (result);
    }

  gfc_free_expr (e);
  return SUCCESS;
}


/* Work function that extracts a particular element from an array
   constructor, freeing the rest.  */

static try
extract_element (gfc_expr * e)
{

  if (e->rank != 0)
    {				/* Something unextractable */
      gfc_free_expr (e);
      return FAILURE;
    }

  if (current_expand.extract_count == current_expand.extract_n)
    current_expand.extracted = e;
  else
    gfc_free_expr (e);

  current_expand.extract_count++;
  return SUCCESS;
}


/* Work function that constructs a new constructor out of the old one,
   stringing new elements together.  */

static try
expand (gfc_expr * e)
{

  if (current_expand.new_head == NULL)
    current_expand.new_head = current_expand.new_tail =
      gfc_get_constructor ();
  else
    {
      current_expand.new_tail->next = gfc_get_constructor ();
      current_expand.new_tail = current_expand.new_tail->next;
    }

  current_expand.new_tail->where = e->where;
  current_expand.new_tail->expr = e;

  mpz_set (current_expand.new_tail->n.offset, *current_expand.offset);
  current_expand.new_tail->n.component = current_expand.component;
  mpz_set (current_expand.new_tail->repeat, *current_expand.repeat);
  return SUCCESS;
}


/* Given an initialization expression that is a variable reference,
   substitute the current value of the iteration variable.  */

void
gfc_simplify_iterator_var (gfc_expr * e)
{
  iterator_stack *p;

  for (p = iter_stack; p; p = p->prev)
    if (e->symtree == p->variable)
      break;

  if (p == NULL)
    return;		/* Variable not found */

  gfc_replace_expr (e, gfc_int_expr (0));

  mpz_set (e->value.integer, p->value);

  return;
}


/* Expand an expression with that is inside of a constructor,
   recursing into other constructors if present.  */

static try
expand_expr (gfc_expr * e)
{

  if (e->expr_type == EXPR_ARRAY)
    return expand_constructor (e->value.constructor);

  e = gfc_copy_expr (e);

  if (gfc_simplify_expr (e, 1) == FAILURE)
    {
      gfc_free_expr (e);
      return FAILURE;
    }

  return current_expand.expand_work_function (e);
}


static try
expand_iterator (gfc_constructor * c)
{
  gfc_expr *start, *end, *step;
  iterator_stack frame;
  mpz_t trip;
  try t;

  end = step = NULL;

  t = FAILURE;

  mpz_init (trip);
  mpz_init (frame.value);

  start = gfc_copy_expr (c->iterator->start);
  if (gfc_simplify_expr (start, 1) == FAILURE)
    goto cleanup;

  if (start->expr_type != EXPR_CONSTANT || start->ts.type != BT_INTEGER)
    goto cleanup;

  end = gfc_copy_expr (c->iterator->end);
  if (gfc_simplify_expr (end, 1) == FAILURE)
    goto cleanup;

  if (end->expr_type != EXPR_CONSTANT || end->ts.type != BT_INTEGER)
    goto cleanup;

  step = gfc_copy_expr (c->iterator->step);
  if (gfc_simplify_expr (step, 1) == FAILURE)
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
      if (expand_expr (c->expr) == FAILURE)
	goto cleanup;

      mpz_add (frame.value, frame.value, step->value.integer);
      mpz_sub_ui (trip, trip, 1);
    }

  t = SUCCESS;

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

static try
expand_constructor (gfc_constructor * c)
{
  gfc_expr *e;

  for (; c; c = c->next)
    {
      if (c->iterator != NULL)
	{
	  if (expand_iterator (c) == FAILURE)
	    return FAILURE;
	  continue;
	}

      e = c->expr;

      if (e->expr_type == EXPR_ARRAY)
	{
	  if (expand_constructor (e->value.constructor) == FAILURE)
	    return FAILURE;

	  continue;
	}

      e = gfc_copy_expr (e);
      if (gfc_simplify_expr (e, 1) == FAILURE)
	{
	  gfc_free_expr (e);
	  return FAILURE;
	}
      current_expand.offset = &c->n.offset;
      current_expand.component = c->n.component;
      current_expand.repeat = &c->repeat;
      if (current_expand.expand_work_function (e) == FAILURE)
	return FAILURE;
    }
  return SUCCESS;
}


/* Top level subroutine for expanding constructors.  We only expand
   constructor if they are small enough.  */

try
gfc_expand_constructor (gfc_expr * e)
{
  expand_info expand_save;
  gfc_expr *f;
  try rc;

  f = gfc_get_array_element (e, GFC_MAX_AC_EXPAND);
  if (f != NULL)
    {
      gfc_free_expr (f);
      return SUCCESS;
    }

  expand_save = current_expand;
  current_expand.new_head = current_expand.new_tail = NULL;

  iter_stack = NULL;

  current_expand.expand_work_function = expand;

  if (expand_constructor (e->value.constructor) == FAILURE)
    {
      gfc_free_constructor (current_expand.new_head);
      rc = FAILURE;
      goto done;
    }

  gfc_free_constructor (e->value.constructor);
  e->value.constructor = current_expand.new_head;

  rc = SUCCESS;

done:
  current_expand = expand_save;

  return rc;
}


/* Work function for checking that an element of a constructor is a
   constant, after removal of any iteration variables.  We return
   FAILURE if not so.  */

static try
constant_element (gfc_expr * e)
{
  int rv;

  rv = gfc_is_constant_expr (e);
  gfc_free_expr (e);

  return rv ? SUCCESS : FAILURE;
}


/* Given an array constructor, determine if the constructor is
   constant or not by expanding it and making sure that all elements
   are constants.  This is a bit of a hack since something like (/ (i,
   i=1,100000000) /) will take a while as* opposed to a more clever
   function that traverses the expression tree. FIXME.  */

int
gfc_constant_ac (gfc_expr * e)
{
  expand_info expand_save;
  try rc;

  iter_stack = NULL;
  expand_save = current_expand;
  current_expand.expand_work_function = constant_element;

  rc = expand_constructor (e->value.constructor);

  current_expand = expand_save;
  if (rc == FAILURE)
    return 0;

  return 1;
}


/* Returns nonzero if an array constructor has been completely
   expanded (no iterators) and zero if iterators are present.  */

int
gfc_expanded_ac (gfc_expr * e)
{
  gfc_constructor *p;

  if (e->expr_type == EXPR_ARRAY)
    for (p = e->value.constructor; p; p = p->next)
      if (p->iterator != NULL || !gfc_expanded_ac (p->expr))
	return 0;

  return 1;
}


/*************** Type resolution of array constructors ***************/

/* Recursive array list resolution function.  All of the elements must
   be of the same type.  */

static try
resolve_array_list (gfc_constructor * p)
{
  try t;

  t = SUCCESS;

  for (; p; p = p->next)
    {
      if (p->iterator != NULL
	  && gfc_resolve_iterator (p->iterator, false) == FAILURE)
	t = FAILURE;

      if (gfc_resolve_expr (p->expr) == FAILURE)
	t = FAILURE;
    }

  return t;
}

/* Resolve character array constructor. If it is a constant character array and
   not specified character length, update character length to the maximum of
   its element constructors' length.  */

void
gfc_resolve_character_array_constructor (gfc_expr * expr)
{
  gfc_constructor * p;
  int max_length;

  gcc_assert (expr->expr_type == EXPR_ARRAY);
  gcc_assert (expr->ts.type == BT_CHARACTER);

  max_length = -1;

  if (expr->ts.cl == NULL)
    {
      for (p = expr->value.constructor; p; p = p->next)
	if (p->expr->ts.cl != NULL)
	  {
	    /* Ensure that if there is a char_len around that it is
	       used; otherwise the middle-end confuses them!  */
	    expr->ts.cl = p->expr->ts.cl;
	    goto got_charlen;
	  }

      expr->ts.cl = gfc_get_charlen ();
      expr->ts.cl->next = gfc_current_ns->cl_list;
      gfc_current_ns->cl_list = expr->ts.cl;
    }

got_charlen:

  if (expr->ts.cl->length == NULL)
    {
      /* Find the maximum length of the elements. Do nothing for variable array
	 constructor, unless the character length is constant or there is a
	constant substring reference.  */

      for (p = expr->value.constructor; p; p = p->next)
	{
	  gfc_ref *ref;
	  for (ref = p->expr->ref; ref; ref = ref->next)
	    if (ref->type == REF_SUBSTRING
		  && ref->u.ss.start->expr_type == EXPR_CONSTANT
		  && ref->u.ss.end->expr_type == EXPR_CONSTANT)
	      break;

	  if (p->expr->expr_type == EXPR_CONSTANT)
	    max_length = MAX (p->expr->value.character.length, max_length);

	  else if (ref)
	    max_length = MAX ((int)(mpz_get_ui (ref->u.ss.end->value.integer)
			      - mpz_get_ui (ref->u.ss.start->value.integer))
			      + 1, max_length);

	  else if (p->expr->ts.cl && p->expr->ts.cl->length
		     && p->expr->ts.cl->length->expr_type == EXPR_CONSTANT)
	    max_length = MAX ((int)mpz_get_si (p->expr->ts.cl->length->value.integer),
			      max_length);

	  else
	    return;
	}

      if (max_length != -1)
	{
	  /* Update the character length of the array constructor.  */
	  expr->ts.cl->length = gfc_int_expr (max_length);
	  /* Update the element constructors.  */
	  for (p = expr->value.constructor; p; p = p->next)
	    if (p->expr->expr_type == EXPR_CONSTANT)
	      gfc_set_constant_character_len (max_length, p->expr);
	}
    }
}

/* Resolve all of the expressions in an array list.  */

try
gfc_resolve_array_constructor (gfc_expr * expr)
{
  try t;

  t = resolve_array_list (expr->value.constructor);
  if (t == SUCCESS)
    t = gfc_check_constructor_type (expr);
  if (t == SUCCESS && expr->ts.type == BT_CHARACTER)
    gfc_resolve_character_array_constructor (expr);

  return t;
}


/* Copy an iterator structure.  */

static gfc_iterator *
copy_iterator (gfc_iterator * src)
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


/* Copy a constructor structure.  */

gfc_constructor *
gfc_copy_constructor (gfc_constructor * src)
{
  gfc_constructor *dest;
  gfc_constructor *tail;

  if (src == NULL)
    return NULL;

  dest = tail = NULL;
  while (src)
    {
      if (dest == NULL)
	dest = tail = gfc_get_constructor ();
      else
	{
	  tail->next = gfc_get_constructor ();
	  tail = tail->next;
	}
      tail->where = src->where;
      tail->expr = gfc_copy_expr (src->expr);
      tail->iterator = copy_iterator (src->iterator);
      mpz_set (tail->n.offset, src->n.offset);
      tail->n.component = src->n.component;
      mpz_set (tail->repeat, src->repeat);
      src = src->next;
    }

  return dest;
}


/* Given an array expression and an element number (starting at zero),
   return a pointer to the array element.  NULL is returned if the
   size of the array has been exceeded.  The expression node returned
   remains a part of the array and should not be freed.  Access is not
   efficient at all, but this is another place where things do not
   have to be particularly fast.  */

gfc_expr *
gfc_get_array_element (gfc_expr * array, int element)
{
  expand_info expand_save;
  gfc_expr *e;
  try rc;

  expand_save = current_expand;
  current_expand.extract_n = element;
  current_expand.expand_work_function = extract_element;
  current_expand.extracted = NULL;
  current_expand.extract_count = 0;

  iter_stack = NULL;

  rc = expand_constructor (array->value.constructor);
  e = current_expand.extracted;
  current_expand = expand_save;

  if (rc == FAILURE)
    return NULL;

  return e;
}


/********* Subroutines for determining the size of an array *********/

/* These are needed just to accommodate RESHAPE().  There are no
   diagnostics here, we just return a negative number if something
   goes wrong.  */


/* Get the size of single dimension of an array specification.  The
   array is guaranteed to be one dimensional.  */

static try
spec_dimen_size (gfc_array_spec * as, int dimen, mpz_t * result)
{

  if (as == NULL)
    return FAILURE;

  if (dimen < 0 || dimen > as->rank - 1)
    gfc_internal_error ("spec_dimen_size(): Bad dimension");

  if (as->type != AS_EXPLICIT
      || as->lower[dimen]->expr_type != EXPR_CONSTANT
      || as->upper[dimen]->expr_type != EXPR_CONSTANT)
    return FAILURE;

  mpz_init (*result);

  mpz_sub (*result, as->upper[dimen]->value.integer,
	   as->lower[dimen]->value.integer);

  mpz_add_ui (*result, *result, 1);

  return SUCCESS;
}


try
spec_size (gfc_array_spec * as, mpz_t * result)
{
  mpz_t size;
  int d;

  mpz_init_set_ui (*result, 1);

  for (d = 0; d < as->rank; d++)
    {
      if (spec_dimen_size (as, d, &size) == FAILURE)
	{
	  mpz_clear (*result);
	  return FAILURE;
	}

      mpz_mul (*result, *result, size);
      mpz_clear (size);
    }

  return SUCCESS;
}


/* Get the number of elements in an array section.  */

static try
ref_dimen_size (gfc_array_ref * ar, int dimen, mpz_t * result)
{
  mpz_t upper, lower, stride;
  try t;

  if (dimen < 0 || ar == NULL || dimen > ar->dimen - 1)
    gfc_internal_error ("ref_dimen_size(): Bad dimension");

  switch (ar->dimen_type[dimen])
    {
    case DIMEN_ELEMENT:
      mpz_init (*result);
      mpz_set_ui (*result, 1);
      t = SUCCESS;
      break;

    case DIMEN_VECTOR:
      t = gfc_array_size (ar->start[dimen], result);	/* Recurse! */
      break;

    case DIMEN_RANGE:
      mpz_init (upper);
      mpz_init (lower);
      mpz_init (stride);
      t = FAILURE;

      if (ar->start[dimen] == NULL)
	{
	  if (ar->as->lower[dimen] == NULL
	      || ar->as->lower[dimen]->expr_type != EXPR_CONSTANT)
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
	      || ar->as->upper[dimen]->expr_type != EXPR_CONSTANT)
	    goto cleanup;
	  mpz_set (upper, ar->as->upper[dimen]->value.integer);
	}
      else
	{
	  if (ar->end[dimen]->expr_type != EXPR_CONSTANT)
	    goto cleanup;
	  mpz_set (upper, ar->end[dimen]->value.integer);
	}

      if (ar->stride[dimen] == NULL)
	mpz_set_ui (stride, 1);
      else
	{
	  if (ar->stride[dimen]->expr_type != EXPR_CONSTANT)
	    goto cleanup;
	  mpz_set (stride, ar->stride[dimen]->value.integer);
	}

      mpz_init (*result);
      mpz_sub (*result, upper, lower);
      mpz_add (*result, *result, stride);
      mpz_div (*result, *result, stride);

      /* Zero stride caught earlier.  */
      if (mpz_cmp_ui (*result, 0) < 0)
	mpz_set_ui (*result, 0);
      t = SUCCESS;

    cleanup:
      mpz_clear (upper);
      mpz_clear (lower);
      mpz_clear (stride);
      return t;

    default:
      gfc_internal_error ("ref_dimen_size(): Bad dimen_type");
    }

  return t;
}


static try
ref_size (gfc_array_ref * ar, mpz_t * result)
{
  mpz_t size;
  int d;

  mpz_init_set_ui (*result, 1);

  for (d = 0; d < ar->dimen; d++)
    {
      if (ref_dimen_size (ar, d, &size) == FAILURE)
	{
	  mpz_clear (*result);
	  return FAILURE;
	}

      mpz_mul (*result, *result, size);
      mpz_clear (size);
    }

  return SUCCESS;
}


/* Given an array expression and a dimension, figure out how many
   elements it has along that dimension.  Returns SUCCESS if we were
   able to return a result in the 'result' variable, FAILURE
   otherwise.  */

try
gfc_array_dimen_size (gfc_expr * array, int dimen, mpz_t * result)
{
  gfc_ref *ref;
  int i;

  if (dimen < 0 || array == NULL || dimen > array->rank - 1)
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

	      return ref_dimen_size (&ref->u.ar, i - 1, result);
	    }
	}

      if (array->shape && array->shape[dimen])
	{
	  mpz_init_set (*result, array->shape[dimen]);
	  return SUCCESS;
	}

      if (spec_dimen_size (array->symtree->n.sym->as, dimen, result) == FAILURE)
	return FAILURE;

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
	return FAILURE;

      mpz_init_set (*result, array->shape[dimen]);

      break;
    }

  return SUCCESS;
}


/* Given an array expression, figure out how many elements are in the
   array.  Returns SUCCESS if this is possible, and sets the 'result'
   variable.  Otherwise returns FAILURE.  */

try
gfc_array_size (gfc_expr * array, mpz_t * result)
{
  expand_info expand_save;
  gfc_ref *ref;
  int i, flag;
  try t;

  switch (array->expr_type)
    {
    case EXPR_ARRAY:
      flag = gfc_suppress_error;
      gfc_suppress_error = 1;

      expand_save = current_expand;

      current_expand.count = result;
      mpz_init_set_ui (*result, 0);

      current_expand.expand_work_function = count_elements;
      iter_stack = NULL;

      t = expand_constructor (array->value.constructor);
      gfc_suppress_error = flag;

      if (t == FAILURE)
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
	return FAILURE;

      mpz_init_set_ui (*result, 1);

      for (i = 0; i < array->rank; i++)
	mpz_mul (*result, *result, array->shape[i]);

      break;
    }

  return SUCCESS;
}


/* Given an array reference, return the shape of the reference in an
   array of mpz_t integers.  */

try
gfc_array_ref_shape (gfc_array_ref * ar, mpz_t * shape)
{
  int d;
  int i;

  d = 0;

  switch (ar->type)
    {
    case AR_FULL:
      for (; d < ar->as->rank; d++)
	if (spec_dimen_size (ar->as, d, &shape[d]) == FAILURE)
	  goto cleanup;

      return SUCCESS;

    case AR_SECTION:
      for (i = 0; i < ar->dimen; i++)
	{
	  if (ar->dimen_type[i] != DIMEN_ELEMENT)
	    {
	      if (ref_dimen_size (ar, i, &shape[d]) == FAILURE)
		goto cleanup;
	      d++;
	    }
	}

      return SUCCESS;

    default:
      break;
    }

cleanup:
  for (d--; d >= 0; d--)
    mpz_clear (shape[d]);

  return FAILURE;
}


/* Given an array expression, find the array reference structure that
   characterizes the reference.  */

gfc_array_ref *
gfc_find_array_ref (gfc_expr * e)
{
  gfc_ref *ref;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY
	&& (ref->u.ar.type == AR_FULL
	    || ref->u.ar.type == AR_SECTION))
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
