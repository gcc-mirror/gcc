/* Supporting functions for resolving DATA statement.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Lifang Zeng <zlf605@hotmail.com>

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
Software Foundation, 59 Temple Place - Suite 330,Boston, MA
02111-1307, USA.  */


/* Notes for DATA statement implementation:
                                                                               
   We first assign initial value to each symbol by gfc_assign_data_value
   during resolveing DATA statement. Refer to check_data_variable and
   traverse_data_list in resolve.c.
                                                                               
   The complexity exists in the handleing of array section, implied do
   and array of struct appeared in DATA statement.
                                                                               
   We call gfc_conv_structure, gfc_con_array_array_initializer,
   etc., to convert the initial value. Refer to trans-expr.c and
   trans-array.c.  */

#include "config.h"
#include "gfortran.h"
#include "assert.h"

static void formalize_init_expr (gfc_expr *);

/* Calculate the array element offset.  */

static void
get_array_index (gfc_array_ref * ar, mpz_t * offset)
{
  gfc_expr *e;
  int i;
  try re;
  mpz_t delta;
  mpz_t tmp;

  mpz_init (tmp);
  mpz_set_si (*offset, 0);
  mpz_init_set_si (delta, 1);
  for (i = 0; i < ar->dimen; i++)
    {
      e = gfc_copy_expr (ar->start[i]);
      re = gfc_simplify_expr (e, 1);

      if ((gfc_is_constant_expr (ar->as->lower[i]) == 0)
	  || (gfc_is_constant_expr (ar->as->upper[i]) == 0)
	  || (gfc_is_constant_expr (e) == 0))
	gfc_error ("non-constant array in DATA statement %L.", &ar->where);        
      mpz_set (tmp, e->value.integer);
      mpz_sub (tmp, tmp, ar->as->lower[i]->value.integer);
      mpz_mul (tmp, tmp, delta);
      mpz_add (*offset, tmp, *offset);

      mpz_sub (tmp, ar->as->upper[i]->value.integer,
      ar->as->lower[i]->value.integer);
      mpz_add_ui (tmp, tmp, 1);
      mpz_mul (delta, tmp, delta);
    }
  mpz_clear (delta);
  mpz_clear (tmp);
}


/* Find if there is a constructor which offset is equal to OFFSET.  */

static gfc_constructor *
find_con_by_offset (mpz_t offset, gfc_constructor *con)
{
  for (; con; con = con->next)
    {
      if (mpz_cmp (offset, con->n.offset) == 0)
        return con;
    }
  return NULL;
}


/* Find if there is a constructor which component is equal to COM.  */

static gfc_constructor *
find_con_by_component (gfc_component *com, gfc_constructor *con)
{
  for (; con; con = con->next)
    {
      if (com == con->n.component)
        return con;
    }
  return NULL;
}

/* Assign RVALUE to LVALUE where we assume that LVALUE is a substring
   reference. We do a little more than that: if LVALUE already has an
   initialization, we put RVALUE into the existing initialization as
   per the rules of assignment to a substring. If LVALUE has no
   initialization yet, we initialize it to all blanks, then filling in
   the RVALUE.  */

static void
assign_substring_data_value (gfc_expr * lvalue, gfc_expr * rvalue)
{
  gfc_symbol *symbol;
  gfc_expr *expr, *init;
  gfc_ref *ref;
  int len, i;
  int start, end;
  char *c, *d;
	    
  symbol = lvalue->symtree->n.sym;
  ref = lvalue->ref;
  init = symbol->value;

  assert (symbol->ts.type == BT_CHARACTER);
  assert (symbol->ts.cl->length->expr_type == EXPR_CONSTANT);
  assert (symbol->ts.cl->length->ts.type == BT_INTEGER);
  assert (symbol->ts.kind == 1);

  gfc_extract_int (symbol->ts.cl->length, &len);
	    
  if (init == NULL)
    {
      /* Setup the expression to hold the constructor.  */
      expr = gfc_get_expr ();
      expr->expr_type = EXPR_CONSTANT;
      expr->ts.type = BT_CHARACTER;
      expr->ts.kind = 1;
	      
      expr->value.character.length = len;
      expr->value.character.string = gfc_getmem (len);
      memset (expr->value.character.string, ' ', len);

      symbol->value = expr;
    }
  else
    expr = init;
	  
  /* Now that we have allocated the memory for the string,
     fill in the initialized places, truncating the
     intialization string if necessary, i.e.
     DATA a(1:2) /'123'/
     doesn't initialize a(3:3).  */

  gfc_extract_int (ref->u.ss.start, &start);
  gfc_extract_int (ref->u.ss.end, &end);
	    
  assert (start >= 1);
  assert (end <= len);

  len = rvalue->value.character.length;
  c = rvalue->value.character.string;
  d = &expr->value.character.string[start - 1];

  for (i = 0; i <= end - start && i < len; i++)
    d[i] = c[i];

  /* Pad with spaces. I.e. 
     DATA a(1:2) /'a'/
     intializes a(1:2) to 'a ' per the rules for assignment.  
     If init == NULL we don't need to do this, as we have
     intialized the whole string to blanks above.  */

  if (init != NULL)
    for (; i <= end - start; i++)
      d[i] = ' ';

  return;
}

/* Assign the initial value RVALUE to  LVALUE's symbol->value. If the
   LVALUE already has an initialization, we extend this, otherwise we
   create a new one.  */

void
gfc_assign_data_value (gfc_expr * lvalue, gfc_expr * rvalue, mpz_t index)
{
  gfc_ref *ref;
  gfc_expr *init;
  gfc_expr *expr;
  gfc_constructor *con;
  gfc_constructor *last_con;
  gfc_symbol *symbol;
  mpz_t offset;

  ref = lvalue->ref;
  if (ref != NULL && ref->type == REF_SUBSTRING)
    {
      /* No need to go through the for (; ref; ref->next) loop, since
	 a single substring lvalue will only refer to a single
	 substring, and therefore ref->next == NULL.  */
      assert (ref->next == NULL);      
      assign_substring_data_value (lvalue, rvalue);
      return;
    }

  symbol = lvalue->symtree->n.sym;
  init = symbol->value;
  last_con = NULL;
  mpz_init_set_si (offset, 0);

  for (; ref; ref = ref->next)
    {
      /* Use the existing initializer expression if it exists.  Otherwise
         create a new one.  */
      if (init == NULL)
	expr = gfc_get_expr ();
      else
	expr = init;

      /* Find or create this element.  */
      switch (ref->type)
	{
	case REF_ARRAY:
	  if (init == NULL)
	    {
	      /* Setup the expression to hold the constructor.  */
	      expr->expr_type = EXPR_ARRAY;
	      if (ref->next)
		{
		  assert (ref->next->type == REF_COMPONENT);
		  expr->ts.type = BT_DERIVED;
		}
	      else
		expr->ts = rvalue->ts;
	      expr->rank = ref->u.ar.as->rank;
	    }
	  else
	    assert (expr->expr_type == EXPR_ARRAY);

	  if (ref->u.ar.type == AR_ELEMENT)
	    get_array_index (&ref->u.ar, &offset);
	  else
	    mpz_set (offset, index);

	  /* Find the same element in the existing constructor.  */
	  con = expr->value.constructor;
	  con = find_con_by_offset (offset, con);

	  if (con == NULL)
	    {
	      /* Create a new constructor.  */
	      con = gfc_get_constructor();
	      mpz_set (con->n.offset, offset);
	      gfc_insert_constructor (expr, con);
	    }
	  break;

	case REF_COMPONENT:
	  if (init == NULL)
	    {
	      /* Setup the expression to hold the constructor.  */
	      expr->expr_type = EXPR_STRUCTURE;
	      expr->ts.type = BT_DERIVED;
	      expr->ts.derived = ref->u.c.sym;
	    }
	  else
	    assert (expr->expr_type == EXPR_STRUCTURE);

	  /* Find the same element in the existing constructor.  */
	  con = expr->value.constructor;
	  con = find_con_by_component (ref->u.c.component, con);

	  if (con == NULL)
	    {
	      /* Create a new constructor.  */
	      con = gfc_get_constructor ();
	      con->n.component = ref->u.c.component;
	      con->next = expr->value.constructor;
	      expr->value.constructor = con;
	    }
	  break;

       /* case REF_SUBSTRING: dealt with separately above. */
	
	default:
	  abort ();
	}

      if (init == NULL)
	{
	  /* Point the container at the new expression.  */
	  if (last_con == NULL)
	    symbol->value = expr;
	  else
	    last_con->expr = expr;
	}
      init = con->expr;
      last_con = con;
    }

  expr = gfc_copy_expr (rvalue);
  if (!gfc_compare_types (&lvalue->ts, &expr->ts))
    gfc_convert_type (expr, &lvalue->ts, 0);

  if (last_con == NULL)
    symbol->value = expr;
  else
    {
      assert (!last_con->expr);
      last_con->expr = expr;
    }
}


/* Modify the index of array section and re-calculate the array offset.  */

void 
gfc_advance_section (mpz_t *section_index, gfc_array_ref *ar,
		     mpz_t *offset_ret)
{
  int i;
  mpz_t delta;
  mpz_t tmp; 
  bool forwards;
  int cmp;

  for (i = 0; i < ar->dimen; i++)
    {
      if (ar->dimen_type[i] != DIMEN_RANGE)
	continue;

      if (ar->stride[i])
	{
	  mpz_add (section_index[i], section_index[i],
		   ar->stride[i]->value.integer);
	if (mpz_cmp_si (ar->stride[i]->value.integer, 0) >= 0)
	  forwards = true;
	else
	  forwards = false;
	}
      else
	{
	  mpz_add_ui (section_index[i], section_index[i], 1);
	  forwards = true;
	}
      
      if (ar->end[i])
	cmp = mpz_cmp (section_index[i], ar->end[i]->value.integer);
      else
	cmp = mpz_cmp (section_index[i], ar->as->upper[i]->value.integer);

      if ((cmp > 0 && forwards)
	  || (cmp < 0 && ! forwards))
	{
          /* Reset index to start, then loop to advance the next index.  */
	  if (ar->start[i])
	    mpz_set (section_index[i], ar->start[i]->value.integer);
	  else
	    mpz_set (section_index[i], ar->as->lower[i]->value.integer);
	}
      else
	break;
    }

  mpz_set_si (*offset_ret, 0);
  mpz_init_set_si (delta, 1);
  mpz_init (tmp);
  for (i = 0; i < ar->dimen; i++)
    {
      mpz_sub (tmp, section_index[i], ar->as->lower[i]->value.integer);
      mpz_mul (tmp, tmp, delta);
      mpz_add (*offset_ret, tmp, *offset_ret);

      mpz_sub (tmp, ar->as->upper[i]->value.integer, 
               ar->as->lower[i]->value.integer);
      mpz_add_ui (tmp, tmp, 1);
      mpz_mul (delta, tmp, delta);
    }
  mpz_clear (tmp);
  mpz_clear (delta);
}


/* Rearrange a structure constructor so the elements are in the specified
   order.  Also insert NULL entries if neccessary.  */

static void
formalize_structure_cons (gfc_expr * expr)
{
  gfc_constructor *head;
  gfc_constructor *tail;
  gfc_constructor *cur;
  gfc_constructor *last;
  gfc_constructor *c;
  gfc_component *order;

  c = expr->value.constructor;

  /* Constructor is already fomalized.  */
  if (c->n.component == NULL)
    return;

  head = tail = NULL;
  for (order = expr->ts.derived->components; order; order = order->next)
    {
      /* Find the next component.  */
      last = NULL;
      cur = c;
      while (cur != NULL && cur->n.component != order)
	{
	  last = cur;
	  cur = cur->next;
	}

      if (cur == NULL)
	{
	  /* Create a new one.  */
	  cur = gfc_get_constructor ();
	}
      else
	{
	  /* Remove it from the chain.  */
	  if (last == NULL)
	    c = cur->next;
	  else
	    last->next = cur->next;
	  cur->next = NULL;

	  formalize_init_expr (cur->expr);
	}

      /* Add it to the new constructor.  */
      if (head == NULL)
	head = tail = cur;
      else
	{
	  tail->next = cur;
	  tail = tail->next;
	}
    }
  assert (c == NULL);
  expr->value.constructor = head;
}


/* Make sure an initialization expression is in normalized form.  Ie. all
   elements of the constructors are in the correct order.  */

static void
formalize_init_expr (gfc_expr * expr)
{
  expr_t type;
  gfc_constructor *c;

  if (expr == NULL)
    return;

  type = expr->expr_type;
  switch (type)
    {
    case EXPR_ARRAY:
      c = expr->value.constructor;
      while (c)
	{
	  formalize_init_expr (c->expr);
	  c = c->next;
	}
      break;

    case EXPR_STRUCTURE:
      formalize_structure_cons (expr);
      break;

    default:
      break;
    }
}


/* Resolve symbol's initial value after all data statement.  */

void
gfc_formalize_init_value (gfc_symbol *sym)
{
  formalize_init_expr (sym->value);
}


/* Get the integer value into RET_AS and SECTION from AS and AR, and return
   offset.  */
 
void
gfc_get_section_index (gfc_array_ref *ar, mpz_t *section_index, mpz_t *offset)
{
  int i;
  mpz_t delta;
  mpz_t tmp;

  mpz_set_si (*offset, 0);
  mpz_init (tmp);
  mpz_init_set_si (delta, 1);
  for (i = 0; i < ar->dimen; i++)
    {
      mpz_init (section_index[i]);
      switch (ar->dimen_type[i])
	{
	case DIMEN_ELEMENT:
	case DIMEN_RANGE:
	  if (ar->start[i])
	    {
	      mpz_sub (tmp, ar->start[i]->value.integer,
		       ar->as->lower[i]->value.integer);
	      mpz_mul (tmp, tmp, delta);
	      mpz_add (*offset, tmp, *offset);
	      mpz_set (section_index[i], ar->start[i]->value.integer);
	    }
	  else
	      mpz_set (section_index[i], ar->as->lower[i]->value.integer);
	  break;

	case DIMEN_VECTOR:
	  gfc_internal_error ("TODO: Vector sections in data statements");

	default:
	  abort ();
	}

      mpz_sub (tmp, ar->as->upper[i]->value.integer, 
               ar->as->lower[i]->value.integer);
      mpz_add_ui (tmp, tmp, 1);
      mpz_mul (delta, tmp, delta);
    }

  mpz_clear (tmp);
  mpz_clear (delta);
}

