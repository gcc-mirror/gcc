/* Expression parser.
   Copyright (C) 2000, 2001, 2002, 2004, 2005, 2006 Free Software Foundation,
   Inc.
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
#include "arith.h"
#include "match.h"

static char expression_syntax[] = N_("Syntax error in expression at %C");


/* Match a user-defined operator name.  This is a normal name with a
   few restrictions.  The error_flag controls whether an error is
   raised if 'true' or 'false' are used or not.  */

match
gfc_match_defined_op_name (char *result, int error_flag)
{
  static const char * const badops[] = {
    "and", "or", "not", "eqv", "neqv", "eq", "ne", "ge", "le", "lt", "gt",
      NULL
  };

  char name[GFC_MAX_SYMBOL_LEN + 1];
  locus old_loc;
  match m;
  int i;

  old_loc = gfc_current_locus;

  m = gfc_match (" . %n .", name);
  if (m != MATCH_YES)
    return m;

  /* .true. and .false. have interpretations as constants.  Trying to
     use these as operators will fail at a later time.  */

  if (strcmp (name, "true") == 0 || strcmp (name, "false") == 0)
    {
      if (error_flag)
	goto error;
      gfc_current_locus = old_loc;
      return MATCH_NO;
    }

  for (i = 0; badops[i]; i++)
    if (strcmp (badops[i], name) == 0)
      goto error;

  for (i = 0; name[i]; i++)
    if (!ISALPHA (name[i]))
      {
	gfc_error ("Bad character '%c' in OPERATOR name at %C", name[i]);
	return MATCH_ERROR;
      }

  strcpy (result, name);
  return MATCH_YES;

error:
  gfc_error ("The name '%s' cannot be used as a defined operator at %C",
	     name);

  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}


/* Match a user defined operator.  The symbol found must be an
   operator already.  */

static match
match_defined_operator (gfc_user_op ** result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  match m;

  m = gfc_match_defined_op_name (name, 0);
  if (m != MATCH_YES)
    return m;

  *result = gfc_get_uop (name);
  return MATCH_YES;
}


/* Check to see if the given operator is next on the input.  If this
   is not the case, the parse pointer remains where it was.  */

static int
next_operator (gfc_intrinsic_op t)
{
  gfc_intrinsic_op u;
  locus old_loc;

  old_loc = gfc_current_locus;
  if (gfc_match_intrinsic_op (&u) == MATCH_YES && t == u)
    return 1;

  gfc_current_locus = old_loc;
  return 0;
}


/* Call the INTRINSIC_PARENTHESES function.  This is both
   used explicitly, as below, or by resolve.c to generate
   temporaries.  */
gfc_expr *
gfc_get_parentheses (gfc_expr *e)
{
  gfc_expr *e2;

  e2 = gfc_get_expr();
  e2->expr_type = EXPR_OP;
  e2->ts = e->ts;
  e2->rank = e->rank;
  e2->where = e->where;
  e2->value.op.operator = INTRINSIC_PARENTHESES;
  e2->value.op.op1 = e;
  e2->value.op.op2 = NULL;
  return e2;
}


/* Match a primary expression.  */

static match
match_primary (gfc_expr ** result)
{
  match m;
  gfc_expr *e;
  locus where;

  m = gfc_match_literal_constant (result, 0);
  if (m != MATCH_NO)
    return m;

  m = gfc_match_array_constructor (result);
  if (m != MATCH_NO)
    return m;

  m = gfc_match_rvalue (result);
  if (m != MATCH_NO)
    return m;

  /* Match an expression in parentheses.  */
  where = gfc_current_locus;

  if (gfc_match_char ('(') != MATCH_YES)
    return MATCH_NO;

  m = gfc_match_expr (&e);
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    return m;

  m = gfc_match_char (')');
  if (m == MATCH_NO)
    gfc_error ("Expected a right parenthesis in expression at %C");

  /* Now we have the expression inside the parentheses, build the
     expression pointing to it. By 7.1.7.2 the integrity of
     parentheses is only conserved in numerical calculations, so we
     don't bother to keep the parentheses otherwise.  */
  if(!gfc_numeric_ts(&e->ts))
    *result = e;
  else
    *result = gfc_get_parentheses (e);

  if (m != MATCH_YES)
    {
      gfc_free_expr (*result);
      return MATCH_ERROR;
    }

  return MATCH_YES;

syntax:
  gfc_error (expression_syntax);
  return MATCH_ERROR;
}


/* Build an operator expression node.  */

static gfc_expr *
build_node (gfc_intrinsic_op operator, locus * where,
	    gfc_expr * op1, gfc_expr * op2)
{
  gfc_expr *new;

  new = gfc_get_expr ();
  new->expr_type = EXPR_OP;
  new->value.op.operator = operator;
  new->where = *where;

  new->value.op.op1 = op1;
  new->value.op.op2 = op2;

  return new;
}


/* Match a level 1 expression.  */

static match
match_level_1 (gfc_expr ** result)
{
  gfc_user_op *uop;
  gfc_expr *e, *f;
  locus where;
  match m;

  where = gfc_current_locus;
  uop = NULL;
  m = match_defined_operator (&uop);
  if (m == MATCH_ERROR)
    return m;

  m = match_primary (&e);
  if (m != MATCH_YES)
    return m;

  if (uop == NULL)
    *result = e;
  else
    {
      f = build_node (INTRINSIC_USER, &where, e, NULL);
      f->value.op.uop = uop;
      *result = f;
    }

  return MATCH_YES;
}


/* As a GNU extension we support an expanded level-2 expression syntax.
   Via this extension we support (arbitrary) nesting of unary plus and
   minus operations following unary and binary operators, such as **.
   The grammar of section 7.1.1.3 is effectively rewitten as:

	R704  mult-operand     is level-1-expr [ power-op ext-mult-operand ]
	R704' ext-mult-operand is add-op ext-mult-operand
			       or mult-operand
	R705  add-operand      is add-operand mult-op ext-mult-operand
			       or mult-operand
	R705' ext-add-operand  is add-op ext-add-operand
			       or add-operand
	R706  level-2-expr     is [ level-2-expr ] add-op ext-add-operand
			       or add-operand
 */

static match match_ext_mult_operand (gfc_expr ** result);
static match match_ext_add_operand (gfc_expr ** result);


static int
match_add_op (void)
{

  if (next_operator (INTRINSIC_MINUS))
    return -1;
  if (next_operator (INTRINSIC_PLUS))
    return 1;
  return 0;
}


static match
match_mult_operand (gfc_expr ** result)
{
  gfc_expr *e, *exp, *r;
  locus where;
  match m;

  m = match_level_1 (&e);
  if (m != MATCH_YES)
    return m;

  if (!next_operator (INTRINSIC_POWER))
    {
      *result = e;
      return MATCH_YES;
    }

  where = gfc_current_locus;

  m = match_ext_mult_operand (&exp);
  if (m == MATCH_NO)
    gfc_error ("Expected exponent in expression at %C");
  if (m != MATCH_YES)
    {
      gfc_free_expr (e);
      return MATCH_ERROR;
    }

  r = gfc_power (e, exp);
  if (r == NULL)
    {
      gfc_free_expr (e);
      gfc_free_expr (exp);
      return MATCH_ERROR;
    }

  r->where = where;
  *result = r;

  return MATCH_YES;
}


static match
match_ext_mult_operand (gfc_expr ** result)
{
  gfc_expr *all, *e;
  locus where;
  match m;
  int i;

  where = gfc_current_locus;
  i = match_add_op ();

  if (i == 0)
    return match_mult_operand (result);

  if (gfc_notify_std (GFC_STD_GNU, "Extension: Unary operator following"
		      " arithmetic operator (use parentheses) at %C")
      == FAILURE)
    return MATCH_ERROR;

  m = match_ext_mult_operand (&e);
  if (m != MATCH_YES)
    return m;

  if (i == -1)
    all = gfc_uminus (e);
  else
    all = gfc_uplus (e);

  if (all == NULL)
    {
      gfc_free_expr (e);
      return MATCH_ERROR;
    }

  all->where = where;
  *result = all;
  return MATCH_YES;
}


static match
match_add_operand (gfc_expr ** result)
{
  gfc_expr *all, *e, *total;
  locus where, old_loc;
  match m;
  gfc_intrinsic_op i;

  m = match_mult_operand (&all);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      /* Build up a string of products or quotients.  */

      old_loc = gfc_current_locus;

      if (next_operator (INTRINSIC_TIMES))
	i = INTRINSIC_TIMES;
      else
	{
	  if (next_operator (INTRINSIC_DIVIDE))
	    i = INTRINSIC_DIVIDE;
	  else
	    break;
	}

      where = gfc_current_locus;

      m = match_ext_mult_operand (&e);
      if (m == MATCH_NO)
	{
	  gfc_current_locus = old_loc;
	  break;
	}

      if (m == MATCH_ERROR)
	{
	  gfc_free_expr (all);
	  return MATCH_ERROR;
	}

      if (i == INTRINSIC_TIMES)
	total = gfc_multiply (all, e);
      else
	total = gfc_divide (all, e);

      if (total == NULL)
	{
	  gfc_free_expr (all);
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}

      all = total;
      all->where = where;
    }

  *result = all;
  return MATCH_YES;
}


static match
match_ext_add_operand (gfc_expr ** result)
{
  gfc_expr *all, *e;
  locus where;
  match m;
  int i;

  where = gfc_current_locus;
  i = match_add_op ();

  if (i == 0)
    return match_add_operand (result);

  if (gfc_notify_std (GFC_STD_GNU, "Extension: Unary operator following"
		      " arithmetic operator (use parentheses) at %C")
      == FAILURE)
    return MATCH_ERROR;

  m = match_ext_add_operand (&e);
  if (m != MATCH_YES)
    return m;

  if (i == -1)
    all = gfc_uminus (e);
  else
    all = gfc_uplus (e);

  if (all == NULL)
    {
      gfc_free_expr (e);
      return MATCH_ERROR;
    }

  all->where = where;
  *result = all;
  return MATCH_YES;
}


/* Match a level 2 expression.  */

static match
match_level_2 (gfc_expr ** result)
{
  gfc_expr *all, *e, *total;
  locus where;
  match m;
  int i;

  where = gfc_current_locus;
  i = match_add_op ();

  if (i != 0)
    {
      m = match_ext_add_operand (&e);
      if (m == MATCH_NO)
	{
	  gfc_error (expression_syntax);
	  m = MATCH_ERROR;
	}
    }
  else
    m = match_add_operand (&e);

  if (m != MATCH_YES)
    return m;

  if (i == 0)
    all = e;
  else
    {
      if (i == -1)
	all = gfc_uminus (e);
      else
	all = gfc_uplus (e);

      if (all == NULL)
	{
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}
    }

  all->where = where;

/* Append add-operands to the sum */

  for (;;)
    {
      where = gfc_current_locus;
      i = match_add_op ();
      if (i == 0)
	break;

      m = match_ext_add_operand (&e);
      if (m == MATCH_NO)
	gfc_error (expression_syntax);
      if (m != MATCH_YES)
	{
	  gfc_free_expr (all);
	  return MATCH_ERROR;
	}

      if (i == -1)
	total = gfc_subtract (all, e);
      else
	total = gfc_add (all, e);

      if (total == NULL)
	{
	  gfc_free_expr (all);
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}

      all = total;
      all->where = where;
    }

  *result = all;
  return MATCH_YES;
}


/* Match a level three expression.  */

static match
match_level_3 (gfc_expr ** result)
{
  gfc_expr *all, *e, *total;
  locus where;
  match m;

  m = match_level_2 (&all);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      if (!next_operator (INTRINSIC_CONCAT))
	break;

      where = gfc_current_locus;

      m = match_level_2 (&e);
      if (m == MATCH_NO)
	{
	  gfc_error (expression_syntax);
	  gfc_free_expr (all);
	}
      if (m != MATCH_YES)
	return MATCH_ERROR;

      total = gfc_concat (all, e);
      if (total == NULL)
	{
	  gfc_free_expr (all);
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}

      all = total;
      all->where = where;
    }

  *result = all;
  return MATCH_YES;
}


/* Match a level 4 expression.  */

static match
match_level_4 (gfc_expr ** result)
{
  gfc_expr *left, *right, *r;
  gfc_intrinsic_op i;
  locus old_loc;
  locus where;
  match m;

  m = match_level_3 (&left);
  if (m != MATCH_YES)
    return m;

  old_loc = gfc_current_locus;

  if (gfc_match_intrinsic_op (&i) != MATCH_YES)
    {
      *result = left;
      return MATCH_YES;
    }

  if (i != INTRINSIC_EQ && i != INTRINSIC_NE && i != INTRINSIC_GE
      && i != INTRINSIC_LE && i != INTRINSIC_LT && i != INTRINSIC_GT)
    {
      gfc_current_locus = old_loc;
      *result = left;
      return MATCH_YES;
    }

  where = gfc_current_locus;

  m = match_level_3 (&right);
  if (m == MATCH_NO)
    gfc_error (expression_syntax);
  if (m != MATCH_YES)
    {
      gfc_free_expr (left);
      return MATCH_ERROR;
    }

  switch (i)
    {
    case INTRINSIC_EQ:
      r = gfc_eq (left, right);
      break;

    case INTRINSIC_NE:
      r = gfc_ne (left, right);
      break;

    case INTRINSIC_LT:
      r = gfc_lt (left, right);
      break;

    case INTRINSIC_LE:
      r = gfc_le (left, right);
      break;

    case INTRINSIC_GT:
      r = gfc_gt (left, right);
      break;

    case INTRINSIC_GE:
      r = gfc_ge (left, right);
      break;

    default:
      gfc_internal_error ("match_level_4(): Bad operator");
    }

  if (r == NULL)
    {
      gfc_free_expr (left);
      gfc_free_expr (right);
      return MATCH_ERROR;
    }

  r->where = where;
  *result = r;

  return MATCH_YES;
}


static match
match_and_operand (gfc_expr ** result)
{
  gfc_expr *e, *r;
  locus where;
  match m;
  int i;

  i = next_operator (INTRINSIC_NOT);
  where = gfc_current_locus;

  m = match_level_4 (&e);
  if (m != MATCH_YES)
    return m;

  r = e;
  if (i)
    {
      r = gfc_not (e);
      if (r == NULL)
	{
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}
    }

  r->where = where;
  *result = r;

  return MATCH_YES;
}


static match
match_or_operand (gfc_expr ** result)
{
  gfc_expr *all, *e, *total;
  locus where;
  match m;

  m = match_and_operand (&all);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      if (!next_operator (INTRINSIC_AND))
	break;
      where = gfc_current_locus;

      m = match_and_operand (&e);
      if (m == MATCH_NO)
	gfc_error (expression_syntax);
      if (m != MATCH_YES)
	{
	  gfc_free_expr (all);
	  return MATCH_ERROR;
	}

      total = gfc_and (all, e);
      if (total == NULL)
	{
	  gfc_free_expr (all);
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}

      all = total;
      all->where = where;
    }

  *result = all;
  return MATCH_YES;
}


static match
match_equiv_operand (gfc_expr ** result)
{
  gfc_expr *all, *e, *total;
  locus where;
  match m;

  m = match_or_operand (&all);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      if (!next_operator (INTRINSIC_OR))
	break;
      where = gfc_current_locus;

      m = match_or_operand (&e);
      if (m == MATCH_NO)
	gfc_error (expression_syntax);
      if (m != MATCH_YES)
	{
	  gfc_free_expr (all);
	  return MATCH_ERROR;
	}

      total = gfc_or (all, e);
      if (total == NULL)
	{
	  gfc_free_expr (all);
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}

      all = total;
      all->where = where;
    }

  *result = all;
  return MATCH_YES;
}


/* Match a level 5 expression.  */

static match
match_level_5 (gfc_expr ** result)
{
  gfc_expr *all, *e, *total;
  locus where;
  match m;
  gfc_intrinsic_op i;

  m = match_equiv_operand (&all);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      if (next_operator (INTRINSIC_EQV))
	i = INTRINSIC_EQV;
      else
	{
	  if (next_operator (INTRINSIC_NEQV))
	    i = INTRINSIC_NEQV;
	  else
	    break;
	}

      where = gfc_current_locus;

      m = match_equiv_operand (&e);
      if (m == MATCH_NO)
	gfc_error (expression_syntax);
      if (m != MATCH_YES)
	{
	  gfc_free_expr (all);
	  return MATCH_ERROR;
	}

      if (i == INTRINSIC_EQV)
	total = gfc_eqv (all, e);
      else
	total = gfc_neqv (all, e);

      if (total == NULL)
	{
	  gfc_free_expr (all);
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}

      all = total;
      all->where = where;
    }

  *result = all;
  return MATCH_YES;
}


/* Match an expression.  At this level, we are stringing together
   level 5 expressions separated by binary operators.  */

match
gfc_match_expr (gfc_expr ** result)
{
  gfc_expr *all, *e;
  gfc_user_op *uop;
  locus where;
  match m;

  m = match_level_5 (&all);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      uop = NULL;
      m = match_defined_operator (&uop);
      if (m == MATCH_NO)
	break;
      if (m == MATCH_ERROR)
	{
	  gfc_free_expr (all);
	  return MATCH_ERROR;
	}

      where = gfc_current_locus;

      m = match_level_5 (&e);
      if (m == MATCH_NO)
	gfc_error (expression_syntax);
      if (m != MATCH_YES)
	{
	  gfc_free_expr (all);
	  return MATCH_ERROR;
	}

      all = build_node (INTRINSIC_USER, &where, all, e);
      all->value.op.uop = uop;
    }

  *result = all;
  return MATCH_YES;
}
