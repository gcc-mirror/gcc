/* Translation of constants
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* trans-const.c -- convert constant values */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include <stdio.h>
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include <gmp.h>
#include <assert.h>
#include <math.h>
#include "gfortran.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"

/* String constants.  */
tree gfc_strconst_bounds;
tree gfc_strconst_fault;
tree gfc_strconst_wrong_return;
tree gfc_strconst_current_filename;

tree gfc_rank_cst[GFC_MAX_DIMENSIONS + 1];

/* Build a constant with given type from an int_cst.  */
tree
gfc_build_const (tree type, tree intval)
{
  tree val;
  tree zero;

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      val = convert (type, intval);
      break;

    case REAL_TYPE:
      val = build_real_from_int_cst (type, intval);
      break;

    case COMPLEX_TYPE:
      val = build_real_from_int_cst (TREE_TYPE (type), intval);
      zero = build_real_from_int_cst (TREE_TYPE (type), integer_zero_node);
      val = build_complex (type, val, zero);
      break;

    default:
      abort ();
    }
  return val;
}

tree
gfc_build_string_const (int length, const char *s)
{
  tree str;
  tree len;

  str = build_string (length, s);
  len = build_int_2 (length, 0);
  TREE_TYPE (str) =
    build_array_type (gfc_character1_type_node,
		      build_range_type (gfc_strlen_type_node,
					integer_one_node, len));
  return str;
}

/* Return a string constant with the given length.  Used for static
   initializers.  The constant will be padded or truncated to match 
   length.  */

tree
gfc_conv_string_init (tree length, gfc_expr * expr)
{
  char *s;
  HOST_WIDE_INT len;
  int slen;
  tree str;

  assert (expr->expr_type == EXPR_CONSTANT);
  assert (expr->ts.type == BT_CHARACTER && expr->ts.kind == 1);
  assert (INTEGER_CST_P (length));
  assert (TREE_INT_CST_HIGH (length) == 0);

  len = TREE_INT_CST_LOW (length);
  slen = expr->value.character.length;

  if (len > slen)
    {
      s = gfc_getmem (len);
      memcpy (s, expr->value.character.string, slen);
      memset (&s[slen], ' ', len - slen);
      str = gfc_build_string_const (len, s);
      gfc_free (s);
    }
  else
    str = gfc_build_string_const (len, expr->value.character.string);

  return str;
}


/* Create a tree node for the string length if it is constant.  */

void
gfc_conv_const_charlen (gfc_charlen * cl)
{
  if (cl->backend_decl)
    return;

  if (cl->length && cl->length->expr_type == EXPR_CONSTANT)
    {
      cl->backend_decl = gfc_conv_mpz_to_tree (cl->length->value.integer,
					       cl->length->ts.kind);
    }
}

void
gfc_init_constants (void)
{
  int n;

  for (n = 0; n <= GFC_MAX_DIMENSIONS; n++)
    {
      gfc_rank_cst[n] = build_int_2 (n, 0);
      TREE_TYPE (gfc_rank_cst[n]) = gfc_array_index_type;
    }

  gfc_strconst_bounds = gfc_build_string_const (21, "Array bound mismatch");

  gfc_strconst_fault =
    gfc_build_string_const (30, "Array reference out of bounds");

  gfc_strconst_wrong_return =
    gfc_build_string_const (32, "Incorrect function return value");

  gfc_strconst_current_filename =
    gfc_build_string_const (strlen (gfc_option.source) + 1,
			    gfc_option.source);
}

#define BITS_PER_HOST_WIDE_INT (8 * sizeof (HOST_WIDE_INT))
/* Converts a GMP integer into a backend tree node.  */
tree
gfc_conv_mpz_to_tree (mpz_t i, int kind)
{
  int val;
  tree res;
  HOST_WIDE_INT high;
  unsigned HOST_WIDE_INT low;
  int negate;
  char buff[10];
  char *p;
  char *q;
  int n;

  /* TODO: could be wrong if sizeof(HOST_WIDE_INT) |= SIZEOF (int).  */
  if (mpz_fits_slong_p (i))
    {
      val = mpz_get_si (i);
      res = build_int_2 (val, (val < 0) ? (HOST_WIDE_INT)-1 : 0);
      TREE_TYPE (res) = gfc_get_int_type (kind);
      return (res);
    }

  n = mpz_sizeinbase (i, 16);
  if (n > 8)
    q = gfc_getmem (n + 2);
  else
    q = buff;

  low = 0;
  high = 0;
  p = mpz_get_str (q, 16, i);
  if (p[0] == '-')
    {
      negate = 1;
      p++;
    }
  else
    negate = 0;

  while (*p)
    {
      n = *(p++);
      if (n >= '0' && n <= '9')
	n = n - '0';
      else if (n >= 'a' && n <= 'z')
	n = n + 10 - 'a';
      else if (n >= 'A' && n <= 'Z')
	n = n + 10 - 'A';
      else
	abort ();

      assert (n >= 0 && n < 16);
      high = (high << 4) + (low >> (BITS_PER_HOST_WIDE_INT - 4));
      low = (low << 4) + n;
    }
  res = build_int_2 (low, high);
  TREE_TYPE (res) = gfc_get_int_type (kind);
  if (negate)
    res = fold (build1 (NEGATE_EXPR, TREE_TYPE (res), res));

  if (q != buff)
    gfc_free (q);

  return res;
}

/* Converts a real constant into backend form.  Uses an intermediate string
   representation.  */
tree
gfc_conv_mpf_to_tree (mpf_t f, int kind)
{
  tree res;
  tree type;
  mp_exp_t exp;
  char *p;
  char *q;
  int n;
  int edigits;

  for (n = 0; gfc_real_kinds[n].kind != 0; n++)
    {
      if (gfc_real_kinds[n].kind == kind)
	break;
    }
  assert (gfc_real_kinds[n].kind);

  assert (gfc_real_kinds[n].radix == 2);

  n = MAX (abs (gfc_real_kinds[n].min_exponent),
	   abs (gfc_real_kinds[n].max_exponent));
#if 0
  edigits = 2 + (int) (log (n) / log (gfc_real_kinds[n].radix));
#endif
  edigits = 1;
  while (n > 0)
    {
      n = n / 10;
      edigits += 3;
    }


  p = mpf_get_str (NULL, &exp, 10, 0, f);

  /* We also have one minus sign, "e", "." and a null terminator.  */
  q = (char *) gfc_getmem (strlen (p) + edigits + 4);

  if (p[0])
    {
      if (p[0] == '-')
	{
	  strcpy (&q[2], &p[1]);
	  q[0] = '-';
	  q[1] = '.';
	}
      else
	{
	  strcpy (&q[1], p);
	  q[0] = '.';
	}
      strcat (q, "e");
      sprintf (&q[strlen (q)], "%d", (int) exp);
    }
  else
    {
      strcpy (q, "0");
    }

  type = gfc_get_real_type (kind);
  res = build_real (type, REAL_VALUE_ATOF (q, TYPE_MODE (type)));
  gfc_free (q);
  gfc_free (p);

  return res;
}


/* Translate any literal constant to a tree.  Constants never have
   pre or post chains.  Character literal constants are special
   special because they have a value and a length, so they cannot be
   returned as a single tree.  It is up to the caller to set the
   length somewhere if necessary.

   Returns the translated constant, or aborts if it gets a type it
   can't handle.  */

tree
gfc_conv_constant_to_tree (gfc_expr * expr)
{
  assert (expr->expr_type == EXPR_CONSTANT);

  switch (expr->ts.type)
    {
    case BT_INTEGER:
      return gfc_conv_mpz_to_tree (expr->value.integer, expr->ts.kind);

    case BT_REAL:
      return gfc_conv_mpf_to_tree (expr->value.real, expr->ts.kind);

    case BT_LOGICAL:
      return build_int_2 (expr->value.logical, 0);

    case BT_COMPLEX:
      {
	tree real = gfc_conv_mpf_to_tree (expr->value.complex.r,
					  expr->ts.kind);
	tree imag = gfc_conv_mpf_to_tree (expr->value.complex.i,
					  expr->ts.kind);

	return build_complex (NULL_TREE, real, imag);
      }

    case BT_CHARACTER:
      return gfc_build_string_const (expr->value.character.length,
				     expr->value.character.string);

    default:
      fatal_error ("gfc_conv_constant_to_tree(): invalid type: %s",
		   gfc_typename (&expr->ts));
    }
}


/* Like gfc_conv_contrant_to_tree, but for a simplified expression.
   We can handle character literal constants here as well.  */

void
gfc_conv_constant (gfc_se * se, gfc_expr * expr)
{
  assert (expr->expr_type == EXPR_CONSTANT);

  if (se->ss != NULL)
    {
      assert (se->ss != gfc_ss_terminator);
      assert (se->ss->type == GFC_SS_SCALAR);
      assert (se->ss->expr == expr);

      se->expr = se->ss->data.scalar.expr;
      se->string_length = se->ss->data.scalar.string_length;
      gfc_advance_se_ss_chain (se);
      return;
    }

  /* Translate the constant and put it in the simplifier structure.  */
  se->expr = gfc_conv_constant_to_tree (expr);

  /* If this is a CHARACTER string, set it's length in the simplifier
     structure, too.  */
  if (expr->ts.type == BT_CHARACTER)
    se->string_length = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (se->expr)));
}
