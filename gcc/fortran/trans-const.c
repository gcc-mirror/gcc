/* Translation of constants
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* trans-const.c -- convert constant values */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"

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
      gcc_unreachable ();
    }
  return val;
}

tree
gfc_build_string_const (int length, const char *s)
{
  tree str;
  tree len;

  str = build_string (length, s);
  len = build_int_cst (NULL_TREE, length);
  TREE_TYPE (str) =
    build_array_type (gfc_character1_type_node,
		      build_range_type (gfc_charlen_type_node,
					integer_one_node, len));
  return str;
}

/* Build a Fortran character constant from a zero-terminated string.
   Since this is mainly used for error messages, the string will get
   translated.  */
tree
gfc_build_cstring_const (const char *msgid)
{
  return gfc_build_string_const (strlen (msgid) + 1, _(msgid));
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

  gcc_assert (expr->expr_type == EXPR_CONSTANT);
  gcc_assert (expr->ts.type == BT_CHARACTER && expr->ts.kind == 1);
  gcc_assert (INTEGER_CST_P (length));
  gcc_assert (TREE_INT_CST_HIGH (length) == 0);

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
      cl->backend_decl = fold_convert (gfc_charlen_type_node,
					cl->backend_decl);
    }
}

void
gfc_init_constants (void)
{
  int n;

  for (n = 0; n <= GFC_MAX_DIMENSIONS; n++)
    gfc_rank_cst[n] = build_int_cst (gfc_array_index_type, n);
}

/* Converts a GMP integer into a backend tree node.  */
tree
gfc_conv_mpz_to_tree (mpz_t i, int kind)
{
  HOST_WIDE_INT high;
  unsigned HOST_WIDE_INT low;

  if (mpz_fits_slong_p (i))
    {
      /* Note that HOST_WIDE_INT is never smaller than long.  */
      low = mpz_get_si (i);
      high = mpz_sgn (i) < 0 ? -1 : 0;
    }
  else
    {
      unsigned HOST_WIDE_INT words[2];
      size_t count;

      /* Since we know that the value is not zero (mpz_fits_slong_p),
	 we know that at least one word will be written, but we don't know
	 about the second.  It's quicker to zero the second word before
	 than conditionally clear it later.  */
      words[1] = 0;

      /* Extract the absolute value into words.  */
      mpz_export (words, &count, -1, sizeof (HOST_WIDE_INT), 0, 0, i);

      /* We assume that all numbers are in range for its type, and that
	 we never create a type larger than 2*HWI, which is the largest
	 that the middle-end can handle.  */
      gcc_assert (count == 1 || count == 2);

      low = words[0];
      high = words[1];

      /* Negate if necessary.  */
      if (mpz_sgn (i) < 0)
	{
	  if (low == 0)
	    high = -high;
	  else
	    low = -low, high = ~high;
	}
    }

  return build_int_cst_wide (gfc_get_int_type (kind), low, high);
}

/* Converts a real constant into backend form.  Uses an intermediate string
   representation.  */

tree
gfc_conv_mpfr_to_tree (mpfr_t f, int kind)
{
  tree res;
  tree type;
  mp_exp_t exp;
  char *p, *q;
  int n;
  REAL_VALUE_TYPE real;

  n = gfc_validate_kind (BT_REAL, kind, false);

  gcc_assert (gfc_real_kinds[n].radix == 2);

  type = gfc_get_real_type (kind);

  /* Take care of Infinity and NaN.  */
  if (mpfr_inf_p (f))
    {
      real_inf (&real);
      if (mpfr_sgn (f) < 0)
	real = REAL_VALUE_NEGATE(real);
      res = build_real (type , real);
      return res;
    }

  if (mpfr_nan_p (f))
    {
      real_nan (&real, "", 0, TYPE_MODE (type));
      res = build_real (type , real);
      return res;
    }

  /* mpfr chooses too small a number of hexadecimal digits if the
     number of binary digits is not divisible by four, therefore we
     have to explicitly request a sufficient number of digits here.  */
  p = mpfr_get_str (NULL, &exp, 16, gfc_real_kinds[n].digits / 4 + 1,
		    f, GFC_RND_MODE);

  /* REAL_VALUE_ATOF expects the exponent for mantissa * 2**exp,
     mpfr_get_str returns the exponent for mantissa * 16**exp, adjust
     for that.  */
  exp *= 4;

  /* The additional 12 characters add space for the sprintf below.
     This leaves 6 digits for the exponent which is certainly enough.  */
  q = (char *) gfc_getmem (strlen (p) + 12);

  if (p[0] == '-')
    sprintf (q, "-0x.%sp%d", &p[1], (int) exp);
  else
    sprintf (q, "0x.%sp%d", p, (int) exp);

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
  gcc_assert (expr->expr_type == EXPR_CONSTANT);

  /* If it is converted from Hollerith constant, we build string constant
     and VIEW_CONVERT to its type.  */
 
  switch (expr->ts.type)
    {
    case BT_INTEGER:
      if (expr->from_H)
	return build1 (VIEW_CONVERT_EXPR,
			gfc_get_int_type (expr->ts.kind),
			gfc_build_string_const (expr->value.character.length,
				expr->value.character.string));
      else
	return gfc_conv_mpz_to_tree (expr->value.integer, expr->ts.kind);

    case BT_REAL:
      if (expr->from_H)
	return build1 (VIEW_CONVERT_EXPR,
			gfc_get_real_type (expr->ts.kind),
			gfc_build_string_const (expr->value.character.length,
				expr->value.character.string));
      else
	return gfc_conv_mpfr_to_tree (expr->value.real, expr->ts.kind);

    case BT_LOGICAL:
      if (expr->from_H)
	return build1 (VIEW_CONVERT_EXPR,
			gfc_get_logical_type (expr->ts.kind),
			gfc_build_string_const (expr->value.character.length,
				expr->value.character.string));
      else
	return build_int_cst (gfc_get_logical_type (expr->ts.kind),
			    expr->value.logical);

    case BT_COMPLEX:
      if (expr->from_H)
	return build1 (VIEW_CONVERT_EXPR,
			gfc_get_complex_type (expr->ts.kind),
			gfc_build_string_const (expr->value.character.length,
				expr->value.character.string));
      else
	{
	  tree real = gfc_conv_mpfr_to_tree (expr->value.complex.r,
					  expr->ts.kind);
	  tree imag = gfc_conv_mpfr_to_tree (expr->value.complex.i,
					  expr->ts.kind);

	  return build_complex (gfc_typenode_for_spec (&expr->ts),
				real, imag);
	}

    case BT_CHARACTER:
    case BT_HOLLERITH:
      return gfc_build_string_const (expr->value.character.length,
				     expr->value.character.string);

    default:
      fatal_error ("gfc_conv_constant_to_tree(): invalid type: %s",
		   gfc_typename (&expr->ts));
    }
}


/* Like gfc_conv_constant_to_tree, but for a simplified expression.
   We can handle character literal constants here as well.  */

void
gfc_conv_constant (gfc_se * se, gfc_expr * expr)
{
  gcc_assert (expr->expr_type == EXPR_CONSTANT);

  if (se->ss != NULL)
    {
      gcc_assert (se->ss != gfc_ss_terminator);
      gcc_assert (se->ss->type == GFC_SS_SCALAR);
      gcc_assert (se->ss->expr == expr);

      se->expr = se->ss->data.scalar.expr;
      se->string_length = se->ss->string_length;
      gfc_advance_se_ss_chain (se);
      return;
    }

  /* Translate the constant and put it in the simplifier structure.  */
  se->expr = gfc_conv_constant_to_tree (expr);

  /* If this is a CHARACTER string, set its length in the simplifier
     structure, too.  */
  if (expr->ts.type == BT_CHARACTER)
    se->string_length = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (se->expr)));
}
