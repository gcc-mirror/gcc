/* Translation of constants
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/* trans-const.c -- convert constant values */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "realmpfr.h"
#include "diagnostic-core.h"	/* For fatal_error.  */
#include "double-int.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"
#include "target-memory.h"

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

/* Build a string constant with C char type.  */

tree
gfc_build_string_const (int length, const char *s)
{
  tree str;
  tree len;

  str = build_string (length, s);
  len = size_int (length);
  TREE_TYPE (str) =
    build_array_type (gfc_character1_type_node,
		      build_range_type (gfc_charlen_type_node,
					size_one_node, len));
  return str;
}


/* Build a string constant with a type given by its kind; take care of
   non-default character kinds.  */

tree
gfc_build_wide_string_const (int kind, int length, const gfc_char_t *string)
{
  int i;
  tree str, len;
  size_t size;
  char *s;

  i = gfc_validate_kind (BT_CHARACTER, kind, false);
  size = length * gfc_character_kinds[i].bit_size / 8;

  s = XCNEWVAR (char, size);
  gfc_encode_character (kind, length, string, (unsigned char *) s, size);

  str = build_string (size, s);
  free (s);

  len = size_int (length);
  TREE_TYPE (str) =
    build_array_type (gfc_get_char_type (kind),
		      build_range_type (gfc_charlen_type_node,
					size_one_node, len));
  return str;
}


/* Build a Fortran character constant from a zero-terminated string.
   There a two version of this function, one that translates the string
   and one that doesn't.  */
tree
gfc_build_cstring_const (const char *string)
{
  return gfc_build_string_const (strlen (string) + 1, string);
}

tree
gfc_build_localized_cstring_const (const char *msgid)
{
  const char *localized = _(msgid);
  return gfc_build_string_const (strlen (localized) + 1, localized);
}


/* Return a string constant with the given length.  Used for static
   initializers.  The constant will be padded or truncated to match 
   length.  */

tree
gfc_conv_string_init (tree length, gfc_expr * expr)
{
  gfc_char_t *s;
  HOST_WIDE_INT len;
  int slen;
  tree str;
  bool free_s = false;

  gcc_assert (expr->expr_type == EXPR_CONSTANT);
  gcc_assert (expr->ts.type == BT_CHARACTER);
  gcc_assert (INTEGER_CST_P (length));
  gcc_assert (TREE_INT_CST_HIGH (length) == 0);

  len = TREE_INT_CST_LOW (length);
  slen = expr->value.character.length;

  if (len > slen)
    {
      s = gfc_get_wide_string (len);
      memcpy (s, expr->value.character.string, slen * sizeof (gfc_char_t));
      gfc_wide_memset (&s[slen], ' ', len - slen);
      free_s = true;
    }
  else
    s = expr->value.character.string;

  str = gfc_build_wide_string_const (expr->ts.kind, len, s);

  if (free_s)
    free (s);

  return str;
}


/* Create a tree node for the string length if it is constant.  */

void
gfc_conv_const_charlen (gfc_charlen * cl)
{
  if (!cl || cl->backend_decl)
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
  double_int val = mpz_get_double_int (gfc_get_int_type (kind), i, true);
  return double_int_to_tree (gfc_get_int_type (kind), val);
}

/* Converts a backend tree into a GMP integer.  */

void
gfc_conv_tree_to_mpz (mpz_t i, tree source)
{
  double_int val = tree_to_double_int (source);
  mpz_set_double_int (i, val, TYPE_UNSIGNED (TREE_TYPE (source)));
}

/* Converts a real constant into backend form.  */

tree
gfc_conv_mpfr_to_tree (mpfr_t f, int kind, int is_snan)
{
  tree type;
  int n;
  REAL_VALUE_TYPE real;

  n = gfc_validate_kind (BT_REAL, kind, false);
  gcc_assert (gfc_real_kinds[n].radix == 2);

  type = gfc_get_real_type (kind);
  if (mpfr_nan_p (f) && is_snan)
     real_from_string (&real, "SNaN");
  else
    real_from_mpfr (&real, f, type, GFC_RND_MODE);

  return build_real (type, real);
}

/* Returns a real constant that is +Infinity if the target
   supports infinities for this floating-point mode, and
   +HUGE_VAL otherwise (the largest representable number).  */

tree
gfc_build_inf_or_huge (tree type, int kind)
{
  if (HONOR_INFINITIES (TYPE_MODE (type)))
    {
      REAL_VALUE_TYPE real;
      real_inf (&real);
      return build_real (type, real);
    }
  else
    {
      int k = gfc_validate_kind (BT_REAL, kind, false);
      return gfc_conv_mpfr_to_tree (gfc_real_kinds[k].huge, kind, 0);
    }
}

/* Converts a backend tree into a real constant.  */

void
gfc_conv_tree_to_mpfr (mpfr_ptr f, tree source)
{
  mpfr_from_real (f, TREE_REAL_CST_PTR (source), GFC_RND_MODE);
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
  tree res;

  gcc_assert (expr->expr_type == EXPR_CONSTANT);

  /* If it is has a prescribed memory representation, we build a string
     constant and VIEW_CONVERT to its type.  */
 
  switch (expr->ts.type)
    {
    case BT_INTEGER:
      if (expr->representation.string)
	return fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
			 gfc_get_int_type (expr->ts.kind),
			 gfc_build_string_const (expr->representation.length,
						 expr->representation.string));
      else
	return gfc_conv_mpz_to_tree (expr->value.integer, expr->ts.kind);

    case BT_REAL:
      if (expr->representation.string)
	return fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
			 gfc_get_real_type (expr->ts.kind),
			 gfc_build_string_const (expr->representation.length,
						 expr->representation.string));
      else
	return gfc_conv_mpfr_to_tree (expr->value.real, expr->ts.kind, expr->is_snan);

    case BT_LOGICAL:
      if (expr->representation.string)
	{
	  tree tmp = fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
			gfc_get_int_type (expr->ts.kind),
			gfc_build_string_const (expr->representation.length,
						expr->representation.string));
	  if (!integer_zerop (tmp) && !integer_onep (tmp))
	    gfc_warning ("Assigning value other than 0 or 1 to LOGICAL"
			 " has undefined result at %L", &expr->where);
	  return fold_convert (gfc_get_logical_type (expr->ts.kind), tmp);
	}
      else
	return build_int_cst (gfc_get_logical_type (expr->ts.kind),
			      expr->value.logical);

    case BT_COMPLEX:
      if (expr->representation.string)
	return fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
			 gfc_get_complex_type (expr->ts.kind),
			 gfc_build_string_const (expr->representation.length,
						 expr->representation.string));
      else
	{
	  tree real = gfc_conv_mpfr_to_tree (mpc_realref (expr->value.complex),
					  expr->ts.kind, expr->is_snan);
	  tree imag = gfc_conv_mpfr_to_tree (mpc_imagref (expr->value.complex),
					  expr->ts.kind, expr->is_snan);

	  return build_complex (gfc_typenode_for_spec (&expr->ts),
				real, imag);
	}

    case BT_CHARACTER:
      res = gfc_build_wide_string_const (expr->ts.kind,
					 expr->value.character.length,
					 expr->value.character.string);
      return res;

    case BT_HOLLERITH:
      return gfc_build_string_const (expr->representation.length,
				     expr->representation.string);

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
  gfc_ss *ss;

  /* We may be receiving an expression for C_NULL_PTR or C_NULL_FUNPTR.  If
     so, the expr_type will not yet be an EXPR_CONSTANT.  We need to make
     it so here.  */
  if (expr->ts.type == BT_DERIVED && expr->ts.u.derived
      && expr->ts.u.derived->attr.is_iso_c)
    {
      if (expr->symtree->n.sym->intmod_sym_id == ISOCBINDING_NULL_PTR 
          || expr->symtree->n.sym->intmod_sym_id == ISOCBINDING_NULL_FUNPTR)
        {
          /* Create a new EXPR_CONSTANT expression for our local uses.  */
          expr = gfc_get_int_expr (gfc_default_integer_kind, NULL, 0);
        }
    }

  if (expr->expr_type != EXPR_CONSTANT)
    {
      gfc_expr *e = gfc_get_int_expr (gfc_default_integer_kind, NULL, 0);
      gfc_error ("non-constant initialization expression at %L", &expr->where);
      se->expr = gfc_conv_constant_to_tree (e);
      return;
    }

  ss = se->ss;
  if (ss != NULL)
    {
      gfc_ss_info *ss_info;

      ss_info = ss->info;
      gcc_assert (ss != gfc_ss_terminator);
      gcc_assert (ss_info->type == GFC_SS_SCALAR);
      gcc_assert (ss_info->expr == expr);

      se->expr = ss_info->data.scalar.value;
      se->string_length = ss_info->string_length;
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
