/* Check functions
   Copyright (C) 2002-2020 Free Software Foundation, Inc.
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
#include "coretypes.h"
#include "options.h"
#include "gfortran.h"
#include "intrinsic.h"
#include "constructor.h"
#include "target-memory.h"


/* Reset a BOZ to a zero value.  This is used to prevent run-on errors
   from resolve.c(resolve_function).  */

static void
reset_boz (gfc_expr *x)
{
  /* Clear boz info.  */
  x->boz.rdx = 0;
  x->boz.len = 0;
  free (x->boz.str);

  x->ts.type = BT_INTEGER;
  x->ts.kind = gfc_default_integer_kind;
  mpz_init (x->value.integer);
  mpz_set_ui (x->value.integer, 0);
}

/* A BOZ literal constant can appear in a limited number of contexts.
   gfc_invalid_boz() is a helper function to simplify error/warning
   generation.  gfortran accepts the nonstandard 'X' for 'Z', and gfortran
   allows the BOZ indicator to appear as a suffix.  If -fallow-invalid-boz
   is used, then issue a warning; otherwise issue an error.  */

bool
gfc_invalid_boz (const char *msg, locus *loc)
{
  if (flag_allow_invalid_boz)
    {
      gfc_warning (0, msg, loc);
      return false;
    }

  const char hint[] = " [see %<-fno-allow-invalid-boz%>]";
  size_t len = strlen (msg) + strlen (hint) + 1;
  char *msg2 = (char *) alloca (len);
  strcpy (msg2, msg);
  strcat (msg2, hint);
  gfc_error (msg2, loc);
  return true;
}


/* Issue an error for an illegal BOZ argument.  */

static bool
illegal_boz_arg (gfc_expr *x)
{
  if (x->ts.type == BT_BOZ)
    {
      gfc_error ("BOZ literal constant at %L cannot be an actual argument "
		 "to %qs", &x->where, gfc_current_intrinsic);
      reset_boz (x);
      return true;
    }

  return false;
}

/* Some precedures take two arguments such that both cannot be BOZ.  */

static bool
boz_args_check(gfc_expr *i, gfc_expr *j)
{
  if (i->ts.type == BT_BOZ && j->ts.type == BT_BOZ)
    {
      gfc_error ("Arguments of %qs at %L and %L cannot both be BOZ "
		 "literal constants", gfc_current_intrinsic, &i->where,
		 &j->where);
      reset_boz (i);
      reset_boz (j);
      return false;

    }

  return true;
}


/* Check that a BOZ is a constant.  */

static bool
is_boz_constant (gfc_expr *a)
{
  if (a->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("Invalid use of BOZ literal constant at %L", &a->where);
      return false;
    }

  return true;
}


/* Convert a octal string into a binary string.  This is used in the
   fallback conversion of an octal string to a REAL.  */

static char *
oct2bin(int nbits, char *oct)
{
  const char bits[8][5] = {
    "000", "001", "010", "011", "100", "101", "110", "111"};

  char *buf, *bufp;
  int i, j, n;

  j = nbits + 1;
  if (nbits == 64) j++;

  bufp = buf = XCNEWVEC (char, j + 1);
  memset (bufp, 0, j + 1);

  n = strlen (oct);
  for (i = 0; i < n; i++, oct++)
    {
      j = *oct - 48;
      strcpy (bufp, &bits[j][0]);
      bufp += 3;
    }

  bufp = XCNEWVEC (char, nbits + 1);
  if (nbits == 64)
    strcpy (bufp, buf + 2);
  else
    strcpy (bufp, buf + 1);

  free (buf);

  return bufp;
}


/* Convert a hexidecimal string into a binary string.  This is used in the
   fallback conversion of a hexidecimal string to a REAL.  */

static char *
hex2bin(int nbits, char *hex)
{
  const char bits[16][5] = {
    "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
    "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"};

  char *buf, *bufp;
  int i, j, n;

  bufp = buf = XCNEWVEC (char, nbits + 1);
  memset (bufp, 0, nbits + 1);

  n = strlen (hex);
  for (i = 0; i < n; i++, hex++)
    {
      j = *hex;
      if (j > 47 && j < 58)
         j -= 48;
      else if (j > 64 && j < 71)
         j -= 55;
      else if (j > 96 && j < 103)
         j -= 87;
      else
         gcc_unreachable ();

      strcpy (bufp, &bits[j][0]);
      bufp += 4;
   }

   return buf;
}


/* Fallback conversion of a BOZ string to REAL.  */

static void
bin2real (gfc_expr *x, int kind)
{
  char buf[114], *sp;
  int b, i, ie, t, w;
  bool sgn;
  mpz_t em;

  i = gfc_validate_kind (BT_REAL, kind, false);
  t = gfc_real_kinds[i].digits - 1;

  /* Number of bits in the exponent.  */
  if (gfc_real_kinds[i].max_exponent == 16384)
    w = 15;
  else if (gfc_real_kinds[i].max_exponent == 1024)
    w = 11;
  else
    w = 8;

  if (x->boz.rdx == 16)
    sp = hex2bin (gfc_real_kinds[i].mode_precision, x->boz.str);
  else if (x->boz.rdx == 8)
    sp = oct2bin (gfc_real_kinds[i].mode_precision, x->boz.str);
  else
    sp = x->boz.str;

  /* Extract sign bit. */
  sgn = *sp != '0';

  /* Extract biased exponent. */
  memset (buf, 0, 114);
  strncpy (buf, ++sp, w);
  mpz_init (em);
  mpz_set_str (em, buf, 2);
  ie = mpz_get_si (em);

  mpfr_init2 (x->value.real, t + 1);
  x->ts.type = BT_REAL;
  x->ts.kind = kind;

  sp += w;		/* Set to first digit in significand. */
  b = (1 << w) - 1;
  if ((i == 0 && ie == b) || (i == 1 && ie == b)
      || ((i == 2 || i == 3) && ie == b))
    {
      bool zeros = true;
      if (i == 2) sp++;
      for (; *sp; sp++)
	{
	  if (*sp != '0')
	    {
	      zeros = false;
	      break;
	    }
	}

      if (zeros)
	mpfr_set_inf (x->value.real, 1);
      else
	mpfr_set_nan (x->value.real);
    }
  else
    {
      if (i == 2)
	strncpy (buf, sp, t + 1);
      else
	{
	  /* Significand with hidden bit. */
 	  buf[0] = '1';
	  strncpy (&buf[1], sp, t);
	}

      /* Convert to significand to integer. */
      mpz_set_str (em, buf, 2);
      ie -= ((1 << (w - 1)) - 1);	/* Unbiased exponent. */
      mpfr_set_z_2exp (x->value.real, em, ie - t, GFC_RND_MODE);
    }

   if (sgn) mpfr_neg (x->value.real, x->value.real, GFC_RND_MODE);

   mpz_clear (em);
}


/* Fortran 2018 treats a BOZ as simply a string of bits.  gfc_boz2real () 
   converts the string into a REAL of the appropriate kind.  The treatment
   of the sign bit is processor dependent.  */

bool
gfc_boz2real (gfc_expr *x, int kind)
{
  extern int gfc_max_integer_kind;
  gfc_typespec ts;
  int len;
  char *buf, *str;

  if (!is_boz_constant (x))
    return false;

  /* Determine the length of the required string.  */
  len = 8 * kind;
  if (x->boz.rdx == 16) len /= 4;
  if (x->boz.rdx == 8) len = len / 3 + 1;
  buf = (char *) alloca (len + 1);		/* +1 for NULL terminator.  */

  if (x->boz.len >= len)			/* Truncate if necessary.  */
    {
      str = x->boz.str + (x->boz.len - len);
      strcpy(buf, str);
    }
  else						/* Copy and pad. */
    {
      memset (buf, 48, len);
      str = buf + (len - x->boz.len);
      strcpy (str, x->boz.str);
    }

  /* Need to adjust leading bits in an octal string.  */
  if (x->boz.rdx == 8)
    {
      /* Clear first bit.  */
      if (kind == 4 || kind == 10 || kind == 16)
	{
	  if (buf[0] == '4')
	    buf[0] = '0';
	  else if (buf[0] == '5')
	    buf[0] = '1';
	  else if (buf[0] == '6')
	    buf[0] = '2';
	  else if (buf[0] == '7')
	    buf[0] = '3';
	}
      /* Clear first two bits.  */
      else
	{
	  if (buf[0] == '4' || buf[0] == '6')
	    buf[0] = '0';
	  else if (buf[0] == '5' || buf[0] == '7')
	    buf[0] = '1';
	}
    }

  /* Reset BOZ string to the truncated or padded version.  */
  free (x->boz.str);
  x->boz.len = len;
  x->boz.str = XCNEWVEC (char, len + 1);
  strncpy (x->boz.str, buf, len);

  /* For some targets, the largest INTEGER in terms of bits is smaller than
     the bits needed to hold the REAL.  Fortunately, the kind type parameter
     indicates the number of bytes required to an INTEGER and a REAL.  */
  if (gfc_max_integer_kind < kind)
    {
      bin2real (x, kind);
    }
  else
    {
      /* Convert to widest possible integer.  */
      gfc_boz2int (x, gfc_max_integer_kind);
      ts.type = BT_REAL;
      ts.kind = kind;
      if (!gfc_convert_boz (x, &ts))
	{
	  gfc_error ("Failure in conversion of BOZ to REAL at %L", &x->where);
	  return false;
	}
    }

  return true;
}


/* Fortran 2018 treats a BOZ as simply a string of bits.  gfc_boz2int () 
   converts the string into an INTEGER of the appropriate kind.  The
   treatment of the sign bit is processor dependent.  If the  converted
   value exceeds the range of the type, then wrap-around semantics are
   applied.  */
 
bool
gfc_boz2int (gfc_expr *x, int kind)
{
  int i, len;
  char *buf, *str;
  mpz_t tmp1;

  if (!is_boz_constant (x))
    return false;

  i = gfc_validate_kind (BT_INTEGER, kind, false);
  len = gfc_integer_kinds[i].bit_size;
  if (x->boz.rdx == 16) len /= 4;
  if (x->boz.rdx == 8) len = len / 3 + 1;
  buf = (char *) alloca (len + 1);		/* +1 for NULL terminator.  */

  if (x->boz.len >= len)			/* Truncate if necessary.  */
    {
      str = x->boz.str + (x->boz.len - len);
      strcpy(buf, str);
    }
  else						/* Copy and pad. */
    {
      memset (buf, 48, len);
      str = buf + (len - x->boz.len);
      strcpy (str, x->boz.str);
    }

  /* Need to adjust leading bits in an octal string.  */
  if (x->boz.rdx == 8)
    {
      /* Clear first bit.  */
      if (kind == 1 || kind == 4 || kind == 16)
	{
	  if (buf[0] == '4')
	    buf[0] = '0';
	  else if (buf[0] == '5')
	    buf[0] = '1';
	  else if (buf[0] == '6')
	    buf[0] = '2';
	  else if (buf[0] == '7')
	    buf[0] = '3';
	}
      /* Clear first two bits.  */
      else
	{
	  if (buf[0] == '4' || buf[0] == '6')
	    buf[0] = '0';
	  else if (buf[0] == '5' || buf[0] == '7')
	    buf[0] = '1';
	}
    }

  /* Convert as-if unsigned integer.  */
  mpz_init (tmp1);
  mpz_set_str (tmp1, buf, x->boz.rdx);

  /* Check for wrap-around.  */
  if (mpz_cmp (tmp1, gfc_integer_kinds[i].huge) > 0)
    {
      mpz_t tmp2;
      mpz_init (tmp2);
      mpz_add_ui (tmp2, gfc_integer_kinds[i].huge, 1);
      mpz_mod (tmp1, tmp1, tmp2);
      mpz_sub (tmp1, tmp1, tmp2);
      mpz_clear (tmp2);
    }

  /* Clear boz info.  */
  x->boz.rdx = 0;
  x->boz.len = 0;
  free (x->boz.str);

  mpz_init (x->value.integer);
  mpz_set (x->value.integer, tmp1);
  x->ts.type = BT_INTEGER;
  x->ts.kind = kind;
  mpz_clear (tmp1);

  return true;
}


/* Make sure an expression is a scalar.  */

static bool
scalar_check (gfc_expr *e, int n)
{
  if (e->rank == 0)
    return true;

  gfc_error ("%qs argument of %qs intrinsic at %L must be a scalar",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where);

  return false;
}


/* Check the type of an expression.  */

static bool
type_check (gfc_expr *e, int n, bt type)
{
  if (e->ts.type == type)
    return true;

  gfc_error ("%qs argument of %qs intrinsic at %L must be %s",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where, gfc_basic_typename (type));

  return false;
}


/* Check that the expression is a numeric type.  */

static bool
numeric_check (gfc_expr *e, int n)
{
  /* Users sometime use a subroutine designator as an actual argument to
     an intrinsic subprogram that expects an argument with a numeric type.  */
  if (e->symtree && e->symtree->n.sym->attr.subroutine)
    goto error;

  if (gfc_numeric_ts (&e->ts))
    return true;

  /* If the expression has not got a type, check if its namespace can
     offer a default type.  */
  if ((e->expr_type == EXPR_VARIABLE || e->expr_type == EXPR_FUNCTION)
	&& e->symtree->n.sym->ts.type == BT_UNKNOWN
	&& gfc_set_default_type (e->symtree->n.sym, 0, e->symtree->n.sym->ns)
	&& gfc_numeric_ts (&e->symtree->n.sym->ts))
    {
      e->ts = e->symtree->n.sym->ts;
      return true;
    }

error:

  gfc_error ("%qs argument of %qs intrinsic at %L must have a numeric type",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where);

  return false;
}


/* Check that an expression is integer or real.  */

static bool
int_or_real_check (gfc_expr *e, int n)
{
  if (e->ts.type != BT_INTEGER && e->ts.type != BT_REAL)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER "
		 "or REAL", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return false;
    }

  return true;
}

/* Check that an expression is integer or real; allow character for
   F2003 or later.  */

static bool
int_or_real_or_char_check_f2003 (gfc_expr *e, int n)
{
  if (e->ts.type != BT_INTEGER && e->ts.type != BT_REAL)
    {
      if (e->ts.type == BT_CHARACTER)
	return gfc_notify_std (GFC_STD_F2003, "Fortran 2003: Character for "
			       "%qs argument of %qs intrinsic at %L",
			       gfc_current_intrinsic_arg[n]->name,
			       gfc_current_intrinsic, &e->where);
      else
	{
	  if (gfc_option.allow_std & GFC_STD_F2003)
	    gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER "
		       "or REAL or CHARACTER",
		       gfc_current_intrinsic_arg[n]->name,
		       gfc_current_intrinsic, &e->where);
	  else
	    gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER "
		       "or REAL", gfc_current_intrinsic_arg[n]->name,
		       gfc_current_intrinsic, &e->where);
	}
      return false;
    }

  return true;
}

/* Check that an expression is an intrinsic type.  */
static bool
intrinsic_type_check (gfc_expr *e, int n)
{
  if (e->ts.type != BT_INTEGER && e->ts.type != BT_REAL
      && e->ts.type != BT_COMPLEX && e->ts.type != BT_CHARACTER
      && e->ts.type != BT_LOGICAL)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be of intrinsic type",
		 gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return false;
    }
  return true;
}

/* Check that an expression is real or complex.  */

static bool
real_or_complex_check (gfc_expr *e, int n)
{
  if (e->ts.type != BT_REAL && e->ts.type != BT_COMPLEX)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be REAL "
		 "or COMPLEX", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return false;
    }

  return true;
}


/* Check that an expression is INTEGER or PROCEDURE.  */

static bool
int_or_proc_check (gfc_expr *e, int n)
{
  if (e->ts.type != BT_INTEGER && e->ts.type != BT_PROCEDURE)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER "
		 "or PROCEDURE", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return false;
    }

  return true;
}


/* Check that the expression is an optional constant integer
   and that it specifies a valid kind for that type.  */

static bool
kind_check (gfc_expr *k, int n, bt type)
{
  int kind;

  if (k == NULL)
    return true;

  if (!type_check (k, n, BT_INTEGER))
    return false;

  if (!scalar_check (k, n))
    return false;

  if (!gfc_check_init_expr (k))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be a constant",
		 gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
		 &k->where);
      return false;
    }

  if (gfc_extract_int (k, &kind)
      || gfc_validate_kind (type, kind, true) < 0)
    {
      gfc_error ("Invalid kind for %s at %L", gfc_basic_typename (type),
		 &k->where);
      return false;
    }

  return true;
}


/* Make sure the expression is a double precision real.  */

static bool
double_check (gfc_expr *d, int n)
{
  if (!type_check (d, n, BT_REAL))
    return false;

  if (d->ts.kind != gfc_default_double_kind)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be double "
		 "precision", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &d->where);
      return false;
    }

  return true;
}


static bool
coarray_check (gfc_expr *e, int n)
{
  if (e->ts.type == BT_CLASS && gfc_expr_attr (e).class_ok
	&& CLASS_DATA (e)->attr.codimension
	&& CLASS_DATA (e)->as->corank)
    {
      gfc_add_class_array_ref (e);
      return true;
    }

  if (!gfc_is_coarray (e))
    {
      gfc_error ("Expected coarray variable as %qs argument to the %s "
                 "intrinsic at %L", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &e->where);
      return false;
    }

  return true;
}


/* Make sure the expression is a logical array.  */

static bool
logical_array_check (gfc_expr *array, int n)
{
  if (array->ts.type != BT_LOGICAL || array->rank == 0)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be a logical "
		 "array", gfc_current_intrinsic_arg[n]->name,
		 gfc_current_intrinsic, &array->where);
      return false;
    }

  return true;
}


/* Make sure an expression is an array.  */

static bool
array_check (gfc_expr *e, int n)
{
  if (e->ts.type == BT_CLASS && gfc_expr_attr (e).class_ok
	&& CLASS_DATA (e)->attr.dimension
	&& CLASS_DATA (e)->as->rank)
    {
      gfc_add_class_array_ref (e);
      return true;
    }

  if (e->rank != 0 && e->ts.type != BT_PROCEDURE)
    return true;

  gfc_error ("%qs argument of %qs intrinsic at %L must be an array",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where);

  return false;
}


/* If expr is a constant, then check to ensure that it is greater than
   of equal to zero.  */

static bool
nonnegative_check (const char *arg, gfc_expr *expr)
{
  int i;

  if (expr->expr_type == EXPR_CONSTANT)
    {
      gfc_extract_int (expr, &i);
      if (i < 0)
	{
	  gfc_error ("%qs at %L must be nonnegative", arg, &expr->where);
	  return false;
	}
    }

  return true;
}


/* If expr is a constant, then check to ensure that it is greater than zero.  */

static bool
positive_check (int n, gfc_expr *expr)
{
  int i;

  if (expr->expr_type == EXPR_CONSTANT)
    {
      gfc_extract_int (expr, &i);
      if (i <= 0)
	{
	  gfc_error ("%qs argument of %qs intrinsic at %L must be positive",
		     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
		     &expr->where);
	  return false;
	}
    }

  return true;
}


/* If expr2 is constant, then check that the value is less than
   (less than or equal to, if 'or_equal' is true) bit_size(expr1).  */

static bool
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
			 "than or equal to BIT_SIZE(%qs)",
			 &expr2->where, arg1);
	      return false;
	    }
	}

      if (or_equal)
	{
	  if (i2 > gfc_integer_kinds[i3].bit_size)
	    {
	      gfc_error ("%qs at %L must be less than "
			 "or equal to BIT_SIZE(%qs)",
			 arg2, &expr2->where, arg1);
	      return false;
	    }
	}
      else
	{
	  if (i2 >= gfc_integer_kinds[i3].bit_size)
	    {
	      gfc_error ("%qs at %L must be less than BIT_SIZE(%qs)",
			 arg2, &expr2->where, arg1);
	      return false;
	    }
	}
    }

  return true;
}


/* If expr is constant, then check that the value is less than or equal
   to the bit_size of the kind k.  */

static bool
less_than_bitsizekind (const char *arg, gfc_expr *expr, int k)
{
  int i, val;

  if (expr->expr_type != EXPR_CONSTANT)
    return true;

  i = gfc_validate_kind (BT_INTEGER, k, false);
  gfc_extract_int (expr, &val);

  if (val > gfc_integer_kinds[i].bit_size)
    {
      gfc_error ("%qs at %L must be less than or equal to the BIT_SIZE of "
		 "INTEGER(KIND=%d)", arg, &expr->where, k);
      return false;
    }

  return true;
}


/* If expr2 and expr3 are constants, then check that the value is less than
   or equal to bit_size(expr1).  */

static bool
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
	  gfc_error ("%<%s + %s%> at %L must be less than or equal "
		     "to BIT_SIZE(%qs)",
		     arg2, arg3, &expr2->where, arg1);
	  return false;
	}
    }

  return true;
}

/* Make sure two expressions have the same type.  */

static bool
same_type_check (gfc_expr *e, int n, gfc_expr *f, int m, bool assoc = false)
{
  gfc_typespec *ets = &e->ts;
  gfc_typespec *fts = &f->ts;

  if (assoc)
    {
      /* Procedure pointer component expressions have the type of the interface
	 procedure. If they are being tested for association with a procedure
	 pointer (ie. not a component), the type of the procedure must be
	 determined.  */
      if (e->ts.type == BT_PROCEDURE && e->symtree->n.sym)
	ets = &e->symtree->n.sym->ts;
      if (f->ts.type == BT_PROCEDURE && f->symtree->n.sym)
	fts = &f->symtree->n.sym->ts;
    }

  if (gfc_compare_types (ets, fts))
    return true;

  gfc_error ("%qs argument of %qs intrinsic at %L must be the same type "
	     "and kind as %qs", gfc_current_intrinsic_arg[m]->name,
	     gfc_current_intrinsic, &f->where,
	     gfc_current_intrinsic_arg[n]->name);

  return false;
}


/* Make sure that an expression has a certain (nonzero) rank.  */

static bool
rank_check (gfc_expr *e, int n, int rank)
{
  if (e->rank == rank)
    return true;

  gfc_error ("%qs argument of %qs intrinsic at %L must be of rank %d",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where, rank);

  return false;
}


/* Make sure a variable expression is not an optional dummy argument.  */

static bool
nonoptional_check (gfc_expr *e, int n)
{
  if (e->expr_type == EXPR_VARIABLE && e->symtree->n.sym->attr.optional)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must not be OPTIONAL",
		 gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
		 &e->where);
    }

  /* TODO: Recursive check on nonoptional variables?  */

  return true;
}


/* Check for ALLOCATABLE attribute.  */

static bool
allocatable_check (gfc_expr *e, int n)
{
  symbol_attribute attr;

  attr = gfc_variable_attr (e, NULL);
  if (!attr.allocatable || attr.associate_var)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be ALLOCATABLE",
		 gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
		 &e->where);
      return false;
    }

  return true;
}


/* Check that an expression has a particular kind.  */

static bool
kind_value_check (gfc_expr *e, int n, int k)
{
  if (e->ts.kind == k)
    return true;

  gfc_error ("%qs argument of %qs intrinsic at %L must be of kind %d",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic,
	     &e->where, k);

  return false;
}


/* Make sure an expression is a variable.  */

static bool
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
	  gfc_error ("%qs argument of %qs intrinsic at %L cannot be "
		     "INTENT(IN)", gfc_current_intrinsic_arg[n]->name,
		     gfc_current_intrinsic, &e->where);
	  return false;
	}
    }

  if (e->expr_type == EXPR_VARIABLE
      && e->symtree->n.sym->attr.flavor != FL_PARAMETER
      && (allow_proc || !e->symtree->n.sym->attr.function))
    return true;

  if (e->expr_type == EXPR_VARIABLE && e->symtree->n.sym->attr.function
      && e->symtree->n.sym == e->symtree->n.sym->result)
    {
      gfc_namespace *ns;
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	if (ns->proc_name == e->symtree->n.sym)
	  return true;
    }

  gfc_error ("%qs argument of %qs intrinsic at %L must be a variable",
	     gfc_current_intrinsic_arg[n]->name, gfc_current_intrinsic, &e->where);

  return false;
}


/* Check the common DIM parameter for correctness.  */

static bool
dim_check (gfc_expr *dim, int n, bool optional)
{
  if (dim == NULL)
    return true;

  if (!type_check (dim, n, BT_INTEGER))
    return false;

  if (!scalar_check (dim, n))
    return false;

  if (!optional && !nonoptional_check (dim, n))
    return false;

  return true;
}


/* If a coarray DIM parameter is a constant, make sure that it is greater than
   zero and less than or equal to the corank of the given array.  */

static bool
dim_corank_check (gfc_expr *dim, gfc_expr *array)
{
  int corank;

  gcc_assert (array->expr_type == EXPR_VARIABLE);

  if (dim->expr_type != EXPR_CONSTANT)
    return true;

  if (array->ts.type == BT_CLASS)
    return true;

  corank = gfc_get_corank (array);

  if (mpz_cmp_ui (dim->value.integer, 1) < 0
      || mpz_cmp_ui (dim->value.integer, corank) > 0)
    {
      gfc_error ("%<dim%> argument of %qs intrinsic at %L is not a valid "
		 "codimension index", gfc_current_intrinsic, &dim->where);

      return false;
    }

  return true;
}


/* If a DIM parameter is a constant, make sure that it is greater than
   zero and less than or equal to the rank of the given array.  If
   allow_assumed is zero then dim must be less than the rank of the array
   for assumed size arrays.  */

static bool
dim_rank_check (gfc_expr *dim, gfc_expr *array, int allow_assumed)
{
  gfc_array_ref *ar;
  int rank;

  if (dim == NULL)
    return true;

  if (dim->expr_type != EXPR_CONSTANT)
    return true;

  if (array->expr_type == EXPR_FUNCTION && array->value.function.isym
      && array->value.function.isym->id == GFC_ISYM_SPREAD)
    rank = array->rank + 1;
  else
    rank = array->rank;

  /* Assumed-rank array.  */
  if (rank == -1)
    rank = GFC_MAX_DIMENSIONS;

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
      gfc_error ("%<dim%> argument of %qs intrinsic at %L is not a valid "
		 "dimension index", gfc_current_intrinsic, &dim->where);

      return false;
    }

  return true;
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

  if (gfc_array_dimen_size (a, ai, &a_size))
    {
      if (gfc_array_dimen_size (b, bi, &b_size))
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

      if (!ra->u.ss.end)
	return -1;

      if ((!ra->u.ss.start || ra->u.ss.start->expr_type == EXPR_CONSTANT)
	  && ra->u.ss.end->expr_type == EXPR_CONSTANT)
	{
	  start_a = ra->u.ss.start ? mpz_get_si (ra->u.ss.start->value.integer)
				   : 1;
	  end_a = mpz_get_si (ra->u.ss.end->value.integer);
	  return (end_a < start_a) ? 0 : end_a - start_a + 1;
	}
      else if (ra->u.ss.start
	       && gfc_dep_compare_expr (ra->u.ss.start, ra->u.ss.end) == 0)
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
   returns true if they have or if the length cannot be determined,
   otherwise return false and raise a gfc_error.  */

bool
gfc_check_same_strlen (const gfc_expr *a, const gfc_expr *b, const char *name)
{
   long len_a, len_b;

   len_a = gfc_var_strlen(a);
   len_b = gfc_var_strlen(b);

   if (len_a == -1 || len_b == -1 || len_a == len_b)
     return true;
   else
     {
       gfc_error ("Unequal character lengths (%ld/%ld) in %s at %L",
		  len_a, len_b, name, &a->where);
       return false;
     }
}


/***** Check functions *****/

/* Check subroutine suitable for intrinsics taking a real argument and
   a kind argument for the result.  */

static bool
check_a_kind (gfc_expr *a, gfc_expr *kind, bt type)
{
  if (!type_check (a, 0, BT_REAL))
    return false;
  if (!kind_check (kind, 1, type))
    return false;

  return true;
}


/* Check subroutine suitable for ceiling, floor and nint.  */

bool
gfc_check_a_ikind (gfc_expr *a, gfc_expr *kind)
{
  return check_a_kind (a, kind, BT_INTEGER);
}


/* Check subroutine suitable for aint, anint.  */

bool
gfc_check_a_xkind (gfc_expr *a, gfc_expr *kind)
{
  return check_a_kind (a, kind, BT_REAL);
}


bool
gfc_check_abs (gfc_expr *a)
{
  if (!numeric_check (a, 0))
    return false;

  return true;
}


bool
gfc_check_achar (gfc_expr *a, gfc_expr *kind)
{
  if (a->ts.type == BT_BOZ)
    {
      if (gfc_invalid_boz ("BOZ literal constant at %L cannot appear in "
			   "ACHAR intrinsic subprogram", &a->where))
	return false;

      if (!gfc_boz2int (a, gfc_default_integer_kind))
	return false;
    }

  if (!type_check (a, 0, BT_INTEGER))
    return false;

  if (!kind_check (kind, 1, BT_CHARACTER))
    return false;

  return true;
}


bool
gfc_check_access_func (gfc_expr *name, gfc_expr *mode)
{
  if (!type_check (name, 0, BT_CHARACTER)
      || !scalar_check (name, 0))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  if (!type_check (mode, 1, BT_CHARACTER)
      || !scalar_check (mode, 1))
    return false;
  if (!kind_value_check (mode, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_all_any (gfc_expr *mask, gfc_expr *dim)
{
  if (!logical_array_check (mask, 0))
    return false;

  if (!dim_check (dim, 1, false))
    return false;

  if (!dim_rank_check (dim, mask, 0))
    return false;

  return true;
}


/* Limited checking for ALLOCATED intrinsic.  Additional checking
   is performed in intrinsic.c(sort_actual), because ALLOCATED
   has two mutually exclusive non-optional arguments.  */

bool
gfc_check_allocated (gfc_expr *array)
{
  /* Tests on allocated components of coarrays need to detour the check to
     argument of the _caf_get.  */
  if (flag_coarray == GFC_FCOARRAY_LIB && array->expr_type == EXPR_FUNCTION
      && array->value.function.isym
      && array->value.function.isym->id == GFC_ISYM_CAF_GET)
    {
      array = array->value.function.actual->expr;
      if (!array->ref)
	return false;
    }

  if (!variable_check (array, 0, false))
    return false;
  if (!allocatable_check (array, 0))
    return false;

  return true;
}


/* Common check function where the first argument must be real or
   integer and the second argument must be the same as the first.  */

bool
gfc_check_a_p (gfc_expr *a, gfc_expr *p)
{
  if (!int_or_real_check (a, 0))
    return false;

  if (a->ts.type != p->ts.type)
    {
      gfc_error ("%qs and %qs arguments of %qs intrinsic at %L must "
		 "have the same type", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &p->where);
      return false;
    }

  if (a->ts.kind != p->ts.kind)
    {
      if (!gfc_notify_std (GFC_STD_GNU, "Different type kinds at %L",
			   &p->where))
       return false;
    }

  return true;
}


bool
gfc_check_x_yd (gfc_expr *x, gfc_expr *y)
{
  if (!double_check (x, 0) || !double_check (y, 1))
    return false;

  return true;
}


bool
gfc_check_associated (gfc_expr *pointer, gfc_expr *target)
{
  symbol_attribute attr1, attr2;
  int i;
  bool t;
  locus *where;

  where = &pointer->where;

  if (pointer->expr_type == EXPR_NULL)
    goto null_arg;

  attr1 = gfc_expr_attr (pointer);

  if (!attr1.pointer && !attr1.proc_pointer)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be a POINTER",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &pointer->where);
      return false;
    }

  /* F2008, C1242.  */
  if (attr1.pointer && gfc_is_coindexed (pointer))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be "
		 "coindexed", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &pointer->where);
      return false;
    }

  /* Target argument is optional.  */
  if (target == NULL)
    return true;

  where = &target->where;
  if (target->expr_type == EXPR_NULL)
    goto null_arg;

  if (target->expr_type == EXPR_VARIABLE || target->expr_type == EXPR_FUNCTION)
    attr2 = gfc_expr_attr (target);
  else
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be a pointer "
		 "or target VARIABLE or FUNCTION",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &target->where);
      return false;
    }

  if (attr1.pointer && !attr2.pointer && !attr2.target)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be a POINTER "
		 "or a TARGET", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &target->where);
      return false;
    }

  /* F2008, C1242.  */
  if (attr1.pointer && gfc_is_coindexed (target))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be "
		 "coindexed", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &target->where);
      return false;
    }

  t = true;
  if (!same_type_check (pointer, 0, target, 1, true))
    t = false;
  if (!rank_check (target, 0, pointer->rank))
    t = false;
  if (target->rank > 0)
    {
      for (i = 0; i < target->rank; i++)
	if (target->ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	  {
	    gfc_error ("Array section with a vector subscript at %L shall not "
		       "be the target of a pointer",
		       &target->where);
	    t = false;
	    break;
	  }
    }
  return t;

null_arg:

  gfc_error ("NULL pointer at %L is not permitted as actual argument "
	     "of %qs intrinsic function", where, gfc_current_intrinsic);
  return false;

}


bool
gfc_check_atan_2 (gfc_expr *y, gfc_expr *x)
{
  /* gfc_notify_std would be a waste of time as the return value
     is seemingly used only for the generic resolution.  The error
     will be: Too many arguments.  */
  if ((gfc_option.allow_std & GFC_STD_F2008) == 0)
    return false;

  return gfc_check_atan2 (y, x);
}


bool
gfc_check_atan2 (gfc_expr *y, gfc_expr *x)
{
  if (!type_check (y, 0, BT_REAL))
    return false;
  if (!same_type_check (y, 0, x, 1))
    return false;

  return true;
}


static bool
gfc_check_atomic (gfc_expr *atom, int atom_no, gfc_expr *value, int val_no,
		  gfc_expr *stat, int stat_no)
{
  if (!scalar_check (atom, atom_no) || !scalar_check (value, val_no))
    return false;

  if (!(atom->ts.type == BT_INTEGER && atom->ts.kind == gfc_atomic_int_kind)
      && !(atom->ts.type == BT_LOGICAL
	   && atom->ts.kind == gfc_atomic_logical_kind))
    {
      gfc_error ("ATOM argument at %L to intrinsic function %s shall be an "
		 "integer of ATOMIC_INT_KIND or a logical of "
		 "ATOMIC_LOGICAL_KIND", &atom->where, gfc_current_intrinsic);
      return false;
    }

  if (!gfc_is_coarray (atom) && !gfc_is_coindexed (atom))
    {
      gfc_error ("ATOM argument at %L of the %s intrinsic function shall be a "
		 "coarray or coindexed", &atom->where, gfc_current_intrinsic);
      return false;
    }

  if (atom->ts.type != value->ts.type)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall have the same "
		 "type as %qs at %L", gfc_current_intrinsic_arg[val_no]->name,
		 gfc_current_intrinsic, &value->where,
		 gfc_current_intrinsic_arg[atom_no]->name, &atom->where);
      return false;
    }

  if (stat != NULL)
    {
      if (!type_check (stat, stat_no, BT_INTEGER))
	return false;
      if (!scalar_check (stat, stat_no))
	return false;
      if (!variable_check (stat, stat_no, false))
	return false;
      if (!kind_value_check (stat, stat_no, gfc_default_integer_kind))
	return false;

      if (!gfc_notify_std (GFC_STD_F2018, "STAT= argument to %s at %L",
			   gfc_current_intrinsic, &stat->where))
	return false;
    }

  return true;
}


bool
gfc_check_atomic_def (gfc_expr *atom, gfc_expr *value, gfc_expr *stat)
{
  if (atom->expr_type == EXPR_FUNCTION
      && atom->value.function.isym
      && atom->value.function.isym->id == GFC_ISYM_CAF_GET)
    atom = atom->value.function.actual->expr;

  if (!gfc_check_vardef_context (atom, false, false, false, NULL))
    {
      gfc_error ("ATOM argument of the %s intrinsic function at %L shall be "
		 "definable", gfc_current_intrinsic, &atom->where);
      return false;
    }

  return gfc_check_atomic (atom, 0, value, 1, stat, 2);
}


bool
gfc_check_atomic_op (gfc_expr *atom, gfc_expr *value, gfc_expr *stat)
{
  if (atom->ts.type != BT_INTEGER || atom->ts.kind != gfc_atomic_int_kind)
    {
      gfc_error ("ATOM argument at %L to intrinsic function %s shall be an "
		 "integer of ATOMIC_INT_KIND", &atom->where,
		 gfc_current_intrinsic);
      return false;
    }

  return gfc_check_atomic_def (atom, value, stat);
}


bool
gfc_check_atomic_ref (gfc_expr *value, gfc_expr *atom, gfc_expr *stat)
{
  if (atom->expr_type == EXPR_FUNCTION
      && atom->value.function.isym
      && atom->value.function.isym->id == GFC_ISYM_CAF_GET)
    atom = atom->value.function.actual->expr;

  if (!gfc_check_vardef_context (value, false, false, false, NULL))
    {
      gfc_error ("VALUE argument of the %s intrinsic function at %L shall be "
		 "definable", gfc_current_intrinsic, &value->where);
      return false;
    }

  return gfc_check_atomic (atom, 1, value, 0, stat, 2);
}


bool
gfc_check_image_status (gfc_expr *image, gfc_expr *team)
{
  /* IMAGE has to be a positive, scalar integer.  */
  if (!type_check (image, 0, BT_INTEGER) || !scalar_check (image, 0)
      || !positive_check (0, image))
    return false;

  if (team)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L not yet supported",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &team->where);
      return false;
    }
  return true;
}


bool
gfc_check_failed_or_stopped_images (gfc_expr *team, gfc_expr *kind)
{
  if (team)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L not yet supported",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &team->where);
      return false;
    }

  if (kind)
    {
      int k;

      if (!type_check (kind, 1, BT_INTEGER) || !scalar_check (kind, 1)
	  || !positive_check (1, kind))
	return false;

      /* Get the kind, reporting error on non-constant or overflow.  */
      gfc_current_locus = kind->where;
      if (gfc_extract_int (kind, &k, 1))
	return false;
      if (gfc_validate_kind (BT_INTEGER, k, true) == -1)
	{
	  gfc_error ("%qs argument of %qs intrinsic at %L shall specify a "
		     "valid integer kind", gfc_current_intrinsic_arg[1]->name,
		     gfc_current_intrinsic, &kind->where);
	  return false;
	}
    }
  return true;
}


bool
gfc_check_get_team (gfc_expr *level)
{
  if (level)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L not yet supported",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &level->where);
      return false;
    }
  return true;
}


bool
gfc_check_atomic_cas (gfc_expr *atom, gfc_expr *old, gfc_expr *compare,
		      gfc_expr *new_val,  gfc_expr *stat)
{
  if (atom->expr_type == EXPR_FUNCTION
      && atom->value.function.isym
      && atom->value.function.isym->id == GFC_ISYM_CAF_GET)
    atom = atom->value.function.actual->expr;

  if (!gfc_check_atomic (atom, 0, new_val, 3, stat, 4))
    return false;

  if (!scalar_check (old, 1) || !scalar_check (compare, 2))
    return false;

  if (!same_type_check (atom, 0, old, 1))
    return false;

  if (!same_type_check (atom, 0, compare, 2))
    return false;

  if (!gfc_check_vardef_context (atom, false, false, false, NULL))
    {
      gfc_error ("ATOM argument of the %s intrinsic function at %L shall be "
		 "definable", gfc_current_intrinsic, &atom->where);
      return false;
    }

  if (!gfc_check_vardef_context (old, false, false, false, NULL))
    {
      gfc_error ("OLD argument of the %s intrinsic function at %L shall be "
		 "definable", gfc_current_intrinsic, &old->where);
      return false;
    }

  return true;
}

bool
gfc_check_event_query (gfc_expr *event, gfc_expr *count, gfc_expr *stat)
{
  if (event->ts.type != BT_DERIVED
      || event->ts.u.derived->from_intmod != INTMOD_ISO_FORTRAN_ENV
      || event->ts.u.derived->intmod_sym_id != ISOFORTRAN_EVENT_TYPE)
    {
      gfc_error ("EVENT argument at %L to the intrinsic EVENT_QUERY "
		 "shall be of type EVENT_TYPE", &event->where);
      return false;
    }

  if (!scalar_check (event, 0))
    return false;

  if (!gfc_check_vardef_context (count, false, false, false, NULL))
    {
      gfc_error ("COUNT argument of the EVENT_QUERY intrinsic function at %L "
		 "shall be definable", &count->where);
      return false;
    }

  if (!type_check (count, 1, BT_INTEGER))
    return false;

  int i = gfc_validate_kind (BT_INTEGER, count->ts.kind, false);
  int j = gfc_validate_kind (BT_INTEGER, gfc_default_integer_kind, false);

  if (gfc_integer_kinds[i].range < gfc_integer_kinds[j].range)
    {
      gfc_error ("COUNT argument of the EVENT_QUERY intrinsic function at %L "
		 "shall have at least the range of the default integer",
		 &count->where);
      return false;
    }

  if (stat != NULL)
    {
      if (!type_check (stat, 2, BT_INTEGER))
	return false;
      if (!scalar_check (stat, 2))
	return false;
      if (!variable_check (stat, 2, false))
	return false;

      if (!gfc_notify_std (GFC_STD_F2018, "STAT= argument to %s at %L",
			   gfc_current_intrinsic, &stat->where))
	return false;
    }

  return true;
}


bool
gfc_check_atomic_fetch_op (gfc_expr *atom, gfc_expr *value, gfc_expr *old,
			   gfc_expr *stat)
{
  if (atom->expr_type == EXPR_FUNCTION
      && atom->value.function.isym
      && atom->value.function.isym->id == GFC_ISYM_CAF_GET)
    atom = atom->value.function.actual->expr;

  if (atom->ts.type != BT_INTEGER || atom->ts.kind != gfc_atomic_int_kind)
    {
      gfc_error ("ATOM argument at %L to intrinsic function %s shall be an "
		 "integer of ATOMIC_INT_KIND", &atom->where,
		 gfc_current_intrinsic);
      return false;
    }

  if (!gfc_check_atomic (atom, 0, value, 1, stat, 3))
    return false;

  if (!scalar_check (old, 2))
    return false;

  if (!same_type_check (atom, 0, old, 2))
    return false;

  if (!gfc_check_vardef_context (atom, false, false, false, NULL))
    {
      gfc_error ("ATOM argument of the %s intrinsic function at %L shall be "
		 "definable", gfc_current_intrinsic, &atom->where);
      return false;
    }

  if (!gfc_check_vardef_context (old, false, false, false, NULL))
    {
      gfc_error ("OLD argument of the %s intrinsic function at %L shall be "
		 "definable", gfc_current_intrinsic, &old->where);
      return false;
    }

  return true;
}


/* BESJN and BESYN functions.  */

bool
gfc_check_besn (gfc_expr *n, gfc_expr *x)
{
  if (!type_check (n, 0, BT_INTEGER))
    return false;
  if (n->expr_type == EXPR_CONSTANT)
    {
      int i;
      gfc_extract_int (n, &i);
      if (i < 0 && !gfc_notify_std (GFC_STD_GNU, "Negative argument "
				    "N at %L", &n->where))
	return false;
    }

  if (!type_check (x, 1, BT_REAL))
    return false;

  return true;
}


/* Transformational version of the Bessel JN and YN functions.  */

bool
gfc_check_bessel_n2 (gfc_expr *n1, gfc_expr *n2, gfc_expr *x)
{
  if (!type_check (n1, 0, BT_INTEGER))
    return false;
  if (!scalar_check (n1, 0))
    return false;
  if (!nonnegative_check ("N1", n1))
    return false;

  if (!type_check (n2, 1, BT_INTEGER))
    return false;
  if (!scalar_check (n2, 1))
    return false;
  if (!nonnegative_check ("N2", n2))
    return false;

  if (!type_check (x, 2, BT_REAL))
    return false;
  if (!scalar_check (x, 2))
    return false;

  return true;
}


bool
gfc_check_bge_bgt_ble_blt (gfc_expr *i, gfc_expr *j)
{
  extern int gfc_max_integer_kind;

  /* If i and j are both BOZ, convert to widest INTEGER.  */
  if (i->ts.type == BT_BOZ && j->ts.type == BT_BOZ)
    {
      if (!gfc_boz2int (i, gfc_max_integer_kind))
	return false;
      if (!gfc_boz2int (j, gfc_max_integer_kind))
	return false;
    }

  /* If i is BOZ and j is integer, convert i to type of j.  */
  if (i->ts.type == BT_BOZ && j->ts.type == BT_INTEGER
      && !gfc_boz2int (i, j->ts.kind))
    return false;

  /* If j is BOZ and i is integer, convert j to type of i.  */
  if (j->ts.type == BT_BOZ && i->ts.type == BT_INTEGER
      && !gfc_boz2int (j, i->ts.kind))
    return false;

  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!type_check (j, 1, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_bitfcn (gfc_expr *i, gfc_expr *pos)
{
  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!type_check (pos, 1, BT_INTEGER))
    return false;

  if (!nonnegative_check ("pos", pos))
    return false;

  if (!less_than_bitsize1 ("i", i, "pos", pos, false))
    return false;

  return true;
}


bool
gfc_check_char (gfc_expr *i, gfc_expr *kind)
{
  if (i->ts.type == BT_BOZ)
    {
      if (gfc_invalid_boz ("BOZ literal constant at %L cannot appear in "
			   "CHAR intrinsic subprogram", &i->where))
	return false;

      if (!gfc_boz2int (i, gfc_default_integer_kind))
	return false;
    }

  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!kind_check (kind, 1, BT_CHARACTER))
    return false;

  return true;
}


bool
gfc_check_chdir (gfc_expr *dir)
{
  if (!type_check (dir, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (dir, 0, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_chdir_sub (gfc_expr *dir, gfc_expr *status)
{
  if (!type_check (dir, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (dir, 0, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 1, BT_INTEGER))
    return false;
  if (!scalar_check (status, 1))
    return false;

  return true;
}


bool
gfc_check_chmod (gfc_expr *name, gfc_expr *mode)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  if (!type_check (mode, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (mode, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_chmod_sub (gfc_expr *name, gfc_expr *mode, gfc_expr *status)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  if (!type_check (mode, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (mode, 1, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 2, BT_INTEGER))
    return false;

  if (!scalar_check (status, 2))
    return false;

  return true;
}


bool
gfc_check_cmplx (gfc_expr *x, gfc_expr *y, gfc_expr *kind)
{
  int k;

  /* Check kind first, because it may be needed in conversion of a BOZ.  */
  if (kind)
    {
      if (!kind_check (kind, 2, BT_COMPLEX))
	return false;
      gfc_extract_int (kind, &k);
    }
  else
    k = gfc_default_complex_kind;

  if (x->ts.type == BT_BOZ && !gfc_boz2real (x, k))
    return false;

  if (!numeric_check (x, 0))
    return false;

  if (y != NULL)
    {
      if (y->ts.type == BT_BOZ && !gfc_boz2real (y, k))
	return false;

      if (!numeric_check (y, 1))
	return false;

      if (x->ts.type == BT_COMPLEX)
	{
	  gfc_error ("%qs argument of %qs intrinsic at %L must not be "
		     "present if %<x%> is COMPLEX",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &y->where);
	  return false;
	}

      if (y->ts.type == BT_COMPLEX)
	{
	  gfc_error ("%qs argument of %qs intrinsic at %L must have a type "
		     "of either REAL or INTEGER",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &y->where);
	  return false;
	}
    }

  if (!kind && warn_conversion
      && x->ts.type == BT_REAL && x->ts.kind > gfc_default_real_kind)
    gfc_warning_now (OPT_Wconversion, "Conversion from %s to default-kind "
		     "COMPLEX(%d) at %L might lose precision, consider using "
		     "the KIND argument", gfc_typename (&x->ts),
		     gfc_default_real_kind, &x->where);
  else if (y && !kind && warn_conversion
	   && y->ts.type == BT_REAL && y->ts.kind > gfc_default_real_kind)
    gfc_warning_now (OPT_Wconversion, "Conversion from %s to default-kind "
		     "COMPLEX(%d) at %L might lose precision, consider using "
		     "the KIND argument", gfc_typename (&y->ts),
		     gfc_default_real_kind, &y->where);
  return true;
}


static bool
check_co_collective (gfc_expr *a, gfc_expr *image_idx, gfc_expr *stat,
		    gfc_expr *errmsg, bool co_reduce)
{
  if (!variable_check (a, 0, false))
    return false;

  if (!gfc_check_vardef_context (a, false, false, false, "argument 'A' with "
				 "INTENT(INOUT)"))
    return false;

  /* Fortran 2008, 12.5.2.4, paragraph 18.  */
  if (gfc_has_vector_subscript (a))
    {
      gfc_error ("Argument %<A%> with INTENT(INOUT) at %L of the intrinsic "
		 "subroutine %s shall not have a vector subscript",
		 &a->where, gfc_current_intrinsic);
      return false;
    }

  if (gfc_is_coindexed (a))
    {
      gfc_error ("The A argument at %L to the intrinsic %s shall not be "
		 "coindexed", &a->where, gfc_current_intrinsic);
      return false;
    }

  if (image_idx != NULL)
    {
      if (!type_check (image_idx, co_reduce ? 2 : 1, BT_INTEGER))
	return false;
      if (!scalar_check (image_idx, co_reduce ? 2 : 1))
	return false;
    }

  if (stat != NULL)
    {
      if (!type_check (stat, co_reduce ? 3 : 2, BT_INTEGER))
	return false;
      if (!scalar_check (stat, co_reduce ? 3 : 2))
	return false;
      if (!variable_check (stat, co_reduce ? 3 : 2, false))
	return false;
      if (stat->ts.kind != 4)
	{
	  gfc_error ("The stat= argument at %L must be a kind=4 integer "
		     "variable", &stat->where);
	  return false;
	}
    }

  if (errmsg != NULL)
    {
      if (!type_check (errmsg, co_reduce ? 4 : 3, BT_CHARACTER))
	return false;
      if (!scalar_check (errmsg, co_reduce ? 4 : 3))
	return false;
      if (!variable_check (errmsg, co_reduce ? 4 : 3, false))
	return false;
      if (errmsg->ts.kind != 1)
	{
	  gfc_error ("The errmsg= argument at %L must be a default-kind "
		     "character variable", &errmsg->where);
	  return false;
	}
    }

  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %L, use %<-fcoarray=%> to enable",
		       &a->where);
      return false;
    }

  return true;
}


bool
gfc_check_co_broadcast (gfc_expr *a, gfc_expr *source_image, gfc_expr *stat,
			gfc_expr *errmsg)
{
  if (a->ts.type == BT_CLASS || gfc_expr_attr (a).alloc_comp)
    {
      gfc_error ("Support for the A argument at %L which is polymorphic A "
		 "argument or has allocatable components is not yet "
		 "implemented", &a->where);
      return false;
    }
  return check_co_collective (a, source_image, stat, errmsg, false);
}


bool
gfc_check_co_reduce (gfc_expr *a, gfc_expr *op, gfc_expr *result_image,
		     gfc_expr *stat, gfc_expr *errmsg)
{
  symbol_attribute attr;
  gfc_formal_arglist *formal;
  gfc_symbol *sym;

  if (a->ts.type == BT_CLASS)
    {
      gfc_error ("The A argument at %L of CO_REDUCE shall not be polymorphic",
		 &a->where);
      return false;
    }

  if (gfc_expr_attr (a).alloc_comp)
    {
      gfc_error ("Support for the A argument at %L with allocatable components"
                 " is not yet implemented", &a->where);
      return false;
    }

  if (!check_co_collective (a, result_image, stat, errmsg, true))
    return false;

  if (!gfc_resolve_expr (op))
    return false;

  attr = gfc_expr_attr (op);
  if (!attr.pure || !attr.function)
    {
      gfc_error ("OPERATOR argument at %L must be a PURE function",
		 &op->where);
      return false;
    }

  if (attr.intrinsic)
    {
      /* None of the intrinsics fulfills the criteria of taking two arguments,
	 returning the same type and kind as the arguments and being permitted
	 as actual argument.  */
      gfc_error ("Intrinsic function %s at %L is not permitted for CO_REDUCE",
		 op->symtree->n.sym->name, &op->where);
      return false;
    }

  if (gfc_is_proc_ptr_comp (op))
    {
      gfc_component *comp = gfc_get_proc_ptr_comp (op);
      sym = comp->ts.interface;
    }
  else
    sym = op->symtree->n.sym;

  formal = sym->formal;

  if (!formal || !formal->next || formal->next->next)
    {
      gfc_error ("The function passed as OPERATOR at %L shall have two "
		 "arguments", &op->where);
      return false;
    }

  if (sym->result->ts.type == BT_UNKNOWN)
    gfc_set_default_type (sym->result, 0, NULL);

  if (!gfc_compare_types (&a->ts, &sym->result->ts))
    {
      gfc_error ("The A argument at %L has type %s but the function passed as "
		 "OPERATOR at %L returns %s",
		 &a->where, gfc_typename (a), &op->where,
		 gfc_typename (&sym->result->ts));
      return false;
    }
  if (!gfc_compare_types (&a->ts, &formal->sym->ts)
      || !gfc_compare_types (&a->ts, &formal->next->sym->ts))
    {
      gfc_error ("The function passed as OPERATOR at %L has arguments of type "
		 "%s and %s but shall have type %s", &op->where,
		 gfc_typename (&formal->sym->ts),
		 gfc_typename (&formal->next->sym->ts), gfc_typename (a));
      return false;
    }
  if (op->rank || attr.allocatable || attr.pointer || formal->sym->as
      || formal->next->sym->as || formal->sym->attr.allocatable
      || formal->next->sym->attr.allocatable || formal->sym->attr.pointer
      || formal->next->sym->attr.pointer)
    {
      gfc_error ("The function passed as OPERATOR at %L shall have scalar "
		 "nonallocatable nonpointer arguments and return a "
		 "nonallocatable nonpointer scalar", &op->where);
      return false;
    }

  if (formal->sym->attr.value != formal->next->sym->attr.value)
    {
      gfc_error ("The function passed as OPERATOR at %L shall have the VALUE "
		 "attribute either for none or both arguments", &op->where);
      return false;
    }

  if (formal->sym->attr.target != formal->next->sym->attr.target)
    {
      gfc_error ("The function passed as OPERATOR at %L shall have the TARGET "
		 "attribute either for none or both arguments", &op->where);
      return false;
    }

  if (formal->sym->attr.asynchronous != formal->next->sym->attr.asynchronous)
    {
      gfc_error ("The function passed as OPERATOR at %L shall have the "
		 "ASYNCHRONOUS attribute either for none or both arguments",
		 &op->where);
      return false;
    }

  if (formal->sym->attr.optional || formal->next->sym->attr.optional)
    {
      gfc_error ("The function passed as OPERATOR at %L shall not have the "
		 "OPTIONAL attribute for either of the arguments", &op->where);
      return false;
    }

  if (a->ts.type == BT_CHARACTER)
    {
      gfc_charlen *cl;
      unsigned long actual_size, formal_size1, formal_size2, result_size;

      cl = a->ts.u.cl;
      actual_size = cl && cl->length && cl->length->expr_type == EXPR_CONSTANT
		     ? mpz_get_ui (cl->length->value.integer) : 0;

      cl = formal->sym->ts.u.cl;
      formal_size1 = cl && cl->length && cl->length->expr_type == EXPR_CONSTANT
		     ? mpz_get_ui (cl->length->value.integer) : 0;

      cl = formal->next->sym->ts.u.cl;
      formal_size2 = cl && cl->length && cl->length->expr_type == EXPR_CONSTANT
		     ? mpz_get_ui (cl->length->value.integer) : 0;

      cl = sym->ts.u.cl;
      result_size = cl && cl->length && cl->length->expr_type == EXPR_CONSTANT
		    ? mpz_get_ui (cl->length->value.integer) : 0;

      if (actual_size
	  && ((formal_size1 && actual_size != formal_size1)
	       || (formal_size2 && actual_size != formal_size2)))
	{
	  gfc_error ("The character length of the A argument at %L and of the "
		     "arguments of the OPERATOR at %L shall be the same",
		     &a->where, &op->where);
	  return false;
	}
      if (actual_size && result_size && actual_size != result_size)
	{
	  gfc_error ("The character length of the A argument at %L and of the "
		     "function result of the OPERATOR at %L shall be the same",
		     &a->where, &op->where);
	  return false;
	}
    }

  return true;
}


bool
gfc_check_co_minmax (gfc_expr *a, gfc_expr *result_image, gfc_expr *stat,
		     gfc_expr *errmsg)
{
  if (a->ts.type != BT_INTEGER && a->ts.type != BT_REAL
      && a->ts.type != BT_CHARACTER)
    {
       gfc_error ("%qs argument of %qs intrinsic at %L shall be of type "
		  "integer, real or character",
		  gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		  &a->where);
       return false;
    }
  return check_co_collective (a, result_image, stat, errmsg, false);
}


bool
gfc_check_co_sum (gfc_expr *a, gfc_expr *result_image, gfc_expr *stat,
		  gfc_expr *errmsg)
{
  if (!numeric_check (a, 0))
    return false;
  return check_co_collective (a, result_image, stat, errmsg, false);
}


bool
gfc_check_complex (gfc_expr *x, gfc_expr *y)
{
  if (!boz_args_check (x, y))
    return false;

  if (x->ts.type == BT_BOZ)
    {
      if (gfc_invalid_boz ("BOZ constant at %L cannot appear in the COMPLEX "
			   "intrinsic subprogram", &x->where))
	{
	  reset_boz (x);
	  return false;
        }
      if (y->ts.type == BT_INTEGER && !gfc_boz2int (x, y->ts.kind))
	return false;
      if (y->ts.type == BT_REAL && !gfc_boz2real (x, y->ts.kind))
    	return false;
    }

  if (y->ts.type == BT_BOZ)
    {
      if (gfc_invalid_boz ("BOZ constant at %L cannot appear in the COMPLEX "
			   "intrinsic subprogram", &y->where))
	{
	  reset_boz (y);
	  return false;
	}
      if (x->ts.type == BT_INTEGER && !gfc_boz2int (y, x->ts.kind))
	return false;
      if (x->ts.type == BT_REAL && !gfc_boz2real (y, x->ts.kind))
	return false;
    }

  if (!int_or_real_check (x, 0))
    return false;
  if (!scalar_check (x, 0))
    return false;

  if (!int_or_real_check (y, 1))
    return false;
  if (!scalar_check (y, 1))
    return false;

  return true;
}


bool
gfc_check_count (gfc_expr *mask, gfc_expr *dim, gfc_expr *kind)
{
  if (!logical_array_check (mask, 0))
    return false;
  if (!dim_check (dim, 1, false))
    return false;
  if (!dim_rank_check (dim, mask, 0))
    return false;
  if (!kind_check (kind, 2, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

  return true;
}


bool
gfc_check_cshift (gfc_expr *array, gfc_expr *shift, gfc_expr *dim)
{
  if (!array_check (array, 0))
    return false;

  if (!type_check (shift, 1, BT_INTEGER))
    return false;

  if (!dim_check (dim, 2, true))
    return false;

  if (!dim_rank_check (dim, array, false))
    return false;

  if (array->rank == 1 || shift->rank == 0)
    {
      if (!scalar_check (shift, 1))
	return false;
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
		    gfc_error ("%qs argument of %qs intrinsic at %L has "
			       "invalid shape in dimension %d (%ld/%ld)",
			       gfc_current_intrinsic_arg[1]->name,
			       gfc_current_intrinsic, &shift->where, i + 1,
			       mpz_get_si (array->shape[i]),
			       mpz_get_si (shift->shape[j]));
		    return false;
		  }

		j += 1;
	      }
	}
    }
  else
    {
      gfc_error ("%qs argument of intrinsic %qs at %L of must have rank "
		 "%d or be a scalar", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &shift->where, array->rank - 1);
      return false;
    }

  return true;
}


bool
gfc_check_ctime (gfc_expr *time)
{
  if (!scalar_check (time, 0))
    return false;

  if (!type_check (time, 0, BT_INTEGER))
    return false;

  return true;
}


bool gfc_check_datan2 (gfc_expr *y, gfc_expr *x)
{
  if (!double_check (y, 0) || !double_check (x, 1))
    return false;

  return true;
}

bool
gfc_check_dcmplx (gfc_expr *x, gfc_expr *y)
{
  if (x->ts.type == BT_BOZ && !gfc_boz2real (x, gfc_default_double_kind))
    return false;

  if (!numeric_check (x, 0))
    return false;

  if (y != NULL)
    {
      if (y->ts.type == BT_BOZ && !gfc_boz2real (y, gfc_default_double_kind))
	return false;

      if (!numeric_check (y, 1))
	return false;

      if (x->ts.type == BT_COMPLEX)
	{
	  gfc_error ("%qs argument of %qs intrinsic at %L must not be "
		     "present if %<x%> is COMPLEX",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &y->where);
	  return false;
	}

      if (y->ts.type == BT_COMPLEX)
	{
	  gfc_error ("%qs argument of %qs intrinsic at %L must have a type "
		     "of either REAL or INTEGER",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &y->where);
	  return false;
	}
    }

  return true;
}


bool
gfc_check_dble (gfc_expr *x)
{
  if (x->ts.type == BT_BOZ && !gfc_boz2real (x, gfc_default_double_kind))
    return false;

  if (!numeric_check (x, 0))
    return false;

  return true;
}


bool
gfc_check_digits (gfc_expr *x)
{
  if (!int_or_real_check (x, 0))
    return false;

  return true;
}


bool
gfc_check_dot_product (gfc_expr *vector_a, gfc_expr *vector_b)
{
  switch (vector_a->ts.type)
    {
    case BT_LOGICAL:
      if (!type_check (vector_b, 1, BT_LOGICAL))
	return false;
      break;

    case BT_INTEGER:
    case BT_REAL:
    case BT_COMPLEX:
      if (!numeric_check (vector_b, 1))
	return false;
      break;

    default:
      gfc_error ("%qs argument of %qs intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &vector_a->where);
      return false;
    }

  if (!rank_check (vector_a, 0, 1))
    return false;

  if (!rank_check (vector_b, 1, 1))
    return false;

  if (! identical_dimen_shape (vector_a, 0, vector_b, 0))
    {
      gfc_error ("Different shape for arguments %qs and %qs at %L for "
		 "intrinsic %<dot_product%>",
		 gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic_arg[1]->name, &vector_a->where);
      return false;
    }

  return true;
}


bool
gfc_check_dprod (gfc_expr *x, gfc_expr *y)
{
  if (!type_check (x, 0, BT_REAL)
      || !type_check (y, 1, BT_REAL))
    return false;

  if (x->ts.kind != gfc_default_real_kind)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be default "
		 "real", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &x->where);
      return false;
    }

  if (y->ts.kind != gfc_default_real_kind)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be default "
		 "real", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &y->where);
      return false;
    }

  return true;
}

bool
gfc_check_dshift (gfc_expr *i, gfc_expr *j, gfc_expr *shift)
{
  /* i and j cannot both be BOZ literal constants.  */
  if (!boz_args_check (i, j))
    return false;

  /* If i is BOZ and j is integer, convert i to type of j.  If j is not
     an integer, clear the BOZ; otherwise, check that i is an integer.  */
  if (i->ts.type == BT_BOZ)
    {
      if (j->ts.type != BT_INTEGER)
        reset_boz (i);
      else if (!gfc_boz2int (i, j->ts.kind))
	return false;
    }
  else if (!type_check (i, 0, BT_INTEGER))
    {
      if (j->ts.type == BT_BOZ)
	reset_boz (j);
      return false;
    }

  /* If j is BOZ and i is integer, convert j to type of i.  If i is not
     an integer, clear the BOZ; otherwise, check that i is an integer.  */
  if (j->ts.type == BT_BOZ)
    {
      if (i->ts.type != BT_INTEGER)
        reset_boz (j);
      else if (!gfc_boz2int (j, i->ts.kind))
	return false;
    }
  else if (!type_check (j, 1, BT_INTEGER))
    return false;

  if (!same_type_check (i, 0, j, 1))
    return false;

  if (!type_check (shift, 2, BT_INTEGER))
    return false;

  if (!nonnegative_check ("SHIFT", shift))
    return false;

  if (!less_than_bitsize1 ("I", i, "SHIFT", shift, true))
    return false;

  return true;
}


bool
gfc_check_eoshift (gfc_expr *array, gfc_expr *shift, gfc_expr *boundary,
		   gfc_expr *dim)
{
  int d;

  if (!array_check (array, 0))
    return false;

  if (!type_check (shift, 1, BT_INTEGER))
    return false;

  if (!dim_check (dim, 3, true))
    return false;

  if (!dim_rank_check (dim, array, false))
    return false;

  if (!dim)
    d = 1;
  else if (dim->expr_type == EXPR_CONSTANT)
    gfc_extract_int (dim, &d);
  else
    d = -1;

  if (array->rank == 1 || shift->rank == 0)
    {
      if (!scalar_check (shift, 1))
	return false;
    }
  else if (shift->rank == array->rank - 1)
    {
      if (d > 0)
	{
	  int i, j;
	  for (i = 0, j = 0; i < array->rank; i++)
	    if (i != d - 1)
	      {
		if (!identical_dimen_shape (array, i, shift, j))
		  {
		    gfc_error ("%qs argument of %qs intrinsic at %L has "
			       "invalid shape in dimension %d (%ld/%ld)",
			       gfc_current_intrinsic_arg[1]->name,
			       gfc_current_intrinsic, &shift->where, i + 1,
			       mpz_get_si (array->shape[i]),
			       mpz_get_si (shift->shape[j]));
		    return false;
		  }

		j += 1;
	      }
	}
    }
  else
    {
      gfc_error ("%qs argument of intrinsic %qs at %L of must have rank "
		 "%d or be a scalar", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &shift->where, array->rank - 1);
      return false;
    }

  if (boundary != NULL)
    {
      if (!same_type_check (array, 0, boundary, 2))
	return false;

      /* Reject unequal string lengths and emit a better error message than
       gfc_check_same_strlen would.  */
      if (array->ts.type == BT_CHARACTER)
	{
	  ssize_t len_a, len_b;

	  len_a = gfc_var_strlen (array);
	  len_b = gfc_var_strlen (boundary);
	  if (len_a != -1 && len_b != -1 && len_a != len_b)
	    {
	      gfc_error ("%qs must be of same type and kind as %qs at %L in %qs",
			 gfc_current_intrinsic_arg[2]->name,
			 gfc_current_intrinsic_arg[0]->name,
			 &boundary->where, gfc_current_intrinsic);
	      return false;
	    }
	}

      if (array->rank == 1 || boundary->rank == 0)
	{
	  if (!scalar_check (boundary, 2))
	    return false;
	}
      else if (boundary->rank == array->rank - 1)
	{
	  if (d > 0)
	    {
	      int i,j;
	      for (i = 0, j = 0; i < array->rank; i++)
		{
		  if (i != d - 1)
		    {
		      if (!identical_dimen_shape (array, i, boundary, j))
			{
			  gfc_error ("%qs argument of %qs intrinsic at %L has "
				     "invalid shape in dimension %d (%ld/%ld)",
				     gfc_current_intrinsic_arg[2]->name,
				     gfc_current_intrinsic, &shift->where, i+1,
				     mpz_get_si (array->shape[i]),
				     mpz_get_si (boundary->shape[j]));
			  return false;
			}
		      j += 1;
		    }
		}
	    }
	}
      else
	{
	  gfc_error ("%qs argument of intrinsic %qs at %L of must have "
		     "rank %d or be a scalar",
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &shift->where, array->rank - 1);
	  return false;
	}
    }
  else
    {
      switch (array->ts.type)
	{
	case BT_INTEGER:
	case BT_LOGICAL:
	case BT_REAL:
	case BT_COMPLEX:
	case BT_CHARACTER:
	  break;

	default:
	  gfc_error ("Missing %qs argument to %qs intrinsic at %L for %qs "
		     "of type %qs", gfc_current_intrinsic_arg[2]->name,
		     gfc_current_intrinsic, &array->where,
		     gfc_current_intrinsic_arg[0]->name,
		     gfc_typename (array));
	  return false;
	}
    }

  return true;
}


bool
gfc_check_float (gfc_expr *a)
{
  if (a->ts.type == BT_BOZ)
    {
      if (gfc_invalid_boz ("BOZ literal constant at %L cannot appear in the "
			   "FLOAT intrinsic subprogram", &a->where))
	{
	  reset_boz (a);
	  return false;
	}
      if (!gfc_boz2int (a, gfc_default_integer_kind))
	return false;
    }

  if (!type_check (a, 0, BT_INTEGER))
    return false;

  if ((a->ts.kind != gfc_default_integer_kind)
      && !gfc_notify_std (GFC_STD_GNU, "non-default INTEGER "
			  "kind argument to %s intrinsic at %L",
			  gfc_current_intrinsic, &a->where))
    return false;

  return true;
}

/* A single complex argument.  */

bool
gfc_check_fn_c (gfc_expr *a)
{
  if (!type_check (a, 0, BT_COMPLEX))
    return false;

  return true;
}


/* A single real argument.  */

bool
gfc_check_fn_r (gfc_expr *a)
{
  if (!type_check (a, 0, BT_REAL))
    return false;

  return true;
}

/* A single double argument.  */

bool
gfc_check_fn_d (gfc_expr *a)
{
  if (!double_check (a, 0))
    return false;

  return true;
}

/* A single real or complex argument.  */

bool
gfc_check_fn_rc (gfc_expr *a)
{
  if (!real_or_complex_check (a, 0))
    return false;

  return true;
}


bool
gfc_check_fn_rc2008 (gfc_expr *a)
{
  if (!real_or_complex_check (a, 0))
    return false;

  if (a->ts.type == BT_COMPLEX
      && !gfc_notify_std (GFC_STD_F2008, "COMPLEX argument %qs "
			  "of %qs intrinsic at %L",
			  gfc_current_intrinsic_arg[0]->name,
			  gfc_current_intrinsic, &a->where))
    return false;

  return true;
}


bool
gfc_check_fnum (gfc_expr *unit)
{
  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  return true;
}


bool
gfc_check_huge (gfc_expr *x)
{
  if (!int_or_real_check (x, 0))
    return false;

  return true;
}


bool
gfc_check_hypot (gfc_expr *x, gfc_expr *y)
{
  if (!type_check (x, 0, BT_REAL))
    return false;
  if (!same_type_check (x, 0, y, 1))
    return false;

  return true;
}


/* Check that the single argument is an integer.  */

bool
gfc_check_i (gfc_expr *i)
{
  if (!type_check (i, 0, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_iand_ieor_ior (gfc_expr *i, gfc_expr *j)
{
  /* i and j cannot both be BOZ literal constants.  */
  if (!boz_args_check (i, j))
    return false;

  /* If i is BOZ and j is integer, convert i to type of j.  */
  if (i->ts.type == BT_BOZ && j->ts.type == BT_INTEGER
      && !gfc_boz2int (i, j->ts.kind))
    return false;

  /* If j is BOZ and i is integer, convert j to type of i.  */
  if (j->ts.type == BT_BOZ && i->ts.type == BT_INTEGER
      && !gfc_boz2int (j, i->ts.kind))
    return false;

  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!type_check (j, 1, BT_INTEGER))
    return false;

  if (i->ts.kind != j->ts.kind)
    {
      gfc_error ("Arguments of %qs have different kind type parameters "
		 "at %L", gfc_current_intrinsic, &i->where);
	return false;
    }

  return true;
}


bool
gfc_check_ibits (gfc_expr *i, gfc_expr *pos, gfc_expr *len)
{
  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!type_check (pos, 1, BT_INTEGER))
    return false;

  if (!type_check (len, 2, BT_INTEGER))
    return false;

  if (!nonnegative_check ("pos", pos))
    return false;

  if (!nonnegative_check ("len", len))
    return false;

  if (!less_than_bitsize2 ("i", i, "pos", pos, "len", len))
    return false;

  return true;
}


bool
gfc_check_ichar_iachar (gfc_expr *c, gfc_expr *kind)
{
  int i;

  if (!type_check (c, 0, BT_CHARACTER))
    return false;

  if (!kind_check (kind, 1, BT_INTEGER))
    return false;

  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

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
		return true;
	      i = mpz_get_si (c->ts.u.cl->length->value.integer);
	    }
	  else
	    return true;
	}
      else
	{
	  start = ref->u.ss.start;
	  end = ref->u.ss.end;

	  gcc_assert (start);
	  if (end == NULL || end->expr_type != EXPR_CONSTANT
	      || start->expr_type != EXPR_CONSTANT)
	    return true;

	  i = mpz_get_si (end->value.integer) + 1
	    - mpz_get_si (start->value.integer);
	}
    }
  else
    return true;

  if (i != 1)
    {
      gfc_error ("Argument of %s at %L must be of length one",
		 gfc_current_intrinsic, &c->where);
      return false;
    }

  return true;
}


bool
gfc_check_idnint (gfc_expr *a)
{
  if (!double_check (a, 0))
    return false;

  return true;
}


bool
gfc_check_index (gfc_expr *string, gfc_expr *substring, gfc_expr *back,
		 gfc_expr *kind)
{
  if (!type_check (string, 0, BT_CHARACTER)
      || !type_check (substring, 1, BT_CHARACTER))
    return false;

  if (back != NULL && !type_check (back, 2, BT_LOGICAL))
    return false;

  if (!kind_check (kind, 3, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

  if (string->ts.kind != substring->ts.kind)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be the same "
		 "kind as %qs", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &substring->where,
		 gfc_current_intrinsic_arg[0]->name);
      return false;
    }

  return true;
}


bool
gfc_check_int (gfc_expr *x, gfc_expr *kind)
{
  /* BOZ is dealt within simplify_int*.  */
  if (x->ts.type == BT_BOZ)
    return true;

  if (!numeric_check (x, 0))
    return false;

  if (!kind_check (kind, 1, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_intconv (gfc_expr *x)
{
  if (strcmp (gfc_current_intrinsic, "short") == 0
      || strcmp (gfc_current_intrinsic, "long") == 0)
    {
      gfc_error ("%qs intrinsic subprogram at %L has been deprecated.  "
		 "Use INT intrinsic subprogram.", gfc_current_intrinsic, 
		 &x->where);
      return false;
    }

  /* BOZ is dealt within simplify_int*.  */
  if (x->ts.type == BT_BOZ)
    return true;

  if (!numeric_check (x, 0))
    return false;

  return true;
}

bool
gfc_check_ishft (gfc_expr *i, gfc_expr *shift)
{
  if (!type_check (i, 0, BT_INTEGER)
      || !type_check (shift, 1, BT_INTEGER))
    return false;

  if (!less_than_bitsize1 ("I", i, NULL, shift, true))
    return false;

  return true;
}


bool
gfc_check_ishftc (gfc_expr *i, gfc_expr *shift, gfc_expr *size)
{
  if (!type_check (i, 0, BT_INTEGER)
      || !type_check (shift, 1, BT_INTEGER))
    return false;

  if (size != NULL)
    {
      int i2, i3;

      if (!type_check (size, 2, BT_INTEGER))
	return false;

      if (!less_than_bitsize1 ("I", i, "SIZE", size, true))
	return false;

      if (size->expr_type == EXPR_CONSTANT)
	{
	  gfc_extract_int (size, &i3);
	  if (i3 <= 0)
	    {
	      gfc_error ("SIZE at %L must be positive", &size->where);
	      return false;
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
		  return false;
		}
	     }
	}
    }
  else if (!less_than_bitsize1 ("I", i, NULL, shift, true))
    return false;

  return true;
}


bool
gfc_check_kill (gfc_expr *pid, gfc_expr *sig)
{
  if (!type_check (pid, 0, BT_INTEGER))
    return false;

  if (!scalar_check (pid, 0))
    return false;

  if (!type_check (sig, 1, BT_INTEGER))
    return false;

  if (!scalar_check (sig, 1))
    return false;

  return true;
}


bool
gfc_check_kill_sub (gfc_expr *pid, gfc_expr *sig, gfc_expr *status)
{
  if (!type_check (pid, 0, BT_INTEGER))
    return false;

  if (!scalar_check (pid, 0))
    return false;

  if (!type_check (sig, 1, BT_INTEGER))
    return false;

  if (!scalar_check (sig, 1))
    return false;

  if (status)
    {
      if (!type_check (status, 2, BT_INTEGER))
	return false;

      if (!scalar_check (status, 2))
	return false;

      if (status->expr_type != EXPR_VARIABLE)
	{
	  gfc_error ("STATUS at %L shall be an INTENT(OUT) variable",
		     &status->where);
	  return false;
	}

      if (status->expr_type == EXPR_VARIABLE
	  && status->symtree && status->symtree->n.sym
	  && status->symtree->n.sym->attr.intent == INTENT_IN)
	{
	  gfc_error ("%qs at %L shall be an INTENT(OUT) variable",
		     status->symtree->name, &status->where);
	  return false;
	}
    }

  return true;
}


bool
gfc_check_kind (gfc_expr *x)
{
  if (gfc_bt_struct (x->ts.type) || x->ts.type == BT_CLASS)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be of "
		 "intrinsic type", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &x->where);
      return false;
    }
  if (x->ts.type == BT_PROCEDURE)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be a data entity",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &x->where);
      return false;
    }

  return true;
}


bool
gfc_check_lbound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  if (!array_check (array, 0))
    return false;

  if (!dim_check (dim, 1, false))
    return false;

  if (!dim_rank_check (dim, array, 1))
    return false;

  if (!kind_check (kind, 2, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

  return true;
}


bool
gfc_check_lcobound (gfc_expr *coarray, gfc_expr *dim, gfc_expr *kind)
{
  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      return false;
    }

  if (!coarray_check (coarray, 0))
    return false;

  if (dim != NULL)
    {
      if (!dim_check (dim, 1, false))
        return false;

      if (!dim_corank_check (dim, coarray))
        return false;
    }

  if (!kind_check (kind, 2, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_len_lentrim (gfc_expr *s, gfc_expr *kind)
{
  if (!type_check (s, 0, BT_CHARACTER))
    return false;

  if (!kind_check (kind, 1, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

  return true;
}


bool
gfc_check_lge_lgt_lle_llt (gfc_expr *a, gfc_expr *b)
{
  if (!type_check (a, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (a, 0, gfc_default_character_kind))
    return false;

  if (!type_check (b, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (b, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_link (gfc_expr *path1, gfc_expr *path2)
{
  if (!type_check (path1, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (path1, 0, gfc_default_character_kind))
    return false;

  if (!type_check (path2, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (path2, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_link_sub (gfc_expr *path1, gfc_expr *path2, gfc_expr *status)
{
  if (!type_check (path1, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (path1, 0, gfc_default_character_kind))
    return false;

  if (!type_check (path2, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (path2, 0, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 2, BT_INTEGER))
    return false;

  if (!scalar_check (status, 2))
    return false;

  return true;
}


bool
gfc_check_loc (gfc_expr *expr)
{
  return variable_check (expr, 0, true);
}


bool
gfc_check_symlnk (gfc_expr *path1, gfc_expr *path2)
{
  if (!type_check (path1, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (path1, 0, gfc_default_character_kind))
    return false;

  if (!type_check (path2, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (path2, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_symlnk_sub (gfc_expr *path1, gfc_expr *path2, gfc_expr *status)
{
  if (!type_check (path1, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (path1, 0, gfc_default_character_kind))
    return false;

  if (!type_check (path2, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (path2, 1, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 2, BT_INTEGER))
    return false;

  if (!scalar_check (status, 2))
    return false;

  return true;
}


bool
gfc_check_logical (gfc_expr *a, gfc_expr *kind)
{
  if (!type_check (a, 0, BT_LOGICAL))
    return false;
  if (!kind_check (kind, 1, BT_LOGICAL))
    return false;

  return true;
}


/* Min/max family.  */

static bool
min_max_args (gfc_actual_arglist *args)
{
  gfc_actual_arglist *arg;
  int i, j, nargs, *nlabels, nlabelless;
  bool a1 = false, a2 = false;

  if (args == NULL || args->next == NULL)
    {
      gfc_error ("Intrinsic %qs at %L must have at least two arguments",
		 gfc_current_intrinsic, gfc_current_intrinsic_where);
      return false;
    }

  if (!args->name)
    a1 = true;

  if (!args->next->name)
    a2 = true;

  nargs = 0;
  for (arg = args; arg; arg = arg->next)
    if (arg->name)
      nargs++;

  if (nargs == 0)
    return true;

  /* Note: Having a keywordless argument after an "arg=" is checked before.  */
  nlabelless = 0;
  nlabels = XALLOCAVEC (int, nargs);
  for (arg = args, i = 0; arg; arg = arg->next, i++)
    if (arg->name)
      {
	int n;
	char *endp;

	if (arg->name[0] != 'a' || arg->name[1] < '1' || arg->name[1] > '9')
	  goto unknown;
	n = strtol (&arg->name[1], &endp, 10);
	if (endp[0] != '\0')
	  goto unknown;
	if (n <= 0)
	  goto unknown;
	if (n <= nlabelless)
	  goto duplicate;
	nlabels[i] = n;
	if (n == 1)
	  a1 = true;
	if (n == 2)
	  a2 = true;
      }
    else
      nlabelless++;

  if (!a1 || !a2)
    {
      gfc_error ("Missing %qs argument to the %s intrinsic at %L",
	         !a1 ? "a1" : "a2", gfc_current_intrinsic,
		 gfc_current_intrinsic_where);
      return false;
    }

  /* Check for duplicates.  */
  for (i = 0; i < nargs; i++)
    for (j = i + 1; j < nargs; j++)
      if (nlabels[i] == nlabels[j])
	goto duplicate;

  return true;

duplicate:
  gfc_error ("Duplicate argument %qs at %L to intrinsic %s", arg->name,
	     &arg->expr->where, gfc_current_intrinsic);
  return false;

unknown:
  gfc_error ("Unknown argument %qs at %L to intrinsic %s", arg->name,
	     &arg->expr->where, gfc_current_intrinsic);
  return false;
}


static bool
check_rest (bt type, int kind, gfc_actual_arglist *arglist)
{
  gfc_actual_arglist *arg, *tmp;
  gfc_expr *x;
  int m, n;

  if (!min_max_args (arglist))
    return false;

  for (arg = arglist, n=1; arg; arg = arg->next, n++)
    {
      x = arg->expr;
      if (x->ts.type != type || x->ts.kind != kind)
	{
	  if (x->ts.type == type)
	    {
	      if (!gfc_notify_std (GFC_STD_GNU, "Different type "
				   "kinds at %L", &x->where))
		return false;
	    }
	  else
	    {
	      gfc_error ("%<a%d%> argument of %qs intrinsic at %L must be "
			 "%s(%d)", n, gfc_current_intrinsic, &x->where,
			 gfc_basic_typename (type), kind);
	      return false;
	    }
	}

      for (tmp = arglist, m=1; tmp != arg; tmp = tmp->next, m++)
	if (!gfc_check_conformance (tmp->expr, x,
				    "arguments 'a%d' and 'a%d' for "
				    "intrinsic '%s'", m, n,
				    gfc_current_intrinsic))
	    return false;
    }

  return true;
}


bool
gfc_check_min_max (gfc_actual_arglist *arg)
{
  gfc_expr *x;

  if (!min_max_args (arg))
    return false;

  x = arg->expr;

  if (x->ts.type == BT_CHARACTER)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			   "with CHARACTER argument at %L",
			   gfc_current_intrinsic, &x->where))
	return false;
    }
  else if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL)
    {
      gfc_error ("%<a1%> argument of %qs intrinsic at %L must be INTEGER, "
		 "REAL or CHARACTER", gfc_current_intrinsic, &x->where);
      return false;
    }

  return check_rest (x->ts.type, x->ts.kind, arg);
}


bool
gfc_check_min_max_integer (gfc_actual_arglist *arg)
{
  return check_rest (BT_INTEGER, gfc_default_integer_kind, arg);
}


bool
gfc_check_min_max_real (gfc_actual_arglist *arg)
{
  return check_rest (BT_REAL, gfc_default_real_kind, arg);
}


bool
gfc_check_min_max_double (gfc_actual_arglist *arg)
{
  return check_rest (BT_REAL, gfc_default_double_kind, arg);
}


/* End of min/max family.  */

bool
gfc_check_malloc (gfc_expr *size)
{
  if (!type_check (size, 0, BT_INTEGER))
    return false;

  if (!scalar_check (size, 0))
    return false;

  return true;
}


bool
gfc_check_matmul (gfc_expr *matrix_a, gfc_expr *matrix_b)
{
  if ((matrix_a->ts.type != BT_LOGICAL) && !gfc_numeric_ts (&matrix_a->ts))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &matrix_a->where);
      return false;
    }

  if ((matrix_b->ts.type != BT_LOGICAL) && !gfc_numeric_ts (&matrix_b->ts))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be numeric "
		 "or LOGICAL", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &matrix_b->where);
      return false;
    }

  if ((matrix_a->ts.type == BT_LOGICAL && gfc_numeric_ts (&matrix_b->ts))
      || (gfc_numeric_ts (&matrix_a->ts) && matrix_b->ts.type == BT_LOGICAL))
    {
      gfc_error ("Argument types of %qs intrinsic at %L must match (%s/%s)",
		 gfc_current_intrinsic, &matrix_a->where,
		 gfc_typename(&matrix_a->ts), gfc_typename(&matrix_b->ts));
       return false;
    }

  switch (matrix_a->rank)
    {
    case 1:
      if (!rank_check (matrix_b, 1, 2))
	return false;
      /* Check for case matrix_a has shape(m), matrix_b has shape (m, k).  */
      if (!identical_dimen_shape (matrix_a, 0, matrix_b, 0))
	{
	  gfc_error ("Different shape on dimension 1 for arguments %qs "
		     "and %qs at %L for intrinsic matmul",
		     gfc_current_intrinsic_arg[0]->name,
		     gfc_current_intrinsic_arg[1]->name, &matrix_a->where);
	  return false;
	}
      break;

    case 2:
      if (matrix_b->rank != 2)
	{
	  if (!rank_check (matrix_b, 1, 1))
	    return false;
	}
      /* matrix_b has rank 1 or 2 here. Common check for the cases
	 - matrix_a has shape (n,m) and matrix_b has shape (m, k)
	 - matrix_a has shape (n,m) and matrix_b has shape (m).  */
      if (!identical_dimen_shape (matrix_a, 1, matrix_b, 0))
	{
	  gfc_error ("Different shape on dimension 2 for argument %qs and "
		     "dimension 1 for argument %qs at %L for intrinsic "
		     "matmul", gfc_current_intrinsic_arg[0]->name,
		     gfc_current_intrinsic_arg[1]->name, &matrix_a->where);
	  return false;
	}
      break;

    default:
      gfc_error ("%qs argument of %qs intrinsic at %L must be of rank "
		 "1 or 2", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &matrix_a->where);
      return false;
    }

  return true;
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
   position of the argument list and we'll have to fix that up.  Also,
   add the BACK argument if that isn't present.  */

bool
gfc_check_minloc_maxloc (gfc_actual_arglist *ap)
{
  gfc_expr *a, *m, *d, *k, *b;

  a = ap->expr;
  if (!int_or_real_or_char_check_f2003 (a, 0) || !array_check (a, 0))
    return false;

  d = ap->next->expr;
  m = ap->next->next->expr;
  k = ap->next->next->next->expr;
  b = ap->next->next->next->next->expr;

  if (b)
    {
      if (!type_check (b, 4, BT_LOGICAL) || !scalar_check (b,4))
	return false;
    }
  else
    {
      b = gfc_get_logical_expr (gfc_logical_4_kind, NULL, 0);
      ap->next->next->next->next->expr = b;
    }

  if (m == NULL && d != NULL && d->ts.type == BT_LOGICAL
      && ap->next->name == NULL)
    {
      m = d;
      d = NULL;
      ap->next->expr = NULL;
      ap->next->next->expr = m;
    }

  if (!dim_check (d, 1, false))
    return false;

  if (!dim_rank_check (d, a, 0))
    return false;

  if (m != NULL && !type_check (m, 2, BT_LOGICAL))
    return false;

  if (m != NULL
      && !gfc_check_conformance (a, m,
				 "arguments '%s' and '%s' for intrinsic %s",
				 gfc_current_intrinsic_arg[0]->name,
				 gfc_current_intrinsic_arg[2]->name,
				 gfc_current_intrinsic))
    return false;

  if (!kind_check (k, 1, BT_INTEGER))
    return false;

  return true;
}

/* Check function for findloc.  Mostly like gfc_check_minloc_maxloc
   above, with the additional "value" argument.  */

bool
gfc_check_findloc (gfc_actual_arglist *ap)
{
  gfc_expr *a, *v, *m, *d, *k, *b;
  bool a1, v1;

  a = ap->expr;
  if (!intrinsic_type_check (a, 0) || !array_check (a, 0))
    return false;

  v = ap->next->expr;
  if (!intrinsic_type_check (v, 1) || !scalar_check (v,1))
    return false;

  /* Check if the type are both logical.  */
  a1 = a->ts.type == BT_LOGICAL;
  v1 = v->ts.type == BT_LOGICAL;
  if ((a1 && !v1) || (!a1 && v1))
    goto incompat;

  /* Check if the type are both character.  */
  a1 = a->ts.type == BT_CHARACTER;
  v1 = v->ts.type == BT_CHARACTER;
  if ((a1 && !v1) || (!a1 && v1))
    goto incompat;

  /* Check the kind of the characters argument match.  */
  if (a1 && v1 && a->ts.kind != v->ts.kind)
    goto incompat;
	 
  d = ap->next->next->expr;
  m = ap->next->next->next->expr;
  k = ap->next->next->next->next->expr;
  b = ap->next->next->next->next->next->expr;

  if (b)
    {
      if (!type_check (b, 5, BT_LOGICAL) || !scalar_check (b,4))
	return false;
    }
  else
    {
      b = gfc_get_logical_expr (gfc_logical_4_kind, NULL, 0);
      ap->next->next->next->next->next->expr = b;
    }

  if (m == NULL && d != NULL && d->ts.type == BT_LOGICAL
      && ap->next->name == NULL)
    {
      m = d;
      d = NULL;
      ap->next->next->expr = NULL;
      ap->next->next->next->expr = m;
    }

  if (!dim_check (d, 2, false))
    return false;

  if (!dim_rank_check (d, a, 0))
    return false;

  if (m != NULL && !type_check (m, 3, BT_LOGICAL))
    return false;

  if (m != NULL
      && !gfc_check_conformance (a, m,
				 "arguments '%s' and '%s' for intrinsic %s",
				 gfc_current_intrinsic_arg[0]->name,
				 gfc_current_intrinsic_arg[3]->name,
				 gfc_current_intrinsic))
    return false;

  if (!kind_check (k, 1, BT_INTEGER))
    return false;

  return true;

incompat:
  gfc_error ("Argument %qs of %qs intrinsic at %L must be in type "
	     "conformance to argument %qs at %L",
	     gfc_current_intrinsic_arg[0]->name,
	     gfc_current_intrinsic, &a->where,
	     gfc_current_intrinsic_arg[1]->name, &v->where);
  return false;
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

static bool
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

  if (!dim_check (d, 1, false))
    return false;

  if (!dim_rank_check (d, a, 0))
    return false;

  if (m != NULL && !type_check (m, 2, BT_LOGICAL))
    return false;

  if (m != NULL
      && !gfc_check_conformance (a, m,
				 "arguments '%s' and '%s' for intrinsic %s",
				 gfc_current_intrinsic_arg[0]->name,
				 gfc_current_intrinsic_arg[2]->name,
				 gfc_current_intrinsic))
    return false;

  return true;
}


bool
gfc_check_minval_maxval (gfc_actual_arglist *ap)
{
  if (!int_or_real_or_char_check_f2003 (ap->expr, 0)
      || !array_check (ap->expr, 0))
    return false;

  return check_reduction (ap);
}


bool
gfc_check_product_sum (gfc_actual_arglist *ap)
{
  if (!numeric_check (ap->expr, 0)
      || !array_check (ap->expr, 0))
    return false;

  return check_reduction (ap);
}


/* For IANY, IALL and IPARITY.  */

bool
gfc_check_mask (gfc_expr *i, gfc_expr *kind)
{
  int k;

  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!nonnegative_check ("I", i))
    return false;

  if (!kind_check (kind, 1, BT_INTEGER))
    return false;

  if (kind)
    gfc_extract_int (kind, &k);
  else
    k = gfc_default_integer_kind;

  if (!less_than_bitsizekind ("I", i, k))
    return false;

  return true;
}


bool
gfc_check_transf_bit_intrins (gfc_actual_arglist *ap)
{
  if (ap->expr->ts.type != BT_INTEGER)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER",
                 gfc_current_intrinsic_arg[0]->name,
                 gfc_current_intrinsic, &ap->expr->where);
      return false;
    }

  if (!array_check (ap->expr, 0))
    return false;

  return check_reduction (ap);
}


bool
gfc_check_merge (gfc_expr *tsource, gfc_expr *fsource, gfc_expr *mask)
{
  if (!same_type_check (tsource, 0, fsource, 1))
    return false;

  if (!type_check (mask, 2, BT_LOGICAL))
    return false;

  if (tsource->ts.type == BT_CHARACTER)
    return gfc_check_same_strlen (tsource, fsource, "MERGE intrinsic");

  return true;
}


bool
gfc_check_merge_bits (gfc_expr *i, gfc_expr *j, gfc_expr *mask)
{
  /* i and j cannot both be BOZ literal constants.  */
  if (!boz_args_check (i, j))
    return false;

  /* If i is BOZ and j is integer, convert i to type of j.  */
  if (i->ts.type == BT_BOZ && j->ts.type == BT_INTEGER
      && !gfc_boz2int (i, j->ts.kind))
    return false;

  /* If j is BOZ and i is integer, convert j to type of i.  */
  if (j->ts.type == BT_BOZ && i->ts.type == BT_INTEGER
      && !gfc_boz2int (j, i->ts.kind))
    return false;

  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!type_check (j, 1, BT_INTEGER))
    return false;

  if (!same_type_check (i, 0, j, 1))
    return false;

  if (mask->ts.type == BT_BOZ && !gfc_boz2int(mask, i->ts.kind))
    return false;

  if (!type_check (mask, 2, BT_INTEGER))
    return false;

  if (!same_type_check (i, 0, mask, 2))
    return false;

  return true;
}


bool
gfc_check_move_alloc (gfc_expr *from, gfc_expr *to)
{
  if (!variable_check (from, 0, false))
    return false;
  if (!allocatable_check (from, 0))
    return false;
  if (gfc_is_coindexed (from))
    {
      gfc_error ("The FROM argument to MOVE_ALLOC at %L shall not be "
		 "coindexed", &from->where);
      return false;
    }

  if (!variable_check (to, 1, false))
    return false;
  if (!allocatable_check (to, 1))
    return false;
  if (gfc_is_coindexed (to))
    {
      gfc_error ("The TO argument to MOVE_ALLOC at %L shall not be "
		 "coindexed", &to->where);
      return false;
    }

  if (from->ts.type == BT_CLASS && to->ts.type == BT_DERIVED)
    {
      gfc_error ("The TO arguments in MOVE_ALLOC at %L must be "
		 "polymorphic if FROM is polymorphic",
		 &to->where);
      return false;
    }

  if (!same_type_check (to, 1, from, 0))
    return false;

  if (to->rank != from->rank)
    {
      gfc_error ("The FROM and TO arguments of the MOVE_ALLOC intrinsic at %L "
		 "must have the same rank %d/%d", &to->where,  from->rank,
		 to->rank);
      return false;
    }

  /* IR F08/0040; cf. 12-006A.  */
  if (gfc_get_corank (to) != gfc_get_corank (from))
    {
      gfc_error ("The FROM and TO arguments of the MOVE_ALLOC intrinsic at %L "
		 "must have the same corank %d/%d", &to->where,
		 gfc_get_corank (from), gfc_get_corank (to));
      return false;
    }

  /*  This is based losely on F2003 12.4.1.7. It is intended to prevent
      the likes of to = sym->cmp1->cmp2 and from = sym->cmp1, where cmp1
      and cmp2 are allocatable.  After the allocation is transferred,
      the 'to' chain is broken by the nullification of the 'from'. A bit
      of reflection reveals that this can only occur for derived types
      with recursive allocatable components.  */
  if (to->expr_type == EXPR_VARIABLE && from->expr_type == EXPR_VARIABLE
      && !strcmp (to->symtree->n.sym->name, from->symtree->n.sym->name))
    {
      gfc_ref *to_ref, *from_ref;
      to_ref = to->ref;
      from_ref = from->ref;
      bool aliasing = true;

      for (; from_ref && to_ref;
	   from_ref = from_ref->next, to_ref = to_ref->next)
	{
	  if (to_ref->type != from->ref->type)
	    aliasing = false;
	  else if (to_ref->type == REF_ARRAY
		   && to_ref->u.ar.type != AR_FULL
		   && from_ref->u.ar.type != AR_FULL)
	    /* Play safe; assume sections and elements are different.  */
	    aliasing = false;
	  else if (to_ref->type == REF_COMPONENT
		   && to_ref->u.c.component != from_ref->u.c.component)
	    aliasing = false;

	  if (!aliasing)
	    break;
	}

      if (aliasing)
	{
	  gfc_error ("The FROM and TO arguments at %L violate aliasing "
		     "restrictions (F2003 12.4.1.7)", &to->where);
	  return false;
	}
    }

  /* CLASS arguments: Make sure the vtab of from is present.  */
  if (to->ts.type == BT_CLASS && !UNLIMITED_POLY (from))
    gfc_find_vtab (&from->ts);

  return true;
}


bool
gfc_check_nearest (gfc_expr *x, gfc_expr *s)
{
  if (!type_check (x, 0, BT_REAL))
    return false;

  if (!type_check (s, 1, BT_REAL))
    return false;

  if (s->expr_type == EXPR_CONSTANT)
    {
      if (mpfr_sgn (s->value.real) == 0)
	{
	  gfc_error ("Argument %<S%> of NEAREST at %L shall not be zero",
		     &s->where);
	  return false;
	}
    }

  return true;
}


bool
gfc_check_new_line (gfc_expr *a)
{
  if (!type_check (a, 0, BT_CHARACTER))
    return false;

  return true;
}


bool
gfc_check_norm2 (gfc_expr *array, gfc_expr *dim)
{
  if (!type_check (array, 0, BT_REAL))
    return false;

  if (!array_check (array, 0))
    return false;

  if (!dim_rank_check (dim, array, false))
    return false;

  return true;
}

bool
gfc_check_null (gfc_expr *mold)
{
  symbol_attribute attr;

  if (mold == NULL)
    return true;

  if (!variable_check (mold, 0, true))
    return false;

  attr = gfc_variable_attr (mold, NULL);

  if (!attr.pointer && !attr.proc_pointer && !attr.allocatable)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be a POINTER, "
		 "ALLOCATABLE or procedure pointer",
		 gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &mold->where);
      return false;
    }

  if (attr.allocatable
      && !gfc_notify_std (GFC_STD_F2003, "NULL intrinsic with "
			  "allocatable MOLD at %L", &mold->where))
    return false;

  /* F2008, C1242.  */
  if (gfc_is_coindexed (mold))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be "
		 "coindexed", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &mold->where);
      return false;
    }

  return true;
}


bool
gfc_check_pack (gfc_expr *array, gfc_expr *mask, gfc_expr *vector)
{
  if (!array_check (array, 0))
    return false;

  if (!type_check (mask, 1, BT_LOGICAL))
    return false;

  if (!gfc_check_conformance (array, mask,
			      "arguments '%s' and '%s' for intrinsic '%s'",
			      gfc_current_intrinsic_arg[0]->name,
			      gfc_current_intrinsic_arg[1]->name,
			      gfc_current_intrinsic))
    return false;

  if (vector != NULL)
    {
      mpz_t array_size, vector_size;
      bool have_array_size, have_vector_size;

      if (!same_type_check (array, 0, vector, 2))
	return false;

      if (!rank_check (vector, 2, 1))
	return false;

      /* VECTOR requires at least as many elements as MASK
         has .TRUE. values.  */
      have_array_size = gfc_array_size(array, &array_size);
      have_vector_size = gfc_array_size(vector, &vector_size);

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
	      gfc_error ("%qs argument of %qs intrinsic at %L must "
			 "provide at least as many elements as there "
			 "are .TRUE. values in %qs (%ld/%d)",
			 gfc_current_intrinsic_arg[2]->name,
			 gfc_current_intrinsic, &vector->where,
			 gfc_current_intrinsic_arg[1]->name,
			 mpz_get_si (vector_size), mask_true_values);
	      return false;
	    }
	}

      if (have_array_size)
	mpz_clear (array_size);
      if (have_vector_size)
	mpz_clear (vector_size);
    }

  return true;
}


bool
gfc_check_parity (gfc_expr *mask, gfc_expr *dim)
{
  if (!type_check (mask, 0, BT_LOGICAL))
    return false;

  if (!array_check (mask, 0))
    return false;

  if (!dim_rank_check (dim, mask, false))
    return false;

  return true;
}


bool
gfc_check_precision (gfc_expr *x)
{
  if (!real_or_complex_check (x, 0))
    return false;

  return true;
}


bool
gfc_check_present (gfc_expr *a)
{
  gfc_symbol *sym;

  if (!variable_check (a, 0, true))
    return false;

  sym = a->symtree->n.sym;
  if (!sym->attr.dummy)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be of a "
		 "dummy variable", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &a->where);
      return false;
    }

  if (!sym->attr.optional)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be of "
		 "an OPTIONAL dummy variable",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &a->where);
      return false;
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
      gfc_error ("%qs argument of %qs intrinsic at %L must not be a "
		 "subobject of %qs", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &a->where, sym->name);
      return false;
    }

  return true;
}


bool
gfc_check_radix (gfc_expr *x)
{
  if (!int_or_real_check (x, 0))
    return false;

  return true;
}


bool
gfc_check_range (gfc_expr *x)
{
  if (!numeric_check (x, 0))
    return false;

  return true;
}


bool
gfc_check_rank (gfc_expr *a)
{
  /* Any data object is allowed; a "data object" is a "constant (4.1.3),
     variable (6), or subobject of a constant (2.4.3.2.3)" (F2008, 1.3.45).  */

  bool is_variable = true;

  /* Functions returning pointers are regarded as variable, cf. F2008, R602.  */
  if (a->expr_type == EXPR_FUNCTION)
    is_variable = a->value.function.esym
		  ? a->value.function.esym->result->attr.pointer
		  : a->symtree->n.sym->result->attr.pointer;

  if (a->expr_type == EXPR_OP
      || a->expr_type == EXPR_NULL
      || a->expr_type == EXPR_COMPCALL
      || a->expr_type == EXPR_PPC
      || a->ts.type == BT_PROCEDURE
      || !is_variable)
    {
      gfc_error ("The argument of the RANK intrinsic at %L must be a data "
		 "object", &a->where);
      return false;
    }

  return true;
}


bool
gfc_check_real (gfc_expr *a, gfc_expr *kind)
{
  if (!kind_check (kind, 1, BT_REAL))
    return false;

  /* BOZ is dealt with in gfc_simplify_real.  */
  if (a->ts.type == BT_BOZ)
    return true;

  if (!numeric_check (a, 0))
    return false;

  return true;
}


bool
gfc_check_rename (gfc_expr *path1, gfc_expr *path2)
{
  if (!type_check (path1, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (path1, 0, gfc_default_character_kind))
    return false;

  if (!type_check (path2, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (path2, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_rename_sub (gfc_expr *path1, gfc_expr *path2, gfc_expr *status)
{
  if (!type_check (path1, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (path1, 0, gfc_default_character_kind))
    return false;

  if (!type_check (path2, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (path2, 1, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 2, BT_INTEGER))
    return false;

  if (!scalar_check (status, 2))
    return false;

  return true;
}


bool
gfc_check_repeat (gfc_expr *x, gfc_expr *y)
{
  if (!type_check (x, 0, BT_CHARACTER))
    return false;

  if (!scalar_check (x, 0))
    return false;

  if (!type_check (y, 0, BT_INTEGER))
    return false;

  if (!scalar_check (y, 1))
    return false;

  return true;
}


bool
gfc_check_reshape (gfc_expr *source, gfc_expr *shape,
		   gfc_expr *pad, gfc_expr *order)
{
  mpz_t size;
  mpz_t nelems;
  int shape_size;

  if (!array_check (source, 0))
    return false;

  if (!rank_check (shape, 1, 1))
    return false;

  if (!type_check (shape, 1, BT_INTEGER))
    return false;

  if (!gfc_array_size (shape, &size))
    {
      gfc_error ("%<shape%> argument of %<reshape%> intrinsic at %L must be an "
		 "array of constant size", &shape->where);
      return false;
    }

  shape_size = mpz_get_ui (size);
  mpz_clear (size);

  if (shape_size <= 0)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L is empty",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &shape->where);
      return false;
    }
  else if (shape_size > GFC_MAX_DIMENSIONS)
    {
      gfc_error ("%<shape%> argument of %<reshape%> intrinsic at %L has more "
		 "than %d elements", &shape->where, GFC_MAX_DIMENSIONS);
      return false;
    }
  else if (shape->expr_type == EXPR_ARRAY && gfc_is_constant_expr (shape))
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
	      gfc_error ("%qs argument of %qs intrinsic at %L has "
			 "negative element (%d)",
			 gfc_current_intrinsic_arg[1]->name,
			 gfc_current_intrinsic, &e->where, extent);
	      return false;
	    }
	}
    }
  else if (shape->expr_type == EXPR_VARIABLE && shape->ref
	   && shape->ref->u.ar.type == AR_FULL && shape->ref->u.ar.dimen == 1
	   && shape->ref->u.ar.as
	   && shape->ref->u.ar.as->lower[0]->expr_type == EXPR_CONSTANT
	   && shape->ref->u.ar.as->lower[0]->ts.type == BT_INTEGER
	   && shape->ref->u.ar.as->upper[0]->expr_type == EXPR_CONSTANT
	   && shape->ref->u.ar.as->upper[0]->ts.type == BT_INTEGER
	   && shape->symtree->n.sym->attr.flavor == FL_PARAMETER)
    {
      int i, extent;
      gfc_expr *e, *v;

      v = shape->symtree->n.sym->value;

      for (i = 0; i < shape_size; i++)
	{
	  e = gfc_constructor_lookup_expr (v->value.constructor, i);
	  if (e == NULL)
	     break;

	  gfc_extract_int (e, &extent);

	  if (extent < 0)
	    {
	      gfc_error ("Element %d of actual argument of RESHAPE at %L "
			 "cannot be negative", i + 1, &shape->where);
	      return false;
	    }
	}
    }

  if (pad != NULL)
    {
      if (!same_type_check (source, 0, pad, 2))
	return false;

      if (!array_check (pad, 2))
	return false;
    }

  if (order != NULL)
    {
      if (!array_check (order, 3))
	return false;

      if (!type_check (order, 3, BT_INTEGER))
	return false;

      if (order->expr_type == EXPR_ARRAY && gfc_is_constant_expr (order))
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
	      gfc_error ("%qs argument of %qs intrinsic at %L "
			 "has wrong number of elements (%d/%d)",
			 gfc_current_intrinsic_arg[3]->name,
			 gfc_current_intrinsic, &order->where,
			 order_size, shape_size);
	      return false;
	    }

	  for (i = 1; i <= order_size; ++i)
	    {
	      e = gfc_constructor_lookup_expr (order->value.constructor, i-1);
	      if (e->expr_type != EXPR_CONSTANT)
		continue;

	      gfc_extract_int (e, &dim);

	      if (dim < 1 || dim > order_size)
		{
		  gfc_error ("%qs argument of %qs intrinsic at %L "
			     "has out-of-range dimension (%d)",
			     gfc_current_intrinsic_arg[3]->name,
			     gfc_current_intrinsic, &e->where, dim);
		  return false;
		}

	      if (perm[dim-1] != 0)
		{
		  gfc_error ("%qs argument of %qs intrinsic at %L has "
			     "invalid permutation of dimensions (dimension "
			     "%qd duplicated)",
			     gfc_current_intrinsic_arg[3]->name,
			     gfc_current_intrinsic, &e->where, dim);
		  return false;
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
      if (gfc_array_size (source, &nelems))
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
	      return false;
	    }
	}
    }

  return true;
}


bool
gfc_check_same_type_as (gfc_expr *a, gfc_expr *b)
{
  if (a->ts.type != BT_DERIVED && a->ts.type != BT_CLASS)
    {
        gfc_error ("%qs argument of %qs intrinsic at %L "
		   "cannot be of type %s",
		   gfc_current_intrinsic_arg[0]->name,
		   gfc_current_intrinsic,
		   &a->where, gfc_typename (a));
        return false;
    }

  if (!(gfc_type_is_extensible (a->ts.u.derived) || UNLIMITED_POLY (a)))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L "
		 "must be of an extensible type",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &a->where);
      return false;
    }

  if (b->ts.type != BT_DERIVED && b->ts.type != BT_CLASS)
    {
        gfc_error ("%qs argument of %qs intrinsic at %L "
		   "cannot be of type %s",
		   gfc_current_intrinsic_arg[0]->name,
		   gfc_current_intrinsic,
		   &b->where, gfc_typename (b));
      return false;
    }

  if (!(gfc_type_is_extensible (b->ts.u.derived) || UNLIMITED_POLY (b)))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L "
		 "must be of an extensible type",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &b->where);
      return false;
    }

  return true;
}


bool
gfc_check_scale (gfc_expr *x, gfc_expr *i)
{
  if (!type_check (x, 0, BT_REAL))
    return false;

  if (!type_check (i, 1, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_scan (gfc_expr *x, gfc_expr *y, gfc_expr *z, gfc_expr *kind)
{
  if (!type_check (x, 0, BT_CHARACTER))
    return false;

  if (!type_check (y, 1, BT_CHARACTER))
    return false;

  if (z != NULL && !type_check (z, 2, BT_LOGICAL))
    return false;

  if (!kind_check (kind, 3, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

  if (!same_type_check (x, 0, y, 1))
    return false;

  return true;
}


bool
gfc_check_secnds (gfc_expr *r)
{
  if (!type_check (r, 0, BT_REAL))
    return false;

  if (!kind_value_check (r, 0, 4))
    return false;

  if (!scalar_check (r, 0))
    return false;

  return true;
}


bool
gfc_check_selected_char_kind (gfc_expr *name)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;

  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  if (!scalar_check (name, 0))
    return false;

  return true;
}


bool
gfc_check_selected_int_kind (gfc_expr *r)
{
  if (!type_check (r, 0, BT_INTEGER))
    return false;

  if (!scalar_check (r, 0))
    return false;

  return true;
}


bool
gfc_check_selected_real_kind (gfc_expr *p, gfc_expr *r, gfc_expr *radix)
{
  if (p == NULL && r == NULL
      && !gfc_notify_std (GFC_STD_F2008, "SELECTED_REAL_KIND with"
			  " neither %<P%> nor %<R%> argument at %L",
			  gfc_current_intrinsic_where))
    return false;

  if (p)
    {
      if (!type_check (p, 0, BT_INTEGER))
	return false;

      if (!scalar_check (p, 0))
	return false;
    }

  if (r)
    {
      if (!type_check (r, 1, BT_INTEGER))
	return false;

      if (!scalar_check (r, 1))
	return false;
    }

  if (radix)
    {
      if (!type_check (radix, 1, BT_INTEGER))
	return false;

      if (!scalar_check (radix, 1))
	return false;

      if (!gfc_notify_std (GFC_STD_F2008, "%qs intrinsic with "
			   "RADIX argument at %L", gfc_current_intrinsic,
			   &radix->where))
	return false;
    }

  return true;
}


bool
gfc_check_set_exponent (gfc_expr *x, gfc_expr *i)
{
  if (!type_check (x, 0, BT_REAL))
    return false;

  if (!type_check (i, 1, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_shape (gfc_expr *source, gfc_expr *kind)
{
  gfc_array_ref *ar;

  if (source->rank == 0 || source->expr_type != EXPR_VARIABLE)
    return true;

  ar = gfc_find_array_ref (source);

  if (ar->as && ar->as->type == AS_ASSUMED_SIZE && ar->type == AR_FULL)
    {
      gfc_error ("%<source%> argument of %<shape%> intrinsic at %L must not be "
		 "an assumed size array", &source->where);
      return false;
    }

  if (!kind_check (kind, 1, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

  return true;
}


bool
gfc_check_shift (gfc_expr *i, gfc_expr *shift)
{
  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!type_check (shift, 0, BT_INTEGER))
    return false;

  if (!nonnegative_check ("SHIFT", shift))
    return false;

  if (!less_than_bitsize1 ("I", i, "SHIFT", shift, true))
    return false;

  return true;
}


bool
gfc_check_sign (gfc_expr *a, gfc_expr *b)
{
  if (!int_or_real_check (a, 0))
    return false;

  if (!same_type_check (a, 0, b, 1))
    return false;

  return true;
}


bool
gfc_check_size (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  if (!array_check (array, 0))
    return false;

  if (!dim_check (dim, 1, true))
    return false;

  if (!dim_rank_check (dim, array, 0))
    return false;

  if (!kind_check (kind, 2, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;


  return true;
}


bool
gfc_check_sizeof (gfc_expr *arg)
{
  if (arg->ts.type == BT_PROCEDURE)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be a procedure",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &arg->where);
      return false;
    }

  /* TYPE(*) is acceptable if and only if it uses an array descriptor.  */
  if (arg->ts.type == BT_ASSUMED
      && (arg->symtree->n.sym->as == NULL
	  || (arg->symtree->n.sym->as->type != AS_ASSUMED_SHAPE
	      && arg->symtree->n.sym->as->type != AS_DEFERRED
	      && arg->symtree->n.sym->as->type != AS_ASSUMED_RANK)))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be TYPE(*)",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &arg->where);
      return false;
    }

  if (arg->rank && arg->expr_type == EXPR_VARIABLE
      && arg->symtree->n.sym->as != NULL
      && arg->symtree->n.sym->as->type == AS_ASSUMED_SIZE && arg->ref
      && arg->ref->type == REF_ARRAY && arg->ref->u.ar.type == AR_FULL)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be an "
		 "assumed-size array", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &arg->where);
      return false;
    }

  return true;
}


/* Check whether an expression is interoperable.  When returning false,
   msg is set to a string telling why the expression is not interoperable,
   otherwise, it is set to NULL.  The msg string can be used in diagnostics.
   If c_loc is true, character with len > 1 are allowed (cf. Fortran
   2003corr5); additionally, assumed-shape/assumed-rank/deferred-shape
   arrays are permitted. And if c_f_ptr is true, deferred-shape arrays
   are permitted.  */

static bool
is_c_interoperable (gfc_expr *expr, const char **msg, bool c_loc, bool c_f_ptr)
{
  *msg = NULL;

  if (expr->ts.type == BT_CLASS)
    {
      *msg = "Expression is polymorphic";
      return false;
    }

  if (expr->ts.type == BT_DERIVED && !expr->ts.u.derived->attr.is_bind_c
      && !expr->ts.u.derived->ts.is_iso_c)
    {
      *msg = "Expression is a noninteroperable derived type";
      return false;
    }

  if (expr->ts.type == BT_PROCEDURE)
    {
      *msg = "Procedure unexpected as argument";
      return false;
    }

  if (gfc_notification_std (GFC_STD_GNU) && expr->ts.type == BT_LOGICAL)
    {
      int i;
      for (i = 0; gfc_logical_kinds[i].kind; i++)
        if (gfc_logical_kinds[i].kind == expr->ts.kind)
          return true;
      *msg = "Extension to use a non-C_Bool-kind LOGICAL";
      return false;
    }

  if (gfc_notification_std (GFC_STD_GNU) && expr->ts.type == BT_CHARACTER
      && expr->ts.kind != 1)
    {
      *msg = "Extension to use a non-C_CHAR-kind CHARACTER";
      return false;
    }

  if (expr->ts.type == BT_CHARACTER) {
    if (expr->ts.deferred)
      {
	/* TS 29113 allows deferred-length strings as dummy arguments,
	   but it is not an interoperable type.  */
	*msg = "Expression shall not be a deferred-length string";
	return false;
      }

    if (expr->ts.u.cl && expr->ts.u.cl->length
	&& !gfc_simplify_expr (expr->ts.u.cl->length, 0))
      gfc_internal_error ("is_c_interoperable(): gfc_simplify_expr failed");

    if (!c_loc && expr->ts.u.cl
	&& (!expr->ts.u.cl->length
	    || expr->ts.u.cl->length->expr_type != EXPR_CONSTANT
	    || mpz_cmp_si (expr->ts.u.cl->length->value.integer, 1) != 0))
      {
	*msg = "Type shall have a character length of 1";
	return false;
      }
    }

  /* Note: The following checks are about interoperatable variables, Fortran
     15.3.5/15.3.6.  In intrinsics like C_LOC or in procedure interface, more
     is allowed, e.g. assumed-shape arrays with TS 29113.  */

  if (gfc_is_coarray (expr))
    {
      *msg = "Coarrays are not interoperable";
      return false;
    }

  if (!c_loc && expr->rank > 0 && expr->expr_type != EXPR_ARRAY)
    {
      gfc_array_ref *ar = gfc_find_array_ref (expr);
      if (ar->type != AR_FULL)
	{
	  *msg = "Only whole-arrays are interoperable";
	  return false;
	}
      if (!c_f_ptr && ar->as->type != AS_EXPLICIT
	  && ar->as->type != AS_ASSUMED_SIZE)
	{
	  *msg = "Only explicit-size and assumed-size arrays are interoperable";
	  return false;
	}
    }

  return true;
}


bool
gfc_check_c_sizeof (gfc_expr *arg)
{
  const char *msg;

  if (!is_c_interoperable (arg, &msg, false, false))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be an "
		 "interoperable data entity: %s",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &arg->where, msg);
      return false;
    }

  if (arg->ts.type == BT_ASSUMED)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be "
		 "TYPE(*)",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &arg->where);
      return false;
    }

  if (arg->rank && arg->expr_type == EXPR_VARIABLE
      && arg->symtree->n.sym->as != NULL
      && arg->symtree->n.sym->as->type == AS_ASSUMED_SIZE && arg->ref
      && arg->ref->type == REF_ARRAY && arg->ref->u.ar.type == AR_FULL)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be an "
		 "assumed-size array", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &arg->where);
      return false;
    }

  return true;
}


bool
gfc_check_c_associated (gfc_expr *c_ptr_1, gfc_expr *c_ptr_2)
{
  if (c_ptr_1->ts.type != BT_DERIVED
      || c_ptr_1->ts.u.derived->from_intmod != INTMOD_ISO_C_BINDING
      || (c_ptr_1->ts.u.derived->intmod_sym_id != ISOCBINDING_PTR
	  && c_ptr_1->ts.u.derived->intmod_sym_id != ISOCBINDING_FUNPTR))
    {
      gfc_error ("Argument C_PTR_1 at %L to C_ASSOCIATED shall have the "
		 "type TYPE(C_PTR) or TYPE(C_FUNPTR)", &c_ptr_1->where);
      return false;
    }

  if (!scalar_check (c_ptr_1, 0))
    return false;

  if (c_ptr_2
      && (c_ptr_2->ts.type != BT_DERIVED
	  || c_ptr_2->ts.u.derived->from_intmod != INTMOD_ISO_C_BINDING
	  || (c_ptr_1->ts.u.derived->intmod_sym_id
	      != c_ptr_2->ts.u.derived->intmod_sym_id)))
    {
      gfc_error ("Argument C_PTR_2 at %L to C_ASSOCIATED shall have the "
		 "same type as C_PTR_1: %s instead of %s", &c_ptr_1->where,
		 gfc_typename (&c_ptr_1->ts),
		 gfc_typename (&c_ptr_2->ts));
      return false;
    }

  if (c_ptr_2 && !scalar_check (c_ptr_2, 1))
    return false;

  return true;
}


bool
gfc_check_c_f_pointer (gfc_expr *cptr, gfc_expr *fptr, gfc_expr *shape)
{
  symbol_attribute attr;
  const char *msg;

  if (cptr->ts.type != BT_DERIVED
      || cptr->ts.u.derived->from_intmod != INTMOD_ISO_C_BINDING
      || cptr->ts.u.derived->intmod_sym_id != ISOCBINDING_PTR)
    {
      gfc_error ("Argument CPTR at %L to C_F_POINTER shall have the "
		 "type TYPE(C_PTR)", &cptr->where);
      return false;
    }

  if (!scalar_check (cptr, 0))
    return false;

  attr = gfc_expr_attr (fptr);

  if (!attr.pointer)
    {
      gfc_error ("Argument FPTR at %L to C_F_POINTER must be a pointer",
		 &fptr->where);
      return false;
    }

  if (fptr->ts.type == BT_CLASS)
    {
      gfc_error ("FPTR argument at %L to C_F_POINTER shall not be polymorphic",
		 &fptr->where);
      return false;
    }

  if (gfc_is_coindexed (fptr))
    {
      gfc_error ("Argument FPTR at %L to C_F_POINTER shall not be "
		 "coindexed", &fptr->where);
      return false;
    }

  if (fptr->rank == 0 && shape)
    {
      gfc_error ("Unexpected SHAPE argument at %L to C_F_POINTER with scalar "
		 "FPTR", &fptr->where);
      return false;
    }
  else if (fptr->rank && !shape)
    {
      gfc_error ("Expected SHAPE argument to C_F_POINTER with array "
		 "FPTR at %L", &fptr->where);
      return false;
    }

  if (shape && !rank_check (shape, 2, 1))
    return false;

  if (shape && !type_check (shape, 2, BT_INTEGER))
    return false;

  if (shape)
    {
      mpz_t size;
      if (gfc_array_size (shape, &size))
	{
	  if (mpz_cmp_ui (size, fptr->rank) != 0)
	    {
	      mpz_clear (size);
	      gfc_error ("SHAPE argument at %L to C_F_POINTER must have the same "
			"size as the RANK of FPTR", &shape->where);
	      return false;
	    }
	  mpz_clear (size);
	}
    }

  if (fptr->ts.type == BT_CLASS)
    {
      gfc_error ("Polymorphic FPTR at %L to C_F_POINTER", &fptr->where);
      return false;
    }

  if (fptr->rank > 0 && !is_c_interoperable (fptr, &msg, false, true))
    return gfc_notify_std (GFC_STD_F2018, "Noninteroperable array FPTR "
			   "at %L to C_F_POINTER: %s", &fptr->where, msg);

  return true;
}


bool
gfc_check_c_f_procpointer (gfc_expr *cptr, gfc_expr *fptr)
{
  symbol_attribute attr;

  if (cptr->ts.type != BT_DERIVED
      || cptr->ts.u.derived->from_intmod != INTMOD_ISO_C_BINDING
      || cptr->ts.u.derived->intmod_sym_id != ISOCBINDING_FUNPTR)
    {
      gfc_error ("Argument CPTR at %L to C_F_PROCPOINTER shall have the "
		 "type TYPE(C_FUNPTR)", &cptr->where);
      return false;
    }

  if (!scalar_check (cptr, 0))
    return false;

  attr = gfc_expr_attr (fptr);

  if (!attr.proc_pointer)
    {
      gfc_error ("Argument FPTR at %L to C_F_PROCPOINTER shall be a procedure "
		 "pointer", &fptr->where);
      return false;
    }

  if (gfc_is_coindexed (fptr))
    {
      gfc_error ("Argument FPTR at %L to C_F_PROCPOINTER shall not be "
		 "coindexed", &fptr->where);
      return false;
    }

  if (!attr.is_bind_c)
    return gfc_notify_std (GFC_STD_F2018, "Noninteroperable procedure "
			   "pointer at %L to C_F_PROCPOINTER", &fptr->where);

  return true;
}


bool
gfc_check_c_funloc (gfc_expr *x)
{
  symbol_attribute attr;

  if (gfc_is_coindexed (x))
    {
      gfc_error ("Argument X at %L to C_FUNLOC shall not be "
		 "coindexed", &x->where);
      return false;
    }

  attr = gfc_expr_attr (x);

  if (attr.function && !attr.proc_pointer && x->expr_type == EXPR_VARIABLE
      && x->symtree->n.sym == x->symtree->n.sym->result)
    for (gfc_namespace *ns = gfc_current_ns; ns; ns = ns->parent)
      if (x->symtree->n.sym == ns->proc_name)
	{
	  gfc_error ("Function result %qs at %L is invalid as X argument "
		     "to C_FUNLOC", x->symtree->n.sym->name, &x->where);
	  return false;
	}

  if (attr.flavor != FL_PROCEDURE)
    {
      gfc_error ("Argument X at %L to C_FUNLOC shall be a procedure "
		 "or a procedure pointer", &x->where);
      return false;
    }

  if (!attr.is_bind_c)
    return gfc_notify_std (GFC_STD_F2018, "Noninteroperable procedure "
			   "at %L to C_FUNLOC", &x->where);
  return true;
}


bool
gfc_check_c_loc (gfc_expr *x)
{
  symbol_attribute attr;
  const char *msg;

  if (gfc_is_coindexed (x))
    {
      gfc_error ("Argument X at %L to C_LOC shall not be coindexed", &x->where);
      return false;
    }

  if (x->ts.type == BT_CLASS)
    {
      gfc_error ("X argument at %L to C_LOC shall not be polymorphic",
		 &x->where);
      return false;
    }

  attr = gfc_expr_attr (x);

  if (!attr.pointer
      && (x->expr_type != EXPR_VARIABLE || !attr.target
	  || attr.flavor == FL_PARAMETER))
    {
      gfc_error ("Argument X at %L to C_LOC shall have either "
		 "the POINTER or the TARGET attribute", &x->where);
      return false;
    }

  if (x->ts.type == BT_CHARACTER
      && gfc_var_strlen (x) == 0)
    {
      gfc_error ("Argument X at %L to C_LOC shall be not be a zero-sized "
		 "string", &x->where);
      return false;
    }

  if (!is_c_interoperable (x, &msg, true, false))
    {
      if (x->ts.type == BT_CLASS)
	{
	  gfc_error ("Argument at %L to C_LOC shall not be polymorphic",
		     &x->where);
	  return false;
	}

      if (x->rank
	  && !gfc_notify_std (GFC_STD_F2018,
			      "Noninteroperable array at %L as"
			      " argument to C_LOC: %s", &x->where, msg))
	  return false;
    }
  else if (x->rank > 0 && gfc_notification_std (GFC_STD_F2008))
    {
      gfc_array_ref *ar = gfc_find_array_ref (x);

      if (ar->as->type != AS_EXPLICIT && ar->as->type != AS_ASSUMED_SIZE
	  && !attr.allocatable
	  && !gfc_notify_std (GFC_STD_F2008,
			      "Array of interoperable type at %L "
			      "to C_LOC which is nonallocatable and neither "
			      "assumed size nor explicit size", &x->where))
	return false;
      else if (ar->type != AR_FULL
	       && !gfc_notify_std (GFC_STD_F2008, "Array section at %L "
				   "to C_LOC", &x->where))
	return false;
    }

  return true;
}


bool
gfc_check_sleep_sub (gfc_expr *seconds)
{
  if (!type_check (seconds, 0, BT_INTEGER))
    return false;

  if (!scalar_check (seconds, 0))
    return false;

  return true;
}

bool
gfc_check_sngl (gfc_expr *a)
{
  if (!type_check (a, 0, BT_REAL))
    return false;

  if ((a->ts.kind != gfc_default_double_kind)
      && !gfc_notify_std (GFC_STD_GNU, "non double precision "
			  "REAL argument to %s intrinsic at %L",
			  gfc_current_intrinsic, &a->where))
    return false;

  return true;
}

bool
gfc_check_spread (gfc_expr *source, gfc_expr *dim, gfc_expr *ncopies)
{
  if (source->rank >= GFC_MAX_DIMENSIONS)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be less "
		 "than rank %d", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &source->where, GFC_MAX_DIMENSIONS);

      return false;
    }

  if (dim == NULL)
    return false;

  if (!dim_check (dim, 1, false))
    return false;

  /* dim_rank_check() does not apply here.  */
  if (dim
      && dim->expr_type == EXPR_CONSTANT
      && (mpz_cmp_ui (dim->value.integer, 1) < 0
	  || mpz_cmp_ui (dim->value.integer, source->rank + 1) > 0))
    {
      gfc_error ("%qs argument of %qs intrinsic at %L is not a valid "
		 "dimension index", gfc_current_intrinsic_arg[1]->name,
		 gfc_current_intrinsic, &dim->where);
      return false;
    }

  if (!type_check (ncopies, 2, BT_INTEGER))
    return false;

  if (!scalar_check (ncopies, 2))
    return false;

  return true;
}


/* Functions for checking FGETC, FPUTC, FGET and FPUT (subroutines and
   functions).  */

bool
gfc_check_fgetputc_sub (gfc_expr *unit, gfc_expr *c, gfc_expr *status)
{
  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  if (!type_check (c, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (c, 1, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 2, BT_INTEGER)
      || !kind_value_check (status, 2, gfc_default_integer_kind)
      || !scalar_check (status, 2))
    return false;

  return true;
}


bool
gfc_check_fgetputc (gfc_expr *unit, gfc_expr *c)
{
  return gfc_check_fgetputc_sub (unit, c, NULL);
}


bool
gfc_check_fgetput_sub (gfc_expr *c, gfc_expr *status)
{
  if (!type_check (c, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (c, 0, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 1, BT_INTEGER)
      || !kind_value_check (status, 1, gfc_default_integer_kind)
      || !scalar_check (status, 1))
    return false;

  return true;
}


bool
gfc_check_fgetput (gfc_expr *c)
{
  return gfc_check_fgetput_sub (c, NULL);
}


bool
gfc_check_fseek_sub (gfc_expr *unit, gfc_expr *offset, gfc_expr *whence, gfc_expr *status)
{
  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  if (!type_check (offset, 1, BT_INTEGER))
    return false;

  if (!scalar_check (offset, 1))
    return false;

  if (!type_check (whence, 2, BT_INTEGER))
    return false;

  if (!scalar_check (whence, 2))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 3, BT_INTEGER))
    return false;

  if (!kind_value_check (status, 3, 4))
    return false;

  if (!scalar_check (status, 3))
    return false;

  return true;
}



bool
gfc_check_fstat (gfc_expr *unit, gfc_expr *array)
{
  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  if (!type_check (array, 1, BT_INTEGER)
      || !kind_value_check (unit, 0, gfc_default_integer_kind))
    return false;

  if (!array_check (array, 1))
    return false;

  return true;
}


bool
gfc_check_fstat_sub (gfc_expr *unit, gfc_expr *array, gfc_expr *status)
{
  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  if (!type_check (array, 1, BT_INTEGER)
      || !kind_value_check (array, 1, gfc_default_integer_kind))
    return false;

  if (!array_check (array, 1))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 2, BT_INTEGER)
      || !kind_value_check (status, 2, gfc_default_integer_kind))
    return false;

  if (!scalar_check (status, 2))
    return false;

  return true;
}


bool
gfc_check_ftell (gfc_expr *unit)
{
  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  return true;
}


bool
gfc_check_ftell_sub (gfc_expr *unit, gfc_expr *offset)
{
  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  if (!type_check (offset, 1, BT_INTEGER))
    return false;

  if (!scalar_check (offset, 1))
    return false;

  return true;
}


bool
gfc_check_stat (gfc_expr *name, gfc_expr *array)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  if (!type_check (array, 1, BT_INTEGER)
      || !kind_value_check (array, 1, gfc_default_integer_kind))
    return false;

  if (!array_check (array, 1))
    return false;

  return true;
}


bool
gfc_check_stat_sub (gfc_expr *name, gfc_expr *array, gfc_expr *status)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  if (!type_check (array, 1, BT_INTEGER)
      || !kind_value_check (array, 1, gfc_default_integer_kind))
    return false;

  if (!array_check (array, 1))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 2, BT_INTEGER)
      || !kind_value_check (array, 1, gfc_default_integer_kind))
    return false;

  if (!scalar_check (status, 2))
    return false;

  return true;
}


bool
gfc_check_image_index (gfc_expr *coarray, gfc_expr *sub)
{
  mpz_t nelems;

  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      return false;
    }

  if (!coarray_check (coarray, 0))
    return false;

  if (sub->rank != 1)
    {
      gfc_error ("%s argument to IMAGE_INDEX must be a rank one array at %L",
                gfc_current_intrinsic_arg[1]->name, &sub->where);
      return false;
    }

  if (gfc_array_size (sub, &nelems))
    {
      int corank = gfc_get_corank (coarray);

      if (mpz_cmp_ui (nelems, corank) != 0)
	{
	  gfc_error ("The number of array elements of the SUB argument to "
		     "IMAGE_INDEX at %L shall be %d (corank) not %d",
		     &sub->where, corank, (int) mpz_get_si (nelems));
	  mpz_clear (nelems);
	  return false;
	}
      mpz_clear (nelems);
    }

  return true;
}


bool
gfc_check_num_images (gfc_expr *distance, gfc_expr *failed)
{
  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      return false;
    }

  if (distance)
    {
      if (!type_check (distance, 0, BT_INTEGER))
	return false;

      if (!nonnegative_check ("DISTANCE", distance))
	return false;

      if (!scalar_check (distance, 0))
	return false;

      if (!gfc_notify_std (GFC_STD_F2018, "DISTANCE= argument to "
			   "NUM_IMAGES at %L", &distance->where))
	return false;
    }

   if (failed)
    {
      if (!type_check (failed, 1, BT_LOGICAL))
	return false;

      if (!scalar_check (failed, 1))
	return false;

      if (!gfc_notify_std (GFC_STD_F2018, "FAILED= argument to "
			   "NUM_IMAGES at %L", &failed->where))
	return false;
    }

  return true;
}


bool
gfc_check_team_number (gfc_expr *team)
{
  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      return false;
    }

  if (team)
    {
      if (team->ts.type != BT_DERIVED
	  || team->ts.u.derived->from_intmod != INTMOD_ISO_FORTRAN_ENV
	  || team->ts.u.derived->intmod_sym_id != ISOFORTRAN_TEAM_TYPE)
	 {
	   gfc_error ("TEAM argument at %L to the intrinsic TEAM_NUMBER "
	   	      "shall be of type TEAM_TYPE", &team->where);
	   return false;
	 }
    }
  else
    return true;

  return true;
}


bool
gfc_check_this_image (gfc_expr *coarray, gfc_expr *dim, gfc_expr *distance)
{
  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      return false;
    }

  if (coarray == NULL && dim == NULL && distance == NULL)
    return true;

  if (dim != NULL && coarray == NULL)
    {
      gfc_error ("DIM argument without COARRAY argument not allowed for "
		 "THIS_IMAGE intrinsic at %L", &dim->where);
      return false;
    }

  if (distance && (coarray || dim))
    {
      gfc_error ("The DISTANCE argument may not be specified together with the "
		 "COARRAY or DIM argument in intrinsic at %L",
		 &distance->where);
      return false;
    }

  /* Assume that we have "this_image (distance)".  */
  if (coarray && !gfc_is_coarray (coarray) && coarray->ts.type == BT_INTEGER)
    {
      if (dim)
	{
	  gfc_error ("Unexpected DIM argument with noncoarray argument at %L",
		     &coarray->where);
	  return false;
	}
      distance = coarray;
    }

  if (distance)
    {
      if (!type_check (distance, 2, BT_INTEGER))
	return false;

      if (!nonnegative_check ("DISTANCE", distance))
	return false;

      if (!scalar_check (distance, 2))
	return false;

      if (!gfc_notify_std (GFC_STD_F2018, "DISTANCE= argument to "
			   "THIS_IMAGE at %L", &distance->where))
	return false;

      return true;
    }

  if (!coarray_check (coarray, 0))
    return false;

  if (dim != NULL)
    {
      if (!dim_check (dim, 1, false))
       return false;

      if (!dim_corank_check (dim, coarray))
       return false;
    }

  return true;
}

/* Calculate the sizes for transfer, used by gfc_check_transfer and also
   by gfc_simplify_transfer.  Return false if we cannot do so.  */

bool
gfc_calculate_transfer_sizes (gfc_expr *source, gfc_expr *mold, gfc_expr *size,
			      size_t *source_size, size_t *result_size,
			      size_t *result_length_p)
{
  size_t result_elt_size;

  if (source->expr_type == EXPR_FUNCTION)
    return false;

  if (size && size->expr_type != EXPR_CONSTANT)
    return false;

  /* Calculate the size of the source.  */
  if (!gfc_target_expr_size (source, source_size))
    return false;

  /* Determine the size of the element.  */
  if (!gfc_element_size (mold, &result_elt_size))
    return false;

  /* If the storage size of SOURCE is greater than zero and MOLD is an array,
   * a scalar with the type and type parameters of MOLD shall not have a
   * storage size equal to zero.
   * If MOLD is a scalar and SIZE is absent, the result is a scalar.
   * If MOLD is an array and SIZE is absent, the result is an array and of
   * rank one. Its size is as small as possible such that its physical
   * representation is not shorter than that of SOURCE.
   * If SIZE is present, the result is an array of rank one and size SIZE.
   */
  if (result_elt_size == 0 && *source_size > 0 && !size
      && mold->expr_type == EXPR_ARRAY)
    {
      gfc_error ("%<MOLD%> argument of %<TRANSFER%> intrinsic at %L is an "
		 "array and shall not have storage size 0 when %<SOURCE%> "
		 "argument has size greater than 0", &mold->where);
      return false;
    }

  if (result_elt_size == 0 && *source_size == 0 && !size)
    {
      *result_size = 0;
      if (result_length_p)
	*result_length_p = 0;
      return true;
    }

  if ((result_elt_size > 0 && (mold->expr_type == EXPR_ARRAY || mold->rank))
      || size)
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

  return true;
}


bool
gfc_check_transfer (gfc_expr *source, gfc_expr *mold, gfc_expr *size)
{
  size_t source_size;
  size_t result_size;

  /* SOURCE shall be a scalar or array of any type.  */
  if (source->ts.type == BT_PROCEDURE
      && source->symtree->n.sym->attr.subroutine == 1)
    {
      gfc_error ("%<SOURCE%> argument of %<TRANSFER%> intrinsic at %L "
                 "must not be a %s", &source->where,
		 gfc_basic_typename (source->ts.type));
      return false;
    }

  if (source->ts.type == BT_BOZ && illegal_boz_arg (source))
    return false;

  if (mold->ts.type == BT_BOZ && illegal_boz_arg (mold))
    return false;

  /* MOLD shall be a scalar or array of any type.  */
  if (mold->ts.type == BT_PROCEDURE
      && mold->symtree->n.sym->attr.subroutine == 1)
    {
      gfc_error ("%<MOLD%> argument of %<TRANSFER%> intrinsic at %L "
                 "must not be a %s", &mold->where,
		 gfc_basic_typename (mold->ts.type));
      return false;
    }

  if (mold->ts.type == BT_HOLLERITH)
    {
      gfc_error ("%<MOLD%> argument of %<TRANSFER%> intrinsic at %L must not be"
                 " %s", &mold->where, gfc_basic_typename (BT_HOLLERITH));
      return false;
    }

  /* SIZE (optional) shall be an integer scalar.  The corresponding actual
     argument shall not be an optional dummy argument.  */
  if (size != NULL)
    {
      if (!type_check (size, 2, BT_INTEGER))
	{
	  if (size->ts.type == BT_BOZ)
	    reset_boz (size);
	  return false;
	}

      if (!scalar_check (size, 2))
	return false;

      if (!nonoptional_check (size, 2))
	return false;
    }

  if (!warn_surprising)
    return true;

  /* If we can't calculate the sizes, we cannot check any more.
     Return true for that case.  */

  if (!gfc_calculate_transfer_sizes (source, mold, size, &source_size,
				     &result_size, NULL))
    return true;

  if (source_size < result_size)
    gfc_warning (OPT_Wsurprising,
		 "Intrinsic TRANSFER at %L has partly undefined result: "
		 "source size %ld < result size %ld", &source->where,
		 (long) source_size, (long) result_size);

  return true;
}


bool
gfc_check_transpose (gfc_expr *matrix)
{
  if (!rank_check (matrix, 0, 2))
    return false;

  return true;
}


bool
gfc_check_ubound (gfc_expr *array, gfc_expr *dim, gfc_expr *kind)
{
  if (!array_check (array, 0))
    return false;

  if (!dim_check (dim, 1, false))
    return false;

  if (!dim_rank_check (dim, array, 0))
    return false;

  if (!kind_check (kind, 2, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

  return true;
}


bool
gfc_check_ucobound (gfc_expr *coarray, gfc_expr *dim, gfc_expr *kind)
{
  if (flag_coarray == GFC_FCOARRAY_NONE)
    {
      gfc_fatal_error ("Coarrays disabled at %C, use %<-fcoarray=%> to enable");
      return false;
    }

  if (!coarray_check (coarray, 0))
    return false;

  if (dim != NULL)
    {
      if (!dim_check (dim, 1, false))
        return false;

      if (!dim_corank_check (dim, coarray))
        return false;
    }

  if (!kind_check (kind, 2, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_unpack (gfc_expr *vector, gfc_expr *mask, gfc_expr *field)
{
  mpz_t vector_size;

  if (!rank_check (vector, 0, 1))
    return false;

  if (!array_check (mask, 1))
    return false;

  if (!type_check (mask, 1, BT_LOGICAL))
    return false;

  if (!same_type_check (vector, 0, field, 2))
    return false;

  if (mask->expr_type == EXPR_ARRAY
      && gfc_array_size (vector, &vector_size))
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
	  gfc_error ("%qs argument of %qs intrinsic at %L must "
		     "provide at least as many elements as there "
		     "are .TRUE. values in %qs (%ld/%d)",
		     gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		     &vector->where, gfc_current_intrinsic_arg[1]->name,
		     mpz_get_si (vector_size), mask_true_count);
	  return false;
	}

      mpz_clear (vector_size);
    }

  if (mask->rank != field->rank && field->rank != 0)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must have "
		 "the same rank as %qs or be a scalar",
		 gfc_current_intrinsic_arg[2]->name, gfc_current_intrinsic,
		 &field->where, gfc_current_intrinsic_arg[1]->name);
      return false;
    }

  if (mask->rank == field->rank)
    {
      int i;
      for (i = 0; i < field->rank; i++)
	if (! identical_dimen_shape (mask, i, field, i))
	{
	  gfc_error ("%qs and %qs arguments of %qs intrinsic at %L "
		     "must have identical shape.",
		     gfc_current_intrinsic_arg[2]->name,
		     gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		     &field->where);
	}
    }

  return true;
}


bool
gfc_check_verify (gfc_expr *x, gfc_expr *y, gfc_expr *z, gfc_expr *kind)
{
  if (!type_check (x, 0, BT_CHARACTER))
    return false;

  if (!same_type_check (x, 0, y, 1))
    return false;

  if (z != NULL && !type_check (z, 2, BT_LOGICAL))
    return false;

  if (!kind_check (kind, 3, BT_INTEGER))
    return false;
  if (kind && !gfc_notify_std (GFC_STD_F2003, "%qs intrinsic "
			       "with KIND argument at %L",
			       gfc_current_intrinsic, &kind->where))
    return false;

  return true;
}


bool
gfc_check_trim (gfc_expr *x)
{
  if (!type_check (x, 0, BT_CHARACTER))
    return false;

  if (!scalar_check (x, 0))
    return false;

   return true;
}


bool
gfc_check_ttynam (gfc_expr *unit)
{
  if (!scalar_check (unit, 0))
    return false;

  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  return true;
}


/************* Check functions for intrinsic subroutines *************/

bool
gfc_check_cpu_time (gfc_expr *time)
{
  if (!scalar_check (time, 0))
    return false;

  if (!type_check (time, 0, BT_REAL))
    return false;

  if (!variable_check (time, 0, false))
    return false;

  return true;
}


bool
gfc_check_date_and_time (gfc_expr *date, gfc_expr *time,
			 gfc_expr *zone, gfc_expr *values)
{
  if (date != NULL)
    {
      if (!type_check (date, 0, BT_CHARACTER))
	return false;
      if (!kind_value_check (date, 0, gfc_default_character_kind))
	return false;
      if (!scalar_check (date, 0))
	return false;
      if (!variable_check (date, 0, false))
	return false;
    }

  if (time != NULL)
    {
      if (!type_check (time, 1, BT_CHARACTER))
	return false;
      if (!kind_value_check (time, 1, gfc_default_character_kind))
	return false;
      if (!scalar_check (time, 1))
	return false;
      if (!variable_check (time, 1, false))
	return false;
    }

  if (zone != NULL)
    {
      if (!type_check (zone, 2, BT_CHARACTER))
	return false;
      if (!kind_value_check (zone, 2, gfc_default_character_kind))
	return false;
      if (!scalar_check (zone, 2))
	return false;
      if (!variable_check (zone, 2, false))
	return false;
    }

  if (values != NULL)
    {
      if (!type_check (values, 3, BT_INTEGER))
	return false;
      if (!array_check (values, 3))
	return false;
      if (!rank_check (values, 3, 1))
	return false;
      if (!variable_check (values, 3, false))
	return false;
    }

  return true;
}


bool
gfc_check_mvbits (gfc_expr *from, gfc_expr *frompos, gfc_expr *len,
		  gfc_expr *to, gfc_expr *topos)
{
  if (!type_check (from, 0, BT_INTEGER))
    return false;

  if (!type_check (frompos, 1, BT_INTEGER))
    return false;

  if (!type_check (len, 2, BT_INTEGER))
    return false;

  if (!same_type_check (from, 0, to, 3))
    return false;

  if (!variable_check (to, 3, false))
    return false;

  if (!type_check (topos, 4, BT_INTEGER))
    return false;

  if (!nonnegative_check ("frompos", frompos))
    return false;

  if (!nonnegative_check ("topos", topos))
    return false;

  if (!nonnegative_check ("len", len))
    return false;

  if (!less_than_bitsize2 ("from", from, "frompos", frompos, "len", len))
    return false;

  if (!less_than_bitsize2 ("to", to, "topos", topos, "len", len))
    return false;

  return true;
}


/* Check the arguments for RANDOM_INIT.  */

bool
gfc_check_random_init (gfc_expr *repeatable, gfc_expr *image_distinct)
{
  if (!type_check (repeatable, 0, BT_LOGICAL))
    return false;

  if (!scalar_check (repeatable, 0))
    return false;

  if (!type_check (image_distinct, 1, BT_LOGICAL))
    return false;

  if (!scalar_check (image_distinct, 1))
    return false;

  return true;
}


bool
gfc_check_random_number (gfc_expr *harvest)
{
  if (!type_check (harvest, 0, BT_REAL))
    return false;

  if (!variable_check (harvest, 0, false))
    return false;

  return true;
}


bool
gfc_check_random_seed (gfc_expr *size, gfc_expr *put, gfc_expr *get)
{
  unsigned int nargs = 0, seed_size;
  locus *where = NULL;
  mpz_t put_size, get_size;

  /* Keep the number of bytes in sync with master_state in
     libgfortran/intrinsics/random.c.  */
  seed_size = 32 / gfc_default_integer_kind;

  if (size != NULL)
    {
      if (size->expr_type != EXPR_VARIABLE
	  || !size->symtree->n.sym->attr.optional)
	nargs++;

      if (!scalar_check (size, 0))
	return false;

      if (!type_check (size, 0, BT_INTEGER))
	return false;

      if (!variable_check (size, 0, false))
	return false;

      if (!kind_value_check (size, 0, gfc_default_integer_kind))
	return false;
    }

  if (put != NULL)
    {
      if (put->expr_type != EXPR_VARIABLE
	  || !put->symtree->n.sym->attr.optional)
	{
	  nargs++;
	  where = &put->where;
	}

      if (!array_check (put, 1))
	return false;

      if (!rank_check (put, 1, 1))
	return false;

      if (!type_check (put, 1, BT_INTEGER))
	return false;

      if (!kind_value_check (put, 1, gfc_default_integer_kind))
	return false;

      if (gfc_array_size (put, &put_size)
	  && mpz_get_ui (put_size) < seed_size)
	gfc_error ("Size of %qs argument of %qs intrinsic at %L "
		   "too small (%i/%i)",
		   gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		   where, (int) mpz_get_ui (put_size), seed_size);
    }

  if (get != NULL)
    {
      if (get->expr_type != EXPR_VARIABLE
	  || !get->symtree->n.sym->attr.optional)
	{
	  nargs++;
	  where = &get->where;
	}

      if (!array_check (get, 2))
	return false;

      if (!rank_check (get, 2, 1))
	return false;

      if (!type_check (get, 2, BT_INTEGER))
	return false;

      if (!variable_check (get, 2, false))
	return false;

      if (!kind_value_check (get, 2, gfc_default_integer_kind))
	return false;

       if (gfc_array_size (get, &get_size)
	   && mpz_get_ui (get_size) < seed_size)
	gfc_error ("Size of %qs argument of %qs intrinsic at %L "
		   "too small (%i/%i)",
		   gfc_current_intrinsic_arg[2]->name, gfc_current_intrinsic,
		   where, (int) mpz_get_ui (get_size), seed_size);
    }

  /* RANDOM_SEED may not have more than one non-optional argument.  */
  if (nargs > 1)
    gfc_error ("Too many arguments to %s at %L", gfc_current_intrinsic, where);

  return true;
}

bool
gfc_check_fe_runtime_error (gfc_actual_arglist *a)
{
  gfc_expr *e;
  size_t len, i;
  int num_percent, nargs;

  e = a->expr;
  if (e->expr_type != EXPR_CONSTANT)
    return true;

  len = e->value.character.length;
  if (e->value.character.string[len-1] != '\0')
    gfc_internal_error ("fe_runtime_error string must be null terminated");

  num_percent = 0;
  for (i=0; i<len-1; i++)
    if (e->value.character.string[i] == '%')
      num_percent ++;

  nargs = 0;
  for (; a; a = a->next)
    nargs ++;

  if (nargs -1 != num_percent)
    gfc_internal_error ("fe_runtime_error: Wrong number of arguments (%d instead of %d)",
			nargs, num_percent++);

  return true;
}

bool
gfc_check_second_sub (gfc_expr *time)
{
  if (!scalar_check (time, 0))
    return false;

  if (!type_check (time, 0, BT_REAL))
    return false;

  if (!kind_value_check (time, 0, 4))
    return false;

  return true;
}


/* COUNT and COUNT_MAX of SYSTEM_CLOCK are scalar, default-kind integer
   variables in Fortran 95.  In Fortran 2003 and later, they can be of any
   kind, and COUNT_RATE can be of type real.  Note, count, count_rate, and
   count_max are all optional arguments */

bool
gfc_check_system_clock (gfc_expr *count, gfc_expr *count_rate,
			gfc_expr *count_max)
{
  if (count != NULL)
    {
      if (!scalar_check (count, 0))
	return false;

      if (!type_check (count, 0, BT_INTEGER))
	return false;

      if (count->ts.kind != gfc_default_integer_kind
	  && !gfc_notify_std (GFC_STD_F2003, "COUNT argument to "
			      "SYSTEM_CLOCK at %L has non-default kind",
			      &count->where))
	return false;

      if (!variable_check (count, 0, false))
	return false;
    }

  if (count_rate != NULL)
    {
      if (!scalar_check (count_rate, 1))
	return false;

      if (!variable_check (count_rate, 1, false))
	return false;

      if (count_rate->ts.type == BT_REAL)
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "Real COUNT_RATE argument to "
			       "SYSTEM_CLOCK at %L", &count_rate->where))
	    return false;
	}
      else
	{
	  if (!type_check (count_rate, 1, BT_INTEGER))
	    return false;

	  if (count_rate->ts.kind != gfc_default_integer_kind
	      && !gfc_notify_std (GFC_STD_F2003, "COUNT_RATE argument to "
				  "SYSTEM_CLOCK at %L has non-default kind",
				  &count_rate->where))
	    return false;
	}

    }

  if (count_max != NULL)
    {
      if (!scalar_check (count_max, 2))
	return false;

      if (!type_check (count_max, 2, BT_INTEGER))
	return false;

      if (count_max->ts.kind != gfc_default_integer_kind
	  && !gfc_notify_std (GFC_STD_F2003, "COUNT_MAX argument to "
			      "SYSTEM_CLOCK at %L has non-default kind",
			      &count_max->where))
	return false;

      if (!variable_check (count_max, 2, false))
	return false;
    }

  return true;
}


bool
gfc_check_irand (gfc_expr *x)
{
  if (x == NULL)
    return true;

  if (!scalar_check (x, 0))
    return false;

  if (!type_check (x, 0, BT_INTEGER))
    return false;

  if (!kind_value_check (x, 0, 4))
    return false;

  return true;
}


bool
gfc_check_alarm_sub (gfc_expr *seconds, gfc_expr *handler, gfc_expr *status)
{
  if (!scalar_check (seconds, 0))
    return false;
  if (!type_check (seconds, 0, BT_INTEGER))
    return false;

  if (!int_or_proc_check (handler, 1))
    return false;
  if (handler->ts.type == BT_INTEGER && !scalar_check (handler, 1))
    return false;

  if (status == NULL)
    return true;

  if (!scalar_check (status, 2))
    return false;
  if (!type_check (status, 2, BT_INTEGER))
    return false;
  if (!kind_value_check (status, 2, gfc_default_integer_kind))
    return false;

  return true;
}


bool
gfc_check_rand (gfc_expr *x)
{
  if (x == NULL)
    return true;

  if (!scalar_check (x, 0))
    return false;

  if (!type_check (x, 0, BT_INTEGER))
    return false;

  if (!kind_value_check (x, 0, 4))
    return false;

  return true;
}


bool
gfc_check_srand (gfc_expr *x)
{
  if (!scalar_check (x, 0))
    return false;

  if (!type_check (x, 0, BT_INTEGER))
    return false;

  if (!kind_value_check (x, 0, 4))
    return false;

  return true;
}


bool
gfc_check_ctime_sub (gfc_expr *time, gfc_expr *result)
{
  if (!scalar_check (time, 0))
    return false;
  if (!type_check (time, 0, BT_INTEGER))
    return false;

  if (!type_check (result, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (result, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_dtime_etime (gfc_expr *x)
{
  if (!array_check (x, 0))
    return false;

  if (!rank_check (x, 0, 1))
    return false;

  if (!variable_check (x, 0, false))
    return false;

  if (!type_check (x, 0, BT_REAL))
    return false;

  if (!kind_value_check (x, 0, 4))
    return false;

  return true;
}


bool
gfc_check_dtime_etime_sub (gfc_expr *values, gfc_expr *time)
{
  if (!array_check (values, 0))
    return false;

  if (!rank_check (values, 0, 1))
    return false;

  if (!variable_check (values, 0, false))
    return false;

  if (!type_check (values, 0, BT_REAL))
    return false;

  if (!kind_value_check (values, 0, 4))
    return false;

  if (!scalar_check (time, 1))
    return false;

  if (!type_check (time, 1, BT_REAL))
    return false;

  if (!kind_value_check (time, 1, 4))
    return false;

  return true;
}


bool
gfc_check_fdate_sub (gfc_expr *date)
{
  if (!type_check (date, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (date, 0, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_gerror (gfc_expr *msg)
{
  if (!type_check (msg, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (msg, 0, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_getcwd_sub (gfc_expr *cwd, gfc_expr *status)
{
  if (!type_check (cwd, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (cwd, 0, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!scalar_check (status, 1))
    return false;

  if (!type_check (status, 1, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_getarg (gfc_expr *pos, gfc_expr *value)
{
  if (!type_check (pos, 0, BT_INTEGER))
    return false;

  if (pos->ts.kind > gfc_default_integer_kind)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be of a kind "
		 "not wider than the default kind (%d)",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &pos->where, gfc_default_integer_kind);
      return false;
    }

  if (!type_check (value, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (value, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_getlog (gfc_expr *msg)
{
  if (!type_check (msg, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (msg, 0, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_exit (gfc_expr *status)
{
  if (status == NULL)
    return true;

  if (!type_check (status, 0, BT_INTEGER))
    return false;

  if (!scalar_check (status, 0))
    return false;

  return true;
}


bool
gfc_check_flush (gfc_expr *unit)
{
  if (unit == NULL)
    return true;

  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  return true;
}


bool
gfc_check_free (gfc_expr *i)
{
  if (!type_check (i, 0, BT_INTEGER))
    return false;

  if (!scalar_check (i, 0))
    return false;

  return true;
}


bool
gfc_check_hostnm (gfc_expr *name)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_hostnm_sub (gfc_expr *name, gfc_expr *status)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!scalar_check (status, 1))
    return false;

  if (!type_check (status, 1, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_itime_idate (gfc_expr *values)
{
  if (!array_check (values, 0))
    return false;

  if (!rank_check (values, 0, 1))
    return false;

  if (!variable_check (values, 0, false))
    return false;

  if (!type_check (values, 0, BT_INTEGER))
    return false;

  if (!kind_value_check (values, 0, gfc_default_integer_kind))
    return false;

  return true;
}


bool
gfc_check_ltime_gmtime (gfc_expr *time, gfc_expr *values)
{
  if (!type_check (time, 0, BT_INTEGER))
    return false;

  if (!kind_value_check (time, 0, gfc_default_integer_kind))
    return false;

  if (!scalar_check (time, 0))
    return false;

  if (!array_check (values, 1))
    return false;

  if (!rank_check (values, 1, 1))
    return false;

  if (!variable_check (values, 1, false))
    return false;

  if (!type_check (values, 1, BT_INTEGER))
    return false;

  if (!kind_value_check (values, 1, gfc_default_integer_kind))
    return false;

  return true;
}


bool
gfc_check_ttynam_sub (gfc_expr *unit, gfc_expr *name)
{
  if (!scalar_check (unit, 0))
    return false;

  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!type_check (name, 1, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 1, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_is_contiguous (gfc_expr *array)
{
  if (array->expr_type == EXPR_NULL)
    {
      gfc_error ("Actual argument at %L of %qs intrinsic shall be an "
		 "associated pointer", &array->where, gfc_current_intrinsic);
      return false;
    }

  if (!array_check (array, 0))
    return false;

  return true;
}


bool
gfc_check_isatty (gfc_expr *unit)
{
  if (unit == NULL)
    return false;

  if (!type_check (unit, 0, BT_INTEGER))
    return false;

  if (!scalar_check (unit, 0))
    return false;

  return true;
}


bool
gfc_check_isnan (gfc_expr *x)
{
  if (!type_check (x, 0, BT_REAL))
    return false;

  return true;
}


bool
gfc_check_perror (gfc_expr *string)
{
  if (!type_check (string, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (string, 0, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_umask (gfc_expr *mask)
{
  if (!type_check (mask, 0, BT_INTEGER))
    return false;

  if (!scalar_check (mask, 0))
    return false;

  return true;
}


bool
gfc_check_umask_sub (gfc_expr *mask, gfc_expr *old)
{
  if (!type_check (mask, 0, BT_INTEGER))
    return false;

  if (!scalar_check (mask, 0))
    return false;

  if (old == NULL)
    return true;

  if (!scalar_check (old, 1))
    return false;

  if (!type_check (old, 1, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_unlink (gfc_expr *name)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  return true;
}


bool
gfc_check_unlink_sub (gfc_expr *name, gfc_expr *status)
{
  if (!type_check (name, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (name, 0, gfc_default_character_kind))
    return false;

  if (status == NULL)
    return true;

  if (!scalar_check (status, 1))
    return false;

  if (!type_check (status, 1, BT_INTEGER))
    return false;

  return true;
}


bool
gfc_check_signal (gfc_expr *number, gfc_expr *handler)
{
  if (!scalar_check (number, 0))
    return false;
  if (!type_check (number, 0, BT_INTEGER))
    return false;

  if (!int_or_proc_check (handler, 1))
    return false;
  if (handler->ts.type == BT_INTEGER && !scalar_check (handler, 1))
    return false;

  return true;
}


bool
gfc_check_signal_sub (gfc_expr *number, gfc_expr *handler, gfc_expr *status)
{
  if (!scalar_check (number, 0))
    return false;
  if (!type_check (number, 0, BT_INTEGER))
    return false;

  if (!int_or_proc_check (handler, 1))
    return false;
  if (handler->ts.type == BT_INTEGER && !scalar_check (handler, 1))
    return false;

  if (status == NULL)
    return true;

  if (!type_check (status, 2, BT_INTEGER))
    return false;
  if (!scalar_check (status, 2))
    return false;

  return true;
}


bool
gfc_check_system_sub (gfc_expr *cmd, gfc_expr *status)
{
  if (!type_check (cmd, 0, BT_CHARACTER))
    return false;
  if (!kind_value_check (cmd, 0, gfc_default_character_kind))
    return false;

  if (!scalar_check (status, 1))
    return false;

  if (!type_check (status, 1, BT_INTEGER))
    return false;

  if (!kind_value_check (status, 1, gfc_default_integer_kind))
    return false;

  return true;
}


/* This is used for the GNU intrinsics AND, OR and XOR.  */
bool
gfc_check_and (gfc_expr *i, gfc_expr *j)
{
  if (i->ts.type != BT_INTEGER
      && i->ts.type != BT_LOGICAL
      && i->ts.type != BT_BOZ)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER, "
                 "LOGICAL, or a BOZ literal constant",
		 gfc_current_intrinsic_arg[0]->name,
                 gfc_current_intrinsic, &i->where);
      return false;
    }

  if (j->ts.type != BT_INTEGER
      && j->ts.type != BT_LOGICAL
      && j->ts.type != BT_BOZ)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER, "
                 "LOGICAL, or a BOZ literal constant",
		 gfc_current_intrinsic_arg[1]->name,
                 gfc_current_intrinsic, &j->where);
      return false;
    }

  /* i and j cannot both be BOZ literal constants.  */
  if (!boz_args_check (i, j))
    return false;

  /* If i is BOZ and j is integer, convert i to type of j.  */
  if (i->ts.type == BT_BOZ)
    {
      if (j->ts.type != BT_INTEGER)
	{
	  gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER",
		     gfc_current_intrinsic_arg[1]->name,
		     gfc_current_intrinsic, &j->where);
	  reset_boz (i);
	  return false;
	}
      if (!gfc_boz2int (i, j->ts.kind))
	return false;
    }

  /* If j is BOZ and i is integer, convert j to type of i.  */
  if (j->ts.type == BT_BOZ)
    {
      if (i->ts.type != BT_INTEGER)
	{
	  gfc_error ("%qs argument of %qs intrinsic at %L must be INTEGER",
		     gfc_current_intrinsic_arg[0]->name,
		     gfc_current_intrinsic, &j->where);
	  reset_boz (j);
	  return false;
	}
      if (!gfc_boz2int (j, i->ts.kind))
	return false;
    }

  if (!same_type_check (i, 0, j, 1, false))
    return false;

  if (!scalar_check (i, 0))
    return false;

  if (!scalar_check (j, 1))
    return false;

  return true;
}


bool
gfc_check_storage_size (gfc_expr *a, gfc_expr *kind)
{

  if (a->expr_type == EXPR_NULL)
    {
      gfc_error ("Intrinsic function NULL at %L cannot be an actual "
		 "argument to STORAGE_SIZE, because it returns a "
		 "disassociated pointer", &a->where);
      return false;
    }

  if (a->ts.type == BT_ASSUMED)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be TYPE(*)",
		 gfc_current_intrinsic_arg[0]->name, gfc_current_intrinsic,
		 &a->where);
      return false;
    }

  if (a->ts.type == BT_PROCEDURE)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L shall not be a "
		 "procedure", gfc_current_intrinsic_arg[0]->name,
		 gfc_current_intrinsic, &a->where);
      return false;
    }

  if (a->ts.type == BT_BOZ && illegal_boz_arg (a))
    return false;

  if (kind == NULL)
    return true;

  if (!type_check (kind, 1, BT_INTEGER))
    return false;

  if (!scalar_check (kind, 1))
    return false;

  if (kind->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("%qs argument of %qs intrinsic at %L must be a constant",
		 gfc_current_intrinsic_arg[1]->name, gfc_current_intrinsic,
		 &kind->where);
      return false;
    }

  return true;
}
