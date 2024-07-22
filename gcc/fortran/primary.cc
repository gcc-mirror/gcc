/* Primary expression subroutines
   Copyright (C) 2000-2024 Free Software Foundation, Inc.
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
#include "arith.h"
#include "match.h"
#include "parse.h"
#include "constructor.h"

int matching_actual_arglist = 0;

/* Matches a kind-parameter expression, which is either a named
   symbolic constant or a nonnegative integer constant.  If
   successful, sets the kind value to the correct integer.
   The argument 'is_iso_c' signals whether the kind is an ISO_C_BINDING
   symbol like e.g. 'c_int'.  */

static match
match_kind_param (int *kind, int *is_iso_c)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;

  *is_iso_c = 0;

  m = gfc_match_small_literal_int (kind, NULL, false);
  if (m != MATCH_NO)
    return m;

  m = gfc_match_name (name, false);
  if (m != MATCH_YES)
    return m;

  if (gfc_find_symbol (name, NULL, 1, &sym))
    return MATCH_ERROR;

  if (sym == NULL)
    return MATCH_NO;

  *is_iso_c = sym->attr.is_iso_c;

  if (sym->attr.flavor != FL_PARAMETER)
    return MATCH_NO;

  if (sym->value == NULL)
    return MATCH_NO;

  if (gfc_extract_int (sym->value, kind))
    return MATCH_NO;

  gfc_set_sym_referenced (sym);

  if (*kind < 0)
    return MATCH_NO;

  return MATCH_YES;
}


/* Get a trailing kind-specification for non-character variables.
   Returns:
     * the integer kind value or
     * -1 if an error was generated,
     * -2 if no kind was found.
   The argument 'is_iso_c' signals whether the kind is an ISO_C_BINDING
   symbol like e.g. 'c_int'.  */

static int
get_kind (int *is_iso_c)
{
  int kind;
  match m;

  *is_iso_c = 0;

  if (gfc_match_char ('_', false) != MATCH_YES)
    return -2;

  m = match_kind_param (&kind, is_iso_c);
  if (m == MATCH_NO)
    gfc_error ("Missing kind-parameter at %C");

  return (m == MATCH_YES) ? kind : -1;
}


/* Given a character and a radix, see if the character is a valid
   digit in that radix.  */

bool
gfc_check_digit (char c, int radix)
{
  bool r;

  switch (radix)
    {
    case 2:
      r = ('0' <= c && c <= '1');
      break;

    case 8:
      r = ('0' <= c && c <= '7');
      break;

    case 10:
      r = ('0' <= c && c <= '9');
      break;

    case 16:
      r = ISXDIGIT (c);
      break;

    default:
      gfc_internal_error ("gfc_check_digit(): bad radix");
    }

  return r;
}


/* Match the digit string part of an integer if signflag is not set,
   the signed digit string part if signflag is set.  If the buffer
   is NULL, we just count characters for the resolution pass.  Returns
   the number of characters matched, -1 for no match.  */

static int
match_digits (int signflag, int radix, char *buffer)
{
  locus old_loc;
  int length;
  char c;

  length = 0;
  c = gfc_next_ascii_char ();

  if (signflag && (c == '+' || c == '-'))
    {
      if (buffer != NULL)
	*buffer++ = c;
      gfc_gobble_whitespace ();
      c = gfc_next_ascii_char ();
      length++;
    }

  if (!gfc_check_digit (c, radix))
    return -1;

  length++;
  if (buffer != NULL)
    *buffer++ = c;

  for (;;)
    {
      old_loc = gfc_current_locus;
      c = gfc_next_ascii_char ();

      if (!gfc_check_digit (c, radix))
	break;

      if (buffer != NULL)
	*buffer++ = c;
      length++;
    }

  gfc_current_locus = old_loc;

  return length;
}

/* Convert an integer string to an expression node.  */

static gfc_expr *
convert_integer (const char *buffer, int kind, int radix, locus *where)
{
  gfc_expr *e;
  const char *t;

  e = gfc_get_constant_expr (BT_INTEGER, kind, where);
  /* A leading plus is allowed, but not by mpz_set_str.  */
  if (buffer[0] == '+')
    t = buffer + 1;
  else
    t = buffer;
  mpz_set_str (e->value.integer, t, radix);

  return e;
}


/* Convert a real string to an expression node.  */

static gfc_expr *
convert_real (const char *buffer, int kind, locus *where)
{
  gfc_expr *e;

  e = gfc_get_constant_expr (BT_REAL, kind, where);
  mpfr_set_str (e->value.real, buffer, 10, GFC_RND_MODE);

  return e;
}


/* Convert a pair of real, constant expression nodes to a single
   complex expression node.  */

static gfc_expr *
convert_complex (gfc_expr *real, gfc_expr *imag, int kind)
{
  gfc_expr *e;

  e = gfc_get_constant_expr (BT_COMPLEX, kind, &real->where);
  mpc_set_fr_fr (e->value.complex, real->value.real, imag->value.real,
		 GFC_MPC_RND_MODE);

  return e;
}


/* Match an integer (digit string and optional kind).
   A sign will be accepted if signflag is set.  */

static match
match_integer_constant (gfc_expr **result, int signflag)
{
  int length, kind, is_iso_c;
  locus old_loc;
  char *buffer;
  gfc_expr *e;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  length = match_digits (signflag, 10, NULL);
  gfc_current_locus = old_loc;
  if (length == -1)
    return MATCH_NO;

  buffer = (char *) alloca (length + 1);
  memset (buffer, '\0', length + 1);

  gfc_gobble_whitespace ();

  match_digits (signflag, 10, buffer);

  kind = get_kind (&is_iso_c);
  if (kind == -2)
    kind = gfc_default_integer_kind;
  if (kind == -1)
    return MATCH_ERROR;

  if (kind == 4 && flag_integer4_kind == 8)
    kind = 8;

  if (gfc_validate_kind (BT_INTEGER, kind, true) < 0)
    {
      gfc_error ("Integer kind %d at %C not available", kind);
      return MATCH_ERROR;
    }

  e = convert_integer (buffer, kind, 10, &gfc_current_locus);
  e->ts.is_c_interop = is_iso_c;

  if (gfc_range_check (e) != ARITH_OK)
    {
      gfc_error ("Integer too big for its kind at %C. This check can be "
		 "disabled with the option %<-fno-range-check%>");

      gfc_free_expr (e);
      return MATCH_ERROR;
    }

  *result = e;
  return MATCH_YES;
}


/* Match a Hollerith constant.  */

static match
match_hollerith_constant (gfc_expr **result)
{
  locus old_loc;
  gfc_expr *e = NULL;
  int num, pad;
  int i;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  if (match_integer_constant (&e, 0) == MATCH_YES
      && gfc_match_char ('h') == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_LEGACY, "Hollerith constant at %C"))
	goto cleanup;

      if (gfc_extract_int (e, &num, 1))
	goto cleanup;
      if (num == 0)
	{
	  gfc_error ("Invalid Hollerith constant: %L must contain at least "
		     "one character", &old_loc);
	  goto cleanup;
	}
      if (e->ts.kind != gfc_default_integer_kind)
	{
	  gfc_error ("Invalid Hollerith constant: Integer kind at %L "
		     "should be default", &old_loc);
	  goto cleanup;
	}
      else
	{
	  gfc_free_expr (e);
	  e = gfc_get_constant_expr (BT_HOLLERITH, gfc_default_character_kind,
				     &gfc_current_locus);

	  /* Calculate padding needed to fit default integer memory.  */
	  pad = gfc_default_integer_kind - (num % gfc_default_integer_kind);

	  e->representation.string = XCNEWVEC (char, num + pad + 1);

	  for (i = 0; i < num; i++)
	    {
	      gfc_char_t c = gfc_next_char_literal (INSTRING_WARN);
	      if (! gfc_wide_fits_in_byte (c))
		{
		  gfc_error ("Invalid Hollerith constant at %L contains a "
			     "wide character", &old_loc);
		  goto cleanup;
		}

	      e->representation.string[i] = (unsigned char) c;
	    }

	  /* Now pad with blanks and end with a null char.  */
	  for (i = 0; i < pad; i++)
	    e->representation.string[num + i] = ' ';

	  e->representation.string[num + i] = '\0';
	  e->representation.length = num + pad;
	  e->ts.u.pad = pad;

	  *result = e;
	  return MATCH_YES;
	}
    }

  gfc_free_expr (e);
  gfc_current_locus = old_loc;
  return MATCH_NO;

cleanup:
  gfc_free_expr (e);
  return MATCH_ERROR;
}


/* Match a binary, octal or hexadecimal constant that can be found in
   a DATA statement.  The standard permits b'010...', o'73...', and
   z'a1...' where b, o, and z can be capital letters.  This function
   also accepts postfixed forms of the constants: '01...'b, '73...'o,
   and 'a1...'z.  An additional extension is the use of x for z.  */

static match
match_boz_constant (gfc_expr **result)
{
  int radix, length, x_hex;
  locus old_loc, start_loc;
  char *buffer, post, delim;
  gfc_expr *e;

  start_loc = old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  x_hex = 0;
  switch (post = gfc_next_ascii_char ())
    {
    case 'b':
      radix = 2;
      post = 0;
      break;
    case 'o':
      radix = 8;
      post = 0;
      break;
    case 'x':
      x_hex = 1;
      /* Fall through.  */
    case 'z':
      radix = 16;
      post = 0;
      break;
    case '\'':
      /* Fall through.  */
    case '\"':
      delim = post;
      post = 1;
      radix = 16;  /* Set to accept any valid digit string.  */
      break;
    default:
      goto backup;
    }

  /* No whitespace allowed here.  */

  if (post == 0)
    delim = gfc_next_ascii_char ();

  if (delim != '\'' && delim != '\"')
    goto backup;

  if (x_hex
      && gfc_invalid_boz (G_("Hexadecimal constant at %L uses "
			  "nonstandard X instead of Z"), &gfc_current_locus))
    return MATCH_ERROR;

  old_loc = gfc_current_locus;

  length = match_digits (0, radix, NULL);
  if (length == -1)
    {
      gfc_error ("Empty set of digits in BOZ constant at %C");
      return MATCH_ERROR;
    }

  if (gfc_next_ascii_char () != delim)
    {
      gfc_error ("Illegal character in BOZ constant at %C");
      return MATCH_ERROR;
    }

  if (post == 1)
    {
      switch (gfc_next_ascii_char ())
	{
	case 'b':
	  radix = 2;
	  break;
	case 'o':
	  radix = 8;
	  break;
	case 'x':
	  /* Fall through.  */
	case 'z':
	  radix = 16;
	  break;
	default:
	  goto backup;
	}

      if (gfc_invalid_boz (G_("BOZ constant at %C uses nonstandard postfix "
			   "syntax"), &gfc_current_locus))
	return MATCH_ERROR;
    }

  gfc_current_locus = old_loc;

  buffer = (char *) alloca (length + 1);
  memset (buffer, '\0', length + 1);

  match_digits (0, radix, buffer);
  gfc_next_ascii_char ();    /* Eat delimiter.  */
  if (post == 1)
    gfc_next_ascii_char ();  /* Eat postfixed b, o, z, or x.  */

  e = gfc_get_expr ();
  e->expr_type = EXPR_CONSTANT;
  e->ts.type = BT_BOZ;
  e->where = gfc_current_locus;
  e->boz.rdx = radix;
  e->boz.len = length;
  e->boz.str = XCNEWVEC (char, length + 1);
  strncpy (e->boz.str, buffer, length);

  if (!gfc_in_match_data ()
      && (!gfc_notify_std(GFC_STD_F2003, "BOZ used outside a DATA "
			  "statement at %L", &e->where)))
    return MATCH_ERROR;

  *result = e;
  return MATCH_YES;

backup:
  gfc_current_locus = start_loc;
  return MATCH_NO;
}


/* Match a real constant of some sort.  Allow a signed constant if signflag
   is nonzero.  */

static match
match_real_constant (gfc_expr **result, int signflag)
{
  int kind, count, seen_dp, seen_digits, is_iso_c, default_exponent;
  locus old_loc, temp_loc;
  char *p, *buffer, c, exp_char;
  gfc_expr *e;
  bool negate;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  e = NULL;

  default_exponent = 0;
  count = 0;
  seen_dp = 0;
  seen_digits = 0;
  exp_char = ' ';
  negate = false;

  c = gfc_next_ascii_char ();
  if (signflag && (c == '+' || c == '-'))
    {
      if (c == '-')
	negate = true;

      gfc_gobble_whitespace ();
      c = gfc_next_ascii_char ();
    }

  /* Scan significand.  */
  for (;; c = gfc_next_ascii_char (), count++)
    {
      if (c == '.')
	{
	  if (seen_dp)
	    goto done;

	  /* Check to see if "." goes with a following operator like
	     ".eq.".  */
	  temp_loc = gfc_current_locus;
	  c = gfc_next_ascii_char ();

	  if (c == 'e' || c == 'd' || c == 'q')
	    {
	      c = gfc_next_ascii_char ();
	      if (c == '.')
		goto done;	/* Operator named .e. or .d.  */
	    }

	  if (ISALPHA (c))
	    goto done;		/* Distinguish 1.e9 from 1.eq.2 */

	  gfc_current_locus = temp_loc;
	  seen_dp = 1;
	  continue;
	}

      if (ISDIGIT (c))
	{
	  seen_digits = 1;
	  continue;
	}

      break;
    }

  if (!seen_digits || (c != 'e' && c != 'd' && c != 'q'))
    goto done;
  exp_char = c;


  if (c == 'q')
    {
      if (!gfc_notify_std (GFC_STD_GNU, "exponent-letter %<q%> in "
			   "real-literal-constant at %C"))
	return MATCH_ERROR;
      else if (warn_real_q_constant)
	gfc_warning (OPT_Wreal_q_constant,
		     "Extension: exponent-letter %<q%> in real-literal-constant "
		     "at %C");
    }

  /* Scan exponent.  */
  c = gfc_next_ascii_char ();
  count++;

  if (c == '+' || c == '-')
    {				/* optional sign */
      c = gfc_next_ascii_char ();
      count++;
    }

  if (!ISDIGIT (c))
    {
      /* With -fdec, default exponent to 0 instead of complaining.  */
      if (flag_dec)
	default_exponent = 1;
      else
	{
	  gfc_error ("Missing exponent in real number at %C");
	  return MATCH_ERROR;
	}
    }

  while (ISDIGIT (c))
    {
      c = gfc_next_ascii_char ();
      count++;
    }

done:
  /* Check that we have a numeric constant.  */
  if (!seen_digits || (!seen_dp && exp_char == ' '))
    {
      gfc_current_locus = old_loc;
      return MATCH_NO;
    }

  /* Convert the number.  */
  gfc_current_locus = old_loc;
  gfc_gobble_whitespace ();

  buffer = (char *) alloca (count + default_exponent + 1);
  memset (buffer, '\0', count + default_exponent + 1);

  p = buffer;
  c = gfc_next_ascii_char ();
  if (c == '+' || c == '-')
    {
      gfc_gobble_whitespace ();
      c = gfc_next_ascii_char ();
    }

  /* Hack for mpfr_set_str().  */
  for (;;)
    {
      if (c == 'd' || c == 'q')
	*p = 'e';
      else
	*p = c;
      p++;
      if (--count == 0)
	break;

      c = gfc_next_ascii_char ();
    }
  if (default_exponent)
    *p++ = '0';

  kind = get_kind (&is_iso_c);
  if (kind == -1)
    goto cleanup;

  if (kind == 4)
    {
      if (flag_real4_kind == 8)
	kind = 8;
      if (flag_real4_kind == 10)
	kind = 10;
      if (flag_real4_kind == 16)
	kind = 16;
    }
  else if (kind == 8)
    {
      if (flag_real8_kind == 4)
	kind = 4;
      if (flag_real8_kind == 10)
	kind = 10;
      if (flag_real8_kind == 16)
	kind = 16;
    }

  switch (exp_char)
    {
    case 'd':
      if (kind != -2)
	{
	  gfc_error ("Real number at %C has a %<d%> exponent and an explicit "
		     "kind");
	  goto cleanup;
	}
      kind = gfc_default_double_kind;
      break;

    case 'q':
      if (kind != -2)
	{
	  gfc_error ("Real number at %C has a %<q%> exponent and an explicit "
		     "kind");
	  goto cleanup;
	}

      /* The maximum possible real kind type parameter is 16.  First, try
	 that for the kind, then fallback to trying kind=10 (Intel 80 bit)
	 extended precision.  If neither value works, just given up.  */
      kind = 16;
      if (gfc_validate_kind (BT_REAL, kind, true) < 0)
	{
	  kind = 10;
          if (gfc_validate_kind (BT_REAL, kind, true) < 0)
	    {
	      gfc_error ("Invalid exponent-letter %<q%> in "
			 "real-literal-constant at %C");
	      goto cleanup;
	    }
	}
      break;

    default:
      if (kind == -2)
	kind = gfc_default_real_kind;

      if (gfc_validate_kind (BT_REAL, kind, true) < 0)
	{
	  gfc_error ("Invalid real kind %d at %C", kind);
	  goto cleanup;
	}
    }

  e = convert_real (buffer, kind, &gfc_current_locus);
  if (negate)
    mpfr_neg (e->value.real, e->value.real, GFC_RND_MODE);
  e->ts.is_c_interop = is_iso_c;

  switch (gfc_range_check (e))
    {
    case ARITH_OK:
      break;
    case ARITH_OVERFLOW:
      gfc_error ("Real constant overflows its kind at %C");
      goto cleanup;

    case ARITH_UNDERFLOW:
      if (warn_underflow)
	gfc_warning (OPT_Wunderflow, "Real constant underflows its kind at %C");
      mpfr_set_ui (e->value.real, 0, GFC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_range_check() returned bad value");
    }

  /* Warn about trailing digits which suggest the user added too many
     trailing digits, which may cause the appearance of higher precision
     than the kind can support.

     This is done by replacing the rightmost non-zero digit with zero
     and comparing with the original value.  If these are equal, we
     assume the user supplied more digits than intended (or forgot to
     convert to the correct kind).
  */

  if (warn_conversion_extra)
    {
      mpfr_t r;
      char *c1;
      bool did_break;

      c1 = strchr (buffer, 'e');
      if (c1 == NULL)
	c1 = buffer + strlen(buffer);

      did_break = false;
      for (p = c1; p > buffer;)
	{
	  p--;
	  if (*p == '.')
	    continue;

	  if (*p != '0')
	    {
	      *p = '0';
	      did_break = true;
	      break;
	    }
	}

      if (did_break)
	{
	  mpfr_init (r);
	  mpfr_set_str (r, buffer, 10, GFC_RND_MODE);
	  if (negate)
	    mpfr_neg (r, r, GFC_RND_MODE);

	  mpfr_sub (r, r, e->value.real, GFC_RND_MODE);

	  if (mpfr_cmp_ui (r, 0) == 0)
	    gfc_warning (OPT_Wconversion_extra, "Non-significant digits "
			 "in %qs number at %C, maybe incorrect KIND",
			 gfc_typename (&e->ts));

	  mpfr_clear (r);
	}
    }

  *result = e;
  return MATCH_YES;

cleanup:
  gfc_free_expr (e);
  return MATCH_ERROR;
}


/* Match a substring reference.  */

static match
match_substring (gfc_charlen *cl, int init, gfc_ref **result, bool deferred)
{
  gfc_expr *start, *end;
  locus old_loc;
  gfc_ref *ref;
  match m;

  start = NULL;
  end = NULL;

  old_loc = gfc_current_locus;

  m = gfc_match_char ('(');
  if (m != MATCH_YES)
    return MATCH_NO;

  if (gfc_match_char (':') != MATCH_YES)
    {
      if (init)
	m = gfc_match_init_expr (&start);
      else
	m = gfc_match_expr (&start);

      if (m != MATCH_YES)
	{
	  m = MATCH_NO;
	  goto cleanup;
	}

      m = gfc_match_char (':');
      if (m != MATCH_YES)
	goto cleanup;
    }

  if (gfc_match_char (')') != MATCH_YES)
    {
      if (init)
	m = gfc_match_init_expr (&end);
      else
	m = gfc_match_expr (&end);

      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      m = gfc_match_char (')');
      if (m == MATCH_NO)
	goto syntax;
    }

  /* Optimize away the (:) reference.  */
  if (start == NULL && end == NULL && !deferred)
    ref = NULL;
  else
    {
      ref = gfc_get_ref ();

      ref->type = REF_SUBSTRING;
      if (start == NULL)
	start = gfc_get_int_expr (gfc_charlen_int_kind, NULL, 1);
      ref->u.ss.start = start;
      if (end == NULL && cl)
	end = gfc_copy_expr (cl->length);
      ref->u.ss.end = end;
      ref->u.ss.length = cl;
    }

  *result = ref;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in SUBSTRING specification at %C");
  m = MATCH_ERROR;

cleanup:
  gfc_free_expr (start);
  gfc_free_expr (end);

  gfc_current_locus = old_loc;
  return m;
}


/* Reads the next character of a string constant, taking care to
   return doubled delimiters on the input as a single instance of
   the delimiter.

   Special return values for "ret" argument are:
     -1   End of the string, as determined by the delimiter
     -2   Unterminated string detected

   Backslash codes are also expanded at this time.  */

static gfc_char_t
next_string_char (gfc_char_t delimiter, int *ret)
{
  locus old_locus;
  gfc_char_t c;

  c = gfc_next_char_literal (INSTRING_WARN);
  *ret = 0;

  if (c == '\n')
    {
      *ret = -2;
      return 0;
    }

  if (flag_backslash && c == '\\')
    {
      old_locus = gfc_current_locus;

      if (gfc_match_special_char (&c) == MATCH_NO)
	gfc_current_locus = old_locus;

      if (!(gfc_option.allow_std & GFC_STD_GNU) && !inhibit_warnings)
	gfc_warning (0, "Extension: backslash character at %C");
    }

  if (c != delimiter)
    return c;

  old_locus = gfc_current_locus;
  c = gfc_next_char_literal (NONSTRING);

  if (c == delimiter)
    return c;
  gfc_current_locus = old_locus;

  *ret = -1;
  return 0;
}


/* Special case of gfc_match_name() that matches a parameter kind name
   before a string constant.  This takes case of the weird but legal
   case of:

     kind_____'string'

   where kind____ is a parameter. gfc_match_name() will happily slurp
   up all the underscores, which leads to problems.  If we return
   MATCH_YES, the parse pointer points to the final underscore, which
   is not part of the name.  We never return MATCH_ERROR-- errors in
   the name will be detected later.  */

static match
match_charkind_name (char *name)
{
  locus old_loc;
  char c, peek;
  int len;

  gfc_gobble_whitespace ();
  c = gfc_next_ascii_char ();
  if (!ISALPHA (c))
    return MATCH_NO;

  *name++ = c;
  len = 1;

  for (;;)
    {
      old_loc = gfc_current_locus;
      c = gfc_next_ascii_char ();

      if (c == '_')
	{
	  peek = gfc_peek_ascii_char ();

	  if (peek == '\'' || peek == '\"')
	    {
	      gfc_current_locus = old_loc;
	      *name = '\0';
	      return MATCH_YES;
	    }
	}

      if (!ISALNUM (c)
	  && c != '_'
	  && (c != '$' || !flag_dollar_ok))
	break;

      *name++ = c;
      if (++len > GFC_MAX_SYMBOL_LEN)
	break;
    }

  return MATCH_NO;
}


/* See if the current input matches a character constant.  Lots of
   contortions have to be done to match the kind parameter which comes
   before the actual string.  The main consideration is that we don't
   want to error out too quickly.  For example, we don't actually do
   any validation of the kinds until we have actually seen a legal
   delimiter.  Using match_kind_param() generates errors too quickly.  */

static match
match_string_constant (gfc_expr **result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1], peek;
  size_t length;
  int kind,save_warn_ampersand, ret;
  locus old_locus, start_locus;
  gfc_symbol *sym;
  gfc_expr *e;
  match m;
  gfc_char_t c, delimiter, *p;

  old_locus = gfc_current_locus;

  gfc_gobble_whitespace ();

  c = gfc_next_char ();
  if (c == '\'' || c == '"')
    {
      kind = gfc_default_character_kind;
      start_locus = gfc_current_locus;
      goto got_delim;
    }

  if (gfc_wide_is_digit (c))
    {
      kind = 0;

      while (gfc_wide_is_digit (c))
	{
	  kind = kind * 10 + c - '0';
	  if (kind > 9999999)
	    goto no_match;
	  c = gfc_next_char ();
	}

    }
  else
    {
      gfc_current_locus = old_locus;

      m = match_charkind_name (name);
      if (m != MATCH_YES)
	goto no_match;

      if (gfc_find_symbol (name, NULL, 1, &sym)
	  || sym == NULL
	  || sym->attr.flavor != FL_PARAMETER)
	goto no_match;

      kind = -1;
      c = gfc_next_char ();
    }

  if (c != '_')
    goto no_match;

  c = gfc_next_char ();
  if (c != '\'' && c != '"')
    goto no_match;

  start_locus = gfc_current_locus;

  if (kind == -1)
    {
      if (gfc_extract_int (sym->value, &kind, 1))
	return MATCH_ERROR;
      gfc_set_sym_referenced (sym);
    }

  if (gfc_validate_kind (BT_CHARACTER, kind, true) < 0)
    {
      gfc_error ("Invalid kind %d for CHARACTER constant at %C", kind);
      return MATCH_ERROR;
    }

got_delim:
  /* Scan the string into a block of memory by first figuring out how
     long it is, allocating the structure, then re-reading it.  This
     isn't particularly efficient, but string constants aren't that
     common in most code.  TODO: Use obstacks?  */

  delimiter = c;
  length = 0;

  for (;;)
    {
      c = next_string_char (delimiter, &ret);
      if (ret == -1)
	break;
      if (ret == -2)
	{
	  gfc_current_locus = start_locus;
	  gfc_error ("Unterminated character constant beginning at %C");
	  return MATCH_ERROR;
	}

      length++;
    }

  /* Peek at the next character to see if it is a b, o, z, or x for the
     postfixed BOZ literal constants.  */
  peek = gfc_peek_ascii_char ();
  if (peek == 'b' || peek == 'o' || peek =='z' || peek == 'x')
    goto no_match;

  e = gfc_get_character_expr (kind, &start_locus, NULL, length);

  gfc_current_locus = start_locus;

  /* We disable the warning for the following loop as the warning has already
     been printed in the loop above.  */
  save_warn_ampersand = warn_ampersand;
  warn_ampersand = false;

  p = e->value.character.string;
  for (size_t i = 0; i < length; i++)
    {
      c = next_string_char (delimiter, &ret);

      if (!gfc_check_character_range (c, kind))
	{
	  gfc_free_expr (e);
	  gfc_error ("Character %qs in string at %C is not representable "
		     "in character kind %d", gfc_print_wide_char (c), kind);
	  return MATCH_ERROR;
	}

      *p++ = c;
    }

  *p = '\0';	/* TODO: C-style string is for development/debug purposes.  */
  warn_ampersand = save_warn_ampersand;

  next_string_char (delimiter, &ret);
  if (ret != -1)
    gfc_internal_error ("match_string_constant(): Delimiter not found");

  if (match_substring (NULL, 0, &e->ref, false) != MATCH_NO)
    e->expr_type = EXPR_SUBSTRING;

  /* Substrings with constant starting and ending points are eligible as
     designators (F2018, section 9.1).  Simplify substrings to make them usable
     e.g. in data statements.  */
  if (e->expr_type == EXPR_SUBSTRING
      && e->ref && e->ref->type == REF_SUBSTRING
      && e->ref->u.ss.start->expr_type == EXPR_CONSTANT
      && (e->ref->u.ss.end == NULL
	  || e->ref->u.ss.end->expr_type == EXPR_CONSTANT))
    {
      gfc_expr *res;
      ptrdiff_t istart, iend;
      size_t length;
      bool equal_length = false;

      /* Basic checks on substring starting and ending indices.  */
      if (!gfc_resolve_substring (e->ref, &equal_length))
	return MATCH_ERROR;

      length = e->value.character.length;
      istart = gfc_mpz_get_hwi (e->ref->u.ss.start->value.integer);
      if (e->ref->u.ss.end == NULL)
	iend = length;
      else
	iend = gfc_mpz_get_hwi (e->ref->u.ss.end->value.integer);

      if (istart <= iend)
	{
	  if (istart < 1)
	    {
	      gfc_error ("Substring start index (%td) at %L below 1",
			 istart, &e->ref->u.ss.start->where);
	      return MATCH_ERROR;
	    }
	  if (iend > (ssize_t) length)
	    {
	      gfc_error ("Substring end index (%td) at %L exceeds string "
			 "length", iend, &e->ref->u.ss.end->where);
	      return MATCH_ERROR;
	    }
	  length = iend - istart + 1;
	}
      else
	length = 0;

      res = gfc_get_constant_expr (BT_CHARACTER, e->ts.kind, &e->where);
      res->value.character.string = gfc_get_wide_string (length + 1);
      res->value.character.length = length;
      if (length > 0)
	memcpy (res->value.character.string,
		&e->value.character.string[istart - 1],
		length * sizeof (gfc_char_t));
      res->value.character.string[length] = '\0';
      e = res;
    }

  *result = e;

  return MATCH_YES;

no_match:
  gfc_current_locus = old_locus;
  return MATCH_NO;
}


/* Match a .true. or .false.  Returns 1 if a .true. was found,
   0 if a .false. was found, and -1 otherwise.  */
static int
match_logical_constant_string (void)
{
  locus orig_loc = gfc_current_locus;

  gfc_gobble_whitespace ();
  if (gfc_next_ascii_char () == '.')
    {
      char ch = gfc_next_ascii_char ();
      if (ch == 'f')
	{
	  if (gfc_next_ascii_char () == 'a'
	      && gfc_next_ascii_char () == 'l'
	      && gfc_next_ascii_char () == 's'
	      && gfc_next_ascii_char () == 'e'
	      && gfc_next_ascii_char () == '.')
	    /* Matched ".false.".  */
	    return 0;
	}
      else if (ch == 't')
	{
	  if (gfc_next_ascii_char () == 'r'
	      && gfc_next_ascii_char () == 'u'
	      && gfc_next_ascii_char () == 'e'
	      && gfc_next_ascii_char () == '.')
	    /* Matched ".true.".  */
	    return 1;
	}
    }
  gfc_current_locus = orig_loc;
  return -1;
}

/* Match a .true. or .false.  */

static match
match_logical_constant (gfc_expr **result)
{
  gfc_expr *e;
  int i, kind, is_iso_c;

  i = match_logical_constant_string ();
  if (i == -1)
    return MATCH_NO;

  kind = get_kind (&is_iso_c);
  if (kind == -1)
    return MATCH_ERROR;
  if (kind == -2)
    kind = gfc_default_logical_kind;

  if (gfc_validate_kind (BT_LOGICAL, kind, true) < 0)
    {
      gfc_error ("Bad kind for logical constant at %C");
      return MATCH_ERROR;
    }

  e = gfc_get_logical_expr (kind, &gfc_current_locus, i);
  e->ts.is_c_interop = is_iso_c;

  *result = e;
  return MATCH_YES;
}


/* Match a real or imaginary part of a complex constant that is a
   symbolic constant.  */

static match
match_sym_complex_part (gfc_expr **result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  gfc_expr *e;
  match m;

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (gfc_find_symbol (name, NULL, 1, &sym) || sym == NULL)
    return MATCH_NO;

  if (sym->attr.flavor != FL_PARAMETER)
    {
      /* Give the matcher for implied do-loops a chance to run.  This yields
	 a much saner error message for "write(*,*) (i, i=1, 6" where the
	 right parenthesis is missing.  */
      char c;
      gfc_gobble_whitespace ();
      c = gfc_peek_ascii_char ();
      if (c == '=' || c == ',')
	{
	  m = MATCH_NO;
	}
      else
	{
	  gfc_error ("Expected PARAMETER symbol in complex constant at %C");
	  m = MATCH_ERROR;
	}
      return m;
    }

  if (!sym->value)
    goto error;

  if (!gfc_numeric_ts (&sym->value->ts))
    {
      gfc_error ("Numeric PARAMETER required in complex constant at %C");
      return MATCH_ERROR;
    }

  if (sym->value->rank != 0)
    {
      gfc_error ("Scalar PARAMETER required in complex constant at %C");
      return MATCH_ERROR;
    }

  if (!gfc_notify_std (GFC_STD_F2003, "PARAMETER symbol in "
		       "complex constant at %C"))
    return MATCH_ERROR;

  switch (sym->value->ts.type)
    {
    case BT_REAL:
      e = gfc_copy_expr (sym->value);
      break;

    case BT_COMPLEX:
      e = gfc_complex2real (sym->value, sym->value->ts.kind);
      if (e == NULL)
	goto error;
      break;

    case BT_INTEGER:
      e = gfc_int2real (sym->value, gfc_default_real_kind);
      if (e == NULL)
	goto error;
      break;

    default:
      gfc_internal_error ("gfc_match_sym_complex_part(): Bad type");
    }

  *result = e;		/* e is a scalar, real, constant expression.  */
  return MATCH_YES;

error:
  gfc_error ("Error converting PARAMETER constant in complex constant at %C");
  return MATCH_ERROR;
}


/* Match a real or imaginary part of a complex number.  */

static match
match_complex_part (gfc_expr **result)
{
  match m;

  m = match_sym_complex_part (result);
  if (m != MATCH_NO)
    return m;

  m = match_real_constant (result, 1);
  if (m != MATCH_NO)
    return m;

  return match_integer_constant (result, 1);
}


/* Try to match a complex constant.  */

static match
match_complex_constant (gfc_expr **result)
{
  gfc_expr *e, *real, *imag;
  gfc_error_buffer old_error;
  gfc_typespec target;
  locus old_loc;
  int kind;
  match m;

  old_loc = gfc_current_locus;
  real = imag = e = NULL;

  m = gfc_match_char ('(');
  if (m != MATCH_YES)
    return m;

  gfc_push_error (&old_error);

  m = match_complex_part (&real);
  if (m == MATCH_NO)
    {
      gfc_free_error (&old_error);
      goto cleanup;
    }

  if (gfc_match_char (',') == MATCH_NO)
    {
      /* It is possible that gfc_int2real issued a warning when
	 converting an integer to real.  Throw this away here.  */

      gfc_clear_warning ();
      gfc_pop_error (&old_error);
      m = MATCH_NO;
      goto cleanup;
    }

  /* If m is error, then something was wrong with the real part and we
     assume we have a complex constant because we've seen the ','.  An
     ambiguous case here is the start of an iterator list of some
     sort. These sort of lists are matched prior to coming here.  */

  if (m == MATCH_ERROR)
    {
      gfc_free_error (&old_error);
      goto cleanup;
    }
  gfc_pop_error (&old_error);

  m = match_complex_part (&imag);
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto cleanup;

  m = gfc_match_char (')');
  if (m == MATCH_NO)
    {
      /* Give the matcher for implied do-loops a chance to run.  This
	 yields a much saner error message for (/ (i, 4=i, 6) /).  */
      if (gfc_peek_ascii_char () == '=')
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}
      else
    goto syntax;
    }

  if (m == MATCH_ERROR)
    goto cleanup;

  /* Decide on the kind of this complex number.  */
  if (real->ts.type == BT_REAL)
    {
      if (imag->ts.type == BT_REAL)
	kind = gfc_kind_max (real, imag);
      else
	kind = real->ts.kind;
    }
  else
    {
      if (imag->ts.type == BT_REAL)
	kind = imag->ts.kind;
      else
	kind = gfc_default_real_kind;
    }
  gfc_clear_ts (&target);
  target.type = BT_REAL;
  target.kind = kind;

  if (real->ts.type != BT_REAL || kind != real->ts.kind)
    gfc_convert_type (real, &target, 2);
  if (imag->ts.type != BT_REAL || kind != imag->ts.kind)
    gfc_convert_type (imag, &target, 2);

  e = convert_complex (real, imag, kind);
  e->where = gfc_current_locus;

  gfc_free_expr (real);
  gfc_free_expr (imag);

  *result = e;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in COMPLEX constant at %C");
  m = MATCH_ERROR;

cleanup:
  gfc_free_expr (e);
  gfc_free_expr (real);
  gfc_free_expr (imag);
  gfc_current_locus = old_loc;

  return m;
}


/* Match constants in any of several forms.  Returns nonzero for a
   match, zero for no match.  */

match
gfc_match_literal_constant (gfc_expr **result, int signflag)
{
  match m;

  m = match_complex_constant (result);
  if (m != MATCH_NO)
    return m;

  m = match_string_constant (result);
  if (m != MATCH_NO)
    return m;

  m = match_boz_constant (result);
  if (m != MATCH_NO)
    return m;

  m = match_real_constant (result, signflag);
  if (m != MATCH_NO)
    return m;

  m = match_hollerith_constant (result);
  if (m != MATCH_NO)
    return m;

  m = match_integer_constant (result, signflag);
  if (m != MATCH_NO)
    return m;

  m = match_logical_constant (result);
  if (m != MATCH_NO)
    return m;

  return MATCH_NO;
}


/* This checks if a symbol is the return value of an encompassing function.
   Function nesting can be maximally two levels deep, but we may have
   additional local namespaces like BLOCK etc.  */

bool
gfc_is_function_return_value (gfc_symbol *sym, gfc_namespace *ns)
{
  if (!sym->attr.function || (sym->result != sym))
    return false;
  while (ns)
    {
      if (ns->proc_name == sym)
	return true;
      ns = ns->parent;
    }
  return false;
}


/* Match a single actual argument value.  An actual argument is
   usually an expression, but can also be a procedure name.  If the
   argument is a single name, it is not always possible to tell
   whether the name is a dummy procedure or not.  We treat these cases
   by creating an argument that looks like a dummy procedure and
   fixing things later during resolution.  */

static match
match_actual_arg (gfc_expr **result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symtree *symtree;
  locus where, w;
  gfc_expr *e;
  char c;

  gfc_gobble_whitespace ();
  where = gfc_current_locus;

  switch (gfc_match_name (name))
    {
    case MATCH_ERROR:
      return MATCH_ERROR;

    case MATCH_NO:
      break;

    case MATCH_YES:
      w = gfc_current_locus;
      gfc_gobble_whitespace ();
      c = gfc_next_ascii_char ();
      gfc_current_locus = w;

      if (c != ',' && c != ')')
	break;

      if (gfc_find_sym_tree (name, NULL, 1, &symtree))
	break;
      /* Handle error elsewhere.  */

      /* Eliminate a couple of common cases where we know we don't
	 have a function argument.  */
      if (symtree == NULL)
	{
	  gfc_get_sym_tree (name, NULL, &symtree, false);
	  gfc_set_sym_referenced (symtree->n.sym);
	}
      else
	{
	  gfc_symbol *sym;

	  sym = symtree->n.sym;
	  gfc_set_sym_referenced (sym);
	  if (sym->attr.flavor == FL_NAMELIST)
	    {
	      gfc_error ("Namelist %qs cannot be an argument at %L",
	      sym->name, &where);
	      break;
	    }
	  if (sym->attr.flavor != FL_PROCEDURE
	      && sym->attr.flavor != FL_UNKNOWN)
	    break;

	  if (sym->attr.in_common && !sym->attr.proc_pointer)
	    {
	      if (!gfc_add_flavor (&sym->attr, FL_VARIABLE,
				   sym->name, &sym->declared_at))
		return MATCH_ERROR;
	      break;
	    }

	  /* If the symbol is a function with itself as the result and
	     is being defined, then we have a variable.  */
	  if (sym->attr.function && sym->result == sym)
	    {
	      if (gfc_is_function_return_value (sym, gfc_current_ns))
		break;

	      if (sym->attr.entry
		  && (sym->ns == gfc_current_ns
		      || sym->ns == gfc_current_ns->parent))
		{
		  gfc_entry_list *el = NULL;

		  for (el = sym->ns->entries; el; el = el->next)
		    if (sym == el->sym)
		      break;

		  if (el)
		    break;
		}
	    }
	}

      e = gfc_get_expr ();	/* Leave it unknown for now */
      e->symtree = symtree;
      e->expr_type = EXPR_VARIABLE;
      e->ts.type = BT_PROCEDURE;
      e->where = where;

      *result = e;
      return MATCH_YES;
    }

  gfc_current_locus = where;
  return gfc_match_expr (result);
}


/* Match a keyword argument or type parameter spec list..  */

static match
match_keyword_arg (gfc_actual_arglist *actual, gfc_actual_arglist *base, bool pdt)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_actual_arglist *a;
  locus name_locus;
  match m;

  name_locus = gfc_current_locus;
  m = gfc_match_name (name);

  if (m != MATCH_YES)
    goto cleanup;
  if (gfc_match_char ('=') != MATCH_YES)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  if (pdt)
    {
      if (gfc_match_char ('*') == MATCH_YES)
	{
	  actual->spec_type = SPEC_ASSUMED;
	  goto add_name;
	}
      else if (gfc_match_char (':') == MATCH_YES)
	{
	  actual->spec_type = SPEC_DEFERRED;
	  goto add_name;
	}
      else
	actual->spec_type = SPEC_EXPLICIT;
    }

  m = match_actual_arg (&actual->expr);
  if (m != MATCH_YES)
    goto cleanup;

  /* Make sure this name has not appeared yet.  */
add_name:
  if (name[0] != '\0')
    {
      for (a = base; a; a = a->next)
	if (a->name != NULL && strcmp (a->name, name) == 0)
	  {
	    gfc_error ("Keyword %qs at %C has already appeared in the "
		       "current argument list", name);
	    return MATCH_ERROR;
	  }
    }

  actual->name = gfc_get_string ("%s", name);
  return MATCH_YES;

cleanup:
  gfc_current_locus = name_locus;
  return m;
}


/* Match an argument list function, such as %VAL.  */

static match
match_arg_list_function (gfc_actual_arglist *result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  locus old_locus;
  match m;

  old_locus = gfc_current_locus;

  if (gfc_match_char ('%') != MATCH_YES)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  m = gfc_match ("%n (", name);
  if (m != MATCH_YES)
    goto cleanup;

  if (name[0] != '\0')
    {
      switch (name[0])
	{
	case 'l':
	  if (startswith (name, "loc"))
	    {
	      result->name = "%LOC";
	      break;
	    }
	  /* FALLTHRU */
	case 'r':
	  if (startswith (name, "ref"))
	    {
	      result->name = "%REF";
	      break;
	    }
	  /* FALLTHRU */
	case 'v':
	  if (startswith (name, "val"))
	    {
	      result->name = "%VAL";
	      break;
	    }
	  /* FALLTHRU */
	default:
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  if (!gfc_notify_std (GFC_STD_GNU, "argument list function at %C"))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  m = match_actual_arg (&result->expr);
  if (m != MATCH_YES)
    goto cleanup;

  if (gfc_match_char (')') != MATCH_YES)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  return MATCH_YES;

cleanup:
  gfc_current_locus = old_locus;
  return m;
}


/* Matches an actual argument list of a function or subroutine, from
   the opening parenthesis to the closing parenthesis.  The argument
   list is assumed to allow keyword arguments because we don't know if
   the symbol associated with the procedure has an implicit interface
   or not.  We make sure keywords are unique. If sub_flag is set,
   we're matching the argument list of a subroutine.

   NOTE: An alternative use for this function is to match type parameter
   spec lists, which are so similar to actual argument lists that the
   machinery can be reused. This use is flagged by the optional argument
   'pdt'.  */

match
gfc_match_actual_arglist (int sub_flag, gfc_actual_arglist **argp, bool pdt)
{
  gfc_actual_arglist *head, *tail;
  int seen_keyword;
  gfc_st_label *label;
  locus old_loc;
  match m;

  *argp = tail = NULL;
  old_loc = gfc_current_locus;

  seen_keyword = 0;

  if (gfc_match_char ('(') == MATCH_NO)
    return (sub_flag) ? MATCH_YES : MATCH_NO;

  if (gfc_match_char (')') == MATCH_YES)
    return MATCH_YES;

  head = NULL;

  matching_actual_arglist++;

  for (;;)
    {
      if (head == NULL)
	head = tail = gfc_get_actual_arglist ();
      else
	{
	  tail->next = gfc_get_actual_arglist ();
	  tail = tail->next;
	}

      if (sub_flag && !pdt && gfc_match_char ('*') == MATCH_YES)
	{
	  m = gfc_match_st_label (&label);
	  if (m == MATCH_NO)
	    gfc_error ("Expected alternate return label at %C");
	  if (m != MATCH_YES)
	    goto cleanup;

	  if (!gfc_notify_std (GFC_STD_F95_OBS, "Alternate-return argument "
			       "at %C"))
	    goto cleanup;

	  tail->label = label;
	  goto next;
	}

      if (pdt && !seen_keyword)
	{
	  if (gfc_match_char (':') == MATCH_YES)
	    {
	      tail->spec_type = SPEC_DEFERRED;
	      goto next;
	    }
	  else if (gfc_match_char ('*') == MATCH_YES)
	    {
	      tail->spec_type = SPEC_ASSUMED;
	      goto next;
	    }
	  else
	    tail->spec_type = SPEC_EXPLICIT;

	  m = match_keyword_arg (tail, head, pdt);
	  if (m == MATCH_YES)
	    {
	      seen_keyword = 1;
	      goto next;
	    }
	  if (m == MATCH_ERROR)
	    goto cleanup;
	}

      /* After the first keyword argument is seen, the following
	 arguments must also have keywords.  */
      if (seen_keyword)
	{
	  m = match_keyword_arg (tail, head, pdt);

	  if (m == MATCH_ERROR)
	    goto cleanup;
	  if (m == MATCH_NO)
	    {
	      gfc_error ("Missing keyword name in actual argument list at %C");
	      goto cleanup;
	    }

	}
      else
	{
	  /* Try an argument list function, like %VAL.  */
	  m = match_arg_list_function (tail);
	  if (m == MATCH_ERROR)
	    goto cleanup;

	  /* See if we have the first keyword argument.  */
	  if (m == MATCH_NO)
	    {
	      m = match_keyword_arg (tail, head, false);
	      if (m == MATCH_YES)
		seen_keyword = 1;
	      if (m == MATCH_ERROR)
		goto cleanup;
	    }

	  if (m == MATCH_NO)
	    {
	      /* Try for a non-keyword argument.  */
	      m = match_actual_arg (&tail->expr);
	      if (m == MATCH_ERROR)
		goto cleanup;
	      if (m == MATCH_NO)
		goto syntax;
	    }
	}


    next:
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  *argp = head;
  matching_actual_arglist--;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in argument list at %C");

cleanup:
  gfc_free_actual_arglist (head);
  gfc_current_locus = old_loc;
  matching_actual_arglist--;
  return MATCH_ERROR;
}


/* Used by gfc_match_varspec() to extend the reference list by one
   element.  */

static gfc_ref *
extend_ref (gfc_expr *primary, gfc_ref *tail)
{
  if (primary->ref == NULL)
    primary->ref = tail = gfc_get_ref ();
  else
    {
      if (tail == NULL)
	gfc_internal_error ("extend_ref(): Bad tail");
      tail->next = gfc_get_ref ();
      tail = tail->next;
    }

  return tail;
}


/* Used by gfc_match_varspec() to match an inquiry reference.  */

bool
is_inquiry_ref (const char *name, gfc_ref **ref)
{
  inquiry_type type;

  if (name == NULL)
    return false;

  if (ref) *ref = NULL;

  if (strcmp (name, "re") == 0)
    type = INQUIRY_RE;
  else if (strcmp (name, "im") == 0)
    type = INQUIRY_IM;
  else if (strcmp (name, "kind") == 0)
    type = INQUIRY_KIND;
  else if (strcmp (name, "len") == 0)
    type = INQUIRY_LEN;
  else
    return false;

  if (ref)
    {
      *ref = gfc_get_ref ();
      (*ref)->type = REF_INQUIRY;
      (*ref)->u.i = type;
    }

  return true;
}


/* Check to see if functions in operator expressions can be resolved now.  */

static bool
resolvable_fcns (gfc_expr *e,
		  gfc_symbol *sym ATTRIBUTE_UNUSED,
		  int *f ATTRIBUTE_UNUSED)
{
  bool p;
  gfc_symbol *s;

  if (e->expr_type != EXPR_FUNCTION)
    return false;

  s = e && e->symtree && e->symtree->n.sym ? e->symtree->n.sym : NULL;
  p = s && (s->attr.use_assoc
	    || s->attr.host_assoc
	    || s->attr.if_source == IFSRC_DECL
	    || s->attr.proc == PROC_INTRINSIC
	    || gfc_is_intrinsic (s, 0, e->where));
  return !p;
}


/* Match any additional specifications associated with the current
   variable like member references or substrings.  If equiv_flag is
   set we only match stuff that is allowed inside an EQUIVALENCE
   statement.  sub_flag tells whether we expect a type-bound procedure found
   to be a subroutine as part of CALL or a FUNCTION. For procedure pointer
   components, 'ppc_arg' determines whether the PPC may be called (with an
   argument list), or whether it may just be referred to as a pointer.  */

match
gfc_match_varspec (gfc_expr *primary, int equiv_flag, bool sub_flag,
		   bool ppc_arg)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_ref *substring, *tail, *tmp;
  gfc_component *component = NULL;
  gfc_component *previous = NULL;
  gfc_symbol *sym = primary->symtree->n.sym;
  gfc_expr *tgt_expr = NULL;
  match m;
  bool unknown;
  bool inquiry;
  bool intrinsic;
  bool inferred_type;
  locus old_loc;
  char sep;

  tail = NULL;

  gfc_gobble_whitespace ();

  if (gfc_peek_ascii_char () == '[')
    {
      if ((sym->ts.type != BT_CLASS && sym->attr.dimension)
	  || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	      && CLASS_DATA (sym)->attr.dimension))
	{
	  gfc_error ("Array section designator, e.g. %<(:)%>, is required "
		     "besides the coarray designator %<[...]%> at %C");
	  return MATCH_ERROR;
	}
      if ((sym->ts.type != BT_CLASS && !sym->attr.codimension)
	  || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	      && !CLASS_DATA (sym)->attr.codimension))
	{
	  gfc_error ("Coarray designator at %C but %qs is not a coarray",
		     sym->name);
	  return MATCH_ERROR;
	}
    }

  if (sym->assoc && sym->assoc->target)
    tgt_expr = sym->assoc->target;

  inferred_type = IS_INFERRED_TYPE (primary);

  /* SELECT TYPE temporaries within an ASSOCIATE block, whose selector has not
     been parsed, can generate errors with array refs.. The SELECT TYPE
     namespace is marked with 'assoc_name_inferred'. During resolution, this is
     detected and gfc_fixup_inferred_type_refs is called.  */
  if (!inferred_type
      && sym->attr.select_type_temporary
      && sym->ns->assoc_name_inferred
      && !sym->attr.select_rank_temporary)
    inferred_type = true;

  /* For associate names, we may not yet know whether they are arrays or not.
     If the selector expression is unambiguously an array; eg. a full array
     or an array section, then the associate name must be an array and we can
     fix it now. Otherwise, if parentheses follow and it is not a character
     type, we have to assume that it actually is one for now.  The final
     decision will be made at resolution, of course.  */
  if (sym->assoc
      && gfc_peek_ascii_char () == '('
      && sym->ts.type != BT_CLASS
      && !sym->attr.dimension)
    {
      gfc_ref *ref = NULL;

      if (!sym->assoc->dangling && tgt_expr)
	{
	   if (tgt_expr->expr_type == EXPR_VARIABLE)
	     gfc_resolve_expr (tgt_expr);

	   ref = tgt_expr->ref;
	   for (; ref; ref = ref->next)
	      if (ref->type == REF_ARRAY
		  && (ref->u.ar.type == AR_FULL
		      || ref->u.ar.type == AR_SECTION))
		break;
	}

      if (ref || (!(sym->assoc->dangling || sym->ts.type == BT_CHARACTER)
		  && sym->assoc->st
		  && sym->assoc->st->n.sym
		  && sym->assoc->st->n.sym->attr.dimension == 0))
	{
	  sym->attr.dimension = 1;
	  if (sym->as == NULL
	      && sym->assoc->st
	      && sym->assoc->st->n.sym
	      && sym->assoc->st->n.sym->as)
	    sym->as = gfc_copy_array_spec (sym->assoc->st->n.sym->as);
	}
    }
  else if (sym->ts.type == BT_CLASS
	   && tgt_expr
	   && tgt_expr->expr_type == EXPR_VARIABLE
	   && sym->ts.u.derived != tgt_expr->ts.u.derived)
    {
      gfc_resolve_expr (tgt_expr);
      if (tgt_expr->rank)
	sym->ts.u.derived = tgt_expr->ts.u.derived;
    }

  if ((inferred_type && !sym->as && gfc_peek_ascii_char () == '(')
      || (equiv_flag && gfc_peek_ascii_char () == '(')
      || gfc_peek_ascii_char () == '[' || sym->attr.codimension
      || (sym->attr.dimension && sym->ts.type != BT_CLASS
	  && !sym->attr.proc_pointer && !gfc_is_proc_ptr_comp (primary)
	  && !(gfc_matching_procptr_assignment
	       && sym->attr.flavor == FL_PROCEDURE))
      || (sym->ts.type == BT_CLASS && sym->attr.class_ok
	  && sym->ts.u.derived && CLASS_DATA (sym)
	  && (CLASS_DATA (sym)->attr.dimension
	      || CLASS_DATA (sym)->attr.codimension)))
    {
      gfc_array_spec *as;

      tail = extend_ref (primary, tail);
      tail->type = REF_ARRAY;

      /* In EQUIVALENCE, we don't know yet whether we are seeing
	 an array, character variable or array of character
	 variables.  We'll leave the decision till resolve time.  */

      if (equiv_flag)
	as = NULL;
      else if (sym->ts.type == BT_CLASS && CLASS_DATA (sym))
	as = CLASS_DATA (sym)->as;
      else
	as = sym->as;

      m = gfc_match_array_ref (&tail->u.ar, as, equiv_flag,
			       as ? as->corank : 0);
      if (m != MATCH_YES)
	return m;

      gfc_gobble_whitespace ();
      if (equiv_flag && gfc_peek_ascii_char () == '(')
	{
	  tail = extend_ref (primary, tail);
	  tail->type = REF_ARRAY;

	  m = gfc_match_array_ref (&tail->u.ar, NULL, equiv_flag, 0);
	  if (m != MATCH_YES)
	    return m;
	}
    }

  primary->ts = sym->ts;

  if (equiv_flag)
    return MATCH_YES;

  /* With DEC extensions, member separator may be '.' or '%'.  */
  sep = gfc_peek_ascii_char ();
  m = gfc_match_member_sep (sym);
  if (m == MATCH_ERROR)
    return MATCH_ERROR;

  inquiry = false;
  if (m == MATCH_YES && sep == '%'
      && primary->ts.type != BT_CLASS
      && (primary->ts.type != BT_DERIVED || inferred_type))
    {
      match mm;
      old_loc = gfc_current_locus;
      mm = gfc_match_name (name);

      /* Check to see if this has a default complex.  */
      if (sym->ts.type == BT_UNKNOWN && tgt_expr == NULL
	  && gfc_get_default_type (sym->name, sym->ns)->type != BT_UNKNOWN)
	{
	  gfc_set_default_type (sym, 0, sym->ns);
	  primary->ts = sym->ts;
	}

      /* This is a usable inquiry reference, if the symbol is already known
	 to have a type or no derived types with a component of this name
	 can be found.  If this was an inquiry reference with the same name
	 as a derived component and the associate-name type is not derived
	 or class, this is fixed up in 'gfc_fixup_inferred_type_refs'.  */
      if (mm == MATCH_YES && is_inquiry_ref (name, &tmp)
	  && !(sym->ts.type == BT_UNKNOWN
		&& gfc_find_derived_types (sym, gfc_current_ns, name)))
	inquiry = true;
      gfc_current_locus = old_loc;
    }

  /* Use the default type if there is one.  */
  if (sym->ts.type == BT_UNKNOWN && m == MATCH_YES
      && gfc_get_default_type (sym->name, sym->ns)->type == BT_DERIVED)
    gfc_set_default_type (sym, 0, sym->ns);

  /* See if the type can be determined by resolution of the selector expression,
     if allowable now, or inferred from references.  */
  if ((sym->ts.type == BT_UNKNOWN || inferred_type)
      && m == MATCH_YES)
    {
      bool sym_present, resolved = false;
      gfc_symbol *tgt_sym;

      sym_present = tgt_expr && tgt_expr->symtree && tgt_expr->symtree->n.sym;
      tgt_sym = sym_present ? tgt_expr->symtree->n.sym : NULL;

      /* These target expressions can be resolved at any time:
	 (i) With a declared symbol or intrinsic function; or
	 (ii) An operator expression,
	 just as long as (iii) all the functions in the expression have been
	 declared or are intrinsic.  */
      if (((sym_present						      // (i)
	    && (tgt_sym->attr.use_assoc
		|| tgt_sym->attr.host_assoc
		|| tgt_sym->attr.if_source == IFSRC_DECL
		|| tgt_sym->attr.proc == PROC_INTRINSIC
		|| gfc_is_intrinsic (tgt_sym, 0, tgt_expr->where)))
	   || (tgt_expr && tgt_expr->expr_type == EXPR_OP))	      // (ii)
	  && !gfc_traverse_expr (tgt_expr, NULL, resolvable_fcns, 0)  // (iii)
	  && gfc_resolve_expr (tgt_expr))
	{
	  sym->ts = tgt_expr->ts;
	  primary->ts = sym->ts;
	  resolved = true;
	}

      /* If this hasn't done the trick and the target expression is a function,
	 or an unresolved operator expression, then this must be a derived type
	 if 'name' matches an accessible type both in this namespace and in the
	 as yet unparsed contained function. In principle, the type could have
	 already been inferred to be complex and yet a derived type with a
	 component name 're' or 'im' could be found.  */
      if (tgt_expr
	  && (tgt_expr->expr_type == EXPR_FUNCTION
	      || (!resolved && tgt_expr->expr_type == EXPR_OP))
	  && (sym->ts.type == BT_UNKNOWN
	      || (inferred_type && sym->ts.type != BT_COMPLEX))
	  && gfc_find_derived_types (sym, gfc_current_ns, name, true))
	{
	  sym->assoc->inferred_type = 1;
	  /* The first returned type is as good as any at this stage. The final
	     determination is made in 'gfc_fixup_inferred_type_refs'*/
	  gfc_symbol **dts = &sym->assoc->derived_types;
	  tgt_expr->ts.type = BT_DERIVED;
	  tgt_expr->ts.kind = 0;
	  tgt_expr->ts.u.derived = *dts;
	  sym->ts = tgt_expr->ts;
	  primary->ts = sym->ts;
	  /* Delete the dt list even if this process has to be done again for
	     another primary expression.  */
	  while (*dts && (*dts)->dt_next)
	    {
	      gfc_symbol **tmp = &(*dts)->dt_next;
	      *dts = NULL;
	      dts = tmp;
	    }
	}
      /* If there is a usable inquiry reference not there are no matching
	 derived types, force the inquiry reference by setting unknown the
	 type of the primary expression.  */
      else if (inquiry && (sym->ts.type == BT_DERIVED && inferred_type)
	       && !gfc_find_derived_types (sym, gfc_current_ns, name))
	primary->ts.type = BT_UNKNOWN;

      /* An inquiry reference might determine the type, otherwise we have an
	 error.  */
      if (sym->ts.type == BT_UNKNOWN && !inquiry)
	{
	  gfc_error ("Symbol %qs at %C has no IMPLICIT type", sym->name);
	  return MATCH_ERROR;
	}
    }
  else if ((sym->ts.type != BT_DERIVED && sym->ts.type != BT_CLASS)
           && m == MATCH_YES && !inquiry)
    {
      gfc_error ("Unexpected %<%c%> for nonderived-type variable %qs at %C",
		 sep, sym->name);
      return MATCH_ERROR;
    }

  if ((sym->ts.type != BT_DERIVED && sym->ts.type != BT_CLASS && !inquiry)
      || m != MATCH_YES)
    goto check_substring;

  if (!inquiry)
    sym = sym->ts.u.derived;
  else
    sym = NULL;

  for (;;)
    {
      bool t;
      gfc_symtree *tbp;

      m = gfc_match_name (name);
      if (m == MATCH_NO)
	gfc_error ("Expected structure component name at %C");
      if (m != MATCH_YES)
	return MATCH_ERROR;

      intrinsic = false;
      if (primary->ts.type != BT_CLASS && primary->ts.type != BT_DERIVED)
	{
	  inquiry = is_inquiry_ref (name, &tmp);
	  if (inquiry)
	    sym = NULL;

	  if (sep == '%')
	    {
	      if (tmp)
		{
		  gfc_symbol *s;
		  switch (tmp->u.i)
		    {
		    case INQUIRY_RE:
		    case INQUIRY_IM:
		      if (!gfc_notify_std (GFC_STD_F2008,
					   "RE or IM part_ref at %C"))
			return MATCH_ERROR;
		      break;

		    case INQUIRY_KIND:
		      if (!gfc_notify_std (GFC_STD_F2003,
					   "KIND part_ref at %C"))
			return MATCH_ERROR;
		      break;

		    case INQUIRY_LEN:
		      if (!gfc_notify_std (GFC_STD_F2003, "LEN part_ref at %C"))
			return MATCH_ERROR;
		      break;
		    }

		  /* If necessary, infer the type of the primary expression
		     and the associate-name using the the inquiry ref..  */
		  s = primary->symtree ? primary->symtree->n.sym : NULL;
		  if (s && s->assoc && s->assoc->target
		      && (s->ts.type == BT_UNKNOWN
			  || (primary->ts.type == BT_UNKNOWN
			      && s->assoc->inferred_type
			      && s->ts.type == BT_DERIVED)))
		    {
		      if (tmp->u.i == INQUIRY_RE || tmp->u.i == INQUIRY_IM)
			{
			  s->ts.type = BT_COMPLEX;
			  s->ts.kind = gfc_default_real_kind;;
			  s->assoc->inferred_type = 1;
			  primary->ts = s->ts;
			}
		      else if (tmp->u.i == INQUIRY_LEN)
			{
			  s->ts.type = BT_CHARACTER;
			  s->ts.kind = gfc_default_character_kind;;
			  s->assoc->inferred_type = 1;
			  primary->ts = s->ts;
			}
		      else if (s->ts.type == BT_UNKNOWN)
			{
			  /* KIND inquiry gives no clue as to symbol type.  */
			  primary->ref = tmp;
			  primary->ts.type = BT_INTEGER;
			  primary->ts.kind = gfc_default_integer_kind;
			  return MATCH_YES;
			}
		    }

		  if ((tmp->u.i == INQUIRY_RE || tmp->u.i == INQUIRY_IM)
		      && primary->ts.type != BT_COMPLEX)
		    {
			gfc_error ("The RE or IM part_ref at %C must be "
				   "applied to a COMPLEX expression");
			return MATCH_ERROR;
		    }
		  else if (tmp->u.i == INQUIRY_LEN
			   && primary->ts.type != BT_CHARACTER)
		    {
			gfc_error ("The LEN part_ref at %C must be applied "
				   "to a CHARACTER expression");
			return MATCH_ERROR;
		    }
		}
	      if (primary->ts.type != BT_UNKNOWN)
		intrinsic = true;
	    }
	}
      else
	inquiry = false;

      if (sym && sym->f2k_derived)
	tbp = gfc_find_typebound_proc (sym, &t, name, false, &gfc_current_locus);
      else
	tbp = NULL;

      if (tbp)
	{
	  gfc_symbol* tbp_sym;

	  if (!t)
	    return MATCH_ERROR;

	  gcc_assert (!tail || !tail->next);

	  if (!(primary->expr_type == EXPR_VARIABLE
		|| (primary->expr_type == EXPR_STRUCTURE
		    && primary->symtree && primary->symtree->n.sym
		    && primary->symtree->n.sym->attr.flavor)))
	    return MATCH_ERROR;

	  if (tbp->n.tb->is_generic)
	    tbp_sym = NULL;
	  else
	    tbp_sym = tbp->n.tb->u.specific->n.sym;

	  primary->expr_type = EXPR_COMPCALL;
	  primary->value.compcall.tbp = tbp->n.tb;
	  primary->value.compcall.name = tbp->name;
	  primary->value.compcall.ignore_pass = 0;
	  primary->value.compcall.assign = 0;
	  primary->value.compcall.base_object = NULL;
	  gcc_assert (primary->symtree->n.sym->attr.referenced);
	  if (tbp_sym)
	    primary->ts = tbp_sym->ts;
	  else
	    gfc_clear_ts (&primary->ts);

	  m = gfc_match_actual_arglist (tbp->n.tb->subroutine,
					&primary->value.compcall.actual);
	  if (m == MATCH_ERROR)
	    return MATCH_ERROR;
	  if (m == MATCH_NO)
	    {
	      if (sub_flag)
		primary->value.compcall.actual = NULL;
	      else
		{
		  gfc_error ("Expected argument list at %C");
		  return MATCH_ERROR;
		}
	    }

	  break;
	}

      previous = component;

      if (!inquiry && !intrinsic)
	component = gfc_find_component (sym, name, false, false, &tmp);
      else
	component = NULL;

      if (intrinsic && !inquiry)
	{
	  if (previous)
	    gfc_error ("%qs at %C is not an inquiry reference to an intrinsic "
			"type component %qs", name, previous->name);
	  else
	    gfc_error ("%qs at %C is not an inquiry reference to an intrinsic "
			"type component", name);
	  return MATCH_ERROR;
	}
      else if (component == NULL && !inquiry)
	return MATCH_ERROR;

      /* Extend the reference chain determined by gfc_find_component or
	 is_inquiry_ref.  */
      if (primary->ref == NULL)
	primary->ref = tmp;
      else
	{
	  /* Set by the for loop below for the last component ref.  */
	  gcc_assert (tail != NULL);
	  tail->next = tmp;
	}

      /* The reference chain may be longer than one hop for union
	 subcomponents; find the new tail.  */
      for (tail = tmp; tail->next; tail = tail->next)
	;

      if (tmp && tmp->type == REF_INQUIRY)
	{
	  if (!primary->where.lb || !primary->where.nextc)
	    primary->where = gfc_current_locus;
	  gfc_simplify_expr (primary, 0);

	  if (primary->expr_type == EXPR_CONSTANT)
	    goto check_done;

	  switch (tmp->u.i)
	    {
	    case INQUIRY_RE:
	    case INQUIRY_IM:
	      if (!gfc_notify_std (GFC_STD_F2008, "RE or IM part_ref at %C"))
		return MATCH_ERROR;

	      if (primary->ts.type != BT_COMPLEX)
		{
		  gfc_error ("The RE or IM part_ref at %C must be "
			     "applied to a COMPLEX expression");
		  return MATCH_ERROR;
		}
	      primary->ts.type = BT_REAL;
	      break;

	    case INQUIRY_LEN:
	      if (!gfc_notify_std (GFC_STD_F2003, "LEN part_ref at %C"))
		return MATCH_ERROR;

	      if (primary->ts.type != BT_CHARACTER)
		{
		  gfc_error ("The LEN part_ref at %C must be applied "
			     "to a CHARACTER expression");
		  return MATCH_ERROR;
		}
	      primary->ts.u.cl = NULL;
	      primary->ts.type = BT_INTEGER;
	      primary->ts.kind = gfc_default_integer_kind;
	      break;

	    case INQUIRY_KIND:
	      if (!gfc_notify_std (GFC_STD_F2003, "KIND part_ref at %C"))
		return MATCH_ERROR;

	      if (primary->ts.type == BT_CLASS
		  || primary->ts.type == BT_DERIVED)
		{
		  gfc_error ("The KIND part_ref at %C must be applied "
			     "to an expression of intrinsic type");
		  return MATCH_ERROR;
		}
	      primary->ts.type = BT_INTEGER;
	      primary->ts.kind = gfc_default_integer_kind;
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  goto check_done;
	}

      primary->ts = component->ts;

      if (component->attr.proc_pointer && ppc_arg)
	{
	  /* Procedure pointer component call: Look for argument list.  */
	  m = gfc_match_actual_arglist (sub_flag,
					&primary->value.compcall.actual);
	  if (m == MATCH_ERROR)
	    return MATCH_ERROR;

	  if (m == MATCH_NO && !gfc_matching_ptr_assignment
	      && !gfc_matching_procptr_assignment && !matching_actual_arglist)
	    {
	      gfc_error ("Procedure pointer component %qs requires an "
			 "argument list at %C", component->name);
	      return MATCH_ERROR;
	    }

	  if (m == MATCH_YES)
	    primary->expr_type = EXPR_PPC;

          break;
	}

      if (component->as != NULL && !component->attr.proc_pointer)
	{
	  tail = extend_ref (primary, tail);
	  tail->type = REF_ARRAY;

	  m = gfc_match_array_ref (&tail->u.ar, component->as, equiv_flag,
			  component->as->corank);
	  if (m != MATCH_YES)
	    return m;
	}
      else if (component->ts.type == BT_CLASS && component->attr.class_ok
	       && CLASS_DATA (component)->as && !component->attr.proc_pointer)
	{
	  tail = extend_ref (primary, tail);
	  tail->type = REF_ARRAY;

	  m = gfc_match_array_ref (&tail->u.ar, CLASS_DATA (component)->as,
				   equiv_flag,
				   CLASS_DATA (component)->as->corank);
	  if (m != MATCH_YES)
	    return m;
	}

check_done:
      /* In principle, we could have eg. expr%re%kind so we must allow for
	 this possibility.  */
      if (gfc_match_char ('%') == MATCH_YES)
	{
	  if (component && (component->ts.type == BT_DERIVED
			    || component->ts.type == BT_CLASS))
	    sym = component->ts.u.derived;
	  continue;
	}
      else if (inquiry)
	break;

      if ((component->ts.type != BT_DERIVED && component->ts.type != BT_CLASS)
  	  || gfc_match_member_sep (component->ts.u.derived) != MATCH_YES)
	break;

      if (component->ts.type == BT_DERIVED || component->ts.type == BT_CLASS)
	sym = component->ts.u.derived;
    }

check_substring:
  unknown = false;
  if (primary->ts.type == BT_UNKNOWN && !gfc_fl_struct (sym->attr.flavor))
    {
      if (gfc_get_default_type (sym->name, sym->ns)->type == BT_CHARACTER)
       {
	 gfc_set_default_type (sym, 0, sym->ns);
	 primary->ts = sym->ts;
	 unknown = true;
       }
    }

  if (primary->ts.type == BT_CHARACTER)
    {
      bool def = primary->ts.deferred == 1;
      switch (match_substring (primary->ts.u.cl, equiv_flag, &substring, def))
	{
	case MATCH_YES:
	  if (tail == NULL)
	    primary->ref = substring;
	  else
	    tail->next = substring;

	  if (primary->expr_type == EXPR_CONSTANT)
	    primary->expr_type = EXPR_SUBSTRING;

	  if (substring)
	    primary->ts.u.cl = NULL;

	  break;

	case MATCH_NO:
	  if (unknown)
	    {
	      gfc_clear_ts (&primary->ts);
	      gfc_clear_ts (&sym->ts);
	    }
	  break;

	case MATCH_ERROR:
	  return MATCH_ERROR;
	}
    }

  /* F08:C611.  */
  if (primary->ts.type == BT_DERIVED && primary->ref
      && primary->ts.u.derived && primary->ts.u.derived->attr.abstract)
    {
      gfc_error ("Nonpolymorphic reference to abstract type at %C");
      return MATCH_ERROR;
    }

  /* F08:C727.  */
  if (primary->expr_type == EXPR_PPC && gfc_is_coindexed (primary))
    {
      gfc_error ("Coindexed procedure-pointer component at %C");
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* Given an expression that is a variable, figure out what the
   ultimate variable's type and attribute is, traversing the reference
   structures if necessary.

   This subroutine is trickier than it looks.  We start at the base
   symbol and store the attribute.  Component references load a
   completely new attribute.

   A couple of rules come into play.  Subobjects of targets are always
   targets themselves.  If we see a component that goes through a
   pointer, then the expression must also be a target, since the
   pointer is associated with something (if it isn't core will soon be
   dumped).  If we see a full part or section of an array, the
   expression is also an array.

   We can have at most one full array reference.  */

symbol_attribute
gfc_variable_attr (gfc_expr *expr, gfc_typespec *ts)
{
  int dimension, codimension, pointer, allocatable, target, optional;
  symbol_attribute attr;
  gfc_ref *ref;
  gfc_symbol *sym;
  gfc_component *comp;
  bool has_inquiry_part;

  if (expr->expr_type != EXPR_VARIABLE
      && expr->expr_type != EXPR_FUNCTION
      && !(expr->expr_type == EXPR_NULL && expr->ts.type != BT_UNKNOWN))
    gfc_internal_error ("gfc_variable_attr(): Expression isn't a variable");

  sym = expr->symtree->n.sym;
  attr = sym->attr;

  optional = attr.optional;
  if (sym->ts.type == BT_CLASS && sym->attr.class_ok && sym->ts.u.derived)
    {
      dimension = CLASS_DATA (sym)->attr.dimension;
      codimension = CLASS_DATA (sym)->attr.codimension;
      pointer = CLASS_DATA (sym)->attr.class_pointer;
      allocatable = CLASS_DATA (sym)->attr.allocatable;
    }
  else
    {
      dimension = attr.dimension;
      codimension = attr.codimension;
      pointer = attr.pointer;
      allocatable = attr.allocatable;
    }

  target = attr.target;
  if (pointer || attr.proc_pointer)
    target = 1;

  /* F2018:11.1.3.3: Other attributes of associate names
     "The associating entity does not have the ALLOCATABLE or POINTER
     attributes; it has the TARGET attribute if and only if the selector is
     a variable and has either the TARGET or POINTER attribute."  */
  if (sym->attr.associate_var && sym->assoc && sym->assoc->target)
    {
      if (sym->assoc->target->expr_type == EXPR_VARIABLE)
	{
	  symbol_attribute tgt_attr;
	  tgt_attr = gfc_expr_attr (sym->assoc->target);
	  target = (tgt_attr.pointer || tgt_attr.target);
	}
      else
	target = 0;
    }

  if (ts != NULL && expr->ts.type == BT_UNKNOWN)
    *ts = sym->ts;

  /* Catch left-overs from match_actual_arg, where an actual argument of a
     procedure is given a temporary ts.type == BT_PROCEDURE.  The fixup is
     needed for structure constructors in DATA statements, where a pointer
     is associated with a data target, and the argument has not been fully
     resolved yet.  Components references are dealt with further below.  */
  if (ts != NULL
      && expr->ts.type == BT_PROCEDURE
      && expr->ref == NULL
      && attr.flavor != FL_PROCEDURE
      && attr.target)
    *ts = sym->ts;

  has_inquiry_part = false;
  for (ref = expr->ref; ref; ref = ref->next)
    if (ref->type == REF_INQUIRY)
      {
	has_inquiry_part = true;
	optional = false;
	break;
      }

  for (ref = expr->ref; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:

	switch (ref->u.ar.type)
	  {
	  case AR_FULL:
	    dimension = 1;
	    break;

	  case AR_SECTION:
	    allocatable = pointer = 0;
	    dimension = 1;
	    optional = false;
	    break;

	  case AR_ELEMENT:
	    /* Handle coarrays.  */
	    if (ref->u.ar.dimen > 0)
	      allocatable = pointer = optional = false;
	    break;

	  case AR_UNKNOWN:
	    /* For standard conforming code, AR_UNKNOWN should not happen.
	       For nonconforming code, gfortran can end up here.  Treat it
	       as a no-op.  */
	    break;
	  }

	break;

      case REF_COMPONENT:
	optional = false;
	comp = ref->u.c.component;
	attr = comp->attr;
	if (ts != NULL && !has_inquiry_part)
	  {
	    *ts = comp->ts;
	    /* Don't set the string length if a substring reference
	       follows.  */
	    if (ts->type == BT_CHARACTER
		&& ref->next && ref->next->type == REF_SUBSTRING)
		ts->u.cl = NULL;
	  }

	if (comp->ts.type == BT_CLASS)
	  {
	    codimension = CLASS_DATA (comp)->attr.codimension;
	    pointer = CLASS_DATA (comp)->attr.class_pointer;
	    allocatable = CLASS_DATA (comp)->attr.allocatable;
	  }
	else
	  {
	    codimension = comp->attr.codimension;
	    if (expr->ts.type == BT_CLASS && strcmp (comp->name, "_data") == 0)
	      pointer = comp->attr.class_pointer;
	    else
	      pointer = comp->attr.pointer;
	    allocatable = comp->attr.allocatable;
	  }
	if (pointer || attr.proc_pointer)
	  target = 1;

	break;

      case REF_INQUIRY:
      case REF_SUBSTRING:
	allocatable = pointer = optional = false;
	break;
      }

  attr.dimension = dimension;
  attr.codimension = codimension;
  attr.pointer = pointer;
  attr.allocatable = allocatable;
  attr.target = target;
  attr.save = sym->attr.save;
  attr.optional = optional;

  return attr;
}


/* Return the attribute from a general expression.  */

symbol_attribute
gfc_expr_attr (gfc_expr *e)
{
  symbol_attribute attr;

  switch (e->expr_type)
    {
    case EXPR_VARIABLE:
      attr = gfc_variable_attr (e, NULL);
      break;

    case EXPR_FUNCTION:
      gfc_clear_attr (&attr);

      if (e->value.function.esym && e->value.function.esym->result)
	{
	  gfc_symbol *sym = e->value.function.esym->result;
	  attr = sym->attr;
	  if (sym->ts.type == BT_CLASS && sym->attr.class_ok)
	    {
	      attr.dimension = CLASS_DATA (sym)->attr.dimension;
	      attr.pointer = CLASS_DATA (sym)->attr.class_pointer;
	      attr.allocatable = CLASS_DATA (sym)->attr.allocatable;
	    }
	}
      else if (e->value.function.isym
	       && e->value.function.isym->transformational
	       && e->ts.type == BT_CLASS)
	attr = CLASS_DATA (e)->attr;
      else if (e->symtree)
	attr = gfc_variable_attr (e, NULL);

      /* TODO: NULL() returns pointers.  May have to take care of this
	 here.  */

      break;

    default:
      gfc_clear_attr (&attr);
      break;
    }

  return attr;
}


/* Given an expression, figure out what the ultimate expression
   attribute is.  This routine is similar to gfc_variable_attr with
   parts of gfc_expr_attr, but focuses more on the needs of
   coarrays.  For coarrays a codimension attribute is kind of
   "infectious" being propagated once set and never cleared.
   The coarray_comp is only set, when the expression refs a coarray
   component.  REFS_COMP is set when present to true only, when this EXPR
   refs a (non-_data) component.  To check whether EXPR refs an allocatable
   component in a derived type coarray *refs_comp needs to be set and
   coarray_comp has to false.  */

static symbol_attribute
caf_variable_attr (gfc_expr *expr, bool in_allocate, bool *refs_comp)
{
  int dimension, codimension, pointer, allocatable, target, coarray_comp;
  symbol_attribute attr;
  gfc_ref *ref;
  gfc_symbol *sym;
  gfc_component *comp;

  if (expr->expr_type != EXPR_VARIABLE && expr->expr_type != EXPR_FUNCTION)
    gfc_internal_error ("gfc_caf_attr(): Expression isn't a variable");

  sym = expr->symtree->n.sym;
  gfc_clear_attr (&attr);

  if (refs_comp)
    *refs_comp = false;

  if (sym->ts.type == BT_CLASS && sym->attr.class_ok)
    {
      dimension = CLASS_DATA (sym)->attr.dimension;
      codimension = CLASS_DATA (sym)->attr.codimension;
      pointer = CLASS_DATA (sym)->attr.class_pointer;
      allocatable = CLASS_DATA (sym)->attr.allocatable;
      attr.alloc_comp = CLASS_DATA (sym)->ts.u.derived->attr.alloc_comp;
      attr.pointer_comp = CLASS_DATA (sym)->ts.u.derived->attr.pointer_comp;
    }
  else
    {
      dimension = sym->attr.dimension;
      codimension = sym->attr.codimension;
      pointer = sym->attr.pointer;
      allocatable = sym->attr.allocatable;
      attr.alloc_comp = sym->ts.type == BT_DERIVED
	  ? sym->ts.u.derived->attr.alloc_comp : 0;
      attr.pointer_comp = sym->ts.type == BT_DERIVED
	  ? sym->ts.u.derived->attr.pointer_comp : 0;
    }

  target = coarray_comp = 0;
  if (pointer || attr.proc_pointer)
    target = 1;

  for (ref = expr->ref; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:

	switch (ref->u.ar.type)
	  {
	  case AR_FULL:
	  case AR_SECTION:
	    dimension = 1;
	    break;

	  case AR_ELEMENT:
	    /* Handle coarrays.  */
	    if (ref->u.ar.dimen > 0 && !in_allocate)
	      allocatable = pointer = 0;
	    break;

	  case AR_UNKNOWN:
	    /* If any of start, end or stride is not integer, there will
	       already have been an error issued.  */
	    int errors;
	    gfc_get_errors (NULL, &errors);
	    if (errors == 0)
	      gfc_internal_error ("gfc_caf_attr(): Bad array reference");
	  }

	break;

      case REF_COMPONENT:
	comp = ref->u.c.component;

	if (comp->ts.type == BT_CLASS)
	  {
	    /* Set coarray_comp only, when this component introduces the
	       coarray.  */
	    coarray_comp = !codimension && CLASS_DATA (comp)->attr.codimension;
	    codimension |= CLASS_DATA (comp)->attr.codimension;
	    pointer = CLASS_DATA (comp)->attr.class_pointer;
	    allocatable = CLASS_DATA (comp)->attr.allocatable;
	  }
	else
	  {
	    /* Set coarray_comp only, when this component introduces the
	       coarray.  */
	    coarray_comp = !codimension && comp->attr.codimension;
	    codimension |= comp->attr.codimension;
	    pointer = comp->attr.pointer;
	    allocatable = comp->attr.allocatable;
	  }

	if (refs_comp && strcmp (comp->name, "_data") != 0
	    && (ref->next == NULL
		|| (ref->next->type == REF_ARRAY && ref->next->next == NULL)))
	  *refs_comp = true;

	if (pointer || attr.proc_pointer)
	  target = 1;

	break;

      case REF_SUBSTRING:
      case REF_INQUIRY:
	allocatable = pointer = 0;
	break;
      }

  attr.dimension = dimension;
  attr.codimension = codimension;
  attr.pointer = pointer;
  attr.allocatable = allocatable;
  attr.target = target;
  attr.save = sym->attr.save;
  attr.coarray_comp = coarray_comp;

  return attr;
}


symbol_attribute
gfc_caf_attr (gfc_expr *e, bool in_allocate, bool *refs_comp)
{
  symbol_attribute attr;

  switch (e->expr_type)
    {
    case EXPR_VARIABLE:
      attr = caf_variable_attr (e, in_allocate, refs_comp);
      break;

    case EXPR_FUNCTION:
      gfc_clear_attr (&attr);

      if (e->value.function.esym && e->value.function.esym->result)
	{
	  gfc_symbol *sym = e->value.function.esym->result;
	  attr = sym->attr;
	  if (sym->ts.type == BT_CLASS)
	    {
	      attr.dimension = CLASS_DATA (sym)->attr.dimension;
	      attr.pointer = CLASS_DATA (sym)->attr.class_pointer;
	      attr.allocatable = CLASS_DATA (sym)->attr.allocatable;
	      attr.alloc_comp = CLASS_DATA (sym)->ts.u.derived->attr.alloc_comp;
	      attr.pointer_comp = CLASS_DATA (sym)->ts.u.derived
		  ->attr.pointer_comp;
	    }
	}
      else if (e->symtree)
	attr = caf_variable_attr (e, in_allocate, refs_comp);
      else
	gfc_clear_attr (&attr);
      break;

    default:
      gfc_clear_attr (&attr);
      break;
    }

  return attr;
}


/* Match a structure constructor.  The initial symbol has already been
   seen.  */

typedef struct gfc_structure_ctor_component
{
  char* name;
  gfc_expr* val;
  locus where;
  struct gfc_structure_ctor_component* next;
}
gfc_structure_ctor_component;

#define gfc_get_structure_ctor_component() XCNEW (gfc_structure_ctor_component)

static void
gfc_free_structure_ctor_component (gfc_structure_ctor_component *comp)
{
  free (comp->name);
  gfc_free_expr (comp->val);
  free (comp);
}


/* Translate the component list into the actual constructor by sorting it in
   the order required; this also checks along the way that each and every
   component actually has an initializer and handles default initializers
   for components without explicit value given.  */
static bool
build_actual_constructor (gfc_structure_ctor_component **comp_head,
			  gfc_constructor_base *ctor_head, gfc_symbol *sym)
{
  gfc_structure_ctor_component *comp_iter;
  gfc_component *comp;

  for (comp = sym->components; comp; comp = comp->next)
    {
      gfc_structure_ctor_component **next_ptr;
      gfc_expr *value = NULL;

      /* Try to find the initializer for the current component by name.  */
      next_ptr = comp_head;
      for (comp_iter = *comp_head; comp_iter; comp_iter = comp_iter->next)
	{
	  if (!strcmp (comp_iter->name, comp->name))
	    break;
	  next_ptr = &comp_iter->next;
	}

      /* If an extension, try building the parent derived type by building
	 a value expression for the parent derived type and calling self.  */
      if (!comp_iter && comp == sym->components && sym->attr.extension)
	{
	  value = gfc_get_structure_constructor_expr (comp->ts.type,
						      comp->ts.kind,
						      &gfc_current_locus);
	  value->ts = comp->ts;

	  if (!build_actual_constructor (comp_head,
					 &value->value.constructor,
					 comp->ts.u.derived))
	    {
	      gfc_free_expr (value);
	      return false;
	    }

	  gfc_constructor_append_expr (ctor_head, value, NULL);
	  continue;
	}

      /* If it was not found, apply NULL expression to set the component as
	 unallocated. Then try the default initializer if there's any;
	 otherwise, it's an error unless this is a deferred parameter.  */
      if (!comp_iter)
	{
	  /* F2018 7.5.10: If an allocatable component has no corresponding
	     component-data-source, then that component has an allocation
	     status of unallocated....  */
	  if (comp->attr.allocatable
	      || (comp->ts.type == BT_CLASS
		  && CLASS_DATA (comp)->attr.allocatable))
	    {
	      if (!gfc_notify_std (GFC_STD_F2008, "No initializer for "
				   "allocatable component %qs given in the "
				   "structure constructor at %C", comp->name))
		return false;
	      value = gfc_get_null_expr (&gfc_current_locus);
	    }
	  /* ....(Preceding sentence) If a component with default
	     initialization has no corresponding component-data-source, then
	     the default initialization is applied to that component.  */
	  else if (comp->initializer)
	    {
	      if (!gfc_notify_std (GFC_STD_F2003, "Structure constructor "
				   "with missing optional arguments at %C"))
		return false;
	      value = gfc_copy_expr (comp->initializer);
	    }
	  /* Do not trap components such as the string length for deferred
	     length character components.  */
	  else if (!comp->attr.artificial)
	    {
	      gfc_error ("No initializer for component %qs given in the"
			 " structure constructor at %C", comp->name);
	      return false;
	    }
	}
      else
	value = comp_iter->val;

      /* Add the value to the constructor chain built.  */
      gfc_constructor_append_expr (ctor_head, value, NULL);

      /* Remove the entry from the component list.  We don't want the expression
	 value to be free'd, so set it to NULL.  */
      if (comp_iter)
	{
	  *next_ptr = comp_iter->next;
	  comp_iter->val = NULL;
	  gfc_free_structure_ctor_component (comp_iter);
	}
    }
  return true;
}


bool
gfc_convert_to_structure_constructor (gfc_expr *e, gfc_symbol *sym, gfc_expr **cexpr,
				      gfc_actual_arglist **arglist,
				      bool parent)
{
  gfc_actual_arglist *actual;
  gfc_structure_ctor_component *comp_tail, *comp_head, *comp_iter;
  gfc_constructor_base ctor_head = NULL;
  gfc_component *comp; /* Is set NULL when named component is first seen */
  const char* last_name = NULL;
  locus old_locus;
  gfc_expr *expr;

  expr = parent ? *cexpr : e;
  old_locus = gfc_current_locus;
  if (parent)
    ; /* gfc_current_locus = *arglist->expr ? ->where;*/
  else
    gfc_current_locus = expr->where;

  comp_tail = comp_head = NULL;

  if (!parent && sym->attr.abstract)
    {
      gfc_error ("Cannot construct ABSTRACT type %qs at %L",
		 sym->name, &expr->where);
      goto cleanup;
    }

  comp = sym->components;
  actual = parent ? *arglist : expr->value.function.actual;
  for ( ; actual; )
    {
      gfc_component *this_comp = NULL;

      if (!comp_head)
	comp_tail = comp_head = gfc_get_structure_ctor_component ();
      else
	{
	  comp_tail->next = gfc_get_structure_ctor_component ();
	  comp_tail = comp_tail->next;
       	}
      if (actual->name)
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "Structure"
			       " constructor with named arguments at %C"))
	    goto cleanup;

	  comp_tail->name = xstrdup (actual->name);
	  last_name = comp_tail->name;
	  comp = NULL;
	}
      else
	{
	  /* Components without name are not allowed after the first named
	     component initializer!  */
	  if (!comp || comp->attr.artificial)
	    {
	      if (last_name)
		gfc_error ("Component initializer without name after component"
			   " named %s at %L", last_name,
			   actual->expr ? &actual->expr->where
					: &gfc_current_locus);
	      else
		gfc_error ("Too many components in structure constructor at "
			   "%L", actual->expr ? &actual->expr->where
					      : &gfc_current_locus);
	      goto cleanup;
	    }

	  comp_tail->name = xstrdup (comp->name);
	}

      /* Find the current component in the structure definition and check
	     its access is not private.  */
      if (comp)
	this_comp = gfc_find_component (sym, comp->name, false, false, NULL);
      else
	{
	  this_comp = gfc_find_component (sym, (const char *)comp_tail->name,
					  false, false, NULL);
	  comp = NULL; /* Reset needed!  */
	}

      /* Here we can check if a component name is given which does not
	 correspond to any component of the defined structure.  */
      if (!this_comp)
	goto cleanup;

      /* For a constant string constructor, make sure the length is
	 correct; truncate or fill with blanks if needed.  */
      if (this_comp->ts.type == BT_CHARACTER && !this_comp->attr.allocatable
	  && this_comp->ts.u.cl && this_comp->ts.u.cl->length
	  && this_comp->ts.u.cl->length->expr_type == EXPR_CONSTANT
	  && this_comp->ts.u.cl->length->ts.type == BT_INTEGER
	  && actual->expr->ts.type == BT_CHARACTER
	  && actual->expr->expr_type == EXPR_CONSTANT)
	{
	  ptrdiff_t c, e1;
	  c = gfc_mpz_get_hwi (this_comp->ts.u.cl->length->value.integer);
	  e1 = actual->expr->value.character.length;

	  if (c != e1)
	    {
	      ptrdiff_t i, to;
	      gfc_char_t *dest;
	      dest = gfc_get_wide_string (c + 1);

	      to = e1 < c ? e1 : c;
	      for (i = 0; i < to; i++)
		dest[i] = actual->expr->value.character.string[i];

	      for (i = e1; i < c; i++)
		dest[i] = ' ';

	      dest[c] = '\0';
	      free (actual->expr->value.character.string);

	      actual->expr->value.character.length = c;
	      actual->expr->value.character.string = dest;

	      if (warn_line_truncation && c < e1)
		gfc_warning_now (OPT_Wcharacter_truncation,
				 "CHARACTER expression will be truncated "
				 "in constructor (%td/%td) at %L", c,
				 e1, &actual->expr->where);
	    }
	}

      comp_tail->val = actual->expr;
      if (actual->expr != NULL)
	comp_tail->where = actual->expr->where;
      actual->expr = NULL;

      /* Check if this component is already given a value.  */
      for (comp_iter = comp_head; comp_iter != comp_tail;
	   comp_iter = comp_iter->next)
	{
	  gcc_assert (comp_iter);
	  if (!strcmp (comp_iter->name, comp_tail->name))
	    {
	      gfc_error ("Component %qs is initialized twice in the structure"
			 " constructor at %L", comp_tail->name,
			 comp_tail->val ? &comp_tail->where
					: &gfc_current_locus);
	      goto cleanup;
	    }
	}

      /* F2008, R457/C725, for PURE C1283.  */
      if (this_comp->attr.pointer && comp_tail->val
	  && gfc_is_coindexed (comp_tail->val))
     	{
	  gfc_error ("Coindexed expression to pointer component %qs in "
		     "structure constructor at %L", comp_tail->name,
		     &comp_tail->where);
	  goto cleanup;
	}

          /* If not explicitly a parent constructor, gather up the components
             and build one.  */
          if (comp && comp == sym->components
                && sym->attr.extension
		&& comp_tail->val
                && (!gfc_bt_struct (comp_tail->val->ts.type)
                      ||
                    comp_tail->val->ts.u.derived != this_comp->ts.u.derived))
            {
              bool m;
	      gfc_actual_arglist *arg_null = NULL;

	      actual->expr = comp_tail->val;
	      comp_tail->val = NULL;

              m = gfc_convert_to_structure_constructor (NULL,
					comp->ts.u.derived, &comp_tail->val,
					comp->ts.u.derived->attr.zero_comp
					  ? &arg_null : &actual, true);
              if (!m)
                goto cleanup;

	      if (comp->ts.u.derived->attr.zero_comp)
		{
		  comp = comp->next;
		  continue;
		}
            }

      if (comp)
	comp = comp->next;
      if (parent && !comp)
	break;

      if (actual)
	actual = actual->next;
    }

  if (!build_actual_constructor (&comp_head, &ctor_head, sym))
    goto cleanup;

  /* No component should be left, as this should have caused an error in the
     loop constructing the component-list (name that does not correspond to any
     component in the structure definition).  */
  if (comp_head && sym->attr.extension)
    {
      for (comp_iter = comp_head; comp_iter; comp_iter = comp_iter->next)
	{
	  gfc_error ("component %qs at %L has already been set by a "
		     "parent derived type constructor", comp_iter->name,
		     &comp_iter->where);
	}
      goto cleanup;
    }
  else
    gcc_assert (!comp_head);

  if (parent)
    {
      expr = gfc_get_structure_constructor_expr (BT_DERIVED, 0, &gfc_current_locus);
      expr->ts.u.derived = sym;
      expr->value.constructor = ctor_head;
      *cexpr = expr;
    }
  else
    {
      expr->ts.u.derived = sym;
      expr->ts.kind = 0;
      expr->ts.type = BT_DERIVED;
      expr->value.constructor = ctor_head;
      expr->expr_type = EXPR_STRUCTURE;
    }

  gfc_current_locus = old_locus;
  if (parent)
    *arglist = actual;
  return true;

  cleanup:
  gfc_current_locus = old_locus;

  for (comp_iter = comp_head; comp_iter; )
    {
      gfc_structure_ctor_component *next = comp_iter->next;
      gfc_free_structure_ctor_component (comp_iter);
      comp_iter = next;
    }
  gfc_constructor_free (ctor_head);

  return false;
}


match
gfc_match_structure_constructor (gfc_symbol *sym, gfc_expr **result)
{
  match m;
  gfc_expr *e;
  gfc_symtree *symtree;
  bool t = true;

  gfc_get_ha_sym_tree (sym->name, &symtree);

  e = gfc_get_expr ();
  e->symtree = symtree;
  e->expr_type = EXPR_FUNCTION;
  e->where = gfc_current_locus;

  gcc_assert (gfc_fl_struct (sym->attr.flavor)
	      && symtree->n.sym->attr.flavor == FL_PROCEDURE);
  e->value.function.esym = sym;
  e->symtree->n.sym->attr.generic = 1;

  m = gfc_match_actual_arglist (0, &e->value.function.actual);
  if (m != MATCH_YES)
    {
      gfc_free_expr (e);
      return m;
    }

  if (!gfc_convert_to_structure_constructor (e, sym, NULL, NULL, false))
    {
      gfc_free_expr (e);
      return MATCH_ERROR;
    }

  /* If a structure constructor is in a DATA statement, then each entity
     in the structure constructor must be a constant.  Try to reduce the
     expression here.  */
  if (gfc_in_match_data ())
    t = gfc_reduce_init_expr (e);

  if (t)
    {
      *result = e;
      return MATCH_YES;
    }
  else
    {
      gfc_free_expr (e);
      return MATCH_ERROR;
    }
}


/* If the symbol is an implicit do loop index and implicitly typed,
   it should not be host associated.  Provide a symtree from the
   current namespace.  */
static match
check_for_implicit_index (gfc_symtree **st, gfc_symbol **sym)
{
  if ((*sym)->attr.flavor == FL_VARIABLE
      && (*sym)->ns != gfc_current_ns
      && (*sym)->attr.implied_index
      && (*sym)->attr.implicit_type
      && !(*sym)->attr.use_assoc)
    {
      int i;
      i = gfc_get_sym_tree ((*sym)->name, NULL, st, false);
      if (i)
	return MATCH_ERROR;
      *sym = (*st)->n.sym;
    }
  return MATCH_YES;
}


/* Procedure pointer as function result: Replace the function symbol by the
   auto-generated hidden result variable named "ppr@".  */

static bool
replace_hidden_procptr_result (gfc_symbol **sym, gfc_symtree **st)
{
  /* Check for procedure pointer result variable.  */
  if ((*sym)->attr.function && !(*sym)->attr.external
      && (*sym)->result && (*sym)->result != *sym
      && (*sym)->result->attr.proc_pointer
      && (*sym) == gfc_current_ns->proc_name
      && (*sym) == (*sym)->result->ns->proc_name
      && strcmp ("ppr@", (*sym)->result->name) == 0)
    {
      /* Automatic replacement with "hidden" result variable.  */
      (*sym)->result->attr.referenced = (*sym)->attr.referenced;
      *sym = (*sym)->result;
      *st = gfc_find_symtree ((*sym)->ns->sym_root, (*sym)->name);
      return true;
    }
  return false;
}


/* Matches a variable name followed by anything that might follow it--
   array reference, argument list of a function, etc.  */

match
gfc_match_rvalue (gfc_expr **result)
{
  gfc_actual_arglist *actual_arglist;
  char name[GFC_MAX_SYMBOL_LEN + 1], argname[GFC_MAX_SYMBOL_LEN + 1];
  gfc_state_data *st;
  gfc_symbol *sym;
  gfc_symtree *symtree;
  locus where, old_loc;
  gfc_expr *e;
  match m, m2;
  int i;
  gfc_typespec *ts;
  bool implicit_char;
  gfc_ref *ref;

  m = gfc_match ("%%loc");
  if (m == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_LEGACY, "%%LOC() as an rvalue at %C"))
        return MATCH_ERROR;
      strncpy (name, "loc", 4);
    }

  else
    {
      m = gfc_match_name (name);
      if (m != MATCH_YES)
        return m;
    }

  /* Check if the symbol exists.  */
  if (gfc_find_sym_tree (name, NULL, 1, &symtree))
    return MATCH_ERROR;

  /* If the symbol doesn't exist, create it unless the name matches a FL_STRUCT
     type. For derived types we create a generic symbol which links to the
     derived type symbol; STRUCTUREs are simpler and must not conflict with
     variables.  */
  if (!symtree)
    if (gfc_find_sym_tree (gfc_dt_upper_string (name), NULL, 1, &symtree))
      return MATCH_ERROR;
  if (!symtree || symtree->n.sym->attr.flavor != FL_STRUCT)
    {
      if (gfc_find_state (COMP_INTERFACE)
          && !gfc_current_ns->has_import_set)
        i = gfc_get_sym_tree (name, NULL, &symtree, false);
      else
        i = gfc_get_ha_sym_tree (name, &symtree);
      if (i)
        return MATCH_ERROR;
    }


  sym = symtree->n.sym;
  e = NULL;
  where = gfc_current_locus;

  replace_hidden_procptr_result (&sym, &symtree);

  /* If this is an implicit do loop index and implicitly typed,
     it should not be host associated.  */
  m = check_for_implicit_index (&symtree, &sym);
  if (m != MATCH_YES)
    return m;

  gfc_set_sym_referenced (sym);
  sym->attr.implied_index = 0;

  if (sym->attr.function && sym->result == sym)
    {
      /* See if this is a directly recursive function call.  */
      gfc_gobble_whitespace ();
      if (sym->attr.recursive
	  && gfc_peek_ascii_char () == '('
	  && gfc_current_ns->proc_name == sym
	  && !sym->attr.dimension)
	{
	  gfc_error ("%qs at %C is the name of a recursive function "
		     "and so refers to the result variable. Use an "
		     "explicit RESULT variable for direct recursion "
		     "(12.5.2.1)", sym->name);
	  return MATCH_ERROR;
	}

      if (gfc_is_function_return_value (sym, gfc_current_ns))
	goto variable;

      if (sym->attr.entry
	  && (sym->ns == gfc_current_ns
	      || sym->ns == gfc_current_ns->parent))
	{
	  gfc_entry_list *el = NULL;

	  for (el = sym->ns->entries; el; el = el->next)
	    if (sym == el->sym)
	      goto variable;
	}
    }

  if (gfc_matching_procptr_assignment)
    {
      /* It can be a procedure or a derived-type procedure or a not-yet-known
	 type.  */
      if (sym->attr.flavor != FL_UNKNOWN
	  && sym->attr.flavor != FL_PROCEDURE
	  && sym->attr.flavor != FL_PARAMETER
	  && sym->attr.flavor != FL_VARIABLE)
	{
	  gfc_error ("Symbol at %C is not appropriate for an expression");
	  return MATCH_ERROR;
	}
      goto procptr0;
    }

  if (sym->attr.function || sym->attr.external || sym->attr.intrinsic)
    goto function0;

  if (sym->attr.generic)
    goto generic_function;

  switch (sym->attr.flavor)
    {
    case FL_VARIABLE:
    variable:
      e = gfc_get_expr ();

      e->expr_type = EXPR_VARIABLE;
      e->symtree = symtree;

      m = gfc_match_varspec (e, 0, false, true);
      break;

    case FL_PARAMETER:
      /* A statement of the form "REAL, parameter :: a(0:10) = 1" will
	 end up here.  Unfortunately, sym->value->expr_type is set to
	 EXPR_CONSTANT, and so the if () branch would be followed without
	 the !sym->as check.  */
      if (sym->value && sym->value->expr_type != EXPR_ARRAY && !sym->as)
	e = gfc_copy_expr (sym->value);
      else
	{
	  e = gfc_get_expr ();
	  e->expr_type = EXPR_VARIABLE;
	}

      e->symtree = symtree;
      m = gfc_match_varspec (e, 0, false, true);

      if (sym->ts.is_c_interop || sym->ts.is_iso_c)
	break;

      /* Variable array references to derived type parameters cause
	 all sorts of headaches in simplification. Treating such
	 expressions as variable works just fine for all array
	 references.  */
      if (sym->value && sym->ts.type == BT_DERIVED && e->ref)
	{
	  for (ref = e->ref; ref; ref = ref->next)
	    if (ref->type == REF_ARRAY)
	      break;

	  if (ref == NULL || ref->u.ar.type == AR_FULL)
	    break;

	  ref = e->ref;
	  e->ref = NULL;
	  gfc_free_expr (e);
	  e = gfc_get_expr ();
	  e->expr_type = EXPR_VARIABLE;
	  e->symtree = symtree;
	  e->ref = ref;
	}

      break;

    case FL_STRUCT:
    case FL_DERIVED:
      sym = gfc_use_derived (sym);
      if (sym == NULL)
	m = MATCH_ERROR;
      else
	goto generic_function;
      break;

    /* If we're here, then the name is known to be the name of a
       procedure, yet it is not sure to be the name of a function.  */
    case FL_PROCEDURE:

    /* Procedure Pointer Assignments.  */
    procptr0:
      if (gfc_matching_procptr_assignment)
	{
	  gfc_gobble_whitespace ();
	  if (!sym->attr.dimension && gfc_peek_ascii_char () == '(')
	    /* Parse functions returning a procptr.  */
	    goto function0;

	  e = gfc_get_expr ();
	  e->expr_type = EXPR_VARIABLE;
	  e->symtree = symtree;
	  m = gfc_match_varspec (e, 0, false, true);
	  if (!e->ref && sym->attr.flavor == FL_UNKNOWN
	      && sym->ts.type == BT_UNKNOWN
	      && !gfc_add_flavor (&sym->attr, FL_PROCEDURE, sym->name, NULL))
	    {
	      m = MATCH_ERROR;
	      break;
	    }
	  break;
	}

      if (sym->attr.subroutine)
	{
	  gfc_error ("Unexpected use of subroutine name %qs at %C",
		     sym->name);
	  m = MATCH_ERROR;
	  break;
	}

      /* At this point, the name has to be a non-statement function.
	 If the name is the same as the current function being
	 compiled, then we have a variable reference (to the function
	 result) if the name is non-recursive.  */

      st = gfc_enclosing_unit (NULL);

      if (st != NULL
	  && st->state == COMP_FUNCTION
	  && st->sym == sym
	  && !sym->attr.recursive)
	{
	  e = gfc_get_expr ();
	  e->symtree = symtree;
	  e->expr_type = EXPR_VARIABLE;

	  m = gfc_match_varspec (e, 0, false, true);
	  break;
	}

    /* Match a function reference.  */
    function0:
      m = gfc_match_actual_arglist (0, &actual_arglist);
      if (m == MATCH_NO)
	{
	  if (sym->attr.proc == PROC_ST_FUNCTION)
	    gfc_error ("Statement function %qs requires argument list at %C",
		       sym->name);
	  else
	    gfc_error ("Function %qs requires an argument list at %C",
		       sym->name);

	  m = MATCH_ERROR;
	  break;
	}

      if (m != MATCH_YES)
	{
	  m = MATCH_ERROR;
	  break;
	}

      gfc_get_ha_sym_tree (name, &symtree);	/* Can't fail */
      sym = symtree->n.sym;

      replace_hidden_procptr_result (&sym, &symtree);

      e = gfc_get_expr ();
      e->symtree = symtree;
      e->expr_type = EXPR_FUNCTION;
      e->value.function.actual = actual_arglist;
      e->where = gfc_current_locus;

      if (sym->ts.type == BT_CLASS && sym->attr.class_ok
	  && CLASS_DATA (sym)->as)
	e->rank = CLASS_DATA (sym)->as->rank;
      else if (sym->as != NULL)
	e->rank = sym->as->rank;

      if (!sym->attr.function
	  && !gfc_add_function (&sym->attr, sym->name, NULL))
	{
	  m = MATCH_ERROR;
	  break;
	}

      /* Check here for the existence of at least one argument for the
         iso_c_binding functions C_LOC, C_FUNLOC, and C_ASSOCIATED.  The
         argument(s) given will be checked in gfc_iso_c_func_interface,
         during resolution of the function call.  */
      if (sym->attr.is_iso_c == 1
	  && (sym->from_intmod == INTMOD_ISO_C_BINDING
	      && (sym->intmod_sym_id == ISOCBINDING_LOC
		  || sym->intmod_sym_id == ISOCBINDING_FUNLOC
		  || sym->intmod_sym_id == ISOCBINDING_ASSOCIATED)))
        {
          /* make sure we were given a param */
          if (actual_arglist == NULL)
            {
              gfc_error ("Missing argument to %qs at %C", sym->name);
              m = MATCH_ERROR;
              break;
            }
        }

      if (sym->result == NULL)
	sym->result = sym;

      gfc_gobble_whitespace ();
      /* F08:C612.  */
      if (gfc_peek_ascii_char() == '%')
	{
	  gfc_error ("The leftmost part-ref in a data-ref cannot be a "
		     "function reference at %C");
	  m = MATCH_ERROR;
	  break;
	}

      m = MATCH_YES;
      break;

    case FL_UNKNOWN:

      /* Special case for derived type variables that get their types
	 via an IMPLICIT statement.  This can't wait for the
	 resolution phase.  */

      old_loc = gfc_current_locus;
      if (gfc_match_member_sep (sym) == MATCH_YES
	  && sym->ts.type == BT_UNKNOWN
	  && gfc_get_default_type (sym->name, sym->ns)->type == BT_DERIVED)
	gfc_set_default_type (sym, 0, sym->ns);
      gfc_current_locus = old_loc;

      /* If the symbol has a (co)dimension attribute, the expression is a
	 variable.  */

      if (sym->attr.dimension || sym->attr.codimension)
	{
	  if (!gfc_add_flavor (&sym->attr, FL_VARIABLE, sym->name, NULL))
	    {
	      m = MATCH_ERROR;
	      break;
	    }

	  e = gfc_get_expr ();
	  e->symtree = symtree;
	  e->expr_type = EXPR_VARIABLE;
	  m = gfc_match_varspec (e, 0, false, true);
	  break;
	}

      if (sym->ts.type == BT_CLASS && sym->attr.class_ok
	  && (CLASS_DATA (sym)->attr.dimension
	      || CLASS_DATA (sym)->attr.codimension))
	{
	  if (!gfc_add_flavor (&sym->attr, FL_VARIABLE, sym->name, NULL))
	    {
	      m = MATCH_ERROR;
	      break;
	    }

	  e = gfc_get_expr ();
	  e->symtree = symtree;
	  e->expr_type = EXPR_VARIABLE;
	  m = gfc_match_varspec (e, 0, false, true);
	  break;
	}

      /* Name is not an array, so we peek to see if a '(' implies a
	 function call or a substring reference.  Otherwise the
	 variable is just a scalar.  */

      gfc_gobble_whitespace ();
      if (gfc_peek_ascii_char () != '(')
	{
	  /* Assume a scalar variable */
	  e = gfc_get_expr ();
	  e->symtree = symtree;
	  e->expr_type = EXPR_VARIABLE;

	  if (!gfc_add_flavor (&sym->attr, FL_VARIABLE, sym->name, NULL))
	    {
	      m = MATCH_ERROR;
	      break;
	    }

	  /*FIXME:??? gfc_match_varspec does set this for us: */
	  e->ts = sym->ts;
	  m = gfc_match_varspec (e, 0, false, true);
	  break;
	}

      /* See if this is a function reference with a keyword argument
	 as first argument. We do this because otherwise a spurious
	 symbol would end up in the symbol table.  */

      old_loc = gfc_current_locus;
      m2 = gfc_match (" ( %n =", argname);
      gfc_current_locus = old_loc;

      e = gfc_get_expr ();
      e->symtree = symtree;

      if (m2 != MATCH_YES)
	{
	  /* Try to figure out whether we're dealing with a character type.
	     We're peeking ahead here, because we don't want to call
	     match_substring if we're dealing with an implicitly typed
	     non-character variable.  */
	  implicit_char = false;
	  if (sym->ts.type == BT_UNKNOWN)
	    {
	      ts = gfc_get_default_type (sym->name, NULL);
	      if (ts->type == BT_CHARACTER)
		implicit_char = true;
	    }

	  /* See if this could possibly be a substring reference of a name
	     that we're not sure is a variable yet.  */

	  if ((implicit_char || sym->ts.type == BT_CHARACTER)
	      && match_substring (sym->ts.u.cl, 0, &e->ref, false) == MATCH_YES)
	    {

	      e->expr_type = EXPR_VARIABLE;

	      if (sym->attr.flavor != FL_VARIABLE
		  && !gfc_add_flavor (&sym->attr, FL_VARIABLE,
				      sym->name, NULL))
		{
		  m = MATCH_ERROR;
		  break;
		}

	      if (sym->ts.type == BT_UNKNOWN
		  && !gfc_set_default_type (sym, 1, NULL))
		{
		  m = MATCH_ERROR;
		  break;
		}

	      e->ts = sym->ts;
	      if (e->ref)
		e->ts.u.cl = NULL;
	      m = MATCH_YES;
	      break;
	    }
	}

      /* Give up, assume we have a function.  */

      gfc_get_sym_tree (name, NULL, &symtree, false);	/* Can't fail */
      sym = symtree->n.sym;
      e->expr_type = EXPR_FUNCTION;

      if (!sym->attr.function
	  && !gfc_add_function (&sym->attr, sym->name, NULL))
	{
	  m = MATCH_ERROR;
	  break;
	}

      sym->result = sym;

      m = gfc_match_actual_arglist (0, &e->value.function.actual);
      if (m == MATCH_NO)
	gfc_error ("Missing argument list in function %qs at %C", sym->name);

      if (m != MATCH_YES)
	{
	  m = MATCH_ERROR;
	  break;
	}

      /* If our new function returns a character, array or structure
	 type, it might have subsequent references.  */

      m = gfc_match_varspec (e, 0, false, true);
      if (m == MATCH_NO)
	m = MATCH_YES;

      break;

    generic_function:
      /* Look for symbol first; if not found, look for STRUCTURE type symbol
         specially. Creates a generic symbol for derived types.  */
      gfc_find_sym_tree (name, NULL, 1, &symtree);
      if (!symtree)
        gfc_find_sym_tree (gfc_dt_upper_string (name), NULL, 1, &symtree);
      if (!symtree || symtree->n.sym->attr.flavor != FL_STRUCT)
        gfc_get_sym_tree (name, NULL, &symtree, false); /* Can't fail */

      e = gfc_get_expr ();
      e->symtree = symtree;
      e->expr_type = EXPR_FUNCTION;

      if (gfc_fl_struct (sym->attr.flavor))
	{
	  e->value.function.esym = sym;
	  e->symtree->n.sym->attr.generic = 1;
	}

      m = gfc_match_actual_arglist (0, &e->value.function.actual);
      break;

    case FL_NAMELIST:
      m = MATCH_ERROR;
      break;

    default:
      gfc_error ("Symbol at %C is not appropriate for an expression");
      return MATCH_ERROR;
    }

  if (m == MATCH_YES)
    {
      e->where = where;
      *result = e;
    }
  else
    gfc_free_expr (e);

  return m;
}


/* Match a variable, i.e. something that can be assigned to.  This
   starts as a symbol, can be a structure component or an array
   reference.  It can be a function if the function doesn't have a
   separate RESULT variable.  If the symbol has not been previously
   seen, we assume it is a variable.

   This function is called by two interface functions:
   gfc_match_variable, which has host_flag = 1, and
   gfc_match_equiv_variable, with host_flag = 0, to restrict the
   match of the symbol to the local scope.  */

static match
match_variable (gfc_expr **result, int equiv_flag, int host_flag)
{
  gfc_symbol *sym, *dt_sym;
  gfc_symtree *st;
  gfc_expr *expr;
  locus where, old_loc;
  match m;

  /* Since nothing has any business being an lvalue in a module
     specification block, an interface block or a contains section,
     we force the changed_symbols mechanism to work by setting
     host_flag to 0. This prevents valid symbols that have the name
     of keywords, such as 'end', being turned into variables by
     failed matching to assignments for, e.g., END INTERFACE.  */
  if (gfc_current_state () == COMP_MODULE
      || gfc_current_state () == COMP_SUBMODULE
      || gfc_current_state () == COMP_INTERFACE
      || gfc_current_state () == COMP_CONTAINS)
    host_flag = 0;

  where = gfc_current_locus;
  m = gfc_match_sym_tree (&st, host_flag);
  if (m != MATCH_YES)
    return m;

  sym = st->n.sym;

  /* If this is an implicit do loop index and implicitly typed,
     it should not be host associated.  */
  m = check_for_implicit_index (&st, &sym);
  if (m != MATCH_YES)
    return m;

  sym->attr.implied_index = 0;

  gfc_set_sym_referenced (sym);

  /* STRUCTUREs may share names with variables, but derived types may not.  */
  if (sym->attr.flavor == FL_PROCEDURE && sym->generic
      && (dt_sym = gfc_find_dt_in_generic (sym)))
    {
      if (dt_sym->attr.flavor == FL_DERIVED)
        gfc_error ("Derived type %qs cannot be used as a variable at %C",
                   sym->name);
      return MATCH_ERROR;
    }

  switch (sym->attr.flavor)
    {
    case FL_VARIABLE:
      /* Everything is alright.  */
      break;

    case FL_UNKNOWN:
      {
	sym_flavor flavor = FL_UNKNOWN;

	gfc_gobble_whitespace ();

	if (sym->attr.external || sym->attr.procedure
	    || sym->attr.function || sym->attr.subroutine)
	  flavor = FL_PROCEDURE;

	/* If it is not a procedure, is not typed and is host associated,
	   we cannot give it a flavor yet.  */
	else if (sym->ns == gfc_current_ns->parent
		   && sym->ts.type == BT_UNKNOWN)
	  break;

	/* These are definitive indicators that this is a variable.  */
	else if (gfc_peek_ascii_char () != '(' || sym->ts.type != BT_UNKNOWN
		 || sym->attr.pointer || sym->as != NULL)
	  flavor = FL_VARIABLE;

	if (flavor != FL_UNKNOWN
	    && !gfc_add_flavor (&sym->attr, flavor, sym->name, NULL))
	  return MATCH_ERROR;
      }
      break;

    case FL_PARAMETER:
      if (equiv_flag)
	{
	  gfc_error ("Named constant at %C in an EQUIVALENCE");
	  return MATCH_ERROR;
	}
      if (gfc_in_match_data())
	{
	  gfc_error ("PARAMETER %qs shall not appear in a DATA statement at %C",
		      sym->name);
	  return MATCH_ERROR;
	}
	/* Otherwise this is checked for an error given in the
	   variable definition context checks.  */
      break;

    case FL_PROCEDURE:
      /* Check for a nonrecursive function result variable.  */
      if (sym->attr.function
	  && !sym->attr.external
	  && sym->result == sym
	  && (gfc_is_function_return_value (sym, gfc_current_ns)
	      || (sym->attr.entry
		  && sym->ns == gfc_current_ns)
	      || (sym->attr.entry
		  && sym->ns == gfc_current_ns->parent)))
	{
	  /* If a function result is a derived type, then the derived
	     type may still have to be resolved.  */

	  if (sym->ts.type == BT_DERIVED
	      && gfc_use_derived (sym->ts.u.derived) == NULL)
	    return MATCH_ERROR;
	  break;
	}

      if (sym->attr.proc_pointer
	  || replace_hidden_procptr_result (&sym, &st))
	break;

      /* Fall through to error */
      gcc_fallthrough ();

    default:
      gfc_error ("%qs at %C is not a variable", sym->name);
      return MATCH_ERROR;
    }

  /* Special case for derived type variables that get their types
     via an IMPLICIT statement.  This can't wait for the
     resolution phase.  */

    {
      gfc_namespace * implicit_ns;

      if (gfc_current_ns->proc_name == sym)
	implicit_ns = gfc_current_ns;
      else
	implicit_ns = sym->ns;

      old_loc = gfc_current_locus;
      if (gfc_match_member_sep (sym) == MATCH_YES
	  && sym->ts.type == BT_UNKNOWN
	  && gfc_get_default_type (sym->name, implicit_ns)->type == BT_DERIVED)
	gfc_set_default_type (sym, 0, implicit_ns);
      gfc_current_locus = old_loc;
    }

  expr = gfc_get_expr ();

  expr->expr_type = EXPR_VARIABLE;
  expr->symtree = st;
  expr->ts = sym->ts;
  expr->where = where;

  /* Now see if we have to do more.  */
  m = gfc_match_varspec (expr, equiv_flag, false, false);
  if (m != MATCH_YES)
    {
      gfc_free_expr (expr);
      return m;
    }

  *result = expr;
  return MATCH_YES;
}


match
gfc_match_variable (gfc_expr **result, int equiv_flag)
{
  return match_variable (result, equiv_flag, 1);
}


match
gfc_match_equiv_variable (gfc_expr **result)
{
  return match_variable (result, 1, 0);
}

