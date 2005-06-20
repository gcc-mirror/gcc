/* Primary expression subroutines
   Copyright (C) 2000, 2001, 2002, 2004, 2005 Free Software Foundation,
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "flags.h"
#include "gfortran.h"
#include "arith.h"
#include "match.h"
#include "parse.h"

/* Matches a kind-parameter expression, which is either a named
   symbolic constant or a nonnegative integer constant.  If
   successful, sets the kind value to the correct integer.  */

static match
match_kind_param (int *kind)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  const char *p;
  match m;

  m = gfc_match_small_literal_int (kind);
  if (m != MATCH_NO)
    return m;

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (gfc_find_symbol (name, NULL, 1, &sym))
    return MATCH_ERROR;

  if (sym == NULL)
    return MATCH_NO;

  if (sym->attr.flavor != FL_PARAMETER)
    return MATCH_NO;

  p = gfc_extract_int (sym->value, kind);
  if (p != NULL)
    return MATCH_NO;

  if (*kind < 0)
    return MATCH_NO;

  return MATCH_YES;
}


/* Get a trailing kind-specification for non-character variables.
   Returns:
      the integer kind value or:
      -1 if an error was generated
      -2 if no kind was found */

static int
get_kind (void)
{
  int kind;
  match m;

  if (gfc_match_char ('_') != MATCH_YES)
    return -2;

  m = match_kind_param (&kind);
  if (m == MATCH_NO)
    gfc_error ("Missing kind-parameter at %C");

  return (m == MATCH_YES) ? kind : -1;
}


/* Given a character and a radix, see if the character is a valid
   digit in that radix.  */

static int
check_digit (int c, int radix)
{
  int r;

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
      gfc_internal_error ("check_digit(): bad radix");
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
  int length, c;

  length = 0;
  c = gfc_next_char ();

  if (signflag && (c == '+' || c == '-'))
    {
      if (buffer != NULL)
	*buffer++ = c;
      gfc_gobble_whitespace ();
      c = gfc_next_char ();
      length++;
    }

  if (!check_digit (c, radix))
    return -1;

  length++;
  if (buffer != NULL)
    *buffer++ = c;

  for (;;)
    {
      old_loc = gfc_current_locus;
      c = gfc_next_char ();

      if (!check_digit (c, radix))
	break;

      if (buffer != NULL)
	*buffer++ = c;
      length++;
    }

  gfc_current_locus = old_loc;

  return length;
}


/* Match an integer (digit string and optional kind).  
   A sign will be accepted if signflag is set.  */

static match
match_integer_constant (gfc_expr ** result, int signflag)
{
  int length, kind;
  locus old_loc;
  char *buffer;
  gfc_expr *e;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  length = match_digits (signflag, 10, NULL);
  gfc_current_locus = old_loc;
  if (length == -1)
    return MATCH_NO;

  buffer = alloca (length + 1);
  memset (buffer, '\0', length + 1);

  gfc_gobble_whitespace ();

  match_digits (signflag, 10, buffer);

  kind = get_kind ();
  if (kind == -2)
    kind = gfc_default_integer_kind;
  if (kind == -1)
    return MATCH_ERROR;

  if (gfc_validate_kind (BT_INTEGER, kind, true) < 0)
    {
      gfc_error ("Integer kind %d at %C not available", kind);
      return MATCH_ERROR;
    }

  e = gfc_convert_integer (buffer, kind, 10, &gfc_current_locus);

  if (gfc_range_check (e) != ARITH_OK)
    {
      gfc_error ("Integer too big for its kind at %C");

      gfc_free_expr (e);
      return MATCH_ERROR;
    }

  *result = e;
  return MATCH_YES;
}


/* Match a binary, octal or hexadecimal constant that can be found in
   a DATA statement.  */

static match
match_boz_constant (gfc_expr ** result)
{
  int radix, delim, length, x_hex, kind;
  locus old_loc;
  char *buffer;
  gfc_expr *e;
  const char *rname;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  x_hex = 0;
  switch (gfc_next_char ())
    {
    case 'b':
      radix = 2;
      rname = "binary";
      break;
    case 'o':
      radix = 8;
      rname = "octal";
      break;
    case 'x':
      x_hex = 1;
      /* Fall through.  */
    case 'z':
      radix = 16;
      rname = "hexadecimal";
      break;
    default:
      goto backup;
    }

  /* No whitespace allowed here.  */

  delim = gfc_next_char ();
  if (delim != '\'' && delim != '\"')
    goto backup;

  if (x_hex && pedantic
      && (gfc_notify_std (GFC_STD_GNU, "Extension: Hexadecimal "
			  "constant at %C uses non-standard syntax.")
	  == FAILURE))
      return MATCH_ERROR;

  old_loc = gfc_current_locus;

  length = match_digits (0, radix, NULL);
  if (length == -1)
    {
      gfc_error ("Empty set of digits in %s constants at %C", rname);
      return MATCH_ERROR;
    }

  if (gfc_next_char () != delim)
    {
      gfc_error ("Illegal character in %s constant at %C.", rname);
      return MATCH_ERROR;
    }

  gfc_current_locus = old_loc;

  buffer = alloca (length + 1);
  memset (buffer, '\0', length + 1);

  match_digits (0, radix, buffer);
  gfc_next_char ();  /* Eat delimiter.  */


  /* In section 5.2.5 and following C567 in the Fortran 2003 standard, we find
     "If a data-stmt-constant is a boz-literal-constant, the corresponding
     variable shall be of type integer.  The boz-literal-constant is treated
     as if it were an int-literal-constant with a kind-param that specifies
     the representation method with the largest decimal exponent range
     supported by the processor."  */

  kind = gfc_max_integer_kind;
  e = gfc_convert_integer (buffer, kind, radix, &gfc_current_locus);

  if (gfc_range_check (e) != ARITH_OK)
    {
      gfc_error ("Integer too big for integer kind %i at %C", kind);

      gfc_free_expr (e);
      return MATCH_ERROR;
    }

  *result = e;
  return MATCH_YES;

backup:
  gfc_current_locus = old_loc;
  return MATCH_NO;
}


/* Match a real constant of some sort.  Allow a signed constant if signflag
   is nonzero.  Allow integer constants if allow_int is true.  */

static match
match_real_constant (gfc_expr ** result, int signflag)
{
  int kind, c, count, seen_dp, seen_digits, exp_char;
  locus old_loc, temp_loc;
  char *p, *buffer;
  gfc_expr *e;
  bool negate;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  e = NULL;

  count = 0;
  seen_dp = 0;
  seen_digits = 0;
  exp_char = ' ';
  negate = FALSE;

  c = gfc_next_char ();
  if (signflag && (c == '+' || c == '-'))
    {
      if (c == '-')
	negate = TRUE;

      gfc_gobble_whitespace ();
      c = gfc_next_char ();
    }

  /* Scan significand.  */
  for (;; c = gfc_next_char (), count++)
    {
      if (c == '.')
	{
	  if (seen_dp)
	    goto done;

	  /* Check to see if "." goes with a following operator like ".eq.".  */
	  temp_loc = gfc_current_locus;
	  c = gfc_next_char ();

	  if (c == 'e' || c == 'd' || c == 'q')
	    {
	      c = gfc_next_char ();
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

  if (!seen_digits
      || (c != 'e' && c != 'd' && c != 'q'))
    goto done;
  exp_char = c;

  /* Scan exponent.  */
  c = gfc_next_char ();
  count++;

  if (c == '+' || c == '-')
    {				/* optional sign */
      c = gfc_next_char ();
      count++;
    }

  if (!ISDIGIT (c))
    {
      gfc_error ("Missing exponent in real number at %C");
      return MATCH_ERROR;
    }

  while (ISDIGIT (c))
    {
      c = gfc_next_char ();
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

  buffer = alloca (count + 1);
  memset (buffer, '\0', count + 1);

  p = buffer;
  c = gfc_next_char ();
  if (c == '+' || c == '-')
    {
      gfc_gobble_whitespace ();
      c = gfc_next_char ();
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

      c = gfc_next_char ();
    }

  kind = get_kind ();
  if (kind == -1)
    goto cleanup;

  switch (exp_char)
    {
    case 'd':
      if (kind != -2)
	{
	  gfc_error
	    ("Real number at %C has a 'd' exponent and an explicit kind");
	  goto cleanup;
	}
      kind = gfc_default_double_kind;
      break;

    case 'q':
      if (kind != -2)
	{
	  gfc_error
	    ("Real number at %C has a 'q' exponent and an explicit kind");
	  goto cleanup;
	}
      kind = gfc_option.q_kind;
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

  e = gfc_convert_real (buffer, kind, &gfc_current_locus);
  if (negate)
    mpfr_neg (e->value.real, e->value.real, GFC_RND_MODE);

  switch (gfc_range_check (e))
    {
    case ARITH_OK:
      break;
    case ARITH_OVERFLOW:
      gfc_error ("Real constant overflows its kind at %C");
      goto cleanup;

    case ARITH_UNDERFLOW:
      if (gfc_option.warn_underflow)
        gfc_warning ("Real constant underflows its kind at %C");
      mpfr_set_ui (e->value.real, 0, GFC_RND_MODE);
      break;

    default:
      gfc_internal_error ("gfc_range_check() returned bad value");
    }

  *result = e;
  return MATCH_YES;

cleanup:
  gfc_free_expr (e);
  return MATCH_ERROR;
}


/* Match a substring reference.  */

static match
match_substring (gfc_charlen * cl, int init, gfc_ref ** result)
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
  if (start == NULL && end == NULL)
    ref = NULL;
  else
    {
      ref = gfc_get_ref ();

      ref->type = REF_SUBSTRING;
      if (start == NULL)
	start = gfc_int_expr (1);
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

   Special return values are:
     -1   End of the string, as determined by the delimiter
     -2   Unterminated string detected

   Backslash codes are also expanded at this time.  */

static int
next_string_char (char delimiter)
{
  locus old_locus;
  int c;

  c = gfc_next_char_literal (1);

  if (c == '\n')
    return -2;

  if (gfc_option.flag_backslash && c == '\\')
    {
      old_locus = gfc_current_locus;

      switch (gfc_next_char_literal (1))
	{
	case 'a':
	  c = '\a';
	  break;
	case 'b':
	  c = '\b';
	  break;
	case 't':
	  c = '\t';
	  break;
	case 'f':
	  c = '\f';
	  break;
	case 'n':
	  c = '\n';
	  break;
	case 'r':
	  c = '\r';
	  break;
	case 'v':
	  c = '\v';
	  break;
	case '\\':
	  c = '\\';
	  break;

	default:
	  /* Unknown backslash codes are simply not expanded */
	  gfc_current_locus = old_locus;
	  break;
	}
    }

  if (c != delimiter)
    return c;

  old_locus = gfc_current_locus;
  c = gfc_next_char_literal (1);

  if (c == delimiter)
    return c;
  gfc_current_locus = old_locus;

  return -1;
}


/* Special case of gfc_match_name() that matches a parameter kind name
   before a string constant.  This takes case of the weird but legal
   case of: weird case of:

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
  c = gfc_next_char ();
  if (!ISALPHA (c))
    return MATCH_NO;

  *name++ = c;
  len = 1;

  for (;;)
    {
      old_loc = gfc_current_locus;
      c = gfc_next_char ();

      if (c == '_')
	{
	  peek = gfc_peek_char ();

	  if (peek == '\'' || peek == '\"')
	    {
	      gfc_current_locus = old_loc;
	      *name = '\0';
	      return MATCH_YES;
	    }
	}

      if (!ISALNUM (c)
	  && c != '_'
	  && (gfc_option.flag_dollar_ok && c != '$'))
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
match_string_constant (gfc_expr ** result)
{
  char *p, name[GFC_MAX_SYMBOL_LEN + 1];
  int i, c, kind, length, delimiter;
  locus old_locus, start_locus;
  gfc_symbol *sym;
  gfc_expr *e;
  const char *q;
  match m;

  old_locus = gfc_current_locus;

  gfc_gobble_whitespace ();

  start_locus = gfc_current_locus;

  c = gfc_next_char ();
  if (c == '\'' || c == '"')
    {
      kind = gfc_default_character_kind;
      goto got_delim;
    }

  if (ISDIGIT (c))
    {
      kind = 0;

      while (ISDIGIT (c))
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

  if (c == ' ')
    {
      gfc_gobble_whitespace ();
      c = gfc_next_char ();
    }

  if (c != '_')
    goto no_match;

  gfc_gobble_whitespace ();
  start_locus = gfc_current_locus;

  c = gfc_next_char ();
  if (c != '\'' && c != '"')
    goto no_match;

  if (kind == -1)
    {
      q = gfc_extract_int (sym->value, &kind);
      if (q != NULL)
	{
	  gfc_error (q);
	  return MATCH_ERROR;
	}
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
      c = next_string_char (delimiter);
      if (c == -1)
	break;
      if (c == -2)
	{
	  gfc_current_locus = start_locus;
	  gfc_error ("Unterminated character constant beginning at %C");
	  return MATCH_ERROR;
	}

      length++;
    }

  e = gfc_get_expr ();

  e->expr_type = EXPR_CONSTANT;
  e->ref = NULL;
  e->ts.type = BT_CHARACTER;
  e->ts.kind = kind;
  e->where = start_locus;

  e->value.character.string = p = gfc_getmem (length + 1);
  e->value.character.length = length;

  gfc_current_locus = start_locus;
  gfc_next_char ();		/* Skip delimiter */

  for (i = 0; i < length; i++)
    *p++ = next_string_char (delimiter);

  *p = '\0';	/* TODO: C-style string is for development/debug purposes.  */

  if (next_string_char (delimiter) != -1)
    gfc_internal_error ("match_string_constant(): Delimiter not found");

  if (match_substring (NULL, 0, &e->ref) != MATCH_NO)
    e->expr_type = EXPR_SUBSTRING;

  *result = e;

  return MATCH_YES;

no_match:
  gfc_current_locus = old_locus;
  return MATCH_NO;
}


/* Match a .true. or .false.  */

static match
match_logical_constant (gfc_expr ** result)
{
  static mstring logical_ops[] = {
    minit (".false.", 0),
    minit (".true.", 1),
    minit (NULL, -1)
  };

  gfc_expr *e;
  int i, kind;

  i = gfc_match_strings (logical_ops);
  if (i == -1)
    return MATCH_NO;

  kind = get_kind ();
  if (kind == -1)
    return MATCH_ERROR;
  if (kind == -2)
    kind = gfc_default_logical_kind;

  if (gfc_validate_kind (BT_LOGICAL, kind, true) < 0)
    gfc_error ("Bad kind for logical constant at %C");

  e = gfc_get_expr ();

  e->expr_type = EXPR_CONSTANT;
  e->value.logical = i;
  e->ts.type = BT_LOGICAL;
  e->ts.kind = kind;
  e->where = gfc_current_locus;

  *result = e;
  return MATCH_YES;
}


/* Match a real or imaginary part of a complex constant that is a
   symbolic constant.  */

static match
match_sym_complex_part (gfc_expr ** result)
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
      gfc_error ("Expected PARAMETER symbol in complex constant at %C");
      return MATCH_ERROR;
    }

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

  *result = e;			/* e is a scalar, real, constant expression */
  return MATCH_YES;

error:
  gfc_error ("Error converting PARAMETER constant in complex constant at %C");
  return MATCH_ERROR;
}


/* Match a real or imaginary part of a complex number.  */

static match
match_complex_part (gfc_expr ** result)
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
match_complex_constant (gfc_expr ** result)
{
  gfc_expr *e, *real, *imag;
  gfc_error_buf old_error;
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
    goto cleanup;

  if (gfc_match_char (',') == MATCH_NO)
    {
      gfc_pop_error (&old_error);
      m = MATCH_NO;
      goto cleanup;
    }

  /* If m is error, then something was wrong with the real part and we
     assume we have a complex constant because we've seen the ','.  An
     ambiguous case here is the start of an iterator list of some
     sort. These sort of lists are matched prior to coming here.  */

  if (m == MATCH_ERROR)
    goto cleanup;
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
      if (gfc_peek_char () == '=')
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
  target.type = BT_REAL;
  target.kind = kind;

  if (real->ts.type != BT_REAL || kind != real->ts.kind)
    gfc_convert_type (real, &target, 2);
  if (imag->ts.type != BT_REAL || kind != imag->ts.kind)
    gfc_convert_type (imag, &target, 2);

  e = gfc_convert_complex (real, imag, kind);
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
gfc_match_literal_constant (gfc_expr ** result, int signflag)
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

  m = match_integer_constant (result, signflag);
  if (m != MATCH_NO)
    return m;

  m = match_logical_constant (result);
  if (m != MATCH_NO)
    return m;

  return MATCH_NO;
}


/* Match a single actual argument value.  An actual argument is
   usually an expression, but can also be a procedure name.  If the
   argument is a single name, it is not always possible to tell
   whether the name is a dummy procedure or not.  We treat these cases
   by creating an argument that looks like a dummy procedure and
   fixing things later during resolution.  */

static match
match_actual_arg (gfc_expr ** result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symtree *symtree;
  locus where, w;
  gfc_expr *e;
  int c;

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
      c = gfc_next_char ();
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
	  gfc_get_sym_tree (name, NULL, &symtree);
          gfc_set_sym_referenced (symtree->n.sym);
        }
      else
	{
          gfc_symbol *sym;

          sym = symtree->n.sym;
          gfc_set_sym_referenced (sym);
	  if (sym->attr.flavor != FL_PROCEDURE
	      && sym->attr.flavor != FL_UNKNOWN)
	    break;

	  /* If the symbol is a function with itself as the result and
	     is being defined, then we have a variable.  */
	  if (sym->result == sym
	      && (gfc_current_ns->proc_name == sym
		  || (gfc_current_ns->parent != NULL
		      && gfc_current_ns->parent->proc_name == sym)))
	    break;
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


/* Match a keyword argument.  */

static match
match_keyword_arg (gfc_actual_arglist * actual, gfc_actual_arglist * base)
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

  m = match_actual_arg (&actual->expr);
  if (m != MATCH_YES)
    goto cleanup;

  /* Make sure this name has not appeared yet.  */

  if (name[0] != '\0')
    {
      for (a = base; a; a = a->next)
	if (a->name != NULL && strcmp (a->name, name) == 0)
	  {
	    gfc_error
	      ("Keyword '%s' at %C has already appeared in the current "
	       "argument list", name);
	    return MATCH_ERROR;
	  }
    }

  actual->name = gfc_get_string (name);
  return MATCH_YES;

cleanup:
  gfc_current_locus = name_locus;
  return m;
}


/* Matches an actual argument list of a function or subroutine, from
   the opening parenthesis to the closing parenthesis.  The argument
   list is assumed to allow keyword arguments because we don't know if
   the symbol associated with the procedure has an implicit interface
   or not.  We make sure keywords are unique. If SUB_FLAG is set,
   we're matching the argument list of a subroutine.  */

match
gfc_match_actual_arglist (int sub_flag, gfc_actual_arglist ** argp)
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

  for (;;)
    {
      if (head == NULL)
	head = tail = gfc_get_actual_arglist ();
      else
	{
	  tail->next = gfc_get_actual_arglist ();
	  tail = tail->next;
	}

      if (sub_flag && gfc_match_char ('*') == MATCH_YES)
	{
	  m = gfc_match_st_label (&label, 0);
	  if (m == MATCH_NO)
	    gfc_error ("Expected alternate return label at %C");
	  if (m != MATCH_YES)
	    goto cleanup;

	  tail->label = label;
	  goto next;
	}

      /* After the first keyword argument is seen, the following
         arguments must also have keywords.  */
      if (seen_keyword)
	{
	  m = match_keyword_arg (tail, head);

	  if (m == MATCH_ERROR)
	    goto cleanup;
	  if (m == MATCH_NO)
	    {
	      gfc_error
		("Missing keyword name in actual argument list at %C");
	      goto cleanup;
	    }

	}
      else
	{
	  /* See if we have the first keyword argument.  */
	  m = match_keyword_arg (tail, head);
	  if (m == MATCH_YES)
	    seen_keyword = 1;
	  if (m == MATCH_ERROR)
	    goto cleanup;

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
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in argument list at %C");

cleanup:
  gfc_free_actual_arglist (head);
  gfc_current_locus = old_loc;

  return MATCH_ERROR;
}


/* Used by match_varspec() to extend the reference list by one
   element.  */

static gfc_ref *
extend_ref (gfc_expr * primary, gfc_ref * tail)
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


/* Match any additional specifications associated with the current
   variable like member references or substrings.  If equiv_flag is
   set we only match stuff that is allowed inside an EQUIVALENCE
   statement.  */

static match
match_varspec (gfc_expr * primary, int equiv_flag)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_ref *substring, *tail;
  gfc_component *component;
  gfc_symbol *sym;
  match m;

  tail = NULL;

  if (primary->symtree->n.sym->attr.dimension
      || (equiv_flag
	  && gfc_peek_char () == '('))
    {

      tail = extend_ref (primary, tail);
      tail->type = REF_ARRAY;

      m = gfc_match_array_ref (&tail->u.ar, primary->symtree->n.sym->as,
                               equiv_flag);
      if (m != MATCH_YES)
	return m;
    }

  sym = primary->symtree->n.sym;
  primary->ts = sym->ts;

  if (sym->ts.type != BT_DERIVED || gfc_match_char ('%') != MATCH_YES)
    goto check_substring;

  sym = sym->ts.derived;

  for (;;)
    {
      m = gfc_match_name (name);
      if (m == MATCH_NO)
	gfc_error ("Expected structure component name at %C");
      if (m != MATCH_YES)
	return MATCH_ERROR;

      component = gfc_find_component (sym, name);
      if (component == NULL)
	return MATCH_ERROR;

      tail = extend_ref (primary, tail);
      tail->type = REF_COMPONENT;

      tail->u.c.component = component;
      tail->u.c.sym = sym;

      primary->ts = component->ts;

      if (component->as != NULL)
	{
	  tail = extend_ref (primary, tail);
	  tail->type = REF_ARRAY;

	  m = gfc_match_array_ref (&tail->u.ar, component->as, equiv_flag);
	  if (m != MATCH_YES)
	    return m;
	}

      if (component->ts.type != BT_DERIVED
	  || gfc_match_char ('%') != MATCH_YES)
	break;

      sym = component->ts.derived;
    }

check_substring:
  if (primary->ts.type == BT_CHARACTER)
    {
      switch (match_substring (primary->ts.cl, equiv_flag, &substring))
	{
	case MATCH_YES:
	  if (tail == NULL)
	    primary->ref = substring;
	  else
	    tail->next = substring;

	  if (primary->expr_type == EXPR_CONSTANT)
	    primary->expr_type = EXPR_SUBSTRING;

	  if (substring)
	    primary->ts.cl = NULL;

	  break;

	case MATCH_NO:
	  break;

	case MATCH_ERROR:
	  return MATCH_ERROR;
	}
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
gfc_variable_attr (gfc_expr * expr, gfc_typespec * ts)
{
  int dimension, pointer, target;
  symbol_attribute attr;
  gfc_ref *ref;

  if (expr->expr_type != EXPR_VARIABLE)
    gfc_internal_error ("gfc_variable_attr(): Expression isn't a variable");

  ref = expr->ref;
  attr = expr->symtree->n.sym->attr;

  dimension = attr.dimension;
  pointer = attr.pointer;

  target = attr.target;
  if (pointer)
    target = 1;

  if (ts != NULL && expr->ts.type == BT_UNKNOWN)
    *ts = expr->symtree->n.sym->ts;

  for (; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:

	switch (ref->u.ar.type)
	  {
	  case AR_FULL:
	    dimension = 1;
	    break;

	  case AR_SECTION:
	    pointer = 0;
	    dimension = 1;
	    break;

	  case AR_ELEMENT:
	    pointer = 0;
	    break;

	  case AR_UNKNOWN:
	    gfc_internal_error ("gfc_variable_attr(): Bad array reference");
	  }

	break;

      case REF_COMPONENT:
	gfc_get_component_attr (&attr, ref->u.c.component);
	if (ts != NULL)
	  *ts = ref->u.c.component->ts;

	pointer = ref->u.c.component->pointer;
	if (pointer)
	  target = 1;

	break;

      case REF_SUBSTRING:
	pointer = 0;
	break;
      }

  attr.dimension = dimension;
  attr.pointer = pointer;
  attr.target = target;

  return attr;
}


/* Return the attribute from a general expression.  */

symbol_attribute
gfc_expr_attr (gfc_expr * e)
{
  symbol_attribute attr;

  switch (e->expr_type)
    {
    case EXPR_VARIABLE:
      attr = gfc_variable_attr (e, NULL);
      break;

    case EXPR_FUNCTION:
      gfc_clear_attr (&attr);

      if (e->value.function.esym != NULL)
	attr = e->value.function.esym->result->attr;

      /* TODO: NULL() returns pointers.  May have to take care of this
         here.  */

      break;

    default:
      gfc_clear_attr (&attr);
      break;
    }

  return attr;
}


/* Match a structure constructor.  The initial symbol has already been
   seen.  */

match
gfc_match_structure_constructor (gfc_symbol * sym, gfc_expr ** result)
{
  gfc_constructor *head, *tail;
  gfc_component *comp;
  gfc_expr *e;
  locus where;
  match m;

  head = tail = NULL;

  if (gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  where = gfc_current_locus;

  gfc_find_component (sym, NULL);

  for (comp = sym->components; comp; comp = comp->next)
    {
      if (head == NULL)
	tail = head = gfc_get_constructor ();
      else
	{
	  tail->next = gfc_get_constructor ();
	  tail = tail->next;
	}

      m = gfc_match_expr (&tail->expr);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      if (gfc_match_char (',') == MATCH_YES)
	{
	  if (comp->next == NULL)
	    {
	      gfc_error
		("Too many components in structure constructor at %C");
	      goto cleanup;
	    }

	  continue;
	}

      break;
    }

  if (gfc_match_char (')') != MATCH_YES)
    goto syntax;

  if (comp->next != NULL)
    {
      gfc_error ("Too few components in structure constructor at %C");
      goto cleanup;
    }

  e = gfc_get_expr ();

  e->expr_type = EXPR_STRUCTURE;

  e->ts.type = BT_DERIVED;
  e->ts.derived = sym;
  e->where = where;

  e->value.constructor = head;

  *result = e;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in structure constructor at %C");

cleanup:
  gfc_free_constructor (head);
  return MATCH_ERROR;
}


/* Matches a variable name followed by anything that might follow it--
   array reference, argument list of a function, etc.  */

match
gfc_match_rvalue (gfc_expr ** result)
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

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (gfc_find_state (COMP_INTERFACE) == SUCCESS)
    i = gfc_get_sym_tree (name, NULL, &symtree);
  else
    i = gfc_get_ha_sym_tree (name, &symtree);

  if (i)
    return MATCH_ERROR;

  sym = symtree->n.sym;
  e = NULL;
  where = gfc_current_locus;

  gfc_set_sym_referenced (sym);

  if (sym->attr.function && sym->result == sym
      && (gfc_current_ns->proc_name == sym
	  || (gfc_current_ns->parent != NULL
	      && gfc_current_ns->parent->proc_name == sym)))
    goto variable;

  if (sym->attr.function || sym->attr.external || sym->attr.intrinsic)
    goto function0;

  if (sym->attr.generic)
    goto generic_function;

  switch (sym->attr.flavor)
    {
    case FL_VARIABLE:
    variable:
      if (sym->ts.type == BT_UNKNOWN && gfc_peek_char () == '%'
	  && gfc_get_default_type (sym, sym->ns)->type == BT_DERIVED)
	gfc_set_default_type (sym, 0, sym->ns);

      e = gfc_get_expr ();

      e->expr_type = EXPR_VARIABLE;
      e->symtree = symtree;

      m = match_varspec (e, 0);
      break;

    case FL_PARAMETER:
      if (sym->value
	  && sym->value->expr_type != EXPR_ARRAY)
	e = gfc_copy_expr (sym->value);
      else
	{
	  e = gfc_get_expr ();
	  e->expr_type = EXPR_VARIABLE;
	}

      e->symtree = symtree;
      m = match_varspec (e, 0);
      break;

    case FL_DERIVED:
      sym = gfc_use_derived (sym);
      if (sym == NULL)
	m = MATCH_ERROR;
      else
        m = gfc_match_structure_constructor (sym, &e);
      break;

    /* If we're here, then the name is known to be the name of a
       procedure, yet it is not sure to be the name of a function.  */
    case FL_PROCEDURE:
      if (sym->attr.subroutine)
	{
	  gfc_error ("Unexpected use of subroutine name '%s' at %C",
		     sym->name);
	  m = MATCH_ERROR;
	  break;
	}

      /* At this point, the name has to be a non-statement function.
         If the name is the same as the current function being
         compiled, then we have a variable reference (to the function
         result) if the name is non-recursive.  */

      st = gfc_enclosing_unit (NULL);

      if (st != NULL && st->state == COMP_FUNCTION
	  && st->sym == sym
	  && !sym->attr.recursive)
	{
	  e = gfc_get_expr ();
	  e->symtree = symtree;
	  e->expr_type = EXPR_VARIABLE;

	  m = match_varspec (e, 0);
	  break;
	}

    /* Match a function reference.  */
    function0:
      m = gfc_match_actual_arglist (0, &actual_arglist);
      if (m == MATCH_NO)
	{
	  if (sym->attr.proc == PROC_ST_FUNCTION)
	    gfc_error ("Statement function '%s' requires argument list at %C",
		       sym->name);
	  else
	    gfc_error ("Function '%s' requires an argument list at %C",
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

      e = gfc_get_expr ();
      e->symtree = symtree;
      e->expr_type = EXPR_FUNCTION;
      e->value.function.actual = actual_arglist;
      e->where = gfc_current_locus;

      if (sym->as != NULL)
	e->rank = sym->as->rank;

      if (!sym->attr.function
	  && gfc_add_function (&sym->attr, sym->name, NULL) == FAILURE)
	{
	  m = MATCH_ERROR;
	  break;
	}

      if (sym->result == NULL)
	sym->result = sym;

      m = MATCH_YES;
      break;

    case FL_UNKNOWN:

      /* Special case for derived type variables that get their types
         via an IMPLICIT statement.  This can't wait for the
         resolution phase.  */

      if (gfc_peek_char () == '%'
	  && sym->ts.type == BT_UNKNOWN
	  && gfc_get_default_type (sym, sym->ns)->type == BT_DERIVED)
	gfc_set_default_type (sym, 0, sym->ns);

      /* If the symbol has a dimension attribute, the expression is a
         variable.  */

      if (sym->attr.dimension)
	{
	  if (gfc_add_flavor (&sym->attr, FL_VARIABLE,
			      sym->name, NULL) == FAILURE)
	    {
	      m = MATCH_ERROR;
	      break;
	    }

	  e = gfc_get_expr ();
	  e->symtree = symtree;
	  e->expr_type = EXPR_VARIABLE;
	  m = match_varspec (e, 0);
	  break;
	}

      /* Name is not an array, so we peek to see if a '(' implies a
         function call or a substring reference.  Otherwise the
         variable is just a scalar.  */

      gfc_gobble_whitespace ();
      if (gfc_peek_char () != '(')
	{
	  /* Assume a scalar variable */
	  e = gfc_get_expr ();
	  e->symtree = symtree;
	  e->expr_type = EXPR_VARIABLE;

	  if (gfc_add_flavor (&sym->attr, FL_VARIABLE,
			      sym->name, NULL) == FAILURE)
	    {
	      m = MATCH_ERROR;
	      break;
	    }

	  e->ts = sym->ts;
	  m = match_varspec (e, 0);
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
	  /* See if this could possibly be a substring reference of a name
	     that we're not sure is a variable yet.  */

	  if ((sym->ts.type == BT_UNKNOWN || sym->ts.type == BT_CHARACTER)
	      && match_substring (sym->ts.cl, 0, &e->ref) == MATCH_YES)
	    {

	      e->expr_type = EXPR_VARIABLE;

	      if (sym->attr.flavor != FL_VARIABLE
		  && gfc_add_flavor (&sym->attr, FL_VARIABLE,
				     sym->name, NULL) == FAILURE)
		{
		  m = MATCH_ERROR;
		  break;
		}

	      if (sym->ts.type == BT_UNKNOWN
		  && gfc_set_default_type (sym, 1, NULL) == FAILURE)
		{
		  m = MATCH_ERROR;
		  break;
		}

	      e->ts = sym->ts;
	      if (e->ref)
		e->ts.cl = NULL;
	      m = MATCH_YES;
	      break;
	    }
	}

      /* Give up, assume we have a function.  */

      gfc_get_sym_tree (name, NULL, &symtree);	/* Can't fail */
      sym = symtree->n.sym;
      e->expr_type = EXPR_FUNCTION;

      if (!sym->attr.function
	  && gfc_add_function (&sym->attr, sym->name, NULL) == FAILURE)
	{
	  m = MATCH_ERROR;
	  break;
	}

      sym->result = sym;

      m = gfc_match_actual_arglist (0, &e->value.function.actual);
      if (m == MATCH_NO)
	gfc_error ("Missing argument list in function '%s' at %C", sym->name);

      if (m != MATCH_YES)
	{
	  m = MATCH_ERROR;
	  break;
	}

      /* If our new function returns a character, array or structure
         type, it might have subsequent references.  */

      m = match_varspec (e, 0);
      if (m == MATCH_NO)
	m = MATCH_YES;

      break;

    generic_function:
      gfc_get_sym_tree (name, NULL, &symtree);	/* Can't fail */

      e = gfc_get_expr ();
      e->symtree = symtree;
      e->expr_type = EXPR_FUNCTION;

      m = gfc_match_actual_arglist (0, &e->value.function.actual);
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


/* Match a variable, ie something that can be assigned to.  This
   starts as a symbol, can be a structure component or an array
   reference.  It can be a function if the function doesn't have a
   separate RESULT variable.  If the symbol has not been previously
   seen, we assume it is a variable.  */

match
gfc_match_variable (gfc_expr ** result, int equiv_flag)
{
  gfc_symbol *sym;
  gfc_symtree *st;
  gfc_expr *expr;
  locus where;
  match m;

  m = gfc_match_sym_tree (&st, 1);
  if (m != MATCH_YES)
    return m;
  where = gfc_current_locus;

  sym = st->n.sym;
  gfc_set_sym_referenced (sym);
  switch (sym->attr.flavor)
    {
    case FL_VARIABLE:
      break;

    case FL_UNKNOWN:
      if (gfc_add_flavor (&sym->attr, FL_VARIABLE,
			  sym->name, NULL) == FAILURE)
	return MATCH_ERROR;
      break;

    case FL_PROCEDURE:
      /* Check for a nonrecursive function result */
      if (sym->attr.function && (sym->result == sym || sym->attr.entry))
	{
	  /* If a function result is a derived type, then the derived
	     type may still have to be resolved.  */

	  if (sym->ts.type == BT_DERIVED
	      && gfc_use_derived (sym->ts.derived) == NULL)
	    return MATCH_ERROR;
	  break;
	}

      /* Fall through to error */

    default:
      gfc_error ("Expected VARIABLE at %C");
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
	
      if (gfc_peek_char () == '%'
	  && sym->ts.type == BT_UNKNOWN
	  && gfc_get_default_type (sym, implicit_ns)->type == BT_DERIVED)
	gfc_set_default_type (sym, 0, implicit_ns);
    }

  expr = gfc_get_expr ();

  expr->expr_type = EXPR_VARIABLE;
  expr->symtree = st;
  expr->ts = sym->ts;
  expr->where = where;

  /* Now see if we have to do more.  */
  m = match_varspec (expr, equiv_flag);
  if (m != MATCH_YES)
    {
      gfc_free_expr (expr);
      return m;
    }

  *result = expr;
  return MATCH_YES;
}
