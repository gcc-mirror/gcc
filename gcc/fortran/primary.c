/* Primary expression subroutines
   Copyright (C) 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.
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
#include "flags.h"
#include "gfortran.h"
#include "arith.h"
#include "match.h"
#include "parse.h"
#include "toplev.h"

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

  m = gfc_match_small_literal_int (kind, NULL);
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

  if (sym->value == NULL)
    return MATCH_NO;

  p = gfc_extract_int (sym->value, kind);
  if (p != NULL)
    return MATCH_NO;

  gfc_set_sym_referenced (sym);

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

int
gfc_check_digit (char c, int radix)
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


/* Match an integer (digit string and optional kind).  
   A sign will be accepted if signflag is set.  */

static match
match_integer_constant (gfc_expr **result, int signflag)
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

  buffer = (char *) alloca (length + 1);
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
      gfc_error ("Integer too big for its kind at %C. This check can be "
		 "disabled with the option -fno-range-check");

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
  const char *msg;
  int num;
  int i;  

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  if (match_integer_constant (&e, 0) == MATCH_YES
      && gfc_match_char ('h') == MATCH_YES)
    {
      if (gfc_notify_std (GFC_STD_LEGACY, "Extension: Hollerith constant "
			  "at %C") == FAILURE)
	goto cleanup;

      msg = gfc_extract_int (e, &num);
      if (msg != NULL)
	{
	  gfc_error (msg);
	  goto cleanup;
	}
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
	  e = gfc_constant_result (BT_HOLLERITH, gfc_default_character_kind,
				   &gfc_current_locus);

	  e->representation.string = XCNEWVEC (char, num + 1);

	  for (i = 0; i < num; i++)
	    {
	      gfc_char_t c = gfc_next_char_literal (1);
	      if (! gfc_wide_fits_in_byte (c))
		{
		  gfc_error ("Invalid Hollerith constant at %L contains a "
			     "wide character", &old_loc);
		  goto cleanup;
		}

	      e->representation.string[i] = (unsigned char) c;
	    }

	  e->representation.string[num] = '\0';
	  e->representation.length = num;

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
  int radix, length, x_hex, kind;
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
      && (gfc_notify_std (GFC_STD_GNU, "Extension: Hexadecimal "
			  "constant at %C uses non-standard syntax")
	  == FAILURE))
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

      if (gfc_notify_std (GFC_STD_GNU, "Extension: BOZ constant "
			  "at %C uses non-standard postfix syntax")
	  == FAILURE)
	return MATCH_ERROR;
    }

  gfc_current_locus = old_loc;

  buffer = (char *) alloca (length + 1);
  memset (buffer, '\0', length + 1);

  match_digits (0, radix, buffer);
  gfc_next_ascii_char ();    /* Eat delimiter.  */
  if (post == 1)
    gfc_next_ascii_char ();  /* Eat postfixed b, o, z, or x.  */

  /* In section 5.2.5 and following C567 in the Fortran 2003 standard, we find
     "If a data-stmt-constant is a boz-literal-constant, the corresponding
     variable shall be of type integer.  The boz-literal-constant is treated
     as if it were an int-literal-constant with a kind-param that specifies
     the representation method with the largest decimal exponent range
     supported by the processor."  */

  kind = gfc_max_integer_kind;
  e = gfc_convert_integer (buffer, kind, radix, &gfc_current_locus);

  /* Mark as boz variable.  */
  e->is_boz = 1;

  if (gfc_range_check (e) != ARITH_OK)
    {
      gfc_error ("Integer too big for integer kind %i at %C", kind);
      gfc_free_expr (e);
      return MATCH_ERROR;
    }

  if (!gfc_in_match_data ()
      && (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: BOZ used outside a DATA "
			  "statement at %C")
	  == FAILURE))
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
  int kind, count, seen_dp, seen_digits;
  locus old_loc, temp_loc;
  char *p, *buffer, c, exp_char;
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

  c = gfc_next_ascii_char ();
  if (signflag && (c == '+' || c == '-'))
    {
      if (c == '-')
	negate = TRUE;

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
      gfc_error ("Missing exponent in real number at %C");
      return MATCH_ERROR;
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

  buffer = (char *) alloca (count + 1);
  memset (buffer, '\0', count + 1);

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

  kind = get_kind ();
  if (kind == -1)
    goto cleanup;

  switch (exp_char)
    {
    case 'd':
      if (kind != -2)
	{
	  gfc_error ("Real number at %C has a 'd' exponent and an explicit "
		     "kind");
	  goto cleanup;
	}
      kind = gfc_default_double_kind;
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
match_substring (gfc_charlen *cl, int init, gfc_ref **result)
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

   Special return values for "ret" argument are:
     -1   End of the string, as determined by the delimiter
     -2   Unterminated string detected

   Backslash codes are also expanded at this time.  */

static gfc_char_t
next_string_char (gfc_char_t delimiter, int *ret)
{
  locus old_locus;
  gfc_char_t c;

  c = gfc_next_char_literal (1);
  *ret = 0;

  if (c == '\n')
    {
      *ret = -2;
      return 0;
    }

  if (gfc_option.flag_backslash && c == '\\')
    {
      old_locus = gfc_current_locus;

      if (gfc_match_special_char (&c) == MATCH_NO)
	gfc_current_locus = old_locus;

      if (!(gfc_option.allow_std & GFC_STD_GNU) && !inhibit_warnings)
	gfc_warning ("Extension: backslash character at %C");
    }

  if (c != delimiter)
    return c;

  old_locus = gfc_current_locus;
  c = gfc_next_char_literal (0);

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
	  && (c != '$' || !gfc_option.flag_dollar_ok))
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
  int i, kind, length, warn_ampersand, ret;
  locus old_locus, start_locus;
  gfc_symbol *sym;
  gfc_expr *e;
  const char *q;
  match m;
  gfc_char_t c, delimiter, *p;

  old_locus = gfc_current_locus;

  gfc_gobble_whitespace ();

  start_locus = gfc_current_locus;

  c = gfc_next_char ();
  if (c == '\'' || c == '"')
    {
      kind = gfc_default_character_kind;
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


  e = gfc_get_expr ();

  e->expr_type = EXPR_CONSTANT;
  e->ref = NULL;
  e->ts.type = BT_CHARACTER;
  e->ts.kind = kind;
  e->ts.is_c_interop = 0;
  e->ts.is_iso_c = 0;
  e->where = start_locus;

  e->value.character.string = p = gfc_get_wide_string (length + 1);
  e->value.character.length = length;

  gfc_current_locus = start_locus;
  gfc_next_char ();		/* Skip delimiter */

  /* We disable the warning for the following loop as the warning has already
     been printed in the loop above.  */
  warn_ampersand = gfc_option.warn_ampersand;
  gfc_option.warn_ampersand = 0;

  for (i = 0; i < length; i++)
    {
      c = next_string_char (delimiter, &ret);

      if (!gfc_check_character_range (c, kind))
	{
	  gfc_error ("Character '%s' in string at %C is not representable "
		     "in character kind %d", gfc_print_wide_char (c), kind);
	  return MATCH_ERROR;
	}

      *p++ = c;
    }

  *p = '\0';	/* TODO: C-style string is for development/debug purposes.  */
  gfc_option.warn_ampersand = warn_ampersand;

  next_string_char (delimiter, &ret);
  if (ret != -1)
    gfc_internal_error ("match_string_constant(): Delimiter not found");

  if (match_substring (NULL, 0, &e->ref) != MATCH_NO)
    e->expr_type = EXPR_SUBSTRING;

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
  int i, kind;

  i = match_logical_constant_string ();
  if (i == -1)
    return MATCH_NO;

  kind = get_kind ();
  if (kind == -1)
    return MATCH_ERROR;
  if (kind == -2)
    kind = gfc_default_logical_kind;

  if (gfc_validate_kind (BT_LOGICAL, kind, true) < 0)
    {
      gfc_error ("Bad kind for logical constant at %C");
      return MATCH_ERROR;
    }

  e = gfc_get_expr ();

  e->expr_type = EXPR_CONSTANT;
  e->value.logical = i;
  e->ts.type = BT_LOGICAL;
  e->ts.kind = kind;
  e->ts.is_c_interop = 0;
  e->ts.is_iso_c = 0;
  e->where = gfc_current_locus;

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

  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: PARAMETER symbol in "
		      "complex constant at %C") == FAILURE)
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
    {
      gfc_free_error (&old_error);
      goto cleanup;
    }

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
  target.type = BT_REAL;
  target.kind = kind;
  target.is_c_interop = 0;
  target.is_iso_c = 0;

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
	  if (sym->attr.flavor != FL_PROCEDURE
	      && sym->attr.flavor != FL_UNKNOWN)
	    break;

	  if (sym->attr.in_common && !sym->attr.proc_pointer)
	    {
	      gfc_add_flavor (&sym->attr, FL_VARIABLE, sym->name,
			      &sym->declared_at);
	      break;
	    }

	  /* If the symbol is a function with itself as the result and
	     is being defined, then we have a variable.  */
	  if (sym->attr.function && sym->result == sym)
	    {
	      if (gfc_current_ns->proc_name == sym
		  || (gfc_current_ns->parent != NULL
		      && gfc_current_ns->parent->proc_name == sym))
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


/* Match a keyword argument.  */

static match
match_keyword_arg (gfc_actual_arglist *actual, gfc_actual_arglist *base)
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
	    gfc_error ("Keyword '%s' at %C has already appeared in the "
		       "current argument list", name);
	    return MATCH_ERROR;
	  }
    }

  actual->name = gfc_get_string (name);
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
	  if (strncmp (name, "loc", 3) == 0)
	    {
	      result->name = "%LOC";
	      break;
	    }
	case 'r':
	  if (strncmp (name, "ref", 3) == 0)
	    {
	      result->name = "%REF";
	      break;
	    }
	case 'v':
	  if (strncmp (name, "val", 3) == 0)
	    {
	      result->name = "%VAL";
	      break;
	    }
	default:
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  if (gfc_notify_std (GFC_STD_GNU, "Extension: argument list "
		      "function at %C") == FAILURE)
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
   we're matching the argument list of a subroutine.  */

match
gfc_match_actual_arglist (int sub_flag, gfc_actual_arglist **argp)
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
	  m = gfc_match_st_label (&label);
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
	      m = match_keyword_arg (tail, head);
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
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in argument list at %C");

cleanup:
  gfc_free_actual_arglist (head);
  gfc_current_locus = old_loc;

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
  gfc_ref *substring, *tail;
  gfc_component *component;
  gfc_symbol *sym = primary->symtree->n.sym;
  match m;
  bool unknown;

  tail = NULL;

  gfc_gobble_whitespace ();
  if ((equiv_flag && gfc_peek_ascii_char () == '(')
      || (sym->attr.dimension && !sym->attr.proc_pointer
	  && !gfc_is_proc_ptr_comp (primary, NULL)
	  && !(gfc_matching_procptr_assignment
	       && sym->attr.flavor == FL_PROCEDURE)))
    {
      /* In EQUIVALENCE, we don't know yet whether we are seeing
	 an array, character variable or array of character
	 variables.  We'll leave the decision till resolve time.  */
      tail = extend_ref (primary, tail);
      tail->type = REF_ARRAY;

      m = gfc_match_array_ref (&tail->u.ar, equiv_flag ? NULL : sym->as,
			       equiv_flag);
      if (m != MATCH_YES)
	return m;

      gfc_gobble_whitespace ();
      if (equiv_flag && gfc_peek_ascii_char () == '(')
	{
	  tail = extend_ref (primary, tail);
	  tail->type = REF_ARRAY;

	  m = gfc_match_array_ref (&tail->u.ar, NULL, equiv_flag);
	  if (m != MATCH_YES)
	    return m;
	}
    }

  primary->ts = sym->ts;

  if (equiv_flag)
    return MATCH_YES;

  if (sym->ts.type == BT_UNKNOWN && gfc_peek_ascii_char () == '%'
      && gfc_get_default_type (sym->name, sym->ns)->type == BT_DERIVED)
    gfc_set_default_type (sym, 0, sym->ns);

  if (sym->ts.type != BT_DERIVED || gfc_match_char ('%') != MATCH_YES)
    goto check_substring;

  sym = sym->ts.u.derived;

  for (;;)
    {
      gfc_try t;
      gfc_symtree *tbp;

      m = gfc_match_name (name);
      if (m == MATCH_NO)
	gfc_error ("Expected structure component name at %C");
      if (m != MATCH_YES)
	return MATCH_ERROR;

      if (sym->f2k_derived)
	tbp = gfc_find_typebound_proc (sym, &t, name, false, &gfc_current_locus);
      else
	tbp = NULL;

      if (tbp)
	{
	  gfc_symbol* tbp_sym;

	  if (t == FAILURE)
	    return MATCH_ERROR;

	  gcc_assert (!tail || !tail->next);
	  gcc_assert (primary->expr_type == EXPR_VARIABLE);

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

      component = gfc_find_component (sym, name, false, false);
      if (component == NULL)
	return MATCH_ERROR;

      tail = extend_ref (primary, tail);
      tail->type = REF_COMPONENT;

      tail->u.c.component = component;
      tail->u.c.sym = sym;

      primary->ts = component->ts;

      if (component->attr.proc_pointer && ppc_arg
	  && !gfc_matching_procptr_assignment)
	{
	  m = gfc_match_actual_arglist (sub_flag,
					&primary->value.compcall.actual);
	  if (m == MATCH_ERROR)
	    return MATCH_ERROR;
	  if (m == MATCH_YES)
	    primary->expr_type = EXPR_PPC;

          break;
	}

      if (component->as != NULL && !component->attr.proc_pointer)
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

      sym = component->ts.u.derived;
    }

check_substring:
  unknown = false;
  if (primary->ts.type == BT_UNKNOWN)
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
      switch (match_substring (primary->ts.u.cl, equiv_flag, &substring))
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
  int dimension, pointer, allocatable, target;
  symbol_attribute attr;
  gfc_ref *ref;

  if (expr->expr_type != EXPR_VARIABLE && expr->expr_type != EXPR_FUNCTION)
    gfc_internal_error ("gfc_variable_attr(): Expression isn't a variable");

  ref = expr->ref;
  attr = expr->symtree->n.sym->attr;

  dimension = attr.dimension;
  pointer = attr.pointer;
  allocatable = attr.allocatable;

  target = attr.target;
  if (pointer || attr.proc_pointer)
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
	    allocatable = pointer = 0;
	    dimension = 1;
	    break;

	  case AR_ELEMENT:
	    allocatable = pointer = 0;
	    break;

	  case AR_UNKNOWN:
	    gfc_internal_error ("gfc_variable_attr(): Bad array reference");
	  }

	break;

      case REF_COMPONENT:
	attr = ref->u.c.component->attr;
	if (ts != NULL)
	  {
	    *ts = ref->u.c.component->ts;
	    /* Don't set the string length if a substring reference
	       follows.  */
	    if (ts->type == BT_CHARACTER
		&& ref->next && ref->next->type == REF_SUBSTRING)
		ts->u.cl = NULL;
	  }

	pointer = ref->u.c.component->attr.pointer;
	allocatable = ref->u.c.component->attr.allocatable;
	if (pointer || attr.proc_pointer)
	  target = 1;

	break;

      case REF_SUBSTRING:
	allocatable = pointer = 0;
	break;
      }

  attr.dimension = dimension;
  attr.pointer = pointer;
  attr.allocatable = allocatable;
  attr.target = target;

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

      if (e->value.function.esym != NULL)
	attr = e->value.function.esym->result->attr;
      else
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
  gfc_free (comp->name);
  gfc_free_expr (comp->val);
}


/* Translate the component list into the actual constructor by sorting it in
   the order required; this also checks along the way that each and every
   component actually has an initializer and handles default initializers
   for components without explicit value given.  */
static gfc_try
build_actual_constructor (gfc_structure_ctor_component **comp_head,
			  gfc_constructor **ctor_head, gfc_symbol *sym)
{
  gfc_structure_ctor_component *comp_iter;
  gfc_constructor *ctor_tail = NULL;
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
	  value = gfc_get_expr ();
	  value->expr_type = EXPR_STRUCTURE;
	  value->value.constructor = NULL;
	  value->ts = comp->ts;
	  value->where = gfc_current_locus;

	  if (build_actual_constructor (comp_head, &value->value.constructor,
					comp->ts.u.derived) == FAILURE)
	    {
	      gfc_free_expr (value);
	      return FAILURE;
	    }
	  *ctor_head = ctor_tail = gfc_get_constructor ();
	  ctor_tail->expr = value;
	  continue;
	}

      /* If it was not found, try the default initializer if there's any;
	 otherwise, it's an error.  */
      if (!comp_iter)
	{
	  if (comp->initializer)
	    {
	      if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: Structure"
				  " constructor with missing optional arguments"
				  " at %C") == FAILURE)
		return FAILURE;
	      value = gfc_copy_expr (comp->initializer);
	    }
	  else
	    {
	      gfc_error ("No initializer for component '%s' given in the"
			 " structure constructor at %C!", comp->name);
	      return FAILURE;
	    }
	}
      else
	value = comp_iter->val;

      /* Add the value to the constructor chain built.  */
      if (ctor_tail)
	{
	  ctor_tail->next = gfc_get_constructor ();
	  ctor_tail = ctor_tail->next;
	}
      else
	*ctor_head = ctor_tail = gfc_get_constructor ();
      gcc_assert (value);
      ctor_tail->expr = value;

      /* Remove the entry from the component list.  We don't want the expression
	 value to be free'd, so set it to NULL.  */
      if (comp_iter)
	{
	  *next_ptr = comp_iter->next;
	  comp_iter->val = NULL;
	  gfc_free_structure_ctor_component (comp_iter);
	}
    }
  return SUCCESS;
}

match
gfc_match_structure_constructor (gfc_symbol *sym, gfc_expr **result,
				 bool parent)
{
  gfc_structure_ctor_component *comp_tail, *comp_head, *comp_iter;
  gfc_constructor *ctor_head, *ctor_tail;
  gfc_component *comp; /* Is set NULL when named component is first seen */
  gfc_expr *e;
  locus where;
  match m;
  const char* last_name = NULL;

  comp_tail = comp_head = NULL;
  ctor_head = ctor_tail = NULL;

  if (!parent && gfc_match_char ('(') != MATCH_YES)
    goto syntax;

  where = gfc_current_locus;

  gfc_find_component (sym, NULL, false, true);

  /* Check that we're not about to construct an ABSTRACT type.  */
  if (!parent && sym->attr.abstract)
    {
      gfc_error ("Can't construct ABSTRACT type '%s' at %C", sym->name);
      return MATCH_ERROR;
    }

  /* Match the component list and store it in a list together with the
     corresponding component names.  Check for empty argument list first.  */
  if (gfc_match_char (')') != MATCH_YES)
    {
      comp = sym->components;
      do
	{
	  gfc_component *this_comp = NULL;

	  if (!comp_head)
	    comp_tail = comp_head = gfc_get_structure_ctor_component ();
	  else
	    {
	      comp_tail->next = gfc_get_structure_ctor_component ();
	      comp_tail = comp_tail->next;
	    }
	  comp_tail->name = XCNEWVEC (char, GFC_MAX_SYMBOL_LEN + 1);
	  comp_tail->val = NULL;
	  comp_tail->where = gfc_current_locus;

	  /* Try matching a component name.  */
	  if (gfc_match_name (comp_tail->name) == MATCH_YES 
	      && gfc_match_char ('=') == MATCH_YES)
	    {
	      if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: Structure"
				  " constructor with named arguments at %C")
		  == FAILURE)
		goto cleanup;

	      last_name = comp_tail->name;
	      comp = NULL;
	    }
	  else
	    {
	      /* Components without name are not allowed after the first named
		 component initializer!  */
	      if (!comp)
		{
		  if (last_name)
		    gfc_error ("Component initializer without name after"
			       " component named %s at %C!", last_name);
		  else if (!parent)
		    gfc_error ("Too many components in structure constructor at"
			       " %C!");
		  goto cleanup;
		}

	      gfc_current_locus = comp_tail->where;
	      strncpy (comp_tail->name, comp->name, GFC_MAX_SYMBOL_LEN + 1);
	    }

	  /* Find the current component in the structure definition and check
	     its access is not private.  */
	  if (comp)
	    this_comp = gfc_find_component (sym, comp->name, false, false);
	  else
	    {
	      this_comp = gfc_find_component (sym,
					      (const char *)comp_tail->name,
					      false, false);
	      comp = NULL; /* Reset needed!  */
	    }

	  /* Here we can check if a component name is given which does not
	     correspond to any component of the defined structure.  */
	  if (!this_comp)
	    goto cleanup;

	  /* Check if this component is already given a value.  */
	  for (comp_iter = comp_head; comp_iter != comp_tail; 
	       comp_iter = comp_iter->next)
	    {
	      gcc_assert (comp_iter);
	      if (!strcmp (comp_iter->name, comp_tail->name))
		{
		  gfc_error ("Component '%s' is initialized twice in the"
			     " structure constructor at %C!", comp_tail->name);
		  goto cleanup;
		}
	    }

	  /* Match the current initializer expression.  */
	  m = gfc_match_expr (&comp_tail->val);
	  if (m == MATCH_NO)
	    goto syntax;
	  if (m == MATCH_ERROR)
	    goto cleanup;

	  /* If not explicitly a parent constructor, gather up the components
	     and build one.  */
	  if (comp && comp == sym->components
		&& sym->attr.extension
		&& (comp_tail->val->ts.type != BT_DERIVED
		      ||
		    comp_tail->val->ts.u.derived != this_comp->ts.u.derived))
	    {
	      gfc_current_locus = where;
	      gfc_free_expr (comp_tail->val);
	      comp_tail->val = NULL;

	      m = gfc_match_structure_constructor (comp->ts.u.derived, 
						   &comp_tail->val, true);
	      if (m == MATCH_NO)
		goto syntax;
	      if (m == MATCH_ERROR)
		goto cleanup;
	    }

 	  if (comp)
	    comp = comp->next;

	  if (parent && !comp)
	    break;
	}

      while (gfc_match_char (',') == MATCH_YES);

      if (!parent && gfc_match_char (')') != MATCH_YES)
	goto syntax;
    }

  if (build_actual_constructor (&comp_head, &ctor_head, sym) == FAILURE)
    goto cleanup;

  /* No component should be left, as this should have caused an error in the
     loop constructing the component-list (name that does not correspond to any
     component in the structure definition).  */
  if (comp_head && sym->attr.extension)
    {
      for (comp_iter = comp_head; comp_iter; comp_iter = comp_iter->next)
	{
	  gfc_error ("component '%s' at %L has already been set by a "
		     "parent derived type constructor", comp_iter->name,
		     &comp_iter->where);
	}
      goto cleanup;
    }
  else
    gcc_assert (!comp_head);

  e = gfc_get_expr ();

  e->expr_type = EXPR_STRUCTURE;

  e->ts.type = BT_DERIVED;
  e->ts.u.derived = sym;
  e->where = where;

  e->value.constructor = ctor_head;

  *result = e;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in structure constructor at %C");

cleanup:
  for (comp_iter = comp_head; comp_iter; )
    {
      gfc_structure_ctor_component *next = comp_iter->next;
      gfc_free_structure_ctor_component (comp_iter);
      comp_iter = next;
    }
  gfc_free_constructor (ctor_head);
  return MATCH_ERROR;
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

static gfc_try
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
      return SUCCESS;
    }
  return FAILURE;
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

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (gfc_find_state (COMP_INTERFACE) == SUCCESS
      && !gfc_current_ns->has_import_set)
    i = gfc_get_sym_tree (name, NULL, &symtree, false);
  else
    i = gfc_get_ha_sym_tree (name, &symtree);

  if (i)
    return MATCH_ERROR;

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
	  gfc_error ("'%s' at %C is the name of a recursive function "
		     "and so refers to the result variable. Use an "
		     "explicit RESULT variable for direct recursion "
		     "(12.5.2.1)", sym->name);
	  return MATCH_ERROR;
	}

      if (gfc_current_ns->proc_name == sym
	  || (gfc_current_ns->parent != NULL
	      && gfc_current_ns->parent->proc_name == sym))
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
    goto procptr0;

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

    case FL_DERIVED:
      sym = gfc_use_derived (sym);
      if (sym == NULL)
	m = MATCH_ERROR;
      else
	m = gfc_match_structure_constructor (sym, &e, false);
      break;

    /* If we're here, then the name is known to be the name of a
       procedure, yet it is not sure to be the name of a function.  */
    case FL_PROCEDURE:

    /* Procedure Pointer Assignments. */
    procptr0:
      if (gfc_matching_procptr_assignment)
	{
	  gfc_gobble_whitespace ();
	  if (!sym->attr.dimension && gfc_peek_ascii_char () == '(')
	    /* Parse functions returning a procptr.  */
	    goto function0;

	  if (gfc_is_intrinsic (sym, 0, gfc_current_locus)
	      || gfc_is_intrinsic (sym, 1, gfc_current_locus))
	    sym->attr.intrinsic = 1;
	  e = gfc_get_expr ();
	  e->expr_type = EXPR_VARIABLE;
	  e->symtree = symtree;
	  m = gfc_match_varspec (e, 0, false, true);
	  break;
	}

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

	  m = gfc_match_varspec (e, 0, false, true);
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

      replace_hidden_procptr_result (&sym, &symtree);

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
              gfc_error ("Missing argument to '%s' at %C", sym->name);
              m = MATCH_ERROR;
              break;
            }
        }

      if (sym->result == NULL)
	sym->result = sym;

      m = MATCH_YES;
      break;

    case FL_UNKNOWN:

      /* Special case for derived type variables that get their types
	 via an IMPLICIT statement.  This can't wait for the
	 resolution phase.  */

      if (gfc_peek_ascii_char () == '%'
	  && sym->ts.type == BT_UNKNOWN
	  && gfc_get_default_type (sym->name, sym->ns)->type == BT_DERIVED)
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

	  if (gfc_add_flavor (&sym->attr, FL_VARIABLE,
			      sym->name, NULL) == FAILURE)
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
	      && match_substring (sym->ts.u.cl, 0, &e->ref) == MATCH_YES)
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

      m = gfc_match_varspec (e, 0, false, true);
      if (m == MATCH_NO)
	m = MATCH_YES;

      break;

    generic_function:
      gfc_get_sym_tree (name, NULL, &symtree, false);	/* Can't fail */

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
  gfc_symbol *sym;
  gfc_symtree *st;
  gfc_expr *expr;
  locus where;
  match m;

  /* Since nothing has any business being an lvalue in a module
     specification block, an interface block or a contains section,
     we force the changed_symbols mechanism to work by setting
     host_flag to 0. This prevents valid symbols that have the name
     of keywords, such as 'end', being turned into variables by
     failed matching to assignments for, e.g., END INTERFACE.  */
  if (gfc_current_state () == COMP_MODULE
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
  switch (sym->attr.flavor)
    {
    case FL_VARIABLE:
      if (sym->attr.is_protected && sym->attr.use_assoc)
	{
	  gfc_error ("Assigning to PROTECTED variable at %C");
	  return MATCH_ERROR;
	}
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
	    && gfc_add_flavor (&sym->attr, flavor, sym->name, NULL) == FAILURE)
	  return MATCH_ERROR;
      }
      break;

    case FL_PARAMETER:
      if (equiv_flag)
	gfc_error ("Named constant at %C in an EQUIVALENCE");
      else
	gfc_error ("Cannot assign to a named constant at %C");
      return MATCH_ERROR;
      break;

    case FL_PROCEDURE:
      /* Check for a nonrecursive function result variable.  */
      if (sym->attr.function
          && !sym->attr.external
          && sym->result == sym
          && ((sym == gfc_current_ns->proc_name
               && sym == gfc_current_ns->proc_name->result)
              || (gfc_current_ns->parent
                  && sym == gfc_current_ns->parent->proc_name->result)
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
	  || replace_hidden_procptr_result (&sym, &st) == SUCCESS)
	break;

      /* Fall through to error */

    default:
      gfc_error ("'%s' at %C is not a variable", sym->name);
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
	
      if (gfc_peek_ascii_char () == '%'
	  && sym->ts.type == BT_UNKNOWN
	  && gfc_get_default_type (sym->name, implicit_ns)->type == BT_DERIVED)
	gfc_set_default_type (sym, 0, implicit_ns);
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

