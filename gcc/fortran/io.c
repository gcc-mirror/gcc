/* Deal with I/O statements & related stuff.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation,
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
#include "flags.h"
#include "gfortran.h"
#include "match.h"
#include "parse.h"

gfc_st_label format_asterisk =
  { -1, ST_LABEL_FORMAT, ST_LABEL_FORMAT, NULL, 0,
    {NULL, NULL}, NULL, NULL};

typedef struct
{
  const char *name, *spec;
  bt type;
}
io_tag;

static const io_tag
	tag_file	= { "FILE", " file = %e", BT_CHARACTER },
	tag_status	= { "STATUS", " status = %e", BT_CHARACTER},
	tag_e_access	= {"ACCESS", " access = %e", BT_CHARACTER},
	tag_e_form	= {"FORM", " form = %e", BT_CHARACTER},
	tag_e_recl	= {"RECL", " recl = %e", BT_INTEGER},
	tag_e_blank	= {"BLANK", " blank = %e", BT_CHARACTER},
	tag_e_position	= {"POSITION", " position = %e", BT_CHARACTER},
	tag_e_action	= {"ACTION", " action = %e", BT_CHARACTER},
	tag_e_delim	= {"DELIM", " delim = %e", BT_CHARACTER},
	tag_e_pad	= {"PAD", " pad = %e", BT_CHARACTER},
	tag_unit	= {"UNIT", " unit = %e", BT_INTEGER},
	tag_advance	= {"ADVANCE", " advance = %e", BT_CHARACTER},
	tag_rec		= {"REC", " rec = %e", BT_INTEGER},
	tag_format	= {"FORMAT", NULL, BT_CHARACTER},
	tag_iomsg	= {"IOMSG", " iomsg = %e", BT_CHARACTER},
	tag_iostat	= {"IOSTAT", " iostat = %v", BT_INTEGER},
	tag_size	= {"SIZE", " size = %v", BT_INTEGER},
	tag_exist	= {"EXIST", " exist = %v", BT_LOGICAL},
	tag_opened	= {"OPENED", " opened = %v", BT_LOGICAL},
	tag_named	= {"NAMED", " named = %v", BT_LOGICAL},
	tag_name	= {"NAME", " name = %v", BT_CHARACTER},
	tag_number	= {"NUMBER", " number = %v", BT_INTEGER},
	tag_s_access	= {"ACCESS", " access = %v", BT_CHARACTER},
	tag_sequential	= {"SEQUENTIAL", " sequential = %v", BT_CHARACTER},
	tag_direct	= {"DIRECT", " direct = %v", BT_CHARACTER},
	tag_s_form	= {"FORM", " form = %v", BT_CHARACTER},
	tag_formatted	= {"FORMATTED", " formatted = %v", BT_CHARACTER},
	tag_unformatted	= {"UNFORMATTED", " unformatted = %v", BT_CHARACTER},
	tag_s_recl	= {"RECL", " recl = %v", BT_INTEGER},
	tag_nextrec	= {"NEXTREC", " nextrec = %v", BT_INTEGER},
	tag_s_blank	= {"BLANK", " blank = %v", BT_CHARACTER},
	tag_s_position	= {"POSITION", " position = %v", BT_CHARACTER},
	tag_s_action	= {"ACTION", " action = %v", BT_CHARACTER},
	tag_read	= {"READ", " read = %v", BT_CHARACTER},
	tag_write	= {"WRITE", " write = %v", BT_CHARACTER},
	tag_readwrite	= {"READWRITE", " readwrite = %v", BT_CHARACTER},
	tag_s_delim	= {"DELIM", " delim = %v", BT_CHARACTER},
	tag_s_pad	= {"PAD", " pad = %v", BT_CHARACTER},
	tag_iolength	= {"IOLENGTH", " iolength = %v", BT_INTEGER},
	tag_err		= {"ERR", " err = %l", BT_UNKNOWN},
	tag_end		= {"END", " end = %l", BT_UNKNOWN},
	tag_eor		= {"EOR", " eor = %l", BT_UNKNOWN};

static gfc_dt *current_dt;

#define RESOLVE_TAG(x, y) if (resolve_tag(x, y) == FAILURE) return FAILURE;


/**************** Fortran 95 FORMAT parser  *****************/

/* FORMAT tokens returned by format_lex().  */
typedef enum
{
  FMT_NONE, FMT_UNKNOWN, FMT_SIGNED_INT, FMT_ZERO, FMT_POSINT, FMT_PERIOD,
  FMT_COMMA, FMT_COLON, FMT_SLASH, FMT_DOLLAR, FMT_POS, FMT_LPAREN,
  FMT_RPAREN, FMT_X, FMT_SIGN, FMT_BLANK, FMT_CHAR, FMT_P, FMT_IBOZ, FMT_F,
  FMT_E, FMT_EXT, FMT_G, FMT_L, FMT_A, FMT_D, FMT_H, FMT_END
}
format_token;

/* Local variables for checking format strings.  The saved_token is
   used to back up by a single format token during the parsing
   process.  */
static char *format_string;
static int format_length, use_last_char;

static format_token saved_token;

static enum
{ MODE_STRING, MODE_FORMAT, MODE_COPY }
mode;


/* Return the next character in the format string.  */

static char
next_char (int in_string)
{
  static char c;

  if (use_last_char)
    {
      use_last_char = 0;
      return c;
    }

  format_length++;

  if (mode == MODE_STRING)
    c = *format_string++;
  else
    {
      c = gfc_next_char_literal (in_string);
      if (c == '\n')
	c = '\0';

      if (mode == MODE_COPY)
	*format_string++ = c;
    }

  c = TOUPPER (c);
  return c;
}


/* Back up one character position.  Only works once.  */

static void
unget_char (void)
{

  use_last_char = 1;
}

static int value = 0;

/* Simple lexical analyzer for getting the next token in a FORMAT
   statement.  */

static format_token
format_lex (void)
{
  format_token token;
  char c, delim;
  int zflag;
  int negative_flag;

  if (saved_token != FMT_NONE)
    {
      token = saved_token;
      saved_token = FMT_NONE;
      return token;
    }

  do
    {
      c = next_char (0);
    }
  while (gfc_is_whitespace (c));

  negative_flag = 0;
  switch (c)
    {
    case '-':
      negative_flag = 1;
    case '+':
      c = next_char (0);
      if (!ISDIGIT (c))
	{
	  token = FMT_UNKNOWN;
	  break;
	}

      value = c - '0';

      do
	{
	  c = next_char (0);
          if(ISDIGIT (c))
            value = 10 * value + c - '0';
	}
      while (ISDIGIT (c));

      unget_char ();

      if (negative_flag)
        value = -value;

      token = FMT_SIGNED_INT;
      break;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      zflag = (c == '0');

      value = c - '0';

      do
	{
	  c = next_char (0);
	  if (c != '0')
	    zflag = 0;
          if (ISDIGIT (c))
            value = 10 * value + c - '0';
	}
      while (ISDIGIT (c));

      unget_char ();
      token = zflag ? FMT_ZERO : FMT_POSINT;
      break;

    case '.':
      token = FMT_PERIOD;
      break;

    case ',':
      token = FMT_COMMA;
      break;

    case ':':
      token = FMT_COLON;
      break;

    case '/':
      token = FMT_SLASH;
      break;

    case '$':
      token = FMT_DOLLAR;
      break;

    case 'T':
      c = next_char (0);
      if (c != 'L' && c != 'R')
	unget_char ();

      token = FMT_POS;
      break;

    case '(':
      token = FMT_LPAREN;
      break;

    case ')':
      token = FMT_RPAREN;
      break;

    case 'X':
      token = FMT_X;
      break;

    case 'S':
      c = next_char (0);
      if (c != 'P' && c != 'S')
	unget_char ();

      token = FMT_SIGN;
      break;

    case 'B':
      c = next_char (0);
      if (c == 'N' || c == 'Z')
	token = FMT_BLANK;
      else
	{
	  unget_char ();
	  token = FMT_IBOZ;
	}

      break;

    case '\'':
    case '"':
      delim = c;

      value = 0;

      for (;;)
	{
	  c = next_char (1);
	  if (c == '\0')
	    {
	      token = FMT_END;
	      break;
	    }

	  if (c == delim)
	    {
	      c = next_char (1);

	      if (c == '\0')
		{
		  token = FMT_END;
		  break;
		}

	      if (c != delim)
		{
		  unget_char ();
		  token = FMT_CHAR;
		  break;
		}
	    }
          value++;
	}
      break;

    case 'P':
      token = FMT_P;
      break;

    case 'I':
    case 'O':
    case 'Z':
      token = FMT_IBOZ;
      break;

    case 'F':
      token = FMT_F;
      break;

    case 'E':
      c = next_char (0);
      if (c == 'N' || c == 'S')
	token = FMT_EXT;
      else
	{
	  token = FMT_E;
	  unget_char ();
	}

      break;

    case 'G':
      token = FMT_G;
      break;

    case 'H':
      token = FMT_H;
      break;

    case 'L':
      token = FMT_L;
      break;

    case 'A':
      token = FMT_A;
      break;

    case 'D':
      token = FMT_D;
      break;

    case '\0':
      token = FMT_END;
      break;

    default:
      token = FMT_UNKNOWN;
      break;
    }

  return token;
}


/* Check a format statement.  The format string, either from a FORMAT
   statement or a constant in an I/O statement has already been parsed
   by itself, and we are checking it for validity.  The dual origin
   means that the warning message is a little less than great.  */

static try
check_format (void)
{
  const char *posint_required	  = _("Positive width required");
  const char *period_required	  = _("Period required");
  const char *nonneg_required	  = _("Nonnegative width required");
  const char *unexpected_element  = _("Unexpected element");
  const char *unexpected_end	  = _("Unexpected end of format string");

  const char *error;
  format_token t, u;
  int level;
  int repeat;
  try rv;

  use_last_char = 0;
  saved_token = FMT_NONE;
  level = 0;
  repeat = 0;
  rv = SUCCESS;

  t = format_lex ();
  if (t != FMT_LPAREN)
    {
      error = _("Missing leading left parenthesis");
      goto syntax;
    }

  t = format_lex ();
  if (t == FMT_RPAREN)
    goto finished;		/* Empty format is legal */
  saved_token = t;

format_item:
  /* In this state, the next thing has to be a format item.  */
  t = format_lex ();
format_item_1:
  switch (t)
    {
    case FMT_POSINT:
      repeat = value;
      t = format_lex ();
      if (t == FMT_LPAREN)
	{
	  level++;
	  goto format_item;
	}

      if (t == FMT_SLASH)
	goto optional_comma;

      goto data_desc;

    case FMT_LPAREN:
      level++;
      goto format_item;

    case FMT_SIGNED_INT:
      /* Signed integer can only precede a P format.  */
      t = format_lex ();
      if (t != FMT_P)
	{
	  error = _("Expected P edit descriptor");
	  goto syntax;
	}

      goto data_desc;

    case FMT_P:
      /* P requires a prior number.  */
      error = _("P descriptor requires leading scale factor");
      goto syntax;

    case FMT_X:
      /* X requires a prior number if we're being pedantic.  */
      if (gfc_notify_std (GFC_STD_GNU, "Extension: X descriptor "
			  "requires leading space count at %C")
	  == FAILURE)
	return FAILURE;
      goto between_desc;

    case FMT_SIGN:
    case FMT_BLANK:
      goto between_desc;

    case FMT_CHAR:
      goto extension_optional_comma;

    case FMT_COLON:
    case FMT_SLASH:
      goto optional_comma;

    case FMT_DOLLAR:
      t = format_lex ();

      if (gfc_notify_std (GFC_STD_GNU, "Extension: $ descriptor at %C")
          == FAILURE)
        return FAILURE;
      if (t != FMT_RPAREN || level > 0)
	{
	  error = _("$ must be the last specifier");
	  goto syntax;
	}

      goto finished;

    case FMT_POS:
    case FMT_IBOZ:
    case FMT_F:
    case FMT_E:
    case FMT_EXT:
    case FMT_G:
    case FMT_L:
    case FMT_A:
    case FMT_D:
      goto data_desc;

    case FMT_H:
      goto data_desc;

    case FMT_END:
      error = unexpected_end;
      goto syntax;

    default:
      error = unexpected_element;
      goto syntax;
    }

data_desc:
  /* In this state, t must currently be a data descriptor.
     Deal with things that can/must follow the descriptor.  */
  switch (t)
    {
    case FMT_SIGN:
    case FMT_BLANK:
    case FMT_X:
      break;

    case FMT_P:
      if (pedantic)
	{
	  t = format_lex ();
	  if (t == FMT_POSINT)
	    {
	      error = _("Repeat count cannot follow P descriptor");
	      goto syntax;
	    }

	  saved_token = t;
	}

      goto optional_comma;

    case FMT_POS:
    case FMT_L:
      t = format_lex ();
      if (t == FMT_POSINT)
	break;

      error = posint_required;
      goto syntax;

    case FMT_A:
      t = format_lex ();
      if (t != FMT_POSINT)
	saved_token = t;
      break;

    case FMT_D:
    case FMT_E:
    case FMT_G:
    case FMT_EXT:
      u = format_lex ();
      if (u != FMT_POSINT)
	{
	  error = posint_required;
	  goto syntax;
	}

      u = format_lex ();
      if (u != FMT_PERIOD)
	{
	  error = period_required;
	  goto syntax;
	}

      u = format_lex ();
      if (u != FMT_ZERO && u != FMT_POSINT)
	{
	  error = nonneg_required;
	  goto syntax;
	}

      if (t == FMT_D)
	break;

      /* Look for optional exponent.  */
      u = format_lex ();
      if (u != FMT_E)
	{
	  saved_token = u;
	}
      else
	{
	  u = format_lex ();
	  if (u != FMT_POSINT)
	    {
	      error = _("Positive exponent width required");
	      goto syntax;
	    }
	}

      break;

    case FMT_F:
      t = format_lex ();
      if (t != FMT_ZERO && t != FMT_POSINT)
	{
	  error = nonneg_required;
	  goto syntax;
	}

      t = format_lex ();
      if (t != FMT_PERIOD)
	{
	  error = period_required;
	  goto syntax;
	}

      t = format_lex ();
      if (t != FMT_ZERO && t != FMT_POSINT)
	{
	  error = nonneg_required;
	  goto syntax;
	}

      break;

    case FMT_H:
      if(mode == MODE_STRING)
      {
        format_string += value;
        format_length -= value;
      }
      else
      {
        while(repeat >0)
         {
          next_char(1);
          repeat -- ;
         }
      }
     break;

    case FMT_IBOZ:
      t = format_lex ();
      if (t != FMT_ZERO && t != FMT_POSINT)
	{
	  error = nonneg_required;
	  goto syntax;
	}

      t = format_lex ();
      if (t != FMT_PERIOD)
	{
	  saved_token = t;
	}
      else
	{
	  t = format_lex ();
	  if (t != FMT_ZERO && t != FMT_POSINT)
	    {
	      error = nonneg_required;
	      goto syntax;
	    }
	}

      break;

    default:
      error = unexpected_element;
      goto syntax;
    }

between_desc:
  /* Between a descriptor and what comes next.  */
  t = format_lex ();
  switch (t)
    {

    case FMT_COMMA:
      goto format_item;

    case FMT_RPAREN:
      level--;
      if (level < 0)
	goto finished;
      goto between_desc;

    case FMT_COLON:
    case FMT_SLASH:
      goto optional_comma;

    case FMT_END:
      error = unexpected_end;
      goto syntax;

    default:
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Missing comma at %C")
	  == FAILURE)
	return FAILURE;
      goto format_item_1;
    }

optional_comma:
  /* Optional comma is a weird between state where we've just finished
     reading a colon, slash or P descriptor.  */
  t = format_lex ();
  switch (t)
    {
    case FMT_COMMA:
      break;

    case FMT_RPAREN:
      level--;
      if (level < 0)
	goto finished;
      goto between_desc;

    default:
      /* Assume that we have another format item.  */
      saved_token = t;
      break;
    }

  goto format_item;

extension_optional_comma:
  /* As a GNU extension, permit a missing comma after a string literal.  */
  t = format_lex ();
  switch (t)
    {
    case FMT_COMMA:
      break;

    case FMT_RPAREN:
      level--;
      if (level < 0)
	goto finished;
      goto between_desc;

    case FMT_COLON:
    case FMT_SLASH:
      goto optional_comma;

    case FMT_END:
      error = unexpected_end;
      goto syntax;

    default:
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Missing comma at %C")
	  == FAILURE)
	return FAILURE;
      saved_token = t;
      break;
    }

  goto format_item;

syntax:
  /* Something went wrong.  If the format we're checking is a string,
     generate a warning, since the program is correct.  If the format
     is in a FORMAT statement, this messes up parsing, which is an
     error.  */
  if (mode != MODE_STRING)
    gfc_error ("%s in format string at %C", error);
  else
    {
      gfc_warning ("%s in format string at %C", error);

      /* TODO: More elaborate measures are needed to show where a problem
         is within a format string that has been calculated.  */
    }

  rv = FAILURE;

finished:
  return rv;
}


/* Given an expression node that is a constant string, see if it looks
   like a format string.  */

static void
check_format_string (gfc_expr * e)
{

  mode = MODE_STRING;
  format_string = e->value.character.string;
  check_format ();
}


/************ Fortran 95 I/O statement matchers *************/

/* Match a FORMAT statement.  This amounts to actually parsing the
   format descriptors in order to correctly locate the end of the
   format string.  */

match
gfc_match_format (void)
{
  gfc_expr *e;
  locus start;

  if (gfc_statement_label == NULL)
    {
      gfc_error ("Missing format label at %C");
      return MATCH_ERROR;
    }
  gfc_gobble_whitespace ();

  mode = MODE_FORMAT;
  format_length = 0;

  start = gfc_current_locus;

  if (check_format () == FAILURE)
    return MATCH_ERROR;

  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_syntax_error (ST_FORMAT);
      return MATCH_ERROR;
    }

  /* The label doesn't get created until after the statement is done
     being matched, so we have to leave the string for later.  */

  gfc_current_locus = start;	/* Back to the beginning */

  new_st.loc = start;
  new_st.op = EXEC_NOP;

  e = gfc_get_expr();
  e->expr_type = EXPR_CONSTANT;
  e->ts.type = BT_CHARACTER;
  e->ts.kind = gfc_default_character_kind;
  e->where = start;
  e->value.character.string = format_string = gfc_getmem(format_length+1);
  e->value.character.length = format_length;
  gfc_statement_label->format = e;

  mode = MODE_COPY;
  check_format ();		/* Guaranteed to succeed */
  gfc_match_eos ();		/* Guaranteed to succeed */

  return MATCH_YES;
}


/* Match an expression I/O tag of some sort.  */

static match
match_etag (const io_tag * tag, gfc_expr ** v)
{
  gfc_expr *result;
  match m;

  m = gfc_match (tag->spec, &result);
  if (m != MATCH_YES)
    return m;

  if (*v != NULL)
    {
      gfc_error ("Duplicate %s specification at %C", tag->name);
      gfc_free_expr (result);
      return MATCH_ERROR;
    }

  *v = result;
  return MATCH_YES;
}


/* Match a variable I/O tag of some sort.  */

static match
match_vtag (const io_tag * tag, gfc_expr ** v)
{
  gfc_expr *result;
  match m;

  m = gfc_match (tag->spec, &result);
  if (m != MATCH_YES)
    return m;

  if (*v != NULL)
    {
      gfc_error ("Duplicate %s specification at %C", tag->name);
      gfc_free_expr (result);
      return MATCH_ERROR;
    }

  if (result->symtree->n.sym->attr.intent == INTENT_IN)
    {
      gfc_error ("Variable tag cannot be INTENT(IN) at %C");
      gfc_free_expr (result);
      return MATCH_ERROR;
    }

  if (gfc_pure (NULL) && gfc_impure_variable (result->symtree->n.sym))
    {
      gfc_error ("Variable tag cannot be assigned in PURE procedure at %C");
      gfc_free_expr (result);
      return MATCH_ERROR;
    }

  *v = result;
  return MATCH_YES;
}


/* Match I/O tags that cause variables to become redefined.  */

static match
match_out_tag(const io_tag *tag, gfc_expr **result)
{
  match m;

  m = match_vtag(tag, result);
  if (m == MATCH_YES)
    gfc_check_do_variable((*result)->symtree);

  return m;
}


/* Match a label I/O tag.  */

static match
match_ltag (const io_tag * tag, gfc_st_label ** label)
{
  match m;
  gfc_st_label *old;

  old = *label;
  m = gfc_match (tag->spec, label);
  if (m == MATCH_YES && old != 0)
    {
      gfc_error ("Duplicate %s label specification at %C", tag->name);
      return MATCH_ERROR;
    }

  return m;
}


/* Do expression resolution and type-checking on an expression tag.  */

static try
resolve_tag (const io_tag * tag, gfc_expr * e)
{

  if (e == NULL)
    return SUCCESS;

  if (gfc_resolve_expr (e) == FAILURE)
    return FAILURE;

  if (e->ts.type != tag->type && tag != &tag_format)
    {
      gfc_error ("%s tag at %L must be of type %s", tag->name,
		&e->where, gfc_basic_typename (tag->type));
      return FAILURE;
    }

  if (tag == &tag_format)
    {
      if (e->expr_type == EXPR_CONSTANT
	  && (e->ts.type != BT_CHARACTER
	      || e->ts.kind != gfc_default_character_kind))
	{
	  gfc_error ("Constant expression in FORMAT tag at %L must be "
		     "of type default CHARACTER", &e->where);
	  return FAILURE;
	}

      /* If e's rank is zero and e is not an element of an array, it should be
	 of integer or character type.  The integer variable should be
	 ASSIGNED.  */
      if (e->symtree == NULL || e->symtree->n.sym->as == NULL
		|| e->symtree->n.sym->as->rank == 0)
	{
	  if (e->ts.type != BT_CHARACTER && e->ts.type != BT_INTEGER)
	    {
	      gfc_error ("%s tag at %L must be of type %s or %s", tag->name,
			&e->where, gfc_basic_typename (BT_CHARACTER),
			gfc_basic_typename (BT_INTEGER));
	      return FAILURE;
	    }
	  else if (e->ts.type == BT_INTEGER && e->expr_type == EXPR_VARIABLE)
	    {
	      if (gfc_notify_std (GFC_STD_F95_DEL,
			"Obsolete: ASSIGNED variable in FORMAT tag at %L",
			&e->where) == FAILURE)
		return FAILURE;
	      if (e->symtree->n.sym->attr.assign != 1)
		{
		  gfc_error ("Variable '%s' at %L has not been assigned a "
			"format label", e->symtree->n.sym->name, &e->where);
		  return FAILURE;
		}
	    }
	  return SUCCESS;
	}
      else
	{
	  /* if rank is nonzero, we allow the type to be character under
	     GFC_STD_GNU and other type under GFC_STD_LEGACY. It may be
	     assigned an Hollerith constant.  */
	  if (e->ts.type == BT_CHARACTER)
	    {
	      if (gfc_notify_std (GFC_STD_GNU,
			"Extension: Character array in FORMAT tag at %L",
			&e->where) == FAILURE)
		return FAILURE;
	    }
	  else
	    {
	      if (gfc_notify_std (GFC_STD_LEGACY,
			"Extension: Non-character in FORMAT tag at %L",
			&e->where) == FAILURE)
		return FAILURE;
	    }
	  return SUCCESS;
	}
    }
  else
    {
      if (e->rank != 0)
	{
	  gfc_error ("%s tag at %L must be scalar", tag->name, &e->where);
	  return FAILURE;
	}
      if (tag == &tag_iomsg)
	{
	  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: IOMSG tag at %L",
			      &e->where) == FAILURE)
	    return FAILURE;
	}
    }

  return SUCCESS;
}


/* Match a single tag of an OPEN statement.  */

static match
match_open_element (gfc_open * open)
{
  match m;

  m = match_etag (&tag_unit, &open->unit);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_iomsg, &open->iomsg);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_iostat, &open->iostat);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_file, &open->file);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_status, &open->status);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_access, &open->access);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_form, &open->form);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_recl, &open->recl);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_blank, &open->blank);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_position, &open->position);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_action, &open->action);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_delim, &open->delim);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_pad, &open->pad);
  if (m != MATCH_NO)
    return m;
  m = match_ltag (&tag_err, &open->err);
  if (m != MATCH_NO)
    return m;

  return MATCH_NO;
}


/* Free the gfc_open structure and all the expressions it contains.  */

void
gfc_free_open (gfc_open * open)
{

  if (open == NULL)
    return;

  gfc_free_expr (open->unit);
  gfc_free_expr (open->iomsg);
  gfc_free_expr (open->iostat);
  gfc_free_expr (open->file);
  gfc_free_expr (open->status);
  gfc_free_expr (open->access);
  gfc_free_expr (open->form);
  gfc_free_expr (open->recl);
  gfc_free_expr (open->blank);
  gfc_free_expr (open->position);
  gfc_free_expr (open->action);
  gfc_free_expr (open->delim);
  gfc_free_expr (open->pad);

  gfc_free (open);
}


/* Resolve everything in a gfc_open structure.  */

try
gfc_resolve_open (gfc_open * open)
{

  RESOLVE_TAG (&tag_unit, open->unit);
  RESOLVE_TAG (&tag_iomsg, open->iomsg);
  RESOLVE_TAG (&tag_iostat, open->iostat);
  RESOLVE_TAG (&tag_file, open->file);
  RESOLVE_TAG (&tag_status, open->status);
  RESOLVE_TAG (&tag_e_access, open->access);
  RESOLVE_TAG (&tag_e_form, open->form);
  RESOLVE_TAG (&tag_e_recl, open->recl);

  RESOLVE_TAG (&tag_e_blank, open->blank);
  RESOLVE_TAG (&tag_e_position, open->position);
  RESOLVE_TAG (&tag_e_action, open->action);
  RESOLVE_TAG (&tag_e_delim, open->delim);
  RESOLVE_TAG (&tag_e_pad, open->pad);

  if (gfc_reference_st_label (open->err, ST_LABEL_TARGET) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Match an OPEN statement.  */

match
gfc_match_open (void)
{
  gfc_open *open;
  match m;

  m = gfc_match_char ('(');
  if (m == MATCH_NO)
    return m;

  open = gfc_getmem (sizeof (gfc_open));

  m = match_open_element (open);

  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    {
      m = gfc_match_expr (&open->unit);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;
    }

  for (;;)
    {
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;

      m = match_open_element (open);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

  if (gfc_match_eos () == MATCH_NO)
    goto syntax;

  if (gfc_pure (NULL))
    {
      gfc_error ("OPEN statement not allowed in PURE procedure at %C");
      goto cleanup;
    }

  new_st.op = EXEC_OPEN;
  new_st.ext.open = open;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_OPEN);

cleanup:
  gfc_free_open (open);
  return MATCH_ERROR;
}


/* Free a gfc_close structure an all its expressions.  */

void
gfc_free_close (gfc_close * close)
{

  if (close == NULL)
    return;

  gfc_free_expr (close->unit);
  gfc_free_expr (close->iomsg);
  gfc_free_expr (close->iostat);
  gfc_free_expr (close->status);

  gfc_free (close);
}


/* Match elements of a CLOSE statement.  */

static match
match_close_element (gfc_close * close)
{
  match m;

  m = match_etag (&tag_unit, &close->unit);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_status, &close->status);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_iomsg, &close->iomsg);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_iostat, &close->iostat);
  if (m != MATCH_NO)
    return m;
  m = match_ltag (&tag_err, &close->err);
  if (m != MATCH_NO)
    return m;

  return MATCH_NO;
}


/* Match a CLOSE statement.  */

match
gfc_match_close (void)
{
  gfc_close *close;
  match m;

  m = gfc_match_char ('(');
  if (m == MATCH_NO)
    return m;

  close = gfc_getmem (sizeof (gfc_close));

  m = match_close_element (close);

  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    {
      m = gfc_match_expr (&close->unit);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;
    }

  for (;;)
    {
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;

      m = match_close_element (close);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

  if (gfc_match_eos () == MATCH_NO)
    goto syntax;

  if (gfc_pure (NULL))
    {
      gfc_error ("CLOSE statement not allowed in PURE procedure at %C");
      goto cleanup;
    }

  new_st.op = EXEC_CLOSE;
  new_st.ext.close = close;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_CLOSE);

cleanup:
  gfc_free_close (close);
  return MATCH_ERROR;
}


/* Resolve everything in a gfc_close structure.  */

try
gfc_resolve_close (gfc_close * close)
{

  RESOLVE_TAG (&tag_unit, close->unit);
  RESOLVE_TAG (&tag_iomsg, close->iomsg);
  RESOLVE_TAG (&tag_iostat, close->iostat);
  RESOLVE_TAG (&tag_status, close->status);

  if (gfc_reference_st_label (close->err, ST_LABEL_TARGET) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Free a gfc_filepos structure.  */

void
gfc_free_filepos (gfc_filepos * fp)
{

  gfc_free_expr (fp->unit);
  gfc_free_expr (fp->iomsg);
  gfc_free_expr (fp->iostat);
  gfc_free (fp);
}


/* Match elements of a REWIND, BACKSPACE, ENDFILE, or FLUSH statement.  */

static match
match_file_element (gfc_filepos * fp)
{
  match m;

  m = match_etag (&tag_unit, &fp->unit);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_iomsg, &fp->iomsg);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_iostat, &fp->iostat);
  if (m != MATCH_NO)
    return m;
  m = match_ltag (&tag_err, &fp->err);
  if (m != MATCH_NO)
    return m;

  return MATCH_NO;
}


/* Match the second half of the file-positioning statements, REWIND,
   BACKSPACE, ENDFILE, or the FLUSH statement.  */

static match
match_filepos (gfc_statement st, gfc_exec_op op)
{
  gfc_filepos *fp;
  match m;

  fp = gfc_getmem (sizeof (gfc_filepos));

  if (gfc_match_char ('(') == MATCH_NO)
    {
      m = gfc_match_expr (&fp->unit);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      goto done;
    }

  m = match_file_element (fp);
  if (m == MATCH_ERROR)
    goto done;
  if (m == MATCH_NO)
    {
      m = gfc_match_expr (&fp->unit);
      if (m == MATCH_ERROR)
	goto done;
      if (m == MATCH_NO)
	goto syntax;
    }

  for (;;)
    {
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;

      m = match_file_element (fp);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

done:
  if (gfc_match_eos () != MATCH_YES)
    goto syntax;

  if (gfc_pure (NULL))
    {
      gfc_error ("%s statement not allowed in PURE procedure at %C",
		 gfc_ascii_statement (st));

      goto cleanup;
    }

  new_st.op = op;
  new_st.ext.filepos = fp;
  return MATCH_YES;

syntax:
  gfc_syntax_error (st);

cleanup:
  gfc_free_filepos (fp);
  return MATCH_ERROR;
}


try
gfc_resolve_filepos (gfc_filepos * fp)
{

  RESOLVE_TAG (&tag_unit, fp->unit);
  RESOLVE_TAG (&tag_iostat, fp->iostat);
  RESOLVE_TAG (&tag_iomsg, fp->iomsg);
  if (gfc_reference_st_label (fp->err, ST_LABEL_TARGET) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Match the file positioning statements: ENDFILE, BACKSPACE, REWIND,
   and the FLUSH statement.  */

match
gfc_match_endfile (void)
{

  return match_filepos (ST_END_FILE, EXEC_ENDFILE);
}

match
gfc_match_backspace (void)
{

  return match_filepos (ST_BACKSPACE, EXEC_BACKSPACE);
}

match
gfc_match_rewind (void)
{

  return match_filepos (ST_REWIND, EXEC_REWIND);
}

match
gfc_match_flush (void)
{
  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: FLUSH statement at %C") == FAILURE)
    return MATCH_ERROR;

  return match_filepos (ST_FLUSH, EXEC_FLUSH);
}

/******************** Data Transfer Statements *********************/

typedef enum
{ M_READ, M_WRITE, M_PRINT, M_INQUIRE }
io_kind;


/* Return a default unit number.  */

static gfc_expr *
default_unit (io_kind k)
{
  int unit;

  if (k == M_READ)
    unit = 5;
  else
    unit = 6;

  return gfc_int_expr (unit);
}


/* Match a unit specification for a data transfer statement.  */

static match
match_dt_unit (io_kind k, gfc_dt * dt)
{
  gfc_expr *e;

  if (gfc_match_char ('*') == MATCH_YES)
    {
      if (dt->io_unit != NULL)
	goto conflict;

      dt->io_unit = default_unit (k);
      return MATCH_YES;
    }

  if (gfc_match_expr (&e) == MATCH_YES)
    {
      if (dt->io_unit != NULL)
	{
	  gfc_free_expr (e);
	  goto conflict;
	}

      dt->io_unit = e;
      return MATCH_YES;
    }

  return MATCH_NO;

conflict:
  gfc_error ("Duplicate UNIT specification at %C");
  return MATCH_ERROR;
}


/* Match a format specification.  */

static match
match_dt_format (gfc_dt * dt)
{
  locus where;
  gfc_expr *e;
  gfc_st_label *label;

  where = gfc_current_locus;

  if (gfc_match_char ('*') == MATCH_YES)
    {
      if (dt->format_expr != NULL || dt->format_label != NULL)
	goto conflict;

      dt->format_label = &format_asterisk;
      return MATCH_YES;
    }

  if (gfc_match_st_label (&label) == MATCH_YES)
    {
      if (dt->format_expr != NULL || dt->format_label != NULL)
	{
	  gfc_free_st_label (label);
	  goto conflict;
	}

      if (gfc_reference_st_label (label, ST_LABEL_FORMAT) == FAILURE)
	return MATCH_ERROR;

      dt->format_label = label;
      return MATCH_YES;
    }

  if (gfc_match_expr (&e) == MATCH_YES)
    {
      if (dt->format_expr != NULL || dt->format_label != NULL)
	{
	  gfc_free_expr (e);
	  goto conflict;
	}
      dt->format_expr = e;
      return MATCH_YES;
    }

  gfc_current_locus = where;	/* The only case where we have to restore */

  return MATCH_NO;

conflict:
  gfc_error ("Duplicate format specification at %C");
  return MATCH_ERROR;
}


/* Traverse a namelist that is part of a READ statement to make sure
   that none of the variables in the namelist are INTENT(IN).  Returns
   nonzero if we find such a variable.  */

static int
check_namelist (gfc_symbol * sym)
{
  gfc_namelist *p;

  for (p = sym->namelist; p; p = p->next)
    if (p->sym->attr.intent == INTENT_IN)
      {
	gfc_error ("Symbol '%s' in namelist '%s' is INTENT(IN) at %C",
		   p->sym->name, sym->name);
	return 1;
      }

  return 0;
}


/* Match a single data transfer element.  */

static match
match_dt_element (io_kind k, gfc_dt * dt)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;

  if (gfc_match (" unit =") == MATCH_YES)
    {
      m = match_dt_unit (k, dt);
      if (m != MATCH_NO)
	return m;
    }

  if (gfc_match (" fmt =") == MATCH_YES)
    {
      m = match_dt_format (dt);
      if (m != MATCH_NO)
	return m;
    }

  if (gfc_match (" nml = %n", name) == MATCH_YES)
    {
      if (dt->namelist != NULL)
	{
	  gfc_error ("Duplicate NML specification at %C");
	  return MATCH_ERROR;
	}

      if (gfc_find_symbol (name, NULL, 1, &sym))
	return MATCH_ERROR;

      if (sym == NULL || sym->attr.flavor != FL_NAMELIST)
	{
	  gfc_error ("Symbol '%s' at %C must be a NAMELIST group name",
		     sym != NULL ? sym->name : name);
	  return MATCH_ERROR;
	}

      dt->namelist = sym;
      if (k == M_READ && check_namelist (sym))
	return MATCH_ERROR;

      return MATCH_YES;
    }

  m = match_etag (&tag_rec, &dt->rec);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_iomsg, &dt->iomsg);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_iostat, &dt->iostat);
  if (m != MATCH_NO)
    return m;
  m = match_ltag (&tag_err, &dt->err);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_advance, &dt->advance);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_size, &dt->size);
  if (m != MATCH_NO)
    return m;

  m = match_ltag (&tag_end, &dt->end);
  if (m == MATCH_YES)
    {
      if (k == M_WRITE)
       {
         gfc_error ("END tag at %C not allowed in output statement");
         return MATCH_ERROR;
       }
      dt->end_where = gfc_current_locus;
    }
  if (m != MATCH_NO)
    return m;

  m = match_ltag (&tag_eor, &dt->eor);
  if (m == MATCH_YES)
    dt->eor_where = gfc_current_locus;
  if (m != MATCH_NO)
    return m;

  return MATCH_NO;
}


/* Free a data transfer structure and everything below it.  */

void
gfc_free_dt (gfc_dt * dt)
{

  if (dt == NULL)
    return;

  gfc_free_expr (dt->io_unit);
  gfc_free_expr (dt->format_expr);
  gfc_free_expr (dt->rec);
  gfc_free_expr (dt->advance);
  gfc_free_expr (dt->iomsg);
  gfc_free_expr (dt->iostat);
  gfc_free_expr (dt->size);

  gfc_free (dt);
}


/* Resolve everything in a gfc_dt structure.  */

try
gfc_resolve_dt (gfc_dt * dt)
{
  gfc_expr *e;

  RESOLVE_TAG (&tag_format, dt->format_expr);
  RESOLVE_TAG (&tag_rec, dt->rec);
  RESOLVE_TAG (&tag_advance, dt->advance);
  RESOLVE_TAG (&tag_iomsg, dt->iomsg);
  RESOLVE_TAG (&tag_iostat, dt->iostat);
  RESOLVE_TAG (&tag_size, dt->size);

  e = dt->io_unit;
  if (gfc_resolve_expr (e) == SUCCESS
      && (e->ts.type != BT_INTEGER
	  && (e->ts.type != BT_CHARACTER
	      || e->expr_type != EXPR_VARIABLE)))
    {
      gfc_error
	("UNIT specification at %L must be an INTEGER expression or a "
	 "CHARACTER variable", &e->where);
      return FAILURE;
    }

  /* Sanity checks on data transfer statements.  */
  if (e->ts.type == BT_CHARACTER)
    {
      if (gfc_has_vector_index (e))
	{
	  gfc_error ("Internal unit with vector subscript at %L",
		     &e->where);
	  return FAILURE;
	}

      if (dt->rec != NULL)
	{
	  gfc_error ("REC tag at %L is incompatible with internal file",
		     &dt->rec->where);
	  return FAILURE;
	}

      if (dt->namelist != NULL)
	{
	  gfc_error ("Internal file at %L is incompatible with namelist",
		     &dt->io_unit->where);
	  return FAILURE;
	}

      if (dt->advance != NULL)
	{
	  gfc_error ("ADVANCE tag at %L is incompatible with internal file",
		     &dt->advance->where);
	  return FAILURE;
	}
    }

  if (dt->rec != NULL)
    {
      if (dt->end != NULL)
	{
	  gfc_error ("REC tag at %L is incompatible with END tag",
		     &dt->rec->where);
	  return FAILURE;
	}

      if (dt->format_label == &format_asterisk)
	{
	  gfc_error
	    ("END tag at %L is incompatible with list directed format (*)",
	     &dt->end_where);
	  return FAILURE;
	}

      if (dt->namelist != NULL)
	{
	  gfc_error ("REC tag at %L is incompatible with namelist",
		     &dt->rec->where);
	  return FAILURE;
	}
    }

  if (dt->advance != NULL && dt->format_label == &format_asterisk)
    {
      gfc_error ("ADVANCE tag at %L is incompatible with list directed "
		 "format (*)", &dt->advance->where);
      return FAILURE;
    }

  if (dt->eor != 0 && dt->advance == NULL)
    {
      gfc_error ("EOR tag at %L requires an ADVANCE tag", &dt->eor_where);
      return FAILURE;
    }

  if (dt->size != NULL && dt->advance == NULL)
    {
      gfc_error ("SIZE tag at %L requires an ADVANCE tag", &dt->size->where);
      return FAILURE;
    }

  /* TODO: Make sure the ADVANCE tag is 'yes' or 'no' if it is a string
     constant.  */

  if (gfc_reference_st_label (dt->err, ST_LABEL_TARGET) == FAILURE)
    return FAILURE;

  if (gfc_reference_st_label (dt->end, ST_LABEL_TARGET) == FAILURE)
    return FAILURE;

  if (gfc_reference_st_label (dt->eor, ST_LABEL_TARGET) == FAILURE)
    return FAILURE;

  /* Check the format label actually exists.  */
  if (dt->format_label && dt->format_label != &format_asterisk
      && dt->format_label->defined == ST_LABEL_UNKNOWN)
    {
      gfc_error ("FORMAT label %d at %L not defined", dt->format_label->value,
	         &dt->format_label->where);
      return FAILURE;
    }
  return SUCCESS;
}


/* Given an io_kind, return its name.  */

static const char *
io_kind_name (io_kind k)
{
  const char *name;

  switch (k)
    {
    case M_READ:
      name = "READ";
      break;
    case M_WRITE:
      name = "WRITE";
      break;
    case M_PRINT:
      name = "PRINT";
      break;
    case M_INQUIRE:
      name = "INQUIRE";
      break;
    default:
      gfc_internal_error ("io_kind_name(): bad I/O-kind");
    }

  return name;
}


/* Match an IO iteration statement of the form:

   ( [<IO element> ,] <IO element>, I = <expr>, <expr> [, <expr> ] )

   which is equivalent to a single IO element.  This function is
   mutually recursive with match_io_element().  */

static match match_io_element (io_kind k, gfc_code **);

static match
match_io_iterator (io_kind k, gfc_code ** result)
{
  gfc_code *head, *tail, *new;
  gfc_iterator *iter;
  locus old_loc;
  match m;
  int n;

  iter = NULL;
  head = NULL;
  old_loc = gfc_current_locus;

  if (gfc_match_char ('(') != MATCH_YES)
    return MATCH_NO;

  m = match_io_element (k, &head);
  tail = head;

  if (m != MATCH_YES || gfc_match_char (',') != MATCH_YES)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  /* Can't be anything but an IO iterator.  Build a list.  */
  iter = gfc_get_iterator ();

  for (n = 1;; n++)
    {
      m = gfc_match_iterator (iter, 0);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_YES)
	{
	  gfc_check_do_variable (iter->var->symtree);
	  break;
	}

      m = match_io_element (k, &new);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	{
	  if (n > 2)
	    goto syntax;
	  goto cleanup;
	}

      tail = gfc_append_code (tail, new);

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

  new = gfc_get_code ();
  new->op = EXEC_DO;
  new->ext.iterator = iter;

  new->block = gfc_get_code ();
  new->block->op = EXEC_DO;
  new->block->next = head;

  *result = new;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in I/O iterator at %C");
  m = MATCH_ERROR;

cleanup:
  gfc_free_iterator (iter, 1);
  gfc_free_statements (head);
  gfc_current_locus = old_loc;
  return m;
}


/* Match a single element of an IO list, which is either a single
   expression or an IO Iterator.  */

static match
match_io_element (io_kind k, gfc_code ** cpp)
{
  gfc_expr *expr;
  gfc_code *cp;
  match m;

  expr = NULL;

  m = match_io_iterator (k, cpp);
  if (m == MATCH_YES)
    return MATCH_YES;

  if (k == M_READ)
    {
      m = gfc_match_variable (&expr, 0);
      if (m == MATCH_NO)
	gfc_error ("Expected variable in READ statement at %C");
    }
  else
    {
      m = gfc_match_expr (&expr);
      if (m == MATCH_NO)
	gfc_error ("Expected expression in %s statement at %C",
		   io_kind_name (k));
    }

  if (m == MATCH_YES)
    switch (k)
      {
      case M_READ:
	if (expr->symtree->n.sym->attr.intent == INTENT_IN)
	  {
	    gfc_error
	      ("Variable '%s' in input list at %C cannot be INTENT(IN)",
	       expr->symtree->n.sym->name);
	    m = MATCH_ERROR;
	  }

	if (gfc_pure (NULL)
	    && gfc_impure_variable (expr->symtree->n.sym)
	    && current_dt->io_unit->ts.type == BT_CHARACTER)
	  {
	    gfc_error ("Cannot read to variable '%s' in PURE procedure at %C",
		       expr->symtree->n.sym->name);
	    m = MATCH_ERROR;
	  }

	if (gfc_check_do_variable (expr->symtree))
	  m = MATCH_ERROR;

	break;

      case M_WRITE:
	if (current_dt->io_unit->ts.type == BT_CHARACTER
	    && gfc_pure (NULL)
	    && current_dt->io_unit->expr_type == EXPR_VARIABLE
	    && gfc_impure_variable (current_dt->io_unit->symtree->n.sym))
	  {
	    gfc_error
	      ("Cannot write to internal file unit '%s' at %C inside a "
	       "PURE procedure", current_dt->io_unit->symtree->n.sym->name);
	    m = MATCH_ERROR;
	  }

	break;

      default:
	break;
      }

  if (m != MATCH_YES)
    {
      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  cp = gfc_get_code ();
  cp->op = EXEC_TRANSFER;
  cp->expr = expr;

  *cpp = cp;
  return MATCH_YES;
}


/* Match an I/O list, building gfc_code structures as we go.  */

static match
match_io_list (io_kind k, gfc_code ** head_p)
{
  gfc_code *head, *tail, *new;
  match m;

  *head_p = head = tail = NULL;
  if (gfc_match_eos () == MATCH_YES)
    return MATCH_YES;

  for (;;)
    {
      m = match_io_element (k, &new);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      tail = gfc_append_code (tail, new);
      if (head == NULL)
	head = new;

      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  *head_p = head;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in %s statement at %C", io_kind_name (k));

cleanup:
  gfc_free_statements (head);
  return MATCH_ERROR;
}


/* Attach the data transfer end node.  */

static void
terminate_io (gfc_code * io_code)
{
  gfc_code *c;

  if (io_code == NULL)
    io_code = new_st.block;

  c = gfc_get_code ();
  c->op = EXEC_DT_END;

  /* Point to structure that is already there */
  c->ext.dt = new_st.ext.dt;
  gfc_append_code (io_code, c);
}


/* Match a READ, WRITE or PRINT statement.  */

static match
match_io (io_kind k)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_code *io_code;
  gfc_symbol *sym;
  gfc_expr *expr;
  int comma_flag, c;
  locus where;
  gfc_dt *dt;
  match m;

  comma_flag = 0;
  current_dt = dt = gfc_getmem (sizeof (gfc_dt));
  if (gfc_match_char ('(') == MATCH_NO)
    {
      where = gfc_current_locus;
      if (k == M_WRITE)
	goto syntax;
      else if (k == M_PRINT)
	{
	  /* Treat the non-standard case of PRINT namelist.  */
	  if ((gfc_current_form == FORM_FIXED || gfc_peek_char () == ' ')
	      && gfc_match_name (name) == MATCH_YES)
	    {
	      gfc_find_symbol (name, NULL, 1, &sym);
	      if (sym && sym->attr.flavor == FL_NAMELIST)
		{
		  if (gfc_notify_std (GFC_STD_GNU, "PRINT namelist at "
				      "%C is an extension") == FAILURE)
		    {
		      m = MATCH_ERROR;
		      goto cleanup;
		    }
		  if (gfc_match_eos () == MATCH_NO)
		    {
		      gfc_error ("Namelist followed by I/O list at %C");
		      m = MATCH_ERROR;
		      goto cleanup;
		    }

		  dt->io_unit = default_unit (k);
		  dt->namelist = sym;
		  goto get_io_list;
		}
	      else
		gfc_current_locus = where;
	    }
	}

      if (gfc_current_form == FORM_FREE)
	{
	  c = gfc_peek_char();
	  if (c != ' ' && c != '*' && c != '\'' && c != '"')
	    {
	      m = MATCH_NO;
	      goto cleanup;
	    }
	}

      m = match_dt_format (dt);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      comma_flag = 1;
      dt->io_unit = default_unit (k);
      goto get_io_list;
    }

  /* Match a control list */
  if (match_dt_element (k, dt) == MATCH_YES)
    goto next;
  if (match_dt_unit (k, dt) != MATCH_YES)
    goto loop;

  if (gfc_match_char (')') == MATCH_YES)
    goto get_io_list;
  if (gfc_match_char (',') != MATCH_YES)
    goto syntax;

  m = match_dt_element (k, dt);
  if (m == MATCH_YES)
    goto next;
  if (m == MATCH_ERROR)
    goto cleanup;

  m = match_dt_format (dt);
  if (m == MATCH_YES)
    goto next;
  if (m == MATCH_ERROR)
    goto cleanup;

  where = gfc_current_locus;

  m = gfc_match_name (name);
  if (m == MATCH_YES)
    {
      gfc_find_symbol (name, NULL, 1, &sym);
      if (sym && sym->attr.flavor == FL_NAMELIST)
	{
	  dt->namelist = sym;
	  if (k == M_READ && check_namelist (sym))
	    {
	      m = MATCH_ERROR;
	      goto cleanup;
	    }
	  goto next;
	}
    }

  gfc_current_locus = where;

  goto loop;			/* No matches, try regular elements */

next:
  if (gfc_match_char (')') == MATCH_YES)
    goto get_io_list;
  if (gfc_match_char (',') != MATCH_YES)
    goto syntax;

loop:
  for (;;)
    {
      m = match_dt_element (k, dt);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto cleanup;

      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

get_io_list:
  /* Optional leading comma (non-standard).  */
  if (!comma_flag
      && gfc_match_char (',') == MATCH_YES
      && k == M_WRITE
      && gfc_notify_std (GFC_STD_GNU, "Extension: Comma before output "
			 "item list at %C is an extension") == FAILURE)
    return MATCH_ERROR;

  io_code = NULL;
  if (gfc_match_eos () != MATCH_YES)
    {
      if (comma_flag && gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Expected comma in I/O list at %C");
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      m = match_io_list (k, &io_code);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

  /* A full IO statement has been matched.  */
  if (dt->io_unit->expr_type == EXPR_VARIABLE
      && k == M_WRITE
      && dt->io_unit->ts.type == BT_CHARACTER
      && dt->io_unit->symtree->n.sym->attr.intent == INTENT_IN)
    {
      gfc_error ("Internal file '%s' at %L is INTENT(IN)",
		 dt->io_unit->symtree->n.sym->name, &dt->io_unit->where);
      m = MATCH_ERROR;
      goto cleanup;
    }

  expr = dt->format_expr;

  if (expr != NULL && expr->expr_type == EXPR_CONSTANT)
    check_format_string (expr);

  if (gfc_pure (NULL)
      && (k == M_READ || k == M_WRITE)
      && dt->io_unit->ts.type != BT_CHARACTER)
    {
      gfc_error
	("io-unit in %s statement at %C must be an internal file in a "
	 "PURE procedure", io_kind_name (k));
      m = MATCH_ERROR;
      goto cleanup;
    }

  new_st.op = (k == M_READ) ? EXEC_READ : EXEC_WRITE;
  new_st.ext.dt = dt;
  new_st.block = gfc_get_code ();
  new_st.block->op = new_st.op;
  new_st.block->next = io_code;

  terminate_io (io_code);

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in %s statement at %C", io_kind_name (k));
  m = MATCH_ERROR;

cleanup:
  gfc_free_dt (dt);
  return m;
}


match
gfc_match_read (void)
{
  return match_io (M_READ);
}

match
gfc_match_write (void)
{
  return match_io (M_WRITE);
}

match
gfc_match_print (void)
{
  match m;

  m = match_io (M_PRINT);
  if (m != MATCH_YES)
    return m;

  if (gfc_pure (NULL))
    {
      gfc_error ("PRINT statement at %C not allowed within PURE procedure");
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* Free a gfc_inquire structure.  */

void
gfc_free_inquire (gfc_inquire * inquire)
{

  if (inquire == NULL)
    return;

  gfc_free_expr (inquire->unit);
  gfc_free_expr (inquire->file);
  gfc_free_expr (inquire->iomsg);
  gfc_free_expr (inquire->iostat);
  gfc_free_expr (inquire->exist);
  gfc_free_expr (inquire->opened);
  gfc_free_expr (inquire->number);
  gfc_free_expr (inquire->named);
  gfc_free_expr (inquire->name);
  gfc_free_expr (inquire->access);
  gfc_free_expr (inquire->sequential);
  gfc_free_expr (inquire->direct);
  gfc_free_expr (inquire->form);
  gfc_free_expr (inquire->formatted);
  gfc_free_expr (inquire->unformatted);
  gfc_free_expr (inquire->recl);
  gfc_free_expr (inquire->nextrec);
  gfc_free_expr (inquire->blank);
  gfc_free_expr (inquire->position);
  gfc_free_expr (inquire->action);
  gfc_free_expr (inquire->read);
  gfc_free_expr (inquire->write);
  gfc_free_expr (inquire->readwrite);
  gfc_free_expr (inquire->delim);
  gfc_free_expr (inquire->pad);
  gfc_free_expr (inquire->iolength);

  gfc_free (inquire);
}


/* Match an element of an INQUIRE statement.  */

#define RETM   if (m != MATCH_NO) return m;

static match
match_inquire_element (gfc_inquire * inquire)
{
  match m;

  m = match_etag (&tag_unit, &inquire->unit);
  RETM m = match_etag (&tag_file, &inquire->file);
  RETM m = match_ltag (&tag_err, &inquire->err);
  RETM m = match_out_tag (&tag_iomsg, &inquire->iomsg);
  RETM m = match_out_tag (&tag_iostat, &inquire->iostat);
  RETM m = match_vtag (&tag_exist, &inquire->exist);
  RETM m = match_vtag (&tag_opened, &inquire->opened);
  RETM m = match_vtag (&tag_named, &inquire->named);
  RETM m = match_vtag (&tag_name, &inquire->name);
  RETM m = match_out_tag (&tag_number, &inquire->number);
  RETM m = match_vtag (&tag_s_access, &inquire->access);
  RETM m = match_vtag (&tag_sequential, &inquire->sequential);
  RETM m = match_vtag (&tag_direct, &inquire->direct);
  RETM m = match_vtag (&tag_s_form, &inquire->form);
  RETM m = match_vtag (&tag_formatted, &inquire->formatted);
  RETM m = match_vtag (&tag_unformatted, &inquire->unformatted);
  RETM m = match_out_tag (&tag_s_recl, &inquire->recl);
  RETM m = match_out_tag (&tag_nextrec, &inquire->nextrec);
  RETM m = match_vtag (&tag_s_blank, &inquire->blank);
  RETM m = match_vtag (&tag_s_position, &inquire->position);
  RETM m = match_vtag (&tag_s_action, &inquire->action);
  RETM m = match_vtag (&tag_read, &inquire->read);
  RETM m = match_vtag (&tag_write, &inquire->write);
  RETM m = match_vtag (&tag_readwrite, &inquire->readwrite);
  RETM m = match_vtag (&tag_s_delim, &inquire->delim);
  RETM m = match_vtag (&tag_s_pad, &inquire->pad);
  RETM m = match_vtag (&tag_iolength, &inquire->iolength);
  RETM return MATCH_NO;
}

#undef RETM


match
gfc_match_inquire (void)
{
  gfc_inquire *inquire;
  gfc_code *code;
  match m;
  locus loc;

  m = gfc_match_char ('(');
  if (m == MATCH_NO)
    return m;

  inquire = gfc_getmem (sizeof (gfc_inquire));

  loc = gfc_current_locus;

  m = match_inquire_element (inquire);
  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    {
      m = gfc_match_expr (&inquire->unit);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

  /* See if we have the IOLENGTH form of the inquire statement.  */
  if (inquire->iolength != NULL)
    {
      if (gfc_match_char (')') != MATCH_YES)
	goto syntax;

      m = match_io_list (M_INQUIRE, &code);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      new_st.op = EXEC_IOLENGTH;
      new_st.expr = inquire->iolength;
      new_st.ext.inquire = inquire;

      if (gfc_pure (NULL))
	{
	  gfc_free_statements (code);
	  gfc_error ("INQUIRE statement not allowed in PURE procedure at %C");
	  return MATCH_ERROR;
	}

      new_st.block = gfc_get_code ();
      new_st.block->op = EXEC_IOLENGTH;
      terminate_io (code);
      new_st.block->next = code;
      return MATCH_YES;
    }

  /* At this point, we have the non-IOLENGTH inquire statement.  */
  for (;;)
    {
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;

      m = match_inquire_element (inquire);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      if (inquire->iolength != NULL)
	{
	  gfc_error ("IOLENGTH tag invalid in INQUIRE statement at %C");
	  goto cleanup;
	}
    }

  if (gfc_match_eos () != MATCH_YES)
    goto syntax;

  if (inquire->unit != NULL && inquire->file != NULL)
    {
      gfc_error ("INQUIRE statement at %L cannot contain both FILE and"
		 " UNIT specifiers", &loc);
      goto cleanup;
    }

  if (inquire->unit == NULL && inquire->file == NULL)
    {
      gfc_error ("INQUIRE statement at %L requires either FILE or"
		     " UNIT specifier", &loc);
      goto cleanup;
    }

  if (gfc_pure (NULL))
    {
      gfc_error ("INQUIRE statement not allowed in PURE procedure at %C");
      goto cleanup;
    }

  new_st.op = EXEC_INQUIRE;
  new_st.ext.inquire = inquire;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_INQUIRE);

cleanup:
  gfc_free_inquire (inquire);
  return MATCH_ERROR;
}


/* Resolve everything in a gfc_inquire structure.  */

try
gfc_resolve_inquire (gfc_inquire * inquire)
{

  RESOLVE_TAG (&tag_unit, inquire->unit);
  RESOLVE_TAG (&tag_file, inquire->file);
  RESOLVE_TAG (&tag_iomsg, inquire->iomsg);
  RESOLVE_TAG (&tag_iostat, inquire->iostat);
  RESOLVE_TAG (&tag_exist, inquire->exist);
  RESOLVE_TAG (&tag_opened, inquire->opened);
  RESOLVE_TAG (&tag_number, inquire->number);
  RESOLVE_TAG (&tag_named, inquire->named);
  RESOLVE_TAG (&tag_name, inquire->name);
  RESOLVE_TAG (&tag_s_access, inquire->access);
  RESOLVE_TAG (&tag_sequential, inquire->sequential);
  RESOLVE_TAG (&tag_direct, inquire->direct);
  RESOLVE_TAG (&tag_s_form, inquire->form);
  RESOLVE_TAG (&tag_formatted, inquire->formatted);
  RESOLVE_TAG (&tag_unformatted, inquire->unformatted);
  RESOLVE_TAG (&tag_s_recl, inquire->recl);
  RESOLVE_TAG (&tag_nextrec, inquire->nextrec);
  RESOLVE_TAG (&tag_s_blank, inquire->blank);
  RESOLVE_TAG (&tag_s_position, inquire->position);
  RESOLVE_TAG (&tag_s_action, inquire->action);
  RESOLVE_TAG (&tag_read, inquire->read);
  RESOLVE_TAG (&tag_write, inquire->write);
  RESOLVE_TAG (&tag_readwrite, inquire->readwrite);
  RESOLVE_TAG (&tag_s_delim, inquire->delim);
  RESOLVE_TAG (&tag_s_pad, inquire->pad);
  RESOLVE_TAG (&tag_iolength, inquire->iolength);

  if (gfc_reference_st_label (inquire->err, ST_LABEL_TARGET) == FAILURE)
    return FAILURE;

  return SUCCESS;
}
