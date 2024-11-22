/* Deal with I/O statements & related stuff.
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
#include "match.h"
#include "parse.h"
#include "constructor.h"

gfc_st_label
format_asterisk = {0, NULL, NULL, -1, ST_LABEL_FORMAT, ST_LABEL_FORMAT, NULL,
		   0, {NULL, NULL}, NULL};

typedef struct
{
  const char *name, *spec, *value;
  bt type;
}
io_tag;

static const io_tag
	tag_readonly	= {"READONLY", " readonly", NULL, BT_UNKNOWN },
	tag_shared	= {"SHARE", " shared", NULL, BT_UNKNOWN },
	tag_noshared	= {"SHARE", " noshared", NULL, BT_UNKNOWN },
	tag_e_share	= {"SHARE", " share =", " %e", BT_CHARACTER },
	tag_v_share	= {"SHARE", " share =", " %v", BT_CHARACTER },
	tag_cc		= {"CARRIAGECONTROL", " carriagecontrol =", " %e",
			   BT_CHARACTER },
	tag_v_cc	= {"CARRIAGECONTROL", " carriagecontrol =", " %v",
			   BT_CHARACTER },
	tag_file	= {"FILE", " file =", " %e", BT_CHARACTER },
	tag_status	= {"STATUS", " status =", " %e", BT_CHARACTER},
	tag_e_access	= {"ACCESS", " access =", " %e", BT_CHARACTER},
	tag_e_form	= {"FORM", " form =", " %e", BT_CHARACTER},
	tag_e_recl	= {"RECL", " recl =", " %e", BT_INTEGER},
	tag_e_blank	= {"BLANK", " blank =", " %e", BT_CHARACTER},
	tag_e_position	= {"POSITION", " position =", " %e", BT_CHARACTER},
	tag_e_action	= {"ACTION", " action =", " %e", BT_CHARACTER},
	tag_e_delim	= {"DELIM", " delim =", " %e", BT_CHARACTER},
	tag_e_pad	= {"PAD", " pad =", " %e", BT_CHARACTER},
	tag_e_decimal	= {"DECIMAL", " decimal =", " %e", BT_CHARACTER},
	tag_e_encoding	= {"ENCODING", " encoding =", " %e", BT_CHARACTER},
	tag_e_async	= {"ASYNCHRONOUS", " asynchronous =", " %e", BT_CHARACTER},
	tag_e_round	= {"ROUND", " round =", " %e", BT_CHARACTER},
	tag_e_sign	= {"SIGN", " sign =", " %e", BT_CHARACTER},
	tag_unit	= {"UNIT", " unit =", " %e", BT_INTEGER},
	tag_advance	= {"ADVANCE", " advance =", " %e", BT_CHARACTER},
	tag_rec		= {"REC", " rec =", " %e", BT_INTEGER},
	tag_spos	= {"POSITION", " pos =", " %e", BT_INTEGER},
	tag_format	= {"FORMAT", NULL, NULL, BT_CHARACTER},
	tag_iomsg	= {"IOMSG", " iomsg =", " %e", BT_CHARACTER},
	tag_iostat	= {"IOSTAT", " iostat =", " %v", BT_INTEGER},
	tag_size	= {"SIZE", " size =", " %v", BT_INTEGER},
	tag_exist	= {"EXIST", " exist =", " %v", BT_LOGICAL},
	tag_opened	= {"OPENED", " opened =", " %v", BT_LOGICAL},
	tag_named	= {"NAMED", " named =", " %v", BT_LOGICAL},
	tag_name	= {"NAME", " name =", " %v", BT_CHARACTER},
	tag_number	= {"NUMBER", " number =", " %v", BT_INTEGER},
	tag_s_access	= {"ACCESS", " access =", " %v", BT_CHARACTER},
	tag_sequential	= {"SEQUENTIAL", " sequential =", " %v", BT_CHARACTER},
	tag_direct	= {"DIRECT", " direct =", " %v", BT_CHARACTER},
	tag_s_form	= {"FORM", " form =", " %v", BT_CHARACTER},
	tag_formatted	= {"FORMATTED", " formatted =", " %v", BT_CHARACTER},
	tag_unformatted	= {"UNFORMATTED", " unformatted =", " %v", BT_CHARACTER},
	tag_s_recl	= {"RECL", " recl =", " %v", BT_INTEGER},
	tag_nextrec	= {"NEXTREC", " nextrec =", " %v", BT_INTEGER},
	tag_s_blank	= {"BLANK", " blank =", " %v", BT_CHARACTER},
	tag_s_position	= {"POSITION", " position =", " %v", BT_CHARACTER},
	tag_s_action	= {"ACTION", " action =", " %v", BT_CHARACTER},
	tag_read	= {"READ", " read =", " %v", BT_CHARACTER},
	tag_write	= {"WRITE", " write =", " %v", BT_CHARACTER},
	tag_readwrite	= {"READWRITE", " readwrite =", " %v", BT_CHARACTER},
	tag_s_delim	= {"DELIM", " delim =", " %v", BT_CHARACTER},
	tag_s_pad	= {"PAD", " pad =", " %v", BT_CHARACTER},
	tag_s_decimal	= {"DECIMAL", " decimal =", " %v", BT_CHARACTER},
	tag_s_encoding	= {"ENCODING", " encoding =", " %v", BT_CHARACTER},
	tag_s_async	= {"ASYNCHRONOUS", " asynchronous =", " %v", BT_CHARACTER},
	tag_s_round	= {"ROUND", " round =", " %v", BT_CHARACTER},
	tag_s_sign	= {"SIGN", " sign =", " %v", BT_CHARACTER},
	tag_iolength	= {"IOLENGTH", " iolength =", " %v", BT_INTEGER},
	tag_convert     = {"CONVERT", " convert =", " %e", BT_CHARACTER},
	tag_strm_out    = {"POS", " pos =", " %v", BT_INTEGER},
	tag_err		= {"ERR", " err =", " %l", BT_UNKNOWN},
	tag_end		= {"END", " end =", " %l", BT_UNKNOWN},
	tag_eor		= {"EOR", " eor =", " %l", BT_UNKNOWN},
	tag_id		= {"ID", " id =", " %v", BT_INTEGER},
	tag_pending	= {"PENDING", " pending =", " %v", BT_LOGICAL},
	tag_newunit	= {"NEWUNIT", " newunit =", " %v", BT_INTEGER},
	tag_s_iqstream	= {"STREAM", " stream =", " %v", BT_CHARACTER};

static gfc_dt *current_dt;

#define RESOLVE_TAG(x, y) if (!resolve_tag (x, y)) return false;

/**************** Fortran 95 FORMAT parser  *****************/

/* FORMAT tokens returned by format_lex().  */
enum format_token
{
  FMT_NONE, FMT_UNKNOWN, FMT_SIGNED_INT, FMT_ZERO, FMT_POSINT, FMT_PERIOD,
  FMT_COMMA, FMT_COLON, FMT_SLASH, FMT_DOLLAR, FMT_LPAREN,
  FMT_RPAREN, FMT_X, FMT_SIGN, FMT_BLANK, FMT_CHAR, FMT_P, FMT_IBOZ, FMT_F,
  FMT_E, FMT_EN, FMT_ES, FMT_G, FMT_L, FMT_A, FMT_D, FMT_H, FMT_END,
  FMT_ERROR, FMT_DC, FMT_DP, FMT_T, FMT_TR, FMT_TL, FMT_STAR, FMT_RC,
  FMT_RD, FMT_RN, FMT_RP, FMT_RU, FMT_RZ, FMT_DT
};

/* Local variables for checking format strings.  The saved_token is
   used to back up by a single format token during the parsing
   process.  */
static gfc_char_t *format_string;
static int format_string_pos;
static int format_length, use_last_char;
static char error_element;
static locus format_locus;

static format_token saved_token;

static enum
{ MODE_STRING, MODE_FORMAT, MODE_COPY }
mode;


/* Return the next character in the format string.  */

static char
next_char (gfc_instring in_string)
{
  static gfc_char_t c;

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
    }

  if (flag_backslash && c == '\\')
    {
      locus old_locus = gfc_current_locus;

      if (gfc_match_special_char (&c) == MATCH_NO)
	gfc_current_locus = old_locus;

      if (!(gfc_option.allow_std & GFC_STD_GNU) && !inhibit_warnings)
	gfc_warning (0, "Extension: backslash character at %C");
    }

  if (mode == MODE_COPY)
    *format_string++ = c;

  if (mode != MODE_STRING)
    format_locus = gfc_current_locus;

  format_string_pos++;

  c = gfc_wide_toupper (c);
  return c;
}


/* Back up one character position.  Only works once.  */

static void
unget_char (void)
{
  use_last_char = 1;
}

/* Eat up the spaces and return a character.  */

static char
next_char_not_space ()
{
  char c;
  do
    {
      error_element = c = next_char (NONSTRING);
      if (c == '\t')
	gfc_warning (OPT_Wtabs, "Nonconforming tab character in format at %C");
    }
  while (gfc_is_whitespace (c));
  return c;
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

  c = next_char_not_space ();

  negative_flag = 0;
  switch (c)
    {
    case '-':
      negative_flag = 1;
      /* Falls through.  */

    case '+':
      c = next_char_not_space ();
      if (!ISDIGIT (c))
	{
	  token = FMT_UNKNOWN;
	  break;
	}

      value = c - '0';

      do
	{
	  c = next_char_not_space ();
	  if (ISDIGIT (c))
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
	  c = next_char_not_space ();
	  if (ISDIGIT (c))
	    {
	      value = 10 * value + c - '0';
	      if (c != '0')
		zflag = 0;
	    }
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
      c = next_char_not_space ();
      switch (c)
	{
	case 'L':
	  token = FMT_TL;
	  break;
	case 'R':
	  token = FMT_TR;
	  break;
	default:
	  token = FMT_T;
	  unget_char ();
	}
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
      c = next_char_not_space ();
      if (c != 'P' && c != 'S')
	unget_char ();

      token = FMT_SIGN;
      break;

    case 'B':
      c = next_char_not_space ();
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
	  c = next_char (INSTRING_WARN);
	  if (c == '\0')
	    {
	      token = FMT_END;
	      break;
	    }

	  if (c == delim)
	    {
	      c = next_char (NONSTRING);

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
      c = next_char_not_space ();
      if (c == 'N' )
	token = FMT_EN;
      else if (c == 'S')
        token = FMT_ES;
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
      c = next_char_not_space ();
      if (c == 'P')
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "DP format "
			       "specifier not allowed at %C"))
	    return FMT_ERROR;
	  token = FMT_DP;
	}
      else if (c == 'C')
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "DC format "
			       "specifier not allowed at %C"))
	    return FMT_ERROR;
	  token = FMT_DC;
	}
      else if (c == 'T')
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "Fortran 2003: DT format "
	      "specifier not allowed at %C"))
	    return FMT_ERROR;
	  token = FMT_DT;
	  c = next_char_not_space ();
	  if (c == '\'' || c == '"')
	    {
	      delim = c;
	      value = 0;

	      for (;;)
		{
		  c = next_char (INSTRING_WARN);
		  if (c == '\0')
		    {
		      token = FMT_END;
		      break;
		    }

		  if (c == delim)
		    {
		      c = next_char (NONSTRING);
		      if (c == '\0')
			{
			  token = FMT_END;
			  break;
			}
		      if (c == '/')
			{
			  token = FMT_SLASH;
			  break;
			}
		      if (c == delim)
			continue;
		      unget_char ();
		      break;
		    }
		}
	    }
	  else if (c == '/')
	    {
	      token = FMT_SLASH;
	      break;
	    }
	  else
	    unget_char ();
	}
      else
	{
	  token = FMT_D;
	  unget_char ();
	}
      break;

    case 'R':
      c = next_char_not_space ();
      switch (c)
	{
	case 'C':
	  token = FMT_RC;
	  break;
	case 'D':
	  token = FMT_RD;
	  break;
	case 'N':
	  token = FMT_RN;
	  break;
	case 'P':
	  token = FMT_RP;
	  break;
	case 'U':
	  token = FMT_RU;
	  break;
	case 'Z':
	  token = FMT_RZ;
	  break;
	default:
	  token = FMT_UNKNOWN;
	  unget_char ();
	  break;
	}
      break;

    case '\0':
      token = FMT_END;
      break;

    case '*':
      token = FMT_STAR;
      break;

    default:
      token = FMT_UNKNOWN;
      break;
    }

  return token;
}


static const char *
token_to_string (format_token t)
{
  switch (t)
    {
      case FMT_D:
	return "D";
      case FMT_G:
	return "G";
      case FMT_E:
	return "E";
      case FMT_EN:
	return "EN";
      case FMT_ES:
	return "ES";
      default:
        return "";
    }
}

/* Check a format statement.  The format string, either from a FORMAT
   statement or a constant in an I/O statement has already been parsed
   by itself, and we are checking it for validity.  The dual origin
   means that the warning message is a little less than great.  */

static bool
check_format (bool is_input)
{
  const char *posint_required
    = G_("Positive width required in format string at %L");
  const char *nonneg_required
    = G_("Nonnegative width required in format string at %L");
  const char *unexpected_element
    = G_("Unexpected element %qc in format string at %L");
  const char *unexpected_end
    = G_("Unexpected end of format string in format string at %L");
  const char *zero_width
    = G_("Zero width in format descriptor in format string at %L");

  const char *error = NULL;
  format_token t, u;
  int level;
  int repeat;
  bool rv;

  use_last_char = 0;
  saved_token = FMT_NONE;
  level = 0;
  repeat = 0;
  rv = true;
  format_string_pos = 0;

  t = format_lex ();
  if (t == FMT_ERROR)
    goto fail;
  if (t != FMT_LPAREN)
    {
      error = G_("Missing leading left parenthesis in format string at %L");
      goto syntax;
    }

  t = format_lex ();
  if (t == FMT_ERROR)
    goto fail;
  if (t == FMT_RPAREN)
    goto finished;		/* Empty format is legal */
  saved_token = t;

format_item:
  /* In this state, the next thing has to be a format item.  */
  t = format_lex ();
  if (t == FMT_ERROR)
    goto fail;
format_item_1:
  switch (t)
    {
    case FMT_STAR:
      repeat = -1;
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t == FMT_LPAREN)
	{
	  level++;
	  goto format_item;
	}
      error = G_("Left parenthesis required after %<*%> in format string "
		 "at %L");
      goto syntax;

    case FMT_POSINT:
      repeat = value;
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
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
    case FMT_ZERO:
      /* Signed integer can only precede a P format.  */
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t != FMT_P)
	{
	  error = G_("Expected P edit descriptor in format string at %L");
	  goto syntax;
	}

      goto data_desc;

    case FMT_P:
      /* P requires a prior number.  */
      error = G_("P descriptor requires leading scale factor in format "
		 "string at %L");
      goto syntax;

    case FMT_X:
      /* X requires a prior number if we're being pedantic.  */
      if (mode != MODE_FORMAT)
	format_locus.nextc += format_string_pos;
      if (!gfc_notify_std (GFC_STD_GNU, "X descriptor requires leading "
			   "space count at %L", &format_locus))
	return false;
      goto between_desc;

    case FMT_SIGN:
    case FMT_BLANK:
    case FMT_DP:
    case FMT_DC:
    case FMT_RC:
    case FMT_RD:
    case FMT_RN:
    case FMT_RP:
    case FMT_RU:
    case FMT_RZ:
      goto between_desc;

    case FMT_CHAR:
      goto extension_optional_comma;

    case FMT_COLON:
    case FMT_SLASH:
      goto optional_comma;

    case FMT_DOLLAR:
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;

      if (!gfc_notify_std (GFC_STD_GNU, "$ descriptor at %L", &format_locus))
	return false;
      if (t != FMT_RPAREN || level > 0)
	{
	  gfc_warning (0, "$ should be the last specifier in format at %L",
		       &format_locus);
	  goto optional_comma_1;
	}

      goto finished;

    case FMT_T:
    case FMT_TL:
    case FMT_TR:
    case FMT_IBOZ:
    case FMT_F:
    case FMT_E:
    case FMT_EN:
    case FMT_ES:
    case FMT_G:
    case FMT_L:
    case FMT_A:
    case FMT_D:
    case FMT_H:
    case FMT_DT:
      goto data_desc;

    case FMT_END:
      error = unexpected_end;
      goto syntax;

    case FMT_RPAREN:
      if (flag_dec_blank_format_item)
	goto finished;
      else
	{
	  error = G_("Missing item in format string at %L");
	  goto syntax;
	}

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
    case FMT_DP:
    case FMT_DC:
    case FMT_X:
      break;

    case FMT_P:
      /* No comma after P allowed only for F, E, EN, ES, D, or G.
	 10.1.1 (1).  */
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (!(gfc_option.allow_std & GFC_STD_F2003) && t != FMT_COMMA
	  && t != FMT_F && t != FMT_E && t != FMT_EN && t != FMT_ES
	  && t != FMT_D && t != FMT_G && t != FMT_RPAREN && t != FMT_SLASH)
	{
	  error = G_("Comma required after P descriptor in format string "
		     "at %L");
	  goto syntax;
	}
      if (t != FMT_COMMA)
	{
	  if (t == FMT_POSINT)
	    {
	      t = format_lex ();
	      if (t == FMT_ERROR)
		goto fail;
	    }
	  if (t != FMT_F && t != FMT_E && t != FMT_EN && t != FMT_ES
	      && t != FMT_D && t != FMT_G && t != FMT_RPAREN && t != FMT_SLASH)
	    {
	      error = G_("Comma required after P descriptor in format string "
			 "at %L");
	      goto syntax;
	    }
	}

      saved_token = t;
      goto optional_comma;

    case FMT_T:
    case FMT_TL:
    case FMT_TR:
      t = format_lex ();
      if (t != FMT_POSINT)
	{
	  error = G_("Positive width required with T descriptor in format "
		     "string at %L");
	  goto syntax;
	}
      break;

    case FMT_L:
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t == FMT_POSINT)
	break;
      if (mode != MODE_FORMAT)
	format_locus.nextc += format_string_pos;
      if (t == FMT_ZERO)
	{
	  switch (gfc_notification_std (GFC_STD_GNU))
	    {
	      case WARNING:
		gfc_warning (0, "Extension: Zero width after L "
			     "descriptor at %L", &format_locus);
		break;
	      case ERROR:
		gfc_error ("Extension: Zero width after L "
			     "descriptor at %L", &format_locus);
		goto fail;
	      case SILENT:
		break;
	      default:
		gcc_unreachable ();
	    }
	}
      else
	{
	  saved_token = t;
	  gfc_notify_std (GFC_STD_GNU, "Missing positive width after "
			  "L descriptor at %L", &format_locus);
	}
      break;

    case FMT_A:
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t == FMT_ZERO)
	{
	  error = zero_width;
	  goto syntax;
	}
      if (t != FMT_POSINT)
	saved_token = t;
      break;

    case FMT_D:
    case FMT_E:
    case FMT_G:
    case FMT_EN:
    case FMT_ES:
      u = format_lex ();
      if (t == FMT_G && u == FMT_ZERO)
	{
	  if (is_input)
	    {
	      error = zero_width;
	      goto syntax;
	    }
	  if (!gfc_notify_std (GFC_STD_F2008, "%<G0%> in format at %L",
			       &format_locus))
	    return false;
	  u = format_lex ();
	  if (u != FMT_PERIOD)
	    {
	      saved_token = u;
	      break;
	    }
	  u = format_lex ();
	  if (u != FMT_POSINT)
	    {
	      error = posint_required;
	      goto syntax;
	    }
	  u = format_lex ();
	  if (u == FMT_E)
	    {
	      error = G_("E specifier not allowed with g0 descriptor in "
			 "format string at %L");
	      goto syntax;
	    }
	  saved_token = u;
	  break;
	}

      if (u != FMT_POSINT)
	{
	  if (flag_dec)
	    {
	      if (flag_dec_format_defaults)
		{
		  /* Assume a default width based on the variable size.  */
		  saved_token = u;
		  break;
		}
	      else
		{
		  gfc_error ("Positive width required in format "
			     "specifier %s at %L", token_to_string (t),
			     &format_locus);
		  saved_token = u;
		  goto fail;
		}
	    }

	  format_locus.nextc += format_string_pos;
	  if (!gfc_notify_std (GFC_STD_F2018,
			       "positive width required at %L",
			       &format_locus))
	    {
	      saved_token = u;
	      goto fail;
	    }
	  if (flag_dec_format_defaults)
	    {
	      /* Assume a default width based on the variable size.  */
	      saved_token = u;
	      break;
	    }
	}

      u = format_lex ();
      if (u == FMT_ERROR)
	goto fail;
      if (u != FMT_PERIOD)
	{
	  /* Warn if -std=legacy, otherwise error.  */
	  format_locus.nextc += format_string_pos;
	  if (gfc_option.warn_std != 0)
	    {
	      gfc_error ("Period required in format "
			     "specifier %s at %L", token_to_string (t),
			     &format_locus);
	      saved_token = u;
              goto fail;
	    }
	  else
	    gfc_warning (0, "Period required in format "
			 "specifier %s at %L", token_to_string (t),
			  &format_locus);
	  /* If we go to finished, we need to unwind this
	     before the next round.  */
	  format_locus.nextc -= format_string_pos;
	  saved_token = u;
	  break;
	}

      u = format_lex ();
      if (u == FMT_ERROR)
	goto fail;
      if (u != FMT_ZERO && u != FMT_POSINT)
	{
	  error = nonneg_required;
	  goto syntax;
	}

      if (t == FMT_D)
	break;

      /* Look for optional exponent.  */
      u = format_lex ();
      if (u == FMT_ERROR)
	goto fail;
      if (u != FMT_E)
	saved_token = u;
      else
	{
	  u = format_lex ();
	  if (u == FMT_ERROR)
	    goto fail;
	  if (u != FMT_POSINT)
	    {
	      if (u == FMT_ZERO)
		{
		  if (!gfc_notify_std (GFC_STD_F2018,
				      "Positive exponent width required in "
				      "format string at %L", &format_locus))
		    {
		      saved_token = u;
		      goto fail;
		    }
		}
	      else
		{
		  error = G_("Positive exponent width required in format "
			     "string at %L");
		  goto syntax;
		}
	    }
	}

      break;

    case FMT_DT:
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      switch (t)
	{
	case FMT_RPAREN:
	  level--;
	  if (level < 0)
	    goto finished;
	  goto between_desc;

	case FMT_COMMA:
	  goto format_item;

	case FMT_COLON:
	  goto format_item_1;

	case FMT_LPAREN:

  dtio_vlist:
	  t = format_lex ();
	  if (t == FMT_ERROR)
	    goto fail;

	  if (t != FMT_POSINT)
	    {
	      error = posint_required;
	      goto syntax;
	    }

	  t = format_lex ();
	  if (t == FMT_ERROR)
	    goto fail;

	  if (t == FMT_COMMA)
	    goto dtio_vlist;
	  if (t != FMT_RPAREN)
	    {
	      error = G_("Right parenthesis expected at %C in format string "
			 "at %L");
	      goto syntax;
	    }
	  goto between_desc;

	default:
	  error = unexpected_element;
	  goto syntax;
	}
      break;

    case FMT_F:
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t != FMT_ZERO && t != FMT_POSINT)
	{
	  if (flag_dec_format_defaults)
	    {
	      /* Assume the default width is expected here and continue lexing.  */
	      value = 0; /* It doesn't matter what we set the value to here.  */
	      saved_token = t;
	      break;
	    }
	  error = nonneg_required;
	  goto syntax;
	}
      else if (is_input && t == FMT_ZERO)
	{
	  error = posint_required;
	  goto syntax;
	}

      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t != FMT_PERIOD)
	{
	  /* Warn if -std=legacy, otherwise error.  */
	  if (gfc_option.warn_std != 0)
	    {
	      error = G_("Period required in format specifier in format "
			 "string at %L");
	      goto syntax;
	    }
	  if (mode != MODE_FORMAT)
	    format_locus.nextc += format_string_pos;
	  gfc_warning (0, "Period required in format specifier at %L",
		       &format_locus);
	  saved_token = t;
	  break;
	}

      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t != FMT_ZERO && t != FMT_POSINT)
	{
	  error = nonneg_required;
	  goto syntax;
	}

      break;

    case FMT_H:
      if (!(gfc_option.allow_std & GFC_STD_GNU) && !inhibit_warnings)
	{
	  if (mode != MODE_FORMAT)
	    format_locus.nextc += format_string_pos;
	  gfc_warning (0, "The H format specifier at %L is"
		       " a Fortran 95 deleted feature", &format_locus);
	}
      if (mode == MODE_STRING)
	{
	  format_string += value;
	  format_length -= value;
          format_string_pos += repeat;
	}
      else
	{
	  while (repeat >0)
	   {
	     next_char (INSTRING_WARN);
	     repeat -- ;
	   }
	}
     break;

    case FMT_IBOZ:
      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t != FMT_ZERO && t != FMT_POSINT)
	{
	  if (flag_dec_format_defaults)
	    {
	      /* Assume the default width is expected here and continue lexing.  */
	      value = 0; /* It doesn't matter what we set the value to here.  */
	      saved_token = t;
	    }
	  else
	    {
	      error = nonneg_required;
	      goto syntax;
	    }
	}
      else if (is_input && t == FMT_ZERO)
	{
	  error = posint_required;
	  goto syntax;
	}

      t = format_lex ();
      if (t == FMT_ERROR)
	goto fail;
      if (t != FMT_PERIOD)
	saved_token = t;
      else
	{
	  t = format_lex ();
	  if (t == FMT_ERROR)
	    goto fail;
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
  if (t == FMT_ERROR)
    goto fail;
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
      if (mode != MODE_FORMAT)
	format_locus.nextc += format_string_pos - 1;
      if (!gfc_notify_std (GFC_STD_GNU, "Missing comma at %L", &format_locus))
	return false;
      /* If we do not actually return a failure, we need to unwind this
         before the next round.  */
      if (mode != MODE_FORMAT)
	format_locus.nextc -= format_string_pos;
      goto format_item_1;
    }

optional_comma:
  /* Optional comma is a weird between state where we've just finished
     reading a colon, slash, dollar or P descriptor.  */
  t = format_lex ();
  if (t == FMT_ERROR)
    goto fail;
optional_comma_1:
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
  if (t == FMT_ERROR)
    goto fail;
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
      if (mode != MODE_FORMAT)
	format_locus.nextc += format_string_pos;
      if (!gfc_notify_std (GFC_STD_GNU, "Missing comma at %L", &format_locus))
	return false;
      /* If we do not actually return a failure, we need to unwind this
         before the next round.  */
      if (mode != MODE_FORMAT)
	format_locus.nextc -= format_string_pos;
      saved_token = t;
      break;
    }

  goto format_item;

syntax:
  if (mode != MODE_FORMAT)
    format_locus.nextc += format_string_pos;
  if (error == unexpected_element)
    gfc_error (error, error_element, &format_locus);
  else
    gfc_error (error, &format_locus);
fail:
  rv = false;

finished:
  return rv;
}


/* Given an expression node that is a constant string, see if it looks
   like a format string.  */

static bool
check_format_string (gfc_expr *e, bool is_input)
{
  bool rv;
  int i;
  if (!e || e->ts.type != BT_CHARACTER || e->expr_type != EXPR_CONSTANT)
    return true;

  mode = MODE_STRING;
  format_string = e->value.character.string;

  /* More elaborate measures are needed to show where a problem is within a
     format string that has been calculated, but that's probably not worth the
     effort.  */
  format_locus = e->where;
  rv = check_format (is_input);
  /* check for extraneous characters at the end of an otherwise valid format
     string, like '(A10,I3)F5'
     start at the end and move back to the last character processed,
     spaces are OK */
  if (rv && e->value.character.length > format_string_pos)
    for (i=e->value.character.length-1;i>format_string_pos-1;i--)
      if (e->value.character.string[i] != ' ')
        {
          format_locus.nextc += format_length + 1;
          gfc_warning (0,
		       "Extraneous characters in format at %L", &format_locus);
          break;
        }
  return rv;
}


/************ Fortran I/O statement matchers *************/

/* Match a FORMAT statement.  This amounts to actually parsing the
   format descriptors in order to correctly locate the end of the
   format string.  */

match
gfc_match_format (void)
{
  gfc_expr *e;
  locus start;

  if (gfc_current_ns->proc_name
      && gfc_current_ns->proc_name->attr.flavor == FL_MODULE)
    {
      gfc_error ("Format statement in module main block at %C");
      return MATCH_ERROR;
    }

  /* Before parsing the rest of a FORMAT statement, check F2008:c1206.  */
  if ((gfc_current_state () == COMP_FUNCTION
       || gfc_current_state () == COMP_SUBROUTINE)
      && gfc_state_stack->previous->state == COMP_INTERFACE)
    {
      gfc_error ("FORMAT statement at %C cannot appear within an INTERFACE");
      return MATCH_ERROR;
    }

  if (gfc_statement_label == NULL)
    {
      gfc_error ("Missing format label at %C");
      return MATCH_ERROR;
    }
  gfc_gobble_whitespace ();

  mode = MODE_FORMAT;
  format_length = 0;

  start = gfc_current_locus;

  if (!check_format (false))
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

  e = gfc_get_character_expr (gfc_default_character_kind, &start,
			      NULL, format_length);
  format_string = e->value.character.string;
  gfc_statement_label->format = e;

  mode = MODE_COPY;
  check_format (false);		/* Guaranteed to succeed */
  gfc_match_eos ();		/* Guaranteed to succeed */

  return MATCH_YES;
}


/* Match an expression I/O tag of some sort.  */

static match
match_etag (const io_tag *tag, gfc_expr **v)
{
  gfc_expr *result;
  match m;

  m = gfc_match (tag->spec);
  if (m != MATCH_YES)
    return m;

  m = gfc_match (tag->value, &result);
  if (m != MATCH_YES)
    {
      gfc_error ("Invalid value for %s specification at %C", tag->name);
      return MATCH_ERROR;
    }

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
match_vtag (const io_tag *tag, gfc_expr **v)
{
  gfc_expr *result;
  match m;

  m = gfc_match (tag->spec);
  if (m != MATCH_YES)
    return m;

  m = gfc_match (tag->value, &result);
  if (m != MATCH_YES)
    {
      gfc_error ("Invalid value for %s specification at %C", tag->name);
      return MATCH_ERROR;
    }

  if (*v != NULL)
    {
      gfc_error ("Duplicate %s specification at %C", tag->name);
      gfc_free_expr (result);
      return MATCH_ERROR;
    }

  if (result->symtree)
    {
      bool impure;

      if (result->symtree->n.sym->attr.intent == INTENT_IN)
	{
	  gfc_error ("Variable %s cannot be INTENT(IN) at %C", tag->name);
	  gfc_free_expr (result);
	  return MATCH_ERROR;
	}

      impure = gfc_impure_variable (result->symtree->n.sym);
      if (impure && gfc_pure (NULL))
	{
	  gfc_error ("Variable %s cannot be assigned in PURE procedure at %C",
		     tag->name);
	  gfc_free_expr (result);
	  return MATCH_ERROR;
	}

      if (impure)
	gfc_unset_implicit_pure (NULL);
    }

  *v = result;
  return MATCH_YES;
}


/* Match I/O tags that cause variables to become redefined.  */

static match
match_out_tag (const io_tag *tag, gfc_expr **result)
{
  match m;

  m = match_vtag (tag, result);
  if (m == MATCH_YES)
    {
      if ((*result)->symtree)
	gfc_check_do_variable ((*result)->symtree);

      if ((*result)->expr_type == EXPR_CONSTANT)
	{
	  gfc_error ("Expecting a variable at %L", &(*result)->where);
	  return MATCH_ERROR;
	}
    }

  return m;
}


/* Match a label I/O tag.  */

static match
match_ltag (const io_tag *tag, gfc_st_label ** label)
{
  match m;
  gfc_st_label *old;

  old = *label;
  m = gfc_match (tag->spec);
  if (m != MATCH_YES)
    return m;

  m = gfc_match (tag->value, label);
  if (m != MATCH_YES)
    {
      gfc_error ("Invalid value for %s specification at %C", tag->name);
      return MATCH_ERROR;
    }

  if (old)
    {
      gfc_error ("Duplicate %s label specification at %C", tag->name);
      return MATCH_ERROR;
    }

  if (!gfc_reference_st_label (*label, ST_LABEL_TARGET))
    return MATCH_ERROR;

  return m;
}


/* Match a tag using match_etag, but only if -fdec is enabled.  */
static match
match_dec_etag (const io_tag *tag, gfc_expr **e)
{
  match m = match_etag (tag, e);
  if (flag_dec && m != MATCH_NO)
    return m;
  else if (m != MATCH_NO)
    {
      gfc_error ("%s at %C is a DEC extension, enable with "
		 "%<-fdec%>", tag->name);
      return MATCH_ERROR;
    }
  return m;
}


/* Match a tag using match_vtag, but only if -fdec is enabled.  */
static match
match_dec_vtag (const io_tag *tag, gfc_expr **e)
{
  match m = match_vtag(tag, e);
  if (flag_dec && m != MATCH_NO)
    return m;
  else if (m != MATCH_NO)
    {
      gfc_error ("%s at %C is a DEC extension, enable with "
		 "%<-fdec%>", tag->name);
      return MATCH_ERROR;
    }
  return m;
}


/* Match a DEC I/O flag tag - a tag with no expression such as READONLY.  */

static match
match_dec_ftag (const io_tag *tag, gfc_open *o)
{
  match m;

  m = gfc_match (tag->spec);
  if (m != MATCH_YES)
    return m;

  if (!flag_dec)
    {
      gfc_error ("%s at %C is a DEC extension, enable with "
		 "%<-fdec%>", tag->name);
      return MATCH_ERROR;
    }

  /* Just set the READONLY flag, which we use at runtime to avoid delete on
     close.  */
  if (tag == &tag_readonly)
    {
      o->readonly |= 1;
      return MATCH_YES;
    }

  /* Interpret SHARED as SHARE='DENYNONE' (read lock).  */
  else if (tag == &tag_shared)
    {
      if (o->share != NULL)
	{
	  gfc_error ("Duplicate %s specification at %C", tag->name);
	  return MATCH_ERROR;
	}
      o->share = gfc_get_character_expr (gfc_default_character_kind,
	  &gfc_current_locus, "denynone", 8);
      return MATCH_YES;
    }

  /* Interpret NOSHARED as SHARE='DENYRW' (exclusive lock).  */
  else if (tag == &tag_noshared)
    {
      if (o->share != NULL)
	{
	  gfc_error ("Duplicate %s specification at %C", tag->name);
	  return MATCH_ERROR;
	}
      o->share = gfc_get_character_expr (gfc_default_character_kind,
	  &gfc_current_locus, "denyrw", 6);
      return MATCH_YES;
    }

  /* We handle all DEC tags above.  */
  gcc_unreachable ();
}


/* Resolution of the FORMAT tag, to be called from resolve_tag.  */

static bool
resolve_tag_format (gfc_expr *e)
{
  if (e->expr_type == EXPR_CONSTANT
      && (e->ts.type != BT_CHARACTER
	  || e->ts.kind != gfc_default_character_kind))
    {
      gfc_error ("Constant expression in FORMAT tag at %L must be "
		 "of type default CHARACTER", &e->where);
      return false;
    }

  /* Concatenate a constant character array into a single character
     expression.  */

  if ((e->expr_type == EXPR_ARRAY || e->rank > 0)
      && e->ts.type == BT_CHARACTER
      && gfc_is_constant_expr (e))
    {
      if (e->expr_type == EXPR_VARIABLE
	  && e->symtree->n.sym->attr.flavor == FL_PARAMETER)
	gfc_simplify_expr (e, 1);

      if (e->expr_type == EXPR_ARRAY)
	{
	  gfc_constructor *c;
	  gfc_charlen_t n, len;
	  gfc_expr *r;
	  gfc_char_t *dest, *src;

	  if (e->value.constructor == NULL)
	   {
	     gfc_error ("FORMAT tag at %L cannot be a zero-sized array",
			&e->where);
	     return false;
	   }

	  n = 0;
	  c = gfc_constructor_first (e->value.constructor);
	  len = c->expr->value.character.length;

	  for ( ; c; c = gfc_constructor_next (c))
	    n += len;

	  r = gfc_get_character_expr (e->ts.kind, &e->where, NULL, n);
	  dest = r->value.character.string;

	  for (c = gfc_constructor_first (e->value.constructor);
	     c; c = gfc_constructor_next (c))
	    {
	      src = c->expr->value.character.string;
	      for (gfc_charlen_t i = 0 ; i < len; i++)
		*dest++ = *src++;
	    }

	  gfc_replace_expr (e, r);
	  return true;
	}
    }

  /* If e's rank is zero and e is not an element of an array, it should be
     of integer or character type.  The integer variable should be
     ASSIGNED.  */
  if (e->rank == 0
      && (e->expr_type != EXPR_VARIABLE
	  || e->symtree == NULL
	  || e->symtree->n.sym->as == NULL
	  || e->symtree->n.sym->as->rank == 0))
    {
      if ((e->ts.type != BT_CHARACTER
	   || e->ts.kind != gfc_default_character_kind)
	  && e->ts.type != BT_INTEGER)
	{
	  gfc_error ("FORMAT tag at %L must be of type default-kind CHARACTER "
		     "or of INTEGER", &e->where);
	  return false;
	}
      else if (e->ts.type == BT_INTEGER && e->expr_type == EXPR_VARIABLE)
	{
	  if (!gfc_notify_std (GFC_STD_F95_DEL, "ASSIGNED variable in "
			       "FORMAT tag at %L", &e->where))
	    return false;
	  if (e->symtree->n.sym->attr.assign != 1)
	    {
	      gfc_error ("Variable %qs at %L has not been assigned a "
			 "format label", e->symtree->n.sym->name, &e->where);
	      return false;
	    }
	}
      else if (e->ts.type == BT_INTEGER)
	{
	  gfc_error ("Scalar %qs in FORMAT tag at %L is not an ASSIGNED "
		     "variable", gfc_basic_typename (e->ts.type), &e->where);
	  return false;
	}

      return true;
    }

  /* If rank is nonzero and type is not character, we allow it under GFC_STD_LEGACY.
     It may be assigned an Hollerith constant.  */
  if (e->ts.type != BT_CHARACTER)
    {
      if (e->ts.type == BT_DERIVED || e->ts.type == BT_CLASS
	  || e->ts.type == BT_VOID || e->ts.type == BT_UNKNOWN)
	{
	  gfc_error ("Non-character non-Hollerith in FORMAT tag at %L",
		     &e->where);
	  return false;
	}
      if (!gfc_notify_std (GFC_STD_LEGACY, "Non-character in FORMAT tag "
			   "at %L", &e->where))
	return false;

      if (e->rank == 0 && e->symtree->n.sym->as->type == AS_ASSUMED_SHAPE)
	{
	  gfc_error ("Non-character assumed shape array element in FORMAT"
		     " tag at %L", &e->where);
	  return false;
	}

      if (e->rank == 0 && e->symtree->n.sym->as->type == AS_ASSUMED_SIZE)
	{
	  gfc_error ("Non-character assumed size array element in FORMAT"
		     " tag at %L", &e->where);
	  return false;
	}

      if (e->rank == 0 && e->symtree->n.sym->attr.pointer)
	{
	  gfc_error ("Non-character pointer array element in FORMAT tag at %L",
		     &e->where);
	  return false;
	}
    }

  return true;
}


/* Do expression resolution and type-checking on an expression tag.  */

static bool
resolve_tag (const io_tag *tag, gfc_expr *e)
{
  if (e == NULL)
    return true;

  if (!gfc_resolve_expr (e))
    return false;

  if (tag == &tag_format)
    return resolve_tag_format (e);

  if (e->ts.type != tag->type)
    {
      gfc_error ("%s tag at %L must be of type %s", tag->name,
		 &e->where, gfc_basic_typename (tag->type));
      return false;
    }

  if (e->ts.type == BT_CHARACTER && e->ts.kind != gfc_default_character_kind)
    {
      gfc_error ("%s tag at %L must be a character string of default kind",
		 tag->name, &e->where);
      return false;
    }

  if (e->rank != 0)
    {
      gfc_error ("%s tag at %L must be scalar", tag->name, &e->where);
      return false;
    }

  if (tag == &tag_iomsg)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "IOMSG tag at %L", &e->where))
	return false;
    }

  if ((tag == &tag_iostat || tag == &tag_size || tag == &tag_iolength
       || tag == &tag_number || tag == &tag_nextrec || tag == &tag_s_recl)
      && e->ts.kind != gfc_default_integer_kind)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "Fortran 95 requires default "
			   "INTEGER in %s tag at %L", tag->name, &e->where))
	return false;
    }

  if (e->ts.kind != gfc_default_logical_kind &&
      (tag == &tag_exist || tag == &tag_named || tag == &tag_opened
       || tag == &tag_pending))
    {
      if (!gfc_notify_std (GFC_STD_F2008, "Non-default LOGICAL kind "
			   "in %s tag at %L", tag->name, &e->where))
	return false;
    }

  if (tag == &tag_newunit)
    {
      if (!gfc_notify_std (GFC_STD_F2008, "NEWUNIT specifier at %L",
			   &e->where))
	return false;
    }

  /* NEWUNIT, IOSTAT, SIZE and IOMSG are variable definition contexts.  */
  if (tag == &tag_newunit || tag == &tag_iostat
      || tag == &tag_size || tag == &tag_iomsg)
    {
      char context[64];

      sprintf (context, _("%s tag"), tag->name);
      if (!gfc_check_vardef_context (e, false, false, false, context))
	return false;
    }

  if (tag == &tag_convert)
    {
      if (!gfc_notify_std (GFC_STD_GNU, "CONVERT tag at %L", &e->where))
	return false;
    }

  return true;
}


/* Match a single tag of an OPEN statement.  */

static match
match_open_element (gfc_open *open)
{
  match m;

  m = match_etag (&tag_e_async, &open->asynchronous);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_unit, &open->unit);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_iomsg, &open->iomsg);
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
  m = match_etag (&tag_e_decimal, &open->decimal);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_encoding, &open->encoding);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_round, &open->round);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_sign, &open->sign);
  if (m != MATCH_NO)
    return m;
  m = match_ltag (&tag_err, &open->err);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_convert, &open->convert);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_newunit, &open->newunit);
  if (m != MATCH_NO)
    return m;

  /* The following are extensions enabled with -fdec.  */
  m = match_dec_etag (&tag_e_share, &open->share);
  if (m != MATCH_NO)
    return m;
  m = match_dec_etag (&tag_cc, &open->cc);
  if (m != MATCH_NO)
    return m;
  m = match_dec_ftag (&tag_readonly, open);
  if (m != MATCH_NO)
    return m;
  m = match_dec_ftag (&tag_shared, open);
  if (m != MATCH_NO)
    return m;
  m = match_dec_ftag (&tag_noshared, open);
  if (m != MATCH_NO)
    return m;

  return MATCH_NO;
}


/* Free the gfc_open structure and all the expressions it contains.  */

void
gfc_free_open (gfc_open *open)
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
  gfc_free_expr (open->decimal);
  gfc_free_expr (open->encoding);
  gfc_free_expr (open->round);
  gfc_free_expr (open->sign);
  gfc_free_expr (open->convert);
  gfc_free_expr (open->asynchronous);
  gfc_free_expr (open->newunit);
  gfc_free_expr (open->share);
  gfc_free_expr (open->cc);
  free (open);
}

static bool
check_open_constraints (gfc_open *open, locus *where);

/* Resolve everything in a gfc_open structure.  */

bool
gfc_resolve_open (gfc_open *open, locus *where)
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
  RESOLVE_TAG (&tag_e_decimal, open->decimal);
  RESOLVE_TAG (&tag_e_encoding, open->encoding);
  RESOLVE_TAG (&tag_e_async, open->asynchronous);
  RESOLVE_TAG (&tag_e_round, open->round);
  RESOLVE_TAG (&tag_e_sign, open->sign);
  RESOLVE_TAG (&tag_convert, open->convert);
  RESOLVE_TAG (&tag_newunit, open->newunit);
  RESOLVE_TAG (&tag_e_share, open->share);
  RESOLVE_TAG (&tag_cc, open->cc);

  if (!gfc_reference_st_label (open->err, ST_LABEL_TARGET))
    return false;

  return check_open_constraints (open, where);
}


/* Check if a given value for a SPECIFIER is either in the list of values
   allowed in F95 or F2003, issuing an error message and returning a zero
   value if it is not allowed.  */


static bool
compare_to_allowed_values (const char *specifier, const char *allowed[],
			   const char *allowed_f2003[],
			   const char *allowed_gnu[], gfc_char_t *value,
			   const char *statement, bool warn, locus *where,
			   int *num = NULL)
{
  int i;
  unsigned int len;

  len = gfc_wide_strlen (value);
  if (len > 0)
  {
    for (len--; len > 0; len--)
      if (value[len] != ' ')
	break;
    len++;
  }

  for (i = 0; allowed[i]; i++)
    if (len == strlen (allowed[i])
	&& gfc_wide_strncasecmp (value, allowed[i], strlen (allowed[i])) == 0)
      {
	if (num)
	  *num = i;
      return 1;
      }

  if (!where)
    where = &gfc_current_locus;

  for (i = 0; allowed_f2003 && allowed_f2003[i]; i++)
    if (len == strlen (allowed_f2003[i])
	&& gfc_wide_strncasecmp (value, allowed_f2003[i],
				 strlen (allowed_f2003[i])) == 0)
      {
	notification n = gfc_notification_std (GFC_STD_F2003);

	if (n == WARNING || (warn && n == ERROR))
	  {
	    gfc_warning (0, "Fortran 2003: %s specifier in %s statement at %L "
			 "has value %qs", specifier, statement, where,
			 allowed_f2003[i]);
	    return 1;
	  }
	else
	  if (n == ERROR)
	    {
	      gfc_notify_std (GFC_STD_F2003, "%s specifier in "
			      "%s statement at %L has value %qs", specifier,
			      statement, where, allowed_f2003[i]);
	      return 0;
	    }

	/* n == SILENT */
	return 1;
      }

  for (i = 0; allowed_gnu && allowed_gnu[i]; i++)
    if (len == strlen (allowed_gnu[i])
	&& gfc_wide_strncasecmp (value, allowed_gnu[i],
				 strlen (allowed_gnu[i])) == 0)
      {
	notification n = gfc_notification_std (GFC_STD_GNU);

	if (n == WARNING || (warn && n == ERROR))
	  {
	    gfc_warning (0, "Extension: %s specifier in %s statement at %L "
			 "has value %qs", specifier, statement, where,
			 allowed_gnu[i]);
	    return 1;
	  }
	else
	  if (n == ERROR)
	    {
	      gfc_notify_std (GFC_STD_GNU, "%s specifier in "
			      "%s statement at %L has value %qs", specifier,
			      statement, where, allowed_gnu[i]);
	      return 0;
	    }

	/* n == SILENT */
	return 1;
      }

  if (warn)
    {
      char *s = gfc_widechar_to_char (value, -1);
      gfc_warning (0,
		   "%s specifier in %s statement at %L has invalid value %qs",
		   specifier, statement, where, s);
      free (s);
      return 1;
    }
  else
    {
      char *s = gfc_widechar_to_char (value, -1);
      gfc_error ("%s specifier in %s statement at %L has invalid value %qs",
		 specifier, statement, where, s);
      free (s);
      return 0;
    }
}


/* Check constraints on the OPEN statement.
   Similar to check_io_constraints for data transfer statements.
   At this point all tags have already been resolved via resolve_tag, which,
   among other things, verifies that BT_CHARACTER tags are of default kind.  */

static bool
check_open_constraints (gfc_open *open, locus *where)
{
#define warn_or_error(...) \
{ \
  if (warn) \
    gfc_warning (0, __VA_ARGS__); \
  else \
    { \
      gfc_error (__VA_ARGS__); \
      return false; \
    } \
}

  bool warn = (open->err || open->iostat) ? true : false;

  /* Checks on the ACCESS specifier.  */
  if (open->access && open->access->expr_type == EXPR_CONSTANT)
    {
      static const char *access_f95[] = { "SEQUENTIAL", "DIRECT", NULL };
      static const char *access_f2003[] = { "STREAM", NULL };
      static const char *access_gnu[] = { "APPEND", NULL };

      if (!compare_to_allowed_values ("ACCESS", access_f95, access_f2003,
				      access_gnu,
				      open->access->value.character.string,
				      "OPEN", warn, &open->access->where))
	return false;
    }

  /* Checks on the ACTION specifier.  */
  if (open->action && open->action->expr_type == EXPR_CONSTANT)
    {
      gfc_char_t *str = open->action->value.character.string;
      static const char *action[] = { "READ", "WRITE", "READWRITE", NULL };

      if (!compare_to_allowed_values ("ACTION", action, NULL, NULL,
				      str, "OPEN", warn, &open->action->where))
	return false;

      /* With READONLY, only allow ACTION='READ'.  */
      if (open->readonly && (gfc_wide_strlen (str) != 4
			     || gfc_wide_strncasecmp (str, "READ", 4) != 0))
	{
	  gfc_error ("ACTION type conflicts with READONLY specifier at %L",
		     &open->action->where);
	  return false;
	}
    }

  /* If we see READONLY and no ACTION, set ACTION='READ'.  */
  else if (open->readonly && open->action == NULL)
    {
      open->action = gfc_get_character_expr (gfc_default_character_kind,
					     &gfc_current_locus, "read", 4);
    }

  /* Checks on the ASYNCHRONOUS specifier.  */
  if (open->asynchronous)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "ASYNCHRONOUS= at %L "
			   "not allowed in Fortran 95",
			   &open->asynchronous->where))
	return false;

      if (open->asynchronous->expr_type == EXPR_CONSTANT)
	{
	  static const char * asynchronous[] = { "YES", "NO", NULL };

	  if (!compare_to_allowed_values ("ASYNCHRONOUS", asynchronous,
			NULL, NULL, open->asynchronous->value.character.string,
			"OPEN", warn, &open->asynchronous->where))
	    return false;
	}
    }

  /* Checks on the BLANK specifier.  */
  if (open->blank)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "BLANK= at %L "
			   "not allowed in Fortran 95", &open->blank->where))
	return false;

      if (open->blank->expr_type == EXPR_CONSTANT)
	{
	  static const char *blank[] = { "ZERO", "NULL", NULL };

	  if (!compare_to_allowed_values ("BLANK", blank, NULL, NULL,
					  open->blank->value.character.string,
					  "OPEN", warn, &open->blank->where))
	    return false;
	}
    }

  /* Checks on the CARRIAGECONTROL specifier.  */
  if (open->cc && open->cc->expr_type == EXPR_CONSTANT)
    {
      static const char *cc[] = { "LIST", "FORTRAN", "NONE", NULL };
      if (!compare_to_allowed_values ("CARRIAGECONTROL", cc, NULL, NULL,
				      open->cc->value.character.string,
				      "OPEN", warn, &open->cc->where))
	return false;
    }

  /* Checks on the DECIMAL specifier.  */
  if (open->decimal)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "DECIMAL= at %L "
			   "not allowed in Fortran 95", &open->decimal->where))
	return false;

      if (open->decimal->expr_type == EXPR_CONSTANT)
	{
	  static const char * decimal[] = { "COMMA", "POINT", NULL };

	  if (!compare_to_allowed_values ("DECIMAL", decimal, NULL, NULL,
					  open->decimal->value.character.string,
					  "OPEN", warn, &open->decimal->where))
	    return false;
	}
    }

  /* Checks on the DELIM specifier.  */
  if (open->delim)
    {
      if (open->delim->expr_type == EXPR_CONSTANT)
	{
	  static const char *delim[] = { "APOSTROPHE", "QUOTE", "NONE", NULL };

	  if (!compare_to_allowed_values ("DELIM", delim, NULL, NULL,
					  open->delim->value.character.string,
					  "OPEN", warn, &open->delim->where))
	    return false;
	}
    }

  /* Checks on the ENCODING specifier.  */
  if (open->encoding)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "ENCODING= at %L "
			   "not allowed in Fortran 95", &open->encoding->where))
	return false;

      if (open->encoding->expr_type == EXPR_CONSTANT)
	{
	  static const char * encoding[] = { "DEFAULT", "UTF-8", NULL };

	  if (!compare_to_allowed_values ("ENCODING", encoding, NULL, NULL,
					  open->encoding->value.character.string,
					  "OPEN", warn, &open->encoding->where))
	    return false;
	}
    }

  /* Checks on the FORM specifier.  */
  if (open->form && open->form->expr_type == EXPR_CONSTANT)
    {
      static const char *form[] = { "FORMATTED", "UNFORMATTED", NULL };

      if (!compare_to_allowed_values ("FORM", form, NULL, NULL,
				      open->form->value.character.string,
				      "OPEN", warn, &open->form->where))
	return false;
    }

  /* Checks on the PAD specifier.  */
  if (open->pad && open->pad->expr_type == EXPR_CONSTANT)
    {
      static const char *pad[] = { "YES", "NO", NULL };

      if (!compare_to_allowed_values ("PAD", pad, NULL, NULL,
				      open->pad->value.character.string,
				      "OPEN", warn, &open->pad->where))
	return false;
    }

  /* Checks on the POSITION specifier.  */
  if (open->position && open->position->expr_type == EXPR_CONSTANT)
    {
      static const char *position[] = { "ASIS", "REWIND", "APPEND", NULL };

      if (!compare_to_allowed_values ("POSITION", position, NULL, NULL,
				      open->position->value.character.string,
				      "OPEN", warn, &open->position->where))
	return false;
    }

  /* Checks on the ROUND specifier.  */
  if (open->round)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "ROUND= at %L "
			   "not allowed in Fortran 95", &open->round->where))
	return false;

      if (open->round->expr_type == EXPR_CONSTANT)
	{
	  static const char * round[] = { "UP", "DOWN", "ZERO", "NEAREST",
					  "COMPATIBLE", "PROCESSOR_DEFINED",
					   NULL };

	  if (!compare_to_allowed_values ("ROUND", round, NULL, NULL,
					  open->round->value.character.string,
					  "OPEN", warn, &open->round->where))
	    return false;
	}
    }

  /* Checks on the SHARE specifier.  */
  if (open->share && open->share->expr_type == EXPR_CONSTANT)
    {
      static const char *share[] = { "DENYNONE", "DENYRW", NULL };
      if (!compare_to_allowed_values ("SHARE", share, NULL, NULL,
				      open->share->value.character.string,
				      "OPEN", warn, &open->share->where))
	return false;
    }

  /* Checks on the SIGN specifier.  */
  if (open->sign)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "SIGN= at %L "
			   "not allowed in Fortran 95", &open->sign->where))
	return false;

      if (open->sign->expr_type == EXPR_CONSTANT)
	{
	  static const char * sign[] = { "PLUS", "SUPPRESS", "PROCESSOR_DEFINED",
					  NULL };

	  if (!compare_to_allowed_values ("SIGN", sign, NULL, NULL,
					  open->sign->value.character.string,
					  "OPEN", warn, &open->sign->where))
	    return false;
	}
    }

  /* Checks on the RECL specifier.  */
  if (open->recl && open->recl->expr_type == EXPR_CONSTANT
      && open->recl->ts.type == BT_INTEGER
      && mpz_sgn (open->recl->value.integer) != 1)
    {
      warn_or_error (G_("RECL in OPEN statement at %L must be positive"),
		     &open->recl->where);
    }

  /* Checks on the STATUS specifier.  */
  if (open->status && open->status->expr_type == EXPR_CONSTANT)
    {
      static const char *status[] = { "OLD", "NEW", "SCRATCH",
	"REPLACE", "UNKNOWN", NULL };

      if (!compare_to_allowed_values ("STATUS", status, NULL, NULL,
				      open->status->value.character.string,
				      "OPEN", warn, &open->status->where))
	return false;

      /* F2003, 9.4.5: If the STATUS= specifier has the value NEW or REPLACE,
	 the FILE= specifier shall appear.  */
      if (open->file == NULL
	  && (gfc_wide_strncasecmp (open->status->value.character.string,
				    "replace", 7) == 0
	      || gfc_wide_strncasecmp (open->status->value.character.string,
				       "new", 3) == 0))
	{
	  char *s = gfc_widechar_to_char (open->status->value.character.string,
					  -1);
	  warn_or_error (G_("The STATUS specified in OPEN statement at %L is "
			 "%qs and no FILE specifier is present"),
			 &open->status->where, s);
	  free (s);
	}

      /* F2003, 9.4.5: If the STATUS= specifier has the value SCRATCH,
	 the FILE= specifier shall not appear.  */
      if (gfc_wide_strncasecmp (open->status->value.character.string,
				"scratch", 7) == 0 && open->file)
	{
	  warn_or_error (G_("The STATUS specified in OPEN statement at %L "
			 "cannot have the value SCRATCH if a FILE specifier "
			 "is present"), &open->status->where);
	}
    }

  /* Checks on NEWUNIT specifier.  */
  if (open->newunit)
    {
      if (open->unit)
	{
	  gfc_error ("UNIT specifier not allowed with NEWUNIT at %L",
		     &open->newunit->where);
	  return false;
	}

      if (!open->file &&
	  (!open->status ||
	   (open->status->expr_type == EXPR_CONSTANT
	     && gfc_wide_strncasecmp (open->status->value.character.string,
				      "scratch", 7) != 0)))
	{
	     gfc_error ("NEWUNIT specifier must have FILE= "
			"or STATUS='scratch' at %L", &open->newunit->where);
	     return false;
	}
    }
  else if (!open->unit)
    {
      gfc_error ("OPEN statement at %L must have UNIT or NEWUNIT specified",
		 where);
      return false;
    }

  /* Things that are not allowed for unformatted I/O.  */
  if (open->form && open->form->expr_type == EXPR_CONSTANT
      && (open->delim || open->decimal || open->encoding || open->round
	  || open->sign || open->pad || open->blank)
      && gfc_wide_strncasecmp (open->form->value.character.string,
			       "unformatted", 11) == 0)
    {
      locus *loc;
      const char *spec;
      if (open->delim)
	{
	  loc = &open->delim->where;
	  spec = "DELIM ";
	}
      else if (open->pad)
	{
	  loc = &open->pad->where;
	  spec = "PAD ";
	}
      else if (open->blank)
	{
	  loc = &open->blank->where;
	  spec = "BLANK ";
	}
      else
	{
	  loc = where;
	  spec = "";
	}

      warn_or_error (G_("%sspecifier at %L not allowed in OPEN statement for "
		     "unformatted I/O"), spec, loc);
    }

  if (open->recl && open->access && open->access->expr_type == EXPR_CONSTANT
      && gfc_wide_strncasecmp (open->access->value.character.string,
			       "stream", 6) == 0)
    {
      warn_or_error (G_("RECL specifier not allowed in OPEN statement at %L for "
		     "stream I/O"), &open->recl->where);
    }

  if (open->position
      && open->access && open->access->expr_type == EXPR_CONSTANT
      && !(gfc_wide_strncasecmp (open->access->value.character.string,
				 "sequential", 10) == 0
	   || gfc_wide_strncasecmp (open->access->value.character.string,
				    "stream", 6) == 0
	   || gfc_wide_strncasecmp (open->access->value.character.string,
				    "append", 6) == 0))
    {
      warn_or_error (G_("POSITION specifier in OPEN statement at %L only allowed "
		     "for stream or sequential ACCESS"), &open->position->where);
    }

  return true;
#undef warn_or_error
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

  open = XCNEW (gfc_open);

  m = match_open_element (open);

  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    {
      m = gfc_match_expr (&open->unit);
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

  gfc_unset_implicit_pure (NULL);

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
gfc_free_close (gfc_close *close)
{
  if (close == NULL)
    return;

  gfc_free_expr (close->unit);
  gfc_free_expr (close->iomsg);
  gfc_free_expr (close->iostat);
  gfc_free_expr (close->status);
  free (close);
}


/* Match elements of a CLOSE statement.  */

static match
match_close_element (gfc_close *close)
{
  match m;

  m = match_etag (&tag_unit, &close->unit);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_status, &close->status);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_iomsg, &close->iomsg);
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

  close = XCNEW (gfc_close);

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

  gfc_unset_implicit_pure (NULL);

  new_st.op = EXEC_CLOSE;
  new_st.ext.close = close;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_CLOSE);

cleanup:
  gfc_free_close (close);
  return MATCH_ERROR;
}


static bool
check_close_constraints (gfc_close *close, locus *where)
{
  bool warn = (close->iostat || close->err) ? true : false;

  if (close->unit == NULL)
    {
      gfc_error ("CLOSE statement at %L requires a UNIT number", where);
      return false;
    }

  if (close->unit->expr_type == EXPR_CONSTANT
      && close->unit->ts.type == BT_INTEGER
      && mpz_sgn (close->unit->value.integer) < 0)
    {
      gfc_error ("UNIT number in CLOSE statement at %L must be non-negative",
		 &close->unit->where);
    }

  /* Checks on the STATUS specifier.  */
  if (close->status && close->status->expr_type == EXPR_CONSTANT)
    {
      static const char *status[] = { "KEEP", "DELETE", NULL };

      if (!compare_to_allowed_values ("STATUS", status, NULL, NULL,
				      close->status->value.character.string,
				      "CLOSE", warn, &close->status->where))
	return false;
    }

  return true;
}

/* Resolve everything in a gfc_close structure.  */

bool
gfc_resolve_close (gfc_close *close, locus *where)
{
  RESOLVE_TAG (&tag_unit, close->unit);
  RESOLVE_TAG (&tag_iomsg, close->iomsg);
  RESOLVE_TAG (&tag_iostat, close->iostat);
  RESOLVE_TAG (&tag_status, close->status);

  if (!gfc_reference_st_label (close->err, ST_LABEL_TARGET))
    return false;

  return check_close_constraints (close, where);
}


/* Free a gfc_filepos structure.  */

void
gfc_free_filepos (gfc_filepos *fp)
{
  gfc_free_expr (fp->unit);
  gfc_free_expr (fp->iomsg);
  gfc_free_expr (fp->iostat);
  free (fp);
}


/* Match elements of a REWIND, BACKSPACE, ENDFILE, or FLUSH statement.  */

static match
match_file_element (gfc_filepos *fp)
{
  match m;

  m = match_etag (&tag_unit, &fp->unit);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_iomsg, &fp->iomsg);
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

  fp = XCNEW (gfc_filepos);

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
    goto cleanup;
  if (m == MATCH_NO)
    {
      m = gfc_match_expr (&fp->unit);
      if (m == MATCH_ERROR || m == MATCH_NO)
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

  gfc_unset_implicit_pure (NULL);

  new_st.op = op;
  new_st.ext.filepos = fp;
  return MATCH_YES;

syntax:
  gfc_syntax_error (st);

cleanup:
  gfc_free_filepos (fp);
  return MATCH_ERROR;
}


bool
gfc_resolve_filepos (gfc_filepos *fp, locus *where)
{
  RESOLVE_TAG (&tag_unit, fp->unit);
  RESOLVE_TAG (&tag_iostat, fp->iostat);
  RESOLVE_TAG (&tag_iomsg, fp->iomsg);

  if (!fp->unit && (fp->iostat || fp->iomsg || fp->err))
    {
      gfc_error ("UNIT number missing in statement at %L", where);
      return false;
    }

  if (!gfc_reference_st_label (fp->err, ST_LABEL_TARGET))
    return false;

  if (fp->unit->expr_type == EXPR_CONSTANT
      && fp->unit->ts.type == BT_INTEGER
      && mpz_sgn (fp->unit->value.integer) < 0)
    {
      gfc_error ("UNIT number in statement at %L must be non-negative",
		 &fp->unit->where);
      return false;
    }

  return true;
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
  if (!gfc_notify_std (GFC_STD_F2003, "FLUSH statement at %C"))
    return MATCH_ERROR;

  return match_filepos (ST_FLUSH, EXEC_FLUSH);
}

/******************** Data Transfer Statements *********************/

/* Return a default unit number.  */

static gfc_expr *
default_unit (io_kind k)
{
  int unit;

  if (k == M_READ)
    unit = 5;
  else
    unit = 6;

  return gfc_get_int_expr (gfc_default_integer_kind, NULL, unit);
}


/* Match a unit specification for a data transfer statement.  */

static match
match_dt_unit (io_kind k, gfc_dt *dt)
{
  gfc_expr *e;
  char c;

  if (gfc_match_char ('*') == MATCH_YES)
    {
      if (dt->io_unit != NULL)
	goto conflict;

      dt->io_unit = default_unit (k);

      c = gfc_peek_ascii_char ();
      if (c == ')')
	gfc_error_now ("Missing format with default unit at %C");

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
match_dt_format (gfc_dt *dt)
{
  locus where;
  gfc_expr *e;
  gfc_st_label *label;
  match m;

  where = gfc_current_locus;

  if (gfc_match_char ('*') == MATCH_YES)
    {
      if (dt->format_expr != NULL || dt->format_label != NULL)
	goto conflict;

      dt->format_label = &format_asterisk;
      return MATCH_YES;
    }

  if ((m = gfc_match_st_label (&label)) == MATCH_YES)
    {
      char c;

      /* Need to check if the format label is actually either an operand
	 to a user-defined operator or is a kind type parameter.  That is,
	 print 2.ip.8      ! .ip. is a user-defined operator return CHARACTER.
	 print 1_'(I0)', i ! 1_'(I0)' is a default character string.  */

      gfc_gobble_whitespace ();
      c = gfc_peek_ascii_char ();
      if (c == '.' || c == '_')
	gfc_current_locus = where;
      else
	{
	  if (dt->format_expr != NULL || dt->format_label != NULL)
	    {
	      gfc_free_st_label (label);
	      goto conflict;
	    }

	  if (!gfc_reference_st_label (label, ST_LABEL_FORMAT))
	    return MATCH_ERROR;

	  dt->format_label = label;
	  return MATCH_YES;
	}
    }
  else if (m == MATCH_ERROR)
    /* The label was zero or too large.  Emit the correct diagnosis.  */
    return MATCH_ERROR;

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

/* Check for formatted read and write DTIO procedures.  */

static bool
dtio_procs_present (gfc_symbol *sym, io_kind k)
{
  gfc_symbol *derived;

  if (sym && sym->ts.u.derived)
    {
      if (sym->ts.type == BT_CLASS && CLASS_DATA (sym))
	derived = CLASS_DATA (sym)->ts.u.derived;
      else if (sym->ts.type == BT_DERIVED)
	derived = sym->ts.u.derived;
      else
	return false;
      if ((k == M_WRITE || k == M_PRINT) &&
	  (gfc_find_specific_dtio_proc (derived, true, true) != NULL))
	return true;
      if ((k == M_READ) &&
	  (gfc_find_specific_dtio_proc (derived, false, true) != NULL))
	return true;
    }
  return false;
}

/* Traverse a namelist that is part of a READ statement to make sure
   that none of the variables in the namelist are INTENT(IN).  Returns
   nonzero if we find such a variable.  */

static int
check_namelist (gfc_symbol *sym)
{
  gfc_namelist *p;

  for (p = sym->namelist; p; p = p->next)
    if (p->sym->attr.intent == INTENT_IN)
      {
	gfc_error ("Symbol %qs in namelist %qs is INTENT(IN) at %C",
		   p->sym->name, sym->name);
	return 1;
      }

  return 0;
}


/* Match a single data transfer element.  */

static match
match_dt_element (io_kind k, gfc_dt *dt)
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
	  gfc_error ("Symbol %qs at %C must be a NAMELIST group name",
		     sym != NULL ? sym->name : name);
	  return MATCH_ERROR;
	}

      dt->namelist = sym;
      if (k == M_READ && check_namelist (sym))
	return MATCH_ERROR;

      return MATCH_YES;
    }

  m = match_etag (&tag_e_async, &dt->asynchronous);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_blank, &dt->blank);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_delim, &dt->delim);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_pad, &dt->pad);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_sign, &dt->sign);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_round, &dt->round);
  if (m != MATCH_NO)
    return m;
  m = match_out_tag (&tag_id, &dt->id);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_e_decimal, &dt->decimal);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_rec, &dt->rec);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_spos, &dt->pos);
  if (m != MATCH_NO)
    return m;
  m = match_etag (&tag_iomsg, &dt->iomsg);
  if (m != MATCH_NO)
    return m;

  m = match_out_tag (&tag_iostat, &dt->iostat);
  if (m != MATCH_NO)
    return m;
  m = match_ltag (&tag_err, &dt->err);
  if (m == MATCH_YES)
    dt->err_where = gfc_current_locus;
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
gfc_free_dt (gfc_dt *dt)
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
  gfc_free_expr (dt->pad);
  gfc_free_expr (dt->delim);
  gfc_free_expr (dt->sign);
  gfc_free_expr (dt->round);
  gfc_free_expr (dt->blank);
  gfc_free_expr (dt->decimal);
  gfc_free_expr (dt->pos);
  gfc_free_expr (dt->dt_io_kind);
  /* dt->extra_comma is a link to dt_io_kind if it is set.  */
  free (dt);
}


static const char *
io_kind_name (io_kind k);

static bool
check_io_constraints (io_kind k, gfc_dt *dt, gfc_code *io_code,
		      locus *spec_end);

/* Resolve everything in a gfc_dt structure.  */

bool
gfc_resolve_dt (gfc_code *dt_code, gfc_dt *dt, locus *loc)
{
  gfc_expr *e;
  io_kind k;

  /* This is set in any case.  */
  gcc_assert (dt->dt_io_kind);
  k = dt->dt_io_kind->value.iokind;

  RESOLVE_TAG (&tag_format, dt->format_expr);
  RESOLVE_TAG (&tag_rec, dt->rec);
  RESOLVE_TAG (&tag_spos, dt->pos);
  RESOLVE_TAG (&tag_advance, dt->advance);
  RESOLVE_TAG (&tag_id, dt->id);
  RESOLVE_TAG (&tag_iomsg, dt->iomsg);
  RESOLVE_TAG (&tag_iostat, dt->iostat);
  RESOLVE_TAG (&tag_size, dt->size);
  RESOLVE_TAG (&tag_e_pad, dt->pad);
  RESOLVE_TAG (&tag_e_delim, dt->delim);
  RESOLVE_TAG (&tag_e_sign, dt->sign);
  RESOLVE_TAG (&tag_e_round, dt->round);
  RESOLVE_TAG (&tag_e_blank, dt->blank);
  RESOLVE_TAG (&tag_e_decimal, dt->decimal);
  RESOLVE_TAG (&tag_e_async, dt->asynchronous);

  /* Check I/O constraints.
     To validate NAMELIST we need to check if we were also given an I/O list,
     which is stored in code->block->next with op EXEC_TRANSFER.
     Note that the I/O list was already resolved from resolve_transfer.  */
  gfc_code *io_code = NULL;
  if (dt_code && dt_code->block && dt_code->block->next
      && dt_code->block->next->op == EXEC_TRANSFER)
    io_code = dt_code->block->next;

  if (!check_io_constraints (k, dt, io_code, loc))
    return false;

  e = dt->io_unit;
  if (e == NULL)
    {
      gfc_error ("UNIT not specified at %L", loc);
      return false;
    }

  if (e->symtree && e->symtree->n.sym->attr.flavor == FL_PARAMETER
      && e->ts.type == BT_CHARACTER)
    {
      gfc_error ("UNIT specification at %L must "
      "not be a character PARAMETER", &e->where);
      return false;
    }

  if (gfc_resolve_expr (e)
      && (e->ts.type != BT_INTEGER
	  && (e->ts.type != BT_CHARACTER || e->expr_type != EXPR_VARIABLE)))
    {
      /* If there is no extra comma signifying the "format" form of the IO
	 statement, then this must be an error.  */
      if (!dt->extra_comma)
	{
	  gfc_error ("UNIT specification at %L must be an INTEGER expression "
		     "or a CHARACTER variable", &e->where);
	  return false;
	}
      else
	{
	  /* At this point, we have an extra comma.  If io_unit has arrived as
	     type character, we assume its really the "format" form of the I/O
	     statement.  We set the io_unit to the default unit and format to
	     the character expression.  See F95 Standard section 9.4.  */
	  if (e->ts.type == BT_CHARACTER && (k == M_READ || k == M_PRINT))
	    {
	      dt->format_expr = dt->io_unit;
	      dt->io_unit = default_unit (k);

	      /* Nullify this pointer now so that a warning/error is not
		 triggered below for the "Extension".  */
	      dt->extra_comma = NULL;
	    }

	  if (k == M_WRITE)
	    {
	      gfc_error ("Invalid form of WRITE statement at %L, UNIT required",
			 &dt->extra_comma->where);
	      return false;
	    }
	}
    }

  if (e->ts.type == BT_CHARACTER)
    {
      if (gfc_has_vector_index (e))
	{
	  gfc_error ("Internal unit with vector subscript at %L", &e->where);
	  return false;
	}

      /* If we are writing, make sure the internal unit can be changed.  */
      gcc_assert (k != M_PRINT);
      if (k == M_WRITE
	  && !gfc_check_vardef_context (e, false, false, false,
					_("internal unit in WRITE")))
	return false;
    }

  if (e->rank && e->ts.type != BT_CHARACTER)
    {
      gfc_error ("External IO UNIT cannot be an array at %L", &e->where);
      return false;
    }

  if (e->expr_type == EXPR_CONSTANT && e->ts.type == BT_INTEGER
      && mpz_sgn (e->value.integer) < 0)
    {
      gfc_error ("UNIT number in statement at %L must be non-negative",
		 &e->where);
      return false;
    }

  /* If we are reading and have a namelist, check that all namelist symbols
     can appear in a variable definition context.  */
  if (dt->namelist)
    {
      gfc_namelist* n;
      for (n = dt->namelist->namelist; n; n = n->next)
	{
	  gfc_expr* e;
	  bool t;

	  if (k == M_READ)
	    {
	      e = gfc_get_variable_expr (gfc_find_sym_in_symtree (n->sym));
	      t = gfc_check_vardef_context (e, false, false, false, NULL);
	      gfc_free_expr (e);

	      if (!t)
		{
		  gfc_error ("NAMELIST %qs in READ statement at %L contains"
			     " the symbol %qs which may not appear in a"
			     " variable definition context",
			     dt->namelist->name, loc, n->sym->name);
		  return false;
		}
	    }

	  t = dtio_procs_present (n->sym, k);

	  if (n->sym->ts.type == BT_CLASS && !t)
	    {
	      gfc_error ("NAMELIST object %qs in namelist %qs at %L is "
			 "polymorphic and requires a defined input/output "
			 "procedure", n->sym->name, dt->namelist->name, loc);
	      return false;
	    }

	  if ((n->sym->ts.type == BT_DERIVED)
	      && (n->sym->ts.u.derived->attr.alloc_comp
		  || n->sym->ts.u.derived->attr.pointer_comp))
	    {
	      if (!gfc_notify_std (GFC_STD_F2003, "NAMELIST object %qs in "
				   "namelist %qs at %L with ALLOCATABLE "
				   "or POINTER components", n->sym->name,
				   dt->namelist->name, loc))
		return false;

	      if (!t)
		{
		  gfc_error ("NAMELIST object %qs in namelist %qs at %L has "
			     "ALLOCATABLE or POINTER components and thus requires "
			     "a defined input/output procedure", n->sym->name,
			     dt->namelist->name, loc);
		  return false;
		}
	    }
	}
    }

  if (dt->extra_comma
      && !gfc_notify_std (GFC_STD_LEGACY, "Comma before i/o item list at %L",
			  &dt->extra_comma->where))
    return false;

  if (dt->err)
    {
      if (!gfc_reference_st_label (dt->err, ST_LABEL_TARGET))
	return false;
      if (dt->err->defined == ST_LABEL_UNKNOWN)
	{
	  gfc_error ("ERR tag label %d at %L not defined",
		      dt->err->value, &dt->err_where);
	  return false;
	}
    }

  if (dt->end)
    {
      if (!gfc_reference_st_label (dt->end, ST_LABEL_TARGET))
	return false;
      if (dt->end->defined == ST_LABEL_UNKNOWN)
	{
	  gfc_error ("END tag label %d at %L not defined",
		      dt->end->value, &dt->end_where);
	  return false;
	}
    }

  if (dt->eor)
    {
      if (!gfc_reference_st_label (dt->eor, ST_LABEL_TARGET))
	return false;
      if (dt->eor->defined == ST_LABEL_UNKNOWN)
	{
	  gfc_error ("EOR tag label %d at %L not defined",
		      dt->eor->value, &dt->eor_where);
	  return false;
	}
    }

  /* Check the format label actually exists.  */
  if (dt->format_label && dt->format_label != &format_asterisk
      && dt->format_label->defined == ST_LABEL_UNKNOWN)
    {
      gfc_error ("FORMAT label %d at %L not defined", dt->format_label->value,
		 loc);
      return false;
    }

  return true;
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

static match match_io_element (io_kind, gfc_code **);

static match
match_io_iterator (io_kind k, gfc_code **result)
{
  gfc_code *head, *tail, *new_code;
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

      m = match_io_element (k, &new_code);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	{
	  if (n > 2)
	    goto syntax;
	  goto cleanup;
	}

      tail = gfc_append_code (tail, new_code);

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

  new_code = gfc_get_code (EXEC_DO);
  new_code->ext.iterator = iter;

  new_code->block = gfc_get_code (EXEC_DO);
  new_code->block->next = head;

  *result = new_code;
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
match_io_element (io_kind k, gfc_code **cpp)
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
	{
	  gfc_error ("Expecting variable in READ statement at %C");
	  m = MATCH_ERROR;
	}

      if (m == MATCH_YES && expr->expr_type == EXPR_CONSTANT)
	{
	  gfc_error ("Expecting variable or io-implied-do in READ statement "
		   "at %L", &expr->where);
	  m = MATCH_ERROR;
	}

      if (m == MATCH_YES
	  && expr->expr_type == EXPR_VARIABLE
	  && expr->symtree->n.sym->attr.external)
	{
	  gfc_error ("Expecting variable or io-implied-do at %L",
		     &expr->where);
	  m = MATCH_ERROR;
	}
    }
  else
    {
      m = gfc_match_expr (&expr);
      if (m == MATCH_NO)
	gfc_error ("Expected expression in %s statement at %C",
		   io_kind_name (k));

      if (m == MATCH_YES && expr->ts.type == BT_BOZ)
	{
	  if (gfc_invalid_boz (G_("BOZ literal constant at %L cannot appear in"
				" an output IO list"), &gfc_current_locus))
	    return MATCH_ERROR;
	  if (!gfc_boz2int (expr, gfc_max_integer_kind))
	    return MATCH_ERROR;
	};
    }

  if (m == MATCH_YES && k == M_READ && gfc_check_do_variable (expr->symtree))
    m = MATCH_ERROR;

  if (m != MATCH_YES)
    {
      gfc_free_expr (expr);
      return MATCH_ERROR;
    }

  cp = gfc_get_code (EXEC_TRANSFER);
  cp->expr1 = expr;
  if (k != M_INQUIRE)
    cp->ext.dt = current_dt;

  *cpp = cp;
  return MATCH_YES;
}


/* Match an I/O list, building gfc_code structures as we go.  */

static match
match_io_list (io_kind k, gfc_code **head_p)
{
  gfc_code *head, *tail, *new_code;
  match m;

  *head_p = head = tail = NULL;
  if (gfc_match_eos () == MATCH_YES)
    return MATCH_YES;

  for (;;)
    {
      m = match_io_element (k, &new_code);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      tail = gfc_append_code (tail, new_code);
      if (head == NULL)
	head = new_code;

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
terminate_io (gfc_code *io_code)
{
  gfc_code *c;

  if (io_code == NULL)
    io_code = new_st.block;

  c = gfc_get_code (EXEC_DT_END);

  /* Point to structure that is already there */
  c->ext.dt = new_st.ext.dt;
  gfc_append_code (io_code, c);
}


/* Check the constraints for a data transfer statement.  The majority of the
   constraints appearing in 9.4 of the standard appear here.

   Tag expressions are already resolved by resolve_tag, which includes
   verifying the type, that they are scalar, and verifying that BT_CHARACTER
   tags are of default kind.  */

static bool
check_io_constraints (io_kind k, gfc_dt *dt, gfc_code *io_code,
		      locus *spec_end)
{
#define io_constraint(condition, msg, where)\
if (condition) \
  {\
    if (GFC_LOCUS_IS_SET (*where))\
      gfc_error ((msg), (where));\
    else\
      gfc_error ((msg), spec_end);\
    return false;\
  }

  gfc_expr *expr;
  gfc_symbol *sym = NULL;
  bool warn, unformatted;

  warn = (dt->err || dt->iostat) ? true : false;
  unformatted = dt->format_expr == NULL && dt->format_label == NULL
		&& dt->namelist == NULL;

  expr = dt->io_unit;
  if (expr && expr->expr_type == EXPR_VARIABLE
      && expr->ts.type == BT_CHARACTER)
    {
      sym = expr->symtree->n.sym;

      io_constraint (k == M_WRITE && sym->attr.intent == INTENT_IN,
		     "Internal file at %L must not be INTENT(IN)",
		     &expr->where);

      io_constraint (gfc_has_vector_index (dt->io_unit),
		     "Internal file incompatible with vector subscript at %L",
		     &expr->where);

      io_constraint (dt->rec != NULL,
		     "REC tag at %L is incompatible with internal file",
		     &dt->rec->where);

      io_constraint (dt->pos != NULL,
		     "POS tag at %L is incompatible with internal file",
		     &dt->pos->where);

      io_constraint (unformatted,
		     "Unformatted I/O not allowed with internal unit at %L",
		     &dt->io_unit->where);

      io_constraint (dt->asynchronous != NULL,
		     "ASYNCHRONOUS tag at %L not allowed with internal file",
		     &dt->asynchronous->where);

      if (dt->namelist != NULL)
	{
	  if (!gfc_notify_std (GFC_STD_F2003, "Internal file at %L with "
			       "namelist", &expr->where))
	    return false;
	}

      io_constraint (dt->advance != NULL,
		     "ADVANCE tag at %L is incompatible with internal file",
		     &dt->advance->where);
    }

  if (expr && expr->ts.type != BT_CHARACTER)
    {

      if (gfc_pure (NULL) && (k == M_READ || k == M_WRITE))
	{
	  gfc_error ("IO UNIT in %s statement at %L must be "
		     "an internal file in a PURE procedure",
		     io_kind_name (k), &expr->where);
	  return false;
	}

      if (k == M_READ || k == M_WRITE)
	gfc_unset_implicit_pure (NULL);
    }

  if (dt->asynchronous)
    {
      int num = -1;
      static const char * asynchronous[] = { "YES", "NO", NULL };

      /* Note: gfc_reduce_init_expr reports an error if not init-expr.  */
      if (!gfc_reduce_init_expr (dt->asynchronous))
	return false;

      if (!compare_to_allowed_values
		("ASYNCHRONOUS", asynchronous, NULL, NULL,
		 dt->asynchronous->value.character.string,
		 io_kind_name (k), warn, &dt->asynchronous->where, &num))
	return false;

      gcc_checking_assert (num != -1);

      /* For "YES", mark related symbols as asynchronous.  */
      if (num == 0)
	{
	  /* SIZE variable.  */
	  if (dt->size)
	    dt->size->symtree->n.sym->attr.asynchronous = 1;

	  /* Variables in a NAMELIST.  */
	  if (dt->namelist)
	    for (gfc_namelist *nl = dt->namelist->namelist; nl; nl = nl->next)
	      nl->sym->attr.asynchronous = 1;

	  /* Variables in an I/O list.  */
	  for (gfc_code *xfer = io_code; xfer && xfer->op == EXEC_TRANSFER;
	       xfer = xfer->next)
	    {
	      gfc_expr *expr = xfer->expr1;
	      while (expr != NULL && expr->expr_type == EXPR_OP
		     && expr->value.op.op == INTRINSIC_PARENTHESES)
		expr = expr->value.op.op1;

	      if (expr && expr->expr_type == EXPR_VARIABLE)
		expr->symtree->n.sym->attr.asynchronous = 1;
	    }
	}
    }

  if (dt->id)
    {
      bool not_yes
	= !dt->asynchronous
	  || gfc_wide_strlen (dt->asynchronous->value.character.string) != 3
	  || gfc_wide_strncasecmp (dt->asynchronous->value.character.string,
				   "yes", 3) != 0;
      io_constraint (not_yes,
		     "ID= specifier at %L must be with ASYNCHRONOUS='yes' "
		     "specifier", &dt->id->where);
    }

  if (dt->decimal)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "DECIMAL= at %L "
			   "not allowed in Fortran 95", &dt->decimal->where))
	return false;

      if (dt->decimal->expr_type == EXPR_CONSTANT)
	{
	  static const char * decimal[] = { "COMMA", "POINT", NULL };

	  if (!compare_to_allowed_values ("DECIMAL", decimal, NULL, NULL,
					  dt->decimal->value.character.string,
					  io_kind_name (k), warn,
					  &dt->decimal->where))
	    return false;

	  io_constraint (unformatted,
			 "the DECIMAL= specifier at %L must be with an "
			 "explicit format expression", &dt->decimal->where);
	}
    }

  if (dt->blank)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "BLANK= at %L "
			   "not allowed in Fortran 95", &dt->blank->where))
	return false;

      if (dt->blank->expr_type == EXPR_CONSTANT)
	{
	  static const char * blank[] = { "NULL", "ZERO", NULL };


	  if (!compare_to_allowed_values ("BLANK", blank, NULL, NULL,
					  dt->blank->value.character.string,
					  io_kind_name (k), warn,
					  &dt->blank->where))
	    return false;

	  io_constraint (unformatted,
			 "the BLANK= specifier at %L must be with an "
			 "explicit format expression", &dt->blank->where);
	}
    }

  if (dt->pad)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "PAD= at %L "
			   "not allowed in Fortran 95", &dt->pad->where))
	return false;

      if (dt->pad->expr_type == EXPR_CONSTANT)
	{
	  static const char * pad[] = { "YES", "NO", NULL };

	  if (!compare_to_allowed_values ("PAD", pad, NULL, NULL,
					  dt->pad->value.character.string,
					  io_kind_name (k), warn,
					  &dt->pad->where))
	    return false;

	  io_constraint (unformatted,
			 "the PAD= specifier at %L must be with an "
			 "explicit format expression", &dt->pad->where);
	}
    }

  if (dt->round)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "ROUND= at %L "
			   "not allowed in Fortran 95", &dt->round->where))
	return false;

      if (dt->round->expr_type == EXPR_CONSTANT)
	{
	  static const char * round[] = { "UP", "DOWN", "ZERO", "NEAREST",
					  "COMPATIBLE", "PROCESSOR_DEFINED",
					  NULL };

	  if (!compare_to_allowed_values ("ROUND", round, NULL, NULL,
					  dt->round->value.character.string,
					  io_kind_name (k), warn,
					  &dt->round->where))
	    return false;
	}
    }

  if (dt->sign)
    {
      /* When implemented, change the following to use gfc_notify_std F2003.
      if (gfc_notify_std (GFC_STD_F2003, "SIGN= at %L "
	  "not allowed in Fortran 95", &dt->sign->where) == false)
	return false;  */

      if (dt->sign->expr_type == EXPR_CONSTANT)
	{
	  static const char * sign[] = { "PLUS", "SUPPRESS", "PROCESSOR_DEFINED",
					 NULL };

	  if (!compare_to_allowed_values ("SIGN", sign, NULL, NULL,
				      dt->sign->value.character.string,
				      io_kind_name (k), warn, &dt->sign->where))
	    return false;

	  io_constraint (unformatted,
			 "SIGN= specifier at %L must be with an "
			 "explicit format expression", &dt->sign->where);

	  io_constraint (k == M_READ,
			 "SIGN= specifier at %L not allowed in a "
			 "READ statement", &dt->sign->where);
	}
    }

  if (dt->delim)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "DELIM= at %L "
			   "not allowed in Fortran 95", &dt->delim->where))
	return false;

      if (dt->delim->expr_type == EXPR_CONSTANT)
	{
	  static const char *delim[] = { "APOSTROPHE", "QUOTE", "NONE", NULL };

	  if (!compare_to_allowed_values ("DELIM", delim, NULL, NULL,
					  dt->delim->value.character.string,
					  io_kind_name (k), warn,
					  &dt->delim->where))
	    return false;

	  io_constraint (k == M_READ,
			 "DELIM= specifier at %L not allowed in a "
			 "READ statement", &dt->delim->where);

	  io_constraint (dt->format_label != &format_asterisk
			 && dt->namelist == NULL,
			 "DELIM= specifier at %L must have FMT=*",
			 &dt->delim->where);

	  io_constraint (unformatted && dt->namelist == NULL,
			 "DELIM= specifier at %L must be with FMT=* or "
			 "NML= specifier", &dt->delim->where);
	}
    }

  if (dt->namelist)
    {
      io_constraint (io_code && dt->namelist,
		     "NAMELIST cannot be followed by IO-list at %L",
		     &io_code->loc);

      io_constraint (dt->format_expr,
		     "IO spec-list cannot contain both NAMELIST group name "
		     "and format specification at %L",
		     &dt->format_expr->where);

      io_constraint (dt->format_label,
		     "IO spec-list cannot contain both NAMELIST group name "
		     "and format label at %L", spec_end);

      io_constraint (dt->rec,
		     "NAMELIST IO is not allowed with a REC= specifier "
		     "at %L", &dt->rec->where);

      io_constraint (dt->advance,
		     "NAMELIST IO is not allowed with a ADVANCE= specifier "
		     "at %L", &dt->advance->where);
    }

  if (dt->rec)
    {
      io_constraint (dt->end,
		     "An END tag is not allowed with a "
		     "REC= specifier at %L", &dt->end_where);

      io_constraint (dt->format_label == &format_asterisk,
		     "FMT=* is not allowed with a REC= specifier "
		     "at %L", spec_end);

      io_constraint (dt->pos,
		     "POS= is not allowed with REC= specifier "
		     "at %L", &dt->pos->where);
    }

  if (dt->advance)
    {
      int not_yes, not_no;
      expr = dt->advance;

      io_constraint (dt->format_label == &format_asterisk,
		     "List directed format(*) is not allowed with a "
		     "ADVANCE= specifier at %L.", &expr->where);

      io_constraint (unformatted,
		     "the ADVANCE= specifier at %L must appear with an "
		     "explicit format expression", &expr->where);

      if (expr->expr_type == EXPR_CONSTANT && expr->ts.type == BT_CHARACTER)
	{
	  const gfc_char_t *advance = expr->value.character.string;
	  not_no = gfc_wide_strlen (advance) != 2
		   || gfc_wide_strncasecmp (advance, "no", 2) != 0;
	  not_yes = gfc_wide_strlen (advance) != 3
		    || gfc_wide_strncasecmp (advance, "yes", 3) != 0;
	}
      else
	{
	  not_no = 0;
	  not_yes = 0;
	}

      io_constraint (not_no && not_yes,
		     "ADVANCE= specifier at %L must have value = "
		     "YES or NO.", &expr->where);

      io_constraint (dt->size && not_no && k == M_READ,
		     "SIZE tag at %L requires an ADVANCE = %<NO%>",
		     &dt->size->where);

      io_constraint (dt->eor && not_no && k == M_READ,
		     "EOR tag at %L requires an ADVANCE = %<NO%>",
		     &dt->eor_where);
    }

  if (k != M_READ)
    {
      io_constraint (dt->end, "END tag not allowed with output at %L",
		     &dt->end_where);

      io_constraint (dt->eor, "EOR tag not allowed with output at %L",
		     &dt->eor_where);

      io_constraint (dt->blank,
		     "BLANK= specifier not allowed with output at %L",
		     &dt->blank->where);

      io_constraint (dt->pad, "PAD= specifier not allowed with output at %L",
		     &dt->pad->where);

      io_constraint (dt->size, "SIZE= specifier not allowed with output at %L",
		     &dt->size->where);
    }
  else
    {
      io_constraint (dt->size && dt->advance == NULL,
		     "SIZE tag at %L requires an ADVANCE tag",
		     &dt->size->where);

      io_constraint (dt->eor && dt->advance == NULL,
		     "EOR tag at %L requires an ADVANCE tag",
		     &dt->eor_where);
    }

  return true;
#undef io_constraint
}


/* Match a READ, WRITE or PRINT statement.  */

static match
match_io (io_kind k)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_code *io_code;
  gfc_symbol *sym;
  int comma_flag;
  locus where;
  locus control;
  gfc_dt *dt;
  match m;

  where = gfc_current_locus;
  comma_flag = 0;
  current_dt = dt = XCNEW (gfc_dt);
  m = gfc_match_char ('(');
  if (m == MATCH_NO)
    {
      where = gfc_current_locus;
      if (k == M_WRITE)
	goto syntax;
      else if (k == M_PRINT)
	{
	  /* Treat the non-standard case of PRINT namelist.  */
	  if ((gfc_current_form == FORM_FIXED || gfc_peek_ascii_char () == ' ')
	      && gfc_match_name (name) == MATCH_YES)
	    {
	      gfc_find_symbol (name, NULL, 1, &sym);
	      if (sym && sym->attr.flavor == FL_NAMELIST)
		{
		  if (!gfc_notify_std (GFC_STD_GNU, "PRINT namelist at "
				       "%C is an extension"))
		    {
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

	  if (gfc_match_char ('*') == MATCH_YES
	      && gfc_match_char(',') == MATCH_YES)
	    {
	      locus where2 = gfc_current_locus;
	      if (gfc_match_eos () == MATCH_YES)
		{
		  gfc_current_locus = where2;
		  gfc_error ("Comma after * at %C not allowed without I/O list");
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	      else
		gfc_current_locus = where;
	    }
	  else
	    gfc_current_locus = where;
	}

      if (gfc_current_form == FORM_FREE)
	{
	  char c = gfc_peek_ascii_char ();
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
  else
    {
      /* Before issuing an error for a malformed 'print (1,*)' type of
	 error, check for a default-char-expr of the form ('(I0)').  */
      if (m == MATCH_YES)
        {
	  control = gfc_current_locus;
	  if (k == M_PRINT)
	    {
	      /* Reset current locus to get the initial '(' in an expression.  */
	      gfc_current_locus = where;
	      dt->format_expr = NULL;
	      m = match_dt_format (dt);

	      if (m == MATCH_ERROR)
		goto cleanup;
	      if (m == MATCH_NO || dt->format_expr == NULL)
		goto syntax;

	      comma_flag = 1;
	      dt->io_unit = default_unit (k);
	      goto get_io_list;
	    }
	  if (k == M_READ)
	    {
	      /* Commit any pending symbols now so that when we undo
		 symbols later we wont lose them.  */
	      gfc_commit_symbols ();
	      /* Reset current locus to get the initial '(' in an expression.  */
	      gfc_current_locus = where;
	      dt->format_expr = NULL;
	      m = gfc_match_expr (&dt->format_expr);
	      if (m == MATCH_YES)
	        {
		  if (dt->format_expr
		      && dt->format_expr->ts.type == BT_CHARACTER)
		    {
		      comma_flag = 1;
		      dt->io_unit = default_unit (k);
		      goto get_io_list;
		    }
		  else
		    {
		      gfc_free_expr (dt->format_expr);
		      dt->format_expr = NULL;
		      gfc_current_locus = control;
		    }
		}
	      else
	        {
		  gfc_clear_error ();
		  gfc_undo_symbols ();
		  gfc_free_expr (dt->format_expr);
		  dt->format_expr = NULL;
		  gfc_current_locus = control;
		}
	    }
	}
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

  /* Save the IO kind for later use.  */
  dt->dt_io_kind = gfc_get_iokind_expr (&gfc_current_locus, k);

  /* Optional leading comma (non-standard).  We use a gfc_expr structure here
     to save the locus.  This is used later when resolving transfer statements
     that might have a format expression without unit number.  */
  if (!comma_flag && gfc_match_char (',') == MATCH_YES)
    dt->extra_comma = dt->dt_io_kind;

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

  /* See if we want to use defaults for missing exponents in real transfers
     and other DEC runtime extensions. */
  if (flag_dec_format_defaults)
    dt->dec_ext = 1;

  /* Check the format string now.  */
  if (dt->format_expr
      && (!gfc_simplify_expr (dt->format_expr, 0)
	  || !check_format_string (dt->format_expr, k == M_READ)))
    return MATCH_ERROR;

  new_st.op = (k == M_READ) ? EXEC_READ : EXEC_WRITE;
  new_st.ext.dt = dt;
  new_st.block = gfc_get_code (new_st.op);
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

  gfc_unset_implicit_pure (NULL);

  return MATCH_YES;
}


/* Free a gfc_inquire structure.  */

void
gfc_free_inquire (gfc_inquire *inquire)
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
  gfc_free_expr (inquire->encoding);
  gfc_free_expr (inquire->pad);
  gfc_free_expr (inquire->iolength);
  gfc_free_expr (inquire->convert);
  gfc_free_expr (inquire->strm_pos);
  gfc_free_expr (inquire->asynchronous);
  gfc_free_expr (inquire->decimal);
  gfc_free_expr (inquire->pending);
  gfc_free_expr (inquire->id);
  gfc_free_expr (inquire->sign);
  gfc_free_expr (inquire->size);
  gfc_free_expr (inquire->round);
  gfc_free_expr (inquire->share);
  gfc_free_expr (inquire->cc);
  free (inquire);
}


/* Match an element of an INQUIRE statement.  */

#define RETM   if (m != MATCH_NO) return m;

static match
match_inquire_element (gfc_inquire *inquire)
{
  match m;

  m = match_etag (&tag_unit, &inquire->unit);
  RETM m = match_etag (&tag_file, &inquire->file);
  RETM m = match_ltag (&tag_err, &inquire->err);
  RETM m = match_etag (&tag_iomsg, &inquire->iomsg);
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
  RETM m = match_vtag (&tag_s_async, &inquire->asynchronous);
  RETM m = match_vtag (&tag_s_delim, &inquire->delim);
  RETM m = match_vtag (&tag_s_decimal, &inquire->decimal);
  RETM m = match_out_tag (&tag_size, &inquire->size);
  RETM m = match_vtag (&tag_s_encoding, &inquire->encoding);
  RETM m = match_vtag (&tag_s_round, &inquire->round);
  RETM m = match_vtag (&tag_s_sign, &inquire->sign);
  RETM m = match_vtag (&tag_s_pad, &inquire->pad);
  RETM m = match_out_tag (&tag_iolength, &inquire->iolength);
  RETM m = match_vtag (&tag_convert, &inquire->convert);
  RETM m = match_out_tag (&tag_strm_out, &inquire->strm_pos);
  RETM m = match_vtag (&tag_pending, &inquire->pending);
  RETM m = match_vtag (&tag_id, &inquire->id);
  RETM m = match_vtag (&tag_s_iqstream, &inquire->iqstream);
  RETM m = match_dec_vtag (&tag_v_share, &inquire->share);
  RETM m = match_dec_vtag (&tag_v_cc, &inquire->cc);
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

  inquire = XCNEW (gfc_inquire);

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

      for (gfc_code *c = code; c; c = c->next)
	if (c->expr1 && c->expr1->expr_type == EXPR_FUNCTION
	    && c->expr1->symtree && c->expr1->symtree->n.sym->attr.function
	    && !c->expr1->symtree->n.sym->attr.external
	    && strcmp (c->expr1->symtree->name, "null") == 0)
	  {
	    gfc_error ("NULL() near %L cannot appear in INQUIRE statement",
		       &c->expr1->where);
	    goto cleanup;
	  }

      new_st.op = EXEC_IOLENGTH;
      new_st.expr1 = inquire->iolength;
      new_st.ext.inquire = inquire;

      if (gfc_pure (NULL))
	{
	  gfc_free_statements (code);
	  gfc_error ("INQUIRE statement not allowed in PURE procedure at %C");
	  return MATCH_ERROR;
	}

      gfc_unset_implicit_pure (NULL);

      new_st.block = gfc_get_code (EXEC_IOLENGTH);
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
      gfc_error ("INQUIRE statement at %L cannot contain both FILE and "
		 "UNIT specifiers", &loc);
      goto cleanup;
    }

  if (inquire->unit == NULL && inquire->file == NULL)
    {
      gfc_error ("INQUIRE statement at %L requires either FILE or "
		 "UNIT specifier", &loc);
      goto cleanup;
    }

  if (inquire->unit != NULL && inquire->unit->expr_type == EXPR_CONSTANT
      && inquire->unit->ts.type == BT_INTEGER
      && ((mpz_get_si (inquire->unit->value.integer) == GFC_INTERNAL_UNIT4)
      || (mpz_get_si (inquire->unit->value.integer) == GFC_INTERNAL_UNIT)))
    {
      gfc_error ("UNIT number in INQUIRE statement at %L cannot "
		 "be %d", &loc, (int) mpz_get_si (inquire->unit->value.integer));
      goto cleanup;
    }

  if (gfc_pure (NULL))
    {
      gfc_error ("INQUIRE statement not allowed in PURE procedure at %C");
      goto cleanup;
    }

  gfc_unset_implicit_pure (NULL);

  if (inquire->id != NULL && inquire->pending == NULL)
    {
      gfc_error ("INQUIRE statement at %L requires a PENDING= specifier with "
		 "the ID= specifier", &loc);
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

bool
gfc_resolve_inquire (gfc_inquire *inquire)
{
  RESOLVE_TAG (&tag_unit, inquire->unit);
  RESOLVE_TAG (&tag_file, inquire->file);
  RESOLVE_TAG (&tag_id, inquire->id);

  /* For INQUIRE, all tags except FILE, ID and UNIT are variable definition
     contexts.  Thus, use an extended RESOLVE_TAG macro for that.  */
#define INQUIRE_RESOLVE_TAG(tag, expr) \
  RESOLVE_TAG (tag, expr); \
  if (expr) \
    { \
      char context[64]; \
      sprintf (context, _("%s tag with INQUIRE"), (tag)->name); \
      if (gfc_check_vardef_context ((expr), false, false, false, \
				    context) == false) \
	return false; \
    }
  INQUIRE_RESOLVE_TAG (&tag_iomsg, inquire->iomsg);
  INQUIRE_RESOLVE_TAG (&tag_iostat, inquire->iostat);
  INQUIRE_RESOLVE_TAG (&tag_exist, inquire->exist);
  INQUIRE_RESOLVE_TAG (&tag_opened, inquire->opened);
  INQUIRE_RESOLVE_TAG (&tag_number, inquire->number);
  INQUIRE_RESOLVE_TAG (&tag_named, inquire->named);
  INQUIRE_RESOLVE_TAG (&tag_name, inquire->name);
  INQUIRE_RESOLVE_TAG (&tag_s_access, inquire->access);
  INQUIRE_RESOLVE_TAG (&tag_sequential, inquire->sequential);
  INQUIRE_RESOLVE_TAG (&tag_direct, inquire->direct);
  INQUIRE_RESOLVE_TAG (&tag_s_form, inquire->form);
  INQUIRE_RESOLVE_TAG (&tag_formatted, inquire->formatted);
  INQUIRE_RESOLVE_TAG (&tag_unformatted, inquire->unformatted);
  INQUIRE_RESOLVE_TAG (&tag_s_recl, inquire->recl);
  INQUIRE_RESOLVE_TAG (&tag_nextrec, inquire->nextrec);
  INQUIRE_RESOLVE_TAG (&tag_s_blank, inquire->blank);
  INQUIRE_RESOLVE_TAG (&tag_s_position, inquire->position);
  INQUIRE_RESOLVE_TAG (&tag_s_action, inquire->action);
  INQUIRE_RESOLVE_TAG (&tag_read, inquire->read);
  INQUIRE_RESOLVE_TAG (&tag_write, inquire->write);
  INQUIRE_RESOLVE_TAG (&tag_readwrite, inquire->readwrite);
  INQUIRE_RESOLVE_TAG (&tag_s_delim, inquire->delim);
  INQUIRE_RESOLVE_TAG (&tag_s_pad, inquire->pad);
  INQUIRE_RESOLVE_TAG (&tag_s_encoding, inquire->encoding);
  INQUIRE_RESOLVE_TAG (&tag_s_round, inquire->round);
  INQUIRE_RESOLVE_TAG (&tag_iolength, inquire->iolength);
  INQUIRE_RESOLVE_TAG (&tag_convert, inquire->convert);
  INQUIRE_RESOLVE_TAG (&tag_strm_out, inquire->strm_pos);
  INQUIRE_RESOLVE_TAG (&tag_s_async, inquire->asynchronous);
  INQUIRE_RESOLVE_TAG (&tag_s_sign, inquire->sign);
  INQUIRE_RESOLVE_TAG (&tag_s_round, inquire->round);
  INQUIRE_RESOLVE_TAG (&tag_pending, inquire->pending);
  INQUIRE_RESOLVE_TAG (&tag_size, inquire->size);
  INQUIRE_RESOLVE_TAG (&tag_s_decimal, inquire->decimal);
  INQUIRE_RESOLVE_TAG (&tag_s_iqstream, inquire->iqstream);
  INQUIRE_RESOLVE_TAG (&tag_v_share, inquire->share);
  INQUIRE_RESOLVE_TAG (&tag_v_cc, inquire->cc);
#undef INQUIRE_RESOLVE_TAG

  if (!gfc_reference_st_label (inquire->err, ST_LABEL_TARGET))
    return false;

  return true;
}


void
gfc_free_wait (gfc_wait *wait)
{
  if (wait == NULL)
    return;

  gfc_free_expr (wait->unit);
  gfc_free_expr (wait->iostat);
  gfc_free_expr (wait->iomsg);
  gfc_free_expr (wait->id);
  free (wait);
}


bool
gfc_resolve_wait (gfc_wait *wait)
{
  RESOLVE_TAG (&tag_unit, wait->unit);
  RESOLVE_TAG (&tag_iomsg, wait->iomsg);
  RESOLVE_TAG (&tag_iostat, wait->iostat);
  RESOLVE_TAG (&tag_id, wait->id);

  if (!gfc_reference_st_label (wait->err, ST_LABEL_TARGET))
    return false;

  if (!gfc_reference_st_label (wait->end, ST_LABEL_TARGET))
    return false;

  return true;
}

/* Match an element of a WAIT statement.  */

#define RETM   if (m != MATCH_NO) return m;

static match
match_wait_element (gfc_wait *wait)
{
  match m;

  m = match_etag (&tag_unit, &wait->unit);
  RETM m = match_ltag (&tag_err, &wait->err);
  RETM m = match_ltag (&tag_end, &wait->end);
  RETM m = match_ltag (&tag_eor, &wait->eor);
  RETM m = match_etag (&tag_iomsg, &wait->iomsg);
  RETM m = match_out_tag (&tag_iostat, &wait->iostat);
  RETM m = match_etag (&tag_id, &wait->id);
  RETM return MATCH_NO;
}

#undef RETM


match
gfc_match_wait (void)
{
  gfc_wait *wait;
  match m;

  m = gfc_match_char ('(');
  if (m == MATCH_NO)
    return m;

  wait = XCNEW (gfc_wait);

  m = match_wait_element (wait);
  if (m == MATCH_ERROR)
    goto cleanup;
  if (m == MATCH_NO)
    {
      m = gfc_match_expr (&wait->unit);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

  for (;;)
    {
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;

      m = match_wait_element (wait);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;
    }

  if (!gfc_notify_std (GFC_STD_F2003, "WAIT at %C "
		       "not allowed in Fortran 95"))
    goto cleanup;

  if (gfc_pure (NULL))
    {
      gfc_error ("WAIT statement not allowed in PURE procedure at %C");
      goto cleanup;
    }

  gfc_unset_implicit_pure (NULL);

  new_st.op = EXEC_WAIT;
  new_st.ext.wait = wait;

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_WAIT);

cleanup:
  gfc_free_wait (wait);
  return MATCH_ERROR;
}
