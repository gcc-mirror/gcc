/* JSON parsing
   Copyright (C) 2017-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "json-parsing.h"
#include "pretty-print.h"
#include "math.h"
#include "make-unique.h"
#include "selftest.h"

using namespace json;

/* Declarations relating to parsing JSON, all within an
   anonymous namespace.  */

namespace {

/* A typedef representing a single unicode character.  */

typedef unsigned unichar;

/* An enum for discriminating different kinds of JSON token.  */

enum token_id
{
  TOK_ERROR,

  TOK_EOF,

  /* Punctuation.  */
  TOK_OPEN_SQUARE,
  TOK_OPEN_CURLY,
  TOK_CLOSE_SQUARE,
  TOK_CLOSE_CURLY,
  TOK_COLON,
  TOK_COMMA,

  /* Literal names.  */
  TOK_TRUE,
  TOK_FALSE,
  TOK_NULL,

  TOK_STRING,
  TOK_FLOAT_NUMBER,
  TOK_INTEGER_NUMBER
};

/* Human-readable descriptions of enum token_id.  */

static const char *token_id_name[] = {
  "error",
  "EOF",
  "'['",
  "'{'",
  "']'",
  "'}'",
  "':'",
  "','",
  "'true'",
  "'false'",
  "'null'",
  "string",
  "number",
  "number"
};

/* Tokens within the JSON lexer.  */

struct token
{
  /* The kind of token.  */
  enum token_id id;

  /* The location of this token within the unicode
     character stream.  */
  location_map::range range;

  union
  {
    /* Value for TOK_ERROR and TOK_STRING.  */
    char *string;

    /* Value for TOK_FLOAT_NUMBER.  */
    double float_number;

    /* Value for TOK_INTEGER_NUMBER.  */
    long integer_number;
  } u;
};

/* A class for lexing JSON.  */

class lexer
{
 public:
  lexer (bool support_comments);
  ~lexer ();

  std::unique_ptr<error> add_utf8 (size_t length, const char *utf8_buf);

  const token *peek ();

  void consume ();

 private:
  bool get_char (unichar &out_char, location_map::point *out_point);
  void unget_char ();
  location_map::point get_next_point () const;
  static void dump_token (FILE *outf, const token *tok);
  void lex_token (token *out);
  void lex_string (token *out);
  void lex_number (token *out, unichar first_char);
  bool rest_of_literal (token *out, const char *suffix);
  std::unique_ptr<error> make_error (const char *msg);
  bool consume_single_line_comment (token *out);
  bool consume_multiline_comment (token *out);

 private:
  auto_vec<unichar> m_buffer;
  int m_next_char_idx;
  int m_next_char_line;
  int m_next_char_column;
  int m_prev_line_final_column; /* for handling unget_char after a '\n'.  */

  static const int MAX_TOKENS = 1;
  token m_next_tokens[MAX_TOKENS];
  int m_num_next_tokens;

  bool m_support_comments;
};

/* A class for parsing JSON.  */

class parser
{
 public:
  parser (location_map *out_loc_map,
	  bool support_comments);
  ~parser ();

  std::unique_ptr<error>
  add_utf8 (size_t length, const char *utf8_buf);

  parser_result_t parse_value (int depth);
  parser_result_t parse_object (int depth);
  parser_result_t parse_array (int depth);

  std::unique_ptr<error>
  require_eof ();

 private:
  location_map::point get_next_token_start ();
  location_map::point get_next_token_end ();

  std::unique_ptr<error>
  require (enum token_id tok_id);

  result<enum token_id, std::unique_ptr<error>>
  require_one_of (enum token_id tok_id_a, enum token_id tok_id_b);

  std::unique_ptr<error>
  error_at (const location_map::range &r,
	    const char *fmt, ...) ATTRIBUTE_PRINTF_3;

  void maybe_record_range (json::value *jv, const location_map::range &r);
  void maybe_record_range (json::value *jv,
			   const location_map::point &start,
			   const location_map::point &end);

 private:
  lexer m_lexer;
  location_map *m_loc_map;
};

} // anonymous namespace for parsing implementation

/* Parser implementation.  */

/* lexer's ctor.  */

lexer::lexer (bool support_comments)
: m_buffer (), m_next_char_idx (0),
  m_next_char_line (1), m_next_char_column (0),
  m_prev_line_final_column (-1),
  m_num_next_tokens (0),
  m_support_comments (support_comments)
{
}

/* lexer's dtor.  */

lexer::~lexer ()
{
  while (m_num_next_tokens > 0)
    consume ();
}

/* Peek the next token.  */

const token *
lexer::peek ()
{
  if (m_num_next_tokens == 0)
    {
      lex_token (&m_next_tokens[0]);
      m_num_next_tokens++;
    }
  return &m_next_tokens[0];
}

/* Consume the next token.  */

void
lexer::consume ()
{
  if (m_num_next_tokens == 0)
    peek ();

  gcc_assert (m_num_next_tokens > 0);
  gcc_assert (m_num_next_tokens <= MAX_TOKENS);

  if (0)
    {
      fprintf (stderr, "consuming token: ");
      dump_token (stderr, &m_next_tokens[0]);
      fprintf (stderr, "\n");
    }

  if (m_next_tokens[0].id == TOK_ERROR
      || m_next_tokens[0].id == TOK_STRING)
    free (m_next_tokens[0].u.string);

  m_num_next_tokens--;
  memmove (&m_next_tokens[0], &m_next_tokens[1],
	   sizeof (token) * m_num_next_tokens);
}

/* Add LENGTH bytes of UTF-8 encoded text from UTF8_BUF to this lexer's
   buffer.
   Return null if successful, or the error if there was a problem.  */

std::unique_ptr<error>
lexer::add_utf8 (size_t length, const char *utf8_buf)
{
  /* Adapted from charset.c:one_utf8_to_cppchar.  */
  static const uchar masks[6] = { 0x7F, 0x1F, 0x0F, 0x07, 0x03, 0x01 };
  static const uchar patns[6] = { 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };

  const uchar *inbuf = (const unsigned char *) (utf8_buf);
  const uchar **inbufp = &inbuf;
  size_t *inbytesleftp = &length;

  while (length > 0)
    {
      unichar c;
      const uchar *inbuf = *inbufp;
      size_t nbytes, i;

      c = *inbuf;
      if (c < 0x80)
	{
	  m_buffer.safe_push (c);
	  *inbytesleftp -= 1;
	  *inbufp += 1;
	  continue;
	}

      /* The number of leading 1-bits in the first byte indicates how many
	 bytes follow.  */
      for (nbytes = 2; nbytes < 7; nbytes++)
	if ((c & ~masks[nbytes-1]) == patns[nbytes-1])
	  goto found;
      return make_error ("ill-formed UTF-8 sequence");
    found:

      if (*inbytesleftp < nbytes)
	return make_error ("ill-formed UTF-8 sequence");

      c = (c & masks[nbytes-1]);
      inbuf++;
      for (i = 1; i < nbytes; i++)
	{
	  unichar n = *inbuf++;
	  if ((n & 0xC0) != 0x80)
	    return make_error ("ill-formed UTF-8 sequence");
	  c = ((c << 6) + (n & 0x3F));
	}

      /* Make sure the shortest possible encoding was used.  */
      if ((   c <=      0x7F && nbytes > 1)
	  || (c <=     0x7FF && nbytes > 2)
	  || (c <=    0xFFFF && nbytes > 3)
	  || (c <=  0x1FFFFF && nbytes > 4)
	  || (c <= 0x3FFFFFF && nbytes > 5))
	return make_error ("ill-formed UTF-8:"
			   " shortest possible encoding not used");

      /* Make sure the character is valid.  */
      if (c > 0x7FFFFFFF || (c >= 0xD800 && c <= 0xDFFF))
	return make_error ("ill-formed UTF-8: invalid character");

      m_buffer.safe_push (c);
      *inbufp = inbuf;
      *inbytesleftp -= nbytes;
    }
  return nullptr;
}

/* Attempt to get the next unicode character from this lexer's buffer.
   If successful, write it to OUT_CHAR, and its location to *OUT_POINT,
   and return true.
   Otherwise, return false.  */

bool
lexer::get_char (unichar &out_char, location_map::point *out_point)
{
  if (m_next_char_idx >= (int)m_buffer.length ())
    return false;

  if (out_point)
    *out_point = get_next_point ();
  out_char = m_buffer[m_next_char_idx++];

  if (out_char == '\n')
    {
      m_next_char_line++;
      m_prev_line_final_column = m_next_char_column;
      m_next_char_column = 0;
    }
  else
    m_next_char_column++;

  return true;
}

/* Undo the last successful get_char.  */

void
lexer::unget_char ()
{
  --m_next_char_idx;
  if (m_next_char_column > 0)
    --m_next_char_column;
  else
    {
      m_next_char_line--;
      m_next_char_column = m_prev_line_final_column;
      /* We don't support more than one unget_char in a row.  */
      gcc_assert (m_prev_line_final_column != -1);
      m_prev_line_final_column = -1;
    }
}

/* Get the location of the next char.  */

location_map::point
lexer::get_next_point () const
{
  location_map::point result;
  result.m_unichar_idx = m_next_char_idx;
  result.m_line = m_next_char_line;
  result.m_column = m_next_char_column;
  return result;
}

/* Print a textual representation of TOK to OUTF.
   This is intended for debugging the lexer and parser,
   rather than for user-facing output.  */

void
lexer::dump_token (FILE *outf, const token *tok)
{
  switch (tok->id)
    {
    case TOK_ERROR:
      fprintf (outf, "TOK_ERROR (\"%s\")", tok->u.string);
      break;

    case TOK_EOF:
      fprintf (outf, "TOK_EOF");
      break;

    case TOK_OPEN_SQUARE:
      fprintf (outf, "TOK_OPEN_SQUARE");
      break;

    case TOK_OPEN_CURLY:
      fprintf (outf, "TOK_OPEN_CURLY");
      break;

    case TOK_CLOSE_SQUARE:
      fprintf (outf, "TOK_CLOSE_SQUARE");
      break;

    case TOK_CLOSE_CURLY:
      fprintf (outf, "TOK_CLOSE_CURLY");
      break;

    case TOK_COLON:
      fprintf (outf, "TOK_COLON");
      break;

    case TOK_COMMA:
      fprintf (outf, "TOK_COMMA");
      break;

    case TOK_TRUE:
      fprintf (outf, "TOK_TRUE");
      break;

    case TOK_FALSE:
      fprintf (outf, "TOK_FALSE");
      break;

    case TOK_NULL:
      fprintf (outf, "TOK_NULL");
      break;

    case TOK_STRING:
      fprintf (outf, "TOK_STRING (\"%s\")", tok->u.string);
      break;

    case TOK_FLOAT_NUMBER:
      fprintf (outf, "TOK_FLOAT_NUMBER (%f)", tok->u.float_number);
      break;

    case TOK_INTEGER_NUMBER:
      fprintf (outf, "TOK_INTEGER_NUMBER (%ld)", tok->u.integer_number);
      break;

    default:
      gcc_unreachable ();
      break;
    }
}

/* Treat "//" as a comment to the end of the line.

   This isn't compliant with the JSON spec,
   but is very handy for writing DejaGnu tests.

   Return true if EOF and populate *OUT, false otherwise.  */

bool
lexer::consume_single_line_comment (token *out)
{
  while (1)
    {
      unichar next_char;
      if (!get_char (next_char, nullptr))
	{
	  out->id = TOK_EOF;
	  location_map::point p = get_next_point ();
	  out->range.m_start = p;
	  out->range.m_end = p;
	  return true;
	}
      if (next_char == '\n')
	return false;
    }
}

/* Treat '/' '*' as a multiline comment until the next closing '*' '/'.

   This isn't compliant with the JSON spec,
   but is very handy for writing DejaGnu tests.

   Return true if EOF and populate *OUT, false otherwise.  */

bool
lexer::consume_multiline_comment (token *out)
{
  while (1)
    {
      unichar next_char;
      if (!get_char (next_char, nullptr))
	{
	  out->id = TOK_ERROR;
	  gcc_unreachable (); // TODO
	  location_map::point p = get_next_point ();
	  out->range.m_start = p;
	  out->range.m_end = p;
	  return true;
	}
      if (next_char != '*')
	continue;
      if (!get_char (next_char, nullptr))
	{
	  out->id = TOK_ERROR;
	  gcc_unreachable (); // TODO
	  location_map::point p = get_next_point ();
	  out->range.m_start = p;
	  out->range.m_end = p;
	  return true;
	}
      if (next_char == '/')
	return false;
    }
}

/* Attempt to lex the input buffer, writing the next token to OUT.
   On errors, TOK_ERROR (or TOK_EOF) is written to OUT.  */

void
lexer::lex_token (token *out)
{
  /* Skip to next non-whitespace char.  */
  unichar next_char;
  location_map::point start_point;
  while (1)
    {
      if (!get_char (next_char, &start_point))
	{
	  out->id = TOK_EOF;
	  location_map::point p = get_next_point ();
	  out->range.m_start = p;
	  out->range.m_end = p;
	  return;
	}
      if (m_support_comments)
	if (next_char == '/')
	  {
	    location_map::point point;
	    unichar next_next_char;
	    if (get_char (next_next_char, &point))
	      {
		switch (next_next_char)
		  {
		  case '/':
		    if (consume_single_line_comment (out))
		      return;
		    continue;
		  case '*':
		    if (consume_multiline_comment (out))
		      return;
		    continue;
		  default:
		    /* A stray single '/'.  Break out of loop, so that we
		       handle it below as an unexpected character.  */
		    goto non_whitespace;
		  }
	      }
	  }
      if (next_char != ' '
	  && next_char != '\t'
	  && next_char != '\n'
	  && next_char != '\r')
	break;
    }

 non_whitespace:

  out->range.m_start = start_point;
  out->range.m_end = start_point;

  switch (next_char)
    {
    case '[':
      out->id = TOK_OPEN_SQUARE;
      break;

    case '{':
      out->id = TOK_OPEN_CURLY;
      break;

    case ']':
      out->id = TOK_CLOSE_SQUARE;
      break;

    case '}':
      out->id = TOK_CLOSE_CURLY;
      break;

    case ':':
      out->id = TOK_COLON;
      break;

    case ',':
      out->id = TOK_COMMA;
      break;

    case '"':
      lex_string (out);
      break;

    case '-':
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
      lex_number (out, next_char);
      break;

    case 't':
      /* Handle literal "true".  */
      if (rest_of_literal (out, "rue"))
	{
	  out->id = TOK_TRUE;
	  break;
	}
      else
	goto err;

    case 'f':
      /* Handle literal "false".  */
      if (rest_of_literal (out, "alse"))
	{
	  out->id = TOK_FALSE;
	  break;
	}
      else
	goto err;

    case 'n':
      /* Handle literal "null".  */
      if (rest_of_literal (out, "ull"))
	{
	  out->id = TOK_NULL;
	  break;
	}
      else
	goto err;

    err:
    default:
      out->id = TOK_ERROR;
      out->u.string = xasprintf ("unexpected character: '%c'", next_char);
      break;
    }
}

/* Having consumed an open-quote character from the lexer's buffer, attempt
   to lex the rest of a JSON string, writing the result to OUT (or TOK_ERROR)
   if an error occurred.
   (ECMA-404 section 9; RFC 7159 section 7).  */

void
lexer::lex_string (token *out)
{
  auto_vec<unichar> content;
  bool still_going = true;
  while (still_going)
    {
      unichar uc;
      if (!get_char (uc, &out->range.m_end))
	{
	  out->id = TOK_ERROR;
	  out->range.m_end = get_next_point ();
	  out->u.string = xstrdup ("EOF within string");
	  return;
	}
      switch (uc)
	{
	case '"':
	  still_going = false;
	  break;
	case '\\':
	  {
	    unichar next_char;
	    if (!get_char (next_char, &out->range.m_end))
	      {
		out->id = TOK_ERROR;
		out->range.m_end = get_next_point ();
		out->u.string = xstrdup ("EOF within string");;
		return;
	      }
	    switch (next_char)
	      {
	      case '"':
	      case '\\':
	      case '/':
		content.safe_push (next_char);
		break;

	      case 'b':
		content.safe_push ('\b');
		break;

	      case 'f':
		content.safe_push ('\f');
		break;

	      case 'n':
		content.safe_push ('\n');
		break;

	      case 'r':
		content.safe_push ('\r');
		break;

	      case 't':
		content.safe_push ('\t');
		break;

	      case 'u':
		{
		  unichar result = 0;
		  for (int i = 0; i < 4; i++)
		    {
		      unichar hexdigit;
		      if (!get_char (hexdigit, &out->range.m_end))
			{
			  out->id = TOK_ERROR;
			  out->range.m_end = get_next_point ();
			  out->u.string = xstrdup ("EOF within string");
			  return;
			}
		      result <<= 4;
		      if (hexdigit >= '0' && hexdigit <= '9')
			result += hexdigit - '0';
		      else if (hexdigit >= 'a' && hexdigit <= 'f')
			result += (hexdigit - 'a') + 10;
		      else if (hexdigit >= 'A' && hexdigit <= 'F')
			result += (hexdigit - 'A') + 10;
		      else
			{
			  out->id = TOK_ERROR;
			  out->range.m_start = out->range.m_end;
			  out->u.string = xstrdup ("bogus hex char");
			  return;
			}
		    }
		  content.safe_push (result);
		}
		break;

	      default:
		out->id = TOK_ERROR;
		out->u.string = xstrdup ("unrecognized escape char");
		return;
	      }
	  }
	  break;

	default:
	  /* Reject unescaped control characters U+0000 through U+001F
	     (ECMA-404 section 9 para 1; RFC 7159 section 7 para 1).  */
	  if (uc <= 0x1f)
	    {
		out->id = TOK_ERROR;
		out->range.m_start = out->range.m_end;
		out->u.string = xstrdup ("unescaped control char");
		return;
	    }

	  /* Otherwise, add regular unicode code point.  */
	  content.safe_push (uc);
	  break;
	}
    }

  out->id = TOK_STRING;

  auto_vec<char> utf8_buf;
  // Adapted from libcpp/charset.c:one_cppchar_to_utf8
  for (unsigned i = 0; i < content.length (); i++)
    {
      static const uchar masks[6] =  { 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
      static const uchar limits[6] = { 0x80, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE };
      size_t nbytes;
      uchar buf[6], *p = &buf[6];
      unichar c = content[i];

      nbytes = 1;
      if (c < 0x80)
	*--p = c;
      else
	{
	  do
	    {
	      *--p = ((c & 0x3F) | 0x80);
	      c >>= 6;
	      nbytes++;
	    }
	  while (c >= 0x3F || (c & limits[nbytes-1]));
	  *--p = (c | masks[nbytes-1]);
	}

      while (p < &buf[6])
	utf8_buf.safe_push (*p++);
    }

  out->u.string = XNEWVEC (char, utf8_buf.length () + 1);
  for (unsigned i = 0; i < utf8_buf.length (); i++)
    out->u.string[i] = utf8_buf[i];
  out->u.string[utf8_buf.length ()] = '\0';
}

/* Having consumed FIRST_CHAR, an initial digit or '-' character from
   the lexer's buffer attempt to lex the rest of a JSON number, writing
   the result to OUT (or TOK_ERROR) if an error occurred.
   (ECMA-404 section 8; RFC 7159 section 6).  */

void
lexer::lex_number (token *out, unichar first_char)
{
  bool negate = false;
  double value = 0.0;
  if (first_char == '-')
    {
      negate = true;
      if (!get_char (first_char, &out->range.m_end))
	{
	  out->id = TOK_ERROR;
	  out->range.m_start = out->range.m_end;
	  out->u.string = xstrdup ("expected digit");
	  return;
	}
    }

  if (first_char == '0')
    value = 0.0;
  else if (!ISDIGIT (first_char))
    {
      out->id = TOK_ERROR;
      out->range.m_start = out->range.m_end;
      out->u.string = xstrdup ("expected digit");
      return;
    }
  else
    {
      /* Got a nonzero digit; expect zero or more digits.  */
      value = first_char - '0';
      while (1)
	{
	  unichar uc;
	  location_map::point point;
	  if (!get_char (uc, &point))
	    break;
	  if (ISDIGIT (uc))
	    {
	      value *= 10;
	      value += uc -'0';
	      out->range.m_end = point;
	      continue;
	    }
	  else
	    {
	      unget_char ();
	      break;
	    }
	}
    }

  /* Optional '.', followed by one or more decimals.  */
  unichar next_char;
  location_map::point point;
  if (get_char (next_char, &point))
    {
      if (next_char == '.')
	{
	  /* Parse decimal digits.  */
	  bool had_digit = false;
	  double digit_factor = 0.1;
	  while (get_char (next_char, &point))
	    {
	      if (!ISDIGIT (next_char))
		{
		  unget_char ();
		  break;
		}
	      value += (next_char - '0') * digit_factor;
	      digit_factor *= 0.1;
	      had_digit = true;
	      out->range.m_end = point;
	    }
	  if (!had_digit)
	    {
	      out->id = TOK_ERROR;
	      out->range.m_start = point;
	      out->range.m_start = point;
	      out->u.string = xstrdup ("expected digit");
	      return;
	    }
	}
      else
	unget_char ();
    }

  /* Parse 'e' and 'E'.  */
  unichar exponent_char;
  if (get_char (exponent_char, &point))
    {
      if (exponent_char == 'e' || exponent_char == 'E')
	{
	  /* Optional +/-.  */
	  unichar sign_char;
	  int exponent = 0;
	  bool negate_exponent = false;
	  bool had_exponent_digit = false;
	  if (!get_char (sign_char, &point))
	    {
	      out->id = TOK_ERROR;
	      out->range.m_start = point;
	      out->range.m_start = point;
	      out->u.string = xstrdup ("EOF within exponent");
	      return;
	    }
	  if (sign_char == '-')
	    negate_exponent = true;
	  else if (sign_char == '+')
	    ;
	  else if (ISDIGIT (sign_char))
	    {
	      exponent = sign_char - '0';
	      had_exponent_digit = true;
	    }
	  else
	    {
	      out->id = TOK_ERROR;
	      out->range.m_start = point;
	      out->range.m_start = point;
	      out->u.string
		= xstrdup ("expected '-','+' or digit within exponent");
	      return;
	    }
	  out->range.m_end = point;

	  /* One or more digits (we might have seen the digit above,
	     though).  */
	  while (1)
	    {
	      unichar uc;
	      location_map::point point;
	      if (!get_char (uc, &point))
		break;
	      if (ISDIGIT (uc))
		{
		  exponent *= 10;
		  exponent += uc -'0';
		  had_exponent_digit = true;
		  out->range.m_end = point;
		  continue;
		}
	      else
		{
		  unget_char ();
		  break;
		}
	    }
	  if (!had_exponent_digit)
	    {
	      out->id = TOK_ERROR;
	      out->range.m_start = point;
	      out->range.m_start = point;
	      out->u.string = xstrdup ("expected digit within exponent");
	      return;
	    }
	  if (negate_exponent)
	    exponent = -exponent;
	  value = value * pow (10, exponent);
	}
      else
	unget_char ();
    }

  if (negate)
    value = -value;

  if (value == (long)value)
    {
      out->id = TOK_INTEGER_NUMBER;
      out->u.integer_number = value;
    }
  else
    {
      out->id = TOK_FLOAT_NUMBER;
      out->u.float_number = value;
    }
}

/* Determine if the next characters to be lexed match SUFFIX.
   SUFFIX must be pure ASCII and not contain newlines.
   If so, consume the characters and return true.
   Otherwise, return false.  */

bool
lexer::rest_of_literal (token *out, const char *suffix)
{
  int suffix_idx = 0;
  int buf_idx = m_next_char_idx;
  while (1)
    {
      if (suffix[suffix_idx] == '\0')
	{
	  m_next_char_idx += suffix_idx;
	  m_next_char_column += suffix_idx;
	  out->range.m_end.m_unichar_idx += suffix_idx;
	  out->range.m_end.m_column += suffix_idx;
	  return true;
	}
      if (buf_idx >= (int)m_buffer.length ())
	return false;
      /* This assumes that suffix is ASCII.  */
      if (m_buffer[buf_idx] != (unichar)suffix[suffix_idx])
	return false;
      buf_idx++;
      suffix_idx++;
    }
}

/* Create a new error instance for MSG, using the location of the next
   character for the location of the error.  */

std::unique_ptr<error>
lexer::make_error (const char *msg)
{
  location_map::point p;
  p.m_unichar_idx = m_next_char_idx;
  p.m_line = m_next_char_line;
  p.m_column = m_next_char_column;
  location_map::range r;
  r.m_start = p;
  r.m_end = p;
  return ::make_unique<error> (r, xstrdup (msg));
}

/* parser's ctor.  */

parser::parser (location_map *out_loc_map,
		bool support_comments)
: m_lexer (support_comments), m_loc_map (out_loc_map)
{
}

/* parser's dtor.  */

parser::~parser ()
{
  if (m_loc_map)
    m_loc_map->on_finished_parsing ();
}

/* Add LENGTH bytes of UTF-8 encoded text from UTF8_BUF to this parser's
   lexer's buffer.  */

std::unique_ptr<error>
parser::add_utf8 (size_t length, const char *utf8_buf)
{
  return m_lexer.add_utf8 (length, utf8_buf);
}

/* Parse a JSON value (object, array, number, string, or literal).
   (ECMA-404 section 5; RFC 7159 section 3).  */

parser_result_t
parser::parse_value (int depth)
{
  const token *tok = m_lexer.peek ();

  /* Avoid stack overflow with deeply-nested inputs; RFC 7159 section 9
     states: "An implementation may set limits on the maximum depth
     of nesting.".

     Ideally we'd avoid this limit (e.g. by rewriting parse_value,
     parse_object, and parse_array into a single function with a vec of
     state).  */
  const int MAX_DEPTH = 100;
  if (depth >= MAX_DEPTH)
    return error_at (tok->range, "maximum nesting depth exceeded: %i",
		     MAX_DEPTH);

  switch (tok->id)
    {
    case TOK_OPEN_CURLY:
      return parse_object (depth);

    case TOK_STRING:
      {
	auto val = ::make_unique<string> (tok->u.string);
	m_lexer.consume ();
	maybe_record_range (val.get (), tok->range);
	return parser_result_t (std::move (val));
      }

    case TOK_OPEN_SQUARE:
      return parse_array (depth);

    case TOK_FLOAT_NUMBER:
      {
	auto val = ::make_unique<float_number> (tok->u.float_number);
	m_lexer.consume ();
	maybe_record_range (val.get (), tok->range);
	return parser_result_t (std::move (val));
      }

    case TOK_INTEGER_NUMBER:
      {
	auto val = ::make_unique<integer_number> (tok->u.integer_number);
	m_lexer.consume ();
	maybe_record_range (val.get (), tok->range);
	return parser_result_t (std::move (val));
      }

    case TOK_TRUE:
      {
	auto val = ::make_unique<literal> (JSON_TRUE);
	m_lexer.consume ();
	maybe_record_range (val.get (), tok->range);
	return parser_result_t (std::move (val));
      }

    case TOK_FALSE:
      {
	auto val = ::make_unique<literal> (JSON_FALSE);
	m_lexer.consume ();
	maybe_record_range (val.get (), tok->range);
	return parser_result_t (std::move (val));
      }

    case TOK_NULL:
      {
	auto val = ::make_unique<literal> (JSON_NULL);
	m_lexer.consume ();
	maybe_record_range (val.get (), tok->range);
	return parser_result_t (std::move (val));
      }

    case TOK_ERROR:
      return error_at (tok->range, "invalid JSON token: %s", tok->u.string);

    default:
      return error_at (tok->range, "expected a JSON value but got %s",
		       token_id_name[tok->id]);
    }
}

/* Parse a JSON object.
   (ECMA-404 section 6; RFC 7159 section 4).  */

parser_result_t
parser::parse_object (int depth)
{
  location_map::point start = get_next_token_start ();

  require (TOK_OPEN_CURLY);

  auto obj = ::make_unique<object> ();

  const token *tok = m_lexer.peek ();
  if (tok->id == TOK_CLOSE_CURLY)
    {
      location_map::point end = get_next_token_end ();
      maybe_record_range (obj.get (), start, end);
      if (auto err = require (TOK_CLOSE_CURLY))
	return parser_result_t (std::move (err));
      return parser_result_t (std::move (obj));
    }
  if (tok->id != TOK_STRING)
    return error_at (tok->range,
		     "expected string for object key after '{'; got %s",
		     token_id_name[tok->id]);
  while (true)
    {
      tok = m_lexer.peek ();
      if (tok->id != TOK_STRING)
	return error_at (tok->range,
			 "expected string for object key after ','; got %s",
			 token_id_name[tok->id]);
      label_text key = label_text::take (xstrdup (tok->u.string));
      m_lexer.consume ();

      if (auto err = require (TOK_COLON))
	return parser_result_t (std::move (err));

      parser_result_t r = parse_value (depth + 1);
      if (r.m_err)
	return r;
      if (!r.m_val)
	return parser_result_t (std::move (obj));

      /* We don't enforce uniqueness for keys.  */
      obj->set (key.get (), std::move (r.m_val));

      location_map::point end = get_next_token_end ();
      result<enum token_id, std::unique_ptr<error>> result
	(require_one_of (TOK_COMMA, TOK_CLOSE_CURLY));
      if (result.m_err)
	return parser_result_t (std::move (result.m_err));
      if (result.m_val == TOK_COMMA)
	continue;
      else
	{
	  /* TOK_CLOSE_CURLY.  */
	  maybe_record_range (obj.get (), start, end);
	  return parser_result_t (std::move (obj));
	}
    }
}

/* Parse a JSON array.
   (ECMA-404 section 7; RFC 7159 section 5).  */

parser_result_t
parser::parse_array (int depth)
{
  location_map::point start = get_next_token_start ();
  if (auto err = require (TOK_OPEN_SQUARE))
    return parser_result_t (std::move (err));

  auto arr = ::make_unique<array> ();

  const token *tok = m_lexer.peek ();
  if (tok->id == TOK_CLOSE_SQUARE)
    {
      location_map::point end = get_next_token_end ();
      maybe_record_range (arr.get (), start, end);
      m_lexer.consume ();
      return parser_result_t (std::move (arr));
    }

  while (true)
    {
      parser_result_t r = parse_value (depth + 1);
      if (r.m_err)
	return r;

      arr->append (std::move (r.m_val));

      location_map::point end = get_next_token_end ();
      result<enum token_id, std::unique_ptr<error>> result
	(require_one_of (TOK_COMMA, TOK_CLOSE_SQUARE));
      if (result.m_err)
	return parser_result_t (std::move (result.m_err));
      if (result.m_val == TOK_COMMA)
	continue;
      else
	{
	  /* TOK_CLOSE_SQUARE.  */
	  maybe_record_range (arr.get (), start, end);
	  return parser_result_t (std::move (arr));
	}
    }
}

/* Get the start point of the next token.  */

location_map::point
parser::get_next_token_start ()
{
  const token *tok = m_lexer.peek ();
  return tok->range.m_start;
}

/* Get the end point of the next token.  */

location_map::point
parser::get_next_token_end ()
{
  const token *tok = m_lexer.peek ();
  return tok->range.m_end;
}

/* Require an EOF, or fail if there is surplus input.  */

std::unique_ptr<error>
parser::require_eof ()
{
  return require (TOK_EOF);
}

/* Consume the next token, issuing an error if it is not of kind TOK_ID.  */

std::unique_ptr<error>
parser::require (enum token_id tok_id)
{
  const token *tok = m_lexer.peek ();
  if (tok->id != tok_id)
    {
      if (tok->id == TOK_ERROR)
	return error_at (tok->range,
			 "expected %s; got bad token: %s",
			 token_id_name[tok_id], tok->u.string);
      else
	return error_at (tok->range,
			 "expected %s; got %s", token_id_name[tok_id],
			 token_id_name[tok->id]);
    }
  m_lexer.consume ();
  return nullptr;
}

/* Consume the next token, issuing an error if it is not of
   kind TOK_ID_A or TOK_ID_B.
   Return which kind it was.  */

result<enum token_id, std::unique_ptr<error>>
parser::require_one_of (enum token_id tok_id_a, enum token_id tok_id_b)
{
  const token *tok = m_lexer.peek ();
  if ((tok->id != tok_id_a)
      && (tok->id != tok_id_b))
    {
      if (tok->id == TOK_ERROR)
	return error_at (tok->range, "expected %s or %s; got bad token: %s",
			 token_id_name[tok_id_a], token_id_name[tok_id_b],
			 tok->u.string);
      else
	return error_at (tok->range, "expected %s or %s; got %s",
			 token_id_name[tok_id_a], token_id_name[tok_id_b],
			 token_id_name[tok->id]);
    }
  enum token_id id = tok->id;
  m_lexer.consume ();
  return result<enum token_id, std::unique_ptr<error>> (id);
}

/* Genarate a parsing error.  */

std::unique_ptr<error>
parser::error_at (const location_map::range &r, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  char *formatted_msg = xvasprintf (fmt, ap);
  va_end (ap);

  return ::make_unique<error> (r, formatted_msg);
}

/* Record that JV has range R within the input file.  */

void
parser::maybe_record_range (json::value *jv, const location_map::range &r)
{
  if (m_loc_map)
    m_loc_map->record_range_for_value (jv, r);
}

/* Record that JV has range START to END within the input file.  */

void
parser::maybe_record_range (json::value *jv,
			    const location_map::point &start,
			    const location_map::point &end)
{
  if (m_loc_map)
    {
      location_map::range r;
      r.m_start = start;
      r.m_end = end;
      m_loc_map->record_range_for_value (jv, r);
    }
}

/* Attempt to parse the UTF-8 encoded buffer at UTF8_BUF
   of the given LENGTH.
   If ALLOW_COMMENTS is true, then allow C and C++ style-comments in the
   buffer, as an extension to JSON, otherwise forbid them.
   If successful, return an json::value in the result.
   if there was a problem, return a json::error in the result.
   If OUT_LOC_MAP is non-NULL, notify *OUT_LOC_MAP about
   source locations of nodes seen during parsing.  */

parser_result_t
json::parse_utf8_string (size_t length,
			 const char *utf8_buf,
			 bool allow_comments,
			 location_map *out_loc_map)
{
  parser p (out_loc_map, allow_comments);
  if (auto err = p.add_utf8 (length, utf8_buf))
    return parser_result_t (std::move (err));
  parser_result_t r = p.parse_value (0);
  if (r.m_err)
    return r;
  if (auto err = p.require_eof ())
    return parser_result_t (std::move (err));
  return r;
}

/* Attempt to parse the nil-terminated UTF-8 encoded buffer at
   UTF8_BUF.
   If ALLOW_COMMENTS is true, then allow C and C++ style-comments in the
   buffer, as an extension to JSON, otherwise forbid them.
   If successful, return a non-NULL json::value *.
   if there was a problem, return NULL and write an error
   message to err_out, which must be deleted by the caller.
   If OUT_LOC_MAP is non-NULL, notify *OUT_LOC_MAP about
   source locations of nodes seen during parsing.  */

json::parser_result_t
json::parse_utf8_string (const char *utf8,
			 bool allow_comments,
			 location_map *out_loc_map)
{
  return parse_utf8_string (strlen (utf8), utf8, allow_comments,
			    out_loc_map);
}


#if CHECKING_P

namespace selftest {

/* Selftests.  */

#define ASSERT_PRINT_EQ(JV, FORMATTED, EXPECTED_JSON)	\
  assert_print_eq (SELFTEST_LOCATION, JV, FORMATTED, EXPECTED_JSON)

/* Implementation detail of ASSERT_RANGE_EQ.  */

static void
assert_point_eq (const location &loc,
		 const location_map::point &actual_point,
		 size_t exp_unichar_idx, int exp_line, int exp_column)
{
  ASSERT_EQ_AT (loc, actual_point.m_unichar_idx, exp_unichar_idx);
  ASSERT_EQ_AT (loc, actual_point.m_line, exp_line);
  ASSERT_EQ_AT (loc, actual_point.m_column, exp_column);
}

/* Implementation detail of ASSERT_RANGE_EQ.  */

static void
assert_range_eq (const location &loc,
		 const location_map::range &actual_range,
		 /* Expected location.  */
		 size_t start_unichar_idx, int start_line, int start_column,
		 size_t end_unichar_idx, int end_line, int end_column)
{
  assert_point_eq (loc, actual_range.m_start,
		   start_unichar_idx, start_line, start_column);
  assert_point_eq (loc, actual_range.m_end,
		   end_unichar_idx, end_line, end_column);
}

/* Assert that ACTUAL_RANGE starts at
   (START_UNICHAR_IDX, START_LINE, START_COLUMN)
   and ends at (END_UNICHAR_IDX, END_LINE, END_COLUMN).  */

#define ASSERT_RANGE_EQ(ACTUAL_RANGE, \
			START_UNICHAR_IDX, START_LINE, START_COLUMN,	\
			END_UNICHAR_IDX, END_LINE, END_COLUMN)		\
  assert_range_eq ((SELFTEST_LOCATION), (ACTUAL_RANGE), \
		   (START_UNICHAR_IDX), (START_LINE), (START_COLUMN),	\
		   (END_UNICHAR_IDX), (END_LINE), (END_COLUMN))

/* Implementation detail of ASSERT_ERR_EQ.  */

static void
assert_err_eq (const location &loc,
	       const json::error *actual_err,
	       /* Expected location.  */
	       size_t start_unichar_idx, int start_line, int start_column,
	       size_t end_unichar_idx, int end_line, int end_column,
	       const char *expected_msg)
{
  ASSERT_TRUE_AT (loc, actual_err);
  const location_map::range &actual_range = actual_err->get_range ();
  ASSERT_EQ_AT (loc, actual_range.m_start.m_unichar_idx, start_unichar_idx);
  ASSERT_EQ_AT (loc, actual_range.m_start.m_line, start_line);
  ASSERT_EQ_AT (loc, actual_range.m_start.m_column, start_column);
  ASSERT_EQ_AT (loc, actual_range.m_end.m_unichar_idx, end_unichar_idx);
  ASSERT_EQ_AT (loc, actual_range.m_end.m_line, end_line);
  ASSERT_EQ_AT (loc, actual_range.m_end.m_column, end_column);
  ASSERT_STREQ_AT (loc, actual_err->get_msg (), expected_msg);
}

/* Assert that ACTUAL_ERR is a non-NULL json::error *,
   with message EXPECTED_MSG, and that its location starts
   at (START_UNICHAR_IDX, START_LINE, START_COLUMN)
   and ends at (END_UNICHAR_IDX, END_LINE, END_COLUMN).  */

#define ASSERT_ERR_EQ(ACTUAL_ERR, \
		      START_UNICHAR_IDX, START_LINE, START_COLUMN,	\
		      END_UNICHAR_IDX, END_LINE, END_COLUMN,	\
		      EXPECTED_MSG)		    \
  assert_err_eq ((SELFTEST_LOCATION), (ACTUAL_ERR), \
		 (START_UNICHAR_IDX), (START_LINE), (START_COLUMN),	\
		 (END_UNICHAR_IDX), (END_LINE), (END_COLUMN),	\
		 (EXPECTED_MSG))

/* Verify that the JSON lexer works as expected.  */

static void
test_lexer ()
{
  lexer l (false);
  const char *str
    /*  0         1         2         3         4         .  */
    /*  01234567890123456789012345678901234567890123456789.  */
    = ("    1066   -1  \n"
       "    -273.15 1e6\n"
       "  [   ] null   true  false  {  }  \"foo\" \n");
  auto err = l.add_utf8 (strlen (str), str);
  ASSERT_EQ (err, nullptr);

  /* Line 1.  */
  {
    const size_t line_offset = 0;

    /* Expect token: "1066" in columns 4-7.  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
      ASSERT_EQ (tok->u.integer_number, 1066);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 4, 1, 4,
		       line_offset + 7, 1, 7);
      l.consume ();
    }
    /* Expect token: "-1" in columns 11-12.  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
      ASSERT_EQ (tok->u.integer_number, -1);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 11, 1, 11,
		       line_offset + 12, 1, 12);
      l.consume ();
    }
  }

  /* Line 2.  */
  {
    const size_t line_offset = 16;

    /* Expect token: "-273.15" in columns 4-10.  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_FLOAT_NUMBER);
      ASSERT_EQ (int(tok->u.float_number), int(-273.15));
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 4, 2, 4,
		       line_offset + 10, 2, 10);
      l.consume ();
    }
    /* Expect token: "1e6" in columns 12-14.  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
      ASSERT_EQ (tok->u.integer_number, 1000000);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 12, 2, 12,
		       line_offset + 14, 2, 14);
      l.consume ();
    }
  }

  /* Line 3.  */
  {
    const size_t line_offset = 32;

    /* Expect token: "[".  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_OPEN_SQUARE);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 2, 3, 2,
		       line_offset + 2, 3, 2);
      l.consume ();
    }
    /* Expect token: "]".  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_CLOSE_SQUARE);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 6, 3, 6,
		       line_offset + 6, 3, 6);
      l.consume ();
    }
    /* Expect token: "null".  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_NULL);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 8, 3, 8,
		       line_offset + 11, 3, 11);
      l.consume ();
    }
    /* Expect token: "true".  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_TRUE);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 15, 3, 15,
		       line_offset + 18, 3, 18);
      l.consume ();
    }
    /* Expect token: "false".  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_FALSE);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 21, 3, 21,
		       line_offset + 25, 3, 25);
      l.consume ();
    }
    /* Expect token: "{".  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_OPEN_CURLY);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 28, 3, 28,
		       line_offset + 28, 3, 28);
      l.consume ();
    }
    /* Expect token: "}".  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_CLOSE_CURLY);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 31, 3, 31,
		       line_offset + 31, 3, 31);
      l.consume ();
    }
    /* Expect token: "\"foo\"".  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_STRING);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 34, 3, 34,
		       line_offset + 38, 3, 38);
      l.consume ();
    }
  }
}

/* Verify that the JSON lexer complains about single-line comments
   when comments are disabled.  */

static void
test_lexing_unsupported_single_line_comment ()
{
  lexer l (false);
  const char *str
    /*  0         1         2         3         4         .  */
    /*  01234567890123456789012345678901234567890123456789.  */
    = ("    1066   // Hello world\n");
  auto err = l.add_utf8 (strlen (str), str);
  ASSERT_EQ (err, nullptr);

  /* Line 1.  */
  {
    const size_t line_offset = 0;
    const int line_1 = 1;

    /* Expect token: "1066" in columns 4-7.  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
      ASSERT_EQ (tok->u.integer_number, 1066);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 4, line_1, 4,
		       line_offset + 7, line_1, 7);
      l.consume ();
    }

    /* Expect error.  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_ERROR);
      ASSERT_STREQ (tok->u.string, "unexpected character: '/'");
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 11, line_1, 11,
		       line_offset + 11, line_1, 11);
      l.consume ();
    }
  }
}

/* Verify that the JSON lexer complains about multiline comments
   when comments are disabled.  */

static void
test_lexing_unsupported_multiline_comment ()
{
  lexer l (false);
  const char *str
    /*  0         1         2         3         4         .  */
    /*  01234567890123456789012345678901234567890123456789.  */
    = ("    1066   /* Hello world\n"
       " continuation of comment\n"
       " end of comment */  42\n");
  auto err = l.add_utf8 (strlen (str), str);
  ASSERT_EQ (err, nullptr);

  /* Line 1.  */
  {
    const size_t line_offset = 0;
    const int line_1 = 1;

    /* Expect token: "1066" in line 1, columns 4-7.  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
      ASSERT_EQ (tok->u.integer_number, 1066);
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 4, line_1, 4,
		       line_offset + 7, line_1, 7);
      l.consume ();
    }

    /* Expect error.  */
    {
      const token *tok = l.peek ();
      ASSERT_EQ (tok->id, TOK_ERROR);
      ASSERT_STREQ (tok->u.string, "unexpected character: '/'");
      ASSERT_RANGE_EQ (tok->range,
		       line_offset + 11, line_1, 11,
		       line_offset + 11, line_1, 11);
      l.consume ();
    }
  }
}

/* Verify that the JSON lexer handles single-line comments
   when comments are enabled.  */

static void
test_lexing_supported_single_line_comment ()
{
  lexer l (true);
  const char *str
    /*  0         1         2         3         4         .  */
    /*  01234567890123456789012345678901234567890123456789.  */
    = ("    1066   // Hello world\n"
       "     42   // etc\n");
  auto err = l.add_utf8 (strlen (str), str);
  ASSERT_EQ (err, nullptr);

  const size_t line_1_offset = 0;
  const size_t line_2_offset = 26;
  const size_t line_3_offset = line_2_offset + 17;

  /* Expect token: "1066" in line 1, columns 4-7.  */
  {
    const int line_1 = 1;
    const token *tok = l.peek ();
    ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
    ASSERT_EQ (tok->u.integer_number, 1066);
    ASSERT_RANGE_EQ (tok->range,
		     line_1_offset + 4, line_1, 4,
		     line_1_offset + 7, line_1, 7);
    l.consume ();
  }

  /* Expect token: "42" in line 2, columns 5-6.  */
  {
    const int line_2 = 2;
    const token *tok = l.peek ();
    ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
    ASSERT_EQ (tok->u.integer_number, 42);
    ASSERT_RANGE_EQ (tok->range,
		     line_2_offset + 5, line_2, 5,
		     line_2_offset + 6, line_2, 6);
    l.consume ();
  }

  /* Expect EOF.  */
  {
    const int line_3 = 3;
    const token *tok = l.peek ();
    ASSERT_EQ (tok->id, TOK_EOF);
    ASSERT_RANGE_EQ (tok->range,
		     line_3_offset + 0, line_3, 0,
		     line_3_offset + 0, line_3, 0);
    l.consume ();
  }
}

/* Verify that the JSON lexer handles multiline comments
   when comments are enabled.  */

static void
test_lexing_supported_multiline_comment ()
{
  lexer l (true);
  const char *str
    /*  0         1         2         3         4         .  */
    /*  01234567890123456789012345678901234567890123456789.  */
    = ("    1066   /* Hello world\n"
       " continuation of comment\n"
       " end of comment */  42\n");
  auto err = l.add_utf8 (strlen (str), str);
  ASSERT_EQ (err, nullptr);

  const size_t line_1_offset = 0;
  const size_t line_2_offset = 26;
  const size_t line_3_offset = line_2_offset + 25;
  const size_t line_4_offset = line_3_offset + 23;

  /* Expect token: "1066" in line 1, columns 4-7.  */
  {
    const int line_1 = 1;
    const token *tok = l.peek ();
    ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
    ASSERT_EQ (tok->u.integer_number, 1066);
    ASSERT_RANGE_EQ (tok->range,
		     line_1_offset + 4, line_1, 4,
		     line_1_offset + 7, line_1, 7);
    l.consume ();
  }

  /* Expect token: "42" in line 3, columns 20-21.  */
  {
    const int line_3 = 3;
    const token *tok = l.peek ();
    ASSERT_EQ (tok->id, TOK_INTEGER_NUMBER);
    ASSERT_EQ (tok->u.integer_number, 42);
    ASSERT_RANGE_EQ (tok->range,
		     line_3_offset + 20, line_3, 20,
		     line_3_offset + 21, line_3, 21);
    l.consume ();
  }

  /* Expect EOF.  */
  {
    const int line_4 = 4;
    const token *tok = l.peek ();
    ASSERT_EQ (tok->id, TOK_EOF);
    ASSERT_RANGE_EQ (tok->range,
		     line_4_offset + 0, line_4, 0,
		     line_4_offset + 0, line_4, 0);
    l.consume ();
  }
}

/* Helper class for writing JSON parsing testcases.
   Attempts to parse a string in ctor, and captures the result (either
   a json::value or a json::error), and a location map.  */

struct parser_testcase
{
public:
  parser_testcase (const char *utf8_string, bool allow_comments = false)
  : m_loc_map (),
    m_result (parse_utf8_string (utf8_string, allow_comments, &m_loc_map))
  {
  }

  const json::value *get_value () const { return m_result.m_val.get (); }
  const json::error *get_error () const { return m_result.m_err.get (); }

  const location_map::range *
  get_range_for_value (const json::value *jv) const
  {
    return m_loc_map.get_range_for_value (jv);
  }

private:
  /* Concrete implementation of location_map for use in
     JSON parsing selftests.  */
  class test_location_map : public location_map
  {
  public:
    void record_range_for_value (json::value *jv, const range &r) final override
    {
      m_map.put (jv, r);
    }

    range *get_range_for_value (const json::value *jv) const
    {
      return const_cast<hash_map<const json::value *, range> &> (m_map)
	.get (jv);
    }

  private:
    hash_map<const json::value *, range> m_map;
  };

  test_location_map m_loc_map;
  json::parser_result_t m_result;
};

/* Verify that parse_utf8_string works as expected.  */

static void
test_parse_string ()
{
  const int line_1 = 1;

  {
    parser_testcase tc ("\"foo\"");
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_EQ (jv->get_kind (), JSON_STRING);
    ASSERT_STREQ (as_a <const json::string *> (jv)->get_string (), "foo");
    ASSERT_PRINT_EQ (*jv, true, "\"foo\"");
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     4, line_1, 4);
  }

  {
    const char *contains_quotes = "\"before \\\"quoted\\\" after\"";
    parser_testcase tc (contains_quotes);
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_EQ (jv->get_kind (), JSON_STRING);
    ASSERT_STREQ (as_a <const json::string *> (jv)->get_string (),
		  "before \"quoted\" after");
    ASSERT_PRINT_EQ (*jv, true, contains_quotes);
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     24, line_1, 24);
  }

  /* Test of non-ASCII input.  This string is the Japanese word "mojibake",
     written as C octal-escaped UTF-8.  */
  const char *mojibake = (/* Opening quote.  */
			  "\""
			  /* U+6587 CJK UNIFIED IDEOGRAPH-6587
			     UTF-8: 0xE6 0x96 0x87
			     C octal escaped UTF-8: \346\226\207.  */
			  "\346\226\207"
			  /* U+5B57 CJK UNIFIED IDEOGRAPH-5B57
			     UTF-8: 0xE5 0xAD 0x97
			     C octal escaped UTF-8: \345\255\227.  */
			  "\345\255\227"
			  /* U+5316 CJK UNIFIED IDEOGRAPH-5316
			     UTF-8: 0xE5 0x8C 0x96
			     C octal escaped UTF-8: \345\214\226.  */
			  "\345\214\226"
			  /* U+3051 HIRAGANA LETTER KE
			     UTF-8: 0xE3 0x81 0x91
			     C octal escaped UTF-8: \343\201\221.  */
			  "\343\201\221"
			  /* Closing quote.  */
			  "\"");
  {
    parser_testcase tc (mojibake);
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_EQ (jv->get_kind (), JSON_STRING);
    /* Result of get_string should be UTF-8 encoded, without quotes.  */
    ASSERT_STREQ (as_a <const json::string *> (jv)->get_string (),
		  "\346\226\207" "\345\255\227" "\345\214\226" "\343\201\221");
    /* Result of dump should be UTF-8 encoded, with quotes.  */
    ASSERT_PRINT_EQ (*jv, false, mojibake);
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     5, line_1, 5);
  }

  /* Test of \u-escaped unicode.  This is "mojibake" again, as above.  */
  {
    const char *escaped_unicode = "\"\\u6587\\u5b57\\u5316\\u3051\"";
    parser_testcase tc (escaped_unicode);
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_EQ (jv->get_kind (), JSON_STRING);
    /* Result of get_string should be UTF-8 encoded, without quotes.  */
    ASSERT_STREQ (as_a <const json::string *> (jv)->get_string (),
		  "\346\226\207" "\345\255\227" "\345\214\226" "\343\201\221");
    /* Result of dump should be UTF-8 encoded, with quotes.  */
    ASSERT_PRINT_EQ (*jv, false, mojibake);
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     25, line_1, 25);
  }
}

/* Verify that we can parse various kinds of JSON numbers.  */

static void
test_parse_number ()
{
  const int line_1 = 1;

  {
    parser_testcase tc ("42");
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_EQ (jv->get_kind (), JSON_INTEGER);
    ASSERT_EQ (as_a <const json::integer_number *> (jv)->get (), 42.0);
    ASSERT_PRINT_EQ (*jv, true, "42");
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     1, line_1, 1);
  }

  /* Negative number.  */
  {
    parser_testcase tc ("-17");
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_EQ (jv->get_kind (), JSON_INTEGER);
    ASSERT_EQ (as_a<const json::integer_number *> (jv)->get (), -17.0);
    ASSERT_PRINT_EQ (*jv, true, "-17");
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     2, line_1, 2);
  }

  /* Decimal.  */
  {
    parser_testcase tc ("3.141");
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_EQ (JSON_FLOAT, jv->get_kind ());
    ASSERT_NEAR (3.141, ((const json::float_number *)jv)->get (), 0.001);
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     4, line_1, 4);
  }

  /* Exponents.  */
  {
    {
      parser_testcase tc ("3.141e+0");
      ASSERT_EQ (tc.get_error (), nullptr);
      const json::value *jv = tc.get_value ();
      ASSERT_EQ (jv->get_kind (), JSON_FLOAT);
      ASSERT_NEAR (as_a <const json::float_number *> (jv)->get (), 3.141, 0.1);
      auto range = tc.get_range_for_value (jv);
      ASSERT_TRUE (range);
      ASSERT_RANGE_EQ (*range,
		       0, line_1, 0,
		       7, line_1, 7);
    }
    {
      parser_testcase tc ("42e2");
      ASSERT_EQ (tc.get_error (), nullptr);
      const json::value *jv = tc.get_value ();
      ASSERT_EQ (jv->get_kind (), JSON_INTEGER);
      ASSERT_EQ (as_a <const json::integer_number *> (jv)->get (), 4200);
      ASSERT_PRINT_EQ (*jv, true, "4200");
      auto range = tc.get_range_for_value (jv);
      ASSERT_TRUE (range);
      ASSERT_RANGE_EQ (*range,
		       0, line_1, 0,
		       3, line_1, 3);
    }
    {
      parser_testcase tc ("42e-1");
      ASSERT_EQ (tc.get_error (), nullptr);
      const json::value *jv = tc.get_value ();
      ASSERT_EQ (jv->get_kind (), JSON_FLOAT);
      ASSERT_NEAR (as_a <const json::float_number *> (jv)->get (), 4.2, 0.1);
      auto range = tc.get_range_for_value (jv);
      ASSERT_TRUE (range);
      ASSERT_RANGE_EQ (*range,
		       0, line_1, 0,
		       4, line_1, 4);
    }
  }
}

/* Verify that JSON array parsing works.  */

static void
test_parse_array ()
{
  const int line_1 = 1;

  parser_testcase tc ("[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]");
  ASSERT_EQ (tc.get_error (), nullptr);
  const json::value *jv = tc.get_value ();
  ASSERT_EQ (jv->get_kind (), JSON_ARRAY);
  const json::array *arr = as_a <const json::array *> (jv);
  ASSERT_EQ (arr->length (), 10);
  auto range = tc.get_range_for_value (jv);
  ASSERT_TRUE (range);
  ASSERT_RANGE_EQ (*range,
		   0, line_1, 0,
		   29, line_1, 29);
  for (int i = 0; i < 10; i++)
    {
      json::value *element = arr->get (i);
      ASSERT_EQ (element->get_kind (), JSON_INTEGER);
      ASSERT_EQ (as_a <json::integer_number *> (element)->get (), i);
      range = tc.get_range_for_value (element);
      ASSERT_TRUE (range);
      const int offset = 1 + (i * 3);
      ASSERT_RANGE_EQ (*range,
		       offset, line_1, offset,
		       offset, line_1, offset);
    }
  ASSERT_PRINT_EQ (*jv, false, "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]");
}

/* Verify that JSON object parsing works.  */

static void
test_parse_object ()
{
  const int line_1 = 1;
  std::unique_ptr<error> err;
  /*                   0            1            2         3  .  */
  /*                   01 2345 678 9012 345 6789 0123456789012.  */
  parser_testcase tc ("{\"foo\": \"bar\", \"baz\": [42, null]}");

  ASSERT_EQ (tc.get_error (), nullptr);
  const json::value *jv = tc.get_value ();
  ASSERT_NE (jv, nullptr);
  ASSERT_EQ (jv->get_kind (), JSON_OBJECT);
  auto range = tc.get_range_for_value (jv);
  ASSERT_TRUE (range);
  ASSERT_RANGE_EQ (*range,
		   0, line_1, 0,
		   32, line_1, 32);
  const json::object *jo = static_cast <const json::object *> (jv);

  json::value *foo_value = jo->get ("foo");
  ASSERT_NE (foo_value, nullptr);
  ASSERT_EQ (foo_value->get_kind (), JSON_STRING);
  ASSERT_STREQ (as_a <json::string *> (foo_value)->get_string (), "bar");
  range = tc.get_range_for_value (foo_value);
  ASSERT_TRUE (range);
  ASSERT_RANGE_EQ (*range,
		   8, line_1, 8,
		   12, line_1, 12);

  json::value *baz_value = jo->get ("baz");
  ASSERT_NE (baz_value, nullptr);
  ASSERT_EQ (baz_value->get_kind (), JSON_ARRAY);
  range = tc.get_range_for_value (baz_value);
  ASSERT_TRUE (range);
  ASSERT_RANGE_EQ (*range,
		   22, line_1, 22,
		   31, line_1, 31);

  json::array *baz_array = as_a <json::array *> (baz_value);
  ASSERT_EQ (baz_array->length (), 2);

  json::value *element0 = baz_array->get (0);
  ASSERT_EQ (as_a <json::integer_number *> (element0)->get (), 42);
  range = tc.get_range_for_value (element0);
  ASSERT_TRUE (range);
  ASSERT_RANGE_EQ (*range,
		   23, line_1, 23,
		   24, line_1, 24);

  json::value *element1 = baz_array->get (1);
  ASSERT_EQ (element1->get_kind (), JSON_NULL);
  range = tc.get_range_for_value (element1);
  ASSERT_TRUE (range);
  ASSERT_RANGE_EQ (*range,
		   27, line_1, 27,
		   30, line_1, 30);
}

/* Verify that the JSON literals "true", "false" and "null" are parsed
   correctly.  */

static void
test_parse_literals ()
{
  const int line_1 = 1;
  {
    parser_testcase tc ("true");
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_NE (jv, nullptr);
    ASSERT_EQ (jv->get_kind (), JSON_TRUE);
    ASSERT_PRINT_EQ (*jv, false, "true");
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     3, line_1, 3);
  }

  {
    parser_testcase tc ("false");
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_NE (jv, nullptr);
    ASSERT_EQ (jv->get_kind (), JSON_FALSE);
    ASSERT_PRINT_EQ (*jv, false, "false");
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     4, line_1, 4);
  }

  {
    parser_testcase tc ("null");
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_NE (jv, nullptr);
    ASSERT_EQ (jv->get_kind (), JSON_NULL);
    ASSERT_PRINT_EQ (*jv, false, "null");
    auto range = tc.get_range_for_value (jv);
    ASSERT_TRUE (range);
    ASSERT_RANGE_EQ (*range,
		     0, line_1, 0,
		     3, line_1, 3);
  }
}

/* Verify that we can parse a simple JSON-RPC request.  */

static void
test_parse_jsonrpc ()
{
  std::unique_ptr<error> err;
  const char *request
    /*  0           1            2           3          4.  */
    /*  01 23456789 012 3456 789 0123456 789 012345678 90.  */
    = ("{\"jsonrpc\": \"2.0\", \"method\": \"subtract\",\n"
    /*  0           1         2           3          4.  */
    /*  0 1234567 8901234567890 1234 56789012345678 90.  */
       " \"params\": [42, 23], \"id\": 1}");
  const int line_1 = 1;
  const int line_2 = 2;
  const size_t line_2_offset = 41;
  parser_testcase tc (request);
  ASSERT_EQ (tc.get_error (), nullptr);
  const json::value *jv = tc.get_value ();
  ASSERT_NE (jv, nullptr);
  auto range = tc.get_range_for_value (jv);
  ASSERT_TRUE (range);
  ASSERT_RANGE_EQ (*range,
		   0, line_1, 0,
		   line_2_offset + 28, line_2, 28);
}

/* Verify that we can parse an empty JSON object.  */

static void
test_parse_empty_object ()
{
  const int line_1 = 1;
  std::unique_ptr<error> err;
  parser_testcase tc ("{}");
  ASSERT_EQ (tc.get_error (), nullptr);
  const json::value *jv = tc.get_value ();
  ASSERT_NE (jv, nullptr);
  ASSERT_EQ (jv->get_kind (), JSON_OBJECT);
  ASSERT_PRINT_EQ (*jv, true, "{}");
  auto range = tc.get_range_for_value (jv);
  ASSERT_TRUE (range);
  ASSERT_RANGE_EQ (*range,
		   0, line_1, 0,
		   1, line_1, 1);
}

/* Verify that comment-parsing can be enabled or disabled.  */

static void
test_parsing_comments ()
{
  const char *str = ("// foo\n"
		     "/*...\n"
		     "...*/ 42 // bar\n"
		     "/* etc */\n");

  /* Parsing with comment support disabled.  */
  {
    parser_testcase tc (str);
    ASSERT_NE (tc.get_error (), nullptr);
    ASSERT_STREQ (tc.get_error ()->get_msg (),
		  "invalid JSON token: unexpected character: '/'");
    ASSERT_EQ (tc.get_value (), nullptr);
  }

  /* Parsing with comment support enabled.  */
  {
    parser_testcase tc (str, true);
    ASSERT_EQ (tc.get_error (), nullptr);
    const json::value *jv = tc.get_value ();
    ASSERT_NE (jv, nullptr);
    ASSERT_EQ (jv->get_kind (), JSON_INTEGER);
    ASSERT_EQ (((const json::integer_number *)jv)->get (), 42);
  }
}

/* Verify that we can parse an empty JSON string.  */

static void
test_error_empty_string ()
{
  const int line_1 = 1;
  parser_testcase tc ("");
  ASSERT_ERR_EQ (tc.get_error (),
		 0, line_1, 0,
		 0, line_1, 0,
		 "expected a JSON value but got EOF");
  ASSERT_EQ (tc.get_value (), nullptr);
}

/* Verify that JSON parsing gracefully handles an invalid token.  */

static void
test_error_bad_token ()
{
  const int line_1 = 1;
  parser_testcase tc ("  not valid ");
  ASSERT_ERR_EQ (tc.get_error (),
		 2, line_1, 2,
		 2, line_1, 2,
		 "invalid JSON token: unexpected character: 'n'");
  ASSERT_EQ (tc.get_value (), nullptr);
}

/* Verify that JSON parsing gracefully handles a missing comma
   within an object.  */

static void
test_error_object_with_missing_comma ()
{
  const int line_1 = 1;
  /*                  0           1           2.  */
  /*                  01 2345 6789012 3456 7890.  */
  const char *json = "{\"foo\" : 42 \"bar\"";
  parser_testcase tc (json);
  ASSERT_ERR_EQ (tc.get_error (),
		 12, line_1, 12,
		 16, line_1, 16,
		 "expected ',' or '}'; got string");
  ASSERT_EQ (tc.get_value (), nullptr);
}

/* Verify that JSON parsing gracefully handles a missing comma
   within an array.  */

static void
test_error_array_with_missing_comma ()
{
  const int line_1 = 1;
  /*                  01234567.  */
  const char *json = "[0, 1 42]";
  parser_testcase tc (json);
  ASSERT_ERR_EQ (tc.get_error (),
		 6, line_1, 6,
		 7, line_1, 7,
		 "expected ',' or ']'; got number");
  ASSERT_EQ (tc.get_value (), nullptr);
}

/* Run all of the selftests within this file.  */

void
json_parser_cc_tests ()
{
  test_lexer ();
  test_lexing_unsupported_single_line_comment ();
  test_lexing_unsupported_multiline_comment ();
  test_lexing_supported_single_line_comment ();
  test_lexing_supported_multiline_comment ();
  test_parse_string ();
  test_parse_number ();
  test_parse_array ();
  test_parse_object ();
  test_parse_literals ();
  test_parse_jsonrpc ();
  test_parse_empty_object ();
  test_parsing_comments ();
  test_error_empty_string ();
  test_error_bad_token ();
  test_error_object_with_missing_comma ();
  test_error_array_with_missing_comma ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
