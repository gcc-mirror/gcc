/* CPP Library - lexical analysis.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987
   Broken out to separate file, Zack Weinberg, Mar 2000

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "intl.h"
#include "cpplib.h"
#include "cpphash.h"

#define PEEKN(N) (CPP_BUFFER (pfile)->rlimit - CPP_BUFFER (pfile)->cur >= (N) \
		  ? CPP_BUFFER (pfile)->cur[N] : EOF)
#define FORWARD(N) CPP_FORWARD (CPP_BUFFER (pfile), (N))
#define GETC() CPP_BUF_GET (CPP_BUFFER (pfile))
#define PEEKC() CPP_BUF_PEEK (CPP_BUFFER (pfile))

static void skip_block_comment	PARAMS ((cpp_reader *));
static void skip_line_comment	PARAMS ((cpp_reader *));
static int maybe_macroexpand	PARAMS ((cpp_reader *, long));
static int skip_comment		PARAMS ((cpp_reader *, int));
static int copy_comment		PARAMS ((cpp_reader *, int));
static void skip_string		PARAMS ((cpp_reader *, int));
static void parse_string	PARAMS ((cpp_reader *, int));
static U_CHAR *find_position	PARAMS ((U_CHAR *, U_CHAR *, unsigned long *));
static int null_cleanup		PARAMS ((cpp_buffer *, cpp_reader *));

/* Re-allocates PFILE->token_buffer so it will hold at least N more chars.  */

void
_cpp_grow_token_buffer (pfile, n)
     cpp_reader *pfile;
     long n;
{
  long old_written = CPP_WRITTEN (pfile);
  pfile->token_buffer_size = n + 2 * pfile->token_buffer_size;
  pfile->token_buffer = (U_CHAR *)
    xrealloc(pfile->token_buffer, pfile->token_buffer_size);
  CPP_SET_WRITTEN (pfile, old_written);
}

static int
null_cleanup (pbuf, pfile)
     cpp_buffer *pbuf ATTRIBUTE_UNUSED;
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Allocate a new cpp_buffer for PFILE, and push it on the input buffer stack.
   If BUFFER != NULL, then use the LENGTH characters in BUFFER
   as the new input buffer.
   Return the new buffer, or NULL on failure.  */

cpp_buffer *
cpp_push_buffer (pfile, buffer, length)
     cpp_reader *pfile;
     const U_CHAR *buffer;
     long length;
{
  cpp_buffer *buf = CPP_BUFFER (pfile);
  cpp_buffer *new;
  if (++pfile->buffer_stack_depth == CPP_STACK_MAX)
    {
      cpp_fatal (pfile, "macro or `#include' recursion too deep");
      return NULL;
    }

  new = (cpp_buffer *) xcalloc (1, sizeof (cpp_buffer));

  new->if_stack = pfile->if_stack;
  new->cleanup = null_cleanup;
  new->buf = new->cur = buffer;
  new->alimit = new->rlimit = buffer + length;
  new->prev = buf;
  new->mark = -1;
  new->line_base = NULL;

  CPP_BUFFER (pfile) = new;
  return new;
}

cpp_buffer *
cpp_pop_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buf = CPP_BUFFER (pfile);
  if (ACTIVE_MARK_P (pfile))
    cpp_ice (pfile, "mark active in cpp_pop_buffer");
  (*buf->cleanup) (buf, pfile);
  CPP_BUFFER (pfile) = CPP_PREV_BUFFER (buf);
  free (buf);
  pfile->buffer_stack_depth--;
  return CPP_BUFFER (pfile);
}

/* Scan until CPP_BUFFER (PFILE) is exhausted into PFILE->token_buffer.
   Pop the buffer when done.  */

void
cpp_scan_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = CPP_BUFFER (pfile);
  enum cpp_token token;
  if (CPP_OPTION (pfile, no_output))
    {
      long old_written = CPP_WRITTEN (pfile);
      /* In no-output mode, we can ignore everything but directives.  */
      for (;;)
	{
	  if (! pfile->only_seen_white)
	    _cpp_skip_rest_of_line (pfile);
	  token = cpp_get_token (pfile);
	  if (token == CPP_EOF) /* Should not happen ...  */
	    break;
	  if (token == CPP_POP && CPP_BUFFER (pfile) == buffer)
	    {
	      if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)) != NULL)
		cpp_pop_buffer (pfile);
	      break;
	    }
	}
      CPP_SET_WRITTEN (pfile, old_written);
    }
  else
    {
      for (;;)
	{
	  token = cpp_get_token (pfile);
	  if (token == CPP_EOF) /* Should not happen ...  */
	    break;
	  if (token == CPP_POP && CPP_BUFFER (pfile) == buffer)
	    {
	      if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)) != NULL)
		cpp_pop_buffer (pfile);
	      break;
	    }
	}
    }
}

/*
 * Rescan a string (which may have escape marks) into pfile's buffer.
 * Place the result in pfile->token_buffer.
 *
 * The input is copied before it is scanned, so it is safe to pass
 * it something from the token_buffer that will get overwritten
 * (because it follows CPP_WRITTEN).  This is used by do_include.
 */

void
cpp_expand_to_buffer (pfile, buf, length)
     cpp_reader *pfile;
     const U_CHAR *buf;
     int length;
{
  register cpp_buffer *ip;
  U_CHAR *buf1;
  int save_no_output;

  if (length < 0)
    {
      cpp_ice (pfile, "length < 0 in cpp_expand_to_buffer");
      return;
    }

  /* Set up the input on the input stack.  */

  buf1 = (U_CHAR *) alloca (length + 1);
  memcpy (buf1, buf, length);
  buf1[length] = 0;

  ip = cpp_push_buffer (pfile, buf1, length);
  if (ip == NULL)
    return;
  ip->has_escapes = 1;

  /* Scan the input, create the output.  */
  save_no_output = CPP_OPTION (pfile, no_output);
  CPP_OPTION (pfile, no_output) = 0;
  CPP_OPTION (pfile, no_line_commands)++;
  cpp_scan_buffer (pfile);
  CPP_OPTION (pfile, no_line_commands)--;
  CPP_OPTION (pfile, no_output) = save_no_output;

  CPP_NUL_TERMINATE (pfile);
}

void
cpp_buf_line_and_col (pbuf, linep, colp)
     register cpp_buffer *pbuf;
     long *linep, *colp;
{
  if (pbuf)
    {
      *linep = pbuf->lineno;
      if (colp)
	*colp = pbuf->cur - pbuf->line_base;
    }
  else
    {
      *linep = 0;
      if (colp)
	*colp = 0;
    }
}

/* Return the topmost cpp_buffer that corresponds to a file (not a macro).  */

cpp_buffer *
cpp_file_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip;

  for (ip = CPP_BUFFER (pfile); ip; ip = CPP_PREV_BUFFER (ip))
    if (ip->ihash != NULL)
      return ip;
  return NULL;
}

/* Skip a C-style block comment.  We know it's a comment, and point is
   at the second character of the starter.  */
static void
skip_block_comment (pfile)
     cpp_reader *pfile;
{
  int c, prev_c = -1;
  long line, col;

  FORWARD(1);
  cpp_buf_line_and_col (CPP_BUFFER (pfile), &line, &col);
  for (;;)
    {
      c = GETC ();
      if (c == EOF)
	{
	  cpp_error_with_line (pfile, line, col, "unterminated comment");
	  return;
	}
      else if (c == '\n' || c == '\r')
	{
	  /* \r cannot be a macro escape marker here. */
	  if (!ACTIVE_MARK_P (pfile))
	    CPP_BUMP_LINE (pfile);
	}
      else if (c == '/' && prev_c == '*')
	return;
      else if (c == '*' && prev_c == '/'
	       && CPP_OPTION (pfile, warn_comments))
	cpp_warning (pfile, "`/*' within comment");

      prev_c = c;
    }
}

/* Skip a C++/Chill line comment.  We know it's a comment, and point
   is at the second character of the initiator.  */
static void
skip_line_comment (pfile)
     cpp_reader *pfile;
{
  FORWARD(1);
  for (;;)
    {
      int c = GETC ();

      /* We don't have to worry about EOF in here.  */
      if (c == '\n')
	{
	  /* Don't consider final '\n' to be part of comment.  */
	  FORWARD(-1);
	  return;
	}
      else if (c == '\r')
	{
	  /* \r cannot be a macro escape marker here. */
	  if (!ACTIVE_MARK_P (pfile))
	    CPP_BUMP_LINE (pfile);
	  if (CPP_OPTION (pfile, warn_comments))
	    cpp_warning (pfile, "backslash-newline within line comment");
	}
    }
}

/* Skip a comment - C, C++, or Chill style.  M is the first character
   of the comment marker.  If this really is a comment, skip to its
   end and return ' '.  If this is not a comment, return M (which will
   be '/' or '-').  */

static int
skip_comment (pfile, m)
     cpp_reader *pfile;
     int m;
{
  if (m == '/' && PEEKC() == '*')
    {
      skip_block_comment (pfile);
      return ' ';
    }
  else if (m == '/' && PEEKC() == '/')
    {
      if (CPP_BUFFER (pfile)->system_header_p)
	{
	  /* We silently allow C++ comments in system headers, irrespective
	     of conformance mode, because lots of busted systems do that
	     and trying to clean it up in fixincludes is a nightmare.  */
	  skip_line_comment (pfile);
	  return ' ';
	}
      else if (CPP_OPTION (pfile, cplusplus_comments))
	{
	  if (CPP_OPTION (pfile, c89)
	      && CPP_PEDANTIC (pfile)
	      && ! CPP_BUFFER (pfile)->warned_cplusplus_comments)
	    {
	      cpp_pedwarn (pfile,
			   "C++ style comments are not allowed in ISO C89");
	      cpp_pedwarn (pfile,
			   "(this will be reported only once per input file)");
	      CPP_BUFFER (pfile)->warned_cplusplus_comments = 1;
	    }
	  skip_line_comment (pfile);
	  return ' ';
	}
      else
	return m;
    }
  else if (m == '-' && PEEKC() == '-'
	   && CPP_OPTION (pfile, chill))
    {
      skip_line_comment (pfile);
      return ' ';
    }
  else
    return m;
}

/* Identical to skip_comment except that it copies the comment into the
   token_buffer.  This is used if !discard_comments.  */
static int
copy_comment (pfile, m)
     cpp_reader *pfile;
     int m;
{
  const U_CHAR *start = CPP_BUFFER (pfile)->cur;  /* XXX Layering violation */
  const U_CHAR *limit;

  if (skip_comment (pfile, m) == m)
    return m;

  limit = CPP_BUFFER (pfile)->cur;
  CPP_RESERVE (pfile, limit - start + 2);
  CPP_PUTC_Q (pfile, m);
  for (; start <= limit; start++)
    if (*start != '\r')
      CPP_PUTC_Q (pfile, *start);

  return ' ';
}

/* Skip whitespace \-newline and comments.  Does not macro-expand.  */

void
_cpp_skip_hspace (pfile)
     cpp_reader *pfile;
{
  int c;
  while (1)
    {
      c = GETC();
      if (c == EOF)
	return;
      else if (is_hspace(c))
	{
	  if ((c == '\f' || c == '\v') && CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "%s in preprocessing directive",
			 c == '\f' ? "formfeed" : "vertical tab");
	}
      else if (c == '\r')
	{
	  /* \r is a backslash-newline marker if !has_escapes, and
	     a deletable-whitespace or no-reexpansion marker otherwise. */
	  if (CPP_BUFFER (pfile)->has_escapes)
	    {
	      if (PEEKC() == ' ')
		FORWARD(1);
	      else
		break;
	    }
	  else
	    CPP_BUMP_LINE (pfile);
	}
      else if (c == '/' || c == '-')
	{
	  c = skip_comment (pfile, c);
	  if (c  != ' ')
	    break;
	}
      else
	break;
    }
  FORWARD(-1);
}

/* Read and discard the rest of the current line.  */

void
_cpp_skip_rest_of_line (pfile)
     cpp_reader *pfile;
{
  for (;;)
    {
      int c = GETC();
      switch (c)
	{
	case '\n':
	  FORWARD(-1);
	case EOF:
	  return;

	case '\r':
	  if (! CPP_BUFFER (pfile)->has_escapes)
	    CPP_BUMP_LINE (pfile);
	  break;
	  
	case '\'':
	case '\"':
	  skip_string (pfile, c);
	  break;

	case '/':
	case '-':
	  skip_comment (pfile, c);
	  break;

	case '\f':
	case '\v':
	  if (CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "%s in preprocessing directive",
			 c == '\f' ? "formfeed" : "vertical tab");
	  break;

	}
    }
}

/* Parse an identifier starting with C.  */

void
_cpp_parse_name (pfile, c)
     cpp_reader *pfile;
     int c;
{
  for (;;)
  {
      if (! is_idchar(c))
      {
	  FORWARD (-1);
	  break;
      }

      if (c == '$' && CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "`$' in identifier");

      CPP_RESERVE(pfile, 2); /* One more for final NUL.  */
      CPP_PUTC_Q (pfile, c);
      c = GETC();
      if (c == EOF)
	break;
  }
  CPP_NUL_TERMINATE_Q (pfile);
  return;
}

/* Parse and skip over a string starting with C.  A single quoted
   string is treated like a double -- some programs (e.g., troff) are
   perverse this way.  (However, a single quoted string is not allowed
   to extend over multiple lines.)  */
static void
skip_string (pfile, c)
     cpp_reader *pfile;
     int c;
{
  long start_line, start_column;
  cpp_buf_line_and_col (cpp_file_buffer (pfile), &start_line, &start_column);

  while (1)
    {
      int cc = GETC();
      switch (cc)
	{
	case EOF:
	  cpp_error_with_line (pfile, start_line, start_column,
			       "unterminated string or character constant");
	  if (pfile->multiline_string_line != start_line
	      && pfile->multiline_string_line != 0)
	    cpp_error_with_line (pfile,
				 pfile->multiline_string_line, -1,
			 "possible real start of unterminated constant");
	  pfile->multiline_string_line = 0;
	  return;

	case '\n':
	  CPP_BUMP_LINE (pfile);
	  /* In Fortran and assembly language, silently terminate
	     strings of either variety at end of line.  This is a
	     kludge around not knowing where comments are in these
	     languages.  */
	  if (CPP_OPTION (pfile, lang_fortran)
	      || CPP_OPTION (pfile, lang_asm))
	    {
	      FORWARD(-1);
	      return;
	    }
	  /* Character constants may not extend over multiple lines.
	     In Standard C, neither may strings.  We accept multiline
	     strings as an extension.  */
	  if (c == '\'')
	    {
	      cpp_error_with_line (pfile, start_line, start_column,
				   "unterminated character constant");
	      FORWARD(-1);
	      return;
	    }
	  if (CPP_PEDANTIC (pfile) && pfile->multiline_string_line == 0)
	    cpp_pedwarn_with_line (pfile, start_line, start_column,
				   "string constant runs past end of line");
	  if (pfile->multiline_string_line == 0)
	    pfile->multiline_string_line = start_line;
	  break;

	case '\r':
	  if (CPP_BUFFER (pfile)->has_escapes)
	    {
	      cpp_ice (pfile, "\\r escape inside string constant");
	      FORWARD(1);
	    }
	  else
	    /* Backslash newline is replaced by nothing at all.  */
	    CPP_BUMP_LINE (pfile);
	  break;

	case '\\':
	  FORWARD(1);
	  break;

	case '\"':
	case '\'':
	  if (cc == c)
	    return;
	  break;
	}
    }
}

/* Parse a string and copy it to the output.  */

static void
parse_string (pfile, c)
     cpp_reader *pfile;
     int c;
{
  const U_CHAR *start = CPP_BUFFER (pfile)->cur;  /* XXX Layering violation */
  const U_CHAR *limit;

  skip_string (pfile, c);

  limit = CPP_BUFFER (pfile)->cur;
  CPP_RESERVE (pfile, limit - start + 2);
  CPP_PUTC_Q (pfile, c);
  for (; start < limit; start++)
    if (*start != '\r')
      CPP_PUTC_Q (pfile, *start);
}

/* Read an assertion into the token buffer, converting to
   canonical form: `#predicate(a n swe r)'  The next non-whitespace
   character to read should be the first letter of the predicate.
   Returns 0 for syntax error, 1 for bare predicate, 2 for predicate
   with answer (see callers for why). In case of 0, an error has been
   printed. */
int
_cpp_parse_assertion (pfile)
     cpp_reader *pfile;
{
  int c, dropwhite;
  _cpp_skip_hspace (pfile);
  c = PEEKC();
  if (! is_idstart(c))
    {
      cpp_error (pfile, "assertion predicate is not an identifier");
      return 0;
    }
  CPP_PUTC(pfile, '#');
  FORWARD(1);
  _cpp_parse_name (pfile, c);

  c = PEEKC();
  if (c != '(')
    {
      if (is_hspace(c) || c == '\r')
	_cpp_skip_hspace (pfile);
      c = PEEKC();
    }
  if (c != '(')
    return 1;

  CPP_PUTC(pfile, '(');
  FORWARD(1);
  dropwhite = 1;
  while ((c = GETC()) != ')')
    {
      if (is_space(c))
	{
	  if (! dropwhite)
	    {
	      CPP_PUTC(pfile, ' ');
	      dropwhite = 1;
	    }
	}
      else if (c == '\n' || c == EOF)
	{
	  if (c == '\n') FORWARD(-1);
	  cpp_error (pfile, "un-terminated assertion answer");
	  return 0;
	}
      else if (c == '\r')
	/* \r cannot be a macro escape here. */
	CPP_BUMP_LINE (pfile);
      else
	{
	  CPP_PUTC (pfile, c);
	  dropwhite = 0;
	}
    }

  if (pfile->limit[-1] == ' ')
    pfile->limit[-1] = ')';
  else if (pfile->limit[-1] == '(')
    {
      cpp_error (pfile, "empty token sequence in assertion");
      return 0;
    }
  else
    CPP_PUTC (pfile, ')');

  CPP_NUL_TERMINATE (pfile);
  return 2;
}

/* Get the next token, and add it to the text in pfile->token_buffer.
   Return the kind of token we got.  */

enum cpp_token
_cpp_lex_token (pfile)
     cpp_reader *pfile;
{
  register int c, c2, c3;
  enum cpp_token token;

 get_next:
  c = GETC();
  switch (c)
    {
    case EOF:
      return CPP_EOF;

    case '/':
      if (PEEKC () == '=')
	goto op2;

    comment:
      if (CPP_OPTION (pfile, discard_comments))
	c = skip_comment (pfile, c);
      else
	c = copy_comment (pfile, c);
      if (c != ' ')
	goto randomchar;
	  
      /* Comments are equivalent to spaces.
	 For -traditional, a comment is equivalent to nothing.  */
      if (CPP_TRADITIONAL (pfile) || !CPP_OPTION (pfile, discard_comments))
	return CPP_COMMENT;
      else
	{
	  CPP_PUTC (pfile, c);
	  return CPP_HSPACE;
	}

    case '#':
      if (pfile->parsing_if_directive)
	{
	  _cpp_skip_hspace (pfile);
	  _cpp_parse_assertion (pfile);
	  return CPP_ASSERTION;
	}

      if (pfile->parsing_define_directive && ! CPP_TRADITIONAL (pfile))
	{
	  CPP_RESERVE (pfile, 3);
	  CPP_PUTC_Q (pfile, '#');
	  CPP_NUL_TERMINATE_Q (pfile);
	  if (PEEKC () != '#')
	    return CPP_STRINGIZE;
	      
	  FORWARD (1);
	  CPP_PUTC_Q (pfile, '#');
	  CPP_NUL_TERMINATE_Q (pfile);
	  return CPP_TOKPASTE;
	}

      if (!pfile->only_seen_white)
	goto randomchar;
      /* -traditional directives are recognized only with the # in
	 column 1.
	 XXX Layering violation.  */
      if (CPP_TRADITIONAL (pfile)
	  && CPP_BUFFER (pfile)->cur - CPP_BUFFER (pfile)->line_base != 1)
	goto randomchar;
      return CPP_DIRECTIVE;

    case '\"':
    case '\'':
      parse_string (pfile, c);
      pfile->only_seen_white = 0;
      return c == '\'' ? CPP_CHAR : CPP_STRING;

    case '$':
      if (!CPP_OPTION (pfile, dollars_in_ident))
	goto randomchar;
      goto letter;

    case ':':
      if (CPP_OPTION (pfile, cplusplus) && PEEKC () == ':')
	goto op2;
      goto randomchar;

    case '&':
    case '+':
    case '|':
      c2 = PEEKC ();
      if (c2 == c || c2 == '=')
	goto op2;
      goto randomchar;

    case '*':
    case '!':
    case '%':
    case '=':
    case '^':
      if (PEEKC () == '=')
	goto op2;
      goto randomchar;

    case '-':
      c2 = PEEKC ();
      if (c2 == '-')
	{
	  if (CPP_OPTION (pfile, chill))
	    goto comment;  /* Chill style comment */
	  else
	    goto op2;
	}
      else if (c2 == '=')
	goto op2;
      else if (c2 == '>')
	{
	  if (CPP_OPTION (pfile, cplusplus) && PEEKN (1) == '*')
	    {
	      /* In C++, there's a ->* operator.  */
	      token = CPP_OTHER;
	      pfile->only_seen_white = 0;
	      CPP_RESERVE (pfile, 4);
	      CPP_PUTC_Q (pfile, c);
	      CPP_PUTC_Q (pfile, GETC ());
	      CPP_PUTC_Q (pfile, GETC ());
	      CPP_NUL_TERMINATE_Q (pfile);
	      return token;
	    }
	  goto op2;
	}
      goto randomchar;

    case '<':
      if (pfile->parsing_include_directive)
	{
	  for (;;)
	    {
	      CPP_PUTC (pfile, c);
	      if (c == '>')
		break;
	      c = GETC ();
	      if (c == '\n' || c == EOF)
		{
		  cpp_error (pfile,
			     "missing '>' in `#include <FILENAME>'");
		  break;
		}
	      else if (c == '\r')
		{
		  if (!CPP_BUFFER (pfile)->has_escapes)
		    {
		      /* Backslash newline is replaced by nothing. */
		      CPP_ADJUST_WRITTEN (pfile, -1);
		      CPP_BUMP_LINE (pfile);
		    }
		  else
		    {
		      /* We might conceivably get \r- or \r<space> in
			 here.  Just delete 'em. */
		      int d = GETC();
		      if (d != '-' && d != ' ')
			cpp_ice (pfile, "unrecognized escape \\r%c", d);
		      CPP_ADJUST_WRITTEN (pfile, -1);
		    }			  
		}
	    }
	  return CPP_STRING;
	}
      /* else fall through */
    case '>':
      c2 = PEEKC ();
      if (c2 == '=')
	goto op2;
      /* GNU C++ supports MIN and MAX operators <? and >?.  */
      if (c2 != c && (!CPP_OPTION (pfile, cplusplus) || c2 != '?'))
	goto randomchar;
      FORWARD(1);
      CPP_RESERVE (pfile, 4);
      CPP_PUTC (pfile, c);
      CPP_PUTC (pfile, c2);
      c3 = PEEKC ();
      if (c3 == '=')
	CPP_PUTC_Q (pfile, GETC ());
      CPP_NUL_TERMINATE_Q (pfile);
      pfile->only_seen_white = 0;
      return CPP_OTHER;

    case '.':
      c2 = PEEKC ();
      if (ISDIGIT(c2))
	{
	  CPP_RESERVE(pfile, 2);
	  CPP_PUTC_Q (pfile, '.');
	  c = GETC ();
	  goto number;
	}

      /* In C++ there's a .* operator.  */
      if (CPP_OPTION (pfile, cplusplus) && c2 == '*')
	goto op2;

      if (c2 == '.' && PEEKN(1) == '.')
	{
	  CPP_RESERVE(pfile, 4);
	  CPP_PUTC_Q (pfile, '.');
	  CPP_PUTC_Q (pfile, '.');
	  CPP_PUTC_Q (pfile, '.');
	  FORWARD (2);
	  CPP_NUL_TERMINATE_Q (pfile);
	  pfile->only_seen_white = 0;
	  return CPP_3DOTS;
	}
      goto randomchar;

    op2:
      token = CPP_OTHER;
      pfile->only_seen_white = 0;
      CPP_RESERVE(pfile, 3);
      CPP_PUTC_Q (pfile, c);
      CPP_PUTC_Q (pfile, GETC ());
      CPP_NUL_TERMINATE_Q (pfile);
      return token;

    case 'L':
      c2 = PEEKC ();
      if ((c2 == '\'' || c2 == '\"') && !CPP_TRADITIONAL (pfile))
	{
	  CPP_PUTC (pfile, c);
	  c = GETC ();
	  parse_string (pfile, c);
	  pfile->only_seen_white = 0;
	  return c == '\'' ? CPP_WCHAR : CPP_WSTRING;
	}
      goto letter;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    number:
    c2  = '.';
    for (;;)
      {
	CPP_RESERVE (pfile, 2);
	CPP_PUTC_Q (pfile, c);
	c = PEEKC ();
	if (c == EOF)
	  break;
	if (!is_numchar(c) && c != '.'
	    && ((c2 != 'e' && c2 != 'E'
		 && ((c2 != 'p' && c2 != 'P')
		     || CPP_OPTION (pfile, c89)))
		|| (c != '+' && c != '-')))
	  break;
	FORWARD(1);
	c2= c;
      }
    CPP_NUL_TERMINATE_Q (pfile);
    pfile->only_seen_white = 0;
    return CPP_NUMBER;
    case 'b': case 'c': case 'd': case 'h': case 'o':
    case 'B': case 'C': case 'D': case 'H': case 'O':
      if (CPP_OPTION (pfile, chill) && PEEKC () == '\'')
	{
	  pfile->only_seen_white = 0;
	  CPP_RESERVE (pfile, 2);
	  CPP_PUTC_Q (pfile, c);
	  CPP_PUTC_Q (pfile, '\'');
	  FORWARD(1);
	  for (;;)
	    {
	      c = GETC();
	      if (c == EOF)
		goto chill_number_eof;
	      if (!is_numchar(c))
		break;
	      CPP_PUTC (pfile, c);
	    }
	  if (c == '\'')
	    {
	      CPP_RESERVE (pfile, 2);
	      CPP_PUTC_Q (pfile, c);
	      CPP_NUL_TERMINATE_Q (pfile);
	      return CPP_STRING;
	    }
	  else
	    {
	      FORWARD(-1);
	    chill_number_eof:
	      CPP_NUL_TERMINATE (pfile);
	      return CPP_NUMBER;
	    }
	}
      else
	goto letter;
    case '_':
    case 'a': case 'e': case 'f': case 'g': case 'i': case 'j':
    case 'k': case 'l': case 'm': case 'n': case 'p': case 'q':
    case 'r': case 's': case 't': case 'u': case 'v': case 'w':
    case 'x': case 'y': case 'z':
    case 'A': case 'E': case 'F': case 'G': case 'I': case 'J':
    case 'K': case 'M': case 'N': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
    letter:
    pfile->only_seen_white = 0;
    _cpp_parse_name (pfile, c);
    return CPP_MACRO;

    case ' ':  case '\t':  case '\v':
      for (;;)
	{
	  CPP_PUTC (pfile, c);
	  c = PEEKC ();
	  if (c == EOF || !is_hspace(c))
	    break;
	  FORWARD(1);
	}
      return CPP_HSPACE;

    case '\r':
      if (CPP_BUFFER (pfile)->has_escapes)
	{
	  c = GETC ();
	  if (c == '-')
	    {
	      if (pfile->output_escapes)
		CPP_PUTS (pfile, "\r-", 2);
	      _cpp_parse_name (pfile, GETC ());
	      return CPP_NAME;
	    }
	  else if (c == ' ')
	    {
	      CPP_RESERVE (pfile, 2);
	      if (pfile->output_escapes)
		CPP_PUTC_Q (pfile, '\r');
	      CPP_PUTC_Q (pfile, c);
	      return CPP_HSPACE;
	    }
	  else
	    {
	      cpp_ice (pfile, "unrecognized escape \\r%c", c);
	      goto get_next;
	    }
	}
      else
	{
	  /* Backslash newline is ignored. */
	  CPP_BUMP_LINE (pfile);
	  goto get_next;
	}

    case '\n':
      CPP_PUTC (pfile, c);
      if (pfile->only_seen_white == 0)
	pfile->only_seen_white = 1;
      CPP_BUMP_LINE (pfile);
      if (! CPP_OPTION (pfile, no_line_commands))
	{
	  pfile->lineno++;
	  if (CPP_BUFFER (pfile)->lineno != pfile->lineno)
	    _cpp_output_line_command (pfile, same_file);
	}
      return CPP_VSPACE;

    case '(': token = CPP_LPAREN;    goto char1;
    case ')': token = CPP_RPAREN;    goto char1;
    case '{': token = CPP_LBRACE;    goto char1;
    case '}': token = CPP_RBRACE;    goto char1;
    case ',': token = CPP_COMMA;     goto char1;
    case ';': token = CPP_SEMICOLON; goto char1;

    randomchar:
    default:
      token = CPP_OTHER;
    char1:
      pfile->only_seen_white = 0;
      CPP_PUTC (pfile, c);
      return token;
    }
}

/* Check for and expand a macro, which is from WRITTEN to CPP_WRITTEN (pfile).
   Caller is expected to have checked no_macro_expand.  */
static int
maybe_macroexpand (pfile, written)
     cpp_reader *pfile;
     long written;
{
  U_CHAR *macro = pfile->token_buffer + written;
  size_t len = CPP_WRITTEN (pfile) - written;
  HASHNODE *hp = _cpp_lookup (pfile, macro, len);

  if (!hp)
    return 0;
  if (hp->type == T_DISABLED)
    {
      if (pfile->output_escapes)
	{
	  /* Insert a no-reexpand marker before IDENT.  */
	  CPP_RESERVE (pfile, 2);
	  CPP_ADJUST_WRITTEN (pfile, 2);
	  macro = pfile->token_buffer + written;

	  memmove (macro + 2, macro, len);
	  macro[0] = '\r';
	  macro[1] = '-';
	}
      return 0;
    }

  /* If macro wants an arglist, verify that a '(' follows.  */
  if (hp->type == T_MACRO && hp->value.defn->nargs >= 0)
    {
      int macbuf_whitespace = 0;
      int c;

      while (CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
	{
	  const U_CHAR *point = CPP_BUFFER (pfile)->cur;
	  for (;;)
	    {
	      _cpp_skip_hspace (pfile);
	      c = PEEKC ();
	      if (c == '\n')
		FORWARD(1);
	      else
		break;
	    }
	  if (point != CPP_BUFFER (pfile)->cur)
	    macbuf_whitespace = 1;
	  if (c == '(')
	    goto is_macro_call;
	  else if (c != EOF)
	    goto not_macro_call;
	  cpp_pop_buffer (pfile);
	}

      CPP_SET_MARK (pfile);
      for (;;)
	{
	  _cpp_skip_hspace (pfile);
	  c = PEEKC ();
	  if (c == '\n')
	    FORWARD(1);
	  else
	    break;
	}
      CPP_GOTO_MARK (pfile);

      if (c != '(')
	{
	not_macro_call:
	  if (macbuf_whitespace)
	    CPP_PUTC (pfile, ' ');
	  return 0;
	}
    }

 is_macro_call:
  /* This is now known to be a macro call.
     Expand the macro, reading arguments as needed,
     and push the expansion on the input stack.  */
  _cpp_macroexpand (pfile, hp);
  CPP_SET_WRITTEN (pfile, written);
  return 1;
}

enum cpp_token
cpp_get_token (pfile)
     cpp_reader *pfile;
{
  enum cpp_token token;
  long written = CPP_WRITTEN (pfile);

 get_next:
  token = _cpp_lex_token (pfile);

  switch (token)
    {
    default:
      return token;

    case CPP_DIRECTIVE:
      if (_cpp_handle_directive (pfile))
	return CPP_DIRECTIVE;
      pfile->only_seen_white = 0;
      CPP_PUTC (pfile, '#');
      return CPP_OTHER;

    case CPP_MACRO:
      if (! pfile->no_macro_expand
	  && maybe_macroexpand (pfile, written))
	goto get_next;
      return CPP_NAME;

    case CPP_EOF:
      if (CPP_BUFFER (pfile)->manual_pop)
	/* If we've been reading from redirected input, the
	   frontend will pop the buffer.  */
	return CPP_EOF;
      else if (CPP_BUFFER (pfile)->seen_eof)
	{
	  if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)) == NULL)
	    return CPP_EOF;

	  cpp_pop_buffer (pfile);
	  goto get_next;
	}
      else
	{
	  _cpp_handle_eof (pfile);
	  return CPP_POP;
	}
    }
}

/* Like cpp_get_token, but skip spaces and comments.  */

enum cpp_token
cpp_get_non_space_token (pfile)
     cpp_reader *pfile;
{
  int old_written = CPP_WRITTEN (pfile);
  for (;;)
    {
      enum cpp_token token = cpp_get_token (pfile);
      if (token != CPP_COMMENT && token != CPP_POP
	  && token != CPP_HSPACE && token != CPP_VSPACE)
	return token;
      CPP_SET_WRITTEN (pfile, old_written);
    }
}

/* Like cpp_get_token, except that it does not read past end-of-line.
   Also, horizontal space is skipped, and macros are popped.  */

enum cpp_token
_cpp_get_directive_token (pfile)
     cpp_reader *pfile;
{
  long old_written = CPP_WRITTEN (pfile);
  enum cpp_token token;

  for (;;)
    {
      _cpp_skip_hspace (pfile);
      if (PEEKC () == '\n')
	return CPP_VSPACE;

      token = cpp_get_token (pfile);
      /* token could be hspace at the beginning of a macro.  */
      if (token == CPP_HSPACE || token == CPP_COMMENT)
	{
	  CPP_SET_WRITTEN (pfile, old_written);
	  continue;
	}

      /* token cannot be vspace, it would have been caught above.  */
      if (token == CPP_VSPACE)
	{
	  cpp_ice (pfile, "VSPACE in get_directive_token");
	  return token;
	}

      /* token cannot be POP unless the buffer is a macro buffer.  */
      if (token != CPP_POP)
	return token;

      if (! CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
	{
	  cpp_ice (pfile, "POP of file buffer in get_directive_token");
	  return token;
	}

      /* We must pop the buffer by hand, or else cpp_get_token might
	 hand us white space or newline on the next invocation.  */
      cpp_pop_buffer (pfile);
    }
}

/* Determine the current line and column.  Used only by read_and_prescan. */
static U_CHAR *
find_position (start, limit, linep)
     U_CHAR *start;
     U_CHAR *limit;
     unsigned long *linep;
{
  unsigned long line = *linep;
  U_CHAR *lbase = start;
  while (start < limit)
    {
      U_CHAR ch = *start++;
      if (ch == '\n' || ch == '\r')
	{
	  line++;
	  lbase = start;
	}
    }
  *linep = line;
  return lbase;
}

/* Read the entire contents of file DESC into buffer BUF.  LEN is how
   much memory to allocate initially; more will be allocated if
   necessary.  Convert end-of-line markers (\n, \r, \r\n, \n\r) to
   canonical form (\n).  If enabled, convert and/or warn about
   trigraphs.  Convert backslash-newline to a one-character escape
   (\r) and remove it from "embarrassing" places (i.e. the middle of a
   token).  If there is no newline at the end of the file, add one and
   warn.  Returns -1 on failure, or the actual length of the data to
   be scanned.

   This function does a lot of work, and can be a serious performance
   bottleneck.  It has been tuned heavily; make sure you understand it
   before hacking.  The common case - no trigraphs, Unix style line
   breaks, backslash-newline set off by whitespace, newline at EOF -
   has been optimized at the expense of the others.  The performance
   penalty for DOS style line breaks (\r\n) is about 15%.
   
   Warnings lose particularly heavily since we have to determine the
   line number, which involves scanning from the beginning of the file
   or from the last warning.  The penalty for the absence of a newline
   at the end of reload1.c is about 60%.  (reload1.c is 329k.)

   If your file has more than one kind of end-of-line marker, you
   will get messed-up line numbering.
   
   So that the cases of the switch statement do not have to concern
   themselves with the complications of reading beyond the end of the
   buffer, the buffer is guaranteed to have at least 3 characters in
   it (or however many are left in the file, if less) on entry to the
   switch.  This is enough to handle trigraphs and the "\\\n\r" and
   "\\\r\n" cases.
   
   The end of the buffer is marked by a '\\', which, being a special
   character, guarantees we will exit the fast-scan loops and perform
   a refill. */

/* Table of characters that can't be handled in the inner loop.
   Keep these contiguous to optimize the performance of the code generated
   for the switch that uses them.  */
#define SPECCASE_EMPTY     0
#define SPECCASE_CR        1
#define SPECCASE_BACKSLASH 2
#define SPECCASE_QUESTION  3

/* Maps trigraph characters to their replacements */
static unsigned int trigraph_map   [1 << CHAR_BIT] = {0};

long
_cpp_read_and_prescan (pfile, fp, desc, len)
     cpp_reader *pfile;
     cpp_buffer *fp;
     int desc;
     size_t len;
{
  U_CHAR *buf = (U_CHAR *) xmalloc (len);
  U_CHAR *ip, *op, *line_base;
  U_CHAR *ibase;
  U_CHAR *speccase = pfile->input_speccase;
  unsigned long line;
  unsigned int deferred_newlines;
  size_t offset;
  int count = 0;

  offset = 0;
  deferred_newlines = 0;
  op = buf;
  line_base = buf;
  line = 1;
  ibase = pfile->input_buffer + 3;
  ip = ibase;
  ip[-1] = '\0';  /* Guarantee no match with \n for SPECCASE_CR */

  for (;;)
    {
      U_CHAR *near_buff_end;

      /* Copy previous char plus unprocessed (at most 2) chars
	 to beginning of buffer, refill it with another
	 read(), and continue processing */
      memcpy(ip - count - 1, ip - 1, 3);
      ip -= count;

      count = read (desc, ibase, pfile->input_buffer_len);
      if (count < 0)
	goto error;
      
      ibase[count] = '\\';  /* Marks end of buffer */
      if (count)
	{
	  near_buff_end = pfile->input_buffer + count;
	  offset += count;
	  if (offset > len)
	    {
	      size_t delta_op;
	      size_t delta_line_base;
	      len *= 2;
	      if (offset > len)
		/* len overflowed.
		   This could happen if the file is larger than half the
		   maximum address space of the machine. */
		goto too_big;

	      delta_op = op - buf;
	      delta_line_base = line_base - buf;
	      buf = (U_CHAR *) xrealloc (buf, len);
	      op = buf + delta_op;
	      line_base = buf + delta_line_base;
	    }
	}
      else
	{
	  if (ip == ibase)
	    break;
	  /* Allow normal processing of the (at most 2) remaining
	     characters.  The end-of-buffer marker is still present
	     and prevents false matches within the switch. */
	  near_buff_end = ibase - 1;
	}

      for (;;)
	{
	  unsigned int span;

	  /* Deal with \-newline, potentially in the middle of a token. */
	  if (deferred_newlines)
	    {
	      if (op != buf && op[-1] != ' ' && op[-1] != '\n' && op[-1] != '\t' && op[-1] != '\r')
		{
		  /* Previous was not white space.  Skip to white
		     space, if we can, before outputting the \r's */
		  span = 0;
		  while (ip[span] != ' '
			 && ip[span] != '\t'
			 && ip[span] != '\n'
			 && speccase[ip[span]] == SPECCASE_EMPTY)
		    span++;
		  memcpy (op, ip, span);
		  op += span;
		  ip += span;
		  if (speccase[ip[0]] != SPECCASE_EMPTY)
		    goto do_speccase;
		}
	      while (deferred_newlines)
		deferred_newlines--, *op++ = '\r';
	    }

	  /* Copy as much as we can without special treatment. */
	  span = 0;
	  while (speccase[ip[span]] == SPECCASE_EMPTY) span++;
	  memcpy (op, ip, span);
	  op += span;
	  ip += span;

	do_speccase:
	  if (ip > near_buff_end) /* Do we have enough chars? */
	    break;
	  switch (speccase[*ip++])
	    {
	    case SPECCASE_CR:  /* \r */
	      if (ip[-2] != '\n')
		{
		  if (*ip == '\n')
		    ip++;
		  *op++ = '\n';
		}
	      break;

	    case SPECCASE_BACKSLASH:  /* \ */
	      if (*ip == '\n')
		{
		  deferred_newlines++;
		  ip++;
		  if (*ip == '\r') ip++;
		}
	      else if (*ip == '\r')
		{
		  deferred_newlines++;
		  ip++;
		  if (*ip == '\n') ip++;
		}
	      else
		*op++ = '\\';
	      break;

	    case SPECCASE_QUESTION: /* ? */
	      {
		unsigned int d, t;

		*op++ = '?'; /* Normal non-trigraph case */
		if (ip[0] != '?')
		  break;
		    
		d = ip[1];
		t = trigraph_map[d];
		if (t == 0)
		  break;

		if (CPP_OPTION (pfile, warn_trigraphs))
		  {
		    unsigned long col;
		    line_base = find_position (line_base, op, &line);
		    col = op - line_base + 1;
		    if (CPP_OPTION (pfile, trigraphs))
		      cpp_warning_with_line (pfile, line, col,
					     "trigraph ??%c converted to %c", d, t);
		    else
		      cpp_warning_with_line (pfile, line, col,
					     "trigraph ??%c ignored", d);
		  }

		ip += 2;
		if (CPP_OPTION (pfile, trigraphs))
		  {
		    op[-1] = t;	    /* Overwrite '?' */
		    if (t == '\\')
		      {
			op--;
			*--ip = '\\';
			goto do_speccase; /* May need buffer refill */
		      }
		  }
		else
		  {
		    *op++ = '?';
		    *op++ = d;
		  }
	      }
	      break;
	    }
	}
    }

  if (offset == 0)
    return 0;

  if (op[-1] != '\n')
    {
      unsigned long col;
      line_base = find_position (line_base, op, &line);
      col = op - line_base + 1;
      cpp_warning_with_line (pfile, line, col, "no newline at end of file\n");
      if (offset + 1 > len)
	{
	  len += 1;
	  if (offset + 1 > len)
	    goto too_big;
	  buf = (U_CHAR *) xrealloc (buf, len);
	  op = buf + offset;
	}
      *op++ = '\n';
    }

  fp->buf = ((len - offset < 20) ? buf : (U_CHAR *)xrealloc (buf, op - buf));
  return op - buf;

 too_big:
  cpp_error (pfile, "file is too large (>%lu bytes)\n", (unsigned long)offset);
  free (buf);
  return -1;

 error:
  cpp_error_from_errno (pfile, fp->ihash->name);
  free (buf);
  return -1;
}

/* Initialize the `input_buffer' 'trigraph_map' and `input_speccase'
   tables.  These are only used by read_and_prescan, but they're large
   and somewhat expensive to set up, so we want them allocated once
   for the duration of the cpp run.  */

void
_cpp_init_input_buffer (pfile)
     cpp_reader *pfile;
{
  U_CHAR *tmp;

  /* Table of characters that cannot be handled by the
     read_and_prescan inner loop.  The number of non-EMPTY entries
     should be as small as humanly possible.  */

  tmp = (U_CHAR *) xmalloc (1 << CHAR_BIT);
  memset (tmp, SPECCASE_EMPTY, 1 << CHAR_BIT);
  tmp['\r'] = SPECCASE_CR;
  tmp['\\'] = SPECCASE_BACKSLASH;
  if (CPP_OPTION (pfile, trigraphs) || CPP_OPTION (pfile, warn_trigraphs))
    tmp['?'] = SPECCASE_QUESTION;
  pfile->input_speccase = tmp;

  /* Trigraph mappings */
  trigraph_map['='] = '#';
  trigraph_map[')'] = ']';
  trigraph_map['!'] = '|';
  trigraph_map['('] = '[';
  trigraph_map['\''] = '^';
  trigraph_map['>'] = '}';
  trigraph_map['/'] = '\\';
  trigraph_map['<'] = '{';
  trigraph_map['-'] = '~';

  /* Determine the appropriate size for the input buffer.  Normal C
     source files are smaller than eight K.  */
  /* 8Kbytes of buffer proper, 1 to detect running off the end without
     address arithmetic all the time, and 3 for pushback during buffer
     refill, in case there's a potential trigraph or end-of-line
     digraph at the end of a block. */

  tmp = (U_CHAR *) xmalloc (8192 + 1 + 3);
  pfile->input_buffer = tmp;
  pfile->input_buffer_len = 8192;
}
