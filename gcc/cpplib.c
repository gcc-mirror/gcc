/* CPP Library.
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

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

#include "cpplib.h"
#include "cpphash.h"
#include "intl.h"
#include "mkdeps.h"

#define SKIP_WHITE_SPACE(p) do { while (is_hspace(*p)) p++; } while (0)

#define PEEKN(N) (CPP_BUFFER (pfile)->rlimit - CPP_BUFFER (pfile)->cur >= (N) ? CPP_BUFFER (pfile)->cur[N] : EOF)
#define FORWARD(N) CPP_FORWARD (CPP_BUFFER (pfile), (N))
#define GETC() CPP_BUF_GET (CPP_BUFFER (pfile))
#define PEEKC() CPP_BUF_PEEK (CPP_BUFFER (pfile))
/* CPP_IS_MACRO_BUFFER is true if the buffer contains macro expansion.
   (Note that it is false while we're expanding macro *arguments*.) */
#define CPP_IS_MACRO_BUFFER(PBUF) ((PBUF)->data != NULL)

/* ACTIVE_MARK_P is true if there's a live mark in the buffer, in which
   case CPP_BUMP_LINE must not be called.  */
#define ACTIVE_MARK_P() (CPP_BUFFER (pfile)->mark != -1)

/* `struct directive' defines one #-directive, including how to handle it.  */

struct directive
{
  int length;			/* Length of name */
  int (*func)			/* Function to handle directive */
    PARAMS ((cpp_reader *, const struct directive *));
  const char *name;		/* Name of directive */
  enum node_type type;		/* Code which describes which directive.  */
};

/* These functions are declared to return int instead of void since they
   are going to be placed in a table and some old compilers have trouble with
   pointers to functions returning void.  */

static int do_define PARAMS ((cpp_reader *, const struct directive *));
static int do_line PARAMS ((cpp_reader *, const struct directive *));
static int do_include PARAMS ((cpp_reader *, const struct directive *));
static int do_undef PARAMS ((cpp_reader *, const struct directive *));
static int do_error PARAMS ((cpp_reader *, const struct directive *));
static int do_pragma PARAMS ((cpp_reader *, const struct directive *));
static int do_ident PARAMS ((cpp_reader *, const struct directive *));
static int do_if PARAMS ((cpp_reader *, const struct directive *));
static int do_xifdef PARAMS ((cpp_reader *, const struct directive *));
static int do_else PARAMS ((cpp_reader *, const struct directive *));
static int do_elif PARAMS ((cpp_reader *, const struct directive *));
static int do_endif PARAMS ((cpp_reader *, const struct directive *));
#ifdef SCCS_DIRECTIVE
static int do_sccs PARAMS ((cpp_reader *, const struct directive *));
#endif
static int do_assert PARAMS ((cpp_reader *, const struct directive *));
static int do_unassert PARAMS ((cpp_reader *, const struct directive *));
static int do_warning PARAMS ((cpp_reader *, const struct directive *));

/* Forward declarations.  */

static void validate_else		PARAMS ((cpp_reader *, const char *));
static HOST_WIDEST_INT eval_if_expression PARAMS ((cpp_reader *));
static void conditional_skip		PARAMS ((cpp_reader *, int,
						enum node_type, U_CHAR *));
static void skip_if_group		PARAMS ((cpp_reader *));
static void parse_name			PARAMS ((cpp_reader *, int));
static void parse_string		PARAMS ((cpp_reader *, int));
static int parse_assertion		PARAMS ((cpp_reader *));
static const char *if_directive_name	PARAMS ((cpp_reader *,
						 struct if_stack *));
static enum cpp_token null_underflow	PARAMS ((cpp_reader *));
static int null_cleanup			PARAMS ((cpp_buffer *, cpp_reader *));
static int skip_comment			PARAMS ((cpp_reader *, int));
static int copy_comment			PARAMS ((cpp_reader *, int));
static void skip_string			PARAMS ((cpp_reader *, int));
static void skip_rest_of_line		PARAMS ((cpp_reader *));
static void cpp_skip_hspace		PARAMS ((cpp_reader *));
static int handle_directive		PARAMS ((cpp_reader *));
static void pass_thru_directive		PARAMS ((const U_CHAR *, size_t,
						 cpp_reader *,
						 const struct directive *));
static int read_line_number		PARAMS ((cpp_reader *, int *));
static U_CHAR *detect_if_not_defined	PARAMS ((cpp_reader *));
static int consider_directive_while_skipping PARAMS ((cpp_reader *,
						      IF_STACK_FRAME *));
static void skip_block_comment		PARAMS ((cpp_reader *));
static void skip_line_comment		PARAMS ((cpp_reader *));
static void parse_set_mark		PARAMS ((cpp_reader *));
static void parse_goto_mark		PARAMS ((cpp_reader *));
static int get_macro_name		PARAMS ((cpp_reader *));

/* Here is the actual list of #-directives.
   This table is ordered by frequency of occurrence; the numbers
   at the end are directive counts from all the source code I have
   lying around (egcs and libc CVS as of 1999-05-18, plus grub-0.5.91,
   linux-2.2.9, and pcmcia-cs-3.0.9).  */

static const struct directive directive_table[] = {
  /* In C89 */
  {  6, do_define,   "define",       T_DEFINE },	/* 270554 */
  {  7, do_include,  "include",      T_INCLUDE },	/*  52262 */
  {  5, do_endif,    "endif",        T_ENDIF },		/*  45855 */
  {  5, do_xifdef,   "ifdef",        T_IFDEF },		/*  22000 */
  {  2, do_if,       "if",           T_IF },		/*  18162 */
  {  4, do_else,     "else",         T_ELSE },		/*   9863 */
  {  6, do_xifdef,   "ifndef",       T_IFNDEF },	/*   9675 */
  {  5, do_undef,    "undef",        T_UNDEF },		/*   4837 */
  {  4, do_line,     "line",         T_LINE },		/*   2465 */
  {  4, do_elif,     "elif",         T_ELIF },		/*    610 */
  {  5, do_error,    "error",        T_ERROR },		/*    475 */
  {  6, do_pragma,   "pragma",       T_PRAGMA },	/*    195 */

  /* Extensions.  All deprecated except #warning and #include_next.  */
  {  7, do_warning,  "warning",      T_WARNING },	/*     22 - GNU   */
  { 12, do_include,  "include_next", T_INCLUDE_NEXT },	/*     19 - GNU   */
  {  5, do_ident,    "ident",        T_IDENT },		/*     11 - SVR4  */
  {  6, do_include,  "import",       T_IMPORT },	/*      0 - ObjC  */
  {  6, do_assert,   "assert",       T_ASSERT },	/*      0 - SVR4  */
  {  8, do_unassert, "unassert",     T_UNASSERT },	/*      0 - SVR4  */
#ifdef SCCS_DIRECTIVE
  {  4, do_sccs,     "sccs",         T_SCCS },		/*      0 - SVR2? */
#endif
  {  -1, 0, "", T_UNUSED }
};

/* Place into PFILE a quoted string representing the string SRC.
   Caller must reserve enough space in pfile->token_buffer.  */

void
quote_string (pfile, src)
     cpp_reader *pfile;
     const char *src;
{
  U_CHAR c;

  CPP_PUTC_Q (pfile, '\"');
  for (;;)
    switch ((c = *src++))
      {
      default:
        if (ISPRINT (c))
	  CPP_PUTC_Q (pfile, c);
	else
	  {
	    sprintf ((char *)CPP_PWRITTEN (pfile), "\\%03o", c);
	    CPP_ADJUST_WRITTEN (pfile, 4);
	  }
	break;

      case '\"':
      case '\\':
	CPP_PUTC_Q (pfile, '\\');
	CPP_PUTC_Q (pfile, c);
	break;
      
      case '\0':
	CPP_PUTC_Q (pfile, '\"');
	CPP_NUL_TERMINATE_Q (pfile);
	return;
      }
}

/* Re-allocates PFILE->token_buffer so it will hold at least N more chars.  */

void
cpp_grow_buffer (pfile, n)
     cpp_reader *pfile;
     long n;
{
  long old_written = CPP_WRITTEN (pfile);
  pfile->token_buffer_size = n + 2 * pfile->token_buffer_size;
  pfile->token_buffer = (U_CHAR *)
    xrealloc(pfile->token_buffer, pfile->token_buffer_size);
  CPP_SET_WRITTEN (pfile, old_written);
}

/* Process the string STR as if it appeared as the body of a #define.
   If STR is just an identifier, define it with value 1.
   If STR has anything after the identifier, then it should
   be identifier=definition. */

void
cpp_define (pfile, str)
     cpp_reader *pfile;
     U_CHAR *str;
{
  U_CHAR *buf, *p;
  size_t count;

  p = strchr (str, '=');
  /* Copy the entire option so we can modify it. 
     Change the first "=" in the string to a space.  If there is none,
     tack " 1" on the end.  Then add a newline and a NUL.  */
  
  if (p)
    {
      count = strlen (str) + 2;
      buf = (U_CHAR *) alloca (count);
      memcpy (buf, str, count - 2);
      buf[p - str] = ' ';
      buf[count - 2] = '\n';
      buf[count - 1] = '\0';
    }
  else
    {
      count = strlen (str) + 4;
      buf = (U_CHAR *) alloca (count);
      memcpy (buf, str, count - 4);
      strcpy (&buf[count-4], " 1\n");
    }

  if (cpp_push_buffer (pfile, buf, count - 1) != NULL)
    {
      do_define (pfile, NULL);
      cpp_pop_buffer (pfile);
    }
}

/* Process the string STR as if it appeared as the body of a #assert. */
void
cpp_assert (pfile, str)
     cpp_reader *pfile;
     U_CHAR *str;
{
  if (cpp_push_buffer (pfile, str, strlen (str)) != NULL)
    {
      do_assert (pfile, NULL);
      cpp_pop_buffer (pfile);
    }
}

/* Determine whether the identifier ID, of length LEN, is a defined macro.  */
int
cpp_defined (pfile, id, len)
     cpp_reader *pfile;
     const U_CHAR *id;
     int len;
{
  HASHNODE *hp = _cpp_lookup (pfile, id, len);
  if (hp && hp->type == T_POISON)
    {
      cpp_error (pfile, "attempt to use poisoned `%s'", hp->name);
      return 0;
    }
  return (hp != NULL);
}

static enum cpp_token
null_underflow (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  return CPP_EOF;
}

static int
null_cleanup (pbuf, pfile)
     cpp_buffer *pbuf ATTRIBUTE_UNUSED;
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  return 0;
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
	  if (!ACTIVE_MARK_P())
	    CPP_BUMP_LINE (pfile);
	}
      else if (c == '/' && prev_c == '*')
	return;
      else if (c == '*' && prev_c == '/'
	       && CPP_OPTIONS (pfile)->warn_comments)
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
	  if (!ACTIVE_MARK_P())
	    CPP_BUMP_LINE (pfile);
	  if (CPP_OPTIONS (pfile)->warn_comments)
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
      else if (CPP_OPTIONS (pfile)->cplusplus_comments)
	{
	  if (CPP_OPTIONS (pfile)->c89
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
	   && CPP_OPTIONS (pfile)->chill)
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
  U_CHAR *start = CPP_BUFFER (pfile)->cur;  /* XXX Layering violation */
  U_CHAR *limit;

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

static void
cpp_skip_hspace (pfile)
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

static void
skip_rest_of_line (pfile)
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

/* Handle a possible # directive.
   '#' has already been read.  */

static int
handle_directive (pfile)
     cpp_reader *pfile;
{
  int c;
  register const struct directive *kt;
  int ident_length;
  U_CHAR *ident;
  long old_written = CPP_WRITTEN (pfile);

  if (CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
    {
      cpp_ice (pfile, "handle_directive called on macro buffer");
      return 0;
    }

  cpp_skip_hspace (pfile);

  c = PEEKC ();
  /* # followed by a number is equivalent to #line.  Do not recognize
     this form in assembly language source files.  Complain about this
     form if we're being pedantic, but not if this is regurgitated
     input (preprocessed or fed back in by the C++ frontend).  */
  if (c >= '0' && c <= '9')
    {
      if (CPP_OPTIONS (pfile)->lang_asm)
	return 0;

      if (CPP_PEDANTIC (pfile)
	  && ! CPP_PREPROCESSED (pfile)
	  && ! CPP_BUFFER (pfile)->manual_pop)
	cpp_pedwarn (pfile, "`#' followed by integer");
      do_line (pfile, NULL);
      return 1;
    }

  /* If we are rescanning preprocessed input, don't obey any directives
     other than # nnn.  */
  if (CPP_PREPROCESSED (pfile))
    return 0;

  /* Now find the directive name.  */
  CPP_PUTC (pfile, '#');
  parse_name (pfile, GETC());
  ident = pfile->token_buffer + old_written + 1;
  ident_length = CPP_PWRITTEN (pfile) - ident;
  if (ident_length == 0)
    {
      /* A line of just `#' becomes blank.  A line with something
	 other than an identifier after the # is reparsed as a non-
	 directive line.  */
      CPP_SET_WRITTEN (pfile, old_written);
      return (PEEKC() == '\n');
    }

  /* Decode the keyword and call the appropriate expansion routine.  */
  for (kt = directive_table; ; kt++)
    {
      if (kt->length <= 0)
	/* # identifier, but not a legit directive.  Pass onward as a
	   CPP_DIRECTIVE token anyway - let the consumer worry about it.  */
	return 1;
      if (kt->length == ident_length
	  && !strncmp (kt->name, ident, ident_length)) 
	break;
    }

  CPP_SET_WRITTEN (pfile, old_written);

  if (pfile->no_directives)
    {
      cpp_error (pfile, "`#%s' may not be used inside a macro argument",
		 kt->name);
      skip_rest_of_line (pfile);
    }
  else
    (*kt->func) (pfile, kt);

  return 1;
}

/* Pass a directive through to the output file.
   BUF points to the contents of the directive, as a contiguous string.
   LEN is the length of the string pointed to by BUF.
   KEYWORD is the keyword-table entry for the directive.  */

static void
pass_thru_directive (buf, len, pfile, keyword)
     const U_CHAR *buf;
     size_t len;
     cpp_reader *pfile;
     const struct directive *keyword;
{
  register unsigned keyword_length = keyword->length;

  CPP_RESERVE (pfile, 1 + keyword_length + len);
  CPP_PUTC_Q (pfile, '#');
  CPP_PUTS_Q (pfile, keyword->name, keyword_length);
  if (len != 0 && buf[0] != ' ')
    CPP_PUTC_Q (pfile, ' ');
  CPP_PUTS_Q (pfile, buf, len);
}

/* Subroutine of do_define: determine the name of the macro to be
   defined.  */

static int
get_macro_name (pfile)
     cpp_reader *pfile;
{
  long here, len;

  here = CPP_WRITTEN (pfile);
  pfile->no_macro_expand++;
  if (get_directive_token (pfile) != CPP_NAME)
    {
      cpp_error (pfile, "`#define' must be followed by an identifier");
      goto invalid;
    }

  len = CPP_WRITTEN (pfile) - here;
  if (len == 7 && !strncmp (pfile->token_buffer + here, "defined", 7))
    {
      cpp_error (pfile, "`defined' is not a legal macro name");
      goto invalid;
    }

  pfile->no_macro_expand--;
  return len;

 invalid:
  skip_rest_of_line (pfile);
  pfile->no_macro_expand--;
  return 0;
}

/* Process a #define command.
   KEYWORD is the keyword-table entry for #define,
   or NULL for a "predefined" macro.  */

static int
do_define (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  HASHNODE *hp;
  DEFINITION *def;
  long here;
  int len, c;
  int funlike = 0;
  U_CHAR *sym;

  here = CPP_WRITTEN (pfile);
  len = get_macro_name (pfile);
  if (len == 0)
    return 0;

  /* Copy out the name so we can pop the token buffer.  */
  len = CPP_WRITTEN (pfile) - here;
  sym = (U_CHAR *) alloca (len + 1);
  memcpy (sym, pfile->token_buffer + here, len);
  sym[len] = '\0';
  CPP_SET_WRITTEN (pfile, here);

  /* If the next character, with no intervening whitespace, is '(',
     then this is a function-like macro.  */
  c = PEEKC ();
  if (c == '(')
    funlike = 1;
  else if (c != '\n' && !is_hspace (c))
    /* Otherwise, C99 requires white space after the name.  We treat it
       as an object-like macro if this happens, with a warning.  */
    cpp_pedwarn (pfile, "missing white space after `#define %.*s'", len, sym);

  def = _cpp_create_definition (pfile, funlike);
  if (def == 0)
    return 0;

  if ((hp = _cpp_lookup (pfile, sym, len)) != NULL)
    {
      int ok;

      /* Redefining a macro is ok if the definitions are the same.  */
      if (hp->type == T_MACRO)
	ok = ! _cpp_compare_defs (pfile, def, hp->value.defn);
      /* Redefining a constant is ok with -D.  */
      else if (hp->type == T_CONST || hp->type == T_STDC)
        ok = ! CPP_OPTIONS (pfile)->done_initializing;
      /* Otherwise it's not ok.  */
      else
	ok = 0;
      /* Print the warning or error if it's not ok.  */
      if (! ok)
	{
	  if (hp->type == T_POISON)
	    cpp_error (pfile, "redefining poisoned `%.*s'", len, sym);
	  else
	    cpp_pedwarn (pfile, "`%.*s' redefined", len, sym);
	  if (hp->type == T_MACRO && CPP_OPTIONS (pfile)->done_initializing)
	    {
	      DEFINITION *d = hp->value.defn;
	      cpp_pedwarn_with_file_and_line (pfile, d->file, d->line, d->col,
			"this is the location of the previous definition");
	    }
	}
      if (hp->type != T_POISON)
	{
	  /* Replace the old definition.  */
	  if (hp->type == T_MACRO)
	    _cpp_free_definition (hp->value.defn);
	  hp->type = T_MACRO;
	  hp->value.defn = def;
	}
    }
  else
    _cpp_install (pfile, sym, len, T_MACRO, (char *) def);

  if (CPP_OPTIONS (pfile)->debug_output
      || CPP_OPTIONS (pfile)->dump_macros == dump_definitions)
    _cpp_dump_definition (pfile, sym, len, def);
  else if (CPP_OPTIONS (pfile)->dump_macros == dump_names)
    pass_thru_directive (sym, len, pfile, keyword);

  return 0;
}


/* Allocate a new cpp_buffer for PFILE, and push it on the input buffer stack.
   If BUFFER != NULL, then use the LENGTH characters in BUFFER
   as the new input buffer.
   Return the new buffer, or NULL on failure.  */

cpp_buffer *
cpp_push_buffer (pfile, buffer, length)
     cpp_reader *pfile;
     U_CHAR *buffer;
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
  new->underflow = null_underflow;
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
  if (ACTIVE_MARK_P())
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
  if (CPP_OPTIONS (pfile)->no_output)
    {
      long old_written = CPP_WRITTEN (pfile);
      /* In no-output mode, we can ignore everything but directives.  */
      for (;;)
	{
	  if (! pfile->only_seen_white)
	    skip_rest_of_line (pfile);
	  token = cpp_get_token (pfile);
	  if (token == CPP_EOF) /* Should not happen ...  */
	    break;
	  if (token == CPP_POP && CPP_BUFFER (pfile) == buffer)
	    {
	      if (CPP_PREV_BUFFER (CPP_BUFFER (pfile))
		  != CPP_NULL_BUFFER (pfile))
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
	      if (CPP_PREV_BUFFER (CPP_BUFFER (pfile))
		  != CPP_NULL_BUFFER (pfile))
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
  save_no_output = CPP_OPTIONS (pfile)->no_output;
  CPP_OPTIONS (pfile)->no_output = 0;
  CPP_OPTIONS (pfile)->no_line_commands++;
  cpp_scan_buffer (pfile);
  CPP_OPTIONS (pfile)->no_line_commands--;
  CPP_OPTIONS (pfile)->no_output = save_no_output;

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

/* Return the cpp_buffer that corresponds to a file (not a macro).  */

cpp_buffer *
cpp_file_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  for ( ; ip != CPP_NULL_BUFFER (pfile); ip = CPP_PREV_BUFFER (ip))
    if (ip->fname != NULL)
      return ip;
  return NULL;
}

/*
 * write out a #line command, for instance, after an #include file.
 * FILE_CHANGE says whether we are entering a file, leaving, or neither.
 */

void
output_line_command (pfile, file_change)
     cpp_reader *pfile;
     enum file_change_code file_change;
{
  long line;
  cpp_buffer *ip;

  if (CPP_OPTIONS (pfile)->no_line_commands
      || CPP_OPTIONS (pfile)->no_output)
    return;

  ip = cpp_file_buffer (pfile);
  cpp_buf_line_and_col (ip, &line, NULL);

  /* If the current file has not changed, we omit the #line if it would
     appear to be a no-op, and we output a few newlines instead
     if we want to increase the line number by a small amount.
     We cannot do this if pfile->lineno is zero, because that means we
     haven't output any line commands yet.  (The very first line command
     output is a `same_file' command.)  */
  if (file_change == same_file && pfile->lineno != 0)
    {
      if (line == pfile->lineno)
	return;

      /* If the inherited line number is a little too small,
	 output some newlines instead of a #line command.  */
      if (line > pfile->lineno && line < pfile->lineno + 8)
	{
	  CPP_RESERVE (pfile, 20);
	  while (line > pfile->lineno)
	    {
	      CPP_PUTC_Q (pfile, '\n');
	      pfile->lineno++;
	    }
	  return;
	}
    }

  CPP_RESERVE (pfile, 4 * strlen (ip->nominal_fname) + 50);
  CPP_PUTS_Q (pfile, "# ", 2);

  sprintf ((char *) CPP_PWRITTEN (pfile), "%ld ", line);
  CPP_ADJUST_WRITTEN (pfile, strlen (CPP_PWRITTEN (pfile)));

  quote_string (pfile, ip->nominal_fname); 
  if (file_change != same_file && file_change != rename_file)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, file_change == enter_file ? '1' : '2');
    }
  /* Tell cc1 if following text comes from a system header file.  */
  if (ip->system_header_p)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, '3');
    }
#ifndef NO_IMPLICIT_EXTERN_C
  /* Tell cc1plus if following text should be treated as C.  */
  if (ip->system_header_p == 2 && CPP_OPTIONS (pfile)->cplusplus)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, '4');
    }
#endif
  CPP_PUTC_Q (pfile, '\n');
  pfile->lineno = line;
}


/* Like cpp_get_token, except that it does not read past end-of-line.
   Also, horizontal space is skipped, and macros are popped.  */

enum cpp_token
get_directive_token (pfile)
     cpp_reader *pfile;
{
  long old_written = CPP_WRITTEN (pfile);
  enum cpp_token token;

  for (;;)
    {
      cpp_skip_hspace (pfile);
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

/* Handle #include and #import.
   This function expects to see "fname" or <fname> on the input.

   The input is normally in part of the output_buffer following
   CPP_WRITTEN, and will get overwritten by output_line_command.
   I.e. in input file specification has been popped by handle_directive.
   This is safe.  */

static int
do_include (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword;
{
  int importing = (keyword->type == T_IMPORT);
  int skip_dirs = (keyword->type == T_INCLUDE_NEXT);
  int angle_brackets = 0;	/* 0 for "...", 1 for <...> */
  int before;  /* included before? */
  long flen;
  unsigned char *ftok;
  cpp_buffer *fp;

  enum cpp_token token;

  /* Chain of dirs to search */
  struct include_hash *ihash;
  struct file_name_list *search_start;
  
  long old_written = CPP_WRITTEN (pfile);

  int fd;

  if (CPP_PEDANTIC (pfile))
    {
      if (importing)
	cpp_pedwarn (pfile, "ANSI C does not allow `#import'");
      if (skip_dirs)
	cpp_pedwarn (pfile, "ANSI C does not allow `#include_next'");
    }

  if (importing && CPP_OPTIONS (pfile)->warn_import
      && !CPP_OPTIONS (pfile)->inhibit_warnings
      && !CPP_BUFFER (pfile)->system_header_p && !pfile->import_warning)
    {
      pfile->import_warning = 1;
      cpp_warning (pfile,
	   "#import is obsolete, use an #ifndef wrapper in the header file");
    }

  pfile->parsing_include_directive++;
  token = get_directive_token (pfile);
  pfile->parsing_include_directive--;

  if (token == CPP_STRING)
    {
      if (pfile->token_buffer[old_written] == '<')
	angle_brackets = 1;
    }
#ifdef VMS
  else if (token == CPP_NAME)
    {
      /* Support '#include xyz' like VAX-C.  It is taken as
         '#include <xyz.h>' and generates a warning.  */
      cpp_warning (pfile,
	       "`#include filename' is obsolete, use `#include <filename.h>'");
      angle_brackets = 1;

      /* Append the missing `.h' to the name. */
      CPP_PUTS (pfile, ".h", 2);
    }
#endif
  else
    {
      cpp_error (pfile,
		 "`#%s' expects \"FILENAME\" or <FILENAME>", keyword->name);
      CPP_SET_WRITTEN (pfile, old_written);
      skip_rest_of_line (pfile);
      return 0;
    }

  flen = CPP_WRITTEN (pfile) - old_written;
  ftok = (unsigned char *) alloca (flen + 1);
  memcpy (ftok, pfile->token_buffer + old_written, flen);
  ftok[flen] = '\0';

  if (get_directive_token (pfile) != CPP_VSPACE)
    {
      cpp_error (pfile, "junk at end of `#include'");
      skip_rest_of_line (pfile);
    }

  CPP_SET_WRITTEN (pfile, old_written);

  if (flen == 0)
    {
      cpp_error (pfile, "empty file name in `#%s'", keyword->name);
      return 0;
    }

  if (CPP_OPTIONS (pfile)->dump_includes)
    pass_thru_directive (ftok,
			 flen
#ifdef VMS
	  - ((token == CPP_NAME) ? 2 : 0)
#endif
			 , pfile, keyword);

#ifdef VMS
  if (token == CPP_STRING)
#endif
    {
      ftok++;
      flen -= 2;
      ftok[flen] = '\0';
    }

  search_start = 0;

  for (fp = CPP_BUFFER (pfile);
       fp != CPP_NULL_BUFFER (pfile);
       fp = CPP_PREV_BUFFER (fp))
    if (fp->fname != NULL)
      break;

  if (fp == CPP_NULL_BUFFER (pfile))
    {
      cpp_ice (pfile, "fp == NULL_BUFFER in do_include");
      return 0;
    }
  
  /* For #include_next, skip in the search path past the dir in which the
     containing file was found.  Treat files specified using an absolute path
     as if there are no more directories to search.  Treat the primary source
     file like any other included source, but generate a warning.  */
  if (skip_dirs && CPP_PREV_BUFFER(fp) != CPP_NULL_BUFFER (pfile))
    {
      if (fp->ihash->foundhere != ABSOLUTE_PATH)
	search_start = fp->ihash->foundhere->next;
    }
  else
    {
      if (skip_dirs)
	cpp_warning (pfile, "#include_next in primary source file");
      
      if (angle_brackets)
	search_start = CPP_OPTIONS (pfile)->bracket_include;
      else
        {
	  if (!CPP_OPTIONS (pfile)->ignore_srcdir)
	    {
	      if (fp)
		search_start = fp->actual_dir;
	    }
	  else
	    search_start = CPP_OPTIONS (pfile)->quote_include;
	}
    }

  if (!search_start)
    {
      cpp_error (pfile, "No include path in which to find %s", ftok);
      return 0;
    }

  fd = _cpp_find_include_file (pfile, ftok, search_start, &ihash, &before);

  if (fd == -2)
    return 0;
  
  if (fd == -1)
    {
      if (CPP_OPTIONS (pfile)->print_deps_missing_files
	  && CPP_PRINT_DEPS (pfile) > (angle_brackets ||
				       (pfile->system_include_depth > 0)))
        {
	  if (!angle_brackets)
	    deps_add_dep (pfile->deps, ftok);
	  else
	    {
	      char *p;
	      struct file_name_list *ptr;
	      /* If requested as a system header, assume it belongs in
		 the first system header directory. */
	      if (CPP_OPTIONS (pfile)->bracket_include)
	        ptr = CPP_OPTIONS (pfile)->bracket_include;
	      else
	        ptr = CPP_OPTIONS (pfile)->quote_include;

	      p = (char *) alloca (strlen (ptr->name)
				   + strlen (ftok) + 2);
	      if (*ptr->name != '\0')
	        {
		  strcpy (p, ptr->name);
		  strcat (p, "/");
	        }
	      strcat (p, ftok);
	      deps_add_dep (pfile->deps, p);
	    }
	}
      /* If -M was specified, and this header file won't be added to
	 the dependency list, then don't count this as an error,
	 because we can still produce correct output.  Otherwise, we
	 can't produce correct output, because there may be
	 dependencies we need inside the missing file, and we don't
	 know what directory this missing file exists in. */
      else if (CPP_PRINT_DEPS (pfile)
	       && (CPP_PRINT_DEPS (pfile)
		   <= (angle_brackets || (pfile->system_include_depth > 0))))
	cpp_warning (pfile, "No include path in which to find %s", ftok);
      else
	cpp_error_from_errno (pfile, ftok);

      return 0;
    }

  /* For -M, add the file to the dependencies on its first inclusion. */
  if (!before && (CPP_PRINT_DEPS (pfile)
		  > (angle_brackets || (pfile->system_include_depth > 0))))
    deps_add_dep (pfile->deps, ihash->name);

  /* Handle -H option.  */
  if (CPP_OPTIONS(pfile)->print_include_names)
    {
      fp = CPP_BUFFER (pfile);
      while ((fp = CPP_PREV_BUFFER (fp)) != CPP_NULL_BUFFER (pfile))
	putc ('.', stderr);
      fprintf (stderr, " %s\n", ihash->name);
    }

  /* Actually process the file */

  if (importing)
    ihash->control_macro = "";
  
  if (cpp_push_buffer (pfile, NULL, 0) == NULL)
    {
      close (fd);
      return 0;
    }
  
  if (angle_brackets)
    pfile->system_include_depth++;   /* Decremented in file_cleanup. */

  if (_cpp_read_include_file (pfile, fd, ihash))
    {
      output_line_command (pfile, enter_file);
      pfile->only_seen_white = 2;
    }

  return 0;
}

/* Subroutine of do_line.  Read next token from PFILE without adding it to
   the output buffer.  If it is a number between 1 and 4, store it in *NUM
   and return 1; otherwise, return 0 and complain if we aren't at the end
   of the directive.  */

static int
read_line_number (pfile, num)
     cpp_reader *pfile;
     int *num;
{
  long save_written = CPP_WRITTEN (pfile);
  U_CHAR *p = pfile->token_buffer + save_written;
  enum cpp_token token = get_directive_token (pfile);
  CPP_SET_WRITTEN (pfile, save_written);

  if (token == CPP_NUMBER && *p >= '1' && *p <= '4' && p[1] == '\0')
    {
      *num = p[0] - '0';
      return 1;
    }
  else
    {
      if (token != CPP_VSPACE && token != CPP_EOF)
	cpp_error (pfile, "invalid format `#line' command");
      return 0;
    }
}

/* Interpret #line command.
   Note that the filename string (if any) is treated as if it were an
   include filename.  That means no escape handling.  */

static int
do_line (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  int new_lineno;
  long old_written = CPP_WRITTEN (pfile);
  enum file_change_code file_change = same_file;
  enum cpp_token token;
  char *x;

  token = get_directive_token (pfile);

  if (token != CPP_NUMBER)
    {
      cpp_error (pfile, "token after `#line' is not an integer");
      goto bad_line_directive;
    }

  new_lineno = strtol (pfile->token_buffer + old_written, &x, 10);
  if (x[0] != '\0')
    {
      cpp_error (pfile, "token after `#line' is not an integer");
      goto bad_line_directive;
    }      
  CPP_SET_WRITTEN (pfile, old_written);

  if (CPP_PEDANTIC (pfile) && (new_lineno <= 0 || new_lineno > 32767))
    cpp_pedwarn (pfile, "line number out of range in `#line' command");

  token = get_directive_token (pfile);

  if (token == CPP_STRING)
    {
      U_CHAR *fname = pfile->token_buffer + old_written + 1;
      U_CHAR *end_name = CPP_PWRITTEN (pfile) - 1;
      int action_number = 0;

      file_change = rename_file;

      if (read_line_number (pfile, &action_number))
	{
	  if (CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "garbage at end of `#line' command");

	  if (action_number == 1)
	    {
	      file_change = enter_file;
	      read_line_number (pfile, &action_number);
	    }
	  else if (action_number == 2)
	    {
	      file_change = leave_file;
	      read_line_number (pfile, &action_number);
	    }
	  if (action_number == 3)
	    {
	      ip->system_header_p = 1;
	      read_line_number (pfile, &action_number);
	    }
	  if (action_number == 4)
	    {
	      ip->system_header_p = 2;
	      read_line_number (pfile, &action_number);
	    }
	}
      
      *end_name = '\0';
      
      if (strcmp (fname, ip->nominal_fname))
	{
	  const char *newname, *oldname;
	  if (!strcmp (fname, ip->fname))
	    newname = ip->fname;
	  else if (ip->last_nominal_fname
		   && !strcmp (fname, ip->last_nominal_fname))
	    newname = ip->last_nominal_fname;
	  else
	    newname = xstrdup (fname);

	  oldname = ip->nominal_fname;
	  ip->nominal_fname = newname;

	  if (ip->last_nominal_fname
	      && ip->last_nominal_fname != oldname
	      && ip->last_nominal_fname != newname
	      && ip->last_nominal_fname != ip->fname)
	    free ((void *) ip->last_nominal_fname);

	  if (newname == ip->fname)
	    ip->last_nominal_fname = NULL;
	  else
	    ip->last_nominal_fname = oldname;
	} 
    }
  else if (token != CPP_VSPACE && token != CPP_EOF)
    {
      cpp_error (pfile, "token after `#line %d' is not a string", new_lineno);
      goto bad_line_directive;
    }

  /* The Newline at the end of this line remains to be processed.
     To put the next line at the specified line number,
     we must store a line number now that is one less.  */
  ip->lineno = new_lineno - 1;
  CPP_SET_WRITTEN (pfile, old_written);
  output_line_command (pfile, file_change);
  return 0;

 bad_line_directive:
  skip_rest_of_line (pfile);
  CPP_SET_WRITTEN (pfile, old_written);
  return 0;
}

/* Remove the definition of a symbol from the symbol table.
   According to the C standard, it is not an error to undef
   something that has no definitions. */
static int
do_undef (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword;
{
  int len;
  HASHNODE *hp;
  U_CHAR *buf, *name, *limit;
  int c;
  long here = CPP_WRITTEN (pfile);
  enum cpp_token token;

  cpp_skip_hspace (pfile);
  c = GETC();
  if (! is_idstart(c))
  {
      cpp_error (pfile, "token after #undef is not an identifier");
      skip_rest_of_line (pfile);
      return 1;
  }

  parse_name (pfile, c);
  buf = pfile->token_buffer + here;
  limit = CPP_PWRITTEN(pfile);

  /* Copy out the token so we can pop the token buffer. */
  len = limit - buf;
  name = (U_CHAR *) alloca (len + 1);
  memcpy (name, buf, len);
  name[len] = '\0';

  token = get_directive_token (pfile);
  if (token != CPP_VSPACE)
  {
      cpp_pedwarn (pfile, "junk on line after #undef");
      skip_rest_of_line (pfile);
  }
  CPP_SET_WRITTEN (pfile, here);

  while ((hp = _cpp_lookup (pfile, name, len)) != NULL)
    {
      /* If we are generating additional info for debugging (with -g) we
	 need to pass through all effective #undef commands.  */
      if (CPP_OPTIONS (pfile)->debug_output && keyword)
	pass_thru_directive (name, len, pfile, keyword);
      if (hp->type == T_POISON)
	cpp_error (pfile, "cannot undefine poisoned `%s'", hp->name);
      else 
	{
	  if (hp->type != T_MACRO)
	    cpp_warning (pfile, "undefining `%s'", hp->name);
	  _cpp_delete_macro (hp);
	}
    }

  return 0;
}

/* Wrap do_undef for -U processing. */
void
cpp_undef (pfile, macro)
     cpp_reader *pfile;
     U_CHAR *macro;
{
  /* Copy the string so we can append a newline.  */
  size_t len = strlen (macro);
  U_CHAR *buf = alloca (len + 2);
  memcpy (buf, macro, len);
  buf[len]     = '\n';
  buf[len + 1] = '\0';
  if (cpp_push_buffer (pfile, buf, len + 1))
    {
      do_undef (pfile, NULL);
      cpp_pop_buffer (pfile);
    }
}

/*
 * Report an error detected by the program we are processing.
 * Use the text of the line in the error message.
 * (We use error because it prints the filename & line#.)
 */

static int
do_error (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  U_CHAR *text, *limit;

  cpp_skip_hspace (pfile);
  text = CPP_BUFFER (pfile)->cur;
  skip_rest_of_line (pfile);
  limit = CPP_BUFFER (pfile)->cur;

  cpp_error (pfile, "#error %.*s", (int)(limit - text), text);
  return 0;
}

/*
 * Report a warning detected by the program we are processing.
 * Use the text of the line in the warning message, then continue.
 */

static int
do_warning (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  U_CHAR *text, *limit;

  cpp_skip_hspace (pfile);
  text = CPP_BUFFER (pfile)->cur;
  skip_rest_of_line (pfile);
  limit = CPP_BUFFER (pfile)->cur;

  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#warning'");

  cpp_warning (pfile, "#warning %.*s", (int)(limit - text), text);
  return 0;
}

/* Report program identification.  */

static int
do_ident (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  long old_written = CPP_WRITTEN (pfile);

  /* Allow #ident in system headers, since that's not user's fault.  */
  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#ident'");

  CPP_PUTS (pfile, "#ident ", 7);

  /* Next token should be a string constant.  */
  if (get_directive_token (pfile) == CPP_STRING)
    /* And then a newline.  */
    if (get_directive_token (pfile) == CPP_VSPACE)
      /* Good - ship it.  */
      return 0;

  cpp_error (pfile, "invalid #ident");
  skip_rest_of_line (pfile);
  CPP_SET_WRITTEN (pfile, old_written);  /* discard directive */

  return 0;
}

/* Pragmata handling.  We handle some of these, and pass the rest on
   to the front end.  C99 defines three pragmas and says that no macro
   expansion is to be performed on them; whether or not macro
   expansion happens for other pragmas is implementation defined.
   This implementation never macro-expands the text after #pragma.

   We currently do not support the _Pragma operator.  Support for that
   has to be coordinated with the front end.  Proposed implementation:
   both #pragma blah blah and _Pragma("blah blah") become
   __builtin_pragma(blah blah) and we teach the parser about that.  */

/* Sub-handlers for the pragmas needing treatment here.
   They return 1 if the token buffer is to be popped, 0 if not. */
static int do_pragma_once		PARAMS ((cpp_reader *));
static int do_pragma_implementation	PARAMS ((cpp_reader *));
static int do_pragma_poison		PARAMS ((cpp_reader *));
static int do_pragma_default		PARAMS ((cpp_reader *));

static int
do_pragma (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  long here, key;
  U_CHAR *buf;
  int pop;
  enum cpp_token token;

  here = CPP_WRITTEN (pfile);
  CPP_PUTS (pfile, "#pragma ", 8);

  key = CPP_WRITTEN (pfile);
  pfile->no_macro_expand++;
  token = get_directive_token (pfile);
  if (token != CPP_NAME)
    {
      if (token == CPP_VSPACE)
	goto empty;
      else
	goto skip;
    }

  buf = pfile->token_buffer + key;
  CPP_PUTC (pfile, ' ');

#define tokis(x) !strncmp(buf, x, sizeof(x) - 1)
  if (tokis ("once"))
    pop = do_pragma_once (pfile);
  else if (tokis ("implementation"))
    pop = do_pragma_implementation (pfile);
  else if (tokis ("poison"))
    pop = do_pragma_poison (pfile);
  else
    pop = do_pragma_default (pfile);
#undef tokis

  if (get_directive_token (pfile) != CPP_VSPACE)
    goto skip;

  if (pop)
    CPP_SET_WRITTEN (pfile, here);
  pfile->no_macro_expand--;
  return 0;

 skip:
  cpp_error (pfile, "malformed #pragma directive");
  skip_rest_of_line (pfile);
 empty:
  CPP_SET_WRITTEN (pfile, here);
  pfile->no_macro_expand--;
  return 0;
}

static int
do_pragma_default (pfile)
     cpp_reader *pfile;
{
  while (get_directive_token (pfile) != CPP_VSPACE)
    CPP_PUTC (pfile, ' ');
  return 0;
}

static int
do_pragma_once (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  if (ip->fname == NULL)
    {
      cpp_ice (pfile, "ip->fname == NULL in do_pragma_once");
      return 1;
    }
  
  /* Allow #pragma once in system headers, since that's not the user's
     fault.  */
  if (!ip->system_header_p)
    cpp_warning (pfile, "`#pragma once' is obsolete");
      
  if (CPP_PREV_BUFFER (ip) == CPP_NULL_BUFFER (pfile))
    cpp_warning (pfile, "`#pragma once' outside include file");
  else
    ip->ihash->control_macro = "";  /* never repeat */

  return 1;
}

static int
do_pragma_implementation (pfile)
     cpp_reader *pfile;
{
  /* Be quiet about `#pragma implementation' for a file only if it hasn't
     been included yet.  */
  enum cpp_token token;
  long written = CPP_WRITTEN (pfile);
  U_CHAR *name;
  U_CHAR *copy;

  token = get_directive_token (pfile);
  if (token == CPP_VSPACE)
    return 0;
  else if (token != CPP_STRING)
    {
      cpp_error (pfile, "malformed #pragma implementation");
      return 1;
    }

  name = pfile->token_buffer + written + 1;
  copy = xstrdup (name);
  copy[strlen(copy)] = '\0';  /* trim trailing quote */

  if (cpp_included (pfile, copy))
    cpp_warning (pfile,
	 "`#pragma implementation' for `%s' appears after file is included",
		 copy);
  free (copy);
  return 0;
}

static int
do_pragma_poison (pfile)
     cpp_reader *pfile;
{
  /* Poison these symbols so that all subsequent usage produces an
     error message.  */
  U_CHAR *p;
  HASHNODE *hp;
  long written;
  size_t len;
  enum cpp_token token;
  int writeit;
  /* As a rule, don't include #pragma poison commands in output,  
     unless the user asks for them.  */
  writeit = (CPP_OPTIONS (pfile)->debug_output
	     || CPP_OPTIONS (pfile)->dump_macros == dump_definitions
	     || CPP_OPTIONS (pfile)->dump_macros == dump_names);

  for (;;)
    {
      written = CPP_WRITTEN (pfile);
      token = get_directive_token (pfile);
      if (token == CPP_VSPACE)
	break;
      if (token != CPP_NAME)
	{
	  cpp_error (pfile, "invalid #pragma poison directive");
	  skip_rest_of_line (pfile);
	  return 1;
	}

      p = pfile->token_buffer + written;
      len = strlen (p);
      if ((hp = _cpp_lookup (pfile, p, len)))
	{
	  if (hp->type != T_POISON)
	    {
	      cpp_warning (pfile, "poisoning existing macro `%s'", p);
	      if (hp->type == T_MACRO)
		_cpp_free_definition (hp->value.defn);
	      hp->value.defn = 0;
	      hp->type = T_POISON;
	    }
	}
      else
	_cpp_install (pfile, p, len, T_POISON, 0);
      if (writeit)
	CPP_PUTC (pfile, ' ');
    }
  return !writeit;
}
 
#ifdef SCCS_DIRECTIVE
/* Just ignore #sccs, on systems where we define it at all.  */

static int
do_sccs (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#sccs'");
  skip_rest_of_line (pfile);
  return 0;
}
#endif

/* We've found an `#if' directive.  If the only thing before it in
   this file is white space, and if it is of the form
   `#if ! defined SYMBOL', then SYMBOL is a possible controlling macro
   for inclusion of this file.  (See redundant_include_p in cppfiles.c
   for an explanation of controlling macros.)  If so, return a
   malloc'd copy of SYMBOL.  Otherwise, return NULL.  */

static U_CHAR *
detect_if_not_defined (pfile)
     cpp_reader *pfile;
{
  U_CHAR *control_macro = 0;

  if (pfile->only_seen_white == 2)
    {
      char *ident;
      enum cpp_token token;
      int base_offset;
      int token_offset;
      int need_rparen = 0;

      /* Save state required for restore.  */
      pfile->no_macro_expand++;
      parse_set_mark (pfile);
      base_offset = CPP_WRITTEN (pfile);

      /* Look for `!', */
      if (get_directive_token (pfile) != CPP_OTHER
	  || CPP_WRITTEN (pfile) != (size_t) base_offset + 1
	  || CPP_PWRITTEN (pfile)[-1] != '!')
	goto restore;

      /* ...then `defined', */
      token_offset = CPP_WRITTEN (pfile);
      token = get_directive_token (pfile);
      if (token != CPP_NAME)
	goto restore;
      ident = pfile->token_buffer + token_offset;
      CPP_NUL_TERMINATE (pfile);
      if (strcmp (ident, "defined"))
	goto restore;

      /* ...then an optional '(' and the name, */
      token_offset = CPP_WRITTEN (pfile);
      token = get_directive_token (pfile);
      if (token == CPP_LPAREN)
	{
	  token_offset = CPP_WRITTEN (pfile);
	  token = get_directive_token (pfile);
	  if (token != CPP_NAME)
	    goto restore;
	  need_rparen = 1;
	}
      else if (token != CPP_NAME)
	goto restore;

      ident = pfile->token_buffer + token_offset;
      CPP_NUL_TERMINATE (pfile);

      /* ...then the ')', if necessary, */
      if ((!need_rparen || get_directive_token (pfile) == CPP_RPAREN)
	  /* ...and make sure there's nothing else on the line.  */
	  && get_directive_token (pfile) == CPP_VSPACE)
	control_macro = xstrdup (ident);

    restore:
      CPP_SET_WRITTEN (pfile, base_offset);
      pfile->no_macro_expand--;
      parse_goto_mark (pfile);
    }

  return control_macro;
}

/*
 * handle #if command by
 *   1) inserting special `defined' keyword into the hash table
 *	that gets turned into 0 or 1 by special_symbol (thus,
 *	if the luser has a symbol called `defined' already, it won't
 *      work inside the #if command)
 *   2) rescan the input into a temporary output buffer
 *   3) pass the output buffer to the yacc parser and collect a value
 *   4) clean up the mess left from steps 1 and 2.
 *   5) call conditional_skip to skip til the next #endif (etc.),
 *      or not, depending on the value from step 3.
 */

static int
do_if (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  U_CHAR *control_macro = detect_if_not_defined (pfile);
  HOST_WIDEST_INT value = eval_if_expression (pfile);
  conditional_skip (pfile, value == 0, T_IF, control_macro);
  return 0;
}

/*
 * handle a #elif directive by not changing  if_stack  either.
 * see the comment above do_else.
 */

static int
do_elif (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack)
    {
      cpp_error (pfile, "`#elif' not within a conditional");
      return 0;
    }
  else
    {
      if (pfile->if_stack->type != T_IF && pfile->if_stack->type != T_ELIF)
	{
	  cpp_error (pfile, "`#elif' after `#else'");
	  cpp_error_with_line (pfile, pfile->if_stack->lineno, -1,
			       "the conditional began here");
	}
      pfile->if_stack->type = T_ELIF;
    }

  if (pfile->if_stack->if_succeeded)
    skip_if_group (pfile);
  else
    {
      HOST_WIDEST_INT value = eval_if_expression (pfile);
      if (value == 0)
	skip_if_group (pfile);
      else
	{
	  ++pfile->if_stack->if_succeeded;	/* continue processing input */
	  output_line_command (pfile, same_file);
	}
    }
  return 0;
}

/*
 * evaluate a #if expression in BUF, of length LENGTH,
 * then parse the result as a C expression and return the value as an int.
 */

static HOST_WIDEST_INT
eval_if_expression (pfile)
     cpp_reader *pfile;
{
  HOST_WIDEST_INT value;
  long old_written = CPP_WRITTEN (pfile);

  pfile->parsing_if_directive++;
  value = _cpp_parse_expr (pfile);
  pfile->parsing_if_directive--;

  skip_rest_of_line (pfile);
  CPP_SET_WRITTEN (pfile, old_written); /* Pop */

  return value;
}

/*
 * routine to handle ifdef/ifndef.  Try to look up the symbol,
 * then do or don't skip to the #endif/#else/#elif depending
 * on what directive is actually being processed.
 */

static int
do_xifdef (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword;
{
  int skip;
  cpp_buffer *ip = CPP_BUFFER (pfile);
  U_CHAR *ident;
  int ident_length;
  enum cpp_token token;
  int start_of_file = 0;
  U_CHAR *control_macro = 0;
  int old_written = CPP_WRITTEN (pfile);

  /* Detect a #ifndef at start of file (not counting comments).  */
  if (ip->fname != 0 && keyword->type == T_IFNDEF)
    start_of_file = pfile->only_seen_white == 2;

  pfile->no_macro_expand++;
  token = get_directive_token (pfile);
  pfile->no_macro_expand--;

  ident = pfile->token_buffer + old_written;
  ident_length = CPP_WRITTEN (pfile) - old_written;
  CPP_SET_WRITTEN (pfile, old_written); /* Pop */

  if (token == CPP_VSPACE || token == CPP_POP || token == CPP_EOF)
    {
      skip = (keyword->type == T_IFDEF);
      if (! CPP_TRADITIONAL (pfile))
	cpp_pedwarn (pfile, "`#%s' with no argument", keyword->name);
    }
  else if (token == CPP_NAME)
    {
      skip = cpp_defined (pfile, ident, ident_length);
      if (keyword->type == T_IFDEF)
	skip = !skip;

      if (start_of_file && !skip)
	{
	  control_macro = (U_CHAR *) xmalloc (ident_length + 1);
	  bcopy (ident, control_macro, ident_length + 1);
	}
    }
  else
    {
      skip = (keyword->type == T_IFDEF);
      if (! CPP_TRADITIONAL (pfile))
	cpp_error (pfile, "`#%s' with invalid argument", keyword->name);
    }

  if (!CPP_TRADITIONAL (pfile))
    { int c;
      cpp_skip_hspace (pfile);
      c = PEEKC ();
      if (c != EOF && c != '\n')
	cpp_pedwarn (pfile, "garbage at end of `#%s' argument", keyword->name);
    }
  skip_rest_of_line (pfile);

  conditional_skip (pfile, skip, T_IF, control_macro);
  return 0;
}

/* Push TYPE on stack; then, if SKIP is nonzero, skip ahead.
   If this is a #ifndef starting at the beginning of a file,
   CONTROL_MACRO is the macro name tested by the #ifndef.
   Otherwise, CONTROL_MACRO is 0.  */

static void
conditional_skip (pfile, skip, type, control_macro)
     cpp_reader *pfile;
     int skip;
     enum node_type type;
     U_CHAR *control_macro;
{
  IF_STACK_FRAME *temp;

  temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
  temp->fname = CPP_BUFFER (pfile)->nominal_fname;
  temp->lineno = CPP_BUFFER (pfile)->lineno;
  temp->next = pfile->if_stack;
  temp->control_macro = control_macro;
  pfile->if_stack = temp;

  pfile->if_stack->type = type;

  if (skip != 0) {
    skip_if_group (pfile);
    return;
  } else {
    ++pfile->if_stack->if_succeeded;
    output_line_command (pfile, same_file);
  }
}

/* Subroutine of skip_if_group.	 Examine one preprocessing directive and
   return 0 if skipping should continue, 1 if it should halt.  Also
   adjusts the if_stack as appropriate.
   The `#' has been read, but not the identifier. */

static int
consider_directive_while_skipping (pfile, stack)
    cpp_reader *pfile;
    IF_STACK_FRAME *stack; 
{
  long ident_len, ident;
  const struct directive *kt;
  IF_STACK_FRAME *temp;
    
  cpp_skip_hspace (pfile);

  ident = CPP_WRITTEN (pfile);
  parse_name (pfile, GETC());
  ident_len = CPP_WRITTEN (pfile) - ident;

  CPP_SET_WRITTEN (pfile, ident);

  for (kt = directive_table; kt->length >= 0; kt++)
    if (kt->length == ident_len
	&& strncmp (pfile->token_buffer + ident, kt->name, kt->length) == 0)
      switch (kt->type)
	{
	case T_IF:
	case T_IFDEF:
	case T_IFNDEF:
	    temp = (IF_STACK_FRAME *) xmalloc (sizeof (IF_STACK_FRAME));
	    temp->next = pfile->if_stack;
	    pfile->if_stack = temp;
	    temp->fname = CPP_BUFFER(pfile)->nominal_fname;
	    temp->type = kt->type;
	    return 0;

	case T_ELSE:
	    if (pfile->if_stack != stack)
	      validate_else (pfile, "#else");
	    /* fall through */
	case T_ELIF:
	    if (pfile->if_stack == stack)
	      return 1;
	    else
	      {
		pfile->if_stack->type = kt->type;
		return 0;
	      }

	    case T_ENDIF:
		if (pfile->if_stack != stack)
		  validate_else (pfile, "#endif");

		if (pfile->if_stack == stack)
		  return 1;
		    
		temp = pfile->if_stack;
		pfile->if_stack = temp->next;
		free (temp);
		return 0;

	    default:
		return 0;
	    }

    /* Don't let erroneous code go by.	*/
    if (!CPP_OPTIONS (pfile)->lang_asm && CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "invalid preprocessor directive name");
    return 0;
}

/* skip to #endif, #else, or #elif.  adjust line numbers, etc.
 * leaves input ptr at the sharp sign found.
 */
static void
skip_if_group (pfile)
    cpp_reader *pfile;
{
  int c;
  IF_STACK_FRAME *save_if_stack = pfile->if_stack; /* don't pop past here */
  U_CHAR *beg_of_line;
  long old_written;

  old_written = CPP_WRITTEN (pfile);
  
  for (;;)
    {
      beg_of_line = CPP_BUFFER (pfile)->cur;

      if (! CPP_TRADITIONAL (pfile))
	cpp_skip_hspace (pfile);
      c = GETC();
      if (c == '\n')
	{
	  CPP_BUMP_LINE (pfile);
	  continue;
	}
      else if (c == '#')
	{
	  if (consider_directive_while_skipping (pfile, save_if_stack))
	    break;
	}
      else if (c == EOF)
	return;	 /* Caller will issue error. */

      FORWARD(-1);
      skip_rest_of_line (pfile);

      c = GETC();
      if (c == EOF)
	return;	 /* Caller will issue error. */
      else
	CPP_BUMP_LINE (pfile);
    }	  

  /* Back up to the beginning of this line.  Caller will process the
     directive. */
  CPP_BUFFER (pfile)->cur = beg_of_line;
  pfile->only_seen_white = 1;
}

/*
 * handle a #else directive.  Do this by just continuing processing
 * without changing  if_stack ;  this is so that the error message
 * for missing #endif's etc. will point to the original #if.  It
 * is possible that something different would be better.
 */

static int
do_else (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  validate_else (pfile, "#else");
  skip_rest_of_line (pfile);

  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack)
    {
      cpp_error (pfile, "`#else' not within a conditional");
      return 0;
    }
  else
    {
      /* #ifndef can't have its special treatment for containing the whole file
	 if it has a #else clause.  */
      pfile->if_stack->control_macro = 0;

      if (pfile->if_stack->type != T_IF && pfile->if_stack->type != T_ELIF)
	{
	  cpp_error (pfile, "`#else' after `#else'");
	  cpp_error_with_line (pfile, pfile->if_stack->lineno, -1,
			       "the conditional began here");
	}
      pfile->if_stack->type = T_ELSE;
    }

  if (pfile->if_stack->if_succeeded)
    skip_if_group (pfile);
  else
    {
      ++pfile->if_stack->if_succeeded;	/* continue processing input */
      output_line_command (pfile, same_file);
    }
  return 0;
}

/*
 * unstack after #endif command
 */

static int
do_endif (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  validate_else (pfile, "#endif");
  skip_rest_of_line (pfile);

  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack)
    cpp_error (pfile, "`#endif' not within a conditional");
  else
    {
      IF_STACK_FRAME *temp = pfile->if_stack;
      pfile->if_stack = temp->next;
      if (temp->control_macro != 0)
	{
	  /* This #endif matched a #ifndef at the start of the file.
	     See if it is at the end of the file.  */
	  int c;

	  parse_set_mark (pfile);

	  for (;;)
	    {
	      cpp_skip_hspace (pfile);
	      c = GETC ();
	      if (c != '\n')
		break;
	    }
	  parse_goto_mark (pfile);

	  if (c == EOF)
	    {
	      /* This #endif ends a #ifndef
		 that contains all of the file (aside from whitespace).
		 Arrange not to include the file again
		 if the macro that was tested is defined. */
	      struct cpp_buffer *ip;
	      for (ip = CPP_BUFFER (pfile); ; ip = CPP_PREV_BUFFER (ip))
		if (ip->fname != NULL)
		  break;
	      ip->ihash->control_macro = (char *) temp->control_macro;
	    }
        }
      free (temp);
      output_line_command (pfile, same_file);
    }
  return 0;
}

/* Issue -pedantic warning for text which is not a comment following
   an #else or #endif.  Do not warn in system headers, as this is harmless
   and very common on old systems.  */

static void
validate_else (pfile, directive)
     cpp_reader *pfile;
     const char *directive;
{
  if (! CPP_PEDANTIC (pfile))
    return;

  cpp_skip_hspace (pfile);
  if (PEEKC () != '\n')
    cpp_pedwarn (pfile,
		 "text following `%s' violates ANSI standard", directive);
}

/* Convert T_IF, etc. to a string.   Used in error messages.  */
static const char *
if_directive_name (pfile, ifs)
     cpp_reader *pfile;
     struct if_stack *ifs;
{
  switch (ifs->type)
    {
    case T_IF:	    return "#if";
    case T_IFDEF:   return "#ifdef";
    case T_IFNDEF:  return "#ifndef";
    case T_ELIF:    return "#elif";
    case T_ELSE:    return "#else";
    default:
      cpp_ice (pfile, "impossible if_stack->type value %d", ifs->type);
      return "unknown";
    }
}

/* Get the next token, and add it to the text in pfile->token_buffer.
   Return the kind of token we got.  */

enum cpp_token
cpp_get_token (pfile)
     cpp_reader *pfile;
{
  register int c, c2, c3;
  enum cpp_token token;
  struct cpp_options *opts = CPP_OPTIONS (pfile);

 get_next:
  c = GETC();
  if (c == EOF)
    {
      if (CPP_BUFFER (pfile)->manual_pop)
	/* If we've been reading from redirected input, the
	   frontend will pop the buffer.  */
	return CPP_EOF;
      else if (CPP_BUFFER (pfile)->seen_eof)
	{
	  if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)) == CPP_NULL_BUFFER (pfile))
	    return CPP_EOF;

	  cpp_pop_buffer (pfile);
	  goto get_next;
	}
      else
	{
	  cpp_buffer *next_buf = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
	  struct if_stack *ifs, *nifs;

	  /* Unwind the conditional stack and generate error messages.  */
	  for (ifs = pfile->if_stack;
	       ifs != CPP_BUFFER (pfile)->if_stack;
	       ifs = nifs)
	    {
	      cpp_error_with_line (pfile, ifs->lineno, -1,
				   "unterminated `%s' conditional",
				   if_directive_name (pfile, ifs));

	      nifs = ifs->next;
	      free (ifs);
	    }
	  pfile->if_stack = ifs;

	  if (CPP_BUFFER (pfile)->nominal_fname
	      && next_buf != CPP_NULL_BUFFER (pfile))
	    {
	      /* We're about to return from an #include file.
		 Emit #line information now (as part of the CPP_POP) result.
		 But the #line refers to the file we will pop to.  */
	      cpp_buffer *cur_buffer = CPP_BUFFER (pfile);
	      CPP_BUFFER (pfile) = next_buf;
	      pfile->input_stack_listing_current = 0;
	      output_line_command (pfile, leave_file);
	      CPP_BUFFER (pfile) = cur_buffer;
	    }

	  CPP_BUFFER (pfile)->seen_eof = 1;
	  return CPP_POP;
	}
    }
  else
    {
      switch (c)
	{
	case '/':
	  if (PEEKC () == '=')
	    goto op2;

	comment:
	  if (opts->discard_comments)
	    c = skip_comment (pfile, c);
	  else
	    c = copy_comment (pfile, c);
	  if (c != ' ')
	    goto randomchar;
	  
	  /* Comments are equivalent to spaces.
	     For -traditional, a comment is equivalent to nothing.  */
	  if (opts->traditional || !opts->discard_comments)
	    return CPP_COMMENT;
	  else
	    {
	      CPP_PUTC (pfile, c);
	      return CPP_HSPACE;
	    }

	case '#':
	  if (pfile->parsing_if_directive)
	    {
	      cpp_skip_hspace (pfile);
	      parse_assertion (pfile);
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
	  if (handle_directive (pfile))
	    return CPP_DIRECTIVE;
	  pfile->only_seen_white = 0;
	  goto randomchar;

	case '\"':
	case '\'':
	  parse_string (pfile, c);
	  pfile->only_seen_white = 0;
	  return c == '\'' ? CPP_CHAR : CPP_STRING;

	case '$':
	  if (!opts->dollars_in_ident)
	    goto randomchar;
	  goto letter;

	case ':':
	  if (opts->cplusplus && PEEKC () == ':')
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
	  if (c2 == '-' && opts->chill)
	    goto comment;  /* Chill style comment */
	  if (c2 == '-' || c2 == '=')
	    goto op2;
	  if (c2 == '>')
	    {
	      if (opts->cplusplus && PEEKN (1) == '*')
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
	  if (c2 != c && (!opts->cplusplus || c2 != '?'))
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
	  if (opts->cplusplus && c2 == '*')
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
		       && ((c2 != 'p' && c2 != 'P') || CPP_C89 (pfile)))
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
	  if (opts->chill && PEEKC () == '\'')
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
          {
	    HASHNODE *hp;
	    unsigned char *ident;
	    int before_name_written = CPP_WRITTEN (pfile);
	    int ident_len;
	    parse_name (pfile, c);
	    pfile->only_seen_white = 0;
	    if (pfile->no_macro_expand)
	      return CPP_NAME;
	    ident = pfile->token_buffer + before_name_written;
	    ident_len = CPP_PWRITTEN (pfile) - ident;
	    hp = _cpp_lookup (pfile, ident, ident_len);
	    if (!hp)
	      return CPP_NAME;
	    if (hp->type == T_DISABLED)
	      {
		if (pfile->output_escapes)
		  { /* Return "\r-IDENT", followed by '\0'.  */
		    int i;
		    CPP_RESERVE (pfile, 3);
		    ident = pfile->token_buffer + before_name_written;
		    CPP_ADJUST_WRITTEN (pfile, 2);
		    for (i = ident_len; i >= 0; i--) ident[i+2] = ident[i];
		    ident[0] = '\r';
		    ident[1] = '-';
		  }
		return CPP_NAME;
	      }

	    /* If macro wants an arglist, verify that a '(' follows.  */
	    if (hp->type == T_MACRO && hp->value.defn->nargs >= 0)
	    {
	      int macbuf_whitespace = 0;

	      while (CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
		{
		  U_CHAR *point = CPP_BUFFER (pfile)->cur;
		  for (;;)
		    {
		      cpp_skip_hspace (pfile);
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

	      parse_set_mark (pfile);
	      for (;;)
		{
		  cpp_skip_hspace (pfile);
		  c = PEEKC ();
		  if (c == '\n')
		    FORWARD(1);
		  else
		    break;
		}
	      parse_goto_mark (pfile);

	      if (c == '(')
		goto is_macro_call;

	    not_macro_call:
	      if (macbuf_whitespace)
		CPP_PUTC (pfile, ' ');
	      return CPP_NAME;
	    }
	  is_macro_call:
	    /* This is now known to be a macro call.
	       Expand the macro, reading arguments as needed,
	       and push the expansion on the input stack.  */
	    _cpp_macroexpand (pfile, hp);
	    CPP_SET_WRITTEN (pfile, before_name_written);
	  }
	  goto get_next;

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
		  parse_name (pfile, GETC ());
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
	  if (! CPP_OPTIONS (pfile)->no_line_commands)
	    {
	      pfile->lineno++;
	      if (CPP_BUFFER (pfile)->lineno != pfile->lineno)
		output_line_command (pfile, same_file);
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

/* Parse an identifier starting with C.  */

static void
parse_name (pfile, c)
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
	  if (CPP_OPTIONS (pfile)->lang_fortran
	      || CPP_OPTIONS (pfile)->lang_asm)
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
  U_CHAR *start = CPP_BUFFER (pfile)->cur;  /* XXX Layering violation */
  U_CHAR *limit;

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
static int
parse_assertion (pfile)
     cpp_reader *pfile;
{
  int c, dropwhite;
  cpp_skip_hspace (pfile);
  c = PEEKC();
  if (! is_idstart(c))
    {
      cpp_error (pfile, "assertion predicate is not an identifier");
      return 0;
    }
  CPP_PUTC(pfile, '#');
  FORWARD(1);
  parse_name(pfile, c);

  c = PEEKC();
  if (c != '(')
    {
      if (is_hspace(c) || c == '\r')
	cpp_skip_hspace (pfile);
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

static int
do_assert (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  char *sym;
  int ret, c;
  HASHNODE *base, *this;
  int baselen, thislen;

  if (CPP_PEDANTIC (pfile) && CPP_OPTIONS (pfile)->done_initializing)
    cpp_pedwarn (pfile, "ANSI C does not allow `#assert'");

  cpp_skip_hspace (pfile);
  sym = (char *) CPP_PWRITTEN (pfile);	/* remember where it starts */
  ret = parse_assertion (pfile);
  if (ret == 0)
    goto error;
  else if (ret == 1)
    {
      cpp_error (pfile, "missing token-sequence in `#assert'");
      goto error;
    }

  cpp_skip_hspace (pfile);
  c = PEEKC();
  if (c != EOF && c != '\n')
    {
      cpp_error (pfile, "junk at end of `#assert'");
      goto error;
    }

  thislen = strlen (sym);
  baselen = index (sym, '(') - sym;
  this = _cpp_lookup (pfile, sym, thislen);
  if (this)
    {
      cpp_warning (pfile, "`%s' re-asserted", sym);
      goto error;
    }

  base = _cpp_lookup (pfile, sym, baselen);
  if (! base)
    base = _cpp_install (pfile, sym, baselen, T_ASSERT, 0);
  else if (base->type != T_ASSERT)
  {
    /* Token clash - but with what?! */
    cpp_ice (pfile, "base->type != T_ASSERT in do_assert");
    goto error;
  }

  this = _cpp_install (pfile, sym, thislen, T_ASSERT,
		      (char *)base->value.aschain);
  base->value.aschain = this;
  
  pfile->limit = (unsigned char *) sym; /* Pop */
  return 0;

 error:
  skip_rest_of_line (pfile);
  pfile->limit = (unsigned char *) sym; /* Pop */
  return 0;
}

static int
do_unassert (pfile, keyword)
     cpp_reader *pfile;
     const struct directive *keyword ATTRIBUTE_UNUSED;
{
  int c, ret;
  char *sym;
  long baselen, thislen;
  HASHNODE *base, *this, *next;
  
  if (CPP_PEDANTIC (pfile) && CPP_OPTIONS (pfile)->done_initializing)
    cpp_pedwarn (pfile, "ANSI C does not allow `#unassert'");

  cpp_skip_hspace (pfile);

  sym = (char *) CPP_PWRITTEN (pfile);	/* remember where it starts */
  ret = parse_assertion (pfile);
  if (ret == 0)
    goto error;
  
  cpp_skip_hspace (pfile);
  c = PEEKC ();
  if (c != EOF && c != '\n')
      cpp_error (pfile, "junk at end of `#unassert'");

  thislen = strlen (sym);
  if (ret == 1)
    {
      base = _cpp_lookup (pfile, sym, thislen);
      if (! base)
	goto error;  /* It isn't an error to #undef what isn't #defined,
			so it isn't an error to #unassert what isn't
			#asserted either. */
      
      for (this = base->value.aschain; this; this = next)
        {
	  next = this->value.aschain;
	  _cpp_delete_macro (this);
	}
      _cpp_delete_macro (base);
    }
  else
    {
      baselen = index (sym, '(') - sym;
      base = _cpp_lookup (pfile, sym, baselen);
      if (! base) goto error;
      this = _cpp_lookup (pfile, sym, thislen);
      if (! this) goto error;

      next = base;
      while (next->value.aschain != this)
	next = next->value.aschain;

      next->value.aschain = this->value.aschain;
      _cpp_delete_macro (this);

      if (base->value.aschain == NULL)
	_cpp_delete_macro (base);  /* Last answer for this predicate deleted. */
    }
  
  pfile->limit = (unsigned char *) sym; /* Pop */
  return 0;
 error:
  skip_rest_of_line (pfile);
  pfile->limit = (unsigned char *) sym; /* Pop */
  return 0;
}

/* Process STR as if it appeared as the body of an #unassert. */
void
cpp_unassert (pfile, str)
     cpp_reader *pfile;
     unsigned char *str;
{
  if (cpp_push_buffer (pfile, str, strlen (str)) != NULL)
    {
      do_unassert (pfile, NULL);
      cpp_pop_buffer (pfile);
    }
}  

/* Remember the current position of PFILE so it may be returned to
   after looking ahead a bit.

   Note that when you set a mark, you _must_ return to that mark.  You
   may not forget about it and continue parsing.  You may not pop a
   buffer with an active mark.  You may not call CPP_BUMP_LINE while a
   mark is active.  */

static void
parse_set_mark (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  if (ACTIVE_MARK_P())
      cpp_ice (pfile, "mark active in parse_set_mark");

  ip->mark = ip->cur - ip->buf;
}

/* Backup the current position of PFILE to that saved in its mark,
   and clear the mark.  */

static void
parse_goto_mark (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  if (!ACTIVE_MARK_P())
      cpp_ice (pfile, "mark not active in parse_goto_mark");

  ip->cur = ip->buf + ip->mark;
  ip->mark = -1;
}
