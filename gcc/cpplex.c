/* CPP Library - lexical analysis.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987
   Broken out to separate file, Zack Weinberg, Mar 2000
   Single-pass line tokenization by Neil Booth, April 2000

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

#ifdef HAVE_MMAP_FILE
# include <sys/mman.h>
#endif

#define PEEKBUF(BUFFER, N) \
  ((BUFFER)->rlimit - (BUFFER)->cur > (N) ? (BUFFER)->cur[N] : EOF)
#define GETBUF(BUFFER) \
  ((BUFFER)->cur < (BUFFER)->rlimit ? *(BUFFER)->cur++ : EOF)
#define FORWARDBUF(BUFFER, N) ((BUFFER)->cur += (N))

#define PEEKN(N) PEEKBUF (CPP_BUFFER (pfile), N)
#define FORWARD(N) FORWARDBUF (CPP_BUFFER (pfile), (N))
#define GETC() GETBUF (CPP_BUFFER (pfile))
#define PEEKC() PEEKBUF (CPP_BUFFER (pfile), 0)

static void skip_block_comment	PARAMS ((cpp_reader *));
static void skip_line_comment	PARAMS ((cpp_reader *));
static int maybe_macroexpand	PARAMS ((cpp_reader *, long));
static int skip_comment		PARAMS ((cpp_reader *, int));
static int copy_comment		PARAMS ((cpp_reader *, int));
static void skip_string		PARAMS ((cpp_reader *, int));
static void parse_string	PARAMS ((cpp_reader *, int));
static U_CHAR *find_position	PARAMS ((U_CHAR *, U_CHAR *, unsigned long *));
static void null_warning        PARAMS ((cpp_reader *, unsigned int));

static void safe_fwrite		PARAMS ((cpp_reader *, const U_CHAR *,
					 size_t, FILE *));
static void output_line_command	PARAMS ((cpp_reader *, cpp_printer *,
					 unsigned int));
static void bump_column		PARAMS ((cpp_printer *, unsigned int,
					 unsigned int));
static void expand_name_space   PARAMS ((cpp_toklist *, unsigned int));
static void pedantic_whitespace	PARAMS ((cpp_reader *, U_CHAR *,
					 unsigned int));

#define auto_expand_name_space(list) \
    expand_name_space ((list), 1 + (list)->name_cap / 2)

#ifdef NEW_LEXER

void init_trigraph_map PARAMS ((void));
static unsigned char* trigraph_replace PARAMS ((cpp_reader *, unsigned char *,
						unsigned char *));
static const unsigned char *backslash_start PARAMS ((cpp_reader *,
						     const unsigned char *));
static int skip_block_comment2 PARAMS ((cpp_reader *));
static int skip_line_comment2 PARAMS ((cpp_reader *));
static void skip_whitespace PARAMS ((cpp_reader *, int));
static void parse_name PARAMS ((cpp_reader *, cpp_toklist *, cpp_name *));
static void parse_number PARAMS ((cpp_reader *, cpp_toklist *, cpp_name *));
static void parse_string2 PARAMS ((cpp_reader *, cpp_toklist *, cpp_name *,
				  unsigned int, int));
static int trigraph_ok PARAMS ((cpp_reader *, const unsigned char *));
static void save_comment PARAMS ((cpp_toklist *, cpp_token *,
				  const unsigned char *,
				  unsigned int, unsigned int));
void _cpp_lex_line PARAMS ((cpp_reader *, cpp_toklist *));

static void _cpp_output_list PARAMS ((cpp_reader *, cpp_toklist *));

static unsigned char * spell_token PARAMS ((cpp_reader *, const cpp_token *,
					    unsigned char *, int));

typedef unsigned int (* speller) PARAMS ((unsigned char *, cpp_toklist *,
					  cpp_token *));

/* Macros on a cpp_name.  */
#define INIT_TOKEN_NAME(list, token) \
  do {(token)->val.name.len = 0; \
      (token)->val.name.text = (list)->namebuf + (list)->name_used; \
      (list)->tokens_used = token - (list)->tokens + 1; \
  } while (0)

/* Maybe put these in the ISTABLE eventually.  */
#define IS_HSPACE(c) ((c) == ' ' || (c) == '\t')
#define IS_NEWLINE(c) ((c) == '\n' || (c) == '\r')

/* Handle LF, CR, CR-LF and LF-CR style newlines.  Assumes next
   character, if any, is in buffer.  */
#define handle_newline(cur, limit, c) \
  do {\
  if ((cur) < (limit) && *(cur) == '\r' + '\n' - c) \
    (cur)++; \
  CPP_BUMP_LINE_CUR (pfile, (cur)); \
  pfile->col_adjust = 0; \
  } while (0)

#define IMMED_TOKEN() (!(cur_token->flags & PREV_WHITESPACE))
#define PREV_TOKEN_TYPE (cur_token[-1].type)

#define PUSH_TOKEN(ttype) cur_token++->type = ttype
#define REVISE_TOKEN(ttype) cur_token[-1].type = ttype
#define BACKUP_TOKEN(ttype) (--cur_token)->type = ttype
#define BACKUP_DIGRAPH(ttype) do { \
  BACKUP_TOKEN(ttype); cur_token->flags |= DIGRAPH;} while (0)

/* An upper bound on the number of bytes needed to spell a token,
   including preceding whitespace.  */
#define TOKEN_LEN(token) (5 + (token_spellings[(token)->type].type > \
		               SPELL_NONE ? (token)->val.name.len: 0))

#endif

/* Order here matters.  Those beyond SPELL_NONE store their spelling
   in the token list, and it's length in the token->val.name.len.  */
enum spell_type
{
  SPELL_OPERATOR = 0,
  SPELL_NONE,
  SPELL_CHAR,    /* FIXME: revert order of NONE and CHAR after transition. */
  SPELL_IDENT,
  SPELL_STRING
};

#define T(e, s) {SPELL_OPERATOR, (const U_CHAR *) s},
#define I(e, s) {SPELL_IDENT, s},
#define S(e, s) {SPELL_STRING, s},
#define C(e, s) {SPELL_CHAR, s},
#define N(e, s) {SPELL_NONE, s},

static const struct token_spelling
{
  ENUM_BITFIELD(spell_type) type : CHAR_BIT;
  const U_CHAR *spelling;
} token_spellings [N_TTYPES + 1] = {TTYPE_TABLE {0, 0} };

#undef T
#undef I
#undef S
#undef C
#undef N

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

  new->buf = new->cur = buffer;
  new->rlimit = buffer + length;
  new->prev = buf;
  new->mark = NULL;
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

  if (buf->ihash)
    {
      _cpp_unwind_if_stack (pfile, buf);
      if (buf->buf)
	free ((PTR) buf->buf);
      if (pfile->system_include_depth)
	pfile->system_include_depth--;
      if (pfile->potential_control_macro)
	{
	  buf->ihash->cmacro = pfile->potential_control_macro;
	  pfile->potential_control_macro = 0;
	}
      pfile->input_stack_listing_current = 0;
    }
  else if (buf->macro)
    {
      cpp_hashnode *m = buf->macro;
  
      m->disabled = 0;
      if ((m->type == T_FMACRO && buf->mapped)
	  || m->type == T_SPECLINE || m->type == T_FILE
	  || m->type == T_BASE_FILE || m->type == T_INCLUDE_LEVEL
	  || m->type == T_STDC)
	free ((PTR) buf->buf);
    }
  CPP_BUFFER (pfile) = CPP_PREV_BUFFER (buf);
  free (buf);
  pfile->buffer_stack_depth--;
  return CPP_BUFFER (pfile);
}

/* Deal with the annoying semantics of fwrite.  */
static void
safe_fwrite (pfile, buf, len, fp)
     cpp_reader *pfile;
     const U_CHAR *buf;
     size_t len;
     FILE *fp;
{
  size_t count;

  while (len)
    {
      count = fwrite (buf, 1, len, fp);
      if (count == 0)
	goto error;
      len -= count;
      buf += count;
    }
  return;

 error:
  cpp_notice_from_errno (pfile, CPP_OPTION (pfile, out_fname));
}

/* Notify the compiler proper that the current line number has jumped,
   or the current file name has changed.  */

static void
output_line_command (pfile, print, line)
     cpp_reader *pfile;
     cpp_printer *print;
     unsigned int line;
{
  cpp_buffer *ip = cpp_file_buffer (pfile);
  enum { same = 0, enter, leave, rname } change;
  static const char * const codes[] = { "", " 1", " 2", "" };

  if (CPP_OPTION (pfile, no_line_commands))
    return;

  /* Determine whether the current filename has changed, and if so,
     how.  'nominal_fname' values are unique, so they can be compared
     by comparing pointers.  */
  if (ip->nominal_fname == print->last_fname)
    change = same;
  else
    {
      if (pfile->buffer_stack_depth == print->last_bsd)
	change = rname;
      else
	{
	  if (pfile->buffer_stack_depth > print->last_bsd)
	    change = enter;
	  else
	    change = leave;
	  print->last_bsd = pfile->buffer_stack_depth;
	}
      print->last_fname = ip->nominal_fname;
    }
  /* If the current file has not changed, we can output a few newlines
     instead if we want to increase the line number by a small amount.
     We cannot do this if print->lineno is zero, because that means we
     haven't output any line commands yet.  (The very first line
     command output is a `same_file' command.)  */
  if (change == same && print->lineno != 0
      && line >= print->lineno && line < print->lineno + 8)
    {
      while (line > print->lineno)
	{
	  putc ('\n', print->outf);
	  print->lineno++;
	}
      return;
    }

#ifndef NO_IMPLICIT_EXTERN_C
  if (CPP_OPTION (pfile, cplusplus))
    fprintf (print->outf, "# %u \"%s\"%s%s%s\n", line, ip->nominal_fname,
	     codes[change],
	     ip->system_header_p ? " 3" : "",
	     (ip->system_header_p == 2) ? " 4" : "");
  else
#endif
    fprintf (print->outf, "# %u \"%s\"%s%s\n", line, ip->nominal_fname,
	     codes[change],
	     ip->system_header_p ? " 3" : "");
  print->lineno = line;
}

/* Write the contents of the token_buffer to the output stream, and
   clear the token_buffer.  Also handles generating line commands and
   keeping track of file transitions.  */

void
cpp_output_tokens (pfile, print)
     cpp_reader *pfile;
     cpp_printer *print;
{
  cpp_buffer *ip;

  if (CPP_WRITTEN (pfile) - print->written)
    {
      if (CPP_PWRITTEN (pfile)[-1] == '\n' && print->lineno)
	print->lineno++;
      safe_fwrite (pfile, pfile->token_buffer,
		   CPP_WRITTEN (pfile) - print->written, print->outf);
    }

  ip = cpp_file_buffer (pfile);
  if (ip)
    output_line_command (pfile, print, CPP_BUF_LINE (ip));

  CPP_SET_WRITTEN (pfile, print->written);
}

/* Helper for cpp_output_list - increases the column number to match
   what we expect it to be.  */

static void
bump_column (print, from, to)
     cpp_printer *print;
     unsigned int from, to;
{
  unsigned int tabs, spcs;
  unsigned int delta = to - from;

  /* Only if FROM is 0, advance by tabs.  */
  if (from == 0)
    tabs = delta / 8, spcs = delta % 8;
  else
    tabs = 0, spcs = delta;

  while (tabs--) putc ('\t', print->outf);
  while (spcs--) putc (' ', print->outf);
}

/* Write out the list L onto pfile->token_buffer.  This function is
   incomplete:

   1) pfile->token_buffer is not going to continue to exist.
   2) At the moment, tokens don't carry the information described
   in cpplib.h; they are all strings.
   3) The list has to be a complete line, and has to be written starting
   at the beginning of a line.  */

void
cpp_output_list (pfile, print, list)
     cpp_reader *pfile;
     cpp_printer *print;
     const cpp_toklist *list;
{
  unsigned int i;
  unsigned int curcol = 1;

  /* XXX Probably does not do what is intended.  */
  if (print->lineno != list->line)
    output_line_command (pfile, print, list->line);
  
  for (i = 0; i < list->tokens_used; i++)
    {
      if (TOK_TYPE (list, i) == CPP_VSPACE)
	{
	  output_line_command (pfile, print, list->tokens[i].aux);
	  continue;
	}
	  
      if (curcol < TOK_COL (list, i))
	{
	  /* Insert space to bring the column to what it should be.  */
	  bump_column (print, curcol - 1, TOK_COL (list, i));
	  curcol = TOK_COL (list, i);
	}
      /* XXX We may have to insert space to prevent an accidental
	 token paste.  */
      safe_fwrite (pfile, TOK_NAME (list, i), TOK_LEN (list, i), print->outf);
      curcol += TOK_LEN (list, i);
    }
}

/* Scan a string (which may have escape marks), perform macro expansion,
   and write the result to the token_buffer.  */

void
_cpp_expand_to_buffer (pfile, buf, length)
     cpp_reader *pfile;
     const U_CHAR *buf;
     int length;
{
  cpp_buffer *stop;
  enum cpp_ttype token;
  U_CHAR *buf1;

  if (length < 0)
    {
      cpp_ice (pfile, "length < 0 in cpp_expand_to_buffer");
      return;
    }

  /* Copy the buffer, because it might be in an unsafe place - for
     example, a sequence on the token_buffer, where the pointers will
     be invalidated if we enlarge the token_buffer.  */
  buf1 = alloca (length);
  memcpy (buf1, buf, length);

  /* Set up the input on the input stack.  */
  stop = CPP_BUFFER (pfile);
  if (cpp_push_buffer (pfile, buf1, length) == NULL)
    return;
  CPP_BUFFER (pfile)->has_escapes = 1;

  /* Scan the input, create the output.  */
  for (;;)
    {
      token = cpp_get_token (pfile);
      if (token == CPP_EOF && CPP_BUFFER (pfile) == stop)
	break;
    }
}

/* Scan until CPP_BUFFER (PFILE) is exhausted, discarding output.  */

void
cpp_scan_buffer_nooutput (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *stop = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
  enum cpp_ttype token;
  unsigned int old_written = CPP_WRITTEN (pfile);
  /* In no-output mode, we can ignore everything but directives.  */
  for (;;)
    {
      if (! pfile->only_seen_white)
	_cpp_skip_rest_of_line (pfile);
      token = cpp_get_token (pfile);
      if (token == CPP_EOF && CPP_BUFFER (pfile) == stop)
	break;
    }
  CPP_SET_WRITTEN (pfile, old_written);
}

/* Scan until CPP_BUFFER (pfile) is exhausted, writing output to PRINT.  */

void
cpp_scan_buffer (pfile, print)
     cpp_reader *pfile;
     cpp_printer *print;
{
  cpp_buffer *stop = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
  enum cpp_ttype token;

  for (;;)
    {
      token = cpp_get_token (pfile);
      if (token == CPP_VSPACE || token == CPP_EOF
	  /* XXX Temporary kluge - force flush after #include only */
	  || (token == CPP_DIRECTIVE
	      && CPP_BUFFER (pfile)->nominal_fname != print->last_fname))
	{
	  cpp_output_tokens (pfile, print);
	  if (token == CPP_EOF && CPP_BUFFER (pfile) == stop)
	    return;
	}
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

/* Token-buffer helper functions.  */

/* Expand a token list's string space. It is *vital* that
   list->tokens_used is correct, to get pointer fix-up right.  */
static void
expand_name_space (list, len)
     cpp_toklist *list;
     unsigned int len;
{
  const U_CHAR *old_namebuf;

  old_namebuf = list->namebuf;
  list->name_cap += len;
  list->namebuf = (unsigned char *) xrealloc (list->namebuf, list->name_cap);

  /* Fix up token text pointers.  */
  if (list->namebuf != old_namebuf)
    {
      unsigned int i;

      for (i = 0; i < list->tokens_used; i++)
	if (token_spellings[list->tokens[i].type].type > SPELL_NONE)
	  list->tokens[i].val.name.text += (list->namebuf - old_namebuf);
    }
}

/* Expand the number of tokens in a list.  */
void
_cpp_expand_token_space (list, count)
     cpp_toklist *list;
     unsigned int count;
{
  unsigned int n;

  list->tokens_cap += count;
  n = list->tokens_cap;
  if (list->flags & LIST_OFFSET)
    list->tokens--, n++;
  list->tokens = (cpp_token *)
    xrealloc (list->tokens, n * sizeof (cpp_token));
  if (list->flags & LIST_OFFSET)
    list->tokens++;		/* Skip the dummy.  */
}

/* Initialize a token list.  If flags is DUMMY_TOKEN, we allocate
   an extra token in front of the token list, as this allows the lexer
   to always peek at the previous token without worrying about
   underflowing the list, and some initial space.  Otherwise, no
   token- or name-space is allocated, and there is no dummy token.  */
void
_cpp_init_toklist (list, flags)
     cpp_toklist *list;
     int flags;
{
  /* We malloc zero bytes because we may want to realloc later, and
     some old implementations don't like realloc-ing a null pointer.  */
  if (flags == NO_DUMMY_TOKEN)
    {
      list->tokens_cap = 0;
      list->tokens = (cpp_token *) malloc (0);
      list->name_cap = 0;
      list->flags = 0;
    }
  else
    {
      /* Initialize token space.  Put a dummy token before the start
	 that will fail matches.  */
      list->tokens_cap = 256;	/* 4K's worth.  */
      list->tokens = (cpp_token *)
	xmalloc ((list->tokens_cap + 1) * sizeof (cpp_token));
      list->tokens[0].type = CPP_EOF;
      list->tokens++;

      /* Initialize name space.  */
      list->name_cap = 1024;
      list->flags = LIST_OFFSET;
    }

  /* Allocate name space.  */
  list->namebuf = (unsigned char *) xmalloc (list->name_cap);

  _cpp_clear_toklist (list);
}

/* Clear a token list.  */
void
_cpp_clear_toklist (list)
     cpp_toklist *list;
{
  list->tokens_used = 0;
  list->name_used = 0;
  list->dirno = -1;
  list->flags &= LIST_OFFSET;  /* clear all but that one */
}

/* Free a token list.  Does not free the list itself, which may be
   embedded in a larger structure.  */
void
_cpp_free_toklist (list)
     cpp_toklist *list;
{
  if (list->flags & LIST_OFFSET)
    free (list->tokens - 1);	/* Backup over dummy token.  */
  else
    free (list->tokens);
  free (list->namebuf);
}

/* Slice a token list: copy the sublist [START, FINISH) into COPY.
   COPY is assumed not to be initialized.  The comment space is not
   copied.  */
void
_cpp_slice_toklist (copy, start, finish)
     cpp_toklist *copy;
     const cpp_token *start, *finish;
{
  unsigned int i, n;
  size_t bytes;

  n = finish - start;
  copy->tokens_cap = n;
  copy->tokens = (cpp_token *) xmalloc (n * sizeof (cpp_token));
  memcpy (copy->tokens, start, n * sizeof (cpp_token));

  bytes = 0;
  for (i = 0; i < n; i++)
    if (token_spellings[start[i].type].type > SPELL_NONE)
      bytes += start[i].val.name.len;

  copy->namebuf = xmalloc (bytes);
  bytes = 0;
  for (i = 0; i < n; i++)
    if (token_spellings[start[i].type].type > SPELL_NONE)
      {
	memcpy (copy->namebuf + bytes,
		start[i].val.name.text, start[i].val.name.len);
	copy->tokens[i].val.name.text = copy->namebuf + bytes;
	bytes += start[i].val.name.len;
      }

  copy->tokens_cap = n;
  copy->tokens_used = n;
  copy->name_used = bytes;
  copy->name_cap = bytes;
  
  copy->flags = 0;
  copy->dirno = -1;
}

/* Shrink a token list down to the minimum size.  */
void
_cpp_squeeze_toklist (list)
     cpp_toklist *list;
{
  long delta;
  const U_CHAR *old_namebuf;

  if (list->flags & LIST_OFFSET)
    {
      list->tokens--;
      memmove (list->tokens, list->tokens + 1,
	       list->tokens_used * sizeof (cpp_token));
      list->tokens = xrealloc (list->tokens,
			       list->tokens_used * sizeof (cpp_token));
      list->flags &= ~LIST_OFFSET;
    }
  else
    list->tokens = xrealloc (list->tokens,
			     list->tokens_used * sizeof (cpp_token));
  list->tokens_cap = list->tokens_used;

  old_namebuf = list->namebuf;
  list->namebuf = xrealloc (list->namebuf, list->name_used);
  list->name_cap = list->name_used;

  /* Fix up token text pointers.  */
  delta = list->namebuf - old_namebuf;
  if (delta)
    {
      unsigned int i;

      for (i = 0; i < list->tokens_used; i++)
	if (token_spellings[list->tokens[i].type].type > SPELL_NONE)
	  list->tokens[i].val.name.text += delta;
    }
}

/* Compare two tokens.  */
int
_cpp_equiv_tokens (a, b)
     const cpp_token *a, *b;
{
  if (a->type != b->type
      || a->flags != b->flags
      || a->aux != b->aux)
    return 0;

  if (token_spellings[a->type].type > SPELL_NONE)
    {
      if (a->val.name.len != b->val.name.len
	  || ustrncmp(a->val.name.text,
		      b->val.name.text,
		      a->val.name.len))
	return 0;
    }
  return 1;
}

/* Compare two token lists.  */
int
_cpp_equiv_toklists (a, b)
     const cpp_toklist *a, *b;
{
  unsigned int i;

  if (a->tokens_used != b->tokens_used)
    return 0;

  for (i = 0; i < a->tokens_used; i++)
    if (! _cpp_equiv_tokens (&a->tokens[i], &b->tokens[i]))
      return 0;
  return 1;
}

/* Scan until we encounter a token of type STOP or a newline, and
   create a token list for it.  Does not macro-expand or execute
   directives.  The final token is not included in the list or
   consumed from the input.  Returns the type of the token stopped at. */

enum cpp_ttype
_cpp_scan_until (pfile, list, stop)
     cpp_reader *pfile;
     cpp_toklist *list;
     enum cpp_ttype stop;
{
  int i, col;
  long written, len;
  enum cpp_ttype type;
  int space_before;

  _cpp_clear_toklist (list);
  list->line = CPP_BUF_LINE (CPP_BUFFER (pfile));

  written = CPP_WRITTEN (pfile);
  i = 0;
  space_before = 0;
  for (;;)
    {
      col = CPP_BUFFER (pfile)->cur - CPP_BUFFER (pfile)->line_base;
      type = _cpp_lex_token (pfile);
      len = CPP_WRITTEN (pfile) - written;
      CPP_SET_WRITTEN (pfile, written);
      if (type == CPP_HSPACE)
	{
	  if (CPP_PEDANTIC (pfile))
	    pedantic_whitespace (pfile, pfile->token_buffer + written, len);
	  space_before = 1;
	  continue;
	}
      else if (type == CPP_COMMENT)
	/* Only happens when processing -traditional macro definitions.
	   Do not give this a token entry, but do not change space_before
	   either.  */
	continue;

      if (list->tokens_used >= list->tokens_cap)
	_cpp_expand_token_space (list, 256);
      if (list->name_used + len >= list->name_cap)
	expand_name_space (list, list->name_used + len + 1 - list->name_cap);

      if (type == CPP_MACRO)
	type = CPP_NAME;

      if (type == CPP_VSPACE || type == stop)
	break;

      list->tokens_used++;
      TOK_TYPE  (list, i) = type;
      TOK_COL   (list, i) = col;
      TOK_AUX   (list, i) = 0;
      TOK_FLAGS (list, i) = space_before ? PREV_WHITESPACE : 0;
      
      TOK_LEN (list, i) = len;
      if (token_spellings[type].type > SPELL_NONE)
	{
	  memcpy (list->namebuf + list->name_used, CPP_PWRITTEN (pfile), len);
	  TOK_NAME (list, i) = list->namebuf + list->name_used;
	  list->name_used += len;
	}
      else
	TOK_NAME (list, i) = token_spellings[type].spelling;
      i++;
      space_before = 0;
    }

  /* XXX Temporary kluge: put back the newline (or whatever).  */
  FORWARD(-1);

  /* Don't consider the first token to have white before.  */
  TOK_FLAGS (list, 0) &= ~PREV_WHITESPACE;
  return type;
}

/* Skip a C-style block comment.  We know it's a comment, and point is
   at the second character of the starter.  */
static void
skip_block_comment (pfile)
     cpp_reader *pfile;
{
  unsigned int line, col;
  const U_CHAR *limit, *cur;

  FORWARD(1);
  line = CPP_BUF_LINE (CPP_BUFFER (pfile));
  col = CPP_BUF_COL (CPP_BUFFER (pfile));
  limit = CPP_BUFFER (pfile)->rlimit;
  cur = CPP_BUFFER (pfile)->cur;

  while (cur < limit)
    {
      char c = *cur++;
      if (c == '\n' || c == '\r')
	{
	  /* \r cannot be a macro escape marker here. */
	  if (!ACTIVE_MARK_P (pfile))
	    CPP_BUMP_LINE_CUR (pfile, cur);
	}
      else if (c == '*')
	{
	  /* Check for teminator.  */
	  if (cur < limit && *cur == '/')
	    goto out;

	  /* Warn about comment starter embedded in comment.  */
	  if (cur[-2] == '/' && CPP_OPTION (pfile, warn_comments))
	    cpp_warning_with_line (pfile, CPP_BUFFER (pfile)->lineno,
				   cur - CPP_BUFFER (pfile)->line_base,
				   "'/*' within comment");
	}
    }

  cpp_error_with_line (pfile, line, col, "unterminated comment");
  cur--;
 out:
  CPP_BUFFER (pfile)->cur = cur + 1;
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
	  if (! CPP_BUFFER (pfile)->warned_cplusplus_comments)
	    {
	      if (CPP_WTRADITIONAL (pfile))
		cpp_pedwarn (pfile,
			"C++ style comments are not allowed in traditional C");
	      else if (CPP_OPTION (pfile, c89) && CPP_PEDANTIC (pfile))
		cpp_pedwarn (pfile,
			"C++ style comments are not allowed in ISO C89");
	      if (CPP_WTRADITIONAL (pfile)
		  || (CPP_OPTION (pfile, c89) && CPP_PEDANTIC (pfile)))
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

static void
null_warning (pfile, count)
     cpp_reader *pfile;
     unsigned int count;
{
  if (count == 1)
    cpp_warning (pfile, "embedded null character ignored");
  else
    cpp_warning (pfile, "embedded null characters ignored");
}

/* Skip whitespace \-newline and comments.  Does not macro-expand.  */

void
_cpp_skip_hspace (pfile)
     cpp_reader *pfile;
{
  unsigned int null_count = 0;
  int c;

  while (1)
    {
      c = GETC();
      if (c == EOF)
	goto out;
      else if (is_hspace(c))
	{
	  if ((c == '\f' || c == '\v') && CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "%s in preprocessing directive",
			 c == '\f' ? "formfeed" : "vertical tab");
	  else if (c == '\0')
	    null_count++;
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
 out:
  if (null_count)
    null_warning (pfile, null_count);
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
  unsigned int start_line, start_column;
  unsigned int null_count = 0;

  start_line = CPP_BUF_LINE (CPP_BUFFER (pfile));
  start_column = CPP_BUF_COL (CPP_BUFFER (pfile));
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
	  goto out;

	case '\0':
	  null_count++;
	  break;
	  
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
	      goto out;
	    }
	  /* Character constants may not extend over multiple lines.
	     In Standard C, neither may strings.  We accept multiline
	     strings as an extension.  */
	  if (c == '\'')
	    {
	      cpp_error_with_line (pfile, start_line, start_column,
				   "unterminated character constant");
	      FORWARD(-1);
	      goto out;
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
	    goto out;
	  break;
	}
    }

 out:
  if (null_count == 1)
    cpp_warning (pfile, "null character in string or character constant");
  else if (null_count > 1)
    cpp_warning (pfile, "null characters in string or character constant");
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

/* Get the next token, and add it to the text in pfile->token_buffer.
   Return the kind of token we got.  */

enum cpp_ttype
_cpp_lex_token (pfile)
     cpp_reader *pfile;
{
  register int c, c2;
  enum cpp_ttype token;

  if (CPP_BUFFER (pfile) == NULL)
    return CPP_EOF;

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
      if (!CPP_OPTION (pfile, discard_comments))
	return CPP_COMMENT;
      else if (CPP_TRADITIONAL (pfile))
	goto get_next;
      else
	{
	  CPP_PUTC (pfile, c);
	  return CPP_HSPACE;
	}

    case '#':
      CPP_PUTC (pfile, c);

    hash:
      c2 = PEEKC ();
      if (c2 == '#')
	{
	  FORWARD (1);
	  CPP_PUTC (pfile, c2);
	  return CPP_PASTE;
	}
      else if (c2 == '%' && PEEKN (1) == ':')
	{
	  /* Digraph: "%:" == "#".  */
	  FORWARD (1);
	  CPP_RESERVE (pfile, 2);
	  CPP_PUTC_Q (pfile, c2);
	  CPP_PUTC_Q (pfile, GETC ());
	  return CPP_PASTE;
	}
      else
	return CPP_HASH;

    case '\"':
    case '\'':
      parse_string (pfile, c);
      return c == '\'' ? CPP_CHAR : CPP_STRING;

    case '$':
      if (!CPP_OPTION (pfile, dollars_in_ident))
	goto randomchar;
      goto letter;

    case ':':
      c2 = PEEKC ();
      /* Digraph: ":>" == "]".  */
      if (c2 == '>'
	  || (c2 == ':' && CPP_OPTION (pfile, cplusplus)))
	goto op2;
      goto randomchar;

    case '&':
    case '+':
    case '|':
      c2 = PEEKC ();
      if (c2 == c || c2 == '=')
	goto op2;
      goto randomchar;

    case '%':
      /* Digraphs: "%:" == "#", "%>" == "}".  */
      c2 = PEEKC ();
      if (c2 == ':')
	{
	  FORWARD (1);
	  CPP_RESERVE (pfile, 2);
	  CPP_PUTC_Q (pfile, c);
	  CPP_PUTC_Q (pfile, c2);
	  goto hash;
	}
      else if (c2 == '>')
	{
	  FORWARD (1);
	  CPP_RESERVE (pfile, 2);
	  CPP_PUTC_Q (pfile, c);
	  CPP_PUTC_Q (pfile, c2);
	  return CPP_OPEN_BRACE;
	}
      /* else fall through */

    case '*':
    case '!':
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
	      CPP_RESERVE (pfile, 4);
	      CPP_PUTC_Q (pfile, c);
	      CPP_PUTC_Q (pfile, GETC ());
	      CPP_PUTC_Q (pfile, GETC ());
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
      /* Digraphs: "<%" == "{", "<:" == "[".  */
      c2 = PEEKC ();
      if (c2 == '%')
	{
	  FORWARD (1);
	  CPP_RESERVE (pfile, 2);
	  CPP_PUTC_Q (pfile, c);
	  CPP_PUTC_Q (pfile, c2);
	  return CPP_CLOSE_BRACE;
	}
      else if (c2 == ':')
	goto op2;
      /* else fall through */
    case '>':
      c2 = PEEKC ();
      if (c2 == '=')
	goto op2;
      /* GNU C++ supports MIN and MAX operators <? and >?.  */
      if (c2 != c && (!CPP_OPTION (pfile, cplusplus) || c2 != '?'))
	goto randomchar;
      FORWARD(1);
      CPP_RESERVE (pfile, 3);
      CPP_PUTC_Q (pfile, c);
      CPP_PUTC_Q (pfile, c2);
      if (PEEKC () == '=')
	CPP_PUTC_Q (pfile, GETC ());
      return CPP_OTHER;

    case '.':
      c2 = PEEKC ();
      if (ISDIGIT (c2))
	{
	  CPP_PUTC (pfile, c);
	  c = GETC ();
	  goto number;
	}

      /* In C++ there's a .* operator.  */
      if (CPP_OPTION (pfile, cplusplus) && c2 == '*')
	goto op2;

      if (c2 == '.' && PEEKN(1) == '.')
	{
	  CPP_RESERVE (pfile, 3);
	  CPP_PUTC_Q (pfile, '.');
	  CPP_PUTC_Q (pfile, '.');
	  CPP_PUTC_Q (pfile, '.');
	  FORWARD (2);
	  return CPP_ELLIPSIS;
	}
      goto randomchar;

    op2:
      CPP_RESERVE (pfile, 2);
      CPP_PUTC_Q (pfile, c);
      CPP_PUTC_Q (pfile, GETC ());
      return CPP_OTHER;

    case 'L':
      c2 = PEEKC ();
      if ((c2 == '\'' || c2 == '\"') && !CPP_TRADITIONAL (pfile))
	{
	  CPP_PUTC (pfile, c);
	  c = GETC ();
	  parse_string (pfile, c);
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
    return CPP_NUMBER;
    case 'b': case 'c': case 'd': case 'h': case 'o':
    case 'B': case 'C': case 'D': case 'H': case 'O':
      if (CPP_OPTION (pfile, chill) && PEEKC () == '\'')
	{
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
	      return CPP_STRING;
	    }
	  else
	    {
	      FORWARD(-1);
	    chill_number_eof:
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
    _cpp_parse_name (pfile, c);
    return CPP_MACRO;

    case ' ':  case '\t':  case '\v': case '\f': case '\0':
      {
	int null_count = 0;

	for (;;)
	  {
	    if (c == '\0')
	      null_count++;
	    else
	      CPP_PUTC (pfile, c);
	    c = PEEKC ();
	    if (c == EOF || !is_hspace(c))
	      break;
	    FORWARD(1);
	  }
	if (null_count)
	  null_warning (pfile, null_count);
	return CPP_HSPACE;
      }

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
	      /* "\r " means a space, but only if necessary to prevent
		 accidental token concatenation.  */
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
	  if (!ACTIVE_MARK_P (pfile))
	    CPP_BUMP_LINE (pfile);
	  goto get_next;
	}

    case '\n':
      CPP_PUTC (pfile, c);
      return CPP_VSPACE;

    case '(': token = CPP_OPEN_PAREN;  goto char1;
    case ')': token = CPP_CLOSE_PAREN; goto char1;
    case '{': token = CPP_OPEN_BRACE;  goto char1;
    case '}': token = CPP_CLOSE_BRACE; goto char1;
    case ',': token = CPP_COMMA;       goto char1;
    case ';': token = CPP_SEMICOLON;   goto char1;

    randomchar:
    default:
      token = CPP_OTHER;
    char1:
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
  cpp_hashnode *hp = cpp_lookup (pfile, macro, len);

  /* cpp_lookup never returns null.  */
  if (hp->type == T_VOID)
    return 0;
  if (hp->disabled || hp->type == T_IDENTITY)
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
  if (hp->type == T_EMPTY)
    {
      /* Special case optimization: macro expands to nothing.  */
      CPP_SET_WRITTEN (pfile, written);
      CPP_PUTC_Q (pfile, ' ');
      return 1;
    }

  /* If macro wants an arglist, verify that a '(' follows.  */
  if (hp->type == T_FMACRO)
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

	  /* K+R treated this as a hard error.  */
	  if (CPP_OPTION (pfile, warn_traditional))
	    cpp_warning (pfile,
	 "traditional C rejects function macro %s in non-function context",
			 hp->name);
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

/* Complain about \v or \f in a preprocessing directive (constraint
   violation, C99 6.10 para 5).  Caller has checked CPP_PEDANTIC.  */
static void
pedantic_whitespace (pfile, p, len)
     cpp_reader *pfile;
     U_CHAR *p;
     unsigned int len;
{
  while (len)
    {
      if (*p == '\v')
	cpp_pedwarn (pfile, "vertical tab in preprocessing directive");
      else if (*p == '\f')
	cpp_pedwarn (pfile, "form feed in preprocessing directive");
      p++;
      len--;
    }
}


enum cpp_ttype
cpp_get_token (pfile)
     cpp_reader *pfile;
{
  enum cpp_ttype token;
  long written = CPP_WRITTEN (pfile);
  int macro_buffer;

 get_next:
  token = _cpp_lex_token (pfile);

  switch (token)
    {
    default:
      if (pfile->skipping)
	break;
      pfile->potential_control_macro = 0;
      pfile->only_seen_white = 0;
      break;

    case CPP_HSPACE:
    case CPP_COMMENT:
      break;

    case CPP_VSPACE:
      if (pfile->only_seen_white == 0)
	pfile->only_seen_white = 1;
      CPP_BUMP_LINE (pfile);
      break;

    case CPP_HASH:
      pfile->potential_control_macro = 0;
      if (!pfile->only_seen_white)
	break;
      /* XXX shouldn't have to do this - remove the hash or %: from
	 the token buffer.  */
      if (CPP_PWRITTEN (pfile)[-1] == '#')
	CPP_ADJUST_WRITTEN (pfile, -1);
      else
	CPP_ADJUST_WRITTEN (pfile, -2);

      if (_cpp_handle_directive (pfile))
	{
	  token = CPP_DIRECTIVE;
	  break;
	}
      pfile->only_seen_white = 0;
      CPP_PUTC (pfile, '#');
      break;

    case CPP_MACRO:
      if (pfile->skipping)
	break;
      pfile->potential_control_macro = 0;
      pfile->only_seen_white = 0;
      if (! pfile->no_macro_expand
	  && maybe_macroexpand (pfile, written))
	goto get_next;
      token = CPP_NAME;
      break;

      /* Do not run this case through the 'skipping' logic.  */
    case CPP_EOF:
      if (CPP_BUFFER (pfile) == NULL)
	return CPP_EOF;
      macro_buffer = CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile));

      cpp_pop_buffer (pfile);
      if (macro_buffer)
	goto get_next;
      return CPP_EOF;
    }
  
  if (pfile->skipping)
    {
      CPP_SET_WRITTEN (pfile, written);
      goto get_next;
    }
  return token;
}

/* Like cpp_get_token, but skip spaces and comments.  */

enum cpp_ttype
cpp_get_non_space_token (pfile)
     cpp_reader *pfile;
{
  int old_written = CPP_WRITTEN (pfile);
  for (;;)
    {
      enum cpp_ttype token = cpp_get_token (pfile);
      if (token != CPP_COMMENT && token != CPP_HSPACE && token != CPP_VSPACE)
	return token;
      CPP_SET_WRITTEN (pfile, old_written);
    }
}

/* Like cpp_get_token, except that it does not execute directives,
   does not consume vertical space, and discards horizontal space.  */
enum cpp_ttype
_cpp_get_directive_token (pfile)
     cpp_reader *pfile;
{
  long old_written;
  enum cpp_ttype token;
  int at_bol;

 get_next:
  at_bol = (CPP_BUFFER (pfile)->cur == CPP_BUFFER (pfile)->line_base);
  old_written = CPP_WRITTEN (pfile);
  token = _cpp_lex_token (pfile);
  switch (token)
    {
    default:
      return token;

    case CPP_VSPACE:
      /* Put it back and return VSPACE.  */
      FORWARD(-1);
      CPP_ADJUST_WRITTEN (pfile, -1);
      return CPP_VSPACE;

    case CPP_HSPACE:
      /* The purpose of this rather strange check is to prevent pedantic
	 warnings for ^L in an #ifdefed out block.  */
      if (CPP_PEDANTIC (pfile) && ! at_bol)
	pedantic_whitespace (pfile, pfile->token_buffer + old_written,
			     CPP_WRITTEN (pfile) - old_written);
      CPP_SET_WRITTEN (pfile, old_written);
      goto get_next;
      return CPP_HSPACE;

    case CPP_MACRO:
      if (! pfile->no_macro_expand
	  && maybe_macroexpand (pfile, old_written))
	goto get_next;
      return CPP_NAME;

    case CPP_EOF:
      if (CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
	{
	  cpp_pop_buffer (pfile);
	  goto get_next;
	}
      else
	/* This can happen for files that don't end with a newline,
	   and for cpp_define and friends.  Pretend they do, so
	   callers don't have to deal.  A warning will be issued by
	   someone else, if necessary.  */
	return CPP_VSPACE;
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

/* The following table is used by _cpp_prescan.  If we have
   designated initializers, it can be constant data; otherwise, it is
   set up at runtime by _cpp_init_input_buffer.  */

#if (GCC_VERSION >= 2007)
#define init_chartab()  /* nothing */
#define CHARTAB __extension__ static const U_CHAR chartab[UCHAR_MAX + 1] = {
#define END };
#define s(p, v) [p] = v,
#else
#define CHARTAB static U_CHAR chartab[UCHAR_MAX + 1] = { 0 }; \
 static void init_chartab PARAMS ((void)) { \
 unsigned char *x = chartab;
#define END }
#define s(p, v) x[p] = v;
#endif

/* Table of characters that can't be handled in the inner loop.
   Also contains the mapping between trigraph third characters and their
   replacements.  */
#define SPECCASE_CR        1
#define SPECCASE_BACKSLASH 2
#define SPECCASE_QUESTION  3
 
CHARTAB
  s('\r', SPECCASE_CR)
  s('\\', SPECCASE_BACKSLASH)
  s('?',  SPECCASE_QUESTION)

  s('=', '#')	s(')', ']')	s('!', '|')
  s('(', '[')	s('\'', '^')	s('>', '}')
  s('/', '\\')	s('<', '{')	s('-', '~')
END

#undef CHARTAB
#undef END
#undef s

#define NORMAL(c) ((chartab[c]) == 0 || (chartab[c]) > SPECCASE_QUESTION)
#define NONTRI(c) ((c) <= SPECCASE_QUESTION)

/* Prescan pass over a file already loaded into BUF.  This is
   translation phases 1 and 2 (C99 5.1.1.2).

   Convert end-of-line markers (\n, \r, \r\n, \n\r) to
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
   will get messed-up line numbering.  */

ssize_t
_cpp_prescan (pfile, fp, len)
     cpp_reader *pfile;
     cpp_buffer *fp;
     ssize_t len;
{
  U_CHAR *buf, *op;
  const U_CHAR *ibase, *ip, *ilimit;
  U_CHAR *line_base;
  unsigned long line;
  unsigned int deferred_newlines;

  /* Allocate an extra byte in case we must add a trailing \n.  */
  buf = (U_CHAR *) xmalloc (len + 1);
  line_base = op = buf;
  ip = ibase = fp->buf;
  ilimit = ibase + len;
  line = 1;
  deferred_newlines = 0;

  for (;;)
    {
      const U_CHAR *iq;

      /* Deal with \-newline, potentially in the middle of a token. */
      if (deferred_newlines)
	{
	  if (op != buf && ! is_space (op[-1]) && op[-1] != '\r')
	    {
	      /* Previous was not white space.  Skip to white
		 space, if we can, before outputting the \r's */
	      iq = ip;
	      while (iq < ilimit
		     && *iq != ' '
		     && *iq != '\t'
		     && *iq != '\n'
		     && NORMAL(*iq))
		iq++;
	      memcpy (op, ip, iq - ip);
	      op += iq - ip;
	      ip += iq - ip;
	      if (! NORMAL(*ip))
		goto do_speccase;
	    }
	  while (deferred_newlines)
	    deferred_newlines--, *op++ = '\r';
	}

      /* Copy as much as we can without special treatment. */
      iq = ip;
      while (iq < ilimit && NORMAL (*iq)) iq++;
      memcpy (op, ip, iq - ip);
      op += iq - ip;
      ip += iq - ip;

    do_speccase:
      if (ip >= ilimit)
	break;

      switch (chartab[*ip++])
	{
	case SPECCASE_CR:  /* \r */
	  if (ip[-2] != '\n')
	    {
	      if (ip < ilimit && *ip == '\n')
		ip++;
	      *op++ = '\n';
	    }
	  break;

	case SPECCASE_BACKSLASH:  /* \ */
	backslash:
	  if (ip < ilimit)
	    {
	      if (*ip == '\n')
		{
		  deferred_newlines++;
		  ip++;
		  if (*ip == '\r') ip++;
		  break;
		}
	      else if (*ip == '\r')
		{
		  deferred_newlines++;
		  ip++;
		  if (*ip == '\n') ip++;
		  break;
		}
	    }

	  *op++ = '\\';
	  break;

	case SPECCASE_QUESTION: /* ? */
	  {
	    unsigned int d, t;

	    *op++ = '?'; /* Normal non-trigraph case */
	    if (ip > ilimit - 2 || ip[0] != '?')
	      break;
		    
	    d = ip[1];
	    t = chartab[d];
	    if (NONTRI (t))
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
		    goto backslash;
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

#ifdef HAVE_MMAP_FILE
  if (fp->mapped)
    munmap ((caddr_t) fp->buf, len);
  else
#endif
    free ((PTR) fp->buf);

  if (op[-1] != '\n')
    {
      unsigned long col;
      line_base = find_position (line_base, op, &line);
      col = op - line_base + 1;
      cpp_warning_with_line (pfile, line, col, "no newline at end of file");
      *op++ = '\n';
    }

  fp->buf = buf;
  return op - buf;
}

/* Allocate pfile->input_buffer, and initialize chartab[]
   if it hasn't happened already.  */
 
void
_cpp_init_input_buffer (pfile)
     cpp_reader *pfile;
{
  U_CHAR *tmp;

  init_chartab ();
  _cpp_init_toklist (&pfile->directbuf, NO_DUMMY_TOKEN);

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

/* Utility routine:
   Compares, in the manner of strcmp(3), the token beginning at TOKEN
   and extending for LEN characters to the NUL-terminated string
   STRING.  Typical usage:

   if (! cpp_idcmp (pfile->token_buffer + here, CPP_WRITTEN (pfile) - here,
                 "inline"))
     { ... }
 */

int
cpp_idcmp (token, len, string)
     const U_CHAR *token;
     size_t len;
     const char *string;
{
  size_t len2 = strlen (string);
  int r;

  if ((r = memcmp (token, string, MIN (len, len2))))
    return r;

  /* The longer of the two strings sorts after the shorter.  */
  if (len == len2)
    return 0;
  else if (len < len2)
    return -1;
  else
    return 1;
}

#ifdef NEW_LEXER

/* Lexing algorithm.

 The original lexer in cpplib was made up of two passes: a first pass
 that replaced trigraphs and deleted esacped newlines, and a second
 pass that tokenized the result of the first pass.  Tokenisation was
 performed by peeking at the next character in the input stream.  For
 example, if the input stream contained "!=", the handler for the !
 character would peek at the next character, and if it were a '='
 would skip over it, and return a "!=" token, otherwise it would
 return just the "!" token.

 To implement a single-pass lexer, this peeking ahead is unworkable.
 An arbitrary number of escaped newlines, and trigraphs (in particular
 ??/ which translates to the escape \), could separate the '!' and '='
 in the input stream, yet the next token is still a "!=".

 Suppose instead that we lex by one logical line at a time, producing
 a token list or stack for each logical line, and when seeing the '!'
 push a CPP_NOT token on the list.  Then if the '!' is part of a
 longer token ("!=") we know we must see the remainder of the token by
 the time we reach the end of the logical line.  Thus we can have the
 '=' handler look at the previous token (at the end of the list / top
 of the stack) and see if it is a "!" token, and if so, instead of
 pushing a "=" token revise the existing token to be a "!=" token.

 This works in the presence of escaped newlines, because the '\' would
 have been pushed on the top of the stack as a CPP_BACKSLASH.  The
 newline ('\n' or '\r') handler looks at the token at the top of the
 stack to see if it is a CPP_BACKSLASH, and if so discards both.
 Otherwise it pushes the newline (CPP_VSPACE) token as normal.  Hence
 the '=' handler would never see any intervening escaped newlines.

 To make trigraphs work in this context, as in precedence trigraphs
 are highest and converted before anything else, the '?' handler does
 lookahead to see if it is a trigraph, and if so skips the trigraph
 and pushes the token it represents onto the top of the stack.  This
 also works in the particular case of a CPP_BACKSLASH trigraph.

 To the preprocessor, whitespace is only significant to the point of
 knowing whether whitespace precedes a particular token.  For example,
 the '=' handler needs to know whether there was whitespace between it
 and a "!" token on the top of the stack, to make the token conversion
 decision correctly.  So each token has a PREV_WHITESPACE flag to
 indicate this - the standard permits consecutive whitespace to be
 regarded as a single space.  The compiler front ends are not
 interested in whitespace at all; they just require a token stream.
 Another place where whitespace is significant to the preprocessor is
 a #define statment - if there is whitespace between the macro name
 and an initial "(" token the macro is "object-like", otherwise it is
 a function-like macro that takes arguments.

 However, all is not rosy.  Parsing of identifiers, numbers, comments
 and strings becomes trickier because of the possibility of raw
 trigraphs and escaped newlines in the input stream.

 The trigraphs are three consecutive characters beginning with two
 question marks.  A question mark is not valid as part of a number or
 identifier, so parsing of a number or identifier terminates normally
 upon reaching it, returning to the mainloop which handles the
 trigraph just like it would in any other position.  Similarly for the
 backslash of a backslash-newline combination.  So we just need the
 escaped-newline dropper in the mainloop to check if the token on the
 top of the stack after dropping the escaped newline is a number or
 identifier, and if so to continue the processing it as if nothing had
 happened.

 For strings, we replace trigraphs whenever we reach a quote or
 newline, because there might be a backslash trigraph escaping them.
 We need to be careful that we start trigraph replacing from where we
 left off previously, because it is possible for a first scan to leave
 "fake" trigraphs that a second scan would pick up as real (e.g. the
 sequence "????/\n=" would find a fake ??= trigraph after removing the
 escaped newline.)

 For line comments, on reaching a newline we scan the previous
 character(s) to see if it escaped, and continue if it is.  Block
 comments ignore everything and just focus on finding the comment
 termination mark.  The only difficult thing, and it is surprisingly
 tricky, is checking if an asterisk precedes the final slash since
 they could be separated by escaped newlines.  If the preprocessor is
 invoked with the output comments option, we don't bother removing
 escaped newlines and replacing trigraphs for output.

 Finally, numbers can begin with a period, which is pushed initially
 as a CPP_DOT token in its own right.  The digit handler checks if the
 previous token was a CPP_DOT not separated by whitespace, and if so
 pops it off the stack and pushes a period into the number's buffer
 before calling the number parser.

*/

static const unsigned char *digraph_spellings [] = {U"%:", U"%:%:", U"<:",
						    U":>", U"<%", U"%>"};
static unsigned char trigraph_map[256];

void
init_trigraph_map ()
{
  trigraph_map['='] = '#';
  trigraph_map['('] = '[';
  trigraph_map[')'] = ']';
  trigraph_map['/'] = '\\';
  trigraph_map['\''] = '^';
  trigraph_map['<'] = '{';
  trigraph_map['>'] = '}';
  trigraph_map['!'] = '|';
  trigraph_map['-'] = '~';
}

/* Call when a trigraph is encountered.  It warns if necessary, and
   returns true if the trigraph should be honoured.  END is the third
   character of a trigraph in the input stream.  */
static int
trigraph_ok (pfile, end)
     cpp_reader *pfile;
     const unsigned char *end;
{
  int accept = CPP_OPTION (pfile, trigraphs);
  
  if (CPP_OPTION (pfile, warn_trigraphs))
    {
      unsigned int col = end - 1 - pfile->buffer->line_base;
      if (accept)
	cpp_warning_with_line (pfile, pfile->buffer->lineno, col, 
			       "trigraph ??%c converted to %c",
			       (int) *end, (int) trigraph_map[*end]);
      else
	cpp_warning_with_line (pfile, pfile->buffer->lineno, col,
			       "trigraph ??%c ignored", (int) *end);
    }
  return accept;
}

/* Scan a string for trigraphs, warning or replacing them inline as
   appropriate.  When parsing a string, we must call this routine
   before processing a newline character (if trigraphs are enabled),
   since the newline might be escaped by a preceding backslash
   trigraph sequence.  Returns a pointer to the end of the name after
   replacement.  */

static unsigned char*
trigraph_replace (pfile, src, limit)
     cpp_reader *pfile;
     unsigned char *src;
     unsigned char* limit;
{
  unsigned char *dest;

  /* Starting with src[1], find two consecutive '?'.  The case of no
     trigraphs is streamlined.  */
  
  for (; src + 1 < limit; src += 2)
    {
      if (src[0] != '?')
	continue;

      /* Make src point to the 1st (NOT 2nd) of two consecutive '?'s.  */
      if (src[-1] == '?')
	src--;
      else if (src + 2 == limit || src[1] != '?')
	continue;

      /* Check if it really is a trigraph.  */
      if (trigraph_map[src[2]] == 0)
	continue;

      dest = src;
      goto trigraph_found;
    }
  return limit;

  /* Now we have a trigraph, we need to scan the remaining buffer, and
     copy-shifting its contents left if replacement is enabled.  */
  for (; src + 2 < limit; dest++, src++)
    if ((*dest = *src) == '?' && src[1] == '?' && trigraph_map[src[2]])
      {
      trigraph_found:
	src += 2;
	if (trigraph_ok (pfile, pfile->buffer->cur - (limit - src)))
	  *dest = trigraph_map[*src];
      }
  
  /* Copy remaining (at most 2) characters.  */
  while (src < limit)
    *dest++ = *src++;
  return dest;
}

/* If CUR is a backslash or the end of a trigraphed backslash, return
   a pointer to its beginning, otherwise NULL.  We don't read beyond
   the buffer start, because there is the start of the comment in the
   buffer.  */
static const unsigned char *
backslash_start (pfile, cur)
     cpp_reader *pfile;
     const unsigned char *cur;
{
  if (cur[0] == '\\')
    return cur;
  if (cur[0] == '/' && cur[-1] == '?' && cur[-2] == '?'
      && trigraph_ok (pfile, cur))
    return cur - 2;
  return 0;
}

/* Skip a C-style block comment.  This is probably the trickiest
   handler.  We find the end of the comment by seeing if an asterisk
   is before every '/' we encounter.  The nasty complication is that a
   previous asterisk may be separated by one or more escaped newlines.
   Returns non-zero if comment terminated by EOF, zero otherwise.  */
static int
skip_block_comment2 (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  const unsigned char *char_after_star = 0;
  register const unsigned char *cur = buffer->cur;
  int seen_eof = 0;
  
  /* Inner loop would think the comment has ended if the first comment
     character is a '/'.  Avoid this and keep the inner loop clean by
     skipping such a character.  */
  if (cur < buffer->rlimit && cur[0] == '/')
    cur++;

  for (; cur < buffer->rlimit; )
    {
      unsigned char c = *cur++;

      /* People like decorating comments with '*', so check for
	 '/' instead for efficiency.  */
      if (c == '/')
	{
	  if (cur[-2] == '*' || cur - 1 == char_after_star)
	    goto out;

	  /* Warn about potential nested comments, but not when
	     the final character inside the comment is a '/'.
	     Don't bother to get it right across escaped newlines.  */
	  if (CPP_OPTION (pfile, warn_comments) && cur + 1 < buffer->rlimit
	      && cur[0] == '*' && cur[1] != '/') 
	    {
	      buffer->cur = cur;
	      cpp_warning (pfile, "'/*' within comment");
	    }
	}
      else if (IS_NEWLINE(c))
	{
	  const unsigned char* bslash = backslash_start (pfile, cur - 2);

	  handle_newline (cur, buffer->rlimit, c);
	  /* Work correctly if there is an asterisk before an
	     arbirtrarily long sequence of escaped newlines.  */
	  if (bslash && (bslash[-1] == '*' || bslash == char_after_star))
	    char_after_star = cur;
	  else
	    char_after_star = 0;
	}
    }
  seen_eof = 1;

 out:
  buffer->cur = cur;
  return seen_eof;
}

/* Skip a C++ or Chill line comment.  Handles escaped newlines.
   Returns non-zero if a multiline comment.  */
static int
skip_line_comment2 (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  register const unsigned char *cur = buffer->cur;
  int multiline = 0;

  for (; cur < buffer->rlimit; )
    {
      unsigned char c = *cur++;

      if (IS_NEWLINE (c))
	{
	  /* Check for a (trigaph?) backslash escaping the newline.  */
	  if (!backslash_start (pfile, cur - 2))
	    goto out;
	  multiline = 1;
	  handle_newline (cur, buffer->rlimit, c);
	}
    }
  cur++;

 out:
  buffer->cur = cur - 1;	/* Leave newline for caller.  */
  return multiline;
}

/* Skips whitespace, stopping at next non-whitespace character.
   Adjusts pfile->col_adjust to account for tabs.  This enables tokens
   to be assigned the correct column.  */
static void
skip_whitespace (pfile, in_directive)
     cpp_reader *pfile;
     int in_directive;
{
  cpp_buffer *buffer = pfile->buffer;
  register const unsigned char *cur = buffer->cur;
  unsigned short null_count = 0;

  for (; cur < buffer->rlimit; )
    {
      unsigned char c = *cur++;

      if (c == '\t')
	{
	  unsigned int col = CPP_BUF_COLUMN (buffer, cur - 1);
	  pfile->col_adjust += (CPP_OPTION (pfile, tabstop) - 1
				- col % CPP_OPTION(pfile, tabstop));
	}
      if (IS_HSPACE(c))		/* FIXME: Fix ISTABLE.  */
	continue;
      if (!is_space(c) || IS_NEWLINE (c)) /* Main loop handles newlines.  */
	goto out;
      if (c == '\0')
	null_count++;
      /* Mut be '\f' or '\v' */
      else if (in_directive && CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "%s in preprocessing directive",
		     c == '\f' ? "formfeed" : "vertical tab");
    }
  cur++;

 out:
  buffer->cur = cur - 1;
  if (null_count)
    cpp_warning (pfile, null_count > 1 ? "embedded null characters ignored"
		 : "embedded null character ignored");
}

/* Parse (append) an identifier.  */
static void
parse_name (pfile, list, name)
     cpp_reader *pfile;
     cpp_toklist *list;
     cpp_name *name;
{
  const unsigned char *name_limit;
  unsigned char *namebuf;
  cpp_buffer *buffer = pfile->buffer;
  register const unsigned char *cur = buffer->cur;

 expanded:
  name_limit = list->namebuf + list->name_cap;
  namebuf = list->namebuf + list->name_used;

  for (; cur < buffer->rlimit && namebuf < name_limit; )
    {
      unsigned char c = *namebuf = *cur; /* Copy a single char.  */

      if (! is_idchar(c))
	goto out;
      namebuf++;
      cur++;
      if (c == '$' && CPP_PEDANTIC (pfile))
	{
	  buffer->cur = cur;
	  cpp_pedwarn (pfile, "'$' character in identifier");
	}
    }

  /* Run out of name space?  */
  if (cur < buffer->rlimit)
    {
      list->name_used = namebuf - list->namebuf;
      auto_expand_name_space (list);
      goto expanded;
    }

 out:
  buffer->cur = cur;
  name->len = namebuf - name->text;
  list->name_used = namebuf - list->namebuf;
}

/* Parse (append) a number.  */

#define VALID_SIGN(c, prevc) \
  (((c) == '+' || (c) == '-') && \
   ((prevc) == 'e' || (prevc) == 'E' \
    || (((prevc) == 'p' || (prevc) == 'P') && !CPP_OPTION (pfile, c89))))

static void
parse_number (pfile, list, name)
     cpp_reader *pfile;
     cpp_toklist *list;
     cpp_name *name;
{
  const unsigned char *name_limit;
  unsigned char *namebuf;
  cpp_buffer *buffer = pfile->buffer;
  register const unsigned char *cur = buffer->cur;

 expanded:
  name_limit = list->namebuf + list->name_cap;
  namebuf = list->namebuf + list->name_used;

  for (; cur < buffer->rlimit && namebuf < name_limit; )
    {
      unsigned char c = *namebuf = *cur; /* Copy a single char.  */

      /* Perhaps we should accept '$' here if we accept it for
         identifiers.  We know namebuf[-1] is safe, because for c to
         be a sign we must have pushed at least one character.  */
      if (!is_numchar (c) && c != '.' && ! VALID_SIGN (c, namebuf[-1]))
	goto out;

      namebuf++;
      cur++;
    }

  /* Run out of name space?  */
  if (cur < buffer->rlimit)
    {
      list->name_used = namebuf - list->namebuf;
      auto_expand_name_space (list);
      goto expanded;
    }
  
 out:
  buffer->cur = cur;
  name->len = namebuf - name->text;
  list->name_used = namebuf - list->namebuf;
}

/* Places a string terminated by an unescaped TERMINATOR into a
   cpp_name, which should be expandable and thus at the top of the
   list's stack.  Handles embedded trigraphs, if necessary, and
   escaped newlines.

   Can be used for character constants (terminator = '\''), string
   constants ('"') and angled headers ('>').  Multi-line strings are
   allowed, except for within directives.  */

static void
parse_string2 (pfile, list, name, terminator, multiline_ok)
     cpp_reader *pfile;
     cpp_toklist *list;
     cpp_name *name;
     unsigned int terminator;
     int multiline_ok;
{
  cpp_buffer *buffer = pfile->buffer;
  register const unsigned char *cur = buffer->cur;
  const unsigned char *name_limit;
  unsigned char *namebuf;
  unsigned int null_count = 0;
  int trigraphed_len = 0;

 expanded:
  name_limit = list->namebuf + list->name_cap;
  namebuf = list->namebuf + list->name_used;

  for (; cur < buffer->rlimit && namebuf < name_limit; )
    {
      unsigned int c = *namebuf++ = *cur++; /* Copy a single char.  */

      if (c == '\0')
	null_count++;
      else if (c == terminator || IS_NEWLINE (c))
	{
	  /* Needed for trigraph_replace and multiline string warning.  */
	  buffer->cur = cur;

	  /* Scan for trigraphs before checking if backslash-escaped.  */
	  if (CPP_OPTION (pfile, trigraphs)
	      || CPP_OPTION (pfile, warn_trigraphs))
	    {
	      namebuf = trigraph_replace (pfile, name->text + trigraphed_len,
					    namebuf);
	      trigraphed_len = namebuf - 2 - (name->text + trigraphed_len);
	      if (trigraphed_len < 0)
		trigraphed_len = 0;
	    }

	  namebuf--;     /* Drop the newline / terminator from the name.  */
	  if (IS_NEWLINE (c))
	    {
	      /* Drop a backslash newline, and continue. */
	      if (namebuf[-1] == '\\')
		{
		  handle_newline (cur, buffer->rlimit, c);
		  namebuf--;
		  continue;
		}

	      cur--;

	      /* In Fortran and assembly language, silently terminate
		 strings of either variety at end of line.  This is a
		 kludge around not knowing where comments are in these
		 languages.  */
	      if (CPP_OPTION (pfile, lang_fortran)
		  || CPP_OPTION (pfile, lang_asm))
		goto out;

	      /* Character constants, headers and asserts may not
		 extend over multiple lines.  In Standard C, neither
		 may strings.  We accept multiline strings as an
		 extension, but not in directives.  */
	      if (!multiline_ok)
		goto unterminated;
		
	      cur++;  /* Move forwards again.  */

	      if (pfile->multiline_string_line == 0)
		{
		  pfile->multiline_string_line = list->line;
		  if (CPP_PEDANTIC (pfile))
		    cpp_pedwarn (pfile, "multi-line string constant");
		}

	      *namebuf++ = '\n';
	      handle_newline (cur, buffer->rlimit, c);
	    }
	  else
	    {
	      unsigned char *temp;

	      /* An odd number of consecutive backslashes represents
		 an escaped terminator.  */
	      temp = namebuf - 1;
	      while (temp >= name->text && *temp == '\\')
		temp--;

	      if ((namebuf - temp) & 1)
		goto out;
	      namebuf++;
	    }
	}
    }

  /* Run out of name space?  */
  if (cur < buffer->rlimit)
    {
      list->name_used = namebuf - list->namebuf;
      auto_expand_name_space (list);
      goto expanded;
    }

  /* We may not have trigraph-replaced the input for this code path,
     but as the input is in error by being unterminated we don't
     bother.  Prevent warnings about no newlines at EOF.  */
  if (IS_NEWLINE(cur[-1]))
    cur--;

 unterminated:
  cpp_error (pfile, "missing terminating %c character", (int) terminator);

  if (terminator == '\"' && pfile->multiline_string_line != list->line
      && pfile->multiline_string_line != 0)
    {
      cpp_error_with_line (pfile, pfile->multiline_string_line, -1,
			   "possible start of unterminated string literal");
      pfile->multiline_string_line = 0;
    }
  
 out:
  buffer->cur = cur;
  name->len = namebuf - name->text;
  list->name_used = namebuf - list->namebuf;

  if (null_count > 0)
    cpp_warning (pfile, (null_count > 1 ? "null characters preserved"
			 : "null character preserved"));
}

/* The character TYPE helps us distinguish comment types: '*' = C
   style, '-' = Chill-style and '/' = C++ style.  For code simplicity,
   the stored comment includes the comment start and any terminator.  */

#define COMMENT_START_LEN 2
static void
save_comment (list, token, from, len, type)
     cpp_toklist *list;
     cpp_token *token;
     const unsigned char *from;
     unsigned int len;
     unsigned int type;
{
  unsigned char *buffer;
  
  len += COMMENT_START_LEN;

  if (list->name_used + len > list->name_cap)
    expand_name_space (list, len);

  INIT_TOKEN_NAME (list, token);
  token->type = CPP_COMMENT;
  token->val.name.len = len;

  buffer = list->namebuf + list->name_used;
  list->name_used += len;

  /* Copy the comment.  */
  if (type == '*')
    {
      *buffer++ = '/';
      *buffer++ = '*';
    }
  else
    {
      *buffer++ = type;
      *buffer++ = type;
    }
  memcpy (buffer, from, len - COMMENT_START_LEN);
}

/*
 *  The tokenizer's main loop.  Returns a token list, representing a
 *  logical line in the input file.  On EOF after some tokens have
 *  been processed, we return immediately.  Then in next call, or if
 *  EOF occurred at the beginning of a logical line, a single CPP_EOF
 *  token is placed in the list.
 *
 *  Implementation relies almost entirely on lookback, rather than
 *  looking forwards.  This means that tokenization requires just
 *  a single pass of the file, even in the presence of trigraphs and
 *  escaped newlines, providing significant performance benefits.
 *  Trigraph overhead is negligible if they are disabled, and low
 *  even when enabled.
 */

#define IS_DIRECTIVE() (list->tokens[first_token].type == CPP_HASH)

void
_cpp_lex_line (pfile, list)
     cpp_reader *pfile;
     cpp_toklist *list;
{
  cpp_token *cur_token, *token_limit;
  cpp_buffer *buffer = pfile->buffer;
  register const unsigned char *cur = buffer->cur;
  unsigned char flags = 0;
  unsigned int first_token = list->tokens_used;

  list->line = CPP_BUF_LINE (buffer);
  pfile->col_adjust = 0;
 expanded:
  token_limit = list->tokens + list->tokens_cap;
  cur_token = list->tokens + list->tokens_used;

  for (; cur < buffer->rlimit && cur_token < token_limit;)
    {
      unsigned char c = *cur++;

      /* Optimize whitespace skipping, as most tokens are probably
	 separated by whitespace. (' ' '\t' '\v' '\f' '\0').  */

      if (is_hspace ((unsigned int) c))
	{
	  /* Step back to get the null warning and tab correction.  */
	  buffer->cur = cur - 1;
	  skip_whitespace (pfile, IS_DIRECTIVE ());
	  cur = buffer->cur;

	  flags = PREV_WHITESPACE;
	  if (cur == buffer->rlimit)
	    break;
	  c = *cur++;
	}

      /* Initialize current token.  Its type is set in the switch.  */
      cur_token->col = CPP_BUF_COLUMN (buffer, cur);
      cur_token->flags = flags;
      flags = 0;

      switch (c)
	{
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  {
	    int prev_dot;

	    cur--;		/* Backup character.  */
	    prev_dot = PREV_TOKEN_TYPE == CPP_DOT && IMMED_TOKEN ();
	    if (prev_dot)
	      cur_token--;
	    INIT_TOKEN_NAME (list, cur_token);
	    /* Prepend an immediately previous CPP_DOT token.  */
	    if (prev_dot)
	      {
		if (list->name_cap == list->name_used)
		  auto_expand_name_space (list);

		cur_token->val.name.len = 1;
		list->namebuf[list->name_used++] = '.';
	      }

	  continue_number:
	    cur_token->type = CPP_NUMBER; /* Before parse_number.  */
	    buffer->cur = cur;
	    parse_number (pfile, list, &cur_token->val.name);
	    cur = buffer->cur;
	    cur_token++;
	  }
	  break;

	letter:
	case '_':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
	case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
	case 's': case 't': case 'u': case 'v': case 'w': case 'x':
	case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
	case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
	case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
	case 'Y': case 'Z':
	  cur--;		     /* Backup character.  */
	  INIT_TOKEN_NAME (list, cur_token);
	  cur_token->type = CPP_NAME; /* Identifier, macro etc.  */

	continue_name:
	  buffer->cur = cur;
	  parse_name (pfile, list, &cur_token->val.name);
	  cur = buffer->cur;

	  /* Find handler for newly created / extended directive.  */
	  if (IS_DIRECTIVE () && cur_token == &list->tokens[first_token + 1])
	    _cpp_check_directive (list, cur_token);
	  cur_token++;
	  break;

	case '\'':
	  /* Fall through.  */
	case '\"':
	  cur_token->type = c == '\'' ? CPP_CHAR : CPP_STRING;
	  /* Do we have a wide string?  */
	  if (cur_token[-1].type == CPP_NAME && IMMED_TOKEN ()
	      && cur_token[-1].val.name.len == 1
	      && cur_token[-1].val.name.text[0] == 'L'
	      && !CPP_TRADITIONAL (pfile))
	    {
	      /* No need for 'L' any more.  */
	      list->name_used--;
	      (--cur_token)->type = (c == '\'' ? CPP_WCHAR : CPP_WSTRING);
	    }

	do_parse_string:
	  /* Here c is one of ' " or >.  */
	  INIT_TOKEN_NAME (list, cur_token);
	  buffer->cur = cur;
	  parse_string2 (pfile, list, &cur_token->val.name, c,
			 c == '"' && !IS_DIRECTIVE());
	  cur = buffer->cur;
	  cur_token++;
	  break;

	case '/':
	  cur_token->type = CPP_DIV;
	  if (IMMED_TOKEN ())
	    {
	      if (PREV_TOKEN_TYPE == CPP_DIV)
		{
		  /* We silently allow C++ comments in system headers,
		     irrespective of conformance mode, because lots of
		     broken systems do that and trying to clean it up
		     in fixincludes is a nightmare.  */
		  if (buffer->system_header_p)
		    goto do_line_comment;
		  else if (CPP_OPTION (pfile, cplusplus_comments))
		    {
		      if (CPP_OPTION (pfile, c89) && CPP_PEDANTIC (pfile)
			  && ! buffer->warned_cplusplus_comments)
			{
			  buffer->cur = cur;
			  cpp_pedwarn (pfile,
			     "C++ style comments are not allowed in ISO C89");
			  cpp_pedwarn (pfile,
			  "(this will be reported only once per input file)");
			  buffer->warned_cplusplus_comments = 1;
			}
		    do_line_comment:
		      buffer->cur = cur;
		      if (cur[-2] != c)
			cpp_warning (pfile,
				     "comment start split across lines");
		      if (skip_line_comment2 (pfile))
			cpp_error_with_line (pfile, list->line,
					     cur_token[-1].col,
					     "multi-line comment");

		      /* Back-up to first '-' or '/'.  */
		      cur_token--;
		      if (!CPP_OPTION (pfile, discard_comments)
			  && (!IS_DIRECTIVE() || list->dirno == 0))
			save_comment (list, cur_token++, cur,
				      buffer->cur - cur, c);
		      cur = buffer->cur;

		      if (!CPP_OPTION (pfile, traditional))
			flags = PREV_WHITESPACE;
		      break;
		    }
		}
	    }
	  cur_token++;
	  break;
		      
	case '*':
	  cur_token->type = CPP_MULT;
	  if (IMMED_TOKEN ())
	    {
	      if (PREV_TOKEN_TYPE == CPP_DIV)
		{
		  buffer->cur = cur;
		  if (cur[-2] != '/')
		    cpp_warning (pfile,
				 "comment start '/*' split across lines");
		  if (skip_block_comment2 (pfile))
		    cpp_error_with_line (pfile, list->line, cur_token[-1].col,
					 "unterminated comment");
		  else if (buffer->cur[-2] != '*')
		    cpp_warning (pfile,
				 "comment end '*/' split across lines");

		  /* Back up to opening '/'.  */
		  cur_token--;
		  if (!CPP_OPTION (pfile, discard_comments)
		      && (!IS_DIRECTIVE() || list->dirno == 0))
		    save_comment (list, cur_token++, cur,
				  buffer->cur - cur, c);
		  cur = buffer->cur;

		  if (!CPP_OPTION (pfile, traditional))
		    flags = PREV_WHITESPACE;
		  break;
		}
	      else if (CPP_OPTION (pfile, cplusplus))
		{
		  /* In C++, there are .* and ->* operators.  */
		  if (PREV_TOKEN_TYPE == CPP_DEREF)
		    BACKUP_TOKEN (CPP_DEREF_STAR);
		  else if (PREV_TOKEN_TYPE == CPP_DOT)
		    BACKUP_TOKEN (CPP_DOT_STAR);
		}
	    }
	  cur_token++;
	  break;

	case '\n':
	case '\r':
	  handle_newline (cur, buffer->rlimit, c);
	  if (PREV_TOKEN_TYPE == CPP_BACKSLASH && IMMED_TOKEN ())
	    {
	      /* Remove the escaped newline.  Then continue to process
		 any interrupted name or number.  */
	      cur_token--;
	      if (IMMED_TOKEN ())
		{
		  cur_token--;
		  if (cur_token->type == CPP_NAME)
		    goto continue_name;
		  else if (cur_token->type == CPP_NUMBER)
		    goto continue_number;
		  cur_token++;
		}
	      /* Remember whitespace setting.  */
	      flags = cur_token->flags;
	      break;
	    }
	  if (PREV_TOKEN_TYPE == CPP_BACKSLASH)
	    {
	      buffer->cur = cur;
	      cpp_warning (pfile, "backslash and newline separated by space");
	    }
	  /* Skip vertical space until we have at least one token to
             return.  */
	  if (cur_token != &list->tokens[first_token])
	    goto out;
	  list->line = CPP_BUF_LINE (buffer);
	  break;

	case '-':
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE == CPP_MINUS)
	    {
	      if (CPP_OPTION (pfile, chill))
		goto do_line_comment;
	      REVISE_TOKEN (CPP_MINUS_MINUS);
	    }
	  else
	    PUSH_TOKEN (CPP_MINUS);
	  break;

	  /* The digraph flag checking ensures that ## and %:%:
	     are interpreted as CPP_PASTE, but #%: and %:# are not.  */
	make_hash:
	case '#':
	  if (PREV_TOKEN_TYPE == CPP_HASH && IMMED_TOKEN ()
	      && ((cur_token->flags ^ cur_token[-1].flags) & DIGRAPH) == 0)
	    REVISE_TOKEN (CPP_PASTE);
	  else
	    PUSH_TOKEN (CPP_HASH);
	  break;

	case ':':
	  cur_token->type = CPP_COLON;
	  if (IMMED_TOKEN ())
	    {
	      if (PREV_TOKEN_TYPE == CPP_COLON
		  && CPP_OPTION (pfile, cplusplus))
		BACKUP_TOKEN (CPP_SCOPE);
	      /* Digraph: "<:" is a '['  */
	      else if (PREV_TOKEN_TYPE == CPP_LESS)
		BACKUP_DIGRAPH (CPP_OPEN_SQUARE);
	      /* Digraph: "%:" is a '#'  */
	      else if (PREV_TOKEN_TYPE == CPP_MOD)
		{
		  (--cur_token)->flags |= DIGRAPH;
		  goto make_hash;
		}
	    }
	  cur_token++;
	  break;

	case '&':
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE == CPP_AND)
	    REVISE_TOKEN (CPP_AND_AND);
	  else
	    PUSH_TOKEN (CPP_AND);
	  break;

	make_or:
	case '|':
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE == CPP_OR)
	    REVISE_TOKEN (CPP_OR_OR);
	  else
	    PUSH_TOKEN (CPP_OR);
	  break;

	case '+':
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE == CPP_PLUS)
	    REVISE_TOKEN (CPP_PLUS_PLUS);
	  else
	    PUSH_TOKEN (CPP_PLUS);
	  break;

	case '=':
	    /* This relies on equidistance of "?=" and "?" tokens.  */
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE <= CPP_LAST_EQ)
	    REVISE_TOKEN (PREV_TOKEN_TYPE + (CPP_EQ_EQ - CPP_EQ));
	  else
	    PUSH_TOKEN (CPP_EQ);
	  break;

	case '>':
	  cur_token->type = CPP_GREATER;
	  if (IMMED_TOKEN ())
	    {
	      if (PREV_TOKEN_TYPE == CPP_GREATER)
		BACKUP_TOKEN (CPP_RSHIFT);
	      else if (PREV_TOKEN_TYPE == CPP_MINUS)
		BACKUP_TOKEN (CPP_DEREF);
	      /* Digraph: ":>" is a ']'  */
	      else if (PREV_TOKEN_TYPE == CPP_COLON)
		BACKUP_DIGRAPH (CPP_CLOSE_SQUARE);
	      /* Digraph: "%>" is a '}'  */
	      else if (PREV_TOKEN_TYPE == CPP_MOD)
		BACKUP_DIGRAPH (CPP_CLOSE_BRACE);
	    }
	  cur_token++;
	  break;
	  
	case '<':
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE == CPP_LESS)
	    {
	      REVISE_TOKEN (CPP_LSHIFT);
	      break;
	    }
	  /* Is this the beginning of a header name?  */
	  if (list->flags & SYNTAX_INCLUDE)
	    {
	      c = '>';	/* Terminator.  */
	      cur_token->type = CPP_HEADER_NAME;
	      goto do_parse_string;
	    }
	  PUSH_TOKEN (CPP_LESS);
	  break;

	case '%':
	  /* Digraph: "<%" is a '{'  */
	  cur_token->type = CPP_MOD;
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE == CPP_LESS)
	    BACKUP_DIGRAPH (CPP_OPEN_BRACE);
	  cur_token++;
	  break;

	case '?':
	  if (cur + 1 < buffer->rlimit && *cur == '?'
	      && trigraph_map[cur[1]] && trigraph_ok (pfile, cur + 1))
	    {
	      /* Handle trigraph.  */
	      cur++;
	      switch (*cur++)
		{
		case '(': goto make_open_square;
		case ')': goto make_close_square;
		case '<': goto make_open_brace;
		case '>': goto make_close_brace;
		case '=': goto make_hash;
		case '!': goto make_or;
		case '-': goto make_complement;
		case '/': goto make_backslash;
		case '\'': goto make_xor;
		}
	    }
	  if (IMMED_TOKEN () && CPP_OPTION (pfile, cplusplus))
	    {
	      /* GNU C++ defines <? and >? operators.  */
	      if (PREV_TOKEN_TYPE == CPP_LESS)
		{
		  REVISE_TOKEN (CPP_MIN);
		  break;
		}
	      else if (PREV_TOKEN_TYPE == CPP_GREATER)
		{
		  REVISE_TOKEN (CPP_MAX);
		  break;
		}
	    }
	  PUSH_TOKEN (CPP_QUERY);
	  break;

	case '.':
	  if (PREV_TOKEN_TYPE == CPP_DOT && cur_token[-2].type == CPP_DOT
	      && IMMED_TOKEN ()
	      && !(cur_token[-1].flags & PREV_WHITESPACE))
	    {
	      cur_token -= 2;
	      PUSH_TOKEN (CPP_ELLIPSIS);
	    }
	  else
	    PUSH_TOKEN (CPP_DOT);
	  break;

	make_complement:
	case '~': PUSH_TOKEN (CPP_COMPL); break;
	make_xor:
	case '^': PUSH_TOKEN (CPP_XOR); break;
	make_open_brace:
	case '{': PUSH_TOKEN (CPP_OPEN_BRACE); break;
	make_close_brace:
	case '}': PUSH_TOKEN (CPP_CLOSE_BRACE); break;
	make_open_square:
	case '[': PUSH_TOKEN (CPP_OPEN_SQUARE); break;
	make_close_square:
	case ']': PUSH_TOKEN (CPP_CLOSE_SQUARE); break;
	make_backslash:
	case '\\': PUSH_TOKEN (CPP_BACKSLASH); break;
	case '!': PUSH_TOKEN (CPP_NOT); break;
	case ',': PUSH_TOKEN (CPP_COMMA); break;
	case ';': PUSH_TOKEN (CPP_SEMICOLON); break;
	case '(': PUSH_TOKEN (CPP_OPEN_PAREN); break;
	case ')': PUSH_TOKEN (CPP_CLOSE_PAREN); break;

	case '$':
	  if (CPP_OPTION (pfile, dollars_in_ident))
	    goto letter;
	  /* Fall through */
	default:
	  cur_token->aux = c;
	  cur_token->val.name.len = 0; /* FIXME: needed for transition only */
	  PUSH_TOKEN (CPP_OTHER);
	  break;
	}
    }

  /* Run out of token space?  */
  if (cur_token == token_limit)
    {
      list->tokens_used = cur_token - list->tokens;
      _cpp_expand_token_space (list, 256);
      goto expanded;
    }

  cur_token->flags = flags;
  if (cur_token == &list->tokens[first_token])
    {
      /* FIXME: move this warning to callers who care.  */
      if (cur > buffer->buf && !IS_NEWLINE (cur[-1]))
	cpp_warning (pfile, "no newline at end of file");
      cur_token++->type = CPP_EOF;
    }

 out:
  list->tokens[first_token].flags |= BOL;
  buffer->cur = cur;
  list->tokens_used = cur_token - list->tokens;
}

/* Write the spelling of a token TOKEN to BUFFER.  The buffer must
   already contain the enough space to hold the token's spelling.  If
   WHITESPACE is true, and the token was preceded by whitespace,
   output a single space before the token proper.  Returns a pointer
   to the character after the last character written.  */

static unsigned char *
spell_token (pfile, token, buffer, whitespace)
     cpp_reader *pfile;		/* Would be nice to be rid of this...  */
     const cpp_token *token;
     unsigned char *buffer;
     int whitespace;
{
  /* Whitespace will not be wanted by handlers of the # and ##
     operators calling this function, but will be wanted by the
     function that writes out the preprocessed file.  */
  if (whitespace && token->flags & PREV_WHITESPACE)
    *buffer++ = ' ';

  switch (token_spellings[token->type].type)
    {
    case SPELL_OPERATOR:
      {
	const unsigned char *spelling;
	unsigned char c;

	if (token->flags & DIGRAPH)
	  spelling = digraph_spellings[token->type - CPP_FIRST_DIGRAPH];
	else
	  spelling = token_spellings[token->type].spelling;
	
	while ((c = *spelling++) != '\0')
	  *buffer++ = c;
      }
      break;

    case SPELL_IDENT:
      memcpy (buffer, token->val.name.text, token->val.name.len);
      buffer += token->val.name.len;
      break;

    case SPELL_STRING:
      {
	unsigned char c;

	if (token->type == CPP_WSTRING || token->type == CPP_WCHAR)
	  *buffer++ = 'L';
	c = '\'';
	if (token->type == CPP_STRING || token->type == CPP_WSTRING)
	  c = '"';
	*buffer++ = c;
	memcpy (buffer, token->val.name.text, token->val.name.len);
	buffer += token->val.name.len;
	*buffer++ = c;
      }
      break;

    case SPELL_CHAR:
      *buffer++ = token->aux;
      break;

    case SPELL_NONE:
      cpp_ice (pfile, "Unspellable token");
      break;
    }

  return buffer;
}

/* Temporary function for illustrative purposes.  */
void
_cpp_lex_file (pfile)
     cpp_reader* pfile;
{
  cpp_toklist* list;

  init_trigraph_map ();
  list = (cpp_toklist *) xmalloc (sizeof (cpp_toklist));
  _cpp_init_toklist (list, DUMMY_TOKEN);

  for (;;)
    {
      _cpp_lex_line (pfile, list);
      if (list->tokens[0].type == CPP_EOF)
	break;

#if 0
      if (list->dirno)
	_cpp_handle_directive (pfile, list);
      else
#endif
	_cpp_output_list (pfile, list);
      _cpp_clear_toklist (list);
    }
}

/* Temporary function for illustrative purposes.  */
static void
_cpp_output_list (pfile, list)
     cpp_reader *pfile;
     cpp_toklist *list;
{
  unsigned int i;

  for (i = 0; i < list->tokens_used; i++)
    {
      CPP_RESERVE (pfile, TOKEN_LEN (&list->tokens[i]));
      pfile->limit = spell_token (pfile, &list->tokens[i], pfile->limit, 1);
    }
}

#endif
