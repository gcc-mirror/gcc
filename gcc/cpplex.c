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

/*

Cleanups to do:-

o Check line numbers assigned to all errors.
o lex_line's use of cur_token, flags and list->token_used is a bit opaque.
o Distinguish integers, floats, and 'other' pp-numbers.
o Store ints and char constants as binary values.
o New command-line assertion syntax.
o Work towards functions in cpperror.c taking a message level parameter.
  If we do this, merge the common code of do_warning and do_error.
o Comment all functions, and describe macro expansion algorithm.
o Move as much out of header files as possible.
o Remove single quote pairs `', and some '', from diagnostics.
o Correct pastability test for CPP_NAME and CPP_NUMBER.

*/

#include "config.h"
#include "system.h"
#include "intl.h"
#include "cpplib.h"
#include "cpphash.h"
#include "symcat.h"

static const cpp_token placemarker_token = {0, 0, CPP_PLACEMARKER, 0 UNION_INIT_ZERO};
static const cpp_token eof_token = {0, 0, CPP_EOF, 0 UNION_INIT_ZERO};

/* Flags for cpp_context.  */
#define CONTEXT_PASTEL	(1 << 0) /* An argument context on LHS of ##.  */
#define CONTEXT_PASTER	(1 << 1) /* An argument context on RHS of ##.  */
#define CONTEXT_RAW	(1 << 2) /* If argument tokens already expanded.  */
#define CONTEXT_ARG	(1 << 3) /* If an argument context.  */

typedef struct cpp_context cpp_context;
struct cpp_context
{
  union
  {
    const cpp_toklist *list;	/* Used for macro contexts only.  */
    const cpp_token **arg;	/* Used for arg contexts only.  */
  } u;

  /* Pushed token to be returned by next call to get_raw_token.  */
  const cpp_token *pushed_token;

  struct macro_args *args;	/* The arguments for a function-like
				   macro.  NULL otherwise.  */
  unsigned short posn;		/* Current posn, index into u.  */
  unsigned short count;		/* No. of tokens in u.  */
  unsigned short level;
  unsigned char flags;
};

typedef struct macro_args macro_args;
struct macro_args
{
  unsigned int *ends;
  const cpp_token **tokens;
  unsigned int capacity;
  unsigned int used;
  unsigned short level;
};

static const cpp_token *get_raw_token PARAMS ((cpp_reader *));
static const cpp_token *parse_arg PARAMS ((cpp_reader *, int, unsigned int,
					   macro_args *, unsigned int *));
static int parse_args PARAMS ((cpp_reader *, cpp_hashnode *, macro_args *));
static void save_token PARAMS ((macro_args *, const cpp_token *));
static int pop_context PARAMS ((cpp_reader *));
static int push_macro_context PARAMS ((cpp_reader *, const cpp_token *));
static void push_arg_context PARAMS ((cpp_reader *, const cpp_token *));
static void free_macro_args PARAMS ((macro_args *));

#define auto_expand_name_space(list) \
    _cpp_expand_name_space ((list), 1 + (list)->name_cap / 2)
static void dump_param_spelling PARAMS ((FILE *, const cpp_toklist *,
					 unsigned int));
static void output_line_command PARAMS ((cpp_reader *, cpp_printer *,
					 unsigned int));

static void process_directive	PARAMS ((cpp_reader *, const cpp_token *));
static unsigned char *trigraph_replace PARAMS ((cpp_reader *, unsigned char *,
						unsigned char *));
static const unsigned char *backslash_start PARAMS ((cpp_reader *,
						     const unsigned char *));
static int skip_block_comment PARAMS ((cpp_reader *));
static int skip_line_comment PARAMS ((cpp_reader *));
static void adjust_column PARAMS ((cpp_reader *, const U_CHAR *));
static void skip_whitespace PARAMS ((cpp_reader *, int));
static const U_CHAR *parse_name PARAMS ((cpp_reader *, cpp_token *,
				   const U_CHAR *, const U_CHAR *));
static void parse_number PARAMS ((cpp_reader *, cpp_toklist *, cpp_string *));
static void parse_string PARAMS ((cpp_reader *, cpp_toklist *, cpp_token *,
				  unsigned int));
static int trigraph_ok PARAMS ((cpp_reader *, const unsigned char *));
static void save_comment PARAMS ((cpp_toklist *, cpp_token *,
				  const unsigned char *,
				  unsigned int, unsigned int));
static void lex_line PARAMS ((cpp_reader *, cpp_toklist *));
static int lex_next PARAMS ((cpp_reader *, int));
static int is_macro_disabled PARAMS ((cpp_reader *, const cpp_toklist *,
				      const cpp_token *));

static cpp_token *stringify_arg PARAMS ((cpp_reader *, const cpp_token *));
static void expand_context_stack PARAMS ((cpp_reader *));
static unsigned char * spell_token PARAMS ((cpp_reader *, const cpp_token *,
					    unsigned char *));
static void output_token PARAMS ((cpp_reader *, FILE *, const cpp_token *,
				  const cpp_token *, int));
typedef unsigned int (* speller) PARAMS ((unsigned char *, cpp_toklist *,
					  cpp_token *));
static cpp_token *make_string_token PARAMS ((cpp_token *, const U_CHAR *,
					    unsigned int));
static cpp_token *alloc_number_token PARAMS ((cpp_reader *, int number));
static const cpp_token *special_symbol PARAMS ((cpp_reader *, cpp_hashnode *,
						const cpp_token *));
static cpp_token *duplicate_token PARAMS ((cpp_reader *, const cpp_token *));
static const cpp_token *maybe_paste_with_next PARAMS ((cpp_reader *,
						       const cpp_token *));
static enum cpp_ttype can_paste PARAMS ((cpp_reader *, const cpp_token *,
					 const cpp_token *, int *));
static unsigned int prevent_macro_expansion	PARAMS ((cpp_reader *));
static void restore_macro_expansion	PARAMS ((cpp_reader *, unsigned int));
static cpp_token *get_temp_token	PARAMS ((cpp_reader *));
static void release_temp_tokens		PARAMS ((cpp_reader *));
static U_CHAR * quote_string PARAMS ((U_CHAR *, const U_CHAR *, unsigned int));
static void process_directive PARAMS ((cpp_reader *, const cpp_token *));

#define INIT_TOKEN_STR(list, token) \
  do {(token)->val.str.len = 0; \
      (token)->val.str.text = (list)->namebuf + (list)->name_used; \
  } while (0)

#define VALID_SIGN(c, prevc) \
  (((c) == '+' || (c) == '-') && \
   ((prevc) == 'e' || (prevc) == 'E' \
    || (((prevc) == 'p' || (prevc) == 'P') && !CPP_OPTION (pfile, c89))))

/* Handle LF, CR, CR-LF and LF-CR style newlines.  Assumes next
   character, if any, is in buffer.  */

#define handle_newline(cur, limit, c) \
 do { \
  if ((cur) < (limit) && *(cur) == '\r' + '\n' - c) \
    (cur)++; \
  pfile->buffer->lineno++; \
  pfile->buffer->line_base = (cur); \
  pfile->col_adjust = 0; \
 } while (0)

#define IMMED_TOKEN() (!(cur_token->flags & PREV_WHITE))
#define PREV_TOKEN_TYPE (cur_token[-1].type)

#define PUSH_TOKEN(ttype) cur_token++->type = (ttype)
#define REVISE_TOKEN(ttype) cur_token[-1].type = (ttype)
#define BACKUP_TOKEN(ttype) (--cur_token)->type = (ttype)
#define BACKUP_DIGRAPH(ttype) do { \
  BACKUP_TOKEN(ttype); cur_token->flags |= DIGRAPH;} while (0)

/* An upper bound on the number of bytes needed to spell a token,
   including preceding whitespace.  */
static inline size_t TOKEN_LEN PARAMS ((const cpp_token *));
static inline size_t
TOKEN_LEN (token)
     const cpp_token *token;
{
  size_t len;

  switch (TOKEN_SPELL (token))
    {
    default:		len = 0;			break;
    case SPELL_STRING:	len = token->val.str.len;	break;
    case SPELL_IDENT:	len = token->val.node->length;	break;
    }
  return len + 5;
}

#define IS_ARG_CONTEXT(c) ((c)->flags & CONTEXT_ARG)
#define CURRENT_CONTEXT(pfile) ((pfile)->contexts + (pfile)->cur_context)
#define ON_REST_ARG(c) \
 (((c)->u.list->flags & VAR_ARGS) \
  && (c)->u.list->tokens[(c)->posn - 1].val.aux \
      == (unsigned int) ((c)->u.list->paramc - 1))

#define ASSIGN_FLAGS_AND_POS(d, s) \
  do {(d)->flags = (s)->flags & (PREV_WHITE | BOL | PASTE_LEFT); \
      if ((d)->flags & BOL) {(d)->col = (s)->col; (d)->line = (s)->line;} \
  } while (0)

/* f is flags, just consisting of PREV_WHITE | BOL.  */
#define MODIFY_FLAGS_AND_POS(d, s, f) \
  do {(d)->flags &= ~(PREV_WHITE | BOL); (d)->flags |= (f); \
      if ((f) & BOL) {(d)->col = (s)->col; (d)->line = (s)->line;} \
  } while (0)

#define OP(e, s) { SPELL_OPERATOR, U s           },
#define TK(e, s) { s,              U STRINGX (e) },

const struct token_spelling
_cpp_token_spellings [N_TTYPES] = {TTYPE_TABLE };

#undef OP
#undef TK

/* Notify the compiler proper that the current line number has jumped,
   or the current file name has changed.  */

static void
output_line_command (pfile, print, line)
     cpp_reader *pfile;
     cpp_printer *print;
     unsigned int line;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  if (line == 0)
    return;

  /* End the previous line of text.  */
  if (pfile->need_newline)
    {
      putc ('\n', print->outf);
      print->lineno++;
    }
  pfile->need_newline = 0;

  if (CPP_OPTION (pfile, no_line_commands))
    return;

  /* If the current file has not changed, we can output a few newlines
     instead if we want to increase the line number by a small amount.
     We cannot do this if print->lineno is zero, because that means we
     haven't output any line commands yet.  (The very first line
     command output is a `same_file' command.)

     'nominal_fname' values are unique, so they can be compared by
     comparing pointers.  */
  if (ip->nominal_fname == print->last_fname && print->lineno > 0
      && line >= print->lineno && line < print->lineno + 8)
    {
      while (line > print->lineno)
	{
	  putc ('\n', print->outf);
	  print->lineno++;
	}
      return;
    }

  fprintf (print->outf, "# %u \"%s\"%s\n", line, ip->nominal_fname,
	   cpp_syshdr_flags (pfile, ip));

  print->last_fname = ip->nominal_fname;
  print->lineno = line;
}

/* Like fprintf, but writes to a printer object.  You should be sure
   always to generate a complete line when you use this function.  */
void
cpp_printf VPARAMS ((cpp_reader *pfile, cpp_printer *print,
		     const char *fmt, ...))
{
  va_list ap;
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  cpp_printer *print;
  const char *fmt;
#endif

  VA_START (ap, fmt);

#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  print = va_arg (ap, cpp_printer *);
  fmt = va_arg (ap, const char *);
#endif

  /* End the previous line of text.  */
  if (pfile->need_newline)
    {
      putc ('\n', print->outf);
      print->lineno++;
    }
  pfile->need_newline = 0;

  vfprintf (print->outf, fmt, ap);
  va_end (ap);
}

/* Scan until CPP_BUFFER (PFILE) is exhausted, discarding output.  */

void
cpp_scan_buffer_nooutput (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *stop = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
  const cpp_token *token;

  /* In no-output mode, we can ignore everything but directives.  */
  for (;;)
    {
      token = _cpp_get_token (pfile);

      if (token->type == CPP_EOF)
	{
	  cpp_pop_buffer (pfile);
	  if (CPP_BUFFER (pfile) == stop)
	    break;
	}

      if (token->type == CPP_HASH && token->flags & BOL
	  && pfile->token_list.directive)
	{
	  process_directive (pfile, token);
	  continue;
	}

      _cpp_skip_rest_of_line (pfile);
    }
}

/* Scan until CPP_BUFFER (pfile) is exhausted, writing output to PRINT.  */
void
cpp_scan_buffer (pfile, print)
     cpp_reader *pfile;
     cpp_printer *print;
{
  cpp_buffer *stop = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
  const cpp_token *token, *prev = 0;

  for (;;)
    {
      token = _cpp_get_token (pfile);
      if (token->type == CPP_EOF)
	{
	  cpp_pop_buffer (pfile);

	  if (CPP_BUFFER (pfile) == stop)
	    return;

	  prev = 0;
	  continue;
	}

      if (token->flags & BOL)
	{
	  output_line_command (pfile, print, pfile->token_list.line);
	  prev = 0;

	  if (token->type == CPP_HASH && pfile->token_list.directive)
	    {
	      process_directive (pfile, token);
	      continue;
	    }
	}

      if (token->type != CPP_PLACEMARKER)
	{
	  output_token (pfile, print->outf, token, prev, 1);
	  pfile->need_newline = 1;
	}

      prev = token;
    }
}

/* Helper routine used by parse_include, which can't see spell_token.
   Reinterpret the current line as an h-char-sequence (< ... >); we are
   looking at the first token after the <.  */
const cpp_token *
_cpp_glue_header_name (pfile)
     cpp_reader *pfile;
{
  const cpp_token *t;
  cpp_token *hdr;
  U_CHAR *buf, *p;
  size_t len, avail;

  avail = 40;
  len = 0;
  buf = xmalloc (avail);

  for (;;)
    {
      t = _cpp_get_token (pfile);
      if (t->type == CPP_GREATER || t->type == CPP_EOF)
	break;

      if (len + TOKEN_LEN (t) > avail)
	{
	  avail = len + TOKEN_LEN (t) + 40;
	  buf = xrealloc (buf, avail);
	}

      if (t->flags & PREV_WHITE)
	buf[len++] = ' ';

      p = spell_token (pfile, t, buf + len);
      len = (size_t) (p - buf);  /* p known >= buf */
    }

  if (t->type == CPP_EOF)
    cpp_error (pfile, "missing terminating > character");

  buf = xrealloc (buf, len);

  hdr = get_temp_token (pfile);
  hdr->type = CPP_HEADER_NAME;
  hdr->flags = 0;
  hdr->val.str.text = buf;
  hdr->val.str.len = len;
  return hdr;
}

/* Token-buffer helper functions.  */

/* Expand a token list's string space. It is *vital* that
   list->tokens_used is correct, to get pointer fix-up right.  */
void
_cpp_expand_name_space (list, len)
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
	if (TOKEN_SPELL (&list->tokens[i]) == SPELL_STRING)
	  list->tokens[i].val.str.text += (list->namebuf - old_namebuf);
    }
}

/* If there is not enough room for LEN more characters, expand the
   list by just enough to have room for LEN characters.  */
void
_cpp_reserve_name_space (list, len)
     cpp_toklist *list;
     unsigned int len;
{
  unsigned int room = list->name_cap - list->name_used;

  if (room < len)
    _cpp_expand_name_space (list, len - room);
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
  if (flags == NO_DUMMY_TOKEN)
    {
      list->tokens_cap = 0;
      list->tokens = 0;
      list->name_cap = 0;
      list->namebuf = 0;
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
      list->namebuf = (unsigned char *) xmalloc (list->name_cap);
      list->flags = LIST_OFFSET;
    }

  _cpp_clear_toklist (list);
}

/* Clear a token list.  */
void
_cpp_clear_toklist (list)
     cpp_toklist *list;
{
  list->tokens_used = 0;
  list->name_used = 0;
  list->directive = 0;
  list->paramc = 0;
  list->params_len = 0;
  list->flags &= LIST_OFFSET;  /* clear all but that one */
}

/* Free a token list.  Does not free the list itself, which may be
   embedded in a larger structure.  */
void
_cpp_free_toklist (list)
     const cpp_toklist *list;
{
  if (list->flags & LIST_OFFSET)
    free (list->tokens - 1);	/* Backup over dummy token.  */
  else
    free (list->tokens);
  free (list->namebuf);
}

/* Compare two tokens.  */
int
_cpp_equiv_tokens (a, b)
     const cpp_token *a, *b;
{
  if (a->type == b->type && a->flags == b->flags)
    switch (TOKEN_SPELL (a))
      {
      default:			/* Keep compiler happy.  */
      case SPELL_OPERATOR:
	return 1;
      case SPELL_CHAR:
      case SPELL_NONE:
	return a->val.aux == b->val.aux; /* arg_no or character.  */
      case SPELL_IDENT:
	return a->val.node == b->val.node;
      case SPELL_STRING:
	return (a->val.str.len == b->val.str.len
		&& !memcmp (a->val.str.text, b->val.str.text,
			    a->val.str.len));
      }

  return 0;
}

/* Compare two token lists.  */
int
_cpp_equiv_toklists (a, b)
     const cpp_toklist *a, *b;
{
  unsigned int i;

  if (a->tokens_used != b->tokens_used
      || a->flags != b->flags
      || a->paramc != b->paramc)
    return 0;

  for (i = 0; i < a->tokens_used; i++)
    if (! _cpp_equiv_tokens (&a->tokens[i], &b->tokens[i]))
      return 0;
  return 1;
}

/* Utility routine:

   Compares, the token TOKEN to the NUL-terminated string STRING.
   TOKEN must be a CPP_NAME.  Returns 1 for equal, 0 for unequal.  */

int
cpp_ideq (token, string)
     const cpp_token *token;
     const char *string;
{
  if (token->type != CPP_NAME)
    return 0;

  return !ustrcmp (token->val.node->name, (const U_CHAR *)string);
}

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
 Hence the '=' handler would never see any intervening tokens.

 To make trigraphs work in this context, as in precedence trigraphs
 are highest and converted before anything else, the '?' handler does
 lookahead to see if it is a trigraph, and if so skips the trigraph
 and pushes the token it represents onto the top of the stack.  This
 also works in the particular case of a CPP_BACKSLASH trigraph.

 To the preprocessor, whitespace is only significant to the point of
 knowing whether whitespace precedes a particular token.  For example,
 the '=' handler needs to know whether there was whitespace between it
 and a "!" token on the top of the stack, to make the token conversion
 decision correctly.  So each token has a PREV_WHITE flag to
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
			       (int) *end, (int) _cpp_trigraph_map[*end]);
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

static unsigned char *
trigraph_replace (pfile, src, limit)
     cpp_reader *pfile;
     unsigned char *src;
     unsigned char *limit;
{
  unsigned char *dest;

  /* Starting with src[1], find two consecutive '?'.  The case of no
     trigraphs is streamlined.  */
  
  for (src++; src + 1 < limit; src += 2)
    {
      if (src[0] != '?')
	continue;

      /* Make src point to the 1st (NOT 2nd) of two consecutive '?'s.  */
      if (src[-1] == '?')
	src--;
      else if (src + 2 == limit || src[1] != '?')
	continue;

      /* Check if it really is a trigraph.  */
      if (_cpp_trigraph_map[src[2]] == 0)
	continue;

      dest = src;
      goto trigraph_found;
    }
  return limit;

  /* Now we have a trigraph, we need to scan the remaining buffer, and
     copy-shifting its contents left if replacement is enabled.  */
  for (; src + 2 < limit; dest++, src++)
    if ((*dest = *src) == '?' && src[1] == '?' && _cpp_trigraph_map[src[2]])
      {
      trigraph_found:
	src += 2;
	if (trigraph_ok (pfile, pfile->buffer->cur - (limit - src)))
	  *dest = _cpp_trigraph_map[*src];
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
skip_block_comment (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  const unsigned char *char_after_star = 0;
  const unsigned char *cur = buffer->cur;
  
  for (; cur < buffer->rlimit; )
    {
      unsigned char c = *cur++;

      /* People like decorating comments with '*', so check for
	 '/' instead for efficiency.  */
      if (c == '/')
	{
	  /* Don't view / then * then / as finishing the comment.  */
	  if ((cur[-2] == '*' && cur - 1 > buffer->cur)
	      || cur - 1 == char_after_star)
	    {
	      buffer->cur = cur;
	      return 0;
	    }

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
      else if (is_vspace (c))
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
      else if (c == '\t')
	adjust_column (pfile, cur - 1);
    }

  buffer->cur = cur;
  return 1;
}

/* Skip a C++ line comment.  Handles escaped newlines.  Returns
   non-zero if a multiline comment.  */
static int
skip_line_comment (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  register const unsigned char *cur = buffer->cur;
  int multiline = 0;

  for (; cur < buffer->rlimit; )
    {
      unsigned char c = *cur++;

      if (is_vspace (c))
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

/* TAB points to a \t character.  Update col_adjust so we track the
   column correctly.  */
static void
adjust_column (pfile, tab)
     cpp_reader *pfile;
     const U_CHAR *tab;
{
  /* Zero-based column.  */
  unsigned int col = CPP_BUF_COLUMN (pfile->buffer, tab);

  /* Round it up to multiple of the tabstop, but subtract 1 since the
     tab itself occupies a character position.  */
  pfile->col_adjust += (CPP_OPTION (pfile, tabstop)
			- col % CPP_OPTION (pfile, tabstop)) - 1;
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
  unsigned short warned = 0;

  /* We only want non-vertical space, i.e. ' ' \t \f \v \0. */
  while (buffer->cur < buffer->rlimit)
    {
      unsigned char c = *buffer->cur;

      if (!is_nvspace (c))
	break;

      buffer->cur++;
      /* Horizontal space always OK.  */
      if (c == ' ')
	continue;
      else if (c == '\t')
	adjust_column (pfile, buffer->cur - 1);
      /* Must be \f \v or \0.  */
      else if (c == '\0')
	{
	  if (!warned)
	    cpp_warning_with_line (pfile, CPP_BUF_LINE (buffer),
				   CPP_BUF_COL (buffer),
				   "embedded null character ignored");
	  warned = 1;
	}
      else if (in_directive && CPP_PEDANTIC (pfile))
	cpp_pedwarn_with_line (pfile, CPP_BUF_LINE (buffer),
			       CPP_BUF_COL (buffer),
			       "%s in preprocessing directive",
			       c == '\f' ? "form feed" : "vertical tab");
    }
}

/* Parse (append) an identifier.  Calculates the hash value of the
   token while parsing, for performance.  The algorithm *must* match
   cpp_lookup().  */
static const U_CHAR *
parse_name (pfile, tok, cur, rlimit)
     cpp_reader *pfile;
     cpp_token *tok;
     const U_CHAR *cur, *rlimit;
{
  const U_CHAR *name;
  unsigned int len;
  unsigned int r;

  name = cur;
  r = 0;
  while (cur < rlimit)
    {
      if (! is_idchar (*cur))
	break;
      /* $ is not a identifier character in the standard, but is
	 commonly accepted as an extension.  Don't warn about it in
	 skipped conditional blocks. */
      if (*cur == '$' && CPP_PEDANTIC (pfile) && ! pfile->skipping)
	{
	  CPP_BUFFER (pfile)->cur = cur;
	  cpp_pedwarn (pfile, "'$' character in identifier");
	}

      r = HASHSTEP (r, cur);
      cur++;
    }
  len = cur - name;

  if (tok->type == CPP_NAME && tok->val.node == 0)
    tok->val.node = _cpp_lookup_with_hash (pfile, name, len, r);
  else
    {
      unsigned int oldlen;
      U_CHAR *newname;

      if (tok->type == CPP_NAME)
	oldlen = tok->val.node->length;
      else
	oldlen = 1;

      newname = alloca (oldlen + len);

      if (tok->type == CPP_NAME)
	memcpy (newname, tok->val.node->name, oldlen);
      else
	newname[0] = tok->val.aux;
      memcpy (newname + oldlen, name, len);
      tok->val.node = cpp_lookup (pfile, newname, len + oldlen);
      tok->type = CPP_NAME;
    }

  return cur;
}

/* Parse (append) a number.  */
static void
parse_number (pfile, list, name)
     cpp_reader *pfile;
     cpp_toklist *list;
     cpp_string *name;
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
   cpp_string, which should be expandable and thus at the top of the
   list's stack.  Handles embedded trigraphs, if necessary, and
   escaped newlines.

   Can be used for character constants (terminator = '\''), string
   constants ('"') and angled headers ('>').  Multi-line strings are
   allowed, except for within directives.  */

static void
parse_string (pfile, list, token, terminator)
     cpp_reader *pfile;
     cpp_toklist *list;
     cpp_token *token;
     unsigned int terminator;
{
  cpp_buffer *buffer = pfile->buffer;
  cpp_string *name = &token->val.str;
  register const unsigned char *cur = buffer->cur;
  const unsigned char *name_limit;
  unsigned char *namebuf;
  unsigned int null_count = 0;
  unsigned int trigraphed = list->name_used;

 expanded:
  name_limit = list->namebuf + list->name_cap;
  namebuf = list->namebuf + list->name_used;

  for (; cur < buffer->rlimit && namebuf < name_limit; )
    {
      unsigned int c = *namebuf++ = *cur++; /* Copy a single char.  */

      if (c == '\0')
	null_count++;
      else if (c == terminator || is_vspace (c))
	{
	  /* Needed for trigraph_replace and multiline string warning.  */
	  buffer->cur = cur;

	  /* Scan for trigraphs before checking if backslash-escaped.  */
	  if ((CPP_OPTION (pfile, trigraphs)
	       || CPP_OPTION (pfile, warn_trigraphs))
	      && namebuf - (list->namebuf + trigraphed) >= 3)
	    {
	      namebuf = trigraph_replace (pfile, list->namebuf + trigraphed,
					  namebuf);
	      /* The test above guarantees trigraphed will be positive.  */
	      trigraphed = namebuf - list->namebuf - 2;
	    }

	  namebuf--;     /* Drop the newline / terminator from the name.  */
	  if (is_vspace (c))
	    {
	      /* Drop a backslash newline, and continue. */
	      U_CHAR *old = namebuf;
	      while (namebuf > list->namebuf && is_hspace (namebuf[-1]))
		namebuf--;
	      if (namebuf > list->namebuf && namebuf[-1] == '\\')
		{
		  handle_newline (cur, buffer->rlimit, c);
		  namebuf--;
		  if (old[-1] != '\\')
		    {
		      buffer->cur = cur;
		      cpp_warning (pfile,
				   "backslash and newline separated by space");
		    }
		  continue;
		}
	      else
		namebuf = old;

	      cur--;

	      /* In assembly language, silently terminate strings of
		 either variety at end of line.  This is a kludge
		 around not knowing where comments are.  */
	      if (CPP_OPTION (pfile, lang_asm))
		goto out;

	      /* Character constants and header names may not extend
		 over multiple lines.  In Standard C, neither may
		 strings.  We accept multiline strings as an
		 extension.  (Even in directives - otherwise, glibc's
		 longlong.h breaks.)  */
	      if (terminator != '"')
		goto unterminated;
		
	      cur++;  /* Move forwards again.  */

	      if (pfile->multiline_string_line == 0)
		{
		  pfile->multiline_string_line = token->line;
		  pfile->multiline_string_column = token->col;
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
  if (is_vspace (cur[-1]))
    cur--;

 unterminated:
  cpp_error (pfile, "missing terminating %c character", (int) terminator);

  if (terminator == '\"' && pfile->multiline_string_line != list->line
      && pfile->multiline_string_line != 0)
    {
      cpp_error_with_line (pfile, pfile->multiline_string_line,
			   pfile->multiline_string_column,
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
   style, '/' = C++ style.  For code simplicity, the stored comment
   includes the comment start and any terminator.  */

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
    _cpp_expand_name_space (list, len);

  INIT_TOKEN_STR (list, token);
  token->type = CPP_COMMENT;
  token->val.str.len = len;

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

#define KNOWN_DIRECTIVE() (list->directive != 0)
#define MIGHT_BE_DIRECTIVE() \
(cur_token == &list->tokens[first_token + 1] && cur_token[-1].type == CPP_HASH)

static void
lex_line (pfile, list)
     cpp_reader *pfile;
     cpp_toklist *list;
{
  cpp_token *cur_token, *token_limit, *first;
  cpp_buffer *buffer = pfile->buffer;
  const unsigned char *cur = buffer->cur;
  unsigned char flags = 0;
  unsigned int first_token = list->tokens_used;

  if (!(list->flags & LIST_OFFSET))
    (abort) ();

 retry:
  list->file = buffer->nominal_fname;
  list->line = CPP_BUF_LINE (buffer);
  pfile->col_adjust = 0;
  pfile->in_lex_line = 1;
  if (cur == buffer->buf)
    list->flags |= BEG_OF_FILE;

 expanded:
  token_limit = list->tokens + list->tokens_cap;
  cur_token = list->tokens + list->tokens_used;

  for (; cur < buffer->rlimit && cur_token < token_limit;)
    {
      unsigned char c;

      /* Optimize non-vertical whitespace skipping; most tokens are
	 probably separated by whitespace. (' ' '\t' '\v' '\f' '\0').  */
      c = *cur;
      if (is_nvspace (c))
	{
	  buffer->cur = cur;
	  skip_whitespace (pfile, (list->tokens[first_token].type == CPP_HASH
				   && cur_token > &list->tokens[first_token]));
	  cur = buffer->cur;

	  flags = PREV_WHITE;
	  if (cur == buffer->rlimit)
	    break;
	  c = *cur;
	}
      cur++;

      /* Initialize current token.  CPP_EOF will not be fixed up by
	 expand_name_space.  */
      list->tokens_used = cur_token - list->tokens + 1;
      cur_token->type = CPP_EOF;
      cur_token->col = CPP_BUF_COLUMN (buffer, cur);
      cur_token->line = CPP_BUF_LINE (buffer);
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
	    INIT_TOKEN_STR (list, cur_token);
	    /* Prepend an immediately previous CPP_DOT token.  */
	    if (prev_dot)
	      {
		if (list->name_cap == list->name_used)
		  auto_expand_name_space (list);

		cur_token->val.str.len = 1;
		list->namebuf[list->name_used++] = '.';
	      }

	  continue_number:
	    cur_token->type = CPP_NUMBER; /* Before parse_number.  */
	    buffer->cur = cur;
	    parse_number (pfile, list, &cur_token->val.str);
	    cur = buffer->cur;
	  }
	  /* Check for # 123 form of #line.  */
	  if (MIGHT_BE_DIRECTIVE ())
	    list->directive = _cpp_check_linemarker (pfile, cur_token,
						     !(cur_token[-1].flags
						       & PREV_WHITE));
	  cur_token++;
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

	  /* In Objective C, '@' may begin certain keywords.  */
	  if (CPP_OPTION (pfile, objc) && cur_token[-1].type == CPP_OTHER
	      && cur_token[-1].val.aux == '@' && IMMED_TOKEN ())
	    cur_token--;
	  else
	    {
	      cur_token->val.node = 0;
	      cur_token->type = CPP_NAME; /* Identifier, macro etc.  */
	    }

	continue_name:
	  cur = parse_name (pfile, cur_token, cur, buffer->rlimit);

	  if (MIGHT_BE_DIRECTIVE ())
	    list->directive = _cpp_check_directive (pfile, cur_token,
						    !(list->tokens[0].flags
						      & PREV_WHITE));
	  /* Convert named operators to their proper types.  */
	  if (cur_token->val.node->type == T_OPERATOR)
	    {
	      cur_token->flags |= NAMED_OP;
	      cur_token->type = cur_token->val.node->value.code;
	    }

	  cur_token++;
	  break;

	case '\'':
	  cur_token->type = CPP_CHAR;
	  if (cur_token[-1].type == CPP_NAME && IMMED_TOKEN ()
	      && cur_token[-1].val.node == pfile->spec_nodes->n_L)
	    BACKUP_TOKEN (CPP_WCHAR);
	  goto do_parse_string;
	  
	case '\"':
	  cur_token->type = CPP_STRING;
	  if (cur_token[-1].type == CPP_NAME && IMMED_TOKEN ()
	      && cur_token[-1].val.node == pfile->spec_nodes->n_L)
	    BACKUP_TOKEN (CPP_WSTRING);
	  else if (CPP_OPTION (pfile, objc)
		   && cur_token[-1].type == CPP_OTHER && IMMED_TOKEN ()
		   && cur_token[-1].val.aux == '@')
	    BACKUP_TOKEN (CPP_OSTRING);

	do_parse_string:
	  /* Here c is one of ' " or >.  */
	  INIT_TOKEN_STR (list, cur_token);
	  buffer->cur = cur;
	  parse_string (pfile, list, cur_token, c);
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
		  if (CPP_IN_SYSTEM_HEADER (pfile))
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
#if 0 /* Leave until new lexer in place.  */
		      if (cur[-2] != c)
			cpp_warning (pfile,
				     "comment start split across lines");
#endif
		      if (skip_line_comment (pfile))
			cpp_warning (pfile, "multi-line comment");

		      /* Back-up to first '-' or '/'.  */
		      cur_token--;
		      if (!CPP_OPTION (pfile, discard_comments)
			  && (!KNOWN_DIRECTIVE()
			      || (list->directive->flags & COMMENTS)))
			save_comment (list, cur_token++, cur,
				      buffer->cur - cur, c);
		      else
			flags = PREV_WHITE;

		      cur = buffer->cur;
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
#if 0 /* Leave until new lexer in place.  */
		  if (cur[-2] != '/')
		    cpp_warning (pfile,
				 "comment start '/*' split across lines");
#endif
		  if (skip_block_comment (pfile))
		    cpp_error_with_line (pfile, list->line, cur_token[-1].col,
					 "unterminated comment");
#if 0 /* Leave until new lexer in place.  */
		  else if (buffer->cur[-2] != '*')
		    cpp_warning (pfile,
				 "comment end '*/' split across lines");
#endif
		  /* Back up to opening '/'.  */
		  cur_token--;
		  if (!CPP_OPTION (pfile, discard_comments)
		      && (!KNOWN_DIRECTIVE()
			  || (list->directive->flags & COMMENTS)))
		    save_comment (list, cur_token++, cur,
				  buffer->cur - cur, c);
		  else
		    flags = PREV_WHITE;

		  cur = buffer->cur;
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
	  if (PREV_TOKEN_TYPE == CPP_BACKSLASH)
	    {
	      /* backslash space newline is still treated as backslash-newline;
		 we think this is standard conforming, with some reservations
		 about actually _using_ the weasel words in C99 5.1.1.2
		 (translation phase 1 is allowed to do whatever it wants to
		 your input as long as it's documented).  */
	      if (! IMMED_TOKEN ())
		{
		  buffer->cur = cur;
		  cpp_warning (pfile,
			       "backslash and newline separated by space");
		}
	      
	      /* Remove the escaped newline.  Then continue to process
		 any interrupted name or number.  */
	      cur_token--;
	      /* Backslash-newline may not be immediately followed by
		 EOF (C99 5.1.1.2).  */
	      if (cur >= buffer->rlimit)
		{
		  cpp_pedwarn (pfile, "backslash-newline at end of file");
		  break;
		}
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
	  else if (MIGHT_BE_DIRECTIVE ())
	    {
	      /* "Null directive." C99 6.10.7: A preprocessing
		 directive of the form # <new-line> has no effect.

		 But it is still a directive, and therefore disappears
		 from the output. */
	      cur_token--;
	      if (cur_token->flags & PREV_WHITE
		  && CPP_WTRADITIONAL (pfile))
		cpp_warning (pfile, "K+R C ignores #\\n with the # indented");
	    }

	  /* Skip vertical space until we have at least one token to
             return.  */
	  if (cur_token != &list->tokens[first_token])
	    goto out;
	  list->line = CPP_BUF_LINE (buffer);
	  break;

	case '-':
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE == CPP_MINUS)
	    REVISE_TOKEN (CPP_MINUS_MINUS);
	  else
	    PUSH_TOKEN (CPP_MINUS);
	  break;

	make_hash:
	case '#':
	  /* The digraph flag checking ensures that ## and %:%:
	     are interpreted as CPP_PASTE, but #%: and %:# are not.  */
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
	      else if (CPP_OPTION (pfile, digraphs))
		{
		  /* Digraph: "<:" is a '['  */
		  if (PREV_TOKEN_TYPE == CPP_LESS)
		    BACKUP_DIGRAPH (CPP_OPEN_SQUARE);
		  /* Digraph: "%:" is a '#'  */
		  else if (PREV_TOKEN_TYPE == CPP_MOD)
		    {
		      (--cur_token)->flags |= DIGRAPH;
		      goto make_hash;
		    }
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
	      else if (CPP_OPTION (pfile, digraphs))
		{
		  /* Digraph: ":>" is a ']'  */
		  if (PREV_TOKEN_TYPE == CPP_COLON)
		    BACKUP_DIGRAPH (CPP_CLOSE_SQUARE);
		  /* Digraph: "%>" is a '}'  */
		  else if (PREV_TOKEN_TYPE == CPP_MOD)
		    BACKUP_DIGRAPH (CPP_CLOSE_BRACE);
		}
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
	  if (KNOWN_DIRECTIVE () && (list->directive->flags & INCL))
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
	  if (IMMED_TOKEN () && PREV_TOKEN_TYPE == CPP_LESS
	      && CPP_OPTION (pfile, digraphs))
	    BACKUP_DIGRAPH (CPP_OPEN_BRACE);
	  cur_token++;
	  break;

	case '?':
	  if (cur + 1 < buffer->rlimit && *cur == '?'
	      && _cpp_trigraph_map[cur[1]] && trigraph_ok (pfile, cur + 1))
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
	      && !(cur_token[-1].flags & PREV_WHITE))
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
	  cur_token->val.aux = c;
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
  if (cur_token == &list->tokens[first_token] && pfile->done_initializing)
    {
      if (cur > buffer->buf && !is_vspace (cur[-1]))
	cpp_pedwarn_with_line (pfile, CPP_BUF_LINE (buffer),
			       CPP_BUF_COLUMN (buffer, cur),
			       "no newline at end of file");
      cur_token++->type = CPP_EOF;
    }

 out:
  /* All tokens are allocated, so the memory location is fixed.  */
  first = &list->tokens[first_token];

  /* Don't complain about the null directive, nor directives in
     assembly source: we don't know where the comments are, and # may
     introduce assembler pseudo-ops.  Don't complain about invalid
     directives in skipped conditional groups (6.10 p4).  */
  if (first->type == CPP_HASH && list->directive == 0 && !pfile->skipping
      && cur_token > first + 1 && !CPP_OPTION (pfile, lang_asm))
    {
      if (first[1].type == CPP_NAME)
	cpp_error (pfile, "invalid preprocessing directive #%s",
		   first[1].val.node->name);
      else
	cpp_error (pfile, "invalid preprocessing directive");

      /* Discard this line to prevent further errors from cc1.  */
      _cpp_clear_toklist (list);
      goto retry;
    }

  /* Put EOF at end of known directives.  This covers "directives do
     not extend beyond the end of the line (description 6.10 part 2)".  */
  if (KNOWN_DIRECTIVE () || !pfile->done_initializing)
    {
      pfile->first_directive_token = first;
      cur_token++->type = CPP_EOF;
    }

  first->flags |= BOL;
  if (first_token != 0)
    /* 6.10.3.10: Within the sequence of preprocessing tokens making
       up the invocation of a function-like macro, new line is
       considered a normal white-space character.  */
    first->flags |= PREV_WHITE;

  buffer->cur = cur;
  list->tokens_used = cur_token - list->tokens;
  pfile->in_lex_line = 0;
}

/* Write the spelling of a token TOKEN, with any appropriate
   whitespace before it, to FP.  PREV is the previous token, which
   is used to determine if we need to shove in an extra space in order
   to avoid accidental token paste.  If WHITE is 0, do not insert any
   leading whitespace.  */
static void
output_token (pfile, fp, token, prev, white)
     cpp_reader *pfile;
     FILE *fp;
     const cpp_token *token, *prev;
     int white;
{
  if (white)
    {
      int dummy;

      if (token->col && (token->flags & BOL))
	{
	  /* Supply enough whitespace to put this token in its original
	     column.  Don't bother trying to reconstruct tabs; we can't
	     get it right in general, and nothing ought to care.  (Yes,
	     some things do care; the fault lies with them.)  */
	  unsigned int spaces = token->col - 1;
      
	  while (spaces--)
	    putc (' ', fp);
	}
      else if (token->flags & PREV_WHITE)
	putc (' ', fp);
      else
      /* Check for and prevent accidental token pasting.
	 In addition to the cases handled by can_paste, consider

	 a + ++b - if there is not a space between the + and ++, it
	 will be misparsed as a++ + b.  But + ## ++ doesn't produce
	 a valid token.  */
	if (prev
	    && (can_paste (pfile, prev, token, &dummy) != CPP_EOF
		|| (prev->type == CPP_PLUS && token->type == CPP_PLUS_PLUS)
		|| (prev->type == CPP_MINUS && token->type == CPP_MINUS_MINUS)))
	putc (' ', fp);
    }

  switch (TOKEN_SPELL (token))
    {
    case SPELL_OPERATOR:
      {
	const unsigned char *spelling;

	if (token->flags & DIGRAPH)
	  spelling = digraph_spellings[token->type - CPP_FIRST_DIGRAPH];
	else if (token->flags & NAMED_OP)
	  goto spell_ident;
	else
	  spelling = TOKEN_NAME (token);

	ufputs (spelling, fp);
      }
      break;

    case SPELL_IDENT:
      spell_ident:
      ufputs (token->val.node->name, fp);
      break;

    case SPELL_STRING:
      {
	int left, right, tag;
	switch (token->type)
	  {
	  case CPP_STRING:	left = '"';  right = '"';  tag = '\0'; break;
	  case CPP_WSTRING:	left = '"';  right = '"';  tag = 'L';  break;
	  case CPP_OSTRING:	left = '"';  right = '"';  tag = '@';  break;
	  case CPP_CHAR:	left = '\''; right = '\''; tag = '\0'; break;
    	  case CPP_WCHAR:	left = '\''; right = '\''; tag = 'L';  break;
	  case CPP_HEADER_NAME:	left = '<';  right = '>';  tag = '\0'; break;
	  default:		left = '\0'; right = '\0'; tag = '\0'; break;
	  }
	if (tag) putc (tag, fp);
	if (left) putc (left, fp);
	fwrite (token->val.str.text, 1, token->val.str.len, fp);
	if (right) putc (right, fp);
      }
      break;

    case SPELL_CHAR:
      putc (token->val.aux, fp);
      break;

    case SPELL_NONE:
      /* Placemarker or EOF - no output.  (Macro args are handled
         elsewhere.  */
      break;
    }
}

/* Dump the original user's spelling of argument index ARG_NO to the
   macro whose expansion is LIST.  */
static void
dump_param_spelling (fp, list, arg_no)
     FILE *fp;
     const cpp_toklist *list;
     unsigned int arg_no;
{
  const U_CHAR *param = list->namebuf;

  while (arg_no--)
    param += ustrlen (param) + 1;
  ufputs (param, fp);
}

/* Output all the tokens of LIST, starting at TOKEN, to FP.  */
void
cpp_output_list (pfile, fp, list, token)
     cpp_reader *pfile;
     FILE *fp;
     const cpp_toklist *list;
     const cpp_token *token;
{
  const cpp_token *limit = list->tokens + list->tokens_used;
  const cpp_token *prev = 0;
  int white = 0;

  while (token < limit)
    {
      /* XXX Find some way we can write macro args from inside
	 output_token/spell_token.  */
      if (token->type == CPP_MACRO_ARG)
	{
	  if (white && token->flags & PREV_WHITE)
	    putc (' ', fp);
	  if (token->flags & STRINGIFY_ARG)
	    putc ('#', fp);
	  dump_param_spelling (fp, list, token->val.aux);
	}
      else
	output_token (pfile, fp, token, prev, white);
      if (token->flags & PASTE_LEFT)
	fputs (" ##", fp);
      prev = token;
      token++;
      white = 1;
    }
}


/* Write the spelling of a token TOKEN to BUFFER.  The buffer must
   already contain the enough space to hold the token's spelling.
   Returns a pointer to the character after the last character
   written.  */

static unsigned char *
spell_token (pfile, token, buffer)
     cpp_reader *pfile;		/* Would be nice to be rid of this...  */
     const cpp_token *token;
     unsigned char *buffer;
{
  switch (TOKEN_SPELL (token))
    {
    case SPELL_OPERATOR:
      {
	const unsigned char *spelling;
	unsigned char c;

	if (token->flags & DIGRAPH)
	  spelling = digraph_spellings[token->type - CPP_FIRST_DIGRAPH];
	else if (token->flags & NAMED_OP)
	  goto spell_ident;
	else
	  spelling = TOKEN_NAME (token);
	
	while ((c = *spelling++) != '\0')
	  *buffer++ = c;
      }
      break;

    case SPELL_IDENT:
      spell_ident:
      memcpy (buffer, token->val.node->name, token->val.node->length);
      buffer += token->val.node->length;
      break;

    case SPELL_STRING:
      {
	int left, right, tag;
	switch (token->type)
	  {
	  case CPP_STRING:	left = '"';  right = '"';  tag = '\0'; break;
	  case CPP_WSTRING:	left = '"';  right = '"';  tag = 'L';  break;
	  case CPP_OSTRING:	left = '"';  right = '"';  tag = '@';  break;
	  case CPP_CHAR:	left = '\''; right = '\''; tag = '\0'; break;
    	  case CPP_WCHAR:	left = '\''; right = '\''; tag = 'L';  break;
	  case CPP_HEADER_NAME:	left = '<';  right = '>';  tag = '\0'; break;
	  default:		left = '\0'; right = '\0'; tag = '\0'; break;
	  }
	if (tag) *buffer++ = tag;
	if (left) *buffer++ = left;
	memcpy (buffer, token->val.str.text, token->val.str.len);
	buffer += token->val.str.len;
	if (right) *buffer++ = right;
      }
      break;

    case SPELL_CHAR:
      *buffer++ = token->val.aux;
      break;

    case SPELL_NONE:
      cpp_ice (pfile, "Unspellable token %s", TOKEN_NAME (token));
      break;
    }

  return buffer;
}

/* Macro expansion algorithm.

Macro expansion is implemented by a single-pass algorithm; there are
no rescan passes involved.  cpp_get_token expands just enough to be
able to return a token to the caller, a consequence is that when it
returns the preprocessor can be in a state of mid-expansion.  The
algorithm does not work by fully expanding a macro invocation into
some kind of token list, and then returning them one by one.

Our expansion state is recorded in a context stack.  We start out with
a single context on the stack, let's call it base context.  This
consists of the token list returned by lex_line that forms the next
logical line in the source file.

The current level in the context stack is stored in the cur_context
member of the cpp_reader structure.  The context it references keeps,
amongst other things, a count of how many tokens form that context and
our position within those tokens.

Fundamentally, calling cpp_get_token will return the next token from
the current context.  If we're at the end of the current context, that
context is popped from the stack first, unless it is the base context,
in which case the next logical line is lexed from the source file.

However, before returning the token, if it is a CPP_NAME token
_cpp_get_token checks to see if it is a macro and if it is enabled.
Each time it encounters a macro name, it calls push_macro_context.
This function checks that the macro should be expanded (with
is_macro_enabled), and if so pushes a new macro context on the stack
which becomes the current context.  It then loops back to read the
first token of the macro context.

A macro context basically consists of the token list representing the
macro's replacement list, which was saved in the hash table by
save_macro_expansion when its #define statement was parsed.  If the
macro is function-like, it also contains the tokens that form the
arguments to the macro.  I say more about macro arguments below, but
for now just saying that each argument is a set of pointers to tokens
is enough.

When taking tokens from a macro context, we may get a CPP_MACRO_ARG
token.  This represents an argument passed to the macro, with the
argument number stored in the token's AUX field.  The argument should
be substituted, this is achieved by pushing an "argument context".  An
argument context is just refers to the tokens forming the argument,
which are obtained directly from the macro context.  The STRINGIFY
flag on a CPP_MACRO_ARG token indicates that the argument should be
stringified.

Here's a few simple rules the context stack obeys:-

  1) The lex_line token list is always context zero.

  2) Context 1, if it exists, must be a macro context.

  3) An argument context can only appear above a macro context.

  4) A macro context can appear above the base context, another macro
  context, or an argument context.

  5) These imply that the minimal level of an argument context is 2.

The only tricky thing left is ensuring that macros are enabled and
disabled correctly.  The algorithm controls macro expansion by the
level of the context a token is taken from in the context stack.  If a
token is taken from a level equal to no_expand_level (a member of
struct cpp_reader), no expansion is performed.

When popping a context off the stack, if no_expand_level equals the
level of the popped context, it is reduced by one to match the new
context level, so that expansion is still disabled.  It does not
increase if a context is pushed, though.  It starts out life as
UINT_MAX, which has the effect that initially macro expansion is
enabled.  I explain how this mechanism works below.

The standard requires:-

  1) Arguments to be fully expanded before substitution.

  2) Stringified arguments to not be expanded, nor the tokens
  immediately surrounding a ## operator.

  3) Continual rescanning until there are no more macros left to
  replace.

  4) Once a macro has been expanded in stage 1) or 3), it cannot be
  expanded again during later rescans.  This prevents infinite
  recursion.

The first thing to observe is that stage 3) is mostly redundant.
Since a macro is disabled once it has been expanded, how can a rescan
find an unexpanded macro name?  There are only two cases where this is
possible:-

  a) If the macro name results from a token paste operation.

  b) If the macro in question is a function-like macro that hasn't
  already been expanded because previously there was not the required
  '(' token immediately following it.  This is only possible when an
  argument is substituted, and after substitution the last token of
  the argument can bind with a parenthesis appearing in the tokens
  following the substitution.  Note that if the '(' appears within the
  argument, the ')' must too, as expanding macro arguments cannot
  "suck in" tokens outside the argument.

So we tackle this as follows.  When parsing the macro invocation for
arguments, we record the tokens forming each argument as a list of
pointers to those tokens.  We do not expand any tokens that are "raw",
i.e. directly from the macro invocation, but other tokens that come
from (nested) argument substitution are fully expanded.

This is achieved by setting the no_expand_level to that of the macro
invocation.  A CPP_MACRO_ARG token never appears in the list of tokens
forming an argument, because parse_args (indirectly) calls
get_raw_token which automatically pushes argument contexts and traces
into them.  Since these contexts are at a higher level than the
no_expand_level, they get fully macro expanded.

"Raw" and non-raw tokens are separated in arguments by null pointers,
with the policy that the initial state of an argument is raw.  If the
first token is not raw, it should be preceded by a null pointer.  When
tracing through the tokens of an argument context, each time
get_raw_token encounters a null pointer, it toggles the flag
CONTEXT_RAW.

This flag, when set, indicates to is_macro_disabled that we are
reading raw tokens which should be macro-expanded.  Similarly, if
clear, is_macro_disabled suppresses re-expansion.

It's probably time for an example.

#define hash #
#define str(x) #x
#define xstr(y) str(y hash)
str(hash)			// "hash"
xstr(hash)			// "# hash"

In the invocation of str, parse_args turns off macro expansion and so
parses the argument as <hash>.  This is the only token (pointer)
passed as the argument to str.  Since <hash> is raw there is no need
for an initial null pointer.  stringify_arg is called from
get_raw_token when tracing through the expansion of str, since the
argument has the STRINGIFY flag set.  stringify_arg turns off
macro_expansion by setting the no_expand_level to that of the argument
context.  Thus it gets the token <hash> and stringifies it to "hash"
correctly.

Similary xstr is passed <hash>.  However, when parse_args is parsing
the invocation of str() in xstr's expansion, get_raw_token encounters
a CPP_MACRO_ARG token for y.  Transparently to parse_args, it pushes
an argument context, and enters the tokens of the argument,
i.e. <hash>.  This is at a higher context level than parse_args
disabled, and so is_macro_disabled permits expansion of it and a macro
context is pushed on top of the argument context.  This contains the
<#> token, and the end result is that <hash> is macro expanded.
However, after popping off the argument context, the <hash> of xstr's
expansion does not get macro expanded because we're back at the
no_expand_level.  The end result is that the argument passed to str is
<NULL> <#> <NULL> <hash>.  Note the nulls - policy is we start off
raw, <#> is not raw, but then <hash> is.

*/


/* Free the storage allocated for macro arguments.  */
static void
free_macro_args (args)
     macro_args *args;
{
  if (args->tokens)
    free ((PTR) args->tokens);
  free (args->ends);
  free (args);
}

/* Determines if a macro has been already used (and is therefore
   disabled).  */
static int
is_macro_disabled (pfile, expansion, token)
     cpp_reader *pfile;
     const cpp_toklist *expansion;
     const cpp_token *token;
{
  cpp_context *context = CURRENT_CONTEXT (pfile);

  /* Arguments on either side of ## are inserted in place without
     macro expansion (6.10.3.3.2).  Conceptually, any macro expansion
     occurs during a later rescan pass.  The effect is that we expand
     iff we would as part of the macro's expansion list, so we should
     drop to the macro's context.  */
  if (IS_ARG_CONTEXT (context))
    {
      if (token->flags & PASTED)
	context--;
      else if (!(context->flags & CONTEXT_RAW))
	return 1;
      else if (context->flags & (CONTEXT_PASTEL | CONTEXT_PASTER))
	context--;
    }

  /* Have we already used this macro?  */
  while (context->level > 0)
    {
      if (!IS_ARG_CONTEXT (context) && context->u.list == expansion)
	return 1;
      /* Raw argument tokens are judged based on the token list they
         came from.  */
      if (context->flags & CONTEXT_RAW)
	context = pfile->contexts + context->level;
      else
	context--;
    }

  /* Function-like macros may be disabled if the '(' is not in the
     current context.  We check this without disrupting the context
     stack.  */
  if (expansion->paramc >= 0)
    {
      const cpp_token *next;
      unsigned int prev_nme;

      context = CURRENT_CONTEXT (pfile);
      /* Drop down any contexts we're at the end of: the '(' may
         appear in lower macro expansions, or in the rest of the file.  */
      while (context->posn == context->count && context > pfile->contexts)
	{
	  context--;
	  /* If we matched, we are disabled, as we appear in the
	     expansion of each macro we meet.  */
	  if (!IS_ARG_CONTEXT (context) && context->u.list == expansion)
	    return 1;
	}

      prev_nme = pfile->no_expand_level;
      pfile->no_expand_level = context - pfile->contexts;
      next = _cpp_get_token (pfile);
      restore_macro_expansion (pfile, prev_nme);
      if (next->type != CPP_OPEN_PAREN)
	{
	  _cpp_push_token (pfile, next);
	  if (CPP_WTRADITIONAL (pfile))
	    cpp_warning (pfile,
	 "function macro %s must be used with arguments in traditional C",
			 token->val.node->name);
	  return 1;
	}
    }

  return 0;
}

/* Add a token to the set of tokens forming the arguments to the macro
   being parsed in parse_args.  */
static void
save_token (args, token)
     macro_args *args;
     const cpp_token *token;
{
  if (args->used == args->capacity)
    {
      args->capacity += args->capacity + 100;
      args->tokens = (const cpp_token **)
	xrealloc ((PTR) args->tokens,
		  args->capacity * sizeof (const cpp_token *));
    }
  args->tokens[args->used++] = token;
}

/* Take and save raw tokens until we finish one argument.  Empty
   arguments are saved as a single CPP_PLACEMARKER token.  */
static const cpp_token *
parse_arg (pfile, var_args, paren_context, args, pcount)
     cpp_reader *pfile;
     int var_args;
     unsigned int paren_context;
     macro_args *args;
     unsigned int *pcount;
{
  const cpp_token *token;
  unsigned int paren = 0, count = 0;
  int raw, was_raw = 1;
  
  for (count = 0;; count++)
    {
      token = _cpp_get_token (pfile);

      switch (token->type)
	{
	default:
	  break;

	case CPP_OPEN_PAREN:
	  paren++;
	  break;

	case CPP_CLOSE_PAREN:
	  if (paren-- != 0)
	    break;
	  goto out;

	case CPP_COMMA:
	  /* Commas are not terminators within parantheses or var_args.  */
	  if (paren || var_args)
	    break;
	  goto out;

	case CPP_EOF:		/* Error reported by caller.  */
	  goto out;
	}

      raw = pfile->cur_context <= paren_context;
      if (raw != was_raw)
	{
	  was_raw = raw;
	  save_token (args, 0);
	  count++;
	}
      save_token (args, token);
    }

 out:
  if (count == 0)
    {
      /* Duplicate the placemarker.  Then we can set its flags and
	 position and safely be using more than one.  */
      save_token (args, duplicate_token (pfile, &placemarker_token));
      count++;
    }

  *pcount = count;
  return token;
}

/* This macro returns true if the argument starting at offset O of arglist
   A is empty - that is, it's either a single PLACEMARKER token, or a null
   pointer followed by a PLACEMARKER.  */

#define empty_argument(A, O) \
 ((A)->tokens[O] ? (A)->tokens[O]->type == CPP_PLACEMARKER \
		 : (A)->tokens[(O)+1]->type == CPP_PLACEMARKER)
   
/* Parse the arguments making up a macro invocation.  Nested arguments
   are automatically macro expanded, but immediate macros are not
   expanded; this enables e.g. operator # to work correctly.  Returns
   non-zero on error.  */
static int
parse_args (pfile, hp, args)
     cpp_reader *pfile;
     cpp_hashnode *hp;
     macro_args *args;
{
  const cpp_token *token;
  const cpp_toklist *macro;
  unsigned int total = 0;
  unsigned int paren_context = pfile->cur_context;
  int argc = 0;

  macro = hp->value.expansion;
  do
    {
      unsigned int count;

      token = parse_arg (pfile, (argc + 1 == macro->paramc
				 && (macro->flags & VAR_ARGS)),
			 paren_context, args, &count);
      if (argc < macro->paramc)
	{
	  total += count;
	  args->ends[argc] = total;
	}
      argc++;
    }
  while (token->type != CPP_CLOSE_PAREN && token->type != CPP_EOF);

  if (token->type == CPP_EOF)
    {
      cpp_error(pfile, "unterminated argument list for macro \"%s\"", hp->name);
      return 1;
    }
  else if (argc < macro->paramc)
    {
      /* A rest argument is allowed to not appear in the invocation at all.
	 e.g. #define debug(format, args...) ...
	 debug("string");
	 This is exactly the same as if the rest argument had received no
	 tokens - debug("string",);  This extension is deprecated.  */

      if (argc + 1 == macro->paramc && (macro->flags & VAR_ARGS))
	{
	  /* Duplicate the placemarker.  Then we can set its flags and
             position and safely be using more than one.  */
	  cpp_token *pm = duplicate_token (pfile, &placemarker_token);
	  pm->flags = VOID_REST;
	  save_token (args, pm);
	  args->ends[argc] = total + 1;

	  if (CPP_OPTION (pfile, c99) && CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "ISO C99 requires rest arguments to be used");

	  return 0;
	}
      else
	{
	  cpp_error (pfile, "not enough arguments for macro \"%s\"", hp->name);
	  return 1;
	}
    }
  /* An empty argument to an empty function-like macro is fine.  */
  else if (argc > macro->paramc
	   && !(macro->paramc == 0 && argc == 1 && empty_argument (args, 0)))
    {
      cpp_error (pfile, "too many arguments for macro \"%s\"", hp->name);
      return 1;
    }

  return 0;
}

/* Adds backslashes before all backslashes and double quotes appearing
   in strings.  Non-printable characters are converted to octal.  */
static U_CHAR *
quote_string (dest, src, len)
     U_CHAR *dest;
     const U_CHAR *src;
     unsigned int len;
{
  while (len--)
    {
      U_CHAR c = *src++;

      if (c == '\\' || c == '"')
	{
	  *dest++ = '\\';
	  *dest++ = c;
	}
      else
	{
	  if (ISPRINT (c))
	    *dest++ = c;
	  else
	    {
	      sprintf ((char *) dest, "\\%03o", c);
	      dest += 4;
	    }
	}
    }

  return dest;
}

/* Allocates a buffer to hold a token's TEXT, and converts TOKEN to a
   CPP_STRING token containing TEXT in quoted form.  */
static cpp_token *
make_string_token (token, text, len)
     cpp_token *token;
     const U_CHAR *text;
     unsigned int len;
{
  U_CHAR *buf;
 
  buf = (U_CHAR *) xmalloc (len * 4);
  token->type = CPP_STRING;
  token->flags = 0;
  token->val.str.text = buf;
  token->val.str.len = quote_string (buf, text, len) - buf;
  return token;
}

/* Allocates and converts a temporary token to a CPP_NUMBER token,
   evaluating to NUMBER.  */
static cpp_token *
alloc_number_token (pfile, number)
     cpp_reader *pfile;
     int number;
{
  cpp_token *result;
  char *buf;

  result = get_temp_token (pfile);
  buf = xmalloc (20);
  sprintf (buf, "%d", number);

  result->type = CPP_NUMBER;
  result->flags = 0;
  result->val.str.text = (U_CHAR *) buf;
  result->val.str.len = strlen (buf);
  return result;
}

/* Returns a temporary token from the temporary token store of PFILE.  */
static cpp_token *
get_temp_token (pfile)
     cpp_reader *pfile;
{
  if (pfile->temp_used == pfile->temp_alloced)
    {
      if (pfile->temp_used == pfile->temp_cap)
	{
	  pfile->temp_cap += pfile->temp_cap + 20;
	  pfile->temp_tokens = (cpp_token **) xrealloc
	    (pfile->temp_tokens, pfile->temp_cap * sizeof (cpp_token *));
	}
      pfile->temp_tokens[pfile->temp_alloced++] = (cpp_token *) xmalloc
	(sizeof (cpp_token));
    }

  return pfile->temp_tokens[pfile->temp_used++];
}

/* Release (not free) for re-use the temporary tokens of PFILE.  */
static void
release_temp_tokens (pfile)
     cpp_reader *pfile;
{
  while (pfile->temp_used)
    {
      cpp_token *token = pfile->temp_tokens[--pfile->temp_used];

      if (TOKEN_SPELL (token) == SPELL_STRING)
	{
	  free ((char *) token->val.str.text);
	  token->val.str.text = 0;
	}
    }
}

/* Free all of PFILE's dynamically-allocated temporary tokens.  */
void
_cpp_free_temp_tokens (pfile)
     cpp_reader *pfile;
{
  if (pfile->temp_tokens)
    {
      /* It is possible, though unlikely (looking for '(' of a funlike
	 macro into EOF), that we haven't released the tokens yet.  */
      release_temp_tokens (pfile);
      while (pfile->temp_alloced)
	free (pfile->temp_tokens[--pfile->temp_alloced]);
      free (pfile->temp_tokens);
    }

  if (pfile->date)
    {
      free ((char *) pfile->date->val.str.text);
      free (pfile->date);
      free ((char *) pfile->time->val.str.text);
      free (pfile->time);
    }
}

/* Copy TOKEN into a temporary token from PFILE's store.  */
static cpp_token *
duplicate_token (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  cpp_token *result = get_temp_token (pfile);

  *result = *token;
  if (TOKEN_SPELL (token) == SPELL_STRING)
    {
      U_CHAR *buff = (U_CHAR *) xmalloc (token->val.str.len);
      memcpy (buff, token->val.str.text, token->val.str.len);
      result->val.str.text = buff;
    }
  return result;
}

/* Determine whether two tokens can be pasted together, and if so,
   what the resulting token is.  Returns CPP_EOF if the tokens cannot
   be pasted, or the appropriate type for the merged token if they
   can.  */
static enum cpp_ttype
can_paste (pfile, token1, token2, digraph)
     cpp_reader * pfile;
     const cpp_token *token1, *token2;
     int* digraph;
{
  enum cpp_ttype a = token1->type, b = token2->type;
  int cxx = CPP_OPTION (pfile, cplusplus);

  /* Treat named operators as if they were ordinary NAMEs.  */
  if (token1->flags & NAMED_OP)
    a = CPP_NAME;
  if (token2->flags & NAMED_OP)
    b = CPP_NAME;

  if (a <= CPP_LAST_EQ && b == CPP_EQ)
    return a + (CPP_EQ_EQ - CPP_EQ);

  switch (a)
    {
    case CPP_GREATER:
      if (b == a) return CPP_RSHIFT;
      if (b == CPP_QUERY && cxx)	return CPP_MAX;
      if (b == CPP_GREATER_EQ)	return CPP_RSHIFT_EQ;
      break;
    case CPP_LESS:
      if (b == a) return CPP_LSHIFT;
      if (b == CPP_QUERY && cxx)	return CPP_MIN;
      if (b == CPP_LESS_EQ)	return CPP_LSHIFT_EQ;
      if (CPP_OPTION (pfile, digraphs))
	{
	  if (b == CPP_COLON)
	    {*digraph = 1; return CPP_OPEN_SQUARE;} /* <: digraph */
	  if (b == CPP_MOD)
	    {*digraph = 1; return CPP_OPEN_BRACE;}	/* <% digraph */
	}
      break;

    case CPP_PLUS: if (b == a)	return CPP_PLUS_PLUS; break;
    case CPP_AND:  if (b == a)	return CPP_AND_AND; break;
    case CPP_OR:   if (b == a)	return CPP_OR_OR;   break;

    case CPP_MINUS:
      if (b == a)		return CPP_MINUS_MINUS;
      if (b == CPP_GREATER)	return CPP_DEREF;
      break;
    case CPP_COLON:
      if (b == a && cxx)	return CPP_SCOPE;
      if (b == CPP_GREATER && CPP_OPTION (pfile, digraphs))
	{*digraph = 1; return CPP_CLOSE_SQUARE;} /* :> digraph */
      break;

    case CPP_MOD:
      if (CPP_OPTION (pfile, digraphs))
	{
	  if (b == CPP_GREATER)
	    {*digraph = 1; return CPP_CLOSE_BRACE;}  /* %> digraph */
	  if (b == CPP_COLON)
	    {*digraph = 1; return CPP_HASH;}         /* %: digraph */
	}
      break;
    case CPP_DEREF:
      if (b == CPP_MULT && cxx)	return CPP_DEREF_STAR;
      break;
    case CPP_DOT:
      if (b == CPP_MULT && cxx)	return CPP_DOT_STAR;
      if (b == CPP_NUMBER)	return CPP_NUMBER;
      break;

    case CPP_HASH:
      if (b == a && (token1->flags & DIGRAPH) == (token2->flags & DIGRAPH))
	/* %:%: digraph */
	{*digraph = (token1->flags & DIGRAPH); return CPP_PASTE;}
      break;

    case CPP_NAME:
      if (b == CPP_NAME)	return CPP_NAME;
      if (b == CPP_NUMBER
	  && is_numstart(token2->val.str.text[0]))	 return CPP_NAME;
      if (b == CPP_CHAR
	  && token1->val.node == pfile->spec_nodes->n_L) return CPP_WCHAR;
      if (b == CPP_STRING
	  && token1->val.node == pfile->spec_nodes->n_L) return CPP_WSTRING;
      break;

    case CPP_NUMBER:
      if (b == CPP_NUMBER)	return CPP_NUMBER;
      if (b == CPP_NAME)	return CPP_NUMBER;
      if (b == CPP_DOT)		return CPP_NUMBER;
      /* Numbers cannot have length zero, so this is safe.  */
      if ((b == CPP_PLUS || b == CPP_MINUS)
	  && VALID_SIGN ('+', token1->val.str.text[token1->val.str.len - 1]))
	return CPP_NUMBER;
      break;

    case CPP_OTHER:
      if (CPP_OPTION (pfile, objc) && token1->val.aux == '@')
	{
	  if (b == CPP_NAME)	return CPP_NAME;
	  if (b == CPP_STRING)	return CPP_OSTRING;
	}

    default:
      break;
    }

  return CPP_EOF;
}

/* Check if TOKEN is to be ##-pasted with the token after it.  */
static const cpp_token *
maybe_paste_with_next (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  cpp_token *pasted;
  const cpp_token *second;
  cpp_context *context = CURRENT_CONTEXT (pfile);

  /* Is this token on the LHS of ## ? */

  while ((token->flags & PASTE_LEFT)
	 || ((context->flags & CONTEXT_PASTEL)
	     && context->posn == context->count))
    {
      /* Suppress macro expansion for next token, but don't conflict
	 with the other method of suppression.  If it is an argument,
	 macro expansion within the argument will still occur.  */
      pfile->paste_level = pfile->cur_context;
      second = _cpp_get_token (pfile);
      pfile->paste_level = 0;

      /* Ignore placemarker argument tokens (cannot be from an empty
	 macro since macros are not expanded).  */
      if (token->type == CPP_PLACEMARKER)
	pasted = duplicate_token (pfile, second);
      else if (second->type == CPP_PLACEMARKER)
	{
	  /* GCC has special extended semantics for , ## b where b is
	     a varargs parameter: the comma disappears if b was given
	     no actual arguments (not merely if b is an empty
	     argument).  */
	  if (token->type == CPP_COMMA && second->flags & VOID_REST)
	    pasted = duplicate_token (pfile, second);
	  else
	    pasted = duplicate_token (pfile, token);
	}
      else
	{
	  int digraph = 0;
	  enum cpp_ttype type = can_paste (pfile, token, second, &digraph);

	  if (type == CPP_EOF)
	    {
	      if (CPP_OPTION (pfile, warn_paste))
		{
		  /* Do not complain about , ## <whatever> if
		     <whatever> came from a variable argument, because
		     the author probably intended the ## to trigger
		     the special extended semantics (see above).  */
		  if (token->type == CPP_COMMA
		      && IS_ARG_CONTEXT (CURRENT_CONTEXT (pfile))
		      && ON_REST_ARG (CURRENT_CONTEXT (pfile) - 1))
		    /* no warning */;
		  else
		    cpp_warning (pfile,
			"pasting would not give a valid preprocessing token");
		}
	      _cpp_push_token (pfile, second);
	      /* A short term hack to safely clear the PASTE_LEFT flag.  */
	      pasted = duplicate_token (pfile, token);
	      pasted->flags &= ~PASTE_LEFT;
	      return pasted;
	    }

	  if (type == CPP_NAME || type == CPP_NUMBER)
	    {
	      /* Join spellings.  */
	      U_CHAR *buf, *end;

	      pasted = get_temp_token (pfile);
	      buf = (U_CHAR *) alloca (TOKEN_LEN (token) + TOKEN_LEN (second));
	      end = spell_token (pfile, token, buf);
	      end = spell_token (pfile, second, end);
	      *end = '\0';

	      if (type == CPP_NAME)
		pasted->val.node = cpp_lookup (pfile, buf, end - buf);
	      else
		{
		  pasted->val.str.text = uxstrdup (buf);
		  pasted->val.str.len = end - buf;
		}
	    }
	  else if (type == CPP_WCHAR || type == CPP_WSTRING
		   || type == CPP_OSTRING)
	    pasted = duplicate_token (pfile, second);
	  else
	    {
	      pasted = get_temp_token (pfile);
	      pasted->val.integer = 0;
	    }

	  pasted->type = type;
	  pasted->flags = digraph ? DIGRAPH : 0;

	  if (type == CPP_NAME && pasted->val.node->type == T_OPERATOR)
	    {
	      pasted->type = pasted->val.node->value.code;
	      pasted->flags |= NAMED_OP;
	    }
	}

      /* The pasted token gets the whitespace flags and position of the
	 first token, the PASTE_LEFT flag of the second token, plus the
	 PASTED flag to indicate it is the result of a paste.  However, we
	 want to preserve the DIGRAPH flag.  */
      pasted->flags &= ~(PREV_WHITE | BOL | PASTE_LEFT);
      pasted->flags |= ((token->flags & (PREV_WHITE | BOL))
			| (second->flags & PASTE_LEFT) | PASTED);
      pasted->col = token->col;
      pasted->line = token->line;

      /* See if there is another token to be pasted onto the one we just
	 constructed.  */
      token = pasted;
      context = CURRENT_CONTEXT (pfile);
      /* and loop */
    }
  return token;
}

/* Convert a token sequence to a single string token according to the
   rules of the ISO C #-operator.  */
#define INIT_SIZE 200
static cpp_token *
stringify_arg (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  cpp_token *result;
  unsigned char *main_buf;
  unsigned int prev_value, backslash_count = 0;
  unsigned int buf_used = 0, whitespace = 0, buf_cap = INIT_SIZE;

  push_arg_context (pfile, token);
  prev_value  = prevent_macro_expansion (pfile);
  main_buf = (unsigned char *) xmalloc (buf_cap);

  result = get_temp_token (pfile);
  ASSIGN_FLAGS_AND_POS (result, token);

  for (; (token = _cpp_get_token (pfile))->type != CPP_EOF; )
    {
      int escape;
      unsigned char *buf;
      unsigned int len = TOKEN_LEN (token);

      if (token->type == CPP_PLACEMARKER)
	continue;

      escape = (token->type == CPP_STRING || token->type == CPP_WSTRING
		|| token->type == CPP_CHAR || token->type == CPP_WCHAR);
      if (escape)
	len *= 4 + 1;

      if (buf_used + len > buf_cap)
	{
	  buf_cap = buf_used + len + INIT_SIZE;
	  main_buf = xrealloc (main_buf, buf_cap);
	}

      if (whitespace && (token->flags & PREV_WHITE))
	main_buf[buf_used++] = ' ';

      if (escape)
	buf = (unsigned char *) xmalloc (len);
      else
	buf = main_buf + buf_used;
      
      len = spell_token (pfile, token, buf) - buf;
      if (escape)
	{
	  buf_used = quote_string (&main_buf[buf_used], buf, len) - main_buf;
	  free (buf);
	}
      else
	buf_used += len;

      whitespace = 1;
      if (token->type == CPP_BACKSLASH)
	backslash_count++;
      else
	backslash_count = 0;
    }

  /* Ignore the final \ of invalid string literals.  */
  if (backslash_count & 1)
    {
      cpp_warning (pfile, "invalid string literal, ignoring final '\\'");
      buf_used--;
    }

  result->type = CPP_STRING;
  result->val.str.text = main_buf;
  result->val.str.len = buf_used;
  restore_macro_expansion (pfile, prev_value);
  return result;
}

/* Allocate more room on the context stack of PFILE.  */
static void
expand_context_stack (pfile)
     cpp_reader *pfile;
{
  pfile->context_cap += pfile->context_cap + 20;
  pfile->contexts = (cpp_context *)
    xrealloc (pfile->contexts, pfile->context_cap * sizeof (cpp_context));
}

/* Push the context of macro NODE onto the context stack.  TOKEN is
   the CPP_NAME token invoking the macro.  */
static int
push_macro_context (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  unsigned char orig_flags;
  macro_args *args;
  cpp_context *context;
  cpp_hashnode *node = token->val.node;

  /* Token's flags may change when parsing args containing a nested
     invocation of this macro.  */
  orig_flags = token->flags & (PREV_WHITE | BOL);
  args = 0;
  if (node->value.expansion->paramc >= 0)
    {
      unsigned int error, prev_nme;

      /* Allocate room for the argument contexts, and parse them.  */
      args  = (macro_args *) xmalloc (sizeof (macro_args));
      args->ends = (unsigned int *)
	xmalloc (node->value.expansion->paramc * sizeof (unsigned int));
      args->tokens = 0;
      args->capacity = 0;
      args->used = 0;

      prev_nme = prevent_macro_expansion (pfile);
      pfile->args = args;
      error = parse_args (pfile, node, args);
      pfile->args = 0;
      restore_macro_expansion (pfile, prev_nme);
      if (error)
	{
	  free_macro_args (args);
	  return 1;
	}
      /* Set the level after the call to parse_args.  */
      args->level = pfile->cur_context;
    }

  /* Now push its context.  */
  pfile->cur_context++;
  if (pfile->cur_context == pfile->context_cap)
    expand_context_stack (pfile);

  context = CURRENT_CONTEXT (pfile);
  context->u.list = node->value.expansion;
  context->args = args;
  context->posn = 0;
  context->count = context->u.list->tokens_used;
  context->level = pfile->cur_context;
  context->flags = 0;
  context->pushed_token = 0;

  /* Set the flags of the first token.  We know there must
     be one, empty macros are a single placemarker token.  */
  MODIFY_FLAGS_AND_POS (&context->u.list->tokens[0], token, orig_flags);

  return 0;
}

/* Push an argument to the current macro onto the context stack.
   TOKEN is the MACRO_ARG token representing the argument expansion.  */
static void
push_arg_context (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  cpp_context *context;
  macro_args *args;

  pfile->cur_context++;
  if (pfile->cur_context == pfile->context_cap)
      expand_context_stack (pfile);

  context = CURRENT_CONTEXT (pfile);
  args = context[-1].args;

  context->count = token->val.aux ? args->ends[token->val.aux - 1]: 0;
  context->u.arg = args->tokens + context->count;
  context->count = args->ends[token->val.aux] - context->count;
  context->args = 0;
  context->posn = 0;
  context->level = args->level;
  context->flags = CONTEXT_ARG | CONTEXT_RAW;
  context->pushed_token = 0;

  /* Set the flags of the first token.  There is one.  */
  {
    const cpp_token *first = context->u.arg[0];
    if (!first)
      first = context->u.arg[1];

    MODIFY_FLAGS_AND_POS ((cpp_token *) first, token,
			  token->flags & (PREV_WHITE | BOL));
  }

  if (token->flags & PASTE_LEFT)
    context->flags |= CONTEXT_PASTEL;
  if (pfile->paste_level)
    context->flags |= CONTEXT_PASTER;
}

/* "Unget" a token.  It is effectively inserted in the token queue and
   will be returned by the next call to get_raw_token.  */
void
_cpp_push_token (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  cpp_context *context = CURRENT_CONTEXT (pfile);

  if (context->posn > 0)
    {
      const cpp_token *prev;
      if (IS_ARG_CONTEXT (context))
	prev = context->u.arg[context->posn - 1];
      else
	prev = &context->u.list->tokens[context->posn - 1];

      if (prev == token)
	{
	  context->posn--;
	  return;
	}
    }

  if (context->pushed_token)
    cpp_ice (pfile, "two tokens pushed in a row");
  if (token->type != CPP_EOF)
    context->pushed_token = token;
  /* Don't push back a directive's CPP_EOF, step back instead.  */
  else if (pfile->cur_context == 0)
    pfile->contexts[0].posn--;
}

/* Handle a preprocessing directive.  TOKEN is the CPP_HASH token
   introducing the directive.  */
static void
process_directive (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  const struct directive *d = pfile->token_list.directive;
  int prev_nme = 0;

  /* Skip over the directive name.  */
  if (token[1].type == CPP_NAME)
    _cpp_get_raw_token (pfile);
  else if (token[1].type != CPP_NUMBER)
    cpp_ice (pfile, "directive begins with %s?!", TOKEN_NAME (token));

  if (! (d->flags & EXPAND))
    prev_nme = prevent_macro_expansion (pfile);
  (void) (*d->handler) (pfile);
  if (! (d->flags & EXPAND))
    restore_macro_expansion (pfile, prev_nme);
  _cpp_skip_rest_of_line (pfile);
}

/* The external interface to return the next token.  All macro
   expansion and directive processing is handled internally, the
   caller only ever sees the output after preprocessing.  */
const cpp_token *
cpp_get_token (pfile)
     cpp_reader *pfile;
{
  const cpp_token *token;
  /* Loop till we hit a non-directive, non-placemarker token.  */
  for (;;)
    {
      token = _cpp_get_token (pfile);

      if (token->type == CPP_PLACEMARKER)
	continue;

      if (token->type == CPP_HASH && token->flags & BOL
	  && pfile->token_list.directive)
	{
	  process_directive (pfile, token);
	  continue;
	}

      return token;
    }
}

/* The internal interface to return the next token.  There are two
   differences between the internal and external interfaces: the
   internal interface may return a PLACEMARKER token, and it does not
   process directives.  */
const cpp_token *
_cpp_get_token (pfile)
     cpp_reader *pfile;
{
  const cpp_token *token, *old_token;
  cpp_hashnode *node;

  /* Loop until we hit a non-macro token.  */
  for (;;)
    {
      token = get_raw_token (pfile);

      /* Short circuit EOF. */
      if (token->type == CPP_EOF)
	return token;

      /* If we are skipping... */
      if (pfile->skipping)
	{
	  /* we still have to process directives,  */
	  if (pfile->token_list.directive)
	    return token;

	  /* but everything else is ignored.  */
	  _cpp_skip_rest_of_line (pfile);
	  continue;
	}

      /* If there's a potential control macro and we get here, then that
	 #ifndef didn't cover the entire file and its argument shouldn't
	 be taken as a control macro.  */
      pfile->potential_control_macro = 0;

      /* If we are rescanning preprocessed input, no macro expansion or
	 token pasting may occur.  */
      if (CPP_OPTION (pfile, preprocessed))
	return token;

      old_token = token;

      /* See if there's a token to paste with this one.  */
      if (!pfile->paste_level)
	token = maybe_paste_with_next (pfile, token);

      /* If it isn't a macro, return it now.  */
      if (token->type != CPP_NAME || token->val.node->type == T_VOID)
	return token;

      /* Is macro expansion disabled in general, or are we in the
	 middle of a token paste, or was this token just pasted?
	 (Note we don't check token->flags & PASTED, because that
	 counts tokens that were pasted at some point in the past,
	 we're only interested in tokens that were pasted by this call
	 to maybe_paste_with_next.)  */
      if (pfile->no_expand_level == pfile->cur_context
	  || pfile->paste_level
	  || (token != old_token
	      && pfile->no_expand_level + 1 == pfile->cur_context))
	return token;

      node = token->val.node;
      if (node->type != T_MACRO)
	return special_symbol (pfile, node, token);

      if (is_macro_disabled (pfile, node->value.expansion, token))
	return token;

      if (push_macro_context (pfile, token))
	return token;
      /* else loop */
    }
}

/* Returns the next raw token, i.e. without performing macro
   expansion.  Argument contexts are automatically entered.  */
static const cpp_token *
get_raw_token (pfile)
     cpp_reader *pfile;
{
  const cpp_token *result;
  cpp_context *context;

  for (;;)
    {
      context = CURRENT_CONTEXT (pfile);
      if (context->pushed_token)
	{
	  result = context->pushed_token;
	  context->pushed_token = 0;
	  return result;	/* Cannot be a CPP_MACRO_ARG */
	}
      else if (context->posn == context->count)
	{
	  if (pop_context (pfile))
	    return &eof_token;
	  continue;
	}
      else if (IS_ARG_CONTEXT (context))
	{
	  result = context->u.arg[context->posn++];
	  if (result == 0)
	    {
	      context->flags ^= CONTEXT_RAW;
	      result = context->u.arg[context->posn++];
	    }
	  return result;	/* Cannot be a CPP_MACRO_ARG */
	}

      result = &context->u.list->tokens[context->posn++];

      if (result->type != CPP_MACRO_ARG)
	return result;

      if (result->flags & STRINGIFY_ARG)
	return stringify_arg (pfile, result);

      push_arg_context (pfile, result);
    }
}

/* Internal interface to get the token without macro expanding.  */
const cpp_token *
_cpp_get_raw_token (pfile)
     cpp_reader *pfile;
{
  int prev_nme = prevent_macro_expansion (pfile);
  const cpp_token *result = _cpp_get_token (pfile);
  restore_macro_expansion (pfile, prev_nme);
  return result;
}

/* A thin wrapper to lex_line.  CLEAR is non-zero if the current token
   list should be overwritten, or zero if we need to append
   (typically, if we are within the arguments to a macro, or looking
   for the '(' to start a function-like macro invocation).  */
static int
lex_next (pfile, clear)
     cpp_reader *pfile;
     int clear;
{
  cpp_toklist *list = &pfile->token_list;
  const cpp_token *old_list = list->tokens;
  unsigned int old_used = list->tokens_used;

  if (clear)
    {
      /* Release all temporary tokens.  */
      _cpp_clear_toklist (list);
      pfile->contexts[0].posn = 0;
      if (pfile->temp_used)
	release_temp_tokens (pfile);
    }
  lex_line (pfile, list);
  pfile->contexts[0].count = list->tokens_used;

  if (!clear && pfile->args)
    {
      /* Fix up argument token pointers.  */
      if (old_list != list->tokens)
	{
	  unsigned int i;

	  for (i = 0; i < pfile->args->used; i++)
	    {
	      const cpp_token *token = pfile->args->tokens[i];
	      if (token >= old_list && token < old_list + old_used)
		pfile->args->tokens[i] = (const cpp_token *)
	        ((char *) token + ((char *) list->tokens - (char *) old_list));
	    }
	}

      /* 6.10.3 paragraph 11: If there are sequences of preprocessing
	 tokens within the list of arguments that would otherwise act as
	 preprocessing directives, the behavior is undefined.

	 This implementation will report a hard error and treat the
	 'sequence of preprocessing tokens' as part of the macro argument,
	 not a directive.  

         Note if pfile->args == 0, we're OK since we're only inside a
         macro argument after a '('.  */
      if (list->directive)
	{
	  cpp_error_with_line (pfile, list->tokens[old_used].line,
			       list->tokens[old_used].col,
			       "#%s may not be used inside a macro argument",
			       list->directive->name);
	  return 1;
	}
    }

  return 0;
}

/* Pops a context off the context stack.  If we're at the bottom, lexes
   the next logical line.  Returns EOF if we're at the end of the
   argument list to the # operator, or we should not "overflow"
   into the rest of the file (e.g. 6.10.3.1.1).  */
static int
pop_context (pfile)
     cpp_reader *pfile;
{
  cpp_context *context;

  if (pfile->cur_context == 0)
    {
      /* If we are currently processing a directive, do not advance.  6.10
	 paragraph 2: A new-line character ends the directive even if it
	 occurs within what would otherwise be an invocation of a
	 function-like macro.  */
      if (pfile->token_list.directive)
	return 1;

      return lex_next (pfile, pfile->no_expand_level == UINT_MAX);
    }

  /* Argument contexts, when parsing args or handling # operator
     return CPP_EOF at the end.  */
  context = CURRENT_CONTEXT (pfile);
  if (IS_ARG_CONTEXT (context) && pfile->cur_context == pfile->no_expand_level)
    return 1;

  /* Free resources when leaving macro contexts.  */
  if (context->args)
    free_macro_args (context->args);

  if (pfile->cur_context == pfile->no_expand_level)
    pfile->no_expand_level--;
  pfile->cur_context--;

  return 0;
}

/* Turn off macro expansion at the current context level.  */
static unsigned int
prevent_macro_expansion (pfile)
     cpp_reader *pfile;
{
  unsigned int prev_value = pfile->no_expand_level;
  pfile->no_expand_level = pfile->cur_context;
  return prev_value;
}

/* Restore macro expansion to its previous state.  */
static void
restore_macro_expansion (pfile, prev_value)
     cpp_reader *pfile;
     unsigned int prev_value;
{
  pfile->no_expand_level = prev_value;
}

/* Used by cpperror.c to obtain the correct line and column to report
   in a diagnostic.  */
unsigned int
_cpp_get_line (pfile, pcol)
     cpp_reader *pfile;
     unsigned int *pcol;
{
  unsigned int index;
  const cpp_token *cur_token;

  if (pfile->in_lex_line)
    index = pfile->token_list.tokens_used;
  else
    index = pfile->contexts[0].posn;

  if (index == 0)
    {
      if (pcol)
	*pcol = 0;
      return 0;
    }

  cur_token = &pfile->token_list.tokens[index - 1];
  if (pcol)
    *pcol = cur_token->col;
  return cur_token->line;
}

#define DSC(str) (const U_CHAR *)str, sizeof str - 1
static const char * const monthnames[] =
{
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};

/* Handle builtin macros like __FILE__.  */
static const cpp_token *
special_symbol (pfile, node, token)
     cpp_reader *pfile;
     cpp_hashnode *node;
     const cpp_token *token;
{
  cpp_token *result;
  cpp_buffer *ip;

  switch (node->type)
    {
    case T_FILE:
    case T_BASE_FILE:
      {
	const char *file;

	ip = CPP_BUFFER (pfile);
	if (ip == 0)
	  file = "";
	else
	  {
	    if (node->type == T_BASE_FILE)
	      while (CPP_PREV_BUFFER (ip) != NULL)
		ip = CPP_PREV_BUFFER (ip);

	    file = ip->nominal_fname;
	  }
	result = make_string_token (get_temp_token (pfile), (U_CHAR *) file,
				    strlen (file));
      }
      break;
	
    case T_INCLUDE_LEVEL:
      /* pfile->include_depth counts the primary source as level 1,
	 but historically __INCLUDE_DEPTH__ has called the primary
	 source level 0.  */
      result = alloc_number_token (pfile, pfile->include_depth - 1);
      break;

    case T_SPECLINE:
      /* If __LINE__ is embedded in a macro, it must expand to the
	 line of the macro's invocation, not its definition.
	 Otherwise things like assert() will not work properly.  */
      result = alloc_number_token (pfile, _cpp_get_line (pfile, NULL));
      break;

    case T_STDC:
      {
	int stdc = 1;

#ifdef STDC_0_IN_SYSTEM_HEADERS
	if (CPP_IN_SYSTEM_HEADER (pfile)
	    && pfile->spec_nodes->n__STRICT_ANSI__->type == T_VOID)
	  stdc = 0;
#endif
	result = alloc_number_token (pfile, stdc);
      }
      break;

    case T_DATE:
    case T_TIME:
      if (pfile->date == 0)
	{
	  /* Allocate __DATE__ and __TIME__ from permanent storage,
	     and save them in pfile so we don't have to do this again.
	     We don't generate these strings at init time because
	     time() and localtime() are very slow on some systems.  */
	  time_t tt = time (NULL);
	  struct tm *tb = localtime (&tt);

	  pfile->date = make_string_token
	    ((cpp_token *) xmalloc (sizeof (cpp_token)), DSC("Oct 11 1347"));
	  pfile->time = make_string_token
	    ((cpp_token *) xmalloc (sizeof (cpp_token)), DSC("12:34:56"));

	  sprintf ((char *) pfile->date->val.str.text, "%s %2d %4d",
		   monthnames[tb->tm_mon], tb->tm_mday, tb->tm_year + 1900);
	  sprintf ((char *) pfile->time->val.str.text, "%02d:%02d:%02d",
		   tb->tm_hour, tb->tm_min, tb->tm_sec);
	}
      result = node->type == T_DATE ? pfile->date: pfile->time;
      break;

    case T_POISON:
      cpp_error (pfile, "attempt to use poisoned \"%s\"", node->name);
      return token;

    default:
      cpp_ice (pfile, "invalid special hash type");
      return token;
    }

  ASSIGN_FLAGS_AND_POS (result, token);
  return result;
}
#undef DSC

/* Allocate pfile->input_buffer, and initialize _cpp_trigraph_map[]
   if it hasn't happened already.  */

void
_cpp_init_input_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_context *base;

  _cpp_init_toklist (&pfile->token_list, DUMMY_TOKEN);
  pfile->no_expand_level = UINT_MAX;
  pfile->context_cap = 20;
  pfile->cur_context = 0;

  pfile->contexts = (cpp_context *)
    xmalloc (pfile->context_cap * sizeof (cpp_context));

  /* Clear the base context.  */
  base = &pfile->contexts[0];
  base->u.list = &pfile->token_list;
  base->posn = 0;
  base->count = 0;
  base->args = 0;
  base->level = 0;
  base->flags = 0;
  base->pushed_token = 0;
}

/* Moves to the end of the directive line, popping contexts as
   necessary.  */
void
_cpp_skip_rest_of_line (pfile)
     cpp_reader *pfile;
{
  /* Discard all stacked contexts.  */
  int i;
  for (i = pfile->cur_context; i > 0; i--)
    if (pfile->contexts[i].args)
      free_macro_args (pfile->contexts[i].args);

  if (pfile->no_expand_level <= pfile->cur_context)
    pfile->no_expand_level = 0;
  pfile->cur_context = 0;

  /* Clear the base context, and clear the directive pointer so that
     get_raw_token will advance to the next line.  */
  pfile->contexts[0].count = 0;
  pfile->contexts[0].posn = 0;
  pfile->token_list.directive = 0;
}

/* Directive handler wrapper used by the command line option
   processor.  */
void
_cpp_run_directive (pfile, dir, buf, count, name)
     cpp_reader *pfile;
     const struct directive *dir;
     const char *buf;
     size_t count;
     const char *name;
{
  if (cpp_push_buffer (pfile, (const U_CHAR *)buf, count) != NULL)
    {
      unsigned int prev_lvl = 0;

      if (name)
	CPP_BUFFER (pfile)->nominal_fname = name;
      else
	CPP_BUFFER (pfile)->nominal_fname = _("<command line>");
      CPP_BUFFER (pfile)->lineno = (unsigned int)-1;

      /* Scan the line now, else prevent_macro_expansion won't work.  */
      lex_next (pfile, 1);
      if (! (dir->flags & EXPAND))
	prev_lvl = prevent_macro_expansion (pfile);

      (void) (*dir->handler) (pfile);

      if (! (dir->flags & EXPAND))
	restore_macro_expansion (pfile, prev_lvl);
      
      _cpp_skip_rest_of_line (pfile);
      cpp_pop_buffer (pfile);
    }
}
