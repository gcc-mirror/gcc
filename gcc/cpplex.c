/* CPP Library - lexical analysis.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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
#include "cpplib.h"
#include "cpphash.h"

/* MULTIBYTE_CHARS support only works for native compilers.
   ??? Ideally what we want is to model widechar support after
   the current floating point support.  */
#ifdef CROSS_COMPILE
#undef MULTIBYTE_CHARS
#endif

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif

/* Tokens with SPELL_STRING store their spelling in the token list,
   and it's length in the token->val.name.len.  */
enum spell_type
{
  SPELL_OPERATOR = 0,
  SPELL_CHAR,
  SPELL_IDENT,
  SPELL_NUMBER,
  SPELL_STRING,
  SPELL_NONE
};

struct token_spelling
{
  enum spell_type category;
  const unsigned char *name;
};

static const unsigned char *const digraph_spellings[] =
{ U"%:", U"%:%:", U"<:", U":>", U"<%", U"%>" };

#define OP(e, s) { SPELL_OPERATOR, U s           },
#define TK(e, s) { s,              U STRINGX (e) },
static const struct token_spelling token_spellings[N_TTYPES] = { TTYPE_TABLE };
#undef OP
#undef TK

#define TOKEN_SPELL(token) (token_spellings[(token)->type].category)
#define TOKEN_NAME(token) (token_spellings[(token)->type].name)
#define BACKUP() do {buffer->cur = buffer->backup_to;} while (0)

static void handle_newline PARAMS ((cpp_reader *));
static cppchar_t skip_escaped_newlines PARAMS ((cpp_reader *));
static cppchar_t get_effective_char PARAMS ((cpp_reader *));

static int skip_block_comment PARAMS ((cpp_reader *));
static int skip_line_comment PARAMS ((cpp_reader *));
static void adjust_column PARAMS ((cpp_reader *));
static int skip_whitespace PARAMS ((cpp_reader *, cppchar_t));
static cpp_hashnode *parse_identifier PARAMS ((cpp_reader *));
static cpp_hashnode *parse_identifier_slow PARAMS ((cpp_reader *,
						    const U_CHAR *));
static void parse_number PARAMS ((cpp_reader *, cpp_string *, cppchar_t, int));
static int unescaped_terminator_p PARAMS ((cpp_reader *, const U_CHAR *));
static void parse_string PARAMS ((cpp_reader *, cpp_token *, cppchar_t));
static void unterminated PARAMS ((cpp_reader *, int));
static bool trigraph_p PARAMS ((cpp_reader *));
static void save_comment PARAMS ((cpp_reader *, cpp_token *, const U_CHAR *));
static int name_p PARAMS ((cpp_reader *, const cpp_string *));
static int maybe_read_ucs PARAMS ((cpp_reader *, const unsigned char **,
				   const unsigned char *, unsigned int *));
static tokenrun *next_tokenrun PARAMS ((tokenrun *));

static unsigned int hex_digit_value PARAMS ((unsigned int));
static _cpp_buff *new_buff PARAMS ((size_t));

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

  return !ustrcmp (NODE_NAME (token->val.node), (const U_CHAR *) string);
}

/* Call when meeting a newline, assumed to be in buffer->cur[-1].
   Returns with buffer->cur pointing to the character immediately
   following the newline (combination).  */
static void
handle_newline (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;

  /* Handle CR-LF and LF-CR.  Most other implementations (e.g. java)
     only accept CR-LF; maybe we should fall back to that behaviour?  */
  if (buffer->cur[-1] + buffer->cur[0] == '\r' + '\n')
    buffer->cur++;

  buffer->line_base = buffer->cur;
  buffer->col_adjust = 0;
  pfile->line++;
}

/* Subroutine of skip_escaped_newlines; called when a 3-character
   sequence beginning with "??" is encountered.  buffer->cur points to
   the second '?'.

   Warn if necessary, and returns true if the sequence forms a
   trigraph and the trigraph should be honoured.  */
static bool
trigraph_p (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  cppchar_t from_char = buffer->cur[1];
  bool accept;

  if (!_cpp_trigraph_map[from_char])
    return false;

  accept = CPP_OPTION (pfile, trigraphs);

  /* Don't warn about trigraphs in comments.  */
  if (CPP_OPTION (pfile, warn_trigraphs) && !pfile->state.lexing_comment)
    {
      if (accept)
	cpp_warning_with_line (pfile, pfile->line, CPP_BUF_COL (buffer) - 1,
			       "trigraph ??%c converted to %c",
			       (int) from_char,
			       (int) _cpp_trigraph_map[from_char]);
      else if (buffer->cur != buffer->last_Wtrigraphs)
	{
	  buffer->last_Wtrigraphs = buffer->cur;
	  cpp_warning_with_line (pfile, pfile->line,
				 CPP_BUF_COL (buffer) - 1,
				 "trigraph ??%c ignored", (int) from_char);
	}
    }

  return accept;
}

/* Skips any escaped newlines introduced by '?' or a '\\', assumed to
   lie in buffer->cur[-1].  Returns the next byte, which will be in
   buffer->cur[-1].  This routine performs preprocessing stages 1 and
   2 of the ISO C standard.  */
static cppchar_t
skip_escaped_newlines (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  cppchar_t next = buffer->cur[-1];

  /* Only do this if we apply stages 1 and 2.  */
  if (!buffer->from_stage3)
    {
      const unsigned char *saved_cur;
      cppchar_t next1;

      do
	{
	  if (next == '?')
	    {
	      if (buffer->cur[0] != '?' || !trigraph_p (pfile))
		break;

	      /* Translate the trigraph.  */
	      next = _cpp_trigraph_map[buffer->cur[1]];
	      buffer->cur += 2;
	      if (next != '\\')
		break;
	    }

	  if (buffer->cur == buffer->rlimit)
	    break;

	  /* We have a backslash, and room for at least one more
	     character.  Skip horizontal whitespace.  */
	  saved_cur = buffer->cur;
	  do
	    next1 = *buffer->cur++;
	  while (is_nvspace (next1) && buffer->cur < buffer->rlimit);

	  if (!is_vspace (next1))
	    {
	      buffer->cur = saved_cur;
	      break;
	    }

	  if (saved_cur != buffer->cur - 1
	      && !pfile->state.lexing_comment)
	    cpp_warning (pfile, "backslash and newline separated by space");

	  handle_newline (pfile);
	  buffer->backup_to = buffer->cur;
	  if (buffer->cur == buffer->rlimit)
	    {
	      cpp_pedwarn (pfile, "backslash-newline at end of file");
	      next = EOF;
	    }
	  else
	    next = *buffer->cur++;
	}
      while (next == '\\' || next == '?');
    }

  return next;
}

/* Obtain the next character, after trigraph conversion and skipping
   an arbitrarily long string of escaped newlines.  The common case of
   no trigraphs or escaped newlines falls through quickly.  On return,
   buffer->backup_to points to where to return to if the character is
   not to be processed.  */
static cppchar_t
get_effective_char (pfile)
     cpp_reader *pfile;
{
  cppchar_t next;
  cpp_buffer *buffer = pfile->buffer;

  buffer->backup_to = buffer->cur;
  next = *buffer->cur++;
  if (__builtin_expect (next == '?' || next == '\\', 0))
    next = skip_escaped_newlines (pfile);

   return next;
}

/* Skip a C-style block comment.  We find the end of the comment by
   seeing if an asterisk is before every '/' we encounter.  Returns
   non-zero if comment terminated by EOF, zero otherwise.  */
static int
skip_block_comment (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  cppchar_t c = EOF, prevc = EOF;

  pfile->state.lexing_comment = 1;
  while (buffer->cur != buffer->rlimit)
    {
      prevc = c, c = *buffer->cur++;

      /* FIXME: For speed, create a new character class of characters
	 of interest inside block comments.  */
      if (c == '?' || c == '\\')
	c = skip_escaped_newlines (pfile);

      /* People like decorating comments with '*', so check for '/'
	 instead for efficiency.  */
      if (c == '/')
	{
	  if (prevc == '*')
	    break;

	  /* Warn about potential nested comments, but not if the '/'
	     comes immediately before the true comment delimiter.
	     Don't bother to get it right across escaped newlines.  */
	  if (CPP_OPTION (pfile, warn_comments)
	      && buffer->cur[0] == '*' && buffer->cur[1] != '/')
	    cpp_warning_with_line (pfile,
				   pfile->line, CPP_BUF_COL (buffer),
				   "\"/*\" within comment");
	}
      else if (is_vspace (c))
	handle_newline (pfile);
      else if (c == '\t')
	adjust_column (pfile);
    }

  pfile->state.lexing_comment = 0;
  return c != '/' || prevc != '*';
}

/* Skip a C++ line comment, leaving buffer->cur pointing to the
   terminating newline.  Handles escaped newlines.  Returns non-zero
   if a multiline comment.  */
static int
skip_line_comment (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  unsigned int orig_line = pfile->line;
  cppchar_t c;

  pfile->state.lexing_comment = 1;
  do
    {
      if (buffer->cur == buffer->rlimit)
	goto at_eof;

      c = *buffer->cur++;
      if (c == '?' || c == '\\')
	c = skip_escaped_newlines (pfile);
    }
  while (!is_vspace (c));

  /* Step back over the newline, except at EOF.  */
  buffer->cur--;
 at_eof:

  pfile->state.lexing_comment = 0;
  return orig_line != pfile->line;
}

/* pfile->buffer->cur is one beyond the \t character.  Update
   col_adjust so we track the column correctly.  */
static void
adjust_column (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  unsigned int col = CPP_BUF_COL (buffer) - 1; /* Zero-based column.  */

  /* Round it up to multiple of the tabstop, but subtract 1 since the
     tab itself occupies a character position.  */
  buffer->col_adjust += (CPP_OPTION (pfile, tabstop)
			 - col % CPP_OPTION (pfile, tabstop)) - 1;
}

/* Skips whitespace, saving the next non-whitespace character.
   Adjusts pfile->col_adjust to account for tabs.  Without this,
   tokens might be assigned an incorrect column.  */
static int
skip_whitespace (pfile, c)
     cpp_reader *pfile;
     cppchar_t c;
{
  cpp_buffer *buffer = pfile->buffer;
  unsigned int warned = 0;

  do
    {
      /* Horizontal space always OK.  */
      if (c == ' ')
	;
      else if (c == '\t')
	adjust_column (pfile);
      /* Just \f \v or \0 left.  */
      else if (c == '\0')
	{
	  if (buffer->cur - 1 == buffer->rlimit)
	    return 0;
	  if (!warned)
	    {
	      cpp_warning (pfile, "null character(s) ignored");
	      warned = 1;
	    }
	}
      else if (pfile->state.in_directive && CPP_PEDANTIC (pfile))
	cpp_pedwarn_with_line (pfile, pfile->line,
			       CPP_BUF_COL (buffer),
			       "%s in preprocessing directive",
			       c == '\f' ? "form feed" : "vertical tab");

      c = *buffer->cur++;
    }
  /* We only want non-vertical space, i.e. ' ' \t \f \v \0.  */
  while (is_nvspace (c));

  buffer->cur--;
  return 1;
}

/* See if the characters of a number token are valid in a name (no
   '.', '+' or '-').  */
static int
name_p (pfile, string)
     cpp_reader *pfile;
     const cpp_string *string;
{
  unsigned int i;

  for (i = 0; i < string->len; i++)
    if (!is_idchar (string->text[i]))
      return 0;

  return 1;  
}

/* Parse an identifier, skipping embedded backslash-newlines.  This is
   a critical inner loop.  The common case is an identifier which has
   not been split by backslash-newline, does not contain a dollar
   sign, and has already been scanned (roughly 10:1 ratio of
   seen:unseen identifiers in normal code; the distribution is
   Poisson-like).  Second most common case is a new identifier, not
   split and no dollar sign.  The other possibilities are rare and
   have been relegated to parse_identifier_slow.  */
static cpp_hashnode *
parse_identifier (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *result;
  const U_CHAR *cur;

  /* Fast-path loop.  Skim over a normal identifier.
     N.B. ISIDNUM does not include $.  */
  cur = pfile->buffer->cur;
  while (ISIDNUM (*cur))
    cur++;

  /* Check for slow-path cases.  */
  if (*cur == '?' || *cur == '\\' || *cur == '$')
    result = parse_identifier_slow (pfile, cur);
  else
    {
      const U_CHAR *base = pfile->buffer->cur - 1;
      result = (cpp_hashnode *)
	ht_lookup (pfile->hash_table, base, cur - base, HT_ALLOC);
      pfile->buffer->cur = cur;
    }

  /* Rarely, identifiers require diagnostics when lexed.
     XXX Has to be forced out of the fast path.  */
  if (__builtin_expect ((result->flags & NODE_DIAGNOSTIC)
			&& !pfile->state.skipping, 0))
    {
      /* It is allowed to poison the same identifier twice.  */
      if ((result->flags & NODE_POISONED) && !pfile->state.poisoned_ok)
	cpp_error (pfile, "attempt to use poisoned \"%s\"",
		   NODE_NAME (result));

      /* Constraint 6.10.3.5: __VA_ARGS__ should only appear in the
	 replacement list of a variadic macro.  */
      if (result == pfile->spec_nodes.n__VA_ARGS__
	  && !pfile->state.va_args_ok)
	cpp_pedwarn (pfile,
	"__VA_ARGS__ can only appear in the expansion of a C99 variadic macro");
    }

  return result;
}

/* Slow path.  This handles identifiers which have been split, and
   identifiers which contain dollar signs.  The part of the identifier
   from PFILE->buffer->cur-1 to CUR has already been scanned.  */
static cpp_hashnode *
parse_identifier_slow (pfile, cur)
     cpp_reader *pfile;
     const U_CHAR *cur;
{
  cpp_buffer *buffer = pfile->buffer;
  const U_CHAR *base = buffer->cur - 1;
  struct obstack *stack = &pfile->hash_table->stack;
  unsigned int c, saw_dollar = 0, len;

  /* Copy the part of the token which is known to be okay.  */
  obstack_grow (stack, base, cur - base);

  /* Now process the part which isn't.  We are looking at one of
     '$', '\\', or '?' on entry to this loop.  */
  c = *cur++;
  buffer->cur = cur;
  do
    {
      while (is_idchar (c))
        {
          obstack_1grow (stack, c);

          if (c == '$')
            saw_dollar++;

          c = *buffer->cur++;
        }

      /* Potential escaped newline?  */
      buffer->backup_to = buffer->cur - 1;
      if (c != '?' && c != '\\')
        break;
      c = skip_escaped_newlines (pfile);
    }
  while (is_idchar (c));

  /* Step back over the unwanted char.  */
  BACKUP ();

  /* $ is not an identifier character in the standard, but is commonly
     accepted as an extension.  Don't warn about it in skipped
     conditional blocks.  */
  if (saw_dollar && CPP_PEDANTIC (pfile) && ! pfile->state.skipping)
    cpp_pedwarn (pfile, "'$' character(s) in identifier");

  /* Identifiers are null-terminated.  */
  len = obstack_object_size (stack);
  obstack_1grow (stack, '\0');

  return (cpp_hashnode *)
    ht_lookup (pfile->hash_table, obstack_finish (stack), len, HT_ALLOCED);
}

/* Parse a number, beginning with character C, skipping embedded
   backslash-newlines.  LEADING_PERIOD is non-zero if there was a "."
   before C.  Place the result in NUMBER.  */
static void
parse_number (pfile, number, c, leading_period)
     cpp_reader *pfile;
     cpp_string *number;
     cppchar_t c;
     int leading_period;
{
  cpp_buffer *buffer = pfile->buffer;
  unsigned char *dest, *limit;

  dest = BUFF_FRONT (pfile->u_buff);
  limit = BUFF_LIMIT (pfile->u_buff);

  /* Place a leading period.  */
  if (leading_period)
    {
      if (dest == limit)
	{
	  _cpp_extend_buff (pfile, &pfile->u_buff, 1);
	  dest = BUFF_FRONT (pfile->u_buff);
	  limit = BUFF_LIMIT (pfile->u_buff);
	}
      *dest++ = '.';
    }
  
  do
    {
      do
	{
	  /* Need room for terminating null.  */
	  if ((size_t) (limit - dest) < 2)
	    {
	      size_t len_so_far = dest - BUFF_FRONT (pfile->u_buff);
	      _cpp_extend_buff (pfile, &pfile->u_buff, 2);
	      dest = BUFF_FRONT (pfile->u_buff) + len_so_far;
	      limit = BUFF_LIMIT (pfile->u_buff);
	    }
	  *dest++ = c;

	  c = *buffer->cur++;
	}
      while (is_numchar (c) || c == '.' || VALID_SIGN (c, dest[-1]));

      /* Potential escaped newline?  */
      buffer->backup_to = buffer->cur - 1;
      if (c != '?' && c != '\\')
	break;
      c = skip_escaped_newlines (pfile);
    }
  while (is_numchar (c) || c == '.' || VALID_SIGN (c, dest[-1]));

  /* Step back over the unwanted char.  */
  BACKUP ();

  /* Null-terminate the number.  */
  *dest = '\0';

  number->text = BUFF_FRONT (pfile->u_buff);
  number->len = dest - number->text;
  BUFF_FRONT (pfile->u_buff) = dest + 1;
}

/* Subroutine of parse_string.  Emits error for unterminated strings.  */
static void
unterminated (pfile, term)
     cpp_reader *pfile;
     int term;
{
  cpp_error (pfile, "missing terminating %c character", term);

  if (term == '\"' && pfile->mls_line && pfile->mls_line != pfile->line)
    {
      cpp_error_with_line (pfile, pfile->mls_line, pfile->mls_col,
			   "possible start of unterminated string literal");
      pfile->mls_line = 0;
    }
}

/* Subroutine of parse_string.  */
static int
unescaped_terminator_p (pfile, dest)
     cpp_reader *pfile;
     const unsigned char *dest;
{
  const unsigned char *start, *temp;

  /* In #include-style directives, terminators are not escapeable.  */
  if (pfile->state.angled_headers)
    return 1;

  start = BUFF_FRONT (pfile->u_buff);

  /* An odd number of consecutive backslashes represents an escaped
     terminator.  */
  for (temp = dest; temp > start && temp[-1] == '\\'; temp--)
    ;

  return ((dest - temp) & 1) == 0;
}

/* Parses a string, character constant, or angle-bracketed header file
   name.  Handles embedded trigraphs and escaped newlines.  The stored
   string is guaranteed NUL-terminated, but it is not guaranteed that
   this is the first NUL since embedded NULs are preserved.
   Multi-line strings are allowed, but they are deprecated.

   When this function returns, buffer->cur points to the next
   character to be processed.  */
static void
parse_string (pfile, token, terminator)
     cpp_reader *pfile;
     cpp_token *token;
     cppchar_t terminator;
{
  cpp_buffer *buffer = pfile->buffer;
  unsigned char *dest, *limit;
  cppchar_t c;
  bool warned_nulls = false, warned_multi = false;

  dest = BUFF_FRONT (pfile->u_buff);
  limit = BUFF_LIMIT (pfile->u_buff);

  for (;;)
    {
      /* We need room for another char, possibly the terminating NUL.  */
      if ((size_t) (limit - dest) < 1)
	{
	  size_t len_so_far = dest - BUFF_FRONT (pfile->u_buff);
	  _cpp_extend_buff (pfile, &pfile->u_buff, 2);
	  dest = BUFF_FRONT (pfile->u_buff) + len_so_far;
	  limit = BUFF_LIMIT (pfile->u_buff);
	}

      /* Handle trigraphs, escaped newlines etc.  */
      c = *buffer->cur++;
      if (c == '?' || c == '\\')
	c = skip_escaped_newlines (pfile);

      if (c == terminator)
	{
	  if (unescaped_terminator_p (pfile, dest))
	    break;
	}
      else if (is_vspace (c))
	{
	  /* In assembly language, silently terminate string and
	     character literals at end of line.  This is a kludge
	     around not knowing where comments are.  */
	  if (CPP_OPTION (pfile, lang) == CLK_ASM && terminator != '>')
	    {
	      buffer->cur--;
	      break;
	    }

	  /* Character constants and header names may not extend over
	     multiple lines.  In Standard C, neither may strings.
	     Unfortunately, we accept multiline strings as an
	     extension, except in #include family directives.  */
	  if (terminator != '"' || pfile->state.angled_headers)
	    {
	      unterminated (pfile, terminator);
	      buffer->cur--;
	      break;
	    }

	  if (!warned_multi)
	    {
	      warned_multi = true;
	      cpp_pedwarn (pfile, "multi-line string literals are deprecated");
	    }

	  if (pfile->mls_line == 0)
	    {
	      pfile->mls_line = token->line;
	      pfile->mls_col = token->col;
	    }
	      
	  handle_newline (pfile);
	  c = '\n';
	}
      else if (c == '\0')
	{
	  if (buffer->cur - 1 == buffer->rlimit)
	    {
	      unterminated (pfile, terminator);
	      buffer->cur--;
	      break;
	    }
	  if (!warned_nulls)
	    {
	      warned_nulls = true;
	      cpp_warning (pfile, "null character(s) preserved in literal");
	    }
	}

      *dest++ = c;
    }

  *dest = '\0';

  token->val.str.text = BUFF_FRONT (pfile->u_buff);
  token->val.str.len = dest - BUFF_FRONT (pfile->u_buff);
  BUFF_FRONT (pfile->u_buff) = dest + 1;
}

/* The stored comment includes the comment start and any terminator.  */
static void
save_comment (pfile, token, from)
     cpp_reader *pfile;
     cpp_token *token;
     const unsigned char *from;
{
  unsigned char *buffer;
  unsigned int len;
  
  len = pfile->buffer->cur - from + 1; /* + 1 for the initial '/'.  */

  /* C++ comments probably (not definitely) have moved past a new
     line, which we don't want to save in the comment.  */
  if (is_vspace (pfile->buffer->cur[-1]))
    len--;
  buffer = _cpp_unaligned_alloc (pfile, len);
  
  token->type = CPP_COMMENT;
  token->val.str.len = len;
  token->val.str.text = buffer;

  buffer[0] = '/';
  memcpy (buffer + 1, from, len - 1);
}

/* Allocate COUNT tokens for RUN.  */
void
_cpp_init_tokenrun (run, count)
     tokenrun *run;
     unsigned int count;
{
  run->base = xnewvec (cpp_token, count);
  run->limit = run->base + count;
  run->next = NULL;
}

/* Returns the next tokenrun, or creates one if there is none.  */
static tokenrun *
next_tokenrun (run)
     tokenrun *run;
{
  if (run->next == NULL)
    {
      run->next = xnew (tokenrun);
      run->next->prev = run;
      _cpp_init_tokenrun (run->next, 250);
    }

  return run->next;
}

/* Allocate a single token that is invalidated at the same time as the
   rest of the tokens on the line.  Has its line and col set to the
   same as the last lexed token, so that diagnostics appear in the
   right place.  */
cpp_token *
_cpp_temp_token (pfile)
     cpp_reader *pfile;
{
  cpp_token *old, *result;

  old = pfile->cur_token - 1;
  if (pfile->cur_token == pfile->cur_run->limit)
    {
      pfile->cur_run = next_tokenrun (pfile->cur_run);
      pfile->cur_token = pfile->cur_run->base;
    }

  result = pfile->cur_token++;
  result->line = old->line;
  result->col = old->col;
  return result;
}

/* Lex a token into RESULT (external interface).  Takes care of issues
   like directive handling, token lookahead, multiple include
   optimization and skipping.  */
const cpp_token *
_cpp_lex_token (pfile)
     cpp_reader *pfile;
{
  cpp_token *result;

  for (;;)
    {
      if (pfile->cur_token == pfile->cur_run->limit)
	{
	  pfile->cur_run = next_tokenrun (pfile->cur_run);
	  pfile->cur_token = pfile->cur_run->base;
	}

      if (pfile->lookaheads)
	{
	  pfile->lookaheads--;
	  result = pfile->cur_token++;
	}
      else
	result = _cpp_lex_direct (pfile);

      if (result->flags & BOL)
	{
	  /* Is this a directive.  If _cpp_handle_directive returns
	     false, it is an assembler #.  */
	  if (result->type == CPP_HASH
	      && !pfile->state.parsing_args
	      && _cpp_handle_directive (pfile, result->flags & PREV_WHITE))
	    continue;
	  if (pfile->cb.line_change && !pfile->state.skipping)
	    (*pfile->cb.line_change)(pfile, result, pfile->state.parsing_args);
	}

      /* We don't skip tokens in directives.  */
      if (pfile->state.in_directive)
	break;

      /* Outside a directive, invalidate controlling macros.  At file
	 EOF, _cpp_lex_direct takes care of popping the buffer, so we never
	 get here and MI optimisation works.  */
      pfile->mi_valid = false;

      if (!pfile->state.skipping || result->type == CPP_EOF)
	break;
    }

  return result;
}

#define IF_NEXT_IS(CHAR, THEN_TYPE, ELSE_TYPE)	\
  do {						\
    if (get_effective_char (pfile) == CHAR)	\
      result->type = THEN_TYPE;			\
    else					\
      {						\
        BACKUP ();				\
        result->type = ELSE_TYPE;		\
      }						\
  } while (0)

/* Lex a token into pfile->cur_token, which is also incremented, to
   get diagnostics pointing to the correct location.

   Does not handle issues such as token lookahead, multiple-include
   optimisation, directives, skipping etc.  This function is only
   suitable for use by _cpp_lex_token, and in special cases like
   lex_expansion_token which doesn't care for any of these issues.

   When meeting a newline, returns CPP_EOF if parsing a directive,
   otherwise returns to the start of the token buffer if permissible.
   Returns the location of the lexed token.  */
cpp_token *
_cpp_lex_direct (pfile)
     cpp_reader *pfile;
{
  cppchar_t c;
  cpp_buffer *buffer;
  const unsigned char *comment_start;
  cpp_token *result = pfile->cur_token++;

 fresh_line:
  buffer = pfile->buffer;
  result->flags = buffer->saved_flags;
  buffer->saved_flags = 0;
 update_tokens_line:
  result->line = pfile->line;

 skipped_white:
  c = *buffer->cur++;
  result->col = CPP_BUF_COLUMN (buffer, buffer->cur);

 trigraph:
  switch (c)
    {
    case ' ': case '\t': case '\f': case '\v': case '\0':
      result->flags |= PREV_WHITE;
      if (skip_whitespace (pfile, c))
	goto skipped_white;

      /* EOF.  */
      buffer->cur--;
      buffer->saved_flags = BOL;
      if (!pfile->state.parsing_args && !pfile->state.in_directive)
	{
	  if (buffer->cur != buffer->line_base)
	    {
	      /* Non-empty files should end in a newline.  Don't warn
		 for command line and _Pragma buffers.  */
	      if (!buffer->from_stage3)
		cpp_pedwarn (pfile, "no newline at end of file");
	      handle_newline (pfile);
	    }

	  /* Don't pop the last buffer.  */
	  if (buffer->prev)
	    {
	      unsigned char stop = buffer->return_at_eof;

	      _cpp_pop_buffer (pfile);
	      if (!stop)
		goto fresh_line;
	    }
	}
      result->type = CPP_EOF;
      break;

    case '\n': case '\r':
      handle_newline (pfile);
      buffer->saved_flags = BOL;
      if (! pfile->state.in_directive)
	{
	  if (pfile->state.parsing_args == 2)
	    buffer->saved_flags |= PREV_WHITE;
	  if (!pfile->keep_tokens)
	    {
	      pfile->cur_run = &pfile->base_run;
	      result = pfile->base_run.base;
	      pfile->cur_token = result + 1;
	    }
	  goto fresh_line;
	}
      result->type = CPP_EOF;
      break;

    case '?':
    case '\\':
      /* These could start an escaped newline, or '?' a trigraph.  Let
	 skip_escaped_newlines do all the work.  */
      {
	unsigned int line = pfile->line;

	c = skip_escaped_newlines (pfile);
	if (line != pfile->line)
	  {
	    buffer->cur--;
	    /* We had at least one escaped newline of some sort.
	       Update the token's line and column.  */
	    goto update_tokens_line;
	  }
      }

      /* We are either the original '?' or '\\', or a trigraph.  */
      if (c == '?')
	result->type = CPP_QUERY;
      else if (c == '\\')
	goto random_char;
      else
	goto trigraph;
      break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      result->type = CPP_NUMBER;
      parse_number (pfile, &result->val.str, c, 0);
      break;

    case 'L':
      /* 'L' may introduce wide characters or strings.  */
	{
	  const unsigned char *pos = buffer->cur;

	  c = get_effective_char (pfile);
	  if (c == '\'' || c == '"')
	    {
	      result->type = (c == '"' ? CPP_WSTRING: CPP_WCHAR);
	      parse_string (pfile, result, c);
	      break;
	    }
	  buffer->cur = pos;
	}
	/* Fall through.  */

    start_ident:
    case '_':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
      result->type = CPP_NAME;
      result->val.node = parse_identifier (pfile);

      /* Convert named operators to their proper types.  */
      if (result->val.node->flags & NODE_OPERATOR)
	{
	  result->flags |= NAMED_OP;
	  result->type = result->val.node->value.operator;
	}
      break;

    case '\'':
    case '"':
      result->type = c == '"' ? CPP_STRING: CPP_CHAR;
      parse_string (pfile, result, c);
      break;

    case '/':
      /* A potential block or line comment.  */
      comment_start = buffer->cur;
      c = get_effective_char (pfile);

      if (c == '*')
	{
	  if (skip_block_comment (pfile))
	    cpp_error (pfile, "unterminated comment");
	}
      else if (c == '/' && (CPP_OPTION (pfile, cplusplus_comments)
			    || CPP_IN_SYSTEM_HEADER (pfile)))
	{
	  /* Warn about comments only if pedantically GNUC89, and not
	     in system headers.  */
	  if (CPP_OPTION (pfile, lang) == CLK_GNUC89 && CPP_PEDANTIC (pfile)
	      && ! buffer->warned_cplusplus_comments)
	    {
	      cpp_pedwarn (pfile,
			   "C++ style comments are not allowed in ISO C89");
	      cpp_pedwarn (pfile,
			   "(this will be reported only once per input file)");
	      buffer->warned_cplusplus_comments = 1;
	    }

	  if (skip_line_comment (pfile) && CPP_OPTION (pfile, warn_comments))
	    cpp_warning (pfile, "multi-line comment");
	}
      else if (c == '=')
	{
	  result->type = CPP_DIV_EQ;
	  break;
	}
      else
	{
	  BACKUP ();
	  result->type = CPP_DIV;
	  break;
	}

      if (!pfile->state.save_comments)
	{
	  result->flags |= PREV_WHITE;
	  goto update_tokens_line;
	}

      /* Save the comment as a token in its own right.  */
      save_comment (pfile, result, comment_start);
      break;

    case '<':
      if (pfile->state.angled_headers)
	{
	  result->type = CPP_HEADER_NAME;
	  parse_string (pfile, result, '>');
	  break;
	}

      c = get_effective_char (pfile);
      if (c == '=')
	result->type = CPP_LESS_EQ;
      else if (c == '<')
	IF_NEXT_IS ('=', CPP_LSHIFT_EQ, CPP_LSHIFT);
      else if (c == '?' && CPP_OPTION (pfile, cplusplus))
	IF_NEXT_IS ('=', CPP_MIN_EQ, CPP_MIN);
      else if (c == ':' && CPP_OPTION (pfile, digraphs))
	{
	  result->type = CPP_OPEN_SQUARE;
	  result->flags |= DIGRAPH;
	}
      else if (c == '%' && CPP_OPTION (pfile, digraphs))
	{
	  result->type = CPP_OPEN_BRACE;
	  result->flags |= DIGRAPH;
	}
      else
	{
	  BACKUP ();
	  result->type = CPP_LESS;
	}
      break;

    case '>':
      c = get_effective_char (pfile);
      if (c == '=')
	result->type = CPP_GREATER_EQ;
      else if (c == '>')
	IF_NEXT_IS ('=', CPP_RSHIFT_EQ, CPP_RSHIFT);
      else if (c == '?' && CPP_OPTION (pfile, cplusplus))
	IF_NEXT_IS ('=', CPP_MAX_EQ, CPP_MAX);
      else
	{
	  BACKUP ();
	  result->type = CPP_GREATER;
	}
      break;

    case '%':
      c = get_effective_char (pfile);
      if (c == '=')
	result->type = CPP_MOD_EQ;
      else if (CPP_OPTION (pfile, digraphs) && c == ':')
	{
	  result->flags |= DIGRAPH;
	  result->type = CPP_HASH;
	  if (get_effective_char (pfile) == '%')
	    {
	      const unsigned char *pos = buffer->cur;

	      if (get_effective_char (pfile) == ':')
		result->type = CPP_PASTE;
	      else
		buffer->cur = pos - 1;
	    }
	  else
	    BACKUP ();
	}
      else if (CPP_OPTION (pfile, digraphs) && c == '>')
	{
	  result->flags |= DIGRAPH;
	  result->type = CPP_CLOSE_BRACE;
	}
      else
	{
	  BACKUP ();
	  result->type = CPP_MOD;
	}
      break;

    case '.':
      result->type = CPP_DOT;
      c = get_effective_char (pfile);
      if (c == '.')
	{
	  const unsigned char *pos = buffer->cur;

	  if (get_effective_char (pfile) == '.')
	    result->type = CPP_ELLIPSIS;
	  else
	    buffer->cur = pos - 1;
	}
      /* All known character sets have 0...9 contiguous.  */
      else if (ISDIGIT (c))
	{
	  result->type = CPP_NUMBER;
	  parse_number (pfile, &result->val.str, c, 1);
	}
      else if (c == '*' && CPP_OPTION (pfile, cplusplus))
	result->type = CPP_DOT_STAR;
      else
	BACKUP ();
      break;

    case '+':
      c = get_effective_char (pfile);
      if (c == '+')
	result->type = CPP_PLUS_PLUS;
      else if (c == '=')
	result->type = CPP_PLUS_EQ;
      else
	{
	  BACKUP ();
	  result->type = CPP_PLUS;
	}
      break;

    case '-':
      c = get_effective_char (pfile);
      if (c == '>')
	{
	  result->type = CPP_DEREF;
	  if (CPP_OPTION (pfile, cplusplus))
	    {
	      if (get_effective_char (pfile) == '*')
		result->type = CPP_DEREF_STAR;
	      else
		BACKUP ();
	    }
	}
      else if (c == '-')
	result->type = CPP_MINUS_MINUS;
      else if (c == '=')
	result->type = CPP_MINUS_EQ;
      else
	{
	  BACKUP ();
	  result->type = CPP_MINUS;
	}
      break;

    case '&':
      c = get_effective_char (pfile);
      if (c == '&')
	result->type = CPP_AND_AND;
      else if (c == '=')
	result->type = CPP_AND_EQ;
      else
	{
	  BACKUP ();
	  result->type = CPP_AND;
	}
      break;
	  
    case '|':
      c = get_effective_char (pfile);
      if (c == '|')
	result->type = CPP_OR_OR;
      else if (c == '=')
	result->type = CPP_OR_EQ;
      else
	{
	  BACKUP ();
	  result->type = CPP_OR;
	}
      break;

    case ':':
      c = get_effective_char (pfile);
      if (c == ':' && CPP_OPTION (pfile, cplusplus))
	result->type = CPP_SCOPE;
      else if (c == '>' && CPP_OPTION (pfile, digraphs))
	{
	  result->flags |= DIGRAPH;
	  result->type = CPP_CLOSE_SQUARE;
	}
      else
	{
	  BACKUP ();
	  result->type = CPP_COLON;
	}
      break;

    case '*': IF_NEXT_IS ('=', CPP_MULT_EQ, CPP_MULT); break;
    case '=': IF_NEXT_IS ('=', CPP_EQ_EQ, CPP_EQ); break;
    case '!': IF_NEXT_IS ('=', CPP_NOT_EQ, CPP_NOT); break;
    case '^': IF_NEXT_IS ('=', CPP_XOR_EQ, CPP_XOR); break;
    case '#': IF_NEXT_IS ('#', CPP_PASTE, CPP_HASH); break;

    case '~': result->type = CPP_COMPL; break;
    case ',': result->type = CPP_COMMA; break;
    case '(': result->type = CPP_OPEN_PAREN; break;
    case ')': result->type = CPP_CLOSE_PAREN; break;
    case '[': result->type = CPP_OPEN_SQUARE; break;
    case ']': result->type = CPP_CLOSE_SQUARE; break;
    case '{': result->type = CPP_OPEN_BRACE; break;
    case '}': result->type = CPP_CLOSE_BRACE; break;
    case ';': result->type = CPP_SEMICOLON; break;

      /* @ is a punctuator in Objective C.  */
    case '@': result->type = CPP_ATSIGN; break;

    case '$':
      if (CPP_OPTION (pfile, dollars_in_ident))
	goto start_ident;
      /* Fall through...  */

    random_char:
    default:
      result->type = CPP_OTHER;
      result->val.c = c;
      break;
    }

  return result;
}

/* An upper bound on the number of bytes needed to spell TOKEN,
   including preceding whitespace.  */
unsigned int
cpp_token_len (token)
     const cpp_token *token;
{
  unsigned int len;

  switch (TOKEN_SPELL (token))
    {
    default:		len = 0;				break;
    case SPELL_NUMBER:
    case SPELL_STRING:	len = token->val.str.len;		break;
    case SPELL_IDENT:	len = NODE_LEN (token->val.node);	break;
    }
  /* 1 for whitespace, 4 for comment delimiters.  */
  return len + 5;
}

/* Write the spelling of a token TOKEN to BUFFER.  The buffer must
   already contain the enough space to hold the token's spelling.
   Returns a pointer to the character after the last character
   written.  */
unsigned char *
cpp_spell_token (pfile, token, buffer)
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
	  spelling
	    = digraph_spellings[(int) token->type - (int) CPP_FIRST_DIGRAPH];
	else if (token->flags & NAMED_OP)
	  goto spell_ident;
	else
	  spelling = TOKEN_NAME (token);
	
	while ((c = *spelling++) != '\0')
	  *buffer++ = c;
      }
      break;

    case SPELL_CHAR:
      *buffer++ = token->val.c;
      break;

    spell_ident:
    case SPELL_IDENT:
      memcpy (buffer, NODE_NAME (token->val.node), NODE_LEN (token->val.node));
      buffer += NODE_LEN (token->val.node);
      break;

    case SPELL_NUMBER:
      memcpy (buffer, token->val.str.text, token->val.str.len);
      buffer += token->val.str.len;
      break;

    case SPELL_STRING:
      {
	int left, right, tag;
	switch (token->type)
	  {
	  case CPP_STRING:	left = '"';  right = '"';  tag = '\0'; break;
	  case CPP_WSTRING:	left = '"';  right = '"';  tag = 'L';  break;
	  case CPP_CHAR:	left = '\''; right = '\''; tag = '\0'; break;
    	  case CPP_WCHAR:	left = '\''; right = '\''; tag = 'L';  break;
	  case CPP_HEADER_NAME:	left = '<';  right = '>';  tag = '\0'; break;
	  default:
	    cpp_ice (pfile, "unknown string token %s\n", TOKEN_NAME (token));
	    return buffer;
	  }
	if (tag) *buffer++ = tag;
	*buffer++ = left;
	memcpy (buffer, token->val.str.text, token->val.str.len);
	buffer += token->val.str.len;
	*buffer++ = right;
      }
      break;

    case SPELL_NONE:
      cpp_ice (pfile, "unspellable token %s", TOKEN_NAME (token));
      break;
    }

  return buffer;
}

/* Returns TOKEN spelt as a null-terminated string.  The string is
   freed when the reader is destroyed.  Useful for diagnostics.  */
unsigned char *
cpp_token_as_text (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  unsigned int len = cpp_token_len (token);
  unsigned char *start = _cpp_unaligned_alloc (pfile, len), *end;

  end = cpp_spell_token (pfile, token, start);
  end[0] = '\0';

  return start;
}

/* Used by C front ends, which really should move to using
   cpp_token_as_text.  */
const char *
cpp_type2name (type)
     enum cpp_ttype type;
{
  return (const char *) token_spellings[type].name;
}

/* Writes the spelling of token to FP, without any preceding space.
   Separated from cpp_spell_token for efficiency - to avoid stdio
   double-buffering.  */
void
cpp_output_token (token, fp)
     const cpp_token *token;
     FILE *fp;
{
  switch (TOKEN_SPELL (token))
    {
    case SPELL_OPERATOR:
      {
	const unsigned char *spelling;
	int c;

	if (token->flags & DIGRAPH)
	  spelling
	    = digraph_spellings[(int) token->type - (int) CPP_FIRST_DIGRAPH];
	else if (token->flags & NAMED_OP)
	  goto spell_ident;
	else
	  spelling = TOKEN_NAME (token);

	c = *spelling;
	do
	  putc (c, fp);
	while ((c = *++spelling) != '\0');
      }
      break;

    case SPELL_CHAR:
      putc (token->val.c, fp);
      break;

    spell_ident:
    case SPELL_IDENT:
      fwrite (NODE_NAME (token->val.node), 1, NODE_LEN (token->val.node), fp);
    break;

    case SPELL_NUMBER:
      fwrite (token->val.str.text, 1, token->val.str.len, fp);
      break;

    case SPELL_STRING:
      {
	int left, right, tag;
	switch (token->type)
	  {
	  case CPP_STRING:	left = '"';  right = '"';  tag = '\0'; break;
	  case CPP_WSTRING:	left = '"';  right = '"';  tag = 'L';  break;
	  case CPP_CHAR:	left = '\''; right = '\''; tag = '\0'; break;
    	  case CPP_WCHAR:	left = '\''; right = '\''; tag = 'L';  break;
	  case CPP_HEADER_NAME:	left = '<';  right = '>';  tag = '\0'; break;
	  default:
	    fprintf (stderr, "impossible STRING token %s\n", TOKEN_NAME (token));
	    return;
	  }
	if (tag) putc (tag, fp);
	putc (left, fp);
	fwrite (token->val.str.text, 1, token->val.str.len, fp);
	putc (right, fp);
      }
      break;

    case SPELL_NONE:
      /* An error, most probably.  */
      break;
    }
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
	return a->val.c == b->val.c; /* Character.  */
      case SPELL_NONE:
	return (a->type != CPP_MACRO_ARG || a->val.arg_no == b->val.arg_no);
      case SPELL_IDENT:
	return a->val.node == b->val.node;
      case SPELL_NUMBER:
      case SPELL_STRING:
	return (a->val.str.len == b->val.str.len
		&& !memcmp (a->val.str.text, b->val.str.text,
			    a->val.str.len));
      }

  return 0;
}

/* Returns nonzero if a space should be inserted to avoid an
   accidental token paste for output.  For simplicity, it is
   conservative, and occasionally advises a space where one is not
   needed, e.g. "." and ".2".  */
int
cpp_avoid_paste (pfile, token1, token2)
     cpp_reader *pfile;
     const cpp_token *token1, *token2;
{
  enum cpp_ttype a = token1->type, b = token2->type;
  cppchar_t c;

  if (token1->flags & NAMED_OP)
    a = CPP_NAME;
  if (token2->flags & NAMED_OP)
    b = CPP_NAME;

  c = EOF;
  if (token2->flags & DIGRAPH)
    c = digraph_spellings[(int) b - (int) CPP_FIRST_DIGRAPH][0];
  else if (token_spellings[b].category == SPELL_OPERATOR)
    c = token_spellings[b].name[0];

  /* Quickly get everything that can paste with an '='.  */
  if ((int) a <= (int) CPP_LAST_EQ && c == '=')
    return 1;

  switch (a)
    {
    case CPP_GREATER:	return c == '>' || c == '?';
    case CPP_LESS:	return c == '<' || c == '?' || c == '%' || c == ':';
    case CPP_PLUS:	return c == '+';
    case CPP_MINUS:	return c == '-' || c == '>';
    case CPP_DIV:	return c == '/' || c == '*'; /* Comments.  */
    case CPP_MOD:	return c == ':' || c == '>';
    case CPP_AND:	return c == '&';
    case CPP_OR:	return c == '|';
    case CPP_COLON:	return c == ':' || c == '>';
    case CPP_DEREF:	return c == '*';
    case CPP_DOT:	return c == '.' || c == '%' || b == CPP_NUMBER;
    case CPP_HASH:	return c == '#' || c == '%'; /* Digraph form.  */
    case CPP_NAME:	return ((b == CPP_NUMBER
				 && name_p (pfile, &token2->val.str))
				|| b == CPP_NAME
				|| b == CPP_CHAR || b == CPP_STRING); /* L */
    case CPP_NUMBER:	return (b == CPP_NUMBER || b == CPP_NAME
				|| c == '.' || c == '+' || c == '-');
    case CPP_OTHER:	return (CPP_OPTION (pfile, objc)
				&& token1->val.c == '@'
				&& (b == CPP_NAME || b == CPP_STRING));
    default:		break;
    }

  return 0;
}

/* Output all the remaining tokens on the current line, and a newline
   character, to FP.  Leading whitespace is removed.  If there are
   macros, special token padding is not performed.  */
void
cpp_output_line (pfile, fp)
     cpp_reader *pfile;
     FILE *fp;
{
  const cpp_token *token;

  token = cpp_get_token (pfile);
  while (token->type != CPP_EOF)
    {
      cpp_output_token (token, fp);
      token = cpp_get_token (pfile);
      if (token->flags & PREV_WHITE)
	putc (' ', fp);
    }

  putc ('\n', fp);
}

/* Returns the value of a hexadecimal digit.  */
static unsigned int
hex_digit_value (c)
     unsigned int c;
{
  if (hex_p (c))
    return hex_value (c);
  else
    abort ();
}

/* Parse a '\uNNNN' or '\UNNNNNNNN' sequence.  Returns 1 to indicate
   failure if cpplib is not parsing C++ or C99.  Such failure is
   silent, and no variables are updated.  Otherwise returns 0, and
   warns if -Wtraditional.

   [lex.charset]: The character designated by the universal character
   name \UNNNNNNNN is that character whose character short name in
   ISO/IEC 10646 is NNNNNNNN; the character designated by the
   universal character name \uNNNN is that character whose character
   short name in ISO/IEC 10646 is 0000NNNN.  If the hexadecimal value
   for a universal character name is less than 0x20 or in the range
   0x7F-0x9F (inclusive), or if the universal character name
   designates a character in the basic source character set, then the
   program is ill-formed.

   We assume that wchar_t is Unicode, so we don't need to do any
   mapping.  Is this ever wrong?

   PC points to the 'u' or 'U', PSTR is points to the byte after PC,
   LIMIT is the end of the string or charconst.  PSTR is updated to
   point after the UCS on return, and the UCS is written into PC.  */

static int
maybe_read_ucs (pfile, pstr, limit, pc)
     cpp_reader *pfile;
     const unsigned char **pstr;
     const unsigned char *limit;
     unsigned int *pc;
{
  const unsigned char *p = *pstr;
  unsigned int code = 0;
  unsigned int c = *pc, length;

  /* Only attempt to interpret a UCS for C++ and C99.  */
  if (! (CPP_OPTION (pfile, cplusplus) || CPP_OPTION (pfile, c99)))
    return 1;

  if (CPP_WTRADITIONAL (pfile))
    cpp_warning (pfile, "the meaning of '\\%c' varies with -traditional", c);

  length = (c == 'u' ? 4: 8);

  if ((size_t) (limit - p) < length)
    {
      cpp_error (pfile, "incomplete universal-character-name");
      /* Skip to the end to avoid more diagnostics.  */
      p = limit;
    }
  else
    {
      for (; length; length--, p++)
	{
	  c = *p;
	  if (ISXDIGIT (c))
	    code = (code << 4) + hex_digit_value (c);
	  else
	    {
	      cpp_error (pfile,
			 "non-hex digit '%c' in universal-character-name", c);
	      /* We shouldn't skip in case there are multibyte chars.  */
	      break;
	    }
	}
    }

#ifdef TARGET_EBCDIC
  cpp_error (pfile, "universal-character-name on EBCDIC target");
  code = 0x3f;  /* EBCDIC invalid character */
#else
 /* True extended characters are OK.  */
  if (code >= 0xa0
      && !(code & 0x80000000)
      && !(code >= 0xD800 && code <= 0xDFFF))
    ;
  /* The standard permits $, @ and ` to be specified as UCNs.  We use
     hex escapes so that this also works with EBCDIC hosts.  */
  else if (code == 0x24 || code == 0x40 || code == 0x60)
    ;
  /* Don't give another error if one occurred above.  */
  else if (length == 0)
    cpp_error (pfile, "universal-character-name out of range");
#endif

  *pstr = p;
  *pc = code;
  return 0;
}

/* Interpret an escape sequence, and return its value.  PSTR points to
   the input pointer, which is just after the backslash.  LIMIT is how
   much text we have.  MASK is a bitmask for the precision for the
   destination type (char or wchar_t).  TRADITIONAL, if true, does not
   interpret escapes that did not exist in traditional C.

   Handles all relevant diagnostics.  */
unsigned int
cpp_parse_escape (pfile, pstr, limit, mask, traditional)
     cpp_reader *pfile;
     const unsigned char **pstr;
     const unsigned char *limit;
     unsigned HOST_WIDE_INT mask;
     int traditional;
{
  int unknown = 0;
  const unsigned char *str = *pstr;
  unsigned int c = *str++;

  switch (c)
    {
    case '\\': case '\'': case '"': case '?': break;
    case 'b': c = TARGET_BS;	  break;
    case 'f': c = TARGET_FF;	  break;
    case 'n': c = TARGET_NEWLINE; break;
    case 'r': c = TARGET_CR;	  break;
    case 't': c = TARGET_TAB;	  break;
    case 'v': c = TARGET_VT;	  break;

    case '(': case '{': case '[': case '%':
      /* '\(', etc, are used at beginning of line to avoid confusing Emacs.
	 '\%' is used to prevent SCCS from getting confused.  */
      unknown = CPP_PEDANTIC (pfile);
      break;

    case 'a':
      if (CPP_WTRADITIONAL (pfile))
	cpp_warning (pfile, "the meaning of '\\a' varies with -traditional");
      if (!traditional)
	c = TARGET_BELL;
      break;

    case 'e': case 'E':
      if (CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "non-ISO-standard escape sequence, '\\%c'", c);
      c = TARGET_ESC;
      break;
      
    case 'u': case 'U':
      unknown = maybe_read_ucs (pfile, &str, limit, &c);
      break;

    case 'x':
      if (CPP_WTRADITIONAL (pfile))
	cpp_warning (pfile, "the meaning of '\\x' varies with -traditional");

      if (!traditional)
	{
	  unsigned int i = 0, overflow = 0;
	  int digits_found = 0;

	  while (str < limit)
	    {
	      c = *str;
	      if (! ISXDIGIT (c))
		break;
	      str++;
	      overflow |= i ^ (i << 4 >> 4);
	      i = (i << 4) + hex_digit_value (c);
	      digits_found = 1;
	    }

	  if (!digits_found)
	    cpp_error (pfile, "\\x used with no following hex digits");

	  if (overflow | (i != (i & mask)))
	    {
	      cpp_pedwarn (pfile, "hex escape sequence out of range");
	      i &= mask;
	    }
	  c = i;
	}
      break;

    case '0':  case '1':  case '2':  case '3':
    case '4':  case '5':  case '6':  case '7':
      {
	unsigned int i = c - '0';
	int count = 0;

	while (str < limit && ++count < 3)
	  {
	    c = *str;
	    if (c < '0' || c > '7')
	      break;
	    str++;
	    i = (i << 3) + c - '0';
	  }

	if (i != (i & mask))
	  {
	    cpp_pedwarn (pfile, "octal escape sequence out of range");
	    i &= mask;
	  }
	c = i;
      }
      break;

    default:
      unknown = 1;
      break;
    }

  if (unknown)
    {
      if (ISGRAPH (c))
	cpp_pedwarn (pfile, "unknown escape sequence '\\%c'", c);
      else
	cpp_pedwarn (pfile, "unknown escape sequence: '\\%03o'", c);
    }

  if (c > mask)
    cpp_pedwarn (pfile, "escape sequence out of range for character");

  *pstr = str;
  return c;
}

#ifndef MAX_CHAR_TYPE_SIZE
#define MAX_CHAR_TYPE_SIZE CHAR_TYPE_SIZE
#endif

#ifndef MAX_WCHAR_TYPE_SIZE
#define MAX_WCHAR_TYPE_SIZE WCHAR_TYPE_SIZE
#endif

/* Interpret a (possibly wide) character constant in TOKEN.
   WARN_MULTI warns about multi-character charconsts, if not
   TRADITIONAL.  TRADITIONAL also indicates not to interpret escapes
   that did not exist in traditional C.  PCHARS_SEEN points to a
   variable that is filled in with the number of characters seen.  */
HOST_WIDE_INT
cpp_interpret_charconst (pfile, token, warn_multi, traditional, pchars_seen)
     cpp_reader *pfile;
     const cpp_token *token;
     int warn_multi;
     int traditional;
     unsigned int *pchars_seen;
{
  const unsigned char *str = token->val.str.text;
  const unsigned char *limit = str + token->val.str.len;
  unsigned int chars_seen = 0;
  unsigned int width, max_chars, c;
  unsigned HOST_WIDE_INT mask;
  HOST_WIDE_INT result = 0;

#ifdef MULTIBYTE_CHARS
  (void) local_mbtowc (NULL, NULL, 0);
#endif

  /* Width in bits.  */
  if (token->type == CPP_CHAR)
    width = MAX_CHAR_TYPE_SIZE;
  else
    width = MAX_WCHAR_TYPE_SIZE;

  if (width < HOST_BITS_PER_WIDE_INT)
    mask = ((unsigned HOST_WIDE_INT) 1 << width) - 1;
  else
    mask = ~0;
  max_chars = HOST_BITS_PER_WIDE_INT / width;

  while (str < limit)
    {
#ifdef MULTIBYTE_CHARS
      wchar_t wc;
      int char_len;

      char_len = local_mbtowc (&wc, str, limit - str);
      if (char_len == -1)
	{
	  cpp_warning (pfile, "ignoring invalid multibyte character");
	  c = *str++;
	}
      else
	{
	  str += char_len;
	  c = wc;
	}
#else
      c = *str++;
#endif

      if (c == '\\')
	c = cpp_parse_escape (pfile, &str, limit, mask, traditional);

#ifdef MAP_CHARACTER
      if (ISPRINT (c))
	c = MAP_CHARACTER (c);
#endif
      
      /* Merge character into result; ignore excess chars.  */
      if (++chars_seen <= max_chars)
	{
	  if (width < HOST_BITS_PER_WIDE_INT)
	    result = (result << width) | (c & mask);
	  else
	    result = c;
	}
    }

  if (chars_seen == 0)
    cpp_error (pfile, "empty character constant");
  else if (chars_seen > max_chars)
    {
      chars_seen = max_chars;
      cpp_warning (pfile, "character constant too long");
    }
  else if (chars_seen > 1 && !traditional && warn_multi)
    cpp_warning (pfile, "multi-character character constant");

  /* If char type is signed, sign-extend the constant.  The
     __CHAR_UNSIGNED__ macro is set by the driver if appropriate.  */
  if (token->type == CPP_CHAR && chars_seen)
    {
      unsigned int nbits = chars_seen * width;

      mask = (unsigned HOST_WIDE_INT) ~0 >> (HOST_BITS_PER_WIDE_INT - nbits);
      if (pfile->spec_nodes.n__CHAR_UNSIGNED__->type == NT_MACRO
	  || ((result >> (nbits - 1)) & 1) == 0)
	result &= mask;
      else
	result |= ~mask;
    }

  *pchars_seen = chars_seen;
  return result;
}

/* Memory buffers.  Changing these three constants can have a dramatic
   effect on performance.  The values here are reasonable defaults,
   but might be tuned.  If you adjust them, be sure to test across a
   range of uses of cpplib, including heavy nested function-like macro
   expansion.  Also check the change in peak memory usage (NJAMD is a
   good tool for this).  */
#define MIN_BUFF_SIZE 8000
#define BUFF_SIZE_UPPER_BOUND(MIN_SIZE) (MIN_BUFF_SIZE + (MIN_SIZE) * 3 / 2)
#define EXTENDED_BUFF_SIZE(BUFF, MIN_EXTRA) \
	(MIN_EXTRA + ((BUFF)->limit - (BUFF)->cur) * 2)

#if MIN_BUFF_SIZE > BUFF_SIZE_UPPER_BOUND (0)
  #error BUFF_SIZE_UPPER_BOUND must be at least as large as MIN_BUFF_SIZE!
#endif

struct dummy
{
  char c;
  union
  {
    double d;
    int *p;
  } u;
};

#define DEFAULT_ALIGNMENT (offsetof (struct dummy, u))
#define CPP_ALIGN(size, align) (((size) + ((align) - 1)) & ~((align) - 1))

/* Create a new allocation buffer.  Place the control block at the end
   of the buffer, so that buffer overflows will cause immediate chaos.  */
static _cpp_buff *
new_buff (len)
     size_t len;
{
  _cpp_buff *result;
  unsigned char *base;

  if (len < MIN_BUFF_SIZE)
    len = MIN_BUFF_SIZE;
  len = CPP_ALIGN (len, DEFAULT_ALIGNMENT);

  base = xmalloc (len + sizeof (_cpp_buff));
  result = (_cpp_buff *) (base + len);
  result->base = base;
  result->cur = base;
  result->limit = base + len;
  result->next = NULL;
  return result;
}

/* Place a chain of unwanted allocation buffers on the free list.  */
void
_cpp_release_buff (pfile, buff)
     cpp_reader *pfile;
     _cpp_buff *buff;
{
  _cpp_buff *end = buff;

  while (end->next)
    end = end->next;
  end->next = pfile->free_buffs;
  pfile->free_buffs = buff;
}

/* Return a free buffer of size at least MIN_SIZE.  */
_cpp_buff *
_cpp_get_buff (pfile, min_size)
     cpp_reader *pfile;
     size_t min_size;
{
  _cpp_buff *result, **p;

  for (p = &pfile->free_buffs;; p = &(*p)->next)
    {
      size_t size;

      if (*p == NULL)
	return new_buff (min_size);
      result = *p;
      size = result->limit - result->base;
      /* Return a buffer that's big enough, but don't waste one that's
         way too big.  */
      if (size >= min_size && size <= BUFF_SIZE_UPPER_BOUND (min_size))
	break;
    }

  *p = result->next;
  result->next = NULL;
  result->cur = result->base;
  return result;
}

/* Creates a new buffer with enough space to hold the uncommitted
   remaining bytes of BUFF, and at least MIN_EXTRA more bytes.  Copies
   the excess bytes to the new buffer.  Chains the new buffer after
   BUFF, and returns the new buffer.  */
_cpp_buff *
_cpp_append_extend_buff (pfile, buff, min_extra)
     cpp_reader *pfile;
     _cpp_buff *buff;
     size_t min_extra;
{
  size_t size = EXTENDED_BUFF_SIZE (buff, min_extra);
  _cpp_buff *new_buff = _cpp_get_buff (pfile, size);

  buff->next = new_buff;
  memcpy (new_buff->base, buff->cur, BUFF_ROOM (buff));
  return new_buff;
}

/* Creates a new buffer with enough space to hold the uncommitted
   remaining bytes of the buffer pointed to by BUFF, and at least
   MIN_EXTRA more bytes.  Copies the excess bytes to the new buffer.
   Chains the new buffer before the buffer pointed to by BUFF, and
   updates the pointer to point to the new buffer.  */
void
_cpp_extend_buff (pfile, pbuff, min_extra)
     cpp_reader *pfile;
     _cpp_buff **pbuff;
     size_t min_extra;
{
  _cpp_buff *new_buff, *old_buff = *pbuff;
  size_t size = EXTENDED_BUFF_SIZE (old_buff, min_extra);

  new_buff = _cpp_get_buff (pfile, size);
  memcpy (new_buff->base, old_buff->cur, BUFF_ROOM (old_buff));
  new_buff->next = old_buff;
  *pbuff = new_buff;
}

/* Free a chain of buffers starting at BUFF.  */
void
_cpp_free_buff (buff)
     _cpp_buff *buff;
{
  _cpp_buff *next;

  for (; buff; buff = next)
    {
      next = buff->next;
      free (buff->base);
    }
}

/* Allocate permanent, unaligned storage of length LEN.  */
unsigned char *
_cpp_unaligned_alloc (pfile, len)
     cpp_reader *pfile;
     size_t len;
{
  _cpp_buff *buff = pfile->u_buff;
  unsigned char *result = buff->cur;

  if (len > (size_t) (buff->limit - result))
    {
      buff = _cpp_get_buff (pfile, len);
      buff->next = pfile->u_buff;
      pfile->u_buff = buff;
      result = buff->cur;
    }

  buff->cur = result + len;
  return result;
}

/* Allocate permanent, unaligned storage of length LEN from a_buff.
   That buffer is used for growing allocations when saving macro
   replacement lists in a #define, and when parsing an answer to an
   assertion in #assert, #unassert or #if (and therefore possibly
   whilst expanding macros).  It therefore must not be used by any
   code that they might call: specifically the lexer and the guts of
   the macro expander.

   All existing other uses clearly fit this restriction: storing
   registered pragmas during initialization.  */
unsigned char *
_cpp_aligned_alloc (pfile, len)
     cpp_reader *pfile;
     size_t len;
{
  _cpp_buff *buff = pfile->a_buff;
  unsigned char *result = buff->cur;

  if (len > (size_t) (buff->limit - result))
    {
      buff = _cpp_get_buff (pfile, len);
      buff->next = pfile->a_buff;
      pfile->a_buff = buff;
      result = buff->cur;
    }

  buff->cur = result + len;
  return result;
}
