/* Part of CPP library.  (Macro and #define handling.)
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "cpphash.h"

struct cpp_macro
{
  cpp_hashnode **params;	/* Parameters, if any.  */
  cpp_token *expansion;		/* First token of replacement list.  */
  unsigned int line;		/* Starting line number.  */
  unsigned int count;		/* Number of tokens in expansion.  */
  unsigned short paramc;	/* Number of parameters.  */
  unsigned int fun_like : 1;	/* If a function-like macro.  */
  unsigned int variadic : 1;	/* If a variadic macro.  */
  unsigned int syshdr   : 1;	/* If macro defined in system header.  */
};

typedef struct macro_arg macro_arg;
struct macro_arg
{
  const cpp_token **first;	/* First token in unexpanded argument.  */
  const cpp_token **expanded;	/* Macro-expanded argument.  */
  const cpp_token *stringified;	/* Stringified argument.  */
  unsigned int count;		/* # of tokens in argument.  */
  unsigned int expanded_count;	/* # of tokens in expanded argument.  */
};

/* Macro expansion.  */

static int enter_macro_context PARAMS ((cpp_reader *, cpp_hashnode *));
static int builtin_macro PARAMS ((cpp_reader *, cpp_hashnode *));
static void push_token_context
  PARAMS ((cpp_reader *, cpp_hashnode *, const cpp_token *, unsigned int));
static void push_ptoken_context
  PARAMS ((cpp_reader *, cpp_hashnode *, _cpp_buff *,
	   const cpp_token **, unsigned int));
static _cpp_buff *collect_args PARAMS ((cpp_reader *, const cpp_hashnode *));
static cpp_context *next_context PARAMS ((cpp_reader *));
static const cpp_token *padding_token
  PARAMS ((cpp_reader *, const cpp_token *));
static void expand_arg PARAMS ((cpp_reader *, macro_arg *));
static const cpp_token *new_string_token PARAMS ((cpp_reader *, U_CHAR *,
						  unsigned int));
static const cpp_token *new_number_token PARAMS ((cpp_reader *, unsigned int));
static const cpp_token *stringify_arg PARAMS ((cpp_reader *, macro_arg *));
static void paste_all_tokens PARAMS ((cpp_reader *, const cpp_token *));
static bool paste_tokens PARAMS ((cpp_reader *, const cpp_token **,
				  const cpp_token *));
static void replace_args PARAMS ((cpp_reader *, cpp_hashnode *, macro_arg *));
static _cpp_buff *funlike_invocation_p PARAMS ((cpp_reader *, cpp_hashnode *));

/* #define directive parsing and handling.  */

static cpp_token *alloc_expansion_token PARAMS ((cpp_reader *, cpp_macro *));
static cpp_token *lex_expansion_token PARAMS ((cpp_reader *, cpp_macro *));
static int warn_of_redefinition PARAMS ((const cpp_hashnode *,
					 const cpp_macro *));
static int save_parameter PARAMS ((cpp_reader *, cpp_macro *, cpp_hashnode *));
static int parse_params PARAMS ((cpp_reader *, cpp_macro *));
static void check_trad_stringification PARAMS ((cpp_reader *,
						const cpp_macro *,
						const cpp_string *));

/* Allocates and returns a CPP_STRING token, containing TEXT of length
   LEN, after null-terminating it.  TEXT must be in permanent storage.  */
static const cpp_token *
new_string_token (pfile, text, len)
     cpp_reader *pfile;
     unsigned char *text;
     unsigned int len;
{
  cpp_token *token = _cpp_temp_token (pfile);

  text[len] = '\0';
  token->type = CPP_STRING;
  token->val.str.len = len;
  token->val.str.text = text;
  token->flags = 0;
  return token;
}

/* Allocates and returns a CPP_NUMBER token evaluating to NUMBER.  */
static const cpp_token *
new_number_token (pfile, number)
     cpp_reader *pfile;
     unsigned int number;
{
  cpp_token *token = _cpp_temp_token (pfile);
  /* 21 bytes holds all NUL-terminated unsigned 64-bit numbers.  */
  unsigned char *buf = _cpp_unaligned_alloc (pfile, 21);

  sprintf ((char *) buf, "%u", number);
  token->type = CPP_NUMBER;
  token->val.str.text = buf;
  token->val.str.len = ustrlen (buf);
  token->flags = 0;
  return token;
}

static const char * const monthnames[] =
{
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

/* Handle builtin macros like __FILE__, and push the resulting token
   on the context stack.  Also handles _Pragma, for which no new token
   is created.  Returns 1 if it generates a new token context, 0 to
   return the token to the caller.  */
static int
builtin_macro (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  const cpp_token *result;

  switch (node->value.builtin)
    {
    default:
      cpp_ice (pfile, "invalid built-in macro \"%s\"", NODE_NAME (node));
      return 0;

    case BT_FILE:
    case BT_BASE_FILE:
      {
	unsigned int len;
	const char *name;
	U_CHAR *buf;
	const struct line_map *map = pfile->map;

	if (node->value.builtin == BT_BASE_FILE)
	  while (! MAIN_FILE_P (map))
	    map = INCLUDED_FROM (&pfile->line_maps, map);

	name = map->to_file;
	len = strlen (name);
	buf = _cpp_unaligned_alloc (pfile, len * 4 + 1);
	len = cpp_quote_string (buf, (const unsigned char *) name, len) - buf;

	result = new_string_token (pfile, buf, len);
      }
      break;

    case BT_INCLUDE_LEVEL:
      /* The line map depth counts the primary source as level 1, but
	 historically __INCLUDE_DEPTH__ has called the primary source
	 level 0.  */
      result = new_number_token (pfile, pfile->line_maps.depth - 1);
      break;

    case BT_SPECLINE:
      /* If __LINE__ is embedded in a macro, it must expand to the
	 line of the macro's invocation, not its definition.
	 Otherwise things like assert() will not work properly.  */
      result = new_number_token (pfile,
				 SOURCE_LINE (pfile->map,
					      pfile->cur_token[-1].line));
      break;

    case BT_STDC:
      {
	int stdc = (!CPP_IN_SYSTEM_HEADER (pfile)
		    || pfile->spec_nodes.n__STRICT_ANSI__->type != NT_VOID);
	result = new_number_token (pfile, stdc);
      }
      break;

    case BT_DATE:
    case BT_TIME:
      if (pfile->date.type == CPP_EOF)
	{
	  /* Allocate __DATE__ and __TIME__ strings from permanent
	     storage.  We only do this once, and don't generate them
	     at init time, because time() and localtime() are very
	     slow on some systems.  */
	  time_t tt = time (NULL);
	  struct tm *tb = localtime (&tt);

	  pfile->date.val.str.text =
	    _cpp_unaligned_alloc (pfile, sizeof ("Oct 11 1347"));
	  pfile->date.val.str.len = sizeof ("Oct 11 1347") - 1;
	  pfile->date.type = CPP_STRING;
	  pfile->date.flags = 0;
	  sprintf ((char *) pfile->date.val.str.text, "%s %2d %4d",
		   monthnames[tb->tm_mon], tb->tm_mday, tb->tm_year + 1900);

	  pfile->time.val.str.text =
	    _cpp_unaligned_alloc (pfile, sizeof ("12:34:56"));
	  pfile->time.val.str.len = sizeof ("12:34:56") - 1;
	  pfile->time.type = CPP_STRING;
	  pfile->time.flags = 0;
	  sprintf ((char *) pfile->time.val.str.text, "%02d:%02d:%02d",
		   tb->tm_hour, tb->tm_min, tb->tm_sec);
	}

      if (node->value.builtin == BT_DATE)
	result = &pfile->date;
      else
	result = &pfile->time;
      break;

    case BT_PRAGMA:
      /* Don't interpret _Pragma within directives.  The standard is
         not clear on this, but to me this makes most sense.  */
      if (pfile->state.in_directive)
	return 0;

      _cpp_do__Pragma (pfile);
      return 1;
    }

  push_token_context (pfile, NULL, result, 1);
  return 1;
}

/* Copies SRC, of length LEN, to DEST, adding backslashes before all
   backslashes and double quotes.  Non-printable characters are
   converted to octal.  DEST must be of sufficient size.  Returns
   a pointer to the end of the string.  */
U_CHAR *
cpp_quote_string (dest, src, len)
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

/* Convert a token sequence ARG to a single string token according to
   the rules of the ISO C #-operator.  */
static const cpp_token *
stringify_arg (pfile, arg)
     cpp_reader *pfile;
     macro_arg *arg;
{
  unsigned char *dest = BUFF_FRONT (pfile->u_buff);
  unsigned int i, escape_it, backslash_count = 0;
  const cpp_token *source = NULL;
  size_t len;

  /* Loop, reading in the argument's tokens.  */
  for (i = 0; i < arg->count; i++)
    {
      const cpp_token *token = arg->first[i];

      if (token->type == CPP_PADDING)
	{
	  if (source == NULL)
	    source = token->val.source;
	  continue;
	}

      escape_it = (token->type == CPP_STRING || token->type == CPP_WSTRING
		   || token->type == CPP_CHAR || token->type == CPP_WCHAR);

      /* Room for each char being written in octal, initial space and
	 final NUL.  */
      len = cpp_token_len (token);
      if (escape_it)
	len *= 4;
      len += 2;

      if ((size_t) (BUFF_LIMIT (pfile->u_buff) - dest) < len)
	{
	  size_t len_so_far = dest - BUFF_FRONT (pfile->u_buff);
	  _cpp_extend_buff (pfile, &pfile->u_buff, len);
	  dest = BUFF_FRONT (pfile->u_buff) + len_so_far;
	}

      /* Leading white space?  */
      if (dest != BUFF_FRONT (pfile->u_buff))
	{
	  if (source == NULL)
	    source = token;
	  if (source->flags & PREV_WHITE)
	    *dest++ = ' ';
	}
      source = NULL;

      if (escape_it)
	{
	  _cpp_buff *buff = _cpp_get_buff (pfile, len);
	  unsigned char *buf = BUFF_FRONT (buff);
	  len = cpp_spell_token (pfile, token, buf) - buf;
	  dest = cpp_quote_string (dest, buf, len);
	  _cpp_release_buff (pfile, buff);
	}
      else
	dest = cpp_spell_token (pfile, token, dest);

      if (token->type == CPP_OTHER && token->val.c == '\\')
	backslash_count++;
      else
	backslash_count = 0;
    }

  /* Ignore the final \ of invalid string literals.  */
  if (backslash_count & 1)
    {
      cpp_warning (pfile, "invalid string literal, ignoring final '\\'");
      dest--;
    }

  /* Commit the memory, including NUL, and return the token.  */
  if ((size_t) (BUFF_LIMIT (pfile->u_buff) - dest) < 1)
    {
      size_t len_so_far = dest - BUFF_FRONT (pfile->u_buff);
      _cpp_extend_buff (pfile, &pfile->u_buff, 1);
      dest = BUFF_FRONT (pfile->u_buff) + len_so_far;
    }
  len = dest - BUFF_FRONT (pfile->u_buff);
  BUFF_FRONT (pfile->u_buff) = dest + 1;
  return new_string_token (pfile, dest - len, len);
}

/* Try to paste two tokens.  On success, return non-zero.  In any
   case, PLHS is updated to point to the pasted token, which is
   guaranteed to not have the PASTE_LEFT flag set.  */
static bool
paste_tokens (pfile, plhs, rhs)
     cpp_reader *pfile;
     const cpp_token **plhs, *rhs;
{
  unsigned char *buf, *end;
  const cpp_token *lhs;
  unsigned int len;
  bool valid;

  lhs = *plhs;
  len = cpp_token_len (lhs) + cpp_token_len (rhs) + 1;
  buf = (unsigned char *) alloca (len);
  end = cpp_spell_token (pfile, lhs, buf);

  /* Avoid comment headers, since they are still processed in stage 3.
     It is simpler to insert a space here, rather than modifying the
     lexer to ignore comments in some circumstances.  Simply returning
     false doesn't work, since we want to clear the PASTE_LEFT flag.  */
  if (lhs->type == CPP_DIV
      && (rhs->type == CPP_MULT || rhs->type == CPP_DIV))
    *end++ = ' ';
  end = cpp_spell_token (pfile, rhs, end);
  *end = '\0';

  cpp_push_buffer (pfile, buf, end - buf, /* from_stage3 */ true, 1);

  /* Tweak the column number the lexer will report.  */
  pfile->buffer->col_adjust = pfile->cur_token[-1].col - 1;

  /* We don't want a leading # to be interpreted as a directive.  */
  pfile->buffer->saved_flags = 0;

  /* Set pfile->cur_token as required by _cpp_lex_direct.  */
  pfile->cur_token = _cpp_temp_token (pfile);
  *plhs = _cpp_lex_direct (pfile);
  valid = pfile->buffer->cur == pfile->buffer->rlimit;
  _cpp_pop_buffer (pfile);

  return valid;
}

/* Handles an arbitrarily long sequence of ## operators, with initial
   operand LHS.  This implementation is left-associative,
   non-recursive, and finishes a paste before handling succeeding
   ones.  If a paste fails, we back up to the RHS of the failing ##
   operator before pushing the context containing the result of prior
   successful pastes, with the effect that the RHS appears in the
   output stream after the pasted LHS normally.  */
static void
paste_all_tokens (pfile, lhs)
     cpp_reader *pfile;
     const cpp_token *lhs;
{
  const cpp_token *rhs;
  cpp_context *context = pfile->context;

  do
    {
      /* Take the token directly from the current context.  We can do
	 this, because we are in the replacement list of either an
	 object-like macro, or a function-like macro with arguments
	 inserted.  In either case, the constraints to #define
	 guarantee we have at least one more token.  */
      if (context->direct_p)
	rhs = context->first.token++;
      else
	rhs = *context->first.ptoken++;

      if (rhs->type == CPP_PADDING)
	abort ();

      if (!paste_tokens (pfile, &lhs, rhs))
	{
	  _cpp_backup_tokens (pfile, 1);

	  /* Mandatory warning for all apart from assembler.  */
	  if (CPP_OPTION (pfile, lang) != CLK_ASM)
	    cpp_warning (pfile,
	 "pasting \"%s\" and \"%s\" does not give a valid preprocessing token",
			 cpp_token_as_text (pfile, lhs),
			 cpp_token_as_text (pfile, rhs));
	  break;
	}
    }
  while (rhs->flags & PASTE_LEFT);

  /* Put the resulting token in its own context.  */
  push_token_context (pfile, NULL, lhs, 1);
}

/* Reads and returns the arguments to a function-like macro
   invocation.  Assumes the opening parenthesis has been processed.
   If there is an error, emits an appropriate diagnostic and returns
   NULL.  Each argument is terminated by a CPP_EOF token, for the
   future benefit of expand_arg().  */
static _cpp_buff *
collect_args (pfile, node)
     cpp_reader *pfile;
     const cpp_hashnode *node;
{
  _cpp_buff *buff, *base_buff;
  cpp_macro *macro;
  macro_arg *args, *arg;
  const cpp_token *token;
  unsigned int argc;
  bool error = false;

  macro = node->value.macro;
  if (macro->paramc)
    argc = macro->paramc;
  else
    argc = 1;
  buff = _cpp_get_buff (pfile, argc * (50 * sizeof (cpp_token *)
				       + sizeof (macro_arg)));
  base_buff = buff;
  args = (macro_arg *) buff->base;
  memset (args, 0, argc * sizeof (macro_arg));
  buff->cur = (unsigned char *) &args[argc];
  arg = args, argc = 0;

  /* Collect the tokens making up each argument.  We don't yet know
     how many arguments have been supplied, whether too many or too
     few.  Hence the slightly bizarre usage of "argc" and "arg".  */
  do
    {
      unsigned int paren_depth = 0;
      unsigned int ntokens = 0;

      argc++;
      arg->first = (const cpp_token **) buff->cur;

      for (;;)
	{
	  /* Require space for 2 new tokens (including a CPP_EOF).  */
	  if ((unsigned char *) &arg->first[ntokens + 2] > buff->limit)
	    {
	      buff = _cpp_append_extend_buff (pfile, buff,
					      1000 * sizeof (cpp_token *));
	      arg->first = (const cpp_token **) buff->cur;
	    }

	  token = cpp_get_token (pfile);

	  if (token->type == CPP_PADDING)
	    {
	      /* Drop leading padding.  */
	      if (ntokens == 0)
		continue;
	    }
	  else if (token->type == CPP_OPEN_PAREN)
	    paren_depth++;
	  else if (token->type == CPP_CLOSE_PAREN)
	    {
	      if (paren_depth-- == 0)
		break;
	    }
	  else if (token->type == CPP_COMMA)
	    {
	      /* A comma does not terminate an argument within
		 parentheses or as part of a variable argument.  */
	      if (paren_depth == 0
		  && ! (macro->variadic && argc == macro->paramc))
		break;
	    }
	  else if (token->type == CPP_EOF
		   || (token->type == CPP_HASH && token->flags & BOL))
	    break;

	  arg->first[ntokens++] = token;
	}

      /* Drop trailing padding.  */
      while (ntokens > 0 && arg->first[ntokens - 1]->type == CPP_PADDING)
	ntokens--;

      arg->count = ntokens;
      arg->first[ntokens] = &pfile->eof;

      /* Terminate the argument.  Excess arguments loop back and
	 overwrite the final legitimate argument, before failing.  */
      if (argc <= macro->paramc)
	{
	  buff->cur = (unsigned char *) &arg->first[ntokens + 1];
	  if (argc != macro->paramc)
	    arg++;
	}
    }
  while (token->type != CPP_CLOSE_PAREN
	 && token->type != CPP_EOF
	 && token->type != CPP_HASH);

  if (token->type == CPP_EOF || token->type == CPP_HASH)
    {
      bool step_back = false;

      /* 6.10.3 paragraph 11: If there are sequences of preprocessing
	 tokens within the list of arguments that would otherwise act
	 as preprocessing directives, the behavior is undefined.

	 This implementation will report a hard error, terminate the
	 macro invocation, and proceed to process the directive.  */
      if (token->type == CPP_HASH)
	{
	  cpp_error (pfile,
		     "directives may not be used inside a macro argument");
	  step_back = true;
	}
      else
	step_back = (pfile->context->prev || pfile->state.in_directive);

      /* We still need the CPP_EOF to end directives, and to end
	 pre-expansion of a macro argument.  Step back is not
	 unconditional, since we don't want to return a CPP_EOF to our
	 callers at the end of an -include-d file.  */
      if (step_back)
	_cpp_backup_tokens (pfile, 1);
      cpp_error (pfile, "unterminated argument list invoking macro \"%s\"",
		 NODE_NAME (node));
      error = true;
    }
  else if (argc < macro->paramc)
    {
      /* As an extension, a rest argument is allowed to not appear in
	 the invocation at all.
	 e.g. #define debug(format, args...) something
	 debug("string");
	 
	 This is exactly the same as if there had been an empty rest
	 argument - debug("string", ).  */

      if (argc + 1 == macro->paramc && macro->variadic)
	{
	  if (CPP_PEDANTIC (pfile) && ! macro->syshdr)
	    cpp_pedwarn (pfile, "ISO C99 requires rest arguments to be used");
	}
      else
	{
	  cpp_error (pfile,
		     "macro \"%s\" requires %u arguments, but only %u given",
		     NODE_NAME (node), macro->paramc, argc);
	  error = true;
	}
    }
  else if (argc > macro->paramc)
    {
      /* Empty argument to a macro taking no arguments is OK.  */
      if (argc != 1 || arg->count)
	{
	  cpp_error (pfile,
		     "macro \"%s\" passed %u arguments, but takes just %u",
		     NODE_NAME (node), argc, macro->paramc);
	  error = true;
	}
    }

  if (!error)
    {
      /* GCC has special semantics for , ## b where b is a varargs
	 parameter: we remove the comma if b was omitted entirely.
	 If b was merely an empty argument, the comma is retained.
	 If the macro takes just one (varargs) parameter, then we
	 retain the comma only if we are standards conforming.

	 If FIRST is NULL replace_args () swallows the comma.  */
      if (macro->variadic && (argc < macro->paramc
			      || (argc == 1 && args[0].count == 0
				  && !CPP_OPTION (pfile, std))))
	args[macro->paramc - 1].first = NULL;
      return base_buff;
    }

  _cpp_release_buff (pfile, base_buff);
  return NULL;
}

/* Search for an opening parenthesis to the macro of NODE, in such a
   way that, if none is found, we don't lose the information in any
   intervening padding tokens.  If we find the parenthesis, collect
   the arguments and return the buffer containing them.  */
static _cpp_buff *
funlike_invocation_p (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  const cpp_token *token, *padding = NULL;

  for (;;)
    {
      token = cpp_get_token (pfile);
      if (token->type != CPP_PADDING)
	break;
      if (padding == NULL
	  || (!(padding->flags & PREV_WHITE) && token->val.source == NULL))
	padding = token;
    }

  if (token->type == CPP_OPEN_PAREN)
    {
      pfile->state.parsing_args = 2;
      return collect_args (pfile, node);
    }

  /* CPP_EOF can be the end of macro arguments, or the end of the
     file.  We mustn't back up over the latter.  Ugh.  */
  if (token->type != CPP_EOF || token == &pfile->eof)
    {
      /* Back up.  We may have skipped padding, in which case backing
	 up more than one token when expanding macros is in general
	 too difficult.  We re-insert it in its own context.  */
      _cpp_backup_tokens (pfile, 1);
      if (padding)
	push_token_context (pfile, NULL, padding, 1);
    }

  return NULL;
}

/* Push the context of a macro with hash entry NODE onto the context
   stack.  If we can successfully expand the macro, we push a context
   containing its yet-to-be-rescanned replacement list and return one.
   Otherwise, we don't push a context and return zero.  */
static int
enter_macro_context (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  /* The presence of a macro invalidates a file's controlling macro.  */
  pfile->mi_valid = false;

  pfile->state.angled_headers = false;

  /* Handle standard macros.  */
  if (! (node->flags & NODE_BUILTIN))
    {
      cpp_macro *macro = node->value.macro;

      if (macro->fun_like)
	{
	  _cpp_buff *buff;

	  pfile->state.prevent_expansion++;
	  pfile->keep_tokens++;
	  pfile->state.parsing_args = 1;
	  buff = funlike_invocation_p (pfile, node);
	  pfile->state.parsing_args = 0;
	  pfile->keep_tokens--;
	  pfile->state.prevent_expansion--;

	  if (buff == NULL)
	    {
	      if (CPP_WTRADITIONAL (pfile) && ! node->value.macro->syshdr)
		cpp_warning (pfile,
 "function-like macro \"%s\" must be used with arguments in traditional C",
			     NODE_NAME (node));

	      return 0;
	    }

	  if (node->value.macro->paramc > 0)
	    replace_args (pfile, node, (macro_arg *) buff->base);
	  _cpp_release_buff (pfile, buff);
	}

      /* Disable the macro within its expansion.  */
      node->flags |= NODE_DISABLED;

      if (macro->paramc == 0)
	push_token_context (pfile, node, macro->expansion, macro->count);

      return 1;
    }

  /* Handle built-in macros and the _Pragma operator.  */
  return builtin_macro (pfile, node);
}

/* Replace the parameters in a function-like macro of NODE with the
   actual ARGS, and place the result in a newly pushed token context.
   Expand each argument before replacing, unless it is operated upon
   by the # or ## operators.  */
static void
replace_args (pfile, node, args)
     cpp_reader *pfile;
     cpp_hashnode *node;
     macro_arg *args;
{
  unsigned int i, total;
  const cpp_token *src, *limit;
  const cpp_token **dest, **first;
  macro_arg *arg;
  _cpp_buff *buff;
  cpp_macro *macro;

  /* First, fully macro-expand arguments, calculating the number of
     tokens in the final expansion as we go.  The ordering of the if
     statements below is subtle; we must handle stringification before
     pasting.  */
  macro = node->value.macro;
  total = macro->count;
  limit = macro->expansion + macro->count;

  for (src = macro->expansion; src < limit; src++)
    if (src->type == CPP_MACRO_ARG)
      {
	/* Leading and trailing padding tokens.  */
	total += 2;

	/* We have an argument.  If it is not being stringified or
	   pasted it is macro-replaced before insertion.  */
	arg = &args[src->val.arg_no - 1];

	if (src->flags & STRINGIFY_ARG)
	  {
	    if (!arg->stringified)
	      arg->stringified = stringify_arg (pfile, arg);
	  }
	else if ((src->flags & PASTE_LEFT)
		 || (src > macro->expansion && (src[-1].flags & PASTE_LEFT)))
	  total += arg->count - 1;
	else
	  {
	    if (!arg->expanded)
	      expand_arg (pfile, arg);
	    total += arg->expanded_count - 1;
	  }
      }

  /* Now allocate space for the expansion, copy the tokens and replace
     the arguments.  */
  buff = _cpp_get_buff (pfile, total * sizeof (cpp_token *));
  first = (const cpp_token **) buff->base;
  dest = first;

  for (src = macro->expansion; src < limit; src++)
    {
      unsigned int count;
      const cpp_token **from, **paste_flag;

      if (src->type != CPP_MACRO_ARG)
	{
	  *dest++ = src;
	  continue;
	}

      paste_flag = 0;
      arg = &args[src->val.arg_no - 1];
      if (src->flags & STRINGIFY_ARG)
	count = 1, from = &arg->stringified;
      else if (src->flags & PASTE_LEFT)
	count = arg->count, from = arg->first;
      else if (src != macro->expansion && (src[-1].flags & PASTE_LEFT))
	{
	  count = arg->count, from = arg->first;
	  if (dest != first)
	    {
	      if (dest[-1]->type == CPP_COMMA
		  && macro->variadic
		  && src->val.arg_no == macro->paramc)
		{
		  /* Swallow a pasted comma if from == NULL, otherwise
		     drop the paste flag.  */
		  if (from == NULL)
		    dest--;
		  else
		    paste_flag = dest - 1;
		}
	      /* Remove the paste flag if the RHS is a placemarker.  */
	      else if (count == 0)
		paste_flag = dest - 1;
	    }
	}
      else
	count = arg->expanded_count, from = arg->expanded;

      /* Padding on the left of an argument (unless RHS of ##).  */
      if (!pfile->state.in_directive
	  && src != macro->expansion && !(src[-1].flags & PASTE_LEFT))
	*dest++ = padding_token (pfile, src);

      if (count)
	{
	  memcpy (dest, from, count * sizeof (cpp_token *));
	  dest += count;

	  /* With a non-empty argument on the LHS of ##, the last
	     token should be flagged PASTE_LEFT.  */
	  if (src->flags & PASTE_LEFT)
	    paste_flag = dest - 1;
	}

      /* Avoid paste on RHS (even case count == 0).  */
      if (!pfile->state.in_directive && !(src->flags & PASTE_LEFT))
	*dest++ = &pfile->avoid_paste;

      /* Add a new paste flag, or remove an unwanted one.  */
      if (paste_flag)
	{
	  cpp_token *token = _cpp_temp_token (pfile);
	  token->type = (*paste_flag)->type;
	  token->val.str = (*paste_flag)->val.str;
	  if (src->flags & PASTE_LEFT)
	    token->flags = (*paste_flag)->flags | PASTE_LEFT;
	  else
	    token->flags = (*paste_flag)->flags & ~PASTE_LEFT;
	  *paste_flag = token;
	}
    }

  /* Free the expanded arguments.  */
  for (i = 0; i < macro->paramc; i++)
    if (args[i].expanded)
      free (args[i].expanded);

  push_ptoken_context (pfile, node, buff, first, dest - first);
}

/* Return a special padding token, with padding inherited from SOURCE.  */
static const cpp_token *
padding_token (pfile, source)
     cpp_reader *pfile;
     const cpp_token *source;
{
  cpp_token *result = _cpp_temp_token (pfile);

  result->type = CPP_PADDING;
  result->val.source = source;
  result->flags = 0;
  return result;
}

/* Get a new uninitialized context.  Create a new one if we cannot
   re-use an old one.  */
static cpp_context *
next_context (pfile)
     cpp_reader *pfile;
{
  cpp_context *result = pfile->context->next;

  if (result == 0)
    {
      result = xnew (cpp_context);
      result->prev = pfile->context;
      result->next = 0;
      pfile->context->next = result;
    }

  pfile->context = result;
  return result;
}

/* Push a list of pointers to tokens.  */
static void
push_ptoken_context (pfile, macro, buff, first, count)
     cpp_reader *pfile;
     cpp_hashnode *macro;
     _cpp_buff *buff;
     const cpp_token **first;
     unsigned int count;
{
  cpp_context *context = next_context (pfile);

  context->direct_p = false;
  context->macro = macro;
  context->buff = buff;
  context->first.ptoken = first;
  context->last.ptoken = first + count;
}

/* Push a list of tokens.  */
static void
push_token_context (pfile, macro, first, count)
     cpp_reader *pfile;
     cpp_hashnode *macro;
     const cpp_token *first;
     unsigned int count;
{
  cpp_context *context = next_context (pfile);

  context->direct_p = true;
  context->macro = macro;
  context->buff = NULL;
  context->first.token = first;
  context->last.token = first + count;
}

/* Expand an argument ARG before replacing parameters in a
   function-like macro.  This works by pushing a context with the
   argument's tokens, and then expanding that into a temporary buffer
   as if it were a normal part of the token stream.  collect_args()
   has terminated the argument's tokens with a CPP_EOF so that we know
   when we have fully expanded the argument.  */
static void
expand_arg (pfile, arg)
     cpp_reader *pfile;
     macro_arg *arg;
{
  unsigned int capacity;

  if (arg->count == 0)
    return;

  /* Loop, reading in the arguments.  */
  capacity = 256;
  arg->expanded = (const cpp_token **)
    xmalloc (capacity * sizeof (cpp_token *));

  push_ptoken_context (pfile, NULL, NULL, arg->first, arg->count + 1);
  for (;;)
    {
      const cpp_token *token;

      if (arg->expanded_count + 1 >= capacity)
	{
	  capacity *= 2;
	  arg->expanded = (const cpp_token **)
	    xrealloc (arg->expanded, capacity * sizeof (cpp_token *));
	}

      token = cpp_get_token (pfile);

      if (token->type == CPP_EOF)
	break;

      arg->expanded[arg->expanded_count++] = token;
    }

  _cpp_pop_context (pfile);
}

/* Pop the current context off the stack, re-enabling the macro if the
   context represented a macro's replacement list.  The context
   structure is not freed so that we can re-use it later.  */
void
_cpp_pop_context (pfile)
     cpp_reader *pfile;
{
  cpp_context *context = pfile->context;

  if (context->macro)
    context->macro->flags &= ~NODE_DISABLED;

  if (context->buff)
    _cpp_release_buff (pfile, context->buff);

  pfile->context = context->prev;
}

/* Eternal routine to get a token.  Also used nearly everywhere
   internally, except for places where we know we can safely call
   the lexer directly, such as lexing a directive name.

   Macro expansions and directives are transparently handled,
   including entering included files.  Thus tokens are post-macro
   expansion, and after any intervening directives.  External callers
   see CPP_EOF only at EOF.  Internal callers also see it when meeting
   a directive inside a macro call, when at the end of a directive and
   state.in_directive is still 1, and at the end of argument
   pre-expansion.  */
const cpp_token *
cpp_get_token (pfile)
     cpp_reader *pfile;
{
  const cpp_token *result;

  for (;;)
    {
      cpp_hashnode *node;
      cpp_context *context = pfile->context;

      /* Context->prev == 0 <=> base context.  */
      if (!context->prev)
	result = _cpp_lex_token (pfile);
      else if (context->first.token != context->last.token)
	{
	  if (context->direct_p)
	    result = context->first.token++;
	  else
	    result = *context->first.ptoken++;

	  if (result->flags & PASTE_LEFT)
	    {
	      paste_all_tokens (pfile, result);
	      if (pfile->state.in_directive)
		continue;
	      return padding_token (pfile, result);
	    }
	}
      else
	{
	  _cpp_pop_context (pfile);
	  if (pfile->state.in_directive)
	    continue;
	  return &pfile->avoid_paste;
	}

      if (result->type != CPP_NAME)
	break;

      node = result->val.node;

      if (node->type != NT_MACRO || (result->flags & NO_EXPAND))
	break;
      
      if (!(node->flags & NODE_DISABLED))
	{
	  if (!pfile->state.prevent_expansion
	      && enter_macro_context (pfile, node))
	    {
	      if (pfile->state.in_directive)
		continue;
	      return padding_token (pfile, result);
	    }
	}
      else
	{
	  /* Flag this token as always unexpandable.  FIXME: move this
	     to collect_args()?.  */
	  cpp_token *t = _cpp_temp_token (pfile);
	  t->type = result->type;
	  t->flags = result->flags | NO_EXPAND;
	  t->val.str = result->val.str;
	  result = t;
	}

      break;
    }

  return result;
}

/* Returns true if we're expanding an object-like macro that was
   defined in a system header.  Just checks the macro at the top of
   the stack.  Used for diagnostic suppression.  */
int
cpp_sys_macro_p (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *node = pfile->context->macro;

  return node && node->value.macro && node->value.macro->syshdr;
}

/* Read each token in, until EOF.  Directives are transparently
   processed.  */
void
cpp_scan_nooutput (pfile)
     cpp_reader *pfile;
{
  while (cpp_get_token (pfile)->type != CPP_EOF)
    ;
}

/* Step back one (or more) tokens.  Can only step mack more than 1 if
   they are from the lexer, and not from macro expansion.  */
void
_cpp_backup_tokens (pfile, count)
     cpp_reader *pfile;
     unsigned int count;
{
  if (pfile->context->prev == NULL)
    {
      pfile->lookaheads += count;
      while (count--)
	{
	  pfile->cur_token--;
	  if (pfile->cur_token == pfile->cur_run->base
	      /* Possible with -fpreprocessed and no leading #line.  */
	      && pfile->cur_run->prev != NULL)
	    {
	      pfile->cur_run = pfile->cur_run->prev;
	      pfile->cur_token = pfile->cur_run->limit;
	    }
	}
    }
  else
    {
      if (count != 1)
	abort ();
      if (pfile->context->direct_p)
	pfile->context->first.token--;
      else
	pfile->context->first.ptoken--;
    }
}

/* #define directive parsing and handling.  */

/* Returns non-zero if a macro redefinition warning is required.  */
static int
warn_of_redefinition (node, macro2)
     const cpp_hashnode *node;
     const cpp_macro *macro2;
{
  const cpp_macro *macro1;
  unsigned int i;

  /* Some redefinitions need to be warned about regardless.  */
  if (node->flags & NODE_WARN)
    return 1;

  /* Redefinition of a macro is allowed if and only if the old and new
     definitions are the same.  (6.10.3 paragraph 2).  */
  macro1 = node->value.macro;

  /* The quick failures.  */
  if (macro1->count != macro2->count
      || macro1->paramc != macro2->paramc
      || macro1->fun_like != macro2->fun_like
      || macro1->variadic != macro2->variadic)
    return 1;

  /* Check each token.  */
  for (i = 0; i < macro1->count; i++)
    if (! _cpp_equiv_tokens (&macro1->expansion[i], &macro2->expansion[i]))
      return 1;

  /* Check parameter spellings.  */
  for (i = 0; i < macro1->paramc; i++)
    if (macro1->params[i] != macro2->params[i])
      return 1;

  return 0;
}

/* Free the definition of hashnode H.  */
void
_cpp_free_definition (h)
     cpp_hashnode *h;
{
  /* Macros and assertions no longer have anything to free.  */
  h->type = NT_VOID;
  /* Clear builtin flag in case of redefinition.  */
  h->flags &= ~(NODE_BUILTIN | NODE_DISABLED);
}

/* Save parameter NODE to the parameter list of macro MACRO.  Returns
   zero on success, non-zero if the parameter is a duplicate.  */
static int
save_parameter (pfile, macro, node)
     cpp_reader *pfile;
     cpp_macro *macro;
     cpp_hashnode *node;
{
  /* Constraint 6.10.3.6 - duplicate parameter names.  */
  if (node->arg_index)
    {
      cpp_error (pfile, "duplicate macro parameter \"%s\"", NODE_NAME (node));
      return 1;
    }

  if (BUFF_ROOM (pfile->a_buff)
      < (macro->paramc + 1) * sizeof (cpp_hashnode *))
    _cpp_extend_buff (pfile, &pfile->a_buff, sizeof (cpp_hashnode *));

  ((cpp_hashnode **) BUFF_FRONT (pfile->a_buff))[macro->paramc++] = node;
  node->arg_index = macro->paramc;
  return 0;
}

/* Check the syntax of the parameters in a MACRO definition.  */
static int
parse_params (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  unsigned int prev_ident = 0;

  for (;;)
    {
      const cpp_token *token = _cpp_lex_token (pfile);

      switch (token->type)
	{
	default:
	  cpp_error (pfile, "\"%s\" may not appear in macro parameter list",
		     cpp_token_as_text (pfile, token));
	  return 0;

	case CPP_NAME:
	  if (prev_ident)
	    {
	      cpp_error (pfile, "macro parameters must be comma-separated");
	      return 0;
	    }
	  prev_ident = 1;

	  if (save_parameter (pfile, macro, token->val.node))
	    return 0;
	  continue;

	case CPP_CLOSE_PAREN:
	  if (prev_ident || macro->paramc == 0)
	    return 1;

	  /* Fall through to pick up the error.  */
	case CPP_COMMA:
	  if (!prev_ident)
	    {
	      cpp_error (pfile, "parameter name missing");
	      return 0;
	    }
	  prev_ident = 0;
	  continue;

	case CPP_ELLIPSIS:
	  macro->variadic = 1;
	  if (!prev_ident)
	    {
	      save_parameter (pfile, macro, pfile->spec_nodes.n__VA_ARGS__);
	      pfile->state.va_args_ok = 1;
	      if (! CPP_OPTION (pfile, c99) && CPP_OPTION (pfile, pedantic))
		cpp_pedwarn (pfile,
		     "anonymous variadic macros were introduced in C99");
	    }
	  else if (CPP_OPTION (pfile, pedantic))
	    cpp_pedwarn (pfile, "ISO C does not permit named variadic macros");

	  /* We're at the end, and just expect a closing parenthesis.  */
	  token = _cpp_lex_token (pfile);
	  if (token->type == CPP_CLOSE_PAREN)
	    return 1;
	  /* Fall through.  */

	case CPP_EOF:
	  cpp_error (pfile, "missing ')' in macro parameter list");
	  return 0;
	}
    }
}

/* Allocate room for a token from a macro's replacement list.  */
static cpp_token *
alloc_expansion_token (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  if (BUFF_ROOM (pfile->a_buff) < (macro->count + 1) * sizeof (cpp_token))
    _cpp_extend_buff (pfile, &pfile->a_buff, sizeof (cpp_token));

  return &((cpp_token *) BUFF_FRONT (pfile->a_buff))[macro->count++];
}

/* Lex a token from the expansion of MACRO, but mark parameters as we
   find them and warn of traditional stringification.  */
static cpp_token *
lex_expansion_token (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  cpp_token *token;

  pfile->cur_token = alloc_expansion_token (pfile, macro);
  token = _cpp_lex_direct (pfile);

  /* Is this a parameter?  */
  if (token->type == CPP_NAME && token->val.node->arg_index)
    {
      token->type = CPP_MACRO_ARG;
      token->val.arg_no = token->val.node->arg_index;
    }
  else if (CPP_WTRADITIONAL (pfile) && macro->paramc > 0
	   && (token->type == CPP_STRING || token->type == CPP_CHAR))
    check_trad_stringification (pfile, macro, &token->val.str);

  return token;
}

/* Parse a macro and save its expansion.  Returns non-zero on success.  */
int
_cpp_create_definition (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  cpp_macro *macro;
  cpp_token *token, *saved_cur_token;
  const cpp_token *ctoken;
  unsigned int i, ok = 1;

  macro = (cpp_macro *) _cpp_aligned_alloc (pfile, sizeof (cpp_macro));
  macro->line = pfile->directive_line;
  macro->params = 0;
  macro->paramc = 0;
  macro->variadic = 0;
  macro->count = 0;
  macro->fun_like = 0;

  /* Get the first token of the expansion (or the '(' of a
     function-like macro).  */
  ctoken = _cpp_lex_token (pfile);

  if (ctoken->type == CPP_OPEN_PAREN && !(ctoken->flags & PREV_WHITE))
    {
      ok = parse_params (pfile, macro);
      macro->params = (cpp_hashnode **) BUFF_FRONT (pfile->a_buff);
      if (!ok)
	goto cleanup2;

      /* Success.  Commit the parameter array.  */
      BUFF_FRONT (pfile->a_buff) = (U_CHAR *) &macro->params[macro->paramc];
      macro->fun_like = 1;
    }
  else if (ctoken->type != CPP_EOF && !(ctoken->flags & PREV_WHITE))
    cpp_pedwarn (pfile, "ISO C requires whitespace after the macro name");

  saved_cur_token = pfile->cur_token;

  if (macro->fun_like)
    token = lex_expansion_token (pfile, macro);
  else
    {
      token = alloc_expansion_token (pfile, macro);
      *token = *ctoken;
    }

  for (;;)
    {
      /* Check the stringifying # constraint 6.10.3.2.1 of
	 function-like macros when lexing the subsequent token.  */
      if (macro->count > 1 && token[-1].type == CPP_HASH && macro->fun_like)
	{
	  if (token->type == CPP_MACRO_ARG)
	    {
	      token->flags &= ~PREV_WHITE;
	      token->flags |= STRINGIFY_ARG;
	      token->flags |= token[-1].flags & PREV_WHITE;
	      token[-1] = token[0];
	      macro->count--;
	    }
	  /* Let assembler get away with murder.  */
	  else if (CPP_OPTION (pfile, lang) != CLK_ASM)
	    {
	      ok = 0;
	      cpp_error (pfile, "'#' is not followed by a macro parameter");
	      goto cleanup1;
	    }
	}

      if (token->type == CPP_EOF)
	break;

      /* Paste operator constraint 6.10.3.3.1.  */
      if (token->type == CPP_PASTE)
	{
	  /* Token-paste ##, can appear in both object-like and
	     function-like macros, but not at the ends.  */
	  if (--macro->count > 0)
	    token = lex_expansion_token (pfile, macro);

	  if (macro->count == 0 || token->type == CPP_EOF)
	    {
	      ok = 0;
	      cpp_error (pfile,
			 "'##' cannot appear at either end of a macro expansion");
	      goto cleanup1;
	    }

	  token[-1].flags |= PASTE_LEFT;
	}

      token = lex_expansion_token (pfile, macro);
    }

  macro->expansion = (cpp_token *) BUFF_FRONT (pfile->a_buff);

  /* Don't count the CPP_EOF.  */
  macro->count--;

  /* Clear whitespace on first token for warn_of_redefinition().  */
  if (macro->count)
    macro->expansion[0].flags &= ~PREV_WHITE;

  /* Commit the memory.  */
  BUFF_FRONT (pfile->a_buff) = (U_CHAR *) &macro->expansion[macro->count];

  /* Implement the macro-defined-to-itself optimisation.  */
  if (macro->count == 1 && !macro->fun_like
      && macro->expansion[0].type == CPP_NAME
      && macro->expansion[0].val.node == node)
    node->flags |= NODE_DISABLED;

  /* To suppress some diagnostics.  */
  macro->syshdr = pfile->map->sysp != 0;

  if (node->type != NT_VOID)
    {
      if (warn_of_redefinition (node, macro))
	{
	  cpp_pedwarn_with_line (pfile, pfile->directive_line, 0,
				 "\"%s\" redefined", NODE_NAME (node));

	  if (node->type == NT_MACRO && !(node->flags & NODE_BUILTIN))
	    cpp_pedwarn_with_line (pfile, node->value.macro->line, 0,
			    "this is the location of the previous definition");
	}
      _cpp_free_definition (node);
    }

  /* Enter definition in hash table.  */
  node->type = NT_MACRO;
  node->value.macro = macro;
  if (! ustrncmp (NODE_NAME (node), DSC ("__STDC_")))
    node->flags |= NODE_WARN;

 cleanup1:

  /* Set type for SEEN_EOL() in cpplib.c, restore the lexer position.  */
  saved_cur_token[-1].type = pfile->cur_token[-1].type;
  pfile->cur_token = saved_cur_token;

 cleanup2:

  /* Stop the lexer accepting __VA_ARGS__.  */
  pfile->state.va_args_ok = 0;

  /* Clear the fast argument lookup indices.  */
  for (i = macro->paramc; i-- > 0; )
    macro->params[i]->arg_index = 0;

  return ok;
}

/* Warn if a token in STRING matches one of a function-like MACRO's
   parameters.  */
static void
check_trad_stringification (pfile, macro, string)
     cpp_reader *pfile;
     const cpp_macro *macro;
     const cpp_string *string;
{
  unsigned int i, len;
  const U_CHAR *p, *q, *limit = string->text + string->len;
  
  /* Loop over the string.  */
  for (p = string->text; p < limit; p = q)
    {
      /* Find the start of an identifier.  */
      while (p < limit && !is_idstart (*p))
	p++;

      /* Find the end of the identifier.  */
      q = p;
      while (q < limit && is_idchar (*q))
	q++;

      len = q - p;

      /* Loop over the function macro arguments to see if the
	 identifier inside the string matches one of them.  */
      for (i = 0; i < macro->paramc; i++)
	{
	  const cpp_hashnode *node = macro->params[i];

	  if (NODE_LEN (node) == len
	      && !memcmp (p, NODE_NAME (node), len))
	    {
	      cpp_warning (pfile,
	   "macro argument \"%s\" would be stringified with -traditional",
			   NODE_NAME (node));
	      break;
	    }
	}
    }
}

/* Returns the name, arguments and expansion of a macro, in a format
   suitable to be read back in again, and therefore also for DWARF 2
   debugging info.  e.g. "PASTE(X, Y) X ## Y", or "MACNAME EXPANSION".
   Caller is expected to generate the "#define" bit if needed.  The
   returned text is temporary, and automatically freed later.  */
const unsigned char *
cpp_macro_definition (pfile, node)
     cpp_reader *pfile;
     const cpp_hashnode *node;
{
  unsigned int i, len;
  const cpp_macro *macro = node->value.macro;
  unsigned char *buffer;

  if (node->type != NT_MACRO || (node->flags & NODE_BUILTIN))
    {
      cpp_ice (pfile, "invalid hash type %d in cpp_macro_definition", node->type);
      return 0;
    }

  /* Calculate length.  */
  len = NODE_LEN (node) + 2;			/* ' ' and NUL.  */
  if (macro->fun_like)
    {
      len += 4;		/* "()" plus possible final ".." of named
			   varargs (we have + 1 below).  */
      for (i = 0; i < macro->paramc; i++)
	len += NODE_LEN (macro->params[i]) + 1; /* "," */
    }

  for (i = 0; i < macro->count; i++)
    {
      cpp_token *token = &macro->expansion[i];

      if (token->type == CPP_MACRO_ARG)
	len += NODE_LEN (macro->params[token->val.arg_no - 1]);
      else
	len += cpp_token_len (token); /* Includes room for ' '.  */
      if (token->flags & STRINGIFY_ARG)
	len++;			/* "#" */
      if (token->flags & PASTE_LEFT)
	len += 3;		/* " ##" */
    }

  if (len > pfile->macro_buffer_len)
    {
      pfile->macro_buffer = (U_CHAR *) xrealloc (pfile->macro_buffer, len);
      pfile->macro_buffer_len = len;
    }

  /* Fill in the buffer.  Start with the macro name.  */
  buffer = pfile->macro_buffer;
  memcpy (buffer, NODE_NAME (node), NODE_LEN (node));
  buffer += NODE_LEN (node);

  /* Parameter names.  */
  if (macro->fun_like)
    {
      *buffer++ = '(';
      for (i = 0; i < macro->paramc; i++)
	{
	  cpp_hashnode *param = macro->params[i];

	  if (param != pfile->spec_nodes.n__VA_ARGS__)
	    {
	      memcpy (buffer, NODE_NAME (param), NODE_LEN (param));
	      buffer += NODE_LEN (param);
	    }

	  if (i + 1 < macro->paramc)
            /* Don't emit a space after the comma here; we're trying
               to emit a Dwarf-friendly definition, and the Dwarf spec
               forbids spaces in the argument list.  */
	    *buffer++ = ',';
	  else if (macro->variadic)
	    *buffer++ = '.', *buffer++ = '.', *buffer++ = '.';
	}
      *buffer++ = ')';
    }

  /* The Dwarf spec requires a space after the macro name, even if the
     definition is the empty string.  */
  *buffer++ = ' ';

  /* Expansion tokens.  */
  if (macro->count)
    {
      for (i = 0; i < macro->count; i++)
	{
	  cpp_token *token = &macro->expansion[i];

	  if (token->flags & PREV_WHITE)
	    *buffer++ = ' ';
	  if (token->flags & STRINGIFY_ARG)
	    *buffer++ = '#';

	  if (token->type == CPP_MACRO_ARG)
	    {
	      len = NODE_LEN (macro->params[token->val.arg_no - 1]);
	      memcpy (buffer,
		      NODE_NAME (macro->params[token->val.arg_no - 1]), len);
	      buffer += len;
	    }
	  else
	    buffer = cpp_spell_token (pfile, token, buffer);

	  if (token->flags & PASTE_LEFT)
	    {
	      *buffer++ = ' ';
	      *buffer++ = '#';
	      *buffer++ = '#';
	      /* Next has PREV_WHITE; see _cpp_create_definition.  */
	    }
	}
    }

  *buffer = '\0';
  return pfile->macro_buffer;
}
