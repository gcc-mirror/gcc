/* Part of CPP library.  (Macro and #define handling.)
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1998,
   1999, 2000 Free Software Foundation, Inc.
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
#include "intl.h"		/* for _("<command line>") below.  */
#include "cpplib.h"
#include "cpphash.h"

struct cpp_macro
{
  cpp_hashnode **params;	/* Parameters, if any.  */
  cpp_token *expansion;		/* First token of replacement list.   */
  const char *file;		/* Defined in file name.  */
  unsigned int line;		/* Starting line number.  */
  unsigned int count;		/* Number of tokens in expansion.  */
  unsigned short paramc;	/* Number of parameters.  */
  unsigned int fun_like : 1;	/* If a function-like macro.  */
  unsigned int var_args : 1;	/* If a variable-args macro.  */
  unsigned int disabled : 1;	/* If macro is disabled.  */
};

typedef struct macro_arg macro_arg;
struct macro_arg
{
  cpp_token *first;		/* First token in unexpanded argument.  */
  cpp_token *expanded;		/* Macro-expanded argument.   */
  cpp_token *stringified;	/* Stringified argument.  */
  unsigned int count;		/* # of tokens in argument.  */
  unsigned int expanded_count;	/* # of tokens in expanded argument.  */
};

/* Macro expansion.  */

static void lock_pools PARAMS ((cpp_reader *));
static void unlock_pools PARAMS ((cpp_reader *));
static int enter_macro_context PARAMS ((cpp_reader *, cpp_token *));
static void builtin_macro PARAMS ((cpp_reader *, cpp_token *));
static cpp_context *push_arg_context PARAMS ((cpp_reader *, macro_arg *));
static enum cpp_ttype parse_arg PARAMS ((cpp_reader *, macro_arg *, int));
static macro_arg *parse_args PARAMS ((cpp_reader *, const cpp_hashnode *));
static cpp_context *next_context PARAMS ((cpp_reader *));
static void expand_arg PARAMS ((cpp_reader *, macro_arg *));
static unsigned char *quote_string PARAMS ((unsigned char *,
					    const unsigned char *,
					    unsigned int));
static void make_string_token PARAMS ((cpp_pool *, cpp_token *,
				       const U_CHAR *, unsigned int));
static void make_number_token PARAMS ((cpp_reader *, cpp_token *, int));
static void stringify_arg PARAMS ((cpp_reader *, macro_arg *));
static void paste_all_tokens PARAMS ((cpp_reader *, cpp_token *));
static void paste_payloads PARAMS ((cpp_reader *, cpp_token *,
				    const cpp_token *));
static int funlike_invocation_p PARAMS ((cpp_reader *, const cpp_hashnode *,
					  struct toklist *));
static void replace_args PARAMS ((cpp_reader *, cpp_macro *, macro_arg *,
				  struct toklist *));

/* Lookaheads.  */

static void save_lookahead_token PARAMS ((cpp_reader *, const cpp_token *));
static void take_lookahead_token PARAMS ((cpp_reader *, cpp_token *));
static void release_lookahead PARAMS ((cpp_reader *));
static cpp_lookahead *alloc_lookahead PARAMS ((cpp_reader *));
static void free_lookahead PARAMS ((cpp_lookahead *));

/* #define directive parsing and handling.  */

static cpp_token *lex_expansion_token PARAMS ((cpp_reader *, cpp_macro *));
static int check_macro_redefinition PARAMS ((cpp_reader *,
					     const cpp_hashnode *,
					     const cpp_macro *));
static int save_parameter PARAMS ((cpp_reader *, cpp_macro *, cpp_hashnode *));
static int parse_params PARAMS ((cpp_reader *, cpp_macro *));
static void check_trad_stringification PARAMS ((cpp_reader *,
						const cpp_macro *,
						const cpp_string *));

/* Allocates a buffer to hold a token's TEXT, and converts TOKEN to a
   CPP_STRING token containing TEXT in quoted form.  */
static void
make_string_token (pool, token, text, len)
     cpp_pool *pool;
     cpp_token *token;
     const U_CHAR *text;
     unsigned int len;
{
  U_CHAR *buf = _cpp_pool_alloc (pool, len * 4);

  token->type = CPP_STRING;
  token->val.str.text = buf;
  token->val.str.len = quote_string (buf, text, len) - buf;
  token->flags = 0;
}

/* Allocates and converts a temporary token to a CPP_NUMBER token,
   evaluating to NUMBER.  */
static void
make_number_token (pfile, token, number)
     cpp_reader *pfile;
     cpp_token *token;
     int number;
{
  unsigned char *buf = _cpp_pool_alloc (pfile->string_pool, 20);

  sprintf ((char *) buf, "%d", number);
  token->type = CPP_NUMBER;
  token->val.str.text = buf;
  token->val.str.len = ustrlen (buf);
  token->flags = 0;
}

static const char * const monthnames[] =
{
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

/* Handle builtin macros like __FILE__.  */
static void
builtin_macro (pfile, token)
     cpp_reader *pfile;
     cpp_token *token;
{
  unsigned char flags = token->flags & PREV_WHITE;
  cpp_hashnode *node = token->val.node;
  cpp_buffer *ip;

  switch (node->value.builtin)
    {
    case BT_FILE:
    case BT_BASE_FILE:
      {
	const char *file;

	ip = CPP_BUFFER (pfile);
	if (ip == 0)
	  file = "";
	else
	  {
	    if (node->value.builtin == BT_BASE_FILE)
	      while (CPP_PREV_BUFFER (ip) != NULL)
		ip = CPP_PREV_BUFFER (ip);

	    file = ip->nominal_fname;
	  }
	make_string_token (pfile->string_pool, token,
			   (U_CHAR *) file, strlen (file));
      }
      break;
	
    case BT_INCLUDE_LEVEL:
      /* pfile->include_depth counts the primary source as level 1,
	 but historically __INCLUDE_DEPTH__ has called the primary
	 source level 0.  */
      make_number_token (pfile, token, pfile->include_depth - 1);
      break;

    case BT_SPECLINE:
      /* If __LINE__ is embedded in a macro, it must expand to the
	 line of the macro's invocation, not its definition.
	 Otherwise things like assert() will not work properly.  */
      make_number_token (pfile, token, cpp_get_line (pfile)->line);
      break;

    case BT_STDC:
      {
	int stdc = 1;

#ifdef STDC_0_IN_SYSTEM_HEADERS
	if (CPP_IN_SYSTEM_HEADER (pfile)
	    && pfile->spec_nodes.n__STRICT_ANSI__->type == NT_VOID)
	  stdc = 0;
#endif
	make_number_token (pfile, token, stdc);
      }
      break;

    case BT_DATE:
    case BT_TIME:
      if (pfile->date.type == CPP_EOF)
	{
	  /* Allocate __DATE__ and __TIME__ from permanent storage,
	     and save them in pfile so we don't have to do this again.
	     We don't generate these strings at init time because
	     time() and localtime() are very slow on some systems.  */
	  time_t tt = time (NULL);
	  struct tm *tb = localtime (&tt);

	  make_string_token (&pfile->ident_pool, &pfile->date,
			     DSC("Oct 11 1347"));
	  make_string_token (&pfile->ident_pool, &pfile->time,
			     DSC("12:34:56"));

	  sprintf ((char *) pfile->date.val.str.text, "%s %2d %4d",
		   monthnames[tb->tm_mon], tb->tm_mday, tb->tm_year + 1900);
	  sprintf ((char *) pfile->time.val.str.text, "%02d:%02d:%02d",
		   tb->tm_hour, tb->tm_min, tb->tm_sec);
	}
      *token = node->value.builtin == BT_DATE ? pfile->date: pfile->time;
      break;

    default:
      cpp_ice (pfile, "invalid builtin macro \"%s\"", node->name);
      break;
    }

  token->flags = flags;
}

/* Used by cpperror.c to obtain the correct line and column to report
   in a diagnostic.  */
const cpp_lexer_pos *
cpp_get_line (pfile)
     cpp_reader *pfile;
{
  /* Within a macro expansion, return the position of the outermost
     invocation.  */
  if (pfile->context->prev)
    return &pfile->macro_pos;
  return &pfile->lexer_pos;
}

static void
lock_pools (pfile)
     cpp_reader *pfile;
{
  _cpp_lock_pool (&pfile->temp_string_pool);
  _cpp_lock_pool (&pfile->argument_pool);
}

static void
unlock_pools (pfile)
     cpp_reader *pfile;
{
  _cpp_unlock_pool (&pfile->temp_string_pool);
  _cpp_unlock_pool (&pfile->argument_pool);
}

static void
paste_payloads (pfile, lhs, rhs)
     cpp_reader *pfile;
     cpp_token *lhs;
     const cpp_token *rhs;
{
  unsigned int total_len = cpp_token_len (lhs) + cpp_token_len (rhs);
  unsigned char *result, *end;
  cpp_pool *pool;

  pool = lhs->type == CPP_NAME ? &pfile->ident_pool: pfile->string_pool;
  result = _cpp_pool_alloc (pool, total_len + 1);

  /* Paste the spellings and null terminate.  */
  end = cpp_spell_token (pfile, rhs, cpp_spell_token (pfile, lhs, result));
  *end = '\0';
  total_len = end - result;

  if (lhs->type == CPP_NAME)
    {
      lhs->val.node = cpp_lookup (pfile, result, total_len);
      if (lhs->val.node->flags & NODE_OPERATOR)
	{
	  lhs->flags |= NAMED_OP;
	  lhs->type = lhs->val.node->value.operator;
	}
    }
  else
    {
      lhs->val.str.text = result;
      lhs->val.str.len = total_len;
    }
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

/* Convert a token sequence to a single string token according to the
   rules of the ISO C #-operator.  */
static void
stringify_arg (pfile, arg)
     cpp_reader *pfile;
     macro_arg *arg;
{
  cpp_pool *pool = pfile->string_pool;
  unsigned char *start = POOL_FRONT (pool);
  unsigned int i, escape_it, total_len = 0, backslash_count = 0;
  unsigned int prev_white = 0;

  /* Loop, reading in the argument's tokens.  */
  for (i = 0; i < arg->count; i++)
    {
      unsigned char *dest;
      const cpp_token *token = &arg->first[i];
      unsigned int len = cpp_token_len (token);

      escape_it = (token->type == CPP_STRING || token->type == CPP_WSTRING
		   || token->type == CPP_CHAR || token->type == CPP_WCHAR
		   || token->type == CPP_OSTRING);

      if (escape_it)
	/* Worst case is each char is octal.  */
	len *= 4;
      len++;			/* Room for initial space.  */

      dest = &start[total_len];
      if (dest + len > POOL_LIMIT (pool))
	{
	  _cpp_next_chunk (pool, len, (unsigned char **) &start);
	  dest = &start[total_len];
	}

      prev_white |= token->flags & PREV_WHITE;
      if (token->type == CPP_PLACEMARKER)
	continue;

      /* No leading white space.  */
      if (prev_white)
	{
	  prev_white = 0;
	  if (total_len > 0)
	    *dest++ = ' ';
	}

      if (escape_it)
	{
	  unsigned char *buf = (unsigned char *) xmalloc (len);

	  len = cpp_spell_token (pfile, token, buf) - buf;
	  dest = quote_string (dest, buf, len);
	  free (buf);
	}
      else
	dest = cpp_spell_token (pfile, token, dest);
      total_len = dest - start;

      if (token->type == CPP_BACKSLASH)
	backslash_count++;
      else
	backslash_count = 0;
    }

  /* Ignore the final \ of invalid string literals.  */
  if (backslash_count & 1)
    {
      cpp_warning (pfile, "invalid string literal, ignoring final '\\'");
      total_len--;
    }

  POOL_COMMIT (pool, total_len);

  arg->stringified = xnew (cpp_token);
  arg->stringified->flags = 0;
  arg->stringified->type = CPP_STRING;
  arg->stringified->val.str.text = start;
  arg->stringified->val.str.len = total_len;
}

/* Handles an arbitrarily long sequence of ## operators.  This
   implementation is left-associative, non-recursive, and finishes a
   paste before handling succeeding ones.  If the paste fails, the
   right hand side of the ## operator is placed in the then-current
   context's lookahead buffer, with the effect that it appears in the
   output stream normally.  */
static void
paste_all_tokens (pfile, lhs)
     cpp_reader *pfile;
     cpp_token *lhs;
{
  unsigned char orig_flags = lhs->flags;
  cpp_token *rhs;

  do
    {
      /* Take the token directly from the current context.  We can do
	 this, because we are in the replacement list of either an
	 object-like macro, or a function-like macro with arguments
	 inserted.  In either case, the constraints to #define
	 guarantee we have at least one more token (empty arguments
	 become placemarkers).  */
      rhs = pfile->context->list.first++;

      if (rhs->type == CPP_PLACEMARKER)
	{
	  /* GCC has special extended semantics for , ## b where b is
	     a varargs parameter: the comma disappears if b was given
	     no actual arguments (not merely if b is an empty
	     argument).  */
	  if (lhs->type == CPP_COMMA && (rhs->flags & VARARGS_FIRST))
	    lhs->type = CPP_PLACEMARKER;
	}
      else if (lhs->type == CPP_PLACEMARKER)
	*lhs = *rhs;
      else
	{
	  int digraph = 0;
	  enum cpp_ttype type = cpp_can_paste (pfile, lhs, rhs, &digraph);

	  if (type == CPP_EOF)
	    {
	      /* Do nothing special about , ## <whatever> if
		 <whatever> came from a variable argument, because the
		 author probably intended the ## to trigger the
		 special extended semantics (see above).  */
	      if (lhs->type == CPP_COMMA && (rhs->flags & VARARGS_FIRST))
		/* nothing */;
	      else
		{
		  if (CPP_OPTION (pfile, warn_paste))
		    cpp_warning (pfile,
		 "pasting \"%s\" and \"%s\" does not give a valid preprocessing token",
				 cpp_token_as_text (pfile, lhs),
				 cpp_token_as_text (pfile, rhs));

		  /* The standard states that behaviour is undefined.
		     By the principle of least surpise, we step back
		     before the RHS, and mark it to prevent macro
		     expansion.  Tests in the testsuite rely on
		     clearing PREV_WHITE here, though you could argue
		     we should actually set it.  */
		  rhs->flags &= ~PREV_WHITE;
		  rhs->flags |= NO_EXPAND;
		}

	      /* Step back so we read the RHS in next.  */
	      pfile->context->list.first--;
	      break;
	    }

	  lhs->type = type;
	  lhs->flags &= ~DIGRAPH;
	  if (digraph)
	    lhs->flags |= DIGRAPH;

	  if (type == CPP_NAME || type == CPP_NUMBER)
	    paste_payloads (pfile, lhs, rhs);
	  else if (type == CPP_WCHAR || type == CPP_WSTRING)
	    lhs->val.str = rhs->val.str;
	}
    }
  while (rhs->flags & PASTE_LEFT);

  /* The pasted token has the PREV_WHITE flag of the LHS, is no longer
     PASTE_LEFT, and is subject to macro expansion.  */
  lhs->flags &= ~(PREV_WHITE | PASTE_LEFT | NO_EXPAND);
  lhs->flags |= orig_flags & PREV_WHITE;
}

/* Reads the unexpanded tokens of a macro argument into ARG.  Empty
   arguments are saved as a single CPP_PLACEMARKER token.  VAR_ARGS is
   non-zero if this is a variable argument.  Returns the type of the
   token that caused reading to finish.  */
static enum cpp_ttype
parse_arg (pfile, arg, var_args)
     cpp_reader *pfile;
     struct macro_arg *arg;
     int var_args;
{
  enum cpp_ttype result;
  unsigned int paren = 0;

  arg->first = (cpp_token *) POOL_FRONT (&pfile->argument_pool);
  for (;; arg->count++)
    {
      cpp_token *token = &arg->first[arg->count];
      if ((unsigned char *) (token + 1) >= POOL_LIMIT (&pfile->argument_pool))
	{
	  _cpp_next_chunk (&pfile->argument_pool, sizeof (cpp_token),
			   (unsigned char **) &arg->first);
	  token = &arg->first[arg->count];
	}

      _cpp_get_token (pfile, token);
      result = token->type;

      if (result == CPP_OPEN_PAREN)
	paren++;
      else if (result == CPP_CLOSE_PAREN && paren-- == 0)
	break;
      /* Commas are not terminators within parantheses or var_args.  */
      else if (result == CPP_COMMA && paren == 0 && !var_args)
	break;
      else if (result == CPP_EOF)
	break;		/* Error reported by caller.  */
      else if (result == CPP_DHASH)
	{
	  /* 6.10.3 paragraph 11: If there are sequences of
	     preprocessing tokens within the list of arguments that
	     would otherwise act as preprocessing directives, the
	     behavior is undefined.

	     This implementation will report a hard error, terminate
	     the macro invocation, and proceed to process the
	     directive.  */
	  cpp_error (pfile, "directives may not be used inside a macro argument");
	  _cpp_push_token (pfile, token, &pfile->lexer_pos);
	  result = CPP_EOF;
	  break;
	}
    }

  /* Empty arguments become a single placemarker token.  */
  if (arg->count == 0)
    {
      arg->first->type = CPP_PLACEMARKER;
      arg->count = 1;
    }

  /* Commit the memory used to store the arguments.  */
  POOL_COMMIT (&pfile->argument_pool, arg->count * sizeof (cpp_token));

  return result;
}

/* Parse the arguments making up a macro invocation.  */
static macro_arg *
parse_args (pfile, node)
     cpp_reader *pfile;
     const cpp_hashnode *node;
{
  cpp_macro *macro = node->value.macro;
  macro_arg *args, *cur;
  enum cpp_ttype type;
  int argc, error = 0;

  /* Allocate room for at least one argument, and zero it out.  */
  argc = macro->paramc ? macro->paramc: 1;
  args = xcnewvec (macro_arg, argc);

  for (cur = args, argc = 0; ;)
    {
      argc++;

      type = parse_arg (pfile, cur, argc == macro->paramc && macro->var_args);
      if (type == CPP_CLOSE_PAREN || type == CPP_EOF)
	break;

      /* Re-use the last argument for excess arguments.  */
      if (argc < macro->paramc)
	cur++;
    }

  if (type == CPP_EOF)
    {
      cpp_error (pfile, "unterminated argument list invoking macro \"%s\"",
		 node->name);
      error = 1;
    }
  else if (argc < macro->paramc)
    {
      /* As an extension, a rest argument is allowed to not appear in
	 the invocation at all.
	 e.g. #define debug(format, args...) something
	 debug("string");
	 
	 This is exactly the same as if there had been an empty rest
	 argument - debug("string", ).  */

      if (argc + 1 == macro->paramc && macro->var_args)
	{
	  /* parse_arg ensured there was space for the closing
	     parenthesis.  Use this space to store a placemarker.  */
	  args[argc].first = (cpp_token *) POOL_FRONT (&pfile->argument_pool);
	  args[argc].first->type = CPP_PLACEMARKER;
	  args[argc].count = 1;
	  POOL_COMMIT (&pfile->argument_pool, sizeof (cpp_token));

	  if (CPP_OPTION (pfile, c99) && CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "ISO C99 requires rest arguments to be used");
	}
      else
	{
	  cpp_error (pfile,
		     "macro \"%s\" requires %u arguments, but only %u given",
		     node->name, macro->paramc, argc);
	  error = 1;
	}
    }
  else if (argc > macro->paramc)
    {
      /* An empty argument to an empty function-like macro is fine.  */
      if (argc != 1 || args[0].first->type != CPP_PLACEMARKER)
	{
	  cpp_error (pfile,
		     "macro \"%s\" passed %u arguments, but takes just %u",
		     node->name, argc, macro->paramc);
	  error = 1;
	}
    }

  if (error)
    {
      free (args);
      args = 0;
    }

  return args;
}

static int
funlike_invocation_p (pfile, node, list)
     cpp_reader *pfile;
     const cpp_hashnode *node;
     struct toklist *list;
{
  cpp_context *orig_context;
  cpp_token maybe_paren;
  macro_arg *args = 0;

  pfile->state.parsing_args = 1;
  pfile->state.prevent_expansion++;
  orig_context = pfile->context;

  cpp_start_lookahead (pfile);
  cpp_get_token (pfile, &maybe_paren);
  cpp_stop_lookahead (pfile, maybe_paren.type == CPP_OPEN_PAREN);

  if (maybe_paren.type == CPP_OPEN_PAREN)
    args = parse_args (pfile, node);
  else if (CPP_WTRADITIONAL (pfile))
    cpp_warning (pfile,
	 "function-like macro \"%s\" must be used with arguments in traditional C",
		 node->name);

  /* Restore original context.  */
  pfile->context = orig_context;
  pfile->state.prevent_expansion--;
  pfile->state.parsing_args = 0;

  if (args)
    {
      if (node->value.macro->paramc > 0)
	replace_args (pfile, node->value.macro, args, list);
      free (args);
    }

  return args != 0;
}

/* Push the context of a macro onto the context stack.  TOKEN is the
   macro name.  If we can successfully start expanding the macro,
   TOKEN is replaced with the first token of the expansion.  */
static int
enter_macro_context (pfile, token)
     cpp_reader *pfile;
     cpp_token *token;
{
  cpp_context *context;
  cpp_macro *macro;
  unsigned char flags;
  struct toklist list;

  macro = token->val.node->value.macro;
  if (macro->disabled)
    {
      token->flags |= NO_EXPAND;
      return 1;
    }

  /* Save the position of the outermost macro invocation.  */
  if (!pfile->context->prev)
    {
      pfile->macro_pos = pfile->lexer_pos;
      lock_pools (pfile);
    }

  if (macro->fun_like && !funlike_invocation_p (pfile, token->val.node, &list))
    {
      if (!pfile->context->prev)
	unlock_pools (pfile);
      return 1;
    }

  /* Now push its context.  */
  context = next_context (pfile);
  if (macro->paramc == 0)
    {
      context->list.first = macro->expansion;
      context->list.limit = macro->expansion + macro->count;
    }
  else
    context->list = list;
  context->macro = macro;

  /* The first expansion token inherits the PREV_WHITE of TOKEN.  */
  flags = token->flags & PREV_WHITE;
  *token = *context->list.first++;
  token->flags |= flags;

  /* Disable the macro within its expansion.  */
  macro->disabled = 1;

  return 0;
}

/* Move to the next context.  Create one if there is none.  */
static cpp_context *
next_context (pfile)
     cpp_reader *pfile;
{
  cpp_context *prev = pfile->context;
  cpp_context *result = prev->next;

  if (result == 0)
    {
      result = xnew (cpp_context);
      prev->next = result;
      result->prev = prev;
      result->next = 0;
    }

  pfile->context = result;
  return result;
}

static void
replace_args (pfile, macro, args, list)
     cpp_reader *pfile;
     cpp_macro *macro;
     macro_arg *args;
     struct toklist *list;
{
  unsigned int i, total;
  const cpp_token *src, *limit;
  cpp_token *dest;
  macro_arg *arg;

  src = macro->expansion;
  limit = src + macro->count;

  /* First, fully macro-expand arguments, calculating the number of
     tokens in the final expansion as we go.  This ensures that the
     possible recursive use of argument_pool is fine.  */
  total = limit - src;
  for (; src < limit; src++)
    if (src->type == CPP_MACRO_ARG)
      {
	/* We have an argument.  If it is not being stringified or
	   pasted it is macro-replaced before insertion.  */
	arg = &args[src->val.aux - 1];
	if (src->flags & STRINGIFY_ARG)
	  {
	    if (!arg->stringified)
	      stringify_arg (pfile, arg);
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

  dest = (cpp_token *) _cpp_pool_alloc (&pfile->argument_pool,
					total * sizeof (cpp_token));
  list->first = dest;
  list->limit = list->first + total;

  for (src = macro->expansion; src < limit; src++)
    if (src->type == CPP_MACRO_ARG)
      {
	unsigned int count;
	const cpp_token *from;

	arg = &args[src->val.aux - 1];
	if (src->flags & STRINGIFY_ARG)
	  from = arg->stringified, count = 1;
	else if ((src->flags & PASTE_LEFT)
		 || (src > macro->expansion && (src[-1].flags & PASTE_LEFT)))
	  count = arg->count, from = arg->first;
	else
	  count = arg->expanded_count, from = arg->expanded;
	memcpy (dest, from, count * sizeof (cpp_token));

	/* The first token gets PREV_WHITE of the CPP_MACRO_ARG.  If
           it is a variable argument, it is also flagged.  */
	dest->flags &= ~PREV_WHITE;
	dest->flags |= src->flags & PREV_WHITE;
	if (macro->var_args && src->val.aux == macro->paramc)
	  dest->flags |= VARARGS_FIRST;

	/* The last token gets the PASTE_LEFT of the CPP_MACRO_ARG.  */
	dest[count - 1].flags |= src->flags & PASTE_LEFT;

	dest += count;
      }
    else
      *dest++ = *src;

  /* Free the expanded arguments.  */
  for (i = 0; i < macro->paramc; i++)
    {
      if (args[i].expanded)
	free (args[i].expanded);
      if (args[i].stringified)
	free (args[i].stringified);
    }
}

/* Subroutine of expand_arg to put the unexpanded tokens on the
   context stack.  */
static cpp_context *
push_arg_context (pfile, arg)
     cpp_reader *pfile;
     macro_arg *arg;
{
  cpp_context *context = next_context (pfile);
  context->macro = 0;
  context->list.first = arg->first;
  context->list.limit = arg->first + arg->count;

  return context;
}

static void
expand_arg (pfile, arg)
     cpp_reader *pfile;
     macro_arg *arg;
{
  cpp_token *token;
  unsigned int capacity = 256;

  /* Loop, reading in the arguments.  */
  arg->expanded = (cpp_token *) xmalloc (capacity * sizeof (cpp_token));
  arg->expanded_count = 0;

  push_arg_context (pfile, arg);
  do
    {
      if (arg->expanded_count >= capacity)
	{
	  capacity *= 2;
	  arg->expanded = (cpp_token *)
	    xrealloc (arg->expanded, capacity * sizeof (cpp_token));
	}
      token = &arg->expanded[arg->expanded_count++];
      _cpp_get_token (pfile, token);
    }
  while (token->type != CPP_EOF);

  arg->expanded_count--;

  /* Pop the context we pushed.  */ 
  pfile->context = pfile->context->prev;
}

void
_cpp_pop_context (pfile)
     cpp_reader *pfile;
{
  cpp_context *context = pfile->context;

  pfile->context = context->prev;
  /* Re-enable a macro and free resources when leaving its expansion.  */
  if (!pfile->state.parsing_args)
    {
      if (!pfile->context->prev)
	unlock_pools (pfile);
      context->macro->disabled = 0;
    }
}

/* Internal routine to return a token, either from an in-progress
   macro expansion, or from the source file as appropriate.  Handles
   macros, so tokens returned are post-expansion.  Does not filter
   CPP_PLACEMARKER tokens.  Returns CPP_EOF at EOL and EOF.  */
void
_cpp_get_token (pfile, token)
     cpp_reader *pfile;
     cpp_token *token;
{
  for (;;)
    {
      cpp_context *context = pfile->context;

      if (pfile->la_read)
	take_lookahead_token (pfile, token);
      /* Context->prev == 0 <=> base context.  */
      else if (!context->prev)
	_cpp_lex_token (pfile, token);
      else if (context->list.first != context->list.limit)
	*token = *context->list.first++;
      else
	{
	  if (context->macro)
	    {
	      _cpp_pop_context (pfile);
	      continue;
	    }
	  token->type = CPP_EOF;
	  token->flags = 0;
	}
      break;
    }

  /* Only perform macro expansion (and therefore pasting) when not
     skipping, or when skipping but in a directive.  The only
     directive where this could be true is #elif.  A possible later
     optimisation: get do_elif to clear skipping so we don't need the
     directive test here.  */
  if (pfile->skipping && !pfile->state.in_directive)
    return;

  for (;;)
    {
      if (token->flags & PASTE_LEFT)
	paste_all_tokens (pfile, token);

      if (token->type != CPP_NAME
	  || token->val.node->type != NT_MACRO
	  || pfile->state.prevent_expansion
	  || token->flags & NO_EXPAND)
	break;

      /* Macros, built-in or not, invalidate controlling macros.  */
      pfile->mi_state = MI_FAILED;

      if (token->val.node->flags & NODE_BUILTIN)
	{
	  builtin_macro (pfile, token);
	  break;
	}
      else if (enter_macro_context (pfile, token))
	break;
    }
}

/* External interface to get a token.  Tokens are returned after macro
   expansion and directives have been handled, as a continuous stream.
   Transparently enters included files.  CPP_EOF indicates end of
   original source file.  Filters out CPP_PLACEMARKER tokens.

   For the benefit of #pragma callbacks which may want to get the
   pragma's tokens, returns CPP_EOF to indicate end-of-directive in
   this case.  */
void
cpp_get_token (pfile, token)
     cpp_reader *pfile;
     cpp_token *token;
{
  for (;;)
    {
      _cpp_get_token (pfile, token);

      if (token->type == CPP_EOF)
	{
	  /* In directives we should pass through EOLs for the callbacks.  */
	  if (pfile->buffer->cur == pfile->buffer->rlimit
	      || pfile->state.in_directive || pfile->state.parsing_args)
	    break;
	  continue;
	}
      else if (token->type == CPP_DHASH)
	{
	  /* Handle directives.  */
	  if (_cpp_handle_directive (pfile, token->flags & PREV_WHITE))
	    continue;
	  /* This is in fact an assembler #.  */
	  if (pfile->skipping)
	    continue;
	  token->type = CPP_HASH;
	}
      /* We are not merging the PREV_WHITE of CPP_PLACEMARKERS.  I
         don't think it really matters.  */
      else if (pfile->skipping || token->type == CPP_PLACEMARKER)
	continue;

      /* Non-comment tokens invalidate any controlling macros.  */
      if (token->type != CPP_COMMENT)
	pfile->mi_state = MI_FAILED;

      break;
    }

  if (pfile->la_write)
    save_lookahead_token (pfile, token);
}

/* Read each token in, until EOF.  Directives are transparently
   processed.  */
void
cpp_scan_buffer_nooutput (pfile)
     cpp_reader *pfile;
{
  cpp_token token;

  do
    do
      cpp_get_token (pfile, &token);
    while (token.type != CPP_EOF);
  while (cpp_pop_buffer (pfile) != 0);
}

/* Lookahead handling.  */

static void
save_lookahead_token (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  if (token->type != CPP_EOF)
    {
      cpp_lookahead *la = pfile->la_write;
      cpp_token_with_pos *twp;

      if (la->count == la->cap)
	{
	  la->cap += la->cap + 8;
	  la->tokens = (cpp_token_with_pos *)
	    xrealloc (la->tokens, la->cap * sizeof (cpp_token_with_pos));
	}

      twp = &la->tokens[la->count++];
      twp->token = *token;
      twp->pos = *cpp_get_line (pfile);
    }
}

static void
take_lookahead_token (pfile, token)
     cpp_reader *pfile;
     cpp_token *token;
{
  cpp_lookahead *la = pfile->la_read;
  cpp_token_with_pos *twp = &la->tokens[la->cur];

  *token = twp->token;
  pfile->lexer_pos = twp->pos;

  if (++la->cur == la->count)
    release_lookahead (pfile);
}

/* Moves the lookahead at the front of the read list to the free store.  */
static void
release_lookahead (pfile)
     cpp_reader *pfile;
{
  cpp_lookahead *la = pfile->la_read;

  pfile->la_read = la->next;
  la->next = pfile->la_unused;
  pfile->la_unused = la;
  unlock_pools (pfile);
}

/* Take a new lookahead from the free store, or allocate one if none.  */
static cpp_lookahead *
alloc_lookahead (pfile)
     cpp_reader *pfile;
{
  cpp_lookahead *la = pfile->la_unused;

  if (la)
    pfile->la_unused = la->next;
  else
    {
      la = xnew (cpp_lookahead);
      la->tokens = 0;
      la->cap = 0;
    }

  la->cur = la->count = 0;
  return la;
}

/* Free memory associated with a lookahead list.  */
static void
free_lookahead (la)
     cpp_lookahead *la;
{
  if (la->tokens)
    free ((PTR) la->tokens);
  free ((PTR) la);
}

/* Free all the lookaheads of a cpp_reader.  */
void
_cpp_free_lookaheads (pfile)
     cpp_reader *pfile;
{
  cpp_lookahead *la, *lan;

  if (pfile->la_read)
    free_lookahead (pfile->la_read);
  if (pfile->la_write)
    free_lookahead (pfile->la_write);

  for (la = pfile->la_unused; la; la = lan)
    {
      lan = la->next;
      free_lookahead (la);
    }
}

/* Allocate a lookahead and move it to the front of the write list.  */
void
cpp_start_lookahead (pfile)
     cpp_reader *pfile;
{
  cpp_lookahead *la = alloc_lookahead (pfile);

  la->next = pfile->la_write;
  pfile->la_write = la;

  la->pos = *cpp_get_line (pfile);

  /* Don't allow memory pools to be re-used whilst we're reading ahead.  */
  lock_pools (pfile);
}

/* Stop reading ahead - either step back, or drop the read ahead.  */
void
cpp_stop_lookahead (pfile, drop)
     cpp_reader *pfile;
     int drop;
{
  cpp_lookahead *la = pfile->la_write;

  pfile->la_write = la->next;
  la->next = pfile->la_read;
  pfile->la_read = la;

  if (drop || la->count == 0)
    release_lookahead (pfile);
  else
    pfile->lexer_pos = la->pos;
}

/* Push a single token back to the front of the queue.  Only to be
   used by cpplib, and only then when necessary.  POS is the position
   to report for the preceding token.  */
void
_cpp_push_token (pfile, token, pos)
     cpp_reader *pfile;
     const cpp_token *token;
     const cpp_lexer_pos *pos;
{
  cpp_start_lookahead (pfile);
  save_lookahead_token (pfile, token);
  cpp_stop_lookahead (pfile, 0);
  pfile->lexer_pos = *pos;
}

/* #define directive parsing and handling.  */

/* Returns non-zero if a macro redefinition is trivial.  */
static int
check_macro_redefinition (pfile, node, macro2)
     cpp_reader *pfile;
     const cpp_hashnode *node;
     const cpp_macro *macro2;
{
  const cpp_macro *macro1;
  unsigned int i;

  if (node->type != NT_MACRO || node->flags & NODE_BUILTIN)
    return ! pfile->done_initializing;

  macro1 = node->value.macro;

  /* The quick failures.  */
  if (macro1->count != macro2->count
      || macro1->paramc != macro2->paramc
      || macro1->fun_like != macro2->fun_like
      || macro1->var_args != macro2->var_args)
    return 0;

  /* Check each token.  */
  for (i = 0; i < macro1->count; i++)
    if (! _cpp_equiv_tokens (&macro1->expansion[i], &macro2->expansion[i]))
      return 0;

  /* Check parameter spellings.  */
  for (i = 0; i < macro1->paramc; i++)
    if (macro1->params[i] != macro2->params[i])
      return 0;

  return 1;
}

/* Free the definition of hashnode H.  */

void
_cpp_free_definition (h)
     cpp_hashnode *h;
{
  /* Macros and assertions no longer have anything to free.  */
  h->type = NT_VOID;
  /* Clear builtin flag in case of redefinition.  */
  h->flags &= ~NODE_BUILTIN;
}

static int
save_parameter (pfile, macro, node)
     cpp_reader *pfile;
     cpp_macro *macro;
     cpp_hashnode *node;
{
  cpp_hashnode **dest;

  /* Constraint 6.10.3.6 - duplicate parameter names.  */
  if (node->arg_index)
    {
      cpp_error (pfile, "duplicate macro parameter \"%s\"", node->name);
      return 1;
    }

  dest = &macro->params[macro->paramc];

  /* Check we have room for the parameters.  */
  if ((unsigned char *) (dest + 1) >= POOL_LIMIT (&pfile->macro_pool))
    {
      _cpp_next_chunk (&pfile->macro_pool, sizeof (cpp_hashnode *),
		       (unsigned char **) &macro->params);
      dest = &macro->params[macro->paramc];
    }

  *dest = node;
  node->arg_index = ++macro->paramc;
  return 0;
}

static int
parse_params (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  cpp_token token;
  unsigned int prev_ident = 0;

  macro->params = (cpp_hashnode **) POOL_FRONT (&pfile->macro_pool);
  for (;;)
    {
      _cpp_lex_token (pfile, &token);

      switch (token.type)
	{
	default:
	  cpp_error (pfile, "\"%s\" may not appear in macro parameter list",
		     cpp_token_as_text (pfile, &token));
	  return 0;

	case CPP_NAME:
	  if (prev_ident)
	    {
	      cpp_error (pfile, "macro parameters must be comma-separated");
	      return 0;
	    }
	  prev_ident = 1;

	  if (save_parameter (pfile, macro, token.val.node))
	    return 0;
	  continue;

	case CPP_CLOSE_PAREN:
	  if (prev_ident || macro->paramc == 0)
	    break;

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
	  macro->var_args = 1;
	  if (!prev_ident)
	    {
	      save_parameter (pfile, macro, pfile->spec_nodes.n__VA_ARGS__);
	      pfile->state.va_args_ok = 1;
	      if (! CPP_OPTION (pfile, c99) && CPP_OPTION (pfile, pedantic))
		cpp_pedwarn (pfile,
		     "C89 does not permit anonymous variable arguments");
	    }
	  else if (CPP_OPTION (pfile, pedantic))
	    cpp_pedwarn (pfile,
			 "ISO C does not permit named variable arguments");

	  /* We're at the end, and just expect a closing parenthesis.  */
	  _cpp_lex_token (pfile, &token);
	  if (token.type == CPP_CLOSE_PAREN)
	    break;
	  /* Fall through.  */

	case CPP_EOF:
	  cpp_error (pfile, "missing ')' in macro parameter list");
	  return 0;
	}

      /* Success.  Commit the parameter array.  */
      POOL_COMMIT (&pfile->macro_pool,
		   macro->paramc * sizeof (cpp_hashnode *));
      return 1;
    }
}

/* Lex a token from a macro's replacement list.  Translate it to a
   CPP_MACRO_ARG if appropriate.  */
static cpp_token *
lex_expansion_token (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  cpp_token *token = &macro->expansion[macro->count];

  /* Check we have room for the token.  */
  if ((unsigned char *) (token + 1) >= POOL_LIMIT (&pfile->macro_pool))
    {
      _cpp_next_chunk (&pfile->macro_pool, sizeof (cpp_token),
		       (unsigned char **) &macro->expansion);
      token = &macro->expansion[macro->count];
    }

  macro->count++;
  _cpp_lex_token (pfile, token);

  /* Is this an argument?  */
  if (token->type == CPP_NAME && token->val.node->arg_index)
    {
      token->type = CPP_MACRO_ARG;
      token->val.aux = token->val.node->arg_index;
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
  cpp_token *token;
  unsigned int i, ok = 1;

  macro = (cpp_macro *) _cpp_pool_alloc (&pfile->macro_pool,
					 sizeof (cpp_macro));
  macro->file = pfile->buffer->nominal_fname;
  macro->line = pfile->directive_pos.line;
  macro->params = 0;
  macro->paramc = 0;
  macro->fun_like = 0;
  macro->var_args = 0;
  macro->count = 0;
  macro->expansion = (cpp_token *) POOL_FRONT (&pfile->macro_pool);

  /* Get the first token of the expansion (or the '(' of a
     function-like macro).  */
  token = lex_expansion_token (pfile, macro);
  if (token->type == CPP_OPEN_PAREN && !(token->flags & PREV_WHITE))
    {
      if (!(ok = parse_params (pfile, macro)))
	goto cleanup;
      macro->count = 0;
      macro->fun_like = 1;
      /* Some of the pool may have been used for the parameter store.  */
      macro->expansion = (cpp_token *) POOL_FRONT (&pfile->macro_pool);
      token = lex_expansion_token (pfile, macro);
    }
  else if (token->type != CPP_EOF && !(token->flags & PREV_WHITE))
    cpp_pedwarn (pfile, "ISO C requires whitespace after the macro name");

  /* Setting it here means we don't catch leading comments.  */
  pfile->state.save_comments = ! CPP_OPTION (pfile, discard_comments);

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
	  else if (!CPP_OPTION (pfile, lang_asm))
	    {
	      ok = 0;
	      cpp_error (pfile, "'#' is not followed by a macro parameter");
	      goto cleanup;
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
	      goto cleanup;
	    }

	  token[-1].flags |= PASTE_LEFT;
	  /* Give it a PREV_WHITE for -dM etc.  */
	  token->flags |= PREV_WHITE;
	}

      token = lex_expansion_token (pfile, macro);
    }

  /* Don't count the CPP_EOF.  Empty macros become a place marker.  */
  if (macro->count > 1)
    macro->count--;
  else
    macro->expansion[0].type = CPP_PLACEMARKER;

  /* Clear the whitespace flag from the leading token.  */
  macro->expansion[0].flags &= ~PREV_WHITE;

  /* Implement the macro-defined-to-itself optimisation.  */
  macro->disabled = (macro->count == 1 && !macro->fun_like
		     && macro->expansion[0].type == CPP_NAME
		     && macro->expansion[0].val.node == node);

  /* Commit the memory.  */
  POOL_COMMIT (&pfile->macro_pool, macro->count * sizeof (cpp_token));

  /* Redefinition of a macro is allowed if and only if the old and new
     definitions are the same.  (6.10.3 paragraph 2). */
  if (node->type != NT_VOID)
    {
      if (CPP_PEDANTIC (pfile)
	  && !check_macro_redefinition (pfile, node, macro))
	{
	  cpp_pedwarn_with_line (pfile, pfile->directive_pos.line,
				 pfile->directive_pos.col,
				 "\"%s\" redefined", node->name);

	  if (pfile->done_initializing && node->type == NT_MACRO
	      && !(node->flags & NODE_BUILTIN))
	    cpp_pedwarn_with_file_and_line (pfile,
					    node->value.macro->file,
					    node->value.macro->line, 1,
			    "this is the location of the previous definition");
	}
      _cpp_free_definition (node);
    }

  /* Enter definition in hash table.  */
  node->type = NT_MACRO;
  node->value.macro = macro;

 cleanup:

  /* Stop the lexer accepting __VA_ARGS__.  */
  pfile->state.va_args_ok = 0;

  /* Clear the fast argument lookup indices.  */
  for (i = macro->paramc; i-- > 0; )
    macro->params[i]->arg_index = 0;

  return ok;
}

/* Warn if a token in `string' matches one of the function macro
   arguments in `info'.  This function assumes that the macro is a
   function macro and not an object macro.  */
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

	  if (node->length == len && !memcmp (p, node->name, len))
	    {
	      cpp_warning (pfile,
	   "macro argument \"%s\" would be stringified with -traditional.",
			   node->name);
	      break;
	    }
	}
    }
}

/* Returns the expansion of a macro, in a format suitable to be read
   back in again, and therefore also for DWARF 2 debugging info.
   Caller is expected to generate the "#define NAME" bit.  The
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
      cpp_ice (pfile, "invalid hash type %d in dump_definition", node->type);
      return 0;
    }

  /* Calculate length.  */
  len = 1;			/* ' ' */
  if (macro->fun_like)
    {
      len += 3;		/* "()" plus possible final "." of ellipsis.  */
      for (i = 0; i < macro->paramc; i++)
	len += macro->params[i]->length + 2; /* ", " */
    }

  if (macro->count > 1 || macro->expansion[0].type != CPP_PLACEMARKER)
    {
      for (i = 0; i < macro->count; i++)
	{
	  cpp_token *token = &macro->expansion[i];

	  if (token->type == CPP_MACRO_ARG)
	    len += macro->params[token->val.aux - 1]->length;
	  else
	    len += cpp_token_len (token); /* Includes room for ' '.  */
	  if (token->flags & STRINGIFY_ARG)
	    len++;			/* "#" */
	  if (token->flags & PASTE_LEFT)
	    len += 3;		/* " ##" */
	}
    }

  if (len > pfile->macro_buffer_len)
    pfile->macro_buffer = (U_CHAR *) xrealloc (pfile->macro_buffer, len);
  buffer = pfile->macro_buffer;

  /* Parameter names.  */
  if (macro->fun_like)
    {
      *buffer++ = '(';
      for (i = 0; i < macro->paramc; i++)
	{
	  cpp_hashnode *param = macro->params[i];

	  if (param != pfile->spec_nodes.n__VA_ARGS__)
	    {
	      memcpy (buffer, param->name, param->length);
	      buffer += param->length;
	    }

	  if (i + 1 < macro->paramc)
	    *buffer++ = ',', *buffer++ = ' ';
	  else if (macro->var_args)
	    *buffer++ = '.', *buffer++ = '.', *buffer++ = '.';
	}
      *buffer++ = ')';
    }

  /* Expansion tokens.  */
  if (macro->count > 1 || macro->expansion[0].type != CPP_PLACEMARKER)
    {
      *buffer++ = ' ';
      for (i = 0; i < macro->count; i++)
	{
	  cpp_token *token = &macro->expansion[i];

	  if (token->flags & PREV_WHITE)
	    *buffer++ = ' ';
	  if (token->flags & STRINGIFY_ARG)
	    *buffer++ = '#';

	  if (token->type == CPP_MACRO_ARG)
	    {
	      len = macro->params[token->val.aux - 1]->length;
	      memcpy (buffer, macro->params[token->val.aux - 1]->name, len);
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
