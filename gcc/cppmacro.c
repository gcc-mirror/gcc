/* Part of CPP library.  (Macro and #define handling.)
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.
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
  unsigned int variadic : 1;	/* If a variadic macro.  */
  unsigned int disabled : 1;	/* If macro is disabled.  */
  unsigned int syshdr   : 1;	/* If macro defined in system header.  */
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
static int enter_macro_context PARAMS ((cpp_reader *, cpp_hashnode *));
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
static int paste_tokens PARAMS ((cpp_reader *, cpp_token *, cpp_token *));
static int funlike_invocation_p PARAMS ((cpp_reader *, const cpp_hashnode *,
					  struct toklist *));
static void replace_args PARAMS ((cpp_reader *, cpp_macro *, macro_arg *,
				  struct toklist *));

/* Lookaheads.  */

static void save_lookahead_token PARAMS ((cpp_reader *, const cpp_token *));
static void take_lookahead_token PARAMS ((cpp_reader *, cpp_token *));
static cpp_lookahead *alloc_lookahead PARAMS ((cpp_reader *));
static void free_lookahead PARAMS ((cpp_lookahead *));

/* #define directive parsing and handling.  */

static cpp_token *lex_expansion_token PARAMS ((cpp_reader *, cpp_macro *));
static int warn_of_redefinition PARAMS ((cpp_reader *, const cpp_hashnode *,
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
  U_CHAR *buf = _cpp_pool_alloc (pool, len * 4 + 1);

  token->type = CPP_STRING;
  token->val.str.text = buf;
  token->val.str.len = quote_string (buf, text, len) - buf;
  buf[token->val.str.len] = '\0';
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
  unsigned char *buf = _cpp_pool_alloc (&pfile->ident_pool, 20);

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
  unsigned char flags = ((token->flags & PREV_WHITE) | AVOID_LPASTE);
  cpp_hashnode *node = token->val.node;

  switch (node->value.builtin)
    {
    case BT_FILE:
    case BT_BASE_FILE:
      {
	const char *name;
	cpp_buffer *buffer = pfile->buffer;

	if (node->value.builtin == BT_BASE_FILE)
	  while (buffer->prev)
	    buffer = buffer->prev;

	name = buffer->nominal_fname;
	make_string_token (&pfile->ident_pool, token,
			   (const unsigned char *) name, strlen (name));
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
	int stdc = (!CPP_IN_SYSTEM_HEADER (pfile)
		    || pfile->spec_nodes.n__STRICT_ANSI__->type != NT_VOID);
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
      cpp_ice (pfile, "invalid builtin macro \"%s\"", NODE_NAME (node));
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
  return &pfile->lexer_pos;
}

static void
lock_pools (pfile)
     cpp_reader *pfile;
{
  _cpp_lock_pool (&pfile->argument_pool);
}

static void
unlock_pools (pfile)
     cpp_reader *pfile;
{
  _cpp_unlock_pool (&pfile->argument_pool);
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
  cpp_pool *pool = &pfile->ident_pool;
  unsigned char *start = POOL_FRONT (pool);
  unsigned int i, escape_it, total_len = 0, backslash_count = 0;

  /* Loop, reading in the argument's tokens.  */
  for (i = 0; i < arg->count; i++)
    {
      unsigned char *dest;
      const cpp_token *token = &arg->first[i];
      unsigned int len = cpp_token_len (token);

      escape_it = (token->type == CPP_STRING || token->type == CPP_WSTRING
		   || token->type == CPP_CHAR || token->type == CPP_WCHAR);

      if (escape_it)
	/* Worst case is each char is octal.  */
	len *= 4;
      len += 2;			/* Room for initial space and final NUL.  */

      dest = &start[total_len];
      if (dest + len > POOL_LIMIT (pool))
	{
	  _cpp_next_chunk (pool, len, (unsigned char **) &start);
	  dest = &start[total_len];
	}

      /* No leading white space.  */
      if (token->flags & PREV_WHITE && total_len > 0)
	*dest++ = ' ';

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

      if (token->type == CPP_OTHER && token->val.c == '\\')
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

  /* Null terminate, and commit the memory.  */
  start[total_len] = '\0';
  POOL_COMMIT (pool, total_len + 1);

  arg->stringified = xnew (cpp_token);
  arg->stringified->flags = 0;
  arg->stringified->type = CPP_STRING;
  arg->stringified->val.str.text = start;
  arg->stringified->val.str.len = total_len;
}

/* Try to paste two tokens.  On success, the LHS becomes the pasted
   token, and 0 is returned.  For failure, we update the flags of the
   RHS appropriately and return non-zero.  */
static int
paste_tokens (pfile, lhs, rhs)
     cpp_reader *pfile;
     cpp_token *lhs, *rhs;
{
  unsigned char flags;
  int digraph = 0;
  enum cpp_ttype type;

  type = cpp_can_paste (pfile, lhs, rhs, &digraph);
  
  if (type == CPP_EOF)
    {
      /* Mandatory warning for all apart from assembler.  */
      if (CPP_OPTION (pfile, lang) != CLK_ASM)
	cpp_warning (pfile,
	 "pasting \"%s\" and \"%s\" does not give a valid preprocessing token",
		     cpp_token_as_text (pfile, lhs),
		     cpp_token_as_text (pfile, rhs));

      /* The standard states that behaviour is undefined.  By the
         principle of least surpise, we step back before the RHS, and
         mark it to prevent macro expansion.  Tests in the testsuite
         rely on clearing PREV_WHITE here, though you could argue we
         should actually set it.  Assembler can have '.' in labels and
         so requires that we don't insert spaces there.  Maybe we should
	 change this to put out a space unless it's assembler.  */
      rhs->flags &= ~PREV_WHITE;
      rhs->flags |= NO_EXPAND;
      return 1;
    }

  flags = lhs->flags & ~DIGRAPH;
  if (digraph)
    flags |= DIGRAPH;

  /* Identifiers and numbers need spellings to be pasted.  */
  if (type == CPP_NAME || type == CPP_NUMBER)
    {
      unsigned int total_len = cpp_token_len (lhs) + cpp_token_len (rhs);
      unsigned char *result, *end;

      result = _cpp_pool_alloc (&pfile->ident_pool, total_len + 1);

      /* Paste the spellings and null terminate.  */
      end = cpp_spell_token (pfile, rhs, cpp_spell_token (pfile, lhs, result));
      *end = '\0';
      total_len = end - result;

      if (type == CPP_NAME)
	{
	  lhs->val.node = cpp_lookup (pfile, result, total_len);
	  if (lhs->val.node->flags & NODE_OPERATOR)
	    {
	      flags |= NAMED_OP;
	      lhs->type = lhs->val.node->value.operator;
	    }
	}
      else
	{
	  lhs->val.str.text = result;
	  lhs->val.str.len = total_len;
	}
    }
  else if (type == CPP_WCHAR || type == CPP_WSTRING)
    lhs->val.str = rhs->val.str;

  /* Set type and flags after pasting spellings.  */
  lhs->type = type;
  lhs->flags = flags;

  return 0;
}

/* Handles an arbitrarily long sequence of ## operators.  This
   implementation is left-associative, non-recursive, and finishes a
   paste before handling succeeding ones.  If the paste fails, we back
   up a token to just after the ## operator, with the effect that it
   appears in the output stream normally.  */
static void
paste_all_tokens (pfile, lhs)
     cpp_reader *pfile;
     cpp_token *lhs;
{
  cpp_token *rhs;
  unsigned char orig_flags = lhs->flags;

  do
    {
      /* Take the token directly from the current context.  We can do
	 this, because we are in the replacement list of either an
	 object-like macro, or a function-like macro with arguments
	 inserted.  In either case, the constraints to #define
	 guarantee we have at least one more token.  */
      rhs = pfile->context->list.first++;
      if (paste_tokens (pfile, lhs, rhs))
	{
	  /* We failed.  Step back so we read the RHS in next.  */
	  pfile->context->list.first--;
	  break;
	}
    }
  while (rhs->flags & PASTE_LEFT);

  /* The pasted token has the PREV_WHITE flag of the LHS, is no longer
     PASTE_LEFT, and is subject to macro expansion.  */
  lhs->flags &= ~(PREV_WHITE | PASTE_LEFT | NO_EXPAND);
  lhs->flags |= orig_flags & (PREV_WHITE | AVOID_LPASTE);
}

/* Reads the unexpanded tokens of a macro argument into ARG.  VAR_ARGS
   is non-zero if this is a variadic macro.  Returns the type of the
   token that caused reading to finish.  */
static enum cpp_ttype
parse_arg (pfile, arg, variadic)
     cpp_reader *pfile;
     struct macro_arg *arg;
     int variadic;
{
  enum cpp_ttype result;
  unsigned int paren = 0;
  unsigned int line;

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

      /* Newlines in arguments are white space (6.10.3.10).  */
      line = pfile->lexer_pos.output_line;
      cpp_get_token (pfile, token);
      if (line != pfile->lexer_pos.output_line)
	token->flags |= PREV_WHITE;

      result = token->type;
      if (result == CPP_OPEN_PAREN)
	paren++;
      else if (result == CPP_CLOSE_PAREN && paren-- == 0)
	break;
      /* Commas are not terminators within parantheses or variadic.  */
      else if (result == CPP_COMMA && paren == 0 && !variadic)
	break;
      else if (result == CPP_EOF)
	break;		/* Error reported by caller.  */
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

      type = parse_arg (pfile, cur, argc == macro->paramc && macro->variadic);
      if (type == CPP_CLOSE_PAREN || type == CPP_EOF)
	break;

      /* Re-use the last argument for excess arguments.  */
      if (argc < macro->paramc)
	cur++;
    }

  if (type == CPP_EOF)
    {
      cpp_error (pfile, "unterminated argument list invoking macro \"%s\"",
		 NODE_NAME (node));
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
	  error = 1;
	}
    }
  else if (argc > macro->paramc)
    {
      /* Empty argument to a macro taking no arguments is OK.  */
      if (argc != 1 || cur->count)
	{
	  cpp_error (pfile,
		     "macro \"%s\" passed %u arguments, but takes just %u",
		     NODE_NAME (node), argc, macro->paramc);
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
  cpp_context *orig;
  cpp_token maybe_paren;
  macro_arg *args = 0;
  cpp_lexer_pos macro_pos;

  macro_pos = pfile->lexer_pos;
  pfile->state.parsing_args = 1;
  pfile->state.prevent_expansion++;
  orig = pfile->context;

  cpp_start_lookahead (pfile);
  cpp_get_token (pfile, &maybe_paren);
  cpp_stop_lookahead (pfile, maybe_paren.type == CPP_OPEN_PAREN);
  pfile->state.parsing_args = 2;

  if (maybe_paren.type == CPP_OPEN_PAREN)
    args = parse_args (pfile, node);
  else if (CPP_WTRADITIONAL (pfile) && ! node->value.macro->syshdr)
    cpp_warning (pfile,
	 "function-like macro \"%s\" must be used with arguments in traditional C",
		 NODE_NAME (node));

  /* Restore original context.  */
  pfile->context = orig;
  pfile->state.prevent_expansion--;
  pfile->state.parsing_args = 0;

  /* Reset the position in case of failure.  If success, the macro's
     expansion appears where the name would have.  */
  pfile->lexer_pos = macro_pos;

  if (args)
    {
      if (node->value.macro->paramc > 0)
	{
	  /* Don't save tokens during pre-expansion.  */
	  struct cpp_lookahead *la_saved = pfile->la_write;
	  pfile->la_write = 0;
	  replace_args (pfile, node->value.macro, args, list);
	  pfile->la_write = la_saved;
	}
      free (args);
    }

  return args != 0;
}

/* Push the context of a macro onto the context stack.  TOKEN is the
   macro name.  If we can successfully start expanding the macro,
   TOKEN is replaced with the first token of the expansion, and we
   return non-zero.  */
static int
enter_macro_context (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  cpp_context *context;
  cpp_macro *macro = node->value.macro;
  struct toklist list;

  /* Save the position of the outermost macro invocation.  */
  if (!pfile->context->prev)
    lock_pools (pfile);

  if (macro->fun_like && !funlike_invocation_p (pfile, node, &list))
    {
      if (!pfile->context->prev)
	unlock_pools (pfile);
      return 0;
    }

  if (macro->paramc == 0)
    {
      list.first = macro->expansion;
      list.limit = macro->expansion + macro->count;
    }

  /* Only push a macro context for non-empty replacement lists.  */
  if (list.first != list.limit)
    {
      context = next_context (pfile);
      context->list = list;
      context->macro = macro;
      
      /* Disable the macro within its expansion.  */
      macro->disabled = 1;
    }

  return 1;
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
  unsigned char flags = 0;
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
	arg = &args[src->val.arg_no - 1];

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
	      {
		arg->expanded_count = 0;
		if (arg->count)
		  expand_arg (pfile, arg);
	      }
	    total += arg->expanded_count - 1;
	  }
      }

  dest = (cpp_token *) _cpp_pool_alloc (&pfile->argument_pool,
					total * sizeof (cpp_token));
  list->first = dest;

  for (src = macro->expansion; src < limit; src++)
    if (src->type == CPP_MACRO_ARG)
      {
	unsigned int count;
	const cpp_token *from;

	arg = &args[src->val.arg_no - 1];
	if (src->flags & STRINGIFY_ARG)
	  from = arg->stringified, count = 1;
	else if (src->flags & PASTE_LEFT)
	  count = arg->count, from = arg->first;
	else if (src > macro->expansion && (src[-1].flags & PASTE_LEFT))
	  {
	    count = arg->count, from = arg->first;
	    if (dest != list->first)
	      {
		/* GCC has special semantics for , ## b where b is a
		   varargs parameter: the comma disappears if b was
		   given no actual arguments (not merely if b is an
		   empty argument); otherwise pasting is turned off.  */
		if (dest[-1].type == CPP_COMMA
		    && macro->variadic
		    && src->val.arg_no == macro->paramc)
		  {
		    if (count == 0)
		      dest--;
		    else
		      dest[-1].flags &= ~PASTE_LEFT;
		  }
		/* Count == 0 is the RHS a placemarker case.  */
		else if (count == 0)
		  dest[-1].flags &= ~PASTE_LEFT;
	      }
	  }
	else
	  count = arg->expanded_count, from = arg->expanded;

	/* Count == 0 is the LHS a placemarker case.  */
	if (count)
	  {
	    memcpy (dest, from, count * sizeof (cpp_token));

	    /* The first token gets PREV_WHITE of the CPP_MACRO_ARG.  */
	    dest->flags &= ~PREV_WHITE;
	    dest->flags |= src->flags & PREV_WHITE;
	    dest->flags |= AVOID_LPASTE;

	    /* The last token gets the PASTE_LEFT of the CPP_MACRO_ARG.  */
	    dest[count - 1].flags |= src->flags & PASTE_LEFT;

	    dest += count;
	  }

	/* The token after the argument must avoid an accidental paste.  */
	flags = AVOID_LPASTE;
      }
    else
      {
	*dest = *src;
	dest->flags |= flags;
	dest++;
	flags = 0;
      }

  list->limit = dest;

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
      cpp_get_token (pfile, token);
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
  if (!pfile->context->prev && !pfile->state.parsing_args)
    unlock_pools (pfile);

  /* Re-enable a macro, temporarily if parsing_args, when leaving its
     expansion.  */
  context->macro->disabled = 0;
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
void
cpp_get_token (pfile, token)
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
	{
	  *token = *context->list.first++;
	  token->flags |= pfile->buffer->saved_flags;
	  pfile->buffer->saved_flags = 0;
	  /* PASTE_LEFT tokens can only appear in macro expansions.  */
	  if (token->flags & PASTE_LEFT)
	    {
	      paste_all_tokens (pfile, token);
	      pfile->buffer->saved_flags = AVOID_LPASTE;
	    }
	}
      else
	{
	  if (context->macro)
	    {
	      /* Avoid accidental paste at the end of a macro.  */
	      pfile->buffer->saved_flags |= AVOID_LPASTE;
	      _cpp_pop_context (pfile);
	      continue;
	    }
	  /* End of argument pre-expansion.  */
	  token->type = CPP_EOF;
	  token->flags = 0;
	  return;
	}

      if (token->type != CPP_NAME)
	break;

      /* Handle macros and the _Pragma operator.  */
      if (token->val.node->type == NT_MACRO
	  && !pfile->state.prevent_expansion
	  && !(token->flags & NO_EXPAND))
	{
	  cpp_hashnode *node = token->val.node;

	  /* Macros invalidate controlling macros.  */
	  pfile->mi_state = MI_FAILED;

	  if (node->flags & NODE_BUILTIN)
	    {
	      builtin_macro (pfile, token);
	      pfile->buffer->saved_flags = AVOID_LPASTE;
	      break;
	    }

	  if (node->value.macro->disabled)
	    token->flags |= NO_EXPAND;
	  else if (enter_macro_context (pfile, node))
	    {
	      /* Pass AVOID_LPASTE and our PREV_WHITE to next token.  */
	      pfile->buffer->saved_flags = ((token->flags & PREV_WHITE)
					    | AVOID_LPASTE);
	      continue;
	    }
	}

      /* Don't interpret _Pragma within directives.  The standard is
         not clear on this, but to me this makes most sense.  */
      if (token->val.node != pfile->spec_nodes.n__Pragma
	  || pfile->state.in_directive)
	break;

      /* Handle it, and loop back for another token.  MI is cleared
         since this token came from either the lexer or a macro.  */
      _cpp_do__Pragma (pfile);
    }

  if (pfile->la_write)
    save_lookahead_token (pfile, token);
}

/* Returns true if we're expanding an object-like macro that was
   defined in a system header.  Just checks the macro at the top of
   the stack.  Used for diagnostic suppression.  */
int
cpp_sys_macro_p (pfile)
     cpp_reader *pfile;
{
  cpp_macro *macro = pfile->context->macro;

  return macro && macro->syshdr;
}

/* Read each token in, until EOF.  Directives are transparently
   processed.  */
void
cpp_scan_buffer_nooutput (pfile, all_buffers)
     cpp_reader *pfile;
     int all_buffers;
{
  cpp_token token;
  cpp_buffer *buffer = all_buffers ? 0: pfile->buffer->prev;

  do
    do
      cpp_get_token (pfile, &token);
    while (token.type != CPP_EOF);
  while (cpp_pop_buffer (pfile) != buffer);
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
    _cpp_release_lookahead (pfile);
}

/* Moves the lookahead at the front of the read list to the free store.  */
void
_cpp_release_lookahead (pfile)
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
    _cpp_release_lookahead (pfile);
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

/* Returns non-zero if a macro redefinition warning is required.  */
static int
warn_of_redefinition (pfile, node, macro2)
     cpp_reader *pfile;
     const cpp_hashnode *node;
     const cpp_macro *macro2;
{
  const cpp_macro *macro1;
  unsigned int i;

  /* Some redefinitions need to be warned about regardless.  */
  if (node->flags & NODE_WARN)
    return 1;

  if (! CPP_PEDANTIC (pfile))
    return 0;

  /* Redefinition of a macro is allowed if and only if the old and new
     definitions are the same.  (6.10.3 paragraph 2). */
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
      cpp_error (pfile, "duplicate macro parameter \"%s\"", NODE_NAME (node));
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
  cpp_token *token;
  unsigned int i, ok = 1;

  macro = (cpp_macro *) _cpp_pool_alloc (&pfile->macro_pool,
					 sizeof (cpp_macro));
  macro->file = pfile->buffer->nominal_fname;
  macro->line = pfile->directive_pos.line;
  macro->params = 0;
  macro->paramc = 0;
  macro->fun_like = 0;
  macro->variadic = 0;
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
	  else if (CPP_OPTION (pfile, lang) != CLK_ASM)
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

  /* Don't count the CPP_EOF.  */
  macro->count--;

  /* Clear the whitespace flag from the leading token.  */
  macro->expansion[0].flags &= ~PREV_WHITE;

  /* Implement the macro-defined-to-itself optimisation.  */
  macro->disabled = (macro->count == 1 && !macro->fun_like
		     && macro->expansion[0].type == CPP_NAME
		     && macro->expansion[0].val.node == node);

  /* To suppress some diagnostics.  */
  macro->syshdr = pfile->buffer->sysp != 0;

  /* Commit the memory.  */
  POOL_COMMIT (&pfile->macro_pool, macro->count * sizeof (cpp_token));

  if (node->type != NT_VOID)
    {
      if (warn_of_redefinition (pfile, node, macro))
	{
	  cpp_pedwarn_with_line (pfile, pfile->directive_pos.line,
				 pfile->directive_pos.col,
				 "\"%s\" redefined", NODE_NAME (node));

	  if (node->type == NT_MACRO && !(node->flags & NODE_BUILTIN))
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
  if (! ustrncmp (NODE_NAME (node), DSC ("__STDC_")))
    node->flags |= NODE_WARN;

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

	  if (NODE_LEN (node) == len
	      && !memcmp (p, NODE_NAME (node), len))
	    {
	      cpp_warning (pfile,
	   "macro argument \"%s\" would be stringified with -traditional.",
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
  len = NODE_LEN (node) + 1;			/* ' ' */
  if (macro->fun_like)
    {
      len += 3;		/* "()" plus possible final "." of named
			   varargs (we have + 2 below).  */
      for (i = 0; i < macro->paramc; i++)
	len += NODE_LEN (macro->params[i]) + 2; /* ", " */
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
	    *buffer++ = ',', *buffer++ = ' ';
	  else if (macro->variadic)
	    *buffer++ = '.', *buffer++ = '.', *buffer++ = '.';
	}
      *buffer++ = ')';
    }

  /* Expansion tokens.  */
  if (macro->count)
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
