/* Part of CPP library.  (Macro handling.)
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
#include "cpplib.h"
#include "cpphash.h"

/* Stores basic information about a macro, before it is allocated.  */
struct macro_info
{
  const cpp_token *first_param;	/* First parameter token.  */
  const cpp_token *first;	/* First expansion token.  */
  unsigned int paramlen;	/* Length of parameter names. */
  unsigned int len;		/* Length of token strings.  */
  unsigned int ntokens;		/* Number of tokens in expansion.  */
  short paramc;			/* Number of parameters.  */
  unsigned char flags;
};

static void dump_funlike_macro	PARAMS ((cpp_reader *, cpp_hashnode *));
static void count_params PARAMS ((cpp_reader *, struct macro_info *));
static int is__va_args__ PARAMS ((cpp_reader *, const cpp_token *));

static int parse_define PARAMS((cpp_reader *, struct macro_info *));
static int check_macro_redefinition PARAMS((cpp_reader *, cpp_hashnode *hp,
					    const cpp_toklist *));
static const cpp_toklist * save_expansion PARAMS((cpp_reader *,
 						  struct macro_info *));
static unsigned int find_param PARAMS ((const cpp_token *,
 					const cpp_token *));
static cpp_toklist * alloc_macro PARAMS ((cpp_reader *, struct macro_info *));


/* Scans for a given token, returning the parameter number if found,
   or 0 if not found.  Scans from FIRST to TOKEN - 1 or the first
   CPP_CLOSE_PAREN for TOKEN.  */
static unsigned int
find_param (first, token)
     const cpp_token *first, *token;
{
  unsigned int param = 0;

  for (; first < token && first->type != CPP_CLOSE_PAREN; first++)
    if (first->type == CPP_NAME)
      {
	param++;
	if (first->val.node == token->val.node)
	  return param;
      }

  return 0;
}

/* Constraint 6.10.3.5: __VA_ARGS__ should only appear in the
   replacement list of a variable-arguments macro.  TOKEN is assumed
   to be of type CPP_NAME.  */
static int
is__va_args__ (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  if (!CPP_PEDANTIC (pfile)
      || token->val.node != pfile->spec_nodes->n__VA_ARGS__)
    return 0;

  cpp_pedwarn_with_line (pfile, token->line, token->col,
       "\"%s\" is only valid in the replacement list of a function-like macro",
		       token->val.node->name);
  return 1;
}

/* Counts the parameters to a function-like macro, the length of their
   null-terminated names, and whether the macro is a variable-argument
   one.  FIRST is the token immediately after the open parenthesis,
   INFO stores the data.

   On success, info->first is updated to the token after the closing
   parenthesis, i.e. the first token of the expansion.  Otherwise
   there was an error, which has been reported.  */
static void
count_params (pfile, info)
     cpp_reader *pfile;
     struct macro_info *info;
{
  unsigned int prev_ident = 0;
  const cpp_token *token;

  info->paramc = 0;
  info->paramlen = 0;
  info->flags = 0;
  info->first = info->first_param; /* Not a ')' indicating success.  */

  for (token = info->first_param;; token++)
    {
      switch (token->type)
	{
	default:
	  cpp_error_with_line (pfile, token->line, token->col,
			       "illegal token in macro parameter list");
	  return;

	case CPP_EOF:
	missing_paren:
	  cpp_error_with_line (pfile, token->line, token->col,
			       "missing ')' in macro parameter list");
	  return;

	case CPP_COMMENT:
	  continue;		/* Ignore -C comments.  */

	case CPP_NAME:
	  if (prev_ident)
	    {
	      cpp_error_with_line (pfile, token->line, token->col,
			   "macro parameters must be comma-separated");
	      return;
	    }

	  /* Constraint 6.10.3.5  */
	  if (is__va_args__ (pfile, token))
	    return;

	  /* Constraint 6.10.3.6 - duplicate parameter names.  */
	  if (find_param (info->first, token))
	    {
	      cpp_error_with_line (pfile, token->line, token->col,
				   "duplicate macro parameter \"%s\"",
				   token->val.node->name);
	      return;
	    }

	  prev_ident = 1;
	  info->paramc++;
	  info->paramlen += token->val.node->length + 1;
	  continue;

	case CPP_CLOSE_PAREN:
	  if (prev_ident || info->paramc == 0)
	    break;

	  /* Fall through to pick up the error.  */
	case CPP_COMMA:
	  if (!prev_ident)
	    {
	      cpp_error_with_line (pfile, token->line, token->col,
				   "parameter name expected");
	      return;
	    }
	  prev_ident = 0;
	  continue;

	case CPP_ELLIPSIS:
	  /* Convert ISO-style var_args to named varargs by changing
	     the ellipsis into an identifier with name __VA_ARGS__.
	     This simplifies other handling. */
	  if (!prev_ident)
	    {
	      cpp_token *tok = (cpp_token *) token;

	      tok->type = CPP_NAME;
	      tok->val.node = pfile->spec_nodes->n__VA_ARGS__;

	      info->paramc++;
	      info->paramlen += tok->val.node->length + 1;

	      if (CPP_PEDANTIC (pfile) && ! CPP_OPTION (pfile, c99))
		cpp_pedwarn (pfile,
			     "C89 does not permit anon varargs macros");
	    }
	  else
	    {
	      info->flags |= GNU_REST_ARGS;
	      if (CPP_PEDANTIC (pfile))
		cpp_pedwarn (pfile,
			     "ISO C does not permit named varargs parameters");
	    }

	  info->flags |= VAR_ARGS;
	  token++;
	  if (token->type == CPP_CLOSE_PAREN)
	    break;
	  goto missing_paren;
	}

      /* Success.  */
      info->first = token + 1;
      if (!pfile->save_parameter_spellings)
	info->paramlen = 0;
      return;
    }
}

/* Parses a #define directive.  On success, returns zero, and INFO is
   filled in appropriately.  */
static int
parse_define (pfile, info)
     cpp_reader *pfile;
     struct macro_info *info;
{
  const cpp_token *token;
  int prev_white = 0;

  /* The first token after the macro's name.  */
  token = _cpp_get_token (pfile);

  /* Constraint 6.10.3.5  */
  if (is__va_args__ (pfile, token - 1))
    return 1;

  while (token->type == CPP_COMMENT)
    token++, prev_white = 1;
  prev_white |= token->flags & PREV_WHITE;

  if (token->type == CPP_OPEN_PAREN && !prev_white)
    {
      /* A function-like macro.  */
      info->first_param = token + 1;
      count_params (pfile, info);
      if (info->first[-1].type != CPP_CLOSE_PAREN)
	return 1;
    }
  else
    {
      /* An object-like macro.  */
      info->paramc = -1;
      info->paramlen = 0;
      info->flags = 0;
      info->first = token;
      if (!prev_white && token->type != CPP_EOF)
	cpp_pedwarn (pfile, "ISO C requires whitespace after the macro name");
    }

  /* Count tokens in expansion.  We drop paste tokens, and stringize
     tokens, so don't count them.  */
  info->ntokens = info->len = 0;
  for (token = info->first; token->type != CPP_EOF; token++)
    {
      if (token->type == CPP_PASTE)
	{
	  /* Token-paste ##, can appear in both object-like and
	     function-like macros, but not at the ends.  Constraint
	     6.10.3.3.1 */
	  if (token == info->first || token[1].type == CPP_EOF)
	    {
	      cpp_error_with_line (pfile, token->line, token->col,
		"'##' cannot appear at either end of a macro expansion");
	      return 1;
	    }
	  continue;
	}
      else if (token->type == CPP_HASH)
	{
	  /* Stringifying #, but a normal character in object-like
             macros.  Must come before a parameter name.  Constraint
             6.10.3.2.1.  */
	  if (info->paramc >= 0)
	    {
	      if (token[1].type == CPP_NAME
		  && find_param (info->first_param, token + 1))
		continue;
	      if (! CPP_OPTION (pfile, lang_asm))
		{
		  cpp_error_with_line (pfile, token->line, token->col,
			       "'#' is not followed by a macro parameter");
		  return 1;
		}
	    }
	}
      else if (token->type == CPP_NAME)
	{
	  /* Constraint 6.10.3.5  */
	  if (!(info->flags & VAR_ARGS) && is__va_args__ (pfile, token))
	    return 1;
	  /* It might be worth doing a check here that we aren't a
	     macro argument, since we don't store the text of macro
	     arguments.  This would reduce "len" and save space.  */
	}
      info->ntokens++;
      if (TOKEN_SPELL (token) == SPELL_STRING)
	info->len += token->val.str.len;
    }
  
  return 0;
}

/* Returns non-zero if a macro redefinition is trivial.  */
static int
check_macro_redefinition (pfile, hp, list2)
     cpp_reader *pfile;
     cpp_hashnode *hp;
     const cpp_toklist *list2;
{
  const cpp_toklist *list1;

  if (hp->type != T_MACRO)
    return ! pfile->done_initializing;

  /* Clear the whitespace and BOL flags of the first tokens.  They get
     altered during macro expansion, but is not significant here.  */
  list1  = hp->value.expansion;
  list1->tokens[0].flags &= ~(PREV_WHITE|BOL);
  list2->tokens[0].flags &= ~(PREV_WHITE|BOL);

  if (!_cpp_equiv_toklists (list1, list2))
    return 0;

  if (CPP_OPTION (pfile, pedantic)
      && list1->paramc > 0
      && (list1->params_len != list2->params_len
	  || memcmp (list1->namebuf, list2->namebuf, list1->params_len)))
    return 0;

  return 1;
}

/* This is a dummy structure whose only purpose is getting alignment
   correct.  */
struct toklist_dummy
{
  cpp_toklist list;
  cpp_token first_token;
};

/* Allocate space to hold the token list, its tokens, their text, and
   the parameter names if needed.  Empty expansions are stored as a
   single placemarker token.

   These are all allocated in a block together for performance
   reasons.  Therefore, this token list cannot be expanded like a
   normal token list.  Try to do so, and you lose.  */
static cpp_toklist *
alloc_macro (pfile, info)
     cpp_reader *pfile;
     struct macro_info *info;
{
  unsigned int size;
  struct toklist_dummy *dummy;
  cpp_toklist *list;

  /* Empty macros become a single placemarker token.  */
  if (info->ntokens == 0)
    info->ntokens = 1;

  size = sizeof (struct toklist_dummy);
  size += (info->ntokens - 1) * sizeof(cpp_token);
  size += info->len + info->paramlen;

  dummy = (struct toklist_dummy *) xmalloc (size);
  list = (cpp_toklist *) dummy;
  
  /* Initialize the monster.  */
  list->tokens = &dummy->first_token;
  list->tokens_used = list->tokens_cap = info->ntokens;

  list->namebuf = (unsigned char *) &list->tokens[info->ntokens];
  list->name_used = list->name_cap = info->len + info->paramlen;

  list->directive = 0;
  list->line = pfile->token_list.line;
  list->file = pfile->token_list.file;
  list->params_len = info->paramlen;
  list->paramc = info->paramc;
  list->flags = info->flags;

  return list;
}

/* Free the definition of macro H.  */

void
_cpp_free_definition (h)
     cpp_hashnode *h;
{
  if (h->type == T_MACRO)
    free ((PTR) h->value.expansion);
  h->value.expansion = NULL;
}

/* Copy the tokens of the expansion, beginning with info->first until
   CPP_EOF.  INFO contains information about the macro.

   Change the type of macro arguments in the expansion from CPP_NAME
   to CPP_MACRO_ARG.  Remove #'s that represent stringification,
   flagging the CPP_MACRO_ARG it operates on STRINGIFY.  Remove ##'s,
   flagging the token on its immediate left PASTE_LEFT.  Returns the
   token list for the macro expansion.  */
static const cpp_toklist *
save_expansion (pfile, info)
     cpp_reader *pfile;
     struct macro_info *info;
{
  const cpp_token *token;
  cpp_toklist *list;
  cpp_token *dest;
  unsigned char *buf;
      
  list = alloc_macro (pfile, info);
  buf = list->namebuf;

  /* Store the null-terminated parameter spellings of a macro, to
     provide pedantic warnings to satisfy 6.10.3.2, or for use when
     dumping macro definitions.  They must go first.  */
  if (list->params_len)
    for (token = info->first_param; token < info->first; token++)
      if (token->type == CPP_NAME)
	{
	  /* Copy null too.  */
	  memcpy (buf, token->val.node->name, token->val.node->length + 1);
	  buf += token->val.node->length + 1;
	}

  dest = list->tokens;
  for (token = info->first; token->type != CPP_EOF; token++)
    {
      unsigned int param_no;

      switch (token->type)
	{
	case CPP_NAME:
	  if (list->paramc == -1)
	    break;

	  /* Check if the name is a macro parameter.  */
	  param_no = find_param (info->first_param, token);
	  if (param_no == 0)
	    break;
	  dest->val.aux = param_no - 1;

	  dest->type = CPP_MACRO_ARG;
	  if (token[-1].type == CPP_HASH)
	    dest->flags = token[-1].flags | STRINGIFY_ARG;
	  else
	    dest->flags = token->flags;  /* Particularly PREV_WHITE.  */
	  /* Turn off PREV_WHITE if we immediately follow a paste.
	     That way, even if the paste turns out to be illegal, there
	     will be no space between the two tokens in the output.  */
	  if (token[-1].type == CPP_PASTE)
	    dest->flags &= ~PREV_WHITE;
	  dest++;
	  continue;

	case CPP_PASTE:
	  dest[-1].flags |= PASTE_LEFT;
	  continue;

	case CPP_HASH:
	  /* Stringifying #.  Constraint 6.10.3.2.1  */
	  if (list->paramc >= 0 && token[1].type == CPP_NAME
	      && find_param (info->first_param, token + 1))
	    continue;
	  break;

	default:
	  break;
	}

      /* Copy the token.  */
      *dest = *token;
      if (TOKEN_SPELL (token) == SPELL_STRING)
	{
	  memcpy (buf, token->val.str.text, token->val.str.len);
	  dest->val.str.text = buf;
	  buf += dest->val.str.len;
	}
      if (token[-1].type == CPP_PASTE)
	dest->flags &= ~PREV_WHITE;
      dest++;
    }

  /* Empty macros become a single placemarker token.  */
  if (dest == list->tokens)
    {
      dest->type = CPP_PLACEMARKER;
      dest->flags = 0;
    }

  return list;
}

/* Parse a macro and save its expansion.  Returns non-zero on success.  */
int
_cpp_create_definition (pfile, hp)
     cpp_reader *pfile;
     cpp_hashnode *hp;
{
  struct macro_info info;
  const cpp_toklist *list;

  if (parse_define (pfile, &info))
    return 0;
  list = save_expansion (pfile, &info);

  /* Check for a redefinition.  Redefinition of a macro is allowed if
     and only if the old and new definitions are the same.
     (6.10.3 paragraph 2). */

  if (hp->type != T_VOID)
    {
      if (!check_macro_redefinition (pfile, hp, list))
	{
	  cpp_pedwarn (pfile, "\"%s\" redefined", hp->name);
	  if (pfile->done_initializing && hp->type == T_MACRO)
	    cpp_pedwarn_with_file_and_line (pfile,
					    hp->value.expansion->file,
					    hp->value.expansion->line, 1,
			    "this is the location of the previous definition");
	}
      _cpp_free_definition (hp);
    }

  /* Enter definition in hash table.  */
  hp->type = T_MACRO;
  hp->value.expansion = list;

  return 1;
}

/* Dump the definition of macro MACRO on stdout.  The format is suitable
   to be read back in again. */

void
_cpp_dump_definition (pfile, hp)
     cpp_reader *pfile;
     cpp_hashnode *hp;
{
  CPP_RESERVE (pfile, hp->length + sizeof "#define ");
  CPP_PUTS_Q (pfile, "#define ", sizeof "#define " - 1);
  CPP_PUTS_Q (pfile, hp->name, hp->length);

  if (hp->type == T_MACRO)
    {
      if (hp->value.expansion->paramc >= 0)
	dump_funlike_macro (pfile, hp);
      else
	{
	  const cpp_toklist *list = hp->value.expansion;
	  list->tokens[0].flags &= ~BOL;
	  list->tokens[0].flags |= PREV_WHITE;
	  _cpp_dump_list (pfile, list, list->tokens, 1);
	}
    }
  else
    cpp_ice (pfile, "invalid hash type %d in dump_definition", hp->type);

  if (CPP_BUFFER (pfile) == 0 || ! pfile->done_initializing)
    CPP_PUTC (pfile, '\n');
}

static void
dump_funlike_macro (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  int i = 0;
  const cpp_toklist * list = node->value.expansion;
  const U_CHAR *param;

  param = list->namebuf;
  CPP_PUTC_Q (pfile, '(');
  for (i = 0; i++ < list->paramc;)
    {
      unsigned int len;

      len = ustrlen (param);
      CPP_PUTS (pfile, param, len);
      if (i < list->paramc)
	CPP_PUTS(pfile, ", ", 2);
      else if (list->flags & VAR_ARGS)
	{
	  if (!ustrcmp (param, U"__VA_ARGS__"))
	    pfile->limit -= sizeof (U"__VA_ARGS__") - 1;
	  CPP_PUTS_Q (pfile, "...", 3);
	}
      param += len + 1;
    }
  CPP_PUTC (pfile, ')');
  list->tokens[0].flags &= ~BOL;
  list->tokens[0].flags |= PREV_WHITE;
  _cpp_dump_list (pfile, list, list->tokens, 1);
}
