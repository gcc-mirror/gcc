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
#include "hashtab.h"
#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* This is the second argument to eq_HASHNODE.  */
struct hashdummy
{
  const U_CHAR *name;
  unsigned int hash;
  unsigned short length;
};

/* Stores basic information about a macro, before it is allocated.  */
struct macro_info
{
  unsigned int paramlen;
  signed short paramc;
  unsigned char flags;
};

/* Initial hash table size.  (It can grow if necessary - see hashtab.c.)  */
#define HASHSIZE 4096

static unsigned int hash_HASHNODE PARAMS ((const void *));
static int eq_HASHNODE		  PARAMS ((const void *, const void *));
static int dump_hash_helper	  PARAMS ((void **, void *));

static void dump_funlike_macro	PARAMS ((cpp_reader *, cpp_hashnode *));

static const cpp_token *count_params PARAMS ((cpp_reader *,
					      const cpp_token *,
					      struct macro_info *));
static int is__va_args__ PARAMS ((cpp_reader *, const cpp_token *));
static const cpp_toklist * parse_define PARAMS((cpp_reader *));
static int check_macro_redefinition PARAMS((cpp_reader *, cpp_hashnode *hp,
					     const cpp_toklist *));
static const cpp_toklist * save_expansion PARAMS((cpp_reader *,
						  const cpp_token *,
						  const cpp_token *,
						  struct macro_info *));
static unsigned int find_param PARAMS ((const cpp_token *,
					const cpp_token *));
static cpp_toklist * alloc_macro PARAMS ((cpp_reader *,
					  struct macro_info *,
					  unsigned int, unsigned int));
static void free_macro PARAMS((const cpp_toklist *));

/* Calculate hash of a string of length LEN.  */
unsigned int
_cpp_calc_hash (str, len)
     const U_CHAR *str;
     size_t len;
{
  size_t n = len;
  unsigned int r = 0;

  do
    r = r * 67 + (*str++ - 113);
  while (--n);
  return r + len;
}

/* Calculate hash of a cpp_hashnode structure.  */
static unsigned int
hash_HASHNODE (x)
     const void *x;
{
  const cpp_hashnode *h = (const cpp_hashnode *)x;
  return h->hash;
}

/* Compare a cpp_hashnode structure (already in the table) with a
   hashdummy structure (not yet in the table).  This relies on the
   rule that the existing entry is the first argument, the potential
   entry the second.  It also relies on the comparison function never
   being called except as a direct consequence of a call to
   the htab_find routines.  */
static int
eq_HASHNODE (x, y)
     const void *x;
     const void *y;
{
  const cpp_hashnode *a = (const cpp_hashnode *)x;
  const struct hashdummy *b = (const struct hashdummy *)y;

  return (a->hash == b->hash
	  && a->length == b->length
	  && !memcmp (a->name, b->name, a->length));
}

/* Find the hash node for name "name", of length LEN.  */

cpp_hashnode *
cpp_lookup (pfile, name, len)
     cpp_reader *pfile;
     const U_CHAR *name;
     int len;
{
  struct hashdummy dummy;
  cpp_hashnode *new, **slot;
  unsigned int hash;
  U_CHAR *p;

  dummy.name = name;
  dummy.length = len;
  dummy.hash = hash = _cpp_calc_hash (name, len);

  slot = (cpp_hashnode **)
    htab_find_slot_with_hash (pfile->hashtab, (void *)&dummy, hash, INSERT);
  if (*slot)
    return *slot;

  /* Create a new hash node.  */
  p = obstack_alloc (pfile->hash_ob, sizeof (cpp_hashnode) + len);
  new = (cpp_hashnode *)p;
  p += offsetof (cpp_hashnode, name);

  new->type = T_VOID;
  new->length = len;
  new->hash = hash;
  new->fe_value = 0;
  new->value.expansion = NULL;

  memcpy (p, name, len);
  p[len] = 0;

  *slot = new;
  return new;
}

/* Set up and tear down internal structures for macro expansion.  */
void
_cpp_init_macros (pfile)
     cpp_reader *pfile;
{
  pfile->hashtab = htab_create (HASHSIZE, hash_HASHNODE,
				eq_HASHNODE, (htab_del) _cpp_free_definition);
  pfile->hash_ob = xnew (struct obstack);
  obstack_init (pfile->hash_ob);
}

void
_cpp_cleanup_macros (pfile)
     cpp_reader *pfile;
{
  htab_delete (pfile->hashtab);
  obstack_free (pfile->hash_ob, 0);
  free (pfile->hash_ob);
}

/* Free the definition of macro H.  */

void
_cpp_free_definition (h)
     cpp_hashnode *h;
{
  if (h->type == T_MACRO)
    free_macro (h->value.expansion);
  h->value.expansion = NULL;
}

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
   INFO stores the data, and should have paramlen and flags zero.

   Returns the token that we stopped scanning at; if it's type isn't
   CPP_CLOSE_PAREN there was an error, which has been reported.  */
static const cpp_token *
count_params (pfile, first, info)
     cpp_reader *pfile;
     const cpp_token *first;
     struct macro_info *info;
{
  unsigned int prev_ident = 0;
  const cpp_token *token;

  info->paramc = 0;
  for (token = first;; token++)
    {
      switch (token->type)
	{
	case CPP_EOF:
	missing_paren:
	  cpp_error_with_line (pfile, token->line, token->col,
			       "missing ')' in macro parameter list");
	  goto out;

	case CPP_COMMENT:
	  continue;		/* Ignore -C comments.  */

	case CPP_NAME:
	  if (prev_ident)
	    {
	      cpp_error_with_line (pfile, token->line, token->col,
			   "macro parameters must be comma-separated");
	      goto out;
	    }

	  /* Constraint 6.10.3.5  */
	  if (is__va_args__ (pfile, token))
	    goto out;

	  /* Constraint 6.10.3.6 - duplicate parameter names.  */
	  if (find_param (first, token))
	    {
	      cpp_error_with_line (pfile, token->line, token->col,
				   "duplicate macro parameter \"%s\"",
				   token->val.node->name);
	      goto out;
	    }

	  prev_ident = 1;
	  info->paramc++;
	  if (pfile->save_parameter_spellings)
	    info->paramlen += token->val.node->length + 1;
	  break;

	default:
	  cpp_error_with_line (pfile, token->line, token->col,
			       "illegal token in macro parameter list");
	  goto out;

	case CPP_CLOSE_PAREN:
	  if (prev_ident || info->paramc == 0)
	    goto out;

	  /* Fall through to pick up the error.  */
	case CPP_COMMA:
	  if (!prev_ident)
	    {
	      cpp_error_with_line (pfile, token->line, token->col,
				   "parameter name expected");
	      if (token->type == CPP_CLOSE_PAREN)
		token--;		/* Return the ',' not ')'.  */
	      goto out;
	    }
	  prev_ident = 0;
	  break;

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
	      if (pfile->save_parameter_spellings)
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
	    goto out;
	  goto missing_paren;
	}
    }

 out:
  return token;
}

/* Parses a #define directive.  Returns null pointer on error.  */
static const cpp_toklist *
parse_define (pfile)
     cpp_reader *pfile;
{
  const cpp_token *token, *first_param;
  struct macro_info info;
  int prev_white = 0;

  /* The first token after the macro's name.  */
  token = _cpp_get_token (pfile);

  /* Constraint 6.10.3.5  */
  if (is__va_args__ (pfile, token - 1))
    return 0;

  while (token->type == CPP_COMMENT)
    token++, prev_white = 1;
  first_param = token + 1;

  /* Assume object-like macro.  */
  info.paramc = -1;
  info.paramlen = 0;
  info.flags = 0;

  if (!prev_white && !(token->flags & PREV_WHITE))
    {
      if (token->type == CPP_OPEN_PAREN)
	{
	  token = count_params (pfile, first_param, &info);
	  if (token->type != CPP_CLOSE_PAREN)
	    return 0;
	  token++;
	}
      else if (token->type != CPP_EOF)
	cpp_pedwarn (pfile,
		     "ISO C requires whitespace after the macro name");
    }

  return save_expansion (pfile, token, first_param, &info);
}

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
alloc_macro (pfile, info, ntokens, len)
     cpp_reader *pfile;
     struct macro_info *info;
     unsigned int ntokens, len;
{
  unsigned int size;
  struct toklist_dummy *dummy;
  cpp_toklist *list;

  size = sizeof (struct toklist_dummy);
  size += (ntokens - 1) * sizeof(cpp_token);
  size += len + info->paramlen;

  dummy = (struct toklist_dummy *) xmalloc (size);
  list = (cpp_toklist *) dummy;
  
  /* Initialize the monster.  */
  list->tokens = &dummy->first_token;
  list->tokens_used = list->tokens_cap = ntokens;

  list->namebuf = (unsigned char *) &list->tokens[ntokens];
  list->name_used = list->name_cap = len + info->paramlen;

  list->directive = 0;
  list->line = pfile->token_list.line;
  list->file = pfile->token_list.file;
  list->params_len = info->paramlen;
  list->paramc = info->paramc;
  list->flags = info->flags;

  return list;
}

/* Free a macro allocated by allocate_macro.  */
static void
free_macro (list)
     const cpp_toklist *list;
{
  free ((PTR) list);
}

/* Copy the tokens of the expansion, beginning with FIRST until
   CPP_EOF.  For a function-like macro, FIRST_PARAM points to the
   first parameter.  INFO contains information about the macro.

   Change the type of macro arguments in the expansion from CPP_NAME
   to CPP_MACRO_ARG.  Remove #'s that represent stringification,
   flagging the CPP_MACRO_ARG it operates on STRINGIFY.  Remove ##'s,
   flagging the token on its immediate left PASTE_LEFT.  Returns the
   token list for the macro expansion, or 0 on error.  */
static const cpp_toklist *
save_expansion (pfile, first, first_param, info)
     cpp_reader *pfile;
     const cpp_token *first;
     const cpp_token *first_param;
     struct macro_info *info;
{
  const cpp_token *token;
  cpp_toklist *list;
  cpp_token *dest;
  unsigned int len, ntokens;
  unsigned char *buf;
      
  /* Count tokens in expansion.  We drop paste tokens, and stringize
     tokens, so don't count them.  */
  ntokens = len = 0;
  for (token = first; token->type != CPP_EOF; token++)
    {
      const char *msg;

      if (token->type == CPP_PASTE)
	{
	  /* Token-paste ##, but is a normal token if traditional.  */
	  if (! CPP_TRADITIONAL (pfile))
	    {
	      msg = "\"##\" cannot appear at either end of a macro expansion";
	      /* Constraint 6.10.3.3.1  */
	      if (token == first || token[1].type == CPP_EOF)
		goto error;
	      continue;
	    }
	}
      else if (token->type == CPP_HASH)
	{
	  /* Stringifying #, but is a normal character if traditional,
	     or in object-like macros.  Constraint 6.10.3.2.1.  */
	  if (info->paramc >= 0 && ! CPP_TRADITIONAL (pfile))
	    {
	      if (token[1].type == CPP_NAME
		  && find_param (first_param, token + 1))
		continue;
	      if (! CPP_OPTION (pfile, lang_asm))
		{
		  msg = "'#' is not followed by a macro parameter";
		error:
		  cpp_error_with_line (pfile, token->line, token->col, msg);
		  return 0;
		}
	    }
	}
      else if (token->type == CPP_NAME)
	{
	  /* Constraint 6.10.3.5  */
	  if (!(info->flags & VAR_ARGS) && is__va_args__ (pfile, token))
	    return 0;
	  /* It might be worth doing a check here that we aren't a
	     macro argument, since we don't store the text of macro
	     arguments.  This would reduce "len" and save space.  */
	}
      ntokens++;
      if (TOKEN_SPELL (token) == SPELL_STRING)
	len += token->val.str.len;
    }

  if (ntokens == 0)
    ntokens++;
  list = alloc_macro (pfile, info, ntokens, len);
  buf = list->namebuf;

  /* Store the null-terminated parameter spellings of a macro, to
     provide pedantic warnings to satisfy 6.10.3.2, or for use when
     dumping macro definitions.  They must go first.  */
  if (list->params_len)
    for (token = first_param; token < first; token++)
      if (token->type == CPP_NAME)
	{
	  /* Copy null too.  */
	  memcpy (buf, token->val.node->name, token->val.node->length + 1);
	  buf += token->val.node->length + 1;
	}

  dest = list->tokens;
  for (token = first; token->type != CPP_EOF; token++)
    {
      unsigned int param_no;

      switch (token->type)
	{
	case CPP_NAME:
	  if (list->paramc == -1)
	    break;

	  /* Check if the name is a macro parameter.  */
	  param_no = find_param (first_param, token);
	  if (param_no == 0)
	    break;
	  dest->val.aux = param_no - 1;

	  dest->type = CPP_MACRO_ARG;
	  if (token[-1].type == CPP_HASH && ! CPP_TRADITIONAL (pfile))
	    dest->flags = token[-1].flags | STRINGIFY_ARG;
	  else
	    dest->flags = token->flags;  /* Particularly PREV_WHITE.  */
	  dest++;
	  continue;

	case CPP_PASTE:
	  if (! CPP_TRADITIONAL (pfile))
	    {
	      dest[-1].flags |= PASTE_LEFT;
	      continue;
	    }
	  break;

	case CPP_HASH:
	  /* Stringifying #.  Constraint 6.10.3.2.1  */
	  if (list->paramc >= 0 && ! CPP_TRADITIONAL (pfile)
	      && token[1].type == CPP_NAME
	      && find_param (first_param, token + 1))
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
      dest++;
    }

  if (dest == list->tokens)
    {
      dest->type = CPP_PLACEMARKER;
      dest->flags = 0;
    }

  return list;
}

int
_cpp_create_definition (pfile, hp)
     cpp_reader *pfile;
     cpp_hashnode *hp;
{
  const cpp_toklist *list;

  list = parse_define (pfile);
  if (!list)
    return 0;

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

/* Dump out the hash table.  */
static int
dump_hash_helper (h, p)
     void **h;
     void *p;
{
  cpp_hashnode *hp = (cpp_hashnode *)*h;
  cpp_reader *pfile = (cpp_reader *)p;

  if (hp->type == T_MACRO)
    _cpp_dump_definition (pfile, hp);
  return 1;
}

void
_cpp_dump_macro_hash (pfile)
     cpp_reader *pfile;
{
  htab_traverse (pfile->hashtab, dump_hash_helper, pfile);
}
