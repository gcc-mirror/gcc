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
#include "hashtab.h"
#include "cpphash.h"

#undef abort

static unsigned int hash_HASHNODE PARAMS ((const void *));
static int eq_HASHNODE		  PARAMS ((const void *, const void *));
static void del_HASHNODE	  PARAMS ((void *));
static void dump_DEFINITION	  PARAMS ((cpp_reader *, DEFINITION *));
static int dump_hash_helper	  PARAMS ((void **, void *));

static void push_macro_expansion PARAMS ((cpp_reader *, const U_CHAR *,
					  int, HASHNODE *));
static int unsafe_chars		 PARAMS ((cpp_reader *, int, int));
static int macro_cleanup	 PARAMS ((cpp_buffer *, cpp_reader *));
static enum cpp_ttype macarg	 PARAMS ((cpp_reader *, int));
static void special_symbol	 PARAMS ((cpp_reader *, HASHNODE *));
static int compare_defs		 PARAMS ((cpp_reader *, DEFINITION *,
					  DEFINITION *));

/* Initial hash table size.  (It can grow if necessary - see hashtab.c.)  */
#define HASHSIZE 500

/* The arglist structure is built by create_definition to tell
   collect_expansion where the argument names begin.  That
   is, for a define like "#define f(x,y,z) foo+x-bar*y", the arglist
   would contain pointers to the strings x, y, and z.
   collect_expansion would then build a DEFINITION node,
   with reflist nodes pointing to the places x, y, and z had
   appeared.  So the arglist is just convenience data passed
   between these two routines.  It is not kept around after
   the current #define has been processed and entered into the
   hash table.  */

struct arg
{
  const U_CHAR *name;
  unsigned int len;
  char rest_arg;
};

struct arglist
{
  U_CHAR *namebuf;
  const struct arg *argv;
  int argc;
};


static DEFINITION *
collect_objlike_expansion PARAMS ((cpp_reader *, cpp_toklist *));
static DEFINITION *
collect_funlike_expansion PARAMS ((cpp_reader *, cpp_toklist *,
				   struct arglist *, unsigned int));
static unsigned int collect_params PARAMS ((cpp_reader *, cpp_toklist *,
					    struct arglist *));

static void warn_trad_stringify	PARAMS ((cpp_reader *, U_CHAR *, size_t,
					 unsigned int, const struct arg *));
static unsigned int trad_stringify PARAMS ((cpp_reader *, U_CHAR *, size_t,
					    unsigned int, const struct arg *,
					    struct reflist **,
					    struct reflist **, unsigned int));
static int duplicate_arg_p PARAMS ((U_CHAR *, U_CHAR *));
static void add_pat PARAMS ((struct reflist **, struct reflist **,
			     unsigned int, unsigned int, int, int, int, int));

/* This structure represents one parsed argument in a macro call.
   `raw' points to the argument text as written (`raw_length' is its length).
   `expanded' points to the argument's macro-expansion
   (its length is `expand_length').
   `stringified_length' is the length the argument would have
   if stringified.  */

/* raw and expanded are relative to ARG_BASE */
#define ARG_BASE ((pfile)->token_buffer)

struct argdata
{
  /* Strings relative to pfile->token_buffer */
  long raw, expanded, stringified;
  int raw_length, expand_length;
  int stringified_length;
};

static void scan_arguments	PARAMS ((cpp_reader *, DEFINITION *,
					  struct argdata *, const U_CHAR *));
static void stringify		PARAMS ((cpp_reader *, struct argdata *));
static void funlike_macroexpand	PARAMS ((cpp_reader *, HASHNODE *,
					 struct argdata *));

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

/* Calculate hash of a HASHNODE structure.  */
static unsigned int
hash_HASHNODE (x)
     const void *x;
{
  const HASHNODE *h = (const HASHNODE *)x;
  return h->hash;
}

/* Compare two HASHNODE structures.  */
static int
eq_HASHNODE (x, y)
     const void *x;
     const void *y;
{
  const HASHNODE *a = (const HASHNODE *)x;
  const HASHNODE *b = (const HASHNODE *)y;

  return (a->length == b->length
	  && !strncmp (a->name, b->name, a->length));
}

/* Destroy a HASHNODE.  */
static void
del_HASHNODE (x)
     void *x;
{
  HASHNODE *h = (HASHNODE *)x;
  
  if (h->type == T_MACRO)
    _cpp_free_definition (h->value.defn);
  else if (h->type == T_MCONST)
    free ((void *) h->value.cpval);
  free ((void *) h->name);
  free (h);
}

/* Allocate and initialize a HASHNODE structure.
   Caller must fill in the value field.  */

HASHNODE *
_cpp_make_hashnode (name, len, type, hash)
     const U_CHAR *name;
     size_t len;
     enum node_type type;
     unsigned long hash;
{
  HASHNODE *hp = (HASHNODE *) xmalloc (sizeof (HASHNODE));
  U_CHAR *p = xmalloc (len + 1);

  hp->type = type;
  hp->length = len;
  hp->name = p;
  hp->hash = hash;
  hp->disabled = 0;

  memcpy (p, name, len);
  p[len] = 0;

  return hp;
}

/* Find the hash node for name "name", which ends at the first
   non-identifier char.

   If LEN is >= 0, it is the length of the name.
   Otherwise, compute the length now.  */

HASHNODE *
_cpp_lookup (pfile, name, len)
     cpp_reader *pfile;
     const U_CHAR *name;
     int len;
{
  const U_CHAR *bp;
  HASHNODE dummy;

  if (len < 0)
    {
      for (bp = name; is_idchar (*bp); bp++);
      len = bp - name;
    }

  dummy.name = name;
  dummy.length = len;
  dummy.hash = _cpp_calc_hash (name, len);

  return (HASHNODE *) htab_find_with_hash (pfile->hashtab,
					   (void *)&dummy, dummy.hash);
}

/* Find the hashtable slot for name "name".  Used to insert or delete.  */

HASHNODE **
_cpp_lookup_slot (pfile, name, len, insert, hash)
     cpp_reader *pfile;
     const U_CHAR *name;
     int len;
     enum insert_option insert;
     unsigned long *hash;
{
  const U_CHAR *bp;
  HASHNODE dummy;
  HASHNODE **slot;

  if (len < 0)
    {
      for (bp = name; is_idchar (*bp); bp++)
	;

      len = bp - name;
    }

  dummy.name = name;
  dummy.length = len;
  dummy.hash = _cpp_calc_hash (name, len);

  slot = (HASHNODE **) htab_find_slot_with_hash (pfile->hashtab,
						 (void *) &dummy,
						 dummy.hash, insert);
  if (insert)
    *hash = dummy.hash;
  return slot;
}

/* Init the hash table.  In here so it can see the hash and eq functions.  */
void
_cpp_init_macro_hash (pfile)
     cpp_reader *pfile;
{
  pfile->hashtab = htab_create (HASHSIZE, hash_HASHNODE,
				eq_HASHNODE, del_HASHNODE);
}

/* Free a DEFINITION structure.  Used by delete_macro, and by
   do_define when redefining macros.  */

void
_cpp_free_definition (d)
     DEFINITION *d;
{
  struct reflist *ap, *nextap;

  for (ap = d->pattern; ap != NULL; ap = nextap)
    {
      nextap = ap->next;
      free (ap);
    }
  if (d->argnames)
    free (d->argnames);
  free (d);
}

static int
macro_cleanup (pbuf, pfile)
     cpp_buffer *pbuf;
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  HASHNODE *m = pbuf->macro;
  
  m->disabled = 0;
  if (m->type == T_FMACRO && pbuf->buf != m->value.defn->expansion)
    free ((PTR) pbuf->buf);
  else if (m->type != T_MACRO && m->type != T_FMACRO && m->type != T_CONST
	   && m->type != T_MCONST && m->type != T_XCONST)
    free ((PTR) pbuf->buf);
  return 0;
}

/* Create pat nodes.  */

static void
add_pat (pat, endpat, nchars, argno, raw_before, raw_after, strize, rest)
     struct reflist **pat, **endpat;
     unsigned int nchars;
     unsigned int argno;
     int raw_before, raw_after, strize, rest;
{
  struct reflist *tpat;
  tpat = (struct reflist *) xmalloc (sizeof (struct reflist));
  tpat->next = NULL;
  tpat->raw_before = raw_before;
  tpat->raw_after = raw_after;
  tpat->stringify = strize;
  tpat->rest_args = rest;
  tpat->argno = argno;
  tpat->nchars = nchars;

  if (*endpat == NULL)
    *pat = tpat;
  else
    (*endpat)->next = tpat;
  *endpat = tpat;
}  

/* Issue warnings for macro argument names seen inside strings.  */
static void
warn_trad_stringify (pfile, p, len, argc, argv)
     cpp_reader *pfile;
     U_CHAR *p;
     size_t len;
     unsigned int argc;
     const struct arg *argv;
{
  U_CHAR *limit;
  unsigned int i;

  limit = p + len;
  for (;;)
    {
      while (p < limit && !is_idstart (*p)) p++;
      if (p >= limit)
	break;

      for (i = 0; i < argc; i++)
	if (!strncmp (p, argv[i].name, argv[i].len)
	    && ! is_idchar (p[argv[i].len]))
	  {
	    cpp_warning (pfile,
		"macro argument \"%s\" would be stringified in traditional C",
			 argv[i].name);
	    break;
	  }
      p++;
      while (p < limit && is_idchar (*p)) p++;
      if (p >= limit)
	break;
    }
}

/* Generate pat nodes for macro arguments seen inside strings.  */
static unsigned int
trad_stringify (pfile, base, len, argc, argv, pat, endpat, last)
     cpp_reader *pfile;
     U_CHAR *base;
     size_t len;
     unsigned int argc;
     const struct arg *argv;
     struct reflist **pat, **endpat;
     unsigned int last;
{
  U_CHAR *p, *limit;
  unsigned int i;

  p = base;
  limit = base + len;
  for (;;)
    {
    proceed:
      while (p < limit && !is_idstart (*p)) p++;
      if (p >= limit)
	break;

      for (i = 0; i < argc; i++)
	if (!strncmp (p, argv[i].name, argv[i].len)
	    && ! is_idchar (p[argv[i].len]))
	  {
	    if (CPP_WTRADITIONAL (pfile))
	      cpp_warning (pfile, "macro argument \"%s\" is stringified",
			   argv[i].name);
	    /* Write out the string up to this point, and add a pat
	       node for the argument.  Note that the argument is NOT
	       stringified.  */
	    CPP_PUTS (pfile, base, p - base);
	    add_pat (pat, endpat, CPP_WRITTEN (pfile) - last, i /* argno */,
		     !is_hspace (p[-1]) /* raw_before */,
		     !is_hspace (p[argv[i].len]) /* raw_after */,
		     0 /* strize */,
		     argv[i].rest_arg);
	    last = CPP_WRITTEN (pfile);
	    base = p + argv[i].len;
	    goto proceed;
	  }
      p++;
      while (p < limit && is_idchar (*p)) p++;
      if (p >= limit)
	break;
    }
  CPP_PUTS (pfile, base, p - base);
  return last;
}

/* Read a replacement list for an object-like macro, and build the
   DEFINITION structure.  LIST contains the replacement list,
   beginning at 1.  */
static DEFINITION *
collect_objlike_expansion (pfile, list)
     cpp_reader *pfile;
     cpp_toklist *list;
{
  DEFINITION *defn;
  unsigned int i;
  unsigned int start;
  int last_was_paste = 0;
  U_CHAR *exp;
  size_t len;

  /* We copy the expansion text into the token_buffer, then out to
     its proper home.  */
  start = CPP_WRITTEN (pfile);
  CPP_PUTS (pfile, "\r ", 2);

  for (i = 1; i < list->tokens_used; i++)
    {
      switch (list->tokens[i].type)
	{
	case CPP_POP:
	case CPP_EOF:
	  cpp_ice (pfile, "EOF in collect_expansion");
	  /* fall through */
	case CPP_VSPACE:
	  goto done;

	case CPP_PASTE:
	  /* ## is not special if it appears right after another ##;
	     nor is it special if -traditional.  */
	  if (last_was_paste || CPP_TRADITIONAL (pfile))
	    break;
	  if (i == 1)
	    cpp_error (pfile, "`##' at start of macro definition");

	  last_was_paste = 1;
	  continue;

	default:;
	}

      if (i > 1 && !last_was_paste
	  && (list->tokens[i].flags & PREV_WHITESPACE))
	CPP_PUTC (pfile, ' ');

      CPP_PUTS (pfile,
		list->tokens[i].val.name.offset + list->namebuf,
		list->tokens[i].val.name.len);
      last_was_paste = 0;
    }
 done:

  if (last_was_paste)
    cpp_error (pfile, "`##' at end of macro definition");

  CPP_PUTS (pfile, "\r ", 2);
  len = CPP_WRITTEN (pfile) - start;
  CPP_SET_WRITTEN (pfile, start);

  if (len <= 4)
    cpp_ice (pfile, "empty object-like macro went through full #define");

  exp = (U_CHAR *) xmalloc (len + 1);
  memcpy (exp, pfile->token_buffer + start, len);
  exp[len] = '\0';

  defn = (DEFINITION *) xmalloc (sizeof (DEFINITION));
  defn->length = len;
  defn->expansion = exp;
  defn->pattern = 0;
  defn->rest_args = 0;
  defn->argnames = 0;
  defn->nargs = -1;

  return defn;
}

/* Read a replacement list for a function-like macro, and build the
   DEFINITION structure.  LIST contains the replacement list,
   beginning at REPLACEMENT.  ARGLIST specifies the formal parameters
   to look for in the text of the definition.  */

static DEFINITION *
collect_funlike_expansion (pfile, list, arglist, replacement)
     cpp_reader *pfile;
     cpp_toklist *list;
     struct arglist *arglist;
     unsigned int replacement;
{
  DEFINITION *defn;
  struct reflist *pat = 0, *endpat = 0;
  enum cpp_ttype token;
  unsigned int start, last;
  unsigned int i;
  int j, argc;
  size_t len;
  const struct arg *argv;
  U_CHAR *tok, *exp;
  enum { START = 0, NORM, ARG, STRIZE, PASTE } last_token = START;

  argv = arglist->argv;
  argc = arglist->argc;

  /* We copy the expansion text into the token_buffer, then out to
     its proper home.  */
  last = start = CPP_WRITTEN (pfile);
  CPP_PUTS (pfile, "\r ", 2);

  for (i = replacement; i < list->tokens_used; i++)
    {
      token = list->tokens[i].type;
      tok = list->tokens[i].val.name.offset + list->namebuf;
      len = list->tokens[i].val.name.len;
      switch (token)
	{
	case CPP_POP:
	case CPP_EOF:
	  cpp_ice (pfile, "EOF in collect_expansion");
	  /* fall through */
	case CPP_VSPACE:
	  goto done;

	case CPP_HASH:
	  /* # is special in function-like macros with no args.
	     (6.10.3.2 para 1.)  However, it is not special after
	     PASTE. (Implied by 6.10.3.3 para 4.)  Nor is it special
	     if -traditional.  */
	  if (last_token == PASTE || CPP_TRADITIONAL (pfile))
	    break;
	  last_token = STRIZE;
	  continue;

	case CPP_PASTE:
	  /* ## is not special if it appears right after another ##;
	     nor is it special if -traditional.  */
	  if (last_token == PASTE || CPP_TRADITIONAL (pfile))
	    break;

	  if (last_token == START)
	    cpp_error (pfile, "`##' at start of macro definition");
	  else if (last_token == ARG)
	    /* If the last token was an argument, mark it raw_after.  */
	    endpat->raw_after = 1;
	  else if (last_token == STRIZE)
	    /* Oops - that wasn't a stringify operator.  */
	    CPP_PUTC (pfile, '#');

	  last_token = PASTE;
	  continue;

	default:;
	}

      if (last_token != PASTE && last_token != START
	  && (list->tokens[i].flags & PREV_WHITESPACE))
	CPP_PUTC (pfile, ' ');
      if (last_token == ARG && CPP_TRADITIONAL (pfile)
	  && !(list->tokens[i].flags & PREV_WHITESPACE))
	endpat->raw_after = 1;

      switch (token)
	{
	case CPP_STRING:
	case CPP_CHAR:
	  if (argc == 0)
	    goto norm;
	  if (CPP_TRADITIONAL (pfile))
	    {
	      last = trad_stringify (pfile, tok, len, argc, argv,
				     &pat, &endpat, last);
	      break;
	    }
	  else
	    {
	      if (CPP_WTRADITIONAL (pfile))
		warn_trad_stringify (pfile, tok, len, argc, argv);
	      goto norm;
	    }
	  
	case CPP_NAME:
	  for (j = 0; j < argc; j++)
	    if (argv[j].len == len
		&& !strncmp (tok, argv[j].name, argv[j].len))
	      goto addref;

	  /* fall through */
	default:
	norm:
	  if (last_token == STRIZE)
	    cpp_error (pfile, "# is not followed by a macro argument name");
	  CPP_PUTS (pfile, tok, len);
	  last_token = NORM;
	}
      continue;

    addref:
      {
	int raw_before = (last_token == PASTE
			  || (CPP_TRADITIONAL (pfile)
			      && !(list->tokens[i].flags & PREV_WHITESPACE)));
      
	add_pat (&pat, &endpat,
		 CPP_WRITTEN (pfile) - last /* nchars */, j /* argno */,
		 raw_before, 0 /* raw_after */,
		 (last_token == STRIZE), argv[j].rest_arg);
      
	last = CPP_WRITTEN (pfile);
      }
      last_token = ARG;
    }
 done:

  if (last_token == STRIZE)
    cpp_error (pfile, "`#' is not followed by a macro argument name");
  else if (last_token == PASTE)
    cpp_error (pfile, "`##' at end of macro definition");

    CPP_PUTS (pfile, "\r ", 2);
  len = CPP_WRITTEN (pfile) - start;
  CPP_SET_WRITTEN (pfile, start);

  exp = (U_CHAR *) xmalloc (len + 1);
  memcpy (exp, pfile->token_buffer + start, len);
  exp[len] = '\0';

  defn = (DEFINITION *) xmalloc (sizeof (DEFINITION));
  defn->length = len;
  defn->expansion = exp;
  defn->pattern = pat;
  defn->rest_args = argc && argv[argc - 1].rest_arg;
  defn->nargs = argc;
  defn->argnames = arglist->namebuf;
  if (argv)
    free ((PTR) argv);
  return defn;
}

/* Is argument NEW, which has just been added to the argument list,
   a duplicate of a previous argument name?  */
static int
duplicate_arg_p (args, new)
     U_CHAR *args, *new;
{
  size_t newlen = strlen (new) + 1;
  size_t oldlen;

  while (args < new)
    {
      oldlen = strlen (args) + 1;
      if (!memcmp (args, new, MIN (oldlen, newlen)))
	return 1;
      args += oldlen;
    }
  return 0;
}

static unsigned int
collect_params (pfile, list, arglist)
     cpp_reader *pfile;
     cpp_toklist *list;
     struct arglist *arglist;
{
  struct arg *argv = 0;
  U_CHAR *namebuf, *p, *tok;
  unsigned int len, argslen;
  unsigned int argc, a, i, j;

  /* The formal parameters list starts at token 1.  */
  if (list->tokens[1].type != CPP_OPEN_PAREN)
    {
      cpp_ice (pfile, "first token = %d not %d in collect_formal_parameters",
	       list->tokens[1].type, CPP_OPEN_PAREN);
      return 0;
    }

  /* Scan once and count the number of parameters; also check for
     syntax errors here.  */
  argc = 0;
  argslen = 0;
  for (i = 2; i < list->tokens_used; i++)
    switch (list->tokens[i].type)
      {
      case CPP_NAME:
	argslen += list->tokens[i].val.name.len + 1;
	argc++;
	break;
      case CPP_COMMA:
	break;
      case CPP_CLOSE_PAREN:
	goto scanned;
      case CPP_VSPACE:
	cpp_error_with_line (pfile, list->line, list->tokens[i].col,
			     "missing right paren in macro argument list");
	return 0;

      default:
	cpp_error_with_line (pfile, list->line, list->tokens[i].col,
			     "syntax error in #define");
	return 0;

      case CPP_ELLIPSIS:
	if (list->tokens[i-1].type != CPP_NAME)
	  {
	    argslen += sizeof "__VA_ARGS__";
	    argc++;
	  }
	i++;
	if (list->tokens[i].type != CPP_CLOSE_PAREN)
	  {
	    cpp_error_with_line (pfile, list->line, list->tokens[i].col,
				 "another parameter follows \"...\"");
	    return 0;
	  }
	goto scanned;
      }

  cpp_ice (pfile, "collect_params: unreachable - i=%d, ntokens=%d, type=%d",
	   i, list->tokens_used, list->tokens[i-1].type);
  return 0;

 scanned:
  if (argc == 0)	/* function-like macro, no arguments */
    {
      arglist->argc = 0;
      arglist->argv = 0;
      arglist->namebuf = 0;
      return i + 1;
    }
  if (argslen == 0)
    {
      cpp_ice (pfile, "collect_params: argc=%d argslen=0", argc);
      return 0;
    }

  /* Now allocate space and copy the suckers.  */
  argv = (struct arg *) xmalloc (argc * sizeof (struct arg));
  namebuf = (U_CHAR *) xmalloc (argslen);
  p = namebuf;
  a = 0;
  for (j = 2; j < i; j++)
    switch (list->tokens[j].type)
      {
      case CPP_NAME:
	tok = list->tokens[j].val.name.offset + list->namebuf;
	len = list->tokens[j].val.name.len;
	memcpy (p, tok, len);
	p[len] = '\0';
	if (duplicate_arg_p (namebuf, p))
	  {
	    cpp_error (pfile, "duplicate macro argument name \"%s\"", tok);
	    a++;
	    break;
	  }
	if (CPP_PEDANTIC (pfile) && CPP_OPTION (pfile, c99)
	    && len == sizeof "__VA_ARGS__" - 1
	    && !strcmp (p, "__VA_ARGS__"))
	  cpp_pedwarn (pfile,
	"C99 does not permit use of __VA_ARGS__ as a macro argument name");
	argv[a].len = len;
	argv[a].name = p;
	argv[a].rest_arg = 0;
	p += len;
	a++;
	break;

      case CPP_COMMA:
	break;

      case CPP_ELLIPSIS:
	if (list->tokens[j-1].type != CPP_NAME)
	  {
	    if (CPP_PEDANTIC (pfile) && ! CPP_OPTION (pfile, c99))
	      cpp_pedwarn (pfile, "C89 does not permit varargs macros");

	    argv[a].len = sizeof "__VA_ARGS__" - 1;
	    argv[a].name = p;
	    argv[a].rest_arg = 1;
	    strcpy (p, "__VA_ARGS__");
	  }
	else
	  {
	    if (CPP_PEDANTIC (pfile))
	      cpp_pedwarn (pfile,
			   "ISO C does not permit named varargs macros");
	    argv[a-1].rest_arg = 1;
	  }
	break;

      default:
	cpp_ice (pfile, "collect_params: impossible token type %d",
		 list->tokens[j].type);
      }

  arglist->argc = argc;
  arglist->argv = argv;
  arglist->namebuf = namebuf;
  return i + 1;
}

/* Create a DEFINITION node for a macro.  The replacement text
   (including formal parameters if present) is in LIST.  If FUNLIKE is
   true, this is a function-like macro.  */

int
_cpp_create_definition (pfile, list, hp)
     cpp_reader *pfile;
     cpp_toklist *list;
     HASHNODE *hp;
{
  DEFINITION *defn = 0;
  U_CHAR *cpval = 0;
  enum node_type ntype;
  int ok;

  /* Special-case a few simple and common idioms:
     #define TOKEN   // nothing
     #define TOKEN TOKEN
     #define TOKEN OTHERTOKEN

     Might also be good to special-case these:

     #define FUNC()  // nothing
     #define FUNC(a, b, ...) // nothing
     #define FUNC(a, b, c) FUNC(a, b, c)
     #define FUNC(a, b, c) foobar(a, b, c)  */

  if (list->tokens_used == 2)
    ntype = T_EMPTY;    /* Empty definition of object-like macro.  */
  else if (list->tokens_used == 3 && list->tokens[1].type == CPP_NAME)
    {
      if (list->tokens[0].val.name.len == list->tokens[1].val.name.len
	  && !strncmp (list->tokens[0].val.name.offset + list->namebuf,
		       list->tokens[1].val.name.offset + list->namebuf,
		       list->tokens[0].val.name.len))
	ntype = T_IDENTITY;
      else
	{
	  ntype = T_MCONST;
	  cpval = xmalloc (list->tokens[1].val.name.len + 1);
	  memcpy (cpval, list->tokens[1].val.name.offset + list->namebuf,
		  list->tokens[1].val.name.len);
	  cpval[list->tokens[1].val.name.len] = '\0';
	}
    }

  /* The macro is function-like only if the next character,
     with no intervening whitespace, is '('.  */
  else if (list->tokens[1].type == CPP_OPEN_PAREN
	   && ! (list->tokens[1].flags & PREV_WHITESPACE))
    {
      struct arglist args;
      int replacement;

      replacement = collect_params (pfile, list, &args);
      if (replacement == 0)
	return 0;
      defn = collect_funlike_expansion (pfile, list, &args, replacement);
      if (defn == 0)
	return 0;

      ntype = T_FMACRO;
    }

  /* Otherwise it is an object-like macro, and C99 requires
     whitespace after the name (6.10.3 para 3).  */
  else
    {
      if (! (list->tokens[1].flags & PREV_WHITESPACE))
	cpp_pedwarn (pfile,
		     "The C standard requires whitespace after #define %s",
		     hp->name);

      defn = collect_objlike_expansion (pfile, list);
      if (defn == 0)
	return 0;

      ntype = T_MACRO;
    }

  /* Check for a redefinition, and its legality.  Redefining a macro
     of whatever stripe is ok if the definitions are the same.
     Redefining a built-in _constant_ (T_CONST or T_XCONST) is ok only
     with -D.  Otherwise a redefinition is not ok.  */

  switch (hp->type)
    {
    case T_VOID:  ok = 1; break;
    default:	  ok = 0; break;

    case T_MACRO: case T_FMACRO:
      ok = (ntype == hp->type && !compare_defs (pfile, defn, hp->value.defn));
      break;
    case T_IDENTITY:
    case T_EMPTY:
      ok = (ntype == hp->type);
      break;
    case T_MCONST:
      ok = (ntype == hp->type && !strcmp (cpval, hp->value.cpval));
      break;
    case T_CONST:
    case T_XCONST:
      ok = ! pfile->done_initializing;
      break;
    }

  /* Print the warning or error if it's not ok.  */
  if (! ok)
    {
      cpp_pedwarn (pfile, "\"%s\" redefined", hp->name);
      if (pfile->done_initializing)
	cpp_pedwarn_with_file_and_line (pfile, hp->file, hp->line, hp->col,
			"this is the location of the previous definition");
    }

  /* And replace the old definition (if any).  */

  if (hp->type == T_MACRO || hp->type == T_FMACRO)
    _cpp_free_definition (hp->value.defn);
  else if (hp->type == T_MCONST || hp->type == T_XCONST)
    free ((PTR) hp->value.cpval);

  if (ntype == T_MACRO || ntype == T_FMACRO)
    hp->value.defn = defn;
  else
    hp->value.cpval = cpval;

  hp->type = ntype;
  hp->file = CPP_BUFFER (pfile)->nominal_fname;
  hp->line = list->line;
  hp->col  = list->tokens[0].col;
  return 1;
}

/*
 * Parse a macro argument and append the info on PFILE's token_buffer.
 * REST_ARGS means to absorb the rest of the args.
 * Return nonzero to indicate a syntax error.
 */

static enum cpp_ttype
macarg (pfile, rest_args)
     cpp_reader *pfile;
     int rest_args;
{
  int paren = 0;
  enum cpp_ttype token;

  /* Try to parse as much of the argument as exists at this
     input stack level.  */
  for (;;)
    {
      token = cpp_get_token (pfile);
      switch (token)
	{
	case CPP_EOF:
	  return token;
	case CPP_POP:
	  /* If we've hit end of file, it's an error (reported by caller).
	     Ditto if it's the end of cpp_expand_to_buffer text.
	     If we've hit end of macro, just continue.  */
	  if (!CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
	    return token;
	  break;
	case CPP_OPEN_PAREN:
	  paren++;
	  break;
	case CPP_CLOSE_PAREN:
	  if (--paren < 0)
	    goto found;
	  break;
	case CPP_COMMA:
	  /* if we've returned to lowest level and
	     we aren't absorbing all args */
	  if (paren == 0 && rest_args == 0)
	    goto found;
	  break;
	found:
	  /* Remove ',' or ')' from argument buffer.  */
	  CPP_ADJUST_WRITTEN (pfile, -1);
	  return token;
	default:;
	}
    }
}


static const char * const monthnames[] =
{
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};

/* Place into PFILE a quoted string representing the string SRC.
   Caller must reserve enough space in pfile->token_buffer.  */

void
_cpp_quote_string (pfile, src)
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
	return;
      }
}

/*
 * expand things like __FILE__.  Place the expansion into the output
 * buffer *without* rescanning.
 */

#define DSC(str) (const U_CHAR *)str, sizeof str - 1
static void
special_symbol (pfile, hp)
     cpp_reader *pfile;
     HASHNODE *hp;
{
  const char *buf;
  cpp_buffer *ip;

  switch (hp->type)
    {
    case T_FILE:
    case T_BASE_FILE:
      ip = cpp_file_buffer (pfile);
      if (ip == NULL)
	{
	  CPP_PUTS (pfile, "\"\"", 2);
	  return;
	}
      if (hp->type == T_BASE_FILE)
	while (CPP_PREV_BUFFER (ip) != NULL)
	  ip = CPP_PREV_BUFFER (ip);

      buf = ip->nominal_fname;
      CPP_RESERVE (pfile, 3 + 4 * strlen (buf));
      _cpp_quote_string (pfile, buf);
      return;

    case T_INCLUDE_LEVEL:
      {
	int true_indepth = 0;
	ip = cpp_file_buffer (pfile);
	while (ip)
	  {
	    true_indepth++;
	    ip = CPP_PREV_BUFFER (ip);
	  }

	CPP_RESERVE (pfile, 10);
	sprintf (CPP_PWRITTEN (pfile), "%d", true_indepth);
	CPP_ADJUST_WRITTEN (pfile, strlen (CPP_PWRITTEN (pfile)));
	return;
      }

    case T_STDC:
#ifdef STDC_0_IN_SYSTEM_HEADERS
      ip = cpp_file_buffer (pfile);
      if (ip && ip->system_header_p
	  && !cpp_defined (pfile, DSC("__STRICT_ANSI__")))
	{
	  CPP_PUTC (pfile, '0');
	  return;
	}
#endif
    constant:
      buf = hp->value.cpval;
      if (!buf || *buf == '\0')
	return;

      CPP_PUTS (pfile, buf, strlen (buf));
      return;

    case T_SPECLINE:
      ip = cpp_file_buffer (pfile);
      if (ip == NULL)
	{
	  CPP_PUTC (pfile, '0');
	  return;
	}
      CPP_RESERVE (pfile, 10);
      sprintf (CPP_PWRITTEN (pfile), "%u", CPP_BUF_LINE (ip));
      CPP_ADJUST_WRITTEN (pfile, strlen (CPP_PWRITTEN (pfile)));
      return;

    case T_DATE:
    case T_TIME:
      /* Generate both __DATE__ and __TIME__, stuff them into their
	 respective hash nodes, and mark the nodes T_XCONST so we
	 don't have to do this again.  We don't generate these strings
	 at init time because time() and localtime() are very slow on
	 some systems.  */
      {
	time_t tt = time (NULL);
	struct tm *tb = localtime (&tt);
	HASHNODE *d, *t;

	if (hp->type == T_DATE)
	  d = hp, t = _cpp_lookup (pfile, DSC("__TIME__"));
	else
	  t = hp, d = _cpp_lookup (pfile, DSC("__DATE__"));

	d->value.cpval = xmalloc (sizeof "'Oct 11 1347'");
	sprintf ((char *)d->value.cpval, "\"%s %2d %4d\"",
		 monthnames[tb->tm_mon], tb->tm_mday, tb->tm_year + 1900);
	d->type = T_XCONST;

	t->value.cpval = xmalloc (sizeof "'12:34:56'");
	sprintf ((char *)t->value.cpval, "\"%02d:%02d:%02d\"",
		 tb->tm_hour, tb->tm_min, tb->tm_sec);
	t->type = T_XCONST;
	goto constant;
      }

    case T_POISON:
      cpp_error (pfile, "attempt to use poisoned `%s'.", hp->name);
      CPP_PUTC (pfile, '0');
      break;

    default:
      cpp_ice (pfile, "invalid special hash type");
      return;
    }
}
#undef DSC

/* Expand a macro call.
   HP points to the symbol that is the macro being called.
   Put the result of expansion onto the input stack
   so that subsequent input by our caller will use it.

   If macro wants arguments, caller has already verified that
   an argument list follows; arguments come from the input stack.  */

void
_cpp_macroexpand (pfile, hp)
     cpp_reader *pfile;
     HASHNODE *hp;
{
  DEFINITION *defn;
  struct argdata *args;
  unsigned int old_written;
  int i;

  /* Object like macro - most common case.  */
  if (hp->type == T_MACRO)
    {
      push_macro_expansion (pfile, hp->value.defn->expansion,
			    hp->value.defn->length, hp);
      return;
    }

  /* Or might it be a constant string?  */
  if (hp->type == T_MCONST || hp->type == T_CONST || hp->type == T_XCONST)
    {
      const U_CHAR *cpval = hp->value.cpval;
      if (cpval && *cpval != '\0')
	push_macro_expansion (pfile, cpval, strlen (cpval), hp);
      return;
    }

  /* Or a special symbol?  */
  if (hp->type != T_FMACRO)
    {
      U_CHAR *xbuf;
      unsigned int len, old_written = CPP_WRITTEN (pfile);
      
      special_symbol (pfile, hp);
      len = CPP_WRITTEN (pfile) - old_written;
      CPP_SET_WRITTEN (pfile, old_written);
      if (len == 0)
	return;

      xbuf = (U_CHAR *) xmalloc (len + 1);
      memcpy (xbuf, CPP_PWRITTEN (pfile), len);
      xbuf[len] = '\0';
      push_macro_expansion (pfile, xbuf, len, hp);
      return;
    }

  /* Okay, it's a full-on function-like macro...  */
  old_written = CPP_WRITTEN (pfile);
  defn = hp->value.defn;

  args = alloca (MAX (defn->nargs, 1) * sizeof (struct argdata));
  for (i = 0; i < MAX (defn->nargs, 1); i++)
    {
      args[i].raw = args[i].expanded = 0;
      args[i].raw_length = 0;
      args[i].expand_length = args[i].stringified_length = -1;
    }

  pfile->output_escapes++;
  scan_arguments (pfile, defn, args, hp->name);

  /* If macro wants zero args, we parsed the arglist for checking only.
     Read directly from the macro definition.  */
  if (defn->nargs == 0 || defn->pattern == 0)
    {
      /* If the defn is the empty string, don't bother pushing it.  */
      if (defn->length > 4)
	push_macro_expansion (pfile, defn->expansion, defn->length, hp);
    }
  else
    funlike_macroexpand (pfile, hp, args);

  CPP_SET_WRITTEN (pfile, old_written);
  pfile->output_escapes--;
}

static void
scan_arguments (pfile, defn, args, name)
     cpp_reader *pfile;
     DEFINITION *defn;
     struct argdata *args;
     const U_CHAR *name;
{
  enum cpp_ttype token;
  unsigned int start_line, start_column;
  unsigned int nargs = defn->nargs;
  unsigned int i;
  
  cpp_buffer *ip = cpp_file_buffer (pfile);
  if (ip)
    {
      start_line = CPP_BUF_LINE (ip);
      start_column = CPP_BUF_COL (ip);
    }
  else
    start_line = start_column = 0;

  /* Parse all the macro args that are supplied.  I counts them.  The
     first NARGS args are stored in ARGS.  The rest are discarded.  If
     rest_args is set then we assume macarg absorbed the rest of the
     args.  */
  i = 0;

  /* Skip over the opening parenthesis.  */
  CPP_OPTION (pfile, discard_comments)++;
  pfile->no_macro_expand++;
  pfile->no_directives++;

  token = cpp_get_non_space_token (pfile);
  if (token != CPP_OPEN_PAREN)
    cpp_ice (pfile, "macroexpand: unexpected token %d (wanted LPAREN)",
	     token);
  CPP_ADJUST_WRITTEN (pfile, -1);

  token = CPP_EOF;
  do
    {
      if (i < MAX (nargs, 1))
	{
	  args[i].raw = CPP_WRITTEN (pfile);
	  token = macarg (pfile, (i == nargs - 1 && defn->rest_args));
	  args[i].raw_length = CPP_WRITTEN (pfile) - args[i].raw;
	}
      else
	token = macarg (pfile, 0);
      if (token == CPP_EOF || token == CPP_POP)
	cpp_error_with_line (pfile, start_line, start_column,
			     "unterminated macro call");
      i++;
    }
  while (token == CPP_COMMA);
  CPP_OPTION (pfile, discard_comments)--;
  pfile->no_macro_expand--;
  pfile->no_directives--;
  if (token != CPP_CLOSE_PAREN)
    return;

  /* foo ( ) is equivalent to foo () unless foo takes exactly one
     argument, in which case the former is allowed and the latter
     is not.  XXX C99 is silent on this rule, but it seems
     inconsistent to me.  */
  if (i == 1 && nargs == 0)
    {
      register U_CHAR *bp = ARG_BASE + args[0].raw;
      register U_CHAR *lim = bp + args[0].raw_length;
      while (bp != lim && is_space(*bp))
	bp++;
      if (bp == lim)
	i = 0;
    }

  /* Don't output an error message if we have already output one for
     a parse error above.  */
  if (nargs == 0 && i > 0)
    {
      cpp_error (pfile, "arguments given to macro `%s'", name);
    }
  else if (i < nargs)
    {
      /* traditional C allows foo() if foo wants one argument.  */
      if (nargs == 1 && i == 0 && CPP_TRADITIONAL (pfile))
	;
      /* the rest args token is allowed to absorb 0 tokens */
      else if (i == nargs - 1 && defn->rest_args)
	;
      else if (i == 0)
	cpp_error (pfile, "macro `%s' used without args", name);
      else if (i == 1)
	cpp_error (pfile, "macro `%s' used with just one arg", name);
      else
	cpp_error (pfile, "macro `%s' used with only %d args", name, i);
    }
  else if (i > nargs)
    {
      cpp_error (pfile, "macro `%s' used with too many (%d) args", name, i);
    }
}

static void
stringify (pfile, arg)
     cpp_reader *pfile;
     struct argdata *arg;
{
  int arglen = arg->raw_length;
  int escaped = 0;
  int in_string = 0;
  int c;
  int i;
  /* Initially need_space is -1.  Otherwise, 1 means the previous
     character was a space, but we suppressed it; 0 means the previous
     character was a non-space.  */
  int need_space = -1;
  i = 0;
  arg->stringified = CPP_WRITTEN (pfile);
  CPP_PUTC (pfile, '\"');	/* insert beginning quote */
  for (; i < arglen; i++)
    {
      c = (ARG_BASE + arg->raw)[i];

      if (!in_string)
	{
	  /* Delete "\r " and "\r-" escapes.  */
	  if (c == '\r')
	    {
	      i++;
	      continue;
	    }
	  /* Internal sequences of whitespace are replaced by one
	     space except within a string or char token. */
	  else if (is_space(c))
	    {
	      if (need_space == 0)
		need_space = 1;
	      continue;
	    }
	  else if (need_space > 0)
	    CPP_PUTC (pfile, ' ');
	  need_space = 0;
	}

      if (escaped)
	escaped = 0;
      else
	{
	  if (c == '\\')
	    escaped = 1;
	  if (in_string)
	    {
	      if (c == in_string)
		in_string = 0;
	    }
	  else if (c == '\"' || c == '\'')
	    in_string = c;
	}

      /* Escape these chars */
      if (c == '\"' || (in_string && c == '\\'))
	CPP_PUTC (pfile, '\\');
      if (ISPRINT (c))
	CPP_PUTC (pfile, c);
      else
	{
	  CPP_RESERVE (pfile, 4);
	  sprintf ((char *) CPP_PWRITTEN (pfile), "\\%03o", (unsigned int) c);
	  CPP_ADJUST_WRITTEN (pfile, 4);
	}
    }
  CPP_PUTC (pfile, '\"');	/* insert ending quote */
  arg->stringified_length  = CPP_WRITTEN (pfile) - arg->stringified;
}

static void
funlike_macroexpand (pfile, hp, args)
     cpp_reader *pfile;
     HASHNODE *hp;
     struct argdata *args;
{
  DEFINITION *defn = hp->value.defn;
  register U_CHAR *xbuf;
  int xbuf_len;
  U_CHAR *exp = defn->expansion;
  int offset;	/* offset in expansion, copied a piece at a time */
  int totlen;	/* total amount of exp buffer filled so far */
  struct reflist *ap, *last_ap;
  int i;

  /* Compute length in characters of the macro's expansion.
     Also count number of times each arg is used.  */
  xbuf_len = defn->length;
  for (ap = defn->pattern; ap != NULL; ap = ap->next)
    {
      if (ap->stringify)
	{
	  /* Stringify if it hasn't already been */
	  if (args[ap->argno].stringified_length < 0)
	    stringify (pfile, &args[ap->argno]);
	  xbuf_len += args[ap->argno].stringified_length;
	}
      else if (ap->raw_before || ap->raw_after)
	/* Add 4 for two \r-space markers to prevent
	   token concatenation.  */
	xbuf_len += args[ap->argno].raw_length + 4;
      else
	{
	  /* We have an ordinary (expanded) occurrence of the arg.
	     So compute its expansion, if we have not already.  */
	  if (args[ap->argno].expand_length < 0)
	    {
	      args[ap->argno].expanded = CPP_WRITTEN (pfile);
	      _cpp_expand_to_buffer (pfile, ARG_BASE + args[ap->argno].raw,
				     args[ap->argno].raw_length);

	      args[ap->argno].expand_length
		= CPP_WRITTEN (pfile) - args[ap->argno].expanded;
	    }

	  /* Add 4 for two \r-space markers to prevent
	     token concatenation.  */
	  xbuf_len += args[ap->argno].expand_length + 4;
	}
    }

  xbuf = (U_CHAR *) xmalloc (xbuf_len + 1);

  /* Generate in XBUF the complete expansion with arguments
     substituted in.  TOTLEN is the total size generated so far.
     OFFSET is the index in the definition of where we are copying
     from.  */
  offset = totlen = 0;
  for (last_ap = NULL, ap = defn->pattern; ap != NULL;
       last_ap = ap, ap = ap->next)
    {
      register struct argdata *arg = &args[ap->argno];
      int count_before = totlen;

      /* Add chars to XBUF.  */
      i = ap->nchars;
      memcpy (&xbuf[totlen], &exp[offset], i);
      totlen += i;
      offset += i;

      /* If followed by an empty rest arg with concatenation,
	 delete the last run of nonwhite chars.  */
      if (arg->raw_length == 0 && totlen > count_before
	  && ((ap->rest_args && ap->raw_before)
	      || (last_ap != NULL && last_ap->rest_args
		  && last_ap->raw_after)))
	{
	  /* Delete final whitespace.  */
	  while (totlen > count_before && is_space(xbuf[totlen - 1]))
	    totlen--;

	  /* Delete the nonwhites before them.  */
	  while (totlen > count_before && !is_space(xbuf[totlen - 1]))
	    totlen--;
	}

      if (ap->stringify != 0)
	{
	  memcpy (xbuf + totlen, ARG_BASE + arg->stringified,
		  arg->stringified_length);
	  totlen += arg->stringified_length;
	}
      else if (ap->raw_before || ap->raw_after)
	{
	  U_CHAR *p1 = ARG_BASE + arg->raw;
	  U_CHAR *l1 = p1 + arg->raw_length;
	  if (ap->raw_before)
	    {
	      /* Arg is concatenated before: delete leading whitespace,
		 whitespace markers, and no-reexpansion markers.  */
	      while (p1 != l1)
		{
		  if (is_space(p1[0]))
		    p1++;
		  else if (p1[0] == '\r')
		    p1 += 2;
		  else
		    break;
		}
	    }
	  if (ap->raw_after)
	    {
	      /* Arg is concatenated after: delete trailing whitespace,
		 whitespace markers, and no-reexpansion markers.  */
	      while (p1 != l1)
		{
		  if (is_space(l1[-1]))
		    l1--;
		  else if (l1[-1] == '\r')
		    l1--;
		  else if (l1[-1] == '-')
		    {
		      if (l1 != p1 + 1 && l1[-2] == '\r')
			l1 -= 2;
		      else
			break;
		    }
		  else
		    break;
		}
	    }

	  /* Delete any no-reexpansion marker that precedes
	     an identifier at the beginning of the argument. */
	  if (p1[0] == '\r' && p1[1] == '-')
	    p1 += 2;

	  memcpy (xbuf + totlen, p1, l1 - p1);
	  totlen += l1 - p1;
	}
      else
	{
	  U_CHAR *expanded = ARG_BASE + arg->expanded;
	  if (!ap->raw_before && totlen > 0 && arg->expand_length
	      && unsafe_chars (pfile, xbuf[totlen - 1], expanded[0]))
	    {
	      xbuf[totlen++] = '\r';
	      xbuf[totlen++] = ' ';
	    }

	  memcpy (xbuf + totlen, expanded, arg->expand_length);
	  totlen += arg->expand_length;

	  if (!ap->raw_after && totlen > 0 && offset < defn->length
	      && unsafe_chars (pfile, xbuf[totlen - 1], exp[offset]))
	    {
	      xbuf[totlen++] = '\r';
	      xbuf[totlen++] = ' ';
	    }
	}
    }

  /* if there is anything left of the definition
     after handling the arg list, copy that in too.  */

  for (i = offset; i < defn->length; i++)
    xbuf[totlen++] = exp[i];
  xbuf[totlen] = 0;

  if (totlen > xbuf_len)
    /* Just die - we've trashed the heap at this point.  */
    abort ();
  
  /* Now put the expansion on the input stack
     so our caller will commence reading from it.  */
  push_macro_expansion (pfile, xbuf, totlen, hp);
}

/* Return 1 iff a token ending in C1 followed directly by a token C2
   could cause mis-tokenization.  */

static int
unsafe_chars (pfile, c1, c2)
     cpp_reader *pfile;
     int c1, c2;
{
  /* If c2 is EOF, that's always safe.  */
  if (c2 == EOF)
    return 0;

  switch (c1)
    {
    case EOF:
      /* We don't know what the previous character was.  We do know
	 that it can't have been an idchar (or else it would have been
	 pasted with the idchars of the macro name), and there are a
	 number of second characters for which it doesn't matter what
	 the first was.  */
      if (is_idchar (c2) || c2 == '\'' || c2 == '\"'
	  || c2 == '(' || c2 == '[' || c2 == '{'
	  || c2 == ')' || c2 == ']' || c2 == '}')
	return 0;
      return 1;

    case '+':  case '-':
      if (c2 == c1 || c2 == '=')
	return 1;
      goto letter;

    case 'e':  case 'E':  case 'p':  case 'P':
      if (c2 == '-' || c2 == '+')
	return 1;		/* could extend a pre-processing number */
      goto letter;

    case '$':
      if (CPP_OPTION (pfile, dollars_in_ident))
	goto letter;
      return 0;

    case 'L':
      if (c2 == '\'' || c2 == '\"')
	return 1;		/* Could turn into L"xxx" or L'xxx'.  */
      goto letter;

    case '.':  case '0':  case '1':  case '2':  case '3':
    case '4':  case '5':  case '6':  case '7':  case '8':  case '9':
    case '_':  case 'a':  case 'b':  case 'c':  case 'd':  case 'f':
    case 'g':  case 'h':  case 'i':  case 'j':  case 'k':  case 'l':
    case 'm':  case 'n':  case 'o':  case 'q':  case 'r':  case 's':
    case 't':  case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':  case 'A':  case 'B':  case 'C':  case 'D':  case 'F':
    case 'G':  case 'H':  case 'I':  case 'J':  case 'K':  case 'M':
    case 'N':  case 'O':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':  case 'Z':
    letter:
    /* We're in the middle of either a name or a pre-processing number.  */
      return (is_idchar(c2) || c2 == '.');

    case '<':  case '>':  case '!':  case '%':  case '#':  case ':':
    case '^':  case '&':  case '|':  case '*':  case '/':  case '=':
      return (c2 == c1 || c2 == '=');
    }
  return 0;
}

static void
push_macro_expansion (pfile, xbuf, len, hp)
     cpp_reader *pfile;
     const U_CHAR *xbuf;
     int len;
     HASHNODE *hp;
{
  cpp_buffer *mbuf;
  int advance_cur = 0;

  /* The first chars of the expansion should be a "\r " added by
     collect_expansion.  This is to prevent accidental token-pasting
     between the text preceding the macro invocation, and the macro
     expansion text.

     We would like to avoid adding unneeded spaces (for the sake of
     tools that use cpp, such as imake).  In some common cases we can
     tell that it is safe to omit the space.  */

  if (xbuf[0] == '\r' && xbuf[1] == ' '
      && !unsafe_chars (pfile, EOF, xbuf[2]))
    advance_cur = 1;

  /* Likewise, avoid the extra space at the end of the macro expansion
     if this is safe.  We can do a better job here since we can know
     what the next char will be.  */
  if (len >= 3 && xbuf[len-2] == '\r' && xbuf[len-1] == ' '
      && !unsafe_chars (pfile, xbuf[len-3], CPP_BUF_PEEK (CPP_BUFFER (pfile))))
    len -= 2;

  /* If the total expansion is "\r \r ", we must not trim both escapes.  */
  if (len == 2 && advance_cur)
    advance_cur = 0;

  mbuf = cpp_push_buffer (pfile, xbuf, len);
  if (mbuf == NULL)
    return;
  if (advance_cur)
    mbuf->cur += 2;
  mbuf->cleanup = macro_cleanup;
  mbuf->macro = hp;
  mbuf->has_escapes = 1;

  /* In C89, a macro cannot be expanded recursively.  Traditional C
     permits it, but any use in an object-like macro must lead to
     infinite recursion, so always follow C89 in object-like macros.
     Likewise, in a function-like macro it must cause infinite
     recursion unless we are actually doing something with the
     arguments.

     Even that criterion is too weak.  The only example known where
     macro recursion isn't infinite is:
	#define bar(x,y) foo(x(y, 0))
	bar(bar, baz)
     which expands to foo(bar(baz, 0)) in C89 and
     foo(foo(baz(0, 0)) in K+R.  This looks pathological to me.
     If someone has a real-world example I would love to see it.  */
  if (hp->type != T_FMACRO
      || hp->value.defn->nargs == 0
      || hp->value.defn->pattern == 0
      || !CPP_TRADITIONAL (pfile))
    hp->disabled = 1;
}

/* Return zero if two DEFINITIONs are isomorphic.  */

static int
compare_defs (pfile, d1, d2)
     cpp_reader *pfile;
     DEFINITION *d1, *d2;
{
  struct reflist *a1, *a2;

  if (d1->nargs != d2->nargs)
    return 1;
  if (strcmp (d1->expansion, d2->expansion))
    return 1;
  if (CPP_PEDANTIC (pfile)
      && d1->argnames && d2->argnames)
    {
      U_CHAR *arg1 = d1->argnames;
      U_CHAR *arg2 = d2->argnames;
      size_t len;
      int i = d1->nargs;
      while (i--)
	{
	  len = strlen (arg1) + 1;
	  if (strcmp (arg1, arg2))
	    return 1;
	  arg1 += len;
	  arg2 += len;
	}
    }
  for (a1 = d1->pattern, a2 = d2->pattern; a1 && a2;
       a1 = a1->next, a2 = a2->next)
    {
      if (a1->nchars != a2->nchars
	  || a1->argno != a2->argno
	  || a1->stringify != a2->stringify
	  || a1->raw_before != a2->raw_before
	  || a1->raw_after != a2->raw_after)
	return 1;
    }
  if (a1 != a2)
    return 1;

  return 0;
}

/* Dump the definition of macro MACRO on stdout.  The format is suitable
   to be read back in again. */

void
_cpp_dump_definition (pfile, hp)
     cpp_reader *pfile;
     HASHNODE *hp;
{
  CPP_RESERVE (pfile, hp->length + sizeof "#define ");
  CPP_PUTS_Q (pfile, "#define ", sizeof "#define " - 1);
  CPP_PUTS_Q (pfile, hp->name, hp->length);

  if (hp->type == T_EMPTY)
    /* do nothing */;
  else if (hp->type == T_FMACRO)
    dump_DEFINITION (pfile, hp->value.defn);
  else
    {
      CPP_PUTC_Q (pfile, ' ');

      if (hp->type == T_IDENTITY)
	CPP_PUTS (pfile, hp->name, hp->length);
      else if (hp->type == T_MCONST)
	CPP_PUTS (pfile, hp->value.cpval, strlen (hp->value.cpval));
      else if (hp->type == T_MACRO)
	{
	  /* The first and last two characters of a macro expansion are
	     always "\r "; this needs to be trimmed out.
	     So we need length-4 chars of space, plus one for the NUL.  */
	  CPP_RESERVE (pfile, hp->value.defn->length - 4 + 1);
	  CPP_PUTS_Q (pfile, hp->value.defn->expansion + 2,
		      hp->value.defn->length - 4);
	}
      else
	cpp_ice (pfile, "invalid hash type %d in dump_definition", hp->type);
    }
  if (CPP_BUFFER (pfile) == 0 || ! pfile->done_initializing)
    CPP_PUTC (pfile, '\n');
}

static void
dump_DEFINITION (pfile, defn)
     cpp_reader *pfile;
     DEFINITION *defn;
{
  struct reflist *r;
  unsigned char **argv = (unsigned char **) alloca (defn->nargs *
						    sizeof(char *));
  int *argl = (int *) alloca (defn->nargs * sizeof(int));
  unsigned char *x;
  int i;

  /* First extract the argument list. */
  x = defn->argnames;
  for (i = 0; i < defn->nargs; i++)
    {
      argv[i] = x;
      argl[i] = strlen (x);
      x += argl[i] + 1;
    }
      
  /* Now print out the argument list. */
  CPP_PUTC_Q (pfile, '(');
  for (i = 0; i < defn->nargs; i++)
    {
      CPP_RESERVE (pfile, argl[i] + 2);
      if (!(i == defn->nargs-1 && defn->rest_args
	    && !strcmp (argv[i], "__VA_ARGS__")))
	CPP_PUTS_Q (pfile, argv[i], argl[i]);
      if (i < defn->nargs-1)
	CPP_PUTS_Q (pfile, ", ", 2);
    }
  if (defn->rest_args)
    CPP_PUTS (pfile, "...", 3);
  CPP_PUTS (pfile, ") ", 2);

  /* Now the definition. */
  x = defn->expansion;
  for (r = defn->pattern; r; r = r->next)
    {
      i = r->nchars;
      if (*x == '\r') x += 2, i -= 2;
      /* i chars for macro text, plus the length of the macro
	 argument name, plus one for a stringify marker, plus two for
	 each concatenation marker. */
      CPP_RESERVE (pfile,
		   i + argl[r->argno] + r->stringify
		   + (r->raw_before + r->raw_after) * 2);

      if (i > 0) CPP_PUTS_Q (pfile, x, i);
      if (r->raw_before)
	CPP_PUTS_Q (pfile, "##", 2);
      if (r->stringify)
	CPP_PUTC_Q (pfile, '#');
      CPP_PUTS_Q (pfile, argv[r->argno], argl[r->argno]);
      if (r->raw_after && !(r->next && r->next->nchars == 0
			    && r->next->raw_before))
	CPP_PUTS_Q (pfile, "##", 2);

      x += i;
    }

  i = defn->length - (x - defn->expansion) - 2;
  if (*x == '\r') x += 2, i -= 2;
  if (i > 0) CPP_PUTS (pfile, x, i);
}

/* Dump out the hash table.  */
static int
dump_hash_helper (h, p)
     void **h;
     void *p;
{
  HASHNODE *hp = (HASHNODE *)*h;
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
