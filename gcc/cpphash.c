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
#include "version.h"
#undef abort

static unsigned int hashf	  PARAMS ((const U_CHAR *, int));
static int comp_def_part	 PARAMS ((int, U_CHAR *, int, U_CHAR *,
					  int, int));
static void push_macro_expansion PARAMS ((cpp_reader *,
					  U_CHAR *, int, HASHNODE *));
static int unsafe_chars		 PARAMS ((cpp_reader *, int, int));
static int macro_cleanup	 PARAMS ((cpp_buffer *, cpp_reader *));
static enum cpp_token macarg	 PARAMS ((cpp_reader *, int));
static struct tm *timestamp	 PARAMS ((cpp_reader *));
static void special_symbol	 PARAMS ((HASHNODE *, cpp_reader *));

#define CPP_IS_MACRO_BUFFER(PBUF) ((PBUF)->data != NULL)
#define FORWARD(N) CPP_FORWARD (CPP_BUFFER (pfile), (N))
#define PEEKC() CPP_BUF_PEEK (CPP_BUFFER (pfile))

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
  U_CHAR *name;
  int len;
  char rest_arg;
};

struct arglist
{
  U_CHAR *namebuf;
  struct arg *argv;
  int argc;
};


static DEFINITION *collect_expansion PARAMS ((cpp_reader *, struct arglist *));
static struct arglist *collect_formal_parameters PARAMS ((cpp_reader *));

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


/* Calculate hash function on a string.  */

static unsigned int
hashf (s, len)
     register const U_CHAR *s;
     register int len;
{
  unsigned int n = len;
  unsigned int r = 0;

  do
    r = r * 67 + (*s++ - 113);
  while (--n);
  return r + len;
}

/* Find the most recent hash node for name "name" (ending with first
   non-identifier char) installed by cpp_install

   If LEN is >= 0, it is the length of the name.
   Otherwise, compute the length by scanning the entire name.  */

HASHNODE *
_cpp_lookup (pfile, name, len)
     cpp_reader *pfile;
     const U_CHAR *name;
     int len;
{
  register const U_CHAR *bp;
  register HASHNODE *bucket;
  register unsigned int hash;

  if (len < 0)
    {
      for (bp = name; is_idchar (*bp); bp++);
      len = bp - name;
    }

  hash = hashf (name, len) % HASHSIZE;

  bucket = pfile->hashtab[hash];
  while (bucket)
    {
      if (bucket->length == len && strncmp (bucket->name, name, len) == 0)
	return bucket;
      bucket = bucket->next;
    }
  return (HASHNODE *) 0;
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
  if (d->nargs >= 0)
    free (d->argnames);
  free (d);
}

/*
 * Delete a hash node.  Some weirdness to free junk from macros.
 * More such weirdness will have to be added if you define more hash
 * types that need it.
 */

void
_cpp_delete_macro (hp)
     HASHNODE *hp;
{
  if (hp->prev != NULL)
    hp->prev->next = hp->next;
  if (hp->next != NULL)
    hp->next->prev = hp->prev;

  /* make sure that the bucket chain header that
     the deleted guy was on points to the right thing afterwards.  */
  if (hp == *hp->bucket_hdr)
    *hp->bucket_hdr = hp->next;

  if (hp->type == T_MACRO)
    _cpp_free_definition (hp->value.defn);

  free (hp);
}

/* Install a name in the main hash table, even if it is already there.
   Name stops with first non alphanumeric, except leading '#'.
   Caller must check against redefinition if that is desired.
   delete_macro () removes things installed by cpp_install () in fifo order.
   this is important because of the `defined' special symbol used
   in #if, and also if pushdef/popdef directives are ever implemented.

   If LEN is >= 0, it is the length of the name.
   Otherwise, compute the length by scanning the entire name.

   If HASH is >= 0, it is the precomputed hash code.
   Otherwise, compute the hash code.  */

HASHNODE *
_cpp_install (pfile, name, len, type, value)
     cpp_reader *pfile;
     const U_CHAR *name;
     int len;
     enum node_type type;
     const char *value;
{
  register HASHNODE *hp;
  register int i, bucket;
  register const U_CHAR *p;
  unsigned int hash;

  if (len < 0)
    {
      p = name;
      while (is_idchar(*p))
	p++;
      len = p - name;
    }

  hash = hashf (name, len) % HASHSIZE;

  i = sizeof (HASHNODE) + len + 1;
  hp = (HASHNODE *) xmalloc (i);
  bucket = hash;
  hp->bucket_hdr = &pfile->hashtab[bucket];
  hp->next = pfile->hashtab[bucket];
  pfile->hashtab[bucket] = hp;
  hp->prev = NULL;
  if (hp->next != NULL)
    hp->next->prev = hp;
  hp->type = type;
  hp->length = len;
  hp->value.cpval = value;
  hp->name = ((U_CHAR *) hp) + sizeof (HASHNODE);
  bcopy (name, hp->name, len);
  hp->name[len] = 0;
  return hp;
}

static int
macro_cleanup (pbuf, pfile)
     cpp_buffer *pbuf;
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  HASHNODE *macro = (HASHNODE *) pbuf->data;
  if (macro->type == T_DISABLED)
    macro->type = T_MACRO;
  if (macro->type != T_MACRO || pbuf->buf != macro->value.defn->expansion)
    free (pbuf->buf);
  return 0;
}


/* Read a replacement list for a macro, and build the DEFINITION
   structure.  ARGLIST specifies the formal parameters to look for in
   the text of the definition.  If ARGLIST is null, this is an
   object-like macro; if it points to an empty arglist, this is a
   function-like macro with no arguments.

   A good half of this is devoted to supporting -traditional.
   Kill me now.  */

static DEFINITION *
collect_expansion (pfile, arglist)
     cpp_reader *pfile;
     struct arglist *arglist;
{
  DEFINITION *defn;
  struct reflist *pat = 0, *endpat = 0;
  enum cpp_token token;
  long start, here, last;
  int i;
  int argc;
  size_t len;
  struct arg *argv;
  U_CHAR *tok, *exp;
  enum { START = 0, NORM, ARG, STRIZE, PASTE } last_token = START;

  if (arglist)
    {
      argv = arglist->argv;
      argc = arglist->argc;
    }
  else
    {
      argv = 0;
      argc = 0;
    }

  last = start = CPP_WRITTEN (pfile);
  last -= 2;  /* two extra chars for the leading escape */
  for (;;)
    {
      /* We use cpp_get_token because get_directive_token would
	 discard whitespace and we can't cope with that yet.  Macro
	 expansion is off, so we are guaranteed not to see POP or EOF.  */

      while (PEEKC () == '\r')
	{
	  FORWARD (1);
	  CPP_BUMP_LINE (pfile);
	}
      if (PEEKC () == '\n')
	goto done;
      here = CPP_WRITTEN (pfile);
      token = cpp_get_token (pfile);
      tok = pfile->token_buffer + here;
      switch (token)
	{
	case CPP_POP:
	case CPP_EOF:
	case CPP_VSPACE:
	  cpp_ice (pfile, "EOF or VSPACE in collect_expansion");
	  goto done;

	case CPP_HSPACE:
	  if (last_token == STRIZE || last_token == PASTE
	      || last_token == START)
	    CPP_SET_WRITTEN (pfile, here);
	  break;

	case CPP_STRINGIZE:
	  if (last_token == PASTE)
	    /* Not really a stringifier.  */
	    goto norm;
	  last_token = STRIZE;
	  CPP_SET_WRITTEN (pfile, here);  /* delete from replacement text */
	  break;

	case CPP_TOKPASTE:
	  /* If the last token was an argument, discard this token and
	     any hspace between it and the argument's position.  Then
	     mark the arg raw_after.  */
	  if (last_token == ARG)
	    {
	      endpat->raw_after = 1;
	      last_token = PASTE;
	      CPP_SET_WRITTEN (pfile, last);
	      break;
	    }
	  else if (last_token == PASTE)
	    /* ## ## - the second ## is ordinary.  */
	    goto norm;
	  else if (last_token == START)
	    cpp_error (pfile, "`##' at start of macro definition");
	  
	  /* Discard the token and any hspace before it.  */
	  while (is_hspace (pfile->token_buffer[here-1]))
	    here--;
	  CPP_SET_WRITTEN (pfile, here);

	  if (last_token == STRIZE)
	    /* Oops - that wasn't a stringify operator.  */
	    CPP_PUTC (pfile, '#');
	  last_token = PASTE;
	  break;

	case CPP_COMMENT:
	  /* We must be in -traditional mode.  Pretend this was a
	     token paste, but only if there was no leading or
	     trailing space.  */
	  CPP_SET_WRITTEN (pfile, here);
	  if (is_hspace (pfile->token_buffer[here-1]))
	    break;
	  if (is_hspace (PEEKC ()))
	    break;
	  if (last_token == ARG)
	    endpat->raw_after = 1;
	  last_token = PASTE;
	  break;

	case CPP_STRING:
	case CPP_CHAR:
	  if (last_token == STRIZE)
	    cpp_error (pfile, "`#' is not followed by a macro argument name");

	  if (CPP_TRADITIONAL (pfile) || CPP_OPTIONS (pfile)->warn_stringify)
	    goto maybe_trad_stringify;
	  else
	    goto norm;
	  
	case CPP_NAME:
	  for (i = 0; i < argc; i++)
	    if (!strncmp (tok, argv[i].name, argv[i].len)
		&& ! is_idchar (tok[argv[i].len]))
	      goto addref;

	  /* fall through */
	default:
	norm:
	  if (last_token == STRIZE)
	    cpp_error (pfile, "`#' is not followed by a macro argument name");
	  last_token = NORM;
	  break;
	}
      continue;

    addref:
      {
	struct reflist *tpat;
	
	/* Make a pat node for this arg and add it to the pat list */
	tpat = (struct reflist *) xmalloc (sizeof (struct reflist));
	tpat->next = NULL;
	tpat->raw_before = (last_token == PASTE);
	tpat->raw_after = 0;
	tpat->stringify = (last_token == STRIZE);
	tpat->rest_args = argv[i].rest_arg;
	tpat->argno = i;
	tpat->nchars = here - last;

	if (endpat == NULL)
	  pat = tpat;
	else
	  endpat->next = tpat;
	endpat = tpat;
	last = here;
      }
      CPP_SET_WRITTEN (pfile, here);  /* discard arg name */
      last_token = ARG;
      continue;

    maybe_trad_stringify:
      last_token = NORM;
      {
	U_CHAR *base, *p, *limit;
	struct reflist *tpat;

	base = p = pfile->token_buffer + here;
	limit = CPP_PWRITTEN (pfile);

	while (++p < limit)
	  {
	    if (is_idstart (*p))
	      continue;
	    for (i = 0; i < argc; i++)
	      if (!strncmp (tok, argv[i].name, argv[i].len)
		  && ! is_idchar (tok[argv[i].len]))
		goto mts_addref;
	    continue;

	  mts_addref:
	    if (!CPP_TRADITIONAL (pfile))
	      {
		/* Must have got here because of -Wtraditional.  */
		cpp_warning (pfile,
	     "macro argument `%.*s' would be stringified with -traditional",
			     (int) argv[i].len, argv[i].name);
		continue;
	      }
	    if (CPP_OPTIONS (pfile)->warn_stringify)
	      cpp_warning (pfile, "macro argument `%.*s' is stringified",
			     (int) argv[i].len, argv[i].name);

	    /* Remove the argument from the string.  */
	    memmove (p, p + argv[i].len, limit - (p + argv[i].len));
	    limit -= argv[i].len;
	
	    /* Make a pat node for this arg and add it to the pat list */
	    tpat = (struct reflist *) xmalloc (sizeof (struct reflist));
	    tpat->next = NULL;

	    /* Don't attempt to paste this with anything.  */
	    tpat->raw_before = 0;
	    tpat->raw_after = 0;
	    tpat->stringify = 1;
	    tpat->rest_args = argv[i].rest_arg;
	    tpat->argno = i;
	    tpat->nchars = (p - base) + here - last;

	    if (endpat == NULL)
	      pat = tpat;
	    else
	      endpat->next = tpat;
	    endpat = tpat;
	    last = (p - base) + here;
	  }
	CPP_ADJUST_WRITTEN (pfile, CPP_PWRITTEN (pfile) - limit);
      }
    }
 done:

  if (last_token == STRIZE)
    cpp_error (pfile, "`#' is not followed by a macro argument name");
  else if (last_token == PASTE)
    cpp_error (pfile, "`##' at end of macro definition");

  if (last_token == START)
    {
      /* Empty macro definition.  */
      exp = xstrdup ("\r \r ");
      len = 1;
    }
  else
    {
      /* Trim trailing white space from definition.  */
      here = CPP_WRITTEN (pfile);
      while (here > last && is_hspace (pfile->token_buffer [here-1]))
	here--;
      CPP_SET_WRITTEN (pfile, here);
  
      CPP_NUL_TERMINATE (pfile);
      len = CPP_WRITTEN (pfile) - start + 1;
      exp = xmalloc (len + 4); /* space for no-concat markers at either end */
      exp[0] = '\r';
      exp[1] = ' ';
      exp[len + 1] = '\r';
      exp[len + 2] = ' ';
      exp[len + 3] = '\0';
      memcpy (&exp[2], pfile->token_buffer + start, len - 1);
    }

  CPP_SET_WRITTEN (pfile, start);

  defn = (DEFINITION *) xmalloc (sizeof (DEFINITION));
  defn->length = len + 3;
  defn->expansion = exp;
  defn->pattern = pat;
  defn->rest_args = 0;
  if (arglist)
    {
      defn->nargs = argc;
      defn->argnames = arglist->namebuf;
      if (argv)
	{
	  defn->rest_args = argv[argc - 1].rest_arg;
	  free (argv);
	}
      free (arglist);
    }
  else
    {
      defn->nargs = -1;
      defn->argnames = 0;
      defn->rest_args = 0;
    }
  return defn;
}

static struct arglist *
collect_formal_parameters (pfile)
     cpp_reader *pfile;
{
  struct arglist *result = 0;
  struct arg *argv = 0;
  U_CHAR *namebuf = (U_CHAR *) xstrdup ("");

  U_CHAR *name, *tok;
  size_t argslen = 1;
  int len;
  int argc = 0;
  int i;
  enum cpp_token token;
  long old_written;

  old_written = CPP_WRITTEN (pfile);
  token = get_directive_token (pfile);
  if (token != CPP_LPAREN)
    {
      cpp_ice (pfile, "first token = %d not %d in collect_formal_parameters",
	       token, CPP_LPAREN);
      goto invalid;
    }

  argv = (struct arg *) xmalloc (sizeof (struct arg));
  argv[argc].len = 0;
  argv[argc].rest_arg = 0;
  for (;;)
    {
      CPP_SET_WRITTEN (pfile, old_written);
      token = get_directive_token (pfile);
      switch (token)
	{
	case CPP_NAME:
	  tok = pfile->token_buffer + old_written;
	  len = CPP_PWRITTEN (pfile) - tok;
	  if (namebuf
	      && (name = strstr (namebuf, tok))
	      && name[len] == ','
	      && (name == namebuf || name[-1] == ','))
	    {
	      cpp_error (pfile, "duplicate macro argument name `%s'", tok);
	      continue;
	    }
	  if (CPP_PEDANTIC (pfile) && CPP_OPTIONS (pfile)->c99
	      && strncmp (tok, "__VA_ARGS__", sizeof "__VA_ARGS__" - 1))
	    cpp_pedwarn (pfile,
	"C99 does not permit use of `__VA_ARGS__' as a macro argument name");
	  namebuf = xrealloc (namebuf, argslen + len + 1);
	  name = &namebuf[argslen - 1];
	  argslen += len + 1;

	  memcpy (name, tok, len);
	  name[len] = ',';
	  name[len+1] = '\0';
	  argv[argc].len = len;
	  argv[argc].rest_arg = 0;
	  break;

	case CPP_COMMA:
	  argc++;
	  argv = xrealloc (argv, (argc + 1)*sizeof(struct arg));
	  argv[argc].len = 0;
	  break;

	case CPP_RPAREN:
	  goto done;

	case CPP_3DOTS:
	  goto rest_arg;

	case CPP_VSPACE:
	  cpp_error (pfile, "missing right paren in macro argument list");
	  goto invalid;

	default:
	  cpp_error (pfile, "syntax error in #define");
	  goto invalid;
	}
    }

 rest_arg:
  /* There are two possible styles for a vararg macro:
     the C99 way:  #define foo(a, ...) a, __VA_ARGS__
     the gnu way:  #define foo(a, b...) a, b
     The C99 way can be considered a special case of the gnu way.
     There are also some constraints to worry about, but we'll handle
     those elsewhere.  */
  if (argv[argc].len == 0)
    {
      if (CPP_PEDANTIC (pfile) && ! CPP_OPTIONS (pfile)->c99)
	cpp_pedwarn (pfile, "C89 does not permit varargs macros");

      len = sizeof "__VA_ARGS__" - 1;
      namebuf = xrealloc (namebuf, argslen + len + 1);
      name = &namebuf[argslen - 1];
      argslen += len;
      memcpy (name, "__VA_ARGS__", len);
      argv[argc].len = len;
    }
  else
    if (CPP_PEDANTIC (pfile))
      cpp_pedwarn (pfile, "ISO C does not permit named varargs macros");

  argv[argc].rest_arg = 1;
  
  token = get_directive_token (pfile);
  if (token != CPP_RPAREN)
    {
      cpp_error (pfile, "another parameter follows `...'");
      goto invalid;
    }

 done:
  /* Go through argv and fix up the pointers.  */
  len = 0;
  for (i = 0; i <= argc; i++)
    {
      argv[i].name = namebuf + len;
      len += argv[i].len + 1;
      namebuf[len - 1] = '\0';
    }

  CPP_SET_WRITTEN (pfile, old_written);

  result = (struct arglist *) xmalloc (sizeof (struct arglist));
  if (namebuf[0] != '\0')
    {
      result->namebuf = namebuf;
      result->argc = argc + 1;
      result->argv = argv;
    }
  else
    {
      free (namebuf);
      result->namebuf = 0;
      result->argc = 0;
      result->argv = 0;
    }

  return result;

 invalid:
  if (argv)
    free (argv);
  if (namebuf)
    free (namebuf);
  return 0;
}

/* Create a DEFINITION node for a macro.  The reader's point is just
   after the macro name.  If FUNLIKE is true, this is a function-like
   macro.  */

DEFINITION *
_cpp_create_definition (pfile, funlike)
     cpp_reader *pfile;
     int funlike;
{
  struct arglist *args = 0;
  long line, col;
  const char *file;
  DEFINITION *defn;

  cpp_buf_line_and_col (CPP_BUFFER (pfile), &line, &col);
  file = CPP_BUFFER (pfile)->nominal_fname;

  pfile->no_macro_expand++;
  pfile->parsing_define_directive++;
  CPP_OPTIONS (pfile)->discard_comments++;
  CPP_OPTIONS (pfile)->no_line_commands++;
  
  if (funlike)
    {
      args = collect_formal_parameters (pfile);
      if (args == 0)
	goto err;
    }

  defn = collect_expansion (pfile, args);
  if (defn == 0)
    goto err;

  defn->line = line;
  defn->file = file;
  defn->col  = col;

  pfile->no_macro_expand--;
  pfile->parsing_define_directive--;
  CPP_OPTIONS (pfile)->discard_comments--;
  CPP_OPTIONS (pfile)->no_line_commands--;
  return defn;

 err:
  pfile->no_macro_expand--;
  pfile->parsing_define_directive--;
  CPP_OPTIONS (pfile)->discard_comments--;
  CPP_OPTIONS (pfile)->no_line_commands--;
  return 0;
}

/*
 * Parse a macro argument and append the info on PFILE's token_buffer.
 * REST_ARGS means to absorb the rest of the args.
 * Return nonzero to indicate a syntax error.
 */

static enum cpp_token
macarg (pfile, rest_args)
     cpp_reader *pfile;
     int rest_args;
{
  int paren = 0;
  enum cpp_token token;

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
	case CPP_LPAREN:
	  paren++;
	  break;
	case CPP_RPAREN:
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


static struct tm *
timestamp (pfile)
     cpp_reader *pfile;
{
  if (!pfile->timebuf)
    {
      time_t t = time ((time_t *) 0);
      pfile->timebuf = localtime (&t);
    }
  return pfile->timebuf;
}

static const char * const monthnames[] =
{
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
};

/*
 * expand things like __FILE__.  Place the expansion into the output
 * buffer *without* rescanning.
 */

static void
special_symbol (hp, pfile)
     HASHNODE *hp;
     cpp_reader *pfile;
{
  const char *buf;
  int len;
  cpp_buffer *ip;

  switch (hp->type)
    {
    case T_FILE:
    case T_BASE_FILE:
      {
	ip = cpp_file_buffer (pfile);
	if (hp->type == T_BASE_FILE)
	  {
	    while (CPP_PREV_BUFFER (ip) != NULL)
	      ip = CPP_PREV_BUFFER (ip);
	  }

	buf = ip->nominal_fname;

	if (!buf)
	  buf = "";
	CPP_RESERVE (pfile, 3 + 4 * strlen (buf));
	quote_string (pfile, buf);
	return;
      }

    case T_INCLUDE_LEVEL:
      {
	int true_indepth = 1;
	ip = cpp_file_buffer (pfile);
	while ((ip = CPP_PREV_BUFFER (ip)) != NULL)
	  true_indepth++;

	CPP_RESERVE (pfile, 10);
	sprintf (CPP_PWRITTEN (pfile), "%d", true_indepth);
	CPP_ADJUST_WRITTEN (pfile, strlen (CPP_PWRITTEN (pfile)));
	return;
      }

    case T_VERSION:
      len = strlen (version_string);
      CPP_RESERVE (pfile, 3 + len);
      CPP_PUTC_Q (pfile, '"');
      CPP_PUTS_Q (pfile, version_string, len);
      CPP_PUTC_Q (pfile, '"');
      CPP_NUL_TERMINATE_Q (pfile);
      return;

    case T_CONST:
      buf = hp->value.cpval;
      if (!buf)
	return;
      if (*buf == '\0')
	buf = "\r ";

      len = strlen (buf);
      CPP_RESERVE (pfile, len + 1);
      CPP_PUTS_Q (pfile, buf, len);
      CPP_NUL_TERMINATE_Q (pfile);
      return;

    case T_STDC:
      CPP_RESERVE (pfile, 2);
#ifdef STDC_0_IN_SYSTEM_HEADERS
      ip = cpp_file_buffer (pfile);
      if (ip->system_header_p
	  && !cpp_defined (pfile, (const U_CHAR *) "__STRICT_ANSI__", 15))
	CPP_PUTC_Q (pfile, '0');
      else
#endif
	CPP_PUTC_Q (pfile, '1');
      CPP_NUL_TERMINATE_Q (pfile);
      return;

    case T_SPECLINE:
      {
	long line;
	cpp_buf_line_and_col (cpp_file_buffer (pfile), &line, NULL);

	CPP_RESERVE (pfile, 10);
	sprintf (CPP_PWRITTEN (pfile), "%ld", line);
	CPP_ADJUST_WRITTEN (pfile, strlen (CPP_PWRITTEN (pfile)));
	return;
      }

    case T_DATE:
    case T_TIME:
      {
	struct tm *timebuf;

	CPP_RESERVE (pfile, 20);
	timebuf = timestamp (pfile);
	if (hp->type == T_DATE)
	  sprintf (CPP_PWRITTEN (pfile), "\"%s %2d %4d\"",
		   monthnames[timebuf->tm_mon],
		   timebuf->tm_mday, timebuf->tm_year + 1900);
	else
	  sprintf (CPP_PWRITTEN (pfile), "\"%02d:%02d:%02d\"",
		   timebuf->tm_hour, timebuf->tm_min, timebuf->tm_sec);

	CPP_ADJUST_WRITTEN (pfile, strlen (CPP_PWRITTEN (pfile)));
	return;
      }

    case T_POISON:
      cpp_error (pfile, "attempt to use poisoned `%s'.", hp->name);
      CPP_RESERVE (pfile, 1);
      CPP_PUTC_Q (pfile, '0');
      CPP_NUL_TERMINATE_Q (pfile);
      break;

    default:
      cpp_ice (pfile, "invalid special hash type");
      return;
    }
}

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
  int nargs;
  DEFINITION *defn;
  register U_CHAR *xbuf;
  long start_line, start_column;
  int xbuf_len;
  struct argdata *args = 0;
  long old_written = CPP_WRITTEN (pfile);
  int rest_args, rest_zero = 0;
  register int i;

  cpp_buf_line_and_col (cpp_file_buffer (pfile), &start_line, &start_column);

  /* Check for and handle special symbols. */
  if (hp->type != T_MACRO)
    {
      special_symbol (hp, pfile);
      xbuf_len = CPP_WRITTEN (pfile) - old_written;
      xbuf = (U_CHAR *) xmalloc (xbuf_len + 1);
      CPP_SET_WRITTEN (pfile, old_written);
      memcpy (xbuf, CPP_PWRITTEN (pfile), xbuf_len + 1);
      push_macro_expansion (pfile, xbuf, xbuf_len, hp);
      CPP_BUFFER (pfile)->has_escapes = 1;
      return;
    }

  defn = hp->value.defn;
  nargs = defn->nargs;
  pfile->output_escapes++;

  if (nargs >= 0)
    {
      enum cpp_token token;

      args = (struct argdata *) alloca ((nargs + 1) * sizeof (struct argdata));

      for (i = 0; i < nargs; i++)
	{
	  args[i].raw = args[i].expanded = 0;
	  args[i].raw_length = 0;
	  args[i].expand_length = args[i].stringified_length = -1;
	}

      /* Parse all the macro args that are supplied.  I counts them.
         The first NARGS args are stored in ARGS.
         The rest are discarded.  If rest_args is set then we assume
         macarg absorbed the rest of the args.  */
      i = 0;
      rest_args = 0;

      /* Skip over the opening parenthesis.  */
      CPP_OPTIONS (pfile)->discard_comments++;
      CPP_OPTIONS (pfile)->no_line_commands++;
      pfile->no_macro_expand++;
      pfile->no_directives++;

      token = cpp_get_non_space_token (pfile);
      if (token != CPP_LPAREN)
	cpp_ice (pfile, "macroexpand: unexpected token %d (wanted LPAREN)",
		 token);
      CPP_ADJUST_WRITTEN (pfile, -1);

      token = CPP_EOF;
      do
	{
	  if (rest_args)
	    continue;
	  if (i < nargs || (nargs == 0 && i == 0))
	    {
	      /* if we are working on last arg which absorbs rest of args... */
	      if (i == nargs - 1 && defn->rest_args)
		rest_args = 1;
	      args[i].raw = CPP_WRITTEN (pfile);
	      token = macarg (pfile, rest_args);
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
      CPP_OPTIONS (pfile)->discard_comments--;
      CPP_OPTIONS (pfile)->no_line_commands--;
      pfile->no_macro_expand--;
      pfile->no_directives--;
      if (token != CPP_RPAREN)
	return;

      /* If we got one arg but it was just whitespace, call that 0 args.  */
      if (i == 1)
	{
	  register U_CHAR *bp = ARG_BASE + args[0].raw;
	  register U_CHAR *lim = bp + args[0].raw_length;
	  /* cpp.texi says for foo ( ) we provide one argument.
	     However, if foo wants just 0 arguments, treat this as 0.  */
	  if (nargs == 0)
	    while (bp != lim && is_space(*bp))
	      bp++;
	  if (bp == lim)
	    i = 0;
	}

      /* Don't output an error message if we have already output one for
         a parse error above.  */
      rest_zero = 0;
      if (nargs == 0 && i > 0)
	{
	  cpp_error (pfile, "arguments given to macro `%s'", hp->name);
	}
      else if (i < nargs)
	{
	  /* traditional C allows foo() if foo wants one argument.  */
	  if (nargs == 1 && i == 0 && CPP_TRADITIONAL (pfile))
	    ;
	  /* the rest args token is allowed to absorb 0 tokens */
	  else if (i == nargs - 1 && defn->rest_args)
	    rest_zero = 1;
	  else if (i == 0)
	    cpp_error (pfile, "macro `%s' used without args", hp->name);
	  else if (i == 1)
	    cpp_error (pfile, "macro `%s' used with just one arg", hp->name);
	  else
	    cpp_error (pfile, "macro `%s' used with only %d args",
		       hp->name, i);
	}
      else if (i > nargs)
	{
	  cpp_error (pfile,
		     "macro `%s' used with too many (%d) args", hp->name, i);
	}
    }

  /* If macro wants zero args, we parsed the arglist for checking only.
     Read directly from the macro definition.  */
  if (nargs <= 0)
    {
      xbuf = defn->expansion;
      xbuf_len = defn->length;
    }
  else
    {
      register U_CHAR *exp = defn->expansion;
      register int offset;	/* offset in expansion,
				   copied a piece at a time */
      register int totlen;	/* total amount of exp buffer filled so far */

      register struct reflist *ap, *last_ap;

      /* Macro really takes args.  Compute the expansion of this call.  */

      /* Compute length in characters of the macro's expansion.
         Also count number of times each arg is used.  */
      xbuf_len = defn->length;
      for (ap = defn->pattern; ap != NULL; ap = ap->next)
	{
	  if (ap->stringify)
	    {
	      register struct argdata *arg = &args[ap->argno];
	      /* Stringify if it hasn't already been */
	      if (arg->stringified_length < 0)
		{
		  int arglen = arg->raw_length;
		  int escaped = 0;
		  int in_string = 0;
		  int c;
		  /* Initially need_space is -1.  Otherwise, 1 means the
		     previous character was a space, but we suppressed it;
		     0 means the previous character was a non-space.  */
		  int need_space = -1;
		  i = 0;
		  arg->stringified = CPP_WRITTEN (pfile);
		  if (!CPP_TRADITIONAL (pfile))
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
			  /* Internal sequences of whitespace are
			     replaced by one space except within
			     a string or char token. */
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
			  sprintf ((char *) CPP_PWRITTEN (pfile), "\\%03o",
				   (unsigned int) c);
			  CPP_ADJUST_WRITTEN (pfile, 4);
			}
		    }
		  if (!CPP_TRADITIONAL (pfile))
		    CPP_PUTC (pfile, '\"');	/* insert ending quote */
		  arg->stringified_length
		    = CPP_WRITTEN (pfile) - arg->stringified;
		}
	      xbuf_len += args[ap->argno].stringified_length;
	    }
	  else if (ap->raw_before || ap->raw_after || CPP_TRADITIONAL (pfile))
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
		  cpp_expand_to_buffer (pfile,
					ARG_BASE + args[ap->argno].raw,
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

      /* Generate in XBUF the complete expansion
         with arguments substituted in.
         TOTLEN is the total size generated so far.
         OFFSET is the index in the definition
         of where we are copying from.  */
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
	  if (rest_zero && totlen > count_before
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
	  else if (ap->raw_before || ap->raw_after || CPP_TRADITIONAL (pfile))
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
		  && !CPP_TRADITIONAL (pfile)
		  && unsafe_chars (pfile, xbuf[totlen - 1], expanded[0]))
		{
		  xbuf[totlen++] = '\r';
		  xbuf[totlen++] = ' ';
		}

	      memcpy (xbuf + totlen, expanded, arg->expand_length);
	      totlen += arg->expand_length;

	      if (!ap->raw_after && totlen > 0 && offset < defn->length
		  && !CPP_TRADITIONAL (pfile)
		  && unsafe_chars (pfile, xbuf[totlen - 1], exp[offset]))
		{
		  xbuf[totlen++] = '\r';
		  xbuf[totlen++] = ' ';
		}
	    }

	  if (totlen > xbuf_len)
	    {
	      cpp_ice (pfile, "buffer overrun in macroexpand");
	      return;
	    }
	}

      /* if there is anything left of the definition
         after handling the arg list, copy that in too.  */

      for (i = offset; i < defn->length; i++)
	{
	  /* if we've reached the end of the macro */
	  if (exp[i] == ')')
	    rest_zero = 0;
	  if (!(rest_zero && last_ap != NULL && last_ap->rest_args
		&& last_ap->raw_after))
	    xbuf[totlen++] = exp[i];
	}

      xbuf[totlen] = 0;
      xbuf_len = totlen;

    }

  pfile->output_escapes--;

  /* Now put the expansion on the input stack
     so our caller will commence reading from it.  */
  push_macro_expansion (pfile, xbuf, xbuf_len, hp);
  CPP_BUFFER (pfile)->has_escapes = 1;

  /* Pop the space we've used in the token_buffer for argument expansion.  */
  CPP_SET_WRITTEN (pfile, old_written);

  /* Recursive macro use sometimes works traditionally.
     #define foo(x,y) bar (x (y,0), y)
     foo (foo, baz)  */

  if (!CPP_TRADITIONAL (pfile))
    hp->type = T_DISABLED;
}

/* Return 1 iff a token ending in C1 followed directly by a token C2
   could cause mis-tokenization.  */

static int
unsafe_chars (pfile, c1, c2)
     cpp_reader *pfile;
     int c1, c2;
{
  switch (c1)
    {
    case '+':  case '-':
      if (c2 == c1 || c2 == '=')
	return 1;
      goto letter;

    case 'e':  case 'E':  case 'p':  case 'P':
      if (c2 == '-' || c2 == '+')
	return 1;		/* could extend a pre-processing number */
      goto letter;

    case '$':
      if (CPP_OPTIONS (pfile)->dollars_in_ident)
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
push_macro_expansion (pfile, xbuf, xbuf_len, hp)
     cpp_reader *pfile;
     register U_CHAR *xbuf;
     int xbuf_len;
     HASHNODE *hp;
{
  register cpp_buffer *mbuf = cpp_push_buffer (pfile, xbuf, xbuf_len);
  if (mbuf == NULL)
    return;
  mbuf->cleanup = macro_cleanup;
  mbuf->data = hp;

  /* The first chars of the expansion should be a "\r " added by
     collect_expansion.  This is to prevent accidental token-pasting
     between the text preceding the macro invocation, and the macro
     expansion text.

     We would like to avoid adding unneeded spaces (for the sake of
     tools that use cpp, such as imake).  In some common cases we can
     tell that it is safe to omit the space.

     The character before the macro invocation cannot have been an
     idchar (or else it would have been pasted with the idchars of
     the macro name).  Therefore, if the first non-space character
     of the expansion is an idchar, we do not need the extra space
     to prevent token pasting.

     Also, we don't need the extra space if the first char is '(',
     or some other (less common) characters.  */

  if (xbuf[0] == '\r' && xbuf[1] == ' '
      && (is_idchar(xbuf[2]) || xbuf[2] == '(' || xbuf[2] == '\''
	  || xbuf[2] == '\"'))
    mbuf->cur += 2;

  /* Likewise, avoid the extra space at the end of the macro expansion
     if this is safe.  We can do a better job here since we can know
     what the next char will be.  */
  if (xbuf_len >= 3
      && mbuf->rlimit[-2] == '\r'
      && mbuf->rlimit[-1] == ' ')
    {
      int c1 = mbuf->rlimit[-3];
      int c2 = CPP_BUF_PEEK (CPP_PREV_BUFFER (CPP_BUFFER (pfile)));
      if (c2 == EOF || !unsafe_chars (pfile, c1, c2))
	mbuf->rlimit -= 2;
    }
}

/* Return zero if two DEFINITIONs are isomorphic.  */

int
_cpp_compare_defs (pfile, d1, d2)
     cpp_reader *pfile;
     DEFINITION *d1, *d2;
{
  struct reflist *a1, *a2;
  U_CHAR *p1 = d1->expansion;
  U_CHAR *p2 = d2->expansion;
  int first = 1;

  if (d1->nargs != d2->nargs)
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
	  len = strlen (arg1);
	  if (strcmp (arg1, arg2))
	    return 1;
	  arg1 += len;
	  arg2 += len;
	}
    }
  for (a1 = d1->pattern, a2 = d2->pattern; a1 && a2;
       a1 = a1->next, a2 = a2->next)
    {
      if (!((a1->nchars == a2->nchars && !strncmp (p1, p2, a1->nchars))
	    || !comp_def_part (first, p1, a1->nchars, p2, a2->nchars, 0))
	  || a1->argno != a2->argno
	  || a1->stringify != a2->stringify
	  || a1->raw_before != a2->raw_before
	  || a1->raw_after != a2->raw_after)
	return 1;
      first = 0;
      p1 += a1->nchars;
      p2 += a2->nchars;
    }
  if (a1 != a2)
    return 1;

  return comp_def_part (first, p1, d1->length - (p1 - d1->expansion),
			p2, d2->length - (p2 - d2->expansion), 1);
}

/* Return 1 if two parts of two macro definitions are effectively different.
   One of the parts starts at BEG1 and has LEN1 chars;
   the other has LEN2 chars at BEG2.
   Any sequence of whitespace matches any other sequence of whitespace.
   FIRST means these parts are the first of a macro definition;
    so ignore leading whitespace entirely.
   LAST means these parts are the last of a macro definition;
    so ignore trailing whitespace entirely.  */

static int
comp_def_part (first, beg1, len1, beg2, len2, last)
     int first;
     U_CHAR *beg1, *beg2;
     int len1, len2;
     int last;
{
  register U_CHAR *end1 = beg1 + len1;
  register U_CHAR *end2 = beg2 + len2;
  if (first)
    {
      while (beg1 != end1 && is_space(*beg1))
	beg1++;
      while (beg2 != end2 && is_space(*beg2))
	beg2++;
    }
  if (last)
    {
      while (beg1 != end1 && is_space(end1[-1]))
	end1--;
      while (beg2 != end2 && is_space(end2[-1]))
	end2--;
    }
  while (beg1 != end1 && beg2 != end2)
    {
      if (is_space(*beg1) && is_space(*beg2))
	{
	  while (beg1 != end1 && is_space(*beg1))
	    beg1++;
	  while (beg2 != end2 && is_space(*beg2))
	    beg2++;
	}
      else if (*beg1 == *beg2)
	{
	  beg1++;
	  beg2++;
	}
      else
	break;
    }
  return (beg1 != end1) || (beg2 != end2);
}

/* Dump the definition of macro MACRO on stdout.  The format is suitable
   to be read back in again. */

void
_cpp_dump_definition (pfile, sym, len, defn)
     cpp_reader *pfile;
     const U_CHAR *sym;
     long len;
     DEFINITION *defn;
{
  if (pfile->lineno == 0)
    output_line_command (pfile, same_file);

  CPP_RESERVE (pfile, len + sizeof "#define ");
  CPP_PUTS_Q (pfile, "#define ", sizeof "#define " -1);
  CPP_PUTS_Q (pfile, sym, len);

  if (defn->nargs == -1)
    {
      CPP_PUTC_Q (pfile, ' ');

      /* The first and last two characters of a macro expansion are
	 always "\r "; this needs to be trimmed out.
	 So we need length-4 chars of space, plus one for the NUL.  */
      CPP_RESERVE (pfile, defn->length - 4 + 1);
      CPP_PUTS_Q (pfile, defn->expansion + 2, defn->length - 4);
    }
  else
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

  if (pfile->lineno == 0)
    CPP_PUTC (pfile, '\n');
  CPP_NUL_TERMINATE (pfile);
}
