/* Part of CPP library.  (Macro handling.)
   Copyright (C) 1986, 87, 89, 92-96, 98, 99, 2000
   Free Software Foundation, Inc.
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
#undef abort

static int comp_def_part	 PARAMS ((int, U_CHAR *, int, U_CHAR *,
					  int, int));
static void push_macro_expansion PARAMS ((cpp_reader *,
					  U_CHAR *, int, HASHNODE *));
static int unsafe_chars		 PARAMS ((int, int));
static int macro_cleanup	 PARAMS ((cpp_buffer *, cpp_reader *));
static enum cpp_token macarg	 PARAMS ((cpp_reader *, int));
static struct tm *timestamp	 PARAMS ((cpp_reader *));
static void special_symbol	 PARAMS ((HASHNODE *, cpp_reader *));


#define SKIP_WHITE_SPACE(p) do { while (is_hspace(*p)) p++; } while (0)
#define CPP_IS_MACRO_BUFFER(PBUF) ((PBUF)->data != NULL)
#define FORWARD(N) CPP_FORWARD (CPP_BUFFER (pfile), (N))

extern char *version_string;

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

struct arglist
{
  struct arglist *next;
  U_CHAR *name;
  int length;
  int argno;
  char rest_args;
};

static DEFINITION *collect_expansion PARAMS ((cpp_reader *, U_CHAR *, U_CHAR *,
					      int, struct arglist *));

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


/* Return hash function on name.  must be compatible with the one
   computed a step at a time, elsewhere  */

int
hashf (name, len, hashsize)
     register const U_CHAR *name;
     register int len;
     int hashsize;
{
  register int r = 0;

  while (len--)
    r = HASHSTEP (r, *name++);

  return MAKE_POS (r) % hashsize;
}

/* Find the most recent hash node for name "name" (ending with first
   non-identifier char) installed by cpp_install

   If LEN is >= 0, it is the length of the name.
   Otherwise, compute the length by scanning the entire name.

   If HASH is >= 0, it is the precomputed hash code.
   Otherwise, compute the hash code.  */

HASHNODE *
cpp_lookup (pfile, name, len, hash)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     const U_CHAR *name;
     int len;
     int hash;
{
  register const U_CHAR *bp;
  register HASHNODE *bucket;

  if (len < 0)
    {
      for (bp = name; is_idchar (*bp); bp++);
      len = bp - name;
    }

  if (hash < 0)
    hash = hashf (name, len, HASHSIZE);

  bucket = pfile->hashtab[hash];
  while (bucket)
    {
      if (bucket->length == len && strncmp (bucket->name, name, len) == 0)
	return bucket;
      bucket = bucket->next;
    }
  return (HASHNODE *) 0;
}

/*
 * Delete a hash node.  Some weirdness to free junk from macros.
 * More such weirdness will have to be added if you define more hash
 * types that need it.
 */

/* Note that the DEFINITION of a macro is removed from the hash table
   but its storage is not freed.  This would be a storage leak
   except that it is not reasonable to keep undefining and redefining
   large numbers of macros many times.
   In any case, this is necessary, because a macro can be #undef'd
   in the middle of reading the arguments to a call to it.
   If #undef freed the DEFINITION, that would crash.  */

void
delete_macro (hp)
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
    {
      DEFINITION *d = hp->value.defn;
      struct reflist *ap, *nextap;

      for (ap = d->pattern; ap != NULL; ap = nextap)
	{
	  nextap = ap->next;
	  free (ap);
	}
      if (d->nargs >= 0)
	free (d->args.argnames);
      free (d);
    }

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
cpp_install (pfile, name, len, type, value, hash)
     cpp_reader *pfile;
     const U_CHAR *name;
     int len;
     enum node_type type;
     const char *value;
     int hash;
{
  register HASHNODE *hp;
  register int i, bucket;
  register const U_CHAR *p;

  if (len < 0)
    {
      p = name;
      while (is_idchar(*p))
	p++;
      len = p - name;
    }

  if (hash < 0)
    hash = hashf (name, len, HASHSIZE);

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


/* Read a replacement list for a macro with parameters.
   Build the DEFINITION structure.
   Reads characters of text starting at BUF until END.
   ARGLIST specifies the formal parameters to look for
   in the text of the definition; NARGS is the number of args
   in that list, or -1 for a macro name that wants no argument list.
   MACRONAME is the macro name itself (so we can avoid recursive expansion)
   and NAMELEN is its length in characters.
   
   Note that comments, backslash-newlines, and leading white space
   have already been deleted from the argument.  */

static DEFINITION *
collect_expansion (pfile, buf, limit, nargs, arglist)
     cpp_reader *pfile;
     U_CHAR *buf, *limit;
     int nargs;
     struct arglist *arglist;
{
  DEFINITION *defn;
  register U_CHAR *p, *lastp, *exp_p;
  struct reflist *endpat = NULL;
  /* Pointer to first nonspace after last ## seen.  */
  U_CHAR *concat = 0;
  /* Pointer to first nonspace after last single-# seen.  */
  U_CHAR *stringify = 0;
  int maxsize;
  int expected_delimiter = '\0';

  /* Scan thru the replacement list, ignoring comments and quoted
     strings, picking up on the macro calls.  It does a linear search
     thru the arg list on every potential symbol.  Profiling might say
     that something smarter should happen.  */

  if (limit < buf)
    {
      cpp_ice (pfile, "limit < buf in collect_expansion");
      limit = buf; /* treat it like a null defn */
    }

  /* Find the beginning of the trailing whitespace.  */
  p = buf;
  while (p < limit && is_space(limit[-1]))
    limit--;

  /* Allocate space for the text in the macro definition.
     Leading and trailing whitespace chars need 2 bytes each.
     Each other input char may or may not need 1 byte,
     so this is an upper bound.  The extra 5 are for invented
     leading and trailing escape-marker and final null.  */
  maxsize = (sizeof (DEFINITION)
	     + (limit - p) + 5);
  defn = (DEFINITION *) xcalloc (1, maxsize);

  defn->nargs = nargs;
  exp_p = defn->expansion = (U_CHAR *) defn + sizeof (DEFINITION);
  lastp = exp_p;

  p = buf;

  /* Add one initial space escape-marker to prevent accidental
     token-pasting (often removed by macroexpand).  */
  *exp_p++ = '\r';
  *exp_p++ = ' ';

  if (limit - p >= 2 && p[0] == '#' && p[1] == '#')
    {
      cpp_error (pfile, "`##' at start of macro definition");
      p += 2;
    }

  /* Process the main body of the definition.  */
  while (p < limit)
    {
      int skipped_arg = 0;
      register U_CHAR c = *p++;

      *exp_p++ = c;

      if (!CPP_TRADITIONAL (pfile))
	{
	  switch (c)
	    {
	    case '\'':
	    case '\"':
	      if (expected_delimiter != '\0')
		{
		  if (c == expected_delimiter)
		    expected_delimiter = '\0';
		}
	      else
		expected_delimiter = c;
	      break;

	    case '\\':
	      if (p < limit && expected_delimiter)
		{
		  /* In a string, backslash goes through
		     and makes next char ordinary.  */
		  *exp_p++ = *p++;
		}
	      break;

	    case '#':
	      /* # is ordinary inside a string.  */
	      if (expected_delimiter)
		break;
	      if (p < limit && *p == '#')
		{
		  /* ##: concatenate preceding and following tokens.  */
		  /* Take out the first #, discard preceding whitespace.  */
		  exp_p--;
		  while (exp_p > lastp && is_hspace(exp_p[-1]))
		    --exp_p;
		  /* Skip the second #.  */
		  p++;
		  /* Discard following whitespace.  */
		  SKIP_WHITE_SPACE (p);
		  concat = p;
		  if (p == limit)
		    cpp_error (pfile, "`##' at end of macro definition");
		}
	      else if (nargs >= 0)
		{
		  /* Single #: stringify following argument ref.
		     Don't leave the # in the expansion.  */
		  exp_p--;
		  SKIP_WHITE_SPACE (p);
		  if (p == limit || !is_idstart(*p)
		      || (*p == 'L' && p + 1 < limit && (p[1] == '\'' ||
							 p[1] == '"')))
		    cpp_error (pfile,
		"`#' operator is not followed by a macro argument name");
		  else
		    stringify = p;
		}
	      break;
	    }
	}
      else
	{
	  /* In -traditional mode, recognize arguments inside strings and
	     character constants, and ignore special properties of #.
	     Arguments inside strings are considered "stringified", but no
	     extra quote marks are supplied.  */
	  switch (c)
	    {
	    case '\'':
	    case '\"':
	      if (expected_delimiter != '\0')
		{
		  if (c == expected_delimiter)
		    expected_delimiter = '\0';
		}
	      else
		expected_delimiter = c;
	      break;

	    case '\\':
	      /* Backslash quotes delimiters and itself,
		 but not macro args.  */
	      if (expected_delimiter != 0 && p < limit
		  && (*p == expected_delimiter || *p == '\\'))
		{
		  *exp_p++ = *p++;
		  continue;
		}
	      break;

	    case '/':
	      if (expected_delimiter != '\0')
		/* No comments inside strings.  */
		break;
	      if (*p == '*')
		{
		  /* If we find a comment that wasn't removed by
		     handle_directive, this must be -traditional.
		     So replace the comment with nothing at all.  */
		  exp_p--;
		  p += 1;
		  while (p < limit && !(p[-2] == '*' && p[-1] == '/'))
		    p++;
		}
	      break;
	    }
	}

      /* Handle the start of a symbol.  */
      if (is_idchar(c) && nargs > 0)
	{
	  U_CHAR *id_beg = p - 1;
	  int id_len;

	  --exp_p;
	  while (p != limit && is_idchar(*p))
	    p++;
	  id_len = p - id_beg;

	  if (is_idstart(c)
	      && !(id_len == 1 && c == 'L' && (*p == '\'' || *p == '"')))
	    {
	      register struct arglist *arg;

	      for (arg = arglist; arg != NULL; arg = arg->next)
		{
		  struct reflist *tpat;

		  if (arg->name[0] == c
		      && arg->length == id_len
		      && strncmp (arg->name, id_beg, id_len) == 0)
		    {
		      if (expected_delimiter && CPP_OPTIONS
			(pfile)->warn_stringify)
			{
			  if (CPP_TRADITIONAL (pfile))
			    {
			      cpp_warning (pfile,
				       "macro argument `%.*s' is stringified.",
					   id_len, arg->name);
			    }
			  else
			    {
			      cpp_warning (pfile,
		    "macro arg `%.*s' would be stringified with -traditional.",
					   id_len, arg->name);
			    }
			}
		      /* If ANSI, don't actually substitute
			 inside a string.  */
		      if (!CPP_TRADITIONAL (pfile) && expected_delimiter)
			break;
		      /* make a pat node for this arg and append it
			 to the end of the pat list */
		      tpat = (struct reflist *)
			xmalloc (sizeof (struct reflist));
		      tpat->next = NULL;
		      tpat->raw_before = concat == id_beg;
		      tpat->raw_after = 0;
		      tpat->rest_args = arg->rest_args;
		      tpat->stringify = (CPP_TRADITIONAL (pfile)
					 ? expected_delimiter != '\0'
					 : stringify == id_beg);

		      if (endpat == NULL)
			defn->pattern = tpat;
		      else
			endpat->next = tpat;
		      endpat = tpat;

		      tpat->argno = arg->argno;
		      tpat->nchars = exp_p - lastp;
		      {
			register U_CHAR *p1 = p;
			SKIP_WHITE_SPACE (p1);
			if (p1 + 2 <= limit && p1[0] == '#' && p1[1] == '#')
			  tpat->raw_after = 1;
		      }
		      lastp = exp_p;
		      skipped_arg = 1;
		      break;
		    }
		}
	    }

	  /* If this was not a macro arg, copy it into the expansion.  */
	  if (!skipped_arg)
	    {
	      register U_CHAR *lim1 = p;
	      p = id_beg;
	      while (p != lim1)
		*exp_p++ = *p++;
	      if (stringify == id_beg)
		cpp_error (pfile,
		"`#' operator should be followed by a macro argument name");
	    }
	}
    }

  if (!CPP_TRADITIONAL (pfile) && expected_delimiter == 0)
    {
      /* If ANSI, put in a "\r " marker to prevent token pasting.
         But not if "inside a string" (which in ANSI mode
         happens only for -D option).  */
      *exp_p++ = '\r';
      *exp_p++ = ' ';
    }

  *exp_p = '\0';

  defn->length = exp_p - defn->expansion;

  /* Crash now if we overrun the allocated size.  */
  if (defn->length + 1 > maxsize)
    abort ();

#if 0
/* This isn't worth the time it takes.  */
  /* give back excess storage */
  defn->expansion = (U_CHAR *) xrealloc (defn->expansion, defn->length + 1);
#endif

  return defn;
}

/*
 * special extension string that can be added to the last macro argument to 
 * allow it to absorb the "rest" of the arguments when expanded.  Ex:
 * 		#define wow(a, b...)		process (b, a, b)
 *		{ wow (1, 2, 3); }	->	{ process (2, 3, 1, 2, 3); }
 *		{ wow (one, two); }	->	{ process (two, one, two); }
 * if this "rest_arg" is used with the concat token '##' and if it is not
 * supplied then the token attached to with ## will not be outputted.  Ex:
 * 		#define wow (a, b...)		process (b ## , a, ## b)
 *		{ wow (1, 2); }		->	{ process (2, 1, 2); }
 *		{ wow (one); }		->	{ process (one); {
 */
static char rest_extension[] = "...";
#define REST_EXTENSION_LENGTH	(sizeof (rest_extension) - 1)

/* Create a DEFINITION node from a #define directive.  Arguments are 
   as for do_define.  */

MACRODEF
create_definition (buf, limit, pfile, predefinition)
     U_CHAR *buf, *limit;
     cpp_reader *pfile;
     int predefinition;
{
  U_CHAR *bp;			/* temp ptr into input buffer */
  U_CHAR *symname;		/* remember where symbol name starts */
  int sym_length;		/* and how long it is */
  int rest_args = 0;
  long line, col;
  const char *file =
    CPP_BUFFER (pfile) ? CPP_BUFFER (pfile)->nominal_fname : "";
  DEFINITION *defn;
  int arglengths = 0;		/* Accumulate lengths of arg names
				   plus number of args.  */
  MACRODEF mdef;
  cpp_buf_line_and_col (CPP_BUFFER (pfile), &line, &col);

  bp = buf;

  while (is_hspace(*bp))
    bp++;

  symname = bp;			/* remember where it starts */

  sym_length = check_macro_name (pfile, bp);
  bp += sym_length;

  /* Lossage will occur if identifiers or control keywords are broken
     across lines using backslash.  This is not the right place to take
     care of that.  */

  if (*bp == '(')
    {
      struct arglist *arg_ptrs = NULL;
      int argno = 0;

      bp++;			/* skip '(' */
      SKIP_WHITE_SPACE (bp);

      /* Loop over macro argument names.  */
      while (*bp != ')')
	{
	  struct arglist *temp;

	  temp = (struct arglist *) alloca (sizeof (struct arglist));
	  temp->name = bp;
	  temp->next = arg_ptrs;
	  temp->argno = argno++;
	  temp->rest_args = 0;
	  arg_ptrs = temp;

	  if (rest_args)
	    cpp_pedwarn (pfile, "another parameter follows `%s'",
			 rest_extension);

	  if (!is_idstart(*bp))
	    cpp_pedwarn (pfile, "invalid character in macro parameter name");

	  /* Find the end of the arg name.  */
	  while (is_idchar(*bp))
	    {
	      bp++;
	      /* do we have a "special" rest-args extension here? */
	      if ((size_t) (limit - bp) > REST_EXTENSION_LENGTH
		  && !strncmp (rest_extension, bp, REST_EXTENSION_LENGTH))
		{
		  rest_args = 1;
		  temp->rest_args = 1;
		  break;
		}
	    }
	  temp->length = bp - temp->name;
	  if (rest_args == 1)
	    bp += REST_EXTENSION_LENGTH;
	  arglengths += temp->length + 2;
	  SKIP_WHITE_SPACE (bp);
	  if (temp->length == 0 || (*bp != ',' && *bp != ')'))
	    {
	      cpp_error (pfile,
			 "badly punctuated parameter list in `#define'");
	      goto nope;
	    }
	  if (*bp == ',')
	    {
	      bp++;
	      SKIP_WHITE_SPACE (bp);
	    }
	  if (bp >= limit)
	    {
	      cpp_error (pfile, "unterminated parameter list in `#define'");
	      goto nope;
	    }
	  {
	    struct arglist *otemp;

	    for (otemp = temp->next; otemp != NULL; otemp = otemp->next)
	      if (temp->length == otemp->length
		  && strncmp (temp->name, otemp->name, temp->length) == 0)
		{
		  U_CHAR *name;

		  name = (U_CHAR *) alloca (temp->length + 1);
		  (void) strncpy (name, temp->name, temp->length);
		  name[temp->length] = '\0';
		  cpp_error (pfile,
			     "duplicate argument name `%s' in `#define'",
			     name);
		  goto nope;
		}
	  }
	}

      ++bp;			/* skip paren */
      SKIP_WHITE_SPACE (bp);
      /* now everything from bp before limit is the definition.  */
      defn = collect_expansion (pfile, bp, limit, argno, arg_ptrs);
      defn->rest_args = rest_args;

      /* Now set defn->args.argnames to the result of concatenating
         the argument names in reverse order
         with comma-space between them.  */
      defn->args.argnames = (U_CHAR *) xmalloc (arglengths + 1);
      {
	struct arglist *temp;
	int i = 0;
	for (temp = arg_ptrs; temp; temp = temp->next)
	  {
	    bcopy (temp->name, &defn->args.argnames[i], temp->length);
	    i += temp->length;
	    if (temp->next != 0)
	      {
		defn->args.argnames[i++] = ',';
		defn->args.argnames[i++] = ' ';
	      }
	  }
	defn->args.argnames[i] = 0;
      }
    }
  else
    {
      /* Simple expansion or empty definition.  */

      if (bp < limit)
	{
	  if (is_hspace(*bp))
	    {
	      bp++;
	      SKIP_WHITE_SPACE (bp);
	    }
	  else
	    /* Per C9x, missing white space after the name in a #define
	       of an object-like macro is always a constraint violation. */
	    cpp_pedwarn (pfile,
			 "missing white space after `#define %.*s'",
			 sym_length, symname);
	}
      /* now everything from bp before limit is the definition.  */
      defn = collect_expansion (pfile, bp, limit, -1, NULL_PTR);
      defn->args.argnames = (U_CHAR *) "";
    }

  defn->line = line;
  defn->file = file;

  /* OP is null if this is a predefinition */
  defn->predefined = predefinition;
  mdef.defn = defn;
  mdef.symnam = symname;
  mdef.symlen = sym_length;

  return mdef;

nope:
  mdef.defn = 0;
  return mdef;
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
	ip = CPP_BUFFER (pfile);
	if (hp->type == T_BASE_FILE)
	  {
	    while (CPP_PREV_BUFFER (ip) != CPP_NULL_BUFFER (pfile))
	      ip = CPP_PREV_BUFFER (ip);
	  }
	else
	  {
	    ip = CPP_BUFFER (pfile);
	    while (!ip->nominal_fname && ip != CPP_NULL_BUFFER (pfile))
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
	int true_indepth = 0;
	ip = CPP_BUFFER (pfile);
	for (; ip != CPP_NULL_BUFFER (pfile); ip = CPP_PREV_BUFFER (ip))
	  if (ip->fname != NULL)
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
      ip = CPP_BUFFER (pfile);
      while (!ip->nominal_fname && ip != CPP_NULL_BUFFER (pfile))
	ip = CPP_PREV_BUFFER (ip);
      if (ip->system_header_p
	  && !cpp_lookup (pfile, (U_CHAR *) "__STRICT_ANSI__", 15, -1))
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
macroexpand (pfile, hp)
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
      bcopy (CPP_PWRITTEN (pfile), xbuf, xbuf_len + 1);
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
	  for (i = 0; i < ap->nchars; i++, offset++)
	    xbuf[totlen++] = exp[offset];

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
	      bcopy (ARG_BASE + arg->stringified,
		     xbuf + totlen, arg->stringified_length);
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

	      bcopy (p1, xbuf + totlen, l1 - p1);
	      totlen += l1 - p1;
	    }
	  else
	    {
	      U_CHAR *expanded = ARG_BASE + arg->expanded;
	      if (!ap->raw_before && totlen > 0 && arg->expand_length
		  && !CPP_TRADITIONAL (pfile)
		  && unsafe_chars (xbuf[totlen - 1], expanded[0]))
		{
		  xbuf[totlen++] = '\r';
		  xbuf[totlen++] = ' ';
		}

	      bcopy (expanded, xbuf + totlen, arg->expand_length);
	      totlen += arg->expand_length;

	      if (!ap->raw_after && totlen > 0 && offset < defn->length
		  && !CPP_TRADITIONAL (pfile)
		  && unsafe_chars (xbuf[totlen - 1], exp[offset]))
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
unsafe_chars (c1, c2)
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
      if (c2 == EOF || !unsafe_chars (c1, c2))
	mbuf->rlimit -= 2;
    }
}

/* Return zero if two DEFINITIONs are isomorphic.  */

int
compare_defs (pfile, d1, d2)
     cpp_reader *pfile;
     DEFINITION *d1, *d2;
{
  register struct reflist *a1, *a2;
  register U_CHAR *p1 = d1->expansion;
  register U_CHAR *p2 = d2->expansion;
  int first = 1;

  if (d1->nargs != d2->nargs)
    return 1;
  if (CPP_PEDANTIC (pfile)
      && strcmp ((char *) d1->args.argnames, (char *) d2->args.argnames))
    return 1;
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
dump_definition (pfile, macro)
     cpp_reader *pfile;
     MACRODEF macro;
{
  DEFINITION *defn = macro.defn;

  CPP_RESERVE (pfile, macro.symlen + sizeof "#define ");
  CPP_PUTS_Q (pfile, "#define ", sizeof "#define " -1);
  CPP_PUTS_Q (pfile, macro.symnam, macro.symlen);

  if (defn->nargs == -1)
    {
      CPP_PUTC_Q (pfile, ' ');

      /* The first and last two characters of a macro expansion are
	 always "\r "; this needs to be trimmed out.
	 So we need length-4 chars of space, plus one for the NUL.  */
      CPP_RESERVE (pfile, defn->length - 4 + 1);
      CPP_PUTS_Q (pfile, defn->expansion + 2, defn->length - 4);
      CPP_NUL_TERMINATE_Q (pfile);
    }
  else
    {
      struct reflist *r;
      unsigned char *argnames = (unsigned char *) xstrdup (defn->args.argnames);
      unsigned char **argv = (unsigned char **) alloca (defn->nargs *
							sizeof(char *));
      int *argl = (int *) alloca (defn->nargs * sizeof(int));
      unsigned char *x;
      int i;

      /* First extract the argument list. */
      x = argnames;
      i = defn->nargs;
      while (i--)
	{
	  argv[i] = x;
	  while (*x != ',' && *x != '\0') x++;
	  argl[i] = x - argv[i];
	  if (*x == ',')
	    {
	      *x = '\0';
	      x += 2;  /* skip the space after the comma */
	    }
	}
      
      /* Now print out the argument list. */
      CPP_PUTC_Q (pfile, '(');
      for (i = 0; i < defn->nargs; i++)
	{
	  CPP_RESERVE (pfile, argl[i] + 2);
	  CPP_PUTS_Q (pfile, argv[i], argl[i]);
	  if (i < defn->nargs-1)
	    CPP_PUTS_Q (pfile, ", ", 2);
	}

      if (defn->rest_args)
	CPP_PUTS (pfile, "...) ", 5);
      else
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
      CPP_NUL_TERMINATE (pfile);
    }
}
