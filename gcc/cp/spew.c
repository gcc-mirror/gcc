/* Type Analyzer for GNU C++.
   Copyright (C) 1987, 89, 92-97, 1998 Free Software Foundation, Inc.
   Hacked... nay, bludgeoned... by Mark Eichin (eichin@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* This file is the type analyzer for GNU C++.  To debug it, define SPEW_DEBUG
   when compiling parse.c and spew.c.  */

#include "config.h"
#include "system.h"
#include "input.h"
#include "tree.h"
#include "lex.h"
#include "cp-tree.h"
#include "parse.h"
#include "flags.h"
#include "obstack.h"
#include "toplev.h"

/* This takes a token stream that hasn't decided much about types and
   tries to figure out as much as it can, with excessive lookahead and
   backtracking.  */

/* fifo of tokens recognized and available to parser.  */
struct token  {
  /* The values for YYCHAR will fit in a short.  */
  short		yychar;
  short		end_of_file;
  YYSTYPE	yylval;
};

static int do_aggr PROTO((void));
static int probe_obstack PROTO((struct obstack *, tree, unsigned int));
static void scan_tokens PROTO((unsigned int));

#ifdef SPEW_DEBUG
static int num_tokens PROTO((void));
static struct token *nth_token PROTO((int));
static void add_token PROTO((struct token *));
static void consume_token PROTO((void));
static int debug_yychar PROTO((int));
#endif

/* From lex.c: */
/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
extern tree lastiddecl;		/* let our brains leak out here too */
extern int	yychar;		/*  the lookahead symbol		*/
extern YYSTYPE	yylval;		/*  the semantic value of the		*/
				/*  lookahead symbol			*/
extern int end_of_file;

struct obstack token_obstack;
int first_token;
  
#ifdef SPEW_DEBUG
int spew_debug = 0;
static unsigned int yylex_ctr = 0;
static int debug_yychar ();
#endif

/* Initialize token_obstack. Called once, from init_parse.  */

void
init_spew ()
{
  gcc_obstack_init (&token_obstack);
}

#ifdef SPEW_DEBUG
/* Use functions for debugging...  */

/* Return the number of tokens available on the fifo.  */

static int
num_tokens ()
{
  return (obstack_object_size (&token_obstack) / sizeof (struct token))
    - first_token;
}

/* Fetch the token N down the line from the head of the fifo.  */

static struct token*
nth_token (n)
     int n;
{
  /* could just have this do slurp_ implicitly, but this way is easier
     to debug...  */
  my_friendly_assert (n < num_tokens (), 298);
  return ((struct token*)obstack_base (&token_obstack)) + n + first_token;
}

/* Add a token to the token fifo.  */

static void
add_token (t)
     struct token* t;
{
  obstack_grow (&token_obstack, t, sizeof (struct token));
}

/* Consume the next token out of the fifo.  */

static void
consume_token ()
{
  if (num_tokens () == 1)
    {
      obstack_free (&token_obstack, obstack_base (&token_obstack));
      first_token = 0;
    }
  else
    first_token++;
}

#else
/* ...otherwise use macros.  */

#define num_tokens() \
  ((obstack_object_size (&token_obstack) / sizeof (struct token)) - first_token)

#define nth_token(N) \
  (((struct token*)obstack_base (&token_obstack))+(N)+first_token)

#define add_token(T) obstack_grow (&token_obstack, (T), sizeof (struct token))

#define consume_token() \
  (num_tokens () == 1							\
   ? (obstack_free (&token_obstack, obstack_base (&token_obstack)),	\
      (first_token = 0))						\
   : first_token++)
#endif

/* Pull in enough tokens from real_yylex that the queue is N long beyond
   the current token.  */

static void
scan_tokens (n)
     unsigned int n;
{
  unsigned int i;
  struct token *tmp;

  /* We cannot read past certain tokens, so make sure we don't.  */
  i = num_tokens ();
  if (i > n)
    return;
  while (i-- > 0)
    {
      tmp = nth_token (i);
      /* Never read past these characters: they might separate
	 the current input stream from one we save away later.  */
      if (tmp->yychar == '{' || tmp->yychar == ':' || tmp->yychar == ';')
	goto pad_tokens;
    }

  while (num_tokens () <= n)
    {
      obstack_blank (&token_obstack, sizeof (struct token));
      tmp = ((struct token *)obstack_next_free (&token_obstack))-1;
      tmp->yychar = real_yylex ();
      tmp->end_of_file = end_of_file;
      tmp->yylval = yylval;
      end_of_file = 0;
      if (tmp->yychar == '{'
	  || tmp->yychar == ':'
	  || tmp->yychar == ';')
	{
	pad_tokens:
	  while (num_tokens () <= n)
	    {
	      obstack_blank (&token_obstack, sizeof (struct token));
	      tmp = ((struct token *)obstack_next_free (&token_obstack))-1;
	      tmp->yychar = EMPTY;
	      tmp->end_of_file = 0;
	    }
	}
    }
}

/* Like _obstack_allocated_p, but stop after checking NLEVELS chunks.  */

static int
probe_obstack (h, obj, nlevels)
     struct obstack *h;
     tree obj;
     unsigned int nlevels;
{
  register struct _obstack_chunk*  lp;	/* below addr of any objects in this chunk */
  register struct _obstack_chunk*  plp;	/* point to previous chunk if any */

  lp = (h)->chunk;
  /* We use >= rather than > since the object cannot be exactly at
     the beginning of the chunk but might be an empty object exactly
     at the end of an adjacent chunk.  */
  for (; nlevels != 0 && lp != 0 && ((tree)lp >= obj || (tree)lp->limit < obj);
       nlevels -= 1)
    {
      plp = lp->prev;
      lp = plp;      
    }
  return nlevels != 0 && lp != 0;
}

/* from lex.c: */
/* Value is 1 (or 2) if we should try to make the next identifier look like
   a typename (when it may be a local variable or a class variable).
   Value is 0 if we treat this name in a default fashion.  */
extern int looking_for_typename;
int looking_for_template;
extern int do_snarf_defarg;

extern struct obstack *current_obstack, *saveable_obstack;
tree got_scope;
tree got_object;

int
peekyylex ()
{
  scan_tokens (0);
  return nth_token (0)->yychar;
}

int
yylex ()
{
  struct token tmp_token;
  tree trrr = NULL_TREE;
  int old_looking_for_typename = 0;

 retry:
#ifdef SPEW_DEBUG
  if (spew_debug)
  {
    yylex_ctr ++;
    fprintf (stderr, "\t\t## %d ##", yylex_ctr);
  }
#endif

  if (do_snarf_defarg)
    {
      my_friendly_assert (num_tokens () == 0, 2837);
      tmp_token.yychar = DEFARG;
      tmp_token.yylval.ttype = snarf_defarg ();
      tmp_token.end_of_file = 0;
      do_snarf_defarg = 0;
      add_token (&tmp_token);
    }

  /* if we've got tokens, send them */
  else if (num_tokens ())
    {
      tmp_token= *nth_token (0);

      /* TMP_TOKEN.YYLVAL.TTYPE may have been allocated on the wrong obstack.
	 If we don't find it in CURRENT_OBSTACK's current or immediately
	 previous chunk, assume it was and copy it to the current obstack.  */
      if ((tmp_token.yychar == CONSTANT
	   || tmp_token.yychar == STRING)
	  && ! TREE_PERMANENT (tmp_token.yylval.ttype)
	  && ! probe_obstack (current_obstack, tmp_token.yylval.ttype, 2)
	  && ! probe_obstack (saveable_obstack, tmp_token.yylval.ttype, 2))
	tmp_token.yylval.ttype = copy_node (tmp_token.yylval.ttype);
    }
  else
    {
      /* if not, grab the next one and think about it */
      tmp_token.yychar = real_yylex ();
      tmp_token.yylval = yylval;
      tmp_token.end_of_file = end_of_file;
      add_token (&tmp_token);
    }

  /* many tokens just need to be returned. At first glance, all we
     have to do is send them back up, but some of them are needed to
     figure out local context.  */
  switch (tmp_token.yychar)
    {
    case EMPTY:
      /* This is a lexical no-op.  */
      consume_token ();
#ifdef SPEW_DEBUG    
      if (spew_debug)
	debug_yychar (tmp_token.yychar);
#endif
      goto retry;

    case IDENTIFIER:
      scan_tokens (1);
      if (nth_token (1)->yychar == SCOPE)
	{
	  /* Don't interfere with the setting from an 'aggr' prefix.  */
	  old_looking_for_typename = looking_for_typename;
	  looking_for_typename = 1;
	}
      else if (nth_token (1)->yychar == '<')
	looking_for_template = 1;

      trrr = lookup_name (tmp_token.yylval.ttype, -2);

      if (trrr)
	{
	  tmp_token.yychar = identifier_type (trrr);
	  switch (tmp_token.yychar)
	    {
	    case TYPENAME:
	    case SELFNAME:
	    case NSNAME:
	    case PTYPENAME:
	      lastiddecl = trrr;

	      /* If this got special lookup, remember it.  In these cases,
	         we don't have to worry about being a declarator-id. */
	      if (got_scope || got_object)
		tmp_token.yylval.ttype = trrr;
	      break;

	    case PFUNCNAME:
	    case IDENTIFIER:
	      lastiddecl = trrr;
	      break;

	    default:
	      my_friendly_abort (101);
	    }
	}
      else
	lastiddecl = NULL_TREE;
      got_scope = NULL_TREE;
      /* and fall through to...  */
    case IDENTIFIER_DEFN:
    case TYPENAME:
    case TYPENAME_DEFN:
    case PTYPENAME:
    case PTYPENAME_DEFN:
      consume_token ();
      /* If we see a SCOPE next, restore the old value.
	 Otherwise, we got what we want. */
      looking_for_typename = old_looking_for_typename;
      looking_for_template = 0;
      break;

    case SCSPEC:
      /* If export, warn that it's unimplemented and go on. */
      if (tmp_token.yylval.ttype == get_identifier("export"))
	{
	  warning ("keyword 'export' not implemented and will be ignored");
	  consume_token ();
	  goto retry;
	}
      else
	{
	  ++first_token;
	  break;
	}

    case NEW:
      /* do_aggr needs to check if the previous token was RID_NEW,
	 so just increment first_token instead of calling consume_token.  */
      ++first_token;
      break;

    case TYPESPEC:
      consume_token ();
      break;

    case AGGR:
      *nth_token (0) = tmp_token;
      do_aggr ();
      /* fall through to output...  */
    case ENUM:
      /* Set this again, in case we are rescanning.  */
      looking_for_typename = 2;
      /* fall through...  */
    default:
      consume_token ();
    }

  /* class member lookup only applies to the first token after the object
     expression, except for explicit destructor calls.  */
  if (tmp_token.yychar != '~')
    got_object = NULL_TREE;

  /* Clear looking_for_typename if we got 'enum { ... };'.  */
  if (tmp_token.yychar == '{' || tmp_token.yychar == ':'
      || tmp_token.yychar == ';')
    looking_for_typename = 0;

  yylval = tmp_token.yylval;
  yychar = tmp_token.yychar;
  end_of_file = tmp_token.end_of_file;
#ifdef SPEW_DEBUG    
  if (spew_debug)
    debug_yychar (yychar);
#endif

  return yychar;
}

/* token[0] == AGGR (struct/union/enum)
   Thus, token[1] is either a TYPENAME or a TYPENAME_DEFN.
   If token[2] == '{' or ':' then it's TYPENAME_DEFN.
   It's also a definition if it's a forward declaration (as in 'struct Foo;')
   which we can tell if token[2] == ';' *and* token[-1] != FRIEND or NEW.  */

static int
do_aggr ()
{
  int yc1, yc2;
  
  scan_tokens (2);
  yc1 = nth_token (1)->yychar;
  if (yc1 != TYPENAME && yc1 != IDENTIFIER && yc1 != PTYPENAME)
    return 0;
  yc2 = nth_token (2)->yychar;
  if (yc2 == ';')
    {
      /* It's a forward declaration iff we were not preceded by
         'friend' or `new'.  */
      if (first_token > 0)
	{
	  if (nth_token (-1)->yychar == SCSPEC
	      && nth_token (-1)->yylval.ttype == ridpointers[(int) RID_FRIEND])
	    return 0;
	  if (nth_token (-1)->yychar == NEW)
	    return 0;
	}
    }
  else if (yc2 != '{' && yc2 != ':')
    return 0;

  switch (yc1)
    {
    case TYPENAME:
      nth_token (1)->yychar = TYPENAME_DEFN;
      break;
    case PTYPENAME:
      nth_token (1)->yychar = PTYPENAME_DEFN;
      break;
    case IDENTIFIER:
      nth_token (1)->yychar = IDENTIFIER_DEFN;
      break;
    default:
      my_friendly_abort (102);
    }
  return 0;
}  
  
#ifdef SPEW_DEBUG    
/* debug_yychar takes a yychar (token number) value and prints its name.  */

static int
debug_yychar (yy)
     int yy;
{
  /* In parse.y: */
  extern char *debug_yytranslate ();
  
  int i;
  
  if (yy<256) {
    fprintf (stderr, "<%d: %c >\n", yy, yy);
    return 0;
  }
  fprintf (stderr, "<%d:%s>\n", yy, debug_yytranslate (yy));
  return 1;
}

#endif
