/* Type Analyzer for GNU C++.
   Copyright (C) 1987, 1989, 1992, 1993 Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This file is the type analyzer for GNU C++.  To debug it, define SPEW_DEBUG
   when compiling parse.c and spew.c.  */

#include "config.h"
#include <stdio.h>
#include "input.h"
#include "tree.h"
#include "lex.h"
#include "parse.h"
#include "cp-tree.h"
#include "flags.h"
#include "obstack.h"

/* This takes a token stream that hasn't decided much about types and
   tries to figure out as much as it can, with excessive lookahead and
   backtracking. */

/* fifo of tokens recognized and available to parser. */
struct token  {
  /* The values for YYCHAR will fit in a short.  */
  short		yychar;
  short		end_of_file;
  YYSTYPE	yylval;
};

static int do_aggr ();
static struct token frob_identifier ();
static struct token hack_scope ();
static tree hack_ptype ();
static tree hack_more_ids ();

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

static char follows_typename[END_OF_SAVED_INPUT+1];
static char follows_identifier[END_OF_SAVED_INPUT+1];

/* This is a hack!!! TEMPLATE_TYPE_SEEN_BEFORE_SCOPE consists of the name
 * of the last template_type parsed in parse.y if it is followed by a
 * scope operator.  It will be reset inside the next invocation of yylex().
 * This is used for recognizing nested types inside templates.
 * - niklas@appli.se */
tree template_type_seen_before_scope;

/* Initialize token_obstack. Called once, from init_lex.  */
void
init_spew ()
{
  static char *chars_following_identifier = ".+-|/%^!?:";
  short *ps;
  static short toks_follow_ids[] =
    { ASSIGN, RANGE, OROR, ANDAND, MIN_MAX, EQCOMPARE,
      ARITHCOMPARE, LSHIFT, RSHIFT, UNARY, PLUSPLUS, MINUSMINUS, POINTSAT,
      POINTSAT_STAR, DOT_STAR, CONSTANT, STRING, SIZEOF, ENUM, IF,
      ELSE, WHILE, DO, FOR, SWITCH, CASE, DEFAULT, BREAK, CONTINUE,
      RETURN, GOTO, ASM_KEYWORD, GCC_ASM_KEYWORD, TYPEOF, ALIGNOF, HEADOF,
      CLASSOF, SIGOF, ATTRIBUTE, AGGR, VISSPEC, DELETE, RAISE, RERAISE, TRY,
      EXCEPT, CATCH, THROW, ANSI_TRY, ANSI_THROW, DYNAMIC_CAST, TYPEID,
      EXTERN_LANG_STRING, ALL, END_OF_SAVED_INPUT, -1 };
  static short toks_follow_types[] =
    { IDENTIFIER, TYPENAME, SCOPED_TYPENAME, SCOPED_NAME, SCSPEC, 
      TYPESPEC, TYPE_QUAL,
      ELLIPSIS, THIS, OPERATOR, TEMPLATE, SCOPE, START_DECLARATOR,
      TYPENAME_COLON, PAREN_STAR_PAREN, TYPENAME_ELLIPSIS, PTYPENAME,
      PRE_PARSED_FUNCTION_DECL, PRE_PARSED_CLASS_DECL, -1 };

  gcc_obstack_init(&token_obstack);

  /* Initialize the arrays saying what tokens are definitely
     (or possibly) valid following typenames and identifiers.  */
  while (*chars_following_identifier)
    follows_identifier[*chars_following_identifier++] = 1;
  for (ps = toks_follow_ids; *ps != -1; ps++)
    follows_identifier[*ps] = 1;
  for (ps = toks_follow_types; *ps != -1; ps++)
    follows_typename[*ps] = 1;
}

#ifdef SPEW_DEBUG
/* Use functions for debugging...  */

/* Return the number of tokens available on the fifo. */
static int
num_tokens ()
{
  return (obstack_object_size(&token_obstack)/sizeof(struct token))
    - first_token;
}

/* Fetch the token N down the line from the head of the fifo. */
static struct token*
nth_token (n)
     int n;
{
  /* could just have this do slurp_ implicitly, but this way is easier
   * to debug... */
  my_friendly_assert (n < num_tokens(), 298);
  return ((struct token*)obstack_base(&token_obstack))+n+first_token;
}

/* Add a token to the token fifo. */
static void
add_token (t)
     struct token* t;
{
  obstack_grow(&token_obstack,t,sizeof (struct token));
}

/* Consume the next token out of the fifo.  */
static void
consume_token()
{
  if (num_tokens() == 1)
    {
      obstack_free(&token_obstack, obstack_base (&token_obstack));
      first_token = 0;
    }
  else
    first_token++;
}

#else
/* ...otherwise use macros.  */

#define num_tokens() \
  ((obstack_object_size(&token_obstack)/sizeof(struct token)) - first_token)

#define nth_token(N) \
  (((struct token*)obstack_base(&token_obstack))+(N)+first_token)

#define add_token(T) obstack_grow(&token_obstack, (T), sizeof (struct token))

#define consume_token() \
  (num_tokens() == 1							\
   ? (obstack_free (&token_obstack, obstack_base (&token_obstack)),	\
      (first_token = 0))						\
   : first_token++)
#endif

/* Pull in enough tokens from real_yylex that the queue is N long.  */

static void
scan_tokens (n)
     int n;
{
  int i;
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

  while (num_tokens() <= n)
    {
      obstack_blank(&token_obstack,sizeof (struct token));
      tmp = ((struct token *)obstack_next_free (&token_obstack))-1;
      tmp->yychar = real_yylex();
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
	      obstack_blank(&token_obstack,sizeof (struct token));
	      tmp = ((struct token *)obstack_next_free (&token_obstack))-1;
	      tmp->yychar = EMPTY;
	      tmp->end_of_file = 0;
	    }
	}
    }
}

/* Create room for N tokens at the front of the fifo.  This is used
   to insert new tokens into the stream ahead of the current token.  */

static void
shift_tokens (n)
     int n;
{
  if (first_token >= n)
    first_token -= n;
  else
    {
      int old_token_count = num_tokens ();
      char *tmp;

      obstack_blank (&token_obstack, (n-first_token) * sizeof (struct token));
      if (old_token_count)
	{
	  tmp = (char *)alloca ((num_tokens () + (n-first_token))
				* sizeof (struct token));
	  /* This move does not rely on the system being able to handle
	     overlapping moves.  */
	  bcopy (nth_token (0), tmp, old_token_count * sizeof (struct token));
	  bcopy (tmp, nth_token (n), old_token_count * sizeof (struct token));
	}
      first_token = 0;
    }
}

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
     at the end of an adjacent chunk. */
  for (; nlevels != 0 && lp != 0 && ((tree)lp >= obj || (tree)lp->limit < obj);
       nlevels -= 1)
    {
      plp = lp->prev;
      lp = plp;      
    }
  return nlevels != 0 && lp != 0;
}

/* from lex.c: */
/* Value is 1 if we should try to make the next identifier look like a
   typename (when it may be a local variable or a class variable).
   Value is 0 if we treat this name in a default fashion.
   Value is -1 if we must not see a type name.  */
extern int looking_for_typename;

extern struct obstack *current_obstack, *saveable_obstack;

int
yylex()
{
  struct token tmp_token;
  tree trrr;

 retry:
#ifdef SPEW_DEBUG
  if (spew_debug)
  {
    yylex_ctr ++;
    fprintf(stderr, "\t\t## %d ##",yylex_ctr);
  }
#endif
  
  /* This is a kludge for recognizing nested types in templates */
  if (template_type_seen_before_scope)
    {
      shift_tokens (2);		/* Sync in hack_more_ids (yes, it's ugly) */
      nth_token (1)->yychar = SCOPE;
      yylval.ttype = hack_more_ids (0, template_type_seen_before_scope);
      template_type_seen_before_scope = 0;
      if (!yylval.ttype)
	{
	  /* Sync back again, leaving SCOPE on the token stream, because we
	   * failed to substitute the original SCOPE token with a
	   * SCOPED_TYPENAME.  See rule "template_type" in parse.y */
	  consume_token ();
	}
      else
	{
	  tree t = TREE_TYPE(yylval.ttype);
	  if (TREE_CODE(yylval.ttype) == SCOPE_REF && 
		t && TREE_CODE(t) == UNINSTANTIATED_P_TYPE)
	    yychar = SCOPED_NAME;
	  else
	    yychar = SCOPED_TYPENAME;
#ifdef SPEW_DEBUG    
	  if (spew_debug)
	    debug_yychar(yychar);
#endif
	  return yychar;
	}
    }

  /* if we've got tokens, send them */
  if (num_tokens())
    {
      tmp_token= *nth_token(0);

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
      add_token(&tmp_token);
    }

  /* many tokens just need to be returned. At first glance, all we
   * have to do is send them back up, but some of them are needed to
   * figure out local context. */
  switch(tmp_token.yychar)
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
      trrr = lookup_name (tmp_token.yylval.ttype, -2);
      if (trrr)
	{
	  tmp_token.yychar = identifier_type (trrr);
	  switch (tmp_token.yychar)
	    {
	    case TYPENAME:
	      lastiddecl = identifier_typedecl_value (tmp_token.yylval.ttype);
	      if (lastiddecl == NULL_TREE)
		lastiddecl = trrr;
	      break;
	    case IDENTIFIER:
	      lastiddecl = trrr;
	      break;
	    case PTYPENAME:
	      /* This is for cases like
		    template<class A> X<A>::operator[] ...
		 since "X" is (presumably) a PTYPENAME; we might want to
		 avoid seeing the entire thing as a type name, but X<A>
		 must be one.

		 It might not work right if the thing after the ::
		 can be a typename nested in X<A>, but I don't think the
		 PT code would be up to dealing with that anyways.  --KR  */
	      if (looking_for_typename == -1)
		{
		  scan_tokens (2);
		  if (nth_token(1)->yychar == '<')
		    looking_for_typename = 0;
		}
	      break;
	    default:
	      my_friendly_abort (101);
	    }
	}
      else
	lastiddecl = trrr;
      /* and fall through to... */
    case TYPENAME:
    case PTYPENAME:
      /* if (new_token) add_token (&tmp_token); */
      *nth_token(0) = tmp_token;
      tmp_token = frob_identifier ();
      if (looking_for_typename < 0)
	{
	  tmp_token.yychar = IDENTIFIER;
	  lastiddecl = 0;
	  looking_for_typename = 0;
	}
      else if (lastiddecl && TREE_CODE (lastiddecl) == TYPE_DECL)
	{
	  scan_tokens (2);
	  if (nth_token(0)->yychar == IDENTIFIER
	      && nth_token (1)->yychar != SCOPE)
	    looking_for_typename = -1;
	  else
	    looking_for_typename = 0;
	  goto finish_typename_processing;
	}
      else
	looking_for_typename = 0;
      break;

    case SCSPEC:
      /* do_aggr needs to check if the previous token was RID_FRIEND,
	 so just increment first_token instead of calling consume_token. */
      first_token++;
      goto finish_typename_processing;
    case TYPESPEC:
      consume_token ();
    finish_typename_processing:
#if 0
      /* Now see if we should insert a START_DECLARATOR token.
         Here are the cases caught:

	 typespec ( * ID ) (	// ptr to function
	 typespec ( & ID ) (	// ref to function
	 typespec ( * ID ) [	// array of pointers
	 typespec ( & ID ) [	// array of references

	 This is a terrible kludge.  */

      scan_tokens (2);
      if (nth_token (0)->yychar == '('
	  && (nth_token (1)->yychar == '*'
	      || nth_token (1)->yychar == '&'))
	{
	  scan_tokens (5);
	  if (nth_token (3)->yychar == ')'
	      && (nth_token (4)->yychar == '('
		  || nth_token (4)->yychar == '['
		  || nth_token (4)->yychar == LEFT_RIGHT)
	      && (nth_token (2)->yychar == IDENTIFIER
		  || nth_token (2)->yychar == TYPENAME))
	    {
	      shift_tokens (1);
	      nth_token (0)->yychar = START_DECLARATOR;
	    }
	}
      /* Extend to handle:

	 typespec (ID::* qf)(   // ptr to member function
	 typespec (ID::* qf)[   // array of ptr to member functions

	 */
      if (nth_token (0)->yychar == '('
	  && (nth_token (1)->yychar == IDENTIFIER
	      || nth_token (1)->yychar == TYPENAME))
	{
	  scan_tokens (7);
	  if (nth_token (2)->yychar == SCOPE
	      && nth_token (3)->yychar == '*'
	      && (nth_token (4)->yychar == IDENTIFIER
		  || nth_token (4)->yychar == TYPENAME)
	      && nth_token (5)->yychar == ')'
	      && (nth_token (6)->yychar == '('
		  || nth_token (6)->yychar == '['
		  || nth_token (6)->yychar == LEFT_RIGHT))
	    {
	      shift_tokens (1);
	      nth_token (0)->yychar = START_DECLARATOR;
	    }
	}
#endif
      break;

#if 0
    case '(':
      /* Handle casts.  We are looking for one of:
         `( TYPENAME' followed by `)', or
	 `( TYPENAME *' followed by one of `[,*,&,)', or
	 `( TYPENAME &' followed by one of `[,*,&,)', or
	 `( TYPENAME [' followed by `]'.  We are punting
	 generality on scanning casts to array types.  */
      scan_tokens (4);
      if (nth_token (1)->yychar == IDENTIFIER)
	{
	  tree type = identifier_typedecl_value (nth_token (1)->yylval.ttype);
	  if (type)
	    switch (nth_token (2)->yychar)
	      {
	      default:
		break;
	      }
	}
      break;

    case SCOPE:
      /* if (new_token) add_token (&tmp_token); */
      *nth_token(0) = tmp_token;
      tmp_token = hack_scope ();
      break;
#endif

    case AGGR:
      *nth_token(0) = tmp_token;
      do_aggr ();
      /* fall through to output... */
    case ENUM:
      /* Set this again, in case we are rescanning.  */
      looking_for_typename = 1;
      /* fall through... */
    default:
#ifdef SPEW_DEBUG    
      if (spew_debug)
	debug_yychar(tmp_token.yychar);
#endif
      consume_token();
      yylval = tmp_token.yylval;
      yychar = tmp_token.yychar;
      end_of_file = tmp_token.end_of_file;
      return tmp_token.yychar;
    }

  if (tmp_token.yychar == SCOPED_TYPENAME)
    {
#if 0
      tree t2 = resolve_scope_to_name (NULL_TREE, tmp_token.yylval.ttype);
      if (t2 != NULL_TREE)
	{
	  tmp_token.yylval.ttype = t2;
	  tmp_token.yychar = TYPENAME;
	}
      else
	{
	  /* unwind? */
	}
    }
  else
    {
      /* couldn't get here, as is... */
#endif
      tmp_token.yychar = TYPENAME;
    }

  yylval = tmp_token.yylval;
  yychar = tmp_token.yychar;
  end_of_file = tmp_token.end_of_file;
#ifdef SPEW_DEBUG    
  if (spew_debug)
    debug_yychar(yychar);
#endif
/*  consume_token(); */ /* already eaten by frob_identifier?... */
  return yychar;
}

/* token[0] == AGGR (struct/union/enum)
 * Thus, token[1] is either a TYPENAME or a TYPENAME_DEFN.
 * If token[2] == '{' or ':' then it's TYPENAME_DEFN.
 * It's also a definition if it's a forward declaration (as in 'struct Foo;')
 * which we can tell lf token[2] == ';' *and* token[-1] != FRIEND.
 */
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
      /* It's a forward declaration iff we were not preceded by 'friend'. */
      if (first_token > 0 && nth_token (-1)->yychar == SCSPEC
	  && nth_token (-1)->yylval.ttype == ridpointers[(int) RID_FRIEND])
	return 0;
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

static struct token
frob_identifier ()
{
  /* we could have a type, if it is followed by :: (if so, suck it all up); */
  /* we could have a ptypename; */
  /* we could have a normal identifier. */
  tree t1;
  struct token rt;
  
  scan_tokens(1);
  rt = *nth_token(0);

#if 0
  if (nth_token(1)->yychar == '<')
    {
      t1 = hack_ptype();	/* suck up the whole thing */
      if (t1)
	{
	  rt.yylval.ttype = t1;
	  rt.yychar = TYPENAME;
	  *nth_token(0) = rt;
	}
      /* else fall out bottom */
    }	
#endif

  if (nth_token(1)->yychar == SCOPE)
    {
#if 0
      t1 = hack_more_ids(0);
      if (t1 && TREE_CODE(t1) == SCOPE_REF)
#else
      t1 = hack_more_ids(0, nth_token (0)->yylval.ttype);
      if (t1)
#endif
	{
	  rt.yylval.ttype = t1;
	  rt.yychar = SCOPED_TYPENAME ;
	  return rt;
	}
      else
	{
	  /* deal with types (enums?) in classes... */
	  struct token *tok;
	  tree ta, tb;
	  scan_tokens(3);

	  /* Have to check for a type conversion operator
	     to a nested type.  */
	  if (nth_token (2)->yychar == OPERATOR)
	    tok = nth_token (3);
	  else
	    tok = nth_token(2);

	  if (tok->yychar == IDENTIFIER || tok->yychar == TYPENAME)
	    {
	      ta = build_parse_node (SCOPE_REF,
				     nth_token(0)->yylval.ttype,
				     tok->yylval.ttype);
	      tb = resolve_scope_to_name (NULL_TREE, ta);

	      if (tb != NULL_TREE)
		{
		  if (nth_token (2)->yychar == OPERATOR)
		    {
		      /* Have to keep these tokens around
			 so we can finish parsing the declaration.
			 What do we do for

			 int foo::operator bar::baz (); 

			 where bar is a nested class in foo?  */
		      nth_token (3)->yychar = TYPENAME;
		      nth_token (3)->yylval.ttype = tb;
		    }
		  else
		    {
		      consume_token (); /* base type */
		      consume_token (); /* SCOPE */
		      consume_token (); /* member type */
		      rt.yychar = TYPENAME;
		      rt.yylval.ttype = tb;
		      rt.end_of_file = tok->end_of_file;
		      return rt;
		    }
		  
		}
	    }      
	  /* else fall out bottom */
	}
    }
	     
  consume_token();
  return rt;
}

#if 0
/* When this function is called, nth_token(0) is the current
   token we are scanning.  This means that the next token we'll
   scan is nth_token (1).  Usually the next token we'll scan
   is nth_token (0) (and the current token is in [yylval,yychar]).  */
tree
arbitrate_lookup (name, exp_decl, type_decl)
     tree name, exp_decl, type_decl;
{
  int ch;
  tree t;
  char *assume;

  scan_tokens (3);
  ch = nth_token (1)->yychar;

  switch (ch)
    {
    case '(':
    case LEFT_RIGHT:
      /* If we guessed wrong here, `build_functional_cast' can fix it.  */
      return type_decl;

    case '=':
      if (global_bindings_p ())
	/* Probably a default parameter.  */
	return type_decl;
      /* Probably not an initialization.  */
      return exp_decl;

    case '[':
      /* This needs special help because an expression inside the
	 brackets means nothing.  */
      {
	int i;

	for (i = 0; i < 42; i++)
	  {
	    int ith_yychar;

	    scan_tokens (3+i);
	    ith_yychar = nth_token (2+i)->yychar;

	    /* If we hit an undefined identifier, assume
	       the decl in arbitration is its type specifier.  */
	    if (ith_yychar == IDENTIFIER
		&& lookup_name (nth_token (2+i)->yylval.ttype, 0) == 0)
	      return type_decl;
	    else if (ith_yychar == ']')
	      {
		/* There are only a few things we expect after a ']'
		   in a declarator.  */
		i += 1;
		scan_tokens (4+i);
		ith_yychar = nth_token (2+i)->yychar;

		/* These are inconclusive.  */
		if (ith_yychar == LEFT_RIGHT
		    || ith_yychar == '('
		    || ith_yychar == '['
		    || ith_yychar == ',')
		  continue;
		/* stmt or decl?  We'll probably never know.  */
		else if (ith_yychar == ';')
		  goto warn_ambiguous;

		if (ith_yychar == '=')
		  {
		    if (nth_token (3+i)->yychar == '{')
		      return type_decl;
		    continue;
		  }

		/* Whatever it is, it looks like we're processing an expr.  */
		return exp_decl;
	      }
	  }
	goto warn_ambiguous;
      }

    case ',':
    case ';':
    case '&':
    case '<':
    case '*':
    case ']':
    case ')':
    case '>':
      /* see if the next token looks like it wants to be part
	 of a declaration list or an expression list.  */
      {
	int i;

	/* Some heuristics: if we are inside a function definition,
	   prefer the local declaration.  */
	if (! global_bindings_p ())
	  {
	    if (IDENTIFIER_LOCAL_VALUE (name) == exp_decl)
	      return exp_decl;
	    if (IDENTIFIER_LOCAL_VALUE (name) != type_decl
		&& IDENTIFIER_CLASS_VALUE (name) == exp_decl)
	      return exp_decl;
	  }
	/* If these symbols follow in a list, we know it's a list of
	   expressions.  */
	if (follows_identifier[nth_token (2)->yychar])
	  return exp_decl;

	/* If we see a id&, or id&) the we are probably in an argument list. */
	if (ch=='&'
	    && (nth_token (2)->yychar == ',' || nth_token (2)->yychar == ')'))
	  return type_decl;

	/* Look for the first identifier or other distinguishing token
	   we find in the next several tokens.  */
	for (i = 0; i < 42; i++)
	  {
	    int ith_yychar;

	    scan_tokens (3+i);
	    ith_yychar = nth_token (2+i)->yychar;

	    if (ith_yychar == IDENTIFIER)
	      {
		tree as_type = lookup_name (nth_token (2+i)->yylval.ttype, 1);
		if (as_type && TREE_CODE (as_type) != TYPE_DECL)
		  return exp_decl;
		/* An undeclared identifier or a typename means we're
		   probably looking at a typename.  */
		return type_decl;
	      }
	    else if (ith_yychar == EMPTY
		     || follows_identifier[ith_yychar])
	      return exp_decl;
	    else if (follows_typename[ith_yychar])
	      return type_decl;
	    /* stmt or decl?  We'll probably never know.  */
	    else if (ith_yychar == ';')
	      goto warn_ambiguous;
	  }
	goto warn_ambiguous;
      }

    default:
      if (follows_identifier[ch])
	return exp_decl;
      if (follows_typename[ch])
	return type_decl;

      /* Fall through...  */
    warn_ambiguous:
      if (ch == '[')
	{
	  assume = "expression";
	  t = exp_decl;
	}
      else
	{
	  assume = "type";
	  t = type_decl;
	}

      warning ("name `%s' could be type or expression; compiler assuming %s",
	       IDENTIFIER_POINTER (DECL_NAME (t)), assume);
      return t;
    }
}
#endif

/* now returns decl_node */

#if 0
static tree
hack_ptype()
{
  /* when we get here, we know that [0] is a ptype and [1] is '<'.
   * now we loop over simple parameters. */
  struct token this_param;
  int n = 2;
  tree tplist = 0;
  tree tc;
  scan_tokens(n+1);
  
  while((this_param = *nth_token(n)).yychar != '>')
    {
      /* if it is a type, add it to the list */
      tree thistype;
    
      switch(this_param.yychar)
	{
	case IDENTIFIER:
	case TYPENAME:
	case TYPESPEC:
	  break;
	default:
	  return 0;
	}

      thistype = this_param.yylval.ttype;
      thistype = lookup_name(thistype, 1);
      thistype = TREE_TYPE (thistype);
        
      if (tplist)
	tplist = chainon (tplist, build_tree_list (NULL_TREE, thistype));
      else
	tplist = build_tree_list(NULL_TREE, thistype);
    
    
      /* then suck up the comma */
      n++;
      scan_tokens(n+1);
      this_param = *nth_token(n);
      if (this_param.yychar == ',')
	{
	  n++;
	  scan_tokens(n+1);
	  continue;
	}
      if (this_param.yychar == '>')
	break;
      return 0;
    }

  /* once we're done, lookup_template_class -> identifier */
  tc = lookup_template_class (nth_token(0)->yylval.ttype,tplist);
  /* then lookup_name on that to get a type, if there is one */
  tc = lookup_name (tc, 1);
  if (tc)
    {
      int i;
      /* don't actually eat the trailing '>'... we can replace it! */
      for (i=0; i<n; i++)
	consume_token();
      /*    IDENTIFIER_TYPE_VALUE (DECL_NAME (tc)) = */
      return DECL_NAME (tc);
    }
  return NULL_TREE;
}
#endif

#if 0
static tree
hack_more_ids (n)
     int n;
{
  /*
   * The recursion should probably do consume_tokens(), since once we've started
   * down an IDENTIFIER SCOPE ... chain, we don't need to back-track - we just
   * get as much as we can, make SCOPE_REF's out of it, and return it.
   */
  struct token this_iter, this2_iter;
  int tmp_y;
  
  scan_tokens(n+1);
  this_iter = *nth_token(n);

  tmp_y = nth_token(n)->yychar;
  if (tmp_y == IDENTIFIER || tmp_y == TYPENAME)
    {
      scan_tokens(n+2+2);
      if (nth_token(n+1)->yychar == SCOPE)
	{
	  if (nth_token(n+1+2)->yychar == SCOPE)
	    {
	      tree hmi;
	
	      consume_token();	/* last IDENTIFIER (this_iter) */
	      consume_token();	/* last SCOPE */
	      this2_iter = *nth_token(n);
	
	      hmi = hack_more_ids (n);
	
	      if (hmi)
		return build_parse_node (SCOPE_REF, this_iter.yylval.ttype, hmi);
	      consume_token(); /* last IDENTIFIER (this2_iter) */
	      return build_parse_node (SCOPE_REF, this_iter.yylval.ttype,
				       this2_iter.yylval.ttype);
	    }
	  else
	    {
	      /* consume_token();	*/	/* last IDENTIFIER */
	      /* leave whatever else we got */
	      /* return this_iter.yylval.ttype; */
	      return NULL_TREE;
	    }
	}
    }
  return NULL_TREE;		/* @@ may need to backtrack */
}
#else
/* niklas@appli.se says:  I didn't understand how the code above was intended
 * to work, so I rewrote it (also changed the interface a bit).  This code
 * dives down an IDENTIFIER/TYPENAME SCOPE ... chain as long as the parsed
 * type prefix constitutes recognizable (by resolve_scope_to_name) types.
 * Interface changed like this:
 * 1. Takes an extra argument containing the name of the the type recognized
 *    so far.
 * 2. Now returns the name of the type instead of a SCOPE_REF. */
static tree
hack_more_ids(n, outer)
  int n;
  tree outer;
{
  int ch;
  tree type, val, inner, outer_t;

  scan_tokens (n + 2);
  if (nth_token (n + 1)->yychar != SCOPE
      || ((ch = nth_token (n + 2)->yychar) != IDENTIFIER && ch != TYPENAME))
    return NULL_TREE;

  inner = nth_token(n+2)->yylval.ttype;
  val = build_parse_node (SCOPE_REF, outer, inner);
  outer_t = TREE_TYPE(outer);
  if (outer_t && TREE_CODE(outer_t) == UNINSTANTIATED_P_TYPE)
    {
      tree t = make_lang_type (UNINSTANTIATED_P_TYPE);
      tree id = inner;
      tree d = build_lang_decl (TYPE_DECL, id, t);

      TYPE_NAME (t) = d;
      TYPE_VALUES (t) = TYPE_VALUES(outer_t);
      TYPE_CONTEXT(t) = outer_t;
/*
      pushdecl_top_level (d);
*/
      pushdecl(d);

      type = val;
      TREE_TYPE(type) = t;
    }
  else
    {
      type = resolve_scope_to_name (NULL_TREE, val);
      if (type == NULL_TREE)
        return NULL_TREE;
    }
  consume_token ();
  consume_token ();
  val = hack_more_ids (n, type);
  if (! val)
    consume_token ();
  return val ? val : type;
}
#endif

#if 0
static struct token
hack_scope ()
{
  /* we've got a :: - what follows is either a global var or a type. */
  /* hmm, template names can be in the global scope too... */
  tree t1;
  struct token rt;
  
  scan_tokens(1);
  if (nth_token(1)->yychar == IDENTIFIER)
    {
      /* @@ this is probably not right, but doesn't get hit yet */
      t1 = build_parse_node (SCOPE_REF,
			     NULL_TREE, /* to get "global" scope */
			     hack_more_ids(0)); /* do some prefetching */
      rt.yylval.ttype = t1;
      rt.yychar =		/*SCOPED_*/TYPENAME;
      return rt;
    }
  else
    {
      rt = *nth_token(0);
      consume_token();
      return rt;
    }
}
#endif
  
/*
 * Generations:
 * 	
 * PINST: PTYPE { saved_arg_count = arg_count($1) }
 *        '<' { arg_c = 0; } PARGS '>'
 *        ;
 * PARG: TYPE
 *       | VALUE
 *       ;
 * (of course the arg counting doesn't work for recursion... Do it right.)
 * PARGS: PARG { assert(arg_c == saved_arg_count); }
 *        | PARG ',' PARGS	{ arg_c++; }
 *        ;
 * ATYPE: PINST
 *        | TYPEID
 *        ;
 * TYPE: ATYPE
 *       | ATYPE { basetype = $1; } '::' TYPEKIDS
 *       ;
 * TYPEKIDS: TYPE { assert ($1 is a member of basetype); }
 * 	  | TYPEKIDS { basetype += $1} TYPE { assert( $3 is in basetype ); }
 * 	  ;
 *
 *
 * state0: ; ATYPE
 * 	TYPE '<': ac = args($0), base = CALL state1, state3	
 * 	TYPE '::': base=$0, state3
 * 	else return TYPE
 * state1: ; begin PARGS
 * 	if(ac < list length) punt
 * 	PARG ",": add to list, state1
 * 	PARG ">": add to list, return
 * 	else unravel
 * state3: ; begin TYPEKIDS
 * 	TYPE: 
 */
  
  
#ifdef SPEW_DEBUG    
/* debug_yychar takes a yychar (token number) value and prints its name. */
static int
debug_yychar (yy)
     int yy;
{
  /* In parse.y: */
  extern char *debug_yytranslate ();
  
  int i;
  
  if(yy<256) {
    fprintf (stderr, "<%d: %c >\n", yy, yy);
    return 0;
  }
  fprintf (stderr, "<%d:%s>\n", yy, debug_yytranslate (yy));
  return 1;
}

#endif
