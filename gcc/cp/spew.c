/* Type Analyzer for GNU C++.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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
#include "cp-tree.h"
#include "cpplib.h"
#include "c-lex.h"
#include "lex.h"
#include "parse.h"
#include "flags.h"
#include "obstack.h"
#include "toplev.h"
#include "ggc.h"
#include "intl.h"
#include "timevar.h"

#ifdef SPEW_DEBUG
#define SPEW_INLINE
#else
#define SPEW_INLINE inline
#endif

/* This takes a token stream that hasn't decided much about types and
   tries to figure out as much as it can, with excessive lookahead and
   backtracking.  */

/* fifo of tokens recognized and available to parser.  */
struct token
{
  /* The values for YYCHAR will fit in a short.  */
  short		yychar;
  unsigned int	lineno;
  YYSTYPE	yylval;
};

/* Since inline methods can refer to text which has not yet been seen,
   we store the text of the method in a structure which is placed in the
   DECL_PENDING_INLINE_INFO field of the FUNCTION_DECL.
   After parsing the body of the class definition, the FUNCTION_DECL's are
   scanned to see which ones have this field set.  Those are then digested
   one at a time.

   This function's FUNCTION_DECL will have a bit set in its common so
   that we know to watch out for it.  */

struct unparsed_text
{
  struct unparsed_text *next;	/* process this one next */
  tree decl;		/* associated declaration */
  const char *filename;	/* name of file we were processing */
  int lineno;		/* line number we got the text from */
  int interface;	/* remembering interface_unknown and interface_only */

  struct token *pos;	/* current position, when rescanning */
  struct token *limit;	/* end of saved text */
};

/* Stack of state saved off when we return to an inline method or
   default argument that has been stored for later parsing.  */
struct feed
{
  struct unparsed_text *input;
  const char *filename;
  int lineno;
  int yychar;
  YYSTYPE yylval;
  int first_token;
  struct obstack token_obstack;
  struct feed *next;
};  

static struct obstack feed_obstack;
static struct feed *feed;

static SPEW_INLINE void do_aggr PARAMS ((void));
static SPEW_INLINE int identifier_type PARAMS ((tree));
static void scan_tokens PARAMS ((int));
static void feed_defarg PARAMS ((tree));
static void finish_defarg PARAMS ((void));
static int read_token PARAMS ((struct token *));

static SPEW_INLINE int num_tokens PARAMS ((void));
static SPEW_INLINE struct token *nth_token PARAMS ((int));
static SPEW_INLINE int add_token PARAMS ((struct token *));
static SPEW_INLINE int shift_token PARAMS ((void));
static SPEW_INLINE void push_token PARAMS ((struct token *));
static SPEW_INLINE void consume_token PARAMS ((void));
static SPEW_INLINE int read_process_identifier PARAMS ((YYSTYPE *));

static SPEW_INLINE void feed_input PARAMS ((struct unparsed_text *));
static SPEW_INLINE void snarf_block PARAMS ((const char *, int));
static tree snarf_defarg PARAMS ((void));
static int frob_id PARAMS ((int, int, tree *));

/* The list of inline functions being held off until we reach the end of
   the current class declaration.  */
static struct unparsed_text *pending_inlines;
static struct unparsed_text *pending_inlines_tail;

/* The list of previously-deferred inline functions currently being parsed.
   This exists solely to be a GC root.  */
static struct unparsed_text *processing_these_inlines;

static void begin_parsing_inclass_inline PARAMS ((struct unparsed_text *));

#ifdef SPEW_DEBUG
int spew_debug = 0;
static unsigned int yylex_ctr = 0;

static void debug_yychar PARAMS ((int));

/* In parse.y: */
extern char *debug_yytranslate PARAMS ((int));
#endif
static enum cpp_ttype last_token;
static tree last_token_id;

/* From lex.c: */
/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
extern tree lastiddecl;		/* let our brains leak out here too */
extern int	yychar;		/*  the lookahead symbol		*/
extern YYSTYPE	yylval;		/*  the semantic value of the		*/
				/*  lookahead symbol			*/
/* The token fifo lives in this obstack.  */
struct obstack token_obstack;
int first_token;

/* Sometimes we need to save tokens for later parsing.  If so, they are
   stored on this obstack.  */
struct obstack inline_text_obstack;
char *inline_text_firstobj;

/* When we see a default argument in a method declaration, we snarf it as
   text using snarf_defarg.  When we get up to namespace scope, we then go
   through and parse all of them using do_pending_defargs.  Since yacc
   parsers are not reentrant, we retain defargs state in these two
   variables so that subsequent calls to do_pending_defargs can resume
   where the previous call left off. DEFARG_FNS is a tree_list where 
   the TREE_TYPE is the current_class_type, TREE_VALUE is the FUNCTION_DECL,
   and TREE_PURPOSE is the list unprocessed dependent functions.  */

static tree defarg_fns;     /* list of functions with unprocessed defargs */
static tree defarg_parm;    /* current default parameter */
static tree defarg_depfns;  /* list of unprocessed fns met during current fn. */
static tree defarg_fnsdone; /* list of fns with circular defargs */

/* Initialize obstacks. Called once, from cxx_init.  */

void
init_spew ()
{
  gcc_obstack_init (&inline_text_obstack);
  inline_text_firstobj = (char *) obstack_alloc (&inline_text_obstack, 0);
  gcc_obstack_init (&token_obstack);
  gcc_obstack_init (&feed_obstack);
  ggc_add_tree_root (&defarg_fns, 1);
  ggc_add_tree_root (&defarg_parm, 1);
  ggc_add_tree_root (&defarg_depfns, 1);
  ggc_add_tree_root (&defarg_fnsdone, 1);

  ggc_add_root (&pending_inlines, 1, sizeof (struct unparsed_text *),
		mark_pending_inlines);
  ggc_add_root (&processing_these_inlines, 1, sizeof (struct unparsed_text *),
		mark_pending_inlines);
}

void
clear_inline_text_obstack ()
{
  obstack_free (&inline_text_obstack, inline_text_firstobj);
}

/* Subroutine of read_token.  */
static SPEW_INLINE int
read_process_identifier (pyylval)
     YYSTYPE *pyylval;
{
  tree id = pyylval->ttype;

  if (C_IS_RESERVED_WORD (id))
    {
      /* Possibly replace the IDENTIFIER_NODE with a magic cookie.
	 Can't put yylval.code numbers in ridpointers[].  Bleah.  */

      switch (C_RID_CODE (id))
	{
	case RID_BITAND: pyylval->code = BIT_AND_EXPR;	return '&';
	case RID_AND_EQ: pyylval->code = BIT_AND_EXPR;	return ASSIGN;
	case RID_BITOR:	 pyylval->code = BIT_IOR_EXPR;	return '|';
	case RID_OR_EQ:	 pyylval->code = BIT_IOR_EXPR;	return ASSIGN;
	case RID_XOR:	 pyylval->code = BIT_XOR_EXPR;	return '^';
	case RID_XOR_EQ: pyylval->code = BIT_XOR_EXPR;	return ASSIGN;
	case RID_NOT_EQ: pyylval->code = NE_EXPR;	return EQCOMPARE;

	default:
	  if (C_RID_YYCODE (id) == TYPESPEC)
	    GNU_xref_ref (current_function_decl, IDENTIFIER_POINTER (id));

	  pyylval->ttype = ridpointers[C_RID_CODE (id)];
	  return C_RID_YYCODE (id);
	}
    }

  GNU_xref_ref (current_function_decl, IDENTIFIER_POINTER (id));

  /* Make sure that user does not collide with our internal naming
     scheme.  This is not necessary if '.' is used to remove them from
     the user's namespace, but is if '$' or double underscores are.  */

#if !defined(JOINER) || JOINER == '$'
  if (VPTR_NAME_P (id)
      || VTABLE_NAME_P (id)
      || TEMP_NAME_P (id)
      || ANON_AGGRNAME_P (id))
     warning (
"identifier name `%s' conflicts with GNU C++ internal naming strategy",
	      IDENTIFIER_POINTER (id));
#endif
  return IDENTIFIER;
}

/* Read the next token from the input file.  The token is written into
   T, and its type number is returned.  */
static int
read_token (t)
     struct token *t;
{
 retry:

  last_token = c_lex (&last_token_id);
  t->yylval.ttype = last_token_id;

  switch (last_token)
    {
#define YYCHAR(YY)	t->yychar = (YY); break;
#define YYCODE(C)	t->yylval.code = (C);

    case CPP_EQ:				YYCHAR('=');
    case CPP_NOT:				YYCHAR('!');
    case CPP_GREATER:	YYCODE(GT_EXPR);	YYCHAR('>');
    case CPP_LESS:	YYCODE(LT_EXPR);	YYCHAR('<');
    case CPP_PLUS:	YYCODE(PLUS_EXPR);	YYCHAR('+');
    case CPP_MINUS:	YYCODE(MINUS_EXPR);	YYCHAR('-');
    case CPP_MULT:	YYCODE(MULT_EXPR);	YYCHAR('*');
    case CPP_DIV:	YYCODE(TRUNC_DIV_EXPR);	YYCHAR('/');
    case CPP_MOD:	YYCODE(TRUNC_MOD_EXPR);	YYCHAR('%');
    case CPP_AND:	YYCODE(BIT_AND_EXPR);	YYCHAR('&');
    case CPP_OR:	YYCODE(BIT_IOR_EXPR);	YYCHAR('|');
    case CPP_XOR:	YYCODE(BIT_XOR_EXPR);	YYCHAR('^');
    case CPP_RSHIFT:	YYCODE(RSHIFT_EXPR);	YYCHAR(RSHIFT);
    case CPP_LSHIFT:	YYCODE(LSHIFT_EXPR);	YYCHAR(LSHIFT);

    case CPP_COMPL:				YYCHAR('~');
    case CPP_AND_AND:				YYCHAR(ANDAND);
    case CPP_OR_OR:				YYCHAR(OROR);
    case CPP_QUERY:				YYCHAR('?');
    case CPP_COLON:				YYCHAR(':');
    case CPP_COMMA:				YYCHAR(',');
    case CPP_OPEN_PAREN:			YYCHAR('(');
    case CPP_CLOSE_PAREN:			YYCHAR(')');
    case CPP_EQ_EQ:	YYCODE(EQ_EXPR);	YYCHAR(EQCOMPARE);
    case CPP_NOT_EQ:	YYCODE(NE_EXPR);	YYCHAR(EQCOMPARE);
    case CPP_GREATER_EQ:YYCODE(GE_EXPR);	YYCHAR(ARITHCOMPARE);
    case CPP_LESS_EQ:	YYCODE(LE_EXPR);	YYCHAR(ARITHCOMPARE);

    case CPP_PLUS_EQ:	YYCODE(PLUS_EXPR);	YYCHAR(ASSIGN);
    case CPP_MINUS_EQ:	YYCODE(MINUS_EXPR);	YYCHAR(ASSIGN);
    case CPP_MULT_EQ:	YYCODE(MULT_EXPR);	YYCHAR(ASSIGN);
    case CPP_DIV_EQ:	YYCODE(TRUNC_DIV_EXPR);	YYCHAR(ASSIGN);
    case CPP_MOD_EQ:	YYCODE(TRUNC_MOD_EXPR);	YYCHAR(ASSIGN);
    case CPP_AND_EQ:	YYCODE(BIT_AND_EXPR);	YYCHAR(ASSIGN);
    case CPP_OR_EQ:	YYCODE(BIT_IOR_EXPR);	YYCHAR(ASSIGN);
    case CPP_XOR_EQ:	YYCODE(BIT_XOR_EXPR);	YYCHAR(ASSIGN);
    case CPP_RSHIFT_EQ:	YYCODE(RSHIFT_EXPR);	YYCHAR(ASSIGN);
    case CPP_LSHIFT_EQ:	YYCODE(LSHIFT_EXPR);	YYCHAR(ASSIGN);

    case CPP_OPEN_SQUARE:			YYCHAR('[');
    case CPP_CLOSE_SQUARE:			YYCHAR(']');
    case CPP_OPEN_BRACE:			YYCHAR('{');
    case CPP_CLOSE_BRACE:			YYCHAR('}');
    case CPP_SEMICOLON:				YYCHAR(';');
    case CPP_ELLIPSIS:				YYCHAR(ELLIPSIS);

    case CPP_PLUS_PLUS:				YYCHAR(PLUSPLUS);
    case CPP_MINUS_MINUS:			YYCHAR(MINUSMINUS);
    case CPP_DEREF:				YYCHAR(POINTSAT);
    case CPP_DOT:				YYCHAR('.');

    /* These tokens are C++ specific.  */
    case CPP_SCOPE:				YYCHAR(SCOPE);
    case CPP_DEREF_STAR: 			YYCHAR(POINTSAT_STAR);
    case CPP_DOT_STAR:				YYCHAR(DOT_STAR);
    case CPP_MIN_EQ:	YYCODE(MIN_EXPR);	YYCHAR(ASSIGN);
    case CPP_MAX_EQ:	YYCODE(MAX_EXPR);	YYCHAR(ASSIGN);
    case CPP_MIN:	YYCODE(MIN_EXPR);	YYCHAR(MIN_MAX);
    case CPP_MAX:	YYCODE(MAX_EXPR);	YYCHAR(MIN_MAX);
#undef YYCHAR
#undef YYCODE

    case CPP_EOF:
      t->yychar = 0;
      break;
      
    case CPP_NAME:
      t->yychar = read_process_identifier (&t->yylval);
      break;

    case CPP_NUMBER:
    case CPP_CHAR:
    case CPP_WCHAR:
      t->yychar = CONSTANT;
      break;

    case CPP_STRING:
    case CPP_WSTRING:
      t->yychar = STRING;
      break;

    default:
      yyerror ("parse error");
      goto retry;
    }

  t->lineno = lineno;
  return t->yychar;
}

static void
feed_input (input)
     struct unparsed_text *input;
{
  struct feed *f;
#if 0
  if (feed)
    abort ();
#endif

  f = obstack_alloc (&feed_obstack, sizeof (struct feed));

  /* The token list starts just after the struct unparsed_text in memory.  */
  input->pos = (struct token *) (input + 1);

#ifdef SPEW_DEBUG
  if (spew_debug)
    fprintf (stderr, "\tfeeding %s:%d [%d tokens]\n",
	     input->filename, input->lineno, input->limit - input->pos);
#endif

  f->input = input;
  f->filename = input_filename;
  f->lineno = lineno;
  f->yychar = yychar;
  f->yylval = yylval;
  f->first_token = first_token;
  f->token_obstack = token_obstack;
  f->next = feed;

  input_filename = input->filename;
  lineno = input->lineno;
  yychar = YYEMPTY;
  yylval.ttype = NULL_TREE;
  first_token = 0;
  gcc_obstack_init (&token_obstack);
  feed = f;
}

void
end_input ()
{
  struct feed *f = feed;

  input_filename = f->filename;
  lineno = f->lineno;
  yychar = f->yychar;
  yylval = f->yylval;
  first_token = f->first_token;
  obstack_free (&token_obstack, 0);
  token_obstack = f->token_obstack;
  feed = f->next;

  obstack_free (&feed_obstack, f);

#ifdef SPEW_DEBUG
  if (spew_debug)
    fprintf (stderr, "\treturning to %s:%d\n", input_filename, lineno);
#endif
}

/* GC callback to mark memory pointed to by the pending inline queue.  */
void
mark_pending_inlines (pi)
     PTR pi;
{
  struct unparsed_text *up = * (struct unparsed_text **)pi;

  while (up)
    {
      struct token *t = (struct token *) (up + 1);
      struct token *l = up->limit;

      while (t < l)
	{
	  /* Some of the possible values for yychar use yylval.code
	     instead of yylval.ttype.  We only have to worry about
	     yychars that could have been returned by read_token.  */
	  switch (t->yychar)
	    {
	    case '+':	    case '-':	    case '*':	    case '/':
	    case '%':	    case '&':	    case '|':	    case '^':
	    case '>':	    case '<':	    case LSHIFT:    case RSHIFT:
	    case ASSIGN:    case MIN_MAX:   case EQCOMPARE: case ARITHCOMPARE:
	      t++;
	      continue;
	    }
	  if (t->yylval.ttype)
	    ggc_mark_tree (t->yylval.ttype);
	  t++;
	}
      up = up->next;
    }
}
  
/* Token queue management.  */

/* Return the number of tokens available on the fifo.  */
static SPEW_INLINE int
num_tokens ()
{
  return (obstack_object_size (&token_obstack) / sizeof (struct token))
    - first_token;
}

/* Fetch the token N down the line from the head of the fifo.  */

static SPEW_INLINE struct token*
nth_token (n)
     int n;
{
#ifdef ENABLE_CHECKING
  /* could just have this do slurp_ implicitly, but this way is easier
     to debug...  */
  my_friendly_assert (n >= 0 && n < num_tokens (), 298);
#endif
  return ((struct token*)obstack_base (&token_obstack)) + n + first_token;
}

static const struct token Teosi = { END_OF_SAVED_INPUT, 0 UNION_INIT_ZERO };
static const struct token Tpad = { EMPTY, 0 UNION_INIT_ZERO };

/* Copy the next token into T and return its value.  */
static SPEW_INLINE int
add_token (t)
     struct token *t;
{
  if (!feed)
    return read_token (t);

  if (feed->input->pos < feed->input->limit)
    {
      memcpy (t, feed->input->pos, sizeof (struct token));
      return (feed->input->pos++)->yychar;
    }
  
  memcpy (t, &Teosi, sizeof (struct token));
  return END_OF_SAVED_INPUT;
}

/* Shift the next token onto the fifo.  */
static SPEW_INLINE int
shift_token ()
{
  size_t point = obstack_object_size (&token_obstack);
  obstack_blank (&token_obstack, sizeof (struct token));
  return add_token ((struct token *) (obstack_base (&token_obstack) + point));
}

/* Consume the next token out of the fifo.  */

static SPEW_INLINE void
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

/* Push a token at the head of the queue; it will be the next token read.  */
static SPEW_INLINE void
push_token (t)
     struct token *t;
{
  if (first_token == 0)  /* We hope this doesn't happen often.  */
    {
      size_t active = obstack_object_size (&token_obstack);
      obstack_blank (&token_obstack, sizeof (struct token));
      if (active)
	memmove (obstack_base (&token_obstack) + sizeof (struct token),
		 obstack_base (&token_obstack), active);
      first_token++;
    }
  first_token--;
  memcpy (nth_token (0), t, sizeof (struct token));
}


/* Pull in enough tokens that the queue is N long beyond the current
   token.  */

static void
scan_tokens (n)
     int n;
{
  int i;
  int num = num_tokens ();
  int yychar;

  /* First, prune any empty tokens at the end.  */
  i = num;
  while (i > 0 && nth_token (i - 1)->yychar == EMPTY)
    i--;
  if (i < num)
    {
      obstack_blank (&token_obstack, -((num - i) * sizeof (struct token)));
      num = i;
    }

  /* Now, if we already have enough tokens, return.  */
  if (num > n)
    return;

  /* Never read past these characters: they might separate
     the current input stream from one we save away later.  */
  for (i = 0; i < num; i++)
    {
      yychar = nth_token (i)->yychar;
      if (yychar == '{' || yychar == ':' || yychar == ';')
	goto pad_tokens;
    }

  while (num_tokens () <= n)
    {
      yychar = shift_token ();
      if (yychar == '{' || yychar == ':' || yychar == ';')
	goto pad_tokens;
    }
  return;
  
 pad_tokens:
  while (num_tokens () <= n)
    obstack_grow (&token_obstack, &Tpad, sizeof (struct token));
}

int looking_for_typename;
int looking_for_template;

static int after_friend;
static int after_new;
static int do_snarf_defarg;

tree got_scope;
tree got_object;

static SPEW_INLINE int
identifier_type (decl)
     tree decl;
{
  tree t;

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      if (TREE_CODE (DECL_TEMPLATE_RESULT (decl)) == TYPE_DECL)
	return PTYPENAME;
      else if (looking_for_template) 
	return PFUNCNAME;
    }
  if (looking_for_template && really_overloaded_fn (decl))
    {
      /* See through a baselink.  */
      if (TREE_CODE (decl) == TREE_LIST)
	decl = TREE_VALUE (decl);

      for (t = decl; t != NULL_TREE; t = OVL_CHAIN (t))
	if (DECL_FUNCTION_TEMPLATE_P (OVL_FUNCTION (t))) 
	  return PFUNCNAME;
    }
  if (TREE_CODE (decl) == NAMESPACE_DECL)
    return NSNAME;
  if (TREE_CODE (decl) != TYPE_DECL)
    return IDENTIFIER;
  if (DECL_ARTIFICIAL (decl) && TREE_TYPE (decl) == current_class_type)
    return SELFNAME;

  /* A constructor declarator for a template type will get here as an
     implicit typename, a TYPENAME_TYPE with a type.  */
  t = got_scope;
  if (t && TREE_CODE (t) == TYPENAME_TYPE)
    t = TREE_TYPE (t);
  decl = TREE_TYPE (decl);
  if (TREE_CODE (decl) == TYPENAME_TYPE)
    decl = TREE_TYPE (decl);
  if (t && t == decl)
    return SELFNAME;

  return TYPENAME;
}

/* token[0] == AGGR (struct/union/enum)
   Thus, token[1] is either a TYPENAME or a TYPENAME_DEFN.
   If token[2] == '{' or ':' then it's TYPENAME_DEFN.
   It's also a definition if it's a forward declaration (as in 'struct Foo;')
   which we can tell if token[2] == ';' *and* token[-1] != FRIEND or NEW.  */

static SPEW_INLINE void
do_aggr ()
{
  int yc1, yc2;
  
  scan_tokens (2);
  yc1 = nth_token (1)->yychar;
  if (yc1 != TYPENAME && yc1 != IDENTIFIER && yc1 != PTYPENAME)
    return;
  yc2 = nth_token (2)->yychar;
  if (yc2 == ';')
    {
      /* It's a forward declaration iff we were not preceded by
         'friend' or `new'.  */
      if (after_friend || after_new)
	return;
    }
  else if (yc2 != '{' && yc2 != ':')
    return;

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
      abort ();
    }
}  

void
see_typename ()
{
  /* Only types expected, not even namespaces. */
  looking_for_typename = 2;
  if (yychar < 0)
    if ((yychar = yylex ()) < 0) yychar = 0;
  looking_for_typename = 0;
  if (yychar == IDENTIFIER)
    {
      lastiddecl = lookup_name (yylval.ttype, -2);
      if (lastiddecl)
	yychar = identifier_type (lastiddecl);
    }
}

int
yylex ()
{
  int yychr;
  int old_looking_for_typename = 0;
  int just_saw_new = 0;
  int just_saw_friend = 0;

  timevar_push (TV_LEX);

 retry:
#ifdef SPEW_DEBUG
  if (spew_debug)
  {
    yylex_ctr ++;
    fprintf (stderr, "\t\t## %d @%d ", yylex_ctr, lineno);
  }
#endif

  if (do_snarf_defarg)
    {
      do_snarf_defarg = 0;
      yylval.ttype = snarf_defarg ();
      yychar = DEFARG;
      got_object = NULL_TREE;
      timevar_pop (TV_LEX);
      return DEFARG;
    }

  /* if we've got tokens, send them */
  else if (num_tokens ())
    yychr = nth_token (0)->yychar;
  else
    yychr = shift_token ();

  /* many tokens just need to be returned. At first glance, all we
     have to do is send them back up, but some of them are needed to
     figure out local context.  */
  switch (yychr)
    {
    case EMPTY:
      /* This is a lexical no-op.  */
#ifdef SPEW_DEBUG    
      if (spew_debug)
	debug_yychar (yychr);
#endif
      consume_token ();
      goto retry;

    case '(':
      scan_tokens (1);
      if (nth_token (1)->yychar == ')')
	{
	  consume_token ();
	  yychr = LEFT_RIGHT;
	}
      break;

    case IDENTIFIER:
    {
      int peek;
      
      scan_tokens (1);
      peek = nth_token (1)->yychar;
      yychr = frob_id (yychr, peek, &nth_token (0)->yylval.ttype);
      break;
    }
    case IDENTIFIER_DEFN:
    case TYPENAME:
    case TYPENAME_DEFN:
    case PTYPENAME:
    case PTYPENAME_DEFN:
      /* If we see a SCOPE next, restore the old value.
	 Otherwise, we got what we want. */
      looking_for_typename = old_looking_for_typename;
      looking_for_template = 0;
      break;

    case SCSPEC:
      if (nth_token (0)->yylval.ttype == ridpointers[RID_EXTERN])
	{
	  scan_tokens (1);
	  if (nth_token (1)->yychar == STRING)
	    {
	      yychr = EXTERN_LANG_STRING;
	      nth_token (1)->yylval.ttype = get_identifier
		(TREE_STRING_POINTER (nth_token (1)->yylval.ttype));
	      consume_token ();
	    }
	}
      /* do_aggr needs to know if the previous token was `friend'.  */
      else if (nth_token (0)->yylval.ttype == ridpointers[RID_FRIEND])
	just_saw_friend = 1;

      break;

    case NEW:
      /* do_aggr needs to know if the previous token was `new'.  */
      just_saw_new = 1;
      break;

    case TYPESPEC:
    case '{':
    case ':':
    case ';':
      /* If this provides a type for us, then revert lexical
	 state to standard state.  */
      looking_for_typename = 0;
      break;

    case AGGR:
      do_aggr ();
      break;

    case ENUM:
      /* Set this again, in case we are rescanning.  */
      looking_for_typename = 2;
      break;

    default:
      break;
    }

  after_friend = just_saw_friend;
  after_new = just_saw_new;

  /* class member lookup only applies to the first token after the object
     expression, except for explicit destructor calls.  */
  if (yychr != '~')
    got_object = NULL_TREE;

  yychar = yychr;
  {
    struct token *tok = nth_token (0);
    
    yylval = tok->yylval;
    if (tok->lineno)
      lineno = tok->lineno;
  }

#ifdef SPEW_DEBUG    
  if (spew_debug)
    debug_yychar (yychr);
#endif
  consume_token ();

  timevar_pop (TV_LEX);
  return yychr;
}

/* Unget character CH from the input stream.
   If RESCAN is non-zero, then we want to `see' this
   character as the next input token.  */

void
yyungetc (ch, rescan)
     int ch;
     int rescan;
{
  /* Unget a character from the input stream.  */
  if (yychar == YYEMPTY || rescan == 0)
    {
      struct token fake;

      /* If we're putting back a brace, undo the change in indent_level
	 from the first time we saw it.  */
      if (ch == '{')
	indent_level--;
      else if (ch == '}')
	indent_level++;

      fake.yychar = ch;
      fake.yylval.ttype = 0;
      fake.lineno = lineno;

      push_token (&fake);
    }
  else
    {
      yychar = ch;
    }
}

/* Lexer hackery to determine what *IDP really is.  */

static int
frob_id (yyc, peek, idp)
     int yyc;
     int peek;
     tree *idp;
{
  tree trrr;
  int old_looking_for_typename = 0;
  
  if (peek == SCOPE)
    {
      /* Don't interfere with the setting from an 'aggr' prefix.  */
      old_looking_for_typename = looking_for_typename;
      looking_for_typename = 1;
    }
  else if (peek == '<')
    looking_for_template = 1;
  trrr = lookup_name (*idp, -2);
  if (trrr)
    {
      yyc = identifier_type (trrr);
      switch(yyc)
        {
          case TYPENAME:
          case SELFNAME:
          case NSNAME:
          case PTYPENAME:
	    /* If this got special lookup, remember it.  In these
	       cases, we know it can't be a declarator-id. */
            if (got_scope || got_object)
              *idp = trrr;
            /* FALLTHROUGH */
          case PFUNCNAME:
          case IDENTIFIER:
            lastiddecl = trrr;
            break;
          default:
            abort ();
        }
    }
  else
    lastiddecl = NULL_TREE;
  got_scope = NULL_TREE;
  looking_for_typename = old_looking_for_typename;
  looking_for_template = 0;
  return yyc;
}

/* ID is an operator name. Duplicate the hackery in yylex to determine what
   it really is.  */

tree frob_opname (id)
     tree id;
{
  scan_tokens (0);
  frob_id (0, nth_token (0)->yychar, &id);
  got_object = NULL_TREE;
  return id;
}

/* Set up the state required to correctly handle the definition of the
   inline function whose preparsed state has been saved in PI.  */

static void
begin_parsing_inclass_inline (pi)
     struct unparsed_text *pi;
{
  tree context;

  /* Record that we are processing the chain of inlines starting at
     PI for GC.  */
  if (cfun)
    cp_function_chain->unparsed_inlines = pi;
  else
    processing_these_inlines = pi;

  ggc_collect ();

  /* If this is an inline function in a local class, we must make sure
     that we save all pertinent information about the function
     surrounding the local class.  */
  context = decl_function_context (pi->decl);
  if (context)
    push_function_context_to (context);

  feed_input (pi);
  interface_unknown = pi->interface == 1;
  interface_only  = pi->interface == 0;
  DECL_PENDING_INLINE_P (pi->decl) = 0;
  DECL_PENDING_INLINE_INFO (pi->decl) = 0;

  /* Pass back a handle to the rest of the inline functions, so that they
     can be processed later.  */
  yychar = PRE_PARSED_FUNCTION_DECL;
  yylval.pi = pi;

  start_function (NULL_TREE, pi->decl, NULL_TREE,
		  (SF_DEFAULT | SF_PRE_PARSED | SF_INCLASS_INLINE));
}

/* Called from the top level: if there are any pending inlines to
   do, set up to process them now.  This function sets up the first function
   to be parsed; after it has been, the rule for fndef in parse.y will
   call process_next_inline to start working on the next one.  */

void
do_pending_inlines ()
{
  /* Oops, we're still dealing with the last batch.  */
  if (yychar == PRE_PARSED_FUNCTION_DECL)
    return;

  if (pending_inlines)
    {
      /* Clear the chain, so that any inlines nested inside the batch
	 we're to process now don't refer to this batch.  See e.g.
	 g++.other/lookup6.C.  */
      struct unparsed_text *first = pending_inlines;
      pending_inlines = pending_inlines_tail = 0;

      begin_parsing_inclass_inline (first);
    }
}

/* Called from the fndecl rule in the parser when the function just parsed
   was declared using a PRE_PARSED_FUNCTION_DECL (i.e. came from
   do_pending_inlines).  */

void
process_next_inline (i)
     struct unparsed_text *i;
{
  tree decl = i->decl;
  tree context = decl_function_context (decl);

  if (context)
    pop_function_context_from (context);
  if (yychar == YYEMPTY)
    yychar = yylex ();
  if (yychar != END_OF_SAVED_INPUT)
    error ("parse error at end of saved function text");
  end_input ();

  i = i->next;
  if (i)
    begin_parsing_inclass_inline (i);
  else
    {
      if (cfun)
	cp_function_chain->unparsed_inlines = 0;
      else
	processing_these_inlines = 0;
      extract_interface_info ();
    }
}


/* Subroutine of snarf_method, deals with actual absorption of the block.  */

static SPEW_INLINE void
snarf_block (starting_file, starting_line)
     const char *starting_file;
     int starting_line;
{
  int blev = 1;
  int look_for_semicolon = 0;
  int look_for_lbrac = 0;
  int look_for_catch = 0;
  int yyc;
  struct token tmp;
  size_t point;

  if (yychar == '{')
    /* We incremented indent_level in yylex; undo that.  */
    indent_level--;
  else if (yychar == '=')
    look_for_semicolon = 1;
  else if (yychar == ':' || yychar == RETURN_KEYWORD || yychar == TRY)
    {
      if (yychar == TRY)
	look_for_catch = 1;
      look_for_lbrac = 1;
      blev = 0;
    }
  else
    yyerror ("parse error in method specification");

  /* The current token is the first one to be recorded.  */
  tmp.yychar = yychar;
  tmp.yylval = yylval;
  tmp.lineno = lineno;
  obstack_grow (&inline_text_obstack, &tmp, sizeof (struct token));

  for (;;)
    {
      point = obstack_object_size (&inline_text_obstack);
      obstack_blank (&inline_text_obstack, sizeof (struct token));
      yyc = add_token ((struct token *)
		       (obstack_base (&inline_text_obstack) + point));

      if (yyc == '{')
	{
	  look_for_lbrac = 0;
	  blev++;
	}
      else if (yyc == '}')
	{
	  blev--;
	  if (blev == 0 && !look_for_semicolon)
	    {
	      if (!look_for_catch)
		break;
	      
	      if (add_token (&tmp) != CATCH)
		{
		  push_token (&tmp);
		  break;
		}

	      look_for_lbrac = 1;
	      obstack_grow (&inline_text_obstack, &tmp, sizeof (struct token));
	    }
	}
      else if (yyc == ';')
	{
	  if (look_for_lbrac)
	    {
	      error ("function body for constructor missing");
	      /* fake a { } to avoid further errors */
	      tmp.yylval.ttype = 0;
	      tmp.yychar = '{';
	      obstack_grow (&inline_text_obstack, &tmp, sizeof (struct token));
	      tmp.yychar = '}';
	      obstack_grow (&inline_text_obstack, &tmp, sizeof (struct token));
	      break;
	    }
	  else if (look_for_semicolon && blev == 0)
	    break;
	}
      else if (yyc == 0)
	{
	  error_with_file_and_line (starting_file, starting_line,
				    "end of file read inside definition");
	  break;
	}
    }
}

/* This function stores away the text for an inline function that should
   be processed later (by do_pending_inlines).  */
void
snarf_method (decl)
     tree decl;
{
  int starting_lineno = lineno;
  const char *starting_filename = input_filename;
  size_t len;

  struct unparsed_text *meth;

  /* Leave room for the header, then absorb the block.  */
  obstack_blank (&inline_text_obstack, sizeof (struct unparsed_text));
  snarf_block (starting_filename, starting_lineno);

  len = obstack_object_size (&inline_text_obstack);
  meth = (struct unparsed_text *) obstack_finish (&inline_text_obstack);

  /* Happens when we get two declarations of the same function in the
     same scope.  */
  if (decl == void_type_node
      || (current_class_type && TYPE_REDEFINED (current_class_type)))
    {
      obstack_free (&inline_text_obstack, (char *)meth);
      return;
    }

  meth->decl = decl;
  meth->filename = starting_filename;
  meth->lineno = starting_lineno;
  meth->limit = (struct token *) ((char *)meth + len);
  meth->interface = (interface_unknown ? 1 : (interface_only ? 0 : 2));
  meth->next = 0;

#ifdef SPEW_DEBUG
  if (spew_debug)
    fprintf (stderr, "\tsaved method of %d tokens from %s:%d\n",
	     meth->limit - (struct token *) (meth + 1),
	     starting_filename, starting_lineno);
#endif

  DECL_PENDING_INLINE_INFO (decl) = meth;
  DECL_PENDING_INLINE_P (decl) = 1;

  if (pending_inlines_tail)
    pending_inlines_tail->next = meth;
  else
    pending_inlines = meth;
  pending_inlines_tail = meth;
}

/* Consume a no-commas expression - a default argument - and save it
   on the inline_text_obstack.  */

static tree
snarf_defarg ()
{
  int starting_lineno = lineno;
  const char *starting_filename = input_filename;
  int yyc;
  int plev = 0;
  size_t point;
  size_t len;
  struct unparsed_text *buf;
  tree arg;

  obstack_blank (&inline_text_obstack, sizeof (struct unparsed_text));

  for (;;)
    {
      point = obstack_object_size (&inline_text_obstack);
      obstack_blank (&inline_text_obstack, sizeof (struct token));
      yyc = add_token ((struct token *)
		       (obstack_base (&inline_text_obstack) + point));

      if (plev <= 0 && (yyc == ')' || yyc == ','))
	break;
      else if (yyc == '(' || yyc == '[')
	++plev;
      else if (yyc == ']' || yyc == ')')
	--plev;
      else if (yyc == 0)
	{
	  error_with_file_and_line (starting_filename, starting_lineno,
				    "end of file read inside default argument");
	  goto done;
	}
    }

  /* Unget the last token.  */
  push_token ((struct token *) (obstack_base (&inline_text_obstack) + point));
  /* This is the documented way to shrink a growing obstack block.  */
  obstack_blank (&inline_text_obstack, - (int) sizeof (struct token));

 done:
  len = obstack_object_size (&inline_text_obstack);
  buf = (struct unparsed_text *) obstack_finish (&inline_text_obstack);

  buf->decl = 0;
  buf->filename = starting_filename;
  buf->lineno = starting_lineno;
  buf->limit = (struct token *) ((char *)buf + len);
  buf->next = 0;
  
#ifdef SPEW_DEBUG
  if (spew_debug)
    fprintf (stderr, "\tsaved defarg of %d tokens from %s:%d\n",
	     buf->limit - (struct token *) (buf + 1),
	     starting_filename, starting_lineno);
#endif

  arg = make_node (DEFAULT_ARG);
  DEFARG_POINTER (arg) = (char *)buf;

  return arg;
}

/* Decide whether the default argument we are about to see should be
   gobbled up as text for later parsing.  */

void
maybe_snarf_defarg ()
{
  if (current_class_type && TYPE_BEING_DEFINED (current_class_type))
    do_snarf_defarg = 1;
}

/* Called from grokfndecl to note a function decl with unparsed default
   arguments for later processing.  Also called from grokdeclarator
   for function types with unparsed defargs; the call from grokfndecl
   will always come second, so we can overwrite the entry from the type.  */

void
add_defarg_fn (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    TREE_VALUE (defarg_fns) = decl;
  else
    {
      defarg_fns = tree_cons (NULL_TREE, decl, defarg_fns);  
      TREE_TYPE (defarg_fns) = current_class_type;
    }
}

/* Helper for do_pending_defargs.  Starts the parsing of a default arg.  */

static void
feed_defarg (p)
     tree p;
{
  tree d = TREE_PURPOSE (p);

  feed_input ((struct unparsed_text *)DEFARG_POINTER (d));
  yychar = DEFARG_MARKER;
  yylval.ttype = p;
}

/* Helper for do_pending_defargs.  Ends the parsing of a default arg.  */

static void
finish_defarg ()
{
  if (yychar == YYEMPTY)
    yychar = yylex ();
  if (yychar != END_OF_SAVED_INPUT)
    error ("parse error at end of saved function text");

  end_input ();
}  

/* Main function for deferred parsing of default arguments.  Called from
   the parser.  */

void
do_pending_defargs ()
{
  if (defarg_parm)
    finish_defarg ();

  for (; defarg_fns;)
    {
      tree current = defarg_fns;
      
      tree defarg_fn = TREE_VALUE (defarg_fns);
      if (defarg_parm == NULL_TREE)
	{
	  push_nested_class (TREE_TYPE (defarg_fns), 1);
	  pushlevel (0);
	  if (TREE_CODE (defarg_fn) == FUNCTION_DECL)
	    maybe_begin_member_template_processing (defarg_fn);

	  if (TREE_CODE (defarg_fn) == FUNCTION_DECL)
	    defarg_parm = TYPE_ARG_TYPES (TREE_TYPE (defarg_fn));
	  else
	    defarg_parm = TYPE_ARG_TYPES (defarg_fn);
	}
      else
	defarg_parm = TREE_CHAIN (defarg_parm);

      for (; defarg_parm; defarg_parm = TREE_CHAIN (defarg_parm))
	if (!TREE_PURPOSE (defarg_parm)
	    || TREE_CODE (TREE_PURPOSE (defarg_parm)) != DEFAULT_ARG)
	  ;/* OK */
	else if (TREE_PURPOSE (current) == error_mark_node)
	  DEFARG_POINTER (TREE_PURPOSE (defarg_parm)) = NULL;
	else
	  {
	    feed_defarg (defarg_parm);

	    /* Return to the parser, which will process this defarg
	       and call us again.  */
	    return;
	  }

      if (TREE_CODE (defarg_fn) == FUNCTION_DECL)
	{
	  maybe_end_member_template_processing ();
	  check_default_args (defarg_fn);
	}

      poplevel (0, 0, 0);
      pop_nested_class ();
      
      defarg_fns = TREE_CHAIN (defarg_fns);
      if (defarg_depfns)
        {
          /* This function's default args depend on unprocessed default args
             of defarg_fns. We will need to reprocess this function, and
             check for circular dependencies.  */
          tree a, b;
          
          for (a = defarg_depfns, b = TREE_PURPOSE (current); a && b; 
               a = TREE_CHAIN (a), b = TREE_CHAIN (b))
            if (TREE_VALUE (a) != TREE_VALUE (b))
              goto different;
          if (a || b)
            {
            different:;
              TREE_CHAIN (current) = NULL_TREE;
              defarg_fns = chainon (defarg_fns, current);
              TREE_PURPOSE (current) = defarg_depfns;
            }
          else
            {
              cp_warning_at ("circular dependency in default args of `%#D'", defarg_fn);
              /* No need to say what else is dependent, as they will be
                 picked up in another pass.  */
              
              /* Immediately repeat, but marked so that we break the loop. */
              defarg_fns = current;
              TREE_PURPOSE (current) = error_mark_node;
            }
          defarg_depfns = NULL_TREE;
        }
      else if (TREE_PURPOSE (current) == error_mark_node)
        defarg_fnsdone = tree_cons (NULL_TREE, defarg_fn, defarg_fnsdone);
    }
}

/* After parsing all the default arguments, we must clear any that remain,
   which will be part of a circular dependency. */
void
done_pending_defargs ()
{
  for (; defarg_fnsdone; defarg_fnsdone = TREE_CHAIN (defarg_fnsdone))
    {
      tree fn = TREE_VALUE (defarg_fnsdone);
      tree parms;
      
      if (TREE_CODE (fn) == FUNCTION_DECL)
        parms = TYPE_ARG_TYPES (TREE_TYPE (fn));
      else
        parms = TYPE_ARG_TYPES (fn);
      for (; parms; parms = TREE_CHAIN (parms))
	if (TREE_PURPOSE (parms)
	    && TREE_CODE (TREE_PURPOSE (parms)) == DEFAULT_ARG)
	  {
            my_friendly_assert (!DEFARG_POINTER (TREE_PURPOSE (parms)), 20010107);
	    TREE_PURPOSE (parms) = NULL_TREE;
	  }
    }
}

/* In processing the current default arg, we called FN, but that call
   required a default argument of FN, and that had not yet been processed.
   Remember FN.  */

void
unprocessed_defarg_fn (fn)
     tree fn;
{
  defarg_depfns = tree_cons (NULL_TREE, fn, defarg_depfns);
}

/* Called from the parser to update an element of TYPE_ARG_TYPES for some
   FUNCTION_TYPE with the newly parsed version of its default argument, which
   was previously digested as text.  */

void
replace_defarg (arg, init)
     tree arg, init;
{
  if (init == error_mark_node)
    TREE_PURPOSE (arg) = error_mark_node;
  else
    {
      if (! processing_template_decl
          && ! can_convert_arg (TREE_VALUE (arg), TREE_TYPE (init), init))
        pedwarn ("invalid type `%T' for default argument to `%T'",
  	    	    TREE_TYPE (init), TREE_VALUE (arg));
      if (!defarg_depfns)
        TREE_PURPOSE (arg) = init;
    }
}

#ifdef SPEW_DEBUG    
/* debug_yychar takes a yychar (token number) value and prints its name.  */

static void
debug_yychar (yy)
     int yy;
{
  if (yy<256)
    fprintf (stderr, "->%d < %c >\n", lineno, yy);
  else if (yy == IDENTIFIER || yy == TYPENAME)
    {
      const char *id;
      if (TREE_CODE (yylval.ttype) == IDENTIFIER_NODE)
	id = IDENTIFIER_POINTER (yylval.ttype);
      else if (TREE_CODE_CLASS (TREE_CODE (yylval.ttype)) == 'd')
	id = IDENTIFIER_POINTER (DECL_NAME (yylval.ttype));
      else
	id = "";
      fprintf (stderr, "->%d <%s `%s'>\n", lineno, debug_yytranslate (yy), id);
    }
  else
    fprintf (stderr, "->%d <%s>\n", lineno, debug_yytranslate (yy));
}

#endif

#define NAME(TYPE) cpp_type2name (TYPE)

void
yyerror (msgid)
     const char *msgid;
{
  const char *string = _(msgid);

  if (last_token == CPP_EOF)
    error ("%s at end of input", string);
  else if (last_token == CPP_CHAR || last_token == CPP_WCHAR)
    {
      unsigned int val = TREE_INT_CST_LOW (yylval.ttype);
      const char *const ell = (last_token == CPP_CHAR) ? "" : "L";
      if (val <= UCHAR_MAX && ISGRAPH (val))
	error ("%s before %s'%c'", string, ell, val);
      else
	error ("%s before %s'\\x%x'", string, ell, val);
    }
  else if (last_token == CPP_STRING
	   || last_token == CPP_WSTRING)
    error ("%s before string constant", string);
  else if (last_token == CPP_NUMBER)
    error ("%s before numeric constant", string);
  else if (last_token == CPP_NAME)
    {
      if (TREE_CODE (last_token_id) == IDENTIFIER_NODE)
        error ("%s before `%s'", string, IDENTIFIER_POINTER (last_token_id));
      else if (ISGRAPH (yychar))
        error ("%s before `%c'", string, yychar);
      else
	error ("%s before `\%o'", string, yychar);
    }
  else
    error ("%s before `%s' token", string, NAME (last_token));
}
