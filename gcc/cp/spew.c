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
#include "c-pragma.h"
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
struct token GTY(())
{
  /* The values for YYCHAR will fit in a short.  */
  short		yychar;
  unsigned int	lineno;
  YYSTYPE GTY ((desc ("%1.yychar"))) yylval;
};

/* Since inline methods can refer to text which has not yet been seen,
   we store the text of the method in a structure which is placed in the
   DECL_PENDING_INLINE_INFO field of the FUNCTION_DECL.
   After parsing the body of the class definition, the FUNCTION_DECL's are
   scanned to see which ones have this field set.  Those are then digested
   one at a time.

   This function's FUNCTION_DECL will have a bit set in its common so
   that we know to watch out for it.  */

#define TOKEN_CHUNK_SIZE 20
struct token_chunk GTY(())
{
  struct token_chunk *next;
  struct token toks[TOKEN_CHUNK_SIZE];
};

struct unparsed_text GTY(())
{
  struct unparsed_text *next;	/* process this one next */
  tree decl;		/* associated declaration */
  location_t locus;     /* location we got the text from */
  int interface;	/* remembering interface_unknown and interface_only */

  struct token_chunk * tokens; /* Start of the token list.  */

  struct token_chunk *last_chunk; /* End of the token list.  */
  short last_pos;	/* Number of tokens used in the last chunk of
			   TOKENS.  */

  short cur_pos;	/* Current token in 'cur_chunk', when rescanning.  */
  struct token_chunk *cur_chunk;  /* Current chunk, when rescanning.  */
};

/* Stack of state saved off when we return to an inline method or
   default argument that has been stored for later parsing.  */
struct feed GTY(())
{
  struct unparsed_text *input;
  location_t locus;
  int yychar;
  YYSTYPE GTY ((desc ("%1.yychar"))) yylval;
  int first_token;
  struct obstack GTY ((skip (""))) token_obstack;
  struct feed *next;
};

static GTY(()) struct feed *feed;

static SPEW_INLINE void do_aggr PARAMS ((void));
static SPEW_INLINE int identifier_type PARAMS ((tree));
static void scan_tokens PARAMS ((int));
static void feed_defarg PARAMS ((tree));
static void finish_defarg PARAMS ((void));
static void yylexstring PARAMS ((struct token *));
static int read_token PARAMS ((struct token *));

static SPEW_INLINE int num_tokens PARAMS ((void));
static SPEW_INLINE struct token *nth_token PARAMS ((int));
static SPEW_INLINE int next_token PARAMS ((struct token *));
static SPEW_INLINE int shift_token PARAMS ((void));
static SPEW_INLINE void push_token PARAMS ((struct token *));
static SPEW_INLINE void consume_token PARAMS ((void));
static SPEW_INLINE int read_process_identifier PARAMS ((YYSTYPE *));

static SPEW_INLINE void feed_input PARAMS ((struct unparsed_text *));
static SPEW_INLINE struct token * space_for_token
  PARAMS ((struct unparsed_text *t));
static SPEW_INLINE struct token * remove_last_token
  PARAMS ((struct unparsed_text *t));
static struct unparsed_text * alloc_unparsed_text
  PARAMS ((const location_t *, tree decl, int interface));

static void snarf_block PARAMS ((struct unparsed_text *t));
static tree snarf_defarg PARAMS ((void));
static void snarf_parenthesized_expression (struct unparsed_text *);
static int frob_id PARAMS ((int, int, tree *));

/* The list of inline functions being held off until we reach the end of
   the current class declaration.  */
static GTY(()) struct unparsed_text *pending_inlines;
static GTY(()) struct unparsed_text *pending_inlines_tail;

/* The list of previously-deferred inline functions currently being parsed.
   This exists solely to be a GC root.  */
static GTY(()) struct unparsed_text *processing_these_inlines;

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
/* the declaration found for the last IDENTIFIER token read in.  yylex
   must look this up to detect typedefs, which get token type
   tTYPENAME, so it is left around in case the identifier is not a
   typedef but is used in a context which makes it a reference to a
   variable.  */
extern tree lastiddecl;		/* let our brains leak out here too */
extern int	yychar;		/*  the lookahead symbol		*/
extern YYSTYPE	yylval;		/*  the semantic value of the		*/
				/*  lookahead symbol			*/
/* The token fifo lives in this obstack.  */
static struct obstack token_obstack;
static int first_token;

/* When we see a default argument in a method declaration, we snarf it as
   text using snarf_defarg.  When we get up to namespace scope, we then go
   through and parse all of them using do_pending_defargs.  Since yacc
   parsers are not reentrant, we retain defargs state in these two
   variables so that subsequent calls to do_pending_defargs can resume
   where the previous call left off. DEFARG_FNS is a tree_list where
   the TREE_TYPE is the current_class_type, TREE_VALUE is the FUNCTION_DECL,
   and TREE_PURPOSE is the list unprocessed dependent functions.  */

/* list of functions with unprocessed defargs */
static GTY(()) tree defarg_fns;
/* current default parameter */
static GTY(()) tree defarg_parm;
/* list of unprocessed fns met during current fn.  */
static GTY(()) tree defarg_depfns;
/* list of fns with circular defargs */
static GTY(()) tree defarg_fnsdone;

/* Initialize obstacks. Called once, from cxx_init.  */

void
init_spew ()
{
  gcc_obstack_init (&token_obstack);
}

/* Subroutine of read_token.  */
static SPEW_INLINE int
read_process_identifier (pyylval)
     YYSTYPE *pyylval;
{
  tree id = pyylval->ttype;

  if (C_IS_RESERVED_WORD (id))
    {
      pyylval->ttype = ridpointers[C_RID_CODE (id)];
      return C_RID_YYCODE (id);
    }

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

/* Concatenate strings before returning them to the parser.  This isn't quite
   as good as having it done in the lexer, but it's better than nothing.  */

static void
yylexstring (t)
     struct token *t;
{
  enum cpp_ttype next_type;
  tree next;

  next_type = c_lex (&next);
  if (next_type == CPP_STRING || next_type == CPP_WSTRING)
    {
      varray_type strings;

      VARRAY_TREE_INIT (strings, 32, "strings");
      VARRAY_PUSH_TREE (strings, t->yylval.ttype);

      do
	{
	  VARRAY_PUSH_TREE (strings, next);
	  next_type = c_lex (&next);
	}
      while (next_type == CPP_STRING || next_type == CPP_WSTRING);

      t->yylval.ttype = combine_strings (strings);
      last_token_id = t->yylval.ttype;
    }

  /* We will have always read one token too many.  */
  _cpp_backup_tokens (parse_in, 1);

  t->yychar = STRING;
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
      yylexstring (t);
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

  f = ggc_alloc (sizeof (struct feed));

  input->cur_chunk = input->tokens;
  input->cur_pos = 0;

#ifdef SPEW_DEBUG
  if (spew_debug)
    fprintf (stderr, "\tfeeding %s:%d [%d tokens]\n",
	     input->locus.file, input->locus.line, input->limit - input->pos);
#endif

  f->input = input;
  f->locus.file = input_filename;
  f->locus.line = lineno;
  f->yychar = yychar;
  f->yylval = yylval;
  f->first_token = first_token;
  f->token_obstack = token_obstack;
  f->next = feed;

  input_filename = input->locus.file;
  lineno = input->locus.line;
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

  input_filename = f->locus.file;
  lineno = f->locus.line;
  yychar = f->yychar;
  yylval = f->yylval;
  first_token = f->first_token;
  obstack_free (&token_obstack, 0);
  token_obstack = f->token_obstack;
  feed = f->next;

#ifdef SPEW_DEBUG
  if (spew_debug)
    fprintf (stderr, "\treturning to %s:%d\n", input_filename, lineno);
#endif
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
next_token (t)
     struct token *t;
{
  if (!feed)
    return read_token (t);

  if (feed->input->cur_chunk != feed->input->last_chunk
      || feed->input->cur_pos != feed->input->last_pos)
    {
      if (feed->input->cur_pos == TOKEN_CHUNK_SIZE)
	{
	  feed->input->cur_chunk = feed->input->cur_chunk->next;
	  feed->input->cur_pos = 0;
	}
      memcpy (t, feed->input->cur_chunk->toks + feed->input->cur_pos,
	      sizeof (struct token));
      feed->input->cur_pos++;
      return t->yychar;
    }

  return 0;
}

/* Shift the next token onto the fifo.  */
static SPEW_INLINE int
shift_token ()
{
  size_t point = obstack_object_size (&token_obstack);
  obstack_blank (&token_obstack, sizeof (struct token));
  return next_token ((struct token *) (obstack_base (&token_obstack) + point));
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
      if (TREE_CODE (decl) == BASELINK)
	decl = BASELINK_FUNCTIONS (decl);

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

  return tTYPENAME;
}

/* token[0] == AGGR (struct/union/enum)
   Thus, token[1] is either a tTYPENAME or a TYPENAME_DEFN.
   If token[2] == '{' or ':' then it's TYPENAME_DEFN.
   It's also a definition if it's a forward declaration (as in 'struct Foo;')
   which we can tell if token[2] == ';' *and* token[-1] != FRIEND or NEW.  */

static SPEW_INLINE void
do_aggr ()
{
  int yc1, yc2;

  scan_tokens (2);
  yc1 = nth_token (1)->yychar;
  if (yc1 != tTYPENAME && yc1 != IDENTIFIER && yc1 != PTYPENAME)
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
    case tTYPENAME:
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
  /* Only types expected, not even namespaces.  */
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
    case tTYPENAME:
    case TYPENAME_DEFN:
    case PTYPENAME:
    case PTYPENAME_DEFN:
      /* If we see a SCOPE next, restore the old value.
	 Otherwise, we got what we want.  */
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
   If RESCAN is nonzero, then we want to `see' this
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
          case tTYPENAME:
          case SELFNAME:
          case NSNAME:
          case PTYPENAME:
	    /* If this got special lookup, remember it.  In these
	       cases, we know it can't be a declarator-id.  */
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

/* Create a new token at the end of the token list in T.  */
static SPEW_INLINE struct token *
space_for_token (t)
     struct unparsed_text *t;
{
  if (t->last_pos != TOKEN_CHUNK_SIZE)
    return t->last_chunk->toks + (t->last_pos++);

  t->last_chunk->next = ggc_alloc_cleared (sizeof (*t->last_chunk->next));
  t->last_chunk = t->last_chunk->next;
  t->last_chunk->next = NULL;

  t->last_pos = 1;
  return t->last_chunk->toks;
}

/* Shrink the token list in T by one token.  */
static SPEW_INLINE struct token *
remove_last_token (t)
     struct unparsed_text *t;
{
  struct token *result = t->last_chunk->toks + t->last_pos - 1;
  if (t->last_pos == 0)
    abort ();
  t->last_pos--;
  if (t->last_pos == 0 && t->last_chunk != t->tokens)
    {
      struct token_chunk *c;
      c = t->tokens;
      while (c->next != t->last_chunk)
	c = c->next;
      c->next = NULL;
      t->last_chunk = c;
      t->last_pos = ARRAY_SIZE (c->toks);
    }
  return result;
}

/* Allocate an 'unparsed_text' structure, ready to use space_for_token.  */
static struct unparsed_text *
alloc_unparsed_text (locus, decl, interface)
     const location_t *locus;
     tree decl;
     int interface;
{
  struct unparsed_text *r;
  r = ggc_alloc_cleared (sizeof (*r));
  r->decl = decl;
  r->locus = *locus;
  r->interface = interface;
  r->tokens = r->last_chunk = ggc_alloc_cleared (sizeof (*r->tokens));
  return r;
}

/* Accumulate the tokens that make up a parenthesized expression in T,
   having already read the opening parenthesis.  */

static void
snarf_parenthesized_expression (struct unparsed_text *t)
{
  int yyc;
  int level = 1;

  while (1)
    {
      yyc = next_token (space_for_token (t));
      if (yyc == '(')
	++level;
      else if (yyc == ')' && --level == 0)
	break;
      else if (yyc == 0)
	{
	  error ("%Hend of file read inside definition", &t->locus);
	  break;
	}
    }
}

/* Subroutine of snarf_method, deals with actual absorption of the block.  */

static void
snarf_block (t)
     struct unparsed_text *t;
{
  int blev = 1;
  int look_for_semicolon = 0;
  int look_for_lbrac = 0;
  int look_for_catch = 0;
  int yyc;
  struct token *current;

  if (yychar == '{')
    ;
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
  current = space_for_token (t);
  current->yychar = yychar;
  current->yylval = yylval;
  current->lineno = lineno;

  for (;;)
    {
      yyc = next_token (space_for_token (t));

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

	      if (next_token (space_for_token (t)) != CATCH)
		{
		  push_token (remove_last_token (t));
		  break;
		}

	      look_for_lbrac = 1;
	    }
	}
      else if (yyc == ';')
	{
	  if (look_for_lbrac)
	    {
	      struct token *fake;

	      error ("function body for constructor missing");
	      /* fake a { } to avoid further errors */
	      fake = space_for_token (t);
	      fake->yylval.ttype = 0;
	      fake->yychar = '{';
	      fake = space_for_token (t);
	      fake->yylval.ttype = 0;
	      fake->yychar = '}';
	      break;
	    }
	  else if (look_for_semicolon && blev == 0)
	    break;
	}
      else if (yyc == '(' && blev == 0)
	snarf_parenthesized_expression (t);
      else if (yyc == 0)
	{
	  error ("%Hend of file read inside definition", &t->locus);
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
  struct unparsed_text *meth;
  location_t starting;
  starting.file = input_filename;
  starting.line = lineno;

  meth = alloc_unparsed_text (&starting, decl, (interface_unknown ? 1
						: (interface_only ? 0 : 2)));

  snarf_block (meth);
  /* Add three END_OF_SAVED_INPUT tokens.  We used to provide an
     infinite stream of END_OF_SAVED_INPUT tokens -- but that can
     cause the compiler to get stuck in an infinite loop when
     encountering invalid code.  We need more than one because the
     parser sometimes peeks ahead several tokens.  */
  memcpy (space_for_token (meth), &Teosi, sizeof (struct token));
  memcpy (space_for_token (meth), &Teosi, sizeof (struct token));
  memcpy (space_for_token (meth), &Teosi, sizeof (struct token));

  /* Happens when we get two declarations of the same function in the
     same scope.  */
  if (decl == void_type_node
      || (current_class_type && TYPE_REDEFINED (current_class_type)))
    return;

#ifdef SPEW_DEBUG
  if (spew_debug)
    fprintf (stderr, "\tsaved method of %d tokens from %s:%d\n",
	     meth->limit, starting.file, starting.line);
#endif

  DECL_PENDING_INLINE_INFO (decl) = meth;
  DECL_PENDING_INLINE_P (decl) = 1;

  /* We need to know that this was defined in the class, so that
     friend templates are handled correctly.  */
  DECL_INITIALIZED_IN_CLASS_P (decl) = 1;

  if (pending_inlines_tail)
    pending_inlines_tail->next = meth;
  else
    pending_inlines = meth;
  pending_inlines_tail = meth;
}

/* Consume a no-commas expression - a default argument - and return
   a DEFAULT_ARG tree node.  */

static tree
snarf_defarg ()
{
  int yyc;
  int plev = 0;
  struct unparsed_text *buf;
  tree arg;
  location_t starting;
  starting.file = input_filename;
  starting.line = lineno;

  buf = alloc_unparsed_text (&starting, 0, 0);

  for (;;)
    {
      yyc = next_token (space_for_token (buf));

      if (plev <= 0 && (yyc == ')' || yyc == ','))
	break;
      else if (yyc == '(' || yyc == '[')
	++plev;
      else if (yyc == ']' || yyc == ')')
	--plev;
      else if (yyc == 0)
	{
	  error ("%Hend of file read inside default argument", &starting);
	  goto done;
	}
    }

  /* Unget the last token.  */
  push_token (remove_last_token (buf));
  /* Add three END_OF_SAVED_INPUT tokens.  We used to provide an
     infinite stream of END_OF_SAVED_INPUT tokens -- but that can
     cause the compiler to get stuck in an infinite loop when
     encountering invalid code.  We need more than one because the
     parser sometimes peeks ahead several tokens.  */
  memcpy (space_for_token (buf), &Teosi, sizeof (struct token));
  memcpy (space_for_token (buf), &Teosi, sizeof (struct token));
  memcpy (space_for_token (buf), &Teosi, sizeof (struct token));

 done:
#ifdef SPEW_DEBUG
  if (spew_debug)
    fprintf (stderr, "\tsaved defarg of %d tokens from %s:%d\n",
	     buf->limit, starting.file, starting.line);
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

              /* Immediately repeat, but marked so that we break the loop.  */
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
   which will be part of a circular dependency.  */
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
  else if (yy == IDENTIFIER || yy == tTYPENAME)
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
      if (yylval.ttype && TREE_CODE (yylval.ttype) == INTEGER_CST)
	{
	  unsigned int val = TREE_INT_CST_LOW (yylval.ttype);
	  const char *const ell = (last_token == CPP_CHAR) ? "" : "L";
	  if (val <= UCHAR_MAX && ISGRAPH (val))
	    error ("%s before %s'%c'", string, ell, val);
	  else
	    error ("%s before %s'\\x%x'", string, ell, val);
	}
      else
	error ("%s", string);
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

#include "gt-cp-spew.h"
