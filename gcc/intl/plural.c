
/*  A Bison parser, made from plural.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define yyparse __gettextparse
#define yylex __gettextlex
#define yyerror __gettexterror
#define yylval __gettextlval
#define yychar __gettextchar
#define yydebug __gettextdebug
#define yynerrs __gettextnerrs
#define	EQUOP2	257
#define	CMPOP2	258
#define	ADDOP2	259
#define	MULOP2	260
#define	NUMBER	261

#line 1 "plural.y"

/* Expression parsing for plural form selection.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Written by Ulrich Drepper <drepper@cygnus.com>, 2000.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License as published
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.  */

/* The bison generated parser uses alloca.  AIX 3 forces us to put this
   declaration at the beginning of the file.  The declaration in bison's
   skeleton file comes too late.  This must come before <config.h>
   because <config.h> may include arbitrary system headers.  */
#if defined _AIX && !defined __GNUC__
 #pragma alloca
#endif

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include "gettextP.h"

/* Names for the libintl functions are a problem.  They must not clash
   with existing names and they should follow ANSI C.  But this source
   code is also used in GNU C Library where the names have a __
   prefix.  So we have to make a difference here.  */
#ifdef _LIBC
# define FREE_EXPRESSION __gettext_free_exp
#else
# define FREE_EXPRESSION gettext_free_exp__
# define __gettextparse gettextparse__
#endif

#define YYLEX_PARAM	&((struct parse_args *) arg)->cp
#define YYPARSE_PARAM	arg

#line 53 "plural.y"
typedef union {
  unsigned long int num;
  enum operator op;
  struct expression *exp;
} YYSTYPE;
#line 59 "plural.y"

/* Prototypes for local functions.  */
static struct expression *new_exp PARAMS ((int nargs, enum operator op,
					   struct expression * const *args));
static inline struct expression *new_exp_0 PARAMS ((enum operator op));
static inline struct expression *new_exp_1 PARAMS ((enum operator op,
						   struct expression *right));
static struct expression *new_exp_2 PARAMS ((enum operator op,
					     struct expression *left,
					     struct expression *right));
static inline struct expression *new_exp_3 PARAMS ((enum operator op,
						   struct expression *bexp,
						   struct expression *tbranch,
						   struct expression *fbranch));
static int yylex PARAMS ((YYSTYPE *lval, const char **pexp));
static void yyerror PARAMS ((const char *str));

/* Allocation of expressions.  */

static struct expression *
new_exp (nargs, op, args)
     int nargs;
     enum operator op;
     struct expression * const *args;
{
  int i;
  struct expression *newp;

  /* If any of the argument could not be malloc'ed, just return NULL.  */
  for (i = nargs - 1; i >= 0; i--)
    if (args[i] == NULL)
      goto fail;

  /* Allocate a new expression.  */
  newp = (struct expression *) malloc (sizeof (*newp));
  if (newp != NULL)
    {
      newp->nargs = nargs;
      newp->operation = op;
      for (i = nargs - 1; i >= 0; i--)
	newp->val.args[i] = args[i];
      return newp;
    }

 fail:
  for (i = nargs - 1; i >= 0; i--)
    FREE_EXPRESSION (args[i]);

  return NULL;
}

static inline struct expression *
new_exp_0 (op)
     enum operator op;
{
  return new_exp (0, op, NULL);
}

static inline struct expression *
new_exp_1 (op, right)
     enum operator op;
     struct expression *right;
{
  struct expression *args[1];

  args[0] = right;
  return new_exp (1, op, args);
}

static struct expression *
new_exp_2 (op, left, right)
     enum operator op;
     struct expression *left;
     struct expression *right;
{
  struct expression *args[2];

  args[0] = left;
  args[1] = right;
  return new_exp (2, op, args);
}

static inline struct expression *
new_exp_3 (op, bexp, tbranch, fbranch)
     enum operator op;
     struct expression *bexp;
     struct expression *tbranch;
     struct expression *fbranch;
{
  struct expression *args[3];

  args[0] = bexp;
  args[1] = tbranch;
  args[2] = fbranch;
  return new_exp (3, op, args);
}

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		27
#define	YYFLAG		-32768
#define	YYNTBASE	16

#define YYTRANSLATE(x) ((unsigned)(x) <= 261 ? yytranslate[x] : 18)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    10,     2,     2,     2,     2,     5,     2,    14,
    15,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    12,     2,     2,
     2,     2,     3,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    13,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     4,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     6,     7,     8,     9,
    11
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     8,    12,    16,    20,    24,    28,    32,    35,
    37,    39
};

static const short yyrhs[] = {    17,
     0,    17,     3,    17,    12,    17,     0,    17,     4,    17,
     0,    17,     5,    17,     0,    17,     6,    17,     0,    17,
     7,    17,     0,    17,     8,    17,     0,    17,     9,    17,
     0,    10,    17,     0,    13,     0,    11,     0,    14,    17,
    15,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   178,   186,   190,   194,   198,   202,   206,   210,   214,   218,
   222,   227
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","'?'","'|'",
"'&'","EQUOP2","CMPOP2","ADDOP2","MULOP2","'!'","NUMBER","':'","'n'","'('","')'",
"start","exp", NULL
};
#endif

static const short yyr1[] = {     0,
    16,    17,    17,    17,    17,    17,    17,    17,    17,    17,
    17,    17
};

static const short yyr2[] = {     0,
     1,     5,     3,     3,     3,     3,     3,     3,     2,     1,
     1,     3
};

static const short yydefact[] = {     0,
     0,    11,    10,     0,     1,     9,     0,     0,     0,     0,
     0,     0,     0,     0,    12,     0,     3,     4,     5,     6,
     7,     8,     0,     2,     0,     0,     0
};

static const short yydefgoto[] = {    25,
     5
};

static const short yypact[] = {    -9,
    -9,-32768,-32768,    -9,    34,-32768,    11,    -9,    -9,    -9,
    -9,    -9,    -9,    -9,-32768,    24,    39,    43,    16,    26,
    -3,-32768,    -9,    34,    21,    53,-32768
};

static const short yypgoto[] = {-32768,
    -1
};


#define	YYLAST		53


static const short yytable[] = {     6,
     1,     2,     7,     3,     4,    14,    16,    17,    18,    19,
    20,    21,    22,     8,     9,    10,    11,    12,    13,    14,
    26,    24,    12,    13,    14,    15,     8,     9,    10,    11,
    12,    13,    14,    13,    14,    23,     8,     9,    10,    11,
    12,    13,    14,    10,    11,    12,    13,    14,    11,    12,
    13,    14,    27
};

static const short yycheck[] = {     1,
    10,    11,     4,    13,    14,     9,     8,     9,    10,    11,
    12,    13,    14,     3,     4,     5,     6,     7,     8,     9,
     0,    23,     7,     8,     9,    15,     3,     4,     5,     6,
     7,     8,     9,     8,     9,    12,     3,     4,     5,     6,
     7,     8,     9,     5,     6,     7,     8,     9,     6,     7,
     8,     9,     0
};
#define YYPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/home/haible/gnu/arch/linuxlibc6/share/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/home/haible/gnu/arch/linuxlibc6/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 179 "plural.y"
{
	    if (yyvsp[0].exp == NULL)
	      YYABORT;
	    ((struct parse_args *) arg)->res = yyvsp[0].exp;
	  ;
    break;}
case 2:
#line 187 "plural.y"
{
	    yyval.exp = new_exp_3 (qmop, yyvsp[-4].exp, yyvsp[-2].exp, yyvsp[0].exp);
	  ;
    break;}
case 3:
#line 191 "plural.y"
{
	    yyval.exp = new_exp_2 (lor, yyvsp[-2].exp, yyvsp[0].exp);
	  ;
    break;}
case 4:
#line 195 "plural.y"
{
	    yyval.exp = new_exp_2 (land, yyvsp[-2].exp, yyvsp[0].exp);
	  ;
    break;}
case 5:
#line 199 "plural.y"
{
	    yyval.exp = new_exp_2 (yyvsp[-1].op, yyvsp[-2].exp, yyvsp[0].exp);
	  ;
    break;}
case 6:
#line 203 "plural.y"
{
	    yyval.exp = new_exp_2 (yyvsp[-1].op, yyvsp[-2].exp, yyvsp[0].exp);
	  ;
    break;}
case 7:
#line 207 "plural.y"
{
	    yyval.exp = new_exp_2 (yyvsp[-1].op, yyvsp[-2].exp, yyvsp[0].exp);
	  ;
    break;}
case 8:
#line 211 "plural.y"
{
	    yyval.exp = new_exp_2 (yyvsp[-1].op, yyvsp[-2].exp, yyvsp[0].exp);
	  ;
    break;}
case 9:
#line 215 "plural.y"
{
	    yyval.exp = new_exp_1 (lnot, yyvsp[0].exp);
	  ;
    break;}
case 10:
#line 219 "plural.y"
{
	    yyval.exp = new_exp_0 (var);
	  ;
    break;}
case 11:
#line 223 "plural.y"
{
	    if ((yyval.exp = new_exp_0 (num)) != NULL)
	      yyval.exp->val.num = yyvsp[0].num;
	  ;
    break;}
case 12:
#line 228 "plural.y"
{
	    yyval.exp = yyvsp[-1].exp;
	  ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/home/haible/gnu/arch/linuxlibc6/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 233 "plural.y"


void
internal_function
FREE_EXPRESSION (exp)
     struct expression *exp;
{
  if (exp == NULL)
    return;

  /* Handle the recursive case.  */
  switch (exp->nargs)
    {
    case 3:
      FREE_EXPRESSION (exp->val.args[2]);
      /* FALLTHROUGH */
    case 2:
      FREE_EXPRESSION (exp->val.args[1]);
      /* FALLTHROUGH */
    case 1:
      FREE_EXPRESSION (exp->val.args[0]);
      /* FALLTHROUGH */
    default:
      break;
    }

  free (exp);
}


static int
yylex (lval, pexp)
     YYSTYPE *lval;
     const char **pexp;
{
  const char *exp = *pexp;
  int result;

  while (1)
    {
      if (exp[0] == '\0')
	{
	  *pexp = exp;
	  return YYEOF;
	}

      if (exp[0] != ' ' && exp[0] != '\t')
	break;

      ++exp;
    }

  result = *exp++;
  switch (result)
    {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      {
	unsigned long int n = result - '0';
	while (exp[0] >= '0' && exp[0] <= '9')
	  {
	    n *= 10;
	    n += exp[0] - '0';
	    ++exp;
	  }
	lval->num = n;
	result = NUMBER;
      }
      break;

    case '=':
      if (exp[0] == '=')
	{
	  ++exp;
	  lval->op = equal;
	  result = EQUOP2;
	}
      else
	result = YYERRCODE;
      break;

    case '!':
      if (exp[0] == '=')
	{
	  ++exp;
	  lval->op = not_equal;
	  result = EQUOP2;
	}
      break;

    case '&':
    case '|':
      if (exp[0] == result)
	++exp;
      else
	result = YYERRCODE;
      break;

    case '<':
      if (exp[0] == '=')
	{
	  ++exp;
	  lval->op = less_or_equal;
	}
      else
	lval->op = less_than;
      result = CMPOP2;
      break;

    case '>':
      if (exp[0] == '=')
	{
	  ++exp;
	  lval->op = greater_or_equal;
	}
      else
	lval->op = greater_than;
      result = CMPOP2;
      break;

    case '*':
      lval->op = mult;
      result = MULOP2;
      break;

    case '/':
      lval->op = divide;
      result = MULOP2;
      break;

    case '%':
      lval->op = module;
      result = MULOP2;
      break;

    case '+':
      lval->op = plus;
      result = ADDOP2;
      break;

    case '-':
      lval->op = minus;
      result = ADDOP2;
      break;

    case 'n':
    case '?':
    case ':':
    case '(':
    case ')':
      /* Nothing, just return the character.  */
      break;

    case ';':
    case '\n':
    case '\0':
      /* Be safe and let the user call this function again.  */
      --exp;
      result = YYEOF;
      break;

    default:
      result = YYERRCODE;
#if YYDEBUG != 0
      --exp;
#endif
      break;
    }

  *pexp = exp;

  return result;
}


static void
yyerror (str)
     const char *str;
{
  /* Do nothing.  We don't print error messages here.  */
}
