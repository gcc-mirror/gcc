
/*  A Bison parser, made from tradcif.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	INT	257
#define	CHAR	258
#define	NAME	259
#define	ERROR	260
#define	OR	261
#define	AND	262
#define	EQUAL	263
#define	NOTEQUAL	264
#define	LEQ	265
#define	GEQ	266
#define	LSH	267
#define	RSH	268
#define	UNARY	269

#line 26 "tradcif.y"

#include "config.h"
#include "system.h"
#include <setjmp.h>

  int yylex PARAMS ((void));
  void yyerror PARAMS ((const char *msgid));
  extern void error   PARAMS ((const char *msgid, ...));
  extern void warning PARAMS ((const char *msgid, ...));
  extern struct hashnode *lookup PARAMS ((const unsigned char *, int, int));

  int parse_number PARAMS ((int));
  int parse_escape PARAMS ((char **));
  int parse_c_expression PARAMS ((char *));

  int expression_value;
  static jmp_buf parse_return_error;

  /* some external tables of character types */
  extern unsigned char is_idstart[], is_idchar[];

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#line 52 "tradcif.y"
typedef union {
  struct constant {long value; int unsignedp;} integer;
  int voidval;
  char *sval;
} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		61
#define	YYFLAG		-32768
#define	YYNTBASE	33

#define YYTRANSLATE(x) ((unsigned)(x) <= 269 ? yytranslate[x] : 36)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    29,     2,     2,     2,    27,    14,     2,    31,
    32,    25,    23,     9,    24,     2,    26,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     8,     2,    17,
     2,    18,     7,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,    13,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,    12,     2,    30,     2,     2,     2,     2,
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
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
    10,    11,    15,    16,    19,    20,    21,    22,    28
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     8,    11,    14,    17,    20,    24,    28,
    32,    36,    40,    44,    48,    52,    56,    60,    64,    68,
    72,    76,    80,    84,    88,    92,    96,   102,   104,   106
};

static const short yyrhs[] = {    34,
     0,    35,     0,    34,     9,    35,     0,    24,    35,     0,
    29,    35,     0,    23,    35,     0,    30,    35,     0,    31,
    34,    32,     0,    35,    25,    35,     0,    35,    26,    35,
     0,    35,    27,    35,     0,    35,    23,    35,     0,    35,
    24,    35,     0,    35,    21,    35,     0,    35,    22,    35,
     0,    35,    15,    35,     0,    35,    16,    35,     0,    35,
    19,    35,     0,    35,    20,    35,     0,    35,    17,    35,
     0,    35,    18,    35,     0,    35,    14,    35,     0,    35,
    13,    35,     0,    35,    12,    35,     0,    35,    11,    35,
     0,    35,    10,    35,     0,    35,     7,    35,     8,    35,
     0,     3,     0,     4,     0,     5,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    81,    86,    87,    92,    95,    98,   100,   103,   108,   114,
   125,   136,   139,   142,   148,   154,   157,   160,   167,   174,
   181,   188,   191,   194,   197,   200,   203,   206,   208,   210
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","INT","CHAR",
"NAME","ERROR","'?'","':'","','","OR","AND","'|'","'^'","'&'","EQUAL","NOTEQUAL",
"'<'","'>'","LEQ","GEQ","LSH","RSH","'+'","'-'","'*'","'/'","'%'","UNARY","'!'",
"'~'","'('","')'","start","exp1","exp", NULL
};
#endif

static const short yyr1[] = {     0,
    33,    34,    34,    35,    35,    35,    35,    35,    35,    35,
    35,    35,    35,    35,    35,    35,    35,    35,    35,    35,
    35,    35,    35,    35,    35,    35,    35,    35,    35,    35
};

static const short yyr2[] = {     0,
     1,     1,     3,     2,     2,     2,     2,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     5,     1,     1,     1
};

static const short yydefact[] = {     0,
    28,    29,    30,     0,     0,     0,     0,     0,     1,     2,
     6,     4,     5,     7,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     8,     3,     0,    26,    25,
    24,    23,    22,    16,    17,    20,    21,    18,    19,    14,
    15,    12,    13,     9,    10,    11,     0,    27,     0,     0,
     0
};

static const short yydefgoto[] = {    59,
     9,    10
};

static const short yypact[] = {    31,
-32768,-32768,-32768,    31,    31,    31,    31,    31,     1,    77,
-32768,-32768,-32768,-32768,     0,    31,    31,    31,    31,    31,
    31,    31,    31,    31,    31,    31,    31,    31,    31,    31,
    31,    31,    31,    31,    31,-32768,    77,    56,    94,    25,
   109,   123,   136,   147,   147,   154,   154,   154,   154,   -19,
   -19,    32,    32,-32768,-32768,-32768,    31,    77,    11,    33,
-32768
};

static const short yypgoto[] = {-32768,
    48,    -4
};


#define	YYLAST		181


static const short yytable[] = {    11,
    12,    13,    14,    31,    32,    33,    34,    35,    16,    16,
    60,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
    56,    36,    61,     1,     2,     3,    20,    21,    22,    23,
    24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
    34,    35,    58,     4,     5,    15,    33,    34,    35,     6,
     7,     8,    17,    57,     0,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
    33,    34,    35,    17,     0,     0,    18,    19,    20,    21,
    22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
    32,    33,    34,    35,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
    35,    21,    22,    23,    24,    25,    26,    27,    28,    29,
    30,    31,    32,    33,    34,    35,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
    33,    34,    35,    25,    26,    27,    28,    29,    30,    31,
    32,    33,    34,    35,    29,    30,    31,    32,    33,    34,
    35
};

static const short yycheck[] = {     4,
     5,     6,     7,    23,    24,    25,    26,    27,     9,     9,
     0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
    35,    32,     0,     3,     4,     5,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    57,    23,    24,     8,    25,    26,    27,    29,
    30,    31,     7,     8,    -1,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,     7,    -1,    -1,    10,    11,    12,    13,
    14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    27,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,    26,    27,    14,    15,    16,    17,
    18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
    15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
    25,    26,    27,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    27,    21,    22,    23,    24,    25,    26,
    27
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/misc/bison.simple"
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

#line 217 "/usr/share/misc/bison.simple"

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
#line 82 "tradcif.y"
{ expression_value = yyvsp[0].integer.value; ;
    break;}
case 3:
#line 88 "tradcif.y"
{ yyval.integer = yyvsp[0].integer; ;
    break;}
case 4:
#line 93 "tradcif.y"
{ yyval.integer.value = - yyvsp[0].integer.value;
			  yyval.integer.unsignedp = yyvsp[0].integer.unsignedp; ;
    break;}
case 5:
#line 96 "tradcif.y"
{ yyval.integer.value = ! yyvsp[0].integer.value;
			  yyval.integer.unsignedp = 0; ;
    break;}
case 6:
#line 99 "tradcif.y"
{ yyval.integer = yyvsp[0].integer; ;
    break;}
case 7:
#line 101 "tradcif.y"
{ yyval.integer.value = ~ yyvsp[0].integer.value;
			  yyval.integer.unsignedp = yyvsp[0].integer.unsignedp; ;
    break;}
case 8:
#line 104 "tradcif.y"
{ yyval.integer = yyvsp[-1].integer; ;
    break;}
case 9:
#line 109 "tradcif.y"
{ yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp;
			  if (yyval.integer.unsignedp)
			    yyval.integer.value = (unsigned) yyvsp[-2].integer.value * yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value * yyvsp[0].integer.value; ;
    break;}
case 10:
#line 115 "tradcif.y"
{ if (yyvsp[0].integer.value == 0)
			    {
			      error ("division by zero in #if");
			      yyvsp[0].integer.value = 1;
			    }
			  yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp;
			  if (yyval.integer.unsignedp)
			    yyval.integer.value = (unsigned) yyvsp[-2].integer.value / yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value / yyvsp[0].integer.value; ;
    break;}
case 11:
#line 126 "tradcif.y"
{ if (yyvsp[0].integer.value == 0)
			    {
			      error ("division by zero in #if");
			      yyvsp[0].integer.value = 1;
			    }
			  yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp;
			  if (yyval.integer.unsignedp)
			    yyval.integer.value = (unsigned) yyvsp[-2].integer.value % yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value % yyvsp[0].integer.value; ;
    break;}
case 12:
#line 137 "tradcif.y"
{ yyval.integer.value = yyvsp[-2].integer.value + yyvsp[0].integer.value;
			  yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp; ;
    break;}
case 13:
#line 140 "tradcif.y"
{ yyval.integer.value = yyvsp[-2].integer.value - yyvsp[0].integer.value;
			  yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp; ;
    break;}
case 14:
#line 143 "tradcif.y"
{ yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp;
			  if (yyval.integer.unsignedp)
			    yyval.integer.value = (unsigned) yyvsp[-2].integer.value << yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value << yyvsp[0].integer.value; ;
    break;}
case 15:
#line 149 "tradcif.y"
{ yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp;
			  if (yyval.integer.unsignedp)
			    yyval.integer.value = (unsigned) yyvsp[-2].integer.value >> yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value >> yyvsp[0].integer.value; ;
    break;}
case 16:
#line 155 "tradcif.y"
{ yyval.integer.value = (yyvsp[-2].integer.value == yyvsp[0].integer.value);
			  yyval.integer.unsignedp = 0; ;
    break;}
case 17:
#line 158 "tradcif.y"
{ yyval.integer.value = (yyvsp[-2].integer.value != yyvsp[0].integer.value);
			  yyval.integer.unsignedp = 0; ;
    break;}
case 18:
#line 161 "tradcif.y"
{ yyval.integer.unsignedp = 0;
			  if (yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp)
			    yyval.integer.value =
			      (unsigned) yyvsp[-2].integer.value <= (unsigned) yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value <= yyvsp[0].integer.value; ;
    break;}
case 19:
#line 168 "tradcif.y"
{ yyval.integer.unsignedp = 0;
			  if (yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp)
			    yyval.integer.value =
			      (unsigned) yyvsp[-2].integer.value >= (unsigned) yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value >= yyvsp[0].integer.value; ;
    break;}
case 20:
#line 175 "tradcif.y"
{ yyval.integer.unsignedp = 0;
			  if (yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp)
			    yyval.integer.value =
			      (unsigned) yyvsp[-2].integer.value < (unsigned) yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value < yyvsp[0].integer.value; ;
    break;}
case 21:
#line 182 "tradcif.y"
{ yyval.integer.unsignedp = 0;
			  if (yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp)
			    yyval.integer.value =
			      (unsigned) yyvsp[-2].integer.value > (unsigned) yyvsp[0].integer.value;
			  else
			    yyval.integer.value = yyvsp[-2].integer.value > yyvsp[0].integer.value; ;
    break;}
case 22:
#line 189 "tradcif.y"
{ yyval.integer.value = yyvsp[-2].integer.value & yyvsp[0].integer.value;
			  yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp; ;
    break;}
case 23:
#line 192 "tradcif.y"
{ yyval.integer.value = yyvsp[-2].integer.value ^ yyvsp[0].integer.value;
			  yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp; ;
    break;}
case 24:
#line 195 "tradcif.y"
{ yyval.integer.value = yyvsp[-2].integer.value | yyvsp[0].integer.value;
			  yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp; ;
    break;}
case 25:
#line 198 "tradcif.y"
{ yyval.integer.value = (yyvsp[-2].integer.value && yyvsp[0].integer.value);
			  yyval.integer.unsignedp = 0; ;
    break;}
case 26:
#line 201 "tradcif.y"
{ yyval.integer.value = (yyvsp[-2].integer.value || yyvsp[0].integer.value);
			  yyval.integer.unsignedp = 0; ;
    break;}
case 27:
#line 204 "tradcif.y"
{ yyval.integer.value = yyvsp[-4].integer.value ? yyvsp[-2].integer.value : yyvsp[0].integer.value;
			  yyval.integer.unsignedp = yyvsp[-2].integer.unsignedp || yyvsp[0].integer.unsignedp; ;
    break;}
case 28:
#line 207 "tradcif.y"
{ yyval.integer = yylval.integer; ;
    break;}
case 29:
#line 209 "tradcif.y"
{ yyval.integer = yylval.integer; ;
    break;}
case 30:
#line 211 "tradcif.y"
{ yyval.integer.value = 0;
			  yyval.integer.unsignedp = 0; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/share/misc/bison.simple"

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
#line 214 "tradcif.y"


/* During parsing of a C expression, the pointer to the next character
   is in this variable.  */

static char *lexptr;

/* Take care of parsing a number (anything that starts with a digit).
   Set yylval and return the token type; update lexptr.
   LEN is the number of characters in it.  */

/* maybe needs to actually deal with floating point numbers */

int
parse_number (olen)
     int olen;
{
  register char *p = lexptr;
  register long n = 0;
  register int c;
  register int base = 10;
  register int len = olen;

  for (c = 0; c < len; c++)
    if (p[c] == '.') {
      /* It's a float since it contains a point.  */
      yyerror ("floating point numbers not allowed in #if expressions");
      return ERROR;
    }

  yylval.integer.unsignedp = 0;

  if (len >= 3 && (!strncmp (p, "0x", 2) || !strncmp (p, "0X", 2))) {
    p += 2;
    base = 16;
    len -= 2;
  }
  else if (*p == '0')
    base = 8;

  while (len > 0) {
    c = *p++;
    len--;
    if (c >= 'A' && c <= 'Z') c += 'a' - 'A';

    if (c >= '0' && c <= '9') {
      n *= base;
      n += c - '0';
    } else if (base == 16 && c >= 'a' && c <= 'f') {
      n *= base;
      n += c - 'a' + 10;
    } else {
      /* `l' means long, and `u' means unsigned.  */
      while (1) {
	if (c == 'l' || c == 'L')
	  ;
	else if (c == 'u' || c == 'U')
	  yylval.integer.unsignedp = 1;
	else
	  break;

	if (len == 0)
	  break;
	c = *p++;
	len--;
      }
      /* Don't look for any more digits after the suffixes.  */
      break;
    }
  }

  if (len != 0) {
    yyerror ("Invalid number in #if expression");
    return ERROR;
  }

  /* If too big to be signed, consider it unsigned.  */
  if (n < 0)
    yylval.integer.unsignedp = 1;

  lexptr = p;
  yylval.integer.value = n;
  return INT;
}

struct token {
  const char *operator;
  int token;
};

#ifndef NULL
#define NULL 0
#endif

static struct token tokentab2[] = {
  {"&&", AND},
  {"||", OR},
  {"<<", LSH},
  {">>", RSH},
  {"==", EQUAL},
  {"!=", NOTEQUAL},
  {"<=", LEQ},
  {">=", GEQ},
  {NULL, ERROR}
};

/* Read one token, getting characters through lexptr.  */

int
yylex ()
{
  register int c;
  register int namelen;
  register char *tokstart;
  register struct token *toktab;

 retry:

  tokstart = lexptr;
  c = *tokstart;
  /* See if it is a special token of length 2.  */
  for (toktab = tokentab2; toktab->operator != NULL; toktab++)
    if (c == *toktab->operator && tokstart[1] == toktab->operator[1]) {
      lexptr += 2;
      return toktab->token;
    }

  switch (c) {
  case 0:
    return 0;
    
  case ' ':
  case '\t':
  case '\r':
  case '\n':
    lexptr++;
    goto retry;
    
  case '\'':
    lexptr++;
    c = *lexptr++;
    if (c == '\\')
      c = parse_escape (&lexptr);

    /* Sign-extend the constant if chars are signed on target machine.  */
    {
      if (lookup ((const unsigned char *)"__CHAR_UNSIGNED__",
		   sizeof ("__CHAR_UNSIGNED__")-1, -1)
	  || ((c >> (CHAR_TYPE_SIZE - 1)) & 1) == 0)
	yylval.integer.value = c & ((1 << CHAR_TYPE_SIZE) - 1);
      else
	yylval.integer.value = c | ~((1 << CHAR_TYPE_SIZE) - 1);
    }

    yylval.integer.unsignedp = 0;
    c = *lexptr++;
    if (c != '\'') {
      yyerror ("Invalid character constant in #if");
      return ERROR;
    }
    
    return CHAR;

    /* some of these chars are invalid in constant expressions;
       maybe do something about them later */
  case '/':
  case '+':
  case '-':
  case '*':
  case '%':
  case '|':
  case '&':
  case '^':
  case '~':
  case '!':
  case '@':
  case '<':
  case '>':
  case '(':
  case ')':
  case '[':
  case ']':
  case '.':
  case '?':
  case ':':
  case '=':
  case '{':
  case '}':
  case ',':
    lexptr++;
    return c;
    
  case '"':
    yyerror ("double quoted strings not allowed in #if expressions");
    return ERROR;
  }
  if (c >= '0' && c <= '9') {
    /* It's a number */
    for (namelen = 0;
	 c = tokstart[namelen], is_idchar[c] || c == '.'; 
	 namelen++)
      ;
    return parse_number (namelen);
  }
  
  if (!is_idstart[c]) {
    yyerror ("Invalid token in expression");
    return ERROR;
  }
  
  /* It is a name.  See how long it is.  */
  
  for (namelen = 0;
       is_idchar[(int)(unsigned char)tokstart[namelen]];
       namelen++)
    ;
  
  lexptr += namelen;
  return NAME;
}


/* Parse a C escape sequence.  STRING_PTR points to a variable
   containing a pointer to the string to parse.  That pointer
   is updated past the characters we use.  The value of the
   escape sequence is returned.

   A negative value means the sequence \ newline was seen,
   which is supposed to be equivalent to nothing at all.

   If \ is followed by a null character, we return a negative
   value and leave the string pointer pointing at the null character.

   If \ is followed by 000, we return 0 and leave the string pointer
   after the zeros.  A value of 0 does not mean end of string.  */

int
parse_escape (string_ptr)
     char **string_ptr;
{
  register int c = *(*string_ptr)++;
  switch (c)
    {
    case 'a':
      return TARGET_BELL;
    case 'b':
      return TARGET_BS;
    case 'e':
      return 033;
    case 'f':
      return TARGET_FF;
    case 'n':
      return TARGET_NEWLINE;
    case 'r':
      return TARGET_CR;
    case 't':
      return TARGET_TAB;
    case 'v':
      return TARGET_VT;
    case '\n':
      return -2;
    case 0:
      (*string_ptr)--;
      return 0;
    case '^':
      c = *(*string_ptr)++;
      if (c == '\\')
	c = parse_escape (string_ptr);
      if (c == '?')
	return 0177;
      return (c & 0200) | (c & 037);
      
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      {
	register int i = c - '0';
	register int count = 0;
	while (++count < 3)
	  {
	    c = *(*string_ptr)++;
	    if (c >= '0' && c <= '7')
	      i = (i << 3) + c - '0';
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	  }
	if ((i & ~((1 << CHAR_TYPE_SIZE) - 1)) != 0)
	  {
	    i &= (1 << CHAR_TYPE_SIZE) - 1;
	    warning ("octal character constant does not fit in a byte");
	  }
	return i;
      }
    case 'x':
      {
	register int i = 0;
	for (;;)
	  {
	    c = *(*string_ptr)++;
	    if (c >= '0' && c <= '9')
	      i = (i << 4) + c - '0';
	    else if (c >= 'a' && c <= 'f')
	      i = (i << 4) + c - 'a' + 10;
	    else if (c >= 'A' && c <= 'F')
	      i = (i << 4) + c - 'A' + 10;
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	  }
	if ((i & ~((1 << BITS_PER_UNIT) - 1)) != 0)
	  {
	    i &= (1 << BITS_PER_UNIT) - 1;
	    warning ("hex character constant does not fit in a byte");
	  }
	return i;
      }
    default:
      return c;
    }
}

void
yyerror (s)
     const char *s;
{
  error (s);
  longjmp (parse_return_error, 1);
}

/* This page contains the entry point to this file.  */

/* Parse STRING as an expression, and complain if this fails
   to use up all of the contents of STRING.  */
/* We do not support C comments.  They should be removed before
   this function is called.  */

int
parse_c_expression (string)
     char *string;
{
  lexptr = string;
  
  if (lexptr == 0 || *lexptr == 0) {
    error ("empty #if expression");
    return 0;			/* don't include the #if group */
  }

  /* if there is some sort of scanning error, just return 0 and assume
     the parsing routine has printed an error message somewhere.
     there is surely a better thing to do than this.     */
  if (setjmp (parse_return_error))
    return 0;

  if (yyparse ())
    return 0;			/* actually this is never reached
				   the way things stand. */
  if (*lexptr)
    error ("Junk after end of expression.");

  return expression_value;	/* set by yyparse () */
}
