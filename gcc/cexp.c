
/*  A Bison parser, made from cexp.y
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

#line 28 "cexp.y"

#include "config.h"

#include "system.h"
#include "intl.h"
#include <setjmp.h>
/* #define YYDEBUG 1 */

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif /* MULTIBYTE_CHARS */

typedef unsigned char U_CHAR;

/* This is used for communicating lists of keywords with cccp.c.  */
struct arglist {
  struct arglist *next;
  U_CHAR *name;
  int length;
  int argno;
};

HOST_WIDEST_INT parse_c_expression PARAMS ((char *, int));

static int yylex PARAMS ((void));
static void yyerror PARAMS ((const char *, ...))
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
static HOST_WIDEST_INT expression_value;
#ifdef TEST_EXP_READER
static int expression_signedp;
#endif

static jmp_buf parse_return_error;

/* Nonzero means count most punctuation as part of a name.  */
static int keyword_parsing = 0;

/* Nonzero means do not evaluate this expression.
   This is a count, since unevaluated expressions can nest.  */
static int skip_evaluation;

/* Nonzero means warn if undefined identifiers are evaluated.  */
static int warn_undef;

/* some external tables of character types */
extern unsigned char is_idstart[], is_idchar[], is_space[];

/* Flag for -pedantic.  */
extern int pedantic;

/* Flag for -traditional.  */
extern int traditional;

/* Flag for -lang-c89.  */
extern int c89;

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE INT_TYPE_SIZE
#endif

#ifndef MAX_CHAR_TYPE_SIZE
#define MAX_CHAR_TYPE_SIZE CHAR_TYPE_SIZE
#endif

#ifndef MAX_INT_TYPE_SIZE
#define MAX_INT_TYPE_SIZE INT_TYPE_SIZE
#endif

#ifndef MAX_LONG_TYPE_SIZE
#define MAX_LONG_TYPE_SIZE LONG_TYPE_SIZE
#endif

#ifndef MAX_WCHAR_TYPE_SIZE
#define MAX_WCHAR_TYPE_SIZE WCHAR_TYPE_SIZE
#endif

#define MAX_CHAR_TYPE_MASK (MAX_CHAR_TYPE_SIZE < HOST_BITS_PER_WIDEST_INT \
			    ? (~ (~ (HOST_WIDEST_INT) 0 << MAX_CHAR_TYPE_SIZE)) \
			    : ~ (HOST_WIDEST_INT) 0)

#define MAX_WCHAR_TYPE_MASK (MAX_WCHAR_TYPE_SIZE < HOST_BITS_PER_WIDEST_INT \
			     ? ~ (~ (HOST_WIDEST_INT) 0 << MAX_WCHAR_TYPE_SIZE) \
			     : ~ (HOST_WIDEST_INT) 0)

/* Suppose A1 + B1 = SUM1, using 2's complement arithmetic ignoring overflow.
   Suppose A, B and SUM have the same respective signs as A1, B1, and SUM1.
   Suppose SIGNEDP is negative if the result is signed, zero if unsigned.
   Then this yields nonzero if overflow occurred during the addition.
   Overflow occurs if A and B have the same sign, but A and SUM differ in sign,
   and SIGNEDP is negative.
   Use `^' to test whether signs differ, and `< 0' to isolate the sign.  */
#define overflow_sum_sign(a, b, sum, signedp) \
	((~((a) ^ (b)) & ((a) ^ (sum)) & (signedp)) < 0)

struct constant;

HOST_WIDEST_INT parse_escape PARAMS ((char **, HOST_WIDEST_INT));
int check_assertion PARAMS ((U_CHAR *, int, int, struct arglist *));
struct hashnode *lookup PARAMS ((U_CHAR *, int, int));
void error PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
void verror PARAMS ((const char *, va_list));
void pedwarn PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
void warning PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;

static int parse_number PARAMS ((int));
static HOST_WIDEST_INT left_shift PARAMS ((struct constant *, unsigned HOST_WIDEST_INT));
static HOST_WIDEST_INT right_shift PARAMS ((struct constant *, unsigned HOST_WIDEST_INT));
static void integer_overflow PARAMS ((void));

/* `signedp' values */
#define SIGNED (~0)
#define UNSIGNED 0

#line 155 "cexp.y"
typedef union {
  struct constant {HOST_WIDEST_INT value; int signedp;} integer;
  struct name {U_CHAR *address; int length;} name;
  struct arglist *keywords;
} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		77
#define	YYFLAG		-32768
#define	YYNTBASE	34

#define YYTRANSLATE(x) ((unsigned)(x) <= 269 ? yytranslate[x] : 43)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    29,     2,    31,     2,    27,    14,     2,    32,
    33,    25,    23,     9,    24,     2,    26,     2,     2,     2,
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
     0,     2,     4,     8,    11,    14,    17,    20,    23,    24,
    31,    35,    39,    43,    47,    51,    55,    59,    63,    67,
    71,    75,    79,    83,    87,    91,    95,    99,   100,   105,
   106,   111,   112,   113,   121,   123,   125,   127,   128,   133
};

static const short yyrhs[] = {    35,
     0,    36,     0,    35,     9,    36,     0,    24,    36,     0,
    29,    36,     0,    23,    36,     0,    30,    36,     0,    31,
     5,     0,     0,    31,     5,    37,    32,    42,    33,     0,
    32,    35,    33,     0,    36,    25,    36,     0,    36,    26,
    36,     0,    36,    27,    36,     0,    36,    23,    36,     0,
    36,    24,    36,     0,    36,    21,    36,     0,    36,    22,
    36,     0,    36,    15,    36,     0,    36,    16,    36,     0,
    36,    19,    36,     0,    36,    20,    36,     0,    36,    17,
    36,     0,    36,    18,    36,     0,    36,    14,    36,     0,
    36,    13,    36,     0,    36,    12,    36,     0,     0,    36,
    11,    38,    36,     0,     0,    36,    10,    39,    36,     0,
     0,     0,    36,     7,    40,    36,     8,    41,    36,     0,
     3,     0,     4,     0,     5,     0,     0,    32,    42,    33,
    42,     0,     5,    42,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   185,   195,   196,   203,   208,   211,   213,   216,   220,   222,
   227,   232,   245,   262,   275,   281,   287,   293,   299,   302,
   305,   312,   319,   326,   333,   336,   339,   342,   345,   348,
   351,   354,   356,   359,   362,   364,   366,   374,   376,   389
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","INT","CHAR",
"NAME","ERROR","'?'","':'","','","OR","AND","'|'","'^'","'&'","EQUAL","NOTEQUAL",
"'<'","'>'","LEQ","GEQ","LSH","RSH","'+'","'-'","'*'","'/'","'%'","UNARY","'!'",
"'~'","'#'","'('","')'","start","exp1","exp","@1","@2","@3","@4","@5","keywords", NULL
};
#endif

static const short yyr1[] = {     0,
    34,    35,    35,    36,    36,    36,    36,    36,    37,    36,
    36,    36,    36,    36,    36,    36,    36,    36,    36,    36,
    36,    36,    36,    36,    36,    36,    36,    38,    36,    39,
    36,    40,    41,    36,    36,    36,    36,    42,    42,    42
};

static const short yyr2[] = {     0,
     1,     1,     3,     2,     2,     2,     2,     2,     0,     6,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     0,     4,     0,
     4,     0,     0,     7,     1,     1,     1,     0,     4,     2
};

static const short yydefact[] = {     0,
    35,    36,    37,     0,     0,     0,     0,     0,     0,     1,
     2,     6,     4,     5,     7,     8,     0,     0,    32,    30,
    28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    11,     3,
     0,     0,     0,    27,    26,    25,    19,    20,    23,    24,
    21,    22,    17,    18,    15,    16,    12,    13,    14,    38,
     0,    31,    29,    38,    38,     0,    33,    40,     0,    10,
     0,    38,    34,    39,     0,     0,     0
};

static const short yydefgoto[] = {    75,
    10,    11,    38,    43,    42,    41,    71,    66
};

static const short yypact[] = {    12,
-32768,-32768,-32768,    12,    12,    12,    12,     1,    12,     4,
    79,-32768,-32768,-32768,-32768,   -21,    31,    12,-32768,-32768,
-32768,    12,    12,    12,    12,    12,    12,    12,    12,    12,
    12,    12,    12,    12,    12,    12,    12,    30,-32768,    79,
    12,    12,    12,   110,   124,   137,   148,   148,   155,   155,
   155,   155,   160,   160,   -17,   -17,-32768,-32768,-32768,     2,
    58,    34,    95,     2,     2,    54,-32768,-32768,    55,-32768,
    12,     2,    79,-32768,    63,   188,-32768
};

static const short yypgoto[] = {-32768,
   180,    -4,-32768,-32768,-32768,-32768,-32768,   -60
};


#define	YYLAST		189


static const short yytable[] = {    12,
    13,    14,    15,    68,    69,    16,    64,    35,    36,    37,
    -9,    74,    18,    40,     1,     2,     3,    44,    45,    46,
    47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
    57,    58,    59,    65,     4,     5,    61,    62,    63,    18,
     6,     7,     8,     9,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
    37,    60,    76,    39,    19,    67,    73,    20,    21,    22,
    23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
    33,    34,    35,    36,    37,    19,    70,    72,    20,    21,
    22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
    32,    33,    34,    35,    36,    37,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    23,    24,    25,    26,    27,    28,    29,    30,
    31,    32,    33,    34,    35,    36,    37,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
    37,    25,    26,    27,    28,    29,    30,    31,    32,    33,
    34,    35,    36,    37,    27,    28,    29,    30,    31,    32,
    33,    34,    35,    36,    37,    31,    32,    33,    34,    35,
    36,    37,    33,    34,    35,    36,    37,    77,    17
};

static const short yycheck[] = {     4,
     5,     6,     7,    64,    65,     5,     5,    25,    26,    27,
    32,    72,     9,    18,     3,     4,     5,    22,    23,    24,
    25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
    35,    36,    37,    32,    23,    24,    41,    42,    43,     9,
    29,    30,    31,    32,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    32,     0,    33,     7,     8,    71,    10,    11,    12,
    13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    27,     7,    33,    33,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    24,    25,    26,    27,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    13,    14,    15,    16,    17,    18,    19,    20,
    21,    22,    23,    24,    25,    26,    27,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    15,    16,    17,    18,    19,    20,    21,    22,    23,
    24,    25,    26,    27,    17,    18,    19,    20,    21,    22,
    23,    24,    25,    26,    27,    21,    22,    23,    24,    25,
    26,    27,    23,    24,    25,    26,    27,     0,     9
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"
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

#line 217 "/usr/lib/bison.simple"

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
#line 186 "cexp.y"
{
		  expression_value = yyvsp[0].integer.value;
#ifdef TEST_EXP_READER
		  expression_signedp = yyvsp[0].integer.signedp;
#endif
		;
    break;}
case 3:
#line 197 "cexp.y"
{ if (pedantic)
			    pedwarn ("comma operator in operand of `#if'");
			  yyval.integer = yyvsp[0].integer; ;
    break;}
case 4:
#line 204 "cexp.y"
{ yyval.integer.value = - yyvsp[0].integer.value;
			  yyval.integer.signedp = yyvsp[0].integer.signedp;
			  if ((yyval.integer.value & yyvsp[0].integer.value & yyval.integer.signedp) < 0)
			    integer_overflow (); ;
    break;}
case 5:
#line 209 "cexp.y"
{ yyval.integer.value = ! yyvsp[0].integer.value;
			  yyval.integer.signedp = SIGNED; ;
    break;}
case 6:
#line 212 "cexp.y"
{ yyval.integer = yyvsp[0].integer; ;
    break;}
case 7:
#line 214 "cexp.y"
{ yyval.integer.value = ~ yyvsp[0].integer.value;
			  yyval.integer.signedp = yyvsp[0].integer.signedp; ;
    break;}
case 8:
#line 217 "cexp.y"
{ yyval.integer.value = check_assertion (yyvsp[0].name.address, yyvsp[0].name.length,
						      0, NULL_PTR);
			  yyval.integer.signedp = SIGNED; ;
    break;}
case 9:
#line 221 "cexp.y"
{ keyword_parsing = 1; ;
    break;}
case 10:
#line 223 "cexp.y"
{ yyval.integer.value = check_assertion (yyvsp[-4].name.address, yyvsp[-4].name.length,
						      1, yyvsp[-1].keywords);
			  keyword_parsing = 0;
			  yyval.integer.signedp = SIGNED; ;
    break;}
case 11:
#line 228 "cexp.y"
{ yyval.integer = yyvsp[-1].integer; ;
    break;}
case 12:
#line 233 "cexp.y"
{ yyval.integer.signedp = yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp;
			  if (yyval.integer.signedp)
			    {
			      yyval.integer.value = yyvsp[-2].integer.value * yyvsp[0].integer.value;
			      if (yyvsp[-2].integer.value
				  && (yyval.integer.value / yyvsp[-2].integer.value != yyvsp[0].integer.value
				      || (yyval.integer.value & yyvsp[-2].integer.value & yyvsp[0].integer.value) < 0))
				integer_overflow ();
			    }
			  else
			    yyval.integer.value = ((unsigned HOST_WIDEST_INT) yyvsp[-2].integer.value
					* yyvsp[0].integer.value); ;
    break;}
case 13:
#line 246 "cexp.y"
{ if (yyvsp[0].integer.value == 0)
			    {
			      if (!skip_evaluation)
				error ("division by zero in #if");
			      yyvsp[0].integer.value = 1;
			    }
			  yyval.integer.signedp = yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp;
			  if (yyval.integer.signedp)
			    {
			      yyval.integer.value = yyvsp[-2].integer.value / yyvsp[0].integer.value;
			      if ((yyval.integer.value & yyvsp[-2].integer.value & yyvsp[0].integer.value) < 0)
				integer_overflow ();
			    }
			  else
			    yyval.integer.value = ((unsigned HOST_WIDEST_INT) yyvsp[-2].integer.value
					/ yyvsp[0].integer.value); ;
    break;}
case 14:
#line 263 "cexp.y"
{ if (yyvsp[0].integer.value == 0)
			    {
			      if (!skip_evaluation)
				error ("division by zero in #if");
			      yyvsp[0].integer.value = 1;
			    }
			  yyval.integer.signedp = yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp;
			  if (yyval.integer.signedp)
			    yyval.integer.value = yyvsp[-2].integer.value % yyvsp[0].integer.value;
			  else
			    yyval.integer.value = ((unsigned HOST_WIDEST_INT) yyvsp[-2].integer.value
					% yyvsp[0].integer.value); ;
    break;}
case 15:
#line 276 "cexp.y"
{ yyval.integer.value = yyvsp[-2].integer.value + yyvsp[0].integer.value;
			  yyval.integer.signedp = yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp;
			  if (overflow_sum_sign (yyvsp[-2].integer.value, yyvsp[0].integer.value,
						 yyval.integer.value, yyval.integer.signedp))
			    integer_overflow (); ;
    break;}
case 16:
#line 282 "cexp.y"
{ yyval.integer.value = yyvsp[-2].integer.value - yyvsp[0].integer.value;
			  yyval.integer.signedp = yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp;
			  if (overflow_sum_sign (yyval.integer.value, yyvsp[0].integer.value,
						 yyvsp[-2].integer.value, yyval.integer.signedp))
			    integer_overflow (); ;
    break;}
case 17:
#line 288 "cexp.y"
{ yyval.integer.signedp = yyvsp[-2].integer.signedp;
			  if ((yyvsp[0].integer.value & yyvsp[0].integer.signedp) < 0)
			    yyval.integer.value = right_shift (&yyvsp[-2].integer, -yyvsp[0].integer.value);
			  else
			    yyval.integer.value = left_shift (&yyvsp[-2].integer, yyvsp[0].integer.value); ;
    break;}
case 18:
#line 294 "cexp.y"
{ yyval.integer.signedp = yyvsp[-2].integer.signedp;
			  if ((yyvsp[0].integer.value & yyvsp[0].integer.signedp) < 0)
			    yyval.integer.value = left_shift (&yyvsp[-2].integer, -yyvsp[0].integer.value);
			  else
			    yyval.integer.value = right_shift (&yyvsp[-2].integer, yyvsp[0].integer.value); ;
    break;}
case 19:
#line 300 "cexp.y"
{ yyval.integer.value = (yyvsp[-2].integer.value == yyvsp[0].integer.value);
			  yyval.integer.signedp = SIGNED; ;
    break;}
case 20:
#line 303 "cexp.y"
{ yyval.integer.value = (yyvsp[-2].integer.value != yyvsp[0].integer.value);
			  yyval.integer.signedp = SIGNED; ;
    break;}
case 21:
#line 306 "cexp.y"
{ yyval.integer.signedp = SIGNED;
			  if (yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp)
			    yyval.integer.value = yyvsp[-2].integer.value <= yyvsp[0].integer.value;
			  else
			    yyval.integer.value = ((unsigned HOST_WIDEST_INT) yyvsp[-2].integer.value
					<= yyvsp[0].integer.value); ;
    break;}
case 22:
#line 313 "cexp.y"
{ yyval.integer.signedp = SIGNED;
			  if (yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp)
			    yyval.integer.value = yyvsp[-2].integer.value >= yyvsp[0].integer.value;
			  else
			    yyval.integer.value = ((unsigned HOST_WIDEST_INT) yyvsp[-2].integer.value
					>= yyvsp[0].integer.value); ;
    break;}
case 23:
#line 320 "cexp.y"
{ yyval.integer.signedp = SIGNED;
			  if (yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp)
			    yyval.integer.value = yyvsp[-2].integer.value < yyvsp[0].integer.value;
			  else
			    yyval.integer.value = ((unsigned HOST_WIDEST_INT) yyvsp[-2].integer.value
					< yyvsp[0].integer.value); ;
    break;}
case 24:
#line 327 "cexp.y"
{ yyval.integer.signedp = SIGNED;
			  if (yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp)
			    yyval.integer.value = yyvsp[-2].integer.value > yyvsp[0].integer.value;
			  else
			    yyval.integer.value = ((unsigned HOST_WIDEST_INT) yyvsp[-2].integer.value
					> yyvsp[0].integer.value); ;
    break;}
case 25:
#line 334 "cexp.y"
{ yyval.integer.value = yyvsp[-2].integer.value & yyvsp[0].integer.value;
			  yyval.integer.signedp = yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp; ;
    break;}
case 26:
#line 337 "cexp.y"
{ yyval.integer.value = yyvsp[-2].integer.value ^ yyvsp[0].integer.value;
			  yyval.integer.signedp = yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp; ;
    break;}
case 27:
#line 340 "cexp.y"
{ yyval.integer.value = yyvsp[-2].integer.value | yyvsp[0].integer.value;
			  yyval.integer.signedp = yyvsp[-2].integer.signedp & yyvsp[0].integer.signedp; ;
    break;}
case 28:
#line 343 "cexp.y"
{ skip_evaluation += !yyvsp[-1].integer.value; ;
    break;}
case 29:
#line 345 "cexp.y"
{ skip_evaluation -= !yyvsp[-3].integer.value;
			  yyval.integer.value = (yyvsp[-3].integer.value && yyvsp[0].integer.value);
			  yyval.integer.signedp = SIGNED; ;
    break;}
case 30:
#line 349 "cexp.y"
{ skip_evaluation += !!yyvsp[-1].integer.value; ;
    break;}
case 31:
#line 351 "cexp.y"
{ skip_evaluation -= !!yyvsp[-3].integer.value;
			  yyval.integer.value = (yyvsp[-3].integer.value || yyvsp[0].integer.value);
			  yyval.integer.signedp = SIGNED; ;
    break;}
case 32:
#line 355 "cexp.y"
{ skip_evaluation += !yyvsp[-1].integer.value; ;
    break;}
case 33:
#line 357 "cexp.y"
{ skip_evaluation += !!yyvsp[-4].integer.value - !yyvsp[-4].integer.value; ;
    break;}
case 34:
#line 359 "cexp.y"
{ skip_evaluation -= !!yyvsp[-6].integer.value;
			  yyval.integer.value = yyvsp[-6].integer.value ? yyvsp[-3].integer.value : yyvsp[0].integer.value;
			  yyval.integer.signedp = yyvsp[-3].integer.signedp & yyvsp[0].integer.signedp; ;
    break;}
case 35:
#line 363 "cexp.y"
{ yyval.integer = yylval.integer; ;
    break;}
case 36:
#line 365 "cexp.y"
{ yyval.integer = yylval.integer; ;
    break;}
case 37:
#line 367 "cexp.y"
{ if (warn_undef && !skip_evaluation)
			    warning ("`%.*s' is not defined",
				     yyvsp[0].name.length, yyvsp[0].name.address);
			  yyval.integer.value = 0;
			  yyval.integer.signedp = SIGNED; ;
    break;}
case 38:
#line 375 "cexp.y"
{ yyval.keywords = 0; ;
    break;}
case 39:
#line 377 "cexp.y"
{ struct arglist *temp;
			  yyval.keywords = (struct arglist *) xmalloc (sizeof (struct arglist));
			  yyval.keywords->next = yyvsp[-2].keywords;
			  yyval.keywords->name = (U_CHAR *) "(";
			  yyval.keywords->length = 1;
			  temp = yyval.keywords;
			  while (temp != 0 && temp->next != 0)
			    temp = temp->next;
			  temp->next = (struct arglist *) xmalloc (sizeof (struct arglist));
			  temp->next->next = yyvsp[0].keywords;
			  temp->next->name = (U_CHAR *) ")";
			  temp->next->length = 1; ;
    break;}
case 40:
#line 390 "cexp.y"
{ yyval.keywords = (struct arglist *) xmalloc (sizeof (struct arglist));
			  yyval.keywords->name = yyvsp[-1].name.address;
			  yyval.keywords->length = yyvsp[-1].name.length;
			  yyval.keywords->next = yyvsp[0].keywords; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/lib/bison.simple"

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
#line 395 "cexp.y"


/* During parsing of a C expression, the pointer to the next character
   is in this variable.  */

static char *lexptr;

/* Take care of parsing a number (anything that starts with a digit).
   Set yylval and return the token type; update lexptr.
   LEN is the number of characters in it.  */

/* maybe needs to actually deal with floating point numbers */

static int
parse_number (olen)
     int olen;
{
  register char *p = lexptr;
  register int c;
  register unsigned HOST_WIDEST_INT n = 0, nd, max_over_base;
  register int base = 10;
  register int len = olen;
  register int overflow = 0;
  register int digit, largest_digit = 0;
  int spec_long = 0;

  yylval.integer.signedp = SIGNED;

  if (*p == '0') {
    base = 8;
    if (len >= 3 && (p[1] == 'x' || p[1] == 'X')) {
      p += 2;
      base = 16;
      len -= 2;
    }
  }

  max_over_base = (unsigned HOST_WIDEST_INT) -1 / base;

  for (; len > 0; len--) {
    c = *p++;

    if (c >= '0' && c <= '9')
      digit = c - '0';
    else if (base == 16 && c >= 'a' && c <= 'f')
      digit = c - 'a' + 10;
    else if (base == 16 && c >= 'A' && c <= 'F')
      digit = c - 'A' + 10;
    else {
      /* `l' means long, and `u' means unsigned.  */
      while (1) {
	if (c == 'l' || c == 'L')
	  {
	    if (!pedantic < spec_long)
	      yyerror ("too many `l's in integer constant");
	    spec_long++;
	  }
	else if (c == 'u' || c == 'U')
	  {
	    if (! yylval.integer.signedp)
	      yyerror ("two `u's in integer constant");
	    yylval.integer.signedp = UNSIGNED;
	  }
	else {
	  if (c == '.' || c == 'e' || c == 'E' || c == 'p' || c == 'P')
	    yyerror ("Floating point numbers not allowed in #if expressions");
	  else
	    yyerror ("missing white space after number `%.*s'",
		     (int) (p - lexptr - 1), lexptr);
	}

	if (--len == 0)
	  break;
	c = *p++;
      }
      /* Don't look for any more digits after the suffixes.  */
      break;
    }
    if (largest_digit < digit)
      largest_digit = digit;
    nd = n * base + digit;
    overflow |= (max_over_base < n) | (nd < n);
    n = nd;
  }

  if (base <= largest_digit)
    pedwarn ("integer constant contains digits beyond the radix");

  if (overflow)
    pedwarn ("integer constant out of range");

  /* If too big to be signed, consider it unsigned.  */
  if (((HOST_WIDEST_INT) n & yylval.integer.signedp) < 0)
    {
      if (base == 10)
	warning ("integer constant is so large that it is unsigned");
      yylval.integer.signedp = UNSIGNED;
    }

  lexptr = p;
  yylval.integer.value = n;
  return INT;
}

struct token {
  const char *operator;
  int token;
};

static struct token tokentab2[] = {
  {"&&", AND},
  {"||", OR},
  {"<<", LSH},
  {">>", RSH},
  {"==", EQUAL},
  {"!=", NOTEQUAL},
  {"<=", LEQ},
  {">=", GEQ},
  {"++", ERROR},
  {"--", ERROR},
  {NULL, ERROR}
};

/* Read one token, getting characters through lexptr.  */

static int
yylex ()
{
  register int c;
  register int namelen;
  register unsigned char *tokstart;
  register struct token *toktab;
  int wide_flag;
  HOST_WIDEST_INT mask;

 retry:

  tokstart = (unsigned char *) lexptr;
  c = *tokstart;
  /* See if it is a special token of length 2.  */
  if (! keyword_parsing)
    for (toktab = tokentab2; toktab->operator != NULL; toktab++)
      if (c == *toktab->operator && tokstart[1] == toktab->operator[1]) {
	lexptr += 2;
	if (toktab->token == ERROR)
	  yyerror ("`%s' not allowed in operand of `#if'", toktab->operator);
	return toktab->token;
      }

  switch (c) {
  case '\n':
    return 0;
    
  case ' ':
  case '\t':
  case '\r':
    lexptr++;
    goto retry;
    
  case 'L':
    /* Capital L may start a wide-string or wide-character constant.  */
    if (lexptr[1] == '\'')
      {
	lexptr++;
	wide_flag = 1;
	mask = MAX_WCHAR_TYPE_MASK;
	goto char_constant;
      }
    if (lexptr[1] == '"')
      {
	lexptr++;
	wide_flag = 1;
	mask = MAX_WCHAR_TYPE_MASK;
	goto string_constant;
      }
    break;

  case '\'':
    wide_flag = 0;
    mask = MAX_CHAR_TYPE_MASK;
  char_constant:
    lexptr++;
    if (keyword_parsing) {
      char *start_ptr = lexptr - 1;
      while (1) {
	c = *lexptr++;
	if (c == '\\')
	  c = parse_escape (&lexptr, mask);
	else if (c == '\'')
	  break;
      }
      yylval.name.address = tokstart;
      yylval.name.length = lexptr - start_ptr;
      return NAME;
    }

    /* This code for reading a character constant
       handles multicharacter constants and wide characters.
       It is mostly copied from c-lex.c.  */
    {
      register HOST_WIDEST_INT result = 0;
      register int num_chars = 0;
      int chars_seen = 0;
      unsigned width = MAX_CHAR_TYPE_SIZE;
      int max_chars;
#ifdef MULTIBYTE_CHARS
      int longest_char = local_mb_cur_max ();
      char *token_buffer = (char *) alloca (longest_char);
      (void) local_mbtowc (NULL_PTR, NULL_PTR, 0);
#endif

      max_chars = MAX_LONG_TYPE_SIZE / width;
      if (wide_flag)
	width = MAX_WCHAR_TYPE_SIZE;

      while (1)
	{
	  c = *lexptr++;

	  if (c == '\'' || c == EOF)
	    break;

	  ++chars_seen;
	  if (c == '\\')
	    {
	      c = parse_escape (&lexptr, mask);
	    }
	  else
	    {
#ifdef MULTIBYTE_CHARS
	      wchar_t wc;
	      int i;
	      int char_len = -1;
	      for (i = 1; i <= longest_char; ++i)
		{
		  token_buffer[i - 1] = c;
		  char_len = local_mbtowc (& wc, token_buffer, i);
		  if (char_len != -1)
		    break;
		  c = *lexptr++;
		}
	      if (char_len > 1)
		{
		  /* mbtowc sometimes needs an extra char before accepting */
		  if (char_len < i)
		    lexptr--;
		  if (! wide_flag)
		    {
		      /* Merge character into result; ignore excess chars.  */
		      for (i = 1; i <= char_len; ++i)
			{
			  if (i > max_chars)
			    break;
			  if (width < HOST_BITS_PER_INT)
			    result = (result << width)
			      | (token_buffer[i - 1]
				 & ((1 << width) - 1));
			  else
			    result = token_buffer[i - 1];
			}
		      num_chars += char_len;
		      continue;
		    }
		}
	      else
		{
		  if (char_len == -1)
		    warning ("Ignoring invalid multibyte character");
		}
	      if (wide_flag)
		c = wc;
#endif /* ! MULTIBYTE_CHARS */
	    }

	  if (wide_flag)
	    {
	      if (chars_seen == 1) /* only keep the first one */
		result = c;
	      continue;
	    }

	  /* Merge character into result; ignore excess chars.  */
	  num_chars++;
	  if (num_chars <= max_chars)
	    {
	      if (width < HOST_BITS_PER_INT)
		result = (result << width) | (c & ((1 << width) - 1));
	      else
		result = c;
	    }
	}

      if (c != '\'')
	error ("malformatted character constant");
      else if (chars_seen == 0)
	error ("empty character constant");
      else if (num_chars > max_chars)
	{
	  num_chars = max_chars;
	  error ("character constant too long");
	}
      else if (chars_seen != 1 && ! traditional)
	warning ("multi-character character constant");

      /* If char type is signed, sign-extend the constant.  */
      if (! wide_flag)
	{
	  int num_bits = num_chars * width;
	  if (num_bits == 0)
	    /* We already got an error; avoid invalid shift.  */
	    yylval.integer.value = 0;
	  else if (lookup ((U_CHAR *) "__CHAR_UNSIGNED__",
		      sizeof ("__CHAR_UNSIGNED__") - 1, -1)
	      || ((result >> (num_bits - 1)) & 1) == 0)
	    yylval.integer.value
	      = result & (~ (unsigned HOST_WIDEST_INT) 0
			  >> (HOST_BITS_PER_WIDEST_INT - num_bits));
	  else
	    yylval.integer.value
	      = result | ~(~ (unsigned HOST_WIDEST_INT) 0
			   >> (HOST_BITS_PER_WIDEST_INT - num_bits));
	}
      else
	{
	  yylval.integer.value = result;
	}
    }

    /* This is always a signed type.  */
    yylval.integer.signedp = SIGNED;
    
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
  case '[':
  case ']':
  case '.':
  case '?':
  case ':':
  case '=':
  case '{':
  case '}':
  case ',':
  case '#':
    if (keyword_parsing)
      break;
  case '(':
  case ')':
    lexptr++;
    return c;

  case '"':
    mask = MAX_CHAR_TYPE_MASK;
  string_constant:
    if (keyword_parsing) {
      char *start_ptr = lexptr;
      lexptr++;
      while (1) {
	c = *lexptr++;
	if (c == '\\')
	  c = parse_escape (&lexptr, mask);
	else if (c == '"')
	  break;
      }
      yylval.name.address = tokstart;
      yylval.name.length = lexptr - start_ptr;
      return NAME;
    }
    yyerror ("string constants not allowed in #if expressions");
    return ERROR;
  }

  if (c >= '0' && c <= '9' && !keyword_parsing) {
    /* It's a number */
    for (namelen = 1; ; namelen++) {
      int d = tokstart[namelen];
      if (! ((is_idchar[d] || d == '.')
	     || ((d == '-' || d == '+')
		 && (c == 'e' || c == 'E'
		     || ((c == 'p' || c == 'P') && ! c89))
		 && ! traditional)))
	break;
      c = d;
    }
    return parse_number (namelen);
  }

  /* It is a name.  See how long it is.  */

  if (keyword_parsing) {
    for (namelen = 0;; namelen++) {
      if (is_space[tokstart[namelen]])
	break;
      if (tokstart[namelen] == '(' || tokstart[namelen] == ')')
	break;
      if (tokstart[namelen] == '"' || tokstart[namelen] == '\'')
	break;
    }
  } else {
    if (!is_idstart[c]) {
      yyerror ("Invalid token in expression");
      return ERROR;
    }

    for (namelen = 0; is_idchar[tokstart[namelen]]; namelen++)
      ;
  }
  
  lexptr += namelen;
  yylval.name.address = tokstart;
  yylval.name.length = namelen;
  return NAME;
}


/* Parse a C escape sequence.  STRING_PTR points to a variable
   containing a pointer to the string to parse.  That pointer
   is updated past the characters we use.  The value of the
   escape sequence is returned.

   RESULT_MASK is used to mask out the result;
   an error is reported if bits are lost thereby.

   A negative value means the sequence \ newline was seen,
   which is supposed to be equivalent to nothing at all.

   If \ is followed by a null character, we return a negative
   value and leave the string pointer pointing at the null character.

   If \ is followed by 000, we return 0 and leave the string pointer
   after the zeros.  A value of 0 does not mean end of string.  */

HOST_WIDEST_INT
parse_escape (string_ptr, result_mask)
     char **string_ptr;
     HOST_WIDEST_INT result_mask;
{
  register int c = *(*string_ptr)++;
  switch (c)
    {
    case 'a':
      return TARGET_BELL;
    case 'b':
      return TARGET_BS;
    case 'e':
    case 'E':
      if (pedantic)
	pedwarn ("non-ANSI-standard escape sequence, `\\%c'", c);
      return TARGET_ESC;
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
      
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      {
	register HOST_WIDEST_INT i = c - '0';
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
	if (i != (i & result_mask))
	  {
	    i &= result_mask;
	    pedwarn ("octal escape sequence out of range");
	  }
	return i;
      }
    case 'x':
      {
	register unsigned HOST_WIDEST_INT i = 0, overflow = 0;
	register int digits_found = 0, digit;
	for (;;)
	  {
	    c = *(*string_ptr)++;
	    if (c >= '0' && c <= '9')
	      digit = c - '0';
	    else if (c >= 'a' && c <= 'f')
	      digit = c - 'a' + 10;
	    else if (c >= 'A' && c <= 'F')
	      digit = c - 'A' + 10;
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	    overflow |= i ^ (i << 4 >> 4);
	    i = (i << 4) + digit;
	    digits_found = 1;
	  }
	if (!digits_found)
	  yyerror ("\\x used with no following hex digits");
	if (overflow | (i != (i & result_mask)))
	  {
	    i &= result_mask;
	    pedwarn ("hex escape sequence out of range");
	  }
	return i;
      }
    default:
      return c;
    }
}

static void
integer_overflow ()
{
  if (!skip_evaluation && pedantic)
    pedwarn ("integer overflow in preprocessor expression");
}

static HOST_WIDEST_INT
left_shift (a, b)
     struct constant *a;
     unsigned HOST_WIDEST_INT b;
{
   /* It's unclear from the C standard whether shifts can overflow.
      The following code ignores overflow; perhaps a C standard
      interpretation ruling is needed.  */
  if (b >= HOST_BITS_PER_WIDEST_INT)
    return 0;
  else
    return (unsigned HOST_WIDEST_INT) a->value << b;
}

static HOST_WIDEST_INT
right_shift (a, b)
     struct constant *a;
     unsigned HOST_WIDEST_INT b;
{
  if (b >= HOST_BITS_PER_WIDEST_INT)
    return a->signedp ? a->value >> (HOST_BITS_PER_WIDEST_INT - 1) : 0;
  else if (a->signedp)
    return a->value >> b;
  else
    return (unsigned HOST_WIDEST_INT) a->value >> b;
}

/* This page contains the entry point to this file.  */

/* Parse STRING as an expression, and complain if this fails
   to use up all of the contents of STRING.
   STRING may contain '\0' bytes; it is terminated by the first '\n'
   outside a string constant, so that we can diagnose '\0' properly.
   If WARN_UNDEFINED is nonzero, warn if undefined identifiers are evaluated.
   We do not support C comments.  They should be removed before
   this function is called.  */

HOST_WIDEST_INT
parse_c_expression (string, warn_undefined)
     char *string;
     int warn_undefined;
{
  lexptr = string;
  warn_undef = warn_undefined;

  /* if there is some sort of scanning error, just return 0 and assume
     the parsing routine has printed an error message somewhere.
     there is surely a better thing to do than this.     */
  if (setjmp (parse_return_error))
    return 0;

  if (yyparse () != 0)
    abort ();

  if (*lexptr != '\n')
    error ("Junk after end of expression.");

  return expression_value;	/* set by yyparse () */
}

static void
yyerror VPARAMS ((const char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  const char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, const char *);
#endif

  verror (msgid, args);
  va_end (args);
  skip_evaluation = 0;
  longjmp (parse_return_error, 1);
}


#ifdef TEST_EXP_READER

#if YYDEBUG
extern int yydebug;
#endif

int pedantic;
int traditional;
int c89;

int main PARAMS ((int, char **));
static void initialize_random_junk PARAMS ((void));
static void print_unsigned_host_widest_int PARAMS ((unsigned HOST_WIDEST_INT));

/* Main program for testing purposes.  */
int
main (argc, argv)
     int argc;
     char **argv;
{
  int n, c;
  char buf[1024];
  unsigned HOST_WIDEST_INT u;

  pedantic = 1 < argc;
  traditional = 2 < argc;
  c89 = 3 < argc;
#if YYDEBUG
  yydebug = 4 < argc;
#endif
  initialize_random_junk ();

  for (;;) {
    printf ("enter expression: ");
    n = 0;
    while ((buf[n] = c = getchar ()) != '\n' && c != EOF)
      n++;
    if (c == EOF)
      break;
    parse_c_expression (buf, 1);
    printf ("parser returned ");
    u = (unsigned HOST_WIDEST_INT) expression_value;
    if (expression_value < 0 && expression_signedp) {
      u = -u;
      printf ("-");
    }
    if (u == 0)
      printf ("0");
    else
      print_unsigned_host_widest_int (u);
    if (! expression_signedp)
      printf("u");
    printf ("\n");
  }

  return 0;
}

static void
print_unsigned_host_widest_int (u)
     unsigned HOST_WIDEST_INT u;
{
  if (u) {
    print_unsigned_host_widest_int (u / 10);
    putchar ('0' + (int) (u % 10));
  }
}

/* table to tell if char can be part of a C identifier. */
unsigned char is_idchar[256];
/* table to tell if char can be first char of a c identifier. */
unsigned char is_idstart[256];
/* table to tell if c is horizontal or vertical space.  */
unsigned char is_space[256];

/*
 * initialize random junk in the hash table and maybe other places
 */
static void
initialize_random_junk ()
{
  register int i;

  /*
   * Set up is_idchar and is_idstart tables.  These should be
   * faster than saying (is_alpha (c) || c == '_'), etc.
   * Must do set up these things before calling any routines tthat
   * refer to them.
   */
  for (i = 'a'; i <= 'z'; i++) {
    ++is_idchar[TOUPPER(i)];
    ++is_idchar[i];
    ++is_idstart[TOUPPER(i)];
    ++is_idstart[i];
  }
  for (i = '0'; i <= '9'; i++)
    ++is_idchar[i];
  ++is_idchar['_'];
  ++is_idstart['_'];
  ++is_idchar['$'];
  ++is_idstart['$'];

  ++is_space[' '];
  ++is_space['\t'];
  ++is_space['\v'];
  ++is_space['\f'];
  ++is_space['\n'];
  ++is_space['\r'];
}

void
error VPARAMS ((char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, char *);
#endif

  fprintf (stderr, "error: ");
  vfprintf (stderr, _(msgid), args);
  fprintf (stderr, "\n");
  va_end (args);
}

void
pedwarn VPARAMS ((char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, char *);
#endif

  fprintf (stderr, "pedwarn: ");
  vfprintf (stderr, _(msgid), args);
  fprintf (stderr, "\n");
  va_end (args);
}

void
warning VPARAMS ((char * msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  char * msgid;
#endif
  va_list args;

  VA_START (args, msgid);

#ifndef ANSI_PROTOTYPES
  msgid = va_arg (args, char *);
#endif

  fprintf (stderr, "warning: ");
  vfprintf (stderr, _(msgid), args);
  fprintf (stderr, "\n");
  va_end (args);
}


int
check_assertion (name, sym_length, tokens_specified, tokens)
     U_CHAR *name;
     int sym_length;
     int tokens_specified;
     struct arglist *tokens;
{
  return 0;
}

struct hashnode *
lookup (name, len, hash)
     U_CHAR *name;
     int len;
     int hash;
{
  return (DEFAULT_SIGNED_CHAR) ? 0 : ((struct hashnode *) -1);
}

PTR
xmalloc (size)
  size_t size;
{
  return (PTR) malloc (size);
}
#endif
