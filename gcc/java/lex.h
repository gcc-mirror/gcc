/* Language lexer definitions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#ifndef GCC_JAVA_LEX_H
#define GCC_JAVA_LEX_H

#include "input.h"

/* Extern global variables declarations  */
extern FILE *finput;

/* A Unicode character, as read from the input file  */
typedef unsigned short unicode_t;

#if defined HAVE_ICONV_H && defined HAVE_ICONV
#include <iconv.h>
#endif /* HAVE_ICONV */

/* Default encoding to use if no encoding is specified.  */
#define DEFAULT_ENCODING "UTF-8"

/* Debug macro to print-out what we match  */
#ifdef JAVA_LEX_DEBUG
#ifdef JAVA_LEX_DEBUG_CHAR
#define JAVA_LEX_CHAR(c)      printf ("java_lex:%d: char '%c'.%d\n", 	\
				      lineno, (c < 128 ? c : '.'), c);
#else
#define JAVA_LEX_CHAR(c)
#endif
#define JAVA_LEX_KW(c)        printf ("java_lex:%d: keyword: '%s'\n", lineno,c)
#define JAVA_LEX_ID(s)        printf ("java_lex:%d: ID: '%s'\n",	\
				      lineno,				\
				      (all_ascii ? s : "<U>"))
#define JAVA_LEX_LIT(s, r)    printf ("java_lex:%d: literal '%s'_%d\n",	\
				      lineno, s, r)
#define JAVA_LEX_CHAR_LIT(s)  printf ("java_lex:%d: literal '%d'\n", lineno, s)
#define JAVA_LEX_STR_LIT(s)   {						 \
				 int i;					 \
				 printf ("java_lex:%d: literal '%s'\n",  \
					 lineno, s);			 \
			       }
#define JAVA_LEX_SEP(c)       printf ("java_lex:%d: separator '%c'\n",lineno,c)
#define JAVA_LEX_OP(c)        printf ("java_lex:%d: operator '%s'\n", lineno,c)
#else
#define JAVA_LEX_CHAR(c)
#define JAVA_LEX_KW(c)
#define JAVA_LEX_ID(s)
#define JAVA_LEX_LIT(s,r)
#define JAVA_LEX_CHAR_LIT(s)
#define JAVA_LEX_STR_LIT(s)
#define JAVA_LEX_SEP(c)
#define JAVA_LEX_OP(s)
#endif

/* Line information containers  */
struct java_line {
  unicode_t *line;		/* The line's unicode */
  char      *unicode_escape_p;	/* The matching char was a unicode escape */
  unicode_t ahead[1];		/* Character ahead */
  char unicode_escape_ahead_p;	/* Character ahead is a unicode escape */
  int max;			/* buffer's max size */
  int size;			/* number of unicodes */
  int current;			/* Current position, unicode based */
  int char_col;			/* Current position, input char based */
  int lineno;			/* Its line number */
  int white_space_only;		/* If it contains only white spaces */
};
#define JAVA_COLUMN_DELTA(p)						\
  (ctxp->c_line->unicode_escape_p [ctxp->c_line->current+(p)] ? 6 : 	\
   (ctxp->c_line->line [ctxp->c_line->current+(p)] == '\t' ? 8 : 1))

struct java_error {
  struct java_line *line;
  int error;
};

typedef struct java_lc_s GTY(()) {
  int line;
  int prev_col;
  int col;
} java_lc;

struct java_lexer
{
  /* The file from which we're reading.  */
  FILE *finput;

  /* Number of consecutive backslashes we've read.  */
  int bs_count;

  /* If nonzero, a value that was pushed back.  */
  unicode_t unget_value;

  /* If nonzero, we've hit EOF.  Used only by java_get_unicode().  */
  int hit_eof : 1;

#ifdef HAVE_ICONV
  /* Nonzero if we've read any bytes.  We only recognize the
     byte-order-marker (BOM) as the first word.  */
  int read_anything : 1;

  /* Nonzero if we have to byte swap.  */
  int byte_swap : 1;

  /* Nonzero if we're using the fallback decoder.  */
  int use_fallback : 1;

  /* The handle for the iconv converter we're using.  */
  iconv_t handle;

  /* Bytes we've read from the file but have not sent to iconv.  */
  char buffer[1024];

  /* Index of first valid character in buffer, -1 if no valid
     characters.  */
  int first;

  /* Index of last valid character in buffer, plus one.  -1 if no
     valid characters in buffer.  */
  int last;

  /* This is a buffer of characters already converted by iconv.  We
     use `char' here because we're assuming that iconv() converts to
     UCS-2, and then we convert it ourselves.  */
  unsigned char out_buffer[1024];

  /* Index of first valid output character.  -1 if no valid
     characters.  */
  int out_first;

  /* Index of last valid output character, plus one.  -1 if no valid
     characters.  */
  int out_last;

#endif /* HAVE_ICONV */
};
typedef struct java_lexer java_lexer;

/* Destroy a lexer object.  */
extern void java_destroy_lexer (java_lexer *);

#define JAVA_LINE_MAX 80

/* Build a location compound integer */
#define BUILD_LOCATION() ((ctxp->elc.line << 12) | (ctxp->elc.col & 0xfff))

/* Those macros are defined differently if we compile jc1-lite
   (JC1_LITE defined) or jc1.  */
#ifdef JC1_LITE

#define DCONST0 0
#define REAL_VALUE_TYPE int
#define GET_IDENTIFIER(S) xstrdup ((S))
#define REAL_VALUE_ATOF(LIT,MODE) 0
#define REAL_VALUE_ISINF(VALUE)   0
#define REAL_VALUE_ISNAN(VALUE)   0
#define SET_REAL_VALUE_ATOF(TARGET,SOURCE)
#define FLOAT_TYPE_NODE 0
#define DOUBLE_TYPE_NODE 0
#define SET_MODIFIER_CTX(TOKEN) java_lval->value = (TOKEN)
#define GET_TYPE_PRECISION(NODE) 4
#define BUILD_OPERATOR(TOKEN)	return TOKEN
#define BUILD_OPERATOR2(TOKEN)	return ASSIGN_ANY_TK
#define SET_LVAL_NODE(NODE)
#define SET_LVAL_NODE_TYPE(NODE, TYPE)
#define BUILD_ID_WFL(EXP) (EXP)
#define JAVA_FLOAT_RANGE_ERROR(S) {}
#define JAVA_INTEGRAL_RANGE_ERROR(S) do { } while (0)

#else

#define DCONST0 dconst0
#define GET_IDENTIFIER(S) get_identifier ((S))
#define SET_REAL_VALUE_ATOF(TARGET,SOURCE) (TARGET) = (SOURCE)
#define FLOAT_TYPE_NODE float_type_node
#define DOUBLE_TYPE_NODE double_type_node
/* Set modifier_ctx according to TOKEN */
#define SET_MODIFIER_CTX(TOKEN)						   \
  {									   \
    ctxp->modifier_ctx [(TOKEN)-PUBLIC_TK] = build_wfl_node (NULL_TREE); \
    java_lval->value = (TOKEN)-PUBLIC_TK;				   \
  }
/* Type precision for long */
#define GET_TYPE_PRECISION(NODE) TYPE_PRECISION (long_type_node) / 8;
/* Build an operator tree node and return TOKEN */
#define BUILD_OPERATOR(TOKEN)				\
  {							\
    java_lval->operator.token = (TOKEN);		\
    java_lval->operator.location = BUILD_LOCATION();	\
    return (TOKEN);					\
  }

/* Build an operator tree node but return ASSIGN_ANY_TK */
#define BUILD_OPERATOR2(TOKEN)				\
  {							\
    java_lval->operator.token = (TOKEN);		\
    java_lval->operator.location = BUILD_LOCATION();	\
    return ASSIGN_ANY_TK;				\
  }
/* Set java_lval->node and TREE_TYPE(java_lval->node) in macros */
#define SET_LVAL_NODE(NODE) java_lval->node = (NODE)
#define SET_LVAL_NODE_TYPE(NODE,TYPE)		\
  {						\
    java_lval->node = (NODE);			\
    TREE_TYPE (java_lval->node) = (TYPE);	\
  }
/* Wrap identifier around a wfl */
#define BUILD_ID_WFL(EXP) build_wfl_node ((EXP))
/* Special ways to report error on numeric literals  */
#define JAVA_FLOAT_RANGE_ERROR(m)					  \
  {									  \
    char msg [1024];							  \
    int i = ctxp->c_line->current;					  \
    ctxp->c_line->current = number_beginning;				  \
    sprintf (msg, "Floating point literal exceeds range of `%s'", (m)); \
    java_lex_error (msg, 0);						  \
    ctxp->c_line->current = i;						  \
  }
#define JAVA_INTEGRAL_RANGE_ERROR(m)		\
  do {						\
    int i = ctxp->c_line->current;		\
    ctxp->c_line->current = number_beginning;	\
    java_lex_error (m, 0);			\
    ctxp->c_line->current = i;			\
  } while (0)

#endif /* Definitions for jc1 compilation only */

/* Macros to decode character ranges */
#define RANGE(c, l, h)           (((c) >= l && (c) <= h))
#define JAVA_WHITE_SPACE_P(c) (c == ' ' || c == '\t' || c == '\f')
#define JAVA_START_CHAR_P(c) ((c < 128					      \
			       && (ISIDST (c) || c == '$'))		      \
                              || (c >= 128 && java_start_char_p (c)))
#define JAVA_PART_CHAR_P(c) ((c < 128					      \
			       && (ISIDNUM (c)				      \
				   || c == '$'				      \
				   || c == 0x0000			      \
				   || RANGE (c, 0x01, 0x08)		      \
				   || RANGE (c, 0x0e, 0x1b)		      \
				   || c == 0x7f))			      \
                              || (c >= 128 && java_part_char_p (c)))
#define JAVA_ASCII_DIGIT(c)    ISDIGIT (c)
#define JAVA_ASCII_OCTDIGIT(c) RANGE (c, '0', '7')
#define JAVA_ASCII_HEXDIGIT(c) ISXDIGIT (c)
#define JAVA_ASCII_FPCHAR(c)   (RANGE (c, 'd', 'f') || RANGE (c, 'D', 'F') || \
				c == '.' || JAVA_ASCII_DIGIT (c))
#define JAVA_FP_SUFFIX(c)      (c == 'D' || c == 'd' || c == 'f' || c == 'F')
#define JAVA_FP_EXP(c)         (c == 'E' || c == 'F')
#define JAVA_FP_PM(c)          (c == '-' || c == '+')
#define JAVA_ASCII_LETTER(c)   ISALPHA (c)

/* Constants  */
#define JAVA_READ_BUFFER 256
#define JAVA_CHAR_ERROR -2
#define UEOF -1

#endif /* ! GCC_JAVA_LEX_H */
