/* Language lexer definitions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
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

#ifndef HAVE_ICONV_H
#undef HAVE_ICONV
#endif

#if defined HAVE_ICONV
#include <iconv.h>
#endif /* HAVE_ICONV */

/* Default encoding to use if no encoding is specified.  */
#define DEFAULT_ENCODING "UTF-8"

typedef struct java_lc_s GTY(()) {
  int line;		/* line number (1-based) */
  int col;		/* column number number (1-based) */
} java_lc;

struct java_lexer
{
  /* The file from which we're reading.  */
  FILE *finput;

  /* Number of consecutive backslashes we've read.  */
  int bs_count;

  /* Next available Unicode character.
   * This is post-Unicode-escape-processing. -1 if EOF. */
  int next_unicode;

  /* True if next_unicode is next available character, or EOF. */
  bool avail_unicode;

  /* Number of source columns of the previous Unicode character (next_unicode).
     If next_unicode==-2, then this is the number of columns of the previous
     Unicode character (most recent result of java_{get,peek}_unicode). */
  int next_columns;

  /* If nonzero, a value that was pushed back.  This is a unicode character,
     but (unlike next_unicode) is pre-'\uXXXX'-processing.  It is also used
     when a '\r' is *not* followed by a '\n'. */
  unicode_t unget_value;

  /* Name of the character encoding we're using.  */
  const char *encoding;

  /* Current source position. */
  java_lc position;

#ifndef USE_MAPPED_LOCATION
  java_lc token_start;		     /* Error's line column info */
#endif

#ifdef HAVE_ICONV
  /* Nonzero if we've read any bytes.  We only recognize the
     byte-order-marker (BOM) as the first word.  */
  unsigned int read_anything : 1;

  /* Nonzero if we have to byte swap.  */
  unsigned int byte_swap : 1;

  /* Nonzero if we're using the fallback decoder.  */
  unsigned int use_fallback : 1;

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
#ifdef USE_MAPPED_LOCATION
#define BUILD_LOCATION() input_location
#else
#define BUILD_LOCATION() ((ctxp->lexer->token_start.line << 12) \
			  | (ctxp->lexer->token_start.col & 0xfff))
#endif

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
#define BUILD_ID_WFL(EXP) (EXP)
#define JAVA_FLOAT_RANGE_ERROR(S) {}
#define JAVA_RANGE_ERROR(S) do { } while (0)

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
/* Wrap identifier around a wfl */
#define BUILD_ID_WFL(EXP) build_wfl_node ((EXP))
/* Special ways to report error on numeric literals  */
#define JAVA_FLOAT_RANGE_ERROR(m)					\
  {									\
    char *msg = xmalloc (100 + strlen (m));				\
    sprintf (msg, "Floating point literal exceeds range of `%s'", (m));	\
    JAVA_RANGE_ERROR(msg);						\
    free (msg);								\
  }
#define JAVA_RANGE_ERROR(msg)						\
  do {									\
    int save_col = ctxp->lexer->position.col;				\
    ctxp->lexer->position.col = number_beginning;			\
    java_lex_error (msg, 0);						\
    ctxp->lexer->position.col = save_col;				\
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
