/* Language lexer definitions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

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
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#ifndef JV_LEX_H
#define JV_LEX_H

#include <setjmp.h>		/* set_float_handler argument uses it */

/* Extern global variables declarations  */
extern FILE *finput;
extern int   lineno;

/* A Unicode character, as read from the input file  */
typedef unsigned short unicode_t;

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
  char      *unicode_escape_p;	/* The maching char was a unicode escape */
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

typedef struct _java_lc {
  int line;
  int prev_col;
  int col;
} java_lc;


#define JAVA_LINE_MAX 80

/* Macro to read and unread bytes */
#define UNGETC(c) ungetc(c, finput)
#define GETC()    getc(finput)

/* Build a location compound integer */
#define BUILD_LOCATION() ((ctxp->elc.line << 12) | (ctxp->elc.col & 0xfff))

/* Those macros are defined differently if we compile jc1-lite
   (JC1_LITE defined) or jc1.  */
#ifdef JC1_LITE

#define DCONST0 0
#define REAL_VALUE_TYPE int
#define SET_FLOAT_HANDLER(H)
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
#define JAVA_INTEGRAL_RANGE_ERROR(S) {}

#else

extern void set_float_handler PARAMS ((jmp_buf));
#define SET_FLOAT_HANDLER(H) set_float_handler ((H))
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
    sprintf (msg, "Floating pointer literal exceeds range of `%s'", (m)); \
    java_lex_error (msg, 0);						  \
    ctxp->c_line->current = i;						  \
  }
#define JAVA_INTEGRAL_RANGE_ERROR(m)		\
  {						\
    int i = ctxp->c_line->current;		\
    ctxp->c_line->current = number_beginning;	\
    java_lex_error (m, 0);			\
    ctxp->c_line->current = i;			\
  }

#endif /* Definitions for jc1 compilation only */

/* Macros to decode character ranges */
#define RANGE(c, l, h)           (((c) >= l && (c) <= h))
#define JAVA_WHITE_SPACE_P(c) (c == ' ' || c == '\t' || c == '\f')
#define JAVA_ID_CHAR_P(c)     ((c < 128 && (RANGE (c, 'A', 'Z') ||	\
					    RANGE (c, 'a', 'z') ||	\
					    RANGE (c, '0', '9') ||	\
					    c == '_'         ||		\
					    c == '$'))       ||		\
			       (c > 127 && java_letter_or_digit_p (c)))
#define JAVA_ASCII_DIGIT(c)    RANGE(c,'0', '9')
#define JAVA_ASCII_OCTDIGIT(c) RANGE(c,'0', '7')
#define JAVA_ASCII_HEXDIGIT(c) (RANGE(c,'0', '9') || 	\
				RANGE(c,'a', 'f') ||	\
				RANGE(c,'A', 'F'))
#define JAVA_ASCII_FPCHAR(c)   (RANGE(c,'d', 'f') || RANGE(c,'D', 'F') || \
				c == '.' || JAVA_ASCII_DIGIT (c))
#define JAVA_FP_SUFFIX(c)      (c == 'D' || c == 'd' || c == 'f' || c == 'F')
#define JAVA_FP_EXP(c)         (c == 'E' || c == 'F')
#define JAVA_FP_PM(c)          (c == '-' || c == '+')
#define JAVA_ASCII_LETTER(c)   (RANGE(c,'a', 'z') || RANGE(c,'A', 'Z'))
#define JAVA_DIGIT_P(c)							      \
   (RANGE (c, 0x030, 0x039) || /* ISO-Latin-1 (and ASCII) digits ('0'-'9') */ \
    RANGE (c, 0x660, 0x669) || /* Arabic-Indic digits */		      \
    RANGE (c, 0x6F0, 0x6F9) || /* Eastern Arabic-Indic digits */	      \
    RANGE (c, 0x966, 0x96F) || /* Devanagari digits */			      \
    RANGE (c, 0x9E6, 0x9EF) || /* Bengali digits */			      \
    RANGE (c, 0xA66, 0xA6F) || /* Gurmukhi digits */			      \
    RANGE (c, 0xAE6, 0xAEF) || /* Gujarati digits */			      \
    RANGE (c, 0xB66, 0xB6F) || /* Oriya digits */			      \
    RANGE (c, 0xBE7, 0xBEF) || /* Tamil digits */			      \
    RANGE (c, 0xC66, 0xC6F) || /* Telugu digits */			      \
    RANGE (c, 0xCE6, 0xCEF) || /* Kannada digits */			      \
    RANGE (c, 0xD66, 0xD6F) || /* Malayalam digits */			      \
    RANGE (c, 0xE50, 0xE59) || /* Thai digits */			      \
    RANGE (c, 0xED0, 0xED9))   /* Lao digits */

/* This is not to be used as a stand alone macro. Use JAVA_ID_CHAR_P()
   or the forcoming JAVA_LETTER_OR_DIGIT_P() instead.
   It need to be split by region. FIXME.  */
#define _JAVA_LETTER_OR_DIGIT_P(c)		\
   (RANGE (c, 0x00C0, 0x00D6) ||		\
    RANGE (c, 0x00D8, 0x00F6) ||		\
    RANGE (c, 0x00F8, 0x01F5) ||		\
    RANGE (c, 0x01FA, 0x0217) ||		\
    RANGE (c, 0x0250, 0x02A8) ||		\
    RANGE (c, 0x02B0, 0x02DE) ||		\
    RANGE (c, 0x02E0, 0x02E9) ||		\
    RANGE (c, 0x0300, 0x0345) ||		\
    RANGE (c, 0x0360, 0x0361) ||		\
    RANGE (c, 0x0374, 0x0375) ||		\
    c == 0x037A            ||			\
    c == 0x037E            ||			\
    RANGE (c, 0x0384, 0x038A) ||		\
    c == 0x038C            ||			\
    c == 0x038E            ||			\
    RANGE (c, 0x038F, 0x03A1) ||		\
    RANGE (c, 0x03A3, 0x03CE) ||		\
    RANGE (c, 0x03D0, 0x03D6) ||		\
    RANGE (c, 0x03DA, 0x03E2) ||		\
    c == 0x03DA            ||			\
    c == 0x03DC            ||			\
    c == 0x03DE            ||			\
    c == 0x03E0            ||			\
    RANGE (c, 0x03E2, 0x03F3) ||		\
    RANGE (c, 0x0401, 0x040C) ||		\
    RANGE (c, 0x040E, 0x044F) ||		\
    RANGE (c, 0x0451, 0x045C) ||		\
    RANGE (c, 0x045E, 0x0486) ||		\
    RANGE (c, 0x0490, 0x04C4) ||		\
    RANGE (c, 0x04C7, 0x04C8) ||		\
    RANGE (c, 0x04CB, 0x04CC) ||		\
    RANGE (c, 0x04D0, 0x04EB) ||		\
    RANGE (c, 0x04EE, 0x04F5) ||		\
    RANGE (c, 0x04F8, 0x04F9) ||		\
    RANGE (c, 0x0531, 0x0556) ||		\
    RANGE (c, 0x0559, 0x055F) ||		\
    RANGE (c, 0x0561, 0x0587) ||		\
    c == 0x0589            ||			\
    RANGE (c, 0x05B0, 0x05B9) ||		\
    RANGE (c, 0x05BB, 0x05C3) ||		\
    RANGE (c, 0x05D0, 0x05EA) ||		\
    RANGE (c, 0x05F0, 0x05F4) ||		\
    c == 0x060C            ||			\
    c == 0x061B            ||			\
    c == 0x061F            ||			\
    c == 0x0621            ||			\
    RANGE (c, 0x0622, 0x063A) ||		\
    RANGE (c, 0x0640, 0x0652) ||		\
    RANGE (c, 0x0660, 0x066D) ||		\
    RANGE (c, 0x0670, 0x06B7) ||		\
    RANGE (c, 0x06BA, 0x06BE) ||		\
    RANGE (c, 0x06C0, 0x06CE) ||		\
    RANGE (c, 0x06D0, 0x06ED) ||		\
    RANGE (c, 0x06F0, 0x06F9) ||		\
    RANGE (c, 0x0901, 0x0903) ||		\
    RANGE (c, 0x0905, 0x0939) ||		\
    RANGE (c, 0x093C, 0x094D) ||		\
    RANGE (c, 0x0950, 0x0954) ||		\
    RANGE (c, 0x0958, 0x0970) ||		\
    RANGE (c, 0x0981, 0x0983) ||		\
    RANGE (c, 0x0985, 0x098C) ||		\
    RANGE (c, 0x098F, 0x0990) ||		\
    RANGE (c, 0x0993, 0x09A8) ||		\
    RANGE (c, 0x09AA, 0x09B0) ||		\
    c == 0x09B2            ||			\
    RANGE (c, 0x09B6, 0x09B9) ||		\
    c == 0x09BC            ||			\
    c == 0x09BE            ||			\
    RANGE (c, 0x09BF, 0x09C4) ||		\
    RANGE (c, 0x09C7, 0x09C8) ||		\
    RANGE (c, 0x09CB, 0x09CD) ||		\
    c == 0x09D7            ||			\
    RANGE (c, 0x09DC, 0x09DD) ||		\
    RANGE (c, 0x09DF, 0x09E3) ||		\
    RANGE (c, 0x09E6, 0x09FA) ||		\
    c == 0x0A02            ||			\
    RANGE (c, 0x0A05, 0x0A0A) ||		\
    RANGE (c, 0x0A0F, 0x0A10) ||		\
    RANGE (c, 0x0A13, 0x0A28) ||		\
    RANGE (c, 0x0A2A, 0x0A30) ||		\
    RANGE (c, 0x0A32, 0x0A33) ||		\
    RANGE (c, 0x0A35, 0x0A36) ||		\
    RANGE (c, 0x0A38, 0x0A39) ||		\
    c == 0x0A3C            ||			\
    c == 0x0A3E            ||			\
    RANGE (c, 0x0A3F, 0x0A42) ||		\
    RANGE (c, 0x0A47, 0x0A48) ||		\
    RANGE (c, 0x0A4B, 0x0A4D) ||		\
    RANGE (c, 0x0A59, 0x0A5C) ||		\
    c == 0x0A5E            ||			\
    RANGE (c, 0x0A66, 0x0A74) ||		\
    RANGE (c, 0x0A81, 0x0A83) ||		\
    RANGE (c, 0x0A85, 0x0A8B) ||		\
    c == 0x0A8D            ||			\
    c == 0x0A8F            ||			\
    RANGE (c, 0x0A90, 0x0A91) ||		\
    RANGE (c, 0x0A93, 0x0AA8) ||		\
    RANGE (c, 0x0AAA, 0x0AB0) ||		\
    RANGE (c, 0x0AB2, 0x0AB3) ||		\
    RANGE (c, 0x0AB5, 0x0AB9) ||		\
    RANGE (c, 0x0ABC, 0x0AC5) ||		\
    RANGE (c, 0x0AC7, 0x0AC9) ||		\
    RANGE (c, 0x0ACB, 0x0ACD) ||		\
    c == 0x0AD0            ||			\
    c == 0x0AE0            ||			\
    RANGE (c, 0x0AE6, 0x0AEF) ||		\
    RANGE (c, 0x0B01, 0x0B03) ||		\
    RANGE (c, 0x0B05, 0x0B0C) ||		\
    RANGE (c, 0x0B0F, 0x0B10) ||		\
    RANGE (c, 0x0B13, 0x0B28) ||		\
    RANGE (c, 0x0B2A, 0x0B30) ||		\
    RANGE (c, 0x0B32, 0x0B33) ||		\
    RANGE (c, 0x0B36, 0x0B39) ||		\
    RANGE (c, 0x0B3C, 0x0B43) ||		\
    RANGE (c, 0x0B47, 0x0B48) ||		\
    RANGE (c, 0x0B4B, 0x0B4D) ||		\
    RANGE (c, 0x0B56, 0x0B57) ||		\
    RANGE (c, 0x0B5C, 0x0B5D) ||		\
    RANGE (c, 0x0B5F, 0x0B61) ||		\
    RANGE (c, 0x0B66, 0x0B70) ||		\
    RANGE (c, 0x0B82, 0x0B83) ||		\
    RANGE (c, 0x0B85, 0x0B8A) ||		\
    RANGE (c, 0x0B8E, 0x0B90) ||		\
    RANGE (c, 0x0B92, 0x0B95) ||		\
    RANGE (c, 0x0B99, 0x0B9A) ||		\
    c == 0x0B9C            ||			\
    c == 0x0B9E            ||			\
    c == 0x0B9F            ||			\
    RANGE (c, 0x0BA3, 0x0BA4) ||		\
    RANGE (c, 0x0BA8, 0x0BAA) ||		\
    RANGE (c, 0x0BAE, 0x0BB5) ||		\
    RANGE (c, 0x0BB7, 0x0BB9) ||		\
    RANGE (c, 0x0BBE, 0x0BC2) ||		\
    RANGE (c, 0x0BC6, 0x0BC8) ||		\
    RANGE (c, 0x0BCA, 0x0BCD) ||		\
    c == 0x0BD7            ||			\
    RANGE (c, 0x0BE7, 0x0BF2) ||		\
    RANGE (c, 0x0C01, 0x0C03) ||		\
    RANGE (c, 0x0C05, 0x0C0C) ||		\
    RANGE (c, 0x0C0E, 0x0C10) ||		\
    RANGE (c, 0x0C12, 0x0C28) ||		\
    RANGE (c, 0x0C2A, 0x0C33) ||		\
    RANGE (c, 0x0C35, 0x0C39) ||		\
    RANGE (c, 0x0C3E, 0x0C44) ||		\
    RANGE (c, 0x0C46, 0x0C48) ||		\
    RANGE (c, 0x0C4A, 0x0C4D) ||		\
    RANGE (c, 0x0C55, 0x0C56) ||		\
    RANGE (c, 0x0C60, 0x0C61) ||		\
    RANGE (c, 0x0C66, 0x0C6F) ||		\
    RANGE (c, 0x0C82, 0x0C83) ||		\
    RANGE (c, 0x0C85, 0x0C8C) ||		\
    RANGE (c, 0x0C8E, 0x0C90) ||		\
    RANGE (c, 0x0C92, 0x0CA8) ||		\
    RANGE (c, 0x0CAA, 0x0CB3) ||		\
    RANGE (c, 0x0CB5, 0x0CB9) ||		\
    RANGE (c, 0x0CBE, 0x0CC4) ||		\
    RANGE (c, 0x0CC6, 0x0CC8) ||		\
    RANGE (c, 0x0CCA, 0x0CCD) ||		\
    RANGE (c, 0x0CD5, 0x0CD6) ||		\
    c == 0x0CDE            ||			\
    c == 0x0CE0            ||			\
    c == 0x0CE1            ||			\
    RANGE (c, 0x0CE6, 0x0CEF) ||		\
    RANGE (c, 0x0D02, 0x0D03) ||		\
    RANGE (c, 0x0D05, 0x0D0C) ||		\
    RANGE (c, 0x0D0E, 0x0D10) ||		\
    RANGE (c, 0x0D12, 0x0D28) ||		\
    RANGE (c, 0x0D2A, 0x0D39) ||		\
    RANGE (c, 0x0D3E, 0x0D43) ||		\
    RANGE (c, 0x0D46, 0x0D48) ||		\
    RANGE (c, 0x0D4A, 0x0D4D) ||		\
    c == 0x0D57            ||			\
    RANGE (c, 0x0D60, 0x0D61) ||		\
    RANGE (c, 0x0D66, 0x0D6F) ||		\
    RANGE (c, 0x0E01, 0x0E3A) ||		\
    RANGE (c, 0x0E3F, 0x0E5B) ||		\
    RANGE (c, 0x0E81, 0x0E82) ||		\
    c == 0x0E84            ||			\
    RANGE (c, 0x0E87, 0x0E88) ||		\
    c == 0x0E8A            ||			\
    c == 0x0E8D            ||			\
    RANGE (c, 0x0E94, 0x0E97) ||		\
    RANGE (c, 0x0E99, 0x0E9F) ||		\
    RANGE (c, 0x0EA1, 0x0EA3) ||		\
    c == 0x0EA5            ||			\
    c == 0x0EA7            ||			\
    RANGE (c, 0x0EAA, 0x0EAB) ||		\
    RANGE (c, 0x0EAD, 0x0EB9) ||		\
    RANGE (c, 0x0EBB, 0x0EBD) ||		\
    RANGE (c, 0x0EC0, 0x0EC4) ||		\
    c == 0x0EC6            ||			\
    c == 0x0EC8            ||			\
    RANGE (c, 0x0EC9, 0x0ECD) ||		\
    RANGE (c, 0x0ED0, 0x0ED9) ||		\
    RANGE (c, 0x0EDC, 0x0EDD) ||		\
    RANGE (c, 0x10A0, 0x10C5) ||		\
    RANGE (c, 0x10D0, 0x10F6) ||		\
    c == 0x10FB            ||			\
    RANGE (c, 0x1100, 0x1159) ||		\
    RANGE (c, 0x115F, 0x11A2) ||		\
    RANGE (c, 0x11A8, 0x11F9) ||		\
    RANGE (c, 0x1E00, 0x1E9A) ||		\
    RANGE (c, 0x1EA0, 0x1EF9) ||		\
    RANGE (c, 0x1F00, 0x1F15) ||		\
    RANGE (c, 0x1F18, 0x1F1D) ||		\
    RANGE (c, 0x1F20, 0x1F45) ||		\
    RANGE (c, 0x1F48, 0x1F4D) ||		\
    RANGE (c, 0x1F50, 0x1F57) ||		\
    c == 0x1F59            ||			\
    c == 0x1F5B            ||			\
    c == 0x1F5D            ||			\
    RANGE (c, 0x1F5F, 0x1F7D) ||		\
    RANGE (c, 0x1F80, 0x1FB4) ||		\
    RANGE (c, 0x1FB6, 0x1FC4) ||		\
    RANGE (c, 0x1FC6, 0x1FD3) ||		\
    RANGE (c, 0x1FD6, 0x1FDB) ||		\
    RANGE (c, 0x1FDD, 0x1FEF) ||		\
    RANGE (c, 0x1FF2, 0x1FF4) ||		\
    RANGE (c, 0x1FF6, 0x1FFE) ||		\
    RANGE (c, 0x3041, 0x3094) ||		\
    RANGE (c, 0x3099, 0x309E) ||		\
    RANGE (c, 0x30A1, 0x30FE) ||		\
    RANGE (c, 0x3105, 0x312C) ||		\
    RANGE (c, 0x3131, 0x318E) ||		\
    RANGE (c, 0x3190, 0x319F) ||		\
    RANGE (c, 0x3200, 0x321C) ||		\
    RANGE (c, 0x3220, 0x3243) ||		\
    RANGE (c, 0x3260, 0x327B) ||		\
    RANGE (c, 0x327F, 0x32B0) ||		\
    RANGE (c, 0x32C0, 0x32CB) ||		\
    RANGE (c, 0x32D0, 0x32FE) ||		\
    RANGE (c, 0x3300, 0x3376) ||		\
    RANGE (c, 0x337B, 0x33DD) ||		\
    RANGE (c, 0x33E0, 0x33FE) ||		\
    RANGE (c, 0x3400, 0x9FA5) ||		\
    RANGE (c, 0xF900, 0xFA2D) ||		\
    RANGE (c, 0xFB00, 0xFB06) ||		\
    RANGE (c, 0xFB13, 0xFB17) ||		\
    RANGE (c, 0xFB1E, 0xFB36) ||		\
    RANGE (c, 0xFB38, 0xFB3C) ||		\
    c == 0xFB3E            ||			\
    c == 0xFB40            ||			\
    c == 0xFB41            ||			\
    c == 0xFB43            ||			\
    c == 0xFB44            ||			\
    c == 0xFB46            ||			\
    RANGE (c, 0xFB47, 0xFBB1) ||		\
    RANGE (c, 0xFBD3, 0xFD3F) ||		\
    RANGE (c, 0xFD50, 0xFD8F) ||		\
    RANGE (c, 0xFD92, 0xFDC7) ||		\
    RANGE (c, 0xFDF0, 0xFDFB) ||		\
    RANGE (c, 0xFE70, 0xFE72) ||		\
    c == 0xFE74            ||			\
    c == 0xFE76            ||			\
    RANGE (c, 0xFE77, 0xFEFC) ||		\
    RANGE (c, 0xFF10, 0xFF19) ||		\
    RANGE (c, 0xFF21, 0xFF3A) ||		\
    RANGE (c, 0xFF41, 0xFF5A) ||		\
    RANGE (c, 0xFF66, 0xFFBE) ||		\
    RANGE (c, 0xFFC2, 0xFFC7) ||		\
    RANGE (c, 0xFFCA, 0xFFCF) ||		\
    RANGE (c, 0xFFD2, 0xFFD7) ||		\
    RANGE (c, 0xFFDA, 0xFFDC))

/* Constants  */
#define JAVA_CHAR_ERROR 0xFFC1	/* This is an illegal unicode!?! FIXME */
#define JAVA_READ_BUFFER 256
#define UEOF (unicode_t)0xffff

#endif
