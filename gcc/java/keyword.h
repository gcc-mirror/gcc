/* C code produced by gperf version 2.7 */
/* Command-line: gperf -L C -C -F , 0 -p -t -j1 -i 1 -g -o -N java_keyword -k1,4,$ keyword.gperf  */
/* Keyword definition for the GNU compiler for the Java(TM) language.
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.
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

struct java_keyword { const char *const name; const int token; };
#ifdef __GNUC__
__inline
#endif
static unsigned int hash		PARAMS ((const char *, unsigned int));
#ifdef __GNUC__
__inline
#endif
const struct java_keyword *java_keyword	PARAMS ((const char *, unsigned int));

#define TOTAL_KEYWORDS 52
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 12
#define MIN_HASH_VALUE 7
#define MAX_HASH_VALUE 85
/* maximum key range = 79, duplicates = 0 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static const unsigned char asso_values[] =
    {
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86,  1, 34,  3,
       1,  1, 18,  7, 21, 28, 86, 14,  1, 86,
      18, 20, 37, 86, 15,  6,  2,  5, 40, 36,
      86, 36, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
      86, 86, 86, 86, 86, 86
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      case 3:
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

#ifdef __GNUC__
__inline
#endif
const struct java_keyword *
java_keyword (str, len)
     register const char *str;
     register unsigned int len;
{
  static const struct java_keyword wordlist[] =
    {
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"else", ELSE_TK},
      {"true", TRUE_TK},
      {"case", CASE_TK},
      {"assert", ASSERT_TK},
      {"default", DEFAULT_TK},
      {"", 0},
      {"abstract", ABSTRACT_TK},
      {"continue", CONTINUE_TK},
      {"extends", EXTENDS_TK},
      {"const", CONST_TK},
      {"static", STATIC_TK},
      {"this", THIS_TK},
      {"long", LONG_TK},
      {"class", CLASS_TK},
      {"", 0},
      {"synchronized", SYNCHRONIZED_TK},
      {"do", DO_TK},
      {"null", NULL_TK},
      {"final", FINAL_TK},
      {"float", FLOAT_TK},
      {"super", SUPER_TK},
      {"short", SHORT_TK},
      {"", 0},
      {"false", FALSE_TK},
      {"transient", TRANSIENT_TK},
      {"catch", CATCH_TK},
      {"int", INT_TK},
      {"throws", THROWS_TK},
      {"switch", SWITCH_TK},
      {"for", FOR_TK},
      {"char", CHAR_TK},
      {"", 0},
      {"interface", INTERFACE_TK},
      {"byte", BYTE_TK},
      {"try", TRY_TK},
      {"double", DOUBLE_TK},
      {"while", WHILE_TK},
      {"return", RETURN_TK},
      {"implements", IMPLEMENTS_TK},
      {"void", VOID_TK},
      {"public", PUBLIC_TK},
      {"if", IF_TK},
      {"protected", PROTECTED_TK},
      {"volatile", VOLATILE_TK},
      {"goto", GOTO_TK},
      {"", 0},
      {"native", NATIVE_TK},
      {"break", BREAK_TK},
      {"", 0},
      {"import", IMPORT_TK},
      {"new", NEW_TK},
      {"instanceof", INSTANCEOF_TK},
      {"package", PACKAGE_TK},
      {"boolean", BOOLEAN_TK},
      {"", 0},
      {"finally", FINALLY_TK},
      {"throw", THROW_TK},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"strictfp", STRICT_TK},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"private", PRIVATE_TK}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
