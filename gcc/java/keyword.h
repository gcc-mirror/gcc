/* C code produced by gperf version 2.7.2 */
/* Command-line: gperf -L C -C -F ', 0' -p -t -j1 -i 1 -g -o -N java_keyword -k'1,4,$' keyword.gperf  */
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

#define TOTAL_KEYWORDS 51
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 12
#define MIN_HASH_VALUE 7
#define MAX_HASH_VALUE 95
/* maximum key range = 89, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static const unsigned char asso_values[] =
    {
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96,  9, 17,  3,
       1,  1, 20, 13, 15, 29, 96, 21,  1, 96,
      35, 39,  1, 96, 15,  6,  2,  1, 41, 17,
      96,  7, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96
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
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"", 0},
      {"else", ELSE_TK},
      {"true", TRUE_TK},
      {"case", CASE_TK},
      {"", 0},
      {"public", PUBLIC_TK},
      {"try", TRY_TK},
      {"protected", PROTECTED_TK},
      {"continue", CONTINUE_TK},
      {"extends", EXTENDS_TK},
      {"const", CONST_TK},
      {"static", STATIC_TK},
      {"this", THIS_TK},
      {"default", DEFAULT_TK},
      {"class", CLASS_TK},
      {"abstract", ABSTRACT_TK},
      {"synchronized", SYNCHRONIZED_TK},
      {"byte", BYTE_TK},
      {"while", WHILE_TK},
      {"double", DOUBLE_TK},
      {"catch", CATCH_TK},
      {"super", SUPER_TK},
      {"short", SHORT_TK},
      {"switch", SWITCH_TK},
      {"package", PACKAGE_TK},
      {"long", LONG_TK},
      {"false", FALSE_TK},
      {"", 0},
      {"int", INT_TK},
      {"final", FINAL_TK},
      {"float", FLOAT_TK},
      {"char", CHAR_TK},
      {"for", FOR_TK},
      {"", 0},
      {"interface", INTERFACE_TK},
      {"null", NULL_TK},
      {"do", DO_TK},
      {"finally", FINALLY_TK},
      {"strictfp", STRICT_TK},
      {"", 0},
      {"implements", IMPLEMENTS_TK},
      {"void", VOID_TK},
      {"transient", TRANSIENT_TK},
      {"", 0},
      {"private", PRIVATE_TK},
      {"if", IF_TK},
      {"break", BREAK_TK},
      {"throws", THROWS_TK},
      {"", 0},
      {"new", NEW_TK},
      {"", 0},
      {"return", RETURN_TK},
      {"", 0},
      {"volatile", VOLATILE_TK},
      {"boolean", BOOLEAN_TK},
      {"instanceof", INSTANCEOF_TK},
      {"", 0},
      {"throw", THROW_TK},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"", 0},
      {"native", NATIVE_TK},
      {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"import", IMPORT_TK},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"goto", GOTO_TK}
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
