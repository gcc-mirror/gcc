/* C code produced by gperf version 2.7.1 (19981006 egcs) */
/* Command-line: gperf -L C -F , 0 -p -t -j1 -i 1 -g -o -N java_keyword -k1,3,$ keyword.gperf  */
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

struct java_keyword { const char *name; int token; };

#define TOTAL_KEYWORDS 50
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 12
#define MIN_HASH_VALUE 6
#define MAX_HASH_VALUE 86
/* maximum key range = 81, duplicates = 0 */

#ifdef __GNUC__
__inline
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static unsigned char asso_values[] =
    {
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 18, 37, 38,
      27,  1, 30,  3, 12,  8, 87,  2, 11, 87,
       8,  1,  5, 87, 24,  1,  1, 30,  2, 36,
      87,  1, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
      87, 87, 87, 87, 87, 87
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 3:
        hval += asso_values[(unsigned char)str[2]];
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
struct java_keyword *
java_keyword (str, len)
     register const char *str;
     register unsigned int len;
{
  static struct java_keyword wordlist[] =
    {
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"try", TRY_TK},
      {"else", ELSE_TK},
      {"short", SHORT_TK},
      {"goto", GOTO_TK},
      {"extends", EXTENDS_TK},
      {"", 0}, {"", 0},
      {"int", INT_TK},
      {"this", THIS_TK},
      {"", 0},
      {"native", NATIVE_TK},
      {"", 0}, {"", 0},
      {"interface", INTERFACE_TK},
      {"import", IMPORT_TK},
      {"private", PRIVATE_TK},
      {"volatile", VOLATILE_TK},
      {"", 0},
      {"implements", IMPLEMENTS_TK},
      {"", 0},
      {"long", LONG_TK},
      {"switch", SWITCH_TK},
      {"abstract", ABSTRACT_TK},
      {"transient", TRANSIENT_TK},
      {"do", DO_TK},
      {"", 0},
      {"throws", THROWS_TK},
      {"", 0},
      {"null", NULL_TK},
      {"super", SUPER_TK},
      {"true", TRUE_TK},
      {"float", FLOAT_TK},
      {"", 0},
      {"return", RETURN_TK},
      {"if", IF_TK},
      {"void", VOID_TK},
      {"protected", PROTECTED_TK},
      {"byte", BYTE_TK},
      {"case", CASE_TK},
      {"break", BREAK_TK},
      {"finally", FINALLY_TK},
      {"false", FALSE_TK},
      {"synchronized", SYNCHRONIZED_TK},
      {"instanceof", INSTANCEOF_TK},
      {"while", WHILE_TK},
      {"package", PACKAGE_TK},
      {"const", CONST_TK},
      {"boolean", BOOLEAN_TK},
      {"final", FINAL_TK},
      {"continue", CONTINUE_TK},
      {"catch", CATCH_TK},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"class", CLASS_TK},
      {"static", STATIC_TK},
      {"double", DOUBLE_TK},
      {"default", DEFAULT_TK},
      {"throw", THROW_TK},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"", 0}, {"", 0}, {"", 0}, {"", 0}, {"", 0},
      {"for", FOR_TK},
      {"", 0},
      {"new", NEW_TK},
      {"char", CHAR_TK},
      {"", 0},
      {"public", PUBLIC_TK}
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
