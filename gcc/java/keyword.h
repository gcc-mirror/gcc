/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -p -t -j1 -i 1 -g -o -N java_keyword -k1,3,$ keyword.gperf  */
/* Keyword definitions for the GNU compiler for the Java(TM) language.
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

struct java_keyword { char *name; int token; };

#define TOTAL_KEYWORDS 50
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 12
#define MIN_HASH_VALUE 6
#define MAX_HASH_VALUE 86
/* maximum key range = 81, duplicates = 0 */

#ifdef __GNUC__
inline
#endif
static unsigned int
hash (str, len)
     register char *str;
     register int unsigned len;
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
     87,  1, 87, 87, 87, 87, 87, 87,
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 3:
        hval += asso_values[str[2]];
      case 2:
      case 1:
        hval += asso_values[str[0]];
        break;
    }
  return hval + asso_values[str[len - 1]];
}

#ifdef __GNUC__
inline
#endif
struct java_keyword *
java_keyword (str, len)
     register char *str;
     register unsigned int len;
{
  static struct java_keyword wordlist[] =
    {
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"try",  TRY_TK},
      {"else",  ELSE_TK},
      {"short",  SHORT_TK},
      {"goto",  GOTO_TK},
      {"extends",  EXTENDS_TK},
      {"",}, {"",}, 
      {"int",  INT_TK},
      {"this",  THIS_TK},
      {"",}, 
      {"native",  NATIVE_TK},
      {"",}, {"",}, 
      {"interface",  INTERFACE_TK},
      {"import",  IMPORT_TK},
      {"private",  PRIVATE_TK},
      {"volatile",  VOLATILE_TK},
      {"",}, 
      {"implements",  IMPLEMENTS_TK},
      {"",}, 
      {"long",  LONG_TK},
      {"switch",  SWITCH_TK},
      {"abstract",  ABSTRACT_TK},
      {"transient",  TRANSIENT_TK},
      {"do",  DO_TK},
      {"",}, 
      {"throws",  THROWS_TK},
      {"",}, 
      {"null",  NULL_TK},
      {"super",  SUPER_TK},
      {"true",  TRUE_TK},
      {"float",  FLOAT_TK},
      {"",}, 
      {"return",  RETURN_TK},
      {"if",  IF_TK},
      {"void",  VOID_TK},
      {"protected",  PROTECTED_TK},
      {"byte",  BYTE_TK},
      {"case",  CASE_TK},
      {"break",  BREAK_TK},
      {"finally",  FINALLY_TK},
      {"false",  FALSE_TK},
      {"synchronized",  SYNCHRONIZED_TK},
      {"instanceof",  INSTANCEOF_TK},
      {"while",  WHILE_TK},
      {"package",  PACKAGE_TK},
      {"const",  CONST_TK},
      {"boolean",  BOOLEAN_TK},
      {"final",  FINAL_TK},
      {"continue",  CONTINUE_TK},
      {"catch",  CATCH_TK},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"class",  CLASS_TK},
      {"static",  STATIC_TK},
      {"double",  DOUBLE_TK},
      {"default",  DEFAULT_TK},
      {"throw",  THROW_TK},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"for",  FOR_TK},
      {"",}, 
      {"new",  NEW_TK},
      {"char",  CHAR_TK},
      {"",}, 
      {"public",  PUBLIC_TK},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register char *s = wordlist[key].name;

          if (*s == *str && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
