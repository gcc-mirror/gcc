/* pcp.h -- Describes the format of a precompiled file
   Copyright (C) 1990 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */



/* Structure allocated for every string in a precompiled file */
typedef struct stringdef STRINGDEF;
struct stringdef
{
  U_CHAR *contents;		/* String to include */
  int len;			/* Its length */
  int writeflag;		/* Whether we write this */
  int lineno;			/* Linenumber of source file */
  U_CHAR *filename;		/* Name of source file */
  STRINGDEF *chain;		/* Global list of strings in natural order */
  int output_mark;		/* Where in the output this goes */
};

typedef struct keydef KEYDEF;
struct keydef
{
  STRINGDEF *str;
  KEYDEF *chain;
};

/* Format: */
/* A precompiled file starts with a series of #define and #undef
 statements:
    #define MAC DEF     ---   Indicates MAC must be defined with defn DEF
    #define MAC         ---   Indicates MAC must be defined with any defn
    #undef MAC          ---   Indicates MAC cannot be defined

These preconditions must be true for a precompiled file to be used.  
The preconditions section is null terminated. */

/* Then, there is a four byte number (in network byte order) which */
 /* indicates the number of strings the file contains. */

/* Each string contains a STRINGDEF structure.  The only component of */
 /* the STRINGDEF structure which is used is the lineno field, which */
 /* should hold the line number in the original header file.  */
 /* Then follows the string, followed by a null.  Then comes a four */
 /* byte number (again, in network byte order) indicating the number */
 /* of keys for this string.  Each key is a KEYDEF structure, with */
 /* irrelevant contents, followed by the null-terminated string. */

/* If the number of keys is 0, then there are no keys for the string, */
 /* in other words, the string will never be included.  If the number */
 /* of keys is -1, this is a special flag indicating there are no keys */
 /* in the file, and the string is mandatory (that is, it must be */
 /* included regardless in the included output).  */

/* A file, then, looks like this:

  Precondition 1
  Precondition 2
  . 
  .
  .
  <NUL>
  Number of strings
    STRINGDEF
    String . . . <NUL>
    Number of keys
      KEYDEF
      Key . . . <NUL>
      KEYDEF 
      Key . . . <NUL>
      .
      .
      .
    STRINGDEF
    String . . . <NUL>
    Number of keys
      KEYDEF
      Key . . . <NUL>
      .
      .
      .
    .
    .
    .

*/
