/* Shared functions related to mangling names for the GNU compiler
   for the Java(TM) language.
   Copyright (C) 2001-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. 

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Alexandre Petit-Bianco <apbianco@cygnus.com> */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "jcf.h"
#include "tree.h"
#include "java-tree.h"
#include "obstack.h"
#include "diagnostic-core.h"

static void append_unicode_mangled_name (const char *, int);
#ifndef HAVE_AS_UTF8
static int  unicode_mangling_length (const char *, int);
#endif

extern struct obstack *mangle_obstack;

static int
utf8_cmp (const unsigned char *str, int length, const char *name)
{
  const unsigned char *limit = str + length;
  int i;

  for (i = 0; name[i]; ++i)
    {
      int ch = UTF8_GET (str, limit);
      if (ch != name[i])
	return ch - name[i];
    }

  return str == limit ? 0 : 1;
}

/* A sorted list of all C++ keywords.  If you change this, be sure
   also to change the list in
   libjava/classpath/tools/gnu/classpath/tools/javah/Keywords.java.  */
static const char *const cxx_keywords[] =
{
  "_Complex",
  "__alignof",
  "__alignof__",
  "__asm",
  "__asm__",
  "__attribute",
  "__attribute__",
  "__builtin_va_arg",
  "__complex",
  "__complex__",
  "__const",
  "__const__",
  "__extension__",
  "__imag",
  "__imag__",
  "__inline",
  "__inline__",
  "__label__",
  "__null",
  "__real",
  "__real__",
  "__restrict",
  "__restrict__",
  "__signed",
  "__signed__",
  "__typeof",
  "__typeof__",
  "__volatile",
  "__volatile__",
  "and",
  "and_eq",
  "asm",
  "auto",
  "bitand",
  "bitor",
  "bool",
  "break",
  "case",
  "catch",
  "char",
  "class",
  "compl",
  "const",
  "const_cast",
  "continue",
  "default",
  "delete",
  "do",
  "double",
  "dynamic_cast",
  "else",
  "enum",
  "explicit",
  "export",
  "extern",
  "false",
  "float",
  "for",
  "friend",
  "goto",
  "if",
  "inline",
  "int",
  "long",
  "mutable",
  "namespace",
  "new",
  "not",
  "not_eq",
  "operator",
  "or",
  "or_eq",
  "private",
  "protected",
  "public",
  "register",
  "reinterpret_cast",
  "return",
  "short",
  "signed",
  "sizeof",
  "static",
  "static_cast",
  "struct",
  "switch",
  "template",
  "this",      
  "throw",
  "true",
  "try",
  "typedef",
  "typeid",
  "typename",
  "typeof",
  "union",
  "unsigned",
  "using",
  "virtual",
  "void",
  "volatile",
  "wchar_t",
  "while",
  "xor",
  "xor_eq"
};

/* Return true if NAME is a C++ keyword.  */
int
cxx_keyword_p (const char *name, int length)
{
  int last = ARRAY_SIZE (cxx_keywords);
  int first = 0;
  int mid = (last + first) / 2;
  int old = -1;

  for (mid = (last + first) / 2;
       mid != old;
       old = mid, mid = (last + first) / 2)
    {
      int kwl = strlen (cxx_keywords[mid]);
      int min_length = kwl > length ? length : kwl;
      int r = utf8_cmp ((const unsigned char *) name, min_length, cxx_keywords[mid]);

      if (r == 0)
	{
	  int i;
	  /* We've found a match if all the remaining characters are `$'.  */
	  for (i = min_length; i < length && name[i] == '$'; ++i)
	    ;
	  if (i == length)
	    return 1;
	  r = 1;
	}

      if (r < 0)
	last = mid;
      else
	first = mid;
    }
  return 0;
}

/* If NAME happens to be a C++ keyword, add `$'.  */
#define MANGLE_CXX_KEYWORDS(NAME, LEN)			\
do							\
  {							\
    if (cxx_keyword_p ((NAME), (LEN)))			\
      {							\
	char *tmp_buf = (char *)alloca ((LEN)+1);	\
	memcpy (tmp_buf, (NAME), (LEN));		\
	tmp_buf[LEN]= '$';				\
	(NAME) = tmp_buf;				\
	(LEN)++;					\
      }							\
  }							\
while (0)


/* If the assembler doesn't support UTF8 in symbol names, some
   characters might need to be escaped.  */

#ifndef HAVE_AS_UTF8

/* Assuming (NAME, LEN) is a Utf8-encoding string, emit the string
   appropriately mangled (with Unicode escapes if needed) to
   MANGLE_OBSTACK.  Note that `java', `lang' and `Object' are used so
   frequently that they could be cached.  */

void
append_gpp_mangled_name (const char *name, int len)
{
  int encoded_len, needs_escapes;
  char buf[6];

  MANGLE_CXX_KEYWORDS (name, len);

  encoded_len = unicode_mangling_length (name, len);
  needs_escapes = encoded_len > 0;

  sprintf (buf, "%d", (needs_escapes ? encoded_len : len));
  obstack_grow (mangle_obstack, buf, strlen (buf));

  if (needs_escapes)
    append_unicode_mangled_name (name, len);
  else
    obstack_grow (mangle_obstack, name, len);
}

/* Assuming (NAME, LEN) is a Utf8-encoded string, emit the string
   appropriately mangled (with Unicode escapes) to MANGLE_OBSTACK.
   Characters needing an escape are encoded `__UNN_' to `__UNNNN_', in
   which case `__U' will be mangled `__U_'.  */

static void
append_unicode_mangled_name (const char *name, int len)
{
  const unsigned char *ptr;
  const unsigned char *limit = (const unsigned char *)name + len;
  int uuU = 0;
  for (ptr = (const unsigned char *) name;  ptr < limit;  )
    {
      int ch = UTF8_GET(ptr, limit);

      if ((ISALNUM (ch) && ch != 'U') || ch == '$')
        {
	  obstack_1grow (mangle_obstack, ch);
          uuU = 0;
        }
      /* Everything else needs encoding */
      else
	{
	  char buf [9];
	  if (ch == '_' || ch == 'U')
	    {
	      /* Prepare to recognize __U */
	      if (ch == '_' && (uuU < 3))
		{
		  uuU++;
		  obstack_1grow (mangle_obstack, ch);
		}
	      /* We recognize __U that we wish to encode
                 __U_. Finish the encoding. */
	      else if (ch == 'U' && (uuU == 2))
		{
		  uuU = 0;
		  obstack_grow (mangle_obstack, "U_", 2);
		}
	      /* Otherwise, just reset uuU and emit the character we
                 have. */
	      else
		{
		  uuU = 0;
		  obstack_1grow (mangle_obstack, ch);
		}
	      continue;
	    }
	  sprintf (buf, "__U%x_", ch);
	  obstack_grow (mangle_obstack, buf, strlen (buf));
	  uuU = 0;
	}
    }
}

/* Assuming (NAME, LEN) is a Utf8-encoding string, calculate the
   length of the string as mangled (a la g++) including Unicode
   escapes.  If no escapes are needed, return 0.  */

static int
unicode_mangling_length (const char *name, int len)
{
  const unsigned char *ptr;
  const unsigned char *limit = (const unsigned char *)name + len;
  int need_escapes = 0;		/* Whether we need an escape or not */
  int num_chars = 0;		/* Number of characters in the mangled name */
  int uuU = 0;			/* Help us to find __U. 0: '_', 1: '__' */
  for (ptr = (const unsigned char *) name;  ptr < limit;  )
    {
      int ch = UTF8_GET(ptr, limit);

      if (ch < 0)
	error ("internal error - invalid Utf8 name");
      if ((ISALNUM (ch) && ch != 'U') || ch == '$')
	{
	  num_chars++;
	  uuU = 0;
	}
      /* Everything else needs encoding */
      else
	{
	  int encoding_length = 2;

	  if (ch == '_' || ch == 'U')
	    {
	      /* It's always at least one character. */
	      num_chars++;

	      /* Prepare to recognize __U */
	      if (ch == '_' && (uuU < 3))
		uuU++;

	      /* We recognize __U that we wish to encode __U_, we
	         count one more character. */
	      else if (ch == 'U' && (uuU == 2))
		{
		  num_chars++;
		  need_escapes = 1;
		  uuU = 0;
		}
	      /* Otherwise, just reset uuU */
	      else
		uuU = 0;

	      continue;
	    }
	  
	  if (ch > 0xff)
	    encoding_length++;
	  if (ch > 0xfff)
	    encoding_length++;
	  
	  num_chars += (4 + encoding_length);
	  need_escapes = 1;
	  uuU = 0;
	}
    }
  if (need_escapes)
    return num_chars;
  else
    return 0;
}

#else

/* The assembler supports UTF8, we don't use escapes. Mangling is
   simply <N>NAME. <N> is the number of UTF8 encoded characters that
   are found in NAME. Note that `java', `lang' and `Object' are used
   so frequently that they could be cached.  */

void
append_gpp_mangled_name (const char *name, int len)
{
  const unsigned char *ptr;
  const unsigned char *limit;
  int encoded_len;
  char buf [6];
  
  MANGLE_CXX_KEYWORDS (name, len);

  limit = (const unsigned char *)name + len;

  /* Compute the length of the string we wish to mangle. */
  for (encoded_len =  0, ptr = (const unsigned char *) name;
       ptr < limit; encoded_len++)
    {
      int ch = UTF8_GET(ptr, limit);

      if (ch < 0)
	error ("internal error - invalid Utf8 name");
    }

  sprintf (buf, "%d", encoded_len);
  obstack_grow (mangle_obstack, buf, strlen (buf));
  obstack_grow (mangle_obstack, name, len);
}

#endif /* HAVE_AS_UTF8 */
