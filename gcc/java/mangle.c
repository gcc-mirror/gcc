/* Functions related to mangling class names for the GNU compiler
   for the Java(TM) language.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "jcf.h"
#include "tree.h"
#include "java-tree.h"
#include "obstack.h"
#include "toplev.h"

/* Assuming (NAME, LEN) is a Utf8-encoding string, calculate
   the length of the string as mangled (a la g++) including Unicode escapes.
   If no escapes are needed, return 0. */

int
unicode_mangling_length (name, len)
     const char *name; 
     int len; 
{
  const unsigned char *ptr;
  const unsigned char *limit = (const unsigned char *)name + len;
  int need_escapes = 0;
  int num_chars = 0;
  int underscores = 0;
  for (ptr = (const unsigned char *) name;  ptr < limit;  )
    {
      int ch = UTF8_GET(ptr, limit);
      if (ch < 0)
	error ("internal error - invalid Utf8 name");
      if (ch >= '0' && ch <= '9')
	need_escapes += num_chars == 0;
      else if (ch == '_')
	underscores++;
      else if (ch != '$' && (ch < 'a' || ch > 'z') && (ch < 'A' || ch > 'Z'))
	need_escapes++;
      num_chars++;
    }
  if (need_escapes)
    return num_chars + 4 * (need_escapes + underscores);
  else
    return 0;
}

/* Assuming (NAME, LEN) is a Utf8-encoding string, emit the string
   appropriately mangled (with Unicode escapes) to OBSTACK. */

void
emit_unicode_mangled_name (obstack, name, len)
     struct obstack *obstack;
     const char *name;
     int len;
{
  const unsigned char *ptr;
  const unsigned char *limit = (const unsigned char *)name + len;
  for (ptr = (const unsigned char *) name;  ptr < limit;  )
    {
      int ch = UTF8_GET(ptr, limit);
      int emit_escape;
      if (ch < 0)
	{
	  error ("internal error - bad Utf8 string");
	  break;
	}
      if (ch >= '0' && ch <= '9')
	emit_escape = (ptr == (const unsigned char *) name);
      else
	emit_escape = (ch < 'a' || ch > 'z') && (ch < 'A' || ch > 'Z');
      if (emit_escape)
	{
	  char buf[6];
	  sprintf (buf, "_%04x", ch);
	  obstack_grow (obstack, buf, 5);
	}
      else
	{
	  obstack_1grow (obstack, ch);
	}
    }
}

/* Assuming (NAME, LEN) is a Utf8-encoding string, emit the string
   appropriately mangled (with Unicode escapes if needed) to OBSTACK. */

void
append_gpp_mangled_name (obstack, name, len)
     struct obstack *obstack;
     const char *name;
     int len;
{
  int encoded_len = unicode_mangling_length (name, len);
  int needs_escapes = encoded_len > 0;
  char buf[6];
  if (needs_escapes)
    {
      sprintf (buf, "U%d", encoded_len);
      obstack_grow (obstack, buf, strlen(buf));
      emit_unicode_mangled_name (obstack, name, len);
    }
  else
    {
      sprintf (buf, "%d", len);
      obstack_grow (obstack, buf, strlen(buf));
      obstack_grow (obstack, name, len);
    }
}

/* Append the mangled name of a class named CLASSNAME onto OBSTACK. */

void
append_gpp_mangled_classtype (obstack, class_name)
     struct obstack *obstack;
     const char *class_name;
{
  const char *ptr;
  int qualifications = 0;

  for (ptr = class_name; *ptr != '\0'; ptr++)
    {
      if (*ptr == '.')
	qualifications++;
    }
  if (qualifications)
    {
      char buf[8];
      if (qualifications >= 9)
	sprintf (buf, "Q_%d_", qualifications + 1);
      else
	sprintf (buf, "Q%d", qualifications + 1);
      obstack_grow (obstack, buf, strlen (buf));
    }
  for (ptr = class_name; ; ptr++)
    {
      if (ptr[0] == '.' || ptr[0] == '\0')
	{
	  append_gpp_mangled_name (obstack, class_name, ptr - class_name);
	  if (ptr[0] == '\0')
	    break;
	  class_name = ptr + 1;
	}
    }
}
