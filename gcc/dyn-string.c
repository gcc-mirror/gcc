/* An abstract string datatype.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.
   Contributed by Mark Mitchell (mark@markmitchell.com).

This file is part of GNU CC.
   
GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "dyn-string.h"

/* Create a new dynamic string capable of holding at least SPACE
   characters, including the terminating NUL.  If SPACE is 0, it
   will be silently increased to 1.  */

dyn_string_t 
dyn_string_new (space)
     int space;
{
  dyn_string_t result = (dyn_string_t) xmalloc (sizeof (struct dyn_string));
 
  if (space == 0)
    /* We need at least one byte in which to store the terminating
       NUL.  */
    space = 1;

  result->allocated = space;
  result->s = (char*) xmalloc (space);
  result->length = 0;
  result->s[0] = '\0';

  return result;
}

/* Free the memory used by DS.  */

void 
dyn_string_delete (ds)
     dyn_string_t ds;
{
  free (ds->s);
  free (ds);
}

/* Append the NUL-terminated string S to DS, resizing DS if
   necessary.  */

dyn_string_t 
dyn_string_append (ds, s)
     dyn_string_t ds;
     const char *s;
{
  int len = strlen (s);
  dyn_string_resize (ds, ds->length + len + 1 /* '\0' */);
  strcpy (ds->s + ds->length, s);
  ds->length += len;

  return ds;
}

/* Increase the capacity of DS so that it can hold at least SPACE
   characters, including the terminating NUL.  This function will not
   (at present) reduce the capacity of DS.  */

dyn_string_t 
dyn_string_resize (ds, space)
     dyn_string_t ds;
     int space;
{
  int new_allocated = ds->allocated;

  while (space > new_allocated)
    new_allocated *= 2;
    
  if (new_allocated != ds->allocated)
    {
      /* We actually need more space.  */
      ds->allocated = new_allocated;
      ds->s = (char*) xrealloc (ds->s, ds->allocated);
    }

  return ds;
}
