/* Part of CPP library.  (memory allocation - xmalloc etc)
   Copyright (C) 1986, 87, 89, 92, 93, 94, 1995, 1998 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "cpplib.h"

static void memory_full PROTO ((void)) ATTRIBUTE_NORETURN;

static void
memory_full ()
{
  cpp_notice ("%s: Memory exhausted.\n", progname);
  exit (FATAL_EXIT_CODE);
}

PTR
xmalloc (size)
  size_t size;
{
  register PTR ptr = (PTR) malloc (size);
  if (ptr == 0)
    memory_full ();
  return ptr;
}

PTR
xcalloc (number, size)
  size_t number, size;
{
  register PTR ptr = (PTR) calloc (number, size);
  if (ptr == 0)
    memory_full ();
  return ptr;
}

PTR
xrealloc (old, size)
  PTR old;
  size_t size;
{
  register PTR ptr;
  if (old)
    ptr = (PTR) realloc (old, size);
  else
    ptr = (PTR) malloc (size);
  if (ptr == 0)
    memory_full ();
  return ptr;
}

char *
xstrdup (input)
  const char *input;
{
  unsigned size = strlen (input);
  char *output = xmalloc (size + 1);
  strcpy (output, input);
  return output;
}
