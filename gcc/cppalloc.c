/* Part of CPP library.  (memory allocation - xmalloc etc)
   Copyright (C) 1986, 87, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by by Paul Rubin, June 1986
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
#include <stdio.h>
#include "cpplib.h"

static void
memory_full ()
{
  fprintf (stderr, "%s: Memory exhausted.\n", progname);
  exit (FATAL_EXIT_CODE);
}

char *
xmalloc (size)
     unsigned size;
{
  register char *ptr = (char *) malloc (size);
  if (ptr == 0)
    memory_full ();
  return ptr;
}

char *
xrealloc (old, size)
     char *old;
     unsigned size;
{
  register char *ptr = (char *) realloc (old, size);
  if (ptr == 0)
    memory_full ();
  return ptr;
}
