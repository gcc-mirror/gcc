/* Utility to generate opcode name list from bytecode definition file.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "hconfig.h"
#include "bi-defs.h"

int
main()
{
  struct def *d;
  struct variation *v;

  yyparse();
  reverse();

  for (d = defs; d; d = d->next)
    for (v = d->variations; v; v = v->next)
      printf("\"%s%s\",\n", d->basename, v->name);

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}

/* Safely allocate NBYTES bytes of memory. Returns pointer to block of
   memory.  */

char *
xmalloc (nbytes)
     int nbytes;
{
  char *tmp = (char *) malloc (nbytes);
  
  if (!tmp)
    {
      fprintf (stderr, "can't allocate %d bytes (out of virtual memory)\n", nbytes);
      exit (FATAL_EXIT_CODE);
    }

  return tmp;
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fprintf (stderr, "Internal gcc abort.\n");
  exit (FATAL_EXIT_CODE);
}
