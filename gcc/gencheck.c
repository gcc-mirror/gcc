/* Generate check macros for tree codes.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

#include "hconfig.h"
#include "system.h"

#define DEFTREECODE(SYM, NAME, TYPE, LEN)   STRINGIFY(SYM),

const char *tree_codes[] = {
#include "tree.def"
#include "gencheck.h"
(char*)0
};

static void usage PARAMS ((void));

static void
usage ()
{
  fprintf (stderr,"Usage: gencheck\n");
}

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv ATTRIBUTE_UNUSED;
{
  int i;

  switch (argc)
    {
    case 1:
      break;

    default:
      usage ();
      return (1);
    }

  printf ("/* This file is generated using gencheck. Do not edit. */\n");
  for (i = 0; tree_codes[i]; i++)
    {
      printf ("#define %s_CHECK(t)\tTREE_CHECK (t, %s)\n",
	      tree_codes[i], tree_codes[i]);
    }

  return 0;
}

#if defined(USE_C_ALLOCA)
/* FIXME: We only need an xmalloc definition because we are forced to
   link with alloca.o on some platforms.  This should go away if/when
   we link against libiberty.a. (ghazi@caip.rutgers.edu 6/3/98) */
PTR
xmalloc (nbytes)
  size_t nbytes;
{
  register PTR tmp = (PTR) malloc (nbytes);

  if (!tmp)
    {
      fprintf (stderr, "can't allocate %d bytes (out of virtual memory)\n",
	       nbytes);
      exit (FATAL_EXIT_CODE);
    }

  return tmp;
}
#endif /* USE_C_ALLOCA */
