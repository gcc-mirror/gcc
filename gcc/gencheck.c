/* Generate check macros for tree codes.
   Copyright (C) 1998, 1999, 2000, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "hconfig.h"
#include "system.h"

#define DEFTREECODE(SYM, NAME, TYPE, LEN)   STRINGX(SYM),

static const char *const tree_codes[] = {
#include "tree.def"
#include "c-common.def"
#include "gencheck.h"
(char*) 0
};

static void usage PARAMS ((void));

static void
usage ()
{
  fputs ("Usage: gencheck\n", stderr);
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

  puts ("/* This file is generated using gencheck. Do not edit. */\n");
  puts ("#ifndef GCC_TREE_CHECK_H");
  puts ("#define GCC_TREE_CHECK_H\n");

  for (i = 0; tree_codes[i]; i++)
    {
      printf ("#define %s_CHECK(t)\tTREE_CHECK (t, %s)\n",
	      tree_codes[i], tree_codes[i]);
    }

  puts ("\n#endif /* GCC_TREE_CHECK_H */");
  return 0;
}
