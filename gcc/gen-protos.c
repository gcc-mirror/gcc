/* gen-protos.c - massages a list of prototypes, for use by fixproto.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <ctype.h>
#include "hconfig.h"
#include "scan.h"

#define HASH_SIZE 2503 /* a prime */

int hash_tab[HASH_SIZE];
int verbose = 0;

sstring linebuf;

/* Avoid error if config defines abort as fancy_abort.
   It's not worth "really" implementing this because ordinary
   compiler users never run fix-header.  */

void
fancy_abort ()
{
  abort ();
}

int
main (argc, argv)
     int argc;
     char** argv;
{
  FILE *inf = stdin;
  FILE *outf = stdout;
  int next_index = 0;
  int i, i0;

  fprintf (outf, "struct fn_decl std_protos[] = {\n");

  for (;;)
    {
      int c = skip_spaces (inf, ' ');
      int param_nesting = 1;
      char *param_start, *param_end, *decl_start,
      *name_start, *name_end;
      register char *ptr;
      if (c == EOF)
	break;
      linebuf.ptr = linebuf.base;
      ungetc (c, inf);
      c = read_upto (inf, &linebuf, '\n');
      if (linebuf.base[0] == '#') /* skip cpp command */
	continue;
      if (linebuf.base[0] == '\0') /* skip empty line */
	continue;

      ptr = linebuf.ptr - 1;
      while (*ptr == ' ' || *ptr == '\t') ptr--;
      if (*ptr-- != ';')
	{
	  fprintf (stderr, "Funny input line: %s\n", linebuf.base);
	  continue;
	}
      while (*ptr == ' ' || *ptr == '\t') ptr--;
      if (*ptr != ')')
	{
	  fprintf (stderr, "Funny input line: %s\n", linebuf.base);
	  continue;
	}
      param_end = ptr;
      for (;;)
	{
	  int c = *--ptr;
	  if (c == '(' && --param_nesting == 0)
	    break;
	  else if (c == ')')
	    param_nesting++;
	}
      param_start = ptr+1;

      ptr--;
      while (*ptr == ' ' || *ptr == '\t') ptr--;

      if (!isalnum (*ptr))
	{
	  if (verbose)
	    fprintf (stderr, "%s: Can't handle this complex prototype: %s\n",
		     argv[0], linebuf.base);
	  continue;
	}
      name_end = ptr+1;

      while (isalnum (*ptr) || *ptr == '_') --ptr;
      name_start = ptr+1;
      while (*ptr == ' ' || *ptr == '\t') ptr--;
      ptr[1] = 0;
      *name_end = 0;
      *param_end = 0;
      *name_end = 0;

      decl_start = linebuf.base;
      if (strncmp (decl_start, "typedef ", 8) == 0)
	continue;
      if (strncmp (decl_start, "extern ", 7) == 0)
	decl_start += 7;


      /* NOTE:  If you edit this,
	 also edit lookup_std_proto in fix-header.c !! */
      i = hash (name_start) % HASH_SIZE;
      i0 = i;
      if (hash_tab[i] != 0)
	{
	  for (;;)
	    {
	      i = (i+1) % HASH_SIZE;
	      if (i == i0)
		abort ();
	      if (hash_tab[i] == 0)
		break;
	    }
	}
      hash_tab[i] = next_index;

      fprintf (outf, "  {\"%s\", \"%s\", \"%s\" },\n",
	       name_start, decl_start, param_start);

      next_index++;

      if (c == EOF)
	break;
    }
  fprintf (outf, "{0, 0, 0}\n};\n");


  fprintf (outf, "#define HASH_SIZE %d\n", HASH_SIZE);
  fprintf (outf, "short hash_tab[HASH_SIZE] = {\n");
  for (i = 0; i < HASH_SIZE; i++)
    fprintf (outf, "  %d,\n", hash_tab[i]);
  fprintf (outf, "};\n");

  return 0;
}
