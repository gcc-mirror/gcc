/* Call Windows NT 3.x linker.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Douglas B. Rupp (drupp@cs.washington.edu).

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>

/* These can be set by command line arguments */
char *linker_path = 0;
int verbose = 0;

int link_arg_max = -1;
char **link_args = (char **) 0;
int link_arg_index = -1;

char *search_dirs = ".";

static int is_regular_file (char *name);

static void
addarg (str)
     char *str;
{
  int i;

  if (++link_arg_index >= link_arg_max)
    {
      char **new_link_args
	= (char **) calloc (link_arg_max + 1000, sizeof (char *));

      for (i = 0; i <= link_arg_max; i++)
	new_link_args [i] = link_args [i];

      if (link_args)
	free (link_args);

      link_arg_max += 1000;
      link_args = new_link_args;
    }

  link_args [link_arg_index] = str;
}

static char *
locate_file (file_name, path_val)
     char *file_name;
     char *path_val;
{
  char buf [1000];
  int file_len = strlen (file_name);
  char *end_path = path_val + strlen (path_val);
  char *ptr;

  /* Handle absolute pathnames */
  if (file_name [0] == '/' || file_name [0] == DIR_SEPARATOR
      || isalpha (file_name [0]) && file_name [1] == ':')
    {
      strncpy (buf, file_name, sizeof buf);
      buf[sizeof buf - 1] = '\0';
      if (is_regular_file (buf))
	return strdup (buf);
      else
	return 0;
  }

  if (! path_val)
    return 0;

  for (;;)
    {
      for (; *path_val == PATH_SEPARATOR ; path_val++)
	;
      if (! *path_val)
	return 0;

      for (ptr = buf; *path_val && *path_val != PATH_SEPARATOR; )
	*ptr++ = *path_val++;

      ptr--;
      if (*ptr != '/' && *ptr != DIR_SEPARATOR)
	*++ptr = DIR_SEPARATOR;

      strcpy (++ptr, file_name);

      if (is_regular_file (buf))
	return strdup (buf);
    }

  return 0;
}

static char *
expand_lib (name)
     char *name;
{
  char *lib, *lib_path;

  lib = malloc (strlen (name) + 6);
  strcpy (lib, "lib");
  strcat (lib, name);
  strcat (lib, ".a");
  lib_path = locate_file (lib, search_dirs);
  if (!lib_path)
    {
      fprintf (stderr, "Couldn't locate library: %s\n", lib);
      exit (1);
    }

  return lib_path;
}

static int
is_regular_file (name)
     char *name;
{
  int ret;
  struct stat statbuf;

  ret = stat(name, &statbuf);
  return !ret && S_ISREG (statbuf.st_mode);
}

static void
process_args (p_argc, argv)
     int *p_argc;
     char *argv[];
{
  int i, j;

  for (i = 1; i < *p_argc; i++)
    {
      /* -v turns on verbose option here and is passed on to gcc */
      if (! strcmp (argv [i], "-v"))
	verbose = 1;
    }
}

main (argc, argv)
     int argc;
     char *argv[];
{
  int i;
  int done_an_ali = 0;
  int file_name_index;
  char *pathval = getenv ("PATH");
  char *spawn_args [5];
  char *tmppathval = malloc (strlen (pathval) + 3);

  strcpy (tmppathval, ".;");
  pathval = strcat (tmppathval, pathval);

  process_args (&argc , argv);

  linker_path = locate_file ("link32.exe", pathval);
  if (!linker_path)
    {
      linker_path = locate_file ("link.exe", pathval);
      if (!linker_path)
	{
	  fprintf (stderr, "Couldn't locate link32 or link\n");
	  exit (1);
	}
    }

  addarg (linker_path);

  for (i = 1; i < argc; i++)
    {
      int arg_len = strlen (argv [i]);

      if (!strcmp (argv [i], "-o"))
	{
	  char *buff, *ptr;
	  int out_len;

	  i++;
	  out_len = strlen (argv[i]) + 10;
	  buff = malloc (out_len);
	  strcpy (buff, "-out:");
	  strcat (buff, argv[i]);
	  ptr = strstr (buff, ".exe");
	  if (ptr == NULL || strlen (ptr) != 4)
	    strcat (buff, ".exe");
	  addarg (buff);
	  addarg ("-debug:full -debugtype:coff");
	}
      else if (arg_len > 2 && !strncmp (argv [i], "-L", 2))
	{
	  char *nbuff, *sdbuff;
	  int j, new_len, search_dirs_len;

	  new_len = strlen (&argv[i][2]);
	  search_dirs_len = strlen (search_dirs);

	  nbuff = malloc (new_len + 1);
	  strcpy (nbuff, &argv[i][2]);

	  for (j = 0; j < new_len; j++)
	    if (nbuff[j] == '/') nbuff[j] = DIR_SEPARATOR;

	  sdbuff = malloc (search_dirs_len + new_len + 2);
	  strcpy (sdbuff, search_dirs);
	  sdbuff[search_dirs_len] = PATH_SEPARATOR;
	  sdbuff[search_dirs_len+1] = 0;
	  strcat (sdbuff, nbuff);

	  if (search_dirs)
	    free (search_dirs);

	  search_dirs = sdbuff;
	}

      else if (arg_len > 2 && !strncmp (argv [i], "-l", 2))
	addarg (expand_lib (&argv[i][2]));
    else if (!strcmp (argv [i], "-v")) 
      ;
    else
      addarg (argv [i]);
    }

  addarg (NULL);

  if (verbose)
    {
      int i;

      for (i = 0; i < link_arg_index; i++)
	printf ("%s ", link_args [i]);
      putchar ('\n');
    }

  if (spawnvp (P_WAIT, linker_path, (const char * const *)link_args) != 0)
    {
      fprintf (stderr, "Error executing %s\n", link_args[0]);
      exit (1);
    }

  exit (0);
}
