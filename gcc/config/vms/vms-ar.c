/* VMS archive wrapper.
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by AdaCore.

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
<http://www.gnu.org/licenses/>.  */

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "libiberty.h"

#define FATAL_EXIT_CODE (44 | 0x10000000)

/* Librarian arguments.  */
static int lib_arg_max = 0;
static const char **lib_args;
static int lib_arg_index = -1;

/* Set for r/c/x/v command.  */
static int replace_mode = 0;
static int create_mode = 0;
static int extract_mode = 0;
static int verbose_mode = 0;

static char modecmd[32];
static char libname[256];

#define TEMP_FILE "arXXXXXX"
#define TEMP_FILE_LEN (sizeof(TEMP_FILE) - 1)
#define SUFFIX ".com"
#define SUFFIX_LEN (sizeof(SUFFIX) - 1)

static char *to_host_file_spec (char *filespec);
static int is_regular_file (char *name);

#ifdef VMS
static char new_host_filespec [255];
static char filename_buff [256];

static int
translate_unix (char *name, int type)
{
  strcpy (filename_buff, name);
  return 0;
}
#endif

static char *
to_host_file_spec (char *filespec)
{
#ifdef VMS
  if (strchr (filespec, ']') || strchr (filespec, ':'))
    return filespec;
  else
    {
      strcpy (filename_buff, filespec);
      decc$to_vms (filespec, translate_unix, 1, 1);
      strcpy (new_host_filespec, filename_buff);
      return new_host_filespec;
    }
#else
  return filespec;
#endif
}

/* Check to see if the file named in NAME is a regular file, i.e. not a
   directory.  */

static int
is_regular_file (char *name)
{
  int ret;
  struct stat statbuf;

  ret = stat (name, &statbuf);
  return !ret && S_ISREG (statbuf.st_mode);
}

/* Add the argument contained in STR to the list of arguments to pass to the
   archiver.  */

static void
addarg (const char *str)
{
  if (++lib_arg_index >= lib_arg_max)
    {
      lib_arg_max += 1000;
      lib_args = XRESIZEVEC (const char *, lib_args, lib_arg_max);
    }

  lib_args[lib_arg_index] = str;
}

static void
usage (void)
{
  printf ("usage: ar -r [-cv] archive file...\n");
  printf ("       ar -c [-rv] archive file...\n");
  printf ("       ar -x [-v] archive [module...]\n");
}

int
main (int argc, char *argv[])
{
  int i, nexti, iarg;
  FILE *comfile;
  int comfd;
  int outlen, maxoutlen = 4000;
  char temp_filename[] = TEMP_FILE SUFFIX;
  char command[256];
  int status;

  if (argc < 2)
    {
      fprintf (stderr, "ar: no command or archive\n");
      exit (FATAL_EXIT_CODE);
    }

  if (argv[1][0] != '-')
    {
      int arglen = strlen (argv[1]);

      /* Compatibility mode.  */
      for (i = 0; i < arglen; i++)
	{
	  if (argv[1][i] == 'r')
	    {
	      replace_mode = 1;
	    }
	  else if (argv[1][i] == 'c')
	    {
	      create_mode = 1;
	    }
	  else if (argv[1][i] == 'x')
	    {
	      extract_mode = 1;
	    }
	  else if (argv[1][i] == 'v')
	    {
	      verbose_mode = 1;
	    }
          else
            {
              fprintf (stderr, "ar: unknown command '%c'\n", argv[1][i]);
              exit (FATAL_EXIT_CODE);
            }
	}
      nexti = 2;
    }
  else
    {
      /* Option mode.  */
      nexti = 1;
      for (i = 1; i < argc; i++)
	{
	  if (argv[i][0] != '-')
	    {
	      nexti = i;
	      break;
	    }
	  else if (strcmp (argv[i], "-r") == 0)
	    {
	      replace_mode = 1;
	    }
	  else if (strcmp (argv[i], "-c") == 0)
	    {
	      create_mode = 1;
	    }
	  else if (strcmp (argv[i], "-x") == 0)
	    {
	      extract_mode = 1;
	    }
	  else if (strcmp (argv[i], "-v") == 0)
	    {
	      verbose_mode = 1;
	    }
	  else if (strcmp (argv[i], "--help") == 0)
	    {
              usage ();
              exit (EXIT_SUCCESS);
	    }
          else
            {
              fprintf (stderr, "ar: unknown option %s\n", argv[i]);
              exit (FATAL_EXIT_CODE);
            }
	}
    }

  if (extract_mode)
    {
      do
        {
          char *lname = argv[nexti];
          int lnamelen;

          /* Next argument is the archive name.  */
          if (is_regular_file (lname))
            {
              addarg (xstrdup (to_host_file_spec (lname)));
              break;
            }

          /* If not found, try with .olb instead of .a.  */
          lnamelen = strlen (lname);

	  if (lnamelen > 2
              && strcmp (&lname [lnamelen - 2], ".a") == 0)
	    {
	      char *nlibname;

	      nlibname = (char *)alloca (lnamelen + 3);
	      strcpy (nlibname, lname);
	      strcpy (&nlibname [lnamelen - 2], ".olb");
	      if (is_regular_file (nlibname))
                {
                  addarg (xstrdup (to_host_file_spec (nlibname)));
                  break;
                }
            }

          fprintf (stderr, "ar: file '%s' doesn't exist\n", lname);
          exit (FATAL_EXIT_CODE);
	} while (0);
    }
  else
    strcpy (libname, to_host_file_spec (argv[nexti]));

  nexti++;

  /* Build command mode.  */
  if (replace_mode)
    {
      strcat (modecmd, "/replace");

      if (!is_regular_file (libname) || !replace_mode)
        {
          /* Really create if the archive doesn't exist.  */
          strcat (modecmd, "/create");
        }
    }
  else if (extract_mode)
    {
      if (nexti == argc)
        {
          /* Extract all.  */
          strcat (modecmd, "/extract=(*");
        }
      else
        strcat (modecmd, "/extract=(");
    }

  /* Add files.  */
  for (i = nexti; i < argc; i++)
    {
      if (extract_mode)
        {
          /* Convert to module name (remove extension) and quote it.  */
          char *module = argv[i];
          int module_len = strlen (module);
          char *newarg = (char *)xmalloc (module_len + 3);
          int l;

          newarg[0] = '"';
          memcpy (newarg + 1, module, module_len);

          l = 1 + module_len;
          if (module_len > 4
              && strcmp (&module[module_len - 4], ".obj") == 0)
            l -= 4;

          newarg[l] = '"';
          newarg[l + 1] = 0;

          addarg (newarg);
        }
      else
        {
          /* Add the filename.  */
          addarg (xstrdup (to_host_file_spec (argv[i])));
        }
    }

  if (extract_mode)
    addarg (")");

  /* Create the command file name.  */
  strcpy (temp_filename, TEMP_FILE SUFFIX);
  comfd = mkstemps (temp_filename, SUFFIX_LEN);
  comfile = fdopen (comfd, "w");

  /* Write the command file.
     We need to split to command into severals ones if it is too long.  */
  outlen = 0;
  for (iarg = 0; iarg <= lib_arg_index; iarg++)
    {
      if (outlen == 0)
        {
          fprintf (comfile, "$ library %s %s -\n", modecmd, libname);
          if (create_mode && iarg == 0)
            strcpy (modecmd, "/replace");
        }

      fprintf (comfile, "%s", lib_args [iarg]);
      outlen += strlen (lib_args [iarg]) + 2;

      if (outlen > maxoutlen || iarg == lib_arg_index)
        {
          /* Will write a new command.  */
          fprintf (comfile, "\n");
          outlen = 0;
        }
      else
        {
          /* Continuation line.  */
          fprintf (comfile, ",-\n");
        }
    }

  fclose (comfile);

  sprintf (command, "@%s", temp_filename);

  status = system (command);

  remove (temp_filename);

  exit (status);
}
