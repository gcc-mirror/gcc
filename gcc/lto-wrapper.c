/* Wrapper to call lto.  Used by collect2 and the linker plugin.
   Copyright (C) 2009 Free Software Foundation, Inc.

   Factored out of collect2 by Rafael Espindola <espindola@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* This program is passed a gcc, a list of gcc arguments and a list of
   object files containing IL. It scans the argument list to check if
   we are in whopr mode or not modifies the arguments and needed and
   prints a list of output files on stdout.

   Example:

   $ lto-wrapper gcc/xgcc -B gcc a.o b.o -o test -flto

   The above will print something like
   /tmp/ccwbQ8B2.lto.o

   If -fwhopr is used instead, more than one file might be produced
   ./ccXj2DTk.lto.ltrans.o
   ./ccCJuXGv.lto.ltrans.o
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "intl.h"
#include "libiberty.h"

int debug;				/* true if -debug */

enum lto_mode_d {
  LTO_MODE_NONE,			/* Not doing LTO. */
  LTO_MODE_LTO,				/* Normal LTO. */
  LTO_MODE_WHOPR			/* WHOPR. */
};

/* Current LTO mode.  */
static enum lto_mode_d lto_mode = LTO_MODE_NONE;

/* Just die. CMSGID is the error message. */

static void __attribute__ ((format (printf, 1, 2)))
fatal (const char * cmsgid, ...)
{
  va_list ap;

  va_start (ap, cmsgid);
  fprintf (stderr, "lto-wrapper: ");
  vfprintf (stderr, _(cmsgid), ap);
  fprintf (stderr, "\n");
  va_end (ap);

  exit (FATAL_EXIT_CODE);
}


/* Die when sys call fails. CMSGID is the error message.  */

static void __attribute__ ((format (printf, 1, 2)))
fatal_perror (const char *cmsgid, ...)
{
  int e = errno;
  va_list ap;

  va_start (ap, cmsgid);
  fprintf (stderr, "lto-wrapper: ");
  vfprintf (stderr, _(cmsgid), ap);
  fprintf (stderr, ": %s\n", xstrerror (e));
  va_end (ap);

  exit (FATAL_EXIT_CODE);
}


/* Execute a program, and wait for the reply. ARGV are the arguments. The
   last one must be NULL. */

static struct pex_obj *
collect_execute (char **argv)
{
  struct pex_obj *pex;
  const char *errmsg;
  int err;

  if (debug)
    {
      char **p_argv;
      const char *str;

      for (p_argv = argv; (str = *p_argv) != (char *) 0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  pex = pex_init (0, "lto-wrapper", NULL);
  if (pex == NULL)
    fatal_perror ("pex_init failed");

  errmsg = pex_run (pex, PEX_LAST | PEX_SEARCH, argv[0], argv, NULL,
		    NULL, &err);
  if (errmsg != NULL)
    {
      if (err != 0)
	{
	  errno = err;
	  fatal_perror (errmsg);
	}
      else
	fatal (errmsg);
    }

  return pex;
}


/* Wait for a process to finish, and exit if a nonzero status is found.
   PROG is the program name. PEX is the process we should wait for. */

static int
collect_wait (const char *prog, struct pex_obj *pex)
{
  int status;

  if (!pex_get_status (pex, 1, &status))
    fatal_perror ("can't get program status");
  pex_free (pex);

  if (status)
    {
      if (WIFSIGNALED (status))
	{
	  int sig = WTERMSIG (status);
	  if (WCOREDUMP (status))
	    fatal ("%s terminated with signal %d [%s], core dumped",
		   prog, sig, strsignal (sig));
	  else
	    fatal ("%s terminated with signal %d [%s]",
		   prog, sig, strsignal (sig));
	}

      if (WIFEXITED (status))
	fatal ("%s returned %d exit status", prog, WEXITSTATUS (status));
    }

  return 0;
}


/* Unlink a temporary LTRANS file unless requested otherwise.  */

static void
maybe_unlink_file (const char *file)
{
  if (! debug)
    {
      if (unlink_if_ordinary (file))
	fatal_perror ("deleting LTRANS file %s", file);
    }
  else
    fprintf (stderr, "[Leaving LTRANS %s]\n", file);
}


/* Execute program ARGV[0] with arguments ARGV. Wait for it to finish.  */

static void
fork_execute (char **argv)
{
  struct pex_obj *pex;
  char *new_argv[3];
  char *args_name = make_temp_file (".args");
  char *at_args = concat ("@", args_name, NULL);
  FILE *args = fopen (args_name, "w");
  int status;

  if (args == NULL)
    fatal ("failed to open %s", args_name);

  status = writeargv (&argv[1], args);

  if (status)
    fatal ("could not write to temporary file %s",  args_name);

  fclose (args);

  new_argv[0] = argv[0];
  new_argv[1] = at_args;
  new_argv[2] = NULL;

  pex = collect_execute (new_argv);
  collect_wait (new_argv[0], pex);

  maybe_unlink_file (args_name);
  free (args_name);
  free (at_args);
}


/* Execute gcc. ARGC is the number of arguments. ARGV contains the arguments. */

static void
run_gcc (unsigned argc, char *argv[])
{
  unsigned i;
  unsigned new_argc = argc;
  const char **new_argv;
  const char **argv_ptr;
  char *ltrans_output_file = NULL;
  char *flto_out = NULL;
  char *list_option_full = NULL;

  new_argc += 8;
  new_argv = (const char **) xcalloc (sizeof (char *), new_argc);

  argv_ptr = new_argv;

  *argv_ptr++ = argv[0];
  *argv_ptr++ = "-combine";
  *argv_ptr++ = "-x";
  *argv_ptr++ = "lto";
  *argv_ptr++ = "-c";
  if (lto_mode == LTO_MODE_LTO)
    {
      flto_out = make_temp_file (".lto.o");
      *argv_ptr++ = "-o";
      *argv_ptr++ = flto_out;
    }
  else if (lto_mode == LTO_MODE_WHOPR)
    {
      const char *list_option = "-fltrans-output-list=";
      size_t list_option_len = strlen (list_option);
      char *tmp;

      ltrans_output_file = make_temp_file (".ltrans.out");
      list_option_full = (char *) xmalloc (sizeof (char) *
		         (strlen (ltrans_output_file) + list_option_len + 1));
      tmp = list_option_full;

      *argv_ptr++ = tmp;
      strcpy (tmp, list_option);
      tmp += list_option_len;
      strcpy (tmp, ltrans_output_file);

      *argv_ptr++ = "-fwpa";
    }
  else
    fatal ("invalid LTO mode");

  /* Add inherited GCC options to the LTO back end command line.
     Filter out some obviously inappropriate options that will
     conflict with  the options that we force above.  We pass
     all of the remaining options on to LTO, and let it complain
     about any it doesn't like. Note that we invoke LTO via the
     `gcc' driver, so the usual option processing takes place.
     Except for `-flto' and `-fwhopr', we should only filter options that
     are meaningful to `ld', lest an option go silently unclaimed.  */
  for (i = 1; i < argc; i++)
    {
      const char *s = argv[i];

      if (strcmp (s, "-flto") == 0 || strcmp (s, "-fwhopr") == 0)
	/* We've handled this LTO option, don't pass it on.  */
	;
      else if (*s == '-' && s[1] == 'o')
	{
	  /* Drop `-o' and its filename argument.  We will use a
	     temporary file for the LTO output.  The `-o' option
	     will be interpreted by the linker.  */
	  if (s[2] == '\0')
	    i++;
	}
      else
	/* Pass the option or argument to LTO.  */
	*argv_ptr++ = s;
    }

  *argv_ptr = NULL;

  fork_execute (CONST_CAST (char **, new_argv));
  free (new_argv);
  new_argv = NULL;

  if (lto_mode == LTO_MODE_LTO)
    {
      printf("%s\n", flto_out);
      free (flto_out);
      flto_out = NULL;
    }
  else if (lto_mode == LTO_MODE_WHOPR)
    {
      FILE *stream = fopen (ltrans_output_file, "r");
      int c;

      if (!stream)
	fatal_perror ("fopen: %s", ltrans_output_file);

      while ((c = getc (stream)) != EOF)
	putc (c, stdout);
      fclose (stream);
      maybe_unlink_file (ltrans_output_file);
      free (ltrans_output_file);
      free (list_option_full);
    }
  else
    fatal ("invalid LTO mode");
}


/* Parse the command line. Copy any unused argument to GCC_ARGV. ARGC is the
   number of arguments. ARGV contains the arguments. */

static int
process_args (int argc, char *argv[], char *gcc_argv[])
{
  int i;
  int j = 0;

  for (i = 1; i < argc; i ++)
    {
      if (! strcmp (argv[i], "-debug"))
	debug = 1;
      else if (! strcmp (argv[i], "-flto"))
	lto_mode = LTO_MODE_LTO;
      else if (! strcmp (argv[i], "-fwhopr"))
	lto_mode = LTO_MODE_WHOPR;
      else
	{
	  gcc_argv[j] = argv[i];
	  j++;
	}
    }

  return j;
}


/* Entry point.  */

int
main (int argc, char *argv[])
{
  char **gcc_argv;
  int gcc_argc;

  gcc_init_libintl ();

  /* We may be called with all the arguments stored in some file and
     passed with @file.  Expand them into argv before processing.  */
  expandargv (&argc, &argv);
  gcc_argv = (char **) xcalloc (sizeof (char *), argc);
  gcc_argc = process_args (argc, argv, gcc_argv);
  run_gcc (gcc_argc, gcc_argv);
  free (gcc_argv);

  return 0;
}
