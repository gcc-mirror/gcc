/* Wrapper to call lto.  Used by collect2 and the linker plugin.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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
#include "obstack.h"

int debug;				/* true if -debug */

enum lto_mode_d {
  LTO_MODE_NONE,			/* Not doing LTO. */
  LTO_MODE_LTO,				/* Normal LTO. */
  LTO_MODE_WHOPR			/* WHOPR. */
};

/* Current LTO mode.  */
static enum lto_mode_d lto_mode = LTO_MODE_NONE;

static char *ltrans_output_file;
static char *flto_out;
static char *args_name;

static void maybe_unlink_file (const char *);

/* Delete tempfiles and exit function.  */

static void
lto_wrapper_exit (int status)
{
  static bool cleanup_done = false;
  if (!cleanup_done)
    {
      /* Setting cleanup_done prevents an infinite loop if one of the
         calls to maybe_unlink_file fails. */
      cleanup_done = true;

      if (ltrans_output_file)
        maybe_unlink_file (ltrans_output_file);
      if (flto_out)
        maybe_unlink_file (flto_out);
      if (args_name)
        maybe_unlink_file (args_name);
    }
  exit (status);
}

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

  lto_wrapper_exit (FATAL_EXIT_CODE);
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

  lto_wrapper_exit (FATAL_EXIT_CODE);
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
  char *at_args;
  FILE *args;
  int status;

  args_name = make_temp_file (".args");
  at_args = concat ("@", args_name, NULL);
  args = fopen (args_name, "w");
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
  free (at_args);
}

/* Template of LTRANS dumpbase suffix.  */
#define DUMPBASE_SUFFIX ".ltrans18446744073709551615"

/* Execute gcc. ARGC is the number of arguments. ARGV contains the arguments. */

static void
run_gcc (unsigned argc, char *argv[])
{
  unsigned i;
  unsigned new_argc = argc;
  const char **new_argv;
  const char **argv_ptr;
  char *list_option_full = NULL;

  new_argc += 12;
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
	    {
	      char *output_dir, *base, *name;

	      i++;
	      output_dir = xstrdup (argv[i]);
	      base = output_dir;
	      for (name = base; *name; name++)
		if (IS_DIR_SEPARATOR (*name))
		  base = name + 1;
	      *base = '\0';

	      *argv_ptr++ = "-dumpbase";
	      if (*output_dir == '\0')
		{
		  static char current_dir[] =
		    { '.', DIR_SEPARATOR, '\0' };
		  output_dir = current_dir;
		  *argv_ptr++ = argv[i];
		}
	      else
		*argv_ptr++ = &argv[i][base - output_dir];

	      *argv_ptr++ = "-dumpdir";
	      *argv_ptr++ = output_dir;
	    }
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
      const char *collect_gcc_options, *collect_gcc;
      struct obstack env_obstack;
      bool seen_dumpbase = false;
      bool seen_o = false;
      char *dumpbase_suffix = NULL;
      unsigned j;

      if (!stream)
	fatal_perror ("fopen: %s", ltrans_output_file);

      /* Get the driver and options.  */
      collect_gcc = getenv ("COLLECT_GCC");
      if (!collect_gcc)
	fatal ("environment variable COLLECT_GCC must be set");

      /* Set the CFLAGS environment variable.  */
      collect_gcc_options = getenv ("COLLECT_GCC_OPTIONS");
      if (!collect_gcc_options)
	fatal ("environment variable COLLECT_GCC_OPTIONS must be set");

      /* Count arguments.  */
      i = 0;
      for (j = 0; collect_gcc_options[j] != '\0'; ++j)
	if (collect_gcc_options[j] == '\'')
	  ++i;

      if (i % 2 != 0)
	fatal ("malformed COLLECT_GCC_OPTIONS");

      /* Initalize the arguments for the LTRANS driver.  */
      new_argv = (const char **) xmalloc ((8 + i / 2) * sizeof (char *));
      argv_ptr = new_argv;
      *argv_ptr++ = collect_gcc;
      *argv_ptr++ = "-xlto";
      *argv_ptr++ = "-c";
      for (j = 0; collect_gcc_options[j] != '\0'; ++j)
	if (collect_gcc_options[j] == '\'')
	  {
	    char *option;

	    ++j;
	    i = j;
	    while (collect_gcc_options[j] != '\'')
	      ++j;
	    obstack_init (&env_obstack);
	    obstack_grow (&env_obstack, &collect_gcc_options[i], j - i);
	    if (seen_dumpbase)
	      obstack_grow (&env_obstack, DUMPBASE_SUFFIX,
			    sizeof (DUMPBASE_SUFFIX));
	    else
	      obstack_1grow (&env_obstack, 0);
	    option = XOBFINISH (&env_obstack, char *);
	    if (seen_dumpbase)
	      {
		dumpbase_suffix = option + 7 + j - i;
		seen_dumpbase = false;
	      }
	    if (seen_o)
	      {
		seen_o = false;
		continue;
	      }

	    /* If we see -o, skip it and its argument.  */
	    if (strncmp (option, "-o", 2) == 0)
	      {
		seen_o = true;
		continue;
	      }

	    /* LTRANS does not need -fwhopr.  */
	    if (strncmp (option, "-fwhopr", 7) != 0)
	      {
		if (strncmp (option, "-dumpbase", 9) == 0)
		  seen_dumpbase = true;
		*argv_ptr++ = option;
	      }
	  }
      *argv_ptr++ = "-fltrans";

      for (;;)
	{
	  const unsigned piece = 32;
	  char *output_name;
	  char *buf, *input_name = (char *)xmalloc (piece);
	  size_t len;

	  buf = input_name;
cont:
	  if (!fgets (buf, piece, stream))
	    break;
	  len = strlen (input_name);
	  if (input_name[len - 1] != '\n')
	    {
	      input_name = (char *)xrealloc (input_name, len + piece);
	      buf = input_name + len;
	      goto cont;
	    }
	  input_name[len - 1] = '\0';

	  if (input_name[0] == '*')
	    {
	      continue;
	      output_name = &input_name[1];
	    }
	  else
	    {
	      struct pex_obj *pex;
	      const char *errmsg;
	      int err;
	      int status;

	      /* Otherwise, add FILES[I] to lto_execute_ltrans command line
		 and add the resulting file to LTRANS output list.  */

	      /* Replace the .o suffix with a .ltrans.o suffix and write
		 the resulting name to the LTRANS output list.  */
	      obstack_init (&env_obstack);
	      obstack_grow (&env_obstack, input_name, strlen (input_name) - 2);
	      obstack_grow (&env_obstack, ".ltrans.o", sizeof (".ltrans.o"));
	      output_name = XOBFINISH (&env_obstack, char *);

	      argv_ptr[0] = "-o";
	      argv_ptr[1] = output_name;
	      argv_ptr[2] = input_name;
	      argv_ptr[3] = NULL;

	      /* Append a sequence number to -dumpbase for LTRANS.  */
	      if (dumpbase_suffix)
		snprintf (dumpbase_suffix, sizeof (DUMPBASE_SUFFIX) - 7,
			  "%lu", (unsigned long) i);

	      /* Execute the driver.  */
	      pex = pex_init (0, "lto1", NULL);
	      if (pex == NULL)
		fatal ("pex_init failed: %s", xstrerror (errno));

	      errmsg = pex_run (pex, PEX_LAST | PEX_SEARCH, new_argv[0],
				CONST_CAST (char **, new_argv),
				NULL, NULL, &err);
	      if (errmsg)
		fatal ("%s: %s", errmsg, xstrerror (err));

	      if (!pex_get_status (pex, 1, &status))
		fatal ("can't get program status: %s", xstrerror (errno));

	      if (status)
		{
		  if (WIFSIGNALED (status))
		    {
		      int sig = WTERMSIG (status);
		      fatal ("%s terminated with signal %d [%s]%s",
			     new_argv[0], sig, strsignal (sig),
			     WCOREDUMP (status) ? ", core dumped" : "");
		    }
		  else
		    fatal ("%s terminated with status %d", new_argv[0], status);
		}

	      pex_free (pex);

	      maybe_unlink_file (input_name);
	    }

	  fputs (output_name, stdout);
	  putc ('\n', stdout);
	}
      fclose (stream);
      maybe_unlink_file (ltrans_output_file);
      free (list_option_full);
      obstack_free (&env_obstack, NULL);
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
