/* Utility functions used by tools like collect2 and lto-wrapper.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "diagnostic.h"
#include "obstack.h"
#include "opts.h"
#include "options.h"
#include "simple-object.h"
#include "lto-section-names.h"
#include "collect-utils.h"

static char *response_file;

bool debug;
bool verbose;
bool save_temps;

/* Delete tempfiles.  */

void
utils_cleanup (void)
{
  static bool cleanup_done = false;

  if (cleanup_done)
    return;

  /* Setting cleanup_done prevents an infinite loop if one of the
     calls to maybe_unlink fails. */
  cleanup_done = true;

  if (response_file)
    maybe_unlink (response_file);
  tool_cleanup ();
}

/* Notify user of a non-error.  */
void
notice (const char *cmsgid, ...)
{
  va_list ap;

  va_start (ap, cmsgid);
  vfprintf (stderr, _(cmsgid), ap);
  va_end (ap);
}

void
fatal_signal (int signum)
{
  signal (signum, SIG_DFL);
  utils_cleanup ();
  /* Get the same signal again, this time not handled,
     so its normal effect occurs.  */
  kill (getpid (), signum);
}

/* Execute a program, and wait for the reply. ARGV are the arguments. The
   last one must be NULL. */

struct pex_obj *
collect_execute (char **argv)
{
  struct pex_obj *pex;
  const char *errmsg;
  int err;

  if (verbose)
    {
      char **p_argv;
      const char *str;

      for (p_argv = argv; (str = *p_argv) != (char *) 0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  pex = pex_init (0, tool_name, NULL);
  if (pex == NULL)
    fatal_error ("pex_init failed: %m");

  /* Do not use PEX_LAST here, we use our stdout for communicating with
     collect2 or the linker-plugin.  Any output from the sub-process
     will confuse that.  */
  errmsg = pex_run (pex, PEX_SEARCH, argv[0], argv, NULL,
		    NULL, &err);
  if (errmsg != NULL)
    {
      if (err != 0)
	{
	  errno = err;
	  fatal_error ("%s: %m", _(errmsg));
	}
      else
	fatal_error (errmsg);
    }

  return pex;
}


/* Wait for a process to finish, and exit if a nonzero status is found.
   PROG is the program name. PEX is the process we should wait for. */

int
collect_wait (const char *prog, struct pex_obj *pex)
{
  int status;

  if (!pex_get_status (pex, 1, &status))
    fatal_error ("can't get program status: %m");
  pex_free (pex);

  if (status)
    {
      if (WIFSIGNALED (status))
	{
	  int sig = WTERMSIG (status);
	  if (WCOREDUMP (status))
	    fatal_error ("%s terminated with signal %d [%s], core dumped",
			 prog, sig, strsignal (sig));
	  else
	    fatal_error ("%s terminated with signal %d [%s]",
			 prog, sig, strsignal (sig));
	}

      if (WIFEXITED (status))
	fatal_error ("%s returned %d exit status", prog, WEXITSTATUS (status));
    }

  return 0;
}

void
do_wait (const char *prog, struct pex_obj *pex)
{
  int ret = collect_wait (prog, pex);
  if (ret != 0)
    fatal_error ("%s returned %d exit status", prog, ret);

  if (response_file && !save_temps)
    {
      unlink (response_file);
      response_file = NULL;
    }
}

/* Unlink a temporary LTRANS file unless requested otherwise.  */

void
maybe_unlink_file (const char *file)
{
  if (!debug)
    {
      if (unlink_if_ordinary (file)
	  && errno != ENOENT)
	fatal_error ("deleting file %s: %m", file);
    }
  else
    fprintf (stderr, "[Leaving %s]\n", file);
}


/* Execute program ARGV[0] with arguments ARGV. Wait for it to finish.  */

void
fork_execute (char **argv)
{
  struct pex_obj *pex;
  char *new_argv[3];
  char *at_args;
  FILE *args;
  int status;

  response_file = make_temp_file (".args");
  at_args = concat ("@", response_file, NULL);
  args = fopen (response_file, "w");
  if (args == NULL)
    fatal_error ("failed to open %s", response_file);

  status = writeargv (&argv[1], args);

  if (status)
    fatal_error ("could not write to temporary file %s",  response_file);

  fclose (args);

  new_argv[0] = argv[0];
  new_argv[1] = at_args;
  new_argv[2] = NULL;

  pex = collect_execute (new_argv);
  do_wait (new_argv[0], pex);

  free (at_args);
}
