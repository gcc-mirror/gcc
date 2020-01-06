/* Utility functions used by tools like collect2 and lto-wrapper.
   Copyright (C) 2009-2020 Free Software Foundation, Inc.

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
  utils_cleanup (true);
  /* Get the same signal again, this time not handled,
     so its normal effect occurs.  */
  kill (getpid (), signum);
}

/* Wait for a process to finish, and exit if a nonzero status is found.  */

int
collect_wait (const char *prog, struct pex_obj *pex)
{
  int status;

  if (!pex_get_status (pex, 1, &status))
    fatal_error (input_location, "cannot get program status: %m");
  pex_free (pex);

  if (response_file && !save_temps)
    {
      unlink (response_file);
      response_file = NULL;
    }

  if (status)
    {
      if (WIFSIGNALED (status))
	{
	  int sig = WTERMSIG (status);
	  fatal_error (input_location, "%s terminated with signal %d [%s]%s",
		       prog, sig, strsignal (sig),
		       WCOREDUMP (status) ? ", core dumped" : "");
	}

      if (WIFEXITED (status))
	return WEXITSTATUS (status);
    }
  return 0;
}

void
do_wait (const char *prog, struct pex_obj *pex)
{
  int ret = collect_wait (prog, pex);
  if (ret != 0)
    fatal_error (input_location, "%s returned %d exit status", prog, ret);
}


/* Execute a program, and wait for the reply.  */

struct pex_obj *
collect_execute (const char *prog, char **argv, const char *outname,
		 const char *errname, int flags, bool use_atfile)
{
  struct pex_obj *pex;
  const char *errmsg;
  int err;
  char *response_arg = NULL;
  char *response_argv[3];

  if (use_atfile && argv[0] != NULL)
    {
      /* If using @file arguments, create a temporary file and put the
         contents of argv into it.  Then change argv to an array corresponding
         to a single argument @FILE, where FILE is the temporary filename.  */

      char **current_argv = argv + 1;
      char *argv0 = argv[0];
      int status;
      FILE *f;

      /* Note: we assume argv contains at least one element; this is
         checked above.  */

      response_file = make_temp_file ("");

      f = fopen (response_file, "w");

      if (f == NULL)
        fatal_error (input_location, "could not open response file %s",
		     response_file);

      status = writeargv (current_argv, f);

      if (status)
        fatal_error (input_location, "could not write to response file %s",
		     response_file);

      status = fclose (f);

      if (EOF == status)
        fatal_error (input_location, "could not close response file %s",
		     response_file);

      response_arg = concat ("@", response_file, NULL);
      response_argv[0] = argv0;
      response_argv[1] = response_arg;
      response_argv[2] = NULL;

      argv = response_argv;
    }

  if (verbose || debug)
    {
      char **p_argv;
      const char *str;

      if (argv[0])
	fprintf (stderr, "%s", argv[0]);
      else
	notice ("[cannot find %s]", prog);

      for (p_argv = &argv[1]; (str = *p_argv) != (char *) 0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  /* If we cannot find a program we need, complain error.  Do this here
     since we might not end up needing something that we could not find.  */

  if (argv[0] == 0)
    fatal_error (input_location, "cannot find %qs", prog);

  pex = pex_init (0, "collect2", NULL);
  if (pex == NULL)
    fatal_error (input_location, "%<pex_init%> failed: %m");

  errmsg = pex_run (pex, flags, argv[0], argv, outname,
		    errname, &err);
  if (errmsg != NULL)
    {
      if (err != 0)
	{
	  errno = err;
	  fatal_error (input_location, "%s: %m", _(errmsg));
	}
      else
	fatal_error (input_location, errmsg);
    }

  free (response_arg);

  return pex;
}

void
fork_execute (const char *prog, char **argv, bool use_atfile)
{
  struct pex_obj *pex;

  pex = collect_execute (prog, argv, NULL, NULL,
			 PEX_LAST | PEX_SEARCH, use_atfile);
  do_wait (prog, pex);
}

/* Delete tempfiles.  */

void
utils_cleanup (bool from_signal)
{
  static bool cleanup_done = false;

  if (cleanup_done)
    return;

  /* Setting cleanup_done prevents an infinite loop if one of the
     calls to maybe_unlink fails. */
  cleanup_done = true;

  tool_cleanup (from_signal);
}
