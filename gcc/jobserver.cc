/* GNU Jobserver Integration Interface.
   Copyright (C) 2005-2020 Free Software Foundation, Inc.

   Contributed by Giuliano Belinassi <giuliano.belinassi@usp.br>

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

#include "jobserver.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "errno.h"

/* Token which make sent when invoking GCC.  */
#define JOBSERVER_MAKE_TOKEN  '+'

bool jobserver_initialized = false;
bool nonblock_mode = false;
static int wfd = -1;
static int rfd = -1;

jobserver_token_t jobserver_curr_token = JOBSERVER_NULL_TOKEN;

/* When using GNU Jobserver but the user did not prepend the recursive make
   token `+' to the GCC invocation, Make can close the file descriptors used
   to comunicate with us, and there is no reliable way to detect this.
   Therefore the best we can do is crash and alert the user to do hte right
   thing.  */
static void jobserver_crash ()
{
  fatal_error (UNKNOWN_LOCATION,
	       "-fparallel-jobs=jobserver, but Make jobserver pipe is closed");
}

/* Initialize this interface.  We try to find whether the Jobserver is active
   and working.  */
bool jobserver_initialize ()
{
  bool success;
  const char *makeflags = getenv ("MAKEFLAGS");
  if (makeflags == NULL)
    return false;

  const char *needle = "--jobserver-auth=";
  const char *n = strstr (makeflags, needle);
  if (n == NULL)
    return false;

  success = (sscanf (n + strlen (needle), "%d,%d", &rfd, &wfd) == 2
		    && rfd > 0
		    && wfd > 0
		    && is_valid_fd (rfd)
		    && is_valid_fd (wfd));

  if (!success)
    return false;

  struct stat statbuf;
  if (fstat (rfd, &statbuf) < 0
      || (statbuf.st_mode & S_IFMT) != S_IFIFO
      || fstat (wfd, &statbuf) < 0
      || (statbuf.st_mode & S_IFMT) != S_IFIFO)
    return false;

  int flags = fcntl (rfd, F_GETFL);
  if (flags < 0)
    return false;

  /* Mark that rfd is O_NONBLOCK, as we will have to use select.  */
  if (flags & O_NONBLOCK)
    nonblock_mode = true;

  return (jobserver_initialized = true);
}

/* Finalize the jobserver, so this interface could be used again later.  */
bool jobserver_finalize ()
{
  if (!jobserver_initialized)
    return false;

  close (rfd);
  close (wfd);

  rfd = wfd = -1;
  nonblock_mode = false;

  jobserver_initialized = false;
  return true;
}

/* Return token to the jobserver.  If c is the NULL token, then return
   the last token we got.  */
void jobserver_return_token (jobserver_token_t c)
{
  ssize_t w;

  if (c == JOBSERVER_NULL_TOKEN)
    c = jobserver_curr_token;

  w = write (wfd, &c, sizeof (jobserver_token_t));

  if (w <= 0)
    jobserver_crash ();
}

/* TODO: Check if select if available in our system.  */
#define HAVE_SELECT

/* Retrieve a token from the Jobserver.  We have two cases, in which we must be
   careful.  First is when the function pselect is available in our system, as
   Make will set the read fd as nonblocking and will expect that we use select.
   (see posixos.c in GNU Make sourcecode).
   The other is when select is not available in our system, and Make will set
   it as blocking.  */
char jobserver_get_token ()
{
  jobserver_token_t ret = JOBSERVER_NULL_TOKEN;
  ssize_t r = -1;

  while (r < 0)
    {
      if (nonblock_mode)
	{
#ifdef HAVE_SELECT
	  fd_set readfd_set;

	  FD_ZERO (&readfd_set);
	  FD_SET (rfd, &readfd_set);

	  r = select (rfd+1, &readfd_set, NULL, NULL, NULL);

	  if (r < 0 && errno == EAGAIN)
	    continue;

	  gcc_assert (r > 0);
#else
	  internal_error ("Make set Jobserver pipe to nonblock mode, but "
			  " select is not supported in your system");
#endif
	}

      r = read (rfd, &ret, sizeof (jobserver_token_t));

      if (!(r > 0 || (r < 0 && errno == EAGAIN)))
	{
	  jobserver_crash ();
	  break;
	}
    }

  return (jobserver_curr_token = ret);
}
