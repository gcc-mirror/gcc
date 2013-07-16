/* Utility functions for finding files relative to GCC binaries.
   Copyright (C) 1992-2013 Free Software Foundation, Inc.

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
#include "filenames.h"
#include "file-find.h"

static bool debug = false;

void
find_file_set_debug(bool debug_state)
{
  debug = debug_state;
}

char *
find_a_file (struct path_prefix *pprefix, const char *name, int mode)
{
  char *temp;
  struct prefix_list *pl;
  int len = pprefix->max_len + strlen (name) + 1;

  if (debug)
    fprintf (stderr, "Looking for '%s'\n", name);

#ifdef HOST_EXECUTABLE_SUFFIX
  len += strlen (HOST_EXECUTABLE_SUFFIX);
#endif

  temp = XNEWVEC (char, len);

  /* Determine the filename to execute (special case for absolute paths).  */

  if (IS_ABSOLUTE_PATH (name))
    {
      if (access (name, mode) == 0)
	{
	  strcpy (temp, name);

	  if (debug)
	    fprintf (stderr, "  - found: absolute path\n");

	  return temp;
	}

#ifdef HOST_EXECUTABLE_SUFFIX
	/* Some systems have a suffix for executable files.
	   So try appending that.  */
      strcpy (temp, name);
	strcat (temp, HOST_EXECUTABLE_SUFFIX);

	if (access (temp, mode) == 0)
	  return temp;
#endif

      if (debug)
	fprintf (stderr, "  - failed to locate using absolute path\n");
    }
  else
    for (pl = pprefix->plist; pl; pl = pl->next)
      {
	struct stat st;

	strcpy (temp, pl->prefix);
	strcat (temp, name);

	if (stat (temp, &st) >= 0
	    && ! S_ISDIR (st.st_mode)
	    && access (temp, mode) == 0)
	  return temp;

#ifdef HOST_EXECUTABLE_SUFFIX
	/* Some systems have a suffix for executable files.
	   So try appending that.  */
	strcat (temp, HOST_EXECUTABLE_SUFFIX);

	if (stat (temp, &st) >= 0
	    && ! S_ISDIR (st.st_mode)
	    && access (temp, mode) == 0)
	  return temp;
#endif
      }

  if (debug && pprefix->plist == NULL)
    fprintf (stderr, "  - failed: no entries in prefix list\n");

  free (temp);
  return 0;
}

/* Add an entry for PREFIX to prefix list PPREFIX.  */

void
add_prefix (struct path_prefix *pprefix, const char *prefix)
{
  struct prefix_list *pl, **prev;
  int len;

  if (pprefix->plist)
    {
      for (pl = pprefix->plist; pl->next; pl = pl->next)
	;
      prev = &pl->next;
    }
  else
    prev = &pprefix->plist;

  /* Keep track of the longest prefix.  */

  len = strlen (prefix);
  if (len > pprefix->max_len)
    pprefix->max_len = len;

  pl = XNEW (struct prefix_list);
  pl->prefix = xstrdup (prefix);

  if (*prev)
    pl->next = *prev;
  else
    pl->next = (struct prefix_list *) 0;
  *prev = pl;
}

/* Take the value of the environment variable ENV, break it into a path, and
   add of the entries to PPREFIX.  */

void
prefix_from_env (const char *env, struct path_prefix *pprefix)
{
  const char *p;
  p = getenv (env);

  if (p)
    prefix_from_string (p, pprefix);
}

void
prefix_from_string (const char *p, struct path_prefix *pprefix)
{
  const char *startp, *endp;
  char *nstore = XNEWVEC (char, strlen (p) + 3);

  if (debug)
    fprintf (stderr, "Convert string '%s' into prefixes, separator = '%c'\n", p, PATH_SEPARATOR);

  startp = endp = p;
  while (1)
    {
      if (*endp == PATH_SEPARATOR || *endp == 0)
	{
	  strncpy (nstore, startp, endp-startp);
	  if (endp == startp)
	    {
	      strcpy (nstore, "./");
	    }
	  else if (! IS_DIR_SEPARATOR (endp[-1]))
	    {
	      nstore[endp-startp] = DIR_SEPARATOR;
	      nstore[endp-startp+1] = 0;
	    }
	  else
	    nstore[endp-startp] = 0;

	  if (debug)
	    fprintf (stderr, "  - add prefix: %s\n", nstore);

	  add_prefix (pprefix, nstore);
	  if (*endp == 0)
	    break;
	  endp = startp = endp + 1;
	}
      else
	endp++;
    }
  free (nstore);
}
