/* proj.c file for GNU Fortran
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.	 */

#include "proj.h"
#include "glimits.j"

#ifndef HAVE_STRTOUL
unsigned long int
strtoul (const char *nptr, char **endptr, int base)
{
  unsigned long int number = 0;
  unsigned long int old_number = 0;

  assert (base == 10);
  assert (endptr == NULL);

  while (ISDIGIT (*nptr))
    {
      number = old_number * 10 + (*(nptr++) - '0');
      if ((number <= old_number) && (old_number != 0))
	return ULONG_MAX;
      old_number = number;
    }

  return number;
}
#endif

#ifndef HAVE_BSEARCH
void *
bsearch (const void *key, const void *base, size_t nmemb, size_t size,
	 int (*compar) (const void *, const void *))
{
  size_t i;
  int cmp;

  /* We do a dumb incremental search, not a binary search, for now. */

  for (i = 0; i < nmemb; ++i)
    {
      if ((cmp = (*compar) (key, base)) == 0)
	return base;
      if (cmp < 0)
	break;
      base += size;
    }

  return NULL;
}
#endif
