/* Generate gcov version string from version.c. See gcov-io.h for
   description of how the version string is generated.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell <nathan@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "version.c" /* We want the actual string.  */

int main (int, char **);

int
main (int argc ATTRIBUTE_UNUSED, char **argv ATTRIBUTE_UNUSED)
{
  unsigned version = 0;
  unsigned char v[4];
  unsigned ix;
  char const *ptr = version_string;
  unsigned major, minor = 0;
  char s = 0;

  major = atoi (ptr);
  while (*ptr && *ptr != '.')
    ptr++;
  if (*ptr)
    minor = atoi (ptr + 1);
  while (*ptr)
    if (*ptr++ == '(')
      {
	s = *ptr;
	break;
      }

  v[0] = (major < 10 ? '0' : 'A' - 10) + major;
  v[1] = (minor / 10) + '0';
  v[2] = (minor % 10) + '0';
  v[3] = s ? s : '*';

  for (ix = 0; ix != 4; ix++)
    version = (version << 8) | v[ix];

  printf ("/* Generated automatically by the program `%s'\n", argv[0]);
  printf ("   from `%s'.  */\n", version_string);
  printf ("\n");
  printf ("#define GCOV_VERSION ((gcov_unsigned_t)%#08x)  /* %.4s */\n",
	  version, v);

  return 0;
}
