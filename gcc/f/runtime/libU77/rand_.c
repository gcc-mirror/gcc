/* Copyright (C) 1995 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#if HAVE_STDLIB_H
#  include <stdlib.h>
#endif
#include "f2c.h"
#ifndef RAND_MAX
#  define RAND_MAX 2147483647	/* from SunOS */
#endif

/* We could presumably do much better than the traditional libc
   version, though at least the glibc one is reasonable, it seems.
   For the sake of the innocent, I'm not sure we should really do
   this... */

/* Note this is per SunOS -- other s may have no arg. */

#ifdef KR_headers
double G77_rand_0 (flag)
  integer *flag;
#else
double G77_rand_0 (integer *flag)
#endif
{
  switch (*flag) {
  case 0:
    break;
  case 1:
    srand (0);			/* Arbitrary choice of initialiser. */
    break;
  default:
    srand (*flag);
  }
  return (float) rand () / RAND_MAX;
}
