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
#include <stdio.h>
#include <errno.h>
#if HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include "f2c.h"

#ifdef KR_headers
/* Subroutine */ int G77_perror_0 (str, Lstr)
     char *str; ftnlen Lstr;
#else
/* Subroutine */ int G77_perror_0 (const char *str, const ftnlen Lstr)
#endif
{
  char buff[1000];
  char *bp, *blast;

  /* same technique as `system' -- what's wrong with malloc? */
  blast = buff + (Lstr < 1000 ? Lstr : 1000);
  for (bp = buff ; bp<blast && *str!='\0' ; )
    *bp++ = *str++;
  *bp = '\0';
  perror (buff);
  return 0;
}
