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
#include <errno.h>
#include <stddef.h>
#if HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include "f2c.h"

#ifndef HAVE_STRERROR
     extern char *sys_errlist [];
#    define strerror(i) (sys_errlist[i])
#endif
#ifdef KR_headers
extern void s_copy ();
/* Subroutine */ int G77_gerror_0 (str, Lstr)
     char *str; ftnlen Lstr;
#else
extern void s_copy(register char *a, register char *b, ftnlen la, ftnlen lb);
/* Subroutine */ int G77_gerror_0 (char *str, ftnlen Lstr)
#endif
{
  char * s;

  s = strerror(errno);
  s_copy (str, s, Lstr, strlen (s));
  return 0;
}
