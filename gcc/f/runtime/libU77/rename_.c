/* Copyright (C) 1995, 1997 Free Software Foundation, Inc.
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
#if HAVE_UNISTD_H
#  include <unistd.h>
#endif
#if HAVE_STDLIB_H
#  include <stdlib.h>
#endif
#include <stdio.h>
#include <errno.h>
#include "f2c.h"

#ifdef KR_headers
void g_char ();

integer G77_rename_0 (path1, path2, Lpath1, Lpath2)
     char *path1, *path2; ftnlen Lpath1, Lpath2;
#else
void g_char(const char *a, ftnlen alen, char *b);

integer G77_rename_0 (const char *path1, const char *path2, const ftnlen Lpath1, const ftnlen Lpath2)
#endif
{
  char *buff1, *buff2;
  char *bp, *blast;
  int i;

  buff1 = malloc (Lpath1+1);
  if (buff1 == NULL) return -1;
  g_char (path1, Lpath1, buff1);
  buff2 = malloc (Lpath2+1);
  if (buff2 == NULL) return -1;
  g_char (path2, Lpath2, buff2);
  i = rename (buff1, buff2);
  free (buff1); free (buff2);
  return i ? errno : 0;
}
