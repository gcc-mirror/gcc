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
#include "f2c.h"
#include "fio.h"

#ifdef KR_headers
integer G77_fnum_0 (lunit)
     integer *lunit;
#else
integer G77_fnum_0 (integer *lunit)
#endif
{
  if (*lunit>=MXUNIT || *lunit<0)
    err(1,101,"fnum");
  /* f__units is a table of descriptions for the unit numbers (defined
     in io.h).  Use file descriptor (ufd) and fileno rather than udev
     field since udev is unix specific */
  return fileno(f__units[*lunit].ufd);
}
