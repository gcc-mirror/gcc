/* proj.h file for Gnu Fortran
   Copyright (C) 1995, 1996, 2000, 2001, 2002 Free Software Foundation, Inc.
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
02111-1307, USA.

*/

#ifndef GCC_F_PROJ_H
#define GCC_F_PROJ_H

#ifdef USE_HCONFIG
#include "hconfig.h"
#else
#include "config.h"
#endif
#include "system.h"

#if (GCC_VERSION < 2000)
 #error "You have to use gcc 2.x to build g77."
#endif

/* Include files everyone gets.  <assert.h> is needed for assert().  */

#include "assert.h"

#ifndef UNUSED	/* Compile with -DUNUSED= if cc doesn't support this. */
#define UNUSED ATTRIBUTE_UNUSED
#endif  /* !defined (UNUSED) */

#ifndef dmpout
#define dmpout stderr
#endif

#endif /* ! GCC_F_PROJ_H */
