/* Configuration for GNU C-compiler for System/370.
   Copyright (C) 1989, 1993 Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modifed for MVS C/370 by Dave Pitts (pitts@mcdata.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* #defines that need visibility everywhere.  */

#define FALSE 0
#define TRUE 1

/* This describes the machine the compiler is hosted on.  */

#define HOST_BITS_PER_CHAR	8
#define HOST_BITS_PER_SHORT	16
#define HOST_BITS_PER_INT	32
#define HOST_BITS_PER_LONG	32
#define HOST_FLOAT_FORMAT	IBM_FLOAT_FORMAT
#define HOST_EBCDIC		1

#define USG
#ifndef MVS
#define MVS
#endif

/* Target machine dependencies.  tm.h is a symbolic link to the actual
   target specific file.  */

#include "tm.h"

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

/* Arguments to use with `exit'.  */

#define SUCCESS_EXIT_CODE	0
#define FATAL_EXIT_CODE		12

#define NO_DBX_FORMAT

