/* Configuration for GNU compiler
   for an Intel i386 or later processor running OS/2 2.x.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Contributed by Samuel Figueroa (figueroa@cs.nyu.edu)

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

#ifndef OS2
#define OS2
#endif

#ifdef __IBMC__
#include <stdlib.h> /* this defines alloca */
#define USG
#define ONLY_INT_FIELDS
#define HAVE_PUTENV
#define USE_PROTOTYPES 1
#define bcmp(a,b,c) memcmp (a,b,c)
#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define index  strchr
#define rindex strrchr
#define kill(a,b) raise(b)
#define mktemp tmpnam
#else
#define ____386BSD____
int spawnv (int modeflag, char *path, char *argv[]);
int spawnvp (int modeflag, char *path, char *argv[]);
#endif /* __IBMC__ */
#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR ';'
#endif

#define EXECUTABLE_SUFFIX ".exe"

#include "i386/xm-i386.h"
