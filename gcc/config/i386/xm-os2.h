/* Configuration for GNU compiler
   for an Intel i386 or later processor running OS/2 2.x.
   Copyright (C) 1993, 1994, 1995, 1997, 1998, 1999 Free Software Foundation, Inc.
   Contributed by Samuel Figueroa (figueroa@apple.com)

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef OS2
#define OS2
#endif

#ifdef __IBMC__
#include <stdlib.h> /* this defines alloca */
#define USG
#define ONLY_INT_FIELDS
#define USE_PROTOTYPES 1
#define strcasecmp stricmp
#define kill(a,b) raise(b)
#define mktemp tmpnam
#else
#ifdef __EMX__
#define EMX
#define USG
#define BSTRING
#define HAVE_PUTENV
#define HAVE_VPRINTF
#define HAVE_STRERROR
#define strcasecmp stricmp
#else
#define ____386BSD____
int spawnv (int modeflag, char *path, char *argv[]);
int spawnvp (int modeflag, char *path, char *argv[]);
#endif /* __EMX__ */
#endif /* __IBMC__ */

#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR ';'
#endif
#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '\\'
#endif
#ifndef DIR_SEPARATOR_2
#define DIR_SEPARATOR_2 '/'
#endif

/* Allow handling of drive names. */
#define HAVE_DOS_BASED_FILE_SYSTEM

#define EXECUTABLE_SUFFIX ".exe"

/* The EMX compiler uses regular .o files */
#ifndef __EMX__
#define OBJECT_SUFFIX ".obj"
#endif

/* This is required to make temporary file names unique on file
   systems which severely restrict the length of file names. */
#define MKTEMP_EACH_FILE

#include "i386/xm-i386.h"
