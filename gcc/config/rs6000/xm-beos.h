/* Configuration for GNU C-compiler for BeOS host.
   Copyright (C) 1997 Free Software Foundation, Inc.
   Contributed by Fred Fish (fnf@cygnus.com), based on xm-rs6000.h
   by Richard Kenner (kenner@vlsi1.ultra.nyu.edu).


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


/* #defines that need visibility everywhere.  */

#define	FALSE	0
#define	TRUE	1

/* This describes the machine the compiler is hosted on.  */

#define	HOST_BITS_PER_CHAR	8
#define	HOST_BITS_PER_SHORT	16
#define	HOST_BITS_PER_INT	32
#define	HOST_BITS_PER_LONG	32
#define HOST_BITS_PER_LONGLONG	64

#define	HOST_WORDS_BIG_ENDIAN

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */

#include "tm.h"

/* Arguments to use with `exit'.  */

#define	SUCCESS_EXIT_CODE	0
#define	FATAL_EXIT_CODE		33

/* Use the C alloca and use only int bitfields.  */

#define USE_C_ALLOCA
extern void *alloca ();
#define	ONLY_INT_FIELDS

/* use ANSI/SYSV style byte manipulation routines instead of BSD ones */

#undef bcopy
#define bcopy(s,d,n)	memmove((d),(s),(n))
#undef bzero
#define bzero(d,n)	memset((d),0,(n))
#undef bcmp
#define bcmp(l,r,n)	memcmp((l),(r),(n))
#undef index
#define index		strchr
#undef rindex
#define rindex		strrchr

/* BeOS is closer to USG than BSD */

#define USG

/* Define various things that the BeOS host has. */

#ifndef HAVE_VPRINTF
#define HAVE_VPRINTF
#endif
#ifndef HAVE_PUTENV
#define HAVE_PUTENV
#endif
#ifndef HAVE_ATEXIT
#define HAVE_ATEXIT
#endif
#ifndef HAVE_RENAME
#define HAVE_RENAME
#endif

#define STDC_HEADERS 1

/* STANDARD_INCLUDE_DIR is the equivalent of "/usr/include" on UNIX. */

#define STANDARD_INCLUDE_DIR	"/boot/develop/headers/posix"

/* SYSTEM_INCLUDE_DIR is the location for system specific, non-POSIX headers. */

#define SYSTEM_INCLUDE_DIR	"/boot/develop/headers/be"

