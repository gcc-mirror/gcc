/* Configuration for GNU C-compiler for DEC Alpha.
   Copyright (C) 1990, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu).

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
#define	HOST_BITS_PER_LONG	64
#define HOST_BITS_PER_LONGLONG  64

/* #define	HOST_WORDS_BIG_ENDIAN  */

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"

/* Arguments to use with `exit'.  */
#define	SUCCESS_EXIT_CODE	0
#define	FATAL_EXIT_CODE		33

/* If compiled with GNU C, use the builtin alloca.  */
#ifndef alloca
#if defined(__GNUC__) && !defined(USE_C_ALLOCA)
#define alloca __builtin_alloca
#else
#if !defined(_WIN32) && !defined(USE_C_ALLOCA) && !defined(OPEN_VMS) && !defined(__INTERIX)
#include <alloca.h>
#else
extern void *alloca ();
#endif
#endif
#endif

/* The host compiler has problems with enum bitfields since it makes
   them signed so we can't fit all our codes in.  */

#ifndef __GNUC__
#define ONLY_INT_FIELDS
#endif

/* Declare some functions needed for this machine.  We don't want to
   include these in the sources since other machines might define them
   differently.  */

extern void *malloc (), *realloc (), *calloc ();

#ifndef inhibit_libc
#include "string.h"
#endif

/* OSF/1 is POSIX.1 compliant.  */

#define POSIX
