/* Configuration for GNU C-compiler for MIPS Rx000 family
   Copyright (C) 1989, 1990, 1991, 1993, 1997 Free Software Foundation, Inc.

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
#define FALSE 0
#define TRUE 1

/* This describes the machine the compiler is hosted on.  */
#define HOST_BITS_PER_CHAR 8
#define HOST_BITS_PER_SHORT 16
#define HOST_BITS_PER_INT 32
#define HOST_BITS_PER_LONG 32
#define HOST_BITS_PER_LONGLONG 64

#if !defined(MIPSEL) && !defined(__MIPSEL__)
#define HOST_WORDS_BIG_ENDIAN
#endif

/* Enable host-conditionals for MIPS machines.  */
#ifndef MIPS
#define MIPS 1
#endif

/* A code distinguishing the floating point format of the host
   machine.  There are three defined values: IEEE_FLOAT_FORMAT,
   VAX_FLOAT_FORMAT, and UNKNOWN_FLOAT_FORMAT.  */

#define HOST_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

#ifndef __GNUC__
/* The MIPS compiler gets it wrong, and treats enumerated bitfields
   as signed quantities, making it impossible to use an 8-bit enum
   for compiling GNU C++.  */
#define ONLY_INT_FIELDS 1
#endif

#ifndef MIPS_OVERRIDE_ALLOCA
#ifndef __GNUC__
#define USE_C_ALLOCA

#ifdef __STDC__
extern void * alloca ();
#else
extern char * alloca ();
#endif

/* for the emacs version of alloca */
#define STACK_DIRECTION	-1
#endif
#endif /* not MIPS_OVERRIDE_ALLOCA */

/* Say if we have vprintf.  BSD Mips targets probably don't have vfprintf.  */
#if defined(__OSF1__) || defined(__OSF__) || defined(__osf__) || defined(bsd4_4)

#else
#define NO_STAB_H		/* mips doesn't typically have stab.h */
#endif
