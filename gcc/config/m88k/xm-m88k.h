/* Configuration for GNU compiler.
   Motorola m88100 in an 88open OCS/BCS environment.
   Copyright (C) 1988, 1989, 1990, 1991, 1993 Free Software Foundation, Inc.

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
#define HOST_BITS_PER_CHAR 8
#define HOST_BITS_PER_SHORT 16
#define HOST_BITS_PER_INT 32
#define HOST_BITS_PER_LONG 32
#define HOST_BITS_PER_LONGLONG 64

#define HOST_WORDS_BIG_ENDIAN

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

/* Use System V memory functions.  */
#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

#define rindex strrchr
#define index strchr

/* The 88open BCS (and ABI) environment doesn't support BSD features
   (vfork, getrusage), so use USG.  The Omron Luna/88k is BSD though.  */
#ifndef luna88k
#define USG
#define NO_SYS_SIGLIST
#endif

/* Define HAVE_VPRINTF if it is available on host system.  */
#define HAVE_VPRINTF

/* If not compiled with GNU C, use the C alloca */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

/* For DG/UX, the best size is different.  */
#ifdef __DGUX__
#define OBSTACK_CHUNK_SIZE (8192-16)
#endif

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"
