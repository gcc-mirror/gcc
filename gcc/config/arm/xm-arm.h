/* Configuration for GNU C-compiler for Acorn RISC Machine.
   Copyright (C) 1991, 1993 Free Software Foundation, Inc.
   Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
              and Martin Simmons (@harleqn.co.uk).
   More major hacks by Richard Earnshaw (rwe11@cl.cam.ac.uk)

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

/* A code distinguishing the floating point format of the host
   machine.  There are three defined values: IEEE_FLOAT_FORMAT,
   VAX_FLOAT_FORMAT, and UNKNOWN_FLOAT_FORMAT.  */

#define HOST_FLOAT_FORMAT IEEE_FLOAT_FORMAT

#define HOST_FLOAT_WORDS_BIG_ENDIAN 1

/* If not compiled with GNU C, use C alloca.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

/* Define this if the library function putenv is available on your machine */
#define HAVE_PUTENV 1

/* Define this if the library function vprintf is available on your machine */
#define HAVE_VPRINTF 1

/* Define this to be 1 if you know the host compiler supports prototypes, even
   if it doesn't define __STDC__, or define it to be 0 if you do not want any
   prototypes when compiling GNU CC. */
#define USE_PROTOTYPES 1

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.  */
#include "tm.h"

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

/* If we have defined POSIX, but are compiling in the BSD environment, then
   we need to define getcwd in terms of getwd.  */
#if defined (POSIX) && defined (_BSD_C)
#define HAVE_GETWD 1
#endif

/* EOF xm-arm.h */


