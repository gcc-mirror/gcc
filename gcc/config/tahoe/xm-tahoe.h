/* Configuration for GNU C-compiler for Tahoe.
   Copyright (C) 1987 Free Software Foundation, Inc.

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

/*
 * File: xm-tahoe.h
 *
 * Original port made at the University of Buffalo by Devon Bowen,
 * Dale Wiles and Kevin Zachmann.
 *
 * Changes for HCX by Piet van Oostrum,
 * University of Utrecht, The Netherlands (piet@cs.ruu.nl)
 *
 * Mail bugs reports or fixes to:	gcc@cs.buffalo.edu
 */


/* This file has the same stuff the vax version does */

/* defines that need visibility everywhere */

#define FALSE 0
#define TRUE 1

/* target machine dependencies
   tm.h is a symbolic link to the actual target specific file.   */

#include "tm.h"

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

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
#define alloca __builtin_alloca
#endif
