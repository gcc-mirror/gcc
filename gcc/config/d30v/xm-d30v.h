/* Configuration for GNU C-compiler for Mitsubishi D30V.
   Copyright (C) 1997, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

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

/* A C expression for the status code to be returned when the compiler exits
   after serious errors.  */
#define FATAL_EXIT_CODE 33

/* A C expression for the status code to be returned when the compiler exits
   without serious errors.  */
#define SUCCESS_EXIT_CODE 0

/* Defined if the host machine stores words of multi-word values in big-endian
   order.  (GNU CC does not depend on the host byte ordering within a word.)  */
#define HOST_WORDS_BIG_ENDIAN

/* A C expression for the number of bits in `char' on the host machine.  */
#define HOST_BITS_PER_CHAR 8

/* A C expression for the number of bits in `short' on the host machine.  */
#define HOST_BITS_PER_SHORT 16

/* A C expression for the number of bits in `int' on the host machine.  */
#define HOST_BITS_PER_INT 32

/* A C expression for the number of bits in `long' on the host machine.  */
#define HOST_BITS_PER_LONG 32

/* Define this macro to indicate that the compiler is running with the `alloca'
   implemented in C.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#else
#define alloca __builtin_alloca
#endif

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"
