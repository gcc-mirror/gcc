/* Configuration for GNU C-compiler for the M32R processor.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the
   Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.  */

/* This describes the machine the compiler is hosted on.  */
#define HOST_BITS_PER_CHAR       8
#define HOST_BITS_PER_SHORT     16
#define HOST_BITS_PER_INT       32
#define HOST_BITS_PER_LONG      32
#define HOST_BITS_PER_LONGLONG  64

/* Doubles are stored in memory with the high order word first.
   This matters when cross-compiling.  */
#define HOST_WORDS_BIG_ENDIAN    1

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.  */
#include "tm.h"

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE	 0
#define FATAL_EXIT_CODE		33

/* If compiled with Sun CC, the use of alloca requires this #include.  */
#ifndef __GNUC__
#include "alloca.h"
#endif
