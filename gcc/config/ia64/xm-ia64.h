/* Definitions of target machine for IA64.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

/* A C expression for the status code to be returned when the compiler exits
   after serious errors.  */
#define FATAL_EXIT_CODE 33

/* A C expression for the status code to be returned when the compiler exits
   without serious errors.  */
#define SUCCESS_EXIT_CODE 0

/* Defined if the host machine stores words of multi-word values in big-endian
   order.  (GNU CC does not depend on the host byte ordering within a word.)  */
#ifdef __BIG_ENDIAN__
#define HOST_WORDS_BIG_ENDIAN
#endif

/* A C expression for the number of bits in `char' on the host machine.  */
#define HOST_BITS_PER_CHAR 8

/* A C expression for the number of bits in `short' on the host machine.  */
#define HOST_BITS_PER_SHORT 16

/* A C expression for the number of bits in `int' on the host machine.  */
#define HOST_BITS_PER_INT 32

/* ??? This depends on the as yet unimplemented ILP32 option.  */

/* A C expression for the number of bits in `long' on the host machine.  */
#define HOST_BITS_PER_LONG 64

/* A C expression for the number of bits in `long long' on the host
   machine.  */
#define HOST_BITS_PER_LONGLONG 64

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */
#include "tm.h"

/* end of xm-ia64.h */
