/* Definitions of target machine for IA-64.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

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

/* Defined if the host machine stores words of multi-word values in big-endian
   order.  (GNU CC does not depend on the host byte ordering within a word.)  */
#ifdef __BIG_ENDIAN__
#define HOST_WORDS_BIG_ENDIAN
#endif

/* ??? This depends on the as yet unimplemented ILP32 option.  */

/* A C expression for the number of bits in `long' on the host machine.  */
#define HOST_BITS_PER_LONG 64
