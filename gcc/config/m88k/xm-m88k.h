/* Configuration for GNU compiler.
   Motorola m88100 in an 88open OCS/BCS environment.
   Copyright (C) 1988, 1989, 1990, 1991, 1993, 1997, 2001
   Free Software Foundation, Inc.

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

/* This describes the machine the compiler is hosted on.  */
#define HOST_WORDS_BIG_ENDIAN

/* For DG/UX, the best size is different.  */
#ifdef __DGUX__
#define OBSTACK_CHUNK_SIZE (8192-16)
#endif
