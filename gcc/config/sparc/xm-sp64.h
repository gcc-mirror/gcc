/* Configuration for GCC for Sparc v9 running 64-bit native.
   Copyright (C) 1997 Free Software Foundation, Inc.

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

#include <sparc/xm-sparc.h>

/* This describes the machine the compiler is hosted on.  */
#if defined(__arch64__) || defined(__sparc_v9__) || defined(__sparcv9)
#undef HOST_BITS_PER_LONG
#define HOST_BITS_PER_LONG 64
#endif
