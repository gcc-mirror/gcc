/* Define symbol to recognize CRIS ABI version 2, for a.out use.
   Contributed by Axis Communications.
   Written by Hans-Peter Nilsson <hp@axis.se>, c:a 1992.

   Copyright (C) 2000, 2001, 2003, 2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "tconfig.h"
#include "tm.h"

#ifdef __AOUT__

/* ELF support was not released before the ABI was changed, so we
   restrict this awkwardness to a.out.  This symbol is for gdb to
   recognize, so it can debug both old and new programs successfully.  */
__asm__ (".global " CRIS_ABI_VERSION_SYMBOL_STRING);
__asm__ (".set " CRIS_ABI_VERSION_SYMBOL_STRING ",0");

#else  /* not __AOUT__ */

/* The file must not be empty (declaration/definition-wise) according to
   ISO, IIRC. */
extern int _Dummy;

#endif /* not __AOUT__ */
