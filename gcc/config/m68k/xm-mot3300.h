/* Configuration for GNU C-compiler for Motorola 68000 family.
   SysV68 Motorola 3300 Delta Series
   Copyright (C) 1994 Free Software Foundation, Inc.

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


#define USG

#include "m68k/xm-m68k.h"

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

#define rindex strrchr
#define index strchr

#define NO_SYS_SIGLIST

/* do not use alloca from -lPW with cc, because function epilogues use %sp */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif
