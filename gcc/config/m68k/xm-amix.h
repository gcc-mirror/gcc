/* Definitions of host machine for GNU compiler.
   Commodore Amiga A3000UX version.
   Copyright (C) 1991, 1997 Free Software Foundation, Inc.

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

#include "m68k/xm-m68kv.h"	/* Use the System V flavor of m68k host */

#define rindex strrchr
#define index strchr

/* Define FULL_PROTOTYPES for protoize.c, to get <unistd.h> included.
   We need this file for things like R_OK, not necessarily prototypes. */

#define FULL_PROTOTYPES

#if defined (__GNUC__) && __GNUC__ == 1
#define alloca __builtin_alloca
#endif
