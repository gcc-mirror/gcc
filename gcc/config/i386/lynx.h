/* Definitions for Intel 386 running LynxOS.
   Copyright (C) 1993, 1995 Free Software Foundation, Inc.

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

#include <i386/gstabs.h>
#include <lynx.h>

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -DI386 -DLynx -DIBITS32 -Asystem(unix) -Asystem(lynx) -Acpu(i386) -Amachine(i386)"

/* This is how to output a reference to a user-level label named NAME.  */

/* Override the svr3 convention of adding a leading underscore. */

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME) fprintf (FILE, "%s", NAME)

/* Apparently LynxOS clobbers ebx when you call into the OS.  */

#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg*/ \
{  1, 1, 1, 1, 0, 0, 0, 1, 1,  1,  1,  1,  1,  1,  1,  1,  1 }
