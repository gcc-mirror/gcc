/* Definitions for Intel 386 running LynxOS, using Lynx's old as and ld.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#include "i386/gstabs.h"
#include "lynx-ng.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -DI386 -DLynx -DIBITS32 -Asystem(unix) -Asystem(lynx) -Acpu(i386) -Amachine(i386)"

/* Provide required defaults for linker switches.  */

#undef LINK_SPEC
#define LINK_SPEC "-P1000 %{msystem-v:-V} %{mcoff:-k}"

/* Apparently LynxOS clobbers ebx when you call into the OS.  */

#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg*/ \
{  1, 1, 1, 1, 0, 0, 0, 1, 1,  1,  1,  1,  1,  1,  1,  1,  1 }
