/* Definitions of target machine for GNU compiler.
   PC532 with National 32532, running Mach 3.0.
   Copyright (C) 1992, 1994 Free Software Foundation, Inc.

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

#include "ns32k/pc532.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dns32532 -DPC532 -DMACH=1 -Asystem(unix) -Asystem(mach) -Acpu(ns32k) -Amachine(ns32k)"

/* There's a bug in the setjmp implementation that strikes
   if the caller of setjmp doesn't have a frame pointer.  */
#undef FRAME_POINTER_REQUIRED
#define FRAME_POINTER_REQUIRED current_function_calls_setjmp
