/* Definitions of target machine for GNU compiler.
   Motorola 88100 in an 88open ABI environment.
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.
   Written by Ron Guilmette (rfg@ncd.com).
   Contributed to FSF by Network Computing Devices.
   Other contributions by Vince Guarna (vguarna@urbana.mcd.mot.com),
   Ray Essick (essick@i88.isc.com), and Wilson Tien (wtien@urbana.mcd.mot.com).
   Currently supported by Tom Wood (wood@dg-rtp.dg.com)

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

/* DWARF_DEBUGGING_INFO defined in svr4.h.  */

#ifndef NO_BUGS
#define AS_BUG_DOT_LABELS
#define AS_BUG_POUND_TYPE
#endif

#include "svr4.h"
#include "m88k.h"

/* Identify the compiler.  */
#undef  VERSION_INFO1
#define VERSION_INFO1 "88open ABI, "

/* Default switches */
#undef	TARGET_DEFAULT
#define TARGET_DEFAULT	(MASK_CHECK_ZERO_DIV | \
			 MASK_OCS_DEBUG_INFO | \
			 MASK_SVR4)

/* Cpp spec.  These pre-assertions are needed for SVR4 as they occur
   often in the system header files.  __svr4__ is our extension.  */

#undef  CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-Dm88000 -Dm88k -Dunix -D__svr4__ -Amachine(m88k) -Acpu(m88k) -Asystem(unix)"

/* For the AT&T SVR4 port, the function is _mcount.  */
#undef	FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) \
  output_function_profiler (FILE, LABELNO, "_mcount", 1)

/* Override svr4.h and m88k.h.  */
#undef	INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP "section\t.init,\"xa\",#progbits"
#undef	CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"section\t.ctors,\"a\",#progbits"
#undef	DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	"section\t.dtors,\"a\",#progbits"
