/* Definitions of target machine for GNU compiler.
   Motorola 88100 in an 88open ABI environment.
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@netcom.com).
   Contributed to FSF by Network Computing Devices.

   Other contributions by Vince Guarna (vguarna@urbana.mcd.mot.com),
   Ray Essick (essick@i88.isc.com), Wilson Tien (wtien@urbana.mcd.mot.com),
   and Tom Wood (Tom_Wood@NeXT.com)

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

/* DWARF_DEBUGGING_INFO defined in svr4.h.  */

#ifndef NO_BUGS
#define AS_BUG_DOT_LABELS
#define AS_BUG_POUND_TYPE
#endif

#include "svr4.h"
#include "m88k/m88k.h"

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
  "-Dm88000 -Dm88k -Dunix -D__svr4__ -Amachine(m88k) -Acpu(m88k) -Asystem(unix) -Asystem(svr4)"

/* For the AT&T SVR4 port, the function is _mcount.  */
#undef	FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) \
  output_function_profiler (FILE, LABELNO, "_mcount", 1)

/* Override svr4.h and m88k.h.  */
#undef	INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP "section\t.init,\"xa\",#progbits"
#undef	FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP "section\t.fini,\"xa\",#progbits"

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.
 
   Note that we want to give these sections the SHF_WRITE attribute
   because these sections will actually contain data (i.e. tables of
   addresses of functions in the current root executable or shared library
   file) and, in the case of a shared library, the relocatable addresses
   will have to be properly resolved/relocated (and then written into) by
   the dynamic linker when it actually attaches the given shared library
   to the executing process.  (Note that on SVR4, you may wish to use the
   `-z text' option to the ELF linker, when building a shared library, as
   an additional check that you are doing everything right.  But if you do
   use the `-z text' option when building a shared library, you will get
   errors unless the .ctors and .dtors sections are marked as writable
   via the SHF_WRITE attribute.)  */
 
#undef	CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"section\t.ctors,\"aw\""
#undef	DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	"section\t.dtors,\"aw\""
