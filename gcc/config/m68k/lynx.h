/* Definitions for Motorola 680x0 running LynxOS.
   Copyright (C) 1993, 1994, 1995, 1996, 1998, 1999, 2000
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

#include <m68k/m68k.h>
#include <m68k/coff.h>

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#undef ASM_OUTPUT_DESTRUCTOR
#undef SELECT_RTX_SECTION

#define BSS_SECTION_ASM_OP "\t.bss"

#include <lynx.h>

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_BITFIELD|MASK_68881|MASK_68020)
#endif

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dmc68000 -DM68K -DLynx -DIBITS32 -Asystem=unix -Asystem=lynx -Acpu=m68k -Amachine=m68k"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* Lynx uses d2 and d3 as scratch registers.  */
#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS \
 {1, 1, 1, 1, 0, 0, 0, 0,   \
  1, 1, 0, 0, 0, 0, 0, 1,   \
  1, 1, 0, 0, 0, 0, 0, 0 }

/* Return floating point values in a fp register.  This make fp code a
   little bit faster.  It also makes -msoft-float code incompatible with
   -m68881 code, so people have to be careful not to mix the two.  */
#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE,FUNC) LIBCALL_VALUE (TYPE_MODE (VALTYPE))

#undef LIBCALL_VALUE
#define LIBCALL_VALUE(MODE)                                                \
 gen_rtx_REG ((MODE),						\
	      ((TARGET_68881					\
		&& ((MODE) == SFmode || (MODE) == DFmode	\
		    || (MODE) == XFmode))			\
           ? 16 : 0))

#undef FUNCTION_VALUE_REGNO_P
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (TARGET_68881 && (N) == 16))

#undef NEEDS_UNTYPED_CALL
#define NEEDS_UNTYPED_CALL 1
