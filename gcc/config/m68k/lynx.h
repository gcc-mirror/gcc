/* Definitions for Motorola 680x0 running LynxOS.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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

#include "m68k/m68k.h"
#include "m68k/m68k-coff.h"

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#undef ASM_OUTPUT_DESTRUCTOR
#undef SELECT_RTX_SECTION

#define BSS_SECTION_ASM_OP ".bss"

#define ASM_LONG ".long"

#include "lynx.h"

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 7
#endif

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dmc68000 -DM68K -DLynx -DIBITS32 -Asystem(unix) -Asystem(lynx) -Acpu(m68k) -Amachine(m68k)"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* Lynx uses d2 and d3 as scratch registers. */
#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS \
 {1, 1, 1, 1, 0, 0, 0, 0,   \
  1, 1, 0, 0, 0, 0, 0, 1,   \
  1, 1, 0, 0, 0, 0, 0, 0 }
