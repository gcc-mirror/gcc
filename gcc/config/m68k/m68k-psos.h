/* Definitions of a target machine for the GNU compiler:
   68040 running pSOS, ELF object files, DBX debugging.
   Copyright (C) 1996 Free Software Foundation, Inc.

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


/* Use MOTOROLA assembler syntax, as gas is configured that way and
   glibc also seems to use it. Must come BEFORE m68k.h! */

#define MOTOROLA

/* Get generic m68k definitions. */

#include "m68k/m68k.h"
#include "m68k/m68kemb.h"

/* Default processor type is a (pure) 68040 with 68881 emulation using
   the floating-point support package. */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_68040_ONLY|MASK_BITFIELD|MASK_68881|MASK_68020)

/* Options passed to CPP, GAS, CC1 and CC1PLUS. We override
   m68k-none.h for consistency with TARGET_DEFAULT. */

#undef CPP_SPEC
#define CPP_SPEC \
"%{!mc68000:%{!m68000:%{!m68332:%{!msoft-float:-D__HAVE_68881__ }}}}\
%{!ansi:-Dmc68000 %{m68010:-Dmc68010 }%{m68020:-Dmc68020 }%{mc68020:-Dmc68020 }%{m68030:-Dmc68030 }%{m68040:-Dmc68040 }%{m68020-40:-Dmc68020 -Dmc68030 -Dmc68040 }%{m68302:-Dmc68302 }%{m68332:-Dmc68332 }%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-Dmc68040 }}}}}}}}}}}\
-D__mc68000__ -D__mc68000 %{m68010:-D__mc68010__ -D__mc68010 }%{m68020:-D__mc68020__ -D__mc68020 }%{mc68020:-D__mc68020__ -D__mc68020 }%{m68030:-D__mc68030__ -D__mc68030 }%{m68040:-D__mc68040__ -D__mc68040 }%{m68020-40:-D__mc68020__ -D__mc68030__ -D__mc68040__ -D__mc68020 -D__mc68030 -D__mc68040 }%{m68302:-D__mc68302__ -D__mc68302 }%{m68332:-D__mc68332__ -D__mc68332 }%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-D__mc68040__ -D__mc68040 }}}}}}}}}}"

#undef ASM_SPEC
#define ASM_SPEC \
"%{m68851}%{mno-68851}%{m68881}%{mno-68881}%{msoft-float:-mno-68881 }\
%{m68000}%{mc68000}%{m68010}%{m68020}%{mc68020}%{m68030}%{m68040}%{m68020-40:-mc68040}%{m68302}%{m68332}%{!m68000:%{!mc68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-mc68040}}}}}}}}}}"

#undef CC1_SPEC
#define CC1_SPEC \
 "%{m68000:%{!m68881:-msoft-float }}%{m68302:-m68000}%{m68332:-m68020 -mnobitfield %{!m68881:-msoft-float}}%{!m68000:%{!mc68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-m68040}}}}}}}}}}"

#undef CC1PLUS_SPEC
#define CC1PLUS_SPEC \
 "%{m68000:%{!m68881:-msoft-float }}%{m68302:-m68000}%{m68332:-m68020 -mnobitfield %{!m68881:-msoft-float}}%{!m68000:%{!mc68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-m68040}}}}}}}}}}"


/* Get processor-independent pSOS definitions. */

#include "psos.h"


/* end of m68k-psos.h */
