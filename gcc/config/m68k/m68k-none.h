/* Definitions of target machine for GNU compiler.  "naked" 68020.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "m68k/m68k.h"

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 7
#endif

/* Names to predefine in the preprocessor for this target machine.
   Always define mc68000.  Other definitions depend on switches given
   to the compiler:

   -m68000: define nothing else
   default, -m68020, -mc68020: define mc68020
   -m68030: define mc68030
   -m68040: define mc68040
   -m68020-40: define mc68020 mc68030 mc68040
   -m68302: define mc68302
   -m68332: define mc68332
   */

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000"
#endif

#ifndef CPP_SPEC

#if TARGET_DEFAULT & 02

/* -m68881 is the default */
#define CPP_SPEC \
"%{!mc68000:%{!m68000:%{!m68332:%{!msoft-float:%{mfpa:-D__HAVE_FPA__ }%{!mfpa:-D__HAVE_68881__ }}}}}\
%{!ansi:%{m68010:-Dmc68010 }%{m68020:-Dmc68020 }%{mc68020:-Dmc68020 }%{m68030:-Dmc68030 }%{m68040:-Dmc68040 }%{m68020-40:-Dmc68020 -Dmc68030 -Dmc68040 }%{m68302:-Dmc68302 }%{m68332:-Dmc68332 }%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-Dmc68020 }}}}}}}}}}}\
%{m68010:-D__mc68010__ -D__mc68010 }%{m68020:-D__mc68020__ -D__mc68020 }%{mc68020:-D__mc68020__ -D__mc68020 }%{m68030:-D__mc68030__ -D__mc68030 }%{m68040:-D__mc68040__ -D__mc68040 }%{m68020-40:-D__mc68020__ -D__mc68030__ -D__mc68040__ -D__mc68020 -D__mc68030 -D__mc68040 }%{m68302:-D__mc68302__ -D__mc68302 }%{m68332:-D__mc68332__ -D__mc68332 }%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-D__mc68020__ -D__mc68020 }}}}}}}}}}"

#else
#if TARGET_DEFAULT & 0100

/* -mfpa is the default */
#define CPP_SPEC \
"%{!msoft-float:%{m68881:-D__HAVE_68881__ }%{!m68881:-D__HAVE_FPA__ }}\
%{!ansi:%{m68010:-Dmc68010 }%{m68020:-Dmc68020 }%{mc68020:-Dmc68020 }%{m68030:-Dmc68030 }%{m68040:-Dmc68040 }%{m68020-40:-Dmc68020 -Dmc68030 -Dmc68040 }%{m68302:-Dmc68302 }%{m68332:-Dmc68332 }%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-Dmc68020 }}}}}}}}}}}\
%{m68010:-D__mc68010__ -D__mc68010 }%{m68020:-D__mc68020__ -D__mc68020 }%{mc68020:-D__mc68020__ -D__mc68020 }%{m68030:-D__mc68030__ -D__mc68030 }%{m68040:-D__mc68040__ -D__mc68040 }%{m68020-40:-D__mc68020__ -D__mc68030__ -D__mc68040__ -D__mc68020 -D__mc68030 -D__mc68040 }%{m68302:-D__mc68302__ -D__mc68302 }%{m68332:-D__mc68332__ -D__mc68332 }%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-D__mc68020__ -D__mc68020 }}}}}}}}}}"

#else

/* -msoft-float is the default */
#define CPP_SPEC \
"%{m68881:-D__HAVE_68881__ }%{mfpa:-D__HAVE_FPA__ }\
%{!ansi:%{m68010:-Dmc68010 }%{m68020:-Dmc68020 }%{mc68020:-Dmc68020 }%{m68030:-Dmc68030 }%{m68040:-Dmc68040 }%{m68020-40:-Dmc68020 -Dmc68030 -Dmc68040 }%{m68302:-Dmc68302 }%{m68332:-Dmc68332 }%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-Dmc68020 }}}}}}}}}}}\
%{m68010:-D__mc68010__ -D__mc68010 }%{m68020:-D__mc68020__ -D__mc68020 }%{mc68020:-D__mc68020__ -D__mc68020 }%{m68030:-D__mc68030__ -D__mc68030 }%{m68040:-D__mc68040__ -D__mc68040 }%{m68020-40:-D__mc68020__ -D__mc68030__ -D__mc68040__ -D__mc68020 -D__mc68030 -D__mc68040 }%{m68302:-D__mc68302__ -D__mc68302 }%{m68332:-D__mc68332__ -D__mc68332 }%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-D__mc68020__ -D__mc68020 }}}}}}}}}}"

#endif
#endif

#endif

/* Pass flags to gas indicating which type of processor we have.  */

#ifndef ASM_SPEC

#define ASM_SPEC \
"%{m68851}%{mno-68851}%{m68881}%{mno-68881}%{msoft-float:-mno-68881 }\
%{m68000}%{mc68000}%{m68010}%{m68020}%{mc68020}%{m68030}%{m68040}%{m68020-40:-mc68040}%{m68302}%{m68332}%{!m68000:%{!mc68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-mc68020}}}}}}}}}}"

#endif

#ifndef CC1_SPEC

#define CC1_SPEC \
 "%{m68000:%{!m68881:-msoft-float }}%{m68302:-m68000}%{m68332:-m68020 -mnobitfield %{!m68881:-msoft-float}}%{!m68000:%{!mc68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:-m68020}}}}}}}}}}"

#endif

/* end of m68k-none.h */
