/* Definitions of target machine for GNU compiler.  "naked" 68020.
   Copyright (C) 1994, 1996 Free Software Foundation, Inc.

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

/* Default to m68k (m68020).  */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT M68K_CPU_m68k
#endif

/* These are values set by the configure script in TARGET_CPU_DEFAULT.
   They are ((desired value for TARGET_DEFAULT) << 4) + sequential integer.
   See m68k.h for the values (it should really define MASK_FOO so we can
   use them).  */
#define M68K_CPU_m68k	((7 << 4) + 0)
#define M68K_CPU_m68000 ((0 << 4) + 1)
#define M68K_CPU_m68010 ((0 << 4) + 1) /* make same as m68000 */
#define M68K_CPU_m68020 ((7 << 4) + 2)
#define M68K_CPU_m68030 ((7 << 4) + 3)
#define M68K_CPU_m68040 ((01007 << 4) + 4)
#define M68K_CPU_m68302 ((0 << 4) + 5)
#define M68K_CPU_m68332 ((1 << 4) + 6)

/* This is tested for below, so if target wants to override this, it
   just set this first in cover file.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (TARGET_CPU_DEFAULT >> 4)
#endif

/* Defaults for the various specs below.
   These are collected here so we only test TARGET_CPU_DEFAULT once.  */
/* ??? CC1_CPU_DEFAULT_SPEC was copied over from the earlier version of
   this file.  However, it's not used anywhere here because it doesn't
   seem to be necessary.  */
#if TARGET_CPU_DEFAULT == M68K_CPU_m68k || TARGET_CPU_DEFAULT == M68K_CPU_m68020
#define CPP_CPU_DEFAULT_SPEC "%{!ansi:-Dmc68020 } -D__mc68020 -D__mc68020__"
#define ASM_CPU_DEFAULT_SPEC "-mc68020"
#define CC1_CPU_DEFAULT_SPEC "-m68020"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68000
#define CPP_CPU_DEFAULT_SPEC "%{!ansi:-Dmc68000 } -D__mc68000 -D__mc68000__"
#define ASM_CPU_DEFAULT_SPEC "-mc68000"
#define CC1_CPU_DEFAULT_SPEC "-m68000"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68030
#define CPP_CPU_DEFAULT_SPEC "%{!ansi:-Dmc68030 } -D__mc68030 -D__mc68030__"
#define ASM_CPU_DEFAULT_SPEC "-mc68030"
#define CC1_CPU_DEFAULT_SPEC "-m68030"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68040
#define CPP_CPU_DEFAULT_SPEC "%{!ansi:-Dmc68040 } -D__mc68040 -D__mc68040__"
#define ASM_CPU_DEFAULT_SPEC "-mc68040"
#define CC1_CPU_DEFAULT_SPEC "-m68040"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68302
#define CPP_CPU_DEFAULT_SPEC "%{!ansi:-Dmc68302 } -D__mc68302 -D__mc68302__"
#define ASM_CPU_DEFAULT_SPEC "-mc68302"
#define CC1_CPU_DEFAULT_SPEC "-m68302"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68332
#define CPP_CPU_DEFAULT_SPEC "%{!ansi:-Dmc68332 -Dmcpu32 } -D__mc68332 -D__mc68332__ -D__mcpu32 -D__mcpu32__"
#define ASM_CPU_DEFAULT_SPEC "-mc68332"
#define CC1_CPU_DEFAULT_SPEC "-m68332"
#else
Unrecognized value in TARGET_CPU_DEFAULT.
#endif
#endif
#endif
#endif
#endif
#endif

/* Always define mc68000.
   Remember that GCC will automatically add __mc68000 and __mc68000__.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000"

/* Define one of __HAVE_68881__, __HAVE_FPA__, __HAVE_SKY__, or nothing 
   (soft float), appropriately.  */
#undef CPP_FPU_SPEC
#if TARGET_DEFAULT & MASK_68881
#define CPP_FPU_SPEC "\
%{!mc68000:%{!m68000:%{!m68302:%{!mcpu32:%{!m68332:%{!m5200:%{!msoft-float:%{!mno-68881:%{!mfpa:%{!msky:-D__HAVE_68881__ }}}}}}}}}} \
%{m68881:-D__HAVE_68881__ }%{mfpa:-D__HAVE_FPA__ }%{msky:-D__HAVE_SKY__ }"
#else
/* This can't currently happen, but we code it anyway to show how it's done.  */
#if TARGET_DEFAULT & MASK_FPA
#define CPP_FPU_SPEC \
"%{!msoft-float:%{m68881:-D__HAVE_68881__ }%{!m68881:-D__HAVE_FPA__ }}"
#else
#define CPP_FPU_SPEC "\
%{m68881:-D__HAVE_68881__ }%{mfpa:-D__HAVE_FPA__ }%{msky:-D__HAVE_SKY__ }"
#endif
#endif

/* Names to predefine in the preprocessor for this target machine.
   Other definitions depend on what the default cpu is and switches
   given to the compiler:

   -m68000, -mc68000: define nothing else
   -m68010: define mc68010
   -m68020, -mc68020: define mc68020
   -m68030: define mc68030
   -m68040: define mc68040
   -m68060: define mc68060
   -m68020-40: define mc68020 mc68030 mc68040
   -m68020-60: define mc68020 mc68030 mc68040 mc68060
   -m68302: define mc68302 
   -m68332: define mc68332 mcpu32
   -mcpu32: define mcpu32
   -m5200: define mcf5200
   default: define as above appropriately

   GCC won't automatically add __'d versions, we have to mention them
   explicitly.  */

#undef CPP_SPEC
#define CPP_SPEC "\
%(cpp_fpu)%{!ansi:%{m68302:-Dmc68302 }%{m68010:-Dmc68010 }%{m68020:-Dmc68020 }%{mc68020:-Dmc68020 }%{m68030:-Dmc68030 }%{m68040:-Dmc68040 }%{m68020-40:-Dmc68020 -Dmc68030 -Dmc68040 }%{m68020-60:-Dmc68020 -Dmc68030 -Dmc68040 -Dmc68060 }%{m68060:-Dmc68060 }%{mcpu32:-Dmcpu32 } %{m68332:-Dmc68332 -Dmcpu32 }%{m5200:-Dmcf5200 }} \
%{m68302:-D__mc68302__ -D__mc68302 }%{m68010:-D__mc68010__ -D__mc68010 }%{m68020:-D__mc68020__ -D__mc68020 }%{mc68020:-D__mc68020__ -D__mc68020 }%{m68030:-D__mc68030__ -D__mc68030 }%{m68040:-D__mc68040__ -D__mc68040 }%{m68020-40:-D__mc68020__ -D__mc68030__ -D__mc68040__ -D__mc68020 -D__mc68030 -D__mc68040 }%{m68020-60:-D__mc68020__ -D__mc68030__ -D__mc68040__ -D__mc68020 -D__mc68030 -D__mc68040 -D__mc68060__ -D__mc68060 }%{m68060:-D__mc68060__ -D__mc68060 }%{mcpu32:-D__mcpu32__ -D__mcpu32 }%{m68332:-D__mc68332__ -D__mc68332 -D__mcpu32__ -D__mcpu32 }%{m5200:-D__mcf5200__ -D__mcf5200 } \
%{!mc68000:%{!m68000:%{!m68302:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68020-60:%{!m68060:%{!mcpu32: %{!m68332:%{!m5200:%(cpp_cpu_default)}}}}}}}}}}}}}} \
%(cpp_subtarget) \
"

/* Pass flags to gas indicating which type of processor we have.  */

#undef ASM_SPEC
#define ASM_SPEC "\
%{m68851}%{mno-68851}%{m68881}%{mno-68881}%{msoft-float:-mno-68881 }%{m68000}%{m68302}%{mc68000}%{m68010}%{m68020}%{mc68020}%{m68030}%{m68040}%{m68020-40:-mc68040 }%{m68020-60:-mc68040 }%{m68060}%{mcpu32}%{m68332}%{m5200}%{!mc68000:%{!m68000:%{!m68302:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68020-60:%{!m68060:%{!mcpu32:%{!m68332:%{!m5200:%(asm_cpu_default) }}}}}}}}}}}}}} \
"

/* cc1/cc1plus always receives all the -m flags. If the specs strings above 
   are consistent with the TARGET_OPTIONS flags in m68k.h, there should be no
   need for any further cc1/cc1plus specs.  */

#undef CC1_SPEC
#define CC1_SPEC ""

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#define EXTRA_SPECS					\
  { "cpp_cpu_default",	CPP_CPU_DEFAULT_SPEC },		\
  { "cpp_fpu",		CPP_FPU_SPEC },			\
  { "cpp_subtarget",	CPP_SUBTARGET_SPEC },		\
  { "asm_cpu_default",	ASM_CPU_DEFAULT_SPEC },		\
/*{ "cc1_cpu_default",	CC1_CPU_DEFAULT__SPEC },*/	\
  SUBTARGET_EXTRA_SPECS

#define CPP_SUBTARGET_SPEC ""
#define SUBTARGET_EXTRA_SPECS

/* Avoid building multilib libraries for the defaults.
   t-m68kbare doesn't support -mfpa in the multilib'd libraries, so we don't
   either.
   For targets not handled here, just build the full set of multilibs.
   The default is m68k 99.9% of the time anyway.  */

#if TARGET_CPU_DEFAULT == M68K_CPU_m68k || TARGET_CPU_DEFAULT == M68K_CPU_m68020
#if TARGET_DEFAULT & MASK_68881
#define MULTILIB_DEFAULTS { "m68020", "m68881" }
#else
#define MULTILIB_DEFAULTS { "m68020", "msoft-float" }
#endif
#endif

#if TARGET_CPU_DEFAULT == M68K_CPU_m68000 || TARGET_CPU_DEFAULT == M68K_CPU_m68302
#if TARGET_DEFAULT & MASK_68881
#define MULTILIB_DEFAULTS { "m68000", "m68881" }
#else
#define MULTILIB_DEFAULTS { "m68000", "msoft-float" }
#endif
#endif
