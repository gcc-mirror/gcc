/* Definitions of target machine for GNU compiler.  Vxworks m68k version.
   Copyright (C) 1994, 1996, 1997, 1998 Free Software Foundation, Inc.

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

/* This comment is here to see if it will keep Sun's cpp from dying.  */

#include "m68k/m68k-none.h"
#include "aoutos.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000 -D__vxworks -D__vxworks_5 -Acpu(m68k) -Amachine(m68k)"

/* The default value for -DCPU=.  */
#if TARGET_CPU_DEFAULT == M68K_CPU_m68k || TARGET_CPU_DEFAULT == M68K_CPU_m68020
#define CPP_SUBTARGET_CPU_DEFAULT_SPEC "-DCPU=MC68020"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68000
#define CPP_SUBTARGET_CPU_DEFAULT_SPEC "-DCPU=MC68000"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68030
#define CPP_SUBTARGET_CPU_DEFAULT_SPEC "-DCPU=MC68030"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68040
#define CPP_SUBTARGET_CPU_DEFAULT_SPEC "-DCPU=MC68040"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68302
#define CPP_SUBTARGET_CPU_DEFAULT_SPEC "-DCPU=MC68302"
#else
#if TARGET_CPU_DEFAULT == M68K_CPU_m68332
#define CPP_SUBTARGET_CPU_DEFAULT_SPEC "-DCPU=MC68332"
#else
Unrecognized value in TARGET_CPU_DEFAULT.
#endif
#endif
#endif
#endif
#endif
#endif

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
{ "cpp_subtarget_cpu_default", CPP_SUBTARGET_CPU_DEFAULT_SPEC }

/* Vxworks header files require that the macro CPU be set.  */
/* ??? The previous code didn't set CPU if -ansi.  */
#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{m68000:-DCPU=MC68000 }%{m68010:-DCPU=MC68010 }%{m68020:-DCPU=MC68020 }%{mc68020:-DCPU=MC68020 }%{m68030:-DCPU=MC68030 }%{m68040:-DCPU=MC68040 }%{m68020-40:-DCPU=MC68020 }%{m68302:-DCPU=MC68000 }%{m68332:-DCPU=CPU32 } \
%{!mc68000:%{!m68000:%{!m68010:%{!mc68020:%{!m68020:%{!m68030:%{!m68040:%{!m68020-40:%{!m68302:%{!m68332:%(cpp_subtarget_cpu_default) }}}}}}}}}} \
"

#define DBX_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO

/* These are the official values from WRS.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "char"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 8
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

/* VxWorks does all the library stuff itself.  */

#define LIB_SPEC ""

/* Provide required defaults for linker. */
 
#define LINK_SPEC "-r"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#define STARTFILE_SPEC ""

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* Allow folding division by zero.  */
#define REAL_INFINITY

/* GCC is the primary compiler for VxWorks, so we don't need this.  */
#undef PCC_STATIC_STRUCT_RETURN

/* Restrict use of 128 bit floating-point by default since VxWorks doesn't
   have the proper accuracy routines for that size; this is not done because
   the hardware doesn't support it, despite the name.  */
#define WIDEST_HARDWARE_FP_SIZE 64
