/* Definitions for Motorola m68k running Linux
   Copyright (C) 1995 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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
#include <linux-aout.h>

/* 68020 with 68881 */
#define TARGET_DEFAULT 7

#define DBX_DEBUGGING_INFO

#define CPP_PREDEFINES \
  "-Dunix -Dmc68000 -Dmc68020 -Dlinux -Asystem(unix) -Asystem(posix) -Acpu(m68k) -Amachine(m68k)"

#undef CPP_SPEC
#if TARGET_DEFAULT & 2
#define CPP_SPEC \
  "%{!msoft-float:-D__HAVE_68881__} %{posix:-D_POSIX_SOURCE}"
#else
#define CPP_SPEC \
  "%{m68881:-D__HAVE_68881__} %{posix:-D_POSIX_SOURCE}"
#endif

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES	{"ieee-fp", 0},

#undef ASM_SPEC
#define ASM_SPEC \
  "%{m68030} %{m68040} %{fpic:-k} %{fPIC:-k}"

#undef LIB_SPEC
#if 1
/* We no longer link with libc_p.a or libg.a by default.  If you want
   to profile or debug the Linux C library, please add -lc_p or -ggdb
   to LDFLAGS at the link time, respectively.  */
#define LIB_SPEC \
  "%{mieee-fp:-lieee} %{p:-lgmon} %{pg:-lgmon} %{!ggdb:-lc} %{ggdb:-lg}"
#else
#define LIB_SPEC \
  "%{mieee-fp:-lieee} %{p:-lgmon -lc_p} %{pg:-lgmon -lc_p} %{!p:%{!pg:%{!g*:-lc} %{g*:-lg}}}"
#endif

/* We want to pass -v to linker */
#undef LINK_SPEC
#define LINK_SPEC	"-m m68klinux %{v:-dll-verbose}"

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* Don't default to pcc-struct-return, because gcc is the only compiler.  */
#undef PCC_STATIC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Allow folding division by zero.  */
#define REAL_INFINITY
