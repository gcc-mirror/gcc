/* Definitions for ARM running Linux
   Copyright (C) 1993, 1994, 1997 Free Software Foundation, Inc.
   Adapted from ARM Linux by Russell King  <rmk92@ecs.soton.ac.uk>.

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <linux-aout.h>

/* these are different... */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
"%{pg:gcrt0.o%s} %{!pg:%{p:gcrt0.o%s} %{!p:crt0.o%s}} %{static:-static}"

#undef ASM_APP_ON
#undef ASM_APP_OFF
#undef COMMENT_BEGIN
 
/* We default to ARM3.  */
#define TARGET_CPU_DEFAULT TARGET_CPU_arm3

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
"-Dunix -Darm -Dlinux -Asystem(unix) -Asystem(posix) -Acpu(arm) -Amachine(arm)"

#undef LIB_SPEC
#define LIB_SPEC \
	"%{mieee-fp:-lieee} %{p:-lgmon} %{pg:-lgmon} %{!ggdb:-lc} %{ggdb:-lg}"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#define HANDLE_SYSV_PRAGMA
  
/* Run-time Target Specification.  */
#define TARGET_VERSION  \
  fputs (" (ARM Linux/a.out)", stderr);

/* This is used in ASM_FILE_START */
#define ARM_OS_NAME "Linux"

/* Unsigned chars produces much better code than signed.  */
#define DEFAULT_SIGNED_CHAR 0

/* Maths operation domain error number, EDOM */
#define TARGET_EDOM 33
#include "arm/aout.h"

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC  "%{posix:-D_POSIX_SOURCE}"
