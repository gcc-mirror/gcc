/* Definitions of target machine for GNU compiler, for DEC Alpha
   running Windows/NT.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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

/* Say this is Windows/NT for the other config files.  */
#define WINDOWS_NT 1

#include "alpha/alpha.h"

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-DWIN32 -D_WIN32 -DWINNT -D__STDC__=0 -DALMOST_STDC\
  -D_M_ALPHA -D_ALPHA_ -D__alpha -D__alpha__\
  -D_LONGLONG -D__unaligned= -D__stdcall= \
  -Asystem(winnt) -Acpu(alpha) -Amachine(alpha)"

#undef ASM_SPEC
#undef ASM_FINAL_SPEC
#define ASM_SPEC "-nopp -nologo %{g:-Zi}"

/* Pointer is 32 bits but the hardware has 64-bit addresses, sign extended. */
#undef POINTER_SIZE
#define POINTER_SIZE 32
#define POINTERS_EXTEND_UNSIGNED 0

/* "long" is 32 bits.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

/* We can't do any debugging.  */
#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO

#include "winnt/win-nt.h"

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");                          \
  fprintf (FILE, "\t.set noat\n");				\
  fprintf (FILE, "\t.globl\t__fltused\n");			\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}

#undef LIB_SPEC
#define LIB_SPEC "%{mwindows:-subsystem windows -e _WinMainCRTStartup \
  USER32.LIB%s GDI32.LIB%s COMDLG32.LIB%s WINSPOOL.LIB%s} \
 %{!mwindows:-subsystem console -e _mainCRTStartup} \
 %{mcrtmt:LIBCMT.LIB%s KERNEL32.LIB%s} %{!mcrtmt:LIBC.LIB%s KERNEL32.LIB%s} \
 %{v}"
