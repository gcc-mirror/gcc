/* OS independent definitions for AMD x86-64.
   Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by Bo Thorsen <bo@suse.de>.

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

#undef ASM_COMMENT_START
#define ASM_COMMENT_START "#"

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
  (TARGET_64BIT ? dbx64_register_map[n] : svr4_dbx_register_map[n])

/* Output assembler code to FILE to call the profiler.  */
#define NO_PROFILE_COUNTERS

#undef MCOUNT_NAME
#define MCOUNT_NAME "mcount"

#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_64BIT ? "long unsigned int" : "unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_64BIT ? "long int" : "int")

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu) %{profile:-p}"

#undef ASM_SPEC
#define ASM_SPEC "%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} \
 %{Wa,*:%*} %{m32:--32}"

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* A C statement to output to the stdio stream FILE an assembler
   command to advance the location counter to a multiple of 1<<LOG
   bytes if it is within MAX_SKIP bytes.

   This is used to align code labels according to Intel recommendations.  */

#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE,LOG,MAX_SKIP)			\
  do {									\
    if ((LOG) != 0) {							\
      if ((MAX_SKIP) == 0) fprintf ((FILE), "\t.p2align %d\n", (LOG));	\
      else fprintf ((FILE), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP));	\
    }									\
  } while (0)


/* i386 System V Release 4 uses DWARF debugging info.
   x86-64 ABI specifies DWARF2.  */

#undef DWARF_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO 1
#define DWARF2_UNWIND_INFO 1
/* Incorrectly autodetected in cross compilation.  */
#undef HAVE_AS_DWARF2_DEBUG_LINE
#define HAVE_AS_DWARF2_DEBUG_LINE

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
