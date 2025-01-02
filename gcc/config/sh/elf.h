/* Definitions of target machine for gcc for Renesas / SuperH SH using ELF.
   Copyright (C) 1996-2025 Free Software Foundation, Inc.
   Contributed by Ian Lance Taylor <ian@cygnus.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Let sh.cc know this is ELF.  */
#undef TARGET_ELF
#define TARGET_ELF 1

/* Generate DWARF2 debugging information and make it the default.  */
#define DWARF2_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Use a more compact format for line information.  */
#define DWARF2_ASM_LINE_DEBUG_INFO 1

#undef WCHAR_TYPE
#define WCHAR_TYPE SH_ELF_WCHAR_TYPE

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* The prefix to add to user-visible assembler symbols.  */
#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

#undef SIZE_TYPE
#define SIZE_TYPE ("unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE ("int")

/* Pass -ml and -mrelax to the assembler and linker.  */
#undef ASM_SPEC
#define ASM_SPEC SH_ASM_SPEC

#undef LINK_SPEC
#define LINK_SPEC SH_LINK_SPEC
#undef LINK_EMUL_PREFIX
#if TARGET_ENDIAN_DEFAULT == MASK_LITTLE_ENDIAN
#define LINK_EMUL_PREFIX "sh%{!mb:l}elf"
#else
#define LINK_EMUL_PREFIX "sh%{ml:l}elf"
#endif

#define DEBUGGER_REGNO(REGNO) SH_DEBUGGER_REGNO (REGNO)

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM) \
  sprintf ((STRING), "*%s%s%ld", LOCAL_LABEL_PREFIX, (PREFIX), (long)(NUM))

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: crt1.o%s} crti.o%s \
   %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

#undef LIB_SPEC
#define LIB_SPEC "-lc"

/* ASM_OUTPUT_CASE_LABEL is defined in elfos.h.  With it,
   a redundant .align was generated.  */
#undef  ASM_OUTPUT_CASE_LABEL
