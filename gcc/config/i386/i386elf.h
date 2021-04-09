/* Target definitions for GCC for Intel 80386 using ELF
   Copyright (C) 1988-2021 Free Software Foundation, Inc.

   Derived from sysv4.h written by Ron Guilmette (rfg@netcom.com).

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

/* Define DEFAULT_PCC_STRUCT_RETURN to 1 because the i386 SVR4 ABI returns
   records and unions in memory. ix86_option_override_internal will overide
   this flag when compiling 64-bit code as we never do pcc_struct_return
   scheme on x86-64.  */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1

#undef CPP_SPEC
#define CPP_SPEC ""

#define ENDFILE_SPEC "crtend.o%s"

#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}}\
			crtbegin.o%s"

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
  (TARGET_64BIT ? dbx64_register_map[n] : svr4_dbx_register_map[n])

#define LOCAL_LABEL_PREFIX	"."

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section

#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP "\t.section\t.bss"

#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)
