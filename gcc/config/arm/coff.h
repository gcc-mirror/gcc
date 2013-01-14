/* Definitions of target machine for GNU compiler.
   For ARM with COFF object format.
   Copyright (C) 1995-2013 Free Software Foundation, Inc.
   Contributed by Doug Evans (devans@cygnus.com).
   
   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Note - it is important that this definition matches the one in tcoff.h.  */
#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"


/* Run-time Target Specification.  */
#undef  TARGET_DEFAULT_FLOAT_ABI
#define TARGET_DEFAULT_FLOAT_ABI ARM_FLOAT_ABI_SOFT

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_APCS_FRAME)

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
  { "marm", "mlittle-endian", "mfloat-abi=soft", "mno-thumb-interwork" }
#endif

/* This is COFF, but prefer stabs.  */
#define SDB_DEBUGGING_INFO 1

#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG


#define TARGET_ASM_FILE_START_APP_OFF true

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  default_coff_asm_named_section

/* Support the ctors/dtors and other sections.  */

#undef INIT_SECTION_ASM_OP

/* Define this macro if jump tables (for `tablejump' insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.  */
/* We put ARM and Thumb-2 jump tables in the text section, because it makes
   the code more efficient, but for Thumb-1 it's better to put them out of
   band unless we are generating compressed tables.  */
#define JUMP_TABLES_IN_TEXT_SECTION					\
   (TARGET_32BIT || (TARGET_THUMB && (optimize_size || flag_pic)))

#undef  READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP	"\t.section .rdata"
#undef  CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section .ctors,\"x\""
#undef  DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	"\t.section .dtors,\"x\""

/* Support the ctors/dtors sections for g++.  */

/* __CTOR_LIST__ and __DTOR_LIST__ must be defined by the linker script.  */
#define CTOR_LISTS_DEFINED_EXTERNALLY

#undef DO_GLOBAL_CTORS_BODY
#undef DO_GLOBAL_DTORS_BODY

/* The ARM development system defines __main.  */
#define NAME__MAIN  "__gccmain"
#define SYMBOL__MAIN __gccmain

#define SUPPORTS_INIT_PRIORITY 0
