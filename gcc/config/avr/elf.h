/* Copyright (C) 2011
   Free Software Foundation, Inc.
   Contributed by Georg-Johann Lay (avr@gjlay.de)

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


/* Overriding some definitions from elfos.h for AVR.  */

#undef PCC_BITFIELD_TYPE_MATTERS

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION avr_asm_named_section

/* Use lame default: no string merging, ...  */
#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION default_select_section

#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT (32768 * 8)

#undef TARGET_HAVE_SWITCHABLE_BSS_SECTIONS

#undef STRING_LIMIT
#define STRING_LIMIT ((unsigned) 64)

/* Setup `readonly_data_section' in `avr_asm_init_sections'.  */
#undef READONLY_DATA_SECTION_ASM_OP

/* Take care of `signal' and `interrupt' attributes.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)     \
  avr_asm_declare_function_name ((FILE), (NAME), (DECL))

#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE, PREFIX, NUM, TABLE) \
  switch_to_section (progmem_section);

/* Be conservative in crtstuff.c.  */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
