/* Target definitions for GCC for Intel 80386 running System V.4
   Copyright (C) 1991-2016 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@netcom.com).

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

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true
#undef X86_FILE_START_VERSION_DIRECTIVE
#define X86_FILE_START_VERSION_DIRECTIVE true

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n)  svr4_dbx_register_map[n]

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* Handle special EH pointer encodings.  Absolute, pc-relative, and
   indirect are handled automatically.  */
#define ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX(FILE, ENCODING, SIZE, ADDR, DONE) \
  do {									\
    if ((SIZE) == 4 && ((ENCODING) & 0x70) == DW_EH_PE_datarel)		\
      {									\
	fputs (ASM_LONG, (FILE));					\
	assemble_name (FILE, XSTR (ADDR, 0));				\
	fputs (((ENCODING) & DW_EH_PE_indirect ? "@GOT" : "@GOTOFF"), (FILE)); \
	goto DONE;							\
      }									\
  } while (0)
