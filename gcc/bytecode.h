/* Bytecode definitions for GNU C-compiler.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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


extern int output_bytecode;
extern int stack_depth;
extern int max_stack_depth;

/* Emit DI constant according to target machine word ordering */

#define bc_emit_bytecode_DI_const(CST) 				\
{ int opcode;							\
  opcode = (WORDS_BIG_ENDIAN					\
	    ? TREE_INT_CST_HIGH (CST) 				\
	    : TREE_INT_CST_LOW (CST));				\
  bc_emit_bytecode_const ((char *) &opcode, sizeof opcode); 	\
  opcode = (WORDS_BIG_ENDIAN					\
	    ? TREE_INT_CST_LOW (CST) 				\
	    : TREE_INT_CST_HIGH (CST));				\
  bc_emit_bytecode_const ((char *) &opcode, sizeof opcode);	\
}

extern void bc_expand_expr ();
extern void bc_output_data_constructor ();
extern void bc_store_field ();
extern void bc_load_bit_field ();
extern void bc_store_bit_field ();
extern void bc_push_offset_and_size ();
extern void bc_init_mode_to_code_map ();

/* These are just stubs, so the compiler will compile for targets
   that aren't yet supported by the bytecode generator. */

#ifndef TARGET_SUPPORTS_BYTECODE

#define MACHINE_SEG_ALIGN 1
#define INT_ALIGN 1
#define PTR_ALIGN 1
#define NAMES_HAVE_UNDERSCORES
#define BC_NOP   (0)
#define BC_GLOBALIZE_LABEL(FP, NAME) BC_NOP
#define BC_OUTPUT_COMMON(FP, NAME, SIZE, ROUNDED) BC_NOP
#define BC_OUTPUT_LOCAL(FP, NAME, SIZE, ROUNDED)  BC_NOP
#define BC_OUTPUT_ALIGN(FP, ALIGN)   BC_NOP
#define BC_OUTPUT_LABEL(FP, NAME)    BC_NOP
#define BC_OUTPUT_SKIP(FP, SIZE)     BC_NOP
#define BC_OUTPUT_LABELREF(FP, NAME) BC_NOP
#define BC_OUTPUT_FLOAT(FP, VAL)     BC_NOP
#define BC_OUTPUT_DOUBLE(FP, VAL)    BC_NOP
#define BC_OUTPUT_BYTE(FP, VAL)      BC_NOP
#define BC_OUTPUT_FILE ASM_OUTPUT_FILE
#define BC_OUTPUT_ASCII ASM_OUTPUT_ASCII
#define BC_OUTPUT_IDENT ASM_OUTPUT_IDENT
#define BCXSTR(RTX)  ((RTX)->bc_label)
#define BC_WRITE_FILE(FP)            BC_NOP
#define BC_WRITE_SEGSYM(SEGSYM, FP)  BC_NOP
#define BC_WRITE_RELOC_ENTRY(SEGRELOC, FP, OFFSET) BC_NOP
#define BC_START_BYTECODE_LINE(FP)   BC_NOP
#define BC_WRITE_BYTECODE(SEP, VAL, FP) BC_NOP
#define BC_WRITE_RTL(R, FP)          BC_NOP
#define BC_EMIT_TRAMPOLINE(TRAMPSEG, CALLINFO) BC_NOP
#define VALIDATE_STACK               BC_NOP

#endif /* !TARGET_SUPPORTS_BYTECODE */
