/* Definition of RISC-V target for GNU compiler.
   Copyright (C) 2011-2017 Free Software Foundation, Inc.
   Contributed by Andrew Waterman (andrew@sifive.com).
   Based on MIPS target for GNU compiler.

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

#ifndef GCC_RISCV_PROTOS_H
#define GCC_RISCV_PROTOS_H

/* Symbol types we understand.  The order of this list must match that of
   the unspec enum in riscv.md, subsequent to UNSPEC_ADDRESS_FIRST.  */
enum riscv_symbol_type {
  SYMBOL_ABSOLUTE,
  SYMBOL_PCREL,
  SYMBOL_GOT_DISP,
  SYMBOL_TLS,
  SYMBOL_TLS_LE,
  SYMBOL_TLS_IE,
  SYMBOL_TLS_GD
};
#define NUM_SYMBOL_TYPES (SYMBOL_TLS_GD + 1)

/* Routines implemented in riscv.c.  */
extern enum riscv_symbol_type riscv_classify_symbolic_expression (rtx);
extern bool riscv_symbolic_constant_p (rtx, enum riscv_symbol_type *);
extern int riscv_regno_mode_ok_for_base_p (int, machine_mode, bool);
extern int riscv_address_insns (rtx, machine_mode, bool);
extern int riscv_const_insns (rtx);
extern int riscv_split_const_insns (rtx);
extern int riscv_load_store_insns (rtx, rtx_insn *);
extern rtx riscv_emit_move (rtx, rtx);
extern bool riscv_split_symbol (rtx, rtx, machine_mode, rtx *);
extern bool riscv_split_symbol_type (enum riscv_symbol_type);
extern rtx riscv_unspec_address (rtx, enum riscv_symbol_type);
extern void riscv_move_integer (rtx, rtx, HOST_WIDE_INT);
extern bool riscv_legitimize_move (machine_mode, rtx, rtx);
extern rtx riscv_subword (rtx, bool);
extern bool riscv_split_64bit_move_p (rtx, rtx);
extern void riscv_split_doubleword_move (rtx, rtx);
extern const char *riscv_output_move (rtx, rtx);
extern const char *riscv_output_gpr_save (unsigned);
#ifdef RTX_CODE
extern void riscv_expand_int_scc (rtx, enum rtx_code, rtx, rtx);
extern void riscv_expand_float_scc (rtx, enum rtx_code, rtx, rtx);
extern void riscv_expand_conditional_branch (rtx, enum rtx_code, rtx, rtx);
#endif
extern rtx riscv_legitimize_call_address (rtx);
extern void riscv_set_return_address (rtx, rtx);
extern bool riscv_expand_block_move (rtx, rtx, rtx);
extern rtx riscv_return_addr (int, rtx);
extern HOST_WIDE_INT riscv_initial_elimination_offset (int, int);
extern void riscv_expand_prologue (void);
extern void riscv_expand_epilogue (bool);
extern bool riscv_can_use_return_insn (void);
extern rtx riscv_function_value (const_tree, const_tree, machine_mode);

/* Routines implemented in riscv-c.c.  */
void riscv_cpu_cpp_builtins (cpp_reader *);

/* Routines implemented in riscv-builtins.c.  */
extern void riscv_atomic_assign_expand_fenv (tree *, tree *, tree *);
extern rtx riscv_expand_builtin (tree, rtx, rtx, machine_mode, int);
extern tree riscv_builtin_decl (unsigned int, bool);
extern void riscv_init_builtins (void);

#endif /* ! GCC_RISCV_PROTOS_H */
