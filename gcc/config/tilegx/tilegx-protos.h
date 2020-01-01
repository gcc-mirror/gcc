/* Prototypes of target machine for TILE-Gx.
   Copyright (C) 2011-2020 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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

#ifndef GCC_TILEGX_PROTOS_H
#define GCC_TILEGX_PROTOS_H

extern void tilegx_init_expanders (void);
extern void tilegx_compute_pcrel_address (rtx, rtx);
extern void tilegx_compute_pcrel_plt_address (rtx, rtx);
extern bool tilegx_legitimate_pic_operand_p (rtx);
extern rtx tilegx_simd_int (rtx, machine_mode);

#ifdef RTX_CODE
extern bool tilegx_bitfield_operand_p (HOST_WIDE_INT, int *, int *);
extern void tilegx_expand_set_const64 (rtx, rtx);
extern bool tilegx_expand_mov (machine_mode, rtx *);
extern void tilegx_expand_unaligned_load (rtx, rtx, HOST_WIDE_INT,
					  HOST_WIDE_INT, bool);
extern void tilegx_expand_movmisalign (machine_mode, rtx *);
extern void tilegx_allocate_stack (rtx, rtx);
extern bool tilegx_expand_muldi (rtx, rtx, rtx);
extern void tilegx_expand_smuldi3_highpart (rtx, rtx, rtx);
extern void tilegx_expand_umuldi3_highpart (rtx, rtx, rtx);

extern bool tilegx_emit_setcc (rtx[], machine_mode);
extern void tilegx_emit_conditional_branch (rtx[], machine_mode);
extern rtx tilegx_emit_conditional_move (rtx);
extern const char *tilegx_output_cbranch_with_opcode (rtx_insn *, rtx *,
						      const char *,
						      const char *, int);
extern const char *tilegx_output_cbranch (rtx_insn *, rtx *, bool);
extern void tilegx_expand_tablejump (rtx, rtx);
extern void tilegx_expand_builtin_vector_binop (rtx (*)(rtx, rtx, rtx),
						machine_mode, rtx,
						machine_mode, rtx, rtx,
						bool);
extern void tilegx_pre_atomic_barrier (enum memmodel);
extern void tilegx_post_atomic_barrier (enum memmodel);
#endif /* RTX_CODE */

extern bool tilegx_can_use_return_insn_p (void);
extern void tilegx_expand_prologue (void);
extern void tilegx_expand_epilogue (bool);
extern int tilegx_initial_elimination_offset (int, int);
extern rtx tilegx_return_addr (int, rtx);
extern rtx tilegx_eh_return_handler_rtx (void);
extern int tilegx_adjust_insn_length (rtx_insn *, int);

extern int tilegx_asm_preferred_eh_data_format (int, int);
extern void tilegx_final_prescan_insn (rtx_insn *);
extern const char *tilegx_asm_output_opcode (FILE *, const char *);
extern void tilegx_function_profiler (FILE *, int);

/* Declare functions in tilegx-c.c */

extern void tilegx_cpu_cpp_builtins (struct cpp_reader *);

#endif /* GCC_TILEGX_PROTOS_H */
