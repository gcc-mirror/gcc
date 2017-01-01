/* Prototypes of target machine for TILEPro.
   Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

#ifndef GCC__TILEPRO_PROTOS_H
#define GCC__TILEPRO_PROTOS_H


extern void tilepro_init_expanders (void);
extern bool tilepro_legitimate_pic_operand_p (rtx);
extern rtx tilepro_simd_int (rtx, machine_mode);

#ifdef RTX_CODE
extern void split_di (rtx[], int, rtx[], rtx[]);
extern bool tilepro_bitfield_operand_p (HOST_WIDE_INT, int *, int *);
extern void tilepro_expand_set_const32 (rtx, rtx);
extern bool tilepro_expand_mov (machine_mode, rtx *);
extern void tilepro_expand_insv (rtx operands[4]);
extern void tilepro_expand_unaligned_load (rtx, rtx, HOST_WIDE_INT,
					   HOST_WIDE_INT, bool);
extern void tilepro_expand_movmisalign (machine_mode, rtx *);
extern bool tilepro_expand_addsi (rtx, rtx, rtx);
extern void tilepro_allocate_stack (rtx, rtx);
extern bool tilepro_expand_mulsi (rtx, rtx, rtx);
extern void tilepro_expand_smulsi3_highpart (rtx, rtx, rtx);
extern void tilepro_expand_umulsi3_highpart (rtx, rtx, rtx);

extern bool tilepro_emit_setcc (rtx[], machine_mode);
extern void tilepro_emit_conditional_branch (rtx[], machine_mode);
extern rtx tilepro_emit_conditional_move (rtx);
extern const char *tilepro_output_cbranch_with_opcode (rtx_insn *, rtx *,
						       const char *,
						       const char *, int,
						       bool);
extern const char *tilepro_output_cbranch (rtx_insn *, rtx *, bool);
extern void tilepro_expand_tablejump (rtx, rtx);
extern void tilepro_expand_builtin_vector_binop (rtx (*)(rtx, rtx, rtx),
						 machine_mode, rtx,
						 machine_mode, rtx, rtx,
						 bool);
#endif /* RTX_CODE */

extern bool tilepro_can_use_return_insn_p (void);
extern void tilepro_expand_prologue (void);
extern void tilepro_expand_epilogue (bool);
extern int tilepro_initial_elimination_offset (int, int);
extern rtx tilepro_return_addr (int, rtx);
extern rtx tilepro_eh_return_handler_rtx (void);
extern int tilepro_adjust_insn_length (rtx_insn *, int);

extern int tilepro_asm_preferred_eh_data_format (int, int);
extern void tilepro_final_prescan_insn (rtx_insn *);
extern const char *tilepro_asm_output_opcode (FILE *, const char *);
extern void tilepro_function_profiler (FILE *, int);

/* Declare functions in tile-c.c */

extern void tilepro_cpu_cpp_builtins (struct cpp_reader *);

#endif /* GCC_TILEPRO_PROTOS_H */
