/* Prototype declarations for the C-SKY back end.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

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

#ifndef GCC_CSKY_PROTOS_H
#define GCC_CSKY_PROTOS_H

extern bool csky_simple_addr_operand_p (rtx);
extern bool csky_symbolic_address_p (rtx);
extern bool csky_legitimate_pic_operand_p (rtx);

extern void csky_cpu_cpp_builtins (cpp_reader *);

extern bool csky_inlinable_constant (HOST_WIDE_INT value);
extern bool csky_shifted_imm8_constant (unsigned HOST_WIDE_INT,
					unsigned int *, unsigned int *);
extern bool csky_valid_mem_constraint_operand (rtx, const char*);

extern bool csky_minipool_load_p (rtx_insn *);
extern const char *csky_output_move (rtx insn, rtx *, machine_mode);
extern const char *csky_output_movedouble (rtx *, machine_mode);
extern const char *csky_output_ck801_move (rtx, rtx *, machine_mode);
extern const char *csky_output_ck801_movedouble (rtx *, machine_mode);
extern char *csky_output_call (rtx *, int);
extern const char *csky_output_casesi (rtx *);

extern bool csky_split_and (rtx *);
extern bool csky_split_ior (rtx *);
extern bool csky_split_xor (rtx *);

#ifdef RTX_CODE
extern bool csky_emit_compare (enum rtx_code, rtx, rtx);
extern bool csky_emit_compare_float (enum rtx_code, rtx, rtx);
#endif /* RTX_CODE */

extern rtx csky_return_addr (int, rtx);
extern void csky_init_expanders (void);
extern HOST_WIDE_INT csky_initial_elimination_offset (int, int);
extern void csky_expand_prologue (void);
extern void csky_expand_epilogue (void);
extern const char *csky_output_return_instruction (void);
extern void csky_set_eh_return_address (rtx, rtx);

extern bool csky_symbol_mentioned_p (rtx);
extern bool csky_label_mentioned_p (rtx);
extern rtx csky_legitimize_pic_address (rtx, rtx, bool);

extern bool csky_tls_referenced_p (rtx);
extern rtx csky_legitimize_tls_address (rtx, rtx);

extern int csky_compute_pushpop_length (rtx *);

extern int csky_default_branch_cost (bool, bool);
extern bool csky_default_logical_op_non_short_circuit (void);

extern void csky_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree);
extern int csky_get_movedouble_length(rtx operands[]);

/* The functions was used for fpuv3.  */
extern const char *fpuv3_output_move (rtx *operands);
extern int fpuv3_const_double_rtx (rtx);
#endif /* GCC_CSKY_PROTOS_H */
