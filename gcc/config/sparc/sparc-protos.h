/* Prototypes of target machine for GNU compiler, for Sun SPARC.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).
   64 bit SPARC V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
   at Cygnus Support.

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

#ifndef __SPARC_PROTOS_H__
#define __SPARC_PROTOS_H__

extern bool sparc_emitting_epilogue;

#ifdef TREE_CODE
extern struct rtx_def *function_value PARAMS ((tree, enum machine_mode, int));
extern void function_arg_advance PARAMS ((CUMULATIVE_ARGS *,
					  enum machine_mode, tree, int));
extern struct rtx_def *function_arg PARAMS ((const CUMULATIVE_ARGS *,
					     enum machine_mode,
					     tree, int, int));
extern int function_arg_partial_nregs  PARAMS ((const CUMULATIVE_ARGS *,
						enum machine_mode,
						tree, int));
extern int function_arg_pass_by_reference PARAMS ((const CUMULATIVE_ARGS *,
						   enum machine_mode,
						   tree, int));
extern struct rtx_def *sparc_builtin_saveregs PARAMS ((void));
#ifdef RTX_CODE
extern void init_cumulative_args PARAMS ((CUMULATIVE_ARGS *, tree, rtx, int));
extern void sparc_va_start PARAMS ((int, tree, rtx));
#endif
extern struct rtx_def *sparc_va_arg PARAMS ((tree, tree));
extern unsigned long sparc_type_code PARAMS ((tree));
#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction' */
extern enum direction function_arg_padding PARAMS ((enum machine_mode, tree));
#endif /* ARGS_SIZE_RTX */
#endif /* TREE_CODE */

extern void load_pic_register PARAMS ((void));
extern void order_regs_for_local_alloc PARAMS ((void));
extern int compute_frame_size PARAMS ((int, int));
extern int check_pic PARAMS ((int));
extern int short_branch PARAMS ((int, int));
extern int sparc_flat_epilogue_delay_slots PARAMS ((void));
extern unsigned long sparc_flat_compute_frame_size PARAMS ((int));
extern void sparc_profile_hook PARAMS ((int));
extern void sparc_override_options PARAMS ((void));
extern int leaf_return_peephole_ok PARAMS ((void));
extern void sparc_output_scratch_registers PARAMS ((FILE *));
extern void sparc_flat_save_restore PARAMS ((FILE *, const char *,
					     unsigned int, unsigned long,
					     unsigned long, const char *,
					     const char *, unsigned long));

#ifdef RTX_CODE
extern enum machine_mode select_cc_mode PARAMS ((enum rtx_code, rtx, rtx));
/* Define the function that build the compare insn for scc and bcc.  */
extern rtx gen_compare_reg PARAMS ((enum rtx_code code, rtx, rtx));
extern void sparc_emit_float_lib_cmp PARAMS ((rtx, rtx, enum rtx_code));
extern void sparc_emit_floatunsdi PARAMS ((rtx [2]));
extern void emit_tfmode_binop PARAMS ((enum rtx_code, rtx *));
extern void emit_tfmode_unop PARAMS ((enum rtx_code, rtx *));
extern void emit_tfmode_cvt PARAMS ((enum rtx_code, rtx *));
/* This function handles all v9 scc insns */
extern int gen_v9_scc PARAMS ((enum rtx_code, rtx *));
extern void sparc_initialize_trampoline PARAMS ((rtx, rtx, rtx));
extern void sparc64_initialize_trampoline PARAMS ((rtx, rtx, rtx));
extern rtx legitimize_pic_address PARAMS ((rtx, enum machine_mode, rtx));
extern void sparc_defer_case_vector PARAMS ((rtx, rtx, int));
extern void sparc_emit_set_const32 PARAMS ((rtx, rtx));
extern void sparc_emit_set_const64 PARAMS ((rtx, rtx));
extern void sparc_emit_set_symbolic_const64 PARAMS ((rtx, rtx, rtx));
extern int sparc_splitdi_legitimate PARAMS ((rtx, rtx));
extern int sparc_absnegfloat_split_legitimate PARAMS ((rtx, rtx));
extern char *output_cbranch PARAMS ((rtx, rtx, int, int, int, int, rtx));
extern const char *output_sibcall PARAMS ((rtx, rtx));
extern char *output_v9branch PARAMS ((rtx, rtx, int, int, int, int, int,
				      rtx));
extern void emit_v9_brxx_insn PARAMS ((enum rtx_code, rtx, rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern int mems_ok_for_ldd_peep PARAMS ((rtx, rtx, rtx));
extern int arith_double_4096_operand PARAMS ((rtx, enum machine_mode));
extern int arith_4096_operand PARAMS ((rtx, enum machine_mode));
extern int zero_operand PARAMS ((rtx, enum machine_mode));
extern int fp_zero_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int empty_delay_slot PARAMS ((rtx));
extern int eligible_for_epilogue_delay PARAMS ((rtx, int));
extern int eligible_for_return_delay PARAMS ((rtx));
extern int eligible_for_sibcall_delay PARAMS ((rtx));
extern int emit_move_sequence PARAMS ((rtx, enum machine_mode));
extern int fp_sethi_p PARAMS ((rtx));
extern int fp_mov_p PARAMS ((rtx));
extern int fp_high_losum_p PARAMS ((rtx));
extern int mem_min_alignment PARAMS ((rtx, int));
extern int pic_address_needs_scratch PARAMS ((rtx));
extern int reg_unused_after PARAMS ((rtx, rtx));
extern int register_ok_for_ldd PARAMS ((rtx));
extern int registers_ok_for_ldd_peep PARAMS ((rtx, rtx));
extern int sparc_flat_eligible_for_epilogue_delay PARAMS ((rtx, int));
extern int v9_regcmp_p PARAMS ((enum rtx_code));
extern char *sparc_v8plus_shift PARAMS ((rtx *, rtx, const char *));
/* Function used for V8+ code generation.  Returns 1 if the high
   32 bits of REG are 0 before INSN.  */   
extern int sparc_check_64 PARAMS ((rtx, rtx));
extern rtx gen_df_reg PARAMS ((rtx, int));
extern int sparc_extra_constraint_check PARAMS ((rtx, int, int));
#endif /* RTX_CODE */

extern void sparc_output_mi_thunk PARAMS ((FILE *, tree, HOST_WIDE_INT, tree));

#endif /* __SPARC_PROTOS_H__ */
