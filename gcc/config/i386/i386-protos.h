/* Definitions of target machine for GNU compiler for IA-32.
   Copyright (C) 1988, 92, 94-99, 2000 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA. */

/* Functions in i386.c */
extern void override_options PROTO((void));
extern void order_regs_for_local_alloc PROTO((void));
extern void optimization_options PROTO((int, int));

extern int ix86_can_use_return_insn_p PROTO((void));

extern void asm_output_function_prefix PROTO((FILE *, char *));
extern void load_pic_register PROTO((void));
extern HOST_WIDE_INT ix86_compute_frame_size PROTO((HOST_WIDE_INT, int *));
extern void ix86_expand_prologue PROTO((void));
extern void ix86_expand_epilogue PROTO((void));

extern void ix86_output_function_block_profiler PROTO((FILE *, int));
extern void ix86_output_block_profiler PROTO((FILE *, int));

#ifdef RTX_CODE
extern int ix86_aligned_p PROTO((rtx));

extern int standard_80387_constant_p PROTO((rtx));
extern int symbolic_reference_mentioned_p PROTO((rtx));

extern int symbolic_operand PROTO((rtx, enum machine_mode));
extern int pic_symbolic_operand PROTO((rtx, enum machine_mode));
extern int call_insn_operand PROTO((rtx, enum machine_mode));
extern int expander_call_insn_operand PROTO((rtx, enum machine_mode));
extern int constant_call_address_operand PROTO((rtx, enum machine_mode));
extern int const0_operand PROTO((rtx, enum machine_mode));
extern int const1_operand PROTO((rtx, enum machine_mode));
extern int const248_operand PROTO((rtx, enum machine_mode));
extern int incdec_operand PROTO((rtx, enum machine_mode));
extern int reg_no_sp_operand PROTO((rtx, enum machine_mode));
extern int q_regs_operand PROTO((rtx, enum machine_mode));
extern int non_q_regs_operand PROTO((rtx, enum machine_mode));
extern int no_comparison_operator PROTO((rtx, enum machine_mode));
extern int fcmov_comparison_operator PROTO((rtx, enum machine_mode));
extern int cmp_fp_expander_operand PROTO((rtx, enum machine_mode));
extern int ext_register_operand PROTO((rtx, enum machine_mode));
extern int binary_fp_operator PROTO((rtx, enum machine_mode));
extern int mult_operator PROTO((rtx, enum machine_mode));
extern int div_operator PROTO((rtx, enum machine_mode));
extern int arith_or_logical_operator PROTO((rtx, enum machine_mode));
extern int promotable_binary_operator PROTO((rtx, enum machine_mode));
extern int memory_displacement_operand PROTO((rtx, enum machine_mode));
extern int cmpsi_operand PROTO((rtx, enum machine_mode));
extern int long_memory_operand PROTO((rtx, enum machine_mode));


extern int legitimate_pic_address_disp_p PROTO((rtx));
extern int legitimate_address_p PROTO((enum machine_mode, rtx, int));
extern rtx legitimize_pic_address PROTO((rtx, rtx));
extern rtx legitimize_address PROTO((rtx, rtx, enum machine_mode));

extern void print_reg PROTO((rtx, int, FILE*));
extern void print_operand PROTO((FILE*, rtx, int));
extern void print_operand_address PROTO((FILE*, rtx));

extern void split_di PROTO((rtx[], int, rtx[], rtx[]));

extern const char *output_387_binary_op PROTO((rtx, rtx*));
extern const char *output_fix_trunc PROTO((rtx, rtx*));
extern const char *output_fp_compare PROTO((rtx, rtx*, int, int));

extern void ix86_expand_move PROTO((enum machine_mode, rtx[]));
extern void ix86_expand_binary_operator PROTO((enum rtx_code,
					       enum machine_mode, rtx[]));
extern int ix86_binary_operator_ok PROTO((enum rtx_code, enum machine_mode,
					  rtx[]));
extern void ix86_expand_unary_operator PROTO((enum rtx_code, enum machine_mode,
					      rtx[]));
extern int ix86_unary_operator_ok PROTO((enum rtx_code, enum machine_mode,
					 rtx[]));
extern void ix86_expand_branch PROTO((enum rtx_code, int, rtx));
extern int ix86_expand_setcc PROTO((enum rtx_code, int, rtx));
extern int ix86_expand_int_movcc PROTO((rtx[]));
extern int ix86_expand_fp_movcc PROTO((rtx[]));
extern int ix86_split_long_move PROTO((rtx[]));
extern void ix86_split_ashldi PROTO((rtx *, rtx));
extern void ix86_split_ashrdi PROTO((rtx *, rtx));
extern void ix86_split_lshrdi PROTO((rtx *, rtx));
extern void ix86_expand_strlensi_unroll_1 PROTO((rtx, rtx, rtx));

extern rtx assign_386_stack_local PROTO((enum machine_mode, int));
extern int ix86_attr_length_default PROTO((rtx));

extern int ix86_issue_rate PROTO((void));
extern int ix86_adjust_cost PROTO((rtx, rtx, rtx, int));
extern void ix86_sched_init PROTO((FILE *, int));
extern int ix86_sched_reorder PROTO((FILE *, int, rtx *, int, int));
extern int ix86_variable_issue PROTO((FILE *, int, rtx, int));

#ifdef TREE_CODE
extern void init_cumulative_args PROTO((CUMULATIVE_ARGS *, tree, rtx));
extern rtx function_arg PROTO((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern void function_arg_advance PROTO((CUMULATIVE_ARGS *, enum machine_mode,
					tree, int));
#endif

#endif

#ifdef TREE_CODE
extern int ix86_valid_decl_attribute_p PROTO((tree, tree, tree, tree));
extern int ix86_valid_type_attribute_p PROTO((tree, tree, tree, tree));
extern int ix86_comp_type_attributes PROTO((tree, tree));
extern int ix86_return_pops_args PROTO((tree, tree, int));
#endif


