/* Definitions of target machine for GNU compiler for IA-32.
   Copyright (C) 1988, 1992, 1994, 1995, 1996, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.

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
extern void override_options PARAMS ((void));
extern void order_regs_for_local_alloc PARAMS ((void));
extern void optimization_options PARAMS ((int, int));

extern int ix86_can_use_return_insn_p PARAMS ((void));

extern void asm_output_function_prefix PARAMS ((FILE *, const char *));
extern void load_pic_register PARAMS ((void));
extern HOST_WIDE_INT ix86_initial_elimination_offset PARAMS((int, int));
extern void ix86_expand_prologue PARAMS ((void));
extern void ix86_expand_epilogue PARAMS ((int));

extern void ix86_output_function_block_profiler PARAMS ((FILE *, int));
extern void ix86_output_block_profiler PARAMS ((FILE *, int));

#ifdef RTX_CODE
extern int ix86_aligned_p PARAMS ((rtx));

extern int standard_80387_constant_p PARAMS ((rtx));
extern int symbolic_reference_mentioned_p PARAMS ((rtx));

extern int symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int pic_symbolic_operand PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int constant_call_address_operand PARAMS ((rtx, enum machine_mode));
extern int const0_operand PARAMS ((rtx, enum machine_mode));
extern int const1_operand PARAMS ((rtx, enum machine_mode));
extern int const248_operand PARAMS ((rtx, enum machine_mode));
extern int incdec_operand PARAMS ((rtx, enum machine_mode));
extern int reg_no_sp_operand PARAMS ((rtx, enum machine_mode));
extern int general_no_elim_operand PARAMS ((rtx, enum machine_mode));
extern int nonmemory_no_elim_operand PARAMS ((rtx, enum machine_mode));
extern int q_regs_operand PARAMS ((rtx, enum machine_mode));
extern int non_q_regs_operand PARAMS ((rtx, enum machine_mode));
extern int no_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int fcmov_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int uno_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int cmp_fp_expander_operand PARAMS ((rtx, enum machine_mode));
extern int ext_register_operand PARAMS ((rtx, enum machine_mode));
extern int binary_fp_operator PARAMS ((rtx, enum machine_mode));
extern int mult_operator PARAMS ((rtx, enum machine_mode));
extern int div_operator PARAMS ((rtx, enum machine_mode));
extern int arith_or_logical_operator PARAMS ((rtx, enum machine_mode));
extern int promotable_binary_operator PARAMS ((rtx, enum machine_mode));
extern int memory_displacement_operand PARAMS ((rtx, enum machine_mode));
extern int cmpsi_operand PARAMS ((rtx, enum machine_mode));
extern int long_memory_operand PARAMS ((rtx, enum machine_mode));
extern int aligned_operand PARAMS ((rtx, enum machine_mode));


extern int legitimate_pic_address_disp_p PARAMS ((rtx));
extern int legitimate_address_p PARAMS ((enum machine_mode, rtx, int));
extern rtx legitimize_pic_address PARAMS ((rtx, rtx));
extern rtx legitimize_address PARAMS ((rtx, rtx, enum machine_mode));

extern void print_reg PARAMS ((rtx, int, FILE*));
extern void print_operand PARAMS ((FILE*, rtx, int));
extern void print_operand_address PARAMS ((FILE*, rtx));

extern void split_di PARAMS ((rtx[], int, rtx[], rtx[]));

extern const char *output_387_binary_op PARAMS ((rtx, rtx*));
extern const char *output_fix_trunc PARAMS ((rtx, rtx*));
extern const char *output_fp_compare PARAMS ((rtx, rtx*, int, int));

extern void i386_dwarf_output_addr_const PARAMS ((FILE*, rtx));
extern rtx i386_simplify_dwarf_addr PARAMS ((rtx));

extern void ix86_expand_move PARAMS ((enum machine_mode, rtx[]));
extern void ix86_expand_binary_operator PARAMS ((enum rtx_code,
					       enum machine_mode, rtx[]));
extern int ix86_binary_operator_ok PARAMS ((enum rtx_code, enum machine_mode,
					  rtx[]));
extern void ix86_expand_unary_operator PARAMS ((enum rtx_code, enum machine_mode,
					      rtx[]));
extern int ix86_unary_operator_ok PARAMS ((enum rtx_code, enum machine_mode,
					 rtx[]));
extern int ix86_match_ccmode PARAMS ((rtx, enum machine_mode));
extern rtx ix86_expand_fp_compare PARAMS ((enum rtx_code, rtx, rtx, rtx));
extern void ix86_expand_branch PARAMS ((enum rtx_code, rtx));
extern int ix86_expand_setcc PARAMS ((enum rtx_code, rtx));
extern int ix86_expand_int_movcc PARAMS ((rtx[]));
extern int ix86_expand_fp_movcc PARAMS ((rtx[]));
extern int ix86_split_long_move PARAMS ((rtx[]));
extern void ix86_split_ashldi PARAMS ((rtx *, rtx));
extern void ix86_split_ashrdi PARAMS ((rtx *, rtx));
extern void ix86_split_lshrdi PARAMS ((rtx *, rtx));
extern void ix86_expand_strlensi_unroll_1 PARAMS ((rtx, rtx, rtx));
extern int ix86_address_cost PARAMS ((rtx));

extern rtx assign_386_stack_local PARAMS ((enum machine_mode, int));
extern int ix86_attr_length_default PARAMS ((rtx));

extern int ix86_issue_rate PARAMS ((void));
extern int ix86_adjust_cost PARAMS ((rtx, rtx, rtx, int));
extern void ix86_sched_init PARAMS ((FILE *, int));
extern int ix86_sched_reorder PARAMS ((FILE *, int, rtx *, int, int));
extern int ix86_variable_issue PARAMS ((FILE *, int, rtx, int));

#ifdef TREE_CODE
extern void init_cumulative_args PARAMS ((CUMULATIVE_ARGS *, tree, rtx));
extern rtx function_arg PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern void function_arg_advance PARAMS ((CUMULATIVE_ARGS *, enum machine_mode,
					tree, int));
#endif

#endif

#ifdef TREE_CODE
extern int ix86_valid_decl_attribute_p PARAMS ((tree, tree, tree, tree));
extern int ix86_valid_type_attribute_p PARAMS ((tree, tree, tree, tree));
extern int ix86_comp_type_attributes PARAMS ((tree, tree));
extern int ix86_return_pops_args PARAMS ((tree, tree, int));
#endif


