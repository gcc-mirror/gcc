/* Definitions of target machine for GNU compiler, for IBM RS/6000.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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

/* Declare functions in rs6000.c */

#ifdef RTX_CODE

#ifdef TREE_CODE
extern void init_cumulative_args PARAMS ((CUMULATIVE_ARGS *, tree, rtx, int));
extern void rs6000_va_start PARAMS ((int, tree, rtx));
#endif /* TREE_CODE */

extern struct rtx_def *rs6000_got_register PARAMS ((rtx));
extern struct rtx_def *find_addr_reg PARAMS ((rtx));
extern int any_operand PARAMS ((rtx, enum machine_mode));
extern int short_cint_operand PARAMS ((rtx, enum machine_mode));
extern int u_short_cint_operand PARAMS ((rtx, enum machine_mode));
extern int non_short_cint_operand PARAMS ((rtx, enum machine_mode));
extern int exact_log2_cint_operand PARAMS ((rtx, enum machine_mode));
extern int gpc_reg_operand PARAMS ((rtx, enum machine_mode));
extern int cc_reg_operand PARAMS ((rtx, enum machine_mode));
extern int cc_reg_not_cr0_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_short_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_neg_short_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_aligned_short_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_u_short_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_cint_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_arith_cint_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_add_cint64_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_sub_cint64_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_logical_cint_operand PARAMS ((rtx, enum machine_mode));
extern int got_operand PARAMS ((rtx, enum machine_mode));
extern int got_no_const_operand PARAMS ((rtx, enum machine_mode));
extern int num_insns_constant PARAMS ((rtx, enum machine_mode));
extern int easy_fp_constant PARAMS ((rtx, enum machine_mode));
extern int zero_fp_constant PARAMS ((rtx, enum machine_mode));
extern int zero_constant PARAMS ((rtx, enum machine_mode));
extern int volatile_mem_operand PARAMS ((rtx, enum machine_mode));
extern int offsettable_mem_operand PARAMS ((rtx, enum machine_mode));
extern int mem_or_easy_const_operand PARAMS ((rtx, enum machine_mode));
extern int add_operand PARAMS ((rtx, enum machine_mode));
extern int non_add_cint_operand PARAMS ((rtx, enum machine_mode));
extern int non_logical_cint_operand PARAMS ((rtx, enum machine_mode));
extern int logical_operand PARAMS ((rtx, enum machine_mode));
extern int mask_operand PARAMS ((rtx, enum machine_mode));
extern int mask64_operand PARAMS ((rtx, enum machine_mode));
extern int and64_operand PARAMS ((rtx, enum machine_mode));
extern int and_operand PARAMS ((rtx, enum machine_mode));
extern int count_register_operand PARAMS ((rtx, enum machine_mode));
extern int xer_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_mem_operand PARAMS ((rtx, enum machine_mode));
extern int lwa_operand PARAMS ((rtx, enum machine_mode));
extern int call_operand PARAMS ((rtx, enum machine_mode));
extern int current_file_function_operand PARAMS ((rtx, enum machine_mode));
extern int input_operand PARAMS ((rtx, enum machine_mode));
extern int small_data_operand PARAMS ((rtx, enum machine_mode));
extern int s8bit_cint_operand PARAMS ((rtx, enum machine_mode));
extern int constant_pool_expr_p PARAMS ((rtx));
extern int toc_relative_expr_p PARAMS ((rtx));
extern int expand_block_move PARAMS ((rtx[]));
extern int load_multiple_operation PARAMS ((rtx, enum machine_mode));
extern const char * rs6000_output_load_multiple PARAMS ((rtx[]));
extern int store_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int branch_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int branch_positive_comparison_operator 
  PARAMS ((rtx, enum machine_mode));
extern int scc_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int trap_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int boolean_operator PARAMS ((rtx, enum machine_mode));
extern int boolean_or_operator PARAMS ((rtx, enum machine_mode));
extern int min_max_operator PARAMS ((rtx, enum machine_mode));
extern int includes_lshift_p PARAMS ((rtx, rtx));
extern int includes_rshift_p PARAMS ((rtx, rtx));
extern int includes_rldic_lshift_p PARAMS ((rtx, rtx));
extern int includes_rldicr_lshift_p PARAMS ((rtx, rtx));
extern int registers_ok_for_quad_peep PARAMS ((rtx, rtx));
extern int addrs_ok_for_quad_peep PARAMS ((rtx, rtx));
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class,
						      enum machine_mode, rtx));
extern int ccr_bit PARAMS ((rtx, int));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern enum rtx_code rs6000_reverse_condition PARAMS ((enum machine_mode,
						       enum rtx_code));
extern void rs6000_emit_sCOND PARAMS ((enum rtx_code, rtx));
extern void rs6000_emit_cbranch PARAMS ((enum rtx_code, rtx));
extern char * output_cbranch PARAMS ((rtx, const char *, int, rtx));
extern rtx rs6000_emit_set_const PARAMS ((rtx, enum machine_mode, rtx, int));
extern int rs6000_emit_cmove PARAMS ((rtx, rtx, rtx, rtx));
extern void rs6000_emit_minmax PARAMS ((rtx, enum rtx_code, rtx, rtx));
extern void output_toc PARAMS ((FILE *, rtx, int, enum machine_mode));
extern void rs6000_initialize_trampoline PARAMS ((rtx, rtx, rtx));
extern struct rtx_def *rs6000_longcall_ref PARAMS ((rtx));
extern void rs6000_fatal_bad_address PARAMS ((rtx));
extern int stmw_operation PARAMS ((rtx, enum machine_mode));
extern int mtcrf_operation PARAMS ((rtx, enum machine_mode));
extern int lmw_operation PARAMS ((rtx, enum machine_mode));
extern struct rtx_def *create_TOC_reference PARAMS ((rtx));
extern void rs6000_emit_eh_toc_restore PARAMS ((rtx));
extern void rs6000_emit_move PARAMS ((rtx, rtx, enum machine_mode));
extern rtx rs6000_legitimize_address PARAMS ((rtx, rtx, enum machine_mode));
extern rtx rs6000_legitimize_reload_address PARAMS ((rtx, enum machine_mode,
			    int, int, int, int *));
extern int rs6000_legitimate_address PARAMS ((enum machine_mode, rtx, int));
extern void rs6000_select_rtx_section PARAMS ((enum machine_mode, rtx));

extern rtx rs6000_return_addr PARAMS ((int, rtx));
extern void rs6000_output_symbol_ref PARAMS ((FILE*, rtx));

extern rtx rs6000_machopic_legitimize_pic_address PARAMS ((rtx orig, enum machine_mode mode, rtx reg));

#endif /* RTX_CODE */

#ifdef TREE_CODE
extern void function_arg_advance PARAMS ((CUMULATIVE_ARGS *, enum machine_mode,
					  tree, int));
extern int function_arg_boundary PARAMS ((enum machine_mode, tree));
extern struct rtx_def *function_arg PARAMS ((CUMULATIVE_ARGS *,
					     enum machine_mode, tree, int));
extern int function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS *,
					       enum machine_mode, tree, int));
extern int function_arg_pass_by_reference PARAMS ((CUMULATIVE_ARGS *,
						   enum machine_mode,
						   tree, int));
extern void setup_incoming_varargs PARAMS ((CUMULATIVE_ARGS *,
					    enum machine_mode, tree,
					    int *, int));
extern struct rtx_def *rs6000_va_arg PARAMS ((tree, tree));
extern void output_mi_thunk PARAMS ((FILE *, tree, int, tree));
extern void rs6000_encode_section_info PARAMS ((tree));
extern void rs6000_select_section PARAMS ((tree, int));
extern void rs6000_unique_section PARAMS ((tree, int));
#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction' */
extern enum direction function_arg_padding PARAMS ((enum machine_mode, tree));
#endif /* ARGS_SIZE_RTX */

#endif /* TREE_CODE */

extern void optimization_options PARAMS ((int, int));
extern void rs6000_override_options PARAMS ((const char *));
extern void rs6000_file_start PARAMS ((FILE *, const char *));
extern struct rtx_def *rs6000_float_const PARAMS ((const char *,
						   enum machine_mode));
extern int direct_return PARAMS ((void));
extern union tree_node *rs6000_build_va_list PARAMS ((void));
extern int first_reg_to_save PARAMS ((void));
extern int first_fp_reg_to_save PARAMS ((void));
extern rs6000_stack_t *rs6000_stack_info PARAMS ((void));
extern void output_ascii PARAMS ((FILE *, const char *, int));
extern void rs6000_gen_section_name PARAMS ((char **, const char *,
					     const char *));
extern void output_function_profiler PARAMS ((FILE *, int));
extern void output_profile_hook  PARAMS ((int));
extern int rs6000_trampoline_size PARAMS ((void));
extern void toc_section PARAMS ((void));
extern void sdata_section PARAMS ((void));
extern void sdata2_section PARAMS ((void));
extern void sbss_section PARAMS ((void));
extern void private_data_section PARAMS ((void));
extern void read_only_data_section PARAMS ((void));
extern void read_only_private_data_section PARAMS ((void));
extern int get_TOC_alias_set PARAMS ((void));
extern int uses_TOC PARAMS ((void));
extern void rs6000_emit_prologue PARAMS ((void));
extern void rs6000_emit_load_toc_table PARAMS ((int));
extern void rs6000_aix_emit_builtin_unwind_init PARAMS ((void));
extern void rs6000_emit_epilogue PARAMS ((int));
extern void debug_stack_info PARAMS ((rs6000_stack_t *));

extern void machopic_output_stub PARAMS ((FILE *, const char *, const char *));
