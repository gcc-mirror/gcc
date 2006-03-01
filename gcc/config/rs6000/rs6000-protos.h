/* Definitions of target machine for GNU compiler, for IBM RS/6000.
   Copyright (C) 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the
   Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.  */

#ifndef GCC_RS6000_PROTOS_H
#define GCC_RS6000_PROTOS_H

/* Declare functions in rs6000.c */

#ifdef RTX_CODE

#ifdef TREE_CODE
extern void init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, int, int, int);
extern void rs6000_va_start (tree, rtx);
#endif /* TREE_CODE */

extern struct rtx_def *rs6000_got_register (rtx);
extern struct rtx_def *find_addr_reg (rtx);
extern int any_operand (rtx, enum machine_mode);
extern int short_cint_operand (rtx, enum machine_mode);
extern int u_short_cint_operand (rtx, enum machine_mode);
extern int non_short_cint_operand (rtx, enum machine_mode);
extern int exact_log2_cint_operand (rtx, enum machine_mode);
extern int gpc_reg_operand (rtx, enum machine_mode);
extern int cc_reg_operand (rtx, enum machine_mode);
extern int cc_reg_not_cr0_operand (rtx, enum machine_mode);
extern int reg_or_short_operand (rtx, enum machine_mode);
extern int reg_or_neg_short_operand (rtx, enum machine_mode);
extern int reg_or_aligned_short_operand (rtx, enum machine_mode);
extern int reg_or_u_short_operand (rtx, enum machine_mode);
extern int reg_or_cint_operand (rtx, enum machine_mode);
extern int reg_or_arith_cint_operand (rtx, enum machine_mode);
extern int reg_or_add_cint64_operand (rtx, enum machine_mode);
extern int reg_or_sub_cint64_operand (rtx, enum machine_mode);
extern int reg_or_logical_cint_operand (rtx, enum machine_mode);
extern int got_operand (rtx, enum machine_mode);
extern int word_offset_memref_operand (rtx, enum machine_mode);
extern int got_no_const_operand (rtx, enum machine_mode);
extern int num_insns_constant (rtx, enum machine_mode);
extern int easy_fp_constant (rtx, enum machine_mode);
extern int easy_vector_constant (rtx, enum machine_mode);
extern rtx gen_easy_vector_constant_add_self (rtx);
extern const char *output_vec_const_move (rtx *);
extern int zero_fp_constant (rtx, enum machine_mode);
extern int zero_constant (rtx, enum machine_mode);
extern int volatile_mem_operand (rtx, enum machine_mode);
extern int offsettable_mem_operand (rtx, enum machine_mode);
extern int mem_or_easy_const_operand (rtx, enum machine_mode);
extern int add_operand (rtx, enum machine_mode);
extern int non_add_cint_operand (rtx, enum machine_mode);
extern int non_logical_cint_operand (rtx, enum machine_mode);
extern int logical_operand (rtx, enum machine_mode);
extern int mask_operand (rtx, enum machine_mode);
extern int mask_operand_wrap (rtx, enum machine_mode);
extern int mask64_operand (rtx, enum machine_mode);
extern int mask64_2_operand (rtx, enum machine_mode);
extern void build_mask64_2_operands (rtx, rtx *);
extern int and64_operand (rtx, enum machine_mode);
extern int and64_2_operand (rtx, enum machine_mode);
extern int and_operand (rtx, enum machine_mode);
extern int count_register_operand (rtx, enum machine_mode);
extern int xer_operand (rtx, enum machine_mode);
extern int reg_or_mem_operand (rtx, enum machine_mode);
extern int lwa_operand (rtx, enum machine_mode);
extern int call_operand (rtx, enum machine_mode);
extern int current_file_function_operand (rtx, enum machine_mode);
extern int input_operand (rtx, enum machine_mode);
extern int small_data_operand (rtx, enum machine_mode);
extern int s8bit_cint_operand (rtx, enum machine_mode);
extern bool legitimate_constant_pool_address_p (rtx);
extern int expand_block_move (rtx[]);
extern int load_multiple_operation (rtx, enum machine_mode);
extern const char * rs6000_output_load_multiple (rtx[]);
extern int store_multiple_operation (rtx, enum machine_mode);
extern int branch_comparison_operator (rtx, enum machine_mode);
extern int branch_positive_comparison_operator (rtx, enum machine_mode);
extern int scc_comparison_operator (rtx, enum machine_mode);
extern int trap_comparison_operator (rtx, enum machine_mode);
extern int boolean_operator (rtx, enum machine_mode);
extern int boolean_or_operator (rtx, enum machine_mode);
extern int min_max_operator (rtx, enum machine_mode);
extern int includes_lshift_p (rtx, rtx);
extern int includes_rshift_p (rtx, rtx);
extern int includes_rldic_lshift_p (rtx, rtx);
extern int includes_rldicr_lshift_p (rtx, rtx);
extern int registers_ok_for_quad_peep (rtx, rtx);
extern int addrs_ok_for_quad_peep (rtx, rtx);
extern bool gpr_or_gpr_p (rtx, rtx);
extern enum reg_class secondary_reload_class (enum reg_class,
					      enum machine_mode, rtx);
extern int ccr_bit (rtx, int);
extern int extract_MB (rtx);
extern int extract_ME (rtx);
extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern enum rtx_code rs6000_reverse_condition (enum machine_mode,
					       enum rtx_code);
extern void rs6000_emit_sCOND (enum rtx_code, rtx);
extern void rs6000_emit_cbranch (enum rtx_code, rtx);
extern char * output_cbranch (rtx, const char *, int, rtx);
extern char * output_e500_flip_eq_bit (rtx, rtx);
extern rtx rs6000_emit_set_const (rtx, enum machine_mode, rtx, int);
extern int rs6000_emit_cmove (rtx, rtx, rtx, rtx);
extern void rs6000_emit_minmax (rtx, enum rtx_code, rtx, rtx);
extern void output_toc (FILE *, rtx, int, enum machine_mode);
extern void rs6000_initialize_trampoline (rtx, rtx, rtx);
extern struct rtx_def *rs6000_longcall_ref (rtx);
extern void rs6000_fatal_bad_address (rtx);
extern int stmw_operation (rtx, enum machine_mode);
extern int mfcr_operation (rtx, enum machine_mode);
extern int mtcrf_operation (rtx, enum machine_mode);
extern int lmw_operation (rtx, enum machine_mode);
extern struct rtx_def *create_TOC_reference (rtx);
extern void rs6000_split_multireg_move (rtx, rtx);
extern void rs6000_emit_move (rtx, rtx, enum machine_mode);
extern rtx rs6000_legitimize_address (rtx, rtx, enum machine_mode);
extern rtx rs6000_legitimize_reload_address (rtx, enum machine_mode,
			    int, int, int, int *);
extern int rs6000_legitimate_address (enum machine_mode, rtx, int);
extern bool rs6000_mode_dependent_address (rtx);
extern rtx rs6000_return_addr (int, rtx);
extern void rs6000_output_symbol_ref (FILE*, rtx);
extern HOST_WIDE_INT rs6000_initial_elimination_offset (int, int);

extern rtx rs6000_machopic_legitimize_pic_address (rtx orig, 
                            enum machine_mode mode, rtx reg);

#endif /* RTX_CODE */

#ifdef TREE_CODE
extern unsigned int rs6000_special_round_type_align (tree, int, int);
extern void function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode,
					  tree, int);
extern int function_arg_boundary (enum machine_mode, tree);
extern struct rtx_def *function_arg (CUMULATIVE_ARGS *,
					     enum machine_mode, tree, int);
extern int function_arg_partial_nregs (CUMULATIVE_ARGS *,
					       enum machine_mode, tree, int);
extern int function_arg_pass_by_reference (CUMULATIVE_ARGS *,
						   enum machine_mode,
						   tree, int);
extern rtx rs6000_function_value (tree, tree);
extern rtx rs6000_libcall_value (enum machine_mode);
extern struct rtx_def *rs6000_va_arg (tree, tree);
extern int function_ok_for_sibcall (tree);
extern void rs6000_elf_declare_function_name (FILE *, const char *, tree);
#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction' */
extern enum direction function_arg_padding (enum machine_mode, tree);
#endif /* ARGS_SIZE_RTX */

#endif /* TREE_CODE */

extern void optimization_options (int, int);
extern void rs6000_override_options (const char *);
extern int direct_return (void);
extern int first_reg_to_save (void);
extern int first_fp_reg_to_save (void);
extern void output_ascii (FILE *, const char *, int);
extern void rs6000_gen_section_name (char **, const char *, const char *);
extern void output_function_profiler (FILE *, int);
extern void output_profile_hook  (int);
extern int rs6000_trampoline_size (void);
extern void toc_section (void);
extern void sdata_section (void);
extern void sdata2_section (void);
extern void sbss_section (void);
extern void private_data_section (void);
extern void read_only_data_section (void);
extern void read_only_private_data_section (void);
extern int get_TOC_alias_set (void);
extern void rs6000_emit_prologue (void);
extern void rs6000_emit_load_toc_table (int);
extern void rs6000_aix_emit_builtin_unwind_init (void);
extern unsigned int rs6000_dbx_register_number (unsigned int);
extern void rs6000_emit_epilogue (int);
extern void rs6000_emit_eh_reg_restore (rtx, rtx);
extern const char * output_isel (rtx *);
extern int vrsave_operation (rtx, enum machine_mode);
extern int rs6000_register_move_cost (enum machine_mode,
					      enum reg_class, enum reg_class);
extern int rs6000_memory_move_cost (enum machine_mode, enum reg_class, int);
extern bool rs6000_tls_referenced_p (rtx);
extern int rs6000_tls_symbol_ref (rtx, enum machine_mode);
extern void rs6000_output_dwarf_dtprel (FILE*, int, rtx);

/* Declare functions in rs6000-c.c */

extern void rs6000_pragma_longcall (struct cpp_reader *);
extern void rs6000_cpu_cpp_builtins (struct cpp_reader *);

#if TARGET_MACHO
char *output_call (rtx, rtx *, int, int);
#endif

#endif  /* rs6000-protos.h */
