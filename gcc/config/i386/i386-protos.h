/* Definitions of target machine for GCC for IA-32.
   Copyright (C) 1988, 1992, 1994, 1995, 1996, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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

/* Functions in i386.c */
extern void override_options (bool);
extern void optimization_options (int, int);
extern void ix86_conditional_register_usage (void);

extern int ix86_can_use_return_insn_p (void);
extern void ix86_setup_frame_addresses (void);

extern void ix86_file_end (void);
extern HOST_WIDE_INT ix86_initial_elimination_offset (int, int);
extern void ix86_expand_prologue (void);
extern void ix86_expand_epilogue (int);

extern void ix86_output_addr_vec_elt (FILE *, int);
extern void ix86_output_addr_diff_elt (FILE *, int, int);

#ifdef RTX_CODE
extern int standard_80387_constant_p (rtx);
extern const char *standard_80387_constant_opcode (rtx);
extern rtx standard_80387_constant_rtx (int);
extern int standard_sse_constant_p (rtx);
extern const char *standard_sse_constant_opcode (rtx, rtx);
extern int symbolic_reference_mentioned_p (rtx);
extern bool extended_reg_mentioned_p (rtx);
extern bool x86_extended_QIreg_mentioned_p (rtx);
extern bool x86_extended_reg_mentioned_p (rtx);
extern enum machine_mode ix86_cc_mode (enum rtx_code, rtx, rtx);

extern int avx_vpermilp_parallel (rtx par, enum machine_mode mode);
extern int avx_vperm2f128_parallel (rtx par, enum machine_mode mode);

extern int ix86_expand_movmem (rtx, rtx, rtx, rtx, rtx, rtx);
extern int ix86_expand_setmem (rtx, rtx, rtx, rtx, rtx, rtx);
extern int ix86_expand_strlen (rtx, rtx, rtx, rtx);

extern bool legitimate_constant_p (rtx);
extern bool constant_address_p (rtx);
extern bool legitimate_pic_operand_p (rtx);
extern int legitimate_pic_address_disp_p (rtx);

extern void print_reg (rtx, int, FILE*);
extern void print_operand (FILE*, rtx, int);
extern void print_operand_address (FILE*, rtx);
extern bool output_addr_const_extra (FILE*, rtx);

extern void split_di (rtx[], int, rtx[], rtx[]);
extern void split_ti (rtx[], int, rtx[], rtx[]);

extern const char *output_set_got (rtx, rtx);
extern const char *output_387_binary_op (rtx, rtx*);
extern const char *output_387_reg_move (rtx, rtx*);
extern const char *output_fix_trunc (rtx, rtx*, int);
extern const char *output_fp_compare (rtx, rtx*, int, int);

extern void ix86_expand_clear (rtx);
extern void ix86_expand_move (enum machine_mode, rtx[]);
extern void ix86_expand_vector_move (enum machine_mode, rtx[]);
extern void ix86_expand_vector_move_misalign (enum machine_mode, rtx[]);
extern void ix86_expand_push (enum machine_mode, rtx);
extern rtx ix86_fixup_binary_operands (enum rtx_code,
				       enum machine_mode, rtx[]);
extern void ix86_fixup_binary_operands_no_copy (enum rtx_code,
						enum machine_mode, rtx[]);
extern void ix86_expand_binary_operator (enum rtx_code,
					 enum machine_mode, rtx[]);
extern int ix86_binary_operator_ok (enum rtx_code, enum machine_mode, rtx[]);
extern bool ix86_lea_for_add_ok (enum rtx_code, rtx, rtx[]);
extern bool ix86_vec_interleave_v2df_operator_ok (rtx operands[3], bool high);
extern bool ix86_dep_by_shift_count (const_rtx set_insn, const_rtx use_insn);
extern bool ix86_agi_dependent (rtx set_insn, rtx use_insn);
extern void ix86_expand_unary_operator (enum rtx_code, enum machine_mode,
					rtx[]);
extern rtx ix86_build_const_vector (enum machine_mode, bool, rtx);
extern rtx ix86_build_signbit_mask (enum machine_mode, bool, bool);
extern void ix86_split_convert_uns_si_sse (rtx[]);
extern void ix86_expand_convert_uns_didf_sse (rtx, rtx);
extern void ix86_expand_convert_uns_sixf_sse (rtx, rtx);
extern void ix86_expand_convert_uns_sidf_sse (rtx, rtx);
extern void ix86_expand_convert_uns_sisf_sse (rtx, rtx);
extern void ix86_expand_convert_sign_didf_sse (rtx, rtx);
extern enum ix86_fpcmp_strategy ix86_fp_comparison_strategy (enum rtx_code);
extern void ix86_expand_fp_absneg_operator (enum rtx_code, enum machine_mode,
					    rtx[]);
extern void ix86_expand_copysign (rtx []);
extern void ix86_split_copysign_const (rtx []);
extern void ix86_split_copysign_var (rtx []);
extern int ix86_unary_operator_ok (enum rtx_code, enum machine_mode, rtx[]);
extern int ix86_match_ccmode (rtx, enum machine_mode);
extern rtx ix86_expand_compare (enum rtx_code);
extern int ix86_use_fcomi_compare (enum rtx_code);
extern void ix86_expand_branch (enum rtx_code, rtx);
extern void ix86_expand_setcc (enum rtx_code, rtx);
extern int ix86_expand_int_movcc (rtx[]);
extern int ix86_expand_fp_movcc (rtx[]);
extern bool ix86_expand_fp_vcond (rtx[]);
extern bool ix86_expand_int_vcond (rtx[]);
extern void ix86_expand_sse_unpack (rtx[], bool, bool);
extern void ix86_expand_sse4_unpack (rtx[], bool, bool);
extern int ix86_expand_int_addcc (rtx[]);
extern void ix86_expand_call (rtx, rtx, rtx, rtx, rtx, int);
extern void x86_initialize_trampoline (rtx, rtx, rtx);
extern rtx ix86_zero_extend_to_Pmode (rtx);
extern void ix86_split_long_move (rtx[]);
extern void ix86_split_ashl (rtx *, rtx, enum machine_mode);
extern void ix86_split_ashr (rtx *, rtx, enum machine_mode);
extern void ix86_split_lshr (rtx *, rtx, enum machine_mode);
extern rtx ix86_find_base_term (rtx);
extern int ix86_check_movabs (rtx, int);

extern rtx assign_386_stack_local (enum machine_mode, enum ix86_stack_slot);
extern int ix86_attr_length_immediate_default (rtx, int);
extern int ix86_attr_length_address_default (rtx);
extern int ix86_attr_length_vex_default (rtx, int, int);

extern enum machine_mode ix86_fp_compare_mode (enum rtx_code);

extern rtx ix86_libcall_value (enum machine_mode);
extern bool ix86_function_value_regno_p (int);
extern bool ix86_function_arg_regno_p (int);
extern int ix86_function_arg_boundary (enum machine_mode, tree);
extern bool ix86_sol10_return_in_memory (const_tree,const_tree);
extern rtx ix86_force_to_memory (enum machine_mode, rtx);
extern void ix86_free_from_memory (enum machine_mode);
extern enum calling_abi ix86_cfun_abi (void);
extern enum calling_abi ix86_function_type_abi (const_tree);
extern void ix86_call_abi_override (const_tree);
extern tree ix86_fn_abi_va_list (tree);
extern tree ix86_canonical_va_list_type (tree);
extern int ix86_enum_va_list (int, const char **, tree *);
extern int ix86_reg_parm_stack_space (const_tree);

extern void ix86_split_fp_branch (enum rtx_code code, rtx, rtx,
				  rtx, rtx, rtx, rtx);
extern bool ix86_hard_regno_mode_ok (int, enum machine_mode);
extern bool ix86_modes_tieable_p (enum machine_mode, enum machine_mode);
extern int ix86_register_move_cost (enum machine_mode, enum reg_class,
				    enum reg_class);
extern int ix86_secondary_memory_needed (enum reg_class, enum reg_class,
					 enum machine_mode, int);
extern bool ix86_cannot_change_mode_class (enum machine_mode,
					   enum machine_mode, enum reg_class);
extern enum reg_class ix86_preferred_reload_class (rtx, enum reg_class);
extern enum reg_class ix86_preferred_output_reload_class (rtx, enum reg_class);
extern int ix86_memory_move_cost (enum machine_mode, enum reg_class, int);
extern int ix86_mode_needed (int, rtx);
extern void emit_i387_cw_initialization (int);
extern void x86_order_regs_for_local_alloc (void);
extern void x86_function_profiler (FILE *, int);
extern void x86_emit_floatuns (rtx [2]);
extern void ix86_emit_fp_unordered_jump (rtx);

extern void ix86_emit_i387_log1p (rtx, rtx);
extern void ix86_emit_swdivsf (rtx, rtx, rtx, enum machine_mode);
extern void ix86_emit_swsqrtsf (rtx, rtx, enum machine_mode, bool);

extern enum rtx_code ix86_reverse_condition (enum rtx_code, enum machine_mode);

extern void ix86_expand_lround (rtx, rtx);
extern void ix86_expand_lfloorceil (rtx, rtx, bool);
extern void ix86_expand_rint (rtx, rtx);
extern void ix86_expand_floorceil (rtx, rtx, bool);
extern void ix86_expand_floorceildf_32 (rtx, rtx, bool);
extern void ix86_expand_round (rtx, rtx);
extern void ix86_expand_rounddf_32 (rtx, rtx);
extern void ix86_expand_trunc (rtx, rtx);
extern void ix86_expand_truncdf_32 (rtx, rtx);

#ifdef TREE_CODE
extern void init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree);
extern rtx function_arg (CUMULATIVE_ARGS *, enum machine_mode, tree, int);
extern void function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode,
				  tree, int);
#endif

#endif

#ifdef TREE_CODE
extern int ix86_return_pops_args (tree, tree, int);

extern int ix86_data_alignment (tree, int);
extern unsigned int ix86_local_alignment (tree, enum machine_mode,
					  unsigned int);
extern unsigned int ix86_minimum_alignment (tree, enum machine_mode,
					    unsigned int);
extern int ix86_constant_alignment (tree, int);
extern tree ix86_handle_shared_attribute (tree *, tree, tree, int, bool *);
extern tree ix86_handle_selectany_attribute (tree *, tree, tree, int, bool *);
extern int x86_field_alignment (tree, int);
extern tree ix86_valid_target_attribute_tree (tree);
#endif

extern rtx ix86_tls_get_addr (void);
extern rtx ix86_tls_module_base (void);

extern void ix86_expand_vector_init (bool, rtx, rtx);
extern void ix86_expand_vector_set (bool, rtx, rtx, int);
extern void ix86_expand_vector_extract (bool, rtx, rtx, int);
extern void ix86_expand_reduc_v4sf (rtx (*)(rtx, rtx, rtx), rtx, rtx);

extern void ix86_expand_vec_extract_even_odd (rtx, rtx, rtx, unsigned);

/* In i386-c.c  */
extern void ix86_target_macros (void);
extern void ix86_register_pragmas (void);

/* In winnt.c  */
extern void i386_pe_unique_section (tree, int);
extern void i386_pe_declare_function_type (FILE *, const char *, int);
extern void i386_pe_record_external_function (tree, const char *);
extern void i386_pe_maybe_record_exported_symbol (tree, const char *, int);
extern void i386_pe_encode_section_info (tree, rtx, int);
extern bool i386_pe_binds_local_p (const_tree);
extern const char *i386_pe_strip_name_encoding_full (const char *);
extern bool i386_pe_valid_dllimport_attribute_p (const_tree);
extern unsigned int i386_pe_section_type_flags (tree, const char *, int);
extern void i386_pe_asm_named_section (const char *, unsigned int, tree);
extern void i386_pe_asm_output_aligned_decl_common (FILE *, tree,
						    const char *,
						    HOST_WIDE_INT,
						    HOST_WIDE_INT);
extern void i386_pe_file_end (void);
extern tree i386_pe_mangle_decl_assembler_name (tree, tree);

/* In winnt-cxx.c and winnt-stubs.c  */
extern void i386_pe_adjust_class_at_definition (tree);
extern bool i386_pe_type_dllimport_p (tree);
extern bool i386_pe_type_dllexport_p (tree);

extern rtx maybe_get_pool_constant (rtx);

extern char internal_label_prefix[16];
extern int internal_label_prefix_len;

enum ix86_address_seg { SEG_DEFAULT, SEG_FS, SEG_GS };
struct ix86_address
{
  rtx base, index, disp;
  HOST_WIDE_INT scale;
  enum ix86_address_seg seg;
};

extern int ix86_decompose_address (rtx, struct ix86_address *);
extern int memory_address_length (rtx addr);
extern void x86_output_aligned_bss (FILE *, tree, const char *,
				    unsigned HOST_WIDE_INT, int);
extern void x86_elf_aligned_common (FILE *, const char *,
				    unsigned HOST_WIDE_INT, int);

#ifdef RTX_CODE
extern void ix86_fp_comparison_codes (enum rtx_code code, enum rtx_code *,
				      enum rtx_code *, enum rtx_code *);
extern enum rtx_code ix86_fp_compare_code_to_integer (enum rtx_code);
extern rtx construct_plt_address (rtx);
#endif
extern int asm_preferred_eh_data_format (int, int);

#ifdef HAVE_ATTR_cpu
extern enum attr_cpu ix86_schedule;
#endif
