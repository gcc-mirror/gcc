/* Prototypes for exported functions of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2023 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

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


/* ------------------------------------------------------------------------ */

/* Defining Data Structures for Per-function Information.  */

extern void nds32_init_expanders (void);


/* Register Usage.  */

/* -- Order of Allocation of Registers.  */
extern void nds32_adjust_reg_alloc_order (void);

/* Register Classes.  */

extern enum reg_class nds32_regno_reg_class (int);


/* Stack Layout and Calling Conventions.  */

/* -- Basic Stack Layout.  */

extern rtx nds32_dynamic_chain_address (rtx);
extern rtx nds32_return_addr_rtx (int, rtx);

/* -- Eliminating Frame Pointer and Arg Pointer.  */

extern HOST_WIDE_INT nds32_initial_elimination_offset (unsigned int,
						       unsigned int);

/* -- Passing Arguments in Registers.  */

extern void nds32_init_cumulative_args (CUMULATIVE_ARGS *,
					tree, rtx, tree, int);

/* -- Function Entry and Exit.  */

extern void nds32_expand_prologue (void);
extern void nds32_expand_epilogue (bool);
extern void nds32_expand_prologue_v3push (void);
extern void nds32_expand_epilogue_v3pop (bool);
extern void nds32_emit_push_fpr_callee_saved (int);
extern void nds32_emit_pop_fpr_callee_saved (int);
extern void nds32_emit_v3pop_fpr_callee_saved (int);

/* Controlling Debugging Information Format.  */

extern unsigned int nds32_debugger_regno (unsigned int);

/* ------------------------------------------------------------------------ */

/* Auxiliary functions for manipulation DI mode.  */

extern rtx nds32_di_high_part_subreg(rtx);
extern rtx nds32_di_low_part_subreg(rtx);

/* Auxiliary functions for expanding rtl used in nds32-multiple.md.  */

extern rtx nds32_expand_load_multiple (int, int, rtx, rtx, bool, rtx *);
extern rtx nds32_expand_store_multiple (int, int, rtx, rtx, bool, rtx *);
extern bool nds32_expand_cpymemsi (rtx, rtx, rtx, rtx);
extern bool nds32_expand_setmem (rtx, rtx, rtx, rtx, rtx, rtx);
extern bool nds32_expand_strlen (rtx, rtx, rtx, rtx);

/* Auxiliary functions for expand unalign load instruction.  */

extern void nds32_expand_unaligned_load (rtx *, enum machine_mode);

/* Auxiliary functions for expand unalign store instruction.  */

extern void nds32_expand_unaligned_store (rtx *, enum machine_mode);

/* Auxiliary functions for multiple load/store predicate checking.  */

extern bool nds32_valid_multiple_load_store_p (rtx, bool, bool);

/* Auxiliary functions for guard function checking in pipelines.md.  */

extern bool nds32_n7_load_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_n7_last_load_to_ii_p (rtx_insn *, rtx_insn *);

extern bool nds32_n8_load_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_n8_load_bi_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_n8_load_to_ex_p (rtx_insn *, rtx_insn *);
extern bool nds32_n8_ex_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_n8_last_load_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_n8_last_load_two_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_n8_last_load_to_ex_p (rtx_insn *, rtx_insn *);

extern bool nds32_e8_load_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_e8_load_to_ex_p (rtx_insn *, rtx_insn *);
extern bool nds32_e8_ex_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_e8_last_load_to_ii_p (rtx_insn *, rtx_insn *);
extern bool nds32_e8_last_load_to_ex_p (rtx_insn *, rtx_insn *);

extern bool nds32_n9_2r1w_mm_to_ex_p (rtx_insn *, rtx_insn *);
extern bool nds32_n9_3r2w_mm_to_ex_p (rtx_insn *, rtx_insn *);
extern bool nds32_n9_last_load_to_ex_p (rtx_insn *, rtx_insn *);

extern bool nds32_n10_ex_to_ex_p (rtx_insn *, rtx_insn *);
extern bool nds32_n10_mm_to_ex_p (rtx_insn *, rtx_insn *);
extern bool nds32_n10_last_load_to_ex_p (rtx_insn *, rtx_insn *);

extern bool nds32_gw_ex_to_ex_p (rtx_insn *, rtx_insn *);
extern bool nds32_gw_mm_to_ex_p (rtx_insn *, rtx_insn *);
extern bool nds32_gw_last_load_to_ex_p (rtx_insn *, rtx_insn *);

extern bool nds32_n13_e2_to_e1_p (rtx_insn *, rtx_insn *);
extern bool nds32_n13_load_to_e1_p (rtx_insn *, rtx_insn *);
extern bool nds32_n13_load_to_e2_p (rtx_insn *, rtx_insn *);
extern bool nds32_n13_last_load_to_e1_p (rtx_insn *, rtx_insn *);
extern bool nds32_n13_last_load_to_e2_p (rtx_insn *, rtx_insn *);
extern bool nds32_n13_last_two_load_to_e1_p (rtx_insn *, rtx_insn *);

/* Auxiliary functions for stack operation predicate checking.  */

extern bool nds32_valid_stack_push_pop_p (rtx, bool);

/* Auxiliary functions for bit operation detection.  */

extern bool nds32_can_use_bclr_p (HOST_WIDE_INT);
extern bool nds32_can_use_bset_p (HOST_WIDE_INT);
extern bool nds32_can_use_btgl_p (HOST_WIDE_INT);

extern bool nds32_can_use_bitci_p (HOST_WIDE_INT);

extern bool nds32_const_double_range_ok_p (rtx, machine_mode,
					   HOST_WIDE_INT, HOST_WIDE_INT);

extern bool nds32_const_unspec_p (rtx x);

/* Auxiliary function for 'Computing the Length of an Insn'.  */

extern int nds32_adjust_insn_length (rtx_insn *, int);

/* Auxiliary functions for FP_AS_GP detection.  */

extern bool nds32_symbol_load_store_p (rtx_insn *);
extern bool nds32_naked_function_p (tree);

/* Auxiliary functions for jump table generation.  */

extern const char *nds32_output_casesi_pc_relative (rtx *);
extern const char *nds32_output_casesi (rtx *);

/* Auxiliary functions for conditional branch generation.  */

extern enum nds32_expand_result_type nds32_expand_cbranch (rtx *);
extern enum nds32_expand_result_type nds32_expand_cstore (rtx *);
extern void nds32_expand_float_cbranch (rtx *);
extern void nds32_expand_float_cstore (rtx *);

/* Auxiliary functions for conditional move generation.  */

extern enum nds32_expand_result_type nds32_expand_movcc (rtx *);
extern void nds32_expand_float_movcc (rtx *);

/* Auxiliary functions for expand extv/insv instruction.  */

extern enum nds32_expand_result_type nds32_expand_extv (rtx *);
extern enum nds32_expand_result_type nds32_expand_insv (rtx *);

/* Auxiliary functions for expand PIC instruction.  */

extern void nds32_expand_pic_move (rtx *);

/* Auxiliary functions to legitimize PIC address.  */

extern rtx nds32_legitimize_pic_address (rtx);

/* Auxiliary functions for expand TLS instruction.  */

extern void nds32_expand_tls_move (rtx *);

/* Auxiliary functions to legitimize TLS address.  */

extern rtx nds32_legitimize_tls_address (rtx);

/* Auxiliary functions to identify thread-local symbol.  */

extern bool nds32_tls_referenced_p (rtx);

/* Auxiliary functions for expand ICT instruction.  */

extern void nds32_expand_ict_move (rtx *);

/* Auxiliary functions to legitimize address for indirect-call symbol.  */

extern rtx nds32_legitimize_ict_address (rtx);

/* Auxiliary functions to identify indirect-call symbol.  */

extern bool nds32_indirect_call_referenced_p (rtx);

/* Auxiliary functions to identify long-call symbol.  */
extern bool nds32_long_call_p (rtx);

/* Auxiliary functions to identify SYMBOL_REF and LABEL_REF pattern.  */

extern bool symbolic_reference_mentioned_p (rtx);

/* Auxiliary functions to identify conditional move comparison operand.  */

extern int nds32_cond_move_p (rtx);

/* Auxiliary functions to identify 16 bit addresing mode.  */

extern enum nds32_16bit_address_type nds32_mem_format (rtx);

/* Auxiliary functions to identify floating-point addresing mode.  */

extern bool nds32_float_mem_operand_p (rtx);

/* Auxiliary functions to output assembly code.  */

extern const char *nds32_output_16bit_store (rtx *, int);
extern const char *nds32_output_16bit_load (rtx *, int);
extern const char *nds32_output_32bit_store (rtx *, int);
extern const char *nds32_output_32bit_load (rtx *, int);
extern const char *nds32_output_32bit_load_s (rtx *, int);
extern const char *nds32_output_float_load(rtx *);
extern const char *nds32_output_float_store(rtx *);
extern const char *nds32_output_smw_single_word (rtx *);
extern const char *nds32_output_smw_double_word (rtx *);
extern const char *nds32_output_lmw_single_word (rtx *);
extern const char *nds32_output_double (rtx *, bool);
extern const char *nds32_output_cbranchsi4_equality_zero (rtx_insn *, rtx *);
extern const char *nds32_output_cbranchsi4_equality_reg (rtx_insn *, rtx *);
extern const char *nds32_output_cbranchsi4_equality_reg_or_const_int (rtx_insn *,
								      rtx *);
extern const char *nds32_output_cbranchsi4_greater_less_zero (rtx_insn *, rtx *);

extern const char *nds32_output_unpkd8 (rtx, rtx, rtx, rtx, bool);

extern const char *nds32_output_call (rtx, rtx *, rtx,
				      const char *, const char *, bool);
extern const char *nds32_output_tls_desc (rtx *);
extern const char *nds32_output_tls_ie (rtx *);
extern const char *nds32_output_symrel (rtx *);

/* Auxiliary functions to output stack push/pop instruction.  */

extern const char *nds32_output_stack_push (rtx);
extern const char *nds32_output_stack_pop (rtx);
extern const char *nds32_output_return (void);


/* Auxiliary functions to split/output sms pattern.  */
extern bool nds32_need_split_sms_p (rtx, rtx, rtx, rtx);
extern const char *nds32_output_sms (rtx, rtx, rtx, rtx);
extern void nds32_split_sms (rtx, rtx, rtx, rtx, rtx, rtx, rtx);

/* Auxiliary functions to split double word RTX pattern.  */

extern void nds32_spilt_doubleword (rtx *, bool);
extern void nds32_split_ashiftdi3 (rtx, rtx, rtx);
extern void nds32_split_ashiftrtdi3 (rtx, rtx, rtx);
extern void nds32_split_lshiftrtdi3 (rtx, rtx, rtx);
extern void nds32_split_rotatertdi3 (rtx, rtx, rtx);

/* Auxiliary functions to split large constant RTX pattern.  */

extern void nds32_expand_constant (machine_mode,
				   HOST_WIDE_INT, rtx, rtx);

/* Auxiliary functions to check using return with null epilogue.  */

extern int nds32_can_use_return_insn (void);
extern scalar_int_mode nds32_case_vector_shorten_mode (int, int, rtx);

/* Auxiliary functions to decide output alignment or not.  */

extern int nds32_target_alignment (rtx_insn *);
extern unsigned int nds32_data_alignment (tree, unsigned int);
extern unsigned int nds32_local_alignment (tree, unsigned int);

/* Auxiliary functions to expand builtin functions.  */

extern void nds32_init_builtins_impl (void);
extern rtx nds32_expand_builtin_impl (tree, rtx, rtx,
				      machine_mode, int);
extern tree nds32_builtin_decl_impl (unsigned, bool);

/* Auxiliary functions for ISR implementation.  */

extern void nds32_check_isr_attrs_conflict (tree, tree);
extern void nds32_construct_isr_vectors_information (tree, const char *);
extern void nds32_asm_file_start_for_isr (void);
extern void nds32_asm_file_end_for_isr (void);
extern bool nds32_isr_function_p (tree);
extern bool nds32_isr_function_critical_p (tree);

/* Auxiliary functions for cost calculation.  */

extern void nds32_init_rtx_costs (void);
extern bool nds32_rtx_costs_impl (rtx, machine_mode, int, int, int *, bool);
extern int nds32_address_cost_impl (rtx, machine_mode, addr_space_t, bool);

/* Auxiliary functions for pre-define marco.  */
extern void nds32_cpu_cpp_builtins(struct cpp_reader *);

/* Auxiliary functions for const_vector's constraints.  */

extern HOST_WIDE_INT const_vector_to_hwint (rtx);
extern bool nds32_valid_CVp5_p (rtx);
extern bool nds32_valid_CVs5_p (rtx);
extern bool nds32_valid_CVs2_p (rtx);
extern bool nds32_valid_CVhi_p (rtx);

/* Auxiliary functions for lwm/smw.  */

extern bool nds32_valid_smw_lwm_base_p (rtx);

extern bool nds32_split_double_word_load_store_p (rtx *,bool);

namespace nds32 {

extern rtx extract_pattern_from_insn (rtx);

size_t parallel_elements (rtx);
rtx parallel_element (rtx, int);
bool load_single_p (rtx_insn *);
bool store_single_p (rtx_insn *);
bool load_double_p (rtx_insn *);
bool store_double_p (rtx_insn *);
bool store_offset_reg_p (rtx_insn *);
bool post_update_insn_p (rtx_insn *);
bool immed_offset_p (rtx);
int find_post_update_rtx (rtx_insn *);
rtx extract_mem_rtx (rtx_insn *);
rtx extract_base_reg (rtx_insn *);
rtx extract_offset_rtx (rtx_insn *);

rtx extract_shift_reg (rtx);

bool movd44_insn_p (rtx_insn *);
rtx extract_movd44_odd_reg (rtx_insn *);

rtx extract_mac_non_acc_rtx (rtx_insn *);

bool divmod_p (rtx_insn *);

rtx extract_branch_target_rtx (rtx_insn *);
rtx extract_branch_condition_rtx (rtx_insn *);
} // namespace nds32

extern bool nds32_use_load_post_increment(machine_mode);

/* Functions for create nds32 specific optimization pass.  */
extern rtl_opt_pass *make_pass_nds32_relax_opt (gcc::context *);
extern rtl_opt_pass *make_pass_nds32_fp_as_gp (gcc::context *);

extern int nds32_alloc_relax_group_id ();

/* ------------------------------------------------------------------------ */
