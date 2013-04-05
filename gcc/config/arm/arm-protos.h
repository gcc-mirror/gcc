/* Prototypes for exported functions defined in arm.c and pe.c
   Copyright (C) 1999-2013 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (rearnsha@arm.com)
   Minor hacks by Nick Clifton (nickc@cygnus.com)

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

#ifndef GCC_ARM_PROTOS_H
#define GCC_ARM_PROTOS_H

extern enum unwind_info_type arm_except_unwind_info (struct gcc_options *);
extern int use_return_insn (int, rtx);
extern enum reg_class arm_regno_class (int);
extern void arm_load_pic_register (unsigned long);
extern int arm_volatile_func (void);
extern void arm_expand_prologue (void);
extern void arm_expand_epilogue (bool);
extern void thumb2_expand_return (void);
extern const char *arm_strip_name_encoding (const char *);
extern void arm_asm_output_labelref (FILE *, const char *);
extern void thumb2_asm_output_opcode (FILE *);
extern unsigned long arm_current_func_type (void);
extern HOST_WIDE_INT arm_compute_initial_elimination_offset (unsigned int,
							     unsigned int);
extern HOST_WIDE_INT thumb_compute_initial_elimination_offset (unsigned int,
							       unsigned int);
extern unsigned int arm_dbx_register_number (unsigned int);
extern void arm_output_fn_unwind (FILE *, bool);
  

#ifdef RTX_CODE
extern bool arm_vector_mode_supported_p (enum machine_mode);
extern bool arm_small_register_classes_for_mode_p (enum machine_mode);
extern int arm_hard_regno_mode_ok (unsigned int, enum machine_mode);
extern bool arm_modes_tieable_p (enum machine_mode, enum machine_mode);
extern int const_ok_for_arm (HOST_WIDE_INT);
extern int const_ok_for_op (HOST_WIDE_INT, enum rtx_code);
extern int const_ok_for_dimode_op (HOST_WIDE_INT, enum rtx_code);
extern int arm_split_constant (RTX_CODE, enum machine_mode, rtx,
			       HOST_WIDE_INT, rtx, rtx, int);
extern int legitimate_pic_operand_p (rtx);
extern rtx legitimize_pic_address (rtx, enum machine_mode, rtx);
extern rtx legitimize_tls_address (rtx, rtx);
extern int arm_legitimate_address_outer_p (enum machine_mode, rtx, RTX_CODE, int);
extern int thumb_legitimate_offset_p (enum machine_mode, HOST_WIDE_INT);
extern bool arm_legitimize_reload_address (rtx *, enum machine_mode, int, int,
					   int);
extern rtx thumb_legitimize_reload_address (rtx *, enum machine_mode, int, int,
					    int);
extern int thumb1_legitimate_address_p (enum machine_mode, rtx, int);
extern bool ldm_stm_operation_p (rtx, bool, enum machine_mode mode,
                                 bool, bool);
extern int arm_const_double_rtx (rtx);
extern int vfp3_const_double_rtx (rtx);
extern int neon_immediate_valid_for_move (rtx, enum machine_mode, rtx *, int *);
extern int neon_immediate_valid_for_logic (rtx, enum machine_mode, int, rtx *,
					   int *);
extern int neon_immediate_valid_for_shift (rtx, enum machine_mode, rtx *,
					   int *, bool);
extern char *neon_output_logic_immediate (const char *, rtx *,
					  enum machine_mode, int, int);
extern char *neon_output_shift_immediate (const char *, char, rtx *,
					  enum machine_mode, int, bool);
extern void neon_pairwise_reduce (rtx, rtx, enum machine_mode,
				  rtx (*) (rtx, rtx, rtx));
extern rtx neon_make_constant (rtx);
extern tree arm_builtin_vectorized_function (tree, tree, tree);
extern void neon_expand_vector_init (rtx, rtx);
extern void neon_lane_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
extern void neon_const_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
extern HOST_WIDE_INT neon_element_bits (enum machine_mode);
extern void neon_reinterpret (rtx, rtx);
extern void neon_emit_pair_result_insn (enum machine_mode,
					rtx (*) (rtx, rtx, rtx, rtx),
					rtx, rtx, rtx);
extern void neon_disambiguate_copy (rtx *, rtx *, rtx *, unsigned int);
extern void neon_split_vcombine (rtx op[3]);
extern enum reg_class coproc_secondary_reload_class (enum machine_mode, rtx,
						     bool);
extern bool arm_tls_referenced_p (rtx);

extern int arm_coproc_mem_operand (rtx, bool);
extern int neon_vector_mem_operand (rtx, int);
extern int neon_struct_mem_operand (rtx);
extern int arm_no_early_store_addr_dep (rtx, rtx);
extern int arm_early_store_addr_dep (rtx, rtx);
extern int arm_early_load_addr_dep (rtx, rtx);
extern int arm_no_early_alu_shift_dep (rtx, rtx);
extern int arm_no_early_alu_shift_value_dep (rtx, rtx);
extern int arm_no_early_mul_dep (rtx, rtx);
extern int arm_mac_accumulator_is_result (rtx, rtx);
extern int arm_mac_accumulator_is_mul_result (rtx, rtx);

extern int tls_mentioned_p (rtx);
extern int symbol_mentioned_p (rtx);
extern int label_mentioned_p (rtx);
extern RTX_CODE minmax_code (rtx);
extern bool arm_sat_operator_match (rtx, rtx, int *, bool *);
extern int adjacent_mem_locations (rtx, rtx);
extern bool gen_ldm_seq (rtx *, int, bool);
extern bool gen_stm_seq (rtx *, int);
extern bool gen_const_stm_seq (rtx *, int);
extern rtx arm_gen_load_multiple (int *, int, rtx, int, rtx, HOST_WIDE_INT *);
extern rtx arm_gen_store_multiple (int *, int, rtx, int, rtx, HOST_WIDE_INT *);
extern bool offset_ok_for_ldrd_strd (HOST_WIDE_INT);
extern bool operands_ok_ldrd_strd (rtx, rtx, rtx, HOST_WIDE_INT, bool, bool);
extern bool gen_operands_ldrd_strd (rtx *, bool, bool, bool);
extern int arm_gen_movmemqi (rtx *);
extern enum machine_mode arm_select_cc_mode (RTX_CODE, rtx, rtx);
extern enum machine_mode arm_select_dominance_cc_mode (rtx, rtx,
						       HOST_WIDE_INT);
extern rtx arm_gen_compare_reg (RTX_CODE, rtx, rtx, rtx);
extern rtx arm_gen_return_addr_mask (void);
extern void arm_reload_in_hi (rtx *);
extern void arm_reload_out_hi (rtx *);
extern int arm_const_double_inline_cost (rtx);
extern bool arm_const_double_by_parts (rtx);
extern bool arm_const_double_by_immediates (rtx);
extern const char *fp_immediate_constant (rtx);
extern void arm_emit_call_insn (rtx, rtx);
extern const char *output_call (rtx *);
extern const char *output_call_mem (rtx *);
void arm_emit_movpair (rtx, rtx);
extern const char *output_mov_long_double_arm_from_arm (rtx *);
extern const char *output_move_double (rtx *, bool, int *count);
extern const char *output_move_quad (rtx *);
extern int arm_count_output_move_double_insns (rtx *);
extern const char *output_move_vfp (rtx *operands);
extern const char *output_move_neon (rtx *operands);
extern int arm_attr_length_move_neon (rtx);
extern int arm_address_offset_is_imm (rtx);
extern const char *output_add_immediate (rtx *);
extern const char *arithmetic_instr (rtx, int);
extern void output_ascii_pseudo_op (FILE *, const unsigned char *, int);
extern const char *output_return_instruction (rtx, bool, bool, bool);
extern void arm_poke_function_name (FILE *, const char *);
extern void arm_final_prescan_insn (rtx);
extern int arm_debugger_arg_offset (int, rtx);
extern bool arm_is_long_call_p (tree);
extern int    arm_emit_vector_const (FILE *, rtx);
extern void arm_emit_fp16_const (rtx c);
extern const char * arm_output_load_gr (rtx *);
extern const char *vfp_output_fstmd (rtx *);
extern void arm_output_multireg_pop (rtx *, bool, rtx, bool, bool);
extern void arm_set_return_address (rtx, rtx);
extern int arm_eliminable_register (rtx);
extern const char *arm_output_shift(rtx *, int);
extern const char *arm_output_iwmmxt_shift_immediate (const char *, rtx *, bool);
extern const char *arm_output_iwmmxt_tinsr (rtx *);
extern unsigned int arm_sync_loop_insns (rtx , rtx *);
extern int arm_attr_length_push_multi(rtx, rtx);
extern void arm_expand_compare_and_swap (rtx op[]);
extern void arm_split_compare_and_swap (rtx op[]);
extern void arm_split_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx, rtx, rtx);
extern rtx arm_load_tp (rtx);

#if defined TREE_CODE
extern void arm_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree);
extern bool arm_pad_arg_upward (enum machine_mode, const_tree);
extern bool arm_pad_reg_upward (enum machine_mode, tree, int);
#endif
extern int arm_apply_result_size (void);

#endif /* RTX_CODE */

/* Thumb functions.  */
extern void arm_init_expanders (void);
extern const char *thumb1_unexpanded_epilogue (void);
extern void thumb1_expand_prologue (void);
extern void thumb1_expand_epilogue (void);
extern const char *thumb1_output_interwork (void);
#ifdef TREE_CODE
extern int is_called_in_ARM_mode (tree);
#endif
extern int thumb_shiftable_const (unsigned HOST_WIDE_INT);
#ifdef RTX_CODE
extern enum arm_cond_code maybe_get_arm_condition_code (rtx);
extern void thumb1_final_prescan_insn (rtx);
extern void thumb2_final_prescan_insn (rtx);
extern const char *thumb_load_double_from_address (rtx *);
extern const char *thumb_output_move_mem_multiple (int, rtx *);
extern const char *thumb_call_via_reg (rtx);
extern void thumb_expand_movmemqi (rtx *);
extern rtx arm_return_addr (int, rtx);
extern void thumb_reload_out_hi (rtx *);
extern void thumb_reload_in_hi (rtx *);
extern void thumb_set_return_address (rtx, rtx);
extern const char *thumb1_output_casesi (rtx *);
extern const char *thumb2_output_casesi (rtx *);
#endif

/* Defined in pe.c.  */
extern int arm_dllexport_name_p (const char *);
extern int arm_dllimport_name_p (const char *);

#ifdef TREE_CODE
extern void arm_pe_unique_section (tree, int);
extern void arm_pe_encode_section_info (tree, rtx, int);
extern int arm_dllexport_p (tree);
extern int arm_dllimport_p (tree);
extern void arm_mark_dllexport (tree);
extern void arm_mark_dllimport (tree);
#endif

extern void arm_pr_long_calls (struct cpp_reader *);
extern void arm_pr_no_long_calls (struct cpp_reader *);
extern void arm_pr_long_calls_off (struct cpp_reader *);

extern void arm_lang_object_attributes_init(void);

extern const char *arm_mangle_type (const_tree);

extern void arm_order_regs_for_local_alloc (void);

/* Vectorizer cost model implementation.  */
struct cpu_vec_costs {
  const int scalar_stmt_cost;   /* Cost of any scalar operation, excluding
				   load and store.  */
  const int scalar_load_cost;   /* Cost of scalar load.  */
  const int scalar_store_cost;  /* Cost of scalar store.  */
  const int vec_stmt_cost;      /* Cost of any vector operation, excluding
                                   load, store, vector-to-scalar and
                                   scalar-to-vector operation.  */
  const int vec_to_scalar_cost;    /* Cost of vect-to-scalar operation.  */
  const int scalar_to_vec_cost;    /* Cost of scalar-to-vector operation.  */
  const int vec_align_load_cost;   /* Cost of aligned vector load.  */
  const int vec_unalign_load_cost; /* Cost of unaligned vector load.  */
  const int vec_unalign_store_cost; /* Cost of unaligned vector load.  */
  const int vec_store_cost;        /* Cost of vector store.  */
  const int cond_taken_branch_cost;    /* Cost of taken branch for vectorizer
					  cost model.  */
  const int cond_not_taken_branch_cost;/* Cost of not taken branch for
					  vectorizer cost model.  */
};

#ifdef RTX_CODE
/* This needs to be here because we need RTX_CODE and similar.  */

struct tune_params
{
  bool (*rtx_costs) (rtx, RTX_CODE, RTX_CODE, int *, bool);
  bool (*sched_adjust_cost) (rtx, rtx, rtx, int *);
  int constant_limit;
  /* Maximum number of instructions to conditionalise in
     arm_final_prescan_insn.  */
  int max_insns_skipped;
  int num_prefetch_slots;
  int l1_cache_size;
  int l1_cache_line_size;
  bool prefer_constant_pool;
  int (*branch_cost) (bool, bool);
  /* Prefer STRD/LDRD instructions over PUSH/POP/LDM/STM.  */
  bool prefer_ldrd_strd;
  /* The preference for non short cirtcuit operation when optimizing for
     performance. The first element covers Thumb state and the second one
     is for ARM state.  */
  bool logical_op_non_short_circuit[2];
  /* Vectorizer costs.  */
  const struct cpu_vec_costs* vec_costs;
  /* Prefer Neon for 64-bit bitops.  */
  bool prefer_neon_for_64bits;
};

extern const struct tune_params *current_tune;
extern int vfp3_const_double_for_fract_bits (rtx);

extern void arm_emit_coreregs_64bit_shift (enum rtx_code, rtx, rtx, rtx, rtx,
					   rtx);
extern bool arm_validize_comparison (rtx *, rtx *, rtx *);
#endif /* RTX_CODE */

extern void arm_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel);
extern bool arm_expand_vec_perm_const (rtx target, rtx op0, rtx op1, rtx sel);

extern bool arm_autoinc_modes_ok_p (enum machine_mode, enum arm_auto_incmodes);

extern void arm_emit_eabi_attribute (const char *, int, int);

#endif /* ! GCC_ARM_PROTOS_H */
