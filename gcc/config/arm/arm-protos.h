/* Prototypes for exported functions defined in arm.c and pe.c
   Copyright (C) 1999-2016 Free Software Foundation, Inc.
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
extern bool use_simple_return_p (void);
extern enum reg_class arm_regno_class (int);
extern void arm_load_pic_register (unsigned long);
extern int arm_volatile_func (void);
extern void arm_expand_prologue (void);
extern void arm_expand_epilogue (bool);
extern void arm_declare_function_name (FILE *, const char *, tree);
extern void thumb2_expand_return (bool);
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

extern rtx arm_expand_builtin (tree exp, rtx target, rtx subtarget
			       ATTRIBUTE_UNUSED, enum machine_mode mode
			       ATTRIBUTE_UNUSED, int ignore ATTRIBUTE_UNUSED);
extern tree arm_builtin_decl (unsigned code, bool initialize_p
			      ATTRIBUTE_UNUSED);
extern void arm_init_builtins (void);
extern void arm_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update);

#ifdef RTX_CODE
extern bool arm_vector_mode_supported_p (machine_mode);
extern bool arm_small_register_classes_for_mode_p (machine_mode);
extern int arm_hard_regno_mode_ok (unsigned int, machine_mode);
extern bool arm_modes_tieable_p (machine_mode, machine_mode);
extern int const_ok_for_arm (HOST_WIDE_INT);
extern int const_ok_for_op (HOST_WIDE_INT, enum rtx_code);
extern int const_ok_for_dimode_op (HOST_WIDE_INT, enum rtx_code);
extern int arm_split_constant (RTX_CODE, machine_mode, rtx,
			       HOST_WIDE_INT, rtx, rtx, int);
extern int legitimate_pic_operand_p (rtx);
extern rtx legitimize_pic_address (rtx, machine_mode, rtx);
extern rtx legitimize_tls_address (rtx, rtx);
extern bool arm_legitimate_address_p (machine_mode, rtx, bool);
extern int arm_legitimate_address_outer_p (machine_mode, rtx, RTX_CODE, int);
extern int thumb_legitimate_offset_p (machine_mode, HOST_WIDE_INT);
extern int thumb1_legitimate_address_p (machine_mode, rtx, int);
extern bool ldm_stm_operation_p (rtx, bool, machine_mode mode,
                                 bool, bool);
extern int arm_const_double_rtx (rtx);
extern int vfp3_const_double_rtx (rtx);
extern int neon_immediate_valid_for_move (rtx, machine_mode, rtx *, int *);
extern int neon_immediate_valid_for_logic (rtx, machine_mode, int, rtx *,
					   int *);
extern int neon_immediate_valid_for_shift (rtx, machine_mode, rtx *,
					   int *, bool);
extern char *neon_output_logic_immediate (const char *, rtx *,
					  machine_mode, int, int);
extern char *neon_output_shift_immediate (const char *, char, rtx *,
					  machine_mode, int, bool);
extern void neon_pairwise_reduce (rtx, rtx, machine_mode,
				  rtx (*) (rtx, rtx, rtx));
extern rtx neon_make_constant (rtx);
extern tree arm_builtin_vectorized_function (unsigned int, tree, tree);
extern void neon_expand_vector_init (rtx, rtx);
extern void neon_lane_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT, const_tree);
extern void neon_const_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
extern HOST_WIDE_INT neon_element_bits (machine_mode);
extern void neon_emit_pair_result_insn (machine_mode,
					rtx (*) (rtx, rtx, rtx, rtx),
					rtx, rtx, rtx);
extern void neon_disambiguate_copy (rtx *, rtx *, rtx *, unsigned int);
extern void neon_split_vcombine (rtx op[3]);
extern enum reg_class coproc_secondary_reload_class (machine_mode, rtx,
						     bool);
extern bool arm_tls_referenced_p (rtx);

extern int arm_coproc_mem_operand (rtx, bool);
extern int neon_vector_mem_operand (rtx, int, bool);
extern int neon_struct_mem_operand (rtx);

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
extern bool gen_movmem_ldrd_strd (rtx *);
extern machine_mode arm_select_cc_mode (RTX_CODE, rtx, rtx);
extern machine_mode arm_select_dominance_cc_mode (rtx, rtx,
						       HOST_WIDE_INT);
extern rtx arm_gen_compare_reg (RTX_CODE, rtx, rtx, rtx);
extern rtx arm_gen_return_addr_mask (void);
extern void arm_reload_in_hi (rtx *);
extern void arm_reload_out_hi (rtx *);
extern int arm_max_const_double_inline_cost (void);
extern int arm_const_double_inline_cost (rtx);
extern bool arm_const_double_by_parts (rtx);
extern bool arm_const_double_by_immediates (rtx);
extern void arm_emit_call_insn (rtx, rtx, bool);
extern const char *output_call (rtx *);
void arm_emit_movpair (rtx, rtx);
extern const char *output_mov_long_double_arm_from_arm (rtx *);
extern const char *output_move_double (rtx *, bool, int *count);
extern const char *output_move_quad (rtx *);
extern int arm_count_output_move_double_insns (rtx *);
extern const char *output_move_vfp (rtx *operands);
extern const char *output_move_neon (rtx *operands);
extern int arm_attr_length_move_neon (rtx_insn *);
extern int arm_address_offset_is_imm (rtx_insn *);
extern const char *output_add_immediate (rtx *);
extern const char *arithmetic_instr (rtx, int);
extern void output_ascii_pseudo_op (FILE *, const unsigned char *, int);
extern const char *output_return_instruction (rtx, bool, bool, bool);
extern const char *output_probe_stack_range (rtx, rtx);
extern void arm_poke_function_name (FILE *, const char *);
extern void arm_final_prescan_insn (rtx_insn *);
extern int arm_debugger_arg_offset (int, rtx);
extern bool arm_is_long_call_p (tree);
extern int    arm_emit_vector_const (FILE *, rtx);
extern void arm_emit_fp16_const (rtx c);
extern const char * arm_output_load_gr (rtx *);
extern const char *vfp_output_vstmd (rtx *);
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
extern bool arm_pad_arg_upward (machine_mode, const_tree);
extern bool arm_pad_reg_upward (machine_mode, tree, int);
#endif
extern int arm_apply_result_size (void);

#endif /* RTX_CODE */

/* Thumb functions.  */
extern void arm_init_expanders (void);
extern const char *thumb1_unexpanded_epilogue (void);
extern void thumb1_expand_prologue (void);
extern void thumb1_expand_epilogue (void);
extern const char *thumb1_output_interwork (void);
extern int thumb_shiftable_const (unsigned HOST_WIDE_INT);
#ifdef RTX_CODE
extern enum arm_cond_code maybe_get_arm_condition_code (rtx);
extern void thumb1_final_prescan_insn (rtx_insn *);
extern void thumb2_final_prescan_insn (rtx_insn *);
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
extern bool arm_change_mode_p (tree);
#endif

extern tree arm_valid_target_attribute_tree (tree, struct gcc_options *,
					     struct gcc_options *);
extern void arm_pr_long_calls (struct cpp_reader *);
extern void arm_pr_no_long_calls (struct cpp_reader *);
extern void arm_pr_long_calls_off (struct cpp_reader *);

extern const char *arm_mangle_type (const_tree);
extern const char *arm_mangle_builtin_type (const_tree);

extern void arm_order_regs_for_local_alloc (void);

extern int arm_max_conditional_execute ();

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

struct cpu_cost_table;

/* Dump function ARM_PRINT_TUNE_INFO should be updated whenever this
   structure is modified.  */

struct tune_params
{
  bool (*rtx_costs) (rtx, RTX_CODE, RTX_CODE, int *, bool);
  const struct cpu_cost_table *insn_extra_cost;
  bool (*sched_adjust_cost) (rtx_insn *, rtx, rtx_insn *, int *);
  int (*branch_cost) (bool, bool);
  /* Vectorizer costs.  */
  const struct cpu_vec_costs* vec_costs;
  int constant_limit;
  /* Maximum number of instructions to conditionalise.  */
  int max_insns_skipped;
  /* Maximum number of instructions to inline calls to memset.  */
  int max_insns_inline_memset;
  /* Issue rate of the processor.  */
  unsigned int issue_rate;
  /* Explicit prefetch data.  */
  struct
    {
      int num_slots;
      int l1_cache_size;
      int l1_cache_line_size;
    } prefetch;
  enum {PREF_CONST_POOL_FALSE, PREF_CONST_POOL_TRUE}
    prefer_constant_pool: 1;
  /* Prefer STRD/LDRD instructions over PUSH/POP/LDM/STM.  */
  enum {PREF_LDRD_FALSE, PREF_LDRD_TRUE} prefer_ldrd_strd: 1;
  /* The preference for non short cirtcuit operation when optimizing for
     performance. The first element covers Thumb state and the second one
     is for ARM state.  */
  enum log_op_non_short_circuit {LOG_OP_NON_SHORT_CIRCUIT_FALSE,
				 LOG_OP_NON_SHORT_CIRCUIT_TRUE};
  log_op_non_short_circuit logical_op_non_short_circuit_thumb: 1;
  log_op_non_short_circuit logical_op_non_short_circuit_arm: 1;
  /* Prefer 32-bit encoding instead of flag-setting 16-bit encoding.  */
  enum {DISPARAGE_FLAGS_NEITHER, DISPARAGE_FLAGS_PARTIAL, DISPARAGE_FLAGS_ALL}
    disparage_flag_setting_t16_encodings: 2;
  enum {PREF_NEON_64_FALSE, PREF_NEON_64_TRUE} prefer_neon_for_64bits: 1;
  /* Prefer to inline string operations like memset by using Neon.  */
  enum {PREF_NEON_STRINGOPS_FALSE, PREF_NEON_STRINGOPS_TRUE}
    string_ops_prefer_neon: 1;
  /* Bitfield encoding the fusible pairs of instructions.  Use FUSE_OPS
     in an initializer if multiple fusion operations are supported on a
     target.  */
  enum fuse_ops
  {
    FUSE_NOTHING   = 0,
    FUSE_MOVW_MOVT = 1 << 0,
    FUSE_AES_AESMC = 1 << 1
  } fusible_ops: 2;
  /* Depth of scheduling queue to check for L2 autoprefetcher.  */
  enum {SCHED_AUTOPREF_OFF, SCHED_AUTOPREF_RANK, SCHED_AUTOPREF_FULL}
    sched_autopref: 2;
};

/* Smash multiple fusion operations into a type that can be used for an
   initializer.  */
#define FUSE_OPS(x) ((tune_params::fuse_ops) (x))

extern const struct tune_params *current_tune;
extern int vfp3_const_double_for_fract_bits (rtx);
/* return power of two from operand, otherwise 0.  */
extern int vfp3_const_double_for_bits (rtx);

extern void arm_emit_coreregs_64bit_shift (enum rtx_code, rtx, rtx, rtx, rtx,
					   rtx);
extern bool arm_valid_symbolic_address_p (rtx);
extern bool arm_validize_comparison (rtx *, rtx *, rtx *);
#endif /* RTX_CODE */

extern bool arm_gen_setmem (rtx *);
extern void arm_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel);
extern bool arm_expand_vec_perm_const (rtx target, rtx op0, rtx op1, rtx sel);

extern bool arm_autoinc_modes_ok_p (machine_mode, enum arm_auto_incmodes);

extern void arm_emit_eabi_attribute (const char *, int, int);

extern void arm_reset_previous_fndecl (void);
extern void save_restore_target_globals (tree);

/* Defined in gcc/common/config/arm-common.c.  */
extern const char *arm_rewrite_selected_cpu (const char *name);

/* Defined in gcc/common/config/arm-c.c.  */
extern void arm_lang_object_attributes_init (void);
extern void arm_register_target_pragmas (void);
extern void arm_cpu_cpp_builtins (struct cpp_reader *);

extern bool arm_is_constant_pool_ref (rtx);

/* Flags used to identify the presence of processor capabilities.  */

/* Bit values used to identify processor capabilities.  */
#define FL_NONE	      (0)	      /* No flags.  */
#define FL_ANY	      (0xffffffff)    /* All flags.  */
#define FL_CO_PROC    (1 << 0)        /* Has external co-processor bus */
#define FL_ARCH3M     (1 << 1)        /* Extended multiply */
#define FL_MODE26     (1 << 2)        /* 26-bit mode support */
#define FL_MODE32     (1 << 3)        /* 32-bit mode support */
#define FL_ARCH4      (1 << 4)        /* Architecture rel 4 */
#define FL_ARCH5      (1 << 5)        /* Architecture rel 5 */
#define FL_THUMB      (1 << 6)        /* Thumb aware */
#define FL_LDSCHED    (1 << 7)	      /* Load scheduling necessary */
#define FL_STRONG     (1 << 8)	      /* StrongARM */
#define FL_ARCH5E     (1 << 9)        /* DSP extensions to v5 */
#define FL_XSCALE     (1 << 10)	      /* XScale */
/* spare	      (1 << 11)	*/
#define FL_ARCH6      (1 << 12)       /* Architecture rel 6.  Adds
					 media instructions.  */
#define FL_VFPV2      (1 << 13)       /* Vector Floating Point V2.  */
#define FL_WBUF	      (1 << 14)	      /* Schedule for write buffer ops.
					 Note: ARM6 & 7 derivatives only.  */
#define FL_ARCH6K     (1 << 15)       /* Architecture rel 6 K extensions.  */
#define FL_THUMB2     (1 << 16)	      /* Thumb-2.  */
#define FL_NOTM	      (1 << 17)	      /* Instructions not present in the 'M'
					 profile.  */
#define FL_THUMB_DIV  (1 << 18)	      /* Hardware divide (Thumb mode).  */
#define FL_VFPV3      (1 << 19)       /* Vector Floating Point V3.  */
#define FL_NEON       (1 << 20)       /* Neon instructions.  */
#define FL_ARCH7EM    (1 << 21)	      /* Instructions present in the ARMv7E-M
					 architecture.  */
#define FL_ARCH7      (1 << 22)       /* Architecture 7.  */
#define FL_ARM_DIV    (1 << 23)	      /* Hardware divide (ARM mode).  */
#define FL_ARCH8      (1 << 24)       /* Architecture 8.  */
#define FL_CRC32      (1 << 25)	      /* ARMv8 CRC32 instructions.  */

#define FL_SMALLMUL   (1 << 26)       /* Small multiply supported.  */
#define FL_NO_VOLATILE_CE   (1 << 27) /* No volatile memory in IT block.  */

#define FL_IWMMXT     (1 << 29)	      /* XScale v2 or "Intel Wireless MMX technology".  */
#define FL_IWMMXT2    (1 << 30)       /* "Intel Wireless MMX2 technology".  */
#define FL_ARCH6KZ    (1 << 31)       /* ARMv6KZ architecture.  */

#define FL2_ARCH8_1   (1 << 0)	      /* Architecture 8.1.  */

/* Flags that only effect tuning, not available instructions.  */
#define FL_TUNE		(FL_WBUF | FL_VFPV2 | FL_STRONG | FL_LDSCHED \
			 | FL_CO_PROC)

#define FL_FOR_ARCH2	FL_NOTM
#define FL_FOR_ARCH3	(FL_FOR_ARCH2 | FL_MODE32)
#define FL_FOR_ARCH3M	(FL_FOR_ARCH3 | FL_ARCH3M)
#define FL_FOR_ARCH4	(FL_FOR_ARCH3M | FL_ARCH4)
#define FL_FOR_ARCH4T	(FL_FOR_ARCH4 | FL_THUMB)
#define FL_FOR_ARCH5	(FL_FOR_ARCH4 | FL_ARCH5)
#define FL_FOR_ARCH5T	(FL_FOR_ARCH5 | FL_THUMB)
#define FL_FOR_ARCH5E	(FL_FOR_ARCH5 | FL_ARCH5E)
#define FL_FOR_ARCH5TE	(FL_FOR_ARCH5E | FL_THUMB)
#define FL_FOR_ARCH5TEJ	FL_FOR_ARCH5TE
#define FL_FOR_ARCH6	(FL_FOR_ARCH5TE | FL_ARCH6)
#define FL_FOR_ARCH6J	FL_FOR_ARCH6
#define FL_FOR_ARCH6K	(FL_FOR_ARCH6 | FL_ARCH6K)
#define FL_FOR_ARCH6Z	FL_FOR_ARCH6
#define FL_FOR_ARCH6KZ	(FL_FOR_ARCH6K | FL_ARCH6KZ)
#define FL_FOR_ARCH6T2	(FL_FOR_ARCH6 | FL_THUMB2)
#define FL_FOR_ARCH6M	(FL_FOR_ARCH6 & ~FL_NOTM)
#define FL_FOR_ARCH7	((FL_FOR_ARCH6T2 & ~FL_NOTM) | FL_ARCH7)
#define FL_FOR_ARCH7A	(FL_FOR_ARCH7 | FL_NOTM | FL_ARCH6K)
#define FL_FOR_ARCH7VE	(FL_FOR_ARCH7A | FL_THUMB_DIV | FL_ARM_DIV)
#define FL_FOR_ARCH7R	(FL_FOR_ARCH7A | FL_THUMB_DIV)
#define FL_FOR_ARCH7M	(FL_FOR_ARCH7 | FL_THUMB_DIV)
#define FL_FOR_ARCH7EM  (FL_FOR_ARCH7M | FL_ARCH7EM)
#define FL_FOR_ARCH8A	(FL_FOR_ARCH7VE | FL_ARCH8)
#define FL2_FOR_ARCH8_1A	FL2_ARCH8_1

/* There are too many feature bits to fit in a single word so the set of cpu and
   fpu capabilities is a structure.  A feature set is created and manipulated
   with the ARM_FSET macros.  */

typedef struct
{
  unsigned long cpu[2];
} arm_feature_set;


/* Initialize a feature set.  */

#define ARM_FSET_MAKE(CPU1,CPU2) { { (CPU1), (CPU2) } }

#define ARM_FSET_MAKE_CPU1(CPU1) ARM_FSET_MAKE ((CPU1), (FL_NONE))
#define ARM_FSET_MAKE_CPU2(CPU2) ARM_FSET_MAKE ((FL_NONE), (CPU2))

/* Accessors.  */

#define ARM_FSET_CPU1(S) ((S).cpu[0])
#define ARM_FSET_CPU2(S) ((S).cpu[1])

/* Useful combinations.  */

#define ARM_FSET_EMPTY ARM_FSET_MAKE (FL_NONE, FL_NONE)
#define ARM_FSET_ANY ARM_FSET_MAKE (FL_ANY, FL_ANY)

/* Tests for a specific CPU feature.  */

#define ARM_FSET_HAS_CPU1(A, F)  \
  (((A).cpu[0] & ((unsigned long)(F))) == ((unsigned long)(F)))
#define ARM_FSET_HAS_CPU2(A, F)  \
  (((A).cpu[1] & ((unsigned long)(F))) == ((unsigned long)(F)))
#define ARM_FSET_HAS_CPU(A, F1, F2)				\
  (ARM_FSET_HAS_CPU1 ((A), (F1)) && ARM_FSET_HAS_CPU2 ((A), (F2)))

/* Add a feature to a feature set.  */

#define ARM_FSET_ADD_CPU1(DST, F)		\
  do {						\
    (DST).cpu[0] |= (F);			\
  } while (0)

#define ARM_FSET_ADD_CPU2(DST, F)		\
  do {						\
    (DST).cpu[1] |= (F);			\
  } while (0)

/* Remove a feature from a feature set.  */

#define ARM_FSET_DEL_CPU1(DST, F)		\
  do {						\
    (DST).cpu[0] &= ~(F);			\
  } while (0)

#define ARM_FSET_DEL_CPU2(DST, F)		\
  do {						\
    (DST).cpu[1] &= ~(F);			\
  } while (0)

/* Union of feature sets.  */

#define ARM_FSET_UNION(DST,F1,F2)		\
  do {						\
    (DST).cpu[0] = (F1).cpu[0] | (F2).cpu[0];	\
    (DST).cpu[1] = (F1).cpu[1] | (F2).cpu[1];	\
  } while (0)

/* Intersection of feature sets.  */

#define ARM_FSET_INTER(DST,F1,F2)		\
  do {						\
    (DST).cpu[0] = (F1).cpu[0] & (F2).cpu[0];	\
    (DST).cpu[1] = (F1).cpu[1] & (F2).cpu[1];	\
  } while (0)

/* Exclusive disjunction.  */

#define ARM_FSET_XOR(DST,F1,F2)				\
  do {							\
    (DST).cpu[0] = (F1).cpu[0] ^ (F2).cpu[0];		\
    (DST).cpu[1] = (F1).cpu[1] ^ (F2).cpu[1];		\
  } while (0)

/* Difference of feature sets: F1 excluding the elements of F2.  */

#define ARM_FSET_EXCLUDE(DST,F1,F2)		\
  do {						\
    (DST).cpu[0] = (F1).cpu[0] & ~(F2).cpu[0];	\
    (DST).cpu[1] = (F1).cpu[1] & ~(F2).cpu[1];	\
  } while (0)

/* Test for an empty feature set.  */

#define ARM_FSET_IS_EMPTY(A)		\
  (!((A).cpu[0]) && !((A).cpu[1]))

/* Tests whether the cpu features of A are a subset of B.  */

#define ARM_FSET_CPU_SUBSET(A,B)					\
  ((((A).cpu[0] & (B).cpu[0]) == (A).cpu[0])				\
   && (((A).cpu[1] & (B).cpu[1]) == (A).cpu[1]))

/* The bits in this mask specify which
   instructions we are allowed to generate.  */
extern arm_feature_set insn_flags;

/* The bits in this mask specify which instruction scheduling options should
   be used.  */
extern arm_feature_set tune_flags;

/* Nonzero if this chip supports the ARM Architecture 3M extensions.  */
extern int arm_arch3m;

/* Nonzero if this chip supports the ARM Architecture 4 extensions.  */
extern int arm_arch4;

/* Nonzero if this chip supports the ARM Architecture 4t extensions.  */
extern int arm_arch4t;

/* Nonzero if this chip supports the ARM Architecture 5 extensions.  */
extern int arm_arch5;

/* Nonzero if this chip supports the ARM Architecture 5E extensions.  */
extern int arm_arch5e;

/* Nonzero if this chip supports the ARM Architecture 6 extensions.  */
extern int arm_arch6;

/* Nonzero if this chip supports the ARM 6K extensions.  */
extern int arm_arch6k;

/* Nonzero if this chip supports the ARM 6KZ extensions.  */
extern int arm_arch6kz;

/* Nonzero if instructions present in ARMv6-M can be used.  */
extern int arm_arch6m;

/* Nonzero if this chip supports the ARM 7 extensions.  */
extern int arm_arch7;

/* Nonzero if instructions not present in the 'M' profile can be used.  */
extern int arm_arch_notm;

/* Nonzero if instructions present in ARMv7E-M can be used.  */
extern int arm_arch7em;

/* Nonzero if instructions present in ARMv8 can be used.  */
extern int arm_arch8;

/* Nonzero if this chip can benefit from load scheduling.  */
extern int arm_ld_sched;

/* Nonzero if this chip is a StrongARM.  */
extern int arm_tune_strongarm;

/* Nonzero if this chip supports Intel Wireless MMX technology.  */
extern int arm_arch_iwmmxt;

/* Nonzero if this chip supports Intel Wireless MMX2 technology.  */
extern int arm_arch_iwmmxt2;

/* Nonzero if this chip is an XScale.  */
extern int arm_arch_xscale;

/* Nonzero if tuning for XScale  */
extern int arm_tune_xscale;

/* Nonzero if we want to tune for stores that access the write-buffer.
   This typically means an ARM6 or ARM7 with MMU or MPU.  */
extern int arm_tune_wbuf;

/* Nonzero if tuning for Cortex-A9.  */
extern int arm_tune_cortex_a9;

/* Nonzero if we should define __THUMB_INTERWORK__ in the
   preprocessor.
   XXX This is a bit of a hack, it's intended to help work around
   problems in GLD which doesn't understand that armv5t code is
   interworking clean.  */
extern int arm_cpp_interwork;

/* Nonzero if chip supports Thumb 2.  */
extern int arm_arch_thumb2;

/* Nonzero if chip supports integer division instruction.  */
extern int arm_arch_arm_hwdiv;
extern int arm_arch_thumb_hwdiv;

/* Nonzero if chip disallows volatile memory access in IT block.  */
extern int arm_arch_no_volatile_ce;

/* Nonzero if we should use Neon to handle 64-bits operations rather
   than core registers.  */
extern int prefer_neon_for_64bits;



#endif /* ! GCC_ARM_PROTOS_H */
