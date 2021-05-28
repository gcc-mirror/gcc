/* Prototypes for exported functions defined in arm.c and pe.c
   Copyright (C) 1999-2021 Free Software Foundation, Inc.
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

#include "sbitmap.h"

extern enum unwind_info_type arm_except_unwind_info (struct gcc_options *);
extern int use_return_insn (int, rtx);
extern bool use_simple_return_p (void);
extern enum reg_class arm_regno_class (int);
extern bool arm_check_builtin_call (location_t , vec<location_t> , tree,
				    tree, unsigned int, tree *);
extern void arm_load_pic_register (unsigned long, rtx);
extern int arm_volatile_func (void);
extern void arm_expand_prologue (void);
extern void arm_expand_epilogue (bool);
extern void arm_declare_function_name (FILE *, const char *, tree);
extern void arm_asm_declare_function_name (FILE *, const char *, tree);
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
			       ATTRIBUTE_UNUSED, machine_mode mode
			       ATTRIBUTE_UNUSED, int ignore ATTRIBUTE_UNUSED);
extern tree arm_builtin_decl (unsigned code, bool initialize_p
			      ATTRIBUTE_UNUSED);
extern void arm_init_builtins (void);
extern void arm_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update);
extern rtx arm_simd_vect_par_cnst_half (machine_mode mode, bool high);
extern bool arm_simd_check_vect_par_cnst_half_p (rtx op, machine_mode mode,
						 bool high);
extern void arm_emit_speculation_barrier_function (void);
extern void arm_decompose_di_binop (rtx, rtx, rtx *, rtx *, rtx *, rtx *);
extern bool arm_q_bit_access (void);
extern bool arm_ge_bits_access (void);
extern bool arm_target_insn_ok_for_lob (rtx);

#ifdef RTX_CODE
enum reg_class
arm_mode_base_reg_class (machine_mode);
extern void arm_gen_unlikely_cbranch (enum rtx_code, machine_mode cc_mode,
				      rtx label_ref);
extern bool arm_vector_mode_supported_p (machine_mode);
extern bool arm_small_register_classes_for_mode_p (machine_mode);
extern int const_ok_for_arm (HOST_WIDE_INT);
extern int const_ok_for_op (HOST_WIDE_INT, enum rtx_code);
extern int const_ok_for_dimode_op (HOST_WIDE_INT, enum rtx_code);
extern void thumb1_gen_const_int_rtl (rtx, HOST_WIDE_INT);
extern void thumb1_gen_const_int_print (rtx, HOST_WIDE_INT);
extern int arm_split_constant (RTX_CODE, machine_mode, rtx,
			       HOST_WIDE_INT, rtx, rtx, int);
extern int legitimate_pic_operand_p (rtx);
extern rtx legitimize_pic_address (rtx, machine_mode, rtx, rtx, bool);
extern rtx legitimize_tls_address (rtx, rtx);
extern bool arm_legitimate_address_p (machine_mode, rtx, bool);
extern int arm_legitimate_address_outer_p (machine_mode, rtx, RTX_CODE, int);
extern int thumb_legitimate_offset_p (machine_mode, HOST_WIDE_INT);
extern int thumb1_legitimate_address_p (machine_mode, rtx, int);
extern bool ldm_stm_operation_p (rtx, bool, machine_mode mode,
                                 bool, bool);
extern bool clear_operation_p (rtx, bool);
extern int arm_const_double_rtx (rtx);
extern int vfp3_const_double_rtx (rtx);
extern int simd_immediate_valid_for_move (rtx, machine_mode, rtx *, int *);
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
extern rtx neon_make_constant (rtx, bool generate = true);
extern tree arm_builtin_vectorized_function (unsigned int, tree, tree);
extern void neon_expand_vector_init (rtx, rtx);
extern void neon_lane_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT, const_tree);
extern void arm_const_bounds (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
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
extern int arm_coproc_mem_operand_no_writeback (rtx);
extern int arm_coproc_mem_operand_wb (rtx, int);
extern int neon_vector_mem_operand (rtx, int, bool);
extern int mve_vector_mem_operand (machine_mode, rtx, bool);
extern int neon_struct_mem_operand (rtx);

extern rtx *neon_vcmla_lane_prepare_operands (rtx *);

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
extern bool valid_operands_ldrd_strd (rtx *, bool);
extern int arm_gen_cpymemqi (rtx *);
extern bool gen_cpymem_ldrd_strd (rtx *);
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
extern rtx arm_load_function_descriptor (rtx funcdesc);
extern void arm_emit_call_insn (rtx, rtx, bool);
bool detect_cmse_nonsecure_call (tree);
extern const char *output_call (rtx *);
void arm_emit_movpair (rtx, rtx);
extern const char *output_mov_long_double_arm_from_arm (rtx *);
extern const char *output_move_double (rtx *, bool, int *count);
extern const char *output_move_quad (rtx *);
extern int arm_count_output_move_double_insns (rtx *);
extern int arm_count_ldrdstrd_insns (rtx *, bool);
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
extern int arm_attr_length_pop_multi(rtx *, bool, bool);
extern void arm_expand_compare_and_swap (rtx op[]);
extern void arm_split_compare_and_swap (rtx op[]);
extern void arm_split_atomic_op (enum rtx_code, rtx, rtx, rtx, rtx, rtx, rtx);
extern rtx arm_load_tp (rtx);
extern bool arm_coproc_builtin_available (enum unspecv);
extern bool arm_coproc_ldc_stc_legitimate_address (rtx);

#if defined TREE_CODE
extern void arm_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree);
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
extern void thumb_expand_cpymemqi (rtx *);
extern rtx arm_return_addr (int, rtx);
extern void thumb_reload_out_hi (rtx *);
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
extern void arm_configure_build_target (struct arm_build_target *,
					struct cl_target_option *, bool);
extern void arm_option_reconfigure_globals (void);
extern void arm_options_perform_arch_sanity_checks (void);
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

/* Addressing mode operations.  Used to index tables in struct
   addr_mode_cost_table.  */
enum arm_addr_mode_op
{
   AMO_DEFAULT,
   AMO_NO_WB,	/* Offset with no writeback.  */
   AMO_WB,	/* Offset with writeback.  */
   AMO_MAX	/* For array size.  */
};

/* Table of additional costs in units of COSTS_N_INSNS() when using
   addressing modes for each access type.  */
struct addr_mode_cost_table
{
   const int integer[AMO_MAX];
   const int fp[AMO_MAX];
   const int vector[AMO_MAX];
};

/* Dump function ARM_PRINT_TUNE_INFO should be updated whenever this
   structure is modified.  */

struct tune_params
{
  const struct cpu_cost_table *insn_extra_cost;
  const struct addr_mode_cost_table *addr_mode_costs;
  bool (*sched_adjust_cost) (rtx_insn *, int, rtx_insn *, int *);
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
extern bool arm_fusion_enabled_p (tune_params::fuse_ops);
extern bool arm_valid_symbolic_address_p (rtx);
extern bool arm_validize_comparison (rtx *, rtx *, rtx *);
extern bool arm_expand_vector_compare (rtx, rtx_code, rtx, rtx, bool);
#endif /* RTX_CODE */

extern bool arm_gen_setmem (rtx *);
extern void arm_expand_vcond (rtx *, machine_mode);
extern void arm_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel);

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

/* Defined in arm-d.c  */
extern void arm_d_target_versions (void);
extern void arm_d_register_target_info (void);

extern bool arm_is_constant_pool_ref (rtx);

/* The bits in this mask specify which instruction scheduling options should
   be used.  */
extern unsigned int tune_flags;

/* Nonzero if this chip supports the ARM Architecture 4 extensions.  */
extern int arm_arch4;

/* Nonzero if this chip supports the ARM Architecture 4t extensions.  */
extern int arm_arch4t;

/* Nonzero if this chip supports the ARM Architecture 5t extensions.  */
extern int arm_arch5t;

/* Nonzero if this chip supports the ARM Architecture 5te extensions.  */
extern int arm_arch5te;

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

/* Nonzero if this chip supports the Large Physical Address Extension.  */
extern int arm_arch_lpae;

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

/* Nonzero if chip supports Thumb 1.  */
extern int arm_arch_thumb1;

/* Nonzero if chip supports Thumb 2.  */
extern int arm_arch_thumb2;

/* Nonzero if chip supports integer division instruction.  */
extern int arm_arch_arm_hwdiv;
extern int arm_arch_thumb_hwdiv;

/* Nonzero if chip disallows volatile memory access in IT block.  */
extern int arm_arch_no_volatile_ce;

/* Structure defining the current overall architectural target and tuning.  */
struct arm_build_target
{
  /* Name of the target CPU, if known, or NULL if the target CPU was not
     specified by the user (and inferred from the -march option).  */
  const char *core_name;
  /* Name of the target ARCH.  NULL if there is a selected CPU.  */
  const char *arch_name;
  /* Preprocessor substring (never NULL).  */
  const char *arch_pp_name;
  /* The base architecture value.  */
  enum base_architecture base_arch;
  /* The profile letter for the architecture, upper case by convention.  */
  char profile;
  /* Bitmap encapsulating the isa_bits for the target environment.  */
  sbitmap isa;
  /* Flags used for tuning.  Long term, these move into tune_params.  */
  unsigned int tune_flags;
  /* Tables with more detailed tuning information.  */
  const struct tune_params *tune;
  /* CPU identifier for the tuning target.  */
  enum processor_type tune_core;
};

extern struct arm_build_target arm_active_target;

/* Table entry for a CPU alias.  */
struct cpu_alias
{
  /* The alias name.  */
  const char *const name;
  /* True if the name should be displayed in help text listing cpu names.  */
  bool visible;
};

/* Table entry for an architectural feature extension.  */
struct cpu_arch_extension
{
  /* Feature name.  */
  const char *const name;
  /* True if the option is negative (removes extensions).  */
  bool remove;
  /* True if the option is an alias for another option with identical effect;
     the option will be ignored for canonicalization.  */
  bool alias;
  /* The modifier bits.  */
  const enum isa_feature isa_bits[isa_num_bits];
};

/* Common elements of both CPU and architectural options.  */
struct cpu_arch_option
{
  /* Name for this option.  */
  const char *name;
  /* List of feature extensions permitted.  */
  const struct cpu_arch_extension *extensions;
  /* Standard feature bits.  */
  enum isa_feature isa_bits[isa_num_bits];
};

/* Table entry for an architecture entry.  */
struct arch_option
{
  /* Common option fields.  */
  cpu_arch_option common;
  /* Short string for this architecture.  */
  const char *arch;
  /* Base architecture, from which this specific architecture is derived.  */
  enum base_architecture base_arch;
  /* The profile letter for the architecture, upper case by convention.  */
  const char profile;
  /* Default tune target (in the absence of any more specific data).  */
  enum processor_type tune_id;
};

/* Table entry for a CPU entry.  */
struct cpu_option
{
  /* Common option fields.  */
  cpu_arch_option common;
  /* List of aliases for this CPU.  */
  const struct cpu_alias *aliases;
  /* Architecture upon which this CPU is based.  */
  enum arch_type arch;
};

extern const arch_option all_architectures[];
extern const cpu_option all_cores[];

const cpu_option *arm_parse_cpu_option_name (const cpu_option *, const char *,
					     const char *, bool = true);
const arch_option *arm_parse_arch_option_name (const arch_option *,
					       const char *, const char *, bool = true);
void arm_parse_option_features (sbitmap, const cpu_arch_option *,
				const char *);

void arm_initialize_isa (sbitmap, const enum isa_feature *);

const char * arm_gen_far_branch (rtx *, int, const char * , const char *);

bool arm_mve_immediate_check(rtx, machine_mode, bool);
#endif /* ! GCC_ARM_PROTOS_H */
