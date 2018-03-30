/* Output routines for GCC for ARM.
   Copyright (C) 1991-2018 Free Software Foundation, Inc.
   Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
   and Martin Simmons (@harleqn.co.uk).
   More major hacks by Richard Earnshaw (rearnsha@arm.com).

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

#define IN_TARGET_CODE 1

#include "config.h"
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "cfghooks.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "explow.h"
#include "expr.h"
#include "cfgrtl.h"
#include "sched-int.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "intl.h"
#include "libfuncs.h"
#include "params.h"
#include "opts.h"
#include "dumpfile.h"
#include "target-globals.h"
#include "builtins.h"
#include "tm-constrs.h"
#include "rtl-iter.h"
#include "optabs-libfuncs.h"
#include "gimplify.h"
#include "gimple.h"
#include "selftest.h"

/* This file should be included last.  */
#include "target-def.h"

/* Forward definitions of types.  */
typedef struct minipool_node    Mnode;
typedef struct minipool_fixup   Mfix;

/* The last .arch and .fpu assembly strings that we printed.  */
static std::string arm_last_printed_arch_string;
static std::string arm_last_printed_fpu_string;

void (*arm_lang_output_object_attributes_hook)(void);

struct four_ints
{
  int i[4];
};

/* Forward function declarations.  */
static bool arm_const_not_ok_for_debug_p (rtx);
static int arm_needs_doubleword_align (machine_mode, const_tree);
static int arm_compute_static_chain_stack_bytes (void);
static arm_stack_offsets *arm_get_frame_offsets (void);
static void arm_compute_frame_layout (void);
static void arm_add_gc_roots (void);
static int arm_gen_constant (enum rtx_code, machine_mode, rtx,
			     unsigned HOST_WIDE_INT, rtx, rtx, int, int);
static unsigned bit_count (unsigned long);
static unsigned bitmap_popcount (const sbitmap);
static int arm_address_register_rtx_p (rtx, int);
static int arm_legitimate_index_p (machine_mode, rtx, RTX_CODE, int);
static bool is_called_in_ARM_mode (tree);
static int thumb2_legitimate_index_p (machine_mode, rtx, int);
static int thumb1_base_register_rtx_p (rtx, machine_mode, int);
static rtx arm_legitimize_address (rtx, rtx, machine_mode);
static reg_class_t arm_preferred_reload_class (rtx, reg_class_t);
static rtx thumb_legitimize_address (rtx, rtx, machine_mode);
inline static int thumb1_index_register_rtx_p (rtx, int);
static int thumb_far_jump_used_p (void);
static bool thumb_force_lr_save (void);
static unsigned arm_size_return_regs (void);
static bool arm_assemble_integer (rtx, unsigned int, int);
static void arm_print_operand (FILE *, rtx, int);
static void arm_print_operand_address (FILE *, machine_mode, rtx);
static bool arm_print_operand_punct_valid_p (unsigned char code);
static const char *fp_const_from_val (REAL_VALUE_TYPE *);
static arm_cc get_arm_condition_code (rtx);
static bool arm_fixed_condition_code_regs (unsigned int *, unsigned int *);
static const char *output_multi_immediate (rtx *, const char *, const char *,
					   int, HOST_WIDE_INT);
static const char *shift_op (rtx, HOST_WIDE_INT *);
static struct machine_function *arm_init_machine_status (void);
static void thumb_exit (FILE *, int);
static HOST_WIDE_INT get_jump_table_size (rtx_jump_table_data *);
static Mnode *move_minipool_fix_forward_ref (Mnode *, Mnode *, HOST_WIDE_INT);
static Mnode *add_minipool_forward_ref (Mfix *);
static Mnode *move_minipool_fix_backward_ref (Mnode *, Mnode *, HOST_WIDE_INT);
static Mnode *add_minipool_backward_ref (Mfix *);
static void assign_minipool_offsets (Mfix *);
static void arm_print_value (FILE *, rtx);
static void dump_minipool (rtx_insn *);
static int arm_barrier_cost (rtx_insn *);
static Mfix *create_fix_barrier (Mfix *, HOST_WIDE_INT);
static void push_minipool_barrier (rtx_insn *, HOST_WIDE_INT);
static void push_minipool_fix (rtx_insn *, HOST_WIDE_INT, rtx *,
			       machine_mode, rtx);
static void arm_reorg (void);
static void note_invalid_constants (rtx_insn *, HOST_WIDE_INT, int);
static unsigned long arm_compute_save_reg0_reg12_mask (void);
static unsigned long arm_compute_save_core_reg_mask (void);
static unsigned long arm_isr_value (tree);
static unsigned long arm_compute_func_type (void);
static tree arm_handle_fndecl_attribute (tree *, tree, tree, int, bool *);
static tree arm_handle_pcs_attribute (tree *, tree, tree, int, bool *);
static tree arm_handle_isr_attribute (tree *, tree, tree, int, bool *);
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
static tree arm_handle_notshared_attribute (tree *, tree, tree, int, bool *);
#endif
static tree arm_handle_cmse_nonsecure_entry (tree *, tree, tree, int, bool *);
static tree arm_handle_cmse_nonsecure_call (tree *, tree, tree, int, bool *);
static void arm_output_function_epilogue (FILE *);
static void arm_output_function_prologue (FILE *);
static int arm_comp_type_attributes (const_tree, const_tree);
static void arm_set_default_type_attributes (tree);
static int arm_adjust_cost (rtx_insn *, int, rtx_insn *, int, unsigned int);
static int arm_sched_reorder (FILE *, int, rtx_insn **, int *, int);
static int optimal_immediate_sequence (enum rtx_code code,
				       unsigned HOST_WIDE_INT val,
				       struct four_ints *return_sequence);
static int optimal_immediate_sequence_1 (enum rtx_code code,
					 unsigned HOST_WIDE_INT val,
					 struct four_ints *return_sequence,
					 int i);
static int arm_get_strip_length (int);
static bool arm_function_ok_for_sibcall (tree, tree);
static machine_mode arm_promote_function_mode (const_tree,
						    machine_mode, int *,
						    const_tree, int);
static bool arm_return_in_memory (const_tree, const_tree);
static rtx arm_function_value (const_tree, const_tree, bool);
static rtx arm_libcall_value_1 (machine_mode);
static rtx arm_libcall_value (machine_mode, const_rtx);
static bool arm_function_value_regno_p (const unsigned int);
static void arm_internal_label (FILE *, const char *, unsigned long);
static void arm_output_mi_thunk (FILE *, tree, HOST_WIDE_INT, HOST_WIDE_INT,
				 tree);
static bool arm_have_conditional_execution (void);
static bool arm_cannot_force_const_mem (machine_mode, rtx);
static bool arm_legitimate_constant_p (machine_mode, rtx);
static bool arm_rtx_costs (rtx, machine_mode, int, int, int *, bool);
static int arm_address_cost (rtx, machine_mode, addr_space_t, bool);
static int arm_register_move_cost (machine_mode, reg_class_t, reg_class_t);
static int arm_memory_move_cost (machine_mode, reg_class_t, bool);
static void emit_constant_insn (rtx cond, rtx pattern);
static rtx_insn *emit_set_insn (rtx, rtx);
static rtx emit_multi_reg_push (unsigned long, unsigned long);
static int arm_arg_partial_bytes (cumulative_args_t, machine_mode,
				  tree, bool);
static rtx arm_function_arg (cumulative_args_t, machine_mode,
			     const_tree, bool);
static void arm_function_arg_advance (cumulative_args_t, machine_mode,
				      const_tree, bool);
static pad_direction arm_function_arg_padding (machine_mode, const_tree);
static unsigned int arm_function_arg_boundary (machine_mode, const_tree);
static rtx aapcs_allocate_return_reg (machine_mode, const_tree,
				      const_tree);
static rtx aapcs_libcall_value (machine_mode);
static int aapcs_select_return_coproc (const_tree, const_tree);

#ifdef OBJECT_FORMAT_ELF
static void arm_elf_asm_constructor (rtx, int) ATTRIBUTE_UNUSED;
static void arm_elf_asm_destructor (rtx, int) ATTRIBUTE_UNUSED;
#endif
#ifndef ARM_PE
static void arm_encode_section_info (tree, rtx, int);
#endif

static void arm_file_end (void);
static void arm_file_start (void);
static void arm_insert_attributes (tree, tree *);

static void arm_setup_incoming_varargs (cumulative_args_t, machine_mode,
					tree, int *, int);
static bool arm_pass_by_reference (cumulative_args_t,
				   machine_mode, const_tree, bool);
static bool arm_promote_prototypes (const_tree);
static bool arm_default_short_enums (void);
static bool arm_align_anon_bitfield (void);
static bool arm_return_in_msb (const_tree);
static bool arm_must_pass_in_stack (machine_mode, const_tree);
static bool arm_return_in_memory (const_tree, const_tree);
#if ARM_UNWIND_INFO
static void arm_unwind_emit (FILE *, rtx_insn *);
static bool arm_output_ttype (rtx);
static void arm_asm_emit_except_personality (rtx);
#endif
static void arm_asm_init_sections (void);
static rtx arm_dwarf_register_span (rtx);

static tree arm_cxx_guard_type (void);
static bool arm_cxx_guard_mask_bit (void);
static tree arm_get_cookie_size (tree);
static bool arm_cookie_has_size (void);
static bool arm_cxx_cdtor_returns_this (void);
static bool arm_cxx_key_method_may_be_inline (void);
static void arm_cxx_determine_class_data_visibility (tree);
static bool arm_cxx_class_data_always_comdat (void);
static bool arm_cxx_use_aeabi_atexit (void);
static void arm_init_libfuncs (void);
static tree arm_build_builtin_va_list (void);
static void arm_expand_builtin_va_start (tree, rtx);
static tree arm_gimplify_va_arg_expr (tree, tree, gimple_seq *, gimple_seq *);
static void arm_option_override (void);
static void arm_option_save (struct cl_target_option *, struct gcc_options *);
static void arm_option_restore (struct gcc_options *,
				struct cl_target_option *);
static void arm_override_options_after_change (void);
static void arm_option_print (FILE *, int, struct cl_target_option *);
static void arm_set_current_function (tree);
static bool arm_can_inline_p (tree, tree);
static void arm_relayout_function (tree);
static bool arm_valid_target_attribute_p (tree, tree, tree, int);
static unsigned HOST_WIDE_INT arm_shift_truncation_mask (machine_mode);
static bool arm_sched_can_speculate_insn (rtx_insn *);
static bool arm_macro_fusion_p (void);
static bool arm_cannot_copy_insn_p (rtx_insn *);
static int arm_issue_rate (void);
static int arm_first_cycle_multipass_dfa_lookahead (void);
static int arm_first_cycle_multipass_dfa_lookahead_guard (rtx_insn *, int);
static void arm_output_dwarf_dtprel (FILE *, int, rtx) ATTRIBUTE_UNUSED;
static bool arm_output_addr_const_extra (FILE *, rtx);
static bool arm_allocate_stack_slots_for_args (void);
static bool arm_warn_func_return (tree);
static tree arm_promoted_type (const_tree t);
static bool arm_scalar_mode_supported_p (scalar_mode);
static bool arm_frame_pointer_required (void);
static bool arm_can_eliminate (const int, const int);
static void arm_asm_trampoline_template (FILE *);
static void arm_trampoline_init (rtx, tree, rtx);
static rtx arm_trampoline_adjust_address (rtx);
static rtx_insn *arm_pic_static_addr (rtx orig, rtx reg);
static bool cortex_a9_sched_adjust_cost (rtx_insn *, int, rtx_insn *, int *);
static bool xscale_sched_adjust_cost (rtx_insn *, int, rtx_insn *, int *);
static bool fa726te_sched_adjust_cost (rtx_insn *, int, rtx_insn *, int *);
static bool arm_array_mode_supported_p (machine_mode,
					unsigned HOST_WIDE_INT);
static machine_mode arm_preferred_simd_mode (scalar_mode);
static bool arm_class_likely_spilled_p (reg_class_t);
static HOST_WIDE_INT arm_vector_alignment (const_tree type);
static bool arm_vector_alignment_reachable (const_tree type, bool is_packed);
static bool arm_builtin_support_vector_misalignment (machine_mode mode,
						     const_tree type,
						     int misalignment,
						     bool is_packed);
static void arm_conditional_register_usage (void);
static enum flt_eval_method arm_excess_precision (enum excess_precision_type);
static reg_class_t arm_preferred_rename_class (reg_class_t rclass);
static void arm_autovectorize_vector_sizes (vector_sizes *);
static int arm_default_branch_cost (bool, bool);
static int arm_cortex_a5_branch_cost (bool, bool);
static int arm_cortex_m_branch_cost (bool, bool);
static int arm_cortex_m7_branch_cost (bool, bool);

static bool arm_vectorize_vec_perm_const (machine_mode, rtx, rtx, rtx,
					  const vec_perm_indices &);

static bool aarch_macro_fusion_pair_p (rtx_insn*, rtx_insn*);

static int arm_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
					   tree vectype,
					   int misalign ATTRIBUTE_UNUSED);
static unsigned arm_add_stmt_cost (void *data, int count,
				   enum vect_cost_for_stmt kind,
				   struct _stmt_vec_info *stmt_info,
				   int misalign,
				   enum vect_cost_model_location where);

static void arm_canonicalize_comparison (int *code, rtx *op0, rtx *op1,
					 bool op0_preserve_value);
static unsigned HOST_WIDE_INT arm_asan_shadow_offset (void);

static void arm_sched_fusion_priority (rtx_insn *, int, int *, int*);
static bool arm_can_output_mi_thunk (const_tree, HOST_WIDE_INT, HOST_WIDE_INT,
				     const_tree);
static section *arm_function_section (tree, enum node_frequency, bool, bool);
static bool arm_asm_elf_flags_numeric (unsigned int flags, unsigned int *num);
static unsigned int arm_elf_section_type_flags (tree decl, const char *name,
						int reloc);
static void arm_expand_divmod_libfunc (rtx, machine_mode, rtx, rtx, rtx *, rtx *);
static opt_scalar_float_mode arm_floatn_mode (int, bool);
static unsigned int arm_hard_regno_nregs (unsigned int, machine_mode);
static bool arm_hard_regno_mode_ok (unsigned int, machine_mode);
static bool arm_modes_tieable_p (machine_mode, machine_mode);
static HOST_WIDE_INT arm_constant_alignment (const_tree, HOST_WIDE_INT);

/* Table of machine attributes.  */
static const struct attribute_spec arm_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  /* Function calls made to this symbol must be done indirectly, because
     it may lie outside of the 26 bit addressing range of a normal function
     call.  */
  { "long_call",    0, 0, false, true,  true,  false, NULL, NULL },
  /* Whereas these functions are always known to reside within the 26 bit
     addressing range.  */
  { "short_call",   0, 0, false, true,  true,  false, NULL, NULL },
  /* Specify the procedure call conventions for a function.  */
  { "pcs",          1, 1, false, true,  true,  false, arm_handle_pcs_attribute,
    NULL },
  /* Interrupt Service Routines have special prologue and epilogue requirements.  */
  { "isr",          0, 1, false, false, false, false, arm_handle_isr_attribute,
    NULL },
  { "interrupt",    0, 1, false, false, false, false, arm_handle_isr_attribute,
    NULL },
  { "naked",        0, 0, true,  false, false, false,
    arm_handle_fndecl_attribute, NULL },
#ifdef ARM_PE
  /* ARM/PE has three new attributes:
     interfacearm - ?
     dllexport - for exporting a function/variable that will live in a dll
     dllimport - for importing a function/variable from a dll

     Microsoft allows multiple declspecs in one __declspec, separating
     them with spaces.  We do NOT support this.  Instead, use __declspec
     multiple times.
  */
  { "dllimport",    0, 0, true,  false, false, false, NULL, NULL },
  { "dllexport",    0, 0, true,  false, false, false, NULL, NULL },
  { "interfacearm", 0, 0, true,  false, false, false,
    arm_handle_fndecl_attribute, NULL },
#elif TARGET_DLLIMPORT_DECL_ATTRIBUTES
  { "dllimport",    0, 0, false, false, false, false, handle_dll_attribute,
    NULL },
  { "dllexport",    0, 0, false, false, false, false, handle_dll_attribute,
    NULL },
  { "notshared",    0, 0, false, true, false, false,
    arm_handle_notshared_attribute, NULL },
#endif
  /* ARMv8-M Security Extensions support.  */
  { "cmse_nonsecure_entry", 0, 0, true, false, false, false,
    arm_handle_cmse_nonsecure_entry, NULL },
  { "cmse_nonsecure_call", 0, 0, true, false, false, true,
    arm_handle_cmse_nonsecure_call, NULL },
  { NULL, 0, 0, false, false, false, false, NULL, NULL }
};

/* Initialize the GCC target structure.  */
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
#undef  TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES merge_dllimport_decl_attributes
#endif

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS arm_legitimize_address

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE arm_attribute_table

#undef  TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES arm_insert_attributes

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START arm_file_start
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END arm_file_end

#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER arm_assemble_integer

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND arm_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS arm_print_operand_address
#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P arm_print_operand_punct_valid_p

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA arm_output_addr_const_extra

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE arm_output_function_prologue

#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE arm_output_function_epilogue

#undef TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P arm_can_inline_p

#undef TARGET_RELAYOUT_FUNCTION
#define TARGET_RELAYOUT_FUNCTION arm_relayout_function

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE arm_option_override

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE arm_override_options_after_change

#undef TARGET_OPTION_SAVE
#define TARGET_OPTION_SAVE arm_option_save

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE arm_option_restore

#undef TARGET_OPTION_PRINT
#define TARGET_OPTION_PRINT arm_option_print

#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES arm_comp_type_attributes

#undef TARGET_SCHED_CAN_SPECULATE_INSN
#define TARGET_SCHED_CAN_SPECULATE_INSN arm_sched_can_speculate_insn

#undef TARGET_SCHED_MACRO_FUSION_P
#define TARGET_SCHED_MACRO_FUSION_P arm_macro_fusion_p

#undef TARGET_SCHED_MACRO_FUSION_PAIR_P
#define TARGET_SCHED_MACRO_FUSION_PAIR_P aarch_macro_fusion_pair_p

#undef  TARGET_SET_DEFAULT_TYPE_ATTRIBUTES
#define TARGET_SET_DEFAULT_TYPE_ATTRIBUTES arm_set_default_type_attributes

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST arm_adjust_cost

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION arm_set_current_function

#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P arm_valid_target_attribute_p

#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER arm_sched_reorder

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST arm_register_move_cost

#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST arm_memory_move_cost

#undef TARGET_ENCODE_SECTION_INFO
#ifdef ARM_PE
#define TARGET_ENCODE_SECTION_INFO  arm_pe_encode_section_info
#else
#define TARGET_ENCODE_SECTION_INFO  arm_encode_section_info
#endif

#undef  TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING arm_strip_name_encoding

#undef  TARGET_ASM_INTERNAL_LABEL
#define TARGET_ASM_INTERNAL_LABEL arm_internal_label

#undef TARGET_FLOATN_MODE
#define TARGET_FLOATN_MODE arm_floatn_mode

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL arm_function_ok_for_sibcall

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE arm_function_value

#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE arm_libcall_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P arm_function_value_regno_p

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK arm_output_mi_thunk
#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK arm_can_output_mi_thunk

#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS arm_rtx_costs
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST arm_address_cost

#undef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK arm_shift_truncation_mask
#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P arm_vector_mode_supported_p
#undef TARGET_ARRAY_MODE_SUPPORTED_P
#define TARGET_ARRAY_MODE_SUPPORTED_P arm_array_mode_supported_p
#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE arm_preferred_simd_mode
#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_SIZES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_SIZES \
  arm_autovectorize_vector_sizes

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG arm_reorg

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  arm_init_builtins
#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN arm_expand_builtin
#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL arm_builtin_decl

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS arm_init_libfuncs

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE arm_promote_function_mode
#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES arm_promote_prototypes
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE arm_pass_by_reference
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES arm_arg_partial_bytes
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG arm_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE arm_function_arg_advance
#undef TARGET_FUNCTION_ARG_PADDING
#define TARGET_FUNCTION_ARG_PADDING arm_function_arg_padding
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY arm_function_arg_boundary

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS arm_setup_incoming_varargs

#undef TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS arm_allocate_stack_slots_for_args

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE arm_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT arm_trampoline_init
#undef TARGET_TRAMPOLINE_ADJUST_ADDRESS
#define TARGET_TRAMPOLINE_ADJUST_ADDRESS arm_trampoline_adjust_address

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN arm_warn_func_return

#undef TARGET_DEFAULT_SHORT_ENUMS
#define TARGET_DEFAULT_SHORT_ENUMS arm_default_short_enums

#undef TARGET_ALIGN_ANON_BITFIELD
#define TARGET_ALIGN_ANON_BITFIELD arm_align_anon_bitfield

#undef TARGET_NARROW_VOLATILE_BITFIELD
#define TARGET_NARROW_VOLATILE_BITFIELD hook_bool_void_false

#undef TARGET_CXX_GUARD_TYPE
#define TARGET_CXX_GUARD_TYPE arm_cxx_guard_type

#undef TARGET_CXX_GUARD_MASK_BIT
#define TARGET_CXX_GUARD_MASK_BIT arm_cxx_guard_mask_bit

#undef TARGET_CXX_GET_COOKIE_SIZE
#define TARGET_CXX_GET_COOKIE_SIZE arm_get_cookie_size

#undef TARGET_CXX_COOKIE_HAS_SIZE
#define TARGET_CXX_COOKIE_HAS_SIZE arm_cookie_has_size

#undef TARGET_CXX_CDTOR_RETURNS_THIS
#define TARGET_CXX_CDTOR_RETURNS_THIS arm_cxx_cdtor_returns_this

#undef TARGET_CXX_KEY_METHOD_MAY_BE_INLINE
#define TARGET_CXX_KEY_METHOD_MAY_BE_INLINE arm_cxx_key_method_may_be_inline

#undef TARGET_CXX_USE_AEABI_ATEXIT
#define TARGET_CXX_USE_AEABI_ATEXIT arm_cxx_use_aeabi_atexit

#undef TARGET_CXX_DETERMINE_CLASS_DATA_VISIBILITY
#define TARGET_CXX_DETERMINE_CLASS_DATA_VISIBILITY \
  arm_cxx_determine_class_data_visibility

#undef TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT
#define TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT arm_cxx_class_data_always_comdat

#undef TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB arm_return_in_msb

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY arm_return_in_memory

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK arm_must_pass_in_stack

#if ARM_UNWIND_INFO
#undef TARGET_ASM_UNWIND_EMIT
#define TARGET_ASM_UNWIND_EMIT arm_unwind_emit

/* EABI unwinding tables use a different format for the typeinfo tables.  */
#undef TARGET_ASM_TTYPE
#define TARGET_ASM_TTYPE arm_output_ttype

#undef TARGET_ARM_EABI_UNWINDER
#define TARGET_ARM_EABI_UNWINDER true

#undef TARGET_ASM_EMIT_EXCEPT_PERSONALITY
#define TARGET_ASM_EMIT_EXCEPT_PERSONALITY arm_asm_emit_except_personality

#endif /* ARM_UNWIND_INFO */

#undef TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS arm_asm_init_sections

#undef TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN arm_dwarf_register_span

#undef  TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P arm_cannot_copy_insn_p

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif

#undef TARGET_HAVE_CONDITIONAL_EXECUTION
#define TARGET_HAVE_CONDITIONAL_EXECUTION arm_have_conditional_execution

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P arm_legitimate_constant_p

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM arm_cannot_force_const_mem

#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 4095

/* The minimum is set such that the total size of the block
   for a particular anchor is -4088 + 1 + 4095 bytes, which is
   divisible by eight, ensuring natural spacing of anchors.  */
#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -4088

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE arm_issue_rate

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  arm_first_cycle_multipass_dfa_lookahead

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD \
  arm_first_cycle_multipass_dfa_lookahead_guard

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE arm_mangle_type

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV arm_atomic_assign_expand_fenv

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST arm_build_builtin_va_list
#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START arm_expand_builtin_va_start
#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR arm_gimplify_va_arg_expr

#ifdef HAVE_AS_TLS
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL arm_output_dwarf_dtprel
#endif

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	arm_legitimate_address_p

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS arm_preferred_reload_class

#undef TARGET_PROMOTED_TYPE
#define TARGET_PROMOTED_TYPE arm_promoted_type

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P arm_scalar_mode_supported_p

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT arm_compute_frame_layout

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED arm_frame_pointer_required

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE arm_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE arm_conditional_register_usage

#undef TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P arm_class_likely_spilled_p

#undef TARGET_VECTORIZE_BUILTINS
#define TARGET_VECTORIZE_BUILTINS

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  arm_builtin_vectorized_function

#undef TARGET_VECTOR_ALIGNMENT
#define TARGET_VECTOR_ALIGNMENT arm_vector_alignment

#undef TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE
#define TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE \
  arm_vector_alignment_reachable

#undef TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT
#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT \
  arm_builtin_support_vector_misalignment

#undef TARGET_PREFERRED_RENAME_CLASS
#define TARGET_PREFERRED_RENAME_CLASS \
  arm_preferred_rename_class

#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST arm_vectorize_vec_perm_const

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  arm_builtin_vectorization_cost
#undef TARGET_VECTORIZE_ADD_STMT_COST
#define TARGET_VECTORIZE_ADD_STMT_COST arm_add_stmt_cost

#undef TARGET_CANONICALIZE_COMPARISON
#define TARGET_CANONICALIZE_COMPARISON \
  arm_canonicalize_comparison

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET arm_asan_shadow_offset

#undef MAX_INSN_PER_IT_BLOCK
#define MAX_INSN_PER_IT_BLOCK (arm_restrict_it ? 1 : 4)

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P can_use_doloop_if_innermost

#undef TARGET_CONST_NOT_OK_FOR_DEBUG_P
#define TARGET_CONST_NOT_OK_FOR_DEBUG_P arm_const_not_ok_for_debug_p

#undef TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS
#define TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS true

#undef TARGET_SCHED_FUSION_PRIORITY
#define TARGET_SCHED_FUSION_PRIORITY arm_sched_fusion_priority

#undef  TARGET_ASM_FUNCTION_SECTION
#define TARGET_ASM_FUNCTION_SECTION arm_function_section

#undef TARGET_ASM_ELF_FLAGS_NUMERIC
#define TARGET_ASM_ELF_FLAGS_NUMERIC arm_asm_elf_flags_numeric

#undef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS arm_elf_section_type_flags

#undef TARGET_EXPAND_DIVMOD_LIBFUNC
#define TARGET_EXPAND_DIVMOD_LIBFUNC arm_expand_divmod_libfunc

#undef TARGET_C_EXCESS_PRECISION
#define TARGET_C_EXCESS_PRECISION arm_excess_precision

/* Although the architecture reserves bits 0 and 1, only the former is
   used for ARM/Thumb ISA selection in v7 and earlier versions.  */
#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 2

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS arm_fixed_condition_code_regs

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS arm_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK arm_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P arm_modes_tieable_p

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS arm_can_change_mode_class

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT arm_constant_alignment

/* Obstack for minipool constant handling.  */
static struct obstack minipool_obstack;
static char *         minipool_startobj;

/* The maximum number of insns skipped which
   will be conditionalised if possible.  */
static int max_insns_skipped = 5;

extern FILE * asm_out_file;

/* True if we are currently building a constant table.  */
int making_const_table;

/* The processor for which instructions should be scheduled.  */
enum processor_type arm_tune = TARGET_CPU_arm_none;

/* The current tuning set.  */
const struct tune_params *current_tune;

/* Which floating point hardware to schedule for.  */
int arm_fpu_attr;

/* Used for Thumb call_via trampolines.  */
rtx thumb_call_via_label[14];
static int thumb_call_reg_needed;

/* The bits in this mask specify which instruction scheduling options should
   be used.  */
unsigned int tune_flags = 0;

/* The highest ARM architecture version supported by the
   target.  */
enum base_architecture arm_base_arch = BASE_ARCH_0;

/* Active target architecture and tuning.  */

struct arm_build_target arm_active_target;

/* The following are used in the arm.md file as equivalents to bits
   in the above two flag variables.  */

/* Nonzero if this chip supports the ARM Architecture 3M extensions.  */
int arm_arch3m = 0;

/* Nonzero if this chip supports the ARM Architecture 4 extensions.  */
int arm_arch4 = 0;

/* Nonzero if this chip supports the ARM Architecture 4t extensions.  */
int arm_arch4t = 0;

/* Nonzero if this chip supports the ARM Architecture 5 extensions.  */
int arm_arch5 = 0;

/* Nonzero if this chip supports the ARM Architecture 5E extensions.  */
int arm_arch5e = 0;

/* Nonzero if this chip supports the ARM Architecture 5TE extensions.  */
int arm_arch5te = 0;

/* Nonzero if this chip supports the ARM Architecture 6 extensions.  */
int arm_arch6 = 0;

/* Nonzero if this chip supports the ARM 6K extensions.  */
int arm_arch6k = 0;

/* Nonzero if this chip supports the ARM 6KZ extensions.  */
int arm_arch6kz = 0;

/* Nonzero if instructions present in ARMv6-M can be used.  */
int arm_arch6m = 0;

/* Nonzero if this chip supports the ARM 7 extensions.  */
int arm_arch7 = 0;

/* Nonzero if this chip supports the Large Physical Address Extension.  */
int arm_arch_lpae = 0;

/* Nonzero if instructions not present in the 'M' profile can be used.  */
int arm_arch_notm = 0;

/* Nonzero if instructions present in ARMv7E-M can be used.  */
int arm_arch7em = 0;

/* Nonzero if instructions present in ARMv8 can be used.  */
int arm_arch8 = 0;

/* Nonzero if this chip supports the ARMv8.1 extensions.  */
int arm_arch8_1 = 0;

/* Nonzero if this chip supports the ARM Architecture 8.2 extensions.  */
int arm_arch8_2 = 0;

/* Nonzero if this chip supports the FP16 instructions extension of ARM
   Architecture 8.2.  */
int arm_fp16_inst = 0;

/* Nonzero if this chip can benefit from load scheduling.  */
int arm_ld_sched = 0;

/* Nonzero if this chip is a StrongARM.  */
int arm_tune_strongarm = 0;

/* Nonzero if this chip supports Intel Wireless MMX technology.  */
int arm_arch_iwmmxt = 0;

/* Nonzero if this chip supports Intel Wireless MMX2 technology.  */
int arm_arch_iwmmxt2 = 0;

/* Nonzero if this chip is an XScale.  */
int arm_arch_xscale = 0;

/* Nonzero if tuning for XScale  */
int arm_tune_xscale = 0;

/* Nonzero if we want to tune for stores that access the write-buffer.
   This typically means an ARM6 or ARM7 with MMU or MPU.  */
int arm_tune_wbuf = 0;

/* Nonzero if tuning for Cortex-A9.  */
int arm_tune_cortex_a9 = 0;

/* Nonzero if we should define __THUMB_INTERWORK__ in the
   preprocessor.
   XXX This is a bit of a hack, it's intended to help work around
   problems in GLD which doesn't understand that armv5t code is
   interworking clean.  */
int arm_cpp_interwork = 0;

/* Nonzero if chip supports Thumb 1.  */
int arm_arch_thumb1;

/* Nonzero if chip supports Thumb 2.  */
int arm_arch_thumb2;

/* Nonzero if chip supports integer division instruction.  */
int arm_arch_arm_hwdiv;
int arm_arch_thumb_hwdiv;

/* Nonzero if chip disallows volatile memory access in IT block.  */
int arm_arch_no_volatile_ce;

/* Nonzero if we should use Neon to handle 64-bits operations rather
   than core registers.  */
int prefer_neon_for_64bits = 0;

/* Nonzero if we shouldn't use literal pools.  */
bool arm_disable_literal_pool = false;

/* The register number to be used for the PIC offset register.  */
unsigned arm_pic_register = INVALID_REGNUM;

enum arm_pcs arm_pcs_default;

/* For an explanation of these variables, see final_prescan_insn below.  */
int arm_ccfsm_state;
/* arm_current_cc is also used for Thumb-2 cond_exec blocks.  */
enum arm_cond_code arm_current_cc;

rtx arm_target_insn;
int arm_target_label;
/* The number of conditionally executed insns, including the current insn.  */
int arm_condexec_count = 0;
/* A bitmask specifying the patterns for the IT block.
   Zero means do not output an IT block before this insn. */
int arm_condexec_mask = 0;
/* The number of bits used in arm_condexec_mask.  */
int arm_condexec_masklen = 0;

/* Nonzero if chip supports the ARMv8 CRC instructions.  */
int arm_arch_crc = 0;

/* Nonzero if chip supports the AdvSIMD Dot Product instructions.  */
int arm_arch_dotprod = 0;

/* Nonzero if chip supports the ARMv8-M security extensions.  */
int arm_arch_cmse = 0;

/* Nonzero if the core has a very small, high-latency, multiply unit.  */
int arm_m_profile_small_mul = 0;

/* The condition codes of the ARM, and the inverse function.  */
static const char * const arm_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

/* The register numbers in sequence, for passing to arm_gen_load_multiple.  */
int arm_regs_in_sequence[] =
{
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
};

#define ARM_LSL_NAME "lsl"
#define streq(string1, string2) (strcmp (string1, string2) == 0)

#define THUMB2_WORK_REGS (0xff & ~(  (1 << THUMB_HARD_FRAME_POINTER_REGNUM) \
				   | (1 << SP_REGNUM) | (1 << PC_REGNUM) \
				   | (1 << PIC_OFFSET_TABLE_REGNUM)))

/* Initialization code.  */

struct cpu_tune
{
  enum processor_type scheduler;
  unsigned int tune_flags;
  const struct tune_params *tune;
};

#define ARM_PREFETCH_NOT_BENEFICIAL { 0, -1, -1 }
#define ARM_PREFETCH_BENEFICIAL(num_slots,l1_size,l1_line_size) \
  {								\
    num_slots,							\
    l1_size,							\
    l1_line_size						\
  }

/* arm generic vectorizer costs.  */
static const
struct cpu_vec_costs arm_default_vec_cost = {
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  1,					/* vec_unalign_load_cost.  */
  1,					/* vec_unalign_store_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
};

/* Cost tables for AArch32 + AArch64 cores should go in aarch-cost-tables.h  */
#include "aarch-cost-tables.h"



const struct cpu_cost_table cortexa9_extra_costs =
{
  /* ALU */
  {
    0,			/* arith.  */
    0,			/* logical.  */
    0,			/* shift.  */
    COSTS_N_INSNS (1),	/* shift_reg.  */
    COSTS_N_INSNS (1),	/* arith_shift.  */
    COSTS_N_INSNS (2),	/* arith_shift_reg.  */
    0,			/* log_shift.  */
    COSTS_N_INSNS (1),	/* log_shift_reg.  */
    COSTS_N_INSNS (1),	/* extend.  */
    COSTS_N_INSNS (2),	/* extend_arith.  */
    COSTS_N_INSNS (1),	/* bfi.  */
    COSTS_N_INSNS (1),	/* bfx.  */
    0,			/* clz.  */
    0,			/* rev.  */
    0,			/* non_exec.  */
    true		/* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (3),	/* simple.  */
      COSTS_N_INSNS (3),	/* flag_setting.  */
      COSTS_N_INSNS (2),	/* extend.  */
      COSTS_N_INSNS (3),	/* add.  */
      COSTS_N_INSNS (2),	/* extend_add.  */
      COSTS_N_INSNS (30)	/* idiv.  No HW div on Cortex A9.  */
    },
    /* MULT DImode */
    {
      0,			/* simple (N/A).  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (4),	/* extend.  */
      0,			/* add (N/A).  */
      COSTS_N_INSNS (4),	/* extend_add.  */
      0				/* idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (2),	/* load.  */
    COSTS_N_INSNS (2),	/* load_sign_extend.  */
    COSTS_N_INSNS (2),	/* ldrd.  */
    COSTS_N_INSNS (2),	/* ldm_1st.  */
    1,			/* ldm_regs_per_insn_1st.  */
    2,			/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (5),	/* loadf.  */
    COSTS_N_INSNS (5),	/* loadd.  */
    COSTS_N_INSNS (1),  /* load_unaligned.  */
    COSTS_N_INSNS (2),	/* store.  */
    COSTS_N_INSNS (2),	/* strd.  */
    COSTS_N_INSNS (2),	/* stm_1st.  */
    1,			/* stm_regs_per_insn_1st.  */
    2,			/* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (1),	/* storef.  */
    COSTS_N_INSNS (1),	/* stored.  */
    COSTS_N_INSNS (1),	/* store_unaligned.  */
    COSTS_N_INSNS (1),	/* loadv.  */
    COSTS_N_INSNS (1)	/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (14),	/* div.  */
      COSTS_N_INSNS (4),	/* mult.  */
      COSTS_N_INSNS (7),	/* mult_addsub. */
      COSTS_N_INSNS (30),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      COSTS_N_INSNS (1),	/* fpconst.  */
      COSTS_N_INSNS (1),	/* neg.  */
      COSTS_N_INSNS (3),	/* compare.  */
      COSTS_N_INSNS (3),	/* widen.  */
      COSTS_N_INSNS (3),	/* narrow.  */
      COSTS_N_INSNS (3),	/* toint.  */
      COSTS_N_INSNS (3),	/* fromint.  */
      COSTS_N_INSNS (3)		/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (24),	/* div.  */
      COSTS_N_INSNS (5),	/* mult.  */
      COSTS_N_INSNS (8),	/* mult_addsub.  */
      COSTS_N_INSNS (30),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      COSTS_N_INSNS (1),	/* fpconst.  */
      COSTS_N_INSNS (1),	/* neg.  */
      COSTS_N_INSNS (3),	/* compare.  */
      COSTS_N_INSNS (3),	/* widen.  */
      COSTS_N_INSNS (3),	/* narrow.  */
      COSTS_N_INSNS (3),	/* toint.  */
      COSTS_N_INSNS (3),	/* fromint.  */
      COSTS_N_INSNS (3)		/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
  }
};

const struct cpu_cost_table cortexa8_extra_costs =
{
  /* ALU */
  {
    0,			/* arith.  */
    0,			/* logical.  */
    COSTS_N_INSNS (1),	/* shift.  */
    0,			/* shift_reg.  */
    COSTS_N_INSNS (1),	/* arith_shift.  */
    0,			/* arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* log_shift.  */
    0,			/* log_shift_reg.  */
    0,			/* extend.  */
    0,			/* extend_arith.  */
    0,			/* bfi.  */
    0,			/* bfx.  */
    0,			/* clz.  */
    0,			/* rev.  */
    0,			/* non_exec.  */
    true		/* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (1),	/* simple.  */
      COSTS_N_INSNS (1),	/* flag_setting.  */
      COSTS_N_INSNS (1),	/* extend.  */
      COSTS_N_INSNS (1),	/* add.  */
      COSTS_N_INSNS (1),	/* extend_add.  */
      COSTS_N_INSNS (30)	/* idiv.  No HW div on Cortex A8.  */
    },
    /* MULT DImode */
    {
      0,			/* simple (N/A).  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (2),	/* extend.  */
      0,			/* add (N/A).  */
      COSTS_N_INSNS (2),	/* extend_add.  */
      0				/* idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (1),	/* load.  */
    COSTS_N_INSNS (1),	/* load_sign_extend.  */
    COSTS_N_INSNS (1),	/* ldrd.  */
    COSTS_N_INSNS (1),	/* ldm_1st.  */
    1,			/* ldm_regs_per_insn_1st.  */
    2,			/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (1),	/* loadf.  */
    COSTS_N_INSNS (1),	/* loadd.  */
    COSTS_N_INSNS (1),  /* load_unaligned.  */
    COSTS_N_INSNS (1),	/* store.  */
    COSTS_N_INSNS (1),	/* strd.  */
    COSTS_N_INSNS (1),	/* stm_1st.  */
    1,			/* stm_regs_per_insn_1st.  */
    2,			/* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (1),	/* storef.  */
    COSTS_N_INSNS (1),	/* stored.  */
    COSTS_N_INSNS (1),	/* store_unaligned.  */
    COSTS_N_INSNS (1),	/* loadv.  */
    COSTS_N_INSNS (1)	/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (36),	/* div.  */
      COSTS_N_INSNS (11),	/* mult.  */
      COSTS_N_INSNS (20),	/* mult_addsub. */
      COSTS_N_INSNS (30),	/* fma.  */
      COSTS_N_INSNS (9),	/* addsub.  */
      COSTS_N_INSNS (3),	/* fpconst.  */
      COSTS_N_INSNS (3),	/* neg.  */
      COSTS_N_INSNS (6),	/* compare.  */
      COSTS_N_INSNS (4),	/* widen.  */
      COSTS_N_INSNS (4),	/* narrow.  */
      COSTS_N_INSNS (8),	/* toint.  */
      COSTS_N_INSNS (8),	/* fromint.  */
      COSTS_N_INSNS (8)		/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (64),	/* div.  */
      COSTS_N_INSNS (16),	/* mult.  */
      COSTS_N_INSNS (25),	/* mult_addsub.  */
      COSTS_N_INSNS (30),	/* fma.  */
      COSTS_N_INSNS (9),	/* addsub.  */
      COSTS_N_INSNS (3),	/* fpconst.  */
      COSTS_N_INSNS (3),	/* neg.  */
      COSTS_N_INSNS (6),	/* compare.  */
      COSTS_N_INSNS (6),	/* widen.  */
      COSTS_N_INSNS (6),	/* narrow.  */
      COSTS_N_INSNS (8),	/* toint.  */
      COSTS_N_INSNS (8),	/* fromint.  */
      COSTS_N_INSNS (8)		/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
  }
};

const struct cpu_cost_table cortexa5_extra_costs =
{
  /* ALU */
  {
    0,			/* arith.  */
    0,			/* logical.  */
    COSTS_N_INSNS (1),	/* shift.  */
    COSTS_N_INSNS (1),	/* shift_reg.  */
    COSTS_N_INSNS (1),	/* arith_shift.  */
    COSTS_N_INSNS (1),	/* arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* log_shift.  */
    COSTS_N_INSNS (1),	/* log_shift_reg.  */
    COSTS_N_INSNS (1),	/* extend.  */
    COSTS_N_INSNS (1),	/* extend_arith.  */
    COSTS_N_INSNS (1),	/* bfi.  */
    COSTS_N_INSNS (1),	/* bfx.  */
    COSTS_N_INSNS (1),	/* clz.  */
    COSTS_N_INSNS (1),	/* rev.  */
    0,			/* non_exec.  */
    true		/* non_exec_costs_exec.  */
  },

  {
    /* MULT SImode */
    {
      0,			/* simple.  */
      COSTS_N_INSNS (1),	/* flag_setting.  */
      COSTS_N_INSNS (1),	/* extend.  */
      COSTS_N_INSNS (1),	/* add.  */
      COSTS_N_INSNS (1),	/* extend_add.  */
      COSTS_N_INSNS (7)		/* idiv.  */
    },
    /* MULT DImode */
    {
      0,			/* simple (N/A).  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (1),	/* extend.  */
      0,			/* add.  */
      COSTS_N_INSNS (2),	/* extend_add.  */
      0				/* idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (1),	/* load.  */
    COSTS_N_INSNS (1),	/* load_sign_extend.  */
    COSTS_N_INSNS (6),	/* ldrd.  */
    COSTS_N_INSNS (1),	/* ldm_1st.  */
    1,			/* ldm_regs_per_insn_1st.  */
    2,			/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* loadf.  */
    COSTS_N_INSNS (4),	/* loadd.  */
    COSTS_N_INSNS (1),	/* load_unaligned.  */
    COSTS_N_INSNS (1),	/* store.  */
    COSTS_N_INSNS (3),	/* strd.  */
    COSTS_N_INSNS (1),	/* stm_1st.  */
    1,			/* stm_regs_per_insn_1st.  */
    2,			/* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* storef.  */
    COSTS_N_INSNS (2),	/* stored.  */
    COSTS_N_INSNS (1),	/* store_unaligned.  */
    COSTS_N_INSNS (1),	/* loadv.  */
    COSTS_N_INSNS (1)	/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (15),	/* div.  */
      COSTS_N_INSNS (3),	/* mult.  */
      COSTS_N_INSNS (7),	/* mult_addsub. */
      COSTS_N_INSNS (7),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      COSTS_N_INSNS (3),	/* fpconst.  */
      COSTS_N_INSNS (3),	/* neg.  */
      COSTS_N_INSNS (3),	/* compare.  */
      COSTS_N_INSNS (3),	/* widen.  */
      COSTS_N_INSNS (3),	/* narrow.  */
      COSTS_N_INSNS (3),	/* toint.  */
      COSTS_N_INSNS (3),	/* fromint.  */
      COSTS_N_INSNS (3)		/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (30),	/* div.  */
      COSTS_N_INSNS (6),	/* mult.  */
      COSTS_N_INSNS (10),	/* mult_addsub.  */
      COSTS_N_INSNS (7),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      COSTS_N_INSNS (3),	/* fpconst.  */
      COSTS_N_INSNS (3),	/* neg.  */
      COSTS_N_INSNS (3),	/* compare.  */
      COSTS_N_INSNS (3),	/* widen.  */
      COSTS_N_INSNS (3),	/* narrow.  */
      COSTS_N_INSNS (3),	/* toint.  */
      COSTS_N_INSNS (3),	/* fromint.  */
      COSTS_N_INSNS (3)		/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
  }
};


const struct cpu_cost_table cortexa7_extra_costs =
{
  /* ALU */
  {
    0,			/* arith.  */
    0,			/* logical.  */
    COSTS_N_INSNS (1),	/* shift.  */
    COSTS_N_INSNS (1),	/* shift_reg.  */
    COSTS_N_INSNS (1),	/* arith_shift.  */
    COSTS_N_INSNS (1),	/* arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* log_shift.  */
    COSTS_N_INSNS (1),	/* log_shift_reg.  */
    COSTS_N_INSNS (1),	/* extend.  */
    COSTS_N_INSNS (1),	/* extend_arith.  */
    COSTS_N_INSNS (1),	/* bfi.  */
    COSTS_N_INSNS (1),	/* bfx.  */
    COSTS_N_INSNS (1),	/* clz.  */
    COSTS_N_INSNS (1),	/* rev.  */
    0,			/* non_exec.  */
    true		/* non_exec_costs_exec.  */
  },

  {
    /* MULT SImode */
    {
      0,			/* simple.  */
      COSTS_N_INSNS (1),	/* flag_setting.  */
      COSTS_N_INSNS (1),	/* extend.  */
      COSTS_N_INSNS (1),	/* add.  */
      COSTS_N_INSNS (1),	/* extend_add.  */
      COSTS_N_INSNS (7)		/* idiv.  */
    },
    /* MULT DImode */
    {
      0,			/* simple (N/A).  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (1),	/* extend.  */
      0,			/* add.  */
      COSTS_N_INSNS (2),	/* extend_add.  */
      0				/* idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (1),	/* load.  */
    COSTS_N_INSNS (1),	/* load_sign_extend.  */
    COSTS_N_INSNS (3),	/* ldrd.  */
    COSTS_N_INSNS (1),	/* ldm_1st.  */
    1,			/* ldm_regs_per_insn_1st.  */
    2,			/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* loadf.  */
    COSTS_N_INSNS (2),	/* loadd.  */
    COSTS_N_INSNS (1),	/* load_unaligned.  */
    COSTS_N_INSNS (1),	/* store.  */
    COSTS_N_INSNS (3),	/* strd.  */
    COSTS_N_INSNS (1),	/* stm_1st.  */
    1,			/* stm_regs_per_insn_1st.  */
    2,			/* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* storef.  */
    COSTS_N_INSNS (2),	/* stored.  */
    COSTS_N_INSNS (1),	/* store_unaligned.  */
    COSTS_N_INSNS (1),	/* loadv.  */
    COSTS_N_INSNS (1)	/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (15),	/* div.  */
      COSTS_N_INSNS (3),	/* mult.  */
      COSTS_N_INSNS (7),	/* mult_addsub. */
      COSTS_N_INSNS (7),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      COSTS_N_INSNS (3),	/* fpconst.  */
      COSTS_N_INSNS (3),	/* neg.  */
      COSTS_N_INSNS (3),	/* compare.  */
      COSTS_N_INSNS (3),	/* widen.  */
      COSTS_N_INSNS (3),	/* narrow.  */
      COSTS_N_INSNS (3),	/* toint.  */
      COSTS_N_INSNS (3),	/* fromint.  */
      COSTS_N_INSNS (3)		/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (30),	/* div.  */
      COSTS_N_INSNS (6),	/* mult.  */
      COSTS_N_INSNS (10),	/* mult_addsub.  */
      COSTS_N_INSNS (7),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      COSTS_N_INSNS (3),	/* fpconst.  */
      COSTS_N_INSNS (3),	/* neg.  */
      COSTS_N_INSNS (3),	/* compare.  */
      COSTS_N_INSNS (3),	/* widen.  */
      COSTS_N_INSNS (3),	/* narrow.  */
      COSTS_N_INSNS (3),	/* toint.  */
      COSTS_N_INSNS (3),	/* fromint.  */
      COSTS_N_INSNS (3)		/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
  }
};

const struct cpu_cost_table cortexa12_extra_costs =
{
  /* ALU */
  {
    0,			/* arith.  */
    0,			/* logical.  */
    0,			/* shift.  */
    COSTS_N_INSNS (1),	/* shift_reg.  */
    COSTS_N_INSNS (1),	/* arith_shift.  */
    COSTS_N_INSNS (1),	/* arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* log_shift.  */
    COSTS_N_INSNS (1),	/* log_shift_reg.  */
    0,			/* extend.  */
    COSTS_N_INSNS (1),	/* extend_arith.  */
    0,			/* bfi.  */
    COSTS_N_INSNS (1),	/* bfx.  */
    COSTS_N_INSNS (1),	/* clz.  */
    COSTS_N_INSNS (1),	/* rev.  */
    0,			/* non_exec.  */
    true		/* non_exec_costs_exec.  */
  },
  /* MULT SImode */
  {
    {
      COSTS_N_INSNS (2),	/* simple.  */
      COSTS_N_INSNS (3),	/* flag_setting.  */
      COSTS_N_INSNS (2),	/* extend.  */
      COSTS_N_INSNS (3),	/* add.  */
      COSTS_N_INSNS (2),	/* extend_add.  */
      COSTS_N_INSNS (18)	/* idiv.  */
    },
    /* MULT DImode */
    {
      0,			/* simple (N/A).  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (3),	/* extend.  */
      0,			/* add (N/A).  */
      COSTS_N_INSNS (3),	/* extend_add.  */
      0				/* idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (3),	/* load.  */
    COSTS_N_INSNS (3),	/* load_sign_extend.  */
    COSTS_N_INSNS (3),	/* ldrd.  */
    COSTS_N_INSNS (3),	/* ldm_1st.  */
    1,			/* ldm_regs_per_insn_1st.  */
    2,			/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (3),	/* loadf.  */
    COSTS_N_INSNS (3),	/* loadd.  */
    0,			/* load_unaligned.  */
    0,			/* store.  */
    0,			/* strd.  */
    0,			/* stm_1st.  */
    1,			/* stm_regs_per_insn_1st.  */
    2,			/* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* storef.  */
    COSTS_N_INSNS (2),	/* stored.  */
    0,			/* store_unaligned.  */
    COSTS_N_INSNS (1),	/* loadv.  */
    COSTS_N_INSNS (1)	/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (17),	/* div.  */
      COSTS_N_INSNS (4),	/* mult.  */
      COSTS_N_INSNS (8),	/* mult_addsub. */
      COSTS_N_INSNS (8),	/* fma.  */
      COSTS_N_INSNS (4),	/* addsub.  */
      COSTS_N_INSNS (2),	/* fpconst. */
      COSTS_N_INSNS (2),	/* neg.  */
      COSTS_N_INSNS (2),	/* compare.  */
      COSTS_N_INSNS (4),	/* widen.  */
      COSTS_N_INSNS (4),	/* narrow.  */
      COSTS_N_INSNS (4),	/* toint.  */
      COSTS_N_INSNS (4),	/* fromint.  */
      COSTS_N_INSNS (4)		/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (31),	/* div.  */
      COSTS_N_INSNS (4),	/* mult.  */
      COSTS_N_INSNS (8),	/* mult_addsub.  */
      COSTS_N_INSNS (8),	/* fma.  */
      COSTS_N_INSNS (4),	/* addsub.  */
      COSTS_N_INSNS (2),	/* fpconst.  */
      COSTS_N_INSNS (2),	/* neg.  */
      COSTS_N_INSNS (2),	/* compare.  */
      COSTS_N_INSNS (4),	/* widen.  */
      COSTS_N_INSNS (4),	/* narrow.  */
      COSTS_N_INSNS (4),	/* toint.  */
      COSTS_N_INSNS (4),	/* fromint.  */
      COSTS_N_INSNS (4)		/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
  }
};

const struct cpu_cost_table cortexa15_extra_costs =
{
  /* ALU */
  {
    0,			/* arith.  */
    0,			/* logical.  */
    0,			/* shift.  */
    0,			/* shift_reg.  */
    COSTS_N_INSNS (1),	/* arith_shift.  */
    COSTS_N_INSNS (1),	/* arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* log_shift.  */
    COSTS_N_INSNS (1),	/* log_shift_reg.  */
    0,			/* extend.  */
    COSTS_N_INSNS (1),	/* extend_arith.  */
    COSTS_N_INSNS (1),	/* bfi.  */
    0,			/* bfx.  */
    0,			/* clz.  */
    0,			/* rev.  */
    0,			/* non_exec.  */
    true		/* non_exec_costs_exec.  */
  },
  /* MULT SImode */
  {
    {
      COSTS_N_INSNS (2),	/* simple.  */
      COSTS_N_INSNS (3),	/* flag_setting.  */
      COSTS_N_INSNS (2),	/* extend.  */
      COSTS_N_INSNS (2),	/* add.  */
      COSTS_N_INSNS (2),	/* extend_add.  */
      COSTS_N_INSNS (18)	/* idiv.  */
    },
    /* MULT DImode */
    {
      0,			/* simple (N/A).  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (3),	/* extend.  */
      0,			/* add (N/A).  */
      COSTS_N_INSNS (3),	/* extend_add.  */
      0				/* idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (3),	/* load.  */
    COSTS_N_INSNS (3),	/* load_sign_extend.  */
    COSTS_N_INSNS (3),	/* ldrd.  */
    COSTS_N_INSNS (4),	/* ldm_1st.  */
    1,			/* ldm_regs_per_insn_1st.  */
    2,			/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (4),	/* loadf.  */
    COSTS_N_INSNS (4),	/* loadd.  */
    0,			/* load_unaligned.  */
    0,			/* store.  */
    0,			/* strd.  */
    COSTS_N_INSNS (1),	/* stm_1st.  */
    1,			/* stm_regs_per_insn_1st.  */
    2,			/* stm_regs_per_insn_subsequent.  */
    0,			/* storef.  */
    0,			/* stored.  */
    0,			/* store_unaligned.  */
    COSTS_N_INSNS (1),	/* loadv.  */
    COSTS_N_INSNS (1)	/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (17),	/* div.  */
      COSTS_N_INSNS (4),	/* mult.  */
      COSTS_N_INSNS (8),	/* mult_addsub. */
      COSTS_N_INSNS (8),	/* fma.  */
      COSTS_N_INSNS (4),	/* addsub.  */
      COSTS_N_INSNS (2),	/* fpconst. */
      COSTS_N_INSNS (2),	/* neg.  */
      COSTS_N_INSNS (5),	/* compare.  */
      COSTS_N_INSNS (4),	/* widen.  */
      COSTS_N_INSNS (4),	/* narrow.  */
      COSTS_N_INSNS (4),	/* toint.  */
      COSTS_N_INSNS (4),	/* fromint.  */
      COSTS_N_INSNS (4)		/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (31),	/* div.  */
      COSTS_N_INSNS (4),	/* mult.  */
      COSTS_N_INSNS (8),	/* mult_addsub.  */
      COSTS_N_INSNS (8),	/* fma.  */
      COSTS_N_INSNS (4),	/* addsub.  */
      COSTS_N_INSNS (2),	/* fpconst.  */
      COSTS_N_INSNS (2),	/* neg.  */
      COSTS_N_INSNS (2),	/* compare.  */
      COSTS_N_INSNS (4),	/* widen.  */
      COSTS_N_INSNS (4),	/* narrow.  */
      COSTS_N_INSNS (4),	/* toint.  */
      COSTS_N_INSNS (4),	/* fromint.  */
      COSTS_N_INSNS (4)		/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
  }
};

const struct cpu_cost_table v7m_extra_costs =
{
  /* ALU */
  {
    0,			/* arith.  */
    0,			/* logical.  */
    0,			/* shift.  */
    0,			/* shift_reg.  */
    0,			/* arith_shift.  */
    COSTS_N_INSNS (1),	/* arith_shift_reg.  */
    0,			/* log_shift.  */
    COSTS_N_INSNS (1),	/* log_shift_reg.  */
    0,			/* extend.  */
    COSTS_N_INSNS (1),	/* extend_arith.  */
    0,			/* bfi.  */
    0,			/* bfx.  */
    0,			/* clz.  */
    0,			/* rev.  */
    COSTS_N_INSNS (1),	/* non_exec.  */
    false		/* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (1),	/* simple.  */
      COSTS_N_INSNS (1),	/* flag_setting.  */
      COSTS_N_INSNS (2),	/* extend.  */
      COSTS_N_INSNS (1),	/* add.  */
      COSTS_N_INSNS (3),	/* extend_add.  */
      COSTS_N_INSNS (8)		/* idiv.  */
    },
    /* MULT DImode */
    {
      0,			/* simple (N/A).  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (2),	/* extend.  */
      0,			/* add (N/A).  */
      COSTS_N_INSNS (3),	/* extend_add.  */
      0				/* idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (2),	/* load.  */
    0,			/* load_sign_extend.  */
    COSTS_N_INSNS (3),	/* ldrd.  */
    COSTS_N_INSNS (2),	/* ldm_1st.  */
    1,			/* ldm_regs_per_insn_1st.  */
    1,			/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* loadf.  */
    COSTS_N_INSNS (3),	/* loadd.  */
    COSTS_N_INSNS (1),  /* load_unaligned.  */
    COSTS_N_INSNS (2),	/* store.  */
    COSTS_N_INSNS (3),	/* strd.  */
    COSTS_N_INSNS (2),	/* stm_1st.  */
    1,			/* stm_regs_per_insn_1st.  */
    1,			/* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* storef.  */
    COSTS_N_INSNS (3),	/* stored.  */
    COSTS_N_INSNS (1),	/* store_unaligned.  */
    COSTS_N_INSNS (1),	/* loadv.  */
    COSTS_N_INSNS (1)	/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (7),	/* div.  */
      COSTS_N_INSNS (2),	/* mult.  */
      COSTS_N_INSNS (5),	/* mult_addsub.  */
      COSTS_N_INSNS (3),	/* fma.  */
      COSTS_N_INSNS (1),	/* addsub.  */
      0,			/* fpconst.  */
      0,			/* neg.  */
      0,			/* compare.  */
      0,			/* widen.  */
      0,			/* narrow.  */
      0,			/* toint.  */
      0,			/* fromint.  */
      0				/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (15),	/* div.  */
      COSTS_N_INSNS (5),	/* mult.  */
      COSTS_N_INSNS (7),	/* mult_addsub.  */
      COSTS_N_INSNS (7),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      0,			/* fpconst.  */
      0,			/* neg.  */
      0,			/* compare.  */
      0,			/* widen.  */
      0,			/* narrow.  */
      0,			/* toint.  */
      0,			/* fromint.  */
      0				/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
  }
};

const struct addr_mode_cost_table generic_addr_mode_costs =
{
  /* int.  */
  {
    COSTS_N_INSNS (0),	/* AMO_DEFAULT.  */
    COSTS_N_INSNS (0),	/* AMO_NO_WB.  */
    COSTS_N_INSNS (0)	/* AMO_WB.  */
  },
  /* float.  */
  {
    COSTS_N_INSNS (0),	/* AMO_DEFAULT.  */
    COSTS_N_INSNS (0),	/* AMO_NO_WB.  */
    COSTS_N_INSNS (0)	/* AMO_WB.  */
  },
  /* vector.  */
  {
    COSTS_N_INSNS (0),	/* AMO_DEFAULT.  */
    COSTS_N_INSNS (0),	/* AMO_NO_WB.  */
    COSTS_N_INSNS (0)	/* AMO_WB.  */
  }
};

const struct tune_params arm_slowmul_tune =
{
  &generic_extra_costs,			/* Insn extra costs.  */
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  3,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_fastmul_tune =
{
  &generic_extra_costs,			/* Insn extra costs.  */
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

/* StrongARM has early execution of branches, so a sequence that is worth
   skipping is shorter.  Set max_insns_skipped to a lower value.  */

const struct tune_params arm_strongarm_tune =
{
  &generic_extra_costs,			/* Insn extra costs.  */
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  3,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_xscale_tune =
{
  &generic_extra_costs,			/* Insn extra costs.  */
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  xscale_sched_adjust_cost,
  arm_default_branch_cost,
  &arm_default_vec_cost,
  2,						/* Constant limit.  */
  3,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_9e_tune =
{
  &generic_extra_costs,			/* Insn extra costs.  */
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_marvell_pj4_tune =
{
  &generic_extra_costs,			/* Insn extra costs.  */
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_v6t2_tune =
{
  &generic_extra_costs,			/* Insn extra costs.  */
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};


/* Generic Cortex tuning.  Use more specific tunings if appropriate.  */
const struct tune_params arm_cortex_tune =
{
  &generic_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_cortex_a8_tune =
{
  &cortexa8_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_cortex_a7_tune =
{
  &cortexa7_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_cortex_a15_tune =
{
  &cortexa15_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  2,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  3,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_TRUE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_ALL,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_FULL
};

const struct tune_params arm_cortex_a35_tune =
{
  &cortexa53_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  FUSE_OPS (tune_params::FUSE_MOVW_MOVT),
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_cortex_a53_tune =
{
  &cortexa53_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  FUSE_OPS (tune_params::FUSE_MOVW_MOVT | tune_params::FUSE_AES_AESMC),
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_cortex_a57_tune =
{
  &cortexa57_extra_costs,
  &generic_addr_mode_costs,		/* addressing mode costs */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  2,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  3,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_TRUE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_ALL,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  FUSE_OPS (tune_params::FUSE_MOVW_MOVT | tune_params::FUSE_AES_AESMC),
  tune_params::SCHED_AUTOPREF_FULL
};

const struct tune_params arm_exynosm1_tune =
{
  &exynosm1_extra_costs,
  &generic_addr_mode_costs,			/* Addressing mode costs.  */
  NULL,						/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  2,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  3,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_TRUE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_FALSE,	/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_FALSE,	/* ARM.  */
  tune_params::DISPARAGE_FLAGS_ALL,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_xgene1_tune =
{
  &xgene1_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  2,						/* Max cond insns.  */
  32,						/* Memset max inline.  */
  4,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_TRUE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_ALL,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

/* Branches can be dual-issued on Cortex-A5, so conditional execution is
   less appealing.  Set max_insns_skipped to a low value.  */

const struct tune_params arm_cortex_a5_tune =
{
  &cortexa5_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_cortex_a5_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  1,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_FALSE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_FALSE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_cortex_a9_tune =
{
  &cortexa9_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  cortex_a9_sched_adjust_cost,
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_BENEFICIAL(4,32,32),
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_cortex_a12_tune =
{
  &cortexa12_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,                        /* Vectorizer costs.  */
  1,						/* Constant limit.  */
  2,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_TRUE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_ALL,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  FUSE_OPS (tune_params::FUSE_MOVW_MOVT),
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_cortex_a73_tune =
{
  &cortexa57_extra_costs,
  &generic_addr_mode_costs,			/* Addressing mode costs.  */
  NULL,						/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,			/* Vectorizer costs.  */
  1,						/* Constant limit.  */
  2,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_TRUE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_ALL,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_TRUE,
  FUSE_OPS (tune_params::FUSE_AES_AESMC | tune_params::FUSE_MOVW_MOVT),
  tune_params::SCHED_AUTOPREF_FULL
};

/* armv7m tuning.  On Cortex-M4 cores for example, MOVW/MOVT take a single
   cycle to execute each.  An LDR from the constant pool also takes two cycles
   to execute, but mildly increases pipelining opportunity (consecutive
   loads/stores can be pipelined together, saving one cycle), and may also
   improve icache utilisation.  Hence we prefer the constant pool for such
   processors.  */

const struct tune_params arm_v7m_tune =
{
  &v7m_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_cortex_m_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  2,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_FALSE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_FALSE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

/* Cortex-M7 tuning.  */

const struct tune_params arm_cortex_m7_tune =
{
  &v7m_extra_costs,
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_cortex_m7_branch_cost,
  &arm_default_vec_cost,
  0,						/* Constant limit.  */
  1,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

/* The arm_v6m_tune is duplicated from arm_cortex_tune, rather than
   arm_v6t2_tune.  It is used for cortex-m0, cortex-m1, cortex-m0plus and
   cortex-m23.  */
const struct tune_params arm_v6m_tune =
{
  &generic_extra_costs,			/* Insn extra costs.  */
  &generic_addr_mode_costs,		/* Addressing mode costs.  */
  NULL,					/* Sched adj cost.  */
  arm_default_branch_cost,
  &arm_default_vec_cost,                        /* Vectorizer costs.  */
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  1,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_FALSE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_FALSE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_FALSE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

const struct tune_params arm_fa726te_tune =
{
  &generic_extra_costs,				/* Insn extra costs.  */
  &generic_addr_mode_costs,			/* Addressing mode costs.  */
  fa726te_sched_adjust_cost,
  arm_default_branch_cost,
  &arm_default_vec_cost,
  1,						/* Constant limit.  */
  5,						/* Max cond insns.  */
  8,						/* Memset max inline.  */
  2,						/* Issue rate.  */
  ARM_PREFETCH_NOT_BENEFICIAL,
  tune_params::PREF_CONST_POOL_TRUE,
  tune_params::PREF_LDRD_FALSE,
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* Thumb.  */
  tune_params::LOG_OP_NON_SHORT_CIRCUIT_TRUE,		/* ARM.  */
  tune_params::DISPARAGE_FLAGS_NEITHER,
  tune_params::PREF_NEON_64_FALSE,
  tune_params::PREF_NEON_STRINGOPS_FALSE,
  tune_params::FUSE_NOTHING,
  tune_params::SCHED_AUTOPREF_OFF
};

/* Auto-generated CPU, FPU and architecture tables.  */
#include "arm-cpu-data.h"

/* The name of the preprocessor macro to define for this architecture.  PROFILE
   is replaced by the architecture name (eg. 8A) in arm_option_override () and
   is thus chosen to be big enough to hold the longest architecture name.  */

char arm_arch_name[] = "__ARM_ARCH_PROFILE__";

/* Supported TLS relocations.  */

enum tls_reloc {
  TLS_GD32,
  TLS_LDM32,
  TLS_LDO32,
  TLS_IE32,
  TLS_LE32,
  TLS_DESCSEQ	/* GNU scheme */
};

/* The maximum number of insns to be used when loading a constant.  */
inline static int
arm_constant_limit (bool size_p)
{
  return size_p ? 1 : current_tune->constant_limit;
}

/* Emit an insn that's a simple single-set.  Both the operands must be known
   to be valid.  */
inline static rtx_insn *
emit_set_insn (rtx x, rtx y)
{
  return emit_insn (gen_rtx_SET (x, y));
}

/* Return the number of bits set in VALUE.  */
static unsigned
bit_count (unsigned long value)
{
  unsigned long count = 0;

  while (value)
    {
      count++;
      value &= value - 1;  /* Clear the least-significant set bit.  */
    }

  return count;
}

/* Return the number of bits set in BMAP.  */
static unsigned
bitmap_popcount (const sbitmap bmap)
{
  unsigned int count = 0;
  unsigned int n = 0;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_BITMAP (bmap, 0, n, sbi)
    count++;
  return count;
}

typedef struct
{
  machine_mode mode;
  const char *name;
} arm_fixed_mode_set;

/* A small helper for setting fixed-point library libfuncs.  */

static void
arm_set_fixed_optab_libfunc (optab optable, machine_mode mode,
			     const char *funcname, const char *modename,
			     int num_suffix)
{
  char buffer[50];

  if (num_suffix == 0)
    sprintf (buffer, "__gnu_%s%s", funcname, modename);
  else
    sprintf (buffer, "__gnu_%s%s%d", funcname, modename, num_suffix);

  set_optab_libfunc (optable, mode, buffer);
}

static void
arm_set_fixed_conv_libfunc (convert_optab optable, machine_mode to,
			    machine_mode from, const char *funcname,
			    const char *toname, const char *fromname)
{
  char buffer[50];
  const char *maybe_suffix_2 = "";

  /* Follow the logic for selecting a "2" suffix in fixed-bit.h.  */
  if (ALL_FIXED_POINT_MODE_P (from) && ALL_FIXED_POINT_MODE_P (to)
      && UNSIGNED_FIXED_POINT_MODE_P (from) == UNSIGNED_FIXED_POINT_MODE_P (to)
      && ALL_FRACT_MODE_P (from) == ALL_FRACT_MODE_P (to))
    maybe_suffix_2 = "2";

  sprintf (buffer, "__gnu_%s%s%s%s", funcname, fromname, toname,
	   maybe_suffix_2);

  set_conv_libfunc (optable, to, from, buffer);
}

/* Set up library functions unique to ARM.  */

static void
arm_init_libfuncs (void)
{
  /* For Linux, we have access to kernel support for atomic operations.  */
  if (arm_abi == ARM_ABI_AAPCS_LINUX)
    init_sync_libfuncs (MAX_SYNC_LIBFUNC_SIZE);

  /* There are no special library functions unless we are using the
     ARM BPABI.  */
  if (!TARGET_BPABI)
    return;

  /* The functions below are described in Section 4 of the "Run-Time
     ABI for the ARM architecture", Version 1.0.  */

  /* Double-precision floating-point arithmetic.  Table 2.  */
  set_optab_libfunc (add_optab, DFmode, "__aeabi_dadd");
  set_optab_libfunc (sdiv_optab, DFmode, "__aeabi_ddiv");
  set_optab_libfunc (smul_optab, DFmode, "__aeabi_dmul");
  set_optab_libfunc (neg_optab, DFmode, "__aeabi_dneg");
  set_optab_libfunc (sub_optab, DFmode, "__aeabi_dsub");

  /* Double-precision comparisons.  Table 3.  */
  set_optab_libfunc (eq_optab, DFmode, "__aeabi_dcmpeq");
  set_optab_libfunc (ne_optab, DFmode, NULL);
  set_optab_libfunc (lt_optab, DFmode, "__aeabi_dcmplt");
  set_optab_libfunc (le_optab, DFmode, "__aeabi_dcmple");
  set_optab_libfunc (ge_optab, DFmode, "__aeabi_dcmpge");
  set_optab_libfunc (gt_optab, DFmode, "__aeabi_dcmpgt");
  set_optab_libfunc (unord_optab, DFmode, "__aeabi_dcmpun");

  /* Single-precision floating-point arithmetic.  Table 4.  */
  set_optab_libfunc (add_optab, SFmode, "__aeabi_fadd");
  set_optab_libfunc (sdiv_optab, SFmode, "__aeabi_fdiv");
  set_optab_libfunc (smul_optab, SFmode, "__aeabi_fmul");
  set_optab_libfunc (neg_optab, SFmode, "__aeabi_fneg");
  set_optab_libfunc (sub_optab, SFmode, "__aeabi_fsub");

  /* Single-precision comparisons.  Table 5.  */
  set_optab_libfunc (eq_optab, SFmode, "__aeabi_fcmpeq");
  set_optab_libfunc (ne_optab, SFmode, NULL);
  set_optab_libfunc (lt_optab, SFmode, "__aeabi_fcmplt");
  set_optab_libfunc (le_optab, SFmode, "__aeabi_fcmple");
  set_optab_libfunc (ge_optab, SFmode, "__aeabi_fcmpge");
  set_optab_libfunc (gt_optab, SFmode, "__aeabi_fcmpgt");
  set_optab_libfunc (unord_optab, SFmode, "__aeabi_fcmpun");

  /* Floating-point to integer conversions.  Table 6.  */
  set_conv_libfunc (sfix_optab, SImode, DFmode, "__aeabi_d2iz");
  set_conv_libfunc (ufix_optab, SImode, DFmode, "__aeabi_d2uiz");
  set_conv_libfunc (sfix_optab, DImode, DFmode, "__aeabi_d2lz");
  set_conv_libfunc (ufix_optab, DImode, DFmode, "__aeabi_d2ulz");
  set_conv_libfunc (sfix_optab, SImode, SFmode, "__aeabi_f2iz");
  set_conv_libfunc (ufix_optab, SImode, SFmode, "__aeabi_f2uiz");
  set_conv_libfunc (sfix_optab, DImode, SFmode, "__aeabi_f2lz");
  set_conv_libfunc (ufix_optab, DImode, SFmode, "__aeabi_f2ulz");

  /* Conversions between floating types.  Table 7.  */
  set_conv_libfunc (trunc_optab, SFmode, DFmode, "__aeabi_d2f");
  set_conv_libfunc (sext_optab, DFmode, SFmode, "__aeabi_f2d");

  /* Integer to floating-point conversions.  Table 8.  */
  set_conv_libfunc (sfloat_optab, DFmode, SImode, "__aeabi_i2d");
  set_conv_libfunc (ufloat_optab, DFmode, SImode, "__aeabi_ui2d");
  set_conv_libfunc (sfloat_optab, DFmode, DImode, "__aeabi_l2d");
  set_conv_libfunc (ufloat_optab, DFmode, DImode, "__aeabi_ul2d");
  set_conv_libfunc (sfloat_optab, SFmode, SImode, "__aeabi_i2f");
  set_conv_libfunc (ufloat_optab, SFmode, SImode, "__aeabi_ui2f");
  set_conv_libfunc (sfloat_optab, SFmode, DImode, "__aeabi_l2f");
  set_conv_libfunc (ufloat_optab, SFmode, DImode, "__aeabi_ul2f");

  /* Long long.  Table 9.  */
  set_optab_libfunc (smul_optab, DImode, "__aeabi_lmul");
  set_optab_libfunc (sdivmod_optab, DImode, "__aeabi_ldivmod");
  set_optab_libfunc (udivmod_optab, DImode, "__aeabi_uldivmod");
  set_optab_libfunc (ashl_optab, DImode, "__aeabi_llsl");
  set_optab_libfunc (lshr_optab, DImode, "__aeabi_llsr");
  set_optab_libfunc (ashr_optab, DImode, "__aeabi_lasr");
  set_optab_libfunc (cmp_optab, DImode, "__aeabi_lcmp");
  set_optab_libfunc (ucmp_optab, DImode, "__aeabi_ulcmp");

  /* Integer (32/32->32) division.  \S 4.3.1.  */
  set_optab_libfunc (sdivmod_optab, SImode, "__aeabi_idivmod");
  set_optab_libfunc (udivmod_optab, SImode, "__aeabi_uidivmod");

  /* The divmod functions are designed so that they can be used for
     plain division, even though they return both the quotient and the
     remainder.  The quotient is returned in the usual location (i.e.,
     r0 for SImode, {r0, r1} for DImode), just as would be expected
     for an ordinary division routine.  Because the AAPCS calling
     conventions specify that all of { r0, r1, r2, r3 } are
     callee-saved registers, there is no need to tell the compiler
     explicitly that those registers are clobbered by these
     routines.  */
  set_optab_libfunc (sdiv_optab, DImode, "__aeabi_ldivmod");
  set_optab_libfunc (udiv_optab, DImode, "__aeabi_uldivmod");

  /* For SImode division the ABI provides div-without-mod routines,
     which are faster.  */
  set_optab_libfunc (sdiv_optab, SImode, "__aeabi_idiv");
  set_optab_libfunc (udiv_optab, SImode, "__aeabi_uidiv");

  /* We don't have mod libcalls.  Fortunately gcc knows how to use the
     divmod libcalls instead.  */
  set_optab_libfunc (smod_optab, DImode, NULL);
  set_optab_libfunc (umod_optab, DImode, NULL);
  set_optab_libfunc (smod_optab, SImode, NULL);
  set_optab_libfunc (umod_optab, SImode, NULL);

  /* Half-precision float operations.  The compiler handles all operations
     with NULL libfuncs by converting the SFmode.  */
  switch (arm_fp16_format)
    {
    case ARM_FP16_FORMAT_IEEE:
    case ARM_FP16_FORMAT_ALTERNATIVE:

      /* Conversions.  */
      set_conv_libfunc (trunc_optab, HFmode, SFmode,
			(arm_fp16_format == ARM_FP16_FORMAT_IEEE
			 ? "__gnu_f2h_ieee"
			 : "__gnu_f2h_alternative"));
      set_conv_libfunc (sext_optab, SFmode, HFmode,
			(arm_fp16_format == ARM_FP16_FORMAT_IEEE
			 ? "__gnu_h2f_ieee"
			 : "__gnu_h2f_alternative"));

      set_conv_libfunc (trunc_optab, HFmode, DFmode,
			(arm_fp16_format == ARM_FP16_FORMAT_IEEE
			 ? "__gnu_d2h_ieee"
			 : "__gnu_d2h_alternative"));

      /* Arithmetic.  */
      set_optab_libfunc (add_optab, HFmode, NULL);
      set_optab_libfunc (sdiv_optab, HFmode, NULL);
      set_optab_libfunc (smul_optab, HFmode, NULL);
      set_optab_libfunc (neg_optab, HFmode, NULL);
      set_optab_libfunc (sub_optab, HFmode, NULL);

      /* Comparisons.  */
      set_optab_libfunc (eq_optab, HFmode, NULL);
      set_optab_libfunc (ne_optab, HFmode, NULL);
      set_optab_libfunc (lt_optab, HFmode, NULL);
      set_optab_libfunc (le_optab, HFmode, NULL);
      set_optab_libfunc (ge_optab, HFmode, NULL);
      set_optab_libfunc (gt_optab, HFmode, NULL);
      set_optab_libfunc (unord_optab, HFmode, NULL);
      break;

    default:
      break;
    }

  /* Use names prefixed with __gnu_ for fixed-point helper functions.  */
  {
    const arm_fixed_mode_set fixed_arith_modes[] =
      {
	{ E_QQmode, "qq" },
	{ E_UQQmode, "uqq" },
	{ E_HQmode, "hq" },
	{ E_UHQmode, "uhq" },
	{ E_SQmode, "sq" },
	{ E_USQmode, "usq" },
	{ E_DQmode, "dq" },
	{ E_UDQmode, "udq" },
	{ E_TQmode, "tq" },
	{ E_UTQmode, "utq" },
	{ E_HAmode, "ha" },
	{ E_UHAmode, "uha" },
	{ E_SAmode, "sa" },
	{ E_USAmode, "usa" },
	{ E_DAmode, "da" },
	{ E_UDAmode, "uda" },
	{ E_TAmode, "ta" },
	{ E_UTAmode, "uta" }
      };
    const arm_fixed_mode_set fixed_conv_modes[] =
      {
	{ E_QQmode, "qq" },
	{ E_UQQmode, "uqq" },
	{ E_HQmode, "hq" },
	{ E_UHQmode, "uhq" },
	{ E_SQmode, "sq" },
	{ E_USQmode, "usq" },
	{ E_DQmode, "dq" },
	{ E_UDQmode, "udq" },
	{ E_TQmode, "tq" },
	{ E_UTQmode, "utq" },
	{ E_HAmode, "ha" },
	{ E_UHAmode, "uha" },
	{ E_SAmode, "sa" },
	{ E_USAmode, "usa" },
	{ E_DAmode, "da" },
	{ E_UDAmode, "uda" },
	{ E_TAmode, "ta" },
	{ E_UTAmode, "uta" },
	{ E_QImode, "qi" },
	{ E_HImode, "hi" },
	{ E_SImode, "si" },
	{ E_DImode, "di" },
	{ E_TImode, "ti" },
	{ E_SFmode, "sf" },
	{ E_DFmode, "df" }
      };
    unsigned int i, j;

    for (i = 0; i < ARRAY_SIZE (fixed_arith_modes); i++)
      {
	arm_set_fixed_optab_libfunc (add_optab, fixed_arith_modes[i].mode,
				     "add", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (ssadd_optab, fixed_arith_modes[i].mode,
				     "ssadd", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (usadd_optab, fixed_arith_modes[i].mode,
				     "usadd", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (sub_optab, fixed_arith_modes[i].mode,
				     "sub", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (sssub_optab, fixed_arith_modes[i].mode,
				     "sssub", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (ussub_optab, fixed_arith_modes[i].mode,
				     "ussub", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (smul_optab, fixed_arith_modes[i].mode,
				     "mul", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (ssmul_optab, fixed_arith_modes[i].mode,
				     "ssmul", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (usmul_optab, fixed_arith_modes[i].mode,
				     "usmul", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (sdiv_optab, fixed_arith_modes[i].mode,
				     "div", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (udiv_optab, fixed_arith_modes[i].mode,
				     "udiv", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (ssdiv_optab, fixed_arith_modes[i].mode,
				     "ssdiv", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (usdiv_optab, fixed_arith_modes[i].mode,
				     "usdiv", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (neg_optab, fixed_arith_modes[i].mode,
				     "neg", fixed_arith_modes[i].name, 2);
	arm_set_fixed_optab_libfunc (ssneg_optab, fixed_arith_modes[i].mode,
				     "ssneg", fixed_arith_modes[i].name, 2);
	arm_set_fixed_optab_libfunc (usneg_optab, fixed_arith_modes[i].mode,
				     "usneg", fixed_arith_modes[i].name, 2);
	arm_set_fixed_optab_libfunc (ashl_optab, fixed_arith_modes[i].mode,
				     "ashl", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (ashr_optab, fixed_arith_modes[i].mode,
				     "ashr", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (lshr_optab, fixed_arith_modes[i].mode,
				     "lshr", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (ssashl_optab, fixed_arith_modes[i].mode,
				     "ssashl", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (usashl_optab, fixed_arith_modes[i].mode,
				     "usashl", fixed_arith_modes[i].name, 3);
	arm_set_fixed_optab_libfunc (cmp_optab, fixed_arith_modes[i].mode,
				     "cmp", fixed_arith_modes[i].name, 2);
      }

    for (i = 0; i < ARRAY_SIZE (fixed_conv_modes); i++)
      for (j = 0; j < ARRAY_SIZE (fixed_conv_modes); j++)
	{
	  if (i == j
	      || (!ALL_FIXED_POINT_MODE_P (fixed_conv_modes[i].mode)
		  && !ALL_FIXED_POINT_MODE_P (fixed_conv_modes[j].mode)))
	    continue;

	  arm_set_fixed_conv_libfunc (fract_optab, fixed_conv_modes[i].mode,
				      fixed_conv_modes[j].mode, "fract",
				      fixed_conv_modes[i].name,
				      fixed_conv_modes[j].name);
	  arm_set_fixed_conv_libfunc (satfract_optab,
				      fixed_conv_modes[i].mode,
				      fixed_conv_modes[j].mode, "satfract",
				      fixed_conv_modes[i].name,
				      fixed_conv_modes[j].name);
	  arm_set_fixed_conv_libfunc (fractuns_optab,
				      fixed_conv_modes[i].mode,
				      fixed_conv_modes[j].mode, "fractuns",
				      fixed_conv_modes[i].name,
				      fixed_conv_modes[j].name);
	  arm_set_fixed_conv_libfunc (satfractuns_optab,
				      fixed_conv_modes[i].mode,
				      fixed_conv_modes[j].mode, "satfractuns",
				      fixed_conv_modes[i].name,
				      fixed_conv_modes[j].name);
	}
  }

  if (TARGET_AAPCS_BASED)
    synchronize_libfunc = init_one_libfunc ("__sync_synchronize");
}

/* On AAPCS systems, this is the "struct __va_list".  */
static GTY(()) tree va_list_type;

/* Return the type to use as __builtin_va_list.  */
static tree
arm_build_builtin_va_list (void)
{
  tree va_list_name;
  tree ap_field;

  if (!TARGET_AAPCS_BASED)
    return std_build_builtin_va_list ();

  /* AAPCS \S 7.1.4 requires that va_list be a typedef for a type
     defined as:

       struct __va_list
       {
	 void *__ap;
       };

     The C Library ABI further reinforces this definition in \S
     4.1.

     We must follow this definition exactly.  The structure tag
     name is visible in C++ mangled names, and thus forms a part
     of the ABI.  The field name may be used by people who
     #include <stdarg.h>.  */
  /* Create the type.  */
  va_list_type = lang_hooks.types.make_type (RECORD_TYPE);
  /* Give it the required name.  */
  va_list_name = build_decl (BUILTINS_LOCATION,
			     TYPE_DECL,
			     get_identifier ("__va_list"),
			     va_list_type);
  DECL_ARTIFICIAL (va_list_name) = 1;
  TYPE_NAME (va_list_type) = va_list_name;
  TYPE_STUB_DECL (va_list_type) = va_list_name;
  /* Create the __ap field.  */
  ap_field = build_decl (BUILTINS_LOCATION,
			 FIELD_DECL,
			 get_identifier ("__ap"),
			 ptr_type_node);
  DECL_ARTIFICIAL (ap_field) = 1;
  DECL_FIELD_CONTEXT (ap_field) = va_list_type;
  TYPE_FIELDS (va_list_type) = ap_field;
  /* Compute its layout.  */
  layout_type (va_list_type);

  return va_list_type;
}

/* Return an expression of type "void *" pointing to the next
   available argument in a variable-argument list.  VALIST is the
   user-level va_list object, of type __builtin_va_list.  */
static tree
arm_extract_valist_ptr (tree valist)
{
  if (TREE_TYPE (valist) == error_mark_node)
    return error_mark_node;

  /* On an AAPCS target, the pointer is stored within "struct
     va_list".  */
  if (TARGET_AAPCS_BASED)
    {
      tree ap_field = TYPE_FIELDS (TREE_TYPE (valist));
      valist = build3 (COMPONENT_REF, TREE_TYPE (ap_field),
		       valist, ap_field, NULL_TREE);
    }

  return valist;
}

/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */
static void
arm_expand_builtin_va_start (tree valist, rtx nextarg)
{
  valist = arm_extract_valist_ptr (valist);
  std_expand_builtin_va_start (valist, nextarg);
}

/* Implement TARGET_GIMPLIFY_VA_ARG_EXPR.  */
static tree
arm_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			  gimple_seq *post_p)
{
  valist = arm_extract_valist_ptr (valist);
  return std_gimplify_va_arg_expr (valist, type, pre_p, post_p);
}

/* Check any incompatible options that the user has specified.  */
static void
arm_option_check_internal (struct gcc_options *opts)
{
  int flags = opts->x_target_flags;

  /* iWMMXt and NEON are incompatible.  */
  if (TARGET_IWMMXT
      && bitmap_bit_p (arm_active_target.isa, isa_bit_neon))
    error ("iWMMXt and NEON are incompatible");

  /* Make sure that the processor choice does not conflict with any of the
     other command line choices.  */
  if (TARGET_ARM_P (flags)
      && !bitmap_bit_p (arm_active_target.isa, isa_bit_notm))
    error ("target CPU does not support ARM mode");

  /* TARGET_BACKTRACE cannot be used here as crtl->is_leaf is not set yet.  */
  if ((TARGET_TPCS_FRAME || TARGET_TPCS_LEAF_FRAME) && TARGET_ARM_P (flags))
    warning (0, "enabling backtrace support is only meaningful when compiling for the Thumb");

  if (TARGET_ARM_P (flags) && TARGET_CALLEE_INTERWORKING)
    warning (0, "enabling callee interworking support is only meaningful when compiling for the Thumb");

  /* If this target is normally configured to use APCS frames, warn if they
     are turned off and debugging is turned on.  */
  if (TARGET_ARM_P (flags)
      && write_symbols != NO_DEBUG
      && !TARGET_APCS_FRAME
      && (TARGET_DEFAULT & MASK_APCS_FRAME))
    warning (0, "-g with -mno-apcs-frame may not give sensible debugging");

  /* iWMMXt unsupported under Thumb mode.  */
  if (TARGET_THUMB_P (flags) && TARGET_IWMMXT)
    error ("iWMMXt unsupported under Thumb mode");

  if (TARGET_HARD_TP && TARGET_THUMB1_P (flags))
    error ("can not use -mtp=cp15 with 16-bit Thumb");

  if (TARGET_THUMB_P (flags) && TARGET_VXWORKS_RTP && flag_pic)
    {
      error ("RTP PIC is incompatible with Thumb");
      flag_pic = 0;
    }

  /* We only support -mpure-code and -mslow-flash-data on M-profile targets
     with MOVT.  */
  if ((target_pure_code || target_slow_flash_data)
      && (!TARGET_HAVE_MOVT || arm_arch_notm || flag_pic || TARGET_NEON))
    {
      const char *flag = (target_pure_code ? "-mpure-code" :
					     "-mslow-flash-data");
      error ("%s only supports non-pic code on M-profile targets with the "
	     "MOVT instruction", flag);
    }

}

/* Recompute the global settings depending on target attribute options.  */

static void
arm_option_params_internal (void)
{
  /* If we are not using the default (ARM mode) section anchor offset
     ranges, then set the correct ranges now.  */
  if (TARGET_THUMB1)
    {
      /* Thumb-1 LDR instructions cannot have negative offsets.
         Permissible positive offset ranges are 5-bit (for byte loads),
         6-bit (for halfword loads), or 7-bit (for word loads).
         Empirical results suggest a 7-bit anchor range gives the best
         overall code size.  */
      targetm.min_anchor_offset = 0;
      targetm.max_anchor_offset = 127;
    }
  else if (TARGET_THUMB2)
    {
      /* The minimum is set such that the total size of the block
         for a particular anchor is 248 + 1 + 4095 bytes, which is
         divisible by eight, ensuring natural spacing of anchors.  */
      targetm.min_anchor_offset = -248;
      targetm.max_anchor_offset = 4095;
    }
  else
    {
      targetm.min_anchor_offset = TARGET_MIN_ANCHOR_OFFSET;
      targetm.max_anchor_offset = TARGET_MAX_ANCHOR_OFFSET;
    }

  /* Increase the number of conditional instructions with -Os.  */
  max_insns_skipped = optimize_size ? 4 : current_tune->max_insns_skipped;

  /* For THUMB2, we limit the conditional sequence to one IT block.  */
  if (TARGET_THUMB2)
    max_insns_skipped = MIN (max_insns_skipped, MAX_INSN_PER_IT_BLOCK);
}

/* True if -mflip-thumb should next add an attribute for the default
   mode, false if it should next add an attribute for the opposite mode.  */
static GTY(()) bool thumb_flipper;

/* Options after initial target override.  */
static GTY(()) tree init_optimize;

static void
arm_override_options_after_change_1 (struct gcc_options *opts)
{
  if (opts->x_align_functions <= 0)
    opts->x_align_functions = TARGET_THUMB_P (opts->x_target_flags)
      && opts->x_optimize_size ? 2 : 4;
}

/* Implement targetm.override_options_after_change.  */

static void
arm_override_options_after_change (void)
{
  arm_configure_build_target (&arm_active_target,
			      TREE_TARGET_OPTION (target_option_default_node),
			      &global_options_set, false);

  arm_override_options_after_change_1 (&global_options);
}

/* Implement TARGET_OPTION_SAVE.  */
static void
arm_option_save (struct cl_target_option *ptr, struct gcc_options *opts)
{
  ptr->x_arm_arch_string = opts->x_arm_arch_string;
  ptr->x_arm_cpu_string = opts->x_arm_cpu_string;
  ptr->x_arm_tune_string = opts->x_arm_tune_string;
}

/* Implement TARGET_OPTION_RESTORE.  */
static void
arm_option_restore (struct gcc_options *opts, struct cl_target_option *ptr)
{
  opts->x_arm_arch_string = ptr->x_arm_arch_string;
  opts->x_arm_cpu_string = ptr->x_arm_cpu_string;
  opts->x_arm_tune_string = ptr->x_arm_tune_string;
  arm_configure_build_target (&arm_active_target, ptr, &global_options_set,
			      false);
}

/* Reset options between modes that the user has specified.  */
static void
arm_option_override_internal (struct gcc_options *opts,
			      struct gcc_options *opts_set)
{
  arm_override_options_after_change_1 (opts);

  if (TARGET_INTERWORK && !bitmap_bit_p (arm_active_target.isa, isa_bit_thumb))
    {
      /* The default is to enable interworking, so this warning message would
	 be confusing to users who have just compiled with, eg, -march=armv3.  */
      /* warning (0, "ignoring -minterwork because target CPU does not support THUMB"); */
      opts->x_target_flags &= ~MASK_INTERWORK;
    }

  if (TARGET_THUMB_P (opts->x_target_flags)
      && !bitmap_bit_p (arm_active_target.isa, isa_bit_thumb))
    {
      warning (0, "target CPU does not support THUMB instructions");
      opts->x_target_flags &= ~MASK_THUMB;
    }

  if (TARGET_APCS_FRAME && TARGET_THUMB_P (opts->x_target_flags))
    {
      /* warning (0, "ignoring -mapcs-frame because -mthumb was used"); */
      opts->x_target_flags &= ~MASK_APCS_FRAME;
    }

  /* Callee super interworking implies thumb interworking.  Adding
     this to the flags here simplifies the logic elsewhere.  */
  if (TARGET_THUMB_P (opts->x_target_flags) && TARGET_CALLEE_INTERWORKING)
    opts->x_target_flags |= MASK_INTERWORK;

  /* need to remember initial values so combinaisons of options like
     -mflip-thumb -mthumb -fno-schedule-insns work for any attribute.  */
  cl_optimization *to = TREE_OPTIMIZATION (init_optimize);

  if (! opts_set->x_arm_restrict_it)
    opts->x_arm_restrict_it = arm_arch8;

  /* ARM execution state and M profile don't have [restrict] IT.  */
  if (!TARGET_THUMB2_P (opts->x_target_flags) || !arm_arch_notm)
    opts->x_arm_restrict_it = 0;

  /* Enable -munaligned-access by default for
     - all ARMv6 architecture-based processors when compiling for a 32-bit ISA
     i.e. Thumb2 and ARM state only.
     - ARMv7-A, ARMv7-R, and ARMv7-M architecture-based processors.
     - ARMv8 architecture-base processors.

     Disable -munaligned-access by default for
     - all pre-ARMv6 architecture-based processors
     - ARMv6-M architecture-based processors
     - ARMv8-M Baseline processors.  */

  if (! opts_set->x_unaligned_access)
    {
      opts->x_unaligned_access = (TARGET_32BIT_P (opts->x_target_flags)
			  && arm_arch6 && (arm_arch_notm || arm_arch7));
    }
  else if (opts->x_unaligned_access == 1
	   && !(arm_arch6 && (arm_arch_notm || arm_arch7)))
    {
      warning (0, "target CPU does not support unaligned accesses");
     opts->x_unaligned_access = 0;
    }

  /* Don't warn since it's on by default in -O2.  */
  if (TARGET_THUMB1_P (opts->x_target_flags))
    opts->x_flag_schedule_insns = 0;
  else
    opts->x_flag_schedule_insns = to->x_flag_schedule_insns;

  /* Disable shrink-wrap when optimizing function for size, since it tends to
     generate additional returns.  */
  if (optimize_function_for_size_p (cfun)
      && TARGET_THUMB2_P (opts->x_target_flags))
    opts->x_flag_shrink_wrap = false;
  else
    opts->x_flag_shrink_wrap = to->x_flag_shrink_wrap;

  /* In Thumb1 mode, we emit the epilogue in RTL, but the last insn
     - epilogue_insns - does not accurately model the corresponding insns
     emitted in the asm file.  In particular, see the comment in thumb_exit
     'Find out how many of the (return) argument registers we can corrupt'.
     As a consequence, the epilogue may clobber registers without fipa-ra
     finding out about it.  Therefore, disable fipa-ra in Thumb1 mode.
     TODO: Accurately model clobbers for epilogue_insns and reenable
     fipa-ra.  */
  if (TARGET_THUMB1_P (opts->x_target_flags))
    opts->x_flag_ipa_ra = 0;
  else
    opts->x_flag_ipa_ra = to->x_flag_ipa_ra;

  /* Thumb2 inline assembly code should always use unified syntax.
     This will apply to ARM and Thumb1 eventually.  */
  opts->x_inline_asm_unified = TARGET_THUMB2_P (opts->x_target_flags);

#ifdef SUBTARGET_OVERRIDE_INTERNAL_OPTIONS
  SUBTARGET_OVERRIDE_INTERNAL_OPTIONS;
#endif
}

static sbitmap isa_all_fpubits;
static sbitmap isa_quirkbits;

/* Configure a build target TARGET from the user-specified options OPTS and
   OPTS_SET.  If WARN_COMPATIBLE, emit a diagnostic if both the CPU and
   architecture have been specified, but the two are not identical.  */
void
arm_configure_build_target (struct arm_build_target *target,
			    struct cl_target_option *opts,
			    struct gcc_options *opts_set,
			    bool warn_compatible)
{
  const cpu_option *arm_selected_tune = NULL;
  const arch_option *arm_selected_arch = NULL;
  const cpu_option *arm_selected_cpu = NULL;
  const arm_fpu_desc *arm_selected_fpu = NULL;
  const char *tune_opts = NULL;
  const char *arch_opts = NULL;
  const char *cpu_opts = NULL;

  bitmap_clear (target->isa);
  target->core_name = NULL;
  target->arch_name = NULL;

  if (opts_set->x_arm_arch_string)
    {
      arm_selected_arch = arm_parse_arch_option_name (all_architectures,
						      "-march",
						      opts->x_arm_arch_string);
      arch_opts = strchr (opts->x_arm_arch_string, '+');
    }

  if (opts_set->x_arm_cpu_string)
    {
      arm_selected_cpu = arm_parse_cpu_option_name (all_cores, "-mcpu",
						    opts->x_arm_cpu_string);
      cpu_opts = strchr (opts->x_arm_cpu_string, '+');
      arm_selected_tune = arm_selected_cpu;
      /* If taking the tuning from -mcpu, we don't need to rescan the
	 options for tuning.  */
    }

  if (opts_set->x_arm_tune_string)
    {
      arm_selected_tune = arm_parse_cpu_option_name (all_cores, "-mtune",
						     opts->x_arm_tune_string);
      tune_opts = strchr (opts->x_arm_tune_string, '+');
    }

  if (arm_selected_arch)
    {
      arm_initialize_isa (target->isa, arm_selected_arch->common.isa_bits);
      arm_parse_option_features (target->isa, &arm_selected_arch->common,
				 arch_opts);

      if (arm_selected_cpu)
	{
	  auto_sbitmap cpu_isa (isa_num_bits);
	  auto_sbitmap isa_delta (isa_num_bits);

	  arm_initialize_isa (cpu_isa, arm_selected_cpu->common.isa_bits);
	  arm_parse_option_features (cpu_isa, &arm_selected_cpu->common,
				     cpu_opts);
	  bitmap_xor (isa_delta, cpu_isa, target->isa);
	  /* Ignore any bits that are quirk bits.  */
	  bitmap_and_compl (isa_delta, isa_delta, isa_quirkbits);
	  /* Ignore (for now) any bits that might be set by -mfpu.  */
	  bitmap_and_compl (isa_delta, isa_delta, isa_all_fpubits);

	  if (!bitmap_empty_p (isa_delta))
	    {
	      if (warn_compatible)
		warning (0, "switch -mcpu=%s conflicts with -march=%s switch",
			 arm_selected_cpu->common.name,
			 arm_selected_arch->common.name);
	      /* -march wins for code generation.
		 -mcpu wins for default tuning.  */
	      if (!arm_selected_tune)
		arm_selected_tune = arm_selected_cpu;

	      arm_selected_cpu = all_cores + arm_selected_arch->tune_id;
	      target->arch_name = arm_selected_arch->common.name;
	    }
	  else
	    {
	      /* Architecture and CPU are essentially the same.
		 Prefer the CPU setting.  */
	      arm_selected_arch = all_architectures + arm_selected_cpu->arch;
	      target->core_name = arm_selected_cpu->common.name;
	      /* Copy the CPU's capabilities, so that we inherit the
		 appropriate extensions and quirks.  */
	      bitmap_copy (target->isa, cpu_isa);
	    }
	}
      else
	{
	  /* Pick a CPU based on the architecture.  */
	  arm_selected_cpu = all_cores + arm_selected_arch->tune_id;
	  target->arch_name = arm_selected_arch->common.name;
	  /* Note: target->core_name is left unset in this path.  */
	}
    }
  else if (arm_selected_cpu)
    {
      target->core_name = arm_selected_cpu->common.name;
      arm_initialize_isa (target->isa, arm_selected_cpu->common.isa_bits);
      arm_parse_option_features (target->isa, &arm_selected_cpu->common,
				 cpu_opts);
      arm_selected_arch = all_architectures + arm_selected_cpu->arch;
    }
  /* If the user did not specify a processor or architecture, choose
     one for them.  */
  else
    {
      const cpu_option *sel;
      auto_sbitmap sought_isa (isa_num_bits);
      bitmap_clear (sought_isa);
      auto_sbitmap default_isa (isa_num_bits);

      arm_selected_cpu = arm_parse_cpu_option_name (all_cores, "default CPU",
						    TARGET_CPU_DEFAULT);
      cpu_opts = strchr (TARGET_CPU_DEFAULT, '+');
      gcc_assert (arm_selected_cpu->common.name);

      /* RWE: All of the selection logic below (to the end of this
	 'if' clause) looks somewhat suspect.  It appears to be mostly
	 there to support forcing thumb support when the default CPU
	 does not have thumb (somewhat dubious in terms of what the
	 user might be expecting).  I think it should be removed once
	 support for the pre-thumb era cores is removed.  */
      sel = arm_selected_cpu;
      arm_initialize_isa (default_isa, sel->common.isa_bits);
      arm_parse_option_features (default_isa, &arm_selected_cpu->common,
				 cpu_opts);

      /* Now check to see if the user has specified any command line
	 switches that require certain abilities from the cpu.  */

      if (TARGET_INTERWORK || TARGET_THUMB)
	{
	  bitmap_set_bit (sought_isa, isa_bit_thumb);
	  bitmap_set_bit (sought_isa, isa_bit_mode32);

	  /* There are no ARM processors that support both APCS-26 and
	     interworking.  Therefore we forcibly remove MODE26 from
	     from the isa features here (if it was set), so that the
	     search below will always be able to find a compatible
	     processor.  */
	  bitmap_clear_bit (default_isa, isa_bit_mode26);
	}

      /* If there are such requirements and the default CPU does not
	 satisfy them, we need to run over the complete list of
	 cores looking for one that is satisfactory.  */
      if (!bitmap_empty_p (sought_isa)
	  && !bitmap_subset_p (sought_isa, default_isa))
	{
	  auto_sbitmap candidate_isa (isa_num_bits);
	  /* We're only interested in a CPU with at least the
	     capabilities of the default CPU and the required
	     additional features.  */
	  bitmap_ior (default_isa, default_isa, sought_isa);

	  /* Try to locate a CPU type that supports all of the abilities
	     of the default CPU, plus the extra abilities requested by
	     the user.  */
	  for (sel = all_cores; sel->common.name != NULL; sel++)
	    {
	      arm_initialize_isa (candidate_isa, sel->common.isa_bits);
	      /* An exact match?  */
	      if (bitmap_equal_p (default_isa, candidate_isa))
		break;
	    }

	  if (sel->common.name == NULL)
	    {
	      unsigned current_bit_count = isa_num_bits;
	      const cpu_option *best_fit = NULL;

	      /* Ideally we would like to issue an error message here
		 saying that it was not possible to find a CPU compatible
		 with the default CPU, but which also supports the command
		 line options specified by the programmer, and so they
		 ought to use the -mcpu=<name> command line option to
		 override the default CPU type.

		 If we cannot find a CPU that has exactly the
		 characteristics of the default CPU and the given
		 command line options we scan the array again looking
		 for a best match.  The best match must have at least
		 the capabilities of the perfect match.  */
	      for (sel = all_cores; sel->common.name != NULL; sel++)
		{
		  arm_initialize_isa (candidate_isa, sel->common.isa_bits);

		  if (bitmap_subset_p (default_isa, candidate_isa))
		    {
		      unsigned count;

		      bitmap_and_compl (candidate_isa, candidate_isa,
					default_isa);
		      count = bitmap_popcount (candidate_isa);

		      if (count < current_bit_count)
			{
			  best_fit = sel;
			  current_bit_count = count;
			}
		    }

		  gcc_assert (best_fit);
		  sel = best_fit;
		}
	    }
	  arm_selected_cpu = sel;
	}

      /* Now we know the CPU, we can finally initialize the target
	 structure.  */
      target->core_name = arm_selected_cpu->common.name;
      arm_initialize_isa (target->isa, arm_selected_cpu->common.isa_bits);
      arm_parse_option_features (target->isa, &arm_selected_cpu->common,
				 cpu_opts);
      arm_selected_arch = all_architectures + arm_selected_cpu->arch;
    }

  gcc_assert (arm_selected_cpu);
  gcc_assert (arm_selected_arch);

  if (opts->x_arm_fpu_index != TARGET_FPU_auto)
    {
      arm_selected_fpu = &all_fpus[opts->x_arm_fpu_index];
      auto_sbitmap fpu_bits (isa_num_bits);

      arm_initialize_isa (fpu_bits, arm_selected_fpu->isa_bits);
      bitmap_and_compl (target->isa, target->isa, isa_all_fpubits);
      bitmap_ior (target->isa, target->isa, fpu_bits);
    }

  if (!arm_selected_tune)
    arm_selected_tune = arm_selected_cpu;
  else /* Validate the features passed to -mtune.  */
    arm_parse_option_features (NULL, &arm_selected_tune->common, tune_opts);

  const cpu_tune *tune_data = &all_tunes[arm_selected_tune - all_cores];

  /* Finish initializing the target structure.  */
  target->arch_pp_name = arm_selected_arch->arch;
  target->base_arch = arm_selected_arch->base_arch;
  target->profile = arm_selected_arch->profile;

  target->tune_flags = tune_data->tune_flags;
  target->tune = tune_data->tune;
  target->tune_core = tune_data->scheduler;
  arm_option_reconfigure_globals ();
}

/* Fix up any incompatible options that the user has specified.  */
static void
arm_option_override (void)
{
  static const enum isa_feature fpu_bitlist[]
    = { ISA_ALL_FPU_INTERNAL, isa_nobit };
  static const enum isa_feature quirk_bitlist[] = { ISA_ALL_QUIRKS, isa_nobit};
  cl_target_option opts;

  isa_quirkbits = sbitmap_alloc (isa_num_bits);
  arm_initialize_isa (isa_quirkbits, quirk_bitlist);

  isa_all_fpubits = sbitmap_alloc (isa_num_bits);
  arm_initialize_isa (isa_all_fpubits, fpu_bitlist);

  arm_active_target.isa = sbitmap_alloc (isa_num_bits);

  if (!global_options_set.x_arm_fpu_index)
    {
      bool ok;
      int fpu_index;

      ok = opt_enum_arg_to_value (OPT_mfpu_, FPUTYPE_AUTO, &fpu_index,
				  CL_TARGET);
      gcc_assert (ok);
      arm_fpu_index = (enum fpu_type) fpu_index;
    }

  cl_target_option_save (&opts, &global_options);
  arm_configure_build_target (&arm_active_target, &opts, &global_options_set,
			      true);

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* Initialize boolean versions of the architectural flags, for use
     in the arm.md file and for enabling feature flags.  */
  arm_option_reconfigure_globals ();

  arm_tune = arm_active_target.tune_core;
  tune_flags = arm_active_target.tune_flags;
  current_tune = arm_active_target.tune;

  /* TBD: Dwarf info for apcs frame is not handled yet.  */
  if (TARGET_APCS_FRAME)
    flag_shrink_wrap = false;

  if (TARGET_APCS_STACK && !TARGET_APCS_FRAME)
    {
      warning (0, "-mapcs-stack-check incompatible with -mno-apcs-frame");
      target_flags |= MASK_APCS_FRAME;
    }

  if (TARGET_POKE_FUNCTION_NAME)
    target_flags |= MASK_APCS_FRAME;

  if (TARGET_APCS_REENT && flag_pic)
    error ("-fpic and -mapcs-reent are incompatible");

  if (TARGET_APCS_REENT)
    warning (0, "APCS reentrant code not supported.  Ignored");

  /* Set up some tuning parameters.  */
  arm_ld_sched = (tune_flags & TF_LDSCHED) != 0;
  arm_tune_strongarm = (tune_flags & TF_STRONG) != 0;
  arm_tune_wbuf = (tune_flags & TF_WBUF) != 0;
  arm_tune_xscale = (tune_flags & TF_XSCALE) != 0;
  arm_tune_cortex_a9 = (arm_tune == TARGET_CPU_cortexa9) != 0;
  arm_m_profile_small_mul = (tune_flags & TF_SMALLMUL) != 0;

  /* For arm2/3 there is no need to do any scheduling if we are doing
     software floating-point.  */
  if (TARGET_SOFT_FLOAT && (tune_flags & TF_NO_MODE32))
    flag_schedule_insns = flag_schedule_insns_after_reload = 0;

  /* Override the default structure alignment for AAPCS ABI.  */
  if (!global_options_set.x_arm_structure_size_boundary)
    {
      if (TARGET_AAPCS_BASED)
	arm_structure_size_boundary = 8;
    }
  else
    {
      warning (0, "option %<-mstructure-size-boundary%> is deprecated");

      if (arm_structure_size_boundary != 8
	  && arm_structure_size_boundary != 32
	  && !(ARM_DOUBLEWORD_ALIGN && arm_structure_size_boundary == 64))
	{
	  if (ARM_DOUBLEWORD_ALIGN)
	    warning (0,
		     "structure size boundary can only be set to 8, 32 or 64");
	  else
	    warning (0, "structure size boundary can only be set to 8 or 32");
	  arm_structure_size_boundary
	    = (TARGET_AAPCS_BASED ? 8 : DEFAULT_STRUCTURE_SIZE_BOUNDARY);
	}
    }

  if (TARGET_VXWORKS_RTP)
    {
      if (!global_options_set.x_arm_pic_data_is_text_relative)
	arm_pic_data_is_text_relative = 0;
    }
  else if (flag_pic
	   && !arm_pic_data_is_text_relative
	   && !(global_options_set.x_target_flags & MASK_SINGLE_PIC_BASE))
    /* When text & data segments don't have a fixed displacement, the
       intended use is with a single, read only, pic base register.
       Unless the user explicitly requested not to do that, set
       it.  */
    target_flags |= MASK_SINGLE_PIC_BASE;

  /* If stack checking is disabled, we can use r10 as the PIC register,
     which keeps r9 available.  The EABI specifies r9 as the PIC register.  */
  if (flag_pic && TARGET_SINGLE_PIC_BASE)
    {
      if (TARGET_VXWORKS_RTP)
	warning (0, "RTP PIC is incompatible with -msingle-pic-base");
      arm_pic_register = (TARGET_APCS_STACK || TARGET_AAPCS_BASED) ? 9 : 10;
    }

  if (flag_pic && TARGET_VXWORKS_RTP)
    arm_pic_register = 9;

  if (arm_pic_register_string != NULL)
    {
      int pic_register = decode_reg_name (arm_pic_register_string);

      if (!flag_pic)
	warning (0, "-mpic-register= is useless without -fpic");

      /* Prevent the user from choosing an obviously stupid PIC register.  */
      else if (pic_register < 0 || call_used_regs[pic_register]
	       || pic_register == HARD_FRAME_POINTER_REGNUM
	       || pic_register == STACK_POINTER_REGNUM
	       || pic_register >= PC_REGNUM
	       || (TARGET_VXWORKS_RTP
		   && (unsigned int) pic_register != arm_pic_register))
	error ("unable to use '%s' for PIC register", arm_pic_register_string);
      else
	arm_pic_register = pic_register;
    }

  /* Enable -mfix-cortex-m3-ldrd by default for Cortex-M3 cores.  */
  if (fix_cm3_ldrd == 2)
    {
      if (bitmap_bit_p (arm_active_target.isa, isa_bit_quirk_cm3_ldrd))
	fix_cm3_ldrd = 1;
      else
	fix_cm3_ldrd = 0;
    }

  /* Hot/Cold partitioning is not currently supported, since we can't
     handle literal pool placement in that case.  */
  if (flag_reorder_blocks_and_partition)
    {
      inform (input_location,
	      "-freorder-blocks-and-partition not supported on this architecture");
      flag_reorder_blocks_and_partition = 0;
      flag_reorder_blocks = 1;
    }

  if (flag_pic)
    /* Hoisting PIC address calculations more aggressively provides a small,
       but measurable, size reduction for PIC code.  Therefore, we decrease
       the bar for unrestricted expression hoisting to the cost of PIC address
       calculation, which is 2 instructions.  */
    maybe_set_param_value (PARAM_GCSE_UNRESTRICTED_COST, 2,
			   global_options.x_param_values,
			   global_options_set.x_param_values);

  /* ARM EABI defaults to strict volatile bitfields.  */
  if (TARGET_AAPCS_BASED && flag_strict_volatile_bitfields < 0
      && abi_version_at_least(2))
    flag_strict_volatile_bitfields = 1;

  /* Enable sw prefetching at -O3 for CPUS that have prefetch, and we
     have deemed it beneficial (signified by setting
     prefetch.num_slots to 1 or more).  */
  if (flag_prefetch_loop_arrays < 0
      && HAVE_prefetch
      && optimize >= 3
      && current_tune->prefetch.num_slots > 0)
    flag_prefetch_loop_arrays = 1;

  /* Set up parameters to be used in prefetching algorithm.  Do not
     override the defaults unless we are tuning for a core we have
     researched values for.  */
  if (current_tune->prefetch.num_slots > 0)
    maybe_set_param_value (PARAM_SIMULTANEOUS_PREFETCHES,
			   current_tune->prefetch.num_slots,
			   global_options.x_param_values,
			   global_options_set.x_param_values);
  if (current_tune->prefetch.l1_cache_line_size >= 0)
    maybe_set_param_value (PARAM_L1_CACHE_LINE_SIZE,
			   current_tune->prefetch.l1_cache_line_size,
			   global_options.x_param_values,
			   global_options_set.x_param_values);
  if (current_tune->prefetch.l1_cache_size >= 0)
    maybe_set_param_value (PARAM_L1_CACHE_SIZE,
			   current_tune->prefetch.l1_cache_size,
			   global_options.x_param_values,
			   global_options_set.x_param_values);

  /* Use Neon to perform 64-bits operations rather than core
     registers.  */
  prefer_neon_for_64bits = current_tune->prefer_neon_for_64bits;
  if (use_neon_for_64bits == 1)
     prefer_neon_for_64bits = true;

  /* Use the alternative scheduling-pressure algorithm by default.  */
  maybe_set_param_value (PARAM_SCHED_PRESSURE_ALGORITHM, SCHED_PRESSURE_MODEL,
			 global_options.x_param_values,
			 global_options_set.x_param_values);

  /* Look through ready list and all of queue for instructions
     relevant for L2 auto-prefetcher.  */
  int param_sched_autopref_queue_depth;

  switch (current_tune->sched_autopref)
    {
    case tune_params::SCHED_AUTOPREF_OFF:
      param_sched_autopref_queue_depth = -1;
      break;

    case tune_params::SCHED_AUTOPREF_RANK:
      param_sched_autopref_queue_depth = 0;
      break;

    case tune_params::SCHED_AUTOPREF_FULL:
      param_sched_autopref_queue_depth = max_insn_queue_index + 1;
      break;

    default:
      gcc_unreachable ();
    }

  maybe_set_param_value (PARAM_SCHED_AUTOPREF_QUEUE_DEPTH,
			 param_sched_autopref_queue_depth,
			 global_options.x_param_values,
			 global_options_set.x_param_values);

  /* Currently, for slow flash data, we just disable literal pools.  We also
     disable it for pure-code.  */
  if (target_slow_flash_data || target_pure_code)
    arm_disable_literal_pool = true;

  /* Disable scheduling fusion by default if it's not armv7 processor
     or doesn't prefer ldrd/strd.  */
  if (flag_schedule_fusion == 2
      && (!arm_arch7 || !current_tune->prefer_ldrd_strd))
    flag_schedule_fusion = 0;

  /* Need to remember initial options before they are overriden.  */
  init_optimize = build_optimization_node (&global_options);

  arm_options_perform_arch_sanity_checks ();
  arm_option_override_internal (&global_options, &global_options_set);
  arm_option_check_internal (&global_options);
  arm_option_params_internal ();

  /* Create the default target_options structure.  */
  target_option_default_node = target_option_current_node
    = build_target_option_node (&global_options);

  /* Register global variables with the garbage collector.  */
  arm_add_gc_roots ();

  /* Init initial mode for testing.  */
  thumb_flipper = TARGET_THUMB;
}


/* Reconfigure global status flags from the active_target.isa.  */
void
arm_option_reconfigure_globals (void)
{
  sprintf (arm_arch_name, "__ARM_ARCH_%s__", arm_active_target.arch_pp_name);
  arm_base_arch = arm_active_target.base_arch;

  /* Initialize boolean versions of the architectural flags, for use
     in the arm.md file.  */
  arm_arch3m = bitmap_bit_p (arm_active_target.isa, isa_bit_armv3m);
  arm_arch4 = bitmap_bit_p (arm_active_target.isa, isa_bit_armv4);
  arm_arch4t = arm_arch4 && bitmap_bit_p (arm_active_target.isa, isa_bit_thumb);
  arm_arch5 = bitmap_bit_p (arm_active_target.isa, isa_bit_armv5);
  arm_arch5e = bitmap_bit_p (arm_active_target.isa, isa_bit_armv5e);
  arm_arch5te = arm_arch5e
    && bitmap_bit_p (arm_active_target.isa, isa_bit_thumb);
  arm_arch6 = bitmap_bit_p (arm_active_target.isa, isa_bit_armv6);
  arm_arch6k = bitmap_bit_p (arm_active_target.isa, isa_bit_armv6k);
  arm_arch_notm = bitmap_bit_p (arm_active_target.isa, isa_bit_notm);
  arm_arch6m = arm_arch6 && !arm_arch_notm;
  arm_arch7 = bitmap_bit_p (arm_active_target.isa, isa_bit_armv7);
  arm_arch7em = bitmap_bit_p (arm_active_target.isa, isa_bit_armv7em);
  arm_arch8 = bitmap_bit_p (arm_active_target.isa, isa_bit_armv8);
  arm_arch8_1 = bitmap_bit_p (arm_active_target.isa, isa_bit_armv8_1);
  arm_arch8_2 = bitmap_bit_p (arm_active_target.isa, isa_bit_armv8_2);
  arm_arch_thumb1 = bitmap_bit_p (arm_active_target.isa, isa_bit_thumb);
  arm_arch_thumb2 = bitmap_bit_p (arm_active_target.isa, isa_bit_thumb2);
  arm_arch_xscale = bitmap_bit_p (arm_active_target.isa, isa_bit_xscale);
  arm_arch_iwmmxt = bitmap_bit_p (arm_active_target.isa, isa_bit_iwmmxt);
  arm_arch_iwmmxt2 = bitmap_bit_p (arm_active_target.isa, isa_bit_iwmmxt2);
  arm_arch_thumb_hwdiv = bitmap_bit_p (arm_active_target.isa, isa_bit_tdiv);
  arm_arch_arm_hwdiv = bitmap_bit_p (arm_active_target.isa, isa_bit_adiv);
  arm_arch_crc = bitmap_bit_p (arm_active_target.isa, isa_bit_crc32);
  arm_arch_cmse = bitmap_bit_p (arm_active_target.isa, isa_bit_cmse);
  arm_fp16_inst = bitmap_bit_p (arm_active_target.isa, isa_bit_fp16);
  arm_arch_lpae = bitmap_bit_p (arm_active_target.isa, isa_bit_lpae);
  if (arm_fp16_inst)
    {
      if (arm_fp16_format == ARM_FP16_FORMAT_ALTERNATIVE)
	error ("selected fp16 options are incompatible");
      arm_fp16_format = ARM_FP16_FORMAT_IEEE;
    }

  /* And finally, set up some quirks.  */
  arm_arch_no_volatile_ce
    = bitmap_bit_p (arm_active_target.isa, isa_bit_quirk_no_volatile_ce);
  arm_arch6kz = arm_arch6k && bitmap_bit_p (arm_active_target.isa,
					    isa_bit_quirk_armv6kz);

  /* Use the cp15 method if it is available.  */
  if (target_thread_pointer == TP_AUTO)
    {
      if (arm_arch6k && !TARGET_THUMB1)
	target_thread_pointer = TP_CP15;
      else
	target_thread_pointer = TP_SOFT;
    }
}

/* Perform some validation between the desired architecture and the rest of the
   options.  */
void
arm_options_perform_arch_sanity_checks (void)
{
  /* V5 code we generate is completely interworking capable, so we turn off
     TARGET_INTERWORK here to avoid many tests later on.  */

  /* XXX However, we must pass the right pre-processor defines to CPP
     or GLD can get confused.  This is a hack.  */
  if (TARGET_INTERWORK)
    arm_cpp_interwork = 1;

  if (arm_arch5)
    target_flags &= ~MASK_INTERWORK;

  if (TARGET_IWMMXT && !ARM_DOUBLEWORD_ALIGN)
    error ("iwmmxt requires an AAPCS compatible ABI for proper operation");

  if (TARGET_IWMMXT_ABI && !TARGET_IWMMXT)
    error ("iwmmxt abi requires an iwmmxt capable cpu");

  /* BPABI targets use linker tricks to allow interworking on cores
     without thumb support.  */
  if (TARGET_INTERWORK
      && !TARGET_BPABI
      && !bitmap_bit_p (arm_active_target.isa, isa_bit_thumb))
    {
      warning (0, "target CPU does not support interworking" );
      target_flags &= ~MASK_INTERWORK;
    }

  /* If soft-float is specified then don't use FPU.  */
  if (TARGET_SOFT_FLOAT)
    arm_fpu_attr = FPU_NONE;
  else
    arm_fpu_attr = FPU_VFP;

  if (TARGET_AAPCS_BASED)
    {
      if (TARGET_CALLER_INTERWORKING)
	error ("AAPCS does not support -mcaller-super-interworking");
      else
	if (TARGET_CALLEE_INTERWORKING)
	  error ("AAPCS does not support -mcallee-super-interworking");
    }

  /* __fp16 support currently assumes the core has ldrh.  */
  if (!arm_arch4 && arm_fp16_format != ARM_FP16_FORMAT_NONE)
    sorry ("__fp16 and no ldrh");

  if (use_cmse && !arm_arch_cmse)
    error ("target CPU does not support ARMv8-M Security Extensions");

  /* We don't clear D16-D31 VFP registers for cmse_nonsecure_call functions
     and ARMv8-M Baseline and Mainline do not allow such configuration.  */
  if (use_cmse && LAST_VFP_REGNUM > LAST_LO_VFP_REGNUM)
    error ("ARMv8-M Security Extensions incompatible with selected FPU");


  if (TARGET_AAPCS_BASED)
    {
      if (arm_abi == ARM_ABI_IWMMXT)
	arm_pcs_default = ARM_PCS_AAPCS_IWMMXT;
      else if (TARGET_HARD_FLOAT_ABI)
	{
	  arm_pcs_default = ARM_PCS_AAPCS_VFP;
	  if (!bitmap_bit_p (arm_active_target.isa, isa_bit_vfpv2))
	    error ("-mfloat-abi=hard: selected processor lacks an FPU");
	}
      else
	arm_pcs_default = ARM_PCS_AAPCS;
    }
  else
    {
      if (arm_float_abi == ARM_FLOAT_ABI_HARD)
	sorry ("-mfloat-abi=hard and VFP");

      if (arm_abi == ARM_ABI_APCS)
	arm_pcs_default = ARM_PCS_APCS;
      else
	arm_pcs_default = ARM_PCS_ATPCS;
    }
}

static void
arm_add_gc_roots (void)
{
  gcc_obstack_init(&minipool_obstack);
  minipool_startobj = (char *) obstack_alloc (&minipool_obstack, 0);
}

/* A table of known ARM exception types.
   For use with the interrupt function attribute.  */

typedef struct
{
  const char *const arg;
  const unsigned long return_value;
}
isr_attribute_arg;

static const isr_attribute_arg isr_attribute_args [] =
{
  { "IRQ",   ARM_FT_ISR },
  { "irq",   ARM_FT_ISR },
  { "FIQ",   ARM_FT_FIQ },
  { "fiq",   ARM_FT_FIQ },
  { "ABORT", ARM_FT_ISR },
  { "abort", ARM_FT_ISR },
  { "ABORT", ARM_FT_ISR },
  { "abort", ARM_FT_ISR },
  { "UNDEF", ARM_FT_EXCEPTION },
  { "undef", ARM_FT_EXCEPTION },
  { "SWI",   ARM_FT_EXCEPTION },
  { "swi",   ARM_FT_EXCEPTION },
  { NULL,    ARM_FT_NORMAL }
};

/* Returns the (interrupt) function type of the current
   function, or ARM_FT_UNKNOWN if the type cannot be determined.  */

static unsigned long
arm_isr_value (tree argument)
{
  const isr_attribute_arg * ptr;
  const char *              arg;

  if (!arm_arch_notm)
    return ARM_FT_NORMAL | ARM_FT_STACKALIGN;

  /* No argument - default to IRQ.  */
  if (argument == NULL_TREE)
    return ARM_FT_ISR;

  /* Get the value of the argument.  */
  if (TREE_VALUE (argument) == NULL_TREE
      || TREE_CODE (TREE_VALUE (argument)) != STRING_CST)
    return ARM_FT_UNKNOWN;

  arg = TREE_STRING_POINTER (TREE_VALUE (argument));

  /* Check it against the list of known arguments.  */
  for (ptr = isr_attribute_args; ptr->arg != NULL; ptr++)
    if (streq (arg, ptr->arg))
      return ptr->return_value;

  /* An unrecognized interrupt type.  */
  return ARM_FT_UNKNOWN;
}

/* Computes the type of the current function.  */

static unsigned long
arm_compute_func_type (void)
{
  unsigned long type = ARM_FT_UNKNOWN;
  tree a;
  tree attr;

  gcc_assert (TREE_CODE (current_function_decl) == FUNCTION_DECL);

  /* Decide if the current function is volatile.  Such functions
     never return, and many memory cycles can be saved by not storing
     register values that will never be needed again.  This optimization
     was added to speed up context switching in a kernel application.  */
  if (optimize > 0
      && (TREE_NOTHROW (current_function_decl)
          || !(flag_unwind_tables
               || (flag_exceptions
		   && arm_except_unwind_info (&global_options) != UI_SJLJ)))
      && TREE_THIS_VOLATILE (current_function_decl))
    type |= ARM_FT_VOLATILE;

  if (cfun->static_chain_decl != NULL)
    type |= ARM_FT_NESTED;

  attr = DECL_ATTRIBUTES (current_function_decl);

  a = lookup_attribute ("naked", attr);
  if (a != NULL_TREE)
    type |= ARM_FT_NAKED;

  a = lookup_attribute ("isr", attr);
  if (a == NULL_TREE)
    a = lookup_attribute ("interrupt", attr);

  if (a == NULL_TREE)
    type |= TARGET_INTERWORK ? ARM_FT_INTERWORKED : ARM_FT_NORMAL;
  else
    type |= arm_isr_value (TREE_VALUE (a));

  if (lookup_attribute ("cmse_nonsecure_entry", attr))
    type |= ARM_FT_CMSE_ENTRY;

  return type;
}

/* Returns the type of the current function.  */

unsigned long
arm_current_func_type (void)
{
  if (ARM_FUNC_TYPE (cfun->machine->func_type) == ARM_FT_UNKNOWN)
    cfun->machine->func_type = arm_compute_func_type ();

  return cfun->machine->func_type;
}

bool
arm_allocate_stack_slots_for_args (void)
{
  /* Naked functions should not allocate stack slots for arguments.  */
  return !IS_NAKED (arm_current_func_type ());
}

static bool
arm_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including the
     return sequence, so suppress warnings about this.  */
  return lookup_attribute ("naked", DECL_ATTRIBUTES (decl)) == NULL_TREE;
}


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   On the ARM, (if r8 is the static chain regnum, and remembering that
   referencing pc adds an offset of 8) the trampoline looks like:
	   ldr 		r8, [pc, #0]
	   ldr		pc, [pc]
	   .word	static chain value
	   .word	function's address
   XXX FIXME: When the trampoline returns, r8 will be clobbered.  */

static void
arm_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\t.syntax unified\n");

  if (TARGET_ARM)
    {
      fprintf (f, "\t.arm\n");
      asm_fprintf (f, "\tldr\t%r, [%r, #0]\n", STATIC_CHAIN_REGNUM, PC_REGNUM);
      asm_fprintf (f, "\tldr\t%r, [%r, #0]\n", PC_REGNUM, PC_REGNUM);
    }
  else if (TARGET_THUMB2)
    {
      fprintf (f, "\t.thumb\n");
      /* The Thumb-2 trampoline is similar to the arm implementation.
	 Unlike 16-bit Thumb, we enter the stub in thumb mode.  */
      asm_fprintf (f, "\tldr.w\t%r, [%r, #4]\n",
		   STATIC_CHAIN_REGNUM, PC_REGNUM);
      asm_fprintf (f, "\tldr.w\t%r, [%r, #4]\n", PC_REGNUM, PC_REGNUM);
    }
  else
    {
      ASM_OUTPUT_ALIGN (f, 2);
      fprintf (f, "\t.code\t16\n");
      fprintf (f, ".Ltrampoline_start:\n");
      asm_fprintf (f, "\tpush\t{r0, r1}\n");
      asm_fprintf (f, "\tldr\tr0, [%r, #8]\n", PC_REGNUM);
      asm_fprintf (f, "\tmov\t%r, r0\n", STATIC_CHAIN_REGNUM);
      asm_fprintf (f, "\tldr\tr0, [%r, #8]\n", PC_REGNUM);
      asm_fprintf (f, "\tstr\tr0, [%r, #4]\n", SP_REGNUM);
      asm_fprintf (f, "\tpop\t{r0, %r}\n", PC_REGNUM);
    }
  assemble_aligned_integer (UNITS_PER_WORD, const0_rtx);
  assemble_aligned_integer (UNITS_PER_WORD, const0_rtx);
}

/* Emit RTL insns to initialize the variable parts of a trampoline.  */

static void
arm_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr, mem, a_tramp;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, TARGET_32BIT ? 8 : 12);
  emit_move_insn (mem, chain_value);

  mem = adjust_address (m_tramp, SImode, TARGET_32BIT ? 12 : 16);
  fnaddr = XEXP (DECL_RTL (fndecl), 0);
  emit_move_insn (mem, fnaddr);

  a_tramp = XEXP (m_tramp, 0);
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__clear_cache"),
		     LCT_NORMAL, VOIDmode, a_tramp, Pmode,
		     plus_constant (Pmode, a_tramp, TRAMPOLINE_SIZE), Pmode);
}

/* Thumb trampolines should be entered in thumb mode, so set
   the bottom bit of the address.  */

static rtx
arm_trampoline_adjust_address (rtx addr)
{
  if (TARGET_THUMB)
    addr = expand_simple_binop (Pmode, IOR, addr, const1_rtx,
				NULL, 0, OPTAB_LIB_WIDEN);
  return addr;
}

/* Return 1 if it is possible to return using a single instruction.
   If SIBLING is non-null, this is a test for a return before a sibling
   call.  SIBLING is the call insn, so we can examine its register usage.  */

int
use_return_insn (int iscond, rtx sibling)
{
  int regno;
  unsigned int func_type;
  unsigned long saved_int_regs;
  unsigned HOST_WIDE_INT stack_adjust;
  arm_stack_offsets *offsets;

  /* Never use a return instruction before reload has run.  */
  if (!reload_completed)
    return 0;

  func_type = arm_current_func_type ();

  /* Naked, volatile and stack alignment functions need special
     consideration.  */
  if (func_type & (ARM_FT_VOLATILE | ARM_FT_NAKED | ARM_FT_STACKALIGN))
    return 0;

  /* So do interrupt functions that use the frame pointer and Thumb
     interrupt functions.  */
  if (IS_INTERRUPT (func_type) && (frame_pointer_needed || TARGET_THUMB))
    return 0;

  if (TARGET_LDRD && current_tune->prefer_ldrd_strd
      && !optimize_function_for_size_p (cfun))
    return 0;

  offsets = arm_get_frame_offsets ();
  stack_adjust = offsets->outgoing_args - offsets->saved_regs;

  /* As do variadic functions.  */
  if (crtl->args.pretend_args_size
      || cfun->machine->uses_anonymous_args
      /* Or if the function calls __builtin_eh_return () */
      || crtl->calls_eh_return
      /* Or if the function calls alloca */
      || cfun->calls_alloca
      /* Or if there is a stack adjustment.  However, if the stack pointer
	 is saved on the stack, we can use a pre-incrementing stack load.  */
      || !(stack_adjust == 0 || (TARGET_APCS_FRAME && frame_pointer_needed
				 && stack_adjust == 4))
      /* Or if the static chain register was saved above the frame, under the
	 assumption that the stack pointer isn't saved on the stack.  */
      || (!(TARGET_APCS_FRAME && frame_pointer_needed)
          && arm_compute_static_chain_stack_bytes() != 0))
    return 0;

  saved_int_regs = offsets->saved_regs_mask;

  /* Unfortunately, the insn

       ldmib sp, {..., sp, ...}

     triggers a bug on most SA-110 based devices, such that the stack
     pointer won't be correctly restored if the instruction takes a
     page fault.  We work around this problem by popping r3 along with
     the other registers, since that is never slower than executing
     another instruction.

     We test for !arm_arch5 here, because code for any architecture
     less than this could potentially be run on one of the buggy
     chips.  */
  if (stack_adjust == 4 && !arm_arch5 && TARGET_ARM)
    {
      /* Validate that r3 is a call-clobbered register (always true in
	 the default abi) ...  */
      if (!call_used_regs[3])
	return 0;

      /* ... that it isn't being used for a return value ... */
      if (arm_size_return_regs () >= (4 * UNITS_PER_WORD))
	return 0;

      /* ... or for a tail-call argument ...  */
      if (sibling)
	{
	  gcc_assert (CALL_P (sibling));

	  if (find_regno_fusage (sibling, USE, 3))
	    return 0;
	}

      /* ... and that there are no call-saved registers in r0-r2
	 (always true in the default ABI).  */
      if (saved_int_regs & 0x7)
	return 0;
    }

  /* Can't be done if interworking with Thumb, and any registers have been
     stacked.  */
  if (TARGET_INTERWORK && saved_int_regs != 0 && !IS_INTERRUPT(func_type))
    return 0;

  /* On StrongARM, conditional returns are expensive if they aren't
     taken and multiple registers have been stacked.  */
  if (iscond && arm_tune_strongarm)
    {
      /* Conditional return when just the LR is stored is a simple
	 conditional-load instruction, that's not expensive.  */
      if (saved_int_regs != 0 && saved_int_regs != (1 << LR_REGNUM))
	return 0;

      if (flag_pic
	  && arm_pic_register != INVALID_REGNUM
	  && df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM))
	return 0;
    }

  /* ARMv8-M nonsecure entry function need to use bxns to return and thus need
     several instructions if anything needs to be popped.  */
  if (saved_int_regs && IS_CMSE_ENTRY (func_type))
    return 0;

  /* If there are saved registers but the LR isn't saved, then we need
     two instructions for the return.  */
  if (saved_int_regs && !(saved_int_regs & (1 << LR_REGNUM)))
    return 0;

  /* Can't be done if any of the VFP regs are pushed,
     since this also requires an insn.  */
  if (TARGET_HARD_FLOAT)
    for (regno = FIRST_VFP_REGNUM; regno <= LAST_VFP_REGNUM; regno++)
      if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
	return 0;

  if (TARGET_REALLY_IWMMXT)
    for (regno = FIRST_IWMMXT_REGNUM; regno <= LAST_IWMMXT_REGNUM; regno++)
      if (df_regs_ever_live_p (regno) && ! call_used_regs[regno])
	return 0;

  return 1;
}

/* Return TRUE if we should try to use a simple_return insn, i.e. perform
   shrink-wrapping if possible.  This is the case if we need to emit a
   prologue, which we can test by looking at the offsets.  */
bool
use_simple_return_p (void)
{
  arm_stack_offsets *offsets;

  /* Note this function can be called before or after reload.  */
  if (!reload_completed)
    arm_compute_frame_layout ();

  offsets = arm_get_frame_offsets ();
  return offsets->outgoing_args != 0;
}

/* Return TRUE if int I is a valid immediate ARM constant.  */

int
const_ok_for_arm (HOST_WIDE_INT i)
{
  int lowbit;

  /* For machines with >32 bit HOST_WIDE_INT, the bits above bit 31 must
     be all zero, or all one.  */
  if ((i & ~(unsigned HOST_WIDE_INT) 0xffffffff) != 0
      && ((i & ~(unsigned HOST_WIDE_INT) 0xffffffff)
	  != ((~(unsigned HOST_WIDE_INT) 0)
	      & ~(unsigned HOST_WIDE_INT) 0xffffffff)))
    return FALSE;

  i &= (unsigned HOST_WIDE_INT) 0xffffffff;

  /* Fast return for 0 and small values.  We must do this for zero, since
     the code below can't handle that one case.  */
  if ((i & ~(unsigned HOST_WIDE_INT) 0xff) == 0)
    return TRUE;

  /* Get the number of trailing zeros.  */
  lowbit = ffs((int) i) - 1;

  /* Only even shifts are allowed in ARM mode so round down to the
     nearest even number.  */
  if (TARGET_ARM)
    lowbit &= ~1;

  if ((i & ~(((unsigned HOST_WIDE_INT) 0xff) << lowbit)) == 0)
    return TRUE;

  if (TARGET_ARM)
    {
      /* Allow rotated constants in ARM mode.  */
      if (lowbit <= 4
	   && ((i & ~0xc000003f) == 0
	       || (i & ~0xf000000f) == 0
	       || (i & ~0xfc000003) == 0))
	return TRUE;
    }
  else if (TARGET_THUMB2)
    {
      HOST_WIDE_INT v;

      /* Allow repeated patterns 0x00XY00XY or 0xXYXYXYXY.  */
      v = i & 0xff;
      v |= v << 16;
      if (i == v || i == (v | (v << 8)))
	return TRUE;

      /* Allow repeated pattern 0xXY00XY00.  */
      v = i & 0xff00;
      v |= v << 16;
      if (i == v)
	return TRUE;
    }
  else if (TARGET_HAVE_MOVT)
    {
      /* Thumb-1 Targets with MOVT.  */
      if (i > 0xffff)
	return FALSE;
      else
	return TRUE;
    }

  return FALSE;
}

/* Return true if I is a valid constant for the operation CODE.  */
int
const_ok_for_op (HOST_WIDE_INT i, enum rtx_code code)
{
  if (const_ok_for_arm (i))
    return 1;

  switch (code)
    {
    case SET:
      /* See if we can use movw.  */
      if (TARGET_HAVE_MOVT && (i & 0xffff0000) == 0)
	return 1;
      else
	/* Otherwise, try mvn.  */
	return const_ok_for_arm (ARM_SIGN_EXTEND (~i));

    case PLUS:
      /* See if we can use addw or subw.  */
      if (TARGET_THUMB2
	  && ((i & 0xfffff000) == 0
	      || ((-i) & 0xfffff000) == 0))
	return 1;
      /* Fall through.  */
    case COMPARE:
    case EQ:
    case NE:
    case GT:
    case LE:
    case LT:
    case GE:
    case GEU:
    case LTU:
    case GTU:
    case LEU:
    case UNORDERED:
    case ORDERED:
    case UNEQ:
    case UNGE:
    case UNLT:
    case UNGT:
    case UNLE:
      return const_ok_for_arm (ARM_SIGN_EXTEND (-i));

    case MINUS:		/* Should only occur with (MINUS I reg) => rsb */
    case XOR:
      return 0;

    case IOR:
      if (TARGET_THUMB2)
	return const_ok_for_arm (ARM_SIGN_EXTEND (~i));
      return 0;

    case AND:
      return const_ok_for_arm (ARM_SIGN_EXTEND (~i));

    default:
      gcc_unreachable ();
    }
}

/* Return true if I is a valid di mode constant for the operation CODE.  */
int
const_ok_for_dimode_op (HOST_WIDE_INT i, enum rtx_code code)
{
  HOST_WIDE_INT hi_val = (i >> 32) & 0xFFFFFFFF;
  HOST_WIDE_INT lo_val = i & 0xFFFFFFFF;
  rtx hi = GEN_INT (hi_val);
  rtx lo = GEN_INT (lo_val);

  if (TARGET_THUMB1)
    return 0;

  switch (code)
    {
    case AND:
    case IOR:
    case XOR:
      return (const_ok_for_op (hi_val, code) || hi_val == 0xFFFFFFFF)
              && (const_ok_for_op (lo_val, code) || lo_val == 0xFFFFFFFF);
    case PLUS:
      return arm_not_operand (hi, SImode) && arm_add_operand (lo, SImode);

    default:
      return 0;
    }
}

/* Emit a sequence of insns to handle a large constant.
   CODE is the code of the operation required, it can be any of SET, PLUS,
   IOR, AND, XOR, MINUS;
   MODE is the mode in which the operation is being performed;
   VAL is the integer to operate on;
   SOURCE is the other operand (a register, or a null-pointer for SET);
   SUBTARGETS means it is safe to create scratch registers if that will
   either produce a simpler sequence, or we will want to cse the values.
   Return value is the number of insns emitted.  */

/* ??? Tweak this for thumb2.  */
int
arm_split_constant (enum rtx_code code, machine_mode mode, rtx insn,
		    HOST_WIDE_INT val, rtx target, rtx source, int subtargets)
{
  rtx cond;

  if (insn && GET_CODE (PATTERN (insn)) == COND_EXEC)
    cond = COND_EXEC_TEST (PATTERN (insn));
  else
    cond = NULL_RTX;

  if (subtargets || code == SET
      || (REG_P (target) && REG_P (source)
	  && REGNO (target) != REGNO (source)))
    {
      /* After arm_reorg has been called, we can't fix up expensive
	 constants by pushing them into memory so we must synthesize
	 them in-line, regardless of the cost.  This is only likely to
	 be more costly on chips that have load delay slots and we are
	 compiling without running the scheduler (so no splitting
	 occurred before the final instruction emission).

	 Ref: gcc -O1 -mcpu=strongarm gcc.c-torture/compile/980506-2.c
      */
      if (!cfun->machine->after_arm_reorg
	  && !cond
	  && (arm_gen_constant (code, mode, NULL_RTX, val, target, source,
				1, 0)
	      > (arm_constant_limit (optimize_function_for_size_p (cfun))
		 + (code != SET))))
	{
	  if (code == SET)
	    {
	      /* Currently SET is the only monadic value for CODE, all
		 the rest are diadic.  */
	      if (TARGET_USE_MOVT)
		arm_emit_movpair (target, GEN_INT (val));
	      else
		emit_set_insn (target, GEN_INT (val));

	      return 1;
	    }
	  else
	    {
	      rtx temp = subtargets ? gen_reg_rtx (mode) : target;

	      if (TARGET_USE_MOVT)
		arm_emit_movpair (temp, GEN_INT (val));
	      else
		emit_set_insn (temp, GEN_INT (val));

	      /* For MINUS, the value is subtracted from, since we never
		 have subtraction of a constant.  */
	      if (code == MINUS)
		emit_set_insn (target, gen_rtx_MINUS (mode, temp, source));
	      else
		emit_set_insn (target,
			       gen_rtx_fmt_ee (code, mode, source, temp));
	      return 2;
	    }
	}
    }

  return arm_gen_constant (code, mode, cond, val, target, source, subtargets,
			   1);
}

/* Return a sequence of integers, in RETURN_SEQUENCE that fit into
   ARM/THUMB2 immediates, and add up to VAL.
   Thr function return value gives the number of insns required.  */
static int
optimal_immediate_sequence (enum rtx_code code, unsigned HOST_WIDE_INT val,
			    struct four_ints *return_sequence)
{
  int best_consecutive_zeros = 0;
  int i;
  int best_start = 0;
  int insns1, insns2;
  struct four_ints tmp_sequence;

  /* If we aren't targeting ARM, the best place to start is always at
     the bottom, otherwise look more closely.  */
  if (TARGET_ARM)
    {
      for (i = 0; i < 32; i += 2)
	{
	  int consecutive_zeros = 0;

	  if (!(val & (3 << i)))
	    {
	      while ((i < 32) && !(val & (3 << i)))
		{
		  consecutive_zeros += 2;
		  i += 2;
		}
	      if (consecutive_zeros > best_consecutive_zeros)
		{
		  best_consecutive_zeros = consecutive_zeros;
		  best_start = i - consecutive_zeros;
		}
	      i -= 2;
	    }
	}
    }

  /* So long as it won't require any more insns to do so, it's
     desirable to emit a small constant (in bits 0...9) in the last
     insn.  This way there is more chance that it can be combined with
     a later addressing insn to form a pre-indexed load or store
     operation.  Consider:

	   *((volatile int *)0xe0000100) = 1;
	   *((volatile int *)0xe0000110) = 2;

     We want this to wind up as:

	    mov rA, #0xe0000000
	    mov rB, #1
	    str rB, [rA, #0x100]
	    mov rB, #2
	    str rB, [rA, #0x110]

     rather than having to synthesize both large constants from scratch.

     Therefore, we calculate how many insns would be required to emit
     the constant starting from `best_start', and also starting from
     zero (i.e. with bit 31 first to be output).  If `best_start' doesn't
     yield a shorter sequence, we may as well use zero.  */
  insns1 = optimal_immediate_sequence_1 (code, val, return_sequence, best_start);
  if (best_start != 0
      && ((HOST_WIDE_INT_1U << best_start) < val))
    {
      insns2 = optimal_immediate_sequence_1 (code, val, &tmp_sequence, 0);
      if (insns2 <= insns1)
	{
	  *return_sequence = tmp_sequence;
	  insns1 = insns2;
	}
    }

  return insns1;
}

/* As for optimal_immediate_sequence, but starting at bit-position I.  */
static int
optimal_immediate_sequence_1 (enum rtx_code code, unsigned HOST_WIDE_INT val,
			     struct four_ints *return_sequence, int i)
{
  int remainder = val & 0xffffffff;
  int insns = 0;

  /* Try and find a way of doing the job in either two or three
     instructions.

     In ARM mode we can use 8-bit constants, rotated to any 2-bit aligned
     location.  We start at position I.  This may be the MSB, or
     optimial_immediate_sequence may have positioned it at the largest block
     of zeros that are aligned on a 2-bit boundary. We then fill up the temps,
     wrapping around to the top of the word when we drop off the bottom.
     In the worst case this code should produce no more than four insns.

     In Thumb2 mode, we can use 32/16-bit replicated constants, and 8-bit
     constants, shifted to any arbitrary location.  We should always start
     at the MSB.  */
  do
    {
      int end;
      unsigned int b1, b2, b3, b4;
      unsigned HOST_WIDE_INT result;
      int loc;

      gcc_assert (insns < 4);

      if (i <= 0)
	i += 32;

      /* First, find the next normal 12/8-bit shifted/rotated immediate.  */
      if (remainder & ((TARGET_ARM ? (3 << (i - 2)) : (1 << (i - 1)))))
	{
	  loc = i;
	  if (i <= 12 && TARGET_THUMB2 && code == PLUS)
	    /* We can use addw/subw for the last 12 bits.  */
	    result = remainder;
	  else
	    {
	      /* Use an 8-bit shifted/rotated immediate.  */
	      end = i - 8;
	      if (end < 0)
		end += 32;
	      result = remainder & ((0x0ff << end)
				   | ((i < end) ? (0xff >> (32 - end))
						: 0));
	      i -= 8;
	    }
	}
      else
	{
	  /* Arm allows rotates by a multiple of two. Thumb-2 allows
	     arbitrary shifts.  */
	  i -= TARGET_ARM ? 2 : 1;
	  continue;
	}

      /* Next, see if we can do a better job with a thumb2 replicated
	 constant.

         We do it this way around to catch the cases like 0x01F001E0 where
	 two 8-bit immediates would work, but a replicated constant would
	 make it worse.

         TODO: 16-bit constants that don't clear all the bits, but still win.
         TODO: Arithmetic splitting for set/add/sub, rather than bitwise.  */
      if (TARGET_THUMB2)
	{
	  b1 = (remainder & 0xff000000) >> 24;
	  b2 = (remainder & 0x00ff0000) >> 16;
	  b3 = (remainder & 0x0000ff00) >> 8;
	  b4 = remainder & 0xff;

	  if (loc > 24)
	    {
	      /* The 8-bit immediate already found clears b1 (and maybe b2),
		 but must leave b3 and b4 alone.  */

	      /* First try to find a 32-bit replicated constant that clears
		 almost everything.  We can assume that we can't do it in one,
		 or else we wouldn't be here.  */
	      unsigned int tmp = b1 & b2 & b3 & b4;
	      unsigned int tmp2 = tmp + (tmp << 8) + (tmp << 16)
				  + (tmp << 24);
	      unsigned int matching_bytes = (tmp == b1) + (tmp == b2)
					    + (tmp == b3) + (tmp == b4);
	      if (tmp
		  && (matching_bytes >= 3
		      || (matching_bytes == 2
			  && const_ok_for_op (remainder & ~tmp2, code))))
		{
		  /* At least 3 of the bytes match, and the fourth has at
		     least as many bits set, or two of the bytes match
		     and it will only require one more insn to finish.  */
		  result = tmp2;
		  i = tmp != b1 ? 32
		      : tmp != b2 ? 24
		      : tmp != b3 ? 16
		      : 8;
		}

	      /* Second, try to find a 16-bit replicated constant that can
		 leave three of the bytes clear.  If b2 or b4 is already
		 zero, then we can.  If the 8-bit from above would not
		 clear b2 anyway, then we still win.  */
	      else if (b1 == b3 && (!b2 || !b4
			       || (remainder & 0x00ff0000 & ~result)))
		{
		  result = remainder & 0xff00ff00;
		  i = 24;
		}
	    }
	  else if (loc > 16)
	    {
	      /* The 8-bit immediate already found clears b2 (and maybe b3)
		 and we don't get here unless b1 is alredy clear, but it will
		 leave b4 unchanged.  */

	      /* If we can clear b2 and b4 at once, then we win, since the
		 8-bits couldn't possibly reach that far.  */
	      if (b2 == b4)
		{
		  result = remainder & 0x00ff00ff;
		  i = 16;
		}
	    }
	}

      return_sequence->i[insns++] = result;
      remainder &= ~result;

      if (code == SET || code == MINUS)
	code = PLUS;
    }
  while (remainder);

  return insns;
}

/* Emit an instruction with the indicated PATTERN.  If COND is
   non-NULL, conditionalize the execution of the instruction on COND
   being true.  */

static void
emit_constant_insn (rtx cond, rtx pattern)
{
  if (cond)
    pattern = gen_rtx_COND_EXEC (VOIDmode, copy_rtx (cond), pattern);
  emit_insn (pattern);
}

/* As above, but extra parameter GENERATE which, if clear, suppresses
   RTL generation.  */

static int
arm_gen_constant (enum rtx_code code, machine_mode mode, rtx cond,
		  unsigned HOST_WIDE_INT val, rtx target, rtx source,
		  int subtargets, int generate)
{
  int can_invert = 0;
  int can_negate = 0;
  int final_invert = 0;
  int i;
  int set_sign_bit_copies = 0;
  int clear_sign_bit_copies = 0;
  int clear_zero_bit_copies = 0;
  int set_zero_bit_copies = 0;
  int insns = 0, neg_insns, inv_insns;
  unsigned HOST_WIDE_INT temp1, temp2;
  unsigned HOST_WIDE_INT remainder = val & 0xffffffff;
  struct four_ints *immediates;
  struct four_ints pos_immediates, neg_immediates, inv_immediates;

  /* Find out which operations are safe for a given CODE.  Also do a quick
     check for degenerate cases; these can occur when DImode operations
     are split.  */
  switch (code)
    {
    case SET:
      can_invert = 1;
      break;

    case PLUS:
      can_negate = 1;
      break;

    case IOR:
      if (remainder == 0xffffffff)
	{
	  if (generate)
	    emit_constant_insn (cond,
				gen_rtx_SET (target,
					     GEN_INT (ARM_SIGN_EXTEND (val))));
	  return 1;
	}

      if (remainder == 0)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;

	  if (generate)
	    emit_constant_insn (cond, gen_rtx_SET (target, source));
	  return 1;
	}
      break;

    case AND:
      if (remainder == 0)
	{
	  if (generate)
	    emit_constant_insn (cond, gen_rtx_SET (target, const0_rtx));
	  return 1;
	}
      if (remainder == 0xffffffff)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  if (generate)
	    emit_constant_insn (cond, gen_rtx_SET (target, source));
	  return 1;
	}
      can_invert = 1;
      break;

    case XOR:
      if (remainder == 0)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  if (generate)
	    emit_constant_insn (cond, gen_rtx_SET (target, source));
	  return 1;
	}

      if (remainder == 0xffffffff)
	{
	  if (generate)
	    emit_constant_insn (cond,
				gen_rtx_SET (target,
					     gen_rtx_NOT (mode, source)));
	  return 1;
	}
      final_invert = 1;
      break;

    case MINUS:
      /* We treat MINUS as (val - source), since (source - val) is always
	 passed as (source + (-val)).  */
      if (remainder == 0)
	{
	  if (generate)
	    emit_constant_insn (cond,
				gen_rtx_SET (target,
					     gen_rtx_NEG (mode, source)));
	  return 1;
	}
      if (const_ok_for_arm (val))
	{
	  if (generate)
	    emit_constant_insn (cond,
				gen_rtx_SET (target,
					     gen_rtx_MINUS (mode, GEN_INT (val),
							    source)));
	  return 1;
	}

      break;

    default:
      gcc_unreachable ();
    }

  /* If we can do it in one insn get out quickly.  */
  if (const_ok_for_op (val, code))
    {
      if (generate)
	emit_constant_insn (cond,
			    gen_rtx_SET (target,
					 (source
					  ? gen_rtx_fmt_ee (code, mode, source,
							    GEN_INT (val))
					  : GEN_INT (val))));
      return 1;
    }

  /* On targets with UXTH/UBFX, we can deal with AND (2^N)-1 in a single
     insn.  */
  if (code == AND && (i = exact_log2 (remainder + 1)) > 0
      && (arm_arch_thumb2 || (i == 16 && arm_arch6 && mode == SImode)))
    {
      if (generate)
	{
	  if (mode == SImode && i == 16)
	    /* Use UXTH in preference to UBFX, since on Thumb2 it's a
	       smaller insn.  */
	    emit_constant_insn (cond,
				gen_zero_extendhisi2
				(target, gen_lowpart (HImode, source)));
	  else
	    /* Extz only supports SImode, but we can coerce the operands
	       into that mode.  */
	    emit_constant_insn (cond,
				gen_extzv_t2 (gen_lowpart (SImode, target),
					      gen_lowpart (SImode, source),
					      GEN_INT (i), const0_rtx));
	}

      return 1;
    }

  /* Calculate a few attributes that may be useful for specific
     optimizations.  */
  /* Count number of leading zeros.  */
  for (i = 31; i >= 0; i--)
    {
      if ((remainder & (1 << i)) == 0)
	clear_sign_bit_copies++;
      else
	break;
    }

  /* Count number of leading 1's.  */
  for (i = 31; i >= 0; i--)
    {
      if ((remainder & (1 << i)) != 0)
	set_sign_bit_copies++;
      else
	break;
    }

  /* Count number of trailing zero's.  */
  for (i = 0; i <= 31; i++)
    {
      if ((remainder & (1 << i)) == 0)
	clear_zero_bit_copies++;
      else
	break;
    }

  /* Count number of trailing 1's.  */
  for (i = 0; i <= 31; i++)
    {
      if ((remainder & (1 << i)) != 0)
	set_zero_bit_copies++;
      else
	break;
    }

  switch (code)
    {
    case SET:
      /* See if we can do this by sign_extending a constant that is known
	 to be negative.  This is a good, way of doing it, since the shift
	 may well merge into a subsequent insn.  */
      if (set_sign_bit_copies > 1)
	{
	  if (const_ok_for_arm
	      (temp1 = ARM_SIGN_EXTEND (remainder
					<< (set_sign_bit_copies - 1))))
	    {
	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
		  emit_constant_insn (cond,
				      gen_rtx_SET (new_src, GEN_INT (temp1)));
		  emit_constant_insn (cond,
				      gen_ashrsi3 (target, new_src,
						   GEN_INT (set_sign_bit_copies - 1)));
		}
	      return 2;
	    }
	  /* For an inverted constant, we will need to set the low bits,
	     these will be shifted out of harm's way.  */
	  temp1 |= (1 << (set_sign_bit_copies - 1)) - 1;
	  if (const_ok_for_arm (~temp1))
	    {
	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
		  emit_constant_insn (cond,
				      gen_rtx_SET (new_src, GEN_INT (temp1)));
		  emit_constant_insn (cond,
				      gen_ashrsi3 (target, new_src,
						   GEN_INT (set_sign_bit_copies - 1)));
		}
	      return 2;
	    }
	}

      /* See if we can calculate the value as the difference between two
	 valid immediates.  */
      if (clear_sign_bit_copies + clear_zero_bit_copies <= 16)
	{
	  int topshift = clear_sign_bit_copies & ~1;

	  temp1 = ARM_SIGN_EXTEND ((remainder + (0x00800000 >> topshift))
				   & (0xff000000 >> topshift));

	  /* If temp1 is zero, then that means the 9 most significant
	     bits of remainder were 1 and we've caused it to overflow.
	     When topshift is 0 we don't need to do anything since we
	     can borrow from 'bit 32'.  */
	  if (temp1 == 0 && topshift != 0)
	    temp1 = 0x80000000 >> (topshift - 1);

	  temp2 = ARM_SIGN_EXTEND (temp1 - remainder);

	  if (const_ok_for_arm (temp2))
	    {
	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
		  emit_constant_insn (cond,
				      gen_rtx_SET (new_src, GEN_INT (temp1)));
		  emit_constant_insn (cond,
				      gen_addsi3 (target, new_src,
						  GEN_INT (-temp2)));
		}

	      return 2;
	    }
	}

      /* See if we can generate this by setting the bottom (or the top)
	 16 bits, and then shifting these into the other half of the
	 word.  We only look for the simplest cases, to do more would cost
	 too much.  Be careful, however, not to generate this when the
	 alternative would take fewer insns.  */
      if (val & 0xffff0000)
	{
	  temp1 = remainder & 0xffff0000;
	  temp2 = remainder & 0x0000ffff;

	  /* Overlaps outside this range are best done using other methods.  */
	  for (i = 9; i < 24; i++)
	    {
	      if ((((temp2 | (temp2 << i)) & 0xffffffff) == remainder)
		  && !const_ok_for_arm (temp2))
		{
		  rtx new_src = (subtargets
				 ? (generate ? gen_reg_rtx (mode) : NULL_RTX)
				 : target);
		  insns = arm_gen_constant (code, mode, cond, temp2, new_src,
					    source, subtargets, generate);
		  source = new_src;
		  if (generate)
		    emit_constant_insn
		      (cond,
		       gen_rtx_SET
		       (target,
			gen_rtx_IOR (mode,
				     gen_rtx_ASHIFT (mode, source,
						     GEN_INT (i)),
				     source)));
		  return insns + 1;
		}
	    }

	  /* Don't duplicate cases already considered.  */
	  for (i = 17; i < 24; i++)
	    {
	      if (((temp1 | (temp1 >> i)) == remainder)
		  && !const_ok_for_arm (temp1))
		{
		  rtx new_src = (subtargets
				 ? (generate ? gen_reg_rtx (mode) : NULL_RTX)
				 : target);
		  insns = arm_gen_constant (code, mode, cond, temp1, new_src,
					    source, subtargets, generate);
		  source = new_src;
		  if (generate)
		    emit_constant_insn
		      (cond,
		       gen_rtx_SET (target,
				    gen_rtx_IOR
				    (mode,
				     gen_rtx_LSHIFTRT (mode, source,
						       GEN_INT (i)),
				     source)));
		  return insns + 1;
		}
	    }
	}
      break;

    case IOR:
    case XOR:
      /* If we have IOR or XOR, and the constant can be loaded in a
	 single instruction, and we can find a temporary to put it in,
	 then this can be done in two instructions instead of 3-4.  */
      if (subtargets
	  /* TARGET can't be NULL if SUBTARGETS is 0 */
	  || (reload_completed && !reg_mentioned_p (target, source)))
	{
	  if (const_ok_for_arm (ARM_SIGN_EXTEND (~val)))
	    {
	      if (generate)
		{
		  rtx sub = subtargets ? gen_reg_rtx (mode) : target;

		  emit_constant_insn (cond,
				      gen_rtx_SET (sub, GEN_INT (val)));
		  emit_constant_insn (cond,
				      gen_rtx_SET (target,
						   gen_rtx_fmt_ee (code, mode,
								   source, sub)));
		}
	      return 2;
	    }
	}

      if (code == XOR)
	break;

      /*  Convert.
	  x = y | constant ( which is composed of set_sign_bit_copies of leading 1s
	                     and the remainder 0s for e.g. 0xfff00000)
	  x = ~(~(y ashift set_sign_bit_copies) lshiftrt set_sign_bit_copies)

	  This can be done in 2 instructions by using shifts with mov or mvn.
	  e.g. for
	  x = x | 0xfff00000;
	  we generate.
	  mvn	r0, r0, asl #12
	  mvn	r0, r0, lsr #12  */
      if (set_sign_bit_copies > 8
	  && (val & (HOST_WIDE_INT_M1U << (32 - set_sign_bit_copies))) == val)
	{
	  if (generate)
	    {
	      rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	      rtx shift = GEN_INT (set_sign_bit_copies);

	      emit_constant_insn
		(cond,
		 gen_rtx_SET (sub,
			      gen_rtx_NOT (mode,
					   gen_rtx_ASHIFT (mode,
							   source,
							   shift))));
	      emit_constant_insn
		(cond,
		 gen_rtx_SET (target,
			      gen_rtx_NOT (mode,
					   gen_rtx_LSHIFTRT (mode, sub,
							     shift))));
	    }
	  return 2;
	}

      /* Convert
	  x = y | constant (which has set_zero_bit_copies number of trailing ones).
	   to
	  x = ~((~y lshiftrt set_zero_bit_copies) ashift set_zero_bit_copies).

	  For eg. r0 = r0 | 0xfff
	       mvn	r0, r0, lsr #12
	       mvn	r0, r0, asl #12

      */
      if (set_zero_bit_copies > 8
	  && (remainder & ((1 << set_zero_bit_copies) - 1)) == remainder)
	{
	  if (generate)
	    {
	      rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	      rtx shift = GEN_INT (set_zero_bit_copies);

	      emit_constant_insn
		(cond,
		 gen_rtx_SET (sub,
			      gen_rtx_NOT (mode,
					   gen_rtx_LSHIFTRT (mode,
							     source,
							     shift))));
	      emit_constant_insn
		(cond,
		 gen_rtx_SET (target,
			      gen_rtx_NOT (mode,
					   gen_rtx_ASHIFT (mode, sub,
							   shift))));
	    }
	  return 2;
	}

      /* This will never be reached for Thumb2 because orn is a valid
	 instruction. This is for Thumb1 and the ARM 32 bit cases.

	 x = y | constant (such that ~constant is a valid constant)
	 Transform this to
	 x = ~(~y & ~constant).
      */
      if (const_ok_for_arm (temp1 = ARM_SIGN_EXTEND (~val)))
	{
	  if (generate)
	    {
	      rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	      emit_constant_insn (cond,
				  gen_rtx_SET (sub,
					       gen_rtx_NOT (mode, source)));
	      source = sub;
	      if (subtargets)
		sub = gen_reg_rtx (mode);
	      emit_constant_insn (cond,
				  gen_rtx_SET (sub,
					       gen_rtx_AND (mode, source,
							    GEN_INT (temp1))));
	      emit_constant_insn (cond,
				  gen_rtx_SET (target,
					       gen_rtx_NOT (mode, sub)));
	    }
	  return 3;
	}
      break;

    case AND:
      /* See if two shifts will do 2 or more insn's worth of work.  */
      if (clear_sign_bit_copies >= 16 && clear_sign_bit_copies < 24)
	{
	  HOST_WIDE_INT shift_mask = ((0xffffffff
				       << (32 - clear_sign_bit_copies))
				      & 0xffffffff);

	  if ((remainder | shift_mask) != 0xffffffff)
	    {
	      HOST_WIDE_INT new_val
	        = ARM_SIGN_EXTEND (remainder | shift_mask);

	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
		  insns = arm_gen_constant (AND, SImode, cond, new_val,
					    new_src, source, subtargets, 1);
		  source = new_src;
		}
	      else
		{
		  rtx targ = subtargets ? NULL_RTX : target;
		  insns = arm_gen_constant (AND, mode, cond, new_val,
					    targ, source, subtargets, 0);
		}
	    }

	  if (generate)
	    {
	      rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
	      rtx shift = GEN_INT (clear_sign_bit_copies);

	      emit_insn (gen_ashlsi3 (new_src, source, shift));
	      emit_insn (gen_lshrsi3 (target, new_src, shift));
	    }

	  return insns + 2;
	}

      if (clear_zero_bit_copies >= 16 && clear_zero_bit_copies < 24)
	{
	  HOST_WIDE_INT shift_mask = (1 << clear_zero_bit_copies) - 1;

	  if ((remainder | shift_mask) != 0xffffffff)
	    {
	      HOST_WIDE_INT new_val
	        = ARM_SIGN_EXTEND (remainder | shift_mask);
	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;

		  insns = arm_gen_constant (AND, mode, cond, new_val,
					    new_src, source, subtargets, 1);
		  source = new_src;
		}
	      else
		{
		  rtx targ = subtargets ? NULL_RTX : target;

		  insns = arm_gen_constant (AND, mode, cond, new_val,
					    targ, source, subtargets, 0);
		}
	    }

	  if (generate)
	    {
	      rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
	      rtx shift = GEN_INT (clear_zero_bit_copies);

	      emit_insn (gen_lshrsi3 (new_src, source, shift));
	      emit_insn (gen_ashlsi3 (target, new_src, shift));
	    }

	  return insns + 2;
	}

      break;

    default:
      break;
    }

  /* Calculate what the instruction sequences would be if we generated it
     normally, negated, or inverted.  */
  if (code == AND)
    /* AND cannot be split into multiple insns, so invert and use BIC.  */
    insns = 99;
  else
    insns = optimal_immediate_sequence (code, remainder, &pos_immediates);

  if (can_negate)
    neg_insns = optimal_immediate_sequence (code, (-remainder) & 0xffffffff,
					    &neg_immediates);
  else
    neg_insns = 99;

  if (can_invert || final_invert)
    inv_insns = optimal_immediate_sequence (code, remainder ^ 0xffffffff,
					    &inv_immediates);
  else
    inv_insns = 99;

  immediates = &pos_immediates;

  /* Is the negated immediate sequence more efficient?  */
  if (neg_insns < insns && neg_insns <= inv_insns)
    {
      insns = neg_insns;
      immediates = &neg_immediates;
    }
  else
    can_negate = 0;

  /* Is the inverted immediate sequence more efficient?
     We must allow for an extra NOT instruction for XOR operations, although
     there is some chance that the final 'mvn' will get optimized later.  */
  if ((inv_insns + 1) < insns || (!final_invert && inv_insns < insns))
    {
      insns = inv_insns;
      immediates = &inv_immediates;
    }
  else
    {
      can_invert = 0;
      final_invert = 0;
    }

  /* Now output the chosen sequence as instructions.  */
  if (generate)
    {
      for (i = 0; i < insns; i++)
	{
	  rtx new_src, temp1_rtx;

	  temp1 = immediates->i[i];

	  if (code == SET || code == MINUS)
	    new_src = (subtargets ? gen_reg_rtx (mode) : target);
	  else if ((final_invert || i < (insns - 1)) && subtargets)
	    new_src = gen_reg_rtx (mode);
	  else
	    new_src = target;

	  if (can_invert)
	    temp1 = ~temp1;
	  else if (can_negate)
	    temp1 = -temp1;

	  temp1 = trunc_int_for_mode (temp1, mode);
	  temp1_rtx = GEN_INT (temp1);

	  if (code == SET)
	    ;
	  else if (code == MINUS)
	    temp1_rtx = gen_rtx_MINUS (mode, temp1_rtx, source);
	  else
	    temp1_rtx = gen_rtx_fmt_ee (code, mode, source, temp1_rtx);

	  emit_constant_insn (cond, gen_rtx_SET (new_src, temp1_rtx));
	  source = new_src;

	  if (code == SET)
	    {
	      can_negate = can_invert;
	      can_invert = 0;
	      code = PLUS;
	    }
	  else if (code == MINUS)
	    code = PLUS;
	}
    }

  if (final_invert)
    {
      if (generate)
	emit_constant_insn (cond, gen_rtx_SET (target,
					       gen_rtx_NOT (mode, source)));
      insns++;
    }

  return insns;
}

/* Canonicalize a comparison so that we are more likely to recognize it.
   This can be done for a few constant compares, where we can make the
   immediate value easier to load.  */

static void
arm_canonicalize_comparison (int *code, rtx *op0, rtx *op1,
			     bool op0_preserve_value)
{
  machine_mode mode;
  unsigned HOST_WIDE_INT i, maxval;

  mode = GET_MODE (*op0);
  if (mode == VOIDmode)
    mode = GET_MODE (*op1);

  maxval = (HOST_WIDE_INT_1U << (GET_MODE_BITSIZE (mode) - 1)) - 1;

  /* For DImode, we have GE/LT/GEU/LTU comparisons.  In ARM mode
     we can also use cmp/cmpeq for GTU/LEU.  GT/LE must be either
     reversed or (for constant OP1) adjusted to GE/LT.  Similarly
     for GTU/LEU in Thumb mode.  */
  if (mode == DImode)
    {

      if (*code == GT || *code == LE
	  || (!TARGET_ARM && (*code == GTU || *code == LEU)))
	{
	  /* Missing comparison.  First try to use an available
	     comparison.  */
	  if (CONST_INT_P (*op1))
	    {
	      i = INTVAL (*op1);
	      switch (*code)
		{
		case GT:
		case LE:
		  if (i != maxval
		      && arm_const_double_by_immediates (GEN_INT (i + 1)))
		    {
		      *op1 = GEN_INT (i + 1);
		      *code = *code == GT ? GE : LT;
		      return;
		    }
		  break;
		case GTU:
		case LEU:
		  if (i != ~((unsigned HOST_WIDE_INT) 0)
		      && arm_const_double_by_immediates (GEN_INT (i + 1)))
		    {
		      *op1 = GEN_INT (i + 1);
		      *code = *code == GTU ? GEU : LTU;
		      return;
		    }
		  break;
		default:
		  gcc_unreachable ();
		}
	    }

	  /* If that did not work, reverse the condition.  */
	  if (!op0_preserve_value)
	    {
	      std::swap (*op0, *op1);
	      *code = (int)swap_condition ((enum rtx_code)*code);
	    }
	}
      return;
    }

  /* If *op0 is (zero_extend:SI (subreg:QI (reg:SI) 0)) and comparing
     with const0_rtx, change it to (and:SI (reg:SI) (const_int 255)),
     to facilitate possible combining with a cmp into 'ands'.  */
  if (mode == SImode
      && GET_CODE (*op0) == ZERO_EXTEND
      && GET_CODE (XEXP (*op0, 0)) == SUBREG
      && GET_MODE (XEXP (*op0, 0)) == QImode
      && GET_MODE (SUBREG_REG (XEXP (*op0, 0))) == SImode
      && subreg_lowpart_p (XEXP (*op0, 0))
      && *op1 == const0_rtx)
    *op0 = gen_rtx_AND (SImode, SUBREG_REG (XEXP (*op0, 0)),
			GEN_INT (255));

  /* Comparisons smaller than DImode.  Only adjust comparisons against
     an out-of-range constant.  */
  if (!CONST_INT_P (*op1)
      || const_ok_for_arm (INTVAL (*op1))
      || const_ok_for_arm (- INTVAL (*op1)))
    return;

  i = INTVAL (*op1);

  switch (*code)
    {
    case EQ:
    case NE:
      return;

    case GT:
    case LE:
      if (i != maxval
	  && (const_ok_for_arm (i + 1) || const_ok_for_arm (-(i + 1))))
	{
	  *op1 = GEN_INT (ARM_SIGN_EXTEND (i + 1));
	  *code = *code == GT ? GE : LT;
	  return;
	}
      break;

    case GE:
    case LT:
      if (i != ~maxval
	  && (const_ok_for_arm (i - 1) || const_ok_for_arm (-(i - 1))))
	{
	  *op1 = GEN_INT (i - 1);
	  *code = *code == GE ? GT : LE;
	  return;
	}
      break;

    case GTU:
    case LEU:
      if (i != ~((unsigned HOST_WIDE_INT) 0)
	  && (const_ok_for_arm (i + 1) || const_ok_for_arm (-(i + 1))))
	{
	  *op1 = GEN_INT (ARM_SIGN_EXTEND (i + 1));
	  *code = *code == GTU ? GEU : LTU;
	  return;
	}
      break;

    case GEU:
    case LTU:
      if (i != 0
	  && (const_ok_for_arm (i - 1) || const_ok_for_arm (-(i - 1))))
	{
	  *op1 = GEN_INT (i - 1);
	  *code = *code == GEU ? GTU : LEU;
	  return;
	}
      break;

    default:
      gcc_unreachable ();
    }
}


/* Define how to find the value returned by a function.  */

static rtx
arm_function_value(const_tree type, const_tree func,
		   bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  int unsignedp ATTRIBUTE_UNUSED;
  rtx r ATTRIBUTE_UNUSED;

  mode = TYPE_MODE (type);

  if (TARGET_AAPCS_BASED)
    return aapcs_allocate_return_reg (mode, type, func);

  /* Promote integer types.  */
  if (INTEGRAL_TYPE_P (type))
    mode = arm_promote_function_mode (type, mode, &unsignedp, func, 1);

  /* Promotes small structs returned in a register to full-word size
     for big-endian AAPCS.  */
  if (arm_return_in_msb (type))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      if (size % UNITS_PER_WORD != 0)
	{
	  size += UNITS_PER_WORD - size % UNITS_PER_WORD;
	  mode = int_mode_for_size (size * BITS_PER_UNIT, 0).require ();
	}
    }

  return arm_libcall_value_1 (mode);
}

/* libcall hashtable helpers.  */

struct libcall_hasher : nofree_ptr_hash <const rtx_def>
{
  static inline hashval_t hash (const rtx_def *);
  static inline bool equal (const rtx_def *, const rtx_def *);
  static inline void remove (rtx_def *);
};

inline bool
libcall_hasher::equal (const rtx_def *p1, const rtx_def *p2)
{
  return rtx_equal_p (p1, p2);
}

inline hashval_t
libcall_hasher::hash (const rtx_def *p1)
{
  return hash_rtx (p1, VOIDmode, NULL, NULL, FALSE);
}

typedef hash_table<libcall_hasher> libcall_table_type;

static void
add_libcall (libcall_table_type *htab, rtx libcall)
{
  *htab->find_slot (libcall, INSERT) = libcall;
}

static bool
arm_libcall_uses_aapcs_base (const_rtx libcall)
{
  static bool init_done = false;
  static libcall_table_type *libcall_htab = NULL;

  if (!init_done)
    {
      init_done = true;

      libcall_htab = new libcall_table_type (31);
      add_libcall (libcall_htab,
		   convert_optab_libfunc (sfloat_optab, SFmode, SImode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (sfloat_optab, DFmode, SImode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (sfloat_optab, SFmode, DImode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (sfloat_optab, DFmode, DImode));

      add_libcall (libcall_htab,
		   convert_optab_libfunc (ufloat_optab, SFmode, SImode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (ufloat_optab, DFmode, SImode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (ufloat_optab, SFmode, DImode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (ufloat_optab, DFmode, DImode));

      add_libcall (libcall_htab,
		   convert_optab_libfunc (sext_optab, SFmode, HFmode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (trunc_optab, HFmode, SFmode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (sfix_optab, SImode, DFmode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (ufix_optab, SImode, DFmode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (sfix_optab, DImode, DFmode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (ufix_optab, DImode, DFmode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (sfix_optab, DImode, SFmode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (ufix_optab, DImode, SFmode));

      /* Values from double-precision helper functions are returned in core
	 registers if the selected core only supports single-precision
	 arithmetic, even if we are using the hard-float ABI.  The same is
	 true for single-precision helpers, but we will never be using the
	 hard-float ABI on a CPU which doesn't support single-precision
	 operations in hardware.  */
      add_libcall (libcall_htab, optab_libfunc (add_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (sdiv_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (smul_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (neg_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (sub_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (eq_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (lt_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (le_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (ge_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (gt_optab, DFmode));
      add_libcall (libcall_htab, optab_libfunc (unord_optab, DFmode));
      add_libcall (libcall_htab, convert_optab_libfunc (sext_optab, DFmode,
							SFmode));
      add_libcall (libcall_htab, convert_optab_libfunc (trunc_optab, SFmode,
							DFmode));
      add_libcall (libcall_htab,
		   convert_optab_libfunc (trunc_optab, HFmode, DFmode));
    }

  return libcall && libcall_htab->find (libcall) != NULL;
}

static rtx
arm_libcall_value_1 (machine_mode mode)
{
  if (TARGET_AAPCS_BASED)
    return aapcs_libcall_value (mode);
  else if (TARGET_IWMMXT_ABI
	   && arm_vector_mode_supported_p (mode))
    return gen_rtx_REG (mode, FIRST_IWMMXT_REGNUM);
  else
    return gen_rtx_REG (mode, ARG_REGISTER (1));
}

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

static rtx
arm_libcall_value (machine_mode mode, const_rtx libcall)
{
  if (TARGET_AAPCS_BASED && arm_pcs_default != ARM_PCS_AAPCS
      && GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      /* The following libcalls return their result in integer registers,
	 even though they return a floating point value.  */
      if (arm_libcall_uses_aapcs_base (libcall))
	return gen_rtx_REG (mode, ARG_REGISTER(1));

    }

  return arm_libcall_value_1 (mode);
}

/* Implement TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool
arm_function_value_regno_p (const unsigned int regno)
{
  if (regno == ARG_REGISTER (1)
      || (TARGET_32BIT
	  && TARGET_AAPCS_BASED
	  && TARGET_HARD_FLOAT
	  && regno == FIRST_VFP_REGNUM)
      || (TARGET_IWMMXT_ABI
	  && regno == FIRST_IWMMXT_REGNUM))
    return true;

  return false;
}

/* Determine the amount of memory needed to store the possible return
   registers of an untyped call.  */
int
arm_apply_result_size (void)
{
  int size = 16;

  if (TARGET_32BIT)
    {
      if (TARGET_HARD_FLOAT_ABI)
	size += 32;
      if (TARGET_IWMMXT_ABI)
	size += 8;
    }

  return size;
}

/* Decide whether TYPE should be returned in memory (true)
   or in a register (false).  FNTYPE is the type of the function making
   the call.  */
static bool
arm_return_in_memory (const_tree type, const_tree fntype)
{
  HOST_WIDE_INT size;

  size = int_size_in_bytes (type);  /* Negative if not fixed size.  */

  if (TARGET_AAPCS_BASED)
    {
      /* Simple, non-aggregate types (ie not including vectors and
	 complex) are always returned in a register (or registers).
	 We don't care about which register here, so we can short-cut
	 some of the detail.  */
      if (!AGGREGATE_TYPE_P (type)
	  && TREE_CODE (type) != VECTOR_TYPE
	  && TREE_CODE (type) != COMPLEX_TYPE)
	return false;

      /* Any return value that is no larger than one word can be
	 returned in r0.  */
      if (((unsigned HOST_WIDE_INT) size) <= UNITS_PER_WORD)
	return false;

      /* Check any available co-processors to see if they accept the
	 type as a register candidate (VFP, for example, can return
	 some aggregates in consecutive registers).  These aren't
	 available if the call is variadic.  */
      if (aapcs_select_return_coproc (type, fntype) >= 0)
	return false;

      /* Vector values should be returned using ARM registers, not
	 memory (unless they're over 16 bytes, which will break since
	 we only have four call-clobbered registers to play with).  */
      if (TREE_CODE (type) == VECTOR_TYPE)
	return (size < 0 || size > (4 * UNITS_PER_WORD));

      /* The rest go in memory.  */
      return true;
    }

  if (TREE_CODE (type) == VECTOR_TYPE)
    return (size < 0 || size > (4 * UNITS_PER_WORD));

  if (!AGGREGATE_TYPE_P (type) &&
      (TREE_CODE (type) != VECTOR_TYPE))
    /* All simple types are returned in registers.  */
    return false;

  if (arm_abi != ARM_ABI_APCS)
    {
      /* ATPCS and later return aggregate types in memory only if they are
	 larger than a word (or are variable size).  */
      return (size < 0 || size > UNITS_PER_WORD);
    }

  /* For the arm-wince targets we choose to be compatible with Microsoft's
     ARM and Thumb compilers, which always return aggregates in memory.  */
#ifndef ARM_WINCE
  /* All structures/unions bigger than one word are returned in memory.
     Also catch the case where int_size_in_bytes returns -1.  In this case
     the aggregate is either huge or of variable size, and in either case
     we will want to return it via memory and not in a register.  */
  if (size < 0 || size > UNITS_PER_WORD)
    return true;

  if (TREE_CODE (type) == RECORD_TYPE)
    {
      tree field;

      /* For a struct the APCS says that we only return in a register
	 if the type is 'integer like' and every addressable element
	 has an offset of zero.  For practical purposes this means
	 that the structure can have at most one non bit-field element
	 and that this element must be the first one in the structure.  */

      /* Find the first field, ignoring non FIELD_DECL things which will
	 have been created by C++.  */
      for (field = TYPE_FIELDS (type);
	   field && TREE_CODE (field) != FIELD_DECL;
	   field = DECL_CHAIN (field))
	continue;

      if (field == NULL)
	return false; /* An empty structure.  Allowed by an extension to ANSI C.  */

      /* Check that the first field is valid for returning in a register.  */

      /* ... Floats are not allowed */
      if (FLOAT_TYPE_P (TREE_TYPE (field)))
	return true;

      /* ... Aggregates that are not themselves valid for returning in
	 a register are not allowed.  */
      if (arm_return_in_memory (TREE_TYPE (field), NULL_TREE))
	return true;

      /* Now check the remaining fields, if any.  Only bitfields are allowed,
	 since they are not addressable.  */
      for (field = DECL_CHAIN (field);
	   field;
	   field = DECL_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (!DECL_BIT_FIELD_TYPE (field))
	    return true;
	}

      return false;
    }

  if (TREE_CODE (type) == UNION_TYPE)
    {
      tree field;

      /* Unions can be returned in registers if every element is
	 integral, or can be returned in an integer register.  */
      for (field = TYPE_FIELDS (type);
	   field;
	   field = DECL_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (FLOAT_TYPE_P (TREE_TYPE (field)))
	    return true;

	  if (arm_return_in_memory (TREE_TYPE (field), NULL_TREE))
	    return true;
	}

      return false;
    }
#endif /* not ARM_WINCE */

  /* Return all other types in memory.  */
  return true;
}

const struct pcs_attribute_arg
{
  const char *arg;
  enum arm_pcs value;
} pcs_attribute_args[] =
  {
    {"aapcs", ARM_PCS_AAPCS},
    {"aapcs-vfp", ARM_PCS_AAPCS_VFP},
#if 0
    /* We could recognize these, but changes would be needed elsewhere
     * to implement them.  */
    {"aapcs-iwmmxt", ARM_PCS_AAPCS_IWMMXT},
    {"atpcs", ARM_PCS_ATPCS},
    {"apcs", ARM_PCS_APCS},
#endif
    {NULL, ARM_PCS_UNKNOWN}
  };

static enum arm_pcs
arm_pcs_from_attribute (tree attr)
{
  const struct pcs_attribute_arg *ptr;
  const char *arg;

  /* Get the value of the argument.  */
  if (TREE_VALUE (attr) == NULL_TREE
      || TREE_CODE (TREE_VALUE (attr)) != STRING_CST)
    return ARM_PCS_UNKNOWN;

  arg = TREE_STRING_POINTER (TREE_VALUE (attr));

  /* Check it against the list of known arguments.  */
  for (ptr = pcs_attribute_args; ptr->arg != NULL; ptr++)
    if (streq (arg, ptr->arg))
      return ptr->value;

  /* An unrecognized interrupt type.  */
  return ARM_PCS_UNKNOWN;
}

/* Get the PCS variant to use for this call.  TYPE is the function's type
   specification, DECL is the specific declartion.  DECL may be null if
   the call could be indirect or if this is a library call.  */
static enum arm_pcs
arm_get_pcs_model (const_tree type, const_tree decl)
{
  bool user_convention = false;
  enum arm_pcs user_pcs = arm_pcs_default;
  tree attr;

  gcc_assert (type);

  attr = lookup_attribute ("pcs", TYPE_ATTRIBUTES (type));
  if (attr)
    {
      user_pcs = arm_pcs_from_attribute (TREE_VALUE (attr));
      user_convention = true;
    }

  if (TARGET_AAPCS_BASED)
    {
      /* Detect varargs functions.  These always use the base rules
	 (no argument is ever a candidate for a co-processor
	 register).  */
      bool base_rules = stdarg_p (type);

      if (user_convention)
	{
	  if (user_pcs > ARM_PCS_AAPCS_LOCAL)
	    sorry ("non-AAPCS derived PCS variant");
	  else if (base_rules && user_pcs != ARM_PCS_AAPCS)
	    error ("variadic functions must use the base AAPCS variant");
	}

      if (base_rules)
	return ARM_PCS_AAPCS;
      else if (user_convention)
	return user_pcs;
      else if (decl && flag_unit_at_a_time)
	{
	  /* Local functions never leak outside this compilation unit,
	     so we are free to use whatever conventions are
	     appropriate.  */
	  /* FIXME: remove CONST_CAST_TREE when cgraph is constified.  */
	  cgraph_local_info *i = cgraph_node::local_info (CONST_CAST_TREE(decl));
	  if (i && i->local)
	    return ARM_PCS_AAPCS_LOCAL;
	}
    }
  else if (user_convention && user_pcs != arm_pcs_default)
    sorry ("PCS variant");

  /* For everything else we use the target's default.  */
  return arm_pcs_default;
}


static void
aapcs_vfp_cum_init (CUMULATIVE_ARGS *pcum  ATTRIBUTE_UNUSED,
		    const_tree fntype ATTRIBUTE_UNUSED,
		    rtx libcall ATTRIBUTE_UNUSED,
		    const_tree fndecl ATTRIBUTE_UNUSED)
{
  /* Record the unallocated VFP registers.  */
  pcum->aapcs_vfp_regs_free = (1 << NUM_VFP_ARG_REGS) - 1;
  pcum->aapcs_vfp_reg_alloc = 0;
}

/* Walk down the type tree of TYPE counting consecutive base elements.
   If *MODEP is VOIDmode, then set it to the first valid floating point
   type.  If a non-floating point type is found, or if a floating point
   type that doesn't match a non-VOIDmode *MODEP is found, then return -1,
   otherwise return the count in the sub-tree.  */
static int
aapcs_vfp_sub_candidate (const_tree type, machine_mode *modep)
{
  machine_mode mode;
  HOST_WIDE_INT size;

  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
      mode = TYPE_MODE (type);
      if (mode != DFmode && mode != SFmode && mode != HFmode)
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 1;

      break;

    case COMPLEX_TYPE:
      mode = TYPE_MODE (TREE_TYPE (type));
      if (mode != DFmode && mode != SFmode)
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 2;

      break;

    case VECTOR_TYPE:
      /* Use V2SImode and V4SImode as representatives of all 64-bit
	 and 128-bit vector types, whether or not those modes are
	 supported with the present options.  */
      size = int_size_in_bytes (type);
      switch (size)
	{
	case 8:
	  mode = V2SImode;
	  break;
	case 16:
	  mode = V4SImode;
	  break;
	default:
	  return -1;
	}

      if (*modep == VOIDmode)
	*modep = mode;

      /* Vector modes are considered to be opaque: two vectors are
	 equivalent for the purposes of being homogeneous aggregates
	 if they are the same size.  */
      if (*modep == mode)
	return 1;

      break;

    case ARRAY_TYPE:
      {
	int count;
	tree index = TYPE_DOMAIN (type);

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	count = aapcs_vfp_sub_candidate (TREE_TYPE (type), modep);
	if (count == -1
	    || !index
	    || !TYPE_MAX_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MAX_VALUE (index))
	    || !TYPE_MIN_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MIN_VALUE (index))
	    || count < 0)
	  return -1;

	count *= (1 + tree_to_uhwi (TYPE_MAX_VALUE (index))
		      - tree_to_uhwi (TYPE_MIN_VALUE (index)));

	/* There must be no padding.  */
	if (wi::to_wide (TYPE_SIZE (type))
	    != count * GET_MODE_BITSIZE (*modep))
	  return -1;

	return count;
      }

    case RECORD_TYPE:
      {
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = aapcs_vfp_sub_candidate (TREE_TYPE (field), modep);
	    if (sub_count < 0)
	      return -1;
	    count += sub_count;
	  }

	/* There must be no padding.  */
	if (wi::to_wide (TYPE_SIZE (type))
	    != count * GET_MODE_BITSIZE (*modep))
	  return -1;

	return count;
      }

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	/* These aren't very interesting except in a degenerate case.  */
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = aapcs_vfp_sub_candidate (TREE_TYPE (field), modep);
	    if (sub_count < 0)
	      return -1;
	    count = count > sub_count ? count : sub_count;
	  }

	/* There must be no padding.  */
	if (wi::to_wide (TYPE_SIZE (type))
	    != count * GET_MODE_BITSIZE (*modep))
	  return -1;

	return count;
      }

    default:
      break;
    }

  return -1;
}

/* Return true if PCS_VARIANT should use VFP registers.  */
static bool
use_vfp_abi (enum arm_pcs pcs_variant, bool is_double)
{
  if (pcs_variant == ARM_PCS_AAPCS_VFP)
    {
      static bool seen_thumb1_vfp = false;

      if (TARGET_THUMB1 && !seen_thumb1_vfp)
	{
	  sorry ("Thumb-1 hard-float VFP ABI");
	  /* sorry() is not immediately fatal, so only display this once.  */
	  seen_thumb1_vfp = true;
	}

      return true;
    }

  if (pcs_variant != ARM_PCS_AAPCS_LOCAL)
    return false;

  return (TARGET_32BIT && TARGET_HARD_FLOAT &&
	  (TARGET_VFP_DOUBLE || !is_double));
}

/* Return true if an argument whose type is TYPE, or mode is MODE, is
   suitable for passing or returning in VFP registers for the PCS
   variant selected.  If it is, then *BASE_MODE is updated to contain
   a machine mode describing each element of the argument's type and
   *COUNT to hold the number of such elements.  */
static bool
aapcs_vfp_is_call_or_return_candidate (enum arm_pcs pcs_variant,
				       machine_mode mode, const_tree type,
				       machine_mode *base_mode, int *count)
{
  machine_mode new_mode = VOIDmode;

  /* If we have the type information, prefer that to working things
     out from the mode.  */
  if (type)
    {
      int ag_count = aapcs_vfp_sub_candidate (type, &new_mode);

      if (ag_count > 0 && ag_count <= 4)
	*count = ag_count;
      else
	return false;
    }
  else if (GET_MODE_CLASS (mode) == MODE_FLOAT
	   || GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	   || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
    {
      *count = 1;
      new_mode = mode;
    }
  else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
    {
      *count = 2;
      new_mode = (mode == DCmode ? DFmode : SFmode);
    }
  else
    return false;


  if (!use_vfp_abi (pcs_variant, ARM_NUM_REGS (new_mode) > 1))
    return false;

  *base_mode = new_mode;
  return true;
}

static bool
aapcs_vfp_is_return_candidate (enum arm_pcs pcs_variant,
			       machine_mode mode, const_tree type)
{
  int count ATTRIBUTE_UNUSED;
  machine_mode ag_mode ATTRIBUTE_UNUSED;

  if (!use_vfp_abi (pcs_variant, false))
    return false;
  return aapcs_vfp_is_call_or_return_candidate (pcs_variant, mode, type,
						&ag_mode, &count);
}

static bool
aapcs_vfp_is_call_candidate (CUMULATIVE_ARGS *pcum, machine_mode mode,
			     const_tree type)
{
  if (!use_vfp_abi (pcum->pcs_variant, false))
    return false;

  return aapcs_vfp_is_call_or_return_candidate (pcum->pcs_variant, mode, type,
						&pcum->aapcs_vfp_rmode,
						&pcum->aapcs_vfp_rcount);
}

/* Implement the allocate field in aapcs_cp_arg_layout.  See the comment there
   for the behaviour of this function.  */

static bool
aapcs_vfp_allocate (CUMULATIVE_ARGS *pcum, machine_mode mode,
		    const_tree type  ATTRIBUTE_UNUSED)
{
  int rmode_size
    = MAX (GET_MODE_SIZE (pcum->aapcs_vfp_rmode), GET_MODE_SIZE (SFmode));
  int shift = rmode_size / GET_MODE_SIZE (SFmode);
  unsigned mask = (1 << (shift * pcum->aapcs_vfp_rcount)) - 1;
  int regno;

  for (regno = 0; regno < NUM_VFP_ARG_REGS; regno += shift)
    if (((pcum->aapcs_vfp_regs_free >> regno) & mask) == mask)
      {
	pcum->aapcs_vfp_reg_alloc = mask << regno;
	if (mode == BLKmode
	    || (mode == TImode && ! TARGET_NEON)
	    || ! arm_hard_regno_mode_ok (FIRST_VFP_REGNUM + regno, mode))
	  {
	    int i;
	    int rcount = pcum->aapcs_vfp_rcount;
	    int rshift = shift;
	    machine_mode rmode = pcum->aapcs_vfp_rmode;
	    rtx par;
	    if (!TARGET_NEON)
	      {
		/* Avoid using unsupported vector modes.  */
		if (rmode == V2SImode)
		  rmode = DImode;
		else if (rmode == V4SImode)
		  {
		    rmode = DImode;
		    rcount *= 2;
		    rshift /= 2;
		  }
	      }
	    par = gen_rtx_PARALLEL (mode, rtvec_alloc (rcount));
	    for (i = 0; i < rcount; i++)
	      {
		rtx tmp = gen_rtx_REG (rmode,
				       FIRST_VFP_REGNUM + regno + i * rshift);
		tmp = gen_rtx_EXPR_LIST
		  (VOIDmode, tmp,
		   GEN_INT (i * GET_MODE_SIZE (rmode)));
		XVECEXP (par, 0, i) = tmp;
	      }

	    pcum->aapcs_reg = par;
	  }
	else
	  pcum->aapcs_reg = gen_rtx_REG (mode, FIRST_VFP_REGNUM + regno);
	return true;
      }
  return false;
}

/* Implement the allocate_return_reg field in aapcs_cp_arg_layout.  See the
   comment there for the behaviour of this function.  */

static rtx
aapcs_vfp_allocate_return_reg (enum arm_pcs pcs_variant ATTRIBUTE_UNUSED,
			       machine_mode mode,
			       const_tree type ATTRIBUTE_UNUSED)
{
  if (!use_vfp_abi (pcs_variant, false))
    return NULL;

  if (mode == BLKmode
      || (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_SIZE (mode) >= GET_MODE_SIZE (TImode)
	  && !TARGET_NEON))
    {
      int count;
      machine_mode ag_mode;
      int i;
      rtx par;
      int shift;

      aapcs_vfp_is_call_or_return_candidate (pcs_variant, mode, type,
					     &ag_mode, &count);

      if (!TARGET_NEON)
	{
	  if (ag_mode == V2SImode)
	    ag_mode = DImode;
	  else if (ag_mode == V4SImode)
	    {
	      ag_mode = DImode;
	      count *= 2;
	    }
	}
      shift = GET_MODE_SIZE(ag_mode) / GET_MODE_SIZE(SFmode);
      par = gen_rtx_PARALLEL (mode, rtvec_alloc (count));
      for (i = 0; i < count; i++)
	{
	  rtx tmp = gen_rtx_REG (ag_mode, FIRST_VFP_REGNUM + i * shift);
	  tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp,
				   GEN_INT (i * GET_MODE_SIZE (ag_mode)));
	  XVECEXP (par, 0, i) = tmp;
	}

      return par;
    }

  return gen_rtx_REG (mode, FIRST_VFP_REGNUM);
}

static void
aapcs_vfp_advance (CUMULATIVE_ARGS *pcum  ATTRIBUTE_UNUSED,
		   machine_mode mode  ATTRIBUTE_UNUSED,
		   const_tree type  ATTRIBUTE_UNUSED)
{
  pcum->aapcs_vfp_regs_free &= ~pcum->aapcs_vfp_reg_alloc;
  pcum->aapcs_vfp_reg_alloc = 0;
  return;
}

#define AAPCS_CP(X)				\
  {						\
    aapcs_ ## X ## _cum_init,			\
    aapcs_ ## X ## _is_call_candidate,		\
    aapcs_ ## X ## _allocate,			\
    aapcs_ ## X ## _is_return_candidate,	\
    aapcs_ ## X ## _allocate_return_reg,	\
    aapcs_ ## X ## _advance			\
  }

/* Table of co-processors that can be used to pass arguments in
   registers.  Idealy no arugment should be a candidate for more than
   one co-processor table entry, but the table is processed in order
   and stops after the first match.  If that entry then fails to put
   the argument into a co-processor register, the argument will go on
   the stack.  */
static struct
{
  /* Initialize co-processor related state in CUMULATIVE_ARGS structure.  */
  void (*cum_init) (CUMULATIVE_ARGS *, const_tree, rtx, const_tree);

  /* Return true if an argument of mode MODE (or type TYPE if MODE is
     BLKmode) is a candidate for this co-processor's registers; this
     function should ignore any position-dependent state in
     CUMULATIVE_ARGS and only use call-type dependent information.  */
  bool (*is_call_candidate) (CUMULATIVE_ARGS *, machine_mode, const_tree);

  /* Return true if the argument does get a co-processor register; it
     should set aapcs_reg to an RTX of the register allocated as is
     required for a return from FUNCTION_ARG.  */
  bool (*allocate) (CUMULATIVE_ARGS *, machine_mode, const_tree);

  /* Return true if a result of mode MODE (or type TYPE if MODE is BLKmode) can
     be returned in this co-processor's registers.  */
  bool (*is_return_candidate) (enum arm_pcs, machine_mode, const_tree);

  /* Allocate and return an RTX element to hold the return type of a call.  This
     routine must not fail and will only be called if is_return_candidate
     returned true with the same parameters.  */
  rtx (*allocate_return_reg) (enum arm_pcs, machine_mode, const_tree);

  /* Finish processing this argument and prepare to start processing
     the next one.  */
  void (*advance) (CUMULATIVE_ARGS *, machine_mode, const_tree);
} aapcs_cp_arg_layout[ARM_NUM_COPROC_SLOTS] =
  {
    AAPCS_CP(vfp)
  };

#undef AAPCS_CP

static int
aapcs_select_call_coproc (CUMULATIVE_ARGS *pcum, machine_mode mode,
			  const_tree type)
{
  int i;

  for (i = 0; i < ARM_NUM_COPROC_SLOTS; i++)
    if (aapcs_cp_arg_layout[i].is_call_candidate (pcum, mode, type))
      return i;

  return -1;
}

static int
aapcs_select_return_coproc (const_tree type, const_tree fntype)
{
  /* We aren't passed a decl, so we can't check that a call is local.
     However, it isn't clear that that would be a win anyway, since it
     might limit some tail-calling opportunities.  */
  enum arm_pcs pcs_variant;

  if (fntype)
    {
      const_tree fndecl = NULL_TREE;

      if (TREE_CODE (fntype) == FUNCTION_DECL)
	{
	  fndecl = fntype;
	  fntype = TREE_TYPE (fntype);
	}

      pcs_variant = arm_get_pcs_model (fntype, fndecl);
    }
  else
    pcs_variant = arm_pcs_default;

  if (pcs_variant != ARM_PCS_AAPCS)
    {
      int i;

      for (i = 0; i < ARM_NUM_COPROC_SLOTS; i++)
	if (aapcs_cp_arg_layout[i].is_return_candidate (pcs_variant,
							TYPE_MODE (type),
							type))
	  return i;
    }
  return -1;
}

static rtx
aapcs_allocate_return_reg (machine_mode mode, const_tree type,
			   const_tree fntype)
{
  /* We aren't passed a decl, so we can't check that a call is local.
     However, it isn't clear that that would be a win anyway, since it
     might limit some tail-calling opportunities.  */
  enum arm_pcs pcs_variant;
  int unsignedp ATTRIBUTE_UNUSED;

  if (fntype)
    {
      const_tree fndecl = NULL_TREE;

      if (TREE_CODE (fntype) == FUNCTION_DECL)
	{
	  fndecl = fntype;
	  fntype = TREE_TYPE (fntype);
	}

      pcs_variant = arm_get_pcs_model (fntype, fndecl);
    }
  else
    pcs_variant = arm_pcs_default;

  /* Promote integer types.  */
  if (type && INTEGRAL_TYPE_P (type))
    mode = arm_promote_function_mode (type, mode, &unsignedp, fntype, 1);

  if (pcs_variant != ARM_PCS_AAPCS)
    {
      int i;

      for (i = 0; i < ARM_NUM_COPROC_SLOTS; i++)
	if (aapcs_cp_arg_layout[i].is_return_candidate (pcs_variant, mode,
							type))
	  return aapcs_cp_arg_layout[i].allocate_return_reg (pcs_variant,
							     mode, type);
    }

  /* Promotes small structs returned in a register to full-word size
     for big-endian AAPCS.  */
  if (type && arm_return_in_msb (type))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      if (size % UNITS_PER_WORD != 0)
	{
	  size += UNITS_PER_WORD - size % UNITS_PER_WORD;
	  mode = int_mode_for_size (size * BITS_PER_UNIT, 0).require ();
	}
    }

  return gen_rtx_REG (mode, R0_REGNUM);
}

static rtx
aapcs_libcall_value (machine_mode mode)
{
  if (BYTES_BIG_ENDIAN && ALL_FIXED_POINT_MODE_P (mode)
      && GET_MODE_SIZE (mode) <= 4)
    mode = SImode;

  return aapcs_allocate_return_reg (mode, NULL_TREE, NULL_TREE);
}

/* Lay out a function argument using the AAPCS rules.  The rule
   numbers referred to here are those in the AAPCS.  */
static void
aapcs_layout_arg (CUMULATIVE_ARGS *pcum, machine_mode mode,
		  const_tree type, bool named)
{
  int nregs, nregs2;
  int ncrn;

  /* We only need to do this once per argument.  */
  if (pcum->aapcs_arg_processed)
    return;

  pcum->aapcs_arg_processed = true;

  /* Special case: if named is false then we are handling an incoming
     anonymous argument which is on the stack.  */
  if (!named)
    return;

  /* Is this a potential co-processor register candidate?  */
  if (pcum->pcs_variant != ARM_PCS_AAPCS)
    {
      int slot = aapcs_select_call_coproc (pcum, mode, type);
      pcum->aapcs_cprc_slot = slot;

      /* We don't have to apply any of the rules from part B of the
	 preparation phase, these are handled elsewhere in the
	 compiler.  */

      if (slot >= 0)
	{
	  /* A Co-processor register candidate goes either in its own
	     class of registers or on the stack.  */
	  if (!pcum->aapcs_cprc_failed[slot])
	    {
	      /* C1.cp - Try to allocate the argument to co-processor
		 registers.  */
	      if (aapcs_cp_arg_layout[slot].allocate (pcum, mode, type))
		return;

	      /* C2.cp - Put the argument on the stack and note that we
		 can't assign any more candidates in this slot.  We also
		 need to note that we have allocated stack space, so that
		 we won't later try to split a non-cprc candidate between
		 core registers and the stack.  */
	      pcum->aapcs_cprc_failed[slot] = true;
	      pcum->can_split = false;
	    }

	  /* We didn't get a register, so this argument goes on the
	     stack.  */
	  gcc_assert (pcum->can_split == false);
	  return;
	}
    }

  /* C3 - For double-word aligned arguments, round the NCRN up to the
     next even number.  */
  ncrn = pcum->aapcs_ncrn;
  if (ncrn & 1)
    {
      int res = arm_needs_doubleword_align (mode, type);
      /* Only warn during RTL expansion of call stmts, otherwise we would
	 warn e.g. during gimplification even on functions that will be
	 always inlined, and we'd warn multiple times.  Don't warn when
	 called in expand_function_start either, as we warn instead in
	 arm_function_arg_boundary in that case.  */
      if (res < 0 && warn_psabi && currently_expanding_gimple_stmt)
	inform (input_location, "parameter passing for argument of type "
		"%qT changed in GCC 7.1", type);
      else if (res > 0)
	ncrn++;
    }

  nregs = ARM_NUM_REGS2(mode, type);

  /* Sigh, this test should really assert that nregs > 0, but a GCC
     extension allows empty structs and then gives them empty size; it
     then allows such a structure to be passed by value.  For some of
     the code below we have to pretend that such an argument has
     non-zero size so that we 'locate' it correctly either in
     registers or on the stack.  */
  gcc_assert (nregs >= 0);

  nregs2 = nregs ? nregs : 1;

  /* C4 - Argument fits entirely in core registers.  */
  if (ncrn + nregs2 <= NUM_ARG_REGS)
    {
      pcum->aapcs_reg = gen_rtx_REG (mode, ncrn);
      pcum->aapcs_next_ncrn = ncrn + nregs;
      return;
    }

  /* C5 - Some core registers left and there are no arguments already
     on the stack: split this argument between the remaining core
     registers and the stack.  */
  if (ncrn < NUM_ARG_REGS && pcum->can_split)
    {
      pcum->aapcs_reg = gen_rtx_REG (mode, ncrn);
      pcum->aapcs_next_ncrn = NUM_ARG_REGS;
      pcum->aapcs_partial = (NUM_ARG_REGS - ncrn) * UNITS_PER_WORD;
      return;
    }

  /* C6 - NCRN is set to 4.  */
  pcum->aapcs_next_ncrn = NUM_ARG_REGS;

  /* C7,C8 - arugment goes on the stack.  We have nothing to do here.  */
  return;
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is NULL.  */
void
arm_init_cumulative_args (CUMULATIVE_ARGS *pcum, tree fntype,
			  rtx libname,
			  tree fndecl ATTRIBUTE_UNUSED)
{
  /* Long call handling.  */
  if (fntype)
    pcum->pcs_variant = arm_get_pcs_model (fntype, fndecl);
  else
    pcum->pcs_variant = arm_pcs_default;

  if (pcum->pcs_variant <= ARM_PCS_AAPCS_LOCAL)
    {
      if (arm_libcall_uses_aapcs_base (libname))
	pcum->pcs_variant = ARM_PCS_AAPCS;

      pcum->aapcs_ncrn = pcum->aapcs_next_ncrn = 0;
      pcum->aapcs_reg = NULL_RTX;
      pcum->aapcs_partial = 0;
      pcum->aapcs_arg_processed = false;
      pcum->aapcs_cprc_slot = -1;
      pcum->can_split = true;

      if (pcum->pcs_variant != ARM_PCS_AAPCS)
	{
	  int i;

	  for (i = 0; i < ARM_NUM_COPROC_SLOTS; i++)
	    {
	      pcum->aapcs_cprc_failed[i] = false;
	      aapcs_cp_arg_layout[i].cum_init (pcum, fntype, libname, fndecl);
	    }
	}
      return;
    }

  /* Legacy ABIs */

  /* On the ARM, the offset starts at 0.  */
  pcum->nregs = 0;
  pcum->iwmmxt_nregs = 0;
  pcum->can_split = true;

  /* Varargs vectors are treated the same as long long.
     named_count avoids having to change the way arm handles 'named' */
  pcum->named_count = 0;
  pcum->nargs = 0;

  if (TARGET_REALLY_IWMMXT && fntype)
    {
      tree fn_arg;

      for (fn_arg = TYPE_ARG_TYPES (fntype);
	   fn_arg;
	   fn_arg = TREE_CHAIN (fn_arg))
	pcum->named_count += 1;

      if (! pcum->named_count)
	pcum->named_count = INT_MAX;
    }
}

/* Return 1 if double word alignment is required for argument passing.
   Return -1 if double word alignment used to be required for argument
   passing before PR77728 ABI fix, but is not required anymore.
   Return 0 if double word alignment is not required and wasn't requried
   before either.  */
static int
arm_needs_doubleword_align (machine_mode mode, const_tree type)
{
  if (!type)
    return GET_MODE_ALIGNMENT (mode) > PARM_BOUNDARY;

  /* Scalar and vector types: Use natural alignment, i.e. of base type.  */
  if (!AGGREGATE_TYPE_P (type))
    return TYPE_ALIGN (TYPE_MAIN_VARIANT (type)) > PARM_BOUNDARY;

  /* Array types: Use member alignment of element type.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    return TYPE_ALIGN (TREE_TYPE (type)) > PARM_BOUNDARY;

  int ret = 0;
  /* Record/aggregate types: Use greatest member alignment of any member.  */ 
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (DECL_ALIGN (field) > PARM_BOUNDARY)
      {
	if (TREE_CODE (field) == FIELD_DECL)
	  return 1;
	else
	  /* Before PR77728 fix, we were incorrectly considering also
	     other aggregate fields, like VAR_DECLs, TYPE_DECLs etc.
	     Make sure we can warn about that with -Wpsabi.  */
	  ret = -1;
      }

  return ret;
}


/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On the ARM, normally the first 16 bytes are passed in registers r0-r3; all
   other arguments are passed on the stack.  If (NAMED == 0) (which happens
   only in assign_parms, since TARGET_SETUP_INCOMING_VARARGS is
   defined), say it is passed in the stack (function_prologue will
   indeed make it pass in the stack if necessary).  */

static rtx
arm_function_arg (cumulative_args_t pcum_v, machine_mode mode,
		  const_tree type, bool named)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  int nregs;

  /* Handle the special case quickly.  Pick an arbitrary value for op2 of
     a call insn (op3 of a call_value insn).  */
  if (mode == VOIDmode)
    return const0_rtx;

  if (pcum->pcs_variant <= ARM_PCS_AAPCS_LOCAL)
    {
      aapcs_layout_arg (pcum, mode, type, named);
      return pcum->aapcs_reg;
    }

  /* Varargs vectors are treated the same as long long.
     named_count avoids having to change the way arm handles 'named' */
  if (TARGET_IWMMXT_ABI
      && arm_vector_mode_supported_p (mode)
      && pcum->named_count > pcum->nargs + 1)
    {
      if (pcum->iwmmxt_nregs <= 9)
	return gen_rtx_REG (mode, pcum->iwmmxt_nregs + FIRST_IWMMXT_REGNUM);
      else
	{
	  pcum->can_split = false;
	  return NULL_RTX;
	}
    }

  /* Put doubleword aligned quantities in even register pairs.  */
  if ((pcum->nregs & 1) && ARM_DOUBLEWORD_ALIGN)
    {
      int res = arm_needs_doubleword_align (mode, type);
      if (res < 0 && warn_psabi)
	inform (input_location, "parameter passing for argument of type "
		"%qT changed in GCC 7.1", type);
      else if (res > 0)
	pcum->nregs++;
    }

  /* Only allow splitting an arg between regs and memory if all preceding
     args were allocated to regs.  For args passed by reference we only count
     the reference pointer.  */
  if (pcum->can_split)
    nregs = 1;
  else
    nregs = ARM_NUM_REGS2 (mode, type);

  if (!named || pcum->nregs + nregs > NUM_ARG_REGS)
    return NULL_RTX;

  return gen_rtx_REG (mode, pcum->nregs);
}

static unsigned int
arm_function_arg_boundary (machine_mode mode, const_tree type)
{
  if (!ARM_DOUBLEWORD_ALIGN)
    return PARM_BOUNDARY;

  int res = arm_needs_doubleword_align (mode, type);
  if (res < 0 && warn_psabi)
    inform (input_location, "parameter passing for argument of type %qT "
	    "changed in GCC 7.1", type);

  return res > 0 ? DOUBLEWORD_ALIGNMENT : PARM_BOUNDARY;
}

static int
arm_arg_partial_bytes (cumulative_args_t pcum_v, machine_mode mode,
		       tree type, bool named)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  int nregs = pcum->nregs;

  if (pcum->pcs_variant <= ARM_PCS_AAPCS_LOCAL)
    {
      aapcs_layout_arg (pcum, mode, type, named);
      return pcum->aapcs_partial;
    }

  if (TARGET_IWMMXT_ABI && arm_vector_mode_supported_p (mode))
    return 0;

  if (NUM_ARG_REGS > nregs
      && (NUM_ARG_REGS < nregs + ARM_NUM_REGS2 (mode, type))
      && pcum->can_split)
    return (NUM_ARG_REGS - nregs) * UNITS_PER_WORD;

  return 0;
}

/* Update the data in PCUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

static void
arm_function_arg_advance (cumulative_args_t pcum_v, machine_mode mode,
			  const_tree type, bool named)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);

  if (pcum->pcs_variant <= ARM_PCS_AAPCS_LOCAL)
    {
      aapcs_layout_arg (pcum, mode, type, named);

      if (pcum->aapcs_cprc_slot >= 0)
	{
	  aapcs_cp_arg_layout[pcum->aapcs_cprc_slot].advance (pcum, mode,
							      type);
	  pcum->aapcs_cprc_slot = -1;
	}

      /* Generic stuff.  */
      pcum->aapcs_arg_processed = false;
      pcum->aapcs_ncrn = pcum->aapcs_next_ncrn;
      pcum->aapcs_reg = NULL_RTX;
      pcum->aapcs_partial = 0;
    }
  else
    {
      pcum->nargs += 1;
      if (arm_vector_mode_supported_p (mode)
	  && pcum->named_count > pcum->nargs
	  && TARGET_IWMMXT_ABI)
	pcum->iwmmxt_nregs += 1;
      else
	pcum->nregs += ARM_NUM_REGS2 (mode, type);
    }
}

/* Variable sized types are passed by reference.  This is a GCC
   extension to the ARM ABI.  */

static bool
arm_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
		       machine_mode mode ATTRIBUTE_UNUSED,
		       const_tree type, bool named ATTRIBUTE_UNUSED)
{
  return type && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST;
}

/* Encode the current state of the #pragma [no_]long_calls.  */
typedef enum
{
  OFF,		/* No #pragma [no_]long_calls is in effect.  */
  LONG,		/* #pragma long_calls is in effect.  */
  SHORT		/* #pragma no_long_calls is in effect.  */
} arm_pragma_enum;

static arm_pragma_enum arm_pragma_long_calls = OFF;

void
arm_pr_long_calls (struct cpp_reader * pfile ATTRIBUTE_UNUSED)
{
  arm_pragma_long_calls = LONG;
}

void
arm_pr_no_long_calls (struct cpp_reader * pfile ATTRIBUTE_UNUSED)
{
  arm_pragma_long_calls = SHORT;
}

void
arm_pr_long_calls_off (struct cpp_reader * pfile ATTRIBUTE_UNUSED)
{
  arm_pragma_long_calls = OFF;
}

/* Handle an attribute requiring a FUNCTION_DECL;
   arguments as in struct attribute_spec.handler.  */
static tree
arm_handle_fndecl_attribute (tree *node, tree name, tree args ATTRIBUTE_UNUSED,
			     int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "interrupt" or "isr" attribute;
   arguments as in struct attribute_spec.handler.  */
static tree
arm_handle_isr_attribute (tree *node, tree name, tree args, int flags,
			  bool *no_add_attrs)
{
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) != FUNCTION_DECL)
	{
	  warning (OPT_Wattributes, "%qE attribute only applies to functions",
		   name);
	  *no_add_attrs = true;
	}
      /* FIXME: the argument if any is checked for type attributes;
	 should it be checked for decl ones?  */
    }
  else
    {
      if (TREE_CODE (*node) == FUNCTION_TYPE
	  || TREE_CODE (*node) == METHOD_TYPE)
	{
	  if (arm_isr_value (args) == ARM_FT_UNKNOWN)
	    {
	      warning (OPT_Wattributes, "%qE attribute ignored",
		       name);
	      *no_add_attrs = true;
	    }
	}
      else if (TREE_CODE (*node) == POINTER_TYPE
	       && (TREE_CODE (TREE_TYPE (*node)) == FUNCTION_TYPE
		   || TREE_CODE (TREE_TYPE (*node)) == METHOD_TYPE)
	       && arm_isr_value (args) != ARM_FT_UNKNOWN)
	{
	  *node = build_variant_type_copy (*node);
	  TREE_TYPE (*node) = build_type_attribute_variant
	    (TREE_TYPE (*node),
	     tree_cons (name, args, TYPE_ATTRIBUTES (TREE_TYPE (*node))));
	  *no_add_attrs = true;
	}
      else
	{
	  /* Possibly pass this attribute on from the type to a decl.  */
	  if (flags & ((int) ATTR_FLAG_DECL_NEXT
		       | (int) ATTR_FLAG_FUNCTION_NEXT
		       | (int) ATTR_FLAG_ARRAY_NEXT))
	    {
	      *no_add_attrs = true;
	      return tree_cons (name, args, NULL_TREE);
	    }
	  else
	    {
	      warning (OPT_Wattributes, "%qE attribute ignored",
		       name);
	    }
	}
    }

  return NULL_TREE;
}

/* Handle a "pcs" attribute; arguments as in struct
   attribute_spec.handler.  */
static tree
arm_handle_pcs_attribute (tree *node ATTRIBUTE_UNUSED, tree name, tree args,
			  int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (arm_pcs_from_attribute (args) == ARM_PCS_UNKNOWN)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
/* Handle the "notshared" attribute.  This attribute is another way of
   requesting hidden visibility.  ARM's compiler supports
   "__declspec(notshared)"; we support the same thing via an
   attribute.  */

static tree
arm_handle_notshared_attribute (tree *node,
				tree name ATTRIBUTE_UNUSED,
				tree args ATTRIBUTE_UNUSED,
				int flags ATTRIBUTE_UNUSED,
				bool *no_add_attrs)
{
  tree decl = TYPE_NAME (*node);

  if (decl)
    {
      DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
      DECL_VISIBILITY_SPECIFIED (decl) = 1;
      *no_add_attrs = false;
    }
  return NULL_TREE;
}
#endif

/* This function returns true if a function with declaration FNDECL and type
   FNTYPE uses the stack to pass arguments or return variables and false
   otherwise.  This is used for functions with the attributes
   'cmse_nonsecure_call' or 'cmse_nonsecure_entry' and this function will issue
   diagnostic messages if the stack is used.  NAME is the name of the attribute
   used.  */

static bool
cmse_func_args_or_return_in_stack (tree fndecl, tree name, tree fntype)
{
  function_args_iterator args_iter;
  CUMULATIVE_ARGS args_so_far_v;
  cumulative_args_t args_so_far;
  bool first_param = true;
  tree arg_type, prev_arg_type = NULL_TREE, ret_type;

  /* Error out if any argument is passed on the stack.  */
  arm_init_cumulative_args (&args_so_far_v, fntype, NULL_RTX, fndecl);
  args_so_far = pack_cumulative_args (&args_so_far_v);
  FOREACH_FUNCTION_ARGS (fntype, arg_type, args_iter)
    {
      rtx arg_rtx;
      machine_mode arg_mode = TYPE_MODE (arg_type);

      prev_arg_type = arg_type;
      if (VOID_TYPE_P (arg_type))
	continue;

      if (!first_param)
	arm_function_arg_advance (args_so_far, arg_mode, arg_type, true);
      arg_rtx = arm_function_arg (args_so_far, arg_mode, arg_type, true);
      if (!arg_rtx
	  || arm_arg_partial_bytes (args_so_far, arg_mode, arg_type, true))
	{
	  error ("%qE attribute not available to functions with arguments "
		 "passed on the stack", name);
	  return true;
	}
      first_param = false;
    }

  /* Error out for variadic functions since we cannot control how many
     arguments will be passed and thus stack could be used.  stdarg_p () is not
     used for the checking to avoid browsing arguments twice.  */
  if (prev_arg_type != NULL_TREE && !VOID_TYPE_P (prev_arg_type))
    {
      error ("%qE attribute not available to functions with variable number "
	     "of arguments", name);
      return true;
    }

  /* Error out if return value is passed on the stack.  */
  ret_type = TREE_TYPE (fntype);
  if (arm_return_in_memory (ret_type, fntype))
    {
      error ("%qE attribute not available to functions that return value on "
	     "the stack", name);
      return true;
    }
  return false;
}

/* Called upon detection of the use of the cmse_nonsecure_entry attribute, this
   function will check whether the attribute is allowed here and will add the
   attribute to the function declaration tree or otherwise issue a warning.  */

static tree
arm_handle_cmse_nonsecure_entry (tree *node, tree name,
				 tree /* args */,
				 int /* flags */,
				 bool *no_add_attrs)
{
  tree fndecl;

  if (!use_cmse)
    {
      *no_add_attrs = true;
      warning (OPT_Wattributes, "%qE attribute ignored without -mcmse option.",
	       name);
      return NULL_TREE;
    }

  /* Ignore attribute for function types.  */
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  fndecl = *node;

  /* Warn for static linkage functions.  */
  if (!TREE_PUBLIC (fndecl))
    {
      warning (OPT_Wattributes, "%qE attribute has no effect on functions "
	       "with static linkage", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  *no_add_attrs |= cmse_func_args_or_return_in_stack (fndecl, name,
						TREE_TYPE (fndecl));
  return NULL_TREE;
}


/* Called upon detection of the use of the cmse_nonsecure_call attribute, this
   function will check whether the attribute is allowed here and will add the
   attribute to the function type tree or otherwise issue a diagnostic.  The
   reason we check this at declaration time is to only allow the use of the
   attribute with declarations of function pointers and not function
   declarations.  This function checks NODE is of the expected type and issues
   diagnostics otherwise using NAME.  If it is not of the expected type
   *NO_ADD_ATTRS will be set to true.  */

static tree
arm_handle_cmse_nonsecure_call (tree *node, tree name,
				 tree /* args */,
				 int /* flags */,
				 bool *no_add_attrs)
{
  tree decl = NULL_TREE, fntype = NULL_TREE;
  tree type;

  if (!use_cmse)
    {
      *no_add_attrs = true;
      warning (OPT_Wattributes, "%qE attribute ignored without -mcmse option.",
	       name);
      return NULL_TREE;
    }

  if (TREE_CODE (*node) == VAR_DECL || TREE_CODE (*node) == TYPE_DECL)
    {
      decl = *node;
      fntype = TREE_TYPE (decl);
    }

  while (fntype != NULL_TREE && TREE_CODE (fntype) == POINTER_TYPE)
    fntype = TREE_TYPE (fntype);

  if (!decl || TREE_CODE (fntype) != FUNCTION_TYPE)
    {
	warning (OPT_Wattributes, "%qE attribute only applies to base type of a "
		 "function pointer", name);
	*no_add_attrs = true;
	return NULL_TREE;
    }

  *no_add_attrs |= cmse_func_args_or_return_in_stack (NULL, name, fntype);

  if (*no_add_attrs)
    return NULL_TREE;

  /* Prevent trees being shared among function types with and without
     cmse_nonsecure_call attribute.  */
  type = TREE_TYPE (decl);

  type = build_distinct_type_copy (type);
  TREE_TYPE (decl) = type;
  fntype = type;

  while (TREE_CODE (fntype) != FUNCTION_TYPE)
    {
      type = fntype;
      fntype = TREE_TYPE (fntype);
      fntype = build_distinct_type_copy (fntype);
      TREE_TYPE (type) = fntype;
    }

  /* Construct a type attribute and add it to the function type.  */
  tree attrs = tree_cons (get_identifier ("cmse_nonsecure_call"), NULL_TREE,
			  TYPE_ATTRIBUTES (fntype));
  TYPE_ATTRIBUTES (fntype) = attrs;
  return NULL_TREE;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */
static int
arm_comp_type_attributes (const_tree type1, const_tree type2)
{
  int l1, l2, s1, s2;

  /* Check for mismatch of non-default calling convention.  */
  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched call attributes.  */
  l1 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type2)) != NULL;
  s1 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type1)) != NULL;
  s2 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type2)) != NULL;

  /* Only bother to check if an attribute is defined.  */
  if (l1 | l2 | s1 | s2)
    {
      /* If one type has an attribute, the other must have the same attribute.  */
      if ((l1 != l2) || (s1 != s2))
	return 0;

      /* Disallow mixed attributes.  */
      if ((l1 & s2) || (l2 & s1))
	return 0;
    }

  /* Check for mismatched ISR attribute.  */
  l1 = lookup_attribute ("isr", TYPE_ATTRIBUTES (type1)) != NULL;
  if (! l1)
    l1 = lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("isr", TYPE_ATTRIBUTES (type2)) != NULL;
  if (! l2)
    l1 = lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type2)) != NULL;
  if (l1 != l2)
    return 0;

  l1 = lookup_attribute ("cmse_nonsecure_call",
			 TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("cmse_nonsecure_call",
			 TYPE_ATTRIBUTES (type2)) != NULL;

  if (l1 != l2)
    return 0;

  return 1;
}

/*  Assigns default attributes to newly defined type.  This is used to
    set short_call/long_call attributes for function types of
    functions defined inside corresponding #pragma scopes.  */
static void
arm_set_default_type_attributes (tree type)
{
  /* Add __attribute__ ((long_call)) to all functions, when
     inside #pragma long_calls or __attribute__ ((short_call)),
     when inside #pragma no_long_calls.  */
  if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    {
      tree type_attr_list, attr_name;
      type_attr_list = TYPE_ATTRIBUTES (type);

      if (arm_pragma_long_calls == LONG)
 	attr_name = get_identifier ("long_call");
      else if (arm_pragma_long_calls == SHORT)
 	attr_name = get_identifier ("short_call");
      else
 	return;

      type_attr_list = tree_cons (attr_name, NULL_TREE, type_attr_list);
      TYPE_ATTRIBUTES (type) = type_attr_list;
    }
}

/* Return true if DECL is known to be linked into section SECTION.  */

static bool
arm_function_in_section_p (tree decl, section *section)
{
  /* We can only be certain about the prevailing symbol definition.  */
  if (!decl_binds_to_current_def_p (decl))
    return false;

  /* If DECL_SECTION_NAME is set, assume it is trustworthy.  */
  if (!DECL_SECTION_NAME (decl))
    {
      /* Make sure that we will not create a unique section for DECL.  */
      if (flag_function_sections || DECL_COMDAT_GROUP (decl))
	return false;
    }

  return function_section (decl) == section;
}

/* Return nonzero if a 32-bit "long_call" should be generated for
   a call from the current function to DECL.  We generate a long_call
   if the function:

        a.  has an __attribute__((long call))
     or b.  is within the scope of a #pragma long_calls
     or c.  the -mlong-calls command line switch has been specified

   However we do not generate a long call if the function:

        d.  has an __attribute__ ((short_call))
     or e.  is inside the scope of a #pragma no_long_calls
     or f.  is defined in the same section as the current function.  */

bool
arm_is_long_call_p (tree decl)
{
  tree attrs;

  if (!decl)
    return TARGET_LONG_CALLS;

  attrs = TYPE_ATTRIBUTES (TREE_TYPE (decl));
  if (lookup_attribute ("short_call", attrs))
    return false;

  /* For "f", be conservative, and only cater for cases in which the
     whole of the current function is placed in the same section.  */
  if (!flag_reorder_blocks_and_partition
      && TREE_CODE (decl) == FUNCTION_DECL
      && arm_function_in_section_p (decl, current_function_section ()))
    return false;

  if (lookup_attribute ("long_call", attrs))
    return true;

  return TARGET_LONG_CALLS;
}

/* Return nonzero if it is ok to make a tail-call to DECL.  */
static bool
arm_function_ok_for_sibcall (tree decl, tree exp)
{
  unsigned long func_type;

  if (cfun->machine->sibcall_blocked)
    return false;

  /* Never tailcall something if we are generating code for Thumb-1.  */
  if (TARGET_THUMB1)
    return false;

  /* The PIC register is live on entry to VxWorks PLT entries, so we
     must make the call before restoring the PIC register.  */
  if (TARGET_VXWORKS_RTP && flag_pic && decl && !targetm.binds_local_p (decl))
    return false;

  /* ??? Cannot tail-call to long calls with APCS frame and VFP, because IP
     may be used both as target of the call and base register for restoring
     the VFP registers  */
  if (TARGET_APCS_FRAME && TARGET_ARM
      && TARGET_HARD_FLOAT
      && decl && arm_is_long_call_p (decl))
    return false;

  /* If we are interworking and the function is not declared static
     then we can't tail-call it unless we know that it exists in this
     compilation unit (since it might be a Thumb routine).  */
  if (TARGET_INTERWORK && decl && TREE_PUBLIC (decl)
      && !TREE_ASM_WRITTEN (decl))
    return false;

  func_type = arm_current_func_type ();
  /* Never tailcall from an ISR routine - it needs a special exit sequence.  */
  if (IS_INTERRUPT (func_type))
    return false;

  /* ARMv8-M non-secure entry functions need to return with bxns which is only
     generated for entry functions themselves.  */
  if (IS_CMSE_ENTRY (arm_current_func_type ()))
    return false;

  /* We do not allow ARMv8-M non-secure calls to be turned into sibling calls,
     this would complicate matters for later code generation.  */
  if (TREE_CODE (exp) == CALL_EXPR)
    {
      tree fntype = TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (exp)));
      if (lookup_attribute ("cmse_nonsecure_call", TYPE_ATTRIBUTES (fntype)))
	return false;
    }

  if (!VOID_TYPE_P (TREE_TYPE (DECL_RESULT (cfun->decl))))
    {
      /* Check that the return value locations are the same.  For
	 example that we aren't returning a value from the sibling in
	 a VFP register but then need to transfer it to a core
	 register.  */
      rtx a, b;
      tree decl_or_type = decl;

      /* If it is an indirect function pointer, get the function type.  */
      if (!decl)
	decl_or_type = TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (exp)));

      a = arm_function_value (TREE_TYPE (exp), decl_or_type, false);
      b = arm_function_value (TREE_TYPE (DECL_RESULT (cfun->decl)),
			      cfun->decl, false);
      if (!rtx_equal_p (a, b))
	return false;
    }

  /* Never tailcall if function may be called with a misaligned SP.  */
  if (IS_STACKALIGN (func_type))
    return false;

  /* The AAPCS says that, on bare-metal, calls to unresolved weak
     references should become a NOP.  Don't convert such calls into
     sibling calls.  */
  if (TARGET_AAPCS_BASED
      && arm_abi == ARM_ABI_AAPCS
      && decl
      && DECL_WEAK (decl))
    return false;

  /* We cannot do a tailcall for an indirect call by descriptor if all the
     argument registers are used because the only register left to load the
     address is IP and it will already contain the static chain.  */
  if (!decl && CALL_EXPR_BY_DESCRIPTOR (exp) && !flag_trampolines)
    {
      tree fntype = TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (exp)));
      CUMULATIVE_ARGS cum;
      cumulative_args_t cum_v;

      arm_init_cumulative_args (&cum, fntype, NULL_RTX, NULL_TREE);
      cum_v = pack_cumulative_args (&cum);

      for (tree t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
	{
	  tree type = TREE_VALUE (t);
	  if (!VOID_TYPE_P (type))
	    arm_function_arg_advance (cum_v, TYPE_MODE (type), type, true);
	}

      if (!arm_function_arg (cum_v, SImode, integer_type_node, true))
	return false;
    }

  /* Everything else is ok.  */
  return true;
}


/* Addressing mode support functions.  */

/* Return nonzero if X is a legitimate immediate operand when compiling
   for PIC.  We know that X satisfies CONSTANT_P and flag_pic is true.  */
int
legitimate_pic_operand_p (rtx x)
{
  if (GET_CODE (x) == SYMBOL_REF
      || (GET_CODE (x) == CONST
	  && GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF))
    return 0;

  return 1;
}

/* Record that the current function needs a PIC register.  Initialize
   cfun->machine->pic_reg if we have not already done so.  */

static void
require_pic_register (void)
{
  /* A lot of the logic here is made obscure by the fact that this
     routine gets called as part of the rtx cost estimation process.
     We don't want those calls to affect any assumptions about the real
     function; and further, we can't call entry_of_function() until we
     start the real expansion process.  */
  if (!crtl->uses_pic_offset_table)
    {
      gcc_assert (can_create_pseudo_p ());
      if (arm_pic_register != INVALID_REGNUM
	  && !(TARGET_THUMB1 && arm_pic_register > LAST_LO_REGNUM))
	{
	  if (!cfun->machine->pic_reg)
	    cfun->machine->pic_reg = gen_rtx_REG (Pmode, arm_pic_register);

	  /* Play games to avoid marking the function as needing pic
	     if we are being called as part of the cost-estimation
	     process.  */
	  if (current_ir_type () != IR_GIMPLE || currently_expanding_to_rtl)
	    crtl->uses_pic_offset_table = 1;
	}
      else
	{
	  rtx_insn *seq, *insn;

	  if (!cfun->machine->pic_reg)
	    cfun->machine->pic_reg = gen_reg_rtx (Pmode);

	  /* Play games to avoid marking the function as needing pic
	     if we are being called as part of the cost-estimation
	     process.  */
	  if (current_ir_type () != IR_GIMPLE || currently_expanding_to_rtl)
	    {
	      crtl->uses_pic_offset_table = 1;
	      start_sequence ();

	      if (TARGET_THUMB1 && arm_pic_register != INVALID_REGNUM
		  && arm_pic_register > LAST_LO_REGNUM)
		emit_move_insn (cfun->machine->pic_reg,
				gen_rtx_REG (Pmode, arm_pic_register));
	      else
		arm_load_pic_register (0UL);

	      seq = get_insns ();
	      end_sequence ();

	      for (insn = seq; insn; insn = NEXT_INSN (insn))
		if (INSN_P (insn))
		  INSN_LOCATION (insn) = prologue_location;

	      /* We can be called during expansion of PHI nodes, where
	         we can't yet emit instructions directly in the final
		 insn stream.  Queue the insns on the entry edge, they will
		 be committed after everything else is expanded.  */
	      insert_insn_on_edge (seq,
				   single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
	    }
	}
    }
}

rtx
legitimize_pic_address (rtx orig, machine_mode mode, rtx reg)
{
  if (GET_CODE (orig) == SYMBOL_REF
      || GET_CODE (orig) == LABEL_REF)
    {
      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      /* VxWorks does not impose a fixed gap between segments; the run-time
	 gap can be different from the object-file gap.  We therefore can't
	 use GOTOFF unless we are absolutely sure that the symbol is in the
	 same segment as the GOT.  Unfortunately, the flexibility of linker
	 scripts means that we can't be sure of that in general, so assume
	 that GOTOFF is never valid on VxWorks.  */
      /* References to weak symbols cannot be resolved locally: they
	 may be overridden by a non-weak definition at link time.  */
      rtx_insn *insn;
      if ((GET_CODE (orig) == LABEL_REF
	   || (GET_CODE (orig) == SYMBOL_REF
	       && SYMBOL_REF_LOCAL_P (orig)
	       && (SYMBOL_REF_DECL (orig)
		   ? !DECL_WEAK (SYMBOL_REF_DECL (orig)) : 1)))
	  && NEED_GOT_RELOC
	  && arm_pic_data_is_text_relative)
	insn = arm_pic_static_addr (orig, reg);
      else
	{
	  rtx pat;
	  rtx mem;

	  /* If this function doesn't have a pic register, create one now.  */
	  require_pic_register ();

	  pat = gen_calculate_pic_address (reg, cfun->machine->pic_reg, orig);

	  /* Make the MEM as close to a constant as possible.  */
	  mem = SET_SRC (pat);
	  gcc_assert (MEM_P (mem) && !MEM_VOLATILE_P (mem));
	  MEM_READONLY_P (mem) = 1;
	  MEM_NOTRAP_P (mem) = 1;

	  insn = emit_insn (pat);
	}

      /* Put a REG_EQUAL note on this insn, so that it can be optimized
	 by loop.  */
      set_unique_reg_note (insn, REG_EQUAL, orig);

      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == cfun->machine->pic_reg)
	return orig;

      /* Handle the case where we have: const (UNSPEC_TLS).  */
      if (GET_CODE (XEXP (orig, 0)) == UNSPEC
	  && XINT (XEXP (orig, 0), 1) == UNSPEC_TLS)
	return orig;

      /* Handle the case where we have:
         const (plus (UNSPEC_TLS) (ADDEND)).  The ADDEND must be a
         CONST_INT.  */
      if (GET_CODE (XEXP (orig, 0)) == PLUS
          && GET_CODE (XEXP (XEXP (orig, 0), 0)) == UNSPEC
          && XINT (XEXP (XEXP (orig, 0), 0), 1) == UNSPEC_TLS)
        {
	  gcc_assert (CONST_INT_P (XEXP (XEXP (orig, 0), 1)));
	  return orig;
	}

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      gcc_assert (GET_CODE (XEXP (orig, 0)) == PLUS);

      base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
      offset = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
				       base == reg ? 0 : reg);

      if (CONST_INT_P (offset))
	{
	  /* The base register doesn't really matter, we only want to
	     test the index for the appropriate mode.  */
	  if (!arm_legitimate_index_p (mode, offset, SET, 0))
	    {
	      gcc_assert (can_create_pseudo_p ());
	      offset = force_reg (Pmode, offset);
	    }

	  if (CONST_INT_P (offset))
	    return plus_constant (Pmode, base, INTVAL (offset));
	}

      if (GET_MODE_SIZE (mode) > 4
	  && (GET_MODE_CLASS (mode) == MODE_INT
	      || TARGET_SOFT_FLOAT))
	{
	  emit_insn (gen_addsi3 (reg, base, offset));
	  return reg;
	}

      return gen_rtx_PLUS (Pmode, base, offset);
    }

  return orig;
}


/* Find a spare register to use during the prolog of a function.  */

static int
thumb_find_work_register (unsigned long pushed_regs_mask)
{
  int reg;

  /* Check the argument registers first as these are call-used.  The
     register allocation order means that sometimes r3 might be used
     but earlier argument registers might not, so check them all.  */
  for (reg = LAST_ARG_REGNUM; reg >= 0; reg --)
    if (!df_regs_ever_live_p (reg))
      return reg;

  /* Before going on to check the call-saved registers we can try a couple
     more ways of deducing that r3 is available.  The first is when we are
     pushing anonymous arguments onto the stack and we have less than 4
     registers worth of fixed arguments(*).  In this case r3 will be part of
     the variable argument list and so we can be sure that it will be
     pushed right at the start of the function.  Hence it will be available
     for the rest of the prologue.
     (*): ie crtl->args.pretend_args_size is greater than 0.  */
  if (cfun->machine->uses_anonymous_args
      && crtl->args.pretend_args_size > 0)
    return LAST_ARG_REGNUM;

  /* The other case is when we have fixed arguments but less than 4 registers
     worth.  In this case r3 might be used in the body of the function, but
     it is not being used to convey an argument into the function.  In theory
     we could just check crtl->args.size to see how many bytes are
     being passed in argument registers, but it seems that it is unreliable.
     Sometimes it will have the value 0 when in fact arguments are being
     passed.  (See testcase execute/20021111-1.c for an example).  So we also
     check the args_info.nregs field as well.  The problem with this field is
     that it makes no allowances for arguments that are passed to the
     function but which are not used.  Hence we could miss an opportunity
     when a function has an unused argument in r3.  But it is better to be
     safe than to be sorry.  */
  if (! cfun->machine->uses_anonymous_args
      && crtl->args.size >= 0
      && crtl->args.size <= (LAST_ARG_REGNUM * UNITS_PER_WORD)
      && (TARGET_AAPCS_BASED
	  ? crtl->args.info.aapcs_ncrn < 4
	  : crtl->args.info.nregs < 4))
    return LAST_ARG_REGNUM;

  /* Otherwise look for a call-saved register that is going to be pushed.  */
  for (reg = LAST_LO_REGNUM; reg > LAST_ARG_REGNUM; reg --)
    if (pushed_regs_mask & (1 << reg))
      return reg;

  if (TARGET_THUMB2)
    {
      /* Thumb-2 can use high regs.  */
      for (reg = FIRST_HI_REGNUM; reg < 15; reg ++)
	if (pushed_regs_mask & (1 << reg))
	  return reg;
    }
  /* Something went wrong - thumb_compute_save_reg_mask()
     should have arranged for a suitable register to be pushed.  */
  gcc_unreachable ();
}

static GTY(()) int pic_labelno;

/* Generate code to load the PIC register.  In thumb mode SCRATCH is a
   low register.  */

void
arm_load_pic_register (unsigned long saved_regs ATTRIBUTE_UNUSED)
{
  rtx l1, labelno, pic_tmp, pic_rtx, pic_reg;

  if (crtl->uses_pic_offset_table == 0 || TARGET_SINGLE_PIC_BASE)
    return;

  gcc_assert (flag_pic);

  pic_reg = cfun->machine->pic_reg;
  if (TARGET_VXWORKS_RTP)
    {
      pic_rtx = gen_rtx_SYMBOL_REF (Pmode, VXWORKS_GOTT_BASE);
      pic_rtx = gen_rtx_CONST (Pmode, pic_rtx);
      emit_insn (gen_pic_load_addr_32bit (pic_reg, pic_rtx));

      emit_insn (gen_rtx_SET (pic_reg, gen_rtx_MEM (Pmode, pic_reg)));

      pic_tmp = gen_rtx_SYMBOL_REF (Pmode, VXWORKS_GOTT_INDEX);
      emit_insn (gen_pic_offset_arm (pic_reg, pic_reg, pic_tmp));
    }
  else
    {
      /* We use an UNSPEC rather than a LABEL_REF because this label
	 never appears in the code stream.  */

      labelno = GEN_INT (pic_labelno++);
      l1 = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_PIC_LABEL);
      l1 = gen_rtx_CONST (VOIDmode, l1);

      /* On the ARM the PC register contains 'dot + 8' at the time of the
	 addition, on the Thumb it is 'dot + 4'.  */
      pic_rtx = plus_constant (Pmode, l1, TARGET_ARM ? 8 : 4);
      pic_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, pic_rtx),
				UNSPEC_GOTSYM_OFF);
      pic_rtx = gen_rtx_CONST (Pmode, pic_rtx);

      if (TARGET_32BIT)
	{
	  emit_insn (gen_pic_load_addr_unified (pic_reg, pic_rtx, labelno));
	}
      else /* TARGET_THUMB1 */
	{
	  if (arm_pic_register != INVALID_REGNUM
	      && REGNO (pic_reg) > LAST_LO_REGNUM)
	    {
	      /* We will have pushed the pic register, so we should always be
		 able to find a work register.  */
	      pic_tmp = gen_rtx_REG (SImode,
				     thumb_find_work_register (saved_regs));
	      emit_insn (gen_pic_load_addr_thumb1 (pic_tmp, pic_rtx));
	      emit_insn (gen_movsi (pic_offset_table_rtx, pic_tmp));
	      emit_insn (gen_pic_add_dot_plus_four (pic_reg, pic_reg, labelno));
	    }
	  else if (arm_pic_register != INVALID_REGNUM
		   && arm_pic_register > LAST_LO_REGNUM
		   && REGNO (pic_reg) <= LAST_LO_REGNUM)
	    {
	      emit_insn (gen_pic_load_addr_unified (pic_reg, pic_rtx, labelno));
	      emit_move_insn (gen_rtx_REG (Pmode, arm_pic_register), pic_reg);
	      emit_use (gen_rtx_REG (Pmode, arm_pic_register));
	    }
	  else
	    emit_insn (gen_pic_load_addr_unified (pic_reg, pic_rtx, labelno));
	}
    }

  /* Need to emit this whether or not we obey regdecls,
     since setjmp/longjmp can cause life info to screw up.  */
  emit_use (pic_reg);
}

/* Generate code to load the address of a static var when flag_pic is set.  */
static rtx_insn *
arm_pic_static_addr (rtx orig, rtx reg)
{
  rtx l1, labelno, offset_rtx;

  gcc_assert (flag_pic);

  /* We use an UNSPEC rather than a LABEL_REF because this label
     never appears in the code stream.  */
  labelno = GEN_INT (pic_labelno++);
  l1 = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_PIC_LABEL);
  l1 = gen_rtx_CONST (VOIDmode, l1);

  /* On the ARM the PC register contains 'dot + 8' at the time of the
     addition, on the Thumb it is 'dot + 4'.  */
  offset_rtx = plus_constant (Pmode, l1, TARGET_ARM ? 8 : 4);
  offset_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, orig, offset_rtx),
                               UNSPEC_SYMBOL_OFFSET);
  offset_rtx = gen_rtx_CONST (Pmode, offset_rtx);

  return emit_insn (gen_pic_load_addr_unified (reg, offset_rtx, labelno));
}

/* Return nonzero if X is valid as an ARM state addressing register.  */
static int
arm_address_register_rtx_p (rtx x, int strict_p)
{
  int regno;

  if (!REG_P (x))
    return 0;

  regno = REGNO (x);

  if (strict_p)
    return ARM_REGNO_OK_FOR_BASE_P (regno);

  return (regno <= LAST_ARM_REGNUM
	  || regno >= FIRST_PSEUDO_REGISTER
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
}

/* Return TRUE if this rtx is the difference of a symbol and a label,
   and will reduce to a PC-relative relocation in the object file.
   Expressions like this can be left alone when generating PIC, rather
   than forced through the GOT.  */
static int
pcrel_constant_p (rtx x)
{
  if (GET_CODE (x) == MINUS)
    return symbol_mentioned_p (XEXP (x, 0)) && label_mentioned_p (XEXP (x, 1));

  return FALSE;
}

/* Return true if X will surely end up in an index register after next
   splitting pass.  */
static bool
will_be_in_index_register (const_rtx x)
{
  /* arm.md: calculate_pic_address will split this into a register.  */
  return GET_CODE (x) == UNSPEC && (XINT (x, 1) == UNSPEC_PIC_SYM);
}

/* Return nonzero if X is a valid ARM state address operand.  */
int
arm_legitimate_address_outer_p (machine_mode mode, rtx x, RTX_CODE outer,
			        int strict_p)
{
  bool use_ldrd;
  enum rtx_code code = GET_CODE (x);

  if (arm_address_register_rtx_p (x, strict_p))
    return 1;

  use_ldrd = (TARGET_LDRD
	      && (mode == DImode || mode == DFmode));

  if (code == POST_INC || code == PRE_DEC
      || ((code == PRE_INC || code == POST_DEC)
	  && (use_ldrd || GET_MODE_SIZE (mode) <= 4)))
    return arm_address_register_rtx_p (XEXP (x, 0), strict_p);

  else if ((code == POST_MODIFY || code == PRE_MODIFY)
	   && arm_address_register_rtx_p (XEXP (x, 0), strict_p)
	   && GET_CODE (XEXP (x, 1)) == PLUS
	   && rtx_equal_p (XEXP (XEXP (x, 1), 0), XEXP (x, 0)))
    {
      rtx addend = XEXP (XEXP (x, 1), 1);

      /* Don't allow ldrd post increment by register because it's hard
	 to fixup invalid register choices.  */
      if (use_ldrd
	  && GET_CODE (x) == POST_MODIFY
	  && REG_P (addend))
	return 0;

      return ((use_ldrd || GET_MODE_SIZE (mode) <= 4)
	      && arm_legitimate_index_p (mode, addend, outer, strict_p));
    }

  /* After reload constants split into minipools will have addresses
     from a LABEL_REF.  */
  else if (reload_completed
	   && (code == LABEL_REF
	       || (code == CONST
		   && GET_CODE (XEXP (x, 0)) == PLUS
		   && GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF
		   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))))
    return 1;

  else if (mode == TImode || (TARGET_NEON && VALID_NEON_STRUCT_MODE (mode)))
    return 0;

  else if (code == PLUS)
    {
      rtx xop0 = XEXP (x, 0);
      rtx xop1 = XEXP (x, 1);

      return ((arm_address_register_rtx_p (xop0, strict_p)
	       && ((CONST_INT_P (xop1)
		    && arm_legitimate_index_p (mode, xop1, outer, strict_p))
		   || (!strict_p && will_be_in_index_register (xop1))))
	      || (arm_address_register_rtx_p (xop1, strict_p)
		  && arm_legitimate_index_p (mode, xop0, outer, strict_p)));
    }

#if 0
  /* Reload currently can't handle MINUS, so disable this for now */
  else if (GET_CODE (x) == MINUS)
    {
      rtx xop0 = XEXP (x, 0);
      rtx xop1 = XEXP (x, 1);

      return (arm_address_register_rtx_p (xop0, strict_p)
	      && arm_legitimate_index_p (mode, xop1, outer, strict_p));
    }
#endif

  else if (GET_MODE_CLASS (mode) != MODE_FLOAT
	   && code == SYMBOL_REF
	   && CONSTANT_POOL_ADDRESS_P (x)
	   && ! (flag_pic
		 && symbol_mentioned_p (get_pool_constant (x))
		 && ! pcrel_constant_p (get_pool_constant (x))))
    return 1;

  return 0;
}

/* Return true if we can avoid creating a constant pool entry for x.  */
static bool
can_avoid_literal_pool_for_label_p (rtx x)
{
  /* Normally we can assign constant values to target registers without
     the help of constant pool.  But there are cases we have to use constant
     pool like:
     1) assign a label to register.
     2) sign-extend a 8bit value to 32bit and then assign to register.

     Constant pool access in format:
     (set (reg r0) (mem (symbol_ref (".LC0"))))
     will cause the use of literal pool (later in function arm_reorg).
     So here we mark such format as an invalid format, then the compiler
     will adjust it into:
     (set (reg r0) (symbol_ref (".LC0")))
     (set (reg r0) (mem (reg r0))).
     No extra register is required, and (mem (reg r0)) won't cause the use
     of literal pools.  */
  if (arm_disable_literal_pool && GET_CODE (x) == SYMBOL_REF
      && CONSTANT_POOL_ADDRESS_P (x))
    return 1;
  return 0;
}


/* Return nonzero if X is a valid Thumb-2 address operand.  */
static int
thumb2_legitimate_address_p (machine_mode mode, rtx x, int strict_p)
{
  bool use_ldrd;
  enum rtx_code code = GET_CODE (x);

  if (arm_address_register_rtx_p (x, strict_p))
    return 1;

  use_ldrd = (TARGET_LDRD
	      && (mode == DImode || mode == DFmode));

  if (code == POST_INC || code == PRE_DEC
      || ((code == PRE_INC || code == POST_DEC)
	  && (use_ldrd || GET_MODE_SIZE (mode) <= 4)))
    return arm_address_register_rtx_p (XEXP (x, 0), strict_p);

  else if ((code == POST_MODIFY || code == PRE_MODIFY)
	   && arm_address_register_rtx_p (XEXP (x, 0), strict_p)
	   && GET_CODE (XEXP (x, 1)) == PLUS
	   && rtx_equal_p (XEXP (XEXP (x, 1), 0), XEXP (x, 0)))
    {
      /* Thumb-2 only has autoincrement by constant.  */
      rtx addend = XEXP (XEXP (x, 1), 1);
      HOST_WIDE_INT offset;

      if (!CONST_INT_P (addend))
	return 0;

      offset = INTVAL(addend);
      if (GET_MODE_SIZE (mode) <= 4)
	return (offset > -256 && offset < 256);

      return (use_ldrd && offset > -1024 && offset < 1024
	      && (offset & 3) == 0);
    }

  /* After reload constants split into minipools will have addresses
     from a LABEL_REF.  */
  else if (reload_completed
	   && (code == LABEL_REF
	       || (code == CONST
		   && GET_CODE (XEXP (x, 0)) == PLUS
		   && GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF
		   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))))
    return 1;

  else if (mode == TImode || (TARGET_NEON && VALID_NEON_STRUCT_MODE (mode)))
    return 0;

  else if (code == PLUS)
    {
      rtx xop0 = XEXP (x, 0);
      rtx xop1 = XEXP (x, 1);

      return ((arm_address_register_rtx_p (xop0, strict_p)
	       && (thumb2_legitimate_index_p (mode, xop1, strict_p)
		   || (!strict_p && will_be_in_index_register (xop1))))
	      || (arm_address_register_rtx_p (xop1, strict_p)
		  && thumb2_legitimate_index_p (mode, xop0, strict_p)));
    }

  else if (can_avoid_literal_pool_for_label_p (x))
    return 0;

  else if (GET_MODE_CLASS (mode) != MODE_FLOAT
	   && code == SYMBOL_REF
	   && CONSTANT_POOL_ADDRESS_P (x)
	   && ! (flag_pic
		 && symbol_mentioned_p (get_pool_constant (x))
		 && ! pcrel_constant_p (get_pool_constant (x))))
    return 1;

  return 0;
}

/* Return nonzero if INDEX is valid for an address index operand in
   ARM state.  */
static int
arm_legitimate_index_p (machine_mode mode, rtx index, RTX_CODE outer,
			int strict_p)
{
  HOST_WIDE_INT range;
  enum rtx_code code = GET_CODE (index);

  /* Standard coprocessor addressing modes.  */
  if (TARGET_HARD_FLOAT
      && (mode == SFmode || mode == DFmode))
    return (code == CONST_INT && INTVAL (index) < 1024
	    && INTVAL (index) > -1024
	    && (INTVAL (index) & 3) == 0);

  /* For quad modes, we restrict the constant offset to be slightly less
     than what the instruction format permits.  We do this because for
     quad mode moves, we will actually decompose them into two separate
     double-mode reads or writes.  INDEX must therefore be a valid
     (double-mode) offset and so should INDEX+8.  */
  if (TARGET_NEON && VALID_NEON_QREG_MODE (mode))
    return (code == CONST_INT
	    && INTVAL (index) < 1016
	    && INTVAL (index) > -1024
	    && (INTVAL (index) & 3) == 0);

  /* We have no such constraint on double mode offsets, so we permit the
     full range of the instruction format.  */
  if (TARGET_NEON && VALID_NEON_DREG_MODE (mode))
    return (code == CONST_INT
	    && INTVAL (index) < 1024
	    && INTVAL (index) > -1024
	    && (INTVAL (index) & 3) == 0);

  if (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (mode))
    return (code == CONST_INT
	    && INTVAL (index) < 1024
	    && INTVAL (index) > -1024
	    && (INTVAL (index) & 3) == 0);

  if (arm_address_register_rtx_p (index, strict_p)
      && (GET_MODE_SIZE (mode) <= 4))
    return 1;

  if (mode == DImode || mode == DFmode)
    {
      if (code == CONST_INT)
	{
	  HOST_WIDE_INT val = INTVAL (index);

	  /* Assume we emit ldrd or 2x ldr if !TARGET_LDRD.
	     If vldr is selected it uses arm_coproc_mem_operand.  */
	  if (TARGET_LDRD)
	    return val > -256 && val < 256;
	  else
	    return val > -4096 && val < 4092;
	}

      return TARGET_LDRD && arm_address_register_rtx_p (index, strict_p);
    }

  if (GET_MODE_SIZE (mode) <= 4
      && ! (arm_arch4
	    && (mode == HImode
		|| mode == HFmode
		|| (mode == QImode && outer == SIGN_EXTEND))))
    {
      if (code == MULT)
	{
	  rtx xiop0 = XEXP (index, 0);
	  rtx xiop1 = XEXP (index, 1);

	  return ((arm_address_register_rtx_p (xiop0, strict_p)
		   && power_of_two_operand (xiop1, SImode))
		  || (arm_address_register_rtx_p (xiop1, strict_p)
		      && power_of_two_operand (xiop0, SImode)));
	}
      else if (code == LSHIFTRT || code == ASHIFTRT
	       || code == ASHIFT || code == ROTATERT)
	{
	  rtx op = XEXP (index, 1);

	  return (arm_address_register_rtx_p (XEXP (index, 0), strict_p)
		  && CONST_INT_P (op)
		  && INTVAL (op) > 0
		  && INTVAL (op) <= 31);
	}
    }

  /* For ARM v4 we may be doing a sign-extend operation during the
     load.  */
  if (arm_arch4)
    {
      if (mode == HImode
	  || mode == HFmode
	  || (outer == SIGN_EXTEND && mode == QImode))
	range = 256;
      else
	range = 4096;
    }
  else
    range = (mode == HImode || mode == HFmode) ? 4095 : 4096;

  return (code == CONST_INT
	  && INTVAL (index) < range
	  && INTVAL (index) > -range);
}

/* Return true if OP is a valid index scaling factor for Thumb-2 address
   index operand.  i.e. 1, 2, 4 or 8.  */
static bool
thumb2_index_mul_operand (rtx op)
{
  HOST_WIDE_INT val;

  if (!CONST_INT_P (op))
    return false;

  val = INTVAL(op);
  return (val == 1 || val == 2 || val == 4 || val == 8);
}

/* Return nonzero if INDEX is a valid Thumb-2 address index operand.  */
static int
thumb2_legitimate_index_p (machine_mode mode, rtx index, int strict_p)
{
  enum rtx_code code = GET_CODE (index);

  /* ??? Combine arm and thumb2 coprocessor addressing modes.  */
  /* Standard coprocessor addressing modes.  */
  if (TARGET_HARD_FLOAT
      && (mode == SFmode || mode == DFmode))
    return (code == CONST_INT && INTVAL (index) < 1024
	    /* Thumb-2 allows only > -256 index range for it's core register
	       load/stores. Since we allow SF/DF in core registers, we have
	       to use the intersection between -256~4096 (core) and -1024~1024
	       (coprocessor).  */
	    && INTVAL (index) > -256
	    && (INTVAL (index) & 3) == 0);

  if (TARGET_REALLY_IWMMXT && VALID_IWMMXT_REG_MODE (mode))
    {
      /* For DImode assume values will usually live in core regs
	 and only allow LDRD addressing modes.  */
      if (!TARGET_LDRD || mode != DImode)
	return (code == CONST_INT
		&& INTVAL (index) < 1024
		&& INTVAL (index) > -1024
		&& (INTVAL (index) & 3) == 0);
    }

  /* For quad modes, we restrict the constant offset to be slightly less
     than what the instruction format permits.  We do this because for
     quad mode moves, we will actually decompose them into two separate
     double-mode reads or writes.  INDEX must therefore be a valid
     (double-mode) offset and so should INDEX+8.  */
  if (TARGET_NEON && VALID_NEON_QREG_MODE (mode))
    return (code == CONST_INT
	    && INTVAL (index) < 1016
	    && INTVAL (index) > -1024
	    && (INTVAL (index) & 3) == 0);

  /* We have no such constraint on double mode offsets, so we permit the
     full range of the instruction format.  */
  if (TARGET_NEON && VALID_NEON_DREG_MODE (mode))
    return (code == CONST_INT
	    && INTVAL (index) < 1024
	    && INTVAL (index) > -1024
	    && (INTVAL (index) & 3) == 0);

  if (arm_address_register_rtx_p (index, strict_p)
      && (GET_MODE_SIZE (mode) <= 4))
    return 1;

  if (mode == DImode || mode == DFmode)
    {
      if (code == CONST_INT)
	{
	  HOST_WIDE_INT val = INTVAL (index);
	  /* Thumb-2 ldrd only has reg+const addressing modes.
	     Assume we emit ldrd or 2x ldr if !TARGET_LDRD.
	     If vldr is selected it uses arm_coproc_mem_operand.  */
	  if (TARGET_LDRD)
	    return IN_RANGE (val, -1020, 1020) && (val & 3) == 0;
	  else
	    return IN_RANGE (val, -255, 4095 - 4);
	}
      else
	return 0;
    }

  if (code == MULT)
    {
      rtx xiop0 = XEXP (index, 0);
      rtx xiop1 = XEXP (index, 1);

      return ((arm_address_register_rtx_p (xiop0, strict_p)
	       && thumb2_index_mul_operand (xiop1))
	      || (arm_address_register_rtx_p (xiop1, strict_p)
		  && thumb2_index_mul_operand (xiop0)));
    }
  else if (code == ASHIFT)
    {
      rtx op = XEXP (index, 1);

      return (arm_address_register_rtx_p (XEXP (index, 0), strict_p)
	      && CONST_INT_P (op)
	      && INTVAL (op) > 0
	      && INTVAL (op) <= 3);
    }

  return (code == CONST_INT
	  && INTVAL (index) < 4096
	  && INTVAL (index) > -256);
}

/* Return nonzero if X is valid as a 16-bit Thumb state base register.  */
static int
thumb1_base_register_rtx_p (rtx x, machine_mode mode, int strict_p)
{
  int regno;

  if (!REG_P (x))
    return 0;

  regno = REGNO (x);

  if (strict_p)
    return THUMB1_REGNO_MODE_OK_FOR_BASE_P (regno, mode);

  return (regno <= LAST_LO_REGNUM
	  || regno > LAST_VIRTUAL_REGISTER
	  || regno == FRAME_POINTER_REGNUM
	  || (GET_MODE_SIZE (mode) >= 4
	      && (regno == STACK_POINTER_REGNUM
		  || regno >= FIRST_PSEUDO_REGISTER
		  || x == hard_frame_pointer_rtx
		  || x == arg_pointer_rtx)));
}

/* Return nonzero if x is a legitimate index register.  This is the case
   for any base register that can access a QImode object.  */
inline static int
thumb1_index_register_rtx_p (rtx x, int strict_p)
{
  return thumb1_base_register_rtx_p (x, QImode, strict_p);
}

/* Return nonzero if x is a legitimate 16-bit Thumb-state address.

   The AP may be eliminated to either the SP or the FP, so we use the
   least common denominator, e.g. SImode, and offsets from 0 to 64.

   ??? Verify whether the above is the right approach.

   ??? Also, the FP may be eliminated to the SP, so perhaps that
   needs special handling also.

   ??? Look at how the mips16 port solves this problem.  It probably uses
   better ways to solve some of these problems.

   Although it is not incorrect, we don't accept QImode and HImode
   addresses based on the frame pointer or arg pointer until the
   reload pass starts.  This is so that eliminating such addresses
   into stack based ones won't produce impossible code.  */
int
thumb1_legitimate_address_p (machine_mode mode, rtx x, int strict_p)
{
  if (TARGET_HAVE_MOVT && can_avoid_literal_pool_for_label_p (x))
    return 0;

  /* ??? Not clear if this is right.  Experiment.  */
  if (GET_MODE_SIZE (mode) < 4
      && !(reload_in_progress || reload_completed)
      && (reg_mentioned_p (frame_pointer_rtx, x)
	  || reg_mentioned_p (arg_pointer_rtx, x)
	  || reg_mentioned_p (virtual_incoming_args_rtx, x)
	  || reg_mentioned_p (virtual_outgoing_args_rtx, x)
	  || reg_mentioned_p (virtual_stack_dynamic_rtx, x)
	  || reg_mentioned_p (virtual_stack_vars_rtx, x)))
    return 0;

  /* Accept any base register.  SP only in SImode or larger.  */
  else if (thumb1_base_register_rtx_p (x, mode, strict_p))
    return 1;

  /* This is PC relative data before arm_reorg runs.  */
  else if (GET_MODE_SIZE (mode) >= 4 && CONSTANT_P (x)
	   && GET_CODE (x) == SYMBOL_REF
           && CONSTANT_POOL_ADDRESS_P (x) && !flag_pic)
    return 1;

  /* This is PC relative data after arm_reorg runs.  */
  else if ((GET_MODE_SIZE (mode) >= 4 || mode == HFmode)
	   && reload_completed
	   && (GET_CODE (x) == LABEL_REF
	       || (GET_CODE (x) == CONST
		   && GET_CODE (XEXP (x, 0)) == PLUS
		   && GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF
		   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))))
    return 1;

  /* Post-inc indexing only supported for SImode and larger.  */
  else if (GET_CODE (x) == POST_INC && GET_MODE_SIZE (mode) >= 4
	   && thumb1_index_register_rtx_p (XEXP (x, 0), strict_p))
    return 1;

  else if (GET_CODE (x) == PLUS)
    {
      /* REG+REG address can be any two index registers.  */
      /* We disallow FRAME+REG addressing since we know that FRAME
	 will be replaced with STACK, and SP relative addressing only
	 permits SP+OFFSET.  */
      if (GET_MODE_SIZE (mode) <= 4
	  && XEXP (x, 0) != frame_pointer_rtx
	  && XEXP (x, 1) != frame_pointer_rtx
	  && thumb1_index_register_rtx_p (XEXP (x, 0), strict_p)
	  && (thumb1_index_register_rtx_p (XEXP (x, 1), strict_p)
	      || (!strict_p && will_be_in_index_register (XEXP (x, 1)))))
	return 1;

      /* REG+const has 5-7 bit offset for non-SP registers.  */
      else if ((thumb1_index_register_rtx_p (XEXP (x, 0), strict_p)
		|| XEXP (x, 0) == arg_pointer_rtx)
	       && CONST_INT_P (XEXP (x, 1))
	       && thumb_legitimate_offset_p (mode, INTVAL (XEXP (x, 1))))
	return 1;

      /* REG+const has 10-bit offset for SP, but only SImode and
	 larger is supported.  */
      /* ??? Should probably check for DI/DFmode overflow here
	 just like GO_IF_LEGITIMATE_OFFSET does.  */
      else if (REG_P (XEXP (x, 0))
	       && REGNO (XEXP (x, 0)) == STACK_POINTER_REGNUM
	       && GET_MODE_SIZE (mode) >= 4
	       && CONST_INT_P (XEXP (x, 1))
	       && INTVAL (XEXP (x, 1)) >= 0
	       && INTVAL (XEXP (x, 1)) + GET_MODE_SIZE (mode) <= 1024
	       && (INTVAL (XEXP (x, 1)) & 3) == 0)
	return 1;

      else if (REG_P (XEXP (x, 0))
	       && (REGNO (XEXP (x, 0)) == FRAME_POINTER_REGNUM
		   || REGNO (XEXP (x, 0)) == ARG_POINTER_REGNUM
		   || (REGNO (XEXP (x, 0)) >= FIRST_VIRTUAL_REGISTER
		       && REGNO (XEXP (x, 0))
			  <= LAST_VIRTUAL_POINTER_REGISTER))
	       && GET_MODE_SIZE (mode) >= 4
	       && CONST_INT_P (XEXP (x, 1))
	       && (INTVAL (XEXP (x, 1)) & 3) == 0)
	return 1;
    }

  else if (GET_MODE_CLASS (mode) != MODE_FLOAT
	   && GET_MODE_SIZE (mode) == 4
	   && GET_CODE (x) == SYMBOL_REF
	   && CONSTANT_POOL_ADDRESS_P (x)
	   && ! (flag_pic
		 && symbol_mentioned_p (get_pool_constant (x))
		 && ! pcrel_constant_p (get_pool_constant (x))))
    return 1;

  return 0;
}

/* Return nonzero if VAL can be used as an offset in a Thumb-state address
   instruction of mode MODE.  */
int
thumb_legitimate_offset_p (machine_mode mode, HOST_WIDE_INT val)
{
  switch (GET_MODE_SIZE (mode))
    {
    case 1:
      return val >= 0 && val < 32;

    case 2:
      return val >= 0 && val < 64 && (val & 1) == 0;

    default:
      return (val >= 0
	      && (val + GET_MODE_SIZE (mode)) <= 128
	      && (val & 3) == 0);
    }
}

bool
arm_legitimate_address_p (machine_mode mode, rtx x, bool strict_p)
{
  if (TARGET_ARM)
    return arm_legitimate_address_outer_p (mode, x, SET, strict_p);
  else if (TARGET_THUMB2)
    return thumb2_legitimate_address_p (mode, x, strict_p);
  else /* if (TARGET_THUMB1) */
    return thumb1_legitimate_address_p (mode, x, strict_p);
}

/* Worker function for TARGET_PREFERRED_RELOAD_CLASS.

   Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS, but for the Thumb core registers and
   immediate constants we prefer a LO_REGS class or a subset.  */

static reg_class_t
arm_preferred_reload_class (rtx x ATTRIBUTE_UNUSED, reg_class_t rclass)
{
  if (TARGET_32BIT)
    return rclass;
  else
    {
      if (rclass == GENERAL_REGS)
	return LO_REGS;
      else
	return rclass;
    }
}

/* Build the SYMBOL_REF for __tls_get_addr.  */

static GTY(()) rtx tls_get_addr_libfunc;

static rtx
get_tls_get_addr (void)
{
  if (!tls_get_addr_libfunc)
    tls_get_addr_libfunc = init_one_libfunc ("__tls_get_addr");
  return tls_get_addr_libfunc;
}

rtx
arm_load_tp (rtx target)
{
  if (!target)
    target = gen_reg_rtx (SImode);

  if (TARGET_HARD_TP)
    {
      /* Can return in any reg.  */
      emit_insn (gen_load_tp_hard (target));
    }
  else
    {
      /* Always returned in r0.  Immediately copy the result into a pseudo,
	 otherwise other uses of r0 (e.g. setting up function arguments) may
	 clobber the value.  */

      rtx tmp;

      emit_insn (gen_load_tp_soft ());

      tmp = gen_rtx_REG (SImode, R0_REGNUM);
      emit_move_insn (target, tmp);
    }
  return target;
}

static rtx
load_tls_operand (rtx x, rtx reg)
{
  rtx tmp;

  if (reg == NULL_RTX)
    reg = gen_reg_rtx (SImode);

  tmp = gen_rtx_CONST (SImode, x);

  emit_move_insn (reg, tmp);

  return reg;
}

static rtx_insn *
arm_call_tls_get_addr (rtx x, rtx reg, rtx *valuep, int reloc)
{
  rtx label, labelno, sum;

  gcc_assert (reloc != TLS_DESCSEQ);
  start_sequence ();

  labelno = GEN_INT (pic_labelno++);
  label = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_PIC_LABEL);
  label = gen_rtx_CONST (VOIDmode, label);

  sum = gen_rtx_UNSPEC (Pmode,
			gen_rtvec (4, x, GEN_INT (reloc), label,
				   GEN_INT (TARGET_ARM ? 8 : 4)),
			UNSPEC_TLS);
  reg = load_tls_operand (sum, reg);

  if (TARGET_ARM)
    emit_insn (gen_pic_add_dot_plus_eight (reg, reg, labelno));
  else
    emit_insn (gen_pic_add_dot_plus_four (reg, reg, labelno));

  *valuep = emit_library_call_value (get_tls_get_addr (), NULL_RTX,
				     LCT_PURE, /* LCT_CONST?  */
				     Pmode, reg, Pmode);

  rtx_insn *insns = get_insns ();
  end_sequence ();

  return insns;
}

static rtx
arm_tls_descseq_addr (rtx x, rtx reg)
{
  rtx labelno = GEN_INT (pic_labelno++);
  rtx label = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_PIC_LABEL);
  rtx sum = gen_rtx_UNSPEC (Pmode,
			    gen_rtvec (4, x, GEN_INT (TLS_DESCSEQ),
				       gen_rtx_CONST (VOIDmode, label),
				       GEN_INT (!TARGET_ARM)),
			    UNSPEC_TLS);
  rtx reg0 = load_tls_operand (sum, gen_rtx_REG (SImode, R0_REGNUM));

  emit_insn (gen_tlscall (x, labelno));
  if (!reg)
    reg = gen_reg_rtx (SImode);
  else
    gcc_assert (REGNO (reg) != R0_REGNUM);

  emit_move_insn (reg, reg0);

  return reg;
}

rtx
legitimize_tls_address (rtx x, rtx reg)
{
  rtx dest, tp, label, labelno, sum, ret, eqv, addend;
  rtx_insn *insns;
  unsigned int model = SYMBOL_REF_TLS_MODEL (x);

  switch (model)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
      if (TARGET_GNU2_TLS)
	{
	  reg = arm_tls_descseq_addr (x, reg);

	  tp = arm_load_tp (NULL_RTX);

	  dest = gen_rtx_PLUS (Pmode, tp, reg);
	}
      else
	{
	  /* Original scheme */
	  insns = arm_call_tls_get_addr (x, reg, &ret, TLS_GD32);
	  dest = gen_reg_rtx (Pmode);
	  emit_libcall_block (insns, dest, ret, x);
	}
      return dest;

    case TLS_MODEL_LOCAL_DYNAMIC:
      if (TARGET_GNU2_TLS)
	{
	  reg = arm_tls_descseq_addr (x, reg);

	  tp = arm_load_tp (NULL_RTX);

	  dest = gen_rtx_PLUS (Pmode, tp, reg);
	}
      else
	{
	  insns = arm_call_tls_get_addr (x, reg, &ret, TLS_LDM32);

	  /* Attach a unique REG_EQUIV, to allow the RTL optimizers to
	     share the LDM result with other LD model accesses.  */
	  eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const1_rtx),
				UNSPEC_TLS);
	  dest = gen_reg_rtx (Pmode);
	  emit_libcall_block (insns, dest, ret, eqv);

	  /* Load the addend.  */
	  addend = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, x,
						     GEN_INT (TLS_LDO32)),
				   UNSPEC_TLS);
	  addend = force_reg (SImode, gen_rtx_CONST (SImode, addend));
	  dest = gen_rtx_PLUS (Pmode, dest, addend);
	}
      return dest;

    case TLS_MODEL_INITIAL_EXEC:
      labelno = GEN_INT (pic_labelno++);
      label = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_PIC_LABEL);
      label = gen_rtx_CONST (VOIDmode, label);
      sum = gen_rtx_UNSPEC (Pmode,
			    gen_rtvec (4, x, GEN_INT (TLS_IE32), label,
				       GEN_INT (TARGET_ARM ? 8 : 4)),
			    UNSPEC_TLS);
      reg = load_tls_operand (sum, reg);

      if (TARGET_ARM)
	emit_insn (gen_tls_load_dot_plus_eight (reg, reg, labelno));
      else if (TARGET_THUMB2)
	emit_insn (gen_tls_load_dot_plus_four (reg, NULL, reg, labelno));
      else
	{
	  emit_insn (gen_pic_add_dot_plus_four (reg, reg, labelno));
	  emit_move_insn (reg, gen_const_mem (SImode, reg));
	}

      tp = arm_load_tp (NULL_RTX);

      return gen_rtx_PLUS (Pmode, tp, reg);

    case TLS_MODEL_LOCAL_EXEC:
      tp = arm_load_tp (NULL_RTX);

      reg = gen_rtx_UNSPEC (Pmode,
			    gen_rtvec (2, x, GEN_INT (TLS_LE32)),
			    UNSPEC_TLS);
      reg = force_reg (SImode, gen_rtx_CONST (SImode, reg));

      return gen_rtx_PLUS (Pmode, tp, reg);

    default:
      abort ();
    }
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.  */
rtx
arm_legitimize_address (rtx x, rtx orig_x, machine_mode mode)
{
  if (arm_tls_referenced_p (x))
    {
      rtx addend = NULL;

      if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  addend = XEXP (XEXP (x, 0), 1);
	  x = XEXP (XEXP (x, 0), 0);
	}

      if (GET_CODE (x) != SYMBOL_REF)
	return x;

      gcc_assert (SYMBOL_REF_TLS_MODEL (x) != 0);

      x = legitimize_tls_address (x, NULL_RTX);

      if (addend)
	{
	  x = gen_rtx_PLUS (SImode, x, addend);
	  orig_x = x;
	}
      else
	return x;
    }

  if (!TARGET_ARM)
    {
      /* TODO: legitimize_address for Thumb2.  */
      if (TARGET_THUMB2)
        return x;
      return thumb_legitimize_address (x, orig_x, mode);
    }

  if (GET_CODE (x) == PLUS)
    {
      rtx xop0 = XEXP (x, 0);
      rtx xop1 = XEXP (x, 1);

      if (CONSTANT_P (xop0) && !symbol_mentioned_p (xop0))
	xop0 = force_reg (SImode, xop0);

      if (CONSTANT_P (xop1) && !CONST_INT_P (xop1)
	  && !symbol_mentioned_p (xop1))
	xop1 = force_reg (SImode, xop1);

      if (ARM_BASE_REGISTER_RTX_P (xop0)
	  && CONST_INT_P (xop1))
	{
	  HOST_WIDE_INT n, low_n;
	  rtx base_reg, val;
	  n = INTVAL (xop1);

	  /* VFP addressing modes actually allow greater offsets, but for
	     now we just stick with the lowest common denominator.  */
	  if (mode == DImode || mode == DFmode)
	    {
	      low_n = n & 0x0f;
	      n &= ~0x0f;
	      if (low_n > 4)
		{
		  n += 16;
		  low_n -= 16;
		}
	    }
	  else
	    {
	      low_n = ((mode) == TImode ? 0
		       : n >= 0 ? (n & 0xfff) : -((-n) & 0xfff));
	      n -= low_n;
	    }

	  base_reg = gen_reg_rtx (SImode);
	  val = force_operand (plus_constant (Pmode, xop0, n), NULL_RTX);
	  emit_move_insn (base_reg, val);
	  x = plus_constant (Pmode, base_reg, low_n);
	}
      else if (xop0 != XEXP (x, 0) || xop1 != XEXP (x, 1))
	x = gen_rtx_PLUS (SImode, xop0, xop1);
    }

  /* XXX We don't allow MINUS any more -- see comment in
     arm_legitimate_address_outer_p ().  */
  else if (GET_CODE (x) == MINUS)
    {
      rtx xop0 = XEXP (x, 0);
      rtx xop1 = XEXP (x, 1);

      if (CONSTANT_P (xop0))
	xop0 = force_reg (SImode, xop0);

      if (CONSTANT_P (xop1) && ! symbol_mentioned_p (xop1))
	xop1 = force_reg (SImode, xop1);

      if (xop0 != XEXP (x, 0) || xop1 != XEXP (x, 1))
	x = gen_rtx_MINUS (SImode, xop0, xop1);
    }

  /* Make sure to take full advantage of the pre-indexed addressing mode
     with absolute addresses which often allows for the base register to
     be factorized for multiple adjacent memory references, and it might
     even allows for the mini pool to be avoided entirely. */
  else if (CONST_INT_P (x) && optimize > 0)
    {
      unsigned int bits;
      HOST_WIDE_INT mask, base, index;
      rtx base_reg;

      /* ldr and ldrb can use a 12-bit index, ldrsb and the rest can only
         use a 8-bit index. So let's use a 12-bit index for SImode only and
         hope that arm_gen_constant will enable ldrb to use more bits. */
      bits = (mode == SImode) ? 12 : 8;
      mask = (1 << bits) - 1;
      base = INTVAL (x) & ~mask;
      index = INTVAL (x) & mask;
      if (bit_count (base & 0xffffffff) > (32 - bits)/2)
        {
	  /* It'll most probably be more efficient to generate the base
	     with more bits set and use a negative index instead. */
	  base |= mask;
	  index -= mask;
	}
      base_reg = force_reg (SImode, GEN_INT (base));
      x = plus_constant (Pmode, base_reg, index);
    }

  if (flag_pic)
    {
      /* We need to find and carefully transform any SYMBOL and LABEL
	 references; so go back to the original address expression.  */
      rtx new_x = legitimize_pic_address (orig_x, mode, NULL_RTX);

      if (new_x != orig_x)
	x = new_x;
    }

  return x;
}


/* Try machine-dependent ways of modifying an illegitimate Thumb address
   to be legitimate.  If we find one, return the new, valid address.  */
rtx
thumb_legitimize_address (rtx x, rtx orig_x, machine_mode mode)
{
  if (GET_CODE (x) == PLUS
      && CONST_INT_P (XEXP (x, 1))
      && (INTVAL (XEXP (x, 1)) >= 32 * GET_MODE_SIZE (mode)
	  || INTVAL (XEXP (x, 1)) < 0))
    {
      rtx xop0 = XEXP (x, 0);
      rtx xop1 = XEXP (x, 1);
      HOST_WIDE_INT offset = INTVAL (xop1);

      /* Try and fold the offset into a biasing of the base register and
	 then offsetting that.  Don't do this when optimizing for space
	 since it can cause too many CSEs.  */
      if (optimize_size && offset >= 0
	  && offset < 256 + 31 * GET_MODE_SIZE (mode))
	{
	  HOST_WIDE_INT delta;

	  if (offset >= 256)
	    delta = offset - (256 - GET_MODE_SIZE (mode));
	  else if (offset < 32 * GET_MODE_SIZE (mode) + 8)
	    delta = 31 * GET_MODE_SIZE (mode);
	  else
	    delta = offset & (~31 * GET_MODE_SIZE (mode));

	  xop0 = force_operand (plus_constant (Pmode, xop0, offset - delta),
				NULL_RTX);
	  x = plus_constant (Pmode, xop0, delta);
	}
      else if (offset < 0 && offset > -256)
	/* Small negative offsets are best done with a subtract before the
	   dereference, forcing these into a register normally takes two
	   instructions.  */
	x = force_operand (x, NULL_RTX);
      else
	{
	  /* For the remaining cases, force the constant into a register.  */
	  xop1 = force_reg (SImode, xop1);
	  x = gen_rtx_PLUS (SImode, xop0, xop1);
	}
    }
  else if (GET_CODE (x) == PLUS
	   && s_register_operand (XEXP (x, 1), SImode)
	   && !s_register_operand (XEXP (x, 0), SImode))
    {
      rtx xop0 = force_operand (XEXP (x, 0), NULL_RTX);

      x = gen_rtx_PLUS (SImode, xop0, XEXP (x, 1));
    }

  if (flag_pic)
    {
      /* We need to find and carefully transform any SYMBOL and LABEL
	 references; so go back to the original address expression.  */
      rtx new_x = legitimize_pic_address (orig_x, mode, NULL_RTX);

      if (new_x != orig_x)
	x = new_x;
    }

  return x;
}

/* Return TRUE if X contains any TLS symbol references.  */

bool
arm_tls_referenced_p (rtx x)
{
  if (! TARGET_HAVE_TLS)
    return false;

  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    {
      const_rtx x = *iter;
      if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x) != 0)
	{
	  /* ARM currently does not provide relocations to encode TLS variables
	     into AArch32 instructions, only data, so there is no way to
	     currently implement these if a literal pool is disabled.  */
	  if (arm_disable_literal_pool)
	    sorry ("accessing thread-local storage is not currently supported "
		   "with -mpure-code or -mslow-flash-data");

	  return true;
	}

      /* Don't recurse into UNSPEC_TLS looking for TLS symbols; these are
	 TLS offsets, not real symbol references.  */
      if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
	iter.skip_subrtxes ();
    }
  return false;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.

   On the ARM, allow any integer (invalid ones are removed later by insn
   patterns), nice doubles and symbol_refs which refer to the function's
   constant pool XXX.

   When generating pic allow anything.  */

static bool
arm_legitimate_constant_p_1 (machine_mode, rtx x)
{
  return flag_pic || !label_mentioned_p (x);
}

static bool
thumb_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  /* Splitters for TARGET_USE_MOVT call arm_emit_movpair which creates high
     RTX.  These RTX must therefore be allowed for Thumb-1 so that when run
     for ARMv8-M Baseline or later the result is valid.  */
  if (TARGET_HAVE_MOVT && GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  return (CONST_INT_P (x)
	  || CONST_DOUBLE_P (x)
	  || CONSTANT_ADDRESS_P (x)
	  || (TARGET_HAVE_MOVT && GET_CODE (x) == SYMBOL_REF)
	  || flag_pic);
}

static bool
arm_legitimate_constant_p (machine_mode mode, rtx x)
{
  return (!arm_cannot_force_const_mem (mode, x)
	  && (TARGET_32BIT
	      ? arm_legitimate_constant_p_1 (mode, x)
	      : thumb_legitimate_constant_p (mode, x)));
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */

static bool
arm_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  rtx base, offset;

  if (ARM_OFFSETS_MUST_BE_WITHIN_SECTIONS_P)
    {
      split_const (x, &base, &offset);
      if (GET_CODE (base) == SYMBOL_REF
	  && !offset_within_block_p (base, INTVAL (offset)))
	return true;
    }
  return arm_tls_referenced_p (x);
}

#define REG_OR_SUBREG_REG(X)						\
  (REG_P (X)							\
   || (GET_CODE (X) == SUBREG && REG_P (SUBREG_REG (X))))

#define REG_OR_SUBREG_RTX(X)			\
   (REG_P (X) ? (X) : SUBREG_REG (X))

static inline int
thumb1_rtx_costs (rtx x, enum rtx_code code, enum rtx_code outer)
{
  machine_mode mode = GET_MODE (x);
  int total, words;

  switch (code)
    {
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATERT:
      return (mode == SImode) ? COSTS_N_INSNS (1) : COSTS_N_INSNS (2);

    case PLUS:
    case MINUS:
    case COMPARE:
    case NEG:
    case NOT:
      return COSTS_N_INSNS (1);

    case MULT:
      if (arm_arch6m && arm_m_profile_small_mul)
	return COSTS_N_INSNS (32);

      if (CONST_INT_P (XEXP (x, 1)))
	{
	  int cycles = 0;
	  unsigned HOST_WIDE_INT i = INTVAL (XEXP (x, 1));

	  while (i)
	    {
	      i >>= 2;
	      cycles++;
	    }
	  return COSTS_N_INSNS (2) + cycles;
	}
      return COSTS_N_INSNS (1) + 16;

    case SET:
      /* A SET doesn't have a mode, so let's look at the SET_DEST to get
	 the mode.  */
      words = ARM_NUM_INTS (GET_MODE_SIZE (GET_MODE (SET_DEST (x))));
      return (COSTS_N_INSNS (words)
	      + 4 * ((MEM_P (SET_SRC (x)))
		     + MEM_P (SET_DEST (x))));

    case CONST_INT:
      if (outer == SET)
	{
	  if (UINTVAL (x) < 256
	      /* 16-bit constant.  */
	      || (TARGET_HAVE_MOVT && !(INTVAL (x) & 0xffff0000)))
	    return 0;
	  if (thumb_shiftable_const (INTVAL (x)))
	    return COSTS_N_INSNS (2);
	  return COSTS_N_INSNS (3);
	}
      else if ((outer == PLUS || outer == COMPARE)
	       && INTVAL (x) < 256 && INTVAL (x) > -256)
	return 0;
      else if ((outer == IOR || outer == XOR || outer == AND)
	       && INTVAL (x) < 256 && INTVAL (x) >= -256)
	return COSTS_N_INSNS (1);
      else if (outer == AND)
	{
	  int i;
	  /* This duplicates the tests in the andsi3 expander.  */
	  for (i = 9; i <= 31; i++)
	    if ((HOST_WIDE_INT_1 << i) - 1 == INTVAL (x)
		|| (HOST_WIDE_INT_1 << i) - 1 == ~INTVAL (x))
	      return COSTS_N_INSNS (2);
	}
      else if (outer == ASHIFT || outer == ASHIFTRT
	       || outer == LSHIFTRT)
	return 0;
      return COSTS_N_INSNS (2);

    case CONST:
    case CONST_DOUBLE:
    case LABEL_REF:
    case SYMBOL_REF:
      return COSTS_N_INSNS (3);

    case UDIV:
    case UMOD:
    case DIV:
    case MOD:
      return 100;

    case TRUNCATE:
      return 99;

    case AND:
    case XOR:
    case IOR:
      /* XXX guess.  */
      return 8;

    case MEM:
      /* XXX another guess.  */
      /* Memory costs quite a lot for the first word, but subsequent words
	 load at the equivalent of a single insn each.  */
      return (10 + 4 * ((GET_MODE_SIZE (mode) - 1) / UNITS_PER_WORD)
	      + ((GET_CODE (x) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (x))
		 ? 4 : 0));

    case IF_THEN_ELSE:
      /* XXX a guess.  */
      if (GET_CODE (XEXP (x, 1)) == PC || GET_CODE (XEXP (x, 2)) == PC)
	return 14;
      return 2;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      total = mode == DImode ? COSTS_N_INSNS (1) : 0;
      total += thumb1_rtx_costs (XEXP (x, 0), GET_CODE (XEXP (x, 0)), code);

      if (mode == SImode)
	return total;

      if (arm_arch6)
	return total + COSTS_N_INSNS (1);

      /* Assume a two-shift sequence.  Increase the cost slightly so
	 we prefer actual shifts over an extend operation.  */
      return total + 1 + COSTS_N_INSNS (2);

    default:
      return 99;
    }
}

/* Estimates the size cost of thumb1 instructions.
   For now most of the code is copied from thumb1_rtx_costs. We need more
   fine grain tuning when we have more related test cases.  */
static inline int
thumb1_size_rtx_costs (rtx x, enum rtx_code code, enum rtx_code outer)
{
  machine_mode mode = GET_MODE (x);
  int words, cost;

  switch (code)
    {
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATERT:
      return (mode == SImode) ? COSTS_N_INSNS (1) : COSTS_N_INSNS (2);

    case PLUS:
    case MINUS:
      /* Thumb-1 needs two instructions to fulfill shiftadd/shiftsub0/shiftsub1
	 defined by RTL expansion, especially for the expansion of
	 multiplication.  */
      if ((GET_CODE (XEXP (x, 0)) == MULT
	   && power_of_two_operand (XEXP (XEXP (x,0),1), SImode))
	  || (GET_CODE (XEXP (x, 1)) == MULT
	      && power_of_two_operand (XEXP (XEXP (x, 1), 1), SImode)))
	return COSTS_N_INSNS (2);
      /* Fall through.  */
    case COMPARE:
    case NEG:
    case NOT:
      return COSTS_N_INSNS (1);

    case MULT:
      if (CONST_INT_P (XEXP (x, 1)))
        {
          /* Thumb1 mul instruction can't operate on const. We must Load it
             into a register first.  */
          int const_size = thumb1_size_rtx_costs (XEXP (x, 1), CONST_INT, SET);
	  /* For the targets which have a very small and high-latency multiply
	     unit, we prefer to synthesize the mult with up to 5 instructions,
	     giving a good balance between size and performance.  */
	  if (arm_arch6m && arm_m_profile_small_mul)
	    return COSTS_N_INSNS (5);
	  else
	    return COSTS_N_INSNS (1) + const_size;
        }
      return COSTS_N_INSNS (1);

    case SET:
      /* A SET doesn't have a mode, so let's look at the SET_DEST to get
	 the mode.  */
      words = ARM_NUM_INTS (GET_MODE_SIZE (GET_MODE (SET_DEST (x))));
      cost = COSTS_N_INSNS (words);
      if (satisfies_constraint_J (SET_SRC (x))
	  || satisfies_constraint_K (SET_SRC (x))
	     /* Too big an immediate for a 2-byte mov, using MOVT.  */
	  || (CONST_INT_P (SET_SRC (x))
	      && UINTVAL (SET_SRC (x)) >= 256
	      && TARGET_HAVE_MOVT
	      && satisfies_constraint_j (SET_SRC (x)))
	     /* thumb1_movdi_insn.  */
	  || ((words > 1) && MEM_P (SET_SRC (x))))
	cost += COSTS_N_INSNS (1);
      return cost;

    case CONST_INT:
      if (outer == SET)
        {
          if (UINTVAL (x) < 256)
            return COSTS_N_INSNS (1);
	  /* movw is 4byte long.  */
	  if (TARGET_HAVE_MOVT && !(INTVAL (x) & 0xffff0000))
	    return COSTS_N_INSNS (2);
	  /* See split "TARGET_THUMB1 && satisfies_constraint_J".  */
	  if (INTVAL (x) >= -255 && INTVAL (x) <= -1)
            return COSTS_N_INSNS (2);
	  /* See split "TARGET_THUMB1 && satisfies_constraint_K".  */
          if (thumb_shiftable_const (INTVAL (x)))
            return COSTS_N_INSNS (2);
          return COSTS_N_INSNS (3);
        }
      else if ((outer == PLUS || outer == COMPARE)
               && INTVAL (x) < 256 && INTVAL (x) > -256)
        return 0;
      else if ((outer == IOR || outer == XOR || outer == AND)
               && INTVAL (x) < 256 && INTVAL (x) >= -256)
        return COSTS_N_INSNS (1);
      else if (outer == AND)
        {
          int i;
          /* This duplicates the tests in the andsi3 expander.  */
          for (i = 9; i <= 31; i++)
            if ((HOST_WIDE_INT_1 << i) - 1 == INTVAL (x)
                || (HOST_WIDE_INT_1 << i) - 1 == ~INTVAL (x))
              return COSTS_N_INSNS (2);
        }
      else if (outer == ASHIFT || outer == ASHIFTRT
               || outer == LSHIFTRT)
        return 0;
      return COSTS_N_INSNS (2);

    case CONST:
    case CONST_DOUBLE:
    case LABEL_REF:
    case SYMBOL_REF:
      return COSTS_N_INSNS (3);

    case UDIV:
    case UMOD:
    case DIV:
    case MOD:
      return 100;

    case TRUNCATE:
      return 99;

    case AND:
    case XOR:
    case IOR:
      return COSTS_N_INSNS (1);

    case MEM:
      return (COSTS_N_INSNS (1)
	      + COSTS_N_INSNS (1)
		* ((GET_MODE_SIZE (mode) - 1) / UNITS_PER_WORD)
              + ((GET_CODE (x) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (x))
                 ? COSTS_N_INSNS (1) : 0));

    case IF_THEN_ELSE:
      /* XXX a guess.  */
      if (GET_CODE (XEXP (x, 1)) == PC || GET_CODE (XEXP (x, 2)) == PC)
        return 14;
      return 2;

    case ZERO_EXTEND:
      /* XXX still guessing.  */
      switch (GET_MODE (XEXP (x, 0)))
        {
          case E_QImode:
            return (1 + (mode == DImode ? 4 : 0)
                    + (MEM_P (XEXP (x, 0)) ? 10 : 0));

          case E_HImode:
            return (4 + (mode == DImode ? 4 : 0)
                    + (MEM_P (XEXP (x, 0)) ? 10 : 0));

          case E_SImode:
            return (1 + (MEM_P (XEXP (x, 0)) ? 10 : 0));

          default:
            return 99;
        }

    default:
      return 99;
    }
}

/* Helper function for arm_rtx_costs.  If the operand is a valid shift
   operand, then return the operand that is being shifted.  If the shift
   is not by a constant, then set SHIFT_REG to point to the operand.
   Return NULL if OP is not a shifter operand.  */
static rtx
shifter_op_p (rtx op, rtx *shift_reg)
{
  enum rtx_code code = GET_CODE (op);

  if (code == MULT && CONST_INT_P (XEXP (op, 1))
      && exact_log2 (INTVAL (XEXP (op, 1))) > 0)
    return XEXP (op, 0);
  else if (code == ROTATE && CONST_INT_P (XEXP (op, 1)))
    return XEXP (op, 0);
  else if (code == ROTATERT || code == ASHIFT || code == LSHIFTRT
	   || code == ASHIFTRT)
    {
      if (!CONST_INT_P (XEXP (op, 1)))
	*shift_reg = XEXP (op, 1);
      return XEXP (op, 0);
    }

  return NULL;
}

static bool
arm_unspec_cost (rtx x, enum rtx_code /* outer_code */, bool speed_p, int *cost)
{
  const struct cpu_cost_table *extra_cost = current_tune->insn_extra_cost;
  rtx_code code = GET_CODE (x);
  gcc_assert (code == UNSPEC || code == UNSPEC_VOLATILE);

  switch (XINT (x, 1))
    {
    case UNSPEC_UNALIGNED_LOAD:
      /* We can only do unaligned loads into the integer unit, and we can't
	 use LDM or LDRD.  */
      *cost = COSTS_N_INSNS (ARM_NUM_REGS (GET_MODE (x)));
      if (speed_p)
	*cost += (ARM_NUM_REGS (GET_MODE (x)) * extra_cost->ldst.load
		  + extra_cost->ldst.load_unaligned);

#ifdef NOT_YET
      *cost += arm_address_cost (XEXP (XVECEXP (x, 0, 0), 0), GET_MODE (x),
				 ADDR_SPACE_GENERIC, speed_p);
#endif
      return true;

    case UNSPEC_UNALIGNED_STORE:
      *cost = COSTS_N_INSNS (ARM_NUM_REGS (GET_MODE (x)));
      if (speed_p)
	*cost += (ARM_NUM_REGS (GET_MODE (x)) * extra_cost->ldst.store
		  + extra_cost->ldst.store_unaligned);

      *cost += rtx_cost (XVECEXP (x, 0, 0), VOIDmode, UNSPEC, 0, speed_p);
#ifdef NOT_YET
      *cost += arm_address_cost (XEXP (XVECEXP (x, 0, 0), 0), GET_MODE (x),
				 ADDR_SPACE_GENERIC, speed_p);
#endif
      return true;

    case UNSPEC_VRINTZ:
    case UNSPEC_VRINTP:
    case UNSPEC_VRINTM:
    case UNSPEC_VRINTR:
    case UNSPEC_VRINTX:
    case UNSPEC_VRINTA:
      if (speed_p)
        *cost += extra_cost->fp[GET_MODE (x) == DFmode].roundint;

      return true;
    default:
      *cost = COSTS_N_INSNS (2);
      break;
    }
  return true;
}

/* Cost of a libcall.  We assume one insn per argument, an amount for the
   call (one insn for -Os) and then one for processing the result.  */
#define LIBCALL_COST(N) COSTS_N_INSNS (N + (speed_p ? 18 : 2))

#define HANDLE_NARROW_SHIFT_ARITH(OP, IDX)				\
	do								\
	  {								\
	    shift_op = shifter_op_p (XEXP (x, IDX), &shift_reg);	\
	    if (shift_op != NULL					\
	        && arm_rtx_shift_left_p (XEXP (x, IDX)))		\
	      {								\
	        if (shift_reg)						\
		  {							\
		    if (speed_p)					\
		      *cost += extra_cost->alu.arith_shift_reg;		\
		    *cost += rtx_cost (shift_reg, GET_MODE (shift_reg),	\
				       ASHIFT, 1, speed_p);		\
		  }							\
	        else if (speed_p)					\
		  *cost += extra_cost->alu.arith_shift;			\
									\
		*cost += (rtx_cost (shift_op, GET_MODE (shift_op),	\
				    ASHIFT, 0, speed_p)			\
			  + rtx_cost (XEXP (x, 1 - IDX),		\
				      GET_MODE (shift_op),		\
			              OP, 1, speed_p));			\
	        return true;						\
	      }								\
	  }								\
	while (0)

/* Helper function for arm_rtx_costs_internal.  Calculates the cost of a MEM,
   considering the costs of the addressing mode and memory access
   separately.  */
static bool
arm_mem_costs (rtx x, const struct cpu_cost_table *extra_cost,
	       int *cost, bool speed_p)
{
  machine_mode mode = GET_MODE (x);

  *cost = COSTS_N_INSNS (1);

  if (flag_pic
      && GET_CODE (XEXP (x, 0)) == PLUS
      && will_be_in_index_register (XEXP (XEXP (x, 0), 1)))
    /* This will be split into two instructions.  Add the cost of the
       additional instruction here.  The cost of the memory access is computed
       below.  See arm.md:calculate_pic_address.  */
    *cost += COSTS_N_INSNS (1);

  /* Calculate cost of the addressing mode.  */
  if (speed_p)
    {
      arm_addr_mode_op op_type;
      switch (GET_CODE (XEXP (x, 0)))
	{
	default:
	case REG:
	  op_type = AMO_DEFAULT;
	  break;
	case MINUS:
	  /* MINUS does not appear in RTL, but the architecture supports it,
	     so handle this case defensively.  */
	  /* fall through */
	case PLUS:
	  op_type = AMO_NO_WB;
	  break;
	case PRE_INC:
	case PRE_DEC:
	case POST_INC:
	case POST_DEC:
	case PRE_MODIFY:
	case POST_MODIFY:
	  op_type = AMO_WB;
	  break;
	}

      if (VECTOR_MODE_P (mode))
	  *cost += current_tune->addr_mode_costs->vector[op_type];
      else if (FLOAT_MODE_P (mode))
	  *cost += current_tune->addr_mode_costs->fp[op_type];
      else
	  *cost += current_tune->addr_mode_costs->integer[op_type];
    }

  /* Calculate cost of memory access.  */
  if (speed_p)
    {
      if (FLOAT_MODE_P (mode))
	{
	  if (GET_MODE_SIZE (mode) == 8)
	    *cost += extra_cost->ldst.loadd;
	  else
	    *cost += extra_cost->ldst.loadf;
	}
      else if (VECTOR_MODE_P (mode))
	*cost += extra_cost->ldst.loadv;
      else
	{
	  /* Integer modes */
	  if (GET_MODE_SIZE (mode) == 8)
	    *cost += extra_cost->ldst.ldrd;
	  else
	    *cost += extra_cost->ldst.load;
	}
    }

  return true;
}

/* RTX costs.  Make an estimate of the cost of executing the operation
   X, which is contained within an operation with code OUTER_CODE.
   SPEED_P indicates whether the cost desired is the performance cost,
   or the size cost.  The estimate is stored in COST and the return
   value is TRUE if the cost calculation is final, or FALSE if the
   caller should recurse through the operands of X to add additional
   costs.

   We currently make no attempt to model the size savings of Thumb-2
   16-bit instructions.  At the normal points in compilation where
   this code is called we have no measure of whether the condition
   flags are live or not, and thus no realistic way to determine what
   the size will eventually be.  */
static bool
arm_rtx_costs_internal (rtx x, enum rtx_code code, enum rtx_code outer_code,
		   const struct cpu_cost_table *extra_cost,
		   int *cost, bool speed_p)
{
  machine_mode mode = GET_MODE (x);

  *cost = COSTS_N_INSNS (1);

  if (TARGET_THUMB1)
    {
      if (speed_p)
	*cost = thumb1_rtx_costs (x, code, outer_code);
      else
	*cost = thumb1_size_rtx_costs (x, code, outer_code);
      return true;
    }

  switch (code)
    {
    case SET:
      *cost = 0;
      /* SET RTXs don't have a mode so we get it from the destination.  */
      mode = GET_MODE (SET_DEST (x));

      if (REG_P (SET_SRC (x))
	  && REG_P (SET_DEST (x)))
	{
	  /* Assume that most copies can be done with a single insn,
	     unless we don't have HW FP, in which case everything
	     larger than word mode will require two insns.  */
	  *cost = COSTS_N_INSNS (((!TARGET_HARD_FLOAT
				   && GET_MODE_SIZE (mode) > 4)
				  || mode == DImode)
				 ? 2 : 1);
	  /* Conditional register moves can be encoded
	     in 16 bits in Thumb mode.  */
	  if (!speed_p && TARGET_THUMB && outer_code == COND_EXEC)
	    *cost >>= 1;

	  return true;
	}

      if (CONST_INT_P (SET_SRC (x)))
	{
	  /* Handle CONST_INT here, since the value doesn't have a mode
	     and we would otherwise be unable to work out the true cost.  */
	  *cost = rtx_cost (SET_DEST (x), GET_MODE (SET_DEST (x)), SET,
			    0, speed_p);
	  outer_code = SET;
	  /* Slightly lower the cost of setting a core reg to a constant.
	     This helps break up chains and allows for better scheduling.  */
	  if (REG_P (SET_DEST (x))
	      && REGNO (SET_DEST (x)) <= LR_REGNUM)
	    *cost -= 1;
	  x = SET_SRC (x);
	  /* Immediate moves with an immediate in the range [0, 255] can be
	     encoded in 16 bits in Thumb mode.  */
	  if (!speed_p && TARGET_THUMB && GET_MODE (x) == SImode
	      && INTVAL (x) >= 0 && INTVAL (x) <=255)
	    *cost >>= 1;
	  goto const_int_cost;
	}

      return false;

    case MEM:
      return arm_mem_costs (x, extra_cost, cost, speed_p);

    case PARALLEL:
    {
   /* Calculations of LDM costs are complex.  We assume an initial cost
   (ldm_1st) which will load the number of registers mentioned in
   ldm_regs_per_insn_1st registers; then each additional
   ldm_regs_per_insn_subsequent registers cost one more insn.  The
   formula for N regs is thus:

   ldm_1st + COSTS_N_INSNS ((max (N - ldm_regs_per_insn_1st, 0)
			     + ldm_regs_per_insn_subsequent - 1)
			    / ldm_regs_per_insn_subsequent).

   Additional costs may also be added for addressing.  A similar
   formula is used for STM.  */

      bool is_ldm = load_multiple_operation (x, SImode);
      bool is_stm = store_multiple_operation (x, SImode);

      if (is_ldm || is_stm)
        {
	  if (speed_p)
	    {
	      HOST_WIDE_INT nregs = XVECLEN (x, 0);
	      HOST_WIDE_INT regs_per_insn_1st = is_ldm
	                              ? extra_cost->ldst.ldm_regs_per_insn_1st
	                              : extra_cost->ldst.stm_regs_per_insn_1st;
	      HOST_WIDE_INT regs_per_insn_sub = is_ldm
	                       ? extra_cost->ldst.ldm_regs_per_insn_subsequent
	                       : extra_cost->ldst.stm_regs_per_insn_subsequent;

	      *cost += regs_per_insn_1st
	               + COSTS_N_INSNS (((MAX (nregs - regs_per_insn_1st, 0))
					    + regs_per_insn_sub - 1)
					  / regs_per_insn_sub);
	      return true;
	    }

        }
      return false;
    }
    case DIV:
    case UDIV:
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT
	  && (mode == SFmode || !TARGET_VFP_SINGLE))
	*cost += COSTS_N_INSNS (speed_p
			       ? extra_cost->fp[mode != SFmode].div : 0);
      else if (mode == SImode && TARGET_IDIV)
	*cost += COSTS_N_INSNS (speed_p ? extra_cost->mult[0].idiv : 0);
      else
	*cost = LIBCALL_COST (2);

      /* Make the cost of sdiv more expensive so when both sdiv and udiv are
	 possible udiv is prefered.  */
      *cost += (code == DIV ? COSTS_N_INSNS (1) : 0);
      return false;	/* All arguments must be in registers.  */

    case MOD:
      /* MOD by a power of 2 can be expanded as:
	 rsbs    r1, r0, #0
	 and     r0, r0, #(n - 1)
	 and     r1, r1, #(n - 1)
	 rsbpl   r0, r1, #0.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && exact_log2 (INTVAL (XEXP (x, 1))) > 0
	  && mode == SImode)
	{
	  *cost += COSTS_N_INSNS (3);

	  if (speed_p)
	    *cost += 2 * extra_cost->alu.logical
		     + extra_cost->alu.arith;
	  return true;
	}

    /* Fall-through.  */
    case UMOD:
      /* Make the cost of sdiv more expensive so when both sdiv and udiv are
	 possible udiv is prefered.  */
      *cost = LIBCALL_COST (2) + (code == MOD ? COSTS_N_INSNS (1) : 0);
      return false;	/* All arguments must be in registers.  */

    case ROTATE:
      if (mode == SImode && REG_P (XEXP (x, 1)))
	{
	  *cost += (COSTS_N_INSNS (1)
		   + rtx_cost (XEXP (x, 0), mode, code, 0, speed_p));
	  if (speed_p)
	    *cost += extra_cost->alu.shift_reg;
	  return true;
	}
      /* Fall through */
    case ROTATERT:
    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
      if (mode == DImode && CONST_INT_P (XEXP (x, 1)))
	{
	  *cost += (COSTS_N_INSNS (2)
		   + rtx_cost (XEXP (x, 0), mode, code, 0, speed_p));
	  if (speed_p)
	    *cost += 2 * extra_cost->alu.shift;
	  /* Slightly disparage left shift by 1 at so we prefer adddi3.  */
	  if (code == ASHIFT && XEXP (x, 1) == CONST1_RTX (SImode))
	    *cost += 1;
	  return true;
	}
      else if (mode == SImode)
	{
	  *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
	  /* Slightly disparage register shifts at -Os, but not by much.  */
	  if (!CONST_INT_P (XEXP (x, 1)))
	    *cost += (speed_p ? extra_cost->alu.shift_reg : 1
		      + rtx_cost (XEXP (x, 1), mode, code, 1, speed_p));
	  return true;
	}
      else if (GET_MODE_CLASS (mode) == MODE_INT
	       && GET_MODE_SIZE (mode) < 4)
	{
	  if (code == ASHIFT)
	    {
	      *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
	      /* Slightly disparage register shifts at -Os, but not by
	         much.  */
	      if (!CONST_INT_P (XEXP (x, 1)))
		*cost += (speed_p ? extra_cost->alu.shift_reg : 1
			  + rtx_cost (XEXP (x, 1), mode, code, 1, speed_p));
	    }
	  else if (code == LSHIFTRT || code == ASHIFTRT)
	    {
	      if (arm_arch_thumb2 && CONST_INT_P (XEXP (x, 1)))
		{
		  /* Can use SBFX/UBFX.  */
		  if (speed_p)
		    *cost += extra_cost->alu.bfx;
		  *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
		}
	      else
		{
		  *cost += COSTS_N_INSNS (1);
		  *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
		  if (speed_p)
		    {
		      if (CONST_INT_P (XEXP (x, 1)))
			*cost += 2 * extra_cost->alu.shift;
		      else
			*cost += (extra_cost->alu.shift
				  + extra_cost->alu.shift_reg);
		    }
		  else
		    /* Slightly disparage register shifts.  */
		    *cost += !CONST_INT_P (XEXP (x, 1));
		}
	    }
	  else /* Rotates.  */
	    {
	      *cost = COSTS_N_INSNS (2 + !CONST_INT_P (XEXP (x, 1)));
	      *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
	      if (speed_p)
		{
		  if (CONST_INT_P (XEXP (x, 1)))
		    *cost += (2 * extra_cost->alu.shift
			      + extra_cost->alu.log_shift);
		  else
		    *cost += (extra_cost->alu.shift
			      + extra_cost->alu.shift_reg
			      + extra_cost->alu.log_shift_reg);
		}
	    }
	  return true;
	}

      *cost = LIBCALL_COST (2);
      return false;

    case BSWAP:
      if (arm_arch6)
        {
          if (mode == SImode)
            {
              if (speed_p)
                *cost += extra_cost->alu.rev;

              return false;
            }
        }
      else
        {
        /* No rev instruction available.  Look at arm_legacy_rev
           and thumb_legacy_rev for the form of RTL used then.  */
          if (TARGET_THUMB)
            {
              *cost += COSTS_N_INSNS (9);

              if (speed_p)
                {
                  *cost += 6 * extra_cost->alu.shift;
                  *cost += 3 * extra_cost->alu.logical;
                }
            }
          else
            {
              *cost += COSTS_N_INSNS (4);

              if (speed_p)
                {
                  *cost += 2 * extra_cost->alu.shift;
                  *cost += extra_cost->alu.arith_shift;
                  *cost += 2 * extra_cost->alu.logical;
                }
            }
          return true;
        }
      return false;

    case MINUS:
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT
	  && (mode == SFmode || !TARGET_VFP_SINGLE))
	{
	  if (GET_CODE (XEXP (x, 0)) == MULT
	      || GET_CODE (XEXP (x, 1)) == MULT)
	    {
	      rtx mul_op0, mul_op1, sub_op;

	      if (speed_p)
		*cost += extra_cost->fp[mode != SFmode].mult_addsub;

	      if (GET_CODE (XEXP (x, 0)) == MULT)
		{
		  mul_op0 = XEXP (XEXP (x, 0), 0);
		  mul_op1 = XEXP (XEXP (x, 0), 1);
		  sub_op = XEXP (x, 1);
		}
	      else
		{
		  mul_op0 = XEXP (XEXP (x, 1), 0);
		  mul_op1 = XEXP (XEXP (x, 1), 1);
		  sub_op = XEXP (x, 0);
		}

	      /* The first operand of the multiply may be optionally
		 negated.  */
	      if (GET_CODE (mul_op0) == NEG)
		mul_op0 = XEXP (mul_op0, 0);

	      *cost += (rtx_cost (mul_op0, mode, code, 0, speed_p)
			+ rtx_cost (mul_op1, mode, code, 0, speed_p)
			+ rtx_cost (sub_op, mode, code, 0, speed_p));

	      return true;
	    }

	  if (speed_p)
	    *cost += extra_cost->fp[mode != SFmode].addsub;
	  return false;
	}

      if (mode == SImode)
	{
	  rtx shift_by_reg = NULL;
	  rtx shift_op;
	  rtx non_shift_op;

	  shift_op = shifter_op_p (XEXP (x, 0), &shift_by_reg);
	  if (shift_op == NULL)
	    {
	      shift_op = shifter_op_p (XEXP (x, 1), &shift_by_reg);
	      non_shift_op = XEXP (x, 0);
	    }
	  else
	    non_shift_op = XEXP (x, 1);

	  if (shift_op != NULL)
	    {
	      if (shift_by_reg != NULL)
		{
		  if (speed_p)
		    *cost += extra_cost->alu.arith_shift_reg;
		  *cost += rtx_cost (shift_by_reg, mode, code, 0, speed_p);
		}
	      else if (speed_p)
		*cost += extra_cost->alu.arith_shift;

	      *cost += rtx_cost (shift_op, mode, code, 0, speed_p);
	      *cost += rtx_cost (non_shift_op, mode, code, 0, speed_p);
	      return true;
	    }

	  if (arm_arch_thumb2
	      && GET_CODE (XEXP (x, 1)) == MULT)
	    {
	      /* MLS.  */
	      if (speed_p)
		*cost += extra_cost->mult[0].add;
	      *cost += rtx_cost (XEXP (x, 0), mode, MINUS, 0, speed_p);
	      *cost += rtx_cost (XEXP (XEXP (x, 1), 0), mode, MULT, 0, speed_p);
	      *cost += rtx_cost (XEXP (XEXP (x, 1), 1), mode, MULT, 1, speed_p);
	      return true;
	    }

	  if (CONST_INT_P (XEXP (x, 0)))
	    {
	      int insns = arm_gen_constant (MINUS, SImode, NULL_RTX,
					    INTVAL (XEXP (x, 0)), NULL_RTX,
					    NULL_RTX, 1, 0);
	      *cost = COSTS_N_INSNS (insns);
	      if (speed_p)
		*cost += insns * extra_cost->alu.arith;
	      *cost += rtx_cost (XEXP (x, 1), mode, code, 1, speed_p);
	      return true;
	    }
	  else if (speed_p)
	    *cost += extra_cost->alu.arith;

	  return false;
	}

      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_SIZE (mode) < 4)
	{
	  rtx shift_op, shift_reg;
	  shift_reg = NULL;

	  /* We check both sides of the MINUS for shifter operands since,
	     unlike PLUS, it's not commutative.  */

	  HANDLE_NARROW_SHIFT_ARITH (MINUS, 0);
	  HANDLE_NARROW_SHIFT_ARITH (MINUS, 1);

	  /* Slightly disparage, as we might need to widen the result.  */
	  *cost += 1;
	  if (speed_p)
	    *cost += extra_cost->alu.arith;

	  if (CONST_INT_P (XEXP (x, 0)))
	    {
	      *cost += rtx_cost (XEXP (x, 1), mode, code, 1, speed_p);
	      return true;
	    }

	  return false;
	}

      if (mode == DImode)
	{
	  *cost += COSTS_N_INSNS (1);

	  if (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND)
	    {
	      rtx op1 = XEXP (x, 1);

	      if (speed_p)
		*cost += 2 * extra_cost->alu.arith;

	      if (GET_CODE (op1) == ZERO_EXTEND)
		*cost += rtx_cost (XEXP (op1, 0), VOIDmode, ZERO_EXTEND,
				   0, speed_p);
	      else
		*cost += rtx_cost (op1, mode, MINUS, 1, speed_p);
	      *cost += rtx_cost (XEXP (XEXP (x, 0), 0), VOIDmode, ZERO_EXTEND,
				 0, speed_p);
	      return true;
	    }
	  else if (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	    {
	      if (speed_p)
		*cost += extra_cost->alu.arith + extra_cost->alu.arith_shift;
	      *cost += (rtx_cost (XEXP (XEXP (x, 0), 0), VOIDmode, SIGN_EXTEND,
				  0, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, MINUS, 1, speed_p));
	      return true;
	    }
	  else if (GET_CODE (XEXP (x, 1)) == ZERO_EXTEND
		   || GET_CODE (XEXP (x, 1)) == SIGN_EXTEND)
	    {
	      if (speed_p)
		*cost += (extra_cost->alu.arith
			  + (GET_CODE (XEXP (x, 1)) == ZERO_EXTEND
			     ? extra_cost->alu.arith
			     : extra_cost->alu.arith_shift));
	      *cost += (rtx_cost (XEXP (x, 0), mode, MINUS, 0, speed_p)
			+ rtx_cost (XEXP (XEXP (x, 1), 0), VOIDmode,
				    GET_CODE (XEXP (x, 1)), 0, speed_p));
	      return true;
	    }

	  if (speed_p)
	    *cost += 2 * extra_cost->alu.arith;
	  return false;
	}

      /* Vector mode?  */

      *cost = LIBCALL_COST (2);
      return false;

    case PLUS:
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT
	  && (mode == SFmode || !TARGET_VFP_SINGLE))
	{
	  if (GET_CODE (XEXP (x, 0)) == MULT)
	    {
	      rtx mul_op0, mul_op1, add_op;

	      if (speed_p)
		*cost += extra_cost->fp[mode != SFmode].mult_addsub;

	      mul_op0 = XEXP (XEXP (x, 0), 0);
	      mul_op1 = XEXP (XEXP (x, 0), 1);
	      add_op = XEXP (x, 1);

	      *cost += (rtx_cost (mul_op0, mode, code, 0, speed_p)
			+ rtx_cost (mul_op1, mode, code, 0, speed_p)
			+ rtx_cost (add_op, mode, code, 0, speed_p));

	      return true;
	    }

	  if (speed_p)
	    *cost += extra_cost->fp[mode != SFmode].addsub;
	  return false;
	}
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  *cost = LIBCALL_COST (2);
	  return false;
	}

	/* Narrow modes can be synthesized in SImode, but the range
	   of useful sub-operations is limited.  Check for shift operations
	   on one of the operands.  Only left shifts can be used in the
	   narrow modes.  */
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_SIZE (mode) < 4)
	{
	  rtx shift_op, shift_reg;
	  shift_reg = NULL;

	  HANDLE_NARROW_SHIFT_ARITH (PLUS, 0);

	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      int insns = arm_gen_constant (PLUS, SImode, NULL_RTX,
					    INTVAL (XEXP (x, 1)), NULL_RTX,
					    NULL_RTX, 1, 0);
	      *cost = COSTS_N_INSNS (insns);
	      if (speed_p)
		*cost += insns * extra_cost->alu.arith;
	      /* Slightly penalize a narrow operation as the result may
		 need widening.  */
	      *cost += 1 + rtx_cost (XEXP (x, 0), mode, PLUS, 0, speed_p);
	      return true;
	    }

	  /* Slightly penalize a narrow operation as the result may
	     need widening.  */
	  *cost += 1;
	  if (speed_p)
	    *cost += extra_cost->alu.arith;

	  return false;
	}

      if (mode == SImode)
	{
	  rtx shift_op, shift_reg;

	  if (TARGET_INT_SIMD
	      && (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
		  || GET_CODE (XEXP (x, 0)) == SIGN_EXTEND))
	    {
	      /* UXTA[BH] or SXTA[BH].  */
	      if (speed_p)
		*cost += extra_cost->alu.extend_arith;
	      *cost += (rtx_cost (XEXP (XEXP (x, 0), 0), VOIDmode, ZERO_EXTEND,
				  0, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, PLUS, 0, speed_p));
	      return true;
	    }

	  shift_reg = NULL;
	  shift_op = shifter_op_p (XEXP (x, 0), &shift_reg);
	  if (shift_op != NULL)
	    {
	      if (shift_reg)
		{
		  if (speed_p)
		    *cost += extra_cost->alu.arith_shift_reg;
		  *cost += rtx_cost (shift_reg, mode, ASHIFT, 1, speed_p);
		}
	      else if (speed_p)
		*cost += extra_cost->alu.arith_shift;

	      *cost += (rtx_cost (shift_op, mode, ASHIFT, 0, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, PLUS, 1, speed_p));
	      return true;
	    }
	  if (GET_CODE (XEXP (x, 0)) == MULT)
	    {
	      rtx mul_op = XEXP (x, 0);

	      if (TARGET_DSP_MULTIPLY
		  && ((GET_CODE (XEXP (mul_op, 0)) == SIGN_EXTEND
		       && (GET_CODE (XEXP (mul_op, 1)) == SIGN_EXTEND
			   || (GET_CODE (XEXP (mul_op, 1)) == ASHIFTRT
			       && CONST_INT_P (XEXP (XEXP (mul_op, 1), 1))
			       && INTVAL (XEXP (XEXP (mul_op, 1), 1)) == 16)))
		      || (GET_CODE (XEXP (mul_op, 0)) == ASHIFTRT
			  && CONST_INT_P (XEXP (XEXP (mul_op, 0), 1))
			  && INTVAL (XEXP (XEXP (mul_op, 0), 1)) == 16
			  && (GET_CODE (XEXP (mul_op, 1)) == SIGN_EXTEND
			      || (GET_CODE (XEXP (mul_op, 1)) == ASHIFTRT
				  && CONST_INT_P (XEXP (XEXP (mul_op, 1), 1))
				  && (INTVAL (XEXP (XEXP (mul_op, 1), 1))
				      == 16))))))
		{
		  /* SMLA[BT][BT].  */
		  if (speed_p)
		    *cost += extra_cost->mult[0].extend_add;
		  *cost += (rtx_cost (XEXP (XEXP (mul_op, 0), 0), mode,
				      SIGN_EXTEND, 0, speed_p)
			    + rtx_cost (XEXP (XEXP (mul_op, 1), 0), mode,
					SIGN_EXTEND, 0, speed_p)
			    + rtx_cost (XEXP (x, 1), mode, PLUS, 1, speed_p));
		  return true;
		}

	      if (speed_p)
		*cost += extra_cost->mult[0].add;
	      *cost += (rtx_cost (XEXP (mul_op, 0), mode, MULT, 0, speed_p)
			+ rtx_cost (XEXP (mul_op, 1), mode, MULT, 1, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, PLUS, 1, speed_p));
	      return true;
	    }
	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      int insns = arm_gen_constant (PLUS, SImode, NULL_RTX,
					    INTVAL (XEXP (x, 1)), NULL_RTX,
					    NULL_RTX, 1, 0);
	      *cost = COSTS_N_INSNS (insns);
	      if (speed_p)
		*cost += insns * extra_cost->alu.arith;
	      *cost += rtx_cost (XEXP (x, 0), mode, PLUS, 0, speed_p);
	      return true;
	    }
	  else if (speed_p)
	    *cost += extra_cost->alu.arith;

	  return false;
	}

      if (mode == DImode)
	{
	  if (arm_arch3m
	      && GET_CODE (XEXP (x, 0)) == MULT
	      && ((GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTEND
		   && GET_CODE (XEXP (XEXP (x, 0), 1)) == ZERO_EXTEND)
		  || (GET_CODE (XEXP (XEXP (x, 0), 0)) == SIGN_EXTEND
		      && GET_CODE (XEXP (XEXP (x, 0), 1)) == SIGN_EXTEND)))
	    {
	      if (speed_p)
		*cost += extra_cost->mult[1].extend_add;
	      *cost += (rtx_cost (XEXP (XEXP (XEXP (x, 0), 0), 0), mode,
				  ZERO_EXTEND, 0, speed_p)
			+ rtx_cost (XEXP (XEXP (XEXP (x, 0), 1), 0), mode,
				    ZERO_EXTEND, 0, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, PLUS, 1, speed_p));
	      return true;
	    }

	  *cost += COSTS_N_INSNS (1);

	  if (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	      || GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	    {
	      if (speed_p)
		*cost += (extra_cost->alu.arith
			  + (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
			     ? extra_cost->alu.arith
			     : extra_cost->alu.arith_shift));

	      *cost += (rtx_cost (XEXP (XEXP (x, 0), 0), VOIDmode, ZERO_EXTEND,
				  0, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, PLUS, 1, speed_p));
	      return true;
	    }

	  if (speed_p)
	    *cost += 2 * extra_cost->alu.arith;
	  return false;
	}

      /* Vector mode?  */
      *cost = LIBCALL_COST (2);
      return false;
    case IOR:
      if (mode == SImode && arm_arch6 && aarch_rev16_p (x))
        {
          if (speed_p)
            *cost += extra_cost->alu.rev;

          return true;
        }
    /* Fall through.  */
    case AND: case XOR:
      if (mode == SImode)
	{
	  enum rtx_code subcode = GET_CODE (XEXP (x, 0));
	  rtx op0 = XEXP (x, 0);
	  rtx shift_op, shift_reg;

	  if (subcode == NOT
	      && (code == AND
		  || (code == IOR && TARGET_THUMB2)))
	    op0 = XEXP (op0, 0);

	  shift_reg = NULL;
	  shift_op = shifter_op_p (op0, &shift_reg);
	  if (shift_op != NULL)
	    {
	      if (shift_reg)
		{
		  if (speed_p)
		    *cost += extra_cost->alu.log_shift_reg;
		  *cost += rtx_cost (shift_reg, mode, ASHIFT, 1, speed_p);
		}
	      else if (speed_p)
		*cost += extra_cost->alu.log_shift;

	      *cost += (rtx_cost (shift_op, mode, ASHIFT, 0, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, code, 1, speed_p));
	      return true;
	    }

	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      int insns = arm_gen_constant (code, SImode, NULL_RTX,
					    INTVAL (XEXP (x, 1)), NULL_RTX,
					    NULL_RTX, 1, 0);

	      *cost = COSTS_N_INSNS (insns);
	      if (speed_p)
		*cost += insns * extra_cost->alu.logical;
	      *cost += rtx_cost (op0, mode, code, 0, speed_p);
	      return true;
	    }

	  if (speed_p)
	    *cost += extra_cost->alu.logical;
	  *cost += (rtx_cost (op0, mode, code, 0, speed_p)
		    + rtx_cost (XEXP (x, 1), mode, code, 1, speed_p));
	  return true;
	}

      if (mode == DImode)
	{
	  rtx op0 = XEXP (x, 0);
	  enum rtx_code subcode = GET_CODE (op0);

	  *cost += COSTS_N_INSNS (1);

	  if (subcode == NOT
	      && (code == AND
		  || (code == IOR && TARGET_THUMB2)))
	    op0 = XEXP (op0, 0);

	  if (GET_CODE (op0) == ZERO_EXTEND)
	    {
	      if (speed_p)
		*cost += 2 * extra_cost->alu.logical;

	      *cost += (rtx_cost (XEXP (op0, 0), VOIDmode, ZERO_EXTEND,
				  0, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, code, 0, speed_p));
	      return true;
	    }
	  else if (GET_CODE (op0) == SIGN_EXTEND)
	    {
	      if (speed_p)
		*cost += extra_cost->alu.logical + extra_cost->alu.log_shift;

	      *cost += (rtx_cost (XEXP (op0, 0), VOIDmode, SIGN_EXTEND,
				  0, speed_p)
			+ rtx_cost (XEXP (x, 1), mode, code, 0, speed_p));
	      return true;
	    }

	  if (speed_p)
	    *cost += 2 * extra_cost->alu.logical;

	  return true;
	}
      /* Vector mode?  */

      *cost = LIBCALL_COST (2);
      return false;

    case MULT:
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT
	  && (mode == SFmode || !TARGET_VFP_SINGLE))
	{
	  rtx op0 = XEXP (x, 0);

	  if (GET_CODE (op0) == NEG && !flag_rounding_math)
	    op0 = XEXP (op0, 0);

	  if (speed_p)
	    *cost += extra_cost->fp[mode != SFmode].mult;

	  *cost += (rtx_cost (op0, mode, MULT, 0, speed_p)
		    + rtx_cost (XEXP (x, 1), mode, MULT, 1, speed_p));
	  return true;
	}
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  *cost = LIBCALL_COST (2);
	  return false;
	}

      if (mode == SImode)
	{
	  if (TARGET_DSP_MULTIPLY
	      && ((GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
		   && (GET_CODE (XEXP (x, 1)) == SIGN_EXTEND
		       || (GET_CODE (XEXP (x, 1)) == ASHIFTRT
			   && CONST_INT_P (XEXP (XEXP (x, 1), 1))
			   && INTVAL (XEXP (XEXP (x, 1), 1)) == 16)))
		  || (GET_CODE (XEXP (x, 0)) == ASHIFTRT
		      && CONST_INT_P (XEXP (XEXP (x, 0), 1))
		      && INTVAL (XEXP (XEXP (x, 0), 1)) == 16
		      && (GET_CODE (XEXP (x, 1)) == SIGN_EXTEND
			  || (GET_CODE (XEXP (x, 1)) == ASHIFTRT
			      && CONST_INT_P (XEXP (XEXP (x, 1), 1))
			      && (INTVAL (XEXP (XEXP (x, 1), 1))
				  == 16))))))
	    {
	      /* SMUL[TB][TB].  */
	      if (speed_p)
		*cost += extra_cost->mult[0].extend;
	      *cost += rtx_cost (XEXP (XEXP (x, 0), 0), mode,
				 SIGN_EXTEND, 0, speed_p);
	      *cost += rtx_cost (XEXP (XEXP (x, 1), 0), mode,
				 SIGN_EXTEND, 1, speed_p);
	      return true;
	    }
	  if (speed_p)
	    *cost += extra_cost->mult[0].simple;
	  return false;
	}

      if (mode == DImode)
	{
	  if (arm_arch3m
	      && ((GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
		   && GET_CODE (XEXP (x, 1)) == ZERO_EXTEND)
		  || (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
		      && GET_CODE (XEXP (x, 1)) == SIGN_EXTEND)))
	    {
	      if (speed_p)
		*cost += extra_cost->mult[1].extend;
	      *cost += (rtx_cost (XEXP (XEXP (x, 0), 0), VOIDmode,
				  ZERO_EXTEND, 0, speed_p)
			+ rtx_cost (XEXP (XEXP (x, 1), 0), VOIDmode,
				    ZERO_EXTEND, 0, speed_p));
	      return true;
	    }

	  *cost = LIBCALL_COST (2);
	  return false;
	}

      /* Vector mode?  */
      *cost = LIBCALL_COST (2);
      return false;

    case NEG:
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT
	  && (mode == SFmode || !TARGET_VFP_SINGLE))
	{
	  if (GET_CODE (XEXP (x, 0)) == MULT)
	    {
	      /* VNMUL.  */
	      *cost = rtx_cost (XEXP (x, 0), mode, NEG, 0, speed_p);
	      return true;
	    }

	  if (speed_p)
	    *cost += extra_cost->fp[mode != SFmode].neg;

	  return false;
	}
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  *cost = LIBCALL_COST (1);
	  return false;
	}

      if (mode == SImode)
	{
	  if (GET_CODE (XEXP (x, 0)) == ABS)
	    {
	      *cost += COSTS_N_INSNS (1);
	      /* Assume the non-flag-changing variant.  */
	      if (speed_p)
		*cost += (extra_cost->alu.log_shift
			  + extra_cost->alu.arith_shift);
	      *cost += rtx_cost (XEXP (XEXP (x, 0), 0), mode, ABS, 0, speed_p);
	      return true;
	    }

	  if (GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == RTX_COMPARE
	      || GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == RTX_COMM_COMPARE)
	    {
	      *cost += COSTS_N_INSNS (1);
	      /* No extra cost for MOV imm and MVN imm.  */
	      /* If the comparison op is using the flags, there's no further
		 cost, otherwise we need to add the cost of the comparison.  */
	      if (!(REG_P (XEXP (XEXP (x, 0), 0))
		    && REGNO (XEXP (XEXP (x, 0), 0)) == CC_REGNUM
		    && XEXP (XEXP (x, 0), 1) == const0_rtx))
		{
		  mode = GET_MODE (XEXP (XEXP (x, 0), 0));
		  *cost += (COSTS_N_INSNS (1)
			    + rtx_cost (XEXP (XEXP (x, 0), 0), mode, COMPARE,
					0, speed_p)
			    + rtx_cost (XEXP (XEXP (x, 0), 1), mode, COMPARE,
					1, speed_p));
		  if (speed_p)
		    *cost += extra_cost->alu.arith;
		}
	      return true;
	    }

	  if (speed_p)
	    *cost += extra_cost->alu.arith;
	  return false;
	}

      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_SIZE (mode) < 4)
	{
	  /* Slightly disparage, as we might need an extend operation.  */
	  *cost += 1;
	  if (speed_p)
	    *cost += extra_cost->alu.arith;
	  return false;
	}

      if (mode == DImode)
	{
	  *cost += COSTS_N_INSNS (1);
	  if (speed_p)
	    *cost += 2 * extra_cost->alu.arith;
	  return false;
	}

      /* Vector mode?  */
      *cost = LIBCALL_COST (1);
      return false;

    case NOT:
      if (mode == SImode)
	{
	  rtx shift_op;
	  rtx shift_reg = NULL;

	  shift_op = shifter_op_p (XEXP (x, 0), &shift_reg);

	  if (shift_op)
	    {
	      if (shift_reg != NULL)
		{
		  if (speed_p)
		    *cost += extra_cost->alu.log_shift_reg;
		  *cost += rtx_cost (shift_reg, mode, ASHIFT, 1, speed_p);
		}
	      else if (speed_p)
		*cost += extra_cost->alu.log_shift;
	      *cost += rtx_cost (shift_op, mode, ASHIFT, 0, speed_p);
	      return true;
	    }

	  if (speed_p)
	    *cost += extra_cost->alu.logical;
	  return false;
	}
      if (mode == DImode)
	{
	  *cost += COSTS_N_INSNS (1);
	  return false;
	}

      /* Vector mode?  */

      *cost += LIBCALL_COST (1);
      return false;

    case IF_THEN_ELSE:
      {
        if (GET_CODE (XEXP (x, 1)) == PC || GET_CODE (XEXP (x, 2)) == PC)
	  {
	    *cost += COSTS_N_INSNS (3);
	    return true;
	  }
	int op1cost = rtx_cost (XEXP (x, 1), mode, SET, 1, speed_p);
	int op2cost = rtx_cost (XEXP (x, 2), mode, SET, 1, speed_p);

	*cost = rtx_cost (XEXP (x, 0), mode, IF_THEN_ELSE, 0, speed_p);
	/* Assume that if one arm of the if_then_else is a register,
	   that it will be tied with the result and eliminate the
	   conditional insn.  */
	if (REG_P (XEXP (x, 1)))
	  *cost += op2cost;
	else if (REG_P (XEXP (x, 2)))
	  *cost += op1cost;
	else
	  {
	    if (speed_p)
	      {
		if (extra_cost->alu.non_exec_costs_exec)
		  *cost += op1cost + op2cost + extra_cost->alu.non_exec;
		else
		  *cost += MAX (op1cost, op2cost) + extra_cost->alu.non_exec;
	      }
	    else
	      *cost += op1cost + op2cost;
	  }
      }
      return true;

    case COMPARE:
      if (cc_register (XEXP (x, 0), VOIDmode) && XEXP (x, 1) == const0_rtx)
	*cost = 0;
      else
	{
	  machine_mode op0mode;
	  /* We'll mostly assume that the cost of a compare is the cost of the
	     LHS.  However, there are some notable exceptions.  */

	  /* Floating point compares are never done as side-effects.  */
	  op0mode = GET_MODE (XEXP (x, 0));
	  if (TARGET_HARD_FLOAT && GET_MODE_CLASS (op0mode) == MODE_FLOAT
	      && (op0mode == SFmode || !TARGET_VFP_SINGLE))
	    {
	      if (speed_p)
		*cost += extra_cost->fp[op0mode != SFmode].compare;

	      if (XEXP (x, 1) == CONST0_RTX (op0mode))
		{
		  *cost += rtx_cost (XEXP (x, 0), op0mode, code, 0, speed_p);
		  return true;
		}

	      return false;
	    }
	  else if (GET_MODE_CLASS (op0mode) == MODE_FLOAT)
	    {
	      *cost = LIBCALL_COST (2);
	      return false;
	    }

	  /* DImode compares normally take two insns.  */
	  if (op0mode == DImode)
	    {
	      *cost += COSTS_N_INSNS (1);
	      if (speed_p)
		*cost += 2 * extra_cost->alu.arith;
	      return false;
	    }

	  if (op0mode == SImode)
	    {
	      rtx shift_op;
	      rtx shift_reg;

	      if (XEXP (x, 1) == const0_rtx
		  && !(REG_P (XEXP (x, 0))
		       || (GET_CODE (XEXP (x, 0)) == SUBREG
			   && REG_P (SUBREG_REG (XEXP (x, 0))))))
		{
		  *cost = rtx_cost (XEXP (x, 0), op0mode, COMPARE, 0, speed_p);

		  /* Multiply operations that set the flags are often
		     significantly more expensive.  */
		  if (speed_p
		      && GET_CODE (XEXP (x, 0)) == MULT
		      && !power_of_two_operand (XEXP (XEXP (x, 0), 1), mode))
		    *cost += extra_cost->mult[0].flag_setting;

		  if (speed_p
		      && GET_CODE (XEXP (x, 0)) == PLUS
		      && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
		      && !power_of_two_operand (XEXP (XEXP (XEXP (x, 0),
							    0), 1), mode))
		    *cost += extra_cost->mult[0].flag_setting;
		  return true;
		}

	      shift_reg = NULL;
	      shift_op = shifter_op_p (XEXP (x, 0), &shift_reg);
	      if (shift_op != NULL)
		{
		  if (shift_reg != NULL)
		    {
		      *cost += rtx_cost (shift_reg, op0mode, ASHIFT,
					 1, speed_p);
		      if (speed_p)
			*cost += extra_cost->alu.arith_shift_reg;
		    }
		  else if (speed_p)
		    *cost += extra_cost->alu.arith_shift;
		  *cost += rtx_cost (shift_op, op0mode, ASHIFT, 0, speed_p);
		  *cost += rtx_cost (XEXP (x, 1), op0mode, COMPARE, 1, speed_p);
		  return true;
		}

	      if (speed_p)
		*cost += extra_cost->alu.arith;
	      if (CONST_INT_P (XEXP (x, 1))
		  && const_ok_for_op (INTVAL (XEXP (x, 1)), COMPARE))
		{
		  *cost += rtx_cost (XEXP (x, 0), op0mode, COMPARE, 0, speed_p);
		  return true;
		}
	      return false;
	    }

	  /* Vector mode?  */

	  *cost = LIBCALL_COST (2);
	  return false;
	}
      return true;

    case EQ:
    case NE:
    case LT:
    case LE:
    case GT:
    case GE:
    case LTU:
    case LEU:
    case GEU:
    case GTU:
    case ORDERED:
    case UNORDERED:
    case UNEQ:
    case UNLE:
    case UNLT:
    case UNGE:
    case UNGT:
    case LTGT:
      if (outer_code == SET)
	{
	  /* Is it a store-flag operation?  */
	  if (REG_P (XEXP (x, 0)) && REGNO (XEXP (x, 0)) == CC_REGNUM
	      && XEXP (x, 1) == const0_rtx)
	    {
	      /* Thumb also needs an IT insn.  */
	      *cost += COSTS_N_INSNS (TARGET_THUMB ? 2 : 1);
	      return true;
	    }
	  if (XEXP (x, 1) == const0_rtx)
	    {
	      switch (code)
		{
		case LT:
		  /* LSR Rd, Rn, #31.  */
		  if (speed_p)
		    *cost += extra_cost->alu.shift;
		  break;

		case EQ:
		  /* RSBS T1, Rn, #0
		     ADC  Rd, Rn, T1.  */

		case NE:
		  /* SUBS T1, Rn, #1
		     SBC  Rd, Rn, T1.  */
		  *cost += COSTS_N_INSNS (1);
		  break;

		case LE:
		  /* RSBS T1, Rn, Rn, LSR #31
		     ADC  Rd, Rn, T1. */
		  *cost += COSTS_N_INSNS (1);
		  if (speed_p)
		    *cost += extra_cost->alu.arith_shift;
		  break;

		case GT:
		  /* RSB  Rd, Rn, Rn, ASR #1
		     LSR  Rd, Rd, #31.  */
		  *cost += COSTS_N_INSNS (1);
		  if (speed_p)
		    *cost += (extra_cost->alu.arith_shift
			      + extra_cost->alu.shift);
		  break;

		case GE:
		  /* ASR  Rd, Rn, #31
		     ADD  Rd, Rn, #1.  */
		  *cost += COSTS_N_INSNS (1);
		  if (speed_p)
		    *cost += extra_cost->alu.shift;
		  break;

		default:
		  /* Remaining cases are either meaningless or would take
		     three insns anyway.  */
		  *cost = COSTS_N_INSNS (3);
		  break;
		}
	      *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
	      return true;
	    }
	  else
	    {
	      *cost += COSTS_N_INSNS (TARGET_THUMB ? 3 : 2);
	      if (CONST_INT_P (XEXP (x, 1))
		  && const_ok_for_op (INTVAL (XEXP (x, 1)), COMPARE))
		{
		  *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
		  return true;
		}

	      return false;
	    }
	}
      /* Not directly inside a set.  If it involves the condition code
	 register it must be the condition for a branch, cond_exec or
	 I_T_E operation.  Since the comparison is performed elsewhere
	 this is just the control part which has no additional
	 cost.  */
      else if (REG_P (XEXP (x, 0)) && REGNO (XEXP (x, 0)) == CC_REGNUM
	       && XEXP (x, 1) == const0_rtx)
	{
	  *cost = 0;
	  return true;
	}
      return false;

    case ABS:
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT
	  && (mode == SFmode || !TARGET_VFP_SINGLE))
	{
	  if (speed_p)
	    *cost += extra_cost->fp[mode != SFmode].neg;

	  return false;
	}
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  *cost = LIBCALL_COST (1);
	  return false;
	}

      if (mode == SImode)
	{
	  if (speed_p)
	    *cost += extra_cost->alu.log_shift + extra_cost->alu.arith_shift;
	  return false;
	}
      /* Vector mode?  */
      *cost = LIBCALL_COST (1);
      return false;

    case SIGN_EXTEND:
      if ((arm_arch4 || GET_MODE (XEXP (x, 0)) == SImode)
	  && MEM_P (XEXP (x, 0)))
	{
	  if (mode == DImode)
	    *cost += COSTS_N_INSNS (1);

	  if (!speed_p)
	    return true;

	  if (GET_MODE (XEXP (x, 0)) == SImode)
	    *cost += extra_cost->ldst.load;
	  else
	    *cost += extra_cost->ldst.load_sign_extend;

	  if (mode == DImode)
	    *cost += extra_cost->alu.shift;

	  return true;
	}

      /* Widening from less than 32-bits requires an extend operation.  */
      if (GET_MODE (XEXP (x, 0)) != SImode && arm_arch6)
	{
	  /* We have SXTB/SXTH.  */
	  *cost += rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed_p);
	  if (speed_p)
	    *cost += extra_cost->alu.extend;
	}
      else if (GET_MODE (XEXP (x, 0)) != SImode)
	{
	  /* Needs two shifts.  */
	  *cost += COSTS_N_INSNS (1);
	  *cost += rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed_p);
	  if (speed_p)
	    *cost += 2 * extra_cost->alu.shift;
	}

      /* Widening beyond 32-bits requires one more insn.  */
      if (mode == DImode)
	{
	  *cost += COSTS_N_INSNS (1);
	  if (speed_p)
	    *cost += extra_cost->alu.shift;
	}

      return true;

    case ZERO_EXTEND:
      if ((arm_arch4
	   || GET_MODE (XEXP (x, 0)) == SImode
	   || GET_MODE (XEXP (x, 0)) == QImode)
	  && MEM_P (XEXP (x, 0)))
	{
	  *cost = rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed_p);

	  if (mode == DImode)
	    *cost += COSTS_N_INSNS (1);  /* No speed penalty.  */

	  return true;
	}

      /* Widening from less than 32-bits requires an extend operation.  */
      if (GET_MODE (XEXP (x, 0)) == QImode)
	{
	  /* UXTB can be a shorter instruction in Thumb2, but it might
	     be slower than the AND Rd, Rn, #255 alternative.  When
	     optimizing for speed it should never be slower to use
	     AND, and we don't really model 16-bit vs 32-bit insns
	     here.  */
	  if (speed_p)
	    *cost += extra_cost->alu.logical;
	}
      else if (GET_MODE (XEXP (x, 0)) != SImode && arm_arch6)
	{
	  /* We have UXTB/UXTH.  */
	  *cost += rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed_p);
	  if (speed_p)
	    *cost += extra_cost->alu.extend;
	}
      else if (GET_MODE (XEXP (x, 0)) != SImode)
	{
	  /* Needs two shifts.  It's marginally preferable to use
	     shifts rather than two BIC instructions as the second
	     shift may merge with a subsequent insn as a shifter
	     op.  */
	  *cost = COSTS_N_INSNS (2);
	  *cost += rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed_p);
	  if (speed_p)
	    *cost += 2 * extra_cost->alu.shift;
	}

      /* Widening beyond 32-bits requires one more insn.  */
      if (mode == DImode)
	{
	  *cost += COSTS_N_INSNS (1);	/* No speed penalty.  */
	}

      return true;

    case CONST_INT:
      *cost = 0;
      /* CONST_INT has no mode, so we cannot tell for sure how many
	 insns are really going to be needed.  The best we can do is
	 look at the value passed.  If it fits in SImode, then assume
	 that's the mode it will be used for.  Otherwise assume it
	 will be used in DImode.  */
      if (INTVAL (x) == trunc_int_for_mode (INTVAL (x), SImode))
	mode = SImode;
      else
	mode = DImode;

      /* Avoid blowing up in arm_gen_constant ().  */
      if (!(outer_code == PLUS
	    || outer_code == AND
	    || outer_code == IOR
	    || outer_code == XOR
	    || outer_code == MINUS))
	outer_code = SET;

    const_int_cost:
      if (mode == SImode)
	{
	  *cost += COSTS_N_INSNS (arm_gen_constant (outer_code, SImode, NULL,
						    INTVAL (x), NULL, NULL,
						    0, 0));
	  /* Extra costs?  */
	}
      else
	{
	  *cost += COSTS_N_INSNS (arm_gen_constant
				  (outer_code, SImode, NULL,
				   trunc_int_for_mode (INTVAL (x), SImode),
				   NULL, NULL, 0, 0)
				  + arm_gen_constant (outer_code, SImode, NULL,
						      INTVAL (x) >> 32, NULL,
						      NULL, 0, 0));
	  /* Extra costs?  */
	}

      return true;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      if (speed_p)
	{
	  if (arm_arch_thumb2 && !flag_pic)
	    *cost += COSTS_N_INSNS (1);
	  else
	    *cost += extra_cost->ldst.load;
	}
      else
	*cost += COSTS_N_INSNS (1);

      if (flag_pic)
	{
	  *cost += COSTS_N_INSNS (1);
	  if (speed_p)
	    *cost += extra_cost->alu.arith;
	}

      return true;

    case CONST_FIXED:
      *cost = COSTS_N_INSNS (4);
      /* Fixme.  */
      return true;

    case CONST_DOUBLE:
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT
	  && (mode == SFmode || !TARGET_VFP_SINGLE))
	{
	  if (vfp3_const_double_rtx (x))
	    {
	      if (speed_p)
		*cost += extra_cost->fp[mode == DFmode].fpconst;
	      return true;
	    }

	  if (speed_p)
	    {
	      if (mode == DFmode)
		*cost += extra_cost->ldst.loadd;
	      else
		*cost += extra_cost->ldst.loadf;
	    }
	  else
	    *cost += COSTS_N_INSNS (1 + (mode == DFmode));

	  return true;
	}
      *cost = COSTS_N_INSNS (4);
      return true;

    case CONST_VECTOR:
      /* Fixme.  */
      if (TARGET_NEON
	  && TARGET_HARD_FLOAT
	  && (VALID_NEON_DREG_MODE (mode) || VALID_NEON_QREG_MODE (mode))
	  && neon_immediate_valid_for_move (x, mode, NULL, NULL))
	*cost = COSTS_N_INSNS (1);
      else
	*cost = COSTS_N_INSNS (4);
      return true;

    case HIGH:
    case LO_SUM:
      /* When optimizing for size, we prefer constant pool entries to
	 MOVW/MOVT pairs, so bump the cost of these slightly.  */
      if (!speed_p)
	*cost += 1;
      return true;

    case CLZ:
      if (speed_p)
	*cost += extra_cost->alu.clz;
      return false;

    case SMIN:
      if (XEXP (x, 1) == const0_rtx)
	{
	  if (speed_p)
	    *cost += extra_cost->alu.log_shift;
	  *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
	  return true;
	}
      /* Fall through.  */
    case SMAX:
    case UMIN:
    case UMAX:
      *cost += COSTS_N_INSNS (1);
      return false;

    case TRUNCATE:
      if (GET_CODE (XEXP (x, 0)) == ASHIFTRT
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && INTVAL (XEXP (XEXP (x, 0), 1)) == 32
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	  && ((GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == SIGN_EXTEND
	       && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == SIGN_EXTEND)
	      || (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == ZERO_EXTEND
		  && (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1))
		      == ZERO_EXTEND))))
	{
	  if (speed_p)
	    *cost += extra_cost->mult[1].extend;
	  *cost += (rtx_cost (XEXP (XEXP (XEXP (x, 0), 0), 0), VOIDmode,
			      ZERO_EXTEND, 0, speed_p)
		    + rtx_cost (XEXP (XEXP (XEXP (x, 0), 0), 1), VOIDmode,
				ZERO_EXTEND, 0, speed_p));
	  return true;
	}
      *cost = LIBCALL_COST (1);
      return false;

    case UNSPEC_VOLATILE:
    case UNSPEC:
      return arm_unspec_cost (x, outer_code, speed_p, cost);

    case PC:
      /* Reading the PC is like reading any other register.  Writing it
	 is more expensive, but we take that into account elsewhere.  */
      *cost = 0;
      return true;

    case ZERO_EXTRACT:
      /* TODO: Simple zero_extract of bottom bits using AND.  */
      /* Fall through.  */
    case SIGN_EXTRACT:
      if (arm_arch6
	  && mode == SImode
	  && CONST_INT_P (XEXP (x, 1))
	  && CONST_INT_P (XEXP (x, 2)))
	{
	  if (speed_p)
	    *cost += extra_cost->alu.bfx;
	  *cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
	  return true;
	}
      /* Without UBFX/SBFX, need to resort to shift operations.  */
      *cost += COSTS_N_INSNS (1);
      if (speed_p)
	*cost += 2 * extra_cost->alu.shift;
      *cost += rtx_cost (XEXP (x, 0), mode, ASHIFT, 0, speed_p);
      return true;

    case FLOAT_EXTEND:
      if (TARGET_HARD_FLOAT)
	{
	  if (speed_p)
	    *cost += extra_cost->fp[mode == DFmode].widen;
	  if (!TARGET_VFP5
	      && GET_MODE (XEXP (x, 0)) == HFmode)
	    {
	      /* Pre v8, widening HF->DF is a two-step process, first
	         widening to SFmode.  */
	      *cost += COSTS_N_INSNS (1);
	      if (speed_p)
		*cost += extra_cost->fp[0].widen;
	    }
	  *cost += rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed_p);
	  return true;
	}

      *cost = LIBCALL_COST (1);
      return false;

    case FLOAT_TRUNCATE:
      if (TARGET_HARD_FLOAT)
	{
	  if (speed_p)
	    *cost += extra_cost->fp[mode == DFmode].narrow;
	  *cost += rtx_cost (XEXP (x, 0), VOIDmode, code, 0, speed_p);
	  return true;
	  /* Vector modes?  */
	}
      *cost = LIBCALL_COST (1);
      return false;

    case FMA:
      if (TARGET_32BIT && TARGET_HARD_FLOAT && TARGET_FMA)
        {
          rtx op0 = XEXP (x, 0);
          rtx op1 = XEXP (x, 1);
          rtx op2 = XEXP (x, 2);


          /* vfms or vfnma.  */
          if (GET_CODE (op0) == NEG)
            op0 = XEXP (op0, 0);

          /* vfnms or vfnma.  */
          if (GET_CODE (op2) == NEG)
            op2 = XEXP (op2, 0);

          *cost += rtx_cost (op0, mode, FMA, 0, speed_p);
          *cost += rtx_cost (op1, mode, FMA, 1, speed_p);
          *cost += rtx_cost (op2, mode, FMA, 2, speed_p);

          if (speed_p)
            *cost += extra_cost->fp[mode ==DFmode].fma;

          return true;
        }

      *cost = LIBCALL_COST (3);
      return false;

    case FIX:
    case UNSIGNED_FIX:
      if (TARGET_HARD_FLOAT)
	{
	  /* The *combine_vcvtf2i reduces a vmul+vcvt into
	     a vcvt fixed-point conversion.  */
	  if (code == FIX && mode == SImode
	      && GET_CODE (XEXP (x, 0)) == FIX
	      && GET_MODE (XEXP (x, 0)) == SFmode
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	      && vfp3_const_double_for_bits (XEXP (XEXP (XEXP (x, 0), 0), 1))
		 > 0)
	    {
	      if (speed_p)
		*cost += extra_cost->fp[0].toint;

	      *cost += rtx_cost (XEXP (XEXP (XEXP (x, 0), 0), 0), mode,
				 code, 0, speed_p);
	      return true;
	    }

	  if (GET_MODE_CLASS (mode) == MODE_INT)
	    {
	      mode = GET_MODE (XEXP (x, 0));
	      if (speed_p)
		*cost += extra_cost->fp[mode == DFmode].toint;
	      /* Strip of the 'cost' of rounding towards zero.  */
	      if (GET_CODE (XEXP (x, 0)) == FIX)
		*cost += rtx_cost (XEXP (XEXP (x, 0), 0), mode, code,
				   0, speed_p);
	      else
		*cost += rtx_cost (XEXP (x, 0), mode, code, 0, speed_p);
	      /* ??? Increase the cost to deal with transferring from
		 FP -> CORE registers?  */
	      return true;
	    }
	  else if (GET_MODE_CLASS (mode) == MODE_FLOAT
		   && TARGET_VFP5)
	    {
	      if (speed_p)
		*cost += extra_cost->fp[mode == DFmode].roundint;
	      return false;
	    }
	  /* Vector costs? */
	}
      *cost = LIBCALL_COST (1);
      return false;

    case FLOAT:
    case UNSIGNED_FLOAT:
      if (TARGET_HARD_FLOAT)
	{
	  /* ??? Increase the cost to deal with transferring from CORE
	     -> FP registers?  */
	  if (speed_p)
	    *cost += extra_cost->fp[mode == DFmode].fromint;
	  return false;
	}
      *cost = LIBCALL_COST (1);
      return false;

    case CALL:
      return true;

    case ASM_OPERANDS:
      {
      /* Just a guess.  Guess number of instructions in the asm
         plus one insn per input.  Always a minimum of COSTS_N_INSNS (1)
         though (see PR60663).  */
        int asm_length = MAX (1, asm_str_count (ASM_OPERANDS_TEMPLATE (x)));
        int num_operands = ASM_OPERANDS_INPUT_LENGTH (x);

        *cost = COSTS_N_INSNS (asm_length + num_operands);
        return true;
      }
    default:
      if (mode != VOIDmode)
	*cost = COSTS_N_INSNS (ARM_NUM_REGS (mode));
      else
	*cost = COSTS_N_INSNS (4); /* Who knows?  */
      return false;
    }
}

#undef HANDLE_NARROW_SHIFT_ARITH

/* RTX costs entry point.  */

static bool
arm_rtx_costs (rtx x, machine_mode mode ATTRIBUTE_UNUSED, int outer_code,
	       int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  bool result;
  int code = GET_CODE (x);
  gcc_assert (current_tune->insn_extra_cost);

  result =  arm_rtx_costs_internal (x, (enum rtx_code) code,
				(enum rtx_code) outer_code,
				current_tune->insn_extra_cost,
				total, speed);

  if (dump_file && arm_verbose_cost)
    {
      print_rtl_single (dump_file, x);
      fprintf (dump_file, "\n%s cost: %d (%s)\n", speed ? "Hot" : "Cold",
	       *total, result ? "final" : "partial");
    }
  return result;
}

/* All address computations that can be done are free, but rtx cost returns
   the same for practically all of them.  So we weight the different types
   of address here in the order (most pref first):
   PRE/POST_INC/DEC, SHIFT or NON-INT sum, INT sum, REG, MEM or LABEL.  */
static inline int
arm_arm_address_cost (rtx x)
{
  enum rtx_code c  = GET_CODE (x);

  if (c == PRE_INC || c == PRE_DEC || c == POST_INC || c == POST_DEC)
    return 0;
  if (c == MEM || c == LABEL_REF || c == SYMBOL_REF)
    return 10;

  if (c == PLUS)
    {
      if (CONST_INT_P (XEXP (x, 1)))
	return 2;

      if (ARITHMETIC_P (XEXP (x, 0)) || ARITHMETIC_P (XEXP (x, 1)))
	return 3;

      return 4;
    }

  return 6;
}

static inline int
arm_thumb_address_cost (rtx x)
{
  enum rtx_code c  = GET_CODE (x);

  if (c == REG)
    return 1;
  if (c == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1)))
    return 1;

  return 2;
}

static int
arm_address_cost (rtx x, machine_mode mode ATTRIBUTE_UNUSED,
		  addr_space_t as ATTRIBUTE_UNUSED, bool speed ATTRIBUTE_UNUSED)
{
  return TARGET_32BIT ? arm_arm_address_cost (x) : arm_thumb_address_cost (x);
}

/* Adjust cost hook for XScale.  */
static bool
xscale_sched_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep,
			  int * cost)
{
  /* Some true dependencies can have a higher cost depending
     on precisely how certain input operands are used.  */
  if (dep_type == 0
      && recog_memoized (insn) >= 0
      && recog_memoized (dep) >= 0)
    {
      int shift_opnum = get_attr_shift (insn);
      enum attr_type attr_type = get_attr_type (dep);

      /* If nonzero, SHIFT_OPNUM contains the operand number of a shifted
	 operand for INSN.  If we have a shifted input operand and the
	 instruction we depend on is another ALU instruction, then we may
	 have to account for an additional stall.  */
      if (shift_opnum != 0
	  && (attr_type == TYPE_ALU_SHIFT_IMM
	      || attr_type == TYPE_ALUS_SHIFT_IMM
	      || attr_type == TYPE_LOGIC_SHIFT_IMM
	      || attr_type == TYPE_LOGICS_SHIFT_IMM
	      || attr_type == TYPE_ALU_SHIFT_REG
	      || attr_type == TYPE_ALUS_SHIFT_REG
	      || attr_type == TYPE_LOGIC_SHIFT_REG
	      || attr_type == TYPE_LOGICS_SHIFT_REG
	      || attr_type == TYPE_MOV_SHIFT
	      || attr_type == TYPE_MVN_SHIFT
	      || attr_type == TYPE_MOV_SHIFT_REG
	      || attr_type == TYPE_MVN_SHIFT_REG))
	{
	  rtx shifted_operand;
	  int opno;

	  /* Get the shifted operand.  */
	  extract_insn (insn);
	  shifted_operand = recog_data.operand[shift_opnum];

	  /* Iterate over all the operands in DEP.  If we write an operand
	     that overlaps with SHIFTED_OPERAND, then we have increase the
	     cost of this dependency.  */
	  extract_insn (dep);
	  preprocess_constraints (dep);
	  for (opno = 0; opno < recog_data.n_operands; opno++)
	    {
	      /* We can ignore strict inputs.  */
	      if (recog_data.operand_type[opno] == OP_IN)
		continue;

	      if (reg_overlap_mentioned_p (recog_data.operand[opno],
					   shifted_operand))
		{
		  *cost = 2;
		  return false;
		}
	    }
	}
    }
  return true;
}

/* Adjust cost hook for Cortex A9.  */
static bool
cortex_a9_sched_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep,
			     int * cost)
{
  switch (dep_type)
    {
    case REG_DEP_ANTI:
      *cost = 0;
      return false;

    case REG_DEP_TRUE:
    case REG_DEP_OUTPUT:
	if (recog_memoized (insn) >= 0
	    && recog_memoized (dep) >= 0)
	  {
	    if (GET_CODE (PATTERN (insn)) == SET)
	      {
		if (GET_MODE_CLASS
		    (GET_MODE (SET_DEST (PATTERN (insn)))) == MODE_FLOAT
		  || GET_MODE_CLASS
		    (GET_MODE (SET_SRC (PATTERN (insn)))) == MODE_FLOAT)
		  {
		    enum attr_type attr_type_insn = get_attr_type (insn);
		    enum attr_type attr_type_dep = get_attr_type (dep);

		    /* By default all dependencies of the form
		       s0 = s0 <op> s1
		       s0 = s0 <op> s2
		       have an extra latency of 1 cycle because
		       of the input and output dependency in this
		       case. However this gets modeled as an true
		       dependency and hence all these checks.  */
		    if (REG_P (SET_DEST (PATTERN (insn)))
			&& reg_set_p (SET_DEST (PATTERN (insn)), dep))
		      {
			/* FMACS is a special case where the dependent
			   instruction can be issued 3 cycles before
			   the normal latency in case of an output
			   dependency.  */
			if ((attr_type_insn == TYPE_FMACS
			     || attr_type_insn == TYPE_FMACD)
			    && (attr_type_dep == TYPE_FMACS
				|| attr_type_dep == TYPE_FMACD))
			  {
			    if (dep_type == REG_DEP_OUTPUT)
			      *cost = insn_default_latency (dep) - 3;
			    else
			      *cost = insn_default_latency (dep);
			    return false;
			  }
			else
			  {
			    if (dep_type == REG_DEP_OUTPUT)
			      *cost = insn_default_latency (dep) + 1;
			    else
			      *cost = insn_default_latency (dep);
			  }
			return false;
		      }
		  }
	      }
	  }
	break;

    default:
      gcc_unreachable ();
    }

  return true;
}

/* Adjust cost hook for FA726TE.  */
static bool
fa726te_sched_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep,
			   int * cost)
{
  /* For FA726TE, true dependency on CPSR (i.e. set cond followed by predicated)
     have penalty of 3.  */
  if (dep_type == REG_DEP_TRUE
      && recog_memoized (insn) >= 0
      && recog_memoized (dep) >= 0
      && get_attr_conds (dep) == CONDS_SET)
    {
      /* Use of carry (e.g. 64-bit arithmetic) in ALU: 3-cycle latency.  */
      if (get_attr_conds (insn) == CONDS_USE
          && get_attr_type (insn) != TYPE_BRANCH)
        {
          *cost = 3;
          return false;
        }

      if (GET_CODE (PATTERN (insn)) == COND_EXEC
          || get_attr_conds (insn) == CONDS_USE)
        {
          *cost = 0;
          return false;
        }
    }

  return true;
}

/* Implement TARGET_REGISTER_MOVE_COST.

   Moves between VFP_REGS and GENERAL_REGS are a single insn, but
   it is typically more expensive than a single memory access.  We set
   the cost to less than two memory accesses so that floating
   point to integer conversion does not go through memory.  */

int
arm_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			reg_class_t from, reg_class_t to)
{
  if (TARGET_32BIT)
    {
      if ((IS_VFP_CLASS (from) && !IS_VFP_CLASS (to))
	  || (!IS_VFP_CLASS (from) && IS_VFP_CLASS (to)))
	return 15;
      else if ((from == IWMMXT_REGS && to != IWMMXT_REGS)
	       || (from != IWMMXT_REGS && to == IWMMXT_REGS))
	return 4;
      else if (from == IWMMXT_GR_REGS || to == IWMMXT_GR_REGS)
	return 20;
      else
	return 2;
    }
  else
    {
      if (from == HI_REGS || to == HI_REGS)
	return 4;
      else
	return 2;
    }
}

/* Implement TARGET_MEMORY_MOVE_COST.  */

int
arm_memory_move_cost (machine_mode mode, reg_class_t rclass,
		      bool in ATTRIBUTE_UNUSED)
{
  if (TARGET_32BIT)
    return 10;
  else
    {
      if (GET_MODE_SIZE (mode) < 4)
	return 8;
      else
	return ((2 * GET_MODE_SIZE (mode)) * (rclass == LO_REGS ? 1 : 2));
    }
}

/* Vectorizer cost model implementation.  */

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int
arm_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
				tree vectype,
				int misalign ATTRIBUTE_UNUSED)
{
  unsigned elements;

  switch (type_of_cost)
    {
      case scalar_stmt:
        return current_tune->vec_costs->scalar_stmt_cost;

      case scalar_load:
        return current_tune->vec_costs->scalar_load_cost;

      case scalar_store:
        return current_tune->vec_costs->scalar_store_cost;

      case vector_stmt:
        return current_tune->vec_costs->vec_stmt_cost;

      case vector_load:
        return current_tune->vec_costs->vec_align_load_cost;

      case vector_store:
        return current_tune->vec_costs->vec_store_cost;

      case vec_to_scalar:
        return current_tune->vec_costs->vec_to_scalar_cost;

      case scalar_to_vec:
        return current_tune->vec_costs->scalar_to_vec_cost;

      case unaligned_load:
      case vector_gather_load:
        return current_tune->vec_costs->vec_unalign_load_cost;

      case unaligned_store:
      case vector_scatter_store:
        return current_tune->vec_costs->vec_unalign_store_cost;

      case cond_branch_taken:
        return current_tune->vec_costs->cond_taken_branch_cost;

      case cond_branch_not_taken:
        return current_tune->vec_costs->cond_not_taken_branch_cost;

      case vec_perm:
      case vec_promote_demote:
        return current_tune->vec_costs->vec_stmt_cost;

      case vec_construct:
	elements = TYPE_VECTOR_SUBPARTS (vectype);
	return elements / 2 + 1;

      default:
        gcc_unreachable ();
    }
}

/* Implement targetm.vectorize.add_stmt_cost.  */

static unsigned
arm_add_stmt_cost (void *data, int count, enum vect_cost_for_stmt kind,
		   struct _stmt_vec_info *stmt_info, int misalign,
		   enum vect_cost_model_location where)
{
  unsigned *cost = (unsigned *) data;
  unsigned retval = 0;

  if (flag_vect_cost_model)
    {
      tree vectype = stmt_info ? stmt_vectype (stmt_info) : NULL_TREE;
      int stmt_cost = arm_builtin_vectorization_cost (kind, vectype, misalign);

      /* Statements in an inner loop relative to the loop being
	 vectorized are weighted more heavily.  The value here is
	 arbitrary and could potentially be improved with analysis.  */
      if (where == vect_body && stmt_info && stmt_in_inner_loop_p (stmt_info))
	count *= 50;  /* FIXME.  */

      retval = (unsigned) (count * stmt_cost);
      cost[where] += retval;
    }

  return retval;
}

/* Return true if and only if this insn can dual-issue only as older.  */
static bool
cortexa7_older_only (rtx_insn *insn)
{
  if (recog_memoized (insn) < 0)
    return false;

  switch (get_attr_type (insn))
    {
    case TYPE_ALU_DSP_REG:
    case TYPE_ALU_SREG:
    case TYPE_ALUS_SREG:
    case TYPE_LOGIC_REG:
    case TYPE_LOGICS_REG:
    case TYPE_ADC_REG:
    case TYPE_ADCS_REG:
    case TYPE_ADR:
    case TYPE_BFM:
    case TYPE_REV:
    case TYPE_MVN_REG:
    case TYPE_SHIFT_IMM:
    case TYPE_SHIFT_REG:
    case TYPE_LOAD_BYTE:
    case TYPE_LOAD_4:
    case TYPE_STORE_4:
    case TYPE_FFARITHS:
    case TYPE_FADDS:
    case TYPE_FFARITHD:
    case TYPE_FADDD:
    case TYPE_FMOV:
    case TYPE_F_CVT:
    case TYPE_FCMPS:
    case TYPE_FCMPD:
    case TYPE_FCONSTS:
    case TYPE_FCONSTD:
    case TYPE_FMULS:
    case TYPE_FMACS:
    case TYPE_FMULD:
    case TYPE_FMACD:
    case TYPE_FDIVS:
    case TYPE_FDIVD:
    case TYPE_F_MRC:
    case TYPE_F_MRRC:
    case TYPE_F_FLAG:
    case TYPE_F_LOADS:
    case TYPE_F_STORES:
      return true;
    default:
      return false;
    }
}

/* Return true if and only if this insn can dual-issue as younger.  */
static bool
cortexa7_younger (FILE *file, int verbose, rtx_insn *insn)
{
  if (recog_memoized (insn) < 0)
    {
      if (verbose > 5)
        fprintf (file, ";; not cortexa7_younger %d\n", INSN_UID (insn));
      return false;
    }

  switch (get_attr_type (insn))
    {
    case TYPE_ALU_IMM:
    case TYPE_ALUS_IMM:
    case TYPE_LOGIC_IMM:
    case TYPE_LOGICS_IMM:
    case TYPE_EXTEND:
    case TYPE_MVN_IMM:
    case TYPE_MOV_IMM:
    case TYPE_MOV_REG:
    case TYPE_MOV_SHIFT:
    case TYPE_MOV_SHIFT_REG:
    case TYPE_BRANCH:
    case TYPE_CALL:
      return true;
    default:
      return false;
    }
}


/* Look for an instruction that can dual issue only as an older
   instruction, and move it in front of any instructions that can
   dual-issue as younger, while preserving the relative order of all
   other instructions in the ready list.  This is a hueuristic to help
   dual-issue in later cycles, by postponing issue of more flexible
   instructions.  This heuristic may affect dual issue opportunities
   in the current cycle.  */
static void
cortexa7_sched_reorder (FILE *file, int verbose, rtx_insn **ready,
			int *n_readyp, int clock)
{
  int i;
  int first_older_only = -1, first_younger = -1;

  if (verbose > 5)
    fprintf (file,
             ";; sched_reorder for cycle %d with %d insns in ready list\n",
             clock,
             *n_readyp);

  /* Traverse the ready list from the head (the instruction to issue
     first), and looking for the first instruction that can issue as
     younger and the first instruction that can dual-issue only as
     older.  */
  for (i = *n_readyp - 1; i >= 0; i--)
    {
      rtx_insn *insn = ready[i];
      if (cortexa7_older_only (insn))
        {
          first_older_only = i;
          if (verbose > 5)
            fprintf (file, ";; reorder older found %d\n", INSN_UID (insn));
          break;
        }
      else if (cortexa7_younger (file, verbose, insn) && first_younger == -1)
        first_younger = i;
    }

  /* Nothing to reorder because either no younger insn found or insn
     that can dual-issue only as older appears before any insn that
     can dual-issue as younger.  */
  if (first_younger == -1)
    {
      if (verbose > 5)
        fprintf (file, ";; sched_reorder nothing to reorder as no younger\n");
      return;
    }

  /* Nothing to reorder because no older-only insn in the ready list.  */
  if (first_older_only == -1)
    {
      if (verbose > 5)
        fprintf (file, ";; sched_reorder nothing to reorder as no older_only\n");
      return;
    }

  /* Move first_older_only insn before first_younger.  */
  if (verbose > 5)
    fprintf (file, ";; cortexa7_sched_reorder insn %d before %d\n",
             INSN_UID(ready [first_older_only]),
             INSN_UID(ready [first_younger]));
  rtx_insn *first_older_only_insn = ready [first_older_only];
  for (i = first_older_only; i < first_younger; i++)
    {
      ready[i] = ready[i+1];
    }

  ready[i] = first_older_only_insn;
  return;
}

/* Implement TARGET_SCHED_REORDER. */
static int
arm_sched_reorder (FILE *file, int verbose, rtx_insn **ready, int *n_readyp,
                   int clock)
{
  switch (arm_tune)
    {
    case TARGET_CPU_cortexa7:
      cortexa7_sched_reorder (file, verbose, ready, n_readyp, clock);
      break;
    default:
      /* Do nothing for other cores.  */
      break;
    }

  return arm_issue_rate ();
}

/* This function implements the target macro TARGET_SCHED_ADJUST_COST.
   It corrects the value of COST based on the relationship between
   INSN and DEP through the dependence LINK.  It returns the new
   value. There is a per-core adjust_cost hook to adjust scheduler costs
   and the per-core hook can choose to completely override the generic
   adjust_cost function. Only put bits of code into arm_adjust_cost that
   are common across all cores.  */
static int
arm_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep, int cost,
		 unsigned int)
{
  rtx i_pat, d_pat;

 /* When generating Thumb-1 code, we want to place flag-setting operations
    close to a conditional branch which depends on them, so that we can
    omit the comparison. */
  if (TARGET_THUMB1
      && dep_type == 0
      && recog_memoized (insn) == CODE_FOR_cbranchsi4_insn
      && recog_memoized (dep) >= 0
      && get_attr_conds (dep) == CONDS_SET)
    return 0;

  if (current_tune->sched_adjust_cost != NULL)
    {
      if (!current_tune->sched_adjust_cost (insn, dep_type, dep, &cost))
	return cost;
    }

  /* XXX Is this strictly true?  */
  if (dep_type == REG_DEP_ANTI
      || dep_type == REG_DEP_OUTPUT)
    return 0;

  /* Call insns don't incur a stall, even if they follow a load.  */
  if (dep_type == 0
      && CALL_P (insn))
    return 1;

  if ((i_pat = single_set (insn)) != NULL
      && MEM_P (SET_SRC (i_pat))
      && (d_pat = single_set (dep)) != NULL
      && MEM_P (SET_DEST (d_pat)))
    {
      rtx src_mem = XEXP (SET_SRC (i_pat), 0);
      /* This is a load after a store, there is no conflict if the load reads
	 from a cached area.  Assume that loads from the stack, and from the
	 constant pool are cached, and that others will miss.  This is a
	 hack.  */

      if ((GET_CODE (src_mem) == SYMBOL_REF
	   && CONSTANT_POOL_ADDRESS_P (src_mem))
	  || reg_mentioned_p (stack_pointer_rtx, src_mem)
	  || reg_mentioned_p (frame_pointer_rtx, src_mem)
	  || reg_mentioned_p (hard_frame_pointer_rtx, src_mem))
	return 1;
    }

  return cost;
}

int
arm_max_conditional_execute (void)
{
  return max_insns_skipped;
}

static int
arm_default_branch_cost (bool speed_p, bool predictable_p ATTRIBUTE_UNUSED)
{
  if (TARGET_32BIT)
    return (TARGET_THUMB2 && !speed_p) ? 1 : 4;
  else
    return (optimize > 0) ? 2 : 0;
}

static int
arm_cortex_a5_branch_cost (bool speed_p, bool predictable_p)
{
  return speed_p ? 0 : arm_default_branch_cost (speed_p, predictable_p);
}

/* Thumb-2 branches are relatively cheap on Cortex-M processors ("1 + P cycles"
   on Cortex-M4, where P varies from 1 to 3 according to some criteria), since
   sequences of non-executed instructions in IT blocks probably take the same
   amount of time as executed instructions (and the IT instruction itself takes
   space in icache).  This function was experimentally determined to give good
   results on a popular embedded benchmark.  */

static int
arm_cortex_m_branch_cost (bool speed_p, bool predictable_p)
{
  return (TARGET_32BIT && speed_p) ? 1
         : arm_default_branch_cost (speed_p, predictable_p);
}

static int
arm_cortex_m7_branch_cost (bool speed_p, bool predictable_p)
{
  return speed_p ? 0 : arm_default_branch_cost (speed_p, predictable_p);
}

static bool fp_consts_inited = false;

static REAL_VALUE_TYPE value_fp0;

static void
init_fp_table (void)
{
  REAL_VALUE_TYPE r;

  r = REAL_VALUE_ATOF ("0", DFmode);
  value_fp0 = r;
  fp_consts_inited = true;
}

/* Return TRUE if rtx X is a valid immediate FP constant.  */
int
arm_const_double_rtx (rtx x)
{
  const REAL_VALUE_TYPE *r;

  if (!fp_consts_inited)
    init_fp_table ();

  r = CONST_DOUBLE_REAL_VALUE (x);
  if (REAL_VALUE_MINUS_ZERO (*r))
    return 0;

  if (real_equal (r, &value_fp0))
    return 1;

  return 0;
}

/* VFPv3 has a fairly wide range of representable immediates, formed from
   "quarter-precision" floating-point values. These can be evaluated using this
   formula (with ^ for exponentiation):

     -1^s * n * 2^-r

   Where 's' is a sign bit (0/1), 'n' and 'r' are integers such that
   16 <= n <= 31 and 0 <= r <= 7.

   These values are mapped onto an 8-bit integer ABCDEFGH s.t.

     - A (most-significant) is the sign bit.
     - BCD are the exponent (encoded as r XOR 3).
     - EFGH are the mantissa (encoded as n - 16).
*/

/* Return an integer index for a VFPv3 immediate operand X suitable for the
   fconst[sd] instruction, or -1 if X isn't suitable.  */
static int
vfp3_const_double_index (rtx x)
{
  REAL_VALUE_TYPE r, m;
  int sign, exponent;
  unsigned HOST_WIDE_INT mantissa, mant_hi;
  unsigned HOST_WIDE_INT mask;
  int point_pos = 2 * HOST_BITS_PER_WIDE_INT - 1;
  bool fail;

  if (!TARGET_VFP3 || !CONST_DOUBLE_P (x))
    return -1;

  r = *CONST_DOUBLE_REAL_VALUE (x);

  /* We can't represent these things, so detect them first.  */
  if (REAL_VALUE_ISINF (r) || REAL_VALUE_ISNAN (r) || REAL_VALUE_MINUS_ZERO (r))
    return -1;

  /* Extract sign, exponent and mantissa.  */
  sign = REAL_VALUE_NEGATIVE (r) ? 1 : 0;
  r = real_value_abs (&r);
  exponent = REAL_EXP (&r);
  /* For the mantissa, we expand into two HOST_WIDE_INTS, apart from the
     highest (sign) bit, with a fixed binary point at bit point_pos.
     WARNING: If there's ever a VFP version which uses more than 2 * H_W_I - 1
     bits for the mantissa, this may fail (low bits would be lost).  */
  real_ldexp (&m, &r, point_pos - exponent);
  wide_int w = real_to_integer (&m, &fail, HOST_BITS_PER_WIDE_INT * 2);
  mantissa = w.elt (0);
  mant_hi = w.elt (1);

  /* If there are bits set in the low part of the mantissa, we can't
     represent this value.  */
  if (mantissa != 0)
    return -1;

  /* Now make it so that mantissa contains the most-significant bits, and move
     the point_pos to indicate that the least-significant bits have been
     discarded.  */
  point_pos -= HOST_BITS_PER_WIDE_INT;
  mantissa = mant_hi;

  /* We can permit four significant bits of mantissa only, plus a high bit
     which is always 1.  */
  mask = (HOST_WIDE_INT_1U << (point_pos - 5)) - 1;
  if ((mantissa & mask) != 0)
    return -1;

  /* Now we know the mantissa is in range, chop off the unneeded bits.  */
  mantissa >>= point_pos - 5;

  /* The mantissa may be zero. Disallow that case. (It's possible to load the
     floating-point immediate zero with Neon using an integer-zero load, but
     that case is handled elsewhere.)  */
  if (mantissa == 0)
    return -1;

  gcc_assert (mantissa >= 16 && mantissa <= 31);

  /* The value of 5 here would be 4 if GCC used IEEE754-like encoding (where
     normalized significands are in the range [1, 2). (Our mantissa is shifted
     left 4 places at this point relative to normalized IEEE754 values).  GCC
     internally uses [0.5, 1) (see real.c), so the exponent returned from
     REAL_EXP must be altered.  */
  exponent = 5 - exponent;

  if (exponent < 0 || exponent > 7)
    return -1;

  /* Sign, mantissa and exponent are now in the correct form to plug into the
     formula described in the comment above.  */
  return (sign << 7) | ((exponent ^ 3) << 4) | (mantissa - 16);
}

/* Return TRUE if rtx X is a valid immediate VFPv3 constant.  */
int
vfp3_const_double_rtx (rtx x)
{
  if (!TARGET_VFP3)
    return 0;

  return vfp3_const_double_index (x) != -1;
}

/* Recognize immediates which can be used in various Neon instructions. Legal
   immediates are described by the following table (for VMVN variants, the
   bitwise inverse of the constant shown is recognized. In either case, VMOV
   is output and the correct instruction to use for a given constant is chosen
   by the assembler). The constant shown is replicated across all elements of
   the destination vector.

   insn elems variant constant (binary)
   ---- ----- ------- -----------------
   vmov  i32     0    00000000 00000000 00000000 abcdefgh
   vmov  i32     1    00000000 00000000 abcdefgh 00000000
   vmov  i32     2    00000000 abcdefgh 00000000 00000000
   vmov  i32     3    abcdefgh 00000000 00000000 00000000
   vmov  i16     4    00000000 abcdefgh
   vmov  i16     5    abcdefgh 00000000
   vmvn  i32     6    00000000 00000000 00000000 abcdefgh
   vmvn  i32     7    00000000 00000000 abcdefgh 00000000
   vmvn  i32     8    00000000 abcdefgh 00000000 00000000
   vmvn  i32     9    abcdefgh 00000000 00000000 00000000
   vmvn  i16    10    00000000 abcdefgh
   vmvn  i16    11    abcdefgh 00000000
   vmov  i32    12    00000000 00000000 abcdefgh 11111111
   vmvn  i32    13    00000000 00000000 abcdefgh 11111111
   vmov  i32    14    00000000 abcdefgh 11111111 11111111
   vmvn  i32    15    00000000 abcdefgh 11111111 11111111
   vmov   i8    16    abcdefgh
   vmov  i64    17    aaaaaaaa bbbbbbbb cccccccc dddddddd
                      eeeeeeee ffffffff gggggggg hhhhhhhh
   vmov  f32    18    aBbbbbbc defgh000 00000000 00000000
   vmov  f32    19    00000000 00000000 00000000 00000000

   For case 18, B = !b. Representable values are exactly those accepted by
   vfp3_const_double_index, but are output as floating-point numbers rather
   than indices.

   For case 19, we will change it to vmov.i32 when assembling.

   Variants 0-5 (inclusive) may also be used as immediates for the second
   operand of VORR/VBIC instructions.

   The INVERSE argument causes the bitwise inverse of the given operand to be
   recognized instead (used for recognizing legal immediates for the VAND/VORN
   pseudo-instructions). If INVERSE is true, the value placed in *MODCONST is
   *not* inverted (i.e. the pseudo-instruction forms vand/vorn should still be
   output, rather than the real insns vbic/vorr).

   INVERSE makes no difference to the recognition of float vectors.

   The return value is the variant of immediate as shown in the above table, or
   -1 if the given value doesn't match any of the listed patterns.
*/
static int
neon_valid_immediate (rtx op, machine_mode mode, int inverse,
		      rtx *modconst, int *elementwidth)
{
#define CHECK(STRIDE, ELSIZE, CLASS, TEST)	\
  matches = 1;					\
  for (i = 0; i < idx; i += (STRIDE))		\
    if (!(TEST))				\
      matches = 0;				\
  if (matches)					\
    {						\
      immtype = (CLASS);			\
      elsize = (ELSIZE);			\
      break;					\
    }

  unsigned int i, elsize = 0, idx = 0, n_elts;
  unsigned int innersize;
  unsigned char bytes[16];
  int immtype = -1, matches;
  unsigned int invmask = inverse ? 0xff : 0;
  bool vector = GET_CODE (op) == CONST_VECTOR;

  if (vector)
    n_elts = CONST_VECTOR_NUNITS (op);
  else
    {
      n_elts = 1;
      if (mode == VOIDmode)
	mode = DImode;
    }

  innersize = GET_MODE_UNIT_SIZE (mode);

  /* Vectors of float constants.  */
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
    {
      rtx el0 = CONST_VECTOR_ELT (op, 0);

      if (!vfp3_const_double_rtx (el0) && el0 != CONST0_RTX (GET_MODE (el0)))
        return -1;

      /* FP16 vectors cannot be represented.  */
      if (GET_MODE_INNER (mode) == HFmode)
	return -1;

      /* All elements in the vector must be the same.  Note that 0.0 and -0.0
	 are distinct in this context.  */
      if (!const_vec_duplicate_p (op))
	return -1;

      if (modconst)
        *modconst = CONST_VECTOR_ELT (op, 0);

      if (elementwidth)
        *elementwidth = 0;

      if (el0 == CONST0_RTX (GET_MODE (el0)))
	return 19;
      else
	return 18;
    }

  /* The tricks done in the code below apply for little-endian vector layout.
     For big-endian vectors only allow vectors of the form { a, a, a..., a }.
     FIXME: Implement logic for big-endian vectors.  */
  if (BYTES_BIG_ENDIAN && vector && !const_vec_duplicate_p (op))
    return -1;

  /* Splat vector constant out into a byte vector.  */
  for (i = 0; i < n_elts; i++)
    {
      rtx el = vector ? CONST_VECTOR_ELT (op, i) : op;
      unsigned HOST_WIDE_INT elpart;

      gcc_assert (CONST_INT_P (el));
      elpart = INTVAL (el);

      for (unsigned int byte = 0; byte < innersize; byte++)
	{
	  bytes[idx++] = (elpart & 0xff) ^ invmask;
	  elpart >>= BITS_PER_UNIT;
	}
    }

  /* Sanity check.  */
  gcc_assert (idx == GET_MODE_SIZE (mode));

  do
    {
      CHECK (4, 32, 0, bytes[i] == bytes[0] && bytes[i + 1] == 0
		       && bytes[i + 2] == 0 && bytes[i + 3] == 0);

      CHECK (4, 32, 1, bytes[i] == 0 && bytes[i + 1] == bytes[1]
		       && bytes[i + 2] == 0 && bytes[i + 3] == 0);

      CHECK (4, 32, 2, bytes[i] == 0 && bytes[i + 1] == 0
		       && bytes[i + 2] == bytes[2] && bytes[i + 3] == 0);

      CHECK (4, 32, 3, bytes[i] == 0 && bytes[i + 1] == 0
		       && bytes[i + 2] == 0 && bytes[i + 3] == bytes[3]);

      CHECK (2, 16, 4, bytes[i] == bytes[0] && bytes[i + 1] == 0);

      CHECK (2, 16, 5, bytes[i] == 0 && bytes[i + 1] == bytes[1]);

      CHECK (4, 32, 6, bytes[i] == bytes[0] && bytes[i + 1] == 0xff
		       && bytes[i + 2] == 0xff && bytes[i + 3] == 0xff);

      CHECK (4, 32, 7, bytes[i] == 0xff && bytes[i + 1] == bytes[1]
		       && bytes[i + 2] == 0xff && bytes[i + 3] == 0xff);

      CHECK (4, 32, 8, bytes[i] == 0xff && bytes[i + 1] == 0xff
		       && bytes[i + 2] == bytes[2] && bytes[i + 3] == 0xff);

      CHECK (4, 32, 9, bytes[i] == 0xff && bytes[i + 1] == 0xff
		       && bytes[i + 2] == 0xff && bytes[i + 3] == bytes[3]);

      CHECK (2, 16, 10, bytes[i] == bytes[0] && bytes[i + 1] == 0xff);

      CHECK (2, 16, 11, bytes[i] == 0xff && bytes[i + 1] == bytes[1]);

      CHECK (4, 32, 12, bytes[i] == 0xff && bytes[i + 1] == bytes[1]
			&& bytes[i + 2] == 0 && bytes[i + 3] == 0);

      CHECK (4, 32, 13, bytes[i] == 0 && bytes[i + 1] == bytes[1]
			&& bytes[i + 2] == 0xff && bytes[i + 3] == 0xff);

      CHECK (4, 32, 14, bytes[i] == 0xff && bytes[i + 1] == 0xff
			&& bytes[i + 2] == bytes[2] && bytes[i + 3] == 0);

      CHECK (4, 32, 15, bytes[i] == 0 && bytes[i + 1] == 0
			&& bytes[i + 2] == bytes[2] && bytes[i + 3] == 0xff);

      CHECK (1, 8, 16, bytes[i] == bytes[0]);

      CHECK (1, 64, 17, (bytes[i] == 0 || bytes[i] == 0xff)
			&& bytes[i] == bytes[(i + 8) % idx]);
    }
  while (0);

  if (immtype == -1)
    return -1;

  if (elementwidth)
    *elementwidth = elsize;

  if (modconst)
    {
      unsigned HOST_WIDE_INT imm = 0;

      /* Un-invert bytes of recognized vector, if necessary.  */
      if (invmask != 0)
        for (i = 0; i < idx; i++)
          bytes[i] ^= invmask;

      if (immtype == 17)
        {
          /* FIXME: Broken on 32-bit H_W_I hosts.  */
          gcc_assert (sizeof (HOST_WIDE_INT) == 8);

          for (i = 0; i < 8; i++)
            imm |= (unsigned HOST_WIDE_INT) (bytes[i] ? 0xff : 0)
                   << (i * BITS_PER_UNIT);

          *modconst = GEN_INT (imm);
        }
      else
        {
          unsigned HOST_WIDE_INT imm = 0;

          for (i = 0; i < elsize / BITS_PER_UNIT; i++)
            imm |= (unsigned HOST_WIDE_INT) bytes[i] << (i * BITS_PER_UNIT);

          *modconst = GEN_INT (imm);
        }
    }

  return immtype;
#undef CHECK
}

/* Return TRUE if rtx X is legal for use as either a Neon VMOV (or, implicitly,
   VMVN) immediate. Write back width per element to *ELEMENTWIDTH (or zero for
   float elements), and a modified constant (whatever should be output for a
   VMOV) in *MODCONST.  */

int
neon_immediate_valid_for_move (rtx op, machine_mode mode,
			       rtx *modconst, int *elementwidth)
{
  rtx tmpconst;
  int tmpwidth;
  int retval = neon_valid_immediate (op, mode, 0, &tmpconst, &tmpwidth);

  if (retval == -1)
    return 0;

  if (modconst)
    *modconst = tmpconst;

  if (elementwidth)
    *elementwidth = tmpwidth;

  return 1;
}

/* Return TRUE if rtx X is legal for use in a VORR or VBIC instruction.  If
   the immediate is valid, write a constant suitable for using as an operand
   to VORR/VBIC/VAND/VORN to *MODCONST and the corresponding element width to
   *ELEMENTWIDTH. See neon_valid_immediate for description of INVERSE.  */

int
neon_immediate_valid_for_logic (rtx op, machine_mode mode, int inverse,
				rtx *modconst, int *elementwidth)
{
  rtx tmpconst;
  int tmpwidth;
  int retval = neon_valid_immediate (op, mode, inverse, &tmpconst, &tmpwidth);

  if (retval < 0 || retval > 5)
    return 0;

  if (modconst)
    *modconst = tmpconst;

  if (elementwidth)
    *elementwidth = tmpwidth;

  return 1;
}

/* Return TRUE if rtx OP is legal for use in a VSHR or VSHL instruction.  If
   the immediate is valid, write a constant suitable for using as an operand
   to VSHR/VSHL to *MODCONST and the corresponding element width to
   *ELEMENTWIDTH. ISLEFTSHIFT is for determine left or right shift,
   because they have different limitations.  */

int
neon_immediate_valid_for_shift (rtx op, machine_mode mode,
				rtx *modconst, int *elementwidth,
				bool isleftshift)
{
  unsigned int innersize = GET_MODE_UNIT_SIZE (mode);
  unsigned int n_elts = CONST_VECTOR_NUNITS (op), i;
  unsigned HOST_WIDE_INT last_elt = 0;
  unsigned HOST_WIDE_INT maxshift;

  /* Split vector constant out into a byte vector.  */
  for (i = 0; i < n_elts; i++)
    {
      rtx el = CONST_VECTOR_ELT (op, i);
      unsigned HOST_WIDE_INT elpart;

      if (CONST_INT_P (el))
        elpart = INTVAL (el);
      else if (CONST_DOUBLE_P (el))
        return 0;
      else
        gcc_unreachable ();

      if (i != 0 && elpart != last_elt)
        return 0;

      last_elt = elpart;
    }

  /* Shift less than element size.  */
  maxshift = innersize * 8;

  if (isleftshift)
    {
      /* Left shift immediate value can be from 0 to <size>-1.  */
      if (last_elt >= maxshift)
        return 0;
    }
  else
    {
      /* Right shift immediate value can be from 1 to <size>.  */
      if (last_elt == 0 || last_elt > maxshift)
	return 0;
    }

  if (elementwidth)
    *elementwidth = innersize * 8;

  if (modconst)
    *modconst = CONST_VECTOR_ELT (op, 0);

  return 1;
}

/* Return a string suitable for output of Neon immediate logic operation
   MNEM.  */

char *
neon_output_logic_immediate (const char *mnem, rtx *op2, machine_mode mode,
			     int inverse, int quad)
{
  int width, is_valid;
  static char templ[40];

  is_valid = neon_immediate_valid_for_logic (*op2, mode, inverse, op2, &width);

  gcc_assert (is_valid != 0);

  if (quad)
    sprintf (templ, "%s.i%d\t%%q0, %%2", mnem, width);
  else
    sprintf (templ, "%s.i%d\t%%P0, %%2", mnem, width);

  return templ;
}

/* Return a string suitable for output of Neon immediate shift operation
   (VSHR or VSHL) MNEM.  */

char *
neon_output_shift_immediate (const char *mnem, char sign, rtx *op2,
			     machine_mode mode, int quad,
			     bool isleftshift)
{
  int width, is_valid;
  static char templ[40];

  is_valid = neon_immediate_valid_for_shift (*op2, mode, op2, &width, isleftshift);
  gcc_assert (is_valid != 0);

  if (quad)
    sprintf (templ, "%s.%c%d\t%%q0, %%q1, %%2", mnem, sign, width);
  else
    sprintf (templ, "%s.%c%d\t%%P0, %%P1, %%2", mnem, sign, width);

  return templ;
}

/* Output a sequence of pairwise operations to implement a reduction.
   NOTE: We do "too much work" here, because pairwise operations work on two
   registers-worth of operands in one go. Unfortunately we can't exploit those
   extra calculations to do the full operation in fewer steps, I don't think.
   Although all vector elements of the result but the first are ignored, we
   actually calculate the same result in each of the elements. An alternative
   such as initially loading a vector with zero to use as each of the second
   operands would use up an additional register and take an extra instruction,
   for no particular gain.  */

void
neon_pairwise_reduce (rtx op0, rtx op1, machine_mode mode,
		      rtx (*reduc) (rtx, rtx, rtx))
{
  unsigned int i, parts = GET_MODE_SIZE (mode) / GET_MODE_UNIT_SIZE (mode);
  rtx tmpsum = op1;

  for (i = parts / 2; i >= 1; i /= 2)
    {
      rtx dest = (i == 1) ? op0 : gen_reg_rtx (mode);
      emit_insn (reduc (dest, tmpsum, tmpsum));
      tmpsum = dest;
    }
}

/* If VALS is a vector constant that can be loaded into a register
   using VDUP, generate instructions to do so and return an RTX to
   assign to the register.  Otherwise return NULL_RTX.  */

static rtx
neon_vdup_constant (rtx vals)
{
  machine_mode mode = GET_MODE (vals);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx x;

  if (GET_CODE (vals) != CONST_VECTOR || GET_MODE_SIZE (inner_mode) > 4)
    return NULL_RTX;

  if (!const_vec_duplicate_p (vals, &x))
    /* The elements are not all the same.  We could handle repeating
       patterns of a mode larger than INNER_MODE here (e.g. int8x8_t
       {0, C, 0, C, 0, C, 0, C} which can be loaded using
       vdup.i16).  */
    return NULL_RTX;

  /* We can load this constant by using VDUP and a constant in a
     single ARM register.  This will be cheaper than a vector
     load.  */

  x = copy_to_mode_reg (inner_mode, x);
  return gen_vec_duplicate (mode, x);
}

/* Generate code to load VALS, which is a PARALLEL containing only
   constants (for vec_init) or CONST_VECTOR, efficiently into a
   register.  Returns an RTX to copy into the register, or NULL_RTX
   for a PARALLEL that can not be converted into a CONST_VECTOR.  */

rtx
neon_make_constant (rtx vals)
{
  machine_mode mode = GET_MODE (vals);
  rtx target;
  rtx const_vec = NULL_RTX;
  int n_elts = GET_MODE_NUNITS (mode);
  int n_const = 0;
  int i;

  if (GET_CODE (vals) == CONST_VECTOR)
    const_vec = vals;
  else if (GET_CODE (vals) == PARALLEL)
    {
      /* A CONST_VECTOR must contain only CONST_INTs and
	 CONST_DOUBLEs, but CONSTANT_P allows more (e.g. SYMBOL_REF).
	 Only store valid constants in a CONST_VECTOR.  */
      for (i = 0; i < n_elts; ++i)
	{
	  rtx x = XVECEXP (vals, 0, i);
	  if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	    n_const++;
	}
      if (n_const == n_elts)
	const_vec = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
    }
  else
    gcc_unreachable ();

  if (const_vec != NULL
      && neon_immediate_valid_for_move (const_vec, mode, NULL, NULL))
    /* Load using VMOV.  On Cortex-A8 this takes one cycle.  */
    return const_vec;
  else if ((target = neon_vdup_constant (vals)) != NULL_RTX)
    /* Loaded using VDUP.  On Cortex-A8 the VDUP takes one NEON
       pipeline cycle; creating the constant takes one or two ARM
       pipeline cycles.  */
    return target;
  else if (const_vec != NULL_RTX)
    /* Load from constant pool.  On Cortex-A8 this takes two cycles
       (for either double or quad vectors).  We can not take advantage
       of single-cycle VLD1 because we need a PC-relative addressing
       mode.  */
    return const_vec;
  else
    /* A PARALLEL containing something not valid inside CONST_VECTOR.
       We can not construct an initializer.  */
    return NULL_RTX;
}

/* Initialize vector TARGET to VALS.  */

void
neon_expand_vector_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  int n_var = 0, one_var = -1;
  bool all_same = true;
  rtx x, mem;
  int i;

  for (i = 0; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);
      if (!CONSTANT_P (x))
	++n_var, one_var = i;

      if (i > 0 && !rtx_equal_p (x, XVECEXP (vals, 0, 0)))
	all_same = false;
    }

  if (n_var == 0)
    {
      rtx constant = neon_make_constant (vals);
      if (constant != NULL_RTX)
	{
	  emit_move_insn (target, constant);
	  return;
	}
    }

  /* Splat a single non-constant element if we can.  */
  if (all_same && GET_MODE_SIZE (inner_mode) <= 4)
    {
      x = copy_to_mode_reg (inner_mode, XVECEXP (vals, 0, 0));
      emit_insn (gen_rtx_SET (target, gen_vec_duplicate (mode, x)));
      return;
    }

  /* One field is non-constant.  Load constant then overwrite varying
     field.  This is more efficient than using the stack.  */
  if (n_var == 1)
    {
      rtx copy = copy_rtx (vals);
      rtx index = GEN_INT (one_var);

      /* Load constant part of vector, substitute neighboring value for
	 varying element.  */
      XVECEXP (copy, 0, one_var) = XVECEXP (vals, 0, (one_var + 1) % n_elts);
      neon_expand_vector_init (target, copy);

      /* Insert variable.  */
      x = copy_to_mode_reg (inner_mode, XVECEXP (vals, 0, one_var));
      switch (mode)
	{
	case E_V8QImode:
	  emit_insn (gen_neon_vset_lanev8qi (target, x, target, index));
	  break;
	case E_V16QImode:
	  emit_insn (gen_neon_vset_lanev16qi (target, x, target, index));
	  break;
	case E_V4HImode:
	  emit_insn (gen_neon_vset_lanev4hi (target, x, target, index));
	  break;
	case E_V8HImode:
	  emit_insn (gen_neon_vset_lanev8hi (target, x, target, index));
	  break;
	case E_V2SImode:
	  emit_insn (gen_neon_vset_lanev2si (target, x, target, index));
	  break;
	case E_V4SImode:
	  emit_insn (gen_neon_vset_lanev4si (target, x, target, index));
	  break;
	case E_V2SFmode:
	  emit_insn (gen_neon_vset_lanev2sf (target, x, target, index));
	  break;
	case E_V4SFmode:
	  emit_insn (gen_neon_vset_lanev4sf (target, x, target, index));
	  break;
	case E_V2DImode:
	  emit_insn (gen_neon_vset_lanev2di (target, x, target, index));
	  break;
	default:
	  gcc_unreachable ();
	}
      return;
    }

  /* Construct the vector in memory one field at a time
     and load the whole vector.  */
  mem = assign_stack_temp (mode, GET_MODE_SIZE (mode));
  for (i = 0; i < n_elts; i++)
    emit_move_insn (adjust_address_nv (mem, inner_mode,
				    i * GET_MODE_SIZE (inner_mode)),
		    XVECEXP (vals, 0, i));
  emit_move_insn (target, mem);
}

/* Ensure OPERAND lies between LOW (inclusive) and HIGH (exclusive).  Raise
   ERR if it doesn't.  EXP indicates the source location, which includes the
   inlining history for intrinsics.  */

static void
bounds_check (rtx operand, HOST_WIDE_INT low, HOST_WIDE_INT high,
	      const_tree exp, const char *desc)
{
  HOST_WIDE_INT lane;

  gcc_assert (CONST_INT_P (operand));

  lane = INTVAL (operand);

  if (lane < low || lane >= high)
    {
      if (exp)
	error ("%K%s %wd out of range %wd - %wd",
	       exp, desc, lane, low, high - 1);
      else
	error ("%s %wd out of range %wd - %wd", desc, lane, low, high - 1);
    }
}

/* Bounds-check lanes.  */

void
neon_lane_bounds (rtx operand, HOST_WIDE_INT low, HOST_WIDE_INT high,
		  const_tree exp)
{
  bounds_check (operand, low, high, exp, "lane");
}

/* Bounds-check constants.  */

void
arm_const_bounds (rtx operand, HOST_WIDE_INT low, HOST_WIDE_INT high)
{
  bounds_check (operand, low, high, NULL_TREE, "constant");
}

HOST_WIDE_INT
neon_element_bits (machine_mode mode)
{
  return GET_MODE_UNIT_BITSIZE (mode);
}


/* Predicates for `match_operand' and `match_operator'.  */

/* Return TRUE if OP is a valid coprocessor memory address pattern.
   WB is true if full writeback address modes are allowed and is false
   if limited writeback address modes (POST_INC and PRE_DEC) are
   allowed.  */

int
arm_coproc_mem_operand (rtx op, bool wb)
{
  rtx ind;

  /* Reject eliminable registers.  */
  if (! (reload_in_progress || reload_completed || lra_in_progress)
      && (   reg_mentioned_p (frame_pointer_rtx, op)
	  || reg_mentioned_p (arg_pointer_rtx, op)
	  || reg_mentioned_p (virtual_incoming_args_rtx, op)
	  || reg_mentioned_p (virtual_outgoing_args_rtx, op)
	  || reg_mentioned_p (virtual_stack_dynamic_rtx, op)
	  || reg_mentioned_p (virtual_stack_vars_rtx, op)))
    return FALSE;

  /* Constants are converted into offsets from labels.  */
  if (!MEM_P (op))
    return FALSE;

  ind = XEXP (op, 0);

  if (reload_completed
      && (GET_CODE (ind) == LABEL_REF
	  || (GET_CODE (ind) == CONST
	      && GET_CODE (XEXP (ind, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (ind, 0), 0)) == LABEL_REF
	      && CONST_INT_P (XEXP (XEXP (ind, 0), 1)))))
    return TRUE;

  /* Match: (mem (reg)).  */
  if (REG_P (ind))
    return arm_address_register_rtx_p (ind, 0);

  /* Autoincremment addressing modes.  POST_INC and PRE_DEC are
     acceptable in any case (subject to verification by
     arm_address_register_rtx_p).  We need WB to be true to accept
     PRE_INC and POST_DEC.  */
  if (GET_CODE (ind) == POST_INC
      || GET_CODE (ind) == PRE_DEC
      || (wb
	  && (GET_CODE (ind) == PRE_INC
	      || GET_CODE (ind) == POST_DEC)))
    return arm_address_register_rtx_p (XEXP (ind, 0), 0);

  if (wb
      && (GET_CODE (ind) == POST_MODIFY || GET_CODE (ind) == PRE_MODIFY)
      && arm_address_register_rtx_p (XEXP (ind, 0), 0)
      && GET_CODE (XEXP (ind, 1)) == PLUS
      && rtx_equal_p (XEXP (XEXP (ind, 1), 0), XEXP (ind, 0)))
    ind = XEXP (ind, 1);

  /* Match:
     (plus (reg)
	   (const)).  */
  if (GET_CODE (ind) == PLUS
      && REG_P (XEXP (ind, 0))
      && REG_MODE_OK_FOR_BASE_P (XEXP (ind, 0), VOIDmode)
      && CONST_INT_P (XEXP (ind, 1))
      && INTVAL (XEXP (ind, 1)) > -1024
      && INTVAL (XEXP (ind, 1)) <  1024
      && (INTVAL (XEXP (ind, 1)) & 3) == 0)
    return TRUE;

  return FALSE;
}

/* Return TRUE if OP is a memory operand which we can load or store a vector
   to/from. TYPE is one of the following values:
    0 - Vector load/stor (vldr)
    1 - Core registers (ldm)
    2 - Element/structure loads (vld1)
 */
int
neon_vector_mem_operand (rtx op, int type, bool strict)
{
  rtx ind;

  /* Reject eliminable registers.  */
  if (strict && ! (reload_in_progress || reload_completed)
      && (reg_mentioned_p (frame_pointer_rtx, op)
	  || reg_mentioned_p (arg_pointer_rtx, op)
	  || reg_mentioned_p (virtual_incoming_args_rtx, op)
	  || reg_mentioned_p (virtual_outgoing_args_rtx, op)
	  || reg_mentioned_p (virtual_stack_dynamic_rtx, op)
	  || reg_mentioned_p (virtual_stack_vars_rtx, op)))
    return FALSE;

  /* Constants are converted into offsets from labels.  */
  if (!MEM_P (op))
    return FALSE;

  ind = XEXP (op, 0);

  if (reload_completed
      && (GET_CODE (ind) == LABEL_REF
	  || (GET_CODE (ind) == CONST
	      && GET_CODE (XEXP (ind, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (ind, 0), 0)) == LABEL_REF
	      && CONST_INT_P (XEXP (XEXP (ind, 0), 1)))))
    return TRUE;

  /* Match: (mem (reg)).  */
  if (REG_P (ind))
    return arm_address_register_rtx_p (ind, 0);

  /* Allow post-increment with Neon registers.  */
  if ((type != 1 && GET_CODE (ind) == POST_INC)
      || (type == 0 && GET_CODE (ind) == PRE_DEC))
    return arm_address_register_rtx_p (XEXP (ind, 0), 0);

  /* Allow post-increment by register for VLDn */
  if (type == 2 && GET_CODE (ind) == POST_MODIFY
      && GET_CODE (XEXP (ind, 1)) == PLUS
      && REG_P (XEXP (XEXP (ind, 1), 1)))
     return true;

  /* Match:
     (plus (reg)
          (const)).  */
  if (type == 0
      && GET_CODE (ind) == PLUS
      && REG_P (XEXP (ind, 0))
      && REG_MODE_OK_FOR_BASE_P (XEXP (ind, 0), VOIDmode)
      && CONST_INT_P (XEXP (ind, 1))
      && INTVAL (XEXP (ind, 1)) > -1024
      /* For quad modes, we restrict the constant offset to be slightly less
	 than what the instruction format permits.  We have no such constraint
	 on double mode offsets.  (This must match arm_legitimate_index_p.)  */
      && (INTVAL (XEXP (ind, 1))
	  < (VALID_NEON_QREG_MODE (GET_MODE (op))? 1016 : 1024))
      && (INTVAL (XEXP (ind, 1)) & 3) == 0)
    return TRUE;

  return FALSE;
}

/* Return TRUE if OP is a mem suitable for loading/storing a Neon struct
   type.  */
int
neon_struct_mem_operand (rtx op)
{
  rtx ind;

  /* Reject eliminable registers.  */
  if (! (reload_in_progress || reload_completed)
      && (   reg_mentioned_p (frame_pointer_rtx, op)
	  || reg_mentioned_p (arg_pointer_rtx, op)
	  || reg_mentioned_p (virtual_incoming_args_rtx, op)
	  || reg_mentioned_p (virtual_outgoing_args_rtx, op)
	  || reg_mentioned_p (virtual_stack_dynamic_rtx, op)
	  || reg_mentioned_p (virtual_stack_vars_rtx, op)))
    return FALSE;

  /* Constants are converted into offsets from labels.  */
  if (!MEM_P (op))
    return FALSE;

  ind = XEXP (op, 0);

  if (reload_completed
      && (GET_CODE (ind) == LABEL_REF
	  || (GET_CODE (ind) == CONST
	      && GET_CODE (XEXP (ind, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (ind, 0), 0)) == LABEL_REF
	      && CONST_INT_P (XEXP (XEXP (ind, 0), 1)))))
    return TRUE;

  /* Match: (mem (reg)).  */
  if (REG_P (ind))
    return arm_address_register_rtx_p (ind, 0);

  /* vldm/vstm allows POST_INC (ia) and PRE_DEC (db).  */
  if (GET_CODE (ind) == POST_INC
      || GET_CODE (ind) == PRE_DEC)
    return arm_address_register_rtx_p (XEXP (ind, 0), 0);

  return FALSE;
}

/* Return true if X is a register that will be eliminated later on.  */
int
arm_eliminable_register (rtx x)
{
  return REG_P (x) && (REGNO (x) == FRAME_POINTER_REGNUM
		       || REGNO (x) == ARG_POINTER_REGNUM
		       || (REGNO (x) >= FIRST_VIRTUAL_REGISTER
			   && REGNO (x) <= LAST_VIRTUAL_REGISTER));
}

/* Return GENERAL_REGS if a scratch register required to reload x to/from
   coprocessor registers.  Otherwise return NO_REGS.  */

enum reg_class
coproc_secondary_reload_class (machine_mode mode, rtx x, bool wb)
{
  if (mode == HFmode)
    {
      if (!TARGET_NEON_FP16 && !TARGET_VFP_FP16INST)
	return GENERAL_REGS;
      if (s_register_operand (x, mode) || neon_vector_mem_operand (x, 2, true))
	return NO_REGS;
      return GENERAL_REGS;
    }

  /* The neon move patterns handle all legitimate vector and struct
     addresses.  */
  if (TARGET_NEON
      && (MEM_P (x) || GET_CODE (x) == CONST_VECTOR)
      && (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	  || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT
	  || VALID_NEON_STRUCT_MODE (mode)))
    return NO_REGS;

  if (arm_coproc_mem_operand (x, wb) || s_register_operand (x, mode))
    return NO_REGS;

  return GENERAL_REGS;
}

/* Values which must be returned in the most-significant end of the return
   register.  */

static bool
arm_return_in_msb (const_tree valtype)
{
  return (TARGET_AAPCS_BASED
          && BYTES_BIG_ENDIAN
	  && (AGGREGATE_TYPE_P (valtype)
	      || TREE_CODE (valtype) == COMPLEX_TYPE
	      || FIXED_POINT_TYPE_P (valtype)));
}

/* Return TRUE if X references a SYMBOL_REF.  */
int
symbol_mentioned_p (rtx x)
{
  const char * fmt;
  int i;

  if (GET_CODE (x) == SYMBOL_REF)
    return 1;

  /* UNSPEC_TLS entries for a symbol include the SYMBOL_REF, but they
     are constant offsets, not symbols.  */
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return 0;

  fmt = GET_RTX_FORMAT (GET_CODE (x));

  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (symbol_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && symbol_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

/* Return TRUE if X references a LABEL_REF.  */
int
label_mentioned_p (rtx x)
{
  const char * fmt;
  int i;

  if (GET_CODE (x) == LABEL_REF)
    return 1;

  /* UNSPEC_TLS entries for a symbol include a LABEL_REF for the referencing
     instruction, but they are constant offsets, not symbols.  */
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return 0;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (label_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && label_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

int
tls_mentioned_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
      return tls_mentioned_p (XEXP (x, 0));

    case UNSPEC:
      if (XINT (x, 1) == UNSPEC_TLS)
	return 1;

    /* Fall through.  */
    default:
      return 0;
    }
}

/* Must not copy any rtx that uses a pc-relative address.
   Also, disallow copying of load-exclusive instructions that
   may appear after splitting of compare-and-swap-style operations
   so as to prevent those loops from being transformed away from their
   canonical forms (see PR 69904).  */

static bool
arm_cannot_copy_insn_p (rtx_insn *insn)
{
  /* The tls call insn cannot be copied, as it is paired with a data
     word.  */
  if (recog_memoized (insn) == CODE_FOR_tlscall)
    return true;

  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), ALL)
    {
      const_rtx x = *iter;
      if (GET_CODE (x) == UNSPEC
	  && (XINT (x, 1) == UNSPEC_PIC_BASE
	      || XINT (x, 1) == UNSPEC_PIC_UNIFIED))
	return true;
    }

  rtx set = single_set (insn);
  if (set)
    {
      rtx src = SET_SRC (set);
      if (GET_CODE (src) == ZERO_EXTEND)
	src = XEXP (src, 0);

      /* Catch the load-exclusive and load-acquire operations.  */
      if (GET_CODE (src) == UNSPEC_VOLATILE
	  && (XINT (src, 1) == VUNSPEC_LL
	      || XINT (src, 1) == VUNSPEC_LAX))
	return true;
    }
  return false;
}

enum rtx_code
minmax_code (rtx x)
{
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case SMAX:
      return GE;
    case SMIN:
      return LE;
    case UMIN:
      return LEU;
    case UMAX:
      return GEU;
    default:
      gcc_unreachable ();
    }
}

/* Match pair of min/max operators that can be implemented via usat/ssat.  */

bool
arm_sat_operator_match (rtx lo_bound, rtx hi_bound,
			int *mask, bool *signed_sat)
{
  /* The high bound must be a power of two minus one.  */
  int log = exact_log2 (INTVAL (hi_bound) + 1);
  if (log == -1)
    return false;

  /* The low bound is either zero (for usat) or one less than the
     negation of the high bound (for ssat).  */
  if (INTVAL (lo_bound) == 0)
    {
      if (mask)
        *mask = log;
      if (signed_sat)
        *signed_sat = false;

      return true;
    }

  if (INTVAL (lo_bound) == -INTVAL (hi_bound) - 1)
    {
      if (mask)
        *mask = log + 1;
      if (signed_sat)
        *signed_sat = true;

      return true;
    }

  return false;
}

/* Return 1 if memory locations are adjacent.  */
int
adjacent_mem_locations (rtx a, rtx b)
{
  /* We don't guarantee to preserve the order of these memory refs.  */
  if (volatile_refs_p (a) || volatile_refs_p (b))
    return 0;

  if ((REG_P (XEXP (a, 0))
       || (GET_CODE (XEXP (a, 0)) == PLUS
	   && CONST_INT_P (XEXP (XEXP (a, 0), 1))))
      && (REG_P (XEXP (b, 0))
	  || (GET_CODE (XEXP (b, 0)) == PLUS
	      && CONST_INT_P (XEXP (XEXP (b, 0), 1)))))
    {
      HOST_WIDE_INT val0 = 0, val1 = 0;
      rtx reg0, reg1;
      int val_diff;

      if (GET_CODE (XEXP (a, 0)) == PLUS)
        {
	  reg0 = XEXP (XEXP (a, 0), 0);
	  val0 = INTVAL (XEXP (XEXP (a, 0), 1));
        }
      else
	reg0 = XEXP (a, 0);

      if (GET_CODE (XEXP (b, 0)) == PLUS)
        {
	  reg1 = XEXP (XEXP (b, 0), 0);
	  val1 = INTVAL (XEXP (XEXP (b, 0), 1));
        }
      else
	reg1 = XEXP (b, 0);

      /* Don't accept any offset that will require multiple
	 instructions to handle, since this would cause the
	 arith_adjacentmem pattern to output an overlong sequence.  */
      if (!const_ok_for_op (val0, PLUS) || !const_ok_for_op (val1, PLUS))
	return 0;

      /* Don't allow an eliminable register: register elimination can make
	 the offset too large.  */
      if (arm_eliminable_register (reg0))
	return 0;

      val_diff = val1 - val0;

      if (arm_ld_sched)
	{
	  /* If the target has load delay slots, then there's no benefit
	     to using an ldm instruction unless the offset is zero and
	     we are optimizing for size.  */
	  return (optimize_size && (REGNO (reg0) == REGNO (reg1))
		  && (val0 == 0 || val1 == 0 || val0 == 4 || val1 == 4)
		  && (val_diff == 4 || val_diff == -4));
	}

      return ((REGNO (reg0) == REGNO (reg1))
	      && (val_diff == 4 || val_diff == -4));
    }

  return 0;
}

/* Return true if OP is a valid load or store multiple operation.  LOAD is true
   for load operations, false for store operations.  CONSECUTIVE is true
   if the register numbers in the operation must be consecutive in the register
   bank. RETURN_PC is true if value is to be loaded in PC.
   The pattern we are trying to match for load is:
     [(SET (R_d0) (MEM (PLUS (addr) (offset))))
      (SET (R_d1) (MEM (PLUS (addr) (offset + <reg_increment>))))
       :
       :
      (SET (R_dn) (MEM (PLUS (addr) (offset + n * <reg_increment>))))
     ]
     where
     1.  If offset is 0, first insn should be (SET (R_d0) (MEM (src_addr))).
     2.  REGNO (R_d0) < REGNO (R_d1) < ... < REGNO (R_dn).
     3.  If consecutive is TRUE, then for kth register being loaded,
         REGNO (R_dk) = REGNO (R_d0) + k.
   The pattern for store is similar.  */
bool
ldm_stm_operation_p (rtx op, bool load, machine_mode mode,
                     bool consecutive, bool return_pc)
{
  HOST_WIDE_INT count = XVECLEN (op, 0);
  rtx reg, mem, addr;
  unsigned regno;
  unsigned first_regno;
  HOST_WIDE_INT i = 1, base = 0, offset = 0;
  rtx elt;
  bool addr_reg_in_reglist = false;
  bool update = false;
  int reg_increment;
  int offset_adj;
  int regs_per_val;

  /* If not in SImode, then registers must be consecutive
     (e.g., VLDM instructions for DFmode).  */
  gcc_assert ((mode == SImode) || consecutive);
  /* Setting return_pc for stores is illegal.  */
  gcc_assert (!return_pc || load);

  /* Set up the increments and the regs per val based on the mode.  */
  reg_increment = GET_MODE_SIZE (mode);
  regs_per_val = reg_increment / 4;
  offset_adj = return_pc ? 1 : 0;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, offset_adj)) != SET
      || (load && !REG_P (SET_DEST (XVECEXP (op, 0, offset_adj)))))
    return false;

  /* Check if this is a write-back.  */
  elt = XVECEXP (op, 0, offset_adj);
  if (GET_CODE (SET_SRC (elt)) == PLUS)
    {
      i++;
      base = 1;
      update = true;

      /* The offset adjustment must be the number of registers being
         popped times the size of a single register.  */
      if (!REG_P (SET_DEST (elt))
          || !REG_P (XEXP (SET_SRC (elt), 0))
          || (REGNO (SET_DEST (elt)) != REGNO (XEXP (SET_SRC (elt), 0)))
          || !CONST_INT_P (XEXP (SET_SRC (elt), 1))
          || INTVAL (XEXP (SET_SRC (elt), 1)) !=
             ((count - 1 - offset_adj) * reg_increment))
        return false;
    }

  i = i + offset_adj;
  base = base + offset_adj;
  /* Perform a quick check so we don't blow up below. If only one reg is loaded,
     success depends on the type: VLDM can do just one reg,
     LDM must do at least two.  */
  if ((count <= i) && (mode == SImode))
      return false;

  elt = XVECEXP (op, 0, i - 1);
  if (GET_CODE (elt) != SET)
    return false;

  if (load)
    {
      reg = SET_DEST (elt);
      mem = SET_SRC (elt);
    }
  else
    {
      reg = SET_SRC (elt);
      mem = SET_DEST (elt);
    }

  if (!REG_P (reg) || !MEM_P (mem))
    return false;

  regno = REGNO (reg);
  first_regno = regno;
  addr = XEXP (mem, 0);
  if (GET_CODE (addr) == PLUS)
    {
      if (!CONST_INT_P (XEXP (addr, 1)))
	return false;

      offset = INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);
    }

  if (!REG_P (addr))
    return false;

  /* Don't allow SP to be loaded unless it is also the base register. It
     guarantees that SP is reset correctly when an LDM instruction
     is interrupted. Otherwise, we might end up with a corrupt stack.  */
  if (load && (REGNO (reg) == SP_REGNUM) && (REGNO (addr) != SP_REGNUM))
    return false;

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);
      if (GET_CODE (elt) != SET)
        return false;

      if (load)
        {
          reg = SET_DEST (elt);
          mem = SET_SRC (elt);
        }
      else
        {
          reg = SET_SRC (elt);
          mem = SET_DEST (elt);
        }

      if (!REG_P (reg)
          || GET_MODE (reg) != mode
          || REGNO (reg) <= regno
          || (consecutive
              && (REGNO (reg) !=
                  (unsigned int) (first_regno + regs_per_val * (i - base))))
          /* Don't allow SP to be loaded unless it is also the base register. It
             guarantees that SP is reset correctly when an LDM instruction
             is interrupted. Otherwise, we might end up with a corrupt stack.  */
          || (load && (REGNO (reg) == SP_REGNUM) && (REGNO (addr) != SP_REGNUM))
          || !MEM_P (mem)
          || GET_MODE (mem) != mode
          || ((GET_CODE (XEXP (mem, 0)) != PLUS
	       || !rtx_equal_p (XEXP (XEXP (mem, 0), 0), addr)
	       || !CONST_INT_P (XEXP (XEXP (mem, 0), 1))
	       || (INTVAL (XEXP (XEXP (mem, 0), 1)) !=
                   offset + (i - base) * reg_increment))
	      && (!REG_P (XEXP (mem, 0))
		  || offset + (i - base) * reg_increment != 0)))
        return false;

      regno = REGNO (reg);
      if (regno == REGNO (addr))
        addr_reg_in_reglist = true;
    }

  if (load)
    {
      if (update && addr_reg_in_reglist)
        return false;

      /* For Thumb-1, address register is always modified - either by write-back
         or by explicit load.  If the pattern does not describe an update,
         then the address register must be in the list of loaded registers.  */
      if (TARGET_THUMB1)
        return update || addr_reg_in_reglist;
    }

  return true;
}

/* Return true iff it would be profitable to turn a sequence of NOPS loads
   or stores (depending on IS_STORE) into a load-multiple or store-multiple
   instruction.  ADD_OFFSET is nonzero if the base address register needs
   to be modified with an add instruction before we can use it.  */

static bool
multiple_operation_profitable_p (bool is_store ATTRIBUTE_UNUSED,
				 int nops, HOST_WIDE_INT add_offset)
 {
  /* For ARM8,9 & StrongARM, 2 ldr instructions are faster than an ldm
     if the offset isn't small enough.  The reason 2 ldrs are faster
     is because these ARMs are able to do more than one cache access
     in a single cycle.  The ARM9 and StrongARM have Harvard caches,
     whilst the ARM8 has a double bandwidth cache.  This means that
     these cores can do both an instruction fetch and a data fetch in
     a single cycle, so the trick of calculating the address into a
     scratch register (one of the result regs) and then doing a load
     multiple actually becomes slower (and no smaller in code size).
     That is the transformation

 	ldr	rd1, [rbase + offset]
 	ldr	rd2, [rbase + offset + 4]

     to

 	add	rd1, rbase, offset
 	ldmia	rd1, {rd1, rd2}

     produces worse code -- '3 cycles + any stalls on rd2' instead of
     '2 cycles + any stalls on rd2'.  On ARMs with only one cache
     access per cycle, the first sequence could never complete in less
     than 6 cycles, whereas the ldm sequence would only take 5 and
     would make better use of sequential accesses if not hitting the
     cache.

     We cheat here and test 'arm_ld_sched' which we currently know to
     only be true for the ARM8, ARM9 and StrongARM.  If this ever
     changes, then the test below needs to be reworked.  */
  if (nops == 2 && arm_ld_sched && add_offset != 0)
    return false;

  /* XScale has load-store double instructions, but they have stricter
     alignment requirements than load-store multiple, so we cannot
     use them.

     For XScale ldm requires 2 + NREGS cycles to complete and blocks
     the pipeline until completion.

	NREGS		CYCLES
	  1		  3
	  2		  4
	  3		  5
	  4		  6

     An ldr instruction takes 1-3 cycles, but does not block the
     pipeline.

	NREGS		CYCLES
	  1		 1-3
	  2		 2-6
	  3		 3-9
	  4		 4-12

     Best case ldr will always win.  However, the more ldr instructions
     we issue, the less likely we are to be able to schedule them well.
     Using ldr instructions also increases code size.

     As a compromise, we use ldr for counts of 1 or 2 regs, and ldm
     for counts of 3 or 4 regs.  */
  if (nops <= 2 && arm_tune_xscale && !optimize_size)
    return false;
  return true;
}

/* Subroutine of load_multiple_sequence and store_multiple_sequence.
   Given an array of UNSORTED_OFFSETS, of which there are NOPS, compute
   an array ORDER which describes the sequence to use when accessing the
   offsets that produces an ascending order.  In this sequence, each
   offset must be larger by exactly 4 than the previous one.  ORDER[0]
   must have been filled in with the lowest offset by the caller.
   If UNSORTED_REGS is nonnull, it is an array of register numbers that
   we use to verify that ORDER produces an ascending order of registers.
   Return true if it was possible to construct such an order, false if
   not.  */

static bool
compute_offset_order (int nops, HOST_WIDE_INT *unsorted_offsets, int *order,
		      int *unsorted_regs)
{
  int i;
  for (i = 1; i < nops; i++)
    {
      int j;

      order[i] = order[i - 1];
      for (j = 0; j < nops; j++)
	if (unsorted_offsets[j] == unsorted_offsets[order[i - 1]] + 4)
	  {
	    /* We must find exactly one offset that is higher than the
	       previous one by 4.  */
	    if (order[i] != order[i - 1])
	      return false;
	    order[i] = j;
	  }
      if (order[i] == order[i - 1])
	return false;
      /* The register numbers must be ascending.  */
      if (unsorted_regs != NULL
	  && unsorted_regs[order[i]] <= unsorted_regs[order[i - 1]])
	return false;
    }
  return true;
}

/* Used to determine in a peephole whether a sequence of load
   instructions can be changed into a load-multiple instruction.
   NOPS is the number of separate load instructions we are examining.  The
   first NOPS entries in OPERANDS are the destination registers, the
   next NOPS entries are memory operands.  If this function is
   successful, *BASE is set to the common base register of the memory
   accesses; *LOAD_OFFSET is set to the first memory location's offset
   from that base register.
   REGS is an array filled in with the destination register numbers.
   SAVED_ORDER (if nonnull), is an array filled in with an order that maps
   insn numbers to an ascending order of stores.  If CHECK_REGS is true,
   the sequence of registers in REGS matches the loads from ascending memory
   locations, and the function verifies that the register numbers are
   themselves ascending.  If CHECK_REGS is false, the register numbers
   are stored in the order they are found in the operands.  */
static int
load_multiple_sequence (rtx *operands, int nops, int *regs, int *saved_order,
			int *base, HOST_WIDE_INT *load_offset, bool check_regs)
{
  int unsorted_regs[MAX_LDM_STM_OPS];
  HOST_WIDE_INT unsorted_offsets[MAX_LDM_STM_OPS];
  int order[MAX_LDM_STM_OPS];
  rtx base_reg_rtx = NULL;
  int base_reg = -1;
  int i, ldm_case;

  /* Can only handle up to MAX_LDM_STM_OPS insns at present, though could be
     easily extended if required.  */
  gcc_assert (nops >= 2 && nops <= MAX_LDM_STM_OPS);

  memset (order, 0, MAX_LDM_STM_OPS * sizeof (int));

  /* Loop over the operands and check that the memory references are
     suitable (i.e. immediate offsets from the same base register).  At
     the same time, extract the target register, and the memory
     offsets.  */
  for (i = 0; i < nops; i++)
    {
      rtx reg;
      rtx offset;

      /* Convert a subreg of a mem into the mem itself.  */
      if (GET_CODE (operands[nops + i]) == SUBREG)
	operands[nops + i] = alter_subreg (operands + (nops + i), true);

      gcc_assert (MEM_P (operands[nops + i]));

      /* Don't reorder volatile memory references; it doesn't seem worth
	 looking for the case where the order is ok anyway.  */
      if (MEM_VOLATILE_P (operands[nops + i]))
	return 0;

      offset = const0_rtx;

      if ((REG_P (reg = XEXP (operands[nops + i], 0))
	   || (GET_CODE (reg) == SUBREG
	       && REG_P (reg = SUBREG_REG (reg))))
	  || (GET_CODE (XEXP (operands[nops + i], 0)) == PLUS
	      && ((REG_P (reg = XEXP (XEXP (operands[nops + i], 0), 0)))
		  || (GET_CODE (reg) == SUBREG
		      && REG_P (reg = SUBREG_REG (reg))))
	      && (CONST_INT_P (offset
		  = XEXP (XEXP (operands[nops + i], 0), 1)))))
	{
	  if (i == 0)
	    {
	      base_reg = REGNO (reg);
	      base_reg_rtx = reg;
	      if (TARGET_THUMB1 && base_reg > LAST_LO_REGNUM)
		return 0;
	    }
	  else if (base_reg != (int) REGNO (reg))
	    /* Not addressed from the same base register.  */
	    return 0;

	  unsorted_regs[i] = (REG_P (operands[i])
			      ? REGNO (operands[i])
			      : REGNO (SUBREG_REG (operands[i])));

	  /* If it isn't an integer register, or if it overwrites the
	     base register but isn't the last insn in the list, then
	     we can't do this.  */
	  if (unsorted_regs[i] < 0
	      || (TARGET_THUMB1 && unsorted_regs[i] > LAST_LO_REGNUM)
	      || unsorted_regs[i] > 14
	      || (i != nops - 1 && unsorted_regs[i] == base_reg))
	    return 0;

          /* Don't allow SP to be loaded unless it is also the base
             register.  It guarantees that SP is reset correctly when
             an LDM instruction is interrupted.  Otherwise, we might
             end up with a corrupt stack.  */
          if (unsorted_regs[i] == SP_REGNUM && base_reg != SP_REGNUM)
            return 0;

	  unsorted_offsets[i] = INTVAL (offset);
	  if (i == 0 || unsorted_offsets[i] < unsorted_offsets[order[0]])
	    order[0] = i;
	}
      else
	/* Not a suitable memory address.  */
	return 0;
    }

  /* All the useful information has now been extracted from the
     operands into unsorted_regs and unsorted_offsets; additionally,
     order[0] has been set to the lowest offset in the list.  Sort
     the offsets into order, verifying that they are adjacent, and
     check that the register numbers are ascending.  */
  if (!compute_offset_order (nops, unsorted_offsets, order,
			     check_regs ? unsorted_regs : NULL))
    return 0;

  if (saved_order)
    memcpy (saved_order, order, sizeof order);

  if (base)
    {
      *base = base_reg;

      for (i = 0; i < nops; i++)
	regs[i] = unsorted_regs[check_regs ? order[i] : i];

      *load_offset = unsorted_offsets[order[0]];
    }

  if (TARGET_THUMB1
      && !peep2_reg_dead_p (nops, base_reg_rtx))
    return 0;

  if (unsorted_offsets[order[0]] == 0)
    ldm_case = 1; /* ldmia */
  else if (TARGET_ARM && unsorted_offsets[order[0]] == 4)
    ldm_case = 2; /* ldmib */
  else if (TARGET_ARM && unsorted_offsets[order[nops - 1]] == 0)
    ldm_case = 3; /* ldmda */
  else if (TARGET_32BIT && unsorted_offsets[order[nops - 1]] == -4)
    ldm_case = 4; /* ldmdb */
  else if (const_ok_for_arm (unsorted_offsets[order[0]])
	   || const_ok_for_arm (-unsorted_offsets[order[0]]))
    ldm_case = 5;
  else
    return 0;

  if (!multiple_operation_profitable_p (false, nops,
					ldm_case == 5
					? unsorted_offsets[order[0]] : 0))
    return 0;

  return ldm_case;
}

/* Used to determine in a peephole whether a sequence of store instructions can
   be changed into a store-multiple instruction.
   NOPS is the number of separate store instructions we are examining.
   NOPS_TOTAL is the total number of instructions recognized by the peephole
   pattern.
   The first NOPS entries in OPERANDS are the source registers, the next
   NOPS entries are memory operands.  If this function is successful, *BASE is
   set to the common base register of the memory accesses; *LOAD_OFFSET is set
   to the first memory location's offset from that base register.  REGS is an
   array filled in with the source register numbers, REG_RTXS (if nonnull) is
   likewise filled with the corresponding rtx's.
   SAVED_ORDER (if nonnull), is an array filled in with an order that maps insn
   numbers to an ascending order of stores.
   If CHECK_REGS is true, the sequence of registers in *REGS matches the stores
   from ascending memory locations, and the function verifies that the register
   numbers are themselves ascending.  If CHECK_REGS is false, the register
   numbers are stored in the order they are found in the operands.  */
static int
store_multiple_sequence (rtx *operands, int nops, int nops_total,
			 int *regs, rtx *reg_rtxs, int *saved_order, int *base,
			 HOST_WIDE_INT *load_offset, bool check_regs)
{
  int unsorted_regs[MAX_LDM_STM_OPS];
  rtx unsorted_reg_rtxs[MAX_LDM_STM_OPS];
  HOST_WIDE_INT unsorted_offsets[MAX_LDM_STM_OPS];
  int order[MAX_LDM_STM_OPS];
  int base_reg = -1;
  rtx base_reg_rtx = NULL;
  int i, stm_case;

  /* Write back of base register is currently only supported for Thumb 1.  */
  int base_writeback = TARGET_THUMB1;

  /* Can only handle up to MAX_LDM_STM_OPS insns at present, though could be
     easily extended if required.  */
  gcc_assert (nops >= 2 && nops <= MAX_LDM_STM_OPS);

  memset (order, 0, MAX_LDM_STM_OPS * sizeof (int));

  /* Loop over the operands and check that the memory references are
     suitable (i.e. immediate offsets from the same base register).  At
     the same time, extract the target register, and the memory
     offsets.  */
  for (i = 0; i < nops; i++)
    {
      rtx reg;
      rtx offset;

      /* Convert a subreg of a mem into the mem itself.  */
      if (GET_CODE (operands[nops + i]) == SUBREG)
	operands[nops + i] = alter_subreg (operands + (nops + i), true);

      gcc_assert (MEM_P (operands[nops + i]));

      /* Don't reorder volatile memory references; it doesn't seem worth
	 looking for the case where the order is ok anyway.  */
      if (MEM_VOLATILE_P (operands[nops + i]))
	return 0;

      offset = const0_rtx;

      if ((REG_P (reg = XEXP (operands[nops + i], 0))
	   || (GET_CODE (reg) == SUBREG
	       && REG_P (reg = SUBREG_REG (reg))))
	  || (GET_CODE (XEXP (operands[nops + i], 0)) == PLUS
	      && ((REG_P (reg = XEXP (XEXP (operands[nops + i], 0), 0)))
		  || (GET_CODE (reg) == SUBREG
		      && REG_P (reg = SUBREG_REG (reg))))
	      && (CONST_INT_P (offset
		  = XEXP (XEXP (operands[nops + i], 0), 1)))))
	{
	  unsorted_reg_rtxs[i] = (REG_P (operands[i])
				  ? operands[i] : SUBREG_REG (operands[i]));
	  unsorted_regs[i] = REGNO (unsorted_reg_rtxs[i]);

	  if (i == 0)
	    {
	      base_reg = REGNO (reg);
	      base_reg_rtx = reg;
	      if (TARGET_THUMB1 && base_reg > LAST_LO_REGNUM)
		return 0;
	    }
	  else if (base_reg != (int) REGNO (reg))
	    /* Not addressed from the same base register.  */
	    return 0;

	  /* If it isn't an integer register, then we can't do this.  */
	  if (unsorted_regs[i] < 0
	      || (TARGET_THUMB1 && unsorted_regs[i] > LAST_LO_REGNUM)
	      /* The effects are unpredictable if the base register is
		 both updated and stored.  */
	      || (base_writeback && unsorted_regs[i] == base_reg)
	      || (TARGET_THUMB2 && unsorted_regs[i] == SP_REGNUM)
	      || unsorted_regs[i] > 14)
	    return 0;

	  unsorted_offsets[i] = INTVAL (offset);
	  if (i == 0 || unsorted_offsets[i] < unsorted_offsets[order[0]])
	    order[0] = i;
	}
      else
	/* Not a suitable memory address.  */
	return 0;
    }

  /* All the useful information has now been extracted from the
     operands into unsorted_regs and unsorted_offsets; additionally,
     order[0] has been set to the lowest offset in the list.  Sort
     the offsets into order, verifying that they are adjacent, and
     check that the register numbers are ascending.  */
  if (!compute_offset_order (nops, unsorted_offsets, order,
			     check_regs ? unsorted_regs : NULL))
    return 0;

  if (saved_order)
    memcpy (saved_order, order, sizeof order);

  if (base)
    {
      *base = base_reg;

      for (i = 0; i < nops; i++)
	{
	  regs[i] = unsorted_regs[check_regs ? order[i] : i];
	  if (reg_rtxs)
	    reg_rtxs[i] = unsorted_reg_rtxs[check_regs ? order[i] : i];
	}

      *load_offset = unsorted_offsets[order[0]];
    }

  if (TARGET_THUMB1
      && !peep2_reg_dead_p (nops_total, base_reg_rtx))
    return 0;

  if (unsorted_offsets[order[0]] == 0)
    stm_case = 1; /* stmia */
  else if (TARGET_ARM && unsorted_offsets[order[0]] == 4)
    stm_case = 2; /* stmib */
  else if (TARGET_ARM && unsorted_offsets[order[nops - 1]] == 0)
    stm_case = 3; /* stmda */
  else if (TARGET_32BIT && unsorted_offsets[order[nops - 1]] == -4)
    stm_case = 4; /* stmdb */
  else
    return 0;

  if (!multiple_operation_profitable_p (false, nops, 0))
    return 0;

  return stm_case;
}

/* Routines for use in generating RTL.  */

/* Generate a load-multiple instruction.  COUNT is the number of loads in
   the instruction; REGS and MEMS are arrays containing the operands.
   BASEREG is the base register to be used in addressing the memory operands.
   WBACK_OFFSET is nonzero if the instruction should update the base
   register.  */

static rtx
arm_gen_load_multiple_1 (int count, int *regs, rtx *mems, rtx basereg,
			 HOST_WIDE_INT wback_offset)
{
  int i = 0, j;
  rtx result;

  if (!multiple_operation_profitable_p (false, count, 0))
    {
      rtx seq;

      start_sequence ();

      for (i = 0; i < count; i++)
	emit_move_insn (gen_rtx_REG (SImode, regs[i]), mems[i]);

      if (wback_offset != 0)
	emit_move_insn (basereg, plus_constant (Pmode, basereg, wback_offset));

      seq = get_insns ();
      end_sequence ();

      return seq;
    }

  result = gen_rtx_PARALLEL (VOIDmode,
			     rtvec_alloc (count + (wback_offset != 0 ? 1 : 0)));
  if (wback_offset != 0)
    {
      XVECEXP (result, 0, 0)
	= gen_rtx_SET (basereg, plus_constant (Pmode, basereg, wback_offset));
      i = 1;
      count++;
    }

  for (j = 0; i < count; i++, j++)
    XVECEXP (result, 0, i)
      = gen_rtx_SET (gen_rtx_REG (SImode, regs[j]), mems[j]);

  return result;
}

/* Generate a store-multiple instruction.  COUNT is the number of stores in
   the instruction; REGS and MEMS are arrays containing the operands.
   BASEREG is the base register to be used in addressing the memory operands.
   WBACK_OFFSET is nonzero if the instruction should update the base
   register.  */

static rtx
arm_gen_store_multiple_1 (int count, int *regs, rtx *mems, rtx basereg,
			  HOST_WIDE_INT wback_offset)
{
  int i = 0, j;
  rtx result;

  if (GET_CODE (basereg) == PLUS)
    basereg = XEXP (basereg, 0);

  if (!multiple_operation_profitable_p (false, count, 0))
    {
      rtx seq;

      start_sequence ();

      for (i = 0; i < count; i++)
	emit_move_insn (mems[i], gen_rtx_REG (SImode, regs[i]));

      if (wback_offset != 0)
	emit_move_insn (basereg, plus_constant (Pmode, basereg, wback_offset));

      seq = get_insns ();
      end_sequence ();

      return seq;
    }

  result = gen_rtx_PARALLEL (VOIDmode,
			     rtvec_alloc (count + (wback_offset != 0 ? 1 : 0)));
  if (wback_offset != 0)
    {
      XVECEXP (result, 0, 0)
	= gen_rtx_SET (basereg, plus_constant (Pmode, basereg, wback_offset));
      i = 1;
      count++;
    }

  for (j = 0; i < count; i++, j++)
    XVECEXP (result, 0, i)
      = gen_rtx_SET (mems[j], gen_rtx_REG (SImode, regs[j]));

  return result;
}

/* Generate either a load-multiple or a store-multiple instruction.  This
   function can be used in situations where we can start with a single MEM
   rtx and adjust its address upwards.
   COUNT is the number of operations in the instruction, not counting a
   possible update of the base register.  REGS is an array containing the
   register operands.
   BASEREG is the base register to be used in addressing the memory operands,
   which are constructed from BASEMEM.
   WRITE_BACK specifies whether the generated instruction should include an
   update of the base register.
   OFFSETP is used to pass an offset to and from this function; this offset
   is not used when constructing the address (instead BASEMEM should have an
   appropriate offset in its address), it is used only for setting
   MEM_OFFSET.  It is updated only if WRITE_BACK is true.*/

static rtx
arm_gen_multiple_op (bool is_load, int *regs, int count, rtx basereg,
		     bool write_back, rtx basemem, HOST_WIDE_INT *offsetp)
{
  rtx mems[MAX_LDM_STM_OPS];
  HOST_WIDE_INT offset = *offsetp;
  int i;

  gcc_assert (count <= MAX_LDM_STM_OPS);

  if (GET_CODE (basereg) == PLUS)
    basereg = XEXP (basereg, 0);

  for (i = 0; i < count; i++)
    {
      rtx addr = plus_constant (Pmode, basereg, i * 4);
      mems[i] = adjust_automodify_address_nv (basemem, SImode, addr, offset);
      offset += 4;
    }

  if (write_back)
    *offsetp = offset;

  if (is_load)
    return arm_gen_load_multiple_1 (count, regs, mems, basereg,
				    write_back ? 4 * count : 0);
  else
    return arm_gen_store_multiple_1 (count, regs, mems, basereg,
				     write_back ? 4 * count : 0);
}

rtx
arm_gen_load_multiple (int *regs, int count, rtx basereg, int write_back,
		       rtx basemem, HOST_WIDE_INT *offsetp)
{
  return arm_gen_multiple_op (TRUE, regs, count, basereg, write_back, basemem,
			      offsetp);
}

rtx
arm_gen_store_multiple (int *regs, int count, rtx basereg, int write_back,
			rtx basemem, HOST_WIDE_INT *offsetp)
{
  return arm_gen_multiple_op (FALSE, regs, count, basereg, write_back, basemem,
			      offsetp);
}

/* Called from a peephole2 expander to turn a sequence of loads into an
   LDM instruction.  OPERANDS are the operands found by the peephole matcher;
   NOPS indicates how many separate loads we are trying to combine.  SORT_REGS
   is true if we can reorder the registers because they are used commutatively
   subsequently.
   Returns true iff we could generate a new instruction.  */

bool
gen_ldm_seq (rtx *operands, int nops, bool sort_regs)
{
  int regs[MAX_LDM_STM_OPS], mem_order[MAX_LDM_STM_OPS];
  rtx mems[MAX_LDM_STM_OPS];
  int i, j, base_reg;
  rtx base_reg_rtx;
  HOST_WIDE_INT offset;
  int write_back = FALSE;
  int ldm_case;
  rtx addr;

  ldm_case = load_multiple_sequence (operands, nops, regs, mem_order,
				     &base_reg, &offset, !sort_regs);

  if (ldm_case == 0)
    return false;

  if (sort_regs)
    for (i = 0; i < nops - 1; i++)
      for (j = i + 1; j < nops; j++)
	if (regs[i] > regs[j])
	  {
	    int t = regs[i];
	    regs[i] = regs[j];
	    regs[j] = t;
	  }
  base_reg_rtx = gen_rtx_REG (Pmode, base_reg);

  if (TARGET_THUMB1)
    {
      gcc_assert (peep2_reg_dead_p (nops, base_reg_rtx));
      gcc_assert (ldm_case == 1 || ldm_case == 5);
      write_back = TRUE;
    }

  if (ldm_case == 5)
    {
      rtx newbase = TARGET_THUMB1 ? base_reg_rtx : gen_rtx_REG (SImode, regs[0]);
      emit_insn (gen_addsi3 (newbase, base_reg_rtx, GEN_INT (offset)));
      offset = 0;
      if (!TARGET_THUMB1)
	base_reg_rtx = newbase;
    }

  for (i = 0; i < nops; i++)
    {
      addr = plus_constant (Pmode, base_reg_rtx, offset + i * 4);
      mems[i] = adjust_automodify_address_nv (operands[nops + mem_order[i]],
					      SImode, addr, 0);
    }
  emit_insn (arm_gen_load_multiple_1 (nops, regs, mems, base_reg_rtx,
				      write_back ? offset + i * 4 : 0));
  return true;
}

/* Called from a peephole2 expander to turn a sequence of stores into an
   STM instruction.  OPERANDS are the operands found by the peephole matcher;
   NOPS indicates how many separate stores we are trying to combine.
   Returns true iff we could generate a new instruction.  */

bool
gen_stm_seq (rtx *operands, int nops)
{
  int i;
  int regs[MAX_LDM_STM_OPS], mem_order[MAX_LDM_STM_OPS];
  rtx mems[MAX_LDM_STM_OPS];
  int base_reg;
  rtx base_reg_rtx;
  HOST_WIDE_INT offset;
  int write_back = FALSE;
  int stm_case;
  rtx addr;
  bool base_reg_dies;

  stm_case = store_multiple_sequence (operands, nops, nops, regs, NULL,
				      mem_order, &base_reg, &offset, true);

  if (stm_case == 0)
    return false;

  base_reg_rtx = gen_rtx_REG (Pmode, base_reg);

  base_reg_dies = peep2_reg_dead_p (nops, base_reg_rtx);
  if (TARGET_THUMB1)
    {
      gcc_assert (base_reg_dies);
      write_back = TRUE;
    }

  if (stm_case == 5)
    {
      gcc_assert (base_reg_dies);
      emit_insn (gen_addsi3 (base_reg_rtx, base_reg_rtx, GEN_INT (offset)));
      offset = 0;
    }

  addr = plus_constant (Pmode, base_reg_rtx, offset);

  for (i = 0; i < nops; i++)
    {
      addr = plus_constant (Pmode, base_reg_rtx, offset + i * 4);
      mems[i] = adjust_automodify_address_nv (operands[nops + mem_order[i]],
					      SImode, addr, 0);
    }
  emit_insn (arm_gen_store_multiple_1 (nops, regs, mems, base_reg_rtx,
				       write_back ? offset + i * 4 : 0));
  return true;
}

/* Called from a peephole2 expander to turn a sequence of stores that are
   preceded by constant loads into an STM instruction.  OPERANDS are the
   operands found by the peephole matcher; NOPS indicates how many
   separate stores we are trying to combine; there are 2 * NOPS
   instructions in the peephole.
   Returns true iff we could generate a new instruction.  */

bool
gen_const_stm_seq (rtx *operands, int nops)
{
  int regs[MAX_LDM_STM_OPS], sorted_regs[MAX_LDM_STM_OPS];
  int reg_order[MAX_LDM_STM_OPS], mem_order[MAX_LDM_STM_OPS];
  rtx reg_rtxs[MAX_LDM_STM_OPS], orig_reg_rtxs[MAX_LDM_STM_OPS];
  rtx mems[MAX_LDM_STM_OPS];
  int base_reg;
  rtx base_reg_rtx;
  HOST_WIDE_INT offset;
  int write_back = FALSE;
  int stm_case;
  rtx addr;
  bool base_reg_dies;
  int i, j;
  HARD_REG_SET allocated;

  stm_case = store_multiple_sequence (operands, nops, 2 * nops, regs, reg_rtxs,
				      mem_order, &base_reg, &offset, false);

  if (stm_case == 0)
    return false;

  memcpy (orig_reg_rtxs, reg_rtxs, sizeof orig_reg_rtxs);

  /* If the same register is used more than once, try to find a free
     register.  */
  CLEAR_HARD_REG_SET (allocated);
  for (i = 0; i < nops; i++)
    {
      for (j = i + 1; j < nops; j++)
	if (regs[i] == regs[j])
	  {
	    rtx t = peep2_find_free_register (0, nops * 2,
					      TARGET_THUMB1 ? "l" : "r",
					      SImode, &allocated);
	    if (t == NULL_RTX)
	      return false;
	    reg_rtxs[i] = t;
	    regs[i] = REGNO (t);
	  }
    }

  /* Compute an ordering that maps the register numbers to an ascending
     sequence.  */
  reg_order[0] = 0;
  for (i = 0; i < nops; i++)
    if (regs[i] < regs[reg_order[0]])
      reg_order[0] = i;

  for (i = 1; i < nops; i++)
    {
      int this_order = reg_order[i - 1];
      for (j = 0; j < nops; j++)
	if (regs[j] > regs[reg_order[i - 1]]
	    && (this_order == reg_order[i - 1]
		|| regs[j] < regs[this_order]))
	  this_order = j;
      reg_order[i] = this_order;
    }

  /* Ensure that registers that must be live after the instruction end
     up with the correct value.  */
  for (i = 0; i < nops; i++)
    {
      int this_order = reg_order[i];
      if ((this_order != mem_order[i]
	   || orig_reg_rtxs[this_order] != reg_rtxs[this_order])
	  && !peep2_reg_dead_p (nops * 2, orig_reg_rtxs[this_order]))
	return false;
    }

  /* Load the constants.  */
  for (i = 0; i < nops; i++)
    {
      rtx op = operands[2 * nops + mem_order[i]];
      sorted_regs[i] = regs[reg_order[i]];
      emit_move_insn (reg_rtxs[reg_order[i]], op);
    }

  base_reg_rtx = gen_rtx_REG (Pmode, base_reg);

  base_reg_dies = peep2_reg_dead_p (nops * 2, base_reg_rtx);
  if (TARGET_THUMB1)
    {
      gcc_assert (base_reg_dies);
      write_back = TRUE;
    }

  if (stm_case == 5)
    {
      gcc_assert (base_reg_dies);
      emit_insn (gen_addsi3 (base_reg_rtx, base_reg_rtx, GEN_INT (offset)));
      offset = 0;
    }

  addr = plus_constant (Pmode, base_reg_rtx, offset);

  for (i = 0; i < nops; i++)
    {
      addr = plus_constant (Pmode, base_reg_rtx, offset + i * 4);
      mems[i] = adjust_automodify_address_nv (operands[nops + mem_order[i]],
					      SImode, addr, 0);
    }
  emit_insn (arm_gen_store_multiple_1 (nops, sorted_regs, mems, base_reg_rtx,
				       write_back ? offset + i * 4 : 0));
  return true;
}

/* Copy a block of memory using plain ldr/str/ldrh/strh instructions, to permit
   unaligned copies on processors which support unaligned semantics for those
   instructions.  INTERLEAVE_FACTOR can be used to attempt to hide load latency
   (using more registers) by doing e.g. load/load/store/store for a factor of 2.
   An interleave factor of 1 (the minimum) will perform no interleaving.
   Load/store multiple are used for aligned addresses where possible.  */

static void
arm_block_move_unaligned_straight (rtx dstbase, rtx srcbase,
				   HOST_WIDE_INT length,
				   unsigned int interleave_factor)
{
  rtx *regs = XALLOCAVEC (rtx, interleave_factor);
  int *regnos = XALLOCAVEC (int, interleave_factor);
  HOST_WIDE_INT block_size_bytes = interleave_factor * UNITS_PER_WORD;
  HOST_WIDE_INT i, j;
  HOST_WIDE_INT remaining = length, words;
  rtx halfword_tmp = NULL, byte_tmp = NULL;
  rtx dst, src;
  bool src_aligned = MEM_ALIGN (srcbase) >= BITS_PER_WORD;
  bool dst_aligned = MEM_ALIGN (dstbase) >= BITS_PER_WORD;
  HOST_WIDE_INT srcoffset, dstoffset;
  HOST_WIDE_INT src_autoinc, dst_autoinc;
  rtx mem, addr;
  
  gcc_assert (interleave_factor >= 1 && interleave_factor <= 4);
  
  /* Use hard registers if we have aligned source or destination so we can use
     load/store multiple with contiguous registers.  */
  if (dst_aligned || src_aligned)
    for (i = 0; i < interleave_factor; i++)
      regs[i] = gen_rtx_REG (SImode, i);
  else
    for (i = 0; i < interleave_factor; i++)
      regs[i] = gen_reg_rtx (SImode);

  dst = copy_addr_to_reg (XEXP (dstbase, 0));
  src = copy_addr_to_reg (XEXP (srcbase, 0));

  srcoffset = dstoffset = 0;
  
  /* Calls to arm_gen_load_multiple and arm_gen_store_multiple update SRC/DST.
     For copying the last bytes we want to subtract this offset again.  */
  src_autoinc = dst_autoinc = 0;

  for (i = 0; i < interleave_factor; i++)
    regnos[i] = i;

  /* Copy BLOCK_SIZE_BYTES chunks.  */

  for (i = 0; i + block_size_bytes <= length; i += block_size_bytes)
    {
      /* Load words.  */
      if (src_aligned && interleave_factor > 1)
	{
	  emit_insn (arm_gen_load_multiple (regnos, interleave_factor, src,
					    TRUE, srcbase, &srcoffset));
	  src_autoinc += UNITS_PER_WORD * interleave_factor;
	}
      else
	{
	  for (j = 0; j < interleave_factor; j++)
	    {
	      addr = plus_constant (Pmode, src, (srcoffset + j * UNITS_PER_WORD
						 - src_autoinc));
	      mem = adjust_automodify_address (srcbase, SImode, addr,
					       srcoffset + j * UNITS_PER_WORD);
	      emit_insn (gen_unaligned_loadsi (regs[j], mem));
	    }
	  srcoffset += block_size_bytes;
	}

      /* Store words.  */
      if (dst_aligned && interleave_factor > 1)
	{
	  emit_insn (arm_gen_store_multiple (regnos, interleave_factor, dst,
					     TRUE, dstbase, &dstoffset));
	  dst_autoinc += UNITS_PER_WORD * interleave_factor;
	}
      else
	{
	  for (j = 0; j < interleave_factor; j++)
	    {
	      addr = plus_constant (Pmode, dst, (dstoffset + j * UNITS_PER_WORD
						 - dst_autoinc));
	      mem = adjust_automodify_address (dstbase, SImode, addr,
					       dstoffset + j * UNITS_PER_WORD);
	      emit_insn (gen_unaligned_storesi (mem, regs[j]));
	    }
	  dstoffset += block_size_bytes;
	}

      remaining -= block_size_bytes;
    }
  
  /* Copy any whole words left (note these aren't interleaved with any
     subsequent halfword/byte load/stores in the interests of simplicity).  */
  
  words = remaining / UNITS_PER_WORD;

  gcc_assert (words < interleave_factor);
  
  if (src_aligned && words > 1)
    {
      emit_insn (arm_gen_load_multiple (regnos, words, src, TRUE, srcbase,
					&srcoffset));
      src_autoinc += UNITS_PER_WORD * words;
    }
  else
    {
      for (j = 0; j < words; j++)
	{
	  addr = plus_constant (Pmode, src,
				srcoffset + j * UNITS_PER_WORD - src_autoinc);
	  mem = adjust_automodify_address (srcbase, SImode, addr,
					   srcoffset + j * UNITS_PER_WORD);
	  if (src_aligned)
	    emit_move_insn (regs[j], mem);
	  else
	    emit_insn (gen_unaligned_loadsi (regs[j], mem));
	}
      srcoffset += words * UNITS_PER_WORD;
    }

  if (dst_aligned && words > 1)
    {
      emit_insn (arm_gen_store_multiple (regnos, words, dst, TRUE, dstbase,
					 &dstoffset));
      dst_autoinc += words * UNITS_PER_WORD;
    }
  else
    {
      for (j = 0; j < words; j++)
	{
	  addr = plus_constant (Pmode, dst,
				dstoffset + j * UNITS_PER_WORD - dst_autoinc);
	  mem = adjust_automodify_address (dstbase, SImode, addr,
					   dstoffset + j * UNITS_PER_WORD);
	  if (dst_aligned)
	    emit_move_insn (mem, regs[j]);
	  else
	    emit_insn (gen_unaligned_storesi (mem, regs[j]));
	}
      dstoffset += words * UNITS_PER_WORD;
    }

  remaining -= words * UNITS_PER_WORD;
  
  gcc_assert (remaining < 4);
  
  /* Copy a halfword if necessary.  */
  
  if (remaining >= 2)
    {
      halfword_tmp = gen_reg_rtx (SImode);

      addr = plus_constant (Pmode, src, srcoffset - src_autoinc);
      mem = adjust_automodify_address (srcbase, HImode, addr, srcoffset);
      emit_insn (gen_unaligned_loadhiu (halfword_tmp, mem));

      /* Either write out immediately, or delay until we've loaded the last
	 byte, depending on interleave factor.  */
      if (interleave_factor == 1)
	{
	  addr = plus_constant (Pmode, dst, dstoffset - dst_autoinc);
	  mem = adjust_automodify_address (dstbase, HImode, addr, dstoffset);
	  emit_insn (gen_unaligned_storehi (mem,
		       gen_lowpart (HImode, halfword_tmp)));
	  halfword_tmp = NULL;
	  dstoffset += 2;
	}

      remaining -= 2;
      srcoffset += 2;
    }
  
  gcc_assert (remaining < 2);
  
  /* Copy last byte.  */
  
  if ((remaining & 1) != 0)
    {
      byte_tmp = gen_reg_rtx (SImode);

      addr = plus_constant (Pmode, src, srcoffset - src_autoinc);
      mem = adjust_automodify_address (srcbase, QImode, addr, srcoffset);
      emit_move_insn (gen_lowpart (QImode, byte_tmp), mem);

      if (interleave_factor == 1)
	{
	  addr = plus_constant (Pmode, dst, dstoffset - dst_autoinc);
	  mem = adjust_automodify_address (dstbase, QImode, addr, dstoffset);
	  emit_move_insn (mem, gen_lowpart (QImode, byte_tmp));
	  byte_tmp = NULL;
	  dstoffset++;
	}

      remaining--;
      srcoffset++;
    }
  
  /* Store last halfword if we haven't done so already.  */
  
  if (halfword_tmp)
    {
      addr = plus_constant (Pmode, dst, dstoffset - dst_autoinc);
      mem = adjust_automodify_address (dstbase, HImode, addr, dstoffset);
      emit_insn (gen_unaligned_storehi (mem,
		   gen_lowpart (HImode, halfword_tmp)));
      dstoffset += 2;
    }

  /* Likewise for last byte.  */

  if (byte_tmp)
    {
      addr = plus_constant (Pmode, dst, dstoffset - dst_autoinc);
      mem = adjust_automodify_address (dstbase, QImode, addr, dstoffset);
      emit_move_insn (mem, gen_lowpart (QImode, byte_tmp));
      dstoffset++;
    }
  
  gcc_assert (remaining == 0 && srcoffset == dstoffset);
}

/* From mips_adjust_block_mem:

   Helper function for doing a loop-based block operation on memory
   reference MEM.  Each iteration of the loop will operate on LENGTH
   bytes of MEM.

   Create a new base register for use within the loop and point it to
   the start of MEM.  Create a new memory reference that uses this
   register.  Store them in *LOOP_REG and *LOOP_MEM respectively.  */

static void
arm_adjust_block_mem (rtx mem, HOST_WIDE_INT length, rtx *loop_reg,
		      rtx *loop_mem)
{
  *loop_reg = copy_addr_to_reg (XEXP (mem, 0));
  
  /* Although the new mem does not refer to a known location,
     it does keep up to LENGTH bytes of alignment.  */
  *loop_mem = change_address (mem, BLKmode, *loop_reg);
  set_mem_align (*loop_mem, MIN (MEM_ALIGN (mem), length * BITS_PER_UNIT));
}

/* From mips_block_move_loop:

   Move LENGTH bytes from SRC to DEST using a loop that moves BYTES_PER_ITER
   bytes at a time.  LENGTH must be at least BYTES_PER_ITER.  Assume that
   the memory regions do not overlap.  */

static void
arm_block_move_unaligned_loop (rtx dest, rtx src, HOST_WIDE_INT length,
			       unsigned int interleave_factor,
			       HOST_WIDE_INT bytes_per_iter)
{
  rtx src_reg, dest_reg, final_src, test;
  HOST_WIDE_INT leftover;
  
  leftover = length % bytes_per_iter;
  length -= leftover;
  
  /* Create registers and memory references for use within the loop.  */
  arm_adjust_block_mem (src, bytes_per_iter, &src_reg, &src);
  arm_adjust_block_mem (dest, bytes_per_iter, &dest_reg, &dest);
  
  /* Calculate the value that SRC_REG should have after the last iteration of
     the loop.  */
  final_src = expand_simple_binop (Pmode, PLUS, src_reg, GEN_INT (length),
				   0, 0, OPTAB_WIDEN);

  /* Emit the start of the loop.  */
  rtx_code_label *label = gen_label_rtx ();
  emit_label (label);
  
  /* Emit the loop body.  */
  arm_block_move_unaligned_straight (dest, src, bytes_per_iter,
				     interleave_factor);

  /* Move on to the next block.  */
  emit_move_insn (src_reg, plus_constant (Pmode, src_reg, bytes_per_iter));
  emit_move_insn (dest_reg, plus_constant (Pmode, dest_reg, bytes_per_iter));
  
  /* Emit the loop condition.  */
  test = gen_rtx_NE (VOIDmode, src_reg, final_src);
  emit_jump_insn (gen_cbranchsi4 (test, src_reg, final_src, label));
  
  /* Mop up any left-over bytes.  */
  if (leftover)
    arm_block_move_unaligned_straight (dest, src, leftover, interleave_factor);
}

/* Emit a block move when either the source or destination is unaligned (not
   aligned to a four-byte boundary).  This may need further tuning depending on
   core type, optimize_size setting, etc.  */

static int
arm_movmemqi_unaligned (rtx *operands)
{
  HOST_WIDE_INT length = INTVAL (operands[2]);
  
  if (optimize_size)
    {
      bool src_aligned = MEM_ALIGN (operands[1]) >= BITS_PER_WORD;
      bool dst_aligned = MEM_ALIGN (operands[0]) >= BITS_PER_WORD;
      /* Inlined memcpy using ldr/str/ldrh/strh can be quite big: try to limit
	 size of code if optimizing for size.  We'll use ldm/stm if src_aligned
	 or dst_aligned though: allow more interleaving in those cases since the
	 resulting code can be smaller.  */
      unsigned int interleave_factor = (src_aligned || dst_aligned) ? 2 : 1;
      HOST_WIDE_INT bytes_per_iter = (src_aligned || dst_aligned) ? 8 : 4;
      
      if (length > 12)
	arm_block_move_unaligned_loop (operands[0], operands[1], length,
				       interleave_factor, bytes_per_iter);
      else
	arm_block_move_unaligned_straight (operands[0], operands[1], length,
					   interleave_factor);
    }
  else
    {
      /* Note that the loop created by arm_block_move_unaligned_loop may be
	 subject to loop unrolling, which makes tuning this condition a little
	 redundant.  */
      if (length > 32)
	arm_block_move_unaligned_loop (operands[0], operands[1], length, 4, 16);
      else
	arm_block_move_unaligned_straight (operands[0], operands[1], length, 4);
    }
  
  return 1;
}

int
arm_gen_movmemqi (rtx *operands)
{
  HOST_WIDE_INT in_words_to_go, out_words_to_go, last_bytes;
  HOST_WIDE_INT srcoffset, dstoffset;
  rtx src, dst, srcbase, dstbase;
  rtx part_bytes_reg = NULL;
  rtx mem;

  if (!CONST_INT_P (operands[2])
      || !CONST_INT_P (operands[3])
      || INTVAL (operands[2]) > 64)
    return 0;

  if (unaligned_access && (INTVAL (operands[3]) & 3) != 0)
    return arm_movmemqi_unaligned (operands);

  if (INTVAL (operands[3]) & 3)
    return 0;

  dstbase = operands[0];
  srcbase = operands[1];

  dst = copy_to_mode_reg (SImode, XEXP (dstbase, 0));
  src = copy_to_mode_reg (SImode, XEXP (srcbase, 0));

  in_words_to_go = ARM_NUM_INTS (INTVAL (operands[2]));
  out_words_to_go = INTVAL (operands[2]) / 4;
  last_bytes = INTVAL (operands[2]) & 3;
  dstoffset = srcoffset = 0;

  if (out_words_to_go != in_words_to_go && ((in_words_to_go - 1) & 3) != 0)
    part_bytes_reg = gen_rtx_REG (SImode, (in_words_to_go - 1) & 3);

  while (in_words_to_go >= 2)
    {
      if (in_words_to_go > 4)
	emit_insn (arm_gen_load_multiple (arm_regs_in_sequence, 4, src,
					  TRUE, srcbase, &srcoffset));
      else
	emit_insn (arm_gen_load_multiple (arm_regs_in_sequence, in_words_to_go,
					  src, FALSE, srcbase,
					  &srcoffset));

      if (out_words_to_go)
	{
	  if (out_words_to_go > 4)
	    emit_insn (arm_gen_store_multiple (arm_regs_in_sequence, 4, dst,
					       TRUE, dstbase, &dstoffset));
	  else if (out_words_to_go != 1)
	    emit_insn (arm_gen_store_multiple (arm_regs_in_sequence,
					       out_words_to_go, dst,
					       (last_bytes == 0
						? FALSE : TRUE),
					       dstbase, &dstoffset));
	  else
	    {
	      mem = adjust_automodify_address (dstbase, SImode, dst, dstoffset);
	      emit_move_insn (mem, gen_rtx_REG (SImode, R0_REGNUM));
	      if (last_bytes != 0)
		{
		  emit_insn (gen_addsi3 (dst, dst, GEN_INT (4)));
		  dstoffset += 4;
		}
	    }
	}

      in_words_to_go -= in_words_to_go < 4 ? in_words_to_go : 4;
      out_words_to_go -= out_words_to_go < 4 ? out_words_to_go : 4;
    }

  /* OUT_WORDS_TO_GO will be zero here if there are byte stores to do.  */
  if (out_words_to_go)
    {
      rtx sreg;

      mem = adjust_automodify_address (srcbase, SImode, src, srcoffset);
      sreg = copy_to_reg (mem);

      mem = adjust_automodify_address (dstbase, SImode, dst, dstoffset);
      emit_move_insn (mem, sreg);
      in_words_to_go--;

      gcc_assert (!in_words_to_go);	/* Sanity check */
    }

  if (in_words_to_go)
    {
      gcc_assert (in_words_to_go > 0);

      mem = adjust_automodify_address (srcbase, SImode, src, srcoffset);
      part_bytes_reg = copy_to_mode_reg (SImode, mem);
    }

  gcc_assert (!last_bytes || part_bytes_reg);

  if (BYTES_BIG_ENDIAN && last_bytes)
    {
      rtx tmp = gen_reg_rtx (SImode);

      /* The bytes we want are in the top end of the word.  */
      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg,
			      GEN_INT (8 * (4 - last_bytes))));
      part_bytes_reg = tmp;

      while (last_bytes)
	{
	  mem = adjust_automodify_address (dstbase, QImode,
					   plus_constant (Pmode, dst,
							  last_bytes - 1),
					   dstoffset + last_bytes - 1);
	  emit_move_insn (mem, gen_lowpart (QImode, part_bytes_reg));

	  if (--last_bytes)
	    {
	      tmp = gen_reg_rtx (SImode);
	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (8)));
	      part_bytes_reg = tmp;
	    }
	}

    }
  else
    {
      if (last_bytes > 1)
	{
	  mem = adjust_automodify_address (dstbase, HImode, dst, dstoffset);
	  emit_move_insn (mem, gen_lowpart (HImode, part_bytes_reg));
	  last_bytes -= 2;
	  if (last_bytes)
	    {
	      rtx tmp = gen_reg_rtx (SImode);
	      emit_insn (gen_addsi3 (dst, dst, const2_rtx));
	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (16)));
	      part_bytes_reg = tmp;
	      dstoffset += 2;
	    }
	}

      if (last_bytes)
	{
	  mem = adjust_automodify_address (dstbase, QImode, dst, dstoffset);
	  emit_move_insn (mem, gen_lowpart (QImode, part_bytes_reg));
	}
    }

  return 1;
}

/* Helper for gen_movmem_ldrd_strd. Increase the address of memory rtx
by mode size.  */
inline static rtx
next_consecutive_mem (rtx mem)
{
  machine_mode mode = GET_MODE (mem);
  HOST_WIDE_INT offset = GET_MODE_SIZE (mode);
  rtx addr = plus_constant (Pmode, XEXP (mem, 0), offset);

  return adjust_automodify_address (mem, mode, addr, offset);
}

/* Copy using LDRD/STRD instructions whenever possible.
   Returns true upon success. */
bool
gen_movmem_ldrd_strd (rtx *operands)
{
  unsigned HOST_WIDE_INT len;
  HOST_WIDE_INT align;
  rtx src, dst, base;
  rtx reg0;
  bool src_aligned, dst_aligned;
  bool src_volatile, dst_volatile;

  gcc_assert (CONST_INT_P (operands[2]));
  gcc_assert (CONST_INT_P (operands[3]));

  len = UINTVAL (operands[2]);
  if (len > 64)
    return false;

  /* Maximum alignment we can assume for both src and dst buffers.  */
  align = INTVAL (operands[3]);

  if ((!unaligned_access) && (len >= 4) && ((align & 3) != 0))
    return false;

  /* Place src and dst addresses in registers
     and update the corresponding mem rtx.  */
  dst = operands[0];
  dst_volatile = MEM_VOLATILE_P (dst);
  dst_aligned = MEM_ALIGN (dst) >= BITS_PER_WORD;
  base = copy_to_mode_reg (SImode, XEXP (dst, 0));
  dst = adjust_automodify_address (dst, VOIDmode, base, 0);

  src = operands[1];
  src_volatile = MEM_VOLATILE_P (src);
  src_aligned = MEM_ALIGN (src) >= BITS_PER_WORD;
  base = copy_to_mode_reg (SImode, XEXP (src, 0));
  src = adjust_automodify_address (src, VOIDmode, base, 0);

  if (!unaligned_access && !(src_aligned && dst_aligned))
    return false;

  if (src_volatile || dst_volatile)
    return false;

  /* If we cannot generate any LDRD/STRD, try to generate LDM/STM.  */
  if (!(dst_aligned || src_aligned))
    return arm_gen_movmemqi (operands);

  /* If the either src or dst is unaligned we'll be accessing it as pairs
     of unaligned SImode accesses.  Otherwise we can generate DImode
     ldrd/strd instructions.  */
  src = adjust_address (src, src_aligned ? DImode : SImode, 0);
  dst = adjust_address (dst, dst_aligned ? DImode : SImode, 0);

  while (len >= 8)
    {
      len -= 8;
      reg0 = gen_reg_rtx (DImode);
      rtx low_reg = NULL_RTX;
      rtx hi_reg = NULL_RTX;

      if (!src_aligned || !dst_aligned)
	{
	  low_reg = gen_lowpart (SImode, reg0);
	  hi_reg = gen_highpart_mode (SImode, DImode, reg0);
	}
      if (src_aligned)
        emit_move_insn (reg0, src);
      else
	{
	  emit_insn (gen_unaligned_loadsi (low_reg, src));
	  src = next_consecutive_mem (src);
	  emit_insn (gen_unaligned_loadsi (hi_reg, src));
	}

      if (dst_aligned)
        emit_move_insn (dst, reg0);
      else
	{
	  emit_insn (gen_unaligned_storesi (dst, low_reg));
	  dst = next_consecutive_mem (dst);
	  emit_insn (gen_unaligned_storesi (dst, hi_reg));
	}

      src = next_consecutive_mem (src);
      dst = next_consecutive_mem (dst);
    }

  gcc_assert (len < 8);
  if (len >= 4)
    {
      /* More than a word but less than a double-word to copy.  Copy a word.  */
      reg0 = gen_reg_rtx (SImode);
      src = adjust_address (src, SImode, 0);
      dst = adjust_address (dst, SImode, 0);
      if (src_aligned)
        emit_move_insn (reg0, src);
      else
        emit_insn (gen_unaligned_loadsi (reg0, src));

      if (dst_aligned)
        emit_move_insn (dst, reg0);
      else
        emit_insn (gen_unaligned_storesi (dst, reg0));

      src = next_consecutive_mem (src);
      dst = next_consecutive_mem (dst);
      len -= 4;
    }

  if (len == 0)
    return true;

  /* Copy the remaining bytes.  */
  if (len >= 2)
    {
      dst = adjust_address (dst, HImode, 0);
      src = adjust_address (src, HImode, 0);
      reg0 = gen_reg_rtx (SImode);
      if (src_aligned)
        emit_insn (gen_zero_extendhisi2 (reg0, src));
      else
        emit_insn (gen_unaligned_loadhiu (reg0, src));

      if (dst_aligned)
        emit_insn (gen_movhi (dst, gen_lowpart(HImode, reg0)));
      else
        emit_insn (gen_unaligned_storehi (dst, gen_lowpart (HImode, reg0)));

      src = next_consecutive_mem (src);
      dst = next_consecutive_mem (dst);
      if (len == 2)
        return true;
    }

  dst = adjust_address (dst, QImode, 0);
  src = adjust_address (src, QImode, 0);
  reg0 = gen_reg_rtx (QImode);
  emit_move_insn (reg0, src);
  emit_move_insn (dst, reg0);
  return true;
}

/* Select a dominance comparison mode if possible for a test of the general
   form (OP (COND_OR (X) (Y)) (const_int 0)).  We support three forms.
   COND_OR == DOM_CC_X_AND_Y => (X && Y)
   COND_OR == DOM_CC_NX_OR_Y => ((! X) || Y)
   COND_OR == DOM_CC_X_OR_Y => (X || Y)
   In all cases OP will be either EQ or NE, but we don't need to know which
   here.  If we are unable to support a dominance comparison we return
   CC mode.  This will then fail to match for the RTL expressions that
   generate this call.  */
machine_mode
arm_select_dominance_cc_mode (rtx x, rtx y, HOST_WIDE_INT cond_or)
{
  enum rtx_code cond1, cond2;
  int swapped = 0;

  /* Currently we will probably get the wrong result if the individual
     comparisons are not simple.  This also ensures that it is safe to
     reverse a comparison if necessary.  */
  if ((arm_select_cc_mode (cond1 = GET_CODE (x), XEXP (x, 0), XEXP (x, 1))
       != CCmode)
      || (arm_select_cc_mode (cond2 = GET_CODE (y), XEXP (y, 0), XEXP (y, 1))
	  != CCmode))
    return CCmode;

  /* The if_then_else variant of this tests the second condition if the
     first passes, but is true if the first fails.  Reverse the first
     condition to get a true "inclusive-or" expression.  */
  if (cond_or == DOM_CC_NX_OR_Y)
    cond1 = reverse_condition (cond1);

  /* If the comparisons are not equal, and one doesn't dominate the other,
     then we can't do this.  */
  if (cond1 != cond2
      && !comparison_dominates_p (cond1, cond2)
      && (swapped = 1, !comparison_dominates_p (cond2, cond1)))
    return CCmode;

  if (swapped)
    std::swap (cond1, cond2);

  switch (cond1)
    {
    case EQ:
      if (cond_or == DOM_CC_X_AND_Y)
	return CC_DEQmode;

      switch (cond2)
	{
	case EQ: return CC_DEQmode;
	case LE: return CC_DLEmode;
	case LEU: return CC_DLEUmode;
	case GE: return CC_DGEmode;
	case GEU: return CC_DGEUmode;
	default: gcc_unreachable ();
	}

    case LT:
      if (cond_or == DOM_CC_X_AND_Y)
	return CC_DLTmode;

      switch (cond2)
	{
	case  LT:
	    return CC_DLTmode;
	case LE:
	  return CC_DLEmode;
	case NE:
	  return CC_DNEmode;
	default:
	  gcc_unreachable ();
	}

    case GT:
      if (cond_or == DOM_CC_X_AND_Y)
	return CC_DGTmode;

      switch (cond2)
	{
	case GT:
	  return CC_DGTmode;
	case GE:
	  return CC_DGEmode;
	case NE:
	  return CC_DNEmode;
	default:
	  gcc_unreachable ();
	}

    case LTU:
      if (cond_or == DOM_CC_X_AND_Y)
	return CC_DLTUmode;

      switch (cond2)
	{
	case LTU:
	  return CC_DLTUmode;
	case LEU:
	  return CC_DLEUmode;
	case NE:
	  return CC_DNEmode;
	default:
	  gcc_unreachable ();
	}

    case GTU:
      if (cond_or == DOM_CC_X_AND_Y)
	return CC_DGTUmode;

      switch (cond2)
	{
	case GTU:
	  return CC_DGTUmode;
	case GEU:
	  return CC_DGEUmode;
	case NE:
	  return CC_DNEmode;
	default:
	  gcc_unreachable ();
	}

    /* The remaining cases only occur when both comparisons are the
       same.  */
    case NE:
      gcc_assert (cond1 == cond2);
      return CC_DNEmode;

    case LE:
      gcc_assert (cond1 == cond2);
      return CC_DLEmode;

    case GE:
      gcc_assert (cond1 == cond2);
      return CC_DGEmode;

    case LEU:
      gcc_assert (cond1 == cond2);
      return CC_DLEUmode;

    case GEU:
      gcc_assert (cond1 == cond2);
      return CC_DGEUmode;

    default:
      gcc_unreachable ();
    }
}

machine_mode
arm_select_cc_mode (enum rtx_code op, rtx x, rtx y)
{
  /* All floating point compares return CCFP if it is an equality
     comparison, and CCFPE otherwise.  */
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    {
      switch (op)
	{
	case EQ:
	case NE:
	case UNORDERED:
	case ORDERED:
	case UNLT:
	case UNLE:
	case UNGT:
	case UNGE:
	case UNEQ:
	case LTGT:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	  return CCFPEmode;

	default:
	  gcc_unreachable ();
	}
    }

  /* A compare with a shifted operand.  Because of canonicalization, the
     comparison will have to be swapped when we emit the assembler.  */
  if (GET_MODE (y) == SImode
      && (REG_P (y) || (GET_CODE (y) == SUBREG))
      && (GET_CODE (x) == ASHIFT || GET_CODE (x) == ASHIFTRT
	  || GET_CODE (x) == LSHIFTRT || GET_CODE (x) == ROTATE
	  || GET_CODE (x) == ROTATERT))
    return CC_SWPmode;

  /* This operation is performed swapped, but since we only rely on the Z
     flag we don't need an additional mode.  */
  if (GET_MODE (y) == SImode
      && (REG_P (y) || (GET_CODE (y) == SUBREG))
      && GET_CODE (x) == NEG
      && (op ==	EQ || op == NE))
    return CC_Zmode;

  /* This is a special case that is used by combine to allow a
     comparison of a shifted byte load to be split into a zero-extend
     followed by a comparison of the shifted integer (only valid for
     equalities and unsigned inequalities).  */
  if (GET_MODE (x) == SImode
      && GET_CODE (x) == ASHIFT
      && CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) == 24
      && GET_CODE (XEXP (x, 0)) == SUBREG
      && MEM_P (SUBREG_REG (XEXP (x, 0)))
      && GET_MODE (SUBREG_REG (XEXP (x, 0))) == QImode
      && (op == EQ || op == NE
	  || op == GEU || op == GTU || op == LTU || op == LEU)
      && CONST_INT_P (y))
    return CC_Zmode;

  /* A construct for a conditional compare, if the false arm contains
     0, then both conditions must be true, otherwise either condition
     must be true.  Not all conditions are possible, so CCmode is
     returned if it can't be done.  */
  if (GET_CODE (x) == IF_THEN_ELSE
      && (XEXP (x, 2) == const0_rtx
	  || XEXP (x, 2) == const1_rtx)
      && COMPARISON_P (XEXP (x, 0))
      && COMPARISON_P (XEXP (x, 1)))
    return arm_select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1),
					 INTVAL (XEXP (x, 2)));

  /* Alternate canonicalizations of the above.  These are somewhat cleaner.  */
  if (GET_CODE (x) == AND
      && (op == EQ || op == NE)
      && COMPARISON_P (XEXP (x, 0))
      && COMPARISON_P (XEXP (x, 1)))
    return arm_select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1),
					 DOM_CC_X_AND_Y);

  if (GET_CODE (x) == IOR
      && (op == EQ || op == NE)
      && COMPARISON_P (XEXP (x, 0))
      && COMPARISON_P (XEXP (x, 1)))
    return arm_select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1),
					 DOM_CC_X_OR_Y);

  /* An operation (on Thumb) where we want to test for a single bit.
     This is done by shifting that bit up into the top bit of a
     scratch register; we can then branch on the sign bit.  */
  if (TARGET_THUMB1
      && GET_MODE (x) == SImode
      && (op == EQ || op == NE)
      && GET_CODE (x) == ZERO_EXTRACT
      && XEXP (x, 1) == const1_rtx)
    return CC_Nmode;

  /* An operation that sets the condition codes as a side-effect, the
     V flag is not set correctly, so we can only use comparisons where
     this doesn't matter.  (For LT and GE we can use "mi" and "pl"
     instead.)  */
  /* ??? Does the ZERO_EXTRACT case really apply to thumb2?  */
  if (GET_MODE (x) == SImode
      && y == const0_rtx
      && (op == EQ || op == NE || op == LT || op == GE)
      && (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS
	  || GET_CODE (x) == AND || GET_CODE (x) == IOR
	  || GET_CODE (x) == XOR || GET_CODE (x) == MULT
	  || GET_CODE (x) == NOT || GET_CODE (x) == NEG
	  || GET_CODE (x) == LSHIFTRT
	  || GET_CODE (x) == ASHIFT || GET_CODE (x) == ASHIFTRT
	  || GET_CODE (x) == ROTATERT
	  || (TARGET_32BIT && GET_CODE (x) == ZERO_EXTRACT)))
    return CC_NOOVmode;

  if (GET_MODE (x) == QImode && (op == EQ || op == NE))
    return CC_Zmode;

  if (GET_MODE (x) == SImode && (op == LTU || op == GEU)
      && GET_CODE (x) == PLUS
      && (rtx_equal_p (XEXP (x, 0), y) || rtx_equal_p (XEXP (x, 1), y)))
    return CC_Cmode;

  if (GET_MODE (x) == DImode || GET_MODE (y) == DImode)
    {
      switch (op)
	{
	case EQ:
	case NE:
	  /* A DImode comparison against zero can be implemented by
	     or'ing the two halves together.  */
	  if (y == const0_rtx)
	    return CC_Zmode;

	  /* We can do an equality test in three Thumb instructions.  */
	  if (!TARGET_32BIT)
	    return CC_Zmode;

	  /* FALLTHROUGH */

	case LTU:
	case LEU:
	case GTU:
	case GEU:
	  /* DImode unsigned comparisons can be implemented by cmp +
	     cmpeq without a scratch register.  Not worth doing in
	     Thumb-2.  */
	  if (TARGET_32BIT)
	    return CC_CZmode;

	  /* FALLTHROUGH */

	case LT:
	case LE:
	case GT:
	case GE:
	  /* DImode signed and unsigned comparisons can be implemented
	     by cmp + sbcs with a scratch register, but that does not
	     set the Z flag - we must reverse GT/LE/GTU/LEU.  */
	  gcc_assert (op != EQ && op != NE);
	  return CC_NCVmode;

	default:
	  gcc_unreachable ();
	}
    }

  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_CC)
    return GET_MODE (x);

  return CCmode;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  FP means this is a
   floating point compare: I don't think that it is needed on the arm.  */
rtx
arm_gen_compare_reg (enum rtx_code code, rtx x, rtx y, rtx scratch)
{
  machine_mode mode;
  rtx cc_reg;
  int dimode_comparison = GET_MODE (x) == DImode || GET_MODE (y) == DImode;

  /* We might have X as a constant, Y as a register because of the predicates
     used for cmpdi.  If so, force X to a register here.  */
  if (dimode_comparison && !REG_P (x))
    x = force_reg (DImode, x);

  mode = SELECT_CC_MODE (code, x, y);
  cc_reg = gen_rtx_REG (mode, CC_REGNUM);

  if (dimode_comparison
      && mode != CC_CZmode)
    {
      rtx clobber, set;

      /* To compare two non-zero values for equality, XOR them and
	 then compare against zero.  Not used for ARM mode; there
	 CC_CZmode is cheaper.  */
      if (mode == CC_Zmode && y != const0_rtx)
	{
	  gcc_assert (!reload_completed);
	  x = expand_binop (DImode, xor_optab, x, y, NULL_RTX, 0, OPTAB_WIDEN);
	  y = const0_rtx;
	}

      /* A scratch register is required.  */
      if (reload_completed)
	gcc_assert (scratch != NULL && GET_MODE (scratch) == SImode);
      else
	scratch = gen_rtx_SCRATCH (SImode);

      clobber = gen_rtx_CLOBBER (VOIDmode, scratch);
      set = gen_rtx_SET (cc_reg, gen_rtx_COMPARE (mode, x, y));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));
    }
  else
    emit_set_insn (cc_reg, gen_rtx_COMPARE (mode, x, y));

  return cc_reg;
}

/* Generate a sequence of insns that will generate the correct return
   address mask depending on the physical architecture that the program
   is running on.  */
rtx
arm_gen_return_addr_mask (void)
{
  rtx reg = gen_reg_rtx (Pmode);

  emit_insn (gen_return_addr_mask (reg));
  return reg;
}

void
arm_reload_in_hi (rtx *operands)
{
  rtx ref = operands[1];
  rtx base, scratch;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_BYTE (ref);
      ref = SUBREG_REG (ref);
    }

  if (REG_P (ref))
    {
      /* We have a pseudo which has been spilt onto the stack; there
	 are two cases here: the first where there is a simple
	 stack-slot replacement and a second where the stack-slot is
	 out of range, or is used as a subreg.  */
      if (reg_equiv_mem (REGNO (ref)))
	{
	  ref = reg_equiv_mem (REGNO (ref));
	  base = find_replacement (&XEXP (ref, 0));
	}
      else
	/* The slot is out of range, or was dressed up in a SUBREG.  */
	base = reg_equiv_address (REGNO (ref));

      /* PR 62554: If there is no equivalent memory location then just move
	 the value as an SImode register move.  This happens when the target
	 architecture variant does not have an HImode register move.  */
      if (base == NULL)
	{
	  gcc_assert (REG_P (operands[0]));
	  emit_insn (gen_movsi (gen_rtx_SUBREG (SImode, operands[0], 0),
				gen_rtx_SUBREG (SImode, ref, 0)));
	  return;
	}
    }
  else
    base = find_replacement (&XEXP (ref, 0));

  /* Handle the case where the address is too complex to be offset by 1.  */
  if (GET_CODE (base) == MINUS
      || (GET_CODE (base) == PLUS && !CONST_INT_P (XEXP (base, 1))))
    {
      rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

      emit_set_insn (base_plus, base);
      base = base_plus;
    }
  else if (GET_CODE (base) == PLUS)
    {
      /* The addend must be CONST_INT, or we would have dealt with it above.  */
      HOST_WIDE_INT hi, lo;

      offset += INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);

      /* Rework the address into a legal sequence of insns.  */
      /* Valid range for lo is -4095 -> 4095 */
      lo = (offset >= 0
	    ? (offset & 0xfff)
	    : -((-offset) & 0xfff));

      /* Corner case, if lo is the max offset then we would be out of range
	 once we have added the additional 1 below, so bump the msb into the
	 pre-loading insn(s).  */
      if (lo == 4095)
	lo &= 0x7ff;

      hi = ((((offset - lo) & (HOST_WIDE_INT) 0xffffffff)
	     ^ (HOST_WIDE_INT) 0x80000000)
	    - (HOST_WIDE_INT) 0x80000000);

      gcc_assert (hi + lo == offset);

      if (hi != 0)
	{
	  rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

	  /* Get the base address; addsi3 knows how to handle constants
	     that require more than one insn.  */
	  emit_insn (gen_addsi3 (base_plus, base, GEN_INT (hi)));
	  base = base_plus;
	  offset = lo;
	}
    }

  /* Operands[2] may overlap operands[0] (though it won't overlap
     operands[1]), that's why we asked for a DImode reg -- so we can
     use the bit that does not overlap.  */
  if (REGNO (operands[2]) == REGNO (operands[0]))
    scratch = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);
  else
    scratch = gen_rtx_REG (SImode, REGNO (operands[2]));

  emit_insn (gen_zero_extendqisi2 (scratch,
				   gen_rtx_MEM (QImode,
						plus_constant (Pmode, base,
							       offset))));
  emit_insn (gen_zero_extendqisi2 (gen_rtx_SUBREG (SImode, operands[0], 0),
				   gen_rtx_MEM (QImode,
						plus_constant (Pmode, base,
							       offset + 1))));
  if (!BYTES_BIG_ENDIAN)
    emit_set_insn (gen_rtx_SUBREG (SImode, operands[0], 0),
		   gen_rtx_IOR (SImode,
				gen_rtx_ASHIFT
				(SImode,
				 gen_rtx_SUBREG (SImode, operands[0], 0),
				 GEN_INT (8)),
				scratch));
  else
    emit_set_insn (gen_rtx_SUBREG (SImode, operands[0], 0),
		   gen_rtx_IOR (SImode,
				gen_rtx_ASHIFT (SImode, scratch,
						GEN_INT (8)),
				gen_rtx_SUBREG (SImode, operands[0], 0)));
}

/* Handle storing a half-word to memory during reload by synthesizing as two
   byte stores.  Take care not to clobber the input values until after we
   have moved them somewhere safe.  This code assumes that if the DImode
   scratch in operands[2] overlaps either the input value or output address
   in some way, then that value must die in this insn (we absolutely need
   two scratch registers for some corner cases).  */
void
arm_reload_out_hi (rtx *operands)
{
  rtx ref = operands[0];
  rtx outval = operands[1];
  rtx base, scratch;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_BYTE (ref);
      ref = SUBREG_REG (ref);
    }

  if (REG_P (ref))
    {
      /* We have a pseudo which has been spilt onto the stack; there
	 are two cases here: the first where there is a simple
	 stack-slot replacement and a second where the stack-slot is
	 out of range, or is used as a subreg.  */
      if (reg_equiv_mem (REGNO (ref)))
	{
	  ref = reg_equiv_mem (REGNO (ref));
	  base = find_replacement (&XEXP (ref, 0));
	}
      else
	/* The slot is out of range, or was dressed up in a SUBREG.  */
	base = reg_equiv_address (REGNO (ref));

      /* PR 62254: If there is no equivalent memory location then just move
	 the value as an SImode register move.  This happens when the target
	 architecture variant does not have an HImode register move.  */
      if (base == NULL)
	{
	  gcc_assert (REG_P (outval) || SUBREG_P (outval));

	  if (REG_P (outval))
	    {
	      emit_insn (gen_movsi (gen_rtx_SUBREG (SImode, ref, 0),
				    gen_rtx_SUBREG (SImode, outval, 0)));
	    }
	  else /* SUBREG_P (outval)  */
	    {
	      if (GET_MODE (SUBREG_REG (outval)) == SImode)
		emit_insn (gen_movsi (gen_rtx_SUBREG (SImode, ref, 0),
				      SUBREG_REG (outval)));
	      else
		/* FIXME: Handle other cases ?  */
		gcc_unreachable ();
	    }
	  return;
	}
    }
  else
    base = find_replacement (&XEXP (ref, 0));

  scratch = gen_rtx_REG (SImode, REGNO (operands[2]));

  /* Handle the case where the address is too complex to be offset by 1.  */
  if (GET_CODE (base) == MINUS
      || (GET_CODE (base) == PLUS && !CONST_INT_P (XEXP (base, 1))))
    {
      rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

      /* Be careful not to destroy OUTVAL.  */
      if (reg_overlap_mentioned_p (base_plus, outval))
	{
	  /* Updating base_plus might destroy outval, see if we can
	     swap the scratch and base_plus.  */
	  if (!reg_overlap_mentioned_p (scratch, outval))
	    std::swap (scratch, base_plus);
	  else
	    {
	      rtx scratch_hi = gen_rtx_REG (HImode, REGNO (operands[2]));

	      /* Be conservative and copy OUTVAL into the scratch now,
		 this should only be necessary if outval is a subreg
		 of something larger than a word.  */
	      /* XXX Might this clobber base?  I can't see how it can,
		 since scratch is known to overlap with OUTVAL, and
		 must be wider than a word.  */
	      emit_insn (gen_movhi (scratch_hi, outval));
	      outval = scratch_hi;
	    }
	}

      emit_set_insn (base_plus, base);
      base = base_plus;
    }
  else if (GET_CODE (base) == PLUS)
    {
      /* The addend must be CONST_INT, or we would have dealt with it above.  */
      HOST_WIDE_INT hi, lo;

      offset += INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);

      /* Rework the address into a legal sequence of insns.  */
      /* Valid range for lo is -4095 -> 4095 */
      lo = (offset >= 0
	    ? (offset & 0xfff)
	    : -((-offset) & 0xfff));

      /* Corner case, if lo is the max offset then we would be out of range
	 once we have added the additional 1 below, so bump the msb into the
	 pre-loading insn(s).  */
      if (lo == 4095)
	lo &= 0x7ff;

      hi = ((((offset - lo) & (HOST_WIDE_INT) 0xffffffff)
	     ^ (HOST_WIDE_INT) 0x80000000)
	    - (HOST_WIDE_INT) 0x80000000);

      gcc_assert (hi + lo == offset);

      if (hi != 0)
	{
	  rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

	  /* Be careful not to destroy OUTVAL.  */
	  if (reg_overlap_mentioned_p (base_plus, outval))
	    {
	      /* Updating base_plus might destroy outval, see if we
		 can swap the scratch and base_plus.  */
	      if (!reg_overlap_mentioned_p (scratch, outval))
	        std::swap (scratch, base_plus);
	      else
		{
		  rtx scratch_hi = gen_rtx_REG (HImode, REGNO (operands[2]));

		  /* Be conservative and copy outval into scratch now,
		     this should only be necessary if outval is a
		     subreg of something larger than a word.  */
		  /* XXX Might this clobber base?  I can't see how it
		     can, since scratch is known to overlap with
		     outval.  */
		  emit_insn (gen_movhi (scratch_hi, outval));
		  outval = scratch_hi;
		}
	    }

	  /* Get the base address; addsi3 knows how to handle constants
	     that require more than one insn.  */
	  emit_insn (gen_addsi3 (base_plus, base, GEN_INT (hi)));
	  base = base_plus;
	  offset = lo;
	}
    }

  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_movqi (gen_rtx_MEM (QImode,
					 plus_constant (Pmode, base,
							offset + 1)),
			    gen_lowpart (QImode, outval)));
      emit_insn (gen_lshrsi3 (scratch,
			      gen_rtx_SUBREG (SImode, outval, 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, plus_constant (Pmode, base,
								offset)),
			    gen_lowpart (QImode, scratch)));
    }
  else
    {
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, plus_constant (Pmode, base,
								offset)),
			    gen_lowpart (QImode, outval)));
      emit_insn (gen_lshrsi3 (scratch,
			      gen_rtx_SUBREG (SImode, outval, 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi (gen_rtx_MEM (QImode,
					 plus_constant (Pmode, base,
							offset + 1)),
			    gen_lowpart (QImode, scratch)));
    }
}

/* Return true if a type must be passed in memory. For AAPCS, small aggregates
   (padded to the size of a word) should be passed in a register.  */

static bool
arm_must_pass_in_stack (machine_mode mode, const_tree type)
{
  if (TARGET_AAPCS_BASED)
    return must_pass_in_stack_var_size (mode, type);
  else
    return must_pass_in_stack_var_size_or_pad (mode, type);
}


/* Implement TARGET_FUNCTION_ARG_PADDING; return PAD_UPWARD if the lowest
   byte of a stack argument has useful data.  For legacy APCS ABIs we use
   the default.  For AAPCS based ABIs small aggregate types are placed
   in the lowest memory address.  */

static pad_direction
arm_function_arg_padding (machine_mode mode, const_tree type)
{
  if (!TARGET_AAPCS_BASED)
    return default_function_arg_padding (mode, type);

  if (type && BYTES_BIG_ENDIAN && INTEGRAL_TYPE_P (type))
    return PAD_DOWNWARD;

  return PAD_UPWARD;
}


/* Similarly, for use by BLOCK_REG_PADDING (MODE, TYPE, FIRST).
   Return !BYTES_BIG_ENDIAN if the least significant byte of the
   register has useful data, and return the opposite if the most
   significant byte does.  */

bool
arm_pad_reg_upward (machine_mode mode,
                    tree type, int first ATTRIBUTE_UNUSED)
{
  if (TARGET_AAPCS_BASED && BYTES_BIG_ENDIAN)
    {
      /* For AAPCS, small aggregates, small fixed-point types,
	 and small complex types are always padded upwards.  */
      if (type)
	{
	  if ((AGGREGATE_TYPE_P (type)
	       || TREE_CODE (type) == COMPLEX_TYPE
	       || FIXED_POINT_TYPE_P (type))
	      && int_size_in_bytes (type) <= 4)
	    return true;
	}
      else
	{
	  if ((COMPLEX_MODE_P (mode) || ALL_FIXED_POINT_MODE_P (mode))
	      && GET_MODE_SIZE (mode) <= 4)
	    return true;
	}
    }

  /* Otherwise, use default padding.  */
  return !BYTES_BIG_ENDIAN;
}

/* Returns true iff OFFSET is valid for use in an LDRD/STRD instruction,
   assuming that the address in the base register is word aligned.  */
bool
offset_ok_for_ldrd_strd (HOST_WIDE_INT offset)
{
  HOST_WIDE_INT max_offset;

  /* Offset must be a multiple of 4 in Thumb mode.  */
  if (TARGET_THUMB2 && ((offset & 3) != 0))
    return false;

  if (TARGET_THUMB2)
    max_offset = 1020;
  else if (TARGET_ARM)
    max_offset = 255;
  else
    return false;

  return ((offset <= max_offset) && (offset >= -max_offset));
}

/* Checks whether the operands are valid for use in an LDRD/STRD instruction.
   Assumes that RT, RT2, and RN are REG.  This is guaranteed by the patterns.
   Assumes that the address in the base register RN is word aligned.  Pattern
   guarantees that both memory accesses use the same base register,
   the offsets are constants within the range, and the gap between the offsets is 4.
   If preload complete then check that registers are legal.  WBACK indicates whether
   address is updated.  LOAD indicates whether memory access is load or store.  */
bool
operands_ok_ldrd_strd (rtx rt, rtx rt2, rtx rn, HOST_WIDE_INT offset,
                       bool wback, bool load)
{
  unsigned int t, t2, n;

  if (!reload_completed)
    return true;

  if (!offset_ok_for_ldrd_strd (offset))
    return false;

  t = REGNO (rt);
  t2 = REGNO (rt2);
  n = REGNO (rn);

  if ((TARGET_THUMB2)
      && ((wback && (n == t || n == t2))
          || (t == SP_REGNUM)
          || (t == PC_REGNUM)
          || (t2 == SP_REGNUM)
          || (t2 == PC_REGNUM)
          || (!load && (n == PC_REGNUM))
          || (load && (t == t2))
          /* Triggers Cortex-M3 LDRD errata.  */
          || (!wback && load && fix_cm3_ldrd && (n == t))))
    return false;

  if ((TARGET_ARM)
      && ((wback && (n == t || n == t2))
          || (t2 == PC_REGNUM)
          || (t % 2 != 0)   /* First destination register is not even.  */
          || (t2 != t + 1)
          /* PC can be used as base register (for offset addressing only),
             but it is depricated.  */
          || (n == PC_REGNUM)))
    return false;

  return true;
}

/* Return true if a 64-bit access with alignment ALIGN and with a
   constant offset OFFSET from the base pointer is permitted on this
   architecture.  */
static bool
align_ok_ldrd_strd (HOST_WIDE_INT align, HOST_WIDE_INT offset)
{
  return (unaligned_access
	  ? (align >= BITS_PER_WORD && (offset & 3) == 0)
	  : (align >= 2 * BITS_PER_WORD && (offset & 7) == 0));
}

/* Helper for gen_operands_ldrd_strd.  Returns true iff the memory
   operand MEM's address contains an immediate offset from the base
   register and has no side effects, in which case it sets BASE,
   OFFSET and ALIGN accordingly.  */
static bool
mem_ok_for_ldrd_strd (rtx mem, rtx *base, rtx *offset, HOST_WIDE_INT *align)
{
  rtx addr;

  gcc_assert (base != NULL && offset != NULL);

  /* TODO: Handle more general memory operand patterns, such as
     PRE_DEC and PRE_INC.  */

  if (side_effects_p (mem))
    return false;

  /* Can't deal with subregs.  */
  if (GET_CODE (mem) == SUBREG)
    return false;

  gcc_assert (MEM_P (mem));

  *offset = const0_rtx;
  *align = MEM_ALIGN (mem);

  addr = XEXP (mem, 0);

  /* If addr isn't valid for DImode, then we can't handle it.  */
  if (!arm_legitimate_address_p (DImode, addr,
				 reload_in_progress || reload_completed))
    return false;

  if (REG_P (addr))
    {
      *base = addr;
      return true;
    }
  else if (GET_CODE (addr) == PLUS || GET_CODE (addr) == MINUS)
    {
      *base = XEXP (addr, 0);
      *offset = XEXP (addr, 1);
      return (REG_P (*base) && CONST_INT_P (*offset));
    }

  return false;
}

/* Called from a peephole2 to replace two word-size accesses with a
   single LDRD/STRD instruction.  Returns true iff we can generate a
   new instruction sequence.  That is, both accesses use the same base
   register and the gap between constant offsets is 4.  This function
   may reorder its operands to match ldrd/strd RTL templates.
   OPERANDS are the operands found by the peephole matcher;
   OPERANDS[0,1] are register operands, and OPERANDS[2,3] are the
   corresponding memory operands.  LOAD indicaates whether the access
   is load or store.  CONST_STORE indicates a store of constant
   integer values held in OPERANDS[4,5] and assumes that the pattern
   is of length 4 insn, for the purpose of checking dead registers.
   COMMUTE indicates that register operands may be reordered.  */
bool
gen_operands_ldrd_strd (rtx *operands, bool load,
                        bool const_store, bool commute)
{
  int nops = 2;
  HOST_WIDE_INT offsets[2], offset, align[2];
  rtx base = NULL_RTX;
  rtx cur_base, cur_offset, tmp;
  int i, gap;
  HARD_REG_SET regset;

  gcc_assert (!const_store || !load);
  /* Check that the memory references are immediate offsets from the
     same base register.  Extract the base register, the destination
     registers, and the corresponding memory offsets.  */
  for (i = 0; i < nops; i++)
    {
      if (!mem_ok_for_ldrd_strd (operands[nops+i], &cur_base, &cur_offset,
				 &align[i]))
        return false;

      if (i == 0)
        base = cur_base;
      else if (REGNO (base) != REGNO (cur_base))
        return false;

      offsets[i] = INTVAL (cur_offset);
      if (GET_CODE (operands[i]) == SUBREG)
        {
          tmp = SUBREG_REG (operands[i]);
          gcc_assert (GET_MODE (operands[i]) == GET_MODE (tmp));
          operands[i] = tmp;
        }
    }

  /* Make sure there is no dependency between the individual loads.  */
  if (load && REGNO (operands[0]) == REGNO (base))
    return false; /* RAW */

  if (load && REGNO (operands[0]) == REGNO (operands[1]))
    return false; /* WAW */

  /* If the same input register is used in both stores
     when storing different constants, try to find a free register.
     For example, the code
	mov r0, 0
	str r0, [r2]
	mov r0, 1
	str r0, [r2, #4]
     can be transformed into
	mov r1, 0
	mov r0, 1
	strd r1, r0, [r2]
     in Thumb mode assuming that r1 is free.
     For ARM mode do the same but only if the starting register
     can be made to be even.  */
  if (const_store
      && REGNO (operands[0]) == REGNO (operands[1])
      && INTVAL (operands[4]) != INTVAL (operands[5]))
    {
    if (TARGET_THUMB2)
      {
        CLEAR_HARD_REG_SET (regset);
        tmp = peep2_find_free_register (0, 4, "r", SImode, &regset);
        if (tmp == NULL_RTX)
          return false;

        /* Use the new register in the first load to ensure that
           if the original input register is not dead after peephole,
           then it will have the correct constant value.  */
        operands[0] = tmp;
      }
    else if (TARGET_ARM)
      {
        int regno = REGNO (operands[0]);
        if (!peep2_reg_dead_p (4, operands[0]))
          {
            /* When the input register is even and is not dead after the
               pattern, it has to hold the second constant but we cannot
               form a legal STRD in ARM mode with this register as the second
               register.  */
            if (regno % 2 == 0)
              return false;

            /* Is regno-1 free? */
            SET_HARD_REG_SET (regset);
            CLEAR_HARD_REG_BIT(regset, regno - 1);
            tmp = peep2_find_free_register (0, 4, "r", SImode, &regset);
            if (tmp == NULL_RTX)
              return false;

            operands[0] = tmp;
          }
        else
          {
            /* Find a DImode register.  */
            CLEAR_HARD_REG_SET (regset);
            tmp = peep2_find_free_register (0, 4, "r", DImode, &regset);
            if (tmp != NULL_RTX)
              {
                operands[0] = simplify_gen_subreg (SImode, tmp, DImode, 0);
                operands[1] = simplify_gen_subreg (SImode, tmp, DImode, 4);
              }
            else
              {
                /* Can we use the input register to form a DI register?  */
                SET_HARD_REG_SET (regset);
                CLEAR_HARD_REG_BIT(regset,
                                   regno % 2 == 0 ? regno + 1 : regno - 1);
                tmp = peep2_find_free_register (0, 4, "r", SImode, &regset);
                if (tmp == NULL_RTX)
                  return false;
                operands[regno % 2 == 1 ? 0 : 1] = tmp;
              }
          }

        gcc_assert (operands[0] != NULL_RTX);
        gcc_assert (operands[1] != NULL_RTX);
        gcc_assert (REGNO (operands[0]) % 2 == 0);
        gcc_assert (REGNO (operands[1]) == REGNO (operands[0]) + 1);
      }
    }

  /* Make sure the instructions are ordered with lower memory access first.  */
  if (offsets[0] > offsets[1])
    {
      gap = offsets[0] - offsets[1];
      offset = offsets[1];

      /* Swap the instructions such that lower memory is accessed first.  */
      std::swap (operands[0], operands[1]);
      std::swap (operands[2], operands[3]);
      std::swap (align[0], align[1]);
      if (const_store)
        std::swap (operands[4], operands[5]);
    }
  else
    {
      gap = offsets[1] - offsets[0];
      offset = offsets[0];
    }

  /* Make sure accesses are to consecutive memory locations.  */
  if (gap != 4)
    return false;

  if (!align_ok_ldrd_strd (align[0], offset))
    return false;

  /* Make sure we generate legal instructions.  */
  if (operands_ok_ldrd_strd (operands[0], operands[1], base, offset,
                             false, load))
    return true;

  /* In Thumb state, where registers are almost unconstrained, there
     is little hope to fix it.  */
  if (TARGET_THUMB2)
    return false;

  if (load && commute)
    {
      /* Try reordering registers.  */
      std::swap (operands[0], operands[1]);
      if (operands_ok_ldrd_strd (operands[0], operands[1], base, offset,
                                 false, load))
        return true;
    }

  if (const_store)
    {
      /* If input registers are dead after this pattern, they can be
         reordered or replaced by other registers that are free in the
         current pattern.  */
      if (!peep2_reg_dead_p (4, operands[0])
          || !peep2_reg_dead_p (4, operands[1]))
        return false;

      /* Try to reorder the input registers.  */
      /* For example, the code
           mov r0, 0
           mov r1, 1
           str r1, [r2]
           str r0, [r2, #4]
         can be transformed into
           mov r1, 0
           mov r0, 1
           strd r0, [r2]
      */
      if (operands_ok_ldrd_strd (operands[1], operands[0], base, offset,
                                  false, false))
        {
          std::swap (operands[0], operands[1]);
          return true;
        }

      /* Try to find a free DI register.  */
      CLEAR_HARD_REG_SET (regset);
      add_to_hard_reg_set (&regset, SImode, REGNO (operands[0]));
      add_to_hard_reg_set (&regset, SImode, REGNO (operands[1]));
      while (true)
        {
          tmp = peep2_find_free_register (0, 4, "r", DImode, &regset);
          if (tmp == NULL_RTX)
            return false;

          /* DREG must be an even-numbered register in DImode.
             Split it into SI registers.  */
          operands[0] = simplify_gen_subreg (SImode, tmp, DImode, 0);
          operands[1] = simplify_gen_subreg (SImode, tmp, DImode, 4);
          gcc_assert (operands[0] != NULL_RTX);
          gcc_assert (operands[1] != NULL_RTX);
          gcc_assert (REGNO (operands[0]) % 2 == 0);
          gcc_assert (REGNO (operands[0]) + 1 == REGNO (operands[1]));

          return (operands_ok_ldrd_strd (operands[0], operands[1],
                                         base, offset,
                                         false, load));
        }
    }

  return false;
}




/* Print a symbolic form of X to the debug file, F.  */
static void
arm_print_value (FILE *f, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      fprintf (f, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      return;

    case CONST_DOUBLE:
      fprintf (f, "<0x%lx,0x%lx>", (long)XWINT (x, 2), (long)XWINT (x, 3));
      return;

    case CONST_VECTOR:
      {
	int i;

	fprintf (f, "<");
	for (i = 0; i < CONST_VECTOR_NUNITS (x); i++)
	  {
	    fprintf (f, HOST_WIDE_INT_PRINT_HEX, INTVAL (CONST_VECTOR_ELT (x, i)));
	    if (i < (CONST_VECTOR_NUNITS (x) - 1))
	      fputc (',', f);
	  }
	fprintf (f, ">");
      }
      return;

    case CONST_STRING:
      fprintf (f, "\"%s\"", XSTR (x, 0));
      return;

    case SYMBOL_REF:
      fprintf (f, "`%s'", XSTR (x, 0));
      return;

    case LABEL_REF:
      fprintf (f, "L%d", INSN_UID (XEXP (x, 0)));
      return;

    case CONST:
      arm_print_value (f, XEXP (x, 0));
      return;

    case PLUS:
      arm_print_value (f, XEXP (x, 0));
      fprintf (f, "+");
      arm_print_value (f, XEXP (x, 1));
      return;

    case PC:
      fprintf (f, "pc");
      return;

    default:
      fprintf (f, "????");
      return;
    }
}

/* Routines for manipulation of the constant pool.  */

/* Arm instructions cannot load a large constant directly into a
   register; they have to come from a pc relative load.  The constant
   must therefore be placed in the addressable range of the pc
   relative load.  Depending on the precise pc relative load
   instruction the range is somewhere between 256 bytes and 4k.  This
   means that we often have to dump a constant inside a function, and
   generate code to branch around it.

   It is important to minimize this, since the branches will slow
   things down and make the code larger.

   Normally we can hide the table after an existing unconditional
   branch so that there is no interruption of the flow, but in the
   worst case the code looks like this:

	ldr	rn, L1
	...
	b	L2
	align
	L1:	.long value
	L2:
	...

	ldr	rn, L3
	...
	b	L4
	align
	L3:	.long value
	L4:
	...

   We fix this by performing a scan after scheduling, which notices
   which instructions need to have their operands fetched from the
   constant table and builds the table.

   The algorithm starts by building a table of all the constants that
   need fixing up and all the natural barriers in the function (places
   where a constant table can be dropped without breaking the flow).
   For each fixup we note how far the pc-relative replacement will be
   able to reach and the offset of the instruction into the function.

   Having built the table we then group the fixes together to form
   tables that are as large as possible (subject to addressing
   constraints) and emit each table of constants after the last
   barrier that is within range of all the instructions in the group.
   If a group does not contain a barrier, then we forcibly create one
   by inserting a jump instruction into the flow.  Once the table has
   been inserted, the insns are then modified to reference the
   relevant entry in the pool.

   Possible enhancements to the algorithm (not implemented) are:

   1) For some processors and object formats, there may be benefit in
   aligning the pools to the start of cache lines; this alignment
   would need to be taken into account when calculating addressability
   of a pool.  */

/* These typedefs are located at the start of this file, so that
   they can be used in the prototypes there.  This comment is to
   remind readers of that fact so that the following structures
   can be understood more easily.

     typedef struct minipool_node    Mnode;
     typedef struct minipool_fixup   Mfix;  */

struct minipool_node
{
  /* Doubly linked chain of entries.  */
  Mnode * next;
  Mnode * prev;
  /* The maximum offset into the code that this entry can be placed.  While
     pushing fixes for forward references, all entries are sorted in order
     of increasing max_address.  */
  HOST_WIDE_INT max_address;
  /* Similarly for an entry inserted for a backwards ref.  */
  HOST_WIDE_INT min_address;
  /* The number of fixes referencing this entry.  This can become zero
     if we "unpush" an entry.  In this case we ignore the entry when we
     come to emit the code.  */
  int refcount;
  /* The offset from the start of the minipool.  */
  HOST_WIDE_INT offset;
  /* The value in table.  */
  rtx value;
  /* The mode of value.  */
  machine_mode mode;
  /* The size of the value.  With iWMMXt enabled
     sizes > 4 also imply an alignment of 8-bytes.  */
  int fix_size;
};

struct minipool_fixup
{
  Mfix *            next;
  rtx_insn *        insn;
  HOST_WIDE_INT     address;
  rtx *             loc;
  machine_mode mode;
  int               fix_size;
  rtx               value;
  Mnode *           minipool;
  HOST_WIDE_INT     forwards;
  HOST_WIDE_INT     backwards;
};

/* Fixes less than a word need padding out to a word boundary.  */
#define MINIPOOL_FIX_SIZE(mode) \
  (GET_MODE_SIZE ((mode)) >= 4 ? GET_MODE_SIZE ((mode)) : 4)

static Mnode *	minipool_vector_head;
static Mnode *	minipool_vector_tail;
static rtx_code_label	*minipool_vector_label;
static int	minipool_pad;

/* The linked list of all minipool fixes required for this function.  */
Mfix * 		minipool_fix_head;
Mfix * 		minipool_fix_tail;
/* The fix entry for the current minipool, once it has been placed.  */
Mfix *		minipool_barrier;

#ifndef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 0
#endif

static HOST_WIDE_INT
get_jump_table_size (rtx_jump_table_data *insn)
{
  /* ADDR_VECs only take room if read-only data does into the text
     section.  */
  if (JUMP_TABLES_IN_TEXT_SECTION || readonly_data_section == text_section)
    {
      rtx body = PATTERN (insn);
      int elt = GET_CODE (body) == ADDR_DIFF_VEC ? 1 : 0;
      HOST_WIDE_INT size;
      HOST_WIDE_INT modesize;

      modesize = GET_MODE_SIZE (GET_MODE (body));
      size = modesize * XVECLEN (body, elt);
      switch (modesize)
	{
	case 1:
	  /* Round up size  of TBB table to a halfword boundary.  */
	  size = (size + 1) & ~HOST_WIDE_INT_1;
	  break;
	case 2:
	  /* No padding necessary for TBH.  */
	  break;
	case 4:
	  /* Add two bytes for alignment on Thumb.  */
	  if (TARGET_THUMB)
	    size += 2;
	  break;
	default:
	  gcc_unreachable ();
	}
      return size;
    }

  return 0;
}

/* Return the maximum amount of padding that will be inserted before
   label LABEL.  */

static HOST_WIDE_INT
get_label_padding (rtx label)
{
  HOST_WIDE_INT align, min_insn_size;

  align = 1 << label_to_alignment (label);
  min_insn_size = TARGET_THUMB ? 2 : 4;
  return align > min_insn_size ? align - min_insn_size : 0;
}

/* Move a minipool fix MP from its current location to before MAX_MP.
   If MAX_MP is NULL, then MP doesn't need moving, but the addressing
   constraints may need updating.  */
static Mnode *
move_minipool_fix_forward_ref (Mnode *mp, Mnode *max_mp,
			       HOST_WIDE_INT max_address)
{
  /* The code below assumes these are different.  */
  gcc_assert (mp != max_mp);

  if (max_mp == NULL)
    {
      if (max_address < mp->max_address)
	mp->max_address = max_address;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      /* Unlink MP from its current position.  Since max_mp is non-null,
       mp->prev must be non-null.  */
      mp->prev->next = mp->next;
      if (mp->next != NULL)
	mp->next->prev = mp->prev;
      else
	minipool_vector_tail = mp->prev;

      /* Re-insert it before MAX_MP.  */
      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;

      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as
     required.  */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}

/* Add a constant to the minipool for a forward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.  */
static Mnode *
add_minipool_forward_ref (Mfix *fix)
{
  /* If set, max_mp is the first pool_entry that has a lower
     constraint than the one we are trying to add.  */
  Mnode *       max_mp = NULL;
  HOST_WIDE_INT max_address = fix->address + fix->forwards - minipool_pad;
  Mnode *       mp;

  /* If the minipool starts before the end of FIX->INSN then this FIX
     can not be placed into the current pool.  Furthermore, adding the
     new constant pool entry may cause the pool to start FIX_SIZE bytes
     earlier.  */
  if (minipool_vector_head &&
      (fix->address + get_attr_length (fix->insn)
       >= minipool_vector_head->max_address - fix->fix_size))
    return NULL;

  /* Scan the pool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (!LABEL_P (fix->value)
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value))
	{
	  /* More than one fix references this entry.  */
	  mp->refcount++;
	  return move_minipool_fix_forward_ref (mp, max_mp, max_address);
	}

      /* Note the insertion point if necessary.  */
      if (max_mp == NULL
	  && mp->max_address > max_address)
	max_mp = mp;

      /* If we are inserting an 8-bytes aligned quantity and
	 we have not already found an insertion point, then
	 make sure that all such 8-byte aligned quantities are
	 placed at the start of the pool.  */
      if (ARM_DOUBLEWORD_ALIGN
	  && max_mp == NULL
	  && fix->fix_size >= 8
	  && mp->fix_size < 8)
	{
	  max_mp = mp;
	  max_address = mp->max_address;
	}
    }

  /* The value is not currently in the minipool, so we need to create
     a new entry for it.  If MAX_MP is NULL, the entry will be put on
     the end of the list since the placement is less constrained than
     any existing entry.  Otherwise, we insert the new fix before
     MAX_MP and, if necessary, adjust the constraints on the other
     entries.  */
  mp = XNEW (Mnode);
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  /* Not yet required for a backwards ref.  */
  mp->min_address = -65536;

  if (max_mp == NULL)
    {
      mp->max_address = max_address;
      mp->next = NULL;
      mp->prev = minipool_vector_tail;

      if (mp->prev == NULL)
	{
	  minipool_vector_head = mp;
	  minipool_vector_label = gen_label_rtx ();
	}
      else
	mp->prev->next = mp;

      minipool_vector_tail = mp;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;
      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as
     required.  */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}

static Mnode *
move_minipool_fix_backward_ref (Mnode *mp, Mnode *min_mp,
				HOST_WIDE_INT  min_address)
{
  HOST_WIDE_INT offset;

  /* The code below assumes these are different.  */
  gcc_assert (mp != min_mp);

  if (min_mp == NULL)
    {
      if (min_address > mp->min_address)
	mp->min_address = min_address;
    }
  else
    {
      /* We will adjust this below if it is too loose.  */
      mp->min_address = min_address;

      /* Unlink MP from its current position.  Since min_mp is non-null,
	 mp->next must be non-null.  */
      mp->next->prev = mp->prev;
      if (mp->prev != NULL)
	mp->prev->next = mp->next;
      else
	minipool_vector_head = mp->next;

      /* Reinsert it after MIN_MP.  */
      mp->prev = min_mp;
      mp->next = min_mp->next;
      min_mp->next = mp;
      if (mp->next != NULL)
	mp->next->prev = mp;
      else
	minipool_vector_tail = mp;
    }

  min_mp = mp;

  offset = 0;
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;
      if (mp->refcount > 0)
	offset += mp->fix_size;

      if (mp->next && mp->next->min_address < mp->min_address + mp->fix_size)
	mp->next->min_address = mp->min_address + mp->fix_size;
    }

  return min_mp;
}

/* Add a constant to the minipool for a backward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.

   Note that the code for insertion for a backwards reference can be
   somewhat confusing because the calculated offsets for each fix do
   not take into account the size of the pool (which is still under
   construction.  */
static Mnode *
add_minipool_backward_ref (Mfix *fix)
{
  /* If set, min_mp is the last pool_entry that has a lower constraint
     than the one we are trying to add.  */
  Mnode *min_mp = NULL;
  /* This can be negative, since it is only a constraint.  */
  HOST_WIDE_INT  min_address = fix->address - fix->backwards;
  Mnode *mp;

  /* If we can't reach the current pool from this insn, or if we can't
     insert this entry at the end of the pool without pushing other
     fixes out of range, then we don't try.  This ensures that we
     can't fail later on.  */
  if (min_address >= minipool_barrier->address
      || (minipool_vector_tail->min_address + fix->fix_size
	  >= minipool_barrier->address))
    return NULL;

  /* Scan the pool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  for (mp = minipool_vector_tail; mp != NULL; mp = mp->prev)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (!LABEL_P (fix->value)
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value)
	  /* Check that there is enough slack to move this entry to the
	     end of the table (this is conservative).  */
	  && (mp->max_address
	      > (minipool_barrier->address
		 + minipool_vector_tail->offset
		 + minipool_vector_tail->fix_size)))
	{
	  mp->refcount++;
	  return move_minipool_fix_backward_ref (mp, min_mp, min_address);
	}

      if (min_mp != NULL)
	mp->min_address += fix->fix_size;
      else
	{
	  /* Note the insertion point if necessary.  */
	  if (mp->min_address < min_address)
	    {
	      /* For now, we do not allow the insertion of 8-byte alignment
		 requiring nodes anywhere but at the start of the pool.  */
	      if (ARM_DOUBLEWORD_ALIGN
		  && fix->fix_size >= 8 && mp->fix_size < 8)
		return NULL;
	      else
		min_mp = mp;
	    }
	  else if (mp->max_address
		   < minipool_barrier->address + mp->offset + fix->fix_size)
	    {
	      /* Inserting before this entry would push the fix beyond
		 its maximum address (which can happen if we have
		 re-located a forwards fix); force the new fix to come
		 after it.  */
	      if (ARM_DOUBLEWORD_ALIGN
		  && fix->fix_size >= 8 && mp->fix_size < 8)
		return NULL;
	      else
		{
		  min_mp = mp;
		  min_address = mp->min_address + fix->fix_size;
		}
	    }
	  /* Do not insert a non-8-byte aligned quantity before 8-byte
	     aligned quantities.  */
	  else if (ARM_DOUBLEWORD_ALIGN
		   && fix->fix_size < 8
		   && mp->fix_size >= 8)
	    {
	      min_mp = mp;
	      min_address = mp->min_address + fix->fix_size;
	    }
	}
    }

  /* We need to create a new entry.  */
  mp = XNEW (Mnode);
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  mp->max_address = minipool_barrier->address + 65536;

  mp->min_address = min_address;

  if (min_mp == NULL)
    {
      mp->prev = NULL;
      mp->next = minipool_vector_head;

      if (mp->next == NULL)
	{
	  minipool_vector_tail = mp;
	  minipool_vector_label = gen_label_rtx ();
	}
      else
	mp->next->prev = mp;

      minipool_vector_head = mp;
    }
  else
    {
      mp->next = min_mp->next;
      mp->prev = min_mp;
      min_mp->next = mp;

      if (mp->next != NULL)
	mp->next->prev = mp;
      else
	minipool_vector_tail = mp;
    }

  /* Save the new entry.  */
  min_mp = mp;

  if (mp->prev)
    mp = mp->prev;
  else
    mp->offset = 0;

  /* Scan over the following entries and adjust their offsets.  */
  while (mp->next != NULL)
    {
      if (mp->next->min_address < mp->min_address + mp->fix_size)
	mp->next->min_address = mp->min_address + mp->fix_size;

      if (mp->refcount)
	mp->next->offset = mp->offset + mp->fix_size;
      else
	mp->next->offset = mp->offset;

      mp = mp->next;
    }

  return min_mp;
}

static void
assign_minipool_offsets (Mfix *barrier)
{
  HOST_WIDE_INT offset = 0;
  Mnode *mp;

  minipool_barrier = barrier;

  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;

      if (mp->refcount > 0)
	offset += mp->fix_size;
    }
}

/* Output the literal table */
static void
dump_minipool (rtx_insn *scan)
{
  Mnode * mp;
  Mnode * nmp;
  int align64 = 0;

  if (ARM_DOUBLEWORD_ALIGN)
    for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
      if (mp->refcount > 0 && mp->fix_size >= 8)
	{
	  align64 = 1;
	  break;
	}

  if (dump_file)
    fprintf (dump_file,
	     ";; Emitting minipool after insn %u; address %ld; align %d (bytes)\n",
	     INSN_UID (scan), (unsigned long) minipool_barrier->address, align64 ? 8 : 4);

  scan = emit_label_after (gen_label_rtx (), scan);
  scan = emit_insn_after (align64 ? gen_align_8 () : gen_align_4 (), scan);
  scan = emit_label_after (minipool_vector_label, scan);

  for (mp = minipool_vector_head; mp != NULL; mp = nmp)
    {
      if (mp->refcount > 0)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file,
		       ";;  Offset %u, min %ld, max %ld ",
		       (unsigned) mp->offset, (unsigned long) mp->min_address,
		       (unsigned long) mp->max_address);
	      arm_print_value (dump_file, mp->value);
	      fputc ('\n', dump_file);
	    }

	  rtx val = copy_rtx (mp->value);

	  switch (GET_MODE_SIZE (mp->mode))
	    {
#ifdef HAVE_consttable_1
	    case 1:
	      scan = emit_insn_after (gen_consttable_1 (val), scan);
	      break;

#endif
#ifdef HAVE_consttable_2
	    case 2:
	      scan = emit_insn_after (gen_consttable_2 (val), scan);
	      break;

#endif
#ifdef HAVE_consttable_4
	    case 4:
	      scan = emit_insn_after (gen_consttable_4 (val), scan);
	      break;

#endif
#ifdef HAVE_consttable_8
	    case 8:
	      scan = emit_insn_after (gen_consttable_8 (val), scan);
	      break;

#endif
#ifdef HAVE_consttable_16
	    case 16:
              scan = emit_insn_after (gen_consttable_16 (val), scan);
              break;

#endif
	    default:
	      gcc_unreachable ();
	    }
	}

      nmp = mp->next;
      free (mp);
    }

  minipool_vector_head = minipool_vector_tail = NULL;
  scan = emit_insn_after (gen_consttable_end (), scan);
  scan = emit_barrier_after (scan);
}

/* Return the cost of forcibly inserting a barrier after INSN.  */
static int
arm_barrier_cost (rtx_insn *insn)
{
  /* Basing the location of the pool on the loop depth is preferable,
     but at the moment, the basic block information seems to be
     corrupt by this stage of the compilation.  */
  int base_cost = 50;
  rtx_insn *next = next_nonnote_insn (insn);

  if (next != NULL && LABEL_P (next))
    base_cost -= 20;

  switch (GET_CODE (insn))
    {
    case CODE_LABEL:
      /* It will always be better to place the table before the label, rather
	 than after it.  */
      return 50;

    case INSN:
    case CALL_INSN:
      return base_cost;

    case JUMP_INSN:
      return base_cost - 10;

    default:
      return base_cost + 10;
    }
}

/* Find the best place in the insn stream in the range
   (FIX->address,MAX_ADDRESS) to forcibly insert a minipool barrier.
   Create the barrier by inserting a jump and add a new fix entry for
   it.  */
static Mfix *
create_fix_barrier (Mfix *fix, HOST_WIDE_INT max_address)
{
  HOST_WIDE_INT count = 0;
  rtx_barrier *barrier;
  rtx_insn *from = fix->insn;
  /* The instruction after which we will insert the jump.  */
  rtx_insn *selected = NULL;
  int selected_cost;
  /* The address at which the jump instruction will be placed.  */
  HOST_WIDE_INT selected_address;
  Mfix * new_fix;
  HOST_WIDE_INT max_count = max_address - fix->address;
  rtx_code_label *label = gen_label_rtx ();

  selected_cost = arm_barrier_cost (from);
  selected_address = fix->address;

  while (from && count < max_count)
    {
      rtx_jump_table_data *tmp;
      int new_cost;

      /* This code shouldn't have been called if there was a natural barrier
	 within range.  */
      gcc_assert (!BARRIER_P (from));

      /* Count the length of this insn.  This must stay in sync with the
	 code that pushes minipool fixes.  */
      if (LABEL_P (from))
	count += get_label_padding (from);
      else
	count += get_attr_length (from);

      /* If there is a jump table, add its length.  */
      if (tablejump_p (from, NULL, &tmp))
	{
	  count += get_jump_table_size (tmp);

	  /* Jump tables aren't in a basic block, so base the cost on
	     the dispatch insn.  If we select this location, we will
	     still put the pool after the table.  */
	  new_cost = arm_barrier_cost (from);

	  if (count < max_count
	      && (!selected || new_cost <= selected_cost))
	    {
	      selected = tmp;
	      selected_cost = new_cost;
	      selected_address = fix->address + count;
	    }

	  /* Continue after the dispatch table.  */
	  from = NEXT_INSN (tmp);
	  continue;
	}

      new_cost = arm_barrier_cost (from);

      if (count < max_count
	  && (!selected || new_cost <= selected_cost))
	{
	  selected = from;
	  selected_cost = new_cost;
	  selected_address = fix->address + count;
	}

      from = NEXT_INSN (from);
    }

  /* Make sure that we found a place to insert the jump.  */
  gcc_assert (selected);

  /* Create a new JUMP_INSN that branches around a barrier.  */
  from = emit_jump_insn_after (gen_jump (label), selected);
  JUMP_LABEL (from) = label;
  barrier = emit_barrier_after (from);
  emit_label_after (label, barrier);

  /* Create a minipool barrier entry for the new barrier.  */
  new_fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* new_fix));
  new_fix->insn = barrier;
  new_fix->address = selected_address;
  new_fix->next = fix->next;
  fix->next = new_fix;

  return new_fix;
}

/* Record that there is a natural barrier in the insn stream at
   ADDRESS.  */
static void
push_minipool_barrier (rtx_insn *insn, HOST_WIDE_INT address)
{
  Mfix * fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* fix));

  fix->insn = insn;
  fix->address = address;

  fix->next = NULL;
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Record INSN, which will need fixing up to load a value from the
   minipool.  ADDRESS is the offset of the insn since the start of the
   function; LOC is a pointer to the part of the insn which requires
   fixing; VALUE is the constant that must be loaded, which is of type
   MODE.  */
static void
push_minipool_fix (rtx_insn *insn, HOST_WIDE_INT address, rtx *loc,
		   machine_mode mode, rtx value)
{
  gcc_assert (!arm_disable_literal_pool);
  Mfix * fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* fix));

  fix->insn = insn;
  fix->address = address;
  fix->loc = loc;
  fix->mode = mode;
  fix->fix_size = MINIPOOL_FIX_SIZE (mode);
  fix->value = value;
  fix->forwards = get_attr_pool_range (insn);
  fix->backwards = get_attr_neg_pool_range (insn);
  fix->minipool = NULL;

  /* If an insn doesn't have a range defined for it, then it isn't
     expecting to be reworked by this code.  Better to stop now than
     to generate duff assembly code.  */
  gcc_assert (fix->forwards || fix->backwards);

  /* If an entry requires 8-byte alignment then assume all constant pools
     require 4 bytes of padding.  Trying to do this later on a per-pool
     basis is awkward because existing pool entries have to be modified.  */
  if (ARM_DOUBLEWORD_ALIGN && fix->fix_size >= 8)
    minipool_pad = 4;

  if (dump_file)
    {
      fprintf (dump_file,
	       ";; %smode fixup for i%d; addr %lu, range (%ld,%ld): ",
	       GET_MODE_NAME (mode),
	       INSN_UID (insn), (unsigned long) address,
	       -1 * (long)fix->backwards, (long)fix->forwards);
      arm_print_value (dump_file, fix->value);
      fprintf (dump_file, "\n");
    }

  /* Add it to the chain of fixes.  */
  fix->next = NULL;

  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Return maximum allowed cost of synthesizing a 64-bit constant VAL inline.
   Returns the number of insns needed, or 99 if we always want to synthesize
   the value.  */
int
arm_max_const_double_inline_cost ()
{
  return ((optimize_size || arm_ld_sched) ? 3 : 4);
}

/* Return the cost of synthesizing a 64-bit constant VAL inline.
   Returns the number of insns needed, or 99 if we don't know how to
   do it.  */
int
arm_const_double_inline_cost (rtx val)
{
  rtx lowpart, highpart;
  machine_mode mode;

  mode = GET_MODE (val);

  if (mode == VOIDmode)
    mode = DImode;

  gcc_assert (GET_MODE_SIZE (mode) == 8);

  lowpart = gen_lowpart (SImode, val);
  highpart = gen_highpart_mode (SImode, mode, val);

  gcc_assert (CONST_INT_P (lowpart));
  gcc_assert (CONST_INT_P (highpart));

  return (arm_gen_constant (SET, SImode, NULL_RTX, INTVAL (lowpart),
			    NULL_RTX, NULL_RTX, 0, 0)
	  + arm_gen_constant (SET, SImode, NULL_RTX, INTVAL (highpart),
			      NULL_RTX, NULL_RTX, 0, 0));
}

/* Cost of loading a SImode constant.  */
static inline int
arm_const_inline_cost (enum rtx_code code, rtx val)
{
  return arm_gen_constant (code, SImode, NULL_RTX, INTVAL (val),
                           NULL_RTX, NULL_RTX, 1, 0);
}

/* Return true if it is worthwhile to split a 64-bit constant into two
   32-bit operations.  This is the case if optimizing for size, or
   if we have load delay slots, or if one 32-bit part can be done with
   a single data operation.  */
bool
arm_const_double_by_parts (rtx val)
{
  machine_mode mode = GET_MODE (val);
  rtx part;

  if (optimize_size || arm_ld_sched)
    return true;

  if (mode == VOIDmode)
    mode = DImode;

  part = gen_highpart_mode (SImode, mode, val);

  gcc_assert (CONST_INT_P (part));

  if (const_ok_for_arm (INTVAL (part))
      || const_ok_for_arm (~INTVAL (part)))
    return true;

  part = gen_lowpart (SImode, val);

  gcc_assert (CONST_INT_P (part));

  if (const_ok_for_arm (INTVAL (part))
      || const_ok_for_arm (~INTVAL (part)))
    return true;

  return false;
}

/* Return true if it is possible to inline both the high and low parts
   of a 64-bit constant into 32-bit data processing instructions.  */
bool
arm_const_double_by_immediates (rtx val)
{
  machine_mode mode = GET_MODE (val);
  rtx part;

  if (mode == VOIDmode)
    mode = DImode;

  part = gen_highpart_mode (SImode, mode, val);

  gcc_assert (CONST_INT_P (part));

  if (!const_ok_for_arm (INTVAL (part)))
    return false;

  part = gen_lowpart (SImode, val);

  gcc_assert (CONST_INT_P (part));

  if (!const_ok_for_arm (INTVAL (part)))
    return false;

  return true;
}

/* Scan INSN and note any of its operands that need fixing.
   If DO_PUSHES is false we do not actually push any of the fixups
   needed.  */
static void
note_invalid_constants (rtx_insn *insn, HOST_WIDE_INT address, int do_pushes)
{
  int opno;

  extract_constrain_insn (insn);

  if (recog_data.n_alternatives == 0)
    return;

  /* Fill in recog_op_alt with information about the constraints of
     this insn.  */
  preprocess_constraints (insn);

  const operand_alternative *op_alt = which_op_alt ();
  for (opno = 0; opno < recog_data.n_operands; opno++)
    {
      /* Things we need to fix can only occur in inputs.  */
      if (recog_data.operand_type[opno] != OP_IN)
	continue;

      /* If this alternative is a memory reference, then any mention
	 of constants in this alternative is really to fool reload
	 into allowing us to accept one there.  We need to fix them up
	 now so that we output the right code.  */
      if (op_alt[opno].memory_ok)
	{
	  rtx op = recog_data.operand[opno];

	  if (CONSTANT_P (op))
	    {
	      if (do_pushes)
		push_minipool_fix (insn, address, recog_data.operand_loc[opno],
				   recog_data.operand_mode[opno], op);
	    }
	  else if (MEM_P (op)
		   && GET_CODE (XEXP (op, 0)) == SYMBOL_REF
		   && CONSTANT_POOL_ADDRESS_P (XEXP (op, 0)))
	    {
	      if (do_pushes)
		{
		  rtx cop = avoid_constant_pool_reference (op);

		  /* Casting the address of something to a mode narrower
		     than a word can cause avoid_constant_pool_reference()
		     to return the pool reference itself.  That's no good to
		     us here.  Lets just hope that we can use the
		     constant pool value directly.  */
		  if (op == cop)
		    cop = get_pool_constant (XEXP (op, 0));

		  push_minipool_fix (insn, address,
				     recog_data.operand_loc[opno],
				     recog_data.operand_mode[opno], cop);
		}

	    }
	}
    }

  return;
}

/* This function computes the clear mask and PADDING_BITS_TO_CLEAR for structs
   and unions in the context of ARMv8-M Security Extensions.  It is used as a
   helper function for both 'cmse_nonsecure_call' and 'cmse_nonsecure_entry'
   functions.  The PADDING_BITS_TO_CLEAR pointer can be the base to either one
   or four masks, depending on whether it is being computed for a
   'cmse_nonsecure_entry' return value or a 'cmse_nonsecure_call' argument
   respectively.  The tree for the type of the argument or a field within an
   argument is passed in ARG_TYPE, the current register this argument or field
   starts in is kept in the pointer REGNO and updated accordingly, the bit this
   argument or field starts at is passed in STARTING_BIT and the last used bit
   is kept in LAST_USED_BIT which is also updated accordingly.  */

static unsigned HOST_WIDE_INT
comp_not_to_clear_mask_str_un (tree arg_type, int * regno,
			       uint32_t * padding_bits_to_clear,
			       unsigned starting_bit, int * last_used_bit)

{
  unsigned HOST_WIDE_INT not_to_clear_reg_mask = 0;

  if (TREE_CODE (arg_type) == RECORD_TYPE)
    {
      unsigned current_bit = starting_bit;
      tree field;
      long int offset, size;


      field = TYPE_FIELDS (arg_type);
      while (field)
	{
	  /* The offset within a structure is always an offset from
	     the start of that structure.  Make sure we take that into the
	     calculation of the register based offset that we use here.  */
	  offset = starting_bit;
	  offset += TREE_INT_CST_ELT (DECL_FIELD_BIT_OFFSET (field), 0);
	  offset %= 32;

	  /* This is the actual size of the field, for bitfields this is the
	     bitfield width and not the container size.  */
	  size = TREE_INT_CST_ELT (DECL_SIZE (field), 0);

	  if (*last_used_bit != offset)
	    {
	      if (offset < *last_used_bit)
		{
		  /* This field's offset is before the 'last_used_bit', that
		     means this field goes on the next register.  So we need to
		     pad the rest of the current register and increase the
		     register number.  */
		  uint32_t mask;
		  mask  = ((uint32_t)-1) - ((uint32_t) 1 << *last_used_bit);
		  mask++;

		  padding_bits_to_clear[*regno] |= mask;
		  not_to_clear_reg_mask |= HOST_WIDE_INT_1U << *regno;
		  (*regno)++;
		}
	      else
		{
		  /* Otherwise we pad the bits between the last field's end and
		     the start of the new field.  */
		  uint32_t mask;

		  mask = ((uint32_t)-1) >> (32 - offset);
		  mask -= ((uint32_t) 1 << *last_used_bit) - 1;
		  padding_bits_to_clear[*regno] |= mask;
		}
	      current_bit = offset;
	    }

	  /* Calculate further padding bits for inner structs/unions too.  */
	  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (field)))
	    {
	      *last_used_bit = current_bit;
	      not_to_clear_reg_mask
		|= comp_not_to_clear_mask_str_un (TREE_TYPE (field), regno,
						  padding_bits_to_clear, offset,
						  last_used_bit);
	    }
	  else
	    {
	      /* Update 'current_bit' with this field's size.  If the
		 'current_bit' lies in a subsequent register, update 'regno' and
		 reset 'current_bit' to point to the current bit in that new
		 register.  */
	      current_bit += size;
	      while (current_bit >= 32)
		{
		  current_bit-=32;
		  not_to_clear_reg_mask |= HOST_WIDE_INT_1U << *regno;
		  (*regno)++;
		}
	      *last_used_bit = current_bit;
	    }

	  field = TREE_CHAIN (field);
	}
      not_to_clear_reg_mask |= HOST_WIDE_INT_1U << *regno;
    }
  else if (TREE_CODE (arg_type) == UNION_TYPE)
    {
      tree field, field_t;
      int i, regno_t, field_size;
      int max_reg = -1;
      int max_bit = -1;
      uint32_t mask;
      uint32_t padding_bits_to_clear_res[NUM_ARG_REGS]
	= {-1, -1, -1, -1};

      /* To compute the padding bits in a union we only consider bits as
	 padding bits if they are always either a padding bit or fall outside a
	 fields size for all fields in the union.  */
      field = TYPE_FIELDS (arg_type);
      while (field)
	{
	  uint32_t padding_bits_to_clear_t[NUM_ARG_REGS]
	    = {0U, 0U, 0U, 0U};
	  int last_used_bit_t = *last_used_bit;
	  regno_t = *regno;
	  field_t = TREE_TYPE (field);

	  /* If the field's type is either a record or a union make sure to
	     compute their padding bits too.  */
	  if (RECORD_OR_UNION_TYPE_P (field_t))
	    not_to_clear_reg_mask
	      |= comp_not_to_clear_mask_str_un (field_t, &regno_t,
						&padding_bits_to_clear_t[0],
						starting_bit, &last_used_bit_t);
	  else
	    {
	      field_size = TREE_INT_CST_ELT (DECL_SIZE (field), 0);
	      regno_t = (field_size / 32) + *regno;
	      last_used_bit_t = (starting_bit + field_size) % 32;
	    }

	  for (i = *regno; i < regno_t; i++)
	    {
	      /* For all but the last register used by this field only keep the
		 padding bits that were padding bits in this field.  */
	      padding_bits_to_clear_res[i] &= padding_bits_to_clear_t[i];
	    }

	    /* For the last register, keep all padding bits that were padding
	       bits in this field and any padding bits that are still valid
	       as padding bits but fall outside of this field's size.  */
	    mask = (((uint32_t) -1) - ((uint32_t) 1 << last_used_bit_t)) + 1;
	    padding_bits_to_clear_res[regno_t]
	      &= padding_bits_to_clear_t[regno_t] | mask;

	  /* Update the maximum size of the fields in terms of registers used
	     ('max_reg') and the 'last_used_bit' in said register.  */
	  if (max_reg < regno_t)
	    {
	      max_reg = regno_t;
	      max_bit = last_used_bit_t;
	    }
	  else if (max_reg == regno_t && max_bit < last_used_bit_t)
	    max_bit = last_used_bit_t;

	  field = TREE_CHAIN (field);
	}

      /* Update the current padding_bits_to_clear using the intersection of the
	 padding bits of all the fields.  */
      for (i=*regno; i < max_reg; i++)
	padding_bits_to_clear[i] |= padding_bits_to_clear_res[i];

      /* Do not keep trailing padding bits, we do not know yet whether this
	 is the end of the argument.  */
      mask = ((uint32_t) 1 << max_bit) - 1;
      padding_bits_to_clear[max_reg]
	|= padding_bits_to_clear_res[max_reg] & mask;

      *regno = max_reg;
      *last_used_bit = max_bit;
    }
  else
    /* This function should only be used for structs and unions.  */
    gcc_unreachable ();

  return not_to_clear_reg_mask;
}

/* In the context of ARMv8-M Security Extensions, this function is used for both
   'cmse_nonsecure_call' and 'cmse_nonsecure_entry' functions to compute what
   registers are used when returning or passing arguments, which is then
   returned as a mask.  It will also compute a mask to indicate padding/unused
   bits for each of these registers, and passes this through the
   PADDING_BITS_TO_CLEAR pointer.  The tree of the argument type is passed in
   ARG_TYPE, the rtl representation of the argument is passed in ARG_RTX and
   the starting register used to pass this argument or return value is passed
   in REGNO.  It makes use of 'comp_not_to_clear_mask_str_un' to compute these
   for struct and union types.  */

static unsigned HOST_WIDE_INT
compute_not_to_clear_mask (tree arg_type, rtx arg_rtx, int regno,
			     uint32_t * padding_bits_to_clear)

{
  int last_used_bit = 0;
  unsigned HOST_WIDE_INT not_to_clear_mask;

  if (RECORD_OR_UNION_TYPE_P (arg_type))
    {
      not_to_clear_mask
	= comp_not_to_clear_mask_str_un (arg_type, &regno,
					 padding_bits_to_clear, 0,
					 &last_used_bit);


      /* If the 'last_used_bit' is not zero, that means we are still using a
	 part of the last 'regno'.  In such cases we must clear the trailing
	 bits.  Otherwise we are not using regno and we should mark it as to
	 clear.  */
      if (last_used_bit != 0)
	padding_bits_to_clear[regno]
	  |= ((uint32_t)-1) - ((uint32_t) 1 << last_used_bit) + 1;
      else
	not_to_clear_mask &= ~(HOST_WIDE_INT_1U << regno);
    }
  else
    {
      not_to_clear_mask = 0;
      /* We are not dealing with structs nor unions.  So these arguments may be
	 passed in floating point registers too.  In some cases a BLKmode is
	 used when returning or passing arguments in multiple VFP registers.  */
      if (GET_MODE (arg_rtx) == BLKmode)
	{
	  int i, arg_regs;
	  rtx reg;

	  /* This should really only occur when dealing with the hard-float
	     ABI.  */
	  gcc_assert (TARGET_HARD_FLOAT_ABI);

	  for (i = 0; i < XVECLEN (arg_rtx, 0); i++)
	    {
	      reg = XEXP (XVECEXP (arg_rtx, 0, i), 0);
	      gcc_assert (REG_P (reg));

	      not_to_clear_mask |= HOST_WIDE_INT_1U << REGNO (reg);

	      /* If we are dealing with DF mode, make sure we don't
		 clear either of the registers it addresses.  */
	      arg_regs = ARM_NUM_REGS (GET_MODE (reg));
	      if (arg_regs > 1)
		{
		  unsigned HOST_WIDE_INT mask;
		  mask = HOST_WIDE_INT_1U << (REGNO (reg) + arg_regs);
		  mask -= HOST_WIDE_INT_1U << REGNO (reg);
		  not_to_clear_mask |= mask;
		}
	    }
	}
      else
	{
	  /* Otherwise we can rely on the MODE to determine how many registers
	     are being used by this argument.  */
	  int arg_regs = ARM_NUM_REGS (GET_MODE (arg_rtx));
	  not_to_clear_mask |= HOST_WIDE_INT_1U << REGNO (arg_rtx);
	  if (arg_regs > 1)
	    {
	      unsigned HOST_WIDE_INT
	      mask = HOST_WIDE_INT_1U << (REGNO (arg_rtx) + arg_regs);
	      mask -= HOST_WIDE_INT_1U << REGNO (arg_rtx);
	      not_to_clear_mask |= mask;
	    }
	}
    }

  return not_to_clear_mask;
}

/* Clear registers secret before doing a cmse_nonsecure_call or returning from
   a cmse_nonsecure_entry function.  TO_CLEAR_BITMAP indicates which registers
   are to be fully cleared, using the value in register CLEARING_REG if more
   efficient.  The PADDING_BITS_LEN entries array PADDING_BITS_TO_CLEAR gives
   the bits that needs to be cleared in caller-saved core registers, with
   SCRATCH_REG used as a scratch register for that clearing.

   NOTE: one of three following assertions must hold:
   - SCRATCH_REG is a low register
   - CLEARING_REG is in the set of registers fully cleared (ie. its bit is set
     in TO_CLEAR_BITMAP)
   - CLEARING_REG is a low register.  */

static void
cmse_clear_registers (sbitmap to_clear_bitmap, uint32_t *padding_bits_to_clear,
		      int padding_bits_len, rtx scratch_reg, rtx clearing_reg)
{
  bool saved_clearing = false;
  rtx saved_clearing_reg = NULL_RTX;
  int i, regno, clearing_regno, minregno = R0_REGNUM, maxregno = minregno - 1;

  gcc_assert (arm_arch_cmse);

  if (!bitmap_empty_p (to_clear_bitmap))
    {
      minregno = bitmap_first_set_bit (to_clear_bitmap);
      maxregno = bitmap_last_set_bit (to_clear_bitmap);
    }
  clearing_regno = REGNO (clearing_reg);

  /* Clear padding bits.  */
  gcc_assert (padding_bits_len <= NUM_ARG_REGS);
  for (i = 0, regno = R0_REGNUM; i < padding_bits_len; i++, regno++)
    {
      uint64_t mask;
      rtx rtx16, dest, cleared_reg = gen_rtx_REG (SImode, regno);

      if (padding_bits_to_clear[i] == 0)
	continue;

      /* If this is a Thumb-1 target and SCRATCH_REG is not a low register, use
	 CLEARING_REG as scratch.  */
      if (TARGET_THUMB1
	  && REGNO (scratch_reg) > LAST_LO_REGNUM)
	{
	  /* clearing_reg is not to be cleared, copy its value into scratch_reg
	     such that we can use clearing_reg to clear the unused bits in the
	     arguments.  */
	  if ((clearing_regno > maxregno
	       || !bitmap_bit_p (to_clear_bitmap, clearing_regno))
	      && !saved_clearing)
	    {
	      gcc_assert (clearing_regno <= LAST_LO_REGNUM);
	      emit_move_insn (scratch_reg, clearing_reg);
	      saved_clearing = true;
	      saved_clearing_reg = scratch_reg;
	    }
	  scratch_reg = clearing_reg;
	}

      /* Fill the lower half of the negated padding_bits_to_clear[i].  */
      mask = (~padding_bits_to_clear[i]) & 0xFFFF;
      emit_move_insn (scratch_reg, gen_int_mode (mask, SImode));

      /* Fill the top half of the negated padding_bits_to_clear[i].  */
      mask = (~padding_bits_to_clear[i]) >> 16;
      rtx16 = gen_int_mode (16, SImode);
      dest = gen_rtx_ZERO_EXTRACT (SImode, scratch_reg, rtx16, rtx16);
      if (mask)
	emit_insn (gen_rtx_SET (dest, gen_int_mode (mask, SImode)));

      emit_insn (gen_andsi3 (cleared_reg, cleared_reg, scratch_reg));
    }
  if (saved_clearing)
    emit_move_insn (clearing_reg, saved_clearing_reg);


  /* Clear full registers.  */

  /* If not marked for clearing, clearing_reg already does not contain
     any secret.  */
  if (clearing_regno <= maxregno
      && bitmap_bit_p (to_clear_bitmap, clearing_regno))
    {
      emit_move_insn (clearing_reg, const0_rtx);
      emit_use (clearing_reg);
      bitmap_clear_bit (to_clear_bitmap, clearing_regno);
    }

  for (regno = minregno; regno <= maxregno; regno++)
    {
      if (!bitmap_bit_p (to_clear_bitmap, regno))
	continue;

      if (IS_VFP_REGNUM (regno))
	{
	  /* If regno is an even vfp register and its successor is also to
	     be cleared, use vmov.  */
	  if (TARGET_VFP_DOUBLE
	      && VFP_REGNO_OK_FOR_DOUBLE (regno)
	      && bitmap_bit_p (to_clear_bitmap, regno + 1))
	    {
	      emit_move_insn (gen_rtx_REG (DFmode, regno),
			      CONST1_RTX (DFmode));
	      emit_use (gen_rtx_REG (DFmode, regno));
	      regno++;
	    }
	  else
	    {
	      emit_move_insn (gen_rtx_REG (SFmode, regno),
			      CONST1_RTX (SFmode));
	      emit_use (gen_rtx_REG (SFmode, regno));
	    }
	}
      else
	{
	  emit_move_insn (gen_rtx_REG (SImode, regno), clearing_reg);
	  emit_use (gen_rtx_REG (SImode, regno));
	}
    }
}

/* Clears caller saved registers not used to pass arguments before a
   cmse_nonsecure_call.  Saving, clearing and restoring of callee saved
   registers is done in __gnu_cmse_nonsecure_call libcall.
   See libgcc/config/arm/cmse_nonsecure_call.S.  */

static void
cmse_nonsecure_call_clear_caller_saved (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;

      FOR_BB_INSNS (bb, insn)
	{
	  unsigned address_regnum, regno, maxregno =
	    TARGET_HARD_FLOAT_ABI ? D7_VFP_REGNUM : NUM_ARG_REGS - 1;
	  auto_sbitmap to_clear_bitmap (maxregno + 1);
	  rtx_insn *seq;
	  rtx pat, call, unspec, clearing_reg, ip_reg, shift;
	  rtx address;
	  CUMULATIVE_ARGS args_so_far_v;
	  cumulative_args_t args_so_far;
	  tree arg_type, fntype;
	  bool first_param = true;
	  function_args_iterator args_iter;
	  uint32_t padding_bits_to_clear[4] = {0U, 0U, 0U, 0U};

	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  if (!CALL_P (insn))
	    continue;

	  pat = PATTERN (insn);
	  gcc_assert (GET_CODE (pat) == PARALLEL && XVECLEN (pat, 0) > 0);
	  call = XVECEXP (pat, 0, 0);

	  /* Get the real call RTX if the insn sets a value, ie. returns.  */
	  if (GET_CODE (call) == SET)
	      call = SET_SRC (call);

	  /* Check if it is a cmse_nonsecure_call.  */
	  unspec = XEXP (call, 0);
	  if (GET_CODE (unspec) != UNSPEC
	      || XINT (unspec, 1) != UNSPEC_NONSECURE_MEM)
	    continue;

	  /* Determine the caller-saved registers we need to clear.  */
	  bitmap_clear (to_clear_bitmap);
	  bitmap_set_range (to_clear_bitmap, R0_REGNUM, NUM_ARG_REGS);

	  /* Only look at the caller-saved floating point registers in case of
	     -mfloat-abi=hard.  For -mfloat-abi=softfp we will be using the
	     lazy store and loads which clear both caller- and callee-saved
	     registers.  */
	  if (TARGET_HARD_FLOAT_ABI)
	    {
	      auto_sbitmap float_bitmap (maxregno + 1);

	      bitmap_clear (float_bitmap);
	      bitmap_set_range (float_bitmap, FIRST_VFP_REGNUM,
				D7_VFP_REGNUM - FIRST_VFP_REGNUM + 1);
	      bitmap_ior (to_clear_bitmap, to_clear_bitmap, float_bitmap);
	    }

	  /* Make sure the register used to hold the function address is not
	     cleared.  */
	  address = RTVEC_ELT (XVEC (unspec, 0), 0);
	  gcc_assert (MEM_P (address));
	  gcc_assert (REG_P (XEXP (address, 0)));
	  address_regnum = REGNO (XEXP (address, 0));
	  if (address_regnum < R0_REGNUM + NUM_ARG_REGS)
	    bitmap_clear_bit (to_clear_bitmap, address_regnum);

	  /* Set basic block of call insn so that df rescan is performed on
	     insns inserted here.  */
	  set_block_for_insn (insn, bb);
	  df_set_flags (DF_DEFER_INSN_RESCAN);
	  start_sequence ();

	  /* Make sure the scheduler doesn't schedule other insns beyond
	     here.  */
	  emit_insn (gen_blockage ());

	  /* Walk through all arguments and clear registers appropriately.
	  */
	  fntype = TREE_TYPE (MEM_EXPR (address));
	  arm_init_cumulative_args (&args_so_far_v, fntype, NULL_RTX,
				    NULL_TREE);
	  args_so_far = pack_cumulative_args (&args_so_far_v);
	  FOREACH_FUNCTION_ARGS (fntype, arg_type, args_iter)
	    {
	      rtx arg_rtx;
	      uint64_t to_clear_args_mask;
	      machine_mode arg_mode = TYPE_MODE (arg_type);

	      if (VOID_TYPE_P (arg_type))
		continue;

	      if (!first_param)
		arm_function_arg_advance (args_so_far, arg_mode, arg_type,
					  true);

	      arg_rtx = arm_function_arg (args_so_far, arg_mode, arg_type,
					  true);
	      gcc_assert (REG_P (arg_rtx));
	      to_clear_args_mask
		= compute_not_to_clear_mask (arg_type, arg_rtx,
					     REGNO (arg_rtx),
					     &padding_bits_to_clear[0]);
	      if (to_clear_args_mask)
		{
		  for (regno = R0_REGNUM; regno <= maxregno; regno++)
		    {
		      if (to_clear_args_mask & (1ULL << regno))
			bitmap_clear_bit (to_clear_bitmap, regno);
		    }
		}

	      first_param = false;
	    }

	  /* We use right shift and left shift to clear the LSB of the address
	     we jump to instead of using bic, to avoid having to use an extra
	     register on Thumb-1.  */
	  clearing_reg = XEXP (address, 0);
	  shift = gen_rtx_LSHIFTRT (SImode, clearing_reg, const1_rtx);
	  emit_insn (gen_rtx_SET (clearing_reg, shift));
	  shift = gen_rtx_ASHIFT (SImode, clearing_reg, const1_rtx);
	  emit_insn (gen_rtx_SET (clearing_reg, shift));

	  /* Clear caller-saved registers that leak before doing a non-secure
	     call.  */
	  ip_reg = gen_rtx_REG (SImode, IP_REGNUM);
	  cmse_clear_registers (to_clear_bitmap, padding_bits_to_clear,
				NUM_ARG_REGS, ip_reg, clearing_reg);

	  seq = get_insns ();
	  end_sequence ();
	  emit_insn_before (seq, insn);
	}
    }
}

/* Rewrite move insn into subtract of 0 if the condition codes will
   be useful in next conditional jump insn.  */

static void
thumb1_reorg (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx dest, src;
      rtx cmp, op0, op1, set = NULL;
      rtx_insn *prev, *insn = BB_END (bb);
      bool insn_clobbered = false;

      while (insn != BB_HEAD (bb) && !NONDEBUG_INSN_P (insn))
	insn = PREV_INSN (insn);

      /* Find the last cbranchsi4_insn in basic block BB.  */
      if (insn == BB_HEAD (bb)
	  || INSN_CODE (insn) != CODE_FOR_cbranchsi4_insn)
	continue;

      /* Get the register with which we are comparing.  */
      cmp = XEXP (SET_SRC (PATTERN (insn)), 0);
      op0 = XEXP (cmp, 0);
      op1 = XEXP (cmp, 1);

      /* Check that comparison is against ZERO.  */
      if (!CONST_INT_P (op1) || INTVAL (op1) != 0)
	continue;

      /* Find the first flag setting insn before INSN in basic block BB.  */
      gcc_assert (insn != BB_HEAD (bb));
      for (prev = PREV_INSN (insn);
	   (!insn_clobbered
	    && prev != BB_HEAD (bb)
	    && (NOTE_P (prev)
		|| DEBUG_INSN_P (prev)
		|| ((set = single_set (prev)) != NULL
		    && get_attr_conds (prev) == CONDS_NOCOND)));
	   prev = PREV_INSN (prev))
	{
	  if (reg_set_p (op0, prev))
	    insn_clobbered = true;
	}

      /* Skip if op0 is clobbered by insn other than prev. */
      if (insn_clobbered)
	continue;

      if (!set)
	continue;

      dest = SET_DEST (set);
      src = SET_SRC (set);
      if (!low_register_operand (dest, SImode)
	  || !low_register_operand (src, SImode))
	continue;

      /* Rewrite move into subtract of 0 if its operand is compared with ZERO
	 in INSN.  Both src and dest of the move insn are checked.  */
      if (REGNO (op0) == REGNO (src) || REGNO (op0) == REGNO (dest))
	{
	  dest = copy_rtx (dest);
	  src = copy_rtx (src);
	  src = gen_rtx_MINUS (SImode, src, const0_rtx);
	  PATTERN (prev) = gen_rtx_SET (dest, src);
	  INSN_CODE (prev) = -1;
	  /* Set test register in INSN to dest.  */
	  XEXP (cmp, 0) = copy_rtx (dest);
	  INSN_CODE (insn) = -1;
	}
    }
}

/* Convert instructions to their cc-clobbering variant if possible, since
   that allows us to use smaller encodings.  */

static void
thumb2_reorg (void)
{
  basic_block bb;
  regset_head live;

  INIT_REG_SET (&live);

  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it now.  */
  compute_bb_for_insn ();
  df_analyze ();

  enum Convert_Action {SKIP, CONV, SWAP_CONV};

  FOR_EACH_BB_FN (bb, cfun)
    {
      if ((current_tune->disparage_flag_setting_t16_encodings
	   == tune_params::DISPARAGE_FLAGS_ALL)
	  && optimize_bb_for_speed_p (bb))
	continue;

      rtx_insn *insn;
      Convert_Action action = SKIP;
      Convert_Action action_for_partial_flag_setting
	= ((current_tune->disparage_flag_setting_t16_encodings
	    != tune_params::DISPARAGE_FLAGS_NEITHER)
	   && optimize_bb_for_speed_p (bb))
	  ? SKIP : CONV;

      COPY_REG_SET (&live, DF_LR_OUT (bb));
      df_simulate_initialize_backwards (bb, &live);
      FOR_BB_INSNS_REVERSE (bb, insn)
	{
	  if (NONJUMP_INSN_P (insn)
	      && !REGNO_REG_SET_P (&live, CC_REGNUM)
	      && GET_CODE (PATTERN (insn)) == SET)
	    {
	      action = SKIP;
	      rtx pat = PATTERN (insn);
	      rtx dst = XEXP (pat, 0);
	      rtx src = XEXP (pat, 1);
	      rtx op0 = NULL_RTX, op1 = NULL_RTX;

	      if (UNARY_P (src) || BINARY_P (src))
		  op0 = XEXP (src, 0);

	      if (BINARY_P (src))
		  op1 = XEXP (src, 1);

	      if (low_register_operand (dst, SImode))
		{
		  switch (GET_CODE (src))
		    {
		    case PLUS:
		      /* Adding two registers and storing the result
			 in the first source is already a 16-bit
			 operation.  */
		      if (rtx_equal_p (dst, op0)
			  && register_operand (op1, SImode))
			break;

		      if (low_register_operand (op0, SImode))
			{
			  /* ADDS <Rd>,<Rn>,<Rm>  */
			  if (low_register_operand (op1, SImode))
			    action = CONV;
			  /* ADDS <Rdn>,#<imm8>  */
			  /* SUBS <Rdn>,#<imm8>  */
			  else if (rtx_equal_p (dst, op0)
				   && CONST_INT_P (op1)
				   && IN_RANGE (INTVAL (op1), -255, 255))
			    action = CONV;
			  /* ADDS <Rd>,<Rn>,#<imm3>  */
			  /* SUBS <Rd>,<Rn>,#<imm3>  */
			  else if (CONST_INT_P (op1)
				   && IN_RANGE (INTVAL (op1), -7, 7))
			    action = CONV;
			}
		      /* ADCS <Rd>, <Rn>  */
		      else if (GET_CODE (XEXP (src, 0)) == PLUS
			      && rtx_equal_p (XEXP (XEXP (src, 0), 0), dst)
			      && low_register_operand (XEXP (XEXP (src, 0), 1),
						       SImode)
			      && COMPARISON_P (op1)
			      && cc_register (XEXP (op1, 0), VOIDmode)
			      && maybe_get_arm_condition_code (op1) == ARM_CS
			      && XEXP (op1, 1) == const0_rtx)
		        action = CONV;
		      break;

		    case MINUS:
		      /* RSBS <Rd>,<Rn>,#0
			 Not handled here: see NEG below.  */
		      /* SUBS <Rd>,<Rn>,#<imm3>
			 SUBS <Rdn>,#<imm8>
			 Not handled here: see PLUS above.  */
		      /* SUBS <Rd>,<Rn>,<Rm>  */
		      if (low_register_operand (op0, SImode)
			  && low_register_operand (op1, SImode))
			    action = CONV;
		      break;

		    case MULT:
		      /* MULS <Rdm>,<Rn>,<Rdm>
			 As an exception to the rule, this is only used
			 when optimizing for size since MULS is slow on all
			 known implementations.  We do not even want to use
			 MULS in cold code, if optimizing for speed, so we
			 test the global flag here.  */
		      if (!optimize_size)
			break;
		      /* Fall through.  */
		    case AND:
		    case IOR:
		    case XOR:
		      /* ANDS <Rdn>,<Rm>  */
		      if (rtx_equal_p (dst, op0)
			  && low_register_operand (op1, SImode))
			action = action_for_partial_flag_setting;
		      else if (rtx_equal_p (dst, op1)
			       && low_register_operand (op0, SImode))
			action = action_for_partial_flag_setting == SKIP
				 ? SKIP : SWAP_CONV;
		      break;

		    case ASHIFTRT:
		    case ASHIFT:
		    case LSHIFTRT:
		      /* ASRS <Rdn>,<Rm> */
		      /* LSRS <Rdn>,<Rm> */
		      /* LSLS <Rdn>,<Rm> */
		      if (rtx_equal_p (dst, op0)
			  && low_register_operand (op1, SImode))
			action = action_for_partial_flag_setting;
		      /* ASRS <Rd>,<Rm>,#<imm5> */
		      /* LSRS <Rd>,<Rm>,#<imm5> */
		      /* LSLS <Rd>,<Rm>,#<imm5> */
		      else if (low_register_operand (op0, SImode)
			       && CONST_INT_P (op1)
			       && IN_RANGE (INTVAL (op1), 0, 31))
			action = action_for_partial_flag_setting;
		      break;

		    case ROTATERT:
		      /* RORS <Rdn>,<Rm>  */
		      if (rtx_equal_p (dst, op0)
			  && low_register_operand (op1, SImode))
			action = action_for_partial_flag_setting;
		      break;

		    case NOT:
		      /* MVNS <Rd>,<Rm>  */
		      if (low_register_operand (op0, SImode))
			action = action_for_partial_flag_setting;
		      break;

		    case NEG:
		      /* NEGS <Rd>,<Rm>  (a.k.a RSBS)  */
		      if (low_register_operand (op0, SImode))
			action = CONV;
		      break;

		    case CONST_INT:
		      /* MOVS <Rd>,#<imm8>  */
		      if (CONST_INT_P (src)
			  && IN_RANGE (INTVAL (src), 0, 255))
			action = action_for_partial_flag_setting;
		      break;

		    case REG:
		      /* MOVS and MOV<c> with registers have different
			 encodings, so are not relevant here.  */
		      break;

		    default:
		      break;
		    }
		}

	      if (action != SKIP)
		{
		  rtx ccreg = gen_rtx_REG (CCmode, CC_REGNUM);
		  rtx clobber = gen_rtx_CLOBBER (VOIDmode, ccreg);
		  rtvec vec;

		  if (action == SWAP_CONV)
		    {
		      src = copy_rtx (src);
		      XEXP (src, 0) = op1;
		      XEXP (src, 1) = op0;
		      pat = gen_rtx_SET (dst, src);
		      vec = gen_rtvec (2, pat, clobber);
		    }
		  else /* action == CONV */
		    vec = gen_rtvec (2, pat, clobber);

		  PATTERN (insn) = gen_rtx_PARALLEL (VOIDmode, vec);
		  INSN_CODE (insn) = -1;
		}
	    }

	  if (NONDEBUG_INSN_P (insn))
	    df_simulate_one_insn_backwards (bb, insn, &live);
	}
    }

  CLEAR_REG_SET (&live);
}

/* Gcc puts the pool in the wrong place for ARM, since we can only
   load addresses a limited distance around the pc.  We do some
   special munging to move the constant pool values to the correct
   point in the code.  */
static void
arm_reorg (void)
{
  rtx_insn *insn;
  HOST_WIDE_INT address = 0;
  Mfix * fix;

  if (use_cmse)
    cmse_nonsecure_call_clear_caller_saved ();
  if (TARGET_THUMB1)
    thumb1_reorg ();
  else if (TARGET_THUMB2)
    thumb2_reorg ();

  /* Ensure all insns that must be split have been split at this point.
     Otherwise, the pool placement code below may compute incorrect
     insn lengths.  Note that when optimizing, all insns have already
     been split at this point.  */
  if (!optimize)
    split_all_insns_noflow ();

  /* Make sure we do not attempt to create a literal pool even though it should
     no longer be necessary to create any.  */
  if (arm_disable_literal_pool)
    return ;

  minipool_fix_head = minipool_fix_tail = NULL;

  /* The first insn must always be a note, or the code below won't
     scan it properly.  */
  insn = get_insns ();
  gcc_assert (NOTE_P (insn));
  minipool_pad = 0;

  /* Scan all the insns and record the operands that will need fixing.  */
  for (insn = next_nonnote_insn (insn); insn; insn = next_nonnote_insn (insn))
    {
      if (BARRIER_P (insn))
	push_minipool_barrier (insn, address);
      else if (INSN_P (insn))
	{
	  rtx_jump_table_data *table;

	  note_invalid_constants (insn, address, true);
	  address += get_attr_length (insn);

	  /* If the insn is a vector jump, add the size of the table
	     and skip the table.  */
	  if (tablejump_p (insn, NULL, &table))
	    {
	      address += get_jump_table_size (table);
	      insn = table;
	    }
	}
      else if (LABEL_P (insn))
	/* Add the worst-case padding due to alignment.  We don't add
	   the _current_ padding because the minipool insertions
	   themselves might change it.  */
	address += get_label_padding (insn);
    }

  fix = minipool_fix_head;

  /* Now scan the fixups and perform the required changes.  */
  while (fix)
    {
      Mfix * ftmp;
      Mfix * fdel;
      Mfix *  last_added_fix;
      Mfix * last_barrier = NULL;
      Mfix * this_fix;

      /* Skip any further barriers before the next fix.  */
      while (fix && BARRIER_P (fix->insn))
	fix = fix->next;

      /* No more fixes.  */
      if (fix == NULL)
	break;

      last_added_fix = NULL;

      for (ftmp = fix; ftmp; ftmp = ftmp->next)
	{
	  if (BARRIER_P (ftmp->insn))
	    {
	      if (ftmp->address >= minipool_vector_head->max_address)
		break;

	      last_barrier = ftmp;
	    }
	  else if ((ftmp->minipool = add_minipool_forward_ref (ftmp)) == NULL)
	    break;

	  last_added_fix = ftmp;  /* Keep track of the last fix added.  */
	}

      /* If we found a barrier, drop back to that; any fixes that we
	 could have reached but come after the barrier will now go in
	 the next mini-pool.  */
      if (last_barrier != NULL)
	{
	  /* Reduce the refcount for those fixes that won't go into this
	     pool after all.  */
	  for (fdel = last_barrier->next;
	       fdel && fdel != ftmp;
	       fdel = fdel->next)
	    {
	      fdel->minipool->refcount--;
	      fdel->minipool = NULL;
	    }

	  ftmp = last_barrier;
	}
      else
        {
	  /* ftmp is first fix that we can't fit into this pool and
	     there no natural barriers that we could use.  Insert a
	     new barrier in the code somewhere between the previous
	     fix and this one, and arrange to jump around it.  */
	  HOST_WIDE_INT max_address;

	  /* The last item on the list of fixes must be a barrier, so
	     we can never run off the end of the list of fixes without
	     last_barrier being set.  */
	  gcc_assert (ftmp);

	  max_address = minipool_vector_head->max_address;
	  /* Check that there isn't another fix that is in range that
	     we couldn't fit into this pool because the pool was
	     already too large: we need to put the pool before such an
	     instruction.  The pool itself may come just after the
	     fix because create_fix_barrier also allows space for a
	     jump instruction.  */
	  if (ftmp->address < max_address)
	    max_address = ftmp->address + 1;

	  last_barrier = create_fix_barrier (last_added_fix, max_address);
	}

      assign_minipool_offsets (last_barrier);

      while (ftmp)
	{
	  if (!BARRIER_P (ftmp->insn)
	      && ((ftmp->minipool = add_minipool_backward_ref (ftmp))
		  == NULL))
	    break;

	  ftmp = ftmp->next;
	}

      /* Scan over the fixes we have identified for this pool, fixing them
	 up and adding the constants to the pool itself.  */
      for (this_fix = fix; this_fix && ftmp != this_fix;
	   this_fix = this_fix->next)
	if (!BARRIER_P (this_fix->insn))
	  {
	    rtx addr
	      = plus_constant (Pmode,
			       gen_rtx_LABEL_REF (VOIDmode,
						  minipool_vector_label),
			       this_fix->minipool->offset);
	    *this_fix->loc = gen_rtx_MEM (this_fix->mode, addr);
	  }

      dump_minipool (last_barrier->insn);
      fix = ftmp;
    }

  /* From now on we must synthesize any constants that we can't handle
     directly.  This can happen if the RTL gets split during final
     instruction generation.  */
  cfun->machine->after_arm_reorg = 1;

  /* Free the minipool memory.  */
  obstack_free (&minipool_obstack, minipool_startobj);
}

/* Routines to output assembly language.  */

/* Return string representation of passed in real value.  */
static const char *
fp_const_from_val (REAL_VALUE_TYPE *r)
{
  if (!fp_consts_inited)
    init_fp_table ();

  gcc_assert (real_equal (r, &value_fp0));
  return "0";
}

/* OPERANDS[0] is the entire list of insns that constitute pop,
   OPERANDS[1] is the base register, RETURN_PC is true iff return insn
   is in the list, UPDATE is true iff the list contains explicit
   update of base register.  */
void
arm_output_multireg_pop (rtx *operands, bool return_pc, rtx cond, bool reverse,
                         bool update)
{
  int i;
  char pattern[100];
  int offset;
  const char *conditional;
  int num_saves = XVECLEN (operands[0], 0);
  unsigned int regno;
  unsigned int regno_base = REGNO (operands[1]);
  bool interrupt_p = IS_INTERRUPT (arm_current_func_type ());

  offset = 0;
  offset += update ? 1 : 0;
  offset += return_pc ? 1 : 0;

  /* Is the base register in the list?  */
  for (i = offset; i < num_saves; i++)
    {
      regno = REGNO (XEXP (XVECEXP (operands[0], 0, i), 0));
      /* If SP is in the list, then the base register must be SP.  */
      gcc_assert ((regno != SP_REGNUM) || (regno_base == SP_REGNUM));
      /* If base register is in the list, there must be no explicit update.  */
      if (regno == regno_base)
        gcc_assert (!update);
    }

  conditional = reverse ? "%?%D0" : "%?%d0";
  /* Can't use POP if returning from an interrupt.  */
  if ((regno_base == SP_REGNUM) && update && !(interrupt_p && return_pc))
    sprintf (pattern, "pop%s\t{", conditional);
  else
    {
      /* Output ldmfd when the base register is SP, otherwise output ldmia.
         It's just a convention, their semantics are identical.  */
      if (regno_base == SP_REGNUM)
	sprintf (pattern, "ldmfd%s\t", conditional);
      else if (update)
	sprintf (pattern, "ldmia%s\t", conditional);
      else
	sprintf (pattern, "ldm%s\t", conditional);

      strcat (pattern, reg_names[regno_base]);
      if (update)
        strcat (pattern, "!, {");
      else
        strcat (pattern, ", {");
    }

  /* Output the first destination register.  */
  strcat (pattern,
          reg_names[REGNO (XEXP (XVECEXP (operands[0], 0, offset), 0))]);

  /* Output the rest of the destination registers.  */
  for (i = offset + 1; i < num_saves; i++)
    {
      strcat (pattern, ", ");
      strcat (pattern,
              reg_names[REGNO (XEXP (XVECEXP (operands[0], 0, i), 0))]);
    }

  strcat (pattern, "}");

  if (interrupt_p && return_pc)
    strcat (pattern, "^");

  output_asm_insn (pattern, &cond);
}


/* Output the assembly for a store multiple.  */

const char *
vfp_output_vstmd (rtx * operands)
{
  char pattern[100];
  int p;
  int base;
  int i;
  rtx addr_reg = REG_P (XEXP (operands[0], 0))
		   ? XEXP (operands[0], 0)
		   : XEXP (XEXP (operands[0], 0), 0);
  bool push_p =  REGNO (addr_reg) == SP_REGNUM;

  if (push_p)
    strcpy (pattern, "vpush%?.64\t{%P1");
  else
    strcpy (pattern, "vstmdb%?.64\t%m0!, {%P1");

  p = strlen (pattern);

  gcc_assert (REG_P (operands[1]));

  base = (REGNO (operands[1]) - FIRST_VFP_REGNUM) / 2;
  for (i = 1; i < XVECLEN (operands[2], 0); i++)
    {
      p += sprintf (&pattern[p], ", d%d", base + i);
    }
  strcpy (&pattern[p], "}");

  output_asm_insn (pattern, operands);
  return "";
}


/* Emit RTL to save block of VFP register pairs to the stack.  Returns the
   number of bytes pushed.  */

static int
vfp_emit_fstmd (int base_reg, int count)
{
  rtx par;
  rtx dwarf;
  rtx tmp, reg;
  int i;

  /* Workaround ARM10 VFPr1 bug.  Data corruption can occur when exactly two
     register pairs are stored by a store multiple insn.  We avoid this
     by pushing an extra pair.  */
  if (count == 2 && !arm_arch6)
    {
      if (base_reg == LAST_VFP_REGNUM - 3)
	base_reg -= 2;
      count++;
    }

  /* FSTMD may not store more than 16 doubleword registers at once.  Split
     larger stores into multiple parts (up to a maximum of two, in
     practice).  */
  if (count > 16)
    {
      int saved;
      /* NOTE: base_reg is an internal register number, so each D register
         counts as 2.  */
      saved = vfp_emit_fstmd (base_reg + 32, count - 16);
      saved += vfp_emit_fstmd (base_reg, 16);
      return saved;
    }

  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));
  dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (count + 1));

  reg = gen_rtx_REG (DFmode, base_reg);
  base_reg += 2;

  XVECEXP (par, 0, 0)
    = gen_rtx_SET (gen_frame_mem
		   (BLKmode,
		    gen_rtx_PRE_MODIFY (Pmode,
					stack_pointer_rtx,
					plus_constant
					(Pmode, stack_pointer_rtx,
					 - (count * 8)))
		    ),
		   gen_rtx_UNSPEC (BLKmode,
				   gen_rtvec (1, reg),
				   UNSPEC_PUSH_MULT));

  tmp = gen_rtx_SET (stack_pointer_rtx,
		     plus_constant (Pmode, stack_pointer_rtx, -(count * 8)));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, 0) = tmp;

  tmp = gen_rtx_SET (gen_frame_mem (DFmode, stack_pointer_rtx), reg);
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, 1) = tmp;

  for (i = 1; i < count; i++)
    {
      reg = gen_rtx_REG (DFmode, base_reg);
      base_reg += 2;
      XVECEXP (par, 0, i) = gen_rtx_USE (VOIDmode, reg);

      tmp = gen_rtx_SET (gen_frame_mem (DFmode,
					plus_constant (Pmode,
						       stack_pointer_rtx,
						       i * 8)),
			 reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (dwarf, 0, i + 1) = tmp;
    }

  par = emit_insn (par);
  add_reg_note (par, REG_FRAME_RELATED_EXPR, dwarf);
  RTX_FRAME_RELATED_P (par) = 1;

  return count * 8;
}

/* Returns true if -mcmse has been passed and the function pointed to by 'addr'
   has the cmse_nonsecure_call attribute and returns false otherwise.  */

bool
detect_cmse_nonsecure_call (tree addr)
{
  if (!addr)
    return FALSE;

  tree fntype = TREE_TYPE (addr);
  if (use_cmse && lookup_attribute ("cmse_nonsecure_call",
				    TYPE_ATTRIBUTES (fntype)))
    return TRUE;
  return FALSE;
}


/* Emit a call instruction with pattern PAT.  ADDR is the address of
   the call target.  */

void
arm_emit_call_insn (rtx pat, rtx addr, bool sibcall)
{
  rtx insn;

  insn = emit_call_insn (pat);

  /* The PIC register is live on entry to VxWorks PIC PLT entries.
     If the call might use such an entry, add a use of the PIC register
     to the instruction's CALL_INSN_FUNCTION_USAGE.  */
  if (TARGET_VXWORKS_RTP
      && flag_pic
      && !sibcall
      && GET_CODE (addr) == SYMBOL_REF
      && (SYMBOL_REF_DECL (addr)
	  ? !targetm.binds_local_p (SYMBOL_REF_DECL (addr))
	  : !SYMBOL_REF_LOCAL_P (addr)))
    {
      require_pic_register ();
      use_reg (&CALL_INSN_FUNCTION_USAGE (insn), cfun->machine->pic_reg);
    }

  if (TARGET_AAPCS_BASED)
    {
      /* For AAPCS, IP and CC can be clobbered by veneers inserted by the
	 linker.  We need to add an IP clobber to allow setting
	 TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS to true.  A CC clobber
	 is not needed since it's a fixed register.  */
      rtx *fusage = &CALL_INSN_FUNCTION_USAGE (insn);
      clobber_reg (fusage, gen_rtx_REG (word_mode, IP_REGNUM));
    }
}

/* Output a 'call' insn.  */
const char *
output_call (rtx *operands)
{
  gcc_assert (!arm_arch5); /* Patterns should call blx <reg> directly.  */

  /* Handle calls to lr using ip (which may be clobbered in subr anyway).  */
  if (REGNO (operands[0]) == LR_REGNUM)
    {
      operands[0] = gen_rtx_REG (SImode, IP_REGNUM);
      output_asm_insn ("mov%?\t%0, %|lr", operands);
    }

  output_asm_insn ("mov%?\t%|lr, %|pc", operands);

  if (TARGET_INTERWORK || arm_arch4t)
    output_asm_insn ("bx%?\t%0", operands);
  else
    output_asm_insn ("mov%?\t%|pc, %0", operands);

  return "";
}

/* Output a move from arm registers to arm registers of a long double
   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.  */
const char *
output_mov_long_double_arm_from_arm (rtx *operands)
{
  /* We have to be careful here because the two might overlap.  */
  int dest_start = REGNO (operands[0]);
  int src_start = REGNO (operands[1]);
  rtx ops[2];
  int i;

  if (dest_start < src_start)
    {
      for (i = 0; i < 3; i++)
	{
	  ops[0] = gen_rtx_REG (SImode, dest_start + i);
	  ops[1] = gen_rtx_REG (SImode, src_start + i);
	  output_asm_insn ("mov%?\t%0, %1", ops);
	}
    }
  else
    {
      for (i = 2; i >= 0; i--)
	{
	  ops[0] = gen_rtx_REG (SImode, dest_start + i);
	  ops[1] = gen_rtx_REG (SImode, src_start + i);
	  output_asm_insn ("mov%?\t%0, %1", ops);
	}
    }

  return "";
}

void
arm_emit_movpair (rtx dest, rtx src)
 {
  /* If the src is an immediate, simplify it.  */
  if (CONST_INT_P (src))
    {
      HOST_WIDE_INT val = INTVAL (src);
      emit_set_insn (dest, GEN_INT (val & 0x0000ffff));
      if ((val >> 16) & 0x0000ffff)
	{
	  emit_set_insn (gen_rtx_ZERO_EXTRACT (SImode, dest, GEN_INT (16),
					       GEN_INT (16)),
			 GEN_INT ((val >> 16) & 0x0000ffff));
	  rtx_insn *insn = get_last_insn ();
	  set_unique_reg_note (insn, REG_EQUAL, copy_rtx (src));
	}
      return;
    }
   emit_set_insn (dest, gen_rtx_HIGH (SImode, src));
   emit_set_insn (dest, gen_rtx_LO_SUM (SImode, dest, src));
   rtx_insn *insn = get_last_insn ();
   set_unique_reg_note (insn, REG_EQUAL, copy_rtx (src));
 }

/* Output a move between double words.  It must be REG<-MEM
   or MEM<-REG.  */
const char *
output_move_double (rtx *operands, bool emit, int *count)
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  rtx otherops[3];
  if (count)
    *count = 1;

  /* The only case when this might happen is when
     you are looking at the length of a DImode instruction
     that has an invalid constant in it.  */
  if (code0 == REG && code1 != MEM)
    {
      gcc_assert (!emit);
      *count = 2;
      return "";
    }

  if (code0 == REG)
    {
      unsigned int reg0 = REGNO (operands[0]);

      otherops[0] = gen_rtx_REG (SImode, 1 + reg0);

      gcc_assert (code1 == MEM);  /* Constraints should ensure this.  */

      switch (GET_CODE (XEXP (operands[1], 0)))
	{
	case REG:

	  if (emit)
	    {
	      if (TARGET_LDRD
		  && !(fix_cm3_ldrd && reg0 == REGNO(XEXP (operands[1], 0))))
		output_asm_insn ("ldrd%?\t%0, [%m1]", operands);
	      else
		output_asm_insn ("ldmia%?\t%m1, %M0", operands);
	    }
	  break;

	case PRE_INC:
	  gcc_assert (TARGET_LDRD);
	  if (emit)
	    output_asm_insn ("ldrd%?\t%0, [%m1, #8]!", operands);
	  break;

	case PRE_DEC:
	  if (emit)
	    {
	      if (TARGET_LDRD)
		output_asm_insn ("ldrd%?\t%0, [%m1, #-8]!", operands);
	      else
		output_asm_insn ("ldmdb%?\t%m1!, %M0", operands);
	    }
	  break;

	case POST_INC:
	  if (emit)
	    {
	      if (TARGET_LDRD)
		output_asm_insn ("ldrd%?\t%0, [%m1], #8", operands);
	      else
		output_asm_insn ("ldmia%?\t%m1!, %M0", operands);
	    }
	  break;

	case POST_DEC:
	  gcc_assert (TARGET_LDRD);
	  if (emit)
	    output_asm_insn ("ldrd%?\t%0, [%m1], #-8", operands);
	  break;

	case PRE_MODIFY:
	case POST_MODIFY:
	  /* Autoicrement addressing modes should never have overlapping
	     base and destination registers, and overlapping index registers
	     are already prohibited, so this doesn't need to worry about
	     fix_cm3_ldrd.  */
	  otherops[0] = operands[0];
	  otherops[1] = XEXP (XEXP (XEXP (operands[1], 0), 1), 0);
	  otherops[2] = XEXP (XEXP (XEXP (operands[1], 0), 1), 1);

	  if (GET_CODE (XEXP (operands[1], 0)) == PRE_MODIFY)
	    {
	      if (reg_overlap_mentioned_p (otherops[0], otherops[2]))
		{
		  /* Registers overlap so split out the increment.  */
		  if (emit)
		    {
		      output_asm_insn ("add%?\t%1, %1, %2", otherops);
		      output_asm_insn ("ldrd%?\t%0, [%1] @split", otherops);
		    }
		  if (count)
		    *count = 2;
		}
	      else
		{
		  /* Use a single insn if we can.
		     FIXME: IWMMXT allows offsets larger than ldrd can
		     handle, fix these up with a pair of ldr.  */
		  if (TARGET_THUMB2
		      || !CONST_INT_P (otherops[2])
		      || (INTVAL (otherops[2]) > -256
			  && INTVAL (otherops[2]) < 256))
		    {
		      if (emit)
			output_asm_insn ("ldrd%?\t%0, [%1, %2]!", otherops);
		    }
		  else
		    {
		      if (emit)
			{
			  output_asm_insn ("ldr%?\t%0, [%1, %2]!", otherops);
			  output_asm_insn ("ldr%?\t%H0, [%1, #4]", otherops);
			}
		      if (count)
			*count = 2;

		    }
		}
	    }
	  else
	    {
	      /* Use a single insn if we can.
		 FIXME: IWMMXT allows offsets larger than ldrd can handle,
		 fix these up with a pair of ldr.  */
	      if (TARGET_THUMB2
		  || !CONST_INT_P (otherops[2])
		  || (INTVAL (otherops[2]) > -256
		      && INTVAL (otherops[2]) < 256))
		{
		  if (emit)
		    output_asm_insn ("ldrd%?\t%0, [%1], %2", otherops);
		}
	      else
		{
		  if (emit)
		    {
		      output_asm_insn ("ldr%?\t%H0, [%1, #4]", otherops);
		      output_asm_insn ("ldr%?\t%0, [%1], %2", otherops);
		    }
		  if (count)
		    *count = 2;
		}
	    }
	  break;

	case LABEL_REF:
	case CONST:
	  /* We might be able to use ldrd %0, %1 here.  However the range is
	     different to ldr/adr, and it is broken on some ARMv7-M
	     implementations.  */
	  /* Use the second register of the pair to avoid problematic
	     overlap.  */
	  otherops[1] = operands[1];
	  if (emit)
	    output_asm_insn ("adr%?\t%0, %1", otherops);
	  operands[1] = otherops[0];
	  if (emit)
	    {
	      if (TARGET_LDRD)
		output_asm_insn ("ldrd%?\t%0, [%1]", operands);
	      else
		output_asm_insn ("ldmia%?\t%1, %M0", operands);
	    }

	  if (count)
	    *count = 2;
	  break;

	  /* ??? This needs checking for thumb2.  */
	default:
	  if (arm_add_operand (XEXP (XEXP (operands[1], 0), 1),
			       GET_MODE (XEXP (XEXP (operands[1], 0), 1))))
	    {
	      otherops[0] = operands[0];
	      otherops[1] = XEXP (XEXP (operands[1], 0), 0);
	      otherops[2] = XEXP (XEXP (operands[1], 0), 1);

	      if (GET_CODE (XEXP (operands[1], 0)) == PLUS)
		{
		  if (CONST_INT_P (otherops[2]) && !TARGET_LDRD)
		    {
		      switch ((int) INTVAL (otherops[2]))
			{
			case -8:
			  if (emit)
			    output_asm_insn ("ldmdb%?\t%1, %M0", otherops);
			  return "";
			case -4:
			  if (TARGET_THUMB2)
			    break;
			  if (emit)
			    output_asm_insn ("ldmda%?\t%1, %M0", otherops);
			  return "";
			case 4:
			  if (TARGET_THUMB2)
			    break;
			  if (emit)
			    output_asm_insn ("ldmib%?\t%1, %M0", otherops);
			  return "";
			}
		    }
		  otherops[0] = gen_rtx_REG(SImode, REGNO(operands[0]) + 1);
		  operands[1] = otherops[0];
		  if (TARGET_LDRD
		      && (REG_P (otherops[2])
			  || TARGET_THUMB2
			  || (CONST_INT_P (otherops[2])
			      && INTVAL (otherops[2]) > -256
			      && INTVAL (otherops[2]) < 256)))
		    {
		      if (reg_overlap_mentioned_p (operands[0],
						   otherops[2]))
			{
			  /* Swap base and index registers over to
			     avoid a conflict.  */
			  std::swap (otherops[1], otherops[2]);
			}
		      /* If both registers conflict, it will usually
			 have been fixed by a splitter.  */
		      if (reg_overlap_mentioned_p (operands[0], otherops[2])
			  || (fix_cm3_ldrd && reg0 == REGNO (otherops[1])))
			{
			  if (emit)
			    {
			      output_asm_insn ("add%?\t%0, %1, %2", otherops);
			      output_asm_insn ("ldrd%?\t%0, [%1]", operands);
			    }
			  if (count)
			    *count = 2;
			}
		      else
			{
			  otherops[0] = operands[0];
			  if (emit)
			    output_asm_insn ("ldrd%?\t%0, [%1, %2]", otherops);
			}
		      return "";
		    }

		  if (CONST_INT_P (otherops[2]))
		    {
		      if (emit)
			{
			  if (!(const_ok_for_arm (INTVAL (otherops[2]))))
			    output_asm_insn ("sub%?\t%0, %1, #%n2", otherops);
			  else
			    output_asm_insn ("add%?\t%0, %1, %2", otherops);
			}
		    }
		  else
		    {
		      if (emit)
			output_asm_insn ("add%?\t%0, %1, %2", otherops);
		    }
		}
	      else
		{
		  if (emit)
		    output_asm_insn ("sub%?\t%0, %1, %2", otherops);
		}

	      if (count)
		*count = 2;

	      if (TARGET_LDRD)
		return "ldrd%?\t%0, [%1]";

	      return "ldmia%?\t%1, %M0";
	    }
	  else
	    {
	      otherops[1] = adjust_address (operands[1], SImode, 4);
	      /* Take care of overlapping base/data reg.  */
	      if (reg_mentioned_p (operands[0], operands[1]))
		{
		  if (emit)
		    {
		      output_asm_insn ("ldr%?\t%0, %1", otherops);
		      output_asm_insn ("ldr%?\t%0, %1", operands);
		    }
		  if (count)
		    *count = 2;

		}
	      else
		{
		  if (emit)
		    {
		      output_asm_insn ("ldr%?\t%0, %1", operands);
		      output_asm_insn ("ldr%?\t%0, %1", otherops);
		    }
		  if (count)
		    *count = 2;
		}
	    }
	}
    }
  else
    {
      /* Constraints should ensure this.  */
      gcc_assert (code0 == MEM && code1 == REG);
      gcc_assert ((REGNO (operands[1]) != IP_REGNUM)
                  || (TARGET_ARM && TARGET_LDRD));

      switch (GET_CODE (XEXP (operands[0], 0)))
        {
	case REG:
	  if (emit)
	    {
	      if (TARGET_LDRD)
		output_asm_insn ("strd%?\t%1, [%m0]", operands);
	      else
		output_asm_insn ("stm%?\t%m0, %M1", operands);
	    }
	  break;

        case PRE_INC:
	  gcc_assert (TARGET_LDRD);
	  if (emit)
	    output_asm_insn ("strd%?\t%1, [%m0, #8]!", operands);
	  break;

        case PRE_DEC:
	  if (emit)
	    {
	      if (TARGET_LDRD)
		output_asm_insn ("strd%?\t%1, [%m0, #-8]!", operands);
	      else
		output_asm_insn ("stmdb%?\t%m0!, %M1", operands);
	    }
	  break;

        case POST_INC:
	  if (emit)
	    {
	      if (TARGET_LDRD)
		output_asm_insn ("strd%?\t%1, [%m0], #8", operands);
	      else
		output_asm_insn ("stm%?\t%m0!, %M1", operands);
	    }
	  break;

        case POST_DEC:
	  gcc_assert (TARGET_LDRD);
	  if (emit)
	    output_asm_insn ("strd%?\t%1, [%m0], #-8", operands);
	  break;

	case PRE_MODIFY:
	case POST_MODIFY:
	  otherops[0] = operands[1];
	  otherops[1] = XEXP (XEXP (XEXP (operands[0], 0), 1), 0);
	  otherops[2] = XEXP (XEXP (XEXP (operands[0], 0), 1), 1);

	  /* IWMMXT allows offsets larger than ldrd can handle,
	     fix these up with a pair of ldr.  */
	  if (!TARGET_THUMB2
	      && CONST_INT_P (otherops[2])
	      && (INTVAL(otherops[2]) <= -256
		  || INTVAL(otherops[2]) >= 256))
	    {
	      if (GET_CODE (XEXP (operands[0], 0)) == PRE_MODIFY)
		{
		  if (emit)
		    {
		      output_asm_insn ("str%?\t%0, [%1, %2]!", otherops);
		      output_asm_insn ("str%?\t%H0, [%1, #4]", otherops);
		    }
		  if (count)
		    *count = 2;
		}
	      else
		{
		  if (emit)
		    {
		      output_asm_insn ("str%?\t%H0, [%1, #4]", otherops);
		      output_asm_insn ("str%?\t%0, [%1], %2", otherops);
		    }
		  if (count)
		    *count = 2;
		}
	    }
	  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_MODIFY)
	    {
	      if (emit)
		output_asm_insn ("strd%?\t%0, [%1, %2]!", otherops);
	    }
	  else
	    {
	      if (emit)
		output_asm_insn ("strd%?\t%0, [%1], %2", otherops);
	    }
	  break;

	case PLUS:
	  otherops[2] = XEXP (XEXP (operands[0], 0), 1);
	  if (CONST_INT_P (otherops[2]) && !TARGET_LDRD)
	    {
	      switch ((int) INTVAL (XEXP (XEXP (operands[0], 0), 1)))
		{
		case -8:
		  if (emit)
		    output_asm_insn ("stmdb%?\t%m0, %M1", operands);
		  return "";

		case -4:
		  if (TARGET_THUMB2)
		    break;
		  if (emit)
		    output_asm_insn ("stmda%?\t%m0, %M1", operands);
		  return "";

		case 4:
		  if (TARGET_THUMB2)
		    break;
		  if (emit)
		    output_asm_insn ("stmib%?\t%m0, %M1", operands);
		  return "";
		}
	    }
	  if (TARGET_LDRD
	      && (REG_P (otherops[2])
		  || TARGET_THUMB2
		  || (CONST_INT_P (otherops[2])
		      && INTVAL (otherops[2]) > -256
		      && INTVAL (otherops[2]) < 256)))
	    {
	      otherops[0] = operands[1];
	      otherops[1] = XEXP (XEXP (operands[0], 0), 0);
	      if (emit)
		output_asm_insn ("strd%?\t%0, [%1, %2]", otherops);
	      return "";
	    }
	  /* Fall through */

        default:
	  otherops[0] = adjust_address (operands[0], SImode, 4);
	  otherops[1] = operands[1];
	  if (emit)
	    {
	      output_asm_insn ("str%?\t%1, %0", operands);
	      output_asm_insn ("str%?\t%H1, %0", otherops);
	    }
	  if (count)
	    *count = 2;
	}
    }

  return "";
}

/* Output a move, load or store for quad-word vectors in ARM registers.  Only
   handles MEMs accepted by neon_vector_mem_operand with TYPE=1.  */

const char *
output_move_quad (rtx *operands)
{
  if (REG_P (operands[0]))
    {
      /* Load, or reg->reg move.  */

      if (MEM_P (operands[1]))
        {
          switch (GET_CODE (XEXP (operands[1], 0)))
            {
            case REG:
              output_asm_insn ("ldmia%?\t%m1, %M0", operands);
              break;

            case LABEL_REF:
            case CONST:
              output_asm_insn ("adr%?\t%0, %1", operands);
              output_asm_insn ("ldmia%?\t%0, %M0", operands);
              break;

            default:
              gcc_unreachable ();
            }
        }
      else
        {
          rtx ops[2];
          int dest, src, i;

          gcc_assert (REG_P (operands[1]));

          dest = REGNO (operands[0]);
          src = REGNO (operands[1]);

          /* This seems pretty dumb, but hopefully GCC won't try to do it
             very often.  */
          if (dest < src)
            for (i = 0; i < 4; i++)
              {
                ops[0] = gen_rtx_REG (SImode, dest + i);
                ops[1] = gen_rtx_REG (SImode, src + i);
                output_asm_insn ("mov%?\t%0, %1", ops);
              }
          else
            for (i = 3; i >= 0; i--)
              {
                ops[0] = gen_rtx_REG (SImode, dest + i);
                ops[1] = gen_rtx_REG (SImode, src + i);
                output_asm_insn ("mov%?\t%0, %1", ops);
              }
        }
    }
  else
    {
      gcc_assert (MEM_P (operands[0]));
      gcc_assert (REG_P (operands[1]));
      gcc_assert (!reg_overlap_mentioned_p (operands[1], operands[0]));

      switch (GET_CODE (XEXP (operands[0], 0)))
        {
        case REG:
          output_asm_insn ("stm%?\t%m0, %M1", operands);
          break;

        default:
          gcc_unreachable ();
        }
    }

  return "";
}

/* Output a VFP load or store instruction.  */

const char *
output_move_vfp (rtx *operands)
{
  rtx reg, mem, addr, ops[2];
  int load = REG_P (operands[0]);
  int dp = GET_MODE_SIZE (GET_MODE (operands[0])) == 8;
  int sp = (!TARGET_VFP_FP16INST
	    || GET_MODE_SIZE (GET_MODE (operands[0])) == 4);
  int integer_p = GET_MODE_CLASS (GET_MODE (operands[0])) == MODE_INT;
  const char *templ;
  char buff[50];
  machine_mode mode;

  reg = operands[!load];
  mem = operands[load];

  mode = GET_MODE (reg);

  gcc_assert (REG_P (reg));
  gcc_assert (IS_VFP_REGNUM (REGNO (reg)));
  gcc_assert ((mode == HFmode && TARGET_HARD_FLOAT)
	      || mode == SFmode
	      || mode == DFmode
	      || mode == HImode
	      || mode == SImode
	      || mode == DImode
              || (TARGET_NEON && VALID_NEON_DREG_MODE (mode)));
  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  switch (GET_CODE (addr))
    {
    case PRE_DEC:
      templ = "v%smdb%%?.%s\t%%0!, {%%%s1}%s";
      ops[0] = XEXP (addr, 0);
      ops[1] = reg;
      break;

    case POST_INC:
      templ = "v%smia%%?.%s\t%%0!, {%%%s1}%s";
      ops[0] = XEXP (addr, 0);
      ops[1] = reg;
      break;

    default:
      templ = "v%sr%%?.%s\t%%%s0, %%1%s";
      ops[0] = reg;
      ops[1] = mem;
      break;
    }

  sprintf (buff, templ,
	   load ? "ld" : "st",
	   dp ? "64" : sp ? "32" : "16",
	   dp ? "P" : "",
	   integer_p ? "\t%@ int" : "");
  output_asm_insn (buff, ops);

  return "";
}

/* Output a Neon double-word or quad-word load or store, or a load
   or store for larger structure modes.

   WARNING: The ordering of elements is weird in big-endian mode,
   because the EABI requires that vectors stored in memory appear
   as though they were stored by a VSTM, as required by the EABI.
   GCC RTL defines element ordering based on in-memory order.
   This can be different from the architectural ordering of elements
   within a NEON register. The intrinsics defined in arm_neon.h use the
   NEON register element ordering, not the GCC RTL element ordering.

   For example, the in-memory ordering of a big-endian a quadword
   vector with 16-bit elements when stored from register pair {d0,d1}
   will be (lowest address first, d0[N] is NEON register element N):

     [d0[3], d0[2], d0[1], d0[0], d1[7], d1[6], d1[5], d1[4]]

   When necessary, quadword registers (dN, dN+1) are moved to ARM
   registers from rN in the order:

     dN -> (rN+1, rN), dN+1 -> (rN+3, rN+2)

   So that STM/LDM can be used on vectors in ARM registers, and the
   same memory layout will result as if VSTM/VLDM were used.

   Instead of VSTM/VLDM we prefer to use VST1.64/VLD1.64 where
   possible, which allows use of appropriate alignment tags.
   Note that the choice of "64" is independent of the actual vector
   element size; this size simply ensures that the behavior is
   equivalent to VSTM/VLDM in both little-endian and big-endian mode.

   Due to limitations of those instructions, use of VST1.64/VLD1.64
   is not possible if:
    - the address contains PRE_DEC, or
    - the mode refers to more than 4 double-word registers

   In those cases, it would be possible to replace VSTM/VLDM by a
   sequence of instructions; this is not currently implemented since
   this is not certain to actually improve performance.  */

const char *
output_move_neon (rtx *operands)
{
  rtx reg, mem, addr, ops[2];
  int regno, nregs, load = REG_P (operands[0]);
  const char *templ;
  char buff[50];
  machine_mode mode;

  reg = operands[!load];
  mem = operands[load];

  mode = GET_MODE (reg);

  gcc_assert (REG_P (reg));
  regno = REGNO (reg);
  nregs = REG_NREGS (reg) / 2;
  gcc_assert (VFP_REGNO_OK_FOR_DOUBLE (regno)
	      || NEON_REGNO_OK_FOR_QUAD (regno));
  gcc_assert (VALID_NEON_DREG_MODE (mode)
	      || VALID_NEON_QREG_MODE (mode)
	      || VALID_NEON_STRUCT_MODE (mode));
  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  /* Strip off const from addresses like (const (plus (...))).  */
  if (GET_CODE (addr) == CONST && GET_CODE (XEXP (addr, 0)) == PLUS)
    addr = XEXP (addr, 0);

  switch (GET_CODE (addr))
    {
    case POST_INC:
      /* We have to use vldm / vstm for too-large modes.  */
      if (nregs > 4)
	{
	  templ = "v%smia%%?\t%%0!, %%h1";
	  ops[0] = XEXP (addr, 0);
	}
      else
	{
	  templ = "v%s1.64\t%%h1, %%A0";
	  ops[0] = mem;
	}
      ops[1] = reg;
      break;

    case PRE_DEC:
      /* We have to use vldm / vstm in this case, since there is no
	 pre-decrement form of the vld1 / vst1 instructions.  */
      templ = "v%smdb%%?\t%%0!, %%h1";
      ops[0] = XEXP (addr, 0);
      ops[1] = reg;
      break;

    case POST_MODIFY:
      /* FIXME: Not currently enabled in neon_vector_mem_operand.  */
      gcc_unreachable ();

    case REG:
      /* We have to use vldm / vstm for too-large modes.  */
      if (nregs > 1)
	{
	  if (nregs > 4)
	    templ = "v%smia%%?\t%%m0, %%h1";
	  else
	    templ = "v%s1.64\t%%h1, %%A0";

	  ops[0] = mem;
	  ops[1] = reg;
	  break;
	}
      /* Fall through.  */
    case LABEL_REF:
    case PLUS:
      {
	int i;
	int overlap = -1;
	for (i = 0; i < nregs; i++)
	  {
	    /* We're only using DImode here because it's a convenient size.  */
	    ops[0] = gen_rtx_REG (DImode, REGNO (reg) + 2 * i);
	    ops[1] = adjust_address (mem, DImode, 8 * i);
	    if (reg_overlap_mentioned_p (ops[0], mem))
	      {
		gcc_assert (overlap == -1);
		overlap = i;
	      }
	    else
	      {
		sprintf (buff, "v%sr%%?\t%%P0, %%1", load ? "ld" : "st");
		output_asm_insn (buff, ops);
	      }
	  }
	if (overlap != -1)
	  {
	    ops[0] = gen_rtx_REG (DImode, REGNO (reg) + 2 * overlap);
	    ops[1] = adjust_address (mem, SImode, 8 * overlap);
	    sprintf (buff, "v%sr%%?\t%%P0, %%1", load ? "ld" : "st");
	    output_asm_insn (buff, ops);
	  }

        return "";
      }

    default:
      gcc_unreachable ();
    }

  sprintf (buff, templ, load ? "ld" : "st");
  output_asm_insn (buff, ops);

  return "";
}

/* Compute and return the length of neon_mov<mode>, where <mode> is
   one of VSTRUCT modes: EI, OI, CI or XI.  */
int
arm_attr_length_move_neon (rtx_insn *insn)
{
  rtx reg, mem, addr;
  int load;
  machine_mode mode;

  extract_insn_cached (insn);

  if (REG_P (recog_data.operand[0]) && REG_P (recog_data.operand[1]))
    {
      mode = GET_MODE (recog_data.operand[0]);
      switch (mode)
	{
	case E_EImode:
	case E_OImode:
	  return 8;
	case E_CImode:
	  return 12;
	case E_XImode:
	  return 16;
	default:
	  gcc_unreachable ();
	}
    }

  load = REG_P (recog_data.operand[0]);
  reg = recog_data.operand[!load];
  mem = recog_data.operand[load];

  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  /* Strip off const from addresses like (const (plus (...))).  */
  if (GET_CODE (addr) == CONST && GET_CODE (XEXP (addr, 0)) == PLUS)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == LABEL_REF || GET_CODE (addr) == PLUS)
    {
      int insns = REG_NREGS (reg) / 2;
      return insns * 4;
    }
  else
    return 4;
}

/* Return nonzero if the offset in the address is an immediate.  Otherwise,
   return zero.  */

int
arm_address_offset_is_imm (rtx_insn *insn)
{
  rtx mem, addr;

  extract_insn_cached (insn);

  if (REG_P (recog_data.operand[0]))
    return 0;

  mem = recog_data.operand[0];

  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  if (REG_P (addr)
      || (GET_CODE (addr) == PLUS
	  && REG_P (XEXP (addr, 0))
	  && CONST_INT_P (XEXP (addr, 1))))
    return 1;
  else
    return 0;
}

/* Output an ADD r, s, #n where n may be too big for one instruction.
   If adding zero to one register, output nothing.  */
const char *
output_add_immediate (rtx *operands)
{
  HOST_WIDE_INT n = INTVAL (operands[2]);

  if (n != 0 || REGNO (operands[0]) != REGNO (operands[1]))
    {
      if (n < 0)
	output_multi_immediate (operands,
				"sub%?\t%0, %1, %2", "sub%?\t%0, %0, %2", 2,
				-n);
      else
	output_multi_immediate (operands,
				"add%?\t%0, %1, %2", "add%?\t%0, %0, %2", 2,
				n);
    }

  return "";
}

/* Output a multiple immediate operation.
   OPERANDS is the vector of operands referred to in the output patterns.
   INSTR1 is the output pattern to use for the first constant.
   INSTR2 is the output pattern to use for subsequent constants.
   IMMED_OP is the index of the constant slot in OPERANDS.
   N is the constant value.  */
static const char *
output_multi_immediate (rtx *operands, const char *instr1, const char *instr2,
			int immed_op, HOST_WIDE_INT n)
{
#if HOST_BITS_PER_WIDE_INT > 32
  n &= 0xffffffff;
#endif

  if (n == 0)
    {
      /* Quick and easy output.  */
      operands[immed_op] = const0_rtx;
      output_asm_insn (instr1, operands);
    }
  else
    {
      int i;
      const char * instr = instr1;

      /* Note that n is never zero here (which would give no output).  */
      for (i = 0; i < 32; i += 2)
	{
	  if (n & (3 << i))
	    {
	      operands[immed_op] = GEN_INT (n & (255 << i));
	      output_asm_insn (instr, operands);
	      instr = instr2;
	      i += 6;
	    }
	}
    }

  return "";
}

/* Return the name of a shifter operation.  */
static const char *
arm_shift_nmem(enum rtx_code code)
{
  switch (code)
    {
    case ASHIFT:
      return ARM_LSL_NAME;

    case ASHIFTRT:
      return "asr";

    case LSHIFTRT:
      return "lsr";

    case ROTATERT:
      return "ror";

    default:
      abort();
    }
}

/* Return the appropriate ARM instruction for the operation code.
   The returned result should not be overwritten.  OP is the rtx of the
   operation.  SHIFT_FIRST_ARG is TRUE if the first argument of the operator
   was shifted.  */
const char *
arithmetic_instr (rtx op, int shift_first_arg)
{
  switch (GET_CODE (op))
    {
    case PLUS:
      return "add";

    case MINUS:
      return shift_first_arg ? "rsb" : "sub";

    case IOR:
      return "orr";

    case XOR:
      return "eor";

    case AND:
      return "and";

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATERT:
      return arm_shift_nmem(GET_CODE(op));

    default:
      gcc_unreachable ();
    }
}

/* Ensure valid constant shifts and return the appropriate shift mnemonic
   for the operation code.  The returned result should not be overwritten.
   OP is the rtx code of the shift.
   On exit, *AMOUNTP will be -1 if the shift is by a register, or a constant
   shift.  */
static const char *
shift_op (rtx op, HOST_WIDE_INT *amountp)
{
  const char * mnem;
  enum rtx_code code = GET_CODE (op);

  switch (code)
    {
    case ROTATE:
      if (!CONST_INT_P (XEXP (op, 1)))
	{
	  output_operand_lossage ("invalid shift operand");
	  return NULL;
	}

      code = ROTATERT;
      *amountp = 32 - INTVAL (XEXP (op, 1));
      mnem = "ror";
      break;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATERT:
      mnem = arm_shift_nmem(code);
      if (CONST_INT_P (XEXP (op, 1)))
	{
	  *amountp = INTVAL (XEXP (op, 1));
	}
      else if (REG_P (XEXP (op, 1)))
	{
	  *amountp = -1;
	  return mnem;
	}
      else
	{
	  output_operand_lossage ("invalid shift operand");
	  return NULL;
	}
      break;

    case MULT:
      /* We never have to worry about the amount being other than a
	 power of 2, since this case can never be reloaded from a reg.  */
      if (!CONST_INT_P (XEXP (op, 1)))
	{
	  output_operand_lossage ("invalid shift operand");
	  return NULL;
	}

      *amountp = INTVAL (XEXP (op, 1)) & 0xFFFFFFFF;

      /* Amount must be a power of two.  */
      if (*amountp & (*amountp - 1))
	{
	  output_operand_lossage ("invalid shift operand");
	  return NULL;
	}

      *amountp = exact_log2 (*amountp);
      gcc_assert (IN_RANGE (*amountp, 0, 31));
      return ARM_LSL_NAME;

    default:
      output_operand_lossage ("invalid shift operand");
      return NULL;
    }

  /* This is not 100% correct, but follows from the desire to merge
     multiplication by a power of 2 with the recognizer for a
     shift.  >=32 is not a valid shift for "lsl", so we must try and
     output a shift that produces the correct arithmetical result.
     Using lsr #32 is identical except for the fact that the carry bit
     is not set correctly if we set the flags; but we never use the
     carry bit from such an operation, so we can ignore that.  */
  if (code == ROTATERT)
    /* Rotate is just modulo 32.  */
    *amountp &= 31;
  else if (*amountp != (*amountp & 31))
    {
      if (code == ASHIFT)
	mnem = "lsr";
      *amountp = 32;
    }

  /* Shifts of 0 are no-ops.  */
  if (*amountp == 0)
    return NULL;

  return mnem;
}

/* Output a .ascii pseudo-op, keeping track of lengths.  This is
   because /bin/as is horribly restrictive.  The judgement about
   whether or not each character is 'printable' (and can be output as
   is) or not (and must be printed with an octal escape) must be made
   with reference to the *host* character set -- the situation is
   similar to that discussed in the comments above pp_c_char in
   c-pretty-print.c.  */

#define MAX_ASCII_LEN 51

void
output_ascii_pseudo_op (FILE *stream, const unsigned char *p, int len)
{
  int i;
  int len_so_far = 0;

  fputs ("\t.ascii\t\"", stream);

  for (i = 0; i < len; i++)
    {
      int c = p[i];

      if (len_so_far >= MAX_ASCII_LEN)
	{
	  fputs ("\"\n\t.ascii\t\"", stream);
	  len_so_far = 0;
	}

      if (ISPRINT (c))
	{
	  if (c == '\\' || c == '\"')
	    {
	      putc ('\\', stream);
	      len_so_far++;
	    }
	  putc (c, stream);
	  len_so_far++;
	}
      else
	{
	  fprintf (stream, "\\%03o", c);
	  len_so_far += 4;
	}
    }

  fputs ("\"\n", stream);
}

/* Whether a register is callee saved or not.  This is necessary because high
   registers are marked as caller saved when optimizing for size on Thumb-1
   targets despite being callee saved in order to avoid using them.  */
#define callee_saved_reg_p(reg) \
  (!call_used_regs[reg] \
   || (TARGET_THUMB1 && optimize_size \
       && reg >= FIRST_HI_REGNUM && reg <= LAST_HI_REGNUM))

/* Compute the register save mask for registers 0 through 12
   inclusive.  This code is used by arm_compute_save_core_reg_mask ().  */

static unsigned long
arm_compute_save_reg0_reg12_mask (void)
{
  unsigned long func_type = arm_current_func_type ();
  unsigned long save_reg_mask = 0;
  unsigned int reg;

  if (IS_INTERRUPT (func_type))
    {
      unsigned int max_reg;
      /* Interrupt functions must not corrupt any registers,
	 even call clobbered ones.  If this is a leaf function
	 we can just examine the registers used by the RTL, but
	 otherwise we have to assume that whatever function is
	 called might clobber anything, and so we have to save
	 all the call-clobbered registers as well.  */
      if (ARM_FUNC_TYPE (func_type) == ARM_FT_FIQ)
	/* FIQ handlers have registers r8 - r12 banked, so
	   we only need to check r0 - r7, Normal ISRs only
	   bank r14 and r15, so we must check up to r12.
	   r13 is the stack pointer which is always preserved,
	   so we do not need to consider it here.  */
	max_reg = 7;
      else
	max_reg = 12;

      for (reg = 0; reg <= max_reg; reg++)
	if (df_regs_ever_live_p (reg)
	    || (! crtl->is_leaf && call_used_regs[reg]))
	  save_reg_mask |= (1 << reg);

      /* Also save the pic base register if necessary.  */
      if (flag_pic
	  && !TARGET_SINGLE_PIC_BASE
	  && arm_pic_register != INVALID_REGNUM
	  && crtl->uses_pic_offset_table)
	save_reg_mask |= 1 << PIC_OFFSET_TABLE_REGNUM;
    }
  else if (IS_VOLATILE(func_type))
    {
      /* For noreturn functions we historically omitted register saves
	 altogether.  However this really messes up debugging.  As a
	 compromise save just the frame pointers.  Combined with the link
	 register saved elsewhere this should be sufficient to get
	 a backtrace.  */
      if (frame_pointer_needed)
	save_reg_mask |= 1 << HARD_FRAME_POINTER_REGNUM;
      if (df_regs_ever_live_p (ARM_HARD_FRAME_POINTER_REGNUM))
	save_reg_mask |= 1 << ARM_HARD_FRAME_POINTER_REGNUM;
      if (df_regs_ever_live_p (THUMB_HARD_FRAME_POINTER_REGNUM))
	save_reg_mask |= 1 << THUMB_HARD_FRAME_POINTER_REGNUM;
    }
  else
    {
      /* In the normal case we only need to save those registers
	 which are call saved and which are used by this function.  */
      for (reg = 0; reg <= 11; reg++)
	if (df_regs_ever_live_p (reg) && callee_saved_reg_p (reg))
	  save_reg_mask |= (1 << reg);

      /* Handle the frame pointer as a special case.  */
      if (frame_pointer_needed)
	save_reg_mask |= 1 << HARD_FRAME_POINTER_REGNUM;

      /* If we aren't loading the PIC register,
	 don't stack it even though it may be live.  */
      if (flag_pic
	  && !TARGET_SINGLE_PIC_BASE
	  && arm_pic_register != INVALID_REGNUM
	  && (df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM)
	      || crtl->uses_pic_offset_table))
	save_reg_mask |= 1 << PIC_OFFSET_TABLE_REGNUM;

      /* The prologue will copy SP into R0, so save it.  */
      if (IS_STACKALIGN (func_type))
	save_reg_mask |= 1;
    }

  /* Save registers so the exception handler can modify them.  */
  if (crtl->calls_eh_return)
    {
      unsigned int i;

      for (i = 0; ; i++)
	{
	  reg = EH_RETURN_DATA_REGNO (i);
	  if (reg == INVALID_REGNUM)
	    break;
	  save_reg_mask |= 1 << reg;
	}
    }

  return save_reg_mask;
}

/* Return true if r3 is live at the start of the function.  */

static bool
arm_r3_live_at_start_p (void)
{
  /* Just look at cfg info, which is still close enough to correct at this
     point.  This gives false positives for broken functions that might use
     uninitialized data that happens to be allocated in r3, but who cares?  */
  return REGNO_REG_SET_P (df_get_live_out (ENTRY_BLOCK_PTR_FOR_FN (cfun)), 3);
}

/* Compute the number of bytes used to store the static chain register on the
   stack, above the stack frame.  We need to know this accurately to get the
   alignment of the rest of the stack frame correct.  */

static int
arm_compute_static_chain_stack_bytes (void)
{
  /* Once the value is updated from the init value of -1, do not
     re-compute.  */
  if (cfun->machine->static_chain_stack_bytes != -1)
    return cfun->machine->static_chain_stack_bytes;

  /* See the defining assertion in arm_expand_prologue.  */
  if (IS_NESTED (arm_current_func_type ())
      && ((TARGET_APCS_FRAME && frame_pointer_needed && TARGET_ARM)
	  || ((flag_stack_check == STATIC_BUILTIN_STACK_CHECK
	       || flag_stack_clash_protection)
	      && !df_regs_ever_live_p (LR_REGNUM)))
      && arm_r3_live_at_start_p ()
      && crtl->args.pretend_args_size == 0)
    return 4;

  return 0;
}

/* Compute a bit mask of which core registers need to be
   saved on the stack for the current function.
   This is used by arm_compute_frame_layout, which may add extra registers.  */

static unsigned long
arm_compute_save_core_reg_mask (void)
{
  unsigned int save_reg_mask = 0;
  unsigned long func_type = arm_current_func_type ();
  unsigned int reg;

  if (IS_NAKED (func_type))
    /* This should never really happen.  */
    return 0;

  /* If we are creating a stack frame, then we must save the frame pointer,
     IP (which will hold the old stack pointer), LR and the PC.  */
  if (TARGET_APCS_FRAME && frame_pointer_needed && TARGET_ARM)
    save_reg_mask |=
      (1 << ARM_HARD_FRAME_POINTER_REGNUM)
      | (1 << IP_REGNUM)
      | (1 << LR_REGNUM)
      | (1 << PC_REGNUM);

  save_reg_mask |= arm_compute_save_reg0_reg12_mask ();

  /* Decide if we need to save the link register.
     Interrupt routines have their own banked link register,
     so they never need to save it.
     Otherwise if we do not use the link register we do not need to save
     it.  If we are pushing other registers onto the stack however, we
     can save an instruction in the epilogue by pushing the link register
     now and then popping it back into the PC.  This incurs extra memory
     accesses though, so we only do it when optimizing for size, and only
     if we know that we will not need a fancy return sequence.  */
  if (df_regs_ever_live_p (LR_REGNUM)
      || (save_reg_mask
	  && optimize_size
	  && ARM_FUNC_TYPE (func_type) == ARM_FT_NORMAL
	  && !crtl->tail_call_emit
	  && !crtl->calls_eh_return))
    save_reg_mask |= 1 << LR_REGNUM;

  if (cfun->machine->lr_save_eliminated)
    save_reg_mask &= ~ (1 << LR_REGNUM);

  if (TARGET_REALLY_IWMMXT
      && ((bit_count (save_reg_mask)
	   + ARM_NUM_INTS (crtl->args.pretend_args_size +
			   arm_compute_static_chain_stack_bytes())
	   ) % 2) != 0)
    {
      /* The total number of registers that are going to be pushed
	 onto the stack is odd.  We need to ensure that the stack
	 is 64-bit aligned before we start to save iWMMXt registers,
	 and also before we start to create locals.  (A local variable
	 might be a double or long long which we will load/store using
	 an iWMMXt instruction).  Therefore we need to push another
	 ARM register, so that the stack will be 64-bit aligned.  We
	 try to avoid using the arg registers (r0 -r3) as they might be
	 used to pass values in a tail call.  */
      for (reg = 4; reg <= 12; reg++)
	if ((save_reg_mask & (1 << reg)) == 0)
	  break;

      if (reg <= 12)
	save_reg_mask |= (1 << reg);
      else
	{
	  cfun->machine->sibcall_blocked = 1;
	  save_reg_mask |= (1 << 3);
	}
    }

  /* We may need to push an additional register for use initializing the
     PIC base register.  */
  if (TARGET_THUMB2 && IS_NESTED (func_type) && flag_pic
      && (save_reg_mask & THUMB2_WORK_REGS) == 0)
    {
      reg = thumb_find_work_register (1 << 4);
      if (!call_used_regs[reg])
	save_reg_mask |= (1 << reg);
    }

  return save_reg_mask;
}

/* Compute a bit mask of which core registers need to be
   saved on the stack for the current function.  */
static unsigned long
thumb1_compute_save_core_reg_mask (void)
{
  unsigned long mask;
  unsigned reg;

  mask = 0;
  for (reg = 0; reg < 12; reg ++)
    if (df_regs_ever_live_p (reg) && callee_saved_reg_p (reg))
      mask |= 1 << reg;

  /* Handle the frame pointer as a special case.  */
  if (frame_pointer_needed)
    mask |= 1 << HARD_FRAME_POINTER_REGNUM;

  if (flag_pic
      && !TARGET_SINGLE_PIC_BASE
      && arm_pic_register != INVALID_REGNUM
      && crtl->uses_pic_offset_table)
    mask |= 1 << PIC_OFFSET_TABLE_REGNUM;

  /* See if we might need r11 for calls to _interwork_r11_call_via_rN().  */
  if (!frame_pointer_needed && CALLER_INTERWORKING_SLOT_SIZE > 0)
    mask |= 1 << ARM_HARD_FRAME_POINTER_REGNUM;

  /* LR will also be pushed if any lo regs are pushed.  */
  if (mask & 0xff || thumb_force_lr_save ())
    mask |= (1 << LR_REGNUM);

  /* Make sure we have a low work register if we need one.
     We will need one if we are going to push a high register,
     but we are not currently intending to push a low register.  */
  if ((mask & 0xff) == 0
      && ((mask & 0x0f00) || TARGET_BACKTRACE))
    {
      /* Use thumb_find_work_register to choose which register
	 we will use.  If the register is live then we will
	 have to push it.  Use LAST_LO_REGNUM as our fallback
	 choice for the register to select.  */
      reg = thumb_find_work_register (1 << LAST_LO_REGNUM);
      /* Make sure the register returned by thumb_find_work_register is
	 not part of the return value.  */
      if (reg * UNITS_PER_WORD <= (unsigned) arm_size_return_regs ())
	reg = LAST_LO_REGNUM;

      if (callee_saved_reg_p (reg))
	mask |= 1 << reg;
    }

  /* The 504 below is 8 bytes less than 512 because there are two possible
     alignment words.  We can't tell here if they will be present or not so we
     have to play it safe and assume that they are. */
  if ((CALLER_INTERWORKING_SLOT_SIZE +
       ROUND_UP_WORD (get_frame_size ()) +
       crtl->outgoing_args_size) >= 504)
    {
      /* This is the same as the code in thumb1_expand_prologue() which
	 determines which register to use for stack decrement. */
      for (reg = LAST_ARG_REGNUM + 1; reg <= LAST_LO_REGNUM; reg++)
	if (mask & (1 << reg))
	  break;

      if (reg > LAST_LO_REGNUM)
	{
	  /* Make sure we have a register available for stack decrement. */
	  mask |= 1 << LAST_LO_REGNUM;
	}
    }

  return mask;
}


/* Return the number of bytes required to save VFP registers.  */
static int
arm_get_vfp_saved_size (void)
{
  unsigned int regno;
  int count;
  int saved;

  saved = 0;
  /* Space for saved VFP registers.  */
  if (TARGET_HARD_FLOAT)
    {
      count = 0;
      for (regno = FIRST_VFP_REGNUM;
	   regno < LAST_VFP_REGNUM;
	   regno += 2)
	{
	  if ((!df_regs_ever_live_p (regno) || call_used_regs[regno])
	      && (!df_regs_ever_live_p (regno + 1) || call_used_regs[regno + 1]))
	    {
	      if (count > 0)
		{
		  /* Workaround ARM10 VFPr1 bug.  */
		  if (count == 2 && !arm_arch6)
		    count++;
		  saved += count * 8;
		}
	      count = 0;
	    }
	  else
	    count++;
	}
      if (count > 0)
	{
	  if (count == 2 && !arm_arch6)
	    count++;
	  saved += count * 8;
	}
    }
  return saved;
}


/* Generate a function exit sequence.  If REALLY_RETURN is false, then do
   everything bar the final return instruction.  If simple_return is true,
   then do not output epilogue, because it has already been emitted in RTL.

   Note: do not forget to update length attribute of corresponding insn pattern
   when changing assembly output (eg. length attribute of
   thumb2_cmse_entry_return when updating Armv8-M Mainline Security Extensions
   register clearing sequences).  */
const char *
output_return_instruction (rtx operand, bool really_return, bool reverse,
                           bool simple_return)
{
  char conditional[10];
  char instr[100];
  unsigned reg;
  unsigned long live_regs_mask;
  unsigned long func_type;
  arm_stack_offsets *offsets;

  func_type = arm_current_func_type ();

  if (IS_NAKED (func_type))
    return "";

  if (IS_VOLATILE (func_type) && TARGET_ABORT_NORETURN)
    {
      /* If this function was declared non-returning, and we have
	 found a tail call, then we have to trust that the called
	 function won't return.  */
      if (really_return)
	{
	  rtx ops[2];

	  /* Otherwise, trap an attempted return by aborting.  */
	  ops[0] = operand;
	  ops[1] = gen_rtx_SYMBOL_REF (Pmode, NEED_PLT_RELOC ? "abort(PLT)"
				       : "abort");
	  assemble_external_libcall (ops[1]);
	  output_asm_insn (reverse ? "bl%D0\t%a1" : "bl%d0\t%a1", ops);
	}

      return "";
    }

  gcc_assert (!cfun->calls_alloca || really_return);

  sprintf (conditional, "%%?%%%c0", reverse ? 'D' : 'd');

  cfun->machine->return_used_this_function = 1;

  offsets = arm_get_frame_offsets ();
  live_regs_mask = offsets->saved_regs_mask;

  if (!simple_return && live_regs_mask)
    {
      const char * return_reg;

      /* If we do not have any special requirements for function exit
	 (e.g. interworking) then we can load the return address
	 directly into the PC.  Otherwise we must load it into LR.  */
      if (really_return
	  && !IS_CMSE_ENTRY (func_type)
	  && (IS_INTERRUPT (func_type) || !TARGET_INTERWORK))
	return_reg = reg_names[PC_REGNUM];
      else
	return_reg = reg_names[LR_REGNUM];

      if ((live_regs_mask & (1 << IP_REGNUM)) == (1 << IP_REGNUM))
	{
	  /* There are three possible reasons for the IP register
	     being saved.  1) a stack frame was created, in which case
	     IP contains the old stack pointer, or 2) an ISR routine
	     corrupted it, or 3) it was saved to align the stack on
	     iWMMXt.  In case 1, restore IP into SP, otherwise just
	     restore IP.  */
	  if (frame_pointer_needed)
	    {
	      live_regs_mask &= ~ (1 << IP_REGNUM);
	      live_regs_mask |=   (1 << SP_REGNUM);
	    }
	  else
	    gcc_assert (IS_INTERRUPT (func_type) || TARGET_REALLY_IWMMXT);
	}

      /* On some ARM architectures it is faster to use LDR rather than
	 LDM to load a single register.  On other architectures, the
	 cost is the same.  In 26 bit mode, or for exception handlers,
	 we have to use LDM to load the PC so that the CPSR is also
	 restored.  */
      for (reg = 0; reg <= LAST_ARM_REGNUM; reg++)
	if (live_regs_mask == (1U << reg))
	  break;

      if (reg <= LAST_ARM_REGNUM
	  && (reg != LR_REGNUM
	      || ! really_return
	      || ! IS_INTERRUPT (func_type)))
	{
	  sprintf (instr, "ldr%s\t%%|%s, [%%|sp], #4", conditional,
		   (reg == LR_REGNUM) ? return_reg : reg_names[reg]);
	}
      else
	{
	  char *p;
	  int first = 1;

	  /* Generate the load multiple instruction to restore the
	     registers.  Note we can get here, even if
	     frame_pointer_needed is true, but only if sp already
	     points to the base of the saved core registers.  */
	  if (live_regs_mask & (1 << SP_REGNUM))
	    {
	      unsigned HOST_WIDE_INT stack_adjust;

	      stack_adjust = offsets->outgoing_args - offsets->saved_regs;
	      gcc_assert (stack_adjust == 0 || stack_adjust == 4);

	      if (stack_adjust && arm_arch5 && TARGET_ARM)
		  sprintf (instr, "ldmib%s\t%%|sp, {", conditional);
	      else
		{
		  /* If we can't use ldmib (SA110 bug),
		     then try to pop r3 instead.  */
		  if (stack_adjust)
		    live_regs_mask |= 1 << 3;

		  sprintf (instr, "ldmfd%s\t%%|sp, {", conditional);
		}
	    }
	  /* For interrupt returns we have to use an LDM rather than
	     a POP so that we can use the exception return variant.  */
	  else if (IS_INTERRUPT (func_type))
	    sprintf (instr, "ldmfd%s\t%%|sp!, {", conditional);
	  else
	    sprintf (instr, "pop%s\t{", conditional);

	  p = instr + strlen (instr);

	  for (reg = 0; reg <= SP_REGNUM; reg++)
	    if (live_regs_mask & (1 << reg))
	      {
		int l = strlen (reg_names[reg]);

		if (first)
		  first = 0;
		else
		  {
		    memcpy (p, ", ", 2);
		    p += 2;
		  }

		memcpy (p, "%|", 2);
		memcpy (p + 2, reg_names[reg], l);
		p += l + 2;
	      }

	  if (live_regs_mask & (1 << LR_REGNUM))
	    {
	      sprintf (p, "%s%%|%s}", first ? "" : ", ", return_reg);
	      /* If returning from an interrupt, restore the CPSR.  */
	      if (IS_INTERRUPT (func_type))
		strcat (p, "^");
	    }
	  else
	    strcpy (p, "}");
	}

      output_asm_insn (instr, & operand);

      /* See if we need to generate an extra instruction to
	 perform the actual function return.  */
      if (really_return
	  && func_type != ARM_FT_INTERWORKED
	  && (live_regs_mask & (1 << LR_REGNUM)) != 0)
	{
	  /* The return has already been handled
	     by loading the LR into the PC.  */
          return "";
	}
    }

  if (really_return)
    {
      switch ((int) ARM_FUNC_TYPE (func_type))
	{
	case ARM_FT_ISR:
	case ARM_FT_FIQ:
	  /* ??? This is wrong for unified assembly syntax.  */
	  sprintf (instr, "sub%ss\t%%|pc, %%|lr, #4", conditional);
	  break;

	case ARM_FT_INTERWORKED:
	  gcc_assert (arm_arch5 || arm_arch4t);
	  sprintf (instr, "bx%s\t%%|lr", conditional);
	  break;

	case ARM_FT_EXCEPTION:
	  /* ??? This is wrong for unified assembly syntax.  */
	  sprintf (instr, "mov%ss\t%%|pc, %%|lr", conditional);
	  break;

	default:
	  if (IS_CMSE_ENTRY (func_type))
	    {
	      /* Check if we have to clear the 'GE bits' which is only used if
		 parallel add and subtraction instructions are available.  */
	      if (TARGET_INT_SIMD)
		snprintf (instr, sizeof (instr),
			  "msr%s\tAPSR_nzcvqg, %%|lr", conditional);
	      else
		snprintf (instr, sizeof (instr),
			  "msr%s\tAPSR_nzcvq, %%|lr", conditional);

	      output_asm_insn (instr, & operand);
	      if (TARGET_HARD_FLOAT && !TARGET_THUMB1)
		{
		  /* Clear the cumulative exception-status bits (0-4,7) and the
		     condition code bits (28-31) of the FPSCR.  We need to
		     remember to clear the first scratch register used (IP) and
		     save and restore the second (r4).  */
		  snprintf (instr, sizeof (instr), "push\t{%%|r4}");
		  output_asm_insn (instr, & operand);
		  snprintf (instr, sizeof (instr), "vmrs\t%%|ip, fpscr");
		  output_asm_insn (instr, & operand);
		  snprintf (instr, sizeof (instr), "movw\t%%|r4, #65376");
		  output_asm_insn (instr, & operand);
		  snprintf (instr, sizeof (instr), "movt\t%%|r4, #4095");
		  output_asm_insn (instr, & operand);
		  snprintf (instr, sizeof (instr), "and\t%%|ip, %%|r4");
		  output_asm_insn (instr, & operand);
		  snprintf (instr, sizeof (instr), "vmsr\tfpscr, %%|ip");
		  output_asm_insn (instr, & operand);
		  snprintf (instr, sizeof (instr), "pop\t{%%|r4}");
		  output_asm_insn (instr, & operand);
		  snprintf (instr, sizeof (instr), "mov\t%%|ip, %%|lr");
		  output_asm_insn (instr, & operand);
		}
	      snprintf (instr, sizeof (instr), "bxns\t%%|lr");
	    }
	  /* Use bx if it's available.  */
	  else if (arm_arch5 || arm_arch4t)
	    sprintf (instr, "bx%s\t%%|lr", conditional);
	  else
	    sprintf (instr, "mov%s\t%%|pc, %%|lr", conditional);
	  break;
	}

      output_asm_insn (instr, & operand);
    }

  return "";
}

/* Output in FILE asm statements needed to declare the NAME of the function
   defined by its DECL node.  */

void
arm_asm_declare_function_name (FILE *file, const char *name, tree decl)
{
  size_t cmse_name_len;
  char *cmse_name = 0;
  char cmse_prefix[] = "__acle_se_";

  /* When compiling with ARMv8-M Security Extensions enabled, we should print an
     extra function label for each function with the 'cmse_nonsecure_entry'
     attribute.  This extra function label should be prepended with
     '__acle_se_', telling the linker that it needs to create secure gateway
     veneers for this function.  */
  if (use_cmse && lookup_attribute ("cmse_nonsecure_entry",
				    DECL_ATTRIBUTES (decl)))
    {
      cmse_name_len = sizeof (cmse_prefix) + strlen (name);
      cmse_name = XALLOCAVEC (char, cmse_name_len);
      snprintf (cmse_name, cmse_name_len, "%s%s", cmse_prefix, name);
      targetm.asm_out.globalize_label (file, cmse_name);

      ARM_DECLARE_FUNCTION_NAME (file, cmse_name, decl);
      ASM_OUTPUT_TYPE_DIRECTIVE (file, cmse_name, "function");
    }

  ARM_DECLARE_FUNCTION_NAME (file, name, decl);
  ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
  ASM_DECLARE_RESULT (file, DECL_RESULT (decl));
  ASM_OUTPUT_LABEL (file, name);

  if (cmse_name)
    ASM_OUTPUT_LABEL (file, cmse_name);

  ARM_OUTPUT_FN_UNWIND (file, TRUE);
}

/* Write the function name into the code section, directly preceding
   the function prologue.

   Code will be output similar to this:
     t0
	 .ascii "arm_poke_function_name", 0
	 .align
     t1
	 .word 0xff000000 + (t1 - t0)
     arm_poke_function_name
	 mov     ip, sp
	 stmfd   sp!, {fp, ip, lr, pc}
	 sub     fp, ip, #4

   When performing a stack backtrace, code can inspect the value
   of 'pc' stored at 'fp' + 0.  If the trace function then looks
   at location pc - 12 and the top 8 bits are set, then we know
   that there is a function name embedded immediately preceding this
   location and has length ((pc[-3]) & 0xff000000).

   We assume that pc is declared as a pointer to an unsigned long.

   It is of no benefit to output the function name if we are assembling
   a leaf function.  These function types will not contain a stack
   backtrace structure, therefore it is not possible to determine the
   function name.  */
void
arm_poke_function_name (FILE *stream, const char *name)
{
  unsigned long alignlength;
  unsigned long length;
  rtx           x;

  length      = strlen (name) + 1;
  alignlength = ROUND_UP_WORD (length);

  ASM_OUTPUT_ASCII (stream, name, length);
  ASM_OUTPUT_ALIGN (stream, 2);
  x = GEN_INT ((unsigned HOST_WIDE_INT) 0xff000000 + alignlength);
  assemble_aligned_integer (UNITS_PER_WORD, x);
}

/* Place some comments into the assembler stream
   describing the current function.  */
static void
arm_output_function_prologue (FILE *f)
{
  unsigned long func_type;

  /* Sanity check.  */
  gcc_assert (!arm_ccfsm_state && !arm_target_insn);

  func_type = arm_current_func_type ();

  switch ((int) ARM_FUNC_TYPE (func_type))
    {
    default:
    case ARM_FT_NORMAL:
      break;
    case ARM_FT_INTERWORKED:
      asm_fprintf (f, "\t%@ Function supports interworking.\n");
      break;
    case ARM_FT_ISR:
      asm_fprintf (f, "\t%@ Interrupt Service Routine.\n");
      break;
    case ARM_FT_FIQ:
      asm_fprintf (f, "\t%@ Fast Interrupt Service Routine.\n");
      break;
    case ARM_FT_EXCEPTION:
      asm_fprintf (f, "\t%@ ARM Exception Handler.\n");
      break;
    }

  if (IS_NAKED (func_type))
    asm_fprintf (f, "\t%@ Naked Function: prologue and epilogue provided by programmer.\n");

  if (IS_VOLATILE (func_type))
    asm_fprintf (f, "\t%@ Volatile: function does not return.\n");

  if (IS_NESTED (func_type))
    asm_fprintf (f, "\t%@ Nested: function declared inside another function.\n");
  if (IS_STACKALIGN (func_type))
    asm_fprintf (f, "\t%@ Stack Align: May be called with mis-aligned SP.\n");
  if (IS_CMSE_ENTRY (func_type))
    asm_fprintf (f, "\t%@ Non-secure entry function: called from non-secure code.\n");

  asm_fprintf (f, "\t%@ args = %wd, pretend = %d, frame = %wd\n",
	       (HOST_WIDE_INT) crtl->args.size,
	       crtl->args.pretend_args_size,
	       (HOST_WIDE_INT) get_frame_size ());

  asm_fprintf (f, "\t%@ frame_needed = %d, uses_anonymous_args = %d\n",
	       frame_pointer_needed,
	       cfun->machine->uses_anonymous_args);

  if (cfun->machine->lr_save_eliminated)
    asm_fprintf (f, "\t%@ link register save eliminated.\n");

  if (crtl->calls_eh_return)
    asm_fprintf (f, "\t@ Calls __builtin_eh_return.\n");

}

static void
arm_output_function_epilogue (FILE *)
{
  arm_stack_offsets *offsets;

  if (TARGET_THUMB1)
    {
      int regno;

      /* Emit any call-via-reg trampolines that are needed for v4t support
	 of call_reg and call_value_reg type insns.  */
      for (regno = 0; regno < LR_REGNUM; regno++)
	{
	  rtx label = cfun->machine->call_via[regno];

	  if (label != NULL)
	    {
	      switch_to_section (function_section (current_function_decl));
	      targetm.asm_out.internal_label (asm_out_file, "L",
					      CODE_LABEL_NUMBER (label));
	      asm_fprintf (asm_out_file, "\tbx\t%r\n", regno);
	    }
	}

      /* ??? Probably not safe to set this here, since it assumes that a
	 function will be emitted as assembly immediately after we generate
	 RTL for it.  This does not happen for inline functions.  */
      cfun->machine->return_used_this_function = 0;
    }
  else /* TARGET_32BIT */
    {
      /* We need to take into account any stack-frame rounding.  */
      offsets = arm_get_frame_offsets ();

      gcc_assert (!use_return_insn (FALSE, NULL)
		  || (cfun->machine->return_used_this_function != 0)
		  || offsets->saved_regs == offsets->outgoing_args
		  || frame_pointer_needed);
    }
}

/* Generate and emit a sequence of insns equivalent to PUSH, but using
   STR and STRD.  If an even number of registers are being pushed, one
   or more STRD patterns are created for each register pair.  If an
   odd number of registers are pushed, emit an initial STR followed by
   as many STRD instructions as are needed.  This works best when the
   stack is initially 64-bit aligned (the normal case), since it
   ensures that each STRD is also 64-bit aligned.  */
static void
thumb2_emit_strd_push (unsigned long saved_regs_mask)
{
  int num_regs = 0;
  int i;
  int regno;
  rtx par = NULL_RTX;
  rtx dwarf = NULL_RTX;
  rtx tmp;
  bool first = true;

  num_regs = bit_count (saved_regs_mask);

  /* Must be at least one register to save, and can't save SP or PC.  */
  gcc_assert (num_regs > 0 && num_regs <= 14);
  gcc_assert (!(saved_regs_mask & (1 << SP_REGNUM)));
  gcc_assert (!(saved_regs_mask & (1 << PC_REGNUM)));

  /* Create sequence for DWARF info.  All the frame-related data for
     debugging is held in this wrapper.  */
  dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (num_regs + 1));

  /* Describe the stack adjustment.  */
  tmp = gen_rtx_SET (stack_pointer_rtx,
		     plus_constant (Pmode, stack_pointer_rtx, -4 * num_regs));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, 0) = tmp;

  /* Find the first register.  */
  for (regno = 0; (saved_regs_mask & (1 << regno)) == 0; regno++)
    ;

  i = 0;

  /* If there's an odd number of registers to push.  Start off by
     pushing a single register.  This ensures that subsequent strd
     operations are dword aligned (assuming that SP was originally
     64-bit aligned).  */
  if ((num_regs & 1) != 0)
    {
      rtx reg, mem, insn;

      reg = gen_rtx_REG (SImode, regno);
      if (num_regs == 1)
	mem = gen_frame_mem (Pmode, gen_rtx_PRE_DEC (Pmode,
						     stack_pointer_rtx));
      else
	mem = gen_frame_mem (Pmode,
			     gen_rtx_PRE_MODIFY
			     (Pmode, stack_pointer_rtx,
			      plus_constant (Pmode, stack_pointer_rtx,
					     -4 * num_regs)));

      tmp = gen_rtx_SET (mem, reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      insn = emit_insn (tmp);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
      tmp = gen_rtx_SET (gen_frame_mem (Pmode, stack_pointer_rtx), reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      i++;
      regno++;
      XVECEXP (dwarf, 0, i) = tmp;
      first = false;
    }

  while (i < num_regs)
    if (saved_regs_mask & (1 << regno))
      {
	rtx reg1, reg2, mem1, mem2;
	rtx tmp0, tmp1, tmp2;
	int regno2;

	/* Find the register to pair with this one.  */
	for (regno2 = regno + 1; (saved_regs_mask & (1 << regno2)) == 0;
	     regno2++)
	  ;

	reg1 = gen_rtx_REG (SImode, regno);
	reg2 = gen_rtx_REG (SImode, regno2);

	if (first)
	  {
	    rtx insn;

	    first = false;
	    mem1 = gen_frame_mem (Pmode, plus_constant (Pmode,
							stack_pointer_rtx,
							-4 * num_regs));
	    mem2 = gen_frame_mem (Pmode, plus_constant (Pmode,
							stack_pointer_rtx,
							-4 * (num_regs - 1)));
	    tmp0 = gen_rtx_SET (stack_pointer_rtx,
				plus_constant (Pmode, stack_pointer_rtx,
					       -4 * (num_regs)));
	    tmp1 = gen_rtx_SET (mem1, reg1);
	    tmp2 = gen_rtx_SET (mem2, reg2);
	    RTX_FRAME_RELATED_P (tmp0) = 1;
	    RTX_FRAME_RELATED_P (tmp1) = 1;
	    RTX_FRAME_RELATED_P (tmp2) = 1;
	    par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (3));
	    XVECEXP (par, 0, 0) = tmp0;
	    XVECEXP (par, 0, 1) = tmp1;
	    XVECEXP (par, 0, 2) = tmp2;
	    insn = emit_insn (par);
	    RTX_FRAME_RELATED_P (insn) = 1;
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
	  }
	else
	  {
	    mem1 = gen_frame_mem (Pmode, plus_constant (Pmode,
							stack_pointer_rtx,
							4 * i));
	    mem2 = gen_frame_mem (Pmode, plus_constant (Pmode,
							stack_pointer_rtx,
							4 * (i + 1)));
	    tmp1 = gen_rtx_SET (mem1, reg1);
	    tmp2 = gen_rtx_SET (mem2, reg2);
	    RTX_FRAME_RELATED_P (tmp1) = 1;
	    RTX_FRAME_RELATED_P (tmp2) = 1;
	    par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
	    XVECEXP (par, 0, 0) = tmp1;
	    XVECEXP (par, 0, 1) = tmp2;
	    emit_insn (par);
	  }

	/* Create unwind information.  This is an approximation.  */
	tmp1 = gen_rtx_SET (gen_frame_mem (Pmode,
					   plus_constant (Pmode,
							  stack_pointer_rtx,
							  4 * i)),
			    reg1);
	tmp2 = gen_rtx_SET (gen_frame_mem (Pmode,
					   plus_constant (Pmode,
							  stack_pointer_rtx,
							  4 * (i + 1))),
			    reg2);

	RTX_FRAME_RELATED_P (tmp1) = 1;
	RTX_FRAME_RELATED_P (tmp2) = 1;
	XVECEXP (dwarf, 0, i + 1) = tmp1;
	XVECEXP (dwarf, 0, i + 2) = tmp2;
	i += 2;
	regno = regno2 + 1;
      }
    else
      regno++;

  return;
}

/* STRD in ARM mode requires consecutive registers.  This function emits STRD
   whenever possible, otherwise it emits single-word stores.  The first store
   also allocates stack space for all saved registers, using writeback with
   post-addressing mode.  All other stores use offset addressing.  If no STRD
   can be emitted, this function emits a sequence of single-word stores,
   and not an STM as before, because single-word stores provide more freedom
   scheduling and can be turned into an STM by peephole optimizations.  */
static void
arm_emit_strd_push (unsigned long saved_regs_mask)
{
  int num_regs = 0;
  int i, j, dwarf_index  = 0;
  int offset = 0;
  rtx dwarf = NULL_RTX;
  rtx insn = NULL_RTX;
  rtx tmp, mem;

  /* TODO: A more efficient code can be emitted by changing the
     layout, e.g., first push all pairs that can use STRD to keep the
     stack aligned, and then push all other registers.  */
  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    if (saved_regs_mask & (1 << i))
      num_regs++;

  gcc_assert (!(saved_regs_mask & (1 << SP_REGNUM)));
  gcc_assert (!(saved_regs_mask & (1 << PC_REGNUM)));
  gcc_assert (num_regs > 0);

  /* Create sequence for DWARF info.  */
  dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (num_regs + 1));

  /* For dwarf info, we generate explicit stack update.  */
  tmp = gen_rtx_SET (stack_pointer_rtx,
                     plus_constant (Pmode, stack_pointer_rtx, -4 * num_regs));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, dwarf_index++) = tmp;

  /* Save registers.  */
  offset = - 4 * num_regs;
  j = 0;
  while (j <= LAST_ARM_REGNUM)
    if (saved_regs_mask & (1 << j))
      {
        if ((j % 2 == 0)
            && (saved_regs_mask & (1 << (j + 1))))
          {
            /* Current register and previous register form register pair for
               which STRD can be generated.  */
            if (offset < 0)
              {
                /* Allocate stack space for all saved registers.  */
                tmp = plus_constant (Pmode, stack_pointer_rtx, offset);
                tmp = gen_rtx_PRE_MODIFY (Pmode, stack_pointer_rtx, tmp);
                mem = gen_frame_mem (DImode, tmp);
                offset = 0;
              }
            else if (offset > 0)
              mem = gen_frame_mem (DImode,
                                   plus_constant (Pmode,
                                                  stack_pointer_rtx,
                                                  offset));
            else
              mem = gen_frame_mem (DImode, stack_pointer_rtx);

            tmp = gen_rtx_SET (mem, gen_rtx_REG (DImode, j));
            RTX_FRAME_RELATED_P (tmp) = 1;
            tmp = emit_insn (tmp);

            /* Record the first store insn.  */
            if (dwarf_index == 1)
              insn = tmp;

            /* Generate dwarf info.  */
            mem = gen_frame_mem (SImode,
                                 plus_constant (Pmode,
                                                stack_pointer_rtx,
                                                offset));
            tmp = gen_rtx_SET (mem, gen_rtx_REG (SImode, j));
            RTX_FRAME_RELATED_P (tmp) = 1;
            XVECEXP (dwarf, 0, dwarf_index++) = tmp;

            mem = gen_frame_mem (SImode,
                                 plus_constant (Pmode,
                                                stack_pointer_rtx,
                                                offset + 4));
            tmp = gen_rtx_SET (mem, gen_rtx_REG (SImode, j + 1));
            RTX_FRAME_RELATED_P (tmp) = 1;
            XVECEXP (dwarf, 0, dwarf_index++) = tmp;

            offset += 8;
            j += 2;
          }
        else
          {
            /* Emit a single word store.  */
            if (offset < 0)
              {
                /* Allocate stack space for all saved registers.  */
                tmp = plus_constant (Pmode, stack_pointer_rtx, offset);
                tmp = gen_rtx_PRE_MODIFY (Pmode, stack_pointer_rtx, tmp);
                mem = gen_frame_mem (SImode, tmp);
                offset = 0;
              }
            else if (offset > 0)
              mem = gen_frame_mem (SImode,
                                   plus_constant (Pmode,
                                                  stack_pointer_rtx,
                                                  offset));
            else
              mem = gen_frame_mem (SImode, stack_pointer_rtx);

            tmp = gen_rtx_SET (mem, gen_rtx_REG (SImode, j));
            RTX_FRAME_RELATED_P (tmp) = 1;
            tmp = emit_insn (tmp);

            /* Record the first store insn.  */
            if (dwarf_index == 1)
              insn = tmp;

            /* Generate dwarf info.  */
            mem = gen_frame_mem (SImode,
                                 plus_constant(Pmode,
                                               stack_pointer_rtx,
                                               offset));
            tmp = gen_rtx_SET (mem, gen_rtx_REG (SImode, j));
            RTX_FRAME_RELATED_P (tmp) = 1;
            XVECEXP (dwarf, 0, dwarf_index++) = tmp;

            offset += 4;
            j += 1;
          }
      }
    else
      j++;

  /* Attach dwarf info to the first insn we generate.  */
  gcc_assert (insn != NULL_RTX);
  add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Generate and emit an insn that we will recognize as a push_multi.
   Unfortunately, since this insn does not reflect very well the actual
   semantics of the operation, we need to annotate the insn for the benefit
   of DWARF2 frame unwind information.  DWARF_REGS_MASK is a subset of
   MASK for registers that should be annotated for DWARF2 frame unwind
   information.  */
static rtx
emit_multi_reg_push (unsigned long mask, unsigned long dwarf_regs_mask)
{
  int num_regs = 0;
  int num_dwarf_regs = 0;
  int i, j;
  rtx par;
  rtx dwarf;
  int dwarf_par_index;
  rtx tmp, reg;

  /* We don't record the PC in the dwarf frame information.  */
  dwarf_regs_mask &= ~(1 << PC_REGNUM);

  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    {
      if (mask & (1 << i))
	num_regs++;
      if (dwarf_regs_mask & (1 << i))
	num_dwarf_regs++;
    }

  gcc_assert (num_regs && num_regs <= 16);
  gcc_assert ((dwarf_regs_mask & ~mask) == 0);

  /* For the body of the insn we are going to generate an UNSPEC in
     parallel with several USEs.  This allows the insn to be recognized
     by the push_multi pattern in the arm.md file.

     The body of the insn looks something like this:

       (parallel [
           (set (mem:BLK (pre_modify:SI (reg:SI sp)
	                                (const_int:SI <num>)))
	        (unspec:BLK [(reg:SI r4)] UNSPEC_PUSH_MULT))
           (use (reg:SI XX))
           (use (reg:SI YY))
	   ...
        ])

     For the frame note however, we try to be more explicit and actually
     show each register being stored into the stack frame, plus a (single)
     decrement of the stack pointer.  We do it this way in order to be
     friendly to the stack unwinding code, which only wants to see a single
     stack decrement per instruction.  The RTL we generate for the note looks
     something like this:

      (sequence [
           (set (reg:SI sp) (plus:SI (reg:SI sp) (const_int -20)))
           (set (mem:SI (reg:SI sp)) (reg:SI r4))
           (set (mem:SI (plus:SI (reg:SI sp) (const_int 4))) (reg:SI XX))
           (set (mem:SI (plus:SI (reg:SI sp) (const_int 8))) (reg:SI YY))
	   ...
        ])

     FIXME:: In an ideal world the PRE_MODIFY would not exist and
     instead we'd have a parallel expression detailing all
     the stores to the various memory addresses so that debug
     information is more up-to-date. Remember however while writing
     this to take care of the constraints with the push instruction.

     Note also that this has to be taken care of for the VFP registers.

     For more see PR43399.  */

  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num_regs));
  dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (num_dwarf_regs + 1));
  dwarf_par_index = 1;

  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    {
      if (mask & (1 << i))
	{
	  reg = gen_rtx_REG (SImode, i);

	  XVECEXP (par, 0, 0)
	    = gen_rtx_SET (gen_frame_mem
			   (BLKmode,
			    gen_rtx_PRE_MODIFY (Pmode,
						stack_pointer_rtx,
						plus_constant
						(Pmode, stack_pointer_rtx,
						 -4 * num_regs))
			    ),
			   gen_rtx_UNSPEC (BLKmode,
					   gen_rtvec (1, reg),
					   UNSPEC_PUSH_MULT));

	  if (dwarf_regs_mask & (1 << i))
	    {
	      tmp = gen_rtx_SET (gen_frame_mem (SImode, stack_pointer_rtx),
				 reg);
	      RTX_FRAME_RELATED_P (tmp) = 1;
	      XVECEXP (dwarf, 0, dwarf_par_index++) = tmp;
	    }

	  break;
	}
    }

  for (j = 1, i++; j < num_regs; i++)
    {
      if (mask & (1 << i))
	{
	  reg = gen_rtx_REG (SImode, i);

	  XVECEXP (par, 0, j) = gen_rtx_USE (VOIDmode, reg);

	  if (dwarf_regs_mask & (1 << i))
	    {
	      tmp
		= gen_rtx_SET (gen_frame_mem
			       (SImode,
				plus_constant (Pmode, stack_pointer_rtx,
					       4 * j)),
			       reg);
	      RTX_FRAME_RELATED_P (tmp) = 1;
	      XVECEXP (dwarf, 0, dwarf_par_index++) = tmp;
	    }

	  j++;
	}
    }

  par = emit_insn (par);

  tmp = gen_rtx_SET (stack_pointer_rtx,
		     plus_constant (Pmode, stack_pointer_rtx, -4 * num_regs));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, 0) = tmp;

  add_reg_note (par, REG_FRAME_RELATED_EXPR, dwarf);

  return par;
}

/* Add a REG_CFA_ADJUST_CFA REG note to INSN.
   SIZE is the offset to be adjusted.
   DEST and SRC might be stack_pointer_rtx or hard_frame_pointer_rtx.  */
static void
arm_add_cfa_adjust_cfa_note (rtx insn, int size, rtx dest, rtx src)
{
  rtx dwarf;

  RTX_FRAME_RELATED_P (insn) = 1;
  dwarf = gen_rtx_SET (dest, plus_constant (Pmode, src, size));
  add_reg_note (insn, REG_CFA_ADJUST_CFA, dwarf);
}

/* Generate and emit an insn pattern that we will recognize as a pop_multi.
   SAVED_REGS_MASK shows which registers need to be restored.

   Unfortunately, since this insn does not reflect very well the actual
   semantics of the operation, we need to annotate the insn for the benefit
   of DWARF2 frame unwind information.  */
static void
arm_emit_multi_reg_pop (unsigned long saved_regs_mask)
{
  int num_regs = 0;
  int i, j;
  rtx par;
  rtx dwarf = NULL_RTX;
  rtx tmp, reg;
  bool return_in_pc = saved_regs_mask & (1 << PC_REGNUM);
  int offset_adj;
  int emit_update;

  offset_adj = return_in_pc ? 1 : 0;
  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    if (saved_regs_mask & (1 << i))
      num_regs++;

  gcc_assert (num_regs && num_regs <= 16);

  /* If SP is in reglist, then we don't emit SP update insn.  */
  emit_update = (saved_regs_mask & (1 << SP_REGNUM)) ? 0 : 1;

  /* The parallel needs to hold num_regs SETs
     and one SET for the stack update.  */
  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num_regs + emit_update + offset_adj));

  if (return_in_pc)
    XVECEXP (par, 0, 0) = ret_rtx;

  if (emit_update)
    {
      /* Increment the stack pointer, based on there being
         num_regs 4-byte registers to restore.  */
      tmp = gen_rtx_SET (stack_pointer_rtx,
                         plus_constant (Pmode,
                                        stack_pointer_rtx,
                                        4 * num_regs));
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (par, 0, offset_adj) = tmp;
    }

  /* Now restore every reg, which may include PC.  */
  for (j = 0, i = 0; j < num_regs; i++)
    if (saved_regs_mask & (1 << i))
      {
        reg = gen_rtx_REG (SImode, i);
        if ((num_regs == 1) && emit_update && !return_in_pc)
          {
            /* Emit single load with writeback.  */
            tmp = gen_frame_mem (SImode,
                                 gen_rtx_POST_INC (Pmode,
                                                   stack_pointer_rtx));
            tmp = emit_insn (gen_rtx_SET (reg, tmp));
            REG_NOTES (tmp) = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
            return;
          }

        tmp = gen_rtx_SET (reg,
                           gen_frame_mem
                           (SImode,
                            plus_constant (Pmode, stack_pointer_rtx, 4 * j)));
        RTX_FRAME_RELATED_P (tmp) = 1;
        XVECEXP (par, 0, j + emit_update + offset_adj) = tmp;

        /* We need to maintain a sequence for DWARF info too.  As dwarf info
           should not have PC, skip PC.  */
        if (i != PC_REGNUM)
          dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);

        j++;
      }

  if (return_in_pc)
    par = emit_jump_insn (par);
  else
    par = emit_insn (par);

  REG_NOTES (par) = dwarf;
  if (!return_in_pc)
    arm_add_cfa_adjust_cfa_note (par, UNITS_PER_WORD * num_regs,
				 stack_pointer_rtx, stack_pointer_rtx);
}

/* Generate and emit an insn pattern that we will recognize as a pop_multi
   of NUM_REGS consecutive VFP regs, starting at FIRST_REG.

   Unfortunately, since this insn does not reflect very well the actual
   semantics of the operation, we need to annotate the insn for the benefit
   of DWARF2 frame unwind information.  */
static void
arm_emit_vfp_multi_reg_pop (int first_reg, int num_regs, rtx base_reg)
{
  int i, j;
  rtx par;
  rtx dwarf = NULL_RTX;
  rtx tmp, reg;

  gcc_assert (num_regs && num_regs <= 32);

    /* Workaround ARM10 VFPr1 bug.  */
  if (num_regs == 2 && !arm_arch6)
    {
      if (first_reg == 15)
        first_reg--;

      num_regs++;
    }

  /* We can emit at most 16 D-registers in a single pop_multi instruction, and
     there could be up to 32 D-registers to restore.
     If there are more than 16 D-registers, make two recursive calls,
     each of which emits one pop_multi instruction.  */
  if (num_regs > 16)
    {
      arm_emit_vfp_multi_reg_pop (first_reg, 16, base_reg);
      arm_emit_vfp_multi_reg_pop (first_reg + 16, num_regs - 16, base_reg);
      return;
    }

  /* The parallel needs to hold num_regs SETs
     and one SET for the stack update.  */
  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num_regs + 1));

  /* Increment the stack pointer, based on there being
     num_regs 8-byte registers to restore.  */
  tmp = gen_rtx_SET (base_reg, plus_constant (Pmode, base_reg, 8 * num_regs));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (par, 0, 0) = tmp;

  /* Now show every reg that will be restored, using a SET for each.  */
  for (j = 0, i=first_reg; j < num_regs; i += 2)
    {
      reg = gen_rtx_REG (DFmode, i);

      tmp = gen_rtx_SET (reg,
                         gen_frame_mem
                         (DFmode,
                          plus_constant (Pmode, base_reg, 8 * j)));
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (par, 0, j + 1) = tmp;

      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);

      j++;
    }

  par = emit_insn (par);
  REG_NOTES (par) = dwarf;

  /* Make sure cfa doesn't leave with IP_REGNUM to allow unwinding fron FP.  */
  if (REGNO (base_reg) == IP_REGNUM)
    {
      RTX_FRAME_RELATED_P (par) = 1;
      add_reg_note (par, REG_CFA_DEF_CFA, hard_frame_pointer_rtx);
    }
  else
    arm_add_cfa_adjust_cfa_note (par, 2 * UNITS_PER_WORD * num_regs,
				 base_reg, base_reg);
}

/* Generate and emit a pattern that will be recognized as LDRD pattern.  If even
   number of registers are being popped, multiple LDRD patterns are created for
   all register pairs.  If odd number of registers are popped, last register is
   loaded by using LDR pattern.  */
static void
thumb2_emit_ldrd_pop (unsigned long saved_regs_mask)
{
  int num_regs = 0;
  int i, j;
  rtx par = NULL_RTX;
  rtx dwarf = NULL_RTX;
  rtx tmp, reg, tmp1;
  bool return_in_pc = saved_regs_mask & (1 << PC_REGNUM);

  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    if (saved_regs_mask & (1 << i))
      num_regs++;

  gcc_assert (num_regs && num_regs <= 16);

  /* We cannot generate ldrd for PC.  Hence, reduce the count if PC is
     to be popped.  So, if num_regs is even, now it will become odd,
     and we can generate pop with PC.  If num_regs is odd, it will be
     even now, and ldr with return can be generated for PC.  */
  if (return_in_pc)
    num_regs--;

  gcc_assert (!(saved_regs_mask & (1 << SP_REGNUM)));

  /* Var j iterates over all the registers to gather all the registers in
     saved_regs_mask.  Var i gives index of saved registers in stack frame.
     A PARALLEL RTX of register-pair is created here, so that pattern for
     LDRD can be matched.  As PC is always last register to be popped, and
     we have already decremented num_regs if PC, we don't have to worry
     about PC in this loop.  */
  for (i = 0, j = 0; i < (num_regs - (num_regs % 2)); j++)
    if (saved_regs_mask & (1 << j))
      {
        /* Create RTX for memory load.  */
        reg = gen_rtx_REG (SImode, j);
        tmp = gen_rtx_SET (reg,
                           gen_frame_mem (SImode,
                               plus_constant (Pmode,
                                              stack_pointer_rtx, 4 * i)));
        RTX_FRAME_RELATED_P (tmp) = 1;

        if (i % 2 == 0)
          {
            /* When saved-register index (i) is even, the RTX to be emitted is
               yet to be created.  Hence create it first.  The LDRD pattern we
               are generating is :
               [ (SET (reg_t0) (MEM (PLUS (SP) (NUM))))
                 (SET (reg_t1) (MEM (PLUS (SP) (NUM + 4)))) ]
               where target registers need not be consecutive.  */
            par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
            dwarf = NULL_RTX;
          }

        /* ith register is added in PARALLEL RTX.  If i is even, the reg_i is
           added as 0th element and if i is odd, reg_i is added as 1st element
           of LDRD pattern shown above.  */
        XVECEXP (par, 0, (i % 2)) = tmp;
        dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);

        if ((i % 2) == 1)
          {
            /* When saved-register index (i) is odd, RTXs for both the registers
               to be loaded are generated in above given LDRD pattern, and the
               pattern can be emitted now.  */
            par = emit_insn (par);
            REG_NOTES (par) = dwarf;
	    RTX_FRAME_RELATED_P (par) = 1;
          }

        i++;
      }

  /* If the number of registers pushed is odd AND return_in_pc is false OR
     number of registers are even AND return_in_pc is true, last register is
     popped using LDR.  It can be PC as well.  Hence, adjust the stack first and
     then LDR with post increment.  */

  /* Increment the stack pointer, based on there being
     num_regs 4-byte registers to restore.  */
  tmp = gen_rtx_SET (stack_pointer_rtx,
                     plus_constant (Pmode, stack_pointer_rtx, 4 * i));
  RTX_FRAME_RELATED_P (tmp) = 1;
  tmp = emit_insn (tmp);
  if (!return_in_pc)
    {
      arm_add_cfa_adjust_cfa_note (tmp, UNITS_PER_WORD * i,
				   stack_pointer_rtx, stack_pointer_rtx);
    }

  dwarf = NULL_RTX;

  if (((num_regs % 2) == 1 && !return_in_pc)
      || ((num_regs % 2) == 0 && return_in_pc))
    {
      /* Scan for the single register to be popped.  Skip until the saved
         register is found.  */
      for (; (saved_regs_mask & (1 << j)) == 0; j++);

      /* Gen LDR with post increment here.  */
      tmp1 = gen_rtx_MEM (SImode,
                          gen_rtx_POST_INC (SImode,
                                            stack_pointer_rtx));
      set_mem_alias_set (tmp1, get_frame_alias_set ());

      reg = gen_rtx_REG (SImode, j);
      tmp = gen_rtx_SET (reg, tmp1);
      RTX_FRAME_RELATED_P (tmp) = 1;
      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);

      if (return_in_pc)
        {
          /* If return_in_pc, j must be PC_REGNUM.  */
          gcc_assert (j == PC_REGNUM);
          par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
          XVECEXP (par, 0, 0) = ret_rtx;
          XVECEXP (par, 0, 1) = tmp;
          par = emit_jump_insn (par);
        }
      else
        {
          par = emit_insn (tmp);
	  REG_NOTES (par) = dwarf;
	  arm_add_cfa_adjust_cfa_note (par, UNITS_PER_WORD,
				       stack_pointer_rtx, stack_pointer_rtx);
        }

    }
  else if ((num_regs % 2) == 1 && return_in_pc)
    {
      /* There are 2 registers to be popped.  So, generate the pattern
         pop_multiple_with_stack_update_and_return to pop in PC.  */
      arm_emit_multi_reg_pop (saved_regs_mask & (~((1 << j) - 1)));
    }

  return;
}

/* LDRD in ARM mode needs consecutive registers as operands.  This function
   emits LDRD whenever possible, otherwise it emits single-word loads. It uses
   offset addressing and then generates one separate stack udpate. This provides
   more scheduling freedom, compared to writeback on every load.  However,
   if the function returns using load into PC directly
   (i.e., if PC is in SAVED_REGS_MASK), the stack needs to be updated
   before the last load.  TODO: Add a peephole optimization to recognize
   the new epilogue sequence as an LDM instruction whenever possible.  TODO: Add
   peephole optimization to merge the load at stack-offset zero
   with the stack update instruction using load with writeback
   in post-index addressing mode.  */
static void
arm_emit_ldrd_pop (unsigned long saved_regs_mask)
{
  int j = 0;
  int offset = 0;
  rtx par = NULL_RTX;
  rtx dwarf = NULL_RTX;
  rtx tmp, mem;

  /* Restore saved registers.  */
  gcc_assert (!((saved_regs_mask & (1 << SP_REGNUM))));
  j = 0;
  while (j <= LAST_ARM_REGNUM)
    if (saved_regs_mask & (1 << j))
      {
        if ((j % 2) == 0
            && (saved_regs_mask & (1 << (j + 1)))
            && (j + 1) != PC_REGNUM)
          {
            /* Current register and next register form register pair for which
               LDRD can be generated. PC is always the last register popped, and
               we handle it separately.  */
            if (offset > 0)
              mem = gen_frame_mem (DImode,
                                   plus_constant (Pmode,
                                                  stack_pointer_rtx,
                                                  offset));
            else
              mem = gen_frame_mem (DImode, stack_pointer_rtx);

            tmp = gen_rtx_SET (gen_rtx_REG (DImode, j), mem);
            tmp = emit_insn (tmp);
	    RTX_FRAME_RELATED_P (tmp) = 1;

            /* Generate dwarf info.  */

            dwarf = alloc_reg_note (REG_CFA_RESTORE,
                                    gen_rtx_REG (SImode, j),
                                    NULL_RTX);
            dwarf = alloc_reg_note (REG_CFA_RESTORE,
                                    gen_rtx_REG (SImode, j + 1),
                                    dwarf);

            REG_NOTES (tmp) = dwarf;

            offset += 8;
            j += 2;
          }
        else if (j != PC_REGNUM)
          {
            /* Emit a single word load.  */
            if (offset > 0)
              mem = gen_frame_mem (SImode,
                                   plus_constant (Pmode,
                                                  stack_pointer_rtx,
                                                  offset));
            else
              mem = gen_frame_mem (SImode, stack_pointer_rtx);

            tmp = gen_rtx_SET (gen_rtx_REG (SImode, j), mem);
            tmp = emit_insn (tmp);
	    RTX_FRAME_RELATED_P (tmp) = 1;

            /* Generate dwarf info.  */
            REG_NOTES (tmp) = alloc_reg_note (REG_CFA_RESTORE,
                                              gen_rtx_REG (SImode, j),
                                              NULL_RTX);

            offset += 4;
            j += 1;
          }
        else /* j == PC_REGNUM */
          j++;
      }
    else
      j++;

  /* Update the stack.  */
  if (offset > 0)
    {
      tmp = gen_rtx_SET (stack_pointer_rtx,
                         plus_constant (Pmode,
                                        stack_pointer_rtx,
                                        offset));
      tmp = emit_insn (tmp);
      arm_add_cfa_adjust_cfa_note (tmp, offset,
				   stack_pointer_rtx, stack_pointer_rtx);
      offset = 0;
    }

  if (saved_regs_mask & (1 << PC_REGNUM))
    {
      /* Only PC is to be popped.  */
      par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
      XVECEXP (par, 0, 0) = ret_rtx;
      tmp = gen_rtx_SET (gen_rtx_REG (SImode, PC_REGNUM),
                         gen_frame_mem (SImode,
                                        gen_rtx_POST_INC (SImode,
                                                          stack_pointer_rtx)));
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (par, 0, 1) = tmp;
      par = emit_jump_insn (par);

      /* Generate dwarf info.  */
      dwarf = alloc_reg_note (REG_CFA_RESTORE,
                              gen_rtx_REG (SImode, PC_REGNUM),
                              NULL_RTX);
      REG_NOTES (par) = dwarf;
      arm_add_cfa_adjust_cfa_note (par, UNITS_PER_WORD,
				   stack_pointer_rtx, stack_pointer_rtx);
    }
}

/* Calculate the size of the return value that is passed in registers.  */
static unsigned
arm_size_return_regs (void)
{
  machine_mode mode;

  if (crtl->return_rtx != 0)
    mode = GET_MODE (crtl->return_rtx);
  else
    mode = DECL_MODE (DECL_RESULT (current_function_decl));

  return GET_MODE_SIZE (mode);
}

/* Return true if the current function needs to save/restore LR.  */
static bool
thumb_force_lr_save (void)
{
  return !cfun->machine->lr_save_eliminated
	 && (!crtl->is_leaf
	     || thumb_far_jump_used_p ()
	     || df_regs_ever_live_p (LR_REGNUM));
}

/* We do not know if r3 will be available because
   we do have an indirect tailcall happening in this
   particular case.  */
static bool
is_indirect_tailcall_p (rtx call)
{
  rtx pat = PATTERN (call);

  /* Indirect tail call.  */
  pat = XVECEXP (pat, 0, 0);
  if (GET_CODE (pat) == SET)
    pat = SET_SRC (pat);

  pat = XEXP (XEXP (pat, 0), 0);
  return REG_P (pat);
}

/* Return true if r3 is used by any of the tail call insns in the
   current function.  */
static bool
any_sibcall_could_use_r3 (void)
{
  edge_iterator ei;
  edge e;

  if (!crtl->tail_call_emit)
    return false;
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    if (e->flags & EDGE_SIBCALL)
      {
	rtx_insn *call = BB_END (e->src);
	if (!CALL_P (call))
	  call = prev_nonnote_nondebug_insn (call);
	gcc_assert (CALL_P (call) && SIBLING_CALL_P (call));
	if (find_regno_fusage (call, USE, 3)
	    || is_indirect_tailcall_p (call))
	  return true;
      }
  return false;
}


/* Compute the distance from register FROM to register TO.
   These can be the arg pointer (26), the soft frame pointer (25),
   the stack pointer (13) or the hard frame pointer (11).
   In thumb mode r7 is used as the soft frame pointer, if needed.
   Typical stack layout looks like this:

       old stack pointer -> |    |
                             ----
                            |    | \
                            |    |   saved arguments for
                            |    |   vararg functions
			    |    | /
                              --
   hard FP & arg pointer -> |    | \
                            |    |   stack
                            |    |   frame
                            |    | /
                              --
                            |    | \
                            |    |   call saved
                            |    |   registers
      soft frame pointer -> |    | /
                              --
                            |    | \
                            |    |   local
                            |    |   variables
     locals base pointer -> |    | /
                              --
                            |    | \
                            |    |   outgoing
                            |    |   arguments
   current stack pointer -> |    | /
                              --

  For a given function some or all of these stack components
  may not be needed, giving rise to the possibility of
  eliminating some of the registers.

  The values returned by this function must reflect the behavior
  of arm_expand_prologue () and arm_compute_save_core_reg_mask ().

  The sign of the number returned reflects the direction of stack
  growth, so the values are positive for all eliminations except
  from the soft frame pointer to the hard frame pointer.

  SFP may point just inside the local variables block to ensure correct
  alignment.  */


/* Return cached stack offsets.  */

static arm_stack_offsets *
arm_get_frame_offsets (void)
{
  struct arm_stack_offsets *offsets;

  offsets = &cfun->machine->stack_offsets;

  return offsets;
}


/* Calculate stack offsets.  These are used to calculate register elimination
   offsets and in prologue/epilogue code.  Also calculates which registers
   should be saved.  */

static void
arm_compute_frame_layout (void)
{
  struct arm_stack_offsets *offsets;
  unsigned long func_type;
  int saved;
  int core_saved;
  HOST_WIDE_INT frame_size;
  int i;

  offsets = &cfun->machine->stack_offsets;

  /* Initially this is the size of the local variables.  It will translated
     into an offset once we have determined the size of preceding data.  */
  frame_size = ROUND_UP_WORD (get_frame_size ());

  /* Space for variadic functions.  */
  offsets->saved_args = crtl->args.pretend_args_size;

  /* In Thumb mode this is incorrect, but never used.  */
  offsets->frame
    = (offsets->saved_args
       + arm_compute_static_chain_stack_bytes ()
       + (frame_pointer_needed ? 4 : 0));

  if (TARGET_32BIT)
    {
      unsigned int regno;

      offsets->saved_regs_mask = arm_compute_save_core_reg_mask ();
      core_saved = bit_count (offsets->saved_regs_mask) * 4;
      saved = core_saved;

      /* We know that SP will be doubleword aligned on entry, and we must
	 preserve that condition at any subroutine call.  We also require the
	 soft frame pointer to be doubleword aligned.  */

      if (TARGET_REALLY_IWMMXT)
	{
	  /* Check for the call-saved iWMMXt registers.  */
	  for (regno = FIRST_IWMMXT_REGNUM;
	       regno <= LAST_IWMMXT_REGNUM;
	       regno++)
	    if (df_regs_ever_live_p (regno) && ! call_used_regs[regno])
	      saved += 8;
	}

      func_type = arm_current_func_type ();
      /* Space for saved VFP registers.  */
      if (! IS_VOLATILE (func_type)
	  && TARGET_HARD_FLOAT)
	saved += arm_get_vfp_saved_size ();
    }
  else /* TARGET_THUMB1 */
    {
      offsets->saved_regs_mask = thumb1_compute_save_core_reg_mask ();
      core_saved = bit_count (offsets->saved_regs_mask) * 4;
      saved = core_saved;
      if (TARGET_BACKTRACE)
	saved += 16;
    }

  /* Saved registers include the stack frame.  */
  offsets->saved_regs
    = offsets->saved_args + arm_compute_static_chain_stack_bytes () + saved;
  offsets->soft_frame = offsets->saved_regs + CALLER_INTERWORKING_SLOT_SIZE;

  /* A leaf function does not need any stack alignment if it has nothing
     on the stack.  */
  if (crtl->is_leaf && frame_size == 0
      /* However if it calls alloca(), we have a dynamically allocated
	 block of BIGGEST_ALIGNMENT on stack, so still do stack alignment.  */
      && ! cfun->calls_alloca)
    {
      offsets->outgoing_args = offsets->soft_frame;
      offsets->locals_base = offsets->soft_frame;
      return;
    }

  /* Ensure SFP has the correct alignment.  */
  if (ARM_DOUBLEWORD_ALIGN
      && (offsets->soft_frame & 7))
    {
      offsets->soft_frame += 4;
      /* Try to align stack by pushing an extra reg.  Don't bother doing this
         when there is a stack frame as the alignment will be rolled into
	 the normal stack adjustment.  */
      if (frame_size + crtl->outgoing_args_size == 0)
	{
	  int reg = -1;

	  /* Register r3 is caller-saved.  Normally it does not need to be
	     saved on entry by the prologue.  However if we choose to save
	     it for padding then we may confuse the compiler into thinking
	     a prologue sequence is required when in fact it is not.  This
	     will occur when shrink-wrapping if r3 is used as a scratch
	     register and there are no other callee-saved writes.

	     This situation can be avoided when other callee-saved registers
	     are available and r3 is not mandatory if we choose a callee-saved
	     register for padding.  */
	  bool prefer_callee_reg_p = false;

	  /* If it is safe to use r3, then do so.  This sometimes
	     generates better code on Thumb-2 by avoiding the need to
	     use 32-bit push/pop instructions.  */
          if (! any_sibcall_could_use_r3 ()
	      && arm_size_return_regs () <= 12
	      && (offsets->saved_regs_mask & (1 << 3)) == 0
	      && (TARGET_THUMB2
		  || !(TARGET_LDRD && current_tune->prefer_ldrd_strd)))
	    {
	      reg = 3;
	      if (!TARGET_THUMB2)
		prefer_callee_reg_p = true;
	    }
	  if (reg == -1
	      || prefer_callee_reg_p)
	    {
	      for (i = 4; i <= (TARGET_THUMB1 ? LAST_LO_REGNUM : 11); i++)
		{
		  /* Avoid fixed registers; they may be changed at
		     arbitrary times so it's unsafe to restore them
		     during the epilogue.  */
		  if (!fixed_regs[i]
		      && (offsets->saved_regs_mask & (1 << i)) == 0)
		    {
		      reg = i;
		      break;
		    }
		}
	    }

	  if (reg != -1)
	    {
	      offsets->saved_regs += 4;
	      offsets->saved_regs_mask |= (1 << reg);
	    }
	}
    }

  offsets->locals_base = offsets->soft_frame + frame_size;
  offsets->outgoing_args = (offsets->locals_base
			    + crtl->outgoing_args_size);

  if (ARM_DOUBLEWORD_ALIGN)
    {
      /* Ensure SP remains doubleword aligned.  */
      if (offsets->outgoing_args & 7)
	offsets->outgoing_args += 4;
      gcc_assert (!(offsets->outgoing_args & 7));
    }
}


/* Calculate the relative offsets for the different stack pointers.  Positive
   offsets are in the direction of stack growth.  */

HOST_WIDE_INT
arm_compute_initial_elimination_offset (unsigned int from, unsigned int to)
{
  arm_stack_offsets *offsets;

  offsets = arm_get_frame_offsets ();

  /* OK, now we have enough information to compute the distances.
     There must be an entry in these switch tables for each pair
     of registers in ELIMINABLE_REGS, even if some of the entries
     seem to be redundant or useless.  */
  switch (from)
    {
    case ARG_POINTER_REGNUM:
      switch (to)
	{
	case THUMB_HARD_FRAME_POINTER_REGNUM:
	  return 0;

	case FRAME_POINTER_REGNUM:
	  /* This is the reverse of the soft frame pointer
	     to hard frame pointer elimination below.  */
	  return offsets->soft_frame - offsets->saved_args;

	case ARM_HARD_FRAME_POINTER_REGNUM:
	  /* This is only non-zero in the case where the static chain register
	     is stored above the frame.  */
	  return offsets->frame - offsets->saved_args - 4;

	case STACK_POINTER_REGNUM:
	  /* If nothing has been pushed on the stack at all
	     then this will return -4.  This *is* correct!  */
	  return offsets->outgoing_args - (offsets->saved_args + 4);

	default:
	  gcc_unreachable ();
	}
      gcc_unreachable ();

    case FRAME_POINTER_REGNUM:
      switch (to)
	{
	case THUMB_HARD_FRAME_POINTER_REGNUM:
	  return 0;

	case ARM_HARD_FRAME_POINTER_REGNUM:
	  /* The hard frame pointer points to the top entry in the
	     stack frame.  The soft frame pointer to the bottom entry
	     in the stack frame.  If there is no stack frame at all,
	     then they are identical.  */

	  return offsets->frame - offsets->soft_frame;

	case STACK_POINTER_REGNUM:
	  return offsets->outgoing_args - offsets->soft_frame;

	default:
	  gcc_unreachable ();
	}
      gcc_unreachable ();

    default:
      /* You cannot eliminate from the stack pointer.
	 In theory you could eliminate from the hard frame
	 pointer to the stack pointer, but this will never
	 happen, since if a stack frame is not needed the
	 hard frame pointer will never be used.  */
      gcc_unreachable ();
    }
}

/* Given FROM and TO register numbers, say whether this elimination is
   allowed.  Frame pointer elimination is automatically handled.

   All eliminations are permissible.  Note that ARG_POINTER_REGNUM and
   HARD_FRAME_POINTER_REGNUM are in fact the same thing.  If we need a frame
   pointer, we must eliminate FRAME_POINTER_REGNUM into
   HARD_FRAME_POINTER_REGNUM and not into STACK_POINTER_REGNUM or
   ARG_POINTER_REGNUM.  */

bool
arm_can_eliminate (const int from, const int to)
{
  return ((to == FRAME_POINTER_REGNUM && from == ARG_POINTER_REGNUM) ? false :
          (to == STACK_POINTER_REGNUM && frame_pointer_needed) ? false :
          (to == ARM_HARD_FRAME_POINTER_REGNUM && TARGET_THUMB) ? false :
          (to == THUMB_HARD_FRAME_POINTER_REGNUM && TARGET_ARM) ? false :
           true);
}

/* Emit RTL to save coprocessor registers on function entry.  Returns the
   number of bytes pushed.  */

static int
arm_save_coproc_regs(void)
{
  int saved_size = 0;
  unsigned reg;
  unsigned start_reg;
  rtx insn;

  for (reg = LAST_IWMMXT_REGNUM; reg >= FIRST_IWMMXT_REGNUM; reg--)
    if (df_regs_ever_live_p (reg) && ! call_used_regs[reg])
      {
	insn = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
	insn = gen_rtx_MEM (V2SImode, insn);
	insn = emit_set_insn (insn, gen_rtx_REG (V2SImode, reg));
	RTX_FRAME_RELATED_P (insn) = 1;
	saved_size += 8;
      }

  if (TARGET_HARD_FLOAT)
    {
      start_reg = FIRST_VFP_REGNUM;

      for (reg = FIRST_VFP_REGNUM; reg < LAST_VFP_REGNUM; reg += 2)
	{
	  if ((!df_regs_ever_live_p (reg) || call_used_regs[reg])
	      && (!df_regs_ever_live_p (reg + 1) || call_used_regs[reg + 1]))
	    {
	      if (start_reg != reg)
		saved_size += vfp_emit_fstmd (start_reg,
					      (reg - start_reg) / 2);
	      start_reg = reg + 2;
	    }
	}
      if (start_reg != reg)
	saved_size += vfp_emit_fstmd (start_reg,
				      (reg - start_reg) / 2);
    }
  return saved_size;
}


/* Set the Thumb frame pointer from the stack pointer.  */

static void
thumb_set_frame_pointer (arm_stack_offsets *offsets)
{
  HOST_WIDE_INT amount;
  rtx insn, dwarf;

  amount = offsets->outgoing_args - offsets->locals_base;
  if (amount < 1024)
    insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
				  stack_pointer_rtx, GEN_INT (amount)));
  else
    {
      emit_insn (gen_movsi (hard_frame_pointer_rtx, GEN_INT (amount)));
      /* Thumb-2 RTL patterns expect sp as the first input.  Thumb-1
         expects the first two operands to be the same.  */
      if (TARGET_THUMB2)
	{
	  insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
					stack_pointer_rtx,
					hard_frame_pointer_rtx));
	}
      else
	{
	  insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
					hard_frame_pointer_rtx,
					stack_pointer_rtx));
	}
      dwarf = gen_rtx_SET (hard_frame_pointer_rtx,
			   plus_constant (Pmode, stack_pointer_rtx, amount));
      RTX_FRAME_RELATED_P (dwarf) = 1;
      add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
    }

  RTX_FRAME_RELATED_P (insn) = 1;
}

struct scratch_reg {
  rtx reg;
  bool saved;
};

/* Return a short-lived scratch register for use as a 2nd scratch register on
   function entry after the registers are saved in the prologue.  This register
   must be released by means of release_scratch_register_on_entry.  IP is not
   considered since it is always used as the 1st scratch register if available.

   REGNO1 is the index number of the 1st scratch register and LIVE_REGS is the
   mask of live registers.  */

static void
get_scratch_register_on_entry (struct scratch_reg *sr, unsigned int regno1,
			       unsigned long live_regs)
{
  int regno = -1;

  sr->saved = false;

  if (regno1 != LR_REGNUM && (live_regs & (1 << LR_REGNUM)) != 0)
    regno = LR_REGNUM;
  else
    {
      unsigned int i;

      for (i = 4; i < 11; i++)
	if (regno1 != i && (live_regs & (1 << i)) != 0)
	  {
	    regno = i;
	    break;
	  }

      if (regno < 0)
	{
	  /* If IP is used as the 1st scratch register for a nested function,
	     then either r3 wasn't available or is used to preserve IP.  */
	  if (regno1 == IP_REGNUM && IS_NESTED (arm_current_func_type ()))
	    regno1 = 3;
	  regno = (regno1 == 3 ? 2 : 3);
	  sr->saved
	    = REGNO_REG_SET_P (df_get_live_out (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
			       regno);
	}
    }

  sr->reg = gen_rtx_REG (SImode, regno);
  if (sr->saved)
    {
      rtx addr = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
      rtx insn = emit_set_insn (gen_frame_mem (SImode, addr), sr->reg);
      rtx x = gen_rtx_SET (stack_pointer_rtx,
		           plus_constant (Pmode, stack_pointer_rtx, -4));
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_FRAME_RELATED_EXPR, x);
    }
}

/* Release a scratch register obtained from the preceding function.  */

static void
release_scratch_register_on_entry (struct scratch_reg *sr)
{
  if (sr->saved)
    {
      rtx addr = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);
      rtx insn = emit_set_insn (sr->reg, gen_frame_mem (SImode, addr));
      rtx x = gen_rtx_SET (stack_pointer_rtx,
			   plus_constant (Pmode, stack_pointer_rtx, 4));
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_FRAME_RELATED_EXPR, x);
    }
}

#define PROBE_INTERVAL (1 << STACK_CHECK_PROBE_INTERVAL_EXP)

#if PROBE_INTERVAL > 4096
#error Cannot use indexed addressing mode for stack probing
#endif

/* Emit code to probe a range of stack addresses from FIRST to FIRST+SIZE,
   inclusive.  These are offsets from the current stack pointer.  REGNO1
   is the index number of the 1st scratch register and LIVE_REGS is the
   mask of live registers.  */

static void
arm_emit_probe_stack_range (HOST_WIDE_INT first, HOST_WIDE_INT size,
			    unsigned int regno1, unsigned long live_regs)
{
  rtx reg1 = gen_rtx_REG (Pmode, regno1);

  /* See if we have a constant small number of probes to generate.  If so,
     that's the easy case.  */
  if (size <= PROBE_INTERVAL)
    {
      emit_move_insn (reg1, GEN_INT (first + PROBE_INTERVAL));
      emit_set_insn (reg1, gen_rtx_MINUS (Pmode, stack_pointer_rtx, reg1));
      emit_stack_probe (plus_constant (Pmode, reg1, PROBE_INTERVAL - size));
    }

  /* The run-time loop is made up of 10 insns in the generic case while the
     compile-time loop is made up of 4+2*(n-2) insns for n # of intervals.  */
  else if (size <= 5 * PROBE_INTERVAL)
    {
      HOST_WIDE_INT i, rem;

      emit_move_insn (reg1, GEN_INT (first + PROBE_INTERVAL));
      emit_set_insn (reg1, gen_rtx_MINUS (Pmode, stack_pointer_rtx, reg1));
      emit_stack_probe (reg1);

      /* Probe at FIRST + N * PROBE_INTERVAL for values of N from 2 until
	 it exceeds SIZE.  If only two probes are needed, this will not
	 generate any code.  Then probe at FIRST + SIZE.  */
      for (i = 2 * PROBE_INTERVAL; i < size; i += PROBE_INTERVAL)
	{
	  emit_set_insn (reg1, plus_constant (Pmode, reg1, -PROBE_INTERVAL));
	  emit_stack_probe (reg1);
	}

      rem = size - (i - PROBE_INTERVAL);
      if (rem > 4095 || (TARGET_THUMB2 && rem > 255))
	{
	  emit_set_insn (reg1, plus_constant (Pmode, reg1, -PROBE_INTERVAL));
	  emit_stack_probe (plus_constant (Pmode, reg1, PROBE_INTERVAL - rem));
	}
      else
	emit_stack_probe (plus_constant (Pmode, reg1, -rem));
    }

  /* Otherwise, do the same as above, but in a loop.  Note that we must be
     extra careful with variables wrapping around because we might be at
     the very top (or the very bottom) of the address space and we have
     to be able to handle this case properly; in particular, we use an
     equality test for the loop condition.  */
  else
    {
      HOST_WIDE_INT rounded_size;
      struct scratch_reg sr;

      get_scratch_register_on_entry (&sr, regno1, live_regs);

      emit_move_insn (reg1, GEN_INT (first));


      /* Step 1: round SIZE to the previous multiple of the interval.  */

      rounded_size = size & -PROBE_INTERVAL;
      emit_move_insn (sr.reg, GEN_INT (rounded_size));


      /* Step 2: compute initial and final value of the loop counter.  */

      /* TEST_ADDR = SP + FIRST.  */
      emit_set_insn (reg1, gen_rtx_MINUS (Pmode, stack_pointer_rtx, reg1));

      /* LAST_ADDR = SP + FIRST + ROUNDED_SIZE.  */
      emit_set_insn (sr.reg, gen_rtx_MINUS (Pmode, reg1, sr.reg));


      /* Step 3: the loop

	 do
	   {
	     TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	     probe at TEST_ADDR
	   }
	 while (TEST_ADDR != LAST_ADDR)

	 probes at FIRST + N * PROBE_INTERVAL for values of N from 1
	 until it is equal to ROUNDED_SIZE.  */

      emit_insn (gen_probe_stack_range (reg1, reg1, sr.reg));


      /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
	 that SIZE is equal to ROUNDED_SIZE.  */

      if (size != rounded_size)
	{
	  HOST_WIDE_INT rem = size - rounded_size;

	  if (rem > 4095 || (TARGET_THUMB2 && rem > 255))
	    {
	      emit_set_insn (sr.reg,
			     plus_constant (Pmode, sr.reg, -PROBE_INTERVAL));
	      emit_stack_probe (plus_constant (Pmode, sr.reg,
					       PROBE_INTERVAL - rem));
	    }
	  else
	    emit_stack_probe (plus_constant (Pmode, sr.reg, -rem));
	}

      release_scratch_register_on_entry (&sr);
    }

  /* Make sure nothing is scheduled before we are done.  */
  emit_insn (gen_blockage ());
}

/* Probe a range of stack addresses from REG1 to REG2 inclusive.  These are
   absolute addresses.  */

const char *
output_probe_stack_range (rtx reg1, rtx reg2)
{
  static int labelno = 0;
  char loop_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  /* Loop.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
  xops[0] = reg1;
  xops[1] = GEN_INT (PROBE_INTERVAL);
  output_asm_insn ("sub\t%0, %0, %1", xops);

  /* Probe at TEST_ADDR.  */
  output_asm_insn ("str\tr0, [%0, #0]", xops);

  /* Test if TEST_ADDR == LAST_ADDR.  */
  xops[1] = reg2;
  output_asm_insn ("cmp\t%0, %1", xops);

  /* Branch.  */
  fputs ("\tbne\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);

  return "";
}

/* Generate the prologue instructions for entry into an ARM or Thumb-2
   function.  */
void
arm_expand_prologue (void)
{
  rtx amount;
  rtx insn;
  rtx ip_rtx;
  unsigned long live_regs_mask;
  unsigned long func_type;
  int fp_offset = 0;
  int saved_pretend_args = 0;
  int saved_regs = 0;
  unsigned HOST_WIDE_INT args_to_push;
  HOST_WIDE_INT size;
  arm_stack_offsets *offsets;
  bool clobber_ip;

  func_type = arm_current_func_type ();

  /* Naked functions don't have prologues.  */
  if (IS_NAKED (func_type))
    {
      if (flag_stack_usage_info)
	current_function_static_stack_size = 0;
      return;
    }

  /* Make a copy of c_f_p_a_s as we may need to modify it locally.  */
  args_to_push = crtl->args.pretend_args_size;

  /* Compute which register we will have to save onto the stack.  */
  offsets = arm_get_frame_offsets ();
  live_regs_mask = offsets->saved_regs_mask;

  ip_rtx = gen_rtx_REG (SImode, IP_REGNUM);

  if (IS_STACKALIGN (func_type))
    {
      rtx r0, r1;

      /* Handle a word-aligned stack pointer.  We generate the following:

	  mov r0, sp
	  bic r1, r0, #7
	  mov sp, r1
	  <save and restore r0 in normal prologue/epilogue>
	  mov sp, r0
	  bx lr

	 The unwinder doesn't need to know about the stack realignment.
	 Just tell it we saved SP in r0.  */
      gcc_assert (TARGET_THUMB2 && !arm_arch_notm && args_to_push == 0);

      r0 = gen_rtx_REG (SImode, R0_REGNUM);
      r1 = gen_rtx_REG (SImode, R1_REGNUM);

      insn = emit_insn (gen_movsi (r0, stack_pointer_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_REGISTER, NULL);

      emit_insn (gen_andsi3 (r1, r0, GEN_INT (~(HOST_WIDE_INT)7)));

      /* ??? The CFA changes here, which may cause GDB to conclude that it
	 has entered a different function.  That said, the unwind info is
	 correct, individually, before and after this instruction because
	 we've described the save of SP, which will override the default
	 handling of SP as restoring from the CFA.  */
      emit_insn (gen_movsi (stack_pointer_rtx, r1));
    }

  /* Let's compute the static_chain_stack_bytes required and store it.  Right
     now the value must be -1 as stored by arm_init_machine_status ().  */
  cfun->machine->static_chain_stack_bytes
    = arm_compute_static_chain_stack_bytes ();

  /* The static chain register is the same as the IP register.  If it is
     clobbered when creating the frame, we need to save and restore it.  */
  clobber_ip = IS_NESTED (func_type)
	       && ((TARGET_APCS_FRAME && frame_pointer_needed && TARGET_ARM)
		   || ((flag_stack_check == STATIC_BUILTIN_STACK_CHECK
			|| flag_stack_clash_protection)
		       && !df_regs_ever_live_p (LR_REGNUM)
		       && arm_r3_live_at_start_p ()));

  /* Find somewhere to store IP whilst the frame is being created.
     We try the following places in order:

       1. The last argument register r3 if it is available.
       2. A slot on the stack above the frame if there are no
	  arguments to push onto the stack.
       3. Register r3 again, after pushing the argument registers
	  onto the stack, if this is a varargs function.
       4. The last slot on the stack created for the arguments to
	  push, if this isn't a varargs function.

     Note - we only need to tell the dwarf2 backend about the SP
     adjustment in the second variant; the static chain register
     doesn't need to be unwound, as it doesn't contain a value
     inherited from the caller.  */
  if (clobber_ip)
    {
      if (!arm_r3_live_at_start_p ())
	insn = emit_set_insn (gen_rtx_REG (SImode, 3), ip_rtx);
      else if (args_to_push == 0)
	{
	  rtx addr, dwarf;

	  gcc_assert(arm_compute_static_chain_stack_bytes() == 4);
	  saved_regs += 4;

	  addr = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
	  insn = emit_set_insn (gen_frame_mem (SImode, addr), ip_rtx);
	  fp_offset = 4;

	  /* Just tell the dwarf backend that we adjusted SP.  */
	  dwarf = gen_rtx_SET (stack_pointer_rtx,
			       plus_constant (Pmode, stack_pointer_rtx,
					      -fp_offset));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
	}
      else
	{
	  /* Store the args on the stack.  */
	  if (cfun->machine->uses_anonymous_args)
	    {
	      insn = emit_multi_reg_push ((0xf0 >> (args_to_push / 4)) & 0xf,
					  (0xf0 >> (args_to_push / 4)) & 0xf);
	      emit_set_insn (gen_rtx_REG (SImode, 3), ip_rtx);
	      saved_pretend_args = 1;
	    }
	  else
	    {
	      rtx addr, dwarf;

	      if (args_to_push == 4)
		addr = gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx);
	      else
		addr = gen_rtx_PRE_MODIFY (Pmode, stack_pointer_rtx,
					   plus_constant (Pmode,
							  stack_pointer_rtx,
							  -args_to_push));

	      insn = emit_set_insn (gen_frame_mem (SImode, addr), ip_rtx);

	      /* Just tell the dwarf backend that we adjusted SP.  */
	      dwarf = gen_rtx_SET (stack_pointer_rtx,
				   plus_constant (Pmode, stack_pointer_rtx,
						  -args_to_push));
	      add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
	    }

	  RTX_FRAME_RELATED_P (insn) = 1;
	  fp_offset = args_to_push;
	  args_to_push = 0;
	}
    }

  if (TARGET_APCS_FRAME && frame_pointer_needed && TARGET_ARM)
    {
      if (IS_INTERRUPT (func_type))
	{
	  /* Interrupt functions must not corrupt any registers.
	     Creating a frame pointer however, corrupts the IP
	     register, so we must push it first.  */
	  emit_multi_reg_push (1 << IP_REGNUM, 1 << IP_REGNUM);

	  /* Do not set RTX_FRAME_RELATED_P on this insn.
	     The dwarf stack unwinding code only wants to see one
	     stack decrement per function, and this is not it.  If
	     this instruction is labeled as being part of the frame
	     creation sequence then dwarf2out_frame_debug_expr will
	     die when it encounters the assignment of IP to FP
	     later on, since the use of SP here establishes SP as
	     the CFA register and not IP.

	     Anyway this instruction is not really part of the stack
	     frame creation although it is part of the prologue.  */
	}

      insn = emit_set_insn (ip_rtx,
			    plus_constant (Pmode, stack_pointer_rtx,
					   fp_offset));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (args_to_push)
    {
      /* Push the argument registers, or reserve space for them.  */
      if (cfun->machine->uses_anonymous_args)
	insn = emit_multi_reg_push
	  ((0xf0 >> (args_to_push / 4)) & 0xf,
	   (0xf0 >> (args_to_push / 4)) & 0xf);
      else
	insn = emit_insn
	  (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
		       GEN_INT (- args_to_push)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* If this is an interrupt service routine, and the link register
     is going to be pushed, and we're not generating extra
     push of IP (needed when frame is needed and frame layout if apcs),
     subtracting four from LR now will mean that the function return
     can be done with a single instruction.  */
  if ((func_type == ARM_FT_ISR || func_type == ARM_FT_FIQ)
      && (live_regs_mask & (1 << LR_REGNUM)) != 0
      && !(frame_pointer_needed && TARGET_APCS_FRAME)
      && TARGET_ARM)
    {
      rtx lr = gen_rtx_REG (SImode, LR_REGNUM);

      emit_set_insn (lr, plus_constant (SImode, lr, -4));
    }

  if (live_regs_mask)
    {
      unsigned long dwarf_regs_mask = live_regs_mask;

      saved_regs += bit_count (live_regs_mask) * 4;
      if (optimize_size && !frame_pointer_needed
	  && saved_regs == offsets->saved_regs - offsets->saved_args)
	{
	  /* If no coprocessor registers are being pushed and we don't have
	     to worry about a frame pointer then push extra registers to
	     create the stack frame.  This is done in a way that does not
	     alter the frame layout, so is independent of the epilogue.  */
	  int n;
	  int frame;
	  n = 0;
	  while (n < 8 && (live_regs_mask & (1 << n)) == 0)
	    n++;
	  frame = offsets->outgoing_args - (offsets->saved_args + saved_regs);
	  if (frame && n * 4 >= frame)
	    {
	      n = frame / 4;
	      live_regs_mask |= (1 << n) - 1;
	      saved_regs += frame;
	    }
	}

      if (TARGET_LDRD
	  && current_tune->prefer_ldrd_strd
          && !optimize_function_for_size_p (cfun))
        {
	  gcc_checking_assert (live_regs_mask == dwarf_regs_mask);
          if (TARGET_THUMB2)
	    thumb2_emit_strd_push (live_regs_mask);
          else if (TARGET_ARM
                   && !TARGET_APCS_FRAME
                   && !IS_INTERRUPT (func_type))
	    arm_emit_strd_push (live_regs_mask);
          else
            {
	      insn = emit_multi_reg_push (live_regs_mask, live_regs_mask);
              RTX_FRAME_RELATED_P (insn) = 1;
            }
        }
      else
        {
	  insn = emit_multi_reg_push (live_regs_mask, dwarf_regs_mask);
          RTX_FRAME_RELATED_P (insn) = 1;
        }
    }

  if (! IS_VOLATILE (func_type))
    saved_regs += arm_save_coproc_regs ();

  if (frame_pointer_needed && TARGET_ARM)
    {
      /* Create the new frame pointer.  */
      if (TARGET_APCS_FRAME)
	{
	  insn = GEN_INT (-(4 + args_to_push + fp_offset));
	  insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx, ip_rtx, insn));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      else
	{
	  insn = GEN_INT (saved_regs - (4 + fp_offset));
	  insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
					stack_pointer_rtx, insn));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  size = offsets->outgoing_args - offsets->saved_args;
  if (flag_stack_usage_info)
    current_function_static_stack_size = size;

  /* If this isn't an interrupt service routine and we have a frame, then do
     stack checking.  We use IP as the first scratch register, except for the
     non-APCS nested functions if LR or r3 are available (see clobber_ip).  */
  if (!IS_INTERRUPT (func_type)
      && (flag_stack_check == STATIC_BUILTIN_STACK_CHECK
	  || flag_stack_clash_protection))
    {
      unsigned int regno;

      if (!IS_NESTED (func_type) || clobber_ip)
	regno = IP_REGNUM;
      else if (df_regs_ever_live_p (LR_REGNUM))
	regno = LR_REGNUM;
      else
	regno = 3;

      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  if (size > PROBE_INTERVAL && size > get_stack_check_protect ())
	    arm_emit_probe_stack_range (get_stack_check_protect (),
					size - get_stack_check_protect (),
					regno, live_regs_mask);
	}
      else if (size > 0)
	arm_emit_probe_stack_range (get_stack_check_protect (), size,
				    regno, live_regs_mask);
    }

  /* Recover the static chain register.  */
  if (clobber_ip)
    {
      if (!arm_r3_live_at_start_p () || saved_pretend_args)
	insn = gen_rtx_REG (SImode, 3);
      else
	{
	  insn = plus_constant (Pmode, hard_frame_pointer_rtx, 4);
	  insn = gen_frame_mem (SImode, insn);
	}
      emit_set_insn (ip_rtx, insn);
      emit_insn (gen_force_register_use (ip_rtx));
    }

  if (offsets->outgoing_args != offsets->saved_args + saved_regs)
    {
      /* This add can produce multiple insns for a large constant, so we
	 need to get tricky.  */
      rtx_insn *last = get_last_insn ();

      amount = GEN_INT (offsets->saved_args + saved_regs
			- offsets->outgoing_args);

      insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				    amount));
      do
	{
	  last = last ? NEXT_INSN (last) : get_insns ();
	  RTX_FRAME_RELATED_P (last) = 1;
	}
      while (last != insn);

      /* If the frame pointer is needed, emit a special barrier that
	 will prevent the scheduler from moving stores to the frame
	 before the stack adjustment.  */
      if (frame_pointer_needed)
	emit_insn (gen_stack_tie (stack_pointer_rtx,
				  hard_frame_pointer_rtx));
    }


  if (frame_pointer_needed && TARGET_THUMB2)
    thumb_set_frame_pointer (offsets);

  if (flag_pic && arm_pic_register != INVALID_REGNUM)
    {
      unsigned long mask;

      mask = live_regs_mask;
      mask &= THUMB2_WORK_REGS;
      if (!IS_NESTED (func_type))
	mask |= (1 << IP_REGNUM);
      arm_load_pic_register (mask);
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  Similarly if the user has requested no
     scheduling in the prolog.  Similarly if we want non-call exceptions
     using the EABI unwinder, to prevent faulting instructions from being
     swapped with a stack adjustment.  */
  if (crtl->profile || !TARGET_SCHED_PROLOG
      || (arm_except_unwind_info (&global_options) == UI_TARGET
	  && cfun->can_throw_non_call_exceptions))
    emit_insn (gen_blockage ());

  /* If the link register is being kept alive, with the return address in it,
     then make sure that it does not get reused by the ce2 pass.  */
  if ((live_regs_mask & (1 << LR_REGNUM)) == 0)
    cfun->machine->lr_save_eliminated = 1;
}

/* Print condition code to STREAM.  Helper function for arm_print_operand.  */
static void
arm_print_condition (FILE *stream)
{
  if (arm_ccfsm_state == 3 || arm_ccfsm_state == 4)
    {
      /* Branch conversion is not implemented for Thumb-2.  */
      if (TARGET_THUMB)
	{
	  output_operand_lossage ("predicated Thumb instruction");
	  return;
	}
      if (current_insn_predicate != NULL)
	{
	  output_operand_lossage
	    ("predicated instruction in conditional sequence");
	  return;
	}

      fputs (arm_condition_codes[arm_current_cc], stream);
    }
  else if (current_insn_predicate)
    {
      enum arm_cond_code code;

      if (TARGET_THUMB1)
	{
	  output_operand_lossage ("predicated Thumb instruction");
	  return;
	}

      code = get_arm_condition_code (current_insn_predicate);
      fputs (arm_condition_codes[code], stream);
    }
}


/* Globally reserved letters: acln
   Puncutation letters currently used: @_|?().!#
   Lower case letters currently used: bcdefhimpqtvwxyz
   Upper case letters currently used: ABCDFGHJKLMNOPQRSTU
   Letters previously used, but now deprecated/obsolete: sVWXYZ.

   Note that the global reservation for 'c' is only for CONSTANT_ADDRESS_P.

   If CODE is 'd', then the X is a condition operand and the instruction
   should only be executed if the condition is true.
   if CODE is 'D', then the X is a condition operand and the instruction
   should only be executed if the condition is false: however, if the mode
   of the comparison is CCFPEmode, then always execute the instruction -- we
   do this because in these circumstances !GE does not necessarily imply LT;
   in these cases the instruction pattern will take care to make sure that
   an instruction containing %d will follow, thereby undoing the effects of
   doing this instruction unconditionally.
   If CODE is 'N' then X is a floating point operand that must be negated
   before output.
   If CODE is 'B' then output a bitwise inverted value of X (a const int).
   If X is a REG and CODE is `M', output a ldm/stm style multi-reg.  */
static void
arm_print_operand (FILE *stream, rtx x, int code)
{
  switch (code)
    {
    case '@':
      fputs (ASM_COMMENT_START, stream);
      return;

    case '_':
      fputs (user_label_prefix, stream);
      return;

    case '|':
      fputs (REGISTER_PREFIX, stream);
      return;

    case '?':
      arm_print_condition (stream);
      return;

    case '.':
      /* The current condition code for a condition code setting instruction.
	 Preceded by 's' in unified syntax, otherwise followed by 's'.  */
      fputc('s', stream);
      arm_print_condition (stream);
      return;

    case '!':
      /* If the instruction is conditionally executed then print
	 the current condition code, otherwise print 's'.  */
      gcc_assert (TARGET_THUMB2);
      if (current_insn_predicate)
	arm_print_condition (stream);
      else
	fputc('s', stream);
      break;

    /* %# is a "break" sequence. It doesn't output anything, but is used to
       separate e.g. operand numbers from following text, if that text consists
       of further digits which we don't want to be part of the operand
       number.  */
    case '#':
      return;

    case 'N':
      {
	REAL_VALUE_TYPE r;
	r = real_value_negate (CONST_DOUBLE_REAL_VALUE (x));
	fprintf (stream, "%s", fp_const_from_val (&r));
      }
      return;

    /* An integer or symbol address without a preceding # sign.  */
    case 'c':
      switch (GET_CODE (x))
	{
	case CONST_INT:
	  fprintf (stream, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
	  break;

	case SYMBOL_REF:
	  output_addr_const (stream, x);
	  break;

	case CONST:
	  if (GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
	    {
	      output_addr_const (stream, x);
	      break;
	    }
	  /* Fall through.  */

	default:
	  output_operand_lossage ("Unsupported operand for code '%c'", code);
	}
      return;

    /* An integer that we want to print in HEX.  */
    case 'x':
      switch (GET_CODE (x))
	{
	case CONST_INT:
	  fprintf (stream, "#" HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
	  break;

	default:
	  output_operand_lossage ("Unsupported operand for code '%c'", code);
	}
      return;

    case 'B':
      if (CONST_INT_P (x))
	{
	  HOST_WIDE_INT val;
	  val = ARM_SIGN_EXTEND (~INTVAL (x));
	  fprintf (stream, HOST_WIDE_INT_PRINT_DEC, val);
	}
      else
	{
	  putc ('~', stream);
	  output_addr_const (stream, x);
	}
      return;

    case 'b':
      /* Print the log2 of a CONST_INT.  */
      {
	HOST_WIDE_INT val;

	if (!CONST_INT_P (x)
	    || (val = exact_log2 (INTVAL (x) & 0xffffffff)) < 0)
	  output_operand_lossage ("Unsupported operand for code '%c'", code);
	else
	  fprintf (stream, "#" HOST_WIDE_INT_PRINT_DEC, val);
      }
      return;

    case 'L':
      /* The low 16 bits of an immediate constant.  */
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, INTVAL(x) & 0xffff);
      return;

    case 'i':
      fprintf (stream, "%s", arithmetic_instr (x, 1));
      return;

    case 'I':
      fprintf (stream, "%s", arithmetic_instr (x, 0));
      return;

    case 'S':
      {
	HOST_WIDE_INT val;
	const char *shift;

	shift = shift_op (x, &val);

	if (shift)
	  {
	    fprintf (stream, ", %s ", shift);
	    if (val == -1)
	      arm_print_operand (stream, XEXP (x, 1), 0);
	    else
	      fprintf (stream, "#" HOST_WIDE_INT_PRINT_DEC, val);
	  }
      }
      return;

      /* An explanation of the 'Q', 'R' and 'H' register operands:

	 In a pair of registers containing a DI or DF value the 'Q'
	 operand returns the register number of the register containing
	 the least significant part of the value.  The 'R' operand returns
	 the register number of the register containing the most
	 significant part of the value.

	 The 'H' operand returns the higher of the two register numbers.
	 On a run where WORDS_BIG_ENDIAN is true the 'H' operand is the
	 same as the 'Q' operand, since the most significant part of the
	 value is held in the lower number register.  The reverse is true
	 on systems where WORDS_BIG_ENDIAN is false.

	 The purpose of these operands is to distinguish between cases
	 where the endian-ness of the values is important (for example
	 when they are added together), and cases where the endian-ness
	 is irrelevant, but the order of register operations is important.
	 For example when loading a value from memory into a register
	 pair, the endian-ness does not matter.  Provided that the value
	 from the lower memory address is put into the lower numbered
	 register, and the value from the higher address is put into the
	 higher numbered register, the load will work regardless of whether
	 the value being loaded is big-wordian or little-wordian.  The
	 order of the two register loads can matter however, if the address
	 of the memory location is actually held in one of the registers
	 being overwritten by the load.

	 The 'Q' and 'R' constraints are also available for 64-bit
	 constants.  */
    case 'Q':
      if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	{
	  rtx part = gen_lowpart (SImode, x);
	  fprintf (stream, "#" HOST_WIDE_INT_PRINT_DEC, INTVAL (part));
	  return;
	}

      if (!REG_P (x) || REGNO (x) > LAST_ARM_REGNUM)
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      asm_fprintf (stream, "%r", REGNO (x) + (WORDS_BIG_ENDIAN ? 1 : 0));
      return;

    case 'R':
      if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	{
	  machine_mode mode = GET_MODE (x);
	  rtx part;

	  if (mode == VOIDmode)
	    mode = DImode;
	  part = gen_highpart_mode (SImode, mode, x);
	  fprintf (stream, "#" HOST_WIDE_INT_PRINT_DEC, INTVAL (part));
	  return;
	}

      if (!REG_P (x) || REGNO (x) > LAST_ARM_REGNUM)
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      asm_fprintf (stream, "%r", REGNO (x) + (WORDS_BIG_ENDIAN ? 0 : 1));
      return;

    case 'H':
      if (!REG_P (x) || REGNO (x) > LAST_ARM_REGNUM)
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      asm_fprintf (stream, "%r", REGNO (x) + 1);
      return;

    case 'J':
      if (!REG_P (x) || REGNO (x) > LAST_ARM_REGNUM)
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      asm_fprintf (stream, "%r", REGNO (x) + (WORDS_BIG_ENDIAN ? 3 : 2));
      return;

    case 'K':
      if (!REG_P (x) || REGNO (x) > LAST_ARM_REGNUM)
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      asm_fprintf (stream, "%r", REGNO (x) + (WORDS_BIG_ENDIAN ? 2 : 3));
      return;

    case 'm':
      asm_fprintf (stream, "%r",
		   REG_P (XEXP (x, 0))
		   ? REGNO (XEXP (x, 0)) : REGNO (XEXP (XEXP (x, 0), 0)));
      return;

    case 'M':
      asm_fprintf (stream, "{%r-%r}",
		   REGNO (x),
		   REGNO (x) + ARM_NUM_REGS (GET_MODE (x)) - 1);
      return;

    /* Like 'M', but writing doubleword vector registers, for use by Neon
       insns.  */
    case 'h':
      {
        int regno = (REGNO (x) - FIRST_VFP_REGNUM) / 2;
        int numregs = ARM_NUM_REGS (GET_MODE (x)) / 2;
        if (numregs == 1)
          asm_fprintf (stream, "{d%d}", regno);
        else
          asm_fprintf (stream, "{d%d-d%d}", regno, regno + numregs - 1);
      }
      return;

    case 'd':
      /* CONST_TRUE_RTX means always -- that's the default.  */
      if (x == const_true_rtx)
	return;

      if (!COMPARISON_P (x))
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      fputs (arm_condition_codes[get_arm_condition_code (x)],
	     stream);
      return;

    case 'D':
      /* CONST_TRUE_RTX means not always -- i.e. never.  We shouldn't ever
	 want to do that.  */
      if (x == const_true_rtx)
	{
	  output_operand_lossage ("instruction never executed");
	  return;
	}
      if (!COMPARISON_P (x))
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      fputs (arm_condition_codes[ARM_INVERSE_CONDITION_CODE
				 (get_arm_condition_code (x))],
	     stream);
      return;

    case 's':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
      /* Former Maverick support, removed after GCC-4.7.  */
      output_operand_lossage ("obsolete Maverick format code '%c'", code);
      return;

    case 'U':
      if (!REG_P (x)
	  || REGNO (x) < FIRST_IWMMXT_GR_REGNUM
	  || REGNO (x) > LAST_IWMMXT_GR_REGNUM)
	/* Bad value for wCG register number.  */
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      else
	fprintf (stream, "%d", REGNO (x) - FIRST_IWMMXT_GR_REGNUM);
      return;

      /* Print an iWMMXt control register name.  */
    case 'w':
      if (!CONST_INT_P (x)
	  || INTVAL (x) < 0
	  || INTVAL (x) >= 16)
	/* Bad value for wC register number.  */
	{
	  output_operand_lossage ("invalid operand for code '%c'", code);
	  return;
	}

      else
	{
	  static const char * wc_reg_names [16] =
	    {
	      "wCID",  "wCon",  "wCSSF", "wCASF",
	      "wC4",   "wC5",   "wC6",   "wC7",
	      "wCGR0", "wCGR1", "wCGR2", "wCGR3",
	      "wC12",  "wC13",  "wC14",  "wC15"
	    };

	  fputs (wc_reg_names [INTVAL (x)], stream);
	}
      return;

    /* Print the high single-precision register of a VFP double-precision
       register.  */
    case 'p':
      {
        machine_mode mode = GET_MODE (x);
        int regno;

        if (GET_MODE_SIZE (mode) != 8 || !REG_P (x))
          {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
          }

        regno = REGNO (x);
        if (!VFP_REGNO_OK_FOR_DOUBLE (regno))
          {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
          }

	fprintf (stream, "s%d", regno - FIRST_VFP_REGNUM + 1);
      }
      return;

    /* Print a VFP/Neon double precision or quad precision register name.  */
    case 'P':
    case 'q':
      {
	machine_mode mode = GET_MODE (x);
	int is_quad = (code == 'q');
	int regno;

	if (GET_MODE_SIZE (mode) != (is_quad ? 16 : 8))
	  {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
	  }

	if (!REG_P (x)
	    || !IS_VFP_REGNUM (REGNO (x)))
	  {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
	  }

	regno = REGNO (x);
	if ((is_quad && !NEON_REGNO_OK_FOR_QUAD (regno))
            || (!is_quad && !VFP_REGNO_OK_FOR_DOUBLE (regno)))
	  {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
	  }

	fprintf (stream, "%c%d", is_quad ? 'q' : 'd',
	  (regno - FIRST_VFP_REGNUM) >> (is_quad ? 2 : 1));
      }
      return;

    /* These two codes print the low/high doubleword register of a Neon quad
       register, respectively.  For pair-structure types, can also print
       low/high quadword registers.  */
    case 'e':
    case 'f':
      {
        machine_mode mode = GET_MODE (x);
        int regno;

        if ((GET_MODE_SIZE (mode) != 16
	     && GET_MODE_SIZE (mode) != 32) || !REG_P (x))
          {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
          }

        regno = REGNO (x);
        if (!NEON_REGNO_OK_FOR_QUAD (regno))
          {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
          }

        if (GET_MODE_SIZE (mode) == 16)
          fprintf (stream, "d%d", ((regno - FIRST_VFP_REGNUM) >> 1)
				  + (code == 'f' ? 1 : 0));
        else
          fprintf (stream, "q%d", ((regno - FIRST_VFP_REGNUM) >> 2)
				  + (code == 'f' ? 1 : 0));
      }
      return;

    /* Print a VFPv3 floating-point constant, represented as an integer
       index.  */
    case 'G':
      {
        int index = vfp3_const_double_index (x);
	gcc_assert (index != -1);
	fprintf (stream, "%d", index);
      }
      return;

    /* Print bits representing opcode features for Neon.

       Bit 0 is 1 for signed, 0 for unsigned.  Floats count as signed
       and polynomials as unsigned.

       Bit 1 is 1 for floats and polynomials, 0 for ordinary integers.

       Bit 2 is 1 for rounding functions, 0 otherwise.  */

    /* Identify the type as 's', 'u', 'p' or 'f'.  */
    case 'T':
      {
        HOST_WIDE_INT bits = INTVAL (x);
        fputc ("uspf"[bits & 3], stream);
      }
      return;

    /* Likewise, but signed and unsigned integers are both 'i'.  */
    case 'F':
      {
        HOST_WIDE_INT bits = INTVAL (x);
        fputc ("iipf"[bits & 3], stream);
      }
      return;

    /* As for 'T', but emit 'u' instead of 'p'.  */
    case 't':
      {
        HOST_WIDE_INT bits = INTVAL (x);
        fputc ("usuf"[bits & 3], stream);
      }
      return;

    /* Bit 2: rounding (vs none).  */
    case 'O':
      {
        HOST_WIDE_INT bits = INTVAL (x);
        fputs ((bits & 4) != 0 ? "r" : "", stream);
      }
      return;

    /* Memory operand for vld1/vst1 instruction.  */
    case 'A':
      {
	rtx addr;
	bool postinc = FALSE;
	rtx postinc_reg = NULL;
	unsigned align, memsize, align_bits;

	gcc_assert (MEM_P (x));
	addr = XEXP (x, 0);
	if (GET_CODE (addr) == POST_INC)
	  {
	    postinc = 1;
	    addr = XEXP (addr, 0);
	  }
	if (GET_CODE (addr) == POST_MODIFY)
	  {
	    postinc_reg = XEXP( XEXP (addr, 1), 1);
	    addr = XEXP (addr, 0);
	  }
	asm_fprintf (stream, "[%r", REGNO (addr));

	/* We know the alignment of this access, so we can emit a hint in the
	   instruction (for some alignments) as an aid to the memory subsystem
	   of the target.  */
	align = MEM_ALIGN (x) >> 3;
	memsize = MEM_SIZE (x);

	/* Only certain alignment specifiers are supported by the hardware.  */
	if (memsize == 32 && (align % 32) == 0)
	  align_bits = 256;
	else if ((memsize == 16 || memsize == 32) && (align % 16) == 0)
	  align_bits = 128;
	else if (memsize >= 8 && (align % 8) == 0)
	  align_bits = 64;
	else
	  align_bits = 0;

	if (align_bits != 0)
	  asm_fprintf (stream, ":%d", align_bits);

	asm_fprintf (stream, "]");

	if (postinc)
	  fputs("!", stream);
	if (postinc_reg)
	  asm_fprintf (stream, ", %r", REGNO (postinc_reg));
      }
      return;

    case 'C':
      {
	rtx addr;

	gcc_assert (MEM_P (x));
	addr = XEXP (x, 0);
	gcc_assert (REG_P (addr));
	asm_fprintf (stream, "[%r]", REGNO (addr));
      }
      return;

    /* Translate an S register number into a D register number and element index.  */
    case 'y':
      {
        machine_mode mode = GET_MODE (x);
        int regno;

        if (GET_MODE_SIZE (mode) != 4 || !REG_P (x))
          {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
          }

        regno = REGNO (x);
        if (!VFP_REGNO_OK_FOR_SINGLE (regno))
          {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
          }

	regno = regno - FIRST_VFP_REGNUM;
	fprintf (stream, "d%d[%d]", regno / 2, regno % 2);
      }
      return;

    case 'v':
	gcc_assert (CONST_DOUBLE_P (x));
	int result;
	result = vfp3_const_double_for_fract_bits (x);
	if (result == 0)
	  result = vfp3_const_double_for_bits (x);
	fprintf (stream, "#%d", result);
	return;

    /* Register specifier for vld1.16/vst1.16.  Translate the S register
       number into a D register number and element index.  */
    case 'z':
      {
        machine_mode mode = GET_MODE (x);
        int regno;

        if (GET_MODE_SIZE (mode) != 2 || !REG_P (x))
          {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
          }

        regno = REGNO (x);
        if (!VFP_REGNO_OK_FOR_SINGLE (regno))
          {
	    output_operand_lossage ("invalid operand for code '%c'", code);
	    return;
          }

	regno = regno - FIRST_VFP_REGNUM;
	fprintf (stream, "d%d[%d]", regno/2, ((regno % 2) ? 2 : 0));
      }
      return;

    default:
      if (x == 0)
	{
	  output_operand_lossage ("missing operand");
	  return;
	}

      switch (GET_CODE (x))
	{
	case REG:
	  asm_fprintf (stream, "%r", REGNO (x));
	  break;

	case MEM:
	  output_address (GET_MODE (x), XEXP (x, 0));
	  break;

	case CONST_DOUBLE:
	  {
            char fpstr[20];
            real_to_decimal (fpstr, CONST_DOUBLE_REAL_VALUE (x),
			      sizeof (fpstr), 0, 1);
            fprintf (stream, "#%s", fpstr);
	  }
	  break;

	default:
	  gcc_assert (GET_CODE (x) != NEG);
	  fputc ('#', stream);
	  if (GET_CODE (x) == HIGH)
	    {
	      fputs (":lower16:", stream);
	      x = XEXP (x, 0);
	    }

	  output_addr_const (stream, x);
	  break;
	}
    }
}

/* Target hook for printing a memory address.  */
static void
arm_print_operand_address (FILE *stream, machine_mode mode, rtx x)
{
  if (TARGET_32BIT)
    {
      int is_minus = GET_CODE (x) == MINUS;

      if (REG_P (x))
	asm_fprintf (stream, "[%r]", REGNO (x));
      else if (GET_CODE (x) == PLUS || is_minus)
	{
	  rtx base = XEXP (x, 0);
	  rtx index = XEXP (x, 1);
	  HOST_WIDE_INT offset = 0;
	  if (!REG_P (base)
	      || (REG_P (index) && REGNO (index) == SP_REGNUM))
	    {
	      /* Ensure that BASE is a register.  */
	      /* (one of them must be).  */
	      /* Also ensure the SP is not used as in index register.  */
	      std::swap (base, index);
	    }
	  switch (GET_CODE (index))
	    {
	    case CONST_INT:
	      offset = INTVAL (index);
	      if (is_minus)
		offset = -offset;
	      asm_fprintf (stream, "[%r, #%wd]",
			   REGNO (base), offset);
	      break;

	    case REG:
	      asm_fprintf (stream, "[%r, %s%r]",
			   REGNO (base), is_minus ? "-" : "",
			   REGNO (index));
	      break;

	    case MULT:
	    case ASHIFTRT:
	    case LSHIFTRT:
	    case ASHIFT:
	    case ROTATERT:
	      {
		asm_fprintf (stream, "[%r, %s%r",
			     REGNO (base), is_minus ? "-" : "",
			     REGNO (XEXP (index, 0)));
		arm_print_operand (stream, index, 'S');
		fputs ("]", stream);
		break;
	      }

	    default:
	      gcc_unreachable ();
	    }
	}
      else if (GET_CODE (x) == PRE_INC || GET_CODE (x) == POST_INC
	       || GET_CODE (x) == PRE_DEC || GET_CODE (x) == POST_DEC)
	{
	  gcc_assert (REG_P (XEXP (x, 0)));

	  if (GET_CODE (x) == PRE_DEC || GET_CODE (x) == PRE_INC)
	    asm_fprintf (stream, "[%r, #%s%d]!",
			 REGNO (XEXP (x, 0)),
			 GET_CODE (x) == PRE_DEC ? "-" : "",
			 GET_MODE_SIZE (mode));
	  else
	    asm_fprintf (stream, "[%r], #%s%d",
			 REGNO (XEXP (x, 0)),
			 GET_CODE (x) == POST_DEC ? "-" : "",
			 GET_MODE_SIZE (mode));
	}
      else if (GET_CODE (x) == PRE_MODIFY)
	{
	  asm_fprintf (stream, "[%r, ", REGNO (XEXP (x, 0)));
	  if (CONST_INT_P (XEXP (XEXP (x, 1), 1)))
	    asm_fprintf (stream, "#%wd]!",
			 INTVAL (XEXP (XEXP (x, 1), 1)));
	  else
	    asm_fprintf (stream, "%r]!",
			 REGNO (XEXP (XEXP (x, 1), 1)));
	}
      else if (GET_CODE (x) == POST_MODIFY)
	{
	  asm_fprintf (stream, "[%r], ", REGNO (XEXP (x, 0)));
	  if (CONST_INT_P (XEXP (XEXP (x, 1), 1)))
	    asm_fprintf (stream, "#%wd",
			 INTVAL (XEXP (XEXP (x, 1), 1)));
	  else
	    asm_fprintf (stream, "%r",
			 REGNO (XEXP (XEXP (x, 1), 1)));
	}
      else output_addr_const (stream, x);
    }
  else
    {
      if (REG_P (x))
	asm_fprintf (stream, "[%r]", REGNO (x));
      else if (GET_CODE (x) == POST_INC)
	asm_fprintf (stream, "%r!", REGNO (XEXP (x, 0)));
      else if (GET_CODE (x) == PLUS)
	{
	  gcc_assert (REG_P (XEXP (x, 0)));
	  if (CONST_INT_P (XEXP (x, 1)))
	    asm_fprintf (stream, "[%r, #%wd]",
			 REGNO (XEXP (x, 0)),
			 INTVAL (XEXP (x, 1)));
	  else
	    asm_fprintf (stream, "[%r, %r]",
			 REGNO (XEXP (x, 0)),
			 REGNO (XEXP (x, 1)));
	}
      else
	output_addr_const (stream, x);
    }
}

/* Target hook for indicating whether a punctuation character for
   TARGET_PRINT_OPERAND is valid.  */
static bool
arm_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '@' || code == '|' || code == '.'
	  || code == '(' || code == ')' || code == '#'
	  || (TARGET_32BIT && (code == '?'))
	  || (TARGET_THUMB2 && (code == '!'))
	  || (TARGET_THUMB && (code == '_')));
}

/* Target hook for assembling integer objects.  The ARM version needs to
   handle word-sized values specially.  */
static bool
arm_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  machine_mode mode;

  if (size == UNITS_PER_WORD && aligned_p)
    {
      fputs ("\t.word\t", asm_out_file);
      output_addr_const (asm_out_file, x);

      /* Mark symbols as position independent.  We only do this in the
	 .text segment, not in the .data segment.  */
      if (NEED_GOT_RELOC && flag_pic && making_const_table &&
	  (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF))
	{
	  /* See legitimize_pic_address for an explanation of the
	     TARGET_VXWORKS_RTP check.  */
	  /* References to weak symbols cannot be resolved locally:
	     they may be overridden by a non-weak definition at link
	     time.  */
	  if (!arm_pic_data_is_text_relative
	      || (GET_CODE (x) == SYMBOL_REF
		  && (!SYMBOL_REF_LOCAL_P (x)
		      || (SYMBOL_REF_DECL (x)
			  ? DECL_WEAK (SYMBOL_REF_DECL (x)) : 0))))
	    fputs ("(GOT)", asm_out_file);
	  else
	    fputs ("(GOTOFF)", asm_out_file);
	}
      fputc ('\n', asm_out_file);
      return true;
    }

  mode = GET_MODE (x);

  if (arm_vector_mode_supported_p (mode))
    {
      int i, units;

      gcc_assert (GET_CODE (x) == CONST_VECTOR);

      units = CONST_VECTOR_NUNITS (x);
      size = GET_MODE_UNIT_SIZE (mode);

      if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
        for (i = 0; i < units; i++)
	  {
	    rtx elt = CONST_VECTOR_ELT (x, i);
	    assemble_integer
	      (elt, size, i == 0 ? BIGGEST_ALIGNMENT : size * BITS_PER_UNIT, 1);
	  }
      else
        for (i = 0; i < units; i++)
          {
            rtx elt = CONST_VECTOR_ELT (x, i);
	    assemble_real
	      (*CONST_DOUBLE_REAL_VALUE (elt),
	       as_a <scalar_float_mode> (GET_MODE_INNER (mode)),
	       i == 0 ? BIGGEST_ALIGNMENT : size * BITS_PER_UNIT);
          }

      return true;
    }

  return default_assemble_integer (x, size, aligned_p);
}

static void
arm_elf_asm_cdtor (rtx symbol, int priority, bool is_ctor)
{
  section *s;

  if (!TARGET_AAPCS_BASED)
    {
      (is_ctor ?
       default_named_section_asm_out_constructor
       : default_named_section_asm_out_destructor) (symbol, priority);
      return;
    }

  /* Put these in the .init_array section, using a special relocation.  */
  if (priority != DEFAULT_INIT_PRIORITY)
    {
      char buf[18];
      sprintf (buf, "%s.%.5u",
	       is_ctor ? ".init_array" : ".fini_array",
	       priority);
      s = get_section (buf, SECTION_WRITE | SECTION_NOTYPE, NULL_TREE);
    }
  else if (is_ctor)
    s = ctors_section;
  else
    s = dtors_section;

  switch_to_section (s);
  assemble_align (POINTER_SIZE);
  fputs ("\t.word\t", asm_out_file);
  output_addr_const (asm_out_file, symbol);
  fputs ("(target1)\n", asm_out_file);
}

/* Add a function to the list of static constructors.  */

static void
arm_elf_asm_constructor (rtx symbol, int priority)
{
  arm_elf_asm_cdtor (symbol, priority, /*is_ctor=*/true);
}

/* Add a function to the list of static destructors.  */

static void
arm_elf_asm_destructor (rtx symbol, int priority)
{
  arm_elf_asm_cdtor (symbol, priority, /*is_ctor=*/false);
}

/* A finite state machine takes care of noticing whether or not instructions
   can be conditionally executed, and thus decrease execution time and code
   size by deleting branch instructions.  The fsm is controlled by
   final_prescan_insn, and controls the actions of ASM_OUTPUT_OPCODE.  */

/* The state of the fsm controlling condition codes are:
   0: normal, do nothing special
   1: make ASM_OUTPUT_OPCODE not output this instruction
   2: make ASM_OUTPUT_OPCODE not output this instruction
   3: make instructions conditional
   4: make instructions conditional

   State transitions (state->state by whom under condition):
   0 -> 1 final_prescan_insn if the `target' is a label
   0 -> 2 final_prescan_insn if the `target' is an unconditional branch
   1 -> 3 ASM_OUTPUT_OPCODE after not having output the conditional branch
   2 -> 4 ASM_OUTPUT_OPCODE after not having output the conditional branch
   3 -> 0 (*targetm.asm_out.internal_label) if the `target' label is reached
          (the target label has CODE_LABEL_NUMBER equal to arm_target_label).
   4 -> 0 final_prescan_insn if the `target' unconditional branch is reached
          (the target insn is arm_target_insn).

   If the jump clobbers the conditions then we use states 2 and 4.

   A similar thing can be done with conditional return insns.

   XXX In case the `target' is an unconditional branch, this conditionalising
   of the instructions always reduces code size, but not always execution
   time.  But then, I want to reduce the code size to somewhere near what
   /bin/cc produces.  */

/* In addition to this, state is maintained for Thumb-2 COND_EXEC
   instructions.  When a COND_EXEC instruction is seen the subsequent
   instructions are scanned so that multiple conditional instructions can be
   combined into a single IT block.  arm_condexec_count and arm_condexec_mask
   specify the length and true/false mask for the IT block.  These will be
   decremented/zeroed by arm_asm_output_opcode as the insns are output.  */

/* Returns the index of the ARM condition code string in
   `arm_condition_codes', or ARM_NV if the comparison is invalid.
   COMPARISON should be an rtx like `(eq (...) (...))'.  */

enum arm_cond_code
maybe_get_arm_condition_code (rtx comparison)
{
  machine_mode mode = GET_MODE (XEXP (comparison, 0));
  enum arm_cond_code code;
  enum rtx_code comp_code = GET_CODE (comparison);

  if (GET_MODE_CLASS (mode) != MODE_CC)
    mode = SELECT_CC_MODE (comp_code, XEXP (comparison, 0),
			   XEXP (comparison, 1));

  switch (mode)
    {
    case E_CC_DNEmode: code = ARM_NE; goto dominance;
    case E_CC_DEQmode: code = ARM_EQ; goto dominance;
    case E_CC_DGEmode: code = ARM_GE; goto dominance;
    case E_CC_DGTmode: code = ARM_GT; goto dominance;
    case E_CC_DLEmode: code = ARM_LE; goto dominance;
    case E_CC_DLTmode: code = ARM_LT; goto dominance;
    case E_CC_DGEUmode: code = ARM_CS; goto dominance;
    case E_CC_DGTUmode: code = ARM_HI; goto dominance;
    case E_CC_DLEUmode: code = ARM_LS; goto dominance;
    case E_CC_DLTUmode: code = ARM_CC;

    dominance:
      if (comp_code == EQ)
	return ARM_INVERSE_CONDITION_CODE (code);
      if (comp_code == NE)
	return code;
      return ARM_NV;

    case E_CC_NOOVmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case GE: return ARM_PL;
	case LT: return ARM_MI;
	default: return ARM_NV;
	}

    case E_CC_Zmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	default: return ARM_NV;
	}

    case E_CC_Nmode:
      switch (comp_code)
	{
	case NE: return ARM_MI;
	case EQ: return ARM_PL;
	default: return ARM_NV;
	}

    case E_CCFPEmode:
    case E_CCFPmode:
      /* We can handle all cases except UNEQ and LTGT.  */
      switch (comp_code)
	{
	case GE: return ARM_GE;
	case GT: return ARM_GT;
	case LE: return ARM_LS;
	case LT: return ARM_MI;
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case ORDERED: return ARM_VC;
	case UNORDERED: return ARM_VS;
	case UNLT: return ARM_LT;
	case UNLE: return ARM_LE;
	case UNGT: return ARM_HI;
	case UNGE: return ARM_PL;
	  /* UNEQ and LTGT do not have a representation.  */
	case UNEQ: /* Fall through.  */
	case LTGT: /* Fall through.  */
	default: return ARM_NV;
	}

    case E_CC_SWPmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case GE: return ARM_LE;
	case GT: return ARM_LT;
	case LE: return ARM_GE;
	case LT: return ARM_GT;
	case GEU: return ARM_LS;
	case GTU: return ARM_CC;
	case LEU: return ARM_CS;
	case LTU: return ARM_HI;
	default: return ARM_NV;
	}

    case E_CC_Cmode:
      switch (comp_code)
	{
	case LTU: return ARM_CS;
	case GEU: return ARM_CC;
	case NE: return ARM_CS;
	case EQ: return ARM_CC;
	default: return ARM_NV;
	}

    case E_CC_CZmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case GEU: return ARM_CS;
	case GTU: return ARM_HI;
	case LEU: return ARM_LS;
	case LTU: return ARM_CC;
	default: return ARM_NV;
	}

    case E_CC_NCVmode:
      switch (comp_code)
	{
	case GE: return ARM_GE;
	case LT: return ARM_LT;
	case GEU: return ARM_CS;
	case LTU: return ARM_CC;
	default: return ARM_NV;
	}

    case E_CC_Vmode:
      switch (comp_code)
	{
	case NE: return ARM_VS;
	case EQ: return ARM_VC;
	default: return ARM_NV;
	}

    case E_CCmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case GE: return ARM_GE;
	case GT: return ARM_GT;
	case LE: return ARM_LE;
	case LT: return ARM_LT;
	case GEU: return ARM_CS;
	case GTU: return ARM_HI;
	case LEU: return ARM_LS;
	case LTU: return ARM_CC;
	default: return ARM_NV;
	}

    default: gcc_unreachable ();
    }
}

/* Like maybe_get_arm_condition_code, but never return ARM_NV.  */
static enum arm_cond_code
get_arm_condition_code (rtx comparison)
{
  enum arm_cond_code code = maybe_get_arm_condition_code (comparison);
  gcc_assert (code != ARM_NV);
  return code;
}

/* Implement TARGET_FIXED_CONDITION_CODE_REGS.  We only have condition
   code registers when not targetting Thumb1.  The VFP condition register
   only exists when generating hard-float code.  */
static bool
arm_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  if (!TARGET_32BIT)
    return false;

  *p1 = CC_REGNUM;
  *p2 = TARGET_HARD_FLOAT ? VFPCC_REGNUM : INVALID_REGNUM;
  return true;
}

/* Tell arm_asm_output_opcode to output IT blocks for conditionally executed
   instructions.  */
void
thumb2_final_prescan_insn (rtx_insn *insn)
{
  rtx_insn *first_insn = insn;
  rtx body = PATTERN (insn);
  rtx predicate;
  enum arm_cond_code code;
  int n;
  int mask;
  int max;

  /* max_insns_skipped in the tune was already taken into account in the
     cost model of ifcvt pass when generating COND_EXEC insns.  At this stage
     just emit the IT blocks as we can.  It does not make sense to split
     the IT blocks.  */
  max = MAX_INSN_PER_IT_BLOCK;

  /* Remove the previous insn from the count of insns to be output.  */
  if (arm_condexec_count)
      arm_condexec_count--;

  /* Nothing to do if we are already inside a conditional block.  */
  if (arm_condexec_count)
    return;

  if (GET_CODE (body) != COND_EXEC)
    return;

  /* Conditional jumps are implemented directly.  */
  if (JUMP_P (insn))
    return;

  predicate = COND_EXEC_TEST (body);
  arm_current_cc = get_arm_condition_code (predicate);

  n = get_attr_ce_count (insn);
  arm_condexec_count = 1;
  arm_condexec_mask = (1 << n) - 1;
  arm_condexec_masklen = n;
  /* See if subsequent instructions can be combined into the same block.  */
  for (;;)
    {
      insn = next_nonnote_insn (insn);

      /* Jumping into the middle of an IT block is illegal, so a label or
         barrier terminates the block.  */
      if (!NONJUMP_INSN_P (insn) && !JUMP_P (insn))
	break;

      body = PATTERN (insn);
      /* USE and CLOBBER aren't really insns, so just skip them.  */
      if (GET_CODE (body) == USE
	  || GET_CODE (body) == CLOBBER)
	continue;

      /* ??? Recognize conditional jumps, and combine them with IT blocks.  */
      if (GET_CODE (body) != COND_EXEC)
	break;
      /* Maximum number of conditionally executed instructions in a block.  */
      n = get_attr_ce_count (insn);
      if (arm_condexec_masklen + n > max)
	break;

      predicate = COND_EXEC_TEST (body);
      code = get_arm_condition_code (predicate);
      mask = (1 << n) - 1;
      if (arm_current_cc == code)
	arm_condexec_mask |= (mask << arm_condexec_masklen);
      else if (arm_current_cc != ARM_INVERSE_CONDITION_CODE(code))
	break;

      arm_condexec_count++;
      arm_condexec_masklen += n;

      /* A jump must be the last instruction in a conditional block.  */
      if (JUMP_P (insn))
	break;
    }
  /* Restore recog_data (getting the attributes of other insns can
     destroy this array, but final.c assumes that it remains intact
     across this call).  */
  extract_constrain_insn_cached (first_insn);
}

void
arm_final_prescan_insn (rtx_insn *insn)
{
  /* BODY will hold the body of INSN.  */
  rtx body = PATTERN (insn);

  /* This will be 1 if trying to repeat the trick, and things need to be
     reversed if it appears to fail.  */
  int reverse = 0;

  /* If we start with a return insn, we only succeed if we find another one.  */
  int seeking_return = 0;
  enum rtx_code return_code = UNKNOWN;

  /* START_INSN will hold the insn from where we start looking.  This is the
     first insn after the following code_label if REVERSE is true.  */
  rtx_insn *start_insn = insn;

  /* If in state 4, check if the target branch is reached, in order to
     change back to state 0.  */
  if (arm_ccfsm_state == 4)
    {
      if (insn == arm_target_insn)
	{
	  arm_target_insn = NULL;
	  arm_ccfsm_state = 0;
	}
      return;
    }

  /* If in state 3, it is possible to repeat the trick, if this insn is an
     unconditional branch to a label, and immediately following this branch
     is the previous target label which is only used once, and the label this
     branch jumps to is not too far off.  */
  if (arm_ccfsm_state == 3)
    {
      if (simplejump_p (insn))
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (BARRIER_P (start_insn))
	    {
	      /* XXX Isn't this always a barrier?  */
	      start_insn = next_nonnote_insn (start_insn);
	    }
	  if (LABEL_P (start_insn)
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    reverse = TRUE;
	  else
	    return;
	}
      else if (ANY_RETURN_P (body))
        {
	  start_insn = next_nonnote_insn (start_insn);
	  if (BARRIER_P (start_insn))
	    start_insn = next_nonnote_insn (start_insn);
	  if (LABEL_P (start_insn)
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    {
	      reverse = TRUE;
	      seeking_return = 1;
	      return_code = GET_CODE (body);
	    }
	  else
	    return;
        }
      else
	return;
    }

  gcc_assert (!arm_ccfsm_state || reverse);
  if (!JUMP_P (insn))
    return;

  /* This jump might be paralleled with a clobber of the condition codes
     the jump should always come first */
  if (GET_CODE (body) == PARALLEL && XVECLEN (body, 0) > 0)
    body = XVECEXP (body, 0, 0);

  if (reverse
      || (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
	  && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE))
    {
      int insns_skipped;
      int fail = FALSE, succeed = FALSE;
      /* Flag which part of the IF_THEN_ELSE is the LABEL_REF.  */
      int then_not_else = TRUE;
      rtx_insn *this_insn = start_insn;
      rtx label = 0;

      /* Register the insn jumped to.  */
      if (reverse)
        {
	  if (!seeking_return)
	    label = XEXP (SET_SRC (body), 0);
        }
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == LABEL_REF)
	label = XEXP (XEXP (SET_SRC (body), 1), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == LABEL_REF)
	{
	  label = XEXP (XEXP (SET_SRC (body), 2), 0);
	  then_not_else = FALSE;
	}
      else if (ANY_RETURN_P (XEXP (SET_SRC (body), 1)))
	{
	  seeking_return = 1;
	  return_code = GET_CODE (XEXP (SET_SRC (body), 1));
	}
      else if (ANY_RETURN_P (XEXP (SET_SRC (body), 2)))
        {
	  seeking_return = 1;
	  return_code = GET_CODE (XEXP (SET_SRC (body), 2));
	  then_not_else = FALSE;
        }
      else
	gcc_unreachable ();

      /* See how many insns this branch skips, and what kind of insns.  If all
	 insns are okay, and the label or unconditional branch to the same
	 label is not too far away, succeed.  */
      for (insns_skipped = 0;
	   !fail && !succeed && insns_skipped++ < max_insns_skipped;)
	{
	  rtx scanbody;

	  this_insn = next_nonnote_insn (this_insn);
	  if (!this_insn)
	    break;

	  switch (GET_CODE (this_insn))
	    {
	    case CODE_LABEL:
	      /* Succeed if it is the target label, otherwise fail since
		 control falls in from somewhere else.  */
	      if (this_insn == label)
		{
		  arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case BARRIER:
	      /* Succeed if the following insn is the target label.
		 Otherwise fail.
		 If return insns are used then the last insn in a function
		 will be a barrier.  */
	      this_insn = next_nonnote_insn (this_insn);
	      if (this_insn && this_insn == label)
		{
		  arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case CALL_INSN:
	      /* The AAPCS says that conditional calls should not be
		 used since they make interworking inefficient (the
		 linker can't transform BL<cond> into BLX).  That's
		 only a problem if the machine has BLX.  */
	      if (arm_arch5)
		{
		  fail = TRUE;
		  break;
		}

	      /* Succeed if the following insn is the target label, or
		 if the following two insns are a barrier and the
		 target label.  */
	      this_insn = next_nonnote_insn (this_insn);
	      if (this_insn && BARRIER_P (this_insn))
		this_insn = next_nonnote_insn (this_insn);

	      if (this_insn && this_insn == label
		  && insns_skipped < max_insns_skipped)
		{
		  arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case JUMP_INSN:
      	      /* If this is an unconditional branch to the same label, succeed.
		 If it is to another label, do nothing.  If it is conditional,
		 fail.  */
	      /* XXX Probably, the tests for SET and the PC are
		 unnecessary.  */

	      scanbody = PATTERN (this_insn);
	      if (GET_CODE (scanbody) == SET
		  && GET_CODE (SET_DEST (scanbody)) == PC)
		{
		  if (GET_CODE (SET_SRC (scanbody)) == LABEL_REF
		      && XEXP (SET_SRC (scanbody), 0) == label && !reverse)
		    {
		      arm_ccfsm_state = 2;
		      succeed = TRUE;
		    }
		  else if (GET_CODE (SET_SRC (scanbody)) == IF_THEN_ELSE)
		    fail = TRUE;
		}
	      /* Fail if a conditional return is undesirable (e.g. on a
		 StrongARM), but still allow this if optimizing for size.  */
	      else if (GET_CODE (scanbody) == return_code
		       && !use_return_insn (TRUE, NULL)
		       && !optimize_size)
		fail = TRUE;
	      else if (GET_CODE (scanbody) == return_code)
	        {
		  arm_ccfsm_state = 2;
		  succeed = TRUE;
	        }
	      else if (GET_CODE (scanbody) == PARALLEL)
	        {
		  switch (get_attr_conds (this_insn))
		    {
		    case CONDS_NOCOND:
		      break;
		    default:
		      fail = TRUE;
		      break;
		    }
		}
	      else
		fail = TRUE;	/* Unrecognized jump (e.g. epilogue).  */

	      break;

	    case INSN:
	      /* Instructions using or affecting the condition codes make it
		 fail.  */
	      scanbody = PATTERN (this_insn);
	      if (!(GET_CODE (scanbody) == SET
		    || GET_CODE (scanbody) == PARALLEL)
		  || get_attr_conds (this_insn) != CONDS_NOCOND)
		fail = TRUE;
	      break;

	    default:
	      break;
	    }
	}
      if (succeed)
	{
	  if ((!seeking_return) && (arm_ccfsm_state == 1 || reverse))
	    arm_target_label = CODE_LABEL_NUMBER (label);
	  else
	    {
	      gcc_assert (seeking_return || arm_ccfsm_state == 2);

	      while (this_insn && GET_CODE (PATTERN (this_insn)) == USE)
	        {
		  this_insn = next_nonnote_insn (this_insn);
		  gcc_assert (!this_insn
			      || (!BARRIER_P (this_insn)
				  && !LABEL_P (this_insn)));
	        }
	      if (!this_insn)
	        {
		  /* Oh, dear! we ran off the end.. give up.  */
		  extract_constrain_insn_cached (insn);
		  arm_ccfsm_state = 0;
		  arm_target_insn = NULL;
		  return;
	        }
	      arm_target_insn = this_insn;
	    }

	  /* If REVERSE is true, ARM_CURRENT_CC needs to be inverted from
	     what it was.  */
	  if (!reverse)
	    arm_current_cc = get_arm_condition_code (XEXP (SET_SRC (body), 0));

	  if (reverse || then_not_else)
	    arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	}

      /* Restore recog_data (getting the attributes of other insns can
	 destroy this array, but final.c assumes that it remains intact
	 across this call.  */
      extract_constrain_insn_cached (insn);
    }
}

/* Output IT instructions.  */
void
thumb2_asm_output_opcode (FILE * stream)
{
  char buff[5];
  int n;

  if (arm_condexec_mask)
    {
      for (n = 0; n < arm_condexec_masklen; n++)
	buff[n] = (arm_condexec_mask & (1 << n)) ? 't' : 'e';
      buff[n] = 0;
      asm_fprintf(stream, "i%s\t%s\n\t", buff,
		  arm_condition_codes[arm_current_cc]);
      arm_condexec_mask = 0;
    }
}

/* Implement TARGET_HARD_REGNO_NREGS.  On the ARM core regs are
   UNITS_PER_WORD bytes wide.  */
static unsigned int
arm_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  if (TARGET_32BIT
      && regno > PC_REGNUM
      && regno != FRAME_POINTER_REGNUM
      && regno != ARG_POINTER_REGNUM
      && !IS_VFP_REGNUM (regno))
    return 1;

  return ARM_NUM_REGS (mode);
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */
static bool
arm_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return (regno == CC_REGNUM
	    || (TARGET_HARD_FLOAT
		&& regno == VFPCC_REGNUM));

  if (regno == CC_REGNUM && GET_MODE_CLASS (mode) != MODE_CC)
    return false;

  if (TARGET_THUMB1)
    /* For the Thumb we only allow values bigger than SImode in
       registers 0 - 6, so that there is always a second low
       register available to hold the upper part of the value.
       We probably we ought to ensure that the register is the
       start of an even numbered register pair.  */
    return (ARM_NUM_REGS (mode) < 2) || (regno < LAST_LO_REGNUM);

  if (TARGET_HARD_FLOAT && IS_VFP_REGNUM (regno))
    {
      if (mode == SFmode || mode == SImode)
	return VFP_REGNO_OK_FOR_SINGLE (regno);

      if (mode == DFmode)
	return VFP_REGNO_OK_FOR_DOUBLE (regno);

      if (mode == HFmode)
	return VFP_REGNO_OK_FOR_SINGLE (regno);

      /* VFP registers can hold HImode values.  */
      if (mode == HImode)
	return VFP_REGNO_OK_FOR_SINGLE (regno);

      if (TARGET_NEON)
        return (VALID_NEON_DREG_MODE (mode) && VFP_REGNO_OK_FOR_DOUBLE (regno))
               || (VALID_NEON_QREG_MODE (mode)
                   && NEON_REGNO_OK_FOR_QUAD (regno))
	       || (mode == TImode && NEON_REGNO_OK_FOR_NREGS (regno, 2))
	       || (mode == EImode && NEON_REGNO_OK_FOR_NREGS (regno, 3))
	       || (mode == OImode && NEON_REGNO_OK_FOR_NREGS (regno, 4))
	       || (mode == CImode && NEON_REGNO_OK_FOR_NREGS (regno, 6))
	       || (mode == XImode && NEON_REGNO_OK_FOR_NREGS (regno, 8));

      return false;
    }

  if (TARGET_REALLY_IWMMXT)
    {
      if (IS_IWMMXT_GR_REGNUM (regno))
	return mode == SImode;

      if (IS_IWMMXT_REGNUM (regno))
	return VALID_IWMMXT_REG_MODE (mode);
    }

  /* We allow almost any value to be stored in the general registers.
     Restrict doubleword quantities to even register pairs in ARM state
     so that we can use ldrd.  Do not allow very large Neon structure
     opaque modes in general registers; they would use too many.  */
  if (regno <= LAST_ARM_REGNUM)
    {
      if (ARM_NUM_REGS (mode) > 4)
	return false;

      if (TARGET_THUMB2)
	return true;

      return !(TARGET_LDRD && GET_MODE_SIZE (mode) > 4 && (regno & 1) != 0);
    }

  if (regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    /* We only allow integers in the fake hard registers.  */
    return GET_MODE_CLASS (mode) == MODE_INT;

  return false;
}

/* Implement TARGET_MODES_TIEABLE_P.  */

static bool
arm_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (GET_MODE_CLASS (mode1) == GET_MODE_CLASS (mode2))
    return true;

  /* We specifically want to allow elements of "structure" modes to
     be tieable to the structure.  This more general condition allows
     other rarer situations too.  */
  if (TARGET_NEON
      && (VALID_NEON_DREG_MODE (mode1)
	  || VALID_NEON_QREG_MODE (mode1)
	  || VALID_NEON_STRUCT_MODE (mode1))
      && (VALID_NEON_DREG_MODE (mode2)
	  || VALID_NEON_QREG_MODE (mode2)
	  || VALID_NEON_STRUCT_MODE (mode2)))
    return true;

  return false;
}

/* For efficiency and historical reasons LO_REGS, HI_REGS and CC_REGS are
   not used in arm mode.  */

enum reg_class
arm_regno_class (int regno)
{
  if (regno == PC_REGNUM)
    return NO_REGS;

  if (TARGET_THUMB1)
    {
      if (regno == STACK_POINTER_REGNUM)
	return STACK_REG;
      if (regno == CC_REGNUM)
	return CC_REG;
      if (regno < 8)
	return LO_REGS;
      return HI_REGS;
    }

  if (TARGET_THUMB2 && regno < 8)
    return LO_REGS;

  if (   regno <= LAST_ARM_REGNUM
      || regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    return TARGET_THUMB2 ? HI_REGS : GENERAL_REGS;

  if (regno == CC_REGNUM || regno == VFPCC_REGNUM)
    return TARGET_THUMB2 ? CC_REG : NO_REGS;

  if (IS_VFP_REGNUM (regno))
    {
      if (regno <= D7_VFP_REGNUM)
	return VFP_D0_D7_REGS;
      else if (regno <= LAST_LO_VFP_REGNUM)
        return VFP_LO_REGS;
      else
        return VFP_HI_REGS;
    }

  if (IS_IWMMXT_REGNUM (regno))
    return IWMMXT_REGS;

  if (IS_IWMMXT_GR_REGNUM (regno))
    return IWMMXT_GR_REGS;

  return NO_REGS;
}

/* Handle a special case when computing the offset
   of an argument from the frame pointer.  */
int
arm_debugger_arg_offset (int value, rtx addr)
{
  rtx_insn *insn;

  /* We are only interested if dbxout_parms() failed to compute the offset.  */
  if (value != 0)
    return 0;

  /* We can only cope with the case where the address is held in a register.  */
  if (!REG_P (addr))
    return 0;

  /* If we are using the frame pointer to point at the argument, then
     an offset of 0 is correct.  */
  if (REGNO (addr) == (unsigned) HARD_FRAME_POINTER_REGNUM)
    return 0;

  /* If we are using the stack pointer to point at the
     argument, then an offset of 0 is correct.  */
  /* ??? Check this is consistent with thumb2 frame layout.  */
  if ((TARGET_THUMB || !frame_pointer_needed)
      && REGNO (addr) == SP_REGNUM)
    return 0;

  /* Oh dear.  The argument is pointed to by a register rather
     than being held in a register, or being stored at a known
     offset from the frame pointer.  Since GDB only understands
     those two kinds of argument we must translate the address
     held in the register into an offset from the frame pointer.
     We do this by searching through the insns for the function
     looking to see where this register gets its value.  If the
     register is initialized from the frame pointer plus an offset
     then we are in luck and we can continue, otherwise we give up.

     This code is exercised by producing debugging information
     for a function with arguments like this:

           double func (double a, double b, int c, double d) {return d;}

     Without this code the stab for parameter 'd' will be set to
     an offset of 0 from the frame pointer, rather than 8.  */

  /* The if() statement says:

     If the insn is a normal instruction
     and if the insn is setting the value in a register
     and if the register being set is the register holding the address of the argument
     and if the address is computing by an addition
     that involves adding to a register
     which is the frame pointer
     a constant integer

     then...  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (   NONJUMP_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) == SET
	  && REGNO    (XEXP (PATTERN (insn), 0)) == REGNO (addr)
	  && GET_CODE (XEXP (PATTERN (insn), 1)) == PLUS
	  && REG_P (XEXP (XEXP (PATTERN (insn), 1), 0))
	  && REGNO    (XEXP (XEXP (PATTERN (insn), 1), 0)) == (unsigned) HARD_FRAME_POINTER_REGNUM
	  && CONST_INT_P (XEXP (XEXP (PATTERN (insn), 1), 1))
	     )
	{
	  value = INTVAL (XEXP (XEXP (PATTERN (insn), 1), 1));

	  break;
	}
    }

  if (value == 0)
    {
      debug_rtx (addr);
      warning (0, "unable to compute real location of stacked parameter");
      value = 8; /* XXX magic hack */
    }

  return value;
}

/* Implement TARGET_PROMOTED_TYPE.  */

static tree
arm_promoted_type (const_tree t)
{
  if (SCALAR_FLOAT_TYPE_P (t)
      && TYPE_PRECISION (t) == 16
      && TYPE_MAIN_VARIANT (t) == arm_fp16_type_node)
    return float_type_node;
  return NULL_TREE;
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.
   This simply adds HFmode as a supported mode; even though we don't
   implement arithmetic on this type directly, it's supported by
   optabs conversions, much the way the double-word arithmetic is
   special-cased in the default hook.  */

static bool
arm_scalar_mode_supported_p (scalar_mode mode)
{
  if (mode == HFmode)
    return (arm_fp16_format != ARM_FP16_FORMAT_NONE);
  else if (ALL_FIXED_POINT_MODE_P (mode))
    return true;
  else
    return default_scalar_mode_supported_p (mode);
}

/* Set the value of FLT_EVAL_METHOD.
   ISO/IEC TS 18661-3 defines two values that we'd like to make use of:

    0: evaluate all operations and constants, whose semantic type has at
       most the range and precision of type float, to the range and
       precision of float; evaluate all other operations and constants to
       the range and precision of the semantic type;

    N, where _FloatN is a supported interchange floating type
       evaluate all operations and constants, whose semantic type has at
       most the range and precision of _FloatN type, to the range and
       precision of the _FloatN type; evaluate all other operations and
       constants to the range and precision of the semantic type;

   If we have the ARMv8.2-A extensions then we support _Float16 in native
   precision, so we should set this to 16.  Otherwise, we support the type,
   but want to evaluate expressions in float precision, so set this to
   0.  */

static enum flt_eval_method
arm_excess_precision (enum excess_precision_type type)
{
  switch (type)
    {
      case EXCESS_PRECISION_TYPE_FAST:
      case EXCESS_PRECISION_TYPE_STANDARD:
	/* We can calculate either in 16-bit range and precision or
	   32-bit range and precision.  Make that decision based on whether
	   we have native support for the ARMv8.2-A 16-bit floating-point
	   instructions or not.  */
	return (TARGET_VFP_FP16INST
		? FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16
		: FLT_EVAL_METHOD_PROMOTE_TO_FLOAT);
      case EXCESS_PRECISION_TYPE_IMPLICIT:
	return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16;
      default:
	gcc_unreachable ();
    }
  return FLT_EVAL_METHOD_UNPREDICTABLE;
}


/* Implement TARGET_FLOATN_MODE.  Make very sure that we don't provide
   _Float16 if we are using anything other than ieee format for 16-bit
   floating point.  Otherwise, punt to the default implementation.  */
static opt_scalar_float_mode
arm_floatn_mode (int n, bool extended)
{
  if (!extended && n == 16)
    {
      if (arm_fp16_format == ARM_FP16_FORMAT_IEEE)
	return HFmode;
      return opt_scalar_float_mode ();
    }

  return default_floatn_mode (n, extended);
}


/* Set up OPERANDS for a register copy from SRC to DEST, taking care
   not to early-clobber SRC registers in the process.

   We assume that the operands described by SRC and DEST represent a
   decomposed copy of OPERANDS[1] into OPERANDS[0].  COUNT is the
   number of components into which the copy has been decomposed.  */
void
neon_disambiguate_copy (rtx *operands, rtx *dest, rtx *src, unsigned int count)
{
  unsigned int i;

  if (!reg_overlap_mentioned_p (operands[0], operands[1])
      || REGNO (operands[0]) < REGNO (operands[1]))
    {
      for (i = 0; i < count; i++)
	{
	  operands[2 * i] = dest[i];
	  operands[2 * i + 1] = src[i];
	}
    }
  else
    {
      for (i = 0; i < count; i++)
	{
	  operands[2 * i] = dest[count - i - 1];
	  operands[2 * i + 1] = src[count - i - 1];
	}
    }
}

/* Split operands into moves from op[1] + op[2] into op[0].  */

void
neon_split_vcombine (rtx operands[3])
{
  unsigned int dest = REGNO (operands[0]);
  unsigned int src1 = REGNO (operands[1]);
  unsigned int src2 = REGNO (operands[2]);
  machine_mode halfmode = GET_MODE (operands[1]);
  unsigned int halfregs = REG_NREGS (operands[1]);
  rtx destlo, desthi;

  if (src1 == dest && src2 == dest + halfregs)
    {
      /* No-op move.  Can't split to nothing; emit something.  */
      emit_note (NOTE_INSN_DELETED);
      return;
    }

  /* Preserve register attributes for variable tracking.  */
  destlo = gen_rtx_REG_offset (operands[0], halfmode, dest, 0);
  desthi = gen_rtx_REG_offset (operands[0], halfmode, dest + halfregs,
			       GET_MODE_SIZE (halfmode));

  /* Special case of reversed high/low parts.  Use VSWP.  */
  if (src2 == dest && src1 == dest + halfregs)
    {
      rtx x = gen_rtx_SET (destlo, operands[1]);
      rtx y = gen_rtx_SET (desthi, operands[2]);
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, x, y)));
      return;
    }

  if (!reg_overlap_mentioned_p (operands[2], destlo))
    {
      /* Try to avoid unnecessary moves if part of the result
	 is in the right place already.  */
      if (src1 != dest)
	emit_move_insn (destlo, operands[1]);
      if (src2 != dest + halfregs)
	emit_move_insn (desthi, operands[2]);
    }
  else
    {
      if (src2 != dest + halfregs)
	emit_move_insn (desthi, operands[2]);
      if (src1 != dest)
	emit_move_insn (destlo, operands[1]);
    }
}

/* Return the number (counting from 0) of
   the least significant set bit in MASK.  */

inline static int
number_of_first_bit_set (unsigned mask)
{
  return ctz_hwi (mask);
}

/* Like emit_multi_reg_push, but allowing for a different set of
   registers to be described as saved.  MASK is the set of registers
   to be saved; REAL_REGS is the set of registers to be described as
   saved.  If REAL_REGS is 0, only describe the stack adjustment.  */

static rtx_insn *
thumb1_emit_multi_reg_push (unsigned long mask, unsigned long real_regs)
{
  unsigned long regno;
  rtx par[10], tmp, reg;
  rtx_insn *insn;
  int i, j;

  /* Build the parallel of the registers actually being stored.  */
  for (i = 0; mask; ++i, mask &= mask - 1)
    {
      regno = ctz_hwi (mask);
      reg = gen_rtx_REG (SImode, regno);

      if (i == 0)
	tmp = gen_rtx_UNSPEC (BLKmode, gen_rtvec (1, reg), UNSPEC_PUSH_MULT);
      else
	tmp = gen_rtx_USE (VOIDmode, reg);

      par[i] = tmp;
    }

  tmp = plus_constant (Pmode, stack_pointer_rtx, -4 * i);
  tmp = gen_rtx_PRE_MODIFY (Pmode, stack_pointer_rtx, tmp);
  tmp = gen_frame_mem (BLKmode, tmp);
  tmp = gen_rtx_SET (tmp, par[0]);
  par[0] = tmp;

  tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (i, par));
  insn = emit_insn (tmp);

  /* Always build the stack adjustment note for unwind info.  */
  tmp = plus_constant (Pmode, stack_pointer_rtx, -4 * i);
  tmp = gen_rtx_SET (stack_pointer_rtx, tmp);
  par[0] = tmp;

  /* Build the parallel of the registers recorded as saved for unwind.  */
  for (j = 0; real_regs; ++j, real_regs &= real_regs - 1)
    {
      regno = ctz_hwi (real_regs);
      reg = gen_rtx_REG (SImode, regno);

      tmp = plus_constant (Pmode, stack_pointer_rtx, j * 4);
      tmp = gen_frame_mem (SImode, tmp);
      tmp = gen_rtx_SET (tmp, reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      par[j + 1] = tmp;
    }

  if (j == 0)
    tmp = par[0];
  else
    {
      RTX_FRAME_RELATED_P (par[0]) = 1;
      tmp = gen_rtx_SEQUENCE (VOIDmode, gen_rtvec_v (j + 1, par));
    }

  add_reg_note (insn, REG_FRAME_RELATED_EXPR, tmp);

  return insn;
}

/* Emit code to push or pop registers to or from the stack.  F is the
   assembly file.  MASK is the registers to pop.  */
static void
thumb_pop (FILE *f, unsigned long mask)
{
  int regno;
  int lo_mask = mask & 0xFF;

  gcc_assert (mask);

  if (lo_mask == 0 && (mask & (1 << PC_REGNUM)))
    {
      /* Special case.  Do not generate a POP PC statement here, do it in
	 thumb_exit() */
      thumb_exit (f, -1);
      return;
    }

  fprintf (f, "\tpop\t{");

  /* Look at the low registers first.  */
  for (regno = 0; regno <= LAST_LO_REGNUM; regno++, lo_mask >>= 1)
    {
      if (lo_mask & 1)
	{
	  asm_fprintf (f, "%r", regno);

	  if ((lo_mask & ~1) != 0)
	    fprintf (f, ", ");
	}
    }

  if (mask & (1 << PC_REGNUM))
    {
      /* Catch popping the PC.  */
      if (TARGET_INTERWORK || TARGET_BACKTRACE || crtl->calls_eh_return
	  || IS_CMSE_ENTRY (arm_current_func_type ()))
	{
	  /* The PC is never poped directly, instead
	     it is popped into r3 and then BX is used.  */
	  fprintf (f, "}\n");

	  thumb_exit (f, -1);

	  return;
	}
      else
	{
	  if (mask & 0xFF)
	    fprintf (f, ", ");

	  asm_fprintf (f, "%r", PC_REGNUM);
	}
    }

  fprintf (f, "}\n");
}

/* Generate code to return from a thumb function.
   If 'reg_containing_return_addr' is -1, then the return address is
   actually on the stack, at the stack pointer.

   Note: do not forget to update length attribute of corresponding insn pattern
   when changing assembly output (eg. length attribute of epilogue_insns when
   updating Armv8-M Baseline Security Extensions register clearing
   sequences).  */
static void
thumb_exit (FILE *f, int reg_containing_return_addr)
{
  unsigned regs_available_for_popping;
  unsigned regs_to_pop;
  int pops_needed;
  unsigned available;
  unsigned required;
  machine_mode mode;
  int size;
  int restore_a4 = FALSE;

  /* Compute the registers we need to pop.  */
  regs_to_pop = 0;
  pops_needed = 0;

  if (reg_containing_return_addr == -1)
    {
      regs_to_pop |= 1 << LR_REGNUM;
      ++pops_needed;
    }

  if (TARGET_BACKTRACE)
    {
      /* Restore the (ARM) frame pointer and stack pointer.  */
      regs_to_pop |= (1 << ARM_HARD_FRAME_POINTER_REGNUM) | (1 << SP_REGNUM);
      pops_needed += 2;
    }

  /* If there is nothing to pop then just emit the BX instruction and
     return.  */
  if (pops_needed == 0)
    {
      if (crtl->calls_eh_return)
	asm_fprintf (f, "\tadd\t%r, %r\n", SP_REGNUM, ARM_EH_STACKADJ_REGNUM);

      if (IS_CMSE_ENTRY (arm_current_func_type ()))
	{
	  asm_fprintf (f, "\tmsr\tAPSR_nzcvq, %r\n",
		       reg_containing_return_addr);
	  asm_fprintf (f, "\tbxns\t%r\n", reg_containing_return_addr);
	}
      else
	asm_fprintf (f, "\tbx\t%r\n", reg_containing_return_addr);
      return;
    }
  /* Otherwise if we are not supporting interworking and we have not created
     a backtrace structure and the function was not entered in ARM mode then
     just pop the return address straight into the PC.  */
  else if (!TARGET_INTERWORK
	   && !TARGET_BACKTRACE
	   && !is_called_in_ARM_mode (current_function_decl)
	   && !crtl->calls_eh_return
	   && !IS_CMSE_ENTRY (arm_current_func_type ()))
    {
      asm_fprintf (f, "\tpop\t{%r}\n", PC_REGNUM);
      return;
    }

  /* Find out how many of the (return) argument registers we can corrupt.  */
  regs_available_for_popping = 0;

  /* If returning via __builtin_eh_return, the bottom three registers
     all contain information needed for the return.  */
  if (crtl->calls_eh_return)
    size = 12;
  else
    {
      /* If we can deduce the registers used from the function's
	 return value.  This is more reliable that examining
	 df_regs_ever_live_p () because that will be set if the register is
	 ever used in the function, not just if the register is used
	 to hold a return value.  */

      if (crtl->return_rtx != 0)
	mode = GET_MODE (crtl->return_rtx);
      else
	mode = DECL_MODE (DECL_RESULT (current_function_decl));

      size = GET_MODE_SIZE (mode);

      if (size == 0)
	{
	  /* In a void function we can use any argument register.
	     In a function that returns a structure on the stack
	     we can use the second and third argument registers.  */
	  if (mode == VOIDmode)
	    regs_available_for_popping =
	      (1 << ARG_REGISTER (1))
	      | (1 << ARG_REGISTER (2))
	      | (1 << ARG_REGISTER (3));
	  else
	    regs_available_for_popping =
	      (1 << ARG_REGISTER (2))
	      | (1 << ARG_REGISTER (3));
	}
      else if (size <= 4)
	regs_available_for_popping =
	  (1 << ARG_REGISTER (2))
	  | (1 << ARG_REGISTER (3));
      else if (size <= 8)
	regs_available_for_popping =
	  (1 << ARG_REGISTER (3));
    }

  /* Match registers to be popped with registers into which we pop them.  */
  for (available = regs_available_for_popping,
       required  = regs_to_pop;
       required != 0 && available != 0;
       available &= ~(available & - available),
       required  &= ~(required  & - required))
    -- pops_needed;

  /* If we have any popping registers left over, remove them.  */
  if (available > 0)
    regs_available_for_popping &= ~available;

  /* Otherwise if we need another popping register we can use
     the fourth argument register.  */
  else if (pops_needed)
    {
      /* If we have not found any free argument registers and
	 reg a4 contains the return address, we must move it.  */
      if (regs_available_for_popping == 0
	  && reg_containing_return_addr == LAST_ARG_REGNUM)
	{
	  asm_fprintf (f, "\tmov\t%r, %r\n", LR_REGNUM, LAST_ARG_REGNUM);
	  reg_containing_return_addr = LR_REGNUM;
	}
      else if (size > 12)
	{
	  /* Register a4 is being used to hold part of the return value,
	     but we have dire need of a free, low register.  */
	  restore_a4 = TRUE;

	  asm_fprintf (f, "\tmov\t%r, %r\n",IP_REGNUM, LAST_ARG_REGNUM);
	}

      if (reg_containing_return_addr != LAST_ARG_REGNUM)
	{
	  /* The fourth argument register is available.  */
	  regs_available_for_popping |= 1 << LAST_ARG_REGNUM;

	  --pops_needed;
	}
    }

  /* Pop as many registers as we can.  */
  thumb_pop (f, regs_available_for_popping);

  /* Process the registers we popped.  */
  if (reg_containing_return_addr == -1)
    {
      /* The return address was popped into the lowest numbered register.  */
      regs_to_pop &= ~(1 << LR_REGNUM);

      reg_containing_return_addr =
	number_of_first_bit_set (regs_available_for_popping);

      /* Remove this register for the mask of available registers, so that
         the return address will not be corrupted by further pops.  */
      regs_available_for_popping &= ~(1 << reg_containing_return_addr);
    }

  /* If we popped other registers then handle them here.  */
  if (regs_available_for_popping)
    {
      int frame_pointer;

      /* Work out which register currently contains the frame pointer.  */
      frame_pointer = number_of_first_bit_set (regs_available_for_popping);

      /* Move it into the correct place.  */
      asm_fprintf (f, "\tmov\t%r, %r\n",
		   ARM_HARD_FRAME_POINTER_REGNUM, frame_pointer);

      /* (Temporarily) remove it from the mask of popped registers.  */
      regs_available_for_popping &= ~(1 << frame_pointer);
      regs_to_pop &= ~(1 << ARM_HARD_FRAME_POINTER_REGNUM);

      if (regs_available_for_popping)
	{
	  int stack_pointer;

	  /* We popped the stack pointer as well,
	     find the register that contains it.  */
	  stack_pointer = number_of_first_bit_set (regs_available_for_popping);

	  /* Move it into the stack register.  */
	  asm_fprintf (f, "\tmov\t%r, %r\n", SP_REGNUM, stack_pointer);

	  /* At this point we have popped all necessary registers, so
	     do not worry about restoring regs_available_for_popping
	     to its correct value:

	     assert (pops_needed == 0)
	     assert (regs_available_for_popping == (1 << frame_pointer))
	     assert (regs_to_pop == (1 << STACK_POINTER))  */
	}
      else
	{
	  /* Since we have just move the popped value into the frame
	     pointer, the popping register is available for reuse, and
	     we know that we still have the stack pointer left to pop.  */
	  regs_available_for_popping |= (1 << frame_pointer);
	}
    }

  /* If we still have registers left on the stack, but we no longer have
     any registers into which we can pop them, then we must move the return
     address into the link register and make available the register that
     contained it.  */
  if (regs_available_for_popping == 0 && pops_needed > 0)
    {
      regs_available_for_popping |= 1 << reg_containing_return_addr;

      asm_fprintf (f, "\tmov\t%r, %r\n", LR_REGNUM,
		   reg_containing_return_addr);

      reg_containing_return_addr = LR_REGNUM;
    }

  /* If we have registers left on the stack then pop some more.
     We know that at most we will want to pop FP and SP.  */
  if (pops_needed > 0)
    {
      int  popped_into;
      int  move_to;

      thumb_pop (f, regs_available_for_popping);

      /* We have popped either FP or SP.
	 Move whichever one it is into the correct register.  */
      popped_into = number_of_first_bit_set (regs_available_for_popping);
      move_to     = number_of_first_bit_set (regs_to_pop);

      asm_fprintf (f, "\tmov\t%r, %r\n", move_to, popped_into);
      --pops_needed;
    }

  /* If we still have not popped everything then we must have only
     had one register available to us and we are now popping the SP.  */
  if (pops_needed > 0)
    {
      int  popped_into;

      thumb_pop (f, regs_available_for_popping);

      popped_into = number_of_first_bit_set (regs_available_for_popping);

      asm_fprintf (f, "\tmov\t%r, %r\n", SP_REGNUM, popped_into);
      /*
	assert (regs_to_pop == (1 << STACK_POINTER))
	assert (pops_needed == 1)
      */
    }

  /* If necessary restore the a4 register.  */
  if (restore_a4)
    {
      if (reg_containing_return_addr != LR_REGNUM)
	{
	  asm_fprintf (f, "\tmov\t%r, %r\n", LR_REGNUM, LAST_ARG_REGNUM);
	  reg_containing_return_addr = LR_REGNUM;
	}

      asm_fprintf (f, "\tmov\t%r, %r\n", LAST_ARG_REGNUM, IP_REGNUM);
    }

  if (crtl->calls_eh_return)
    asm_fprintf (f, "\tadd\t%r, %r\n", SP_REGNUM, ARM_EH_STACKADJ_REGNUM);

  /* Return to caller.  */
  if (IS_CMSE_ENTRY (arm_current_func_type ()))
    {
      /* This is for the cases where LR is not being used to contain the return
         address.  It may therefore contain information that we might not want
	 to leak, hence it must be cleared.  The value in R0 will never be a
	 secret at this point, so it is safe to use it, see the clearing code
	 in 'cmse_nonsecure_entry_clear_before_return'.  */
      if (reg_containing_return_addr != LR_REGNUM)
	asm_fprintf (f, "\tmov\tlr, r0\n");

      asm_fprintf (f, "\tmsr\tAPSR_nzcvq, %r\n", reg_containing_return_addr);
      asm_fprintf (f, "\tbxns\t%r\n", reg_containing_return_addr);
    }
  else
    asm_fprintf (f, "\tbx\t%r\n", reg_containing_return_addr);
}

/* Scan INSN just before assembler is output for it.
   For Thumb-1, we track the status of the condition codes; this
   information is used in the cbranchsi4_insn pattern.  */
void
thumb1_final_prescan_insn (rtx_insn *insn)
{
  if (flag_print_asm_name)
    asm_fprintf (asm_out_file, "%@ 0x%04x\n",
		 INSN_ADDRESSES (INSN_UID (insn)));
  /* Don't overwrite the previous setter when we get to a cbranch.  */
  if (INSN_CODE (insn) != CODE_FOR_cbranchsi4_insn)
    {
      enum attr_conds conds;

      if (cfun->machine->thumb1_cc_insn)
	{
	  if (modified_in_p (cfun->machine->thumb1_cc_op0, insn)
	      || modified_in_p (cfun->machine->thumb1_cc_op1, insn))
	    CC_STATUS_INIT;
	}
      conds = get_attr_conds (insn);
      if (conds == CONDS_SET)
	{
	  rtx set = single_set (insn);
	  cfun->machine->thumb1_cc_insn = insn;
	  cfun->machine->thumb1_cc_op0 = SET_DEST (set);
	  cfun->machine->thumb1_cc_op1 = const0_rtx;
	  cfun->machine->thumb1_cc_mode = CC_NOOVmode;
	  if (INSN_CODE (insn) == CODE_FOR_thumb1_subsi3_insn)
	    {
	      rtx src1 = XEXP (SET_SRC (set), 1);
	      if (src1 == const0_rtx)
		cfun->machine->thumb1_cc_mode = CCmode;
	    }
	  else if (REG_P (SET_DEST (set)) && REG_P (SET_SRC (set)))
	    {
	      /* Record the src register operand instead of dest because
		 cprop_hardreg pass propagates src.  */
	      cfun->machine->thumb1_cc_op0 = SET_SRC (set);
	    }
	}
      else if (conds != CONDS_NOCOND)
	cfun->machine->thumb1_cc_insn = NULL_RTX;
    }

    /* Check if unexpected far jump is used.  */
    if (cfun->machine->lr_save_eliminated
        && get_attr_far_jump (insn) == FAR_JUMP_YES)
      internal_error("Unexpected thumb1 far jump");
}

int
thumb_shiftable_const (unsigned HOST_WIDE_INT val)
{
  unsigned HOST_WIDE_INT mask = 0xff;
  int i;

  val = val & (unsigned HOST_WIDE_INT)0xffffffffu;
  if (val == 0) /* XXX */
    return 0;

  for (i = 0; i < 25; i++)
    if ((val & (mask << i)) == val)
      return 1;

  return 0;
}

/* Returns nonzero if the current function contains,
   or might contain a far jump.  */
static int
thumb_far_jump_used_p (void)
{
  rtx_insn *insn;
  bool far_jump = false;
  unsigned int func_size = 0;

  /* If we have already decided that far jumps may be used,
     do not bother checking again, and always return true even if
     it turns out that they are not being used.  Once we have made
     the decision that far jumps are present (and that hence the link
     register will be pushed onto the stack) we cannot go back on it.  */
  if (cfun->machine->far_jump_used)
    return 1;

  /* If this function is not being called from the prologue/epilogue
     generation code then it must be being called from the
     INITIAL_ELIMINATION_OFFSET macro.  */
  if (!(ARM_DOUBLEWORD_ALIGN || reload_completed))
    {
      /* In this case we know that we are being asked about the elimination
	 of the arg pointer register.  If that register is not being used,
	 then there are no arguments on the stack, and we do not have to
	 worry that a far jump might force the prologue to push the link
	 register, changing the stack offsets.  In this case we can just
	 return false, since the presence of far jumps in the function will
	 not affect stack offsets.

	 If the arg pointer is live (or if it was live, but has now been
	 eliminated and so set to dead) then we do have to test to see if
	 the function might contain a far jump.  This test can lead to some
	 false negatives, since before reload is completed, then length of
	 branch instructions is not known, so gcc defaults to returning their
	 longest length, which in turn sets the far jump attribute to true.

	 A false negative will not result in bad code being generated, but it
	 will result in a needless push and pop of the link register.  We
	 hope that this does not occur too often.

	 If we need doubleword stack alignment this could affect the other
	 elimination offsets so we can't risk getting it wrong.  */
      if (df_regs_ever_live_p (ARG_POINTER_REGNUM))
	cfun->machine->arg_pointer_live = 1;
      else if (!cfun->machine->arg_pointer_live)
	return 0;
    }

  /* We should not change far_jump_used during or after reload, as there is
     no chance to change stack frame layout.  */
  if (reload_in_progress || reload_completed)
    return 0;

  /* Check to see if the function contains a branch
     insn with the far jump attribute set.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (JUMP_P (insn) && get_attr_far_jump (insn) == FAR_JUMP_YES)
	{
	  far_jump = true;
	}
      func_size += get_attr_length (insn);
    }

  /* Attribute far_jump will always be true for thumb1 before
     shorten_branch pass.  So checking far_jump attribute before
     shorten_branch isn't much useful.

     Following heuristic tries to estimate more accurately if a far jump
     may finally be used.  The heuristic is very conservative as there is
     no chance to roll-back the decision of not to use far jump.

     Thumb1 long branch offset is -2048 to 2046.  The worst case is each
     2-byte insn is associated with a 4 byte constant pool.  Using
     function size 2048/3 as the threshold is conservative enough.  */
  if (far_jump)
    {
      if ((func_size * 3) >= 2048)
        {
	  /* Record the fact that we have decided that
	     the function does use far jumps.  */
	  cfun->machine->far_jump_used = 1;
	  return 1;
	}
    }

  return 0;
}

/* Return nonzero if FUNC must be entered in ARM mode.  */
static bool
is_called_in_ARM_mode (tree func)
{
  gcc_assert (TREE_CODE (func) == FUNCTION_DECL);

  /* Ignore the problem about functions whose address is taken.  */
  if (TARGET_CALLEE_INTERWORKING && TREE_PUBLIC (func))
    return true;

#ifdef ARM_PE
  return lookup_attribute ("interfacearm", DECL_ATTRIBUTES (func)) != NULL_TREE;
#else
  return false;
#endif
}

/* Given the stack offsets and register mask in OFFSETS, decide how
   many additional registers to push instead of subtracting a constant
   from SP.  For epilogues the principle is the same except we use pop.
   FOR_PROLOGUE indicates which we're generating.  */
static int
thumb1_extra_regs_pushed (arm_stack_offsets *offsets, bool for_prologue)
{
  HOST_WIDE_INT amount;
  unsigned long live_regs_mask = offsets->saved_regs_mask;
  /* Extract a mask of the ones we can give to the Thumb's push/pop
     instruction.  */
  unsigned long l_mask = live_regs_mask & (for_prologue ? 0x40ff : 0xff);
  /* Then count how many other high registers will need to be pushed.  */
  unsigned long high_regs_pushed = bit_count (live_regs_mask & 0x0f00);
  int n_free, reg_base, size;

  if (!for_prologue && frame_pointer_needed)
    amount = offsets->locals_base - offsets->saved_regs;
  else
    amount = offsets->outgoing_args - offsets->saved_regs;

  /* If the stack frame size is 512 exactly, we can save one load
     instruction, which should make this a win even when optimizing
     for speed.  */
  if (!optimize_size && amount != 512)
    return 0;

  /* Can't do this if there are high registers to push.  */
  if (high_regs_pushed != 0)
    return 0;

  /* Shouldn't do it in the prologue if no registers would normally
     be pushed at all.  In the epilogue, also allow it if we'll have
     a pop insn for the PC.  */
  if  (l_mask == 0
       && (for_prologue
	   || TARGET_BACKTRACE
	   || (live_regs_mask & 1 << LR_REGNUM) == 0
	   || TARGET_INTERWORK
	   || crtl->args.pretend_args_size != 0))
    return 0;

  /* Don't do this if thumb_expand_prologue wants to emit instructions
     between the push and the stack frame allocation.  */
  if (for_prologue
      && ((flag_pic && arm_pic_register != INVALID_REGNUM)
	  || (!frame_pointer_needed && CALLER_INTERWORKING_SLOT_SIZE > 0)))
    return 0;

  reg_base = 0;
  n_free = 0;
  if (!for_prologue)
    {
      size = arm_size_return_regs ();
      reg_base = ARM_NUM_INTS (size);
      live_regs_mask >>= reg_base;
    }

  while (reg_base + n_free < 8 && !(live_regs_mask & 1)
	 && (for_prologue || call_used_regs[reg_base + n_free]))
    {
      live_regs_mask >>= 1;
      n_free++;
    }

  if (n_free == 0)
    return 0;
  gcc_assert (amount / 4 * 4 == amount);

  if (amount >= 512 && (amount - n_free * 4) < 512)
    return (amount - 508) / 4;
  if (amount <= n_free * 4)
    return amount / 4;
  return 0;
}

/* The bits which aren't usefully expanded as rtl.  */
const char *
thumb1_unexpanded_epilogue (void)
{
  arm_stack_offsets *offsets;
  int regno;
  unsigned long live_regs_mask = 0;
  int high_regs_pushed = 0;
  int extra_pop;
  int had_to_push_lr;
  int size;

  if (cfun->machine->return_used_this_function != 0)
    return "";

  if (IS_NAKED (arm_current_func_type ()))
    return "";

  offsets = arm_get_frame_offsets ();
  live_regs_mask = offsets->saved_regs_mask;
  high_regs_pushed = bit_count (live_regs_mask & 0x0f00);

  /* If we can deduce the registers used from the function's return value.
     This is more reliable that examining df_regs_ever_live_p () because that
     will be set if the register is ever used in the function, not just if
     the register is used to hold a return value.  */
  size = arm_size_return_regs ();

  extra_pop = thumb1_extra_regs_pushed (offsets, false);
  if (extra_pop > 0)
    {
      unsigned long extra_mask = (1 << extra_pop) - 1;
      live_regs_mask |= extra_mask << ARM_NUM_INTS (size);
    }

  /* The prolog may have pushed some high registers to use as
     work registers.  e.g. the testsuite file:
     gcc/testsuite/gcc/gcc.c-torture/execute/complex-2.c
     compiles to produce:
	push	{r4, r5, r6, r7, lr}
	mov	r7, r9
	mov	r6, r8
	push	{r6, r7}
     as part of the prolog.  We have to undo that pushing here.  */

  if (high_regs_pushed)
    {
      unsigned long mask = live_regs_mask & 0xff;
      int next_hi_reg;

      /* The available low registers depend on the size of the value we are
         returning.  */
      if (size <= 12)
	mask |=  1 << 3;
      if (size <= 8)
	mask |= 1 << 2;

      if (mask == 0)
	/* Oh dear!  We have no low registers into which we can pop
           high registers!  */
	internal_error
	  ("no low registers available for popping high registers");

      for (next_hi_reg = 8; next_hi_reg < 13; next_hi_reg++)
	if (live_regs_mask & (1 << next_hi_reg))
	  break;

      while (high_regs_pushed)
	{
	  /* Find lo register(s) into which the high register(s) can
             be popped.  */
	  for (regno = 0; regno <= LAST_LO_REGNUM; regno++)
	    {
	      if (mask & (1 << regno))
		high_regs_pushed--;
	      if (high_regs_pushed == 0)
		break;
	    }

	  mask &= (2 << regno) - 1;	/* A noop if regno == 8 */

	  /* Pop the values into the low register(s).  */
	  thumb_pop (asm_out_file, mask);

	  /* Move the value(s) into the high registers.  */
	  for (regno = 0; regno <= LAST_LO_REGNUM; regno++)
	    {
	      if (mask & (1 << regno))
		{
		  asm_fprintf (asm_out_file, "\tmov\t%r, %r\n", next_hi_reg,
			       regno);

		  for (next_hi_reg++; next_hi_reg < 13; next_hi_reg++)
		    if (live_regs_mask & (1 << next_hi_reg))
		      break;
		}
	    }
	}
      live_regs_mask &= ~0x0f00;
    }

  had_to_push_lr = (live_regs_mask & (1 << LR_REGNUM)) != 0;
  live_regs_mask &= 0xff;

  if (crtl->args.pretend_args_size == 0 || TARGET_BACKTRACE)
    {
      /* Pop the return address into the PC.  */
      if (had_to_push_lr)
	live_regs_mask |= 1 << PC_REGNUM;

      /* Either no argument registers were pushed or a backtrace
	 structure was created which includes an adjusted stack
	 pointer, so just pop everything.  */
      if (live_regs_mask)
	thumb_pop (asm_out_file, live_regs_mask);

      /* We have either just popped the return address into the
	 PC or it is was kept in LR for the entire function.
	 Note that thumb_pop has already called thumb_exit if the
	 PC was in the list.  */
      if (!had_to_push_lr)
	thumb_exit (asm_out_file, LR_REGNUM);
    }
  else
    {
      /* Pop everything but the return address.  */
      if (live_regs_mask)
	thumb_pop (asm_out_file, live_regs_mask);

      if (had_to_push_lr)
	{
	  if (size > 12)
	    {
	      /* We have no free low regs, so save one.  */
	      asm_fprintf (asm_out_file, "\tmov\t%r, %r\n", IP_REGNUM,
			   LAST_ARG_REGNUM);
	    }

	  /* Get the return address into a temporary register.  */
	  thumb_pop (asm_out_file, 1 << LAST_ARG_REGNUM);

	  if (size > 12)
	    {
	      /* Move the return address to lr.  */
	      asm_fprintf (asm_out_file, "\tmov\t%r, %r\n", LR_REGNUM,
			   LAST_ARG_REGNUM);
	      /* Restore the low register.  */
	      asm_fprintf (asm_out_file, "\tmov\t%r, %r\n", LAST_ARG_REGNUM,
			   IP_REGNUM);
	      regno = LR_REGNUM;
	    }
	  else
	    regno = LAST_ARG_REGNUM;
	}
      else
	regno = LR_REGNUM;

      /* Remove the argument registers that were pushed onto the stack.  */
      asm_fprintf (asm_out_file, "\tadd\t%r, %r, #%d\n",
		   SP_REGNUM, SP_REGNUM,
		   crtl->args.pretend_args_size);

      thumb_exit (asm_out_file, regno);
    }

  return "";
}

/* Functions to save and restore machine-specific function data.  */
static struct machine_function *
arm_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_cleared_alloc<machine_function> ();

#if ARM_FT_UNKNOWN != 0
  machine->func_type = ARM_FT_UNKNOWN;
#endif
  machine->static_chain_stack_bytes = -1;
  return machine;
}

/* Return an RTX indicating where the return address to the
   calling function can be found.  */
rtx
arm_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return NULL_RTX;

  return get_hard_reg_initial_val (Pmode, LR_REGNUM);
}

/* Do anything needed before RTL is emitted for each function.  */
void
arm_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function status.  */
  init_machine_status = arm_init_machine_status;

  /* This is to stop the combine pass optimizing away the alignment
     adjustment of va_arg.  */
  /* ??? It is claimed that this should not be necessary.  */
  if (cfun)
    mark_reg_pointer (arg_pointer_rtx, PARM_BOUNDARY);
}

/* Check that FUNC is called with a different mode.  */

bool
arm_change_mode_p (tree func)
{
  if (TREE_CODE (func) != FUNCTION_DECL)
    return false;

  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (func);

  if (!callee_tree)
    callee_tree = target_option_default_node;

  struct cl_target_option *callee_opts = TREE_TARGET_OPTION (callee_tree);
  int flags = callee_opts->x_target_flags;

  return (TARGET_THUMB_P (flags) != TARGET_THUMB);
}

/* Like arm_compute_initial_elimination offset.  Simpler because there
   isn't an ABI specified frame pointer for Thumb.  Instead, we set it
   to point at the base of the local variables after static stack
   space for a function has been allocated.  */

HOST_WIDE_INT
thumb_compute_initial_elimination_offset (unsigned int from, unsigned int to)
{
  arm_stack_offsets *offsets;

  offsets = arm_get_frame_offsets ();

  switch (from)
    {
    case ARG_POINTER_REGNUM:
      switch (to)
	{
	case STACK_POINTER_REGNUM:
	  return offsets->outgoing_args - offsets->saved_args;

	case FRAME_POINTER_REGNUM:
	  return offsets->soft_frame - offsets->saved_args;

	case ARM_HARD_FRAME_POINTER_REGNUM:
	  return offsets->saved_regs - offsets->saved_args;

	case THUMB_HARD_FRAME_POINTER_REGNUM:
	  return offsets->locals_base - offsets->saved_args;

	default:
	  gcc_unreachable ();
	}
      break;

    case FRAME_POINTER_REGNUM:
      switch (to)
	{
	case STACK_POINTER_REGNUM:
	  return offsets->outgoing_args - offsets->soft_frame;

	case ARM_HARD_FRAME_POINTER_REGNUM:
	  return offsets->saved_regs - offsets->soft_frame;

	case THUMB_HARD_FRAME_POINTER_REGNUM:
	  return offsets->locals_base - offsets->soft_frame;

	default:
	  gcc_unreachable ();
	}
      break;

    default:
      gcc_unreachable ();
    }
}

/* Generate the function's prologue.  */

void
thumb1_expand_prologue (void)
{
  rtx_insn *insn;

  HOST_WIDE_INT amount;
  HOST_WIDE_INT size;
  arm_stack_offsets *offsets;
  unsigned long func_type;
  int regno;
  unsigned long live_regs_mask;
  unsigned long l_mask;
  unsigned high_regs_pushed = 0;
  bool lr_needs_saving;

  func_type = arm_current_func_type ();

  /* Naked functions don't have prologues.  */
  if (IS_NAKED (func_type))
    {
      if (flag_stack_usage_info)
	current_function_static_stack_size = 0;
      return;
    }

  if (IS_INTERRUPT (func_type))
    {
      error ("interrupt Service Routines cannot be coded in Thumb mode");
      return;
    }

  if (is_called_in_ARM_mode (current_function_decl))
    emit_insn (gen_prologue_thumb1_interwork ());

  offsets = arm_get_frame_offsets ();
  live_regs_mask = offsets->saved_regs_mask;
  lr_needs_saving = live_regs_mask & (1 << LR_REGNUM);

  /* Extract a mask of the ones we can give to the Thumb's push instruction.  */
  l_mask = live_regs_mask & 0x40ff;
  /* Then count how many other high registers will need to be pushed.  */
  high_regs_pushed = bit_count (live_regs_mask & 0x0f00);

  if (crtl->args.pretend_args_size)
    {
      rtx x = GEN_INT (-crtl->args.pretend_args_size);

      if (cfun->machine->uses_anonymous_args)
	{
	  int num_pushes = ARM_NUM_INTS (crtl->args.pretend_args_size);
	  unsigned long mask;

	  mask = 1ul << (LAST_ARG_REGNUM + 1);
	  mask -= 1ul << (LAST_ARG_REGNUM + 1 - num_pushes);

	  insn = thumb1_emit_multi_reg_push (mask, 0);
	}
      else
	{
	  insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
					stack_pointer_rtx, x));
	}
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (TARGET_BACKTRACE)
    {
      HOST_WIDE_INT offset = 0;
      unsigned work_register;
      rtx work_reg, x, arm_hfp_rtx;

      /* We have been asked to create a stack backtrace structure.
         The code looks like this:

	 0   .align 2
	 0   func:
         0     sub   SP, #16         Reserve space for 4 registers.
	 2     push  {R7}            Push low registers.
         4     add   R7, SP, #20     Get the stack pointer before the push.
         6     str   R7, [SP, #8]    Store the stack pointer
					(before reserving the space).
         8     mov   R7, PC          Get hold of the start of this code + 12.
        10     str   R7, [SP, #16]   Store it.
        12     mov   R7, FP          Get hold of the current frame pointer.
        14     str   R7, [SP, #4]    Store it.
        16     mov   R7, LR          Get hold of the current return address.
        18     str   R7, [SP, #12]   Store it.
        20     add   R7, SP, #16     Point at the start of the
					backtrace structure.
        22     mov   FP, R7          Put this value into the frame pointer.  */

      work_register = thumb_find_work_register (live_regs_mask);
      work_reg = gen_rtx_REG (SImode, work_register);
      arm_hfp_rtx = gen_rtx_REG (SImode, ARM_HARD_FRAME_POINTER_REGNUM);

      insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
				    stack_pointer_rtx, GEN_INT (-16)));
      RTX_FRAME_RELATED_P (insn) = 1;

      if (l_mask)
	{
	  insn = thumb1_emit_multi_reg_push (l_mask, l_mask);
	  RTX_FRAME_RELATED_P (insn) = 1;
	  lr_needs_saving = false;

	  offset = bit_count (l_mask) * UNITS_PER_WORD;
	}

      x = GEN_INT (offset + 16 + crtl->args.pretend_args_size);
      emit_insn (gen_addsi3 (work_reg, stack_pointer_rtx, x));

      x = plus_constant (Pmode, stack_pointer_rtx, offset + 4);
      x = gen_frame_mem (SImode, x);
      emit_move_insn (x, work_reg);

      /* Make sure that the instruction fetching the PC is in the right place
	 to calculate "start of backtrace creation code + 12".  */
      /* ??? The stores using the common WORK_REG ought to be enough to
	 prevent the scheduler from doing anything weird.  Failing that
	 we could always move all of the following into an UNSPEC_VOLATILE.  */
      if (l_mask)
	{
	  x = gen_rtx_REG (SImode, PC_REGNUM);
	  emit_move_insn (work_reg, x);

	  x = plus_constant (Pmode, stack_pointer_rtx, offset + 12);
	  x = gen_frame_mem (SImode, x);
	  emit_move_insn (x, work_reg);

	  emit_move_insn (work_reg, arm_hfp_rtx);

	  x = plus_constant (Pmode, stack_pointer_rtx, offset);
	  x = gen_frame_mem (SImode, x);
	  emit_move_insn (x, work_reg);
	}
      else
	{
	  emit_move_insn (work_reg, arm_hfp_rtx);

	  x = plus_constant (Pmode, stack_pointer_rtx, offset);
	  x = gen_frame_mem (SImode, x);
	  emit_move_insn (x, work_reg);

	  x = gen_rtx_REG (SImode, PC_REGNUM);
	  emit_move_insn (work_reg, x);

	  x = plus_constant (Pmode, stack_pointer_rtx, offset + 12);
	  x = gen_frame_mem (SImode, x);
	  emit_move_insn (x, work_reg);
	}

      x = gen_rtx_REG (SImode, LR_REGNUM);
      emit_move_insn (work_reg, x);

      x = plus_constant (Pmode, stack_pointer_rtx, offset + 8);
      x = gen_frame_mem (SImode, x);
      emit_move_insn (x, work_reg);

      x = GEN_INT (offset + 12);
      emit_insn (gen_addsi3 (work_reg, stack_pointer_rtx, x));

      emit_move_insn (arm_hfp_rtx, work_reg);
    }
  /* Optimization:  If we are not pushing any low registers but we are going
     to push some high registers then delay our first push.  This will just
     be a push of LR and we can combine it with the push of the first high
     register.  */
  else if ((l_mask & 0xff) != 0
	   || (high_regs_pushed == 0 && lr_needs_saving))
    {
      unsigned long mask = l_mask;
      mask |= (1 << thumb1_extra_regs_pushed (offsets, true)) - 1;
      insn = thumb1_emit_multi_reg_push (mask, mask);
      RTX_FRAME_RELATED_P (insn) = 1;
      lr_needs_saving = false;
    }

  if (high_regs_pushed)
    {
      unsigned pushable_regs;
      unsigned next_hi_reg;
      unsigned arg_regs_num = TARGET_AAPCS_BASED ? crtl->args.info.aapcs_ncrn
						 : crtl->args.info.nregs;
      unsigned arg_regs_mask = (1 << arg_regs_num) - 1;

      for (next_hi_reg = 12; next_hi_reg > LAST_LO_REGNUM; next_hi_reg--)
	if (live_regs_mask & (1 << next_hi_reg))
	  break;

      /* Here we need to mask out registers used for passing arguments
	 even if they can be pushed.  This is to avoid using them to stash the high
	 registers.  Such kind of stash may clobber the use of arguments.  */
      pushable_regs = l_mask & (~arg_regs_mask);
      if (lr_needs_saving)
	pushable_regs &= ~(1 << LR_REGNUM);

      if (pushable_regs == 0)
	pushable_regs = 1 << thumb_find_work_register (live_regs_mask);

      while (high_regs_pushed > 0)
	{
	  unsigned long real_regs_mask = 0;
	  unsigned long push_mask = 0;

	  for (regno = LR_REGNUM; regno >= 0; regno --)
	    {
	      if (pushable_regs & (1 << regno))
		{
		  emit_move_insn (gen_rtx_REG (SImode, regno),
				  gen_rtx_REG (SImode, next_hi_reg));

		  high_regs_pushed --;
		  real_regs_mask |= (1 << next_hi_reg);
		  push_mask |= (1 << regno);

		  if (high_regs_pushed)
		    {
		      for (next_hi_reg --; next_hi_reg > LAST_LO_REGNUM;
			   next_hi_reg --)
			if (live_regs_mask & (1 << next_hi_reg))
			  break;
		    }
		  else
		    break;
		}
	    }

	  /* If we had to find a work register and we have not yet
	     saved the LR then add it to the list of regs to push.  */
	  if (lr_needs_saving)
	    {
	      push_mask |= 1 << LR_REGNUM;
	      real_regs_mask |= 1 << LR_REGNUM;
	      lr_needs_saving = false;
	    }

	  insn = thumb1_emit_multi_reg_push (push_mask, real_regs_mask);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  /* Load the pic register before setting the frame pointer,
     so we can use r7 as a temporary work register.  */
  if (flag_pic && arm_pic_register != INVALID_REGNUM)
    arm_load_pic_register (live_regs_mask);

  if (!frame_pointer_needed && CALLER_INTERWORKING_SLOT_SIZE > 0)
    emit_move_insn (gen_rtx_REG (Pmode, ARM_HARD_FRAME_POINTER_REGNUM),
		    stack_pointer_rtx);

  size = offsets->outgoing_args - offsets->saved_args;
  if (flag_stack_usage_info)
    current_function_static_stack_size = size;

  /* If we have a frame, then do stack checking.  FIXME: not implemented.  */
  if ((flag_stack_check == STATIC_BUILTIN_STACK_CHECK
       || flag_stack_clash_protection)
      && size)
    sorry ("-fstack-check=specific for Thumb-1");

  amount = offsets->outgoing_args - offsets->saved_regs;
  amount -= 4 * thumb1_extra_regs_pushed (offsets, true);
  if (amount)
    {
      if (amount < 512)
	{
	  insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
					GEN_INT (- amount)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      else
	{
	  rtx reg, dwarf;

	  /* The stack decrement is too big for an immediate value in a single
	     insn.  In theory we could issue multiple subtracts, but after
	     three of them it becomes more space efficient to place the full
	     value in the constant pool and load into a register.  (Also the
	     ARM debugger really likes to see only one stack decrement per
	     function).  So instead we look for a scratch register into which
	     we can load the decrement, and then we subtract this from the
	     stack pointer.  Unfortunately on the thumb the only available
	     scratch registers are the argument registers, and we cannot use
	     these as they may hold arguments to the function.  Instead we
	     attempt to locate a call preserved register which is used by this
	     function.  If we can find one, then we know that it will have
	     been pushed at the start of the prologue and so we can corrupt
	     it now.  */
	  for (regno = LAST_ARG_REGNUM + 1; regno <= LAST_LO_REGNUM; regno++)
	    if (live_regs_mask & (1 << regno))
	      break;

	  gcc_assert(regno <= LAST_LO_REGNUM);

	  reg = gen_rtx_REG (SImode, regno);

	  emit_insn (gen_movsi (reg, GEN_INT (- amount)));

	  insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
					stack_pointer_rtx, reg));

	  dwarf = gen_rtx_SET (stack_pointer_rtx,
			       plus_constant (Pmode, stack_pointer_rtx,
					      -amount));
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  if (frame_pointer_needed)
    thumb_set_frame_pointer (offsets);

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  Similarly if the user has requested no
     scheduling in the prolog.  Similarly if we want non-call exceptions
     using the EABI unwinder, to prevent faulting instructions from being
     swapped with a stack adjustment.  */
  if (crtl->profile || !TARGET_SCHED_PROLOG
      || (arm_except_unwind_info (&global_options) == UI_TARGET
	  && cfun->can_throw_non_call_exceptions))
    emit_insn (gen_blockage ());

  cfun->machine->lr_save_eliminated = !thumb_force_lr_save ();
  if (live_regs_mask & 0xff)
    cfun->machine->lr_save_eliminated = 0;
}

/* Clear caller saved registers not used to pass return values and leaked
   condition flags before exiting a cmse_nonsecure_entry function.  */

void
cmse_nonsecure_entry_clear_before_return (void)
{
  int regno, maxregno = TARGET_HARD_FLOAT ? LAST_VFP_REGNUM : IP_REGNUM;
  uint32_t padding_bits_to_clear = 0;
  auto_sbitmap to_clear_bitmap (maxregno + 1);
  rtx r1_reg, result_rtl, clearing_reg = NULL_RTX;
  tree result_type;

  bitmap_clear (to_clear_bitmap);
  bitmap_set_range (to_clear_bitmap, R0_REGNUM, NUM_ARG_REGS);
  bitmap_set_bit (to_clear_bitmap, IP_REGNUM);

  /* If we are not dealing with -mfloat-abi=soft we will need to clear VFP
     registers.  */
  if (TARGET_HARD_FLOAT)
    {
      int float_bits = D7_VFP_REGNUM - FIRST_VFP_REGNUM + 1;

      bitmap_set_range (to_clear_bitmap, FIRST_VFP_REGNUM, float_bits);

      /* Make sure we don't clear the two scratch registers used to clear the
	 relevant FPSCR bits in output_return_instruction.  */
      emit_use (gen_rtx_REG (SImode, IP_REGNUM));
      bitmap_clear_bit (to_clear_bitmap, IP_REGNUM);
      emit_use (gen_rtx_REG (SImode, 4));
      bitmap_clear_bit (to_clear_bitmap, 4);
    }

  /* If the user has defined registers to be caller saved, these are no longer
     restored by the function before returning and must thus be cleared for
     security purposes.  */
  for (regno = NUM_ARG_REGS; regno <= maxregno; regno++)
    {
      /* We do not touch registers that can be used to pass arguments as per
	 the AAPCS, since these should never be made callee-saved by user
	 options.  */
      if (IN_RANGE (regno, FIRST_VFP_REGNUM, D7_VFP_REGNUM))
	continue;
      if (IN_RANGE (regno, IP_REGNUM, PC_REGNUM))
	continue;
      if (call_used_regs[regno])
	bitmap_set_bit (to_clear_bitmap, regno);
    }

  /* Make sure we do not clear the registers used to return the result in.  */
  result_type = TREE_TYPE (DECL_RESULT (current_function_decl));
  if (!VOID_TYPE_P (result_type))
    {
      uint64_t to_clear_return_mask;
      result_rtl = arm_function_value (result_type, current_function_decl, 0);

      /* No need to check that we return in registers, because we don't
	 support returning on stack yet.  */
      gcc_assert (REG_P (result_rtl));
      to_clear_return_mask
	= compute_not_to_clear_mask (result_type, result_rtl, 0,
				     &padding_bits_to_clear);
      if (to_clear_return_mask)
	{
	  gcc_assert ((unsigned) maxregno < sizeof (long long) * __CHAR_BIT__);
	  for (regno = R0_REGNUM; regno <= maxregno; regno++)
	    {
	      if (to_clear_return_mask & (1ULL << regno))
		bitmap_clear_bit (to_clear_bitmap, regno);
	    }
	}
    }

  if (padding_bits_to_clear != 0)
    {
      int to_clear_bitmap_size = SBITMAP_SIZE ((sbitmap) to_clear_bitmap);
      auto_sbitmap to_clear_arg_regs_bitmap (to_clear_bitmap_size);

      /* Padding_bits_to_clear is not 0 so we know we are dealing with
	 returning a composite type, which only uses r0.  Let's make sure that
	 r1-r3 is cleared too.  */
      bitmap_clear (to_clear_arg_regs_bitmap);
      bitmap_set_range (to_clear_arg_regs_bitmap, R1_REGNUM, NUM_ARG_REGS - 1);
      gcc_assert (bitmap_subset_p (to_clear_arg_regs_bitmap, to_clear_bitmap));
    }

  /* Clear full registers that leak before returning.  */
  clearing_reg = gen_rtx_REG (SImode, TARGET_THUMB1 ? R0_REGNUM : LR_REGNUM);
  r1_reg = gen_rtx_REG (SImode, R0_REGNUM + 1);
  cmse_clear_registers (to_clear_bitmap, &padding_bits_to_clear, 1, r1_reg,
			clearing_reg);
}

/* Generate pattern *pop_multiple_with_stack_update_and_return if single
   POP instruction can be generated.  LR should be replaced by PC.  All
   the checks required are already done by  USE_RETURN_INSN ().  Hence,
   all we really need to check here is if single register is to be
   returned, or multiple register return.  */
void
thumb2_expand_return (bool simple_return)
{
  int i, num_regs;
  unsigned long saved_regs_mask;
  arm_stack_offsets *offsets;

  offsets = arm_get_frame_offsets ();
  saved_regs_mask = offsets->saved_regs_mask;

  for (i = 0, num_regs = 0; i <= LAST_ARM_REGNUM; i++)
    if (saved_regs_mask & (1 << i))
      num_regs++;

  if (!simple_return && saved_regs_mask)
    {
      /* TODO: Verify that this path is never taken for cmse_nonsecure_entry
	 functions or adapt code to handle according to ACLE.  This path should
	 not be reachable for cmse_nonsecure_entry functions though we prefer
	 to assert it for now to ensure that future code changes do not silently
	 change this behavior.  */
      gcc_assert (!IS_CMSE_ENTRY (arm_current_func_type ()));
      if (num_regs == 1)
        {
          rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
          rtx reg = gen_rtx_REG (SImode, PC_REGNUM);
          rtx addr = gen_rtx_MEM (SImode,
                                  gen_rtx_POST_INC (SImode,
                                                    stack_pointer_rtx));
          set_mem_alias_set (addr, get_frame_alias_set ());
          XVECEXP (par, 0, 0) = ret_rtx;
          XVECEXP (par, 0, 1) = gen_rtx_SET (reg, addr);
          RTX_FRAME_RELATED_P (XVECEXP (par, 0, 1)) = 1;
          emit_jump_insn (par);
        }
      else
        {
          saved_regs_mask &= ~ (1 << LR_REGNUM);
          saved_regs_mask |=   (1 << PC_REGNUM);
          arm_emit_multi_reg_pop (saved_regs_mask);
        }
    }
  else
    {
      if (IS_CMSE_ENTRY (arm_current_func_type ()))
	cmse_nonsecure_entry_clear_before_return ();
      emit_jump_insn (simple_return_rtx);
    }
}

void
thumb1_expand_epilogue (void)
{
  HOST_WIDE_INT amount;
  arm_stack_offsets *offsets;
  int regno;

  /* Naked functions don't have prologues.  */
  if (IS_NAKED (arm_current_func_type ()))
    return;

  offsets = arm_get_frame_offsets ();
  amount = offsets->outgoing_args - offsets->saved_regs;

  if (frame_pointer_needed)
    {
      emit_insn (gen_movsi (stack_pointer_rtx, hard_frame_pointer_rtx));
      amount = offsets->locals_base - offsets->saved_regs;
    }
  amount -= 4 * thumb1_extra_regs_pushed (offsets, false);

  gcc_assert (amount >= 0);
  if (amount)
    {
      emit_insn (gen_blockage ());

      if (amount < 512)
	emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (amount)));
      else
	{
	  /* r3 is always free in the epilogue.  */
	  rtx reg = gen_rtx_REG (SImode, LAST_ARG_REGNUM);

	  emit_insn (gen_movsi (reg, GEN_INT (amount)));
	  emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, reg));
	}
    }

  /* Emit a USE (stack_pointer_rtx), so that
     the stack adjustment will not be deleted.  */
  emit_insn (gen_force_register_use (stack_pointer_rtx));

  if (crtl->profile || !TARGET_SCHED_PROLOG)
    emit_insn (gen_blockage ());

  /* Emit a clobber for each insn that will be restored in the epilogue,
     so that flow2 will get register lifetimes correct.  */
  for (regno = 0; regno < 13; regno++)
    if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
      emit_clobber (gen_rtx_REG (SImode, regno));

  if (! df_regs_ever_live_p (LR_REGNUM))
    emit_use (gen_rtx_REG (SImode, LR_REGNUM));

  /* Clear all caller-saved regs that are not used to return.  */
  if (IS_CMSE_ENTRY (arm_current_func_type ()))
    cmse_nonsecure_entry_clear_before_return ();
}

/* Epilogue code for APCS frame.  */
static void
arm_expand_epilogue_apcs_frame (bool really_return)
{
  unsigned long func_type;
  unsigned long saved_regs_mask;
  int num_regs = 0;
  int i;
  int floats_from_frame = 0;
  arm_stack_offsets *offsets;

  gcc_assert (TARGET_APCS_FRAME && frame_pointer_needed && TARGET_ARM);
  func_type = arm_current_func_type ();

  /* Get frame offsets for ARM.  */
  offsets = arm_get_frame_offsets ();
  saved_regs_mask = offsets->saved_regs_mask;

  /* Find the offset of the floating-point save area in the frame.  */
  floats_from_frame
    = (offsets->saved_args
       + arm_compute_static_chain_stack_bytes ()
       - offsets->frame);

  /* Compute how many core registers saved and how far away the floats are.  */
  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    if (saved_regs_mask & (1 << i))
      {
        num_regs++;
        floats_from_frame += 4;
      }

  if (TARGET_HARD_FLOAT)
    {
      int start_reg;
      rtx ip_rtx = gen_rtx_REG (SImode, IP_REGNUM);

      /* The offset is from IP_REGNUM.  */
      int saved_size = arm_get_vfp_saved_size ();
      if (saved_size > 0)
        {
	  rtx_insn *insn;
          floats_from_frame += saved_size;
          insn = emit_insn (gen_addsi3 (ip_rtx,
					hard_frame_pointer_rtx,
					GEN_INT (-floats_from_frame)));
	  arm_add_cfa_adjust_cfa_note (insn, -floats_from_frame,
				       ip_rtx, hard_frame_pointer_rtx);
        }

      /* Generate VFP register multi-pop.  */
      start_reg = FIRST_VFP_REGNUM;

      for (i = FIRST_VFP_REGNUM; i < LAST_VFP_REGNUM; i += 2)
        /* Look for a case where a reg does not need restoring.  */
        if ((!df_regs_ever_live_p (i) || call_used_regs[i])
            && (!df_regs_ever_live_p (i + 1)
                || call_used_regs[i + 1]))
          {
            if (start_reg != i)
              arm_emit_vfp_multi_reg_pop (start_reg,
                                          (i - start_reg) / 2,
                                          gen_rtx_REG (SImode,
                                                       IP_REGNUM));
            start_reg = i + 2;
          }

      /* Restore the remaining regs that we have discovered (or possibly
         even all of them, if the conditional in the for loop never
         fired).  */
      if (start_reg != i)
        arm_emit_vfp_multi_reg_pop (start_reg,
                                    (i - start_reg) / 2,
                                    gen_rtx_REG (SImode, IP_REGNUM));
    }

  if (TARGET_IWMMXT)
    {
      /* The frame pointer is guaranteed to be non-double-word aligned, as
         it is set to double-word-aligned old_stack_pointer - 4.  */
      rtx_insn *insn;
      int lrm_count = (num_regs % 2) ? (num_regs + 2) : (num_regs + 1);

      for (i = LAST_IWMMXT_REGNUM; i >= FIRST_IWMMXT_REGNUM; i--)
        if (df_regs_ever_live_p (i) && !call_used_regs[i])
          {
            rtx addr = gen_frame_mem (V2SImode,
                                 plus_constant (Pmode, hard_frame_pointer_rtx,
                                                - lrm_count * 4));
            insn = emit_insn (gen_movsi (gen_rtx_REG (V2SImode, i), addr));
            REG_NOTES (insn) = alloc_reg_note (REG_CFA_RESTORE,
                                               gen_rtx_REG (V2SImode, i),
                                               NULL_RTX);
            lrm_count += 2;
          }
    }

  /* saved_regs_mask should contain IP which contains old stack pointer
     at the time of activation creation.  Since SP and IP are adjacent registers,
     we can restore the value directly into SP.  */
  gcc_assert (saved_regs_mask & (1 << IP_REGNUM));
  saved_regs_mask &= ~(1 << IP_REGNUM);
  saved_regs_mask |= (1 << SP_REGNUM);

  /* There are two registers left in saved_regs_mask - LR and PC.  We
     only need to restore LR (the return address), but to
     save time we can load it directly into PC, unless we need a
     special function exit sequence, or we are not really returning.  */
  if (really_return
      && ARM_FUNC_TYPE (func_type) == ARM_FT_NORMAL
      && !crtl->calls_eh_return)
    /* Delete LR from the register mask, so that LR on
       the stack is loaded into the PC in the register mask.  */
    saved_regs_mask &= ~(1 << LR_REGNUM);
  else
    saved_regs_mask &= ~(1 << PC_REGNUM);

  num_regs = bit_count (saved_regs_mask);
  if ((offsets->outgoing_args != (1 + num_regs)) || cfun->calls_alloca)
    {
      rtx_insn *insn;
      emit_insn (gen_blockage ());
      /* Unwind the stack to just below the saved registers.  */
      insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
				    hard_frame_pointer_rtx,
				    GEN_INT (- 4 * num_regs)));

      arm_add_cfa_adjust_cfa_note (insn, - 4 * num_regs,
				   stack_pointer_rtx, hard_frame_pointer_rtx);
    }

  arm_emit_multi_reg_pop (saved_regs_mask);

  if (IS_INTERRUPT (func_type))
    {
      /* Interrupt handlers will have pushed the
         IP onto the stack, so restore it now.  */
      rtx_insn *insn;
      rtx addr = gen_rtx_MEM (SImode,
                              gen_rtx_POST_INC (SImode,
                              stack_pointer_rtx));
      set_mem_alias_set (addr, get_frame_alias_set ());
      insn = emit_insn (gen_movsi (gen_rtx_REG (SImode, IP_REGNUM), addr));
      REG_NOTES (insn) = alloc_reg_note (REG_CFA_RESTORE,
                                         gen_rtx_REG (SImode, IP_REGNUM),
                                         NULL_RTX);
    }

  if (!really_return || (saved_regs_mask & (1 << PC_REGNUM)))
    return;

  if (crtl->calls_eh_return)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   gen_rtx_REG (SImode, ARM_EH_STACKADJ_REGNUM)));

  if (IS_STACKALIGN (func_type))
    /* Restore the original stack pointer.  Before prologue, the stack was
       realigned and the original stack pointer saved in r0.  For details,
       see comment in arm_expand_prologue.  */
    emit_insn (gen_movsi (stack_pointer_rtx, gen_rtx_REG (SImode, R0_REGNUM)));

  emit_jump_insn (simple_return_rtx);
}

/* Generate RTL to represent ARM epilogue.  Really_return is true if the
   function is not a sibcall.  */
void
arm_expand_epilogue (bool really_return)
{
  unsigned long func_type;
  unsigned long saved_regs_mask;
  int num_regs = 0;
  int i;
  int amount;
  arm_stack_offsets *offsets;

  func_type = arm_current_func_type ();

  /* Naked functions don't have epilogue.  Hence, generate return pattern, and
     let output_return_instruction take care of instruction emission if any.  */
  if (IS_NAKED (func_type)
      || (IS_VOLATILE (func_type) && TARGET_ABORT_NORETURN))
    {
      if (really_return)
        emit_jump_insn (simple_return_rtx);
      return;
    }

  /* If we are throwing an exception, then we really must be doing a
     return, so we can't tail-call.  */
  gcc_assert (!crtl->calls_eh_return || really_return);

  if (TARGET_APCS_FRAME && frame_pointer_needed && TARGET_ARM)
    {
      arm_expand_epilogue_apcs_frame (really_return);
      return;
    }

  /* Get frame offsets for ARM.  */
  offsets = arm_get_frame_offsets ();
  saved_regs_mask = offsets->saved_regs_mask;
  num_regs = bit_count (saved_regs_mask);

  if (frame_pointer_needed)
    {
      rtx_insn *insn;
      /* Restore stack pointer if necessary.  */
      if (TARGET_ARM)
        {
          /* In ARM mode, frame pointer points to first saved register.
             Restore stack pointer to last saved register.  */
          amount = offsets->frame - offsets->saved_regs;

          /* Force out any pending memory operations that reference stacked data
             before stack de-allocation occurs.  */
          emit_insn (gen_blockage ());
	  insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
			    hard_frame_pointer_rtx,
			    GEN_INT (amount)));
	  arm_add_cfa_adjust_cfa_note (insn, amount,
				       stack_pointer_rtx,
				       hard_frame_pointer_rtx);

          /* Emit USE(stack_pointer_rtx) to ensure that stack adjustment is not
             deleted.  */
          emit_insn (gen_force_register_use (stack_pointer_rtx));
        }
      else
        {
          /* In Thumb-2 mode, the frame pointer points to the last saved
             register.  */
	  amount = offsets->locals_base - offsets->saved_regs;
	  if (amount)
	    {
	      insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
				hard_frame_pointer_rtx,
				GEN_INT (amount)));
	      arm_add_cfa_adjust_cfa_note (insn, amount,
					   hard_frame_pointer_rtx,
					   hard_frame_pointer_rtx);
	    }

          /* Force out any pending memory operations that reference stacked data
             before stack de-allocation occurs.  */
          emit_insn (gen_blockage ());
	  insn = emit_insn (gen_movsi (stack_pointer_rtx,
				       hard_frame_pointer_rtx));
	  arm_add_cfa_adjust_cfa_note (insn, 0,
				       stack_pointer_rtx,
				       hard_frame_pointer_rtx);
          /* Emit USE(stack_pointer_rtx) to ensure that stack adjustment is not
             deleted.  */
          emit_insn (gen_force_register_use (stack_pointer_rtx));
        }
    }
  else
    {
      /* Pop off outgoing args and local frame to adjust stack pointer to
         last saved register.  */
      amount = offsets->outgoing_args - offsets->saved_regs;
      if (amount)
        {
	  rtx_insn *tmp;
          /* Force out any pending memory operations that reference stacked data
             before stack de-allocation occurs.  */
          emit_insn (gen_blockage ());
	  tmp = emit_insn (gen_addsi3 (stack_pointer_rtx,
				       stack_pointer_rtx,
				       GEN_INT (amount)));
	  arm_add_cfa_adjust_cfa_note (tmp, amount,
				       stack_pointer_rtx, stack_pointer_rtx);
          /* Emit USE(stack_pointer_rtx) to ensure that stack adjustment is
             not deleted.  */
          emit_insn (gen_force_register_use (stack_pointer_rtx));
        }
    }

  if (TARGET_HARD_FLOAT)
    {
      /* Generate VFP register multi-pop.  */
      int end_reg = LAST_VFP_REGNUM + 1;

      /* Scan the registers in reverse order.  We need to match
         any groupings made in the prologue and generate matching
         vldm operations.  The need to match groups is because,
         unlike pop, vldm can only do consecutive regs.  */
      for (i = LAST_VFP_REGNUM - 1; i >= FIRST_VFP_REGNUM; i -= 2)
        /* Look for a case where a reg does not need restoring.  */
        if ((!df_regs_ever_live_p (i) || call_used_regs[i])
            && (!df_regs_ever_live_p (i + 1)
                || call_used_regs[i + 1]))
          {
            /* Restore the regs discovered so far (from reg+2 to
               end_reg).  */
            if (end_reg > i + 2)
              arm_emit_vfp_multi_reg_pop (i + 2,
                                          (end_reg - (i + 2)) / 2,
                                          stack_pointer_rtx);
            end_reg = i;
          }

      /* Restore the remaining regs that we have discovered (or possibly
         even all of them, if the conditional in the for loop never
         fired).  */
      if (end_reg > i + 2)
        arm_emit_vfp_multi_reg_pop (i + 2,
                                    (end_reg - (i + 2)) / 2,
                                    stack_pointer_rtx);
    }

  if (TARGET_IWMMXT)
    for (i = FIRST_IWMMXT_REGNUM; i <= LAST_IWMMXT_REGNUM; i++)
      if (df_regs_ever_live_p (i) && !call_used_regs[i])
        {
          rtx_insn *insn;
          rtx addr = gen_rtx_MEM (V2SImode,
                                  gen_rtx_POST_INC (SImode,
                                                    stack_pointer_rtx));
          set_mem_alias_set (addr, get_frame_alias_set ());
          insn = emit_insn (gen_movsi (gen_rtx_REG (V2SImode, i), addr));
          REG_NOTES (insn) = alloc_reg_note (REG_CFA_RESTORE,
                                             gen_rtx_REG (V2SImode, i),
                                             NULL_RTX);
	  arm_add_cfa_adjust_cfa_note (insn, UNITS_PER_WORD,
				       stack_pointer_rtx, stack_pointer_rtx);
        }

  if (saved_regs_mask)
    {
      rtx insn;
      bool return_in_pc = false;

      if (ARM_FUNC_TYPE (func_type) != ARM_FT_INTERWORKED
          && (TARGET_ARM || ARM_FUNC_TYPE (func_type) == ARM_FT_NORMAL)
	  && !IS_CMSE_ENTRY (func_type)
          && !IS_STACKALIGN (func_type)
          && really_return
          && crtl->args.pretend_args_size == 0
          && saved_regs_mask & (1 << LR_REGNUM)
          && !crtl->calls_eh_return)
        {
          saved_regs_mask &= ~(1 << LR_REGNUM);
          saved_regs_mask |= (1 << PC_REGNUM);
          return_in_pc = true;
        }

      if (num_regs == 1 && (!IS_INTERRUPT (func_type) || !return_in_pc))
        {
          for (i = 0; i <= LAST_ARM_REGNUM; i++)
            if (saved_regs_mask & (1 << i))
              {
                rtx addr = gen_rtx_MEM (SImode,
                                        gen_rtx_POST_INC (SImode,
                                                          stack_pointer_rtx));
                set_mem_alias_set (addr, get_frame_alias_set ());

                if (i == PC_REGNUM)
                  {
                    insn = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
                    XVECEXP (insn, 0, 0) = ret_rtx;
                    XVECEXP (insn, 0, 1) = gen_rtx_SET (gen_rtx_REG (SImode, i),
                                                        addr);
                    RTX_FRAME_RELATED_P (XVECEXP (insn, 0, 1)) = 1;
                    insn = emit_jump_insn (insn);
                  }
                else
                  {
                    insn = emit_insn (gen_movsi (gen_rtx_REG (SImode, i),
                                                 addr));
                    REG_NOTES (insn) = alloc_reg_note (REG_CFA_RESTORE,
                                                       gen_rtx_REG (SImode, i),
                                                       NULL_RTX);
		    arm_add_cfa_adjust_cfa_note (insn, UNITS_PER_WORD,
						 stack_pointer_rtx,
						 stack_pointer_rtx);
                  }
              }
        }
      else
        {
          if (TARGET_LDRD
	      && current_tune->prefer_ldrd_strd
              && !optimize_function_for_size_p (cfun))
            {
              if (TARGET_THUMB2)
                thumb2_emit_ldrd_pop (saved_regs_mask);
              else if (TARGET_ARM && !IS_INTERRUPT (func_type))
                arm_emit_ldrd_pop (saved_regs_mask);
              else
                arm_emit_multi_reg_pop (saved_regs_mask);
            }
          else
            arm_emit_multi_reg_pop (saved_regs_mask);
        }

      if (return_in_pc)
        return;
    }

  amount
    = crtl->args.pretend_args_size + arm_compute_static_chain_stack_bytes();
  if (amount)
    {
      int i, j;
      rtx dwarf = NULL_RTX;
      rtx_insn *tmp =
	emit_insn (gen_addsi3 (stack_pointer_rtx,
			       stack_pointer_rtx,
			       GEN_INT (amount)));

      RTX_FRAME_RELATED_P (tmp) = 1;

      if (cfun->machine->uses_anonymous_args)
	{
	  /* Restore pretend args.  Refer arm_expand_prologue on how to save
	     pretend_args in stack.  */
	  int num_regs = crtl->args.pretend_args_size / 4;
	  saved_regs_mask = (0xf0 >> num_regs) & 0xf;
	  for (j = 0, i = 0; j < num_regs; i++)
	    if (saved_regs_mask & (1 << i))
	      {
		rtx reg = gen_rtx_REG (SImode, i);
		dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
		j++;
	      }
	  REG_NOTES (tmp) = dwarf;
	}
      arm_add_cfa_adjust_cfa_note (tmp, amount,
				   stack_pointer_rtx, stack_pointer_rtx);
    }

    /* Clear all caller-saved regs that are not used to return.  */
    if (IS_CMSE_ENTRY (arm_current_func_type ()))
      {
	/* CMSE_ENTRY always returns.  */
	gcc_assert (really_return);
	cmse_nonsecure_entry_clear_before_return ();
      }

  if (!really_return)
    return;

  if (crtl->calls_eh_return)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
                           stack_pointer_rtx,
                           gen_rtx_REG (SImode, ARM_EH_STACKADJ_REGNUM)));

  if (IS_STACKALIGN (func_type))
    /* Restore the original stack pointer.  Before prologue, the stack was
       realigned and the original stack pointer saved in r0.  For details,
       see comment in arm_expand_prologue.  */
    emit_insn (gen_movsi (stack_pointer_rtx, gen_rtx_REG (SImode, R0_REGNUM)));

  emit_jump_insn (simple_return_rtx);
}

/* Implementation of insn prologue_thumb1_interwork.  This is the first
   "instruction" of a function called in ARM mode.  Swap to thumb mode.  */

const char *
thumb1_output_interwork (void)
{
  const char * name;
  FILE *f = asm_out_file;

  gcc_assert (MEM_P (DECL_RTL (current_function_decl)));
  gcc_assert (GET_CODE (XEXP (DECL_RTL (current_function_decl), 0))
	      == SYMBOL_REF);
  name = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);

  /* Generate code sequence to switch us into Thumb mode.  */
  /* The .code 32 directive has already been emitted by
     ASM_DECLARE_FUNCTION_NAME.  */
  asm_fprintf (f, "\torr\t%r, %r, #1\n", IP_REGNUM, PC_REGNUM);
  asm_fprintf (f, "\tbx\t%r\n", IP_REGNUM);

  /* Generate a label, so that the debugger will notice the
     change in instruction sets.  This label is also used by
     the assembler to bypass the ARM code when this function
     is called from a Thumb encoded function elsewhere in the
     same file.  Hence the definition of STUB_NAME here must
     agree with the definition in gas/config/tc-arm.c.  */

#define STUB_NAME ".real_start_of"

  fprintf (f, "\t.code\t16\n");
#ifdef ARM_PE
  if (arm_dllexport_name_p (name))
    name = arm_strip_name_encoding (name);
#endif
  asm_fprintf (f, "\t.globl %s%U%s\n", STUB_NAME, name);
  fprintf (f, "\t.thumb_func\n");
  asm_fprintf (f, "%s%U%s:\n", STUB_NAME, name);

  return "";
}

/* Handle the case of a double word load into a low register from
   a computed memory address.  The computed address may involve a
   register which is overwritten by the load.  */
const char *
thumb_load_double_from_address (rtx *operands)
{
  rtx addr;
  rtx base;
  rtx offset;
  rtx arg1;
  rtx arg2;

  gcc_assert (REG_P (operands[0]));
  gcc_assert (MEM_P (operands[1]));

  /* Get the memory address.  */
  addr = XEXP (operands[1], 0);

  /* Work out how the memory address is computed.  */
  switch (GET_CODE (addr))
    {
    case REG:
      operands[2] = adjust_address (operands[1], SImode, 4);

      if (REGNO (operands[0]) == REGNO (addr))
	{
	  output_asm_insn ("ldr\t%H0, %2", operands);
	  output_asm_insn ("ldr\t%0, %1", operands);
	}
      else
	{
	  output_asm_insn ("ldr\t%0, %1", operands);
	  output_asm_insn ("ldr\t%H0, %2", operands);
	}
      break;

    case CONST:
      /* Compute <address> + 4 for the high order load.  */
      operands[2] = adjust_address (operands[1], SImode, 4);

      output_asm_insn ("ldr\t%0, %1", operands);
      output_asm_insn ("ldr\t%H0, %2", operands);
      break;

    case PLUS:
      arg1   = XEXP (addr, 0);
      arg2   = XEXP (addr, 1);

      if (CONSTANT_P (arg1))
	base = arg2, offset = arg1;
      else
	base = arg1, offset = arg2;

      gcc_assert (REG_P (base));

      /* Catch the case of <address> = <reg> + <reg> */
      if (REG_P (offset))
	{
	  int reg_offset = REGNO (offset);
	  int reg_base   = REGNO (base);
	  int reg_dest   = REGNO (operands[0]);

	  /* Add the base and offset registers together into the
             higher destination register.  */
	  asm_fprintf (asm_out_file, "\tadd\t%r, %r, %r",
		       reg_dest + 1, reg_base, reg_offset);

	  /* Load the lower destination register from the address in
             the higher destination register.  */
	  asm_fprintf (asm_out_file, "\tldr\t%r, [%r, #0]",
		       reg_dest, reg_dest + 1);

	  /* Load the higher destination register from its own address
             plus 4.  */
	  asm_fprintf (asm_out_file, "\tldr\t%r, [%r, #4]",
		       reg_dest + 1, reg_dest + 1);
	}
      else
	{
	  /* Compute <address> + 4 for the high order load.  */
	  operands[2] = adjust_address (operands[1], SImode, 4);

	  /* If the computed address is held in the low order register
	     then load the high order register first, otherwise always
	     load the low order register first.  */
	  if (REGNO (operands[0]) == REGNO (base))
	    {
	      output_asm_insn ("ldr\t%H0, %2", operands);
	      output_asm_insn ("ldr\t%0, %1", operands);
	    }
	  else
	    {
	      output_asm_insn ("ldr\t%0, %1", operands);
	      output_asm_insn ("ldr\t%H0, %2", operands);
	    }
	}
      break;

    case LABEL_REF:
      /* With no registers to worry about we can just load the value
         directly.  */
      operands[2] = adjust_address (operands[1], SImode, 4);

      output_asm_insn ("ldr\t%H0, %2", operands);
      output_asm_insn ("ldr\t%0, %1", operands);
      break;

    default:
      gcc_unreachable ();
    }

  return "";
}

const char *
thumb_output_move_mem_multiple (int n, rtx *operands)
{
  switch (n)
    {
    case 2:
      if (REGNO (operands[4]) > REGNO (operands[5]))
	std::swap (operands[4], operands[5]);

      output_asm_insn ("ldmia\t%1!, {%4, %5}", operands);
      output_asm_insn ("stmia\t%0!, {%4, %5}", operands);
      break;

    case 3:
      if (REGNO (operands[4]) > REGNO (operands[5]))
        std::swap (operands[4], operands[5]);
      if (REGNO (operands[5]) > REGNO (operands[6]))
        std::swap (operands[5], operands[6]);
      if (REGNO (operands[4]) > REGNO (operands[5]))
        std::swap (operands[4], operands[5]);

      output_asm_insn ("ldmia\t%1!, {%4, %5, %6}", operands);
      output_asm_insn ("stmia\t%0!, {%4, %5, %6}", operands);
      break;

    default:
      gcc_unreachable ();
    }

  return "";
}

/* Output a call-via instruction for thumb state.  */
const char *
thumb_call_via_reg (rtx reg)
{
  int regno = REGNO (reg);
  rtx *labelp;

  gcc_assert (regno < LR_REGNUM);

  /* If we are in the normal text section we can use a single instance
     per compilation unit.  If we are doing function sections, then we need
     an entry per section, since we can't rely on reachability.  */
  if (in_section == text_section)
    {
      thumb_call_reg_needed = 1;

      if (thumb_call_via_label[regno] == NULL)
	thumb_call_via_label[regno] = gen_label_rtx ();
      labelp = thumb_call_via_label + regno;
    }
  else
    {
      if (cfun->machine->call_via[regno] == NULL)
	cfun->machine->call_via[regno] = gen_label_rtx ();
      labelp = cfun->machine->call_via + regno;
    }

  output_asm_insn ("bl\t%a0", labelp);
  return "";
}

/* Routines for generating rtl.  */
void
thumb_expand_movmemqi (rtx *operands)
{
  rtx out = copy_to_mode_reg (SImode, XEXP (operands[0], 0));
  rtx in  = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  HOST_WIDE_INT len = INTVAL (operands[2]);
  HOST_WIDE_INT offset = 0;

  while (len >= 12)
    {
      emit_insn (gen_movmem12b (out, in, out, in));
      len -= 12;
    }

  if (len >= 8)
    {
      emit_insn (gen_movmem8b (out, in, out, in));
      len -= 8;
    }

  if (len >= 4)
    {
      rtx reg = gen_reg_rtx (SImode);
      emit_insn (gen_movsi (reg, gen_rtx_MEM (SImode, in)));
      emit_insn (gen_movsi (gen_rtx_MEM (SImode, out), reg));
      len -= 4;
      offset += 4;
    }

  if (len >= 2)
    {
      rtx reg = gen_reg_rtx (HImode);
      emit_insn (gen_movhi (reg, gen_rtx_MEM (HImode,
					      plus_constant (Pmode, in,
							     offset))));
      emit_insn (gen_movhi (gen_rtx_MEM (HImode, plus_constant (Pmode, out,
								offset)),
			    reg));
      len -= 2;
      offset += 2;
    }

  if (len)
    {
      rtx reg = gen_reg_rtx (QImode);
      emit_insn (gen_movqi (reg, gen_rtx_MEM (QImode,
					      plus_constant (Pmode, in,
							     offset))));
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, plus_constant (Pmode, out,
								offset)),
			    reg));
    }
}

void
thumb_reload_out_hi (rtx *operands)
{
  emit_insn (gen_thumb_movhi_clobber (operands[0], operands[1], operands[2]));
}

/* Return the length of a function name prefix
    that starts with the character 'c'.  */
static int
arm_get_strip_length (int c)
{
  switch (c)
    {
    ARM_NAME_ENCODING_LENGTHS
      default: return 0;
    }
}

/* Return a pointer to a function's name with any
   and all prefix encodings stripped from it.  */
const char *
arm_strip_name_encoding (const char *name)
{
  int skip;

  while ((skip = arm_get_strip_length (* name)))
    name += skip;

  return name;
}

/* If there is a '*' anywhere in the name's prefix, then
   emit the stripped name verbatim, otherwise prepend an
   underscore if leading underscores are being used.  */
void
arm_asm_output_labelref (FILE *stream, const char *name)
{
  int skip;
  int verbatim = 0;

  while ((skip = arm_get_strip_length (* name)))
    {
      verbatim |= (*name == '*');
      name += skip;
    }

  if (verbatim)
    fputs (name, stream);
  else
    asm_fprintf (stream, "%U%s", name);
}

/* This function is used to emit an EABI tag and its associated value.
   We emit the numerical value of the tag in case the assembler does not
   support textual tags.  (Eg gas prior to 2.20).  If requested we include
   the tag name in a comment so that anyone reading the assembler output
   will know which tag is being set.

   This function is not static because arm-c.c needs it too.  */

void
arm_emit_eabi_attribute (const char *name, int num, int val)
{
  asm_fprintf (asm_out_file, "\t.eabi_attribute %d, %d", num, val);
  if (flag_verbose_asm || flag_debug_asm)
    asm_fprintf (asm_out_file, "\t%s %s", ASM_COMMENT_START, name);
  asm_fprintf (asm_out_file, "\n");
}

/* This function is used to print CPU tuning information as comment
   in assembler file.  Pointers are not printed for now.  */

void
arm_print_tune_info (void)
{
  asm_fprintf (asm_out_file, "\t" ASM_COMMENT_START ".tune parameters\n");
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START "constant_limit:\t%d\n",
	       current_tune->constant_limit);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "max_insns_skipped:\t%d\n", current_tune->max_insns_skipped);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "prefetch.num_slots:\t%d\n", current_tune->prefetch.num_slots);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "prefetch.l1_cache_size:\t%d\n",
	       current_tune->prefetch.l1_cache_size);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "prefetch.l1_cache_line_size:\t%d\n",
	       current_tune->prefetch.l1_cache_line_size);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "prefer_constant_pool:\t%d\n",
	       (int) current_tune->prefer_constant_pool);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "branch_cost:\t(s:speed, p:predictable)\n");
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START "\t\ts&p\tcost\n");
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START "\t\t00\t%d\n",
	       current_tune->branch_cost (false, false));
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START "\t\t01\t%d\n",
	       current_tune->branch_cost (false, true));
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START "\t\t10\t%d\n",
	       current_tune->branch_cost (true, false));
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START "\t\t11\t%d\n",
	       current_tune->branch_cost (true, true));
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "prefer_ldrd_strd:\t%d\n",
	       (int) current_tune->prefer_ldrd_strd);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "logical_op_non_short_circuit:\t[%d,%d]\n",
	       (int) current_tune->logical_op_non_short_circuit_thumb,
	       (int) current_tune->logical_op_non_short_circuit_arm);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "prefer_neon_for_64bits:\t%d\n",
	       (int) current_tune->prefer_neon_for_64bits);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "disparage_flag_setting_t16_encodings:\t%d\n",
	       (int) current_tune->disparage_flag_setting_t16_encodings);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "string_ops_prefer_neon:\t%d\n",
	       (int) current_tune->string_ops_prefer_neon);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START
	       "max_insns_inline_memset:\t%d\n",
	       current_tune->max_insns_inline_memset);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START "fusible_ops:\t%u\n",
	       current_tune->fusible_ops);
  asm_fprintf (asm_out_file, "\t\t" ASM_COMMENT_START "sched_autopref:\t%d\n",
	       (int) current_tune->sched_autopref);
}

/* Print .arch and .arch_extension directives corresponding to the
   current architecture configuration.  */
static void
arm_print_asm_arch_directives ()
{
  const arch_option *arch
    = arm_parse_arch_option_name (all_architectures, "-march",
				  arm_active_target.arch_name);
  auto_sbitmap opt_bits (isa_num_bits);

  gcc_assert (arch);

  asm_fprintf (asm_out_file, "\t.arch %s\n", arm_active_target.arch_name);
  arm_last_printed_arch_string = arm_active_target.arch_name;
  if (!arch->common.extensions)
    return;

  for (const struct cpu_arch_extension *opt = arch->common.extensions;
       opt->name != NULL;
       opt++)
    {
      if (!opt->remove)
	{
	  arm_initialize_isa (opt_bits, opt->isa_bits);

	  /* If every feature bit of this option is set in the target
	     ISA specification, print out the option name.  However,
	     don't print anything if all the bits are part of the
	     FPU specification.  */
	  if (bitmap_subset_p (opt_bits, arm_active_target.isa)
	      && !bitmap_subset_p (opt_bits, isa_all_fpubits))
	    asm_fprintf (asm_out_file, "\t.arch_extension %s\n", opt->name);
	}
    }
}

static void
arm_file_start (void)
{
  int val;

  if (TARGET_BPABI)
    {
      /* We don't have a specified CPU.  Use the architecture to
	 generate the tags.

	 Note: it might be better to do this unconditionally, then the
	 assembler would not need to know about all new CPU names as
	 they are added.  */
      if (!arm_active_target.core_name)
	{
	  /* armv7ve doesn't support any extensions.  */
	  if (strcmp (arm_active_target.arch_name, "armv7ve") == 0)
	    {
	      /* Keep backward compatability for assemblers
		 which don't support armv7ve.  */
	      asm_fprintf (asm_out_file, "\t.arch armv7-a\n");
	      asm_fprintf (asm_out_file, "\t.arch_extension virt\n");
	      asm_fprintf (asm_out_file, "\t.arch_extension idiv\n");
	      asm_fprintf (asm_out_file, "\t.arch_extension sec\n");
	      asm_fprintf (asm_out_file, "\t.arch_extension mp\n");
	      arm_last_printed_arch_string = "armv7ve";
	    }
	  else
	    arm_print_asm_arch_directives ();
	}
      else if (strncmp (arm_active_target.core_name, "generic", 7) == 0)
	{
	  asm_fprintf (asm_out_file, "\t.arch %s\n",
		       arm_active_target.core_name + 8);
	  arm_last_printed_arch_string = arm_active_target.core_name + 8;
	}
      else
	{
	  const char* truncated_name
	    = arm_rewrite_selected_cpu (arm_active_target.core_name);
	  asm_fprintf (asm_out_file, "\t.cpu %s\n", truncated_name);
	}

      if (print_tune_info)
	arm_print_tune_info ();

      if (! TARGET_SOFT_FLOAT)
	{
	  if (TARGET_HARD_FLOAT && TARGET_VFP_SINGLE)
	    arm_emit_eabi_attribute ("Tag_ABI_HardFP_use", 27, 1);

	  if (TARGET_HARD_FLOAT_ABI)
	    arm_emit_eabi_attribute ("Tag_ABI_VFP_args", 28, 1);
	}

      /* Some of these attributes only apply when the corresponding features
	 are used.  However we don't have any easy way of figuring this out.
	 Conservatively record the setting that would have been used.  */

      if (flag_rounding_math)
	arm_emit_eabi_attribute ("Tag_ABI_FP_rounding", 19, 1);

      if (!flag_unsafe_math_optimizations)
	{
	  arm_emit_eabi_attribute ("Tag_ABI_FP_denormal", 20, 1);
	  arm_emit_eabi_attribute ("Tag_ABI_FP_exceptions", 21, 1);
	}
      if (flag_signaling_nans)
	arm_emit_eabi_attribute ("Tag_ABI_FP_user_exceptions", 22, 1);

      arm_emit_eabi_attribute ("Tag_ABI_FP_number_model", 23,
			   flag_finite_math_only ? 1 : 3);

      arm_emit_eabi_attribute ("Tag_ABI_align8_needed", 24, 1);
      arm_emit_eabi_attribute ("Tag_ABI_align8_preserved", 25, 1);
      arm_emit_eabi_attribute ("Tag_ABI_enum_size", 26,
			       flag_short_enums ? 1 : 2);

      /* Tag_ABI_optimization_goals.  */
      if (optimize_size)
	val = 4;
      else if (optimize >= 2)
	val = 2;
      else if (optimize)
	val = 1;
      else
	val = 6;
      arm_emit_eabi_attribute ("Tag_ABI_optimization_goals", 30, val);

      arm_emit_eabi_attribute ("Tag_CPU_unaligned_access", 34,
			       unaligned_access);

      if (arm_fp16_format)
	arm_emit_eabi_attribute ("Tag_ABI_FP_16bit_format", 38,
			     (int) arm_fp16_format);

      if (arm_lang_output_object_attributes_hook)
	arm_lang_output_object_attributes_hook();
    }

  default_file_start ();
}

static void
arm_file_end (void)
{
  int regno;

  if (NEED_INDICATE_EXEC_STACK)
    /* Add .note.GNU-stack.  */
    file_end_indicate_exec_stack ();

  if (! thumb_call_reg_needed)
    return;

  switch_to_section (text_section);
  asm_fprintf (asm_out_file, "\t.code 16\n");
  ASM_OUTPUT_ALIGN (asm_out_file, 1);

  for (regno = 0; regno < LR_REGNUM; regno++)
    {
      rtx label = thumb_call_via_label[regno];

      if (label != 0)
	{
	  targetm.asm_out.internal_label (asm_out_file, "L",
					  CODE_LABEL_NUMBER (label));
	  asm_fprintf (asm_out_file, "\tbx\t%r\n", regno);
	}
    }
}

#ifndef ARM_PE
/* Symbols in the text segment can be accessed without indirecting via the
   constant pool; it may take an extra binary operation, but this is still
   faster than indirecting via memory.  Don't do this when not optimizing,
   since we won't be calculating al of the offsets necessary to do this
   simplification.  */

static void
arm_encode_section_info (tree decl, rtx rtl, int first)
{
  if (optimize > 0 && TREE_CONSTANT (decl))
    SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;

  default_encode_section_info (decl, rtl, first);
}
#endif /* !ARM_PE */

static void
arm_internal_label (FILE *stream, const char *prefix, unsigned long labelno)
{
  if (arm_ccfsm_state == 3 && (unsigned) arm_target_label == labelno
      && !strcmp (prefix, "L"))
    {
      arm_ccfsm_state = 0;
      arm_target_insn = NULL;
    }
  default_internal_label (stream, prefix, labelno);
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */

static void
arm_thumb1_mi_thunk (FILE *file, tree, HOST_WIDE_INT delta,
		     HOST_WIDE_INT, tree function)
{
  static int thunk_label = 0;
  char label[256];
  char labelpc[256];
  int mi_delta = delta;
  const char *const mi_op = mi_delta < 0 ? "sub" : "add";
  int shift = 0;
  int this_regno = (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function)
                    ? 1 : 0);
  if (mi_delta < 0)
    mi_delta = - mi_delta;

  final_start_function (emit_barrier (), file, 1);

  if (TARGET_THUMB1)
    {
      int labelno = thunk_label++;
      ASM_GENERATE_INTERNAL_LABEL (label, "LTHUMBFUNC", labelno);
      /* Thunks are entered in arm mode when available.  */
      if (TARGET_THUMB1_ONLY)
	{
	  /* push r3 so we can use it as a temporary.  */
	  /* TODO: Omit this save if r3 is not used.  */
	  fputs ("\tpush {r3}\n", file);
	  fputs ("\tldr\tr3, ", file);
	}
      else
	{
	  fputs ("\tldr\tr12, ", file);
	}
      assemble_name (file, label);
      fputc ('\n', file);
      if (flag_pic)
	{
	  /* If we are generating PIC, the ldr instruction below loads
	     "(target - 7) - .LTHUNKPCn" into r12.  The pc reads as
	     the address of the add + 8, so we have:

	     r12 = (target - 7) - .LTHUNKPCn + (.LTHUNKPCn + 8)
	         = target + 1.

	     Note that we have "+ 1" because some versions of GNU ld
	     don't set the low bit of the result for R_ARM_REL32
	     relocations against thumb function symbols.
	     On ARMv6M this is +4, not +8.  */
	  ASM_GENERATE_INTERNAL_LABEL (labelpc, "LTHUNKPC", labelno);
	  assemble_name (file, labelpc);
	  fputs (":\n", file);
	  if (TARGET_THUMB1_ONLY)
	    {
	      /* This is 2 insns after the start of the thunk, so we know it
	         is 4-byte aligned.  */
	      fputs ("\tadd\tr3, pc, r3\n", file);
	      fputs ("\tmov r12, r3\n", file);
	    }
	  else
	    fputs ("\tadd\tr12, pc, r12\n", file);
	}
      else if (TARGET_THUMB1_ONLY)
	fputs ("\tmov r12, r3\n", file);
    }
  if (TARGET_THUMB1_ONLY)
    {
      if (mi_delta > 255)
	{
	  fputs ("\tldr\tr3, ", file);
	  assemble_name (file, label);
	  fputs ("+4\n", file);
	  asm_fprintf (file, "\t%ss\t%r, %r, r3\n",
		       mi_op, this_regno, this_regno);
	}
      else if (mi_delta != 0)
	{
	  /* Thumb1 unified syntax requires s suffix in instruction name when
	     one of the operands is immediate.  */
	  asm_fprintf (file, "\t%ss\t%r, %r, #%d\n",
		       mi_op, this_regno, this_regno,
		       mi_delta);
	}
    }
  else
    {
      /* TODO: Use movw/movt for large constants when available.  */
      while (mi_delta != 0)
	{
	  if ((mi_delta & (3 << shift)) == 0)
	    shift += 2;
	  else
	    {
	      asm_fprintf (file, "\t%s\t%r, %r, #%d\n",
			   mi_op, this_regno, this_regno,
			   mi_delta & (0xff << shift));
	      mi_delta &= ~(0xff << shift);
	      shift += 8;
	    }
	}
    }
  if (TARGET_THUMB1)
    {
      if (TARGET_THUMB1_ONLY)
	fputs ("\tpop\t{r3}\n", file);

      fprintf (file, "\tbx\tr12\n");
      ASM_OUTPUT_ALIGN (file, 2);
      assemble_name (file, label);
      fputs (":\n", file);
      if (flag_pic)
	{
	  /* Output ".word .LTHUNKn-[3,7]-.LTHUNKPCn".  */
	  rtx tem = XEXP (DECL_RTL (function), 0);
	  /* For TARGET_THUMB1_ONLY the thunk is in Thumb mode, so the PC
	     pipeline offset is four rather than eight.  Adjust the offset
	     accordingly.  */
	  tem = plus_constant (GET_MODE (tem), tem,
			       TARGET_THUMB1_ONLY ? -3 : -7);
	  tem = gen_rtx_MINUS (GET_MODE (tem),
			       tem,
			       gen_rtx_SYMBOL_REF (Pmode,
						   ggc_strdup (labelpc)));
	  assemble_integer (tem, 4, BITS_PER_WORD, 1);
	}
      else
	/* Output ".word .LTHUNKn".  */
	assemble_integer (XEXP (DECL_RTL (function), 0), 4, BITS_PER_WORD, 1);

      if (TARGET_THUMB1_ONLY && mi_delta > 255)
	assemble_integer (GEN_INT(mi_delta), 4, BITS_PER_WORD, 1);
    }
  else
    {
      fputs ("\tb\t", file);
      assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
      if (NEED_PLT_RELOC)
        fputs ("(PLT)", file);
      fputc ('\n', file);
    }

  final_end_function ();
}

/* MI thunk handling for TARGET_32BIT.  */

static void
arm32_output_mi_thunk (FILE *file, tree, HOST_WIDE_INT delta,
		       HOST_WIDE_INT vcall_offset, tree function)
{
  /* On ARM, this_regno is R0 or R1 depending on
     whether the function returns an aggregate or not.
  */
  int this_regno = (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)),
				       function)
		    ? R1_REGNUM : R0_REGNUM);

  rtx temp = gen_rtx_REG (Pmode, IP_REGNUM);
  rtx this_rtx = gen_rtx_REG (Pmode, this_regno);
  reload_completed = 1;
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Add DELTA to THIS_RTX.  */
  if (delta != 0)
    arm_split_constant (PLUS, Pmode, NULL_RTX,
			delta, this_rtx, this_rtx, false);

  /* Add *(*THIS_RTX + VCALL_OFFSET) to THIS_RTX.  */
  if (vcall_offset != 0)
    {
      /* Load *THIS_RTX.  */
      emit_move_insn (temp, gen_rtx_MEM (Pmode, this_rtx));
      /* Compute *THIS_RTX + VCALL_OFFSET.  */
      arm_split_constant (PLUS, Pmode, NULL_RTX, vcall_offset, temp, temp,
			  false);
      /* Compute *(*THIS_RTX + VCALL_OFFSET).  */
      emit_move_insn (temp, gen_rtx_MEM (Pmode, temp));
      emit_insn (gen_add3_insn (this_rtx, this_rtx, temp));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  rtx funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
  rtx_insn * insn = emit_call_insn (gen_sibcall (funexp, const0_rtx, NULL_RTX));
  SIBLING_CALL_P (insn) = 1;

  insn = get_insns ();
  shorten_branches (insn);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();

  /* Stop pretending this is a post-reload pass.  */
  reload_completed = 0;
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */

static void
arm_output_mi_thunk (FILE *file, tree thunk, HOST_WIDE_INT delta,
		     HOST_WIDE_INT vcall_offset, tree function)
{
  if (TARGET_32BIT)
    arm32_output_mi_thunk (file, thunk, delta, vcall_offset, function);
  else
    arm_thumb1_mi_thunk (file, thunk, delta, vcall_offset, function);
}

int
arm_emit_vector_const (FILE *file, rtx x)
{
  int i;
  const char * pattern;

  gcc_assert (GET_CODE (x) == CONST_VECTOR);

  switch (GET_MODE (x))
    {
    case E_V2SImode: pattern = "%08x"; break;
    case E_V4HImode: pattern = "%04x"; break;
    case E_V8QImode: pattern = "%02x"; break;
    default:       gcc_unreachable ();
    }

  fprintf (file, "0x");
  for (i = CONST_VECTOR_NUNITS (x); i--;)
    {
      rtx element;

      element = CONST_VECTOR_ELT (x, i);
      fprintf (file, pattern, INTVAL (element));
    }

  return 1;
}

/* Emit a fp16 constant appropriately padded to occupy a 4-byte word.
   HFmode constant pool entries are actually loaded with ldr.  */
void
arm_emit_fp16_const (rtx c)
{
  long bits;

  bits = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (c), HFmode);
  if (WORDS_BIG_ENDIAN)
    assemble_zeros (2);
  assemble_integer (GEN_INT (bits), 2, BITS_PER_WORD, 1);
  if (!WORDS_BIG_ENDIAN)
    assemble_zeros (2);
}

const char *
arm_output_load_gr (rtx *operands)
{
  rtx reg;
  rtx offset;
  rtx wcgr;
  rtx sum;

  if (!MEM_P (operands [1])
      || GET_CODE (sum = XEXP (operands [1], 0)) != PLUS
      || !REG_P (reg = XEXP (sum, 0))
      || !CONST_INT_P (offset = XEXP (sum, 1))
      || ((INTVAL (offset) < 1024) && (INTVAL (offset) > -1024)))
    return "wldrw%?\t%0, %1";

  /* Fix up an out-of-range load of a GR register.  */
  output_asm_insn ("str%?\t%0, [sp, #-4]!\t@ Start of GR load expansion", & reg);
  wcgr = operands[0];
  operands[0] = reg;
  output_asm_insn ("ldr%?\t%0, %1", operands);

  operands[0] = wcgr;
  operands[1] = reg;
  output_asm_insn ("tmcr%?\t%0, %1", operands);
  output_asm_insn ("ldr%?\t%0, [sp], #4\t@ End of GR load expansion", & reg);

  return "";
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.

   On the ARM, PRETEND_SIZE is set in order to have the prologue push the last
   named arg and all anonymous args onto the stack.
   XXX I know the prologue shouldn't be pushing registers, but it is faster
   that way.  */

static void
arm_setup_incoming_varargs (cumulative_args_t pcum_v,
			    machine_mode mode,
			    tree type,
			    int *pretend_size,
			    int second_time ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  int nregs;

  cfun->machine->uses_anonymous_args = 1;
  if (pcum->pcs_variant <= ARM_PCS_AAPCS_LOCAL)
    {
      nregs = pcum->aapcs_ncrn;
      if (nregs & 1)
	{
	  int res = arm_needs_doubleword_align (mode, type);
	  if (res < 0 && warn_psabi)
	    inform (input_location, "parameter passing for argument of "
		    "type %qT changed in GCC 7.1", type);
	  else if (res > 0)
	    nregs++;
	}
    }
  else
    nregs = pcum->nregs;

  if (nregs < NUM_ARG_REGS)
    *pretend_size = (NUM_ARG_REGS - nregs) * UNITS_PER_WORD;
}

/* We can't rely on the caller doing the proper promotion when
   using APCS or ATPCS.  */

static bool
arm_promote_prototypes (const_tree t ATTRIBUTE_UNUSED)
{
    return !TARGET_AAPCS_BASED;
}

static machine_mode
arm_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
                           machine_mode mode,
                           int *punsignedp ATTRIBUTE_UNUSED,
                           const_tree fntype ATTRIBUTE_UNUSED,
                           int for_return ATTRIBUTE_UNUSED)
{
  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) < 4)
    return SImode;

  return mode;
}


static bool
arm_default_short_enums (void)
{
  return ARM_DEFAULT_SHORT_ENUMS;
}


/* AAPCS requires that anonymous bitfields affect structure alignment.  */

static bool
arm_align_anon_bitfield (void)
{
  return TARGET_AAPCS_BASED;
}


/* The generic C++ ABI says 64-bit (long long).  The EABI says 32-bit.  */

static tree
arm_cxx_guard_type (void)
{
  return TARGET_AAPCS_BASED ? integer_type_node : long_long_integer_type_node;
}


/* The EABI says test the least significant bit of a guard variable.  */

static bool
arm_cxx_guard_mask_bit (void)
{
  return TARGET_AAPCS_BASED;
}


/* The EABI specifies that all array cookies are 8 bytes long.  */

static tree
arm_get_cookie_size (tree type)
{
  tree size;

  if (!TARGET_AAPCS_BASED)
    return default_cxx_get_cookie_size (type);

  size = build_int_cst (sizetype, 8);
  return size;
}


/* The EABI says that array cookies should also contain the element size.  */

static bool
arm_cookie_has_size (void)
{
  return TARGET_AAPCS_BASED;
}


/* The EABI says constructors and destructors should return a pointer to
   the object constructed/destroyed.  */

static bool
arm_cxx_cdtor_returns_this (void)
{
  return TARGET_AAPCS_BASED;
}

/* The EABI says that an inline function may never be the key
   method.  */

static bool
arm_cxx_key_method_may_be_inline (void)
{
  return !TARGET_AAPCS_BASED;
}

static void
arm_cxx_determine_class_data_visibility (tree decl)
{
  if (!TARGET_AAPCS_BASED
      || !TARGET_DLLIMPORT_DECL_ATTRIBUTES)
    return;

  /* In general, \S 3.2.5.5 of the ARM EABI requires that class data
     is exported.  However, on systems without dynamic vague linkage,
     \S 3.2.5.6 says that COMDAT class data has hidden linkage.  */
  if (!TARGET_ARM_DYNAMIC_VAGUE_LINKAGE_P && DECL_COMDAT (decl))
    DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
  else
    DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;
}

static bool
arm_cxx_class_data_always_comdat (void)
{
  /* \S 3.2.5.4 of the ARM C++ ABI says that class data only have
     vague linkage if the class has no key function.  */
  return !TARGET_AAPCS_BASED;
}


/* The EABI says __aeabi_atexit should be used to register static
   destructors.  */

static bool
arm_cxx_use_aeabi_atexit (void)
{
  return TARGET_AAPCS_BASED;
}


void
arm_set_return_address (rtx source, rtx scratch)
{
  arm_stack_offsets *offsets;
  HOST_WIDE_INT delta;
  rtx addr, mem;
  unsigned long saved_regs;

  offsets = arm_get_frame_offsets ();
  saved_regs = offsets->saved_regs_mask;

  if ((saved_regs & (1 << LR_REGNUM)) == 0)
    emit_move_insn (gen_rtx_REG (Pmode, LR_REGNUM), source);
  else
    {
      if (frame_pointer_needed)
	addr = plus_constant (Pmode, hard_frame_pointer_rtx, -4);
      else
	{
	  /* LR will be the first saved register.  */
	  delta = offsets->outgoing_args - (offsets->frame + 4);


	  if (delta >= 4096)
	    {
	      emit_insn (gen_addsi3 (scratch, stack_pointer_rtx,
				     GEN_INT (delta & ~4095)));
	      addr = scratch;
	      delta &= 4095;
	    }
	  else
	    addr = stack_pointer_rtx;

	  addr = plus_constant (Pmode, addr, delta);
	}

      /* The store needs to be marked to prevent DSE from deleting
	 it as dead if it is based on fp.  */
      mem = gen_frame_mem (Pmode, addr);
      MEM_VOLATILE_P (mem) = true;
      emit_move_insn (mem, source);
    }
}


void
thumb_set_return_address (rtx source, rtx scratch)
{
  arm_stack_offsets *offsets;
  HOST_WIDE_INT delta;
  HOST_WIDE_INT limit;
  int reg;
  rtx addr, mem;
  unsigned long mask;

  emit_use (source);

  offsets = arm_get_frame_offsets ();
  mask = offsets->saved_regs_mask;
  if (mask & (1 << LR_REGNUM))
    {
      limit = 1024;
      /* Find the saved regs.  */
      if (frame_pointer_needed)
	{
	  delta = offsets->soft_frame - offsets->saved_args;
	  reg = THUMB_HARD_FRAME_POINTER_REGNUM;
	  if (TARGET_THUMB1)
	    limit = 128;
	}
      else
	{
	  delta = offsets->outgoing_args - offsets->saved_args;
	  reg = SP_REGNUM;
	}
      /* Allow for the stack frame.  */
      if (TARGET_THUMB1 && TARGET_BACKTRACE)
	delta -= 16;
      /* The link register is always the first saved register.  */
      delta -= 4;

      /* Construct the address.  */
      addr = gen_rtx_REG (SImode, reg);
      if (delta > limit)
	{
	  emit_insn (gen_movsi (scratch, GEN_INT (delta)));
	  emit_insn (gen_addsi3 (scratch, scratch, stack_pointer_rtx));
	  addr = scratch;
	}
      else
	addr = plus_constant (Pmode, addr, delta);

      /* The store needs to be marked to prevent DSE from deleting
	 it as dead if it is based on fp.  */
      mem = gen_frame_mem (Pmode, addr);
      MEM_VOLATILE_P (mem) = true;
      emit_move_insn (mem, source);
    }
  else
    emit_move_insn (gen_rtx_REG (Pmode, LR_REGNUM), source);
}

/* Implements target hook vector_mode_supported_p.  */
bool
arm_vector_mode_supported_p (machine_mode mode)
{
  /* Neon also supports V2SImode, etc. listed in the clause below.  */
  if (TARGET_NEON && (mode == V2SFmode || mode == V4SImode || mode == V8HImode
      || mode == V4HFmode || mode == V16QImode || mode == V4SFmode
      || mode == V2DImode || mode == V8HFmode))
    return true;

  if ((TARGET_NEON || TARGET_IWMMXT)
      && ((mode == V2SImode)
	  || (mode == V4HImode)
	  || (mode == V8QImode)))
    return true;

  if (TARGET_INT_SIMD && (mode == V4UQQmode || mode == V4QQmode
      || mode == V2UHQmode || mode == V2HQmode || mode == V2UHAmode
      || mode == V2HAmode))
    return true;

  return false;
}

/* Implements target hook array_mode_supported_p.  */

static bool
arm_array_mode_supported_p (machine_mode mode,
			    unsigned HOST_WIDE_INT nelems)
{
  /* We don't want to enable interleaved loads and stores for BYTES_BIG_ENDIAN
     for now, as the lane-swapping logic needs to be extended in the expanders.
     See PR target/82518.  */
  if (TARGET_NEON && !BYTES_BIG_ENDIAN
      && (VALID_NEON_DREG_MODE (mode) || VALID_NEON_QREG_MODE (mode))
      && (nelems >= 2 && nelems <= 4))
    return true;

  return false;
}

/* Use the option -mvectorize-with-neon-double to override the use of quardword
   registers when autovectorizing for Neon, at least until multiple vector
   widths are supported properly by the middle-end.  */

static machine_mode
arm_preferred_simd_mode (scalar_mode mode)
{
  if (TARGET_NEON)
    switch (mode)
      {
      case E_SFmode:
	return TARGET_NEON_VECTORIZE_DOUBLE ? V2SFmode : V4SFmode;
      case E_SImode:
	return TARGET_NEON_VECTORIZE_DOUBLE ? V2SImode : V4SImode;
      case E_HImode:
	return TARGET_NEON_VECTORIZE_DOUBLE ? V4HImode : V8HImode;
      case E_QImode:
	return TARGET_NEON_VECTORIZE_DOUBLE ? V8QImode : V16QImode;
      case E_DImode:
	if (!TARGET_NEON_VECTORIZE_DOUBLE)
	  return V2DImode;
	break;

      default:;
      }

  if (TARGET_REALLY_IWMMXT)
    switch (mode)
      {
      case E_SImode:
	return V2SImode;
      case E_HImode:
	return V4HImode;
      case E_QImode:
	return V8QImode;

      default:;
      }

  return word_mode;
}

/* Implement TARGET_CLASS_LIKELY_SPILLED_P.

   We need to define this for LO_REGS on Thumb-1.  Otherwise we can end up
   using r0-r4 for function arguments, r7 for the stack frame and don't have
   enough left over to do doubleword arithmetic.  For Thumb-2 all the
   potentially problematic instructions accept high registers so this is not
   necessary.  Care needs to be taken to avoid adding new Thumb-2 patterns
   that require many low registers.  */
static bool
arm_class_likely_spilled_p (reg_class_t rclass)
{
  if ((TARGET_THUMB1 && rclass == LO_REGS)
      || rclass  == CC_REG)
    return true;

  return false;
}

/* Implements target hook small_register_classes_for_mode_p.  */
bool
arm_small_register_classes_for_mode_p (machine_mode mode ATTRIBUTE_UNUSED)
{
  return TARGET_THUMB1;
}

/* Implement TARGET_SHIFT_TRUNCATION_MASK.  SImode shifts use normal
   ARM insns and therefore guarantee that the shift count is modulo 256.
   DImode shifts (those implemented by lib1funcs.S or by optabs.c)
   guarantee no particular behavior for out-of-range counts.  */

static unsigned HOST_WIDE_INT
arm_shift_truncation_mask (machine_mode mode)
{
  return mode == SImode ? 255 : 0;
}


/* Map internal gcc register numbers to DWARF2 register numbers.  */

unsigned int
arm_dbx_register_number (unsigned int regno)
{
  if (regno < 16)
    return regno;

  if (IS_VFP_REGNUM (regno))
    {
      /* See comment in arm_dwarf_register_span.  */
      if (VFP_REGNO_OK_FOR_SINGLE (regno))
	return 64 + regno - FIRST_VFP_REGNUM;
      else
	return 256 + (regno - FIRST_VFP_REGNUM) / 2;
    }

  if (IS_IWMMXT_GR_REGNUM (regno))
    return 104 + regno - FIRST_IWMMXT_GR_REGNUM;

  if (IS_IWMMXT_REGNUM (regno))
    return 112 + regno - FIRST_IWMMXT_REGNUM;

  return DWARF_FRAME_REGISTERS;
}

/* Dwarf models VFPv3 registers as 32 64-bit registers.
   GCC models tham as 64 32-bit registers, so we need to describe this to
   the DWARF generation code.  Other registers can use the default.  */
static rtx
arm_dwarf_register_span (rtx rtl)
{
  machine_mode mode;
  unsigned regno;
  rtx parts[16];
  int nregs;
  int i;

  regno = REGNO (rtl);
  if (!IS_VFP_REGNUM (regno))
    return NULL_RTX;

  /* XXX FIXME: The EABI defines two VFP register ranges:
	64-95: Legacy VFPv2 numbering for S0-S31 (obsolescent)
	256-287: D0-D31
     The recommended encoding for S0-S31 is a DW_OP_bit_piece of the
     corresponding D register.  Until GDB supports this, we shall use the
     legacy encodings.  We also use these encodings for D0-D15 for
     compatibility with older debuggers.  */
  mode = GET_MODE (rtl);
  if (GET_MODE_SIZE (mode) < 8)
    return NULL_RTX;

  if (VFP_REGNO_OK_FOR_SINGLE (regno))
    {
      nregs = GET_MODE_SIZE (mode) / 4;
      for (i = 0; i < nregs; i += 2)
	if (TARGET_BIG_END)
	  {
	    parts[i] = gen_rtx_REG (SImode, regno + i + 1);
	    parts[i + 1] = gen_rtx_REG (SImode, regno + i);
	  }
	else
	  {
	    parts[i] = gen_rtx_REG (SImode, regno + i);
	    parts[i + 1] = gen_rtx_REG (SImode, regno + i + 1);
	  }
    }
  else
    {
      nregs = GET_MODE_SIZE (mode) / 8;
      for (i = 0; i < nregs; i++)
	parts[i] = gen_rtx_REG (DImode, regno + i);
    }

  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nregs , parts));
}

#if ARM_UNWIND_INFO
/* Emit unwind directives for a store-multiple instruction or stack pointer
   push during alignment.
   These should only ever be generated by the function prologue code, so
   expect them to have a particular form.
   The store-multiple instruction sometimes pushes pc as the last register,
   although it should not be tracked into unwind information, or for -Os
   sometimes pushes some dummy registers before first register that needs
   to be tracked in unwind information; such dummy registers are there just
   to avoid separate stack adjustment, and will not be restored in the
   epilogue.  */

static void
arm_unwind_emit_sequence (FILE * asm_out_file, rtx p)
{
  int i;
  HOST_WIDE_INT offset;
  HOST_WIDE_INT nregs;
  int reg_size;
  unsigned reg;
  unsigned lastreg;
  unsigned padfirst = 0, padlast = 0;
  rtx e;

  e = XVECEXP (p, 0, 0);
  gcc_assert (GET_CODE (e) == SET);

  /* First insn will adjust the stack pointer.  */
  gcc_assert (GET_CODE (e) == SET
	      && REG_P (SET_DEST (e))
	      && REGNO (SET_DEST (e)) == SP_REGNUM
	      && GET_CODE (SET_SRC (e)) == PLUS);

  offset = -INTVAL (XEXP (SET_SRC (e), 1));
  nregs = XVECLEN (p, 0) - 1;
  gcc_assert (nregs);

  reg = REGNO (SET_SRC (XVECEXP (p, 0, 1)));
  if (reg < 16)
    {
      /* For -Os dummy registers can be pushed at the beginning to
	 avoid separate stack pointer adjustment.  */
      e = XVECEXP (p, 0, 1);
      e = XEXP (SET_DEST (e), 0);
      if (GET_CODE (e) == PLUS)
	padfirst = INTVAL (XEXP (e, 1));
      gcc_assert (padfirst == 0 || optimize_size);
      /* The function prologue may also push pc, but not annotate it as it is
	 never restored.  We turn this into a stack pointer adjustment.  */
      e = XVECEXP (p, 0, nregs);
      e = XEXP (SET_DEST (e), 0);
      if (GET_CODE (e) == PLUS)
	padlast = offset - INTVAL (XEXP (e, 1)) - 4;
      else
	padlast = offset - 4;
      gcc_assert (padlast == 0 || padlast == 4);
      if (padlast == 4)
	fprintf (asm_out_file, "\t.pad #4\n");
      reg_size = 4;
      fprintf (asm_out_file, "\t.save {");
    }
  else if (IS_VFP_REGNUM (reg))
    {
      reg_size = 8;
      fprintf (asm_out_file, "\t.vsave {");
    }
  else
    /* Unknown register type.  */
    gcc_unreachable ();

  /* If the stack increment doesn't match the size of the saved registers,
     something has gone horribly wrong.  */
  gcc_assert (offset == padfirst + nregs * reg_size + padlast);

  offset = padfirst;
  lastreg = 0;
  /* The remaining insns will describe the stores.  */
  for (i = 1; i <= nregs; i++)
    {
      /* Expect (set (mem <addr>) (reg)).
         Where <addr> is (reg:SP) or (plus (reg:SP) (const_int)).  */
      e = XVECEXP (p, 0, i);
      gcc_assert (GET_CODE (e) == SET
		  && MEM_P (SET_DEST (e))
		  && REG_P (SET_SRC (e)));

      reg = REGNO (SET_SRC (e));
      gcc_assert (reg >= lastreg);

      if (i != 1)
	fprintf (asm_out_file, ", ");
      /* We can't use %r for vfp because we need to use the
	 double precision register names.  */
      if (IS_VFP_REGNUM (reg))
	asm_fprintf (asm_out_file, "d%d", (reg - FIRST_VFP_REGNUM) / 2);
      else
	asm_fprintf (asm_out_file, "%r", reg);

      if (flag_checking)
	{
	  /* Check that the addresses are consecutive.  */
	  e = XEXP (SET_DEST (e), 0);
	  if (GET_CODE (e) == PLUS)
	    gcc_assert (REG_P (XEXP (e, 0))
			&& REGNO (XEXP (e, 0)) == SP_REGNUM
			&& CONST_INT_P (XEXP (e, 1))
			&& offset == INTVAL (XEXP (e, 1)));
	  else
	    gcc_assert (i == 1
			&& REG_P (e)
			&& REGNO (e) == SP_REGNUM);
	  offset += reg_size;
	}
    }
  fprintf (asm_out_file, "}\n");
  if (padfirst)
    fprintf (asm_out_file, "\t.pad #%d\n", padfirst);
}

/*  Emit unwind directives for a SET.  */

static void
arm_unwind_emit_set (FILE * asm_out_file, rtx p)
{
  rtx e0;
  rtx e1;
  unsigned reg;

  e0 = XEXP (p, 0);
  e1 = XEXP (p, 1);
  switch (GET_CODE (e0))
    {
    case MEM:
      /* Pushing a single register.  */
      if (GET_CODE (XEXP (e0, 0)) != PRE_DEC
	  || !REG_P (XEXP (XEXP (e0, 0), 0))
	  || REGNO (XEXP (XEXP (e0, 0), 0)) != SP_REGNUM)
	abort ();

      asm_fprintf (asm_out_file, "\t.save ");
      if (IS_VFP_REGNUM (REGNO (e1)))
	asm_fprintf(asm_out_file, "{d%d}\n",
		    (REGNO (e1) - FIRST_VFP_REGNUM) / 2);
      else
	asm_fprintf(asm_out_file, "{%r}\n", REGNO (e1));
      break;

    case REG:
      if (REGNO (e0) == SP_REGNUM)
	{
	  /* A stack increment.  */
	  if (GET_CODE (e1) != PLUS
	      || !REG_P (XEXP (e1, 0))
	      || REGNO (XEXP (e1, 0)) != SP_REGNUM
	      || !CONST_INT_P (XEXP (e1, 1)))
	    abort ();

	  asm_fprintf (asm_out_file, "\t.pad #%wd\n",
		       -INTVAL (XEXP (e1, 1)));
	}
      else if (REGNO (e0) == HARD_FRAME_POINTER_REGNUM)
	{
	  HOST_WIDE_INT offset;

	  if (GET_CODE (e1) == PLUS)
	    {
	      if (!REG_P (XEXP (e1, 0))
		  || !CONST_INT_P (XEXP (e1, 1)))
		abort ();
	      reg = REGNO (XEXP (e1, 0));
	      offset = INTVAL (XEXP (e1, 1));
	      asm_fprintf (asm_out_file, "\t.setfp %r, %r, #%wd\n",
			   HARD_FRAME_POINTER_REGNUM, reg,
			   offset);
	    }
	  else if (REG_P (e1))
	    {
	      reg = REGNO (e1);
	      asm_fprintf (asm_out_file, "\t.setfp %r, %r\n",
			   HARD_FRAME_POINTER_REGNUM, reg);
	    }
	  else
	    abort ();
	}
      else if (REG_P (e1) && REGNO (e1) == SP_REGNUM)
	{
	  /* Move from sp to reg.  */
	  asm_fprintf (asm_out_file, "\t.movsp %r\n", REGNO (e0));
	}
     else if (GET_CODE (e1) == PLUS
	      && REG_P (XEXP (e1, 0))
	      && REGNO (XEXP (e1, 0)) == SP_REGNUM
	      && CONST_INT_P (XEXP (e1, 1)))
	{
	  /* Set reg to offset from sp.  */
	  asm_fprintf (asm_out_file, "\t.movsp %r, #%d\n",
		       REGNO (e0), (int)INTVAL(XEXP (e1, 1)));
	}
      else
	abort ();
      break;

    default:
      abort ();
    }
}


/* Emit unwind directives for the given insn.  */

static void
arm_unwind_emit (FILE * asm_out_file, rtx_insn *insn)
{
  rtx note, pat;
  bool handled_one = false;

  if (arm_except_unwind_info (&global_options) != UI_TARGET)
    return;

  if (!(flag_unwind_tables || crtl->uses_eh_lsda)
      && (TREE_NOTHROW (current_function_decl)
	  || crtl->all_throwers_are_sibcalls))
    return;

  if (NOTE_P (insn) || !RTX_FRAME_RELATED_P (insn))
    return;

  for (note = REG_NOTES (insn); note ; note = XEXP (note, 1))
    {
      switch (REG_NOTE_KIND (note))
	{
	case REG_FRAME_RELATED_EXPR:
	  pat = XEXP (note, 0);
	  goto found;

	case REG_CFA_REGISTER:
	  pat = XEXP (note, 0);
	  if (pat == NULL)
	    {
	      pat = PATTERN (insn);
	      if (GET_CODE (pat) == PARALLEL)
		pat = XVECEXP (pat, 0, 0);
	    }

	  /* Only emitted for IS_STACKALIGN re-alignment.  */
	  {
	    rtx dest, src;
	    unsigned reg;

	    src = SET_SRC (pat);
	    dest = SET_DEST (pat);

	    gcc_assert (src == stack_pointer_rtx);
	    reg = REGNO (dest);
	    asm_fprintf (asm_out_file, "\t.unwind_raw 0, 0x%x @ vsp = r%d\n",
			 reg + 0x90, reg);
	  }
	  handled_one = true;
	  break;

	/* The INSN is generated in epilogue.  It is set as RTX_FRAME_RELATED_P
	   to get correct dwarf information for shrink-wrap.  We should not
	   emit unwind information for it because these are used either for
	   pretend arguments or notes to adjust sp and restore registers from
	   stack.  */
	case REG_CFA_DEF_CFA:
	case REG_CFA_ADJUST_CFA:
	case REG_CFA_RESTORE:
	  return;

	case REG_CFA_EXPRESSION:
	case REG_CFA_OFFSET:
	  /* ??? Only handling here what we actually emit.  */
	  gcc_unreachable ();

	default:
	  break;
	}
    }
  if (handled_one)
    return;
  pat = PATTERN (insn);
 found:

  switch (GET_CODE (pat))
    {
    case SET:
      arm_unwind_emit_set (asm_out_file, pat);
      break;

    case SEQUENCE:
      /* Store multiple.  */
      arm_unwind_emit_sequence (asm_out_file, pat);
      break;

    default:
      abort();
    }
}


/* Output a reference from a function exception table to the type_info
   object X.  The EABI specifies that the symbol should be relocated by
   an R_ARM_TARGET2 relocation.  */

static bool
arm_output_ttype (rtx x)
{
  fputs ("\t.word\t", asm_out_file);
  output_addr_const (asm_out_file, x);
  /* Use special relocations for symbol references.  */
  if (!CONST_INT_P (x))
    fputs ("(TARGET2)", asm_out_file);
  fputc ('\n', asm_out_file);

  return TRUE;
}

/* Implement TARGET_ASM_EMIT_EXCEPT_PERSONALITY.  */

static void
arm_asm_emit_except_personality (rtx personality)
{
  fputs ("\t.personality\t", asm_out_file);
  output_addr_const (asm_out_file, personality);
  fputc ('\n', asm_out_file);
}
#endif /* ARM_UNWIND_INFO */

/* Implement TARGET_ASM_INITIALIZE_SECTIONS.  */

static void
arm_asm_init_sections (void)
{
#if ARM_UNWIND_INFO
  exception_section = get_unnamed_section (0, output_section_asm_op,
					   "\t.handlerdata");
#endif /* ARM_UNWIND_INFO */

#ifdef OBJECT_FORMAT_ELF
  if (target_pure_code)
    text_section->unnamed.data = "\t.section .text,\"0x20000006\",%progbits";
#endif
}

/* Output unwind directives for the start/end of a function.  */

void
arm_output_fn_unwind (FILE * f, bool prologue)
{
  if (arm_except_unwind_info (&global_options) != UI_TARGET)
    return;

  if (prologue)
    fputs ("\t.fnstart\n", f);
  else
    {
      /* If this function will never be unwound, then mark it as such.
         The came condition is used in arm_unwind_emit to suppress
	 the frame annotations.  */
      if (!(flag_unwind_tables || crtl->uses_eh_lsda)
	  && (TREE_NOTHROW (current_function_decl)
	      || crtl->all_throwers_are_sibcalls))
	fputs("\t.cantunwind\n", f);

      fputs ("\t.fnend\n", f);
    }
}

static bool
arm_emit_tls_decoration (FILE *fp, rtx x)
{
  enum tls_reloc reloc;
  rtx val;

  val = XVECEXP (x, 0, 0);
  reloc = (enum tls_reloc) INTVAL (XVECEXP (x, 0, 1));

  output_addr_const (fp, val);

  switch (reloc)
    {
    case TLS_GD32:
      fputs ("(tlsgd)", fp);
      break;
    case TLS_LDM32:
      fputs ("(tlsldm)", fp);
      break;
    case TLS_LDO32:
      fputs ("(tlsldo)", fp);
      break;
    case TLS_IE32:
      fputs ("(gottpoff)", fp);
      break;
    case TLS_LE32:
      fputs ("(tpoff)", fp);
      break;
    case TLS_DESCSEQ:
      fputs ("(tlsdesc)", fp);
      break;
    default:
      gcc_unreachable ();
    }

  switch (reloc)
    {
    case TLS_GD32:
    case TLS_LDM32:
    case TLS_IE32:
    case TLS_DESCSEQ:
      fputs (" + (. - ", fp);
      output_addr_const (fp, XVECEXP (x, 0, 2));
      /* For DESCSEQ the 3rd operand encodes thumbness, and is added */
      fputs (reloc == TLS_DESCSEQ ? " + " : " - ", fp);
      output_addr_const (fp, XVECEXP (x, 0, 3));
      fputc (')', fp);
      break;
    default:
      break;
    }

  return TRUE;
}

/* ARM implementation of TARGET_ASM_OUTPUT_DWARF_DTPREL.  */

static void
arm_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  gcc_assert (size == 4);
  fputs ("\t.word\t", file);
  output_addr_const (file, x);
  fputs ("(tlsldo)", file);
}

/* Implement TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */

static bool
arm_output_addr_const_extra (FILE *fp, rtx x)
{
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return arm_emit_tls_decoration (fp, x);
  else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_PIC_LABEL)
    {
      char label[256];
      int labelno = INTVAL (XVECEXP (x, 0, 0));

      ASM_GENERATE_INTERNAL_LABEL (label, "LPIC", labelno);
      assemble_name_raw (fp, label);

      return TRUE;
    }
  else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_GOTSYM_OFF)
    {
      assemble_name (fp, "_GLOBAL_OFFSET_TABLE_");
      if (GOT_PCREL)
	fputs ("+.", fp);
      fputs ("-(", fp);
      output_addr_const (fp, XVECEXP (x, 0, 0));
      fputc (')', fp);
      return TRUE;
    }
  else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_SYMBOL_OFFSET)
    {
      output_addr_const (fp, XVECEXP (x, 0, 0));
      if (GOT_PCREL)
        fputs ("+.", fp);
      fputs ("-(", fp);
      output_addr_const (fp, XVECEXP (x, 0, 1));
      fputc (')', fp);
      return TRUE;
    }
  else if (GET_CODE (x) == CONST_VECTOR)
    return arm_emit_vector_const (fp, x);

  return FALSE;
}

/* Output assembly for a shift instruction.
   SET_FLAGS determines how the instruction modifies the condition codes.
   0 - Do not set condition codes.
   1 - Set condition codes.
   2 - Use smallest instruction.  */
const char *
arm_output_shift(rtx * operands, int set_flags)
{
  char pattern[100];
  static const char flag_chars[3] = {'?', '.', '!'};
  const char *shift;
  HOST_WIDE_INT val;
  char c;

  c = flag_chars[set_flags];
  shift = shift_op(operands[3], &val);
  if (shift)
    {
      if (val != -1)
	operands[2] = GEN_INT(val);
      sprintf (pattern, "%s%%%c\t%%0, %%1, %%2", shift, c);
    }
  else
    sprintf (pattern, "mov%%%c\t%%0, %%1", c);

  output_asm_insn (pattern, operands);
  return "";
}

/* Output assembly for a WMMX immediate shift instruction.  */
const char *
arm_output_iwmmxt_shift_immediate (const char *insn_name, rtx *operands, bool wror_or_wsra)
{
  int shift = INTVAL (operands[2]);
  char templ[50];
  machine_mode opmode = GET_MODE (operands[0]);

  gcc_assert (shift >= 0);

  /* If the shift value in the register versions is > 63 (for D qualifier),
     31 (for W qualifier) or 15 (for H qualifier).  */
  if (((opmode == V4HImode) && (shift > 15))
	|| ((opmode == V2SImode) && (shift > 31))
	|| ((opmode == DImode) && (shift > 63)))
  {
    if (wror_or_wsra)
      {
        sprintf (templ, "%s\t%%0, %%1, #%d", insn_name, 32);
        output_asm_insn (templ, operands);
        if (opmode == DImode)
          {
	    sprintf (templ, "%s\t%%0, %%0, #%d", insn_name, 32);
	    output_asm_insn (templ, operands);
          }
      }
    else
      {
        /* The destination register will contain all zeros.  */
        sprintf (templ, "wzero\t%%0");
        output_asm_insn (templ, operands);
      }
    return "";
  }

  if ((opmode == DImode) && (shift > 32))
    {
      sprintf (templ, "%s\t%%0, %%1, #%d", insn_name, 32);
      output_asm_insn (templ, operands);
      sprintf (templ, "%s\t%%0, %%0, #%d", insn_name, shift - 32);
      output_asm_insn (templ, operands);
    }
  else
    {
      sprintf (templ, "%s\t%%0, %%1, #%d", insn_name, shift);
      output_asm_insn (templ, operands);
    }
  return "";
}

/* Output assembly for a WMMX tinsr instruction.  */
const char *
arm_output_iwmmxt_tinsr (rtx *operands)
{
  int mask = INTVAL (operands[3]);
  int i;
  char templ[50];
  int units = mode_nunits[GET_MODE (operands[0])];
  gcc_assert ((mask & (mask - 1)) == 0);
  for (i = 0; i < units; ++i)
    {
      if ((mask & 0x01) == 1)
        {
          break;
        }
      mask >>= 1;
    }
  gcc_assert (i < units);
  {
    switch (GET_MODE (operands[0]))
      {
      case E_V8QImode:
	sprintf (templ, "tinsrb%%?\t%%0, %%2, #%d", i);
	break;
      case E_V4HImode:
	sprintf (templ, "tinsrh%%?\t%%0, %%2, #%d", i);
	break;
      case E_V2SImode:
	sprintf (templ, "tinsrw%%?\t%%0, %%2, #%d", i);
	break;
      default:
	gcc_unreachable ();
	break;
      }
    output_asm_insn (templ, operands);
  }
  return "";
}

/* Output a Thumb-1 casesi dispatch sequence.  */
const char *
thumb1_output_casesi (rtx *operands)
{
  rtx diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[0])));

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  switch (GET_MODE(diff_vec))
    {
    case E_QImode:
      return (ADDR_DIFF_VEC_FLAGS (diff_vec).offset_unsigned ?
	      "bl\t%___gnu_thumb1_case_uqi" : "bl\t%___gnu_thumb1_case_sqi");
    case E_HImode:
      return (ADDR_DIFF_VEC_FLAGS (diff_vec).offset_unsigned ?
	      "bl\t%___gnu_thumb1_case_uhi" : "bl\t%___gnu_thumb1_case_shi");
    case E_SImode:
      return "bl\t%___gnu_thumb1_case_si";
    default:
      gcc_unreachable ();
    }
}

/* Output a Thumb-2 casesi instruction.  */
const char *
thumb2_output_casesi (rtx *operands)
{
  rtx diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[2])));

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  output_asm_insn ("cmp\t%0, %1", operands);
  output_asm_insn ("bhi\t%l3", operands);
  switch (GET_MODE(diff_vec))
    {
    case E_QImode:
      return "tbb\t[%|pc, %0]";
    case E_HImode:
      return "tbh\t[%|pc, %0, lsl #1]";
    case E_SImode:
      if (flag_pic)
	{
	  output_asm_insn ("adr\t%4, %l2", operands);
	  output_asm_insn ("ldr\t%5, [%4, %0, lsl #2]", operands);
	  output_asm_insn ("add\t%4, %4, %5", operands);
	  return "bx\t%4";
	}
      else
	{
	  output_asm_insn ("adr\t%4, %l2", operands);
	  return "ldr\t%|pc, [%4, %0, lsl #2]";
	}
    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_SCHED_ISSUE_RATE.  Lookup the issue rate in the
   per-core tuning structs.  */
static int
arm_issue_rate (void)
{
  return current_tune->issue_rate;
}

/* Return how many instructions should scheduler lookahead to choose the
   best one.  */
static int
arm_first_cycle_multipass_dfa_lookahead (void)
{
  int issue_rate = arm_issue_rate ();

  return issue_rate > 1 && !sched_fusion ? issue_rate : 0;
}

/* Enable modeling of L2 auto-prefetcher.  */
static int
arm_first_cycle_multipass_dfa_lookahead_guard (rtx_insn *insn, int ready_index)
{
  return autopref_multipass_dfa_lookahead_guard (insn, ready_index);
}

const char *
arm_mangle_type (const_tree type)
{
  /* The ARM ABI documents (10th October 2008) say that "__va_list"
     has to be managled as if it is in the "std" namespace.  */
  if (TARGET_AAPCS_BASED
      && lang_hooks.types_compatible_p (CONST_CAST_TREE (type), va_list_type))
    return "St9__va_list";

  /* Half-precision float.  */
  if (TREE_CODE (type) == REAL_TYPE && TYPE_PRECISION (type) == 16)
    return "Dh";

  /* Try mangling as a Neon type, TYPE_NAME is non-NULL if this is a
     builtin type.  */
  if (TYPE_NAME (type) != NULL)
    return arm_mangle_builtin_type (type);

  /* Use the default mangling.  */
  return NULL;
}

/* Order of allocation of core registers for Thumb: this allocation is
   written over the corresponding initial entries of the array
   initialized with REG_ALLOC_ORDER.  We allocate all low registers
   first.  Saving and restoring a low register is usually cheaper than
   using a call-clobbered high register.  */

static const int thumb_core_reg_alloc_order[] =
{
   3,  2,  1,  0,  4,  5,  6,  7,
  12, 14,  8,  9, 10, 11
};

/* Adjust register allocation order when compiling for Thumb.  */

void
arm_order_regs_for_local_alloc (void)
{
  const int arm_reg_alloc_order[] = REG_ALLOC_ORDER;
  memcpy(reg_alloc_order, arm_reg_alloc_order, sizeof (reg_alloc_order));
  if (TARGET_THUMB)
    memcpy (reg_alloc_order, thumb_core_reg_alloc_order,
            sizeof (thumb_core_reg_alloc_order));
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.  */

bool
arm_frame_pointer_required (void)
{
  if (SUBTARGET_FRAME_POINTER_REQUIRED)
    return true;

  /* If the function receives nonlocal gotos, it needs to save the frame
     pointer in the nonlocal_goto_save_area object.  */
  if (cfun->has_nonlocal_label)
    return true;

  /* The frame pointer is required for non-leaf APCS frames.  */
  if (TARGET_ARM && TARGET_APCS_FRAME && !crtl->is_leaf)
    return true;

  /* If we are probing the stack in the prologue, we will have a faulting
     instruction prior to the stack adjustment and this requires a frame
     pointer if we want to catch the exception using the EABI unwinder.  */
  if (!IS_INTERRUPT (arm_current_func_type ())
      && (flag_stack_check == STATIC_BUILTIN_STACK_CHECK
	  || flag_stack_clash_protection)
      && arm_except_unwind_info (&global_options) == UI_TARGET
      && cfun->can_throw_non_call_exceptions)
    {
      HOST_WIDE_INT size = get_frame_size ();

      /* That's irrelevant if there is no stack adjustment.  */
      if (size <= 0)
	return false;

      /* That's relevant only if there is a stack probe.  */
      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  /* We don't have the final size of the frame so adjust.  */
	  size += 32 * UNITS_PER_WORD;
	  if (size > PROBE_INTERVAL && size > get_stack_check_protect ())
	    return true;
	}
      else
	return true;
    }

  return false;
}

/* Only thumb1 can't support conditional execution, so return true if
   the target is not thumb1.  */
static bool
arm_have_conditional_execution (void)
{
  return !TARGET_THUMB1;
}

/* The AAPCS sets the maximum alignment of a vector to 64 bits.  */
static HOST_WIDE_INT
arm_vector_alignment (const_tree type)
{
  HOST_WIDE_INT align = tree_to_shwi (TYPE_SIZE (type));

  if (TARGET_AAPCS_BASED)
    align = MIN (align, 64);

  return align;
}

static void
arm_autovectorize_vector_sizes (vector_sizes *sizes)
{
  if (!TARGET_NEON_VECTORIZE_DOUBLE)
    {
      sizes->safe_push (16);
      sizes->safe_push (8);
    }
}

static bool
arm_vector_alignment_reachable (const_tree type, bool is_packed)
{
  /* Vectors which aren't in packed structures will not be less aligned than
     the natural alignment of their element type, so this is safe.  */
  if (TARGET_NEON && !BYTES_BIG_ENDIAN && unaligned_access)
    return !is_packed;

  return default_builtin_vector_alignment_reachable (type, is_packed);
}

static bool
arm_builtin_support_vector_misalignment (machine_mode mode,
					 const_tree type, int misalignment,
					 bool is_packed)
{
  if (TARGET_NEON && !BYTES_BIG_ENDIAN && unaligned_access)
    {
      HOST_WIDE_INT align = TYPE_ALIGN_UNIT (type);

      if (is_packed)
        return align == 1;

      /* If the misalignment is unknown, we should be able to handle the access
	 so long as it is not to a member of a packed data structure.  */
      if (misalignment == -1)
        return true;

      /* Return true if the misalignment is a multiple of the natural alignment
         of the vector's element type.  This is probably always going to be
	 true in practice, since we've already established that this isn't a
	 packed access.  */
      return ((misalignment % align) == 0);
    }

  return default_builtin_support_vector_misalignment (mode, type, misalignment,
						      is_packed);
}

static void
arm_conditional_register_usage (void)
{
  int regno;

  if (TARGET_THUMB1 && optimize_size)
    {
      /* When optimizing for size on Thumb-1, it's better not
        to use the HI regs, because of the overhead of
        stacking them.  */
      for (regno = FIRST_HI_REGNUM; regno <= LAST_HI_REGNUM; ++regno)
	fixed_regs[regno] = call_used_regs[regno] = 1;
    }

  /* The link register can be clobbered by any branch insn,
     but we have no way to track that at present, so mark
     it as unavailable.  */
  if (TARGET_THUMB1)
    fixed_regs[LR_REGNUM] = call_used_regs[LR_REGNUM] = 1;

  if (TARGET_32BIT && TARGET_HARD_FLOAT)
    {
      /* VFPv3 registers are disabled when earlier VFP
	 versions are selected due to the definition of
	 LAST_VFP_REGNUM.  */
      for (regno = FIRST_VFP_REGNUM;
	   regno <= LAST_VFP_REGNUM; ++ regno)
	{
	  fixed_regs[regno] = 0;
	  call_used_regs[regno] = regno < FIRST_VFP_REGNUM + 16
	    || regno >= FIRST_VFP_REGNUM + 32;
	}
    }

  if (TARGET_REALLY_IWMMXT)
    {
      regno = FIRST_IWMMXT_GR_REGNUM;
      /* The 2002/10/09 revision of the XScale ABI has wCG0
         and wCG1 as call-preserved registers.  The 2002/11/21
         revision changed this so that all wCG registers are
         scratch registers.  */
      for (regno = FIRST_IWMMXT_GR_REGNUM;
	   regno <= LAST_IWMMXT_GR_REGNUM; ++ regno)
	fixed_regs[regno] = 0;
      /* The XScale ABI has wR0 - wR9 as scratch registers,
	 the rest as call-preserved registers.  */
      for (regno = FIRST_IWMMXT_REGNUM;
	   regno <= LAST_IWMMXT_REGNUM; ++ regno)
	{
	  fixed_regs[regno] = 0;
	  call_used_regs[regno] = regno < FIRST_IWMMXT_REGNUM + 10;
	}
    }

  if ((unsigned) PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
  else if (TARGET_APCS_STACK)
    {
      fixed_regs[10]     = 1;
      call_used_regs[10] = 1;
    }
  /* -mcaller-super-interworking reserves r11 for calls to
     _interwork_r11_call_via_rN().  Making the register global
     is an easy way of ensuring that it remains valid for all
     calls.  */
  if (TARGET_APCS_FRAME || TARGET_CALLER_INTERWORKING
      || TARGET_TPCS_FRAME || TARGET_TPCS_LEAF_FRAME)
    {
      fixed_regs[ARM_HARD_FRAME_POINTER_REGNUM] = 1;
      call_used_regs[ARM_HARD_FRAME_POINTER_REGNUM] = 1;
      if (TARGET_CALLER_INTERWORKING)
	global_regs[ARM_HARD_FRAME_POINTER_REGNUM] = 1;
    }
  SUBTARGET_CONDITIONAL_REGISTER_USAGE
}

static reg_class_t
arm_preferred_rename_class (reg_class_t rclass)
{
  /* Thumb-2 instructions using LO_REGS may be smaller than instructions
     using GENERIC_REGS.  During register rename pass, we prefer LO_REGS,
     and code size can be reduced.  */
  if (TARGET_THUMB2 && rclass == GENERAL_REGS)
    return LO_REGS;
  else
    return NO_REGS;
}

/* Compute the attribute "length" of insn "*push_multi".
   So this function MUST be kept in sync with that insn pattern.  */
int
arm_attr_length_push_multi(rtx parallel_op, rtx first_op)
{
  int i, regno, hi_reg;
  int num_saves = XVECLEN (parallel_op, 0);

  /* ARM mode.  */
  if (TARGET_ARM)
    return 4;
  /* Thumb1 mode.  */
  if (TARGET_THUMB1)
    return 2;

  /* Thumb2 mode.  */
  regno = REGNO (first_op);
  /* For PUSH/STM under Thumb2 mode, we can use 16-bit encodings if the register
     list is 8-bit.  Normally this means all registers in the list must be
     LO_REGS, that is (R0 -R7).  If any HI_REGS used, then we must use 32-bit
     encodings.  There is one exception for PUSH that LR in HI_REGS can be used
     with 16-bit encoding.  */
  hi_reg = (REGNO_REG_CLASS (regno) == HI_REGS) && (regno != LR_REGNUM);
  for (i = 1; i < num_saves && !hi_reg; i++)
    {
      regno = REGNO (XEXP (XVECEXP (parallel_op, 0, i), 0));
      hi_reg |= (REGNO_REG_CLASS (regno) == HI_REGS) && (regno != LR_REGNUM);
    }

  if (!hi_reg)
    return 2;
  return 4;
}

/* Compute the attribute "length" of insn.  Currently, this function is used
   for "*load_multiple_with_writeback", "*pop_multiple_with_return" and
   "*pop_multiple_with_writeback_and_return".  OPERANDS is the toplevel PARALLEL
   rtx, RETURN_PC is true if OPERANDS contains return insn.  WRITE_BACK_P is
   true if OPERANDS contains insn which explicit updates base register.  */

int
arm_attr_length_pop_multi (rtx *operands, bool return_pc, bool write_back_p)
{
  /* ARM mode.  */
  if (TARGET_ARM)
    return 4;
  /* Thumb1 mode.  */
  if (TARGET_THUMB1)
    return 2;

  rtx parallel_op = operands[0];
  /* Initialize to elements number of PARALLEL.  */
  unsigned indx = XVECLEN (parallel_op, 0) - 1;
  /* Initialize the value to base register.  */
  unsigned regno = REGNO (operands[1]);
  /* Skip return and write back pattern.
     We only need register pop pattern for later analysis.  */
  unsigned first_indx = 0;
  first_indx += return_pc ? 1 : 0;
  first_indx += write_back_p ? 1 : 0;

  /* A pop operation can be done through LDM or POP.  If the base register is SP
     and if it's with write back, then a LDM will be alias of POP.  */
  bool pop_p = (regno == SP_REGNUM && write_back_p);
  bool ldm_p = !pop_p;

  /* Check base register for LDM.  */
  if (ldm_p && REGNO_REG_CLASS (regno) == HI_REGS)
    return 4;

  /* Check each register in the list.  */
  for (; indx >= first_indx; indx--)
    {
      regno = REGNO (XEXP (XVECEXP (parallel_op, 0, indx), 0));
      /* For POP, PC in HI_REGS can be used with 16-bit encoding.  See similar
	 comment in arm_attr_length_push_multi.  */
      if (REGNO_REG_CLASS (regno) == HI_REGS
	  && (regno != PC_REGNUM || ldm_p))
	return 4;
    }

  return 2;
}

/* Compute the number of instructions emitted by output_move_double.  */
int
arm_count_output_move_double_insns (rtx *operands)
{
  int count;
  rtx ops[2];
  /* output_move_double may modify the operands array, so call it
     here on a copy of the array.  */
  ops[0] = operands[0];
  ops[1] = operands[1];
  output_move_double (ops, false, &count);
  return count;
}

int
vfp3_const_double_for_fract_bits (rtx operand)
{
  REAL_VALUE_TYPE r0;
  
  if (!CONST_DOUBLE_P (operand))
    return 0;
  
  r0 = *CONST_DOUBLE_REAL_VALUE (operand);
  if (exact_real_inverse (DFmode, &r0)
      && !REAL_VALUE_NEGATIVE (r0))
    {
      if (exact_real_truncate (DFmode, &r0))
	{
	  HOST_WIDE_INT value = real_to_integer (&r0);
	  value = value & 0xffffffff;
	  if ((value != 0) && ( (value & (value - 1)) == 0))
	    {
	      int ret = exact_log2 (value);
	      gcc_assert (IN_RANGE (ret, 0, 31));
	      return ret;
	    }
	}
    }
  return 0;
}

/* If X is a CONST_DOUBLE with a value that is a power of 2 whose
   log2 is in [1, 32], return that log2.  Otherwise return -1.
   This is used in the patterns for vcvt.s32.f32 floating-point to
   fixed-point conversions.  */

int
vfp3_const_double_for_bits (rtx x)
{
  const REAL_VALUE_TYPE *r;

  if (!CONST_DOUBLE_P (x))
    return -1;

  r = CONST_DOUBLE_REAL_VALUE (x);

  if (REAL_VALUE_NEGATIVE (*r)
      || REAL_VALUE_ISNAN (*r)
      || REAL_VALUE_ISINF (*r)
      || !real_isinteger (r, SFmode))
    return -1;

  HOST_WIDE_INT hwint = exact_log2 (real_to_integer (r));

/* The exact_log2 above will have returned -1 if this is
   not an exact log2.  */
  if (!IN_RANGE (hwint, 1, 32))
    return -1;

  return hwint;
}


/* Emit a memory barrier around an atomic sequence according to MODEL.  */

static void
arm_pre_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, true))
    emit_insn (gen_memory_barrier ());
}

static void
arm_post_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, false))
    emit_insn (gen_memory_barrier ());
}

/* Emit the load-exclusive and store-exclusive instructions.
   Use acquire and release versions if necessary.  */

static void
arm_emit_load_exclusive (machine_mode mode, rtx rval, rtx mem, bool acq)
{
  rtx (*gen) (rtx, rtx);

  if (acq)
    {
      switch (mode)
        {
        case E_QImode: gen = gen_arm_load_acquire_exclusiveqi; break;
        case E_HImode: gen = gen_arm_load_acquire_exclusivehi; break;
        case E_SImode: gen = gen_arm_load_acquire_exclusivesi; break;
        case E_DImode: gen = gen_arm_load_acquire_exclusivedi; break;
        default:
          gcc_unreachable ();
        }
    }
  else
    {
      switch (mode)
        {
        case E_QImode: gen = gen_arm_load_exclusiveqi; break;
        case E_HImode: gen = gen_arm_load_exclusivehi; break;
        case E_SImode: gen = gen_arm_load_exclusivesi; break;
        case E_DImode: gen = gen_arm_load_exclusivedi; break;
        default:
          gcc_unreachable ();
        }
    }

  emit_insn (gen (rval, mem));
}

static void
arm_emit_store_exclusive (machine_mode mode, rtx bval, rtx rval,
                          rtx mem, bool rel)
{
  rtx (*gen) (rtx, rtx, rtx);

  if (rel)
    {
      switch (mode)
        {
        case E_QImode: gen = gen_arm_store_release_exclusiveqi; break;
        case E_HImode: gen = gen_arm_store_release_exclusivehi; break;
        case E_SImode: gen = gen_arm_store_release_exclusivesi; break;
        case E_DImode: gen = gen_arm_store_release_exclusivedi; break;
        default:
          gcc_unreachable ();
        }
    }
  else
    {
      switch (mode)
        {
        case E_QImode: gen = gen_arm_store_exclusiveqi; break;
        case E_HImode: gen = gen_arm_store_exclusivehi; break;
        case E_SImode: gen = gen_arm_store_exclusivesi; break;
        case E_DImode: gen = gen_arm_store_exclusivedi; break;
        default:
          gcc_unreachable ();
        }
    }

  emit_insn (gen (bval, rval, mem));
}

/* Mark the previous jump instruction as unlikely.  */

static void
emit_unlikely_jump (rtx insn)
{
  rtx_insn *jump = emit_jump_insn (insn);
  add_reg_br_prob_note (jump, profile_probability::very_unlikely ());
}

/* Expand a compare and swap pattern.  */

void
arm_expand_compare_and_swap (rtx operands[])
{
  rtx bval, bdst, rval, mem, oldval, newval, is_weak, mod_s, mod_f, x;
  machine_mode mode;
  rtx (*gen) (rtx, rtx, rtx, rtx, rtx, rtx, rtx, rtx);

  bval = operands[0];
  rval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = operands[5];
  mod_s = operands[6];
  mod_f = operands[7];
  mode = GET_MODE (mem);

  /* Normally the succ memory model must be stronger than fail, but in the
     unlikely event of fail being ACQUIRE and succ being RELEASE we need to
     promote succ to ACQ_REL so that we don't lose the acquire semantics.  */

  if (TARGET_HAVE_LDACQ
      && is_mm_acquire (memmodel_from_int (INTVAL (mod_f)))
      && is_mm_release (memmodel_from_int (INTVAL (mod_s))))
    mod_s = GEN_INT (MEMMODEL_ACQ_REL);

  switch (mode)
    {
    case E_QImode:
    case E_HImode:
      /* For narrow modes, we're going to perform the comparison in SImode,
	 so do the zero-extension now.  */
      rval = gen_reg_rtx (SImode);
      oldval = convert_modes (SImode, mode, oldval, true);
      /* FALLTHRU */

    case E_SImode:
      /* Force the value into a register if needed.  We waited until after
	 the zero-extension above to do this properly.  */
      if (!arm_add_operand (oldval, SImode))
	oldval = force_reg (SImode, oldval);
      break;

    case E_DImode:
      if (!cmpdi_operand (oldval, mode))
	oldval = force_reg (mode, oldval);
      break;

    default:
      gcc_unreachable ();
    }

  if (TARGET_THUMB1)
    {
      switch (mode)
	{
	case E_QImode: gen = gen_atomic_compare_and_swapt1qi_1; break;
	case E_HImode: gen = gen_atomic_compare_and_swapt1hi_1; break;
	case E_SImode: gen = gen_atomic_compare_and_swapt1si_1; break;
	case E_DImode: gen = gen_atomic_compare_and_swapt1di_1; break;
	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      switch (mode)
	{
	case E_QImode: gen = gen_atomic_compare_and_swap32qi_1; break;
	case E_HImode: gen = gen_atomic_compare_and_swap32hi_1; break;
	case E_SImode: gen = gen_atomic_compare_and_swap32si_1; break;
	case E_DImode: gen = gen_atomic_compare_and_swap32di_1; break;
	default:
	  gcc_unreachable ();
	}
    }

  bdst = TARGET_THUMB1 ? bval : gen_rtx_REG (CC_Zmode, CC_REGNUM);
  emit_insn (gen (bdst, rval, mem, oldval, newval, is_weak, mod_s, mod_f));

  if (mode == QImode || mode == HImode)
    emit_move_insn (operands[1], gen_lowpart (mode, rval));

  /* In all cases, we arrange for success to be signaled by Z set.
     This arrangement allows for the boolean result to be used directly
     in a subsequent branch, post optimization.  For Thumb-1 targets, the
     boolean negation of the result is also stored in bval because Thumb-1
     backend lacks dependency tracking for CC flag due to flag-setting not
     being represented at RTL level.  */
  if (TARGET_THUMB1)
      emit_insn (gen_cstoresi_eq0_thumb1 (bval, bdst));
  else
    {
      x = gen_rtx_EQ (SImode, bdst, const0_rtx);
      emit_insn (gen_rtx_SET (bval, x));
    }
}

/* Split a compare and swap pattern.  It is IMPLEMENTATION DEFINED whether
   another memory store between the load-exclusive and store-exclusive can
   reset the monitor from Exclusive to Open state.  This means we must wait
   until after reload to split the pattern, lest we get a register spill in
   the middle of the atomic sequence.  Success of the compare and swap is
   indicated by the Z flag set for 32bit targets and by neg_bval being zero
   for Thumb-1 targets (ie. negation of the boolean value returned by
   atomic_compare_and_swapmode standard pattern in operand 0).  */

void
arm_split_compare_and_swap (rtx operands[])
{
  rtx rval, mem, oldval, newval, neg_bval;
  machine_mode mode;
  enum memmodel mod_s, mod_f;
  bool is_weak;
  rtx_code_label *label1, *label2;
  rtx x, cond;

  rval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = (operands[5] != const0_rtx);
  mod_s = memmodel_from_int (INTVAL (operands[6]));
  mod_f = memmodel_from_int (INTVAL (operands[7]));
  neg_bval = TARGET_THUMB1 ? operands[0] : operands[8];
  mode = GET_MODE (mem);

  bool is_armv8_sync = arm_arch8 && is_mm_sync (mod_s);

  bool use_acquire = TARGET_HAVE_LDACQ
                     && !(is_mm_relaxed (mod_s) || is_mm_consume (mod_s)
			  || is_mm_release (mod_s));
		
  bool use_release = TARGET_HAVE_LDACQ
                     && !(is_mm_relaxed (mod_s) || is_mm_consume (mod_s)
			  || is_mm_acquire (mod_s));

  /* For ARMv8, the load-acquire is too weak for __sync memory orders.  Instead,
     a full barrier is emitted after the store-release.  */
  if (is_armv8_sync)
    use_acquire = false;

  /* Checks whether a barrier is needed and emits one accordingly.  */
  if (!(use_acquire || use_release))
    arm_pre_atomic_barrier (mod_s);

  label1 = NULL;
  if (!is_weak)
    {
      label1 = gen_label_rtx ();
      emit_label (label1);
    }
  label2 = gen_label_rtx ();

  arm_emit_load_exclusive (mode, rval, mem, use_acquire);

  /* Z is set to 0 for 32bit targets (resp. rval set to 1) if oldval != rval,
     as required to communicate with arm_expand_compare_and_swap.  */
  if (TARGET_32BIT)
    {
      cond = arm_gen_compare_reg (NE, rval, oldval, neg_bval);
      x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, label2), pc_rtx);
      emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));
    }
  else
    {
      emit_move_insn (neg_bval, const1_rtx);
      cond = gen_rtx_NE (VOIDmode, rval, oldval);
      if (thumb1_cmpneg_operand (oldval, SImode))
	emit_unlikely_jump (gen_cbranchsi4_scratch (neg_bval, rval, oldval,
						    label2, cond));
      else
	emit_unlikely_jump (gen_cbranchsi4_insn (cond, rval, oldval, label2));
    }

  arm_emit_store_exclusive (mode, neg_bval, mem, newval, use_release);

  /* Weak or strong, we want EQ to be true for success, so that we
     match the flags that we got from the compare above.  */
  if (TARGET_32BIT)
    {
      cond = gen_rtx_REG (CCmode, CC_REGNUM);
      x = gen_rtx_COMPARE (CCmode, neg_bval, const0_rtx);
      emit_insn (gen_rtx_SET (cond, x));
    }

  if (!is_weak)
    {
      /* Z is set to boolean value of !neg_bval, as required to communicate
	 with arm_expand_compare_and_swap.  */
      x = gen_rtx_NE (VOIDmode, neg_bval, const0_rtx);
      emit_unlikely_jump (gen_cbranchsi4 (x, neg_bval, const0_rtx, label1));
    }

  if (!is_mm_relaxed (mod_f))
    emit_label (label2);

  /* Checks whether a barrier is needed and emits one accordingly.  */
  if (is_armv8_sync
      || !(use_acquire || use_release))
    arm_post_atomic_barrier (mod_s);

  if (is_mm_relaxed (mod_f))
    emit_label (label2);
}

/* Split an atomic operation pattern.  Operation is given by CODE and is one
   of PLUS, MINUS, IOR, XOR, SET (for an exchange operation) or NOT (for a nand
   operation).  Operation is performed on the content at MEM and on VALUE
   following the memory model MODEL_RTX.  The content at MEM before and after
   the operation is returned in OLD_OUT and NEW_OUT respectively while the
   success of the operation is returned in COND.  Using a scratch register or
   an operand register for these determines what result is returned for that
   pattern.  */

void
arm_split_atomic_op (enum rtx_code code, rtx old_out, rtx new_out, rtx mem,
		     rtx value, rtx model_rtx, rtx cond)
{
  enum memmodel model = memmodel_from_int (INTVAL (model_rtx));
  machine_mode mode = GET_MODE (mem);
  machine_mode wmode = (mode == DImode ? DImode : SImode);
  rtx_code_label *label;
  bool all_low_regs, bind_old_new;
  rtx x;

  bool is_armv8_sync = arm_arch8 && is_mm_sync (model);

  bool use_acquire = TARGET_HAVE_LDACQ
                     && !(is_mm_relaxed (model) || is_mm_consume (model)
			  || is_mm_release (model));

  bool use_release = TARGET_HAVE_LDACQ
                     && !(is_mm_relaxed (model) || is_mm_consume (model)
			  || is_mm_acquire (model));

  /* For ARMv8, a load-acquire is too weak for __sync memory orders.  Instead,
     a full barrier is emitted after the store-release.  */
  if (is_armv8_sync)
    use_acquire = false;

  /* Checks whether a barrier is needed and emits one accordingly.  */
  if (!(use_acquire || use_release))
    arm_pre_atomic_barrier (model);

  label = gen_label_rtx ();
  emit_label (label);

  if (new_out)
    new_out = gen_lowpart (wmode, new_out);
  if (old_out)
    old_out = gen_lowpart (wmode, old_out);
  else
    old_out = new_out;
  value = simplify_gen_subreg (wmode, value, mode, 0);

  arm_emit_load_exclusive (mode, old_out, mem, use_acquire);

  /* Does the operation require destination and first operand to use the same
     register?  This is decided by register constraints of relevant insn
     patterns in thumb1.md.  */
  gcc_assert (!new_out || REG_P (new_out));
  all_low_regs = REG_P (value) && REGNO_REG_CLASS (REGNO (value)) == LO_REGS
		 && new_out && REGNO_REG_CLASS (REGNO (new_out)) == LO_REGS
		 && REGNO_REG_CLASS (REGNO (old_out)) == LO_REGS;
  bind_old_new =
    (TARGET_THUMB1
     && code != SET
     && code != MINUS
     && (code != PLUS || (!all_low_regs && !satisfies_constraint_L (value))));

  /* We want to return the old value while putting the result of the operation
     in the same register as the old value so copy the old value over to the
     destination register and use that register for the operation.  */
  if (old_out && bind_old_new)
    {
      emit_move_insn (new_out, old_out);
      old_out = new_out;
    }

  switch (code)
    {
    case SET:
      new_out = value;
      break;

    case NOT:
      x = gen_rtx_AND (wmode, old_out, value);
      emit_insn (gen_rtx_SET (new_out, x));
      x = gen_rtx_NOT (wmode, new_out);
      emit_insn (gen_rtx_SET (new_out, x));
      break;

    case MINUS:
      if (CONST_INT_P (value))
	{
	  value = GEN_INT (-INTVAL (value));
	  code = PLUS;
	}
      /* FALLTHRU */

    case PLUS:
      if (mode == DImode)
	{
	  /* DImode plus/minus need to clobber flags.  */
	  /* The adddi3 and subdi3 patterns are incorrectly written so that
	     they require matching operands, even when we could easily support
	     three operands.  Thankfully, this can be fixed up post-splitting,
	     as the individual add+adc patterns do accept three operands and
	     post-reload cprop can make these moves go away.  */
	  emit_move_insn (new_out, old_out);
	  if (code == PLUS)
	    x = gen_adddi3 (new_out, new_out, value);
	  else
	    x = gen_subdi3 (new_out, new_out, value);
	  emit_insn (x);
	  break;
	}
      /* FALLTHRU */

    default:
      x = gen_rtx_fmt_ee (code, wmode, old_out, value);
      emit_insn (gen_rtx_SET (new_out, x));
      break;
    }

  arm_emit_store_exclusive (mode, cond, mem, gen_lowpart (mode, new_out),
                            use_release);

  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  emit_unlikely_jump (gen_cbranchsi4 (x, cond, const0_rtx, label));

  /* Checks whether a barrier is needed and emits one accordingly.  */
  if (is_armv8_sync
      || !(use_acquire || use_release))
    arm_post_atomic_barrier (model);
}

#define MAX_VECT_LEN 16

struct expand_vec_perm_d
{
  rtx target, op0, op1;
  vec_perm_indices perm;
  machine_mode vmode;
  bool one_vector_p;
  bool testing_p;
};

/* Generate a variable permutation.  */

static void
arm_expand_vec_perm_1 (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode vmode = GET_MODE (target);
  bool one_vector_p = rtx_equal_p (op0, op1);

  gcc_checking_assert (vmode == V8QImode || vmode == V16QImode);
  gcc_checking_assert (GET_MODE (op0) == vmode);
  gcc_checking_assert (GET_MODE (op1) == vmode);
  gcc_checking_assert (GET_MODE (sel) == vmode);
  gcc_checking_assert (TARGET_NEON);

  if (one_vector_p)
    {
      if (vmode == V8QImode)
	emit_insn (gen_neon_vtbl1v8qi (target, op0, sel));
      else
	emit_insn (gen_neon_vtbl1v16qi (target, op0, sel));
    }
  else
    {
      rtx pair;

      if (vmode == V8QImode)
	{
	  pair = gen_reg_rtx (V16QImode);
	  emit_insn (gen_neon_vcombinev8qi (pair, op0, op1));
	  pair = gen_lowpart (TImode, pair);
	  emit_insn (gen_neon_vtbl2v8qi (target, pair, sel));
	}
      else
	{
	  pair = gen_reg_rtx (OImode);
	  emit_insn (gen_neon_vcombinev16qi (pair, op0, op1));
	  emit_insn (gen_neon_vtbl2v16qi (target, pair, sel));
	}
    }
}

void
arm_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode vmode = GET_MODE (target);
  unsigned int nelt = GET_MODE_NUNITS (vmode);
  bool one_vector_p = rtx_equal_p (op0, op1);
  rtx mask;

  /* TODO: ARM's VTBL indexing is little-endian.  In order to handle GCC's
     numbering of elements for big-endian, we must reverse the order.  */
  gcc_checking_assert (!BYTES_BIG_ENDIAN);

  /* The VTBL instruction does not use a modulo index, so we must take care
     of that ourselves.  */
  mask = GEN_INT (one_vector_p ? nelt - 1 : 2 * nelt - 1);
  mask = gen_const_vec_duplicate (vmode, mask);
  sel = expand_simple_binop (vmode, AND, sel, mask, NULL, 0, OPTAB_LIB_WIDEN);

  arm_expand_vec_perm_1 (target, op0, op1, sel);
}

/* Map lane ordering between architectural lane order, and GCC lane order,
   taking into account ABI.  See comment above output_move_neon for details.  */

static int
neon_endian_lane_map (machine_mode mode, int lane)
{
  if (BYTES_BIG_ENDIAN)
  {
    int nelems = GET_MODE_NUNITS (mode);
    /* Reverse lane order.  */
    lane = (nelems - 1 - lane);
    /* Reverse D register order, to match ABI.  */
    if (GET_MODE_SIZE (mode) == 16)
      lane = lane ^ (nelems / 2);
  }
  return lane;
}

/* Some permutations index into pairs of vectors, this is a helper function
   to map indexes into those pairs of vectors.  */

static int
neon_pair_endian_lane_map (machine_mode mode, int lane)
{
  int nelem = GET_MODE_NUNITS (mode);
  if (BYTES_BIG_ENDIAN)
    lane =
      neon_endian_lane_map (mode, lane & (nelem - 1)) + (lane & nelem);
  return lane;
}

/* Generate or test for an insn that supports a constant permutation.  */

/* Recognize patterns for the VUZP insns.  */

static bool
arm_evpc_neon_vuzp (struct expand_vec_perm_d *d)
{
  unsigned int i, odd, mask, nelt = d->perm.length ();
  rtx out0, out1, in0, in1;
  rtx (*gen)(rtx, rtx, rtx, rtx);
  int first_elem;
  int swap_nelt;

  if (GET_MODE_UNIT_SIZE (d->vmode) >= 8)
    return false;

  /* arm_expand_vec_perm_const_1 () helpfully swaps the operands for the
     big endian pattern on 64 bit vectors, so we correct for that.  */
  swap_nelt = BYTES_BIG_ENDIAN && !d->one_vector_p
    && GET_MODE_SIZE (d->vmode) == 8 ? nelt : 0;

  first_elem = d->perm[neon_endian_lane_map (d->vmode, 0)] ^ swap_nelt;

  if (first_elem == neon_endian_lane_map (d->vmode, 0))
    odd = 0;
  else if (first_elem == neon_endian_lane_map (d->vmode, 1))
    odd = 1;
  else
    return false;
  mask = (d->one_vector_p ? nelt - 1 : 2 * nelt - 1);

  for (i = 0; i < nelt; i++)
    {
      unsigned elt =
	(neon_pair_endian_lane_map (d->vmode, i) * 2 + odd) & mask;
      if ((d->perm[i] ^ swap_nelt) != neon_pair_endian_lane_map (d->vmode, elt))
	return false;
    }

  /* Success!  */
  if (d->testing_p)
    return true;

  switch (d->vmode)
    {
    case E_V16QImode: gen = gen_neon_vuzpv16qi_internal; break;
    case E_V8QImode:  gen = gen_neon_vuzpv8qi_internal;  break;
    case E_V8HImode:  gen = gen_neon_vuzpv8hi_internal;  break;
    case E_V4HImode:  gen = gen_neon_vuzpv4hi_internal;  break;
    case E_V8HFmode:  gen = gen_neon_vuzpv8hf_internal;  break;
    case E_V4HFmode:  gen = gen_neon_vuzpv4hf_internal;  break;
    case E_V4SImode:  gen = gen_neon_vuzpv4si_internal;  break;
    case E_V2SImode:  gen = gen_neon_vuzpv2si_internal;  break;
    case E_V2SFmode:  gen = gen_neon_vuzpv2sf_internal;  break;
    case E_V4SFmode:  gen = gen_neon_vuzpv4sf_internal;  break;
    default:
      gcc_unreachable ();
    }

  in0 = d->op0;
  in1 = d->op1;
  if (swap_nelt != 0)
    std::swap (in0, in1);

  out0 = d->target;
  out1 = gen_reg_rtx (d->vmode);
  if (odd)
    std::swap (out0, out1);

  emit_insn (gen (out0, in0, in1, out1));
  return true;
}

/* Recognize patterns for the VZIP insns.  */

static bool
arm_evpc_neon_vzip (struct expand_vec_perm_d *d)
{
  unsigned int i, high, mask, nelt = d->perm.length ();
  rtx out0, out1, in0, in1;
  rtx (*gen)(rtx, rtx, rtx, rtx);
  int first_elem;
  bool is_swapped;

  if (GET_MODE_UNIT_SIZE (d->vmode) >= 8)
    return false;

  is_swapped = BYTES_BIG_ENDIAN;

  first_elem = d->perm[neon_endian_lane_map (d->vmode, 0) ^ is_swapped];

  high = nelt / 2;
  if (first_elem == neon_endian_lane_map (d->vmode, high))
    ;
  else if (first_elem == neon_endian_lane_map (d->vmode, 0))
    high = 0;
  else
    return false;
  mask = (d->one_vector_p ? nelt - 1 : 2 * nelt - 1);

  for (i = 0; i < nelt / 2; i++)
    {
      unsigned elt =
	neon_pair_endian_lane_map (d->vmode, i + high) & mask;
      if (d->perm[neon_pair_endian_lane_map (d->vmode, 2 * i + is_swapped)]
	  != elt)
	return false;
      elt =
	neon_pair_endian_lane_map (d->vmode, i + nelt + high) & mask;
      if (d->perm[neon_pair_endian_lane_map (d->vmode, 2 * i + !is_swapped)]
	  != elt)
	return false;
    }

  /* Success!  */
  if (d->testing_p)
    return true;

  switch (d->vmode)
    {
    case E_V16QImode: gen = gen_neon_vzipv16qi_internal; break;
    case E_V8QImode:  gen = gen_neon_vzipv8qi_internal;  break;
    case E_V8HImode:  gen = gen_neon_vzipv8hi_internal;  break;
    case E_V4HImode:  gen = gen_neon_vzipv4hi_internal;  break;
    case E_V8HFmode:  gen = gen_neon_vzipv8hf_internal;  break;
    case E_V4HFmode:  gen = gen_neon_vzipv4hf_internal;  break;
    case E_V4SImode:  gen = gen_neon_vzipv4si_internal;  break;
    case E_V2SImode:  gen = gen_neon_vzipv2si_internal;  break;
    case E_V2SFmode:  gen = gen_neon_vzipv2sf_internal;  break;
    case E_V4SFmode:  gen = gen_neon_vzipv4sf_internal;  break;
    default:
      gcc_unreachable ();
    }

  in0 = d->op0;
  in1 = d->op1;
  if (is_swapped)
    std::swap (in0, in1);

  out0 = d->target;
  out1 = gen_reg_rtx (d->vmode);
  if (high)
    std::swap (out0, out1);

  emit_insn (gen (out0, in0, in1, out1));
  return true;
}

/* Recognize patterns for the VREV insns.  */

static bool
arm_evpc_neon_vrev (struct expand_vec_perm_d *d)
{
  unsigned int i, j, diff, nelt = d->perm.length ();
  rtx (*gen)(rtx, rtx);

  if (!d->one_vector_p)
    return false;

  diff = d->perm[0];
  switch (diff)
    {
    case 7:
      switch (d->vmode)
	{
	case E_V16QImode: gen = gen_neon_vrev64v16qi; break;
	case E_V8QImode:  gen = gen_neon_vrev64v8qi;  break;
	default:
	  return false;
	}
      break;
    case 3:
      switch (d->vmode)
	{
	case E_V16QImode: gen = gen_neon_vrev32v16qi; break;
	case E_V8QImode:  gen = gen_neon_vrev32v8qi;  break;
	case E_V8HImode:  gen = gen_neon_vrev64v8hi;  break;
	case E_V4HImode:  gen = gen_neon_vrev64v4hi;  break;
	case E_V8HFmode:  gen = gen_neon_vrev64v8hf;  break;
	case E_V4HFmode:  gen = gen_neon_vrev64v4hf;  break;
	default:
	  return false;
	}
      break;
    case 1:
      switch (d->vmode)
	{
	case E_V16QImode: gen = gen_neon_vrev16v16qi; break;
	case E_V8QImode:  gen = gen_neon_vrev16v8qi;  break;
	case E_V8HImode:  gen = gen_neon_vrev32v8hi;  break;
	case E_V4HImode:  gen = gen_neon_vrev32v4hi;  break;
	case E_V4SImode:  gen = gen_neon_vrev64v4si;  break;
	case E_V2SImode:  gen = gen_neon_vrev64v2si;  break;
	case E_V4SFmode:  gen = gen_neon_vrev64v4sf;  break;
	case E_V2SFmode:  gen = gen_neon_vrev64v2sf;  break;
	default:
	  return false;
	}
      break;
    default:
      return false;
    }

  for (i = 0; i < nelt ; i += diff + 1)
    for (j = 0; j <= diff; j += 1)
      {
	/* This is guaranteed to be true as the value of diff
	   is 7, 3, 1 and we should have enough elements in the
	   queue to generate this. Getting a vector mask with a
	   value of diff other than these values implies that
	   something is wrong by the time we get here.  */
	gcc_assert (i + j < nelt);
	if (d->perm[i + j] != i + diff - j)
	  return false;
      }

  /* Success! */
  if (d->testing_p)
    return true;

  emit_insn (gen (d->target, d->op0));
  return true;
}

/* Recognize patterns for the VTRN insns.  */

static bool
arm_evpc_neon_vtrn (struct expand_vec_perm_d *d)
{
  unsigned int i, odd, mask, nelt = d->perm.length ();
  rtx out0, out1, in0, in1;
  rtx (*gen)(rtx, rtx, rtx, rtx);

  if (GET_MODE_UNIT_SIZE (d->vmode) >= 8)
    return false;

  /* Note that these are little-endian tests.  Adjust for big-endian later.  */
  if (d->perm[0] == 0)
    odd = 0;
  else if (d->perm[0] == 1)
    odd = 1;
  else
    return false;
  mask = (d->one_vector_p ? nelt - 1 : 2 * nelt - 1);

  for (i = 0; i < nelt; i += 2)
    {
      if (d->perm[i] != i + odd)
	return false;
      if (d->perm[i + 1] != ((i + nelt + odd) & mask))
	return false;
    }

  /* Success!  */
  if (d->testing_p)
    return true;

  switch (d->vmode)
    {
    case E_V16QImode: gen = gen_neon_vtrnv16qi_internal; break;
    case E_V8QImode:  gen = gen_neon_vtrnv8qi_internal;  break;
    case E_V8HImode:  gen = gen_neon_vtrnv8hi_internal;  break;
    case E_V4HImode:  gen = gen_neon_vtrnv4hi_internal;  break;
    case E_V8HFmode:  gen = gen_neon_vtrnv8hf_internal;  break;
    case E_V4HFmode:  gen = gen_neon_vtrnv4hf_internal;  break;
    case E_V4SImode:  gen = gen_neon_vtrnv4si_internal;  break;
    case E_V2SImode:  gen = gen_neon_vtrnv2si_internal;  break;
    case E_V2SFmode:  gen = gen_neon_vtrnv2sf_internal;  break;
    case E_V4SFmode:  gen = gen_neon_vtrnv4sf_internal;  break;
    default:
      gcc_unreachable ();
    }

  in0 = d->op0;
  in1 = d->op1;
  if (BYTES_BIG_ENDIAN)
    {
      std::swap (in0, in1);
      odd = !odd;
    }

  out0 = d->target;
  out1 = gen_reg_rtx (d->vmode);
  if (odd)
    std::swap (out0, out1);

  emit_insn (gen (out0, in0, in1, out1));
  return true;
}

/* Recognize patterns for the VEXT insns.  */

static bool
arm_evpc_neon_vext (struct expand_vec_perm_d *d)
{
  unsigned int i, nelt = d->perm.length ();
  rtx (*gen) (rtx, rtx, rtx, rtx);
  rtx offset;

  unsigned int location;

  unsigned int next  = d->perm[0] + 1;

  /* TODO: Handle GCC's numbering of elements for big-endian.  */
  if (BYTES_BIG_ENDIAN)
    return false;

  /* Check if the extracted indexes are increasing by one.  */
  for (i = 1; i < nelt; next++, i++)
    {
      /* If we hit the most significant element of the 2nd vector in
	 the previous iteration, no need to test further.  */
      if (next == 2 * nelt)
	return false;

      /* If we are operating on only one vector: it could be a
	 rotation.  If there are only two elements of size < 64, let
	 arm_evpc_neon_vrev catch it.  */
      if (d->one_vector_p && (next == nelt))
	{
	  if ((nelt == 2) && (d->vmode != V2DImode))
	    return false;
	  else
	    next = 0;
	}

      if (d->perm[i] != next)
	return false;
    }

  location = d->perm[0];

  switch (d->vmode)
    {
    case E_V16QImode: gen = gen_neon_vextv16qi; break;
    case E_V8QImode: gen = gen_neon_vextv8qi; break;
    case E_V4HImode: gen = gen_neon_vextv4hi; break;
    case E_V8HImode: gen = gen_neon_vextv8hi; break;
    case E_V2SImode: gen = gen_neon_vextv2si; break;
    case E_V4SImode: gen = gen_neon_vextv4si; break;
    case E_V4HFmode: gen = gen_neon_vextv4hf; break;
    case E_V8HFmode: gen = gen_neon_vextv8hf; break;
    case E_V2SFmode: gen = gen_neon_vextv2sf; break;
    case E_V4SFmode: gen = gen_neon_vextv4sf; break;
    case E_V2DImode: gen = gen_neon_vextv2di; break;
    default:
      return false;
    }

  /* Success! */
  if (d->testing_p)
    return true;

  offset = GEN_INT (location);
  emit_insn (gen (d->target, d->op0, d->op1, offset));
  return true;
}

/* The NEON VTBL instruction is a fully variable permuation that's even
   stronger than what we expose via VEC_PERM_EXPR.  What it doesn't do
   is mask the index operand as VEC_PERM_EXPR requires.  Therefore we
   can do slightly better by expanding this as a constant where we don't
   have to apply a mask.  */

static bool
arm_evpc_neon_vtbl (struct expand_vec_perm_d *d)
{
  rtx rperm[MAX_VECT_LEN], sel;
  machine_mode vmode = d->vmode;
  unsigned int i, nelt = d->perm.length ();

  /* TODO: ARM's VTBL indexing is little-endian.  In order to handle GCC's
     numbering of elements for big-endian, we must reverse the order.  */
  if (BYTES_BIG_ENDIAN)
    return false;

  if (d->testing_p)
    return true;

  /* Generic code will try constant permutation twice.  Once with the
     original mode and again with the elements lowered to QImode.
     So wait and don't do the selector expansion ourselves.  */
  if (vmode != V8QImode && vmode != V16QImode)
    return false;

  for (i = 0; i < nelt; ++i)
    rperm[i] = GEN_INT (d->perm[i]);
  sel = gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (nelt, rperm));
  sel = force_reg (vmode, sel);

  arm_expand_vec_perm_1 (d->target, d->op0, d->op1, sel);
  return true;
}

static bool
arm_expand_vec_perm_const_1 (struct expand_vec_perm_d *d)
{
  /* Check if the input mask matches vext before reordering the
     operands.  */
  if (TARGET_NEON)
    if (arm_evpc_neon_vext (d))
      return true;

  /* The pattern matching functions above are written to look for a small
     number to begin the sequence (0, 1, N/2).  If we begin with an index
     from the second operand, we can swap the operands.  */
  unsigned int nelt = d->perm.length ();
  if (d->perm[0] >= nelt)
    {
      d->perm.rotate_inputs (1);
      std::swap (d->op0, d->op1);
    }

  if (TARGET_NEON)
    {
      if (arm_evpc_neon_vuzp (d))
	return true;
      if (arm_evpc_neon_vzip (d))
	return true;
      if (arm_evpc_neon_vrev (d))
	return true;
      if (arm_evpc_neon_vtrn (d))
	return true;
      return arm_evpc_neon_vtbl (d);
    }
  return false;
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.  */

static bool
arm_vectorize_vec_perm_const (machine_mode vmode, rtx target, rtx op0, rtx op1,
			      const vec_perm_indices &sel)
{
  struct expand_vec_perm_d d;
  int i, nelt, which;

  if (!VALID_NEON_DREG_MODE (vmode) && !VALID_NEON_QREG_MODE (vmode))
    return false;

  d.target = target;
  d.op0 = op0;
  d.op1 = op1;

  d.vmode = vmode;
  gcc_assert (VECTOR_MODE_P (d.vmode));
  d.testing_p = !target;

  nelt = GET_MODE_NUNITS (d.vmode);
  for (i = which = 0; i < nelt; ++i)
    {
      int ei = sel[i] & (2 * nelt - 1);
      which |= (ei < nelt ? 1 : 2);
    }

  switch (which)
    {
    default:
      gcc_unreachable();

    case 3:
      d.one_vector_p = false;
      if (d.testing_p || !rtx_equal_p (op0, op1))
	break;

      /* The elements of PERM do not suggest that only the first operand
	 is used, but both operands are identical.  Allow easier matching
	 of the permutation by folding the permutation into the single
	 input vector.  */
      /* FALLTHRU */
    case 2:
      d.op0 = op1;
      d.one_vector_p = true;
      break;

    case 1:
      d.op1 = op0;
      d.one_vector_p = true;
      break;
    }

  d.perm.new_vector (sel.encoding (), d.one_vector_p ? 1 : 2, nelt);

  if (!d.testing_p)
    return arm_expand_vec_perm_const_1 (&d);

  d.target = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 1);
  d.op1 = d.op0 = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 2);
  if (!d.one_vector_p)
    d.op1 = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 3);

  start_sequence ();
  bool ret = arm_expand_vec_perm_const_1 (&d);
  end_sequence ();

  return ret;
}

bool
arm_autoinc_modes_ok_p (machine_mode mode, enum arm_auto_incmodes code)
{
  /* If we are soft float and we do not have ldrd
     then all auto increment forms are ok.  */
  if (TARGET_SOFT_FLOAT && (TARGET_LDRD || GET_MODE_SIZE (mode) <= 4))
    return true;

  switch (code)
    {
      /* Post increment and Pre Decrement are supported for all
	 instruction forms except for vector forms.  */
    case ARM_POST_INC:
    case ARM_PRE_DEC:
      if (VECTOR_MODE_P (mode))
	{
	  if (code != ARM_PRE_DEC)
	    return true;
	  else
	    return false;
	}
      
      return true;

    case ARM_POST_DEC:
    case ARM_PRE_INC:
      /* Without LDRD and mode size greater than
	 word size, there is no point in auto-incrementing
         because ldm and stm will not have these forms.  */
      if (!TARGET_LDRD && GET_MODE_SIZE (mode) > 4)
	return false;

      /* Vector and floating point modes do not support
	 these auto increment forms.  */
      if (FLOAT_MODE_P (mode) || VECTOR_MODE_P (mode))
	return false;

      return true;
     
    default:
      return false;
      
    }

  return false;
}

/* The default expansion of general 64-bit shifts in core-regs is suboptimal,
   on ARM, since we know that shifts by negative amounts are no-ops.
   Additionally, the default expansion code is not available or suitable
   for post-reload insn splits (this can occur when the register allocator
   chooses not to do a shift in NEON).
   
   This function is used in both initial expand and post-reload splits, and
   handles all kinds of 64-bit shifts.

   Input requirements:
    - It is safe for the input and output to be the same register, but
      early-clobber rules apply for the shift amount and scratch registers.
    - Shift by register requires both scratch registers.  In all other cases
      the scratch registers may be NULL.
    - Ashiftrt by a register also clobbers the CC register.  */
void
arm_emit_coreregs_64bit_shift (enum rtx_code code, rtx out, rtx in,
			       rtx amount, rtx scratch1, rtx scratch2)
{
  rtx out_high = gen_highpart (SImode, out);
  rtx out_low = gen_lowpart (SImode, out);
  rtx in_high = gen_highpart (SImode, in);
  rtx in_low = gen_lowpart (SImode, in);

  /* Terminology:
	in = the register pair containing the input value.
	out = the destination register pair.
	up = the high- or low-part of each pair.
	down = the opposite part to "up".
     In a shift, we can consider bits to shift from "up"-stream to
     "down"-stream, so in a left-shift "up" is the low-part and "down"
     is the high-part of each register pair.  */

  rtx out_up   = code == ASHIFT ? out_low : out_high;
  rtx out_down = code == ASHIFT ? out_high : out_low;
  rtx in_up   = code == ASHIFT ? in_low : in_high;
  rtx in_down = code == ASHIFT ? in_high : in_low;

  gcc_assert (code == ASHIFT || code == ASHIFTRT || code == LSHIFTRT);
  gcc_assert (out
	      && (REG_P (out) || GET_CODE (out) == SUBREG)
	      && GET_MODE (out) == DImode);
  gcc_assert (in
	      && (REG_P (in) || GET_CODE (in) == SUBREG)
	      && GET_MODE (in) == DImode);
  gcc_assert (amount
	      && (((REG_P (amount) || GET_CODE (amount) == SUBREG)
		   && GET_MODE (amount) == SImode)
		  || CONST_INT_P (amount)));
  gcc_assert (scratch1 == NULL
	      || (GET_CODE (scratch1) == SCRATCH)
	      || (GET_MODE (scratch1) == SImode
		  && REG_P (scratch1)));
  gcc_assert (scratch2 == NULL
	      || (GET_CODE (scratch2) == SCRATCH)
	      || (GET_MODE (scratch2) == SImode
		  && REG_P (scratch2)));
  gcc_assert (!REG_P (out) || !REG_P (amount)
	      || !HARD_REGISTER_P (out)
	      || (REGNO (out) != REGNO (amount)
		  && REGNO (out) + 1 != REGNO (amount)));

  /* Macros to make following code more readable.  */
  #define SUB_32(DEST,SRC) \
	    gen_addsi3 ((DEST), (SRC), GEN_INT (-32))
  #define RSB_32(DEST,SRC) \
	    gen_subsi3 ((DEST), GEN_INT (32), (SRC))
  #define SUB_S_32(DEST,SRC) \
	    gen_addsi3_compare0 ((DEST), (SRC), \
				 GEN_INT (-32))
  #define SET(DEST,SRC) \
	    gen_rtx_SET ((DEST), (SRC))
  #define SHIFT(CODE,SRC,AMOUNT) \
	    gen_rtx_fmt_ee ((CODE), SImode, (SRC), (AMOUNT))
  #define LSHIFT(CODE,SRC,AMOUNT) \
	    gen_rtx_fmt_ee ((CODE) == ASHIFT ? ASHIFT : LSHIFTRT, \
			    SImode, (SRC), (AMOUNT))
  #define REV_LSHIFT(CODE,SRC,AMOUNT) \
	    gen_rtx_fmt_ee ((CODE) == ASHIFT ? LSHIFTRT : ASHIFT, \
			    SImode, (SRC), (AMOUNT))
  #define ORR(A,B) \
	    gen_rtx_IOR (SImode, (A), (B))
  #define BRANCH(COND,LABEL) \
	    gen_arm_cond_branch ((LABEL), \
				 gen_rtx_ ## COND (CCmode, cc_reg, \
						   const0_rtx), \
				 cc_reg)

  /* Shifts by register and shifts by constant are handled separately.  */
  if (CONST_INT_P (amount))
    {
      /* We have a shift-by-constant.  */

      /* First, handle out-of-range shift amounts.
	 In both cases we try to match the result an ARM instruction in a
	 shift-by-register would give.  This helps reduce execution
	 differences between optimization levels, but it won't stop other
         parts of the compiler doing different things.  This is "undefined
         behavior, in any case.  */
      if (INTVAL (amount) <= 0)
	emit_insn (gen_movdi (out, in));
      else if (INTVAL (amount) >= 64)
	{
	  if (code == ASHIFTRT)
	    {
	      rtx const31_rtx = GEN_INT (31);
	      emit_insn (SET (out_down, SHIFT (code, in_up, const31_rtx)));
	      emit_insn (SET (out_up, SHIFT (code, in_up, const31_rtx)));
	    }
	  else
	    emit_insn (gen_movdi (out, const0_rtx));
	}

      /* Now handle valid shifts. */
      else if (INTVAL (amount) < 32)
	{
	  /* Shifts by a constant less than 32.  */
	  rtx reverse_amount = GEN_INT (32 - INTVAL (amount));

	  /* Clearing the out register in DImode first avoids lots
	     of spilling and results in less stack usage.
	     Later this redundant insn is completely removed.
	     Do that only if "in" and "out" are different registers.  */
	  if (REG_P (out) && REG_P (in) && REGNO (out) != REGNO (in))
	    emit_insn (SET (out, const0_rtx));
	  emit_insn (SET (out_down, LSHIFT (code, in_down, amount)));
	  emit_insn (SET (out_down,
			  ORR (REV_LSHIFT (code, in_up, reverse_amount),
			       out_down)));
	  emit_insn (SET (out_up, SHIFT (code, in_up, amount)));
	}
      else
	{
	  /* Shifts by a constant greater than 31.  */
	  rtx adj_amount = GEN_INT (INTVAL (amount) - 32);

	  if (REG_P (out) && REG_P (in) && REGNO (out) != REGNO (in))
	    emit_insn (SET (out, const0_rtx));
	  emit_insn (SET (out_down, SHIFT (code, in_up, adj_amount)));
	  if (code == ASHIFTRT)
	    emit_insn (gen_ashrsi3 (out_up, in_up,
				    GEN_INT (31)));
	  else
	    emit_insn (SET (out_up, const0_rtx));
	}
    }
  else
    {
      /* We have a shift-by-register.  */
      rtx cc_reg = gen_rtx_REG (CC_NOOVmode, CC_REGNUM);

      /* This alternative requires the scratch registers.  */
      gcc_assert (scratch1 && REG_P (scratch1));
      gcc_assert (scratch2 && REG_P (scratch2));

      /* We will need the values "amount-32" and "32-amount" later.
         Swapping them around now allows the later code to be more general. */
      switch (code)
	{
	case ASHIFT:
	  emit_insn (SUB_32 (scratch1, amount));
	  emit_insn (RSB_32 (scratch2, amount));
	  break;
	case ASHIFTRT:
	  emit_insn (RSB_32 (scratch1, amount));
	  /* Also set CC = amount > 32.  */
	  emit_insn (SUB_S_32 (scratch2, amount));
	  break;
	case LSHIFTRT:
	  emit_insn (RSB_32 (scratch1, amount));
	  emit_insn (SUB_32 (scratch2, amount));
	  break;
	default:
	  gcc_unreachable ();
	}

      /* Emit code like this:

	 arithmetic-left:
	    out_down = in_down << amount;
	    out_down = (in_up << (amount - 32)) | out_down;
	    out_down = ((unsigned)in_up >> (32 - amount)) | out_down;
	    out_up = in_up << amount;

	 arithmetic-right:
	    out_down = in_down >> amount;
	    out_down = (in_up << (32 - amount)) | out_down;
	    if (amount < 32)
	      out_down = ((signed)in_up >> (amount - 32)) | out_down;
	    out_up = in_up << amount;

	 logical-right:
	    out_down = in_down >> amount;
	    out_down = (in_up << (32 - amount)) | out_down;
	    if (amount < 32)
	      out_down = ((unsigned)in_up >> (amount - 32)) | out_down;
	    out_up = in_up << amount;

	  The ARM and Thumb2 variants are the same but implemented slightly
	  differently.  If this were only called during expand we could just
	  use the Thumb2 case and let combine do the right thing, but this
	  can also be called from post-reload splitters.  */

      emit_insn (SET (out_down, LSHIFT (code, in_down, amount)));

      if (!TARGET_THUMB2)
	{
	  /* Emit code for ARM mode.  */
	  emit_insn (SET (out_down,
			  ORR (SHIFT (ASHIFT, in_up, scratch1), out_down)));
	  if (code == ASHIFTRT)
	    {
	      rtx_code_label *done_label = gen_label_rtx ();
	      emit_jump_insn (BRANCH (LT, done_label));
	      emit_insn (SET (out_down, ORR (SHIFT (ASHIFTRT, in_up, scratch2),
					     out_down)));
	      emit_label (done_label);
	    }
	  else
	    emit_insn (SET (out_down, ORR (SHIFT (LSHIFTRT, in_up, scratch2),
					   out_down)));
	}
      else
	{
	  /* Emit code for Thumb2 mode.
	     Thumb2 can't do shift and or in one insn.  */
	  emit_insn (SET (scratch1, SHIFT (ASHIFT, in_up, scratch1)));
	  emit_insn (gen_iorsi3 (out_down, out_down, scratch1));

	  if (code == ASHIFTRT)
	    {
	      rtx_code_label *done_label = gen_label_rtx ();
	      emit_jump_insn (BRANCH (LT, done_label));
	      emit_insn (SET (scratch2, SHIFT (ASHIFTRT, in_up, scratch2)));
	      emit_insn (SET (out_down, ORR (out_down, scratch2)));
	      emit_label (done_label);
	    }
	  else
	    {
	      emit_insn (SET (scratch2, SHIFT (LSHIFTRT, in_up, scratch2)));
	      emit_insn (gen_iorsi3 (out_down, out_down, scratch2));
	    }
	}

      emit_insn (SET (out_up, SHIFT (code, in_up, amount)));
    }

  #undef SUB_32
  #undef RSB_32
  #undef SUB_S_32
  #undef SET
  #undef SHIFT
  #undef LSHIFT
  #undef REV_LSHIFT
  #undef ORR
  #undef BRANCH
}

/* Returns true if the pattern is a valid symbolic address, which is either a
   symbol_ref or (symbol_ref + addend).

   According to the ARM ELF ABI, the initial addend of REL-type relocations
   processing MOVW and MOVT instructions is formed by interpreting the 16-bit
   literal field of the instruction as a 16-bit signed value in the range
   -32768 <= A < 32768.  */

bool
arm_valid_symbolic_address_p (rtx addr)
{
  rtx xop0, xop1 = NULL_RTX;
  rtx tmp = addr;

  if (target_word_relocations)
    return false;

  if (GET_CODE (tmp) == SYMBOL_REF || GET_CODE (tmp) == LABEL_REF)
    return true;

  /* (const (plus: symbol_ref const_int))  */
  if (GET_CODE (addr) == CONST)
    tmp = XEXP (addr, 0);

  if (GET_CODE (tmp) == PLUS)
    {
      xop0 = XEXP (tmp, 0);
      xop1 = XEXP (tmp, 1);

      if (GET_CODE (xop0) == SYMBOL_REF && CONST_INT_P (xop1))
	  return IN_RANGE (INTVAL (xop1), -0x8000, 0x7fff);
    }

  return false;
}

/* Returns true if a valid comparison operation and makes
   the operands in a form that is valid.  */
bool
arm_validize_comparison (rtx *comparison, rtx * op1, rtx * op2)
{
  enum rtx_code code = GET_CODE (*comparison);
  int code_int;
  machine_mode mode = (GET_MODE (*op1) == VOIDmode)
    ? GET_MODE (*op2) : GET_MODE (*op1);

  gcc_assert (GET_MODE (*op1) != VOIDmode || GET_MODE (*op2) != VOIDmode);

  if (code == UNEQ || code == LTGT)
    return false;

  code_int = (int)code;
  arm_canonicalize_comparison (&code_int, op1, op2, 0);
  PUT_CODE (*comparison, (enum rtx_code)code_int);

  switch (mode)
    {
    case E_SImode:
      if (!arm_add_operand (*op1, mode))
	*op1 = force_reg (mode, *op1);
      if (!arm_add_operand (*op2, mode))
	*op2 = force_reg (mode, *op2);
      return true;

    case E_DImode:
      if (!cmpdi_operand (*op1, mode))
	*op1 = force_reg (mode, *op1);
      if (!cmpdi_operand (*op2, mode))
	*op2 = force_reg (mode, *op2);
      return true;

    case E_HFmode:
      if (!TARGET_VFP_FP16INST)
	break;
      /* FP16 comparisons are done in SF mode.  */
      mode = SFmode;
      *op1 = convert_to_mode (mode, *op1, 1);
      *op2 = convert_to_mode (mode, *op2, 1);
      /* Fall through.  */
    case E_SFmode:
    case E_DFmode:
      if (!vfp_compare_operand (*op1, mode))
	*op1 = force_reg (mode, *op1);
      if (!vfp_compare_operand (*op2, mode))
	*op2 = force_reg (mode, *op2);
      return true;
    default:
      break;
    }

  return false;

}

/* Maximum number of instructions to set block of memory.  */
static int
arm_block_set_max_insns (void)
{
  if (optimize_function_for_size_p (cfun))
    return 4;
  else
    return current_tune->max_insns_inline_memset;
}

/* Return TRUE if it's profitable to set block of memory for
   non-vectorized case.  VAL is the value to set the memory
   with.  LENGTH is the number of bytes to set.  ALIGN is the
   alignment of the destination memory in bytes.  UNALIGNED_P
   is TRUE if we can only set the memory with instructions
   meeting alignment requirements.  USE_STRD_P is TRUE if we
   can use strd to set the memory.  */
static bool
arm_block_set_non_vect_profit_p (rtx val,
				 unsigned HOST_WIDE_INT length,
				 unsigned HOST_WIDE_INT align,
				 bool unaligned_p, bool use_strd_p)
{
  int num = 0;
  /* For leftovers in bytes of 0-7, we can set the memory block using
     strb/strh/str with minimum instruction number.  */
  const int leftover[8] = {0, 1, 1, 2, 1, 2, 2, 3};

  if (unaligned_p)
    {
      num = arm_const_inline_cost (SET, val);
      num += length / align + length % align;
    }
  else if (use_strd_p)
    {
      num = arm_const_double_inline_cost (val);
      num += (length >> 3) + leftover[length & 7];
    }
  else
    {
      num = arm_const_inline_cost (SET, val);
      num += (length >> 2) + leftover[length & 3];
    }

  /* We may be able to combine last pair STRH/STRB into a single STR
     by shifting one byte back.  */
  if (unaligned_access && length > 3 && (length & 3) == 3)
    num--;

  return (num <= arm_block_set_max_insns ());
}

/* Return TRUE if it's profitable to set block of memory for
   vectorized case.  LENGTH is the number of bytes to set.
   ALIGN is the alignment of destination memory in bytes.
   MODE is the vector mode used to set the memory.  */
static bool
arm_block_set_vect_profit_p (unsigned HOST_WIDE_INT length,
			     unsigned HOST_WIDE_INT align,
			     machine_mode mode)
{
  int num;
  bool unaligned_p = ((align & 3) != 0);
  unsigned int nelt = GET_MODE_NUNITS (mode);

  /* Instruction loading constant value.  */
  num = 1;
  /* Instructions storing the memory.  */
  num += (length + nelt - 1) / nelt;
  /* Instructions adjusting the address expression.  Only need to
     adjust address expression if it's 4 bytes aligned and bytes
     leftover can only be stored by mis-aligned store instruction.  */
  if (!unaligned_p && (length & 3) != 0)
    num++;

  /* Store the first 16 bytes using vst1:v16qi for the aligned case.  */
  if (!unaligned_p && mode == V16QImode)
    num--;

  return (num <= arm_block_set_max_insns ());
}

/* Set a block of memory using vectorization instructions for the
   unaligned case.  We fill the first LENGTH bytes of the memory
   area starting from DSTBASE with byte constant VALUE.  ALIGN is
   the alignment requirement of memory.  Return TRUE if succeeded.  */
static bool
arm_block_set_unaligned_vect (rtx dstbase,
			      unsigned HOST_WIDE_INT length,
			      unsigned HOST_WIDE_INT value,
			      unsigned HOST_WIDE_INT align)
{
  unsigned int i, nelt_v16, nelt_v8, nelt_mode;
  rtx dst, mem;
  rtx val_vec, reg;
  rtx (*gen_func) (rtx, rtx);
  machine_mode mode;
  unsigned HOST_WIDE_INT v = value;
  unsigned int offset = 0;
  gcc_assert ((align & 0x3) != 0);
  nelt_v8 = GET_MODE_NUNITS (V8QImode);
  nelt_v16 = GET_MODE_NUNITS (V16QImode);
  if (length >= nelt_v16)
    {
      mode = V16QImode;
      gen_func = gen_movmisalignv16qi;
    }
  else
    {
      mode = V8QImode;
      gen_func = gen_movmisalignv8qi;
    }
  nelt_mode = GET_MODE_NUNITS (mode);
  gcc_assert (length >= nelt_mode);
  /* Skip if it isn't profitable.  */
  if (!arm_block_set_vect_profit_p (length, align, mode))
    return false;

  dst = copy_addr_to_reg (XEXP (dstbase, 0));
  mem = adjust_automodify_address (dstbase, mode, dst, offset);

  v = sext_hwi (v, BITS_PER_WORD);

  reg = gen_reg_rtx (mode);
  val_vec = gen_const_vec_duplicate (mode, GEN_INT (v));
  /* Emit instruction loading the constant value.  */
  emit_move_insn (reg, val_vec);

  /* Handle nelt_mode bytes in a vector.  */
  for (i = 0; (i + nelt_mode <= length); i += nelt_mode)
    {
      emit_insn ((*gen_func) (mem, reg));
      if (i + 2 * nelt_mode <= length)
	{
	  emit_insn (gen_add2_insn (dst, GEN_INT (nelt_mode)));
	  offset += nelt_mode;
	  mem = adjust_automodify_address (dstbase, mode, dst, offset);
	}
    }

  /* If there are not less than nelt_v8 bytes leftover, we must be in
     V16QI mode.  */
  gcc_assert ((i + nelt_v8) > length || mode == V16QImode);

  /* Handle (8, 16) bytes leftover.  */
  if (i + nelt_v8 < length)
    {
      emit_insn (gen_add2_insn (dst, GEN_INT (length - i)));
      offset += length - i;
      mem = adjust_automodify_address (dstbase, mode, dst, offset);

      /* We are shifting bytes back, set the alignment accordingly.  */
      if ((length & 1) != 0 && align >= 2)
	set_mem_align (mem, BITS_PER_UNIT);

      emit_insn (gen_movmisalignv16qi (mem, reg));
    }
  /* Handle (0, 8] bytes leftover.  */
  else if (i < length && i + nelt_v8 >= length)
    {
      if (mode == V16QImode)
	reg = gen_lowpart (V8QImode, reg);

      emit_insn (gen_add2_insn (dst, GEN_INT ((length - i)
					      + (nelt_mode - nelt_v8))));
      offset += (length - i) + (nelt_mode - nelt_v8);
      mem = adjust_automodify_address (dstbase, V8QImode, dst, offset);

      /* We are shifting bytes back, set the alignment accordingly.  */
      if ((length & 1) != 0 && align >= 2)
	set_mem_align (mem, BITS_PER_UNIT);

      emit_insn (gen_movmisalignv8qi (mem, reg));
    }

  return true;
}

/* Set a block of memory using vectorization instructions for the
   aligned case.  We fill the first LENGTH bytes of the memory area
   starting from DSTBASE with byte constant VALUE.  ALIGN is the
   alignment requirement of memory.  Return TRUE if succeeded.  */
static bool
arm_block_set_aligned_vect (rtx dstbase,
			    unsigned HOST_WIDE_INT length,
			    unsigned HOST_WIDE_INT value,
			    unsigned HOST_WIDE_INT align)
{
  unsigned int i, nelt_v8, nelt_v16, nelt_mode;
  rtx dst, addr, mem;
  rtx val_vec, reg;
  machine_mode mode;
  unsigned HOST_WIDE_INT v = value;
  unsigned int offset = 0;

  gcc_assert ((align & 0x3) == 0);
  nelt_v8 = GET_MODE_NUNITS (V8QImode);
  nelt_v16 = GET_MODE_NUNITS (V16QImode);
  if (length >= nelt_v16 && unaligned_access && !BYTES_BIG_ENDIAN)
    mode = V16QImode;
  else
    mode = V8QImode;

  nelt_mode = GET_MODE_NUNITS (mode);
  gcc_assert (length >= nelt_mode);
  /* Skip if it isn't profitable.  */
  if (!arm_block_set_vect_profit_p (length, align, mode))
    return false;

  dst = copy_addr_to_reg (XEXP (dstbase, 0));

  v = sext_hwi (v, BITS_PER_WORD);

  reg = gen_reg_rtx (mode);
  val_vec = gen_const_vec_duplicate (mode, GEN_INT (v));
  /* Emit instruction loading the constant value.  */
  emit_move_insn (reg, val_vec);

  i = 0;
  /* Handle first 16 bytes specially using vst1:v16qi instruction.  */
  if (mode == V16QImode)
    {
      mem = adjust_automodify_address (dstbase, mode, dst, offset);
      emit_insn (gen_movmisalignv16qi (mem, reg));
      i += nelt_mode;
      /* Handle (8, 16) bytes leftover using vst1:v16qi again.  */
      if (i + nelt_v8 < length && i + nelt_v16 > length)
	{
	  emit_insn (gen_add2_insn (dst, GEN_INT (length - nelt_mode)));
	  offset += length - nelt_mode;
	  mem = adjust_automodify_address (dstbase, mode, dst, offset);
	  /* We are shifting bytes back, set the alignment accordingly.  */
	  if ((length & 0x3) == 0)
	    set_mem_align (mem, BITS_PER_UNIT * 4);
	  else if ((length & 0x1) == 0)
	    set_mem_align (mem, BITS_PER_UNIT * 2);
	  else
	    set_mem_align (mem, BITS_PER_UNIT);

	  emit_insn (gen_movmisalignv16qi (mem, reg));
	  return true;
	}
      /* Fall through for bytes leftover.  */
      mode = V8QImode;
      nelt_mode = GET_MODE_NUNITS (mode);
      reg = gen_lowpart (V8QImode, reg);
    }

  /* Handle 8 bytes in a vector.  */
  for (; (i + nelt_mode <= length); i += nelt_mode)
    {
      addr = plus_constant (Pmode, dst, i);
      mem = adjust_automodify_address (dstbase, mode, addr, offset + i);
      emit_move_insn (mem, reg);
    }

  /* Handle single word leftover by shifting 4 bytes back.  We can
     use aligned access for this case.  */
  if (i + UNITS_PER_WORD == length)
    {
      addr = plus_constant (Pmode, dst, i - UNITS_PER_WORD);
      offset += i - UNITS_PER_WORD;
      mem = adjust_automodify_address (dstbase, mode, addr, offset);
      /* We are shifting 4 bytes back, set the alignment accordingly.  */
      if (align > UNITS_PER_WORD)
	set_mem_align (mem, BITS_PER_UNIT * UNITS_PER_WORD);

      emit_move_insn (mem, reg);
    }
  /* Handle (0, 4), (4, 8) bytes leftover by shifting bytes back.
     We have to use unaligned access for this case.  */
  else if (i < length)
    {
      emit_insn (gen_add2_insn (dst, GEN_INT (length - nelt_mode)));
      offset += length - nelt_mode;
      mem = adjust_automodify_address (dstbase, mode, dst, offset);
      /* We are shifting bytes back, set the alignment accordingly.  */
      if ((length & 1) == 0)
	set_mem_align (mem, BITS_PER_UNIT * 2);
      else
	set_mem_align (mem, BITS_PER_UNIT);

      emit_insn (gen_movmisalignv8qi (mem, reg));
    }

  return true;
}

/* Set a block of memory using plain strh/strb instructions, only
   using instructions allowed by ALIGN on processor.  We fill the
   first LENGTH bytes of the memory area starting from DSTBASE
   with byte constant VALUE.  ALIGN is the alignment requirement
   of memory.  */
static bool
arm_block_set_unaligned_non_vect (rtx dstbase,
				  unsigned HOST_WIDE_INT length,
				  unsigned HOST_WIDE_INT value,
				  unsigned HOST_WIDE_INT align)
{
  unsigned int i;
  rtx dst, addr, mem;
  rtx val_exp, val_reg, reg;
  machine_mode mode;
  HOST_WIDE_INT v = value;

  gcc_assert (align == 1 || align == 2);

  if (align == 2)
    v |= (value << BITS_PER_UNIT);

  v = sext_hwi (v, BITS_PER_WORD);
  val_exp = GEN_INT (v);
  /* Skip if it isn't profitable.  */
  if (!arm_block_set_non_vect_profit_p (val_exp, length,
					align, true, false))
    return false;

  dst = copy_addr_to_reg (XEXP (dstbase, 0));
  mode = (align == 2 ? HImode : QImode);
  val_reg = force_reg (SImode, val_exp);
  reg = gen_lowpart (mode, val_reg);

  for (i = 0; (i + GET_MODE_SIZE (mode) <= length); i += GET_MODE_SIZE (mode))
    {
      addr = plus_constant (Pmode, dst, i);
      mem = adjust_automodify_address (dstbase, mode, addr, i);
      emit_move_insn (mem, reg);
    }

  /* Handle single byte leftover.  */
  if (i + 1 == length)
    {
      reg = gen_lowpart (QImode, val_reg);
      addr = plus_constant (Pmode, dst, i);
      mem = adjust_automodify_address (dstbase, QImode, addr, i);
      emit_move_insn (mem, reg);
      i++;
    }

  gcc_assert (i == length);
  return true;
}

/* Set a block of memory using plain strd/str/strh/strb instructions,
   to permit unaligned copies on processors which support unaligned
   semantics for those instructions.  We fill the first LENGTH bytes
   of the memory area starting from DSTBASE with byte constant VALUE.
   ALIGN is the alignment requirement of memory.  */
static bool
arm_block_set_aligned_non_vect (rtx dstbase,
				unsigned HOST_WIDE_INT length,
				unsigned HOST_WIDE_INT value,
				unsigned HOST_WIDE_INT align)
{
  unsigned int i;
  rtx dst, addr, mem;
  rtx val_exp, val_reg, reg;
  unsigned HOST_WIDE_INT v;
  bool use_strd_p;

  use_strd_p = (length >= 2 * UNITS_PER_WORD && (align & 3) == 0
		&& TARGET_LDRD && current_tune->prefer_ldrd_strd);

  v = (value | (value << 8) | (value << 16) | (value << 24));
  if (length < UNITS_PER_WORD)
    v &= (0xFFFFFFFF >> (UNITS_PER_WORD - length) * BITS_PER_UNIT);

  if (use_strd_p)
    v |= (v << BITS_PER_WORD);
  else
    v = sext_hwi (v, BITS_PER_WORD);

  val_exp = GEN_INT (v);
  /* Skip if it isn't profitable.  */
  if (!arm_block_set_non_vect_profit_p (val_exp, length,
					align, false, use_strd_p))
    {
      if (!use_strd_p)
	return false;

      /* Try without strd.  */
      v = (v >> BITS_PER_WORD);
      v = sext_hwi (v, BITS_PER_WORD);
      val_exp = GEN_INT (v);
      use_strd_p = false;
      if (!arm_block_set_non_vect_profit_p (val_exp, length,
					    align, false, use_strd_p))
	return false;
    }

  i = 0;
  dst = copy_addr_to_reg (XEXP (dstbase, 0));
  /* Handle double words using strd if possible.  */
  if (use_strd_p)
    {
      val_reg = force_reg (DImode, val_exp);
      reg = val_reg;
      for (; (i + 8 <= length); i += 8)
	{
	  addr = plus_constant (Pmode, dst, i);
	  mem = adjust_automodify_address (dstbase, DImode, addr, i);
	  emit_move_insn (mem, reg);
	}
    }
  else
    val_reg = force_reg (SImode, val_exp);

  /* Handle words.  */
  reg = (use_strd_p ? gen_lowpart (SImode, val_reg) : val_reg);
  for (; (i + 4 <= length); i += 4)
    {
      addr = plus_constant (Pmode, dst, i);
      mem = adjust_automodify_address (dstbase, SImode, addr, i);
      if ((align & 3) == 0)
	emit_move_insn (mem, reg);
      else
	emit_insn (gen_unaligned_storesi (mem, reg));
    }

  /* Merge last pair of STRH and STRB into a STR if possible.  */
  if (unaligned_access && i > 0 && (i + 3) == length)
    {
      addr = plus_constant (Pmode, dst, i - 1);
      mem = adjust_automodify_address (dstbase, SImode, addr, i - 1);
      /* We are shifting one byte back, set the alignment accordingly.  */
      if ((align & 1) == 0)
	set_mem_align (mem, BITS_PER_UNIT);

      /* Most likely this is an unaligned access, and we can't tell at
	 compilation time.  */
      emit_insn (gen_unaligned_storesi (mem, reg));
      return true;
    }

  /* Handle half word leftover.  */
  if (i + 2 <= length)
    {
      reg = gen_lowpart (HImode, val_reg);
      addr = plus_constant (Pmode, dst, i);
      mem = adjust_automodify_address (dstbase, HImode, addr, i);
      if ((align & 1) == 0)
	emit_move_insn (mem, reg);
      else
	emit_insn (gen_unaligned_storehi (mem, reg));

      i += 2;
    }

  /* Handle single byte leftover.  */
  if (i + 1 == length)
    {
      reg = gen_lowpart (QImode, val_reg);
      addr = plus_constant (Pmode, dst, i);
      mem = adjust_automodify_address (dstbase, QImode, addr, i);
      emit_move_insn (mem, reg);
    }

  return true;
}

/* Set a block of memory using vectorization instructions for both
   aligned and unaligned cases.  We fill the first LENGTH bytes of
   the memory area starting from DSTBASE with byte constant VALUE.
   ALIGN is the alignment requirement of memory.  */
static bool
arm_block_set_vect (rtx dstbase,
		    unsigned HOST_WIDE_INT length,
		    unsigned HOST_WIDE_INT value,
		    unsigned HOST_WIDE_INT align)
{
  /* Check whether we need to use unaligned store instruction.  */
  if (((align & 3) != 0 || (length & 3) != 0)
      /* Check whether unaligned store instruction is available.  */
      && (!unaligned_access || BYTES_BIG_ENDIAN))
    return false;

  if ((align & 3) == 0)
    return arm_block_set_aligned_vect (dstbase, length, value, align);
  else
    return arm_block_set_unaligned_vect (dstbase, length, value, align);
}

/* Expand string store operation.  Firstly we try to do that by using
   vectorization instructions, then try with ARM unaligned access and
   double-word store if profitable.  OPERANDS[0] is the destination,
   OPERANDS[1] is the number of bytes, operands[2] is the value to
   initialize the memory, OPERANDS[3] is the known alignment of the
   destination.  */
bool
arm_gen_setmem (rtx *operands)
{
  rtx dstbase = operands[0];
  unsigned HOST_WIDE_INT length;
  unsigned HOST_WIDE_INT value;
  unsigned HOST_WIDE_INT align;

  if (!CONST_INT_P (operands[2]) || !CONST_INT_P (operands[1]))
    return false;

  length = UINTVAL (operands[1]);
  if (length > 64)
    return false;

  value = (UINTVAL (operands[2]) & 0xFF);
  align = UINTVAL (operands[3]);
  if (TARGET_NEON && length >= 8
      && current_tune->string_ops_prefer_neon
      && arm_block_set_vect (dstbase, length, value, align))
    return true;

  if (!unaligned_access && (align & 3) != 0)
    return arm_block_set_unaligned_non_vect (dstbase, length, value, align);

  return arm_block_set_aligned_non_vect (dstbase, length, value, align);
}


static bool
arm_macro_fusion_p (void)
{
  return current_tune->fusible_ops != tune_params::FUSE_NOTHING;
}

/* Return true if the two back-to-back sets PREV_SET, CURR_SET are suitable
   for MOVW / MOVT macro fusion.  */

static bool
arm_sets_movw_movt_fusible_p (rtx prev_set, rtx curr_set)
{
  /* We are trying to fuse
     movw imm / movt imm
    instructions as a group that gets scheduled together.  */

  rtx set_dest = SET_DEST (curr_set);

  if (GET_MODE (set_dest) != SImode)
    return false;

  /* We are trying to match:
     prev (movw)  == (set (reg r0) (const_int imm16))
     curr (movt) == (set (zero_extract (reg r0)
					(const_int 16)
					(const_int 16))
			  (const_int imm16_1))
     or
     prev (movw) == (set (reg r1)
			  (high (symbol_ref ("SYM"))))
    curr (movt) == (set (reg r0)
			(lo_sum (reg r1)
				(symbol_ref ("SYM"))))  */

    if (GET_CODE (set_dest) == ZERO_EXTRACT)
      {
	if (CONST_INT_P (SET_SRC (curr_set))
	    && CONST_INT_P (SET_SRC (prev_set))
	    && REG_P (XEXP (set_dest, 0))
	    && REG_P (SET_DEST (prev_set))
	    && REGNO (XEXP (set_dest, 0)) == REGNO (SET_DEST (prev_set)))
	  return true;

      }
    else if (GET_CODE (SET_SRC (curr_set)) == LO_SUM
	     && REG_P (SET_DEST (curr_set))
	     && REG_P (SET_DEST (prev_set))
	     && GET_CODE (SET_SRC (prev_set)) == HIGH
	     && REGNO (SET_DEST (curr_set)) == REGNO (SET_DEST (prev_set)))
      return true;

  return false;
}

static bool
aarch_macro_fusion_pair_p (rtx_insn* prev, rtx_insn* curr)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);

  if (!prev_set
      || !curr_set)
    return false;

  if (any_condjump_p (curr))
    return false;

  if (!arm_macro_fusion_p ())
    return false;

  if (current_tune->fusible_ops & tune_params::FUSE_AES_AESMC
      && aarch_crypto_can_dual_issue (prev, curr))
    return true;

  if (current_tune->fusible_ops & tune_params::FUSE_MOVW_MOVT
      && arm_sets_movw_movt_fusible_p (prev_set, curr_set))
    return true;

  return false;
}

/* Return true iff the instruction fusion described by OP is enabled.  */
bool
arm_fusion_enabled_p (tune_params::fuse_ops op)
{
  return current_tune->fusible_ops & op;
}

/* Implement TARGET_SCHED_CAN_SPECULATE_INSN.  Return true if INSN can be
   scheduled for speculative execution.  Reject the long-running division
   and square-root instructions.  */

static bool
arm_sched_can_speculate_insn (rtx_insn *insn)
{
  switch (get_attr_type (insn))
    {
      case TYPE_SDIV:
      case TYPE_UDIV:
      case TYPE_FDIVS:
      case TYPE_FDIVD:
      case TYPE_FSQRTS:
      case TYPE_FSQRTD:
      case TYPE_NEON_FP_SQRT_S:
      case TYPE_NEON_FP_SQRT_D:
      case TYPE_NEON_FP_SQRT_S_Q:
      case TYPE_NEON_FP_SQRT_D_Q:
      case TYPE_NEON_FP_DIV_S:
      case TYPE_NEON_FP_DIV_D:
      case TYPE_NEON_FP_DIV_S_Q:
      case TYPE_NEON_FP_DIV_D_Q:
	return false;
      default:
	return true;
    }
}

/* Implement the TARGET_ASAN_SHADOW_OFFSET hook.  */

static unsigned HOST_WIDE_INT
arm_asan_shadow_offset (void)
{
  return HOST_WIDE_INT_1U << 29;
}


/* This is a temporary fix for PR60655.  Ideally we need
   to handle most of these cases in the generic part but
   currently we reject minus (..) (sym_ref).  We try to 
   ameliorate the case with minus (sym_ref1) (sym_ref2)
   where they are in the same section.  */

static bool
arm_const_not_ok_for_debug_p (rtx p)
{
  tree decl_op0 = NULL;
  tree decl_op1 = NULL;

  if (GET_CODE (p) == UNSPEC)
    return true;
  if (GET_CODE (p) == MINUS)
    {
      if (GET_CODE (XEXP (p, 1)) == SYMBOL_REF)
	{
	  decl_op1 = SYMBOL_REF_DECL (XEXP (p, 1));
	  if (decl_op1
	      && GET_CODE (XEXP (p, 0)) == SYMBOL_REF
	      && (decl_op0 = SYMBOL_REF_DECL (XEXP (p, 0))))
	    {
	      if ((VAR_P (decl_op1)
		   || TREE_CODE (decl_op1) == CONST_DECL)
		  && (VAR_P (decl_op0)
		      || TREE_CODE (decl_op0) == CONST_DECL))
		return (get_variable_section (decl_op1, false)
			!= get_variable_section (decl_op0, false));

	      if (TREE_CODE (decl_op1) == LABEL_DECL
		  && TREE_CODE (decl_op0) == LABEL_DECL)
		return (DECL_CONTEXT (decl_op1)
			!= DECL_CONTEXT (decl_op0));
	    }

	  return true;
	}
    }

  return false;
}

/* return TRUE if x is a reference to a value in a constant pool */
extern bool
arm_is_constant_pool_ref (rtx x)
{
  return (MEM_P (x)
	  && GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)));
}

/* Remember the last target of arm_set_current_function.  */
static GTY(()) tree arm_previous_fndecl;

/* Restore or save the TREE_TARGET_GLOBALS from or to NEW_TREE.  */

void
save_restore_target_globals (tree new_tree)
{
  /* If we have a previous state, use it.  */
  if (TREE_TARGET_GLOBALS (new_tree))
    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
  else if (new_tree == target_option_default_node)
    restore_target_globals (&default_target_globals);
  else
    {
      /* Call target_reinit and save the state for TARGET_GLOBALS.  */
      TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
    }

  arm_option_params_internal ();
}

/* Invalidate arm_previous_fndecl.  */

void
arm_reset_previous_fndecl (void)
{
  arm_previous_fndecl = NULL_TREE;
}

/* Establish appropriate back-end context for processing the function
   FNDECL.  The argument might be NULL to indicate processing at top
   level, outside of any function scope.  */

static void
arm_set_current_function (tree fndecl)
{
  if (!fndecl || fndecl == arm_previous_fndecl)
    return;

  tree old_tree = (arm_previous_fndecl
		   ? DECL_FUNCTION_SPECIFIC_TARGET (arm_previous_fndecl)
		   : NULL_TREE);

  tree new_tree = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  /* If current function has no attributes but previous one did,
     use the default node.  */
  if (! new_tree && old_tree)
    new_tree = target_option_default_node;

  /* If nothing to do return.  #pragma GCC reset or #pragma GCC pop to
     the default have been handled by save_restore_target_globals from
     arm_pragma_target_parse.  */
  if (old_tree == new_tree)
    return;

  arm_previous_fndecl = fndecl;

  /* First set the target options.  */
  cl_target_option_restore (&global_options, TREE_TARGET_OPTION (new_tree));

  save_restore_target_globals (new_tree);
}

/* Implement TARGET_OPTION_PRINT.  */

static void
arm_option_print (FILE *file, int indent, struct cl_target_option *ptr)
{
  int flags = ptr->x_target_flags;
  const char *fpu_name;

  fpu_name = (ptr->x_arm_fpu_index == TARGET_FPU_auto
	      ? "auto" : all_fpus[ptr->x_arm_fpu_index].name);

  fprintf (file, "%*sselected isa %s\n", indent, "",
	   TARGET_THUMB2_P (flags) ? "thumb2" :
	   TARGET_THUMB_P (flags) ? "thumb1" :
	   "arm");

  if (ptr->x_arm_arch_string)
    fprintf (file, "%*sselected architecture %s\n", indent, "",
	     ptr->x_arm_arch_string);

  if (ptr->x_arm_cpu_string)
    fprintf (file, "%*sselected CPU %s\n", indent, "",
	     ptr->x_arm_cpu_string);

  if (ptr->x_arm_tune_string)
    fprintf (file, "%*sselected tune %s\n", indent, "",
	     ptr->x_arm_tune_string);

  fprintf (file, "%*sselected fpu %s\n", indent, "", fpu_name);
}

/* Hook to determine if one function can safely inline another.  */

static bool
arm_can_inline_p (tree caller, tree callee)
{
  tree caller_tree = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (callee);
  bool can_inline = true;

  struct cl_target_option *caller_opts
	= TREE_TARGET_OPTION (caller_tree ? caller_tree
					   : target_option_default_node);

  struct cl_target_option *callee_opts
	= TREE_TARGET_OPTION (callee_tree ? callee_tree
					   : target_option_default_node);

  if (callee_opts == caller_opts)
    return true;

  /* Callee's ISA features should be a subset of the caller's.  */
  struct arm_build_target caller_target;
  struct arm_build_target callee_target;
  caller_target.isa = sbitmap_alloc (isa_num_bits);
  callee_target.isa = sbitmap_alloc (isa_num_bits);

  arm_configure_build_target (&caller_target, caller_opts, &global_options_set,
			      false);
  arm_configure_build_target (&callee_target, callee_opts, &global_options_set,
			      false);
  if (!bitmap_subset_p (callee_target.isa, caller_target.isa))
    can_inline = false;

  sbitmap_free (caller_target.isa);
  sbitmap_free (callee_target.isa);

  /* OK to inline between different modes.
     Function with mode specific instructions, e.g using asm,
     must be explicitly protected with noinline.  */
  return can_inline;
}

/* Hook to fix function's alignment affected by target attribute.  */

static void
arm_relayout_function (tree fndecl)
{
  if (DECL_USER_ALIGN (fndecl))
    return;

  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  if (!callee_tree)
    callee_tree = target_option_default_node;

  struct cl_target_option *opts = TREE_TARGET_OPTION (callee_tree);
  SET_DECL_ALIGN
    (fndecl,
     FUNCTION_ALIGNMENT (FUNCTION_BOUNDARY_P (opts->x_target_flags)));
}

/* Inner function to process the attribute((target(...))), take an argument and
   set the current options from the argument.  If we have a list, recursively
   go over the list.  */

static bool
arm_valid_target_attribute_rec (tree args, struct gcc_options *opts)
{
  if (TREE_CODE (args) == TREE_LIST)
    {
      bool ret = true;

      for (; args; args = TREE_CHAIN (args))
	if (TREE_VALUE (args)
	    && !arm_valid_target_attribute_rec (TREE_VALUE (args), opts))
	  ret = false;
      return ret;
    }

  else if (TREE_CODE (args) != STRING_CST)
    {
      error ("attribute %<target%> argument not a string");
      return false;
    }

  char *argstr = ASTRDUP (TREE_STRING_POINTER (args));
  char *q;

  while ((q = strtok (argstr, ",")) != NULL)
    {
      while (ISSPACE (*q)) ++q;

      argstr = NULL;
      if (!strncmp (q, "thumb", 5))
	  opts->x_target_flags |= MASK_THUMB;

      else if (!strncmp (q, "arm", 3))
	  opts->x_target_flags &= ~MASK_THUMB;

      else if (!strncmp (q, "fpu=", 4))
	{
	  int fpu_index;
	  if (! opt_enum_arg_to_value (OPT_mfpu_, q+4,
				       &fpu_index, CL_TARGET))
	    {
	      error ("invalid fpu for target attribute or pragma %qs", q);
	      return false;
	    }
	  if (fpu_index == TARGET_FPU_auto)
	    {
	      /* This doesn't really make sense until we support
		 general dynamic selection of the architecture and all
		 sub-features.  */
	      sorry ("auto fpu selection not currently permitted here");
	      return false;
	    }
	  opts->x_arm_fpu_index = (enum fpu_type) fpu_index;
	}
      else if (!strncmp (q, "arch=", 5))
	{
	  char* arch = q+5;
	  const arch_option *arm_selected_arch
	     = arm_parse_arch_option_name (all_architectures, "arch", arch);

	  if (!arm_selected_arch)
	    {
	      error ("invalid architecture for target attribute or pragma %qs",
		     q);
	      return false;
	    }

	  opts->x_arm_arch_string = xstrndup (arch, strlen (arch));
	}
      else if (q[0] == '+')
	{
	  opts->x_arm_arch_string
	    = xasprintf ("%s%s", opts->x_arm_arch_string, q);
	}
      else
	{
	  error ("unknown target attribute or pragma %qs", q);
	  return false;
	}
    }

  return true;
}

/* Return a TARGET_OPTION_NODE tree of the target options listed or NULL.  */

tree
arm_valid_target_attribute_tree (tree args, struct gcc_options *opts,
				 struct gcc_options *opts_set)
{
  struct cl_target_option cl_opts;

  if (!arm_valid_target_attribute_rec (args, opts))
    return NULL_TREE;

  cl_target_option_save (&cl_opts, opts);
  arm_configure_build_target (&arm_active_target, &cl_opts, opts_set, false);
  arm_option_check_internal (opts);
  /* Do any overrides, such as global options arch=xxx.
     We do this since arm_active_target was overridden.  */
  arm_option_reconfigure_globals ();
  arm_options_perform_arch_sanity_checks ();
  arm_option_override_internal (opts, opts_set);

  return build_target_option_node (opts);
}

static void 
add_attribute  (const char * mode, tree *attributes)
{
  size_t len = strlen (mode);
  tree value = build_string (len, mode);

  TREE_TYPE (value) = build_array_type (char_type_node,
					build_index_type (size_int (len)));

  *attributes = tree_cons (get_identifier ("target"),
			   build_tree_list (NULL_TREE, value),
			   *attributes);
}

/* For testing. Insert thumb or arm modes alternatively on functions.  */

static void
arm_insert_attributes (tree fndecl, tree * attributes)
{
  const char *mode;

  if (! TARGET_FLIP_THUMB)
    return;

  if (TREE_CODE (fndecl) != FUNCTION_DECL || DECL_EXTERNAL(fndecl)
      || DECL_BUILT_IN (fndecl) || DECL_ARTIFICIAL (fndecl))
   return;

  /* Nested definitions must inherit mode.  */
  if (current_function_decl)
   {
     mode = TARGET_THUMB ? "thumb" : "arm";      
     add_attribute (mode, attributes);
     return;
   }

  /* If there is already a setting don't change it.  */
  if (lookup_attribute ("target", *attributes) != NULL)
    return;

  mode = thumb_flipper ? "thumb" : "arm";
  add_attribute (mode, attributes);

  thumb_flipper = !thumb_flipper;
}

/* Hook to validate attribute((target("string"))).  */

static bool
arm_valid_target_attribute_p (tree fndecl, tree ARG_UNUSED (name),
			      tree args, int ARG_UNUSED (flags))
{
  bool ret = true;
  struct gcc_options func_options;
  tree cur_tree, new_optimize;
  gcc_assert ((fndecl != NULL_TREE) && (args != NULL_TREE));

  /* Get the optimization options of the current function.  */
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  /* If the function changed the optimization levels as well as setting target
     options, start with the optimizations specified.  */
  if (!func_optimize)
    func_optimize = optimization_default_node;

  /* Init func_options.  */
  memset (&func_options, 0, sizeof (func_options));
  init_options_struct (&func_options, NULL);
  lang_hooks.init_options_struct (&func_options);

  /* Initialize func_options to the defaults.  */
  cl_optimization_restore (&func_options,
			   TREE_OPTIMIZATION (func_optimize));

  cl_target_option_restore (&func_options,
			    TREE_TARGET_OPTION (target_option_default_node));

  /* Set func_options flags with new target mode.  */
  cur_tree = arm_valid_target_attribute_tree (args, &func_options,
					      &global_options_set);

  if (cur_tree == NULL_TREE)
    ret = false;

  new_optimize = build_optimization_node (&func_options);

  DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = cur_tree;

  DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;

  finalize_options_struct (&func_options);

  return ret;
}

/* Match an ISA feature bitmap to a named FPU.  We always use the
   first entry that exactly matches the feature set, so that we
   effectively canonicalize the FPU name for the assembler.  */
static const char*
arm_identify_fpu_from_isa (sbitmap isa)
{
  auto_sbitmap fpubits (isa_num_bits);
  auto_sbitmap cand_fpubits (isa_num_bits);

  bitmap_and (fpubits, isa, isa_all_fpubits);

  /* If there are no ISA feature bits relating to the FPU, we must be
     doing soft-float.  */
  if (bitmap_empty_p (fpubits))
    return "softvfp";

  for (unsigned int i = 0; i < TARGET_FPU_auto; i++)
    {
      arm_initialize_isa (cand_fpubits, all_fpus[i].isa_bits);
      if (bitmap_equal_p (fpubits, cand_fpubits))
	return all_fpus[i].name;
    }
  /* We must find an entry, or things have gone wrong.  */
  gcc_unreachable ();
}

/* Implement ASM_DECLARE_FUNCTION_NAME.  Output the ISA features used
   by the function fndecl.  */
void
arm_declare_function_name (FILE *stream, const char *name, tree decl)
{
  tree target_parts = DECL_FUNCTION_SPECIFIC_TARGET (decl);

  struct cl_target_option *targ_options;
  if (target_parts)
    targ_options = TREE_TARGET_OPTION (target_parts);
  else
    targ_options = TREE_TARGET_OPTION (target_option_current_node);
  gcc_assert (targ_options);

  /* Only update the assembler .arch string if it is distinct from the last
     such string we printed. arch_to_print is set conditionally in case
     targ_options->x_arm_arch_string is NULL which can be the case
     when cc1 is invoked directly without passing -march option.  */
  std::string arch_to_print;
  if (targ_options->x_arm_arch_string)
    arch_to_print = targ_options->x_arm_arch_string;

  if (arch_to_print != arm_last_printed_arch_string)
    {
      std::string arch_name
	= arch_to_print.substr (0, arch_to_print.find ("+"));
      asm_fprintf (asm_out_file, "\t.arch %s\n", arch_name.c_str ());
      const arch_option *arch
	= arm_parse_arch_option_name (all_architectures, "-march",
				      targ_options->x_arm_arch_string);
      auto_sbitmap opt_bits (isa_num_bits);

      gcc_assert (arch);
      if (arch->common.extensions)
	{
	  for (const struct cpu_arch_extension *opt = arch->common.extensions;
	       opt->name != NULL;
	       opt++)
	    {
	      if (!opt->remove)
		{
		  arm_initialize_isa (opt_bits, opt->isa_bits);
		  if (bitmap_subset_p (opt_bits, arm_active_target.isa)
		      && !bitmap_subset_p (opt_bits, isa_all_fpubits))
		    asm_fprintf (asm_out_file, "\t.arch_extension %s\n",
				 opt->name);
		}
	     }
	}

      arm_last_printed_arch_string = arch_to_print;
    }

  fprintf (stream, "\t.syntax unified\n");

  if (TARGET_THUMB)
    {
      if (is_called_in_ARM_mode (decl)
	  || (TARGET_THUMB1 && !TARGET_THUMB1_ONLY
	      && cfun->is_thunk))
	fprintf (stream, "\t.code 32\n");
      else if (TARGET_THUMB1)
	fprintf (stream, "\t.code\t16\n\t.thumb_func\n");
      else
	fprintf (stream, "\t.thumb\n\t.thumb_func\n");
    }
  else
    fprintf (stream, "\t.arm\n");

  std::string fpu_to_print
    = TARGET_SOFT_FLOAT
	? "softvfp" : arm_identify_fpu_from_isa (arm_active_target.isa);

  if (fpu_to_print != arm_last_printed_arch_string)
    {
      asm_fprintf (asm_out_file, "\t.fpu %s\n", fpu_to_print.c_str ());
      arm_last_printed_fpu_string = fpu_to_print;
    }

  if (TARGET_POKE_FUNCTION_NAME)
    arm_poke_function_name (stream, (const char *) name);
}

/* If MEM is in the form of [base+offset], extract the two parts
   of address and set to BASE and OFFSET, otherwise return false
   after clearing BASE and OFFSET.  */

static bool
extract_base_offset_in_addr (rtx mem, rtx *base, rtx *offset)
{
  rtx addr;

  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  /* Strip off const from addresses like (const (addr)).  */
  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == REG)
    {
      *base = addr;
      *offset = const0_rtx;
      return true;
    }

  if (GET_CODE (addr) == PLUS
      && GET_CODE (XEXP (addr, 0)) == REG
      && CONST_INT_P (XEXP (addr, 1)))
    {
      *base = XEXP (addr, 0);
      *offset = XEXP (addr, 1);
      return true;
    }

  *base = NULL_RTX;
  *offset = NULL_RTX;

  return false;
}

/* If INSN is a load or store of address in the form of [base+offset],
   extract the two parts and set to BASE and OFFSET.  IS_LOAD is set
   to TRUE if it's a load.  Return TRUE if INSN is such an instruction,
   otherwise return FALSE.  */

static bool
fusion_load_store (rtx_insn *insn, rtx *base, rtx *offset, bool *is_load)
{
  rtx x, dest, src;

  gcc_assert (INSN_P (insn));
  x = PATTERN (insn);
  if (GET_CODE (x) != SET)
    return false;

  src = SET_SRC (x);
  dest = SET_DEST (x);
  if (GET_CODE (src) == REG && GET_CODE (dest) == MEM)
    {
      *is_load = false;
      extract_base_offset_in_addr (dest, base, offset);
    }
  else if (GET_CODE (src) == MEM && GET_CODE (dest) == REG)
    {
      *is_load = true;
      extract_base_offset_in_addr (src, base, offset);
    }
  else
    return false;

  return (*base != NULL_RTX && *offset != NULL_RTX);
}

/* Implement the TARGET_SCHED_FUSION_PRIORITY hook.

   Currently we only support to fuse ldr or str instructions, so FUSION_PRI
   and PRI are only calculated for these instructions.  For other instruction,
   FUSION_PRI and PRI are simply set to MAX_PRI.  In the future, other kind
   instruction fusion can be supported by returning different priorities.

   It's important that irrelevant instructions get the largest FUSION_PRI.  */

static void
arm_sched_fusion_priority (rtx_insn *insn, int max_pri,
			   int *fusion_pri, int *pri)
{
  int tmp, off_val;
  bool is_load;
  rtx base, offset;

  gcc_assert (INSN_P (insn));

  tmp = max_pri - 1;
  if (!fusion_load_store (insn, &base, &offset, &is_load))
    {
      *pri = tmp;
      *fusion_pri = tmp;
      return;
    }

  /* Load goes first.  */
  if (is_load)
    *fusion_pri = tmp - 1;
  else
    *fusion_pri = tmp - 2;

  tmp /= 2;

  /* INSN with smaller base register goes first.  */
  tmp -= ((REGNO (base) & 0xff) << 20);

  /* INSN with smaller offset goes first.  */
  off_val = (int)(INTVAL (offset));
  if (off_val >= 0)
    tmp -= (off_val & 0xfffff);
  else
    tmp += ((- off_val) & 0xfffff);

  *pri = tmp;
  return;
}


/* Construct and return a PARALLEL RTX vector with elements numbering the
   lanes of either the high (HIGH == TRUE) or low (HIGH == FALSE) half of
   the vector - from the perspective of the architecture.  This does not
   line up with GCC's perspective on lane numbers, so we end up with
   different masks depending on our target endian-ness.  The diagram
   below may help.  We must draw the distinction when building masks
   which select one half of the vector.  An instruction selecting
   architectural low-lanes for a big-endian target, must be described using
   a mask selecting GCC high-lanes.

                 Big-Endian             Little-Endian

GCC             0   1   2   3           3   2   1   0
              | x | x | x | x |       | x | x | x | x |
Architecture    3   2   1   0           3   2   1   0

Low Mask:         { 2, 3 }                { 0, 1 }
High Mask:        { 0, 1 }                { 2, 3 }
*/

rtx
arm_simd_vect_par_cnst_half (machine_mode mode, bool high)
{
  int nunits = GET_MODE_NUNITS (mode);
  rtvec v = rtvec_alloc (nunits / 2);
  int high_base = nunits / 2;
  int low_base = 0;
  int base;
  rtx t1;
  int i;

  if (BYTES_BIG_ENDIAN)
    base = high ? low_base : high_base;
  else
    base = high ? high_base : low_base;

  for (i = 0; i < nunits / 2; i++)
    RTVEC_ELT (v, i) = GEN_INT (base + i);

  t1 = gen_rtx_PARALLEL (mode, v);
  return t1;
}

/* Check OP for validity as a PARALLEL RTX vector with elements
   numbering the lanes of either the high (HIGH == TRUE) or low lanes,
   from the perspective of the architecture.  See the diagram above
   arm_simd_vect_par_cnst_half_p for more details.  */

bool
arm_simd_check_vect_par_cnst_half_p (rtx op, machine_mode mode,
				       bool high)
{
  rtx ideal = arm_simd_vect_par_cnst_half (mode, high);
  HOST_WIDE_INT count_op = XVECLEN (op, 0);
  HOST_WIDE_INT count_ideal = XVECLEN (ideal, 0);
  int i = 0;

  if (!VECTOR_MODE_P (mode))
    return false;

  if (count_op != count_ideal)
    return false;

  for (i = 0; i < count_ideal; i++)
    {
      rtx elt_op = XVECEXP (op, 0, i);
      rtx elt_ideal = XVECEXP (ideal, 0, i);

      if (!CONST_INT_P (elt_op)
	  || INTVAL (elt_ideal) != INTVAL (elt_op))
	return false;
    }
  return true;
}

/* Can output mi_thunk for all cases except for non-zero vcall_offset
   in Thumb1.  */
static bool
arm_can_output_mi_thunk (const_tree, HOST_WIDE_INT, HOST_WIDE_INT vcall_offset,
			 const_tree)
{
  /* For now, we punt and not handle this for TARGET_THUMB1.  */
  if (vcall_offset && TARGET_THUMB1)
    return false;

  /* Otherwise ok.  */
  return true;
}

/* Generate RTL for a conditional branch with rtx comparison CODE in
   mode CC_MODE. The destination of the unlikely conditional branch
   is LABEL_REF.  */

void
arm_gen_unlikely_cbranch (enum rtx_code code, machine_mode cc_mode,
			  rtx label_ref)
{
  rtx x;
  x = gen_rtx_fmt_ee (code, VOIDmode,
		      gen_rtx_REG (cc_mode, CC_REGNUM),
		      const0_rtx);

  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (VOIDmode, label_ref),
			    pc_rtx);
  emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));
}

/* Implement the TARGET_ASM_ELF_FLAGS_NUMERIC hook.

   For pure-code sections there is no letter code for this attribute, so
   output all the section flags numerically when this is needed.  */

static bool
arm_asm_elf_flags_numeric (unsigned int flags, unsigned int *num)
{

  if (flags & SECTION_ARM_PURECODE)
    {
      *num = 0x20000000;

      if (!(flags & SECTION_DEBUG))
	*num |= 0x2;
      if (flags & SECTION_EXCLUDE)
	*num |= 0x80000000;
      if (flags & SECTION_WRITE)
	*num |= 0x1;
      if (flags & SECTION_CODE)
	*num |= 0x4;
      if (flags & SECTION_MERGE)
	*num |= 0x10;
      if (flags & SECTION_STRINGS)
	*num |= 0x20;
      if (flags & SECTION_TLS)
	*num |= 0x400;
      if (HAVE_COMDAT_GROUP && (flags & SECTION_LINKONCE))
	*num |= 0x200;

	return true;
    }

  return false;
}

/* Implement the TARGET_ASM_FUNCTION_SECTION hook.

   If pure-code is passed as an option, make sure all functions are in
   sections that have the SHF_ARM_PURECODE attribute.  */

static section *
arm_function_section (tree decl, enum node_frequency freq,
		      bool startup, bool exit)
{
  const char * section_name;
  section * sec;

  if (!decl || TREE_CODE (decl) != FUNCTION_DECL)
    return default_function_section (decl, freq, startup, exit);

  if (!target_pure_code)
    return default_function_section (decl, freq, startup, exit);


  section_name = DECL_SECTION_NAME (decl);

  /* If a function is not in a named section then it falls under the 'default'
     text section, also known as '.text'.  We can preserve previous behavior as
     the default text section already has the SHF_ARM_PURECODE section
     attribute.  */
  if (!section_name)
    {
      section *default_sec = default_function_section (decl, freq, startup,
						       exit);

      /* If default_sec is not null, then it must be a special section like for
	 example .text.startup.  We set the pure-code attribute and return the
	 same section to preserve existing behavior.  */
      if (default_sec)
	  default_sec->common.flags |= SECTION_ARM_PURECODE;
      return default_sec;
    }

  /* Otherwise look whether a section has already been created with
     'section_name'.  */
  sec = get_named_section (decl, section_name, 0);
  if (!sec)
    /* If that is not the case passing NULL as the section's name to
       'get_named_section' will create a section with the declaration's
       section name.  */
    sec = get_named_section (decl, NULL, 0);

  /* Set the SHF_ARM_PURECODE attribute.  */
  sec->common.flags |= SECTION_ARM_PURECODE;

  return sec;
}

/* Implements the TARGET_SECTION_FLAGS hook.

   If DECL is a function declaration and pure-code is passed as an option
   then add the SFH_ARM_PURECODE attribute to the section flags.  NAME is the
   section's name and RELOC indicates whether the declarations initializer may
   contain runtime relocations.  */

static unsigned int
arm_elf_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags = default_section_type_flags (decl, name, reloc);

  if (decl && TREE_CODE (decl) == FUNCTION_DECL && target_pure_code)
    flags |= SECTION_ARM_PURECODE;

  return flags;
}

/* Generate call to __aeabi_[mode]divmod (op0, op1).  */

static void
arm_expand_divmod_libfunc (rtx libfunc, machine_mode mode,
			   rtx op0, rtx op1,
			   rtx *quot_p, rtx *rem_p)
{
  if (mode == SImode)
    gcc_assert (!TARGET_IDIV);

  scalar_int_mode libval_mode
    = smallest_int_mode_for_size (2 * GET_MODE_BITSIZE (mode));

  rtx libval = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
					libval_mode,
					op0, GET_MODE (op0),
					op1, GET_MODE (op1));

  rtx quotient = simplify_gen_subreg (mode, libval, libval_mode, 0);
  rtx remainder = simplify_gen_subreg (mode, libval, libval_mode,
				       GET_MODE_SIZE (mode));

  gcc_assert (quotient);
  gcc_assert (remainder);

  *quot_p = quotient;
  *rem_p = remainder;
}

/*  This function checks for the availability of the coprocessor builtin passed
    in BUILTIN for the current target.  Returns true if it is available and
    false otherwise.  If a BUILTIN is passed for which this function has not
    been implemented it will cause an exception.  */

bool
arm_coproc_builtin_available (enum unspecv builtin)
{
  /* None of these builtins are available in Thumb mode if the target only
     supports Thumb-1.  */
  if (TARGET_THUMB1)
    return false;

  switch (builtin)
    {
      case VUNSPEC_CDP:
      case VUNSPEC_LDC:
      case VUNSPEC_LDCL:
      case VUNSPEC_STC:
      case VUNSPEC_STCL:
      case VUNSPEC_MCR:
      case VUNSPEC_MRC:
	if (arm_arch4)
	  return true;
	break;
      case VUNSPEC_CDP2:
      case VUNSPEC_LDC2:
      case VUNSPEC_LDC2L:
      case VUNSPEC_STC2:
      case VUNSPEC_STC2L:
      case VUNSPEC_MCR2:
      case VUNSPEC_MRC2:
	/* Only present in ARMv5*, ARMv6 (but not ARMv6-M), ARMv7* and
	   ARMv8-{A,M}.  */
	if (arm_arch5)
	  return true;
	break;
      case VUNSPEC_MCRR:
      case VUNSPEC_MRRC:
	/* Only present in ARMv5TE, ARMv6 (but not ARMv6-M), ARMv7* and
	   ARMv8-{A,M}.  */
	if (arm_arch6 || arm_arch5te)
	  return true;
	break;
      case VUNSPEC_MCRR2:
      case VUNSPEC_MRRC2:
	if (arm_arch6)
	  return true;
	break;
      default:
	gcc_unreachable ();
    }
  return false;
}

/* This function returns true if OP is a valid memory operand for the ldc and
   stc coprocessor instructions and false otherwise.  */

bool
arm_coproc_ldc_stc_legitimate_address (rtx op)
{
  HOST_WIDE_INT range;
  /* Has to be a memory operand.  */
  if (!MEM_P (op))
    return false;

  op = XEXP (op, 0);

  /* We accept registers.  */
  if (REG_P (op))
    return true;

  switch GET_CODE (op)
    {
      case PLUS:
	{
	  /* Or registers with an offset.  */
	  if (!REG_P (XEXP (op, 0)))
	    return false;

	  op = XEXP (op, 1);

	  /* The offset must be an immediate though.  */
	  if (!CONST_INT_P (op))
	    return false;

	  range = INTVAL (op);

	  /* Within the range of [-1020,1020].  */
	  if (!IN_RANGE (range, -1020, 1020))
	    return false;

	  /* And a multiple of 4.  */
	  return (range % 4) == 0;
	}
      case PRE_INC:
      case POST_INC:
      case PRE_DEC:
      case POST_DEC:
	return REG_P (XEXP (op, 0));
      default:
	gcc_unreachable ();
    }
  return false;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.

   In VFPv1, VFP registers could only be accessed in the mode they were
   set, so subregs would be invalid there.  However, we don't support
   VFPv1 at the moment, and the restriction was lifted in VFPv2.

   In big-endian mode, modes greater than word size (i.e. DFmode) are stored in
   VFP registers in little-endian order.  We can't describe that accurately to
   GCC, so avoid taking subregs of such values.

   The only exception is going from a 128-bit to a 64-bit type.  In that
   case the data layout happens to be consistent for big-endian, so we
   explicitly allow that case.  */

static bool
arm_can_change_mode_class (machine_mode from, machine_mode to,
			   reg_class_t rclass)
{
  if (TARGET_BIG_END
      && !(GET_MODE_SIZE (from) == 16 && GET_MODE_SIZE (to) == 8)
      && (GET_MODE_SIZE (from) > UNITS_PER_WORD
	  || GET_MODE_SIZE (to) > UNITS_PER_WORD)
      && reg_classes_intersect_p (VFP_REGS, rclass))
    return false;
  return true;
}

/* Implement TARGET_CONSTANT_ALIGNMENT.  Make strings word-aligned so
   strcpy from constants will be faster.  */

static HOST_WIDE_INT
arm_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  unsigned int factor = (TARGET_THUMB || ! arm_tune_xscale ? 1 : 2);
  if (TREE_CODE (exp) == STRING_CST && !optimize_size)
    return MAX (align, BITS_PER_WORD * factor);
  return align;
}

#if CHECKING_P
namespace selftest {

/* Scan the static data tables generated by parsecpu.awk looking for
   potential issues with the data.  We primarily check for
   inconsistencies in the option extensions at present (extensions
   that duplicate others but aren't marked as aliases).  Furthermore,
   for correct canonicalization later options must never be a subset
   of an earlier option.  Any extension should also only specify other
   feature bits and never an architecture bit.  The architecture is inferred
   from the declaration of the extension.  */
static void
arm_test_cpu_arch_data (void)
{
  const arch_option *arch;
  const cpu_option *cpu;
  auto_sbitmap target_isa (isa_num_bits);
  auto_sbitmap isa1 (isa_num_bits);
  auto_sbitmap isa2 (isa_num_bits);

  for (arch = all_architectures; arch->common.name != NULL; ++arch)
    {
      const cpu_arch_extension *ext1, *ext2;

      if (arch->common.extensions == NULL)
	continue;

      arm_initialize_isa (target_isa, arch->common.isa_bits);

      for (ext1 = arch->common.extensions; ext1->name != NULL; ++ext1)
	{
	  if (ext1->alias)
	    continue;

	  arm_initialize_isa (isa1, ext1->isa_bits);
	  for (ext2 = ext1 + 1; ext2->name != NULL; ++ext2)
	    {
	      if (ext2->alias || ext1->remove != ext2->remove)
		continue;

	      arm_initialize_isa (isa2, ext2->isa_bits);
	      /* If the option is a subset of the parent option, it doesn't
		 add anything and so isn't useful.  */
	      ASSERT_TRUE (!bitmap_subset_p (isa2, isa1));

	      /* If the extension specifies any architectural bits then
		 disallow it.  Extensions should only specify feature bits.  */
	      ASSERT_TRUE (!bitmap_intersect_p (isa2, target_isa));
	    }
	}
    }

  for (cpu = all_cores; cpu->common.name != NULL; ++cpu)
    {
      const cpu_arch_extension *ext1, *ext2;

      if (cpu->common.extensions == NULL)
	continue;

      arm_initialize_isa (target_isa, arch->common.isa_bits);

      for (ext1 = cpu->common.extensions; ext1->name != NULL; ++ext1)
	{
	  if (ext1->alias)
	    continue;

	  arm_initialize_isa (isa1, ext1->isa_bits);
	  for (ext2 = ext1 + 1; ext2->name != NULL; ++ext2)
	    {
	      if (ext2->alias || ext1->remove != ext2->remove)
		continue;

	      arm_initialize_isa (isa2, ext2->isa_bits);
	      /* If the option is a subset of the parent option, it doesn't
		 add anything and so isn't useful.  */
	      ASSERT_TRUE (!bitmap_subset_p (isa2, isa1));

	      /* If the extension specifies any architectural bits then
		 disallow it.  Extensions should only specify feature bits.  */
	      ASSERT_TRUE (!bitmap_intersect_p (isa2, target_isa));
	    }
	}
    }
}

/* Scan the static data tables generated by parsecpu.awk looking for
   potential issues with the data.  Here we check for consistency between the
   fpu bits, in particular we check that ISA_ALL_FPU_INTERNAL does not contain
   a feature bit that is not defined by any FPU flag.  */
static void
arm_test_fpu_data (void)
{
  auto_sbitmap isa_all_fpubits (isa_num_bits);
  auto_sbitmap fpubits (isa_num_bits);
  auto_sbitmap tmpset (isa_num_bits);

  static const enum isa_feature fpu_bitlist[]
    = { ISA_ALL_FPU_INTERNAL, isa_nobit };
  arm_initialize_isa (isa_all_fpubits, fpu_bitlist);

  for (unsigned int i = 0; i < TARGET_FPU_auto; i++)
  {
    arm_initialize_isa (fpubits, all_fpus[i].isa_bits);
    bitmap_and_compl (tmpset, isa_all_fpubits, fpubits);
    bitmap_clear (isa_all_fpubits);
    bitmap_copy (isa_all_fpubits, tmpset);
  }

  if (!bitmap_empty_p (isa_all_fpubits))
    {
	fprintf (stderr, "Error: found feature bits in the ALL_FPU_INTERAL"
			 " group that are not defined by any FPU.\n"
			 "       Check your arm-cpus.in.\n");
	ASSERT_TRUE (bitmap_empty_p (isa_all_fpubits));
    }
}

static void
arm_run_selftests (void)
{
  arm_test_cpu_arch_data ();
  arm_test_fpu_data ();
}
} /* Namespace selftest.  */

#undef TARGET_RUN_TARGET_SELFTESTS
#define TARGET_RUN_TARGET_SELFTESTS selftest::arm_run_selftests
#endif /* CHECKING_P */

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-arm.h"
