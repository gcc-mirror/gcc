/* Output routines for GCC for Renesas / SuperH SH.
   Copyright (C) 1993-2016 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com).
   Improved by Jim Wilson (wilson@cygnus.com).

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

#include <sstream>

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "flags.h"
#include "explow.h"
#include "expr.h"
#include "reload.h"
#include "output.h"
#include "insn-attr.h"
#include "dwarf2.h"
#include "langhooks.h"
#include "cfgrtl.h"
#include "intl.h"
#include "sched-int.h"
#include "gimplify.h"
#include "tm-constrs.h"
#include "opts.h"
#include "tree-pass.h"
#include "context.h"
#include "builtins.h"
#include "rtl-iter.h"

/* This file should be included last.  */
#include "target-def.h"

int code_for_indirect_jump_scratch = CODE_FOR_indirect_jump_scratch;

#define CONST_OK_FOR_ADD(size) CONST_OK_FOR_I08 (size)
#define GEN_MOV (*(gen_movsi))
#define GEN_ADD3 (*(gen_addsi3))
#define GEN_SUB3 (*(gen_subsi3))

/* Used to simplify the logic below.  Find the attributes wherever
   they may be.  */
#define SH_ATTRIBUTES(decl) \
  (TYPE_P (decl)) ? TYPE_ATTRIBUTES (decl) \
		  : DECL_ATTRIBUTES (decl) \
		  ? (DECL_ATTRIBUTES (decl)) \
		  : TYPE_ATTRIBUTES (TREE_TYPE (decl))

/* Set to true by expand_prologue() when the function is an
   interrupt handler.  */
bool current_function_interrupt;

tree sh_deferred_function_attributes;
tree *sh_deferred_function_attributes_tail = &sh_deferred_function_attributes;

/* Global variables for machine-dependent things.  */

/* Which cpu are we scheduling for.  */
enum processor_type sh_cpu;

/* Definitions used in ready queue reordering for first scheduling pass.  */

/* Reg weights arrays for modes SFmode and SImode, indexed by insn LUID.  */
static short *regmode_weight[2];

/* Total SFmode and SImode weights of scheduled insns.  */
static int curr_regmode_pressure[2];

/* Number of r0 life regions.  */
static int r0_life_regions;

/* If true, skip cycles for Q -> R movement.  */
static int skip_cycles = 0;

/* Cached value of can_issue_more.  This is cached in sh_variable_issue hook
   and returned from sh_reorder2.  */
static short cached_can_issue_more;

/* Unique number for UNSPEC_BBR pattern.  */
static unsigned int unspec_bbr_uid = 1;

/* Provides the class number of the smallest class containing
   reg number.  */
enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER] =
{
  R0_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  FP0_REGS,FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  FP_REGS, FP_REGS, FP_REGS, FP_REGS,
  TARGET_REGS, TARGET_REGS, TARGET_REGS, TARGET_REGS,
  TARGET_REGS, TARGET_REGS, TARGET_REGS, TARGET_REGS,
  DF_REGS, DF_REGS, DF_REGS, DF_REGS,
  DF_REGS, DF_REGS, DF_REGS, DF_REGS,
  NO_REGS, GENERAL_REGS, PR_REGS, T_REGS,
  MAC_REGS, MAC_REGS, FPUL_REGS, FPSCR_REGS,
  GENERAL_REGS, GENERAL_REGS,
};

char sh_register_names[FIRST_PSEUDO_REGISTER] \
  [MAX_REGISTER_NAME_LENGTH + 1] = SH_REGISTER_NAMES_INITIALIZER;

char sh_additional_register_names[ADDREGNAMES_SIZE] \
  [MAX_ADDITIONAL_REGISTER_NAME_LENGTH + 1]
  = SH_ADDITIONAL_REGISTER_NAMES_INITIALIZER;

int assembler_dialect;

static void split_branches (rtx_insn *);
static int branch_dest (rtx);
static void print_slot (rtx_sequence *);
static rtx_code_label *add_constant (rtx, machine_mode, rtx);
static void dump_table (rtx_insn *, rtx_insn *);
static bool broken_move (rtx_insn *);
static bool mova_p (rtx_insn *);
static rtx_insn *find_barrier (int, rtx_insn *, rtx_insn *);
static bool noncall_uses_reg (rtx, rtx_insn *, rtx *);
static rtx_insn *gen_block_redirect (rtx_insn *, int, int);
static void sh_reorg (void);
static void sh_option_override (void);
static void sh_override_options_after_change (void);
static void output_stack_adjust (int, rtx, int, HARD_REG_SET *, bool);
static rtx_insn* emit_frame_insn (rtx);
static rtx push (int);
static void pop (int);
static void push_regs (HARD_REG_SET* mask, bool interrupt_handler);
static int calc_live_regs (HARD_REG_SET *);
static HOST_WIDE_INT rounded_frame_size (int);
static bool sh_frame_pointer_required (void);
static void sh_emit_mode_set (int, int, int, HARD_REG_SET);
static int sh_mode_needed (int, rtx_insn *);
static int sh_mode_after (int, int, rtx_insn *);
static int sh_mode_entry (int);
static int sh_mode_exit (int);
static int sh_mode_priority (int entity, int n);

static rtx mark_constant_pool_use (rtx);
static tree sh_handle_interrupt_handler_attribute (tree *, tree, tree,
						   int, bool *);
static tree sh_handle_resbank_handler_attribute (tree *, tree,
						 tree, int, bool *);
static tree sh2a_handle_function_vector_handler_attribute (tree *, tree,
							   tree, int, bool *);
static tree sh_handle_sp_switch_attribute (tree *, tree, tree, int, bool *);
static tree sh_handle_trap_exit_attribute (tree *, tree, tree, int, bool *);
static tree sh_handle_renesas_attribute (tree *, tree, tree, int, bool *);
static void sh_print_operand (FILE *, rtx, int);
static void sh_print_operand_address (FILE *, machine_mode, rtx);
static bool sh_print_operand_punct_valid_p (unsigned char code);
static bool sh_asm_output_addr_const_extra (FILE *file, rtx x);
static void sh_output_function_epilogue (FILE *, HOST_WIDE_INT);
static void sh_insert_attributes (tree, tree *);
static const char *sh_check_pch_target_flags (int);
static int sh_register_move_cost (machine_mode, reg_class_t, reg_class_t);
static int sh_adjust_cost (rtx_insn *, rtx, rtx_insn *, int);
static int sh_issue_rate (void);
static int sh_dfa_new_cycle (FILE *, int, rtx_insn *, int, int, int *sort_p);
static short find_set_regmode_weight (rtx, machine_mode);
static short find_insn_regmode_weight (rtx, machine_mode);
static void find_regmode_weight (basic_block, machine_mode);
static int find_r0_life_regions (basic_block);
static void  sh_md_init_global (FILE *, int, int);
static void  sh_md_finish_global (FILE *, int);
static int rank_for_reorder (const void *, const void *);
static void swap_reorder (rtx_insn **, int);
static void ready_reorder (rtx_insn **, int);
static bool high_pressure (machine_mode);
static int sh_reorder (FILE *, int, rtx_insn **, int *, int);
static int sh_reorder2 (FILE *, int, rtx_insn **, int *, int);
static void sh_md_init (FILE *, int, int);
static int sh_variable_issue (FILE *, int, rtx_insn *, int);

static bool sh_function_ok_for_sibcall (tree, tree);

static bool sh_can_follow_jump (const rtx_insn *, const rtx_insn *);
static bool sh_ms_bitfield_layout_p (const_tree);

static void sh_init_builtins (void);
static tree sh_builtin_decl (unsigned, bool);
static rtx sh_expand_builtin (tree, rtx, rtx, machine_mode, int);
static void sh_output_mi_thunk (FILE *, tree, HOST_WIDE_INT,
				HOST_WIDE_INT, tree);
static void sh_file_start (void);
static bool sh_assemble_integer (rtx, unsigned int, int);
static bool flow_dependent_p (rtx, rtx);
static void flow_dependent_p_1 (rtx, const_rtx, void *);
static int shiftcosts (rtx);
static int and_xor_ior_costs (rtx, int);
static int addsubcosts (rtx);
static int multcosts (rtx);
static bool unspec_caller_rtx_p (rtx);
static bool sh_cannot_copy_insn_p (rtx_insn *);
static bool sh_cannot_force_const_mem_p (machine_mode, rtx);
static bool sh_rtx_costs (rtx, machine_mode, int, int, int *, bool);
static int sh_address_cost (rtx, machine_mode, addr_space_t, bool);
static int sh_pr_n_sets (void);
static rtx sh_allocate_initial_value (rtx);
static reg_class_t sh_preferred_reload_class (rtx, reg_class_t);
static reg_class_t sh_secondary_reload (bool, rtx, reg_class_t,
                                        machine_mode,
                                        struct secondary_reload_info *);
static bool sh_legitimate_address_p (machine_mode, rtx, bool);
static rtx sh_legitimize_address (rtx, rtx, machine_mode);
static rtx sh_delegitimize_address (rtx);
static bool sh_cannot_substitute_mem_equiv_p (rtx);
static bool sh_legitimize_address_displacement (rtx *, rtx *, machine_mode);
static int scavenge_reg (HARD_REG_SET *s);

static rtx sh_struct_value_rtx (tree, int);
static rtx sh_function_value (const_tree, const_tree, bool);
static bool sh_function_value_regno_p (const unsigned int);
static rtx sh_libcall_value (machine_mode, const_rtx);
static bool sh_return_in_memory (const_tree, const_tree);
static rtx sh_builtin_saveregs (void);
static void sh_setup_incoming_varargs (cumulative_args_t, machine_mode,
				       tree, int *, int);
static bool sh_strict_argument_naming (cumulative_args_t);
static bool sh_pretend_outgoing_varargs_named (cumulative_args_t);
static void sh_atomic_assign_expand_fenv (tree *, tree *, tree *);
static tree sh_build_builtin_va_list (void);
static void sh_va_start (tree, rtx);
static tree sh_gimplify_va_arg_expr (tree, tree, gimple_seq *, gimple_seq *);
static bool sh_promote_prototypes (const_tree);
static machine_mode sh_promote_function_mode (const_tree type,
						   machine_mode,
						   int *punsignedp,
						   const_tree funtype,
						   int for_return);
static bool sh_pass_by_reference (cumulative_args_t, machine_mode,
				  const_tree, bool);
static bool sh_callee_copies (cumulative_args_t, machine_mode,
			      const_tree, bool);
static int sh_arg_partial_bytes (cumulative_args_t, machine_mode,
			         tree, bool);
static void sh_function_arg_advance (cumulative_args_t, machine_mode,
				     const_tree, bool);
static rtx sh_function_arg (cumulative_args_t, machine_mode,
			    const_tree, bool);
static int sh_dwarf_calling_convention (const_tree);
static void sh_encode_section_info (tree, rtx, int);
static bool sh2a_function_vector_p (tree);
static void sh_trampoline_init (rtx, tree, rtx);
static rtx sh_trampoline_adjust_address (rtx);
static void sh_conditional_register_usage (void);
static bool sh_legitimate_constant_p (machine_mode, rtx);
static int mov_insn_size (machine_mode, bool);
static int mov_insn_alignment_mask (machine_mode, bool);
static bool sh_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT,
					       unsigned int,
					       enum by_pieces_operation,
					       bool);
static bool sequence_insn_p (rtx_insn *);
static void sh_canonicalize_comparison (int *, rtx *, rtx *, bool);
static void sh_canonicalize_comparison (enum rtx_code&, rtx&, rtx&,
					machine_mode, bool);
static bool sh_legitimate_combined_insn (rtx_insn* insn);

static bool sh_fixed_condition_code_regs (unsigned int* p1, unsigned int* p2);

static void sh_init_sync_libfuncs (void) ATTRIBUTE_UNUSED;

static const struct attribute_spec sh_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { "interrupt_handler", 0, 0, true,  false, false,
    sh_handle_interrupt_handler_attribute, false },
  { "sp_switch",         1, 1, true,  false, false,
     sh_handle_sp_switch_attribute, false },
  { "trap_exit",         1, 1, true,  false, false,
    sh_handle_trap_exit_attribute, false },
  { "renesas",           0, 0, false, true, false,
    sh_handle_renesas_attribute, false },
  { "trapa_handler",     0, 0, true,  false, false,
    sh_handle_interrupt_handler_attribute, false },
  { "nosave_low_regs",   0, 0, true,  false, false,
    sh_handle_interrupt_handler_attribute, false },
  { "resbank",           0, 0, true,  false, false,
    sh_handle_resbank_handler_attribute, false },
  { "function_vector",   1, 1, true,  false, false,
    sh2a_handle_function_vector_handler_attribute, false },
  { NULL,                0, 0, false, false, false, NULL, false }
};

/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE sh_attribute_table

/* The next two are used for debug info when compiling with -gdwarf.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.uaword\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.ualong\t"

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE sh_option_override

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE \
  sh_override_options_after_change

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND sh_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS sh_print_operand_address
#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P sh_print_operand_punct_valid_p
#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA sh_asm_output_addr_const_extra
 
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE sh_output_function_epilogue

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK sh_output_mi_thunk

#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START sh_file_start
#undef TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true

#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER sh_assemble_integer

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST sh_register_move_cost

#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES sh_insert_attributes

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST sh_adjust_cost

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE sh_issue_rate

/* The next 5 hooks have been implemented for reenabling sched1.  With the
   help of these macros we are limiting the movement of insns in sched1 to
   reduce the register pressure.  The overall idea is to keep count of SImode
   and SFmode regs required by already scheduled insns. When these counts
   cross some threshold values; give priority to insns that free registers.
   The insn that frees registers is most likely to be the insn with lowest
   LUID (original insn order); but such an insn might be there in the stalled
   queue (Q) instead of the ready queue (R).  To solve this, we skip cycles
   up to a max of 8 cycles so that such insns may move from Q -> R.

   The description of the hooks are as below:

   TARGET_SCHED_INIT_GLOBAL: Added a new target hook in the generic
   scheduler; it is called inside the sched_init function just after
   find_insn_reg_weights function call. It is used to calculate the SImode
   and SFmode weights of insns of basic blocks; much similar to what
   find_insn_reg_weights does.
   TARGET_SCHED_FINISH_GLOBAL: Corresponding cleanup hook.

   TARGET_SCHED_DFA_NEW_CYCLE: Skip cycles if high register pressure is
   indicated by TARGET_SCHED_REORDER2; doing this may move insns from
   (Q)->(R).

   TARGET_SCHED_REORDER: If the register pressure for SImode or SFmode is
   high; reorder the ready queue so that the insn with lowest LUID will be
   issued next.

   TARGET_SCHED_REORDER2: If the register pressure is high, indicate to
   TARGET_SCHED_DFA_NEW_CYCLE to skip cycles.

   TARGET_SCHED_VARIABLE_ISSUE: Cache the value of can_issue_more so that it
   can be returned from TARGET_SCHED_REORDER2.

   TARGET_SCHED_INIT: Reset the register pressure counting variables.  */

#undef TARGET_SCHED_DFA_NEW_CYCLE
#define TARGET_SCHED_DFA_NEW_CYCLE sh_dfa_new_cycle

#undef TARGET_SCHED_INIT_GLOBAL
#define TARGET_SCHED_INIT_GLOBAL sh_md_init_global

#undef TARGET_SCHED_FINISH_GLOBAL
#define TARGET_SCHED_FINISH_GLOBAL sh_md_finish_global

#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE sh_variable_issue

#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER sh_reorder

#undef TARGET_SCHED_REORDER2
#define TARGET_SCHED_REORDER2 sh_reorder2

#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT sh_md_init

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS sh_delegitimize_address

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS sh_legitimize_address

#undef TARGET_CAN_FOLLOW_JUMP
#define TARGET_CAN_FOLLOW_JUMP sh_can_follow_jump

#undef TARGET_MS_BITFIELD_LAYOUT_P
#define TARGET_MS_BITFIELD_LAYOUT_P sh_ms_bitfield_layout_p

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS sh_init_builtins
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL sh_builtin_decl
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN sh_expand_builtin

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL sh_function_ok_for_sibcall

#undef TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P sh_cannot_copy_insn_p
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS sh_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST sh_address_cost
#undef TARGET_ALLOCATE_INITIAL_VALUE
#define TARGET_ALLOCATE_INITIAL_VALUE sh_allocate_initial_value

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG sh_reorg

#undef TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN sh_dwarf_register_span

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES sh_promote_prototypes
#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE sh_promote_function_mode

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE sh_function_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P sh_function_value_regno_p
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE sh_libcall_value
#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX sh_struct_value_rtx
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY sh_return_in_memory

#undef TARGET_EXPAND_BUILTIN_SAVEREGS
#define TARGET_EXPAND_BUILTIN_SAVEREGS sh_builtin_saveregs
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS sh_setup_incoming_varargs
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING sh_strict_argument_naming
#undef TARGET_PRETEND_OUTGOING_VARARGS_NAMED
#define TARGET_PRETEND_OUTGOING_VARARGS_NAMED sh_pretend_outgoing_varargs_named
#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE sh_pass_by_reference
#undef TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES sh_callee_copies
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES sh_arg_partial_bytes
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG sh_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE sh_function_arg_advance

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV sh_atomic_assign_expand_fenv

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST sh_build_builtin_va_list
#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START sh_va_start
#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR sh_gimplify_va_arg_expr

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P sh_vector_mode_supported_p

#undef TARGET_CHECK_PCH_TARGET_FLAGS
#define TARGET_CHECK_PCH_TARGET_FLAGS sh_check_pch_target_flags

#undef TARGET_DWARF_CALLING_CONVENTION
#define TARGET_DWARF_CALLING_CONVENTION sh_dwarf_calling_convention

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED sh_frame_pointer_required

#undef TARGET_MODE_EMIT
#define TARGET_MODE_EMIT sh_emit_mode_set

#undef TARGET_MODE_NEEDED
#define TARGET_MODE_NEEDED sh_mode_needed

#undef TARGET_MODE_AFTER
#define TARGET_MODE_AFTER sh_mode_after

#undef TARGET_MODE_ENTRY
#define TARGET_MODE_ENTRY sh_mode_entry

#undef TARGET_MODE_EXIT
#define TARGET_MODE_EXIT sh_mode_exit

#undef TARGET_MODE_PRIORITY
#define TARGET_MODE_PRIORITY sh_mode_priority

/* Return regmode weight for insn.  */
#define INSN_REGMODE_WEIGHT(INSN, MODE)\
  regmode_weight[((MODE) == SImode) ? 0 : 1][INSN_UID (INSN)]

/* Return current register pressure for regmode.  */
#define CURR_REGMODE_PRESSURE(MODE)\
  curr_regmode_pressure[((MODE) == SImode) ? 0 : 1]

#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO	sh_encode_section_info

#undef TARGET_LRA_P
#define TARGET_LRA_P sh_lra_p

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD sh_secondary_reload

#undef  TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS sh_preferred_reload_class

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE sh_conditional_register_usage

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	sh_legitimate_address_p

#undef TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P
#define TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P sh_cannot_substitute_mem_equiv_p

#undef TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT
#define TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT \
  sh_legitimize_address_displacement

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT		sh_trampoline_init
#undef TARGET_TRAMPOLINE_ADJUST_ADDRESS
#define TARGET_TRAMPOLINE_ADJUST_ADDRESS sh_trampoline_adjust_address

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P	sh_legitimate_constant_p

#undef TARGET_CANONICALIZE_COMPARISON
#define TARGET_CANONICALIZE_COMPARISON	sh_canonicalize_comparison

#undef TARGET_LEGITIMATE_COMBINED_INSN
#define TARGET_LEGITIMATE_COMBINED_INSN sh_legitimate_combined_insn

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS sh_fixed_condition_code_regs

#undef TARGET_USE_BY_PIECES_INFRASTRUCTURE_P
#define TARGET_USE_BY_PIECES_INFRASTRUCTURE_P \
  sh_use_by_pieces_infrastructure_p

/* Machine-specific symbol_ref flags.  */
#define SYMBOL_FLAG_FUNCVEC_FUNCTION	(SYMBOL_FLAG_MACH_DEP << 0)

/* The tas.b instruction sets the 7th bit in the byte, i.e. 0x80.  This value
   is used by optabs.c atomic op expansion code as well as in sync.md.  */
#undef TARGET_ATOMIC_TEST_AND_SET_TRUEVAL
#define TARGET_ATOMIC_TEST_AND_SET_TRUEVAL 0x80

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM sh_cannot_force_const_mem_p

struct gcc_target targetm = TARGET_INITIALIZER;


/* Information on the currently selected atomic model.
   This is initialized in sh_option_override.  */
static sh_atomic_model selected_atomic_model_;

const sh_atomic_model&
selected_atomic_model (void)
{
  return selected_atomic_model_;
}

static sh_atomic_model
parse_validate_atomic_model_option (const char* str)
{
  const char* model_names[sh_atomic_model::num_models];
  model_names[sh_atomic_model::none] = "none";
  model_names[sh_atomic_model::soft_gusa] = "soft-gusa";
  model_names[sh_atomic_model::hard_llcs] = "hard-llcs";
  model_names[sh_atomic_model::soft_tcb] = "soft-tcb";
  model_names[sh_atomic_model::soft_imask] = "soft-imask";

  const char* model_cdef_names[sh_atomic_model::num_models];
  model_cdef_names[sh_atomic_model::none] = "NONE";
  model_cdef_names[sh_atomic_model::soft_gusa] = "SOFT_GUSA";
  model_cdef_names[sh_atomic_model::hard_llcs] = "HARD_LLCS";
  model_cdef_names[sh_atomic_model::soft_tcb] = "SOFT_TCB";
  model_cdef_names[sh_atomic_model::soft_imask] = "SOFT_IMASK";

  sh_atomic_model ret;
  ret.type = sh_atomic_model::none;
  ret.name = model_names[sh_atomic_model::none];
  ret.cdef_name = model_cdef_names[sh_atomic_model::none];
  ret.strict = false;
  ret.tcb_gbr_offset = -1;

  /* Handle empty string as 'none'.  */
  if (str == NULL || *str == '\0')
    return ret;

#define err_ret(...) do { error (__VA_ARGS__); return ret; } while (0)

  std::vector<std::string> tokens;
  for (std::stringstream ss (str); ss.good (); )
  {
    tokens.push_back (std::string ());
    std::getline (ss, tokens.back (), ',');
  }

  if (tokens.empty ())
    err_ret ("invalid atomic model option");

  /* The first token must be the atomic model name.  */
  {
    for (size_t i = 0; i < sh_atomic_model::num_models; ++i)
      if (tokens.front () == model_names[i])
	{
	  ret.type = (sh_atomic_model::enum_type)i;
	  ret.name = model_names[i];
	  ret.cdef_name = model_cdef_names[i];
	  goto got_mode_name;
	}

    err_ret ("invalid atomic model name \"%s\"", tokens.front ().c_str ());
got_mode_name:;
  }

  /* Go through the remaining tokens.  */
  for (size_t i = 1; i < tokens.size (); ++i)
    {
      if (tokens[i] == "strict")
	ret.strict = true;
      else if (tokens[i].find ("gbr-offset=") == 0)
	{
	  std::string offset_str = tokens[i].substr (strlen ("gbr-offset="));
	  ret.tcb_gbr_offset = integral_argument (offset_str.c_str ());
	  if (offset_str.empty () || ret.tcb_gbr_offset == -1)
	    err_ret ("could not parse gbr-offset value \"%s\" in atomic model "
		     "option", offset_str.c_str ());
	}
      else
	err_ret ("unknown parameter \"%s\" in atomic model option",
		 tokens[i].c_str ());
    }

  /* Check that the selection makes sense.  */
  if (ret.type == sh_atomic_model::soft_gusa && !TARGET_SH3)
    err_ret ("atomic model %s is only available on SH3 and SH4 targets",
	     ret.name);

  if (ret.type == sh_atomic_model::hard_llcs && !TARGET_SH4A)
    err_ret ("atomic model %s is only available on SH4A targets", ret.name);

  if (ret.type == sh_atomic_model::soft_tcb && ret.tcb_gbr_offset == -1)
    err_ret ("atomic model %s requires gbr-offset parameter", ret.name);

  if (ret.type == sh_atomic_model::soft_tcb
      && (ret.tcb_gbr_offset < 0 || ret.tcb_gbr_offset > 1020
          || (ret.tcb_gbr_offset & 3) != 0))
    err_ret ("invalid gbr-offset value \"%d\" for atomic model %s; it must be "
	     "a multiple of 4 in the range 0-1020", ret.tcb_gbr_offset,
	     ret.name);

  if (ret.type == sh_atomic_model::soft_imask && TARGET_USERMODE)
    err_ret ("cannot use atomic model %s in user mode", ret.name);

  return ret;

#undef err_ret
}

/* Register SH specific RTL passes.  */
extern opt_pass* make_pass_sh_treg_combine (gcc::context* ctx, bool split_insns,
					    const char* name);
extern opt_pass* make_pass_sh_optimize_sett_clrt (gcc::context* ctx,
						  const char* name);
static void
register_sh_passes (void)
{
/* Running the sh_treg_combine pass after ce1 generates better code when
   comparisons are combined and reg-reg moves are introduced, because
   reg-reg moves will be eliminated afterwards.  However, there are quite
   some cases where combine will be unable to fold comparison related insns,
   thus for now don't do it.
  register_pass (make_pass_sh_treg_combine (g, false, "sh_treg_combine1"),
		 PASS_POS_INSERT_AFTER, "ce1", 1);
*/

  /* Run sh_treg_combine pass after combine but before register allocation.  */
  register_pass (make_pass_sh_treg_combine (g, true, "sh_treg_combine2"),
		 PASS_POS_INSERT_AFTER, "split1", 1);

  /* Run sh_treg_combine pass after register allocation and basic block
     reordering as this sometimes creates new opportunities.  */
  register_pass (make_pass_sh_treg_combine (g, true, "sh_treg_combine3"),
		 PASS_POS_INSERT_AFTER, "split4", 1);

  /* Optimize sett and clrt insns, by e.g. removing them if the T bit value
     is known after a conditional branch.
     This must be done after basic blocks and branch conditions have
     stabilized and won't be changed by further passes.  */
  register_pass (make_pass_sh_optimize_sett_clrt (g, "sh_optimize_sett_clrt"),
		 PASS_POS_INSERT_BEFORE, "sched2", 1);
}

/* Implement TARGET_OPTION_OVERRIDE macro.  Validate and override 
   various options, and do some machine dependent initialization.  */
static void
sh_option_override (void)
{
  int regno;

  SUBTARGET_OVERRIDE_OPTIONS;

  sh_cpu = PROCESSOR_SH1;
  assembler_dialect = 0;
  if (TARGET_SH2)
    sh_cpu = PROCESSOR_SH2;
  if (TARGET_SH2E)
    sh_cpu = PROCESSOR_SH2E;
  if (TARGET_SH2A)
    sh_cpu = PROCESSOR_SH2A;
  if (TARGET_SH3)
    sh_cpu = PROCESSOR_SH3;
  if (TARGET_SH3E)
    sh_cpu = PROCESSOR_SH3E;
  if (TARGET_SH4)
    {
      assembler_dialect = 1;
      sh_cpu = PROCESSOR_SH4;
    }
  if (TARGET_SH4A)
    {
      assembler_dialect = 1;
      sh_cpu = PROCESSOR_SH4A;
    }

  /* User/priviledged mode is supported only on SH3* and SH4*.
     Disable it for everything else.  */
  if (!TARGET_SH3 && TARGET_USERMODE)
    TARGET_USERMODE = false;

  if (! strcmp (sh_div_str, "call-div1"))
    sh_div_strategy = SH_DIV_CALL_DIV1;
  else if (! strcmp (sh_div_str, "call-fp") && TARGET_FPU_ANY)
    sh_div_strategy = SH_DIV_CALL_FP;
  else if (! strcmp (sh_div_str, "call-table") && TARGET_DYNSHIFT)
    sh_div_strategy = SH_DIV_CALL_TABLE;
  else
    {
      /* Pick one that makes most sense for the target in general.
	 It is not much good to use different functions depending on -Os,
	 since then we'll end up with two different functions when some of
	 the code is compiled for size, and some for speed.  */

      /* SH4 tends to emphasize speed.  */
      if (TARGET_HARD_SH4)
	sh_div_strategy = SH_DIV_CALL_TABLE;
      /* These have their own way of doing things.  */
      else if (TARGET_SH2A)
	sh_div_strategy = SH_DIV_INTRINSIC;
      /* SH1 .. SH3 cores often go into small-footprint systems, so
	 default to the smallest implementation available.  */
      else
	sh_div_strategy = SH_DIV_CALL_DIV1;
    }

  if (sh_divsi3_libfunc[0])
    ; /* User supplied - leave it alone.  */
  else if (TARGET_DIVIDE_CALL_FP)
    sh_divsi3_libfunc = "__sdivsi3_i4";
  else if (TARGET_DIVIDE_CALL_TABLE)
    sh_divsi3_libfunc = "__sdivsi3_i4i";
  else
    sh_divsi3_libfunc = "__sdivsi3";

  if (sh_branch_cost == -1)
    {
      /*  The SH1 does not have delay slots, hence we get a pipeline stall
	  at every branch.  The SH4 is superscalar, so the single delay slot
	  is not sufficient to keep both pipelines filled.
	  In any case, set the default branch cost to '2', as it results in
	  slightly overall smaller code and also enables some if conversions
	  that are required for matching special T bit related insns.  */
      sh_branch_cost = 2;
    }

  /* Set -mzdcbranch for SH4 / SH4A if not otherwise specified by the user.  */
  if (! global_options_set.x_TARGET_ZDCBRANCH && TARGET_HARD_SH4)
    TARGET_ZDCBRANCH = 1;

  /* FDPIC code is a special form of PIC, and the vast majority of code
     generation constraints that apply to PIC also apply to FDPIC, so we
     set flag_pic to avoid the need to check TARGET_FDPIC everywhere
     flag_pic is checked. */
  if (TARGET_FDPIC && !flag_pic)
    flag_pic = 2;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (! VALID_REGISTER_P (regno))
      sh_register_names[regno][0] = '\0';

  for (regno = 0; regno < ADDREGNAMES_SIZE; regno++)
    if (! VALID_REGISTER_P (ADDREGNAMES_REGNO (regno)))
      sh_additional_register_names[regno][0] = '\0';

  if (flag_pic && ! TARGET_PREFERGOT)
    flag_no_function_cse = 1;

  if (targetm.small_register_classes_for_mode_p (VOIDmode))
    {
      /* Never run scheduling before reload, since that can
	 break global alloc, and generates slower code anyway due
	 to the pressure on R0.  */
      /* Enable sched1 for SH4 if the user explicitly requests.
	 When sched1 is enabled, the ready queue will be reordered by
	 the target hooks if pressure is high.  We can not do this for
	 PIC, SH3 and lower as they give spill failures for R0.  */
      if (!TARGET_HARD_SH4 || flag_pic)
	flag_schedule_insns = 0;
      /* ??? Current exception handling places basic block boundaries
	 after call_insns.  It causes the high pressure on R0 and gives
	 spill failures for R0 in reload.  See PR 22553 and the thread
	 on gcc-patches
	 <http://gcc.gnu.org/ml/gcc-patches/2005-10/msg00816.html>.  */
      else if (flag_exceptions)
	{
	  if (flag_schedule_insns && global_options_set.x_flag_schedule_insns)
	    warning (0, "ignoring -fschedule-insns because of exception "
			"handling bug");
	  flag_schedule_insns = 0;
	}
      else if (flag_schedule_insns
	       && !global_options_set.x_flag_schedule_insns)
	flag_schedule_insns = 0;
    }

  /* Unwind info is not correct around the CFG unless either a frame
     pointer is present or M_A_O_A is set.  Fixing this requires rewriting
     unwind info generation to be aware of the CFG and propagating states
     around edges.  */
  if ((flag_unwind_tables || flag_asynchronous_unwind_tables
       || flag_exceptions || flag_non_call_exceptions)
      && flag_omit_frame_pointer && !TARGET_ACCUMULATE_OUTGOING_ARGS)
    {
      warning (0, "unwind tables currently require either a frame pointer "
	       "or -maccumulate-outgoing-args for correctness");
      TARGET_ACCUMULATE_OUTGOING_ARGS = 1;
    }

  if (flag_unsafe_math_optimizations)
    {
      /* Enable fsca insn for SH4A if not otherwise specified by the user.  */
      if (global_options_set.x_TARGET_FSCA == 0 && TARGET_SH4A_FP)
	TARGET_FSCA = 1;

      /* Enable fsrra insn for SH4A if not otherwise specified by the user.  */
      if (global_options_set.x_TARGET_FSRRA == 0 && TARGET_SH4A_FP)
	TARGET_FSRRA = 1;
    }

  /*  Allow fsrra insn only if -funsafe-math-optimizations and
      -ffinite-math-only is enabled.  */
  TARGET_FSRRA = TARGET_FSRRA
		 && flag_unsafe_math_optimizations
		 && flag_finite_math_only;

  /* If the -mieee option was not explicitly set by the user, turn it on
     unless -ffinite-math-only was specified.  See also PR 33135.  */
  if (! global_options_set.x_TARGET_IEEE)
    TARGET_IEEE = ! flag_finite_math_only;

  if (sh_fixed_range_str)
    sh_fix_range (sh_fixed_range_str);

  /* This target defaults to strict volatile bitfields.  */
  if (flag_strict_volatile_bitfields < 0 && abi_version_at_least(2))
    flag_strict_volatile_bitfields = 1;

  sh_override_options_after_change ();

  /* Parse atomic model option and make sure it is valid for the current
     target CPU.  */
  selected_atomic_model_
    = parse_validate_atomic_model_option (sh_atomic_model_str);

  register_sh_passes ();
}

/* Implement targetm.override_options_after_change.  */

static void
sh_override_options_after_change (void)
{
  /*  Adjust loop, jump and function alignment values (in bytes), if those
      were not specified by the user using -falign-loops, -falign-jumps
      and -falign-functions options.
      32 bit alignment is better for speed, because instructions can be
      fetched as a pair from a longword boundary.  For size use 16 bit
      alignment to get more compact code.
      Aligning all jumps increases the code size, even if it might
      result in slightly faster code.  Thus, it is set to the smallest 
      alignment possible if not specified by the user.  */
  if (align_loops == 0)
    align_loops = optimize_size ? 2 : 4;

  if (align_jumps == 0)
    align_jumps = 2;
  else if (align_jumps < 2)
    align_jumps = 2;

  if (align_functions == 0)
    align_functions = optimize_size ? 2 : 4;

  /* The linker relaxation code breaks when a function contains
     alignments that are larger than that at the start of a
     compilation unit.  */
  if (TARGET_RELAX)
    {
      int min_align = align_loops > align_jumps ? align_loops : align_jumps;

      /* Also take possible .long constants / mova tables into account.	*/
      if (min_align < 4)
	min_align = 4;
      if (align_functions < min_align)
	align_functions = min_align;
    }
}

/* Print the operand address in x to the stream.  */
static void
sh_print_operand_address (FILE *stream, machine_mode /*mode*/, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
      fprintf (stream, "@%s", reg_names[true_regnum (x)]);
      break;

    case PLUS:
      {
	rtx base = XEXP (x, 0);
	rtx index = XEXP (x, 1);

	switch (GET_CODE (index))
	  {
	  case CONST_INT:
	    fprintf (stream, "@(%d,%s)", (int) INTVAL (index),
		     reg_names[true_regnum (base)]);
	    break;

	  case REG:
	  case SUBREG:
	    {
	      int base_num = true_regnum (base);
	      int index_num = true_regnum (index);

	      /* If base or index is R0, make sure that it comes first.
		 Usually one of them will be R0, but the order might be wrong.
		 If neither base nor index are R0 it's an error and we just
		 pass it on to the assembler.  This avoids silent wrong code
		 bugs.  */
	      if (base_num == 0 && index_num != 0)
		std::swap (base_num, index_num);

	      fprintf (stream, "@(%s,%s)", reg_names[index_num],
					   reg_names[base_num]);
	      break;
	    }

	  default:
	    gcc_unreachable ();
	  }
      }
      break;

    case PRE_DEC:
      fprintf (stream, "@-%s", reg_names[true_regnum (XEXP (x, 0))]);
      break;

    case POST_INC:
      fprintf (stream, "@%s+", reg_names[true_regnum (XEXP (x, 0))]);
      break;

    default:
      x = mark_constant_pool_use (x);
      output_addr_const (stream, x);
      break;
    }
}

/* Print operand x (an rtx) in assembler syntax to file stream
   according to modifier code.

   '.'  print a .s if insn needs delay slot
   ','  print LOCAL_LABEL_PREFIX
   '@'  print trap, rte or rts depending upon pragma interruptness
   '#'  output a nop if there is nothing to put in the delay slot
   '''  print likelihood suffix (/u for unlikely).
   '>'  print branch target if -fverbose-asm
   'O'  print a constant without the #
   'R'  print the LSW of a dp value - changes if in little endian
   'S'  print the MSW of a dp value - changes if in little endian
   'T'  print the next word of a dp value - same as 'R' in big endian mode.
   'M'  print .b / .w / .l / .s / .d suffix if operand is a MEM.
   'N'  print 'r63' if the operand is (const_int 0).
   'd'  print a V2SF reg as dN instead of fpN.
   'm'  print a pair `base,offset' or `base,index', for LD and ST.
   'U'  Likewise for {LD,ST}{HI,LO}.
   'V'  print the position of a single bit set.
   'W'  print the position of a single bit cleared.
   't'  print a memory address which is a register.
   'u'  prints the lowest 16 bits of CONST_INT, as an unsigned value.
   'o'  output an operator.  */
static void
sh_print_operand (FILE *stream, rtx x, int code)
{
  int regno;
  machine_mode mode;

  switch (code)
    {
      tree trapa_attr;

    case '.':
      if (final_sequence
	  && ! INSN_ANNULLED_BRANCH_P (final_sequence->insn (0))
	  && get_attr_length (final_sequence->insn (1)))
	fprintf (stream, ASSEMBLER_DIALECT ? "/s" : ".s");
      break;
    case ',':
      fprintf (stream, "%s", LOCAL_LABEL_PREFIX);
      break;
    case '@':
      trapa_attr = lookup_attribute ("trap_exit",
				      DECL_ATTRIBUTES (current_function_decl));
      if (trapa_attr)
	fprintf (stream, "trapa	#%ld",
		 (long) TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (trapa_attr))));
      else if (sh_cfun_interrupt_handler_p ())
	{
	  if (sh_cfun_resbank_handler_p ())
	    fprintf (stream, "resbank\n");
	  fprintf (stream, "rte");
	}
      else
	fprintf (stream, "rts");
      break;
    case '#':
      /* Output a nop if there's nothing in the delay slot.  */
      if (dbr_sequence_length () == 0)
	fprintf (stream, "\n\tnop");
      break;
    case '\'':
      {
	rtx note = find_reg_note (current_output_insn, REG_BR_PROB, 0);

	if (note && XINT (note, 0) * 2 < REG_BR_PROB_BASE)
	  fputs ("/u", stream);
	break;
      }
    case '>':
      if (flag_verbose_asm && JUMP_LABEL (current_output_insn))
	{
	  fputs ("\t! target: ", stream);
	  output_addr_const (stream, JUMP_LABEL (current_output_insn));
	}
      break;
    case 'O':
      x = mark_constant_pool_use (x);
      output_addr_const (stream, x);
      break;
    /* N.B.: %R / %S / %T adjust memory addresses by four.
       While they can be used to access 64 bit parts of a larger value
       held in general purpose registers, that won't work with memory -
       neither for fp registers, since the frxx names are used.  */
    case 'R':
      if (REG_P (x) || GET_CODE (x) == SUBREG)
	{
	  regno = true_regnum (x);
	  regno += FP_REGISTER_P (regno) ? 1 : SH_REG_LSW_OFFSET;
	  fputs (reg_names[regno], (stream));
	}
      else if (MEM_P (x))
	{
	  x = adjust_address (x, SImode, 4 * SH_REG_LSW_OFFSET);
	  sh_print_operand_address (stream, GET_MODE (x), XEXP (x, 0));
	}
      else
	{
	  rtx sub = NULL_RTX;

	  mode = GET_MODE (x);
	  if (mode == VOIDmode)
	    mode = DImode;
	  if (GET_MODE_SIZE (mode) >= 8)
	    sub = simplify_subreg (SImode, x, mode, 4 * SH_REG_LSW_OFFSET);
	  if (sub)
	    sh_print_operand (stream, sub, 0);
	  else
	    output_operand_lossage ("invalid operand to %%R");
	}
      break;
    case 'S':
      if (REG_P (x) || GET_CODE (x) == SUBREG)
	{
	  regno = true_regnum (x);
	  regno += FP_REGISTER_P (regno) ? 0 : SH_REG_MSW_OFFSET;
	  fputs (reg_names[regno], (stream));
	}
      else if (MEM_P (x))
	{
	  x = adjust_address (x, SImode, 4 * SH_REG_MSW_OFFSET);
	  sh_print_operand_address (stream, GET_MODE (x), XEXP (x, 0));
	}
      else
	{
	  rtx sub = NULL_RTX;

	  mode = GET_MODE (x);
	  if (mode == VOIDmode)
	    mode = DImode;
	  if (GET_MODE_SIZE (mode) >= 8)
	    sub = simplify_subreg (SImode, x, mode, 4 * SH_REG_MSW_OFFSET);
	  if (sub)
	    sh_print_operand (stream, sub, 0);
	  else
	    output_operand_lossage ("invalid operand to %%S");
	}
      break;
    case 'T':
      /* Next word of a double.  */
      switch (GET_CODE (x))
	{
	case REG:
	  fputs (reg_names[REGNO (x) + 1], (stream));
	  break;
	case MEM:
	  {
	    machine_mode mode = GET_MODE (x);
	    if (GET_CODE (XEXP (x, 0)) != PRE_DEC
		&& GET_CODE (XEXP (x, 0)) != POST_INC)
	      x = adjust_address (x, SImode, 4);
	    sh_print_operand_address (stream, mode, XEXP (x, 0));
	  }
	  break;
	default:
	  break;
	}
      break;

    case 't':
      gcc_assert (MEM_P (x));
      x = XEXP (x, 0);
      switch (GET_CODE (x))
	{
	case REG:
	case SUBREG:
	  sh_print_operand (stream, x, 0);
	  break;
	default:
	  break;
	}
      break;

    case 'o':
      switch (GET_CODE (x))
	{
	case PLUS:  fputs ("add", stream); break;
	case MINUS: fputs ("sub", stream); break;
	case MULT:  fputs ("mul", stream); break;
	case DIV:   fputs ("div", stream); break;
	case EQ:    fputs ("eq",  stream); break;
	case NE:    fputs ("ne",  stream); break;
	case GT:  case LT:  fputs ("gt",  stream); break;
	case GE:  case LE:  fputs ("ge",  stream); break;
	case GTU: case LTU: fputs ("gtu", stream); break;
	case GEU: case LEU: fputs ("geu", stream); break;
	default:
	  break;
	}
      break;
    case 'M':
      if (MEM_P (x))
	{
	  switch (GET_MODE (x))
	    {
	    case QImode: fputs (".b", stream); break;
	    case HImode: fputs (".w", stream); break;
	    case SImode: fputs (".l", stream); break;
	    case SFmode: fputs (".s", stream); break;
	    case DFmode: fputs (".d", stream); break;
	    default: gcc_unreachable ();
	    }
	}
      break;

    case 'm':
      gcc_assert (MEM_P (x));
      x = XEXP (x, 0);
      /* Fall through.  */
    case 'U':
      switch (GET_CODE (x))
	{
	case REG:
	case SUBREG:
	  sh_print_operand (stream, x, 0);
	  fputs (", 0", stream);
	  break;

	case PLUS:
	  sh_print_operand (stream, XEXP (x, 0), 0);
	  fputs (", ", stream);
	  sh_print_operand (stream, XEXP (x, 1), 0);
	  break;

	default:
	  gcc_unreachable ();
	}
      break;

    case 'V':
      {
	int num = exact_log2 (INTVAL (x));
	gcc_assert (num >= 0);
	fprintf (stream, "#%d", num);
      }
      break;

    case 'W':
      {
	int num = exact_log2 (~INTVAL (x));
	gcc_assert (num >= 0);
	fprintf (stream, "#%d", num);
      }
      break;

    case 'd':
      gcc_assert (REG_P (x) && GET_MODE (x) == V2SFmode);

      fprintf ((stream), "d%s", reg_names[REGNO (x)] + 1);
      break;

    case 'N':
      if (x == CONST0_RTX (GET_MODE (x)))
	{
	  fprintf ((stream), "r63");
	  break;
	}
      goto default_output;
    case 'u':
      if (CONST_INT_P (x))
	{
	  fprintf ((stream), "%u", (unsigned) INTVAL (x) & (0x10000 - 1));
	  break;
	}
      /* Fall through.  */

    default_output:
    default:
      regno = 0;
      mode = GET_MODE (x);

      switch (GET_CODE (x))
	{
	case TRUNCATE:
	  {
	    rtx inner = XEXP (x, 0);
	    int offset = 0;
	    machine_mode inner_mode;

	    /* We might see SUBREGs with vector mode registers inside.  */
	    if (GET_CODE (inner) == SUBREG
		&& (GET_MODE_SIZE (GET_MODE (inner))
		    == GET_MODE_SIZE (GET_MODE (SUBREG_REG (inner))))
		&& subreg_lowpart_p (inner))
	      inner = SUBREG_REG (inner);
	    if (CONST_INT_P (inner))
	      {
		x = GEN_INT (trunc_int_for_mode (INTVAL (inner), GET_MODE (x)));
		goto default_output;
	      }
	    inner_mode = GET_MODE (inner);
	    if (GET_CODE (inner) == SUBREG
		&& (GET_MODE_SIZE (GET_MODE (inner))
		    < GET_MODE_SIZE (GET_MODE (SUBREG_REG (inner))))
		&& REG_P (SUBREG_REG (inner)))
	      {
		offset = subreg_regno_offset (REGNO (SUBREG_REG (inner)),
					      GET_MODE (SUBREG_REG (inner)),
					      SUBREG_BYTE (inner),
					      GET_MODE (inner));
		inner = SUBREG_REG (inner);
	      }
	    if (!REG_P (inner) || GET_MODE_SIZE (inner_mode) > 8)
	      abort ();
	    /* Floating point register pairs are always big endian;
	       general purpose registers are 64 bit wide.  */
	    regno = REGNO (inner);
	    regno = (HARD_REGNO_NREGS (regno, inner_mode)
		     - HARD_REGNO_NREGS (regno, mode))
		     + offset;
	    x = inner;
	    goto reg;
	  }
	case SIGN_EXTEND:
	  x = XEXP (x, 0);
	  goto reg;
	case SUBREG:
	  gcc_assert (SUBREG_BYTE (x) == 0
		      && REG_P (SUBREG_REG (x)));

	  x = SUBREG_REG (x);
	  /* Fall through.  */

	reg:
	case REG:
	  regno += REGNO (x);
	  if (FP_REGISTER_P (regno)
	      && mode == V16SFmode)
	    fprintf ((stream), "mtrx%s", reg_names[regno] + 2);
	  else if (FP_REGISTER_P (REGNO (x))
		   && mode == V4SFmode)
	    fprintf ((stream), "fv%s", reg_names[regno] + 2);
	  else if (REG_P (x)
		   && mode == V2SFmode)
	    fprintf ((stream), "fp%s", reg_names[regno] + 2);
	  else if (FP_REGISTER_P (REGNO (x))
		   && GET_MODE_SIZE (mode) > 4)
	    fprintf ((stream), "d%s", reg_names[regno] + 1);
	  else
	    fputs (reg_names[regno], (stream));
	  break;

	case MEM:
	  output_address (GET_MODE (x), XEXP (x, 0));
	  break;

	default:
	  fputc ('#', stream);
	  output_addr_const (stream, x);
	  break;
	}
      break;
    }
}

static bool
sh_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '.' || code == '#' || code == '@' || code == ','
	  || code == '$' || code == '\'' || code == '>');
}

/* Implement TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */
static bool
sh_asm_output_addr_const_extra (FILE *file, rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    {
      switch (XINT (x, 1))
	{
	case UNSPEC_PIC:
	  /* GLOBAL_OFFSET_TABLE or local symbols, no suffix.  */
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  break;
	case UNSPEC_GOT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOT", file);
	  break;
	case UNSPEC_GOTOFF:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOTOFF", file);
	  break;
	case UNSPEC_PLT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@PLT", file);
	  break;
	case UNSPEC_GOTPLT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOTPLT", file);
	  break;
	case UNSPEC_PCREL:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@PCREL", file);
	  break;
	case UNSPEC_DTPOFF:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@DTPOFF", file);
	  break;
	case UNSPEC_GOTTPOFF:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOTTPOFF", file);
	  break;
	case UNSPEC_TPOFF:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@TPOFF", file);
	  break;
	case UNSPEC_CALLER:
	  {
	    char name[32];
	    /* LPCS stands for Label for PIC Call Site.  */
	    targetm.asm_out.generate_internal_label (name, "LPCS",
						     INTVAL (XVECEXP (x, 0, 0)));
	    assemble_name (file, name);
	  }
	  break;
	case UNSPEC_SYMOFF:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputc ('-', file);
	  if (GET_CODE (XVECEXP (x, 0, 1)) == CONST)
	    {
	      fputc ('(', file);
	      output_addr_const (file, XVECEXP (x, 0, 1));
	      fputc (')', file);
	    }
	  else
	    output_addr_const (file, XVECEXP (x, 0, 1));
	  break;
	case UNSPEC_PCREL_SYMOFF:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("-(", file);
	  output_addr_const (file, XVECEXP (x, 0, 1));
	  fputs ("-.)", file);
	  break;
	case UNSPEC_GOTFUNCDESC:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOTFUNCDESC", file);
	  break;
	case UNSPEC_GOTOFFFUNCDESC:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOTOFFFUNCDESC", file);
	  break;
	default:
	  return false;
	}
      return true;
    }
  else
    return false;
}

/* Encode symbol attributes of a SYMBOL_REF into its
   SYMBOL_REF_FLAGS.  */
static void
sh_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && sh2a_function_vector_p (decl) && TARGET_SH2A)
    SYMBOL_REF_FLAGS (XEXP (rtl, 0)) |= SYMBOL_FLAG_FUNCVEC_FUNCTION;
}

/* Prepare operands for a move define_expand; specifically, one of the
   operands must be in a register.  */
void
prepare_move_operands (rtx operands[], machine_mode mode)
{
  if ((mode == SImode || mode == DImode)
      && flag_pic
      && ! ((mode == Pmode || mode == ptr_mode)
	    && tls_symbolic_operand (operands[1], Pmode) != TLS_MODEL_NONE))
    {
      rtx temp;
      if (SYMBOLIC_CONST_P (operands[1]))
	{
	  if (MEM_P (operands[0]))
	    operands[1] = force_reg (Pmode, operands[1]);
	  else
	    {
	      temp = (!can_create_pseudo_p ()
		      ? operands[0]
		      : gen_reg_rtx (Pmode));
	      operands[1] = legitimize_pic_address (operands[1], mode, temp);
	    }
	}
      else if (GET_CODE (operands[1]) == CONST
	       && GET_CODE (XEXP (operands[1], 0)) == PLUS
	       && SYMBOLIC_CONST_P (XEXP (XEXP (operands[1], 0), 0)))
	{
	  temp = !can_create_pseudo_p () ? operands[0] : gen_reg_rtx (Pmode);
	  temp = legitimize_pic_address (XEXP (XEXP (operands[1], 0), 0),
					 mode, temp);
	  operands[1] = expand_binop (mode, add_optab, temp,
				      XEXP (XEXP (operands[1], 0), 1),
				      (!can_create_pseudo_p ()
				       ? temp
				       : gen_reg_rtx (Pmode)),
				      0, OPTAB_LIB_WIDEN);
	}
    }

  if (! reload_in_progress && ! reload_completed)
    {
      /* Copy the source to a register if both operands aren't registers.  */
      if (! register_operand (operands[0], mode)
	  && ! register_operand (operands[1], mode))
	operands[1] = copy_to_mode_reg (mode, operands[1]);

      if (MEM_P (operands[0]) && ! memory_operand (operands[0], mode))
	{
	  /* This is like change_address_1 (operands[0], mode, 0, 1) ,
	     except that we can't use that function because it is static.  */
	  rtx new_rtx = change_address (operands[0], mode, 0);
	  MEM_COPY_ATTRIBUTES (new_rtx, operands[0]);
	  operands[0] = new_rtx;
	}

      /* This case can happen while generating code to move the result
	 of a library call to the target.  Reject `st r0,@(rX,rY)' because
	 reload will fail to find a spill register for rX, since r0 is already
	 being used for the source.  */
      else if (refers_to_regno_p (R0_REG, operands[1])
	       && MEM_P (operands[0])
	       && GET_CODE (XEXP (operands[0], 0)) == PLUS
	       && REG_P (XEXP (XEXP (operands[0], 0), 1)))
	operands[1] = copy_to_mode_reg (mode, operands[1]);

      /* When the displacement addressing is used, RA will assign r0 to
	 the pseudo register operand for the QI/HImode load/store.
	 This tends to make a long live range for R0 and might cause
	 anomalous register spills in some case with LRA.  See PR
	 target/55212.
	 We split possible load/store to two move insns via r0 so as to
	 shorten R0 live range.  It will make some codes worse but will
	 win on average for LRA.
	 Also when base+index addressing is used and the index term is
	 a subreg, LRA assumes that more hard registers can be available
	 in some situation.  It isn't the case for SH in the problematic
	 case.  We can pre-allocate R0 for that index term to avoid
	 the issue.  See PR target/66591.  */
      else if (sh_lra_p ()
	       && ! TARGET_SH2A
	       && ((REG_P (operands[0]) && MEM_P (operands[1]))
		   || (REG_P (operands[1]) && MEM_P (operands[0]))))
	{
	  bool load_p = REG_P (operands[0]);
	  rtx reg = operands[load_p ? 0 : 1];
	  rtx adr = XEXP (operands[load_p ? 1 : 0], 0);

	  if ((mode == QImode || mode == HImode)
	      && REGNO (reg) >= FIRST_PSEUDO_REGISTER
	      && GET_CODE (adr) == PLUS
	      && REG_P (XEXP (adr, 0))
	      && (REGNO (XEXP (adr, 0)) >= FIRST_PSEUDO_REGISTER)
	      && CONST_INT_P (XEXP (adr, 1))
	      && INTVAL (XEXP (adr, 1)) != 0
	      && sh_legitimate_index_p (mode, XEXP (adr, 1), false, true))
	    {
	      rtx r0_rtx = gen_rtx_REG (mode, R0_REG);
	      emit_move_insn (r0_rtx, operands[1]);
	      operands[1] = r0_rtx;
	    }
	  if (REGNO (reg) >= FIRST_PSEUDO_REGISTER
	      && GET_CODE (adr) == PLUS
	      && REG_P (XEXP (adr, 0))
	      && (REGNO (XEXP (adr, 0)) >= FIRST_PSEUDO_REGISTER)
	      && SUBREG_P (XEXP (adr, 1))
	      && REG_P (SUBREG_REG (XEXP (adr, 1))))
	    {
	      rtx r0_rtx = gen_rtx_REG (GET_MODE (XEXP (adr, 1)), R0_REG);
	      emit_move_insn (r0_rtx, XEXP (adr, 1));
	      XEXP (adr, 1) = r0_rtx;
	    }
	}
    }

  if (mode == Pmode || mode == ptr_mode)
    {
      rtx op0 = operands[0];
      rtx op1 = operands[1];
      rtx opc;
      if (GET_CODE (op1) == CONST
	  && GET_CODE (XEXP (op1, 0)) == PLUS
	  && (tls_symbolic_operand (XEXP (XEXP (op1, 0), 0), Pmode)
	      != TLS_MODEL_NONE))
	{
	  opc = XEXP (XEXP (op1, 0), 1);
	  op1 = XEXP (XEXP (op1, 0), 0);
	}
      else
	opc = NULL_RTX;

      enum tls_model tls_kind;

      if (! reload_in_progress && ! reload_completed
	  && (tls_kind = tls_symbolic_operand (op1, Pmode)) != TLS_MODEL_NONE)
	{
	  rtx tga_op1, tga_ret, tmp, tmp2;

	  if (! flag_pic
	      && (tls_kind == TLS_MODEL_GLOBAL_DYNAMIC
		  || tls_kind == TLS_MODEL_LOCAL_DYNAMIC
		  || tls_kind == TLS_MODEL_INITIAL_EXEC))
	    {
	      static int got_labelno;
	      /* Don't schedule insns for getting GOT address when
		 the first scheduling is enabled, to avoid spill
		 failures for R0.  */
	      if (flag_schedule_insns)
		emit_insn (gen_blockage ());
	      emit_insn (gen_GOTaddr2picreg (GEN_INT (++got_labelno)));
	      emit_use (gen_rtx_REG (SImode, PIC_REG));
	      if (flag_schedule_insns)
		emit_insn (gen_blockage ());
	    }

	  switch (tls_kind)
	    {
	    case TLS_MODEL_GLOBAL_DYNAMIC:
	      tga_ret = gen_rtx_REG (Pmode, R0_REG);
	      if (TARGET_FDPIC)
		emit_move_insn (gen_rtx_REG (Pmode, PIC_REG),
				sh_get_fdpic_reg_initial_val ());
	      emit_call_insn (gen_tls_global_dynamic (tga_ret, op1));
	      tmp = gen_reg_rtx (Pmode);
	      emit_move_insn (tmp, tga_ret);
	      op1 = tmp;
	      break;

	    case TLS_MODEL_LOCAL_DYNAMIC:
	      tga_ret = gen_rtx_REG (Pmode, R0_REG);
	      if (TARGET_FDPIC)
		emit_move_insn (gen_rtx_REG (Pmode, PIC_REG),
				sh_get_fdpic_reg_initial_val ());
	      emit_call_insn (gen_tls_local_dynamic (tga_ret, op1));

	      tmp = gen_reg_rtx (Pmode);
	      emit_move_insn (tmp, tga_ret);

	      if (register_operand (op0, Pmode))
		tmp2 = op0;
	      else
		tmp2 = gen_reg_rtx (Pmode);

	      emit_insn (gen_symDTPOFF2reg (tmp2, op1, tmp));
	      op1 = tmp2;
	      break;

	    case TLS_MODEL_INITIAL_EXEC:
	      tga_op1 = !can_create_pseudo_p () ? op0 : gen_reg_rtx (Pmode);
	      tmp = gen_sym2GOTTPOFF (op1);
	      if (TARGET_FDPIC)
		emit_move_insn (gen_rtx_REG (Pmode, PIC_REG),
				sh_get_fdpic_reg_initial_val ());
	      emit_insn (gen_tls_initial_exec (tga_op1, tmp));
	      op1 = tga_op1;
	      break;

	    case TLS_MODEL_LOCAL_EXEC:
	      tmp2 = gen_reg_rtx (Pmode);
	      emit_insn (gen_store_gbr (tmp2));
	      tmp = gen_reg_rtx (Pmode);
	      emit_insn (gen_symTPOFF2reg (tmp, op1));

	      if (register_operand (op0, Pmode))
		op1 = op0;
	      else
		op1 = gen_reg_rtx (Pmode);

	      emit_insn (gen_addsi3 (op1, tmp, tmp2));
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  if (opc)
	    emit_insn (gen_addsi3 (op1, op1, force_reg (SImode, opc)));
	  operands[1] = op1;
	}
    }

  if (SH_OFFSETS_MUST_BE_WITHIN_SECTIONS_P)
    {
      rtx base, offset;
      split_const (operands[1], &base, &offset);

      if (GET_CODE (base) == SYMBOL_REF
	  && !offset_within_block_p (base, INTVAL (offset)))
	{
	  rtx tmp = can_create_pseudo_p () ? gen_reg_rtx (mode) : operands[0];
	  emit_move_insn (tmp, base);
	  if (!arith_operand (offset, mode))
	    offset = force_reg (mode, offset);
	  emit_insn (gen_add3_insn (operands[0], tmp, offset));
	}
    }
}

/* Implement the canonicalize_comparison target hook for the combine
   pass.  For the target hook this function is invoked via
   sh_canonicalize_comparison.  This function is also re-used to
   canonicalize comparisons in cbranch pattern expanders.  */
static void
sh_canonicalize_comparison (enum rtx_code& cmp, rtx& op0, rtx& op1,
			    machine_mode mode,
			    bool op0_preserve_value)
{
  /* When invoked from within the combine pass the mode is not specified,
     so try to get it from one of the operands.  */
  if (mode == VOIDmode)
    mode = GET_MODE (op0);
  if (mode == VOIDmode)
    mode = GET_MODE (op1);

  // We need to have a mode to do something useful here.
  if (mode == VOIDmode)
    return;

  // Currently, we don't deal with floats here.
  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    return;

  // Make sure that the constant operand is the second operand.
  if (CONST_INT_P (op0) && !CONST_INT_P (op1))
    {
      if (op0_preserve_value)
	return;

      std::swap (op0, op1);
      cmp = swap_condition (cmp);
    }

  if (CONST_INT_P (op1))
    {
      /* Try to adjust the constant operand in such a way that available
	 comparison insns can be utilized better and the constant can be
	 loaded with a 'mov #imm,Rm' insn.  This avoids a load from the
	 constant pool.  */
      const HOST_WIDE_INT val = INTVAL (op1);

      /* x > -1		  --> x >= 0
	 x > 0xFFFFFF7F	  --> x >= 0xFFFFFF80
	 x <= -1	  --> x < 0
	 x <= 0xFFFFFF7F  --> x < 0xFFFFFF80  */
      if ((val == -1 || val == -0x81) && (cmp == GT || cmp == LE))
	{
	  cmp = cmp == GT ? GE : LT;
	  op1 = gen_int_mode (val + 1, mode);
        }

      /* x >= 1     --> x > 0
	 x >= 0x80  --> x > 0x7F
	 x < 1      --> x <= 0
	 x < 0x80   --> x <= 0x7F  */
      else if ((val == 1 || val == 0x80) && (cmp == GE || cmp == LT))
	{
	  cmp = cmp == GE ? GT : LE;
	  op1 = gen_int_mode (val - 1, mode);
	}

      /* unsigned x >= 1  --> x != 0
	 unsigned x < 1   --> x == 0  */
      else if (val == 1 && (cmp == GEU || cmp == LTU))
	{
	  cmp = cmp == GEU ? NE : EQ;
	  op1 = CONST0_RTX (mode);
	}

      /* unsigned x >= 0x80  --> unsigned x > 0x7F
	 unsigned x < 0x80   --> unsigned x < 0x7F  */
      else if (val == 0x80 && (cmp == GEU || cmp == LTU))
	{
	  cmp = cmp == GEU ? GTU : LEU;
	  op1 = gen_int_mode (val - 1, mode);
	}

      /* unsigned x > 0   --> x != 0
	 unsigned x <= 0  --> x == 0  */
      else if (val == 0 && (cmp == GTU || cmp == LEU))
	cmp = cmp == GTU ? NE : EQ;

      /* unsigned x > 0x7FFFFFFF   --> signed x < 0
	 unsigned x <= 0x7FFFFFFF  --> signed x >= 0  */
      else if (mode == SImode && (cmp == GTU || cmp == LEU)
	       && val == 0x7FFFFFFF)
	{
	  cmp = cmp == GTU ? LT : GE;
	  op1 = const0_rtx;
	}

      /* unsigned x >= 0x80000000  --> signed x < 0
	 unsigned x < 0x80000000   --> signed x >= 0  */
      else if (mode == SImode && (cmp == GEU || cmp == LTU)
	       && (unsigned HOST_WIDE_INT)val
		   == ((unsigned HOST_WIDE_INT)0x7FFFFFFF + 1))
	{
	  cmp = cmp == GEU ? LT : GE;
	  op1 = const0_rtx;
	}
    }
}

/* This function implements the canonicalize_comparison target hook.
   This wrapper around the internally used sh_canonicalize_comparison
   function is needed to do the enum rtx_code <-> int conversion.
   Target hooks cannot use enum rtx_code in its definition.  */
static void
sh_canonicalize_comparison (int *code, rtx *op0, rtx *op1,
			    bool op0_preserve_value)
{
  enum rtx_code tmp_code = (enum rtx_code)*code;
  sh_canonicalize_comparison (tmp_code, *op0, *op1,
			      VOIDmode, op0_preserve_value);
  *code = (int)tmp_code;
}

/* This function implements the legitimate_combined_insn target hook,
   which the combine pass uses to early reject combined insns, before
   it tries to recog the insn and determine its cost.  */
static bool
sh_legitimate_combined_insn (rtx_insn* insn)
{
  /* Reject combinations of memory loads and zero extensions, as these
     interfere with other combine patterns such as zero extracts and bit
     tests.  The SH2A movu.{b|w} insns are formed later in the
     'sh_optimize_extu_exts' pass after combine/split1.  */
  rtx p = PATTERN (insn);
  if (GET_CODE (p) == SET
      && REG_P (XEXP (p, 0)) && GET_MODE (XEXP (p, 0)) == SImode
      && GET_CODE (XEXP (p, 1)) == ZERO_EXTEND
      && MEM_P (XEXP (XEXP (p, 1), 0)))
      return false;

  return true;
}

bool
sh_fixed_condition_code_regs (unsigned int* p1, unsigned int* p2)
{
  *p1 = T_REG;
  *p2 = INVALID_REGNUM;
  return true;
}

/* Try to calculate the branch distance of a conditional branch in bytes.

   FIXME: Because of PR 59189 we can't use the CFG here.  Instead just
   walk from this insn into the next (fall-through) basic block and see if
   we hit the label.  */
unsigned int
sh_cbranch_distance (rtx_insn* _cbranch_insn, unsigned int max_dist)
{
  rtx_jump_insn* cbranch_insn = safe_as_a<rtx_jump_insn*> (_cbranch_insn);

  if (dump_file)
    {
      fprintf (dump_file, "sh_cbranch_distance insn = \n");
      print_rtl_single (dump_file, cbranch_insn);
    }

  unsigned int dist = 0;

  for (rtx_insn* i = next_nonnote_insn (cbranch_insn);
       i != NULL && dist < max_dist; i = next_nonnote_insn (i))
    {
      const unsigned int i_len = get_attr_length (i);
      dist += i_len;

      if (dump_file)
	fprintf (dump_file, "  insn %d  length = %u  dist = %u\n",
		 INSN_UID (i), i_len, dist);

      if (rtx_code_label* l = dyn_cast<rtx_code_label*> (i))
	{
	  if (l == cbranch_insn->jump_target ())
	    {
	      if (dump_file)
		fprintf (dump_file, "  cbranch dist = %u\n", dist);
	      return dist;
	    }
	  break;
	}
    }

  if (dump_file)
    fprintf (dump_file, "  cbranch dist = unknown\n");

  return unknown_cbranch_distance;
}

enum rtx_code
prepare_cbranch_operands (rtx *operands, machine_mode mode,
			  enum rtx_code comparison)
{
  gcc_assert (can_create_pseudo_p ());

  if (comparison == LAST_AND_UNUSED_RTX_CODE)
    comparison = GET_CODE (operands[0]);

  sh_canonicalize_comparison (comparison, operands[1], operands[2],
			      mode, false);

  rtx op1 = operands[1];
  operands[1] = force_reg (mode, op1);

  /* When we are handling DImode comparisons, we want to keep constants so
     that we can optimize the component comparisons; however, memory loads
     are better issued as a whole so that they can be scheduled well.
     SImode equality comparisons allow I08 constants, but only when they
     compare r0.  Hence, if operands[1] has to be loaded from somewhere else
     into a register, that register might as well be r0, and we allow the
     constant.  If it is already in a register, this is likely to be
     allocated to a different hard register, thus we load the constant into
     a register unless it is zero.  */
  if (!REG_P (operands[2])
      && (!CONST_INT_P (operands[2])
	  || (mode == SImode && operands[2] != CONST0_RTX (SImode)
	      && ((comparison != EQ && comparison != NE)
		  || (REG_P (op1) && REGNO (op1) != R0_REG)
		  || !satisfies_constraint_I08 (operands[2])))))
    operands[2] = force_reg (mode, operands[2]);

  return comparison;
}

void
expand_cbranchsi4 (rtx *operands, enum rtx_code comparison, int probability)
{
  rtx (*branch_expander) (rtx) = gen_branch_true;
  comparison = prepare_cbranch_operands (operands, SImode, comparison);
  switch (comparison)
    {
    case NE: case LT: case LE: case LTU: case LEU:
      comparison = reverse_condition (comparison);
      branch_expander = gen_branch_false;
    default: ;
    }
  emit_insn (gen_rtx_SET (get_t_reg_rtx (),
			  gen_rtx_fmt_ee (comparison, SImode,
					  operands[1], operands[2])));
  rtx_insn *jump = emit_jump_insn (branch_expander (operands[3]));
  if (probability >= 0)
    add_int_reg_note (jump, REG_BR_PROB, probability);
}

/* ??? How should we distribute probabilities when more than one branch
   is generated.  So far we only have some ad-hoc observations:
   - If the operands are random, they are likely to differ in both parts.
   - If comparing items in a hash chain, the operands are random or equal;
     operation should be EQ or NE.
   - If items are searched in an ordered tree from the root, we can expect
     the highpart to be unequal about half of the time; operation should be
     an inequality comparison, operands non-constant, and overall probability
     about 50%.  Likewise for quicksort.
   - Range checks will be often made against constants.  Even if we assume for
     simplicity an even distribution of the non-constant operand over a
     sub-range here, the same probability could be generated with differently
     wide sub-ranges - as long as the ratio of the part of the subrange that
     is before the threshold to the part that comes after the threshold stays
     the same.  Thus, we can't really tell anything here;
     assuming random distribution is at least simple.
 */
bool
expand_cbranchdi4 (rtx *operands, enum rtx_code comparison)
{
  enum rtx_code msw_taken, msw_skip, lsw_taken;
  rtx_code_label *skip_label = NULL;
  rtx op1h, op1l, op2h, op2l;
  int num_branches;
  int prob, rev_prob;
  int msw_taken_prob = -1, msw_skip_prob = -1, lsw_taken_prob = -1;

  comparison = prepare_cbranch_operands (operands, DImode, comparison);
  op1h = gen_highpart_mode (SImode, DImode, operands[1]);
  op2h = gen_highpart_mode (SImode, DImode, operands[2]);
  op1l = gen_lowpart (SImode, operands[1]);
  op2l = gen_lowpart (SImode, operands[2]);
  msw_taken = msw_skip = lsw_taken = LAST_AND_UNUSED_RTX_CODE;
  prob = split_branch_probability;
  rev_prob = REG_BR_PROB_BASE - prob;
  switch (comparison)
    {
    case EQ:
      msw_skip = NE;
      lsw_taken = EQ;
      if (prob >= 0)
	{
	  // If we had more precision, we'd use rev_prob - (rev_prob >> 32) .
	  msw_skip_prob = rev_prob;
	  if (REG_BR_PROB_BASE <= 65535)
	    lsw_taken_prob = prob ? REG_BR_PROB_BASE : 0;
	  else
	    {
	      lsw_taken_prob
		= (prob
		   ? (REG_BR_PROB_BASE
		      - ((gcov_type) REG_BR_PROB_BASE * rev_prob
			 / ((gcov_type) prob << 32)))
		   : 0);
	    }
	}
      break;
    case NE:
      msw_taken = NE;
      msw_taken_prob = prob;
      lsw_taken = NE;
      lsw_taken_prob = 0;
      break;
    case GTU: case GT:
      msw_taken = comparison;
      if (CONST_INT_P (op2l) && INTVAL (op2l) == -1)
	break;
      if (comparison != GTU || op2h != CONST0_RTX (SImode))
	msw_skip = swap_condition (msw_taken);
      lsw_taken = GTU;
      break;
    case GEU: case GE:
      if (op2l == CONST0_RTX (SImode))
	msw_taken = comparison;
      else
	{
	  msw_taken = comparison == GE ? GT : GTU;
	  msw_skip = swap_condition (msw_taken);
	  lsw_taken = GEU;
	}
      break;
    case LTU: case LT:
      msw_taken = comparison;
      if (op2l == CONST0_RTX (SImode))
	break;
      msw_skip = swap_condition (msw_taken);
      lsw_taken = LTU;
      break;
    case LEU: case LE:
      if (CONST_INT_P (op2l) && INTVAL (op2l) == -1)
	msw_taken = comparison;
      else
	{
	  lsw_taken = LEU;
	  if (comparison == LE)
	    msw_taken = LT;
	  else if (op2h != CONST0_RTX (SImode))
	    msw_taken = LTU;
	  else
	    {
	      msw_skip = swap_condition (LTU);
	      break;
	    }
	  msw_skip = swap_condition (msw_taken);
	}
      break;
    default: return false;
    }
  num_branches = ((msw_taken != LAST_AND_UNUSED_RTX_CODE)
		  + (msw_skip != LAST_AND_UNUSED_RTX_CODE)
		  + (lsw_taken != LAST_AND_UNUSED_RTX_CODE));
  if (comparison != EQ && comparison != NE && num_branches > 1)
    {
      if (!CONSTANT_P (operands[2])
	  && prob >= (int) (REG_BR_PROB_BASE * 3 / 8U)
	  && prob <= (int) (REG_BR_PROB_BASE * 5 / 8U))
	{
	  msw_taken_prob = prob / 2U;
	  msw_skip_prob
	    = REG_BR_PROB_BASE * rev_prob / (REG_BR_PROB_BASE + rev_prob);
	  lsw_taken_prob = prob;
	}
      else
	{
	  msw_taken_prob = prob;
	  msw_skip_prob = REG_BR_PROB_BASE;
	  /* ??? If we have a constant op2h, should we use that when
	     calculating lsw_taken_prob?  */
	  lsw_taken_prob = prob;
	}
    }
  operands[1] = op1h;
  operands[2] = op2h;
  operands[4] = NULL_RTX;

  if (msw_taken != LAST_AND_UNUSED_RTX_CODE)
    expand_cbranchsi4 (operands, msw_taken, msw_taken_prob);
  if (msw_skip != LAST_AND_UNUSED_RTX_CODE)
    {
      rtx taken_label = operands[3];

      /* Operands were possibly modified, but msw_skip doesn't expect this.
	 Always use the original ones.  */
      if (msw_taken != LAST_AND_UNUSED_RTX_CODE)
	{
	  operands[1] = op1h;
	  operands[2] = op2h;
	}

      operands[3] = skip_label = gen_label_rtx ();
      expand_cbranchsi4 (operands, msw_skip, msw_skip_prob);
      operands[3] = taken_label;
    }
  operands[1] = op1l;
  operands[2] = op2l;
  if (lsw_taken != LAST_AND_UNUSED_RTX_CODE)
    expand_cbranchsi4 (operands, lsw_taken, lsw_taken_prob);
  if (msw_skip != LAST_AND_UNUSED_RTX_CODE)
    emit_label (skip_label);
  return true;
}

/* Given an operand, return 1 if the evaluated operand plugged into an
   if_then_else will result in a branch_true, 0 if branch_false, or
   -1 if neither nor applies.  The truth table goes like this:

       op   | cmpval |   code  | result
   ---------+--------+---------+--------------------
      T (0) |   0    |  EQ (1) |  0 = 0 ^ (0 == 1)
      T (0) |   1    |  EQ (1) |  1 = 0 ^ (1 == 1)
      T (0) |   0    |  NE (0) |  1 = 0 ^ (0 == 0)
      T (0) |   1    |  NE (0) |  0 = 0 ^ (1 == 0)
     !T (1) |   0    |  EQ (1) |  1 = 1 ^ (0 == 1)
     !T (1) |   1    |  EQ (1) |  0 = 1 ^ (1 == 1)
     !T (1) |   0    |  NE (0) |  0 = 1 ^ (0 == 0)
     !T (1) |   1    |  NE (0) |  1 = 1 ^ (1 == 0)  */
int
sh_eval_treg_value (rtx op)
{
  if (t_reg_operand (op, GET_MODE (op)))
    return 1;
  if (negt_reg_operand (op, GET_MODE (op)))
    return 0;

  rtx_code code = GET_CODE (op);
  if ((code != EQ && code != NE) || !CONST_INT_P (XEXP (op, 1)))
    return -1;

  int cmpop = code == EQ ? 1 : 0;
  int cmpval = INTVAL (XEXP (op, 1));
  if (cmpval != 0 && cmpval != 1)
    return -1;

  int t;
  if (t_reg_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0))))
    t = 0;
  else if (negt_reg_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0))))
    t = 1;
  else
    return -1;
  
  return t ^ (cmpval == cmpop);
}

/* Emit INSN, possibly in a PARALLEL with an USE/CLOBBER of FPSCR bits in case
   of floating-point comparisons.  */
static void
sh_emit_set_t_insn (rtx insn, machine_mode mode)
{
  if (TARGET_FPU_ANY && GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_CODE (insn) != PARALLEL)
    {
      insn = gen_rtx_PARALLEL (VOIDmode,
	  gen_rtvec (3, insn,
	      gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, FPSCR_STAT_REG)),
	      gen_rtx_USE (VOIDmode, gen_rtx_REG (SImode, FPSCR_MODES_REG))));
    }
  emit_insn (insn);
}

/* Prepare the operands for an scc instruction; make sure that the
   compare has been done and the result is in T_REG.  */
void
sh_emit_scc_to_t (enum rtx_code code, rtx op0, rtx op1)
{
  rtx t_reg = get_t_reg_rtx ();
  enum rtx_code oldcode = code;

  /* First need a compare insn.  */
  switch (code)
    {
    case NE:
      /* It isn't possible to handle this case.  */
      gcc_unreachable ();
    case LT:
      code = GT;
      break;
    case LE:
      code = GE;
      break;
    case LTU:
      code = GTU;
      break;
    case LEU:
      code = GEU;
      break;
    default:
      break;
    }
  if (code != oldcode)
    std::swap (op0, op1);

  machine_mode mode = GET_MODE (op0);
  if (mode == VOIDmode)
    mode = GET_MODE (op1);

  op0 = force_reg (mode, op0);
  if ((code != EQ && code != NE
       && (op1 != const0_rtx
	   || code == GTU  || code == GEU || code == LTU || code == LEU))
      || (mode == DImode && op1 != const0_rtx)
      || (TARGET_SH2E && GET_MODE_CLASS (mode) == MODE_FLOAT))
    op1 = force_reg (mode, op1);

  sh_emit_set_t_insn (gen_rtx_SET (t_reg,
			           gen_rtx_fmt_ee (code, SImode, op0, op1)),
		      mode);
}

/* Called from the md file, set up the operands of a compare instruction.  */
void
sh_emit_compare_and_branch (rtx *operands, machine_mode mode)
{
  enum rtx_code code = GET_CODE (operands[0]);
  enum rtx_code branch_code;
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx insn;
  bool need_ccmpeq = false;

  if (TARGET_SH2E && GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      op0 = force_reg (mode, op0);
      op1 = force_reg (mode, op1);
    }
  else
    {
      if (code != EQ || mode == DImode)
	{
	  /* Force args into regs, since we can't use constants here.  */
	  op0 = force_reg (mode, op0);
	  if (op1 != const0_rtx || code == GTU  || code == GEU)
	    op1 = force_reg (mode, op1);
        }
    }

  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      if (code == LT
	  || (code == LE && TARGET_IEEE && TARGET_SH2E)
	  || (code == GE && !(TARGET_IEEE && TARGET_SH2E)))
	{
	  std::swap (op0, op1);
	  code = swap_condition (code);
	}

      /* GE becomes fcmp/gt+fcmp/eq, for SH2E and TARGET_IEEE only.  */
      if (code == GE)
	{
	  gcc_assert (TARGET_IEEE && TARGET_SH2E);
	  need_ccmpeq = true;
	  code = GT;
	}

      /* Now we can have EQ, NE, GT, LE.  NE and LE are then transformed
	 to EQ/GT respectively.  */
      gcc_assert (code == EQ || code == GT || code == NE || code == LE);
    }

  switch (code)
    {
    case EQ:
    case GT:
    case GE:
    case GTU:
    case GEU:
      branch_code = code;
      break;
    case NE:
    case LT:
    case LE:
    case LTU:
    case LEU:
      branch_code = reverse_condition (code);
      break;
    default:
      gcc_unreachable ();
    }

  insn = gen_rtx_SET (get_t_reg_rtx (),
		      gen_rtx_fmt_ee (branch_code, SImode, op0, op1));

  sh_emit_set_t_insn (insn, mode);
  if (need_ccmpeq)
    sh_emit_set_t_insn (gen_ieee_ccmpeqsf_t (op0, op1), mode);

  if (branch_code == code)
    emit_jump_insn (gen_branch_true (operands[3]));
  else
    emit_jump_insn (gen_branch_false (operands[3]));
}

void
sh_emit_compare_and_set (rtx *operands, machine_mode mode)
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = operands[2];
  rtx op1 = operands[3];
  rtx_code_label *lab = NULL;
  bool invert = false;

  op0 = force_reg (mode, op0);
  if ((code != EQ && code != NE
       && (op1 != const0_rtx
	   || code == GTU  || code == GEU || code == LTU || code == LEU))
      || (mode == DImode && op1 != const0_rtx)
      || (TARGET_SH2E && GET_MODE_CLASS (mode) == MODE_FLOAT))
    op1 = force_reg (mode, op1);

  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      if (code == LT || code == LE)
	{
	  std::swap (op0, op1);
	  code = swap_condition (code);
	}
      if (code == GE)
	{
	  if (TARGET_IEEE)
	    {
	      lab = gen_label_rtx ();
	      sh_emit_scc_to_t (EQ, op0, op1);
	      emit_jump_insn (gen_branch_true (lab));
	      code = GT;
	   }
	  else
	    {
	      code = LT;
	      invert = true;
	    }
	}
    }

  if (code == NE)
    {
      code = EQ;
      invert = true;
    }

  sh_emit_scc_to_t (code, op0, op1);
  if (lab)
    emit_label (lab);
  if (invert)
    emit_insn (gen_movnegt (operands[0], get_t_reg_rtx ()));
  else
    emit_move_insn (operands[0], get_t_reg_rtx ());
}

/* Functions to output assembly code.  */

/* Return a sequence of instructions to perform DI or DF move.

   Since the SH cannot move a DI or DF in one instruction, we have
   to take care when we see overlapping source and dest registers.  */
const char *
output_movedouble (rtx insn ATTRIBUTE_UNUSED, rtx operands[],
		   machine_mode mode)
{
  rtx dst = operands[0];
  rtx src = operands[1];

  if (MEM_P (dst)
      && GET_CODE (XEXP (dst, 0)) == PRE_DEC)
    return     "mov.l	%T1,%0"	"\n"
	   "	mov.l	%1,%0";

  if (register_operand (dst, mode)
      && register_operand (src, mode))
    {
      if (REGNO (src) == MACH_REG)
	return         "sts	mach,%S0" "\n"
	       "	sts	macl,%R0";

      /* When mov.d r1,r2 do r2->r3 then r1->r2;
         when mov.d r1,r0 do r1->r0 then r2->r1.  */
      if (REGNO (src) + 1 == REGNO (dst))
	return         "mov	%T1,%T0" "\n"
	       "	mov	%1,%0";
      else
	return         "mov	%1,%0" "\n"
	       "	mov	%T1,%T0";
    }
  else if (CONST_INT_P (src))
    {
      if (INTVAL (src) < 0)
	output_asm_insn ("mov	#-1,%S0", operands);
      else
	output_asm_insn ("mov	#0,%S0", operands);

      return "mov	%1,%R0";
    }
  else if (MEM_P (src))
    {
      int ptrreg = -1;
      int dreg = REGNO (dst);
      rtx inside = XEXP (src, 0);

      switch (GET_CODE (inside))
	{
	case REG:
	  ptrreg = REGNO (inside);
	  break;

	case SUBREG:
	  ptrreg = subreg_regno (inside);
	  break;

	case PLUS:
	  ptrreg = REGNO (XEXP (inside, 0));
	  /* ??? A r0+REG address shouldn't be possible here, because it isn't
	     an offsettable address.  Unfortunately, offsettable addresses use
	     QImode to check the offset, and a QImode offsettable address
	     requires r0 for the other operand, which is not currently
	     supported, so we can't use the 'o' constraint.
	     Thus we must check for and handle r0+REG addresses here.
	     We punt for now, since this is likely very rare.  */
	  gcc_assert (!REG_P (XEXP (inside, 1)));
	  break;
	  
	case LABEL_REF:
	  return       "mov.l	%1,%0" "\n"
		 "	mov.l	%1+4,%T0";
	case POST_INC:
	  return       "mov.l	%1,%0" "\n"
		 "	mov.l	%1,%T0";
	default:
	  gcc_unreachable ();
	}

      /* Work out the safe way to copy.  Copy into the second half first.  */
      if (dreg == ptrreg)
	return         "mov.l	%T1,%T0" "\n"
	       "	mov.l	%1,%0";
    }

  return       "mov.l	%1,%0" "\n"
	 "	mov.l	%T1,%T0";
}

/* Print an instruction which would have gone into a delay slot after
   another instruction, but couldn't because the other instruction expanded
   into a sequence where putting the slot insn at the end wouldn't work.  */
static void
print_slot (rtx_sequence *seq)
{
  final_scan_insn (seq->insn (1), asm_out_file, optimize, 1, NULL);

  seq->insn (1)->set_deleted ();
}

const char *
output_far_jump (rtx_insn *insn, rtx op)
{
  struct { rtx lab, reg, op; } this_jmp;
  rtx_code_label *braf_base_lab = NULL;
  const char *jump;
  int far;
  int offset = branch_dest (insn) - INSN_ADDRESSES (INSN_UID (insn));
  rtx_insn *prev;

  this_jmp.lab = gen_label_rtx ();

  if (TARGET_SH2
      && offset >= -32764
      && offset - get_attr_length (insn) <= 32766
      && ! CROSSING_JUMP_P (insn))
    {
      far = 0;
      jump =   "mov.w	%O0,%1" "\n"
	     "	braf	%1";
    }
  else
    {
      far = 1;
      if (flag_pic)
	{
	  if (TARGET_SH2)
	    jump =     "mov.l	%O0,%1" "\n"
		   "	braf	%1";
	  else
	    jump =     "mov.l	r0,@-r15"	"\n"
		   "	mova	%O0,r0"		"\n"
		   "	mov.l	@r0,%1"		"\n"
		   "	add	r0,%1"		"\n"
		   "	mov.l	@r15+,r0"	"\n"
		   "	jmp	@%1";
	}
      else
	jump =         "mov.l	%O0,%1" "\n"
	       "	jmp	@%1";
    }
  /* If we have a scratch register available, use it.  */
  if (NONJUMP_INSN_P ((prev = prev_nonnote_insn (insn)))
      && INSN_CODE (prev) == CODE_FOR_indirect_jump_scratch)
    {
      this_jmp.reg = SET_DEST (XVECEXP (PATTERN (prev), 0, 0));
      if (REGNO (this_jmp.reg) == R0_REG && flag_pic && ! TARGET_SH2)
	jump =         "mov.l	r1,@-r15"	"\n"
	       "	mova	%O0,r0"		"\n"
	       "	mov.l	@r0,r1"		"\n"
	       "	add	r1,r0"		"\n"
	       "	mov.l	@r15+,r1"	"\n"
	       "	jmp	@%1";
      output_asm_insn (jump, &this_jmp.lab);
      if (dbr_sequence_length ())
	print_slot (final_sequence);
      else
	output_asm_insn ("nop", 0);
    }
  else
    {
      /* Output the delay slot insn first if any.  */
      if (dbr_sequence_length ())
	print_slot (final_sequence);

      this_jmp.reg = gen_rtx_REG (SImode, 13);
      output_asm_insn ("mov.l	r13,@-r15", 0);
      output_asm_insn (jump, &this_jmp.lab);
      output_asm_insn ("mov.l	@r15+,r13", 0);
    }
  if (far && flag_pic && TARGET_SH2)
    {
      braf_base_lab = gen_label_rtx ();
      (*targetm.asm_out.internal_label) (asm_out_file, "L",
				 CODE_LABEL_NUMBER (braf_base_lab));
    }
  if (far)
    output_asm_insn (".align	2", 0);
  (*targetm.asm_out.internal_label) (asm_out_file, "L", CODE_LABEL_NUMBER (this_jmp.lab));
  this_jmp.op = op;
  if (far && flag_pic)
    {
      if (TARGET_SH2)
	this_jmp.lab = braf_base_lab;
      output_asm_insn (".long	%O2-%O0", &this_jmp.lab);
    }
  else
    output_asm_insn (far ? ".long	%O2" : ".word %O2-%O0", &this_jmp.lab);
  return "";
}

/* Local label counter, used for constants in the pool and inside
   pattern branches.  */
static int lf = 100;

/* Output code for ordinary branches.  */
const char *
output_branch (int logic, rtx_insn *insn, rtx *operands)
{
  switch (get_attr_length (insn))
    {
    case 6:
      /* This can happen if filling the delay slot has caused a forward
	 branch to exceed its range (we could reverse it, but only
	 when we know we won't overextend other branches; this should
	 best be handled by relaxation).
	 It can also happen when other condbranches hoist delay slot insn
	 from their destination, thus leading to code size increase.
	 But the branch will still be in the range -4092..+4098 bytes.  */
      if (! TARGET_RELAX)
	{
	  int label = lf++;
	  /* The call to print_slot will clobber the operands.  */
	  rtx op0 = operands[0];

	  /* If the instruction in the delay slot is annulled (true), then
	     there is no delay slot where we can put it now.  The only safe
	     place for it is after the label.  final will do that by default.  */

	  if (final_sequence
	      && ! INSN_ANNULLED_BRANCH_P (final_sequence->insn (0))
	      && get_attr_length (final_sequence->insn (1)))
	    {
	      asm_fprintf (asm_out_file, "\tb%s%ss\t%LLF%d\n", logic ? "f" : "t",
	                   ASSEMBLER_DIALECT ? "/" : ".", label);
	      print_slot (final_sequence);
	    }
	  else
	    asm_fprintf (asm_out_file, "\tb%s\t%LLF%d\n", logic ? "f" : "t", label);

	  output_asm_insn ("bra\t%l0", &op0);
	  fprintf (asm_out_file, "\tnop\n");
	  (*targetm.asm_out.internal_label) (asm_out_file, "LF", label);

	  return "";
	}
      /* When relaxing, handle this like a short branch.  The linker
	 will fix it up if it still doesn't fit after relaxation.  */
    case 2:
      return logic ? "bt%.\t%l0" : "bf%.\t%l0";

      /* These are for SH2e, in which we have to account for the
	 extra nop because of the hardware bug in annulled branches.  */
    case 8:
      if (! TARGET_RELAX)
	{
	  int label = lf++;

	  gcc_assert (!final_sequence
		      || !(INSN_ANNULLED_BRANCH_P
			   (XVECEXP (final_sequence, 0, 0))));
	  asm_fprintf (asm_out_file, "b%s%ss\t%LLF%d\n",
		       logic ? "f" : "t",
		       ASSEMBLER_DIALECT ? "/" : ".", label);
	  fprintf (asm_out_file, "\tnop\n");
	  output_asm_insn ("bra\t%l0", operands);
	  fprintf (asm_out_file, "\tnop\n");
	  (*targetm.asm_out.internal_label) (asm_out_file, "LF", label);

	  return "";
	}
      /* When relaxing, fall through.  */
    case 4:
      {
	char buffer[10];

	sprintf (buffer, "b%s%ss\t%%l0",
		 logic ? "t" : "f",
		 ASSEMBLER_DIALECT ? "/" : ".");
	output_asm_insn (buffer, &operands[0]);
	return "nop";
      }

    default:
      /* There should be no longer branches now - that would
	 indicate that something has destroyed the branches set
	 up in machine_dependent_reorg.  */
      gcc_unreachable ();
    }
}

/* Output a code sequence for INSN using TEMPL with OPERANDS; but before,
   fill in operands 9 as a label to the successor insn.
   We try to use jump threading where possible.
   IF CODE matches the comparison in the IF_THEN_ELSE of a following jump,
   we assume the jump is taken.  I.e. EQ means follow jmp and bf, NE means
   follow jmp and bt, if the address is in range.  */
const char *
output_branchy_insn (enum rtx_code code, const char *templ,
		     rtx_insn *insn, rtx *operands)
{
  rtx_insn *next_insn = NEXT_INSN (insn);

  if (next_insn && JUMP_P (next_insn) && condjump_p (next_insn))
    {
      rtx src = SET_SRC (PATTERN (next_insn));
      if (GET_CODE (src) == IF_THEN_ELSE && GET_CODE (XEXP (src, 0)) != code)
	{
	  /* Following branch not taken */
	  rtx_code_label *lab = gen_label_rtx ();
	  emit_label_after (lab, next_insn);
	  INSN_ADDRESSES_NEW (lab,
			      INSN_ADDRESSES (INSN_UID (next_insn))
			      + get_attr_length (next_insn));
	  operands[9] = lab;
	  return templ;
	}
      else
	{
	  int offset = (branch_dest (next_insn)
			- INSN_ADDRESSES (INSN_UID (next_insn)) + 4);
	  if (offset >= -252 && offset <= 258)
	    {
	      if (GET_CODE (src) == IF_THEN_ELSE)
		/* branch_true */
		src = XEXP (src, 1);
	      operands[9] = src;
	      return templ;
	    }
	}
    }
  rtx_code_label *lab = gen_label_rtx ();
  emit_label_after (lab, insn);
  INSN_ADDRESSES_NEW (lab,
		      INSN_ADDRESSES (INSN_UID (insn))
		      + get_attr_length (insn));
  operands[9] = lab;
  return templ;
}

const char *
output_ieee_ccmpeq (rtx_insn *insn, rtx *operands)
{
  return output_branchy_insn (NE,      "bt	%l9" "\n"
				  "	fcmp/eq	%1,%0",
			      insn, operands);
}

/* Output the start of the assembler file.  */
static void
sh_file_start (void)
{
  default_file_start ();

  if (TARGET_ELF)
    /* We need to show the text section with the proper
       attributes as in TEXT_SECTION_ASM_OP, before dwarf2out
       emits it without attributes in TEXT_SECTION_ASM_OP, else GAS
       will complain.  We can teach GAS specifically about the
       default attributes for our choice of text section, but
       then we would have to change GAS again if/when we change
       the text section name.  */
    fprintf (asm_out_file, "%s\n", TEXT_SECTION_ASM_OP);
  else
    /* Switch to the data section so that the coffsem symbol
       isn't in the text section.  */
    switch_to_section (data_section);

  if (TARGET_LITTLE_ENDIAN)
    fputs ("\t.little\n", asm_out_file);
}

/* Implementation of TARGET_ASM_INTEGER for SH.  Pointers to functions
   need to be output as pointers to function descriptors for
   FDPIC.  */

static bool
sh_assemble_integer (rtx value, unsigned int size, int aligned_p)
{
  if (TARGET_FDPIC && size == UNITS_PER_WORD
      && GET_CODE (value) == SYMBOL_REF && SYMBOL_REF_FUNCTION_P (value))
    {
      fputs ("\t.long\t", asm_out_file);
      output_addr_const (asm_out_file, value);
      fputs ("@FUNCDESC\n", asm_out_file);
      return true;
    }
  return default_assemble_integer (value, size, aligned_p);
}

/* Check if PAT includes UNSPEC_CALLER unspec pattern.  */
static bool
unspec_caller_rtx_p (rtx pat)
{
  rtx base, offset;
  split_const (pat, &base, &offset);

  if (GET_CODE (base) == UNSPEC)
    {
      if (XINT (base, 1) == UNSPEC_CALLER)
	return true;
      for (int i = 0; i < XVECLEN (base, 0); i++)
	if (unspec_caller_rtx_p (XVECEXP (base, 0, i)))
	  return true;
    }
  return false;
}

/* Indicate that INSN cannot be duplicated.  This is true for insn
   that generates a unique label.  */
static bool
sh_cannot_copy_insn_p (rtx_insn *insn)
{
  if (!reload_completed || !flag_pic)
    return false;

  if (!NONJUMP_INSN_P (insn))
    return false;
  if (asm_noperands (insn) >= 0)
    return false;

  rtx pat = PATTERN (insn);

  if (GET_CODE (pat) == CLOBBER || GET_CODE (pat) == USE)
    return false;

  if (TARGET_FDPIC && GET_CODE (pat) == PARALLEL)
    {
      rtx t = XVECEXP (pat, 0, XVECLEN (pat, 0) - 1);
      if (GET_CODE (t) == USE && unspec_caller_rtx_p (XEXP (t, 0)))
	return true;
    }

  if (GET_CODE (pat) != SET)
    return false;
  pat = SET_SRC (pat);

  if (unspec_caller_rtx_p (pat))
    return true;

  return false;
}

/* Number of instructions used to make an arithmetic right shift by N.  */
static const char ashiftrt_insns[] =
  { 0,1,2,3,4,5,8,8,8,8,8,8,8,8,8,8,2,3,4,5,8,8,8,8,8,8,8,8,8,8,8,2};

/* Description of a logical left or right shift, when expanded to a sequence
   of 1/2/8/16 shifts.
   Notice that one bit right shifts clobber the T bit.  One bit left shifts
   are done with an 'add Rn,Rm' insn and thus do not clobber the T bit.  */
enum
{
  ASHL_CLOBBERS_T = 1 << 0,
  LSHR_CLOBBERS_T = 1 << 1
};

struct ashl_lshr_sequence
{
  char insn_count;
  signed char amount[6];
  char clobbers_t;
};

static const struct ashl_lshr_sequence ashl_lshr_seq[32] =
{
  { 0, { 0 },		    0 },		// 0
  { 1, { 1 },		    LSHR_CLOBBERS_T },
  { 1, { 2 },		    0 },
  { 2, { 2, 1 },	    LSHR_CLOBBERS_T },
  { 2, { 2, 2 },	    0 },		// 4
  { 3, { 2, 1, 2 },	    LSHR_CLOBBERS_T },
  { 3, { 2, 2, 2 },	    0 },
  { 4, { 2, 2, 1, 2 },	    LSHR_CLOBBERS_T },
  { 1, { 8 },		    0 },		// 8
  { 2, { 8, 1 },	    LSHR_CLOBBERS_T },
  { 2, { 8, 2 },	    0 },
  { 3, { 8, 1, 2 },	    LSHR_CLOBBERS_T },
  { 3, { 8, 2, 2 },	    0 },		// 12
  { 4, { 8, 2, 1, 2 },	    LSHR_CLOBBERS_T },
  { 3, { 8, -2, 8 },	    0 },
  { 3, { 8, -1, 8 },	    ASHL_CLOBBERS_T },
  { 1, { 16 },		    0 },		// 16
  { 2, { 16, 1 },	    LSHR_CLOBBERS_T },
  { 2, { 16, 2 },	    0 },
  { 3, { 16, 1, 2 },	    LSHR_CLOBBERS_T },
  { 3, { 16, 2, 2 },	    0 },		// 20
  { 4, { 16, 2, 1, 2 },	    LSHR_CLOBBERS_T },
  { 3, { 16, -2, 8 },	    0 },
  { 3, { 16, -1, 8 },	    ASHL_CLOBBERS_T },
  { 2, { 16, 8 },	    0 },		// 24
  { 3, { 16, 1, 8 },	    LSHR_CLOBBERS_T },
  { 3, { 16, 8, 2 },	    0 },
  { 4, { 16, 8, 1, 2 },     LSHR_CLOBBERS_T },
  { 4, { 16, 8, 2, 2 },	    0 },		// 28
  { 4, { 16, -1, -2, 16 },  ASHL_CLOBBERS_T },
  { 3, { 16, -2, 16 },	    0 },

  /* For a right shift by 31 a 2 insn shll-movt sequence can be used.
     For a left shift by 31 a 2 insn and-rotl sequences can be used.
     However, the shift-and combiner code needs this entry here to be in
     terms of real shift insns.  */
  { 3, { 16, -1, 16 },	    ASHL_CLOBBERS_T }
};

/* Individual shift amounts for shift amounts < 16, up to three highmost
   bits might be clobbered.  This is typically used when combined with some
   kind of sign or zero extension.  */
static const struct ashl_lshr_sequence ext_ashl_lshr_seq[32] =
{
  { 0, { 0 },		    0 },		// 0
  { 1, { 1 },		    LSHR_CLOBBERS_T },
  { 1, { 2 },		    0 },
  { 2, { 2, 1 },	    LSHR_CLOBBERS_T },
  { 2, { 2, 2 },	    0 },		// 4
  { 3, { 2, 1, 2 },	    LSHR_CLOBBERS_T },
  { 2, { 8, -2 },	    0 },
  { 2, { 8, -1 },	    ASHL_CLOBBERS_T },
  { 1, { 8 },		    0 },		// 8
  { 2, { 8, 1 },	    LSHR_CLOBBERS_T },
  { 2, { 8, 2 },	    0 },
  { 3, { 8, 1, 2 },	    LSHR_CLOBBERS_T },
  { 3, { 8, 2, 2 },	    0 },		// 12
  { 3, { 16, -2, -1 },	    ASHL_CLOBBERS_T },
  { 2, { 16, -2 },	    0 },
  { 2, { 16, -1 },	    ASHL_CLOBBERS_T },
  { 1, { 16 },		    0 },		// 16
  { 2, { 16, 1 },	    LSHR_CLOBBERS_T },
  { 2, { 16, 2 },	    0 },
  { 3, { 16, 1, 2 },	    LSHR_CLOBBERS_T },
  { 3, { 16, 2, 2 },	    0 },		// 20
  { 4, { 16, 2, 1, 2 },	    LSHR_CLOBBERS_T },
  { 3, { 16, -2, 8 },	    0 },
  { 3, { 16, -1, 8 },	    ASHL_CLOBBERS_T },
  { 2, { 16, 8 },	    0 },		// 24
  { 3, { 16, 1, 8 },	    LSHR_CLOBBERS_T },
  { 3, { 16, 8, 2 },	    0 },
  { 4, { 16, 8, 1, 2 },	    LSHR_CLOBBERS_T },
  { 4, { 16, 8, 2, 2 },	    0 },		// 28
  { 4, { 16, -1, -2, 16 },  ASHL_CLOBBERS_T },
  { 3, { 16, -2, 16 },	    0 },
  { 3, { 16, -1, 16 },	    ASHL_CLOBBERS_T }
};

/* Return true if a shift left consisting of 1/2/8/16 shift instructions
   will clobber the T bit.  */
bool
sh_ashlsi_clobbers_t_reg_p (rtx shift_amount)
{
  gcc_assert (CONST_INT_P (shift_amount));
  
  const int shift_amount_i = INTVAL (shift_amount) & 31;

  /* Special case for shift count of 31: use and-rotl sequence.  */
  if (shift_amount_i == 31)
    return true;

  return (ashl_lshr_seq[shift_amount_i].clobbers_t
	  & ASHL_CLOBBERS_T) != 0;
}

/* Return true if a logical right shift consisting of 1/2/8/16 shift
   instructions will clobber the T bit.  */
bool
sh_lshrsi_clobbers_t_reg_p (rtx shift_amount)
{
  gcc_assert (CONST_INT_P (shift_amount));

  /* For right shifts the constant might be negative.  */
  const int shift_amount_i = std::abs (INTVAL (shift_amount)) & 31;
 
  /* Special case for shift count of 31: use shll-movt sequence.  */
  if (shift_amount_i == 31)
    return true;

  return (ashl_lshr_seq[shift_amount_i].clobbers_t
	  & LSHR_CLOBBERS_T) != 0;
}

/* Return true if it is potentially beneficial to use a dynamic shift
   instruction (shad / shar) instead of a combination of 1/2/8/16 
   shift instructions for the specified shift count.
   If dynamic shifts are not available, always return false.  */
bool
sh_dynamicalize_shift_p (rtx count)
{
  gcc_assert (CONST_INT_P (count));

  /* For right shifts the constant might be negative.  */
  const int shift_amount_i = std::abs (INTVAL (count)) & 31;
  int insn_count;

  /* For left and right shifts, there are shorter 2 insn sequences for
     shift amounts of 31.  */
  if (shift_amount_i == 31)
    insn_count = 2;
  else
    insn_count = ashl_lshr_seq[shift_amount_i].insn_count;

  return TARGET_DYNSHIFT && (insn_count > 1 + SH_DYNAMIC_SHIFT_COST);
}

/* Assuming we have a value that has been sign-extended by at least one bit,
   can we use the ext_shift_amounts with the last shift turned to an
   arithmetic shift to shift it by N without data loss, and quicker than by
   other means?  */
#define EXT_SHIFT_SIGNED(n) (((n) | 8) == 15)

/* Return the cost of a shift.  */
static inline int
shiftcosts (rtx x)
{
  if (GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
    {
      if (GET_MODE (x) == DImode
	  && CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) == 1)
	return 2;

      /* Everything else is invalid, because there is no pattern for it.  */
      return -1;
    }
  /* If shift by a non constant, then this will be expensive.  */
  if (!CONST_INT_P (XEXP (x, 1)))
    return SH_DYNAMIC_SHIFT_COST;

  /* Otherwise, return the true cost in instructions.  Cope with out of range
     shift counts more or less arbitrarily.  */
  int value = INTVAL (XEXP (x, 1)) & 31;

  if (GET_CODE (x) == ASHIFTRT)
    {
      int cost = ashiftrt_insns[value];
      /* If dynamic shifts are available and profitable in this case, then we
	 put the constant in a reg and use shad.  */
      if (cost > 1 + SH_DYNAMIC_SHIFT_COST)
	cost = 1 + SH_DYNAMIC_SHIFT_COST;
      return cost;
    }
  else
    return ashl_lshr_seq[value].insn_count;
}

/* Return the cost of an AND/XOR/IOR operation.  */
static inline int
and_xor_ior_costs (rtx x, int code)
{
  /* On SH1-4 we have only max. SImode operations.
     Double the cost for modes > SImode.  */
  const int cost_scale = GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD ? 2 : 1;

  /* A logical operation with two registers is a single cycle
     instruction.  */
  if (!CONST_INT_P (XEXP (x, 1)))
    return 1 * cost_scale;

  int i = INTVAL (XEXP (x, 1));

  /* These constants are single cycle extu.[bw] instructions.  */
  if ((i == 0xff || i == 0xffff) && code == AND)
    return 1 * cost_scale;
  /* Constants that can be used in an instruction as an immediate are
     a single cycle, but this requires r0, so make it a little more
     expensive.  */
  if (CONST_OK_FOR_K08 (i))
    return 2 * cost_scale;
  /* Constants that can be loaded with a mov immediate need one more cycle.
     This case is probably unnecessary.  */
  if (CONST_OK_FOR_I08 (i))
    return 2 * cost_scale;
  /* Any other constant requires an additional 2 cycle pc-relative load.
     This case is probably unnecessary.  */
  return 3 * cost_scale;
}

/* Return the cost of an addition or a subtraction.  */
static inline int
addsubcosts (rtx x)
{
  if (GET_MODE (x) == SImode)
    {
      /* The addc or subc patterns will eventually become one or two
	 instructions.  Below are some costs for some of the patterns
	 which combine would reject because the costs of the individual
	 insns in the patterns are lower.

	 FIXME: It would be much easier if we had something like insn cost
	 attributes and the cost calculation machinery used those attributes
	 in the first place.  This would eliminate redundant recog-like C
	 code to calculate costs of complex patterns.  */
      rtx op0 = XEXP (x, 0);
      rtx op1 = XEXP (x, 1);

      if (GET_CODE (x) == PLUS)
	{
	  if (GET_CODE (op0) == AND
	      && XEXP (op0, 1) == const1_rtx
	      && (GET_CODE (op1) == PLUS
		  || (GET_CODE (op1) == MULT && XEXP (op1, 1) == const2_rtx)))
	    return 1;

	  if (GET_CODE (op0) == MULT && XEXP (op0, 1) == const2_rtx
	      && GET_CODE (op1) == LSHIFTRT
	      && CONST_INT_P (XEXP (op1, 1)) && INTVAL (XEXP (op1, 1)) == 31)
	    return 1;
	}
      /* Let's assume that adding the result of an insns that stores into
	 the T bit is cheap.  */
      if (treg_set_expr (op1, SImode))
	return 1;
      if (treg_set_expr (op0, SImode))
	return 1;
    }

  /* On SH1-4 we have only max. SImode operations.
     Double the cost for modes > SImode.  */
  const int cost_scale = GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD ? 2 : 1;

  /* Adding a register is a single cycle insn.  */
  if (REG_P (XEXP (x, 1))
      || GET_CODE (XEXP (x, 1)) == SUBREG)
    return 1 * cost_scale;

  /* Likewise for small constants.  */
  if (CONST_INT_P (XEXP (x, 1))
      && CONST_OK_FOR_ADD (INTVAL (XEXP (x, 1))))
    return 1 * cost_scale;

  /* Any other constant requires a 2 cycle pc-relative load plus an
     addition.  */
  return 3 * cost_scale;
}

/* Return the cost of a multiply.  */
static inline int
multcosts (rtx x ATTRIBUTE_UNUSED)
{
  if (sh_multcost >= 0)
    return sh_multcost;

  if (TARGET_SH2)
    {
      /* We have a mul insn, so we can never take more than the mul and the
	 read of the mac reg, but count more because of the latency and extra
	 reg usage.  */
      if (optimize_size)
	return 2;
      return 3;
    }

  /* If we're aiming at small code, then just count the number of
     insns in a multiply call sequence.  */
  if (optimize_size)
    return 5;

  /* Otherwise count all the insns in the routine we'd be calling too.  */
  return 20;
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */
static bool
sh_rtx_costs (rtx x, machine_mode mode ATTRIBUTE_UNUSED, int outer_code,
	      int opno ATTRIBUTE_UNUSED,
	      int *total, bool speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);

  switch (code)
    {
      /* The lower-subreg pass decides whether to split multi-word regs
	 into individual regs by looking at the cost for a SET of certain
	 modes with the following patterns:
	   (set (reg) (reg)) 
	   (set (reg) (const_int 0))
	 On machines that support vector-move operations a multi-word move
	 is the same cost as individual reg move.  On SH there is no
	 vector-move, so we have to provide the correct cost in the number
	 of move insns to load/store the reg of the mode in question.  */
    case SET:
      if (register_operand (SET_DEST (x), VOIDmode)
	    && (register_operand (SET_SRC (x), VOIDmode)
		|| satisfies_constraint_Z (SET_SRC (x))))
	{
	  const machine_mode mode = GET_MODE (SET_DEST (x));
	  *total = COSTS_N_INSNS (GET_MODE_SIZE (mode)
				  / mov_insn_size (mode, TARGET_SH2A));
	  return true;
        }
      return false;

    /* The cost of a mem access is mainly the cost of the address mode.  */
    case MEM:
      *total = sh_address_cost (XEXP (x, 0), GET_MODE (x), MEM_ADDR_SPACE (x),
				true);
      return true;

    case IF_THEN_ELSE:
      /* This case is required for the if_then_else negc pattern.  */
      if (treg_set_expr (XEXP (x, 0), SImode))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else
	return false;

    /* Zero extracts of single bits are usually combine patterns for the
       tst insns.  */
    case ZERO_EXTRACT:
      if (GET_CODE (XEXP (x, 0)) == XOR
	  && arith_reg_operand (XEXP (XEXP (x, 0), 0), VOIDmode)
	  && XEXP (x, 1) == const1_rtx
	  && CONST_INT_P (XEXP (x, 2))
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  /* Check that the xor constaint overlaps with the extracted bit.  */
	  && (INTVAL (XEXP (XEXP (x, 0), 1)) & (1LL << INTVAL (XEXP (x, 2)))))
	{
	  *total = 1; //COSTS_N_INSNS (1);
	  return true;
	}

      /* div0s variant.  */
      if (GET_CODE (XEXP (x, 0)) == XOR
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == XOR
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
	{
	  *total = 1;
	  return true;
	}
      return false;

    /* The cost of a sign or zero extend depends on whether the source is a
       reg or a mem.  In case of a mem take the address into acount.  */
    case SIGN_EXTEND:
      if (arith_reg_operand (XEXP (x, 0), GET_MODE (XEXP (x, 0))))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      if (MEM_P (XEXP (x, 0)))
	{
	  *total = sh_address_cost (XEXP (XEXP (x, 0), 0),
				    GET_MODE (XEXP (x, 0)),
				    MEM_ADDR_SPACE (XEXP (x, 0)), true);
	  return true;
	}
      return false;

    case ZERO_EXTEND:
      if (arith_reg_operand (XEXP (x, 0), GET_MODE (XEXP (x, 0))))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else if (TARGET_SH2A && MEM_P (XEXP (x, 0))
	       && (GET_MODE (XEXP (x, 0)) == QImode
		   || GET_MODE (XEXP (x, 0)) == HImode))
	{
	  /* Handle SH2A's movu.b and movu.w insn.  */
	  *total = sh_address_cost (XEXP (XEXP (x, 0), 0), 
				    GET_MODE (XEXP (x, 0)), 
				    MEM_ADDR_SPACE (XEXP (x, 0)), true);
	  return true;
	}
      return false;

    /* mems for SFmode and DFmode can be inside a parallel due to
       the way the fpscr is handled.  */
    case PARALLEL:
      for (int i = 0; i < XVECLEN (x, 0); i++)
	{
	  rtx xx = XVECEXP (x, 0, i);
	  if (GET_CODE (xx) == SET && MEM_P (XEXP (xx, 0)))
	    {
	      *total = sh_address_cost (XEXP (XEXP (xx, 0), 0), 
					GET_MODE (XEXP (xx, 0)),
					MEM_ADDR_SPACE (XEXP (xx, 0)), true);
	      return true;
	    }
	  if (GET_CODE (xx) == SET && MEM_P (XEXP (xx, 1)))
	    {
	      *total = sh_address_cost (XEXP (XEXP (xx, 1), 0),
					GET_MODE (XEXP (xx, 1)),
					MEM_ADDR_SPACE (XEXP (xx, 1)), true);
	      return true;
	    }
	}

      if (sh_1el_vec (x, VOIDmode))
	*total = outer_code != SET;
      else if (sh_rep_vec (x, VOIDmode))
	*total = ((GET_MODE_UNIT_SIZE (GET_MODE (x)) + 3) / 4
		  + (outer_code != SET));
      else
	*total = COSTS_N_INSNS (3) + (outer_code != SET);
      return true;

    case CONST_INT:
      if (CONST_OK_FOR_I08 (INTVAL (x)))
        *total = 0;
      else if ((outer_code == AND || outer_code == IOR || outer_code == XOR)
	       && CONST_OK_FOR_K08 (INTVAL (x)))
        *total = 1;
      /* prepare_cmp_insn will force costly constants int registers before
	 the cbranch[sd]i4 patterns can see them, so preserve potentially
	 interesting ones not covered by I08 above.  */
      else if (outer_code == COMPARE
	       && ((unsigned HOST_WIDE_INT) INTVAL (x)
		    == (unsigned HOST_WIDE_INT) 0x7fffffff + 1
		    || INTVAL (x) == 0x7fffffff
		   || INTVAL (x) == 0x80 || INTVAL (x) == -0x81))
        *total = 1;
      else
        *total = 8;
      return true;

    case EQ:
      /* An and with a constant compared against zero is
	 most likely going to be a TST #imm, R0 instruction.  */
      if (XEXP (x, 1) == const0_rtx
          && ((GET_CODE (XEXP (x, 0)) == AND
               || (SUBREG_P (XEXP (x, 0))
		   && GET_CODE (SUBREG_REG (XEXP (x, 0))) == AND))
	      || GET_CODE (XEXP (x, 0)) == ZERO_EXTRACT))
	{
	  *total = 1;
	  return true;
	}

      else if (XEXP (x, 1) == const0_rtx
	       && GET_CODE (XEXP (x, 0)) == AND
	       && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == ASHIFT
	       && arith_reg_operand (XEXP (XEXP (XEXP (x, 0), 0), 0), SImode)
	       && CONST_INT_P (XEXP (XEXP (XEXP (x, 0), 0), 1)))
	{
	  *total = 1;
	  return true;
	}
      else
	return false;

    case SMIN:
    case SMAX:
      /* This is most likely a clips.b or clips.w insn that is being made up
	 by combine.  */
      if (TARGET_SH2A
	  && (GET_CODE (XEXP (x, 0)) == SMAX || GET_CODE (XEXP (x, 0)) == SMIN)
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && REG_P (XEXP (XEXP (x, 0), 0))
	  && CONST_INT_P (XEXP (x, 1)))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else
	return false;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = 5;
      return true;

    case CONST_DOUBLE:
      /* prepare_cmp_insn will force costly constants int registers before
	 the cbranchdi4 pattern can see them, so preserve potentially
	 interesting ones.  */
      if (outer_code == COMPARE && GET_MODE (x) == DImode)
	*total = 1;
      else
	*total = 10;
      return true;

    case CONST_VECTOR:
    /* FIXME: This looks broken.  Only the last statement has any effect.
       Probably this could be folded with the PARALLEL case?  */
      if (x == CONST0_RTX (GET_MODE (x)))
	*total = 0;
      else if (sh_1el_vec (x, VOIDmode))
	*total = outer_code != SET;
      if (sh_rep_vec (x, VOIDmode))
	*total = ((GET_MODE_UNIT_SIZE (GET_MODE (x)) + 3) / 4
		  + (outer_code != SET));
      *total = COSTS_N_INSNS (3) + (outer_code != SET);
      return true;

    case PLUS:
    case MINUS:
      *total = COSTS_N_INSNS (addsubcosts (x));
      return true;

    case AND:
      /* Check for (and (not (reg)) (const_int 1)) which is a tst insn.  */
      if (GET_CODE (XEXP (x, 0)) == NOT && XEXP (x, 1) == const1_rtx)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      /* Fall through.  */

    case XOR:
    case IOR:
      *total = COSTS_N_INSNS (and_xor_ior_costs (x, code));
      return true;

    case MULT:
      *total = COSTS_N_INSNS (multcosts (x));
      return true;

    case LT:
    case GE:
      /* div0s sign comparison.  */
      if (GET_CODE (XEXP (x, 0)) == XOR
	  && REG_P ((XEXP (XEXP (x, 0), 0)))
	  && REG_P ((XEXP (XEXP (x, 0), 1)))
	  && satisfies_constraint_Z (XEXP (x, 1)))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else
	return false;

    case LSHIFTRT:
      /* div0s sign comparison.  */
      if (GET_CODE (XEXP (x, 0)) == XOR
	  && REG_P ((XEXP (XEXP (x, 0), 0)))
	  && REG_P ((XEXP (XEXP (x, 0), 1)))
	  && CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) == 31)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      /* Fall through to shiftcosts.  */
    case ASHIFT:
    case ASHIFTRT:
      {
	int cost = shiftcosts (x);
	if (cost < 0)
	  return false;
	*total = COSTS_N_INSNS (cost);
	return true;
      }

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = COSTS_N_INSNS (20);
      return true;

    case FLOAT:
    case FIX:
      *total = 100;
      return true;

    default:
      return false;
    }
}

/* Determine the size of the fundamental move insn that will be used
   for the specified mode.  */
static inline int
mov_insn_size (machine_mode mode, bool consider_sh2a)
{
  const int mode_sz = GET_MODE_SIZE (mode);

  if ((consider_sh2a && TARGET_SH2A_DOUBLE && mode == DFmode)
      || (TARGET_FMOVD && mode == DFmode))
    return mode_sz;
  else
    {
      /* The max. available mode for actual move insns is SImode.
	 Larger accesses will be split into multiple loads/stores.  */
      const int max_mov_sz = GET_MODE_SIZE (SImode);
      return mode_sz >= max_mov_sz ? max_mov_sz : mode_sz;
    }
}

/* Determine the maximum possible displacement for a move insn for the
   specified mode.  */
int
sh_max_mov_insn_displacement (machine_mode mode, bool consider_sh2a)
{
  /* The 4 byte displacement move insns are the same as the 2 byte
     versions but take a 12 bit displacement.  All we need to do is to
     scale the max. displacement value accordingly.  */
  const int disp_scale = consider_sh2a ? (4095 / 15) : 1;

  /* SH2A supports FPU move insns with 12 bit displacements.
     Other variants to do not support any kind of displacements for
     FPU move insns.  */
  if (! consider_sh2a && TARGET_FPU_ANY && GET_MODE_CLASS (mode) == MODE_FLOAT)
    return 0;
  else
    {
      const int mov_insn_sz = mov_insn_size (mode, consider_sh2a);
      const int mode_sz = GET_MODE_SIZE (mode);
      int r = 15 * mov_insn_sz * disp_scale;
    
      /* If the mov insn will be split into multiple loads/stores, the
	 maximum possible displacement is a bit smaller.  */
      if (mode_sz > mov_insn_sz)
	r -= mode_sz - mov_insn_sz;
      return r;
    }
}

/* Determine the alignment mask for a move insn of the
   specified mode.  */
static inline int
mov_insn_alignment_mask (machine_mode mode, bool consider_sh2a)
{
  const int mov_insn_sz = mov_insn_size (mode, consider_sh2a);
  return mov_insn_sz > 0 ? (mov_insn_sz - 1) : 0;
}

/* Return the displacement value of a displacement address.  */
HOST_WIDE_INT
sh_disp_addr_displacement (rtx x)
{
  gcc_assert (satisfies_constraint_Sdd (x));
  return INTVAL (XEXP (XEXP (x, 0), 1));
}

/* Compute the cost of an address.  */
static int
sh_address_cost (rtx x, machine_mode mode,
		 addr_space_t as ATTRIBUTE_UNUSED, bool speed ATTRIBUTE_UNUSED)
{
  /* 'GBR + 0'.  Account one more because of R0 restriction.  */
  if (REG_P (x) && REGNO (x) == GBR_REG)
    return 2;

  /* Simple reg, post-inc, pre-dec addressing.  */
  if (REG_P (x) || GET_CODE (x) == POST_INC || GET_CODE (x) == PRE_DEC)
    return 1;

  /* 'reg + disp' addressing.  */
  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0)) && CONST_INT_P (XEXP (x, 1)))
    {
      /* 'GBR + disp'.  Account one more because of R0 restriction.  */
      if (REGNO (XEXP (x, 0)) == GBR_REG
	  && gbr_displacement (XEXP (x, 1), mode))
	return 2;

      const HOST_WIDE_INT offset = INTVAL (XEXP (x, 1));

      if (offset == 0)
	return 1;

      /* The displacement would fit into a 2 byte move insn.
	 HImode and QImode loads/stores with displacement put pressure on
	 R0 which will most likely require another reg copy.  Thus account
	 a higher cost for that.  */
      if (offset > 0 && offset <= sh_max_mov_insn_displacement (mode, false))
	return (mode == HImode || mode == QImode) ? 2 : 1;

      /* The displacement would fit into a 4 byte move insn (SH2A).  */
      if (TARGET_SH2A
	  && offset > 0 && offset <= sh_max_mov_insn_displacement (mode, true))
	return 2;

      /* The displacement is probably out of range and will require extra
	 calculations.  */
      return 3;
    }

  /* 'reg + reg' addressing.  Account a slightly higher cost because of 
     increased pressure on R0.  */
  if (GET_CODE (x) == PLUS && ! CONSTANT_P (XEXP (x, 1)))
    return 3;

  /* Not sure what it is - probably expensive.  */
  return 10;
}

/* Code to expand a shift.  */
static void
gen_ashift (int type, int n, rtx reg)
{
  rtx n_rtx;

  /* Negative values here come from the shift_amounts array.  */
  if (n < 0)
    {
      if (type == ASHIFT)
	type = LSHIFTRT;
      else
	type = ASHIFT;
      n = -n;
    }

  n_rtx = GEN_INT (n);
  gcc_assert (satisfies_constraint_P27 (n_rtx));

  switch (type)
    {
    case ASHIFTRT:
      emit_insn (gen_ashrsi3_k (reg, reg, n_rtx));
      break;
    case LSHIFTRT:
      if (n == 1)
	emit_insn (gen_shlr (reg, reg));
      else
	emit_insn (gen_lshrsi3_k (reg, reg, n_rtx));
      break;
    case ASHIFT:
      emit_insn (gen_ashlsi3_k (reg, reg, n_rtx));
      break;
    default:
      gcc_unreachable ();
    }
}

/* Code to expand a HImode shift.  */
static void
gen_ashift_hi (int type, int n, rtx reg)
{
  /* Negative values here come from the shift_amounts array.  */
  if (n < 0)
    {
      if (type == ASHIFT)
	type = LSHIFTRT;
      else
	type = ASHIFT;
      n = -n;
    }

  switch (type)
    {
    case ASHIFTRT:
    case LSHIFTRT:
      /* We don't have HImode right shift operations because using the
	 ordinary 32 bit shift instructions for that doesn't generate proper
	 zero/sign extension.
	 gen_ashift_hi is only called in contexts where we know that the
	 sign extension works out correctly.  */
      {
	int offset = 0;
	if (GET_CODE (reg) == SUBREG)
	  {
	    offset = SUBREG_BYTE (reg);
	    reg = SUBREG_REG (reg);
	  }
	gen_ashift (type, n, gen_rtx_SUBREG (SImode, reg, offset));
	break;
      }
    case ASHIFT:
      emit_insn (gen_ashlhi3_k (reg, reg, GEN_INT (n)));
      break;
    }
}

/* Output RTL to split a constant shift into its component SH constant
   shift instructions.  */
void
gen_shifty_op (int code, rtx *operands)
{
  int value = INTVAL (operands[2]);
  int max, i;

  /* Truncate the shift count in case it is out of bounds.  */
  value = value & 31;

  if (value == 31)
    {
      if (code == LSHIFTRT)
	{
	  emit_insn (gen_rotlsi3_1 (operands[0], operands[0]));
	  emit_insn (gen_movt (operands[0], get_t_reg_rtx ()));
	  return;
	}
      else if (code == ASHIFT)
	{
	  /* There is a two instruction sequence for 31 bit left shifts,
	     but it requires r0.  */
	  if (REG_P (operands[0]) && REGNO (operands[0]) == 0)
	    {
	      emit_insn (gen_andsi3 (operands[0], operands[0], const1_rtx));
	      emit_insn (gen_rotlsi3_31 (operands[0], operands[0]));
	      return;
	    }
	}
    }
  else if (value == 0)
    {
      /* This can happen even when optimizing, if there were subregs before
	 reload.  Don't output a nop here, as this is never optimized away;
	 use a no-op move instead.  */
      emit_insn (gen_rtx_SET (operands[0], operands[0]));
      return;
    }

  max = ashl_lshr_seq[value].insn_count;
  for (i = 0; i < max; i++)
    gen_ashift (code, ashl_lshr_seq[value].amount[i], operands[0]);
}

/* Same as gen_shifty_op, but optimized for values where the topmost bits
   don't matter.  */
void
gen_shifty_hi_op (int code, rtx *operands)
{
  int value = INTVAL (operands[2]);
  int max, i;
  void (*gen_fun) (int, int, rtx);

  /* This operation is used by and_shl for SImode values with a few
     high bits known to be cleared.  */
  value &= 31;
  if (value == 0)
    {
      emit_insn (gen_nop ());
      return;
    }

  gen_fun = GET_MODE (operands[0]) == HImode ? gen_ashift_hi : gen_ashift;
  if (code == ASHIFT)
    {
      max = ext_ashl_lshr_seq[value].insn_count;
      for (i = 0; i < max; i++)
	gen_fun (code, ext_ashl_lshr_seq[value].amount[i], operands[0]);
    }
  else
    /* When shifting right, emit the shifts in reverse order, so that
       solitary negative values come first.  */
    for (i = ext_ashl_lshr_seq[value].insn_count - 1; i >= 0; i--)
      gen_fun (code, ext_ashl_lshr_seq[value].amount[i], operands[0]);
}

/* Output RTL for an arithmetic right shift.
   ??? Rewrite to use super-optimizer sequences.  */
bool
expand_ashiftrt (rtx *operands)
{
  rtx wrk;
  char func[18];
  int value;

  if (TARGET_DYNSHIFT)
    {
      if (!CONST_INT_P (operands[2]))
	{
	  rtx count = copy_to_mode_reg (SImode, operands[2]);
	  emit_insn (gen_negsi2 (count, count));
	  emit_insn (gen_ashrsi3_d (operands[0], operands[1], count));
	  return true;
	}
      else if (ashiftrt_insns[INTVAL (operands[2]) & 31]
	       > 1 + SH_DYNAMIC_SHIFT_COST)
	{
	  rtx count
	    = force_reg (SImode, GEN_INT (- (INTVAL (operands[2]) & 31)));
	  emit_insn (gen_ashrsi3_d (operands[0], operands[1], count));
	  return true;
	}
    }
  if (!CONST_INT_P (operands[2]))
    return false;

  value = INTVAL (operands[2]) & 31;

  if (value == 31)
    {
      /* If we are called from abs expansion, arrange things so that we
	 we can use a single MT instruction that doesn't clobber the source,
	 if LICM can hoist out the load of the constant zero.  */
      if (currently_expanding_to_rtl)
	{
	  emit_insn (gen_cmpgtsi_t (force_reg (SImode, CONST0_RTX (SImode)),
				    operands[1]));
	  emit_insn (gen_mov_neg_si_t (operands[0], get_t_reg_rtx ()));
	  return true;
	}
      emit_insn (gen_ashrsi2_31 (operands[0], operands[1]));
      return true;
    }
  else if (value >= 16 && value <= 19)
    {
      wrk = gen_reg_rtx (SImode);
      emit_insn (gen_ashrsi2_16 (wrk, operands[1]));
      value -= 16;
      while (value--)
	gen_ashift (ASHIFTRT, 1, wrk);
      emit_move_insn (operands[0], wrk);
      return true;
    }
  /* Expand a short sequence inline, longer call a magic routine.  */
  else if (value <= 5)
    {
      wrk = gen_reg_rtx (SImode);
      emit_move_insn (wrk, operands[1]);
      while (value--)
	gen_ashift (ASHIFTRT, 1, wrk);
      emit_move_insn (operands[0], wrk);
      return true;
    }

  wrk = gen_reg_rtx (Pmode);

  /* Load the value into an arg reg and call a helper.  */
  emit_move_insn (gen_rtx_REG (SImode, 4), operands[1]);
  sprintf (func, "__ashiftrt_r4_%d", value);
  rtx lab = function_symbol (wrk, func, SFUNC_STATIC).lab;
  emit_insn (gen_ashrsi3_n (GEN_INT (value), wrk, lab));
  emit_move_insn (operands[0], gen_rtx_REG (SImode, 4));
  return true;
}

/* Try to find a good way to implement the combiner pattern
  [(set (match_operand:SI 0 "register_operand" "r")
        (and:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
                           (match_operand:SI 2 "const_int_operand" "n"))
                (match_operand:SI 3 "const_int_operand" "n"))) .
  LEFT_RTX is operand 2 in the above pattern, and MASK_RTX is operand 3.
  return 0 for simple right / left or left/right shift combination.
  return 1 for a combination of shifts with zero_extend.
  return 2 for a combination of shifts with an AND that needs r0.
  return 3 for a combination of shifts with an AND that needs an extra
    scratch register, when the three highmost bits of the AND mask are clear.
  return 4 for a combination of shifts with an AND that needs an extra
    scratch register, when any of the three highmost bits of the AND mask
    is set.
  If ATTRP is set, store an initial right shift width in ATTRP[0],
  and the instruction length in ATTRP[1] .  These values are not valid
  when returning 0.
  When ATTRP is set and returning 1, ATTRP[2] gets set to the index into
  shift_amounts for the last shift value that is to be used before the
  sign extend.  */
int
shl_and_kind (rtx left_rtx, rtx mask_rtx, int *attrp)
{
  unsigned HOST_WIDE_INT mask, lsb, mask2, lsb2;
  int left = INTVAL (left_rtx), right;
  int best = 0;
  int cost, best_cost = 10000;
  int best_right = 0, best_len = 0;
  int i;
  int can_ext;

  if (left < 0 || left > 31)
    return 0;
  if (CONST_INT_P (mask_rtx))
    mask = (unsigned HOST_WIDE_INT) INTVAL (mask_rtx) >> left;
  else
    mask = (unsigned HOST_WIDE_INT) GET_MODE_MASK (SImode) >> left;
  /* Can this be expressed as a right shift / left shift pair?  */
  lsb = ((mask ^ (mask - 1)) >> 1) + 1;
  right = exact_log2 (lsb);
  mask2 = ~(mask + lsb - 1);
  lsb2 = ((mask2 ^ (mask2 - 1)) >> 1) + 1;
  /* mask has no zeroes but trailing zeroes <==> ! mask2 */
  if (! mask2)
    best_cost = ashl_lshr_seq[right].insn_count
		+ ashl_lshr_seq[right + left].insn_count;
  /* mask has no trailing zeroes <==> ! right */
  else if (! right && mask2 == ~(lsb2 - 1))
    {
      int late_right = exact_log2 (lsb2);
      best_cost = ashl_lshr_seq[left + late_right].insn_count
		  + ashl_lshr_seq[late_right].insn_count;
    }
  /* Try to use zero extend.  */
  if (mask2 == ~(lsb2 - 1))
    {
      int width, first;

      for (width = 8; width <= 16; width += 8)
	{
	  /* Can we zero-extend right away?  */
	  if (lsb2 == (unsigned HOST_WIDE_INT) 1 << width)
	    {
	      cost = 1 + ext_ashl_lshr_seq[right].insn_count
		       + ext_ashl_lshr_seq[left + right].insn_count;
	      if (cost < best_cost)
		{
		  best = 1;
		  best_cost = cost;
		  best_right = right;
		  best_len = cost;
		  if (attrp)
		    attrp[2] = -1;
		}
	      continue;
	    }
	  /* ??? Could try to put zero extend into initial right shift,
	     or even shift a bit left before the right shift.  */
	  /* Determine value of first part of left shift, to get to the
	     zero extend cut-off point.  */
	  first = width - exact_log2 (lsb2) + right;
	  if (first >= 0 && right + left - first >= 0)
	    {
	      cost = ext_ashl_lshr_seq[right].insn_count
		     + ext_ashl_lshr_seq[first].insn_count + 1
		     + ext_ashl_lshr_seq[right + left - first].insn_count;

	      if (cost < best_cost)
		{
		  best = 1;
		  best_cost = cost;
		  best_right = right;
		  best_len = cost;
		  if (attrp)
		    attrp[2] = first;
		}
	    }
	}
    }
  /* Try to use r0 AND pattern */
  for (i = 0; i <= 2; i++)
    {
      if (i > right)
	break;
      if (! CONST_OK_FOR_K08 (mask >> i))
	continue;
      cost = (i != 0) + 2 + ext_ashl_lshr_seq[left + i].insn_count;
      if (cost < best_cost)
	{
	  best = 2;
	  best_cost = cost;
	  best_right = i;
	  best_len = cost - 1;
	}
    }
  /* Try to use a scratch register to hold the AND operand.  */
  can_ext = ((mask << left) & ((unsigned HOST_WIDE_INT) 3 << 30)) == 0;
  for (i = 0; i <= 2; i++)
    {
      if (i > right)
	break;
      cost = (i != 0) + (CONST_OK_FOR_I08 (mask >> i) ? 2 : 3)
	     + (can_ext
		? ext_ashl_lshr_seq
		: ashl_lshr_seq)[left + i].insn_count;
      if (cost < best_cost)
	{
	  best = 4 - can_ext;
	  best_cost = cost;
	  best_right = i;
	  best_len = cost - 1 - ! CONST_OK_FOR_I08 (mask >> i);
	}
    }

  if (attrp)
    {
      attrp[0] = best_right;
      attrp[1] = best_len;
    }
  return best;
}

/* This is used in length attributes of the unnamed instructions
   corresponding to shl_and_kind return values of 1 and 2.  */
int
shl_and_length (rtx insn)
{
  rtx set_src, left_rtx, mask_rtx;
  int attributes[3];

  set_src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
  left_rtx = XEXP (XEXP (set_src, 0), 1);
  mask_rtx = XEXP (set_src, 1);
  shl_and_kind (left_rtx, mask_rtx, attributes);
  return attributes[1];
}

/* This is used in length attribute of the and_shl_scratch instruction.  */
int
shl_and_scr_length (rtx insn)
{
  rtx set_src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
  int len = ashl_lshr_seq[INTVAL (XEXP (set_src, 1)) & 31].insn_count;
  rtx op = XEXP (set_src, 0);
  len += ashl_lshr_seq[INTVAL (XEXP (op, 1)) & 31].insn_count + 1;
  op = XEXP (XEXP (op, 0), 0);
  return len + ashl_lshr_seq[INTVAL (XEXP (op, 1)) & 31].insn_count;
}

/* Generate rtl for instructions for which shl_and_kind advised a particular
   method of generating them, i.e. returned zero.  */
bool
gen_shl_and (rtx dest, rtx left_rtx, rtx mask_rtx, rtx source)
{
  int attributes[3];
  unsigned HOST_WIDE_INT mask;
  int kind = shl_and_kind (left_rtx, mask_rtx, attributes);
  int right, total_shift;
  void (*shift_gen_fun) (int, rtx *) = gen_shifty_hi_op;

  right = attributes[0];
  total_shift = INTVAL (left_rtx) + right;
  mask = (unsigned HOST_WIDE_INT) INTVAL (mask_rtx) >> total_shift;
  switch (kind)
    {
    default:
      return true;
    case 1:
      {
	int first = attributes[2];
	rtx operands[3];

	if (first < 0)
	  {
	    emit_insn ((mask << right) <= 0xff
		       ? gen_zero_extendqisi2 (dest,
					       gen_lowpart (QImode, source))
		       : gen_zero_extendhisi2 (dest,
					       gen_lowpart (HImode, source)));
	    source = dest;
	  }
	if (source != dest)
	  emit_insn (gen_movsi (dest, source));
	operands[0] = dest;
	if (right)
	  {
	    operands[2] = GEN_INT (right);
	    gen_shifty_hi_op (LSHIFTRT, operands);
	  }
	if (first > 0)
	  {
	    operands[2] = GEN_INT (first);
	    gen_shifty_hi_op (ASHIFT, operands);
	    total_shift -= first;
	    mask <<= first;
	  }
	if (first >= 0)
	  emit_insn (mask <= 0xff
		     ? gen_zero_extendqisi2 (dest, gen_lowpart (QImode, dest))
		     : gen_zero_extendhisi2 (dest, gen_lowpart (HImode, dest)));
	if (total_shift > 0)
	  {
	    operands[2] = GEN_INT (total_shift);
	    gen_shifty_hi_op (ASHIFT, operands);
	  }
	break;
      }
    case 4:
      shift_gen_fun = gen_shifty_op;
    case 3:
      /* If the topmost bit that matters is set, set the topmost bits
	 that don't matter.  This way, we might be able to get a shorter
	 signed constant.  */
      if (mask & ((HOST_WIDE_INT) 1 << (31 - total_shift)))
	mask |= (HOST_WIDE_INT) ((HOST_WIDE_INT_M1U) << (31 - total_shift));
    case 2:
      /* Don't expand fine-grained when combining, because that will
         make the pattern fail.  */
      if (currently_expanding_to_rtl
	  || reload_in_progress || reload_completed)
	{
	  rtx operands[3];

	  /* Cases 3 and 4 should be handled by this split
	     only while combining  */
	  gcc_assert (kind <= 2);
	  if (right)
	    {
	      emit_insn (gen_lshrsi3 (dest, source, GEN_INT (right)));
	      source = dest;
	    }
	  emit_insn (gen_andsi3 (dest, source, GEN_INT (mask)));
	  if (total_shift)
	    {
	      operands[0] = dest;
	      operands[1] = dest;
	      operands[2] = GEN_INT (total_shift);
	      shift_gen_fun (ASHIFT, operands);
	    }
	  break;
	}
      else
	{
	  int neg = 0;
	  if (kind != 4 && total_shift < 16)
	    {
	      neg = -ext_ashl_lshr_seq[total_shift].amount[1];
	      if (neg > 0)
		neg -= ext_ashl_lshr_seq[total_shift].amount[2];
	      else
		neg = 0;
	    }
	  emit_insn (gen_and_shl_scratch (dest, source,
					  GEN_INT (right),
					  GEN_INT (mask),
					  GEN_INT (total_shift + neg),
					  GEN_INT (neg)));
	  emit_insn (gen_movsi (dest, dest));
	  break;
	}
    }
  return false;
}

/* Try to find a good way to implement the combiner pattern
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extract:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
                                    (match_operand:SI 2 "const_int_operand" "n")
                         (match_operand:SI 3 "const_int_operand" "n")
                         (const_int 0)))
   (clobber (reg:SI T_REG))]
  LEFT_RTX is operand 2 in the above pattern, and SIZE_RTX is operand 3.
  return 0 for simple left / right shift combination.
  return 1 for left shift / 8 bit sign extend / left shift.
  return 2 for left shift / 16 bit sign extend / left shift.
  return 3 for left shift / 8 bit sign extend / shift / sign extend.
  return 4 for left shift / 16 bit sign extend / shift / sign extend.
  return 5 for left shift / 16 bit sign extend / right shift
  return 6 for < 8 bit sign extend / left shift.
  return 7 for < 8 bit sign extend / left shift / single right shift.
  If COSTP is nonzero, assign the calculated cost to *COSTP.  */
int
shl_sext_kind (rtx left_rtx, rtx size_rtx, int *costp)
{
  int left, size, insize, ext;
  int cost = 0, best_cost;
  int kind;

  left = INTVAL (left_rtx);
  size = INTVAL (size_rtx);
  insize = size - left;
  gcc_assert (insize > 0);
  /* Default to left / right shift.  */
  kind = 0;
  best_cost = ashl_lshr_seq[32 - insize].insn_count
	      + ashl_lshr_seq[32 - size].insn_count;
  if (size <= 16)
    {
      /* 16 bit shift / sign extend / 16 bit shift */
      cost = ashl_lshr_seq[16 - insize].insn_count + 1
	     + ashl_lshr_seq[16 - size].insn_count;
      /* If ashiftrt_insns[16 - size] is 8, this choice will be overridden
	 below, by alternative 3 or something even better.  */
      if (cost < best_cost)
	{
	  kind = 5;
	  best_cost = cost;
	}
    }
  /* Try a plain sign extend between two shifts.  */
  for (ext = 16; ext >= insize; ext -= 8)
    {
      if (ext <= size)
	{
	  cost = ext_ashl_lshr_seq[ext - insize].insn_count + 1
		 + ashl_lshr_seq[size - ext].insn_count;
	  if (cost < best_cost)
	    {
	      kind = ext / (unsigned) 8;
	      best_cost = cost;
	    }
	}
      /* Check if we can do a sloppy shift with a final signed shift
	 restoring the sign.  */
      if (EXT_SHIFT_SIGNED (size - ext))
	cost = ext_ashl_lshr_seq[ext - insize].insn_count
	       + ext_ashl_lshr_seq[size - ext].insn_count + 1;
      /* If not, maybe it's still cheaper to do the second shift sloppy,
	 and do a final sign extend?  */
      else if (size <= 16)
	cost = ext_ashl_lshr_seq[ext - insize].insn_count + 1
	  + ext_ashl_lshr_seq[size > ext ? size - ext : ext - size].insn_count
	  + 1;
      else
	continue;
      if (cost < best_cost)
	{
	  kind = ext / (unsigned) 8 + 2;
	  best_cost = cost;
	}
    }
  /* Check if we can sign extend in r0 */
  if (insize < 8)
    {
      cost = 3 + ashl_lshr_seq[left].insn_count;
      if (cost < best_cost)
	{
	  kind = 6;
	  best_cost = cost;
	}
      /* Try the same with a final signed shift.  */
      if (left < 31)
	{
	  cost = 3 + ext_ashl_lshr_seq[left + 1].insn_count + 1;
	  if (cost < best_cost)
	    {
	      kind = 7;
	      best_cost = cost;
	    }
	}
    }
  if (TARGET_DYNSHIFT)
    {
      /* Try to use a dynamic shift.  */
      cost = ashl_lshr_seq[32 - insize].insn_count + 1 + SH_DYNAMIC_SHIFT_COST;
      if (cost < best_cost)
	{
	  kind = 0;
	  best_cost = cost;
	}
    }
  if (costp)
    *costp = cost;
  return kind;
}

/* Function to be used in the length attribute of the instructions
   implementing this pattern.  */
int
shl_sext_length (rtx insn)
{
  rtx set_src, left_rtx, size_rtx;
  int cost;

  set_src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
  left_rtx = XEXP (XEXP (set_src, 0), 1);
  size_rtx = XEXP (set_src, 1);
  shl_sext_kind (left_rtx, size_rtx, &cost);
  return cost;
}

/* Generate rtl for this pattern */
bool
gen_shl_sext (rtx dest, rtx left_rtx, rtx size_rtx, rtx source)
{
  int kind;
  int left, size, insize, cost;
  rtx operands[3];

  kind = shl_sext_kind (left_rtx, size_rtx, &cost);
  left = INTVAL (left_rtx);
  size = INTVAL (size_rtx);
  insize = size - left;
  switch (kind)
    {
    case 1:
    case 2:
    case 3:
    case 4:
      {
	int ext = kind & 1 ? 8 : 16;
	int shift2 = size - ext;

	/* Don't expand fine-grained when combining, because that will
	   make the pattern fail.  */
	if (! currently_expanding_to_rtl
	    && ! reload_in_progress && ! reload_completed)
	  {
	    emit_insn (gen_shl_sext_ext (dest, source, left_rtx, size_rtx));
	    emit_insn (gen_movsi (dest, source));
	    break;
	  }
	if (dest != source)
	  emit_insn (gen_movsi (dest, source));
	operands[0] = dest;
	if (ext - insize)
	  {
	    operands[2] = GEN_INT (ext - insize);
	    gen_shifty_hi_op (ASHIFT, operands);
	  }
	emit_insn (kind & 1
		   ? gen_extendqisi2 (dest, gen_lowpart (QImode, dest))
		   : gen_extendhisi2 (dest, gen_lowpart (HImode, dest)));
	if (kind <= 2)
	  {
	    if (shift2)
	      {
		operands[2] = GEN_INT (shift2);
		gen_shifty_op (ASHIFT, operands);
	      }
	  }
	else
	  {
	    if (shift2 > 0)
	      {
		if (EXT_SHIFT_SIGNED (shift2))
		  {
		    operands[2] = GEN_INT (shift2 + 1);
		    gen_shifty_op (ASHIFT, operands);
		    operands[2] = const1_rtx;
		    gen_shifty_op (ASHIFTRT, operands);
		    break;
		  }
		operands[2] = GEN_INT (shift2);
		gen_shifty_hi_op (ASHIFT, operands);
	      }
	    else if (shift2)
	      {
		operands[2] = GEN_INT (-shift2);
		gen_shifty_hi_op (LSHIFTRT, operands);
	      }
	    emit_insn (size <= 8
		       ? gen_extendqisi2 (dest, gen_lowpart (QImode, dest))
		       : gen_extendhisi2 (dest, gen_lowpart (HImode, dest)));
	  }
	break;
      }
    case 5:
      {
	int i = 16 - size;
	if (! currently_expanding_to_rtl
	    && ! reload_in_progress && ! reload_completed)
	  emit_insn (gen_shl_sext_ext (dest, source, left_rtx, size_rtx));
	else
	  {
	    operands[0] = dest;
	    operands[2] = GEN_INT (16 - insize);
	    gen_shifty_hi_op (ASHIFT, operands);
	    emit_insn (gen_extendhisi2 (dest, gen_lowpart (HImode, dest)));
	  }
	/* Don't use gen_ashrsi3 because it generates new pseudos.  */
	while (--i >= 0)
	  gen_ashift (ASHIFTRT, 1, dest);
	break;
      }
    case 6:
    case 7:
      /* Don't expand fine-grained when combining, because that will
	 make the pattern fail.  */
      if (! currently_expanding_to_rtl
	  && ! reload_in_progress && ! reload_completed)
	{
	  emit_insn (gen_shl_sext_ext (dest, source, left_rtx, size_rtx));
	  emit_insn (gen_movsi (dest, source));
	  break;
	}
      emit_insn (gen_andsi3 (dest, source, GEN_INT ((1 << insize) - 1)));
      emit_insn (gen_xorsi3 (dest, dest, GEN_INT (1 << (insize - 1))));
      emit_insn (gen_addsi3 (dest, dest, GEN_INT (HOST_WIDE_INT_M1U << (insize - 1))));
      operands[0] = dest;
      operands[2] = kind == 7 ? GEN_INT (left + 1) : left_rtx;
      gen_shifty_op (ASHIFT, operands);
      if (kind == 7)
	emit_insn (gen_ashrsi3_k (dest, dest, const1_rtx));
      break;
    default:
      return true;
    }
  return false;
}

typedef struct label_ref_list_d
{
  rtx_code_label *label;
  struct label_ref_list_d *next;
} *label_ref_list_t;

static object_allocator<label_ref_list_d> label_ref_list_d_pool
  ("label references list");

/* The SH cannot load a large constant into a register, constants have to
   come from a pc relative load.  The reference of a pc relative load
   instruction must be less than 1k in front of the instruction.  This
   means that we often have to dump a constant inside a function, and
   generate code to branch around it.

   It is important to minimize this, since the branches will slow things
   down and make things bigger.

   Worst case code looks like:

   mov.l L1,rn
   bra   L2
   nop
   align
   L1:   .long value
   L2:
   ..

   mov.l L3,rn
   bra   L4
   nop
   align
   L3:   .long value
   L4:
   ..

   We fix this by performing a scan before scheduling, which notices which
   instructions need to have their operands fetched from the constant table
   and builds the table.

   The algorithm is:

   scan, find an instruction which needs a pcrel move.  Look forward, find the
   last barrier which is within MAX_COUNT bytes of the requirement.
   If there isn't one, make one.  Process all the instructions between
   the find and the barrier.

   In the above example, we can tell that L3 is within 1k of L1, so
   the first move can be shrunk from the 3 insn+constant sequence into
   just 1 insn, and the constant moved to L3 to make:

   mov.l        L1,rn
   ..
   mov.l        L3,rn
   bra          L4
   nop
   align
   L3:.long value
   L4:.long value

   Then the second move becomes the target for the shortening process.  */

typedef struct
{
  rtx value;			/* Value in table.  */
  rtx_code_label *label;	/* Label of value.  */
  label_ref_list_t wend;	/* End of window.  */
  machine_mode mode;	/* Mode of value.  */

  /* True if this constant is accessed as part of a post-increment
     sequence.  Note that HImode constants are never accessed in this way.  */
  bool part_of_sequence_p;
} pool_node;

/* The maximum number of constants that can fit into one pool, since
   constants in the range 0..510 are at least 2 bytes long, and in the
   range from there to 1018 at least 4 bytes.  */

#define MAX_POOL_SIZE 372
static pool_node pool_vector[MAX_POOL_SIZE];
static int pool_size;
static rtx_code_label *pool_window_label;
static int pool_window_last;

static int max_labelno_before_reorg;

/* ??? If we need a constant in HImode which is the truncated value of a
   constant we need in SImode, we could combine the two entries thus saving
   two bytes.  Is this common enough to be worth the effort of implementing
   it?  */

/* ??? This stuff should be done at the same time that we shorten branches.
   As it is now, we must assume that all branches are the maximum size, and
   this causes us to almost always output constant pools sooner than
   necessary.  */

/* Add a constant to the pool and return its label.  */
static rtx_code_label *
add_constant (rtx x, machine_mode mode, rtx last_value)
{
  rtx_code_label *lab, *new_rtx;
  label_ref_list_t ref, newref;

  /* First see if we've already got it.  */
  for (int i = 0; i < pool_size; i++)
    {
      if (x->code == pool_vector[i].value->code
	  && mode == pool_vector[i].mode)
	{
	  if (x->code == CODE_LABEL)
	    {
	      if (XINT (x, 3) != XINT (pool_vector[i].value, 3))
		continue;
	    }
	  if (rtx_equal_p (x, pool_vector[i].value))
	    {
	      lab = new_rtx = 0;
	      if (! last_value
		  || ! i
		  || ! rtx_equal_p (last_value, pool_vector[i-1].value))
		{
		  new_rtx = gen_label_rtx ();
		  LABEL_REFS (new_rtx) = pool_vector[i].label;
		  pool_vector[i].label = lab = new_rtx;
		}
	      if (lab && pool_window_label)
		{
		  newref = label_ref_list_d_pool.allocate ();
		  newref->label = pool_window_label;
		  ref = pool_vector[pool_window_last].wend;
		  newref->next = ref;
		  pool_vector[pool_window_last].wend = newref;
		}
	      if (new_rtx)
		pool_window_label = new_rtx;
	      pool_window_last = i;
	      return lab;
	    }
	}
    }

  /* Need a new one.  */
  pool_vector[pool_size].value = x;
  if (last_value && rtx_equal_p (last_value, pool_vector[pool_size - 1].value))
    {
      lab = 0;
      pool_vector[pool_size - 1].part_of_sequence_p = true;
    }
  else
    lab = gen_label_rtx ();
  pool_vector[pool_size].mode = mode;
  pool_vector[pool_size].label = lab;
  pool_vector[pool_size].wend = NULL;
  pool_vector[pool_size].part_of_sequence_p = (lab == 0);
  if (lab && pool_window_label)
    {
      newref = label_ref_list_d_pool.allocate ();
      newref->label = pool_window_label;
      ref = pool_vector[pool_window_last].wend;
      newref->next = ref;
      pool_vector[pool_window_last].wend = newref;
    }
  if (lab)
    pool_window_label = lab;
  pool_window_last = pool_size;
  pool_size++;
  return lab;
}

/* Output the literal table.  START, if nonzero, is the first instruction
   this table is needed for, and also indicates that there is at least one
   casesi_worker_2 instruction; We have to emit the operand3 labels from
   these insns at a 4-byte  aligned position.  BARRIER is the barrier
   after which we are to place the table.  */
static void
dump_table (rtx_insn *start, rtx_insn *barrier)
{
  rtx_insn *scan = barrier;
  bool need_align = true;
  rtx lab;
  label_ref_list_t ref;
  bool have_df = false;

  /* Do two passes, first time dump out the HI sized constants.  */

  for (int i = 0; i < pool_size; i++)
    {
      pool_node *p = &pool_vector[i];

      if (p->mode == HImode)
	{
	  if (need_align)
	    {
	      scan = emit_insn_after (gen_align_2 (), scan);
	      need_align = false;
	    }
	  for (lab = p->label; lab; lab = LABEL_REFS (lab))
	    scan = emit_label_after (lab, scan);
	  scan = emit_insn_after (gen_consttable_2 (p->value, const0_rtx),
				  scan);
	  for (ref = p->wend; ref; ref = ref->next)
	    {
	      lab = ref->label;
	      scan = emit_insn_after (gen_consttable_window_end (lab), scan);
	    }
	}
      else if (p->mode == DFmode)
	have_df = true;
    }

  need_align = true;

  if (start)
    {
      scan = emit_insn_after (gen_align_4 (), scan);
      need_align = false;
      for (; start != barrier; start = NEXT_INSN (start))
	if (NONJUMP_INSN_P (start)
	    && recog_memoized (start) == CODE_FOR_casesi_worker_2)
	  {
	    rtx src = SET_SRC (XVECEXP (PATTERN (start), 0, 0));
	    rtx lab = XEXP (XVECEXP (src, 0, 3), 0);

	    scan = emit_label_after (lab, scan);
	  }
    }
  if (TARGET_FMOVD && TARGET_ALIGN_DOUBLE && have_df)
    {
      rtx_insn *align_insn = NULL;

      scan = emit_label_after (gen_label_rtx (), scan);
      scan = emit_insn_after (gen_align_log (GEN_INT (3)), scan);
      need_align = false;

      for (int i = 0; i < pool_size; i++)
	{
	  pool_node *p = &pool_vector[i];

	  switch (p->mode)
	    {
	    case HImode:
	      break;
	    case SImode:
	    case SFmode:
	      if (align_insn && !p->part_of_sequence_p)
		{
		  for (lab = p->label; lab; lab = LABEL_REFS (lab))
		    emit_label_before (lab, align_insn);
		  emit_insn_before (gen_consttable_4 (p->value, const0_rtx),
				    align_insn);
		  for (ref = p->wend; ref; ref = ref->next)
		    {
		      lab = ref->label;
		      emit_insn_before (gen_consttable_window_end (lab),
					align_insn);
		    }
		  delete_insn (align_insn);
		  align_insn = NULL;
		  continue;
		}
	      else
		{
		  for (lab = p->label; lab; lab = LABEL_REFS (lab))
		    scan = emit_label_after (lab, scan);
		  scan = emit_insn_after (gen_consttable_4 (p->value,
							    const0_rtx), scan);
		  need_align = ! need_align;
		}
	      break;
	    case DFmode:
	      if (need_align)
		{
		  scan = emit_insn_after (gen_align_log (GEN_INT (3)), scan);
		  align_insn = scan;
		  need_align = false;
		}
	    case DImode:
	      for (lab = p->label; lab; lab = LABEL_REFS (lab))
		scan = emit_label_after (lab, scan);
	      scan = emit_insn_after (gen_consttable_8 (p->value, const0_rtx),
				      scan);
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  if (p->mode != HImode)
	    {
	      for (ref = p->wend; ref; ref = ref->next)
		{
		  lab = ref->label;
		  scan = emit_insn_after (gen_consttable_window_end (lab),
					  scan);
		}
	    }
	}

      pool_size = 0;
    }

  for (int i = 0; i < pool_size; i++)
    {
      pool_node *p = &pool_vector[i];

      switch (p->mode)
	{
	case HImode:
	  break;
	case SImode:
	case SFmode:
	  if (need_align)
	    {
	      need_align = false;
	      scan = emit_label_after (gen_label_rtx (), scan);
	      scan = emit_insn_after (gen_align_4 (), scan);
	    }
	  for (lab = p->label; lab; lab = LABEL_REFS (lab))
	    scan = emit_label_after (lab, scan);
	  scan = emit_insn_after (gen_consttable_4 (p->value, const0_rtx),
				  scan);
	  break;
	case DFmode:
	case DImode:
	  if (need_align)
	    {
	      need_align = false;
	      scan = emit_label_after (gen_label_rtx (), scan);
	      scan = emit_insn_after (gen_align_4 (), scan);
	    }
	  for (lab = p->label; lab; lab = LABEL_REFS (lab))
	    scan = emit_label_after (lab, scan);
	  scan = emit_insn_after (gen_consttable_8 (p->value, const0_rtx),
				  scan);
	  break;
	default:
	  gcc_unreachable ();
	}

      if (p->mode != HImode)
	{
	  for (ref = p->wend; ref; ref = ref->next)
	    {
	      lab = ref->label;
	      scan = emit_insn_after (gen_consttable_window_end (lab), scan);
	    }
	}
    }

  scan = emit_insn_after (gen_consttable_end (), scan);
  scan = emit_barrier_after (scan);
  pool_size = 0;
  pool_window_label = NULL;
  pool_window_last = 0;
}

#define MOVA_LABELREF(mova) XVECEXP (SET_SRC (PATTERN (mova)), 0, 0)

/* Nonzero if the insn is a move instruction which needs to be fixed.  */

/* ??? For a DImode/DFmode moves, we don't need to fix it if each half of the
   CONST_DOUBLE input value is CONST_OK_FOR_I08.  For a SFmode move, we don't
   need to fix it if the input value is CONST_OK_FOR_I08.  */
static bool
broken_move (rtx_insn *insn)
{
  if (NONJUMP_INSN_P (insn))
    {
      rtx pat = PATTERN (insn);
      if (GET_CODE (pat) == PARALLEL)
	pat = XVECEXP (pat, 0, 0);
      if (GET_CODE (pat) == SET
	  /* We can load any 8-bit value if we don't care what the high
	     order bits end up as.  */
	  && GET_MODE (SET_DEST (pat)) != QImode
	  && (CONSTANT_P (SET_SRC (pat))
	      || (GET_CODE (SET_SRC (pat)) == UNSPEC_VOLATILE
		  && XINT (SET_SRC (pat), 1) ==  UNSPECV_SP_SWITCH_B)
	      /* Match mova_const.  */
	      || (GET_CODE (SET_SRC (pat)) == UNSPEC
		  && XINT (SET_SRC (pat), 1) == UNSPEC_MOVA
		  && GET_CODE (XVECEXP (SET_SRC (pat), 0, 0)) == CONST))
	  && ! (TARGET_SH2E
		&& GET_CODE (SET_SRC (pat)) == CONST_DOUBLE
		&& (fp_zero_operand (SET_SRC (pat))
		    || fp_one_operand (SET_SRC (pat)))
		/* In general we don't know the current setting of fpscr, so
		   disable fldi.
		   There is an exception if this was a register-register move
		   before reload - and hence it was ascertained that we have
		   single precision setting - and in a post-reload optimization
		   we changed this to do a constant load.  In that case
		   we don't have an r0 clobber, hence we must use fldi.  */
		&& (TARGET_FMOVD
		    || (GET_CODE (XEXP (XVECEXP (PATTERN (insn), 0, 2), 0))
			== SCRATCH))
		&& REG_P (SET_DEST (pat))
		&& FP_REGISTER_P (REGNO (SET_DEST (pat))))
	  && ! (TARGET_SH2A
		&& GET_MODE (SET_DEST (pat)) == SImode
		&& (satisfies_constraint_I20 (SET_SRC (pat))
		   || satisfies_constraint_I28 (SET_SRC (pat))))
	  && ! satisfies_constraint_I08 (SET_SRC (pat)))
	return true;
    }

  return false;
}

/* Return true if the specified insn is a mova insn.  */
static bool
mova_p (rtx_insn *insn)
{
  return (NONJUMP_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) == SET
	  && GET_CODE (SET_SRC (PATTERN (insn))) == UNSPEC
	  && XINT (SET_SRC (PATTERN (insn)), 1) == UNSPEC_MOVA
	  /* Don't match mova_const.  */
	  && GET_CODE (MOVA_LABELREF (insn)) == LABEL_REF);
}

/* Fix up a mova from a switch that went out of range.  */
static void
fixup_mova (rtx_insn *mova)
{
  PUT_MODE (XEXP (MOVA_LABELREF (mova), 0), QImode);
  if (! flag_pic)
    {
      SET_SRC (PATTERN (mova)) = MOVA_LABELREF (mova);
      INSN_CODE (mova) = -1;
    }
  else
    {
      rtx_insn *worker = mova;
      rtx_code_label *lab = gen_label_rtx ();
      rtx wpat, wpat0, wpat1, wsrc, target, base, diff;

      do
	{
	  worker = NEXT_INSN (worker);
	  gcc_assert (worker
		      && !LABEL_P (worker)
		      && !JUMP_P (worker));
	} while (NOTE_P (worker)
		 || recog_memoized (worker) != CODE_FOR_casesi_worker_1);
      wpat = PATTERN (worker);
      wpat0 = XVECEXP (wpat, 0, 0);
      wpat1 = XVECEXP (wpat, 0, 1);
      wsrc = SET_SRC (wpat0);
      PATTERN (worker) = (gen_casesi_worker_2
			  (SET_DEST (wpat0), XVECEXP (wsrc, 0, 1),
			   XEXP (XVECEXP (wsrc, 0, 2), 0), lab,
			   XEXP (wpat1, 0)));
      INSN_CODE (worker) = -1;
      target = XVECEXP (SET_SRC (PATTERN (mova)), 0, 0);
      base = gen_rtx_LABEL_REF (Pmode, lab);
      diff = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, target, base), UNSPEC_SYMOFF);
      SET_SRC (PATTERN (mova)) = gen_rtx_CONST (Pmode, diff);
      INSN_CODE (mova) = -1;
    }
}

/* NEW_MOVA is a mova we've just encountered while scanning forward.  Update
   *num_mova, and check if the new mova is not nested within the first one.
   return 0 if *first_mova was replaced, 1 if new_mova was replaced,
   2 if new_mova has been assigned to *first_mova, -1 otherwise..  */
static int
untangle_mova (int *num_mova, rtx_insn **first_mova, rtx_insn *new_mova)
{
  int n_addr = 0; /* Initialization to shut up spurious warning.  */
  int f_target, n_target = 0; /* Likewise.  */

  if (optimize)
    {
      /* If NEW_MOVA has no address yet, it will be handled later.  */
      if (INSN_ADDRESSES_SIZE() <= (unsigned) INSN_UID (new_mova))
	return -1;

      n_addr = INSN_ADDRESSES (INSN_UID (new_mova));
      n_target = INSN_ADDRESSES (INSN_UID (XEXP (MOVA_LABELREF (new_mova), 0)));
      if (n_addr > n_target || n_addr + 1022 < n_target)
	{
	  /* Change the mova into a load.
	     broken_move will then return true for it.  */
	  fixup_mova (new_mova);
	  return 1;
	}
    }
  if (!(*num_mova)++)
    {
      *first_mova = new_mova;
      return 2;
    }
  if (!optimize
      || ((f_target
	   = INSN_ADDRESSES (INSN_UID (XEXP (MOVA_LABELREF (*first_mova), 0))))
	  >= n_target))
    return -1;

  (*num_mova)--;
  if (f_target - INSN_ADDRESSES (INSN_UID (*first_mova))
      > n_target - n_addr)
    {
      fixup_mova (*first_mova);
      return 0;
    }
  else
    {
      fixup_mova (new_mova);
      return 1;
    }
}

/* Find the last barrier from insn FROM which is close enough to hold the
   constant pool.  If we can't find one, then create one near the end of
   the range.  */
static rtx_insn *
find_barrier (int num_mova, rtx_insn *mova, rtx_insn *from)
{
  int count_si = 0;
  int count_hi = 0;
  int found_hi = 0;
  int found_si = 0;
  int hi_align = 2;
  int si_align = 2;
  int leading_mova = num_mova;
  rtx_insn *barrier_before_mova = NULL;
  rtx_insn *found_barrier = NULL;
  rtx_insn *good_barrier = NULL;
  int si_limit;
  int hi_limit;
  rtx_insn *orig = from;
  rtx_insn *last_got = NULL;
  rtx_insn *last_symoff = NULL;

  /* For HImode: range is 510, add 4 because pc counts from address of
     second instruction after this one, subtract 2 for the jump instruction
     that we may need to emit before the table, subtract 2 for the instruction
     that fills the jump delay slot (in very rare cases, reorg will take an
     instruction from after the constant pool or will leave the delay slot
     empty).  This gives 510.
     For SImode: range is 1020, add 4 because pc counts from address of
     second instruction after this one, subtract 2 in case pc is 2 byte
     aligned, subtract 2 for the jump instruction that we may need to emit
     before the table, subtract 2 for the instruction that fills the jump
     delay slot.  This gives 1018.  */

  /* The branch will always be shortened now that the reference address for
     forward branches is the successor address, thus we need no longer make
     adjustments to the [sh]i_limit for -O0.  */

  si_limit = 1018;
  hi_limit = 510;

  while (from && count_si < si_limit && count_hi < hi_limit)
    {
      int inc = get_attr_length (from);
      int new_align = 1;

      /* If this is a label that existed at the time of the compute_alignments
	 call, determine the alignment.  N.B.  When find_barrier recurses for
	 an out-of-reach mova, we might see labels at the start of previously
	 inserted constant tables.  */
      if (LABEL_P (from)
	  && CODE_LABEL_NUMBER (from) <= max_labelno_before_reorg)
	{
	  if (optimize)
	    new_align = 1 << label_to_alignment (from);
	  else if (BARRIER_P (prev_nonnote_insn (from)))
	    new_align = 1 << barrier_align (from);
	  else
	    new_align = 1;
	  inc = 0;
	}
      /* In case we are scanning a constant table because of recursion, check
	 for explicit alignments.  If the table is long, we might be forced
	 to emit the new table in front of it; the length of the alignment
	 might be the last straw.  */
      else if (NONJUMP_INSN_P (from)
	       && GET_CODE (PATTERN (from)) == UNSPEC_VOLATILE
	       && XINT (PATTERN (from), 1) == UNSPECV_ALIGN)
	new_align = INTVAL (XVECEXP (PATTERN (from), 0, 0));
      /* When we find the end of a constant table, paste the new constant
	 at the end.  That is better than putting it in front because
	 this way, we don't need extra alignment for adding a 4-byte-aligned
	 mov(a) label to a 2/4 or 8/4 byte aligned table.  */
      else if (NONJUMP_INSN_P (from)
	       && GET_CODE (PATTERN (from)) == UNSPEC_VOLATILE
	       && XINT (PATTERN (from), 1) == UNSPECV_CONST_END)
	return from;

      if (BARRIER_P (from))
	{
	  rtx_insn *next;

	  found_barrier = from;

	  /* If we are at the end of the function, or in front of an alignment
	     instruction, we need not insert an extra alignment.  We prefer
	     this kind of barrier.  */
	  if (barrier_align (from) > 2)
	    good_barrier = from;

	  /* If we are at the end of a hot/cold block, dump the constants
	     here.  */
	  next = NEXT_INSN (from);
	  if (next
	      && NOTE_P (next)
	      && NOTE_KIND (next) == NOTE_INSN_SWITCH_TEXT_SECTIONS)
	    break;
	}

      if (broken_move (from))
	{
	  rtx pat, src, dst;
	  machine_mode mode;

	  pat = PATTERN (from);
	  if (GET_CODE (pat) == PARALLEL)
	    pat = XVECEXP (pat, 0, 0);
	  src = SET_SRC (pat);
	  dst = SET_DEST (pat);
	  mode = GET_MODE (dst);

	  /* GOT pcrelat setting comes in pair of
	     mova	.L8,r0
	     mov.l	.L8,r12
	     instructions.  (plus add r0,r12).
	     Remember if we see one without the other.  */
	  if (GET_CODE (src) == UNSPEC && PIC_ADDR_P (XVECEXP (src, 0, 0)))
	    last_got = last_got ? NULL : from;
	  else if (PIC_ADDR_P (src))
	    last_got = last_got ? NULL : from;

	  /* We must explicitly check the mode, because sometimes the
	     front end will generate code to load unsigned constants into
	     HImode targets without properly sign extending them.  */
	  if (mode == HImode
	      || (mode == SImode && satisfies_constraint_I16 (src)
		  && REGNO (dst) != FPUL_REG))
	    {
	      found_hi += 2;
	      /* We put the short constants before the long constants, so
		 we must count the length of short constants in the range
		 for the long constants.  */
	      /* ??? This isn't optimal, but is easy to do.  */
	      si_limit -= 2;
	    }
	  else
	    {
	      /* We dump DF/DI constants before SF/SI ones, because
		 the limit is the same, but the alignment requirements
		 are higher.  We may waste up to 4 additional bytes
		 for alignment, and the DF/DI constant may have
		 another SF/SI constant placed before it.  */
	      while (si_align > 2 && found_si + si_align - 2 > count_si)
		si_align >>= 1;
	      if (found_si > count_si)
		count_si = found_si;
	      found_si += GET_MODE_SIZE (mode);
	      if (num_mova)
		si_limit -= GET_MODE_SIZE (mode);
	    }
	}

      if (mova_p (from))
	{
	  switch (untangle_mova (&num_mova, &mova, from))
	    {
	      case 1:
		if (flag_pic)
		  {
		    rtx src = SET_SRC (PATTERN (from));
		    if (GET_CODE (src) == CONST
			&& GET_CODE (XEXP (src, 0)) == UNSPEC
			&& XINT (XEXP (src, 0), 1) == UNSPEC_SYMOFF)
		      last_symoff = from;
		  }
		break;
	      case 0:	return find_barrier (0, 0, mova);
	      case 2:
		{
		  leading_mova = 0;
		  barrier_before_mova
		    = good_barrier ? good_barrier : found_barrier;
		}
	      default:	break;
	    }
	  if (found_si > count_si)
	    count_si = found_si;
	}
      else if (JUMP_TABLE_DATA_P (from)
	       && GET_CODE (PATTERN (from)) == ADDR_DIFF_VEC)
	{
	  if ((num_mova > 1 && GET_MODE (prev_nonnote_insn (from)) == VOIDmode)
	      || (num_mova
		  && (prev_nonnote_insn (from)
		      == XEXP (MOVA_LABELREF (mova), 0))))
	    num_mova--;
	  if (barrier_align (next_real_insn (from)) == align_jumps_log)
	    {
	      /* We have just passed the barrier in front of the
		 ADDR_DIFF_VEC, which is stored in found_barrier.  Since
		 the ADDR_DIFF_VEC is accessed as data, just like our pool
		 constants, this is a good opportunity to accommodate what
		 we have gathered so far.
		 If we waited any longer, we could end up at a barrier in
		 front of code, which gives worse cache usage for separated
		 instruction / data caches.  */
	      good_barrier = found_barrier;
	      break;
	    }
	  else
	    {
	      rtx body = PATTERN (from);
	      inc = XVECLEN (body, 1) * GET_MODE_SIZE (GET_MODE (body));
	    }
	}
      /* For the SH1, we generate alignments even after jumps-around-jumps.  */
      else if (JUMP_P (from)
	       && ! TARGET_SH2
	       && ! optimize_size)
	new_align = 4;

      /* There is a possibility that a bf is transformed into a bf/s by the
	 delay slot scheduler.  */
      if (JUMP_P (from)
	  && get_attr_type (from) == TYPE_CBRANCH
	  && ! sequence_insn_p (from))
	inc += 2;

      if (found_si)
	{
	  count_si += inc;
	  if (new_align > si_align)
	    {
	      si_limit -= (count_si - 1) & (new_align - si_align);
	      si_align = new_align;
	    }
	  count_si = (count_si + new_align - 1) & -new_align;
	}
      if (found_hi)
	{
	  count_hi += inc;
	  if (new_align > hi_align)
	    {
	      hi_limit -= (count_hi - 1) & (new_align - hi_align);
	      hi_align = new_align;
	    }
	  count_hi = (count_hi + new_align - 1) & -new_align;
	}
      from = NEXT_INSN (from);
    }

  if (num_mova)
    {
      if (leading_mova)
	{
	  /* Try as we might, the leading mova is out of range.  Change
	     it into a load (which will become a pcload) and retry.  */
	  fixup_mova (mova);
	  return find_barrier (0, 0, mova);
	}
      else
	{
	  /* Insert the constant pool table before the mova instruction,
	     to prevent the mova label reference from going out of range.  */
	  from = mova;
	  good_barrier = found_barrier = barrier_before_mova;
	}
    }

  if (found_barrier)
    {
      if (good_barrier && next_real_insn (found_barrier))
	found_barrier = good_barrier;
    }
  else
    {
      /* We didn't find a barrier in time to dump our stuff,
	 so we'll make one.  */
      rtx_code_label *label = gen_label_rtx ();

      /* Don't emit a constant table in the middle of insns for
	 casesi_worker_2.  This is a bit overkill but is enough
	 because casesi_worker_2 wouldn't appear so frequently.  */
      if (last_symoff)
	from = last_symoff;

      /* If we exceeded the range, then we must back up over the last
	 instruction we looked at.  Otherwise, we just need to undo the
	 NEXT_INSN at the end of the loop.  */
      if (PREV_INSN (from) != orig
	  && (count_hi > hi_limit || count_si > si_limit))
	from = PREV_INSN (PREV_INSN (from));
      else
	from = PREV_INSN (from);

      /* Don't emit a constant table int the middle of global pointer setting,
	 since that that would move the addressing base GOT into another table. 
	 We need the first mov instruction before the _GLOBAL_OFFSET_TABLE_
	 in the pool anyway, so just move up the whole constant pool.

	 However, avoid doing so when the last single GOT mov is the starting
	 insn itself. Going past above the start insn would create a negative
	 offset, causing errors.  */
      if (last_got && last_got != orig)
        from = PREV_INSN (last_got);

      /* Don't insert the constant pool table at the position which
	 may be the landing pad.  */
      if (flag_exceptions
	  && CALL_P (from)
	  && find_reg_note (from, REG_EH_REGION, NULL_RTX))
	from = PREV_INSN (from);

      /* Walk back to be just before any jump or label.
	 Putting it before a label reduces the number of times the branch
	 around the constant pool table will be hit.  Putting it before
	 a jump makes it more likely that the bra delay slot will be
	 filled.  */
      while (NOTE_P (from) || JUMP_P (from)
	     || LABEL_P (from))
	from = PREV_INSN (from);

      /* Make sure we do not split between a call and its corresponding
	 CALL_ARG_LOCATION note.  */
      if (CALL_P (from))
	{
	  rtx_insn *next = NEXT_INSN (from);
	  if (next && NOTE_P (next)
	      && NOTE_KIND (next) == NOTE_INSN_CALL_ARG_LOCATION)
	    from = next;
	}

      from = emit_jump_insn_after (gen_jump (label), from);
      JUMP_LABEL (from) = label;
      LABEL_NUSES (label) = 1;
      found_barrier = emit_barrier_after (from);
      emit_label_after (label, found_barrier);
    }

  return found_barrier;
}

/* If the instruction INSN is implemented by a special function, and we can
   positively find the register that is used to call the sfunc, and this
   register is not used anywhere else in this instruction - except as the
   destination of a set, return this register; else, return 0.  */
rtx
sfunc_uses_reg (rtx_insn *insn)
{
  int i;
  rtx pattern, part, reg_part, reg;

  if (!NONJUMP_INSN_P (insn))
    return NULL_RTX;
  pattern = PATTERN (insn);
  if (GET_CODE (pattern) != PARALLEL || get_attr_type (insn) != TYPE_SFUNC)
    return NULL_RTX;

  for (reg_part = NULL_RTX, i = XVECLEN (pattern, 0) - 1; i >= 1; i--)
    {
      part = XVECEXP (pattern, 0, i);
      if (GET_CODE (part) == USE && GET_MODE (XEXP (part, 0)) == SImode)
	reg_part = part;
    }
  if (! reg_part)
    return NULL_RTX;
  reg = XEXP (reg_part, 0);
  for (int i = XVECLEN (pattern, 0) - 1; i >= 0; i--)
    {
      part = XVECEXP (pattern, 0, i);
      if (part == reg_part || GET_CODE (part) == CLOBBER)
	continue;
      if (reg_mentioned_p (reg, ((GET_CODE (part) == SET
				  && REG_P (SET_DEST (part)))
				 ? SET_SRC (part) : part)))
	return NULL_RTX;
    }
  return reg;
}

/* See if the only way in which INSN uses REG is by calling it, or by
   setting it while calling it.  Set *SET to a SET rtx if the register
   is set by INSN.  */
static bool
noncall_uses_reg (rtx reg, rtx_insn *insn, rtx *set)
{
  *set = NULL_RTX;

  rtx reg2 = sfunc_uses_reg (insn);
  if (reg2 && REGNO (reg2) == REGNO (reg))
    {
      rtx pattern = single_set (insn);
      if (pattern
	  && REG_P (SET_DEST (pattern))
	  && REGNO (reg) == REGNO (SET_DEST (pattern)))
	*set = pattern;
      return false;
    }
  if (!CALL_P (insn))
    {
      /* We don't use rtx_equal_p because we don't care if the mode is
	 different.  */
      rtx pattern = single_set (insn);
      if (pattern
	  && REG_P (SET_DEST (pattern))
	  && REGNO (reg) == REGNO (SET_DEST (pattern)))
	{
	  rtx par, part;
	  int i;

	  *set = pattern;
	  par = PATTERN (insn);
	  if (GET_CODE (par) == PARALLEL)
	    for (i = XVECLEN (par, 0) - 1; i >= 0; i--)
	      {
		part = XVECEXP (par, 0, i);
		if (GET_CODE (part) != SET && reg_mentioned_p (reg, part))
		  return true;
	      }
	  return reg_mentioned_p (reg, SET_SRC (pattern));
	}

      return true;
    }

  rtx pattern = PATTERN (insn);

  if (GET_CODE (pattern) == PARALLEL)
    {
      for (int i = XVECLEN (pattern, 0) - 1; i >= 1; i--)
	if (reg_mentioned_p (reg, XVECEXP (pattern, 0, i)))
	  return true;
      pattern = XVECEXP (pattern, 0, 0);
    }

  if (GET_CODE (pattern) == SET)
    {
      if (reg_mentioned_p (reg, SET_DEST (pattern)))
	{
	  /* We don't use rtx_equal_p, because we don't care if the
	     mode is different.  */
	  if (!REG_P (SET_DEST (pattern))
	      || REGNO (reg) != REGNO (SET_DEST (pattern)))
	    return true;

	  *set = pattern;
	}

      pattern = SET_SRC (pattern);
    }

  if (GET_CODE (pattern) != CALL
      || !MEM_P (XEXP (pattern, 0))
      || ! rtx_equal_p (reg, XEXP (XEXP (pattern, 0), 0)))
    return true;

  return false;
}

/* Given a X, a pattern of an insn or a part of it, return a mask of used
   general registers.  Bits 0..15 mean that the respective registers
   are used as inputs in the instruction.  Bits 16..31 mean that the
   registers 0..15, respectively, are used as outputs, or are clobbered.
   IS_DEST should be set to 16 if X is the destination of a SET, else to 0.  */
int
regs_used (rtx x, int is_dest)
{
  enum rtx_code code;
  const char *fmt;
  int used = 0;

  if (! x)
    return used;
  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      if (REGNO (x) < 16)
	return (((1 << HARD_REGNO_NREGS (0, GET_MODE (x))) - 1)
		<< (REGNO (x) + is_dest));
      return 0;
    case SUBREG:
      {
	rtx y = SUBREG_REG (x);

	if (!REG_P (y))
	  break;
	if (REGNO (y) < 16)
	  return (((1 << HARD_REGNO_NREGS (0, GET_MODE (x))) - 1)
		  << (REGNO (y) +
		      subreg_regno_offset (REGNO (y),
					   GET_MODE (y),
					   SUBREG_BYTE (x),
					   GET_MODE (x)) + is_dest));
	return 0;
      }
    case SET:
      return regs_used (SET_SRC (x), 0) | regs_used (SET_DEST (x), 16);
    case RETURN:
      /* If there was a return value, it must have been indicated with USE.  */
      return 0x00ffff00;
    case CLOBBER:
      is_dest = 1;
      break;
    case MEM:
      is_dest = 0;
      break;
    case CALL:
      used |= 0x00ff00f0;
      break;
    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);

  for (int i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	    used |= regs_used (XVECEXP (x, i, j), is_dest);
	}
      else if (fmt[i] == 'e')
	used |= regs_used (XEXP (x, i), is_dest);
    }
  return used;
}

/* Create an instruction that prevents redirection of a conditional branch
   to the destination of the JUMP with address ADDR.
   If the branch needs to be implemented as an indirect jump, try to find
   a scratch register for it.
   If NEED_BLOCK is 0, don't do anything unless we need a scratch register.
   If any preceding insn that doesn't fit into a delay slot is good enough,
   pass 1.  Pass 2 if a definite blocking insn is needed.
   -1 is used internally to avoid deep recursion.
   If a blocking instruction is made or recognized, return it.  */
static rtx_insn *
gen_block_redirect (rtx_insn *jump, int addr, int need_block)
{
  int dead = 0;
  rtx_insn *prev = prev_nonnote_insn (jump);

  /* First, check if we already have an instruction that satisfies our need.  */
  if (prev && NONJUMP_INSN_P (prev) && ! prev->deleted ())
    {
      if (INSN_CODE (prev) == CODE_FOR_indirect_jump_scratch)
	return prev;
      if (GET_CODE (PATTERN (prev)) == USE
	  || GET_CODE (PATTERN (prev)) == CLOBBER
	  || get_attr_in_delay_slot (prev) == IN_DELAY_SLOT_YES)
	prev = jump;
      else if ((need_block &= ~1) < 0)
	return prev;
      else if (recog_memoized (prev) == CODE_FOR_block_branch_redirect)
	need_block = 0;
    }
  if (GET_CODE (PATTERN (jump)) == RETURN)
    {
      if (! need_block)
	return prev;
      /* Reorg even does nasty things with return insns that cause branches
	 to go out of range - see find_end_label and callers.  */
      return emit_insn_before (gen_block_branch_redirect (const0_rtx) , jump);
    }
  /* We can't use JUMP_LABEL here because it might be undefined
     when not optimizing.  */
  rtx dest = XEXP (SET_SRC (PATTERN (jump)), 0);
  /* If the branch is out of range, try to find a scratch register for it.  */
  if (optimize
      && (INSN_ADDRESSES (INSN_UID (dest)) - addr + (unsigned) 4092
	  > 4092 + 4098))
    {
      rtx_insn *scan;
      /* Don't look for the stack pointer as a scratch register,
	 it would cause trouble if an interrupt occurred.  */
      unsigned attempt = 0x7fff, used;
      int jump_left = flag_expensive_optimizations + 1;

      /* It is likely that the most recent eligible instruction is wanted for
	 the delay slot.  Therefore, find out which registers it uses, and
	 try to avoid using them.  */

      for (scan = jump; (scan = PREV_INSN (scan)); )
	{
	  if (scan->deleted ())
	    continue;
	  rtx_code code = GET_CODE (scan);
	  if (code == CODE_LABEL || code == JUMP_INSN)
	    break;
	  if (code == INSN
	      && GET_CODE (PATTERN (scan)) != USE
	      && GET_CODE (PATTERN (scan)) != CLOBBER
	      && get_attr_in_delay_slot (scan) == IN_DELAY_SLOT_YES)
	    {
	      attempt &= ~regs_used (PATTERN (scan), 0);
	      break;
	    }
	}
      for (used = dead = 0, scan = JUMP_LABEL_AS_INSN (jump);
	   (scan = NEXT_INSN (scan)); )
	{
	  if (scan->deleted ())
	    continue;
	  rtx_code code = GET_CODE (scan);
	  if (INSN_P (scan))
	    {
	      used |= regs_used (PATTERN (scan), 0);
	      if (code == CALL_INSN)
		used |= regs_used (CALL_INSN_FUNCTION_USAGE (scan), 0);
	      dead |= (used >> 16) & ~used;
	      if (dead & attempt)
		{
		  dead &= attempt;
		  break;
		}
	      if (code == JUMP_INSN)
		{
		  if (jump_left-- && simplejump_p (scan))
		    scan = JUMP_LABEL_AS_INSN (scan);
		  else
		    break;
		}
	    }
	}
      /* Mask out the stack pointer again, in case it was
	 the only 'free' register we have found.  */
      dead &= 0x7fff;
    }
  /* If the immediate destination is still in range, check for possible
     threading with a jump beyond the delay slot insn.
     Don't check if we are called recursively; the jump has been or will be
     checked in a different invocation then.  */

  else if (optimize && need_block >= 0)
    {
      rtx_insn *next = next_active_insn (next_active_insn (dest));
      if (next && JUMP_P (next)
	  && GET_CODE (PATTERN (next)) == SET
	  && recog_memoized (next) == CODE_FOR_jump_compact)
	{
	  dest = JUMP_LABEL (next);
	  if (dest
	      && (INSN_ADDRESSES (INSN_UID (dest)) - addr + (unsigned) 4092
		  > 4092 + 4098))
	    gen_block_redirect (next, INSN_ADDRESSES (INSN_UID (next)), -1);
	}
    }

  if (dead)
    {
      rtx reg = gen_rtx_REG (SImode, exact_log2 (dead & -dead));

      /* It would be nice if we could convert the jump into an indirect
	 jump / far branch right now, and thus exposing all constituent
	 instructions to further optimization.  However, reorg uses
	 simplejump_p to determine if there is an unconditional jump where
	 it should try to schedule instructions from the target of the
	 branch; simplejump_p fails for indirect jumps even if they have
	 a JUMP_LABEL.  */
      rtx_insn *insn = emit_insn_before (gen_indirect_jump_scratch
					 (reg, GEN_INT (unspec_bbr_uid++)),
					 jump);
      /* ??? We would like this to have the scope of the jump, but that
	 scope will change when a delay slot insn of an inner scope is added.
	 Hence, after delay slot scheduling, we'll have to expect
	 NOTE_INSN_BLOCK_END notes between the indirect_jump_scratch and
	 the jump.  */

      INSN_LOCATION (insn) = INSN_LOCATION (jump);
      INSN_CODE (insn) = CODE_FOR_indirect_jump_scratch;
      return insn;
    }
  else if (need_block)
    /* We can't use JUMP_LABEL here because it might be undefined
       when not optimizing.  */
    return emit_insn_before (gen_block_branch_redirect
			     (GEN_INT (unspec_bbr_uid++)),
			     jump);
  return prev;
}

#define CONDJUMP_MIN -252
#define CONDJUMP_MAX 262
struct far_branch
{
  /* A label (to be placed) in front of the jump
     that jumps to our ultimate destination.  */
  rtx_insn *near_label;
  /* Where we are going to insert it if we cannot move the jump any farther,
     or the jump itself if we have picked up an existing jump.  */
  rtx_insn *insert_place;
  /* The ultimate destination.  */
  rtx_insn *far_label;
  struct far_branch *prev;
  /* If the branch has already been created, its address;
     else the address of its first prospective user.  */
  int address;
};

enum mdep_reorg_phase_e mdep_reorg_phase;

static void
gen_far_branch (struct far_branch *bp)
{
  rtx_insn *insn = bp->insert_place;
  rtx_jump_insn *jump;
  rtx_code_label *label = gen_label_rtx ();

  emit_label_after (label, insn);
  if (bp->far_label)
    {
      jump = emit_jump_insn_after (gen_jump (bp->far_label), insn);
      LABEL_NUSES (bp->far_label)++;
    }
  else
    jump = emit_jump_insn_after (gen_return (), insn);

  /* Emit a barrier so that reorg knows that any following instructions
     are not reachable via a fall-through path.
     But don't do this when not optimizing, since we wouldn't suppress the
     alignment for the barrier then, and could end up with out-of-range
     pc-relative loads.  */
  if (optimize)
    emit_barrier_after (jump);
  emit_label_after (bp->near_label, insn);

  if (bp->far_label)
    JUMP_LABEL (jump) = bp->far_label;
  else
    {
      rtx pat = PATTERN (jump);
      gcc_assert (ANY_RETURN_P (pat));
      JUMP_LABEL (jump) = pat;
    }

  bool ok = invert_jump (as_a <rtx_jump_insn *> (insn), label, 1);
  gcc_assert (ok);

  /* If we are branching around a jump (rather than a return), prevent
     reorg from using an insn from the jump target as the delay slot insn -
     when reorg did this, it pessimized code (we rather hide the delay slot)
     and it could cause branches to go out of range.  */
  if (bp->far_label)
    (emit_insn_after
     (gen_stuff_delay_slot
      (GEN_INT (unspec_bbr_uid++),
       GEN_INT (recog_memoized (insn) == CODE_FOR_branch_false)),
      insn));
  /* Prevent reorg from undoing our splits.  */
  gen_block_redirect (jump, bp->address += 2, 2);
}

/* Fix up ADDR_DIFF_VECs.  */
void
fixup_addr_diff_vecs (rtx_insn *first)
{
  rtx_insn *insn;

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      rtx vec_lab, pat, prevpat, x, braf_label;
      rtx_insn *prev;

      if (! JUMP_TABLE_DATA_P (insn)
	  || GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
	continue;
      pat = PATTERN (insn);
      vec_lab = XEXP (XEXP (pat, 0), 0);

      /* Search the matching casesi_jump_2.  */
      for (prev = as_a <rtx_insn *> (vec_lab); ; prev = PREV_INSN (prev))
	{
	  if (!JUMP_P (prev))
	    continue;
	  prevpat = PATTERN (prev);
	  if (GET_CODE (prevpat) != PARALLEL || XVECLEN (prevpat, 0) != 2)
	    continue;
	  x = XVECEXP (prevpat, 0, 1);
	  if (GET_CODE (x) != USE)
	    continue;
	  x = XEXP (x, 0);
	  if (GET_CODE (x) == LABEL_REF && XEXP (x, 0) == vec_lab)
	    break;
	}
      /* FIXME: This is a bug in the optimizer, but it seems harmless
	 to just avoid panicing.  */
      if (!prev)
	continue;

      /* Emit the reference label of the braf where it belongs, right after
	 the casesi_jump_2 (i.e. braf).  */
      braf_label = XEXP (XEXP (SET_SRC (XVECEXP (prevpat, 0, 0)), 1), 0);
      emit_label_after (braf_label, prev);

      /* Fix up the ADDR_DIF_VEC to be relative
	 to the reference address of the braf.  */
      XEXP (XEXP (pat, 0), 0) = braf_label;
    }
}

/* BARRIER_OR_LABEL is either a BARRIER or a CODE_LABEL immediately following
   a barrier.  Return the base 2 logarithm of the desired alignment.  */
int
barrier_align (rtx_insn *barrier_or_label)
{
  if (! barrier_or_label)
    return 0;

  if (LABEL_P (barrier_or_label)
      && NEXT_INSN (barrier_or_label)
      && JUMP_TABLE_DATA_P (NEXT_INSN (barrier_or_label)))
    return 2;

  if (BARRIER_P (barrier_or_label)
      && PREV_INSN (barrier_or_label)
      && JUMP_TABLE_DATA_P (PREV_INSN (barrier_or_label)))
    {
      rtx pat = PATTERN (PREV_INSN (barrier_or_label));
      /* If this is a very small table, we want to keep the alignment after
	 the table to the minimum for proper code alignment.  */
      return ((optimize_size
	       || ((unsigned) XVECLEN (pat, 1) * GET_MODE_SIZE (GET_MODE (pat))
		   <= (unsigned) 1 << (CACHE_LOG - 2)))
	      ? 1 : align_jumps_log);
    }

  rtx next = next_active_insn (barrier_or_label);

  if (! next)
    return 0;

  rtx pat = PATTERN (next);

  if (GET_CODE (pat) == UNSPEC_VOLATILE && XINT (pat, 1) == UNSPECV_ALIGN)
    /* This is a barrier in front of a constant table.  */
    return 0;

  if (optimize_size)
    return 0;

  if (! TARGET_SH2 || ! optimize)
    return align_jumps_log;

  /* When fixing up pcloads, a constant table might be inserted just before
     the basic block that ends with the barrier.  Thus, we can't trust the
     instruction lengths before that.  */
  if (mdep_reorg_phase > SH_FIXUP_PCLOAD)
    {
      /* Check if there is an immediately preceding branch to the insn beyond
	 the barrier.  We must weight the cost of discarding useful information
	 from the current cache line when executing this branch and there is
	 an alignment, against that of fetching unneeded insn in front of the
	 branch target when there is no alignment.  */

      /* There are two delay_slot cases to consider.  One is the simple case
	 where the preceding branch is to the insn beyond the barrier (simple
	 delay slot filling), and the other is where the preceding branch has
	 a delay slot that is a duplicate of the insn after the barrier
	 (fill_eager_delay_slots) and the branch is to the insn after the insn
	 after the barrier.  */

      int slot, credit;
      bool jump_to_next = false;

      /* Skip to the insn before the JUMP_INSN before the barrier under
	 investigation.  */
      rtx_insn *prev = prev_real_insn (prev_active_insn (barrier_or_label));

      for (slot = 2, credit = (1 << (CACHE_LOG - 2)) + 2;
	   credit >= 0 && prev && NONJUMP_INSN_P (prev);
	   prev = prev_real_insn (prev))
	{
	  jump_to_next = false;
	  if (GET_CODE (PATTERN (prev)) == USE
	      || GET_CODE (PATTERN (prev)) == CLOBBER)
	    continue;
	  if (rtx_sequence *prev_seq = dyn_cast <rtx_sequence *> (PATTERN (prev)))
	    {
	      prev = prev_seq->insn (1);
	      if (INSN_UID (prev) == INSN_UID (next))
		{
	  	  /* Delay slot was filled with insn at jump target.  */
		  jump_to_next = true;
		  continue;
  		}
	    }

	  if (slot &&
	      get_attr_in_delay_slot (prev) == IN_DELAY_SLOT_YES)
	    slot = 0;
	  credit -= get_attr_length (prev);
	}
      if (prev && jump_to_label_p (prev))
	{
	  rtx_insn *x;
	  if (jump_to_next
	      || next_real_insn (JUMP_LABEL (prev)) == next
	      /* If relax_delay_slots() decides NEXT was redundant
		 with some previous instruction, it will have
		 redirected PREV's jump to the following insn.  */
	      || JUMP_LABEL (prev) == next_nonnote_insn (next)
	      /* There is no upper bound on redundant instructions
		 that might have been skipped, but we must not put an
		 alignment where none had been before.  */
	      || (x = (NEXT_INSN (NEXT_INSN (PREV_INSN (prev)))),
		  (INSN_P (x)
		   && (INSN_CODE (x) == CODE_FOR_block_branch_redirect
		       || INSN_CODE (x) == CODE_FOR_indirect_jump_scratch
		       || INSN_CODE (x) == CODE_FOR_stuff_delay_slot))))
	    {
	      rtx pat = PATTERN (prev);
	      if (GET_CODE (pat) == PARALLEL)
		pat = XVECEXP (pat, 0, 0);
	      if (credit - slot >= (GET_CODE (SET_SRC (pat)) == PC ? 2 : 0))
		return 0;
	    }
	}
    }

  return align_jumps_log;
}

/* If we are inside a phony loop, almost any kind of label can turn up as the
   first one in the loop.  Aligning a braf label causes incorrect switch
   destination addresses; we can detect braf labels because they are
   followed by a BARRIER.
   Applying loop alignment to small constant or switch tables is a waste
   of space, so we suppress this too.  */
int
sh_loop_align (rtx_insn *label)
{
  rtx_insn *next = label;

  if (! optimize || optimize_size)
    return 0;

  do
    next = next_nonnote_insn (next);
  while (next && LABEL_P (next));

  if (! next
      || ! INSN_P (next)
      || recog_memoized (next) == CODE_FOR_consttable_2)
    return 0;

  return align_loops_log;
}

/* Do a final pass over the function, just before delayed branch
   scheduling.  */
static void
sh_reorg (void)
{
  rtx_insn *first, *insn, *mova = NULL;
  int num_mova;
  rtx r0_rtx = gen_rtx_REG (Pmode, 0);
  rtx r0_inc_rtx = gen_rtx_POST_INC (Pmode, r0_rtx);

  first = get_insns ();
  max_labelno_before_reorg = max_label_num ();

  /* We must split call insns before introducing `mova's.  If we're
     optimizing, they'll have already been split.  Otherwise, make
     sure we don't split them too late.  */
  if (! optimize)
    split_all_insns_noflow ();

  /* If relaxing, generate pseudo-ops to associate function calls with
     the symbols they call.  It does no harm to not generate these
     pseudo-ops.  However, when we can generate them, it enables the
     linker to potentially relax the jsr to a bsr, and eliminate the
     register load and, possibly, the constant pool entry.  */

  mdep_reorg_phase = SH_INSERT_USES_LABELS;
  if (TARGET_RELAX)
    {
      /* Remove all REG_LABEL_OPERAND notes.  We want to use them for our
	 own purposes.  This works because none of the remaining passes
	 need to look at them.

	 ??? But it may break in the future.  We should use a machine
	 dependent REG_NOTE, or some other approach entirely.  */
      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  if (INSN_P (insn))
	    {
	      rtx note;

	      while ((note = find_reg_note (insn, REG_LABEL_OPERAND,
					    NULL_RTX)) != 0)
		remove_note (insn, note);
	    }
	}

      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  rtx pattern, reg, set, dies;
	  rtx_code_label *label;
	  rtx_insn *link, *scan;
	  int rescan = 0, foundinsn = 0;

	  if (CALL_P (insn))
	    {
	      pattern = PATTERN (insn);

	      if (GET_CODE (pattern) == PARALLEL)
		pattern = XVECEXP (pattern, 0, 0);
	      if (GET_CODE (pattern) == SET)
		pattern = SET_SRC (pattern);

	      if (GET_CODE (pattern) != CALL
		  || !MEM_P (XEXP (pattern, 0)))
		continue;

	      reg = XEXP (XEXP (pattern, 0), 0);
	    }
	  else
	    {
	      reg = sfunc_uses_reg (insn);
	      if (! reg)
		continue;
	    }

	  if (!REG_P (reg))
	    continue;

	  /* Try scanning backward to find where the register is set.  */
	  link = NULL;
	  for (scan = PREV_INSN (insn);
	       scan && !LABEL_P (scan);
	       scan = PREV_INSN (scan))
	    {
	      if (! INSN_P (scan))
		continue;

	      if (! reg_mentioned_p (reg, scan))
		continue;

	      if (noncall_uses_reg (reg, scan, &set))
		break;

	      if (set)
		{
		  link = scan;
		  break;
		}
	    }

	  if (! link)
	    continue;

	  /* The register is set at LINK.  */

	  /* We can only optimize the function call if the register is
	     being set to a symbol.  In theory, we could sometimes
	     optimize calls to a constant location, but the assembler
	     and linker do not support that at present.  */
	  if (GET_CODE (SET_SRC (set)) != SYMBOL_REF
	      && GET_CODE (SET_SRC (set)) != LABEL_REF)
	    continue;

	  /* Scan forward from LINK to the place where REG dies, and
	     make sure that the only insns which use REG are
	     themselves function calls.  */

	  /* ??? This doesn't work for call targets that were allocated
	     by reload, since there may not be a REG_DEAD note for the
	     register.  */

	  dies = NULL_RTX;
	  for (scan = NEXT_INSN (link); scan; scan = NEXT_INSN (scan))
	    {
	      rtx scanset;

	      /* Don't try to trace forward past a CODE_LABEL if we haven't
		 seen INSN yet.  Ordinarily, we will only find the setting insn
		 if it is in the same basic block.  However,
		 cross-jumping can insert code labels in between the load and
		 the call, and can result in situations where a single call
		 insn may have two targets depending on where we came from.  */

	      if (LABEL_P (scan) && ! foundinsn)
		break;

	      if (! INSN_P (scan))
		continue;

	      /* Don't try to trace forward past a JUMP.  To optimize
		 safely, we would have to check that all the
		 instructions at the jump destination did not use REG.  */

	      if (JUMP_P (scan))
		break;

	      if (! reg_mentioned_p (reg, scan))
		continue;

	      if (noncall_uses_reg (reg, scan, &scanset))
		break;

	      if (scan == insn)
		foundinsn = 1;

	      if (scan != insn
		  && (CALL_P (scan) || sfunc_uses_reg (scan)))
		{
		  /* There is a function call to this register other
		     than the one we are checking.  If we optimize
		     this call, we need to rescan again below.  */
		  rescan = 1;
		}

	      /* ??? We shouldn't have to worry about SCANSET here.
		 We should just be able to check for a REG_DEAD note
		 on a function call.  However, the REG_DEAD notes are
		 apparently not dependable around libcalls; c-torture
		 execute/920501-2 is a test case.  If SCANSET is set,
		 then this insn sets the register, so it must have
		 died earlier.  Unfortunately, this will only handle
		 the cases in which the register is, in fact, set in a
		 later insn.  */

	      /* ??? We shouldn't have to use FOUNDINSN here.
		 This dates back to when we used LOG_LINKS to find 
		 the most recent insn which sets the register.  */

	      if (foundinsn
		  && (scanset
		      || find_reg_note (scan, REG_DEAD, reg)))
		{
		  dies = scan;
		  break;
		}
	    }

	  if (! dies)
	    {
	      /* Either there was a branch, or some insn used REG
		 other than as a function call address.  */
	      continue;
	    }

	  /* Create a code label, and put it in a REG_LABEL_OPERAND note
	     on the insn which sets the register, and on each call insn
	     which uses the register.  In final_prescan_insn we look for
	     the REG_LABEL_OPERAND notes, and output the appropriate label
	     or pseudo-op.  */

	  label = gen_label_rtx ();
	  add_reg_note (link, REG_LABEL_OPERAND, label);
	  add_reg_note (insn, REG_LABEL_OPERAND, label);
	  if (rescan)
	    {
	      scan = link;
	      do
		{
		  rtx reg2;

		  scan = NEXT_INSN (scan);
		  if (scan != insn
		      && ((CALL_P (scan)
			   && reg_mentioned_p (reg, scan))
			  || ((reg2 = sfunc_uses_reg (scan))
			      && REGNO (reg2) == REGNO (reg))))
		    add_reg_note (scan, REG_LABEL_OPERAND, label);
		}
	      while (scan != dies);
	    }
	}
    }

  if (TARGET_SH2)
    fixup_addr_diff_vecs (first);

  if (optimize)
    {
      mdep_reorg_phase = SH_SHORTEN_BRANCHES0;
      shorten_branches (first);
    }

  /* Scan the function looking for move instructions which have to be
     changed to pc-relative loads and insert the literal tables.  */
  mdep_reorg_phase = SH_FIXUP_PCLOAD;
  for (insn = first, num_mova = 0; insn; insn = NEXT_INSN (insn))
    {
      if (mova_p (insn))
	{
	  /* ??? basic block reordering can move a switch table dispatch
	     below the switch table.  Check if that has happened.
	     We only have the addresses available when optimizing; but then,
	     this check shouldn't be needed when not optimizing.  */
	  if (!untangle_mova (&num_mova, &mova, insn))
	    {
	      insn = mova;
	      num_mova = 0;
	    }
	}
      else if (JUMP_TABLE_DATA_P (insn)
	       && GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC
	       && num_mova
	       /* ??? loop invariant motion can also move a mova out of a
		  loop.  Since loop does this code motion anyway, maybe we
		  should wrap UNSPEC_MOVA into a CONST, so that reload can
		  move it back.  */
	       && ((num_mova > 1
		    && GET_MODE (prev_nonnote_insn (insn)) == VOIDmode)
		   || (prev_nonnote_insn (insn)
		       == XEXP (MOVA_LABELREF (mova), 0))))
	{
	  rtx_insn *scan;
	  int total;

	  num_mova--;

	  /* Some code might have been inserted between the mova and
	     its ADDR_DIFF_VEC.  Check if the mova is still in range.  */
	  for (scan = mova, total = 0; scan != insn; scan = NEXT_INSN (scan))
	    total += get_attr_length (scan);

	  /* range of mova is 1020, add 4 because pc counts from address of
	     second instruction after this one, subtract 2 in case pc is 2
	     byte aligned.  Possible alignment needed for the ADDR_DIFF_VEC
	     cancels out with alignment effects of the mova itself.  */
	  if (total > 1022)
	    {
	      /* Change the mova into a load, and restart scanning
		 there.  broken_move will then return true for mova.  */
	      fixup_mova (mova);
	      insn = mova;
	    }
	}
      if (broken_move (insn)
	  || (NONJUMP_INSN_P (insn)
	      && recog_memoized (insn) == CODE_FOR_casesi_worker_2))
	{
	  rtx_insn *scan;
	  /* Scan ahead looking for a barrier to stick the constant table
	     behind.  */
	  rtx_insn *barrier = find_barrier (num_mova, mova, insn);
	  rtx_insn *last_float_move = NULL;
	  rtx last_float = 0, *last_float_addr = NULL;
	  int need_aligned_label = 0;

	  if (num_mova && ! mova_p (mova))
	    {
	      /* find_barrier had to change the first mova into a
		 pcload; thus, we have to start with this new pcload.  */
	      insn = mova;
	      num_mova = 0;
	    }
	  /* Now find all the moves between the points and modify them.  */
	  for (scan = insn; scan != barrier; scan = NEXT_INSN (scan))
	    {
	      if (LABEL_P (scan))
		last_float = 0;
	      if (NONJUMP_INSN_P (scan)
		  && recog_memoized (scan) == CODE_FOR_casesi_worker_2)
		need_aligned_label = 1;
	      if (broken_move (scan))
		{
		  rtx *patp = &PATTERN (scan), pat = *patp;
		  rtx src, dst;
		  rtx lab;
		  rtx newsrc;
		  machine_mode mode;

		  if (GET_CODE (pat) == PARALLEL)
		    patp = &XVECEXP (pat, 0, 0), pat = *patp;
		  src = SET_SRC (pat);
		  dst = SET_DEST (pat);
		  mode = GET_MODE (dst);

		  if (mode == SImode && satisfies_constraint_I16 (src)
		      && REGNO (dst) != FPUL_REG)
		    {
		      int offset = 0;

		      mode = HImode;
		      while (GET_CODE (dst) == SUBREG)
			{
			  offset += subreg_regno_offset (REGNO (SUBREG_REG (dst)),
							 GET_MODE (SUBREG_REG (dst)),
							 SUBREG_BYTE (dst),
							 GET_MODE (dst));
			  dst = SUBREG_REG (dst);
			}
		      dst = gen_rtx_REG (HImode, REGNO (dst) + offset);
		    }
		  if (REG_P (dst) && FP_ANY_REGISTER_P (REGNO (dst)))
		    {
		      /* This must be an insn that clobbers r0.  */
		      rtx *clobberp = &XVECEXP (PATTERN (scan), 0,
						XVECLEN (PATTERN (scan), 0)
						- 1);
		      rtx clobber = *clobberp;

		      gcc_assert (GET_CODE (clobber) == CLOBBER
				  && rtx_equal_p (XEXP (clobber, 0), r0_rtx));

		      if (last_float
			  && reg_set_between_p (r0_rtx, last_float_move, scan))
			last_float = 0;
		      lab = add_constant (src, mode, last_float);
		      if (lab)
			emit_insn_before (gen_mova (lab), scan);
		      else
			{
			  /* There will be a REG_UNUSED note for r0 on
			     LAST_FLOAT_MOVE; we have to change it to REG_INC,
			     lest reorg:mark_target_live_regs will not
			     consider r0 to be used, and we end up with delay
			     slot insn in front of SCAN that clobbers r0.  */
			  rtx note
			    = find_regno_note (last_float_move, REG_UNUSED, 0);

			  /* If we are not optimizing, then there may not be
			     a note.  */
			  if (note)
			    PUT_REG_NOTE_KIND (note, REG_INC);

			  *last_float_addr = r0_inc_rtx;
			}
		      last_float_move = scan;
		      last_float = src;
		      newsrc = gen_const_mem (mode,
					(((TARGET_SH4 && ! TARGET_FMOVD)
					  || REGNO (dst) == FPUL_REG)
					 ? r0_inc_rtx
					 : r0_rtx));
		      last_float_addr = &XEXP (newsrc, 0);

		      /* Remove the clobber of r0.  */
		      *clobberp = gen_rtx_CLOBBER (GET_MODE (clobber),
						   gen_rtx_SCRATCH (Pmode));
		    }
		  /* This is a mova needing a label.  Create it.  */
		  else if (GET_CODE (src) == UNSPEC
			   && XINT (src, 1) == UNSPEC_MOVA
			   && GET_CODE (XVECEXP (src, 0, 0)) == CONST)
		    {
		      lab = add_constant (XVECEXP (src, 0, 0), mode, 0);
		      newsrc = gen_rtx_LABEL_REF (VOIDmode, lab);
		      newsrc = gen_rtx_UNSPEC (SImode,
					       gen_rtvec (1, newsrc),
					       UNSPEC_MOVA);
		    }
		  else if (GET_CODE (src) == UNSPEC_VOLATILE
			   && XINT (src, 1) == UNSPECV_SP_SWITCH_B)
		    {
		      newsrc = XVECEXP (src, 0, 0);
		      XVECEXP (src, 0, 0) = gen_const_mem (mode, newsrc);
		      INSN_CODE (scan) = -1;
		      continue;
		    }
		  else
		    {
		      lab = add_constant (src, mode, 0);
		      newsrc = gen_rtx_LABEL_REF (VOIDmode, lab);
		      newsrc = gen_const_mem (mode, newsrc);
		    }
		  *patp = gen_rtx_SET (dst, newsrc);
		  INSN_CODE (scan) = -1;
		}
	    }
	  dump_table (need_aligned_label ? insn : 0, barrier);
	  insn = barrier;
	}
    }
  label_ref_list_d_pool.release ();
  for (insn = first; insn; insn = NEXT_INSN (insn))
    PUT_MODE (insn, VOIDmode);

  mdep_reorg_phase = SH_SHORTEN_BRANCHES1;
  INSN_ADDRESSES_FREE ();
  split_branches (first);

  /* The INSN_REFERENCES_ARE_DELAYED in sh.h is problematic because it
     also has an effect on the register that holds the address of the sfunc.
     Insert an extra dummy insn in front of each sfunc that pretends to
     use this register.  */
  if (flag_delayed_branch)
    {
      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  rtx reg = sfunc_uses_reg (insn);

	  if (! reg)
	    continue;
	  emit_insn_before (gen_use_sfunc_addr (reg), insn);
	}
    }
  mdep_reorg_phase = SH_AFTER_MDEP_REORG;
}

/* Return the UID of the insn that follows the specified label.  */
int
get_dest_uid (rtx label, int max_uid)
{
  rtx_insn *dest = next_real_insn (label);

  if (! dest)
    /* This can happen for an undefined label.  */
    return 0;
  int dest_uid = INSN_UID (dest);
  /* If this is a newly created branch redirection blocking instruction,
     we cannot index the branch_uid or insn_addresses arrays with its
     uid.  But then, we won't need to, because the actual destination is
     the following branch.  */
  while (dest_uid >= max_uid)
    {
      dest = NEXT_INSN (dest);
      dest_uid = INSN_UID (dest);
    }
  if (JUMP_P (dest) && GET_CODE (PATTERN (dest)) == RETURN)
    return 0;
  return dest_uid;
}

/* Split condbranches that are out of range.  Also add clobbers for
   scratch registers that are needed in far jumps.
   We do this before delay slot scheduling, so that it can take our
   newly created instructions into account.  It also allows us to
   find branches with common targets more easily.  */
static void
split_branches (rtx_insn *first)
{
  rtx_insn *insn;
  struct far_branch **uid_branch, *far_branch_list = 0;
  int max_uid = get_max_uid ();
  int ok;

  /* Find out which branches are out of range.  */
  shorten_branches (first);

  uid_branch = (struct far_branch **) alloca (max_uid * sizeof *uid_branch);
  memset ((char *) uid_branch, 0, max_uid * sizeof *uid_branch);

  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (! INSN_P (insn))
      continue;
    else if (insn->deleted ())
      {
	/* Shorten_branches would split this instruction again,
	   so transform it into a note.  */
	SET_INSN_DELETED (insn);
      }
    else if (JUMP_P (insn))
      {
	enum attr_type type = get_attr_type (insn);
	if (type == TYPE_CBRANCH)
	  {
	    rtx_insn *next, *beyond;

	    if (get_attr_length (insn) > 4)
	      {
		rtx src = SET_SRC (PATTERN (insn));
		rtx olabel = XEXP (XEXP (src, 1), 0);
		int addr = INSN_ADDRESSES (INSN_UID (insn));
		rtx_insn *label = 0;
		int dest_uid = get_dest_uid (olabel, max_uid);
		struct far_branch *bp = uid_branch[dest_uid];

		/* redirect_jump needs a valid JUMP_LABEL, and it might delete
		   the label if the LABEL_NUSES count drops to zero.  There is
		   always a jump_optimize pass that sets these values, but it
		   proceeds to delete unreferenced code, and then if not
		   optimizing, to un-delete the deleted instructions, thus
		   leaving labels with too low uses counts.  */
		if (! optimize)
		  {
		    JUMP_LABEL (insn) = olabel;
		    LABEL_NUSES (olabel)++;
		  }
		if (! bp)
		  {
		    bp = (struct far_branch *) alloca (sizeof *bp);
		    uid_branch[dest_uid] = bp;
		    bp->prev = far_branch_list;
		    far_branch_list = bp;
		    bp->far_label = as_a <rtx_insn *> (
				      XEXP (XEXP (SET_SRC (PATTERN (insn)), 1),
					    0));
		    LABEL_NUSES (bp->far_label)++;
		  }
		else
		  {
		    label = bp->near_label;
		    if (! label && bp->address - addr >= CONDJUMP_MIN)
		      {
			rtx_insn *block = bp->insert_place;

			if (GET_CODE (PATTERN (block)) == RETURN)
			  block = PREV_INSN (block);
			else
			  block = gen_block_redirect (block,
						      bp->address, 2);
			label = emit_label_after (gen_label_rtx (),
						  PREV_INSN (block));
			bp->near_label = label;
		      }
		    else if (label && ! NEXT_INSN (label))
		      {
			if (addr + 2 - bp->address <= CONDJUMP_MAX)
			  bp->insert_place = insn;
			else
			  gen_far_branch (bp);
		      }
		  }
		if (! label
		    || (NEXT_INSN (label) && bp->address - addr < CONDJUMP_MIN))
		  {
		    bp->near_label = label = gen_label_rtx ();
		    bp->insert_place = insn;
		    bp->address = addr;
		  }
		ok = redirect_jump (as_a <rtx_jump_insn *> (insn), label, 0);
		gcc_assert (ok);
	      }
	    else
	      {
		/* get_attr_length (insn) == 2 */
		/* Check if we have a pattern where reorg wants to redirect
		   the branch to a label from an unconditional branch that
		   is too far away.  */
		/* We can't use JUMP_LABEL here because it might be undefined
		   when not optimizing.  */
		/* A syntax error might cause beyond to be NULL_RTX.  */
		beyond
		  = next_active_insn (XEXP (XEXP (SET_SRC (PATTERN (insn)), 1),
					    0));

		if (beyond
		    && (JUMP_P (beyond)
			|| ((beyond = next_active_insn (beyond))
			    && JUMP_P (beyond)))
		    && GET_CODE (PATTERN (beyond)) == SET
		    && recog_memoized (beyond) == CODE_FOR_jump_compact
		    && ((INSN_ADDRESSES
			 (INSN_UID (XEXP (SET_SRC (PATTERN (beyond)), 0)))
			 - INSN_ADDRESSES (INSN_UID (insn)) + (unsigned) 252)
			> 252 + 258 + 2))
		  gen_block_redirect (beyond,
				      INSN_ADDRESSES (INSN_UID (beyond)), 1);
	      }

	    next = next_active_insn (insn);

	    if (next
		&& (JUMP_P (next)
		    || ((next = next_active_insn (next))
			&& JUMP_P (next)))
		&& GET_CODE (PATTERN (next)) == SET
		&& recog_memoized (next) == CODE_FOR_jump_compact
		&& ((INSN_ADDRESSES
		     (INSN_UID (XEXP (SET_SRC (PATTERN (next)), 0)))
		     - INSN_ADDRESSES (INSN_UID (insn)) + (unsigned) 252)
		    > 252 + 258 + 2))
	      gen_block_redirect (next, INSN_ADDRESSES (INSN_UID (next)), 1);
	  }
	else if (type == TYPE_JUMP || type == TYPE_RETURN)
	  {
	    int addr = INSN_ADDRESSES (INSN_UID (insn));
	    rtx_insn *far_label = 0;
	    int dest_uid = 0;
	    struct far_branch *bp;

	    if (type == TYPE_JUMP)
	      {
		if (CROSSING_JUMP_P (insn))
		  {
		    emit_insn_before (gen_block_branch_redirect (const0_rtx),
				      insn);
		    continue;
		  }

		far_label = as_a <rtx_insn *> (
			      XEXP (SET_SRC (PATTERN (insn)), 0));
		dest_uid = get_dest_uid (far_label, max_uid);
		if (! dest_uid)
		  {
		    /* Parse errors can lead to labels outside
		      the insn stream.  */
		    if (! NEXT_INSN (far_label))
		      continue;

		    if (! optimize)
		      {
			JUMP_LABEL (insn) = far_label;
			LABEL_NUSES (far_label)++;
		      }
		    redirect_jump (as_a <rtx_jump_insn *> (insn), ret_rtx, 1);
		    far_label = 0;
		  }
	      }
	    bp = uid_branch[dest_uid];
	    if (! bp)
	      {
		bp = (struct far_branch *) alloca (sizeof *bp);
		uid_branch[dest_uid] = bp;
		bp->prev = far_branch_list;
		far_branch_list = bp;
		bp->near_label = 0;
		bp->far_label = far_label;
		if (far_label)
		  LABEL_NUSES (far_label)++;
	      }
	    else if (bp->near_label && ! NEXT_INSN (bp->near_label))
	      if (addr - bp->address <= CONDJUMP_MAX)
		emit_label_after (bp->near_label, PREV_INSN (insn));
	      else
		{
		  gen_far_branch (bp);
		  bp->near_label = 0;
		}
	    else
	      bp->near_label = 0;
	    bp->address = addr;
	    bp->insert_place = insn;
	    if (! far_label)
	      emit_insn_before (gen_block_branch_redirect (const0_rtx), insn);
	    else
	      gen_block_redirect (insn, addr, bp->near_label ? 2 : 0);
	  }
      }
  /* Generate all pending far branches,
     and free our references to the far labels.  */
  while (far_branch_list)
    {
      if (far_branch_list->near_label
	  && ! NEXT_INSN (far_branch_list->near_label))
	gen_far_branch (far_branch_list);
      if (optimize
	  && far_branch_list->far_label
	  && ! --LABEL_NUSES (far_branch_list->far_label))
	delete_insn (far_branch_list->far_label);
      far_branch_list = far_branch_list->prev;
    }

  /* Instruction length information is no longer valid due to the new
     instructions that have been generated.  */
  init_insn_lengths ();
}

/* Dump out instruction addresses, which is useful for debugging the
   constant pool table stuff.

   If relaxing, output the label and pseudo-ops used to link together
   calls and the instruction which set the registers.

   ??? The addresses printed by this routine for insns are nonsense for
   insns which are inside of a sequence where none of the inner insns have
   variable length.  This is because the second pass of shorten_branches
   does not bother to update them.  */
void
final_prescan_insn (rtx_insn *insn, rtx *opvec ATTRIBUTE_UNUSED,
		    int noperands ATTRIBUTE_UNUSED)
{
  if (TARGET_DUMPISIZE)
    fprintf (asm_out_file, "\n! at %04x\n", INSN_ADDRESSES (INSN_UID (insn)));

  if (TARGET_RELAX)
    {
      if (rtx note = find_reg_note (insn, REG_LABEL_OPERAND, NULL_RTX))
	{
	  rtx pattern = PATTERN (insn);
	  if (GET_CODE (pattern) == PARALLEL)
	    pattern = XVECEXP (pattern, 0, 0);
	  switch (GET_CODE (pattern))
	    {
	    case SET:
	      if (GET_CODE (SET_SRC (pattern)) != CALL
		  && get_attr_type (insn) != TYPE_SFUNC)
		{
		  targetm.asm_out.internal_label
		    (asm_out_file, "L", CODE_LABEL_NUMBER (XEXP (note, 0)));
		  break;
		}
	      /* else FALLTHROUGH */
	    case CALL:
	      asm_fprintf (asm_out_file, "\t.uses %LL%d\n",
			   CODE_LABEL_NUMBER (XEXP (note, 0)));
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}
    }
}

/* Dump out any constants accumulated in the final pass.  These will
   only be labels.  */
const char *
output_jump_label_table (void)
{
  if (pool_size)
    {
      fprintf (asm_out_file, "\t.align 2\n");
      for (int i = 0; i < pool_size; i++)
	{
	  pool_node *p = &pool_vector[i];

	  (*targetm.asm_out.internal_label) (asm_out_file, "L",
				     CODE_LABEL_NUMBER (p->label));
	  output_asm_insn (".long	%O0", &p->value);
	}
      pool_size = 0;
    }

  return "";
}

/* A full frame looks like:

   arg-5
   arg-4
   [ if current_function_anonymous_args
   arg-3
   arg-2
   arg-1
   arg-0 ]
   saved-fp
   saved-r10
   saved-r11
   saved-r12
   saved-pr
   local-n
   ..
   local-1
   local-0        <- fp points here.

   Number of bytes pushed for anonymous args, used to pass information
   between expand_prologue and expand_epilogue.

   Adjust the stack by SIZE bytes.  REG holds the rtl of the register to be
   adjusted.  If epilogue_p is zero, this is for a prologue; otherwise, it's
   for an epilogue and a negative value means that it's for a sibcall
   epilogue.  If LIVE_REGS_MASK is nonzero, it points to a HARD_REG_SET of
   all the registers that are about to be restored, and hence dead.  */
static void
output_stack_adjust (int size, rtx reg, int epilogue_p,
		     HARD_REG_SET *live_regs_mask, bool frame_p)
{
  rtx_insn *(*emit_fn) (rtx) = frame_p ? &emit_frame_insn : &emit_insn;
  if (size)
    {
      HOST_WIDE_INT align = STACK_BOUNDARY / BITS_PER_UNIT;

/* This test is bogus, as output_stack_adjust is used to re-align the
   stack.  */
#if 0
      gcc_assert (!(size % align));
#endif

      if (CONST_OK_FOR_ADD (size))
	emit_fn (GEN_ADD3 (reg, reg, GEN_INT (size)));
      /* Try to do it with two partial adjustments; however, we must make
	 sure that the stack is properly aligned at all times, in case
	 an interrupt occurs between the two partial adjustments.  */
      else if (CONST_OK_FOR_ADD (size / 2 & -align)
	       && CONST_OK_FOR_ADD (size - (size / 2 & -align)))
	{
	  emit_fn (GEN_ADD3 (reg, reg, GEN_INT (size / 2 & -align)));
	  emit_fn (GEN_ADD3 (reg, reg, GEN_INT (size - (size / 2 & -align))));
	}
      else
	{
	  rtx const_reg;
	  rtx insn;
	  int temp = epilogue_p ? 7 : 1;
	  int i;

	  /* If TEMP is invalid, we could temporarily save a general
	     register to MACL.  However, there is currently no need
	     to handle this case, so just die when we see it.  */
	  if (epilogue_p < 0
	      || current_function_interrupt
	      || ! call_really_used_regs[temp] || fixed_regs[temp])
	    temp = -1;
	  if (temp < 0 && ! current_function_interrupt && epilogue_p >= 0)
	    {
	      HARD_REG_SET temps;
	      COPY_HARD_REG_SET (temps, call_used_reg_set);
	      AND_COMPL_HARD_REG_SET (temps, call_fixed_reg_set);
	      if (epilogue_p > 0)
		{
		  int nreg = 0;
		  if (crtl->return_rtx)
		    {
		      machine_mode mode;
		      mode = GET_MODE (crtl->return_rtx);
		      if (BASE_RETURN_VALUE_REG (mode) == FIRST_RET_REG)
			nreg = HARD_REGNO_NREGS (FIRST_RET_REG, mode);
		    }
		  for (i = 0; i < nreg; i++)
		    CLEAR_HARD_REG_BIT (temps, FIRST_RET_REG + i);
		  if (crtl->calls_eh_return)
		    {
		      CLEAR_HARD_REG_BIT (temps, EH_RETURN_STACKADJ_REGNO);
		      for (i = 0; i <= 3; i++)
			CLEAR_HARD_REG_BIT (temps, EH_RETURN_DATA_REGNO (i));
		    }
		}
	      if (epilogue_p <= 0)
		{
		  for (i = FIRST_PARM_REG;
		       i < FIRST_PARM_REG + NPARM_REGS (SImode); i++)
		    CLEAR_HARD_REG_BIT (temps, i);
		  if (cfun->static_chain_decl != NULL)
		    CLEAR_HARD_REG_BIT (temps, STATIC_CHAIN_REGNUM);
		}
	      temp = scavenge_reg (&temps);
	    }
	  if (temp < 0 && live_regs_mask)
	    {
	      HARD_REG_SET temps;

	      COPY_HARD_REG_SET (temps, *live_regs_mask);
	      CLEAR_HARD_REG_BIT (temps, REGNO (reg));
	      temp = scavenge_reg (&temps);
	    }
	  if (temp < 0)
	    {
	      rtx adj_reg, tmp_reg, mem;
	      
	      /* If we reached here, the most likely case is the (sibcall)
		 epilogue.  Put a special push/pop sequence for such case as
		 the last resort.  This looks lengthy but would not be problem
		 because it seems to be very rare.  */
	      gcc_assert (epilogue_p);

	      /* ??? There is still the slight possibility that r4 or
		  r5 have been reserved as fixed registers or assigned
		  as global registers, and they change during an
		  interrupt.  There are possible ways to handle this:
		     
		  - If we are adjusting the frame pointer (r14), we can do
		    with a single temp register and an ordinary push / pop
		    on the stack.
		  - Grab any call-used or call-saved registers (i.e. not
		    fixed or globals) for the temps we need.  We might
		    also grab r14 if we are adjusting the stack pointer.
		    If we can't find enough available registers, issue
		    a diagnostic and die - the user must have reserved
		    way too many registers.
		 But since all this is rather unlikely to happen and
		 would require extra testing, we just die if r4 / r5
		 are not available.  */
	      gcc_assert (!fixed_regs[4] && !fixed_regs[5]
			  && !global_regs[4] && !global_regs[5]);

	      adj_reg = gen_rtx_REG (GET_MODE (reg), 4);
	      tmp_reg = gen_rtx_REG (GET_MODE (reg), 5);
	      emit_move_insn (gen_tmp_stack_mem (Pmode, reg), adj_reg);
	      emit_insn (GEN_MOV (adj_reg, GEN_INT (size)));
	      emit_insn (GEN_ADD3 (adj_reg, adj_reg, reg));
	      mem = gen_tmp_stack_mem (Pmode, gen_rtx_PRE_DEC (Pmode, adj_reg));
	      emit_move_insn (mem, tmp_reg);
	      emit_move_insn (tmp_reg, gen_tmp_stack_mem (Pmode, reg));
	      mem = gen_tmp_stack_mem (Pmode, gen_rtx_PRE_DEC (Pmode, adj_reg));
	      emit_move_insn (mem, tmp_reg);
	      emit_move_insn (reg, adj_reg);
	      mem = gen_tmp_stack_mem (Pmode, gen_rtx_POST_INC (Pmode, reg));
	      emit_move_insn (adj_reg, mem);
	      mem = gen_tmp_stack_mem (Pmode, gen_rtx_POST_INC (Pmode, reg));
	      emit_move_insn (tmp_reg, mem);
	      /* Tell flow the insns that pop r4/r5 aren't dead.  */
	      emit_use (tmp_reg);
	      emit_use (adj_reg);
	      return;
	    }
	  const_reg = gen_rtx_REG (GET_MODE (reg), temp);

	  /* If SIZE is negative, subtract the positive value.
	     This sometimes allows a constant pool entry to be shared
	     between prologue and epilogue code.  */
	  if (size < 0)
	    {
	      emit_insn (GEN_MOV (const_reg, GEN_INT (-size)));
	      insn = emit_fn (GEN_SUB3 (reg, reg, const_reg));
	    }
	  else
	    {
	      emit_insn (GEN_MOV (const_reg, GEN_INT (size)));
	      insn = emit_fn (GEN_ADD3 (reg, reg, const_reg));
	    }
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (reg, gen_rtx_PLUS (SImode, reg,
							GEN_INT (size))));
	}
    }
}

/* Emit the specified insn and mark it as frame related.  */
static rtx_insn *
emit_frame_insn (rtx x)
{
  rtx_insn *insn = emit_insn (x);
  RTX_FRAME_RELATED_P (insn) = 1;
  return insn;
}

/* Output RTL to push register RN onto the stack.  */
static rtx
push (int rn)
{
  rtx x;
  if (rn == FPUL_REG)
    x = gen_push_fpul ();
  else if (rn == FPSCR_REG)
    x = gen_push_fpscr ();
  else if (TARGET_FPU_DOUBLE && TARGET_FMOVD
	   && ! TARGET_FPU_SINGLE && FP_OR_XD_REGISTER_P (rn))
    {
      if (FP_REGISTER_P (rn) && (rn - FIRST_FP_REG) & 1)
	return NULL_RTX;
      x = gen_push_4 (gen_rtx_REG (DFmode, rn));
    }
  else if (TARGET_SH2E && FP_REGISTER_P (rn))
    x = gen_push_e (gen_rtx_REG (SFmode, rn));
  else
    x = gen_push (gen_rtx_REG (SImode, rn));

  x = emit_frame_insn (x);
  add_reg_note (x, REG_INC, gen_rtx_REG (SImode, STACK_POINTER_REGNUM));
  return x;
}

/* Output RTL to pop register RN from the stack.  */
static void
pop (int rn)
{
  rtx x, sp_reg, reg;
  if (rn == FPUL_REG)
    x = gen_pop_fpul ();
  else if (rn == FPSCR_REG)
    x = gen_pop_fpscr ();
  else if (TARGET_FPU_DOUBLE && TARGET_FMOVD
	   && ! TARGET_FPU_SINGLE && FP_OR_XD_REGISTER_P (rn))
    {
      if (FP_REGISTER_P (rn) && (rn - FIRST_FP_REG) & 1)
	return;
      x = gen_pop_4 (gen_rtx_REG (DFmode, rn));
    }
  else if (TARGET_SH2E && FP_REGISTER_P (rn))
    x = gen_pop_e (gen_rtx_REG (SFmode, rn));
  else
    x = gen_pop (gen_rtx_REG (SImode, rn));

  x = emit_insn (x);

  sp_reg = gen_rtx_REG (SImode, STACK_POINTER_REGNUM);
  reg = copy_rtx (GET_CODE (PATTERN (x)) == PARALLEL
		  ? SET_DEST (XVECEXP (PATTERN (x), 0, 0))
		  : SET_DEST (PATTERN (x)));
  add_reg_note (x, REG_CFA_RESTORE, reg);
  add_reg_note (x, REG_CFA_ADJUST_CFA,
		gen_rtx_SET (sp_reg,
			     plus_constant (SImode, sp_reg,
					    GET_MODE_SIZE (GET_MODE (reg)))));
  add_reg_note (x, REG_INC, gen_rtx_REG (SImode, STACK_POINTER_REGNUM));
  RTX_FRAME_RELATED_P (x) = 1;
}

/* Generate code to push the regs specified in the mask.  */
static void
push_regs (HARD_REG_SET *mask, bool interrupt_handler)
{
  bool skip_fpscr = false;

  /* Push PR last; this gives better latencies after the prologue, and
     candidates for the return delay slot when there are no general
     registers pushed.  */
  for (int i = interrupt_handler ? LAST_BANKED_REG + 1 : 0;
       i < FIRST_PSEUDO_REGISTER; i++)
    {
      /* If this is an interrupt handler, and the SZ bit varies,
	 and we have to push any floating point register, we need
	 to switch to the correct precision first.  */
      if (i == FIRST_FP_REG && interrupt_handler && TARGET_FMOVD
	  && hard_reg_set_intersect_p (*mask, reg_class_contents[DF_REGS]))
	{
	  HARD_REG_SET unsaved;

	  push (FPSCR_REG);
	  COMPL_HARD_REG_SET (unsaved, *mask);
	  fpscr_set_from_mem (NORMAL_MODE (FP_MODE), unsaved);
	  skip_fpscr = true;
	}
      if (i != PR_REG
	  && (i != FPSCR_REG || ! skip_fpscr)
	  && TEST_HARD_REG_BIT (*mask, i))
	{
	/* If the ISR has RESBANK attribute assigned, don't push any of
	   the following registers - R0-R14, MACH, MACL and GBR.  */
      if (! (sh_cfun_resbank_handler_p ()
	     && ((i >= FIRST_GENERAL_REG && i < LAST_GENERAL_REG)
		 || i == MACH_REG
		 || i == MACL_REG
		 || i == GBR_REG)))
	  push (i);
	}
    }

  /* Push banked registers last to improve delay slot opportunities.  */
  if (interrupt_handler)
    {
      bool use_movml = false;

      if (TARGET_SH2A)
	{
	  unsigned int count = 0;

	  for (int i = FIRST_BANKED_REG; i <= LAST_BANKED_REG; i++)
	    if (TEST_HARD_REG_BIT (*mask, i))
	      count++;
	    else
	      break;

	  /* Use movml when all banked registers are pushed.  */
	  if (count == LAST_BANKED_REG - FIRST_BANKED_REG + 1)
	    use_movml = true;
	}

      if (sh_cfun_resbank_handler_p ())
	; /* Do nothing.  */
      else if (use_movml)
	{
	  rtx x, mem, reg, set;
	  rtx sp_reg = gen_rtx_REG (SImode, STACK_POINTER_REGNUM);

	  /* We must avoid scheduling multiple store insn with another
	     insns.  */
	  emit_insn (gen_blockage ());
	  x = gen_movml_push_banked (sp_reg);
	  x = emit_frame_insn (x);
	  for (int i = FIRST_BANKED_REG; i <= LAST_BANKED_REG; i++)
	    {
	      mem = gen_rtx_MEM (SImode, plus_constant (Pmode, sp_reg, i * 4));
	      reg = gen_rtx_REG (SImode, i);
	      add_reg_note (x, REG_CFA_OFFSET, gen_rtx_SET (mem, reg));
	    }

	  set = gen_rtx_SET (sp_reg, plus_constant (Pmode, sp_reg, - 32));
	  add_reg_note (x, REG_CFA_ADJUST_CFA, set);
	  emit_insn (gen_blockage ());
	}
      else
	for (int i = FIRST_BANKED_REG; i <= LAST_BANKED_REG; i++)
	  if (TEST_HARD_REG_BIT (*mask, i))
	    push (i);
    }

  /* Don't push PR register for an ISR with RESBANK attribute assigned.  */
  if (TEST_HARD_REG_BIT (*mask, PR_REG) && !sh_cfun_resbank_handler_p ())
    push (PR_REG);
}

/* Work out the registers which need to be saved, both as a mask and a
   count of saved words.  Return the count.

   If doing a pragma interrupt function, then push all regs used by the
   function, and if we call another function (we can tell by looking at PR),
   make sure that all the regs it clobbers are safe too.  */
static int
calc_live_regs (HARD_REG_SET *live_regs_mask)
{
  unsigned int reg;
  tree attrs;
  bool interrupt_or_trapa_handler, trapa_handler, interrupt_handler;
  bool nosave_low_regs;

  attrs = DECL_ATTRIBUTES (current_function_decl);
  interrupt_or_trapa_handler = sh_cfun_interrupt_handler_p ();
  trapa_handler = lookup_attribute ("trapa_handler", attrs) != NULL_TREE;
  interrupt_handler = interrupt_or_trapa_handler && ! trapa_handler;
  nosave_low_regs = lookup_attribute ("nosave_low_regs", attrs) != NULL_TREE;

  CLEAR_HARD_REG_SET (*live_regs_mask);
  if (TARGET_FPU_DOUBLE && TARGET_FMOVD && interrupt_handler
      && df_regs_ever_live_p (FPSCR_REG))
    target_flags &= ~MASK_FPU_SINGLE;
  /* If we can save a lot of saves by switching to double mode, do that.  */
  else if (TARGET_FPU_DOUBLE && TARGET_FMOVD && TARGET_FPU_SINGLE)
    for (int count = 0, reg = FIRST_FP_REG; reg <= LAST_FP_REG; reg += 2)
      if (df_regs_ever_live_p (reg) && df_regs_ever_live_p (reg+1)
	  && (! call_really_used_regs[reg]
	      || interrupt_handler)
	  && ++count > 2)
	{
	  target_flags &= ~MASK_FPU_SINGLE;
	  break;
	}


  rtx pr_initial = has_hard_reg_initial_val (Pmode, PR_REG);
  bool pr_live = (pr_initial
		 ? (!REG_P (pr_initial)
		    || REGNO (pr_initial) != (PR_REG))
		 : df_regs_ever_live_p (PR_REG));
  /* For Shcompact, if not optimizing, we end up with a memory reference
     using the return address pointer for __builtin_return_address even
     though there is no actual need to put the PR register on the stack.  */
  pr_live |= df_regs_ever_live_p (RETURN_ADDRESS_POINTER_REGNUM);

  /* Force PR to be live if the prologue has to call the SHmedia
     argument decoder or register saver.  */
  bool has_call = pr_live;

  int count;
  for (count = 0, reg = FIRST_PSEUDO_REGISTER; reg-- != 0; )
    {
      if (reg == PR_REG
	  ? pr_live
	  : interrupt_handler
	  ? (/* Need to save all the regs ever live.  */
	     (df_regs_ever_live_p (reg)
	      || (call_really_used_regs[reg]
		  && (! fixed_regs[reg] || reg == MACH_REG || reg == MACL_REG
		      || reg == PIC_OFFSET_TABLE_REGNUM)
		  && has_call))
	     && reg != STACK_POINTER_REGNUM && reg != ARG_POINTER_REGNUM
	     && reg != RETURN_ADDRESS_POINTER_REGNUM
	     && reg != T_REG && reg != GBR_REG
	     && reg != FPSCR_MODES_REG && reg != FPSCR_STAT_REG
	     /* Push fpscr only on targets which have FPU */
	     && (reg != FPSCR_REG || TARGET_FPU_ANY))
	  : (/* Only push those regs which are used and need to be saved.  */
	     (false)
	     || (df_regs_ever_live_p (reg)
		 && ((!call_really_used_regs[reg]
		      && !(reg != PIC_OFFSET_TABLE_REGNUM
			   && fixed_regs[reg] && call_used_regs[reg]))
		     || (trapa_handler && reg == FPSCR_REG && TARGET_FPU_ANY)))
	     || (crtl->calls_eh_return
		 && (reg == EH_RETURN_DATA_REGNO (0)
		     || reg == EH_RETURN_DATA_REGNO (1)
		     || reg == EH_RETURN_DATA_REGNO (2)
		     || reg == EH_RETURN_DATA_REGNO (3)))
	     || ((reg == MACL_REG || reg == MACH_REG)
		 && df_regs_ever_live_p (reg)
		 && sh_cfun_attr_renesas_p ())
	     ))
	{
	  SET_HARD_REG_BIT (*live_regs_mask, reg);
	  count += GET_MODE_SIZE (REGISTER_NATURAL_MODE (reg));

	  if (TARGET_FPU_DOUBLE && TARGET_FMOVD
	      && GET_MODE_CLASS (REGISTER_NATURAL_MODE (reg)) == MODE_FLOAT)
	    {
	      if (FP_REGISTER_P (reg))
		{
		  if (! TARGET_FPU_SINGLE && ! df_regs_ever_live_p (reg ^ 1))
		    {
		      SET_HARD_REG_BIT (*live_regs_mask, (reg ^ 1));
		      count += GET_MODE_SIZE (REGISTER_NATURAL_MODE (reg ^ 1));
		    }
		}
	      else if (XD_REGISTER_P (reg))
		{
		  /* Must switch to double mode to access these registers.  */
		  target_flags &= ~MASK_FPU_SINGLE;
		}
	    }
	}
      if (nosave_low_regs && reg == R8_REG)
	break;
    }

  return count;
}

/* Code to generate prologue and epilogue sequences */

/* PUSHED is the number of bytes that are being pushed on the
   stack for register saves.  Return the frame size, padded
   appropriately so that the stack stays properly aligned.  */
static HOST_WIDE_INT
rounded_frame_size (int pushed)
{
  HOST_WIDE_INT size = get_frame_size ();
  HOST_WIDE_INT align = STACK_BOUNDARY / BITS_PER_UNIT;

  if (ACCUMULATE_OUTGOING_ARGS)
    size += crtl->outgoing_args_size;

  return ((size + pushed + align - 1) & -align) - pushed;
}

/* Expand code for the function prologue.  */
void
sh_expand_prologue (void)
{
  int save_flags = target_flags;
  tree sp_switch_attr
    = lookup_attribute ("sp_switch", DECL_ATTRIBUTES (current_function_decl));

  current_function_interrupt = sh_cfun_interrupt_handler_p ();

  /* We have pretend args if we had an object sent partially in registers
     and partially on the stack, e.g. a large structure.  */
  int pretend_args = crtl->args.pretend_args_size;
  if (TARGET_VARARGS_PRETEND_ARGS (current_function_decl)
      && (NPARM_REGS(SImode)
	  > crtl->args.info.arg_count[(int) SH_ARG_INT]))
    pretend_args = 0;

  output_stack_adjust (-pretend_args, stack_pointer_rtx, 0, NULL, true);
  int stack_usage = pretend_args;

  /* Emit the code for SETUP_VARARGS.  */
  if (cfun->stdarg)
    {
      if (TARGET_VARARGS_PRETEND_ARGS (current_function_decl))
	{
	  /* Push arg regs as if they'd been provided by caller in stack.  */
	  for (int i = 0; i < NPARM_REGS(SImode); i++)
	    {
	      int rn = NPARM_REGS(SImode) + FIRST_PARM_REG - i - 1;

	      if (i >= (NPARM_REGS(SImode)
			- crtl->args.info.arg_count[(int) SH_ARG_INT]
			))
		break;
	      push (rn);
	      stack_usage += GET_MODE_SIZE (SImode);
	    }
	}
    }

  /* If we're supposed to switch stacks at function entry, do so now.  */
  if (sp_switch_attr)
    {
      rtx lab, newsrc;
      /* The argument specifies a variable holding the address of the
	 stack the interrupt function should switch to/from at entry/exit.  */
      tree arg = TREE_VALUE ( TREE_VALUE (sp_switch_attr));
      const char* s = ggc_strdup (TREE_STRING_POINTER (arg));
      rtx sp_switch = gen_rtx_SYMBOL_REF (Pmode, s);

      lab = add_constant (sp_switch, SImode, 0);
      newsrc = gen_rtx_LABEL_REF (VOIDmode, lab);

      emit_insn (gen_sp_switch_1 (newsrc));
    }

  HARD_REG_SET live_regs_mask;
  int d = calc_live_regs (&live_regs_mask);
  /* ??? Maybe we could save some switching if we can move a mode switch
     that already happens to be at the function start into the prologue.  */
  if (target_flags != save_flags && ! current_function_interrupt)
    emit_insn (gen_toggle_sz ());

  push_regs (&live_regs_mask, current_function_interrupt);
  stack_usage += d;

  if (flag_pic && !TARGET_FDPIC
      && df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM))
    emit_insn (gen_GOTaddr2picreg (const0_rtx));

  if (target_flags != save_flags && ! current_function_interrupt)
    emit_insn (gen_toggle_sz ());

  target_flags = save_flags;

  output_stack_adjust (-rounded_frame_size (d),
		       stack_pointer_rtx, 0, NULL, true);
  stack_usage += rounded_frame_size (d);

  if (frame_pointer_needed)
    emit_frame_insn (GEN_MOV (hard_frame_pointer_rtx, stack_pointer_rtx));

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  Similarly if some call instructions are swapped
     before frame related insns, it'll confuse the unwinder because
     currently SH has no unwind info for function epilogues.  */
  if (crtl->profile || flag_exceptions || flag_unwind_tables)
    emit_insn (gen_blockage ());

  if (flag_stack_usage_info)
    current_function_static_stack_size = stack_usage;
}

/* Expand code for the function epilogue.  */
void
sh_expand_epilogue (bool sibcall_p)
{
  int save_flags = target_flags;
  bool fpscr_deferred = false;
  int e = sibcall_p ? -1 : 1;

  HARD_REG_SET live_regs_mask;
  int d = calc_live_regs (&live_regs_mask);

  int save_size = d;
  int frame_size = rounded_frame_size (d);

  if (frame_pointer_needed)
    {
      /* We must avoid scheduling the epilogue with previous basic blocks.
	 See PR/18032 and PR/40313.  */
      emit_insn (gen_blockage ());
      output_stack_adjust (frame_size, hard_frame_pointer_rtx, e,
			   &live_regs_mask, true);

      /* We must avoid moving the stack pointer adjustment past code
	 which reads from the local frame, else an interrupt could
	 occur after the SP adjustment and clobber data in the local
	 frame.  */
      emit_insn (gen_blockage ());
      emit_frame_insn (GEN_MOV (stack_pointer_rtx, hard_frame_pointer_rtx));
    }
  else if (frame_size)
    {
      /* We must avoid moving the stack pointer adjustment past code
	 which reads from the local frame, else an interrupt could
	 occur after the SP adjustment and clobber data in the local
	 frame.  */
      emit_insn (gen_blockage ());
      output_stack_adjust (frame_size, stack_pointer_rtx, e,
			   &live_regs_mask, true);
    }

  /* Pop all the registers.  */

  if (target_flags != save_flags && ! current_function_interrupt)
    emit_insn (gen_toggle_sz ());

    {
      int last_reg;

      save_size = 0;
	/* For an ISR with RESBANK attribute assigned, don't pop PR
	   register.  */
      if (TEST_HARD_REG_BIT (live_regs_mask, PR_REG)
	  && !sh_cfun_resbank_handler_p ())	
	{
	  if (!frame_pointer_needed)
	    emit_insn (gen_blockage ());
	  pop (PR_REG);
	}

      /* Banked registers are popped first to avoid being scheduled in the
	 delay slot. RTE switches banks before the ds instruction.  */
      if (current_function_interrupt)
	{
	  bool use_movml = false;

	  if (TARGET_SH2A)
	    {
	      unsigned int count = 0;

	      for (int i = FIRST_BANKED_REG; i <= LAST_BANKED_REG; i++)
		if (TEST_HARD_REG_BIT (live_regs_mask, i))
		  count++;
		else
		  break;

	      /* Use movml when all banked register are poped.  */
	      if (count == LAST_BANKED_REG - FIRST_BANKED_REG + 1)
		use_movml = true;
	    }

	  if (sh_cfun_resbank_handler_p ())
	    ; /* Do nothing.  */
	  else if (use_movml)
	    {
	      rtx sp_reg = gen_rtx_REG (SImode, STACK_POINTER_REGNUM);

	      /* We must avoid scheduling multiple load insn with another
		 insns.  */
	      emit_insn (gen_blockage ());
	      emit_insn (gen_movml_pop_banked (sp_reg));
	      emit_insn (gen_blockage ());
	    }
	  else
	    for (int i = LAST_BANKED_REG; i >= FIRST_BANKED_REG; i--)
	      if (TEST_HARD_REG_BIT (live_regs_mask, i))
		pop (i);

	  last_reg = FIRST_PSEUDO_REGISTER - LAST_BANKED_REG - 1;
	}
      else
	last_reg = FIRST_PSEUDO_REGISTER;

      for (int i = 0; i < last_reg; i++)
	{
	  int j = (FIRST_PSEUDO_REGISTER - 1) - i;

	  if (j == FPSCR_REG && current_function_interrupt && TARGET_FMOVD
	      && hard_reg_set_intersect_p (live_regs_mask,
					  reg_class_contents[DF_REGS]))
	    fpscr_deferred = true;
	  /* For an ISR with RESBANK attribute assigned, don't pop
	     following registers, R0-R14, MACH, MACL and GBR.  */
	  else if (j != PR_REG && TEST_HARD_REG_BIT (live_regs_mask, j) 
		   && ! (sh_cfun_resbank_handler_p ()
			 && ((j >= FIRST_GENERAL_REG
			      && j < LAST_GENERAL_REG)
			      || j == MACH_REG
			      || j == MACL_REG
			      || j == GBR_REG)))
	    pop (j);

	  if (j == FIRST_FP_REG && fpscr_deferred)
	    pop (FPSCR_REG);
	}
    }
  if (target_flags != save_flags && ! current_function_interrupt)
    emit_insn (gen_toggle_sz ());
  target_flags = save_flags;

  output_stack_adjust (crtl->args.pretend_args_size + save_size,
		       stack_pointer_rtx, e, NULL, true);

  if (crtl->calls_eh_return)
    emit_insn (GEN_ADD3 (stack_pointer_rtx, stack_pointer_rtx,
			 EH_RETURN_STACKADJ_RTX));

  /* Switch back to the normal stack if necessary.  */
  if (lookup_attribute ("sp_switch", DECL_ATTRIBUTES (current_function_decl)))
    emit_insn (gen_sp_switch_2 ());

  /* Tell flow the insn that pops PR isn't dead.  */
  if (TEST_HARD_REG_BIT (live_regs_mask, PR_REG))
    emit_use (gen_rtx_REG (SImode, PR_REG));
}

/* Emit code to change the current function's return address to RA.
   TEMP is available as a scratch register, if needed.  */
void
sh_set_return_address (rtx ra, rtx tmp)
{
  HARD_REG_SET live_regs_mask;
  int d = calc_live_regs (&live_regs_mask);

  /* If pr_reg isn't life, we can set it directly.  */
  if (! TEST_HARD_REG_BIT (live_regs_mask, PR_REG))
    {
      rtx rr = gen_rtx_REG (SImode, PR_REG);
      emit_insn (GEN_MOV (rr, ra));
      /* Tell flow the register for return isn't dead.  */
      emit_use (rr);
      return;
    }

  int pr_offset = rounded_frame_size (d);

  emit_insn (GEN_MOV (tmp, GEN_INT (pr_offset)));

  if (frame_pointer_needed)
    emit_insn (GEN_ADD3 (tmp, tmp, hard_frame_pointer_rtx));
  else
    emit_insn (GEN_ADD3 (tmp, tmp, stack_pointer_rtx));

  tmp = gen_frame_mem (Pmode, tmp);
  emit_insn (GEN_MOV (tmp, ra));
  /* Tell this store isn't dead.  */
  emit_use (tmp);
}

/* Clear variables at function end.  */
static void
sh_output_function_epilogue (FILE *file ATTRIBUTE_UNUSED,
			     HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
}

static rtx
sh_builtin_saveregs (void)
{
  /* First unnamed integer register.  */
  int first_intreg = crtl->args.info.arg_count[(int) SH_ARG_INT];
  /* Number of integer registers we need to save.  */
  int n_intregs = MAX (0, NPARM_REGS (SImode) - first_intreg);
  /* First unnamed SFmode float reg */
  int first_floatreg = crtl->args.info.arg_count[(int) SH_ARG_FLOAT];
  /* Number of SFmode float regs to save.  */
  int n_floatregs = MAX (0, NPARM_REGS (SFmode) - first_floatreg);
  rtx regbuf, fpregs;
  int bufsize, regno;
  alias_set_type alias_set;

  if (!TARGET_FPU_ANY)
    {
      error ("__builtin_saveregs not supported by this subtarget");
      return const0_rtx;
    }

  /* Allocate block of memory for the regs.  */
  /* ??? If n_intregs + n_floatregs == 0, should we allocate at least 1 byte?
     Or can assign_stack_local accept a 0 SIZE argument?  */
  bufsize = (n_intregs * UNITS_PER_WORD) + (n_floatregs * UNITS_PER_WORD);

  if (n_floatregs & 1)
    {
      rtx addr;

      regbuf = assign_stack_local (BLKmode, bufsize + UNITS_PER_WORD, 0);
      addr = copy_to_mode_reg (Pmode, XEXP (regbuf, 0));
      emit_insn (gen_iorsi3 (addr, addr, GEN_INT (UNITS_PER_WORD)));
      regbuf = change_address (regbuf, BLKmode, addr);
    }
  else if (STACK_BOUNDARY < 64 && TARGET_FPU_DOUBLE && n_floatregs)
    {
      rtx addr, mask;

      regbuf = assign_stack_local (BLKmode, bufsize + UNITS_PER_WORD, 0);
      addr = copy_to_mode_reg (Pmode, plus_constant (Pmode,
						     XEXP (regbuf, 0), 4));
      mask = copy_to_mode_reg (Pmode, GEN_INT (-8));
      emit_insn (gen_andsi3 (addr, addr, mask));
      regbuf = change_address (regbuf, BLKmode, addr);
    }
  else
    regbuf = assign_stack_local (BLKmode, bufsize, TARGET_FPU_DOUBLE ? 64 : 0);
  alias_set = get_varargs_alias_set ();
  set_mem_alias_set (regbuf, alias_set);

  /* Save int args.
     This is optimized to only save the regs that are necessary.  Explicitly
     named args need not be saved.  */
  if (n_intregs > 0)
    move_block_from_reg (BASE_ARG_REG (SImode) + first_intreg,
			 adjust_address (regbuf, BLKmode,
					 n_floatregs * UNITS_PER_WORD),
			 n_intregs);

  /* Save float args.
     This is optimized to only save the regs that are necessary.  Explicitly
     named args need not be saved.
     We explicitly build a pointer to the buffer because it halves the insn
     count when not optimizing (otherwise the pointer is built for each reg
     saved).
     We emit the moves in reverse order so that we can use predecrement.  */

  fpregs = copy_to_mode_reg (Pmode,
			     plus_constant (Pmode, XEXP (regbuf, 0),
					    n_floatregs * UNITS_PER_WORD));
  if (TARGET_FPU_DOUBLE)
    {
      rtx mem;
      for (regno = NPARM_REGS (DFmode) - 2; regno >= first_floatreg; regno -= 2)
	{
	  emit_insn (gen_addsi3 (fpregs, fpregs,
				 GEN_INT (-2 * UNITS_PER_WORD)));
	  mem = change_address (regbuf, DFmode, fpregs);
	  emit_move_insn (mem,
			  gen_rtx_REG (DFmode, BASE_ARG_REG (DFmode) + regno));
	}
      regno = first_floatreg;
      if (regno & 1)
	{
	  emit_insn (gen_addsi3 (fpregs, fpregs, GEN_INT (-UNITS_PER_WORD)));
	  mem = change_address (regbuf, SFmode, fpregs);
	  emit_move_insn (mem,
			  gen_rtx_REG (SFmode, BASE_ARG_REG (SFmode)
					       + regno - SH_REG_MSW_OFFSET));
	}
    }
  else
    for (regno = NPARM_REGS (SFmode) - 1; regno >= first_floatreg; regno--)
      {
        rtx mem;

	emit_insn (gen_addsi3 (fpregs, fpregs, GEN_INT (-UNITS_PER_WORD)));
	mem = change_address (regbuf, SFmode, fpregs);
	emit_move_insn (mem,
			gen_rtx_REG (SFmode, BASE_ARG_REG (SFmode) + regno));
      }

  /* Return the address of the regbuf.  */
  return XEXP (regbuf, 0);
}

/* Define the `__builtin_va_list' type for the ABI.  */
static tree
sh_build_builtin_va_list (void)
{
  tree f_next_o, f_next_o_limit, f_next_fp, f_next_fp_limit, f_next_stack;
  tree record, type_decl;

  if ((! TARGET_SH2E && ! TARGET_SH4)
      || TARGET_HITACHI || sh_cfun_attr_renesas_p ())
    return ptr_type_node;

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  type_decl = build_decl (BUILTINS_LOCATION,
			  TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_next_o = build_decl (BUILTINS_LOCATION,
			 FIELD_DECL, get_identifier ("__va_next_o"),
			 ptr_type_node);
  f_next_o_limit = build_decl (BUILTINS_LOCATION,
			       FIELD_DECL,
			       get_identifier ("__va_next_o_limit"),
			       ptr_type_node);
  f_next_fp = build_decl (BUILTINS_LOCATION,
			  FIELD_DECL, get_identifier ("__va_next_fp"),
			  ptr_type_node);
  f_next_fp_limit = build_decl (BUILTINS_LOCATION,
				FIELD_DECL,
				get_identifier ("__va_next_fp_limit"),
				ptr_type_node);
  f_next_stack = build_decl (BUILTINS_LOCATION,
			     FIELD_DECL, get_identifier ("__va_next_stack"),
			     ptr_type_node);

  DECL_FIELD_CONTEXT (f_next_o) = record;
  DECL_FIELD_CONTEXT (f_next_o_limit) = record;
  DECL_FIELD_CONTEXT (f_next_fp) = record;
  DECL_FIELD_CONTEXT (f_next_fp_limit) = record;
  DECL_FIELD_CONTEXT (f_next_stack) = record;

  TYPE_STUB_DECL (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_next_o;
  DECL_CHAIN (f_next_o) = f_next_o_limit;
  DECL_CHAIN (f_next_o_limit) = f_next_fp;
  DECL_CHAIN (f_next_fp) = f_next_fp_limit;
  DECL_CHAIN (f_next_fp_limit) = f_next_stack;

  layout_type (record);

  return record;
}

/* Implement `va_start' for varargs and stdarg.  */
static void
sh_va_start (tree valist, rtx nextarg)
{
  tree f_next_o, f_next_o_limit, f_next_fp, f_next_fp_limit, f_next_stack;
  tree next_o, next_o_limit, next_fp, next_fp_limit, next_stack;
  tree t, u;
  int nfp, nint;

  if ((! TARGET_SH2E && ! TARGET_SH4)
      || TARGET_HITACHI || sh_cfun_attr_renesas_p ())
    {
      std_expand_builtin_va_start (valist, nextarg);
      return;
    }

  f_next_o = TYPE_FIELDS (va_list_type_node);
  f_next_o_limit = DECL_CHAIN (f_next_o);
  f_next_fp = DECL_CHAIN (f_next_o_limit);
  f_next_fp_limit = DECL_CHAIN (f_next_fp);
  f_next_stack = DECL_CHAIN (f_next_fp_limit);

  next_o = build3 (COMPONENT_REF, TREE_TYPE (f_next_o), valist, f_next_o,
		   NULL_TREE);
  next_o_limit = build3 (COMPONENT_REF, TREE_TYPE (f_next_o_limit),
			 valist, f_next_o_limit, NULL_TREE);
  next_fp = build3 (COMPONENT_REF, TREE_TYPE (f_next_fp), valist, f_next_fp,
		    NULL_TREE);
  next_fp_limit = build3 (COMPONENT_REF, TREE_TYPE (f_next_fp_limit),
			  valist, f_next_fp_limit, NULL_TREE);
  next_stack = build3 (COMPONENT_REF, TREE_TYPE (f_next_stack),
		       valist, f_next_stack, NULL_TREE);

  /* Call __builtin_saveregs.  */
  u = make_tree (sizetype, expand_builtin_saveregs ());
  u = fold_convert (ptr_type_node, u);
  t = build2 (MODIFY_EXPR, ptr_type_node, next_fp, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  nfp = crtl->args.info.arg_count[SH_ARG_FLOAT];
  if (nfp < 8)
    nfp = 8 - nfp;
  else
    nfp = 0;
  u = fold_build_pointer_plus_hwi (u, UNITS_PER_WORD * nfp);
  t = build2 (MODIFY_EXPR, ptr_type_node, next_fp_limit, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = build2 (MODIFY_EXPR, ptr_type_node, next_o, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  nint = crtl->args.info.arg_count[SH_ARG_INT];
  if (nint < 4)
    nint = 4 - nint;
  else
    nint = 0;
  u = fold_build_pointer_plus_hwi (u, UNITS_PER_WORD * nint);
  t = build2 (MODIFY_EXPR, ptr_type_node, next_o_limit, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  u = make_tree (ptr_type_node, nextarg);
  t = build2 (MODIFY_EXPR, ptr_type_node, next_stack, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* TYPE is a RECORD_TYPE.  If there is only a single nonzero-sized
   member, return it.  */
static tree
find_sole_member (tree type)
{
  tree field, member = NULL_TREE;

  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) != FIELD_DECL)
	continue;
      if (!DECL_SIZE (field))
	return NULL_TREE;
      if (integer_zerop (DECL_SIZE (field)))
	continue;
      if (member)
	return NULL_TREE;
      member = field;
    }
  return member;
}

/* Implement `va_arg'.  */
static tree
sh_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			 gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree tmp;
  tree addr, lab_over = NULL, result = NULL;
  tree eff_type;

  const bool pass_by_ref =
    !VOID_TYPE_P (type)
    && targetm.calls.must_pass_in_stack (TYPE_MODE (type), type);

  if (pass_by_ref)
    type = build_pointer_type (type);

  HOST_WIDE_INT size = int_size_in_bytes (type);
  HOST_WIDE_INT rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;
  tree pptr_type_node = build_pointer_type (ptr_type_node);

  if ((TARGET_SH2E || TARGET_SH4)
      && ! (TARGET_HITACHI || sh_cfun_attr_renesas_p ()))
    {
      tree f_next_o, f_next_o_limit, f_next_fp, f_next_fp_limit, f_next_stack;
      tree next_o, next_o_limit, next_fp, next_fp_limit, next_stack;
      tree lab_false;
      tree member;

      f_next_o = TYPE_FIELDS (va_list_type_node);
      f_next_o_limit = DECL_CHAIN (f_next_o);
      f_next_fp = DECL_CHAIN (f_next_o_limit);
      f_next_fp_limit = DECL_CHAIN (f_next_fp);
      f_next_stack = DECL_CHAIN (f_next_fp_limit);

      next_o = build3 (COMPONENT_REF, TREE_TYPE (f_next_o), valist, f_next_o,
		       NULL_TREE);
      next_o_limit = build3 (COMPONENT_REF, TREE_TYPE (f_next_o_limit),
			     valist, f_next_o_limit, NULL_TREE);
      next_fp = build3 (COMPONENT_REF, TREE_TYPE (f_next_fp),
			valist, f_next_fp, NULL_TREE);
      next_fp_limit = build3 (COMPONENT_REF, TREE_TYPE (f_next_fp_limit),
			      valist, f_next_fp_limit, NULL_TREE);
      next_stack = build3 (COMPONENT_REF, TREE_TYPE (f_next_stack),
			   valist, f_next_stack, NULL_TREE);

      /* Structures with a single member with a distinct mode are passed
	 like their member.  This is relevant if the latter has a REAL_TYPE
	 or COMPLEX_TYPE type.  */
      eff_type = type;
      while (TREE_CODE (eff_type) == RECORD_TYPE
	     && (member = find_sole_member (eff_type))
	     && (TREE_CODE (TREE_TYPE (member)) == REAL_TYPE
		 || TREE_CODE (TREE_TYPE (member)) == COMPLEX_TYPE
		 || TREE_CODE (TREE_TYPE (member)) == RECORD_TYPE))
	{
	  tree field_type = TREE_TYPE (member);

	  if (TYPE_MODE (eff_type) == TYPE_MODE (field_type))
	    eff_type = field_type;
	  else
	    {
	      gcc_assert ((TYPE_ALIGN (eff_type)
			   < GET_MODE_ALIGNMENT (TYPE_MODE (field_type)))
			  || (TYPE_ALIGN (eff_type)
			      > GET_MODE_BITSIZE (TYPE_MODE (field_type))));
	      break;
	    }
	}

      bool pass_as_float;
      if (TARGET_FPU_DOUBLE)
	{
	  pass_as_float = ((TREE_CODE (eff_type) == REAL_TYPE && size <= 8)
			   || (TREE_CODE (eff_type) == COMPLEX_TYPE
			       && TREE_CODE (TREE_TYPE (eff_type)) == REAL_TYPE
			       && size <= 16));
	}
      else
	{
	  pass_as_float = (TREE_CODE (eff_type) == REAL_TYPE && size == 4);
	}

      addr = create_tmp_var (pptr_type_node);
      lab_false = create_artificial_label (UNKNOWN_LOCATION);
      lab_over = create_artificial_label (UNKNOWN_LOCATION);

      valist = build_simple_mem_ref (addr);

      if (pass_as_float)
	{
	  tree next_fp_tmp = create_tmp_var (TREE_TYPE (f_next_fp));
	  tree cmp;
	  bool is_double = size == 8 && TREE_CODE (eff_type) == REAL_TYPE;

	  tmp = build1 (ADDR_EXPR, pptr_type_node, unshare_expr (next_fp));
	  gimplify_assign (unshare_expr (addr), tmp, pre_p);

	  gimplify_assign (unshare_expr (next_fp_tmp), valist, pre_p);
	  tmp = next_fp_limit;
	  if (size > 4 && !is_double)
	    tmp = fold_build_pointer_plus_hwi (unshare_expr (tmp), 4 - size);
	  tmp = build2 (GE_EXPR, boolean_type_node,
			unshare_expr (next_fp_tmp), unshare_expr (tmp));
	  cmp = build3 (COND_EXPR, void_type_node, tmp,
		        build1 (GOTO_EXPR, void_type_node,
				unshare_expr (lab_false)), NULL_TREE);
	  if (!is_double)
	    gimplify_and_add (cmp, pre_p);

	  if (TYPE_ALIGN (eff_type) > BITS_PER_WORD
	      || (is_double || size == 16))
	    {
	      tmp = fold_convert (sizetype, next_fp_tmp);
	      tmp = build2 (BIT_AND_EXPR, sizetype, tmp,
			    size_int (UNITS_PER_WORD));
	      tmp = fold_build_pointer_plus (unshare_expr (next_fp_tmp), tmp);
	      gimplify_assign (unshare_expr (next_fp_tmp), tmp, pre_p);
	    }
	  if (is_double)
	    gimplify_and_add (cmp, pre_p);

#ifdef FUNCTION_ARG_SCmode_WART
	  if (TYPE_MODE (eff_type) == SCmode
	      && TARGET_SH4 && TARGET_LITTLE_ENDIAN)
	    {
	      tree subtype = TREE_TYPE (eff_type);
	      tree real, imag;

	      imag
		= std_gimplify_va_arg_expr (next_fp_tmp, subtype, pre_p, NULL);
	      imag = get_initialized_tmp_var (imag, pre_p, NULL);

	      real
		= std_gimplify_va_arg_expr (next_fp_tmp, subtype, pre_p, NULL);
	      real = get_initialized_tmp_var (real, pre_p, NULL);

	      result = build2 (COMPLEX_EXPR, eff_type, real, imag);
	      if (type != eff_type)
		result = build1 (VIEW_CONVERT_EXPR, type, result);
	      result = get_initialized_tmp_var (result, pre_p, NULL);
	    }
#endif /* FUNCTION_ARG_SCmode_WART */

	  tmp = build1 (GOTO_EXPR, void_type_node, unshare_expr (lab_over));
	  gimplify_and_add (tmp, pre_p);

	  tmp = build1 (LABEL_EXPR, void_type_node, unshare_expr (lab_false));
	  gimplify_and_add (tmp, pre_p);

	  tmp = build1 (ADDR_EXPR, pptr_type_node, unshare_expr (next_stack));
	  gimplify_assign (unshare_expr (addr), tmp, pre_p);
	  gimplify_assign (unshare_expr (next_fp_tmp),
			   unshare_expr (valist), pre_p);

	  gimplify_assign (unshare_expr (valist),
			   unshare_expr (next_fp_tmp), post_p);
	  valist = next_fp_tmp;
	}
      else
	{
	  tmp = fold_build_pointer_plus_hwi (unshare_expr (next_o), rsize);
	  tmp = build2 (GT_EXPR, boolean_type_node, tmp,
			unshare_expr (next_o_limit));
	  tmp = build3 (COND_EXPR, void_type_node, tmp,
		        build1 (GOTO_EXPR, void_type_node,
				unshare_expr (lab_false)),
			NULL_TREE);
	  gimplify_and_add (tmp, pre_p);

	  tmp = build1 (ADDR_EXPR, pptr_type_node, unshare_expr (next_o));
	  gimplify_assign (unshare_expr (addr), tmp, pre_p);

	  tmp = build1 (GOTO_EXPR, void_type_node, unshare_expr (lab_over));
	  gimplify_and_add (tmp, pre_p);

	  tmp = build1 (LABEL_EXPR, void_type_node, unshare_expr (lab_false));
	  gimplify_and_add (tmp, pre_p);

	  if (size > 4 && ! (TARGET_SH4 || TARGET_SH2A))
	    gimplify_assign (unshare_expr (next_o),
			     unshare_expr (next_o_limit), pre_p);

	  tmp = build1 (ADDR_EXPR, pptr_type_node, unshare_expr (next_stack));
	  gimplify_assign (unshare_expr (addr), tmp, pre_p);
	}

      if (!result)
	{
	  tmp = build1 (LABEL_EXPR, void_type_node, unshare_expr (lab_over));
	  gimplify_and_add (tmp, pre_p);
	}
    }

  /* ??? In va-sh.h, there had been code to make values larger than
     size 8 indirect.  This does not match the FUNCTION_ARG macros.  */

  tmp = std_gimplify_va_arg_expr (valist, type, pre_p, NULL);
  if (result)
    {
      gimplify_assign (result, tmp, pre_p);
      result = build1 (NOP_EXPR, TREE_TYPE (result), result);
      tmp = build1 (LABEL_EXPR, void_type_node, unshare_expr (lab_over));
      gimplify_and_add (tmp, pre_p);
    }
  else
    result = tmp;

  if (pass_by_ref)
    result = build_va_arg_indirect_ref (result);

  return result;
}

/* 64 bit floating points memory transfers are paired single precision loads
   or store.  So DWARF information needs fixing in little endian (unless
   PR=SZ=1 in FPSCR).  */
rtx
sh_dwarf_register_span (rtx reg)
{
  unsigned regno = REGNO (reg);

  if (WORDS_BIG_ENDIAN || GET_MODE (reg) != DFmode)
    return NULL_RTX;

  return
    gen_rtx_PARALLEL (VOIDmode,
		      gen_rtvec (2,
				 gen_rtx_REG (SFmode, regno + 1),
				 gen_rtx_REG (SFmode, regno)));
}

static machine_mode
sh_promote_function_mode (const_tree type, machine_mode mode,
			  int *punsignedp, const_tree funtype,
			  int for_return)
{
  if (sh_promote_prototypes (funtype))
    return promote_mode (type, mode, punsignedp);
  else
    return default_promote_function_mode (type, mode, punsignedp, funtype,
					  for_return);
}

static bool
sh_promote_prototypes (const_tree type)
{
  if (TARGET_HITACHI)
    return false;
  if (! type)
    return true;
  return ! sh_attr_renesas_p (type);
}

static bool
sh_pass_by_reference (cumulative_args_t cum_v, machine_mode mode,
		      const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (targetm.calls.must_pass_in_stack (mode, type))
    return true;

  /* ??? std_gimplify_va_arg_expr passes NULL for cum.  That function
     wants to know about pass-by-reference semantics for incoming
     arguments.  */
  if (! cum)
    return false;

  return false;
}

static bool
sh_callee_copies (cumulative_args_t cum, machine_mode mode,
		  const_tree type, bool named ATTRIBUTE_UNUSED)
{
  /* ??? How can it possibly be correct to return true only on the
     caller side of the equation?  Is there someplace else in the
     sh backend that's magically producing the copies?  */
  return (get_cumulative_args (cum)->outgoing
	  && ((mode == BLKmode ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode))
	      % SH_MIN_ALIGN_FOR_CALLEE_COPY == 0));
}

static sh_arg_class
get_sh_arg_class (machine_mode mode)
{
  if (TARGET_FPU_ANY && mode == SFmode)
    return SH_ARG_FLOAT;

  if (TARGET_FPU_DOUBLE
      && (GET_MODE_CLASS (mode) == MODE_FLOAT
	  || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT))
    return SH_ARG_FLOAT;

  return SH_ARG_INT;
}

/* Round a register number up to a proper boundary for an arg of mode
   MODE.
   The SH doesn't care about double alignment, so we only
   round doubles to even regs when asked to explicitly.  */
static int
sh_round_reg (const CUMULATIVE_ARGS& cum, machine_mode mode)
{
  /* FIXME: This used to be a macro and has been copy pasted into this
     function as is.  Make this more readable.  */
  return
  (((TARGET_ALIGN_DOUBLE
      || (TARGET_FPU_DOUBLE
	  && (mode == DFmode || mode == DCmode)
	  && cum.arg_count[(int) SH_ARG_FLOAT] < NPARM_REGS (mode)))
     && GET_MODE_UNIT_SIZE (mode) > UNITS_PER_WORD)
    ? (cum.arg_count[(int) get_sh_arg_class (mode)]
       + (cum.arg_count[(int) get_sh_arg_class (mode)] & 1))
    : cum.arg_count[(int) get_sh_arg_class (mode)]);
}

/* Return true if arg of the specified mode should be passed in a register
   or false otherwise.  */
static bool
sh_pass_in_reg_p (const CUMULATIVE_ARGS& cum, machine_mode mode,
		  const_tree type)
{
  /* FIXME: This used to be a macro and has been copy pasted into this
     function as is.  Make this more readable.  */
  return
  ((type == 0
    || (! TREE_ADDRESSABLE (type)
	&& (! (TARGET_HITACHI || cum.renesas_abi)
	    || ! (AGGREGATE_TYPE_P (type)
		  || (!TARGET_FPU_ANY
		      && (GET_MODE_CLASS (mode) == MODE_FLOAT
			  && GET_MODE_SIZE (mode) > GET_MODE_SIZE (SFmode)))))))
   && ! cum.force_mem
   && (TARGET_SH2E
       ? ((mode) == BLKmode
	  ? ((cum.arg_count[(int) SH_ARG_INT] * UNITS_PER_WORD
	      + int_size_in_bytes (type))
	     <= NPARM_REGS (SImode) * UNITS_PER_WORD)
	  : ((sh_round_reg (cum, mode)
	      + HARD_REGNO_NREGS (BASE_ARG_REG (mode), mode))
	     <= NPARM_REGS (mode)))
       : sh_round_reg (cum, mode) < NPARM_REGS (mode)));
}

static int
sh_arg_partial_bytes (cumulative_args_t cum_v, machine_mode mode,
		      tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int words = 0;

  if (sh_pass_in_reg_p (*cum, mode, type)
      && !TARGET_FPU_DOUBLE
      && (sh_round_reg (*cum, mode)
	  + (mode != BLKmode
	     ? CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD)
	     : CEIL (int_size_in_bytes (type), UNITS_PER_WORD))
	  > NPARM_REGS (mode)))
    words = NPARM_REGS (mode) - sh_round_reg (*cum, mode);

  return words * UNITS_PER_WORD;
}


/* Define where to put the arguments to a function.
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

   On SH the first args are normally in registers
   and the rest are pushed.  Any arg that starts within the first
   NPARM_REGS words is at least partially passed in a register unless
   its data type forbids.  */
static rtx
sh_function_arg (cumulative_args_t ca_v, machine_mode mode,
		 const_tree type, bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  if (mode == VOIDmode)
    return ca->renesas_abi ? const1_rtx : const0_rtx;

  if (sh_pass_in_reg_p (*ca, mode, type)
      && (named || ! (TARGET_HITACHI || ca->renesas_abi)))
    {
      int regno;

      if (mode == SCmode && TARGET_SH4 && TARGET_LITTLE_ENDIAN
	  && (! FUNCTION_ARG_SCmode_WART || (sh_round_reg (*ca, mode) & 1)))
	{
	  rtx r1 = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SFmode,
						   BASE_ARG_REG (mode)
						   + (sh_round_reg (*ca, mode) ^ 1)),
				      const0_rtx);
	  rtx r2 = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SFmode,
						   BASE_ARG_REG (mode)
						   + ((sh_round_reg (*ca, mode) + 1) ^ 1)),
				      GEN_INT (4));
	  return gen_rtx_PARALLEL(SCmode, gen_rtvec(2, r1, r2));
	}

     /* If the alignment of a DF value causes an SF register to be
	skipped, we will use that skipped register for the next SF
	value.  */
      if ((TARGET_HITACHI || ca->renesas_abi)
	  && ca->free_single_fp_reg
	  && mode == SFmode)
	return gen_rtx_REG (mode, ca->free_single_fp_reg);

      regno = (BASE_ARG_REG (mode) + sh_round_reg (*ca, mode))
	       ^ (mode == SFmode && TARGET_SH4
		  && TARGET_LITTLE_ENDIAN
		  && ! TARGET_HITACHI && ! ca->renesas_abi);
      return gen_rtx_REG (mode, regno);

    }

  return NULL_RTX;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be
   available.)  */
static void
sh_function_arg_advance (cumulative_args_t ca_v, machine_mode mode,
			 const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  if (ca->force_mem)
    ca->force_mem = false;

  if ((TARGET_HITACHI || ca->renesas_abi) && TARGET_FPU_DOUBLE)
    {
      /* Note that we've used the skipped register.  */
      if (mode == SFmode && ca->free_single_fp_reg)
	{
	  ca->free_single_fp_reg = 0;
	  return;
	}
      /* When we have a DF after an SF, there's an SF register that get
	 skipped in order to align the DF value.  We note this skipped
	 register, because the next SF value will use it, and not the
	 SF that follows the DF.  */
      if (mode == DFmode
	  && sh_round_reg (*ca, DFmode) != sh_round_reg (*ca, SFmode))
	{
	  ca->free_single_fp_reg = (sh_round_reg (*ca, SFmode)
				    + BASE_ARG_REG (mode));
	}
    }

  if (! ((TARGET_SH4 || TARGET_SH2A) || ca->renesas_abi)
      || sh_pass_in_reg_p (*ca, mode, type))
    (ca->arg_count[(int) get_sh_arg_class (mode)]
     = (sh_round_reg (*ca, mode)
	+ (mode == BLKmode
	   ? CEIL (int_size_in_bytes (type), UNITS_PER_WORD)
	   : CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD))));
}

/* The Renesas calling convention doesn't quite fit into this scheme since
   the address is passed like an invisible argument, but one that is always
   passed in memory.  */
static rtx
sh_struct_value_rtx (tree fndecl, int incoming ATTRIBUTE_UNUSED)
{
  if (TARGET_HITACHI || sh_attr_renesas_p (fndecl))
    return NULL_RTX;
  return gen_rtx_REG (Pmode, 2);
}

/* Worker function for TARGET_FUNCTION_VALUE.

   For the SH, this is like LIBCALL_VALUE, except that we must change the
   mode like PROMOTE_MODE does.
   ??? PROMOTE_MODE is ignored for non-scalar types.  The set of types
   tested here has to be kept in sync with the one in
   explow.c:promote_mode.  */
static rtx
sh_function_value (const_tree valtype,
		   const_tree fn_decl_or_type,
		   bool outgoing ATTRIBUTE_UNUSED)
{
  if (fn_decl_or_type
      && !DECL_P (fn_decl_or_type))
    fn_decl_or_type = NULL;

  return gen_rtx_REG (
	   ((GET_MODE_CLASS (TYPE_MODE (valtype)) == MODE_INT
	     && GET_MODE_SIZE (TYPE_MODE (valtype)) < 4
	     && (TREE_CODE (valtype) == INTEGER_TYPE
		 || TREE_CODE (valtype) == ENUMERAL_TYPE
		 || TREE_CODE (valtype) == BOOLEAN_TYPE
		 || TREE_CODE (valtype) == REAL_TYPE
		 || TREE_CODE (valtype) == OFFSET_TYPE))
	    && sh_promote_prototypes (fn_decl_or_type)
	    ? SImode : TYPE_MODE (valtype)),
	   BASE_RETURN_VALUE_REG (TYPE_MODE (valtype)));
}

/* Worker function for TARGET_LIBCALL_VALUE.  */
static rtx
sh_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, BASE_RETURN_VALUE_REG (mode));
}

/* Return true if N is a possible register number of function value.  */
static bool
sh_function_value_regno_p (const unsigned int regno)
{
  return regno == FIRST_RET_REG || (TARGET_SH2E && regno == FIRST_FP_RET_REG);
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */
static bool
sh_return_in_memory (const_tree type, const_tree fndecl)
{
  return TYPE_MODE (type) == BLKmode
	 || ((TARGET_HITACHI || sh_attr_renesas_p (fndecl))
	     && TREE_CODE (type) == RECORD_TYPE);
}

/* We actually emit the code in sh_expand_prologue.  We used to use
   a static variable to flag that we need to emit this code, but that
   doesn't when inlining, when functions are deferred and then emitted
   later.  Fortunately, we already have two flags that are part of struct
   function that tell if a function uses varargs or stdarg.  */
static void
sh_setup_incoming_varargs (cumulative_args_t ca,
			   machine_mode mode,
			   tree type,
			   int *pretend_arg_size,
			   int second_time ATTRIBUTE_UNUSED)
{
  gcc_assert (cfun->stdarg);
  if (TARGET_VARARGS_PRETEND_ARGS (current_function_decl))
    {
      int named_parm_regs, anon_parm_regs;

      named_parm_regs = (sh_round_reg (*get_cumulative_args (ca), mode)
			 + (mode == BLKmode
			    ? CEIL (int_size_in_bytes (type), UNITS_PER_WORD)
			    : CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD)));
      anon_parm_regs = NPARM_REGS (SImode) - named_parm_regs;
      if (anon_parm_regs > 0)
	*pretend_arg_size = anon_parm_regs * 4;
    }
}

static bool
sh_strict_argument_naming (cumulative_args_t ca ATTRIBUTE_UNUSED)
{
  return false;
}

static bool
sh_pretend_outgoing_varargs_named (cumulative_args_t ca_v)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  return ! (TARGET_HITACHI || ca->renesas_abi);
}


/* Define the offset between two registers, one to be eliminated, and
   the other its replacement, at the start of a routine.  */
int
initial_elimination_offset (int from, int to)
{
  const int regs_saved_rounding = 0;
  int save_flags = target_flags;
  HARD_REG_SET live_regs_mask;

  int regs_saved = calc_live_regs (&live_regs_mask);

  int total_auto_space = rounded_frame_size (regs_saved) - regs_saved_rounding;
  target_flags = save_flags;

  int total_saved_regs_space = regs_saved + regs_saved_rounding;

  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return total_saved_regs_space + total_auto_space;

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return total_saved_regs_space + total_auto_space;

  /* Initial gap between fp and sp is 0.  */
  if (from == HARD_FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return 0;

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return rounded_frame_size (0);

  if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return rounded_frame_size (0);

  gcc_assert (from == RETURN_ADDRESS_POINTER_REGNUM
	      && (to == HARD_FRAME_POINTER_REGNUM
		  || to == STACK_POINTER_REGNUM));
  return total_auto_space;
}

/* Parse the -mfixed-range= option string.  */
void
sh_fix_range (const char *const_str)
{
  /* str must be of the form REG1'-'REG2{,REG1'-'REG} where REG1 and
     REG2 are either register names or register numbers.  The effect
     of this option is to mark the registers in the range from REG1 to
     REG2 as ``fixed'' so they won't be used by the compiler.  */

  char* str = strcpy ((char*)alloca (strlen (const_str) + 1), const_str);

  while (1)
    {
      char* dash = strchr (str, '-');
      if (!dash)
	{
	  warning (0, "value of -mfixed-range must have form REG1-REG2");
	  return;
	}
      *dash = '\0';
      char* comma = strchr (dash + 1, ',');
      if (comma)
	*comma = '\0';

      int first = decode_reg_name (str);
      if (first < 0)
	{
	  warning (0, "unknown register name: %s", str);
	  return;
	}

      int last = decode_reg_name (dash + 1);
      if (last < 0)
	{
	  warning (0, "unknown register name: %s", dash + 1);
	  return;
	}

      *dash = '-';

      if (first > last)
	{
	  warning (0, "%s-%s is an empty range", str, dash + 1);
	  return;
	}

      for (int i = first; i <= last; ++i)
	fixed_regs[i] = call_used_regs[i] = 1;

      if (!comma)
	break;

      *comma = ',';
      str = comma + 1;
    }
}

/* Insert any deferred function attributes from earlier pragmas.  */
static void
sh_insert_attributes (tree node, tree *attributes)
{
  if (TREE_CODE (node) != FUNCTION_DECL)
    return;

  /* We are only interested in fields.  */
  if (!DECL_P (node))
    return;

  /* Append the attributes to the deferred attributes.  */
  *sh_deferred_function_attributes_tail = *attributes;
  tree attrs = sh_deferred_function_attributes;
  if (!attrs)
    return;

  /* Some attributes imply or require the interrupt attribute.  */
  if (!lookup_attribute ("interrupt_handler", attrs)
      && !lookup_attribute ("interrupt_handler", DECL_ATTRIBUTES (node)))
    {
      /* If we have a trapa_handler, but no interrupt_handler attribute,
	 insert an interrupt_handler attribute.  */
      if (lookup_attribute ("trapa_handler", attrs) != NULL_TREE)
	/* We can't use sh_pr_interrupt here because that's not in the
	   java frontend.  */
	attrs
	  = tree_cons (get_identifier("interrupt_handler"), NULL_TREE, attrs);
      /* However, for sp_switch, trap_exit, nosave_low_regs and resbank,
	 if the interrupt attribute is missing, we ignore the attribute
	 and warn.  */
      else if (lookup_attribute ("sp_switch", attrs)
	       || lookup_attribute ("trap_exit", attrs)
	       || lookup_attribute ("nosave_low_regs", attrs)
	       || lookup_attribute ("resbank", attrs))
	{
	  tree *tail;

	  for (tail = attributes; attrs; attrs = TREE_CHAIN (attrs))
	    {
	      if (is_attribute_p ("sp_switch", TREE_PURPOSE (attrs))
		  || is_attribute_p ("trap_exit", TREE_PURPOSE (attrs))
		  || is_attribute_p ("nosave_low_regs", TREE_PURPOSE (attrs))
		  || is_attribute_p ("resbank", TREE_PURPOSE (attrs)))
		warning (OPT_Wattributes,
			 "%qE attribute only applies to interrupt functions",
			 TREE_PURPOSE (attrs));
	      else
		{
		  *tail = tree_cons (TREE_PURPOSE (attrs), NULL_TREE,
				     NULL_TREE);
		  tail = &TREE_CHAIN (*tail);
		}
	    }
	  attrs = *attributes;
	}
    }

  /* Install the processed list.  */
  *attributes = attrs;

  /* Clear deferred attributes.  */
  sh_deferred_function_attributes = NULL_TREE;
  sh_deferred_function_attributes_tail = &sh_deferred_function_attributes;

  return;
}

/*------------------------------------------------------------------------------
  Target specific attributes
  Supported attributes are:

   * interrupt_handler
	Specifies this function is an interrupt handler.

   * trapa_handler
	Like interrupt_handler, but don't save all registers.

   * sp_switch
	Specifies an alternate stack for an interrupt handler to run on.

   * trap_exit
	Use a trapa to exit an interrupt function instead of rte.

   * nosave_low_regs
	Don't save r0..r7 in an interrupt handler function.
	This is useful on SH3* and SH4*, which have a separate set of low
	regs for user and privileged modes.
	This is mainly to be used for non-reentrant interrupt handlers (i.e.
	those that run with interrupts disabled and thus can't be
	interrupted thenselves).

   * renesas
	Use Renesas calling/layout conventions (functions and structures).

   * resbank
	In case of an interrupt handler function, use a register bank to
	save registers R0-R14, MACH, MACL, GBR and PR.
	This is available only on SH2A targets.

   * function_vector
	Declares a function to be called using the TBR relative addressing
	mode.  Takes an argument that specifies the slot number in the table
	where this function can be looked up by the JSR/N @@(disp8,TBR) insn.
*/

/* Handle a 'resbank' attribute.  */
static tree
sh_handle_resbank_handler_attribute (tree * node, tree name,
				     tree args ATTRIBUTE_UNUSED,
				     int flags ATTRIBUTE_UNUSED,
				     bool * no_add_attrs)
{
  if (!TARGET_SH2A)
    {
      warning (OPT_Wattributes, "%qE attribute is supported only for SH2A",
	       name);
      *no_add_attrs = true;
    }
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "interrupt_handler" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
sh_handle_interrupt_handler_attribute (tree *node, tree name,
				       tree args ATTRIBUTE_UNUSED,
				       int flags ATTRIBUTE_UNUSED,
				       bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an 'function_vector' attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
sh2a_handle_function_vector_handler_attribute (tree * node, tree name,
					       tree args ATTRIBUTE_UNUSED,
					       int flags ATTRIBUTE_UNUSED,
					       bool * no_add_attrs)
{
  if (!TARGET_SH2A)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to SH2A",
	       name);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (TREE_VALUE (args)) != INTEGER_CST)
    {
      /* The argument must be a constant integer.  */
      warning (OPT_Wattributes,
	       "%qE attribute argument not an integer constant",
	       name);
      *no_add_attrs = true;
    }
  else if (TREE_INT_CST_LOW (TREE_VALUE (args)) > 255)
    {
      /* The argument value must be between 0 to 255.  */
      warning (OPT_Wattributes,
	       "%qE attribute argument should be between 0 to 255",
	       name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

/* Returns true if current function has been assigned the attribute
   'function_vector'.  */
bool
sh2a_is_function_vector_call (rtx x)
{
  if (GET_CODE (x) == SYMBOL_REF
      && (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_FUNCVEC_FUNCTION))
    {
      tree tr = SYMBOL_REF_DECL (x);

      if (sh2a_function_vector_p (tr))
        return true;
    }

  return false;
}

/* Returns the function vector number, if the attribute
   'function_vector' is assigned, otherwise returns zero.  */
int
sh2a_get_function_vector_number (rtx x)
{
  if ((GET_CODE (x) == SYMBOL_REF)
      && (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_FUNCVEC_FUNCTION))
    {
      tree t = SYMBOL_REF_DECL (x);

      if (TREE_CODE (t) != FUNCTION_DECL)
	return 0;

      for (tree list = SH_ATTRIBUTES (t); list; list = TREE_CHAIN (list))
	if (is_attribute_p ("function_vector", TREE_PURPOSE (list)))
	  return TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (list)));

      return 0;
    }
  else
    return 0;
}

/* Handle an "sp_switch" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
sh_handle_sp_switch_attribute (tree *node, tree name, tree args,
			       int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (TREE_VALUE (args)) != STRING_CST)
    {
      /* The argument must be a constant string.  */
      warning (OPT_Wattributes, "%qE attribute argument not a string constant",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "trap_exit" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
sh_handle_trap_exit_attribute (tree *node, tree name, tree args,
			       int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }
  /* The argument specifies a trap number to be used in a trapa instruction
     at function exit (instead of an rte instruction).  */
  else if (TREE_CODE (TREE_VALUE (args)) != INTEGER_CST)
    {
      /* The argument must be a constant integer.  */
      warning (OPT_Wattributes, "%qE attribute argument not an "
	       "integer constant", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static tree
sh_handle_renesas_attribute (tree *node ATTRIBUTE_UNUSED,
			     tree name ATTRIBUTE_UNUSED,
			     tree args ATTRIBUTE_UNUSED,
			     int flags ATTRIBUTE_UNUSED,
			     bool *no_add_attrs ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

/* True if __attribute__((renesas)) or -mrenesas.  */
bool
sh_attr_renesas_p (const_tree td)
{
  if (TARGET_HITACHI)
    return true;
  if (td == NULL_TREE)
    return false;
  if (DECL_P (td))
    td = TREE_TYPE (td);
  if (td == error_mark_node)
    return false;
  return lookup_attribute ("renesas", TYPE_ATTRIBUTES (td)) != NULL_TREE;
}

/* True if __attribute__((renesas)) or -mrenesas, for the current
   function.  */
bool
sh_cfun_attr_renesas_p (void)
{
  return sh_attr_renesas_p (current_function_decl);
}

/* Returns true if the current function has the "interrupt_handler"
   attribute set.  */
bool
sh_cfun_interrupt_handler_p (void)
{
  return (lookup_attribute ("interrupt_handler",
			    DECL_ATTRIBUTES (current_function_decl))
	  != NULL_TREE);
}

/* Returns true if FUNC has been assigned the attribute
   "function_vector".  */
bool
sh2a_function_vector_p (tree func)
{
  if (TREE_CODE (func) != FUNCTION_DECL)
    return false;

  for (tree list = SH_ATTRIBUTES (func); list; list = TREE_CHAIN (list))
    if (is_attribute_p ("function_vector", TREE_PURPOSE (list)))
      return true;

  return false;
}

/* Returns true if given tree has the "resbank" attribute set.  */
bool
sh_cfun_resbank_handler_p (void)
{
  return ((lookup_attribute ("resbank",
			     DECL_ATTRIBUTES (current_function_decl))
	  != NULL_TREE)
	  && (lookup_attribute ("interrupt_handler",
				DECL_ATTRIBUTES (current_function_decl))
	      != NULL_TREE) && TARGET_SH2A);
}

/* Returns true if the current function has a "trap_exit" attribute set.  */
bool
sh_cfun_trap_exit_p (void)
{
  return lookup_attribute ("trap_exit", DECL_ATTRIBUTES (current_function_decl))
	 != NULL_TREE;
}

/* Implement TARGET_CHECK_PCH_TARGET_FLAGS.  */
static const char *
sh_check_pch_target_flags (int old_flags)
{
  if ((old_flags ^ target_flags) & (MASK_SH1 | MASK_SH2 | MASK_SH3
				    | MASK_SH_E | MASK_HARD_SH4
				    | MASK_FPU_SINGLE | MASK_SH4))
    return _("created and used with different architectures / ABIs");
  if ((old_flags ^ target_flags) & MASK_HITACHI)
    return _("created and used with different ABIs");
  if ((old_flags ^ target_flags) & MASK_LITTLE_ENDIAN)
    return _("created and used with different endianness");
  return NULL;
}

/* Predicates used by the templates.  */

/* Returns true if OP is MACL, MACH or PR.  The input must be a REG rtx.
   Used only in general_movsrc_operand.  */
bool
system_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (REGNO (op))
    {
    case PR_REG:
    case MACL_REG:
    case MACH_REG:
      return true;
    }
  return false;
}

/* Returns true if OP is a floating point value with value 0.0.  */
bool
fp_zero_operand (rtx op)
{
  if (GET_MODE (op) != SFmode)
    return false;

  const REAL_VALUE_TYPE* r = CONST_DOUBLE_REAL_VALUE (op);
  return real_equal (r, &dconst0) && ! REAL_VALUE_MINUS_ZERO (*r);
}

/* Returns true if OP is a floating point value with value 1.0.  */
bool
fp_one_operand (rtx op)
{
  if (GET_MODE (op) != SFmode)
    return false;

  return real_equal (CONST_DOUBLE_REAL_VALUE (op), &dconst1);
}

/* Return the TLS type for TLS symbols.  */
enum tls_model
tls_symbolic_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (op) != SYMBOL_REF)
    return TLS_MODEL_NONE;
  return SYMBOL_REF_TLS_MODEL (op);
}

/* Return the destination address of a branch.  */
static int
branch_dest (rtx branch)
{
  rtx dest = SET_SRC (PATTERN (branch));

  if (GET_CODE (dest) == IF_THEN_ELSE)
    dest = XEXP (dest, 1);

  return INSN_ADDRESSES (INSN_UID (XEXP (dest, 0)));
}

/* Return nonzero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels.  It may live past calls or jumps though.  */
bool
reg_unused_after (rtx reg, rtx_insn *insn)
{
  /* If the reg is set by this instruction, then it is safe for our
     case.  Disregard the case where this is a store to memory, since
     we are checking a register used in the store address.  */
  rtx set = single_set (insn);
  if (set && !MEM_P (SET_DEST (set))
      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
    return true;

  while ((insn = NEXT_INSN (insn)))
    {
      if (!INSN_P (insn))
	continue;

      rtx_code code = GET_CODE (insn);

#if 0
      /* If this is a label that existed before reload, then the register
	 is dead here.  However, if this is a label added by reorg, then
	 the register may still be live here.  We can't tell the difference,
	 so we just ignore labels completely.  */
      if (code == CODE_LABEL)
	return 1;
      /* else */
#endif

      if (code == JUMP_INSN)
	return false;

      /* If this is a sequence, we must handle them all at once.
	 We could have for instance a call that sets the target register,
	 and an insn in a delay slot that uses the register.  In this case,
	 we must return 0.  */
      else if (code == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  rtx_sequence *seq = as_a <rtx_sequence *> (PATTERN (insn));
	  bool retval = false;

	  for (int i = 0; i < seq->len (); i++)
	    {
	      rtx_insn *this_insn = seq->insn (i);
	      rtx set = single_set (this_insn);

	      if (CALL_P (this_insn))
		code = CALL_INSN;
	      else if (JUMP_P (this_insn))
		{
		  if (INSN_ANNULLED_BRANCH_P (this_insn))
		    return false;
		  code = JUMP_INSN;
		}

	      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
		return false;
	      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
		{
		  if (!MEM_P (SET_DEST (set)))
		    retval = true;
		  else
		    return false;
		}
	      if (set == NULL_RTX
		  && reg_overlap_mentioned_p (reg, PATTERN (this_insn)))
		return false;
	    }
	  if (retval)
	    return true;
	  else if (code == JUMP_INSN)
	    return false;
	}

      rtx set = single_set (insn);
      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
	return false;
      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	return !MEM_P (SET_DEST (set));
      if (set == NULL && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	return false;

      if (code == CALL_INSN && call_really_used_regs[REGNO (reg)])
	return true;
    }
  return true;
}


static GTY(()) rtx t_reg_rtx;
rtx
get_t_reg_rtx (void)
{
  if (! t_reg_rtx)
    t_reg_rtx = gen_rtx_REG (SImode, T_REG);
  return t_reg_rtx;
}

static GTY(()) tree fpscr_values;

static void
emit_fpu_switch (rtx scratch, int index)
{
  if (fpscr_values == NULL)
    {
      tree t = build_index_type (integer_one_node);
      t = build_array_type (integer_type_node, t);
      t = build_decl (BUILTINS_LOCATION,
		      VAR_DECL, get_identifier ("__fpscr_values"), t);
      DECL_ARTIFICIAL (t) = 1;
      DECL_IGNORED_P (t) = 1;
      DECL_EXTERNAL (t) = 1;
      TREE_STATIC (t) = 1;
      TREE_PUBLIC (t) = 1;
      TREE_USED (t) = 1;

      fpscr_values = t;
    }

  rtx src = DECL_RTL (fpscr_values);
  if (!can_create_pseudo_p ())
    {
      emit_move_insn (scratch, XEXP (src, 0));
      if (index != 0)
	emit_insn (gen_addsi3 (scratch, scratch, GEN_INT (index * 4)));
      src = adjust_automodify_address (src, SImode, scratch, index * 4);
    }
  else
    src = adjust_address (src, SImode, index * 4);

  emit_insn (gen_lds_fpscr (src));
}

static rtx get_free_reg (HARD_REG_SET);

/* This function returns a register to use to load the address to load
   the fpscr from.  Currently it always returns r1 or r7, but when we are
   able to use pseudo registers after combine, or have a better mechanism
   for choosing a register, it should be done here.  */
/* REGS_LIVE is the liveness information for the point for which we
   need this allocation.  In some bare-bones exit blocks, r1 is live at the
   start.  We can even have all of r0..r3 being live:
__complex__ long long f (double d) { if (d == 0) return 2; else return 3; }
   INSN before which new insns are placed with will clobber the register
   we return.  If a basic block consists only of setting the return value
   register to a pseudo and using that register, the return value is not
   live before or after this block, yet we we'll insert our insns right in
   the middle.  */
static rtx
get_free_reg (HARD_REG_SET regs_live)
{
  if (! TEST_HARD_REG_BIT (regs_live, 1))
    return gen_rtx_REG (Pmode, 1);

  /* Hard reg 1 is live; since this is a small register classes target,
     there shouldn't be anything but a jump before the function end.  */
  gcc_assert (!TEST_HARD_REG_BIT (regs_live, 7));
  return gen_rtx_REG (Pmode, 7);
}

/* This function will set the fpscr from memory.
   MODE is the mode we are setting it to.  */
void
fpscr_set_from_mem (int mode, HARD_REG_SET regs_live)
{
  enum attr_fp_mode fp_mode = (enum attr_fp_mode) mode;
  enum attr_fp_mode norm_mode = ACTUAL_NORMAL_MODE (FP_MODE);

  rtx addr_reg = !can_create_pseudo_p () ? get_free_reg (regs_live) : NULL_RTX;
  emit_fpu_switch (addr_reg, fp_mode == norm_mode);
}

/* Is the given character a logical line separator for the assembler?  */
#ifndef IS_ASM_LOGICAL_LINE_SEPARATOR
#define IS_ASM_LOGICAL_LINE_SEPARATOR(C, STR) ((C) == ';')
#endif

static bool
sequence_insn_p (rtx_insn *insn)
{
  rtx_insn* prev = PREV_INSN (insn);
  if (prev == NULL)
    return false;

  rtx_insn* next = NEXT_INSN (prev);
  if (next == NULL)
    return false;

  return INSN_P (next) && GET_CODE (PATTERN (next)) == SEQUENCE;
}

int
sh_insn_length_adjustment (rtx_insn *insn)
{
  /* Instructions with unfilled delay slots take up an extra two bytes for
     the nop in the delay slot.  */
  if (((NONJUMP_INSN_P (insn)
	&& GET_CODE (PATTERN (insn)) != USE
	&& GET_CODE (PATTERN (insn)) != CLOBBER)
       || CALL_P (insn) || JUMP_P (insn))
      && ! sequence_insn_p (insn)
      && get_attr_needs_delay_slot (insn) == NEEDS_DELAY_SLOT_YES)
    return 2;

  /* Increase the insn length of a cbranch without a delay slot insn to
     force a delay slot which will be stuffed with a nop.  */
  if (TARGET_CBRANCH_FORCE_DELAY_SLOT && TARGET_SH2
      && JUMP_P (insn) && get_attr_type (insn) == TYPE_CBRANCH
      && ! sequence_insn_p (insn))
    return 2;

  /* sh-dsp parallel processing insn take four bytes instead of two.  */

  if (NONJUMP_INSN_P (insn))
    {
      int sum = 0;
      rtx body = PATTERN (insn);
      const char *templ;
      char c;
      bool maybe_label = true;

      if (GET_CODE (body) == ASM_INPUT)
	templ = XSTR (body, 0);
      else if (asm_noperands (body) >= 0)
	templ
	  = decode_asm_operands (body, NULL, NULL, NULL, NULL, NULL);
      else
	return 0;
      do
	{
	  int ppi_adjust = 0;

	  do
	    c = *templ++;
	  while (c == ' ' || c == '\t');
	  /* all sh-dsp parallel-processing insns start with p.
	     The only non-ppi sh insn starting with p is pref.
	     The only ppi starting with pr is prnd.  */
	  if ((c == 'p' || c == 'P') && strncasecmp ("re", templ, 2))
	    ppi_adjust = 2;
	  /* The repeat pseudo-insn expands two three insns, a total of
	     six bytes in size.  */
	  else if ((c == 'r' || c == 'R')
		   && ! strncasecmp ("epeat", templ, 5))
	    ppi_adjust = 4;
	  while (c && c != '\n'
		 && ! IS_ASM_LOGICAL_LINE_SEPARATOR (c, templ))
	    {
	      /* If this is a label, it is obviously not a ppi insn.  */
	      if (c == ':' && maybe_label)
		{
		  ppi_adjust = 0;
		  break;
		}
	      else if (c == '\'' || c == '"')
		maybe_label = false;
	      c = *templ++;
	    }
	  sum += ppi_adjust;
	  maybe_label = c != ':';
	}
      while (c);
      return sum;
    }
  return 0;
}

/* Return TRUE for a valid displacement for the REG+disp addressing
   with MODE.  */
bool
sh_legitimate_index_p (machine_mode mode, rtx op, bool consider_sh2a,
		       bool allow_zero)
{
  if (! CONST_INT_P (op))
    return false;

    {
      const HOST_WIDE_INT offset = INTVAL (op);
      const int max_disp = sh_max_mov_insn_displacement (mode, consider_sh2a);
      const int align_mask = mov_insn_alignment_mask (mode, consider_sh2a);

      /* If the mode does not support any displacement always return false.
	 Even though an index of '0' is actually always valid, it will cause
	 troubles when e.g. a DFmode move is split into two SFmode moves,
	 where one SFmode move will have index '0' and the other move will
	 have index '4'.  */
       if (!allow_zero && max_disp < 1)
	return false;

      return offset >= 0 && offset <= max_disp && (offset & align_mask) == 0;
    }
}

/* Recognize an RTL expression that is a valid memory address for
   an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.
   Allow  REG
	  REG+disp
	  REG+r0
	  REG++
	  --REG
	  GBR
	  GBR+disp  */
static bool
sh_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  if (REG_P (x) && REGNO (x) == GBR_REG)
    return true;

  if (MAYBE_BASE_REGISTER_RTX_P (x, strict))
    return true;
  else if ((GET_CODE (x) == POST_INC || GET_CODE (x) == PRE_DEC)
	   && MAYBE_BASE_REGISTER_RTX_P (XEXP (x, 0), strict))
    return true;
  else if (GET_CODE (x) == PLUS)
    {
      rtx xop0 = XEXP (x, 0);
      rtx xop1 = XEXP (x, 1);

      if (REG_P (xop0) && REGNO (xop0) == GBR_REG)
	return gbr_displacement (xop1, mode);

      if (GET_MODE_SIZE (mode) <= 8
	  && MAYBE_BASE_REGISTER_RTX_P (xop0, strict)
	  && sh_legitimate_index_p (mode, xop1, TARGET_SH2A, false))
	return true;

      if (GET_MODE_SIZE (mode) <= 4
	  || (TARGET_FPU_DOUBLE && TARGET_FMOVD && mode == DFmode))
	{
	  if (MAYBE_BASE_REGISTER_RTX_P (xop1, strict)
	      && MAYBE_INDEX_REGISTER_RTX_P (xop0, strict))
	    return true;
	  if (MAYBE_INDEX_REGISTER_RTX_P (xop1, strict)
	      && MAYBE_BASE_REGISTER_RTX_P (xop0, strict))
	    return true;
	}
    }

  return false;
}

/* Return TRUE if X references a SYMBOL_REF or LABEL_REF whose symbol
   isn't protected by a PIC unspec.  */
bool
nonpic_symbol_mentioned_p (rtx x)
{
  if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF
      || GET_CODE (x) == PC)
    return true;

  /* We don't want to look into the possible MEM location of a
     CONST_DOUBLE, since we're not going to use it, in general.  */
  if (GET_CODE (x) == CONST_DOUBLE)
    return false;

  if (GET_CODE (x) == UNSPEC
      && (XINT (x, 1) == UNSPEC_PIC
	  || XINT (x, 1) == UNSPEC_GOT
	  || XINT (x, 1) == UNSPEC_GOTOFF
	  || XINT (x, 1) == UNSPEC_GOTPLT
	  || XINT (x, 1) == UNSPEC_GOTTPOFF
	  || XINT (x, 1) == UNSPEC_DTPOFF
	  || XINT (x, 1) == UNSPEC_TPOFF
	  || XINT (x, 1) == UNSPEC_PLT
	  || XINT (x, 1) == UNSPEC_PCREL
	  || XINT (x, 1) == UNSPEC_SYMOFF
	  || XINT (x, 1) == UNSPEC_PCREL_SYMOFF
	  || XINT (x, 1) == UNSPEC_GOTFUNCDESC
	  || XINT (x, 1) == UNSPEC_GOTOFFFUNCDESC))
    return false;

  const char* fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (int i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  for (int j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (nonpic_symbol_mentioned_p (XVECEXP (x, i, j)))
	      return true;
	}
      else if (fmt[i] == 'e' && nonpic_symbol_mentioned_p (XEXP (x, i)))
	return true;
    }

  return false;
}

/* Convert a non-PIC address in `orig' to a PIC address using @GOT or
   @GOTOFF in `reg'.  */
rtx
legitimize_pic_address (rtx orig, machine_mode mode ATTRIBUTE_UNUSED, rtx reg)
{
  if (tls_symbolic_operand (orig, Pmode) != TLS_MODEL_NONE)
    return orig;

  if (GET_CODE (orig) == LABEL_REF
      || (GET_CODE (orig) == SYMBOL_REF && SYMBOL_REF_LOCAL_P (orig)))
    {
      if (reg == NULL_RTX)
	reg = gen_reg_rtx (Pmode);

      if (TARGET_FDPIC
	  && GET_CODE (orig) == SYMBOL_REF && SYMBOL_REF_FUNCTION_P (orig))
	{
	  /* Weak functions may be NULL which doesn't work with
	     GOTOFFFUNCDESC because the runtime offset is not known.  */
	  if (SYMBOL_REF_WEAK (orig))
	    emit_insn (gen_symGOTFUNCDESC2reg (reg, orig));
	  else
	    emit_insn (gen_symGOTOFFFUNCDESC2reg (reg, orig));
	}
      else if (TARGET_FDPIC
	       && (GET_CODE (orig) == LABEL_REF
		   || (GET_CODE (orig) == SYMBOL_REF && SYMBOL_REF_DECL (orig)
		       && (TREE_READONLY (SYMBOL_REF_DECL (orig))
			   || SYMBOL_REF_EXTERNAL_P (orig)
			   || DECL_SECTION_NAME(SYMBOL_REF_DECL (orig))))))
	/* In FDPIC, GOTOFF can only be used for writable data.  */
	emit_insn (gen_symGOT2reg (reg, orig));
      else
	emit_insn (gen_symGOTOFF2reg (reg, orig));
      return reg;
    }
  else if (GET_CODE (orig) == SYMBOL_REF)
    {
      if (reg == NULL_RTX)
	reg = gen_reg_rtx (Pmode);

      if (TARGET_FDPIC && SYMBOL_REF_FUNCTION_P (orig))
	emit_insn (gen_symGOTFUNCDESC2reg (reg, orig));
      else
	emit_insn (gen_symGOT2reg (reg, orig));
      return reg;
    }
  return orig;
}

/* Given a (logical) mode size and an offset in bytes, try to find a the
   appropriate displacement value for a mov insn.  On SH the displacements
   are limited to max. 60 bytes for SImode, max. 30 bytes in HImode and max.
   15 bytes in QImode.  To compensate this we create a new base address by
   adding an adjustment value to it.

   If the originally requested offset is greater than 127 we prefer using
   values 124..127 over 128..131 to increase opportunities to use the
   add #imm, Rn insn.

   In some cases it is possible that a requested offset might seem unaligned
   or inappropriate for the mode size, like offset = 2 and mode size = 4.
   This is compensated by adjusting the base address so that the effective
   address of the displacement move insn will be aligned. 

   This is not the best possible way of rebasing the base address, as it
   does not look at other present displacement addressings around it.
   In some cases this can create more base address adjustments than would
   actually be necessary.  */
struct disp_adjust
{
  rtx offset_adjust;
  rtx mov_disp;
};

static struct disp_adjust
sh_find_mov_disp_adjust (machine_mode mode, HOST_WIDE_INT offset)
{
  struct disp_adjust res = { NULL_RTX, NULL_RTX };

  /* Do not try to use SH2A's large displacements here, because this would
     effectively disable the small displacement insns.  */
  const int mode_sz = GET_MODE_SIZE (mode);
  const int mov_insn_sz = mov_insn_size (mode, false);
  const int max_disp = sh_max_mov_insn_displacement (mode, false);
  const int max_disp_next = max_disp + mov_insn_sz;
  HOST_WIDE_INT align_modifier = offset > 127 ? mov_insn_sz : 0;
  HOST_WIDE_INT offset_adjust;

  /* In some cases this actually does happen and we must check for it.  */
  if (mode_sz < 1 || mode_sz > 8 || max_disp < 1)
    return res;

  /* Keeps the previous behavior for QImode displacement addressing.
     This just decides how the offset is re-based.  Removing this special
     case will result in slightly bigger code on average, but it's not that
     bad actually.  */
  if (mov_insn_sz == 1)
    align_modifier = 0;

  offset_adjust = ((offset + align_modifier) & ~max_disp) - align_modifier;

  if (mode_sz + offset - offset_adjust <= max_disp_next)
    {
      res.offset_adjust = GEN_INT (offset_adjust);
      res.mov_disp = GEN_INT (offset - offset_adjust);
    }

  return res;
}

/* Try to modify an illegitimate address and make it legitimate.
   If we find one, return the new, valid address.
   Otherwise, return the original address.  */
static rtx
sh_legitimize_address (rtx x, rtx oldx, machine_mode mode)
{
  if (flag_pic)
    x = legitimize_pic_address (oldx, mode, NULL_RTX);

  if ((TARGET_FPU_DOUBLE && mode == DFmode)
      || (TARGET_SH2E && mode == SFmode))
    return x;

  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1))
      && BASE_REGISTER_RTX_P (XEXP (x, 0)))
    {
      struct disp_adjust adj = sh_find_mov_disp_adjust (mode,
							INTVAL (XEXP (x, 1)));

      if (adj.offset_adjust != NULL_RTX && adj.mov_disp != NULL_RTX)
	{
	  rtx sum = expand_binop (Pmode, add_optab, XEXP (x, 0),
				  adj.offset_adjust, NULL_RTX, 0,
				  OPTAB_LIB_WIDEN);
	  return gen_rtx_PLUS (Pmode, sum, adj.mov_disp);
	}
    }
  return x;
}

/* Attempt to replace *p, which is an address that needs reloading, with
   a valid memory address for an operand of mode MODE.
   Like for sh_legitimize_address, for the SH we try to get a normal form
   of the address.  That will allow inheritance of the address reloads.  */
bool
sh_legitimize_reload_address (rtx *p, machine_mode mode, int opnum,
			      int itype)
{
  enum reload_type type = (enum reload_type) itype;
  const int mode_sz = GET_MODE_SIZE (mode);

  if (sh_lra_p ())
    return false;

  if (GET_CODE (*p) == PLUS && CONST_INT_P (XEXP (*p, 1))
      && MAYBE_BASE_REGISTER_RTX_P (XEXP (*p, 0), true))
    {
      const HOST_WIDE_INT offset = INTVAL (XEXP (*p, 1));
      struct disp_adjust adj = sh_find_mov_disp_adjust (mode, offset);

      if (TARGET_SH2A && mode == DFmode && (offset & 0x7))
	{
	  push_reload (*p, NULL_RTX, p, NULL,
		       BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, opnum, type);
	  return true;
	}

      if (TARGET_SH2E && mode == SFmode)
	{
	  *p = copy_rtx (*p);
	  push_reload (*p, NULL_RTX, p, NULL,
		       BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, opnum, type);
	  return true;
	}

      /* FIXME: Do not allow to legitimize QImode and HImode displacement
	 moves because then reload has a problem figuring the constraint
	 that the move insn target/source reg must be R0.
	 Or maybe some handling is wrong in sh_secondary_reload for this
	 to work properly? */
      if ((mode_sz == 4 || mode_sz == 8)
	  && ! (TARGET_SH4 && mode == DFmode)
	  && adj.offset_adjust != NULL_RTX && adj.mov_disp != NULL_RTX)
	{
	  rtx sum = gen_rtx_PLUS (Pmode, XEXP (*p, 0), adj.offset_adjust);
	  *p = gen_rtx_PLUS (Pmode, sum, adj.mov_disp);
	  push_reload (sum, NULL_RTX, &XEXP (*p, 0), NULL,
		       BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, opnum, type);
	  return true;
	}
    }

  /* We must re-recognize what we created before.  */
  if (GET_CODE (*p) == PLUS
      && (mode_sz == 4 || mode_sz == 8)
      && GET_CODE (XEXP (*p, 0)) == PLUS
      && CONST_INT_P (XEXP (XEXP (*p, 0), 1))
      && MAYBE_BASE_REGISTER_RTX_P (XEXP (XEXP (*p, 0), 0), true)
      && CONST_INT_P (XEXP (*p, 1))
      && ! (TARGET_SH2E && mode == SFmode))
    {
      /* Because this address is so complex, we know it must have
	 been created by LEGITIMIZE_RELOAD_ADDRESS before; thus,
	 it is already unshared, and needs no further unsharing.  */
      push_reload (XEXP (*p, 0), NULL_RTX, &XEXP (*p, 0), NULL,
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, opnum, type);
      return true;
    }

  return false;
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize various UNSPEC sequences
   and turn them back into a direct symbol reference.  */
static rtx
sh_delegitimize_address (rtx orig_x)
{
  orig_x = delegitimize_mem_from_attrs (orig_x);

  rtx x = orig_x;
  if (MEM_P (x))
    x = XEXP (x, 0);
  if (GET_CODE (x) == CONST)
    {
      rtx y = XEXP (x, 0);
      if (GET_CODE (y) == UNSPEC)
	{
	  if (XINT (y, 1) == UNSPEC_GOT
	      || XINT (y, 1) == UNSPEC_GOTOFF
	      || XINT (y, 1) == UNSPEC_SYMOFF)
	    return XVECEXP (y, 0, 0);
	  else if (XINT (y, 1) == UNSPEC_PCREL_SYMOFF)
	    {
	      if (GET_CODE (XVECEXP (y, 0, 0)) == CONST)
		{
		  rtx symplt = XEXP (XVECEXP (y, 0, 0), 0);

		  if (GET_CODE (symplt) == UNSPEC
		      && (XINT (symplt, 1) == UNSPEC_PLT
			  || XINT (symplt, 1) == UNSPEC_PCREL))
		    return XVECEXP (symplt, 0, 0);
		}
	    }
	}
    }

  return orig_x;
}

/* Mark the use of a constant in the literal table. If the constant
   has multiple labels, make it unique.  */
static rtx
mark_constant_pool_use (rtx x)
{
  if (x == NULL_RTX)
    return x;

  switch (GET_CODE (x))
    {
    case LABEL_REF:
      x = XEXP (x, 0);
    case CODE_LABEL:
      break;
    default:
      return x;
    }

  /* Get the first label in the list of labels for the same constant
     and delete another labels in the list.  */
  rtx_insn* lab = as_a <rtx_insn*> (x);
  for (rtx_insn* insn = PREV_INSN (lab); insn; insn = PREV_INSN (insn))
    {
      if (!LABEL_P (insn)
	  || LABEL_REFS (insn) != NEXT_INSN (insn))
	break;
      lab = insn;
    }

  for (rtx insn = LABEL_REFS (lab); insn; insn = LABEL_REFS (insn))
    as_a<rtx_insn *> (insn)->set_deleted ();

  /* Mark constants in a window.  */
  for (rtx_insn* insn = NEXT_INSN (as_a <rtx_insn *> (x)); insn;
       insn = NEXT_INSN (insn))
    {
      if (!NONJUMP_INSN_P (insn))
	continue;

      rtx pattern = PATTERN (insn);
      if (GET_CODE (pattern) != UNSPEC_VOLATILE)
	continue;

      switch (XINT (pattern, 1))
	{
	case UNSPECV_CONST2:
	case UNSPECV_CONST4:
	case UNSPECV_CONST8:
	  XVECEXP (pattern, 0, 1) = const1_rtx;
	  break;
	case UNSPECV_WINDOW_END:
	  if (XVECEXP (pattern, 0, 0) == x)
	    return lab;
	  break;
	case UNSPECV_CONST_END:
	  return lab;
	default:
	  break;
	}
    }

  return lab;
}

/* Return true if it's possible to redirect BRANCH1 to the destination
   of an unconditional jump BRANCH2.  We only want to do this if the
   resulting branch will have a short displacement.  */
static bool
sh_can_follow_jump (const rtx_insn *branch1, const rtx_insn *branch2)
{
  /* Don't follow if BRANCH2 is possible to be a jump crossing between
     hot and cold partitions.  */
  if (flag_reorder_blocks_and_partition
      && simplejump_p (branch2)
      && CROSSING_JUMP_P (branch2))
    return false;

  if (flag_expensive_optimizations && simplejump_p (branch2))
    {
      rtx dest = XEXP (SET_SRC (single_set (branch2)), 0);
      rtx_insn *insn;
      int distance;

      for (distance = 0, insn = NEXT_INSN (branch1);
	   insn && distance < 256;
	   insn = PREV_INSN (insn))
	{
	  if (insn == dest)
	    return true;
	  else
	    distance += get_attr_length (insn);
	}
      for (distance = 0, insn = NEXT_INSN (branch1);
	   insn && distance < 256;
	   insn = NEXT_INSN (insn))
	{
	  if (insn == dest)
	    return true;
	  else
	    distance += get_attr_length (insn);
	}
    }
  return false;
}

/* Return nonzero if register old_reg can be renamed to register new_reg.  */
bool
sh_hard_regno_rename_ok (unsigned int old_reg ATTRIBUTE_UNUSED,
			 unsigned int new_reg)
{
  /* Interrupt functions can only use registers that have already been
     saved by the prologue, even if they would normally be
     call-clobbered.  */
  if (sh_cfun_interrupt_handler_p () && !df_regs_ever_live_p (new_reg))
    return false;

  return true;
}

/* Function to update the integer COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  This can be used for example to specify to
   the scheduler that an output- or anti-dependence does not incur
   the same cost as a data-dependence.  The return value should be
   the new value for COST.  */
static int
sh_adjust_cost (rtx_insn *insn, rtx link ATTRIBUTE_UNUSED,
		rtx_insn *dep_insn, int cost)
{
  rtx reg, use_pat;

  if (REG_NOTE_KIND (link) == 0)
    {
      if (recog_memoized (insn) < 0
	  || recog_memoized (dep_insn) < 0)
	return cost;

      rtx dep_set = single_set (dep_insn);

      /* The latency that we specify in the scheduling description refers
	 to the actual output, not to an auto-increment register; for that,
	 the latency is one.  */
      if (dep_set && MEM_P (SET_SRC (dep_set)) && cost > 1)
	{
	  rtx set = single_set (insn);

	  if (set
	      && !reg_mentioned_p (SET_DEST (dep_set), SET_SRC (set))
	      && (!MEM_P (SET_DEST (set))
		  || !reg_mentioned_p (SET_DEST (dep_set),
				       XEXP (SET_DEST (set), 0))))
	    cost = 1;
	}
      /* The only input for a call that is timing-critical is the
	 function's address.  */
      if (CALL_P (insn))
	{
	  rtx call = get_call_rtx_from (insn);
	  if (call
		  /* sibcalli_thunk uses a symbol_ref in an unspec.  */
	      && (GET_CODE (XEXP (XEXP (call, 0), 0)) == UNSPEC
		  || ! reg_set_p (XEXP (XEXP (call, 0), 0), dep_insn)))
	    cost -= TARGET_SH4_300 ? 3 : 6;
	}
      /* Likewise, the most timing critical input for an sfuncs call
	 is the function address.  However, sfuncs typically start
	 using their arguments pretty quickly.
	 Assume a four cycle delay for SH4 before they are needed.
	 Cached ST40-300 calls are quicker, so assume only a one
	 cycle delay there.
	 ??? Maybe we should encode the delays till input registers
	 are needed by sfuncs into the sfunc call insn.  */
      /* All sfunc calls are parallels with at least four components.
	 Exploit this to avoid unnecessary calls to sfunc_uses_reg.  */
      else if (GET_CODE (PATTERN (insn)) == PARALLEL
	       && XVECLEN (PATTERN (insn), 0) >= 4
	       && (reg = sfunc_uses_reg (insn)))
	{
	  if (! reg_set_p (reg, dep_insn))
	    cost -= TARGET_SH4_300 ? 1 : 4;
	}
      if (TARGET_HARD_SH4 && !TARGET_SH4_300)
	{
	  attr_type dep_type = get_attr_type (dep_insn);
	  attr_type type;
	  if (dep_type == TYPE_FLOAD || dep_type == TYPE_PCFLOAD)
	    cost--;
	  else if ((dep_type == TYPE_LOAD_SI || dep_type == TYPE_PCLOAD_SI)
		   && (type = get_attr_type (insn)) != TYPE_CALL
		   && type != TYPE_SFUNC)
	    cost--;
	  /* When the preceding instruction loads the shift amount of
	     the following SHAD/SHLD, the latency of the load is increased
	     by 1 cycle.  */
	  if (get_attr_type (insn) == TYPE_DYN_SHIFT
	      && get_attr_any_int_load (dep_insn) == ANY_INT_LOAD_YES
	      && reg_overlap_mentioned_p (SET_DEST (dep_set),
					  XEXP (SET_SRC (single_set (insn)),
						1)))
	    cost++;
	  /* When an LS group instruction with a latency of less than
	     3 cycles is followed by a double-precision floating-point
	     instruction, FIPR, or FTRV, the latency of the first
	     instruction is increased to 3 cycles.  */
	  else if (cost < 3
		   && get_attr_insn_class (dep_insn) == INSN_CLASS_LS_GROUP
		   && get_attr_dfp_comp (insn) == DFP_COMP_YES)
	    cost = 3;
	  /* The lsw register of a double-precision computation is ready one
	     cycle earlier.  */
	  else if (reload_completed
		   && get_attr_dfp_comp (dep_insn) == DFP_COMP_YES
		   && (use_pat = single_set (insn))
		   && ! regno_use_in (REGNO (SET_DEST (single_set (dep_insn))),
				      SET_SRC (use_pat)))
	    cost -= 1;

	  if (get_attr_any_fp_comp (dep_insn) == ANY_FP_COMP_YES
	      && get_attr_late_fp_use (insn) == LATE_FP_USE_YES)
	    cost -= 1;
	}
      else if (TARGET_SH4_300)
	{
	  /* Stores need their input register two cycles later.  */
	  attr_type type;
	  if (dep_set && cost >= 1
	      && ((type = get_attr_type (insn)) == TYPE_STORE
		  || type == TYPE_PSTORE
		  || type == TYPE_FSTORE || type == TYPE_MAC_MEM))
	    {
	      rtx set = single_set (insn);

	      if (!reg_mentioned_p (SET_SRC (set), XEXP (SET_DEST (set), 0))
		  && rtx_equal_p (SET_SRC (set), SET_DEST (dep_set)))
		{
		  cost -= 2;
		  /* But don't reduce the cost below 1 if the address depends
		     on a side effect of dep_insn.  */
		  if (cost < 1
		      && modified_in_p (XEXP (SET_DEST (set), 0), dep_insn))
		    cost = 1;
		}
	    }
	}
    }
  /* An anti-dependence penalty of two applies if the first insn is a double
     precision fadd / fsub / fmul.  */
  else if (!TARGET_SH4_300
	   && REG_NOTE_KIND (link) == REG_DEP_ANTI
	   && recog_memoized (dep_insn) >= 0
	   && (get_attr_type (dep_insn) == TYPE_DFP_ARITH
	       || get_attr_type (dep_insn) == TYPE_DFP_MUL)
	   /* A lot of alleged anti-flow dependences are fake,
	      so check this one is real.  */
	   && flow_dependent_p (dep_insn, insn))
    cost = 2;

  return cost;
}

/* Check if INSN is flow-dependent on DEP_INSN.  Can also be used to check
   if DEP_INSN is anti-flow dependent on INSN.  */
static bool
flow_dependent_p (rtx insn, rtx dep_insn)
{
  rtx tmp = PATTERN (insn);

  note_stores (PATTERN (dep_insn), flow_dependent_p_1, &tmp);
  return tmp == NULL_RTX;
}

/* A helper function for flow_dependent_p called through note_stores.  */
static void
flow_dependent_p_1 (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data)
{
  rtx * pinsn = (rtx *) data;

  if (*pinsn && reg_referenced_p (x, *pinsn))
    *pinsn = NULL_RTX;
}

/* For use by sh_allocate_initial_value.  Note that sh.md contains some
   'special function' patterns (type sfunc) that clobber pr, but that
   do not look like function calls to leaf_function_p.  Hence we must
   do this extra check.  */
static int
sh_pr_n_sets (void)
{
  return DF_REG_DEF_COUNT (PR_REG);
}

/* Return where to allocate pseudo for a given hard register initial
   value.  */
static rtx
sh_allocate_initial_value (rtx hard_reg)
{
  if (REGNO (hard_reg) == PR_REG)
    {
      if (crtl->is_leaf && ! sh_pr_n_sets ())
	return hard_reg;
      else
	return gen_frame_mem (Pmode, return_address_pointer_rtx);
    }

  return NULL_RTX;
}

/* This function returns "2" to indicate dual issue for the SH4
   processor.  To be used by the DFA pipeline description.  */
static int
sh_issue_rate (void)
{
  if (TARGET_SUPERSCALAR)
    return 2;
  else
    return 1;
}

/* Functions for ready queue reordering for sched1.  */

/* Get weight for mode for a set x.  */
static short
find_set_regmode_weight (rtx x, machine_mode mode)
{
  if (GET_CODE (x) == CLOBBER && register_operand (SET_DEST (x), mode))
    return 1;
  if (GET_CODE (x) == SET && register_operand (SET_DEST (x), mode))
    {
      if (REG_P (SET_DEST (x)))
	{
	  if (!reg_mentioned_p (SET_DEST (x), SET_SRC (x)))
	    return 1;
	  else
	    return 0;
	}
      return 1;
    }
  return 0;
}

/* Get regmode weight for insn.  */
static short
find_insn_regmode_weight (rtx insn, machine_mode mode)
{
  /* Increment weight for each register born here.  */
  rtx x = PATTERN (insn);
  short reg_weight = find_set_regmode_weight (x, mode);
  if (GET_CODE (x) == PARALLEL)
    {
      int j;
      for (j = XVECLEN (x, 0) - 1; j >= 0; j--)
	{
	  x = XVECEXP (PATTERN (insn), 0, j);
	  reg_weight += find_set_regmode_weight (x, mode);
	}
    }
  /* Decrement weight for each register that dies here.  */
  for (x = REG_NOTES (insn); x; x = XEXP (x, 1))
    {
      if (REG_NOTE_KIND (x) == REG_DEAD || REG_NOTE_KIND (x) == REG_UNUSED)
	{
	  rtx note = XEXP (x, 0);
	  if (REG_P (note) && GET_MODE (note) == mode)
	    reg_weight--;
	}
    }
  return reg_weight;
}

/* Calculate regmode weights for all insns of a basic block.  */
static void
find_regmode_weight (basic_block b, machine_mode mode)
{
  rtx_insn *insn, *next_tail, *head, *tail;

  get_ebb_head_tail (b, b, &head, &tail);
  next_tail = NEXT_INSN (tail);

  for (insn = head; insn != next_tail; insn = NEXT_INSN (insn))
    {
      /* Handle register life information.  */
      if (!INSN_P (insn))
	continue;

      if (mode == SFmode)
	INSN_REGMODE_WEIGHT (insn, mode) =
	  find_insn_regmode_weight (insn, mode)
	  + 2 * find_insn_regmode_weight (insn, DFmode);
      else if (mode == SImode)
	INSN_REGMODE_WEIGHT (insn, mode) =
	  find_insn_regmode_weight (insn, mode)
	  + 2 * find_insn_regmode_weight (insn, DImode);
    }
}

/* Comparison function for ready queue sorting.  */
static int
rank_for_reorder (const void *x, const void *y)
{
  rtx_insn *tmp = *(rtx_insn * const *) y;
  rtx_insn *tmp2 = *(rtx_insn * const *) x;

  /* The insn in a schedule group should be issued the first.  */
  if (SCHED_GROUP_P (tmp) != SCHED_GROUP_P (tmp2))
    return SCHED_GROUP_P (tmp2) ? 1 : -1;

  /* If insns are equally good, sort by INSN_LUID (original insn order), This
     minimizes instruction movement, thus minimizing sched's effect on
     register pressure.  */
  return INSN_LUID (tmp) - INSN_LUID (tmp2);
}

/* Resort the array A in which only element at index N may be out of order.  */
static void
swap_reorder (rtx_insn **a, int n)
{
  rtx_insn *insn = a[n - 1];
  int i = n - 2;

  while (i >= 0 && rank_for_reorder (a + i, &insn) >= 0)
    {
      a[i + 1] = a[i];
      i -= 1;
    }
  a[i + 1] = insn;
}

/* Sort the ready list by ascending priority.  */
static void
ready_reorder (rtx_insn **ready, int nready)
{
  if (nready == 2)
    swap_reorder (ready, nready);
  else if (nready > 2)
     qsort (ready, nready, sizeof (rtx_insn *), rank_for_reorder);
}

/* Count life regions of r0 for a block.  */
static int
find_r0_life_regions (basic_block b)
{
  bool live;
  int set;
  int death = 0;

  if (REGNO_REG_SET_P (df_get_live_in (b), R0_REG))
    {
      set = 1;
      live = true;
    }
  else
    {
      set = 0;
      live = false;
    }

  rtx_insn* insn = BB_HEAD (b);
  rtx_insn* end = BB_END (b);
  rtx r0_reg = gen_rtx_REG (SImode, R0_REG);
  while (1)
    {
      if (INSN_P (insn))
	{
	  if (find_regno_note (insn, REG_DEAD, R0_REG))
	    {
	      death++;
	      live = false;
	    }

	  rtx pset;
	  if (!live
	      && (pset = single_set (insn))
	      && reg_overlap_mentioned_p (r0_reg, SET_DEST (pset))
	      && !find_regno_note (insn, REG_UNUSED, R0_REG))
	    {
	      set++;
	      live = true;
	    }
	}
      if (insn == end)
	break;
      insn = NEXT_INSN (insn);
    }
  return set - death;
}

/* Calculate regmode weights for all insns of all basic block.  */
static void
sh_md_init_global (FILE *dump ATTRIBUTE_UNUSED,
		   int verbose ATTRIBUTE_UNUSED,
		   int old_max_uid)
{
  basic_block b;

  regmode_weight[0] = (short *) xcalloc (old_max_uid, sizeof (short));
  regmode_weight[1] = (short *) xcalloc (old_max_uid, sizeof (short));
  r0_life_regions = 0;

  FOR_EACH_BB_REVERSE_FN (b, cfun)
  {
    find_regmode_weight (b, SImode);
    find_regmode_weight (b, SFmode);
    if (!reload_completed)
      r0_life_regions += find_r0_life_regions (b);
  }

  CURR_REGMODE_PRESSURE (SImode) = 0;
  CURR_REGMODE_PRESSURE (SFmode) = 0;
}

/* Cleanup.  */
static void
sh_md_finish_global (FILE *dump ATTRIBUTE_UNUSED,
		     int verbose ATTRIBUTE_UNUSED)
{
  if (regmode_weight[0])
    {
      free (regmode_weight[0]);
      regmode_weight[0] = NULL;
    }
  if (regmode_weight[1])
    {
      free (regmode_weight[1]);
      regmode_weight[1] = NULL;
    }
}

/* Cache the can_issue_more so that we can return it from reorder2. Also,
   keep count of register pressures on SImode and SFmode. */
static int
sh_variable_issue (FILE *dump ATTRIBUTE_UNUSED,
		   int sched_verbose ATTRIBUTE_UNUSED,
		   rtx_insn *insn,
		   int can_issue_more)
{
  if (GET_CODE (PATTERN (insn)) != USE
      && GET_CODE (PATTERN (insn)) != CLOBBER)
    cached_can_issue_more = can_issue_more - 1;
  else
    cached_can_issue_more = can_issue_more;

  if (reload_completed)
    return cached_can_issue_more;

  CURR_REGMODE_PRESSURE (SImode) += INSN_REGMODE_WEIGHT (insn, SImode);
  CURR_REGMODE_PRESSURE (SFmode) += INSN_REGMODE_WEIGHT (insn, SFmode);

  return cached_can_issue_more;
}

static void
sh_md_init (FILE *dump ATTRIBUTE_UNUSED,
	    int verbose ATTRIBUTE_UNUSED,
	    int veclen ATTRIBUTE_UNUSED)
{
  CURR_REGMODE_PRESSURE (SImode) = 0;
  CURR_REGMODE_PRESSURE (SFmode) = 0;
}

/* Some magic numbers.  */
/* Pressure on register r0 can lead to spill failures. so avoid sched1 for
   functions that already have high pressure on r0. */
#define R0_MAX_LIFE_REGIONS 2
/* Register Pressure thresholds for SImode and SFmode registers.  */
#define SIMODE_MAX_WEIGHT 5
#define SFMODE_MAX_WEIGHT 10

/* Return true if the pressure is high for MODE.  */
static bool
high_pressure (machine_mode mode)
{
  /* Pressure on register r0 can lead to spill failures. so avoid sched1 for
     functions that already have high pressure on r0. */
   if (r0_life_regions >= R0_MAX_LIFE_REGIONS)
     return true;

  if (mode == SFmode)
    return (CURR_REGMODE_PRESSURE (SFmode) > SFMODE_MAX_WEIGHT);
  else
    return (CURR_REGMODE_PRESSURE (SImode) > SIMODE_MAX_WEIGHT);
}

/* Reorder ready queue if register pressure is high.  */
static int
sh_reorder (FILE *dump ATTRIBUTE_UNUSED,
	    int sched_verbose ATTRIBUTE_UNUSED,
	    rtx_insn **ready,
	    int *n_readyp,
	    int clock_var ATTRIBUTE_UNUSED)
{
  if (reload_completed)
    return sh_issue_rate ();

  if (high_pressure (SFmode) || high_pressure (SImode))
    {
      ready_reorder (ready, *n_readyp);
    }

  return sh_issue_rate ();
}

/* Skip cycles if the current register pressure is high.  */
static int
sh_reorder2 (FILE *dump ATTRIBUTE_UNUSED,
	     int sched_verbose ATTRIBUTE_UNUSED,
	     rtx_insn **ready ATTRIBUTE_UNUSED,
	     int *n_readyp ATTRIBUTE_UNUSED,
	     int clock_var ATTRIBUTE_UNUSED)
{
  if (reload_completed)
    return cached_can_issue_more;

  if (high_pressure(SFmode) || high_pressure (SImode))
    skip_cycles = 1;

  return cached_can_issue_more;
}

/* Skip cycles without sorting the ready queue. This will move insn from
   Q->R. If this is the last cycle we are skipping; allow sorting of ready
   queue by sh_reorder.  */

/* Generally, skipping these many cycles are sufficient for all insns to move
   from Q -> R.  */
#define MAX_SKIPS 8

static int
sh_dfa_new_cycle (FILE *sched_dump ATTRIBUTE_UNUSED,
		  int sched_verbose ATTRIBUTE_UNUSED,
		  rtx_insn *insn ATTRIBUTE_UNUSED,
		  int last_clock_var,
		  int clock_var,
		  int *sort_p)
{
  if (reload_completed)
    return 0;

  if (skip_cycles)
    {
      if ((clock_var - last_clock_var) < MAX_SKIPS)
	{
	  *sort_p = 0;
	  return 1;
	}
      /* If this is the last cycle we are skipping, allow reordering of R.  */
      if ((clock_var - last_clock_var) == MAX_SKIPS)
	{
	  *sort_p = 1;
	  return 1;
	}
    }

  skip_cycles = 0;

  return 0;
}

static bool
sh_ms_bitfield_layout_p (const_tree record_type ATTRIBUTE_UNUSED)
{
  return TARGET_HITACHI || sh_attr_renesas_p (record_type);
}

/*
   On the SH1..SH4, the trampoline looks like
   2 0002 D202     	   	mov.l	l2,r2
   1 0000 D301     		mov.l	l1,r3
   3 0004 422B     		jmp	@r2
   4 0006 0009     		nop
   5 0008 00000000 	l1:  	.long   area
   6 000c 00000000 	l2:	.long   function

   FDPIC needs a form that includes a function descriptor and
   code to load the GOT register:
   0 0000 00000000		.long	l0
   1 0004 00000000		.long	gotval
   2 0008 D302    	l0:	mov.l	l1,r3
   3 000a D203    		mov.l	l2,r2
   4 000c 6122    		mov.l	@r2,r1
   5 000e 5C21    		mov.l	@(4,r2),r12
   6 0010 412B    		jmp	@r1
   7 0012 0009    		nop
   8 0014 00000000	l1:	.long	area
   9 0018 00000000	l2:	.long	function

   SH5 (compact) uses r1 instead of r3 for the static chain.  */

/* Emit insns to store a value at memory address + offset.  */
static void
sh_emit_storesi (rtx addr, HOST_WIDE_INT offset, rtx value)
{
  gcc_assert ((offset & 3) == 0);
  emit_move_insn (offset == 0
		  ? change_address (addr, SImode, NULL_RTX)
		  : adjust_address (addr, SImode, offset), value);
}

/* Emit insns to store w0 at addr + offset and w1 at addr + offset + 2.  */
static void
sh_emit_storehi (rtx addr, HOST_WIDE_INT offset, uint16_t w0, uint16_t w1)
{
  sh_emit_storesi (addr, offset, gen_int_mode (TARGET_LITTLE_ENDIAN
					       ? (w0 | (w1 << 16))
					       : (w1 | (w0 << 16)), SImode));
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
static void
sh_trampoline_init (rtx tramp_mem, tree fndecl, rtx cxt)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx tramp = force_reg (Pmode, XEXP (tramp_mem, 0));

  if (TARGET_FDPIC)
    {
      rtx a = force_reg (Pmode, plus_constant (Pmode, XEXP (tramp_mem, 0), 8));

      sh_emit_storesi (tramp_mem, 0, a);
      sh_emit_storesi (tramp_mem, 4, sh_get_fdpic_reg_initial_val ());

      sh_emit_storehi (tramp_mem,  8, 0xd302, 0xd203);
      sh_emit_storehi (tramp_mem, 12, 0x6122, 0x5c21);
      sh_emit_storehi (tramp_mem, 16, 0x412b, 0x0009);

      sh_emit_storesi (tramp_mem, 20, cxt);
      sh_emit_storesi (tramp_mem, 24, fnaddr);
    }
  else
    {
      sh_emit_storehi (tramp_mem, 0, 0xd202, 0xd301);
      sh_emit_storehi (tramp_mem, 4, 0x422b, 0x0009);

      sh_emit_storesi (tramp_mem,  8, cxt);
      sh_emit_storesi (tramp_mem, 12, fnaddr);
    }
  if (TARGET_HARD_SH4)
    {
      if (!TARGET_INLINE_IC_INVALIDATE
	  || (!(TARGET_SH4A || TARGET_SH4_300) && TARGET_USERMODE))
	emit_library_call (function_symbol (NULL, "__ic_invalidate",
					    FUNCTION_ORDINARY).sym,
			   LCT_NORMAL, VOIDmode, 1, tramp, SImode);
      else
	emit_insn (gen_ic_invalidate_line (tramp));
    }
}

/* On SH5, trampolines are SHmedia code, so add 1 to the address.  */
static rtx
sh_trampoline_adjust_address (rtx tramp)
{
  return tramp;
}

/* If PIC, we cannot make sibling calls to global functions
   because the PLT requires r12 to be live.  */
static bool
sh_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  return (1
	  && ! sh_cfun_interrupt_handler_p ()
	  && (! flag_pic || TARGET_FDPIC
	      || (decl && ! (TREE_PUBLIC (decl) || DECL_WEAK (decl)))
	      || (decl && DECL_VISIBILITY (decl) != VISIBILITY_DEFAULT)));
}

/* Expand to appropriate sym*_label2reg for SYM and SIBCALL_P.  */
void
sh_expand_sym_label2reg (rtx reg, rtx sym, rtx lab, bool sibcall_p)
{
  const_tree decl = SYMBOL_REF_DECL (sym);
  bool is_weak = (decl && DECL_P (decl) && DECL_WEAK (decl));

  if (!is_weak && SYMBOL_REF_LOCAL_P (sym))
    emit_insn (gen_sym_label2reg (reg, sym, lab));
  else if (sibcall_p && SYMBOL_REF_LOCAL_P (sym))
    emit_insn (gen_symPCREL_label2reg (reg, sym, lab));
  else
    emit_insn (gen_symPLT_label2reg (reg, sym, lab));
}

/* Machine specific built-in functions.  */

struct builtin_description
{
  bool (* const is_enabled) (void);
  const enum insn_code icode;
  const char *const name;
  int signature;
  tree fndecl;
};

/* This function can be used if there are any built-ins that are not for
   SHmedia.  It's commented out to avoid the defined-but-unused warning.  */
static bool
sh1_builtin_p (void)
{
  return TARGET_SH1;
}

/* describe number and signedness of arguments; arg[0] == result
   (1: unsigned, 2: signed, 4: don't care, 8: pointer 0: no argument */
/* 9: 64-bit pointer, 10: 32-bit pointer */
static const char signature_args[][4] =
{
#define SH_BLTIN_V2SI2 0
  { 4, 4 },
#define SH_BLTIN_V4HI2 1
  { 4, 4 },
#define SH_BLTIN_V2SI3 2
  { 4, 4, 4 },
#define SH_BLTIN_V4HI3 3
  { 4, 4, 4 },
#define SH_BLTIN_V8QI3 4
  { 4, 4, 4 },
#define SH_BLTIN_MAC_HISI 5
  { 1, 4, 4, 1 },
#define SH_BLTIN_SH_HI 6
  { 4, 4, 1 },
#define SH_BLTIN_SH_SI 7
  { 4, 4, 1 },
#define SH_BLTIN_V4HI2V2SI 8
  { 4, 4, 4 },
#define SH_BLTIN_V4HI2V8QI 9
  { 4, 4, 4 },
#define SH_BLTIN_SISF 10
  { 4, 2 },
#define SH_BLTIN_LDUA_L 11
  { 2, 10 },
#define SH_BLTIN_LDUA_Q 12
  { 1, 10 },
#define SH_BLTIN_STUA_L 13
  { 0, 10, 2 },
#define SH_BLTIN_STUA_Q 14
  { 0, 10, 1 },
#define SH_BLTIN_LDUA_L64 15
  { 2, 9 },
#define SH_BLTIN_LDUA_Q64 16
  { 1, 9 },
#define SH_BLTIN_STUA_L64 17
  { 0, 9, 2 },
#define SH_BLTIN_STUA_Q64 18
  { 0, 9, 1 },
#define SH_BLTIN_NUM_SHARED_SIGNATURES 19
#define SH_BLTIN_2 19
#define SH_BLTIN_SU 19
  { 1, 2 },
#define SH_BLTIN_3 20
#define SH_BLTIN_SUS 20
  { 2, 2, 1 },
#define SH_BLTIN_PSSV 21
  { 0, 8, 2, 2 },
#define SH_BLTIN_XXUU 22
#define SH_BLTIN_UUUU 22
  { 1, 1, 1, 1 },
#define SH_BLTIN_PV 23
  { 0, 8 },
#define SH_BLTIN_VP 24
  { 8, 0 },
#define SH_BLTIN_UV 25
  { 1, 0 },
#define SH_BLTIN_VU 26
  { 0, 1 },
};
/* mcmv: operands considered unsigned.  */
/* mmulsum_wq, msad_ubq: result considered unsigned long long.  */
/* mperm: control value considered unsigned int.  */
/* mshalds, mshard, mshards, mshlld, mshlrd: shift count is unsigned int.  */
/* mshards_q: returns signed short.  */
/* nsb: takes long long arg, returns unsigned char.  */
static struct builtin_description bdesc[] =
{
  { sh1_builtin_p,
    CODE_FOR_sts_fpscr, "__builtin_sh_get_fpscr", SH_BLTIN_UV, 0 },
  { sh1_builtin_p,
    CODE_FOR_set_fpscr, "__builtin_sh_set_fpscr", SH_BLTIN_VU, 0 },
};

static tree sh_builtin_get_fpscr;
static tree sh_builtin_set_fpscr;

static void
sh_init_builtins (void)
{
  tree shared[SH_BLTIN_NUM_SHARED_SIGNATURES];
  memset (shared, 0, sizeof shared);

  for (unsigned int di = 0; di < ARRAY_SIZE (bdesc); ++di)
    {
      builtin_description* d = &bdesc[di];

      if (!d->is_enabled ())
	continue;

      tree type, arg_type = NULL_TREE;
      int signature = d->signature;

      if (signature < SH_BLTIN_NUM_SHARED_SIGNATURES && shared[signature])
	type = shared[signature];
      else
	{
	  int has_result = signature_args[signature][0] != 0;
	  tree args[3];

	  if (! TARGET_FPU_ANY
	      && FLOAT_MODE_P (insn_data[d->icode].operand[0].mode))
	    continue;
	  for (unsigned int i = 0; i < ARRAY_SIZE (args); i++)
	    args[i] = NULL_TREE;
	  for (int i = 3; ; i--)
	    {
	      int arg = signature_args[signature][i];
	      int opno = i - 1 + has_result;

	      if (arg & 8)
		arg_type = ptr_type_node;
	      else if (arg)
		arg_type = (*lang_hooks.types.type_for_mode)
		  (insn_data[d->icode].operand[opno].mode, (arg & 1));
	      else if (i)
		continue;
	      else
		arg_type = void_type_node;
	      if (i == 0)
		break;
	      args[i-1] = arg_type;
	    }
	  type = build_function_type_list (arg_type, args[0], args[1],
					   args[2], NULL_TREE);
	  if (signature < SH_BLTIN_NUM_SHARED_SIGNATURES)
	    shared[signature] = type;
	}
      d->fndecl =
	add_builtin_function (d->name, type, d - bdesc, BUILT_IN_MD,
			      NULL, NULL_TREE);
      /* Recode {sts,set}_fpscr decls for sh_atomic_assign_expand_fenv.  */
      if (d->icode == CODE_FOR_sts_fpscr)
	sh_builtin_get_fpscr = d->fndecl;
      else if (d->icode == CODE_FOR_set_fpscr)
	sh_builtin_set_fpscr = d->fndecl;
    }
}

/* Implement TARGET_ATOMIC_ASSIGN_EXPAND_FENV.  */

static void
sh_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  const unsigned SH_FE_INVALID = 64;
  const unsigned SH_FE_DIVBYZERO = 32;
  const unsigned SH_FE_OVERFLOW = 16;
  const unsigned SH_FE_UNDERFLOW = 8;
  const unsigned SH_FE_INEXACT = 4;
  const unsigned HOST_WIDE_INT SH_FE_ALL_EXCEPT = (SH_FE_INVALID
						   | SH_FE_DIVBYZERO
						   | SH_FE_OVERFLOW
						   | SH_FE_UNDERFLOW
						   | SH_FE_INEXACT);
  const unsigned HOST_WIDE_INT SH_FE_EXCEPT_SHIFT = 5;
  tree fenv_var, mask, ld_fenv, masked_fenv;
  tree new_fenv_var, reload_fenv, restore_fnenv;
  tree update_call, atomic_feraiseexcept, hold_fnclex;

  if (! TARGET_FPU_ANY)
    return;

  /* Generate the equivalent of :
       unsigned int fenv_var;
       fenv_var = __builtin_sh_get_fpscr ();

       unsigned int masked_fenv;
       masked_fenv = fenv_var & mask;

       __builtin_sh_set_fpscr (masked_fenv);  */

  fenv_var = create_tmp_var_raw (unsigned_type_node);
  mask = build_int_cst (unsigned_type_node,
			~((SH_FE_ALL_EXCEPT << SH_FE_EXCEPT_SHIFT)
			  | SH_FE_ALL_EXCEPT));
  ld_fenv = build2 (MODIFY_EXPR, unsigned_type_node,
		    fenv_var, build_call_expr (sh_builtin_get_fpscr, 0));
  masked_fenv = build2 (BIT_AND_EXPR, unsigned_type_node, fenv_var, mask);
  hold_fnclex = build_call_expr (sh_builtin_set_fpscr, 1, masked_fenv);
  fenv_var = build4 (TARGET_EXPR, unsigned_type_node, fenv_var,
		     build2 (COMPOUND_EXPR, void_type_node, masked_fenv,
			     ld_fenv),
		     NULL_TREE, NULL_TREE);
  *hold = build2 (COMPOUND_EXPR, void_type_node, fenv_var, hold_fnclex);

  /* Store the value of masked_fenv to clear the exceptions:
     __builtin_sh_set_fpscr (masked_fenv);  */

  *clear = build_call_expr (sh_builtin_set_fpscr, 1, masked_fenv);

  /* Generate the equivalent of :
       unsigned int new_fenv_var;
       new_fenv_var = __builtin_sh_get_fpscr ();

       __builtin_sh_set_fpscr (fenv_var);

       __atomic_feraiseexcept (new_fenv_var);  */

  new_fenv_var = create_tmp_var_raw (unsigned_type_node);
  reload_fenv = build2 (MODIFY_EXPR, unsigned_type_node, new_fenv_var,
			build_call_expr (sh_builtin_get_fpscr, 0));
  restore_fnenv = build_call_expr (sh_builtin_set_fpscr, 1, fenv_var);
  atomic_feraiseexcept = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  update_call = build_call_expr (atomic_feraiseexcept, 1,
				 fold_convert (integer_type_node,
					       new_fenv_var));
  *update = build2 (COMPOUND_EXPR, void_type_node,
		    build2 (COMPOUND_EXPR, void_type_node,
			    reload_fenv, restore_fnenv), update_call);
}

/* Implements target hook vector_mode_supported_p.  */
bool
sh_vector_mode_supported_p (machine_mode mode ATTRIBUTE_UNUSED)
{
  return false;
}

bool
sh_frame_pointer_required (void)
{
/* If needed override this in other tm.h files to cope with various OS 
   lossage requiring a frame pointer.  */
  if (SUBTARGET_FRAME_POINTER_REQUIRED)
    return true;

  if (crtl->profile)
    return true;

  return false;
}

/* Implements target hook dwarf_calling_convention.  Return an enum
   of dwarf_calling_convention.  */
int
sh_dwarf_calling_convention (const_tree func)
{
  if (sh_attr_renesas_p (func))
    return DW_CC_GNU_renesas_sh;

  return DW_CC_normal;
}

/* Returns the sh builtin decl for CODE.  */
static tree
sh_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= ARRAY_SIZE (bdesc))
    return error_mark_node;

  if (!bdesc[code].is_enabled ())
    return error_mark_node;

  return bdesc[code].fndecl;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */
static rtx
sh_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
		   machine_mode mode ATTRIBUTE_UNUSED, int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  const struct builtin_description *d = &bdesc[fcode];
  enum insn_code icode = d->icode;
  int signature = d->signature;
  int nop = 0;
  rtx op[4];

  if (signature_args[signature][0])
    {
      if (ignore)
	return NULL_RTX;

      machine_mode tmode = insn_data[icode].operand[0].mode;
      if (! target || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      op[nop++] = target;
    }
  else
    target = NULL_RTX;

  for (int i = 1; i <= 3; i++, nop++)
    {
      if (! signature_args[signature][i])
	break;
      tree arg = CALL_EXPR_ARG (exp, i - 1);
      if (arg == error_mark_node)
	return const0_rtx;

      machine_mode opmode;
      tree optype;
      if (signature_args[signature][i] & 8)
	{
	  opmode = ptr_mode;
	  optype = ptr_type_node;
	}
      else
	{
	  opmode = insn_data[icode].operand[nop].mode;
	  optype = (*lang_hooks.types.type_for_mode) (opmode, 0);
	}

      machine_mode argmode = TYPE_MODE (TREE_TYPE (arg));
      if (argmode != opmode)
	arg = build1 (NOP_EXPR, optype, arg);
      op[nop] = expand_expr (arg, NULL_RTX, opmode, EXPAND_NORMAL);
      if (! (*insn_data[icode].operand[nop].predicate) (op[nop], opmode))
	op[nop] = copy_to_mode_reg (opmode, op[nop]);
    }

  rtx pat = NULL_RTX;

  switch (nop)
    {
    case 1:
      pat = (*insn_data[d->icode].genfun) (op[0]);
      break;
    case 2:
      pat = (*insn_data[d->icode].genfun) (op[0], op[1]);
      break;
    case 3:
      pat = (*insn_data[d->icode].genfun) (op[0], op[1], op[2]);
      break;
    case 4:
      pat = (*insn_data[d->icode].genfun) (op[0], op[1], op[2], op[3]);
      break;
    default:
      gcc_unreachable ();
    }
  if (! pat)
    return NULL_RTX;
  emit_insn (pat);
  return target;
}

/* Return true if hard register REGNO can hold a value of machine-mode MODE.
   We can allow any mode in any general register.  The special registers
   only allow SImode.  Don't allow any mode in the PR.

   We cannot hold DCmode values in the XD registers because alter_reg
   handles subregs of them incorrectly.  We could work around this by
   spacing the XD registers like the DR registers, but this would require
   additional memory in every compilation to hold larger register vectors.
   We could hold SFmode / SCmode values in XD registers, but that
   would require a tertiary reload when reloading from / to memory,
   and a secondary reload to reload from / to general regs; that
   seems to be a losing proposition.

   We want to allow TImode FP regs so that when V4SFmode is loaded as TImode,
   it won't be ferried through GP registers first.  */
bool
sh_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (SPECIAL_REGISTER_P (regno))
    return mode == SImode;

  if (regno == FPUL_REG)
    return (mode == SImode || mode == SFmode);

  if (FP_REGISTER_P (regno) && mode == SFmode)
    return true;

  if (mode == V2SFmode)
    {
      if (((FP_REGISTER_P (regno) && (regno - FIRST_FP_REG) % 2 == 0)
	   || GENERAL_REGISTER_P (regno)))
	return true;
      else
	return false;
    }

  if (mode == V4SFmode)
    {
      if ((FP_REGISTER_P (regno) && (regno - FIRST_FP_REG) % 4 == 0)
	  || GENERAL_REGISTER_P (regno))
	return true;
      else
	return false;
    }

  if (mode == V16SFmode)
    return regno == FIRST_XD_REG;

  if (FP_REGISTER_P (regno))
    {
      if (mode == SFmode
	  || mode == SImode
	  || ((TARGET_SH2E) && mode == SCmode)
	  || (((TARGET_FPU_DOUBLE && mode == DFmode) || mode == DCmode)
	      && ((regno - FIRST_FP_REG) & 1) == 0)
	  || (TARGET_SH4 && mode == TImode
	      && ((regno - FIRST_FP_REG) & 3) == 0))
	return true;
      else
	return false;
    }

  if (XD_REGISTER_P (regno))
    return mode == DFmode;

  if (regno == PR_REG)
    return mode == SImode;

  if (regno == FPSCR_REG)
    return mode == SImode;

  return true;
}

/* Specify the modes required to caller save a given hard regno.
   choose_hard_reg_mode chooses mode based on HARD_REGNO_MODE_OK
   and returns ?Imode for float regs when sh_hard_regno_mode_ok
   permits integer modes on them.  That makes LRA's split process
   unhappy.  See PR55212.
 */
machine_mode
sh_hard_regno_caller_save_mode (unsigned int regno, unsigned int nregs,
				machine_mode mode)
{
  if (FP_REGISTER_P (regno)
      && (mode == SFmode
	  || mode == SCmode
	  || ((mode == DFmode || mode == DCmode)
	      && ((regno - FIRST_FP_REG) & 1) == 0)))
    return mode;

  return choose_hard_reg_mode (regno, nregs, false);
}

/* Return the class of registers for which a mode change from FROM to TO
   is invalid.  */
bool
sh_cannot_change_mode_class (machine_mode from, machine_mode to,
			     enum reg_class rclass)
{
  /* We want to enable the use of SUBREGs as a means to
     VEC_SELECT a single element of a vector.  */

  /* This effectively disallows using GENERAL_REGS for SFmode vector subregs.
     This can be problematic when SFmode vector subregs need to be accessed
     on the stack with displacement addressing, as it happens with -O0.
     Thus we disallow the mode change for -O0.  */
  if (to == SFmode && VECTOR_MODE_P (from) && GET_MODE_INNER (from) == SFmode)
    return optimize ? (reg_classes_intersect_p (GENERAL_REGS, rclass)) : false;

  if (GET_MODE_SIZE (from) != GET_MODE_SIZE (to))
    {
      if (TARGET_LITTLE_ENDIAN)
	{
	  if (GET_MODE_SIZE (to) < 8 || GET_MODE_SIZE (from) < 8)
	    return reg_classes_intersect_p (DF_REGS, rclass);
	}
      else
	{
	  if (GET_MODE_SIZE (from) < 8)
	    return reg_classes_intersect_p (DF_REGS, rclass);
	}
    }
  return false;
}

/* Return true if registers in machine mode MODE will likely be
   allocated to registers in small register classes.  */
bool
sh_small_register_classes_for_mode_p (machine_mode mode ATTRIBUTE_UNUSED)
{
  return true;
}

/* If ADDRESS refers to a CODE_LABEL, add NUSES to the number of times
   that label is used.  */
void
sh_mark_label (rtx address, int nuses)
{
  if (GOTOFF_P (address))
    {
      /* Extract the label or symbol.  */
      address = XEXP (address, 0);
      if (GET_CODE (address) == PLUS)
	address = XEXP (address, 0);
      address = XVECEXP (address, 0, 0);
    }
  if (GET_CODE (address) == LABEL_REF
      && LABEL_P (XEXP (address, 0)))
    LABEL_NUSES (XEXP (address, 0)) += nuses;
}

/* Compute extra cost of moving data between one register class
   and another.

   If SECONDARY*_RELOAD_CLASS says something about the src/dst pair, regclass
   uses this information.  Hence, the general register <-> floating point
   register information here is not used for SFmode.  */
static int
sh_register_move_cost (machine_mode mode,
		       reg_class_t srcclass, reg_class_t dstclass)
{
  if (dstclass == T_REGS || dstclass == PR_REGS)
    return 10;

  if (dstclass == MAC_REGS && srcclass == MAC_REGS)
    return 4;

  if (mode == SImode && TARGET_FMOVD
      && REGCLASS_HAS_FP_REG (srcclass)
      && REGCLASS_HAS_FP_REG (dstclass))
    return 4;

  if (REGCLASS_HAS_FP_REG (dstclass) && srcclass == T_REGS)
    return ((TARGET_HARD_SH4 && !optimize_size) ? 10 : 7);

  if ((REGCLASS_HAS_FP_REG (dstclass) && srcclass == MAC_REGS)
      || (dstclass == MAC_REGS && REGCLASS_HAS_FP_REG (srcclass)))
    return 9;

  if ((REGCLASS_HAS_FP_REG (dstclass)
       && REGCLASS_HAS_GENERAL_REG (srcclass))
      || (REGCLASS_HAS_GENERAL_REG (dstclass)
	  && REGCLASS_HAS_FP_REG (srcclass)))
    {
      /* Discourage trying to use fp regs for a pointer.  This also
	 discourages fp regs with SImode because Pmode is an alias
	 of SImode on this target.  See PR target/48596.  */
      int addend = (mode == Pmode) ? 40 : 0;

      return ((TARGET_FMOVD ? 8 : 12) + addend)
	     * ((GET_MODE_SIZE (mode) + 7) / 8U);
    }

  if ((dstclass == FPUL_REGS
       && REGCLASS_HAS_GENERAL_REG (srcclass))
      || (srcclass == FPUL_REGS
	  && REGCLASS_HAS_GENERAL_REG (dstclass)))
    return 5;

  if ((dstclass == FPUL_REGS
       && (srcclass == PR_REGS || srcclass == MAC_REGS || srcclass == T_REGS))
      || (srcclass == FPUL_REGS
	  && (dstclass == PR_REGS || dstclass == MAC_REGS)))
    return 7;

  if ((srcclass == FPSCR_REGS && ! REGCLASS_HAS_GENERAL_REG (dstclass))
      || (dstclass == FPSCR_REGS && ! REGCLASS_HAS_GENERAL_REG (srcclass)))
  return 4;

  if (TARGET_FMOVD
      && ! REGCLASS_HAS_GENERAL_REG (srcclass)
      && ! REGCLASS_HAS_GENERAL_REG (dstclass))
    return 2 * ((GET_MODE_SIZE (mode) + 7) / 8U);

  return 2 * ((GET_MODE_SIZE (mode) + 3) / 4U);
}

static rtx
emit_load_ptr (rtx reg, rtx addr)
{
  rtx mem = gen_const_mem (ptr_mode, addr);

  if (Pmode != ptr_mode)
    mem = gen_rtx_SIGN_EXTEND (Pmode, mem);
  return emit_move_insn (reg, mem);
}

static void
sh_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
		    HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
		    tree function)
{
  CUMULATIVE_ARGS cum;
  int structure_value_byref = 0;
  rtx this_rtx, this_value, sibcall, funexp;
  rtx_insn *insns;
  tree funtype = TREE_TYPE (function);
  int simple_add = CONST_OK_FOR_ADD (delta);
  int did_load = 0;
  rtx scratch0, scratch1, scratch2;

  reload_completed = 1;
  epilogue_completed = 1;
  crtl->uses_only_leaf_regs = 1;

  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Find the "this" pointer.  We have such a wide range of ABIs for the
     SH that it's best to do this completely machine independently.
     "this" is passed as first argument, unless a structure return pointer
     comes first, in which case "this" comes second.  */
  INIT_CUMULATIVE_ARGS (cum, funtype, NULL_RTX, 0, 1);
#ifndef PCC_STATIC_STRUCT_RETURN
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    structure_value_byref = 1;
#endif /* not PCC_STATIC_STRUCT_RETURN */
  if (structure_value_byref && sh_struct_value_rtx (function, 0) == 0)
    {
      tree ptype = build_pointer_type (TREE_TYPE (funtype));

      sh_function_arg_advance (pack_cumulative_args (&cum), Pmode, ptype, true);
    }
  this_rtx
    = sh_function_arg (pack_cumulative_args (&cum), Pmode, ptr_type_node, true);

  /* For SHcompact, we only have r0 for a scratch register: r1 is the
     static chain pointer (even if you can't have nested virtual functions
     right now, someone might implement them sometime), and the rest of the
     registers are used for argument passing, are callee-saved, or reserved.  */
  /* We need to check call_used_regs / fixed_regs in case -fcall_saved-reg /
     -ffixed-reg has been used.  */
  if (! call_used_regs[0] || fixed_regs[0])
    error ("r0 needs to be available as a call-clobbered register");
  scratch0 = scratch1 = scratch2 = gen_rtx_REG (Pmode, 0);

    {
      if (call_used_regs[1] && ! fixed_regs[1])
	scratch1 = gen_rtx_REG (ptr_mode, 1);
      /* N.B., if not TARGET_HITACHI, register 2 is used to pass the pointer
	 pointing where to return struct values.  */
      if (call_used_regs[3] && ! fixed_regs[3])
	scratch2 = gen_rtx_REG (Pmode, 3);
    }

  this_value = plus_constant (Pmode, this_rtx, delta);
  if (vcall_offset
      && (simple_add || scratch0 != scratch1)
      && strict_memory_address_p (ptr_mode, this_value))
    {
      emit_load_ptr (scratch0, this_value);
      did_load = 1;
    }

  if (!delta)
    ; /* Do nothing.  */
  else if (simple_add)
    emit_move_insn (this_rtx, this_value);
  else
    {
      emit_move_insn (scratch1, GEN_INT (delta));
      emit_insn (gen_add2_insn (this_rtx, scratch1));
    }

  if (vcall_offset)
    {
      rtx offset_addr;

      if (!did_load)
	emit_load_ptr (scratch0, this_rtx);

      offset_addr = plus_constant (Pmode, scratch0, vcall_offset);
      if (strict_memory_address_p (ptr_mode, offset_addr))
	; /* Do nothing.  */
      else if (scratch0 != scratch1)
	{
	  /* scratch0 != scratch1, and we have indexed loads.  Get better
	     schedule by loading the offset into r1 and using an indexed
	     load - then the load of r1 can issue before the load from
	     (this_rtx + delta) finishes.  */
	  emit_move_insn (scratch1, GEN_INT (vcall_offset));
	  offset_addr = gen_rtx_PLUS (Pmode, scratch0, scratch1);
	}
      else if (CONST_OK_FOR_ADD (vcall_offset))
	{
	  emit_insn (gen_add2_insn (scratch0, GEN_INT (vcall_offset)));
	  offset_addr = scratch0;
	}
      else if (scratch0 != scratch1)
	{
	  emit_move_insn (scratch1, GEN_INT (vcall_offset));
	  emit_insn (gen_add2_insn (scratch0, scratch1));
	  offset_addr = scratch0;
	}
      else
	gcc_unreachable (); /* FIXME */
      emit_load_ptr (scratch0, offset_addr);

      if (Pmode != ptr_mode)
	scratch0 = gen_rtx_TRUNCATE (ptr_mode, scratch0);
      emit_insn (gen_add2_insn (this_rtx, scratch0));
    }

  /* Generate a tail call to the target function.  */
  if (! TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  /* If the function is overridden, so is the thunk, hence we don't
     need GOT addressing even if this is a public symbol.  */
#if 0
  if (TARGET_SH1 && ! flag_weak)
    sibcall = gen_sibcalli_thunk (funexp, const0_rtx);
  else
#endif
  if (TARGET_SH2 && flag_pic)
    {
      if (TARGET_FDPIC)
	{
	  sibcall = gen_sibcall_pcrel_fdpic (funexp, const0_rtx);
	  XEXP (XVECEXP (sibcall, 0, 3), 0) = scratch2;
	}
      else
	{
	  sibcall = gen_sibcall_pcrel (funexp, const0_rtx);
	  XEXP (XVECEXP (sibcall, 0, 2), 0) = scratch2;
	}
    }
  else
    {
      emit_move_insn (scratch2, funexp);
      funexp = gen_rtx_MEM (FUNCTION_MODE, scratch2);
      sibcall = gen_sibcall (funexp, const0_rtx, NULL_RTX);
    }
  sibcall = emit_call_insn (sibcall);
  SIBLING_CALL_P (sibcall) = 1;
  use_reg (&CALL_INSN_FUNCTION_USAGE (sibcall), this_rtx);
  emit_barrier ();

  /* Run just enough of rest_of_compilation to do scheduling and get
     the insns emitted.  Note that use_thunk calls
     assemble_start_function and assemble_end_function.  */

  insns = get_insns ();

  if (optimize > 0)
    {
      if (! cfun->cfg)
	init_flow (cfun);
      split_all_insns_noflow ();
    }

  sh_reorg ();
  shorten_branches (insns);
  final_start_function (insns, file, 1);
  final (insns, file, 1);
  final_end_function ();

  reload_completed = 0;
  epilogue_completed = 0;
}

/* Return an RTX pair for the address and call site label of a function
   NAME of kind KIND, placing the result in TARGET if not NULL.  For
   SFUNC_STATIC, if FDPIC, the LAB member of result will be set to
   (const_int 0) if jsr should be used, or a label_ref if bsrf should
   be used.  For FDPIC, both SFUNC_GOT and SFUNC_STATIC will return the
   address of the function itself, not a function descriptor, so they
   can only be used with functions not using the FDPIC register that
   are known to be called directory without a PLT entry.  */

function_symbol_result
function_symbol (rtx target, const char *name, sh_function_kind kind)
{
  /* If this is not an ordinary function, the name usually comes from a
     string literal or an sprintf buffer.  Make sure we use the same
     string consistently, so that cse will be able to unify address loads.  */
  if (kind != FUNCTION_ORDINARY)
    name = IDENTIFIER_POINTER (get_identifier (name));
  rtx sym = gen_rtx_SYMBOL_REF (Pmode, name);
  rtx lab = const0_rtx;
  SYMBOL_REF_FLAGS (sym) = SYMBOL_FLAG_FUNCTION;
  if (flag_pic)
    switch (kind)
      {
      case FUNCTION_ORDINARY:
	break;
      case SFUNC_GOT:
	{
	  rtx reg = target ? target : gen_reg_rtx (Pmode);

	  emit_insn (gen_symGOT2reg (reg, sym));
	  sym = reg;
	  break;
	}
      case SFUNC_STATIC:
	{
	  rtx reg = target ? target : gen_reg_rtx (Pmode);

	  if (TARGET_FDPIC)
	    {
	      /* We use PC-relative calls, since GOTOFF can only refer
		 to writable data.  This works along with sh_sfunc_call.  */
 	      lab = PATTERN (gen_call_site ());
	      emit_insn (gen_sym_label2reg (reg, sym, lab));
	    }
	  else
	    {
	      /* ??? To allow cse to work, we use GOTOFF relocations.
		 we could add combiner patterns to transform this into
		 straight pc-relative calls with sym2PIC / bsrf when
		 label load and function call are still 1:1 and in the
		 same basic block during combine.  */
	      emit_insn (gen_symGOTOFF2reg (reg, sym));
	    }

	  sym = reg;
	  break;
	}
      }
  if (target && sym != target)
    {
      emit_move_insn (target, sym);
      return function_symbol_result (target, lab);
    }
  return function_symbol_result (sym, lab);
}

/* Find the number of the first general purpose register in S that
   is not set.  */
static int
scavenge_reg (HARD_REG_SET *s)
{
  for (int r = FIRST_GENERAL_REG; r <= LAST_GENERAL_REG; r++)
    if (TEST_HARD_REG_BIT (*s, r))
      return r;
  return -1;
}

rtx
sh_get_pr_initial_val (void)
{
  /* If we haven't finished rtl generation, there might be a nonlocal label
     that we haven't seen yet.
     ??? get_hard_reg_initial_val fails if it is called after register
     allocation has started, unless it has been called before for the
     same register.  And even then, we end in trouble if we didn't use
     the register in the same basic block before.  So call
     get_hard_reg_initial_val now and wrap it in an unspec if we might
     need to replace it.  */
  /* ??? We also must do this for TARGET_SH1 in general, because otherwise
     combine can put the pseudo returned by get_hard_reg_initial_val into
     instructions that need a general purpose registers, which will fail to
     be recognized when the pseudo becomes allocated to PR.  */
  rtx val = get_hard_reg_initial_val (Pmode, PR_REG);
  return gen_rtx_UNSPEC (SImode, gen_rtvec (1, val), UNSPEC_RA);
}

bool
sh_expand_t_scc (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx target = operands[0];
  rtx op0 = operands[2];
  rtx op1 = operands[3];
  rtx result = target;

  if (!REG_P (op0) || REGNO (op0) != T_REG
      || !CONST_INT_P (op1))
    return false;
  if (!REG_P (result))
    result = gen_reg_rtx (SImode);
  HOST_WIDE_INT val = INTVAL (op1);
  if ((code == EQ && val == 1) || (code == NE && val == 0))
    emit_insn (gen_movt (result, get_t_reg_rtx ()));
  else if ((code == EQ && val == 0) || (code == NE && val == 1))
    emit_insn (gen_movnegt (result, get_t_reg_rtx ()));
  else if (code == EQ || code == NE)
    emit_insn (gen_move_insn (result, GEN_INT (code == NE)));
  else
    return false;
  if (result != target)
    emit_move_insn (target, result);
  return true;
}

/* INSN is an sfunc; return the rtx that describes the address used.  */
static rtx
extract_sfunc_addr (rtx insn)
{
  rtx pattern = PATTERN (insn);
  const int len = XVECLEN (pattern, 0);
  for (int i = 0; i < len; i++)
    {
      rtx part = XVECEXP (pattern, 0, i);
      if (GET_CODE (part) == USE && GET_MODE (XEXP (part, 0)) == Pmode
	  && GENERAL_REGISTER_P (true_regnum (XEXP (part, 0))))
	return XEXP (part, 0);
    }
  gcc_assert (GET_CODE (XVECEXP (pattern, 0, 0)) == UNSPEC_VOLATILE);
  return XVECEXP (XVECEXP (pattern, 0, 0), 0, 1);
}

/* Verify that the register in use_sfunc_addr still agrees with the address
   used in the sfunc.  This prevents fill_slots_from_thread from changing
   use_sfunc_addr.
   INSN is the use_sfunc_addr instruction, and REG is the register it
   guards.  */
bool
check_use_sfunc_addr (rtx_insn *insn, rtx reg)
{
  /* Search for the sfunc.  It should really come right after INSN.  */
  while ((insn = NEXT_INSN (insn)))
    {
      if (LABEL_P (insn) || JUMP_P (insn))
	break;
      if (! INSN_P (insn))
	continue;

      if (rtx_sequence *seq = dyn_cast<rtx_sequence *> (PATTERN (insn)))
	insn = seq->insn (0);
      if (GET_CODE (PATTERN (insn)) != PARALLEL
	  || get_attr_type (insn) != TYPE_SFUNC)
	continue;
      return rtx_equal_p (extract_sfunc_addr (insn), reg);
    }
  gcc_unreachable ();
}

/* This function returns a constant rtx that represents 2**15 / pi in
   SFmode.  It's used to scale a fixed-point signed 16.16-bit fraction
   of a full circle back to an SFmode value, i.e. 0x10000 maps to 2*pi.  */
static GTY(()) rtx sh_fsca_sf2int_rtx;

rtx
sh_fsca_sf2int (void)
{
  if (! sh_fsca_sf2int_rtx)
    {
      REAL_VALUE_TYPE rv;

      real_from_string (&rv, "10430.378350470453");
      sh_fsca_sf2int_rtx = const_double_from_real_value (rv, SFmode);
    }

  return sh_fsca_sf2int_rtx;
}

/* This function returns a constant rtx that represents pi / 2**15 in
   SFmode.  It's used to scale SFmode angles, in radians, to a
   fixed-point signed 16.16-bit fraction of a full circle, i.e. 2*pi
   maps to 0x10000.  */
static GTY(()) rtx sh_fsca_int2sf_rtx;

rtx
sh_fsca_int2sf (void)
{
  if (! sh_fsca_int2sf_rtx)
    {
      REAL_VALUE_TYPE rv;

      real_from_string (&rv, "9.587379924285257e-5");
      sh_fsca_int2sf_rtx = const_double_from_real_value (rv, SFmode);
    }

  return sh_fsca_int2sf_rtx;
}

/* Initialize the CUMULATIVE_ARGS structure.  */
void
sh_init_cumulative_args (CUMULATIVE_ARGS *  pcum,
			 tree		    fntype,
			 rtx		    libname ATTRIBUTE_UNUSED,
			 tree		    fndecl,
			 signed int	    n_named_args,
			 machine_mode  mode)
{
  pcum->arg_count [(int) SH_ARG_FLOAT] = 0;
  pcum->free_single_fp_reg = 0;
  pcum->outgoing = n_named_args != -1;

  /* FIXME: Should we check TARGET_HITACHI here ???  */
  pcum->renesas_abi = sh_attr_renesas_p (fntype);

  if (fntype)
    {
      pcum->force_mem = ((TARGET_HITACHI || pcum->renesas_abi)
			 && aggregate_value_p (TREE_TYPE (fntype), fndecl));
      pcum->prototype_p = prototype_p (fntype);
      pcum->arg_count [(int) SH_ARG_INT] = false;
    }
  else
    {
      pcum->arg_count [(int) SH_ARG_INT] = 0;
      pcum->prototype_p = false;
      if (mode != VOIDmode)
	{
	  /* If the default ABI is the Renesas ABI then all library
	     calls must assume that the library will be using the
	     Renesas ABI.  So if the function would return its result
	     in memory then we must force the address of this memory
	     block onto the stack.  Ideally we would like to call
	     targetm.calls.return_in_memory() here but we do not have
	     the TYPE or the FNDECL available so we synthesize the
	     contents of that function as best we can.  */
	  pcum->force_mem =
	    (TARGET_DEFAULT & MASK_HITACHI)
	    && (mode == BLKmode
		|| (GET_MODE_SIZE (mode) > 4
		    && !(mode == DFmode
			 && TARGET_FPU_DOUBLE)));
	}
      else
	pcum->force_mem = false;
    }
}

rtx
sh_gen_truncate (machine_mode mode, rtx x, int need_sign_ext)
{
  enum rtx_code code = TRUNCATE;

  if (GET_CODE (x) == ZERO_EXTEND || GET_CODE (x) == SIGN_EXTEND)
    {
      rtx inner = XEXP (x, 0);
      machine_mode inner_mode = GET_MODE (inner);

      if (inner_mode == mode)
	return inner;
      else if (GET_MODE_SIZE (inner_mode) >= GET_MODE_SIZE (mode))
	x = inner;
      else if (GET_MODE_SIZE (inner_mode) < GET_MODE_SIZE (mode)
	       && (! need_sign_ext || GET_CODE (x) == SIGN_EXTEND))
	{
	  code = GET_CODE (x);
	  x = inner;
	}
    }
  return gen_rtx_fmt_e (code, mode, x);
}

/* Load and store depend on the highpart of the address.  However,
   set_attr_alternative does not give well-defined results before reload,
   so we must look at the rtl ourselves to see if any of the feeding
   registers is used in a memref.

   Return true iff INSN contains a MEM.  */
bool
sh_contains_memref_p (rtx insn)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
    if (MEM_P (*iter))
      return true;
  return false;
}

/* Return true iff INSN loads a banked register.  */
bool
sh_loads_bankedreg_p (rtx insn)
{
  if (GET_CODE (PATTERN (insn)) == SET)
    {
      rtx op = SET_DEST (PATTERN(insn));
      if (REG_P (op) && BANKED_REGISTER_P (REGNO (op)))
	return true;
    }

  return false;
}

/* Implement TARGET_PREFERRED_RELOAD_CLASS.  */
static reg_class_t
sh_preferred_reload_class (rtx x ATTRIBUTE_UNUSED, reg_class_t rclass)
{
  return rclass;
}

/* Implement TARGET_SECONDARY_RELOAD.  */
static reg_class_t
sh_secondary_reload (bool in_p, rtx x, reg_class_t rclass_i,
		     machine_mode mode, secondary_reload_info *sri)
{
  enum reg_class rclass = (enum reg_class) rclass_i;

  if (MEM_P (x) && GET_CODE (XEXP (x, 0)) == PLUS
      && REG_P (XEXP (XEXP (x, 0), 0))
      && REGNO (XEXP (XEXP (x, 0), 0)) == GBR_REG)
    return rclass == R0_REGS ? NO_REGS : R0_REGS;

  if (MEM_P (x) && REG_P (XEXP (x, 0)) && REGNO (XEXP (x, 0)) == GBR_REG)
    return rclass == R0_REGS ? NO_REGS : R0_REGS;

  if (REG_P (x) && REGNO (x) == GBR_REG)
    return NO_REGS;

  if (in_p)
    {
      if (REGCLASS_HAS_FP_REG (rclass)
	  && immediate_operand ((x), mode)
	  && ! ((fp_zero_operand (x) || fp_one_operand (x)) && mode == SFmode))
	switch (mode)
	  {
	  case SFmode:
	    sri->icode = CODE_FOR_reload_insf__frn;
	    return NO_REGS;
	  case DFmode:
	    sri->icode = CODE_FOR_reload_indf__frn;
	    return NO_REGS;
	  case SImode:
	    /* ??? If we knew that we are in the appropriate mode -
	       single precision - we could use a reload pattern directly.  */
	    return FPUL_REGS;
	  default:
	    abort ();
	  }
      if (rclass == FPUL_REGS
	  && ((REG_P (x) && (REGNO (x) == MACL_REG || REGNO (x) == MACH_REG
			     || REGNO (x) == T_REG))
	      || GET_CODE (x) == PLUS))
	return GENERAL_REGS;
      if (rclass == FPUL_REGS && immediate_operand (x, mode))
	{
	  if (satisfies_constraint_I08 (x) || fp_zero_operand (x))
	    return GENERAL_REGS;
	  else if (mode == SFmode)
	    return FP_REGS;
	  sri->icode = CODE_FOR_reload_insi__i_fpul;
	  return NO_REGS;
	}
      if (rclass == FPSCR_REGS
	  && ((REG_P (x) && REGNO (x) >= FIRST_PSEUDO_REGISTER)
	      || (MEM_P (x) && GET_CODE (XEXP (x, 0)) == PLUS)))
        return GENERAL_REGS;
    } /* end of input-only processing.  */

  if (((REGCLASS_HAS_FP_REG (rclass)
	&& (REG_P (x)
	    && (GENERAL_OR_AP_REGISTER_P (REGNO (x))
		|| (FP_REGISTER_P (REGNO (x)) && mode == SImode
		    && TARGET_FMOVD))))
       || (REGCLASS_HAS_GENERAL_REG (rclass)
	   && REG_P (x)
	   && FP_REGISTER_P (REGNO (x))))
      && (mode == SFmode || mode == SImode))
    return FPUL_REGS;
  if ((rclass == FPUL_REGS
       || (REGCLASS_HAS_FP_REG (rclass) && mode == SImode))
      && (MEM_P (x)
	  || (REG_P (x)
	      && (REGNO (x) >= FIRST_PSEUDO_REGISTER
		  || REGNO (x) == T_REG
		  || system_reg_operand (x, VOIDmode)))))
    {
      if (rclass == FPUL_REGS)
	return GENERAL_REGS;
      return NO_REGS;  // LRA wants NO_REGS here, it used to be FPUL_REGS;
    }

  if ((rclass == MAC_REGS || rclass == PR_REGS)
      && REG_P (x) && ! GENERAL_REGISTER_P (REGNO (x))
      && rclass != REGNO_REG_CLASS (REGNO (x)))
    return GENERAL_REGS;

 /* If here fall back to loading FPUL register through general registers.
    This case can happen when movsi_ie insn is picked initially to
    load/store the FPUL register from/to another register, and then the
    other register is allocated on the stack.  */
  if (rclass == FPUL_REGS && true_regnum (x) == -1)
    return GENERAL_REGS;

  /* Force mov.b / mov.w displacement addressing insn to use R0 as
     the other operand.
     On SH2A could also just leave it alone here, which would result in a
     4 byte move insn being generated instead.  However, for this to work
     the insns must have the appropriate alternatives.  */
  if ((mode == QImode || mode == HImode) && rclass != R0_REGS
      && satisfies_constraint_Sdd (x)
      && sh_disp_addr_displacement (x)
	 <= sh_max_mov_insn_displacement (mode, false))
    return R0_REGS;

  /* When reload is trying to address a QImode or HImode subreg on the stack, 
     force any subreg byte into R0_REGS, as this is going to become a
     displacement address.
     We could restrict this to SUBREG_BYTE (x) > 0, but if the actual reg
     is on the stack, the memref to it might already require a displacement
     and that has to be added to the final address.  At this point we don't
     know the cumulative displacement so we assume the worst case.  */
  if ((mode == QImode || mode == HImode) && rclass != R0_REGS 
      && GET_CODE (x) == SUBREG && true_regnum (x) == -1)
    return R0_REGS;

  return NO_REGS;
}

/* Return true if SUBST can't safely replace its equivalent during RA.  */
static bool
sh_cannot_substitute_mem_equiv_p (rtx)
{
  /* If SUBST is mem[base+index] or QI/HImode mem[base+disp], the insn
     uses R0 and may cause spill failure when R0 is already used.
     We have to return true for that case at least.
     Moreover SH has strong R0 parity and also have not enough numbers of
     the hard registers to make the equiv substitution win in the size
     and the speed on average working sets.  The pseudos produced to
     hold the equiv values can't get good hard registers for bad cases
     and end up memory save/restore insns which make the code worse.  */
  return true;
}

/* Return true if DISP can be legitimized.  */
static bool
sh_legitimize_address_displacement (rtx *disp, rtx *offs,
				    machine_mode mode)
{
  if ((TARGET_FPU_DOUBLE && mode == DFmode)
      || (TARGET_SH2E && mode == SFmode))
    return false;

  struct disp_adjust adj = sh_find_mov_disp_adjust (mode, INTVAL (*disp));
  if (adj.offset_adjust != NULL_RTX && adj.mov_disp != NULL_RTX)
    {
      *disp = adj.mov_disp;
      *offs = adj.offset_adjust;
      return true;
    }
 
  return false;
}

/* Return true if movsf insn should be splited with an additional
   register.  */
bool
sh_movsf_ie_ra_split_p (rtx op0, rtx op1, rtx op2)
{
  /* op0 == op1 */
  if (rtx_equal_p (op0, op1))
    return true;
  /* fy, FQ, reg */
  if (GET_CODE (op1) == CONST_DOUBLE
      && ! satisfies_constraint_G (op1)
      && ! satisfies_constraint_H (op1)
      && REG_P (op0)
      && REG_P (op2))
    return true;
  /* f, r, y */
  if (REG_P (op0) && FP_REGISTER_P (REGNO (op0))
      && REG_P (op1) && GENERAL_REGISTER_P (REGNO (op1))
      && REG_P (op2) && (REGNO (op2) == FPUL_REG))
    return true;
  /* r, f, y */
  if (REG_P (op1) && FP_REGISTER_P (REGNO (op1))
      && REG_P (op0) && GENERAL_REGISTER_P (REGNO (op0))
      && REG_P (op2) && (REGNO (op2) == FPUL_REG))
    return true;

  return false;
}

static void
sh_conditional_register_usage (void)
{
  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno ++)
    if (! VALID_REGISTER_P (regno))
      fixed_regs[regno] = call_used_regs[regno] = 1;
  /* R8 and R9 are call-clobbered on SH5, but not on earlier SH ABIs.  */
  if (flag_pic)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
  if (TARGET_FDPIC)
    {
      fixed_regs[PIC_REG] = 1;
      call_used_regs[PIC_REG] = 1;
      call_really_used_regs[PIC_REG] = 1;
    }
  /* Renesas saves and restores mac registers on call.  */
  if (TARGET_HITACHI && ! TARGET_NOMACSAVE)
    {
      call_really_used_regs[MACH_REG] = 0;
      call_really_used_regs[MACL_REG] = 0;
    }

  for (int regno = FIRST_GENERAL_REG; regno <= LAST_GENERAL_REG; regno++)
    if (! fixed_regs[regno] && call_really_used_regs[regno])
      SET_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], regno);

  call_really_used_regs[FPSCR_MODES_REG] = 0;
  call_really_used_regs[FPSCR_STAT_REG] = 0;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P

   can_store_by_pieces constructs VOIDmode CONST_DOUBLEs.  */
static bool
sh_legitimate_constant_p (machine_mode mode, rtx x)
{
  if (SH_OFFSETS_MUST_BE_WITHIN_SECTIONS_P)
    {
      rtx base, offset;
      split_const (x, &base, &offset);

      if (GET_CODE (base) == SYMBOL_REF
	  && !offset_within_block_p (base, INTVAL (offset)))
       return false;
    }

  if (TARGET_FDPIC
      && (SYMBOLIC_CONST_P (x)
	  || (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS
	      && SYMBOLIC_CONST_P (XEXP (XEXP (x, 0), 0)))))
    return false;

  return GET_CODE (x) != CONST_DOUBLE
	 || mode == DFmode || mode == SFmode
	 || mode == DImode || GET_MODE (x) == VOIDmode;
}

enum sh_divide_strategy_e sh_div_strategy = SH_DIV_STRATEGY_DEFAULT;

static void
sh_init_sync_libfuncs (void)
{
  init_sync_libfuncs (UNITS_PER_WORD);
}

/* Return true if it is appropriate to emit `ret' instructions in the
   body of a function.  */
bool
sh_can_use_simple_return_p (void)
{
  if (! reload_completed || frame_pointer_needed)
    return false;

  /* Moving prologue around does't reduce the size.  */
  if (optimize_function_for_size_p (cfun))
    return false;

  /* Finally, allow for pr save.  */
  HARD_REG_SET live_regs_mask;
  int d = calc_live_regs (&live_regs_mask);

  if (rounded_frame_size (d) > 4)
   return false;

  return true;
}

/*------------------------------------------------------------------------------
  Address mode optimization support code
*/

typedef HOST_WIDE_INT disp_t;
static const disp_t MIN_DISP = HOST_WIDE_INT_MIN;
static const disp_t MAX_DISP = HOST_WIDE_INT_MAX;
static const disp_t INVALID_DISP = MAX_DISP;

/* A memory reference which is described by a base register and a
   displacement.  */
class base_reg_disp
{
public:
  base_reg_disp (rtx br, disp_t d);

  bool is_reg (void) const;
  bool is_disp (void) const;
  rtx reg (void) const;
  disp_t disp (void) const;

private:
  rtx reg_;
  disp_t disp_;
};

inline
base_reg_disp::base_reg_disp (rtx br, disp_t d)
: reg_ (br), disp_ (d)
{
}
 
inline bool
base_reg_disp::is_reg (void) const
{
  return reg_ != NULL_RTX && disp_ != INVALID_DISP;
}

inline bool
base_reg_disp::is_disp (void) const
{
  return reg_ == NULL_RTX && disp_ != INVALID_DISP;
}

inline rtx
base_reg_disp::reg (void) const
{
  return reg_;
}

inline disp_t
base_reg_disp::disp (void) const
{
  return disp_;
}

/* Find the base register and calculate the displacement for a given
   address rtx 'x'.  */
static base_reg_disp
sh_find_base_reg_disp (rtx_insn* insn, rtx x, disp_t disp = 0,
		       rtx base_reg = NULL)
{
  if (REG_P (x))
    {
      if (REGNO (x) == GBR_REG)
	return base_reg_disp (x, disp);

      /* We've reached a hard-reg.  This is probably the point where
	 function args are copied to pseudos.  Do not go any further and
	 stick to the pseudo.  If the original mem addr was in a hard reg
	 from the beginning, it will become the base reg.  */
      if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	return base_reg_disp (base_reg != NULL ? base_reg : x, disp);

      /* Find the def of the reg and trace it.  If there are more than one
	 defs and they are not the same, assume it's not safe to proceed.  */
      rtx_insn* last_i = NULL;
      rtx last_set = NULL;
      for (df_ref d = DF_REG_DEF_CHAIN (REGNO (x)); d != NULL;
	   d = DF_REF_NEXT_REG (d))
	{
	  rtx set = const_cast<rtx> (set_of (x, DF_REF_INSN (d)));

	  /* Accept multiple defs, as long as they are equal.  */
	  if (last_set == NULL || rtx_equal_p (last_set, set))
	    {
	      last_i = DF_REF_INSN (d);
	      last_set = set;
	    }
	  else
	    {
	      last_i = NULL;
	      last_set = NULL;
	      break;
	    }
	}

      if (last_set != NULL && last_i != NULL)
	return sh_find_base_reg_disp (last_i, XEXP (last_set, 1), disp,
				      XEXP (last_set, 0));

      /* When here, no previous insn was found that sets the reg.
	 The input reg is already the base reg.  */
      return base_reg_disp (x, disp);
    }

  else if (GET_CODE (x) == PLUS)
    {
      base_reg_disp left_val = sh_find_base_reg_disp (insn, XEXP (x, 0));
      base_reg_disp right_val = sh_find_base_reg_disp (insn, XEXP (x, 1));

      /* Either left or right val must be a reg.
	 We don't handle the case of 'reg + reg' here.  */
      if (left_val.is_reg () && right_val.is_disp ())
	return base_reg_disp (left_val.reg (), left_val.disp ()
					       + right_val.disp () + disp);
      else if (right_val.is_reg () && left_val.is_disp ())
	return base_reg_disp (right_val.reg (), right_val.disp ()
						+ left_val.disp () + disp);
      else
	return base_reg_disp (base_reg, disp);
    }

  else if (CONST_INT_P (x))
    return base_reg_disp (NULL, disp + INTVAL (x));

  /* Didn't find anything useful.  */
  return base_reg_disp (base_reg, disp);
}

/* Given an insn and a memory operand, try to find an equivalent GBR
   based memory address and return the corresponding new memory address.
   Return NULL_RTX if not found.  */
rtx
sh_find_equiv_gbr_addr (rtx_insn* insn, rtx mem)
{
  if (!MEM_P (mem) || gbr_address_mem (mem, GET_MODE (mem)))
    return NULL_RTX;

  /* Leave post/pre inc/dec or any other side effect addresses alone.  */
  if (side_effects_p (XEXP (mem, 0)))
    return NULL_RTX;

  /* When not optimizing there might be no dataflow available.  */
  if (df == NULL)
    return NULL_RTX;

  base_reg_disp gbr_disp = sh_find_base_reg_disp (insn, XEXP (mem, 0));

  if (gbr_disp.is_reg () && REGNO (gbr_disp.reg ()) == GBR_REG)
    {
      /* If GBR is marked as call clobbered we bail out if we see a call.
	 FIXME: Actually should check if this mem refers to the gbr value
	 before or after the call.  If there is a store_gbr preceeding this
	 mem, it's safe to use GBR for this mem.

	 If GBR is not marked as call clobbered, but there is some other
	 def than a call, it's probably a load_gbr upon which we also
	 bail out to be on the safe side.
	 FIXME: Should check if we have a use-after-def case, such as
	 the call case above.  */
      for (df_ref d = DF_REG_DEF_CHAIN (GBR_REG); d != NULL;
	   d = DF_REF_NEXT_REG (d))
	{
	  if (CALL_P (DF_REF_INSN (d)))
	    {
	      if (REGNO_REG_SET_P (regs_invalidated_by_call_regset, GBR_REG))
		return NULL_RTX;
	      else
		continue;
	    }
	  else
	    return NULL_RTX;
	}

      rtx disp = GEN_INT (gbr_disp.disp ());
      if (gbr_displacement (disp, GET_MODE (mem)))
	return gen_rtx_PLUS (SImode, gen_rtx_REG (SImode, GBR_REG), disp);
    }

  return NULL_RTX;
}

/*------------------------------------------------------------------------------
  Manual insn combine support code.
*/

/* Return true if the specified insn contains any UNSPECs or
   UNSPEC_VOLATILEs.  */
static bool
sh_unspec_insn_p (rtx x)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (i, array, x, ALL)
    if (*i != NULL
	&& (GET_CODE (*i) == UNSPEC || GET_CODE (*i) == UNSPEC_VOLATILE))
      return true;

  return false;
}

/* Return true if the register operands of the specified insn are modified
   between the specified from and to insns (exclusive of those two).  */
bool
sh_insn_operands_modified_between_p (rtx_insn* operands_insn,
				     const rtx_insn* from,
				     const rtx_insn* to)
{
  /*  FIXME: Return true for multiple sets for now.  */
  rtx s = single_set (operands_insn);
  if (s == NULL_RTX)
    return true;

  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (i, array, SET_SRC (s), ALL)
    if (*i != NULL &&
	((REG_P (*i) || SUBREG_P (*i)) && reg_set_between_p (*i, from, to)))
      return true;

  return false;
}

/* Given an insn, determine whether it's a 'nott' insn, i.e. an insn that
   negates the T bit and stores the result in the T bit.  */
bool
sh_is_nott_insn (const rtx_insn* i)
{
  return i != NULL && GET_CODE (PATTERN (i)) == SET
	 && t_reg_operand (XEXP (PATTERN (i), 0), VOIDmode)
	 && negt_reg_operand (XEXP (PATTERN (i), 1), VOIDmode);
}

rtx
sh_movt_set_dest (const rtx_insn* i)
{
  if (i == NULL)
    return NULL;

  const_rtx p = PATTERN (i);
  return GET_CODE (p) == SET
	 && arith_reg_dest (XEXP (p, 0), SImode)
	 && t_reg_operand (XEXP (p, 1), VOIDmode) ? XEXP (p, 0) : NULL;
}

/* Given an insn, check whether it's a 'movrt' kind of insn, i.e. an insn
   that stores the negated T bit in a register, and return the destination
   register rtx, or null.  */
rtx
sh_movrt_set_dest (const rtx_insn* i)
{
  if (i == NULL)
    return NULL;

  const_rtx p = PATTERN (i);

  /* The negc movrt replacement is inside a parallel.  */
  if (GET_CODE (p) == PARALLEL)
    p = XVECEXP (p, 0, 0);

  return GET_CODE (p) == SET
	 && arith_reg_dest (XEXP (p, 0), SImode)
	 && negt_reg_operand (XEXP (p, 1), VOIDmode) ? XEXP (p, 0) : NULL;
}

/* Given an insn and a reg number, tell whether the reg dies or is unused
   after the insn.  */
bool
sh_reg_dead_or_unused_after_insn (const rtx_insn* i, int regno)
{
  return find_regno_note (i, REG_DEAD, regno) != NULL
	 || find_regno_note (i, REG_UNUSED, regno) != NULL;
}

/* Given an insn and a reg number, remove reg dead or reg unused notes to
   mark it as being used after the insn.  */
void
sh_remove_reg_dead_or_unused_notes (rtx_insn* i, int regno)
{
  if (rtx n = find_regno_note (i, REG_DEAD, regno))
    remove_note (i, n);
  if (rtx n = find_regno_note (i, REG_UNUSED, regno))
    remove_note (i, n);
}

/* Given an insn check if it contains any post/pre inc/dec mem operands and
   add the REG_INC notes accordingly.
   FIXME: This function is very similar to lra.c (add_auto_inc_notes).
   FIXME: This function is currently used by peephole2 patterns because
	  the peephole2 pass does not preserve REG_INC notes.  If the notes
	  are dropped the following passes will do wrong things.  */
rtx_insn*
sh_check_add_incdec_notes (rtx_insn* i)
{
  struct for_each_inc_dec_clb
  {
    static int func (rtx mem ATTRIBUTE_UNUSED, rtx op ATTRIBUTE_UNUSED,
		     rtx dest, rtx src ATTRIBUTE_UNUSED,
		     rtx srcoff ATTRIBUTE_UNUSED, void* arg)
    {
      gcc_assert (REG_P (dest));

      rtx_insn* i = (rtx_insn*)arg;
      if (find_regno_note (i, REG_INC, REGNO (dest)) == NULL)
	add_reg_note (i, REG_INC, dest);

      return 0;
    }
  };

  for_each_inc_dec (PATTERN (i), for_each_inc_dec_clb::func, i);
  return i;
}

/* Given a move insn destiation and a source, make sure that the move source
   operand is not a post-inc mem load with the same address reg as the
   destination.  Returns the modified source operand with the post-inc removed
   if necessary.  */
rtx
sh_remove_overlapping_post_inc (rtx dst, rtx src)
{
  if (!MEM_P (src))
    return src;

  rtx addr = XEXP (src, 0);

  if (GET_CODE (addr) == POST_INC
      && reg_overlap_mentioned_p (XEXP (addr, 0), dst))
    return replace_equiv_address (src, XEXP (addr, 0));

  gcc_assert (GET_CODE (addr) != POST_MODIFY);
  return src;
}

/* Emit a move insn that is safe to be used in peephole patterns.  */
rtx_insn*
sh_peephole_emit_move_insn (rtx dst, rtx src)
{
  return sh_check_add_incdec_notes (
	emit_move_insn (dst, sh_remove_overlapping_post_inc (dst, src)));
}

/* Given an op rtx and an insn, try to find out whether the result of the
   specified op consists only of logical operations on T bit stores.  */
bool
sh_is_logical_t_store_expr (rtx op, rtx_insn* insn)
{
  if (!logical_operator (op, SImode))
    return false;

  rtx ops[2] = { XEXP (op, 0), XEXP (op, 1) };
  int op_is_t_count = 0;

  for (int i = 0; i < 2; ++i)
    {
      if (t_reg_operand (ops[i], VOIDmode)
	  || negt_reg_operand (ops[i], VOIDmode))
	op_is_t_count++;

      else
	{
	  set_of_reg op_set = sh_find_set_of_reg (ops[i], insn,
						  prev_nonnote_insn_bb);
	  if (op_set.set_src == NULL_RTX)
	    continue;

	  if (t_reg_operand (op_set.set_src, VOIDmode)
	      || negt_reg_operand (op_set.set_src, VOIDmode)
	      || sh_is_logical_t_store_expr (op_set.set_src, op_set.insn))
	      op_is_t_count++;
	}
    }
  
  return op_is_t_count == 2;
}

/* Given the operand that is extended in a sign/zero extend insn, and the
   insn, try to figure out whether the sign/zero extension can be replaced
   by a simple reg-reg copy.  If so, the replacement reg rtx is returned,
   NULL_RTX otherwise.  */
rtx
sh_try_omit_signzero_extend (rtx extended_op, rtx_insn* insn)
{
  if (REG_P (extended_op))
    extended_op = extended_op;
  else if (GET_CODE (extended_op) == SUBREG && REG_P (SUBREG_REG (extended_op)))
    extended_op = SUBREG_REG (extended_op);
  else
    return NULL_RTX;

  /* Reg moves must be of the same mode.  */
  if (GET_MODE (extended_op) != SImode)
    return NULL_RTX;

  set_of_reg s = sh_find_set_of_reg (extended_op, insn, prev_nonnote_insn_bb);
  if (s.set_src == NULL_RTX)
    return NULL_RTX;

  if (t_reg_operand (s.set_src, VOIDmode)
      || negt_reg_operand (s.set_src, VOIDmode))
    return extended_op;

  /* If the zero extended reg was formed by a logical operation, check the
     operands of the logical operation.  If both originated from T bit
     stores the zero extension can be eliminated.  */
  else if (sh_is_logical_t_store_expr (s.set_src, s.insn))
    return extended_op;

  return NULL_RTX;
}

/* Given the current insn, which is assumed to be a movrt_negc insn, try to
   figure out whether it should be converted into a movt-xor sequence in
   the movrt_negc splitter.
   Returns true if insns have been modified and the splitter has succeeded.  */
bool
sh_split_movrt_negc_to_movt_xor (rtx_insn* curr_insn, rtx operands[])
{
  /* In cases such as
	tst	r4,r4
	mov	#-1,r1
	negc	r1,r1
	tst	r4,r4
     we can replace the T bit clobbering negc with a movt-xor sequence and
     eliminate the redundant comparison.
     Because the xor insn depends on register allocation results, allow this
     only before reload.  */
  if (!can_create_pseudo_p ())
    return false;

  set_of_reg t_before_negc = sh_find_set_of_reg (get_t_reg_rtx (), curr_insn,
						 prev_nonnote_insn_bb);
  set_of_reg t_after_negc = sh_find_set_of_reg (get_t_reg_rtx (), curr_insn,
						next_nonnote_insn_bb);

  if (t_before_negc.set_rtx != NULL_RTX && t_after_negc.set_rtx != NULL_RTX
      && rtx_equal_p (t_before_negc.set_rtx, t_after_negc.set_rtx)
      && !reg_used_between_p (get_t_reg_rtx (), curr_insn, t_after_negc.insn)
      && !sh_insn_operands_modified_between_p (t_before_negc.insn,
					       t_before_negc.insn,
					       t_after_negc.insn)
      && !modified_between_p (get_t_reg_rtx (), curr_insn, t_after_negc.insn)
      && !sh_unspec_insn_p (t_after_negc.insn)
      && !volatile_insn_p (PATTERN (t_after_negc.insn))
      && !side_effects_p (PATTERN (t_after_negc.insn))
      && !may_trap_or_fault_p (PATTERN (t_after_negc.insn)))
    {
      emit_insn (gen_movrt_xor (operands[0], get_t_reg_rtx ()));
      set_insn_deleted (t_after_negc.insn);
      return true;
    }
  else
    return false;
}

/* Given a reg and the current insn, see if the value of the reg originated
   from a sign or zero extension and return the discovered information.  */
sh_extending_set_of_reg
sh_find_extending_set_of_reg (rtx reg, rtx_insn* curr_insn)
{
  if (reg == NULL)
    return sh_extending_set_of_reg (curr_insn);

  if (SUBREG_P (reg))
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return sh_extending_set_of_reg (curr_insn);

  /* FIXME: Also search the predecessor basic blocks.  It seems that checking
     only the adjacent predecessor blocks would cover most of the cases.
     Also try to look through the first extension that we hit.  There are some
     cases, where a zero_extend is followed an (implicit) sign_extend, and it
     fails to see the sign_extend.  */
  sh_extending_set_of_reg result =
	sh_find_set_of_reg (reg, curr_insn, prev_nonnote_insn_bb, true);

  if (result.set_src != NULL)
    {
      if (GET_CODE (result.set_src) == SIGN_EXTEND
	  || GET_CODE (result.set_src) == ZERO_EXTEND)
	{
	  if (dump_file)
	    fprintf (dump_file, "sh_find_extending_set_of_reg: reg %d is "
				"explicitly sign/zero extended in insn %d\n",
				REGNO (reg), INSN_UID (result.insn));
	  result.from_mode = GET_MODE (XEXP (result.set_src, 0));
	  result.ext_code = GET_CODE (result.set_src);
	}
      else if (MEM_P (result.set_src)
	       && (GET_MODE (result.set_src) == QImode
		   || GET_MODE (result.set_src) == HImode)
	       && !sh_unspec_insn_p (result.insn))
	{
	  /* On SH QIHImode memory loads always sign extend.  However, in
	     some cases where it seems that the higher bits are not
	     interesting, the loads will not be expanded as sign extending
	     insns, but as QIHImode loads into QIHImode regs.  We report that
	     the reg has been sign extended by the mem load.  When it is used
	     as such, we must convert the mem load into a sign extending insn,
	     see also sh_extending_set_of_reg::use_as_extended_reg.  */
	  if (dump_file)
	    fprintf (dump_file, "sh_find_extending_set_of_reg: reg %d is "
				"implicitly sign extended in insn %d\n",
				REGNO (reg), INSN_UID (result.insn));
	  result.from_mode = GET_MODE (result.set_src);
	  result.ext_code = SIGN_EXTEND;
	}
    }

  return result;
}

/* Given a reg that is known to be sign or zero extended at some insn,
   take the appropriate measures so that the extended value can be used as
   a reg at the specified insn and return the resulting reg rtx.  */
rtx
sh_extending_set_of_reg::use_as_extended_reg (rtx_insn* use_at_insn) const
{
  gcc_assert (insn != NULL && set_src != NULL && set_rtx != NULL);
  gcc_assert (ext_code == SIGN_EXTEND || ext_code == ZERO_EXTEND);
  gcc_assert (from_mode == QImode || from_mode == HImode);

  if (MEM_P (set_src) && ext_code == SIGN_EXTEND)
    {
      if (dump_file)
	fprintf (dump_file,
		 "use_as_extended_reg: converting non-extending mem load in "
		 "insn %d into sign-extending load\n", INSN_UID (insn));

	rtx r = gen_reg_rtx (SImode);
	rtx_insn* i0;
	if (from_mode == QImode)
	  i0 = emit_insn_after (gen_extendqisi2 (r, set_src), insn);
	else if (from_mode == HImode)
	  i0 = emit_insn_after (gen_extendhisi2 (r, set_src), insn);
	else
	  gcc_unreachable ();

	emit_insn_after (
		gen_move_insn (XEXP (set_rtx, 0),
			       gen_lowpart (GET_MODE (set_src), r)), i0);
	set_insn_deleted (insn);
	return r;
    }
  else
    {
      rtx extension_dst = XEXP (set_rtx, 0);
      if (GET_MODE (extension_dst) != SImode)
	extension_dst = simplify_gen_subreg (SImode, extension_dst,
					     GET_MODE (extension_dst), 0);
      if (modified_between_p (extension_dst, insn, use_at_insn))
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "use_as_extended_reg: dest reg %d of extending insn %d is "
		     "modified, inserting a reg-reg copy\n",
		     REGNO (extension_dst), INSN_UID (insn));

	  rtx r = gen_reg_rtx (SImode);
	  emit_insn_after (gen_move_insn (r, extension_dst), insn);
	  return r;
	}
      else
	{
	  sh_remove_reg_dead_or_unused_notes (insn, REGNO (extension_dst));
	  return extension_dst;
	}
    }
}

bool
sh_extending_set_of_reg::can_use_as_unextended_reg (void) const
{
  if ((ext_code == SIGN_EXTEND || ext_code == ZERO_EXTEND)
      && (from_mode == QImode || from_mode == HImode)
      && set_src != NULL)
    return arith_reg_operand (XEXP (set_src, 0), from_mode);
  else
    return false;
}

rtx
sh_extending_set_of_reg::use_as_unextended_reg (rtx_insn* use_at_insn) const
{
  gcc_assert (can_use_as_unextended_reg ());

  rtx r = XEXP (set_src, 0);
  rtx r0 = simplify_gen_subreg (SImode, r, from_mode, 0);

  if (modified_between_p (r, insn, use_at_insn))
    {
      rtx r1 = gen_reg_rtx (SImode);
      emit_insn_after (gen_move_insn (r1, r0), insn);
      return r1;
    }
  else
    {
      sh_remove_reg_dead_or_unused_notes (insn, SUBREG_P (r)
						? REGNO (SUBREG_REG (r))
						: REGNO (r));
      return r0;
    }
}

/* Given the current insn, which is assumed to be the *tst<mode>_t_subregs insn,
   perform the necessary checks on the operands and split it accordingly.  */
void
sh_split_tst_subregs (rtx_insn* curr_insn, machine_mode subreg_mode,
		      int subreg_offset, rtx operands[])
{
  gcc_assert (subreg_mode == QImode || subreg_mode == HImode);

  sh_extending_set_of_reg eop0 = sh_find_extending_set_of_reg (operands[0],
							       curr_insn);
  sh_extending_set_of_reg eop1 = sh_find_extending_set_of_reg (operands[1],
							       curr_insn);

  /* If one of the operands is known to be zero extended, that's already
     sufficient to mask out the unwanted high bits.  */
  if (eop0.ext_code == ZERO_EXTEND && eop0.from_mode == subreg_mode)
    {
      emit_insn (gen_tstsi_t (eop0.use_as_extended_reg (curr_insn),
			      operands[1]));
      return;
    }
  if (eop1.ext_code == ZERO_EXTEND && eop1.from_mode == subreg_mode)
    {
      emit_insn (gen_tstsi_t (operands[0],
			      eop1.use_as_extended_reg (curr_insn)));
      return;
    }

  /* None of the operands seem to be zero extended.
     If both are sign extended it's OK, too.  */
  if (eop0.ext_code == SIGN_EXTEND && eop1.ext_code == SIGN_EXTEND
      && eop0.from_mode == subreg_mode && eop1.from_mode == subreg_mode)
    {
      emit_insn (gen_tstsi_t (eop0.use_as_extended_reg (curr_insn),
			      eop1.use_as_extended_reg (curr_insn)));
      return;
    }

  /* Otherwise we have to insert a zero extension on one of the operands to
     mask out the unwanted high bits.
     Prefer the operand that has no known extension.  */
  if (eop0.ext_code != UNKNOWN && eop1.ext_code == UNKNOWN)
    std::swap (operands[0], operands[1]);

  rtx tmp0 = gen_reg_rtx (SImode);
  rtx tmp1 = simplify_gen_subreg (subreg_mode, operands[0],
				  GET_MODE (operands[0]), subreg_offset);
  emit_insn (subreg_mode == QImode
	     ? gen_zero_extendqisi2 (tmp0, tmp1)
	     : gen_zero_extendhisi2 (tmp0, tmp1));
  emit_insn (gen_tstsi_t (tmp0, operands[1]));
}

/* A helper class to increment/decrement a counter variable each time a
   function is entered/left.  */
class scope_counter
{
public:
  scope_counter (int& counter) : m_counter (counter) { ++m_counter; }

  ~scope_counter (void)
  {
    --m_counter;
    gcc_assert (m_counter >= 0);
  }

  int count (void) const { return m_counter; }

private:
  int& m_counter;
};

/* Given an rtx x, determine whether the expression can be used to create
   an insn that calulates x and stores the result in the T bit.
   This is used by the 'treg_set_expr' predicate to construct insns sequences
   where T bit results are fed into other insns, such as addc, subc, negc
   insns.

   FIXME: The patterns that expand 'treg_set_expr' operands tend to
   distinguish between 'positive' and 'negative' forms.  For now this has to
   be done in the preparation code.  We could also introduce
   'pos_treg_set_expr' and 'neg_treg_set_expr' predicates for that and write
   two different patterns for the 'postive' and 'negative' forms.  However,
   the total amount of lines of code seems to be about the same and the
   '{pos|neg}_treg_set_expr' predicates would be more expensive, because the
   recog function would need to look inside the expression by temporarily
   splitting it.  */
static int sh_recog_treg_set_expr_reent_count = 0;

bool
sh_recog_treg_set_expr (rtx op, machine_mode mode)
{
  scope_counter recursion (sh_recog_treg_set_expr_reent_count);

  /* Limit the recursion count to avoid nested expressions which we can't
     resolve to a single treg set insn.  */
  if (recursion.count () > 1)
    return false;

  /* Early accept known possible operands before doing recog.  */
  if (op == const0_rtx || op == const1_rtx || t_reg_operand (op, mode)
      || negt_reg_operand (op, mode))
    return true;

  /* Early reject impossible operands before doing recog.
     There are some (set ((t) (subreg ...))) patterns, but we must be careful
     not to allow any invalid reg-reg or mem-reg moves, or else other passes
     such as lower-subreg will bail out.  Some insns such as SH4A movua are
     done with UNSPEC, so must reject those, too, or else it would result
     in an invalid reg -> treg move.  */
  if (CONST_INT_P (op) || register_operand (op, mode)
      || memory_operand (op, mode) || sh_unspec_insn_p (op))
    return false;

  if (!can_create_pseudo_p ())
    return false;

  /* expand_debug_locations may call this to compute rtx costs at
     very early stage.  In that case, don't make new insns here to
     avoid codegen differences with -g. */
  if (currently_expanding_to_rtl)
    return false;

  /* We are going to invoke recog in a re-entrant way and thus
     have to capture its current state and restore it afterwards.  */
  recog_data_d prev_recog_data = recog_data;

  rtx_insn* i = make_insn_raw (gen_rtx_SET (get_t_reg_rtx (), op));
  SET_PREV_INSN (i) = NULL;
  SET_NEXT_INSN (i) = NULL;

  /* If the comparison op doesn't have a result mode, set it to SImode.  */
  machine_mode prev_op_mode = GET_MODE (op);
  if (COMPARISON_P (op) && prev_op_mode == VOIDmode)
    PUT_MODE (op, SImode);

  int result = recog (PATTERN (i), i, 0);

  /* It seems there is no insn like that.  Create a negated version and
     try again.  If we hit a negated form, we'll allow that and append a
     nott sequence when splitting out the insns.  Insns that do the split
     can then remove the trailing nott if they know how to deal with it.  */
  if (result < 0 && COMPARISON_P (op))
    {
      machine_mode cmp_mode = GET_MODE (XEXP (op, 0));
      if (cmp_mode == VOIDmode)
        cmp_mode = GET_MODE (XEXP (op, 1));

      rtx_code prev_code = GET_CODE (op);
      PUT_CODE (op, reverse_condition (GET_CODE (op)));
      result = recog (PATTERN (i), i, 0);
      PUT_CODE (op, prev_code);
    }

  PUT_MODE (op, prev_op_mode);
  recog_data = prev_recog_data;
  return result >= 0;
}

/* Returns true when recog of a 'treg_set_expr' is currently in progress.
   This can be used as a condition for insn/split patterns to allow certain
   T bit setting patters only to be matched as sub expressions of other
   patterns.  */
bool
sh_in_recog_treg_set_expr (void)
{
  return sh_recog_treg_set_expr_reent_count > 0;
}

/* Given an rtx x, which is assumed to be some expression that has been
   matched by the 'treg_set_expr' predicate before, split and emit the
   insns that are necessary to calculate the expression and store the result
   in the T bit.
   The splitting is done recursively similar to 'try_split' in emit-rt.c.
   Unfortunately we can't use 'try_split' here directly, as it tries to invoke
   'delete_insn' which then causes the DF parts to bail out, because we
   currently are inside another gen_split* function and would invoke
   'try_split' in a reentrant way.  */
static std::pair<rtx_insn*, rtx_insn*>
sh_try_split_insn_simple (rtx_insn* i, rtx_insn* curr_insn, int n = 0)
{
  if (dump_file)
    {
      fprintf (dump_file, "sh_try_split_insn_simple n = %d i = \n", n);
      print_rtl_single (dump_file, i);
      fprintf (dump_file, "\n");
    }

  rtx_insn* seq = split_insns (PATTERN (i), curr_insn);

  if (seq == NULL)
    return std::make_pair (i, i);

  /* Avoid infinite splitter loops if any insn of the result matches
     the original pattern.  */
  for (rtx_insn* s = seq; s != NULL; s = NEXT_INSN (s))
    if (INSN_P (s) && rtx_equal_p (PATTERN (s), PATTERN (i)))
      return std::make_pair (i, i);

  unshare_all_rtl_in_chain (seq);

  /* 'seq' is now a replacement for 'i'.  Assuming that 'i' is an insn in
     a linked list, replace the single insn with the new insns.  */
  rtx_insn* seqlast = seq;
  while (NEXT_INSN (seqlast) != NULL)
    seqlast = NEXT_INSN (seqlast);

  if (rtx_insn* iprev = PREV_INSN (i))
    SET_NEXT_INSN (iprev) = seq;
  if (rtx_insn* inext = NEXT_INSN (i))
    SET_PREV_INSN (inext) = seqlast;

  SET_PREV_INSN (seq) = PREV_INSN (i);
  SET_NEXT_INSN (seqlast) = NEXT_INSN (i);

  SET_PREV_INSN (i) = NULL;
  SET_NEXT_INSN (i) = NULL;

  /* Recursively split all insns.  */
  for (i = seq; ; i = NEXT_INSN (i))
    {
      std::pair<rtx_insn*, rtx_insn*> ii =
	  sh_try_split_insn_simple (i, curr_insn, n + 1);
      if (i == seq)
	seq = ii.first;
      if (i == seqlast)
	{
	  seqlast = ii.second;
	  break;
	}
      i = ii.first;
    }

  return std::make_pair (seq, seqlast);
}

sh_treg_insns
sh_split_treg_set_expr (rtx x, rtx_insn* curr_insn)
{
  if (t_reg_operand (x, VOIDmode))
    return sh_treg_insns ();

  scope_counter in_treg_set_expr (sh_recog_treg_set_expr_reent_count);

  rtx_insn* i = make_insn_raw (gen_rtx_SET (get_t_reg_rtx (), x));
  SET_PREV_INSN (i) = NULL;
  SET_NEXT_INSN (i) = NULL;

  if (dump_file)
    {
      fprintf (dump_file, "split_treg_set_expr insn:\n");
      print_rtl (dump_file, i);
      fprintf (dump_file, "\n");
    }

  /* If the insn is not found, we will try a negated form and append
     a nott.  */
  bool append_nott = false;

  /* We are going to invoke recog/split_insns in a re-entrant way and thus
     have to capture its current state and restore it afterwards.  */
  recog_data_d prev_recog_data = recog_data;

  if (negt_reg_operand (x, GET_MODE (x)))
    {
      /* This is a normal movt followed by a nott.  It will be converted
	 into a movrt after initial expansion.  */
      XEXP (PATTERN (i), 1) = get_t_reg_rtx ();
      append_nott = true;
    }
  else
    {
      /* If the comparison op doesn't have a mode set, set it to SImode.  */
      if (COMPARISON_P (x) && GET_MODE (x) == VOIDmode)
	PUT_MODE (x, SImode);

      int insn_code = recog (PATTERN (i), i, 0);

      if (insn_code < 0 && COMPARISON_P (x))
	{
	  machine_mode cmp_mode = GET_MODE (XEXP (x, 0));
	  if (cmp_mode == VOIDmode)
	    cmp_mode = GET_MODE (XEXP (x, 1));

	  PUT_CODE (x, reverse_condition (GET_CODE (x)));
	  insn_code = recog (PATTERN (i), i, 0);
	  append_nott = true;
	}

      gcc_assert (insn_code >= 0);
    }

  /* Try to recursively split the insn.  Some insns might refuse to split
     any further while we are in the treg_set_expr splitting phase.  They
     will be emitted as part of the outer insn and then split again.  */
  std::pair<rtx_insn*, rtx_insn*> insnlist =
	sh_try_split_insn_simple (i, curr_insn);

  /* Restore recog state.  */
  recog_data = prev_recog_data;

  rtx_insn* nott_insn = sh_is_nott_insn (insnlist.second)
			? insnlist.second
			: NULL;
  if (dump_file)
    {
      fprintf (dump_file, "split_treg_set_expr insnlist:\n");
      print_rtl (dump_file, insnlist.first);
      fprintf (dump_file, "\n");

      if (nott_insn != NULL)
	fprintf (dump_file, "trailing nott insn %d\n", INSN_UID (nott_insn));
    }

  emit_insn (insnlist.first);

  if (nott_insn != NULL && append_nott)
    {
      if (dump_file)
	fprintf (dump_file, "removing trailing nott\n");
      remove_insn (nott_insn);
      nott_insn = NULL;
      append_nott = false;
    }

  if (append_nott)
    nott_insn = emit_insn (gen_nott (get_t_reg_rtx ()));

  rtx_insn* first_insn = get_insns ();

  if (dump_file)
    {
      fprintf (dump_file, "resulting insns:\n");
      print_rtl (dump_file, first_insn);
      fprintf (dump_file, "\n");
    }

  return sh_treg_insns (first_insn, nott_insn);
}

/*------------------------------------------------------------------------------
  Mode switching support code.
*/

static void
sh_emit_mode_set (int entity ATTRIBUTE_UNUSED, int mode,
		  int prev_mode, HARD_REG_SET regs_live ATTRIBUTE_UNUSED)
{
  if ((TARGET_SH4A_FP || TARGET_SH4_300)
      && prev_mode != FP_MODE_NONE && prev_mode != mode)
    {
      emit_insn (gen_toggle_pr ());
      if (TARGET_FMOVD)
	emit_insn (gen_toggle_sz ());
    }
  else if (mode != FP_MODE_NONE)
    {
      rtx tmp = gen_reg_rtx (SImode);
      emit_insn (gen_sts_fpscr (tmp));
      rtx i = NULL;

      const unsigned HOST_WIDE_INT fpbits =
	  TARGET_FMOVD ? (FPSCR_PR | FPSCR_SZ) : FPSCR_PR;

      if (prev_mode != FP_MODE_NONE && prev_mode != mode)
	i = gen_xorsi3 (tmp, tmp, force_reg (SImode, GEN_INT (fpbits)));
      else if (mode == FP_MODE_SINGLE)
	i = gen_andsi3 (tmp, tmp, force_reg (SImode, GEN_INT (~fpbits)));
      else if (mode == FP_MODE_DOUBLE)
	i = gen_iorsi3 (tmp, tmp, force_reg (SImode, GEN_INT (fpbits)));
      else
	gcc_unreachable ();

      emit_insn (i);
      emit_insn (gen_lds_fpscr (tmp));
    }
}

static int
sh_mode_needed (int entity ATTRIBUTE_UNUSED, rtx_insn *insn)
{
  return recog_memoized (insn) >= 0  ? get_attr_fp_mode (insn) : FP_MODE_NONE;
}

static int
sh_mode_after (int entity ATTRIBUTE_UNUSED, int mode, rtx_insn *insn)
{
  if (TARGET_HITACHI && recog_memoized (insn) >= 0 &&
      get_attr_fp_set (insn) != FP_SET_NONE)
    return (int) get_attr_fp_set (insn);
  else
    return mode;
}

static int
sh_mode_entry (int entity ATTRIBUTE_UNUSED)
{
  return NORMAL_MODE (entity);
}

static int
sh_mode_exit (int entity ATTRIBUTE_UNUSED)
{
  return sh_cfun_attr_renesas_p () ? FP_MODE_NONE : NORMAL_MODE (entity);
}

static int
sh_mode_priority (int entity ATTRIBUTE_UNUSED, int n)
{
  return ((TARGET_FPU_SINGLE != 0) ^ (n) ? FP_MODE_SINGLE : FP_MODE_DOUBLE);
}

/*------------------------------------------------------------------------------
  Misc
*/

/* Return true if we use LRA instead of reload pass.  */
bool
sh_lra_p (void)
{
  return sh_lra_flag;
}

/* Implement TARGET_USE_BY_PIECES_INFRASTRUCTURE_P.  */

static bool
sh_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
				   unsigned int align,
				   enum by_pieces_operation op,
				   bool speed_p)
{
  switch (op)
    {
      case MOVE_BY_PIECES:
	return move_by_pieces_ninsns (size, align, MOVE_MAX_PIECES + 1)
	  < (!speed_p ? 2 : (align >= 32) ? 16 : 2);
      case STORE_BY_PIECES:
      case SET_BY_PIECES:
	return move_by_pieces_ninsns (size, align, STORE_MAX_PIECES + 1)
	  < (!speed_p ? 2 : (align >= 32) ? 16 : 2);
      default:
	return default_use_by_pieces_infrastructure_p (size, align,
						       op, speed_p);
    }
}

bool
sh_cannot_force_const_mem_p (machine_mode mode ATTRIBUTE_UNUSED,
			     rtx x ATTRIBUTE_UNUSED)
{
  return TARGET_FDPIC;
}

/* Emit insns to load the function address from FUNCDESC (an FDPIC
   function descriptor) into r1 and the GOT address into r12,
   returning an rtx for r1.  */

rtx
sh_load_function_descriptor (rtx funcdesc)
{
  rtx r1 = gen_rtx_REG (Pmode, R1_REG);
  rtx pic_reg = gen_rtx_REG (Pmode, PIC_REG);
  rtx fnaddr = gen_rtx_MEM (Pmode, funcdesc);
  rtx gotaddr = gen_rtx_MEM (Pmode, plus_constant (Pmode, funcdesc, 4));

  emit_move_insn (r1, fnaddr);
  /* The ABI requires the entry point address to be loaded first, so
     prevent the load from being moved after that of the GOT
     address.  */
  emit_insn (gen_blockage ());
  emit_move_insn (pic_reg, gotaddr);
  return r1;
}

/* Return an rtx holding the initial value of the FDPIC register (the
   FDPIC pointer passed in from the caller).  */

rtx
sh_get_fdpic_reg_initial_val (void)
{
  return get_hard_reg_initial_val (Pmode, PIC_REG);
}

#include "gt-sh.h"
