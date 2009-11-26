/* Default initializers for a generic GCC target.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.

   In other words, you are welcome to use, share and improve this program.
   You are forbidden to forbid anyone else to use, share and improve
   what you give them.   Help stamp out software-hoarding!  */

/* See target.h for a description of what this file contains and how to
   use it.

   We want to have non-NULL default definitions of all hook functions,
   even if they do nothing.  */

/* Note that if one of these macros must be defined in an OS .h file
   rather than the .c file, then we need to wrap the default
   definition in a #ifndef, since files include tm.h before this one.  */

/* Assembler output.  */
#ifndef TARGET_ASM_OPEN_PAREN
#define TARGET_ASM_OPEN_PAREN "("
#endif
#ifndef TARGET_ASM_CLOSE_PAREN
#define TARGET_ASM_CLOSE_PAREN ")"
#endif

#define TARGET_ASM_BYTE_OP "\t.byte\t"

#define TARGET_ASM_ALIGNED_HI_OP "\t.short\t"
#define TARGET_ASM_ALIGNED_SI_OP "\t.long\t"
#define TARGET_ASM_ALIGNED_DI_OP NULL
#define TARGET_ASM_ALIGNED_TI_OP NULL

/* GAS and SYSV4 assemblers accept these.  */
#if defined (OBJECT_FORMAT_ELF)
#define TARGET_ASM_UNALIGNED_HI_OP "\t.2byte\t"
#define TARGET_ASM_UNALIGNED_SI_OP "\t.4byte\t"
#define TARGET_ASM_UNALIGNED_DI_OP "\t.8byte\t"
#define TARGET_ASM_UNALIGNED_TI_OP NULL
#else
#define TARGET_ASM_UNALIGNED_HI_OP NULL
#define TARGET_ASM_UNALIGNED_SI_OP NULL
#define TARGET_ASM_UNALIGNED_DI_OP NULL
#define TARGET_ASM_UNALIGNED_TI_OP NULL
#endif /* OBJECT_FORMAT_ELF */

#define TARGET_ASM_INTEGER default_assemble_integer

#ifndef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL default_globalize_label
#endif

#ifndef TARGET_ASM_GLOBALIZE_DECL_NAME
#define TARGET_ASM_GLOBALIZE_DECL_NAME default_globalize_decl_name
#endif

#ifndef TARGET_ASM_EMIT_UNWIND_LABEL
#define TARGET_ASM_EMIT_UNWIND_LABEL default_emit_unwind_label
#endif

#ifndef TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL
#define TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL default_emit_except_table_label
#endif

#ifndef TARGET_UNWIND_EMIT
#define TARGET_UNWIND_EMIT default_unwind_emit
#endif

#ifndef TARGET_ASM_INTERNAL_LABEL
#define TARGET_ASM_INTERNAL_LABEL default_internal_label
#endif

#ifndef TARGET_ASM_TTYPE
#define TARGET_ASM_TTYPE hook_bool_rtx_false
#endif

#ifndef TARGET_ASM_ASSEMBLE_VISIBILITY
#define TARGET_ASM_ASSEMBLE_VISIBILITY default_assemble_visibility
#endif

#define TARGET_ASM_FUNCTION_PROLOGUE default_function_pro_epilogue
#define TARGET_ASM_FUNCTION_EPILOGUE default_function_pro_epilogue
#define TARGET_ASM_FUNCTION_END_PROLOGUE no_asm_to_stream
#define TARGET_ASM_FUNCTION_BEGIN_EPILOGUE no_asm_to_stream

#ifndef TARGET_ASM_RELOC_RW_MASK
#define TARGET_ASM_RELOC_RW_MASK default_reloc_rw_mask
#endif

#ifndef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION default_select_section
#endif

#ifndef TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION default_unique_section
#endif

#ifndef TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION default_function_rodata_section
#endif

#ifndef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION default_select_rtx_section
#endif

#if !defined(TARGET_ASM_CONSTRUCTOR) && !defined(USE_COLLECT2)
# ifdef CTORS_SECTION_ASM_OP
#  define TARGET_ASM_CONSTRUCTOR default_ctor_section_asm_out_constructor
# else
#  ifdef TARGET_ASM_NAMED_SECTION
#   define TARGET_ASM_CONSTRUCTOR default_named_section_asm_out_constructor
#  else
#   define TARGET_ASM_CONSTRUCTOR default_stabs_asm_out_constructor
#  endif
# endif
#endif

#if !defined(TARGET_ASM_DESTRUCTOR) && !defined(USE_COLLECT2)
# ifdef DTORS_SECTION_ASM_OP
#  define TARGET_ASM_DESTRUCTOR default_dtor_section_asm_out_destructor
# else
#  ifdef TARGET_ASM_NAMED_SECTION
#   define TARGET_ASM_DESTRUCTOR default_named_section_asm_out_destructor
#  else
#   define TARGET_ASM_DESTRUCTOR default_stabs_asm_out_destructor
#  endif
# endif
#endif

#define TARGET_ASM_OUTPUT_MI_THUNK NULL
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_false

#if !defined(TARGET_HAVE_CTORS_DTORS)
# if defined(TARGET_ASM_CONSTRUCTOR) && defined(TARGET_ASM_DESTRUCTOR)
# define TARGET_HAVE_CTORS_DTORS true
# else
# define TARGET_HAVE_CTORS_DTORS false
# define TARGET_ASM_CONSTRUCTOR NULL
# define TARGET_ASM_DESTRUCTOR NULL
# endif
#endif

#ifndef TARGET_HAVE_SWITCHABLE_BSS_SECTIONS
#define TARGET_HAVE_SWITCHABLE_BSS_SECTIONS false
#endif

#ifndef TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS hook_void_void
#endif

#ifdef TARGET_ASM_NAMED_SECTION
#define TARGET_HAVE_NAMED_SECTIONS true
#else
#define TARGET_ASM_NAMED_SECTION default_no_named_section
#define TARGET_HAVE_NAMED_SECTIONS false
#endif

#ifndef TARGET_INVALID_WITHIN_DOLOOP
#define TARGET_INVALID_WITHIN_DOLOOP default_invalid_within_doloop
#endif

#ifndef TARGET_VALID_DLLIMPORT_ATTRIBUTE_P
#define TARGET_VALID_DLLIMPORT_ATTRIBUTE_P hook_bool_const_tree_true
#endif

#ifndef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS false
#endif

#ifndef TARGET_HAVE_SRODATA_SECTION
#define TARGET_HAVE_SRODATA_SECTION false
#endif

#ifndef TARGET_TERMINATE_DW2_EH_FRAME_INFO
#ifdef EH_FRAME_SECTION_NAME
#define TARGET_TERMINATE_DW2_EH_FRAME_INFO false
#else
#define TARGET_TERMINATE_DW2_EH_FRAME_INFO true
#endif
#endif

#define TARGET_DWARF_REGISTER_SPAN hook_rtx_rtx_null
#define TARGET_INIT_DWARF_REG_SIZES_EXTRA hook_void_tree

#ifndef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START default_file_start
#endif

#ifndef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END hook_void_void
#endif

#ifndef TARGET_EXTRA_LIVE_ON_ENTRY
#define TARGET_EXTRA_LIVE_ON_ENTRY hook_void_bitmap
#endif

#ifndef TARGET_ASM_FILE_START_APP_OFF
#define TARGET_ASM_FILE_START_APP_OFF false
#endif

#ifndef TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE false
#endif

#ifndef TARGET_ASM_EXTERNAL_LIBCALL
#define TARGET_ASM_EXTERNAL_LIBCALL default_external_libcall
#endif

#ifndef TARGET_ASM_MARK_DECL_PRESERVED
#define TARGET_ASM_MARK_DECL_PRESERVED hook_void_constcharptr
#endif

#ifndef TARGET_ASM_OUTPUT_ANCHOR
#ifdef ASM_OUTPUT_DEF
#define TARGET_ASM_OUTPUT_ANCHOR default_asm_output_anchor
#else
#define TARGET_ASM_OUTPUT_ANCHOR NULL
#endif
#endif

#ifndef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL NULL
#endif

#ifndef TARGET_ASM_FINAL_POSTSCAN_INSN
#define TARGET_ASM_FINAL_POSTSCAN_INSN NULL
#endif

#ifndef TARGET_ASM_RECORD_GCC_SWITCHES
#define TARGET_ASM_RECORD_GCC_SWITCHES NULL
#endif
#ifndef TARGET_ASM_RECORD_GCC_SWITCHES_SECTION
#define TARGET_ASM_RECORD_GCC_SWITCHES_SECTION ".GCC.command.line"
#endif

#define TARGET_ASM_TRAMPOLINE_TEMPLATE NULL

#define TARGET_ASM_ALIGNED_INT_OP				\
		       {TARGET_ASM_ALIGNED_HI_OP,		\
			TARGET_ASM_ALIGNED_SI_OP,		\
			TARGET_ASM_ALIGNED_DI_OP,		\
			TARGET_ASM_ALIGNED_TI_OP}

#define TARGET_ASM_UNALIGNED_INT_OP				\
		       {TARGET_ASM_UNALIGNED_HI_OP,		\
			TARGET_ASM_UNALIGNED_SI_OP,		\
			TARGET_ASM_UNALIGNED_DI_OP,		\
			TARGET_ASM_UNALIGNED_TI_OP}

#define TARGET_ASM_OUT {TARGET_ASM_OPEN_PAREN,			\
			TARGET_ASM_CLOSE_PAREN,			\
			TARGET_ASM_BYTE_OP,			\
			TARGET_ASM_ALIGNED_INT_OP,		\
			TARGET_ASM_UNALIGNED_INT_OP,		\
			TARGET_ASM_INTEGER,			\
			TARGET_ASM_GLOBALIZE_LABEL,		\
			TARGET_ASM_GLOBALIZE_DECL_NAME,		\
                        TARGET_ASM_EMIT_UNWIND_LABEL,           \
			TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL,	\
			TARGET_UNWIND_EMIT,			\
			TARGET_ASM_INTERNAL_LABEL,		\
			TARGET_ASM_TTYPE,			\
			TARGET_ASM_ASSEMBLE_VISIBILITY,		\
			TARGET_ASM_FUNCTION_PROLOGUE,		\
			TARGET_ASM_FUNCTION_END_PROLOGUE,	\
			TARGET_ASM_FUNCTION_BEGIN_EPILOGUE,	\
			TARGET_ASM_FUNCTION_EPILOGUE,		\
			TARGET_ASM_INIT_SECTIONS,		\
			TARGET_ASM_NAMED_SECTION,		\
			TARGET_ASM_RELOC_RW_MASK,		\
			TARGET_ASM_SELECT_SECTION,		\
			TARGET_ASM_SELECT_RTX_SECTION,		\
			TARGET_ASM_UNIQUE_SECTION,		\
			TARGET_ASM_FUNCTION_RODATA_SECTION,	\
			TARGET_ASM_CONSTRUCTOR,			\
			TARGET_ASM_DESTRUCTOR,                  \
                        TARGET_ASM_OUTPUT_MI_THUNK,             \
                        TARGET_ASM_CAN_OUTPUT_MI_THUNK,         \
                        TARGET_ASM_FILE_START,                  \
                        TARGET_ASM_FILE_END,			\
			TARGET_ASM_EXTERNAL_LIBCALL,            \
                        TARGET_ASM_MARK_DECL_PRESERVED,		\
			TARGET_ASM_RECORD_GCC_SWITCHES,		\
			TARGET_ASM_RECORD_GCC_SWITCHES_SECTION,	\
			TARGET_ASM_OUTPUT_ANCHOR,		\
			TARGET_ASM_OUTPUT_DWARF_DTPREL,		\
			TARGET_ASM_FINAL_POSTSCAN_INSN,		\
			TARGET_ASM_TRAMPOLINE_TEMPLATE }

/* Scheduler hooks.  All of these default to null pointers, which
   haifa-sched.c looks for and handles.  */
#define TARGET_SCHED_ADJUST_COST 0
#define TARGET_SCHED_ADJUST_PRIORITY 0
#define TARGET_SCHED_ISSUE_RATE 0
#define TARGET_SCHED_VARIABLE_ISSUE 0
#define TARGET_SCHED_INIT 0
#define TARGET_SCHED_FINISH 0
#define TARGET_SCHED_INIT_GLOBAL 0
#define TARGET_SCHED_FINISH_GLOBAL 0
#define TARGET_SCHED_REORDER 0
#define TARGET_SCHED_REORDER2 0
#define TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK 0
#define TARGET_SCHED_INIT_DFA_PRE_CYCLE_INSN 0
#define TARGET_SCHED_DFA_PRE_CYCLE_INSN 0
#define TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN 0
#define TARGET_SCHED_DFA_POST_CYCLE_INSN 0
#define TARGET_SCHED_DFA_PRE_ADVANCE_CYCLE 0
#define TARGET_SCHED_DFA_POST_ADVANCE_CYCLE 0
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD 0
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD 0
#define TARGET_SCHED_DFA_NEW_CYCLE 0
#define TARGET_SCHED_IS_COSTLY_DEPENDENCE 0
#define TARGET_SCHED_ADJUST_COST_2 0
#define TARGET_SCHED_H_I_D_EXTENDED 0
#define TARGET_SCHED_ALLOC_SCHED_CONTEXT 0
#define TARGET_SCHED_INIT_SCHED_CONTEXT 0
#define TARGET_SCHED_SET_SCHED_CONTEXT 0
#define TARGET_SCHED_CLEAR_SCHED_CONTEXT 0
#define TARGET_SCHED_FREE_SCHED_CONTEXT 0
#define TARGET_SCHED_SPECULATE_INSN 0
#define TARGET_SCHED_NEEDS_BLOCK_P 0
#define TARGET_SCHED_GEN_SPEC_CHECK 0
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD_SPEC 0
#define TARGET_SCHED_SET_SCHED_FLAGS 0
#define TARGET_SCHED_GET_INSN_SPEC_DS 0
#define TARGET_SCHED_GET_INSN_CHECKED_DS 0
#define TARGET_SCHED_SKIP_RTX_P 0
#define TARGET_SCHED_SMS_RES_MII 0

#define TARGET_SCHED						\
  {TARGET_SCHED_ADJUST_COST,					\
   TARGET_SCHED_ADJUST_PRIORITY,				\
   TARGET_SCHED_ISSUE_RATE,					\
   TARGET_SCHED_VARIABLE_ISSUE,					\
   TARGET_SCHED_INIT,						\
   TARGET_SCHED_FINISH,						\
   TARGET_SCHED_INIT_GLOBAL,					\
   TARGET_SCHED_FINISH_GLOBAL,					\
   TARGET_SCHED_REORDER,					\
   TARGET_SCHED_REORDER2,					\
   TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK,			\
   TARGET_SCHED_INIT_DFA_PRE_CYCLE_INSN,			\
   TARGET_SCHED_DFA_PRE_CYCLE_INSN,				\
   TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN,			\
   TARGET_SCHED_DFA_POST_CYCLE_INSN,			        \
   TARGET_SCHED_DFA_PRE_ADVANCE_CYCLE,                          \
   TARGET_SCHED_DFA_POST_ADVANCE_CYCLE,                         \
   TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD,		\
   TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD,	\
   TARGET_SCHED_DFA_NEW_CYCLE,					\
   TARGET_SCHED_IS_COSTLY_DEPENDENCE,                           \
   TARGET_SCHED_ADJUST_COST_2,                                  \
   TARGET_SCHED_H_I_D_EXTENDED,					\
   TARGET_SCHED_ALLOC_SCHED_CONTEXT,                            \
   TARGET_SCHED_INIT_SCHED_CONTEXT,                             \
   TARGET_SCHED_SET_SCHED_CONTEXT,                              \
   TARGET_SCHED_CLEAR_SCHED_CONTEXT,                            \
   TARGET_SCHED_FREE_SCHED_CONTEXT,                             \
   TARGET_SCHED_SPECULATE_INSN,                                 \
   TARGET_SCHED_NEEDS_BLOCK_P,                                  \
   TARGET_SCHED_GEN_SPEC_CHECK,				        \
   TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD_SPEC, \
   TARGET_SCHED_SET_SCHED_FLAGS,                                \
   TARGET_SCHED_GET_INSN_SPEC_DS,                               \
   TARGET_SCHED_GET_INSN_CHECKED_DS,                            \
   TARGET_SCHED_SKIP_RTX_P,					\
   TARGET_SCHED_SMS_RES_MII}

#define TARGET_VECTORIZE_BUILTIN_MASK_FOR_LOAD 0
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  default_builtin_vectorized_function
#define TARGET_VECTORIZE_BUILTIN_CONVERSION \
  default_builtin_vectorized_conversion
#define TARGET_VECTORIZE_BUILTIN_MUL_WIDEN_EVEN 0
#define TARGET_VECTORIZE_BUILTIN_MUL_WIDEN_ODD 0
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST 0
#define TARGET_VECTOR_ALIGNMENT_REACHABLE \
  default_builtin_vector_alignment_reachable
#define TARGET_VECTORIZE_BUILTIN_VEC_PERM 0
#define TARGET_VECTORIZE_BUILTIN_VEC_PERM_OK \
  hook_bool_tree_tree_true
#define TARGET_SUPPORT_VECTOR_MISALIGNMENT \
  default_builtin_support_vector_misalignment


#define TARGET_VECTORIZE                                                \
  {									\
    TARGET_VECTORIZE_BUILTIN_MASK_FOR_LOAD,				\
    TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION,			\
    TARGET_VECTORIZE_BUILTIN_CONVERSION,				\
    TARGET_VECTORIZE_BUILTIN_MUL_WIDEN_EVEN,                            \
    TARGET_VECTORIZE_BUILTIN_MUL_WIDEN_ODD,				\
    TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST,			\
    TARGET_VECTOR_ALIGNMENT_REACHABLE,                                  \
    TARGET_VECTORIZE_BUILTIN_VEC_PERM,					\
    TARGET_VECTORIZE_BUILTIN_VEC_PERM_OK,				\
    TARGET_SUPPORT_VECTOR_MISALIGNMENT					\
  }

#define TARGET_DEFAULT_TARGET_FLAGS 0

#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE hook_void_void

#define TARGET_HANDLE_OPTION hook_bool_size_t_constcharptr_int_true
#define TARGET_HELP NULL

/* In except.c */
#define TARGET_EH_RETURN_FILTER_MODE  default_eh_return_filter_mode

/* In libgcc2.c */
#define TARGET_LIBGCC_CMP_RETURN_MODE  default_libgcc_cmp_return_mode
#define TARGET_LIBGCC_SHIFT_COUNT_MODE default_libgcc_shift_count_mode

/* In unwind-generic.h.  */
#define TARGET_UNWIND_WORD_MODE default_unwind_word_mode

/* In tree.c.  */
#define TARGET_MERGE_DECL_ATTRIBUTES merge_decl_attributes
#define TARGET_MERGE_TYPE_ATTRIBUTES merge_type_attributes
#define TARGET_ATTRIBUTE_TABLE NULL

/* In cse.c.  */
#define TARGET_ADDRESS_COST default_address_cost
#define TARGET_CONST_ANCHOR 0

/* In builtins.c.  */
#define TARGET_INIT_BUILTINS hook_void_void
#define TARGET_EXPAND_BUILTIN default_expand_builtin
#define TARGET_RESOLVE_OVERLOADED_BUILTIN NULL
#define TARGET_FOLD_BUILTIN hook_tree_tree_tree_bool_null
#define TARGET_BUILTIN_DECL NULL

/* In tree-ssa-math-opts.c  */
#define TARGET_BUILTIN_RECIPROCAL default_builtin_reciprocal

/* In varasm.c.  */
#ifndef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS default_section_type_flags
#endif

#ifndef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING default_strip_name_encoding
#endif

#ifndef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P default_binds_local_p
#endif

#ifndef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK default_shift_truncation_mask
#endif

#ifndef TARGET_MIN_DIVISIONS_FOR_RECIP_MUL
#define TARGET_MIN_DIVISIONS_FOR_RECIP_MUL default_min_divisions_for_recip_mul
#endif

#ifndef TARGET_MODE_REP_EXTENDED
#define TARGET_MODE_REP_EXTENDED default_mode_rep_extended
#endif

#ifndef TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE default_valid_pointer_mode
#endif

#ifndef TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE default_addr_space_pointer_mode
#endif

#ifndef TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE default_addr_space_address_mode
#endif

#ifndef TARGET_ADDR_SPACE_VALID_POINTER_MODE
#define TARGET_ADDR_SPACE_VALID_POINTER_MODE \
	default_addr_space_valid_pointer_mode
#endif

#ifndef TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P \
  default_addr_space_legitimate_address_p
#endif

#ifndef TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS
#define TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS \
  default_addr_space_legitimize_address
#endif

#ifndef TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P default_addr_space_subset_p
#endif

#ifndef TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT default_addr_space_convert
#endif

#define TARGET_ADDR_SPACE_HOOKS			\
  {						\
    TARGET_ADDR_SPACE_POINTER_MODE,		\
    TARGET_ADDR_SPACE_ADDRESS_MODE,		\
    TARGET_ADDR_SPACE_VALID_POINTER_MODE,	\
    TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P,	\
    TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS,	\
    TARGET_ADDR_SPACE_SUBSET_P,			\
    TARGET_ADDR_SPACE_CONVERT,			\
  }

#ifndef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P default_scalar_mode_supported_p
#endif

#ifndef TARGET_DECIMAL_FLOAT_SUPPORTED_P
#define TARGET_DECIMAL_FLOAT_SUPPORTED_P default_decimal_float_supported_p
#endif

#ifndef TARGET_FIXED_POINT_SUPPORTED_P
#define TARGET_FIXED_POINT_SUPPORTED_P default_fixed_point_supported_p
#endif

#ifndef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P hook_bool_mode_false
#endif

/* In hooks.c.  */
#define TARGET_CANNOT_MODIFY_JUMPS_P hook_bool_void_false
#define TARGET_BRANCH_TARGET_REGISTER_CLASS \
  default_branch_target_register_class
#define TARGET_BRANCH_TARGET_REGISTER_CALLEE_SAVED hook_bool_bool_false
#define TARGET_HAVE_CONDITIONAL_EXECUTION default_have_conditional_execution
#define TARGET_CANNOT_FORCE_CONST_MEM hook_bool_rtx_false
#define TARGET_CANNOT_COPY_INSN_P NULL
#define TARGET_COMMUTATIVE_P hook_bool_const_rtx_commutative_p
#define TARGET_LEGITIMIZE_ADDRESS default_legitimize_address
#define TARGET_DELEGITIMIZE_ADDRESS delegitimize_mem_from_attrs
#define TARGET_LEGITIMATE_ADDRESS_P default_legitimate_address_p
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P hook_bool_mode_const_rtx_false
#define TARGET_MIN_ANCHOR_OFFSET 0
#define TARGET_MAX_ANCHOR_OFFSET 0
#define TARGET_USE_ANCHORS_FOR_SYMBOL_P default_use_anchors_for_symbol_p
#define TARGET_FUNCTION_OK_FOR_SIBCALL hook_bool_tree_tree_false
#define TARGET_COMP_TYPE_ATTRIBUTES hook_int_const_tree_const_tree_1
#ifndef TARGET_SET_DEFAULT_TYPE_ATTRIBUTES
#define TARGET_SET_DEFAULT_TYPE_ATTRIBUTES hook_void_tree
#endif
#define TARGET_INSERT_ATTRIBUTES hook_void_tree_treeptr
#define TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P hook_bool_const_tree_false
#define TARGET_MS_BITFIELD_LAYOUT_P hook_bool_const_tree_false
#define TARGET_ALIGN_ANON_BITFIELD hook_bool_void_false
#define TARGET_NARROW_VOLATILE_BITFIELD hook_bool_void_false
#define TARGET_RTX_COSTS hook_bool_rtx_int_int_intp_bool_false
#define TARGET_MANGLE_TYPE hook_constcharptr_const_tree_null
#define TARGET_ALLOCATE_INITIAL_VALUE NULL

#define TARGET_UNSPEC_MAY_TRAP_P default_unspec_may_trap_p

#ifndef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION hook_void_tree
#endif

#ifndef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS hook_void_void
#endif

#ifndef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P hook_bool_const_tree_false
#endif

#ifndef TARGET_MANGLE_DECL_ASSEMBLER_NAME
#define TARGET_MANGLE_DECL_ASSEMBLER_NAME default_mangle_decl_assembler_name
#endif

#ifndef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO default_encode_section_info
#endif

#ifndef TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN
#define TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN hook_invalid_arg_for_unprototyped_fn
#endif

#define TARGET_INVALID_CONVERSION hook_constcharptr_const_tree_const_tree_null
#define TARGET_INVALID_UNARY_OP hook_constcharptr_int_const_tree_null
#define TARGET_INVALID_BINARY_OP hook_constcharptr_int_const_tree_const_tree_null
#define TARGET_INVALID_PARAMETER_TYPE hook_constcharptr_const_tree_null
#define TARGET_INVALID_RETURN_TYPE hook_constcharptr_const_tree_null
#define TARGET_PROMOTED_TYPE hook_tree_const_tree_null
#define TARGET_CONVERT_TO_TYPE hook_tree_tree_tree_null

#define TARGET_FIXED_CONDITION_CODE_REGS hook_bool_uintp_uintp_false

#define TARGET_CC_MODES_COMPATIBLE default_cc_modes_compatible

#define TARGET_MACHINE_DEPENDENT_REORG 0

#define TARGET_BUILD_BUILTIN_VA_LIST std_build_builtin_va_list
#define TARGET_FN_ABI_VA_LIST std_fn_abi_va_list
#define TARGET_CANONICAL_VA_LIST_TYPE std_canonical_va_list_type
#define TARGET_EXPAND_BUILTIN_VA_START 0

#define TARGET_GET_PCH_VALIDITY default_get_pch_validity
#define TARGET_PCH_VALID_P default_pch_valid_p
#define TARGET_CHECK_PCH_TARGET_FLAGS NULL

#define TARGET_DEFAULT_SHORT_ENUMS hook_bool_void_false

#define TARGET_BUILTIN_SETJMP_FRAME_VALUE default_builtin_setjmp_frame_value

#define TARGET_MD_ASM_CLOBBERS hook_tree_tree_tree_tree_3rd_identity

#define TARGET_DWARF_CALLING_CONVENTION hook_int_const_tree_0

#define TARGET_DWARF_HANDLE_FRAME_UNSPEC 0

#define TARGET_STDARG_OPTIMIZE_HOOK 0

#define TARGET_STACK_PROTECT_GUARD  default_stack_protect_guard
#define TARGET_STACK_PROTECT_FAIL   default_external_stack_protect_fail

#define TARGET_ARM_EABI_UNWINDER false

#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_false

#define TARGET_STRUCT_VALUE_RTX hook_rtx_tree_int_null
#define TARGET_RETURN_IN_MEMORY default_return_in_memory
#define TARGET_RETURN_IN_MSB hook_bool_const_tree_false

#define TARGET_EXPAND_BUILTIN_SAVEREGS default_expand_builtin_saveregs
#define TARGET_SETUP_INCOMING_VARARGS default_setup_incoming_varargs
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_false
#define TARGET_PRETEND_OUTGOING_VARARGS_NAMED \
  default_pretend_outgoing_varargs_named
#define TARGET_SPLIT_COMPLEX_ARG NULL

#define TARGET_GIMPLIFY_VA_ARG_EXPR std_gimplify_va_arg_expr
#define TARGET_PASS_BY_REFERENCE hook_bool_CUMULATIVE_ARGS_mode_tree_bool_false

#define TARGET_RELAXED_ORDERING false

#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size_or_pad
#define TARGET_CALLEE_COPIES hook_bool_CUMULATIVE_ARGS_mode_tree_bool_false
#define TARGET_ARG_PARTIAL_BYTES hook_int_CUMULATIVE_ARGS_mode_tree_bool_0

#define TARGET_FUNCTION_VALUE default_function_value
#define TARGET_LIBCALL_VALUE default_libcall_value
#define TARGET_INTERNAL_ARG_POINTER default_internal_arg_pointer
#define TARGET_UPDATE_STACK_BOUNDARY NULL
#define TARGET_GET_DRAP_RTX NULL
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS hook_bool_void_true
#define TARGET_STATIC_CHAIN default_static_chain
#define TARGET_TRAMPOLINE_INIT default_trampoline_init
#define TARGET_TRAMPOLINE_ADJUST_ADDRESS NULL

#define TARGET_CALLS {						\
   TARGET_PROMOTE_FUNCTION_MODE,				\
   TARGET_PROMOTE_PROTOTYPES,					\
   TARGET_STRUCT_VALUE_RTX,					\
   TARGET_RETURN_IN_MEMORY,					\
   TARGET_RETURN_IN_MSB,					\
   TARGET_PASS_BY_REFERENCE,					\
   TARGET_EXPAND_BUILTIN_SAVEREGS,				\
   TARGET_SETUP_INCOMING_VARARGS,				\
   TARGET_STRICT_ARGUMENT_NAMING,				\
   TARGET_PRETEND_OUTGOING_VARARGS_NAMED,			\
   TARGET_SPLIT_COMPLEX_ARG,					\
   TARGET_MUST_PASS_IN_STACK,					\
   TARGET_CALLEE_COPIES,					\
   TARGET_ARG_PARTIAL_BYTES,					\
   TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN,			\
   TARGET_FUNCTION_VALUE,					\
   TARGET_LIBCALL_VALUE,					\
   TARGET_INTERNAL_ARG_POINTER,					\
   TARGET_UPDATE_STACK_BOUNDARY,				\
   TARGET_GET_DRAP_RTX,						\
   TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS,			\
   TARGET_STATIC_CHAIN,						\
   TARGET_TRAMPOLINE_INIT,					\
   TARGET_TRAMPOLINE_ADJUST_ADDRESS				\
   }

#ifndef TARGET_UNWIND_TABLES_DEFAULT
#define TARGET_UNWIND_TABLES_DEFAULT false
#endif

#ifndef TARGET_HANDLE_PRAGMA_EXTERN_PREFIX
#define TARGET_HANDLE_PRAGMA_EXTERN_PREFIX 0
#endif

#ifdef IRA_COVER_CLASSES
#define TARGET_IRA_COVER_CLASSES default_ira_cover_classes
#else
#define TARGET_IRA_COVER_CLASSES 0
#endif

#ifndef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD default_secondary_reload
#endif

#ifndef TARGET_EXPAND_TO_RTL_HOOK
#define TARGET_EXPAND_TO_RTL_HOOK hook_void_void
#endif

#ifndef TARGET_INSTANTIATE_DECLS
#define TARGET_INSTANTIATE_DECLS hook_void_void
#endif

#ifndef TARGET_HARD_REGNO_SCRATCH_OK
#define TARGET_HARD_REGNO_SCRATCH_OK default_hard_regno_scratch_ok
#endif

#ifndef TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD default_case_values_threshold
#endif

#ifndef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED hook_bool_void_false
#endif

#ifndef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE hook_bool_const_int_const_int_true
#endif

/* C specific.  */
#ifndef TARGET_C_MODE_FOR_SUFFIX
#define TARGET_C_MODE_FOR_SUFFIX default_mode_for_suffix
#endif

#define TARGET_C				\
  {						\
    TARGET_C_MODE_FOR_SUFFIX			\
  }

/* C++ specific.  */
#ifndef TARGET_CXX_GUARD_TYPE
#define TARGET_CXX_GUARD_TYPE default_cxx_guard_type
#endif

#ifndef TARGET_CXX_GUARD_MASK_BIT
#define TARGET_CXX_GUARD_MASK_BIT hook_bool_void_false
#endif

#ifndef TARGET_CXX_GET_COOKIE_SIZE
#define TARGET_CXX_GET_COOKIE_SIZE default_cxx_get_cookie_size
#endif

#ifndef TARGET_CXX_COOKIE_HAS_SIZE
#define TARGET_CXX_COOKIE_HAS_SIZE hook_bool_void_false
#endif

#ifndef TARGET_CXX_IMPORT_EXPORT_CLASS
#define TARGET_CXX_IMPORT_EXPORT_CLASS NULL
#endif

#ifndef TARGET_CXX_CDTOR_RETURNS_THIS
#define TARGET_CXX_CDTOR_RETURNS_THIS hook_bool_void_false
#endif

#ifndef TARGET_CXX_KEY_METHOD_MAY_BE_INLINE
#define TARGET_CXX_KEY_METHOD_MAY_BE_INLINE hook_bool_void_true
#endif

#ifndef TARGET_CXX_DETERMINE_CLASS_DATA_VISIBILITY
#define TARGET_CXX_DETERMINE_CLASS_DATA_VISIBILITY hook_void_tree
#endif

#ifndef TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT
#define TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT hook_bool_void_true
#endif

#ifndef TARGET_CXX_LIBRARY_RTTI_COMDAT
#define TARGET_CXX_LIBRARY_RTTI_COMDAT hook_bool_void_true
#endif

#ifndef TARGET_CXX_USE_AEABI_ATEXIT
#define TARGET_CXX_USE_AEABI_ATEXIT hook_bool_void_false
#endif

#ifndef TARGET_CXX_USE_ATEXIT_FOR_CXA_ATEXIT
#define TARGET_CXX_USE_ATEXIT_FOR_CXA_ATEXIT hook_bool_void_false
#endif

#ifndef TARGET_CXX_ADJUST_CLASS_AT_DEFINITION
#define TARGET_CXX_ADJUST_CLASS_AT_DEFINITION hook_void_tree
#endif

#define TARGET_CXX				\
  {						\
    TARGET_CXX_GUARD_TYPE,			\
    TARGET_CXX_GUARD_MASK_BIT,			\
    TARGET_CXX_GET_COOKIE_SIZE,			\
    TARGET_CXX_COOKIE_HAS_SIZE,			\
    TARGET_CXX_IMPORT_EXPORT_CLASS,		\
    TARGET_CXX_CDTOR_RETURNS_THIS,		\
    TARGET_CXX_KEY_METHOD_MAY_BE_INLINE,	\
    TARGET_CXX_DETERMINE_CLASS_DATA_VISIBILITY,	\
    TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT,        \
    TARGET_CXX_LIBRARY_RTTI_COMDAT,	        \
    TARGET_CXX_USE_AEABI_ATEXIT,		\
    TARGET_CXX_USE_ATEXIT_FOR_CXA_ATEXIT,	\
    TARGET_CXX_ADJUST_CLASS_AT_DEFINITION	\
  }

/* EMUTLS specific */
#ifndef TARGET_EMUTLS_GET_ADDRESS
#define TARGET_EMUTLS_GET_ADDRESS "__builtin___emutls_get_address"
#endif

#ifndef TARGET_EMUTLS_REGISTER_COMMON
#define TARGET_EMUTLS_REGISTER_COMMON "__builtin___emutls_register_common"
#endif

#ifndef TARGET_EMUTLS_VAR_SECTION
#define TARGET_EMUTLS_VAR_SECTION NULL
#endif

#ifndef TARGET_EMUTLS_TMPL_SECTION
#define TARGET_EMUTLS_TMPL_SECTION NULL
#endif

#ifndef TARGET_EMUTLS_VAR_PREFIX
#define TARGET_EMUTLS_VAR_PREFIX NULL
#endif

#ifndef TARGET_EMUTLS_TMPL_PREFIX
#define TARGET_EMUTLS_TMPL_PREFIX NULL
#endif

#ifndef TARGET_EMUTLS_VAR_FIELDS
#define TARGET_EMUTLS_VAR_FIELDS default_emutls_var_fields
#endif

#ifndef TARGET_EMUTLS_VAR_INIT
#define TARGET_EMUTLS_VAR_INIT default_emutls_var_init
#endif

#ifndef TARGET_EMUTLS_VAR_ALIGN_FIXED
#define TARGET_EMUTLS_VAR_ALIGN_FIXED false
#endif

#ifndef TARGET_EMUTLS_DEBUG_FORM_TLS_ADDRESS
#define TARGET_EMUTLS_DEBUG_FORM_TLS_ADDRESS false
#endif

#define TARGET_EMUTLS				\
  {						\
    TARGET_EMUTLS_GET_ADDRESS,  		\
    TARGET_EMUTLS_REGISTER_COMMON,  		\
    TARGET_EMUTLS_VAR_SECTION,  		\
    TARGET_EMUTLS_TMPL_SECTION,  		\
    TARGET_EMUTLS_VAR_PREFIX,  			\
    TARGET_EMUTLS_TMPL_PREFIX,  		\
    TARGET_EMUTLS_VAR_FIELDS,			\
    TARGET_EMUTLS_VAR_INIT,			\
    TARGET_EMUTLS_VAR_ALIGN_FIXED,		\
    TARGET_EMUTLS_DEBUG_FORM_TLS_ADDRESS	\
  }

/* Function specific option attribute support.  */
#ifndef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P \
  default_target_option_valid_attribute_p
#endif

#ifndef TARGET_OPTION_SAVE
#define TARGET_OPTION_SAVE NULL
#endif

#ifndef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE NULL
#endif

#ifndef TARGET_OPTION_PRINT
#define TARGET_OPTION_PRINT NULL
#endif

#ifndef TARGET_OPTION_PRAGMA_PARSE
#define TARGET_OPTION_PRAGMA_PARSE default_target_option_pragma_parse
#endif

#ifndef TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P default_target_can_inline_p
#endif

#define TARGET_OPTION_HOOKS			\
  {						\
    TARGET_OPTION_VALID_ATTRIBUTE_P,		\
    TARGET_OPTION_SAVE,				\
    TARGET_OPTION_RESTORE,			\
    TARGET_OPTION_PRINT,			\
    TARGET_OPTION_PRAGMA_PARSE,			\
    TARGET_CAN_INLINE_P,			\
  }

/* The whole shebang.  */
#define TARGET_INITIALIZER			\
{						\
  TARGET_ASM_OUT,				\
  TARGET_SCHED,					\
  TARGET_VECTORIZE,				\
  TARGET_DEFAULT_TARGET_FLAGS,			\
  TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE,		\
  TARGET_HANDLE_OPTION,				\
  TARGET_HELP,					\
  TARGET_EH_RETURN_FILTER_MODE,			\
  TARGET_LIBGCC_CMP_RETURN_MODE,                \
  TARGET_LIBGCC_SHIFT_COUNT_MODE,               \
  TARGET_UNWIND_WORD_MODE,			\
  TARGET_MERGE_DECL_ATTRIBUTES,			\
  TARGET_MERGE_TYPE_ATTRIBUTES,			\
  TARGET_ATTRIBUTE_TABLE,			\
  TARGET_COMP_TYPE_ATTRIBUTES,			\
  TARGET_SET_DEFAULT_TYPE_ATTRIBUTES,		\
  TARGET_INSERT_ATTRIBUTES,			\
  TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P,	\
  TARGET_MS_BITFIELD_LAYOUT_P,			\
  TARGET_DECIMAL_FLOAT_SUPPORTED_P,		\
  TARGET_FIXED_POINT_SUPPORTED_P,		\
  TARGET_ALIGN_ANON_BITFIELD,			\
  TARGET_NARROW_VOLATILE_BITFIELD,		\
  TARGET_INIT_BUILTINS,				\
  TARGET_BUILTIN_DECL,				\
  TARGET_EXPAND_BUILTIN,			\
  TARGET_RESOLVE_OVERLOADED_BUILTIN,		\
  TARGET_FOLD_BUILTIN,				\
  TARGET_BUILTIN_RECIPROCAL,			\
  TARGET_MANGLE_TYPE,				\
  TARGET_INIT_LIBFUNCS,				\
  TARGET_SECTION_TYPE_FLAGS,			\
  TARGET_CANNOT_MODIFY_JUMPS_P,			\
  TARGET_BRANCH_TARGET_REGISTER_CLASS,		\
  TARGET_BRANCH_TARGET_REGISTER_CALLEE_SAVED,	\
  TARGET_HAVE_CONDITIONAL_EXECUTION,		\
  TARGET_CANNOT_FORCE_CONST_MEM,		\
  TARGET_CANNOT_COPY_INSN_P,			\
  TARGET_COMMUTATIVE_P,				\
  TARGET_LEGITIMIZE_ADDRESS,			\
  TARGET_DELEGITIMIZE_ADDRESS,			\
  TARGET_LEGITIMATE_ADDRESS_P,			\
  TARGET_USE_BLOCKS_FOR_CONSTANT_P,		\
  TARGET_MIN_ANCHOR_OFFSET,			\
  TARGET_MAX_ANCHOR_OFFSET,			\
  TARGET_USE_ANCHORS_FOR_SYMBOL_P,		\
  TARGET_FUNCTION_OK_FOR_SIBCALL,		\
  TARGET_SET_CURRENT_FUNCTION,			\
  TARGET_IN_SMALL_DATA_P,			\
  TARGET_BINDS_LOCAL_P,				\
  TARGET_MANGLE_DECL_ASSEMBLER_NAME,		\
  TARGET_ENCODE_SECTION_INFO,			\
  TARGET_STRIP_NAME_ENCODING,			\
  TARGET_SHIFT_TRUNCATION_MASK,			\
  TARGET_MIN_DIVISIONS_FOR_RECIP_MUL,		\
  TARGET_MODE_REP_EXTENDED,			\
  TARGET_VALID_POINTER_MODE,                    \
  TARGET_ADDR_SPACE_HOOKS,			\
  TARGET_SCALAR_MODE_SUPPORTED_P,		\
  TARGET_VECTOR_MODE_SUPPORTED_P,               \
  TARGET_RTX_COSTS,				\
  TARGET_ADDRESS_COST,				\
  TARGET_ALLOCATE_INITIAL_VALUE,		\
  TARGET_UNSPEC_MAY_TRAP_P,                     \
  TARGET_DWARF_REGISTER_SPAN,                   \
  TARGET_INIT_DWARF_REG_SIZES_EXTRA,		\
  TARGET_FIXED_CONDITION_CODE_REGS,		\
  TARGET_CC_MODES_COMPATIBLE,			\
  TARGET_MACHINE_DEPENDENT_REORG,		\
  TARGET_BUILD_BUILTIN_VA_LIST,			\
  TARGET_FN_ABI_VA_LIST,			\
  TARGET_CANONICAL_VA_LIST_TYPE,			\
  TARGET_EXPAND_BUILTIN_VA_START,		\
  TARGET_GIMPLIFY_VA_ARG_EXPR,			\
  TARGET_GET_PCH_VALIDITY,			\
  TARGET_PCH_VALID_P,				\
  TARGET_CHECK_PCH_TARGET_FLAGS,		\
  TARGET_DEFAULT_SHORT_ENUMS,			\
  TARGET_BUILTIN_SETJMP_FRAME_VALUE,		\
  TARGET_MD_ASM_CLOBBERS,			\
  TARGET_DWARF_CALLING_CONVENTION,              \
  TARGET_DWARF_HANDLE_FRAME_UNSPEC,		\
  TARGET_STDARG_OPTIMIZE_HOOK,			\
  TARGET_STACK_PROTECT_GUARD,			\
  TARGET_STACK_PROTECT_FAIL,			\
  TARGET_INVALID_WITHIN_DOLOOP,			\
  TARGET_VALID_DLLIMPORT_ATTRIBUTE_P,		\
  TARGET_CONST_ANCHOR,				\
  TARGET_CALLS,					\
  TARGET_INVALID_CONVERSION,			\
  TARGET_INVALID_UNARY_OP,			\
  TARGET_INVALID_BINARY_OP,			\
  TARGET_INVALID_PARAMETER_TYPE,		\
  TARGET_INVALID_RETURN_TYPE,			\
  TARGET_PROMOTED_TYPE,				\
  TARGET_CONVERT_TO_TYPE,			\
  TARGET_IRA_COVER_CLASSES,			\
  TARGET_SECONDARY_RELOAD,			\
  TARGET_EXPAND_TO_RTL_HOOK,			\
  TARGET_INSTANTIATE_DECLS,			\
  TARGET_HARD_REGNO_SCRATCH_OK,			\
  TARGET_CASE_VALUES_THRESHOLD,			\
  TARGET_FRAME_POINTER_REQUIRED,		\
  TARGET_CAN_ELIMINATE,				\
  TARGET_C,					\
  TARGET_CXX,					\
  TARGET_EMUTLS,				\
  TARGET_OPTION_HOOKS,				\
  TARGET_EXTRA_LIVE_ON_ENTRY,			\
  TARGET_UNWIND_TABLES_DEFAULT,			\
  TARGET_HAVE_NAMED_SECTIONS,			\
  TARGET_HAVE_SWITCHABLE_BSS_SECTIONS,		\
  TARGET_HAVE_CTORS_DTORS,			\
  TARGET_HAVE_TLS,				\
  TARGET_HAVE_SRODATA_SECTION,			\
  TARGET_TERMINATE_DW2_EH_FRAME_INFO,		\
  TARGET_ASM_FILE_START_APP_OFF,		\
  TARGET_ASM_FILE_START_FILE_DIRECTIVE,		\
  TARGET_HANDLE_PRAGMA_EXTERN_PREFIX,		\
  TARGET_RELAXED_ORDERING,			\
  TARGET_ARM_EABI_UNWINDER			\
}

#define TARGET_HANDLE_C_OPTION default_handle_c_option
#define TARGETCM_INITIALIZER { TARGET_HANDLE_C_OPTION }

#include "hooks.h"
#include "targhooks.h"
