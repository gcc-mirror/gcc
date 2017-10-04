/* Language hooks common to C++ and ObjC++ front ends.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.
   Contributed by Ziemowit Laski  <zlaski@apple.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_CP_OBJCP_COMMON
#define GCC_CP_OBJCP_COMMON

/* In cp/objcp-common.c, cp/cp-lang.c and objcp/objcp-lang.c.  */

extern tree cp_get_debug_type (const_tree);
extern tree objcp_tsubst_copy_and_build (tree, tree, tsubst_flags_t,
					 tree, bool);

extern int cp_decl_dwarf_attribute (const_tree, int);
extern int cp_type_dwarf_attribute (const_tree, int);
extern void cp_common_init_ts (void);
extern tree cp_unit_size_without_reusable_padding (tree);

/* Lang hooks that are shared between C++ and ObjC++ are defined here.  Hooks
   specific to C++ or ObjC++ go in cp/cp-lang.c and objcp/objcp-lang.c,
   respectively.  */

#undef LANG_HOOKS_FREE_LANG_DATA
#define LANG_HOOKS_FREE_LANG_DATA cp_free_lang_data
#undef LANG_HOOKS_TREE_SIZE
#define LANG_HOOKS_TREE_SIZE cp_tree_size
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH cxx_finish
#undef LANG_HOOKS_CLEAR_BINDING_STACK
#define LANG_HOOKS_CLEAR_BINDING_STACK pop_everything
#undef LANG_HOOKS_OPTION_LANG_MASK
#define LANG_HOOKS_OPTION_LANG_MASK c_common_option_lang_mask
#undef LANG_HOOKS_COMPLAIN_WRONG_LANG_P
#define LANG_HOOKS_COMPLAIN_WRONG_LANG_P c_common_complain_wrong_lang_p
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#define LANG_HOOKS_INIT_OPTIONS_STRUCT c_common_init_options_struct
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS c_common_init_options
#undef LANG_HOOKS_INITIALIZE_DIAGNOSTICS
#define LANG_HOOKS_INITIALIZE_DIAGNOSTICS cxx_initialize_diagnostics
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION c_common_handle_option
#undef LANG_HOOKS_HANDLE_FILENAME
#define LANG_HOOKS_HANDLE_FILENAME c_common_handle_filename
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS c_common_post_options
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET cxx_get_alias_set
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE c_common_parse_file
#undef LANG_HOOKS_DUP_LANG_SPECIFIC_DECL
#define LANG_HOOKS_DUP_LANG_SPECIFIC_DECL cxx_dup_lang_specific_decl
#undef LANG_HOOKS_SET_DECL_ASSEMBLER_NAME
#define LANG_HOOKS_SET_DECL_ASSEMBLER_NAME mangle_decl
#undef LANG_HOOKS_PRINT_STATISTICS
#define LANG_HOOKS_PRINT_STATISTICS cxx_print_statistics
#undef LANG_HOOKS_PRINT_XNODE
#define LANG_HOOKS_PRINT_XNODE cxx_print_xnode
#undef LANG_HOOKS_DECL_PRINTABLE_NAME
#define LANG_HOOKS_DECL_PRINTABLE_NAME	cxx_printable_name
#undef LANG_HOOKS_PRINT_DECL
#define LANG_HOOKS_PRINT_DECL cxx_print_decl
#undef LANG_HOOKS_PRINT_TYPE
#define LANG_HOOKS_PRINT_TYPE cxx_print_type
#undef LANG_HOOKS_PRINT_IDENTIFIER
#define LANG_HOOKS_PRINT_IDENTIFIER cxx_print_identifier
#undef LANG_HOOKS_TYPES_COMPATIBLE_P
#define LANG_HOOKS_TYPES_COMPATIBLE_P cxx_types_compatible_p
#undef LANG_HOOKS_PRINT_ERROR_FUNCTION
#define LANG_HOOKS_PRINT_ERROR_FUNCTION	cxx_print_error_function
#undef LANG_HOOKS_WARN_UNUSED_GLOBAL_DECL
#define LANG_HOOKS_WARN_UNUSED_GLOBAL_DECL cxx_warn_unused_global_decl
#undef LANG_HOOKS_POST_COMPILATION_PARSING_CLEANUPS
#define LANG_HOOKS_POST_COMPILATION_PARSING_CLEANUPS cxx_post_compilation_parsing_cleanups
#undef  LANG_HOOKS_BUILTIN_FUNCTION
#define LANG_HOOKS_BUILTIN_FUNCTION cxx_builtin_function
#undef  LANG_HOOKS_BUILTIN_FUNCTION_EXT_SCOPE
#define LANG_HOOKS_BUILTIN_FUNCTION_EXT_SCOPE cxx_builtin_function_ext_scope
#undef	LANG_HOOKS_TYPE_HASH_EQ
#define LANG_HOOKS_TYPE_HASH_EQ	cxx_type_hash_eq
#undef	LANG_HOOKS_COPY_LANG_QUALIFIERS
#define LANG_HOOKS_COPY_LANG_QUALIFIERS	cxx_copy_lang_qualifiers
#undef LANG_HOOKS_MISSING_NORETURN_OK_P
#define LANG_HOOKS_MISSING_NORETURN_OK_P cp_missing_noreturn_ok_p
#undef LANG_HOOKS_BLOCK_MAY_FALLTHRU
#define LANG_HOOKS_BLOCK_MAY_FALLTHRU cxx_block_may_fallthru

/* Attribute hooks.  */
#undef LANG_HOOKS_COMMON_ATTRIBUTE_TABLE
#define LANG_HOOKS_COMMON_ATTRIBUTE_TABLE c_common_attribute_table
#undef LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE
#define LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE c_common_format_attribute_table
#undef LANG_HOOKS_ATTRIBUTE_TABLE
#define LANG_HOOKS_ATTRIBUTE_TABLE cxx_attribute_table

#undef LANG_HOOKS_TREE_INLINING_VAR_MOD_TYPE_P
#define LANG_HOOKS_TREE_INLINING_VAR_MOD_TYPE_P cp_var_mod_type_p
#undef LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN
#define LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN cp_dump_tree
#undef LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN
#define LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN cp_type_quals

#undef LANG_HOOKS_MAKE_TYPE
#define LANG_HOOKS_MAKE_TYPE cxx_make_type
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE c_common_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE c_common_type_for_size
#undef LANG_HOOKS_INCOMPLETE_TYPE_ERROR
#define LANG_HOOKS_INCOMPLETE_TYPE_ERROR cxx_incomplete_type_error
#undef LANG_HOOKS_TYPE_PROMOTES_TO
#define LANG_HOOKS_TYPE_PROMOTES_TO cxx_type_promotes_to
#undef LANG_HOOKS_REGISTER_BUILTIN_TYPE
#define LANG_HOOKS_REGISTER_BUILTIN_TYPE c_register_builtin_type
#undef LANG_HOOKS_RECONSTRUCT_COMPLEX_TYPE
#define LANG_HOOKS_RECONSTRUCT_COMPLEX_TYPE cp_reconstruct_complex_type
#undef LANG_HOOKS_GET_DEBUG_TYPE
#define LANG_HOOKS_GET_DEBUG_TYPE cp_get_debug_type
#undef LANG_HOOKS_TO_TARGET_CHARSET
#define LANG_HOOKS_TO_TARGET_CHARSET c_common_to_target_charset
#undef LANG_HOOKS_GIMPLIFY_EXPR
#define LANG_HOOKS_GIMPLIFY_EXPR cp_gimplify_expr
#undef LANG_HOOKS_DECL_DWARF_ATTRIBUTE
#define LANG_HOOKS_DECL_DWARF_ATTRIBUTE cp_decl_dwarf_attribute
#undef LANG_HOOKS_TYPE_DWARF_ATTRIBUTE
#define LANG_HOOKS_TYPE_DWARF_ATTRIBUTE cp_type_dwarf_attribute
#undef LANG_HOOKS_UNIT_SIZE_WITHOUT_REUSABLE_PADDING
#define LANG_HOOKS_UNIT_SIZE_WITHOUT_REUSABLE_PADDING cp_unit_size_without_reusable_padding

#undef LANG_HOOKS_OMP_PREDETERMINED_SHARING
#define LANG_HOOKS_OMP_PREDETERMINED_SHARING cxx_omp_predetermined_sharing
#undef LANG_HOOKS_OMP_CLAUSE_DEFAULT_CTOR
#define LANG_HOOKS_OMP_CLAUSE_DEFAULT_CTOR cxx_omp_clause_default_ctor
#undef LANG_HOOKS_OMP_CLAUSE_COPY_CTOR
#define LANG_HOOKS_OMP_CLAUSE_COPY_CTOR cxx_omp_clause_copy_ctor
#undef LANG_HOOKS_OMP_CLAUSE_ASSIGN_OP
#define LANG_HOOKS_OMP_CLAUSE_ASSIGN_OP cxx_omp_clause_assign_op
#undef LANG_HOOKS_OMP_CLAUSE_DTOR
#define LANG_HOOKS_OMP_CLAUSE_DTOR cxx_omp_clause_dtor
#undef LANG_HOOKS_OMP_FINISH_CLAUSE
#define LANG_HOOKS_OMP_FINISH_CLAUSE cxx_omp_finish_clause
#undef LANG_HOOKS_OMP_PRIVATIZE_BY_REFERENCE
#define LANG_HOOKS_OMP_PRIVATIZE_BY_REFERENCE cxx_omp_privatize_by_reference
#undef LANG_HOOKS_OMP_MAPPABLE_TYPE
#define LANG_HOOKS_OMP_MAPPABLE_TYPE cp_omp_mappable_type
#undef LANG_HOOKS_OMP_DISREGARD_VALUE_EXPR
#define LANG_HOOKS_OMP_DISREGARD_VALUE_EXPR cxx_omp_disregard_value_expr

#undef LANG_HOOKS_EH_USE_CXA_END_CLEANUP
#define LANG_HOOKS_EH_USE_CXA_END_CLEANUP true

#undef LANG_HOOKS_EH_PROTECT_CLEANUP_ACTIONS
#define LANG_HOOKS_EH_PROTECT_CLEANUP_ACTIONS cp_protect_cleanup_actions
#endif /* GCC_CP_OBJCP_COMMON */
