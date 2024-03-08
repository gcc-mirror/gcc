/* Language hooks common to C and ObjC front ends.
   Copyright (C) 2004-2024 Free Software Foundation, Inc.
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

#ifndef GCC_C_OBJC_COMMON
#define GCC_C_OBJC_COMMON

/* Implemented in c-objc-common.cc.  */
extern void c_register_features ();

/* Lang hooks that are shared between C and ObjC are defined here.  Hooks
   specific to C or ObjC go in c-lang.cc and objc/objc-lang.cc, respectively.  */

#undef LANG_HOOKS_IDENTIFIER_SIZE
#define LANG_HOOKS_IDENTIFIER_SIZE C_SIZEOF_STRUCT_LANG_IDENTIFIER
#undef LANG_HOOKS_TREE_SIZE
#define LANG_HOOKS_TREE_SIZE c_tree_size
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH c_common_finish
#undef LANG_HOOKS_OPTION_LANG_MASK
#define LANG_HOOKS_OPTION_LANG_MASK c_common_option_lang_mask
#undef LANG_HOOKS_COMPLAIN_WRONG_LANG_P
#define LANG_HOOKS_COMPLAIN_WRONG_LANG_P c_common_complain_wrong_lang_p
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#define LANG_HOOKS_INIT_OPTIONS_STRUCT c_common_init_options_struct
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS c_common_init_options
#undef LANG_HOOKS_INITIALIZE_DIAGNOSTICS
#define LANG_HOOKS_INITIALIZE_DIAGNOSTICS c_initialize_diagnostics
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION c_common_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS c_common_post_options
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET c_get_alias_set
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE c_common_parse_file
#undef LANG_HOOKS_FINISH_INCOMPLETE_DECL
#define LANG_HOOKS_FINISH_INCOMPLETE_DECL c_finish_incomplete_decl
#undef LANG_HOOKS_WARN_UNUSED_GLOBAL_DECL
#define LANG_HOOKS_WARN_UNUSED_GLOBAL_DECL c_warn_unused_global_decl
#undef LANG_HOOKS_PRINT_IDENTIFIER
#define LANG_HOOKS_PRINT_IDENTIFIER c_print_identifier
#undef LANG_HOOKS_TYPES_COMPATIBLE_P
#define LANG_HOOKS_TYPES_COMPATIBLE_P c_types_compatible_p
#undef LANG_HOOKS_MISSING_NORETURN_OK_P
#define LANG_HOOKS_MISSING_NORETURN_OK_P c_missing_noreturn_ok_p
#undef LANG_HOOKS_BLOCK_MAY_FALLTHRU
#define LANG_HOOKS_BLOCK_MAY_FALLTHRU c_block_may_fallthru
#undef  LANG_HOOKS_BUILTIN_FUNCTION
#define LANG_HOOKS_BUILTIN_FUNCTION c_builtin_function
#undef  LANG_HOOKS_BUILTIN_FUNCTION_EXT_SCOPE
#define LANG_HOOKS_BUILTIN_FUNCTION_EXT_SCOPE c_builtin_function_ext_scope
#undef  LANG_HOOKS_SIMULATE_BUILTIN_FUNCTION_DECL
#define LANG_HOOKS_SIMULATE_BUILTIN_FUNCTION_DECL \
  c_simulate_builtin_function_decl
#undef LANG_HOOKS_EMITS_BEGIN_STMT
#define LANG_HOOKS_EMITS_BEGIN_STMT true
#undef LANG_HOOKS_FINALIZE_EARLY_DEBUG
#define LANG_HOOKS_FINALIZE_EARLY_DEBUG c_common_finalize_early_debug

static const scoped_attribute_specs *const c_objc_attribute_table[] =
{
  &std_attribute_table,
  &c_common_gnu_attribute_table,
  &c_common_format_attribute_table
};

#undef LANG_HOOKS_ATTRIBUTE_TABLE
#define LANG_HOOKS_ATTRIBUTE_TABLE c_objc_attribute_table

#undef LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN
#define LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN c_dump_tree

#undef LANG_HOOKS_SIMULATE_ENUM_DECL
#define LANG_HOOKS_SIMULATE_ENUM_DECL c_simulate_enum_decl
#undef LANG_HOOKS_SIMULATE_RECORD_DECL
#define LANG_HOOKS_SIMULATE_RECORD_DECL c_simulate_record_decl
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE c_common_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE c_common_type_for_size
#undef LANG_HOOKS_INCOMPLETE_TYPE_ERROR
#define LANG_HOOKS_INCOMPLETE_TYPE_ERROR c_incomplete_type_error
#undef LANG_HOOKS_TYPE_PROMOTES_TO
#define LANG_HOOKS_TYPE_PROMOTES_TO c_type_promotes_to
#undef LANG_HOOKS_REGISTER_BUILTIN_TYPE
#define LANG_HOOKS_REGISTER_BUILTIN_TYPE c_register_builtin_type
#undef LANG_HOOKS_TO_TARGET_CHARSET
#define LANG_HOOKS_TO_TARGET_CHARSET c_common_to_target_charset
#undef LANG_HOOKS_EXPR_TO_DECL
#define LANG_HOOKS_EXPR_TO_DECL c_expr_to_decl

/* The C front end's scoping structure is very different from
   that expected by the language-independent code; it is best
   to disable getdecls.
   This means it must also provide its own write_globals.  */

#undef LANG_HOOKS_GETDECLS
#define LANG_HOOKS_GETDECLS hook_tree_void_null

/* Hooks for tree gimplification.  */
#undef LANG_HOOKS_GIMPLIFY_EXPR
#define LANG_HOOKS_GIMPLIFY_EXPR c_gimplify_expr

#undef LANG_HOOKS_TYPE_DWARF_ATTRIBUTE
#define LANG_HOOKS_TYPE_DWARF_ATTRIBUTE c_type_dwarf_attribute

#undef LANG_HOOKS_OMP_PREDETERMINED_SHARING
#define LANG_HOOKS_OMP_PREDETERMINED_SHARING c_omp_predetermined_sharing

#undef LANG_HOOKS_OMP_PREDETERMINED_MAPPING
#define LANG_HOOKS_OMP_PREDETERMINED_MAPPING c_omp_predetermined_mapping

#undef LANG_HOOKS_OMP_CLAUSE_COPY_CTOR
#define LANG_HOOKS_OMP_CLAUSE_COPY_CTOR c_omp_clause_copy_ctor

#undef LANG_HOOKS_OMP_CLAUSE_ASSIGN_OP
#define LANG_HOOKS_OMP_CLAUSE_ASSIGN_OP c_omp_clause_copy_ctor

#undef LANG_HOOKS_TREE_INLINING_VAR_MOD_TYPE_P
#define LANG_HOOKS_TREE_INLINING_VAR_MOD_TYPE_P c_var_mod_p
#endif /* GCC_C_OBJC_COMMON */
