/* Default macros to initialize the lang_hooks data structure.
   Copyright 2001, 2002 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva  <aoliva@redhat.com>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_LANG_HOOKS_DEF_H
#define GCC_LANG_HOOKS_DEF_H

#include "hooks.h"

struct diagnostic_context;

/* Provide a hook routine for alias sets that always returns 1.  This is
   used by languages that haven't deal with alias sets yet.  */
extern HOST_WIDE_INT hook_get_alias_set_0	PARAMS ((tree));

/* Note to creators of new hooks:

   The macros in this file should NOT be surrounded by a
   #ifdef...#endif pair, since this file declares the defaults.  Each
   front end overrides any hooks it wishes to, in the file containing
   its struct lang_hooks, AFTER including this file.

   Prefix all default hooks with "lhd_".  */

/* See langhooks.h for the definition and documentation of each hook.  */

extern void lhd_do_nothing PARAMS ((void));
extern void lhd_do_nothing_t PARAMS ((tree));
extern void lhd_do_nothing_i PARAMS ((int));
extern void lhd_do_nothing_f PARAMS ((struct function *));
extern int lhd_decode_option PARAMS ((int, char **));
extern HOST_WIDE_INT lhd_get_alias_set PARAMS ((tree));
extern tree lhd_return_tree PARAMS ((tree));
extern tree lhd_return_null_tree PARAMS ((tree));
extern int lhd_safe_from_p PARAMS ((rtx, tree));
extern int lhd_staticp PARAMS ((tree));
extern int lhd_unsafe_for_reeval PARAMS ((tree));
extern void lhd_clear_binding_stack PARAMS ((void));
extern void lhd_print_tree_nothing PARAMS ((FILE *, tree, int));
extern const char *lhd_decl_printable_name PARAMS ((tree, int));
extern rtx lhd_expand_expr PARAMS ((tree, rtx, enum machine_mode, int));
extern void lhd_print_error_function PARAMS ((struct diagnostic_context *,
					      const char *));
extern void lhd_set_decl_assembler_name PARAMS ((tree));
extern bool lhd_can_use_bit_fields_p PARAMS ((void));
extern bool lhd_warn_unused_global_decl PARAMS ((tree));
extern void lhd_incomplete_type_error PARAMS ((tree, tree));
extern tree lhd_type_promotes_to PARAMS ((tree));
extern bool lhd_decl_ok_for_sibcall PARAMS ((tree));
extern tree lhd_expr_size PARAMS ((tree));

/* Declarations of default tree inlining hooks.  */
tree lhd_tree_inlining_walk_subtrees		PARAMS ((tree *, int *,
							 walk_tree_fn,
							 void *, void *));
int lhd_tree_inlining_cannot_inline_tree_fn	PARAMS ((tree *));
int lhd_tree_inlining_disregard_inline_limits	PARAMS ((tree));
tree lhd_tree_inlining_add_pending_fn_decls	PARAMS ((void *, tree));
int lhd_tree_inlining_tree_chain_matters_p	PARAMS ((tree));
int lhd_tree_inlining_auto_var_in_fn_p		PARAMS ((tree, tree));
tree lhd_tree_inlining_copy_res_decl_for_inlining PARAMS ((tree, tree,
							   tree, void *,
							   int *, void *));
int lhd_tree_inlining_anon_aggr_type_p		PARAMS ((tree));
int lhd_tree_inlining_start_inlining		PARAMS ((tree));
void lhd_tree_inlining_end_inlining		PARAMS ((tree));
tree lhd_tree_inlining_convert_parm_for_inlining PARAMS ((tree, tree, tree));

void write_global_declarations PARAMS ((void));

#define LANG_HOOKS_NAME			"GNU unknown"
#define LANG_HOOKS_IDENTIFIER_SIZE	sizeof (struct lang_identifier)
#define LANG_HOOKS_INIT			lhd_do_nothing
#define LANG_HOOKS_FINISH		lhd_do_nothing
#define LANG_HOOKS_PARSE_FILE		lhd_do_nothing_i
#define LANG_HOOKS_CLEAR_BINDING_STACK	lhd_clear_binding_stack
#define LANG_HOOKS_INIT_OPTIONS		lhd_do_nothing
#define LANG_HOOKS_DECODE_OPTION	lhd_decode_option
#define LANG_HOOKS_POST_OPTIONS		hook_bool_void_false
#define LANG_HOOKS_GET_ALIAS_SET	lhd_get_alias_set
#define LANG_HOOKS_EXPAND_CONSTANT	lhd_return_tree
#define LANG_HOOKS_EXPAND_EXPR		lhd_expand_expr
#define LANG_HOOKS_SAFE_FROM_P		lhd_safe_from_p
#define LANG_HOOKS_FINISH_INCOMPLETE_DECL lhd_do_nothing_t
#define LANG_HOOKS_UNSAFE_FOR_REEVAL	lhd_unsafe_for_reeval
#define LANG_HOOKS_STATICP		lhd_staticp
#define LANG_HOOKS_INSERT_DEFAULT_ATTRIBUTES lhd_do_nothing_t
#define LANG_HOOKS_DUP_LANG_SPECIFIC_DECL lhd_do_nothing_t
#define LANG_HOOKS_UNSAVE_EXPR_NOW	lhd_unsave_expr_now
#define LANG_HOOKS_MAYBE_BUILD_CLEANUP	lhd_return_null_tree
#define LANG_HOOKS_SET_DECL_ASSEMBLER_NAME lhd_set_decl_assembler_name
#define LANG_HOOKS_CAN_USE_BIT_FIELDS_P lhd_can_use_bit_fields_p
#define LANG_HOOKS_HONOR_READONLY	false
#define LANG_HOOKS_PRINT_STATISTICS	lhd_do_nothing
#define LANG_HOOKS_PRINT_XNODE		lhd_print_tree_nothing
#define LANG_HOOKS_PRINT_DECL		lhd_print_tree_nothing
#define LANG_HOOKS_PRINT_TYPE		lhd_print_tree_nothing
#define LANG_HOOKS_PRINT_IDENTIFIER	lhd_print_tree_nothing
#define LANG_HOOKS_PRINT_ERROR_FUNCTION lhd_print_error_function
#define LANG_HOOKS_DECL_PRINTABLE_NAME	lhd_decl_printable_name
#define LANG_HOOKS_EXPR_SIZE		lhd_expr_size

#define LANG_HOOKS_FUNCTION_INIT	lhd_do_nothing_f
#define LANG_HOOKS_FUNCTION_FINAL	lhd_do_nothing_f
#define LANG_HOOKS_FUNCTION_ENTER_NESTED lhd_do_nothing_f
#define LANG_HOOKS_FUNCTION_LEAVE_NESTED lhd_do_nothing_f

/* Attribute hooks.  */
#define LANG_HOOKS_ATTRIBUTE_TABLE		NULL
#define LANG_HOOKS_COMMON_ATTRIBUTE_TABLE	NULL
#define LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE	NULL

/* Tree inlining hooks.  */
#define LANG_HOOKS_TREE_INLINING_WALK_SUBTREES lhd_tree_inlining_walk_subtrees
#define LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN \
  lhd_tree_inlining_cannot_inline_tree_fn
#define LANG_HOOKS_TREE_INLINING_DISREGARD_INLINE_LIMITS \
  lhd_tree_inlining_disregard_inline_limits
#define LANG_HOOKS_TREE_INLINING_ADD_PENDING_FN_DECLS \
  lhd_tree_inlining_add_pending_fn_decls
#define LANG_HOOKS_TREE_INLINING_TREE_CHAIN_MATTERS_P \
  lhd_tree_inlining_tree_chain_matters_p
#define LANG_HOOKS_TREE_INLINING_AUTO_VAR_IN_FN_P \
  lhd_tree_inlining_auto_var_in_fn_p
#define LANG_HOOKS_TREE_INLINING_COPY_RES_DECL_FOR_INLINING \
  lhd_tree_inlining_copy_res_decl_for_inlining
#define LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P \
  lhd_tree_inlining_anon_aggr_type_p
#define LANG_HOOKS_TREE_INLINING_VAR_MOD_TYPE_P \
  hook_bool_tree_false
#define LANG_HOOKS_TREE_INLINING_START_INLINING \
  lhd_tree_inlining_start_inlining
#define LANG_HOOKS_TREE_INLINING_END_INLINING \
  lhd_tree_inlining_end_inlining
#define LANG_HOOKS_TREE_INLINING_CONVERT_PARM_FOR_INLINING \
  lhd_tree_inlining_convert_parm_for_inlining

#define LANG_HOOKS_TREE_INLINING_INITIALIZER { \
  LANG_HOOKS_TREE_INLINING_WALK_SUBTREES, \
  LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN, \
  LANG_HOOKS_TREE_INLINING_DISREGARD_INLINE_LIMITS, \
  LANG_HOOKS_TREE_INLINING_ADD_PENDING_FN_DECLS, \
  LANG_HOOKS_TREE_INLINING_TREE_CHAIN_MATTERS_P, \
  LANG_HOOKS_TREE_INLINING_AUTO_VAR_IN_FN_P, \
  LANG_HOOKS_TREE_INLINING_COPY_RES_DECL_FOR_INLINING, \
  LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P, \
  LANG_HOOKS_TREE_INLINING_VAR_MOD_TYPE_P, \
  LANG_HOOKS_TREE_INLINING_START_INLINING, \
  LANG_HOOKS_TREE_INLINING_END_INLINING, \
  LANG_HOOKS_TREE_INLINING_CONVERT_PARM_FOR_INLINING \
} \

#define LANG_HOOKS_FUNCTION_INITIALIZER {	\
  LANG_HOOKS_FUNCTION_INIT,			\
  LANG_HOOKS_FUNCTION_FINAL,			\
  LANG_HOOKS_FUNCTION_ENTER_NESTED,		\
  LANG_HOOKS_FUNCTION_LEAVE_NESTED		\
}

/* Tree dump hooks.  */
int lhd_tree_dump_dump_tree 			PARAMS ((void *, tree));
int lhd_tree_dump_type_quals			PARAMS ((tree));

#define LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN lhd_tree_dump_dump_tree
#define LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN lhd_tree_dump_type_quals

#define LANG_HOOKS_TREE_DUMP_INITIALIZER { \
  LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN, \
  LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN \
}

/* Types hooks.  There are no reasonable defaults for most of them,
   so we create a compile-time error instead.  */
#define LANG_HOOKS_MAKE_TYPE make_node
#define LANG_HOOKS_INCOMPLETE_TYPE_ERROR lhd_incomplete_type_error
#define LANG_HOOKS_TYPE_PROMOTES_TO lhd_type_promotes_to

#define LANG_HOOKS_FOR_TYPES_INITIALIZER { \
  LANG_HOOKS_MAKE_TYPE, \
  LANG_HOOKS_TYPE_FOR_MODE, \
  LANG_HOOKS_TYPE_FOR_SIZE, \
  LANG_HOOKS_UNSIGNED_TYPE, \
  LANG_HOOKS_SIGNED_TYPE, \
  LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE, \
  LANG_HOOKS_TYPE_PROMOTES_TO, \
  LANG_HOOKS_INCOMPLETE_TYPE_ERROR \
}

/* Declaration hooks.  */
#define LANG_HOOKS_PUSHLEVEL	pushlevel
#define LANG_HOOKS_POPLEVEL	poplevel
#define LANG_HOOKS_GLOBAL_BINDINGS_P global_bindings_p
#define LANG_HOOKS_INSERT_BLOCK	insert_block
#define LANG_HOOKS_SET_BLOCK	set_block
#define LANG_HOOKS_PUSHDECL	pushdecl
#define LANG_HOOKS_GETDECLS	getdecls
#define LANG_HOOKS_WARN_UNUSED_GLOBAL_DECL lhd_warn_unused_global_decl
#define LANG_HOOKS_WRITE_GLOBALS write_global_declarations
#define LANG_HOOKS_DECL_OK_FOR_SIBCALL	lhd_decl_ok_for_sibcall

#define LANG_HOOKS_DECLS { \
  LANG_HOOKS_PUSHLEVEL, \
  LANG_HOOKS_POPLEVEL, \
  LANG_HOOKS_GLOBAL_BINDINGS_P, \
  LANG_HOOKS_INSERT_BLOCK, \
  LANG_HOOKS_SET_BLOCK, \
  LANG_HOOKS_PUSHDECL, \
  LANG_HOOKS_GETDECLS, \
  LANG_HOOKS_WARN_UNUSED_GLOBAL_DECL, \
  LANG_HOOKS_WRITE_GLOBALS, \
  LANG_HOOKS_DECL_OK_FOR_SIBCALL, \
}

/* The whole thing.  The structure is defined in langhooks.h.  */
#define LANG_HOOKS_INITIALIZER { \
  LANG_HOOKS_NAME, \
  LANG_HOOKS_IDENTIFIER_SIZE, \
  LANG_HOOKS_INIT_OPTIONS, \
  LANG_HOOKS_DECODE_OPTION, \
  LANG_HOOKS_POST_OPTIONS, \
  LANG_HOOKS_INIT, \
  LANG_HOOKS_FINISH, \
  LANG_HOOKS_PARSE_FILE, \
  LANG_HOOKS_CLEAR_BINDING_STACK, \
  LANG_HOOKS_GET_ALIAS_SET, \
  LANG_HOOKS_EXPAND_CONSTANT, \
  LANG_HOOKS_EXPAND_EXPR, \
  LANG_HOOKS_TRUTHVALUE_CONVERSION, \
  LANG_HOOKS_INSERT_DEFAULT_ATTRIBUTES, \
  LANG_HOOKS_SAFE_FROM_P, \
  LANG_HOOKS_FINISH_INCOMPLETE_DECL, \
  LANG_HOOKS_UNSAFE_FOR_REEVAL, \
  LANG_HOOKS_MARK_ADDRESSABLE, \
  LANG_HOOKS_STATICP, \
  LANG_HOOKS_DUP_LANG_SPECIFIC_DECL, \
  LANG_HOOKS_UNSAVE_EXPR_NOW, \
  LANG_HOOKS_MAYBE_BUILD_CLEANUP, \
  LANG_HOOKS_SET_DECL_ASSEMBLER_NAME, \
  LANG_HOOKS_CAN_USE_BIT_FIELDS_P, \
  LANG_HOOKS_HONOR_READONLY, \
  LANG_HOOKS_PRINT_STATISTICS, \
  LANG_HOOKS_PRINT_XNODE, \
  LANG_HOOKS_PRINT_DECL, \
  LANG_HOOKS_PRINT_TYPE, \
  LANG_HOOKS_PRINT_IDENTIFIER, \
  LANG_HOOKS_DECL_PRINTABLE_NAME, \
  LANG_HOOKS_PRINT_ERROR_FUNCTION, \
  LANG_HOOKS_EXPR_SIZE, \
  LANG_HOOKS_ATTRIBUTE_TABLE, \
  LANG_HOOKS_COMMON_ATTRIBUTE_TABLE, \
  LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE, \
  LANG_HOOKS_FUNCTION_INITIALIZER, \
  LANG_HOOKS_TREE_INLINING_INITIALIZER, \
  LANG_HOOKS_TREE_DUMP_INITIALIZER, \
  LANG_HOOKS_DECLS, \
  LANG_HOOKS_FOR_TYPES_INITIALIZER \
}

#endif /* GCC_LANG_HOOKS_DEF_H */
