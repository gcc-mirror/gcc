/* Default macros to initialize the lang_hooks data structure.
   Copyright 2001 Free Software Foundation, Inc.
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
extern int lhd_decode_option PARAMS ((int, char **));
extern HOST_WIDE_INT lhd_get_alias_set PARAMS ((tree));
extern tree lhd_return_tree PARAMS ((tree));
extern int lhd_safe_from_p PARAMS ((rtx, tree));
extern int lhd_staticp PARAMS ((tree));
extern void lhd_clear_binding_stack PARAMS ((void));
extern void lhd_print_tree_nothing PARAMS ((FILE *, tree, int));
extern void lhd_set_yydebug PARAMS ((int));

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

#define LANG_HOOKS_NAME			"GNU unknown"
#define LANG_HOOKS_IDENTIFIER_SIZE	sizeof (struct lang_identifier)
#define LANG_HOOKS_INIT			lhd_do_nothing
#define LANG_HOOKS_FINISH		lhd_do_nothing
#define LANG_HOOKS_CLEAR_BINDING_STACK	lhd_clear_binding_stack
#define LANG_HOOKS_INIT_OPTIONS		lhd_do_nothing
#define LANG_HOOKS_DECODE_OPTION	lhd_decode_option
#define LANG_HOOKS_POST_OPTIONS		lhd_do_nothing
#define LANG_HOOKS_GET_ALIAS_SET	lhd_get_alias_set
#define LANG_HOOKS_EXPAND_CONSTANT	lhd_return_tree
#define LANG_HOOKS_SAFE_FROM_P		lhd_safe_from_p
#define LANG_HOOKS_STATICP		lhd_staticp
#define LANG_HOOKS_HONOR_READONLY	false
#define LANG_HOOKS_PRINT_STATISTICS	lhd_do_nothing
#define LANG_HOOKS_PRINT_XNODE		lhd_print_tree_nothing
#define LANG_HOOKS_PRINT_DECL		lhd_print_tree_nothing
#define LANG_HOOKS_PRINT_TYPE		lhd_print_tree_nothing
#define LANG_HOOKS_PRINT_IDENTIFIER	lhd_print_tree_nothing
#define LANG_HOOKS_SET_YYDEBUG		lhd_set_yydebug

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
#define LANG_HOOKS_TREE_INLINING_START_INLINING \
  lhd_tree_inlining_start_inlining
#define LANG_HOOKS_TREE_INLINING_END_INLINING \
  lhd_tree_inlining_end_inlining

#define LANG_HOOKS_TREE_INLINING_INITIALIZER { \
  LANG_HOOKS_TREE_INLINING_WALK_SUBTREES, \
  LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN, \
  LANG_HOOKS_TREE_INLINING_DISREGARD_INLINE_LIMITS, \
  LANG_HOOKS_TREE_INLINING_ADD_PENDING_FN_DECLS, \
  LANG_HOOKS_TREE_INLINING_TREE_CHAIN_MATTERS_P, \
  LANG_HOOKS_TREE_INLINING_AUTO_VAR_IN_FN_P, \
  LANG_HOOKS_TREE_INLINING_COPY_RES_DECL_FOR_INLINING, \
  LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P, \
  LANG_HOOKS_TREE_INLINING_START_INLINING, \
  LANG_HOOKS_TREE_INLINING_END_INLINING \
} \

/* Tree dump hooks.  */
int lhd_tree_dump_dump_tree 			PARAMS ((void *, tree));
int lhd_tree_dump_type_quals			PARAMS ((tree));

#define LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN lhd_tree_dump_dump_tree
#define LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN lhd_tree_dump_type_quals

#define LANG_HOOKS_TREE_DUMP_INITIALIZER { \
  LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN, \
  LANG_HOOKS_TREE_DUMP_TYPE_QUALS_FN \
} \

/* The whole thing.  The structure is defined in langhooks.h.  */
#define LANG_HOOKS_INITIALIZER { \
  LANG_HOOKS_NAME, \
  LANG_HOOKS_IDENTIFIER_SIZE, \
  LANG_HOOKS_INIT_OPTIONS, \
  LANG_HOOKS_DECODE_OPTION, \
  LANG_HOOKS_POST_OPTIONS, \
  LANG_HOOKS_INIT, \
  LANG_HOOKS_FINISH, \
  LANG_HOOKS_CLEAR_BINDING_STACK, \
  LANG_HOOKS_GET_ALIAS_SET, \
  LANG_HOOKS_EXPAND_CONSTANT, \
  LANG_HOOKS_SAFE_FROM_P, \
  LANG_HOOKS_STATICP, \
  LANG_HOOKS_HONOR_READONLY, \
  LANG_HOOKS_PRINT_STATISTICS, \
  LANG_HOOKS_PRINT_XNODE, \
  LANG_HOOKS_PRINT_DECL, \
  LANG_HOOKS_PRINT_TYPE, \
  LANG_HOOKS_PRINT_IDENTIFIER, \
  LANG_HOOKS_SET_YYDEBUG, \
  LANG_HOOKS_TREE_INLINING_INITIALIZER, \
  LANG_HOOKS_TREE_DUMP_INITIALIZER \
}

#endif /* GCC_LANG_HOOKS_DEF_H */
