/* Tree inlining hooks and declarations.
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

#ifndef GCC_TREE_INLINE_H
#define GCC_TREE_INLINE_H

/* Function prototypes.  */

void optimize_inline_calls PARAMS ((tree));
int tree_inlinable_function_p PARAMS ((tree));
tree walk_tree PARAMS ((tree*, walk_tree_fn, void*, void*));
tree walk_tree_without_duplicates PARAMS ((tree*, walk_tree_fn, void*));
tree copy_tree_r PARAMS ((tree*, int*, void*));
void clone_body PARAMS ((tree, tree, void*));
void remap_save_expr PARAMS ((tree*, void*, tree, int*));

/* LANG_WALK_SUBTREES is called by walk_tree() after handling common
   cases, but before walking code-specific sub-trees.  If
   lang_walk_subtrees is defined for a language, it should handle
   language-specific tree codes, as well as language-specific
   information associated to common tree codes.  If a tree node is
   completely handled within this function, it should set *SUBTREES to
   0, so that generic handling isn't attempted.  For language-specific
   tree codes, generic handling would abort(), so make sure it is set
   properly.  Both SUBTREES and *SUBTREES is guaranteed to be non-zero
   when the function is called.  */

#define LANG_WALK_SUBTREES(TP,SUBTREES,FUNC,DATA,HTAB) \
  (lang_walk_subtrees \
   ? (*lang_walk_subtrees)((TP),(SUBTREES),(FUNC),(DATA),(HTAB)) \
   : 0)
typedef tree treeopt_walk_subtrees_type PARAMS ((tree*, int*, walk_tree_fn,
						 void*, void*));
extern treeopt_walk_subtrees_type *lang_walk_subtrees;

/* LANG_CANNOT_INLINE_TREE_FN is called to determine whether there are
   language-specific reasons for not inlining a given function.  */

#define LANG_CANNOT_INLINE_TREE_FN(FNP) \
  (lang_cannot_inline_tree_fn ? (*lang_cannot_inline_tree_fn)(FNP) : 0)
typedef int treeopt_cannot_inline_tree_fn_type PARAMS ((tree*));
extern treeopt_cannot_inline_tree_fn_type *lang_cannot_inline_tree_fn;

/* LANG_DISREGARD_INLINE_LIMITS is called to determine whether a
   function should be inlined even if it would exceed inlining limits.  */

#define LANG_DISREGARD_INLINE_LIMITS(FN) \
  (lang_disregard_inline_limits ? (*lang_disregard_inline_limits)(FN) : 0)
typedef int treeopt_disregard_inline_limits_type PARAMS ((tree));
extern treeopt_disregard_inline_limits_type *lang_disregard_inline_limits;

/* LANG_ADD_PENDING_FN_DECLS is called before starting to inline a
   function, to push any language-specific functions that should not
   be inlined into the current function, into VAFNP.  PFN is the top
   of varray, and should be returned if no functions are pushed into
   VAFNP.  The top of the varray should be returned.  */

#define LANG_ADD_PENDING_FN_DECLS(VAFNP,PFN) \
  (lang_add_pending_fn_decls \
   ? (*lang_add_pending_fn_decls)((VAFNP),(PFN)) \
   : (PFN))
typedef tree treeopt_add_pending_fn_decls_type PARAMS ((void*,tree));
extern treeopt_add_pending_fn_decls_type *lang_add_pending_fn_decls;

/* LANG_TREE_CHAIN_MATTERS_P indicates whether the TREE_CHAIN of a
   language-specific tree node is relevant, i.e., whether it should be
   walked, copied and preserved across copies.  */

#define LANG_TREE_CHAIN_MATTERS_P(T) \
  (lang_tree_chain_matters_p ? (*lang_tree_chain_matters_p)(T) : 0)
typedef int treeopt_tree_chain_matters_p_type PARAMS ((tree));
extern treeopt_tree_chain_matters_p_type *lang_tree_chain_matters_p;

/* LANG_AUTO_VAR_IN_FN_P is called to determine whether VT is an
   automatic variable defined in function FT.  */

#define LANG_AUTO_VAR_IN_FN_P(VT,FT) \
  (lang_auto_var_in_fn_p ? (*lang_auto_var_in_fn_p)((VT),(FT)) \
   : (DECL_P (VT) && DECL_CONTEXT (VT) == (FT) \
      && (((TREE_CODE (VT) == VAR_DECL || TREE_CODE (VT) == PARM_DECL) \
	   && ! TREE_STATIC (VT)) \
	  || TREE_CODE (VT) == LABEL_DECL \
	  || TREE_CODE (VT) == RESULT_DECL)))
typedef int treeopt_auto_var_in_fn_p_type PARAMS ((tree,tree));
extern treeopt_auto_var_in_fn_p_type *lang_auto_var_in_fn_p;

/* LANG_COPY_RES_DECL_FOR_INLINING should return a declaration for the
   result RES of function FN to be inlined into CALLER.  NDP points to
   an integer that should be set in case a new declaration wasn't
   created (presumably because RES was of aggregate type, such that a
   TARGET_EXPR is used for the result).  TEXPS is a pointer to a
   varray with the stack of TARGET_EXPRs seen while inlining functions
   into caller; the top of TEXPS is supposed to match RES.  */

#define LANG_COPY_RES_DECL_FOR_INLINING(RES,FN,CALLER,DM,NDP,TEXPS) \
  (lang_copy_res_decl_for_inlining \
   ? (*lang_copy_res_decl_for_inlining)((RES),(FN),(CALLER),\
					(DM),(NDP),(TEXPS)) \
   : copy_decl_for_inlining ((RES), (FN), (CALLER)))
typedef tree treeopt_copy_res_decl_for_inlining_type PARAMS ((tree, tree,
							      tree, void*,
							      int*, void*));
extern treeopt_copy_res_decl_for_inlining_type
*lang_copy_res_decl_for_inlining;

/* LANG_ANON_AGGR_TYPE_P determines whether T is a type node
   representing an anonymous aggregate (union, struct, etc), i.e., one
   whose members are in the same scope as the union itself.  */

#define LANG_ANON_AGGR_TYPE_P(T) \
  (lang_anon_aggr_type_p ? (*lang_anon_aggr_type_p)(T) : 0)
typedef int treeopt_anon_aggr_type_p PARAMS ((tree));
extern treeopt_anon_aggr_type_p *lang_anon_aggr_type_p;

/* 0 if we should not perform inlining.
   1 if we should expand functions calls inline at the tree level.  
   2 if we should consider *all* functions to be inline 
   candidates.  */

extern int flag_inline_trees;

#endif /* GCC_TREE_INLINE_H */
