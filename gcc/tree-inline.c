/* Control and data flow functions for trees.
   Copyright 2001 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

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

#include "config.h"
#include "system.h"
#include "tree.h"
#include "tree-inline.h"

/* Definitions of language hooks.  */

treeopt_walk_subtrees_type *lang_walk_subtrees;
treeopt_cannot_inline_tree_fn_type *lang_cannot_inline_tree_fn;
treeopt_disregard_inline_limits_type *lang_disregard_inline_limits;
treeopt_add_pending_fn_decls_type *lang_add_pending_fn_decls;
treeopt_tree_chain_matters_p_type *lang_tree_chain_matters_p;
treeopt_auto_var_in_fn_p_type *lang_auto_var_in_fn_p;
treeopt_copy_res_decl_for_inlining_type *lang_copy_res_decl_for_inlining;
treeopt_anon_aggr_type_p *lang_anon_aggr_type_p;

/* 0 if we should not perform inlining.
   1 if we should expand functions calls inline at the tree level.  
   2 if we should consider *all* functions to be inline 
   candidates.  */

int flag_inline_trees = 0;
