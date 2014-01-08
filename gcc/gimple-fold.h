/* Gimple folding definitions.

   Copyright (C) 2011-2014 Free Software Foundation, Inc.
   Contributed by Richard Guenther <rguenther@suse.de>

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

#ifndef GCC_GIMPLE_FOLD_H
#define GCC_GIMPLE_FOLD_H

extern tree canonicalize_constructor_val (tree, tree);
extern tree get_symbol_constant_value (tree);
extern void gimplify_and_update_call_from_tree (gimple_stmt_iterator *, tree);
extern tree gimple_fold_builtin (gimple);
extern tree gimple_extract_devirt_binfo_from_cst (tree, tree);
extern bool fold_stmt (gimple_stmt_iterator *);
extern bool fold_stmt_inplace (gimple_stmt_iterator *);
extern tree maybe_fold_and_comparisons (enum tree_code, tree, tree, 
					enum tree_code, tree, tree);
extern tree maybe_fold_or_comparisons (enum tree_code, tree, tree,
				       enum tree_code, tree, tree);
extern tree gimple_fold_stmt_to_constant_1 (gimple, tree (*) (tree));
extern tree gimple_fold_stmt_to_constant (gimple, tree (*) (tree));
extern tree fold_const_aggregate_ref_1 (tree, tree (*) (tree));
extern tree fold_const_aggregate_ref (tree);
extern tree gimple_get_virt_method_for_binfo (HOST_WIDE_INT, tree);
extern bool gimple_val_nonnegative_real_p (tree);
extern tree gimple_fold_indirect_ref (tree);

#endif  /* GCC_GIMPLE_FOLD_H */
